(** #752: OCaml-side before/after row-mutation hooks, [Db.register_row_hook] /
    [Db.unregister_row_hook] — the seam downstream camel#75 needs so a
    GADT-certified program's [before:]/[after:] hooks can veto or observe a
    write without going through a SQL [CREATE TRIGGER] body.

    What is pinned here, one test group per design decision the issue asked
    for:

    - {b unknown table} — [register_row_hook] on a nonexistent table reports
      [`Unknown_table] and registers nothing (#437's contract, reused).
    - {b veto} — a [`Before] hook's [Error] aborts the INSERT before it
      writes anything, and — because OCaml hooks are the outer gate (see
      Ordering below) — pre-empts a coexisting SQL [BEFORE] trigger's body
      from running at all, which is a stronger and simpler guarantee than
      rolling that body's nested DML back.
    - {b observe} — [`After] hooks see the row that was actually written;
      UPDATE hands both [old_row] and [new_row], DELETE hands only
      [old_row], INSERT only [new_row].
    - {b [`After] errors also abort the statement} — deliberately the same
      as a [`Before] veto, not a log-and-continue; see
      [test_after_hook_error_aborts_statement] and the [register_row_hook]
      doc comment for why.
    - {b ordering} — multiple row hooks on the same key fire in registration
      order (#746's contract, reused); row hooks and a SQL trigger on the
      same (table, timing, event) are sandwiched, not interleaved: at
      [`Before] every row hook runs before the SQL trigger, at [`After]
      every matching SQL trigger runs before the row hooks.
    - {b unregister} stops delivery, and is a no-op for a handle whose id
      does not match anything live.
    - a QCheck property: for any sequence of register/unregister ops on one
      (table, timing, event), the hooks an INSERT fires are exactly the
      currently-registered ones, in registration order. *)

module Db = Granary.Db
module Row = Granary_encoding.Row
module Store = Granary_store.Store
module Ustore = Granary_unix.Store

let () = Granary_unix.install ()
let run = Lwt_main.run

(* A file-backed store, for the {!Store.is_closing} tests below --
   [Store.is_closing] is [false] unconditionally for the in-memory backend,
   which has no teardown state. Each call gets its own path so tests can run
   in any order without colliding. *)
let file_db_counter = ref 0

let fresh_file_store () =
  let n = !file_db_counter in
  incr file_db_counter;
  let path = Printf.sprintf "/tmp/granary_row_hooks_752_test_%04d.db" n in
  List.iter
    (fun suffix ->
       try Unix.unlink (path ^ suffix) with
       | _ -> ())
    [ ""; "-wal"; ".aslog" ];
  match run (Ustore.open_file ~path ()) with
  | Ok store -> store
  | Error e -> Alcotest.failf "open_file: %a" Store.pp_error e
;;

let with_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let exec_error db ~needle sql =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "%S was expected to fail with %S" sql needle
  | Error e ->
    let msg = Format.asprintf "%a" Db.pp_error e in
    let contains =
      let nl = String.length needle
      and hl = String.length msg in
      let rec go i = i + nl <= hl && (String.sub msg i nl = needle || go (i + 1)) in
      nl = 0 || go 0
    in
    Alcotest.(check bool)
      (Printf.sprintf "%S failed with %S (got %S)" sql needle msg)
      true
      contains
;;

let show_value = function
  | Row.V_text s -> s
  | Row.V_null -> "<null>"
  | Row.V_int n -> Int64.to_string n
  | Row.V_real f -> string_of_float f
  | Row.V_blob _ -> "<blob>"
;;

let show_row (r : Row.t) = Array.to_list r |> List.map show_value |> String.concat "|"

let texts db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream -> List.map show_row (run (Lwt_stream.to_list stream))
;;

(* Run a nested statement on [db] from inside a hook body, translating a
   failure into the [(unit, string) result] a hook's [fn] must return. *)
let exec_ok_lwt db sql =
  let open Lwt.Syntax in
  let* r = Db.execute db sql in
  match r with
  | Ok () -> Lwt.return (Ok ())
  | Error e -> Lwt.return (Error (Format.asprintf "%a" Db.pp_error e))
;;

let attach db ~table ~timing ~event fn =
  match Db.register_row_hook db ~table ~timing ~event fn with
  | Ok h -> h
  | Error (`Unknown_table t) -> Alcotest.failf "expected %S to be a known table" t
  | Error (`Columnstore_unsupported t) ->
    Alcotest.failf "expected %S to be a non-columnstore table" t
  | Error `Store_closing -> Alcotest.fail "expected the store not to be closing"
;;

let ok_hook fn m =
  fn m;
  Lwt.return (Ok ())
;;

(* ------------------------------------------------------------------ *)
(* Unknown table                                                       *)
(* ------------------------------------------------------------------ *)

let test_unknown_table_reports_and_registers_nothing () =
  with_db (fun db ->
    match
      Db.register_row_hook db ~table:"nope" ~timing:`Before ~event:`Insert (fun _ ->
        Lwt.return (Ok ()))
    with
    | Error (`Unknown_table t) -> Alcotest.(check string) "names the table" "nope" t
    | Error (`Columnstore_unsupported t) ->
      Alcotest.failf "expected `Unknown_table, got `Columnstore_unsupported %S" t
    | Error `Store_closing -> Alcotest.fail "expected `Unknown_table, got `Store_closing"
    | Ok _ -> Alcotest.fail "expected `Unknown_table")
;;

(* ------------------------------------------------------------------ *)
(* Veto                                                                 *)
(* ------------------------------------------------------------------ *)

let test_before_insert_veto_blocks_the_write () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let _h =
      attach db ~table:"t" ~timing:`Before ~event:`Insert (fun _ ->
        Lwt.return (Error "nope"))
    in
    exec_error db ~needle:"nope" "INSERT INTO t VALUES (1, 10)";
    Alcotest.(check (list string)) "nothing written" [] (texts db "SELECT * FROM t"))
;;

(* A [`Before] hook runs before any matching SQL trigger (the "outer gate"
   ordering decision), so a veto here means the trigger body never executes
   at all -- a stronger guarantee than rolling its nested DML back. *)
let test_before_veto_preempts_the_sql_triggers_body () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "CREATE TABLE audit (id INTEGER)";
    exec
      db
      "CREATE TRIGGER t_bi BEFORE INSERT ON t BEGIN INSERT INTO audit VALUES (NEW.id); \
       END";
    let _h =
      attach db ~table:"t" ~timing:`Before ~event:`Insert (fun _ ->
        Lwt.return (Error "veto"))
    in
    exec_error db ~needle:"veto" "INSERT INTO t VALUES (1, 10)";
    Alcotest.(check (list string)) "t untouched" [] (texts db "SELECT * FROM t");
    Alcotest.(check (list string))
      "the trigger body never ran"
      []
      (texts db "SELECT * FROM audit"))
;;

let test_after_hook_error_aborts_statement () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let _h =
      attach db ~table:"t" ~timing:`After ~event:`Insert (fun _ ->
        Lwt.return (Error "boom"))
    in
    exec_error db ~needle:"boom" "INSERT INTO t VALUES (1, 10)";
    (* Autocommit: the whole statement's transaction rolls back, including the
       primary write, exactly like a failing AFTER trigger body would today. *)
    Alcotest.(check (list string)) "row not durable" [] (texts db "SELECT * FROM t"))
;;

(* ------------------------------------------------------------------ *)
(* Observe                                                              *)
(* ------------------------------------------------------------------ *)

let test_after_insert_observes_the_row () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let seen = ref [] in
    let _h =
      attach
        db
        ~table:"t"
        ~timing:`After
        ~event:`Insert
        (ok_hook (fun m -> seen := m :: !seen))
    in
    exec db "INSERT INTO t VALUES (1, 10)";
    match !seen with
    | [ ({ Db.table = "t"; new_row = Some row; old_row = None } : Db.row_mutation) ] ->
      Alcotest.(check string) "new row contents" "1|10" (show_row row)
    | _ -> Alcotest.fail "expected exactly one Insert mutation with new_row set")
;;

let test_update_hook_receives_old_and_new () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    let seen = ref [] in
    let _h =
      attach
        db
        ~table:"t"
        ~timing:`After
        ~event:`Update
        (ok_hook (fun m -> seen := m :: !seen))
    in
    exec db "UPDATE t SET v = 20 WHERE id = 1";
    match !seen with
    | [ ({ Db.table = "t"; old_row = Some old_r; new_row = Some new_r } : Db.row_mutation)
      ] ->
      Alcotest.(check string) "old row" "1|10" (show_row old_r);
      Alcotest.(check string) "new row" "1|20" (show_row new_r)
    | _ -> Alcotest.fail "expected exactly one Update mutation with both rows set")
;;

let test_delete_hook_receives_old_row_only () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    let seen = ref [] in
    let _h =
      attach
        db
        ~table:"t"
        ~timing:`After
        ~event:`Delete
        (ok_hook (fun m -> seen := m :: !seen))
    in
    exec db "DELETE FROM t WHERE id = 1";
    match !seen with
    | [ ({ Db.table = "t"; old_row = Some old_r; new_row = None } : Db.row_mutation) ] ->
      Alcotest.(check string) "old row" "1|10" (show_row old_r)
    | _ -> Alcotest.fail "expected exactly one Delete mutation with old_row set")
;;

(* ------------------------------------------------------------------ *)
(* Ordering                                                             *)
(* ------------------------------------------------------------------ *)

let test_multiple_hooks_fire_in_registration_order () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let log = ref [] in
    let mk tag = ok_hook (fun _ -> log := tag :: !log) in
    List.iter
      (fun tag -> ignore (attach db ~table:"t" ~timing:`After ~event:`Insert (mk tag)))
      [ "a"; "b"; "c" ];
    exec db "INSERT INTO t VALUES (1, 10)";
    Alcotest.(check (list string))
      "fired in the order they were registered"
      [ "a"; "b"; "c" ]
      (List.rev !log))
;;

(* Both hooks and the SQL trigger append a row to [order_log]; the nested
   writes only reflect true call order if they share the same borrowed
   transaction, so the whole statement runs inside an explicit BEGIN and the
   hook reaches [Db.execute] on the SAME [db] (which resolves to
   [t.explicit_txn], not a fresh autocommit transaction). *)
let setup_ordering db =
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
  exec db "CREATE TABLE order_log (id INTEGER PRIMARY KEY AUTOINCREMENT, tag TEXT)"
;;

let test_before_hook_runs_before_the_sql_trigger () =
  with_db (fun db ->
    setup_ordering db;
    exec
      db
      "CREATE TRIGGER t_bi BEFORE INSERT ON t BEGIN INSERT INTO order_log (tag) VALUES \
       ('trigger'); END";
    let _h =
      attach db ~table:"t" ~timing:`Before ~event:`Insert (fun _ ->
        exec_ok_lwt db "INSERT INTO order_log (tag) VALUES ('ocaml')")
    in
    exec db "BEGIN";
    exec db "INSERT INTO t VALUES (1, 10)";
    exec db "COMMIT";
    Alcotest.(check (list string))
      "the ocaml row hook runs before the SQL BEFORE trigger"
      [ "ocaml"; "trigger" ]
      (texts db "SELECT tag FROM order_log ORDER BY id"))
;;

let test_after_hook_runs_after_the_sql_trigger () =
  with_db (fun db ->
    setup_ordering db;
    exec
      db
      "CREATE TRIGGER t_ai AFTER INSERT ON t BEGIN INSERT INTO order_log (tag) VALUES \
       ('trigger'); END";
    let _h =
      attach db ~table:"t" ~timing:`After ~event:`Insert (fun _ ->
        exec_ok_lwt db "INSERT INTO order_log (tag) VALUES ('ocaml')")
    in
    exec db "BEGIN";
    exec db "INSERT INTO t VALUES (1, 10)";
    exec db "COMMIT";
    Alcotest.(check (list string))
      "the SQL AFTER trigger runs before the ocaml row hook"
      [ "trigger"; "ocaml" ]
      (texts db "SELECT tag FROM order_log ORDER BY id"))
;;

(* ------------------------------------------------------------------ *)
(* Unregister                                                           *)
(* ------------------------------------------------------------------ *)

let test_unregister_stops_delivery () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let n = ref 0 in
    let h =
      attach db ~table:"t" ~timing:`After ~event:`Insert (ok_hook (fun _ -> incr n))
    in
    exec db "INSERT INTO t VALUES (1, 10)";
    Alcotest.(check int) "fired once while registered" 1 !n;
    Db.unregister_row_hook db h;
    exec db "INSERT INTO t VALUES (2, 20)";
    Alcotest.(check int) "silent after unregister" 1 !n)
;;

let test_unregister_is_idempotent_and_ignores_foreign_handles () =
  with_db (fun db1 ->
    exec db1 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let n1 = ref 0 in
    let h1 =
      attach db1 ~table:"t" ~timing:`After ~event:`Insert (ok_hook (fun _ -> incr n1))
    in
    (* removing twice must not raise *)
    Db.unregister_row_hook db1 h1;
    Db.unregister_row_hook db1 h1;
    exec db1 "INSERT INTO t VALUES (1, 10)";
    Alcotest.(check int) "already removed, does not fire" 0 !n1;
    with_db (fun db2 ->
      exec db2 "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
      let n2 = ref 0 in
      let _h2 =
        attach db2 ~table:"t" ~timing:`After ~event:`Insert (ok_hook (fun _ -> incr n2))
      in
      (* a handle minted on db1 (even before removal) means nothing to db2 *)
      let h1' =
        attach db1 ~table:"t" ~timing:`After ~event:`Insert (ok_hook (fun _ -> incr n1))
      in
      Db.unregister_row_hook db2 h1';
      exec db2 "INSERT INTO t VALUES (1, 10)";
      Alcotest.(check int) "db2's own hook still fires" 1 !n2))
;;

(* ------------------------------------------------------------------ *)
(* DDL invalidation (review findings #1/#2)                             *)
(* ------------------------------------------------------------------ *)

(* #1a: DROP TABLE must purge every hook registered on it, or a later,
   unrelated CREATE TABLE of the same name silently reattaches a stale one. *)
let test_drop_table_purges_its_hooks () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let fired = ref 0 in
    let _h =
      attach db ~table:"t" ~timing:`Before ~event:`Insert (fun _ ->
        incr fired;
        Lwt.return (Error "stale hook must never run again"))
    in
    exec db "DROP TABLE t";
    (* A differently-shaped table under the same name. *)
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)";
    exec db "INSERT INTO t VALUES (1, 'ok')";
    Alcotest.(check int) "the stale hook never fires" 0 !fired;
    Alcotest.(check (list string))
      "the new table has its row"
      [ "1|ok" ]
      (texts db "SELECT * FROM t"))
;;

(* #1b: RENAME migrates the hook to the new name rather than stranding it. *)
let test_rename_table_migrates_its_hooks () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let seen = ref [] in
    let _h =
      attach
        db
        ~table:"t"
        ~timing:`After
        ~event:`Insert
        (ok_hook (fun m -> seen := m :: !seen))
    in
    exec db "ALTER TABLE t RENAME TO t2";
    exec db "INSERT INTO t2 VALUES (1, 10)";
    match !seen with
    | [ ({ Db.table = "t2"; new_row = Some row; old_row = None } : Db.row_mutation) ] ->
      Alcotest.(check string)
        "fires under the new name with the right row"
        "1|10"
        (show_row row)
    | _ ->
      Alcotest.fail "expected exactly one Insert mutation reported under the new name")
;;

(* Round-4 review #1: unregister_row_hook must still detach the hook after a
   RENAME has ALREADY COMMITTED -- not just survive a rolled-back one. The
   handle's rh_table was captured as "t" at registration time; the registry
   entry now lives under "t2". Resolving removal by identity (not by
   re-deriving the captured table name) is what makes this work: a stale
   rh_table must not leave the hook permanently undetachable, which for a
   `Before hook is an undetachable veto. *)
let test_unregister_after_a_committed_rename_still_detaches_the_hook () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let fired = ref 0 in
    let h =
      attach db ~table:"t" ~timing:`After ~event:`Insert (ok_hook (fun _ -> incr fired))
    in
    (* Autocommit: this RENAME is durable before the next statement runs. *)
    exec db "ALTER TABLE t RENAME TO t2";
    exec db "INSERT INTO t2 VALUES (1, 10)";
    Alcotest.(check int) "fires under the new name before unregister" 1 !fired;
    Db.unregister_row_hook db h;
    exec db "INSERT INTO t2 VALUES (2, 20)";
    Alcotest.(check int)
      "unregister (by identity) detaches it even after a committed rename"
      1
      !fired)
;;

(* #1c: a hook registered while a CREATE TABLE is in flight must not survive
   that transaction's ROLLBACK -- otherwise a LATER, unrelated CREATE TABLE
   of the same name silently reattaches it. *)
let test_rolled_back_create_table_leaves_no_hook () =
  with_db (fun db ->
    exec db "BEGIN";
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let fired = ref 0 in
    let _h =
      attach db ~table:"t" ~timing:`Before ~event:`Insert (fun _ ->
        incr fired;
        Lwt.return (Error "must never run: its table was rolled back"))
    in
    exec db "ROLLBACK";
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    Alcotest.(check int) "the rolled-back registration never fires" 0 !fired;
    Alcotest.(check (list string))
      "the real row landed"
      [ "1|10" ]
      (texts db "SELECT * FROM t"))
;;

(* Round-2 review #1: a rolled-back DROP TABLE must restore the hooks it
   purged, exactly as it restores the catalog row -- the purge is not itself
   allowed to survive a ROLLBACK that undoes the DROP it was reacting to. *)
let test_rolled_back_drop_table_restores_its_hooks () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let fired = ref 0 in
    let _h =
      attach db ~table:"t" ~timing:`After ~event:`Insert (ok_hook (fun _ -> incr fired))
    in
    exec db "BEGIN";
    exec db "DROP TABLE t";
    exec db "ROLLBACK";
    exec db "INSERT INTO t VALUES (1, 10)";
    Alcotest.(check int) "the hook survives the rolled-back DROP" 1 !fired)
;;

(* Round-2 review #2: a hook registered mid-transaction, then relocated by a
   RENAME in the SAME transaction, must be fully undone when that whole
   transaction rolls back -- the registration itself happened inside the
   rolled-back transaction, so the CORRECT end state is no hook at all, not
   one left dangling under whatever name the (also rolled-back) rename moved
   it to. This is the LIFO-ordering case: the rename's own undo must run
   BEFORE register_row_hook's undo (which was registered earlier and looks
   for the ORIGINAL name), or the latter finds nothing under that name and
   removes nothing -- leaving the hook permanently misfiled under the
   rename's target name. Proven here the way the review frames the danger: a
   LATER, wholly unrelated table created under that target name must not
   silently inherit the stale hook (and, since it's a [`Before] hook, gain an
   unexpected veto). *)
let test_rolled_back_rename_does_not_leave_a_hook_misfiled_under_the_target_name () =
  with_db (fun db ->
    exec db "CREATE TABLE a (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "BEGIN";
    let fired = ref 0 in
    let _h =
      attach db ~table:"a" ~timing:`After ~event:`Insert (ok_hook (fun _ -> incr fired))
    in
    exec db "ALTER TABLE a RENAME TO b";
    exec db "ROLLBACK";
    (* The registration happened inside the now-rolled-back transaction, so
       it is gone entirely -- not restored under "a". *)
    exec db "INSERT INTO a VALUES (1, 10)";
    Alcotest.(check int) "not restored under the original name either" 0 !fired;
    (* The real regression: a later, unrelated table named "b" must not
       silently inherit a hook stranded there by the aborted rename. *)
    exec db "CREATE TABLE b (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO b VALUES (1, 10)";
    Alcotest.(check int) "an unrelated later table 'b' does not inherit it" 0 !fired)
;;

(* #2: a row hook on a COLUMNSTORE table can never fire (INSERT bypasses the
   hook pair entirely; UPDATE/DELETE are refused outright on such a table),
   so registration itself is refused. *)
let test_columnstore_table_refuses_registration () =
  with_db (fun db ->
    exec db "CREATE TABLE c (id INTEGER, v INTEGER) USING COLUMNSTORE";
    match
      Db.register_row_hook db ~table:"c" ~timing:`After ~event:`Insert (fun _ ->
        Lwt.return (Ok ()))
    with
    | Error (`Columnstore_unsupported t) ->
      Alcotest.(check string) "names the table" "c" t
    | Error (`Unknown_table t) ->
      Alcotest.failf "expected `Columnstore_unsupported, got `Unknown_table %S" t
    | Error `Store_closing ->
      Alcotest.fail "expected `Columnstore_unsupported, got `Store_closing"
    | Ok _ -> Alcotest.fail "expected `Columnstore_unsupported")
;;

(* Round-3 review #1: a DROP TABLE run from INSIDE a trigger body (nested DML,
   via Db.run_trigger_op) must purge that table's row hooks exactly like a
   top-level DROP TABLE does. This was the gap in the round-2 design: the
   purge lived in Db.ml's run_dml/run_core only, and trigger-body DML never
   goes through either. *)
let test_drop_table_purges_hooks_even_from_inside_a_trigger_body () =
  with_db (fun db ->
    exec db "CREATE TABLE a (id INTEGER PRIMARY KEY)";
    exec db "CREATE TABLE b (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "CREATE TRIGGER trg AFTER INSERT ON a BEGIN DROP TABLE b; END";
    let fired = ref 0 in
    let _h =
      attach db ~table:"b" ~timing:`Before ~event:`Insert (fun _ ->
        incr fired;
        Lwt.return (Error "stale hook must never run again"))
    in
    (* Fires the trigger, whose body drops b via nested DML. *)
    exec db "INSERT INTO a VALUES (1)";
    (* A differently-shaped table under the same name. *)
    exec db "CREATE TABLE b (id INTEGER PRIMARY KEY, name TEXT)";
    exec db "INSERT INTO b VALUES (1, 'ok')";
    Alcotest.(check int) "the stale hook never fires" 0 !fired;
    Alcotest.(check (list string))
      "the new table has its row"
      [ "1|ok" ]
      (texts db "SELECT * FROM b"))
;;

(* Round-3 review #2: a hook registered through one Db.t handle must be
   purged/migrated when a SIBLING handle over the SAME store runs the
   DROP/RENAME -- not just when the registering handle itself does. This is
   the more severe round-3 finding: without the registry living on Store.t,
   this defeats create_worker_handle's whole multi-handle guarantee
   (#589/#633). *)
let test_worker_handle_sibling_drop_purges_the_others_hook () =
  with_db (fun a ->
    exec a "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let fired = ref 0 in
    let _h =
      attach a ~table:"t" ~timing:`Before ~event:`Insert (fun _ ->
        incr fired;
        Lwt.return (Error "stale hook must never run again"))
    in
    let b = run (Db.create_worker_handle a) in
    (* Sibling handle drops and recreates the table with a different shape. *)
    exec b "DROP TABLE t";
    exec b "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)";
    (* Through b, whose own catalog is current -- a's is stale relative to a
       sibling's DDL, the pre-existing per-handle caveat create_worker_handle
       already documents (#589/#633/#634) and unrelated to what this test is
       checking: whether the STORE-LEVEL row-hook registry was purged. *)
    exec b "INSERT INTO t VALUES (1, 'ok')";
    Alcotest.(check int) "the stale hook, registered on a, never fires" 0 !fired;
    Alcotest.(check (list string))
      "the new table has its row"
      [ "1|ok" ]
      (texts b "SELECT * FROM t"))
;;

(* Round-3 review #2, the RENAME half: a hook registered on one handle must
   follow a RENAME run from a SIBLING handle, and fire under the new name
   observed through either handle. *)
let test_worker_handle_sibling_rename_migrates_the_others_hook () =
  with_db (fun a ->
    exec a "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let seen = ref [] in
    let _h =
      attach
        a
        ~table:"t"
        ~timing:`After
        ~event:`Insert
        (ok_hook (fun m -> seen := m :: !seen))
    in
    let b = run (Db.create_worker_handle a) in
    exec b "ALTER TABLE t RENAME TO t2";
    exec b "INSERT INTO t2 VALUES (1, 10)";
    match !seen with
    | [ ({ Db.table = "t2"; new_row = Some row; old_row = None } : Db.row_mutation) ] ->
      Alcotest.(check string)
        "fires under the new name for a write from the sibling handle"
        "1|10"
        (show_row row)
    | _ ->
      Alcotest.fail "expected exactly one Insert mutation reported under the new name")
;;

(* ------------------------------------------------------------------ *)
(* Exception normalisation (review finding #3)                          *)
(* ------------------------------------------------------------------ *)

exception Boom

(* A hook that raises rather than returning [Error] must still surface as an
   ordinary [Error (Runtime _)] from [Db.execute] -- never as an unhandled
   exception escaping [Lwt_main.run], which would break the [(_, error)
   result] contract every other failure in this library upholds. *)
let test_hook_raising_is_normalised_to_a_result () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let _h = attach db ~table:"t" ~timing:`Before ~event:`Insert (fun _ -> raise Boom) in
    match run (Db.execute db "INSERT INTO t VALUES (1, 10)") with
    | Ok () -> Alcotest.fail "expected the raised exception to abort the statement"
    | Error e ->
      let msg = Format.asprintf "%a" Db.pp_error e in
      Alcotest.(check bool)
        (Printf.sprintf "reports Runtime mentioning the exception (got %S)" msg)
        true
        (String.length msg > 0)
    | exception _ ->
      Alcotest.fail "the raised exception escaped Db.execute's result contract")
;;

(* ------------------------------------------------------------------ *)
(* Recursion guard (review round 3, item 4)                              *)
(* ------------------------------------------------------------------ *)

(* A Before/Insert hook that always does its own nested INSERT on the same
   table re-fires itself indefinitely; without a depth cap this would
   exhaust the OCaml call stack instead of failing cleanly. Runs inside an
   explicit transaction so the nested Db.execute reuses t.explicit_txn
   instead of trying (and deadlocking on) a fresh autocommit transaction
   while the outer one is still open. *)
let test_row_hook_self_recursion_is_bounded () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY)";
    exec db "BEGIN";
    let next_id = ref 2 in
    let _h =
      attach db ~table:"t" ~timing:`Before ~event:`Insert (fun _ ->
        let id = !next_id in
        incr next_id;
        exec_ok_lwt db (Printf.sprintf "INSERT INTO t VALUES (%d)" id))
    in
    exec_error db ~needle:"recursion limit" "INSERT INTO t VALUES (1)";
    exec db "ROLLBACK")
;;

(* Round-4 review: the recursion-depth counter must be SHARED across every
   Db.t handle over one store, not per-handle -- otherwise a chain that
   alternates handles (create_worker_handle siblings) could nest well past
   the intended cap before either handle's own copy noticed.
   Db.of_store, called twice over the SAME manually-created Store.t, gives
   two sibling handles the same way create_worker_handle does, but also
   hands back the raw Store.t so the test can pre-load
   Store.row_hook_depth directly -- simulating recursion already attributed
   to "the other handle" -- without needing genuine nested DML across two
   handles, which cannot be constructed without deadlocking: a second
   handle's autocommit INSERT would try to begin a fresh write transaction
   while the first handle's is still open on the very same store. *)
let test_row_hook_recursion_limit_is_shared_across_sibling_handles () =
  let store = Store.create () in
  let a = run (Db.of_store store) in
  let b = run (Db.of_store store) in
  Fun.protect
    ~finally:(fun () ->
      (try run (Db.close a) with
       | _ -> ());
      try run (Db.close b) with
      | _ -> ())
    (fun () ->
       exec a "CREATE TABLE t (id INTEGER PRIMARY KEY)";
       let reg = Store.row_hooks store in
       let fired = ref 0 in
       let _h =
         attach
           a
           ~table:"t"
           ~timing:`Before
           ~event:`Insert
           (ok_hook (fun _ -> incr fired))
       in
       (* Simulate max_row_hook_depth levels of recursion already attributed
          to a DIFFERENT handle (b) sharing this store. If the guard were
          per-handle, a's own fresh counter would read 0 here and this
          INSERT would succeed instead of hitting the limit. *)
       for _ = 1 to 32 do
         Store.row_hook_depth_incr reg
       done;
       exec_error a ~needle:"recursion limit" "INSERT INTO t VALUES (1)";
       Alcotest.(check int) "the hook never got to run" 0 !fired;
       for _ = 1 to 32 do
         Store.row_hook_depth_decr reg
       done)
;;

(* ------------------------------------------------------------------ *)
(* Transactional unregister (review round 3, item 3)                     *)
(* ------------------------------------------------------------------ *)

let test_rolled_back_unregister_restores_the_hook () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let fired = ref 0 in
    let h =
      attach db ~table:"t" ~timing:`After ~event:`Insert (ok_hook (fun _ -> incr fired))
    in
    exec db "BEGIN";
    Db.unregister_row_hook db h;
    exec db "ROLLBACK";
    exec db "INSERT INTO t VALUES (1, 10)";
    Alcotest.(check int) "the rolled-back unregister leaves the hook attached" 1 !fired)
;;

(* ------------------------------------------------------------------ *)
(* VACUUM race window (review round 5, item 1)                           *)
(* ------------------------------------------------------------------ *)

(* Db.vacuum closes the OLD store, carries the row-hook registry over to the
   new one, and only THEN swaps t.store / bumps the invalidation cohort --
   with real Lwt-yielding work (Cat.open_, Cat.load_columnar_stores) in
   between. A sibling handle whose t.store still names that already-closed
   store during this window is not yet "stale" by the cohort-generation
   check (which only flips after the swap), so without a dedicated check
   register_row_hook/unregister_row_hook -- which never open a transaction,
   unlike ordinary DML -- would silently operate on an abandoned registry.

   Rather than racing real concurrent fibers against Db.vacuum's exact
   timing (no existing test in this codebase does that for VACUUM; every
   existing test, e.g. test_vacuum_worker_634.ml, runs VACUUM to completion
   and then checks a sibling), this reconstructs the SAME store-level
   condition directly and deterministically: open a file-backed Store.t,
   wrap it as a Db.t, then close the STORE OBJECT ITSELF underneath the
   handle -- exactly the property the race window has (t.store already
   closing) -- without needing VACUUM's file-rebuild machinery or any
   interleaving at all. *)

let test_register_refuses_once_the_store_is_closing () =
  let store = fresh_file_store () in
  let db = run (Db.of_store store) in
  Fun.protect
    ~finally:(fun () ->
      try run (Store.close store) with
      | _ -> ())
    (fun () ->
       exec db "CREATE TABLE t (id INTEGER PRIMARY KEY)";
       run (Store.close store);
       match
         Db.register_row_hook db ~table:"t" ~timing:`Before ~event:`Insert (fun _ ->
           Lwt.return (Ok ()))
       with
       | Error `Store_closing -> ()
       | Error (`Unknown_table t) -> Alcotest.failf "expected `Store_closing, got %S" t
       | Error (`Columnstore_unsupported t) ->
         Alcotest.failf "expected `Store_closing, got `Columnstore_unsupported %S" t
       | Ok _ ->
         Alcotest.fail
           "expected `Store_closing: the store was already closing at the call")
;;

let test_unregister_is_a_silent_noop_once_the_store_is_closing () =
  let store = fresh_file_store () in
  let db = run (Db.of_store store) in
  Fun.protect
    ~finally:(fun () ->
      try run (Store.close store) with
      | _ -> ())
    (fun () ->
       exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
       let fired = ref 0 in
       let h =
         attach
           db
           ~table:"t"
           ~timing:`After
           ~event:`Insert
           (ok_hook (fun _ -> incr fired))
       in
       run (Store.close store);
       (* Must not raise, matching unregister_row_hook's existing
          "idempotent, never raises" contract -- Fun.protect's finally would
          mask a raise here anyway, so the absence of a crash is checked by
          the test simply reaching this point. *)
       Db.unregister_row_hook db h)
;;

(* ------------------------------------------------------------------ *)
(* Concurrent registration survives a rollback's undo (review round 5,      *)
(* item 2)                                                                  *)
(* ------------------------------------------------------------------ *)

(* Round-5 review #2: the undo Db.unregister_row_hook schedules for a
   ROLLBACK must not blindly replace the whole (table, timing, event) list
   with a pre-removal snapshot -- if a DIFFERENT caller registers a hook on
   that exact key before the ROLLBACK replays, a blind replace would erase
   that hook's list entry (its row_hook_index entry would survive, pointing
   at a list that no longer contains it) with no error to anyone. The
   concurrent registration goes through a SIBLING handle so its own
   registration-time transactional undo (which would apply if it were
   registered on a_db while a_db's transaction is open) cannot itself be
   the reason it survives -- isolating the property under test to the
   unregister-undo's merge behaviour alone. *)
let test_unregister_rollback_does_not_clobber_a_concurrent_registration () =
  with_db (fun a_db ->
    exec a_db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let b_db = run (Db.create_worker_handle a_db) in
    let fired_a = ref 0 in
    let fired_b = ref 0 in
    let a =
      attach
        a_db
        ~table:"t"
        ~timing:`After
        ~event:`Insert
        (ok_hook (fun _ -> incr fired_a))
    in
    exec a_db "BEGIN";
    Db.unregister_row_hook a_db a;
    let _b =
      attach
        b_db
        ~table:"t"
        ~timing:`After
        ~event:`Insert
        (ok_hook (fun _ -> incr fired_b))
    in
    exec a_db "ROLLBACK";
    exec a_db "INSERT INTO t VALUES (1, 10)";
    Alcotest.(check int) "a's rolled-back removal fires again" 1 !fired_a;
    Alcotest.(check int)
      "b, registered by a sibling handle in the interim, still fires"
      1
      !fired_b)
;;

(* Round-5 review #2, the purge half: DROP TABLE's row-hook purge is undone
   by the same blind-snapshot mechanism prior to this fix. b_db's own
   catalog cache still describes the pre-drop "t" (the pre-existing,
   documented cross-handle DDL-visibility caveat, #589/#633/#634), so it can
   register on "t" while a_db's DROP is only provisionally in effect --
   exactly the interleaving the review names. *)
let test_purge_rollback_does_not_clobber_a_concurrent_registration () =
  with_db (fun a_db ->
    exec a_db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let fired_old = ref 0 in
    let _old_hook =
      attach
        a_db
        ~table:"t"
        ~timing:`After
        ~event:`Insert
        (ok_hook (fun _ -> incr fired_old))
    in
    let b_db = run (Db.create_worker_handle a_db) in
    exec a_db "BEGIN";
    exec a_db "DROP TABLE t";
    let fired_new = ref 0 in
    let _new_hook =
      attach
        b_db
        ~table:"t"
        ~timing:`After
        ~event:`Insert
        (ok_hook (fun _ -> incr fired_new))
    in
    exec a_db "ROLLBACK";
    exec a_db "INSERT INTO t VALUES (1, 10)";
    Alcotest.(check int) "the rolled-back purge restores the old hook" 1 !fired_old;
    Alcotest.(check int)
      "the hook registered by a sibling in the interim still fires"
      1
      !fired_new)
;;

(* #752 (review round 6, item 1) -- see [move_row_hook_key]'s doc comment in
   store.ml. Constructed directly against the Store-level primitive,
   mirroring the round-5 concurrent-registration tests above but for the
   third sibling function (move, not unregister/purge): register a hook
   under "Foo", forward-migrate it to "Bar" (mirroring what
   [ALTER TABLE ... RENAME] does), have a DIFFERENT hook register directly
   on "Bar" -- simulating the sibling handle that can see "Bar" as live
   before the rename's transaction commits (#589/#633) -- then replay the
   migration's undo (as a ROLLBACK does). The sibling's hook must still be
   registered under "Bar"; it must never be swept onto "Foo", a table name
   it never named. *)
let test_rolled_back_rename_does_not_steal_a_concurrent_registration_at_the_target () =
  let store = Store.create () in
  let noop_fn (_ : Store.row_mutation) = Lwt.return (Ok ()) in
  let reg = Store.row_hooks store in
  let moved_id =
    Store.row_hook_register reg ~table:"Foo" ~timing:`After ~event:`Insert noop_fn
  in
  let undo = Store.row_hooks_migrate_table reg ~old_name:"Foo" ~new_name:"Bar" in
  (* The sibling's concurrent registration, landing directly on the rename's
     target name while the renaming transaction is (per the repro) still
     open. *)
  let sibling_id =
    Store.row_hook_register reg ~table:"Bar" ~timing:`After ~event:`Insert noop_fn
  in
  (* ROLLBACK replays the migration's undo. *)
  undo ();
  let at_foo = Store.row_hook_fire_list reg ~table:"Foo" ~timing:`After ~event:`Insert in
  let at_bar = Store.row_hook_fire_list reg ~table:"Bar" ~timing:`After ~event:`Insert in
  Alcotest.(check (list int))
    "the originally-migrated hook is restored under the original name"
    [ moved_id ]
    (List.map fst at_foo);
  Alcotest.(check (list int))
    "the sibling's hook, registered directly on the target name, is NOT stolen onto the \
     original name"
    [ sibling_id ]
    (List.map fst at_bar)
;;

(* ------------------------------------------------------------------ *)
(* Reentrant-write guard (review round 6, item 2)                        *)
(* ------------------------------------------------------------------ *)

(* Before this fix, a hook's own nested [Db.execute] with no explicit
   transaction open computed autocommit mode and tried to open a SECOND
   write transaction on the same store while the statement that fired the
   hook still held the writer lock -- not re-entrant (#740) -- and hung
   forever. [Store.rw_begin] now refuses immediately instead. This test
   would never have terminated before the fix; terminating at all is part
   of what it proves (a test that can hang is not acceptable here). *)
let test_reentrant_autocommit_nested_dml_fails_fast_instead_of_hanging () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY)";
    let nested_result = ref None in
    let h =
      attach db ~table:"t" ~timing:`After ~event:`Insert (fun _ ->
        let open Lwt.Syntax in
        (* No BEGIN anywhere -- the statement that fires this hook is itself
           autocommit, so this nested INSERT has no open explicit
           transaction to join and must attempt a fresh one. *)
        let* r = exec_ok_lwt db "INSERT INTO t VALUES (2)" in
        nested_result := Some r;
        Lwt.return r)
    in
    exec_error db ~needle:"refused" "INSERT INTO t VALUES (1)";
    (match !nested_result with
     | Some (Error msg) ->
       Alcotest.(check bool)
         (Printf.sprintf "the nested write's own error mentions the refusal (got %S)" msg)
         true
         (let nl = String.length "refused"
          and hl = String.length msg in
          let rec go i =
            i + nl <= hl && (String.sub msg i nl = "refused" || go (i + 1))
          in
          go 0)
     | Some (Ok ()) ->
       Alcotest.fail "expected the reentrant nested write to fail, not succeed"
     | None -> Alcotest.fail "expected the hook's nested Db.execute to run and report");
    (* The whole outer statement -- primary write included -- rolled back
       with the hook's [Error], and the attempted nested write never got far
       enough to write anything either (rw_begin refused before touching the
       tree). Detach the hook before proving the connection still works:
       otherwise this next INSERT would refire the very same failing hook. *)
    Db.unregister_row_hook db h;
    exec db "INSERT INTO t VALUES (3)";
    Alcotest.(check (list string))
      "nothing from the failed attempt persisted, and the connection still works \
       afterwards -- the refusal did not wedge the writer lock the way an actual \
       deadlock would have"
      [ "3" ]
      (texts db "SELECT * FROM t"))
;;

(* The sibling-handle variant of the same hazard: a worker-handle sibling's
   autocommit nested DML shares the SAME store, and therefore the SAME
   writer lock, as the handle whose statement fired the hook. Previously
   documented (see
   [test_row_hook_recursion_limit_is_shared_across_sibling_handles]'s own
   comment) as "cannot be constructed without deadlocking." It can be
   constructed now, because it no longer deadlocks -- it fails fast. *)
let test_reentrant_autocommit_nested_dml_across_sibling_handles_fails_fast () =
  with_db (fun a_db ->
    exec a_db "CREATE TABLE t (id INTEGER PRIMARY KEY)";
    let b_db = run (Db.create_worker_handle a_db) in
    let nested_result = ref None in
    let _h =
      attach a_db ~table:"t" ~timing:`After ~event:`Insert (fun _ ->
        let open Lwt.Syntax in
        let* r = exec_ok_lwt b_db "INSERT INTO t VALUES (2)" in
        nested_result := Some r;
        Lwt.return r)
    in
    exec_error a_db ~needle:"refused" "INSERT INTO t VALUES (1)";
    match !nested_result with
    | Some (Error _) -> ()
    | Some (Ok ()) ->
      Alcotest.fail "expected the sibling's reentrant write to fail, not succeed"
    | None -> Alcotest.fail "expected the sibling's nested Db.execute to run and report")
;;

(* ------------------------------------------------------------------ *)
(* QCheck: registered/unregistered set matches what fires               *)
(* ------------------------------------------------------------------ *)

(* Model: [live] holds the ids of currently-registered hooks for one fixed
   (table, timing, event), oldest-registration-first -- exactly the order an
   INSERT should fire them in. [Register] appends a fresh id; [Unregister n]
   removes the [n mod length]-th live id, keeping the rest in place. *)
type op =
  | Register
  | Unregister of int

let show_op = function
  | Register -> "Register"
  | Unregister n -> Printf.sprintf "Unregister %d" n
;;

let print_ops ops = "[ " ^ String.concat " ; " (List.map show_op ops) ^ " ]"

let gen_ops : op list QCheck.Gen.t =
  fun rng ->
  let open QCheck.Gen in
  let len = int_range 0 30 rng in
  let live = ref 0 in
  List.init len (fun _ ->
    if !live > 0 && bool rng
    then (
      let n = int_range 0 (!live - 1) rng in
      decr live;
      Unregister n)
    else (
      incr live;
      Register))
;;

let arb_ops = QCheck.make ~print:print_ops ~shrink:QCheck.Shrink.list gen_ops

let prop_fired_matches_registered =
  QCheck.Test.make
    ~count:300
    ~name:"row hooks: fired set matches registered, in order"
    arb_ops
  @@ fun ops ->
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () -> run (Db.close db))
    (fun () ->
       run (Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)") |> ignore;
       let next_id = ref 0 in
       let handles : (int * Db.row_hook) list ref = ref [] in
       (* newest-registration-last *)
       let fired = ref [] in
       List.iter
         (function
           | Register ->
             let id = !next_id in
             incr next_id;
             let h =
               attach
                 db
                 ~table:"t"
                 ~timing:`After
                 ~event:`Insert
                 (ok_hook (fun _ -> fired := id :: !fired))
             in
             handles := !handles @ [ id, h ]
           | Unregister n ->
             (match !handles with
              | [] -> ()
              | hs ->
                let n = n mod List.length hs in
                let id, h = List.nth hs n in
                Db.unregister_row_hook db h;
                handles := List.filter (fun (i, _) -> i <> id) !handles))
         ops;
       fired := [];
       run (Db.execute db "INSERT INTO t VALUES (1, 10)") |> ignore;
       let expected = List.map fst !handles in
       let actual = List.rev !fired in
       if expected = actual
       then true
       else (
         Printf.eprintf
           "DIVERGENCE: ops=%s expected=[%s] actual=[%s]\n%!"
           (print_ops ops)
           (String.concat ";" (List.map string_of_int expected))
           (String.concat ";" (List.map string_of_int actual));
         false))
;;

(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "row mutation hooks (#752)"
    [ ( "unknown table"
      , [ Alcotest.test_case
            "reports `Unknown_table and registers nothing"
            `Quick
            test_unknown_table_reports_and_registers_nothing
        ] )
    ; ( "veto"
      , [ Alcotest.test_case
            "before-insert veto blocks the write"
            `Quick
            test_before_insert_veto_blocks_the_write
        ; Alcotest.test_case
            "before veto preempts a coexisting SQL trigger's body"
            `Quick
            test_before_veto_preempts_the_sql_triggers_body
        ; Alcotest.test_case
            "after-hook error also aborts the statement"
            `Quick
            test_after_hook_error_aborts_statement
        ] )
    ; ( "observe"
      , [ Alcotest.test_case
            "after-insert observes the row"
            `Quick
            test_after_insert_observes_the_row
        ; Alcotest.test_case
            "update hook receives old and new"
            `Quick
            test_update_hook_receives_old_and_new
        ; Alcotest.test_case
            "delete hook receives old row only"
            `Quick
            test_delete_hook_receives_old_row_only
        ] )
    ; ( "ordering"
      , [ Alcotest.test_case
            "multiple hooks fire in registration order"
            `Quick
            test_multiple_hooks_fire_in_registration_order
        ; Alcotest.test_case
            "before hook runs before the SQL trigger"
            `Quick
            test_before_hook_runs_before_the_sql_trigger
        ; Alcotest.test_case
            "after hook runs after the SQL trigger"
            `Quick
            test_after_hook_runs_after_the_sql_trigger
        ] )
    ; ( "unregister"
      , [ Alcotest.test_case "stops delivery" `Quick test_unregister_stops_delivery
        ; Alcotest.test_case
            "idempotent and ignores foreign handles"
            `Quick
            test_unregister_is_idempotent_and_ignores_foreign_handles
        ] )
    ; ( "ddl invalidation"
      , [ Alcotest.test_case
            "DROP TABLE purges its hooks"
            `Quick
            test_drop_table_purges_its_hooks
        ; Alcotest.test_case
            "RENAME migrates its hooks"
            `Quick
            test_rename_table_migrates_its_hooks
        ; Alcotest.test_case
            "a rolled-back CREATE TABLE leaves no hook behind"
            `Quick
            test_rolled_back_create_table_leaves_no_hook
        ; Alcotest.test_case
            "a rolled-back DROP TABLE restores its hooks"
            `Quick
            test_rolled_back_drop_table_restores_its_hooks
        ; Alcotest.test_case
            "a rolled-back RENAME does not misfile a hook under the target name"
            `Quick
            test_rolled_back_rename_does_not_leave_a_hook_misfiled_under_the_target_name
        ; Alcotest.test_case
            "a COLUMNSTORE table refuses registration"
            `Quick
            test_columnstore_table_refuses_registration
        ; Alcotest.test_case
            "DROP TABLE from inside a trigger body purges its hooks"
            `Quick
            test_drop_table_purges_hooks_even_from_inside_a_trigger_body
        ; Alcotest.test_case
            "a sibling worker handle's DROP purges the other handle's hook"
            `Quick
            test_worker_handle_sibling_drop_purges_the_others_hook
        ; Alcotest.test_case
            "a sibling worker handle's RENAME migrates the other handle's hook"
            `Quick
            test_worker_handle_sibling_rename_migrates_the_others_hook
        ; Alcotest.test_case
            "unregister after a committed rename still detaches the hook"
            `Quick
            test_unregister_after_a_committed_rename_still_detaches_the_hook
        ] )
    ; ( "exception normalisation"
      , [ Alcotest.test_case
            "a raising hook is normalised to a result"
            `Quick
            test_hook_raising_is_normalised_to_a_result
        ] )
    ; ( "recursion guard"
      , [ Alcotest.test_case
            "self-recursion is bounded"
            `Quick
            test_row_hook_self_recursion_is_bounded
        ; Alcotest.test_case
            "the recursion limit is shared across sibling handles"
            `Quick
            test_row_hook_recursion_limit_is_shared_across_sibling_handles
        ] )
    ; ( "transactional unregister"
      , [ Alcotest.test_case
            "a rolled-back unregister restores the hook"
            `Quick
            test_rolled_back_unregister_restores_the_hook
        ] )
    ; ( "vacuum race window"
      , [ Alcotest.test_case
            "register refuses once the store is closing"
            `Quick
            test_register_refuses_once_the_store_is_closing
        ; Alcotest.test_case
            "unregister is a silent no-op once the store is closing"
            `Quick
            test_unregister_is_a_silent_noop_once_the_store_is_closing
        ] )
    ; ( "concurrent registration survives a rollback's undo"
      , [ Alcotest.test_case
            "unregister's rollback does not clobber a concurrent registration"
            `Quick
            test_unregister_rollback_does_not_clobber_a_concurrent_registration
        ; Alcotest.test_case
            "purge's rollback does not clobber a concurrent registration"
            `Quick
            test_purge_rollback_does_not_clobber_a_concurrent_registration
        ; Alcotest.test_case
            "rename's rollback does not steal a concurrent registration at the target"
            `Quick
            test_rolled_back_rename_does_not_steal_a_concurrent_registration_at_the_target
        ] )
    ; ( "reentrant write guard (round 6, item 2)"
      , [ Alcotest.test_case
            "a hook's own reentrant autocommit nested DML fails fast instead of hanging"
            `Quick
            test_reentrant_autocommit_nested_dml_fails_fast_instead_of_hanging
        ; Alcotest.test_case
            "the same guard covers a sibling handle's reentrant autocommit nested DML"
            `Quick
            test_reentrant_autocommit_nested_dml_across_sibling_handles_fails_fast
        ] )
    ; "qcheck", [ QCheck_alcotest.to_alcotest prop_fired_matches_registered ]
    ]
;;
