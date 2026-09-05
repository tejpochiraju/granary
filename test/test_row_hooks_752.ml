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

let run = Lwt_main.run

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
    | Ok _ -> Alcotest.fail "expected `Columnstore_unsupported")
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
            "a COLUMNSTORE table refuses registration"
            `Quick
            test_columnstore_table_refuses_registration
        ] )
    ; ( "exception normalisation"
      , [ Alcotest.test_case
            "a raising hook is normalised to a result"
            `Quick
            test_hook_raising_is_normalised_to_a_result
        ] )
    ; "qcheck", [ QCheck_alcotest.to_alcotest prop_fired_matches_registered ]
    ]
;;
