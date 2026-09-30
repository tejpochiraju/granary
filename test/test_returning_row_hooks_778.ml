(** #778: `INSERT`/`UPDATE`/`DELETE ... RETURNING` — [Db.query]'s streaming
    path for these ops ({!Sql.Exec.stream_insert_returning},
    [stream_update_returning], [stream_delete_returning]) called
    [execute_insert]/[execute_update]/[execute_delete] with no
    [?before_hook]/[?after_hook] at all, so an OCaml row hook registered via
    {!Db.register_row_hook} had no power over the RETURNING spelling of an
    otherwise identical write: a [`Before] veto did not block it, and an
    [`After] audit hook never observed it. [Db.execute "DELETE FROM t ..."]
    fired the hooks; [Db.query "DELETE FROM t ... RETURNING id"] — the exact
    same write — fired nothing.

    Fixed the same way #773/#775 fixed the FK-cascade and
    [PRAGMA not_null_repair] write paths that had the identical gap: the three
    streaming functions now resolve their hooks through
    {!Sql.Exec.row_hook_for}, the dynamically-scoped by-name resolver [Db]
    installs with {!Sql.Exec.with_row_hooks} around every [Sql.Exec] entry
    point (including [query]). INSERT additionally resolves the
    REPLACE-conflict-delete and UPSERT-conflict-update hooks
    ([on_replace_delete{,_before}] / [on_upsert_update{,_before}]), mirroring
    [Db.insert_replace_upsert_hooks] on the [Db] side.

    What is pinned here, one group per DML shape plus the parity check the
    issue asks for:

    - `INSERT ... RETURNING`, `UPDATE ... RETURNING`, `DELETE ... RETURNING`
      each fire their [`Before] and [`After] hooks, observing the same
      old/new rows the non-RETURNING spelling would;
    - a [`Before] veto on any of the three actually blocks the write —
      [Db.query] fails and nothing lands in the store;
    - `INSERT OR REPLACE ... RETURNING` fires [`Delete] hooks on the row it
      displaces, and `INSERT ... ON CONFLICT ... DO UPDATE ... RETURNING`
      fires [`Update] hooks on the upserted row — the two secondary-hook
      wires the fix also had to thread through;
    - the [Db.execute] and [Db.query] spellings of the *same* statement
      (RETURNING appended) fire the identical hook sequence. *)

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

let show_value = function
  | Row.V_text s -> s
  | Row.V_null -> "<null>"
  | Row.V_int n -> Int64.to_string n
  | Row.V_real f -> string_of_float f
  | Row.V_blob _ -> "<blob>"
;;

let show_row (r : Row.t) = Array.to_list r |> List.map show_value |> String.concat "|"

let show_opt_row = function
  | None -> "-"
  | Some r -> show_row r
;;

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let texts db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream -> List.map show_row (run (Lwt_stream.to_list stream))
;;

(* [Db.query] on RETURNING is expected to fail (a `Before veto, or an `After
   error) -- confirm it does, with [needle] somewhere in the rendered error,
   the same contract [exec_error] pins for [Db.execute] elsewhere. *)
let query_error db ~needle sql =
  match run (Db.query db sql) with
  | Ok stream ->
    (* The write itself runs eagerly inside [to_stream] for these ops -- a
       veto raises before a stream is ever handed back -- but drain
       defensively in case that invariant ever loosens, so a regression here
       fails on the assertion below rather than hanging. *)
    let (_ : string list) = List.map show_row (run (Lwt_stream.to_list stream)) in
    Alcotest.failf "%S was expected to fail with %S, but succeeded" sql needle
  | Error e ->
    let msg = Format.asprintf "%a" Db.pp_error e in
    Alcotest.(check bool)
      (Printf.sprintf "%S failed with %S (got %S)" sql needle msg)
      true
      (contains ~needle msg)
;;

let attach db ~table ~timing ~event fn =
  match Db.register_row_hook db ~table ~timing ~event fn with
  | Ok h -> h
  | Error (`Unknown_table t) -> Alcotest.failf "expected %S to be a known table" t
  | Error (`Columnstore_unsupported t) ->
    Alcotest.failf "expected %S to be a non-columnstore table" t
  | Error `Store_closing -> Alcotest.fail "expected the store not to be closing"
;;

(* Append one line per firing to [log], describing the mutation exactly enough
   to tell a [`Before] from an [`After] and an insert/update/delete apart. *)
let recorder log label (m : Db.row_mutation) =
  log
  := !log
     @ [ Printf.sprintf
           "%s %s old=%s new=%s"
           label
           m.Db.table
           (show_opt_row m.Db.old_row)
           (show_opt_row m.Db.new_row)
       ];
  Lwt.return (Ok ())
;;

let watch db log ~table ~timing ~event ~label =
  ignore (attach db ~table ~timing ~event (recorder log label) : Db.row_hook)
;;

let watch_both db log ~table ~event ~label =
  watch db log ~table ~timing:`Before ~event ~label:("before:" ^ label);
  watch db log ~table ~timing:`After ~event ~label:("after:" ^ label)
;;

let vetoer msg (_ : Db.row_mutation) = Lwt.return (Error msg)
let check_log ~msg want log = Alcotest.(check (list string)) msg want !log

(* ------------------------------------------------------------------ *)
(* INSERT ... RETURNING                                                *)
(* ------------------------------------------------------------------ *)

let test_insert_returning_fires_before_and_after_hooks () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let log = ref [] in
    watch_both db log ~table:"t" ~event:`Insert ~label:"ins";
    Alcotest.(check (list string))
      "RETURNING projects the inserted row"
      [ "1|10" ]
      (texts db "INSERT INTO t VALUES (1, 10) RETURNING id, v");
    check_log
      ~msg:"both timings fired once, with the new row"
      [ "before:ins t old=- new=1|10"; "after:ins t old=- new=1|10" ]
      log)
;;

let test_insert_returning_before_veto_blocks_the_write () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let _h = attach db ~table:"t" ~timing:`Before ~event:`Insert (vetoer "nope") in
    query_error db ~needle:"nope" "INSERT INTO t VALUES (1, 10) RETURNING id, v";
    Alcotest.(check (list string)) "nothing written" [] (texts db "SELECT * FROM t"))
;;

(* ------------------------------------------------------------------ *)
(* UPDATE ... RETURNING                                                *)
(* ------------------------------------------------------------------ *)

let test_update_returning_fires_before_and_after_hooks () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    let log = ref [] in
    watch_both db log ~table:"t" ~event:`Update ~label:"upd";
    Alcotest.(check (list string))
      "RETURNING projects the updated row"
      [ "1|20" ]
      (texts db "UPDATE t SET v = 20 WHERE id = 1 RETURNING id, v");
    check_log
      ~msg:"both timings fired once, with old and new"
      [ "before:upd t old=1|10 new=1|20"; "after:upd t old=1|10 new=1|20" ]
      log)
;;

let test_update_returning_before_veto_blocks_the_write () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    let _h = attach db ~table:"t" ~timing:`Before ~event:`Update (vetoer "no update") in
    query_error db ~needle:"no update" "UPDATE t SET v = 20 WHERE id = 1 RETURNING id, v";
    Alcotest.(check (list string)) "row unchanged" [ "1|10" ] (texts db "SELECT * FROM t"))
;;

(* ------------------------------------------------------------------ *)
(* DELETE ... RETURNING                                                *)
(* ------------------------------------------------------------------ *)

let test_delete_returning_fires_before_and_after_hooks () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    let log = ref [] in
    watch_both db log ~table:"t" ~event:`Delete ~label:"del";
    Alcotest.(check (list string))
      "RETURNING projects the deleted row"
      [ "1|10" ]
      (texts db "DELETE FROM t WHERE id = 1 RETURNING id, v");
    check_log
      ~msg:"both timings fired once, with the old row"
      [ "before:del t old=1|10 new=-"; "after:del t old=1|10 new=-" ]
      log)
;;

let test_delete_returning_before_veto_blocks_the_write () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    let _h = attach db ~table:"t" ~timing:`Before ~event:`Delete (vetoer "keep it") in
    query_error db ~needle:"keep it" "DELETE FROM t WHERE id = 1 RETURNING id, v";
    Alcotest.(check (list string)) "row survives" [ "1|10" ] (texts db "SELECT * FROM t"))
;;

(* ------------------------------------------------------------------ *)
(* INSERT's secondary hooks: REPLACE-displacement and UPSERT-update    *)
(* ------------------------------------------------------------------ *)

let test_insert_or_replace_returning_fires_delete_hooks_on_displaced_row () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    let log = ref [] in
    watch_both db log ~table:"t" ~event:`Delete ~label:"del";
    Alcotest.(check (list string))
      "RETURNING projects the row actually stored (the replacement)"
      [ "1|99" ]
      (texts db "INSERT OR REPLACE INTO t VALUES (1, 99) RETURNING id, v");
    check_log
      ~msg:"the displaced row fired DELETE hooks, both timings"
      [ "before:del t old=1|10 new=-"; "after:del t old=1|10 new=-" ]
      log)
;;

let test_insert_on_conflict_do_update_returning_fires_update_hooks () =
  with_db (fun db ->
    exec db "CREATE TABLE kv (k INTEGER PRIMARY KEY, v TEXT)";
    exec db "INSERT INTO kv VALUES (1, 'first')";
    let log = ref [] in
    watch_both db log ~table:"kv" ~event:`Update ~label:"ups";
    Alcotest.(check (list string))
      "RETURNING projects the upserted row"
      [ "1|second" ]
      (texts
         db
         "INSERT INTO kv (k, v) VALUES (1, 'second') ON CONFLICT(k) DO UPDATE SET v = \
          excluded.v RETURNING k, v");
    check_log
      ~msg:"the upsert fired UPDATE hooks, both timings"
      [ "before:ups kv old=1|first new=1|second"
      ; "after:ups kv old=1|first new=1|second"
      ]
      log)
;;

(* ------------------------------------------------------------------ *)
(* Db.execute vs Db.query: the same statement, RETURNING appended,     *)
(* fires the identical hook sequence.                                  *)
(* ------------------------------------------------------------------ *)

let test_execute_and_query_spellings_fire_the_same_hooks () =
  let setup db =
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)"
  in
  let db_execute = run (Db.open_in_memory ()) in
  let db_query = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      (try run (Db.close db_execute) with
       | _ -> ());
      try run (Db.close db_query) with
      | _ -> ())
    (fun () ->
       setup db_execute;
       setup db_query;
       let log_execute = ref [] in
       let log_query = ref [] in
       watch_both db_execute log_execute ~table:"t" ~event:`Update ~label:"upd";
       watch_both db_query log_query ~table:"t" ~event:`Update ~label:"upd";
       exec db_execute "UPDATE t SET v = 20 WHERE id = 1";
       let (_ : string list) =
         texts db_query "UPDATE t SET v = 20 WHERE id = 1 RETURNING id"
       in
       Alcotest.(check (list string))
         "Db.execute and Db.query fire the identical UPDATE hook sequence"
         !log_execute
         !log_query)
;;

let test_execute_and_query_veto_agree () =
  let setup db =
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    let (_ : Db.row_hook) =
      attach db ~table:"t" ~timing:`Before ~event:`Delete (vetoer "same veto")
    in
    ()
  in
  let db_execute = run (Db.open_in_memory ()) in
  let db_query = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      (try run (Db.close db_execute) with
       | _ -> ());
      try run (Db.close db_query) with
      | _ -> ())
    (fun () ->
       setup db_execute;
       setup db_query;
       (match run (Db.execute db_execute "DELETE FROM t WHERE id = 1") with
        | Ok () -> Alcotest.fail "Db.execute: expected the veto to block the delete"
        | Error e ->
          let msg = Format.asprintf "%a" Db.pp_error e in
          Alcotest.(check bool)
            "Db.execute veto message"
            true
            (contains ~needle:"same veto" msg));
       query_error db_query ~needle:"same veto" "DELETE FROM t WHERE id = 1 RETURNING id";
       Alcotest.(check (list string))
         "both handles kept the row"
         [ "1|10" ]
         (texts db_execute "SELECT * FROM t");
       Alcotest.(check (list string))
         "both handles kept the row"
         [ "1|10" ]
         (texts db_query "SELECT * FROM t"))
;;

(* ------------------------------------------------------------------ *)
(* #778 review round 3                                                  *)
(* ------------------------------------------------------------------ *)

(* Finding 3 (raised in round 2, repeated in round 3): [stream_insert_returning]
   resolves its hooks ONCE before the multi-row [VALUES] loop, matching the
   pre-existing non-RETURNING [execute_insert_values] path and its known
   limitation, tracked as #771 (open) -- not a new regression, but untested
   until now. A hook that unregisters itself mid-statement (the documented
   #752 self-mutation pattern) keeps firing its stale, already-resolved
   closure for every row after the one that unregistered it. This pins that
   behavior, AND pins the parity claim itself by running the identical
   statement without RETURNING through [Db.execute] on a second handle: if
   #771 is ever fixed on one path, this test fails until it is fixed on both. *)
let test_multi_row_returning_insert_resolves_hooks_once_per_statement () =
  let arm db log =
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    let h = ref None in
    let self_unregistering (m : Db.row_mutation) =
      (match !h with
       | Some hook -> Db.unregister_row_hook db hook
       | None -> ());
      recorder log "self-unreg" m
    in
    h := Some (attach db ~table:"t" ~timing:`Before ~event:`Insert self_unregistering)
  in
  let want = [ "self-unreg t old=- new=1|10"; "self-unreg t old=- new=2|20" ] in
  with_db (fun db ->
    let log = ref [] in
    arm db log;
    Alcotest.(check (list string))
      "both rows inserted"
      [ "1|10"; "2|20" ]
      (texts db "INSERT INTO t VALUES (1, 10), (2, 20) RETURNING id, v");
    check_log
      ~msg:
        "the hook fired for BOTH rows even though it unregistered itself after the first \
         -- hooks are resolved once per statement (#771), not once per row"
      want
      log);
  with_db (fun db ->
    let log = ref [] in
    arm db log;
    exec db "INSERT INTO t VALUES (1, 10), (2, 20)";
    check_log
      ~msg:"the non-RETURNING Db.execute spelling behaves identically (#771 parity)"
      want
      log)
;;

(* Finding 5: this PR makes [execute_insert]'s #631 statement-level savepoint
   branch reachable for RETURNING for the first time -- before this fix,
   [before_hook] was always [None] on the RETURNING path, and #631's savepoint
   is only taken when a BEFORE INSERT hook exists AND the resolution can skip.
   Pins that the #631 invariant ("a skipped row's own nested DML leaves
   nothing behind, including inside a borrowed explicit transaction") still
   holds when the write is spelled with RETURNING. *)
let test_or_ignore_before_hook_nested_dml_leaves_nothing_returning () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "CREATE TABLE audit (id INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    let nested_writer (_ : Db.row_mutation) =
      Lwt.bind (Db.execute db "INSERT INTO audit VALUES (999)") (function
        | Ok () -> Lwt.return (Ok ())
        | Error e -> Lwt.return (Error (Format.asprintf "%a" Db.pp_error e)))
    in
    let _h = attach db ~table:"t" ~timing:`Before ~event:`Insert nested_writer in
    exec db "BEGIN";
    Alcotest.(check (list string))
      "the conflicting row is skipped, nothing returned"
      []
      (texts db "INSERT OR IGNORE INTO t VALUES (1, 99) RETURNING id, v");
    exec db "COMMIT";
    Alcotest.(check (list string))
      "the skipped row's own nested DML (audit insert) left nothing behind, even though \
       the write was spelled with RETURNING inside an explicit transaction"
      []
      (texts db "SELECT * FROM audit");
    Alcotest.(check (list string))
      "the original row is untouched"
      [ "1|10" ]
      (texts db "SELECT * FROM t"))
;;

(* Residual, deliberately out of #778's scope: the RETURNING path resolves
   hooks through [Db.make_row_hook_lookup], which is OCaml-row-hooks-only by
   the #773 design decision, so a SQL [CREATE TRIGGER] body does NOT fire for
   the RETURNING spelling of a write that fires it through [Db.execute]. That
   was already true before #778 (the RETURNING path fired nothing at all);
   pinned here so the asymmetry is a recorded decision that has to be re-made,
   not a silent drift. *)
let test_sql_trigger_does_not_fire_on_returning_residual () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "CREATE TABLE audit (id INTEGER)";
    exec
      db
      "CREATE TRIGGER t_ai AFTER INSERT ON t BEGIN INSERT INTO audit VALUES (NEW.id); END";
    exec db "INSERT INTO t VALUES (1, 10)";
    let (_ : string list) = texts db "INSERT INTO t VALUES (2, 20) RETURNING id" in
    Alcotest.(check (list string))
      "the SQL trigger fired for Db.execute's insert only, not the RETURNING one"
      [ "1" ]
      (texts db "SELECT id FROM audit"))
;;

(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "RETURNING DML row hooks (#778)"
    [ ( "INSERT ... RETURNING"
      , [ Alcotest.test_case
            "fires before/after hooks"
            `Quick
            test_insert_returning_fires_before_and_after_hooks
        ; Alcotest.test_case
            "before veto blocks the write"
            `Quick
            test_insert_returning_before_veto_blocks_the_write
        ] )
    ; ( "UPDATE ... RETURNING"
      , [ Alcotest.test_case
            "fires before/after hooks"
            `Quick
            test_update_returning_fires_before_and_after_hooks
        ; Alcotest.test_case
            "before veto blocks the write"
            `Quick
            test_update_returning_before_veto_blocks_the_write
        ] )
    ; ( "DELETE ... RETURNING"
      , [ Alcotest.test_case
            "fires before/after hooks"
            `Quick
            test_delete_returning_fires_before_and_after_hooks
        ; Alcotest.test_case
            "before veto blocks the write"
            `Quick
            test_delete_returning_before_veto_blocks_the_write
        ] )
    ; ( "INSERT ... RETURNING secondary hooks"
      , [ Alcotest.test_case
            "OR REPLACE fires DELETE hooks on the displaced row"
            `Quick
            test_insert_or_replace_returning_fires_delete_hooks_on_displaced_row
        ; Alcotest.test_case
            "ON CONFLICT DO UPDATE fires UPDATE hooks"
            `Quick
            test_insert_on_conflict_do_update_returning_fires_update_hooks
        ] )
    ; ( "Db.execute / Db.query parity"
      , [ Alcotest.test_case
            "the same statement fires the same hooks either way"
            `Quick
            test_execute_and_query_spellings_fire_the_same_hooks
        ; Alcotest.test_case
            "a veto blocks the write either way, with the same message"
            `Quick
            test_execute_and_query_veto_agree
        ] )
    ; ( "round 3 (#778 review)"
      , [ Alcotest.test_case
            "a multi-row VALUES RETURNING insert resolves hooks once per statement"
            `Quick
            test_multi_row_returning_insert_resolves_hooks_once_per_statement
        ; Alcotest.test_case
            "OR IGNORE's BEFORE hook's own nested DML leaves nothing behind, RETURNING \
             included"
            `Quick
            test_or_ignore_before_hook_nested_dml_leaves_nothing_returning
        ; Alcotest.test_case
            "residual: a SQL trigger does not fire for the RETURNING spelling"
            `Quick
            test_sql_trigger_does_not_fire_on_returning_residual
        ] )
    ]
;;
