(** #666: a rolled-back statement's #417 row-level deltas are reverted with the
    store; its #240 dirty names are deliberately not.

    Three pieces of per-statement state ride alongside a write: the [Store]
    trees, the [Schema_cache], and the ambient accumulator in [Exec]. Rollback
    reverted the first two and not the third, so a write the store had UNDONE
    still described a row to the incremental-view-maintenance consumer.

    The consequence is the reason this is not a counter bug. [Db.drive_reactive]
    installs a change accumulator around every statement, [rv_absorb_changes]
    consumes it, and the maintenance applies each delta to the materialisation
    [_rv_<name>]. A stale [Inserted] therefore becomes a {b phantom row in a
    materialised reactive view}: a row the view reports and the base table does
    not hold. Every test below reads the materialisation back and compares it
    against the authoritative query over the base table, because a test that only
    counted rows in the base table would have passed throughout — the base table
    was always correct.

    The statement that produces it is #631's: an [OR IGNORE] INSERT whose BEFORE
    INSERT trigger performs nested DML and whose row is then skipped. #631 made
    the trigger's writes vanish from the store in both autocommit and an explicit
    transaction; the deltas describing them survived both.

    {b The extent is wider than #631's savepoint.} That savepoint is taken only
    when the transaction is BORROWED (plus a BEFORE INSERT trigger, plus
    [CA_ignore]) — in autocommit the skip arms of [execute_insert_write] /
    [execute_upsert_update] roll the whole per-row transaction back instead, and
    the accumulator rides Lwt storage straight across that rollback. So the mark
    is taken unconditionally in [execute_insert] and restored on either
    mechanism, and both are pinned here. All three skip arms are covered, since
    they are three different functions: the rowid-alias PK ([put_x]'s
    [CA_ignore] arm), a secondary UNIQUE index ([check_insert_unique] →
    [Iw_skip]), and #599's NOT NULL skip.

    {b The #240 name set is NOT reverted, and that is a decision.} It is an
    invalidation hint: a superfluous entry costs an external cache one miss,
    while a missing one is a stale read, so the safe direction is to
    over-invalidate. [dirty_names_survive_the_undone_write] pins that, next to
    the delta test it deliberately disagrees with — the two halves of one
    accumulator answering differently is the point, not an inconsistency.

    {b The opposite direction is open, not fixed here.} [Db.drive_reactive]
    drops the whole accumulator when a statement returns [Error]. In autocommit
    that is right (the transaction was rolled back); in a borrowed transaction a
    raising statement's partial writes survive, so their deltas are dropped
    while the rows remain — a view MISSING rows rather than inventing them. It
    is unobservable through today's public surfaces and is tracked as #737.

    {b The control cases matter as much as the defect.} A restore that fired too
    widely would eat every trigger's deltas, which is the same wrong answer with
    the sign flipped (a missing row instead of a phantom one), and it would be
    invisible to a test that only checks the skipped case. So a surviving row
    keeps its deltas, and a multi-row VALUES list drops only the skipped row's. *)

open Lwt.Syntax
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

let query db sql =
  run
    (let* r = Db.query db sql in
     match r with
     | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
     | Ok stream -> Lwt_stream.to_list stream)
;;

let show_value = function
  | Row.V_text s -> s
  | Row.V_null -> "<null>"
  | Row.V_int n -> Int64.to_string n
  | Row.V_real f -> string_of_float f
  | Row.V_blob _ -> "<blob>"
;;

let texts db sql =
  List.map
    (fun (r : Row.t) -> Array.to_list r |> List.map show_value |> String.concat "|")
    (query db sql)
  |> List.sort compare
;;

let run_stmt db sql params =
  let st =
    run
      (let* r = Db.prepare db sql in
       match r with
       | Ok st -> Lwt.return st
       | Error e -> Alcotest.failf "prepare %S: %a" sql Db.pp_error e)
  in
  run (Db.run st ~params)
;;

(* The whole point of the issue: the materialisation must equal the
   authoritative query over the base table. Reading only one of the two would
   miss the defect in one direction or the other. *)
let check_view db label =
  Alcotest.(check (list string))
    (label ^ ": _rv_av = authoritative GROUP BY over audit")
    (texts db "SELECT grp, COUNT(*) FROM audit GROUP BY grp")
    (texts db "SELECT * FROM _rv_av")
;;

let audit_rows db = texts db "SELECT k, grp FROM audit"

(* [t] carries both conflict shapes at once so one fixture reaches all three
   skip arms: [k] is the rowid-alias PRIMARY KEY (resolved by
   [execute_insert_write]'s [put_x] arm), [v] is NOT NULL (#599's skip) and
   carries a secondary UNIQUE index (resolved by [check_insert_unique] as
   [Ic_skip]). The BEFORE INSERT trigger writes to [audit], which the reactive
   view is built over — [audit] is a plain rowid table so the nested INSERT also
   allocates from the shared rowid counter, the way #631's fixture does.

   The view is a COUNT/GROUP BY, i.e. a DELTA-maintainable shape: an
   unmaintainable one (MIN) would be repaired by the full refresh and could not
   show the phantom at all. *)
let seed db =
  exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER NOT NULL)";
  exec db "CREATE UNIQUE INDEX t_v ON t (v)";
  exec db "CREATE TABLE audit (k INTEGER, grp TEXT)";
  exec db "INSERT INTO t VALUES (1, 5)";
  exec
    db
    "CREATE TRIGGER t_bi BEFORE INSERT ON t BEGIN INSERT INTO audit VALUES (NEW.k, 'g'); \
     END";
  exec db "CREATE REACTIVE VIEW av AS SELECT grp, COUNT(*) FROM audit GROUP BY grp"
;;

(* ------------------------------------------------------------------ *)
(* The defect: a skipped row's trigger deltas                          *)
(* ------------------------------------------------------------------ *)

(* Autocommit. #631's savepoint is NOT taken here ([owned] is true), so this
   case is reached only by the wider extent: the skip arm's [S.rollback] of the
   per-row transaction, after which the mark is restored. Before the fix the
   view reported one 'g' row and [audit] held none. *)
let autocommit_alias_pk_skip_leaves_no_phantom () =
  with_db (fun db ->
    seed db;
    check_view db "empty";
    exec db "INSERT OR IGNORE INTO t VALUES (1, 9)";
    Alcotest.(check (list string)) "the trigger's write was undone" [] (audit_rows db);
    check_view db "after an alias-PK skip")
;;

(* The secondary UNIQUE index reaches a different function
   ([check_insert_unique] → [Iw_skip]), so it is pinned separately: v = 5 is
   taken, k = 2 is free. *)
let autocommit_unique_index_skip_leaves_no_phantom () =
  with_db (fun db ->
    seed db;
    exec db "INSERT OR IGNORE INTO t VALUES (2, 5)";
    Alcotest.(check (list string)) "the trigger's write was undone" [] (audit_rows db);
    check_view db "after a UNIQUE-index skip")
;;

(* #599's NOT NULL skip is the third arm, and it is decided in
   [execute_insert] itself — after the BEFORE trigger has already run and
   written. Bound rather than literal so [Sema]'s static check does not
   pre-empt the runtime skip (#599). *)
let autocommit_not_null_skip_leaves_no_phantom () =
  with_db (fun db ->
    seed db;
    (match run_stmt db "INSERT OR IGNORE INTO t VALUES (3, ?)" [ Db.V_null ] with
     | Ok n -> Alcotest.(check int) "NOT NULL skip reports 0 rows" 0 n
     | Error e -> Alcotest.failf "NOT NULL skip raised: %a" Db.pp_error e);
    Alcotest.(check (list string)) "the trigger's write was undone" [] (audit_rows db);
    check_view db "after a NOT NULL skip")
;;

(* Inside an explicit transaction the store is reverted by #631's statement
   savepoint instead. The materialisation is flushed at COMMIT, so it is read
   back after it — that is when a phantom would become visible to a reader. *)
let explicit_transaction_skip_leaves_no_phantom () =
  with_db (fun db ->
    seed db;
    exec db "BEGIN";
    exec db "INSERT OR IGNORE INTO t VALUES (1, 9)";
    Alcotest.(check (list string))
      "the trigger's write was undone inside the transaction"
      []
      (audit_rows db);
    exec db "COMMIT";
    Alcotest.(check (list string)) "and after COMMIT" [] (audit_rows db);
    check_view db "after an explicit-transaction skip")
;;

(* ------------------------------------------------------------------ *)
(* Controls: the restore must not fire too widely                      *)
(* ------------------------------------------------------------------ *)

(* A row that SURVIVES keeps its trigger's deltas. Without this the fix could
   be "drop the deltas whenever a savepoint or rollback is in play", which
   turns a phantom row into a missing one — equally wrong and equally silent. *)
let a_surviving_row_keeps_its_deltas () =
  with_db (fun db ->
    seed db;
    exec db "INSERT OR IGNORE INTO t VALUES (2, 7)";
    Alcotest.(check (list string)) "the trigger wrote" [ "2|g" ] (audit_rows db);
    check_view db "after a surviving autocommit insert";
    exec db "BEGIN";
    exec db "INSERT OR IGNORE INTO t VALUES (3, 8)";
    exec db "COMMIT";
    Alcotest.(check (list string))
      "the trigger wrote inside the transaction too"
      [ "2|g"; "3|g" ]
      (audit_rows db);
    check_view db "after a surviving transactional insert")
;;

(* The mark is taken per ROW, not per statement, so a multi-row VALUES list must
   drop only the skipped row's deltas. A mark that snapshotted the whole log per
   statement would restore over its siblings' writes as well; one that
   snapshotted per row but proportionally to the deltas already recorded would
   make this quadratic. Run in both transaction modes, since the two undo
   mechanisms differ. *)
let multi_row_values_drops_only_the_skipped_rows_deltas () =
  with_db (fun db ->
    seed db;
    exec db "INSERT OR IGNORE INTO t VALUES (2, 7), (1, 9), (3, 8)";
    Alcotest.(check (list string))
      "only the skipped row's trigger write is gone"
      [ "2|g"; "3|g" ]
      (audit_rows db);
    check_view db "after a mixed multi-row insert (autocommit)";
    exec db "BEGIN";
    exec db "INSERT OR IGNORE INTO t VALUES (4, 10), (2, 11), (5, 12)";
    exec db "COMMIT";
    Alcotest.(check (list string))
      "same inside a transaction"
      [ "2|g"; "3|g"; "4|g"; "5|g" ]
      (audit_rows db);
    check_view db "after a mixed multi-row insert (transaction)")
;;

(* A repeated skip must stay stable rather than drifting: the mark is restored
   every time, so nothing accumulates. *)
let repeated_skips_do_not_accumulate () =
  with_db (fun db ->
    seed db;
    for _ = 1 to 5 do
      exec db "INSERT OR IGNORE INTO t VALUES (1, 9)"
    done;
    Alcotest.(check (list string)) "still nothing in audit" [] (audit_rows db);
    check_view db "after five skips";
    exec db "INSERT OR IGNORE INTO t VALUES (2, 7)";
    check_view db "and a real insert still lands")
;;

(* ------------------------------------------------------------------ *)
(* #240: the name set is deliberately NOT reverted                     *)
(* ------------------------------------------------------------------ *)

(* The other half of the same accumulator, answering the other way on purpose.
   [audit] is still reported dirty by a statement whose write to it was undone,
   because a superfluous invalidation costs an external cache one miss while a
   missing one is a stale read. Asserting the SAFE direction explicitly is what
   keeps a later "consistency" change from quietly turning it into the unsafe
   one. *)
let dirty_names_survive_the_undone_write () =
  with_db (fun db ->
    seed db;
    match run (Db.execute_with_dirty db "INSERT OR IGNORE INTO t VALUES (1, 9)") with
    | Error e -> Alcotest.failf "execute_with_dirty: %a" Db.pp_error e
    | Ok dirty ->
      Alcotest.(check bool)
        "audit is still reported dirty though its row was undone"
        true
        (List.mem "audit" dirty);
      Alcotest.(check (list string)) "and audit really is empty" [] (audit_rows db))
;;

(* The delta half of the very same accumulator says the opposite, read through
   the public #417 surface rather than through the view. Keeping the two
   assertions adjacent is deliberate: they are the recorded decision. *)
let changes_do_not_survive_the_undone_write () =
  with_db (fun db ->
    seed db;
    match run (Db.execute_with_changes db "INSERT OR IGNORE INTO t VALUES (1, 9)") with
    | Error e -> Alcotest.failf "execute_with_changes: %a" Db.pp_error e
    | Ok changes ->
      Alcotest.(check (list string))
        "no table reports any row-level delta"
        []
        (List.filter_map
           (fun (tbl, cs) ->
              match cs with
              | [] -> None
              | _ :: _ -> Some tbl)
           changes
         |> List.sort compare))
;;

let () =
  Alcotest.run
    "rollback-deltas-666"
    [ ( "phantom-row"
      , [ Alcotest.test_case
            "autocommit: an alias-PK skip leaves no phantom"
            `Quick
            autocommit_alias_pk_skip_leaves_no_phantom
        ; Alcotest.test_case
            "autocommit: a UNIQUE-index skip leaves no phantom"
            `Quick
            autocommit_unique_index_skip_leaves_no_phantom
        ; Alcotest.test_case
            "autocommit: a NOT NULL skip leaves no phantom"
            `Quick
            autocommit_not_null_skip_leaves_no_phantom
        ; Alcotest.test_case
            "explicit transaction: a skip leaves no phantom either"
            `Quick
            explicit_transaction_skip_leaves_no_phantom
        ] )
    ; ( "controls"
      , [ Alcotest.test_case
            "a surviving row keeps its deltas"
            `Quick
            a_surviving_row_keeps_its_deltas
        ; Alcotest.test_case
            "multi-row VALUES drops only the skipped row's deltas"
            `Quick
            multi_row_values_drops_only_the_skipped_rows_deltas
        ; Alcotest.test_case
            "repeated skips do not accumulate"
            `Quick
            repeated_skips_do_not_accumulate
        ] )
    ; ( "accumulator-halves"
      , [ Alcotest.test_case
            "#240 dirty names survive the undone write"
            `Quick
            dirty_names_survive_the_undone_write
        ; Alcotest.test_case
            "#417 row deltas do not"
            `Quick
            changes_do_not_survive_the_undone_write
        ] )
    ]
;;
