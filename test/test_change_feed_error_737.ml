(** #737: a statement that returns [Error] no longer takes its reactive views
    down with it.

    [Db.drive_reactive] installs a change accumulator around every statement and
    hands it to [rv_absorb_changes] on success. On failure it used to drop the
    whole accumulator, on the premise that a raising statement's writes were
    rolled back with its transaction. That premise is false in two independent
    ways, and both leave the materialisation [_rv_<name>] {b missing rows} — the
    mirror image of #666's phantom row, and just as silent:

    - in a {b borrowed} transaction #631 deliberately RELEASES the statement
      savepoint on an exception rather than rolling it back, so the partial
      writes survive to the enclosing COMMIT;
    - in {b autocommit} a statement is not one transaction. A multi-row [VALUES]
      list and [INSERT ... SELECT] run one [execute_insert] — and one COMMIT —
      per row, so a failure on row k leaves rows 1..k-1 durably committed. The
      issue text calls autocommit correct; it is not, and
      [autocommit_multi_row_insert_keeps_its_committed_rows] is that case.

    {b The fix is a resync, not an absorb, and one test decides that.}
    Absorbing the accumulator on [Error] is the obvious mirror of #666 and it is
    {b not sufficient}: [execute_insert] records its [Inserted] delta only after
    [execute_insert_write] returns, and the AFTER INSERT trigger fires inside
    it — so a raising AFTER trigger in a borrowed transaction leaves the row in
    the store with no delta recorded anywhere.
    [borrowed_after_insert_trigger_raise_keeps_the_written_row] is that shape,
    and it fails under an absorbing fix exactly as it failed before. Dropping
    the untrusted deltas and rebuilding the views from the base tables is exact
    whatever the write path did, and it is the same answer [ROLLBACK TO] has
    always given for the same reason (#427).

    {b Every defect case reads the materialisation back and compares it against
    the authoritative query over the base table.} A test that counted rows in
    the base table alone would pass throughout — the base table was always
    right.

    {b The controls pin the narrowing, and they need a seam.} A resync is
    invisible when the view is already correct, so the controls write a bogus
    row straight into [_rv_av] first: a resync rebuilds from the base table and
    removes it, an incremental flush leaves it. That is the only way to tell
    "no resync was scheduled" from "a resync happened and changed nothing", and
    without it a fix that resynced on every error — including a parse error —
    would look identical. *)

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

(* The statement under test is expected to fail; its error is the interesting
   part of the fixture, so a success is a broken fixture rather than a pass. *)
let exec_expect_error db sql =
  match run (Db.execute db sql) with
  | Error _ -> ()
  | Ok () -> Alcotest.failf "exec %S was expected to fail and did not" sql
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

(* The issue in one assertion: the materialisation must equal the authoritative
   query over the base table. *)
let check_view db label =
  Alcotest.(check (list string))
    (label ^ ": _rv_av = authoritative GROUP BY over base")
    (texts db "SELECT grp, COUNT(*) FROM base GROUP BY grp")
    (texts db "SELECT * FROM _rv_av")
;;

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  go 0
;;

let base_rows db = texts db "SELECT id, grp FROM base"
let view_rows db = texts db "SELECT * FROM _rv_av"

(* A COUNT/GROUP BY view, i.e. a DELTA-maintainable shape. An unmaintainable
   one (MIN) is refreshed in full on every flush and could not show the defect
   at all. *)
let seed db =
  exec db "CREATE TABLE base (id INTEGER PRIMARY KEY, grp TEXT)";
  exec db "CREATE REACTIVE VIEW av AS SELECT grp, COUNT(*) FROM base GROUP BY grp"
;;

(* ------------------------------------------------------------------ *)
(* The defect: rows that survived the error, deltas that did not       *)
(* ------------------------------------------------------------------ *)

(* The issue's own repro. Rows 1 and 2 are written, row 3 collides with row 1,
   and #631 keeps the first two inside the borrowed transaction. Before the fix
   the view reported nothing at all. *)
let borrowed_multi_row_insert_keeps_its_surviving_rows () =
  with_db (fun db ->
    seed db;
    exec db "BEGIN";
    exec_expect_error db "INSERT INTO base VALUES (1,'g'),(2,'g'),(1,'g')";
    exec db "COMMIT";
    Alcotest.(check (list string))
      "the partial writes survived the error"
      [ "1|g"; "2|g" ]
      (base_rows db);
    check_view db "after a partially-applied INSERT in a transaction")
;;

(* Autocommit is NOT the safe half. The same statement commits per row, so rows
   1 and 2 are durable even though nothing wrapped them. *)
let autocommit_multi_row_insert_keeps_its_committed_rows () =
  with_db (fun db ->
    seed db;
    exec_expect_error db "INSERT INTO base VALUES (1,'g'),(2,'g'),(1,'g')";
    Alcotest.(check (list string))
      "the committed rows survived the error"
      [ "1|g"; "2|g" ]
      (base_rows db);
    check_view db "after a partially-applied INSERT in autocommit")
;;

(* [INSERT ... SELECT] is the second per-row-committing spelling and reaches a
   different function ([execute_insert_select_op]). *)
let autocommit_insert_select_keeps_its_committed_rows () =
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE src (id INTEGER, grp TEXT)";
    exec db "INSERT INTO src VALUES (1,'g'),(2,'g'),(1,'g')";
    exec_expect_error db "INSERT INTO base SELECT id, grp FROM src";
    Alcotest.(check (list string))
      "the committed rows survived the error"
      [ "1|g"; "2|g" ]
      (base_rows db);
    check_view db "after a partially-applied INSERT ... SELECT")
;;

(* The case that rules out absorbing the accumulator. The row is written by
   [execute_insert_write], the AFTER INSERT trigger raises inside it, and
   [execute_insert]'s [record_change] — which runs after it returns — never
   happens. So the row survives the borrowed transaction with NO delta
   describing it, and only a rebuild from the base table can find it. *)
let borrowed_after_insert_trigger_raise_keeps_the_written_row () =
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE guard (id INTEGER PRIMARY KEY)";
    exec db "INSERT INTO guard VALUES (1)";
    exec
      db
      "CREATE TRIGGER b_ai AFTER INSERT ON base BEGIN INSERT INTO guard VALUES (1); END";
    exec db "BEGIN";
    exec_expect_error db "INSERT INTO base VALUES (1,'g')";
    exec db "COMMIT";
    Alcotest.(check (list string))
      "the row survived, though no delta was ever recorded for it"
      [ "1|g" ]
      (base_rows db);
    check_view db "after a raising AFTER INSERT trigger")
;;

(* [execute_update_op] is a different exception handler again, and its write
   loop records each row's delta as it goes. The NOT NULL violation is reached
   through [write_row_rekeyed] on the SECOND row, so the first row's move is
   already committed to the borrowed transaction when the statement raises.
   Spelled as a CASE rather than a literal NULL so [Sema]'s static check does
   not reject the statement before it runs.

   The pre-pass shapes do NOT reach this: [validate_update_unique] runs before
   the write loop, so a UNIQUE violation raises with nothing yet written. That
   is why this case uses NOT NULL. *)
let borrowed_update_partial_failure_keeps_its_rows () =
  with_db (fun db ->
    exec db "CREATE TABLE base (id INTEGER PRIMARY KEY, grp TEXT NOT NULL)";
    exec db "CREATE REACTIVE VIEW av AS SELECT grp, COUNT(*) FROM base GROUP BY grp";
    exec db "INSERT INTO base VALUES (1,'a'),(2,'b'),(3,'c')";
    check_view db "seeded";
    exec db "BEGIN";
    exec_expect_error db "UPDATE base SET grp = CASE WHEN id = 2 THEN NULL ELSE 'z' END";
    exec db "COMMIT";
    Alcotest.(check (list string))
      "the first row moved and stayed moved"
      [ "1|z"; "2|b"; "3|c" ]
      (base_rows db);
    check_view db "after a partially-applied UPDATE in a transaction")
;;

(* [execute_delete_op]'s handler, reached through an AFTER DELETE trigger that
   raises once every row has already been deleted and recorded. *)
let borrowed_delete_with_a_raising_after_trigger () =
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE guard (id INTEGER PRIMARY KEY)";
    exec db "INSERT INTO guard VALUES (1)";
    exec db "INSERT INTO base VALUES (1,'a'),(2,'b')";
    check_view db "seeded";
    exec
      db
      "CREATE TRIGGER b_ad AFTER DELETE ON base BEGIN INSERT INTO guard VALUES (1); END";
    exec db "BEGIN";
    exec_expect_error db "DELETE FROM base WHERE id = 1";
    exec db "COMMIT";
    check_view db "after a raising AFTER DELETE trigger")
;;

(* ------------------------------------------------------------------ *)
(* The seam: is a resync scheduled, or not?                            *)
(* ------------------------------------------------------------------ *)

(* [_rv_av] is an ordinary table, so a bogus row can be written into it
   directly. A resync rebuilds the view from [base] and removes it; an
   incremental flush leaves it alone. Nothing else distinguishes the two, since
   a resync of an already-correct view produces no visible change. *)
let corrupt_the_materialisation db = exec db "INSERT INTO _rv_av VALUES ('zz', 99)"
let sentinel_present db = List.mem "zz|99" (view_rows db)

(* The narrowing that keeps the error path cheap: the common `try INSERT, catch
   UNIQUE` in autocommit rolls its per-row transaction back and records
   nothing, so no rebuild is scheduled for it. Without the sentinel this case
   would pass against a fix that resynced on every error. *)
let a_failing_autocommit_statement_that_wrote_nothing_schedules_no_resync () =
  with_db (fun db ->
    seed db;
    exec db "INSERT INTO base VALUES (1,'g')";
    corrupt_the_materialisation db;
    exec_expect_error db "INSERT INTO base VALUES (1,'h')";
    Alcotest.(check bool)
      "no resync was scheduled for a statement that wrote nothing"
      true
      (sentinel_present db))
;;

(* Neither does a statement that never reached the executor. *)
let parse_and_sema_errors_schedule_no_resync () =
  with_db (fun db ->
    seed db;
    exec db "INSERT INTO base VALUES (1,'g')";
    corrupt_the_materialisation db;
    exec_expect_error db "SELECT nope FROM base";
    exec_expect_error db "syntax ~ error";
    Alcotest.(check bool)
      "no resync was scheduled for a statement that never ran"
      true
      (sentinel_present db))
;;

(* The positive half of the same seam: inside a transaction any error may have
   left writes behind — including the no-delta shape above — so the rebuild is
   scheduled unconditionally, and it really does rebuild. *)
let a_failing_statement_inside_a_transaction_does_schedule_one () =
  with_db (fun db ->
    seed db;
    exec db "INSERT INTO base VALUES (1,'g')";
    corrupt_the_materialisation db;
    exec db "BEGIN";
    exec_expect_error db "INSERT INTO base VALUES (1,'h')";
    exec db "COMMIT";
    Alcotest.(check bool)
      "the transaction's COMMIT rebuilt the view from the base table"
      false
      (sentinel_present db);
    check_view db "after the rebuild")
;;

(* A SUCCEEDING statement must still take the incremental path — the fix must
   not have turned every flush into a rebuild, which would hide a real
   maintenance regression behind a full recompute. The sentinel survives an
   incremental flush and would not survive a resync. *)
let a_succeeding_statement_still_maintains_incrementally () =
  with_db (fun db ->
    seed db;
    exec db "INSERT INTO base VALUES (1,'g')";
    corrupt_the_materialisation db;
    exec db "INSERT INTO base VALUES (2,'g')";
    Alcotest.(check bool)
      "the successful insert was applied incrementally, not resynced"
      true
      (sentinel_present db);
    Alcotest.(check bool) "and its own delta landed" true (List.mem "g|2" (view_rows db)))
;;

(* ------------------------------------------------------------------ *)
(* Controls around the error itself                                    *)
(* ------------------------------------------------------------------ *)

(* The statement's own error is what the caller gets back; the maintenance the
   failure schedules must not replace it with something about a view. *)
let the_statements_own_error_is_still_returned () =
  with_db (fun db ->
    seed db;
    match run (Db.execute db "INSERT INTO base VALUES (1,'g'),(2,'g'),(1,'g')") with
    | Ok () -> Alcotest.fail "expected the UNIQUE violation"
    | Error e ->
      let msg = Format.asprintf "%a" Db.pp_error e in
      Alcotest.(check bool)
        (Printf.sprintf "error still names the UNIQUE violation: %s" msg)
        true
        (contains ~needle:"UNIQUE constraint failed" msg))
;;

(* A rolled-back transaction restores the base table to what the
   materialisation already reflects, so the view must be correct afterwards
   whether or not a rebuild was scheduled inside it. *)
let a_rolled_back_transaction_leaves_the_view_correct () =
  with_db (fun db ->
    seed db;
    exec db "INSERT INTO base VALUES (9,'z')";
    check_view db "seeded";
    exec db "BEGIN";
    exec_expect_error db "INSERT INTO base VALUES (1,'g'),(2,'g'),(1,'g')";
    exec db "ROLLBACK";
    Alcotest.(check (list string)) "the transaction was undone" [ "9|z" ] (base_rows db);
    exec db "INSERT INTO base VALUES (8,'z')";
    check_view db "after a rolled-back partial failure")
;;

let () =
  Alcotest.run
    "change-feed-error-737"
    [ ( "missing-row"
      , [ Alcotest.test_case
            "borrowed: a partially-applied INSERT keeps its surviving rows"
            `Quick
            borrowed_multi_row_insert_keeps_its_surviving_rows
        ; Alcotest.test_case
            "autocommit: a partially-applied INSERT keeps its committed rows"
            `Quick
            autocommit_multi_row_insert_keeps_its_committed_rows
        ; Alcotest.test_case
            "autocommit: so does INSERT ... SELECT"
            `Quick
            autocommit_insert_select_keeps_its_committed_rows
        ; Alcotest.test_case
            "borrowed: a raising AFTER INSERT trigger leaves a row with no delta"
            `Quick
            borrowed_after_insert_trigger_raise_keeps_the_written_row
        ; Alcotest.test_case
            "borrowed: a partially-applied UPDATE"
            `Quick
            borrowed_update_partial_failure_keeps_its_rows
        ; Alcotest.test_case
            "borrowed: a DELETE whose AFTER trigger raises"
            `Quick
            borrowed_delete_with_a_raising_after_trigger
        ] )
    ; ( "resync-or-not"
      , [ Alcotest.test_case
            "autocommit: a statement that wrote nothing schedules no resync"
            `Quick
            a_failing_autocommit_statement_that_wrote_nothing_schedules_no_resync
        ; Alcotest.test_case
            "neither does a parse or sema error"
            `Quick
            parse_and_sema_errors_schedule_no_resync
        ; Alcotest.test_case
            "a failing statement inside a transaction does"
            `Quick
            a_failing_statement_inside_a_transaction_does_schedule_one
        ; Alcotest.test_case
            "a succeeding statement still maintains incrementally"
            `Quick
            a_succeeding_statement_still_maintains_incrementally
        ] )
    ; ( "controls"
      , [ Alcotest.test_case
            "the statement's own error is still returned"
            `Quick
            the_statements_own_error_is_still_returned
        ; Alcotest.test_case
            "a rolled-back transaction leaves the view correct"
            `Quick
            a_rolled_back_transaction_leaves_the_view_correct
        ] )
    ]
;;
