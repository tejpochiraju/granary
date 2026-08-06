(** #639 and #631: two defects on the same INSERT path, both silent.

    {b #639 — the conflict modifier ate the ON CONFLICT clause.} [CA_ignore]
    matched above the upsert arm in both places that resolve a conflict
    ([check_insert_unique] for a secondary UNIQUE index, [execute_insert_write]'s
    [put_x] arm for the rowid-alias PK), so [INSERT OR IGNORE ... ON CONFLICT(k)
    DO UPDATE] silently skipped instead of running the DO UPDATE. A caller who
    writes both is asking for "insert, or update on conflict, and skip rows that
    violate a constraint" and got "insert, or do nothing" — for {i any} conflict,
    not just the NULL binding the issue was found with.

    The rule the fix pins: {b an explicit ON CONFLICT target beats the
    statement's conflict-resolution modifier, for the index it names}, and the
    modifier still governs every other index. That is SQLite's rule and it is
    the only reading under which writing both clauses means anything. It applies
    to [OR REPLACE] as well as [OR IGNORE] — see
    [or_replace_also_defers_to_the_conflict_target], which is a behaviour change
    the issue did not name and which is pinned deliberately rather than left to
    the arm ordering.

    {b The target is resolved in its own pass, before the modifier sees
    anything}, and the tests that matter most here are the ones that pin why.
    The first cut of this fix merely reordered the match arms inside a single
    fold over the unique indexes, which left the answer depending on the order
    [Cat.indexes_for_table] happened to return them in — i.e. on [CREATE UNIQUE
    INDEX] order — whenever a row conflicted on the target {i and} on another
    index at once. Under [OR IGNORE] one order ran the DO UPDATE and the other
    silently skipped; under [OR REPLACE] one order queued a delete that
    [execute_insert]'s upsert branch then threw away. Those are
    [a_conflict_on_the_target_and_another_index_is_order_independent],
    [or_replace_with_a_second_conflict_displaces_nothing] and
    [an_alias_pk_target_beats_a_secondary_conflict] — the last because the
    rowid-alias PK carries no index and so is invisible to that fold, and needs
    its own probe before [check_insert_unique] runs at all.

    NOT NULL is {b not} a uniqueness conflict, so an ON CONFLICT clause never
    intercepts it — the modifier does, exactly as #599 decided. Three
    consequences are pinned here because they pull in different directions:

    - a NULL in the row being {i inserted} skips under [OR IGNORE], and now does
      so for both conflict shapes ([not_null_upsert_skip_is_index_independent]).
      It used to depend on which index the row collided with: an alias-PK
      collision reached [execute_insert_write]'s [null_skip] and skipped, a
      secondary-index collision resolved to [upsert_rowid] first and ran the
      DO UPDATE.
    - the same NULL under a {i raising} modifier raises, and it took a second
      review to get that right: the alias pre-probe routes a conflicting row
      past [execute_insert_write] altogether, which silently turned the raise
      into a successful DO UPDATE. The check now sits in [execute_insert] for
      any statement carrying an upsert clause.
      [a_conflicting_insert_half_null_still_raises] covers both shapes with a
      row that actually collides — [insert_half_null_still_raises_without_or_ignore]
      does not, because it inserts a non-conflicting key, which is exactly how
      the regression got in. [a_plain_insert_keeps_its_error_precedence] pins
      the other side: with no upsert clause nothing moved.
    - a NULL {i assigned by the DO UPDATE} raises, because that write funnels
      through [write_row_rekeyed] → [enforce_not_null], which #599 requires to
      stay unconditional. That is the exact statement in the issue, and it is
      what the sqlite3 3.45.1 oracle reports.

    {b What [write_row_rekeyed] does NOT do is check uniqueness.} Its index loop
    is an unconditional del/put, and [check_index_unique_on_update] is reached
    only from the plain-UPDATE pre-pass. So a DO UPDATE can write a duplicate
    into a UNIQUE index — pre-existing, true on [main] too, tracked as #667, and
    deliberately not pinned as correct anywhere below.

    {b #631 — a skipped row's trigger side effects survived a BEGIN.} A BEFORE
    INSERT trigger whose body performs nested DML, on a row then skipped by
    [OR IGNORE], had its writes rolled back in autocommit ([owned = true] →
    [S.rollback]) and {i committed} inside an explicit transaction ([owned =
    false] → nothing). The undo was keyed on who owns the transaction, not on
    what the statement decided. It is now a statement-level savepoint, and it
    covers both skip reasons — the long-standing UNIQUE skip and #599's NOT NULL
    skip — because it keys on "the statement wrote nothing", not on why. *)

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
;;

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let prepare db sql =
  run
    (let* r = Db.prepare db sql in
     match r with
     | Ok st -> Lwt.return st
     | Error e -> Alcotest.failf "prepare %S: %a" sql Db.pp_error e)
;;

let run_stmt db sql params =
  let st = prepare db sql in
  run (Db.run st ~params)
;;

let expect_error db ~needle sql params =
  match run_stmt db sql params with
  | Ok n -> Alcotest.failf "%S was expected to fail with %S, wrote %d rows" sql needle n
  | Error e ->
    let msg = Format.asprintf "%a" Db.pp_error e in
    Alcotest.(check bool)
      (Printf.sprintf "%S failed with %S (got %S)" sql needle msg)
      true
      (contains ~needle msg)
;;

let expect_rows db ~msg want sql = Alcotest.(check (list string)) msg want (texts db sql)

(* Every spelling of the conflict-resolution modifier, in the order #599 pins
   them. [""] is the bare INSERT. *)
let all_modifiers =
  [ ""; "OR ABORT "; "OR FAIL "; "OR ROLLBACK "; "OR IGNORE "; "OR REPLACE " ]
;;

(* Two tables, because the two conflict shapes are resolved by two different
   functions and #639 was present in BOTH. [t] conflicts on the rowid-alias
   PRIMARY KEY (resolved by [execute_insert_write]'s [put_x] arm); [s] conflicts
   on a secondary UNIQUE index (resolved by [check_insert_unique]). Tests that
   care about the distinction run over both. *)
let seed_alias db =
  exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER NOT NULL)";
  exec db "INSERT INTO t VALUES (1, 5)"
;;

let seed_secondary db =
  exec db "CREATE TABLE s (id INTEGER PRIMARY KEY, k INTEGER, v INTEGER NOT NULL)";
  exec db "CREATE UNIQUE INDEX s_k ON s (k)";
  exec db "INSERT INTO s VALUES (1, 100, 5)"
;;

(* ------------------------------------------------------------------ *)
(* #639 — the defect itself                                            *)
(* ------------------------------------------------------------------ *)

(* The issue's own statement, both conflict shapes. Before the fix each of
   these reported 0 rows and left the stored row untouched, silently. *)
let or_ignore_runs_the_do_update () =
  with_db (fun db ->
    seed_alias db;
    (match
       run_stmt
         db
         "INSERT OR IGNORE INTO t VALUES (1, 9) ON CONFLICT(k) DO UPDATE SET v = 42"
         []
     with
     | Ok _ -> ()
     | Error e ->
       Alcotest.failf "alias-PK upsert under OR IGNORE raised: %a" Db.pp_error e);
    expect_rows db ~msg:"alias PK: the DO UPDATE ran" [ "1|42" ] "SELECT * FROM t");
  with_db (fun db ->
    seed_secondary db;
    (match
       run_stmt
         db
         "INSERT OR IGNORE INTO s VALUES (2, 100, 9) ON CONFLICT(k) DO UPDATE SET v = 42"
         []
     with
     | Ok _ -> ()
     | Error e ->
       Alcotest.failf "secondary upsert under OR IGNORE raised: %a" Db.pp_error e);
    expect_rows
      db
      ~msg:"secondary index: the DO UPDATE ran, no second row was inserted"
      [ "1|100|42" ]
      "SELECT * FROM s")
;;

(* WITH the ON CONFLICT clause, every modifier runs the DO UPDATE — the target
   wins over all six spellings, so the answer does not depend on which one the
   caller wrote. Only [OR IGNORE] changed here; the other five are pinned so a
   later "simplification" of the arm order cannot quietly re-split them. *)
let every_modifier_runs_the_do_update () =
  List.iter
    (fun modifier ->
       with_db (fun db ->
         seed_alias db;
         let sql =
           Printf.sprintf
             "INSERT %sINTO t VALUES (1, 9) ON CONFLICT(k) DO UPDATE SET v = 42"
             modifier
         in
         (match run_stmt db sql [] with
          | Ok _ -> ()
          | Error e -> Alcotest.failf "%S raised: %a" sql Db.pp_error e);
         expect_rows
           db
           ~msg:(Printf.sprintf "%S ran the DO UPDATE" modifier)
           [ "1|42" ]
           "SELECT * FROM t"))
    all_modifiers
;;

(* WITHOUT the ON CONFLICT clause, the modifier is the only thing deciding, and
   its long-standing meanings are unchanged. This is the control for the test
   above: the same six spellings, the same conflicting row, three answers. *)
let modifiers_without_a_conflict_target_are_unchanged () =
  let expected =
    [ "", `Raises
    ; "OR ABORT ", `Raises
    ; "OR FAIL ", `Raises
    ; "OR ROLLBACK ", `Raises
    ; "OR IGNORE ", `Skips
    ; "OR REPLACE ", `Replaces
    ]
  in
  List.iter
    (fun (modifier, outcome) ->
       with_db (fun db ->
         seed_alias db;
         let sql = Printf.sprintf "INSERT %sINTO t VALUES (1, 9)" modifier in
         (match outcome with
          | `Raises -> expect_error db ~needle:"UNIQUE constraint failed" sql []
          | `Skips ->
            (match run_stmt db sql [] with
             | Ok n -> Alcotest.(check int) "OR IGNORE reports 0 rows" 0 n
             | Error e -> Alcotest.failf "OR IGNORE raised: %a" Db.pp_error e)
          | `Replaces ->
            (match run_stmt db sql [] with
             | Ok _ -> ()
             | Error e -> Alcotest.failf "OR REPLACE raised: %a" Db.pp_error e));
         let want =
           match outcome with
           | `Replaces -> [ "1|9" ]
           | _ -> [ "1|5" ]
         in
         expect_rows
           db
           ~msg:(Printf.sprintf "%S without a conflict target" modifier)
           want
           "SELECT * FROM t"))
    expected
;;

(* [OR REPLACE] is the arm the reorder moves that the issue does NOT mention,
   and the change is real: it used to delete the conflicting row and insert the
   new one, and now defers to the DO UPDATE for the index the ON CONFLICT clause
   names. Pinned on the secondary-index shape because that is where the two
   outcomes are distinguishable — a REPLACE would leave rowid 2 with k=100,
   whereas the DO UPDATE leaves rowid 1. *)
let or_replace_also_defers_to_the_conflict_target () =
  with_db (fun db ->
    seed_secondary db;
    exec
      db
      "INSERT OR REPLACE INTO s VALUES (2, 100, 9) ON CONFLICT(k) DO UPDATE SET v = 42";
    expect_rows
      db
      ~msg:"the original row was updated in place, not deleted and re-inserted"
      [ "1|100|42" ]
      "SELECT * FROM s")
;;

(* The target is INDEX-SPECIFIC: a conflict on an index the ON CONFLICT clause
   does not name still falls to the modifier. This is the half of the rule that
   the reorder must not swallow — if the upsert arm matched unconditionally,
   [OR IGNORE] would stop skipping. *)
let the_modifier_still_governs_other_indexes () =
  let setup db =
    exec
      db
      "CREATE TABLE m (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, v INTEGER NOT NULL)";
    exec db "CREATE UNIQUE INDEX m_a ON m (a)";
    exec db "CREATE UNIQUE INDEX m_b ON m (b)";
    exec db "INSERT INTO m VALUES (1, 10, 20, 5)"
  in
  (* Conflicts on b; the clause names a. OR IGNORE must skip, not update. *)
  with_db (fun db ->
    setup db;
    (match
       run_stmt
         db
         "INSERT OR IGNORE INTO m VALUES (2, 11, 20, 9) ON CONFLICT(a) DO UPDATE SET v = \
          42"
         []
     with
     | Ok n -> Alcotest.(check int) "the unnamed-index conflict is skipped" 0 n
     | Error e -> Alcotest.failf "OR IGNORE raised on an unnamed index: %a" Db.pp_error e);
    expect_rows db ~msg:"nothing was updated" [ "1|10|20|5" ] "SELECT * FROM m");
  (* Same statement, bare: the unnamed index still raises. *)
  with_db (fun db ->
    setup db;
    expect_error
      db
      ~needle:"UNIQUE constraint failed"
      "INSERT INTO m VALUES (2, 11, 20, 9) ON CONFLICT(a) DO UPDATE SET v = 42"
      [];
    expect_rows db ~msg:"nothing was written" [ "1|10|20|5" ] "SELECT * FROM m");
  (* And the NAMED index still upserts, in the same schema. *)
  with_db (fun db ->
    setup db;
    exec
      db
      "INSERT OR IGNORE INTO m VALUES (2, 10, 21, 9) ON CONFLICT(a) DO UPDATE SET v = 42";
    expect_rows
      db
      ~msg:"the named-index conflict updated"
      [ "1|10|20|42" ]
      "SELECT * FROM m")
;;

(* The case the reorder NEWLY created, caught in review of PR #652: a row that
   conflicts on the named index AND on another one at the same time. Before the
   target got its own pass, [check_insert_unique] folded over every unique index
   and let whichever conflicted first decide — and [Schema_cache.by_table_add]
   prepends, so [indexes_for_table] is newest-index-first, i.e. the fold order is
   [CREATE UNIQUE INDEX] order reversed.

   So the SAME schema and the SAME statement gave two answers depending on which
   index was created last: target first → [upsert_rid] set, then the other index
   set [skip], which [execute_insert]'s upsert branch discarded, and the DO
   UPDATE ran; other index first → [skip] won, [upsert_rid] stayed [None], and
   the row was silently skipped — #639 unfixed.

   Both creation orders are run below and must agree. The order is the whole
   point of the test, so it must not be "tidied" into one. *)
let a_conflict_on_the_target_and_another_index_is_order_independent () =
  let setup db ~target_index_last =
    exec
      db
      "CREATE TABLE m (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, v INTEGER NOT NULL)";
    if target_index_last
    then (
      exec db "CREATE UNIQUE INDEX m_b ON m (b)";
      exec db "CREATE UNIQUE INDEX m_a ON m (a)")
    else (
      exec db "CREATE UNIQUE INDEX m_a ON m (a)";
      exec db "CREATE UNIQUE INDEX m_b ON m (b)");
    exec db "INSERT INTO m VALUES (1, 10, 20, 5)"
  in
  List.iter
    (fun target_index_last ->
       let label =
         if target_index_last then "target index created last" else "target index first"
       in
       with_db (fun db ->
         setup db ~target_index_last;
         (* Conflicts on BOTH a (the named target) and b. The target wins. *)
         (match
            run_stmt
              db
              "INSERT OR IGNORE INTO m VALUES (2, 10, 20, 9) ON CONFLICT(a) DO UPDATE \
               SET v = 42"
              []
          with
          | Ok n -> Alcotest.(check int) (label ^ ": the DO UPDATE ran") 1 n
          | Error e -> Alcotest.failf "%s: raised: %a" label Db.pp_error e);
         expect_rows
           db
           ~msg:(label ^ ": updated in place, whichever order the indexes were built")
           [ "1|10|20|42" ]
           "SELECT * FROM m"))
    [ false; true ]
;;

(* The same collision under OR REPLACE, which is where the discarded accumulator
   cost a DELETE rather than an UPDATE. With rows (1,10,20,5) and (2,11,21,6), a
   row conflicting with row 1 on the named index [a] and with row 2 on [b] used
   to queue row 2 for deletion and then take the upsert branch — which never
   calls [delete_replace_conflicts], so the delete was silently dropped.

   The answer the fix pins is that NOTHING is displaced: the target supersedes
   the insert, so the row that would have conflicted with row 2 is never
   written, and row 2 has no reason to go. *)
let or_replace_with_a_second_conflict_displaces_nothing () =
  with_db (fun db ->
    exec
      db
      "CREATE TABLE m (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, v INTEGER NOT NULL)";
    exec db "CREATE UNIQUE INDEX m_a ON m (a)";
    exec db "CREATE UNIQUE INDEX m_b ON m (b)";
    exec db "INSERT INTO m VALUES (1, 10, 20, 5)";
    exec db "INSERT INTO m VALUES (2, 11, 21, 6)";
    exec
      db
      "INSERT OR REPLACE INTO m VALUES (3, 10, 21, 9) ON CONFLICT(a) DO UPDATE SET v = 42";
    expect_rows
      db
      ~msg:"row 1 updated in place and row 2 was NOT deleted"
      [ "1|10|20|42"; "2|11|21|6" ]
      "SELECT * FROM m ORDER BY id")
;;

(* The same collision under the RAISING modifiers, which is a behaviour change
   from `main` and needs saying out loud.

   On `main` the single fold reached `m_b`'s catch-all and raised
   "UNIQUE constraint failed: m.b". With the target probed first the fold over
   `others` is never entered, so the DO UPDATE runs. That is the right answer:
   the `b` conflict belonged to the row being INSERTED, and that row is
   discarded — row 1's own `b` is untouched by `SET v = 42`, so there is no
   duplicate to report. `main` was raising a false positive.

   What this does NOT establish is that the DO UPDATE's own writes are
   uniqueness-checked. They are not — see #667. A `DO UPDATE SET b = 21` here
   would silently write a duplicate into `m_b`, on this revision and on `main`
   alike. Deliberately not pinned as correct: it is a pre-existing gap, not a
   decision. *)
let raising_modifiers_no_longer_report_the_discarded_rows_conflict () =
  List.iter
    (fun modifier ->
       with_db (fun db ->
         exec
           db
           "CREATE TABLE m (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, v INTEGER NOT \
            NULL)";
         exec db "CREATE UNIQUE INDEX m_a ON m (a)";
         exec db "CREATE UNIQUE INDEX m_b ON m (b)";
         exec db "INSERT INTO m VALUES (1, 10, 20, 5)";
         exec db "INSERT INTO m VALUES (2, 11, 21, 6)";
         let sql =
           Printf.sprintf
             "INSERT %sINTO m VALUES (3, 10, 21, 9) ON CONFLICT(a) DO UPDATE SET v = 42"
             modifier
         in
         (match run_stmt db sql [] with
          | Ok n ->
            Alcotest.(check int) (Printf.sprintf "%S ran the DO UPDATE" modifier) 1 n
          | Error e ->
            Alcotest.failf
              "%S raised on the discarded row's conflict: %a"
              sql
              Db.pp_error
              e);
         expect_rows
           db
           ~msg:(Printf.sprintf "%S updated row 1 and left row 2 alone" modifier)
           [ "1|10|20|42"; "2|11|21|6" ]
           "SELECT * FROM m ORDER BY id"))
    [ ""; "OR ABORT "; "OR FAIL "; "OR ROLLBACK " ]
;;

(* The rowid-alias PK is the other shape an ON CONFLICT clause can name, and it
   carries no index, so [check_insert_unique] cannot see it. Without the
   pre-probe a secondary conflict decided first: under OR IGNORE the row was
   skipped and the DO UPDATE lost, under OR REPLACE the secondary row was
   deleted before [put_x] even discovered the alias conflict. *)
let an_alias_pk_target_beats_a_secondary_conflict () =
  let setup db =
    exec db "CREATE TABLE w (k INTEGER PRIMARY KEY, b INTEGER, v INTEGER NOT NULL)";
    exec db "CREATE UNIQUE INDEX w_b ON w (b)";
    exec db "INSERT INTO w VALUES (1, 20, 5)";
    exec db "INSERT INTO w VALUES (2, 21, 6)"
  in
  (* Conflicts on the alias PK (k=1, the named target) AND on w_b against row 2. *)
  with_db (fun db ->
    setup db;
    (match
       run_stmt
         db
         "INSERT OR IGNORE INTO w VALUES (1, 21, 9) ON CONFLICT(k) DO UPDATE SET v = 42"
         []
     with
     | Ok n -> Alcotest.(check int) "the DO UPDATE ran, not the skip" 1 n
     | Error e ->
       Alcotest.failf "alias target lost to a secondary conflict: %a" Db.pp_error e);
    expect_rows
      db
      ~msg:"row 1 updated, row 2 untouched"
      [ "1|20|42"; "2|21|6" ]
      "SELECT * FROM w ORDER BY k");
  (* Same collision under OR REPLACE: row 2 must NOT be displaced. *)
  with_db (fun db ->
    setup db;
    exec
      db
      "INSERT OR REPLACE INTO w VALUES (1, 21, 9) ON CONFLICT(k) DO UPDATE SET v = 42";
    expect_rows
      db
      ~msg:"row 1 updated in place and row 2 survives"
      [ "1|20|42"; "2|21|6" ]
      "SELECT * FROM w ORDER BY k");
  (* And the alias target with NO secondary conflict still upserts — the probe
     must not have broken the plain case. *)
  with_db (fun db ->
    setup db;
    exec
      db
      "INSERT OR IGNORE INTO w VALUES (1, 22, 9) ON CONFLICT(k) DO UPDATE SET v = 42";
    expect_rows
      db
      ~msg:"plain alias-PK upsert still works"
      [ "1|20|42"; "2|21|6" ]
      "SELECT * FROM w ORDER BY k")
;;

(* A non-unique index whose columns happen to match the ON CONFLICT list is NOT
   a conflict target — a target must name a uniqueness constraint. Pinned
   because [index_is_conflict_target] tests [idx_unique] and dropping that test
   would silently turn any same-column index into one.

   {b This pins a divergence from SQLite, which is recorded in CLAUDE.md rather
   than assumed.} SQLite REJECTS the statement outright — "ON CONFLICT clause
   does not match any PRIMARY KEY or UNIQUE constraint" — where granary treats
   it as a plain INSERT and silently ignores the clause. The divergence is
   pre-existing (nothing has ever validated a conflict target against the
   schema) and #639 does not fix it; it is tracked as #668. What #639 does fix
   is the part that mattered here: whatever the clause names, it cannot promote
   a non-unique index into a target. *)
let a_non_unique_index_is_not_a_conflict_target () =
  with_db (fun db ->
    exec db "CREATE TABLE n (id INTEGER PRIMARY KEY, a INTEGER, v INTEGER NOT NULL)";
    exec db "CREATE INDEX n_a ON n (a)";
    exec db "INSERT INTO n VALUES (1, 10, 5)";
    (* No uniqueness on a, so nothing conflicts: this is a plain insert. *)
    (match
       run_stmt
         db
         "INSERT OR IGNORE INTO n VALUES (2, 10, 9) ON CONFLICT(a) DO UPDATE SET v = 42"
         []
     with
     | Ok n -> Alcotest.(check int) "inserted, not upserted" 1 n
     | Error e -> Alcotest.failf "raised: %a" Db.pp_error e);
    expect_rows
      db
      ~msg:"both rows present; no DO UPDATE ran"
      [ "1|10|5"; "2|10|9" ]
      "SELECT * FROM n ORDER BY id")
;;

(* ------------------------------------------------------------------ *)
(* #639 × #599 — NOT NULL through the combination                      *)
(* ------------------------------------------------------------------ *)

(* The issue's exact case: the NULL is in the DO UPDATE's assignment, so it is
   the UPDATE half that violates NOT NULL. That write funnels through
   [write_row_rekeyed] → [enforce_not_null], which #599 requires to stay
   unconditional (plain UPDATE and ON UPDATE CASCADE share it and have no
   OR IGNORE form to consult). So it RAISES — and before the fix it did not,
   because the DO UPDATE never ran: 0 rows, silently. *)
let do_update_assigning_null_raises_under_every_modifier () =
  List.iter
    (fun modifier ->
       with_db (fun db ->
         seed_alias db;
         expect_error
           db
           ~needle:"NOT NULL constraint failed: t.v"
           (Printf.sprintf
              "INSERT %sINTO t VALUES (1, 9) ON CONFLICT(k) DO UPDATE SET v = ?"
              modifier)
           [ Db.V_null ];
         expect_rows
           db
           ~msg:(Printf.sprintf "%S left the row untouched" modifier)
           [ "1|5" ]
           "SELECT * FROM t"))
    all_modifiers
;;

(* The LITERAL spelling of that assignment is refused earlier, by
   [Sema.bind_upsert_assignments], under every modifier including OR IGNORE.
   #639 notes that this binder check was load-bearing while the runtime path
   below it was unreachable; it is reachable now (the test above proves it), and
   the check still stays — it is the earlier, better-located error, exactly as
   #599 says of the other suspended check. It is NOT suspended under CA_ignore,
   because the runtime answer for a DO UPDATE is "raise", not "skip", so the two
   levels agree rather than disagreeing. *)
let literal_null_in_do_update_is_still_a_bind_error () =
  List.iter
    (fun modifier ->
       with_db (fun db ->
         seed_alias db;
         let sql =
           Printf.sprintf
             "INSERT %sINTO t VALUES (1, 9) ON CONFLICT(k) DO UPDATE SET v = NULL"
             modifier
         in
         (match run (Db.execute db sql) with
          | Ok () -> Alcotest.failf "%S accepted a literal NULL" sql
          | Error e ->
            let msg = Format.asprintf "%a" Db.pp_error e in
            Alcotest.(check bool)
              (Printf.sprintf "%S refused by the binder (%S)" sql msg)
              true
              (contains ~needle:"NOT NULL violation: v" msg));
         expect_rows db ~msg:"row untouched" [ "1|5" ] "SELECT * FROM t"))
    all_modifiers
;;

(* The other direction: the NULL is in the row being INSERTED, so it is the
   INSERT half that violates NOT NULL. An ON CONFLICT clause only ever
   intercepts a UNIQUENESS conflict, so the modifier decides — [OR IGNORE]
   skips, and the DO UPDATE does not run.

   The point of testing BOTH tables is that this used to depend on which index
   the row collided with. The NOT NULL skip lived in [execute_insert_write],
   which an alias-PK conflict reaches and a secondary-index conflict does not
   (the latter resolves to [upsert_rowid] in [check_insert_unique] and goes
   straight to the upsert). Same statement shape, two answers. *)
let not_null_upsert_skip_is_index_independent () =
  with_db (fun db ->
    seed_alias db;
    (match
       run_stmt
         db
         "INSERT OR IGNORE INTO t VALUES (1, ?) ON CONFLICT(k) DO UPDATE SET v = 42"
         [ Db.V_null ]
     with
     | Ok n -> Alcotest.(check int) "alias PK: skipped, 0 rows" 0 n
     | Error e -> Alcotest.failf "alias PK raised instead of skipping: %a" Db.pp_error e);
    expect_rows db ~msg:"alias PK: the DO UPDATE did not run" [ "1|5" ] "SELECT * FROM t");
  with_db (fun db ->
    seed_secondary db;
    (match
       run_stmt
         db
         "INSERT OR IGNORE INTO s VALUES (2, 100, ?) ON CONFLICT(k) DO UPDATE SET v = 42"
         [ Db.V_null ]
     with
     | Ok n -> Alcotest.(check int) "secondary: skipped, 0 rows" 0 n
     | Error e -> Alcotest.failf "secondary raised instead of skipping: %a" Db.pp_error e);
    expect_rows
      db
      ~msg:"secondary index: the DO UPDATE did not run either"
      [ "1|100|5" ]
      "SELECT * FROM s")
;;

(* And every other modifier still raises on that same insert-half NULL, which is
   #599's decided answer — including OR REPLACE, the deliberate divergence from
   SQLite's DEFAULT substitution. The ON CONFLICT clause does not soften it. *)
let insert_half_null_still_raises_without_or_ignore () =
  List.iter
    (fun modifier ->
       with_db (fun db ->
         seed_alias db;
         expect_error
           db
           ~needle:"NOT NULL constraint failed: t.v"
           (Printf.sprintf
              "INSERT %sINTO t VALUES (2, ?) ON CONFLICT(k) DO UPDATE SET v = 42"
              modifier)
           [ Db.V_null ];
         expect_rows
           db
           ~msg:(Printf.sprintf "%S wrote nothing" modifier)
           [ "1|5" ]
           "SELECT * FROM t"))
    [ ""; "OR ABORT "; "OR FAIL "; "OR ROLLBACK "; "OR REPLACE " ]
;;

(* The same thing where the row DOES conflict, which is the case the test above
   misses — it inserts k = 2, which collides with nothing, so it never reaches
   the conflict machinery at all.

   Caught in review of PR #652: the alias-PK pre-probe routes a conflicting row
   straight to the upsert branch, bypassing [execute_insert_write] and therefore
   its NOT NULL check, so this silently became a successful DO UPDATE for every
   raising modifier. It raised on `main`. The check moved into [execute_insert]
   under `Option.is_some upsert_update` so it raises again.

   Both conflict shapes, because "the answer must not depend on which index the
   row hit" is the whole point — and note the secondary shape NEVER raised here
   on `main`, so it is the one changing to agree with the alias shape rather
   than the other way round. *)
let a_conflicting_insert_half_null_still_raises () =
  List.iter
    (fun modifier ->
       (* alias PK: row 1 exists, so this conflicts on the named target *)
       with_db (fun db ->
         seed_alias db;
         expect_error
           db
           ~needle:"NOT NULL constraint failed: t.v"
           (Printf.sprintf
              "INSERT %sINTO t VALUES (1, ?) ON CONFLICT(k) DO UPDATE SET v = 42"
              modifier)
           [ Db.V_null ];
         expect_rows
           db
           ~msg:(Printf.sprintf "%S: alias-PK conflict, DO UPDATE did not run" modifier)
           [ "1|5" ]
           "SELECT * FROM t");
       (* secondary index: k = 100 exists, so this conflicts on the named target *)
       with_db (fun db ->
         seed_secondary db;
         expect_error
           db
           ~needle:"NOT NULL constraint failed: s.v"
           (Printf.sprintf
              "INSERT %sINTO s VALUES (2, 100, ?) ON CONFLICT(k) DO UPDATE SET v = 42"
              modifier)
           [ Db.V_null ];
         expect_rows
           db
           ~msg:(Printf.sprintf "%S: secondary conflict, DO UPDATE did not run" modifier)
           [ "1|100|5" ]
           "SELECT * FROM s"))
    [ ""; "OR ABORT "; "OR FAIL "; "OR ROLLBACK "; "OR REPLACE " ]
;;

(* A plain INSERT — no ON CONFLICT clause — keeps `main`'s behaviour exactly,
   including the precedence between a UNIQUE error and a NOT NULL one. The
   early check above is entered only when there is an upsert clause or
   `OR IGNORE`, precisely so this case does not move. A row that violates NOT
   NULL *and* collides with a secondary UNIQUE index still reports UNIQUE
   first, because [check_insert_unique] runs before [execute_insert_write]'s
   NOT NULL check and nothing was inserted ahead of it. *)
let a_plain_insert_keeps_its_error_precedence () =
  with_db (fun db ->
    seed_secondary db;
    expect_error
      db
      ~needle:"UNIQUE constraint failed"
      "INSERT INTO s VALUES (2, 100, ?)"
      [ Db.V_null ];
    expect_rows db ~msg:"nothing written" [ "1|100|5" ] "SELECT * FROM s")
;;

(* ------------------------------------------------------------------ *)
(* #639 — the other statement shapes                                   *)
(* ------------------------------------------------------------------ *)

(* Multi-row VALUES: [execute_insert_values] loops [execute_insert] per row with
   the same upsert clause, so conflicting and non-conflicting rows in one
   statement take different halves of it. Before the fix all three rows fell to
   the skip. *)
let multi_row_values_upserts_per_row () =
  with_db (fun db ->
    seed_alias db;
    (match
       run_stmt
         db
         "INSERT OR IGNORE INTO t VALUES (1, 10), (2, 20), (3, 30) ON CONFLICT(k) DO \
          UPDATE SET v = 99"
         []
     with
     | Ok n -> Alcotest.(check int) "one updated, two inserted" 3 n
     | Error e -> Alcotest.failf "multi-row upsert raised: %a" Db.pp_error e);
    expect_rows
      db
      ~msg:"the conflicting row updated, the others inserted"
      [ "1|99"; "2|20"; "3|30" ]
      "SELECT * FROM t ORDER BY k")
;;

(* The same statement with one NULL: the NULL row skips (insert-half NOT NULL,
   modifier decides), the conflicting row still updates, the clean row still
   inserts. Three different fates from one statement — the case #599 records as
   the practical cost of a static check that fires per STATEMENT. *)
let multi_row_values_mixes_skip_update_and_insert () =
  with_db (fun db ->
    seed_alias db;
    (match
       run_stmt
         db
         "INSERT OR IGNORE INTO t VALUES (1, ?), (2, ?), (3, ?) ON CONFLICT(k) DO UPDATE \
          SET v = 99"
         [ Db.V_int 10L; Db.V_null; Db.V_int 30L ]
     with
     | Ok n -> Alcotest.(check int) "one updated, one skipped, one inserted" 2 n
     | Error e -> Alcotest.failf "mixed multi-row upsert raised: %a" Db.pp_error e);
    expect_rows
      db
      ~msg:"only the NULL row is missing"
      [ "1|99"; "3|30" ]
      "SELECT * FROM t ORDER BY k")
;;

(* The INSERT ... SELECT form. Two separate facts:

   1. It has no upsert spelling at all — [S_insert_select] carries no
      [upsert_update] and the grammar offers no [opt_upsert] on those
      alternatives — so the combination is REFUSED at parse time rather than
      silently dropped. That is the only acceptable answer while the feature is
      missing; a silently-ignored DO UPDATE would be #639 again in a new place.
   2. The projection form's [OR IGNORE] skip is unchanged. [Sema] does not
      inspect a projection, so this route reaches the runtime check with no
      static opinion — the third of #599's three spellings, and the one that was
      always silent. *)
let insert_select_refuses_the_upsert_and_keeps_the_skip () =
  with_db (fun db ->
    seed_alias db;
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO src VALUES (1, 10)";
    exec db "INSERT INTO src VALUES (2, NULL)";
    exec db "INSERT INTO src VALUES (3, 30)";
    (match
       run
         (Db.execute
            db
            "INSERT OR IGNORE INTO t SELECT k, v FROM src ON CONFLICT(k) DO UPDATE SET v \
             = 99")
     with
     | Ok () ->
       Alcotest.fail "INSERT ... SELECT ... ON CONFLICT DO UPDATE was silently accepted"
     | Error _ -> ());
    (* Without the clause: the projected NULL skips, the conflicting row skips
       (no upsert to run), the clean row lands. *)
    (match run_stmt db "INSERT OR IGNORE INTO t SELECT k, v FROM src" [] with
     | Ok n -> Alcotest.(check int) "one of three source rows written" 1 n
     | Error e -> Alcotest.failf "INSERT ... SELECT OR IGNORE raised: %a" Db.pp_error e);
    expect_rows
      db
      ~msg:"the projected NULL and the conflict both skipped"
      [ "1|5"; "3|30" ]
      "SELECT * FROM t ORDER BY k")
;;

(* ------------------------------------------------------------------ *)
(* #631 — a skipped row's trigger side effects                         *)
(* ------------------------------------------------------------------ *)

(* [audit] receives one row per BEFORE INSERT fire, whether or not the INSERT
   that fired it survives. Deliberately a plain rowid table so the nested
   INSERT allocates from the shared counter — a savepoint rollback has to
   restore that too (#303's [snapshot_rowids]), not just the tree. *)
let seed_triggered db =
  exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER NOT NULL)";
  exec db "CREATE TABLE audit (k INTEGER)";
  exec db "INSERT INTO t VALUES (1, 5)";
  exec
    db
    "CREATE TRIGGER t_bi BEFORE INSERT ON t BEGIN INSERT INTO audit VALUES (NEW.k); END"
;;

let audit_rows db = texts db "SELECT k FROM audit"

(* The autocommit answer, which is the one #631 calls intended: a skipped row
   leaves nothing behind, for either skip reason. Pinned as the reference the
   explicit-transaction case is now required to match. *)
let autocommit_skip_leaves_no_trigger_trace () =
  with_db (fun db ->
    seed_triggered db;
    (* UNIQUE skip *)
    exec db "INSERT OR IGNORE INTO t VALUES (1, 9)";
    Alcotest.(check (list string)) "UNIQUE skip left no audit row" [] (audit_rows db);
    (* NOT NULL skip *)
    (match run_stmt db "INSERT OR IGNORE INTO t VALUES (2, ?)" [ Db.V_null ] with
     | Ok n -> Alcotest.(check int) "NOT NULL skip reports 0 rows" 0 n
     | Error e -> Alcotest.failf "NOT NULL skip raised: %a" Db.pp_error e);
    Alcotest.(check (list string)) "NOT NULL skip left no audit row" [] (audit_rows db);
    expect_rows db ~msg:"t untouched" [ "1|5" ] "SELECT * FROM t")
;;

(* The defect. Inside an explicit BEGIN the same two statements used to leave an
   audit row each, because the undo was [if owned then S.rollback] and the
   caller owned the transaction. Checked both before and after COMMIT: before,
   because that is what the caller's own subsequent statements would see; after,
   because that is what is durable. *)
let explicit_transaction_skip_leaves_no_trigger_trace () =
  with_db (fun db ->
    seed_triggered db;
    exec db "BEGIN";
    exec db "INSERT OR IGNORE INTO t VALUES (1, 9)";
    Alcotest.(check (list string))
      "UNIQUE skip left no audit row inside the transaction"
      []
      (audit_rows db);
    (match run_stmt db "INSERT OR IGNORE INTO t VALUES (2, ?)" [ Db.V_null ] with
     | Ok n -> Alcotest.(check int) "NOT NULL skip reports 0 rows" 0 n
     | Error e -> Alcotest.failf "NOT NULL skip raised: %a" Db.pp_error e);
    Alcotest.(check (list string))
      "NOT NULL skip left no audit row inside the transaction"
      []
      (audit_rows db);
    exec db "COMMIT";
    Alcotest.(check (list string)) "and none after COMMIT" [] (audit_rows db);
    expect_rows db ~msg:"t untouched" [ "1|5" ] "SELECT * FROM t")
;;

(* The savepoint must be RELEASED, not rolled back, when the row survives —
   otherwise the fix would eat every trigger's effects instead of only a skipped
   row's. Run inside a transaction so the savepoint is actually taken. *)
let explicit_transaction_keeps_a_surviving_rows_trigger_trace () =
  with_db (fun db ->
    seed_triggered db;
    exec db "BEGIN";
    exec db "INSERT OR IGNORE INTO t VALUES (2, 20)";
    Alcotest.(check (list string))
      "the surviving row's trigger fired"
      [ "2" ]
      (audit_rows db);
    (* A skip immediately after must not undo the earlier row's trace either —
       the savepoint is taken per row, so it cannot reach back past the
       preceding statement. *)
    exec db "INSERT OR IGNORE INTO t VALUES (1, 9)";
    Alcotest.(check (list string))
      "the earlier trace survives the later skip"
      [ "2" ]
      (audit_rows db);
    exec db "COMMIT";
    Alcotest.(check (list string)) "and is durable" [ "2" ] (audit_rows db);
    expect_rows
      db
      ~msg:"only the new row landed"
      [ "1|5"; "2|20" ]
      "SELECT * FROM t ORDER BY k")
;;

(* Within ONE multi-row statement: the surviving rows' traces stay and the
   skipped row's goes. This is the case a whole-statement undo would get wrong
   in the other direction. *)
let multi_row_skip_undoes_only_the_skipped_rows_trigger () =
  with_db (fun db ->
    seed_triggered db;
    exec db "BEGIN";
    (match
       run_stmt
         db
         "INSERT OR IGNORE INTO t VALUES (2, ?), (1, ?), (3, ?)"
         [ Db.V_int 20L; Db.V_int 9L; Db.V_int 30L ]
     with
     | Ok n -> Alcotest.(check int) "two of three rows written" 2 n
     | Error e -> Alcotest.failf "multi-row OR IGNORE raised: %a" Db.pp_error e);
    Alcotest.(check (list string))
      "only the skipped row's trigger trace is gone"
      [ "2"; "3" ]
      (audit_rows db);
    exec db "COMMIT";
    Alcotest.(check (list string)) "durable" [ "2"; "3" ] (audit_rows db))
;;

(* The savepoint is opened and resolved inside one statement: it never aborts
   the caller's transaction and never becomes a second exit from it. After a
   skip the transaction is still live and still committable, and a ROLLBACK
   still discards everything the transaction did — both halves matter, because
   #555/#584 guard exactly that containment. *)
let the_undo_does_not_disturb_the_enclosing_transaction () =
  with_db (fun db ->
    seed_triggered db;
    exec db "BEGIN";
    exec db "INSERT OR IGNORE INTO t VALUES (1, 9)";
    (* still usable *)
    exec db "INSERT INTO t VALUES (4, 40)";
    exec db "COMMIT";
    expect_rows
      db
      ~msg:"the transaction committed normally"
      [ "1|5"; "4|40" ]
      "SELECT * FROM t ORDER BY k";
    Alcotest.(check (list string))
      "one trace, from the surviving row"
      [ "4" ]
      (audit_rows db));
  with_db (fun db ->
    seed_triggered db;
    exec db "BEGIN";
    exec db "INSERT OR IGNORE INTO t VALUES (1, 9)";
    exec db "INSERT INTO t VALUES (4, 40)";
    exec db "ROLLBACK";
    expect_rows db ~msg:"ROLLBACK still discards everything" [ "1|5" ] "SELECT * FROM t";
    Alcotest.(check (list string))
      "including the surviving row's trace"
      []
      (audit_rows db))
;;

(* A user SAVEPOINT around the statement still works: the internal savepoint
   name is unique per row and is released before the statement returns, so it
   never sits between a user's SAVEPOINT and their ROLLBACK TO. *)
let a_user_savepoint_still_nests_correctly () =
  with_db (fun db ->
    seed_triggered db;
    exec db "BEGIN";
    exec db "SAVEPOINT sp1";
    exec db "INSERT OR IGNORE INTO t VALUES (1, 9)";
    exec db "INSERT INTO t VALUES (5, 50)";
    exec db "ROLLBACK TO sp1";
    exec db "INSERT INTO t VALUES (6, 60)";
    exec db "COMMIT";
    expect_rows
      db
      ~msg:"only the post-rollback row landed"
      [ "1|5"; "6|60" ]
      "SELECT * FROM t ORDER BY k";
    Alcotest.(check (list string))
      "one trace, from the post-rollback row"
      [ "6" ]
      (audit_rows db))
;;

(* ------------------------------------------------------------------ *)
(* Property                                                            *)
(* ------------------------------------------------------------------ *)

(* Over a random mix of fresh keys, colliding keys and NULL values, an
   [INSERT OR IGNORE ... ON CONFLICT(k) DO UPDATE SET v = 99] never raises, and
   for every key the stored value is decidable from the inputs alone: a NULL
   value skips the row entirely; a colliding key gets 99; a fresh key gets its
   own value. The "never raises" clause is what the old behaviour also
   satisfied — it is the value assertion that separates a skip from an update. *)
let prop_upsert_under_or_ignore_is_decidable =
  QCheck2.Test.make
    ~name:"OR IGNORE + DO UPDATE: skip on NULL, update on collision, insert otherwise"
    ~count:100
    QCheck2.Gen.(
      list_size (int_range 1 8) (pair (int_range 1 4) (option (int_range 0 50))))
    (fun rows ->
       with_db (fun db ->
         exec db "CREATE TABLE p (k INTEGER PRIMARY KEY, v INTEGER NOT NULL)";
         exec db "INSERT INTO p VALUES (1, 7)";
         (* Model: k=1 is pre-seeded with 7. *)
         let model = Hashtbl.create 8 in
         Hashtbl.replace model 1 7;
         let ok = ref true in
         List.iter
           (fun (k, v) ->
              let sql =
                "INSERT OR IGNORE INTO p VALUES (?, ?) ON CONFLICT(k) DO UPDATE SET v = \
                 99"
              in
              let params =
                [ Db.V_int (Int64.of_int k)
                ; (match v with
                   | None -> Db.V_null
                   | Some n -> Db.V_int (Int64.of_int n))
                ]
              in
              match run_stmt db sql params with
              | Error _ -> ok := false
              | Ok _ ->
                (match v with
                 | None -> () (* NOT NULL on the insert half: skipped *)
                 | Some n ->
                   if Hashtbl.mem model k
                   then Hashtbl.replace model k 99
                   else Hashtbl.replace model k n))
           rows;
         let want =
           Hashtbl.fold (fun k v acc -> (k, v) :: acc) model []
           |> List.sort compare
           |> List.map (fun (k, v) -> Printf.sprintf "%d|%d" k v)
         in
         !ok && texts db "SELECT * FROM p ORDER BY k" = want))
;;

let () =
  Alcotest.run
    "or_ignore_upsert_639"
    [ ( "639-conflict-target-wins"
      , [ Alcotest.test_case
            "OR IGNORE runs the DO UPDATE (both conflict shapes)"
            `Quick
            or_ignore_runs_the_do_update
        ; Alcotest.test_case
            "every modifier runs the DO UPDATE when a target is named"
            `Quick
            every_modifier_runs_the_do_update
        ; Alcotest.test_case
            "without a target the modifiers are unchanged"
            `Quick
            modifiers_without_a_conflict_target_are_unchanged
        ; Alcotest.test_case
            "OR REPLACE defers to the conflict target too"
            `Quick
            or_replace_also_defers_to_the_conflict_target
        ; Alcotest.test_case
            "the modifier still governs indexes the clause does not name"
            `Quick
            the_modifier_still_governs_other_indexes
        ; Alcotest.test_case
            "a conflict on the target AND another index is order-independent"
            `Quick
            a_conflict_on_the_target_and_another_index_is_order_independent
        ; Alcotest.test_case
            "OR REPLACE with a second conflict displaces nothing"
            `Quick
            or_replace_with_a_second_conflict_displaces_nothing
        ; Alcotest.test_case
            "an alias-PK target beats a secondary conflict"
            `Quick
            an_alias_pk_target_beats_a_secondary_conflict
        ; Alcotest.test_case
            "a non-unique index is not a conflict target"
            `Quick
            a_non_unique_index_is_not_a_conflict_target
        ; Alcotest.test_case
            "raising modifiers no longer report the discarded row's conflict"
            `Quick
            raising_modifiers_no_longer_report_the_discarded_rows_conflict
        ] )
    ; ( "639-not-null"
      , [ Alcotest.test_case
            "a DO UPDATE assigning NULL raises under every modifier"
            `Quick
            do_update_assigning_null_raises_under_every_modifier
        ; Alcotest.test_case
            "a literal NULL in the DO UPDATE is still a bind error"
            `Quick
            literal_null_in_do_update_is_still_a_bind_error
        ; Alcotest.test_case
            "the insert-half NULL skip no longer depends on which index"
            `Quick
            not_null_upsert_skip_is_index_independent
        ; Alcotest.test_case
            "every other modifier still raises on the insert-half NULL"
            `Quick
            insert_half_null_still_raises_without_or_ignore
        ; Alcotest.test_case
            "a CONFLICTING insert-half NULL still raises (both shapes)"
            `Quick
            a_conflicting_insert_half_null_still_raises
        ; Alcotest.test_case
            "a plain INSERT keeps its error precedence"
            `Quick
            a_plain_insert_keeps_its_error_precedence
        ] )
    ; ( "639-statement-shapes"
      , [ Alcotest.test_case
            "multi-row VALUES upserts per row"
            `Quick
            multi_row_values_upserts_per_row
        ; Alcotest.test_case
            "multi-row VALUES mixes skip, update and insert"
            `Quick
            multi_row_values_mixes_skip_update_and_insert
        ; Alcotest.test_case
            "INSERT ... SELECT refuses the upsert and keeps the skip"
            `Quick
            insert_select_refuses_the_upsert_and_keeps_the_skip
        ] )
    ; ( "631-skipped-row-triggers"
      , [ Alcotest.test_case
            "autocommit: a skip leaves no trigger trace"
            `Quick
            autocommit_skip_leaves_no_trigger_trace
        ; Alcotest.test_case
            "explicit transaction: a skip leaves no trigger trace either"
            `Quick
            explicit_transaction_skip_leaves_no_trigger_trace
        ; Alcotest.test_case
            "a surviving row keeps its trigger trace"
            `Quick
            explicit_transaction_keeps_a_surviving_rows_trigger_trace
        ; Alcotest.test_case
            "multi-row: only the skipped row's trigger is undone"
            `Quick
            multi_row_skip_undoes_only_the_skipped_rows_trigger
        ; Alcotest.test_case
            "the undo does not disturb the enclosing transaction"
            `Quick
            the_undo_does_not_disturb_the_enclosing_transaction
        ; Alcotest.test_case
            "a user SAVEPOINT still nests correctly"
            `Quick
            a_user_savepoint_still_nests_correctly
        ] )
    ; ( "property"
      , List.map QCheck_alcotest.to_alcotest [ prop_upsert_under_or_ignore_is_decidable ]
      )
    ]
;;
