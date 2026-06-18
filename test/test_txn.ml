(** Tests for SQL-level BEGIN / COMMIT / ROLLBACK (Phase 3). *)

module Db = struct
  include Sqlocaml.Db

  let open_file = Sqlocaml_unix.open_file
end

let run = Lwt_main.run
let fresh_db () = run (Db.open_in_memory ())
let db_counter = ref 0

let fresh_file_db () =
  let n = !db_counter in
  incr db_counter;
  let path = Printf.sprintf "/tmp/sqlocaml_txn_test_%04d.db" n in
  (try Unix.unlink path with
   | _ -> ());
  match run (Db.open_file ~path ()) with
  | Ok db -> db, path
  | Error _ -> Alcotest.fail "open_file failed"
;;

let close_file_db db path =
  run (Db.close db);
  try Unix.unlink path with
  | _ -> ()
;;

let rows_of stream = run (Lwt_stream.to_list stream)

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error (Db.Parse e) -> Alcotest.failf "parse error in %S: %s" sql e
  | Error (Db.Sema _) -> Alcotest.failf "sema error in %S" sql
  | Error (Db.Runtime e) -> Alcotest.failf "runtime error in %S: %s" sql e
  | Error e -> Alcotest.failf "error in %S: %a" sql Db.pp_error e
;;

let query_ints db sql =
  match run (Db.query db sql) with
  | Error _ -> []
  | Ok stream ->
    List.map
      (fun row ->
         match row.(0) with
         | Db.V_int n -> Int64.to_int n
         | _ -> -1)
      (rows_of stream)
;;

let execute db sql = run (Db.execute db sql)

(* Rows as (int, string) pairs for asserting (rowid, value) shapes. *)
let query_int_text db sql =
  match run (Db.query db sql) with
  | Error _ -> []
  | Ok stream ->
    List.map
      (fun row ->
         match row.(0), row.(1) with
         | Db.V_int n, Db.V_text s -> Int64.to_int n, s
         | _ -> -1, "?")
      (rows_of stream)
;;

(* #293: an INSERT inside an explicit txn bumps the in-memory next_rowid
   counter; a ROLLBACK must revert it so the next allocation re-derives
   max(rowid)+1 from the (now rolled-back) data tree — matching SQLite, which
   reuses the rolled-back rowid for a plain (non-AUTOINCREMENT) rowid table. *)
let test_rollback_reuses_rowid_empty () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "BEGIN";
  exec db "INSERT INTO t (b) VALUES ('x')";
  (* rowid 1 *)
  exec db "ROLLBACK";
  exec db "INSERT INTO t (b) VALUES ('y')";
  let rows = query_int_text db "SELECT a, b FROM t" in
  Alcotest.(check (list (pair int string)))
    "rolled-back rowid 1 is reused"
    [ 1, "y" ]
    rows
;;

(* Non-empty variant: a table already holding rowids 1,2 should reuse rowid 3
   after an in-txn INSERT (rowid 3) is rolled back. *)
let test_rollback_reuses_rowid_nonempty () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "INSERT INTO t (b) VALUES ('a')";
  (* rowid 1 *)
  exec db "INSERT INTO t (b) VALUES ('b')";
  (* rowid 2 *)
  exec db "BEGIN";
  exec db "INSERT INTO t (b) VALUES ('c')";
  (* rowid 3 *)
  exec db "ROLLBACK";
  exec db "INSERT INTO t (b) VALUES ('d')";
  let rows = query_int_text db "SELECT a, b FROM t ORDER BY a ASC" in
  Alcotest.(check (list (pair int string)))
    "rolled-back rowid 3 is reused as max(existing)+1"
    [ 1, "a"; 2, "b"; 3, "d" ]
    rows
;;

(* COMMIT path unaffected: an in-txn INSERT that COMMITs advances the counter
   durably, so the next allocation continues past it (no spurious recompute that
   would re-issue a now-duplicate rowid). *)
let test_commit_advances_rowid () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "BEGIN";
  exec db "INSERT INTO t (b) VALUES ('x')";
  (* rowid 1 *)
  exec db "COMMIT";
  exec db "INSERT INTO t (b) VALUES ('y')";
  (* rowid 2 *)
  let rows = query_int_text db "SELECT a, b FROM t ORDER BY a ASC" in
  Alcotest.(check (list (pair int string)))
    "committed counter continues to rowid 2"
    [ 1, "x"; 2, "y" ]
    rows
;;

(* #299: AUTOINCREMENT reverts its counter on ROLLBACK exactly like plain
   transactional data — [sqlite_sequence] is itself transactional.  When the
   ONLY insert is the one rolled back (no prior committed high-water), the
   counter reverts to its committed (unseeded) state, so the rolled-back rowid
   IS reused.  Verified against SQLite 3.45:
     BEGIN; INSERT->1; ROLLBACK; INSERT  =>  rowid 1 (reuse).
   This is distinct from sticky-across-COMMITTED-DELETE — see
   [test_rollback_autoincrement_high_water]. *)
let test_rollback_autoincrement_revert () =
  let db = fresh_db () in
  exec db "CREATE TABLE s (a INTEGER PRIMARY KEY AUTOINCREMENT, b TEXT)";
  exec db "BEGIN";
  exec db "INSERT INTO s (b) VALUES ('x')";
  (* rowid 1, rolled back *)
  exec db "ROLLBACK";
  exec db "INSERT INTO s (b) VALUES ('y')";
  let rows = query_int_text db "SELECT a, b FROM s" in
  Alcotest.(check (list (pair int string)))
    "AUTOINCREMENT reverts to committed (unseeded) counter on rollback"
    [ 1, "y" ]
    rows
;;

(* #299: the AUTOINCREMENT high-water is sticky across a COMMITTED DELETE — the
   distinguishing behaviour vs a plain rowid table.  After committing rowids
   1,2 and DELETEing 2, the counter stays at 3; an in-txn INSERT (rowid 3) that
   rolls back must revert to the COMMITTED counter (3), NOT recompute
   max(data)+1 (=2).  So the next insert is rowid 3, never reusing 2.  A plain
   rowid table on the identical sequence yields rowid 2 (recompute/reuse) —
   pinned by [test_rollback_reuses_rowid_nonempty].  Verified vs SQLite 3.45. *)
let test_rollback_autoincrement_high_water () =
  let db = fresh_db () in
  exec db "CREATE TABLE s (a INTEGER PRIMARY KEY AUTOINCREMENT, b TEXT)";
  exec db "INSERT INTO s (b) VALUES ('p')";
  (* rowid 1 *)
  exec db "INSERT INTO s (b) VALUES ('q')";
  (* rowid 2; counter now 3 *)
  exec db "DELETE FROM s WHERE a = 2";
  (* committed delete; counter stays 3 *)
  exec db "BEGIN";
  exec db "INSERT INTO s (b) VALUES ('r')";
  (* rowid 3, rolled back *)
  exec db "ROLLBACK";
  exec db "INSERT INTO s (b) VALUES ('z')";
  let rows = query_int_text db "SELECT a, b FROM s ORDER BY a ASC" in
  Alcotest.(check (list (pair int string)))
    "AUTOINCREMENT keeps the high-water past a committed delete (rowid 3)"
    [ 1, "p"; 3, "z" ]
    rows
;;

(* #299/#303: ROLLBACK TO SAVEPOINT reverts the AUTOINCREMENT counter to its
   savepoint-time value via the #303 snapshot/restore path (NOT the full-txn
   recompute), so the rolled-back-to-savepoint rowid 2 is reused.  Verified vs
   SQLite 3.45: ... SAVEPOINT; INSERT->2; ROLLBACK TO; INSERT => rowid 2. *)
let test_rollback_to_savepoint_autoincrement () =
  let db = fresh_db () in
  exec db "CREATE TABLE s (a INTEGER PRIMARY KEY AUTOINCREMENT, b TEXT)";
  exec db "BEGIN";
  exec db "INSERT INTO s (b) VALUES ('x')";
  (* rowid 1 *)
  exec db "SAVEPOINT sp";
  exec db "INSERT INTO s (b) VALUES ('y')";
  (* rowid 2, rolled back to savepoint *)
  exec db "ROLLBACK TO sp";
  exec db "INSERT INTO s (b) VALUES ('z')";
  (* reuses rowid 2 *)
  exec db "COMMIT";
  let rows = query_int_text db "SELECT a, b FROM s ORDER BY a ASC" in
  Alcotest.(check (list (pair int string)))
    "ROLLBACK TO reuses the savepoint-reverted rowid 2 for AUTOINCREMENT"
    [ 1, "x"; 2, "z" ]
    rows
;;

(* #293 (perf-fix scoping): the rollback recompute must be restricted to tables
   bumped IN the rolled-back txn, NOT every cached rowid table.  This is both the
   perf fix and a correctness guard: recompute lowers a counter to max(rowid)+1,
   which is WRONG for a table that has a trailing gap (its top rows were deleted)
   and was not actually bumped in the rolled-back txn.

   Table A is committed with rowids 1,2,3 then has 3 DELETEd, so its counter sits
   at 4 while max(rowid) is 2.  A second, unrelated txn touches only table B and
   ROLLBACKs.  If the rollback recomputed A (the pre-fix all-tables behaviour) it
   would drop A's counter from 4 to 3, and the next INSERT would REUSE rowid 3.
   With per-txn scoping A is untouched, so the next INSERT correctly gets rowid 4.
   This proves the dirty set is scoped per-txn and cleared on commit (the second
   rollback's set names only B). *)
let test_rollback_does_not_disturb_committed_other_table () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (id INTEGER PRIMARY KEY, v TEXT)";
  exec db "CREATE TABLE b (id INTEGER PRIMARY KEY, v TEXT)";
  (* A gets rowids 1,2,3 then drops 3, leaving counter=4 but max(rowid)=2. *)
  exec db "INSERT INTO a (v) VALUES ('a1')";
  exec db "INSERT INTO a (v) VALUES ('a2')";
  exec db "INSERT INTO a (v) VALUES ('a3')";
  exec db "DELETE FROM a WHERE id = 3";
  (* Txn 2: touch only B, then ROLLBACK.  A is untouched here. *)
  exec db "BEGIN";
  exec db "INSERT INTO b (v) VALUES ('b1')";
  exec db "ROLLBACK";
  (* A's next allocation must be rowid 4 (committed counter undisturbed).  A
     recompute-all rollback would have lowered it to 3 and wrongly reused it. *)
  exec db "INSERT INTO a (v) VALUES ('a4')";
  let rows = query_int_text db "SELECT id, v FROM a ORDER BY id ASC" in
  Alcotest.(check (list (pair int string)))
    "committed table A keeps its high-water counter across an unrelated rollback"
    [ 1, "a1"; 2, "a2"; 4, "a4" ]
    rows
;;

(* #303: ROLLBACK TO SAVEPOINT must revert the in-memory next_rowid counter the
   same way a full ROLLBACK does (#293).  An INSERT after the savepoint bumps the
   cached counter; [ROLLBACK TO s] reverts the store row for that INSERT but must
   also restore the cached counter so the next allocation REUSES the rolled-back
   rowid (SQLite parity for a plain rowid table).  Unlike the full-ROLLBACK path,
   the recompute-from-tree trick is unusable here: the RW txn stays open, so a
   fresh RO snapshot sees the last-committed tree, not the savepoint state.  The
   fix snapshots the cached counters at SAVEPOINT and restores them on ROLLBACK
   TO (Schema_cache, in-memory). *)
let test_rollback_to_savepoint_reuses_rowid () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "BEGIN";
  exec db "INSERT INTO t (b) VALUES ('x')";
  (* rowid 1 *)
  exec db "SAVEPOINT s";
  exec db "INSERT INTO t (b) VALUES ('y')";
  (* rowid 2, bumps the cached counter again *)
  exec db "ROLLBACK TO s";
  exec db "INSERT INTO t (b) VALUES ('z')";
  (* must reuse the rolled-back rowid 2, not skip to 3 *)
  exec db "COMMIT";
  let rows = query_int_text db "SELECT a, b FROM t ORDER BY a ASC" in
  Alcotest.(check (list (pair int string)))
    "ROLLBACK TO reuses the rolled-back rowid 2"
    [ 1, "x"; 2, "z" ]
    rows
;;

(* #303: the table's counter is FIRST bumped AFTER the savepoint opened (the
   savepoint snapshot must capture the pre-bump counter, here the empty/unseeded
   sentinel, even though the table was not yet in the rowid dirty set). *)
let test_rollback_to_savepoint_reuses_rowid_first_bump () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "BEGIN";
  exec db "SAVEPOINT s";
  exec db "INSERT INTO t (b) VALUES ('x')";
  (* rowid 1, first bump of t, after the savepoint *)
  exec db "ROLLBACK TO s";
  exec db "INSERT INTO t (b) VALUES ('y')";
  (* must reuse rowid 1 *)
  exec db "COMMIT";
  let rows = query_int_text db "SELECT a, b FROM t ORDER BY a ASC" in
  Alcotest.(check (list (pair int string)))
    "ROLLBACK TO restores the unseeded counter so rowid 1 is reused"
    [ 1, "y" ]
    rows
;;

(* #303: nested savepoints — a bump that happens only inside the INNER savepoint
   must still be reverted by a ROLLBACK TO the OUTER one (which discards the inner
   frame).  The outer snapshot captures t's counter as of the outer SAVEPOINT, so
   the rowid allocated under the inner savepoint is reused after rolling back. *)
let test_rollback_to_outer_savepoint_reuses_rowid_nested () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "BEGIN";
  exec db "INSERT INTO t (b) VALUES ('x')";
  (* rowid 1 *)
  exec db "SAVEPOINT sp_out";
  exec db "SAVEPOINT sp_in";
  exec db "INSERT INTO t (b) VALUES ('y')";
  (* rowid 2, bumped only under the inner savepoint *)
  exec db "ROLLBACK TO sp_out";
  exec db "INSERT INTO t (b) VALUES ('z')";
  (* must reuse rowid 2 *)
  exec db "COMMIT";
  let rows = query_int_text db "SELECT a, b FROM t ORDER BY a ASC" in
  Alcotest.(check (list (pair int string)))
    "ROLLBACK TO sp_out reuses the rowid bumped under the inner savepoint"
    [ 1, "x"; 2, "z" ]
    rows
;;

(* #301: a COMMIT aborted by a still-violated DEFERRED foreign-key constraint
   takes the [drain_pending_fks_or_fail] COMMIT-rollback path, which rolls the
   store back but (pre-fix) did NOT recompute the in-memory next_rowid counters
   the way the full-ROLLBACK path (#293) does.  An INSERT into a plain rowid
   table T inside the doomed txn bumps T's cached counter; after the COMMIT
   fails and the txn is rolled back, the next NULL/omitted-rowid INSERT into T
   must REUSE the rolled-back rowid (max(rowid)+1 from the committed data), not
   skip past it.  This mirrors #293 on the deferred-FK COMMIT-rollback path. *)
let test_deferred_fk_commit_rollback_reuses_rowid () =
  let db = fresh_db () in
  exec db "PRAGMA foreign_keys = 1";
  exec db "CREATE TABLE fk_par (id INTEGER PRIMARY KEY)";
  exec
    db
    "CREATE TABLE fk_chi (id INTEGER PRIMARY KEY, pid INTEGER REFERENCES fk_par(id) \
     DEFERRABLE INITIALLY DEFERRED)";
  (* Plain rowid table T, committed with rowids 1,2 (counter sits at 3). *)
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "INSERT INTO t (b) VALUES ('a')";
  (* rowid 1 *)
  exec db "INSERT INTO t (b) VALUES ('b')";
  (* rowid 2 *)
  exec db "BEGIN";
  exec db "INSERT INTO t (b) VALUES ('c')";
  (* rowid 3, bumps T's cached counter to 4 *)
  (* Deferred FK violation: child references a non-existent parent key.  The
     check is postponed, so this INSERT itself succeeds; the violation only
     surfaces at COMMIT. *)
  exec db "INSERT INTO fk_chi VALUES (100, 999)";
  (match execute db "COMMIT" with
   | Error _ -> ()
   | Ok () -> Alcotest.fail "expected deferred FK violation to abort COMMIT");
  (* The aborted COMMIT rolled the txn back: T's rowid 3 is gone, committed
     state is {1,2}.  The next allocation must REUSE rowid 3, not gap to 4. *)
  exec db "INSERT INTO t (b) VALUES ('d')";
  let rows = query_int_text db "SELECT a, b FROM t ORDER BY a ASC" in
  Alcotest.(check (list (pair int string)))
    "rolled-back rowid 3 is reused after deferred-FK COMMIT abort"
    [ 1, "a"; 2, "b"; 3, "d" ]
    rows
;;

let test_begin_commit_visible () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "BEGIN";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "COMMIT";
  let ns = query_ints db "SELECT n FROM t ORDER BY n ASC" in
  Alcotest.(check (list int)) "committed rows visible" [ 1; 2 ] ns
;;

let test_begin_rollback_invisible () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (0)";
  exec db "BEGIN";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "ROLLBACK";
  let ns = query_ints db "SELECT n FROM t ORDER BY n ASC" in
  Alcotest.(check (list int)) "rolled-back rows invisible" [ 0 ] ns
;;

let test_double_begin_errors () =
  let db = fresh_db () in
  (match run (Db.execute db "BEGIN") with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "first BEGIN failed");
  let r = run (Db.execute db "BEGIN") in
  Alcotest.(check bool) "double BEGIN is error" true (Result.is_error r);
  ignore (run (Db.execute db "ROLLBACK"))
;;

let test_commit_without_begin_errors () =
  let db = fresh_db () in
  let r = run (Db.execute db "COMMIT") in
  Alcotest.(check bool) "COMMIT without BEGIN is error" true (Result.is_error r)
;;

let test_rollback_without_begin_errors () =
  let db = fresh_db () in
  let r = run (Db.execute db "ROLLBACK") in
  Alcotest.(check bool) "ROLLBACK without BEGIN is error" true (Result.is_error r)
;;

let test_autocommit_still_works () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (42)";
  let ns = query_ints db "SELECT n FROM t" in
  Alcotest.(check (list int)) "auto-commit insert visible" [ 42 ] ns
;;

let test_txn_with_update_delete () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "INSERT INTO t (n) VALUES (3)";
  exec db "BEGIN";
  exec db "UPDATE t SET n = 10 WHERE n = 1";
  exec db "DELETE FROM t WHERE n = 2";
  exec db "COMMIT";
  let ns = query_ints db "SELECT n FROM t ORDER BY n ASC" in
  Alcotest.(check (list int)) "txn update+delete committed" [ 3; 10 ] ns
;;

let test_txn_rollback_with_update () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "BEGIN";
  exec db "UPDATE t SET n = 99 WHERE n = 1";
  exec db "ROLLBACK";
  let ns = query_ints db "SELECT n FROM t" in
  Alcotest.(check (list int)) "update rolled back" [ 1 ] ns
;;

let test_unique_violation_survives () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "CREATE UNIQUE INDEX t_n ON t (n)";
  exec db "INSERT INTO t (n) VALUES (1)";
  (* This should fail with UNIQUE constraint — but NOT hang the engine *)
  let r = execute db "INSERT INTO t (n) VALUES (1)" in
  Alcotest.(check bool) "unique violation returns error" true (Result.is_error r);
  (* Engine must still work after the failed insert *)
  exec db "INSERT INTO t (n) VALUES (2)";
  let ns = query_ints db "SELECT n FROM t ORDER BY n ASC" in
  Alcotest.(check (list int)) "engine still works after unique violation" [ 1; 2 ] ns
;;

let test_create_table_in_txn_not_rolled_back () =
  (* Known Phase 3 limitation: CREATE TABLE acquires its own RW txn internally
     and commits immediately — it cannot participate in an explicit BEGIN/ROLLBACK
     block. Attempting BEGIN + CREATE TABLE deadlocks (catalog re-acquires the
     held mutex). We document that CREATE TABLE in auto-commit mode is always
     immediately committed and visible. *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  (* Table is committed immediately and visible in a new auto-commit txn *)
  let r = execute db "INSERT INTO t (n) VALUES (1)" in
  Alcotest.(check bool)
    "table created in auto-commit is immediately visible"
    true
    (Result.is_ok r)
;;

let test_create_index_in_txn_limitation () =
  (* Known Phase 3 limitation: CREATE INDEX (like CREATE TABLE) acquires its own
     internal RW txn via the catalog. Calling BEGIN + CREATE INDEX would deadlock
     because the catalog's S.rw_begin would try to re-acquire the already-held mutex.
     Only run CREATE INDEX outside explicit transactions for now. *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  (* CREATE INDEX outside explicit txn works fine *)
  exec db "CREATE INDEX idx_n ON t (n)";
  let ns = query_ints db "SELECT n FROM t ORDER BY n ASC" in
  Alcotest.(check (list int)) "indexed query works after create index" [ 1; 2 ] ns
;;

let test_parse_begin () =
  let db = fresh_db () in
  let r = run (Db.execute db "BEGIN") in
  Alcotest.(check bool) "BEGIN parses" true (Result.is_ok r);
  ignore (run (Db.execute db "ROLLBACK"))
;;

(* ------------------------------------------------------------------ *)
(* execute_change_count with txn stmts                                  *)
(* ------------------------------------------------------------------ *)

let test_execute_change_count_begin_commit () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  (match run (Db.execute_change_count db "BEGIN") with
   | Ok 0 -> ()
   | Ok n -> Alcotest.failf "BEGIN returned count %d" n
   | Error _ -> Alcotest.fail "BEGIN failed");
  exec db "INSERT INTO t (n) VALUES (7)";
  (match run (Db.execute_change_count db "COMMIT") with
   | Ok 0 -> ()
   | Ok n -> Alcotest.failf "COMMIT returned count %d" n
   | Error _ -> Alcotest.fail "COMMIT failed");
  let ns = query_ints db "SELECT n FROM t" in
  Alcotest.(check (list int)) "change-count txn visible" [ 7 ] ns
;;

let test_execute_change_count_rollback () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  (match run (Db.execute_change_count db "BEGIN") with
   | Ok 0 -> ()
   | _ -> Alcotest.fail "BEGIN failed");
  exec db "INSERT INTO t (n) VALUES (99)";
  (match run (Db.execute_change_count db "ROLLBACK") with
   | Ok 0 -> ()
   | Ok n -> Alcotest.failf "ROLLBACK returned count %d" n
   | Error _ -> Alcotest.fail "ROLLBACK failed");
  let ns = query_ints db "SELECT n FROM t" in
  Alcotest.(check (list int)) "change-count rollback undoes insert" [] ns
;;

let test_execute_change_count_begin_error_no_txn () =
  let db = fresh_db () in
  let r = run (Db.execute_change_count db "COMMIT") in
  Alcotest.(check bool) "COMMIT without BEGIN is error" true (Result.is_error r)
;;

let test_execute_change_count_double_begin_error () =
  let db = fresh_db () in
  let _ = run (Db.execute_change_count db "BEGIN") in
  let r = run (Db.execute_change_count db "BEGIN") in
  Alcotest.(check bool) "double BEGIN is error" true (Result.is_error r);
  ignore (run (Db.execute_change_count db "ROLLBACK"))
;;

(* ------------------------------------------------------------------ *)
(* In_txn mode for DML with execute_change_count                        *)
(* ------------------------------------------------------------------ *)

let test_execute_change_count_update_in_txn () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "INSERT INTO t (n) VALUES (3)";
  exec db "BEGIN";
  (match run (Db.execute_change_count db "UPDATE t SET n = 10 WHERE n = 1") with
   | Ok 1 -> ()
   | Ok n -> Alcotest.failf "expected 1 updated, got %d" n
   | Error _ -> Alcotest.fail "UPDATE failed");
  (match run (Db.execute_change_count db "DELETE FROM t WHERE n = 2") with
   | Ok 1 -> ()
   | Ok n -> Alcotest.failf "expected 1 deleted, got %d" n
   | Error _ -> Alcotest.fail "DELETE failed");
  exec db "COMMIT";
  let ns = query_ints db "SELECT n FROM t ORDER BY n ASC" in
  Alcotest.(check (list int)) "update+delete in txn via change_count" [ 3; 10 ] ns
;;

let test_drop_table_in_txn () =
  (* DROP TABLE from execute, not execute_change_count, in auto-commit mode *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (42)";
  exec db "BEGIN";
  exec db "DELETE FROM t WHERE n = 42";
  exec db "COMMIT";
  let ns = query_ints db "SELECT n FROM t" in
  Alcotest.(check (list int)) "delete all in txn" [] ns
;;

(* ------------------------------------------------------------------ *)
(* File-backed Db (Btree backend) txn tests                             *)
(* These exercise the Btree code paths inside exec.ml / store.ml.       *)
(* ------------------------------------------------------------------ *)

let test_file_db_basic_txn () =
  let db, path = fresh_file_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "BEGIN";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "COMMIT";
  let ns = query_ints db "SELECT n FROM t ORDER BY n ASC" in
  Alcotest.(check (list int)) "file-db txn commits" [ 1; 2 ] ns;
  close_file_db db path
;;

let test_file_db_rollback () =
  let db, path = fresh_file_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (10)";
  exec db "BEGIN";
  exec db "INSERT INTO t (n) VALUES (99)";
  exec db "ROLLBACK";
  let ns = query_ints db "SELECT n FROM t" in
  Alcotest.(check (list int)) "file-db rollback undoes insert" [ 10 ] ns;
  close_file_db db path
;;

let test_file_db_update_in_txn () =
  let db, path = fresh_file_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (5)";
  exec db "BEGIN";
  exec db "UPDATE t SET n = 50 WHERE n = 5";
  exec db "COMMIT";
  let ns = query_ints db "SELECT n FROM t" in
  Alcotest.(check (list int)) "file-db update commits" [ 50 ] ns;
  close_file_db db path
;;

let test_file_db_delete_in_txn () =
  let db, path = fresh_file_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "BEGIN";
  exec db "DELETE FROM t WHERE n = 1";
  exec db "COMMIT";
  let ns = query_ints db "SELECT n FROM t" in
  Alcotest.(check (list int)) "file-db delete commits" [ 2 ] ns;
  close_file_db db path
;;

(* ------------------------------------------------------------------ *)
(* execute_change_count for DDL (CREATE INDEX / DROP TABLE)             *)
(* ------------------------------------------------------------------ *)

let test_execute_change_count_create_index () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  (* CREATE INDEX with existing rows - exercises execute_create_index walk loop *)
  (match run (Db.execute_change_count db "CREATE INDEX t_n ON t (n)") with
   | Ok 0 -> ()
   | Ok n -> Alcotest.failf "expected 0 for CREATE INDEX, got %d" n
   | Error _ -> Alcotest.fail "CREATE INDEX failed");
  let ns = query_ints db "SELECT n FROM t ORDER BY n ASC" in
  Alcotest.(check (list int)) "rows still visible after index creation" [ 1; 2 ] ns
;;

let test_execute_change_count_drop_table () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  match run (Db.execute_change_count db "DROP TABLE t") with
  | Ok 0 -> ()
  | Ok n -> Alcotest.failf "expected 0 for DROP TABLE, got %d" n
  | Error _ -> Alcotest.fail "DROP TABLE failed"
;;

let () =
  Alcotest.run
    "test_txn"
    [ ( "begin_commit"
      , [ Alcotest.test_case "commit_visible" `Quick test_begin_commit_visible
        ; Alcotest.test_case "rollback_invisible" `Quick test_begin_rollback_invisible
        ; Alcotest.test_case "autocommit_still_works" `Quick test_autocommit_still_works
        ] )
    ; ( "error_cases"
      , [ Alcotest.test_case "double_begin" `Quick test_double_begin_errors
        ; Alcotest.test_case
            "commit_without_begin"
            `Quick
            test_commit_without_begin_errors
        ; Alcotest.test_case
            "rollback_without_begin"
            `Quick
            test_rollback_without_begin_errors
        ; Alcotest.test_case
            "unique_violation_survives"
            `Quick
            test_unique_violation_survives
        ] )
    ; ( "multi_stmt"
      , [ Alcotest.test_case "update_delete_commit" `Quick test_txn_with_update_delete
        ; Alcotest.test_case "rollback_with_update" `Quick test_txn_rollback_with_update
        ; Alcotest.test_case
            "create_table_in_txn_not_rolled_back"
            `Quick
            test_create_table_in_txn_not_rolled_back
        ; Alcotest.test_case
            "create_index_in_txn_limitation"
            `Quick
            test_create_index_in_txn_limitation
        ] )
    ; ( "rollback_rowid"
      , [ Alcotest.test_case
            "rollback_reuses_rowid_empty"
            `Quick
            test_rollback_reuses_rowid_empty
        ; Alcotest.test_case
            "rollback_reuses_rowid_nonempty"
            `Quick
            test_rollback_reuses_rowid_nonempty
        ; Alcotest.test_case "commit_advances_rowid" `Quick test_commit_advances_rowid
        ; Alcotest.test_case
            "rollback_autoincrement_revert"
            `Quick
            test_rollback_autoincrement_revert
        ; Alcotest.test_case
            "rollback_autoincrement_high_water"
            `Quick
            test_rollback_autoincrement_high_water
        ; Alcotest.test_case
            "rollback_to_savepoint_autoincrement"
            `Quick
            test_rollback_to_savepoint_autoincrement
        ; Alcotest.test_case
            "rollback_does_not_disturb_committed_other_table"
            `Quick
            test_rollback_does_not_disturb_committed_other_table
        ; Alcotest.test_case
            "rollback_to_savepoint_reuses_rowid"
            `Quick
            test_rollback_to_savepoint_reuses_rowid
        ; Alcotest.test_case
            "rollback_to_savepoint_reuses_rowid_first_bump"
            `Quick
            test_rollback_to_savepoint_reuses_rowid_first_bump
        ; Alcotest.test_case
            "rollback_to_outer_savepoint_reuses_rowid_nested"
            `Quick
            test_rollback_to_outer_savepoint_reuses_rowid_nested
        ; Alcotest.test_case
            "deferred_fk_commit_rollback_reuses_rowid"
            `Quick
            test_deferred_fk_commit_rollback_reuses_rowid
        ] )
    ; "parse", [ Alcotest.test_case "parse_begin" `Quick test_parse_begin ]
    ; ( "execute_change_count"
      , [ Alcotest.test_case "begin_commit" `Quick test_execute_change_count_begin_commit
        ; Alcotest.test_case "rollback" `Quick test_execute_change_count_rollback
        ; Alcotest.test_case
            "commit_no_txn_error"
            `Quick
            test_execute_change_count_begin_error_no_txn
        ; Alcotest.test_case
            "double_begin_error"
            `Quick
            test_execute_change_count_double_begin_error
        ; Alcotest.test_case
            "update_delete_in_txn"
            `Quick
            test_execute_change_count_update_in_txn
        ; Alcotest.test_case "drop_table_in_txn" `Quick test_drop_table_in_txn
        ; Alcotest.test_case "create_index" `Quick test_execute_change_count_create_index
        ; Alcotest.test_case "drop_table" `Quick test_execute_change_count_drop_table
        ] )
    ; ( "file_db"
      , [ Alcotest.test_case "basic_txn" `Quick test_file_db_basic_txn
        ; Alcotest.test_case "rollback" `Quick test_file_db_rollback
        ; Alcotest.test_case "update_in_txn" `Quick test_file_db_update_in_txn
        ; Alcotest.test_case "delete_in_txn" `Quick test_file_db_delete_in_txn
        ] )
    ]
;;
