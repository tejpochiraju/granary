(** Tests for SQL-level BEGIN / COMMIT / ROLLBACK (Phase 3). *)

open Lwt.Syntax

module Db = struct
  include Granary.Db

  let open_file = Granary_unix.open_file
end

(* #555: ATTACH needs the Unix file provider installed. *)
let () = Granary_unix.install ()
let run = Lwt_main.run
let fresh_db () = run (Db.open_in_memory ())
let db_counter = ref 0

let fresh_file_db () =
  let n = !db_counter in
  incr db_counter;
  let path = Printf.sprintf "/tmp/granary_txn_test_%04d.db" n in
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

(* #293 (perf-fix scoping) / #409: the rollback recompute must be restricted to
   tables bumped IN the rolled-back txn, NOT every cached rowid table.

   A is AUTOINCREMENT so its high-water counter is *sticky*: deleting rowid 3
   does NOT lower it (SQLite parity — unlike a plain rowid table, which reuses a
   deleted max; see #409 and test_savepoint_stm), so A sits at counter 4 with
   max(rowid) 2.  A second, unrelated txn touches only table B and ROLLBACKs.
   With per-txn dirty-set scoping the rollback recomputes only B, leaving A's
   committed counter undisturbed, so A's next INSERT correctly gets rowid 4.
   (This used a plain table before #409 — which incorrectly retained the deleted
   high-water; AUTOINCREMENT now carries the legitimately-ahead counter.) *)
let test_rollback_does_not_disturb_committed_other_table () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (id INTEGER PRIMARY KEY AUTOINCREMENT, v TEXT)";
  exec db "CREATE TABLE b (id INTEGER PRIMARY KEY, v TEXT)";
  (* A gets rowids 1,2,3 then drops 3; AUTOINCREMENT keeps counter=4, max=2. *)
  exec db "INSERT INTO a (v) VALUES ('a1')";
  exec db "INSERT INTO a (v) VALUES ('a2')";
  exec db "INSERT INTO a (v) VALUES ('a3')";
  exec db "DELETE FROM a WHERE id = 3";
  (* Txn 2: touch only B, then ROLLBACK.  A is untouched here. *)
  exec db "BEGIN";
  exec db "INSERT INTO b (v) VALUES ('b1')";
  exec db "ROLLBACK";
  (* A's next allocation must be rowid 4 (sticky counter undisturbed).  A
     recompute-all rollback would have lowered it to 3 and wrongly reused it. *)
  exec db "INSERT INTO a (v) VALUES ('a4')";
  let rows = query_int_text db "SELECT id, v FROM a ORDER BY id ASC" in
  Alcotest.(check (list (pair int string)))
    "AUTOINCREMENT table A keeps its sticky counter across an unrelated rollback"
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

(* ------------------------------------------------------------------ *)
(* #555: overlapping-BEGIN containment                                  *)
(* ------------------------------------------------------------------ *)

(* A [Db.t] has one explicit-transaction slot, so a second BEGIN cannot be
   honoured.  Before #555 it was merely refused, and the refused caller's NEXT
   statement silently joined the transaction that won — up to and including a
   COMMIT of somebody else's half-finished work.  These pin the containment:
   after the refused BEGIN the handle is poisoned, every statement is rejected,
   and only ROLLBACK clears it.

   Everything below is scoped to the POISON WINDOW — the span between the failed
   BEGIN and the first ROLLBACK.  The containment does not extend past that; see
   the [residual_584] group for what is still reachable once the poison clears,
   and [worker_handle_serializes_without_poison] for what an application should
   be doing instead of sharing a handle. *)

let test_double_begin_poisons_connection () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "BEGIN";
  Alcotest.(check bool) "not poisoned before" false (Db.transaction_poisoned db);
  let r = run (Db.execute db "BEGIN") in
  Alcotest.(check bool) "second BEGIN is error" true (Result.is_error r);
  Alcotest.(check bool) "poisoned after" true (Db.transaction_poisoned db);
  (* Every statement kind is now rejected — this is the whole point: the losing
     fiber cannot read or write inside the winner's transaction. *)
  Alcotest.(check bool)
    "write rejected"
    true
    (Result.is_error (run (Db.execute db "INSERT INTO t (n) VALUES (1)")));
  Alcotest.(check bool)
    "read rejected"
    true
    (Result.is_error (run (Db.query db "SELECT n FROM t")));
  Alcotest.(check bool)
    "DDL rejected"
    true
    (Result.is_error (run (Db.execute db "CREATE TABLE u (n INTEGER)")));
  Alcotest.(check bool)
    "SAVEPOINT rejected"
    true
    (Result.is_error (run (Db.execute db "SAVEPOINT sp")));
  (* The hazard itself: COMMIT must NOT be able to commit the winner's work. *)
  Alcotest.(check bool)
    "COMMIT rejected"
    true
    (Result.is_error (run (Db.execute db "COMMIT")));
  Alcotest.(check bool) "still poisoned" true (Db.transaction_poisoned db);
  (* ROLLBACK is the one exit, and it succeeds. *)
  (match run (Db.execute db "ROLLBACK") with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "ROLLBACK on a poisoned connection must succeed");
  Alcotest.(check bool) "poison cleared" false (Db.transaction_poisoned db);
  (* And the connection is fully usable again — recovery is well-defined. *)
  exec db "INSERT INTO t (n) VALUES (7)";
  Alcotest.(check (list int))
    "usable after rollback"
    [ 7 ]
    (query_ints db "SELECT n FROM t")
;;

(* Inside the window, the poisoned COMMIT must not commit the winner's
   uncommitted writes: the ROLLBACK that clears the poison discards them.
   (Contrast [test_584_rollback_reopens_the_hazard], where a COMMIT issued AFTER
   the poison has cleared does commit somebody else's work.) *)
let test_poisoned_commit_does_not_commit_other_txn () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "BEGIN";
  exec db "INSERT INTO t (n) VALUES (1)";
  (* "fiber B" arrives *)
  ignore (run (Db.execute db "BEGIN"));
  ignore (run (Db.execute db "COMMIT"));
  ignore (run (Db.execute db "ROLLBACK"));
  Alcotest.(check (list int))
    "in-flight writes discarded, not committed"
    []
    (query_ints db "SELECT n FROM t")
;;

(* Prepared statements resolve their mode from the same one slot, so they are
   gated too. *)
let test_poison_rejects_prepared_statements () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  let ins =
    match run (Db.prepare db "INSERT INTO t (n) VALUES (?)") with
    | Ok st -> st
    | Error _ -> Alcotest.fail "prepare INSERT failed"
  in
  let sel =
    match run (Db.prepare db "SELECT n FROM t") with
    | Ok st -> st
    | Error _ -> Alcotest.fail "prepare SELECT failed"
  in
  exec db "BEGIN";
  ignore (run (Db.execute db "BEGIN"));
  Alcotest.(check bool)
    "prepared run rejected"
    true
    (Result.is_error (run (Db.run ins ~params:[ Db.V_int 1L ])));
  Alcotest.(check bool)
    "prepared iter rejected"
    true
    (Result.is_error (run (Db.iter sel ~params:[])));
  ignore (run (Db.execute db "ROLLBACK"))
;;

(* ROLLBACK on a connection that was never poisoned and has no transaction is
   still an error — the poison exit did not weaken that. *)
let test_rollback_without_begin_still_errors_when_clean () =
  let db = fresh_db () in
  Alcotest.(check bool)
    "ROLLBACK with nothing open is an error"
    true
    (Result.is_error (run (Db.execute db "ROLLBACK")))
;;

(* Autocommit sharing is untouched: no BEGIN, no poison, ever. *)
let test_autocommit_never_poisons () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  for i = 1 to 20 do
    exec db (Printf.sprintf "INSERT INTO t (n) VALUES (%d)" i)
  done;
  Alcotest.(check bool) "never poisoned" false (Db.transaction_poisoned db);
  Alcotest.(check (list int))
    "all autocommit writes visible"
    (List.init 20 (fun i -> i + 1))
    (query_ints db "SELECT n FROM t ORDER BY n ASC")
;;

(* ------------------------------------------------------------------ *)
(* #555 under ATTACH, and the #584 residual                             *)
(* ------------------------------------------------------------------ *)

let is_err r = Result.is_error r
let ok_exn what r = if Result.is_error r then Alcotest.failf "%s: expected Ok" what

(* BEGIN is not a routing statement, so it goes to the ACTIVE schema's
   sub-handle and the poison lands there — not on the handle the application
   holds. Two consequences, both of which were wrong in the first cut of this
   fix:

   - [transaction_poisoned] must look at the attached sub-handles, or it answers
     [false] on a genuinely poisoned connection and an application polling it
     for recovery never learns it must ROLLBACK.
   - [PRAGMA active_database = …] IS a routing statement, so it resolves to the
     top-level handle and the generic gate (which tests the routed handle) waves
     it through. That let a caller walk away from the poisoned schema, after
     which the prescribed recovery breaks: ROLLBACK routes to the new schema and
     answers "no active transaction" while the poisoned one still holds its
     writer lock. Leaving a poisoned schema is now refused. *)
let test_poison_under_attach () =
  let db, path = fresh_file_db () in
  let aux_path = path ^ ".aux" in
  (try Unix.unlink aux_path with
   | _ -> ());
  exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux_path);
  exec db "PRAGMA active_database = 'aux'";
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "BEGIN";
  Alcotest.(check bool) "not poisoned before" false (Db.transaction_poisoned db);
  Alcotest.(check bool)
    "second BEGIN on aux is error"
    true
    (is_err (run (Db.execute db "BEGIN")));
  (* The accessor must see through to the sub-handle. *)
  Alcotest.(check bool)
    "top-level accessor reports poisoned"
    true
    (Db.transaction_poisoned db);
  (* And the schema switch must not be an escape hatch. *)
  Alcotest.(check bool)
    "cannot leave a poisoned schema"
    true
    (is_err (run (Db.execute db "PRAGMA active_database = 'main'")));
  Alcotest.(check bool)
    "statements on the poisoned schema still rejected"
    true
    (is_err (run (Db.query db "SELECT n FROM t")));
  (* ROLLBACK still routes to aux, so recovery works. *)
  (* DETACH routes to the top handle too, and left ungated it would drop the
     sub-handle and its transaction — a clean outcome, but a SECOND exit from
     the poisoned state, contradicting the documented "ROLLBACK is the sole
     exit". Refused, so the contract stays true. *)
  Alcotest.(check bool)
    "cannot DETACH out of a poisoned schema"
    true
    (is_err (run (Db.execute db "DETACH DATABASE aux")));
  ok_exn "ROLLBACK on the poisoned schema" (run (Db.execute db "ROLLBACK"));
  Alcotest.(check bool) "poison cleared" false (Db.transaction_poisoned db);
  ok_exn
    "schema switch after recovery"
    (run (Db.execute db "PRAGMA active_database = 'main'"));
  ok_exn "DETACH after recovery" (run (Db.execute db "DETACH DATABASE aux"));
  close_file_db db path;
  (try Unix.unlink aux_path with
   | _ -> ());
  try Unix.unlink (aux_path ^ "-wal") with
  | _ -> ()
;;

(* #584 — KNOWN-CURRENT BEHAVIOUR, PINNED AS A CANARY, NOT AN ENDORSEMENT.

   The poison narrows the #555 hazard to the window between the failed BEGIN and
   the first ROLLBACK. ROLLBACK is also the prescribed recovery, so the window
   closes by design — and past it the original contamination is reachable with
   the two fibers exchanged. Nothing below raises an error, and the final row
   set is wrong: fiber A's COMMIT commits fiber B's uncommitted work.

   This is pinned so the hole is a canary rather than an undiscovered defect. A
   fix for #584/#585 (a scoped transaction combinator, so the engine can tell
   the owner from the intruder) SHOULD make this test fail — at which point
   update it to the new, correct expectations rather than deleting it. *)
let test_584_rollback_reopens_the_hazard () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  (* A owns the slot. *)
  exec db "BEGIN";
  exec db "INSERT INTO t (n) VALUES (1)";
  (* B collides, is poisoned, and recovers exactly as documented. *)
  Alcotest.(check bool) "B BEGIN refused" true (is_err (run (Db.execute db "BEGIN")));
  ok_exn "B ROLLBACK clears the poison" (run (Db.execute db "ROLLBACK"));
  (* B now takes the slot. A was never told its transaction was aborted. *)
  ok_exn "B BEGIN succeeds" (run (Db.execute db "BEGIN"));
  ok_exn "B INSERT" (run (Db.execute db "INSERT INTO t (n) VALUES (99)"));
  (* A's write lands inside B's transaction, and A's COMMIT commits it. *)
  ok_exn
    "A INSERT (silently inside B's txn)"
    (run (Db.execute db "INSERT INTO t (n) VALUES (2)"));
  ok_exn "A COMMIT (commits B's half-finished work)" (run (Db.execute db "COMMIT"));
  Alcotest.(check (list int))
    "#584: A's row 1 is gone and B's uncommitted 99 is durable"
    [ 2; 99 ]
    (query_ints db "SELECT n FROM t ORDER BY n ASC")
;;

(* #584, adjacent case — same root cause, also pinned.

   If B recovers and does NOT re-BEGIN, A's subsequent writes run in AUTOCOMMIT.
   Each returns Ok and is durably committed on its own; A only discovers the
   transaction is gone at COMMIT, by which time the partial writes are on disk.
   Atomicity is lost silently. *)
let test_584_orphaned_writes_autocommit () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "BEGIN";
  exec db "INSERT INTO t (n) VALUES (1)";
  Alcotest.(check bool) "B BEGIN refused" true (is_err (run (Db.execute db "BEGIN")));
  ok_exn "B ROLLBACK" (run (Db.execute db "ROLLBACK"));
  (* A carries on, unaware. Each write autocommits. *)
  ok_exn "A INSERT 2" (run (Db.execute db "INSERT INTO t (n) VALUES (2)"));
  ok_exn "A INSERT 3" (run (Db.execute db "INSERT INTO t (n) VALUES (3)"));
  Alcotest.(check bool)
    "A only finds out at COMMIT"
    true
    (is_err (run (Db.execute db "COMMIT")));
  Alcotest.(check (list int))
    "#584: row 1 rolled back, rows 2 and 3 committed non-atomically"
    [ 2; 3 ]
    (query_ints db "SELECT n FROM t ORDER BY n ASC")
;;

(* ------------------------------------------------------------------ *)
(* #555 with two genuine Lwt fibers                                     *)
(* ------------------------------------------------------------------ *)

(* The cases above drive one handle sequentially, which is what the hazard
   reduces to. This one interleaves two real fibers on a shared handle, so the
   claim "two fibers sharing a Db.t" is exercised rather than asserted. *)
let test_two_fibers_share_one_handle () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  let a_has_begun, wake_a_begun = Lwt.wait () in
  let b_has_collided, wake_b_collided = Lwt.wait () in
  let b_begin_err = ref false
  and a_write_after_poison_err = ref false
  and poisoned_seen = ref false in
  run
    (Lwt.join
       [ (let* r = Db.execute db "BEGIN" in
          ok_exn "A BEGIN" r;
          let* r = Db.execute db "INSERT INTO t (n) VALUES (1)" in
          ok_exn "A INSERT" r;
          Lwt.wakeup wake_a_begun ();
          let* () = b_has_collided in
          (* A is now collateral damage: its own writes are refused too. *)
          let* r = Db.execute db "INSERT INTO t (n) VALUES (2)" in
          a_write_after_poison_err := is_err r;
          Lwt.return_unit)
       ; (let* () = a_has_begun in
          let* r = Db.execute db "BEGIN" in
          b_begin_err := is_err r;
          poisoned_seen := Db.transaction_poisoned db;
          Lwt.wakeup wake_b_collided ();
          Lwt.return_unit)
       ]);
  Alcotest.(check bool) "B's BEGIN errored" true !b_begin_err;
  Alcotest.(check bool) "B observed the poison" true !poisoned_seen;
  Alcotest.(check bool) "A's own write refused too" true !a_write_after_poison_err;
  ok_exn "ROLLBACK recovers" (run (Db.execute db "ROLLBACK"));
  Alcotest.(check (list int)) "nothing committed" [] (query_ints db "SELECT n FROM t")
;;

(* [create_worker_handle] is the supported way to run explicit transactions from
   more than one fiber, and it does NOT go through the poison at all: each
   handle has its own transaction slot, and because both share one [Store.t]
   they share its single-writer lock, so the second fiber BLOCKS until the first
   commits. Pinned because CLAUDE.md now points applications here, and because
   #555's premise — "a second Db.t over the same path would be a second lock
   with no mutual exclusion" — is false for this constructor. *)
let test_worker_handle_serializes_without_poison () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, n INTEGER)";
  let wdb = run (Db.create_worker_handle db) in
  let a_has_begun, wake_a_begun = Lwt.wait () in
  let a_may_commit, wake_a_commit = Lwt.wait () in
  let b_was_blocked = ref false in
  let rec spin n =
    if n = 0 then Lwt.return_unit else Lwt.bind (Lwt.pause ()) (fun () -> spin (n - 1))
  in
  run
    (Lwt.join
       [ (let* r = Db.execute db "BEGIN" in
          ok_exn "A BEGIN" r;
          let* r = Db.execute db "INSERT INTO t (id, n) VALUES (10, 1)" in
          ok_exn "A INSERT" r;
          Lwt.wakeup wake_a_begun ();
          let* () = a_may_commit in
          let* r = Db.execute db "COMMIT" in
          ok_exn "A COMMIT" r;
          Lwt.return_unit)
       ; (let* () = a_has_begun in
          (* B's BEGIN must not error and must not resolve while A holds the
             writer lock. *)
          let p = Db.execute wdb "BEGIN" in
          let* () = spin 50 in
          (b_was_blocked
           := match Lwt.state p with
              | Lwt.Sleep -> true
              | _ -> false);
          Lwt.wakeup wake_a_commit ();
          let* r = p in
          ok_exn "B BEGIN (once A committed)" r;
          let* r = Db.execute wdb "INSERT INTO t (id, n) VALUES (20, 2)" in
          ok_exn "B INSERT" r;
          let* r = Db.execute wdb "COMMIT" in
          ok_exn "B COMMIT" r;
          Lwt.return_unit)
       ]);
  Alcotest.(check bool) "B blocked on the shared writer lock" true !b_was_blocked;
  Alcotest.(check bool) "parent handle never poisoned" false (Db.transaction_poisoned db);
  Alcotest.(check bool) "worker handle never poisoned" false (Db.transaction_poisoned wdb);
  (* Committed rows ARE visible across handles — they share one store. It is
     only the per-catalog rowid counter that goes stale; see
     [test_worker_handle_stale_rowid_counter]. Hence the explicit ids above. *)
  Alcotest.(check (list int))
    "each handle sees the other's committed rows"
    [ 1; 2 ]
    (query_ints wdb "SELECT n FROM t ORDER BY n ASC");
  Alcotest.(check (list int))
    "both transactions committed, in order"
    [ 1; 2 ]
    (query_ints db "SELECT n FROM t ORDER BY n ASC")
;;

(* KNOWN-CURRENT BEHAVIOUR, PINNED — the sharp edge on [create_worker_handle],
   found while writing the test above (#589).

   [create_worker_handle] is [of_store], which builds a FRESH catalog. A catalog
   caches each rowid table's [next_rowid]. Two handles therefore hold two
   independent counters over one shared data tree, and neither invalidates the
   other. So an INSERT that lets the engine assign the rowid can allocate a
   rowid the other handle has already committed, and the second write SILENTLY
   OVERWRITES the first. No error, no constraint violation, one row where there
   should be two.

   The shared writer lock does not help: this is not a race. The sequence below
   is strictly sequential and still loses the row.

   This is why the fibers test above inserts explicit primary keys, and why
   CLAUDE.md's pointer to [create_worker_handle] carries the caveat in capitals.
   A fix (re-derive or share the counter) SHOULD make this test fail. *)
let query_text_text db sql =
  match run (Db.query db sql) with
  | Error _ -> []
  | Ok stream ->
    List.map
      (fun row ->
         match row.(0), row.(1) with
         | Db.V_text a, Db.V_text b -> a, b
         | _ -> "?", "?")
      (rows_of stream)
;;

(* KNOWN-CURRENT BEHAVIOUR, PINNED — the worst face of #589, and the reason the
   guidance is "WITHOUT ROWID or a caller-supplied INTEGER PRIMARY KEY", NOT the
   plausible-sounding "use explicit primary keys".

   A TEXT PRIMARY KEY is an explicit primary key with explicit values, and it is
   NOT safe: the table still has an engine-assigned rowid underneath, so the
   stale counter still collides. But here the damage is worse than a lost row.
   The PK index keeps a phantom entry for the overwritten key pointing at the
   reused rowid, which now holds the other row's payload, so:

   - a seek on the lost key returns the WRONG ROW, and
   - re-inserting the lost key fails with a phantom UNIQUE violation.

   Silent corruption producing wrong answers, not merely row loss. A fix for
   #589 SHOULD make this test fail. *)
let test_worker_handle_text_pk_corruption () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (k TEXT PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (k, b) VALUES ('k1', 'x')";
  exec wdb "INSERT INTO t (k, b) VALUES ('k2', 'y')";
  Alcotest.(check (list (pair string string)))
    "#589: k1's row was overwritten despite an explicit TEXT PRIMARY KEY"
    [ "k2", "y" ]
    (query_text_text db "SELECT k, b FROM t ORDER BY k ASC");
  (* The index still has an entry for k1, pointing at the reused rowid. *)
  Alcotest.(check (list (pair string string)))
    "#589: seeking the lost key returns the WRONG row"
    [ "k2", "y" ]
    (query_text_text db "SELECT k, b FROM t WHERE k = 'k1'");
  (* And the key can never be re-inserted. *)
  Alcotest.(check bool)
    "#589: phantom UNIQUE violation re-inserting the lost key"
    true
    (is_err (run (Db.execute db "INSERT INTO t (k, b) VALUES ('k1', 'z')")))
;;

let test_worker_handle_stale_rowid_counter () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  (* The worker's catalog snapshots next_rowid = 1 for [t]. *)
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (b) VALUES ('x')";
  (* parent assigns rowid 1 *)
  exec wdb "INSERT INTO t (b) VALUES ('y')";
  (* worker assigns rowid 1 AGAIN *)
  Alcotest.(check (list (pair int string)))
    "#589: the worker's stale counter overwrote the parent's row"
    [ 1, "y" ]
    (query_int_text db "SELECT a, b FROM t ORDER BY a ASC")
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
    ; ( "poison_555"
      , [ Alcotest.test_case
            "double_begin_poisons_connection"
            `Quick
            test_double_begin_poisons_connection
        ; Alcotest.test_case
            "poisoned_commit_does_not_commit_other_txn"
            `Quick
            test_poisoned_commit_does_not_commit_other_txn
        ; Alcotest.test_case
            "poison_rejects_prepared_statements"
            `Quick
            test_poison_rejects_prepared_statements
        ; Alcotest.test_case
            "rollback_without_begin_still_errors_when_clean"
            `Quick
            test_rollback_without_begin_still_errors_when_clean
        ; Alcotest.test_case
            "autocommit_never_poisons"
            `Quick
            test_autocommit_never_poisons
        ; Alcotest.test_case "poison_under_attach" `Quick test_poison_under_attach
        ; Alcotest.test_case
            "two_fibers_share_one_handle"
            `Quick
            test_two_fibers_share_one_handle
        ; Alcotest.test_case
            "worker_handle_serializes_without_poison"
            `Quick
            test_worker_handle_serializes_without_poison
        ; Alcotest.test_case
            "worker_handle_stale_rowid_counter"
            `Quick
            test_worker_handle_stale_rowid_counter
        ; Alcotest.test_case
            "worker_handle_text_pk_corruption"
            `Quick
            test_worker_handle_text_pk_corruption
        ] )
    ; ( "residual_584"
      , [ Alcotest.test_case
            "rollback_reopens_the_hazard"
            `Quick
            test_584_rollback_reopens_the_hazard
        ; Alcotest.test_case
            "orphaned_writes_autocommit"
            `Quick
            test_584_orphaned_writes_autocommit
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
