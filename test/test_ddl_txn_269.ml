(** #269: DDL inside an explicit transaction must not deadlock.

    Before the fix, a [CREATE TABLE]/[CREATE INDEX]/[CREATE VIRTUAL TABLE]/
    [CREATE VIEW]/[CREATE TRIGGER] issued inside an open [BEGIN … COMMIT] hung
    the connection forever: the catalog opened its OWN writer transaction while
    the explicit transaction already held the store's single-writer lock —
    self-deadlock.

    The fix threads the ambient explicit transaction through the catalog DDL
    path (mirroring how #262 threaded the read path), so DDL participates in the
    surrounding transaction.  These tests pin both halves of the contract:

    - DDL + DML inside [BEGIN … COMMIT] succeeds and is durable; and
    - a [ROLLBACK] discards the schema change (the table/index/view/trigger is
      gone AND the in-memory catalog cache agrees with the store, so the name is
      free to be recreated).

    A deadlock regression shows up as the whole test binary hanging; CI's
    per-job timeout (and a local [timeout 60]) turns that into a failure. *)

open Lwt.Syntax
module Db = Sqlocaml.Db

let run = Lwt_main.run

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "db error: %a" Db.pp_error e
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

(* Run a statement expecting it to FAIL; return the error message. *)
let exec_err db sql =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "expected %S to fail, but it succeeded" sql
  | Error e -> Format.asprintf "%a" Db.pp_error e
;;

let vstr = function
  | Db.V_int i -> Printf.sprintf "i:%Ld" i
  | Db.V_real f -> Printf.sprintf "r:%.17g" f
  | Db.V_text s -> Printf.sprintf "t:%s" s
  | Db.V_null -> "null"
  | Db.V_blob b -> Printf.sprintf "b:%s" (String.escaped (Bytes.to_string b))
;;

let rows db sql =
  run
    (let* s = Db.query db sql in
     let* rows = Lwt_stream.to_list (unwrap s) in
     Lwt.return
       (List.map (fun r -> String.concat "," (Array.to_list (Array.map vstr r))) rows))
;;

(* Does a [SELECT] from [tbl] fail (table absent)?  Used to assert a rolled-back
   CREATE really left no table behind. *)
let table_absent db tbl =
  match run (Db.query db (Printf.sprintf "SELECT * FROM \"%s\"" tbl)) with
  | Error _ -> true
  | Ok s ->
    (try
       let _ = run (Lwt_stream.to_list s) in
       false
     with
     | _ -> true)
;;

(* ------------------------------------------------------------------ *)
(* CREATE TABLE                                                         *)
(* ------------------------------------------------------------------ *)

let test_create_table_in_txn_commits () =
  with_db (fun db ->
    exec db "BEGIN";
    exec db "CREATE TABLE t (a INTEGER, b TEXT)";
    exec db "INSERT INTO t VALUES (1, 'x')";
    exec db "COMMIT";
    Alcotest.(check (list string))
      "row survives commit"
      [ "i:1,t:x" ]
      (rows db "SELECT a, b FROM t"))
;;

let test_create_table_in_txn_rollback_discards () =
  with_db (fun db ->
    exec db "BEGIN";
    exec db "CREATE TABLE r (a INTEGER)";
    exec db "INSERT INTO r VALUES (42)";
    exec db "ROLLBACK";
    (* The table must be gone … *)
    Alcotest.(check bool) "table discarded by rollback" true (table_absent db "r");
    (* … AND the catalog cache must agree with the store: the name is free, so a
       fresh autocommit CREATE of the same name succeeds (a stale cache entry
       would raise "already exists"). *)
    exec db "CREATE TABLE r (a INTEGER, c TEXT)";
    exec db "INSERT INTO r VALUES (7, 'ok')";
    Alcotest.(check (list string))
      "recreated table is the new one"
      [ "i:7,t:ok" ]
      (rows db "SELECT a, c FROM r"))
;;

let test_multiple_tables_one_txn () =
  with_db (fun db ->
    exec db "BEGIN";
    exec db "CREATE TABLE a (x INTEGER)";
    exec db "CREATE TABLE b (y INTEGER)";
    exec db "INSERT INTO a VALUES (1)";
    exec db "INSERT INTO b VALUES (2)";
    exec db "COMMIT";
    Alcotest.(check (list string)) "a" [ "i:1" ] (rows db "SELECT x FROM a");
    Alcotest.(check (list string)) "b" [ "i:2" ] (rows db "SELECT y FROM b"))
;;

(* ------------------------------------------------------------------ *)
(* CREATE INDEX                                                         *)
(* ------------------------------------------------------------------ *)

let test_create_index_in_txn_commits () =
  with_db (fun db ->
    exec db "BEGIN";
    exec db "CREATE TABLE t (a INTEGER, b TEXT)";
    exec db "INSERT INTO t VALUES (1, 'x')";
    exec db "INSERT INTO t VALUES (2, 'y')";
    exec db "CREATE UNIQUE INDEX t_a ON t (a)";
    exec db "COMMIT";
    (* Index is durable and enforces uniqueness. *)
    let _ = exec_err db "INSERT INTO t VALUES (1, 'dup')" in
    Alcotest.(check (list string))
      "lookup via indexed column"
      [ "t:y" ]
      (rows db "SELECT b FROM t WHERE a = 2"))
;;

let test_create_index_in_txn_rollback_discards () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "BEGIN";
    exec db "CREATE INDEX t_a ON t (a)";
    exec db "ROLLBACK";
    (* Index name is free again: recreating it must succeed. *)
    exec db "CREATE INDEX t_a ON t (a)")
;;

(* ------------------------------------------------------------------ *)
(* CREATE VIRTUAL TABLE (FTS5)                                          *)
(* ------------------------------------------------------------------ *)

let test_create_fts_in_txn_commits () =
  with_db (fun db ->
    exec db "BEGIN";
    exec db "CREATE VIRTUAL TABLE docs USING fts5(body)";
    exec db "COMMIT";
    exec db "INSERT INTO docs (body) VALUES ('hello world')";
    Alcotest.(check (list string))
      "fts match after txn create"
      [ "t:hello world" ]
      (rows db "SELECT body FROM docs WHERE docs MATCH 'world'"))
;;

(* ------------------------------------------------------------------ *)
(* CREATE VIEW / CREATE TRIGGER                                         *)
(* ------------------------------------------------------------------ *)

let test_create_view_in_txn_commits () =
  with_db (fun db ->
    exec db "BEGIN";
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "INSERT INTO t VALUES (1)";
    exec db "INSERT INTO t VALUES (2)";
    exec db "CREATE VIEW v AS SELECT a FROM t WHERE a > 1";
    exec db "COMMIT";
    Alcotest.(check (list string)) "view query" [ "i:2" ] (rows db "SELECT a FROM v"))
;;

let test_create_trigger_in_txn_commits () =
  with_db (fun db ->
    exec db "BEGIN";
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE TABLE log (a INTEGER)";
    exec
      db
      "CREATE TRIGGER trg AFTER INSERT ON t BEGIN INSERT INTO log VALUES (NEW.a); END";
    exec db "COMMIT";
    exec db "INSERT INTO t VALUES (5)";
    Alcotest.(check (list string)) "trigger fired" [ "i:5" ] (rows db "SELECT a FROM log"))
;;

(* ------------------------------------------------------------------ *)
(* The downstream goal (#264): a schema-bearing dump wrapped in          *)
(* BEGIN … COMMIT now replays atomically.                                *)
(* ------------------------------------------------------------------ *)

let test_schema_dump_replays_in_txn () =
  (* Build a small schema-bearing database. *)
  let script =
    with_db (fun db ->
      exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
      exec db "INSERT INTO t VALUES (1, 'one')";
      exec db "INSERT INTO t VALUES (2, 'two')";
      exec db "CREATE INDEX t_b ON t (b)";
      exec db "CREATE VIEW v AS SELECT b FROM t WHERE a = 2";
      unwrap (run (Db.dump_to_string db ())))
  in
  (* Replay the WHOLE schema dump wrapped in an explicit transaction.
     [Db.execute] runs one statement per call.  This schema has no triggers and
     no semicolons inside string literals, so a plain split on ';' is a faithful
     statement boundary here. *)
  with_db (fun db ->
    exec db "BEGIN";
    List.iter
      (fun stmt ->
         let s = String.trim stmt in
         if s <> "" then exec db s)
      (String.split_on_char ';' script);
    exec db "COMMIT";
    Alcotest.(check (list string))
      "data restored under txn"
      [ "i:1,t:one"; "i:2,t:two" ]
      (rows db "SELECT a, b FROM t ORDER BY a");
    Alcotest.(check (list string)) "view restored" [ "t:two" ] (rows db "SELECT b FROM v"))
;;

(* ------------------------------------------------------------------ *)
(* ALTER TABLE — #282: must execute inside an explicit transaction      *)
(* (commit durable, rollback discards), no longer reject or deadlock.   *)
(* ------------------------------------------------------------------ *)

let test_alter_add_column_in_txn_commits () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "INSERT INTO t VALUES (1)";
    exec db "BEGIN";
    exec db "ALTER TABLE t ADD COLUMN b TEXT";
    exec db "INSERT INTO t VALUES (2, 'y')";
    exec db "COMMIT";
    Alcotest.(check (list string))
      "added column durable after commit"
      [ "i:1,null"; "i:2,t:y" ]
      (rows db "SELECT a, b FROM t ORDER BY a"))
;;

let test_alter_add_column_in_txn_rollback_discards () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "INSERT INTO t VALUES (1)";
    exec db "BEGIN";
    exec db "ALTER TABLE t ADD COLUMN b TEXT";
    exec db "ROLLBACK";
    (* Column is gone: a 2-value insert must fail, a 1-value insert works, and
       the column name is free to be re-added. *)
    let _ = exec_err db "INSERT INTO t VALUES (9, 'z')" in
    exec db "INSERT INTO t VALUES (2)";
    Alcotest.(check (list string))
      "rolled-back column absent"
      [ "i:1"; "i:2" ]
      (rows db "SELECT a FROM t ORDER BY a");
    (* Cache agrees with the store: re-adding the same column succeeds. *)
    exec db "ALTER TABLE t ADD COLUMN b TEXT")
;;

let test_alter_drop_column_in_txn_commits () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b TEXT, c INTEGER)";
    exec db "INSERT INTO t VALUES (1, 'x', 10)";
    exec db "BEGIN";
    (* A row inserted earlier in the SAME txn must be migrated too (RYW). *)
    exec db "INSERT INTO t VALUES (2, 'y', 20)";
    exec db "ALTER TABLE t DROP COLUMN b";
    exec db "COMMIT";
    Alcotest.(check (list string))
      "dropped column gone, all rows reshaped"
      [ "i:1,i:10"; "i:2,i:20" ]
      (rows db "SELECT a, c FROM t ORDER BY a"))
;;

let test_alter_drop_column_in_txn_rollback_discards () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b TEXT)";
    exec db "INSERT INTO t VALUES (1, 'x')";
    exec db "CREATE INDEX t_b ON t (b)";
    exec db "BEGIN";
    exec db "ALTER TABLE t DROP COLUMN b";
    exec db "ROLLBACK";
    (* Column and its dependent index are both restored: the indexed lookup
       still works and the row keeps its original shape. *)
    Alcotest.(check (list string))
      "rolled-back drop restores column data"
      [ "i:1,t:x" ]
      (rows db "SELECT a, b FROM t");
    Alcotest.(check (list string))
      "dependent index restored (lookup by b)"
      [ "i:1" ]
      (rows db "SELECT a FROM t WHERE b = 'x'");
    (* Index name is still taken — recreating it must fail, proving the cache
       undo re-registered it. *)
    let _ = exec_err db "CREATE INDEX t_b ON t (b)" in
    ())
;;

let test_alter_rename_table_in_txn_commits () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "INSERT INTO t VALUES (1)";
    exec db "BEGIN";
    exec db "ALTER TABLE t RENAME TO u";
    exec db "INSERT INTO u VALUES (2)";
    exec db "COMMIT";
    Alcotest.(check bool) "old name gone" true (table_absent db "t");
    Alcotest.(check (list string))
      "data under new name"
      [ "i:1"; "i:2" ]
      (rows db "SELECT a FROM u ORDER BY a"))
;;

let test_alter_rename_table_in_txn_rollback_discards () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "INSERT INTO t VALUES (1)";
    exec db "BEGIN";
    exec db "ALTER TABLE t RENAME TO u";
    exec db "ROLLBACK";
    (* New name gone, old name intact. *)
    Alcotest.(check bool) "new name gone after rollback" true (table_absent db "u");
    Alcotest.(check (list string))
      "old name and data intact"
      [ "i:1" ]
      (rows db "SELECT a FROM t"))
;;

let test_alter_rename_column_in_txn_commits () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b TEXT)";
    exec db "INSERT INTO t VALUES (1, 'x')";
    exec db "BEGIN";
    exec db "ALTER TABLE t RENAME COLUMN b TO c";
    exec db "COMMIT";
    Alcotest.(check (list string))
      "renamed column queryable under new name"
      [ "i:1,t:x" ]
      (rows db "SELECT a, c FROM t"))
;;

let test_alter_rename_column_in_txn_rollback_discards () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b TEXT)";
    exec db "INSERT INTO t VALUES (1, 'x')";
    exec db "BEGIN";
    exec db "ALTER TABLE t RENAME COLUMN b TO c";
    exec db "ROLLBACK";
    (* Old column name is back; the new name does not resolve. *)
    Alcotest.(check (list string))
      "rolled-back rename restores old column name"
      [ "i:1,t:x" ]
      (rows db "SELECT a, b FROM t");
    let _ = exec_err db "SELECT c FROM t" in
    ())
;;

(* The whole transaction stays usable after an ALTER, and autocommit ALTER is
   of course still fine. *)
let test_alter_then_more_dml_in_txn () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "BEGIN";
    exec db "ALTER TABLE t ADD COLUMN b TEXT";
    exec db "INSERT INTO t VALUES (1, 'p')";
    exec db "ALTER TABLE t ADD COLUMN c INTEGER";
    exec db "INSERT INTO t VALUES (2, 'q', 7)";
    exec db "COMMIT";
    Alcotest.(check (list string))
      "two ALTERs + DML in one txn"
      [ "i:1,t:p,null"; "i:2,t:q,i:7" ]
      (rows db "SELECT a, b, c FROM t ORDER BY a"))
;;

(* An index CREATED in the same txn must have its [idx_table] back-reference
   remapped by a RENAME TABLE later in that txn — [finish_rename] scans
   _sys_indexes THROUGH the txn so the still-uncommitted index entry is caught. *)
let test_alter_rename_table_remaps_in_txn_index_commit () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b TEXT)";
    exec db "INSERT INTO t VALUES (1, 'x')";
    exec db "BEGIN";
    exec db "CREATE UNIQUE INDEX t_b ON t (b)";
    exec db "ALTER TABLE t RENAME TO u";
    exec db "COMMIT";
    (* The in-txn index is live on the new table name: uniqueness enforced … *)
    let _ = exec_err db "INSERT INTO u VALUES (2, 'x')" in
    (* … and it serves lookups by the indexed column. *)
    Alcotest.(check (list string))
      "in-txn index usable under new table name"
      [ "i:1" ]
      (rows db "SELECT a FROM u WHERE b = 'x'");
    (* The index name is taken under the new table — recreating it must fail. *)
    let _ = exec_err db "CREATE INDEX t_b ON u (b)" in
    ())
;;

let test_alter_rename_table_remaps_in_txn_index_rollback () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b TEXT)";
    exec db "BEGIN";
    exec db "CREATE INDEX t_b ON t (b)";
    exec db "ALTER TABLE t RENAME TO u";
    exec db "ROLLBACK";
    (* LIFO undo replay unwinds the rename then the index: u is gone, t is back,
       and the index name is free to be recreated on t. *)
    Alcotest.(check bool) "renamed-away name gone" true (table_absent db "u");
    exec db "INSERT INTO t VALUES (1, 'x')";
    Alcotest.(check (list string))
      "original table intact"
      [ "i:1,t:x" ]
      (rows db "SELECT a, b FROM t");
    exec db "CREATE INDEX t_b ON t (b)")
;;

(* Multiple ALTERs on the SAME table in one txn, then ROLLBACK: the undo log is
   replayed most-recent-first, so each closure's captured [table_meta] is
   overwritten by the next until the original shape is restored exactly. *)
let test_multi_alter_same_table_rollback () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "INSERT INTO t VALUES (1)";
    exec db "BEGIN";
    exec db "ALTER TABLE t ADD COLUMN b TEXT";
    exec db "ALTER TABLE t ADD COLUMN c INTEGER";
    exec db "ALTER TABLE t RENAME COLUMN a TO id";
    exec db "ROLLBACK";
    (* None of the three changes survive. *)
    let _ = exec_err db "SELECT b FROM t" in
    let _ = exec_err db "SELECT c FROM t" in
    let _ = exec_err db "SELECT id FROM t" in
    Alcotest.(check (list string))
      "original single-column shape restored"
      [ "i:1" ]
      (rows db "SELECT a FROM t");
    (* The names are free again — re-adding the column succeeds. *)
    exec db "ALTER TABLE t ADD COLUMN b TEXT")
;;

(* ------------------------------------------------------------------ *)
(* #286: a DDL statement that fails inside an explicit transaction       *)
(* leaves the transaction UNCOMMITTABLE — a later COMMIT is forced to    *)
(* roll back rather than persist partial DDL effects.                    *)
(*                                                                       *)
(* Trigger: [ALTER TABLE t RENAME TO u] where [u] was created earlier in *)
(* the SAME uncommitted transaction.  Semantic analysis validates against*)
(* the committed catalog and does not see the in-txn [u], so it passes;  *)
(* the catalog mutator then raises "table already exists: u" from inside *)
(* [with_ddl_txn]'s borrowed-txn path, which poisons the txn.            *)
(* ------------------------------------------------------------------ *)

let contains ~needle haystack =
  let nl = String.length needle
  and hl = String.length haystack in
  let rec go i =
    i + nl <= hl && (String.equal (String.sub haystack i nl) needle || go (i + 1))
  in
  nl = 0 || go 0
;;

(* COMMIT after a poisoned in-txn DDL is rejected, and the WHOLE transaction
   is rolled back: every change made in the txn (both [CREATE TABLE]s) is gone,
   and the catalog cache agrees with the store (the names are free to recreate). *)
let test_failed_in_txn_ddl_poisons_commit () =
  with_db (fun db ->
    exec db "BEGIN";
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE TABLE u (b TEXT)";
    (* sema passes (committed catalog has no [u]); catalog raises → poison. *)
    let alter_err = exec_err db "ALTER TABLE t RENAME TO u" in
    Alcotest.(check bool)
      "rename failed because target exists"
      true
      (contains ~needle:"already exists" alter_err);
    let commit_err = exec_err db "COMMIT" in
    Alcotest.(check bool)
      "COMMIT rejected: transaction was uncommittable"
      true
      (contains ~needle:"uncommittable" commit_err);
    (* The forced rollback discarded the entire txn: both tables are gone … *)
    Alcotest.(check bool) "t discarded by forced rollback" true (table_absent db "t");
    Alcotest.(check bool) "u discarded by forced rollback" true (table_absent db "u");
    (* … and the connection is clean: a fresh autocommit CREATE of the same
       names succeeds (a stale cache entry would raise "already exists"). *)
    exec db "CREATE TABLE t (a INTEGER, c TEXT)";
    exec db "INSERT INTO t VALUES (1, 'ok')";
    Alcotest.(check (list string))
      "recreated table is usable"
      [ "i:1,t:ok" ]
      (rows db "SELECT a, c FROM t"))
;;

(* An explicit ROLLBACK after a poisoned in-txn DDL also unwinds cleanly (the
   poison flag must not interfere with the normal ROLLBACK path), and the
   connection remains usable for a fresh transaction. *)
let test_failed_in_txn_ddl_then_rollback_clean () =
  with_db (fun db ->
    exec db "BEGIN";
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE TABLE u (b TEXT)";
    let _ = exec_err db "ALTER TABLE t RENAME TO u" in
    exec db "ROLLBACK";
    Alcotest.(check bool) "t gone after rollback" true (table_absent db "t");
    Alcotest.(check bool) "u gone after rollback" true (table_absent db "u");
    (* The poison flag was cleared by ROLLBACK: a fresh txn commits normally. *)
    exec db "BEGIN";
    exec db "CREATE TABLE w (a INTEGER)";
    exec db "INSERT INTO w VALUES (5)";
    exec db "COMMIT";
    Alcotest.(check (list string))
      "next transaction commits normally"
      [ "i:5" ]
      (rows db "SELECT a FROM w"))
;;

(* Once poisoned, the transaction stays uncommittable even if later statements
   succeed — matching SQLite's "transaction is uncommittable" semantics.  A
   successful DML after the failed DDL does not clear the poison. *)
let test_poison_persists_through_later_success () =
  with_db (fun db ->
    exec db "BEGIN";
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE TABLE u (b TEXT)";
    let _ = exec_err db "ALTER TABLE t RENAME TO u" in
    (* A perfectly valid statement still runs … *)
    exec db "INSERT INTO u VALUES ('still works')";
    Alcotest.(check (list string))
      "DML after poison still executes in-txn"
      [ "t:still works" ]
      (rows db "SELECT b FROM u");
    (* … but COMMIT is still rejected: the txn never became committable again. *)
    let commit_err = exec_err db "COMMIT" in
    Alcotest.(check bool)
      "COMMIT still rejected after a later successful statement"
      true
      (contains ~needle:"uncommittable" commit_err);
    Alcotest.(check bool) "u discarded" true (table_absent db "u"))
;;

(* The poison also blocks the savepoint-release auto-commit path: a bare
   [SAVEPOINT] (no [BEGIN]) auto-begins a transaction, and [RELEASE]ing the last
   savepoint would auto-commit it.  A failed in-txn DDL must force that RELEASE to
   roll back instead.  Covers the second of the two commit paths this fix touches. *)
let test_failed_in_txn_ddl_poisons_savepoint_release () =
  with_db (fun db ->
    exec db "SAVEPOINT sp";
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE TABLE u (b TEXT)";
    let _ = exec_err db "ALTER TABLE t RENAME TO u" in
    let release_err = exec_err db "RELEASE sp" in
    Alcotest.(check bool)
      "RELEASE auto-commit rejected: transaction was uncommittable"
      true
      (contains ~needle:"uncommittable" release_err);
    Alcotest.(check bool) "t discarded by forced rollback" true (table_absent db "t");
    Alcotest.(check bool) "u discarded by forced rollback" true (table_absent db "u");
    (* The connection is clean: a fresh autocommit CREATE of the same name works. *)
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "INSERT INTO t VALUES (3)";
    Alcotest.(check (list string))
      "recreated table usable"
      [ "i:3" ]
      (rows db "SELECT a FROM t"))
;;

(* ------------------------------------------------------------------ *)
(* DROP TABLE / DROP INDEX inside an explicit transaction (#279)         *)
(*                                                                       *)
(* DROP removes the catalog cache entry BEFORE the caller commits, but   *)
(* (pre-#279) registered no schema-undo.  A [ROLLBACK] then restored the *)
(* table/index on disk (the store reverts the _sys_* deletes — DROP does *)
(* not reclaim the B+-tree pages) but left the in-memory cache           *)
(* disagreeing: the name read as absent until reopen.  These tests pin   *)
(* the symmetry with CREATE — a rolled-back DROP leaves the catalog cache *)
(* agreeing with the store (the object is back AND usable), and a DROP   *)
(* now participates in the poison/forced-rollback machinery (#286).      *)
(* ------------------------------------------------------------------ *)

let test_drop_table_in_txn_commits () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "INSERT INTO t VALUES (1)";
    exec db "BEGIN";
    exec db "DROP TABLE t";
    exec db "COMMIT";
    (* Table is gone and the name is free to recreate. *)
    Alcotest.(check bool) "table dropped by commit" true (table_absent db "t");
    exec db "CREATE TABLE t (b TEXT)";
    exec db "INSERT INTO t VALUES ('new')";
    Alcotest.(check (list string))
      "recreated table is the new one"
      [ "t:new" ]
      (rows db "SELECT b FROM t"))
;;

let test_drop_table_in_txn_rollback_restores () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b TEXT)";
    exec db "INSERT INTO t VALUES (1, 'x')";
    exec db "INSERT INTO t VALUES (2, 'y')";
    exec db "BEGIN";
    exec db "DROP TABLE t";
    exec db "ROLLBACK";
    (* The table — and its data — must be back, AND the catalog cache must agree
       with the store: SELECT works without a reopen.  Pre-#279 the cache lost
       the entry, so this SELECT failed ("no such table"). *)
    Alcotest.(check (list string))
      "rolled-back DROP restores the table and its rows"
      [ "i:1,t:x"; "i:2,t:y" ]
      (rows db "SELECT a, b FROM t");
    (* The name is occupied again: a fresh CREATE of the same name must fail
       (a cache that lost the entry would wrongly accept it). *)
    let err = exec_err db "CREATE TABLE t (z INTEGER)" in
    Alcotest.(check bool)
      "name still taken after rolled-back DROP"
      true
      (contains ~needle:"already exists" err))
;;

let test_drop_index_in_txn_commits () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE UNIQUE INDEX t_a ON t (a)";
    exec db "INSERT INTO t VALUES (1)";
    exec db "BEGIN";
    exec db "DROP INDEX t_a";
    exec db "COMMIT";
    (* Index is durably gone: uniqueness is no longer enforced … *)
    exec db "INSERT INTO t VALUES (1)";
    Alcotest.(check (list string))
      "duplicate allowed after committed DROP INDEX"
      [ "i:1"; "i:1" ]
      (rows db "SELECT a FROM t");
    (* … and the name is free to recreate. *)
    exec db "CREATE INDEX t_a ON t (a)")
;;

let test_drop_index_in_txn_rollback_restores () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE INDEX t_a ON t (a)";
    exec db "BEGIN";
    exec db "DROP INDEX t_a";
    exec db "ROLLBACK";
    (* The index is back in the cache: re-creating it by name must fail
       ("already exists").  Pre-#279 the cache lost the index, so this CREATE
       wrongly succeeded. *)
    let err = exec_err db "CREATE INDEX t_a ON t (a)" in
    Alcotest.(check bool)
      "index name still taken after rolled-back DROP"
      true
      (contains ~needle:"already exists" err);
    (* And it is genuinely present: DROP INDEX succeeds (no "no such index"). *)
    exec db "DROP INDEX t_a")
;;

let test_drop_table_in_txn_rollback_restores_indexes () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b TEXT)";
    exec db "CREATE INDEX t_a ON t (a)";
    exec db "INSERT INTO t VALUES (1, 'x')";
    exec db "BEGIN";
    (* DROP TABLE drops [t] AND its dependent index [t_a] from the cache. *)
    exec db "DROP TABLE t";
    exec db "ROLLBACK";
    (* Table restored with data … *)
    Alcotest.(check (list string))
      "table restored after rolled-back DROP TABLE"
      [ "i:1,t:x" ]
      (rows db "SELECT a, b FROM t");
    (* … and its dependent index restored too (re-CREATE by name fails). *)
    let err = exec_err db "CREATE INDEX t_a ON t (a)" in
    Alcotest.(check bool)
      "dependent index name still taken after rolled-back DROP TABLE"
      true
      (contains ~needle:"already exists" err))
;;

(* A DROP whose cache mutation is later undone by a FORCED rollback (the txn was
   poisoned by an unrelated failed DDL) must come back.  Exercises the DROP
   schema-undo on the poisoned-COMMIT path, not just explicit ROLLBACK. *)
let test_drop_in_txn_then_poison_restores () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "INSERT INTO t VALUES (1)";
    exec db "CREATE TABLE keep (b TEXT)";
    exec db "BEGIN";
    exec db "DROP TABLE t";
    exec db "CREATE TABLE z (c INTEGER)";
    (* poison: rename onto a name created earlier in this same txn. *)
    let _ = exec_err db "ALTER TABLE keep RENAME TO z" in
    let commit_err = exec_err db "COMMIT" in
    Alcotest.(check bool)
      "COMMIT rejected: transaction uncommittable"
      true
      (contains ~needle:"uncommittable" commit_err);
    (* The forced rollback restored the dropped table (cache + store agree). *)
    Alcotest.(check (list string))
      "dropped table restored by forced rollback"
      [ "i:1" ]
      (rows db "SELECT a FROM t");
    (* [z] (created in the txn) is gone; [keep] was never renamed. *)
    Alcotest.(check bool) "z discarded" true (table_absent db "z");
    Alcotest.(check (list string)) "keep intact" [] (rows db "SELECT b FROM keep"))
;;

let () =
  Alcotest.run
    "ddl_txn_269"
    [ ( "create_table"
      , [ Alcotest.test_case "commit" `Quick test_create_table_in_txn_commits
        ; Alcotest.test_case
            "rollback discards"
            `Quick
            test_create_table_in_txn_rollback_discards
        ; Alcotest.test_case "multiple tables" `Quick test_multiple_tables_one_txn
        ] )
    ; ( "create_index"
      , [ Alcotest.test_case "commit" `Quick test_create_index_in_txn_commits
        ; Alcotest.test_case
            "rollback discards"
            `Quick
            test_create_index_in_txn_rollback_discards
        ] )
    ; "create_fts", [ Alcotest.test_case "commit" `Quick test_create_fts_in_txn_commits ]
    ; ( "create_view_trigger"
      , [ Alcotest.test_case "view commit" `Quick test_create_view_in_txn_commits
        ; Alcotest.test_case "trigger commit" `Quick test_create_trigger_in_txn_commits
        ] )
    ; ( "schema_dump"
      , [ Alcotest.test_case "replays in txn" `Quick test_schema_dump_replays_in_txn ] )
    ; ( "alter_table"
      , [ Alcotest.test_case
            "add column commit"
            `Quick
            test_alter_add_column_in_txn_commits
        ; Alcotest.test_case
            "add column rollback discards"
            `Quick
            test_alter_add_column_in_txn_rollback_discards
        ; Alcotest.test_case
            "drop column commit"
            `Quick
            test_alter_drop_column_in_txn_commits
        ; Alcotest.test_case
            "drop column rollback discards"
            `Quick
            test_alter_drop_column_in_txn_rollback_discards
        ; Alcotest.test_case
            "rename table commit"
            `Quick
            test_alter_rename_table_in_txn_commits
        ; Alcotest.test_case
            "rename table rollback discards"
            `Quick
            test_alter_rename_table_in_txn_rollback_discards
        ; Alcotest.test_case
            "rename column commit"
            `Quick
            test_alter_rename_column_in_txn_commits
        ; Alcotest.test_case
            "rename column rollback discards"
            `Quick
            test_alter_rename_column_in_txn_rollback_discards
        ; Alcotest.test_case "alter then more dml" `Quick test_alter_then_more_dml_in_txn
        ; Alcotest.test_case
            "rename table remaps in-txn index (commit)"
            `Quick
            test_alter_rename_table_remaps_in_txn_index_commit
        ; Alcotest.test_case
            "rename table remaps in-txn index (rollback)"
            `Quick
            test_alter_rename_table_remaps_in_txn_index_rollback
        ; Alcotest.test_case
            "multi-alter same table rollback (LIFO undo)"
            `Quick
            test_multi_alter_same_table_rollback
        ] )
    ; ( "uncommittable_286"
      , [ Alcotest.test_case
            "failed in-txn DDL poisons COMMIT (forced rollback)"
            `Quick
            test_failed_in_txn_ddl_poisons_commit
        ; Alcotest.test_case
            "failed in-txn DDL then explicit ROLLBACK is clean"
            `Quick
            test_failed_in_txn_ddl_then_rollback_clean
        ; Alcotest.test_case
            "poison persists through a later successful statement"
            `Quick
            test_poison_persists_through_later_success
        ; Alcotest.test_case
            "failed in-txn DDL poisons SAVEPOINT release auto-commit"
            `Quick
            test_failed_in_txn_ddl_poisons_savepoint_release
        ] )
    ; ( "drop_table_index"
      , [ Alcotest.test_case "drop table commit" `Quick test_drop_table_in_txn_commits
        ; Alcotest.test_case
            "drop table rollback restores"
            `Quick
            test_drop_table_in_txn_rollback_restores
        ; Alcotest.test_case "drop index commit" `Quick test_drop_index_in_txn_commits
        ; Alcotest.test_case
            "drop index rollback restores"
            `Quick
            test_drop_index_in_txn_rollback_restores
        ; Alcotest.test_case
            "drop table rollback restores dependent indexes"
            `Quick
            test_drop_table_in_txn_rollback_restores_indexes
        ; Alcotest.test_case
            "drop in-txn then poison restores on forced rollback"
            `Quick
            test_drop_in_txn_then_poison_restores
        ] )
    ]
;;
