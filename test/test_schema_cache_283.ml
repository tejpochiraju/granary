(** #283: public-API rollback round-trip guards for Schema_cache.

    The refactor sealed the catalog's three cache hash-tables (tables / indexes /
    fts) and the schema undo-log into a [Schema_cache] module.  It is a PURE
    refactor — zero behaviour change.  These tests are regression guards: they
    assert that after [ROLLBACK] / [ROLLBACK TO SAVEPOINT] the cache and the
    store are in agreement for each DDL mutator kind, verified entirely through
    observable SQL behaviour (no peeking at catalog internals).

    All 6 cases are expected to pass on the already-refactored code; this file
    exists so that a future regression in cache-invalidation or undo-log replay
    is caught at the test gate rather than in production. *)

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
     let* r = Lwt_stream.to_list (unwrap s) in
     Lwt.return
       (List.map (fun row -> String.concat "," (Array.to_list (Array.map vstr row))) r))
;;

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

let contains ~needle haystack =
  let nl = String.length needle
  and hl = String.length haystack in
  let rec go i =
    i + nl <= hl && (String.equal (String.sub haystack i nl) needle || go (i + 1))
  in
  nl = 0 || go 0
;;

(* ------------------------------------------------------------------ *)
(* Case 1: CREATE TABLE rollback — name freed in cache                  *)
(* ------------------------------------------------------------------ *)

(* BEGIN; CREATE TABLE t(a); ROLLBACK; verifies:
   - SELECT from t now errors (no such table), AND
   - a fresh CREATE TABLE t succeeds (name is free again in the cache). *)
let test_create_table_rollback () =
  with_db (fun db ->
    exec db "BEGIN";
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "ROLLBACK";
    (* The table must be absent … *)
    Alcotest.(check bool) "table absent after rollback" true (table_absent db "t");
    (* … and the name must be free in the cache so a fresh CREATE succeeds.  A
       stale cache entry would raise "already exists". *)
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "INSERT INTO t VALUES (42)";
    Alcotest.(check (list string))
      "recreated table holds new data"
      [ "i:42" ]
      (rows db "SELECT a FROM t"))
;;

(* ------------------------------------------------------------------ *)
(* Case 2: DROP TABLE (with dependent index) rollback — both restored    *)
(* ------------------------------------------------------------------ *)

(* Committed setup: CREATE TABLE t(a); CREATE INDEX i ON t(a);
   Then: BEGIN; DROP TABLE t; ROLLBACK;
   Verifies:
   - t is back (INSERT works), AND
   - index i is back (duplicate CREATE INDEX i fails as already-exists). *)
let test_drop_table_with_index_rollback () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE INDEX i ON t (a)";
    exec db "BEGIN";
    exec db "DROP TABLE t";
    exec db "ROLLBACK";
    (* Table is back with its data path intact. *)
    exec db "INSERT INTO t (a) VALUES (1)";
    Alcotest.(check (list string))
      "table restored after rolled-back DROP"
      [ "i:1" ]
      (rows db "SELECT a FROM t");
    (* Index is back in the cache: a duplicate CREATE must fail. *)
    let err = exec_err db "CREATE INDEX i ON t (a)" in
    Alcotest.(check bool)
      "index name still taken after rolled-back DROP TABLE"
      true
      (contains ~needle:"already exists" err))
;;

(* ------------------------------------------------------------------ *)
(* Case 3: CREATE INDEX / DROP INDEX rollback — both directions          *)
(* ------------------------------------------------------------------ *)

(* (a) CREATE INDEX rolled back: re-CREATE succeeds.
   (b) DROP INDEX rolled back: re-CREATE fails (index still taken). *)
let test_create_drop_index_rollback () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    (* Part (a): CREATE INDEX inside txn, then ROLLBACK → index absent. *)
    exec db "BEGIN";
    exec db "CREATE INDEX i ON t (a)";
    exec db "ROLLBACK";
    (* Index name is free: recreating must succeed. *)
    exec db "CREATE INDEX i ON t (a)";
    (* Part (b): now i is committed; DROP INDEX inside txn, then ROLLBACK
       → index present. *)
    exec db "BEGIN";
    exec db "DROP INDEX i";
    exec db "ROLLBACK";
    (* Index name is still taken: re-CREATE must fail. *)
    let err = exec_err db "CREATE INDEX i ON t (a)" in
    Alcotest.(check bool)
      "index name still taken after rolled-back DROP INDEX"
      true
      (contains ~needle:"already exists" err))
;;

(* ------------------------------------------------------------------ *)
(* Case 4: ALTER TABLE rollback — original column set restored           *)
(* ------------------------------------------------------------------ *)

(* For each of ADD COLUMN, DROP COLUMN, and RENAME COLUMN: run the ALTER
   inside BEGIN … ROLLBACK and confirm the ORIGINAL column shape survives.
   Uses the same SQL spellings as test_ddl_txn_269 for ADD/DROP/RENAME. *)
let test_alter_column_rollback () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b TEXT)";
    exec db "INSERT INTO t VALUES (1, 'x')";
    (* --- ADD COLUMN c, then ROLLBACK --- *)
    exec db "BEGIN";
    exec db "ALTER TABLE t ADD COLUMN c INTEGER";
    exec db "ROLLBACK";
    (* c is gone: a 3-value INSERT fails; original (a, b) shape is intact. *)
    let _ = exec_err db "INSERT INTO t VALUES (2, 'y', 99)" in
    Alcotest.(check (list string))
      "add column rolled back — original shape intact"
      [ "i:1,t:x" ]
      (rows db "SELECT a, b FROM t");
    (* Cache agrees: re-adding the same column name succeeds. *)
    exec db "ALTER TABLE t ADD COLUMN c INTEGER";
    exec db "INSERT INTO t VALUES (2, 'y', 99)";
    Alcotest.(check (list string))
      "re-added column durable"
      [ "i:1,t:x,null"; "i:2,t:y,i:99" ]
      (rows db "SELECT a, b, c FROM t ORDER BY a");
    (* Reset to 2-column shape for the next sub-case. *)
    exec db "DROP TABLE t";
    exec db "CREATE TABLE t (a INTEGER, b TEXT)";
    exec db "INSERT INTO t VALUES (1, 'x')";
    (* --- DROP COLUMN b, then ROLLBACK --- *)
    exec db "BEGIN";
    exec db "ALTER TABLE t DROP COLUMN b";
    exec db "ROLLBACK";
    (* b is back: SELECT a, b works; the row's data is intact. *)
    Alcotest.(check (list string))
      "drop column rolled back — b restored"
      [ "i:1,t:x" ]
      (rows db "SELECT a, b FROM t");
    (* Cache agrees: re-adding b would now be a duplicate — verify b is
       still known by checking that a SELECT of b is valid. *)
    exec db "INSERT INTO t VALUES (2, 'y')";
    Alcotest.(check (list string))
      "b still present — insert with b works"
      [ "i:1,t:x"; "i:2,t:y" ]
      (rows db "SELECT a, b FROM t ORDER BY a");
    (* --- RENAME COLUMN a TO z, then ROLLBACK --- *)
    exec db "BEGIN";
    exec db "ALTER TABLE t RENAME COLUMN a TO z";
    exec db "ROLLBACK";
    (* Old name a is back; new name z does not resolve. *)
    Alcotest.(check (list string))
      "rename column rolled back — original name a intact"
      [ "i:1,t:x"; "i:2,t:y" ]
      (rows db "SELECT a, b FROM t ORDER BY a");
    let _ = exec_err db "SELECT z FROM t" in
    ())
;;

(* ------------------------------------------------------------------ *)
(* Case 5: SAVEPOINT partial rollback — a survives, b does not          *)
(* ------------------------------------------------------------------ *)

(* BEGIN; CREATE TABLE a(x); SAVEPOINT s; CREATE TABLE b(y);
   ROLLBACK TO s; COMMIT;
   After commit: a exists, b does not. *)
let test_savepoint_partial_rollback () =
  with_db (fun db ->
    exec db "BEGIN";
    exec db "CREATE TABLE a (x INTEGER)";
    exec db "SAVEPOINT s";
    exec db "CREATE TABLE b (y INTEGER)";
    exec db "ROLLBACK TO s";
    exec db "COMMIT";
    (* a survived the partial rollback and the outer COMMIT. *)
    exec db "INSERT INTO a (x) VALUES (1)";
    Alcotest.(check (list string))
      "a exists after partial savepoint rollback"
      [ "i:1" ]
      (rows db "SELECT x FROM a");
    (* b was rolled back to the savepoint; it must be absent. *)
    Alcotest.(check bool) "b absent after ROLLBACK TO s" true (table_absent db "b"))
;;

(* ------------------------------------------------------------------ *)
(* Case 6: rowid reuse after ROLLBACK (#293 parity)                      *)
(* ------------------------------------------------------------------ *)

(* Committed CREATE TABLE t(a); then BEGIN; INSERT … (grabs rowid 1); ROLLBACK;
   then INSERT again → new row's rowid must be 1 (reused, SQLite parity).
   Read via INTEGER PRIMARY KEY alias. *)
let test_rowid_reuse_after_rollback () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
    exec db "BEGIN";
    exec db "INSERT INTO t (b) VALUES ('x')";
    (* rowid 1, but rolled back *)
    exec db "ROLLBACK";
    exec db "INSERT INTO t (b) VALUES ('y')";
    (* should reuse rowid 1 *)
    Alcotest.(check (list string))
      "rowid 1 reused after rollback"
      [ "i:1,t:y" ]
      (rows db "SELECT a, b FROM t"))
;;

let () =
  Alcotest.run
    "schema_cache_283"
    [ ( "create_table"
      , [ Alcotest.test_case "rollback frees name in cache" `Quick test_create_table_rollback
        ] )
    ; ( "drop_table_with_index"
      , [ Alcotest.test_case
            "rollback restores table and dependent index"
            `Quick
            test_drop_table_with_index_rollback
        ] )
    ; ( "create_drop_index"
      , [ Alcotest.test_case
            "create/drop index rollback both directions"
            `Quick
            test_create_drop_index_rollback
        ] )
    ; ( "alter_column"
      , [ Alcotest.test_case
            "add/drop/rename column rollback restores original shape"
            `Quick
            test_alter_column_rollback
        ] )
    ; ( "savepoint_partial_rollback"
      , [ Alcotest.test_case
            "ROLLBACK TO SAVEPOINT keeps pre-savepoint DDL, drops post"
            `Quick
            test_savepoint_partial_rollback
        ] )
    ; ( "rowid_reuse"
      , [ Alcotest.test_case
            "rowid reused after INSERT + ROLLBACK (SQLite parity)"
            `Quick
            test_rowid_reuse_after_rollback
        ] )
    ]
;;
