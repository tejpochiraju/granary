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
    ]
;;
