(** #243 (T1): INTEGER PRIMARY KEY as a rowid alias.

    These target the behaviours that the cross-engine [sqlite_compare] suite does
    not exercise directly: the UNIQUE-on-PK error, autoincrement seeding past an
    explicit id, [last_insert_rowid()] for a non-monotonic explicit id, the
    ON CONFLICT variants on the PK, and FK references to an alias parent.  The
    broad SELECT/JOIN/aggregate parity is covered by [sqlite_compare]. *)

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

let exec_expect_error db sql =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "expected an error from %S but it succeeded" sql
  | Error _ -> ()
;;

let query_rows db sql =
  unwrap
    (run
       (let open Lwt.Syntax in
        let* s = Db.query db sql in
        match s with
        | Error e -> Lwt.return (Error e)
        | Ok stream ->
          let* rows = Lwt_stream.to_list stream in
          Lwt.return (Ok rows)))
;;

let scalar_int db sql =
  match query_rows db sql with
  | [ [| Db.V_int n |] ] -> n
  | rows -> Alcotest.failf "%S: expected one int row, got %d" sql (List.length rows)
;;

let ints db sql =
  List.map
    (function
      | [| Db.V_int n |] -> n
      | _ -> Alcotest.failf "%S: expected int rows" sql)
    (query_rows db sql)
;;

(* A duplicate explicit INTEGER PRIMARY KEY must error (uniqueness now enforced
   by the table tree, not a __pk index). *)
let test_dup_pk_errors () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)";
    exec db "INSERT INTO t VALUES (1, 'a')";
    exec_expect_error db "INSERT INTO t VALUES (1, 'b')";
    (* The original row is intact, no second row written. *)
    Alcotest.(check (list int64))
      "only the original row remains"
      [ 1L ]
      (ints db "SELECT id FROM t ORDER BY id");
    Alcotest.(check string)
      "value unchanged"
      "a"
      (match query_rows db "SELECT v FROM t WHERE id = 1" with
       | [ [| Db.V_text s |] ] -> s
       | _ -> "??"))
;;

(* NULL / omitted id autoincrements; SELECT id and last_insert_rowid() agree. *)
let test_null_autoincrement () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)";
    exec db "INSERT INTO t (v) VALUES ('a')";
    exec db "INSERT INTO t (v) VALUES ('b')";
    Alcotest.(check (list int64))
      "auto ids 1,2"
      [ 1L; 2L ]
      (ints db "SELECT id FROM t ORDER BY id");
    Alcotest.(check int64)
      "last_insert_rowid is the last auto id"
      2L
      (scalar_int db "SELECT last_insert_rowid()"))
;;

(* An explicit id advances the autoincrement seed so a later NULL gets max+1. *)
let test_explicit_then_null_seed () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY)";
    exec db "INSERT INTO t VALUES (100)";
    exec db "INSERT INTO t (id) VALUES (NULL)";
    Alcotest.(check (list int64))
      "NULL after explicit 100 gets 101"
      [ 100L; 101L ]
      (ints db "SELECT id FROM t ORDER BY id"))
;;

(* last_insert_rowid() must be the ACTUAL inserted id, not next_rowid-1, when an
   explicit id is below the running max.  This is the Step 7 correctness case. *)
let test_last_insert_rowid_non_monotonic () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY)";
    exec db "INSERT INTO t VALUES (100)";
    exec db "INSERT INTO t VALUES (5)";
    Alcotest.(check int64)
      "last_insert_rowid is 5, not 100/99"
      5L
      (scalar_int db "SELECT last_insert_rowid()"))
;;

(* WHERE id = <lit> on the alias returns the exact row (routed through
   Op_rowid_lookup, but we assert the observable result). *)
let test_pk_point_lookup () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)";
    for i = 1 to 20 do
      exec db (Printf.sprintf "INSERT INTO t VALUES (%d, 'v%d')" i i)
    done;
    Alcotest.(check string)
      "lookup id=7"
      "v7"
      (match query_rows db "SELECT v FROM t WHERE id = 7" with
       | [ [| Db.V_text s |] ] -> s
       | _ -> "??");
    Alcotest.(check (list int64))
      "lookup of absent id is empty"
      []
      (ints db "SELECT id FROM t WHERE id = 999"))
;;

let test_replace_on_pk () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)";
    exec db "INSERT INTO t VALUES (1, 'a')";
    exec db "INSERT OR REPLACE INTO t VALUES (1, 'b')";
    Alcotest.(check (list int64)) "still one row" [ 1L ] (ints db "SELECT id FROM t");
    Alcotest.(check string)
      "value replaced"
      "b"
      (match query_rows db "SELECT v FROM t WHERE id = 1" with
       | [ [| Db.V_text s |] ] -> s
       | _ -> "??"))
;;

let test_ignore_on_pk () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)";
    exec db "INSERT INTO t VALUES (1, 'a')";
    exec db "INSERT OR IGNORE INTO t VALUES (1, 'b')";
    Alcotest.(check string)
      "original kept on IGNORE"
      "a"
      (match query_rows db "SELECT v FROM t WHERE id = 1" with
       | [ [| Db.V_text s |] ] -> s
       | _ -> "??"))
;;

let test_upsert_on_pk () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)";
    exec db "INSERT INTO t VALUES (1, 'a')";
    exec db "INSERT INTO t VALUES (1, 'b') ON CONFLICT(id) DO UPDATE SET v = 'updated'";
    Alcotest.(check (list int64)) "still one row" [ 1L ] (ints db "SELECT id FROM t");
    Alcotest.(check string)
      "upsert updated the row"
      "updated"
      (match query_rows db "SELECT v FROM t WHERE id = 1" with
       | [ [| Db.V_text s |] ] -> s
       | _ -> "??"))
;;

(* FK referencing an alias parent: valid child inserts, invalid rejected. *)
let test_fk_to_alias_parent () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE parent (id INTEGER PRIMARY KEY, name TEXT)";
    exec
      db
      "CREATE TABLE child (cid INTEGER PRIMARY KEY, pid INTEGER REFERENCES parent(id))";
    exec db "INSERT INTO parent VALUES (1, 'p1')";
    exec db "INSERT INTO child VALUES (10, 1)";
    exec_expect_error db "INSERT INTO child VALUES (11, 999)";
    Alcotest.(check (list int64))
      "only the valid child"
      [ 10L ]
      (ints db "SELECT cid FROM child ORDER BY cid"))
;;

(* Negative and int64-boundary explicit ids round-trip as keys. *)
let test_negative_and_large_ids () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY)";
    exec db "INSERT INTO t VALUES (-5)";
    exec db "INSERT INTO t VALUES (9223372036854775807)";
    exec db "INSERT INTO t VALUES (0)";
    Alcotest.(check (list int64))
      "negative, zero, max sort correctly"
      [ -5L; 0L; 9223372036854775807L ]
      (ints db "SELECT id FROM t ORDER BY id");
    Alcotest.(check string)
      "lookup negative id"
      "ok"
      (match query_rows db "SELECT 'ok' FROM t WHERE id = -5" with
       | [ [| Db.V_text s |] ] -> s
       | _ -> "missing"))
;;

(* Table-level single-column INTEGER PRIMARY KEY(id) also qualifies as an alias. *)
let test_table_level_pk_alias () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER, v TEXT, PRIMARY KEY(id))";
    exec db "INSERT INTO t VALUES (1, 'a')";
    exec_expect_error db "INSERT INTO t VALUES (1, 'b')";
    Alcotest.(check string)
      "table-level PK lookup"
      "a"
      (match query_rows db "SELECT v FROM t WHERE id = 1" with
       | [ [| Db.V_text s |] ] -> s
       | _ -> "??"))
;;

let () =
  Alcotest.run
    "rowid_alias"
    [ ( "t1"
      , [ Alcotest.test_case "duplicate explicit PK errors" `Quick test_dup_pk_errors
        ; Alcotest.test_case "NULL id autoincrements" `Quick test_null_autoincrement
        ; Alcotest.test_case
            "explicit id seeds autoincrement"
            `Quick
            test_explicit_then_null_seed
        ; Alcotest.test_case
            "last_insert_rowid non-monotonic"
            `Quick
            test_last_insert_rowid_non_monotonic
        ; Alcotest.test_case "PK point lookup" `Quick test_pk_point_lookup
        ; Alcotest.test_case "INSERT OR REPLACE on PK" `Quick test_replace_on_pk
        ; Alcotest.test_case "INSERT OR IGNORE on PK" `Quick test_ignore_on_pk
        ; Alcotest.test_case "UPSERT on PK" `Quick test_upsert_on_pk
        ; Alcotest.test_case "FK to alias parent" `Quick test_fk_to_alias_parent
        ; Alcotest.test_case "negative and large ids" `Quick test_negative_and_large_ids
        ; Alcotest.test_case "table-level PK alias" `Quick test_table_level_pk_alias
        ] )
    ]
;;
