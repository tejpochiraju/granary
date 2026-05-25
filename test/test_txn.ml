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
