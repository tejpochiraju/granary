(** Tests for SQL-level BEGIN / COMMIT / ROLLBACK (Phase 3). *)

module Db = Sqlocaml.Db

let run = Lwt_main.run

let fresh_db () = run (Db.open_in_memory ())

let rows_of stream = run (Lwt_stream.to_list stream)

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error (Db.Parse e) -> Alcotest.failf "parse error in %S: %s" sql e
  | Error (Db.Sema _)  -> Alcotest.failf "sema error in %S" sql
  | Error (Db.Runtime e) -> Alcotest.failf "runtime error in %S: %s" sql e

let query_ints db sql =
  match run (Db.query db sql) with
  | Error _ -> []
  | Ok stream ->
    List.map (fun row ->
      match row.(0) with
      | Db.V_int n -> Int64.to_int n
      | _ -> -1
    ) (rows_of stream)

let test_begin_commit_visible () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "BEGIN";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "COMMIT";
  let ns = query_ints db "SELECT n FROM t ORDER BY n ASC" in
  Alcotest.(check (list int)) "committed rows visible" [1; 2] ns

let test_begin_rollback_invisible () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (0)";
  exec db "BEGIN";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "ROLLBACK";
  let ns = query_ints db "SELECT n FROM t ORDER BY n ASC" in
  Alcotest.(check (list int)) "rolled-back rows invisible" [0] ns

let test_double_begin_errors () =
  let db = fresh_db () in
  (match run (Db.execute db "BEGIN") with Ok () -> () | Error _ -> Alcotest.fail "first BEGIN failed");
  let r = run (Db.execute db "BEGIN") in
  Alcotest.(check bool) "double BEGIN is error" true (Result.is_error r);
  ignore (run (Db.execute db "ROLLBACK"))

let test_commit_without_begin_errors () =
  let db = fresh_db () in
  let r = run (Db.execute db "COMMIT") in
  Alcotest.(check bool) "COMMIT without BEGIN is error" true (Result.is_error r)

let test_rollback_without_begin_errors () =
  let db = fresh_db () in
  let r = run (Db.execute db "ROLLBACK") in
  Alcotest.(check bool) "ROLLBACK without BEGIN is error" true (Result.is_error r)

let test_autocommit_still_works () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (42)";
  let ns = query_ints db "SELECT n FROM t" in
  Alcotest.(check (list int)) "auto-commit insert visible" [42] ns

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
  Alcotest.(check (list int)) "txn update+delete committed" [3; 10] ns

let test_txn_rollback_with_update () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "BEGIN";
  exec db "UPDATE t SET n = 99 WHERE n = 1";
  exec db "ROLLBACK";
  let ns = query_ints db "SELECT n FROM t" in
  Alcotest.(check (list int)) "update rolled back" [1] ns

let test_parse_begin () =
  let db = fresh_db () in
  let r = run (Db.execute db "BEGIN") in
  Alcotest.(check bool) "BEGIN parses" true (Result.is_ok r);
  ignore (run (Db.execute db "ROLLBACK"))

let () =
  Alcotest.run "test_txn"
    [ "begin_commit", [
        Alcotest.test_case "commit_visible"        `Quick test_begin_commit_visible;
        Alcotest.test_case "rollback_invisible"    `Quick test_begin_rollback_invisible;
        Alcotest.test_case "autocommit_still_works" `Quick test_autocommit_still_works;
      ];
      "error_cases", [
        Alcotest.test_case "double_begin"           `Quick test_double_begin_errors;
        Alcotest.test_case "commit_without_begin"   `Quick test_commit_without_begin_errors;
        Alcotest.test_case "rollback_without_begin" `Quick test_rollback_without_begin_errors;
      ];
      "multi_stmt", [
        Alcotest.test_case "update_delete_commit"   `Quick test_txn_with_update_delete;
        Alcotest.test_case "rollback_with_update"   `Quick test_txn_rollback_with_update;
      ];
      "parse", [
        Alcotest.test_case "parse_begin" `Quick test_parse_begin;
      ];
    ]
