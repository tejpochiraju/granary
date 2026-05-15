open Lwt.Syntax
module Db  = Sqlocaml.Db
module Row = Sqlocaml_encoding.Row

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let run = Lwt_main.run

(** Unwrap Error or fail. *)
let err_or_fail label = function
  | Error e -> e
  | Ok _    -> Alcotest.failf "%s: expected Error, got Ok" label

(** Fresh in-memory database. *)
let fresh_db () = run (Db.open_in_memory ())

(** run execute and assert success. *)
let exec db sql =
  run (
    let* result = Db.execute db sql in
    (match result with
     | Ok () -> ()
     | Error _ -> Alcotest.failf "exec: unexpected error for: %s" sql);
    Lwt.return_unit
  )

(** run query, collect rows, assert Ok. *)
let query_ok db sql =
  run (
    let* result = Db.query db sql in
    match result with
    | Error _ -> Alcotest.failf "query_ok: unexpected error for: %s" sql
    | Ok stream ->
      Lwt_stream.to_list stream
  )

(* ------------------------------------------------------------------ *)
(* Value helpers for clean assertions                                    *)
(* ------------------------------------------------------------------ *)

let value_testable : Db.value Alcotest.testable =
  let pp ppf v = match v with
    | Db.V_int  n -> Format.fprintf ppf "V_int(%Ld)" n
    | Db.V_text s -> Format.fprintf ppf "V_text(%S)" s
    | Db.V_null   -> Format.fprintf ppf "V_null"
    | Db.V_real f -> Format.fprintf ppf "V_real(%h)" f
    | Db.V_blob b -> Format.fprintf ppf "V_blob(%d bytes)" (Bytes.length b)
  in
  let eq a b = match a, b with
    | Db.V_int  x, Db.V_int  y -> Int64.equal x y
    | Db.V_text x, Db.V_text y -> String.equal x y
    | Db.V_null,   Db.V_null   -> true
    | Db.V_real x, Db.V_real y -> Float.equal x y
    | Db.V_blob x, Db.V_blob y -> Bytes.equal x y
    | _,           _           -> false
  in
  Alcotest.testable pp eq

let row_testable : Db.row Alcotest.testable =
  let pp ppf arr =
    Format.fprintf ppf "[|";
    Array.iter (fun v ->
      Format.fprintf ppf " ";
      (Alcotest.pp value_testable) ppf v;
    ) arr;
    Format.fprintf ppf " |]"
  in
  let eq a b =
    Array.length a = Array.length b &&
    Array.for_all2 (fun x y -> Alcotest.equal value_testable x y) a b
  in
  Alcotest.testable pp eq

(* ------------------------------------------------------------------ *)
(* Group 1: Walking skeleton (Phase 0 demo)                             *)
(* ------------------------------------------------------------------ *)

let walking_skeleton () =
  let db = fresh_db () in
  exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT NOT NULL)";
  exec db "INSERT INTO users (id, name) VALUES (1, 'alice')";
  exec db "INSERT INTO users (id, name) VALUES (2, 'bob')";
  exec db "INSERT INTO users (id, name) VALUES (3, 'carol')";
  let rows = query_ok db "SELECT id, name FROM users WHERE id = 2" in
  Alcotest.(check int) "exactly 1 row" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.(check int) "2 columns" 2 (Array.length row);
  Alcotest.check value_testable "id = V_int 2L"    (Db.V_int 2L)    row.(0);
  Alcotest.check value_testable "name = V_text bob" (Db.V_text "bob") row.(1)

(* ------------------------------------------------------------------ *)
(* Group 2: SELECT * variations                                          *)
(* ------------------------------------------------------------------ *)

let select_all_rows () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "INSERT INTO t (n) VALUES (3)";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "3 rows" 3 (List.length rows)

let select_star_columns () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, name TEXT)";
  exec db "INSERT INTO t (id, name) VALUES (10, 'foo')";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.(check int) "2 columns (star preserves order)" 2 (Array.length row);
  Alcotest.check value_testable "col 0 = id=10"     (Db.V_int 10L)    row.(0);
  Alcotest.check value_testable "col 1 = name=foo"  (Db.V_text "foo") row.(1)

let select_named_cols () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, name TEXT)";
  exec db "INSERT INTO t (id, name) VALUES (7, 'bar')";
  let rows = query_ok db "SELECT id FROM t" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.(check int) "1 column (named projection)" 1 (Array.length row);
  Alcotest.check value_testable "id=7" (Db.V_int 7L) row.(0)

(* ------------------------------------------------------------------ *)
(* Group 3: Multiple tables                                              *)
(* ------------------------------------------------------------------ *)

let two_tables () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (x INTEGER)";
  exec db "CREATE TABLE b (y TEXT)";
  exec db "INSERT INTO a (x) VALUES (42)";
  exec db "INSERT INTO b (y) VALUES ('hello')";
  let rows_a = query_ok db "SELECT * FROM a" in
  let rows_b = query_ok db "SELECT * FROM b" in
  Alcotest.(check int) "a: 1 row" 1 (List.length rows_a);
  Alcotest.(check int) "b: 1 row" 1 (List.length rows_b);
  Alcotest.check value_testable "a.x=42"       (Db.V_int 42L)       (List.hd rows_a).(0);
  Alcotest.check value_testable "b.y=hello"    (Db.V_text "hello")  (List.hd rows_b).(0)

let table_isolation () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (x INTEGER)";
  exec db "CREATE TABLE b (y INTEGER)";
  exec db "INSERT INTO a (x) VALUES (1)";
  exec db "INSERT INTO a (x) VALUES (2)";
  exec db "INSERT INTO b (y) VALUES (99)";
  let rows_a = query_ok db "SELECT * FROM a" in
  let rows_b = query_ok db "SELECT * FROM b" in
  Alcotest.(check int) "a: 2 rows" 2 (List.length rows_a);
  Alcotest.(check int) "b: 1 row"  1 (List.length rows_b)

(* ------------------------------------------------------------------ *)
(* Group 4: Error propagation through the API                           *)
(* ------------------------------------------------------------------ *)

let parse_error () =
  let db = fresh_db () in
  let result = run (Db.execute db "SLECT * FROM t;") in
  (match err_or_fail "parse_error" result with
   | Db.Parse _ -> ()
   | _ -> Alcotest.fail "expected Parse error")

let unknown_table () =
  let db = fresh_db () in
  let result = run (Db.query db "SELECT * FROM ghost") in
  (match err_or_fail "unknown_table" result with
   | Db.Sema (Sqlocaml_sql.Sema.Unknown_table tbl) ->
     Alcotest.(check string) "table name is ghost" "ghost" tbl
   | _ -> Alcotest.fail "expected Sema(Unknown_table \"ghost\")")

let unknown_column () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x INTEGER)";
  let result = run (Db.query db "SELECT bogus FROM t") in
  (match err_or_fail "unknown_column" result with
   | Db.Sema (Sqlocaml_sql.Sema.Unknown_column _) -> ()
   | _ -> Alcotest.fail "expected Sema(Unknown_column ...)")

let type_mismatch () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x INTEGER)";
  let result = run (Db.execute db "INSERT INTO t (x) VALUES ('text')") in
  (match err_or_fail "type_mismatch" result with
   | Db.Sema (Sqlocaml_sql.Sema.Type_mismatch _) -> ()
   | _ -> Alcotest.fail "expected Sema(Type_mismatch ...)")

let arity_mismatch () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
  let result = run (Db.execute db "INSERT INTO t (a) VALUES (1, 2)") in
  (match err_or_fail "arity_mismatch" result with
   | Db.Sema (Sqlocaml_sql.Sema.Arity_mismatch _) -> ()
   | _ -> Alcotest.fail "expected Sema(Arity_mismatch ...)")

(* ------------------------------------------------------------------ *)
(* Group 5: NULL handling end-to-end                                     *)
(* ------------------------------------------------------------------ *)

let insert_null () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, val TEXT)";
  exec db "INSERT INTO t (id, val) VALUES (1, NULL)";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.(check int) "2 columns" 2 (Array.length row);
  Alcotest.check value_testable "id=1"        (Db.V_int 1L) row.(0);
  Alcotest.check value_testable "val=V_null"  Db.V_null     row.(1)

let where_null_no_match () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, val TEXT)";
  exec db "INSERT INTO t (id, val) VALUES (1, NULL)";
  (* NULL doesn't compare equal to anything *)
  let rows = query_ok db "SELECT * FROM t WHERE val = 'something'" in
  Alcotest.(check int) "0 rows (NULL doesn't match)" 0 (List.length rows)

(* ------------------------------------------------------------------ *)
(* Group 6: Rowid ordering                                               *)
(* ------------------------------------------------------------------ *)

let insert_order () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  (* Insert values out of sorted order to confirm rowid (insertion) ordering *)
  exec db "INSERT INTO t (n) VALUES (30)";
  exec db "INSERT INTO t (n) VALUES (10)";
  exec db "INSERT INTO t (n) VALUES (20)";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "3 rows" 3 (List.length rows);
  let ns = List.map (fun r -> match r.(0) with
    | Db.V_int n -> n
    | _          -> Int64.minus_one) rows in
  (* Expect insertion order: 30, 10, 20 *)
  Alcotest.(check (list int64)) "insertion order preserved" [30L; 10L; 20L] ns

(* ------------------------------------------------------------------ *)
(* Group 7: Stress / robustness                                          *)
(* ------------------------------------------------------------------ *)

let many_rows () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, val TEXT)";
  for i = 1 to 100 do
    let sql = Printf.sprintf "INSERT INTO t (id, val) VALUES (%d, 'v%d')" i i in
    exec db sql
  done;
  (* SELECT * → 100 rows *)
  let all_rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "100 rows" 100 (List.length all_rows);
  (* SELECT WHERE id=50 → 1 row *)
  let filtered = query_ok db "SELECT * FROM t WHERE id = 50" in
  Alcotest.(check int) "1 filtered row" 1 (List.length filtered);
  let row = List.hd filtered in
  Alcotest.check value_testable "id=50" (Db.V_int 50L)    row.(0);
  Alcotest.check value_testable "val"   (Db.V_text "v50") row.(1)

let close_then_reuse () =
  (* First db: open, use, close *)
  let db1 = fresh_db () in
  exec db1 "CREATE TABLE t (x INTEGER)";
  exec db1 "INSERT INTO t (x) VALUES (1)";
  run (Db.close db1);
  (* Second db: open fresh, fully independent *)
  let db2 = fresh_db () in
  exec db2 "CREATE TABLE s (y TEXT)";
  exec db2 "INSERT INTO s (y) VALUES ('hello')";
  let rows = query_ok db2 "SELECT * FROM s" in
  Alcotest.(check int) "db2 has 1 row" 1 (List.length rows);
  Alcotest.check value_testable "db2 row value" (Db.V_text "hello") (List.hd rows).(0);
  run (Db.close db2)

(* ------------------------------------------------------------------ *)
(* Group 8: Row content verification                                     *)
(* ------------------------------------------------------------------ *)

let row_content_exact () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, name TEXT, score INTEGER)";
  exec db "INSERT INTO t (id, name, score) VALUES (42, 'eve', 99)";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  let expected = [| Db.V_int 42L; Db.V_text "eve"; Db.V_int 99L |] in
  Alcotest.check row_testable "exact row content" expected (List.hd rows)

let select_partial_projection () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER, b TEXT, c INTEGER)";
  exec db "INSERT INTO t (a, b, c) VALUES (1, 'x', 2)";
  exec db "INSERT INTO t (a, b, c) VALUES (3, 'y', 4)";
  let rows = query_ok db "SELECT a, c FROM t WHERE a = 3" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.(check int) "2 columns in projection" 2 (Array.length row);
  Alcotest.check value_testable "a=3" (Db.V_int 3L) row.(0);
  Alcotest.check value_testable "c=4" (Db.V_int 4L) row.(1)

let empty_table_select () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x INTEGER)";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "empty table → 0 rows" 0 (List.length rows)

let select_where_text () =
  let db = fresh_db () in
  exec db "CREATE TABLE words (id INTEGER, word TEXT)";
  exec db "INSERT INTO words (id, word) VALUES (1, 'apple')";
  exec db "INSERT INTO words (id, word) VALUES (2, 'banana')";
  exec db "INSERT INTO words (id, word) VALUES (3, 'cherry')";
  let rows = query_ok db "SELECT id, word FROM words WHERE word = 'banana'" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.check value_testable "id=2"       (Db.V_int 2L)       row.(0);
  Alcotest.check value_testable "word=banana" (Db.V_text "banana") row.(1)

(* ------------------------------------------------------------------ *)
(* Group 9: Already_exists error                                         *)
(* ------------------------------------------------------------------ *)

let already_exists_error () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x INTEGER)";
  let result = run (Db.execute db "CREATE TABLE t (y TEXT)") in
  (match err_or_fail "already_exists" result with
   | Db.Sema (Sqlocaml_sql.Sema.Already_exists name) ->
     Alcotest.(check string) "table name is t" "t" name
   | _ -> Alcotest.fail "expected Sema(Already_exists \"t\")")

let parse_failure_via_lexer () =
  run (
    let* db = Db.open_in_memory () in
    (* '@' is an unknown character — lexer calls failwith, not Parser.Error *)
    let* r = Db.execute db "@ invalid" in
    (match r with
     | Error (Db.Parse _) -> ()
     | _ -> Alcotest.fail "expected Parse error from lexer Failure");
    Db.close db
  )

let execute_with_select_is_runtime_error () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (x INTEGER);" in
    (* execute with a SELECT — planner produces read op → Exec.execute raises → Runtime *)
    let* r = Db.execute db "SELECT * FROM t;" in
    (match r with
     | Error (Db.Runtime _) -> ()
     | _ -> Alcotest.fail "expected Runtime error for SELECT via execute");
    Db.close db
  )

let query_with_insert_is_runtime_error () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (x INTEGER);" in
    (* query with INSERT — planner produces write op → Exec.query raises → Runtime *)
    let* r = Db.query db "INSERT INTO t (x) VALUES (1);" in
    (match r with
     | Error (Db.Runtime _) -> ()
     | _ -> Alcotest.fail "expected Runtime error for INSERT via query");
    Db.close db
  )

(* ------------------------------------------------------------------ *)
(* Group 10: REAL column type end-to-end                                *)
(* ------------------------------------------------------------------ *)

let real_create_insert_select () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, val REAL)";
  exec db "INSERT INTO t (id, val) VALUES (1, 3.14)";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.(check int) "2 columns" 2 (Array.length row);
  Alcotest.check value_testable "id=1" (Db.V_int 1L) row.(0);
  Alcotest.check value_testable "val=3.14" (Db.V_real 3.14) row.(1)

let real_negative () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x REAL)";
  exec db "INSERT INTO t (x) VALUES (-1.5)";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  Alcotest.check value_testable "x=-1.5" (Db.V_real (-1.5)) (List.hd rows).(0)

let real_zero () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x REAL)";
  exec db "INSERT INTO t (x) VALUES (0.0)";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  Alcotest.check value_testable "x=0.0" (Db.V_real 0.0) (List.hd rows).(0)

let real_null () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x REAL)";
  exec db "INSERT INTO t (x) VALUES (NULL)";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  Alcotest.check value_testable "x=null" Db.V_null (List.hd rows).(0)

let real_multiple_rows () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, x REAL)";
  exec db "INSERT INTO t (id, x) VALUES (1, 1.0)";
  exec db "INSERT INTO t (id, x) VALUES (2, 2.5)";
  exec db "INSERT INTO t (id, x) VALUES (3, -3.14)";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "3 rows" 3 (List.length rows);
  Alcotest.check value_testable "row0.x" (Db.V_real 1.0)    (List.nth rows 0).(1);
  Alcotest.check value_testable "row1.x" (Db.V_real 2.5)    (List.nth rows 1).(1);
  Alcotest.check value_testable "row2.x" (Db.V_real (-3.14)) (List.nth rows 2).(1)

let real_type_mismatch () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x REAL)";
  (* Inserting an integer literal into a REAL column should fail type check *)
  let result = run (Db.execute db "INSERT INTO t (x) VALUES ('text')") in
  (match err_or_fail "real_type_mismatch" result with
   | Db.Sema (Sqlocaml_sql.Sema.Type_mismatch _) -> ()
   | _ -> Alcotest.fail "expected Sema(Type_mismatch)")

(* ------------------------------------------------------------------ *)
(* Group 11: BLOB column type end-to-end                                *)
(* Note: BLOB literals can't be expressed in SQL directly (no hex      *)
(* literal syntax in Phase 0). We test NULL insertion and type errors.  *)
(* ------------------------------------------------------------------ *)

let blob_null_insert () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, data BLOB)";
  exec db "INSERT INTO t (id, data) VALUES (1, NULL)";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.check value_testable "id=1"    (Db.V_int 1L) row.(0);
  Alcotest.check value_testable "data=null" Db.V_null   row.(1)

let blob_type_mismatch () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (data BLOB)";
  let result = run (Db.execute db "INSERT INTO t (data) VALUES ('text')") in
  (match err_or_fail "blob_type_mismatch" result with
   | Db.Sema (Sqlocaml_sql.Sema.Type_mismatch _) -> ()
   | _ -> Alcotest.fail "expected Sema(Type_mismatch) for text into blob col")

let blob_schema_preserved () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, data BLOB, name TEXT)";
  exec db "INSERT INTO t (id, data, name) VALUES (42, NULL, 'hello')";
  let rows = query_ok db "SELECT id, name FROM t" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.(check int) "2 cols projected" 2 (Array.length row);
  Alcotest.check value_testable "id=42"      (Db.V_int 42L)       row.(0);
  Alcotest.check value_testable "name=hello" (Db.V_text "hello")  row.(1)

(* ------------------------------------------------------------------ *)
(* Group 12: REAL and BLOB together                                     *)
(* ------------------------------------------------------------------ *)

let real_and_blob_together () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, score REAL, payload BLOB)";
  exec db "INSERT INTO t (id, score, payload) VALUES (1, 99.5, NULL)";
  exec db "INSERT INTO t (id, score, payload) VALUES (2, NULL, NULL)";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "2 rows" 2 (List.length rows);
  let r0 = List.nth rows 0 in
  let r1 = List.nth rows 1 in
  Alcotest.check value_testable "r0.id"      (Db.V_int 1L)    r0.(0);
  Alcotest.check value_testable "r0.score"   (Db.V_real 99.5) r0.(1);
  Alcotest.check value_testable "r0.payload" Db.V_null         r0.(2);
  Alcotest.check value_testable "r1.id"      (Db.V_int 2L)    r1.(0);
  Alcotest.check value_testable "r1.score"   Db.V_null         r1.(1);
  Alcotest.check value_testable "r1.payload" Db.V_null         r1.(2)

let already_exists_real_blob () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (f REAL, b BLOB)";
  let result = run (Db.execute db "CREATE TABLE t (x INTEGER)") in
  (match err_or_fail "already_exists" result with
   | Db.Sema (Sqlocaml_sql.Sema.Already_exists name) ->
     Alcotest.(check string) "table name is t" "t" name
   | _ -> Alcotest.fail "expected Sema(Already_exists)")

(* ------------------------------------------------------------------ *)
(* Runner                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "E2E" [
    "walking_skeleton", [
      Alcotest.test_case "walking_skeleton" `Quick walking_skeleton;
    ];
    "select_variations", [
      Alcotest.test_case "select_all_rows"    `Quick select_all_rows;
      Alcotest.test_case "select_star_columns" `Quick select_star_columns;
      Alcotest.test_case "select_named_cols"  `Quick select_named_cols;
    ];
    "multiple_tables", [
      Alcotest.test_case "two_tables"      `Quick two_tables;
      Alcotest.test_case "table_isolation" `Quick table_isolation;
    ];
    "error_propagation", [
      Alcotest.test_case "parse_error"                       `Quick parse_error;
      Alcotest.test_case "unknown_table"                     `Quick unknown_table;
      Alcotest.test_case "unknown_column"                    `Quick unknown_column;
      Alcotest.test_case "type_mismatch"                     `Quick type_mismatch;
      Alcotest.test_case "arity_mismatch"                    `Quick arity_mismatch;
      Alcotest.test_case "already_exists"                    `Quick already_exists_error;
      Alcotest.test_case "parse_failure_via_lexer"           `Quick parse_failure_via_lexer;
      Alcotest.test_case "execute_with_select_is_runtime"    `Quick execute_with_select_is_runtime_error;
      Alcotest.test_case "query_with_insert_is_runtime"      `Quick query_with_insert_is_runtime_error;
    ];
    "null_handling", [
      Alcotest.test_case "insert_null"        `Quick insert_null;
      Alcotest.test_case "where_null_no_match" `Quick where_null_no_match;
    ];
    "rowid_ordering", [
      Alcotest.test_case "insert_order" `Quick insert_order;
    ];
    "stress_robustness", [
      Alcotest.test_case "many_rows"        `Quick many_rows;
      Alcotest.test_case "close_then_reuse" `Quick close_then_reuse;
    ];
    "row_content", [
      Alcotest.test_case "row_content_exact"         `Quick row_content_exact;
      Alcotest.test_case "select_partial_projection"  `Quick select_partial_projection;
      Alcotest.test_case "empty_table_select"         `Quick empty_table_select;
      Alcotest.test_case "select_where_text"          `Quick select_where_text;
    ];
    "real_column", [
      Alcotest.test_case "real_create_insert_select" `Quick real_create_insert_select;
      Alcotest.test_case "real_negative"             `Quick real_negative;
      Alcotest.test_case "real_zero"                 `Quick real_zero;
      Alcotest.test_case "real_null"                 `Quick real_null;
      Alcotest.test_case "real_multiple_rows"        `Quick real_multiple_rows;
      Alcotest.test_case "real_type_mismatch"        `Quick real_type_mismatch;
    ];
    "blob_column", [
      Alcotest.test_case "blob_null_insert"          `Quick blob_null_insert;
      Alcotest.test_case "blob_type_mismatch"        `Quick blob_type_mismatch;
      Alcotest.test_case "blob_schema_preserved"     `Quick blob_schema_preserved;
    ];
    "real_and_blob", [
      Alcotest.test_case "real_and_blob_together"    `Quick real_and_blob_together;
      Alcotest.test_case "already_exists_real_blob"  `Quick already_exists_real_blob;
    ];
  ]
