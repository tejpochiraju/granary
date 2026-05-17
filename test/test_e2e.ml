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

let fmt_err e =
  Format.asprintf "%a" Db.pp_error e

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
(* Group 13: ORDER BY end-to-end                                        *)
(* ------------------------------------------------------------------ *)

let order_by_asc () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (30)";
  exec db "INSERT INTO t (n) VALUES (10)";
  exec db "INSERT INTO t (n) VALUES (20)";
  let rows = query_ok db "SELECT * FROM t ORDER BY n ASC" in
  Alcotest.(check int) "3 rows" 3 (List.length rows);
  let ns = List.map (fun r -> match r.(0) with
    | Db.V_int n -> n | _ -> Int64.minus_one) rows in
  Alcotest.(check (list int64)) "ascending order" [10L; 20L; 30L] ns

let order_by_desc () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (30)";
  exec db "INSERT INTO t (n) VALUES (10)";
  exec db "INSERT INTO t (n) VALUES (20)";
  let rows = query_ok db "SELECT * FROM t ORDER BY n DESC" in
  Alcotest.(check int) "3 rows" 3 (List.length rows);
  let ns = List.map (fun r -> match r.(0) with
    | Db.V_int n -> n | _ -> Int64.minus_one) rows in
  Alcotest.(check (list int64)) "descending order" [30L; 20L; 10L] ns

let order_by_default_asc () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (3)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  let rows = query_ok db "SELECT * FROM t ORDER BY n" in
  let ns = List.map (fun r -> match r.(0) with
    | Db.V_int n -> n | _ -> Int64.minus_one) rows in
  Alcotest.(check (list int64)) "default asc" [1L; 2L; 3L] ns

let limit_no_order () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (10)";
  exec db "INSERT INTO t (n) VALUES (20)";
  exec db "INSERT INTO t (n) VALUES (30)";
  exec db "INSERT INTO t (n) VALUES (40)";
  exec db "INSERT INTO t (n) VALUES (50)";
  let rows = query_ok db "SELECT * FROM t LIMIT 3" in
  Alcotest.(check int) "limit 3" 3 (List.length rows);
  let ns = List.map (fun r -> match r.(0) with
    | Db.V_int n -> n | _ -> Int64.minus_one) rows in
  Alcotest.(check (list int64)) "insertion order first 3" [10L; 20L; 30L] ns

let limit_with_offset () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (10)";
  exec db "INSERT INTO t (n) VALUES (20)";
  exec db "INSERT INTO t (n) VALUES (30)";
  exec db "INSERT INTO t (n) VALUES (40)";
  exec db "INSERT INTO t (n) VALUES (50)";
  let rows = query_ok db "SELECT * FROM t LIMIT 3 OFFSET 2" in
  Alcotest.(check int) "limit 3 offset 2 → 3 rows" 3 (List.length rows);
  let ns = List.map (fun r -> match r.(0) with
    | Db.V_int n -> n | _ -> Int64.minus_one) rows in
  Alcotest.(check (list int64)) "rows at positions 2,3,4" [30L; 40L; 50L] ns

let order_by_then_limit () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (30)";
  exec db "INSERT INTO t (n) VALUES (10)";
  exec db "INSERT INTO t (n) VALUES (40)";
  exec db "INSERT INTO t (n) VALUES (20)";
  let rows = query_ok db "SELECT * FROM t ORDER BY n ASC LIMIT 2" in
  Alcotest.(check int) "limit 2 after sort" 2 (List.length rows);
  let ns = List.map (fun r -> match r.(0) with
    | Db.V_int n -> n | _ -> Int64.minus_one) rows in
  Alcotest.(check (list int64)) "smallest 2" [10L; 20L] ns

let order_by_limit_offset () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (30)";
  exec db "INSERT INTO t (n) VALUES (10)";
  exec db "INSERT INTO t (n) VALUES (40)";
  exec db "INSERT INTO t (n) VALUES (20)";
  let rows = query_ok db "SELECT * FROM t ORDER BY n ASC LIMIT 2 OFFSET 1" in
  Alcotest.(check int) "limit 2 offset 1 after sort" 2 (List.length rows);
  let ns = List.map (fun r -> match r.(0) with
    | Db.V_int n -> n | _ -> Int64.minus_one) rows in
  Alcotest.(check (list int64)) "2nd and 3rd smallest" [20L; 30L] ns

let order_by_null_first () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 5)";
  exec db "INSERT INTO t (id, n) VALUES (2, NULL)";
  exec db "INSERT INTO t (id, n) VALUES (3, 2)";
  let rows = query_ok db "SELECT id, n FROM t ORDER BY n ASC" in
  Alcotest.(check int) "3 rows" 3 (List.length rows);
  (* NULL < 2 < 5 in ASC (matches SQLite: NULLs are less than any value) *)
  (match (List.nth rows 0).(1) with
   | Db.V_null -> ()
   | _ -> Alcotest.fail "expected NULL first");
  (match (List.nth rows 1).(1) with
   | Db.V_int 2L -> ()
   | _ -> Alcotest.fail "expected 2 second");
  (match (List.nth rows 2).(1) with
   | Db.V_int 5L -> ()
   | _ -> Alcotest.fail "expected 5 last")

let order_by_text () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (name TEXT)";
  exec db "INSERT INTO t (name) VALUES ('charlie')";
  exec db "INSERT INTO t (name) VALUES ('alice')";
  exec db "INSERT INTO t (name) VALUES ('bob')";
  let rows = query_ok db "SELECT * FROM t ORDER BY name ASC" in
  let names = List.map (fun r -> match r.(0) with
    | Db.V_text s -> s | _ -> "") rows in
  Alcotest.(check (list string)) "alphabetical order" ["alice"; "bob"; "charlie"] names

let order_by_expr () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (3)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (2)";
  let rows = query_ok db "SELECT n FROM t ORDER BY n * -1" in
  let vals = List.map (fun r -> match r.(0) with
    | Db.V_int n -> n | _ -> -1L) rows in
  Alcotest.(check (list int64)) "order_by_expr" [3L; 2L; 1L] vals

let test_order_by_expr_join () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, n INTEGER)" in
    let* _ = Db.execute db "CREATE TABLE u (tid INTEGER, v TEXT)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 30)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 10)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (3, 20)" in
    let* _ = Db.execute db "INSERT INTO u VALUES (1, 'a')" in
    let* _ = Db.execute db "INSERT INTO u VALUES (2, 'b')" in
    let* _ = Db.execute db "INSERT INTO u VALUES (3, 'c')" in
    let* r = Db.query db "SELECT u.v FROM t JOIN u ON t.id = u.tid ORDER BY t.n" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
    let vals = List.map (fun r -> match r.(0) with Db.V_text s -> s | _ -> "?") rows in
    Alcotest.(check (list string)) "join_order_by" ["b"; "c"; "a"] vals;
    Db.close db
  )

(* ------------------------------------------------------------------ *)
(* Group 14: QCheck ORDER BY properties                                 *)
(* ------------------------------------------------------------------ *)

let qcheck_order_by_asc_sorted =
  QCheck.Test.make
    ~name:"order_by_asc: result is sorted ascending"
    ~count:10_000
    QCheck.(list_size Gen.(0 -- 20) nat_small)
    (fun ns ->
      let db = Lwt_main.run (Db.open_in_memory ()) in
      Lwt_main.run (
        let* _ = Db.execute db "CREATE TABLE t (n INTEGER)" in
        let* () = Lwt_list.iter_s (fun n ->
          let sql = Printf.sprintf "INSERT INTO t (n) VALUES (%d)" n in
          let* _ = Db.execute db sql in
          Lwt.return_unit
        ) ns in
        let* result = Db.query db "SELECT * FROM t ORDER BY n ASC" in
        match result with
        | Error _ -> Lwt.return false
        | Ok stream ->
          let* rows = Lwt_stream.to_list stream in
          let got = List.map (fun r -> match r.(0) with
            | Db.V_int x -> Int64.to_int x | _ -> 0) rows in
          let sorted = List.sort compare ns in
          Lwt.return (got = sorted)
      ))

let qcheck_order_by_desc_sorted =
  QCheck.Test.make
    ~name:"order_by_desc: result is sorted descending"
    ~count:10_000
    QCheck.(list_size Gen.(0 -- 20) nat_small)
    (fun ns ->
      let db = Lwt_main.run (Db.open_in_memory ()) in
      Lwt_main.run (
        let* _ = Db.execute db "CREATE TABLE t (n INTEGER)" in
        let* () = Lwt_list.iter_s (fun n ->
          let sql = Printf.sprintf "INSERT INTO t (n) VALUES (%d)" n in
          let* _ = Db.execute db sql in
          Lwt.return_unit
        ) ns in
        let* result = Db.query db "SELECT * FROM t ORDER BY n DESC" in
        match result with
        | Error _ -> Lwt.return false
        | Ok stream ->
          let* rows = Lwt_stream.to_list stream in
          let got = List.map (fun r -> match r.(0) with
            | Db.V_int x -> Int64.to_int x | _ -> 0) rows in
          let sorted = List.sort (fun a b -> compare b a) ns in
          Lwt.return (got = sorted)
      ))

let qcheck_limit_count =
  QCheck.Test.make
    ~name:"limit n: result has at most n rows"
    ~count:10_000
    QCheck.(pair (list_size Gen.(0 -- 20) nat_small) (1 -- 10))
    (fun (ns, lim) ->
      let db = Lwt_main.run (Db.open_in_memory ()) in
      Lwt_main.run (
        let* _ = Db.execute db "CREATE TABLE t (n INTEGER)" in
        let* () = Lwt_list.iter_s (fun n ->
          let sql = Printf.sprintf "INSERT INTO t (n) VALUES (%d)" n in
          let* _ = Db.execute db sql in
          Lwt.return_unit
        ) ns in
        let sql = Printf.sprintf "SELECT * FROM t LIMIT %d" lim in
        let* result = Db.query db sql in
        match result with
        | Error _ -> Lwt.return false
        | Ok stream ->
          let* rows = Lwt_stream.to_list stream in
          let n_got = List.length rows in
          let n_exp = min lim (List.length ns) in
          Lwt.return (n_got = n_exp)
      ))

(* ------------------------------------------------------------------ *)
(* Group 15: CREATE INDEX + index lookup                                *)
(* ------------------------------------------------------------------ *)

let create_index_simple () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, name TEXT)";
  exec db "INSERT INTO t (id, name) VALUES (1, 'alice')";
  exec db "INSERT INTO t (id, name) VALUES (2, 'bob')";
  exec db "INSERT INTO t (id, name) VALUES (3, 'carol')";
  exec db "CREATE INDEX idx_t_id ON t (id)";
  let rows = query_ok db "SELECT id, name FROM t WHERE id = 2" in
  Alcotest.(check int) "1 row found via index" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.check value_testable "id=2"  (Db.V_int 2L)    row.(0);
  Alcotest.check value_testable "name"  (Db.V_text "bob") row.(1)

let create_index_on_text_column () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, name TEXT)";
  exec db "INSERT INTO t (id, name) VALUES (1, 'alice')";
  exec db "INSERT INTO t (id, name) VALUES (2, 'bob')";
  exec db "INSERT INTO t (id, name) VALUES (3, 'carol')";
  exec db "CREATE INDEX idx_name ON t (name)";
  let rows = query_ok db "SELECT id FROM t WHERE name = 'bob'" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  Alcotest.check value_testable "id=2" (Db.V_int 2L) (List.hd rows).(0)

let create_unique_index () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, name TEXT)";
  exec db "INSERT INTO t (id, name) VALUES (1, 'alice')";
  exec db "CREATE UNIQUE INDEX idx_id ON t (id)";
  let rows = query_ok db "SELECT * FROM t WHERE id = 1" in
  Alcotest.(check int) "1 row" 1 (List.length rows)

let unique_index_rejects_duplicate () =
  (* Second INSERT with same indexed value must return an error. *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, name TEXT)";
  exec db "CREATE UNIQUE INDEX idx ON t (id)";
  exec db "INSERT INTO t (id, name) VALUES (1, 'alice')";
  let result = run (Db.execute db "INSERT INTO t (id, name) VALUES (1, 'bob')") in
  (match err_or_fail "unique_index_rejects_duplicate" result with
   | Db.Runtime _ -> ()
   | _ -> Alcotest.fail "expected Runtime error for UNIQUE constraint violation")

let unique_index_allows_distinct_values () =
  (* Two rows with different values on a UNIQUE index must both succeed. *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, name TEXT)";
  exec db "CREATE UNIQUE INDEX idx ON t (id)";
  exec db "INSERT INTO t (id, name) VALUES (1, 'alice')";
  exec db "INSERT INTO t (id, name) VALUES (2, 'bob')";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "2 rows" 2 (List.length rows)

let index_lookup_no_match () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, name TEXT)";
  exec db "INSERT INTO t (id, name) VALUES (1, 'alice')";
  exec db "INSERT INTO t (id, name) VALUES (2, 'bob')";
  exec db "CREATE INDEX idx_id ON t (id)";
  let rows = query_ok db "SELECT * FROM t WHERE id = 99" in
  Alcotest.(check int) "0 rows for missing key" 0 (List.length rows)

let index_lookup_returns_all_matches () =
  (* Index lookup must return every row whose key matches, not just one. *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (val INTEGER, name TEXT)";
  exec db "INSERT INTO t (val, name) VALUES (5, 'a')";
  exec db "INSERT INTO t (val, name) VALUES (5, 'b')";
  exec db "INSERT INTO t (val, name) VALUES (5, 'c')";
  exec db "INSERT INTO t (val, name) VALUES (7, 'd')";
  exec db "CREATE INDEX idx_val ON t (val)";
  let rows = query_ok db "SELECT name FROM t WHERE val = 5" in
  Alcotest.(check int) "3 matches" 3 (List.length rows);
  let names = List.map (fun r -> match r.(0) with
    | Db.V_text s -> s | _ -> "") rows |> List.sort String.compare in
  Alcotest.(check (list string)) "matched names" ["a"; "b"; "c"] names

let index_lookup_matches_seq_scan () =
  (* For each value we insert, verify the index lookup result equals
     what a sequential scan with WHERE would return. *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (val INTEGER, name TEXT)";
  let data = [
    (10, "alpha"); (20, "beta"); (10, "gamma"); (30, "delta"); (20, "epsilon")
  ] in
  List.iter (fun (n, s) ->
    let sql = Printf.sprintf "INSERT INTO t (val, name) VALUES (%d, '%s')" n s in
    exec db sql
  ) data;
  (* Snapshot pre-index results *)
  let pre_10 = query_ok db "SELECT name FROM t WHERE val = 10" in
  let pre_20 = query_ok db "SELECT name FROM t WHERE val = 20" in
  let pre_30 = query_ok db "SELECT name FROM t WHERE val = 30" in
  exec db "CREATE INDEX idx_val ON t (val)";
  let post_10 = query_ok db "SELECT name FROM t WHERE val = 10" in
  let post_20 = query_ok db "SELECT name FROM t WHERE val = 20" in
  let post_30 = query_ok db "SELECT name FROM t WHERE val = 30" in
  let names rows =
    List.map (fun r -> match r.(0) with Db.V_text s -> s | _ -> "") rows
    |> List.sort String.compare
  in
  Alcotest.(check (list string)) "val=10 results" (names pre_10) (names post_10);
  Alcotest.(check (list string)) "val=20 results" (names pre_20) (names post_20);
  Alcotest.(check (list string)) "val=30 results" (names pre_30) (names post_30)

let index_lookup_after_inserts () =
  (* Inserts after CREATE INDEX must also be findable. *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, name TEXT)";
  exec db "INSERT INTO t (id, name) VALUES (1, 'alice')";
  exec db "CREATE INDEX idx_id ON t (id)";
  exec db "INSERT INTO t (id, name) VALUES (2, 'bob')";
  exec db "INSERT INTO t (id, name) VALUES (3, 'carol')";
  let rows = query_ok db "SELECT name FROM t WHERE id = 3" in
  Alcotest.(check int) "1 row found" 1 (List.length rows);
  Alcotest.check value_testable "name=carol" (Db.V_text "carol") (List.hd rows).(0)

let create_index_unknown_table () =
  let db = fresh_db () in
  let result = run (Db.execute db "CREATE INDEX idx ON ghost (x)") in
  (match err_or_fail "create_index_unknown_table" result with
   | Db.Sema (Sqlocaml_sql.Sema.Unknown_table _) -> ()
   | _ -> Alcotest.fail "expected Sema(Unknown_table)")

let create_index_unknown_column () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER)";
  let result = run (Db.execute db "CREATE INDEX idx ON t (bogus)") in
  (match err_or_fail "create_index_unknown_column" result with
   | Db.Sema (Sqlocaml_sql.Sema.Unknown_column _) -> ()
   | _ -> Alcotest.fail "expected Sema(Unknown_column)")

let create_index_duplicate () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER)";
  exec db "CREATE INDEX idx ON t (id)";
  let result = run (Db.execute db "CREATE INDEX idx ON t (id)") in
  (match err_or_fail "create_index_duplicate" result with
   | Db.Sema (Sqlocaml_sql.Sema.Already_exists _) -> ()
   | _ -> Alcotest.fail "expected Sema(Already_exists)")

let index_lookup_where_null_no_match () =
  (* WHERE col = NULL must return 0 rows even when an index exists
     and there are NULL-valued rows. *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, val TEXT)";
  exec db "INSERT INTO t (id, val) VALUES (1, NULL)";
  exec db "INSERT INTO t (id, val) VALUES (2, 'a')";
  exec db "CREATE INDEX idx ON t (val)";
  let rows = query_ok db "SELECT * FROM t WHERE val = NULL" in
  Alcotest.(check int) "0 rows for col = NULL" 0 (List.length rows)

(* ------------------------------------------------------------------ *)
(* Multi-column index test                                             *)
(* ------------------------------------------------------------------ *)

let test_multi_col_index () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER, b INTEGER, c TEXT)";
  exec db "CREATE INDEX idx_ab ON t (a, b)";
  exec db "INSERT INTO t VALUES (1, 2, 'x')";
  exec db "INSERT INTO t VALUES (3, 4, 'y')";
  (* multi-column index was built; seq scan with WHERE a=1 still works *)
  let rows = query_ok db "SELECT c FROM t WHERE a = 1" in
  Alcotest.(check int) "one row" 1 (List.length rows);
  Alcotest.check value_testable "value" (Db.V_text "x") (List.hd rows).(0)

let test_multi_col_unique_index () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER, b INTEGER, c TEXT)";
  exec db "CREATE UNIQUE INDEX idx_ab ON t (a, b)";
  exec db "INSERT INTO t VALUES (1, 2, 'x')";
  (* Same a=1 but different b=3 — must NOT violate unique constraint *)
  let r1 = run (Db.execute db "INSERT INTO t VALUES (1, 3, 'y')") in
  (match r1 with
   | Error _ -> Alcotest.fail "should allow (1,3): different b column"
   | Ok () -> ());
  (* Same (a=1, b=2) — MUST violate unique constraint *)
  let r2 = run (Db.execute db "INSERT INTO t VALUES (1, 2, 'z')") in
  (match r2 with
   | Ok () -> Alcotest.fail "should have rejected duplicate (1,2)"
   | Error _ -> ())

(* QCheck: random integer inserts + CREATE INDEX; index lookup must
   match sequential scan for every distinct value. *)
let qcheck_index_lookup_matches_seq_scan =
  QCheck.Test.make
    ~name:"index_lookup: same rows as seq_scan for all values"
    ~count:10_000
    QCheck.(list_size Gen.(0 -- 20) (int_range (-100) 100))
    (fun ns ->
       let db = Lwt_main.run (Db.open_in_memory ()) in
       Lwt_main.run (
         let* _ = Db.execute db "CREATE TABLE t (n INTEGER)" in
         let* () = Lwt_list.iter_s (fun n ->
           let sql = Printf.sprintf "INSERT INTO t (n) VALUES (%d)" n in
           let* _ = Db.execute db sql in
           Lwt.return_unit
         ) ns in
         (* Snapshot results per distinct value, pre-index *)
         let distinct = List.sort_uniq compare ns in
         let* pre_counts = Lwt_list.map_s (fun v ->
           let* r = Db.query db (Printf.sprintf "SELECT * FROM t WHERE n = %d" v) in
           match r with
           | Error _ -> Lwt.return (v, -1)
           | Ok stream ->
             let* rows = Lwt_stream.to_list stream in
             Lwt.return (v, List.length rows)
         ) distinct in
         let* _ = Db.execute db "CREATE INDEX idx_n ON t (n)" in
         let* post_counts = Lwt_list.map_s (fun v ->
           let* r = Db.query db (Printf.sprintf "SELECT * FROM t WHERE n = %d" v) in
           match r with
           | Error _ -> Lwt.return (v, -1)
           | Ok stream ->
             let* rows = Lwt_stream.to_list stream in
             Lwt.return (v, List.length rows)
         ) distinct in
         (* Also check a value that's not present (should give 0 in both) *)
         let absent = 1000 in
         let* r1 = Db.query db (Printf.sprintf "SELECT * FROM t WHERE n = %d" absent) in
         let* absent_post = match r1 with
           | Error _ -> Lwt.return (-1)
           | Ok s -> let* rs = Lwt_stream.to_list s in Lwt.return (List.length rs)
         in
         Lwt.return (pre_counts = post_counts && absent_post = 0)
       ))

(* QCheck: random text inserts + CREATE INDEX on the text column *)
let qcheck_text_index_lookup =
  QCheck.Test.make
    ~name:"text_index_lookup: results match seq_scan for any value"
    ~count:10_000
    QCheck.(list_size Gen.(0 -- 15)
              (string_size ~gen:Gen.(char_range 'a' 'd') Gen.(1 -- 4)))
    (fun ss ->
       let db = Lwt_main.run (Db.open_in_memory ()) in
       Lwt_main.run (
         let* _ = Db.execute db "CREATE TABLE t (s TEXT)" in
         let* () = Lwt_list.iter_s (fun s ->
           (* Escape single quotes by skipping inserts that contain them. *)
           if String.contains s '\'' then Lwt.return_unit
           else
             let sql = Printf.sprintf "INSERT INTO t (s) VALUES ('%s')" s in
             let* _ = Db.execute db sql in
             Lwt.return_unit
         ) ss in
         let distinct =
           List.filter (fun s -> not (String.contains s '\''))
             (List.sort_uniq compare ss)
         in
         let* pre_counts = Lwt_list.map_s (fun v ->
           let* r = Db.query db (Printf.sprintf "SELECT * FROM t WHERE s = '%s'" v) in
           match r with
           | Error _ -> Lwt.return (v, -1)
           | Ok stream ->
             let* rows = Lwt_stream.to_list stream in
             Lwt.return (v, List.length rows)
         ) distinct in
         let* _ = Db.execute db "CREATE INDEX idx_s ON t (s)" in
         let* post_counts = Lwt_list.map_s (fun v ->
           let* r = Db.query db (Printf.sprintf "SELECT * FROM t WHERE s = '%s'" v) in
           match r with
           | Error _ -> Lwt.return (v, -1)
           | Ok stream ->
             let* rows = Lwt_stream.to_list stream in
             Lwt.return (v, List.length rows)
         ) distinct in
         Lwt.return (pre_counts = post_counts)
       ))

(* ------------------------------------------------------------------ *)
(* Group 16: NOT NULL enforcement + DEFAULT constraints (Task 4)        *)
(* ------------------------------------------------------------------ *)

(** INSERT NULL into a NOT NULL column → Not_null_violation. *)
let not_null_insert_null () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER NOT NULL)";
  let result = run (Db.execute db "INSERT INTO t (n) VALUES (NULL)") in
  match result with
  | Error (Db.Sema (Sqlocaml_sql.Sema.Not_null_violation "n")) -> ()
  | Error (Db.Runtime _) -> ()   (* also acceptable: runtime enforcement *)
  | Error e ->
    (match e with
     | Db.Parse msg  -> Alcotest.failf "expected Not_null_violation, got Parse: %s" msg
     | Db.Sema _     -> Alcotest.fail "expected Not_null_violation, got other Sema error"
     | Db.Runtime msg -> Alcotest.failf "expected Not_null_violation, got Runtime: %s" msg)
  | Ok () -> Alcotest.fail "expected Not_null_violation error, got Ok"

(** INSERT omitting a NOT NULL column that has a DEFAULT → uses default. *)
let not_null_default_used_when_omitted () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER NOT NULL DEFAULT 42, s TEXT)";
  exec db "INSERT INTO t (s) VALUES ('x')";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "1 row inserted" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.check value_testable "n=42 (from default)" (Db.V_int 42L) row.(0);
  Alcotest.check value_testable "s=x"                  (Db.V_text "x") row.(1)

(** INSERT omitting a NOT NULL column with DEFAULT 0 → uses 0. *)
let not_null_default_zero () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER NOT NULL DEFAULT 0, s TEXT)";
  exec db "INSERT INTO t (s) VALUES ('hello')";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  Alcotest.check value_testable "n=0 (default)" (Db.V_int 0L) (List.hd rows).(0)

(** UPDATE SET col = NULL on a NOT NULL column → Not_null_violation. *)
let not_null_update_to_null () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER NOT NULL DEFAULT 1)";
  exec db "INSERT INTO t (n) VALUES (5)";
  let result = run (Db.execute db "UPDATE t SET n = NULL") in
  match result with
  | Error (Db.Sema (Sqlocaml_sql.Sema.Not_null_violation "n")) -> ()
  | Error (Db.Runtime _) -> ()   (* also acceptable *)
  | Ok ()   -> Alcotest.fail "expected Not_null_violation on UPDATE, got Ok"
  | Error e ->
    (match e with
     | Db.Parse msg  -> Alcotest.failf "got Parse: %s" msg
     | Db.Sema _     -> Alcotest.fail "got other Sema error"
     | Db.Runtime msg -> Alcotest.failf "got Runtime: %s" msg)

(** Explicit DEFAULT value is returned after SELECT. *)
let default_value_readable () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x INTEGER DEFAULT 99, y TEXT DEFAULT 'hi')";
  exec db "INSERT INTO t (x) VALUES (1)";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.check value_testable "x=1 (explicit)"   (Db.V_int 1L)    row.(0);
  Alcotest.check value_testable "y=hi (default)"   (Db.V_text "hi") row.(1)

(** INSERT omitting a NOT NULL column that has no DEFAULT → error. *)
let not_null_omit_no_default () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, n INTEGER NOT NULL)" in
    let* result = Db.execute db "INSERT INTO t (id) VALUES (1)" in
    (match result with
     | Error _ -> ()
     | Ok () -> Alcotest.fail "expected error when omitting NOT NULL column with no default");
    Db.close db
  )

(** QCheck: for a table with n INTEGER NOT NULL DEFAULT 99, inserting without
    specifying n always produces n=99.  10,000 trials. *)
let qcheck_default_applied =
  QCheck.Test.make
    ~name:"default_applied: omitted NOT NULL DEFAULT 99 col always returns 99"
    ~count:10_000
    QCheck.(string_size ~gen:Gen.(char_range 'a' 'z') Gen.(1 -- 8))
    (fun s ->
       if String.contains s '\'' then true  (* skip strings with quotes *)
       else
         let db = Lwt_main.run (Db.open_in_memory ()) in
         Lwt_main.run (
           let* _ = Db.execute db
               "CREATE TABLE t (n INTEGER NOT NULL DEFAULT 99, s TEXT)" in
           let sql = Printf.sprintf "INSERT INTO t (s) VALUES ('%s')" s in
           let* res = Db.execute db sql in
           match res with
           | Error _ -> Lwt.return false
           | Ok () ->
             let* qres = Db.query db "SELECT * FROM t" in
             match qres with
             | Error _ -> Lwt.return false
             | Ok stream ->
               let* rows = Lwt_stream.to_list stream in
               match rows with
               | [row] ->
                 (match row.(0) with
                  | Db.V_int 99L -> Lwt.return true
                  | _             -> Lwt.return false)
               | _ -> Lwt.return false))

(* ------------------------------------------------------------------ *)
(* Group 17: DROP TABLE and DROP INDEX (Task 7)                         *)
(* ------------------------------------------------------------------ *)

(** Helper: expect a Sema(Unknown_table) error. *)
let expect_unknown_table label result =
  match err_or_fail label result with
  | Db.Sema (Sqlocaml_sql.Sema.Unknown_table _) -> ()
  | _ -> Alcotest.failf "%s: expected Sema(Unknown_table)" label

(** Helper: expect a Sema(Unknown_index) error. *)
let expect_unknown_index label result =
  match err_or_fail label result with
  | Db.Sema (Sqlocaml_sql.Sema.Unknown_index _) -> ()
  | _ -> Alcotest.failf "%s: expected Sema(Unknown_index)" label

(** DROP TABLE then SELECT returns Unknown_table. *)
let drop_table_then_select () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "DROP TABLE t";
  let result = run (Db.query db "SELECT * FROM t") in
  expect_unknown_table "drop_table_then_select" result

(** DROP TABLE on a non-existent table → Unknown_table. *)
let drop_table_nonexistent () =
  let db = fresh_db () in
  let result = run (Db.execute db "DROP TABLE ghost") in
  expect_unknown_table "drop_table_nonexistent" result

(** DROP TABLE removes all rows — old data not visible after recreate. *)
let drop_table_data_gone () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (42)";
  exec db "DROP TABLE t";
  exec db "CREATE TABLE t (n INTEGER)";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "no old rows after DROP+CREATE" 0 (List.length rows)

(** DROP TABLE with associated index — indexes gone from catalog; subsequent
    SELECT on recreated table works (no crash). *)
let drop_table_with_index () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, name TEXT)";
  exec db "INSERT INTO t (id, name) VALUES (1, 'alice')";
  exec db "CREATE INDEX idx ON t (id)";
  exec db "DROP TABLE t";
  (* Table is gone — index must be gone too *)
  let result = run (Db.query db "SELECT * FROM t") in
  expect_unknown_table "drop_table_with_index: table gone" result;
  (* DROP INDEX on a now-deleted index should return Unknown_index *)
  let result2 = run (Db.execute db "DROP INDEX idx") in
  expect_unknown_index "drop_table_with_index: index gone" result2

(** DROP TABLE then re-CREATE with same name succeeds and is empty. *)
let drop_table_then_recreate () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x INTEGER)";
  exec db "INSERT INTO t (x) VALUES (100)";
  exec db "DROP TABLE t";
  exec db "CREATE TABLE t (x INTEGER)";
  exec db "INSERT INTO t (x) VALUES (200)";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "1 row in recreated table" 1 (List.length rows);
  Alcotest.check value_testable "new row value" (Db.V_int 200L) (List.hd rows).(0)

(** DROP INDEX then SELECT falls back to seq scan (no crash). *)
let drop_index_then_select () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, name TEXT)";
  exec db "INSERT INTO t (id, name) VALUES (1, 'alice')";
  exec db "INSERT INTO t (id, name) VALUES (2, 'bob')";
  exec db "CREATE INDEX idx ON t (id)";
  exec db "DROP INDEX idx";
  (* Query still returns correct rows via seq scan *)
  let rows = query_ok db "SELECT name FROM t WHERE id = 2" in
  Alcotest.(check int) "1 row via seq scan after DROP INDEX" 1 (List.length rows);
  Alcotest.check value_testable "name=bob" (Db.V_text "bob") (List.hd rows).(0)

(** DROP INDEX on non-existent index → Unknown_index. *)
let drop_index_nonexistent () =
  let db = fresh_db () in
  let result = run (Db.execute db "DROP INDEX no_such_idx") in
  expect_unknown_index "drop_index_nonexistent" result

(** DROP INDEX then re-CREATE with same name succeeds. *)
let drop_index_then_recreate () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER)";
  exec db "INSERT INTO t (id) VALUES (1)";
  exec db "CREATE INDEX idx ON t (id)";
  exec db "DROP INDEX idx";
  exec db "CREATE INDEX idx ON t (id)";
  let rows = query_ok db "SELECT * FROM t WHERE id = 1" in
  Alcotest.(check int) "1 row via recreated index" 1 (List.length rows)

(** DROP the very first index ever created (id=0, first entry in _sys_indexes)
    and verify re-creation succeeds, proving the on-disk record was deleted. *)
let drop_first_index_then_recreate () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER)";
  (* This is the first and only index created — it receives id=0,
     stored at key varint(0) = the first entry in _sys_indexes. *)
  exec db "CREATE INDEX idx ON t (id)";
  exec db "DROP INDEX idx";
  (* Re-creation must succeed: if the on-disk record was not deleted,
     a re-open would see a ghost entry and fail to insert again. *)
  exec db "CREATE INDEX idx ON t (id)";
  let rows = query_ok db "SELECT * FROM t WHERE id = 1" in
  Alcotest.(check int) "0 rows (recreated index, empty table)" 0 (List.length rows)

(** After DROP TABLE and re-CREATE, only new rows are visible. *)
let drop_table_recreate_old_data_invisible () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, val TEXT)";
  exec db "INSERT INTO t (id, val) VALUES (1, 'old1')";
  exec db "INSERT INTO t (id, val) VALUES (2, 'old2')";
  exec db "DROP TABLE t";
  exec db "CREATE TABLE t (id INTEGER, val TEXT)";
  exec db "INSERT INTO t (id, val) VALUES (10, 'new10')";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "only 1 new row visible" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.check value_testable "id=10"     (Db.V_int 10L)     row.(0);
  Alcotest.check value_testable "val=new10" (Db.V_text "new10") row.(1)

(* ------------------------------------------------------------------ *)
(* Group 17: Gap-fill — Db.open_file error path, REAL DEFAULT roundtrip *)
(* ------------------------------------------------------------------ *)

(** Db.open_file with a path that points to an existing directory triggers
    an OS-level error (e.g., EISDIR) — covers db.ml lines 34-36 and
    store.ml line 219. *)
let open_file_invalid_path () =
  run (
    (* "/" is always a directory; openfile with O_RDWR on it fails. *)
    let* result = Db.open_file ~path:"/" in
    (match result with
     | Ok _ -> Alcotest.fail "expected Error opening '/'"
     | Error (Db.Runtime _) -> ()
     | Error _ -> Alcotest.fail "expected Runtime error variant");
    Lwt.return_unit
  )

(** Tempfile helper that survives a single test. *)
let with_tempfile f =
  let path = Filename.temp_file "sqlocaml_gap_" ".db" in
  (try Unix.unlink path with Unix.Unix_error _ -> ());
  let result = f path in
  (try Unix.unlink path with Unix.Unix_error _ -> ());
  result

(** REAL DEFAULT round-trips through close+reopen, exercising
    encode/decode_default_value DV_real (catalog.ml lines 98-103 / 124-130)
    and the Some-default decode branch (catalog.ml line 174). *)
let default_real_persists () =
  with_tempfile (fun path ->
    run (
      let* db_res = Db.open_file ~path in
      let db = match db_res with
        | Ok d -> d
        | Error _ -> Alcotest.fail "open_file failed"
      in
      let* _ = Db.execute db "CREATE TABLE t (n INTEGER, f REAL DEFAULT 2.5)" in
      let* () = Db.close db in
      (* Reopen and check schema *)
      let* db_res2 = Db.open_file ~path in
      let db2 = match db_res2 with
        | Ok d -> d
        | Error _ -> Alcotest.fail "reopen failed"
      in
      let* _ = Db.execute db2 "INSERT INTO t (n) VALUES (1)" in
      let* rq = Db.query db2 "SELECT * FROM t" in
      let* rows = match rq with
        | Ok stream -> Lwt_stream.to_list stream
        | Error _ -> Alcotest.fail "query failed after reopen"
      in
      Alcotest.(check int) "1 row" 1 (List.length rows);
      let r = List.hd rows in
      Alcotest.check value_testable "n=1" (Db.V_int 1L) r.(0);
      Alcotest.check value_testable "f=2.5 (default)" (Db.V_real 2.5) r.(1);
      Db.close db2
    )
  )

(** TEXT DEFAULT round-trips through close+reopen.
    Exercises catalog DV_text encode/decode_default_value branches. *)
let default_text_persists () =
  with_tempfile (fun path ->
    run (
      let* db_res = Db.open_file ~path in
      let db = match db_res with
        | Ok d -> d
        | Error _ -> Alcotest.fail "open_file failed"
      in
      let* _ = Db.execute db "CREATE TABLE t (n INTEGER, s TEXT DEFAULT 'hello')" in
      let* () = Db.close db in
      let* db_res2 = Db.open_file ~path in
      let db2 = match db_res2 with
        | Ok d -> d
        | Error _ -> Alcotest.fail "reopen failed"
      in
      let* _ = Db.execute db2 "INSERT INTO t (n) VALUES (1)" in
      let* rq = Db.query db2 "SELECT * FROM t" in
      let* rows = match rq with
        | Ok stream -> Lwt_stream.to_list stream
        | Error _ -> Alcotest.fail "query failed after reopen"
      in
      Alcotest.(check int) "1 row" 1 (List.length rows);
      let r = List.hd rows in
      Alcotest.check value_testable "s=hello (default)" (Db.V_text "hello") r.(1);
      Db.close db2
    )
  )

(** INTEGER DEFAULT round-trips through close+reopen. Exercises DV_int
    encode/decode branches in catalog.ml. *)
let default_int_persists () =
  with_tempfile (fun path ->
    run (
      let* db_res = Db.open_file ~path in
      let db = match db_res with
        | Ok d -> d
        | Error _ -> Alcotest.fail "open_file failed"
      in
      let* _ = Db.execute db "CREATE TABLE t (n INTEGER, k INTEGER DEFAULT 42)" in
      let* () = Db.close db in
      let* db_res2 = Db.open_file ~path in
      let db2 = match db_res2 with
        | Ok d -> d
        | Error _ -> Alcotest.fail "reopen failed"
      in
      let* _ = Db.execute db2 "INSERT INTO t (n) VALUES (1)" in
      let* rq = Db.query db2 "SELECT * FROM t" in
      let* rows = match rq with
        | Ok stream -> Lwt_stream.to_list stream
        | Error _ -> Alcotest.fail "query failed after reopen"
      in
      Alcotest.(check int) "1 row" 1 (List.length rows);
      let r = List.hd rows in
      Alcotest.check value_testable "k=42 (default)" (Db.V_int 42L) r.(1);
      Db.close db2
    )
  )

(* ------------------------------------------------------------------ *)
(* Group: BETWEEN and IN operators                                      *)
(* ------------------------------------------------------------------ *)

let test_between_query () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (5)";
  exec db "INSERT INTO t VALUES (10)";
  let rows = query_ok db "SELECT n FROM t WHERE n BETWEEN 3 AND 7" in
  let vals = List.map (fun r -> match r.(0) with
    | Db.V_int n -> n | _ -> -1L) rows in
  Alcotest.(check (list int64)) "between" [5L] vals

let test_not_between_query () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (5)";
  exec db "INSERT INTO t VALUES (10)";
  let rows = query_ok db "SELECT n FROM t WHERE n NOT BETWEEN 3 AND 7" in
  let vals = List.sort Int64.compare (List.map (fun r -> match r.(0) with
    | Db.V_int n -> n | _ -> -1L) rows) in
  Alcotest.(check (list int64)) "not between" [1L; 10L] vals

let test_in_query () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (2)";
  exec db "INSERT INTO t VALUES (3)";
  let rows = query_ok db "SELECT n FROM t WHERE n IN (1, 3)" in
  let vals = List.sort Int64.compare (List.map (fun r -> match r.(0) with
    | Db.V_int n -> n | _ -> -1L) rows) in
  Alcotest.(check (list int64)) "in" [1L; 3L] vals

let test_not_in_query () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (2)";
  exec db "INSERT INTO t VALUES (3)";
  let rows = query_ok db "SELECT n FROM t WHERE n NOT IN (1, 3)" in
  let vals = List.map (fun r -> match r.(0) with
    | Db.V_int n -> n | _ -> -1L) rows in
  Alcotest.(check (list int64)) "not in" [2L] vals

(* ------------------------------------------------------------------ *)
(* PRAGMA tests                                                         *)
(* ------------------------------------------------------------------ *)

module D = Db

let test_pragma_table_info () =
  run (
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE TABLE t (id INTEGER NOT NULL PRIMARY KEY, name TEXT)" in
    let* r = D.query db "PRAGMA table_info(t)" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
    Alcotest.(check int) "pragma rows" 2 (List.length rows);
    let col0 = List.nth rows 0 in
    Alcotest.(check string) "col0 name" "id"      (match col0.(1) with D.V_text s -> s | _ -> "?");
    Alcotest.(check string) "col0 type" "INTEGER"  (match col0.(2) with D.V_text s -> s | _ -> "?");
    Alcotest.(check int64)  "col0 nn"   1L         (match col0.(3) with D.V_int n -> n | _ -> -1L);
    Alcotest.(check int64)  "col0 pk"   1L         (match col0.(5) with D.V_int n -> n | _ -> -1L);
    Lwt.return_unit)

let test_pragma_index_list () =
  run (
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE TABLE t (id INTEGER, name TEXT)" in
    let* _ = D.execute db "CREATE INDEX idx_name ON t (name)" in
    let* r = D.query db "PRAGMA index_list(t)" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
    Alcotest.(check int) "idx rows" 1 (List.length rows);
    let row = List.hd rows in
    Alcotest.(check string) "idx name" "idx_name" (match row.(1) with D.V_text s -> s | _ -> "?");
    Alcotest.(check int64)  "idx uniq" 0L         (match row.(2) with D.V_int n -> n | _ -> -1L);
    Lwt.return_unit)

(* ------------------------------------------------------------------ *)
(* SELECT DISTINCT                                                      *)
(* ------------------------------------------------------------------ *)

let test_distinct () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x INTEGER, y TEXT)";
  exec db "INSERT INTO t VALUES (1, 'a')";
  exec db "INSERT INTO t VALUES (1, 'b')";
  exec db "INSERT INTO t VALUES (2, 'a')";
  exec db "INSERT INTO t VALUES (1, 'a')";
  let rows = query_ok db "SELECT DISTINCT x FROM t ORDER BY x" in
  Alcotest.(check int) "2 distinct x rows" 2 (List.length rows);
  let xs = List.map (fun r -> r.(0)) rows in
  Alcotest.check (Alcotest.list value_testable) "distinct x values"
    [Db.V_int 1L; Db.V_int 2L] xs

let test_distinct_multicolumn () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x INTEGER, y TEXT)";
  exec db "INSERT INTO t VALUES (1, 'a')";
  exec db "INSERT INTO t VALUES (1, 'a')";
  exec db "INSERT INTO t VALUES (1, 'b')";
  exec db "INSERT INTO t VALUES (2, 'a')";
  (* Use ORDER BY x only (multi-key ORDER BY is not yet supported). *)
  let rows = query_ok db "SELECT DISTINCT x, y FROM t ORDER BY x" in
  Alcotest.(check int) "3 distinct (x,y) rows" 3 (List.length rows)

let test_distinct_null () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x INTEGER)";
  exec db "INSERT INTO t VALUES (NULL)";
  exec db "INSERT INTO t VALUES (NULL)";
  exec db "INSERT INTO t VALUES (1)";
  let rows = query_ok db "SELECT DISTINCT x FROM t ORDER BY x" in
  Alcotest.(check int) "2 distinct x rows (with null)" 2 (List.length rows);
  let xs = List.map (fun r -> r.(0)) rows in
  Alcotest.check (Alcotest.list value_testable) "distinct x with null"
    [Db.V_null; Db.V_int 1L] xs

(* ------------------------------------------------------------------ *)
(* UNION / INTERSECT / EXCEPT                                           *)
(* ------------------------------------------------------------------ *)

let sort_rows rows =
  List.sort (fun a b ->
    let cmp_val x y = match x, y with
      | Db.V_int  x, Db.V_int  y -> Int64.compare x y
      | Db.V_text x, Db.V_text y -> String.compare x y
      | Db.V_null,   Db.V_null   -> 0
      | Db.V_null,   _           -> -1
      | _,           Db.V_null   -> 1
      | Db.V_real x, Db.V_real y -> Float.compare x y
      | _,           _           -> 0
    in
    let len = min (Array.length a) (Array.length b) in
    let rec go i =
      if i >= len then 0
      else let c = cmp_val a.(i) b.(i) in
           if c <> 0 then c else go (i+1)
    in
    go 0
  ) rows

let test_union () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (x INTEGER)";
  exec db "CREATE TABLE b (x INTEGER)";
  exec db "INSERT INTO a VALUES (1)";
  exec db "INSERT INTO a VALUES (2)";
  exec db "INSERT INTO b VALUES (2)";
  exec db "INSERT INTO b VALUES (3)";
  let rows = sort_rows (query_ok db "SELECT x FROM a UNION SELECT x FROM b") in
  Alcotest.(check int) "union deduplicates: 3 rows" 3 (List.length rows);
  let xs = List.map (fun r -> r.(0)) rows in
  Alcotest.check (Alcotest.list value_testable) "union values"
    [Db.V_int 1L; Db.V_int 2L; Db.V_int 3L] xs

let test_union_all () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (x INTEGER)";
  exec db "CREATE TABLE b (x INTEGER)";
  exec db "INSERT INTO a VALUES (1)";
  exec db "INSERT INTO a VALUES (2)";
  exec db "INSERT INTO b VALUES (2)";
  exec db "INSERT INTO b VALUES (3)";
  let rows = sort_rows (query_ok db "SELECT x FROM a UNION ALL SELECT x FROM b") in
  Alcotest.(check int) "union all keeps duplicates: 4 rows" 4 (List.length rows);
  let xs = List.map (fun r -> r.(0)) rows in
  Alcotest.check (Alcotest.list value_testable) "union all values"
    [Db.V_int 1L; Db.V_int 2L; Db.V_int 2L; Db.V_int 3L] xs

let test_intersect () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (x INTEGER)";
  exec db "CREATE TABLE b (x INTEGER)";
  exec db "INSERT INTO a VALUES (1)";
  exec db "INSERT INTO a VALUES (2)";
  exec db "INSERT INTO a VALUES (2)";
  exec db "INSERT INTO b VALUES (2)";
  exec db "INSERT INTO b VALUES (3)";
  let rows = sort_rows (query_ok db "SELECT x FROM a INTERSECT SELECT x FROM b") in
  Alcotest.(check int) "intersect: 1 row" 1 (List.length rows);
  let xs = List.map (fun r -> r.(0)) rows in
  Alcotest.check (Alcotest.list value_testable) "intersect values"
    [Db.V_int 2L] xs

let test_except () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (x INTEGER)";
  exec db "CREATE TABLE b (x INTEGER)";
  exec db "INSERT INTO a VALUES (1)";
  exec db "INSERT INTO a VALUES (2)";
  exec db "INSERT INTO a VALUES (2)";
  exec db "INSERT INTO b VALUES (2)";
  let rows = sort_rows (query_ok db "SELECT x FROM a EXCEPT SELECT x FROM b") in
  Alcotest.(check int) "except: 1 row" 1 (List.length rows);
  let xs = List.map (fun r -> r.(0)) rows in
  Alcotest.check (Alcotest.list value_testable) "except values"
    [Db.V_int 1L] xs

(* ------------------------------------------------------------------ *)
(* Named and indexed parameters                                         *)
(* ------------------------------------------------------------------ *)

let test_indexed_params () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (x INTEGER, y INTEGER)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (10, 20)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (30, 40)" in
    (* ?1 reuses slot 0 twice — both conditions use the same value *)
    let* stmt_r = Db.prepare db "SELECT x FROM t WHERE x = ?1 OR y = ?1" in
    (match stmt_r with
     | Error e -> Alcotest.failf "prepare: %s" (Format.asprintf "%a" Db.pp_error e)
     | Ok st ->
       let* r = Db.iter st ~params:[Db.V_int 10L] in
       (match r with
        | Error e -> Alcotest.failf "iter: %s" (Format.asprintf "%a" Db.pp_error e)
        | Ok stream ->
          let* rows = Lwt_stream.to_list stream in
          Alcotest.(check int) "indexed param rows" 1 (List.length rows);
          let* () = Db.finalize st in
          Lwt.return_unit)))

let test_named_params_colon () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (x INTEGER, y TEXT)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'hello')" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 'world')" in
    let* stmt_r = Db.prepare db "SELECT y FROM t WHERE x = :id" in
    (match stmt_r with
     | Error e -> Alcotest.failf "prepare: %s" (Format.asprintf "%a" Db.pp_error e)
     | Ok st ->
       let params = Array.to_list (Db.params_of_named st ["id", Db.V_int 1L]) in
       let* r = Db.iter st ~params in
       (match r with
        | Error e -> Alcotest.failf "iter: %s" (Format.asprintf "%a" Db.pp_error e)
        | Ok stream ->
          let* rows = Lwt_stream.to_list stream in
          Alcotest.(check int) "named colon rows" 1 (List.length rows);
          (match rows with
           | [| Db.V_text s |] :: _ ->
             Alcotest.(check string) "named colon value" "hello" s
           | _ -> Alcotest.fail "unexpected rows");
          let* () = Db.finalize st in
          Lwt.return_unit)))

(* ------------------------------------------------------------------ *)
(* Date/time functions                                                  *)
(* ------------------------------------------------------------------ *)

(** Helper: create a single-row "dummy" table for SELECT-expression tests.
    The parser requires FROM <table>, so we use a 1-row table as a probe. *)
let dummy_db () =
  let db = fresh_db () in
  exec db "CREATE TABLE _d (n INTEGER)";
  exec db "INSERT INTO _d (n) VALUES (1)";
  db

let test_date_fn () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT DATE('2024-01-15') FROM _d" in
  (match rows with
   | [[| Db.V_text d |]] ->
     Alcotest.(check string) "date fn" "2024-01-15" d
   | _ -> Alcotest.fail "expected one row with text")

let test_time_fn () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT TIME('12:30:45') FROM _d" in
  (match rows with
   | [[| Db.V_text t |]] ->
     Alcotest.(check string) "time fn" "12:30:45" t
   | _ -> Alcotest.fail "expected one row with text")

let test_datetime_fn () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT DATETIME('2024-01-15 12:30:45') FROM _d" in
  (match rows with
   | [[| Db.V_text dt |]] ->
     Alcotest.(check string) "datetime fn" "2024-01-15 12:30:45" dt
   | _ -> Alcotest.fail "expected one row with text")

let test_julianday_fn () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT JULIANDAY('2000-01-01') FROM _d" in
  (match rows with
   | [[| Db.V_real jd |]] ->
     Alcotest.(check bool) "julianday 2000-01-01"
       true (abs_float (jd -. 2451544.5) < 0.001)
   | _ -> Alcotest.fail "expected one row with real")

let test_unixepoch_fn () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT UNIXEPOCH('1970-01-01') FROM _d" in
  (match rows with
   | [[| Db.V_int n |]] ->
     Alcotest.(check int64) "unixepoch 1970" 0L n
   | _ -> Alcotest.fail "expected one row with int")

let test_strftime_fn () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT STRFTIME('%Y-%m-%d', '2024-06-15') FROM _d" in
  (match rows with
   | [[| Db.V_text s |]] ->
     Alcotest.(check string) "strftime" "2024-06-15" s
   | _ -> Alcotest.fail "expected one row with text")

let test_date_now () =
  Lwt_main.run (
    (* 2024-01-15 00:00:00 UTC as unix timestamp *)
    let fixed_ts = 1705276800.0 in
    let* db = Db.open_in_memory ~clock:(fun () -> fixed_ts) () in
    let* _ = Db.execute db "CREATE TABLE _d (n INTEGER)" in
    let* _ = Db.execute db "INSERT INTO _d (n) VALUES (1)" in
    let* result = Db.query db "SELECT DATE('now') FROM _d" in
    (match result with
     | Error e -> Alcotest.failf "%a" Db.pp_error e
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       (match rows with
        | [[| Db.V_text d |]] ->
          Alcotest.(check string) "date now" "2024-01-15" d
        | _ -> Alcotest.fail "expected date string");
       Lwt.return_unit))

(* ------------------------------------------------------------------ *)
(* Phase 7: multi-key ORDER BY                                          *)
(* ------------------------------------------------------------------ *)

(** First key equal, second key (ASC) breaks the tie. *)
let test_multikey_order_by () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x INTEGER, y INTEGER)";
  exec db "INSERT INTO t VALUES (1, 3)";
  exec db "INSERT INTO t VALUES (1, 1)";
  exec db "INSERT INTO t VALUES (2, 2)";
  exec db "INSERT INTO t VALUES (1, 2)";
  let rows = query_ok db "SELECT x, y FROM t ORDER BY x ASC, y ASC" in
  Alcotest.(check int) "multikey: 4 rows" 4 (List.length rows);
  let pairs = List.map (fun r -> r.(0), r.(1)) rows in
  Alcotest.check (Alcotest.list (Alcotest.pair value_testable value_testable))
    "multikey order asc asc"
    [ Db.V_int 1L, Db.V_int 1L
    ; Db.V_int 1L, Db.V_int 2L
    ; Db.V_int 1L, Db.V_int 3L
    ; Db.V_int 2L, Db.V_int 2L ] pairs

(** First key equal, second key DESC breaks the tie. *)
let test_multikey_order_by_mixed () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x INTEGER, y INTEGER)";
  exec db "INSERT INTO t VALUES (1, 1)";
  exec db "INSERT INTO t VALUES (1, 3)";
  exec db "INSERT INTO t VALUES (1, 2)";
  let rows = query_ok db "SELECT x, y FROM t ORDER BY x ASC, y DESC" in
  Alcotest.(check int) "multikey mixed: 3 rows" 3 (List.length rows);
  let ys = List.map (fun r -> r.(1)) rows in
  Alcotest.check (Alcotest.list value_testable) "multikey asc desc"
    [Db.V_int 3L; Db.V_int 2L; Db.V_int 1L] ys

(* ------------------------------------------------------------------ *)
(* Phase 7: DISTINCT with V_real and V_blob (row_key coverage)          *)
(* ------------------------------------------------------------------ *)

(** DISTINCT on REAL column — exercises V_real arm in row_key. *)
let test_distinct_real () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x REAL)";
  exec db "INSERT INTO t VALUES (1.5)";
  exec db "INSERT INTO t VALUES (1.5)";
  exec db "INSERT INTO t VALUES (2.5)";
  let rows = query_ok db "SELECT DISTINCT x FROM t ORDER BY x" in
  Alcotest.(check int) "distinct real: 2 rows" 2 (List.length rows);
  let xs = List.map (fun r -> r.(0)) rows in
  Alcotest.check (Alcotest.list value_testable) "distinct real values"
    [Db.V_real 1.5; Db.V_real 2.5] xs

(* ------------------------------------------------------------------ *)
(* Phase 7: INTERSECT / EXCEPT edge cases                               *)
(* ------------------------------------------------------------------ *)

(** INTERSECT where right side is empty → empty result. *)
let test_intersect_empty_right () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (x INTEGER)";
  exec db "CREATE TABLE b (x INTEGER)";
  exec db "INSERT INTO a VALUES (1)";
  exec db "INSERT INTO a VALUES (2)";
  (* b is empty *)
  let rows = query_ok db "SELECT x FROM a INTERSECT SELECT x FROM b" in
  Alcotest.(check int) "intersect empty right: 0 rows" 0 (List.length rows)

(** EXCEPT deduplication: left has duplicate 1s not in right → only one appears. *)
let test_except_dedup_left () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (x INTEGER)";
  exec db "CREATE TABLE b (x INTEGER)";
  exec db "INSERT INTO a VALUES (1)";
  exec db "INSERT INTO a VALUES (1)";
  exec db "INSERT INTO a VALUES (2)";
  exec db "INSERT INTO b VALUES (3)";
  (* EXCEPT: both 1s from a are NOT in b, but DISTINCT → only one 1; 2 is also kept *)
  let rows = sort_rows (query_ok db "SELECT x FROM a EXCEPT SELECT x FROM b") in
  Alcotest.(check int) "except dedup: 2 rows" 2 (List.length rows);
  let xs = List.map (fun r -> r.(0)) rows in
  Alcotest.check (Alcotest.list value_testable) "except dedup values"
    [Db.V_int 1L; Db.V_int 2L] xs

(** EXCEPT where right side is empty → all left rows retained. *)
let test_except_empty_right () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (x INTEGER)";
  exec db "CREATE TABLE b (x INTEGER)";
  exec db "INSERT INTO a VALUES (1)";
  exec db "INSERT INTO a VALUES (2)";
  (* b is empty: all a rows should appear *)
  let rows = sort_rows (query_ok db "SELECT x FROM a EXCEPT SELECT x FROM b") in
  Alcotest.(check int) "except empty right: 2 rows" 2 (List.length rows);
  let xs = List.map (fun r -> r.(0)) rows in
  Alcotest.check (Alcotest.list value_testable) "except empty right values"
    [Db.V_int 1L; Db.V_int 2L] xs

(* ------------------------------------------------------------------ *)
(* Phase 7: datetime eval_func error / fallback branches                *)
(* ------------------------------------------------------------------ *)

(** DATE with invalid time string → NULL (Error _ branch). *)
let test_date_invalid_input () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT DATE('not-a-date') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for invalid date string")

(** DATE with a non-text arg (integer literal) → NULL (_ -> V_null branch). *)
let test_date_nontext_arg () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT DATE(42) FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for integer DATE arg")

(** DATE with a modifier (rest <> [] branch) → NULL. *)
let test_date_with_modifier () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT DATE('2024-01-15', '+1 day') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL when modifier present")

(** TIME with invalid string → NULL (Error _ branch). *)
let test_time_invalid_input () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT TIME('bad-time') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for invalid time string")

(** TIME with non-text arg → NULL. *)
let test_time_nontext_arg () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT TIME(0) FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for integer TIME arg")

(** TIME with modifier → NULL. *)
let test_time_with_modifier () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT TIME('12:00:00', '+1 hour') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL when TIME modifier present")

(** DATETIME with invalid string → NULL (Error _ branch). *)
let test_datetime_invalid_input () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT DATETIME('bogus') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for invalid datetime string")

(** DATETIME with non-text arg → NULL. *)
let test_datetime_nontext_arg () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT DATETIME(1) FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for integer DATETIME arg")

(** DATETIME with modifier → NULL. *)
let test_datetime_with_modifier () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT DATETIME('2024-01-15', '+1 day') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL when DATETIME modifier present")

(** JULIANDAY with invalid string → NULL (Error _ branch). *)
let test_julianday_invalid () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT JULIANDAY('bad') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for invalid julianday string")

(** JULIANDAY with non-text arg → NULL. *)
let test_julianday_nontext_arg () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT JULIANDAY(42) FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for integer JULIANDAY arg")

(** JULIANDAY with modifier → NULL. *)
let test_julianday_with_modifier () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT JULIANDAY('2000-01-01', 'start of month') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL when JULIANDAY modifier present")

(** UNIXEPOCH with invalid string → NULL (Error _ branch). *)
let test_unixepoch_invalid () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT UNIXEPOCH('bad') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for invalid unixepoch string")

(** UNIXEPOCH with non-text arg → NULL. *)
let test_unixepoch_nontext_arg () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT UNIXEPOCH(0) FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for integer UNIXEPOCH arg")

(** UNIXEPOCH with modifier → NULL. *)
let test_unixepoch_with_modifier () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT UNIXEPOCH('1970-01-01', '+1 day') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL when UNIXEPOCH modifier present")

(** STRFTIME where fmt is not text (NULL) → NULL (_ -> V_null fallthrough). *)
let test_strftime_null_fmt () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT STRFTIME(NULL, '2024-01-15') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL when STRFTIME fmt is NULL")

(** STRFTIME with invalid datetime string → NULL (Error _ branch). *)
let test_strftime_invalid_ts () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT STRFTIME('%Y', 'bad-date') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL when STRFTIME ts is invalid")

(** STRFTIME with modifier → NULL (rest <> [] branch). *)
let test_strftime_with_modifier () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT STRFTIME('%Y', '2024-01-15', '+1 day') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL when STRFTIME has modifier")

(* ------------------------------------------------------------------ *)
(* Phase 7: datetime NULL-arg branches ([V_null] and V_null :: _)       *)
(* ------------------------------------------------------------------ *)

(** DATE(NULL) → NULL via the [V_null] single-arg branch. *)
let test_date_null_arg () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT DATE(NULL) FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for DATE(NULL)")

(** DATE(NULL, 'x') → NULL via the V_null :: _ branch. *)
let test_date_null_with_rest () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT DATE(NULL, '+1 day') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for DATE(NULL, modifier)")

(** TIME(NULL) → NULL via the [V_null] single-arg branch. *)
let test_time_null_arg () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT TIME(NULL) FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for TIME(NULL)")

(** TIME(NULL, 'x') → NULL via the V_null :: _ branch. *)
let test_time_null_with_rest () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT TIME(NULL, '+1 hour') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for TIME(NULL, modifier)")

(** DATETIME(NULL) → NULL via the [V_null] single-arg branch. *)
let test_datetime_null_arg () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT DATETIME(NULL) FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for DATETIME(NULL)")

(** DATETIME(NULL, 'x') → NULL via the V_null :: _ branch. *)
let test_datetime_null_with_rest () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT DATETIME(NULL, '+1 day') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for DATETIME(NULL, modifier)")

(** JULIANDAY(NULL) → NULL via the [V_null] single-arg branch. *)
let test_julianday_null_arg () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT JULIANDAY(NULL) FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for JULIANDAY(NULL)")

(** JULIANDAY(NULL, 'x') → NULL via the V_null :: _ branch. *)
let test_julianday_null_with_rest () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT JULIANDAY(NULL, 'start of month') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for JULIANDAY(NULL, modifier)")

(** UNIXEPOCH(NULL) → NULL via the [V_null] single-arg branch. *)
let test_unixepoch_null_arg () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT UNIXEPOCH(NULL) FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for UNIXEPOCH(NULL)")

(** UNIXEPOCH(NULL, 'x') → NULL via the V_null :: _ branch. *)
let test_unixepoch_null_with_rest () =
  let db = dummy_db () in
  let rows = query_ok db "SELECT UNIXEPOCH(NULL, '+1 day') FROM _d" in
  (match rows with
   | [[| Db.V_null |]] -> ()
   | _ -> Alcotest.fail "expected NULL for UNIXEPOCH(NULL, modifier)")

(* ------------------------------------------------------------------ *)
(* Phase 7: row_key V_blob coverage via DISTINCT on BLOB column         *)
(* ------------------------------------------------------------------ *)

(** DISTINCT on BLOB column with NULL values — exercises V_blob and V_null
    arms in row_key via two NULL blobs (deduplicated to one). *)
let test_distinct_blob_null () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (x BLOB)";
  exec db "INSERT INTO t VALUES (NULL)";
  exec db "INSERT INTO t VALUES (NULL)";
  let rows = query_ok db "SELECT DISTINCT x FROM t" in
  Alcotest.(check int) "distinct blob null: 1 row" 1 (List.length rows)

(* ------------------------------------------------------------------ *)
(* Group: ON CONFLICT (INSERT OR REPLACE / INSERT OR IGNORE)           *)
(* ------------------------------------------------------------------ *)

let test_insert_or_ignore () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, v TEXT)" in
    let* _ = Db.execute db "CREATE UNIQUE INDEX idx_id ON t (id)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'first')" in
    let* _ = Db.execute db "INSERT OR IGNORE INTO t VALUES (1, 'second')" in
    let* r = Db.query db "SELECT v FROM t WHERE id = 1" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "row count" 1 (List.length rows);
       Alcotest.(check string) "value unchanged" "first"
         (match rows with [[| Db.V_text s |]] -> s | _ -> "WRONG");
       Lwt.return_unit))

let test_insert_or_replace () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, v TEXT)" in
    let* _ = Db.execute db "CREATE UNIQUE INDEX idx_id ON t (id)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'first')" in
    let* _ = Db.execute db "INSERT OR REPLACE INTO t VALUES (1, 'second')" in
    let* r = Db.query db "SELECT v FROM t WHERE id = 1" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "row count" 1 (List.length rows);
       Alcotest.(check string) "value replaced" "second"
         (match rows with [[| Db.V_text s |]] -> s | _ -> "WRONG");
       Lwt.return_unit))

let test_insert_or_ignore_unique () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, name TEXT)" in
    let* _ = Db.execute db "CREATE UNIQUE INDEX idx_name ON t (name)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'alice')" in
    let* _ = Db.execute db "INSERT OR IGNORE INTO t VALUES (2, 'alice')" in
    let* r = Db.query db "SELECT COUNT(*) FROM t" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "only original row" 1
         (match rows with [[| Db.V_int n |]] -> Int64.to_int n | _ -> -1);
       Lwt.return_unit))

let test_insert_or_replace_no_conflict () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)" in
    let* _ = Db.execute db "INSERT OR REPLACE INTO t VALUES (99, 'new')" in
    let* r = Db.query db "SELECT v FROM t WHERE id = 99" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "row inserted" 1 (List.length rows);
       Lwt.return_unit))

let test_insert_or_replace_multi_unique () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (a INTEGER, b INTEGER)" in
    let* _ = Db.execute db "CREATE UNIQUE INDEX idx_a ON t (a)" in
    let* _ = Db.execute db "CREATE UNIQUE INDEX idx_b ON t (b)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 100)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 200)" in
    (* New row (a=1, b=200) conflicts with row 1 via idx_a AND row 2 via idx_b
       REPLACE must delete BOTH old rows and insert the new one *)
    let* _ = Db.execute db "INSERT OR REPLACE INTO t VALUES (1, 200)" in
    let* r = Db.query db "SELECT COUNT(*) FROM t" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "only new row remains" 1
         (match rows with [[| Db.V_int n |]] -> Int64.to_int n | _ -> -1);
       Lwt.return_unit))

(* ------------------------------------------------------------------ *)
(* RETURNING clause tests                                               *)
(* ------------------------------------------------------------------ *)

let test_insert_returning () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, v TEXT)" in
    let* r = Db.query db "INSERT INTO t VALUES (1, 'hello') RETURNING id, v" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "one returned row" 1 (List.length rows);
       let row = List.hd rows in
       Alcotest.(check int) "id=1" 1 (match row.(0) with Db.V_int n -> Int64.to_int n | _ -> -1);
       Alcotest.(check string) "v=hello" "hello" (match row.(1) with Db.V_text s -> s | _ -> "X");
       Lwt.return_unit))

let test_update_returning () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, v TEXT)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'old')" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 'keep')" in
    let* r = Db.query db "UPDATE t SET v = 'new' WHERE id = 1 RETURNING id, v" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "one updated row returned" 1 (List.length rows);
       let row = List.hd rows in
       Alcotest.(check string) "v=new" "new" (match row.(1) with Db.V_text s -> s | _ -> "X");
       Lwt.return_unit))

let test_delete_returning () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, v TEXT)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'a')" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 'b')" in
    let* r = Db.query db "DELETE FROM t WHERE id = 1 RETURNING id, v" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "one deleted row returned" 1 (List.length rows);
       let row = List.hd rows in
       Alcotest.(check int) "id=1" 1 (match row.(0) with Db.V_int n -> Int64.to_int n | _ -> -1);
       Lwt.return_unit))

let test_update_returning_multi () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, v INTEGER)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 10)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 20)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (3, 30)" in
    let* r = Db.query db "UPDATE t SET v = v + 1 RETURNING id, v" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "3 rows returned" 3 (List.length rows);
       Lwt.return_unit))

let test_delete_returning_multi () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (3)" in
    let* r = Db.query db "DELETE FROM t RETURNING id" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "all 3 rows returned" 3 (List.length rows);
       Lwt.return_unit))

(* ------------------------------------------------------------------ *)
(* alter_table                                                          *)
(* ------------------------------------------------------------------ *)

let test_alter_add_column () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, name TEXT)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'alice')" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 'bob')" in
    let* _ = Db.execute db "ALTER TABLE t ADD COLUMN score INTEGER DEFAULT 0" in
    let* r = Db.query db "SELECT id, name, score FROM t ORDER BY id" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "2 rows" 2 (List.length rows);
       (match rows with
        | [r1; r2] ->
          Alcotest.(check int) "score1=0" 0 (match r1.(2) with Db.V_int n -> Int64.to_int n | _ -> -1);
          Alcotest.(check int) "score2=0" 0 (match r2.(2) with Db.V_int n -> Int64.to_int n | _ -> -1)
        | _ -> Alcotest.fail "wrong row count");
       Lwt.return_unit))

let test_alter_add_column_null_default () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (42)" in
    let* _ = Db.execute db "ALTER TABLE t ADD COLUMN extra TEXT" in
    let* r = Db.query db "SELECT id, extra FROM t" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       let row = List.hd rows in
       Alcotest.(check bool) "extra is null" true (row.(1) = Db.V_null);
       Lwt.return_unit))

let test_alter_add_not_null_no_default_error () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER)" in
    let r = Lwt_main.run (Db.execute db "ALTER TABLE t ADD COLUMN x TEXT NOT NULL") in
    Alcotest.(check bool) "error for NOT NULL without DEFAULT" true
      (match r with Error _ -> true | Ok _ -> false);
    Lwt.return_unit)

let test_rename_table () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE old_name (id INTEGER, v TEXT)" in
    let* _ = Db.execute db "INSERT INTO old_name VALUES (1, 'x')" in
    let* _ = Db.execute db "ALTER TABLE old_name RENAME TO new_name" in
    (* Old name is gone *)
    let r_old = Lwt_main.run (Db.query db "SELECT * FROM old_name") in
    Alcotest.(check bool) "old name gone" true (match r_old with Error _ -> true | Ok _ -> false);
    (* New name works *)
    let* r = Db.query db "SELECT id, v FROM new_name" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "row still there" 1 (List.length rows);
       Lwt.return_unit))

let test_rename_column () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, old_col TEXT)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'hello')" in
    let* _ = Db.execute db "ALTER TABLE t RENAME COLUMN old_col TO new_col" in
    let* r = Db.query db "SELECT id, new_col FROM t" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       let row = List.hd rows in
       Alcotest.(check string) "value preserved" "hello"
         (match row.(1) with Db.V_text s -> s | _ -> "X");
       (* Old column name should fail *)
       let* r_old = Db.query db "SELECT old_col FROM t" in
       Alcotest.(check bool) "old col name gone" true
         (match r_old with Error _ -> true | Ok _ -> false);
       Lwt.return_unit))

let test_rename_column_no_keyword () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (x INTEGER, y TEXT)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'abc')" in
    let* _ = Db.execute db "ALTER TABLE t RENAME y TO z" in
    let* r = Db.query db "SELECT x, z FROM t" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       let row = List.hd rows in
       Alcotest.(check string) "value preserved" "abc"
         (match row.(1) with Db.V_text s -> s | _ -> "X");
       Lwt.return_unit))

(* ------------------------------------------------------------------ *)
(* Group: Table-level UNIQUE and PRIMARY KEY constraints               *)
(* ------------------------------------------------------------------ *)

let test_table_unique_constraint () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE t (a TEXT, b INTEGER, UNIQUE(a, b))" in
    let* _ = Db.execute db "INSERT INTO t VALUES ('x', 1)" in
    let r = Lwt_main.run (Db.execute db "INSERT INTO t VALUES ('x', 1)") in
    Alcotest.(check bool) "unique violation raises" true
      (match r with Error (Db.Runtime _) -> true | _ -> false);
    (* Different combinations are fine *)
    let* _ = Db.execute db "INSERT INTO t VALUES ('x', 2)" in
    let* _ = Db.execute db "INSERT INTO t VALUES ('y', 1)" in
    let* r2 = Db.query db "SELECT COUNT(*) FROM t" in
    (match r2 with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "3 unique rows" 3
         (match rows with [[| Db.V_int n |]] -> Int64.to_int n | _ -> -1);
       Lwt.return_unit))

let test_table_primary_key_constraint () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY(id))" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'alice')" in
    let r = Lwt_main.run (Db.execute db "INSERT INTO t VALUES (1, 'bob')") in
    Alcotest.(check bool) "pk violation raises" true
      (match r with Error (Db.Runtime _) -> true | _ -> false);
    Lwt.return_unit)

(* ------------------------------------------------------------------ *)
(* Phase 8 edge-case / error-path tests                                *)
(* ------------------------------------------------------------------ *)

let test_insert_or_abort_error () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, v TEXT)" in
    let* _ = Db.execute db "CREATE UNIQUE INDEX idx_id ON t (id)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'a')" in
    let r = Lwt_main.run (Db.execute db "INSERT OR ABORT INTO t VALUES (1, 'b')") in
    Alcotest.(check bool) "abort raises on conflict" true
      (match r with Error (Db.Runtime _) -> true | _ -> false);
    Lwt.return_unit)

let test_insert_returning_ignored () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, v TEXT)" in
    let* _ = Db.execute db "CREATE UNIQUE INDEX idx_id ON t (id)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'orig')" in
    let* r = Db.query db "INSERT OR IGNORE INTO t VALUES (1, 'new') RETURNING id, v" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "empty stream when ignored" 0 (List.length rows);
       Lwt.return_unit))

let test_alter_rename_nonexistent () =
  Lwt_main.run (
    let r = Lwt_main.run (
      let* db = Db.open_in_memory () in
      Db.execute db "ALTER TABLE nonexistent RENAME TO new_name"
    ) in
    Alcotest.(check bool) "error on nonexistent table" true
      (match r with Error _ -> true | Ok _ -> false);
    Lwt.return_unit)

let test_alter_rename_column_nonexistent () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER)" in
    let r = Lwt_main.run (Db.execute db "ALTER TABLE t RENAME COLUMN nonexistent TO x") in
    Alcotest.(check bool) "error on nonexistent column" true
      (match r with Error _ -> true | Ok _ -> false);
    Lwt.return_unit)

let test_alter_add_column_duplicate () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, v TEXT)" in
    let r = Lwt_main.run (Db.execute db "ALTER TABLE t ADD COLUMN v TEXT") in
    Alcotest.(check bool) "error on duplicate column name" true
      (match r with Error _ -> true | Ok _ -> false);
    Lwt.return_unit)

let test_alter_rename_table_to_existing () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE a (id INTEGER)" in
    let* _ = Db.execute db "CREATE TABLE b (id INTEGER)" in
    let r = Lwt_main.run (Db.execute db "ALTER TABLE a RENAME TO b") in
    Alcotest.(check bool) "error renaming to existing table name" true
      (match r with Error _ -> true | Ok _ -> false);
    Lwt.return_unit)

let test_alter_add_column_nonexistent_table () =
  Lwt_main.run (
    let r = Lwt_main.run (
      let* db = Db.open_in_memory () in
      Db.execute db "ALTER TABLE nonexistent ADD COLUMN x TEXT"
    ) in
    Alcotest.(check bool) "error on nonexistent table" true
      (match r with Error _ -> true | Ok _ -> false);
    Lwt.return_unit)

(* ------------------------------------------------------------------ *)
(* Subqueries                                                           *)
(* ------------------------------------------------------------------ *)

let test_scalar_subquery () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 10)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 20)" in
    let* r = Db.query db "SELECT (SELECT MAX(v) FROM t)" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
    Alcotest.(check int) "scalar subquery returns one row" 1 (List.length rows);
    (match rows with
     | [row] ->
       Alcotest.check value_testable "scalar subquery returns max" (Db.V_int 20L) row.(0)
     | _ -> Alcotest.fail "expected exactly one row");
    Lwt.return_unit)

let test_scalar_subquery_null () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)" in
    let* r = Db.query db "SELECT (SELECT MAX(v) FROM t)" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
    Alcotest.(check int) "scalar subquery returns one row" 1 (List.length rows);
    (match rows with
     | [row] ->
       Alcotest.check value_testable "scalar subquery on empty table returns null" Db.V_null row.(0)
     | _ -> Alcotest.fail "expected exactly one row");
    Lwt.return_unit)

let test_exists_subquery () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 10)" in
    let* _ = Db.execute db "CREATE TABLE r (id INTEGER PRIMARY KEY, ref_id INTEGER)" in
    let* _ = Db.execute db "INSERT INTO r VALUES (1, 1)" in
    let* _ = Db.execute db "INSERT INTO r VALUES (2, 99)" in
    let* res = Db.query db
      "SELECT id FROM r WHERE EXISTS (SELECT 1 FROM t WHERE t.id = 1)" in
    let* rows = (match res with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
    Alcotest.(check int) "exists matches two rows" 2 (List.length rows);
    Lwt.return_unit)

let test_exists_subquery_false () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY)" in
    let* _ = Db.execute db "CREATE TABLE r (id INTEGER PRIMARY KEY)" in
    let* _ = Db.execute db "INSERT INTO r VALUES (1)" in
    let* res = Db.query db
      "SELECT id FROM r WHERE EXISTS (SELECT 1 FROM t)" in
    let* rows = (match res with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
    Alcotest.(check int) "exists on empty inner table returns 0 rows" 0 (List.length rows);
    Lwt.return_unit)

let test_in_select_subquery () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 10)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 20)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (3, 30)" in
    let* _ = Db.execute db "CREATE TABLE allowed (v INTEGER)" in
    let* _ = Db.execute db "INSERT INTO allowed VALUES (10)" in
    let* _ = Db.execute db "INSERT INTO allowed VALUES (30)" in
    let* res = Db.query db "SELECT id FROM t WHERE v IN (SELECT v FROM allowed)" in
    let* rows = (match res with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
    let ids = List.map (fun r -> r.(0)) rows in
    Alcotest.(check (list value_testable)) "in-select returns matching rows"
      [Db.V_int 1L; Db.V_int 3L] ids;
    Lwt.return_unit)

let test_not_in_select_subquery () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 10)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 20)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (3, 30)" in
    let* _ = Db.execute db "CREATE TABLE excluded (v INTEGER)" in
    let* _ = Db.execute db "INSERT INTO excluded VALUES (20)" in
    let* res = Db.query db "SELECT id FROM t WHERE v NOT IN (SELECT v FROM excluded)" in
    let* rows = (match res with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
    let ids = List.map (fun r -> r.(0)) rows in
    Alcotest.(check (list value_testable)) "not-in-select excludes row 2"
      [Db.V_int 1L; Db.V_int 3L] ids;
    Lwt.return_unit)

(* ------------------------------------------------------------------ *)
(* FOREIGN KEY parse-only                                                *)
(* ------------------------------------------------------------------ *)

let test_fk_parse_create () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)" in
    (* FK syntax must be accepted without error *)
    let* _ = Db.execute db
      "CREATE TABLE orders (id INTEGER PRIMARY KEY, user_id INTEGER REFERENCES users(id))" in
    let* n = Db.execute db "INSERT INTO orders VALUES (1, 1)" in
    Alcotest.(check bool) "insert into FK table works (no enforcement)" true (n = Ok ());
    Lwt.return_unit)

let test_fk_parse_no_col () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)" in
    (* REFERENCES without explicit column is also valid SQL syntax *)
    let* _ = Db.execute db
      "CREATE TABLE orders (id INTEGER PRIMARY KEY, user_id INTEGER REFERENCES users)" in
    let* n = Db.execute db "INSERT INTO orders VALUES (1, 999)" in
    (* No FK enforcement — 999 doesn't exist in users but insert succeeds *)
    Alcotest.(check bool) "insert without FK enforcement" true (n = Ok ());
    Lwt.return_unit)

(* ------------------------------------------------------------------ *)
(* CHECK constraints                                                     *)
(* ------------------------------------------------------------------ *)

let test_check_insert_ok () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE prices (id INTEGER PRIMARY KEY, amount REAL CHECK (amount > 0))" in
    let* res = Db.execute db "INSERT INTO prices VALUES (1, 9.99)" in
    Alcotest.(check bool) "valid row inserted" true (res = Ok ());
    Lwt.return_unit)

let test_check_insert_violation () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE prices (id INTEGER PRIMARY KEY, amount REAL CHECK (amount > 0))" in
    let* res = Db.execute db "INSERT INTO prices VALUES (1, -5.0)" in
    (match res with
     | Error _ -> ()
     | Ok ()   -> Alcotest.fail "expected CHECK violation but insert succeeded");
    Lwt.return_unit)

let test_check_update_violation () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER CHECK (v >= 0))" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 5)" in
    let* res = Db.execute db "UPDATE t SET v = -1 WHERE id = 1" in
    (match res with
     | Error _ -> ()
     | Ok ()   -> Alcotest.fail "expected CHECK violation on update");
    Lwt.return_unit)

let test_check_null_allowed () =
  run (
    (* SQLite CHECK: NULL in CHECK expr -> passes (not a violation) *)
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER CHECK (v > 0))" in
    let* res = Db.execute db "INSERT INTO t VALUES (1, NULL)" in
    Alcotest.(check bool) "null passes CHECK" true (res = Ok ());
    Lwt.return_unit)

let test_check_cache_invalidation () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (v INTEGER CHECK (v > 100))" in
    let* _ = Db.execute db "INSERT INTO t VALUES (150)" in
    let* _ = Db.execute db "DROP TABLE t" in
    let* _ = Db.execute db "CREATE TABLE t (v INTEGER CHECK (v < 5))" in
    (* This should pass new CHECK (v < 5), NOT be blocked by old cache *)
    let* res = Db.execute db "INSERT INTO t VALUES (3)" in
    Alcotest.(check bool) "insert passes new CHECK after DROP+CREATE" true (res = Ok ());
    (* This should be blocked by new CHECK (v < 5) *)
    let* res2 = Db.execute db "INSERT INTO t VALUES (200)" in
    (match res2 with
     | Error _ -> ()
     | Ok ()   -> Alcotest.fail "old values should fail new CHECK");
    Lwt.return_unit)

let test_check_persisted () =
  run (
    let tmpfile = Filename.temp_file "sqlocaml_check_" ".db" in
    Fun.protect ~finally:(fun () -> try Unix.unlink tmpfile with _ -> ()) (fun () ->
      let* db_res = Db.open_file ~path:tmpfile in
      let db = match db_res with Ok d -> d | Error _ -> failwith "open_file failed" in
      let* _ = Db.execute db
        "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER CHECK (v > 0))" in
      let* _ = Db.execute db "INSERT INTO t VALUES (1, 10)" in
      let* () = Db.close db in
      let* db2_res = Db.open_file ~path:tmpfile in
      let db2 = match db2_res with Ok d -> d | Error _ -> failwith "open_file2 failed" in
      let* res = Db.execute db2 "INSERT INTO t VALUES (2, -1)" in
      let* () = Db.close db2 in
      (match res with
       | Error _ -> ()
       | Ok ()   -> Alcotest.fail "expected CHECK to persist after reopen");
      Lwt.return_unit))

(* ------------------------------------------------------------------ *)
(* Phase 9 edge cases                                                   *)
(* ------------------------------------------------------------------ *)

let test_const_select () =
  run (
    let* db = Db.open_in_memory () in
    let* r = Db.query db "SELECT 1 + 1" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "const select error: %a" Db.pp_error e) in
    Alcotest.(check int) "const select returns one row" 1 (List.length rows);
    (match rows with
     | [row] ->
       Alcotest.check value_testable "1+1 = V_int 2" (Db.V_int 2L) row.(0)
     | _ -> Alcotest.fail "expected exactly one row");
    Lwt.return_unit)

let test_subquery_and_predicate () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 10)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 20)" in
    let* res = Db.query db
      "SELECT id FROM t WHERE EXISTS (SELECT 1 FROM t WHERE id = 1) AND v > 15" in
    let* rows = (match res with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "subquery_and_pred error: %a" Db.pp_error e) in
    let ids = List.map (fun r -> r.(0)) rows in
    Alcotest.(check (list value_testable)) "exists AND predicate"
      [Db.V_int 2L] ids;
    Lwt.return_unit)

let test_check_complex_expr () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE prices_t (id INTEGER PRIMARY KEY, price REAL CHECK (price > 0 AND price < 1000))" in
    let* res = Db.execute db "INSERT INTO prices_t VALUES (1, 99.9)" in
    Alcotest.(check bool) "complex check passes" true (res = Ok ());
    let* res2 = Db.execute db "INSERT INTO prices_t VALUES (2, 1500.0)" in
    (match res2 with
     | Error _ -> ()
     | Ok ()   -> Alcotest.fail "expected CHECK violation for price > 1000");
    Lwt.return_unit)

let test_check_function_in_expr () =
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE names_t (id INTEGER PRIMARY KEY, name TEXT CHECK (LENGTH(name) > 0))" in
    let* res = Db.execute db "INSERT INTO names_t VALUES (1, 'Alice')" in
    (match res with
     | Error e -> Alcotest.failf "INSERT error: %a" Db.pp_error e
     | Ok () -> ());
    Alcotest.(check bool) "check with function passes" true (res = Ok ());
    let* res2 = Db.execute db "INSERT INTO names_t VALUES (2, '')" in
    (match res2 with
     | Error _ -> ()
     | Ok ()   -> Alcotest.fail "expected CHECK violation for empty name");
    Lwt.return_unit)

let test_text_scalar_subquery () =
  (* Covers exec.ml value_to_literal V_text branch *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE words (id INTEGER, w TEXT)" in
    let* _ = Db.execute db "INSERT INTO words VALUES (1, 'apple')" in
    let* _ = Db.execute db "INSERT INTO words VALUES (2, 'banana')" in
    let* r = Db.query db "SELECT (SELECT MAX(w) FROM words)" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "text scalar subquery error: %a" Db.pp_error e) in
    Alcotest.(check int) "one row" 1 (List.length rows);
    (match rows with
     | [row] ->
       Alcotest.check value_testable "max text" (Db.V_text "banana") row.(0)
     | _ -> Alcotest.fail "expected one row");
    Lwt.return_unit)

let test_real_scalar_subquery () =
  (* Covers exec.ml value_to_literal V_real branch *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE prices (id INTEGER, price REAL)" in
    let* _ = Db.execute db "INSERT INTO prices VALUES (1, 1.5)" in
    let* _ = Db.execute db "INSERT INTO prices VALUES (2, 9.99)" in
    let* r = Db.query db "SELECT (SELECT MAX(price) FROM prices)" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "real scalar subquery error: %a" Db.pp_error e) in
    Alcotest.(check int) "one row" 1 (List.length rows);
    (match rows with
     | [row] ->
       Alcotest.check value_testable "max real" (Db.V_real 9.99) row.(0)
     | _ -> Alcotest.fail "expected one row");
    Lwt.return_unit)

let test_like_in_where () =
  (* Covers sema.ml Like/Glob in ast_binop_to_sema and exec.ml like_match *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE names (id INTEGER, name TEXT)" in
    let* _ = Db.execute db "INSERT INTO names VALUES (1, 'alice')" in
    let* _ = Db.execute db "INSERT INTO names VALUES (2, 'bob')" in
    let* _ = Db.execute db "INSERT INTO names VALUES (3, 'alicia')" in
    let* r = Db.query db "SELECT id FROM names WHERE name LIKE 'ali%' ORDER BY id" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "LIKE query error: %a" Db.pp_error e) in
    let ids = List.map (fun r -> r.(0)) rows in
    Alcotest.(check (list value_testable)) "LIKE 'ali%' matches alice and alicia"
      [Db.V_int 1L; Db.V_int 3L] ids;
    Lwt.return_unit)

let test_modulo_operator () =
  (* Covers sema.ml Mod in ast_binop_to_sema and exec.ml Mod evaluation *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE nums (n INTEGER)" in
    let* _ = Db.execute db "INSERT INTO nums VALUES (7)" in
    let* _ = Db.execute db "INSERT INTO nums VALUES (10)" in
    let* r = Db.query db "SELECT n % 3 FROM nums ORDER BY n" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "modulo query error: %a" Db.pp_error e) in
    let vals = List.map (fun r -> r.(0)) rows in
    Alcotest.(check (list value_testable)) "7%3=1, 10%3=1"
      [Db.V_int 1L; Db.V_int 1L] vals;
    Lwt.return_unit)

let test_sum_real_with_nulls () =
  (* Covers exec.ml SUM REAL path with NULLs (lines 1896-1904) *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE prices2 (id INTEGER, price REAL)" in
    let* _ = Db.execute db "INSERT INTO prices2 VALUES (1, 1.5)" in
    let* _ = Db.execute db "INSERT INTO prices2 VALUES (2, NULL)" in
    let* _ = Db.execute db "INSERT INTO prices2 VALUES (3, 2.5)" in
    let* r = Db.query db "SELECT SUM(price) FROM prices2" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "SUM REAL query error: %a" Db.pp_error e) in
    Alcotest.(check int) "one row" 1 (List.length rows);
    (match rows with
     | [row] ->
       Alcotest.check value_testable "SUM(1.5 + NULL + 2.5) = 4.0" (Db.V_real 4.0) row.(0)
     | _ -> Alcotest.fail "expected one row");
    Lwt.return_unit)

let test_avg_real_column () =
  (* Covers exec.ml AVG with REAL values (lines 1913-1922) *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE real_vals (v REAL)" in
    let* _ = Db.execute db "INSERT INTO real_vals VALUES (2.0)" in
    let* _ = Db.execute db "INSERT INTO real_vals VALUES (NULL)" in
    let* _ = Db.execute db "INSERT INTO real_vals VALUES (4.0)" in
    let* r = Db.query db "SELECT AVG(v) FROM real_vals" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "AVG REAL query error: %a" Db.pp_error e) in
    Alcotest.(check int) "one row" 1 (List.length rows);
    (match rows with
     | [row] ->
       Alcotest.check value_testable "AVG(2.0, null, 4.0) = 3.0" (Db.V_real 3.0) row.(0)
     | _ -> Alcotest.fail "expected one row");
    Lwt.return_unit)

let test_check_add_binop () =
  (* Covers exec.ml ast_binop_to_plan for Add/Mod operators via CHECK *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE even_t (id INTEGER PRIMARY KEY, n INTEGER CHECK (n % 2 = 0))" in
    let* res = Db.execute db "INSERT INTO even_t VALUES (1, 4)" in
    Alcotest.(check bool) "even passes CHECK" true (res = Ok ());
    let* res2 = Db.execute db "INSERT INTO even_t VALUES (2, 3)" in
    (match res2 with
     | Error _ -> ()
     | Ok () -> Alcotest.fail "odd should fail CHECK");
    Lwt.return_unit)

let test_check_concat_binop () =
  (* Covers exec.ml ast_binop_to_plan for Concat/Ne via CHECK *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE prefix_t (id INTEGER PRIMARY KEY, name TEXT CHECK (name || '' != ''))" in
    let* res = Db.execute db "INSERT INTO prefix_t VALUES (1, 'Alice')" in
    Alcotest.(check bool) "non-empty name passes" true (res = Ok ());
    Lwt.return_unit)

let test_check_like_in_constraint () =
  (* Covers exec.ml ast_binop_to_plan for Like via CHECK *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE upper_t (id INTEGER PRIMARY KEY, code TEXT CHECK (code LIKE 'A%'))" in
    let* res = Db.execute db "INSERT INTO upper_t VALUES (1, 'ABC')" in
    Alcotest.(check bool) "A-prefix passes CHECK LIKE" true (res = Ok ());
    let* res2 = Db.execute db "INSERT INTO upper_t VALUES (2, 'xyz')" in
    (match res2 with
     | Error _ -> ()
     | Ok () -> Alcotest.fail "non-A prefix should fail CHECK LIKE");
    Lwt.return_unit)

let test_check_not_operator () =
  (* Covers exec.ml ast_expr_to_plan_check E_not/E_neg/E_bitnot paths *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE not_t (id INTEGER PRIMARY KEY, n INTEGER CHECK (NOT (n = 0)))" in
    let* res = Db.execute db "INSERT INTO not_t VALUES (1, 5)" in
    Alcotest.(check bool) "NOT(n=0) with n=5 passes" true (res = Ok ());
    let* res2 = Db.execute db "INSERT INTO not_t VALUES (2, 0)" in
    (match res2 with
     | Error _ -> ()
     | Ok () -> Alcotest.fail "NOT(n=0) with n=0 should fail");
    Lwt.return_unit)

let test_check_between_in_check () =
  (* Covers exec.ml ast_expr_to_plan_check E_between path *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE range_t (id INTEGER PRIMARY KEY, v INTEGER CHECK (v BETWEEN 1 AND 10))" in
    let* res = Db.execute db "INSERT INTO range_t VALUES (1, 5)" in
    Alcotest.(check bool) "v BETWEEN 1 AND 10 with v=5 passes" true (res = Ok ());
    let* res2 = Db.execute db "INSERT INTO range_t VALUES (2, 15)" in
    (match res2 with
     | Error _ -> ()
     | Ok () -> Alcotest.fail "v=15 should fail BETWEEN 1 AND 10");
    Lwt.return_unit)

let test_check_is_not_null_in_check () =
  (* Covers exec.ml ast_expr_to_plan_check E_is_not_null/E_is_null path *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE notnull_t (id INTEGER PRIMARY KEY, v TEXT CHECK (v IS NOT NULL))" in
    let* res = Db.execute db "INSERT INTO notnull_t VALUES (1, 'hello')" in
    Alcotest.(check bool) "IS NOT NULL with 'hello' passes" true (res = Ok ());
    let* res2 = Db.execute db "INSERT INTO notnull_t VALUES (2, NULL)" in
    (match res2 with
     | Error _ -> ()
     | Ok () -> Alcotest.fail "NULL should fail IS NOT NULL check");
    Lwt.return_unit)

let test_real_mod_int () =
  (* Covers exec.ml V_real,V_int Mod path (line 432-433) *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE rmod (v REAL)" in
    let* _ = Db.execute db "INSERT INTO rmod VALUES (7.5)" in
    (* 7.5 % 2 → should be 1.5 (real % int) *)
    let* r = Db.query db "SELECT v % 2 FROM rmod" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "real_mod_int error: %a" Db.pp_error e) in
    Alcotest.(check int) "one row" 1 (List.length rows);
    (match rows with
     | [row] ->
       Alcotest.check value_testable "7.5 % 2 = 1.5" (Db.V_real 1.5) row.(0)
     | _ -> Alcotest.fail "expected one row");
    Lwt.return_unit)

let test_real_ne_comparison () =
  (* Covers exec.ml eval_binop Ne with V_real,V_real (line 401) and null (line 398) *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE reals (id INTEGER, v REAL)" in
    let* _ = Db.execute db "INSERT INTO reals VALUES (1, 1.5)" in
    let* _ = Db.execute db "INSERT INTO reals VALUES (2, 2.5)" in
    let* _ = Db.execute db "INSERT INTO reals VALUES (3, NULL)" in
    (* REAL != REAL: v != 1.5 should return rows 2 (2.5 != 1.5) but not 1 or 3 *)
    let* r = Db.query db "SELECT id FROM reals WHERE v != 1.5 ORDER BY id" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "real_ne_comparison error: %a" Db.pp_error e) in
    let ids = List.map (fun r -> r.(0)) rows in
    Alcotest.(check (list value_testable)) "only id=2 has v != 1.5"
      [Db.V_int 2L] ids;
    Lwt.return_unit)

let test_alter_add_text_default () =
  (* Covers exec.ml lines 1415-1416: ALTER TABLE ADD COLUMN with TEXT/REAL default *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE alter_t (id INTEGER)" in
    let* _ = Db.execute db "INSERT INTO alter_t VALUES (1)" in
    (* Add TEXT column with default — covers L_text branch in execute_with_count *)
    let* res = Db.execute db "ALTER TABLE alter_t ADD COLUMN status TEXT DEFAULT 'active'" in
    Alcotest.(check bool) "ALTER ADD TEXT default ok" true (res = Ok ());
    (* Add REAL column with default — covers L_real branch *)
    let* res2 = Db.execute db "ALTER TABLE alter_t ADD COLUMN score REAL DEFAULT 0.0" in
    Alcotest.(check bool) "ALTER ADD REAL default ok" true (res2 = Ok ());
    let* r = Db.query db "SELECT id, status, score FROM alter_t" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "alter_add_text_default query error: %a" Db.pp_error e) in
    Alcotest.(check int) "one row" 1 (List.length rows);
    (match rows with
     | [row] ->
       Alcotest.check value_testable "status='active'" (Db.V_text "active") row.(1);
       Alcotest.check value_testable "score=0.0" (Db.V_real 0.0) row.(2)
     | _ -> Alcotest.fail "expected one row");
    Lwt.return_unit)

let test_int_real_comparison () =
  (* Covers exec.ml cmp_result V_int,V_real and V_real,V_int cross-type paths *)
  run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE mixed (n INTEGER)" in
    let* _ = Db.execute db "INSERT INTO mixed VALUES (1)" in
    let* _ = Db.execute db "INSERT INTO mixed VALUES (2)" in
    let* _ = Db.execute db "INSERT INTO mixed VALUES (3)" in
    (* WHERE n > 2.5 → integer vs real comparison, covering V_int,V_real path *)
    let* r = Db.query db "SELECT n FROM mixed WHERE n > 2.5 ORDER BY n" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "int_real_cmp error: %a" Db.pp_error e) in
    let vals = List.map (fun r -> r.(0)) rows in
    Alcotest.(check (list value_testable)) "only 3 > 2.5"
      [Db.V_int 3L] vals;
    Lwt.return_unit)

(* ------------------------------------------------------------------ *)
(* Phase 9 string-function coverage                                     *)
(* These tests target specific uncovered branches in exec.ml:          *)
(*  - str_trim_spaces / str_rtrim_spaces empty-result branch            *)
(*  - str_trim_chars / str_rtrim_chars empty-result branch              *)
(*  - str_replace with empty old-string branch                          *)
(*  - tab/newline/cr whitespace in TRIM functions                       *)
(* ------------------------------------------------------------------ *)

let test_trim_all_whitespace () =
  (* TRIM of strings with tabs/newlines → hits \t, \n, \r branches in trim helpers *)
  run (
    let* db = Db.open_in_memory () in
    (* All spaces → str_trim_spaces empty-result branch *)
    let* r = Db.query db "SELECT TRIM('   ')" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "TRIM all-space error: %a" Db.pp_error e) in
    (match rows with
     | [row] -> Alcotest.check value_testable "TRIM of spaces = empty" (Db.V_text "") row.(0)
     | _ -> Alcotest.fail "expected one row");
    (* TRIM with tab chars on both sides — covers '\t' in str_trim_spaces *)
    let* r2 = Db.query db "SELECT TRIM('\thello\t')" in
    let* rows2 = (match r2 with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "TRIM tab error: %a" Db.pp_error e) in
    (match rows2 with
     | [row] -> Alcotest.check value_testable "TRIM tabs from both sides" (Db.V_text "hello") row.(0)
     | _ -> Alcotest.fail "expected one row");
    (* LTRIM with tab — covers '\t' in str_ltrim_spaces *)
    let* r3 = Db.query db "SELECT LTRIM('\thello')" in
    let* rows3 = (match r3 with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "LTRIM tab error: %a" Db.pp_error e) in
    (match rows3 with
     | [row] -> Alcotest.check value_testable "LTRIM tab" (Db.V_text "hello") row.(0)
     | _ -> Alcotest.fail "expected one row");
    (* RTRIM with tabs only → str_rtrim_spaces empty-result branch *)
    let* r4 = Db.query db "SELECT RTRIM('\t\t\t')" in
    let* rows4 = (match r4 with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "RTRIM all-tabs error: %a" Db.pp_error e) in
    (match rows4 with
     | [row] -> Alcotest.check value_testable "RTRIM of tabs = empty" (Db.V_text "") row.(0)
     | _ -> Alcotest.fail "expected one row");
    Lwt.return_unit)

let test_trim_chars_all_removed () =
  (* TRIM(s, chars) where all chars are in the trim-set → "" *)
  run (
    let* db = Db.open_in_memory () in
    let* r = Db.query db "SELECT TRIM('xxxxx', 'x')" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "TRIM chars all-removed error: %a" Db.pp_error e) in
    (match rows with
     | [row] -> Alcotest.check value_testable "TRIM('xxxxx','x') = empty" (Db.V_text "") row.(0)
     | _ -> Alcotest.fail "expected one row");
    let* r2 = Db.query db "SELECT RTRIM('aaa', 'a')" in
    let* rows2 = (match r2 with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "RTRIM chars all-removed error: %a" Db.pp_error e) in
    (match rows2 with
     | [row] -> Alcotest.check value_testable "RTRIM('aaa','a') = empty" (Db.V_text "") row.(0)
     | _ -> Alcotest.fail "expected one row");
    Lwt.return_unit)

let test_replace_empty_old () =
  (* REPLACE(s, '', rep) → s unchanged (covers str_replace empty-old branch) *)
  run (
    let* db = Db.open_in_memory () in
    let* r = Db.query db "SELECT REPLACE('hello', '', 'X')" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error e ->
      Alcotest.failf "REPLACE empty-old error: %a" Db.pp_error e) in
    (match rows with
     | [row] -> Alcotest.check value_testable "REPLACE with empty old = original" (Db.V_text "hello") row.(0)
     | _ -> Alcotest.fail "expected one row");
    Lwt.return_unit)

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
    "order_by", [
      Alcotest.test_case "order_by_asc"         `Quick order_by_asc;
      Alcotest.test_case "order_by_desc"        `Quick order_by_desc;
      Alcotest.test_case "order_by_default_asc" `Quick order_by_default_asc;
      Alcotest.test_case "order_by_null_first"  `Quick order_by_null_first;
      Alcotest.test_case "order_by_text"        `Quick order_by_text;
      Alcotest.test_case "order_by_expr"        `Quick order_by_expr;
      Alcotest.test_case "join_order_by"        `Quick test_order_by_expr_join;
    ];
    "limit_offset", [
      Alcotest.test_case "limit_no_order"       `Quick limit_no_order;
      Alcotest.test_case "limit_with_offset"    `Quick limit_with_offset;
      Alcotest.test_case "order_by_then_limit"  `Quick order_by_then_limit;
      Alcotest.test_case "order_by_limit_offset" `Quick order_by_limit_offset;
    ];
    "qcheck_order_limit", (
      List.map QCheck_alcotest.to_alcotest [
        qcheck_order_by_asc_sorted;
        qcheck_order_by_desc_sorted;
        qcheck_limit_count;
      ]
    );
    "create_index", [
      Alcotest.test_case "create_index_simple"              `Quick create_index_simple;
      Alcotest.test_case "create_index_on_text_column"      `Quick create_index_on_text_column;
      Alcotest.test_case "create_unique_index"              `Quick create_unique_index;
      Alcotest.test_case "unique_index_rejects_duplicate"    `Quick unique_index_rejects_duplicate;
      Alcotest.test_case "unique_index_allows_distinct"      `Quick unique_index_allows_distinct_values;
      Alcotest.test_case "index_lookup_no_match"            `Quick index_lookup_no_match;
      Alcotest.test_case "index_lookup_returns_all_matches" `Quick index_lookup_returns_all_matches;
      Alcotest.test_case "index_lookup_matches_seq_scan"    `Quick index_lookup_matches_seq_scan;
      Alcotest.test_case "index_lookup_after_inserts"       `Quick index_lookup_after_inserts;
      Alcotest.test_case "create_index_unknown_table"       `Quick create_index_unknown_table;
      Alcotest.test_case "create_index_unknown_column"      `Quick create_index_unknown_column;
      Alcotest.test_case "create_index_duplicate"           `Quick create_index_duplicate;
      Alcotest.test_case "index_lookup_where_null_no_match"  `Quick index_lookup_where_null_no_match;
      Alcotest.test_case "multi_col_index"                   `Quick test_multi_col_index;
      Alcotest.test_case "multi_col_unique_index"             `Quick test_multi_col_unique_index;
    ];
    "qcheck_create_index", (
      List.map QCheck_alcotest.to_alcotest [
        qcheck_index_lookup_matches_seq_scan;
        qcheck_text_index_lookup;
      ]
    );
    "not_null_and_default", [
      Alcotest.test_case "not_null_insert_null"             `Quick not_null_insert_null;
      Alcotest.test_case "not_null_default_used_when_omitted" `Quick not_null_default_used_when_omitted;
      Alcotest.test_case "not_null_default_zero"            `Quick not_null_default_zero;
      Alcotest.test_case "not_null_update_to_null"          `Quick not_null_update_to_null;
      Alcotest.test_case "default_value_readable"           `Quick default_value_readable;
      Alcotest.test_case "not_null_omit_no_default"         `Quick not_null_omit_no_default;
    ];
    "qcheck_not_null_default", (
      List.map QCheck_alcotest.to_alcotest [
        qcheck_default_applied;
      ]
    );
    "drop_table_and_index", [
      Alcotest.test_case "drop_table_then_select"             `Quick drop_table_then_select;
      Alcotest.test_case "drop_table_nonexistent"             `Quick drop_table_nonexistent;
      Alcotest.test_case "drop_table_data_gone"               `Quick drop_table_data_gone;
      Alcotest.test_case "drop_table_with_index"              `Quick drop_table_with_index;
      Alcotest.test_case "drop_table_then_recreate"           `Quick drop_table_then_recreate;
      Alcotest.test_case "drop_index_then_select"             `Quick drop_index_then_select;
      Alcotest.test_case "drop_index_nonexistent"             `Quick drop_index_nonexistent;
      Alcotest.test_case "drop_index_then_recreate"           `Quick drop_index_then_recreate;
      Alcotest.test_case "drop_table_recreate_old_data_invis" `Quick drop_table_recreate_old_data_invisible;
      Alcotest.test_case "drop_first_index_then_recreate"     `Quick drop_first_index_then_recreate;
    ];
    "gap_fill", [
      Alcotest.test_case "open_file_invalid_path"  `Quick open_file_invalid_path;
      Alcotest.test_case "default_real_persists"   `Quick default_real_persists;
      Alcotest.test_case "default_text_persists"   `Quick default_text_persists;
      Alcotest.test_case "default_int_persists"    `Quick default_int_persists;
    ];
    "between_in", [
      Alcotest.test_case "between"     `Quick test_between_query;
      Alcotest.test_case "not_between" `Quick test_not_between_query;
      Alcotest.test_case "in"          `Quick test_in_query;
      Alcotest.test_case "not_in"      `Quick test_not_in_query;
    ];
    "pragma", [
      Alcotest.test_case "table_info"  `Quick test_pragma_table_info;
      Alcotest.test_case "index_list"  `Quick test_pragma_index_list;
    ];
    "select_distinct", [
      Alcotest.test_case "distinct_single_col"   `Quick test_distinct;
      Alcotest.test_case "distinct_multicolumn"  `Quick test_distinct_multicolumn;
      Alcotest.test_case "distinct_null"         `Quick test_distinct_null;
    ];
    "set_operations", [
      Alcotest.test_case "union"      `Quick test_union;
      Alcotest.test_case "union_all"  `Quick test_union_all;
      Alcotest.test_case "intersect"  `Quick test_intersect;
      Alcotest.test_case "except"     `Quick test_except;
    ];
    "named_indexed_params", [
      Alcotest.test_case "indexed_params"       `Quick test_indexed_params;
      Alcotest.test_case "named_params_colon"   `Quick test_named_params_colon;
    ];
    "datetime_functions", [
      Alcotest.test_case "date_fn"      `Quick test_date_fn;
      Alcotest.test_case "time_fn"      `Quick test_time_fn;
      Alcotest.test_case "datetime_fn"  `Quick test_datetime_fn;
      Alcotest.test_case "julianday_fn" `Quick test_julianday_fn;
      Alcotest.test_case "unixepoch_fn" `Quick test_unixepoch_fn;
      Alcotest.test_case "strftime_fn"  `Quick test_strftime_fn;
      Alcotest.test_case "date_now"     `Quick test_date_now;
    ];
    "multikey_order_by", [
      Alcotest.test_case "multikey_asc_asc"   `Quick test_multikey_order_by;
      Alcotest.test_case "multikey_asc_desc"  `Quick test_multikey_order_by_mixed;
    ];
    "distinct_types", [
      Alcotest.test_case "distinct_real"  `Quick test_distinct_real;
    ];
    "set_op_edge_cases", [
      Alcotest.test_case "intersect_empty_right"  `Quick test_intersect_empty_right;
      Alcotest.test_case "except_dedup_left"      `Quick test_except_dedup_left;
      Alcotest.test_case "except_empty_right"     `Quick test_except_empty_right;
    ];
    "datetime_null_branches", [
      Alcotest.test_case "date_invalid_input"      `Quick test_date_invalid_input;
      Alcotest.test_case "date_nontext_arg"        `Quick test_date_nontext_arg;
      Alcotest.test_case "date_with_modifier"      `Quick test_date_with_modifier;
      Alcotest.test_case "time_invalid_input"      `Quick test_time_invalid_input;
      Alcotest.test_case "time_nontext_arg"        `Quick test_time_nontext_arg;
      Alcotest.test_case "time_with_modifier"      `Quick test_time_with_modifier;
      Alcotest.test_case "datetime_invalid_input"  `Quick test_datetime_invalid_input;
      Alcotest.test_case "datetime_nontext_arg"    `Quick test_datetime_nontext_arg;
      Alcotest.test_case "datetime_with_modifier"  `Quick test_datetime_with_modifier;
      Alcotest.test_case "julianday_invalid"       `Quick test_julianday_invalid;
      Alcotest.test_case "julianday_nontext_arg"   `Quick test_julianday_nontext_arg;
      Alcotest.test_case "julianday_with_modifier" `Quick test_julianday_with_modifier;
      Alcotest.test_case "unixepoch_invalid"       `Quick test_unixepoch_invalid;
      Alcotest.test_case "unixepoch_nontext_arg"   `Quick test_unixepoch_nontext_arg;
      Alcotest.test_case "unixepoch_with_modifier" `Quick test_unixepoch_with_modifier;
      Alcotest.test_case "strftime_null_fmt"       `Quick test_strftime_null_fmt;
      Alcotest.test_case "strftime_invalid_ts"     `Quick test_strftime_invalid_ts;
      Alcotest.test_case "strftime_with_modifier"  `Quick test_strftime_with_modifier;
    ];
    "datetime_null_arg_branches", [
      Alcotest.test_case "date_null_arg"           `Quick test_date_null_arg;
      Alcotest.test_case "date_null_with_rest"     `Quick test_date_null_with_rest;
      Alcotest.test_case "time_null_arg"           `Quick test_time_null_arg;
      Alcotest.test_case "time_null_with_rest"     `Quick test_time_null_with_rest;
      Alcotest.test_case "datetime_null_arg"       `Quick test_datetime_null_arg;
      Alcotest.test_case "datetime_null_with_rest" `Quick test_datetime_null_with_rest;
      Alcotest.test_case "julianday_null_arg"      `Quick test_julianday_null_arg;
      Alcotest.test_case "julianday_null_with_rest" `Quick test_julianday_null_with_rest;
      Alcotest.test_case "unixepoch_null_arg"      `Quick test_unixepoch_null_arg;
      Alcotest.test_case "unixepoch_null_with_rest" `Quick test_unixepoch_null_with_rest;
    ];
    "distinct_blob", [
      Alcotest.test_case "distinct_blob_null"  `Quick test_distinct_blob_null;
    ];
    "on_conflict", [
      Alcotest.test_case "insert_or_ignore"             `Quick test_insert_or_ignore;
      Alcotest.test_case "insert_or_replace"            `Quick test_insert_or_replace;
      Alcotest.test_case "insert_or_ignore_unique"      `Quick test_insert_or_ignore_unique;
      Alcotest.test_case "insert_or_replace_no_conflict" `Quick test_insert_or_replace_no_conflict;
      Alcotest.test_case "insert_or_replace_multi_unique" `Quick test_insert_or_replace_multi_unique;
      Alcotest.test_case "insert_or_abort_error"        `Quick test_insert_or_abort_error;
    ];
    "returning", [
      Alcotest.test_case "insert_returning"        `Quick test_insert_returning;
      Alcotest.test_case "update_returning"        `Quick test_update_returning;
      Alcotest.test_case "delete_returning"        `Quick test_delete_returning;
      Alcotest.test_case "update_returning_multi"  `Quick test_update_returning_multi;
      Alcotest.test_case "delete_returning_multi"  `Quick test_delete_returning_multi;
      Alcotest.test_case "insert_returning_ignored" `Quick test_insert_returning_ignored;
    ];
    "alter_table", [
      Alcotest.test_case "add_column"                    `Quick test_alter_add_column;
      Alcotest.test_case "add_column_null_default"       `Quick test_alter_add_column_null_default;
      Alcotest.test_case "add_not_null_no_default_error" `Quick test_alter_add_not_null_no_default_error;
      Alcotest.test_case "rename_table"                  `Quick test_rename_table;
      Alcotest.test_case "rename_column"                 `Quick test_rename_column;
      Alcotest.test_case "rename_column_no_keyword"      `Quick test_rename_column_no_keyword;
      Alcotest.test_case "rename_nonexistent"            `Quick test_alter_rename_nonexistent;
      Alcotest.test_case "rename_column_nonexistent"     `Quick test_alter_rename_column_nonexistent;
      Alcotest.test_case "add_column_duplicate"          `Quick test_alter_add_column_duplicate;
      Alcotest.test_case "rename_table_to_existing"      `Quick test_alter_rename_table_to_existing;
      Alcotest.test_case "add_column_nonexistent_table"  `Quick test_alter_add_column_nonexistent_table;
    ];
    "table_constraints", [
      Alcotest.test_case "table_unique_constraint"      `Quick test_table_unique_constraint;
      Alcotest.test_case "table_primary_key_constraint" `Quick test_table_primary_key_constraint;
    ];
    "subqueries", [
      Alcotest.test_case "scalar_subquery"         `Quick test_scalar_subquery;
      Alcotest.test_case "scalar_subquery_null"    `Quick test_scalar_subquery_null;
      Alcotest.test_case "exists_true"             `Quick test_exists_subquery;
      Alcotest.test_case "exists_false"            `Quick test_exists_subquery_false;
      Alcotest.test_case "in_select"               `Quick test_in_select_subquery;
      Alcotest.test_case "not_in_select"           `Quick test_not_in_select_subquery;
    ];
    "fk_parse", [
      Alcotest.test_case "fk_references_col"     `Quick test_fk_parse_create;
      Alcotest.test_case "fk_references_no_col"  `Quick test_fk_parse_no_col;
    ];
    "check_constraints", [
      Alcotest.test_case "insert_ok"           `Quick test_check_insert_ok;
      Alcotest.test_case "insert_violation"    `Quick test_check_insert_violation;
      Alcotest.test_case "update_violation"    `Quick test_check_update_violation;
      Alcotest.test_case "null_allowed"        `Quick test_check_null_allowed;
      Alcotest.test_case "persisted"           `Quick test_check_persisted;
      Alcotest.test_case "cache_invalidation"  `Quick test_check_cache_invalidation;
    ];
    "phase9_edge", [
      Alcotest.test_case "const_select"          `Quick test_const_select;
      Alcotest.test_case "subquery_and_pred"     `Quick test_subquery_and_predicate;
      Alcotest.test_case "check_complex_expr"    `Quick test_check_complex_expr;
      Alcotest.test_case "check_function_expr"   `Quick test_check_function_in_expr;
      Alcotest.test_case "text_scalar_subquery"  `Quick test_text_scalar_subquery;
      Alcotest.test_case "real_scalar_subquery"  `Quick test_real_scalar_subquery;
      Alcotest.test_case "like_in_where"         `Quick test_like_in_where;
      Alcotest.test_case "modulo_operator"       `Quick test_modulo_operator;
      Alcotest.test_case "sum_real_with_nulls"   `Quick test_sum_real_with_nulls;
      Alcotest.test_case "avg_real_column"       `Quick test_avg_real_column;
      Alcotest.test_case "check_add_binop"       `Quick test_check_add_binop;
      Alcotest.test_case "check_concat_binop"    `Quick test_check_concat_binop;
      Alcotest.test_case "check_like_constraint" `Quick test_check_like_in_constraint;
      Alcotest.test_case "int_real_comparison"   `Quick test_int_real_comparison;
      Alcotest.test_case "real_ne_comparison"    `Quick test_real_ne_comparison;
      Alcotest.test_case "real_mod_int"          `Quick test_real_mod_int;
      Alcotest.test_case "alter_add_text_default"   `Quick test_alter_add_text_default;
      Alcotest.test_case "check_not_operator"        `Quick test_check_not_operator;
      Alcotest.test_case "check_between_in_check"    `Quick test_check_between_in_check;
      Alcotest.test_case "check_is_not_null_in_check" `Quick test_check_is_not_null_in_check;
    ];
    "string_coverage", [
      Alcotest.test_case "trim_all_whitespace"    `Quick test_trim_all_whitespace;
      Alcotest.test_case "trim_chars_all_removed" `Quick test_trim_chars_all_removed;
      Alcotest.test_case "replace_empty_old"      `Quick test_replace_empty_old;
    ];
  ]
