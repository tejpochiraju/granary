open Lwt.Syntax
module Db  = Sqlocaml.Db
module Row = Sqlocaml_encoding.Row

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let run = Lwt_main.run

let fresh_db () = run (Db.open_in_memory ())

let exec db sql =
  run (
    let* result = Db.execute db sql in
    (match result with
     | Ok () -> ()
     | Error _ -> Alcotest.failf "exec: unexpected error for: %s" sql);
    Lwt.return_unit
  )

let exec_err db sql =
  run (
    let* result = Db.execute db sql in
    (match result with
     | Ok () -> Alcotest.failf "exec_err: expected Error for: %s" sql
     | Error e -> Lwt.return e)
  )

let query_ok db sql =
  run (
    let* result = Db.query db sql in
    match result with
    | Error _ -> Alcotest.failf "query_ok: unexpected error for: %s" sql
    | Ok stream ->
      Lwt_stream.to_list stream
  )

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

let int_of_v = function
  | Db.V_int n -> Int64.to_int n
  | _ -> -1

let text_of_v = function
  | Db.V_text s -> s
  | _ -> ""

(* ------------------------------------------------------------------ *)
(* Group 1: Basic UPDATE semantics                                      *)
(* ------------------------------------------------------------------ *)

let update_all_no_where () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  exec db "INSERT INTO t (id, n) VALUES (2, 20)";
  exec db "INSERT INTO t (id, n) VALUES (3, 30)";
  exec db "UPDATE t SET n = 99";
  let rows = query_ok db "SELECT n FROM t" in
  Alcotest.(check int) "3 rows" 3 (List.length rows);
  let ns = List.map (fun r -> int_of_v r.(0)) rows in
  Alcotest.(check (list int)) "all 99" [99; 99; 99] ns

let update_with_where () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  exec db "INSERT INTO t (id, n) VALUES (2, 20)";
  exec db "INSERT INTO t (id, n) VALUES (3, 30)";
  exec db "UPDATE t SET n = 99 WHERE id = 2";
  let rows = query_ok db "SELECT id, n FROM t ORDER BY id ASC" in
  let pairs = List.map (fun r -> (int_of_v r.(0), int_of_v r.(1))) rows in
  Alcotest.(check (list (pair int int))) "rows" [(1, 10); (2, 99); (3, 30)] pairs

let update_text_column_with_filter () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER, s TEXT)";
  exec db "INSERT INTO t (id, n, s) VALUES (1, 1, 'a')";
  exec db "INSERT INTO t (id, n, s) VALUES (2, 5, 'b')";
  exec db "INSERT INTO t (id, n, s) VALUES (3, 7, 'c')";
  exec db "UPDATE t SET s = 'new' WHERE n > 3";
  let rows = query_ok db "SELECT id, s FROM t ORDER BY id ASC" in
  let pairs = List.map (fun r -> (int_of_v r.(0), text_of_v r.(1))) rows in
  Alcotest.(check (list (pair int string)))
    "rows" [(1, "a"); (2, "new"); (3, "new")] pairs

let update_expression_self_reference () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "INSERT INTO t (n) VALUES (3)";
  exec db "UPDATE t SET n = n + 10";
  let rows = query_ok db "SELECT n FROM t" in
  let ns = List.map (fun r -> int_of_v r.(0)) rows in
  Alcotest.(check (list int)) "incremented" [11; 12; 13] ns

let update_to_null () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, s TEXT)";
  exec db "INSERT INTO t (id, s) VALUES (1, 'a')";
  exec db "INSERT INTO t (id, s) VALUES (2, 'b')";
  exec db "INSERT INTO t (id, s) VALUES (3, 'c')";
  exec db "UPDATE t SET s = NULL";
  let rows = query_ok db "SELECT s FROM t" in
  Alcotest.(check int) "3 rows" 3 (List.length rows);
  List.iter (fun r ->
    Alcotest.check value_testable "NULL" Db.V_null r.(0)
  ) rows

let update_multiple_assignments () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER, s TEXT)";
  exec db "INSERT INTO t (id, n, s) VALUES (1, 1, 'old')";
  exec db "INSERT INTO t (id, n, s) VALUES (2, 2, 'old')";
  exec db "UPDATE t SET n = 100, s = 'new' WHERE id = 1";
  let rows = query_ok db "SELECT id, n, s FROM t ORDER BY id ASC" in
  let triples = List.map (fun r ->
    (int_of_v r.(0), int_of_v r.(1), text_of_v r.(2))) rows in
  Alcotest.(check (list (triple int int string)))
    "rows" [(1, 100, "new"); (2, 2, "old")] triples

let update_where_no_match () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  exec db "INSERT INTO t (id, n) VALUES (2, 20)";
  exec db "UPDATE t SET n = 99 WHERE id = 999";
  let rows = query_ok db "SELECT id, n FROM t ORDER BY id ASC" in
  let pairs = List.map (fun r -> (int_of_v r.(0), int_of_v r.(1))) rows in
  Alcotest.(check (list (pair int int))) "unchanged" [(1, 10); (2, 20)] pairs

let update_with_complex_where () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  exec db "INSERT INTO t (id, n) VALUES (2, 20)";
  exec db "INSERT INTO t (id, n) VALUES (3, 30)";
  exec db "INSERT INTO t (id, n) VALUES (4, 40)";
  exec db "UPDATE t SET n = 0 WHERE n > 15 AND n < 35";
  let rows = query_ok db "SELECT id, n FROM t ORDER BY id ASC" in
  let pairs = List.map (fun r -> (int_of_v r.(0), int_of_v r.(1))) rows in
  Alcotest.(check (list (pair int int)))
    "rows" [(1, 10); (2, 0); (3, 0); (4, 40)] pairs

let update_empty_table () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "UPDATE t SET n = 99";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "0 rows" 0 (List.length rows)

(* ------------------------------------------------------------------ *)
(* Group 2: Semantic errors                                              *)
(* ------------------------------------------------------------------ *)

let update_unknown_table () =
  let db = fresh_db () in
  let result = run (Db.execute db "UPDATE ghost SET n = 1") in
  match result with
  | Error (Db.Sema (Sqlocaml_sql.Sema.Unknown_table tbl)) ->
    Alcotest.(check string) "table name" "ghost" tbl
  | _ -> Alcotest.fail "expected Sema(Unknown_table \"ghost\")"

let update_unknown_column () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  let e = exec_err db "UPDATE t SET bad_col = 1" in
  match e with
  | Db.Sema (Sqlocaml_sql.Sema.Unknown_column _) -> ()
  | _ -> Alcotest.fail "expected Sema(Unknown_column)"

let update_unknown_column_in_where () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  let e = exec_err db "UPDATE t SET n = 1 WHERE bogus = 2" in
  match e with
  | Db.Sema (Sqlocaml_sql.Sema.Unknown_column _) -> ()
  | _ -> Alcotest.fail "expected Sema(Unknown_column)"

let update_type_mismatch_int_text () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  let e = exec_err db "UPDATE t SET n = 'text'" in
  match e with
  | Db.Sema (Sqlocaml_sql.Sema.Type_mismatch _) -> ()
  | _ -> Alcotest.fail "expected Sema(Type_mismatch)"

let update_type_mismatch_text_int () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT)";
  let e = exec_err db "UPDATE t SET s = 42" in
  match e with
  | Db.Sema (Sqlocaml_sql.Sema.Type_mismatch _) -> ()
  | _ -> Alcotest.fail "expected Sema(Type_mismatch)"

let update_null_is_allowed_on_int () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  (* NULL is accepted for any column. *)
  exec db "UPDATE t SET n = NULL";
  let rows = query_ok db "SELECT n FROM t" in
  Alcotest.check value_testable "n = NULL" Db.V_null (List.hd rows).(0)

(* ------------------------------------------------------------------ *)
(* Group 3: UPDATE via query is a runtime error                          *)
(* ------------------------------------------------------------------ *)

let update_via_query_returns_runtime_error () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  let result = run (Db.query db "UPDATE t SET n = 99") in
  match result with
  | Error (Db.Runtime _) -> ()
  | _ -> Alcotest.fail "expected Runtime error for UPDATE via query"

(* ------------------------------------------------------------------ *)
(* Group 4: Index interactions                                          *)
(* ------------------------------------------------------------------ *)

let update_with_unique_index_violation () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  exec db "INSERT INTO t (id, n) VALUES (2, 20)";
  exec db "CREATE UNIQUE INDEX idx ON t (n)";
  (* Attempt to set n=20 on id=1, conflicting with id=2's n=20. *)
  let result = run (Db.execute db "UPDATE t SET n = 20 WHERE id = 1") in
  (match result with
   | Error (Db.Runtime _) -> ()
   | _ -> Alcotest.fail "expected Runtime error for UNIQUE violation");
  (* The row should not have been modified. *)
  let rows = query_ok db "SELECT id, n FROM t ORDER BY id ASC" in
  let pairs = List.map (fun r -> (int_of_v r.(0), int_of_v r.(1))) rows in
  Alcotest.(check (list (pair int int)))
    "unchanged" [(1, 10); (2, 20)] pairs

let update_with_unique_index_to_same_value_ok () =
  (* UPDATE that doesn't change the indexed value should not raise. *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER, s TEXT)";
  exec db "INSERT INTO t (id, n, s) VALUES (1, 10, 'a')";
  exec db "INSERT INTO t (id, n, s) VALUES (2, 20, 'b')";
  exec db "CREATE UNIQUE INDEX idx ON t (n)";
  (* Set the same value back — should be fine. *)
  exec db "UPDATE t SET s = 'X' WHERE id = 1";
  let rows = query_ok db "SELECT id, n, s FROM t ORDER BY id ASC" in
  let triples = List.map (fun r ->
    (int_of_v r.(0), int_of_v r.(1), text_of_v r.(2))) rows in
  Alcotest.(check (list (triple int int string)))
    "rows" [(1, 10, "X"); (2, 20, "b")] triples

let update_with_unique_index_new_value_ok () =
  (* Updating to a distinct new value must succeed. *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  exec db "INSERT INTO t (id, n) VALUES (2, 20)";
  exec db "CREATE UNIQUE INDEX idx ON t (n)";
  exec db "UPDATE t SET n = 30 WHERE id = 1";
  let rows = query_ok db "SELECT id, n FROM t ORDER BY id ASC" in
  let pairs = List.map (fun r -> (int_of_v r.(0), int_of_v r.(1))) rows in
  Alcotest.(check (list (pair int int)))
    "rows" [(1, 30); (2, 20)] pairs;
  (* The updated row must be findable via the index. *)
  let lookup = query_ok db "SELECT id FROM t WHERE n = 30" in
  Alcotest.(check int) "1 match for n=30" 1 (List.length lookup);
  Alcotest.check value_testable "id=1" (Db.V_int 1L) (List.hd lookup).(0);
  let lookup_old = query_ok db "SELECT id FROM t WHERE n = 10" in
  Alcotest.(check int) "0 match for n=10 (old value gone)"
    0 (List.length lookup_old)

(* Update TEXT/REAL/BLOB unique-indexed column to same value — exercises
   the unchanged check in execute_update (exec.ml lines 354-359). *)
let update_text_unique_same_value () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, s TEXT)";
  exec db "INSERT INTO t (id, s) VALUES (1, 'hello')";
  exec db "INSERT INTO t (id, s) VALUES (2, 'world')";
  exec db "CREATE UNIQUE INDEX idx ON t (s)";
  (* Update s to the same value — unchanged check hits V_text arm *)
  exec db "UPDATE t SET s = 'hello' WHERE id = 1";
  let rows = query_ok db "SELECT id FROM t WHERE s = 'hello'" in
  Alcotest.(check int) "still 1 row for s='hello'" 1 (List.length rows)

let update_real_unique_same_value () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, f REAL)";
  exec db "INSERT INTO t (id, f) VALUES (1, 3.14)";
  exec db "CREATE UNIQUE INDEX idx ON t (f)";
  (* Update f to the same value — unchanged check hits V_real arm *)
  exec db "UPDATE t SET f = 3.14 WHERE id = 1";
  let rows = query_ok db "SELECT id FROM t" in
  Alcotest.(check int) "row still present" 1 (List.length rows)

let update_real_unique_new_value () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, f REAL)";
  exec db "INSERT INTO t (id, f) VALUES (1, 1.0)";
  exec db "INSERT INTO t (id, f) VALUES (2, 2.0)";
  exec db "CREATE UNIQUE INDEX idx ON t (f)";
  (* Update to a different real value — not unchanged *)
  exec db "UPDATE t SET f = 9.9 WHERE id = 1";
  let rows = query_ok db "SELECT id FROM t" in
  Alcotest.(check int) "both rows still present" 2 (List.length rows)

let update_null_unchanged () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 5)";
  exec db "CREATE UNIQUE INDEX idx ON t (n)";
  (* Set n to NULL then update to same NULL — hits V_null/V_null arm *)
  exec db "UPDATE t SET n = NULL WHERE id = 1";
  exec db "UPDATE t SET n = NULL WHERE id = 1";
  let rows = query_ok db "SELECT id FROM t" in
  Alcotest.(check int) "row still present after null→null" 1 (List.length rows)

let update_text_unique_new_value () =
  (* Update to a different TEXT value in a unique index — not unchanged, checks no violation *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, s TEXT)";
  exec db "INSERT INTO t (id, s) VALUES (1, 'hello')";
  exec db "INSERT INTO t (id, s) VALUES (2, 'world')";
  exec db "CREATE UNIQUE INDEX idx ON t (s)";
  exec db "UPDATE t SET s = 'newval' WHERE id = 1";
  let rows = query_ok db "SELECT id FROM t WHERE s = 'newval'" in
  Alcotest.(check int) "updated row found by new text value" 1 (List.length rows)

let update_cross_type_unchanged () =
  (* Test where old_v and new_v have different types (the `_` arm) *)
  (* This can happen if a column value somehow has a different type than expected *)
  (* In practice, the `_` arm in unchanged check is hit when old_v is e.g. null
     and new_v is a non-null value of different type *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 5)";
  exec db "CREATE UNIQUE INDEX idx ON t (n)";
  (* NULL to non-null: old_v = V_null, new_v = V_int 10 — hits `_` arm *)
  exec db "UPDATE t SET n = NULL WHERE id = 1";
  exec db "UPDATE t SET n = 10 WHERE id = 1";
  let rows = query_ok db "SELECT id FROM t" in
  Alcotest.(check int) "row present after null→int update" 1 (List.length rows)

let update_with_non_unique_index_lookup () =
  (* Verify that index entries are correctly updated for non-unique
     indexes — the new value should be findable, the old value should
     not be (for the moved row). *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  exec db "INSERT INTO t (id, n) VALUES (2, 10)";
  exec db "INSERT INTO t (id, n) VALUES (3, 20)";
  exec db "CREATE INDEX idx ON t (n)";
  exec db "UPDATE t SET n = 99 WHERE id = 1";
  let found_99 = query_ok db "SELECT id FROM t WHERE n = 99" in
  Alcotest.(check int) "1 match for n=99" 1 (List.length found_99);
  Alcotest.check value_testable "id=1" (Db.V_int 1L) (List.hd found_99).(0);
  let found_10 = query_ok db "SELECT id FROM t WHERE n = 10" in
  Alcotest.(check int) "1 match for n=10 (only id=2)"
    1 (List.length found_10);
  Alcotest.check value_testable "id=2" (Db.V_int 2L) (List.hd found_10).(0)

(* ------------------------------------------------------------------ *)
(* Group 5: Rows-affected count                                          *)
(* ------------------------------------------------------------------ *)

let update_returns_count () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  exec db "INSERT INTO t (id, n) VALUES (2, 20)";
  exec db "INSERT INTO t (id, n) VALUES (3, 30)";
  exec db "INSERT INTO t (id, n) VALUES (4, 40)";
  let result = run (Db.execute_change_count db "UPDATE t SET n = 0 WHERE n > 15") in
  match result with
  | Ok n -> Alcotest.(check int) "3 rows affected" 3 n
  | Error _ -> Alcotest.fail "expected Ok count"

let update_returns_zero_when_no_match () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  let result = run (Db.execute_change_count db "UPDATE t SET n = 0 WHERE id = 999") in
  match result with
  | Ok n -> Alcotest.(check int) "0 rows" 0 n
  | Error _ -> Alcotest.fail "expected Ok 0"

let update_all_returns_total_count () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  for i = 1 to 7 do
    exec db (Printf.sprintf "INSERT INTO t (n) VALUES (%d)" i)
  done;
  let result = run (Db.execute_change_count db "UPDATE t SET n = 0") in
  match result with
  | Ok n -> Alcotest.(check int) "7 rows" 7 n
  | Error _ -> Alcotest.fail "expected Ok 7"

(* ------------------------------------------------------------------ *)
(* Group 6: Real and Blob columns                                        *)
(* ------------------------------------------------------------------ *)

let update_real_column () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, x REAL)";
  exec db "INSERT INTO t (id, x) VALUES (1, 1.0)";
  exec db "INSERT INTO t (id, x) VALUES (2, 2.0)";
  exec db "UPDATE t SET x = 99.5 WHERE id = 2";
  let rows = query_ok db "SELECT id, x FROM t ORDER BY id ASC" in
  let pairs = List.map (fun r -> (int_of_v r.(0), r.(1))) rows in
  match pairs with
  | [(1, x1); (2, x2)] ->
    Alcotest.check value_testable "row 1 unchanged" (Db.V_real 1.0) x1;
    Alcotest.check value_testable "row 2 updated"   (Db.V_real 99.5) x2
  | _ -> Alcotest.fail "expected 2 rows"

(* ------------------------------------------------------------------ *)
(* Group 7: QCheck                                                       *)
(* ------------------------------------------------------------------ *)

let qcheck_update_doubles_values =
  QCheck.Test.make
    ~name:"UPDATE SET n = n * 2: every value is doubled"
    ~count:10_000
    QCheck.(list_size Gen.(0 -- 15) (int_range (-100) 100))
    (fun ns ->
       let db = Lwt_main.run (Db.open_in_memory ()) in
       Lwt_main.run (
         let* _ = Db.execute db "CREATE TABLE t (n INTEGER)" in
         let* () = Lwt_list.iter_s (fun n ->
           let sql = Printf.sprintf "INSERT INTO t (n) VALUES (%d)" n in
           let* _ = Db.execute db sql in
           Lwt.return_unit
         ) ns in
         let* _ = Db.execute db "UPDATE t SET n = n * 2" in
         let* result = Db.query db "SELECT n FROM t" in
         match result with
         | Error _ -> Lwt.return false
         | Ok stream ->
           let* rows = Lwt_stream.to_list stream in
           let got = List.map (fun r -> match r.(0) with
             | Row.V_int x -> Int64.to_int x
             | _ -> 0) rows in
           let expected = List.map (fun n -> n * 2) ns in
           Lwt.return (List.length got = List.length expected
                       && got = expected)
       ))

let qcheck_update_count_matches =
  QCheck.Test.make
    ~name:"UPDATE WHERE n > k: count matches number of matching rows"
    ~count:10_000
    QCheck.(pair (list_size Gen.(0 -- 15) (int_range 0 50)) (int_range 0 50))
    (fun (ns, k) ->
       let db = Lwt_main.run (Db.open_in_memory ()) in
       Lwt_main.run (
         let* _ = Db.execute db "CREATE TABLE t (n INTEGER)" in
         let* () = Lwt_list.iter_s (fun n ->
           let sql = Printf.sprintf "INSERT INTO t (n) VALUES (%d)" n in
           let* _ = Db.execute db sql in
           Lwt.return_unit
         ) ns in
         let sql = Printf.sprintf "UPDATE t SET n = -1 WHERE n > %d" k in
         let* result = Db.execute_change_count db sql in
         match result with
         | Error _ -> Lwt.return false
         | Ok got_count ->
           let expected = List.length (List.filter (fun n -> n > k) ns) in
           Lwt.return (got_count = expected)
       ))

(* ------------------------------------------------------------------ *)
(* Group 8: Known limitations                                           *)
(* ------------------------------------------------------------------ *)

(* sqlocaml checks UNIQUE constraints against committed (pre-update) index
   state before applying any mutations.  This means a single UPDATE that
   "swaps" two unique values across rows is rejected, even though the final
   state would be valid: when the check runs for the first row being updated
   (n=1 → n=-1), the committed index still contains -1 (owned by the second
   row), so sqlocaml raises a UNIQUE violation.

   This is a known Phase 2 limitation: a correct implementation would defer
   the constraint check until all row mutations have been applied (or remove
   old index entries before checking).  For now, callers must work around
   this by updating through an intermediate value that doesn't collide. *)
let update_unique_swap_rejected () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, n INTEGER)" in
    let* _ = Db.execute db "CREATE UNIQUE INDEX idx ON t(n)" in
    let* _ = Db.execute db "INSERT INTO t (id, n) VALUES (1, 1)" in
    let* _ = Db.execute db "INSERT INTO t (id, n) VALUES (2, -1)" in
    (* Attempt to negate all n values: row 1: 1 -> -1, row 2: -1 -> 1.
       Logically this is a pure swap and the final state would satisfy
       UNIQUE(n), but sqlocaml rejects it because -1 already exists in the
       committed index when the pre-update check runs for row 1. *)
    let* result = Db.execute db "UPDATE t SET n = 0 - n" in
    (* Expect a UNIQUE violation Runtime error — not Ok. *)
    Alcotest.(check bool) "swap rejected due to committed-state UNIQUE check" true
      (match result with Error (Db.Runtime _) -> true | _ -> false);
    Db.close db
  )

(* ------------------------------------------------------------------ *)
(* Runner                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Update" [
    "basic", [
      Alcotest.test_case "update_all_no_where"            `Quick update_all_no_where;
      Alcotest.test_case "update_with_where"              `Quick update_with_where;
      Alcotest.test_case "update_text_column_with_filter" `Quick update_text_column_with_filter;
      Alcotest.test_case "update_expression_self_ref"     `Quick update_expression_self_reference;
      Alcotest.test_case "update_to_null"                 `Quick update_to_null;
      Alcotest.test_case "update_multiple_assignments"    `Quick update_multiple_assignments;
      Alcotest.test_case "update_where_no_match"          `Quick update_where_no_match;
      Alcotest.test_case "update_with_complex_where"      `Quick update_with_complex_where;
      Alcotest.test_case "update_empty_table"             `Quick update_empty_table;
    ];
    "sema_errors", [
      Alcotest.test_case "unknown_table"           `Quick update_unknown_table;
      Alcotest.test_case "unknown_column"          `Quick update_unknown_column;
      Alcotest.test_case "unknown_column_in_where" `Quick update_unknown_column_in_where;
      Alcotest.test_case "type_mismatch_int_text"  `Quick update_type_mismatch_int_text;
      Alcotest.test_case "type_mismatch_text_int"  `Quick update_type_mismatch_text_int;
      Alcotest.test_case "null_is_allowed_on_int"  `Quick update_null_is_allowed_on_int;
    ];
    "runtime_routing", [
      Alcotest.test_case "update_via_query_is_runtime_error"
        `Quick update_via_query_returns_runtime_error;
    ];
    "indexes", [
      Alcotest.test_case "unique_index_violation"        `Quick update_with_unique_index_violation;
      Alcotest.test_case "unique_index_same_value_ok"    `Quick update_with_unique_index_to_same_value_ok;
      Alcotest.test_case "unique_index_new_value_ok"     `Quick update_with_unique_index_new_value_ok;
      Alcotest.test_case "non_unique_index_lookup"       `Quick update_with_non_unique_index_lookup;
      Alcotest.test_case "text_unique_same_value"        `Quick update_text_unique_same_value;
      Alcotest.test_case "real_unique_same_value"        `Quick update_real_unique_same_value;
      Alcotest.test_case "real_unique_new_value"         `Quick update_real_unique_new_value;
      Alcotest.test_case "null_unchanged"                `Quick update_null_unchanged;
      Alcotest.test_case "text_unique_new_value"          `Quick update_text_unique_new_value;
      Alcotest.test_case "cross_type_unchanged"          `Quick update_cross_type_unchanged;
    ];
    "row_count", [
      Alcotest.test_case "update_returns_count"             `Quick update_returns_count;
      Alcotest.test_case "update_returns_zero_when_no_match" `Quick update_returns_zero_when_no_match;
      Alcotest.test_case "update_all_returns_total_count"   `Quick update_all_returns_total_count;
    ];
    "real_column", [
      Alcotest.test_case "update_real_column" `Quick update_real_column;
    ];
    "qcheck", (
      List.map QCheck_alcotest.to_alcotest [
        qcheck_update_doubles_values;
        qcheck_update_count_matches;
      ]
    );
    "known_limitations", [
      (* This test documents the committed-state UNIQUE check limitation:
         a single UPDATE that swaps unique values across rows is incorrectly
         rejected.  The test asserts the *current* (buggy) behaviour so that
         any future fix will cause it to fail and prompt updating the test. *)
      Alcotest.test_case "update_unique_swap_rejected"
        `Quick update_unique_swap_rejected;
    ];
  ]
