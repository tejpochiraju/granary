(** Tests for the Phase 2 rich expression language.

    Covers comparison operators, arithmetic, logical AND/OR/NOT,
    IS NULL / IS NOT NULL, unary minus, and qualified column
    references — all exercised both end-to-end (via SQL) and by
    direct invocation of [Exec.eval_expr] where useful. *)

open Lwt.Syntax
module Db   = Sqlocaml.Db
module Row  = Sqlocaml_encoding.Row
module Ast  = Sqlocaml_sql.Ast
module Plan = Sqlocaml_sql.Plan
module Exec = Sqlocaml_sql.Exec

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
     | Error _ -> Alcotest.failf "exec failed: %s" sql);
    Lwt.return_unit
  )

let query_ok db sql =
  run (
    let* result = Db.query db sql in
    match result with
    | Error _ -> Alcotest.failf "query failed: %s" sql
    | Ok stream -> Lwt_stream.to_list stream
  )

(** Return the integer value of column 0 in [row], or fail. *)
let int_of_row0 row =
  match row.(0) with
  | Db.V_int n -> Int64.to_int n
  | _ -> Alcotest.failf "expected V_int in col 0"

(** Structural equality on [Row.value] without polymorphic equality —
    bytes/strings are compared by [Bytes.equal] / [String.equal] and
    floats by their bit pattern (so NaN equals NaN here, but that
    isn't exercised by the eval tests below). *)
let value_eq (a : Row.value) (b : Row.value) : bool =
  match a, b with
  | Row.V_int  x, Row.V_int  y -> Int64.equal x y
  | Row.V_text x, Row.V_text y -> String.equal x y
  | Row.V_null,   Row.V_null   -> true
  | Row.V_real x, Row.V_real y ->
    Int64.equal (Int64.bits_of_float x) (Int64.bits_of_float y)
  | Row.V_blob x, Row.V_blob y -> Bytes.equal x y
  | _                          -> false

(** Helper: build a table [t (n INTEGER, s TEXT)] populated with the
    five rows (1,'a'), (2,'b'), (3,NULL), (4,'d'), (5,NULL).  Used by
    almost every test below. *)
let seed_t_with_n_s db =
  exec db "CREATE TABLE t (n INTEGER, s TEXT)";
  exec db "INSERT INTO t (n, s) VALUES (1, 'a')";
  exec db "INSERT INTO t (n, s) VALUES (2, 'b')";
  exec db "INSERT INTO t (n)    VALUES (3)";
  exec db "INSERT INTO t (n, s) VALUES (4, 'd')";
  exec db "INSERT INTO t (n)    VALUES (5)"

(* ------------------------------------------------------------------ *)
(* Group 1: Comparison operators                                        *)
(* ------------------------------------------------------------------ *)

let where_gt () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n > 2" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "n > 2 → [3;4;5]" [3;4;5] ns

let where_ge () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n >= 4" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "n >= 4 → [4;5]" [4;5] ns

let where_lt () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n < 3" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "n < 3 → [1;2]" [1;2] ns

let where_le () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n <= 2" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "n <= 2 → [1;2]" [1;2] ns

let where_ne_bang () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n != 3" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "n != 3 → [1;2;4;5]" [1;2;4;5] ns

let where_ne_angle () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n <> 3" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "n <> 3 → [1;2;4;5]" [1;2;4;5] ns

(* ------------------------------------------------------------------ *)
(* Group 2: Logical AND / OR / NOT                                      *)
(* ------------------------------------------------------------------ *)

let where_and_range () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n >= 2 AND n <= 4" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "2 <= n <= 4 → [2;3;4]" [2;3;4] ns

let where_or_outside_range () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n < 2 OR n > 4" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "n<2 OR n>4 → [1;5]" [1;5] ns

let where_not () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE NOT (n = 3)" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "NOT(n=3) → [1;2;4;5]" [1;2;4;5] ns

(* ------------------------------------------------------------------ *)
(* Group 3: IS NULL / IS NOT NULL                                       *)
(* ------------------------------------------------------------------ *)

let where_is_null () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE s IS NULL" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "s IS NULL → [3;5]" [3;5] ns

let where_is_not_null () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE s IS NOT NULL" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "s IS NOT NULL → [1;2;4]" [1;2;4] ns

(* ------------------------------------------------------------------ *)
(* Group 4: NULL = NULL is never true                                   *)
(* ------------------------------------------------------------------ *)

let where_null_eq_null () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE NULL = NULL" in
  Alcotest.(check int) "NULL = NULL → 0 rows" 0 (List.length rows)

let where_null_ne_null () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE NULL != NULL" in
  Alcotest.(check int) "NULL != NULL → 0 rows" 0 (List.length rows)

let where_s_eq_null_no_rows () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE s = NULL" in
  Alcotest.(check int) "s = NULL → 0 rows (use IS NULL)" 0 (List.length rows)

let where_one_eq_one () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE 1 = 1" in
  Alcotest.(check int) "1 = 1 → all 5 rows" 5 (List.length rows)

(* ------------------------------------------------------------------ *)
(* Group 5: Arithmetic in WHERE                                         *)
(* ------------------------------------------------------------------ *)

let where_arith_add () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n + 1 = 6" in
  let ns = List.map int_of_row0 rows in
  Alcotest.(check (list int)) "n + 1 = 6 → [5]" [5] ns

let where_arith_sub () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n - 1 = 2" in
  let ns = List.map int_of_row0 rows in
  Alcotest.(check (list int)) "n - 1 = 2 → [3]" [3] ns

let where_arith_mul () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n * 2 = 6" in
  let ns = List.map int_of_row0 rows in
  Alcotest.(check (list int)) "n * 2 = 6 → [3]" [3] ns

let where_arith_div () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n / 2 = 2" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  (* integer division: 4/2=2, 5/2=2 *)
  Alcotest.(check (list int)) "n / 2 = 2 → [4;5]" [4;5] ns

let where_unary_minus () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE -n = -3" in
  let ns = List.map int_of_row0 rows in
  Alcotest.(check (list int)) "-n = -3 → [3]" [3] ns

let where_arith_precedence () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  (* 1 + 2 * 3 = 7 → only n = 1 satisfies (n + 2 * n) = 1+2=3? Let me test 2+3=5 *)
  let rows = query_ok db "SELECT n FROM t WHERE 1 + 2 * n = 7" in
  (* 1 + 2*n = 7 → n = 3 *)
  let ns = List.map int_of_row0 rows in
  Alcotest.(check (list int)) "1 + 2*n = 7 → [3]" [3] ns

let where_arith_parens () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  (* (1 + 2) * n = 9 → n = 3 *)
  let rows = query_ok db "SELECT n FROM t WHERE (1 + 2) * n = 9" in
  let ns = List.map int_of_row0 rows in
  Alcotest.(check (list int)) "(1+2)*n=9 → [3]" [3] ns

(* ------------------------------------------------------------------ *)
(* Group 6: Qualified column reference (table.col)                      *)
(* ------------------------------------------------------------------ *)

let where_qualified_col () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  (* Phase 2 Task 1: single-table queries — t.n resolves to column n. *)
  let rows = query_ok db "SELECT n FROM t WHERE t.n = 2" in
  let ns = List.map int_of_row0 rows in
  Alcotest.(check (list int)) "t.n = 2 → [2]" [2] ns

(* ------------------------------------------------------------------ *)
(* Group 7: Direct eval_expr smoke tests                                *)
(* ------------------------------------------------------------------ *)

let eval_eq () =
  let row = [| Row.V_int 5L; Row.V_text "hi" |] in
  let e = Plan.P_binop (Plan.Eq, Plan.P_col 0, Plan.P_lit (Ast.L_int 5L)) in
  Alcotest.(check bool) "5 = 5 → V_int 1"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 1L))

(* Ne text, real, blob — covers exec.ml lines 100-103 *)
let eval_ne_text () =
  let row = [| Row.V_text "abc" |] in
  let e = Plan.P_binop (Plan.Ne, Plan.P_col 0, Plan.P_lit (Ast.L_text "xyz")) in
  Alcotest.(check bool) "abc != xyz → V_int 1"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 1L))

let eval_ne_text_equal () =
  let row = [| Row.V_text "same" |] in
  let e = Plan.P_binop (Plan.Ne, Plan.P_col 0, Plan.P_lit (Ast.L_text "same")) in
  Alcotest.(check bool) "same != same → V_int 0"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 0L))

let eval_ne_real () =
  let row = [| Row.V_real 1.0 |] in
  let e = Plan.P_binop (Plan.Ne, Plan.P_col 0, Plan.P_lit (Ast.L_real 2.0)) in
  Alcotest.(check bool) "1.0 != 2.0 → V_int 1"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 1L))

let eval_ne_blob () =
  let row = [| Row.V_blob (Bytes.of_string "a") |] in
  let e = Plan.P_binop (Plan.Ne, Plan.P_col 0, Plan.P_lit (Ast.L_blob (Bytes.of_string "b"))) in
  Alcotest.(check bool) "a != b → V_int 1"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 1L))

(* cmp_result for text/real/blob — covers exec.ml lines 120-124 *)
let eval_lt_text () =
  let row = [| Row.V_text "apple" |] in
  let e = Plan.P_binop (Plan.Lt, Plan.P_col 0, Plan.P_lit (Ast.L_text "banana")) in
  Alcotest.(check bool) "apple < banana → V_int 1"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 1L))

let eval_lt_real () =
  let row = [| Row.V_real 1.5 |] in
  let e = Plan.P_binop (Plan.Lt, Plan.P_col 0, Plan.P_lit (Ast.L_real 2.5)) in
  Alcotest.(check bool) "1.5 < 2.5 → V_int 1"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 1L))

let eval_lt_blob () =
  let row = [| Row.V_blob (Bytes.of_string "a") |] in
  let e = Plan.P_binop (Plan.Lt, Plan.P_col 0, Plan.P_lit (Ast.L_blob (Bytes.of_string "b"))) in
  Alcotest.(check bool) "a-blob < b-blob → V_int 1"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 1L))

let eval_cmp_cross_type () =
  (* Cross-type comparison: int vs text — returns false (0) *)
  let row = [| Row.V_int 5L; Row.V_text "abc" |] in
  let e = Plan.P_binop (Plan.Lt, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool) "int < text (cross-type) → 0"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 0L))

(* arith_op real + int — covers exec.ml lines 131-133 *)
let eval_arith_real_int () =
  let row = [| Row.V_real 2.5 |] in
  let e = Plan.P_binop (Plan.Add, Plan.P_col 0, Plan.P_lit (Ast.L_int 1L)) in
  Alcotest.(check bool) "2.5 + 1 → V_real 3.5"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_real 3.5))

let eval_compare_values_cross_type () =
  (* compare_values cross-type (non-null): int vs text → 0 (line 35 of exec.ml) *)
  (* We can test this via Eq which uses compare_values internally, or via a sort *)
  (* Actually compare_values is also called in sort — use a direct approach via eval_binop *)
  (* For cross-type cmp_result: text vs int → 0 *)
  let row = [| Row.V_text "x"; Row.V_int 5L |] in
  let e = Plan.P_binop (Plan.Gt, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool) "text > int (cross-type) → 0"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 0L))

let eval_arith_text_error () =
  (* arith_op: text + int → failwith (line 133 of exec.ml) *)
  let row = [| Row.V_text "a"; Row.V_int 5L |] in
  let e = Plan.P_binop (Plan.Add, Plan.P_col 0, Plan.P_col 1) in
  (try
     let _ = Exec.eval_expr [||] row e in
     Alcotest.fail "expected arithmetic error on text + int"
   with Failure _ -> ())

let eval_ne_null_left () =
  (* V_null on left of Ne → Row.V_null (3VL: comparisons with NULL return NULL) *)
  let row = [| Row.V_null |] in
  let e = Plan.P_binop (Plan.Ne, Plan.P_col 0, Plan.P_lit (Ast.L_int 5L)) in
  Alcotest.(check bool) "null != 5 → V_null"
    true (value_eq (Exec.eval_expr [||] row e) Row.V_null)

let eval_neq () =
  let row = [| Row.V_int 5L |] in
  let e = Plan.P_binop (Plan.Ne, Plan.P_col 0, Plan.P_lit (Ast.L_int 6L)) in
  Alcotest.(check bool) "5 != 6 → V_int 1"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 1L))

let eval_lt () =
  let row = [| Row.V_int 3L |] in
  let e = Plan.P_binop (Plan.Lt, Plan.P_col 0, Plan.P_lit (Ast.L_int 5L)) in
  Alcotest.(check bool) "3 < 5 → V_int 1"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 1L))

let eval_arith_add () =
  let row = [| Row.V_int 3L |] in
  let e = Plan.P_binop (Plan.Add, Plan.P_col 0, Plan.P_lit (Ast.L_int 4L)) in
  Alcotest.(check bool) "3 + 4 → V_int 7"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 7L))

let eval_arith_add_real () =
  let row = [| Row.V_real 1.5 |] in
  let e = Plan.P_binop (Plan.Add, Plan.P_col 0, Plan.P_lit (Ast.L_real 2.5)) in
  Alcotest.(check bool) "1.5 + 2.5 → V_real 4.0"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_real 4.0))

let eval_arith_mixed_int_real () =
  let row = [| Row.V_int 3L |] in
  let e = Plan.P_binop (Plan.Add, Plan.P_col 0, Plan.P_lit (Ast.L_real 0.5)) in
  Alcotest.(check bool) "3 + 0.5 → V_real 3.5"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_real 3.5))

let eval_arith_null () =
  let row = [| Row.V_null |] in
  let e = Plan.P_binop (Plan.Add, Plan.P_col 0, Plan.P_lit (Ast.L_int 1L)) in
  Alcotest.(check bool) "NULL + 1 → V_null"
    true (value_eq (Exec.eval_expr [||] row e) Row.V_null)

let eval_neg_int () =
  let row = [| Row.V_int 7L |] in
  let e = Plan.P_neg (Plan.P_col 0) in
  Alcotest.(check bool) "-7 → V_int -7"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int (-7L)))

let eval_neg_real () =
  let row = [| Row.V_real 2.0 |] in
  let e = Plan.P_neg (Plan.P_col 0) in
  Alcotest.(check bool) "-2.0 → V_real -2.0"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_real (-2.0)))

let eval_neg_null () =
  let row = [| Row.V_null |] in
  let e = Plan.P_neg (Plan.P_col 0) in
  Alcotest.(check bool) "-NULL → V_null"
    true (value_eq (Exec.eval_expr [||] row e) Row.V_null)

let eval_neg_text_raises () =
  let row = [| Row.V_text "x" |] in
  let e = Plan.P_neg (Plan.P_col 0) in
  (try
     let _ = Exec.eval_expr [||] row e in
     Alcotest.fail "expected failure on negating TEXT"
   with Failure _ -> ())

let eval_is_null_true () =
  let row = [| Row.V_null |] in
  let e = Plan.P_is_null (Plan.P_col 0) in
  Alcotest.(check bool) "NULL IS NULL → V_int 1"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 1L))

let eval_is_null_false () =
  let row = [| Row.V_int 1L |] in
  let e = Plan.P_is_null (Plan.P_col 0) in
  Alcotest.(check bool) "1 IS NULL → V_int 0"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 0L))

let eval_is_not_null_true () =
  let row = [| Row.V_int 1L |] in
  let e = Plan.P_is_not_null (Plan.P_col 0) in
  Alcotest.(check bool) "1 IS NOT NULL → V_int 1"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 1L))

let eval_not_true () =
  let row = [| Row.V_int 0L |] in
  let e = Plan.P_not (Plan.P_col 0) in
  Alcotest.(check bool) "NOT 0 → V_int 1"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 1L))

let eval_not_null () =
  (* NOT NULL → V_null (3VL: NOT of unknown is unknown) *)
  let row = [| Row.V_null |] in
  let e = Plan.P_not (Plan.P_col 0) in
  Alcotest.(check bool) "NOT NULL → V_null"
    true (value_eq (Exec.eval_expr [||] row e) Row.V_null)

let eval_div_zero_raises () =
  let row = [| Row.V_int 5L |] in
  let e = Plan.P_binop (Plan.Div, Plan.P_col 0, Plan.P_lit (Ast.L_int 0L)) in
  (try
     let _ = Exec.eval_expr [||] row e in
     Alcotest.fail "expected division-by-zero failure"
   with Failure _ -> ())

let eval_and () =
  let row = [| Row.V_int 1L; Row.V_int 0L |] in
  let e = Plan.P_binop (Plan.And, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool) "1 AND 0 → V_int 0"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 0L))

let eval_or () =
  let row = [| Row.V_int 1L; Row.V_int 0L |] in
  let e = Plan.P_binop (Plan.Or, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool) "1 OR 0 → V_int 1"
    true (value_eq (Exec.eval_expr [||] row e) (Row.V_int 1L))

let eval_cmp_null () =
  (* NULL < 1 → V_null (3VL: comparisons with NULL return NULL) *)
  let row = [| Row.V_null |] in
  let e = Plan.P_binop (Plan.Lt, Plan.P_col 0, Plan.P_lit (Ast.L_int 1L)) in
  Alcotest.(check bool) "NULL < 1 → V_null"
    true (value_eq (Exec.eval_expr [||] row e) Row.V_null)

(* ------------------------------------------------------------------ *)
(* Group 8: QCheck property-based tests                                 *)
(* ------------------------------------------------------------------ *)

(** Build a fresh table populated with [values] (one integer per row),
    then execute [sql] and return the rows. *)
let setup_one_col_int values =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  List.iter (fun n ->
    exec db (Printf.sprintf "INSERT INTO t (n) VALUES (%d)" n)
  ) values;
  db

let prop_lt_filter =
  QCheck.Test.make ~count:10_000 ~name:"WHERE n < k matches all rows with n<k"
    (let gen_int = QCheck.int_range (-100) 100 in
     QCheck.pair (QCheck.list_size (QCheck.Gen.int_range 0 8) gen_int) gen_int)
    (fun (values, k) ->
      let db = setup_one_col_int values in
      let sql = Printf.sprintf "SELECT n FROM t WHERE n < %d" k in
      let rows = query_ok db sql in
      let got = List.sort compare (List.map int_of_row0 rows) in
      let expected = List.sort compare (List.filter (fun n -> n < k) values) in
      got = expected)

let prop_ge_filter =
  QCheck.Test.make ~count:10_000 ~name:"WHERE n >= k matches all rows with n>=k"
    (let gen_int = QCheck.int_range (-100) 100 in
     QCheck.pair (QCheck.list_size (QCheck.Gen.int_range 0 8) gen_int) gen_int)
    (fun (values, k) ->
      let db = setup_one_col_int values in
      let sql = Printf.sprintf "SELECT n FROM t WHERE n >= %d" k in
      let rows = query_ok db sql in
      let got = List.sort compare (List.map int_of_row0 rows) in
      let expected = List.sort compare (List.filter (fun n -> n >= k) values) in
      got = expected)

let prop_ne_filter =
  QCheck.Test.make ~count:10_000 ~name:"WHERE n != k matches all rows where n<>k"
    (let gen_int = QCheck.int_range (-50) 50 in
     QCheck.pair (QCheck.list_size (QCheck.Gen.int_range 0 8) gen_int) gen_int)
    (fun (values, k) ->
      let db = setup_one_col_int values in
      let sql = Printf.sprintf "SELECT n FROM t WHERE n != %d" k in
      let rows = query_ok db sql in
      let got = List.sort compare (List.map int_of_row0 rows) in
      let expected = List.sort compare (List.filter (fun n -> n <> k) values) in
      got = expected)

let prop_and_intersection =
  QCheck.Test.make ~count:10_000 ~name:"WHERE (n >= a) AND (n <= b) is intersection"
    (let gen_int = QCheck.int_range (-50) 50 in
     QCheck.triple (QCheck.list_size (QCheck.Gen.int_range 0 8) gen_int) gen_int gen_int)
    (fun (values, a, b) ->
      let db = setup_one_col_int values in
      let sql = Printf.sprintf "SELECT n FROM t WHERE n >= %d AND n <= %d" a b in
      let rows = query_ok db sql in
      let got = List.sort compare (List.map int_of_row0 rows) in
      let expected = List.sort compare
        (List.filter (fun n -> n >= a && n <= b) values) in
      got = expected)

let prop_or_union =
  QCheck.Test.make ~count:10_000 ~name:"WHERE (n < a) OR (n > b) is union"
    (let gen_int = QCheck.int_range (-50) 50 in
     QCheck.triple (QCheck.list_size (QCheck.Gen.int_range 0 8) gen_int) gen_int gen_int)
    (fun (values, a, b) ->
      let db = setup_one_col_int values in
      let sql = Printf.sprintf "SELECT n FROM t WHERE n < %d OR n > %d" a b in
      let rows = query_ok db sql in
      let got = List.sort compare (List.map int_of_row0 rows) in
      let expected = List.sort compare
        (List.filter (fun n -> n < a || n > b) values) in
      got = expected)

let prop_not_complement =
  QCheck.Test.make ~count:10_000 ~name:"WHERE NOT (n = k) is complement of n=k"
    (let gen_int = QCheck.int_range (-50) 50 in
     QCheck.pair (QCheck.list_size (QCheck.Gen.int_range 0 8) gen_int) gen_int)
    (fun (values, k) ->
      let db = setup_one_col_int values in
      let sql = Printf.sprintf "SELECT n FROM t WHERE NOT (n = %d)" k in
      let rows = query_ok db sql in
      let got = List.sort compare (List.map int_of_row0 rows) in
      let expected = List.sort compare (List.filter (fun n -> n <> k) values) in
      got = expected)

let prop_arith_add =
  QCheck.Test.make ~count:10_000 ~name:"WHERE n + k = (n+k) for each row"
    (let gen_int = QCheck.int_range (-50) 50 in
     QCheck.pair gen_int gen_int)
    (fun (start, k) ->
      let values = [start; start + 1; start + 2] in
      let db = setup_one_col_int values in
      let target = start + k in
      let sql = Printf.sprintf "SELECT n FROM t WHERE n + %d = %d" k target in
      let rows = query_ok db sql in
      let got = List.map int_of_row0 rows in
      got = [start])

(* ------------------------------------------------------------------ *)
(* Runner                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  let qcheck_tests = List.map QCheck_alcotest.to_alcotest [
    prop_lt_filter;
    prop_ge_filter;
    prop_ne_filter;
    prop_and_intersection;
    prop_or_union;
    prop_not_complement;
    prop_arith_add;
  ] in
  Alcotest.run "Expr" [
    "comparison", [
      Alcotest.test_case "where_gt"        `Quick where_gt;
      Alcotest.test_case "where_ge"        `Quick where_ge;
      Alcotest.test_case "where_lt"        `Quick where_lt;
      Alcotest.test_case "where_le"        `Quick where_le;
      Alcotest.test_case "where_ne_bang"   `Quick where_ne_bang;
      Alcotest.test_case "where_ne_angle"  `Quick where_ne_angle;
    ];
    "logical", [
      Alcotest.test_case "where_and_range"        `Quick where_and_range;
      Alcotest.test_case "where_or_outside_range" `Quick where_or_outside_range;
      Alcotest.test_case "where_not"              `Quick where_not;
    ];
    "null", [
      Alcotest.test_case "where_is_null"           `Quick where_is_null;
      Alcotest.test_case "where_is_not_null"       `Quick where_is_not_null;
      Alcotest.test_case "where_null_eq_null"      `Quick where_null_eq_null;
      Alcotest.test_case "where_null_ne_null"      `Quick where_null_ne_null;
      Alcotest.test_case "where_s_eq_null_no_rows" `Quick where_s_eq_null_no_rows;
      Alcotest.test_case "where_one_eq_one"        `Quick where_one_eq_one;
    ];
    "arithmetic", [
      Alcotest.test_case "where_arith_add"        `Quick where_arith_add;
      Alcotest.test_case "where_arith_sub"        `Quick where_arith_sub;
      Alcotest.test_case "where_arith_mul"        `Quick where_arith_mul;
      Alcotest.test_case "where_arith_div"        `Quick where_arith_div;
      Alcotest.test_case "where_unary_minus"      `Quick where_unary_minus;
      Alcotest.test_case "where_arith_precedence" `Quick where_arith_precedence;
      Alcotest.test_case "where_arith_parens"     `Quick where_arith_parens;
    ];
    "qualified", [
      Alcotest.test_case "where_qualified_col" `Quick where_qualified_col;
    ];
    "eval_expr", [
      Alcotest.test_case "eval_eq"                 `Quick eval_eq;
      Alcotest.test_case "eval_neq"                `Quick eval_neq;
      Alcotest.test_case "eval_lt"                 `Quick eval_lt;
      Alcotest.test_case "eval_arith_add"          `Quick eval_arith_add;
      Alcotest.test_case "eval_arith_add_real"     `Quick eval_arith_add_real;
      Alcotest.test_case "eval_arith_mixed_int_real" `Quick eval_arith_mixed_int_real;
      Alcotest.test_case "eval_arith_null"         `Quick eval_arith_null;
      Alcotest.test_case "eval_neg_int"            `Quick eval_neg_int;
      Alcotest.test_case "eval_neg_real"           `Quick eval_neg_real;
      Alcotest.test_case "eval_neg_null"           `Quick eval_neg_null;
      Alcotest.test_case "eval_neg_text_raises"    `Quick eval_neg_text_raises;
      Alcotest.test_case "eval_is_null_true"       `Quick eval_is_null_true;
      Alcotest.test_case "eval_is_null_false"      `Quick eval_is_null_false;
      Alcotest.test_case "eval_is_not_null_true"   `Quick eval_is_not_null_true;
      Alcotest.test_case "eval_not_true"           `Quick eval_not_true;
      Alcotest.test_case "eval_not_null"            `Quick eval_not_null;
      Alcotest.test_case "eval_div_zero_raises"    `Quick eval_div_zero_raises;
      Alcotest.test_case "eval_and"                `Quick eval_and;
      Alcotest.test_case "eval_or"                 `Quick eval_or;
      Alcotest.test_case "eval_cmp_null"           `Quick eval_cmp_null;
      Alcotest.test_case "eval_ne_text"             `Quick eval_ne_text;
      Alcotest.test_case "eval_ne_text_equal"       `Quick eval_ne_text_equal;
      Alcotest.test_case "eval_ne_real"             `Quick eval_ne_real;
      Alcotest.test_case "eval_ne_blob"             `Quick eval_ne_blob;
      Alcotest.test_case "eval_lt_text"             `Quick eval_lt_text;
      Alcotest.test_case "eval_lt_real"             `Quick eval_lt_real;
      Alcotest.test_case "eval_lt_blob"             `Quick eval_lt_blob;
      Alcotest.test_case "eval_cmp_cross_type"      `Quick eval_cmp_cross_type;
      Alcotest.test_case "eval_arith_real_int"      `Quick eval_arith_real_int;
      Alcotest.test_case "eval_compare_values_cross_type" `Quick eval_compare_values_cross_type;
      Alcotest.test_case "eval_arith_text_error"          `Quick eval_arith_text_error;
      Alcotest.test_case "eval_ne_null_left"              `Quick eval_ne_null_left;
    ];
    "qcheck", qcheck_tests;
  ]
