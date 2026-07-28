(** Tests for the Phase 2 rich expression language.

    Covers comparison operators, arithmetic, logical AND/OR/NOT,
    IS NULL / IS NOT NULL, unary minus, and qualified column
    references — all exercised both end-to-end (via SQL) and by
    direct invocation of [Exec.eval_expr] where useful. *)

open Lwt.Syntax
module Db = Granary.Db
module Row = Granary_encoding.Row
module Ast = Granary_sql.Ast
module Plan = Granary_sql.Plan
module Exec = Granary_sql.Exec

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let run = Lwt_main.run
let fresh_db () = run (Db.open_in_memory ())

let exec db sql =
  run
    (let* result = Db.execute db sql in
     (match result with
      | Ok () -> ()
      | Error _ -> Alcotest.failf "exec failed: %s" sql);
     Lwt.return_unit)
;;

let query_ok db sql =
  run
    (let* result = Db.query db sql in
     match result with
     | Error _ -> Alcotest.failf "query failed: %s" sql
     | Ok stream -> Lwt_stream.to_list stream)
;;

(** Return the integer value of column 0 in [row], or fail. *)
let int_of_row0 row =
  match row.(0) with
  | Db.V_int n -> Int64.to_int n
  | _ -> Alcotest.failf "expected V_int in col 0"
;;

(** Structural equality on [Row.value] without polymorphic equality —
    bytes/strings are compared by [Bytes.equal] / [String.equal] and
    floats by their bit pattern (so NaN equals NaN here, but that
    isn't exercised by the eval tests below). *)
let value_eq (a : Row.value) (b : Row.value) : bool =
  match a, b with
  | Row.V_int x, Row.V_int y -> Int64.equal x y
  | Row.V_text x, Row.V_text y -> String.equal x y
  | Row.V_null, Row.V_null -> true
  | Row.V_real x, Row.V_real y ->
    Int64.equal (Int64.bits_of_float x) (Int64.bits_of_float y)
  | Row.V_blob x, Row.V_blob y -> Bytes.equal x y
  | _ -> false
;;

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
;;

(* ------------------------------------------------------------------ *)
(* Group 1: Comparison operators                                        *)
(* ------------------------------------------------------------------ *)

let where_gt () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n > 2" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "n > 2 → [3;4;5]" [ 3; 4; 5 ] ns
;;

let where_ge () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n >= 4" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "n >= 4 → [4;5]" [ 4; 5 ] ns
;;

let where_lt () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n < 3" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "n < 3 → [1;2]" [ 1; 2 ] ns
;;

let where_le () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n <= 2" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "n <= 2 → [1;2]" [ 1; 2 ] ns
;;

let where_ne_bang () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n != 3" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "n != 3 → [1;2;4;5]" [ 1; 2; 4; 5 ] ns
;;

let where_ne_angle () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n <> 3" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "n <> 3 → [1;2;4;5]" [ 1; 2; 4; 5 ] ns
;;

(* ------------------------------------------------------------------ *)
(* Group 2: Logical AND / OR / NOT                                      *)
(* ------------------------------------------------------------------ *)

let where_and_range () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n >= 2 AND n <= 4" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "2 <= n <= 4 → [2;3;4]" [ 2; 3; 4 ] ns
;;

let where_or_outside_range () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n < 2 OR n > 4" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "n<2 OR n>4 → [1;5]" [ 1; 5 ] ns
;;

let where_not () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE NOT (n = 3)" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "NOT(n=3) → [1;2;4;5]" [ 1; 2; 4; 5 ] ns
;;

(* ------------------------------------------------------------------ *)
(* Group 3: IS NULL / IS NOT NULL                                       *)
(* ------------------------------------------------------------------ *)

let where_is_null () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE s IS NULL" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "s IS NULL → [3;5]" [ 3; 5 ] ns
;;

let where_is_not_null () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE s IS NOT NULL" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  Alcotest.(check (list int)) "s IS NOT NULL → [1;2;4]" [ 1; 2; 4 ] ns
;;

(* ------------------------------------------------------------------ *)
(* Group 4: NULL = NULL is never true                                   *)
(* ------------------------------------------------------------------ *)

let where_null_eq_null () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE NULL = NULL" in
  Alcotest.(check int) "NULL = NULL → 0 rows" 0 (List.length rows)
;;

let where_null_ne_null () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE NULL != NULL" in
  Alcotest.(check int) "NULL != NULL → 0 rows" 0 (List.length rows)
;;

let where_s_eq_null_no_rows () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE s = NULL" in
  Alcotest.(check int) "s = NULL → 0 rows (use IS NULL)" 0 (List.length rows)
;;

let where_one_eq_one () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE 1 = 1" in
  Alcotest.(check int) "1 = 1 → all 5 rows" 5 (List.length rows)
;;

(* ------------------------------------------------------------------ *)
(* Group 5: Arithmetic in WHERE                                         *)
(* ------------------------------------------------------------------ *)

let where_arith_add () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n + 1 = 6" in
  let ns = List.map int_of_row0 rows in
  Alcotest.(check (list int)) "n + 1 = 6 → [5]" [ 5 ] ns
;;

let where_arith_sub () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n - 1 = 2" in
  let ns = List.map int_of_row0 rows in
  Alcotest.(check (list int)) "n - 1 = 2 → [3]" [ 3 ] ns
;;

let where_arith_mul () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n * 2 = 6" in
  let ns = List.map int_of_row0 rows in
  Alcotest.(check (list int)) "n * 2 = 6 → [3]" [ 3 ] ns
;;

let where_arith_div () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE n / 2 = 2" in
  let ns = List.sort compare (List.map int_of_row0 rows) in
  (* integer division: 4/2=2, 5/2=2 *)
  Alcotest.(check (list int)) "n / 2 = 2 → [4;5]" [ 4; 5 ] ns
;;

let where_unary_minus () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  let rows = query_ok db "SELECT n FROM t WHERE -n = -3" in
  let ns = List.map int_of_row0 rows in
  Alcotest.(check (list int)) "-n = -3 → [3]" [ 3 ] ns
;;

let where_arith_precedence () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  (* 1 + 2 * 3 = 7 → only n = 1 satisfies (n + 2 * n) = 1+2=3? Let me test 2+3=5 *)
  let rows = query_ok db "SELECT n FROM t WHERE 1 + 2 * n = 7" in
  (* 1 + 2*n = 7 → n = 3 *)
  let ns = List.map int_of_row0 rows in
  Alcotest.(check (list int)) "1 + 2*n = 7 → [3]" [ 3 ] ns
;;

let where_arith_parens () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  (* (1 + 2) * n = 9 → n = 3 *)
  let rows = query_ok db "SELECT n FROM t WHERE (1 + 2) * n = 9" in
  let ns = List.map int_of_row0 rows in
  Alcotest.(check (list int)) "(1+2)*n=9 → [3]" [ 3 ] ns
;;

(* ------------------------------------------------------------------ *)
(* Group 6: Qualified column reference (table.col)                      *)
(* ------------------------------------------------------------------ *)

let where_qualified_col () =
  let db = fresh_db () in
  seed_t_with_n_s db;
  (* Phase 2 Task 1: single-table queries — t.n resolves to column n. *)
  let rows = query_ok db "SELECT n FROM t WHERE t.n = 2" in
  let ns = List.map int_of_row0 rows in
  Alcotest.(check (list int)) "t.n = 2 → [2]" [ 2 ] ns
;;

(* ------------------------------------------------------------------ *)
(* Group 7: Direct eval_expr smoke tests                                *)
(* ------------------------------------------------------------------ *)

let eval_eq () =
  let row = [| Row.V_int 5L; Row.V_text "hi" |] in
  let e = Plan.P_binop (Plan.Eq, Plan.P_col 0, Plan.P_lit (Ast.L_int 5L)) in
  Alcotest.(check bool)
    "5 = 5 → V_int 1"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 1L))
;;

(* Ne text, real, blob — covers exec.ml lines 100-103 *)
let eval_ne_text () =
  let row = [| Row.V_text "abc" |] in
  let e = Plan.P_binop (Plan.Ne, Plan.P_col 0, Plan.P_lit (Ast.L_text "xyz")) in
  Alcotest.(check bool)
    "abc != xyz → V_int 1"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 1L))
;;

let eval_ne_text_equal () =
  let row = [| Row.V_text "same" |] in
  let e = Plan.P_binop (Plan.Ne, Plan.P_col 0, Plan.P_lit (Ast.L_text "same")) in
  Alcotest.(check bool)
    "same != same → V_int 0"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 0L))
;;

let eval_ne_real () =
  let row = [| Row.V_real 1.0 |] in
  let e = Plan.P_binop (Plan.Ne, Plan.P_col 0, Plan.P_lit (Ast.L_real 2.0)) in
  Alcotest.(check bool)
    "1.0 != 2.0 → V_int 1"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 1L))
;;

let eval_ne_blob () =
  let row = [| Row.V_blob (Bytes.of_string "a") |] in
  let e =
    Plan.P_binop (Plan.Ne, Plan.P_col 0, Plan.P_lit (Ast.L_blob (Bytes.of_string "b")))
  in
  Alcotest.(check bool)
    "a != b → V_int 1"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 1L))
;;

(* cmp_result for text/real/blob — covers exec.ml lines 120-124 *)
let eval_lt_text () =
  let row = [| Row.V_text "apple" |] in
  let e = Plan.P_binop (Plan.Lt, Plan.P_col 0, Plan.P_lit (Ast.L_text "banana")) in
  Alcotest.(check bool)
    "apple < banana → V_int 1"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 1L))
;;

let eval_lt_real () =
  let row = [| Row.V_real 1.5 |] in
  let e = Plan.P_binop (Plan.Lt, Plan.P_col 0, Plan.P_lit (Ast.L_real 2.5)) in
  Alcotest.(check bool)
    "1.5 < 2.5 → V_int 1"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 1L))
;;

let eval_lt_blob () =
  let row = [| Row.V_blob (Bytes.of_string "a") |] in
  let e =
    Plan.P_binop (Plan.Lt, Plan.P_col 0, Plan.P_lit (Ast.L_blob (Bytes.of_string "b")))
  in
  Alcotest.(check bool)
    "a-blob < b-blob → V_int 1"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 1L))
;;

let eval_cmp_cross_type () =
  (* Cross-type comparison: int vs text — returns false (0) *)
  let row = [| Row.V_int 5L; Row.V_text "abc" |] in
  let e = Plan.P_binop (Plan.Lt, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "int < text (cross-type) → 0"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 0L))
;;

(* arith_op real + int — covers exec.ml lines 131-133 *)
let eval_arith_real_int () =
  let row = [| Row.V_real 2.5 |] in
  let e = Plan.P_binop (Plan.Add, Plan.P_col 0, Plan.P_lit (Ast.L_int 1L)) in
  Alcotest.(check bool)
    "2.5 + 1 → V_real 3.5"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_real 3.5))
;;

let eval_compare_values_cross_type () =
  (* compare_values cross-type (non-null): int vs text → 0 (line 35 of exec.ml) *)
  (* We can test this via Eq which uses compare_values internally, or via a sort *)
  (* Actually compare_values is also called in sort — use a direct approach via eval_binop *)
  (* For cross-type cmp_result: text vs int → 0 *)
  let row = [| Row.V_text "x"; Row.V_int 5L |] in
  let e = Plan.P_binop (Plan.Gt, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "text > int (cross-type) → 0"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 0L))
;;

let eval_arith_text_error () =
  (* arith_op: text + int → failwith (line 133 of exec.ml) *)
  let row = [| Row.V_text "a"; Row.V_int 5L |] in
  let e = Plan.P_binop (Plan.Add, Plan.P_col 0, Plan.P_col 1) in
  try
    let _ = Exec.eval_expr None [||] row e in
    Alcotest.fail "expected arithmetic error on text + int"
  with
  | Failure _ -> ()
;;

let eval_ne_null_left () =
  (* V_null on left of Ne → Row.V_null (3VL: comparisons with NULL return NULL) *)
  let row = [| Row.V_null |] in
  let e = Plan.P_binop (Plan.Ne, Plan.P_col 0, Plan.P_lit (Ast.L_int 5L)) in
  Alcotest.(check bool)
    "null != 5 → V_null"
    true
    (value_eq (Exec.eval_expr None [||] row e) Row.V_null)
;;

let eval_neq () =
  let row = [| Row.V_int 5L |] in
  let e = Plan.P_binop (Plan.Ne, Plan.P_col 0, Plan.P_lit (Ast.L_int 6L)) in
  Alcotest.(check bool)
    "5 != 6 → V_int 1"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 1L))
;;

let eval_lt () =
  let row = [| Row.V_int 3L |] in
  let e = Plan.P_binop (Plan.Lt, Plan.P_col 0, Plan.P_lit (Ast.L_int 5L)) in
  Alcotest.(check bool)
    "3 < 5 → V_int 1"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 1L))
;;

let eval_arith_add () =
  let row = [| Row.V_int 3L |] in
  let e = Plan.P_binop (Plan.Add, Plan.P_col 0, Plan.P_lit (Ast.L_int 4L)) in
  Alcotest.(check bool)
    "3 + 4 → V_int 7"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 7L))
;;

let eval_arith_add_real () =
  let row = [| Row.V_real 1.5 |] in
  let e = Plan.P_binop (Plan.Add, Plan.P_col 0, Plan.P_lit (Ast.L_real 2.5)) in
  Alcotest.(check bool)
    "1.5 + 2.5 → V_real 4.0"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_real 4.0))
;;

let eval_arith_mixed_int_real () =
  let row = [| Row.V_int 3L |] in
  let e = Plan.P_binop (Plan.Add, Plan.P_col 0, Plan.P_lit (Ast.L_real 0.5)) in
  Alcotest.(check bool)
    "3 + 0.5 → V_real 3.5"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_real 3.5))
;;

let eval_arith_null () =
  let row = [| Row.V_null |] in
  let e = Plan.P_binop (Plan.Add, Plan.P_col 0, Plan.P_lit (Ast.L_int 1L)) in
  Alcotest.(check bool)
    "NULL + 1 → V_null"
    true
    (value_eq (Exec.eval_expr None [||] row e) Row.V_null)
;;

let eval_neg_int () =
  let row = [| Row.V_int 7L |] in
  let e = Plan.P_neg (Plan.P_col 0) in
  Alcotest.(check bool)
    "-7 → V_int -7"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int (-7L)))
;;

let eval_neg_real () =
  let row = [| Row.V_real 2.0 |] in
  let e = Plan.P_neg (Plan.P_col 0) in
  Alcotest.(check bool)
    "-2.0 → V_real -2.0"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_real (-2.0)))
;;

let eval_neg_null () =
  let row = [| Row.V_null |] in
  let e = Plan.P_neg (Plan.P_col 0) in
  Alcotest.(check bool)
    "-NULL → V_null"
    true
    (value_eq (Exec.eval_expr None [||] row e) Row.V_null)
;;

let eval_neg_text_raises () =
  let row = [| Row.V_text "x" |] in
  let e = Plan.P_neg (Plan.P_col 0) in
  try
    let _ = Exec.eval_expr None [||] row e in
    Alcotest.fail "expected failure on negating TEXT"
  with
  | Failure _ -> ()
;;

let eval_is_null_true () =
  let row = [| Row.V_null |] in
  let e = Plan.P_is_null (Plan.P_col 0) in
  Alcotest.(check bool)
    "NULL IS NULL → V_int 1"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 1L))
;;

let eval_is_null_false () =
  let row = [| Row.V_int 1L |] in
  let e = Plan.P_is_null (Plan.P_col 0) in
  Alcotest.(check bool)
    "1 IS NULL → V_int 0"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 0L))
;;

let eval_is_not_null_true () =
  let row = [| Row.V_int 1L |] in
  let e = Plan.P_is_not_null (Plan.P_col 0) in
  Alcotest.(check bool)
    "1 IS NOT NULL → V_int 1"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 1L))
;;

let eval_not_true () =
  let row = [| Row.V_int 0L |] in
  let e = Plan.P_not (Plan.P_col 0) in
  Alcotest.(check bool)
    "NOT 0 → V_int 1"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 1L))
;;

let eval_not_null () =
  (* NOT NULL → V_null (3VL: NOT of unknown is unknown) *)
  let row = [| Row.V_null |] in
  let e = Plan.P_not (Plan.P_col 0) in
  Alcotest.(check bool)
    "NOT NULL → V_null"
    true
    (value_eq (Exec.eval_expr None [||] row e) Row.V_null)
;;

let eval_div_zero_raises () =
  let row = [| Row.V_int 5L |] in
  let e = Plan.P_binop (Plan.Div, Plan.P_col 0, Plan.P_lit (Ast.L_int 0L)) in
  try
    let _ = Exec.eval_expr None [||] row e in
    Alcotest.fail "expected division-by-zero failure"
  with
  | Failure _ -> ()
;;

let eval_and () =
  let row = [| Row.V_int 1L; Row.V_int 0L |] in
  let e = Plan.P_binop (Plan.And, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "1 AND 0 → V_int 0"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 0L))
;;

let eval_or () =
  let row = [| Row.V_int 1L; Row.V_int 0L |] in
  let e = Plan.P_binop (Plan.Or, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "1 OR 0 → V_int 1"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 1L))
;;

let eval_cmp_null () =
  (* NULL < 1 → V_null (3VL: comparisons with NULL return NULL) *)
  let row = [| Row.V_null |] in
  let e = Plan.P_binop (Plan.Lt, Plan.P_col 0, Plan.P_lit (Ast.L_int 1L)) in
  Alcotest.(check bool)
    "NULL < 1 → V_null"
    true
    (value_eq (Exec.eval_expr None [||] row e) Row.V_null)
;;

(* ------------------------------------------------------------------ *)
(* Group 8: QCheck property-based tests                                 *)
(* ------------------------------------------------------------------ *)

(** Build a fresh table populated with [values] (one integer per row),
    then execute [sql] and return the rows. *)
let setup_one_col_int values =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  List.iter (fun n -> exec db (Printf.sprintf "INSERT INTO t (n) VALUES (%d)" n)) values;
  db
;;

let prop_lt_filter =
  QCheck.Test.make
    ~count:10_000
    ~name:"WHERE n < k matches all rows with n<k"
    (let gen_int = QCheck.int_range (-100) 100 in
     QCheck.pair (QCheck.list_size (QCheck.Gen.int_range 0 8) gen_int) gen_int)
    (fun (values, k) ->
       let db = setup_one_col_int values in
       let sql = Printf.sprintf "SELECT n FROM t WHERE n < %d" k in
       let rows = query_ok db sql in
       let got = List.sort compare (List.map int_of_row0 rows) in
       let expected = List.sort compare (List.filter (fun n -> n < k) values) in
       got = expected)
;;

let prop_ge_filter =
  QCheck.Test.make
    ~count:10_000
    ~name:"WHERE n >= k matches all rows with n>=k"
    (let gen_int = QCheck.int_range (-100) 100 in
     QCheck.pair (QCheck.list_size (QCheck.Gen.int_range 0 8) gen_int) gen_int)
    (fun (values, k) ->
       let db = setup_one_col_int values in
       let sql = Printf.sprintf "SELECT n FROM t WHERE n >= %d" k in
       let rows = query_ok db sql in
       let got = List.sort compare (List.map int_of_row0 rows) in
       let expected = List.sort compare (List.filter (fun n -> n >= k) values) in
       got = expected)
;;

let prop_ne_filter =
  QCheck.Test.make
    ~count:10_000
    ~name:"WHERE n != k matches all rows where n<>k"
    (let gen_int = QCheck.int_range (-50) 50 in
     QCheck.pair (QCheck.list_size (QCheck.Gen.int_range 0 8) gen_int) gen_int)
    (fun (values, k) ->
       let db = setup_one_col_int values in
       let sql = Printf.sprintf "SELECT n FROM t WHERE n != %d" k in
       let rows = query_ok db sql in
       let got = List.sort compare (List.map int_of_row0 rows) in
       let expected = List.sort compare (List.filter (fun n -> n <> k) values) in
       got = expected)
;;

let prop_and_intersection =
  QCheck.Test.make
    ~count:10_000
    ~name:"WHERE (n >= a) AND (n <= b) is intersection"
    (let gen_int = QCheck.int_range (-50) 50 in
     QCheck.triple (QCheck.list_size (QCheck.Gen.int_range 0 8) gen_int) gen_int gen_int)
    (fun (values, a, b) ->
       let db = setup_one_col_int values in
       let sql = Printf.sprintf "SELECT n FROM t WHERE n >= %d AND n <= %d" a b in
       let rows = query_ok db sql in
       let got = List.sort compare (List.map int_of_row0 rows) in
       let expected =
         List.sort compare (List.filter (fun n -> n >= a && n <= b) values)
       in
       got = expected)
;;

let prop_or_union =
  QCheck.Test.make
    ~count:10_000
    ~name:"WHERE (n < a) OR (n > b) is union"
    (let gen_int = QCheck.int_range (-50) 50 in
     QCheck.triple (QCheck.list_size (QCheck.Gen.int_range 0 8) gen_int) gen_int gen_int)
    (fun (values, a, b) ->
       let db = setup_one_col_int values in
       let sql = Printf.sprintf "SELECT n FROM t WHERE n < %d OR n > %d" a b in
       let rows = query_ok db sql in
       let got = List.sort compare (List.map int_of_row0 rows) in
       let expected = List.sort compare (List.filter (fun n -> n < a || n > b) values) in
       got = expected)
;;

let prop_not_complement =
  QCheck.Test.make
    ~count:10_000
    ~name:"WHERE NOT (n = k) is complement of n=k"
    (let gen_int = QCheck.int_range (-50) 50 in
     QCheck.pair (QCheck.list_size (QCheck.Gen.int_range 0 8) gen_int) gen_int)
    (fun (values, k) ->
       let db = setup_one_col_int values in
       let sql = Printf.sprintf "SELECT n FROM t WHERE NOT (n = %d)" k in
       let rows = query_ok db sql in
       let got = List.sort compare (List.map int_of_row0 rows) in
       let expected = List.sort compare (List.filter (fun n -> n <> k) values) in
       got = expected)
;;

let prop_arith_add =
  QCheck.Test.make
    ~count:10_000
    ~name:"WHERE n + k = (n+k) for each row"
    (let gen_int = QCheck.int_range (-50) 50 in
     QCheck.pair gen_int gen_int)
    (fun (start, k) ->
       let values = [ start; start + 1; start + 2 ] in
       let db = setup_one_col_int values in
       let target = start + k in
       let sql = Printf.sprintf "SELECT n FROM t WHERE n + %d = %d" k target in
       let rows = query_ok db sql in
       let got = List.map int_of_row0 rows in
       got = [ start ])
;;

(* ------------------------------------------------------------------ *)
(* Group 9: Extended binary operators                                    *)
(* ------------------------------------------------------------------ *)

let test_concat () =
  let row = [||] in
  let v =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_binop
         (Plan.Concat, Plan.P_lit (Ast.L_text "foo"), Plan.P_lit (Ast.L_text "bar")))
  in
  Alcotest.(check string)
    "concat"
    "foobar"
    (match v with
     | Row.V_text s -> s
     | _ -> "?")
;;

let test_mod () =
  let row = [||] in
  let v =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_binop (Plan.Mod, Plan.P_lit (Ast.L_int 7L), Plan.P_lit (Ast.L_int 3L)))
  in
  Alcotest.(check int64)
    "mod"
    1L
    (match v with
     | Row.V_int n -> n
     | _ -> -1L)
;;

let test_bit_and () =
  let row = [||] in
  let v =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_binop (Plan.Bit_and, Plan.P_lit (Ast.L_int 5L), Plan.P_lit (Ast.L_int 3L)))
  in
  Alcotest.(check int64)
    "bit_and"
    1L
    (match v with
     | Row.V_int n -> n
     | _ -> -1L)
;;

let test_bit_or () =
  let row = [||] in
  let v =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_binop (Plan.Bit_or, Plan.P_lit (Ast.L_int 5L), Plan.P_lit (Ast.L_int 2L)))
  in
  Alcotest.(check int64)
    "bit_or"
    7L
    (match v with
     | Row.V_int n -> n
     | _ -> -1L)
;;

let test_lshift () =
  let row = [||] in
  let v =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_binop (Plan.Lshift, Plan.P_lit (Ast.L_int 2L), Plan.P_lit (Ast.L_int 3L)))
  in
  Alcotest.(check int64)
    "lshift"
    16L
    (match v with
     | Row.V_int n -> n
     | _ -> -1L)
;;

let test_rshift () =
  let row = [||] in
  let v =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_binop (Plan.Rshift, Plan.P_lit (Ast.L_int 16L), Plan.P_lit (Ast.L_int 2L)))
  in
  Alcotest.(check int64)
    "rshift"
    4L
    (match v with
     | Row.V_int n -> n
     | _ -> -1L)
;;

let test_bitnot () =
  let row = [||] in
  let v = Exec.eval_expr None [||] row (Plan.P_bitnot (Plan.P_lit (Ast.L_int 5L))) in
  Alcotest.(check int64)
    "bitnot"
    (-6L)
    (match v with
     | Row.V_int n -> n
     | _ -> 0L)
;;

let test_rshift_negative () =
  let row = [||] in
  let v =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_binop (Plan.Rshift, Plan.P_lit (Ast.L_int (-4L)), Plan.P_lit (Ast.L_int 1L)))
  in
  Alcotest.(check int64)
    "rshift_neg"
    (-2L)
    (match v with
     | Row.V_int n -> n
     | _ -> 0L)
;;

(* ------------------------------------------------------------------ *)
(* Group 10: LIKE and GLOB pattern matching                             *)
(* ------------------------------------------------------------------ *)

let test_like_match () =
  let row = [||] in
  let like a p =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_binop (Plan.Like, Plan.P_lit (Ast.L_text a), Plan.P_lit (Ast.L_text p)))
  in
  Alcotest.(check int64)
    "like percent"
    1L
    (match like "hello" "hel%" with
     | Row.V_int n -> n
     | _ -> -1L);
  Alcotest.(check int64)
    "like underscore"
    1L
    (match like "hello" "h_llo" with
     | Row.V_int n -> n
     | _ -> -1L);
  Alcotest.(check int64)
    "like case"
    1L
    (match like "Hello" "hello%" with
     | Row.V_int n -> n
     | _ -> -1L);
  Alcotest.(check int64)
    "like no match"
    0L
    (match like "hello" "world%" with
     | Row.V_int n -> n
     | _ -> -1L)
;;

let test_like_null () =
  let row = [||] in
  let v =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_binop (Plan.Like, Plan.P_lit Ast.L_null, Plan.P_lit (Ast.L_text "%")))
  in
  Alcotest.(check bool) "like null" true (v = Row.V_null)
;;

let test_glob_null () =
  let row = [||] in
  let v =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_binop (Plan.Glob, Plan.P_lit Ast.L_null, Plan.P_lit (Ast.L_text "*")))
  in
  Alcotest.(check bool) "glob null" true (v = Row.V_null)
;;

let test_glob_match () =
  let row = [||] in
  let glob s p =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_binop (Plan.Glob, Plan.P_lit (Ast.L_text s), Plan.P_lit (Ast.L_text p)))
  in
  Alcotest.(check int64)
    "glob star"
    1L
    (match glob "hello" "hel*" with
     | Row.V_int n -> n
     | _ -> -1L);
  Alcotest.(check int64)
    "glob q"
    1L
    (match glob "hello" "h?llo" with
     | Row.V_int n -> n
     | _ -> -1L);
  Alcotest.(check int64)
    "glob case"
    0L
    (match glob "Hello" "hello*" with
     | Row.V_int n -> n
     | _ -> -1L)
;;

(* ------------------------------------------------------------------ *)
(* Group 11: BETWEEN and IN operators                                   *)
(* ------------------------------------------------------------------ *)

let test_between () =
  let row = [||] in
  let between x lo hi =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_between
         (Plan.P_lit (Ast.L_int x), Plan.P_lit (Ast.L_int lo), Plan.P_lit (Ast.L_int hi)))
  in
  Alcotest.(check int64)
    "between in"
    1L
    (match between 5L 1L 10L with
     | Row.V_int n -> n
     | _ -> -1L);
  Alcotest.(check int64)
    "between lo"
    1L
    (match between 1L 1L 10L with
     | Row.V_int n -> n
     | _ -> -1L);
  Alcotest.(check int64)
    "between hi"
    1L
    (match between 10L 1L 10L with
     | Row.V_int n -> n
     | _ -> -1L);
  Alcotest.(check int64)
    "between out"
    0L
    (match between 0L 1L 10L with
     | Row.V_int n -> n
     | _ -> -1L)
;;

let test_in () =
  let row = [||] in
  let in_list x vs =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_in
         (Plan.P_lit (Ast.L_int x), List.map (fun v -> Plan.P_lit (Ast.L_int v)) vs))
  in
  Alcotest.(check int64)
    "in found"
    1L
    (match in_list 3L [ 1L; 2L; 3L ] with
     | Row.V_int n -> n
     | _ -> -1L);
  Alcotest.(check int64)
    "in not found"
    0L
    (match in_list 4L [ 1L; 2L; 3L ] with
     | Row.V_int n -> n
     | _ -> -1L)
;;

let test_in_null () =
  let row = [||] in
  let v =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_in (Plan.P_lit Ast.L_null, [ Plan.P_lit (Ast.L_int 1L) ]))
  in
  Alcotest.(check bool) "in null subject" true (v = Row.V_null)
;;

let test_in_null_in_list () =
  let row = [||] in
  (* x NOT IN (NULL, 2) when x=5: should be NULL because list has NULL *)
  let v_in =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_in
         (Plan.P_lit (Ast.L_int 5L), [ Plan.P_lit Ast.L_null; Plan.P_lit (Ast.L_int 2L) ]))
  in
  Alcotest.(check bool) "in with null in list" true (v_in = Row.V_null);
  (* x IN (NULL, 5) when x=5: found → 1 (NULL doesn't block a definite match) *)
  let v_in2 =
    Exec.eval_expr
      None
      [||]
      row
      (Plan.P_in
         (Plan.P_lit (Ast.L_int 5L), [ Plan.P_lit Ast.L_null; Plan.P_lit (Ast.L_int 5L) ]))
  in
  Alcotest.(check int64)
    "in with null and match"
    1L
    (match v_in2 with
     | Row.V_int n -> n
     | _ -> -1L)
;;

(* ------------------------------------------------------------------ *)
(* Group N: Additional binop coverage for uncovered exec.ml branches    *)
(* ------------------------------------------------------------------ *)

(** AND with null — one side is null, other is true → V_null (exec.ml line 364). *)
let eval_and_null_true () =
  let row = [| Row.V_null; Row.V_int 1L |] in
  let e = Plan.P_binop (Plan.And, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "NULL AND 1 → V_null"
    true
    (value_eq (Exec.eval_expr None [||] row e) Row.V_null)
;;

(** OR with null — one side is null, other is false → V_null (exec.ml line 370). *)
let eval_or_null_false () =
  let row = [| Row.V_null; Row.V_int 0L |] in
  let e = Plan.P_binop (Plan.Or, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "NULL OR 0 → V_null"
    true
    (value_eq (Exec.eval_expr None [||] row e) Row.V_null)
;;

(** Concat text || int (exec.ml line 403). *)
let eval_concat_text_int () =
  let row = [| Row.V_text "x"; Row.V_int 5L |] in
  let e = Plan.P_binop (Plan.Concat, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "'x' || 5 → V_text 'x5'"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_text "x5"))
;;

(** Concat int || text (exec.ml line 404). *)
let eval_concat_int_text () =
  let row = [| Row.V_int 5L; Row.V_text "x" |] in
  let e = Plan.P_binop (Plan.Concat, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "5 || 'x' → V_text '5x'"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_text "5x"))
;;

(** Concat int || int (exec.ml line 405). *)
let eval_concat_int_int () =
  let row = [| Row.V_int 1L; Row.V_int 2L |] in
  let e = Plan.P_binop (Plan.Concat, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "1 || 2 → V_text '12'"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_text "12"))
;;

(** Mod with real/real — exec.ml line 412-413. *)
let eval_mod_real_real () =
  let row = [| Row.V_real 5.5; Row.V_real 2.0 |] in
  let e = Plan.P_binop (Plan.Mod, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "5.5 % 2.0 → V_real 1.5"
    true
    (match Exec.eval_expr None [||] row e with
     | Row.V_real f -> abs_float (f -. 1.5) < 1e-9
     | _ -> false)
;;

(** Mod with int/real — exec.ml line 414-415. *)
let eval_mod_int_real () =
  let row = [| Row.V_int 7L; Row.V_real 3.0 |] in
  let e = Plan.P_binop (Plan.Mod, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "7 % 3.0 → V_real 1.0"
    true
    (match Exec.eval_expr None [||] row e with
     | Row.V_real f -> abs_float (f -. 1.0) < 1e-9
     | _ -> false)
;;

(** Mod with real/int — exec.ml line 416-417. *)
let eval_mod_real_int () =
  let row = [| Row.V_real 7.0; Row.V_int 3L |] in
  let e = Plan.P_binop (Plan.Mod, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "7.0 % 3 → V_real 1.0"
    true
    (match Exec.eval_expr None [||] row e with
     | Row.V_real f -> abs_float (f -. 1.0) < 1e-9
     | _ -> false)
;;

(** Bit_and with null (exec.ml line 421). *)
let eval_bit_and_null () =
  let row = [| Row.V_null; Row.V_int 5L |] in
  let e = Plan.P_binop (Plan.Bit_and, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "NULL & 5 → V_null"
    true
    (value_eq (Exec.eval_expr None [||] row e) Row.V_null)
;;

(** Bit_or with null (exec.ml line 426). *)
let eval_bit_or_null () =
  let row = [| Row.V_null; Row.V_int 5L |] in
  let e = Plan.P_binop (Plan.Bit_or, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "NULL | 5 → V_null"
    true
    (value_eq (Exec.eval_expr None [||] row e) Row.V_null)
;;

(** Lshift with null (exec.ml line 431). *)
let eval_lshift_null () =
  let row = [| Row.V_null; Row.V_int 2L |] in
  let e = Plan.P_binop (Plan.Lshift, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "NULL << 2 → V_null"
    true
    (value_eq (Exec.eval_expr None [||] row e) Row.V_null)
;;

(** Lshift with large shift (>= 64) → 0 (exec.ml line 434). *)
let eval_lshift_overflow () =
  let row = [| Row.V_int 1L; Row.V_int 64L |] in
  let e = Plan.P_binop (Plan.Lshift, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "1 << 64 → 0"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 0L))
;;

(** Rshift with null (exec.ml line 438). *)
let eval_rshift_null () =
  let row = [| Row.V_null; Row.V_int 2L |] in
  let e = Plan.P_binop (Plan.Rshift, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "NULL >> 2 → V_null"
    true
    (value_eq (Exec.eval_expr None [||] row e) Row.V_null)
;;

(** Rshift with large shift (>= 64) → 0 (exec.ml line 441). *)
let eval_rshift_overflow () =
  let row = [| Row.V_int 1L; Row.V_int 65L |] in
  let e = Plan.P_binop (Plan.Rshift, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "1 >> 65 → 0"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 0L))
;;

(** LIKE with non-text args → V_null (exec.ml line 448). *)
let eval_like_non_text () =
  let row = [| Row.V_int 5L; Row.V_text "%" |] in
  let e = Plan.P_binop (Plan.Like, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "5 LIKE '%' → V_null"
    true
    (value_eq (Exec.eval_expr None [||] row e) Row.V_null)
;;

(** GLOB with non-text args → V_null (exec.ml line 454). *)
let eval_glob_non_text () =
  let row = [| Row.V_int 5L; Row.V_text "*" |] in
  let e = Plan.P_binop (Plan.Glob, Plan.P_col 0, Plan.P_col 1) in
  Alcotest.(check bool)
    "5 GLOB '*' → V_null"
    true
    (value_eq (Exec.eval_expr None [||] row e) Row.V_null)
;;

(** Abs with int literal — exercises exec.ml line 237. *)
let eval_abs_int () =
  let row = [||] in
  let e = Plan.P_func (Ast.Fn_abs, [ Plan.P_lit (Ast.L_int (-5L)) ]) in
  Alcotest.(check bool)
    "ABS(-5) → V_int 5"
    true
    (value_eq (Exec.eval_expr None [||] row e) (Row.V_int 5L))
;;

(* ------------------------------------------------------------------ *)
(* Runner                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  let qcheck_tests =
    List.map
      QCheck_alcotest.to_alcotest
      [ prop_lt_filter
      ; prop_ge_filter
      ; prop_ne_filter
      ; prop_and_intersection
      ; prop_or_union
      ; prop_not_complement
      ; prop_arith_add
      ]
  in
  Alcotest.run
    "Expr"
    [ ( "comparison"
      , [ Alcotest.test_case "where_gt" `Quick where_gt
        ; Alcotest.test_case "where_ge" `Quick where_ge
        ; Alcotest.test_case "where_lt" `Quick where_lt
        ; Alcotest.test_case "where_le" `Quick where_le
        ; Alcotest.test_case "where_ne_bang" `Quick where_ne_bang
        ; Alcotest.test_case "where_ne_angle" `Quick where_ne_angle
        ] )
    ; ( "logical"
      , [ Alcotest.test_case "where_and_range" `Quick where_and_range
        ; Alcotest.test_case "where_or_outside_range" `Quick where_or_outside_range
        ; Alcotest.test_case "where_not" `Quick where_not
        ] )
    ; ( "null"
      , [ Alcotest.test_case "where_is_null" `Quick where_is_null
        ; Alcotest.test_case "where_is_not_null" `Quick where_is_not_null
        ; Alcotest.test_case "where_null_eq_null" `Quick where_null_eq_null
        ; Alcotest.test_case "where_null_ne_null" `Quick where_null_ne_null
        ; Alcotest.test_case "where_s_eq_null_no_rows" `Quick where_s_eq_null_no_rows
        ; Alcotest.test_case "where_one_eq_one" `Quick where_one_eq_one
        ] )
    ; ( "arithmetic"
      , [ Alcotest.test_case "where_arith_add" `Quick where_arith_add
        ; Alcotest.test_case "where_arith_sub" `Quick where_arith_sub
        ; Alcotest.test_case "where_arith_mul" `Quick where_arith_mul
        ; Alcotest.test_case "where_arith_div" `Quick where_arith_div
        ; Alcotest.test_case "where_unary_minus" `Quick where_unary_minus
        ; Alcotest.test_case "where_arith_precedence" `Quick where_arith_precedence
        ; Alcotest.test_case "where_arith_parens" `Quick where_arith_parens
        ] )
    ; "qualified", [ Alcotest.test_case "where_qualified_col" `Quick where_qualified_col ]
    ; ( "eval_expr"
      , [ Alcotest.test_case "eval_eq" `Quick eval_eq
        ; Alcotest.test_case "eval_neq" `Quick eval_neq
        ; Alcotest.test_case "eval_lt" `Quick eval_lt
        ; Alcotest.test_case "eval_arith_add" `Quick eval_arith_add
        ; Alcotest.test_case "eval_arith_add_real" `Quick eval_arith_add_real
        ; Alcotest.test_case "eval_arith_mixed_int_real" `Quick eval_arith_mixed_int_real
        ; Alcotest.test_case "eval_arith_null" `Quick eval_arith_null
        ; Alcotest.test_case "eval_neg_int" `Quick eval_neg_int
        ; Alcotest.test_case "eval_neg_real" `Quick eval_neg_real
        ; Alcotest.test_case "eval_neg_null" `Quick eval_neg_null
        ; Alcotest.test_case "eval_neg_text_raises" `Quick eval_neg_text_raises
        ; Alcotest.test_case "eval_is_null_true" `Quick eval_is_null_true
        ; Alcotest.test_case "eval_is_null_false" `Quick eval_is_null_false
        ; Alcotest.test_case "eval_is_not_null_true" `Quick eval_is_not_null_true
        ; Alcotest.test_case "eval_not_true" `Quick eval_not_true
        ; Alcotest.test_case "eval_not_null" `Quick eval_not_null
        ; Alcotest.test_case "eval_div_zero_raises" `Quick eval_div_zero_raises
        ; Alcotest.test_case "eval_and" `Quick eval_and
        ; Alcotest.test_case "eval_or" `Quick eval_or
        ; Alcotest.test_case "eval_cmp_null" `Quick eval_cmp_null
        ; Alcotest.test_case "eval_ne_text" `Quick eval_ne_text
        ; Alcotest.test_case "eval_ne_text_equal" `Quick eval_ne_text_equal
        ; Alcotest.test_case "eval_ne_real" `Quick eval_ne_real
        ; Alcotest.test_case "eval_ne_blob" `Quick eval_ne_blob
        ; Alcotest.test_case "eval_lt_text" `Quick eval_lt_text
        ; Alcotest.test_case "eval_lt_real" `Quick eval_lt_real
        ; Alcotest.test_case "eval_lt_blob" `Quick eval_lt_blob
        ; Alcotest.test_case "eval_cmp_cross_type" `Quick eval_cmp_cross_type
        ; Alcotest.test_case "eval_arith_real_int" `Quick eval_arith_real_int
        ; Alcotest.test_case
            "eval_compare_values_cross_type"
            `Quick
            eval_compare_values_cross_type
        ; Alcotest.test_case "eval_arith_text_error" `Quick eval_arith_text_error
        ; Alcotest.test_case "eval_ne_null_left" `Quick eval_ne_null_left
        ] )
    ; "qcheck", qcheck_tests
    ; ( "binary_operators"
      , [ Alcotest.test_case "concat" `Quick test_concat
        ; Alcotest.test_case "mod" `Quick test_mod
        ; Alcotest.test_case "bit_and" `Quick test_bit_and
        ; Alcotest.test_case "bit_or" `Quick test_bit_or
        ; Alcotest.test_case "lshift" `Quick test_lshift
        ; Alcotest.test_case "rshift" `Quick test_rshift
        ; Alcotest.test_case "rshift_negative" `Quick test_rshift_negative
        ; Alcotest.test_case "bitnot" `Quick test_bitnot
        ; Alcotest.test_case "and_null_true" `Quick eval_and_null_true
        ; Alcotest.test_case "or_null_false" `Quick eval_or_null_false
        ; Alcotest.test_case "concat_text_int" `Quick eval_concat_text_int
        ; Alcotest.test_case "concat_int_text" `Quick eval_concat_int_text
        ; Alcotest.test_case "concat_int_int" `Quick eval_concat_int_int
        ; Alcotest.test_case "mod_real_real" `Quick eval_mod_real_real
        ; Alcotest.test_case "mod_int_real" `Quick eval_mod_int_real
        ; Alcotest.test_case "mod_real_int" `Quick eval_mod_real_int
        ; Alcotest.test_case "bit_and_null" `Quick eval_bit_and_null
        ; Alcotest.test_case "bit_or_null" `Quick eval_bit_or_null
        ; Alcotest.test_case "lshift_null" `Quick eval_lshift_null
        ; Alcotest.test_case "lshift_overflow" `Quick eval_lshift_overflow
        ; Alcotest.test_case "rshift_null" `Quick eval_rshift_null
        ; Alcotest.test_case "rshift_overflow" `Quick eval_rshift_overflow
        ; Alcotest.test_case "like_non_text" `Quick eval_like_non_text
        ; Alcotest.test_case "glob_non_text" `Quick eval_glob_non_text
        ; Alcotest.test_case "abs_int" `Quick eval_abs_int
        ] )
    ; ( "like_glob"
      , [ Alcotest.test_case "like_match" `Quick test_like_match
        ; Alcotest.test_case "like_null" `Quick test_like_null
        ; Alcotest.test_case "glob_null" `Quick test_glob_null
        ; Alcotest.test_case "glob_match" `Quick test_glob_match
        ] )
    ; ( "between_in"
      , [ Alcotest.test_case "between" `Quick test_between
        ; Alcotest.test_case "in" `Quick test_in
        ; Alcotest.test_case "in_null" `Quick test_in_null
        ; Alcotest.test_case "in_null_in_list" `Quick test_in_null_in_list
        ] )
    ]
;;
