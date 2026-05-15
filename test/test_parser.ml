open Sqlocaml_sql

let parse s =
  let lexbuf = Lexing.from_string s in
  Parser.stmt_eof Lexer.token lexbuf

(* ------------------------------------------------------------------ *)
(* Group 1: CREATE TABLE                                                *)
(* ------------------------------------------------------------------ *)

let create_simple () =
  match parse "CREATE TABLE t (id INTEGER);" with
  | Ast.S_create_table { name = "t"; columns = [c] } ->
    Alcotest.(check string) "col name" "id" c.name;
    Alcotest.(check bool) "col is int" true (c.ty = Ast.Ty_int)
  | _ -> Alcotest.fail "expected S_create_table"

let create_two_cols () =
  match parse "CREATE TABLE users (id INTEGER, name TEXT);" with
  | Ast.S_create_table { name = "users"; columns = [c1; c2] } ->
    Alcotest.(check string) "c1" "id" c1.name;
    Alcotest.(check string) "c2" "name" c2.name;
    Alcotest.(check bool) "c2 text" true (c2.ty = Ast.Ty_text)
  | _ -> Alcotest.fail "expected 2-col table"

let create_primary_key () =
  match parse "CREATE TABLE t (id INTEGER PRIMARY KEY);" with
  | Ast.S_create_table { columns = [c]; _ } ->
    Alcotest.(check bool) "pk" true c.primary_key;
    Alcotest.(check bool) "not null default false" false c.not_null
  | _ -> Alcotest.fail "expected S_create_table"

let create_not_null () =
  match parse "CREATE TABLE t (name TEXT NOT NULL);" with
  | Ast.S_create_table { columns = [c]; _ } ->
    Alcotest.(check bool) "not_null" true c.not_null;
    Alcotest.(check bool) "not pk" false c.primary_key
  | _ -> Alcotest.fail "expected S_create_table"

let create_both_constraints () =
  match parse "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT NOT NULL);" with
  | Ast.S_create_table { name = "t"; columns = [c1; c2] } ->
    Alcotest.(check bool) "c1 pk" true c1.primary_key;
    Alcotest.(check bool) "c2 not_null" true c2.not_null
  | _ -> Alcotest.fail "expected 2-col"

let create_many_cols () =
  match parse "CREATE TABLE t (a INTEGER, b TEXT, c INTEGER, d TEXT, e INTEGER);" with
  | Ast.S_create_table { columns; _ } ->
    Alcotest.(check int) "5 cols" 5 (List.length columns)
  | _ -> Alcotest.fail "expected 5-col"

let create_no_semi () =
  (* Semicolon is optional *)
  match parse "CREATE TABLE t (id INTEGER)" with
  | Ast.S_create_table { name = "t"; _ } -> ()
  | _ -> Alcotest.fail "expected S_create_table without semi"

let create_syntax_error () =
  try
    ignore (parse "CREATE TABLE (id INTEGER);");
    Alcotest.fail "expected syntax error"
  with Parser.Error | Failure _ -> ()

(* ------------------------------------------------------------------ *)
(* Group 2: INSERT                                                      *)
(* ------------------------------------------------------------------ *)

let insert_named_cols () =
  match parse "INSERT INTO users (id, name) VALUES (1, 'alice');" with
  | Ast.S_insert { table = "users"; columns = ["id"; "name"];
                   values = [Ast.L_int 1L; Ast.L_text "alice"] } -> ()
  | _ -> Alcotest.fail "expected S_insert"

let insert_int_only () =
  match parse "INSERT INTO t (n) VALUES (42);" with
  | Ast.S_insert { columns = ["n"]; values = [Ast.L_int 42L]; _ } -> ()
  | _ -> Alcotest.fail "expected int insert"

let insert_null_value () =
  match parse "INSERT INTO t (n) VALUES (NULL);" with
  | Ast.S_insert { values = [Ast.L_null]; _ } -> ()
  | _ -> Alcotest.fail "expected null value"

let insert_negative_int () =
  match parse "INSERT INTO t (n) VALUES (-99);" with
  | Ast.S_insert { values = [Ast.L_int (-99L)]; _ } -> ()
  | _ -> Alcotest.fail "expected negative int"

let insert_empty_string () =
  match parse "INSERT INTO t (s) VALUES ('');" with
  | Ast.S_insert { values = [Ast.L_text ""]; _ } -> ()
  | _ -> Alcotest.fail "expected empty string"

let insert_multiple_rows_not_supported () =
  (* Phase 0: one VALUES list only *)
  try
    ignore (parse "INSERT INTO t (n) VALUES (1), (2);");
    Alcotest.fail "expected syntax error for multi-row insert"
  with Parser.Error | Failure _ -> ()

let insert_no_col_list () =
  (* Phase 0 grammar requires an explicit column list; no-column-list INSERT is a syntax error *)
  try
    ignore (parse "INSERT INTO t VALUES (1);");
    Alcotest.fail "expected Parser.Error for INSERT without column list"
  with Parser.Error | Failure _ -> ()

(* ------------------------------------------------------------------ *)
(* Group 3: SELECT                                                      *)
(* ------------------------------------------------------------------ *)

let select_star () =
  match parse "SELECT * FROM users;" with
  | Ast.S_select { proj = `All; table = "users"; where = None } -> ()
  | _ -> Alcotest.fail "expected SELECT *"

let select_cols () =
  match parse "SELECT id, name FROM users;" with
  | Ast.S_select { proj = `Cols ["id"; "name"]; table = "users"; where = None } -> ()
  | _ -> Alcotest.fail "expected col list"

let select_single_col () =
  match parse "SELECT id FROM t;" with
  | Ast.S_select { proj = `Cols ["id"]; _ } -> ()
  | _ -> Alcotest.fail "expected single col"

let select_where_eq_int () =
  match parse "SELECT * FROM users WHERE id = 42;" with
  | Ast.S_select { where = Some (Ast.E_eq (Ast.E_col "id", Ast.E_lit (Ast.L_int 42L))); _ } -> ()
  | _ -> Alcotest.fail "expected WHERE id=42"

let select_where_eq_string () =
  match parse "SELECT * FROM t WHERE name = 'bob';" with
  | Ast.S_select { where = Some (Ast.E_eq (Ast.E_col "name", Ast.E_lit (Ast.L_text "bob"))); _ } -> ()
  | _ -> Alcotest.fail "expected WHERE name='bob'"

let select_where_eq_null () =
  match parse "SELECT * FROM t WHERE x = NULL;" with
  | Ast.S_select { where = Some (Ast.E_eq (Ast.E_col "x", Ast.E_lit Ast.L_null)); _ } -> ()
  | _ -> Alcotest.fail "expected WHERE x=NULL"

let select_where_reversed () =
  (* literal = col *)
  match parse "SELECT * FROM t WHERE 42 = id;" with
  | Ast.S_select { where = Some (Ast.E_eq (Ast.E_lit (Ast.L_int 42L), Ast.E_col "id")); _ } -> ()
  | _ -> Alcotest.fail "expected reversed eq"

let select_no_semi () =
  match parse "SELECT * FROM t" with
  | Ast.S_select { proj = `All; _ } -> ()
  | _ -> Alcotest.fail "expected no-semi SELECT"

let select_cols_where () =
  match parse "SELECT id, name FROM users WHERE id = 1;" with
  | Ast.S_select { proj = `Cols ["id"; "name"]; where = Some _; _ } -> ()
  | _ -> Alcotest.fail "expected col+where"

(* ------------------------------------------------------------------ *)
(* Main                                                                 *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Parser" [
    "create-table", [
      Alcotest.test_case "simple"             `Quick create_simple;
      Alcotest.test_case "two-cols"           `Quick create_two_cols;
      Alcotest.test_case "primary-key"        `Quick create_primary_key;
      Alcotest.test_case "not-null"           `Quick create_not_null;
      Alcotest.test_case "both-constraints"   `Quick create_both_constraints;
      Alcotest.test_case "many-cols"          `Quick create_many_cols;
      Alcotest.test_case "no-semi"            `Quick create_no_semi;
      Alcotest.test_case "syntax-error"       `Quick create_syntax_error;
    ];
    "insert", [
      Alcotest.test_case "named-cols"         `Quick insert_named_cols;
      Alcotest.test_case "int-only"           `Quick insert_int_only;
      Alcotest.test_case "null-value"         `Quick insert_null_value;
      Alcotest.test_case "negative-int"       `Quick insert_negative_int;
      Alcotest.test_case "empty-string"       `Quick insert_empty_string;
      Alcotest.test_case "multi-row-error"    `Quick insert_multiple_rows_not_supported;
      Alcotest.test_case "no-col-list"        `Quick insert_no_col_list;
    ];
    "select", [
      Alcotest.test_case "star"               `Quick select_star;
      Alcotest.test_case "cols"               `Quick select_cols;
      Alcotest.test_case "single-col"         `Quick select_single_col;
      Alcotest.test_case "where-eq-int"       `Quick select_where_eq_int;
      Alcotest.test_case "where-eq-string"    `Quick select_where_eq_string;
      Alcotest.test_case "where-eq-null"      `Quick select_where_eq_null;
      Alcotest.test_case "where-reversed"     `Quick select_where_reversed;
      Alcotest.test_case "no-semi"            `Quick select_no_semi;
      Alcotest.test_case "cols-where"         `Quick select_cols_where;
    ];
  ]
