open Sqlocaml_sql

let tokenize s =
  let buf = Lexing.from_string s in
  let rec loop acc =
    let t = Lexer.token buf in
    let acc' = t :: acc in
    if t = Parser.EOF then List.rev acc'
    else loop acc'
  in
  loop []

(* ------------------------------------------------------------------ *)
(* Group 1: Individual keyword tokens                                   *)
(* ------------------------------------------------------------------ *)

let test_kw_create () =
  Alcotest.(check (list string)) "CREATE"
    ["CREATE"; "EOF"]
    (List.map (fun t -> match t with
       | Parser.CREATE -> "CREATE" | Parser.EOF -> "EOF" | _ -> "?") (tokenize "CREATE"))

(* Rather than re-implementing a pretty-printer, we compare token lists
   directly using polymorphic equality which works fine for sum types. *)

let tok_list : Parser.token list Alcotest.testable =
  let pp ppf _tl = Format.fprintf ppf "<token list>" in
  Alcotest.testable pp (=)

let check name expected input =
  Alcotest.test_case name `Quick (fun () ->
    Alcotest.(check tok_list) name expected (tokenize input))

(* ------------------------------------------------------------------ *)
(* Group 1: Keywords                                                    *)
(* ------------------------------------------------------------------ *)

let kw_tests = [
  check "kw_create"  [Parser.CREATE;  Parser.EOF]          "CREATE";
  check "kw_table"   [Parser.TABLE;   Parser.EOF]          "TABLE";
  check "kw_insert"  [Parser.INSERT;  Parser.EOF]          "INSERT";
  check "kw_into"    [Parser.INTO;    Parser.EOF]          "INTO";
  check "kw_values"  [Parser.VALUES;  Parser.EOF]          "VALUES";
  check "kw_select"  [Parser.SELECT;  Parser.EOF]          "SELECT";
  check "kw_from"    [Parser.FROM;    Parser.EOF]          "FROM";
  check "kw_where"   [Parser.WHERE;   Parser.EOF]          "WHERE";
  check "kw_integer" [Parser.INTEGER_TY; Parser.EOF]       "INTEGER";
  check "kw_text"    [Parser.TEXT_TY; Parser.EOF]          "TEXT";
  check "kw_not"     [Parser.NOT;     Parser.EOF]          "NOT";
  check "kw_null"    [Parser.NULL;    Parser.EOF]          "NULL";
  check "kw_primary" [Parser.PRIMARY; Parser.EOF]          "PRIMARY";
  check "kw_key"     [Parser.KEY;     Parser.EOF]          "KEY";
  check "kw_and"     [Parser.AND;     Parser.EOF]          "AND";
]

(* ------------------------------------------------------------------ *)
(* Group 2: Punctuation tokens                                          *)
(* ------------------------------------------------------------------ *)

let punct_tests = [
  check "punct_star"   [Parser.STAR;   Parser.EOF] "*";
  check "punct_lparen" [Parser.LPAREN; Parser.EOF] "(";
  check "punct_rparen" [Parser.RPAREN; Parser.EOF] ")";
  check "punct_comma"  [Parser.COMMA;  Parser.EOF] ",";
  check "punct_semi"   [Parser.SEMI;   Parser.EOF] ";";
  check "punct_eq"     [Parser.EQ;     Parser.EOF] "=";
]

(* ------------------------------------------------------------------ *)
(* Group 3: Literal tokens                                              *)
(* ------------------------------------------------------------------ *)

let lit_tests = [
  check "int_42"          [Parser.INT_LIT 42L;           Parser.EOF] "42";
  check "int_0"           [Parser.INT_LIT 0L;            Parser.EOF] "0";
  check "int_big"         [Parser.INT_LIT 9999999999L;   Parser.EOF] "9999999999";
  check "int_neg1"        [Parser.INT_LIT (-1L);         Parser.EOF] "-1";
  check "int_neg_big"     [Parser.INT_LIT (-9999999999L);Parser.EOF] "-9999999999";
  check "str_hello"       [Parser.STRING_LIT "hello";    Parser.EOF] "'hello'";
  check "str_empty"       [Parser.STRING_LIT "";         Parser.EOF] "''";
  check "str_with_space"  [Parser.STRING_LIT "hello world"; Parser.EOF] "'hello world'";
  check "ident_users"     [Parser.IDENT "users";         Parser.EOF] "users";
  check "ident_underscore"[Parser.IDENT "_col";          Parser.EOF] "_col";
  check "ident_alphanum"  [Parser.IDENT "col123";        Parser.EOF] "col123";
]

(* ------------------------------------------------------------------ *)
(* Group 4: Whitespace and comments                                     *)
(* ------------------------------------------------------------------ *)

let ws_tests = [
  check "ws_leading_trailing" [Parser.CREATE; Parser.EOF]         "  CREATE  ";
  check "ws_tab"              [Parser.CREATE; Parser.TABLE; Parser.EOF] "CREATE\tTABLE";
  check "ws_newline"          [Parser.CREATE; Parser.TABLE; Parser.EOF] "CREATE\nTABLE";
  check "comment_only"        [Parser.CREATE; Parser.EOF]         "-- comment\nCREATE";
  check "comment_inline"      [Parser.CREATE; Parser.TABLE; Parser.EOF] "CREATE -- comment\nTABLE";
]

(* ------------------------------------------------------------------ *)
(* Group 5: Multi-token sequences                                       *)
(* ------------------------------------------------------------------ *)

let multi_tests = [
  check "create_table_name"
    [Parser.CREATE; Parser.TABLE; Parser.IDENT "users"; Parser.EOF]
    "CREATE TABLE users";

  check "insert_stmt"
    [Parser.INSERT; Parser.INTO; Parser.IDENT "t";
     Parser.LPAREN; Parser.IDENT "id"; Parser.RPAREN;
     Parser.VALUES; Parser.LPAREN; Parser.INT_LIT 1L; Parser.RPAREN;
     Parser.SEMI; Parser.EOF]
    "INSERT INTO t (id) VALUES (1);";

  check "select_where"
    [Parser.SELECT; Parser.STAR; Parser.FROM; Parser.IDENT "users";
     Parser.WHERE; Parser.IDENT "id"; Parser.EQ; Parser.INT_LIT 42L;
     Parser.SEMI; Parser.EOF]
    "SELECT * FROM users WHERE id = 42;";

  check "select_cols"
    [Parser.SELECT; Parser.IDENT "id"; Parser.COMMA; Parser.IDENT "name";
     Parser.FROM; Parser.IDENT "t"; Parser.SEMI; Parser.EOF]
    "SELECT id, name FROM t;";
]

(* ------------------------------------------------------------------ *)
(* Group 6: Full CREATE TABLE statement                                 *)
(* ------------------------------------------------------------------ *)

let create_table_tests = [
  check "create_table_full"
    [Parser.CREATE; Parser.TABLE; Parser.IDENT "users";
     Parser.LPAREN;
     Parser.IDENT "id"; Parser.INTEGER_TY; Parser.PRIMARY; Parser.KEY;
     Parser.COMMA;
     Parser.IDENT "name"; Parser.TEXT_TY; Parser.NOT; Parser.NULL;
     Parser.RPAREN; Parser.SEMI;
     Parser.EOF]
    "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT NOT NULL);";
]

(* ------------------------------------------------------------------ *)
(* Group 7: Error handling                                              *)
(* ------------------------------------------------------------------ *)

let error_tests = [
  Alcotest.test_case "unknown_char" `Quick (fun () ->
    match tokenize "@" with
    | _ -> Alcotest.fail "expected exception for '@'"
    | exception _ -> ()
  );
  Alcotest.test_case "unterminated_string" `Quick (fun () ->
    match tokenize "'unterminated" with
    | _ -> Alcotest.fail "expected exception for unterminated string"
    | exception _ -> ()
  );
]

(* ------------------------------------------------------------------ *)
(* Main                                                                 *)
(* ------------------------------------------------------------------ *)

let () =
  ignore test_kw_create;  (* suppress unused-var warning *)
  Alcotest.run "Lexer" [
    "keywords",   kw_tests;
    "punctuation",punct_tests;
    "literals",   lit_tests;
    "whitespace", ws_tests;
    "multi-token",multi_tests;
    "create-table",create_table_tests;
    "errors",     error_tests;
  ]
