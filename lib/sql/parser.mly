%{
  open Ast

  type col_constraint = Col_not_null | Col_primary_key
%}

%token <string> IDENT
%token <int64>  INT_LIT
%token <string> STRING_LIT
%token <float>  FLOAT_LIT
%token CREATE TABLE INSERT INTO VALUES SELECT FROM WHERE
%token INTEGER_TY TEXT_TY REAL_TY BLOB_TY
%token NOT NULL PRIMARY KEY AND
%token ORDER BY ASC DESC LIMIT OFFSET
%token STAR LPAREN RPAREN COMMA SEMI EQ
%token EOF

%left EQ

%start <Ast.stmt> stmt_eof

%%

stmt_eof:
  | s = stmt SEMI? EOF { s }

stmt:
  | s = create_table { s }
  | s = insert       { s }
  | s = select       { s }

create_table:
  | CREATE TABLE name = IDENT LPAREN cols = separated_nonempty_list(COMMA, column_def) RPAREN
    { S_create_table { name; columns = cols } }

column_def:
  | name = IDENT ty = col_ty cs = column_constraint*
    { let not_null    = List.mem Col_not_null cs in
      let primary_key = List.mem Col_primary_key cs in
      { name; ty; not_null; primary_key } }

col_ty:
  | INTEGER_TY { Ty_int }
  | TEXT_TY    { Ty_text }
  | REAL_TY    { Ty_real }
  | BLOB_TY    { Ty_blob }

column_constraint:
  | NOT NULL    { Col_not_null }
  | PRIMARY KEY { Col_primary_key }

insert:
  | INSERT INTO table = IDENT LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
      VALUES LPAREN vals = separated_nonempty_list(COMMA, literal) RPAREN
    { S_insert { table; columns = cols; values = vals } }

literal:
  | n = INT_LIT    { L_int n }
  | s = STRING_LIT { L_text s }
  | NULL           { L_null }
  | f = FLOAT_LIT  { L_real f }

select:
  | SELECT proj = projection FROM table = IDENT wh = where_opt
      ob = order_by_clause lim = limit_clause
    { let (limit, offset) = lim in
      S_select { proj; table; where = wh; order = ob; limit; offset } }

projection:
  | STAR                                             { `All }
  | cols = separated_nonempty_list(COMMA, IDENT)     { `Cols cols }

where_opt:
  |                { None }
  | WHERE e = expr { Some e }

order_by_clause:
  |                                                                 { [] }
  | ORDER BY keys = separated_nonempty_list(COMMA, order_key)      { keys }

order_key:
  | name = IDENT           { { col = name; dir = Asc } }
  | name = IDENT ASC       { { col = name; dir = Asc } }
  | name = IDENT DESC      { { col = name; dir = Desc } }

limit_clause:
  |                                         { (None, None) }
  | LIMIT n = INT_LIT                       { (Some (Int64.to_int n), None) }
  | LIMIT n = INT_LIT OFFSET m = INT_LIT   { (Some (Int64.to_int n), Some (Int64.to_int m)) }

expr:
  | l = literal              { E_lit l }
  | name = IDENT             { E_col name }
  | a = expr EQ b = expr     { E_eq (a, b) }
