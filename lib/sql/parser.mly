%{
  open Ast

  type col_constraint =
    | Col_not_null
    | Col_primary_key
    | Col_default of literal
%}

%token <string> IDENT
%token <int64>  INT_LIT
%token <string> STRING_LIT
%token <float>  FLOAT_LIT
%token CREATE TABLE INSERT INTO VALUES SELECT FROM WHERE
%token INTEGER_TY TEXT_TY REAL_TY BLOB_TY
%token NOT NULL PRIMARY KEY DEFAULT AND OR IS
%token ORDER BY ASC DESC LIMIT OFFSET
%token INDEX ON UNIQUE
%token UPDATE SET
%token DELETE
%token STAR LPAREN RPAREN COMMA SEMI
%token EQ NE LT LE GT GE
%token PLUS MINUS SLASH DOT
%token EOF

%left OR
%left AND
%right NOT
%nonassoc IS
%left EQ NE LT LE GT GE
%left PLUS MINUS
%left STAR SLASH
%nonassoc UMINUS

%start <Ast.stmt> stmt_eof

%%

stmt_eof:
  | s = stmt SEMI? EOF { s }

stmt:
  | s = create_table { s }
  | s = create_index { s }
  | s = insert       { s }
  | s = select       { s }
  | s = update       { s }
  | s = delete       { s }

create_table:
  | CREATE TABLE name = IDENT LPAREN cols = separated_nonempty_list(COMMA, column_def) RPAREN
    { S_create_table { name; columns = cols } }

create_index:
  | CREATE INDEX name = IDENT ON table = IDENT LPAREN col = IDENT RPAREN
    { S_create_index { name; table; column = col; unique = false } }
  | CREATE UNIQUE INDEX name = IDENT ON table = IDENT LPAREN col = IDENT RPAREN
    { S_create_index { name; table; column = col; unique = true } }

column_def:
  | name = IDENT ty = col_ty cs = column_constraint*
    { let not_null    = List.mem Col_not_null cs in
      let primary_key = List.mem Col_primary_key cs in
      let default     = List.fold_left (fun acc c ->
          match c with Col_default l -> Some l | _ -> acc) None cs in
      { name; ty; not_null; primary_key; default } }

col_ty:
  | INTEGER_TY { Ty_int }
  | TEXT_TY    { Ty_text }
  | REAL_TY    { Ty_real }
  | BLOB_TY    { Ty_blob }

column_constraint:
  | NOT NULL              { Col_not_null }
  | PRIMARY KEY           { Col_primary_key }
  | DEFAULT l = def_value { Col_default l }

def_value:
  | n = INT_LIT              { L_int n }
  | s = STRING_LIT           { L_text s }
  | NULL                     { L_null }
  | f = FLOAT_LIT            { L_real f }
  | MINUS n = INT_LIT        { L_int (Int64.neg n) }
  | MINUS f = FLOAT_LIT      { L_real (-. f) }

insert:
  | INSERT INTO table = IDENT LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
      VALUES LPAREN vals = separated_nonempty_list(COMMA, insert_value) RPAREN
    { S_insert { table; columns = cols; values = vals } }

literal:
  | n = INT_LIT    { L_int n }
  | s = STRING_LIT { L_text s }
  | NULL           { L_null }
  | f = FLOAT_LIT  { L_real f }

(* INSERT VALUES allows a leading unary minus on numeric literals so that
   queries like `INSERT INTO t (n) VALUES (-99)` produce L_int (-99L). *)
insert_value:
  | l = literal            { l }
  | MINUS n = INT_LIT      { L_int (Int64.neg n) }
  | MINUS f = FLOAT_LIT    { L_real (-. f) }

select:
  | SELECT proj = projection FROM table = IDENT wh = where_opt
      ob = order_by_clause lim = limit_clause
    { let (limit, offset) = lim in
      S_select { proj; table; where = wh; order = ob; limit; offset } }

update:
  | UPDATE table = IDENT SET
      assignments = separated_nonempty_list(COMMA, assignment)
      wh = where_opt
    { S_update { table; assignments; where = wh } }

delete:
  | DELETE FROM table = IDENT wh = where_opt
    { S_delete { table; where = wh } }

assignment:
  | col = IDENT EQ value = expr { (col, value) }

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
  | l = literal                       { E_lit l }
  | name = IDENT                      { E_col name }
  | t = IDENT DOT c = IDENT           { E_tbl_col (t, c) }
  | a = expr AND b = expr             { E_binop (And, a, b) }
  | a = expr OR  b = expr             { E_binop (Or,  a, b) }
  | NOT e = expr                      { E_not e }
  | a = expr EQ  b = expr             { E_binop (Eq,  a, b) }
  | a = expr NE  b = expr             { E_binop (Ne,  a, b) }
  | a = expr LT  b = expr             { E_binop (Lt,  a, b) }
  | a = expr LE  b = expr             { E_binop (Le,  a, b) }
  | a = expr GT  b = expr             { E_binop (Gt,  a, b) }
  | a = expr GE  b = expr             { E_binop (Ge,  a, b) }
  | a = expr PLUS  b = expr           { E_binop (Add, a, b) }
  | a = expr MINUS b = expr           { E_binop (Sub, a, b) }
  | a = expr STAR  b = expr           { E_binop (Mul, a, b) }
  | a = expr SLASH b = expr           { E_binop (Div, a, b) }
  | MINUS e = expr %prec UMINUS       { E_neg e }
  | e = expr IS NULL                  { E_is_null e }
  | e = expr IS NOT NULL              { E_is_not_null e }
  | LPAREN e = expr RPAREN            { e }
