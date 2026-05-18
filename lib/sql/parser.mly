%{
  open Ast

  type col_constraint =
    | Col_not_null
    | Col_primary_key
    | Col_default of literal
    | Col_check   of expr
    | Col_fk_ref             (* parse-only FK reference — no semantic meaning *)

  type table_item =
    | TI_col of column_def
    | TI_constraint of table_constraint
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
%token DISTINCT
%token DROP
%token BEGIN COMMIT ROLLBACK
%token ABORT IGNORE FAIL
%token JOIN INNER LEFT OUTER
%token GROUP HAVING
%token COUNT SUM AVG MIN MAX
%token LENGTH LOWER UPPER ABS COALESCE IFNULL
%token SUBSTR TRIM LTRIM RTRIM REPLACE INSTR ROUND TYPEOF
%token DATE DATETIME JULIANDAY STRFTIME TIME UNIXEPOCH
%token VIRTUAL USING FTS5
%token MATCH
%token ALTER ADD RENAME TO COLUMN
%token PRAGMA
%token RETURNING
%token UNION INTERSECT EXCEPT ALL
%token LIKE GLOB
%token BETWEEN IN EXISTS
%token CASE WHEN THEN ELSE END
%token AS CAST NULLIF IIF WITH
%token CHECK
%token REFERENCES FOREIGN
%token QUESTION
%token <int>    IPARAM
%token <string> NAMED_PARAM
%token STAR LPAREN RPAREN COMMA SEMI
%token EQ NE LT LE GT GE
%token PLUS MINUS SLASH DOT
%token CONCAT PERCENT AMPERSAND PIPE TILDE LSHIFT RSHIFT
%token EOF

%left OR
%left AND
%right NOT
%nonassoc IS
%nonassoc IN BETWEEN LIKE GLOB
%nonassoc BETWEEN_PREC
%left EQ NE LT LE GT GE
%left PIPE
%left AMPERSAND
%left LSHIFT RSHIFT
%left PLUS MINUS
%left STAR SLASH PERCENT
%left CONCAT
%nonassoc TILDE UMINUS

%start <Ast.stmt> stmt_eof
%start <Ast.expr> expr_only

%%

expr_only:
  | e = expr EOF { e }

stmt_eof:
  | s = stmt SEMI? EOF { s }

stmt:
  | s = with_cte          { s }
  | s = create_table      { s }
  | s = create_fts_table  { s }
  | s = create_index      { s }
  | s = insert            { s }
  | s = compound_select   { s }
  | s = update            { s }
  | s = delete            { s }
  | s = drop_table        { s }
  | s = drop_index        { s }
  | s = begin_stmt        { s }
  | s = commit_stmt       { s }
  | s = rollback_stmt     { s }
  | s = pragma_stmt       { s }
  | s = alter_table       { s }

with_cte:
  | WITH name = IDENT AS LPAREN def = compound_select RPAREN query = compound_select
    { Ast.S_with_cte { name; def; query } }

pragma_stmt:
  | PRAGMA name = IDENT LPAREN arg = IDENT RPAREN
    { match String.lowercase_ascii name with
      | "table_info" -> S_pragma (Pragma_table_info arg)
      | "index_list" -> S_pragma (Pragma_index_list arg)
      | _ -> failwith (Printf.sprintf "unknown pragma: %s" name) }

drop_table:
  | DROP TABLE name = IDENT { S_drop_table { name } }

drop_index:
  | DROP INDEX name = IDENT { S_drop_index { name } }

alter_table:
  | ALTER TABLE table = IDENT ADD COLUMN col = column_def
    { Ast.S_alter_table { table; action = Ast.AA_add_column col } }
  | ALTER TABLE table = IDENT ADD col = column_def
    { Ast.S_alter_table { table; action = Ast.AA_add_column col } }
  | ALTER TABLE table = IDENT RENAME TO new_name = IDENT
    { Ast.S_alter_table { table; action = Ast.AA_rename_table new_name } }
  | ALTER TABLE table = IDENT RENAME COLUMN old_col = IDENT TO new_col = IDENT
    { Ast.S_alter_table { table; action = Ast.AA_rename_column (old_col, new_col) } }
  | ALTER TABLE table = IDENT RENAME old_col = IDENT TO new_col = IDENT
    { Ast.S_alter_table { table; action = Ast.AA_rename_column (old_col, new_col) } }

begin_stmt:
  | BEGIN    { S_begin }

commit_stmt:
  | COMMIT   { S_commit }

rollback_stmt:
  | ROLLBACK { S_rollback }

table_item:
  | col = column_def
    { TI_col col }
  | UNIQUE LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
    { TI_constraint (Ast.TC_unique cols) }
  | PRIMARY KEY LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
    { TI_constraint (Ast.TC_primary_key cols) }

create_table:
  | CREATE TABLE name = IDENT LPAREN items = separated_nonempty_list(COMMA, table_item) RPAREN
    { let cols = List.filter_map (function TI_col c -> Some c | _ -> None) items in
      let cons = List.filter_map (function TI_constraint c -> Some c | _ -> None) items in
      S_create_table { name; columns = cols; constraints = cons } }

create_fts_table:
  | CREATE VIRTUAL TABLE name = IDENT USING FTS5
      LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
    { S_create_fts_table { name; columns = cols } }

create_index:
  | CREATE INDEX name = IDENT ON table = IDENT
      LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
    { S_create_index { name; table; columns = cols; unique = false } }
  | CREATE UNIQUE INDEX name = IDENT ON table = IDENT
      LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
    { S_create_index { name; table; columns = cols; unique = true } }

column_def:
  | name = IDENT ty = col_ty cs = column_constraint*
    { let not_null    = List.mem Col_not_null cs in
      let primary_key = List.mem Col_primary_key cs in
      let default     = List.fold_left (fun acc c ->
          match c with Col_default l -> Some l | _ -> acc) None cs in
      let check       = List.fold_left (fun acc c ->
          match c with Col_check e -> Some e | _ -> acc) None cs in
      { name; ty; not_null; primary_key; default; check } }

col_ty:
  | INTEGER_TY { Ty_int }
  | TEXT_TY    { Ty_text }
  | REAL_TY    { Ty_real }
  | BLOB_TY    { Ty_blob }

column_constraint:
  | NOT NULL              { Col_not_null }
  | PRIMARY KEY           { Col_primary_key }
  | DEFAULT l = def_value { Col_default l }
  | CHECK LPAREN e = expr RPAREN { Col_check e }
  | REFERENCES t = IDENT                                { ignore t; Col_fk_ref }
  | REFERENCES t = IDENT LPAREN c = IDENT RPAREN       { ignore t; ignore c; Col_fk_ref }

def_value:
  | n = INT_LIT              { L_int n }
  | s = STRING_LIT           { L_text s }
  | NULL                     { L_null }
  | f = FLOAT_LIT            { L_real f }
  | MINUS n = INT_LIT        { L_int (Int64.neg n) }
  | MINUS f = FLOAT_LIT      { L_real (-. f) }

opt_conflict:
  | OR REPLACE  { Some Ast.CA_replace  }
  | OR IGNORE   { Some Ast.CA_ignore   }
  | OR ABORT    { Some Ast.CA_abort    }
  | OR FAIL     { Some Ast.CA_fail     }
  | OR ROLLBACK { Some Ast.CA_rollback }
  |             { None }

value_row:
  | LPAREN vals = separated_nonempty_list(COMMA, insert_expr) RPAREN { vals }

insert:
  | INSERT oc = opt_conflict INTO table = IDENT
      LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
      VALUES rows = separated_nonempty_list(COMMA, value_row)
      ret = opt_returning
    { Ast.S_insert { table; columns = cols; values = rows; on_conflict = oc; returning = ret } }
  | INSERT oc = opt_conflict INTO table = IDENT
      VALUES rows = separated_nonempty_list(COMMA, value_row)
      ret = opt_returning
    { Ast.S_insert { table; columns = []; values = rows; on_conflict = oc; returning = ret } }

opt_returning:
  | RETURNING exprs = separated_nonempty_list(COMMA, expr) { exprs }
  |                                                         { [] }

literal:
  | n = INT_LIT    { L_int n }
  | s = STRING_LIT { L_text s }
  | NULL           { L_null }
  | f = FLOAT_LIT  { L_real f }

(* INSERT VALUES allows a leading unary minus on numeric literals and
   positional parameters (?). *)
insert_expr:
  | l = literal            { E_lit l }
  | MINUS n = INT_LIT      { E_lit (L_int (Int64.neg n)) }
  | MINUS f = FLOAT_LIT    { E_lit (L_real (-. f)) }
  | QUESTION        { E_param Param_anon }
  | i = IPARAM      { E_param (Param_index i) }
  | n = NAMED_PARAM { E_param (Param_name n) }

compound_select:
  | s = select  { s }
  | left = compound_select UNION ALL right = select
    { S_compound { op = Union_all; left; right } }
  | left = compound_select UNION right = select
    { S_compound { op = Union; left; right } }
  | left = compound_select INTERSECT right = select
    { S_compound { op = Intersect; left; right } }
  | left = compound_select EXCEPT right = select
    { S_compound { op = Except; left; right } }

select:
  | SELECT distinct = boption(DISTINCT) proj = projection ft = from_tail
    { match ft with
      | Some (table, tbl_alias, js, wh, gb, hv, ob, limit, offset) ->
        S_select { distinct; proj; table; table_alias = tbl_alias; joins = js; where = wh;
                   group_by = gb; having = hv;
                   order = ob; limit; offset }
      | None ->
        let exprs = match proj with
          | `Exprs es -> List.map fst es
          | `Cols names -> List.map (fun n -> E_col n) names
          | `All -> []
        in
        S_const_select { exprs } }

from_tail:
  | FROM table = IDENT tbl_alias = option(preceded(AS, IDENT))
      js = join_clauses wh = where_opt
      gb = group_by_clause hv = having_clause ob = order_by_clause lim = limit_clause
    { let (limit, offset) = lim in
      Some (table, tbl_alias, js, wh, gb, hv, ob, limit, offset) }
  |   { None }

join_clauses:
  |                                  { [] }
  | j = join_clause rest = join_clauses { j :: rest }

join_clause:
  | INNER JOIN t = IDENT alias = option(preceded(AS, IDENT)) ON e = expr
    { { kind = Inner; table = t; alias; on = e } }
  | LEFT JOIN t = IDENT alias = option(preceded(AS, IDENT)) ON e = expr
    { { kind = Left;  table = t; alias; on = e } }
  | LEFT OUTER JOIN t = IDENT alias = option(preceded(AS, IDENT)) ON e = expr
    { { kind = Left;  table = t; alias; on = e } }
  | JOIN t = IDENT alias = option(preceded(AS, IDENT)) ON e = expr
    { { kind = Inner; table = t; alias; on = e } }

update:
  | UPDATE table = IDENT SET
      assignments = separated_nonempty_list(COMMA, assignment)
      wh = where_opt
      ret = opt_returning
    { S_update { table; assignments; where = wh; returning = ret } }

delete:
  | DELETE FROM table = IDENT wh = where_opt ret = opt_returning
    { S_delete { table; where = wh; returning = ret } }

assignment:
  | col = IDENT EQ value = expr { (col, value) }

projection:
  | STAR                                             { `All }
  | items = separated_nonempty_list(COMMA, proj_item)
    { (* If every item is a plain column reference, produce `Cols
         (preserves existing AST shape).  Otherwise produce `Exprs. *)
      let all_plain_cols = List.for_all (function `Col _ -> true | _ -> false) items in
      if all_plain_cols then
        `Cols (List.map (function `Col c -> c | _ -> assert false) items)
      else
        `Exprs (List.map (function
          | `Col c         -> (Ast.E_col c, None)
          | `ExprA (e, a)  -> (e, a)) items) }

scalar_expr:
  | LENGTH   LPAREN e = expr RPAREN                                   { E_func (Fn_length,   [e]) }
  | LOWER    LPAREN e = expr RPAREN                                   { E_func (Fn_lower,    [e]) }
  | UPPER    LPAREN e = expr RPAREN                                   { E_func (Fn_upper,    [e]) }
  | ABS      LPAREN e = expr RPAREN                                   { E_func (Fn_abs,      [e]) }
  | IFNULL   LPAREN a = expr COMMA b = expr RPAREN                    { E_func (Fn_ifnull,   [a; b]) }
  | COALESCE LPAREN es = separated_nonempty_list(COMMA, expr) RPAREN  { E_func (Fn_coalesce, es) }
  | SUBSTR   LPAREN s = expr COMMA start = expr RPAREN
    { E_func (Fn_substr, [s; start]) }
  | SUBSTR   LPAREN s = expr COMMA start = expr COMMA len = expr RPAREN
    { E_func (Fn_substr, [s; start; len]) }
  | TRIM     LPAREN s = expr RPAREN
    { E_func (Fn_trim, [s]) }
  | TRIM     LPAREN s = expr COMMA chars = expr RPAREN
    { E_func (Fn_trim, [s; chars]) }
  | LTRIM    LPAREN s = expr RPAREN
    { E_func (Fn_ltrim, [s]) }
  | LTRIM    LPAREN s = expr COMMA chars = expr RPAREN
    { E_func (Fn_ltrim, [s; chars]) }
  | RTRIM    LPAREN s = expr RPAREN
    { E_func (Fn_rtrim, [s]) }
  | RTRIM    LPAREN s = expr COMMA chars = expr RPAREN
    { E_func (Fn_rtrim, [s; chars]) }
  | REPLACE  LPAREN s = expr COMMA old_s = expr COMMA rep = expr RPAREN
    { E_func (Fn_replace, [s; old_s; rep]) }
  | INSTR    LPAREN s = expr COMMA sub = expr RPAREN
    { E_func (Fn_instr, [s; sub]) }
  | ROUND    LPAREN n = expr RPAREN
    { E_func (Fn_round, [n]) }
  | ROUND    LPAREN n = expr COMMA d = expr RPAREN
    { E_func (Fn_round, [n; d]) }
  | TYPEOF   LPAREN e = expr RPAREN
    { E_func (Fn_typeof, [e]) }
  | DATE      LPAREN args = separated_nonempty_list(COMMA, expr) RPAREN
    { E_func (Fn_date,      args) }
  | DATETIME  LPAREN args = separated_nonempty_list(COMMA, expr) RPAREN
    { E_func (Fn_datetime,  args) }
  | JULIANDAY LPAREN args = separated_nonempty_list(COMMA, expr) RPAREN
    { E_func (Fn_julianday, args) }
  | STRFTIME  LPAREN args = separated_nonempty_list(COMMA, expr) RPAREN
    { E_func (Fn_strftime,  args) }
  | TIME      LPAREN args = separated_nonempty_list(COMMA, expr) RPAREN
    { E_func (Fn_time,      args) }
  | UNIXEPOCH LPAREN args = separated_nonempty_list(COMMA, expr) RPAREN
    { E_func (Fn_unixepoch, args) }
  | CAST LPAREN e = expr AS t = col_ty RPAREN
    { E_cast (e, t) }
  | NULLIF LPAREN a = expr COMMA b = expr RPAREN
    { E_case { scrutinee = None;
               branches  = [(E_binop (Eq, a, b), E_lit L_null)];
               else_     = Some a } }
  | IIF LPAREN c = expr COMMA t = expr COMMA f = expr RPAREN
    { E_case { scrutinee = None;
               branches  = [(c, t)];
               else_     = Some f } }

proj_item:
  | e = expr AS alias = IDENT { `ExprA (e, Some alias) }
  | e = expr { match e with E_col name -> `Col name | _ -> `ExprA (e, None) }

where_opt:
  |                { None }
  | WHERE e = expr { Some e }

group_by_clause:
  |                                                                 { [] }
  | GROUP BY cs = separated_nonempty_list(COMMA, IDENT)            { cs }

having_clause:
  |                                                                 { None }
  | HAVING e = expr                                                 { Some e }

order_by_clause:
  |                                                                 { [] }
  | ORDER BY keys = separated_nonempty_list(COMMA, order_key)      { keys }

order_key:
  | e = expr      { { expr = e; dir = Asc } }
  | e = expr ASC  { { expr = e; dir = Asc } }
  | e = expr DESC { { expr = e; dir = Desc } }

limit_clause:
  |                                         { (None, None) }
  | LIMIT n = INT_LIT                       { (Some (Int64.to_int n), None) }
  | LIMIT n = INT_LIT OFFSET m = INT_LIT   { (Some (Int64.to_int n), Some (Int64.to_int m)) }

agg_expr:
  | COUNT LPAREN STAR RPAREN          { E_agg (Agg_count, None) }
  | COUNT LPAREN e = expr RPAREN      { E_agg (Agg_count, Some e) }
  | SUM   LPAREN e = expr RPAREN      { E_agg (Agg_sum,   Some e) }
  | AVG   LPAREN e = expr RPAREN      { E_agg (Agg_avg,   Some e) }
  | MIN   LPAREN e = expr RPAREN      { E_agg (Agg_min,   Some e) }
  | MAX   LPAREN e = expr RPAREN      { E_agg (Agg_max,   Some e) }

when_clause:
  | WHEN cond = expr THEN result = expr { (cond, result) }

else_clause:
  | ELSE e = expr { e }

case_expr:
  | CASE bs = nonempty_list(when_clause) el = option(else_clause) END
    { E_case { scrutinee = None; branches = bs; else_ = el } }
  | CASE scr = expr bs = nonempty_list(when_clause) el = option(else_clause) END
    { E_case { scrutinee = Some scr; branches = bs; else_ = el } }

(* between_bound is an expression that may not contain a bare AND binary
   operator at the top level.  This prevents the reduce/reduce conflict that
   arises in  "expr BETWEEN expr AND expr"  where the AND token is ambiguous
   between the BETWEEN separator and the binary AND operator. *)
between_bound:
  | l = literal                            { E_lit l }
  | name = IDENT                           { E_col name }
  | t = IDENT DOT c = IDENT               { E_tbl_col (t, c) }
  | e = agg_expr                           { e }
  | e = scalar_expr                        { e }
  | NOT e = between_bound                  { E_not e }
  | a = between_bound EQ  b = between_bound { E_binop (Eq,  a, b) }
  | a = between_bound NE  b = between_bound { E_binop (Ne,  a, b) }
  | a = between_bound LT  b = between_bound { E_binop (Lt,  a, b) }
  | a = between_bound LE  b = between_bound { E_binop (Le,  a, b) }
  | a = between_bound GT  b = between_bound { E_binop (Gt,  a, b) }
  | a = between_bound GE  b = between_bound { E_binop (Ge,  a, b) }
  | a = between_bound PLUS  b = between_bound { E_binop (Add, a, b) }
  | a = between_bound MINUS b = between_bound { E_binop (Sub, a, b) }
  | a = between_bound STAR  b = between_bound { E_binop (Mul, a, b) }
  | a = between_bound SLASH b = between_bound { E_binop (Div, a, b) }
  | a = between_bound CONCAT    b = between_bound { E_binop (Concat, a, b) }
  | a = between_bound PERCENT   b = between_bound { E_binop (Mod, a, b) }
  | a = between_bound AMPERSAND b = between_bound { E_binop (Bit_and, a, b) }
  | a = between_bound PIPE      b = between_bound { E_binop (Bit_or, a, b) }
  | a = between_bound LSHIFT    b = between_bound { E_binop (Lshift, a, b) }
  | a = between_bound RSHIFT    b = between_bound { E_binop (Rshift, a, b) }
  | TILDE e = between_bound %prec TILDE    { E_bitnot e }
  | MINUS e = between_bound %prec UMINUS   { E_neg e }
  | a = between_bound LIKE b = between_bound { E_binop (Like, a, b) }
  | a = between_bound GLOB b = between_bound { E_binop (Glob, a, b) }
  | LPAREN e = expr RPAREN                 { e }
  | e = case_expr                           { e }
  | QUESTION        { E_param Param_anon }
  | i = IPARAM      { E_param (Param_index i) }
  | n = NAMED_PARAM { E_param (Param_name n) }

expr:
  | l = literal                       { E_lit l }
  | name = IDENT                      { E_col name }
  | t = IDENT DOT c = IDENT           { E_tbl_col (t, c) }
  | e = agg_expr                      { e }
  | e = scalar_expr                   { e }
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
  | a = expr CONCAT    b = expr       { E_binop (Concat, a, b) }
  | a = expr PERCENT   b = expr       { E_binop (Mod, a, b) }
  | a = expr AMPERSAND b = expr       { E_binop (Bit_and, a, b) }
  | a = expr PIPE      b = expr       { E_binop (Bit_or, a, b) }
  | a = expr LSHIFT    b = expr       { E_binop (Lshift, a, b) }
  | a = expr RSHIFT    b = expr       { E_binop (Rshift, a, b) }
  | TILDE e = expr %prec TILDE        { E_bitnot e }
  | MINUS e = expr %prec UMINUS       { E_neg e }
  | a = expr LIKE b = expr             { E_binop (Like, a, b) }
  | a = expr NOT LIKE b = expr %prec LIKE { E_not (E_binop (Like, a, b)) }
  | a = expr GLOB b = expr             { E_binop (Glob, a, b) }
  | a = expr NOT GLOB b = expr %prec GLOB { E_not (E_binop (Glob, a, b)) }
  | a = expr BETWEEN lo = between_bound AND hi = between_bound %prec BETWEEN_PREC
    { E_between (a, lo, hi) }
  | a = expr NOT BETWEEN lo = between_bound AND hi = between_bound %prec BETWEEN_PREC
    { E_not (E_between (a, lo, hi)) }
  | a = expr IN LPAREN s = compound_select RPAREN
    { E_in_select (a, s) }
  | a = expr NOT IN LPAREN s = compound_select RPAREN
    { E_not (E_in_select (a, s)) }
  | a = expr IN LPAREN vals = separated_nonempty_list(COMMA, expr) RPAREN
    { E_in (a, vals) }
  | a = expr NOT IN LPAREN vals = separated_nonempty_list(COMMA, expr) RPAREN
    { E_not (E_in (a, vals)) }
  | EXISTS LPAREN s = compound_select RPAREN
    { E_exists s }
  | e = case_expr { e }
  | e = expr IS NULL                  { E_is_null e }
  | e = expr IS NOT NULL              { E_is_not_null e }
  | t = IDENT MATCH s = STRING_LIT    { E_match (t, s) }
  | LPAREN s = compound_select RPAREN { E_subquery s }
  | LPAREN e = expr RPAREN            { e }
  | QUESTION        { E_param Param_anon }
  | i = IPARAM      { E_param (Param_index i) }
  | n = NAMED_PARAM { E_param (Param_name n) }
