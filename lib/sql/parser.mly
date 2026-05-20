%{
  open Ast

  type col_constraint =
    | Col_not_null
    | Col_primary_key
    | Col_default of literal
    | Col_check   of expr
    | Col_fk_ref  of string * string option * Ast.fk_action * Ast.fk_action
    | Col_generated of expr * [`Stored | `Virtual]

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
%token BEGIN COMMIT ROLLBACK SAVEPOINT RELEASE
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
%token BETWEEN IN EXISTS IF
%token CASE WHEN THEN ELSE END
%token AS CAST NULLIF IIF WITH
%token CONFLICT DO VIEW
%token TRIGGER BEFORE AFTER
%token CASCADE RESTRICT
%token OVER PARTITION RECURSIVE COLLATE
%token PRECEDING FOLLOWING
%token CHECK
%token REFERENCES FOREIGN
%token CEIL FLOOR SQRT POW EXP LN LOG LOG2 LOG10 SIGN TRUNC PI
%token SIN COS TAN ASIN ACOS ATAN ATAN2 DEGREES RADIANS
%token JSON_EXTRACT JSON_OBJECT_FN JSON_ARRAY_FN JSON_TYPE JSON_VALID
%token JSON_SET JSON_INSERT_FN JSON_REPLACE_FN JSON_REMOVE
%token NULLS
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
%nonassoc COLLATE_PREC
%nonassoc TILDE UMINUS

%start <Ast.stmt> stmt_eof
%start <Ast.expr> expr_only

%%

expr_only:
  | e = expr EOF { e }

stmt_eof:
  | s = stmt SEMI? EOF { s }

(* Allow SQL keywords to be used as identifiers in non-keyword positions.
   This is necessary because many keywords (SUM, DESC, etc.) are commonly
   used as column aliases, CTE names, table names, and column names. *)
any_ident:
  | id = IDENT    { id }
  | ASC           { "asc" }
  | DESC          { "desc" }
  | SUM           { "sum" }
  | AVG           { "avg" }
  | MIN           { "min" }
  | MAX           { "max" }
  | COUNT         { "count" }
  | ALL           { "all" }
  | ADD           { "add" }
  | AFTER         { "after" }
  | BEFORE        { "before" }
  | BEGIN         { "begin" }
  | CASCADE       { "cascade" }
  | COLUMN        { "column" }
  | CONFLICT      { "conflict" }
  | DEFAULT       { "default" }
  | DO            { "do" }
  | END           { "end" }
  | FAIL          { "fail" }
  | FOLLOWING     { "following" }
  | IGNORE        { "ignore" }
  | INDEX         { "index" }
  | INNER         { "inner" }
  | INTO          { "into" }
  | IS            { "is" }
  | JOIN          { "join" }
  | KEY           { "key" }
  | LEFT          { "left" }
  | MATCH         { "match" }
  | NULLS         { "nulls" }
  | OFFSET        { "offset" }
  | OUTER         { "outer" }
  | OVER          { "over" }
  | PARTITION     { "partition" }
  | PRECEDING     { "preceding" }
  | PRIMARY       { "primary" }
  | RECURSIVE     { "recursive" }
  | RELEASE       { "release" }
  | RENAME        { "rename" }
  | REPLACE       { "replace" }
  | RESTRICT      { "restrict" }
  | RETURNING     { "returning" }
  | ROLLBACK      { "rollback" }
  | SAVEPOINT     { "savepoint" }
  | SET           { "set" }
  | TABLE         { "table" }
  | TO            { "to" }
  | TRIGGER       { "trigger" }
  | UNIQUE        { "unique" }
  | UPDATE        { "update" }
  | USING         { "using" }
  | VALUES        { "values" }
  | VIEW          { "view" }
  | VIRTUAL       { "virtual" }
  | ABORT         { "abort" }

stmt:
  | s = with_cte          { s }
  | s = create_view       { s }
  | s = drop_view         { s }
  | s = create_trigger    { s }
  | s = drop_trigger      { s }
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
  | s = savepoint_stmt    { s }
  | s = release_stmt      { s }
  | s = rollback_to_stmt  { s }
  | s = pragma_stmt       { s }
  | s = alter_table       { s }

with_cte:
  | WITH name = any_ident AS LPAREN def = compound_select RPAREN query = compound_select
    { Ast.S_with_cte { name; def; query; recursive = false } }
  | WITH RECURSIVE name = any_ident AS LPAREN def = compound_select RPAREN query = compound_select
    { Ast.S_with_cte { name; def; query; recursive = true } }

pragma_stmt:
  (* Arg form: PRAGMA name(arg) — must come first to avoid conflicts *)
  | PRAGMA name = any_ident LPAREN arg = any_ident RPAREN
    { match String.lowercase_ascii name with
      | "table_info"       -> Ast.S_pragma (Ast.Pragma_table_info arg)
      | "index_list"       -> Ast.S_pragma (Ast.Pragma_index_list arg)
      | "foreign_key_list" -> Ast.S_pragma (Ast.Pragma_foreign_key_list arg)
      | _ -> failwith (Printf.sprintf "unknown pragma: %s(%s)" name arg) }

  (* Setter form: PRAGMA name = value *)
  | PRAGMA name = any_ident EQ value = pragma_value
    { match String.lowercase_ascii name with
      | "user_version" ->
        (try Ast.S_pragma (Ast.Pragma_user_version_set (Int64.of_string value))
         with Failure _ ->
           failwith (Printf.sprintf "PRAGMA user_version: expected integer, got %s" value))
      | _ -> Ast.S_pragma (Ast.Pragma_set (name, value)) }

  (* Bare getter form: PRAGMA name — new in Phase 26 *)
  | PRAGMA name = any_ident
    { match String.lowercase_ascii name with
      | "foreign_keys"    -> Ast.S_pragma Ast.Pragma_foreign_keys
      | "user_version"    -> Ast.S_pragma Ast.Pragma_user_version
      | "journal_mode"    -> Ast.S_pragma Ast.Pragma_journal_mode
      | "integrity_check" -> Ast.S_pragma Ast.Pragma_integrity_check
      | _                 -> Ast.S_pragma (Ast.Pragma_set (name, "")) }

pragma_value:
  | v = IDENT              { v }
  | ON                     { "on" }
  | n = INT_LIT            { Int64.to_string n }

drop_table:
  | DROP TABLE name = any_ident
    { S_drop_table { name; if_exists = false } }
  | DROP TABLE IF EXISTS name = any_ident
    { S_drop_table { name; if_exists = true } }

drop_index:
  | DROP INDEX name = any_ident
    { S_drop_index { name; if_exists = false } }
  | DROP INDEX IF EXISTS name = any_ident
    { S_drop_index { name; if_exists = true } }

create_view:
  | CREATE VIEW name = any_ident AS query = compound_select
    { Ast.S_create_view { name; query } }

drop_view:
  | DROP VIEW name = any_ident
    { Ast.S_drop_view { name; if_exists = false } }
  | DROP VIEW IF EXISTS name = any_ident
    { Ast.S_drop_view { name; if_exists = true } }

create_trigger:
  | CREATE TRIGGER name = any_ident
    timing = trigger_timing
    event  = trigger_event
    ON table = any_ident
    when_  = trigger_when
    BEGIN body = trigger_body END
    { Ast.S_create_trigger { name; timing; event; table; when_; body } }

drop_trigger:
  | DROP TRIGGER name = any_ident
    { Ast.S_drop_trigger { name; if_exists = false } }
  | DROP TRIGGER IF EXISTS name = any_ident
    { Ast.S_drop_trigger { name; if_exists = true } }

trigger_timing:
  | BEFORE { Ast.TT_before }
  | AFTER  { Ast.TT_after  }

trigger_event:
  | INSERT { Ast.TE_insert }
  | UPDATE { Ast.TE_update }
  | DELETE { Ast.TE_delete }

trigger_when:
  | WHEN e = expr { Some e }
  |               { None   }

trigger_body:
  | s = stmt SEMI             { [s] }
  | s = stmt SEMI rest = trigger_body { s :: rest }

(* Single referential action: CASCADE | RESTRICT | SET NULL | SET DEFAULT | NO ACTION *)
fk_ref_action:
  | CASCADE       { Ast.FA_cascade }
  | RESTRICT      { Ast.FA_restrict }
  | SET NULL      { Ast.FA_set_null }
  | SET DEFAULT   { Ast.FA_set_default }
  | IDENT IDENT   { Ast.FA_no_action }
  | IDENT         { Ast.FA_restrict }

(* Optional ON DELETE / ON UPDATE pair in any order *)
fk_on_clauses:
  | ON DELETE od = fk_ref_action ON UPDATE ou = fk_ref_action { (od, ou) }
  | ON UPDATE ou = fk_ref_action ON DELETE od = fk_ref_action { (od, ou) }
  | ON DELETE od = fk_ref_action  { (od, Ast.FA_no_action) }
  | ON UPDATE ou = fk_ref_action  { (Ast.FA_no_action, ou) }
  |                               { (Ast.FA_no_action, Ast.FA_no_action) }

alter_table:
  | ALTER TABLE table = any_ident ADD COLUMN col = column_def
    { Ast.S_alter_table { table; action = Ast.AA_add_column col } }
  | ALTER TABLE table = any_ident ADD col = column_def
    { Ast.S_alter_table { table; action = Ast.AA_add_column col } }
  | ALTER TABLE table = any_ident RENAME TO new_name = any_ident
    { Ast.S_alter_table { table; action = Ast.AA_rename_table new_name } }
  | ALTER TABLE table = any_ident RENAME COLUMN old_col = any_ident TO new_col = any_ident
    { Ast.S_alter_table { table; action = Ast.AA_rename_column (old_col, new_col) } }
  | ALTER TABLE table = any_ident RENAME old_col = any_ident TO new_col = any_ident
    { Ast.S_alter_table { table; action = Ast.AA_rename_column (old_col, new_col) } }
  | ALTER TABLE table = any_ident DROP COLUMN col = any_ident
    { Ast.S_alter_table { table; action = Ast.AA_drop_column col } }
  | ALTER TABLE table = any_ident DROP col = any_ident
    { Ast.S_alter_table { table; action = Ast.AA_drop_column col } }

begin_stmt:
  | BEGIN    { S_begin }

commit_stmt:
  | COMMIT   { S_commit }

rollback_stmt:
  | ROLLBACK { S_rollback }

savepoint_stmt:
  | SAVEPOINT name = any_ident { Ast.S_savepoint name }

release_stmt:
  | RELEASE name = any_ident { Ast.S_release name }

rollback_to_stmt:
  | ROLLBACK TO name = any_ident { Ast.S_rollback_to name }

table_item:
  | col = column_def
    { TI_col col }
  | UNIQUE LPAREN cols = separated_nonempty_list(COMMA, any_ident) RPAREN
    { TI_constraint (Ast.TC_unique cols) }
  | PRIMARY KEY LPAREN cols = separated_nonempty_list(COMMA, any_ident) RPAREN
    { TI_constraint (Ast.TC_primary_key cols) }
  | FOREIGN KEY LPAREN local_cols = separated_nonempty_list(COMMA, any_ident) RPAREN
      REFERENCES parent_table = any_ident LPAREN parent_cols = separated_nonempty_list(COMMA, any_ident) RPAREN
      oc = fk_on_clauses
    { let (on_delete, on_update) = oc in
      TI_constraint (Ast.TC_foreign_key {
        local_cols; parent_table; parent_cols; on_delete; on_update;
      }) }

create_table:
  | CREATE TABLE name = any_ident LPAREN items = separated_nonempty_list(COMMA, table_item) RPAREN
    { let cols = List.filter_map (function TI_col c -> Some c | _ -> None) items in
      let cons = List.filter_map (function TI_constraint c -> Some c | _ -> None) items in
      S_create_table { name; columns = cols; constraints = cons; if_not_exists = false } }
  | CREATE TABLE IF NOT EXISTS name = any_ident LPAREN items = separated_nonempty_list(COMMA, table_item) RPAREN
    { let cols = List.filter_map (function TI_col c -> Some c | _ -> None) items in
      let cons = List.filter_map (function TI_constraint c -> Some c | _ -> None) items in
      S_create_table { name; columns = cols; constraints = cons; if_not_exists = true } }

create_fts_table:
  | CREATE VIRTUAL TABLE name = any_ident USING FTS5
      LPAREN cols = separated_nonempty_list(COMMA, any_ident) RPAREN
    { S_create_fts_table { name; columns = cols } }

index_col_expr:
  | e = expr { e }

create_index:
  | CREATE INDEX name = any_ident ON table = any_ident
      LPAREN cols = separated_nonempty_list(COMMA, index_col_expr) RPAREN wh = where_opt
    { S_create_index { name; table; columns = cols; where_clause = wh; unique = false; if_not_exists = false } }
  | CREATE UNIQUE INDEX name = any_ident ON table = any_ident
      LPAREN cols = separated_nonempty_list(COMMA, index_col_expr) RPAREN wh = where_opt
    { S_create_index { name; table; columns = cols; where_clause = wh; unique = true; if_not_exists = false } }
  | CREATE INDEX IF NOT EXISTS name = any_ident ON table = any_ident
      LPAREN cols = separated_nonempty_list(COMMA, index_col_expr) RPAREN wh = where_opt
    { S_create_index { name; table; columns = cols; where_clause = wh; unique = false; if_not_exists = true } }
  | CREATE UNIQUE INDEX IF NOT EXISTS name = any_ident ON table = any_ident
      LPAREN cols = separated_nonempty_list(COMMA, index_col_expr) RPAREN wh = where_opt
    { S_create_index { name; table; columns = cols; where_clause = wh; unique = true; if_not_exists = true } }

column_def:
  | name = any_ident ty = col_ty cs = column_constraint*
    { let not_null    = List.mem Col_not_null cs in
      let primary_key = List.mem Col_primary_key cs in
      let default     = List.fold_left (fun acc c ->
          match c with Col_default l -> Some l | _ -> acc) None cs in
      let check       = List.fold_left (fun acc c ->
          match c with Col_check e -> Some e | _ -> acc) None cs in
      let fk_ref      = List.fold_left (fun acc c ->
          match c with
          | Col_fk_ref (t, col_opt, od, ou) ->
            Some (t, Option.value ~default:"" col_opt, od, ou)
          | _ -> acc) None cs in
      let generated_as = List.fold_left (fun acc c ->
          match c with Col_generated (e, s) -> Some (e, s) | _ -> acc) None cs in
      { name; ty; not_null; primary_key; default; check; fk_ref; generated_as } }

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
  | REFERENCES t = any_ident oc = fk_on_clauses
    { let (od, ou) = oc in Col_fk_ref (t, None, od, ou) }
  | REFERENCES t = any_ident LPAREN c = any_ident RPAREN oc = fk_on_clauses
    { let (od, ou) = oc in Col_fk_ref (t, Some c, od, ou) }
  | gen = any_ident always = any_ident AS LPAREN e = expr RPAREN storage = generated_storage
    { if String.uppercase_ascii gen <> "GENERATED"
         || String.uppercase_ascii always <> "ALWAYS"
      then failwith (Printf.sprintf
             "expected GENERATED ALWAYS AS (...), got: %s %s AS" gen always);
      Col_generated (e, storage) }

generated_storage:
  | id = any_ident { if String.uppercase_ascii id = "STORED" then `Stored
                     else if String.uppercase_ascii id = "VIRTUAL" then `Virtual
                     else failwith (Printf.sprintf "expected STORED or VIRTUAL, got %s" id) }
  |                { `Virtual }

def_value:
  | n = INT_LIT              { L_int n }
  | s = STRING_LIT           { L_text s }
  | NULL                     { L_null }
  | f = FLOAT_LIT            { L_real f }
  | MINUS n = INT_LIT        { L_int (Int64.neg n) }
  | MINUS f = FLOAT_LIT      { L_real (-. f) }
  | id = any_ident           { match String.uppercase_ascii id with
                               | "CURRENT_TIMESTAMP" -> L_current_timestamp
                               | "CURRENT_DATE"      -> L_current_date
                               | "CURRENT_TIME"      -> L_current_time
                               | _ -> failwith (Printf.sprintf "unknown DEFAULT value: %s" id) }

opt_conflict:
  | OR REPLACE  { Some Ast.CA_replace  }
  | OR IGNORE   { Some Ast.CA_ignore   }
  | OR ABORT    { Some Ast.CA_abort    }
  | OR FAIL     { Some Ast.CA_fail     }
  | OR ROLLBACK { Some Ast.CA_rollback }
  |             { None }

value_row:
  | LPAREN vals = separated_nonempty_list(COMMA, expr) RPAREN { vals }

insert:
  | INSERT oc = opt_conflict INTO table = any_ident
      LPAREN cols = separated_nonempty_list(COMMA, any_ident) RPAREN
      VALUES rows = separated_nonempty_list(COMMA, value_row)
      upsert = opt_upsert
      ret = opt_returning
    { Ast.S_insert { table; columns = cols; values = rows; on_conflict = oc; returning = ret;
                     upsert_update = upsert } }
  | INSERT oc = opt_conflict INTO table = any_ident
      VALUES rows = separated_nonempty_list(COMMA, value_row)
      upsert = opt_upsert
      ret = opt_returning
    { Ast.S_insert { table; columns = []; values = rows; on_conflict = oc; returning = ret;
                     upsert_update = upsert } }

opt_returning:
  | RETURNING exprs = separated_nonempty_list(COMMA, expr) { exprs }
  |                                                         { [] }

opt_upsert:
  | ON CONFLICT LPAREN cols = separated_nonempty_list(COMMA, any_ident) RPAREN
    DO UPDATE SET assigns = separated_nonempty_list(COMMA, assignment)
    { Some Ast.{ conflict_cols = cols; assignments = assigns } }
  |  { None }

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
          | `Exprs es -> es
          | `Cols names -> List.map (fun n -> (E_col n, None)) names
          | `All -> []
        in
        S_const_select { exprs } }

from_tail:
  | FROM table = any_ident tbl_alias = option(preceded(AS, any_ident))
      js = join_clauses wh = where_opt
      gb = group_by_clause hv = having_clause ob = order_by_clause lim = limit_clause
    { let (limit, offset) = lim in
      Some (table, tbl_alias, js, wh, gb, hv, ob, limit, offset) }
  |   { None }

join_clauses:
  |                                  { [] }
  | j = join_clause rest = join_clauses { j :: rest }

join_clause:
  | INNER JOIN t = any_ident alias = option(preceded(AS, any_ident)) ON e = expr
    { { kind = Inner; table = t; alias; on = e } }
  | LEFT JOIN t = any_ident alias = option(preceded(AS, any_ident)) ON e = expr
    { { kind = Left;  table = t; alias; on = e } }
  | LEFT OUTER JOIN t = any_ident alias = option(preceded(AS, any_ident)) ON e = expr
    { { kind = Left;  table = t; alias; on = e } }
  | JOIN t = any_ident alias = option(preceded(AS, any_ident)) ON e = expr
    { { kind = Inner; table = t; alias; on = e } }

update:
  | UPDATE table = any_ident SET
      assignments = separated_nonempty_list(COMMA, assignment)
      wh = where_opt
      ob = order_by_clause
      lim = limit_clause
      ret = opt_returning
    { let (limit, offset) = lim in
      S_update { table; assignments; where = wh; order = ob; limit; offset; returning = ret } }

delete:
  | DELETE FROM table = any_ident wh = where_opt ob = order_by_clause lim = limit_clause ret = opt_returning
    { let (limit, offset) = lim in
      S_delete { table; where = wh; order = ob; limit; offset; returning = ret } }

assignment:
  | col = any_ident EQ value = expr { (col, value) }

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
  | CEIL    LPAREN e = expr RPAREN                            { E_func (Fn_ceil,    [e]) }
  | FLOOR   LPAREN e = expr RPAREN                            { E_func (Fn_floor,   [e]) }
  | SQRT    LPAREN e = expr RPAREN                            { E_func (Fn_sqrt,    [e]) }
  | POW     LPAREN b = expr COMMA e = expr RPAREN             { E_func (Fn_pow,     [b; e]) }
  | EXP     LPAREN e = expr RPAREN                            { E_func (Fn_exp,     [e]) }
  | LN      LPAREN e = expr RPAREN                            { E_func (Fn_ln,      [e]) }
  | LOG     LPAREN e = expr RPAREN                            { E_func (Fn_log,     [e]) }
  | LOG     LPAREN b = expr COMMA x = expr RPAREN             { E_func (Fn_log,     [b; x]) }
  | LOG2    LPAREN e = expr RPAREN                            { E_func (Fn_log2,    [e]) }
  | LOG10   LPAREN e = expr RPAREN                            { E_func (Fn_log10,   [e]) }
  | SIGN    LPAREN e = expr RPAREN                            { E_func (Fn_sign,    [e]) }
  | TRUNC   LPAREN e = expr RPAREN                            { E_func (Fn_trunc,   [e]) }
  | TRUNC   LPAREN e = expr COMMA d = expr RPAREN             { E_func (Fn_trunc,   [e; d]) }
  | PI      LPAREN RPAREN                                     { E_func (Fn_pi,      []) }
  | SIN     LPAREN e = expr RPAREN                            { E_func (Fn_sin,     [e]) }
  | COS     LPAREN e = expr RPAREN                            { E_func (Fn_cos,     [e]) }
  | TAN     LPAREN e = expr RPAREN                            { E_func (Fn_tan,     [e]) }
  | ASIN    LPAREN e = expr RPAREN                            { E_func (Fn_asin,    [e]) }
  | ACOS    LPAREN e = expr RPAREN                            { E_func (Fn_acos,    [e]) }
  | ATAN    LPAREN e = expr RPAREN                            { E_func (Fn_atan,    [e]) }
  | ATAN2   LPAREN y = expr COMMA x = expr RPAREN             { E_func (Fn_atan2,   [y; x]) }
  | DEGREES LPAREN e = expr RPAREN                            { E_func (Fn_degrees, [e]) }
  | RADIANS LPAREN e = expr RPAREN                            { E_func (Fn_radians, [e]) }
  | JSON_EXTRACT LPAREN j = expr COMMA p = expr RPAREN
    { E_func (Fn_json_extract, [j; p]) }
  | JSON_OBJECT_FN LPAREN args = separated_list(COMMA, expr) RPAREN
    { E_func (Fn_json_object, args) }
  | JSON_ARRAY_FN LPAREN args = separated_list(COMMA, expr) RPAREN
    { E_func (Fn_json_array, args) }
  | JSON_TYPE LPAREN e = expr RPAREN
    { E_func (Fn_json_type, [e]) }
  | JSON_TYPE LPAREN e = expr COMMA p = expr RPAREN
    { E_func (Fn_json_type, [e; p]) }
  | JSON_VALID LPAREN e = expr RPAREN
    { E_func (Fn_json_valid, [e]) }
  | JSON_SET LPAREN args = separated_nonempty_list(COMMA, expr) RPAREN
    { E_func (Fn_json_set, args) }
  | JSON_INSERT_FN LPAREN args = separated_nonempty_list(COMMA, expr) RPAREN
    { E_func (Fn_json_insert, args) }
  | JSON_REPLACE_FN LPAREN args = separated_nonempty_list(COMMA, expr) RPAREN
    { E_func (Fn_json_replace, args) }
  | JSON_REMOVE LPAREN args = separated_nonempty_list(COMMA, expr) RPAREN
    { E_func (Fn_json_remove, args) }

proj_item:
  | e = expr AS alias = any_ident { `ExprA (e, Some alias) }
  | e = expr { match e with E_col name -> `Col name | _ -> `ExprA (e, None) }

where_opt:
  |                { None }
  | WHERE e = expr { Some e }

group_by_clause:
  |                                                                 { [] }
  | GROUP BY cs = separated_nonempty_list(COMMA, any_ident)        { cs }

having_clause:
  |                                                                 { None }
  | HAVING e = expr                                                 { Some e }

order_by_clause:
  |                                                                 { [] }
  | ORDER BY keys = separated_nonempty_list(COMMA, order_key)      { keys }

nulls_clause:
  | NULLS fkw = IDENT
    { match String.uppercase_ascii fkw with
      | "FIRST" -> `Nulls_first
      | "LAST"  -> `Nulls_last
      | _ -> failwith (Printf.sprintf "expected FIRST or LAST after NULLS, got: %s" fkw) }

order_key:
  | e = expr                               { { expr = e; dir = Asc;  nulls = None } }
  | e = expr ASC                           { { expr = e; dir = Asc;  nulls = None } }
  | e = expr DESC                          { { expr = e; dir = Desc; nulls = None } }
  | e = expr nc = nulls_clause             { { expr = e; dir = Asc;  nulls = Some nc } }
  | e = expr ASC  nc = nulls_clause        { { expr = e; dir = Asc;  nulls = Some nc } }
  | e = expr DESC nc = nulls_clause        { { expr = e; dir = Desc; nulls = Some nc } }

limit_clause:
  |                                         { (None, None) }
  | LIMIT n = INT_LIT                       { (Some (Int64.to_int n), None) }
  | LIMIT n = INT_LIT OFFSET m = INT_LIT   { (Some (Int64.to_int n), Some (Int64.to_int m)) }

agg_or_window_expr:
  | COUNT LPAREN STAR RPAREN ow = option(preceded(OVER, window_spec))
    { match ow with
      | None   -> E_agg (Agg_count, None)
      | Some w -> E_window { func = WF_agg Agg_count; args = []; window = w } }
  | COUNT LPAREN e = expr RPAREN ow = option(preceded(OVER, window_spec))
    { match ow with
      | None   -> E_agg (Agg_count, Some e)
      | Some w -> E_window { func = WF_agg Agg_count; args = [e]; window = w } }
  | SUM LPAREN e = expr RPAREN ow = option(preceded(OVER, window_spec))
    { match ow with
      | None   -> E_agg (Agg_sum, Some e)
      | Some w -> E_window { func = WF_agg Agg_sum; args = [e]; window = w } }
  | AVG LPAREN e = expr RPAREN ow = option(preceded(OVER, window_spec))
    { match ow with
      | None   -> E_agg (Agg_avg, Some e)
      | Some w -> E_window { func = WF_agg Agg_avg; args = [e]; window = w } }
  | MIN LPAREN e = expr RPAREN ow = option(preceded(OVER, window_spec))
    { match ow with
      | None   -> E_agg (Agg_min, Some e)
      | Some w -> E_window { func = WF_agg Agg_min; args = [e]; window = w } }
  | MAX LPAREN e = expr RPAREN ow = option(preceded(OVER, window_spec))
    { match ow with
      | None   -> E_agg (Agg_max, Some e)
      | Some w -> E_window { func = WF_agg Agg_max; args = [e]; window = w } }

window_spec:
  | LPAREN pb = partition_clause ob = order_by_clause fs = option(frame_spec) RPAREN
    { Ast.{ partition_by = pb; order_by = ob; frame = fs } }

frame_spec:
  | unit_id = IDENT BETWEEN start = frame_bound AND end_ = frame_bound
    { let unit = match String.uppercase_ascii unit_id with
        | "ROWS"  -> Ast.Frame_rows
        | "RANGE" -> Ast.Frame_range
        | other   -> failwith (Printf.sprintf "expected ROWS or RANGE, got: %s" other)
      in
      Ast.{ unit; start; end_ } }

frame_bound:
  | u = IDENT PRECEDING
    { match String.uppercase_ascii u with
      | "UNBOUNDED" -> Ast.FB_unbounded_preceding
      | other -> failwith (Printf.sprintf "expected UNBOUNDED PRECEDING, got: %s PRECEDING" other) }
  | n = INT_LIT PRECEDING { Ast.FB_preceding (Int64.to_int n) }
  | c = IDENT r = IDENT
    { match String.uppercase_ascii c, String.uppercase_ascii r with
      | "CURRENT", "ROW" -> Ast.FB_current_row
      | _ -> failwith (Printf.sprintf "expected CURRENT ROW, got: %s %s" c r) }
  | n = INT_LIT FOLLOWING { Ast.FB_following (Int64.to_int n) }
  | u = IDENT FOLLOWING
    { match String.uppercase_ascii u with
      | "UNBOUNDED" -> Ast.FB_unbounded_following
      | other -> failwith (Printf.sprintf "expected UNBOUNDED FOLLOWING, got: %s FOLLOWING" other) }

collation_name:
  | id = IDENT
    { match String.uppercase_ascii id with
      | "NOCASE"  -> Ast.Collate_nocase
      | "BINARY"  -> Ast.Collate_binary
      | "RTRIM"   -> Ast.Collate_rtrim
      | other     -> failwith ("unknown collation: " ^ other) }
  | RTRIM  { Ast.Collate_rtrim }

partition_clause:
  |                                                                   { [] }
  | PARTITION BY es = separated_nonempty_list(COMMA, expr)           { es }

window_func_args:
  |                                                                   { [] }
  | es = separated_nonempty_list(COMMA, expr)                        { es }

window_func_name:
  | id = IDENT
    { match String.uppercase_ascii id with
      | "ROW_NUMBER"  -> Ast.WF_row_number
      | "RANK"        -> Ast.WF_rank
      | "DENSE_RANK"  -> Ast.WF_dense_rank
      | "NTILE"       -> Ast.WF_ntile
      | "LAG"         -> Ast.WF_lag
      | "LEAD"        -> Ast.WF_lead
      | "FIRST_VALUE" -> Ast.WF_first_value
      | "LAST_VALUE"  -> Ast.WF_last_value
      | "NTH_VALUE"    -> Ast.WF_nth_value
      | "PERCENT_RANK" -> Ast.WF_percent_rank
      | "CUME_DIST"    -> Ast.WF_cume_dist
      | other          -> failwith (Printf.sprintf "Unknown window function: %s" other) }

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
  | name = any_ident                       { E_col name }
  | t = any_ident DOT c = any_ident       { E_tbl_col (t, c) }
  | e = agg_or_window_expr                 { e }
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
  | name = any_ident                  { E_col name }
  | t = any_ident DOT c = any_ident  { E_tbl_col (t, c) }
  | e = agg_or_window_expr            { e }
  | e = scalar_expr                   { e }
  | func = window_func_name LPAREN args = window_func_args RPAREN OVER ws = window_spec
    { E_window { func; args; window = ws } }
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
  | e = expr COLLATE c = collation_name
      { E_collate (e, c) }             %prec COLLATE_PREC
