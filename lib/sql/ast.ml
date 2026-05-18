(** Phase 0 SQL AST.
    Covers: CREATE TABLE, INSERT INTO ... VALUES, SELECT ... FROM ... WHERE col = lit.
    Extended in later phases for UPDATE, DELETE, JOINs, etc. *)

type ty =
  | Ty_int   (** INTEGER column type *)
  | Ty_text  (** TEXT column type *)
  | Ty_real  (** REAL (float64) column type *)
  | Ty_blob  (** BLOB (bytes) column type *)

type literal =
  | L_int  of int64
  | L_text of string
  | L_null
  | L_real of float
  | L_blob of bytes

type param =
  | Param_anon            (** ? — assigned next slot in encounter order *)
  | Param_index of int    (** ?1, ?2 ... — explicit 1-based slot *)
  | Param_name  of string (** :name  @name  $name *)

type conflict_action = CA_rollback | CA_abort | CA_fail | CA_ignore | CA_replace

type binop =
  | Eq | Ne | Lt | Le | Gt | Ge   (** comparison *)
  | Add | Sub | Mul | Div          (** arithmetic *)
  | And | Or                       (** logical *)
  | Concat                         (** string concatenation || *)
  | Mod                            (** modulo % *)
  | Bit_and | Bit_or               (** bitwise & | *)
  | Lshift | Rshift                (** shift << >> *)
  | Like | Glob                    (** pattern matching *)

(** Aggregate functions supported in Phase 2 Task 6. *)
type agg_func = Agg_count | Agg_sum | Agg_avg | Agg_min | Agg_max

(** Scalar functions supported in Phase 5 Task 1. *)
type scalar_func =
  | Fn_length
  | Fn_lower
  | Fn_upper
  | Fn_abs
  | Fn_coalesce
  | Fn_ifnull
  | Fn_substr                        (** SUBSTR(s, start[, len]) — 1-indexed *)
  | Fn_trim    | Fn_ltrim  | Fn_rtrim (** TRIM, LTRIM, RTRIM — optional 2nd arg for chars *)
  | Fn_replace                       (** REPLACE(s, old, new) *)
  | Fn_instr                         (** INSTR(s, sub) → 1-indexed position or 0 *)
  | Fn_round                         (** ROUND(n[, digits]) *)
  | Fn_typeof                        (** TYPEOF(x) → 'integer'|'real'|'text'|'blob'|'null' *)
  | Fn_date                              (** DATE(ts[, mod...]) → 'YYYY-MM-DD' *)
  | Fn_time                              (** TIME(ts[, mod...]) → 'HH:MM:SS' *)
  | Fn_datetime                          (** DATETIME(ts[, mod...]) → 'YYYY-MM-DD HH:MM:SS' *)
  | Fn_strftime                          (** STRFTIME(fmt, ts[, mod...]) → formatted string *)
  | Fn_julianday                         (** JULIANDAY(ts[, mod...]) → float *)
  | Fn_unixepoch                         (** UNIXEPOCH(ts[, mod...]) → integer *)

type set_op = Union | Union_all | Intersect | Except

type order_dir = Asc | Desc

type join_kind = Inner | Left

type table_constraint =
  | TC_unique      of string list   (** UNIQUE(col1, col2, ...) *)
  | TC_primary_key of string list   (** PRIMARY KEY(col1, col2, ...) *)

(** Expressions, statements, and column_def are mutually recursive because
    column_def.check embeds an [expr], and subquery expressions embed a [stmt]. *)
type expr =
  | E_lit         of literal
  | E_col         of string                (** unqualified column reference *)
  | E_tbl_col     of string * string       (** qualified: table.col *)
  | E_binop       of binop * expr * expr
  | E_not         of expr
  | E_is_null     of expr
  | E_is_not_null of expr
  | E_neg         of expr                  (** unary minus *)
  | E_bitnot      of expr                  (** bitwise NOT ~ *)
  | E_between     of expr * expr * expr   (** subject BETWEEN lo AND hi *)
  | E_in          of expr * expr list     (** subject IN (val1, val2, ...) *)
  | E_agg         of agg_func * expr option
    (** Aggregate call; [None] argument means [COUNT( * )]. *)
  | E_func        of scalar_func * expr list
    (** Scalar function call. *)
  | E_param       of param
    (** parameter: ?, ?1, :name, @name, $name *)
  | E_match       of string * string
    (** [E_match (table_name, query_string)]: [WHERE table MATCH 'query'] *)
  | E_subquery  of stmt               (** scalar subquery: (SELECT ...) in expr position *)
  | E_exists    of stmt               (** EXISTS (SELECT ...) *)
  | E_in_select of expr * stmt        (** x IN (SELECT ...) *)
  | E_case of {
      scrutinee : expr option;           (** None = searched form, Some = simple form *)
      branches  : (expr * expr) list;    (** (WHEN condition/value, THEN result) *)
      else_     : expr option;
    }
  | E_cast of expr * ty
    (** CAST(expr AS type) — SQLite type coercion *)

and order_key = {
  expr : expr;
  dir  : order_dir;
}

and join_clause = {
  kind  : join_kind;
  table : string;                (** right-side table name *)
  alias : string option;         (** optional alias — used in E_tbl_col resolution *)
  on    : expr;                  (** join condition (predicate over both tables) *)
}

and stmt =
  | S_create_table of {
      name        : string;
      columns     : column_def list;
      constraints : table_constraint list;
    }
  | S_insert of {
      table       : string;
      columns     : string list;   (** named columns; empty = "all in order" *)
      values      : expr list;
      on_conflict : conflict_action option;
      returning   : expr list;   (** empty = no RETURNING *)
    }
  | S_select of {
      distinct    : bool;
      proj        : [ `All | `Cols of string list | `Exprs of (expr * string option) list ];
        (** [`Exprs] supports arbitrary projection expressions (used for
            aggregates).  Plain column projection still parses to
            [`Cols]. *)
      table       : string;
      table_alias : string option;     (** optional AS alias for the FROM table *)
      joins       : join_clause list;  (** empty list = no joins *)
      where       : expr option;
      group_by    : string list;       (** column names; empty = no GROUP BY *)
      having      : expr option;       (** HAVING predicate (may reference aggregates) *)
      order       : order_key list;    (** empty = no ORDER BY *)
      limit       : int option;
      offset      : int option;
    }
  | S_create_index of {
      name    : string;
      table   : string;
      columns : string list;
      unique  : bool;
    }
  | S_update of {
      table       : string;
      assignments : (string * expr) list;   (** [(col_name, new_value_expr)] *)
      where       : expr option;
      returning   : expr list;
    }
  | S_delete of {
      table     : string;
      where     : expr option;
      returning : expr list;
    }
  | S_drop_table of {
      name : string;
    }
  | S_drop_index of {
      name : string;
    }
  | S_alter_table of {
      table  : string;
      action : alter_action;
    }
  | S_begin
  | S_commit
  | S_rollback
  | S_compound of {
      op    : set_op;
      left  : stmt;
      right : stmt;
    }
  | S_create_fts_table of {
      name    : string;
      columns : string list;
    }
  | S_pragma of pragma_kind

  | S_const_select of {
      (** FROM-less SELECT that evaluates constant expressions — returns one row.
          Used when a scalar subquery appears in projection position at the
          outermost query level (e.g. [SELECT (SELECT max(v) FROM t)]). *)
      exprs : expr list;
    }
  | S_with_cte of {
      name  : string;
      def   : stmt;
      query : stmt;
    }

and pragma_kind =
  | Pragma_table_info of string
  | Pragma_index_list of string

and column_def = {
  name        : string;
  ty          : ty;
  not_null    : bool;
  primary_key : bool;
  default     : literal option;  (* None = no DEFAULT *)
  check       : expr option;     (* None = no CHECK constraint *)
}

and alter_action =
  | AA_add_column    of column_def
  | AA_rename_table  of string              (* new table name *)
  | AA_rename_column of string * string     (* old_col_name * new_col_name *)

let binop_to_sql = function
  | Eq -> "=" | Ne -> "!=" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | And -> "AND" | Or -> "OR" | Concat -> "||"
  | Bit_and -> "&" | Bit_or -> "|" | Lshift -> "<<" | Rshift -> ">>"
  | Like -> "LIKE" | Glob -> "GLOB"

let func_to_sql = function
  | Fn_length -> "LENGTH" | Fn_lower -> "LOWER" | Fn_upper -> "UPPER"
  | Fn_abs -> "ABS" | Fn_coalesce -> "COALESCE" | Fn_ifnull -> "IFNULL"
  | Fn_substr -> "SUBSTR" | Fn_trim -> "TRIM" | Fn_ltrim -> "LTRIM"
  | Fn_rtrim -> "RTRIM" | Fn_replace -> "REPLACE" | Fn_instr -> "INSTR"
  | Fn_round -> "ROUND" | Fn_typeof -> "TYPEOF"
  | Fn_date -> "DATE" | Fn_time -> "TIME" | Fn_datetime -> "DATETIME"
  | Fn_strftime -> "STRFTIME" | Fn_julianday -> "JULIANDAY"
  | Fn_unixepoch -> "UNIXEPOCH"

let rec expr_to_sql = function
  | E_lit (L_int n)  -> Int64.to_string n
  | E_lit (L_text s) ->
    Printf.sprintf "'%s'" (String.concat "''" (String.split_on_char '\'' s))
  | E_lit L_null     -> "NULL"
  | E_lit (L_real f) -> Printf.sprintf "%.17g" f
  | E_lit (L_blob _) -> failwith "expr_to_sql: BLOB literals not supported in CHECK constraints"
  | E_col name       -> name
  | E_tbl_col (t, c) -> Printf.sprintf "%s.%s" t c
  | E_param Param_anon -> "?"
  | E_param (Param_index i) -> Printf.sprintf "?%d" i
  | E_param (Param_name n)  -> Printf.sprintf ":%s" n
  | E_binop (op, a, b) ->
    Printf.sprintf "(%s %s %s)" (expr_to_sql a) (binop_to_sql op) (expr_to_sql b)
  | E_not e          -> Printf.sprintf "NOT (%s)" (expr_to_sql e)
  | E_is_null e      -> Printf.sprintf "(%s) IS NULL" (expr_to_sql e)
  | E_is_not_null e  -> Printf.sprintf "(%s) IS NOT NULL" (expr_to_sql e)
  | E_neg e          -> Printf.sprintf "(-(%s))" (expr_to_sql e)
  | E_bitnot e       -> Printf.sprintf "(~(%s))" (expr_to_sql e)
  | E_between (x, lo, hi) ->
    Printf.sprintf "(%s) BETWEEN (%s) AND (%s)"
      (expr_to_sql x) (expr_to_sql lo) (expr_to_sql hi)
  | E_in (x, vals) ->
    Printf.sprintf "(%s) IN (%s)" (expr_to_sql x)
      (String.concat ", " (List.map expr_to_sql vals))
  | E_func (f, args) ->
    Printf.sprintf "%s(%s)" (func_to_sql f)
      (String.concat ", " (List.map expr_to_sql args))
  | E_case { scrutinee; branches; else_ } ->
    let scr = match scrutinee with
      | None   -> ""
      | Some e -> " " ^ expr_to_sql e
    in
    let brs = String.concat " " (List.map (fun (cond, res) ->
      Printf.sprintf "WHEN %s THEN %s" (expr_to_sql cond) (expr_to_sql res)
    ) branches) in
    let el = match else_ with
      | None   -> ""
      | Some e -> " ELSE " ^ expr_to_sql e
    in
    Printf.sprintf "CASE%s %s%s END" scr brs el
  | E_cast (e, ty) ->
    let tn = match ty with
      | Ty_int  -> "INTEGER" | Ty_text -> "TEXT"
      | Ty_real -> "REAL"    | Ty_blob -> "BLOB"
    in
    Printf.sprintf "CAST(%s AS %s)" (expr_to_sql e) tn
  | E_agg _ | E_match _ | E_subquery _ | E_exists _ | E_in_select _ ->
    failwith "expr_to_sql: unsupported expression form"
