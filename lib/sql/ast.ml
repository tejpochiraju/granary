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
  | L_current_timestamp
  | L_current_date
  | L_current_time

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
type agg_func =
  | Agg_count
  | Agg_sum
  | Agg_avg
  | Agg_min
  | Agg_max
  | Agg_group_concat of string option
    (** GROUP_CONCAT(col) → None (default sep ","); GROUP_CONCAT(col, 'sep') → Some sep *)

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
  | Fn_ceil          (** CEIL(x) / CEILING(x) — round up *)
  | Fn_floor         (** FLOOR(x) — round down *)
  | Fn_sqrt          (** SQRT(x) — square root *)
  | Fn_pow           (** POW(x,y) / POWER(x,y) — x^y *)
  | Fn_exp           (** EXP(x) — e^x *)
  | Fn_ln            (** LN(x) — natural log *)
  | Fn_log           (** LOG(x) → ln x; LOG(B,x) → log base B of x *)
  | Fn_log2          (** LOG2(x) — log base 2 *)
  | Fn_log10         (** LOG10(x) — log base 10 *)
  | Fn_sign          (** SIGN(x) → -1 | 0 | 1 as integer *)
  | Fn_trunc         (** TRUNC(x[,d]) — truncate toward zero *)
  | Fn_pi            (** PI() — constant π, zero args *)
  | Fn_sin           (** SIN(x) *)
  | Fn_cos           (** COS(x) *)
  | Fn_tan           (** TAN(x) *)
  | Fn_asin          (** ASIN(x) *)
  | Fn_acos          (** ACOS(x) *)
  | Fn_atan          (** ATAN(x) *)
  | Fn_atan2         (** ATAN2(y,x) — two-argument arctangent *)
  | Fn_degrees       (** DEGREES(x) — radians to degrees *)
  | Fn_radians       (** RADIANS(x) — degrees to radians *)
  | Fn_json_extract   (** json_extract(json, path) *)
  | Fn_json_object    (** json_object(k,v,...) *)
  | Fn_json_array     (** json_array(v,...) *)
  | Fn_json_type      (** json_type(json[,path]) *)
  | Fn_json_valid     (** json_valid(json) → 0|1 *)
  | Fn_json_set     (** json_set(json, path, val[, path, val ...]) *)
  | Fn_json_insert  (** json_insert — insert only if absent *)
  | Fn_json_replace (** json_replace — update only if present *)
  | Fn_json_remove  (** json_remove(json, path[, path ...]) *)
  | Fn_hex       (** HEX(x) — hex encoding of blob, text, or integer *)
  | Fn_char      (** CHAR(x,...) — Unicode code points to UTF-8 string *)
  | Fn_unicode   (** UNICODE(s) — first Unicode code point of string, or NULL *)
  | Fn_printf    (** PRINTF(fmt,...) / FORMAT(fmt,...) — printf-style formatting *)
  | Fn_zeroblob  (** ZEROBLOB(n) — blob of n zero bytes *)

type set_op = Union | Union_all | Intersect | Except

type trigger_timing = TT_before | TT_after | TT_instead_of

type trigger_event  = TE_insert | TE_update | TE_delete

type order_dir = Asc | Desc

type collation = Collate_binary | Collate_nocase | Collate_rtrim

type frame_unit = Frame_rows | Frame_range

type frame_bound =
  | FB_unbounded_preceding
  | FB_preceding of int
  | FB_current_row
  | FB_following of int
  | FB_unbounded_following

type frame_spec = {
  unit  : frame_unit;
  start : frame_bound;
  end_  : frame_bound;
}

type join_kind = Inner | Left

type fk_action = Sqlocaml_catalog.Catalog.fk_action =
  | FA_no_action
  | FA_restrict
  | FA_cascade
  | FA_set_null
  | FA_set_default

type table_constraint =
  | TC_unique      of string list   (** UNIQUE(col1, col2, ...) *)
  | TC_primary_key of string list   (** PRIMARY KEY(col1, col2, ...) *)
  | TC_foreign_key of {
      local_cols   : string list;
      parent_table : string;
      parent_cols  : string list;
      on_delete    : fk_action;
      on_update    : fk_action;
    }  (** FOREIGN KEY(local_cols) REFERENCES parent_table(parent_cols) ON DELETE/UPDATE action *)

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
  | E_window of {
      func   : window_func;
      args   : expr list;
      window : window_spec;
    }
    (** Window function call: FUNC(...) OVER (PARTITION BY ... ORDER BY ...) *)
  | E_collate of expr * collation   (** expr COLLATE collation_name *)
  | E_fts_snippet of {
      table     : string;
      col_idx   : int;
      start_tag : string;
      end_tag   : string;
      ellipsis  : string;
      n_tokens  : int;
    }
    (** snippet(table, col_idx, start_tag, end_tag, ellipsis, n_tokens) *)

and window_func =
  | WF_row_number
  | WF_rank
  | WF_dense_rank
  | WF_ntile
  | WF_lag
  | WF_lead
  | WF_first_value
  | WF_last_value
  | WF_nth_value
  | WF_agg of agg_func
  | WF_percent_rank
  | WF_cume_dist

and window_spec = {
  partition_by : expr list;
  order_by     : order_key list;
  frame        : frame_spec option;
}

and order_key = {
  expr  : expr;
  dir   : order_dir;
  nulls : [`Nulls_first | `Nulls_last] option;
}

and join_clause = {
  kind  : join_kind;
  table : string;                (** right-side table name *)
  alias : string option;         (** optional alias — used in E_tbl_col resolution *)
  on    : expr;                  (** join condition (predicate over both tables) *)
}

and upsert_update = {
  conflict_cols : string list;
  assignments   : (string * expr) list;
}

and stmt =
  | S_create_table of {
      name          : string;
      columns       : column_def list;
      constraints   : table_constraint list;
      if_not_exists : bool;
    }
  | S_insert of {
      table         : string;
      columns       : string list;   (** named columns; empty = "all in order" *)
      values        : expr list list;   (** one inner list per VALUES row *)
      on_conflict   : conflict_action option;
      returning     : expr list;   (** empty = no RETURNING *)
      upsert_update : upsert_update option;
    }
  | S_insert_select of {
      table       : string;
      columns     : string list;          (** empty = all non-generated columns *)
      on_conflict : conflict_action option;
      select      : stmt;
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
      name          : string;
      table         : string;
      columns       : expr list;        (* E_col "name" for plain cols, any expr for expression indexes *)
      where_clause  : expr option;
      unique        : bool;
      if_not_exists : bool;
    }
  | S_update of {
      table       : string;
      assignments : (string * expr) list;   (** [(col_name, new_value_expr)] *)
      where       : expr option;
      order       : order_key list;
      limit       : int option;
      offset      : int option;
      returning   : expr list;
    }
  | S_delete of {
      table     : string;
      where     : expr option;
      order     : order_key list;
      limit     : int option;
      offset    : int option;
      returning : expr list;
    }
  | S_drop_table of {
      name      : string;
      if_exists : bool;
    }
  | S_drop_index of {
      name      : string;
      if_exists : bool;
    }
  | S_alter_table of {
      table  : string;
      action : alter_action;
    }
  | S_begin
  | S_commit
  | S_rollback
  | S_savepoint   of string  (** SAVEPOINT name *)
  | S_release     of string  (** RELEASE name *)
  | S_rollback_to of string  (** ROLLBACK TO name *)
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
          Used when a SELECT has no FROM clause.
          Each entry pairs the expression with an optional column alias. *)
      exprs : (expr * string option) list;
    }
  | S_with_cte of {
      name      : string;
      def       : stmt;
      query     : stmt;
      recursive : bool;
    }
  | S_create_view of {
      name  : string;
      query : stmt;
    }
  | S_drop_view of {
      name      : string;
      if_exists : bool;
    }
  | S_create_trigger of {
      name    : string;
      timing  : trigger_timing;
      event   : trigger_event;
      table   : string;
      when_   : expr option;       (** WHEN clause; None if absent *)
      body    : stmt list;         (** statements between BEGIN…END *)
    }
  | S_drop_trigger of {
      name      : string;
      if_exists : bool;
    }
  | S_explain of {
      analyze : bool;   (** false = EXPLAIN; true = EXPLAIN ANALYZE *)
      stmt    : stmt;
    }

and pragma_kind =
  | Pragma_table_info       of string   (* PRAGMA table_info(tbl) *)
  | Pragma_index_list       of string   (* PRAGMA index_list(tbl) *)
  | Pragma_foreign_key_list of string   (* PRAGMA foreign_key_list(tbl) *)
  | Pragma_foreign_keys                 (* PRAGMA foreign_keys  → read flag *)
  | Pragma_foreign_keys_set of bool    (* PRAGMA foreign_keys = 0/1 → set flag *)
  | Pragma_user_version                 (* PRAGMA user_version  → read from meta *)
  | Pragma_user_version_set of int64    (* PRAGMA user_version = N → write *)
  | Pragma_journal_mode                 (* PRAGMA journal_mode  → "delete" *)
  | Pragma_integrity_check              (* PRAGMA integrity_check → errors or "ok" *)
  | Pragma_set of string * string       (* fallback no-op setter *)

and column_def = {
  name         : string;
  ty           : ty;
  not_null     : bool;
  primary_key  : bool;
  default      : literal option;  (* None = no DEFAULT *)
  check        : expr option;     (* None = no CHECK constraint *)
  fk_ref       : (string * string * fk_action * fk_action) option;
  (** [(parent_table, parent_col, on_delete, on_update)]. None = no FK. *)
  generated_as : (expr * [`Stored | `Virtual]) option;
  (** GENERATED ALWAYS AS (expr) [STORED | VIRTUAL]. None = not generated. *)
}

and alter_action =
  | AA_add_column    of column_def
  | AA_rename_table  of string              (* new table name *)
  | AA_rename_column of string * string     (* old_col_name * new_col_name *)
  | AA_drop_column   of string              (** column name to drop *)

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
  | Fn_ceil -> "CEIL" | Fn_floor -> "FLOOR" | Fn_sqrt -> "SQRT"
  | Fn_pow -> "POW" | Fn_exp -> "EXP" | Fn_ln -> "LN"
  | Fn_log -> "LOG" | Fn_log2 -> "LOG2" | Fn_log10 -> "LOG10"
  | Fn_sign -> "SIGN" | Fn_trunc -> "TRUNC" | Fn_pi -> "PI"
  | Fn_sin -> "SIN" | Fn_cos -> "COS" | Fn_tan -> "TAN"
  | Fn_asin -> "ASIN" | Fn_acos -> "ACOS" | Fn_atan -> "ATAN"
  | Fn_atan2 -> "ATAN2" | Fn_degrees -> "DEGREES" | Fn_radians -> "RADIANS"
  | Fn_json_extract -> "JSON_EXTRACT" | Fn_json_object -> "JSON_OBJECT"
  | Fn_json_array -> "JSON_ARRAY"     | Fn_json_type   -> "JSON_TYPE"
  | Fn_json_valid -> "JSON_VALID"
  | Fn_json_set -> "JSON_SET"         | Fn_json_insert  -> "JSON_INSERT"
  | Fn_json_replace -> "JSON_REPLACE" | Fn_json_remove  -> "JSON_REMOVE"
  | Fn_hex      -> "HEX"
  | Fn_char     -> "CHAR"
  | Fn_unicode  -> "UNICODE"
  | Fn_printf   -> "PRINTF"
  | Fn_zeroblob -> "ZEROBLOB"

let rec expr_to_sql = function
  | E_lit (L_int n)  -> Int64.to_string n
  | E_lit (L_text s) ->
    Printf.sprintf "'%s'" (String.concat "''" (String.split_on_char '\'' s))
  | E_lit L_null     -> "NULL"
  | E_lit (L_real f) -> Printf.sprintf "%.17g" f
  | E_lit (L_blob _) -> failwith "expr_to_sql: BLOB literals not supported in CHECK constraints"
  | E_lit L_current_timestamp -> "CURRENT_TIMESTAMP"
  | E_lit L_current_date      -> "CURRENT_DATE"
  | E_lit L_current_time      -> "CURRENT_TIME"
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
  | E_collate (e, c) ->
    let cname = match c with
      | Collate_nocase -> "NOCASE"
      | Collate_binary -> "BINARY"
      | Collate_rtrim  -> "RTRIM"
    in
    Printf.sprintf "(%s) COLLATE %s" (expr_to_sql e) cname
  | E_fts_snippet { table; col_idx; start_tag; end_tag; ellipsis; n_tokens } ->
    Printf.sprintf "snippet(%s,%d,'%s','%s','%s',%d)"
      table col_idx start_tag end_tag ellipsis n_tokens
  | E_agg _ | E_match _ | E_subquery _ | E_exists _ | E_in_select _ | E_window _ ->
    failwith "expr_to_sql: unsupported expression form"
