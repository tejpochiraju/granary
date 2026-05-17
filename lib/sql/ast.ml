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

type set_op = Union | Union_all | Intersect | Except

type column_def = {
  name        : string;
  ty          : ty;
  not_null    : bool;
  primary_key : bool;
  default     : literal option;  (* None = no DEFAULT *)
}

type alter_action =
  | AA_add_column    of column_def
  | AA_rename_table  of string              (* new table name *)
  | AA_rename_column of string * string     (* old_col_name * new_col_name *)

type order_dir = Asc | Desc

type order_key = {
  expr : expr;
  dir  : order_dir;
}

type join_kind = Inner | Left

(** A single JOIN clause attached to a SELECT.
    Phase 2 supports a single right-hand table (no nested joins beyond
    a flat list) and an ON predicate. *)
type join_clause = {
  kind  : join_kind;
  table : string;                (** right-side table name *)
  alias : string option;         (** optional alias — stored but unused in Phase 2 *)
  on    : expr;                  (** join condition (predicate over both tables) *)
}

type stmt =
  | S_create_table of {
      name    : string;
      columns : column_def list;
    }
  | S_insert of {
      table       : string;
      columns     : string list;   (** named columns; empty = "all in order" *)
      values      : expr list;
      on_conflict : conflict_action option;
      returning   : expr list;   (** empty = no RETURNING *)
    }
  | S_select of {
      distinct : bool;
      proj     : [ `All | `Cols of string list | `Exprs of expr list ];
        (** [`Exprs] supports arbitrary projection expressions (used for
            aggregates).  Plain column projection still parses to
            [`Cols]. *)
      table    : string;
      joins    : join_clause list;  (** empty list = no joins *)
      where    : expr option;
      group_by : string list;       (** column names; empty = no GROUP BY *)
      having   : expr option;       (** HAVING predicate (may reference aggregates) *)
      order    : order_key list;    (** empty = no ORDER BY *)
      limit    : int option;
      offset   : int option;
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

and pragma_kind =
  | Pragma_table_info of string
  | Pragma_index_list of string
