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

type binop =
  | Eq | Ne | Lt | Le | Gt | Ge   (** comparison *)
  | Add | Sub | Mul | Div          (** arithmetic *)
  | And | Or                       (** logical *)
  | Concat                         (** string concatenation || *)
  | Mod                            (** modulo % *)
  | Bit_and | Bit_or               (** bitwise & | *)
  | Lshift | Rshift                (** shift << >> *)

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
  | E_agg         of agg_func * expr option
    (** Aggregate call; [None] argument means [COUNT( * )]. *)
  | E_func        of scalar_func * expr list
    (** Scalar function call. *)
  | E_param       of int
    (** 0-indexed positional parameter: ? *)
  | E_match       of string * string
    (** [E_match (table_name, query_string)]: [WHERE table MATCH 'query'] *)

type column_def = {
  name        : string;
  ty          : ty;
  not_null    : bool;
  primary_key : bool;
  default     : literal option;  (* None = no DEFAULT *)
}

type order_dir = Asc | Desc

type order_key = {
  col       : string;
  table_opt : string option;  (* Some "t" for ORDER BY t.col *)
  dir       : order_dir;
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
      table   : string;
      columns : string list;   (** named columns; empty = "all in order" *)
      values  : expr list;
    }
  | S_select of {
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
      name   : string;
      table  : string;
      column : string;
      unique : bool;
    }
  | S_update of {
      table       : string;
      assignments : (string * expr) list;   (** [(col_name, new_value_expr)] *)
      where       : expr option;
    }
  | S_delete of {
      table : string;
      where : expr option;
    }
  | S_drop_table of {
      name : string;
    }
  | S_drop_index of {
      name : string;
    }
  | S_begin
  | S_commit
  | S_rollback
  | S_create_fts_table of {
      name    : string;
      columns : string list;
    }
