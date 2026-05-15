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

type expr =
  | E_lit         of literal
  | E_col         of string                (** unqualified column reference *)
  | E_tbl_col     of string * string       (** qualified: table.col *)
  | E_binop       of binop * expr * expr
  | E_not         of expr
  | E_is_null     of expr
  | E_is_not_null of expr
  | E_neg         of expr                  (** unary minus *)

type column_def = {
  name        : string;
  ty          : ty;
  not_null    : bool;
  primary_key : bool;
  default     : literal option;  (* None = no DEFAULT *)
}

type order_dir = Asc | Desc

type order_key = {
  col : string;
  dir : order_dir;
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
      values  : literal list;
    }
  | S_select of {
      proj   : [ `All | `Cols of string list ];
      table  : string;
      joins  : join_clause list;  (** empty list = no joins *)
      where  : expr option;
      order  : order_key list;   (** empty = no ORDER BY *)
      limit  : int option;
      offset : int option;
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
