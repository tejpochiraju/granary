(** Phase 0 SQL AST.
    Covers: CREATE TABLE, INSERT INTO ... VALUES, SELECT ... FROM ... WHERE col = lit.
    Extended in later phases for UPDATE, DELETE, JOINs, etc. *)

type ty =
  | Ty_int   (** INTEGER column type *)
  | Ty_text  (** TEXT column type *)

type literal =
  | L_int  of int64
  | L_text of string
  | L_null

type expr =
  | E_lit of literal
  | E_col of string          (** unqualified column reference *)
  | E_eq  of expr * expr     (** equality — only binary op in Phase 0 *)

type column_def = {
  name        : string;
  ty          : ty;
  not_null    : bool;
  primary_key : bool;
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
      proj  : [ `All | `Cols of string list ];
      table : string;
      where : expr option;
    }
