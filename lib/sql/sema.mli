(** Name resolution and light type checking for Phase 0 SQL. *)

type binop = Eq | Ne | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | And | Or

type bound_expr =
  | BE_lit         of Ast.literal
  | BE_col         of int                  (** column ordinal in the table *)
  | BE_binop       of binop * bound_expr * bound_expr
  | BE_not         of bound_expr
  | BE_is_null     of bound_expr
  | BE_is_not_null of bound_expr
  | BE_neg         of bound_expr

type bound_order_key = {
  col_idx : int;
  dir     : Ast.order_dir;
}

type bound_stmt =
  | BS_create_table of {
      name    : string;
      columns : Sqlocaml_encoding.Row.column list;
    }
  | BS_insert of {
      table_meta : Sqlocaml_catalog.Catalog.table_meta;
      ordinals   : int list;            (** column ordinals for the named cols *)
      values     : Ast.literal list;
    }
  | BS_select of {
      table_meta : Sqlocaml_catalog.Catalog.table_meta;
      proj       : int list;            (** column ordinals to project *)
      where      : bound_expr option;
      order      : bound_order_key list;
      limit      : int option;
      offset     : int option;
    }
  | BS_create_index of {
      name       : string;
      table_meta : Sqlocaml_catalog.Catalog.table_meta;
      col_idx    : int;                 (** column ordinal in the table *)
      unique     : bool;
    }
  | BS_update of {
      table_meta  : Sqlocaml_catalog.Catalog.table_meta;
      assignments : (int * bound_expr) list;
        (** [(col_ordinal, new_value_expr)] *)
      where       : bound_expr option;
    }
  | BS_delete of {
      table_meta : Sqlocaml_catalog.Catalog.table_meta;
      where      : bound_expr option;
    }

type error =
  | Unknown_table  of string
  | Unknown_column of { table : string; column : string }
  | Type_mismatch  of { expected : Sqlocaml_encoding.Row.ty;
                        got      : Sqlocaml_encoding.Row.ty }
  | Arity_mismatch of { expected : int; got : int }
  | Already_exists of string
  | Invalid_limit  of string
  | Unsupported    of string

val bind :
  Sqlocaml_catalog.Catalog.t ->
  Ast.stmt ->
  (bound_stmt, error) result Lwt.t
