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

(** A bound JOIN clause.
    Column ordinals in [on] are absolute within the combined
    [left ++ right] row: left table columns occupy [0 .. n_left-1] and
    right table columns occupy [right_col_offset .. right_col_offset + n_right - 1]. *)
type bound_join = {
  kind             : Ast.join_kind;
  right_meta       : Sqlocaml_catalog.Catalog.table_meta;
  on               : bound_expr;
  right_col_offset : int;        (** ordinal of the first right-table column in the combined row *)
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
      proj       : int list;            (** column ordinals to project
                                            (refer to the combined row when [join] is set) *)
      where      : bound_expr option;
      order      : bound_order_key list;
      limit      : int option;
      offset     : int option;
      join       : bound_join option;   (** Phase 2: single optional JOIN *)
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
  | Unknown_table       of string
  | Unknown_column      of { table : string; column : string }
  | Ambiguous_column    of string   (* column name appears in both joined tables *)
  | Type_mismatch       of { expected : Sqlocaml_encoding.Row.ty;
                             got      : Sqlocaml_encoding.Row.ty }
  | Arity_mismatch      of { expected : int; got : int }
  | Already_exists      of string
  | Invalid_limit       of string
  | Unsupported         of string
  | Not_null_violation  of string   (* column name *)

val bind :
  Sqlocaml_catalog.Catalog.t ->
  Ast.stmt ->
  (bound_stmt, error) result Lwt.t
