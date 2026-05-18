(** Name resolution and light type checking for Phase 0 SQL. *)

type binop = Eq | Ne | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | And | Or
           | Concat | Mod | Bit_and | Bit_or | Lshift | Rshift
           | Like | Glob

type bound_expr =
  | BE_lit         of Ast.literal
  | BE_col         of int                  (** column ordinal in the table *)
  | BE_binop       of binop * bound_expr * bound_expr
  | BE_not         of bound_expr
  | BE_is_null     of bound_expr
  | BE_is_not_null of bound_expr
  | BE_neg         of bound_expr
  | BE_bitnot      of bound_expr
  | BE_between     of bound_expr * bound_expr * bound_expr
    (** BETWEEN predicate. *)
  | BE_in          of bound_expr * bound_expr list
    (** IN value list predicate. *)
  | BE_func        of Ast.scalar_func * bound_expr list
    (** Scalar function call (Phase 5). *)
  | BE_param       of int
    (** 0-indexed positional parameter (?). *)
  | BE_match       of Sqlocaml_catalog.Catalog.fts_table_meta * Fts_query.fts_query
    (** FTS MATCH expression: [table MATCH 'query']. *)
  | BE_subquery  of Ast.stmt
    (** Scalar subquery: [(SELECT ...)] in expression position. *)
  | BE_exists    of Ast.stmt
    (** EXISTS predicate: [EXISTS (SELECT ...)]. *)
  | BE_in_select of bound_expr * Ast.stmt
    (** IN subquery: [x IN (SELECT ...)]. *)
  | BE_case of {
      scrutinee : bound_expr option;
      branches  : (bound_expr * bound_expr) list;
      else_     : bound_expr option;
    }
    (** CASE [scrutinee] WHEN ... THEN ... [ELSE ...] END *)
  | BE_cast of bound_expr * Ast.ty
  | BE_excluded_col of int
    (** Reference to the i-th column of the proposed INSERT row (the 'excluded' pseudo-table). *)
  | BE_window_slot of int
    (** Reference to the i-th window function result appended after input columns by Op_window. *)
  | BE_collate of bound_expr * Ast.collation
    (** expr COLLATE collation_name *)

type bound_order_key = {
  key : bound_expr;
  dir : Ast.order_dir;
}

type window_sema = {
  func         : Ast.window_func;
  args         : bound_expr list;
  partition_by : bound_expr list;
  order_by     : bound_order_key list;
}

(** Specification of a single aggregate computation.
    [col_ord] is [None] for COUNT-star and [Some i] for [COUNT(col)],
    [SUM(col)], [AVG(col)], [MIN(col)], [MAX(col)] where [i] is the
    column ordinal in the (combined) input row. *)
type agg_spec = {
  func    : Ast.agg_func;
  col_ord : int option;
}

(** Projection item in an aggregated SELECT.  The output row of
    [Op_aggregate] has shape [[group_col_value; agg1; agg2; ...]] when
    [GROUP BY] is present, and [[agg1; agg2; ...]] otherwise.
    Projection items below describe how to compute each projected
    column FROM that aggregate output row. *)
type agg_proj_item =
  | AP_group_col              (** project the GROUP BY column (only valid if group_by present) *)
  | AP_agg_slot of int        (** project the [i]-th aggregate result from the aggregate output *)

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
      name      : string;
      columns   : Sqlocaml_encoding.Row.column list;
      uniq_idxs : (string * string list) list;
        (** Auto-generated UNIQUE index specs: (index_name, [col_name; ...]).
            Planner creates Op_create_index for each. *)
    }
  | BS_insert of {
      table_meta    : Sqlocaml_catalog.Catalog.table_meta;
      ordinals      : int list;            (** column ordinals for the named cols *)
      values        : bound_expr list list;   (* one sublist per VALUES row *)
      on_conflict   : Ast.conflict_action option;
      returning     : bound_expr list;
      upsert_update : (string list * (int * bound_expr) list) option;
    }
  | BS_select of {
      distinct   : bool;
      table_meta : Sqlocaml_catalog.Catalog.table_meta;
      proj       : int list;            (** column ordinals to project
                                            (refer to the combined row when [join] is set)
                                            — used when this is NOT an aggregated query
                                            and [expr_proj] is empty *)
      expr_proj  : (bound_expr * string option) list;
        (** Phase 5: non-empty when projection contains scalar functions
            or other arbitrary expressions.  When non-empty, [proj] is
            empty and [expr_proj] governs the output columns. *)
      where      : bound_expr option;
      order      : bound_order_key list;
      limit      : int option;
      offset     : int option;
      joins      : bound_join list;     (** Phase 10: zero or more JOINs *)
      group_by   : int option;
        (** [Some i] = GROUP BY column at ordinal [i] (in combined row).
            [None] with non-empty [aggs] = one big group over all rows.
            [None] with empty [aggs] = no aggregation (ordinary SELECT). *)
      aggs       : agg_spec list;       (** ordered list of aggregates to compute *)
      having     : bound_expr option;
        (** HAVING predicate.  Column references inside resolve against
            the OUTPUT row of [Op_aggregate]:
              - ordinal 0 = group column value (if [group_by] is [Some]);
              - ordinals 1..N (or 0..N-1 if no group_by) = aggregate slots. *)
      agg_proj   : agg_proj_item list;
        (** When [aggs] is non-empty, this is the projection list over
            the aggregate output row (ignore [proj]).  Empty otherwise. *)
      windows    : window_sema list;
    }
  | BS_create_index of {
      name       : string;
      table_meta : Sqlocaml_catalog.Catalog.table_meta;
      col_idxs   : int list;            (** column ordinals in the table *)
      unique     : bool;
    }
  | BS_update of {
      table_meta  : Sqlocaml_catalog.Catalog.table_meta;
      assignments : (int * bound_expr) list;
        (** [(col_ordinal, new_value_expr)] *)
      where       : bound_expr option;
      returning   : bound_expr list;
    }
  | BS_delete of {
      table_meta : Sqlocaml_catalog.Catalog.table_meta;
      where      : bound_expr option;
      returning  : bound_expr list;
    }
  | BS_drop_table of {
      name       : string;
      table_meta : Sqlocaml_catalog.Catalog.table_meta;
    }
  | BS_drop_index of {
      name     : string;
      idx_info : Sqlocaml_catalog.Catalog.index_info;
    }
  | BS_begin
  | BS_commit
  | BS_rollback
  | BS_create_fts_table of {
      name    : string;
      columns : string list;
    }
  | BS_fts_insert of {
      fts_meta   : Sqlocaml_catalog.Catalog.fts_table_meta;
      col_names  : string list;
      col_values : bound_expr list;
    }
  | BS_fts_delete of {
      fts_meta : Sqlocaml_catalog.Catalog.fts_table_meta;
      where    : bound_expr option;
    }
  | BS_fts_seq_scan of {
      fts_meta : Sqlocaml_catalog.Catalog.fts_table_meta;
      where    : bound_expr option;
    }
  | BS_fts_match_scan of {
      fts_meta     : Sqlocaml_catalog.Catalog.fts_table_meta;
      query        : Fts_query.fts_query;
      proj         : int list;
      include_rank : bool;
    }
  | BS_pragma of {
      kind : Ast.pragma_kind;
    }
  | BS_alter_table of {
      table_meta : Sqlocaml_catalog.Catalog.table_meta;
      action     : Ast.alter_action;
    }
  | BS_compound of {
      op    : Ast.set_op;
      left  : bound_stmt;
      right : bound_stmt;
    }
  | BS_const_select of {
      exprs : (bound_expr * string option) list;
    }
  | BS_with_cte of {
      name      : string;
      def       : bound_stmt;
      query     : bound_stmt;
      recursive : bool;
    }
  | BS_create_view of { name: string; query: Ast.stmt }
  | BS_drop_view   of { name: string }

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
  | Unknown_index       of string   (* index name *)

val pp_error : Format.formatter -> error -> unit

val bind :
  ?views:(string, Ast.stmt) Hashtbl.t ->
  Sqlocaml_catalog.Catalog.t ->
  Ast.stmt ->
  (bound_stmt, error) result Lwt.t

val bind_returning_params :
  ?views:(string, Ast.stmt) Hashtbl.t ->
  Sqlocaml_catalog.Catalog.t ->
  Ast.stmt ->
  ((bound_stmt * (string * int) list), error) result Lwt.t
