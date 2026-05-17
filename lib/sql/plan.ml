(** Physical query plan operators for Phase 0.
    Phase 0 operators: CreateTable, Insert, SeqScan, Filter, Project.
    Extended in later phases with IndexLookup, HashJoin, Sort, etc. *)

module Cat = Sqlocaml_catalog.Catalog

type binop = Eq | Ne | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | And | Or
           | Concat | Mod | Bit_and | Bit_or | Lshift | Rshift
           | Like | Glob

type expr =
  | P_lit         of Ast.literal
  | P_col         of int                   (** column ordinal *)
  | P_binop       of binop * expr * expr
  | P_not         of expr
  | P_is_null     of expr
  | P_is_not_null of expr
  | P_neg         of expr
  | P_bitnot      of expr
  | P_between     of expr * expr * expr
  | P_in          of expr * expr list
  | P_func        of Ast.scalar_func * expr list
  | P_param       of int                   (** 0-indexed positional parameter *)

type op =
  | Op_create_table of {
      name    : string;
      columns : Sqlocaml_encoding.Row.column list;
    }
  | Op_insert of {
      table_meta : Cat.table_meta;
      ordinals   : int list;
      values     : expr list;
    }
  | Op_seq_scan of {
      table_meta : Cat.table_meta;
    }
  | Op_filter of {
      pred  : expr;
      child : op;
    }
  | Op_project of {
      ordinals : int list;
      child    : op;
    }
  | Op_expr_project of {
      exprs : expr list;
      child : op;
    }
  | Op_sort of {
      key   : expr;
      dir   : [`Asc | `Desc];
      child : op;
    }
  | Op_limit of {
      limit  : int;
      offset : int;
      child  : op;
    }
  | Op_create_index of {
      name     : string;
      table    : string;
      tree_id  : int;          (** table's tree_id *)
      col_idxs : int list;     (** column ordinals in table schema *)
      unique   : bool;
      columns  : Sqlocaml_encoding.Row.column list;
        (** columns of the target table — needed for row decoding
            during index population *)
    }
  | Op_index_lookup of {
      table_tree : int;                 (** table's tree_id *)
      idx_tree   : int;                 (** index tree_id *)
      col_idx    : int;                 (** column ordinal for encoding *)
      col_type   : Sqlocaml_encoding.Row.ty;
      lookup_val : expr;                (** value to look up *)
      table_meta : Cat.table_meta;      (** for row decoding *)
    }
  | Op_update of {
      table_meta  : Cat.table_meta;
      assignments : (int * expr) list;  (** [(col_ordinal, new_value_expr)] *)
      where       : expr option;
      indexes     : Cat.index_info list;
    }
  | Op_delete of {
      table_meta : Cat.table_meta;
      where      : expr option;
      indexes    : Cat.index_info list;
    }
  | Op_drop_table of {
      table_meta : Cat.table_meta;
      indexes    : Cat.index_info list;
    }
  | Op_drop_index of {
      idx_info : Cat.index_info;
    }
  | Op_nested_loop_join of {
      left             : op;                  (** left input (any op stream) *)
      right_meta       : Cat.table_meta;      (** right table for row decode *)
      idx_tree         : int;                 (** right-side index tree id *)
      right_col_idx    : int;                 (** join col ordinal IN RIGHT TABLE *)
      left_col_idx     : int;                 (** join col ordinal in the LEFT row *)
      join_kind        : [ `Inner | `Left ];
      right_col_offset : int;                 (** = n_left_cols *)
      n_right_cols     : int;
    }
  | Op_hash_join of {
      left             : op;
      right            : op;
      left_key         : int;                 (** col ordinal in the left row *)
      right_key        : int;                 (** col ordinal in the right row *)
      join_kind        : [ `Inner | `Left ];
      right_col_offset : int;
      n_right_cols     : int;
    }
  | Op_aggregate of {
      child     : op;
      group_col : int option;
        (** column ordinal in the CHILD row.  [None] = one big group. *)
      aggs      : agg_spec list;
      having    : expr option;
        (** evaluated on the OUTPUT row of [Op_aggregate];
            output row = [group_col_value?; agg1; agg2; ...]. *)
      proj      : proj_item list;
        (** projection over the aggregate output row.  Maps to the final
            row emitted to downstream operators. *)
    }
  | Op_begin
  | Op_commit
  | Op_rollback
  | Op_create_fts_table of {
      name    : string;
      columns : string list;
    }
  | Op_fts_insert of {
      fts_meta   : Cat.fts_table_meta;
      col_names  : string list;
      col_values : expr list;
    }
  | Op_fts_delete of {
      fts_meta : Cat.fts_table_meta;
      where    : expr option;
    }
  | Op_fts_seq_scan of {
      fts_meta : Cat.fts_table_meta;
      where    : expr option;
    }
  | Op_fts_match_scan of {
      fts_meta     : Cat.fts_table_meta;
      query        : Fts_query.fts_query;
      proj         : int list;
      include_rank : bool;  (** if true, append BM25 score as last projected column *)
    }
  | Op_pragma_rows of {
      rows : Sqlocaml_encoding.Row.t list;
    }
  | Op_distinct of {
      child : op;
    }

and proj_item =
  | PI_group_col            (** project the group column (must have [group_col = Some _]) *)
  | PI_agg_slot of int      (** project the k-th aggregate result *)

and agg_spec = {
  func    : Ast.agg_func;
  col_ord : int option;     (** [None] means COUNT-star *)
}
