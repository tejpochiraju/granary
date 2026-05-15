(** Physical query plan operators for Phase 0.
    Phase 0 operators: CreateTable, Insert, SeqScan, Filter, Project.
    Extended in later phases with IndexLookup, HashJoin, Sort, etc. *)

module Cat = Sqlocaml_catalog.Catalog

type binop = Eq | Ne | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | And | Or

type expr =
  | P_lit         of Ast.literal
  | P_col         of int                   (** column ordinal *)
  | P_binop       of binop * expr * expr
  | P_not         of expr
  | P_is_null     of expr
  | P_is_not_null of expr
  | P_neg         of expr

type op =
  | Op_create_table of {
      name    : string;
      columns : Sqlocaml_encoding.Row.column list;
    }
  | Op_insert of {
      table_meta : Cat.table_meta;
      ordinals   : int list;
      values     : Ast.literal list;
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
  | Op_sort of {
      col_idx : int;
      dir     : [`Asc | `Desc];
      child   : op;
    }
  | Op_limit of {
      limit  : int;
      offset : int;
      child  : op;
    }
  | Op_create_index of {
      name     : string;
      table    : string;
      tree_id  : int;    (** table's tree_id *)
      col_idx  : int;    (** column ordinal in table schema *)
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
