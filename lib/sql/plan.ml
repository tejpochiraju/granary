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
  | P_subquery  of Ast.stmt
  | P_exists    of Ast.stmt
  | P_in_select of expr * Ast.stmt
  | P_case of {
      scrutinee : expr option;
      branches  : (expr * expr) list;
      else_     : expr option;
    }
  | P_cast of expr * Ast.ty
  | P_excluded_col of int
    (** Column reference into the proposed INSERT excluded row. *)
  | P_window_slot of int
    (** Window function slot reference — substituted to P_col(n_input_cols+i) by planner. *)
  | P_collate of expr * Ast.collation

type window_plan_item = {
  func         : Ast.window_func;
  args         : expr list;
  partition_by : expr list;
  order_by     : (expr * [`Asc | `Desc] * [`Nulls_first | `Nulls_last]) list;
  frame        : Ast.frame_spec option;
}

type snippet_spec = {
  col_idx   : int;
  start_tag : string;
  end_tag   : string;
  ellipsis  : string;
  n_tokens  : int;
}

type op =
  | Op_create_table of {
      name           : string;
      columns        : Sqlocaml_encoding.Row.column list;
      uniq_idxs      : (string * string list) list;
      if_not_exists  : bool;
      fk_constraints : (string list * string * string list * Sqlocaml_catalog.Catalog.fk_action * Sqlocaml_catalog.Catalog.fk_action * bool) list;
        (** [(local_cols, parent_table, parent_cols, on_delete, on_update, deferrable)] *)
      without_rowid  : bool;
    }
  | Op_insert of {
      table_meta    : Cat.table_meta;
      ordinals      : int list;
      values        : expr list list;   (* one sublist per VALUES row *)
      on_conflict   : Ast.conflict_action option;
      returning     : expr list;
      upsert_update : (string list * (int * expr) list) option;
    }
  | Op_insert_select of {
      table_meta  : Cat.table_meta;
      ordinals    : int list;
      source      : op;
      on_conflict : Ast.conflict_action option;
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
      exprs : (expr * string option) list;
      child : op;
    }
  | Op_sort of {
      keys  : (expr * [`Asc | `Desc] * [`Nulls_first | `Nulls_last]) list;
      child : op;
    }
  | Op_limit of {
      limit  : int;
      offset : int;
      child  : op;
    }
  | Op_create_index of {
      name           : string;
      table          : string;
      tree_id        : int;          (** table's tree_id *)
      col_sqls       : string list;  (** col name (plain) or expr SQL (expression) *)
      col_expr_flags : bool list;    (** true = expression index column, false = plain column *)
      where_expr     : expr option;
      where_sql      : string option;
      unique         : bool;
      columns        : Sqlocaml_encoding.Row.column list;
        (** columns of the target table — needed for row decoding
            during index population *)
      if_not_exists  : bool;
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
      order       : (expr * [`Asc | `Desc] * [`Nulls_first | `Nulls_last]) list;
      limit       : int option;
      offset      : int option;
      indexes     : Cat.index_info list;
      returning   : expr list;
    }
  | Op_delete of {
      table_meta : Cat.table_meta;
      where      : expr option;
      order      : (expr * [`Asc | `Desc] * [`Nulls_first | `Nulls_last]) list;
      limit      : int option;
      offset     : int option;
      indexes    : Cat.index_info list;
      returning  : expr list;
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
      child      : op;
      group_cols : int list;
        (** column ordinals in the CHILD row.  [[]] = one big group. *)
      aggs       : agg_spec list;
      having     : expr option;
        (** evaluated on the OUTPUT row of [Op_aggregate];
            output row = [group_col0; group_col1; ...; agg1; agg2; ...]. *)
      proj       : proj_item list;
        (** projection over the aggregate output row.  Maps to the final
            row emitted to downstream operators. *)
      windows    : window_plan_item list;
        (** Post-aggregate window functions; [] for plain GROUP BY. *)
    }
  | Op_alter_table of {
      table_meta : Cat.table_meta;
      action     : Ast.alter_action;
    }
  | Op_begin
  | Op_commit
  | Op_rollback
  | Op_savepoint   of string
  | Op_release     of string
  | Op_rollback_to of string
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
      query        : Fts_query.t;
      proj         : int list;
      include_rank : bool;  (** if true, append BM25 score as last projected column *)
      snippets     : snippet_spec list;
    }
  | Op_pragma_rows of {
      rows : Sqlocaml_encoding.Row.t list;
    }
  | Op_pragma_get_user_version
    (** Read user_version from sys_meta at exec time; returns one row [[V_int n]]. *)
  | Op_pragma_set_user_version of { version : int64 }
    (** Write user_version to sys_meta; DDL-like, returns 0 rows. *)
  | Op_pragma_integrity_check
    (** Scan all table + index B-trees; return [["ok"]] or list of error strings. *)
  | Op_pragma_get_fk
    (** Read fk_enforcement flag from catalog; returns one row [[V_int 0|1]]. *)
  | Op_pragma_set_fk of { on : bool }
    (** Write fk_enforcement flag to catalog; DDL-like, returns 0 rows. *)
  | Op_pragma_get_recursive_triggers
    (** Read recursive_triggers flag from catalog; returns one row [[V_int 0|1]]. *)
  | Op_pragma_set_recursive_triggers of { on : bool }
    (** Write recursive_triggers flag to catalog; DDL-like, returns 0 rows. *)
  | Op_pragma_get_defer_fk
    (** Read defer_foreign_keys flag from catalog; returns one row [[V_int 0|1]]. *)
  | Op_pragma_set_defer_fk of { on : bool }
    (** Write defer_foreign_keys flag to catalog; DDL-like, returns 0 rows. *)
  | Op_pragma_wal_checkpoint
    (** Migrate WAL contents to main DB and reset; no-op outside WAL mode. *)
  | Op_pragma_get_wal_autocheckpoint
    (** Read per-connection auto-checkpoint threshold. *)
  | Op_pragma_set_wal_autocheckpoint of { n : int64 }
    (** Set per-connection auto-checkpoint threshold (0 disables). *)
  | Op_vacuum
    (** Compact-rebuild the database file (phase 37 / #120). *)
  | Op_attach of { path : string; schema : string }
    (** [ATTACH DATABASE 'path' AS schema] — phase 40 / #64.  Mutates
        the executing [Db.t]'s [attached] map; never observed by exec.ml. *)
  | Op_detach of { schema : string }
    (** [DETACH DATABASE schema] — phase 40 / #64. *)
  | Op_database_list
    (** [PRAGMA database_list] — phase 40 / #64.  Yields one row per
        schema: (seq INT, name TEXT, file TEXT). *)
  | Op_active_database_get
    (** [PRAGMA active_database] — phase 40 / #64. *)
  | Op_active_database_set of { schema : string }
    (** [PRAGMA active_database = schema] — phase 40 / #64.  Subsequent
        non-routing statements route through [t.attached] under [schema]. *)
  | Op_distinct of {
      child : op;
    }
  | Op_union of {
      all   : bool;
      left  : op;
      right : op;
    }
  | Op_intersect of {
      left  : op;
      right : op;
    }
  | Op_except of {
      left  : op;
      right : op;
    }
  | Op_const_select of {
      exprs : (expr * string option) list;
    }
  | Op_window of {
      child        : op;
      windows      : window_plan_item list;
      n_input_cols : int;
    }
  | Op_with_cte of {
      cte_name  : string;
      def       : op;
      query     : op;
      recursive : bool;
    }
  | Op_cte_scan of {
      cte_name : string;
      n_cols   : int;
    }
  | Op_create_view of {
      name  : string;
      query : Ast.stmt;
    }
  | Op_drop_view of {
      name : string;
    }
  | Op_create_trigger of {
      name    : string;
      timing  : Ast.trigger_timing;
      event   : Ast.trigger_event;
      table   : string;
      when_   : Ast.expr option;
      body    : Ast.stmt list;
    }
  | Op_drop_trigger of { name : string }
  | Op_sqlite_master
    (** Virtual scan that reconstructs sqlite_master rows from catalog metadata. *)
  | Op_no_op
    (** No-op plan node produced by IF EXISTS DROP when object not found. *)
  | Op_changes
    (** Returns rows affected by last DML. Intercepted in db.ml query — not exec.ml. *)
  | Op_last_insert_rowid
    (** Returns rowid of last INSERT. Intercepted in db.ml query — not exec.ml. *)
  | Op_total_changes
    (** Returns total rows affected since the connection was opened.
        Intercepted in db.ml query — not exec.ml. *)
  | Op_explain of {
      analyze : bool;
      inner   : op;
    }

and proj_item =
  | PI_group_col of int     (** project the i-th GROUP BY column (index into group_cols) *)
  | PI_agg_slot of int      (** project the k-th aggregate result *)
  | PI_window_slot of int   (** project the j-th post-aggregate window function result *)

and agg_spec = {
  func    : Ast.agg_func;
  col_ord : int option;     (** [None] means COUNT-star *)
}

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
