(** Physical query plan operators for Phase 0.
    Phase 0 operators: CreateTable, Insert, SeqScan, Filter, Project.
    Extended in later phases with IndexLookup, HashJoin, Sort, etc. *)

module Cat = Granary_catalog.Catalog

type binop =
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Concat
  | Mod
  | Bit_and
  | Bit_or
  | Lshift
  | Rshift
  | Like
  | Glob

type expr =
  | P_lit of Ast.literal
  | P_col of int (** column ordinal *)
  | P_binop of binop * expr * expr
  | P_not of expr
  | P_is_null of expr
  | P_is_not_null of expr
  | P_neg of expr
  | P_bitnot of expr
  | P_between of expr * expr * expr
  | P_in of expr * expr list
  | P_func of Ast.scalar_func * expr list
  | P_param of int (** 0-indexed positional parameter *)
  | P_subquery of Ast.stmt
  | P_exists of Ast.stmt
  | P_in_select of expr * Ast.stmt
  | P_case of
      { scrutinee : expr option
      ; branches : (expr * expr) list
      ; else_ : expr option
      }
  | P_cast of expr * Ast.ty
  | P_excluded_col of int (** Column reference into the proposed INSERT excluded row. *)
  | P_window_slot of int
  (** Window function slot reference — substituted to P_col(n_input_cols+i) by planner. *)
  | P_collate of expr * Ast.collation

type window_plan_item =
  { func : Ast.window_func
  ; args : expr list
  ; partition_by : expr list
  ; order_by : (expr * [ `Asc | `Desc ] * [ `Nulls_first | `Nulls_last ]) list
  ; frame : Ast.frame_spec option
  }

type snippet_spec =
  { col_idx : int
  ; start_tag : string
  ; end_tag : string
  ; ellipsis : string
  ; n_tokens : int
  }

(* #517: a range over the index column right after a seek's equality prefix.
   Both ends are inclusive by construction; see plan.mli for why that is safe
   and why only fixed-width column types are bounded. *)
type range =
  { r_ty : Granary_encoding.Row.ty
  ; r_lo : expr option
  ; r_hi : expr option
  }

type seek =
  | Seek_rowid of expr
  | Seek_index of
      { idx_tree : int
      ; keys : (int * Granary_encoding.Row.ty * expr) list
      ; range : range option
      }

(* #516: one component of a nested-loop join's index probe key.  See plan.mli
   for why the constant parts are a pure narrowing. *)
type probe_part =
  | Probe_from_left of int
  | Probe_const of expr

type op =
  | Op_create_table of
      { name : string
      ; columns : Granary_encoding.Row.column list
      ; uniq_idxs : (string * string list * Cat.idx_origin) list
      ; if_not_exists : bool
      ; fk_constraints :
          (string list
          * string
          * string list
          * Granary_catalog.Catalog.fk_action
          * Granary_catalog.Catalog.fk_action
          * bool)
            list
        (** [(local_cols, parent_table, parent_cols, on_delete, on_update, deferrable)] *)
      ; without_rowid : bool
      ; autoincrement : bool (** #299: INTEGER PRIMARY KEY AUTOINCREMENT. *)
      }
  | Op_col_create_table of
      { name : string
      ; columns : Granary_encoding.Row.column list
      ; if_not_exists : bool
      }
  | Op_insert of
      { table_meta : Cat.table_meta
      ; ordinals : int list
      ; values : expr list list (* one sublist per VALUES row *)
      ; on_conflict : Ast.conflict_action option
      ; returning : expr list
      ; upsert_update : (string list * (int * expr) list) option
      }
  | Op_insert_select of
      { table_meta : Cat.table_meta
      ; ordinals : int list
      ; source : op
      ; on_conflict : Ast.conflict_action option
      }
  | Op_seq_scan of
      { table_meta : Cat.table_meta
      ; alias : string option
        (** #635: the FROM item's alias, when it has one.  An alias REPLACES the
            table name as this input's scope identifier, so a correlated
            subquery's outer reference resolves against [alias] and not against
            [table_meta.name]. *)
      }
  | Op_filter of
      { pred : expr
      ; child : op
      }
  | Op_project of
      { ordinals : int list
      ; child : op
      }
  | Op_expr_project of
      { exprs : (expr * string option) list
      ; child : op
      }
  | Op_sort of
      { keys : (expr * [ `Asc | `Desc ] * [ `Nulls_first | `Nulls_last ]) list
      ; child : op
      }
  | Op_limit of
      { limit : int
      ; offset : int
      ; child : op
      }
  | Op_create_index of
      { name : string
      ; table : string
      ; tree_id : int (** table's tree_id *)
      ; col_sqls : string list (** col name (plain) or expr SQL (expression) *)
      ; col_expr_flags : bool list
        (** true = expression index column, false = plain column *)
      ; where_expr : expr option
      ; where_sql : string option
      ; unique : bool
      ; columns : Granary_encoding.Row.column list
        (** columns of the target table — needed for row decoding
            during index population *)
      ; if_not_exists : bool
      }
  | Op_index_lookup of
      { table_tree : int (** table's tree_id *)
      ; idx_tree : int (** index tree_id *)
      ; keys : (int * Granary_encoding.Row.ty * expr) list
        (** leading index columns pinned by equality, in INDEX column order:
            [(column ordinal, column type, value expression)] *)
      ; range : range option (** #517: range over the column after [keys] *)
      ; table_meta : Cat.table_meta (** for row decoding *)
      ; alias : string option (** #635: see {!Op_seq_scan}'s [alias] *)
      }
  | Op_rowid_lookup of
      { table_meta : Cat.table_meta
      ; lookup_val : expr
      ; alias : string option (** #635: see {!Op_seq_scan}'s [alias] *)
      }
  | Op_col_seq_scan of
      { table_meta : Cat.table_meta
      ; alias : string option (** #635: see {!Op_seq_scan}'s [alias] *)
      }
  | Op_update of
      { table_meta : Cat.table_meta
      ; assignments : (int * expr) list (** [(col_ordinal, new_value_expr)] *)
      ; where : expr option
      ; seek : seek option (** #508: optional index/rowid narrowing for [where] *)
      ; order : (expr * [ `Asc | `Desc ] * [ `Nulls_first | `Nulls_last ]) list
      ; limit : int option
      ; offset : int option
      ; indexes : Cat.index_info list
      ; returning : expr list
      }
  | Op_delete of
      { table_meta : Cat.table_meta
      ; where : expr option
      ; seek : seek option (** #508: optional index/rowid narrowing for [where] *)
      ; order : (expr * [ `Asc | `Desc ] * [ `Nulls_first | `Nulls_last ]) list
      ; limit : int option
      ; offset : int option
      ; indexes : Cat.index_info list
      ; returning : expr list
      }
  | Op_drop_table of
      { table_meta : Cat.table_meta
      ; indexes : Cat.index_info list
      }
  | Op_drop_index of { idx_info : Cat.index_info }
  | Op_nested_loop_join of
      { left : op (** left input (any op stream) *)
      ; right_meta : Cat.table_meta (** right table for row decode *)
      ; right_alias : string option (** #635: see {!Op_seq_scan}'s [alias] *)
      ; idx_tree : int (** right-side index tree id *)
      ; probe : probe_part list (** probe key, in index-column order *)
      ; probe_range : range option
        (** #570: a #532 range bound on the index column after the probe key *)
      ; join_kind : [ `Inner | `Left ]
      ; right_col_offset : int (** = n_left_cols *)
      ; n_right_cols : int
      }
  | Op_hash_join of
      { left : op
      ; right : op
      ; left_key : int (** col ordinal in the left row *)
      ; right_key : int (** col ordinal in the right row *)
      ; on_pred : expr option
        (** #552: the ON predicate, evaluated on the joined row {i inside} the
            join as its match test.  Set only on the cartesian fallback
            ([left_key < 0]), where the ON predicate is not a [col = col]
            equality the hash keys can express.

            It has to live here rather than in an [Op_filter] above the join
            because for an outer join the ON predicate {b is} the match test: a
            left row that satisfies it for no right row must still be emitted,
            null-extended, and a filter above the join sees that null-extended
            row and rejects it.  For an [`Inner] join the two placements are
            equivalent and the planner still emits the filter. *)
      ; join_kind : [ `Inner | `Left ]
      ; right_col_offset : int
      ; n_right_cols : int
      }
  | Op_aggregate of
      { child : op
      ; group_cols : int list
        (** column ordinals in the CHILD row.  [[]] = one big group. *)
      ; aggs : agg_spec list
      ; having : expr option
        (** evaluated on the OUTPUT row of [Op_aggregate];
            output row = [group_col0; group_col1; ...; agg1; agg2; ...]. *)
      ; proj : proj_item list
        (** projection over the aggregate output row.  Maps to the final
            row emitted to downstream operators. *)
      ; windows : window_plan_item list
        (** Post-aggregate window functions; [] for plain GROUP BY. *)
      }
  | Op_alter_table of
      { table_meta : Cat.table_meta
      ; action : Ast.alter_action
      }
  | Op_begin
  | Op_commit
  | Op_rollback
  | Op_savepoint of string
  | Op_release of string
  | Op_rollback_to of string
  | Op_create_fts_table of
      { name : string
      ; columns : string list
      }
  | Op_fts_insert of
      { fts_meta : Cat.fts_table_meta
      ; col_names : string list
      ; col_values : expr list
      ; rowid_value : expr option (** #330: explicit [rowid] from the column list *)
      }
  | Op_fts_delete of
      { fts_meta : Cat.fts_table_meta
      ; where : expr option
      }
  | Op_fts_seq_scan of
      { fts_meta : Cat.fts_table_meta
      ; where : expr option
      }
  | Op_fts_match_scan of
      { fts_meta : Cat.fts_table_meta
      ; query : Fts_query.t
      ; proj : int list
      ; include_rank : bool (** if true, append BM25 score as last projected column *)
      ; snippets : snippet_spec list
      }
  | Op_pragma_rows of { rows : Granary_encoding.Row.t list }
  | Op_pragma_get_user_version
  (** Read user_version from sys_meta at exec time; returns one row [[V_int n]]. *)
  | Op_pragma_set_user_version of { version : int64 }
  (** Write user_version to sys_meta; DDL-like, returns 0 rows. *)
  | Op_pragma_integrity_check
  (** Scan all table + index B-trees; return [["ok"]] or list of error strings. *)
  | Op_pragma_not_null_check
  (** #563: scan every row table for stored NULLs in a column the loaded schema
      declares NOT NULL; one row [[table; column; count]] per offending pair,
      none when the database is clean.  Read-only. *)
  | Op_pragma_not_null_repair
  (** #563: DELETE the rows [Op_pragma_not_null_check] reports; one row
      [[table; column; deleted]] per pair actually repaired. *)
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
  | Op_pragma_get_wal_autocheckpoint (** Read per-connection auto-checkpoint threshold. *)
  | Op_pragma_set_wal_autocheckpoint of { n : int64 }
  (** Set per-connection auto-checkpoint threshold (0 disables). *)
  | Op_pragma_get_synchronous (** #298 read durability mode *)
  | Op_pragma_set_synchronous of { mode : string }
  | Op_pragma_get_wal_batch_commits
  | Op_pragma_set_wal_batch_commits of { n : int64 }
  | Op_pragma_get_wal_batch_interval_ms
  | Op_pragma_set_wal_batch_interval_ms of { n : int64 }
  | Op_vacuum (** Compact-rebuild the database file (phase 37 / #120). *)
  | Op_attach of
      { path : string
      ; schema : string
      }
  (** [ATTACH DATABASE 'path' AS schema] — phase 40 / #64.  Mutates
        the executing [Db.t]'s [attached] map; never observed by exec.ml. *)
  | Op_detach of { schema : string } (** [DETACH DATABASE schema] — phase 40 / #64. *)
  | Op_database_list
  (** [PRAGMA database_list] — phase 40 / #64.  Yields one row per
        schema: (seq INT, name TEXT, file TEXT). *)
  | Op_active_database_get (** [PRAGMA active_database] — phase 40 / #64. *)
  | Op_active_database_set of { schema : string }
  (** [PRAGMA active_database = schema] — phase 40 / #64.  Subsequent
        non-routing statements route through [t.attached] under [schema]. *)
  | Op_distinct of { child : op }
  | Op_union of
      { all : bool
      ; left : op
      ; right : op
      }
  | Op_intersect of
      { left : op
      ; right : op
      }
  | Op_except of
      { left : op
      ; right : op
      }
  | Op_const_select of { exprs : (expr * string option) list }
  | Op_window of
      { child : op
      ; windows : window_plan_item list
      ; n_input_cols : int
      }
  | Op_with_cte of
      { cte_name : string
      ; def : op
      ; query : op
      ; recursive : bool
      }
  | Op_cte_scan of
      { cte_name : string
      ; n_cols : int
      }
  | Op_create_view of
      { name : string
      ; query : Ast.stmt
      }
  | Op_create_reactive_view of
      { name : string
      ; query : Ast.stmt
      ; refresh : Ast.refresh_mode
      }
  | Op_drop_view of { name : string }
  | Op_drop_reactive_view of
      { name : string
      ; if_exists : bool
      }
  | Op_create_trigger of
      { name : string
      ; timing : Ast.trigger_timing
      ; event : Ast.trigger_event
      ; table : string
      ; when_ : Ast.expr option
      ; body : Ast.stmt list
      }
  | Op_drop_trigger of { name : string }
  | Op_sqlite_master
  (** Virtual scan that reconstructs sqlite_master rows from catalog metadata. *)
  | Op_sqlite_sequence (** #312: virtual scan over AUTOINCREMENT counters (name, seq). *)
  | Op_seq_set of
      { table : string
      ; seq : int64
      } (** #312.1: writable sqlite_sequence SET/INSERT -> set [table]'s next_rowid. *)
  | Op_seq_reset of { table : string option }
  (** #312.1: writable sqlite_sequence DELETE -> reset [table]'s next_rowid;
      [None] (bare DELETE, no WHERE) resets every AUTOINCREMENT counter. *)
  | Op_no_op (** No-op plan node produced by IF EXISTS DROP when object not found. *)
  | Op_changes
  (** Returns rows affected by last DML. Intercepted in db.ml query — not exec.ml. *)
  | Op_last_insert_rowid
  (** Returns rowid of last INSERT. Intercepted in db.ml query — not exec.ml. *)
  | Op_total_changes
  (** Returns total rows affected since the connection was opened.
        Intercepted in db.ml query — not exec.ml. *)
  | Op_explain of
      { analyze : bool
      ; inner : op
      }

and proj_item =
  | PI_group_col of int (** project the i-th GROUP BY column (index into group_cols) *)
  | PI_agg_slot of int (** project the k-th aggregate result *)
  | PI_window_slot of int (** project the j-th post-aggregate window function result *)
  | PI_expr of expr
  (** #507: evaluate an expression over the aggregate output row
          [group_cols @ aggs @ window_results] *)

and agg_spec =
  { func : Ast.agg_func
  ; col_ord : int option (** [None] means COUNT-star *)
  }

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
