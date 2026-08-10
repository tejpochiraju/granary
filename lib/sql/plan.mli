(** Physical query-plan IR.

    The planner ({!module:Planner}) lowers a bound {!Ast.stmt} into an {!op}
    tree, which the executor ({!module:Exec}) walks to produce rows.  Plan
    expressions ({!expr}) differ from {!Ast.expr} in that column references are
    resolved to integer ordinals.  Every type is exposed concretely because the
    planner constructs and the executor matches on the full structure. *)

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

(** #517: a range restriction on the index column immediately after a seek's
    equality-covered prefix — the [range] of an [equality* \[range\]] access
    path.

    Both ends are treated as {b inclusive} regardless of whether the SQL wrote
    [>] or [>=].  That is deliberate: the bound is only ever a narrowing, never
    a substitute for the predicate, which the plan still evaluates on every row
    the seek produces.  Treating a strict bound as inclusive can only widen the
    span scanned by one key, and keeps the encoding free of off-by-one
    reasoning.

    Only [Integer] and [Real] columns are bounded, because their index-key
    encoding is 9 bytes wide and order-preserving, so the stop condition can be
    a byte comparison at a known offset.  Text and blob encodings are
    variable-length, which would make that comparison depend on where the next
    column's bytes begin; those fall back to an unbounded prefix scan.

    Note that width is {i not} what makes the stop test sound — a NULL (or a
    NaN real) in the bounded column encodes to a single byte, so the comparison
    window runs past the column boundary on those entries.  It is still correct,
    for a reason that turns on the ordering of the encoding's type tags rather
    than their widths; the argument is written out at [range_seek_bounds] in
    exec.ml.  (Named in prose, not as an odoc reference: that function is
    internal to [Exec] and not in its signature, and [Exec] depends on [Plan]
    rather than the other way round.) *)
type range =
  { r_ty : Granary_encoding.Row.ty (** the bounded column's type *)
  ; r_lo : expr option (** lower end, inclusive *)
  ; r_hi : expr option (** upper end, inclusive *)
  }

(** #508: a narrowing access path for a DML statement's WHERE clause.  It only
    restricts the candidate rows the write path considers — the full WHERE
    predicate is still evaluated on every candidate — so a seek can never change
    which rows a statement affects, only how many are read to find them. *)
type seek =
  | Seek_rowid of expr (** the INTEGER PRIMARY KEY rowid alias is pinned *)
  | Seek_index of
      { idx_tree : int
      ; keys : (int * Granary_encoding.Row.ty * expr) list
        (** leading index columns pinned by equality, in index-column order *)
      ; range : range option
        (** #517: an optional range over the index column right after [keys] *)
      ; bail_out_at : int option
        (** #550: for a seek over a NON-UNIQUE index, the planner has no
            cardinality statistic to know ahead of time how many rows the
            equality prefix matches — unlike a range bound, whose window is
            read straight off the query, an equality prefix on a non-unique
            index could match anywhere from one row to the whole table.
            [Some k] tells the DML drain to abandon the index walk once it has
            walked more than [k] entries and fall back to a full table scan
            instead. [None] means "walk unconditionally": always true for
            {!Seek_rowid} (at most one row) and for a UNIQUE index (this field
            is only ever set on the [Seek_index] variant, and only when the
            index it names is not unique). *)
      }

(** #516: one component of a nested-loop join's index probe key.

    The probe key is built in index-column order from two sources: the join
    column, whose value differs per left row, and equality conjuncts of the
    WHERE clause that pin other columns of the same index to a constant.  A
    right table keyed on [(a, b)] and joined on [b] is only seekable because
    [a = ?] in the WHERE clause supplies the leading column.

    {b The constant parts narrow the probe and nothing else.}  They are taken
    only from the top-level [AND] spine of the WHERE clause, which the planner
    still applies in full to the joined row, so a constant can never change
    which joined rows survive — only how many right rows are read to find
    them. *)
type probe_part =
  | Probe_from_left of int (** take the value from this column ordinal of the LEFT row *)
  | Probe_const of expr (** a constant pinned by a WHERE equality on the right table *)

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
  (** DDL: [CREATE TABLE name (...) USING COLUMNSTORE].
          Registers the table in the catalog with [Columnar] storage. *)
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
        (** #635: the FROM item's alias, when it has one.

            An alias {b replaces} the table name as this input's scope
            identifier rather than adding to it, exactly as it does inside a
            subquery's own FROM ([Exec.inner_scope_of]).  So [FROM l AS x] makes
            an outer reference [x.a] resolvable and [l.a] {i not} — which is
            what sqlite3 does, and what lets a self-join with distinct aliases
            be resolved where a self-join without them still cannot be.

            Before #635 the alias existed only in the [Ast] and in [Sema]'s
            [tables]; it was dropped at plan construction, so
            [Exec.get_outer_scan_metas] could only match on
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
        (** #508: the leading index columns pinned by equality, in INDEX column
            order — [(column ordinal, column type, value expression)].  A
            single-column index yields a one-element list; a composite index
            yields one element per covered leading column, and the seek matches
            that encoded prefix.  Conjuncts not consumed here are left to a
            residual [Op_filter] above. *)
      ; range : range option
        (** #517: an optional range over the index column immediately after
            [keys], narrowing the span the seek scans.  See {!range} — it never
            replaces the predicate. *)
      ; table_meta : Cat.table_meta (** for row decoding *)
      ; alias : string option (** #635: see {!Op_seq_scan}'s [alias] *)
      }
  | Op_rowid_lookup of
      { table_meta : Cat.table_meta
      ; lookup_val : expr
        (** #243 (T1): point lookup on an INTEGER PRIMARY KEY rowid alias — a
            single table-tree seek by the integer key, no index. *)
      ; alias : string option (** #635: see {!Op_seq_scan}'s [alias] *)
      }
  | Op_col_seq_scan of
      { table_meta : Cat.table_meta
      ; alias : string option (** #635: see {!Op_seq_scan}'s [alias] *)
      } (** Scan a columnar table; emits one [Row.t] per stored row. *)
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
      ; probe : probe_part list
        (** the probe key, in index-column order, covering a leading prefix of
            [idx_tree]'s columns.  Always contains at least one
            {!Probe_from_left}, or the join would not be driven by the left
            input at all. *)
      ; probe_range : range option
        (** #570: a range bound on the index column {i after} the probe key's
            last pinned column, re-based to the right table's ordinals by the
            same {!Planner} pass that feeds the hash join's build side (#532).

            The probe key stops at the first index column pinned by neither the
            left row nor a WHERE equality; before #570 a range on that column
            contributed nothing, so whether it narrowed the read depended on
            which strategy the cost model happened to pick.  It is applied per
            driving row exactly as [Op_index_lookup]'s [range] is applied to a
            seek, and is sound for the same reason: [chain_joins] applies the
            whole WHERE clause to the joined row, so narrowing what a probe
            reads cannot change which joined rows survive. *)
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
      ; limit : int option
        (** #687: applied to the score-sorted match list BEFORE the
          content-tree fetch, so a bounded [LIMIT] does not pay a fetch per
          unreturned match. *)
      ; offset : int option (** #687: sliced together with [limit], same window. *)
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
  | Op_pragma_checkpoint_status
  (** #638 read the checkpoint-failure signal: one row of
      (total_failures, consecutive_failures, last_error). *)
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
  ; col_ord : int option (** [None] means COUNT-star, or an expression argument *)
  ; arg_expr : expr option
    (** #488: the aggregate's argument as a general expression, evaluated
          against each INPUT row before accumulating.  [None] for the bare
          column and COUNT-star forms, which stay on [col_ord] so the #247
          fast path can keep pruning the decode to the columns it needs.
          When this is [Some _], [col_ord] is [None] and every consumer must
          read the argument through it. *)
  ; distinct : bool
    (** #491: the argument list carried [DISTINCT]; the aggregate consumes each
            distinct argument value once.  Orthogonal to how the argument is
            READ: it applies equally to the [col_ord] and [arg_expr] forms,
            because the dedup key is the argument VALUE, not a column.  Always
            [false] for a COUNT-star, which has no argument to deduplicate. *)
  }
