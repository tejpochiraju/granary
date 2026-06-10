(** In-memory catalog backed by system trees in the Store.
    System tree allocation: 0=_sys_tables, 1=_sys_columns, 2=_sys_indexes,
    3=_sys_meta, 4=_sys_fts_tables, 5=_sys_views, 6=_sys_triggers,
    7=_sys_catalog_mirror (#174 redundant schema copy).  User tables and
    indexes use tree_ids >= 16. *)

type t

(** Pretty-print a summary: table/index/FTS counts and FK-enforcement flag. *)
val pp : Format.formatter -> t -> unit

type fk_action =
  | FA_no_action
  | FA_restrict
  | FA_cascade
  | FA_set_null
  | FA_set_default

type fk_constraint =
  { fk_local_cols : string list
  ; fk_parent_table : string
  ; fk_parent_cols : string list
  ; fk_on_delete : fk_action
  ; fk_on_update : fk_action
  ; fk_deferrable : bool (** false = IMMEDIATE (default), true = INITIALLY DEFERRED *)
  }

(** Kind of pending FK check (matched against current row state at commit). *)
type pending_fk_kind =
  [ `Insert
  | `Update
  | `Delete
  ]

(** Re-verification callback for a queued FK violation.  Universally
    polymorphic in the transaction mode so it accepts either the active
    write txn (deferred path inside an explicit transaction) or an RO
    snapshot opened after auto-commit. *)
type pending_fk_recheck = { recheck : 'm. 'm Sqlocaml_store.Store.txn -> bool Lwt.t }

(** A queued FK violation awaiting re-verification at commit. *)
type pending_fk_check =
  { pfk_kind : pending_fk_kind
  ; pfk_table : string
  ; pfk_rowid : int64
  ; pfk_message : string
  ; pfk_recheck : pending_fk_recheck
    (** Re-run the FK check.  An open transaction (the active write txn
        about to be committed, or a fresh RO snapshot taken AFTER an
        auto-commit) is supplied so the recheck observes the live state —
        opening a separate RO snapshot mid-transaction on the B+-tree
        backend would see only the pre-txn state and report spurious
        violations.

        Returns true if STILL violated; false if the violation has been
        resolved (e.g. parent row now exists, child row now deleted). *)
  }

type table_meta =
  { name : string
  ; tree_id : Sqlocaml_store.Store.tree_id
  ; columns : Sqlocaml_encoding.Row.column list
  ; next_rowid : int64
  ; fk_constraints : fk_constraint list
  ; without_rowid : bool
    (** WITHOUT ROWID — the INTEGER PRIMARY KEY column's value is used
        as the row's storage key (no auto-allocated rowid).  Phase 37. *)
  ; autoincrement : bool
    (** #299: [INTEGER PRIMARY KEY AUTOINCREMENT].  When [true] the rowid
        counter is a sticky high-water mark: on ROLLBACK it reverts to the
        last-committed value (read back from [_sys_tables]) instead of being
        recomputed as [max(rowid)+1] from the data tree, so a committed DELETE
        of the top row is never reused. *)
  }

(** Sentinel [next_rowid] for a rowid/alias table that has never seeded its
    counter — conceptually [max = -inf].  Used to tell "never inserted" apart
    from a live counter (e.g. an AUTOINCREMENT table absent from
    [sqlite_sequence] until its first insert).  See #250/#312. *)
val empty_next_rowid : int64

(** #243 (T1): index of the INTEGER PRIMARY KEY column that aliases the rowid,
    if the table qualifies (rowid table, single INTEGER PRIMARY KEY column).
    For such tables the table tree is keyed by this column's value and no
    separate __pk index exists.  None for every other shape. *)
val compute_rowid_alias_col
  :  Sqlocaml_encoding.Row.column list
  -> without_rowid:bool
  -> int option

(** [rowid_alias_col m] = [compute_rowid_alias_col m.columns ~without_rowid]. *)
val rowid_alias_col : table_meta -> int option

(** #243 (T1): record the rowid of the most recently INSERTed row (for
    [last_insert_rowid()]).  Set by the executor after each successful insert. *)
val set_last_inserted_rowid : t -> int64 -> unit

(** #243 (T1): the rowid recorded by the last {!set_last_inserted_rowid}; read by
    the db layer to answer [last_insert_rowid()]. *)
val last_inserted_rowid : t -> int64

(** How an index came to exist.  [`Implicit_pk] and [`Implicit_unique] are
    auto-created from a table's PRIMARY KEY / UNIQUE constraints at
    {!create_table} time; [`User] is an explicit [CREATE INDEX].  The logical
    dump (#264) keys on this field to decide which indexes a replayed
    [CREATE TABLE] already recreates, rather than sniffing the [__pk_]/[__uniq_]
    name prefix the binder assigns (which a user index could collide with). *)
type idx_origin =
  [ `Implicit_pk
  | `Implicit_unique
  | `User
  ]

type index_info =
  { idx_name : string
  ; idx_table : string
  ; idx_columns : string list (* col names for plain; expr SQL for expression indexes *)
  ; idx_unique : bool
  ; idx_tree_id : Sqlocaml_store.Store.tree_id
  ; idx_expr_flags : bool list (* true = expression index column, false = plain column *)
  ; idx_where_sql : string option
  ; idx_origin : idx_origin
  }

type fts_table_meta =
  { fts_name : string
  ; fts_content_tree : Sqlocaml_store.Store.tree_id
  ; fts_index_tree : Sqlocaml_store.Store.tree_id
  ; fts_columns : string list
  }

(** Open (or initialise) a catalog on the given store.
    Loads all existing table metadata and index metadata from the store. *)
val open_ : Sqlocaml_store.Store.t -> t Lwt.t

(** Read the user_version from the sys_meta tree inside an already-open
    transaction (RO or RW).  Returns 0 if not yet set. *)
val read_user_version_tx : _ Sqlocaml_store.Store.txn -> int64 Lwt.t

(** Write user_version to the sys_meta tree inside an already-open RW
    transaction.  Caller is responsible for the commit. *)
val write_user_version_tx
  :  Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> int64
  -> unit Lwt.t

(** Create a new table, returning its assigned tree_id.
    Raises [Failure] if a table with that name already exists.

    [?txn] (#269): run the creation — tree-ID allocation, catalog rows, and
    cache insertion — through this already-held explicit writer transaction
    instead of opening (and committing) a fresh one.  This lets [CREATE TABLE]
    participate in an ambient [BEGIN … COMMIT] (no self-deadlock) and be undone
    by [rollback_schema_changes] on [ROLLBACK]. *)
val create_table
  :  ?txn:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> t
  -> name:string
  -> columns:Sqlocaml_encoding.Row.column list
  -> without_rowid:bool
  -> autoincrement:bool
  -> Sqlocaml_store.Store.tree_id Lwt.t

(** Find a table by name. Returns [None] if not found. *)
val find_table : t -> name:string -> table_meta option Lwt.t

(** Synchronous in-memory lookup (no Lwt). Always up to date since the cache
    is updated on every DDL operation. *)
val find_table_cached : t -> name:string -> table_meta option

(** Schema fingerprint (#174) of a table: a stable 64-bit hash of its shape
    (columns + WITHOUT ROWID), computed from the in-memory cache via
    {!Sqlocaml_encoding.Schema_fingerprint}.  [None] if no such table.  Stable
    across table renames; used for drift detection on open, the redundant
    catalog mirror, per-page fingerprint stamps, and replication schema
    matching (#92 / #172). *)
val table_fingerprint : t -> name:string -> int64 option

(** [(tree_id, fingerprint)] for every persistent table currently in the
    catalog (ephemeral CTE entries are excluded).  Used to register per-tree
    page-header stamps with the store (#174). *)
val fingerprints_by_tree_id : t -> (Sqlocaml_store.Store.tree_id * int64) list

(** [(tree_id, fingerprint)] recorded in the redundant catalog mirror (#174).
    On a healthy database this agrees with {!fingerprints_by_tree_id}; a
    divergence indicates schema drift or corruption in one of the two copies. *)
val mirror_fingerprints : t -> (Sqlocaml_store.Store.tree_id * int64) list Lwt.t

(** A disagreement between the primary catalog and the redundant mirror (#174). *)
type schema_discrepancy =
  | Fingerprint_mismatch of
      { tree_id : Sqlocaml_store.Store.tree_id
      ; primary : int64 (** fingerprint computed from the primary catalog *)
      ; mirror : int64 (** fingerprint recorded in the mirror *)
      }
  | Missing_in_mirror of Sqlocaml_store.Store.tree_id
  (** a table present in the primary catalog has no mirror entry *)
  | Missing_in_primary of Sqlocaml_store.Store.tree_id
  (** the mirror records a table the primary catalog does not have *)

(** Cross-check the primary catalog against the redundant mirror and report
    every disagreement (#174).  An empty list means the two copies agree.  A
    non-empty list signals schema drift or corruption; [open_] also logs a
    warning for any {!Fingerprint_mismatch} it finds, but stays openable so
    recovery tooling can run. *)
val verify_against_mirror : t -> schema_discrepancy list Lwt.t

(** Temporarily register an ephemeral (CTE) table entry in the in-memory cache.
    tree_id = -1 is the sentinel for CTE virtual tables. *)
val register_ephemeral : t -> table_meta -> unit

(** Remove a previously registered ephemeral table entry from the in-memory cache. *)
val unregister_ephemeral : t -> name:string -> unit

(** List all known tables. Order is unspecified. *)
val list_tables : t -> table_meta list Lwt.t

(** Allocate and return the next rowid for a table, incrementing the counter.
    Raises [Failure] if the table does not exist. *)
val next_rowid : t -> name:string -> int64 Lwt.t

(** Like [next_rowid] but operates within an already-held RW transaction.
    Does NOT commit; the caller owns the commit.  Use within explicit
    transactions to avoid deadlocking on the store's writer mutex. *)
val next_rowid_in_txn
  :  ?defer_counter:bool
  -> t
  -> name:string
  -> Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> int64 Lwt.t

(** #243 (T1): advance a table's autoincrement counter so the next allocation is
    at least [at_least] (= an explicitly-inserted INTEGER PRIMARY KEY value + 1),
    matching SQLite. Never lowers the counter; does NOT commit. *)
val bump_next_rowid_in_txn
  :  ?defer_counter:bool
  -> t
  -> name:string
  -> at_least:int64
  -> Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> unit Lwt.t

(** #312.1: writable [sqlite_sequence] SET/INSERT.  Sets table [name]'s
    autoincrement counter to [max(requested + 1, max(rowid) + 1)] (SQLite's
    effective rule).  Mutates through the held RW transaction (does NOT commit)
    and marks the counter dirty so a ROLLBACK reverts it.  Fails if [name] is
    unknown or not an AUTOINCREMENT table. *)
val set_next_rowid_in_txn
  :  t
  -> name:string
  -> requested:int64
  -> Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> unit Lwt.t

(** #312.1: writable [sqlite_sequence] DELETE.  Resets table [name]'s
    autoincrement counter to the never-seeded sentinel so the next insert
    recomputes from data.  Mutates through the held RW transaction (does NOT
    commit) and marks the counter dirty so a ROLLBACK reverts it.  Fails if
    [name] is unknown or not an AUTOINCREMENT table. *)
val reset_next_rowid_in_txn
  :  t
  -> name:string
  -> Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> unit Lwt.t

(** #312.1: writable [sqlite_sequence] [DELETE] with no WHERE — reset every
    seeded AUTOINCREMENT counter (SQLite parity; what [sqlite3 .dump] emits).
    Mutates through the held RW transaction (does NOT commit). *)
val reset_all_next_rowid_in_txn
  :  t
  -> Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> unit Lwt.t

(** #347: flush all dirty rowid counters into the B-tree under [tx].  Call once
    before [S.commit] when inserts used [~defer_counter:true] to skip per-row
    [put_table_counter_tx] writes.  Clears the dirty set; the subsequent
    [commit_schema_changes] reset is a no-op. *)
val flush_dirty_counters_tx
  :  t
  -> Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> unit Lwt.t

(** Create a new index on a single column of an existing table.
    The new index gets its own [tree_id] (separate from the table tree).
    The index metadata is persisted in the [_sys_indexes] tree.
    The actual index entries are NOT populated here; callers (the executor)
    must scan the table and emit index entries within the same logical
    operation.

    Errors:
    - [`Error msg] if a table with [table] does not exist.
    - [`Error msg] if an index named [name] already exists.
    - [`Error msg] if any column in [columns] is not a column of [table].

    [?txn] (#269): run through this already-held explicit writer transaction
    instead of opening (and committing) a fresh one. *)
val create_index
  :  ?txn:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> t
  -> name:string
  -> table:string
  -> columns:string list
  -> unique:bool
  -> expr_flags:bool list
  -> where_sql:string option
  -> origin:idx_origin
  -> (index_info, string) result Lwt.t

(** Return the list of indexes on the given table.  Order is unspecified. *)
val indexes_for_table : t -> table:string -> index_info list

(** Look up an index by name. *)
val find_index : t -> name:string -> index_info option

(** [find_index_covering_cols cat ~table_name ~col_idxs] returns an index whose
    leading key columns match [col_idxs] (in order), and which is safe to use
    for FK enforcement scans:
      - all columns are plain (no expression columns);
      - no partial-index WHERE clause.
    Returns [None] if no such index exists.

    The index may have more columns than [col_idxs]; only the leading prefix is
    required to match. *)
val find_index_covering_cols
  :  t
  -> table_name:string
  -> col_idxs:int list
  -> index_info option

(** Remove a table and all its indexes from the catalog.
    Removes entries from _sys_tables, _sys_columns, and _sys_indexes.
    The B+-tree pages for the table and its indexes are NOT reclaimed (Phase 3). *)
val drop_table
  :  t
  -> Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> name:string
  -> unit Lwt.t

(** Remove an index from _sys_indexes.
    The B+-tree pages are NOT reclaimed (Phase 3). *)
val drop_index
  :  t
  -> Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> name:string
  -> unit Lwt.t

(** Add a new column to an existing table.
    Updates the catalog's persistent storage and in-memory cache.
    Returns [Error msg] if the table does not exist or the column already exists.

    [?txn] (#282): write through this already-held explicit writer transaction
    instead of opening a fresh one, and register a schema-cache undo so a
    [ROLLBACK] restores the prior table shape. *)
val add_column
  :  ?txn:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> t
  -> table_name:string
  -> column:Sqlocaml_encoding.Row.column
  -> (unit, string) result Lwt.t

(** Rename a table.
    Updates _sys_tables, re-keys all _sys_columns entries, and refreshes the
    in-memory cache and any index entries that reference the old table name.
    Returns [Error msg] if [old_name] does not exist or [new_name] already exists.

    [?txn] (#282): as for [add_column]. *)
val rename_table
  :  ?txn:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> t
  -> old_name:string
  -> new_name:string
  -> (unit, string) result Lwt.t

(** Rename a column within a table.
    Updates the _sys_columns entry and refreshes the in-memory cache.
    Returns [Error msg] if the table or column does not exist.

    [?txn] (#282): as for [add_column]. *)
val rename_column
  :  ?txn:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> t
  -> table_name:string
  -> old_col:string
  -> new_col:string
  -> (unit, string) result Lwt.t

(** Remove a column from an existing table.
    Re-keys all column entries with ordinal > drop_idx (shift down by 1).
    Does NOT migrate existing row data — caller (the executor) is responsible.
    Returns [Error msg] if the table or column does not exist.

    [?txn] (#282): as for [add_column]. *)
val drop_column
  :  ?txn:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> t
  -> table_name:string
  -> col_name:string
  -> (unit, string) result Lwt.t

(** True if a table with [name] exists in the catalog. *)
val table_exists : t -> name:string -> bool

(** True if an index with [name] exists in the catalog. *)
val index_exists : t -> name:string -> bool

(** Find an FTS table by name. Returns [None] if not found. *)
val find_fts : t -> string -> fts_table_meta option

(** List all FTS virtual tables in the catalog. Order is unspecified. *)
val list_fts_tables : t -> fts_table_meta list

(** Create a new FTS virtual table.  Allocates two new tree IDs (content tree
    and inverted-index tree) and persists the entry in the [_sys_fts_tables]
    system tree.  Raises [Failure] if a table with that name already exists.

    [?txn] (#269): run the creation through this already-held explicit writer
    transaction instead of opening (and committing) a fresh one. *)
val create_fts_table
  :  ?txn:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> t
  -> name:string
  -> columns:string list
  -> fts_table_meta Lwt.t

(** Allocate and return the next rowid for an FTS table within an already-held
    RW transaction.  Does NOT commit; the caller owns the commit. *)
val next_fts_rowid_in_txn
  :  t
  -> name:string
  -> Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> int64 Lwt.t

(** #330: advance an FTS table's rowid high-water so the next auto-allocated
    rowid is past [rowid] (used after an explicit-rowid insert).  No-op when the
    counter is already beyond it.  Does NOT commit; the caller owns the commit. *)
val ensure_fts_rowid_above_in_txn
  :  t
  -> name:string
  -> Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> int64
  -> unit Lwt.t

(** Persist FK constraints for a table to the sys_meta B-tree.

    [?txn] (#269): write through this already-held explicit writer transaction
    instead of opening (and committing) a fresh one. *)
val save_fk_constraints
  :  ?txn:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> t
  -> table_name:string
  -> fks:fk_constraint list
  -> unit Lwt.t

(** Update FK constraints in the in-memory cache for a table. *)
val set_fk_constraints : t -> table_name:string -> fks:fk_constraint list -> unit

(** #269: register an in-memory schema-cache reversal for DDL run through an
    explicit transaction.  The closure is invoked by [rollback_schema_changes]
    if the transaction is rolled back; it is discarded by [commit_schema_changes]
    on commit.  Used by the db layer for view/trigger caches it owns.

    The closure MUST be idempotent (e.g. [Hashtbl.replace]/[remove] over values
    captured at registration, not read-modify-write of live state).  #280's
    [savepoint_rollback_schema] reverts the cache by re-running the closures
    registered since a savepoint; its defensive "snapshot not reached" branch
    can also leave an already-run closure queued for the outer ROLLBACK.  Both
    rely on a second invocation being a no-op. *)
val register_schema_undo : t -> (unit -> unit) -> unit

(** #269: discard the pending schema-undo closures — the transaction's DDL is now
    durable.  Call at COMMIT (and savepoint-release auto-commit). *)
val commit_schema_changes : t -> unit

(** #269: run the pending schema-undo closures (most-recent-first), reverting the
    in-memory cache mutations of DDL whose store writes were just rolled back.
    Call at ROLLBACK. *)
val rollback_schema_changes : t -> unit

(** #293: re-derive the cached [next_rowid] from the data tree (max(rowid)+1, as
    at open-time) for ONLY the rowid tables bumped during the rolled-back
    transaction, discarding the in-memory counter bump made by INSERTs run
    through it.  This makes the next rowid allocation reuse a rolled-back rowid,
    matching SQLite for plain (non-AUTOINCREMENT) rowid tables.  Restricting to
    the bumped set (usually one table) keeps rollback ~O(1) instead of scanning
    every table's tree.  Clears the bumped set.  Must be called by the db layer
    AFTER the store rollback (so the trees show last-committed state and the RW
    lock is free), and after [rollback_schema_changes] (which leaves the bumped
    set intact for this call to consume). *)
val recompute_rowid_counters_after_rollback : t -> unit Lwt.t

(** #280: open a schema-undo savepoint named [name].  Records the current
    undo-log position so a later [savepoint_rollback_schema]/
    [savepoint_release_schema] of [name] can act on only the entries registered
    since this point.  Call from the db layer's SAVEPOINT handler. *)
val savepoint_begin_schema : t -> string -> unit

(** #280: ROLLBACK TO savepoint [name] — run (most-recent-first) and drop the
    schema-undo closures registered since [name] was opened, reverting their
    in-memory cache mutations to agree with the store rolled back to [name].
    Keeps [name] (re-rollback-able) and drops newer savepoints.  No-op if [name]
    is unknown. *)
val savepoint_rollback_schema : t -> string -> unit

(** #280: RELEASE savepoint [name] — drop the marker (and newer ones) without
    running any undo; the since-[name] entries merge into the enclosing scope so
    an outer ROLLBACK still unwinds them.  No-op if [name] is unknown. *)
val savepoint_release_schema : t -> string -> unit

(** #286: mark the ambient explicit transaction uncommittable.  Called when an
    in-txn DDL statement fails partway through — its borrowed txn is left open
    with partial on-disk effects, so a subsequent COMMIT must be forced to roll
    back instead.  Cleared by [commit_schema_changes]/[rollback_schema_changes]. *)
val mark_schema_txn_poisoned : t -> unit

(** #286: whether the active explicit transaction has been poisoned by a failed
    in-txn DDL statement.  The db layer checks this at COMMIT and rolls back
    (returning an error) instead of persisting partial DDL effects. *)
val schema_txn_poisoned : t -> bool

(** Load all persisted view definitions. Returns [(view_name, create_view_sql)] pairs. *)
val load_all_views : Sqlocaml_store.Store.t -> (string * string) list Lwt.t

(** #322/#323: load all view definitions through a caller-supplied snapshot,
    so [Db.dump] can read view DDL under its single shared RO snapshot and keep
    the schema section point-in-time consistent with the row data. *)
val load_all_views_in_tx : _ Sqlocaml_store.Store.txn -> (string * string) list Lwt.t

(** Persist a view's SQL text to the sys_views B-tree.

    [?txn] (#269): write through this already-held explicit writer transaction
    instead of opening (and committing) a fresh one. *)
val persist_view
  :  ?txn:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> Sqlocaml_store.Store.t
  -> name:string
  -> sql:string
  -> unit Lwt.t

(** Remove a view's SQL text from the sys_views B-tree.  [?txn] as above (#269). *)
val remove_view
  :  ?txn:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> Sqlocaml_store.Store.t
  -> name:string
  -> unit Lwt.t

(** Load all persisted trigger definitions. Returns [(trigger_name, create_trigger_sql)] pairs. *)
val load_all_triggers : Sqlocaml_store.Store.t -> (string * string) list Lwt.t

(** #322/#323: trigger-DDL counterpart to {!load_all_views_in_tx}. *)
val load_all_triggers_in_tx : _ Sqlocaml_store.Store.txn -> (string * string) list Lwt.t

(** Persist a trigger's CREATE TRIGGER SQL to the sys_triggers B-tree.
    Call this whenever CREATE TRIGGER is executed.

    [?txn] (#269): write through this already-held explicit writer transaction
    instead of opening (and committing) a fresh one. *)
val persist_trigger
  :  ?txn:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> Sqlocaml_store.Store.t
  -> name:string
  -> sql:string
  -> unit Lwt.t

(** Remove a trigger's SQL from the sys_triggers B-tree.
    Call this whenever DROP TRIGGER is executed.  [?txn] as above (#269). *)
val remove_trigger
  :  ?txn:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> Sqlocaml_store.Store.t
  -> name:string
  -> unit Lwt.t

(** Get the current FK enforcement flag (default false). *)
val get_fk_enforcement : t -> bool

(** Set the FK enforcement flag (PRAGMA foreign_keys = 0/1). *)
val set_fk_enforcement : t -> bool -> unit

(** Get the current recursive-triggers flag (default true in sqlocaml — a
    deliberate divergence from real SQLite which defaults OFF). When this
    flag is false, DML executed inside a trigger body does NOT fire further
    triggers. *)
val get_recursive_triggers : t -> bool

(** Set the recursive-triggers flag (PRAGMA recursive_triggers = 0/1). *)
val set_recursive_triggers : t -> bool -> unit

(** Get the runtime PRAGMA defer_foreign_keys flag (default false). When ON
    every FK enforcement site behaves as DEFERRED for the duration of the
    transaction. SQLite resets this flag to OFF at every transaction end. *)
val get_defer_fks_pragma : t -> bool

(** Set the runtime PRAGMA defer_foreign_keys flag. *)
val set_defer_fks_pragma : t -> bool -> unit

(** Append a pending FK check to be drained at commit time. *)
val queue_pending_fk_check : t -> pending_fk_check -> unit

(** Return and clear the pending FK check list. *)
val drain_pending_fk_checks : t -> pending_fk_check list

(** Clear the pending FK check list without raising (used on rollback). *)
val clear_pending_fk_checks : t -> unit

(** Peek at the current pending FK check count (debugging/tests). *)
val pending_fk_check_count : t -> int

(** Underlying store handle. Exposed so subsystems (FK deferred rechecks)
    can open their own RO snapshots without threading the store through
    every function signature. *)
val store : t -> Sqlocaml_store.Store.t
