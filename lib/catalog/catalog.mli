(** In-memory catalog backed by system trees in the Store.
    System tree allocation: 0=_sys_tables, 1=_sys_columns, 2=_sys_indexes,
    3=_sys_meta, 4=_sys_fts_tables, 5=_sys_views, 6=_sys_triggers.  User tables and indexes use tree_ids >= 16. *)

type t

type fk_action =
  | FA_no_action
  | FA_restrict
  | FA_cascade
  | FA_set_null
  | FA_set_default

type fk_constraint = {
  fk_local_col    : string;
  fk_parent_table : string;
  fk_parent_col   : string;
  fk_on_delete    : fk_action;
  fk_on_update    : fk_action;
}

type table_meta = {
  name            : string;
  tree_id         : Sqlocaml_store.Store.tree_id;
  columns         : Sqlocaml_encoding.Row.column list;
  next_rowid      : int64;
  fk_constraints  : fk_constraint list;
}

type index_info = {
  idx_name      : string;
  idx_table     : string;
  idx_columns   : string list;      (* col names for plain; expr SQL for expression indexes *)
  idx_unique    : bool;
  idx_tree_id   : Sqlocaml_store.Store.tree_id;
  idx_expr_flags: bool list;        (* true = expression index column, false = plain column *)
  idx_where_sql : string option;
}

type fts_table_meta = {
  fts_name         : string;
  fts_content_tree : Sqlocaml_store.Store.tree_id;
  fts_index_tree   : Sqlocaml_store.Store.tree_id;
  fts_columns      : string list;
}

(** Open (or initialise) a catalog on the given store.
    Loads all existing table metadata and index metadata from the store. *)
val open_ : Sqlocaml_store.Store.t -> t Lwt.t

(** Read the user_version from the sys_meta tree inside an already-open
    transaction (RO or RW).  Returns 0 if not yet set. *)
val read_user_version_tx : _ Sqlocaml_store.Store.txn -> int64 Lwt.t

(** Write user_version to the sys_meta tree inside an already-open RW
    transaction.  Caller is responsible for the commit. *)
val write_user_version_tx :
  Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn -> int64 -> unit Lwt.t

(** Create a new table, returning its assigned tree_id.
    Raises [Failure] if a table with that name already exists. *)
val create_table :
  t ->
  name:string ->
  columns:Sqlocaml_encoding.Row.column list ->
  Sqlocaml_store.Store.tree_id Lwt.t

(** Find a table by name. Returns [None] if not found. *)
val find_table : t -> name:string -> table_meta option Lwt.t

(** Synchronous in-memory lookup (no Lwt). Always up to date since the cache
    is updated on every DDL operation. *)
val find_table_cached : t -> name:string -> table_meta option

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
val next_rowid_in_txn :
  t -> name:string -> Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn -> int64 Lwt.t

(** Create a new index on a single column of an existing table.
    The new index gets its own [tree_id] (separate from the table tree).
    The index metadata is persisted in the [_sys_indexes] tree.
    The actual index entries are NOT populated here; callers (the executor)
    must scan the table and emit index entries within the same logical
    operation.

    Errors:
    - [`Error msg] if a table with [table] does not exist.
    - [`Error msg] if an index named [name] already exists.
    - [`Error msg] if any column in [columns] is not a column of [table]. *)
val create_index :
  t ->
  name:string ->
  table:string ->
  columns:string list ->
  unique:bool ->
  expr_flags:bool list ->
  where_sql:string option ->
  (index_info, string) result Lwt.t

(** Return the list of indexes on the given table.  Order is unspecified. *)
val indexes_for_table : t -> table:string -> index_info list

(** Look up an index by name. *)
val find_index : t -> name:string -> index_info option

(** Remove a table and all its indexes from the catalog.
    Removes entries from _sys_tables, _sys_columns, and _sys_indexes.
    The B+-tree pages for the table and its indexes are NOT reclaimed (Phase 3). *)
val drop_table :
  t ->
  Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn ->
  name:string ->
  unit Lwt.t

(** Remove an index from _sys_indexes.
    The B+-tree pages are NOT reclaimed (Phase 3). *)
val drop_index :
  t ->
  Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn ->
  name:string ->
  unit Lwt.t

(** Add a new column to an existing table.
    Updates the catalog's persistent storage and in-memory cache.
    Returns [Error msg] if the table does not exist or the column already exists. *)
val add_column :
  t ->
  table_name:string ->
  column:Sqlocaml_encoding.Row.column ->
  (unit, string) result Lwt.t

(** Rename a table.
    Updates _sys_tables, re-keys all _sys_columns entries, and refreshes the
    in-memory cache and any index entries that reference the old table name.
    Returns [Error msg] if [old_name] does not exist or [new_name] already exists. *)
val rename_table :
  t -> old_name:string -> new_name:string -> (unit, string) result Lwt.t

(** Rename a column within a table.
    Updates the _sys_columns entry and refreshes the in-memory cache.
    Returns [Error msg] if the table or column does not exist. *)
val rename_column :
  t -> table_name:string -> old_col:string -> new_col:string -> (unit, string) result Lwt.t

(** Remove a column from an existing table.
    Re-keys all column entries with ordinal > drop_idx (shift down by 1).
    Does NOT migrate existing row data — caller (the executor) is responsible.
    Returns [Error msg] if the table or column does not exist. *)
val drop_column :
  t -> table_name:string -> col_name:string -> (unit, string) result Lwt.t

(** True if a table with [name] exists in the catalog. *)
val table_exists : t -> name:string -> bool

(** True if an index with [name] exists in the catalog. *)
val index_exists : t -> name:string -> bool

(** Find an FTS table by name. Returns [None] if not found. *)
val find_fts : t -> string -> fts_table_meta option

(** Create a new FTS virtual table.  Allocates two new tree IDs (content tree
    and inverted-index tree) and persists the entry in the [_sys_fts_tables]
    system tree.  Raises [Failure] if a table with that name already exists. *)
val create_fts_table : t -> name:string -> columns:string list -> fts_table_meta Lwt.t

(** Allocate and return the next rowid for an FTS table within an already-held
    RW transaction.  Does NOT commit; the caller owns the commit. *)
val next_fts_rowid_in_txn :
  t -> name:string -> Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn -> int64 Lwt.t

(** Persist FK constraints for a table to the sys_meta B-tree. *)
val save_fk_constraints : t -> table_name:string -> fks:fk_constraint list -> unit Lwt.t

(** Update FK constraints in the in-memory cache for a table. *)
val set_fk_constraints : t -> table_name:string -> fks:fk_constraint list -> unit

(** Load all persisted view definitions. Returns [(view_name, create_view_sql)] pairs. *)
val load_all_views : Sqlocaml_store.Store.t -> (string * string) list Lwt.t

(** Persist a view's SQL text to the sys_views B-tree. *)
val persist_view : Sqlocaml_store.Store.t -> name:string -> sql:string -> unit Lwt.t

(** Remove a view's SQL text from the sys_views B-tree. *)
val remove_view : Sqlocaml_store.Store.t -> name:string -> unit Lwt.t

(** Load all persisted trigger definitions. Returns [(trigger_name, create_trigger_sql)] pairs. *)
val load_all_triggers : Sqlocaml_store.Store.t -> (string * string) list Lwt.t

(** Persist a trigger's CREATE TRIGGER SQL to the sys_triggers B-tree.
    Call this whenever CREATE TRIGGER is executed. *)
val persist_trigger : Sqlocaml_store.Store.t -> name:string -> sql:string -> unit Lwt.t

(** Remove a trigger's SQL from the sys_triggers B-tree.
    Call this whenever DROP TRIGGER is executed. *)
val remove_trigger : Sqlocaml_store.Store.t -> name:string -> unit Lwt.t

(** Get the current FK enforcement flag (default false). *)
val get_fk_enforcement : t -> bool

(** Set the FK enforcement flag (PRAGMA foreign_keys = 0/1). *)
val set_fk_enforcement : t -> bool -> unit
