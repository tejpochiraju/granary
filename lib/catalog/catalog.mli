(** In-memory catalog backed by system trees in the Store.
    System tree allocation: 0=_sys_tables, 1=_sys_columns, 2=_sys_indexes,
    3=_sys_meta.  User tables and indexes use tree_ids >= 16. *)

type t

type table_meta = {
  name : string;
  tree_id : Sqlocaml_store.Store.tree_id;
  columns : Sqlocaml_encoding.Row.column list;
  next_rowid : int64;
}

type index_info = {
  idx_name    : string;
  idx_table   : string;
  idx_column  : string;
  idx_unique  : bool;
  idx_tree_id : Sqlocaml_store.Store.tree_id;
}

(** Open (or initialise) a catalog on the given store.
    Loads all existing table metadata and index metadata from the store. *)
val open_ : Sqlocaml_store.Store.t -> t Lwt.t

(** Create a new table, returning its assigned tree_id.
    Raises [Failure] if a table with that name already exists. *)
val create_table :
  t ->
  name:string ->
  columns:Sqlocaml_encoding.Row.column list ->
  Sqlocaml_store.Store.tree_id Lwt.t

(** Find a table by name. Returns [None] if not found. *)
val find_table : t -> name:string -> table_meta option Lwt.t

(** List all known tables. Order is unspecified. *)
val list_tables : t -> table_meta list Lwt.t

(** Allocate and return the next rowid for a table, incrementing the counter.
    Raises [Failure] if the table does not exist. *)
val next_rowid : t -> name:string -> int64 Lwt.t

(** Create a new index on a single column of an existing table.
    The new index gets its own [tree_id] (separate from the table tree).
    The index metadata is persisted in the [_sys_indexes] tree.
    The actual index entries are NOT populated here; callers (the executor)
    must scan the table and emit index entries within the same logical
    operation.

    Errors:
    - [`Error msg] if a table with [table] does not exist.
    - [`Error msg] if an index named [name] already exists.
    - [`Error msg] if [column] is not a column of [table]. *)
val create_index :
  t ->
  name:string ->
  table:string ->
  column:string ->
  unique:bool ->
  (index_info, string) result Lwt.t

(** Return the list of indexes on the given table.  Order is unspecified. *)
val indexes_for_table : t -> table:string -> index_info list

(** Look up an index by name. *)
val find_index : t -> name:string -> index_info option
