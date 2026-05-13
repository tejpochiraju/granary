(** In-memory catalog backed by system trees in the Store.
    System tree allocation: 0=_sys_tables, 1=_sys_columns, 3=_sys_meta.
    User tables use tree_ids >= 16. *)

type t

type table_meta = {
  name : string;
  tree_id : Sqlocaml_store.Store.tree_id;
  columns : Sqlocaml_encoding.Row.column list;
  next_rowid : int64;
}

(** Open (or initialise) a catalog on the given store.
    Loads all existing table metadata from the store into the in-memory cache. *)
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
