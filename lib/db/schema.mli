(** #433: an opaque, read-only projection of a database's in-memory catalog.

    {!Db.schema} hands one of these out instead of the live
    {!Granary_catalog.Catalog.t}, which is a read-{e write} surface:
    [Catalog.t] also carries [create_table], [drop_table], [add_column],
    [set_last_inserted_rowid], [store], … so a consumer could mutate the
    catalog — or reach the raw store — out of band from SQL DDL and the WAL.
    Before #433 that constraint lived in [Db.catalog]'s doc comment and was
    enforced by nothing.

    This type is a {e live view}, not a snapshot: it holds the very catalog the
    handle executes against, so DDL run through SQL is visible through it
    immediately and no staleness is possible. Nothing here copies the catalog —
    a clone would go stale, and duplicated catalog state is how this engine has
    previously lost rows (see CLAUDE.md's #589 / #633 sections).

    The projected records ({!table}) are plain immutable data. In particular
    {!table} deliberately does {e not} expose a columnar table's
    [Granary_columnar.Col_store.t], which {!Granary_catalog.Catalog.storage}
    does and which is itself mutable. *)

(** A read-only view of one database handle's catalog. *)
type t

(** Format a one-line summary of the underlying catalog (table/index counts). *)
val pp : Format.formatter -> t -> unit

(** The projection of one table's schema. All fields are immutable data. *)
type table =
  { name : string (** The table's name. *)
  ; columns : Granary_encoding.Row.column list
    (** Columns in declaration order: name, declared type, NOT NULL,
            PRIMARY KEY, DEFAULT, CHECK and GENERATED metadata. *)
  ; fk_constraints : Granary_catalog.Catalog.fk_constraint list
    (** FOREIGN KEY constraints declared on this table. *)
  ; without_rowid : bool (** True for a WITHOUT ROWID table. *)
  ; columnar : bool (** True for a [USING COLUMNSTORE] table. *)
  }

(** Format a one-line summary of a projected table. *)
val pp_table : Format.formatter -> table -> unit

(** Wrap a live catalog as a read-only view. This is the engine's own
    constructor — {!Db.schema} is how a caller obtains one. There is
    deliberately no inverse: a {!t} can never be turned back into a
    [Catalog.t]. *)
val of_catalog : Granary_catalog.Catalog.t -> t

(** All tables known to the catalog, in unspecified order. *)
val list_tables : t -> table list Lwt.t

(** [find_table s ~name] is the projection of table [name], or [None]. *)
val find_table : t -> name:string -> table option Lwt.t

(** True if a table with that name exists. *)
val table_exists : t -> name:string -> bool

(** Indexes on the given table, in unspecified order. [index_info] is an
    immutable record — name, table, key columns, uniqueness, partial-index
    WHERE clause, origin, and the #576 statistics. *)
val indexes_for_table : t -> table:string -> Granary_catalog.Catalog.index_info list

(** [find_index s ~name] is the index named [name], or [None]. *)
val find_index : t -> name:string -> Granary_catalog.Catalog.index_info option

(** True if an index with that name exists. *)
val index_exists : t -> name:string -> bool
