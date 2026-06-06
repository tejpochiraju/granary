(** Public API for sqlocaml — pure-OCaml in-memory SQL engine (Phase 0). *)

type t

(** Pretty-print a summary: path, active schema, savepoint depth, total changes. *)
val pp : Format.formatter -> t -> unit

(** Re-export value type for convenience. *)
type value = Sqlocaml_encoding.Row.value =
  | V_int of int64
  | V_text of string
  | V_null
  | V_real of float
  | V_blob of bytes

type row = Sqlocaml_encoding.Row.t (* value array *)

type error =
  | Parse of string (** SQL syntax error *)
  | Sema of Sqlocaml_sql.Sema.error (** name/type error *)
  | Runtime of string (** unexpected internal error *)

(** #239: per-query cost/stats signal for an external cost-based cache,
    re-exported from {!Sqlocaml_sql.Exec.query_stats}.  [rows_examined] is the
    rows pulled from a base table/index scan (the work signal — a full scan
    behind a selective filter reads [rows_examined = N] returning one row);
    [rows_returned] is the result size once the stream is drained; [used_index]
    is the plan-time fact that the base access is an index/rowid/FTS seek rather
    than a full scan.  See {!query_with_stats} / {!iter_with_stats}. *)
type query_stats = Sqlocaml_sql.Exec.query_stats =
  { mutable rows_examined : int
  ; mutable rows_returned : int
  ; mutable used_index : bool
  }

(** Open a fresh in-memory database.  [clock] is an optional Unix-timestamp
    provider used by SQL date/time functions when invoked with the literal
    ['now'].  Omit for MirageOS-friendly cores that have no Unix dependency. *)
val open_in_memory : ?clock:(unit -> float) -> unit -> t Lwt.t

(** Open a SQL engine on any block device given as I/O callbacks.
    Use with [Sqlocaml_mirage_block.Mirage_backend.Make(B)] to build
    the callbacks from a [Mirage_block.S] device.  Pass [~n_pages:0L]
    for Mirage adapters; the adapter handles device-capacity bounds
    internally.  [~close] is called by [Db.close].

    This is the platform-agnostic entry point.  Unix file convenience
    constructors ([open_file] / [open_file_wal]) live in the [sqlocaml.unix]
    driver library so this core carries no [unix] dependency (#170). *)
val open_block
  :  ?geom:Sqlocaml_storage.Geometry.t
  -> read_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> write_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> sync:(unit -> (unit, string) result Lwt.t)
  -> resize:(n_pages:int64 -> (unit, string) result Lwt.t)
  -> n_pages:int64
  -> close:(unit -> unit Lwt.t)
  -> unit
  -> (t, error) result Lwt.t

(** Wrap an already-open {!Sqlocaml_store.Store.t} as a database handle,
    loading its catalog, views, and triggers.  [file_path] records the
    on-disk path for file-backed handles so VACUUM can rebuild in place;
    omit it for in-memory / arbitrary block devices.  Used by the
    [sqlocaml.unix] driver and by ATTACH. *)
val of_store
  :  ?clock:(unit -> float)
  -> ?file_path:string
  -> Sqlocaml_store.Store.t
  -> t Lwt.t

(** File operations the engine needs for ATTACH and VACUUM.  The core has no
    OS/filesystem dependency (#170); a platform driver (e.g. [sqlocaml.unix])
    supplies these via {!set_file_provider}.  Without a provider, ATTACH and
    VACUUM fail with a clear error. *)
type file_provider =
  { open_store :
      ?geom:Sqlocaml_store.Store.Geometry.t
      -> path:string
      -> unit
      -> (Sqlocaml_store.Store.t, Sqlocaml_store.Store.error) result Lwt.t
    (** [geom] (#176) is the geometry to CREATE a fresh file with — VACUUM
        passes the source's geometry so the rebuilt file keeps its page_size and
        reserved bytes.  Ignored when opening an existing file (its geometry is
        peeked from the header). *)
  ; remove_file : string -> unit (** best-effort unlink; ignore if absent *)
  ; rename_file : string -> string -> unit (** atomic rename over the target *)
  }

(** Install the process-wide file provider used by ATTACH and VACUUM. *)
val set_file_provider : file_provider -> unit

(** Close the database, flushing and releasing the underlying store. *)
val close : t -> unit Lwt.t

(** Create a lightweight worker handle sharing the same underlying store.
    Equivalent to [let* () = Lwt.return_unit in of_store (store t)].
    Used by Jepsen-style concurrent workloads where each worker needs
    its own [explicit_txn] without exposing the raw store. *)
val create_worker_handle : t -> t Lwt.t

(** Number of WAL fsyncs performed since open.  Returns 0 for non-WAL
    databases.  Exposed for #77 group-commit testing. *)
val wal_sync_count : t -> int

(** Rebuild the database file in place: copies every tree from the
    current file into a fresh sibling [path ^ ".vacuum-tmp"], then
    atomically renames it over the original.  This drops free-list
    pages and re-packs everything densely.

    Only works on file-backed databases (opened via the [sqlocaml.unix]
    driver); in-memory and arbitrary-block-device handles raise [Failure],
    as does any handle when no file provider is installed (see
    {!set_file_provider}).

    Must not be called inside an explicit transaction.  Any open
    prepared statements created from the previous file will continue
    to work logically but observe the recompacted file.  Phase 37 / #120. *)
val vacuum : t -> unit Lwt.t

(** Execute a DDL or DML statement (CREATE TABLE, INSERT, UPDATE, ...).
    Returns [Ok ()] on success, [Error e] on failure. *)
val execute : t -> string -> (unit, error) result Lwt.t

(** Like [execute], but returns the rows-affected count.  For DDL the
    count is [0]; for INSERT it is [1]; for UPDATE it is the number of
    rows whose contents were modified. *)
val execute_change_count : t -> string -> (int, error) result Lwt.t

(** Execute a query (SELECT).
    Returns [Ok stream] on success, [Error e] on failure.
    The stream is lazy — rows are produced on demand. *)
val query : t -> string -> (row Lwt_stream.t, error) result Lwt.t

(** #239: like {!query}, but also returns a per-query cost/stats record
    ([rows_examined], [rows_returned], [used_index]) for an external cost-based
    cache.  The record's counters are populated as the returned stream is
    consumed — read them once it is fully drained.  See
    {!Sqlocaml_sql.Exec.query_stats}. *)
val query_with_stats : t -> string -> (row Lwt_stream.t * query_stats, error) result Lwt.t

(** #264: stream a logical SQL dump (a {{:https://sqlite.org/cli.html#dump}.dump}-style
    export) of the database to [sink], one chunk at a time.

    The output is a self-contained SQL script that recreates the database when
    replayed statement-by-statement through {!execute}: [PRAGMA foreign_keys=OFF;],
    then for each table its [CREATE TABLE] followed by [INSERT] statements for
    its rows, then FTS virtual tables, explicit indexes, views (in dependency
    order) and triggers. Tables are emitted in creation order. Generated-column
    values are omitted (recomputed on insert); implicit PRIMARY KEY indexes the
    [CREATE TABLE] already implies are not re-emitted.

    The whole script is wrapped in [BEGIN]/[COMMIT] (after the leading [PRAGMA
    foreign_keys=OFF;], which stays outside the transaction), so a restore
    applies atomically — matching [sqlite3 .dump] (#281). This became possible
    once #269 removed the DDL-in-transaction deadlock; every statement the dump
    emits is transactional, so there is no per-statement-autocommit fallback. A
    consequence: replay the script as-is — do {e not} wrap it in your own
    [BEGIN]/[COMMIT], as the engine rejects a nested transaction.

    {b Not a point-in-time snapshot.} Each table is read in its own read
    transaction, so a concurrent commit between two tables' reads can produce a
    dump that reflects no single committed state. Dump from a quiescent database,
    or one with no concurrent writers, for a consistent result.

    {b FTS5 content is not dumped.} An FTS5 table's [CREATE VIRTUAL TABLE] is
    emitted but its rows are not (content-dumping is a planned follow-up), so a
    database whose data lives in FTS5 tables restores with those tables {e empty}
    — be aware before relying on this as a backup.

    Composite / table-level PRIMARY KEYs round-trip as plain [UNIQUE] indexes
    (this engine's internal representation): the data is preserved, but the
    restored schema reports no PRIMARY KEY and permits NULLs in those columns.

    [schema_only] omits all [INSERT]s; [data_only] omits all DDL (leaving only
    the row [INSERT]s). Both modes carry the same [BEGIN]/[COMMIT] wrapper as a
    full dump. Passing both yields an essentially empty dump. *)
val dump
  :  t
  -> ?schema_only:bool
  -> ?data_only:bool
  -> sink:(string -> unit Lwt.t)
  -> unit
  -> (unit, error) result Lwt.t

(** #264: like {!dump}, but collects the whole dump into a single string.
    Convenient for small databases; for large ones prefer {!dump} with a
    streaming [sink] to avoid materializing the entire script in memory. *)
val dump_to_string
  :  t
  -> ?schema_only:bool
  -> ?data_only:bool
  -> unit
  -> (string, error) result Lwt.t

(** Format an [error] value for human-readable output. *)
val pp_error : Format.formatter -> error -> unit

(** A compiled prepared statement.  Can be reused with different
    parameter bindings via [run] and [iter]. *)
type stmt

(** Compile [sql] into a prepared statement.  Returns [Error] if the
    SQL cannot be parsed or the schema check fails. *)
val prepare : t -> string -> (stmt, error) result Lwt.t

(** Execute a write statement (INSERT, UPDATE, DELETE) with the given
    positional parameter values.  Returns the rows-affected count. *)
val run : stmt -> params:value list -> (int, error) result Lwt.t

(** Execute a read statement (SELECT) with the given positional
    parameter values.  Returns a stream of result rows.

    Inside an explicit transaction the stream observes the transaction's own
    uncommitted writes (read-your-own-writes, #262).  The stream is a stable
    snapshot taken when iteration begins: rows written to the same table later
    in the transaction are not retroactively seen by an in-flight stream, and
    the stream remains safe to drain after a subsequent write or [COMMIT]. *)
val iter : stmt -> params:value list -> (row Lwt_stream.t, error) result Lwt.t

(** #239: like {!iter}, but also returns a per-query cost/stats record populated
    as the returned stream is consumed.  See {!Sqlocaml_sql.Exec.query_stats}. *)
val iter_with_stats
  :  stmt
  -> params:value list
  -> (row Lwt_stream.t * query_stats, error) result Lwt.t

(** Release resources held by a prepared statement.  No-op in this
    implementation, but should be called for forward compatibility. *)
val finalize : stmt -> unit Lwt.t

(** Look up the 0-based slot index for a named parameter.
    Returns [None] if the name was not found in the prepared statement. *)
val param_slot : stmt -> string -> int option

(** Build a parameter array from a named association list.
    Unnamed or unknown parameters are left as [V_null]. *)
val params_of_named : stmt -> (string * value) list -> value array
