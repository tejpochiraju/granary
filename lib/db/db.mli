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
  | History_unavailable
  (** #266: an as-of read was requested on a database opened without
          [~as_of_history:true]. *)
  | History_pruned
  (** #266: the as-of target predates the retained history floor (see
          {!history_pin}), so the snapshot is no longer available. *)

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

    [clock] and [durability] are open-options forwarded to [of_store]:
    [durability] sets the database-wide durability knob (full/batched/off)
    before the catalog is loaded.

    This is the platform-agnostic entry point.  Unix file convenience
    constructors ([open_file] / [open_file_wal]) live in the [sqlocaml.unix]
    driver library so this core carries no [unix] dependency (#170). *)
val open_block
  :  ?geom:Sqlocaml_storage.Geometry.t
  -> ?clock:(unit -> float)
  -> ?durability:Sqlocaml_store.Store.durability
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
    omit it for in-memory / arbitrary block devices.  [durability] sets the
    database-wide durability knob (full/batched/off) on the store before the
    catalog is loaded — use this to configure the store's fsync policy at
    open time without needing a raw store handle afterwards.  Used by the
    [sqlocaml.unix] driver and by ATTACH. *)
val of_store
  :  ?clock:(unit -> float)
  -> ?durability:Sqlocaml_store.Store.durability
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

(** Re-export of the internal-events type (#382). *)
module Event = Sqlocaml_store.Store.Event

(** Register/clear the internals-monitor observer on this database's store.
    No-op for in-memory databases.  The callback should be cheap and
    non-blocking (see {!Sqlocaml_store.Store.set_event_callback}). *)
val set_event_callback : t -> (Event.t -> unit) option -> unit

(** [tree_of_table t name] is the storage tree id backing table [name] in the
    active schema, or [None] if no such table exists or the table has no real
    on-disk tree (e.g. an ephemeral/CTE table, or a legacy zero-tid columnar
    table, which carry a negative sentinel tree id). Used by the internals
    monitor to filter page events by table (#385). *)
val tree_of_table : t -> string -> int option

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

(** [query_as_of t target sql] (#266) runs a read-only [sql] query against the
    database as it existed at [target] (a past txn id or timestamp).  Requires
    the db to have been opened with [~as_of_history:true]; otherwise the result
    carries [History_unavailable].  [History_pruned] when [target] predates the
    retained floor.  Schema is read at HEAD: a query whose table had DDL after
    [target] may misinterpret older rows (schema-as-of is out of scope, #266). *)
val query_as_of
  :  t
  -> Sqlocaml_store.History.target
  -> string
  -> (row Lwt_stream.t, error) result Lwt.t

(** [history_pin t ~txn_id] (#266) sets the retention floor at [txn_id]: the
    committed root at or before [txn_id] is retained so {!query_as_of} can reach
    it.  No-op when as-of history is not enabled. *)
val history_pin : t -> txn_id:int64 -> unit

(** [history_floor t] (#266) is the current retention floor txn id, or [None]
    when no floor is pinned (or as-of history is not enabled). *)
val history_floor : t -> int64 option

(** [history_release t] (#266) clears the retention floor, allowing pruning of
    previously pinned historical roots. *)
val history_release : t -> unit

(** [history_log t] (#266) is the recorded commit history (the [<path>.aslog]
    sidecar), oldest first.  Empty when as-of history is not enabled. *)
val history_log : t -> Sqlocaml_store.History.record list Lwt.t

(** #240: the set of user tables whose {e rows} a write statement actually
    mutated, including tables touched indirectly by triggers and FK cascades.
    Sorted and deduplicated; internal/system tables are excluded.  Enables an
    external read cache to invalidate the tables whose row data changed.

    {b Scope: row-level DML only.}  This signal reports {!Db.execute}-style row
    mutations (INSERT / UPDATE / DELETE, upserts, cascades, trigger bodies,
    columnar and FTS writes).  It does {b not} report schema-changing or
    destructive DDL — [DROP TABLE], and [ALTER TABLE] in all its forms
    ([ADD]/[DROP]/[RENAME COLUMN], [RENAME TABLE]) — {e even when the DDL
    physically rewrites every row} (e.g. [ALTER TABLE … DROP COLUMN], which
    reshapes the stored rows).  Such a statement yields an {b empty} list here.
    A consumer that caches query results MUST invalidate on schema changes
    through a separate schema-version / fingerprint signal (cf. #174); an empty
    result from a DDL statement therefore does {b not} imply the table's
    observable contents are unchanged. *)
type dirty_tables = string list

(** Like {!execute}, but also returns the {!dirty_tables} the statement mutated.
    The list is empty for a no-op write (e.g. [INSERT OR IGNORE] that inserts
    nothing) and for {e all} DDL (including row-rewriting DDL such as [ALTER
    TABLE … DROP COLUMN] and [DROP TABLE]) — see the {!dirty_tables} scope note;
    DDL invalidation is out of band. *)
val execute_with_dirty : t -> string -> (dirty_tables, error) result Lwt.t

(** Like {!execute_change_count}, but also returns the {!dirty_tables} the
    statement mutated alongside the rows-affected count. *)
val execute_change_count_with_dirty
  :  t
  -> string
  -> (int * dirty_tables, error) result Lwt.t

(** #387: the projected output column names for a row-returning [sql], without
    executing it.  Parses and binds [sql] against the current schema and returns
    a best-effort name per result column: a SELECT alias or bare column name
    where known, [*] expanded to the source table's columns, and a positional
    [col_N] placeholder for anonymous expressions.  Intended for header display
    (e.g. the REPL); the list length matches the row width [query] produces for
    the same statement. *)
val query_columns : t -> string -> (string list, error) result Lwt.t

(** #264: stream a logical SQL dump (a {{:https://sqlite.org/cli.html#dump}.dump}-style
    export) of the database to [sink], one chunk at a time.

    The output is a self-contained SQL script that recreates the database when
    replayed statement-by-statement through {!execute}: [PRAGMA foreign_keys=OFF;],
    then for each table its [CREATE TABLE] followed by [INSERT] statements for
    its rows, then each FTS5 virtual table's [CREATE VIRTUAL TABLE] followed by
    [INSERT]s for its content, then explicit indexes, views (in dependency order)
    and triggers. Tables are emitted in creation order. Generated-column values
    are omitted (recomputed on insert); implicit PRIMARY KEY indexes the
    [CREATE TABLE] already implies are not re-emitted.

    The whole script is wrapped in [BEGIN]/[COMMIT] (after the leading [PRAGMA
    foreign_keys=OFF;], which stays outside the transaction), so a restore
    applies atomically — matching [sqlite3 .dump] (#281). This became possible
    once #269 removed the DDL-in-transaction deadlock; every statement the dump
    emits is transactional, so there is no per-statement-autocommit fallback. A
    consequence: replay the script as-is — do {e not} wrap it in your own
    [BEGIN]/[COMMIT], as the engine rejects a nested transaction.

    {b Point-in-time row data (#274).} Every table's row data is read through
    one read-only snapshot of the committed state, taken when the dump begins
    (when an explicit transaction is active, its view is used instead), so the
    emitted rows reflect a single consistent moment even if other writers commit
    concurrently while the dump runs — matching [sqlite3 .dump], which previously
    differed by reading each table in its own snapshot (a torn dump). View and
    trigger DDL is read through that same view too (#322/#323/#329): the shared
    snapshot in autocommit, or the explicit transaction when one is active — so
    the schema section is always consistent with the row data (point-in-time
    against a concurrent [CREATE]/[DROP VIEW|TRIGGER] commit, and read-your-own
    -writes inside a transaction). Table/index DDL and the AUTOINCREMENT high-water
    still come from the in-memory catalog (a separate consistency surface), so for
    a fully consistent result those should be quiescent during the dump.

    {b FTS5 content is dumped (#319/#330).} An FTS5 table's [CREATE VIRTUAL TABLE]
    is emitted followed by [INSERT INTO t(rowid, ..)] statements for its stored
    content, so a replay re-inserts the original rows (rebuilding the index) with
    their {e original rowids} — a delete leaves a rowid gap that the restore
    preserves rather than re-packing, matching how [sqlite3 .dump] round-trips an
    FTS table.

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

(** Like {!run}, but also returns the {!dirty_tables} the prepared write
    mutated alongside the rows-affected count. *)
val run_with_dirty : stmt -> params:value list -> (int * dirty_tables, error) result Lwt.t

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
