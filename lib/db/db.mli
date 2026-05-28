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

(** Expose the underlying store so callers can create additional per-worker
    handles via {!of_store} for workloads that need concurrent explicit
    transactions. *)
val store : t -> Sqlocaml_store.Store.t

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
    parameter values.  Returns a stream of result rows. *)
val iter : stmt -> params:value list -> (row Lwt_stream.t, error) result Lwt.t

(** Release resources held by a prepared statement.  No-op in this
    implementation, but should be called for forward compatibility. *)
val finalize : stmt -> unit Lwt.t

(** Look up the 0-based slot index for a named parameter.
    Returns [None] if the name was not found in the prepared statement. *)
val param_slot : stmt -> string -> int option

(** Build a parameter array from a named association list.
    Unnamed or unknown parameters are left as [V_null]. *)
val params_of_named : stmt -> (string * value) list -> value array
