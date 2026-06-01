(** Unix-file convenience constructors for {!Sqlocaml_store.Store}.
    They build pager block-IO closures over a [Unix_file] (and a WAL sidecar)
    and hand them to the platform-agnostic [Store.open_block]/[open_block_wal],
    so the store core stays free of any [unix] dependency (#170). *)

(** Open a B+-tree store over a Unix file at [path].  Creates and pre-sizes the
    file if absent; otherwise reopens an existing sqlocaml database.

    [page_size] (default 4096) and [reserved_bytes_per_page] (default 0) set the
    page geometry when CREATING a fresh file (#95); both are fixed for the life
    of the file.  When reopening, the file's stored geometry is used.  Set
    [explicit_geometry] to [true] to reject a reopen whose stored geometry
    differs from the supplied one (otherwise the stored geometry silently wins). *)
val open_file
  :  ?page_size:int
  -> ?reserved_bytes_per_page:int
  -> ?explicit_geometry:bool
  -> path:string
  -> unit
  -> (Sqlocaml_store.Store.t, Sqlocaml_store.Store.error) result Lwt.t

(** Open a WAL-mode store using [path] for the main DB and [path ^ "-wal"] for
    the WAL.  Crash recovery on the WAL runs automatically at open.  Geometry
    arguments behave as in {!open_file} (#95). *)
val open_file_wal
  :  ?page_size:int
  -> ?reserved_bytes_per_page:int
  -> ?explicit_geometry:bool
  -> path:string
  -> unit
  -> (Sqlocaml_store.Store.t, Sqlocaml_store.Store.error) result Lwt.t

(** One-shot hot copy of [src] to a new file at [dest].
    Writes to a temporary file first, then atomically renames to [dest]
    (with a directory fsync), so [dest] is never left in a partial state.
    The destination is a self-contained DB with no WAL sidecar —
    it opens standalone via {!open_file}.

    Handles both non-WAL and WAL-mode sources (the copy resolves
    pages through the snapshot WAL overlay, so recently committed
    but un-checkpointed data is included). *)
val copy_to_file
  :  Sqlocaml_store.Store.t
  -> dest:string
  -> (unit, Sqlocaml_store.Store.error) result Lwt.t
