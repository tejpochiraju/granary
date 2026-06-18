(** Unix-file convenience constructors for {!Sqlocaml_store.Store}.
    They build pager block-IO closures over a [Unix_file] (and a WAL sidecar)
    and hand them to the platform-agnostic [Store.open_block]/[open_block_wal],
    so the store core stays free of any [unix] dependency (#170). *)

(** [file_history_sink ~path] (#266) is an append-only, fsync-on-append
    {!Sqlocaml_store.History.sink} backed by the file at [path] (the
    [<db>.aslog] sidecar).  A torn tail record is dropped on load. *)
val file_history_sink : path:string -> Sqlocaml_store.History.sink

(** Open a B+-tree store over a Unix file at [path].  Creates and pre-sizes the
    file if absent; otherwise reopens an existing sqlocaml database.

    [page_size] (default 4096) and [reserved_bytes_per_page] (default 0) set the
    page geometry when CREATING a fresh file (#95); both are fixed for the life
    of the file.  When reopening, the file's stored geometry is used.  Set
    [explicit_geometry] to [true] to reject a reopen whose stored geometry
    differs from the supplied one (otherwise the stored geometry silently wins).

    When supplied ([key], a 32-byte AES-256 key), the database is created/opened
    encrypted at rest (AES-256-GCM); absent ⇒ plaintext (opt-in default). Fixed
    at creation.

    (#266) When [as_of_history] is [true], commits are recorded to a
    [<path>.aslog] sidecar (via {!file_history_sink}) with a wall-clock
    timestamp, enabling as-of reads. Default [false]. *)
val open_file
  :  ?page_size:int
  -> ?reserved_bytes_per_page:int
  -> ?explicit_geometry:bool
  -> ?as_of_history:bool
  -> ?key:string
  -> path:string
  -> unit
  -> (Sqlocaml_store.Store.t, Sqlocaml_store.Store.error) result Lwt.t

(** Open a WAL-mode store using [path] for the main DB and [path ^ "-wal"] for
    the WAL.  Crash recovery on the WAL runs automatically at open.  Geometry
    arguments behave as in {!open_file} (#95).

    When supplied ([key], a 32-byte AES-256 key), the database is created/opened
    encrypted at rest (AES-256-GCM); absent ⇒ plaintext (opt-in default). Fixed
    at creation.

    (#266) When [as_of_history] is [true], commits are recorded to a
    [<path>.aslog] sidecar (via {!file_history_sink}) with a wall-clock
    timestamp, enabling as-of reads. Default [false]. *)
val open_file_wal
  :  ?page_size:int
  -> ?reserved_bytes_per_page:int
  -> ?explicit_geometry:bool
  -> ?as_of_history:bool
  -> ?key:string
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

(** [rotate_key_file ~src_path ~old_key ~new_key ~dest] offline-rotates the
    encryption key of the database at [src_path] (opened with [old_key]),
    writing a new self-contained file at [dest] encrypted under [new_key] (#215).
    Crash-safe via a temp file + atomic rename + directory fsync.  WAL-aware: an
    existing [src_path ^ "-wal"] sidecar is folded in.  Returns
    [Encryption_key_mismatch] for a wrong [old_key], [Not_encrypted] if the
    source is not encrypted, or a [Block_error] for I/O / bad key length.

    Note: if the post-rename directory fsync fails, this returns a [Block_error]
    even though [dest] is already the live, fully-written rotated file (the
    rename succeeded); only the directory entry's durability is in doubt. *)
val rotate_key_file
  :  src_path:string
  -> old_key:string
  -> new_key:string
  -> dest:string
  -> (unit, Sqlocaml_store.Store.error) result Lwt.t
