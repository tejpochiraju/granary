(** Ordered byte-keyed store.
    Phase 0: in-memory [BytesMap] per [tree_id].
    Phase 1: CoW B+-tree on BLOCK behind the same interface.

    Two backends are available:
    - [create ()] — in-memory [BytesMap] (no size limits on keys/values).
      This is the legacy Phase 0 backend, retained for tests and
      ephemeral use cases that exceed B+-tree leaf-cell size limits.
    - [open_file ~path] — CoW B+-tree over a Unix_file BLOCK device.
      Persistent across reopen. Keys ≤ 512 bytes, values ≤ 1024 bytes. *)

type t

(** Phantom types for transaction modes. *)
type ro
type rw

(** A transaction handle, phantom-typed by mode. *)
type 'a txn

(** Each tree is an independent ordered key→value map.
    System trees use IDs 0–15; user tables use 16+. *)
type tree_id = int

(** Errors from the persistent (B+-tree) backend.  The in-memory backend
    never returns errors. *)
type error =
  | Block_error of string
  | Corruption of string
  | Key_too_large of int
  | Value_too_large of int
  | Header_error of string

val pp_error : Format.formatter -> error -> unit

(** Open a fresh in-memory store with no trees. *)
val create : unit -> t

(** Open a B+-tree backed store over a Unix file.  Creates the file if
    absent.  If the file is non-empty it must be a valid sqlocaml database
    written by a previous [open_file]/[commit] sequence. *)
val open_file : path:string -> (t, error) result Lwt.t

(** Open a B+-tree backed store from any block device, given as I/O callbacks.
    Probes pages 0 and 1 for valid headers; if both are corrupt, treats the
    device as fresh and initialises it.  Pass [~n_pages:0L] for Mirage adapters
    (which bound-check internally against device capacity).
    [~close] is called by [Store.close]. *)
val open_block :
  read_page  : (page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  write_page : (page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  sync       : (unit -> (unit, string) result Lwt.t) ->
  resize     : (n_pages:int64 -> (unit, string) result Lwt.t) ->
  n_pages    : int64 ->
  close      : (unit -> unit Lwt.t) ->
  (t, error) result Lwt.t

(** Open a B+-tree backed store in WAL mode. Commits append dirty pages
    to the WAL device; reads route through the WAL first and fall back
    to the main DB. Crash recovery is performed automatically when the
    WAL is opened. *)
val open_block_wal :
  read_page  : (page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  write_page : (page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  sync       : (unit -> (unit, string) result Lwt.t) ->
  resize     : (n_pages:int64 -> (unit, string) result Lwt.t) ->
  n_pages    : int64 ->
  wal_read_at  : (offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  wal_write_at : (offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  wal_sync     : (unit -> (unit, string) result Lwt.t) ->
  wal_size_bytes : int64 ->
  close      : (unit -> unit Lwt.t) ->
  wal_close  : (unit -> unit Lwt.t) ->
  (t, error) result Lwt.t

(** Convenience wrapper: open WAL-mode store using two Unix files,
    [path] for the main DB and [path ^ "-wal"] for the WAL. *)
val open_file_wal : path:string -> (t, error) result Lwt.t

(** Close the store. After this, any use of the store or its txns is
    undefined. *)
val close : t -> unit Lwt.t

(** Begin a read-only transaction. Multiple RO txns may run concurrently. *)
val ro_begin : t -> ro txn Lwt.t

(** Begin a read-write transaction. Only one RW txn may be active at a
    time; this call blocks until the previous one commits or rolls back. *)
val rw_begin : t -> rw txn Lwt.t

(** Commit a read-write transaction, making its mutations durable. *)
val commit   : rw txn -> unit Lwt.t

(** Roll back a read-write transaction. Phase 0: mutations cannot be
    rolled back (they were applied immediately); this just releases the
    writer lock. Phase 3 introduces true rollback. *)
val rollback : rw txn -> unit Lwt.t

(** Push a named savepoint by snapshotting current Mem tree state.
    No-op on the B-tree backend (deferred). *)
val savepoint_begin   : rw txn -> string -> unit Lwt.t

(** Release the named savepoint and all newer ones.
    Writes accumulated since the savepoint remain in the outer transaction.
    No-op on the B-tree backend. *)
val savepoint_release : rw txn -> string -> unit Lwt.t

(** Restore to the named savepoint, dropping all newer savepoints.
    The named savepoint is kept so ROLLBACK TO can be repeated.
    No-op on the B-tree backend. *)
val savepoint_rollback : rw txn -> string -> unit Lwt.t

(** End a read-only transaction. *)
val ro_end   : ro txn -> unit Lwt.t

(** Look up a key in a tree. Works in both RO and RW transactions. *)
val get : _ txn -> tree_id -> bytes -> bytes option Lwt.t

(** Insert or update a key in a tree. Only available in RW transactions. *)
val put : rw txn -> tree_id -> bytes -> bytes -> unit Lwt.t

(** Delete a key from a tree. No-op if the key does not exist. Only
    available in RW transactions. *)
val del : rw txn -> tree_id -> bytes -> unit Lwt.t

(** A cursor for iterating over an ordered tree snapshot. *)
type cursor

(** Open a cursor over a tree. The cursor sees a snapshot of the tree as
    of the moment it was opened. Works in both RO and RW transactions. *)
val cursor_open : _ txn -> tree_id -> cursor Lwt.t

(** Close a cursor, releasing its resources. *)
val cursor_close : cursor -> unit

(** Result of a seek or first operation. *)
type seek_result =
  | Found of bytes
    (** Cursor is positioned at the exact key. *)
  | Not_found of [`Greater of bytes | `End]
    (** No exact match. [`Greater k] means cursor is at the next key [k].
        [`End] means there is no key >= the sought key. *)

(** Seek to the smallest key >= the given key.
    Returns [Found k] if [k] matches exactly, or [Not_found] otherwise.
    After this call, [cursor_next] returns the entry at or after the
    sought position. *)
val cursor_seek : cursor -> bytes -> seek_result

(** Position the cursor at the first key in the tree.
    Returns [Found k] if the tree is non-empty, [Not_found `End] if empty.
    After this call, [cursor_next] returns the first entry. *)
val cursor_first : cursor -> seek_result

(** Advance the cursor and return the current (key, value) pair, or [None]
    if the cursor is exhausted.
    On the first call after [cursor_open], [cursor_first], or
    [cursor_seek], returns the positioned entry (not the next one).
    On subsequent calls, advances and returns the next entry. *)
val cursor_next : cursor -> (bytes * bytes) option

(** Return the value at the current cursor position without advancing,
    or [None] if the cursor is not positioned (exhausted or before first). *)
val cursor_value : cursor -> bytes option

(** True if the store is operating in WAL mode (opened via
    [open_block_wal] / [open_file_wal]). *)
val wal_mode : t -> bool

(** Migrate every page in the WAL index to the main DB, sync, then
    reset the WAL. No-op outside WAL mode. Acquires the RW mutex
    internally so it serialises with commits. *)
val checkpoint : t -> unit Lwt.t

(** Number of entries in the in-memory freelist (diagnostics / testing). *)
val freelist_size : t -> int

(** Raw freelist entries for testing — (page_id, freed_at_txn_id) pairs. *)
val freelist_entries : t -> (int32 * int64) list

(** Current total file page count (diagnostics / testing). *)
val n_pages : t -> int64

(** Enumerate every [tree_id] currently registered in the meta tree.
    The list is unordered.  On the in-memory backend, returns the keys
    of the per-tree hashtable.  Used by VACUUM (phase 37). *)
val list_tree_ids : t -> tree_id list Lwt.t
