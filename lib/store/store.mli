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

(** Number of entries in the in-memory freelist (diagnostics / testing). *)
val freelist_size : t -> int

(** Raw freelist entries for testing — (page_id, freed_at_txn_id) pairs. *)
val freelist_entries : t -> (int32 * int64) list

(** Current total file page count (diagnostics / testing). *)
val n_pages : t -> int64
