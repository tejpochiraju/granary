(** Copy-on-write B+-tree over the {!Pager} layer.

    Every modification (put/del) allocates fresh pages, writes the new state
    into them via {!Pager.write}, and frees the old pages via {!Pager.free}.
    Returns a new {!t} whose [root_page] reflects the latest root.

    Keys are sorted by [Bytes.compare].  Max key size 512 bytes, max value
    size 1024 bytes. *)

type t

type error =
  | Pager_error of Pager.error
  | Key_too_large of int
  | Value_too_large of int
  | Tree_corrupt of string

(** Pretty-print the tree's root page and snapshot frame bound. *)
val pp : Format.formatter -> t -> unit

(** Pretty-print an {!error}. *)
val pp_error : Format.formatter -> error -> unit

(** Create a tree view.  [root_page=0L] means an empty tree (no root yet).
    [snapshot_frames]: when [Some n], reads resolve against WAL frames < n
    (RO-snapshot semantics); when [None] (default), reads consult the writer's
    dirty set and latest WAL.
    [pin_set]: when provided (RO snapshots only), reads through this view pin
    the pages they materialise into the given set (#159) so concurrent writer
    CoW churn can't evict the reader's working set; released via
    [Pager.unpin_all]. *)
val create
  :  ?snapshot_frames:int
  -> ?pin_set:(int64, unit) Hashtbl.t
  -> Pager.t
  -> root_page:int64
  -> t

(** The current root page id.  Changes after each mutation.
    [0L] means an empty tree. *)
val root_page : t -> int64

(** Point lookup. *)
val get : t -> bytes -> (bytes option, error) result Lwt.t

(** Insert or replace.  Returns an updated [t] whose [root_page] reflects the
    new root.  Mutations stamp freed pages via {!Pager.get_txn_id}; the pager
    manages the transaction ID internally — the caller is responsible only for
    calling [Store.rw_begin] which sets the correct txn_id.

    @return [Error (Key_too_large n)] if the key is more than 512 bytes.
    @return [Error (Value_too_large n)] if the value is more than 1024 bytes. *)
val put : t -> bytes -> bytes -> (t, error) result Lwt.t

(** [put_x t key value] inserts [value] at [key] in a single tree descent.
    Returns [(t', None)] when [key] was absent (value written).
    Returns [(t, Some Bytes.empty)] when [key] already existed (NOT overwritten).
    The returned bytes on conflict are a sentinel — callers that need the old
    value must fetch it separately via {!get}. *)
val put_x : t -> bytes -> bytes -> (t * bytes option, error) result Lwt.t

(** Opaque cached position at a tree's rightmost leaf, enabling O(1) appends
    for a run of strictly-increasing inserts (#356).  Held by the Store per
    tree-id; re-validated against the live page on every use. *)
type append_cursor

(** The cursor's current maximum key (the tree's rightmost key). *)
val append_cursor_max_key : append_cursor -> bytes

(** Result of {!try_inplace_append}. *)
type append_outcome =
  | Appended of append_cursor (** inserted in place; carries the updated cursor *)
  | Not_applicable (** cursor stale or leaf full — caller must use {!put_x} *)
  | Append_failed of error

(** [try_inplace_append t ac ~key ~value] appends ([key], [value]) to the
    rightmost leaf cached in [ac] in O(1), if [ac] still matches the live page
    and the entry fits.  [key] must exceed every key in the tree (the caller
    guarantees this; it is re-validated).  Returns [Not_applicable] — never an
    error — when the cursor is stale or the leaf is full, so the caller falls
    back to {!put_x}. *)
val try_inplace_append
  :  t
  -> append_cursor
  -> key:bytes
  -> value:bytes
  -> append_outcome Lwt.t

(** Descend the rightmost spine and build a fresh {!append_cursor} for the
    tree's current rightmost leaf, or [None] for an empty tree.  Used to
    (re)prime the cursor after a general-path append or a split. *)
val rightmost_append_cursor : t -> (append_cursor option, error) result Lwt.t

(** Delete a key.  No-op if absent.  Returns an updated [t].

    Phase 1 deletion is lazy: an emptied leaf is left in the tree (callers
    won't see it as [get] returns [None] and the cursor skips it).  The only
    structural change is that if the root itself becomes empty, [root_page]
    is reset to [0L]. *)
val del : t -> bytes -> (t, error) result Lwt.t

type cursor

(** Open a forward cursor over the tree.  The cursor is positioned at the
    first entry, so the very first {!cursor_next} returns the first entry. *)
val cursor_open : t -> (cursor, error) result Lwt.t

(** Advance the cursor to the first entry [>=] the given key.

    @return [`Found] if an exact match exists.
    @return [`Not_found_after k] if the cursor stopped at a key greater than
            [k] (or past the end of the tree). *)
val cursor_seek
  :  cursor
  -> bytes
  -> ([ `Found | `Not_found_after of bytes ], error) result Lwt.t

(** Return the entry at the current cursor position and advance.
    Returns [None] when the cursor has passed the last entry. *)
val cursor_next : cursor -> ((bytes * bytes) option, error) result Lwt.t

(** Cursor is purely in-memory state — close is a no-op for now but kept
    for API symmetry / future resource management. *)
val cursor_close : cursor -> unit
