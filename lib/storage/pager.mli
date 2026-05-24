(** Page cache + allocator over a BLOCK backend.
    Holds dirty pages in memory until [flush] is called. *)

type t
type error = Block_error of string | Corruption of string

(** Pretty-print an {!error}. *)
val pp_error : Format.formatter -> error -> unit

(** Create a pager over an open Unix_file or Mem block device.
    [n_pages]: current total pages in the file (from header, or 0 for empty file).
    [freelist]: the current freelist state (deserialized from the file, or [Freelist.empty]). *)
val create :
  read_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  write_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  sync:(unit -> (unit, string) result Lwt.t) ->
  resize:(n_pages:int64 -> (unit, string) result Lwt.t) ->
  n_pages:int64 ->
  freelist:Freelist.t ->
  t

(** Read a page.
    [snapshot_frames] (default [None]): writer / non-WAL reader path —
    consults the dirty set first, then the WAL's latest frame, then main
    DB.  [Some n]: snapshot reader bounded to WAL frames strictly less
    than [n]; never consults the dirty set.

    [pin_set] (default [None]): when provided, every main-DB-cached page
    returned through this call is pinned for the owning RO snapshot (#159)
    — recorded in [pin_set] and refcounted internally so eviction skips it
    until the snapshot releases it via {!unpin_all}.  Pages served from a
    WAL frame are not cached and so are never pinned.  Subject to a budget
    that always leaves a reserve of evictable slots.

    The returned Cstruct.t is a fresh copy — caller may modify it freely. *)
val read :
  ?snapshot_frames:int ->
  ?pin_set:(int64, unit) Hashtbl.t ->
  t -> int64 -> (Cstruct.t, error) result Lwt.t

(** Release every page pinned into [pin_set] by a snapshot's reads.
    Decrements the shared pin refcount per page; pages reaching zero
    become evictable again.  Called from [Store.ro_end]. *)
val unpin_all : t -> (int64, unit) Hashtbl.t -> unit

(** Mark a page as dirty with new contents. Buffered until [flush].
    Does not write to BLOCK immediately. The Cstruct.t is copied internally. *)
val write : t -> int64 -> Cstruct.t -> unit

(** Allocate a new page ID. Tries freelist first (reusing pages where
    freed_at_txn_id < alloc_min_safe); extends file if none available. *)
val alloc : t -> (int64, error) result Lwt.t

(** Free a page (add to in-memory freelist with [freed_at_txn_id]). *)
val free : t -> page_id:int64 -> freed_at_txn_id:int64 -> unit

(** Flush all dirty pages to BLOCK: write each dirty page, then sync.
    Clears the dirty set after success. *)
val flush : t -> (unit, error) result Lwt.t

(** Like {!flush} but skips the trailing device sync when in WAL mode.
    Used by the group-commit coordinator to write the batch's frames
    without paying a per-writer fsync.  On non-WAL backends this is
    identical to {!flush}. *)
val flush_no_sync : t -> (unit, error) result Lwt.t

(** Invoke the WAL device sync.  No-op when no WAL hook is installed.
    Used by the group-commit coordinator after a sequence of
    {!flush_no_sync} calls. *)
val wal_sync : t -> (unit, error) result Lwt.t

(** Current total page count (updated by [alloc]). *)
val n_pages : t -> int64

(** Current freelist state (for serialisation into header). *)
val freelist : t -> Freelist.t

(** Set the current RW transaction ID. B+-tree uses get_txn_id for freed_at stamps. *)
val set_txn_id : t -> int64 -> unit

(** Get the current RW transaction ID (used by btree.ml for freed_at stamps). *)
val get_txn_id : t -> int64

(** Set the minimum txn_id threshold for freelist reuse.
    A freed page is reusable iff freed_at_txn_id < alloc_min_safe. *)
val set_alloc_min_safe : t -> int64 -> unit

(** Number of distinct pages currently pinned by live RO snapshots (#159).
    Diagnostic/testing only. *)
val pinned_count : t -> int

(** Replace the in-memory freelist (used after deserializing from disk). *)
val set_freelist : t -> Freelist.t -> unit

val set_n_pages : t -> int64 -> unit
(** Override the pager's current page count.  Used by [Store.open_block]
    after probing headers to set the authoritative logical page count
    without going through the resize callback. *)

(** Discard all dirty pages (and remove them from the read cache) without
    writing them to disk. Used on rollback to prevent aborted writes from
    being visible. *)
val clear_dirty : t -> unit

(** Optional WAL hook. When set, [read] consults [wal_find_page] before
    falling back to the read cache + main DB; [flush] appends the dirty
    set to the WAL as a single commit batch instead of writing to the
    main DB. The Pager does not depend on [Sqlocaml_storage.Wal] — the
    caller wires the callbacks in. *)
type wal_callbacks = {
  wal_find_page    : int64 -> int option;
  wal_find_page_at : int64 -> max_frame:int -> int option;
  wal_read_frame   : int -> (Cstruct.t, string) result Lwt.t;
  wal_append_commit: (int64 * Cstruct.t) list -> (unit, string) result Lwt.t;
  wal_append_commit_no_sync :
    (int64 * Cstruct.t) list -> (unit, string) result Lwt.t;
  wal_sync         : unit -> (unit, string) result Lwt.t;
}

(** Install (or clear) the WAL hook. Pass [None] to revert to direct
    main-DB writes. *)
val set_wal : t -> wal_callbacks option -> unit

(** True iff a WAL hook is currently installed. *)
val wal_mode : t -> bool

(** Write a single page directly to the main-DB callback, bypassing the
    WAL hook. Used by checkpointing. Does not sync. *)
val flush_one_to_main :
  t -> page_id:int64 -> buf:Cstruct.t -> (unit, error) result Lwt.t

(** Sync the main-DB callback. Used at the end of checkpoint. *)
val flush_sync_main : t -> (unit, error) result Lwt.t

(** Opaque snapshot of the dirty page set, captured at a given moment.
    Used by Store.savepoint to roll back to an intermediate state without
    aborting the entire transaction. *)
type dirty_snapshot

(** Clone the current dirty set. The Cstruct buffers themselves are not
    deep-copied — they are only ever replaced (not mutated in-place) by
    [write], so sharing references is safe. *)
val dirty_clone : t -> dirty_snapshot

(** Restore the dirty set to a snapshot. Any pages now-dirty but not in
    the snapshot are removed; pages in the snapshot are reinstated with
    their snapshotted content. The read cache is left as-is. *)
val dirty_restore : t -> dirty_snapshot -> unit
