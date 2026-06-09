(** Pager: page cache + allocator over a BLOCK backend. *)

type cache_key = int64 * int

type wal_callbacks =
  { wal_find_page : int64 -> int option
  ; wal_find_page_at : int64 -> max_frame:int -> int option
  ; wal_read_frame : int -> (Cstruct.t, string) result Lwt.t
  ; wal_append_commit : (int64 * Cstruct.t) list -> (unit, string) result Lwt.t
  ; wal_append_commit_no_sync : (int64 * Cstruct.t) list -> (unit, string) result Lwt.t
  ; wal_sync : unit -> (unit, string) result Lwt.t
  }

type t

type error =
  | Block_error of string
  | Corruption of string

type dirty_snapshot = (int64, Cstruct.t) Hashtbl.t

(** Pretty-print a pager error. *)
val pp_error : Format.formatter -> error -> unit

(** Pretty-print the pager state (n_pages, cache/dirty sizes, txn_id). *)
val pp : Format.formatter -> t -> unit

(** Create a pager over BLOCK callbacks with the given initial page count and freelist. *)
val create
  :  read_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> write_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> sync:(unit -> (unit, string) result Lwt.t)
  -> resize:(n_pages:int64 -> (unit, string) result Lwt.t)
  -> n_pages:int64
  -> freelist:Freelist.t
  -> t

(** Set the schema-fingerprint stamp for subsequently-built Btree pages (#174). *)
val set_write_tag : t -> int32 -> unit

(** Current schema-fingerprint stamp (#174). *)
val write_tag : t -> int32

(** Set the file's page geometry (called once on open, before any page op). *)
val set_geom : t -> Geometry.t -> unit

(** Current page geometry. *)
val geom : t -> Geometry.t

(** Page size in bytes. *)
val page_size : t -> int

(** Reserved bytes per page (#174 schema fingerprint). *)
val reserved_bytes : t -> int

(** Maximum bytes of user data per page. *)
val max_data_bytes : t -> int

(** Maximum payload bytes for an overflow page. *)
val max_overflow_payload_bytes : t -> int

(** Maximum freelist entries that fit on one page. *)
val max_freelist_entries_per_page : t -> int

(** Attach or detach the WAL overlay.  Clears cache on transition into WAL mode. *)
val set_wal : t -> wal_callbacks option -> unit

(** True when a WAL overlay is attached. *)
val wal_mode : t -> bool

(** Read [page_id], consulting dirty (writer path) or the WAL snapshot.
    When [~bypass_cache:true], the page is read from the block device but NOT
    inserted into the shared FIFO cache.  Intended for single-use pages such as
    BLOB overflow chains where caching would evict hot B-tree pages without
    benefit.  On a cache hit the cached buffer is still returned even when
    [bypass_cache] is set; only insertion-on-miss is suppressed. *)
val read
  :  ?snapshot_frames:int
  -> ?pin_set:(int64, unit) Hashtbl.t
  -> ?bypass_cache:bool
  -> t
  -> int64
  -> (Cstruct.t, error) result Lwt.t

(** Zero-copy borrow-read: the callback receives the page buffer directly.
    When [~bypass_cache:true], the page is read from the block device but NOT
    inserted into the shared FIFO cache.  See {!val:read} for details. *)
val read_borrow
  :  ?snapshot_frames:int
  -> ?pin_set:(int64, unit) Hashtbl.t
  -> ?bypass_cache:bool
  -> t
  -> int64
  -> (Cstruct.t -> 'a Lwt.t)
  -> ('a, error) result Lwt.t

(** Write [buf] to [page_id] (defensive copy). *)
val write : t -> int64 -> Cstruct.t -> unit

(** Write [buf] to [page_id] (takes ownership, no defensive copy). *)
val write_owned : t -> int64 -> Cstruct.t -> unit

(** Allocate a page: txn-owned pool, then main freelist, then file extension. *)
val alloc : t -> (int64, error) result Lwt.t

(** Free [page_id] stamped with [freed_at_txn_id]; txn-owned pages route to the pool. *)
val free : t -> page_id:int64 -> freed_at_txn_id:int64 -> unit

(** Flush all dirty pages to the WAL with an fsync. *)
val flush : t -> (unit, error) result Lwt.t

(** Flush all dirty pages to the WAL without an fsync. *)
val flush_no_sync : t -> (unit, error) result Lwt.t

(** Flush dirty pages directly to the main BLOCK device with an fsync. *)
val flush_sync_main : t -> (unit, error) result Lwt.t

(** Flush a single page to the main BLOCK device. *)
val flush_one_to_main : t -> page_id:int64 -> buf:Cstruct.t -> (unit, error) result Lwt.t

(** Current page count (may grow via [alloc], shrink via [set_n_pages]). *)
val n_pages : t -> int64

(** Current freelist state. *)
val freelist : t -> Freelist.t

(** Set the current RW transaction ID. *)
val set_txn_id : t -> int64 -> unit

(** Current RW transaction ID (0 when no RW txn is active). *)
val get_txn_id : t -> int64

(** Set the minimum safe txn ID for freelist reuse (gated by active readers). *)
val set_alloc_min_safe : t -> int64 -> unit

(** Number of distinct pages pinned by live RO snapshots (#159). *)
val pinned_count : t -> int

(** Replace the entire freelist (used by rollback and checkpoint). *)
val set_freelist : t -> Freelist.t -> unit

(** Override n_pages (used by savepoint rollback to truncate the file). *)
val set_n_pages : t -> int64 -> unit

(** Discard all dirty pages and rollback state (#297 pool included). *)
val clear_dirty : t -> unit

(** Snapshot the dirty set (for savepoints). *)
val dirty_clone : t -> dirty_snapshot

(** Restore a dirty set snapshot (for savepoints). *)
val dirty_restore : t -> dirty_snapshot -> unit

(** Release every pin held by an RO snapshot (#159). *)
val unpin_all : t -> (int64, unit) Hashtbl.t -> unit

(** Forward-sync the WAL (fsync the WAL file). *)
val wal_sync : t -> (unit, error) result Lwt.t

(* #297: txn-owned page pool API *)

(** Set the page count threshold at rw_begin; pages >= this are txn-owned. *)
val set_n_pages_at_rw_begin : t -> int64 -> unit

(** Current txn-owned page pool (freed pages available for reuse). *)
val txn_owned_pool_get : t -> int64 list

(** Replace the txn-owned page pool (used by commit and rollback). *)
val txn_owned_pool_set : t -> int64 list -> unit
