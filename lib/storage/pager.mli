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

val pp_error : Format.formatter -> error -> unit
val pp : Format.formatter -> t -> unit

val create
  :  read_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> write_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> sync:(unit -> (unit, string) result Lwt.t)
  -> resize:(n_pages:int64 -> (unit, string) result Lwt.t)
  -> n_pages:int64
  -> freelist:Freelist.t
  -> t

val set_write_tag : t -> int32 -> unit
val write_tag : t -> int32
val set_geom : t -> Geometry.t -> unit
val geom : t -> Geometry.t
val page_size : t -> int
val reserved_bytes : t -> int
val max_data_bytes : t -> int
val max_overflow_payload_bytes : t -> int
val max_freelist_entries_per_page : t -> int
val set_wal : t -> wal_callbacks option -> unit
val wal_mode : t -> bool

val read
  :  ?snapshot_frames:int
  -> ?pin_set:(int64, unit) Hashtbl.t
  -> t
  -> int64
  -> (Cstruct.t, error) result Lwt.t

val read_borrow
  :  ?snapshot_frames:int
  -> ?pin_set:(int64, unit) Hashtbl.t
  -> t
  -> int64
  -> (Cstruct.t -> 'a Lwt.t)
  -> ('a, error) result Lwt.t

val write : t -> int64 -> Cstruct.t -> unit
val write_owned : t -> int64 -> Cstruct.t -> unit
val alloc : t -> (int64, error) result Lwt.t
val free : t -> page_id:int64 -> freed_at_txn_id:int64 -> unit
val flush : t -> (unit, error) result Lwt.t
val flush_no_sync : t -> (unit, error) result Lwt.t
val flush_sync_main : t -> (unit, error) result Lwt.t
val flush_one_to_main : t -> page_id:int64 -> buf:Cstruct.t -> (unit, error) result Lwt.t
val n_pages : t -> int64
val freelist : t -> Freelist.t
val set_txn_id : t -> int64 -> unit
val get_txn_id : t -> int64
val set_alloc_min_safe : t -> int64 -> unit
val pinned_count : t -> int
val set_freelist : t -> Freelist.t -> unit
val set_n_pages : t -> int64 -> unit
val clear_dirty : t -> unit
val dirty_clone : t -> dirty_snapshot
val dirty_restore : t -> dirty_snapshot -> unit
val unpin_all : t -> (int64, unit) Hashtbl.t -> unit
val wal_sync : t -> (unit, error) result Lwt.t

(* #297: txn-owned page pool API *)
val set_n_pages_at_rw_begin : t -> int64 -> unit
val txn_owned_pool_get : t -> int64 list
val txn_owned_pool_set : t -> int64 list -> unit
