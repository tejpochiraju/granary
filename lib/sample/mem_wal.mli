(** In-memory, byte-addressed WAL device (#403).

    [Store.open_block_wal] expects a WAL accessed by positioned {b byte} I/O at
    non-sector-aligned offsets, so the WAL cannot ride a page-addressed
    [Mirage_block] device directly. This is the shared in-memory backing that
    both the sample unikernel ([mirage/unikernel.ml]) and the host smoke test
    ([test/test_mirage_unikernel_smoke.ml]) wire into [Store.open_block_wal], so
    the one piece of arch-relevant glue is exercised from a single source.

    It is {b not} durable across process runs (the buffer is lost on exit), so a
    file-backed main DB written with this WAL must be re-created fresh each run. *)

type t

(** [create ?initial_size ()] is a fresh zero-filled WAL buffer ([initial_size]
    bytes, default 65536). The buffer grows on demand as the WAL is written. *)
val create : ?initial_size:int -> unit -> t

(** Current buffer size in bytes — pass as [~wal_size_bytes] to
    [Store.open_block_wal]. *)
val size_bytes : t -> int64

(** [read_at t ~offset buf] fills [buf] from [offset]; bytes past the written
    region read back as zero (the WAL grows lazily). *)
val read_at : t -> offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t

(** [write_at t ~offset src] writes [src] at [offset], growing the buffer to
    cover [offset + length src]. *)
val write_at : t -> offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t

(** No-op flush: an in-memory buffer has nothing to fsync. Pass as [~wal_sync].
    [Db.wal_sync_count] still counts the engine's calls to this callback. *)
val sync : unit -> (unit, string) result Lwt.t

(** Pretty-print the device (its current buffer size). *)
val pp : Format.formatter -> t -> unit
