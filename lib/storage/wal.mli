(** Write-Ahead Log over byte-addressable storage.

    Frames are 4096-byte pages tagged with metadata; the WAL is a sequence
    of frames followed by an optional uncommitted tail. Each batch ends
    with a frame that has the [commit] bit set; recovery on open scans
    forward and accepts only frames whose commit batch is complete.

    Layout
    ------
    The WAL begins with a 24-byte header containing a magic identifier,
    a 64-bit salt assigned at open time when the WAL is empty, and a
    64-bit seed used by the per-frame checksum. Frame [i] occupies
    [header_size + i * frame_size_bytes] bytes.

    Per-frame layout:
        offset  0   page_id : uint64 big-endian
        offset  8   flags   : uint64 big-endian  (bit 0 = commit marker)
        offset 16   checksum: uint64 big-endian  (FNV-1a-64 of preceding
                                                  fields || salt || seed
                                                  || page bytes)
        offset 24   page    : 4096 bytes

    Crash-safety guarantee
    ----------------------
    A frame is considered durable only if (a) its checksum verifies and
    (b) some later frame in the same forward scan has the commit bit set
    without an intervening checksum failure. Partial trailing batches are
    silently discarded on recovery and overwritten by the next append.

    This module is purely an append-only frame store; integration with the
    pager (read-from-WAL routing, checkpointing back to the main DB) lives
    in the [Sqlocaml_store.Store] module. *)

type t

(** Pretty-print committed-frame count and current WAL size in bytes. *)
val pp : Format.formatter -> t -> unit

type frame =
  { frame_idx : int (** 0-based index in WAL *)
  ; page_id : int64
  ; is_commit : bool
  ; page : Cstruct.t (** 4096 bytes *)
  }

(** Size of one WAL frame for the DEFAULT 4096-byte geometry (24-byte frame
    header + 4096-byte page = 4120).  A WAL opened with a non-default
    [page_size] uses a proportionally larger frame internally (#95). *)
val frame_size_bytes : int

(** Size of a WAL frame header in bytes (24). *)
val header_size_bytes : int

type error =
  | Block_error of string
  | Corrupt_frame of int
  (** frame_idx of a structurally-present frame that failed integrity: a bad
      checksum on a directly-addressed read, or (on an encrypted WAL) a frame
      whose checksum passed but whose GCM tag failed — i.e. tampering (#219) *)

val pp_error : Format.formatter -> error -> unit

(** Open a WAL over the given byte-addressable callbacks. If the device is
    empty (or smaller than [header_size_bytes]) the WAL is initialised
    with a fresh salt and seed. Otherwise the header is read, and a
    forward scan recovers the index of every page in the last contiguous
    committed batch. A trailing partial batch is silently discarded.

    [page_size] (default {!Geometry.default}'s 4096) sets the frame's page
    payload size; it must match the main DB geometry (#95).

    [cipher] (default [None]): when supplied, the page payload of each frame
    is AES-256-GCM encrypted; the 24-byte meta header stays plaintext so
    keyless recovery forward-scans still work. The FNV checksum covers the
    whole payload region (ciphertext + nonce + tag). Encrypted frames are
    [Crypto.overhead] (32) bytes larger than plaintext frames. *)
val open_
  :  ?cipher:Crypto.t option
  -> ?page_size:int
  -> read_at:(offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> write_at:(offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> sync:(unit -> (unit, string) result Lwt.t)
  -> size_bytes:int64
  -> unit
  -> (t, error) result Lwt.t

(** Total committed frames currently in the WAL. *)
val committed_frames : t -> int

(** Number of successful device syncs since this WAL was opened.
    Exposed for #77 group-commit testing: tracks how many fsyncs the
    coordinator has issued. *)
val sync_count : t -> int

(** Most recent committed frame index for [page_id], or [None] if absent. *)
val find_page : t -> int64 -> int option

(** Snapshot-aware lookup: most recent frame index for [page_id] strictly
    less than [max_frame]. Used by RO snapshots so a reader captured at
    [committed_frames = max_frame] does not see frames written after.
    Returns [None] if no frame for [page_id] satisfies the bound. *)
val find_page_at : t -> int64 -> max_frame:int -> int option

(** Read the page bytes at a given frame index. The caller must not modify
    the returned Cstruct; it is a fresh allocation per call. *)
val read_frame : t -> int -> (Cstruct.t, error) result Lwt.t

(** Append a batch of pages; the LAST entry in the list is automatically
    marked as the commit frame. Performs a single [sync] at the end and
    only then updates the in-memory index. If [sync] fails the index is
    left unchanged so the partial batch is invisible to readers. *)
val append_commit : t -> (int64 * Cstruct.t) list -> (unit, error) result Lwt.t

(** Append a batch of pages but skip the trailing [sync].  The in-memory
    index and [committed_frames] are bumped immediately so concurrent
    writers (still under the store's [rw_mutex]) can locate the newly
    written frames and subsequent appends place their frames at the
    correct base.  Durability is deferred to a separate {!flush_sync}
    call by the group-commit coordinator.  Sync failure is treated as
    fatal by upstream callers — see [Sqlocaml_store.Store.commit]. *)
val append_commit_no_sync : t -> (int64 * Cstruct.t) list -> (unit, error) result Lwt.t

(** Invoke the underlying device sync.  Used by the group-commit
    coordinator after one or more {!append_commit_no_sync} calls. *)
val flush_sync : t -> (unit, error) result Lwt.t

(** Reset the WAL: discards all committed frames and the in-memory index.
    Used by checkpointing to truncate the log after migrating its
    contents to the main DB. The on-disk WAL is not physically truncated;
    later appends overwrite from the beginning.  Bumps {!epoch} so that
    replication consumers can detect that their snapshot has been
    invalidated. *)
val reset : t -> unit

(** WAL generation counter — bumped every time [reset] is called (i.e. after
    each checkpoint).  Starts at 0 on open.  Replication can use this to
    detect whether a checkpoint expired its snapshot. *)
val epoch : t -> int64

(** Iterate over every (page_id, frame_idx) currently in the WAL index.
    Order is unspecified. *)
val iter_index : t -> (int64 -> int -> unit) -> unit

(** Compute the FNV-1a-64 checksum for a single WAL frame, given the source
    salt and seed.  Used by replication to verify transport integrity. *)
val frame_checksum
  :  salt:int64
  -> seed:int64
  -> page_id:int64
  -> flags:int64
  -> page:Cstruct.t
  -> int64

(** Return the WAL's salt (assigned at open). *)
val salt : t -> int64

(** Return the WAL's seed (assigned at open). *)
val seed : t -> int64
