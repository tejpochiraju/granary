(** Append-only commit-log records for whole-DB as-of time travel (#266).

    Each committed root is recorded as a fixed-width 28-byte record:
    [txn_id(8) ++ timestamp(8) ++ root_page(8) ++ crc32(4)], big-endian,
    with the CRC32 (IEEE polynomial) computed over the first 24 bytes.  A
    torn or corrupt tail record is dropped on load — the database header
    remains the source of truth for the live head. *)

(** One committed whole-DB snapshot. *)
type record =
  { txn_id : int64 (** monotonic commit id *)
  ; timestamp : int64 (** wall-clock ms since epoch at commit *)
  ; root_page : int64 (** meta-tree root committed at [txn_id] *)
  }

(** A target to resolve against the log. *)
type target =
  [ `Txn of int64
  | `Ts of int64
  ]

(** An injected append-only log backend (file, block device, or in-memory). *)
type sink =
  { append : record -> unit Lwt.t (** append one record; best-effort *)
  ; load : unit -> record list Lwt.t (** all records in ascending txn order *)
  }

(** Fixed serialized size of one record, in bytes. *)
val record_size : int

(** Serialize a record to a fresh {!record_size}-byte buffer. *)
val encode : record -> Cstruct.t

(** Decode a single {!record_size}-byte record.  [None] if the buffer is too
    short or the CRC does not match. *)
val decode : Cstruct.t -> record option

(** Parse a buffer of concatenated records.  Stops at the first short or
    CRC-failing record (a torn tail), returning every valid record before it. *)
val decode_all : Cstruct.t -> record list

(** [resolve records target] returns the record with the largest [txn_id]
    (for [`Txn]) or [timestamp] (for [`Ts]) that is [<=] the target, or
    [None] if every record is newer (or the list is empty).  [records] must
    be in ascending order. *)
val resolve : record list -> target -> record option
