(** Page format codec for sqlocaml.

    All B+-tree on-disk state lives in 4096-byte pages.  This module defines
    the page layout as Cstruct accessors and the types for each page kind.
    No I/O happens here — this is a pure codec. *)

val page_size : int
(** 4096 bytes per page. *)

val header_size : int
(** 16 bytes — the common page header. *)

val data_offset : int
(** 16 — first byte of the data area (same as [header_size]). *)

val max_data_bytes : int
(** 4080 — bytes available in the data area (bytes 16..4095). *)

(** Kind of a page. *)
type kind = Header | Branch | Leaf | Freelist

(** Common page header fields (bytes 0–15). *)
type common = {
  kind       : kind;
  flags      : int;    (** uint8; reserved, should be 0 *)
  n_keys     : int;    (** uint16 BE; number of keys / entries *)
  right_page : int32;  (** uint32 BE; rightmost-child / next-leaf / next-freelist *)
  crc32      : int32;  (** uint32 BE; CRC32 of whole page with this field zeroed *)
}

val read_common  : Cstruct.t -> common
(** Read the 16-byte common header from [buf].
    Raises [Failure "invalid page kind"] if the kind byte is not 0–3. *)

val write_common : Cstruct.t -> common -> unit
(** Write the 16-byte common header into [buf].  Zeros the 4-byte reserved
    field (bytes 12–15). *)

(** CRC32 (IEEE polynomial) of page, with crc32 field (bytes 8..11) zeroed *)
val compute_crc : Cstruct.t -> int32

val verify_crc  : Cstruct.t -> bool
(** [true] if [compute_crc buf] equals the crc32 stored in the common header. *)

val seal : Cstruct.t -> unit
(** Compute CRC and write it into the page at bytes 8..11.
    Call after all other fields are written. *)

(** Header page fields (kind = [Header]; bytes 16–63). *)
type header_fields = {
  txn_id         : int64;  (** int64 BE; higher = more recent *)
  root_page      : int64;  (** int64 BE; page-id of B+-tree root *)
  freelist_page  : int64;  (** int64 BE; first freelist page, 0 if none *)
  n_pages_total  : int64;  (** int64 BE; total pages in file including headers *)
  schema_version : int64;  (** int64 BE *)
  page_size      : int32;  (** int32 BE; must be 4096 *)
  format_version : int32;  (** int32 BE; 1 for v1 *)
}

val read_header_fields  : Cstruct.t -> header_fields
(** Read header-page fields from [buf] (reads bytes 16–63). *)

val write_header_fields : Cstruct.t -> header_fields -> unit
(** Write header-page fields into [buf].  Zeros bytes 16–4095 first (including
    the reserved area 64–4095) to prevent stale data from corrupting the CRC,
    then writes the known fields into bytes 16–63. *)

(** A decoded branch page entry. *)
type branch_entry = {
  key         : bytes;
  left_child  : int32;
  next_offset : int;
}

(** Branch page entry iteration.

    Call repeatedly with increasing [offset] starting at [data_offset].
    The caller is responsible for stopping after [n_keys] entries; `` `End``
    is only returned when [offset] is truly out of the page bounds.

    Returns `` `Entry branch_entry`` or `` `End``. *)
val branch_entry_at : Cstruct.t -> offset:int ->
  [ `Entry of branch_entry | `End ]

(** Append a branch entry at [offset].  Returns the next offset after the
    appended entry.  Raises [Invalid_argument] if the entry would overflow the
    page or if [key] is longer than 65535 bytes. *)
val branch_append_entry : Cstruct.t -> offset:int -> key:bytes -> left_child:int32 -> int

(** A decoded leaf page entry. *)
type leaf_entry = {
  key         : bytes;
  value       : bytes;
  next_offset : int;
}

(** Leaf page entry iteration.

    Call repeatedly starting at [data_offset].  The caller is responsible for
    stopping after [n_keys] entries.

    Returns `` `Entry leaf_entry`` or `` `End``. *)
val leaf_entry_at : Cstruct.t -> offset:int ->
  [ `Entry of leaf_entry | `End ]

(** Append a leaf entry at [offset].  Returns the next offset.  Raises
    [Invalid_argument] if the entry would overflow the page or if [key] or
    [value] is longer than 65535 bytes. *)
val leaf_append_entry : Cstruct.t -> offset:int -> key:bytes -> value:bytes -> int

val max_freelist_entries_per_page : int
(** [4080 / 12 = 340] freelist entries fit per page. *)

(** A decoded freelist page entry. *)
type freelist_entry = {
  page_id          : int32;
  freed_at_txn_id  : int64;
}

(** Read a freelist entry by 0-based [index]. *)
val freelist_entry_at : Cstruct.t -> index:int -> freelist_entry

(** Write a freelist entry at 0-based [index]. *)
val freelist_set_entry : Cstruct.t -> index:int -> page_id:int32 -> freed_at_txn_id:int64 -> unit
