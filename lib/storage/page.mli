(** Page format codec for sqlocaml.

    All B+-tree on-disk state lives in 4096-byte pages.  This module defines
    the page layout as Cstruct accessors and the types for each page kind.
    No I/O happens here — this is a pure codec. *)

(** Default page size in bytes (4096).  The codec itself sizes to the actual
    buffer length (#95); runtime geometry lives in {!Geometry}/{!Pager}.  This
    constant is the default value, used for building default-geometry pages
    (mainly in tests). *)
val page_size : int

(** 16 bytes — the common page header (geometry-independent). *)
val header_size : int

(** 16 — first byte of the data area (same as [header_size]). *)
val data_offset : int

(** 4080 — usable data bytes for the DEFAULT 4096 geometry.  For a runtime
    geometry use {!Geometry.max_data_bytes}. *)
val max_data_bytes : int

(** Kind of a page. *)
type kind =
  | Header
  | Branch
  | Leaf
  | Freelist
  | Overflow

(** Common page header fields (bytes 0–15). *)
type common =
  { kind : kind
  ; flags : int (** uint8; reserved, should be 0 *)
  ; n_keys : int (** uint16 BE; number of keys / entries *)
  ; right_page : int32 (** uint32 BE; rightmost-child / next-leaf / next-freelist *)
  ; crc32 : int32 (** uint32 BE; CRC32 of whole page with this field zeroed *)
  }

(** Read the 16-byte common header from [buf].
    Raises [Failure "invalid page kind"] if the kind byte is not 0–3. *)
val read_common : Cstruct.t -> common

(** Write the 16-byte common header into [buf].  Zeros the 4-byte reserved
    field (bytes 12–15). *)
val write_common : Cstruct.t -> common -> unit

(** CRC32 (IEEE polynomial) of page, with crc32 field (bytes 8..11) zeroed *)
val compute_crc : Cstruct.t -> int32

(** [true] if [compute_crc buf] equals the crc32 stored in the common header. *)
val verify_crc : Cstruct.t -> bool

(** Compute CRC and write it into the page at bytes 8..11.
    Call after all other fields are written. *)
val seal : Cstruct.t -> unit

(** Write the per-tree schema-fingerprint stamp (#174) into the reserved
    bytes 12–15.  Call AFTER {!write_common} and BEFORE {!seal} so the CRC
    covers it.  0 means "no stamp" (system trees, legacy pages). *)
val write_tag : Cstruct.t -> int32 -> unit

(** Read the per-tree schema-fingerprint stamp from bytes 12–15 (#174). *)
val read_tag : Cstruct.t -> int32

(** Header page fields (kind = [Header]; bytes 16–103, remainder reserved). *)
type header_fields =
  { txn_id : int64 (** int64 BE; higher = more recent *)
  ; root_page : int64 (** int64 BE; page-id of B+-tree root *)
  ; freelist_page : int64 (** int64 BE; first freelist page, 0 if none *)
  ; n_pages_total : int64 (** int64 BE; total pages in file including headers *)
  ; schema_version : int64 (** int64 BE *)
  ; page_size : int32 (** int32 BE; a multiple of 4096 (#95) *)
  ; format_version : int32 (** int32 BE; 1 for v1 *)
  ; reserved_bytes_per_page : int32
    (** int32 BE; fixed bytes carved off each page's tail (#95), 0 by default *)
  ; enc_magic : int32
    (** uint32 BE at byte 68; 0x53454E43 "SENC" when encrypted, else 0 (#84) *)
  ; canary_nonce : string
    (** 16 bytes at bytes 72–87; meaningful only when [enc_magic] is set (#84) *)
  ; canary_tag : string
    (** 16 bytes at bytes 88–103; meaningful only when [enc_magic] is set (#84) *)
  }

(** Read header-page fields from [buf] (reads bytes 16–103). *)
val read_header_fields : Cstruct.t -> header_fields

(** Write header-page fields into [buf].  Zeros bytes 16–(page_size-1) first
    (including the reserved area 104+) to prevent stale data from corrupting
    the CRC, then writes the known fields into bytes 16–103. *)
val write_header_fields : Cstruct.t -> header_fields -> unit

(** A decoded branch page entry. *)
type branch_entry =
  { key : bytes
  ; left_child : int32
  ; next_offset : int
  }

(** Branch page entry iteration.

    Call repeatedly with increasing [offset] starting at [data_offset].
    The caller is responsible for stopping after [n_keys] entries; `` `End``
    is only returned when [offset] is truly out of the page bounds.

    Returns `` `Entry branch_entry`` or `` `End``. *)
val branch_entry_at : Cstruct.t -> offset:int -> [ `Entry of branch_entry | `End ]

(** Append a branch entry at [offset].  Returns the next offset after the
    appended entry.  The usable ceiling is [Cstruct.length buf - reserved]
    ([reserved] defaults to 0, #95); the page size is taken from the buffer.
    Raises [Invalid_argument] if the entry would overflow that ceiling or if
    [key] is longer than 65535 bytes. *)
val branch_append_entry
  :  ?reserved:int
  -> Cstruct.t
  -> offset:int
  -> key:bytes
  -> left_child:int32
  -> int

(** A decoded leaf page entry. *)
type leaf_entry =
  { key : bytes
  ; value : bytes
  ; next_offset : int
  }

(** Leaf page entry iteration.

    Call repeatedly starting at [data_offset].  The caller is responsible for
    stopping after [n_keys] entries.

    Returns `` `Entry leaf_entry`` or `` `End``. *)
val leaf_entry_at : Cstruct.t -> offset:int -> [ `Entry of leaf_entry | `End ]

(** Append a leaf entry at [offset].  Returns the next offset.  The usable
    ceiling is [Cstruct.length buf - reserved] ([reserved] defaults to 0, #95);
    the page size is taken from the buffer.  Raises [Invalid_argument] if the
    entry would overflow that ceiling or if [key] or [value] is longer than
    65535 bytes. *)
val leaf_append_entry
  :  ?reserved:int
  -> Cstruct.t
  -> offset:int
  -> key:bytes
  -> value:bytes
  -> int

(** In-place point lookup on a sorted leaf page ([n_keys] = [common.n_keys]).
    Returns the matching value (freshly copied) or [None].  Equivalent to
    decoding the full entry list and scanning it, but allocates ONLY the
    matched value — no entry list, and no key/value bytes for the entries it
    skips (#245).  Bounds handling mirrors {!leaf_entry_at}. *)
val leaf_lookup : Cstruct.t -> n_keys:int -> key:bytes -> bytes option

(** In-place branch child selection on a sorted branch page.  Returns the
    child page-id (int32) to descend into for [key] — the [left_child] of the
    first entry whose key strictly exceeds [key], else [right_page]
    (= [common.right_page]).  Allocates nothing; byte-identical to the
    list-based pick (#245). *)
val branch_pick : Cstruct.t -> n_keys:int -> right_page:int32 -> key:bytes -> int32

(** Result of {!leaf_find_position}: where a key belongs in a sorted leaf page. *)
type leaf_position =
  { insert_off : int
  ; data_end : int
  ; key_found : bool
  }

(** Zero-alloc scan to find where [key] belongs in a sorted leaf page.
    [insert_off] is the offset of the first entry >= [key] (or [data_end]).
    [key_found] is true iff an exact match exists at [insert_off]. *)
val leaf_find_position : Cstruct.t -> n_keys:int -> key:bytes -> leaf_position

(** Build a new leaf page with ([key], [stored_value]) inserted at
    [pos.insert_off] via byte-surgery (no entry decode/re-encode).
    Caller guarantees [pos.key_found = false] and the entry fits without split. *)
val leaf_blit_insert
  :  Cstruct.t
  -> pos:leaf_position
  -> key:bytes
  -> stored_value:bytes
  -> right_page:int32
  -> write_tag:int32
  -> n_keys:int
  -> Cstruct.t

(** Insert ([key], [stored_value]) into a sorted leaf page IN PLACE at
    [pos.insert_off], shifting tail entries up and bumping n_keys.  Mutates
    [buf] directly — used by the in-place insert fast path (#356) when the leaf
    is already owned by the current write txn.  Caller guarantees
    [pos.key_found = false] and the entry fits.  Preserves right_page and tag;
    CRC reseals at flush. *)
val leaf_insert_inplace
  :  Cstruct.t
  -> pos:leaf_position
  -> key:bytes
  -> stored_value:bytes
  -> n_keys:int
  -> unit

(** Zero-alloc test whether the leaf entry starting at byte [offset] has key
    equal to [key].  Bounds-checked (false on a malformed offset).  Used to
    validate an append cursor against the live page (#356). *)
val leaf_key_matches_at : Cstruct.t -> offset:int -> key:bytes -> bool

(** Like {!branch_pick} but also returns the chosen child's ordinal index and
    byte offset of its [left_child] int32 field (-1 for [right_page]). *)
val branch_pick_with_info
  :  Cstruct.t
  -> n_keys:int
  -> right_page:int32
  -> key:bytes
  -> int32 * int * int

(** Build a new branch page identical to [buf] except the child pointer at
    [child_ptr_offset] is replaced with [new_child] ([child_ptr_offset < 0]
    updates [right_page] instead).  Returns a fresh, freshly-sealed Cstruct. *)
val branch_blit_update_child
  :  Cstruct.t
  -> child_ptr_offset:int
  -> new_child:int32
  -> write_tag:int32
  -> Cstruct.t

(** [4080 / 12 = 340] freelist entries per page for the DEFAULT 4096 geometry.
    For a runtime geometry use {!Geometry.max_freelist_entries_per_page}. *)
val max_freelist_entries_per_page : int

(** A decoded freelist page entry. *)
type freelist_entry =
  { page_id : int32
  ; freed_at_txn_id : int64
  }

(** Read a freelist entry by 0-based [index]. *)
val freelist_entry_at : Cstruct.t -> index:int -> freelist_entry

(** Write a freelist entry at 0-based [index]. *)
val freelist_set_entry
  :  Cstruct.t
  -> index:int
  -> page_id:int32
  -> freed_at_txn_id:int64
  -> unit

(** Maximum payload bytes per overflow page for the default 4096 geometry
    (4080 - 2).  For a runtime geometry use {!Geometry.max_overflow_payload_bytes}. *)
val max_overflow_payload_bytes : int

(** Read the payload length stored at bytes 16..17 of an Overflow page. *)
val overflow_payload_len : Cstruct.t -> int

(** Slice the payload bytes from an Overflow page. *)
val overflow_payload : Cstruct.t -> bytes

(** Write an overflow page: sets kind=Overflow, n_keys=0, right_page=next_pid,
    payload_len, and copies [payload] into the data area.  The maximum payload
    is [Cstruct.length buf - data_offset - 2 - reserved] ([reserved] defaults to
    0, #95).  Does NOT seal — caller invokes [seal]. *)
val write_overflow
  :  ?reserved:int
  -> Cstruct.t
  -> next_pid:int32
  -> payload:bytes
  -> payload_off:int
  -> payload_len:int
  -> unit
