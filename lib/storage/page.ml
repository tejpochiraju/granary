(** Page format codec — pure, no I/O, no Lwt. *)

(* ------------------------------------------------------------------ *)
(* Constants                                                           *)
(* ------------------------------------------------------------------ *)

let page_size = 4096
let header_size = 16
let data_offset = 16
let max_data_bytes = 4080 (* 4096 - 16 *)

(* ------------------------------------------------------------------ *)
(* Kind                                                                *)
(* ------------------------------------------------------------------ *)

type kind =
  | Header
  | Branch
  | Leaf
  | Freelist
  | Overflow

let kind_of_byte = function
  | 0 -> Header
  | 1 -> Branch
  | 2 -> Leaf
  | 3 -> Freelist
  | 4 -> Overflow
  | n -> failwith (Printf.sprintf "invalid page kind: %d" n)
;;

let byte_of_kind = function
  | Header -> 0
  | Branch -> 1
  | Leaf -> 2
  | Freelist -> 3
  | Overflow -> 4
;;

(* ------------------------------------------------------------------ *)
(* Common header                                                       *)
(* ------------------------------------------------------------------ *)

type common =
  { kind : kind
  ; flags : int
  ; n_keys : int
  ; right_page : int32
  ; crc32 : int32
  }

(*
  Common header layout (bytes 0–15):
    +0  [1] kind       (uint8)
    +1  [1] flags      (uint8)
    +2  [2] n_keys     (uint16 BE)
    +4  [4] right_page (uint32 BE)
    +8  [4] crc32      (uint32 BE)
    +12 [4] reserved   (zeros)
*)

let read_common buf =
  let kind_byte = Cstruct.get_uint8 buf 0 in
  let kind = kind_of_byte kind_byte in
  let flags = Cstruct.get_uint8 buf 1 in
  let n_keys = Cstruct.BE.get_uint16 buf 2 in
  let right_page = Cstruct.BE.get_uint32 buf 4 in
  let crc32 = Cstruct.BE.get_uint32 buf 8 in
  { kind; flags; n_keys; right_page; crc32 }
;;

let write_common buf c =
  Cstruct.set_uint8 buf 0 (byte_of_kind c.kind);
  Cstruct.set_uint8 buf 1 c.flags;
  Cstruct.BE.set_uint16 buf 2 c.n_keys;
  Cstruct.BE.set_uint32 buf 4 c.right_page;
  Cstruct.BE.set_uint32 buf 8 c.crc32;
  (* zero the reserved field *)
  Cstruct.BE.set_uint32 buf 12 0l
;;

(* ------------------------------------------------------------------ *)
(* CRC32 (IEEE polynomial)                                             *)
(* ------------------------------------------------------------------ *)

(* CRC32 table using native OCaml int (63-bit on 64-bit platforms) to avoid
   the per-operation heap allocation that OCaml's Int32 boxing incurs.  Every
   value fits in a 32-bit unsigned range so 63-bit int is a safe superset.
   Requires 64-bit platform — correct for all sqlocaml/MirageOS targets. *)
let crc32_table : int array =
  let table = Array.make 256 0 in
  for i = 0 to 255 do
    let crc = ref i in
    for _ = 0 to 7 do
      if !crc land 1 = 1 then crc := (!crc lsr 1) lxor 0xEDB88320 else crc := !crc lsr 1
    done;
    table.(i) <- !crc
  done;
  table
;;

(*
  Compute CRC32 over all bytes of [buf], treating bytes 8..11 (the
  crc32 field) as zeros without modifying the buffer.  Returns a native
  int holding the unsigned 32-bit CRC value (high 31 bits are zero).
*)
let compute_crc_int buf =
  let crc = ref 0xFFFFFFFF in
  for i = 0 to Cstruct.length buf - 1 do
    let byte = if i >= 8 && i <= 11 then 0 else Char.code (Cstruct.get_char buf i) in
    crc := crc32_table.(!crc lxor byte land 0xFF) lxor (!crc lsr 8)
  done;
  !crc lxor 0xFFFFFFFF
;;

let compute_crc buf = Int32.of_int (compute_crc_int buf)

let verify_crc buf =
  let stored = Cstruct.BE.get_uint32 buf 8 in
  Int32.of_int (compute_crc_int buf) = stored
;;

let seal buf = Cstruct.BE.set_uint32 buf 8 (Int32.of_int (compute_crc_int buf))

(* #174: per-tree schema-fingerprint stamp.  It lives in the 4 reserved bytes
   at offset 12 of the common header (zeroed by [write_common]).  Write it
   AFTER [write_common] and BEFORE [seal] — the CRC covers bytes 12..15, so the
   stamp is integrity-protected and any tamper is caught on read. *)
let tag_offset = 12
let write_tag buf (tag : int32) = Cstruct.BE.set_uint32 buf tag_offset tag
let read_tag buf : int32 = Cstruct.BE.get_uint32 buf tag_offset

(* ------------------------------------------------------------------ *)
(* Header page fields (kind = Header, bytes 16–63)                    *)
(* ------------------------------------------------------------------ *)

(*
  Header page layout (bytes 16..):
    +16  [8] txn_id         (int64 BE)
    +24  [8] root_page      (int64 BE)
    +32  [8] freelist_page  (int64 BE)
    +40  [8] n_pages_total  (int64 BE)
    +48  [8] schema_version (int64 BE)
    +56  [4] page_size      (int32 BE)
    +60  [4] format_version (int32 BE)
    +64  [4] reserved_bytes_per_page (int32 BE, #95)
    +68  [4] enc_magic      (uint32 BE; 0x53454E43 "SENC" when encrypted, #84)
    +72  [16] canary_nonce  (16 bytes; meaningful when enc_magic set, #84)
    +88  [16] canary_tag    (16 bytes; meaningful when enc_magic set, #84)
    +104..(page_size-1) reserved zeros
*)

type header_fields =
  { txn_id : int64
  ; root_page : int64
  ; freelist_page : int64
  ; n_pages_total : int64
  ; schema_version : int64
  ; page_size : int32
  ; format_version : int32
  ; reserved_bytes_per_page : int32
  ; enc_magic : int32 (** 0x53454E43 "SENC" when encrypted, else 0 (#84) *)
  ; canary_nonce : string (** 16 bytes; meaningful only when enc_magic set *)
  ; canary_tag : string (** 16 bytes; meaningful only when enc_magic set *)
  }

let read_header_fields buf =
  let txn_id = Cstruct.BE.get_uint64 buf 16 in
  let root_page = Cstruct.BE.get_uint64 buf 24 in
  let freelist_page = Cstruct.BE.get_uint64 buf 32 in
  let n_pages_total = Cstruct.BE.get_uint64 buf 40 in
  let schema_version = Cstruct.BE.get_uint64 buf 48 in
  let page_size = Cstruct.BE.get_uint32 buf 56 in
  let format_version = Cstruct.BE.get_uint32 buf 60 in
  let reserved_bytes_per_page = Cstruct.BE.get_uint32 buf 64 in
  let enc_magic = Cstruct.BE.get_uint32 buf 68 in
  let canary_nonce = Cstruct.to_string buf ~off:72 ~len:16 in
  let canary_tag = Cstruct.to_string buf ~off:88 ~len:16 in
  { txn_id
  ; root_page
  ; freelist_page
  ; n_pages_total
  ; schema_version
  ; page_size
  ; format_version
  ; reserved_bytes_per_page
  ; enc_magic
  ; canary_nonce
  ; canary_tag
  }
;;

let write_header_fields buf hf =
  (* Zero bytes 16..end first so the reserved area is clean (sized to the
     actual page buffer, #95).  write_common handles bytes 0..15 separately. *)
  Cstruct.memset (Cstruct.sub buf 16 (Cstruct.length buf - 16)) 0;
  Cstruct.BE.set_uint64 buf 16 hf.txn_id;
  Cstruct.BE.set_uint64 buf 24 hf.root_page;
  Cstruct.BE.set_uint64 buf 32 hf.freelist_page;
  Cstruct.BE.set_uint64 buf 40 hf.n_pages_total;
  Cstruct.BE.set_uint64 buf 48 hf.schema_version;
  Cstruct.BE.set_uint32 buf 56 hf.page_size;
  Cstruct.BE.set_uint32 buf 60 hf.format_version;
  Cstruct.BE.set_uint32 buf 64 hf.reserved_bytes_per_page;
  Cstruct.BE.set_uint32 buf 68 hf.enc_magic;
  if Int32.equal hf.enc_magic 0l
  then ()
  else (
    Cstruct.blit_from_string hf.canary_nonce 0 buf 72 16;
    Cstruct.blit_from_string hf.canary_tag 0 buf 88 16)
;;

(* ------------------------------------------------------------------ *)
(* Branch page entries                                                 *)
(* ------------------------------------------------------------------ *)

(*
  Each branch entry:
    [key_len: uint16 BE][key: key_len bytes][left_child: uint32 BE]
  Total size per entry: 2 + key_len + 4 = 6 + key_len bytes.
*)

type branch_entry =
  { key : bytes
  ; left_child : int32
  ; next_offset : int
  }

let branch_entry_at buf ~offset =
  let page_size = Cstruct.length buf in
  (* Need at least 6 bytes for key_len (2) + left_child (4) *)
  if offset + 6 > page_size
  then `End
  else (
    let key_len = Cstruct.BE.get_uint16 buf offset in
    if offset + 2 + key_len + 4 > page_size
    then `End
    else (
      let key = Bytes.create key_len in
      Cstruct.blit_to_bytes buf (offset + 2) key 0 key_len;
      let left_child = Cstruct.BE.get_uint32 buf (offset + 2 + key_len) in
      let next_offset = offset + 2 + key_len + 4 in
      `Entry { key; left_child; next_offset }))
;;

let branch_append_entry ?(reserved = 0) buf ~offset ~key ~left_child =
  let page_size = Cstruct.length buf in
  let key_len = Bytes.length key in
  if key_len > 0xFFFF
  then invalid_arg "branch_append_entry: key too long (max 65535 bytes)";
  let entry_size = 2 + key_len + 4 in
  if offset + entry_size > page_size - reserved
  then
    invalid_arg
      (Printf.sprintf
         "branch_append_entry: entry size %d would overflow page at offset %d"
         entry_size
         offset);
  Cstruct.BE.set_uint16 buf offset key_len;
  Cstruct.blit_from_bytes key 0 buf (offset + 2) key_len;
  Cstruct.BE.set_uint32 buf (offset + 2 + key_len) left_child;
  offset + 2 + key_len + 4
;;

(* ------------------------------------------------------------------ *)
(* Leaf page entries                                                   *)
(* ------------------------------------------------------------------ *)

(*
  Each leaf entry:
    [key_len: uint16 BE][key: key_len bytes][val_len: uint16 BE][val: val_len bytes]
  Total size per entry: 2 + key_len + 2 + val_len = 4 + key_len + val_len bytes.
*)

type leaf_entry =
  { key : bytes
  ; value : bytes
  ; next_offset : int
  }

let leaf_entry_at buf ~offset =
  let page_size = Cstruct.length buf in
  (* Need at least 4 bytes for key_len (2) + val_len (2) *)
  if offset + 4 > page_size
  then `End
  else (
    let key_len = Cstruct.BE.get_uint16 buf offset in
    if offset + 2 + key_len + 2 > page_size
    then `End
    else (
      let val_len = Cstruct.BE.get_uint16 buf (offset + 2 + key_len) in
      if offset + 2 + key_len + 2 + val_len > page_size
      then `End
      else (
        let key = Bytes.create key_len in
        Cstruct.blit_to_bytes buf (offset + 2) key 0 key_len;
        let value = Bytes.create val_len in
        Cstruct.blit_to_bytes buf (offset + 2 + key_len + 2) value 0 val_len;
        let next_offset = offset + 2 + key_len + 2 + val_len in
        `Entry { key; value; next_offset })))
;;

let leaf_append_entry ?(reserved = 0) buf ~offset ~key ~value =
  let page_size = Cstruct.length buf in
  let key_len = Bytes.length key in
  let val_len = Bytes.length value in
  if key_len > 0xFFFF then invalid_arg "leaf_append_entry: key too long (max 65535 bytes)";
  if val_len > 0xFFFF
  then invalid_arg "leaf_append_entry: value too long (max 65535 bytes)";
  let entry_size = 2 + key_len + 2 + val_len in
  if offset + entry_size > page_size - reserved
  then
    invalid_arg
      (Printf.sprintf
         "leaf_append_entry: entry size %d would overflow page at offset %d"
         entry_size
         offset);
  Cstruct.BE.set_uint16 buf offset key_len;
  Cstruct.blit_from_bytes key 0 buf (offset + 2) key_len;
  Cstruct.BE.set_uint16 buf (offset + 2 + key_len) val_len;
  Cstruct.blit_from_bytes value 0 buf (offset + 2 + key_len + 2) val_len;
  offset + 2 + key_len + 2 + val_len
;;

(* ------------------------------------------------------------------ *)
(* In-place B-tree search (no per-entry allocation, #245)              *)
(* ------------------------------------------------------------------ *)

(* These searches are called once per entry / once per page on the hot point-
   lookup path, so they must allocate NOTHING beyond the matched value.  Each
   loop is a TOP-LEVEL recursive function taking all state as parameters: a
   nested [let rec] would capture its free variables into a closure that OCaml
   heap-allocates on every call (measured ~72 B per comparison — O(n_keys) of
   them per page, which would defeat the point of avoiding the entry list).

   MUST STAY IN SYNC with the entry layout encoded in [leaf_entry_at] /
   [branch_entry_at] above: these duplicate the byte offsets (key_len/val_len
   positions, bounds checks) to read in place instead of decoding to records.
   Any format change (e.g. the deferred cell-pointer directory) must touch both
   sides; the [prop_leaf_lookup_matches] / [prop_branch_pick_matches] QCheck
   equivalence tests are the backstop that catches drift. *)

(* Byte loop for [compare_key_at]: compare [key.[0..n)] against [buf] bytes at
   [kstart..kstart+n) as UNSIGNED.  Returns 0 if the [n]-byte prefixes are
   equal, else the sign of the first differing byte. *)
let rec compare_key_loop buf kstart key n i =
  if i >= n
  then 0
  else (
    let a = Char.code (Bytes.unsafe_get key i) in
    let b = Cstruct.get_uint8 buf (kstart + i) in
    if a <> b then if a < b then -1 else 1 else compare_key_loop buf kstart key n (i + 1))
;;

(* Compare a target [key] against the entry key stored in [buf] at byte range
   [kstart .. kstart+klen), WITHOUT copying the stored key out.  Same sign
   convention as [Bytes.compare key stored_key]: negative if [key] sorts
   before, 0 if equal, positive if after.  Bytes compared as UNSIGNED, then the
   shorter key sorts first — byte-identical to [Bytes.compare].  Both leaf and
   branch entries start with [key_len: uint16][key: key_len], so this serves
   both.  Allocates nothing. *)
let compare_key_at buf ~kstart ~klen ~key =
  let kb = Bytes.length key in
  let n = if kb < klen then kb else klen in
  let c = compare_key_loop buf kstart key n 0 in
  if c <> 0 then c else Int.compare kb klen
;;

let rec leaf_lookup_loop buf page_size n_keys key offset i =
  if i >= n_keys || offset + 4 > page_size
  then None
  else (
    let key_len = Cstruct.BE.get_uint16 buf offset in
    let val_off = offset + 2 + key_len in
    if val_off + 2 > page_size
    then None
    else (
      let val_len = Cstruct.BE.get_uint16 buf val_off in
      if val_off + 2 + val_len > page_size
      then None
      else (
        let c = compare_key_at buf ~kstart:(offset + 2) ~klen:key_len ~key in
        if c = 0
        then (
          let value = Bytes.create val_len in
          Cstruct.blit_to_bytes buf (val_off + 2) value 0 val_len;
          Some value)
        else if c < 0
        then None (* target sorts before this entry: not present *)
        else leaf_lookup_loop buf page_size n_keys key (val_off + 2 + val_len) (i + 1))))
;;

(* In-place point lookup on a sorted leaf page ([n_keys] = [common.n_keys]).
   Walks entries by offset, comparing each key against the page bytes directly;
   returns the matching value (freshly copied) or [None].  Allocates ONLY the
   matched value — no entry list, no per-entry key/value bytes, no closure.
   Bounds handling and the sorted short-circuit exactly mirror
   [decode]-then-linear-scan over {!leaf_entry_at}, so results are
   byte-identical (#245). *)
let leaf_lookup buf ~n_keys ~key : bytes option =
  leaf_lookup_loop buf (Cstruct.length buf) n_keys key data_offset 0
;;

let rec branch_pick_loop buf page_size n_keys right_page key offset i =
  if i >= n_keys || offset + 6 > page_size
  then right_page
  else (
    let key_len = Cstruct.BE.get_uint16 buf offset in
    if offset + 2 + key_len + 4 > page_size
    then right_page
    else (
      let c = compare_key_at buf ~kstart:(offset + 2) ~klen:key_len ~key in
      if c < 0
      then Cstruct.BE.get_uint32 buf (offset + 2 + key_len)
      else
        branch_pick_loop
          buf
          page_size
          n_keys
          right_page
          key
          (offset + 2 + key_len + 4)
          (i + 1)))
;;

(* In-place branch child selection on a sorted branch page.  Returns the child
   page-id (int32) to descend into for [key] — the [left_child] of the first
   entry whose key is strictly greater than [key], else [right_page]
   (= [common.right_page]).  Allocates nothing; byte-identical to the
   list-based pick (#245). *)
let branch_pick buf ~n_keys ~right_page ~key : int32 =
  branch_pick_loop buf (Cstruct.length buf) n_keys right_page key data_offset 0
;;

(* ------------------------------------------------------------------ *)
(* Zero-alloc write-path helpers (#356)                                *)
(* ------------------------------------------------------------------ *)

(* Result of {!leaf_find_position}: where a key belongs in a sorted leaf page. *)
type leaf_position =
  { insert_off : int (* byte offset of first entry >= key, or data_end *)
  ; data_end : int (* byte offset right after last entry *)
  ; key_found : bool (* true iff an exact match exists at insert_off *)
  }

(* Scan a sorted leaf page to find where [key] belongs.  Single forward pass,
   allocates nothing.  Returns [insert_off = data_end] when the new key is
   larger than all existing keys. *)
let leaf_find_position buf ~n_keys ~key : leaf_position =
  let page_size = Cstruct.length buf in
  let rec scan offset i insert_off insert_set key_found =
    if i >= n_keys || offset + 4 > page_size
    then
      { insert_off = (if insert_set then insert_off else offset)
      ; data_end = offset
      ; key_found
      }
    else (
      let key_len = Cstruct.BE.get_uint16 buf offset in
      let val_off = offset + 2 + key_len in
      let val_len = Cstruct.BE.get_uint16 buf val_off in
      let next_off = val_off + 2 + val_len in
      if insert_set
      then scan next_off (i + 1) insert_off true key_found
      else (
        let c = compare_key_at buf ~kstart:(offset + 2) ~klen:key_len ~key in
        if c < 0
        then scan next_off (i + 1) offset true false
        else if c = 0
        then scan next_off (i + 1) offset true true
        else scan next_off (i + 1) (-1) false false))
  in
  scan data_offset 0 (-1) false false
;;

(* Build a new leaf page with [key, stored_value] inserted at [pos.insert_off].
   Entries before the insertion point are blitted from [buf]; entries after are
   blitted after the new entry.  Updates n_keys and tag.  CRC seal is deferred
   to flush-time (#356) — the pager seals all dirty pages at WAL-commit. *)
let leaf_blit_insert buf ~pos ~key ~stored_value ~right_page ~write_tag:tag ~n_keys =
  let page_size = Cstruct.length buf in
  let new_buf = Cstruct.create page_size in
  Cstruct.memset new_buf 0;
  let before_len = pos.insert_off - data_offset in
  if before_len > 0 then Cstruct.blit buf data_offset new_buf data_offset before_len;
  let after_off =
    leaf_append_entry new_buf ~offset:pos.insert_off ~key ~value:stored_value
  in
  let after_len = pos.data_end - pos.insert_off in
  if after_len > 0 then Cstruct.blit buf pos.insert_off new_buf after_off after_len;
  let common = { kind = Leaf; flags = 0; n_keys = n_keys + 1; right_page; crc32 = 0l } in
  write_common new_buf common;
  write_tag new_buf tag;
  new_buf
;;

(* Like [branch_pick_loop] but also returns the child's ordinal index and the
   byte offset of its [left_child] int32 field within [buf] (-1 for right_page).
   Allocates nothing. *)
let rec branch_pick_with_info_loop buf page_size n_keys right_page key offset i =
  if i >= n_keys || offset + 6 > page_size
  then right_page, i, -1
  else (
    let key_len = Cstruct.BE.get_uint16 buf offset in
    if offset + 2 + key_len + 4 > page_size
    then right_page, i, -1
    else (
      let c = compare_key_at buf ~kstart:(offset + 2) ~klen:key_len ~key in
      if c < 0
      then (
        let ptr_off = offset + 2 + key_len in
        Cstruct.BE.get_uint32 buf ptr_off, i, ptr_off)
      else
        branch_pick_with_info_loop
          buf
          page_size
          n_keys
          right_page
          key
          (offset + 2 + key_len + 4)
          (i + 1)))
;;

(* In-place branch child selection that also returns the chosen child's ordinal
   index and the byte offset of its [left_child] field (-1 for [right_page]).
   Used in the fast CoW branch-update path (#356). *)
let branch_pick_with_info buf ~n_keys ~right_page ~key : int32 * int * int =
  branch_pick_with_info_loop buf (Cstruct.length buf) n_keys right_page key data_offset 0
;;

(* Build a new branch page identical to [buf] except the child pointer at
   [child_ptr_offset] is replaced with [new_child].  If [child_ptr_offset < 0]
   the [right_page] header field is updated instead.  Returns a fresh,
   freshly-sealed Cstruct. *)
let branch_blit_update_child buf ~child_ptr_offset ~new_child ~write_tag:tag =
  let page_size = Cstruct.length buf in
  let new_buf = Cstruct.create page_size in
  Cstruct.blit buf 0 new_buf 0 page_size;
  if child_ptr_offset >= 0
  then Cstruct.BE.set_uint32 new_buf child_ptr_offset new_child
  else Cstruct.BE.set_uint32 new_buf 4 new_child;
  write_tag new_buf tag;
  new_buf
;;

(* ------------------------------------------------------------------ *)
(* Freelist page entries                                               *)
(* ------------------------------------------------------------------ *)

(*
  Each freelist entry:
    [page_id: uint32 BE][freed_at_txn_id: int64 BE]
  Total: 4 + 8 = 12 bytes per entry.
*)

type freelist_entry =
  { page_id : int32
  ; freed_at_txn_id : int64
  }

let freelist_entry_size = 12
let max_freelist_entries_per_page = max_data_bytes / freelist_entry_size
(* = 4080 / 12 = 340 *)

let freelist_entry_at buf ~index =
  let off = data_offset + (index * freelist_entry_size) in
  let page_id = Cstruct.BE.get_uint32 buf off in
  let freed_at_txn_id = Cstruct.BE.get_uint64 buf (off + 4) in
  { page_id; freed_at_txn_id }
;;

let freelist_set_entry buf ~index ~page_id ~freed_at_txn_id =
  let off = data_offset + (index * freelist_entry_size) in
  Cstruct.BE.set_uint32 buf off page_id;
  Cstruct.BE.set_uint64 buf (off + 4) freed_at_txn_id
;;

(* ------------------------------------------------------------------ *)
(* Overflow page (kind = Overflow)                                     *)
(* ------------------------------------------------------------------ *)

(*
  Overflow page layout:
    +0   [16] common header (kind=Overflow; right_page = next_pid; n_keys = 0)
    +16  [2]  payload_len (uint16 BE)
    +18  [payload_len] payload bytes
    +18+payload_len..4095 unused (zero-fill)
*)

let max_overflow_payload_bytes = max_data_bytes - 2
let overflow_payload_len buf = Cstruct.BE.get_uint16 buf data_offset

let overflow_payload buf =
  let len = overflow_payload_len buf in
  let out = Bytes.create len in
  Cstruct.blit_to_bytes buf (data_offset + 2) out 0 len;
  out
;;

let write_overflow ?(reserved = 0) buf ~next_pid ~payload ~payload_off ~payload_len =
  let max_payload = Cstruct.length buf - data_offset - 2 - reserved in
  if payload_len < 0 || payload_len > max_payload
  then
    invalid_arg (Printf.sprintf "write_overflow: payload_len %d out of range" payload_len);
  Cstruct.memset buf 0;
  let common =
    { kind = Overflow; flags = 0; n_keys = 0; right_page = next_pid; crc32 = 0l }
  in
  write_common buf common;
  Cstruct.BE.set_uint16 buf data_offset payload_len;
  Cstruct.blit_from_bytes payload payload_off buf (data_offset + 2) payload_len
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
