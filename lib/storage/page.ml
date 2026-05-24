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

(* Build the IEEE CRC32 lookup table at module load time. *)
let crc32_table : int32 array =
  let table = Array.make 256 0l in
  for i = 0 to 255 do
    let crc = ref (Int32.of_int i) in
    for _ = 0 to 7 do
      if Int32.logand !crc 1l = 1l
      then crc := Int32.logxor (Int32.shift_right_logical !crc 1) 0xEDB88320l
      else crc := Int32.shift_right_logical !crc 1
    done;
    table.(i) <- !crc
  done;
  table
;;

(*
  Compute CRC32 over all 4096 bytes of [buf], treating bytes 8..11 (the
  crc32 field) as zeros without modifying the buffer.
*)
let compute_crc buf =
  let crc = ref 0xFFFFFFFFl in
  for i = 0 to page_size - 1 do
    (* Treat bytes 8..11 (the stored CRC32 field) as zero during computation *)
    let byte = if i >= 8 && i <= 11 then 0 else Char.code (Cstruct.get_char buf i) in
    let idx = Int32.to_int (Int32.logand (Int32.logxor !crc (Int32.of_int byte)) 0xFFl) in
    crc := Int32.logxor (Int32.shift_right_logical !crc 8) crc32_table.(idx)
  done;
  Int32.logxor !crc 0xFFFFFFFFl
;;

let verify_crc buf =
  let stored = Cstruct.BE.get_uint32 buf 8 in
  let computed = compute_crc buf in
  computed = stored
;;

let seal buf =
  let crc = compute_crc buf in
  Cstruct.BE.set_uint32 buf 8 crc
;;

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
    +64..4095 reserved zeros
*)

type header_fields =
  { txn_id : int64
  ; root_page : int64
  ; freelist_page : int64
  ; n_pages_total : int64
  ; schema_version : int64
  ; page_size : int32
  ; format_version : int32
  }

let read_header_fields buf =
  let txn_id = Cstruct.BE.get_uint64 buf 16 in
  let root_page = Cstruct.BE.get_uint64 buf 24 in
  let freelist_page = Cstruct.BE.get_uint64 buf 32 in
  let n_pages_total = Cstruct.BE.get_uint64 buf 40 in
  let schema_version = Cstruct.BE.get_uint64 buf 48 in
  let page_size = Cstruct.BE.get_uint32 buf 56 in
  let format_version = Cstruct.BE.get_uint32 buf 60 in
  { txn_id
  ; root_page
  ; freelist_page
  ; n_pages_total
  ; schema_version
  ; page_size
  ; format_version
  }
;;

let write_header_fields buf hf =
  (* Zero bytes 16..4095 first so the reserved area (64..4095) is clean.
     write_common handles bytes 0..15 separately. *)
  Cstruct.memset (Cstruct.sub buf 16 (page_size - 16)) 0;
  Cstruct.BE.set_uint64 buf 16 hf.txn_id;
  Cstruct.BE.set_uint64 buf 24 hf.root_page;
  Cstruct.BE.set_uint64 buf 32 hf.freelist_page;
  Cstruct.BE.set_uint64 buf 40 hf.n_pages_total;
  Cstruct.BE.set_uint64 buf 48 hf.schema_version;
  Cstruct.BE.set_uint32 buf 56 hf.page_size;
  Cstruct.BE.set_uint32 buf 60 hf.format_version
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

let branch_append_entry buf ~offset ~key ~left_child =
  let key_len = Bytes.length key in
  if key_len > 0xFFFF
  then invalid_arg "branch_append_entry: key too long (max 65535 bytes)";
  let entry_size = 2 + key_len + 4 in
  if offset + entry_size > page_size
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

let leaf_append_entry buf ~offset ~key ~value =
  let key_len = Bytes.length key in
  let val_len = Bytes.length value in
  if key_len > 0xFFFF then invalid_arg "leaf_append_entry: key too long (max 65535 bytes)";
  if val_len > 0xFFFF
  then invalid_arg "leaf_append_entry: value too long (max 65535 bytes)";
  let entry_size = 2 + key_len + 2 + val_len in
  if offset + entry_size > page_size
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

let write_overflow buf ~next_pid ~payload ~payload_off ~payload_len =
  if payload_len < 0 || payload_len > max_overflow_payload_bytes
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
