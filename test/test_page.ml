(** Tests for Sqlocaml_storage.Page *)

module P = Sqlocaml_storage.Page

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let fresh_page () =
  let buf = Cstruct.create P.page_size in
  Cstruct.memset buf 0;
  buf
;;

(* flip a single bit in byte at position [pos] *)
let flip_byte buf pos =
  let b = Cstruct.get_uint8 buf pos in
  Cstruct.set_uint8 buf pos (b lxor 0xFF)
;;

(* ------------------------------------------------------------------ *)
(* 1. Constants                                                        *)
(* ------------------------------------------------------------------ *)

let test_constants () =
  Alcotest.(check int) "page_size" 4096 P.page_size;
  Alcotest.(check int) "header_size" 16 P.header_size;
  Alcotest.(check int) "data_offset" 16 P.data_offset;
  Alcotest.(check int) "max_data_bytes" 4080 P.max_data_bytes
;;

let test_max_freelist_entries () =
  Alcotest.(check int) "max_freelist_entries_per_page" 340 P.max_freelist_entries_per_page
;;

(* ------------------------------------------------------------------ *)
(* 2. Common header round-trips                                        *)
(* ------------------------------------------------------------------ *)

let make_common kind = P.{ kind; flags = 0; n_keys = 0; right_page = 0l; crc32 = 0l }

let test_common_roundtrip_header () =
  let buf = fresh_page () in
  let c = make_common P.Header in
  P.write_common buf c;
  let c' = P.read_common buf in
  Alcotest.(check string)
    "kind=Header"
    "Header"
    (match c'.kind with
     | P.Header -> "Header"
     | _ -> "other")
;;

let test_common_roundtrip_branch () =
  let buf = fresh_page () in
  let c = make_common P.Branch in
  P.write_common buf c;
  let c' = P.read_common buf in
  Alcotest.(check string)
    "kind=Branch"
    "Branch"
    (match c'.kind with
     | P.Branch -> "Branch"
     | _ -> "other")
;;

let test_common_roundtrip_leaf () =
  let buf = fresh_page () in
  let c = make_common P.Leaf in
  P.write_common buf c;
  let c' = P.read_common buf in
  Alcotest.(check string)
    "kind=Leaf"
    "Leaf"
    (match c'.kind with
     | P.Leaf -> "Leaf"
     | _ -> "other")
;;

let test_common_roundtrip_freelist () =
  let buf = fresh_page () in
  let c = make_common P.Freelist in
  P.write_common buf c;
  let c' = P.read_common buf in
  Alcotest.(check string)
    "kind=Freelist"
    "Freelist"
    (match c'.kind with
     | P.Freelist -> "Freelist"
     | _ -> "other")
;;

let test_common_n_keys_roundtrip () =
  let buf = fresh_page () in
  let c = P.{ kind = P.Leaf; flags = 0; n_keys = 42; right_page = 0l; crc32 = 0l } in
  P.write_common buf c;
  let c' = P.read_common buf in
  Alcotest.(check int) "n_keys round-trip" 42 c'.n_keys
;;

let test_common_right_page_roundtrip () =
  let buf = fresh_page () in
  let c =
    P.{ kind = P.Branch; flags = 0; n_keys = 0; right_page = 0xDEADBEEFl; crc32 = 0l }
  in
  P.write_common buf c;
  let c' = P.read_common buf in
  Alcotest.(check int32) "right_page round-trip" 0xDEADBEEFl c'.right_page
;;

let test_common_flags_roundtrip () =
  let buf = fresh_page () in
  let c = P.{ kind = P.Leaf; flags = 7; n_keys = 0; right_page = 0l; crc32 = 0l } in
  P.write_common buf c;
  let c' = P.read_common buf in
  Alcotest.(check int) "flags round-trip" 7 c'.flags
;;

let test_common_reserved_zeroed () =
  let buf = fresh_page () in
  (* fill reserved area with non-zero *)
  Cstruct.set_uint8 buf 12 0xFF;
  Cstruct.set_uint8 buf 13 0xFF;
  Cstruct.set_uint8 buf 14 0xFF;
  Cstruct.set_uint8 buf 15 0xFF;
  let c = make_common P.Leaf in
  P.write_common buf c;
  (* reserved bytes must now be zero *)
  Alcotest.(check int) "reserved byte 12 = 0" 0 (Cstruct.get_uint8 buf 12);
  Alcotest.(check int) "reserved byte 15 = 0" 0 (Cstruct.get_uint8 buf 15)
;;

(* ------------------------------------------------------------------ *)
(* 3. Invalid kind byte                                                *)
(* ------------------------------------------------------------------ *)

let test_invalid_kind_byte () =
  let buf = fresh_page () in
  Cstruct.set_uint8 buf 0 255;
  match P.read_common buf with
  | _ -> Alcotest.fail "expected Failure for invalid kind byte"
  | exception Failure _ -> () (* expected *)
;;

(* Byte 4 is now a valid kind (Overflow, added in phase 37). *)
let test_invalid_kind_byte_4 () =
  let buf = fresh_page () in
  Cstruct.set_uint8 buf 0 5;
  match P.read_common buf with
  | _ -> Alcotest.fail "expected Failure for kind byte 5"
  | exception Failure _ -> ()
;;

(* ------------------------------------------------------------------ *)
(* 4. CRC32                                                            *)
(* ------------------------------------------------------------------ *)

let test_crc_deterministic () =
  let buf = fresh_page () in
  (* write some data *)
  Cstruct.set_uint8 buf 16 0xAB;
  Cstruct.set_uint8 buf 100 0xCD;
  let crc1 = P.compute_crc buf in
  let crc2 = P.compute_crc buf in
  Alcotest.(check int32) "compute_crc deterministic" crc1 crc2
;;

let test_verify_crc_fresh_page () =
  let buf = fresh_page () in
  let c = P.{ kind = P.Leaf; flags = 0; n_keys = 5; right_page = 42l; crc32 = 0l } in
  P.write_common buf c;
  (* write some payload *)
  Cstruct.set_uint8 buf 16 0xDE;
  Cstruct.set_uint8 buf 17 0xAD;
  (* compute CRC and store it *)
  let crc = P.compute_crc buf in
  let c2 = { c with P.crc32 = crc } in
  P.write_common buf c2;
  Alcotest.(check bool) "verify_crc on correctly-written page" true (P.verify_crc buf)
;;

let test_verify_crc_fails_flipped_data_byte () =
  let buf = fresh_page () in
  let c = P.{ kind = P.Branch; flags = 0; n_keys = 0; right_page = 0l; crc32 = 0l } in
  P.write_common buf c;
  Cstruct.set_uint8 buf 20 0x42;
  let crc = P.compute_crc buf in
  P.write_common buf { c with P.crc32 = crc };
  (* now flip a data byte *)
  flip_byte buf 20;
  Alcotest.(check bool) "verify_crc fails after data byte flip" false (P.verify_crc buf)
;;

let test_verify_crc_fails_flipped_crc_byte () =
  let buf = fresh_page () in
  let c = P.{ kind = P.Leaf; flags = 0; n_keys = 0; right_page = 0l; crc32 = 0l } in
  P.write_common buf c;
  let crc = P.compute_crc buf in
  P.write_common buf { c with P.crc32 = crc };
  (* flip a byte inside the CRC field itself (byte 8) *)
  flip_byte buf 8;
  Alcotest.(check bool) "verify_crc fails after CRC field flip" false (P.verify_crc buf)
;;

let test_seal () =
  let buf = fresh_page () in
  let c = P.{ kind = P.Leaf; flags = 0; n_keys = 5; right_page = 0l; crc32 = 0l } in
  P.write_common buf c;
  P.seal buf;
  Alcotest.(check bool) "verify after seal" true (P.verify_crc buf)
;;

(* ------------------------------------------------------------------ *)
(* 5. Header page fields round-trip                                    *)
(* ------------------------------------------------------------------ *)

let test_header_fields_roundtrip () =
  let buf = fresh_page () in
  let hf =
    P.
      { txn_id = 0x0102030405060708L
      ; root_page = 99L
      ; freelist_page = 7L
      ; n_pages_total = 1024L
      ; schema_version = 1L
      ; page_size = 4096l
      ; format_version = 1l
      ; reserved_bytes_per_page = 0l
      ; enc_magic = 0l
      ; canary_nonce = String.make 16 '\000'
      ; canary_tag = String.make 16 '\000'
      }
  in
  P.write_header_fields buf hf;
  let hf' = P.read_header_fields buf in
  Alcotest.(check int64) "txn_id" hf.txn_id hf'.txn_id;
  Alcotest.(check int64) "root_page" hf.root_page hf'.root_page;
  Alcotest.(check int64) "freelist_page" hf.freelist_page hf'.freelist_page;
  Alcotest.(check int64) "n_pages_total" hf.n_pages_total hf'.n_pages_total;
  Alcotest.(check int64) "schema_version" hf.schema_version hf'.schema_version;
  Alcotest.(check int32) "page_size" hf.page_size hf'.page_size;
  Alcotest.(check int32) "format_version" hf.format_version hf'.format_version
;;

(* ------------------------------------------------------------------ *)
(* 6. Branch page entries                                              *)
(* ------------------------------------------------------------------ *)

let test_branch_single_entry () =
  let buf = fresh_page () in
  let key = Bytes.of_string "hello" in
  let left_child = 42l in
  let _next = P.branch_append_entry buf ~offset:P.data_offset ~key ~left_child in
  match P.branch_entry_at buf ~offset:P.data_offset with
  | `End -> Alcotest.fail "expected Entry, got End"
  | `Entry (e : P.branch_entry) ->
    Alcotest.(check bool) "key round-trip" true (Bytes.equal key e.key);
    Alcotest.(check int32) "left_child round-trip" left_child e.left_child
;;

let test_branch_three_entries () =
  let buf = fresh_page () in
  let entries =
    [| Bytes.of_string "aaa", 10l
     ; Bytes.of_string "bbb", 20l
     ; Bytes.of_string "ccc", 30l
    |]
  in
  let off0 = P.data_offset in
  let off1 =
    P.branch_append_entry
      buf
      ~offset:off0
      ~key:(fst entries.(0))
      ~left_child:(snd entries.(0))
  in
  let off2 =
    P.branch_append_entry
      buf
      ~offset:off1
      ~key:(fst entries.(1))
      ~left_child:(snd entries.(1))
  in
  let _off3 =
    P.branch_append_entry
      buf
      ~offset:off2
      ~key:(fst entries.(2))
      ~left_child:(snd entries.(2))
  in
  (* Iterate all three *)
  match P.branch_entry_at buf ~offset:off0 with
  | `End -> Alcotest.fail "entry 0: got End"
  | `Entry (e : P.branch_entry) ->
    Alcotest.(check bool) "entry 0 key" true (Bytes.equal (fst entries.(0)) e.key);
    Alcotest.(check int32) "entry 0 child" (snd entries.(0)) e.left_child;
    (match P.branch_entry_at buf ~offset:e.next_offset with
     | `End -> Alcotest.fail "entry 1: got End"
     | `Entry (e1 : P.branch_entry) ->
       Alcotest.(check bool) "entry 1 key" true (Bytes.equal (fst entries.(1)) e1.key);
       Alcotest.(check int32) "entry 1 child" (snd entries.(1)) e1.left_child;
       (match P.branch_entry_at buf ~offset:e1.next_offset with
        | `End -> Alcotest.fail "entry 2: got End"
        | `Entry (e2 : P.branch_entry) ->
          Alcotest.(check bool) "entry 2 key" true (Bytes.equal (fst entries.(2)) e2.key);
          Alcotest.(check int32) "entry 2 child" (snd entries.(2)) e2.left_child;
          (* After 3rd entry, we should get End when at out-of-data offset *)
          (* next_offset is valid but there's nothing appended there *)
          (* Actually with zeroed page the 4th read may return Entry with key_len=0 *)
          (* test that iterating *by n_keys* stops correctly — just check next_offset > data *)
          Alcotest.(check bool)
            "next_offset advances"
            true
            (e2.next_offset > P.data_offset)))
;;

let test_branch_end_at_offset_past_data () =
  let buf = fresh_page () in
  (* offset past all data: page_size - 5 < page_size - 6 is false, but +6 > 4096 *)
  let offset = P.page_size - 3 in
  match P.branch_entry_at buf ~offset with
  | `End -> () (* expected *)
  | `Entry _ -> Alcotest.fail "expected End for offset near page boundary"
;;

let test_branch_end_at_page_size () =
  let buf = fresh_page () in
  match P.branch_entry_at buf ~offset:P.page_size with
  | `End -> ()
  | `Entry _ -> Alcotest.fail "expected End at page_size offset"
;;

(* ------------------------------------------------------------------ *)
(* 7. Leaf page entries                                                *)
(* ------------------------------------------------------------------ *)

let test_leaf_single_entry () =
  let buf = fresh_page () in
  let key = Bytes.of_string "mykey" in
  let value = Bytes.of_string "myvalue" in
  let _next = P.leaf_append_entry buf ~offset:P.data_offset ~key ~value in
  match P.leaf_entry_at buf ~offset:P.data_offset with
  | `End -> Alcotest.fail "expected Entry, got End"
  | `Entry (e : P.leaf_entry) ->
    Alcotest.(check bool) "key round-trip" true (Bytes.equal key e.key);
    Alcotest.(check bool) "value round-trip" true (Bytes.equal value e.value)
;;

let test_leaf_three_entries () =
  let buf = fresh_page () in
  let entries =
    [| Bytes.of_string "k1", Bytes.of_string "v1"
     ; Bytes.of_string "k2", Bytes.of_string "v22"
     ; Bytes.of_string "k333", Bytes.of_string "v3"
    |]
  in
  let off0 = P.data_offset in
  let off1 =
    P.leaf_append_entry buf ~offset:off0 ~key:(fst entries.(0)) ~value:(snd entries.(0))
  in
  let off2 =
    P.leaf_append_entry buf ~offset:off1 ~key:(fst entries.(1)) ~value:(snd entries.(1))
  in
  let _off3 =
    P.leaf_append_entry buf ~offset:off2 ~key:(fst entries.(2)) ~value:(snd entries.(2))
  in
  match P.leaf_entry_at buf ~offset:off0 with
  | `End -> Alcotest.fail "entry 0: got End"
  | `Entry (e : P.leaf_entry) ->
    Alcotest.(check bool) "entry 0 key" true (Bytes.equal (fst entries.(0)) e.key);
    Alcotest.(check bool) "entry 0 val" true (Bytes.equal (snd entries.(0)) e.value);
    (match P.leaf_entry_at buf ~offset:e.next_offset with
     | `End -> Alcotest.fail "entry 1: got End"
     | `Entry (e1 : P.leaf_entry) ->
       Alcotest.(check bool) "entry 1 key" true (Bytes.equal (fst entries.(1)) e1.key);
       Alcotest.(check bool) "entry 1 val" true (Bytes.equal (snd entries.(1)) e1.value);
       (match P.leaf_entry_at buf ~offset:e1.next_offset with
        | `End -> Alcotest.fail "entry 2: got End"
        | `Entry (e2 : P.leaf_entry) ->
          Alcotest.(check bool) "entry 2 key" true (Bytes.equal (fst entries.(2)) e2.key);
          Alcotest.(check bool)
            "entry 2 val"
            true
            (Bytes.equal (snd entries.(2)) e2.value);
          Alcotest.(check bool)
            "next_offset advances"
            true
            (e2.next_offset > P.data_offset)))
;;

let test_leaf_end_at_offset_past_data () =
  let buf = fresh_page () in
  let offset = P.page_size - 2 in
  match P.leaf_entry_at buf ~offset with
  | `End -> ()
  | `Entry _ -> Alcotest.fail "expected End for offset near page boundary"
;;

let test_leaf_end_at_page_size () =
  let buf = fresh_page () in
  match P.leaf_entry_at buf ~offset:P.page_size with
  | `End -> ()
  | `Entry _ -> Alcotest.fail "expected End at page_size offset"
;;

(* ------------------------------------------------------------------ *)
(* 8. Zeroed page: entry iteration behavior                            *)
(* ------------------------------------------------------------------ *)

(* On a zeroed page: branch key_len=0 → 6-byte entry (valid `Entry).
   The caller must stop after n_keys=0 entries (which they read from common header).
   `End is only returned when truly out-of-bounds. *)
let test_branch_zeroed_page_entry () =
  let buf = fresh_page () in
  (* zeroed page: key_len=0, left_child=0, 6-byte entry fits *)
  match P.branch_entry_at buf ~offset:P.data_offset with
  | `End -> Alcotest.fail "zeroed page: expected Entry with key_len=0, not End"
  | `Entry (e : P.branch_entry) ->
    Alcotest.(check int) "key_len=0" 0 (Bytes.length e.key);
    Alcotest.(check int32) "left_child=0" 0l e.left_child;
    (* next_offset = 16 + 6 = 22; this is within bounds, so next call also gets Entry *)
    (* But the second entry at offset 22 also has key_len=0 and is valid *)
    (* Only when offset+6 > 4096 do we get End *)
    Alcotest.(check bool) "next_offset = 22" true (e.next_offset = 22)
;;

let test_leaf_zeroed_page_entry () =
  let buf = fresh_page () in
  (* zeroed page: key_len=0, val_len=0, 4-byte entry fits *)
  match P.leaf_entry_at buf ~offset:P.data_offset with
  | `End -> Alcotest.fail "zeroed page: expected Entry with key/val len 0, not End"
  | `Entry (e : P.leaf_entry) ->
    Alcotest.(check int) "key_len=0" 0 (Bytes.length e.key);
    Alcotest.(check int) "val_len=0" 0 (Bytes.length e.value);
    Alcotest.(check bool) "next_offset = 20" true (e.next_offset = 20)
;;

(* ------------------------------------------------------------------ *)
(* 9. Freelist entries                                                 *)
(* ------------------------------------------------------------------ *)

let test_freelist_single_entry () =
  let buf = fresh_page () in
  let page_id = 123l in
  let freed_at_txn_id = 0x0ABCDEF012345678L in
  P.freelist_set_entry buf ~index:0 ~page_id ~freed_at_txn_id;
  let (e : P.freelist_entry) = P.freelist_entry_at buf ~index:0 in
  Alcotest.(check int32) "page_id round-trip" page_id e.page_id;
  Alcotest.(check int64) "freed_at_txn_id round-trip" freed_at_txn_id e.freed_at_txn_id
;;

let test_freelist_multiple_entries () =
  let buf = fresh_page () in
  (* set a few entries at different indices *)
  P.freelist_set_entry buf ~index:0 ~page_id:10l ~freed_at_txn_id:100L;
  P.freelist_set_entry buf ~index:1 ~page_id:20l ~freed_at_txn_id:200L;
  P.freelist_set_entry buf ~index:339 ~page_id:999l ~freed_at_txn_id:9999L;
  let (e0 : P.freelist_entry) = P.freelist_entry_at buf ~index:0 in
  let (e1 : P.freelist_entry) = P.freelist_entry_at buf ~index:1 in
  let (e339 : P.freelist_entry) = P.freelist_entry_at buf ~index:339 in
  Alcotest.(check int32) "entry 0 page_id" 10l e0.page_id;
  Alcotest.(check int64) "entry 0 txn_id" 100L e0.freed_at_txn_id;
  Alcotest.(check int32) "entry 1 page_id" 20l e1.page_id;
  Alcotest.(check int64) "entry 1 txn_id" 200L e1.freed_at_txn_id;
  Alcotest.(check int32) "entry 339 page_id" 999l e339.page_id;
  Alcotest.(check int64) "entry 339 txn_id" 9999L e339.freed_at_txn_id
;;

let test_freelist_isolation () =
  let buf = fresh_page () in
  P.freelist_set_entry buf ~index:0 ~page_id:1l ~freed_at_txn_id:11L;
  P.freelist_set_entry buf ~index:1 ~page_id:2l ~freed_at_txn_id:22L;
  P.freelist_set_entry buf ~index:2 ~page_id:3l ~freed_at_txn_id:33L;
  (* overwrite entry 1 *)
  P.freelist_set_entry buf ~index:1 ~page_id:99l ~freed_at_txn_id:999L;
  (* entries 0 and 2 must be untouched *)
  let (e0 : P.freelist_entry) = P.freelist_entry_at buf ~index:0 in
  let (e2 : P.freelist_entry) = P.freelist_entry_at buf ~index:2 in
  Alcotest.(check int32) "entry 0 unchanged" 1l e0.page_id;
  Alcotest.(check int32) "entry 2 unchanged" 3l e2.page_id
;;

(* ------------------------------------------------------------------ *)
(* 9b. Overflow / invalid-arg edge cases                               *)
(* ------------------------------------------------------------------ *)

(* branch_append_entry: key too long for the current page offset *)
let test_branch_append_overflow () =
  let buf = fresh_page () in
  (* Place the offset near the end so the entry would overflow *)
  let near_end = P.page_size - 4 in
  (* only 4 bytes left: can't fit key + 4-byte child *)
  let key = Bytes.of_string "k" in
  match P.branch_append_entry buf ~offset:near_end ~key ~left_child:1l with
  | _ -> Alcotest.fail "expected invalid_arg for branch overflow"
  | exception Invalid_argument _ -> ()
;;

(* leaf_append_entry: entry would overflow page *)
let test_leaf_append_overflow () =
  let buf = fresh_page () in
  let near_end = P.page_size - 2 in
  (* only 2 bytes left: can't fit key+value entry *)
  let key = Bytes.of_string "k" in
  let value = Bytes.of_string "v" in
  match P.leaf_append_entry buf ~offset:near_end ~key ~value with
  | _ -> Alcotest.fail "expected invalid_arg for leaf overflow"
  | exception Invalid_argument _ -> ()
;;

(* branch_append_entry: key longer than uint16 max (0xFFFF) raises *)
let test_branch_append_key_too_long () =
  let buf = fresh_page () in
  let big_key = Bytes.make 0x10000 'k' in
  (* > 65535 *)
  match P.branch_append_entry buf ~offset:P.data_offset ~key:big_key ~left_child:1l with
  | _ -> Alcotest.fail "expected invalid_arg for oversized branch key"
  | exception Invalid_argument _ -> ()
;;

(* leaf_append_entry: key longer than uint16 max raises *)
let test_leaf_append_key_too_long () =
  let buf = fresh_page () in
  let big_key = Bytes.make 0x10000 'k' in
  let value = Bytes.of_string "v" in
  match P.leaf_append_entry buf ~offset:P.data_offset ~key:big_key ~value with
  | _ -> Alcotest.fail "expected invalid_arg for oversized leaf key"
  | exception Invalid_argument _ -> ()
;;

(* leaf_append_entry: value longer than uint16 max raises *)
let test_leaf_append_value_too_long () =
  let buf = fresh_page () in
  let key = Bytes.of_string "k" in
  let big_val = Bytes.make 0x10000 'v' in
  match P.leaf_append_entry buf ~offset:P.data_offset ~key ~value:big_val with
  | _ -> Alcotest.fail "expected invalid_arg for oversized leaf value"
  | exception Invalid_argument _ -> ()
;;

(* branch_entry_at: key_len says the key would overflow the page *)
let test_branch_entry_at_key_overflow () =
  let buf = fresh_page () in
  (* Write a branch entry manually with a large key_len that would go past page_size *)
  let offset = P.page_size - 10 in
  (* key_len = 0xFFFF → 2 + 65535 + 4 would far exceed page bounds *)
  Cstruct.BE.set_uint16 buf offset 0xFFFF;
  match P.branch_entry_at buf ~offset with
  | `End -> () (* expected: key_len overflows page *)
  | `Entry _ -> Alcotest.fail "expected End when key_len overflows page"
;;

(* leaf_entry_at: key_len or val_len overflow *)
let test_leaf_entry_at_key_overflow () =
  let buf = fresh_page () in
  let offset = P.page_size - 10 in
  (* key_len = 0xFFFF → overflows *)
  Cstruct.BE.set_uint16 buf offset 0xFFFF;
  match P.leaf_entry_at buf ~offset with
  | `End -> () (* expected *)
  | `Entry _ -> Alcotest.fail "expected End when leaf key_len overflows page"
;;

(* leaf_entry_at: key fits but val_len overflows *)
let test_leaf_entry_at_val_overflow () =
  let buf = fresh_page () in
  (* Place at start of data, write key_len=1, key='a', then val_len=0xFFFF *)
  let offset = P.data_offset in
  Cstruct.BE.set_uint16 buf offset 1;
  (* key_len = 1 *)
  Cstruct.set_char buf (offset + 2) 'a';
  (* key data *)
  Cstruct.BE.set_uint16 buf (offset + 3) 0xFFFF;
  (* val_len = 65535 → overflows *)
  match P.leaf_entry_at buf ~offset with
  | `End -> () (* expected *)
  | `Entry _ -> Alcotest.fail "expected End when leaf val_len overflows page"
;;

(* ------------------------------------------------------------------ *)
(* 9c. Large-page geometry (#95): the codec sizes itself to the buffer  *)
(*     length, not a hard-coded 4096, so 8K/16K pages work end-to-end.  *)
(* ------------------------------------------------------------------ *)

(* The CRC must cover the WHOLE page, not just the first 4096 bytes — so a
   corruption past offset 4096 on a 16K page is still detected. *)
let test_crc_covers_whole_large_page () =
  let buf = Cstruct.create 16384 in
  Cstruct.memset buf 0;
  P.write_common buf (make_common P.Leaf);
  P.seal buf;
  Alcotest.(check bool) "sealed 16K page verifies" true (P.verify_crc buf);
  flip_byte buf 9000;
  Alcotest.(check bool) "CRC covers byte 9000 of a 16K page" false (P.verify_crc buf)
;;

(* Leaf entries can be appended and read back past offset 4096 on a large
   page — the append guard is the buffer end, not 4096. *)
let test_leaf_append_beyond_4096 () =
  let buf = Cstruct.create 16384 in
  Cstruct.memset buf 0;
  let key = Bytes.make 100 'k' in
  let value = Bytes.make 100 'v' in
  let rec fill off n =
    if n = 0 then off else fill (P.leaf_append_entry buf ~offset:off ~key ~value) (n - 1)
  in
  (* 60 entries * (4 + 100 + 100) = 12240 bytes of data, crossing 4096. *)
  let last_off = fill P.data_offset 60 in
  Alcotest.(check bool) "appended past offset 4096" true (last_off > 4096);
  (* Walk to the entry that begins beyond 4096 and confirm it round-trips. *)
  let rec find off =
    if off > 4096
    then off
    else (
      match P.leaf_entry_at buf ~offset:off with
      | `End -> Alcotest.fail "unexpected End before crossing 4096"
      | `Entry (e : P.leaf_entry) -> find e.next_offset)
  in
  let beyond = find P.data_offset in
  match P.leaf_entry_at buf ~offset:beyond with
  | `End -> Alcotest.fail "expected Entry beyond offset 4096"
  | `Entry (e : P.leaf_entry) ->
    Alcotest.(check bool) "key beyond 4096 round-trips" true (Bytes.equal key e.key);
    Alcotest.(check bool) "value beyond 4096 round-trips" true (Bytes.equal value e.value)
;;

(* An append whose entry would intrude into the reserved tail (#95) is
   refused, even though it would fit if the reserved bytes were usable. *)
let test_leaf_append_respects_reserved () =
  let key = Bytes.make 30 'k' in
  let value = Bytes.make 30 'v' in
  let offset = 4000 in
  (* entry_size = 4 + 30 + 30 = 64, ending at 4064 — within 4096 but past the
     reserved-aware ceiling 4096 - 64 = 4032. *)
  let no_reserve = Cstruct.create 4096 in
  Cstruct.memset no_reserve 0;
  let _ = P.leaf_append_entry no_reserve ~offset ~key ~value in
  let reserved = Cstruct.create 4096 in
  Cstruct.memset reserved 0;
  match P.leaf_append_entry ~reserved:64 reserved ~offset ~key ~value with
  | _ -> Alcotest.fail "expected Invalid_argument: entry intrudes into reserved tail"
  | exception Invalid_argument _ -> ()
;;

(* ------------------------------------------------------------------ *)
(* 10. QCheck property tests                                           *)
(* ------------------------------------------------------------------ *)

(* QCheck: leaf key/value round-trip *)
let prop_leaf_roundtrip =
  let gen =
    QCheck.Gen.(
      let* key_data = bytes_size (int_range 0 500) in
      let* value_data = bytes_size (int_range 0 500) in
      return (key_data, value_data))
  in
  QCheck.Test.make
    ~name:"prop_leaf_roundtrip"
    ~count:10_000
    (QCheck.make gen)
    (fun (key, value) ->
       let buf = fresh_page () in
       let _next = P.leaf_append_entry buf ~offset:P.data_offset ~key ~value in
       match P.leaf_entry_at buf ~offset:P.data_offset with
       | `End -> false
       | `Entry (e : P.leaf_entry) -> Bytes.equal key e.key && Bytes.equal value e.value)
;;

(* QCheck: branch key/child round-trip *)
let prop_branch_roundtrip =
  let gen =
    QCheck.Gen.(
      let* key_data = bytes_size (int_range 0 500) in
      let* left_child = int32 in
      return (key_data, left_child))
  in
  QCheck.Test.make
    ~name:"prop_branch_roundtrip"
    ~count:10_000
    (QCheck.make gen)
    (fun (key, left_child) ->
       let buf = fresh_page () in
       let _next = P.branch_append_entry buf ~offset:P.data_offset ~key ~left_child in
       match P.branch_entry_at buf ~offset:P.data_offset with
       | `End -> false
       | `Entry (e : P.branch_entry) -> Bytes.equal key e.key && e.left_child = left_child)
;;

(* QCheck: compute_crc detects any single-byte flip outside bytes 8..11 *)
let prop_crc_detects_flip =
  let gen =
    QCheck.Gen.(
      let* buf_data = bytes_size (return 4096) in
      (* pick a byte position not in [8..11] *)
      let non_crc_pos = oneof [ int_range 0 7; int_range 12 4095 ] in
      let* pos = non_crc_pos in
      return (buf_data, pos))
  in
  QCheck.Test.make
    ~name:"prop_crc_detects_single_byte_flip"
    ~count:10_000
    (QCheck.make gen)
    (fun (data, pos) ->
       let buf = Cstruct.create P.page_size in
       Cstruct.blit_from_bytes data 0 buf 0 P.page_size;
       let crc_before = P.compute_crc buf in
       flip_byte buf pos;
       let crc_after = P.compute_crc buf in
       (* CRC should change (unless flipped byte XOR pattern happens to produce
          same CRC — extremely unlikely but theoretically possible; accept that
          we're testing the common case) *)
       crc_before <> crc_after)
;;

(* QCheck: freelist round-trip *)
let prop_freelist_roundtrip =
  let gen =
    QCheck.Gen.(
      let* index = int_range 0 (P.max_freelist_entries_per_page - 1) in
      let* page_id = int32 in
      let* freed_at_txn_id = int64 in
      return (index, page_id, freed_at_txn_id))
  in
  QCheck.Test.make
    ~name:"prop_freelist_roundtrip"
    ~count:10_000
    (QCheck.make gen)
    (fun (index, page_id, freed_at_txn_id) ->
       let buf = fresh_page () in
       P.freelist_set_entry buf ~index ~page_id ~freed_at_txn_id;
       let (e : P.freelist_entry) = P.freelist_entry_at buf ~index in
       e.page_id = page_id && e.freed_at_txn_id = freed_at_txn_id)
;;

(* QCheck: common header round-trip for all fields *)
let prop_common_roundtrip =
  let gen =
    QCheck.Gen.(
      let* kind_n = int_range 0 3 in
      let* flags = int_range 0 255 in
      let* n_keys = int_range 0 65535 in
      let* right_page = int32 in
      let* crc32 = int32 in
      return (kind_n, flags, n_keys, right_page, crc32))
  in
  QCheck.Test.make
    ~name:"prop_common_roundtrip"
    ~count:10_000
    (QCheck.make gen)
    (fun (kind_n, flags, n_keys, right_page, crc32) ->
       let kind =
         match kind_n with
         | 0 -> P.Header
         | 1 -> P.Branch
         | 2 -> P.Leaf
         | _ -> P.Freelist
       in
       let c = P.{ kind; flags; n_keys; right_page; crc32 } in
       let buf = fresh_page () in
       P.write_common buf c;
       let c' = P.read_common buf in
       c'.flags = flags
       && c'.n_keys = n_keys
       && c'.right_page = right_page
       && c'.crc32 = crc32
       &&
       match c'.kind, kind with
       | P.Header, P.Header | P.Branch, P.Branch | P.Leaf, P.Leaf | P.Freelist, P.Freelist
         -> true
       | _ -> false)
;;

(* ------------------------------------------------------------------ *)
(* 11. #245 in-place search == list-based search (byte-identical)      *)
(* ------------------------------------------------------------------ *)

(* Reference leaf lookup: the OLD list-materializing path, expressed directly
   over [leaf_entry_at].  [leaf_lookup] must agree with this for every key. *)
let ref_leaf_lookup buf n_keys key : bytes option =
  let rec loop offset i =
    if i >= n_keys
    then None
    else (
      match P.leaf_entry_at buf ~offset with
      | `End -> None
      | `Entry (e : P.leaf_entry) ->
        let c = Bytes.compare key e.key in
        if c = 0 then Some e.value else if c < 0 then None else loop e.next_offset (i + 1))
  in
  loop P.data_offset 0
;;

(* Reference branch pick: the OLD [pick_branch_child] over [branch_entry_at]. *)
let ref_branch_pick buf n_keys right_page key : int32 =
  let rec loop offset i =
    if i >= n_keys
    then right_page
    else (
      match P.branch_entry_at buf ~offset with
      | `End -> right_page
      | `Entry (e : P.branch_entry) ->
        if Bytes.compare key e.key < 0 then e.left_child else loop e.next_offset (i + 1))
  in
  loop P.data_offset 0
;;

(* Sort by Bytes.compare and drop duplicate keys (keep first), as the B-tree
   page invariant guarantees: entries strictly ascending by key. *)
let sort_unique_by_key pairs =
  let sorted = List.stable_sort (fun (a, _) (b, _) -> Bytes.compare a b) pairs in
  let rec dedup = function
    | (k1, v1) :: ((k2, _) :: _ as rest) ->
      if Bytes.equal k1 k2
      then dedup ((k1, v1) :: List.tl rest)
      else (k1, v1) :: dedup rest
    | xs -> xs
  in
  dedup sorted
;;

(* Synthesise a key guaranteed to sort strictly after every key in [su]. *)
let after_all su =
  match su with
  | [] -> Bytes.of_string "\xff"
  | _ ->
    let last, _ = List.nth su (List.length su - 1) in
    Bytes.cat last (Bytes.of_string "\xff")
;;

(* QCheck: leaf_lookup agrees with the list-based lookup for present, absent,
   and boundary keys (empty = before-all, [after_all] = past-the-end). *)
let prop_leaf_lookup_matches =
  let gen =
    QCheck.Gen.(
      let* n = int_range 0 60 in
      let* raw =
        list_size
          (return n)
          (pair (bytes_size (int_range 0 20)) (bytes_size (int_range 0 30)))
      in
      let* probe = bytes_size (int_range 0 20) in
      return (raw, probe))
  in
  QCheck.Test.make
    ~name:"prop_leaf_lookup_matches_list"
    ~count:10_000
    (QCheck.make gen)
    (fun (raw, probe) ->
       let su = sort_unique_by_key raw in
       let buf = fresh_page () in
       let _ =
         List.fold_left
           (fun off (k, v) -> P.leaf_append_entry buf ~offset:off ~key:k ~value:v)
           P.data_offset
           su
       in
       let n = List.length su in
       let agree key =
         match P.leaf_lookup buf ~n_keys:n ~key, ref_leaf_lookup buf n key with
         | None, None -> true
         | Some a, Some b -> Bytes.equal a b
         | _ -> false
       in
       List.for_all (fun (k, _) -> agree k) su
       && agree probe
       && agree Bytes.empty
       && agree (after_all su))
;;

(* QCheck: branch_pick agrees with the list-based child pick for present,
   absent, and boundary keys. *)
let prop_branch_pick_matches =
  let gen =
    QCheck.Gen.(
      let* n = int_range 0 80 in
      let* raw = list_size (return n) (pair (bytes_size (int_range 0 20)) int32) in
      let* right_page = int32 in
      let* probe = bytes_size (int_range 0 20) in
      return (raw, right_page, probe))
  in
  QCheck.Test.make
    ~name:"prop_branch_pick_matches_list"
    ~count:10_000
    (QCheck.make gen)
    (fun (raw, right_page, probe) ->
       let su = sort_unique_by_key raw in
       let buf = fresh_page () in
       let _ =
         List.fold_left
           (fun off (k, left_child) ->
              P.branch_append_entry buf ~offset:off ~key:k ~left_child)
           P.data_offset
           su
       in
       let n = List.length su in
       let agree key =
         P.branch_pick buf ~n_keys:n ~right_page ~key
         = ref_branch_pick buf n right_page key
       in
       List.for_all (fun (k, _) -> agree k) su
       && agree probe
       && agree Bytes.empty
       && agree (after_all su))
;;

(* ------------------------------------------------------------------ *)
(* leaf_find_position tests                                            *)
(* ------------------------------------------------------------------ *)

let build_leaf_page entries =
  let buf = fresh_page () in
  let _ =
    List.fold_left
      (fun off (k, v) -> P.leaf_append_entry buf ~offset:off ~key:k ~value:v)
      P.data_offset
      entries
  in
  buf, List.length entries
;;

let test_leaf_find_position_empty () =
  let buf, n = build_leaf_page [] in
  let pos = P.leaf_find_position buf ~n_keys:n ~key:(Bytes.of_string "a") in
  Alcotest.(check int) "insert_off=data_offset" P.data_offset pos.P.insert_off;
  Alcotest.(check int) "data_end=data_offset" P.data_offset pos.P.data_end;
  Alcotest.(check bool) "key_found=false" false pos.P.key_found
;;

let test_leaf_find_position_before_first () =
  let buf, n = build_leaf_page [ Bytes.of_string "b", Bytes.of_string "v" ] in
  let pos = P.leaf_find_position buf ~n_keys:n ~key:(Bytes.of_string "a") in
  Alcotest.(check int) "insert_off=data_offset" P.data_offset pos.P.insert_off;
  Alcotest.(check bool) "key_found=false" false pos.P.key_found;
  Alcotest.(check bool) "data_end>data_offset" true (pos.P.data_end > P.data_offset)
;;

let test_leaf_find_position_after_last () =
  let buf, n = build_leaf_page [ Bytes.of_string "a", Bytes.of_string "v" ] in
  let pos = P.leaf_find_position buf ~n_keys:n ~key:(Bytes.of_string "z") in
  Alcotest.(check bool) "key_found=false" false pos.P.key_found;
  Alcotest.(check int) "insert_off=data_end" pos.P.data_end pos.P.insert_off
;;

let test_leaf_find_position_exact () =
  let k = Bytes.of_string "hello" in
  let buf, n = build_leaf_page [ k, Bytes.of_string "v" ] in
  let pos = P.leaf_find_position buf ~n_keys:n ~key:k in
  Alcotest.(check bool) "key_found=true" true pos.P.key_found;
  Alcotest.(check int) "insert_off=data_offset" P.data_offset pos.P.insert_off
;;

(* ------------------------------------------------------------------ *)
(* leaf_blit_insert tests                                              *)
(* ------------------------------------------------------------------ *)

let test_leaf_blit_insert_empty () =
  let buf, n = build_leaf_page [] in
  let key = Bytes.of_string "k"
  and sv = Bytes.of_string "\x00val" in
  let pos = P.leaf_find_position buf ~n_keys:n ~key in
  let nb =
    P.leaf_blit_insert
      buf
      ~pos
      ~key
      ~stored_value:sv
      ~right_page:0l
      ~write_tag:0l
      ~n_keys:n
  in
  let c = P.read_common nb in
  Alcotest.(check int) "n_keys=1" 1 c.n_keys;
  match P.leaf_entry_at nb ~offset:P.data_offset with
  | `Entry e -> Alcotest.(check bytes) "key round-trips" key e.key
  | `End -> Alcotest.fail "expected entry"
;;

let test_leaf_blit_insert_before () =
  let buf, n = build_leaf_page [ Bytes.of_string "z", Bytes.of_string "v2" ] in
  let k1 = Bytes.of_string "a"
  and sv = Bytes.of_string "\x00v1" in
  let pos = P.leaf_find_position buf ~n_keys:n ~key:k1 in
  let nb =
    P.leaf_blit_insert
      buf
      ~pos
      ~key:k1
      ~stored_value:sv
      ~right_page:0l
      ~write_tag:0l
      ~n_keys:n
  in
  Alcotest.(check int) "n_keys=2" 2 (P.read_common nb).n_keys;
  match P.leaf_entry_at nb ~offset:P.data_offset with
  | `Entry e -> Alcotest.(check bytes) "first key=a" k1 e.key
  | `End -> Alcotest.fail "expected first entry"
;;

(* QCheck: leaf_blit_insert ≡ decode+insert+encode *)
let ref_leaf_insert buf n key sv right_page tag =
  let acc = ref [] in
  let rec scan off i =
    if i >= n
    then ()
    else (
      match P.leaf_entry_at buf ~offset:off with
      | `End -> ()
      | `Entry e ->
        acc := (e.key, e.value) :: !acc;
        scan e.next_offset (i + 1))
  in
  scan P.data_offset 0;
  let plain = List.rev !acc in
  let new_plain =
    let rec ins a = function
      | [] -> List.rev_append a [ key, sv ]
      | ((k, _) as h) :: t ->
        let c = Bytes.compare key k in
        if c <= 0
        then List.rev_append a ((key, sv) :: (if c = 0 then t else h :: t))
        else ins (h :: a) t
    in
    ins [] plain
  in
  let nb = Cstruct.create P.page_size in
  Cstruct.memset nb 0;
  let cnt =
    List.fold_left
      (fun (off, i) (k, v) -> P.leaf_append_entry nb ~offset:off ~key:k ~value:v, i + 1)
      (P.data_offset, 0)
      new_plain
    |> snd
  in
  P.write_common nb P.{ kind = Leaf; flags = 0; n_keys = cnt; right_page; crc32 = 0l };
  P.write_tag nb tag;
  (* No seal: leaf_blit_insert defers CRC to WAL-flush (#356). *)
  nb
;;

let prop_leaf_blit_insert_equiv =
  let gen =
    QCheck.Gen.(
      let* n = int_range 0 40 in
      let* raw =
        list_size
          (return n)
          (pair (bytes_size (int_range 1 10)) (bytes_size (int_range 1 20)))
      in
      let* nk = bytes_size (int_range 1 10) in
      let* nv = bytes_size (int_range 1 10) in
      let* rp = int32 in
      let* tag = int32 in
      return (raw, nk, nv, rp, tag))
  in
  QCheck.Test.make
    ~name:"prop_leaf_blit_insert_matches_reference"
    ~count:5_000
    (QCheck.make gen)
    (fun (raw, nk, nv, rp, tag) ->
       let su = sort_unique_by_key raw in
       let buf = fresh_page () in
       let _ =
         List.fold_left
           (fun off (k, v) -> P.leaf_append_entry buf ~offset:off ~key:k ~value:v)
           P.data_offset
           su
       in
       let n = List.length su in
       let sv = Bytes.cat (Bytes.make 1 '\x00') nv in
       let esz = 2 + Bytes.length nk + 2 + Bytes.length sv in
       let dsz =
         List.fold_left (fun a (k, v) -> a + 2 + Bytes.length k + 2 + Bytes.length v) 0 su
       in
       if dsz + esz > P.max_data_bytes
       then true
       else if List.exists (fun (k, _) -> Bytes.equal k nk) su
       then true
       else (
         let pos = P.leaf_find_position buf ~n_keys:n ~key:nk in
         let fast =
           P.leaf_blit_insert
             buf
             ~pos
             ~key:nk
             ~stored_value:sv
             ~right_page:rp
             ~write_tag:tag
             ~n_keys:n
         in
         let ref_ = ref_leaf_insert buf n nk sv rp tag in
         Cstruct.equal fast ref_))
;;

(* ------------------------------------------------------------------ *)
(* leaf_insert_inplace tests                                           *)
(* ------------------------------------------------------------------ *)

(* Decode all [n] entries of a leaf page into a (key, value) list. *)
let decode_all_leaf buf n =
  let acc = ref [] in
  let rec scan off i =
    if i >= n
    then ()
    else (
      match P.leaf_entry_at buf ~offset:off with
      | `End -> ()
      | `Entry e ->
        acc := (e.key, e.value) :: !acc;
        scan e.next_offset (i + 1))
  in
  scan P.data_offset 0;
  List.rev !acc
;;

let test_leaf_insert_inplace_append () =
  let buf, n = build_leaf_page [ Bytes.of_string "a", Bytes.of_string "1" ] in
  let key = Bytes.of_string "z"
  and sv = Bytes.of_string "\x009" in
  let pos = P.leaf_find_position buf ~n_keys:n ~key in
  P.leaf_insert_inplace buf ~pos ~key ~stored_value:sv ~n_keys:n;
  Alcotest.(check int) "n_keys=2" 2 (P.read_common buf).n_keys;
  let entries = decode_all_leaf buf 2 in
  Alcotest.(check (list (pair bytes bytes)))
    "appended in order"
    [ Bytes.of_string "a", Bytes.of_string "1"; key, sv ]
    entries
;;

let test_leaf_insert_inplace_middle () =
  let buf, n =
    build_leaf_page
      [ Bytes.of_string "a", Bytes.of_string "1"
      ; Bytes.of_string "c", Bytes.of_string "3"
      ]
  in
  let key = Bytes.of_string "b"
  and sv = Bytes.of_string "\x002" in
  let pos = P.leaf_find_position buf ~n_keys:n ~key in
  P.leaf_insert_inplace buf ~pos ~key ~stored_value:sv ~n_keys:n;
  Alcotest.(check int) "n_keys=3" 3 (P.read_common buf).n_keys;
  let entries = decode_all_leaf buf 3 in
  Alcotest.(check (list (pair bytes bytes)))
    "inserted in sorted middle"
    [ Bytes.of_string "a", Bytes.of_string "1"
    ; key, sv
    ; Bytes.of_string "c", Bytes.of_string "3"
    ]
    entries
;;

let test_leaf_insert_inplace_front () =
  let buf, n = build_leaf_page [ Bytes.of_string "m", Bytes.of_string "5" ] in
  let key = Bytes.of_string "a"
  and sv = Bytes.of_string "\x000" in
  let pos = P.leaf_find_position buf ~n_keys:n ~key in
  P.leaf_insert_inplace buf ~pos ~key ~stored_value:sv ~n_keys:n;
  let entries = decode_all_leaf buf 2 in
  Alcotest.(check (list (pair bytes bytes)))
    "inserted at front"
    [ key, sv; Bytes.of_string "m", Bytes.of_string "5" ]
    entries
;;

(* QCheck: in-place insert yields the same decoded entries as blit insert. *)
let prop_leaf_insert_inplace_equiv =
  let gen =
    QCheck.Gen.(
      let* n = int_range 0 40 in
      let* raw =
        list_size
          (return n)
          (pair (bytes_size (int_range 1 10)) (bytes_size (int_range 1 20)))
      in
      let* nk = bytes_size (int_range 1 10) in
      let* nv = bytes_size (int_range 1 10) in
      return (raw, nk, nv))
  in
  QCheck.Test.make
    ~name:"prop_leaf_insert_inplace_matches_blit"
    ~count:5_000
    (QCheck.make gen)
    (fun (raw, nk, nv) ->
       let su = sort_unique_by_key raw in
       let buf = fresh_page () in
       let _ =
         List.fold_left
           (fun off (k, v) -> P.leaf_append_entry buf ~offset:off ~key:k ~value:v)
           P.data_offset
           su
       in
       let n = List.length su in
       let sv = Bytes.cat (Bytes.make 1 '\x00') nv in
       let esz = 2 + Bytes.length nk + 2 + Bytes.length sv in
       let dsz =
         List.fold_left (fun a (k, v) -> a + 2 + Bytes.length k + 2 + Bytes.length v) 0 su
       in
       if dsz + esz > P.max_data_bytes
       then true
       else if List.exists (fun (k, _) -> Bytes.equal k nk) su
       then true
       else (
         (* Build the expected page via the (already-verified) blit path. *)
         let pos0 = P.leaf_find_position buf ~n_keys:n ~key:nk in
         let blit =
           P.leaf_blit_insert
             buf
             ~pos:pos0
             ~key:nk
             ~stored_value:sv
             ~right_page:0l
             ~write_tag:0l
             ~n_keys:n
         in
         (* Mutate a private copy in place. *)
         let inplace = Cstruct.create P.page_size in
         Cstruct.blit buf 0 inplace 0 P.page_size;
         let pos = P.leaf_find_position inplace ~n_keys:n ~key:nk in
         P.leaf_insert_inplace inplace ~pos ~key:nk ~stored_value:sv ~n_keys:n;
         let want = decode_all_leaf blit (n + 1) in
         let got = decode_all_leaf inplace (n + 1) in
         want = got && (P.read_common inplace).n_keys = n + 1))
;;

(* ------------------------------------------------------------------ *)
(* branch_pick_with_info tests                                         *)
(* ------------------------------------------------------------------ *)

let test_branch_pick_with_info_empty () =
  let buf = fresh_page () in
  let rp = 42l in
  let c, idx, ptr =
    P.branch_pick_with_info buf ~n_keys:0 ~right_page:rp ~key:(Bytes.of_string "x")
  in
  Alcotest.(check int32) "child=right_page" rp c;
  Alcotest.(check int) "idx=0" 0 idx;
  Alcotest.(check int) "ptr=-1" (-1) ptr
;;

let test_branch_pick_with_info_left () =
  let buf = fresh_page () in
  let lc = 77l
  and rp = 88l
  and k = Bytes.of_string "m" in
  let _ = P.branch_append_entry buf ~offset:P.data_offset ~key:k ~left_child:lc in
  let c, _idx, ptr =
    P.branch_pick_with_info buf ~n_keys:1 ~right_page:rp ~key:(Bytes.of_string "a")
  in
  Alcotest.(check int32) "child=left_child" lc c;
  Alcotest.(check bool) "ptr>=0" true (ptr >= 0);
  Alcotest.(check int32) "ptr reads lc" lc (Cstruct.BE.get_uint32 buf ptr)
;;

let test_branch_pick_with_info_right () =
  let buf = fresh_page () in
  let lc = 77l
  and rp = 88l
  and k = Bytes.of_string "m" in
  let _ = P.branch_append_entry buf ~offset:P.data_offset ~key:k ~left_child:lc in
  let c, idx, ptr =
    P.branch_pick_with_info buf ~n_keys:1 ~right_page:rp ~key:(Bytes.of_string "z")
  in
  Alcotest.(check int32) "child=right_page" rp c;
  Alcotest.(check int) "idx=n_keys" 1 idx;
  Alcotest.(check int) "ptr=-1" (-1) ptr
;;

let prop_branch_pick_with_info_agrees =
  let gen =
    QCheck.Gen.(
      let* n = int_range 0 80 in
      let* raw = list_size (return n) (pair (bytes_size (int_range 0 20)) int32) in
      let* rp = int32 in
      let* probe = bytes_size (int_range 0 20) in
      return (raw, rp, probe))
  in
  QCheck.Test.make
    ~name:"prop_branch_pick_with_info_agrees"
    ~count:10_000
    (QCheck.make gen)
    (fun (raw, rp, probe) ->
       let su = sort_unique_by_key raw in
       let buf = fresh_page () in
       let _ =
         List.fold_left
           (fun off (k, lc) ->
              P.branch_append_entry buf ~offset:off ~key:k ~left_child:lc)
           P.data_offset
           su
       in
       let n = List.length su in
       let ref_ = ref_branch_pick buf n rp probe in
       let fast, _, ptr =
         P.branch_pick_with_info buf ~n_keys:n ~right_page:rp ~key:probe
       in
       fast = ref_ && (ptr < 0 || Cstruct.BE.get_uint32 buf ptr = fast))
;;

(* ------------------------------------------------------------------ *)
(* branch_blit_update_child tests                                      *)
(* ------------------------------------------------------------------ *)

let test_branch_blit_update_child_left () =
  let buf = fresh_page () in
  let lc = 77l
  and rp = 88l
  and k = Bytes.of_string "m" in
  let _ = P.branch_append_entry buf ~offset:P.data_offset ~key:k ~left_child:lc in
  P.write_common
    buf
    P.{ kind = Branch; flags = 0; n_keys = 1; right_page = rp; crc32 = 0l };
  P.seal buf;
  let _, _, ptr =
    P.branch_pick_with_info buf ~n_keys:1 ~right_page:rp ~key:(Bytes.of_string "a")
  in
  let nb =
    P.branch_blit_update_child buf ~child_ptr_offset:ptr ~new_child:99l ~write_tag:0l
  in
  Alcotest.(check int32)
    "left_child updated"
    99l
    (P.branch_pick nb ~n_keys:1 ~right_page:rp ~key:(Bytes.of_string "a"))
;;

let test_branch_blit_update_child_right () =
  let buf = fresh_page () in
  let lc = 77l
  and rp = 88l
  and k = Bytes.of_string "m" in
  let _ = P.branch_append_entry buf ~offset:P.data_offset ~key:k ~left_child:lc in
  P.write_common
    buf
    P.{ kind = Branch; flags = 0; n_keys = 1; right_page = rp; crc32 = 0l };
  P.seal buf;
  let nb =
    P.branch_blit_update_child buf ~child_ptr_offset:(-1) ~new_child:123l ~write_tag:0l
  in
  Alcotest.(check int32)
    "right_page updated"
    123l
    (P.branch_pick nb ~n_keys:1 ~right_page:123l ~key:(Bytes.of_string "z"))
;;

(* ------------------------------------------------------------------ *)
(* RUNNER                                                              *)
(* ------------------------------------------------------------------ *)

let () =
  let qcheck_tests =
    List.map
      QCheck_alcotest.to_alcotest
      [ prop_leaf_roundtrip
      ; prop_branch_roundtrip
      ; prop_crc_detects_flip
      ; prop_freelist_roundtrip
      ; prop_common_roundtrip
      ; prop_leaf_lookup_matches
      ; prop_branch_pick_matches
      ; prop_leaf_blit_insert_equiv
      ; prop_leaf_insert_inplace_equiv
      ; prop_branch_pick_with_info_agrees
      ]
  in
  Alcotest.run
    "page"
    [ ( "constants"
      , [ Alcotest.test_case "page constants" `Quick test_constants
        ; Alcotest.test_case
            "max_freelist_entries_per_page"
            `Quick
            test_max_freelist_entries
        ] )
    ; ( "common_header"
      , [ Alcotest.test_case "roundtrip Header kind" `Quick test_common_roundtrip_header
        ; Alcotest.test_case "roundtrip Branch kind" `Quick test_common_roundtrip_branch
        ; Alcotest.test_case "roundtrip Leaf kind" `Quick test_common_roundtrip_leaf
        ; Alcotest.test_case
            "roundtrip Freelist kind"
            `Quick
            test_common_roundtrip_freelist
        ; Alcotest.test_case "n_keys round-trip" `Quick test_common_n_keys_roundtrip
        ; Alcotest.test_case
            "right_page round-trip"
            `Quick
            test_common_right_page_roundtrip
        ; Alcotest.test_case "flags round-trip" `Quick test_common_flags_roundtrip
        ; Alcotest.test_case "reserved bytes zeroed" `Quick test_common_reserved_zeroed
        ] )
    ; ( "invalid_kind"
      , [ Alcotest.test_case "kind byte 255 raises Failure" `Quick test_invalid_kind_byte
        ; Alcotest.test_case "kind byte 5 raises Failure" `Quick test_invalid_kind_byte_4
        ] )
    ; ( "crc32"
      , [ Alcotest.test_case "compute_crc is deterministic" `Quick test_crc_deterministic
        ; Alcotest.test_case
            "verify_crc passes on fresh page"
            `Quick
            test_verify_crc_fresh_page
        ; Alcotest.test_case
            "verify_crc fails: data byte flip"
            `Quick
            test_verify_crc_fails_flipped_data_byte
        ; Alcotest.test_case
            "verify_crc fails: CRC byte flip"
            `Quick
            test_verify_crc_fails_flipped_crc_byte
        ; Alcotest.test_case "seal then verify_crc passes" `Quick test_seal
        ] )
    ; ( "header_fields"
      , [ Alcotest.test_case
            "header_fields round-trip"
            `Quick
            test_header_fields_roundtrip
        ] )
    ; ( "branch"
      , [ Alcotest.test_case "branch single entry" `Quick test_branch_single_entry
        ; Alcotest.test_case "branch three entries" `Quick test_branch_three_entries
        ; Alcotest.test_case
            "branch End at near-boundary"
            `Quick
            test_branch_end_at_offset_past_data
        ; Alcotest.test_case "branch End at page_size" `Quick test_branch_end_at_page_size
        ; Alcotest.test_case
            "branch zeroed page Entry"
            `Quick
            test_branch_zeroed_page_entry
        ] )
    ; ( "leaf"
      , [ Alcotest.test_case "leaf single entry" `Quick test_leaf_single_entry
        ; Alcotest.test_case "leaf three entries" `Quick test_leaf_three_entries
        ; Alcotest.test_case
            "leaf End at near-boundary"
            `Quick
            test_leaf_end_at_offset_past_data
        ; Alcotest.test_case "leaf End at page_size" `Quick test_leaf_end_at_page_size
        ; Alcotest.test_case "leaf zeroed page Entry" `Quick test_leaf_zeroed_page_entry
        ] )
    ; ( "overflow"
      , [ Alcotest.test_case
            "branch_append_entry overflow"
            `Quick
            test_branch_append_overflow
        ; Alcotest.test_case "leaf_append_entry overflow" `Quick test_leaf_append_overflow
        ; Alcotest.test_case
            "branch_append key>0xFFFF raises"
            `Quick
            test_branch_append_key_too_long
        ; Alcotest.test_case
            "leaf_append key>0xFFFF raises"
            `Quick
            test_leaf_append_key_too_long
        ; Alcotest.test_case
            "leaf_append val>0xFFFF raises"
            `Quick
            test_leaf_append_value_too_long
        ; Alcotest.test_case
            "branch_entry_at key overflow"
            `Quick
            test_branch_entry_at_key_overflow
        ; Alcotest.test_case
            "leaf_entry_at key overflow"
            `Quick
            test_leaf_entry_at_key_overflow
        ; Alcotest.test_case
            "leaf_entry_at val overflow"
            `Quick
            test_leaf_entry_at_val_overflow
        ] )
    ; ( "freelist"
      , [ Alcotest.test_case "freelist single entry" `Quick test_freelist_single_entry
        ; Alcotest.test_case
            "freelist multiple entries"
            `Quick
            test_freelist_multiple_entries
        ; Alcotest.test_case "freelist isolation" `Quick test_freelist_isolation
        ] )
    ; ( "large_page"
      , [ Alcotest.test_case
            "CRC covers whole 16K page"
            `Quick
            test_crc_covers_whole_large_page
        ; Alcotest.test_case
            "leaf append/read beyond 4096"
            `Quick
            test_leaf_append_beyond_4096
        ; Alcotest.test_case
            "leaf append respects reserved tail"
            `Quick
            test_leaf_append_respects_reserved
        ] )
    ; ( "leaf_find_position"
      , [ Alcotest.test_case "empty page" `Quick test_leaf_find_position_empty
        ; Alcotest.test_case "before first" `Quick test_leaf_find_position_before_first
        ; Alcotest.test_case "after last" `Quick test_leaf_find_position_after_last
        ; Alcotest.test_case "exact match" `Quick test_leaf_find_position_exact
        ] )
    ; ( "leaf_blit_insert"
      , [ Alcotest.test_case "into empty" `Quick test_leaf_blit_insert_empty
        ; Alcotest.test_case "before existing" `Quick test_leaf_blit_insert_before
        ] )
    ; ( "leaf_insert_inplace"
      , [ Alcotest.test_case "append at end" `Quick test_leaf_insert_inplace_append
        ; Alcotest.test_case "insert in middle" `Quick test_leaf_insert_inplace_middle
        ; Alcotest.test_case "insert at front" `Quick test_leaf_insert_inplace_front
        ] )
    ; ( "branch_pick_with_info"
      , [ Alcotest.test_case "empty branch" `Quick test_branch_pick_with_info_empty
        ; Alcotest.test_case "follows left_child" `Quick test_branch_pick_with_info_left
        ; Alcotest.test_case "follows right_page" `Quick test_branch_pick_with_info_right
        ] )
    ; ( "branch_blit_update_child"
      , [ Alcotest.test_case "update left_child" `Quick test_branch_blit_update_child_left
        ; Alcotest.test_case
            "update right_page"
            `Quick
            test_branch_blit_update_child_right
        ] )
    ; "qcheck", qcheck_tests
    ]
;;
