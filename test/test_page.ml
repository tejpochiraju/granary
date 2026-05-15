(** Tests for Sqlocaml_storage.Page *)

module P = Sqlocaml_storage.Page

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let fresh_page () =
  let buf = Cstruct.create P.page_size in
  Cstruct.memset buf 0;
  buf

(* flip a single bit in byte at position [pos] *)
let flip_byte buf pos =
  let b = Cstruct.get_uint8 buf pos in
  Cstruct.set_uint8 buf pos (b lxor 0xFF)

(* ------------------------------------------------------------------ *)
(* 1. Constants                                                        *)
(* ------------------------------------------------------------------ *)

let test_constants () =
  Alcotest.(check int) "page_size"      4096 P.page_size;
  Alcotest.(check int) "header_size"    16   P.header_size;
  Alcotest.(check int) "data_offset"    16   P.data_offset;
  Alcotest.(check int) "max_data_bytes" 4080 P.max_data_bytes

let test_max_freelist_entries () =
  Alcotest.(check int) "max_freelist_entries_per_page" 340 P.max_freelist_entries_per_page

(* ------------------------------------------------------------------ *)
(* 2. Common header round-trips                                        *)
(* ------------------------------------------------------------------ *)

let make_common kind =
  P.{ kind; flags = 0; n_keys = 0; right_page = 0l; crc32 = 0l }

let test_common_roundtrip_header () =
  let buf = fresh_page () in
  let c = make_common P.Header in
  P.write_common buf c;
  let c' = P.read_common buf in
  Alcotest.(check string) "kind=Header" "Header"
    (match c'.kind with P.Header -> "Header" | _ -> "other")

let test_common_roundtrip_branch () =
  let buf = fresh_page () in
  let c = make_common P.Branch in
  P.write_common buf c;
  let c' = P.read_common buf in
  Alcotest.(check string) "kind=Branch" "Branch"
    (match c'.kind with P.Branch -> "Branch" | _ -> "other")

let test_common_roundtrip_leaf () =
  let buf = fresh_page () in
  let c = make_common P.Leaf in
  P.write_common buf c;
  let c' = P.read_common buf in
  Alcotest.(check string) "kind=Leaf" "Leaf"
    (match c'.kind with P.Leaf -> "Leaf" | _ -> "other")

let test_common_roundtrip_freelist () =
  let buf = fresh_page () in
  let c = make_common P.Freelist in
  P.write_common buf c;
  let c' = P.read_common buf in
  Alcotest.(check string) "kind=Freelist" "Freelist"
    (match c'.kind with P.Freelist -> "Freelist" | _ -> "other")

let test_common_n_keys_roundtrip () =
  let buf = fresh_page () in
  let c = P.{ kind = P.Leaf; flags = 0; n_keys = 42; right_page = 0l; crc32 = 0l } in
  P.write_common buf c;
  let c' = P.read_common buf in
  Alcotest.(check int) "n_keys round-trip" 42 c'.n_keys

let test_common_right_page_roundtrip () =
  let buf = fresh_page () in
  let c = P.{ kind = P.Branch; flags = 0; n_keys = 0; right_page = 0xDEADBEEFl; crc32 = 0l } in
  P.write_common buf c;
  let c' = P.read_common buf in
  Alcotest.(check int32) "right_page round-trip" 0xDEADBEEFl c'.right_page

let test_common_flags_roundtrip () =
  let buf = fresh_page () in
  let c = P.{ kind = P.Leaf; flags = 7; n_keys = 0; right_page = 0l; crc32 = 0l } in
  P.write_common buf c;
  let c' = P.read_common buf in
  Alcotest.(check int) "flags round-trip" 7 c'.flags

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

(* ------------------------------------------------------------------ *)
(* 3. Invalid kind byte                                                *)
(* ------------------------------------------------------------------ *)

let test_invalid_kind_byte () =
  let buf = fresh_page () in
  Cstruct.set_uint8 buf 0 255;
  match P.read_common buf with
  | _ -> Alcotest.fail "expected Failure for invalid kind byte"
  | exception Failure _ -> ()  (* expected *)

let test_invalid_kind_byte_4 () =
  let buf = fresh_page () in
  Cstruct.set_uint8 buf 0 4;
  match P.read_common buf with
  | _ -> Alcotest.fail "expected Failure for kind byte 4"
  | exception Failure _ -> ()

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

let test_verify_crc_fails_flipped_crc_byte () =
  let buf = fresh_page () in
  let c = P.{ kind = P.Leaf; flags = 0; n_keys = 0; right_page = 0l; crc32 = 0l } in
  P.write_common buf c;
  let crc = P.compute_crc buf in
  P.write_common buf { c with P.crc32 = crc };
  (* flip a byte inside the CRC field itself (byte 8) *)
  flip_byte buf 8;
  Alcotest.(check bool) "verify_crc fails after CRC field flip" false (P.verify_crc buf)

let test_seal () =
  let buf = fresh_page () in
  let c = P.{ kind = P.Leaf; flags = 0; n_keys = 5; right_page = 0l; crc32 = 0l } in
  P.write_common buf c;
  P.seal buf;
  Alcotest.(check bool) "verify after seal" true (P.verify_crc buf)

(* ------------------------------------------------------------------ *)
(* 5. Header page fields round-trip                                    *)
(* ------------------------------------------------------------------ *)

let test_header_fields_roundtrip () =
  let buf = fresh_page () in
  let hf = P.{
    txn_id         = 0x0102030405060708L;
    root_page      = 99L;
    freelist_page  = 7L;
    n_pages_total  = 1024L;
    schema_version = 1L;
    page_size      = 4096l;
    format_version = 1l;
  } in
  P.write_header_fields buf hf;
  let hf' = P.read_header_fields buf in
  Alcotest.(check int64) "txn_id"         hf.txn_id         hf'.txn_id;
  Alcotest.(check int64) "root_page"      hf.root_page      hf'.root_page;
  Alcotest.(check int64) "freelist_page"  hf.freelist_page  hf'.freelist_page;
  Alcotest.(check int64) "n_pages_total"  hf.n_pages_total  hf'.n_pages_total;
  Alcotest.(check int64) "schema_version" hf.schema_version hf'.schema_version;
  Alcotest.(check int32) "page_size"      hf.page_size      hf'.page_size;
  Alcotest.(check int32) "format_version" hf.format_version hf'.format_version

(* ------------------------------------------------------------------ *)
(* 6. Branch page entries                                              *)
(* ------------------------------------------------------------------ *)

let test_branch_single_entry () =
  let buf = fresh_page () in
  let key = Bytes.of_string "hello" in
  let left_child = 42l in
  let _next = P.branch_append_entry buf ~offset:P.data_offset ~key ~left_child in
  (match P.branch_entry_at buf ~offset:P.data_offset with
   | `End -> Alcotest.fail "expected Entry, got End"
   | `Entry (e : P.branch_entry) ->
     Alcotest.(check bool) "key round-trip" true (Bytes.equal key e.key);
     Alcotest.(check int32) "left_child round-trip" left_child e.left_child)

let test_branch_three_entries () =
  let buf = fresh_page () in
  let entries = [|
    (Bytes.of_string "aaa", 10l);
    (Bytes.of_string "bbb", 20l);
    (Bytes.of_string "ccc", 30l);
  |] in
  let off0 = P.data_offset in
  let off1 = P.branch_append_entry buf ~offset:off0
               ~key:(fst entries.(0)) ~left_child:(snd entries.(0)) in
  let off2 = P.branch_append_entry buf ~offset:off1
               ~key:(fst entries.(1)) ~left_child:(snd entries.(1)) in
  let _off3 = P.branch_append_entry buf ~offset:off2
               ~key:(fst entries.(2)) ~left_child:(snd entries.(2)) in
  (* Iterate all three *)
  (match P.branch_entry_at buf ~offset:off0 with
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
           Alcotest.(check bool) "next_offset advances" true (e2.next_offset > P.data_offset))))

let test_branch_end_at_offset_past_data () =
  let buf = fresh_page () in
  (* offset past all data: page_size - 5 < page_size - 6 is false, but +6 > 4096 *)
  let offset = P.page_size - 3 in
  (match P.branch_entry_at buf ~offset with
   | `End -> ()  (* expected *)
   | `Entry _ -> Alcotest.fail "expected End for offset near page boundary")

let test_branch_end_at_page_size () =
  let buf = fresh_page () in
  (match P.branch_entry_at buf ~offset:P.page_size with
   | `End -> ()
   | `Entry _ -> Alcotest.fail "expected End at page_size offset")

(* ------------------------------------------------------------------ *)
(* 7. Leaf page entries                                                *)
(* ------------------------------------------------------------------ *)

let test_leaf_single_entry () =
  let buf = fresh_page () in
  let key   = Bytes.of_string "mykey" in
  let value = Bytes.of_string "myvalue" in
  let _next = P.leaf_append_entry buf ~offset:P.data_offset ~key ~value in
  (match P.leaf_entry_at buf ~offset:P.data_offset with
   | `End -> Alcotest.fail "expected Entry, got End"
   | `Entry (e : P.leaf_entry) ->
     Alcotest.(check bool) "key round-trip" true (Bytes.equal key e.key);
     Alcotest.(check bool) "value round-trip" true (Bytes.equal value e.value))

let test_leaf_three_entries () =
  let buf = fresh_page () in
  let entries = [|
    (Bytes.of_string "k1", Bytes.of_string "v1");
    (Bytes.of_string "k2", Bytes.of_string "v22");
    (Bytes.of_string "k333", Bytes.of_string "v3");
  |] in
  let off0 = P.data_offset in
  let off1 = P.leaf_append_entry buf ~offset:off0
               ~key:(fst entries.(0)) ~value:(snd entries.(0)) in
  let off2 = P.leaf_append_entry buf ~offset:off1
               ~key:(fst entries.(1)) ~value:(snd entries.(1)) in
  let _off3 = P.leaf_append_entry buf ~offset:off2
               ~key:(fst entries.(2)) ~value:(snd entries.(2)) in
  (match P.leaf_entry_at buf ~offset:off0 with
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
           Alcotest.(check bool) "entry 2 val" true (Bytes.equal (snd entries.(2)) e2.value);
           Alcotest.(check bool) "next_offset advances" true (e2.next_offset > P.data_offset))))

let test_leaf_end_at_offset_past_data () =
  let buf = fresh_page () in
  let offset = P.page_size - 2 in
  (match P.leaf_entry_at buf ~offset with
   | `End -> ()
   | `Entry _ -> Alcotest.fail "expected End for offset near page boundary")

let test_leaf_end_at_page_size () =
  let buf = fresh_page () in
  (match P.leaf_entry_at buf ~offset:P.page_size with
   | `End -> ()
   | `Entry _ -> Alcotest.fail "expected End at page_size offset")

(* ------------------------------------------------------------------ *)
(* 8. Zeroed page: entry iteration behavior                            *)
(* ------------------------------------------------------------------ *)

(* On a zeroed page: branch key_len=0 → 6-byte entry (valid `Entry).
   The caller must stop after n_keys=0 entries (which they read from common header).
   `End is only returned when truly out-of-bounds. *)
let test_branch_zeroed_page_entry () =
  let buf = fresh_page () in
  (* zeroed page: key_len=0, left_child=0, 6-byte entry fits *)
  (match P.branch_entry_at buf ~offset:P.data_offset with
   | `End -> Alcotest.fail "zeroed page: expected Entry with key_len=0, not End"
   | `Entry (e : P.branch_entry) ->
     Alcotest.(check int) "key_len=0" 0 (Bytes.length e.key);
     Alcotest.(check int32) "left_child=0" 0l e.left_child;
     (* next_offset = 16 + 6 = 22; this is within bounds, so next call also gets Entry *)
     (* But the second entry at offset 22 also has key_len=0 and is valid *)
     (* Only when offset+6 > 4096 do we get End *)
     Alcotest.(check bool) "next_offset = 22" true (e.next_offset = 22))

let test_leaf_zeroed_page_entry () =
  let buf = fresh_page () in
  (* zeroed page: key_len=0, val_len=0, 4-byte entry fits *)
  (match P.leaf_entry_at buf ~offset:P.data_offset with
   | `End -> Alcotest.fail "zeroed page: expected Entry with key/val len 0, not End"
   | `Entry (e : P.leaf_entry) ->
     Alcotest.(check int) "key_len=0" 0 (Bytes.length e.key);
     Alcotest.(check int) "val_len=0" 0 (Bytes.length e.value);
     Alcotest.(check bool) "next_offset = 20" true (e.next_offset = 20))

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

let test_freelist_multiple_entries () =
  let buf = fresh_page () in
  (* set a few entries at different indices *)
  P.freelist_set_entry buf ~index:0   ~page_id:10l  ~freed_at_txn_id:100L;
  P.freelist_set_entry buf ~index:1   ~page_id:20l  ~freed_at_txn_id:200L;
  P.freelist_set_entry buf ~index:339 ~page_id:999l ~freed_at_txn_id:9999L;
  let (e0 : P.freelist_entry) = P.freelist_entry_at buf ~index:0 in
  let (e1 : P.freelist_entry) = P.freelist_entry_at buf ~index:1 in
  let (e339 : P.freelist_entry) = P.freelist_entry_at buf ~index:339 in
  Alcotest.(check int32) "entry 0 page_id"  10l  e0.page_id;
  Alcotest.(check int64) "entry 0 txn_id"   100L e0.freed_at_txn_id;
  Alcotest.(check int32) "entry 1 page_id"  20l  e1.page_id;
  Alcotest.(check int64) "entry 1 txn_id"   200L e1.freed_at_txn_id;
  Alcotest.(check int32) "entry 339 page_id" 999l e339.page_id;
  Alcotest.(check int64) "entry 339 txn_id" 9999L e339.freed_at_txn_id

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

(* ------------------------------------------------------------------ *)
(* 9b. Overflow / invalid-arg edge cases                               *)
(* ------------------------------------------------------------------ *)

(* branch_append_entry: key too long for the current page offset *)
let test_branch_append_overflow () =
  let buf = fresh_page () in
  (* Place the offset near the end so the entry would overflow *)
  let near_end = P.page_size - 4 in  (* only 4 bytes left: can't fit key + 4-byte child *)
  let key = Bytes.of_string "k" in
  match P.branch_append_entry buf ~offset:near_end ~key ~left_child:1l with
  | _ -> Alcotest.fail "expected invalid_arg for branch overflow"
  | exception Invalid_argument _ -> ()

(* leaf_append_entry: entry would overflow page *)
let test_leaf_append_overflow () =
  let buf = fresh_page () in
  let near_end = P.page_size - 2 in  (* only 2 bytes left: can't fit key+value entry *)
  let key   = Bytes.of_string "k" in
  let value = Bytes.of_string "v" in
  match P.leaf_append_entry buf ~offset:near_end ~key ~value with
  | _ -> Alcotest.fail "expected invalid_arg for leaf overflow"
  | exception Invalid_argument _ -> ()

(* branch_entry_at: key_len says the key would overflow the page *)
let test_branch_entry_at_key_overflow () =
  let buf = fresh_page () in
  (* Write a branch entry manually with a large key_len that would go past page_size *)
  let offset = P.page_size - 10 in
  (* key_len = 0xFFFF → 2 + 65535 + 4 would far exceed page bounds *)
  Cstruct.BE.set_uint16 buf offset 0xFFFF;
  match P.branch_entry_at buf ~offset with
  | `End -> ()  (* expected: key_len overflows page *)
  | `Entry _ -> Alcotest.fail "expected End when key_len overflows page"

(* leaf_entry_at: key_len or val_len overflow *)
let test_leaf_entry_at_key_overflow () =
  let buf = fresh_page () in
  let offset = P.page_size - 10 in
  (* key_len = 0xFFFF → overflows *)
  Cstruct.BE.set_uint16 buf offset 0xFFFF;
  match P.leaf_entry_at buf ~offset with
  | `End -> ()  (* expected *)
  | `Entry _ -> Alcotest.fail "expected End when leaf key_len overflows page"

(* leaf_entry_at: key fits but val_len overflows *)
let test_leaf_entry_at_val_overflow () =
  let buf = fresh_page () in
  (* Place at start of data, write key_len=1, key='a', then val_len=0xFFFF *)
  let offset = P.data_offset in
  Cstruct.BE.set_uint16 buf offset 1;          (* key_len = 1 *)
  Cstruct.set_char buf (offset + 2) 'a';       (* key data *)
  Cstruct.BE.set_uint16 buf (offset + 3) 0xFFFF; (* val_len = 65535 → overflows *)
  match P.leaf_entry_at buf ~offset with
  | `End -> ()  (* expected *)
  | `Entry _ -> Alcotest.fail "expected End when leaf val_len overflows page"

(* ------------------------------------------------------------------ *)
(* 10. QCheck property tests                                           *)
(* ------------------------------------------------------------------ *)

(* QCheck: leaf key/value round-trip *)
let prop_leaf_roundtrip =
  let gen =
    QCheck.Gen.(
      let* key_data   = bytes_size (int_range 0 500) in
      let* value_data = bytes_size (int_range 0 500) in
      return (key_data, value_data)
    )
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
       | `Entry (e : P.leaf_entry) ->
         Bytes.equal key e.key && Bytes.equal value e.value)

(* QCheck: branch key/child round-trip *)
let prop_branch_roundtrip =
  let gen =
    QCheck.Gen.(
      let* key_data    = bytes_size (int_range 0 500) in
      let* left_child  = int32 in
      return (key_data, left_child)
    )
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
       | `Entry (e : P.branch_entry) ->
         Bytes.equal key e.key && e.left_child = left_child)

(* QCheck: compute_crc detects any single-byte flip outside bytes 8..11 *)
let prop_crc_detects_flip =
  let gen =
    QCheck.Gen.(
      let* buf_data = bytes_size (return 4096) in
      (* pick a byte position not in [8..11] *)
      let non_crc_pos = oneof [
        int_range 0 7;
        int_range 12 4095;
      ] in
      let* pos = non_crc_pos in
      return (buf_data, pos)
    )
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

(* QCheck: freelist round-trip *)
let prop_freelist_roundtrip =
  let gen =
    QCheck.Gen.(
      let* index          = int_range 0 (P.max_freelist_entries_per_page - 1) in
      let* page_id        = int32 in
      let* freed_at_txn_id = int64 in
      return (index, page_id, freed_at_txn_id)
    )
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

(* QCheck: common header round-trip for all fields *)
let prop_common_roundtrip =
  let gen =
    QCheck.Gen.(
      let* kind_n    = int_range 0 3 in
      let* flags     = int_range 0 255 in
      let* n_keys    = int_range 0 65535 in
      let* right_page = int32 in
      let* crc32      = int32 in
      return (kind_n, flags, n_keys, right_page, crc32)
    )
  in
  QCheck.Test.make
    ~name:"prop_common_roundtrip"
    ~count:10_000
    (QCheck.make gen)
    (fun (kind_n, flags, n_keys, right_page, crc32) ->
       let kind = match kind_n with
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
       && (match c'.kind, kind with
           | P.Header, P.Header | P.Branch, P.Branch
           | P.Leaf, P.Leaf | P.Freelist, P.Freelist -> true
           | _ -> false))

(* ------------------------------------------------------------------ *)
(* RUNNER                                                              *)
(* ------------------------------------------------------------------ *)

let () =
  let qcheck_tests =
    List.map QCheck_alcotest.to_alcotest [
      prop_leaf_roundtrip;
      prop_branch_roundtrip;
      prop_crc_detects_flip;
      prop_freelist_roundtrip;
      prop_common_roundtrip;
    ]
  in
  Alcotest.run "page" [
    "constants", [
      Alcotest.test_case "page constants"                   `Quick test_constants;
      Alcotest.test_case "max_freelist_entries_per_page"    `Quick test_max_freelist_entries;
    ];
    "common_header", [
      Alcotest.test_case "roundtrip Header kind"            `Quick test_common_roundtrip_header;
      Alcotest.test_case "roundtrip Branch kind"            `Quick test_common_roundtrip_branch;
      Alcotest.test_case "roundtrip Leaf kind"              `Quick test_common_roundtrip_leaf;
      Alcotest.test_case "roundtrip Freelist kind"          `Quick test_common_roundtrip_freelist;
      Alcotest.test_case "n_keys round-trip"                `Quick test_common_n_keys_roundtrip;
      Alcotest.test_case "right_page round-trip"            `Quick test_common_right_page_roundtrip;
      Alcotest.test_case "flags round-trip"                 `Quick test_common_flags_roundtrip;
      Alcotest.test_case "reserved bytes zeroed"            `Quick test_common_reserved_zeroed;
    ];
    "invalid_kind", [
      Alcotest.test_case "kind byte 255 raises Failure"     `Quick test_invalid_kind_byte;
      Alcotest.test_case "kind byte 4 raises Failure"       `Quick test_invalid_kind_byte_4;
    ];
    "crc32", [
      Alcotest.test_case "compute_crc is deterministic"     `Quick test_crc_deterministic;
      Alcotest.test_case "verify_crc passes on fresh page"  `Quick test_verify_crc_fresh_page;
      Alcotest.test_case "verify_crc fails: data byte flip" `Quick test_verify_crc_fails_flipped_data_byte;
      Alcotest.test_case "verify_crc fails: CRC byte flip"  `Quick test_verify_crc_fails_flipped_crc_byte;
      Alcotest.test_case "seal then verify_crc passes"      `Quick test_seal;
    ];
    "header_fields", [
      Alcotest.test_case "header_fields round-trip"         `Quick test_header_fields_roundtrip;
    ];
    "branch", [
      Alcotest.test_case "branch single entry"              `Quick test_branch_single_entry;
      Alcotest.test_case "branch three entries"             `Quick test_branch_three_entries;
      Alcotest.test_case "branch End at near-boundary"      `Quick test_branch_end_at_offset_past_data;
      Alcotest.test_case "branch End at page_size"          `Quick test_branch_end_at_page_size;
      Alcotest.test_case "branch zeroed page Entry"         `Quick test_branch_zeroed_page_entry;
    ];
    "leaf", [
      Alcotest.test_case "leaf single entry"                `Quick test_leaf_single_entry;
      Alcotest.test_case "leaf three entries"               `Quick test_leaf_three_entries;
      Alcotest.test_case "leaf End at near-boundary"        `Quick test_leaf_end_at_offset_past_data;
      Alcotest.test_case "leaf End at page_size"            `Quick test_leaf_end_at_page_size;
      Alcotest.test_case "leaf zeroed page Entry"           `Quick test_leaf_zeroed_page_entry;
    ];
    "overflow", [
      Alcotest.test_case "branch_append_entry overflow"     `Quick test_branch_append_overflow;
      Alcotest.test_case "leaf_append_entry overflow"       `Quick test_leaf_append_overflow;
      Alcotest.test_case "branch_entry_at key overflow"     `Quick test_branch_entry_at_key_overflow;
      Alcotest.test_case "leaf_entry_at key overflow"       `Quick test_leaf_entry_at_key_overflow;
      Alcotest.test_case "leaf_entry_at val overflow"       `Quick test_leaf_entry_at_val_overflow;
    ];
    "freelist", [
      Alcotest.test_case "freelist single entry"            `Quick test_freelist_single_entry;
      Alcotest.test_case "freelist multiple entries"        `Quick test_freelist_multiple_entries;
      Alcotest.test_case "freelist isolation"               `Quick test_freelist_isolation;
    ];
    "qcheck", qcheck_tests;
  ]
