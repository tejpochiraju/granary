open Sqlocaml_encoding

(* ── Fixed round-trip cases ─────────────────────────────────────────── *)

let cases_uint64 =
  [ 0L; 1L; 127L; 128L; 255L; 256L; 16383L; 16384L; 2097151L; 2097152L; Int64.max_int ]
;;

let cases_int64 =
  [ 0L
  ; 1L
  ; -1L
  ; 127L
  ; -127L
  ; 128L
  ; -128L
  ; 16383L
  ; -16383L
  ; 16384L
  ; -16384L
  ; Int64.max_int
  ; Int64.min_int
  ; -1000000L
  ; 1000000L
  ]
;;

let roundtrip_uint64 v () =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf v;
  let b = Buffer.to_bytes buf in
  let decoded, off = Varint.decode_uint64 b 0 in
  Alcotest.(check int64) "value matches" v decoded;
  Alcotest.(check int) "consumed all bytes" (Bytes.length b) off
;;

let roundtrip_int64 v () =
  let buf = Buffer.create 16 in
  Varint.encode_int64 buf v;
  let b = Buffer.to_bytes buf in
  let decoded, off = Varint.decode_int64 b 0 in
  Alcotest.(check int64) "value matches" v decoded;
  Alcotest.(check int) "consumed all bytes" (Bytes.length b) off
;;

(* ── Encoding length tests (uint64) ─────────────────────────────────── *)

let check_uint64_len v expected_bytes () =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf v;
  Alcotest.(check int) "byte length" expected_bytes (Buffer.length buf)
;;

let uint64_len_cases =
  [ 0L, 1, "0 → 1 byte"
  ; 127L, 1, "127 → 1 byte"
  ; 128L, 2, "128 → 2 bytes"
  ; 16383L, 2, "16383 → 2 bytes"
  ; 16384L, 3, "16384 → 3 bytes"
  ; 2097151L, 3, "2097151 → 3 bytes"
  ; 2097152L, 4, "2097152 → 4 bytes"
  ; Int64.max_int, 9, "max_int → 9 bytes"
  ]
;;

(* ── Encoding length tests (int64 / zigzag) ─────────────────────────── *)

let check_int64_len v expected_bytes () =
  let buf = Buffer.create 16 in
  Varint.encode_int64 buf v;
  Alcotest.(check int) "byte length" expected_bytes (Buffer.length buf)
;;

let int64_len_cases =
  [ 0L, 1, "0 → 1 byte"
  ; -1L, 1, "-1 → zigzag=1 → 1 byte"
  ; 1L, 1, "1 → zigzag=2 → 1 byte"
  ; -64L, 1, "-64 → zigzag=127 → 1 byte"
  ; 64L, 2, "64 → zigzag=128 → 2 bytes"
  ; Int64.min_int, 10, "min_int → zigzag=0xFFFFFFFF… → 10 bytes"
  ]
;;

(* ── Decode-at-offset test ───────────────────────────────────────────── *)

let test_decode_at_offset () =
  let buf = Buffer.create 32 in
  Varint.encode_uint64 buf 42L;
  Varint.encode_uint64 buf 9999L;
  let b = Buffer.to_bytes buf in
  let v1, off1 = Varint.decode_uint64 b 0 in
  Alcotest.(check int64) "first value" 42L v1;
  let v2, off2 = Varint.decode_uint64 b off1 in
  Alcotest.(check int64) "second value" 9999L v2;
  Alcotest.(check int) "consumed whole buffer" (Bytes.length b) off2
;;

(* ── Out-of-bounds decode raises ────────────────────────────────────── *)

let test_oob_raises () =
  let buf = Buffer.create 4 in
  Varint.encode_uint64 buf 0L;
  let b = Buffer.to_bytes buf in
  (* offset 1 is past the single-byte encoding of 0 *)
  try
    let _ = Varint.decode_uint64 b 1 in
    Alcotest.fail "expected exception for out-of-bounds decode"
  with
  | Invalid_argument _ -> ()
;;

(* ── QCheck property tests ──────────────────────────────────────────── *)

let prop_uint64_roundtrip =
  QCheck.Test.make ~count:10000 ~name:"uint64 roundtrip" QCheck.int64 (fun i ->
    let i = if Int64.compare i 0L < 0 then Int64.neg i else i in
    let buf = Buffer.create 16 in
    Varint.encode_uint64 buf i;
    let b = Buffer.to_bytes buf in
    let v, off = Varint.decode_uint64 b 0 in
    Int64.equal v i && off = Bytes.length b)
;;

let prop_int64_roundtrip =
  QCheck.Test.make ~count:10000 ~name:"int64 roundtrip" QCheck.int64 (fun i ->
    let buf = Buffer.create 16 in
    Varint.encode_int64 buf i;
    let b = Buffer.to_bytes buf in
    let v, off = Varint.decode_int64 b 0 in
    Int64.equal v i && off = Bytes.length b)
;;

let prop_max_bytes =
  QCheck.Test.make ~count:10000 ~name:"max 10 bytes" QCheck.int64 (fun i ->
    let buf = Buffer.create 16 in
    Varint.encode_int64 buf i;
    Buffer.length buf <= 10)
;;

let prop_deterministic =
  QCheck.Test.make ~count:10000 ~name:"deterministic" QCheck.int64 (fun i ->
    let b1 = Buffer.create 8 in
    Varint.encode_int64 b1 i;
    let b2 = Buffer.create 8 in
    Varint.encode_int64 b2 i;
    Bytes.equal (Buffer.to_bytes b1) (Buffer.to_bytes b2))
;;

(* ── Assemble test suites ───────────────────────────────────────────── *)

let () =
  let uint64_rt_tests =
    List.map
      (fun v ->
         Alcotest.test_case
           (Printf.sprintf "roundtrip uint64 %Ld" v)
           `Quick
           (roundtrip_uint64 v))
      cases_uint64
  in
  let int64_rt_tests =
    List.map
      (fun v ->
         Alcotest.test_case
           (Printf.sprintf "roundtrip int64 %Ld" v)
           `Quick
           (roundtrip_int64 v))
      cases_int64
  in
  let uint64_len_tests =
    List.map
      (fun (v, expected, label) ->
         Alcotest.test_case label `Quick (check_uint64_len v expected))
      uint64_len_cases
  in
  let int64_len_tests =
    List.map
      (fun (v, expected, label) ->
         Alcotest.test_case label `Quick (check_int64_len v expected))
      int64_len_cases
  in
  let qcheck_tests =
    List.map
      QCheck_alcotest.to_alcotest
      [ prop_uint64_roundtrip; prop_int64_roundtrip; prop_max_bytes; prop_deterministic ]
  in
  Alcotest.run
    "varint"
    [ "uint64 roundtrip", uint64_rt_tests
    ; "int64 roundtrip", int64_rt_tests
    ; "uint64 byte lengths", uint64_len_tests
    ; "int64 byte lengths", int64_len_tests
    ; "decode at offset", [ Alcotest.test_case "two values" `Quick test_decode_at_offset ]
    ; "oob raises", [ Alcotest.test_case "offset past end" `Quick test_oob_raises ]
    ; "qcheck", qcheck_tests
    ]
;;
