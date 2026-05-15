open Sqlocaml_encoding.Index_key

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let cmp_bytes a b = Bytes.compare a b

let sign x = if x < 0 then -1 else if x > 0 then 1 else 0

(* ------------------------------------------------------------------ *)
(* Unit tests — NULL                                                    *)
(* ------------------------------------------------------------------ *)

let test_null_encodes_to_one_byte () =
  let enc = encode_value IK_null in
  Alcotest.(check int) "null length" 1 (Bytes.length enc);
  Alcotest.(check int) "null byte" 0x00 (Bytes.get_uint8 enc 0)

let test_null_less_than_int_zero () =
  let enc_null = encode_value IK_null in
  let enc_zero = encode_value (IK_int 0L) in
  if not (cmp_bytes enc_null enc_zero < 0) then
    Alcotest.fail "NULL should sort before IK_int 0L"

(* ------------------------------------------------------------------ *)
(* Unit tests — INTEGER ordering                                        *)
(* ------------------------------------------------------------------ *)

let test_int_ordering_min_zero_max () =
  let enc_min  = encode_value (IK_int Int64.min_int) in
  let enc_zero = encode_value (IK_int 0L) in
  let enc_max  = encode_value (IK_int Int64.max_int) in
  if not (cmp_bytes enc_min enc_zero < 0) then
    Alcotest.fail "IK_int min_int should sort before IK_int 0L";
  if not (cmp_bytes enc_zero enc_max < 0) then
    Alcotest.fail "IK_int 0L should sort before IK_int max_int"

let test_null_less_than_min_int () =
  let enc_null = encode_value IK_null in
  let enc_min  = encode_value (IK_int Int64.min_int) in
  if not (cmp_bytes enc_null enc_min < 0) then
    Alcotest.fail "NULL should sort before IK_int min_int"

(* ------------------------------------------------------------------ *)
(* Unit tests — REAL ordering                                           *)
(* ------------------------------------------------------------------ *)

let test_real_ordering_neg_zero_pos () =
  let enc_neg = encode_value (IK_real (-1.0)) in
  let enc_z   = encode_value (IK_real 0.0) in
  let enc_pos = encode_value (IK_real 1.0) in
  if not (cmp_bytes enc_neg enc_z < 0) then
    Alcotest.fail "IK_real -1.0 should sort before IK_real 0.0";
  if not (cmp_bytes enc_z enc_pos < 0) then
    Alcotest.fail "IK_real 0.0 should sort before IK_real 1.0"

let test_neg_zero_less_than_pos_zero () =
  let enc_neg_zero = encode_value (IK_real (-0.0)) in
  let enc_pos_zero = encode_value (IK_real 0.0) in
  if not (cmp_bytes enc_neg_zero enc_pos_zero < 0) then
    Alcotest.fail "-0.0 should sort before +0.0 in encoding"

let test_nan_encodes_as_null () =
  let enc_nan  = encode_value (IK_real Float.nan) in
  let enc_null = encode_value IK_null in
  if not (Bytes.equal enc_nan enc_null) then
    Alcotest.fail "NaN should encode identically to IK_null"

(* ------------------------------------------------------------------ *)
(* Unit tests — TEXT ordering                                           *)
(* ------------------------------------------------------------------ *)

let test_text_ordering () =
  let enc_a   = encode_value (IK_text "a") in
  let enc_aa  = encode_value (IK_text "aa") in
  let enc_b   = encode_value (IK_text "b") in
  if not (cmp_bytes enc_a enc_aa < 0) then
    Alcotest.fail "IK_text \"a\" should sort before IK_text \"aa\"";
  if not (cmp_bytes enc_aa enc_b < 0) then
    Alcotest.fail "IK_text \"aa\" should sort before IK_text \"b\""

let test_text_embedded_null_roundtrip () =
  let s = "a\x00b" in
  let enc = encode_value (IK_text s) in
  (* Decode the single value: we wrap it in an encode/decode round-trip via
     the full encode/decode to exercise the machinery *)
  (match decode (encode [IK_text s] ~rowid:0L) with
  | Error msg -> Alcotest.failf "decode error: %s" msg
  | Ok ([IK_text s'], 0L) ->
    Alcotest.(check string) "text with null" s s'
  | Ok _ ->
    Alcotest.fail "unexpected decode result");
  (* Also verify the encoding doesn't contain a raw 0x00 that would
     confuse later fields — the escape means length > 1+len(s) *)
  let raw_len = String.length s in
  if Bytes.length enc <= 1 + raw_len then
    Alcotest.fail "expected escaping to increase length"

let test_text_embedded_null_order () =
  (* "a\x00" < "a\x00b" < "aa" < "b" lexicographically in SQL semantics,
     and our encoding must preserve that *)
  let a_null    = encode_value (IK_text "a\x00") in
  let a_null_b  = encode_value (IK_text "a\x00b") in
  let aa        = encode_value (IK_text "aa") in
  (* "a\x00" < "a\x00b" *)
  if not (cmp_bytes a_null a_null_b < 0) then
    Alcotest.fail "\"a\\x00\" should sort before \"a\\x00b\"";
  (* "a\x00b" < "aa": 'a'(0x61) then escape 0x00→0x00,0xFF then 'b'; vs 'a' then 'a'(0x61).
     After tag, first byte 'a'='a', second byte: 0x00 < 0x61='a', so yes < *)
  if not (cmp_bytes a_null_b aa < 0) then
    Alcotest.fail "\"a\\x00b\" should sort before \"aa\""

(* ------------------------------------------------------------------ *)
(* Unit tests — BLOB ordering                                           *)
(* ------------------------------------------------------------------ *)

let test_blob_ordering () =
  let enc_empty = encode_value (IK_blob (Bytes.of_string "")) in
  let enc_one   = encode_value (IK_blob (Bytes.of_string "\x01")) in
  (* Encoding: only 0x00 bytes are escaped (to 0x00 0xFF); terminator is 0x00 0x00.
     Empty blob: [tag, 0x00, 0x00]. Blob "\x01": [tag, 0x01, 0x00, 0x00].
     Compare: tag equal; 0x00 < 0x01 → empty < "\x01" ✓ *)
  if not (cmp_bytes enc_empty enc_one < 0) then
    Alcotest.fail "IK_blob \"\" should sort before IK_blob \"\\x01\""

(* ------------------------------------------------------------------ *)
(* Unit tests — cross-type ordering                                     *)
(* ------------------------------------------------------------------ *)

let test_cross_type_ordering () =
  let enc_null = encode_value IK_null in
  let enc_int  = encode_value (IK_int 0L) in
  let enc_real = encode_value (IK_real 0.0) in
  let enc_text = encode_value (IK_text "") in
  let enc_blob = encode_value (IK_blob Bytes.empty) in
  if not (cmp_bytes enc_null enc_int  < 0) then Alcotest.fail "NULL < INT";
  if not (cmp_bytes enc_int  enc_real < 0) then Alcotest.fail "INT < REAL";
  if not (cmp_bytes enc_real enc_text < 0) then Alcotest.fail "REAL < TEXT";
  if not (cmp_bytes enc_text enc_blob < 0) then Alcotest.fail "TEXT < BLOB"

(* ------------------------------------------------------------------ *)
(* Unit tests — round-trip                                              *)
(* ------------------------------------------------------------------ *)

let test_roundtrip_int_zero () =
  let v = IK_int 0L in
  let enc = encode_value v in
  match decode (Bytes.cat enc (Bytes.create 8)) with
  | Error msg -> Alcotest.failf "decode error: %s" msg
  | Ok ([IK_int 0L], _) -> ()
  | Ok _ -> Alcotest.fail "unexpected result"

let test_roundtrip_text_hello () =
  match decode (encode [IK_text "hello"] ~rowid:0L) with
  | Error msg -> Alcotest.failf "decode error: %s" msg
  | Ok ([IK_text "hello"], 0L) -> ()
  | Ok _ -> Alcotest.fail "unexpected result"

let test_roundtrip_empty_key () =
  match decode (encode [] ~rowid:42L) with
  | Error msg -> Alcotest.failf "decode error: %s" msg
  | Ok ([], 42L) -> ()
  | Ok (_, r) -> Alcotest.failf "wrong rowid: %Ld" r

let test_roundtrip_multi_column () =
  match decode (encode [IK_int 1L; IK_text "x"] ~rowid:99L) with
  | Error msg -> Alcotest.failf "decode error: %s" msg
  | Ok ([IK_int 1L; IK_text "x"], 99L) -> ()
  | Ok _ -> Alcotest.fail "unexpected result"

let test_roundtrip_text_embedded_null () =
  let s = "a\x00b" in
  match decode (encode [IK_text s] ~rowid:0L) with
  | Error msg -> Alcotest.failf "decode error: %s" msg
  | Ok ([IK_text s'], 0L) ->
    Alcotest.(check string) "embedded null text" s s'
  | Ok _ -> Alcotest.fail "unexpected result"

(* ------------------------------------------------------------------ *)
(* QCheck property tests                                                *)
(* ------------------------------------------------------------------ *)

(* QCheck: integer ordering preservation *)
let prop_int_order_preserving =
  QCheck.Test.make ~count:10000 ~name:"int order preserving"
    (QCheck.pair QCheck.int64 QCheck.int64) (fun (a, b) ->
      let cmp_int   = sign (Int64.compare a b) in
      let cmp_bytes = sign (Bytes.compare
                              (encode_value (IK_int a))
                              (encode_value (IK_int b))) in
      cmp_int = cmp_bytes)

(* QCheck: string ordering preservation *)
let prop_text_order_preserving =
  QCheck.Test.make ~count:10000 ~name:"text order preserving"
    (QCheck.pair QCheck.string QCheck.string) (fun (a, b) ->
      let cmp_str   = sign (String.compare a b) in
      let cmp_bytes = sign (Bytes.compare
                              (encode_value (IK_text a))
                              (encode_value (IK_text b))) in
      cmp_str = cmp_bytes)

(* Generator for a single index key value (non-NaN reals) *)
let gen_value =
  let open QCheck.Gen in
  oneof_weighted [
    (1, return IK_null);
    (3, map (fun i -> IK_int i) int64);
    (3, map (fun f ->
      (* Avoid NaN and infinity for clean round-trip testing *)
      let f = if Float.is_nan f || Float.is_infinite f then 0.0 else f in
      IK_real f) float);
    (3, map (fun s -> IK_text s) string);
    (3, map (fun s -> IK_blob (Bytes.of_string s)) string);
  ]

(* Generator for 1-3 column keys *)
let gen_key =
  let open QCheck.Gen in
  let* n = int_range 1 3 in
  let* cols = list_size (return n) gen_value in
  let* rowid = int64 in
  return (cols, rowid)

let arb_key = QCheck.make gen_key

(* QCheck: encode/decode round-trip *)
let prop_encode_decode_roundtrip =
  QCheck.Test.make ~count:10000 ~name:"encode/decode roundtrip"
    arb_key (fun (cols, rowid) ->
      (* Filter out NaN reals — they encode as IK_null so can't round-trip *)
      let cols_clean = List.map (function
        | IK_real f when Float.is_nan f -> IK_null
        | v -> v) cols
      in
      match decode (encode cols_clean ~rowid) with
      | Error _ -> false
      | Ok (cols', rowid') ->
        rowid = rowid' &&
        List.length cols_clean = List.length cols' &&
        List.for_all2 (fun a b ->
          match a, b with
          | IK_null,   IK_null   -> true
          | IK_int x,  IK_int y  -> Int64.equal x y
          | IK_real x, IK_real y -> Int64.equal (Int64.bits_of_float x) (Int64.bits_of_float y)
          | IK_text x, IK_text y -> String.equal x y
          | IK_blob x, IK_blob y -> Bytes.equal x y
          | _, _ -> false
        ) cols_clean cols')

(* QCheck: REAL ordering preserved (non-NaN, non-inf) *)
let prop_real_order_preserving =
  QCheck.Test.make ~count:10000 ~name:"real order preserving"
    (QCheck.pair QCheck.float QCheck.float) (fun (a, b) ->
      (* Skip NaN and infinity *)
      QCheck.assume (not (Float.is_nan a) && not (Float.is_nan b));
      QCheck.assume (not (Float.is_infinite a) && not (Float.is_infinite b));
      let cmp_float = sign (Float.compare a b) in
      let cmp_bytes = sign (Bytes.compare
                               (encode_value (IK_real a))
                               (encode_value (IK_real b))) in
      cmp_float = cmp_bytes)

(* ------------------------------------------------------------------ *)
(* Alcotest suite                                                       *)
(* ------------------------------------------------------------------ *)

let () =
  let unit_tests = [
    "null",   [
      Alcotest.test_case "encodes to 1 byte [0x00]"      `Quick test_null_encodes_to_one_byte;
      Alcotest.test_case "null < IK_int 0L"              `Quick test_null_less_than_int_zero;
      Alcotest.test_case "null < IK_int min_int"         `Quick test_null_less_than_min_int;
    ];
    "integer", [
      Alcotest.test_case "min_int < 0L < max_int"        `Quick test_int_ordering_min_zero_max;
    ];
    "real", [
      Alcotest.test_case "-1.0 < 0.0 < 1.0"             `Quick test_real_ordering_neg_zero_pos;
      Alcotest.test_case "-0.0 < +0.0"                   `Quick test_neg_zero_less_than_pos_zero;
      Alcotest.test_case "NaN encodes as IK_null"        `Quick test_nan_encodes_as_null;
    ];
    "text", [
      Alcotest.test_case "\"a\" < \"aa\" < \"b\""        `Quick test_text_ordering;
      Alcotest.test_case "embedded null roundtrip"        `Quick test_text_embedded_null_roundtrip;
      Alcotest.test_case "embedded null order"            `Quick test_text_embedded_null_order;
    ];
    "blob", [
      Alcotest.test_case "\"\" < \"\\x01\""              `Quick test_blob_ordering;
    ];
    "cross_type", [
      Alcotest.test_case "NULL < INT < REAL < TEXT < BLOB" `Quick test_cross_type_ordering;
    ];
    "roundtrip", [
      Alcotest.test_case "IK_int 0L"                     `Quick test_roundtrip_int_zero;
      Alcotest.test_case "IK_text \"hello\""             `Quick test_roundtrip_text_hello;
      Alcotest.test_case "empty key rowid=42"            `Quick test_roundtrip_empty_key;
      Alcotest.test_case "multi-column [int; text]"      `Quick test_roundtrip_multi_column;
      Alcotest.test_case "text with embedded null"        `Quick test_roundtrip_text_embedded_null;
    ];
  ] in
  let qcheck_tests =
    List.map QCheck_alcotest.to_alcotest [
      prop_int_order_preserving;
      prop_text_order_preserving;
      prop_encode_decode_roundtrip;
      prop_real_order_preserving;
    ]
  in
  Alcotest.run "Index_key" (unit_tests @ ["qcheck", qcheck_tests])
