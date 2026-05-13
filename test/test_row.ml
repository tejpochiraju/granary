open Sqlocaml_encoding

(* ── Schemas ─────────────────────────────────────────────────────────── *)

let schema_ints : Row.schema = [
  { Row.name = "a"; ty = Integer };
  { Row.name = "b"; ty = Integer };
]

let schema_mixed : Row.schema = [
  { Row.name = "id"; ty = Integer };
  { Row.name = "name"; ty = Text };
]

let schema_texts : Row.schema = [
  { Row.name = "x"; ty = Text };
  { Row.name = "y"; ty = Text };
]

let schema_mixed_rev : Row.schema = [
  { Row.name = "name"; ty = Text };
  { Row.name = "id"; ty = Integer };
]

let schema_4col : Row.schema = [
  { Row.name = "a"; ty = Integer };
  { Row.name = "b"; ty = Integer };
  { Row.name = "c"; ty = Integer };
  { Row.name = "d"; ty = Integer };
]

let schema_single_int : Row.schema = [
  { Row.name = "x"; ty = Integer };
]

let schema_single_text : Row.schema = [
  { Row.name = "x"; ty = Text };
]

(* ── Alcotest testable for Row.t ─────────────────────────────────────── *)

let pp_value fmt v = match v with
  | Row.V_int n  -> Format.fprintf fmt "V_int %Ld" n
  | Row.V_text s -> Format.fprintf fmt "V_text %S" s
  | Row.V_null   -> Format.pp_print_string fmt "V_null"

let pp_row fmt arr =
  Format.fprintf fmt "[|";
  Array.iter (fun v -> Format.fprintf fmt " %a;" pp_value v) arr;
  Format.fprintf fmt " |]"

let row_testable =
  Alcotest.testable pp_row Row.equal

(* ── Category 1: Basic roundtrip ─────────────────────────────────────── *)

let test_roundtrip schema row () =
  let encoded = Row.encode schema row in
  let decoded  = Row.decode schema encoded in
  Alcotest.check row_testable "roundtrip" row decoded

let basic_roundtrip_tests = [
  "ints: 42 and 0",
  (fun () -> test_roundtrip schema_ints [| Row.V_int 42L; Row.V_int 0L |] ());

  "mixed: int + text",
  (fun () -> test_roundtrip schema_mixed [| Row.V_int 1L; Row.V_text "alice" |] ());

  "texts: hello world",
  (fun () -> test_roundtrip schema_texts [| Row.V_text "hello"; Row.V_text "world" |] ());

  "ints: max_int and min_int",
  (fun () -> test_roundtrip schema_ints
    [| Row.V_int Int64.max_int; Row.V_int Int64.min_int |] ());

  "mixed rev: empty string + int 0",
  (fun () -> test_roundtrip schema_mixed_rev [| Row.V_text ""; Row.V_int 0L |] ());
]

(* ── Category 2: NULL handling ───────────────────────────────────────── *)

let null_tests = [
  "all null",
  (fun () -> test_roundtrip schema_mixed [| Row.V_null; Row.V_null |] ());

  "first null",
  (fun () -> test_roundtrip schema_mixed [| Row.V_null; Row.V_text "hi" |] ());

  "last null",
  (fun () -> test_roundtrip schema_mixed [| Row.V_int 99L; Row.V_null |] ());

  "alternating nulls (4 col)",
  (fun () -> test_roundtrip schema_4col
    [| Row.V_null; Row.V_int 1L; Row.V_null; Row.V_int 2L |] ());
]

(* ── Category 3: Single-column schemas ──────────────────────────────── *)

let single_col_tests = [
  "single int 0",
  (fun () -> test_roundtrip schema_single_int [| Row.V_int 0L |] ());

  "single text",
  (fun () -> test_roundtrip schema_single_text [| Row.V_text "single" |] ());

  "single null int col",
  (fun () -> test_roundtrip schema_single_int [| Row.V_null |] ());
]

(* ── Category 4: Edge cases ──────────────────────────────────────────── *)

let edge_tests = [
  "empty text roundtrip",
  (fun () -> test_roundtrip schema_single_text [| Row.V_text "" |] ());

  "long text 10000 chars",
  (fun () -> test_roundtrip schema_single_text
    [| Row.V_text (String.make 10000 'a') |] ());

  "special chars in text (binary)",
  (fun () -> test_roundtrip schema_single_text
    [| Row.V_text "\x00\x01\xff" |] ());

  "negative int64: -1",
  (fun () -> test_roundtrip schema_single_int [| Row.V_int (-1L) |] ());

  "negative int64: min_int",
  (fun () -> test_roundtrip schema_single_int [| Row.V_int Int64.min_int |] ());

  "zero int64",
  (fun () -> test_roundtrip schema_single_int [| Row.V_int 0L |] ());

  "zero-column roundtrip",
  (fun () ->
    let schema : Row.schema = [] in
    let row : Row.t = [| |] in
    let encoded = Row.encode schema row in
    let decoded = Row.decode schema encoded in
    Alcotest.(check bool) "zero-col roundtrip" true (Row.equal row decoded));
]

(* ── Category 5: equal function ──────────────────────────────────────── *)

let equal_tests = [
  "equal ints same",
  (fun () -> Alcotest.(check bool) "eq"
    true (Row.equal [| Row.V_int 1L |] [| Row.V_int 1L |]));

  "equal ints different",
  (fun () -> Alcotest.(check bool) "neq"
    false (Row.equal [| Row.V_int 1L |] [| Row.V_int 2L |]));

  "equal null null",
  (fun () -> Alcotest.(check bool) "eq"
    true (Row.equal [| Row.V_null |] [| Row.V_null |]));

  "equal null vs int0",
  (fun () -> Alcotest.(check bool) "neq"
    false (Row.equal [| Row.V_null |] [| Row.V_int 0L |]));

  "equal text same",
  (fun () -> Alcotest.(check bool) "eq"
    true (Row.equal [| Row.V_text "a" |] [| Row.V_text "a" |]));

  "equal text different",
  (fun () -> Alcotest.(check bool) "neq"
    false (Row.equal [| Row.V_text "a" |] [| Row.V_text "b" |]));

  "equal different lengths",
  (fun () -> Alcotest.(check bool) "neq"
    false (Row.equal [| Row.V_int 1L |] [| Row.V_int 1L; Row.V_int 2L |]));
]

(* ── Category 6: Error conditions ────────────────────────────────────── *)

let error_tests = [
  "encode arity mismatch: too few cols",
  (fun () ->
    try
      let _ = Row.encode schema_ints [| Row.V_int 1L |] in
      Alcotest.fail "expected Invalid_argument"
    with Invalid_argument _ -> ());

  "encode arity mismatch: too many cols",
  (fun () ->
    try
      let _ = Row.encode schema_single_int [| Row.V_int 1L; Row.V_int 2L |] in
      Alcotest.fail "expected Invalid_argument"
    with Invalid_argument _ -> ());

  "decode arity mismatch: wrong varint in bytes",
  (fun () ->
    (* encode a 2-col row, then try to decode as 1-col schema *)
    let encoded = Row.encode schema_ints [| Row.V_int 1L; Row.V_int 2L |] in
    try
      let _ = Row.decode schema_single_int encoded in
      Alcotest.fail "expected Invalid_argument"
    with Invalid_argument _ -> ());

  "type mismatch: int in text column",
  (fun () ->
    let schema = [{ Row.name = "x"; ty = Row.Text }] in
    let row = [| Row.V_int 42L |] in
    try
      let _ = Row.encode schema row in
      Alcotest.fail "expected Invalid_argument for int in text column"
    with Invalid_argument _ -> ());

  "type mismatch: text in integer column",
  (fun () ->
    let schema = [{ Row.name = "x"; ty = Row.Integer }] in
    let row = [| Row.V_text "hello" |] in
    try
      let _ = Row.encode schema row in
      Alcotest.fail "expected Invalid_argument for text in integer column"
    with Invalid_argument _ -> ());
]

(* ── Category 7: QCheck property tests ──────────────────────────────── *)

let value_gen (col : Row.column) =
  QCheck.Gen.(
    oneof_weighted [
      1, return Row.V_null;
      9, (match col.ty with
          | Row.Integer -> map (fun n -> Row.V_int n) int64
          | Row.Text ->
            map (fun s -> Row.V_text s)
              (string_size ~gen:(char_range 'a' 'z') (int_range 0 100)))
    ])

let row_gen schema =
  QCheck.Gen.(
    let gens = List.map value_gen schema in
    flatten_list gens |> map Array.of_list)

let prop_roundtrip schema name =
  QCheck.Test.make ~count:5000 ~name
    (QCheck.make (row_gen schema))
    (fun row ->
      let encoded = Row.encode schema row in
      let decoded  = Row.decode schema encoded in
      Row.equal row decoded)

let prop_roundtrip_mixed =
  prop_roundtrip schema_mixed "prop_roundtrip_mixed"

let prop_roundtrip_ints =
  prop_roundtrip schema_ints "prop_roundtrip_ints"

let prop_roundtrip_texts =
  prop_roundtrip schema_texts "prop_roundtrip_texts"

let prop_encode_deterministic =
  QCheck.Test.make ~count:5000 ~name:"prop_encode_deterministic"
    (QCheck.make (row_gen schema_mixed))
    (fun row ->
      let b1 = Row.encode schema_mixed row in
      let b2 = Row.encode schema_mixed row in
      Bytes.equal b1 b2)

(* ── Category 8: Bitmap correctness ─────────────────────────────────── *)

(* Helper: read the null bitmap from an encoded row.
   Wire format: varint(n_cols) ++ bitmap(ceil(n/8) bytes) ++ values *)
let read_bitmap encoded n_cols =
  (* skip the leading varint *)
  let _n, off = Varint.decode_uint64 encoded 0 in
  let bitmap_bytes = (n_cols + 7) / 8 in
  Bytes.sub encoded off bitmap_bytes

let is_null_bit bitmap col_idx =
  let byte_idx = col_idx / 8 and bit_idx = col_idx mod 8 in
  (Bytes.get_uint8 bitmap byte_idx lsr bit_idx) land 1 = 1

let bitmap_tests = [
  "bitmap: null at 0, present at 1",
  (fun () ->
    let row     = [| Row.V_null; Row.V_int 1L |] in
    let encoded = Row.encode schema_ints row in
    let bm      = read_bitmap encoded 2 in
    Alcotest.(check bool) "bit0 set"   true  (is_null_bit bm 0);
    Alcotest.(check bool) "bit1 clear" false (is_null_bit bm 1));

  "bitmap: present at 0, null at 1",
  (fun () ->
    let row     = [| Row.V_int 1L; Row.V_null |] in
    let encoded = Row.encode schema_ints row in
    let bm      = read_bitmap encoded 2 in
    Alcotest.(check bool) "bit0 clear" false (is_null_bit bm 0);
    Alcotest.(check bool) "bit1 set"   true  (is_null_bit bm 1));

  "bitmap: both null",
  (fun () ->
    let row     = [| Row.V_null; Row.V_null |] in
    let encoded = Row.encode schema_ints row in
    let bm      = read_bitmap encoded 2 in
    Alcotest.(check bool) "bit0 set" true (is_null_bit bm 0);
    Alcotest.(check bool) "bit1 set" true (is_null_bit bm 1));

  "bitmap: both present",
  (fun () ->
    let row     = [| Row.V_int 1L; Row.V_int 2L |] in
    let encoded = Row.encode schema_ints row in
    let bm      = read_bitmap encoded 2 in
    Alcotest.(check bool) "bit0 clear" false (is_null_bit bm 0);
    Alcotest.(check bool) "bit1 clear" false (is_null_bit bm 1));
]

(* ── Assemble and run ────────────────────────────────────────────────── *)

let make_tests label pairs =
  List.map (fun (name, fn) -> Alcotest.test_case name `Quick fn) pairs
  |> fun cases -> (label, cases)

let () =
  let qcheck_tests =
    List.map QCheck_alcotest.to_alcotest
      [ prop_roundtrip_mixed
      ; prop_roundtrip_ints
      ; prop_roundtrip_texts
      ; prop_encode_deterministic ]
  in
  Alcotest.run "row" [
    make_tests "basic roundtrip"    basic_roundtrip_tests;
    make_tests "null handling"      null_tests;
    make_tests "single column"      single_col_tests;
    make_tests "edge cases"         edge_tests;
    make_tests "equal"              equal_tests;
    make_tests "error conditions"   error_tests;
    make_tests "bitmap correctness" bitmap_tests;
    "qcheck", qcheck_tests;
  ]
