open Granary_encoding

(* ── Schemas ─────────────────────────────────────────────────────────── *)

let schema_ints : Row.schema =
  [ { Row.name = "a"
    ; ty = Integer
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ; { Row.name = "b"
    ; ty = Integer
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ]
;;

let schema_mixed : Row.schema =
  [ { Row.name = "id"
    ; ty = Integer
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ; { Row.name = "name"
    ; ty = Text
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ]
;;

let schema_texts : Row.schema =
  [ { Row.name = "x"
    ; ty = Text
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ; { Row.name = "y"
    ; ty = Text
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ]
;;

let schema_mixed_rev : Row.schema =
  [ { Row.name = "name"
    ; ty = Text
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ; { Row.name = "id"
    ; ty = Integer
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ]
;;

let schema_4col : Row.schema =
  [ { Row.name = "a"
    ; ty = Integer
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ; { Row.name = "b"
    ; ty = Integer
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ; { Row.name = "c"
    ; ty = Integer
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ; { Row.name = "d"
    ; ty = Integer
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ]
;;

let schema_single_int : Row.schema =
  [ { Row.name = "x"
    ; ty = Integer
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ]
;;

let schema_single_text : Row.schema =
  [ { Row.name = "x"
    ; ty = Text
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ]
;;

let schema_single_real : Row.schema =
  [ { Row.name = "x"
    ; ty = Real
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ]
;;

let schema_single_blob : Row.schema =
  [ { Row.name = "x"
    ; ty = Blob
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ]
;;

let schema_real_blob : Row.schema =
  [ { Row.name = "f"
    ; ty = Real
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ; { Row.name = "b"
    ; ty = Blob
    ; not_null = false
    ; primary_key = false
    ; pk_desc = false
    ; default = None
    ; check_sql = None
    ; generated_as = None
    }
  ]
;;

(* ── Alcotest testable for Row.t ─────────────────────────────────────── *)

let pp_value fmt v =
  match v with
  | Row.V_int n -> Format.fprintf fmt "V_int %Ld" n
  | Row.V_text s -> Format.fprintf fmt "V_text %S" s
  | Row.V_null -> Format.pp_print_string fmt "V_null"
  | Row.V_real f -> Format.fprintf fmt "V_real %h" f
  | Row.V_blob b -> Format.fprintf fmt "V_blob(%d bytes)" (Bytes.length b)
;;

let pp_row fmt arr =
  Format.fprintf fmt "[|";
  Array.iter (fun v -> Format.fprintf fmt " %a;" pp_value v) arr;
  Format.fprintf fmt " |]"
;;

let row_testable = Alcotest.testable pp_row Row.equal

(* ── Category 1: Basic roundtrip ─────────────────────────────────────── *)

let test_roundtrip schema row () =
  let encoded = Row.encode schema row in
  let decoded = Row.decode schema encoded in
  Alcotest.check row_testable "roundtrip" row decoded
;;

let basic_roundtrip_tests =
  [ ( "ints: 42 and 0"
    , fun () -> test_roundtrip schema_ints [| Row.V_int 42L; Row.V_int 0L |] () )
  ; ( "mixed: int + text"
    , fun () -> test_roundtrip schema_mixed [| Row.V_int 1L; Row.V_text "alice" |] () )
  ; ( "texts: hello world"
    , fun () ->
        test_roundtrip schema_texts [| Row.V_text "hello"; Row.V_text "world" |] () )
  ; ( "ints: max_int and min_int"
    , fun () ->
        test_roundtrip
          schema_ints
          [| Row.V_int Int64.max_int; Row.V_int Int64.min_int |]
          () )
  ; ( "mixed rev: empty string + int 0"
    , fun () -> test_roundtrip schema_mixed_rev [| Row.V_text ""; Row.V_int 0L |] () )
  ]
;;

(* ── Category 2: NULL handling ───────────────────────────────────────── *)

let null_tests =
  [ ("all null", fun () -> test_roundtrip schema_mixed [| Row.V_null; Row.V_null |] ())
  ; ( "first null"
    , fun () -> test_roundtrip schema_mixed [| Row.V_null; Row.V_text "hi" |] () )
  ; ("last null", fun () -> test_roundtrip schema_mixed [| Row.V_int 99L; Row.V_null |] ())
  ; ( "alternating nulls (4 col)"
    , fun () ->
        test_roundtrip
          schema_4col
          [| Row.V_null; Row.V_int 1L; Row.V_null; Row.V_int 2L |]
          () )
  ]
;;

(* ── Category 3: Single-column schemas ──────────────────────────────── *)

let single_col_tests =
  [ ("single int 0", fun () -> test_roundtrip schema_single_int [| Row.V_int 0L |] ())
  ; ( "single text"
    , fun () -> test_roundtrip schema_single_text [| Row.V_text "single" |] () )
  ; ("single null int col", fun () -> test_roundtrip schema_single_int [| Row.V_null |] ())
  ]
;;

(* ── Category 4: Edge cases ──────────────────────────────────────────── *)

let edge_tests =
  [ ( "empty text roundtrip"
    , fun () -> test_roundtrip schema_single_text [| Row.V_text "" |] () )
  ; ( "long text 10000 chars"
    , fun () ->
        test_roundtrip schema_single_text [| Row.V_text (String.make 10000 'a') |] () )
  ; ( "special chars in text (binary)"
    , fun () -> test_roundtrip schema_single_text [| Row.V_text "\x00\x01\xff" |] () )
  ; ( "negative int64: -1"
    , fun () -> test_roundtrip schema_single_int [| Row.V_int (-1L) |] () )
  ; ( "negative int64: min_int"
    , fun () -> test_roundtrip schema_single_int [| Row.V_int Int64.min_int |] () )
  ; ("zero int64", fun () -> test_roundtrip schema_single_int [| Row.V_int 0L |] ())
  ; ( "zero-column roundtrip"
    , fun () ->
        let schema : Row.schema = [] in
        let row : Row.t = [||] in
        let encoded = Row.encode schema row in
        let decoded = Row.decode schema encoded in
        Alcotest.(check bool) "zero-col roundtrip" true (Row.equal row decoded) )
  ]
;;

(* ── Category 5: equal function ──────────────────────────────────────── *)

let equal_tests =
  [ ( "equal ints same"
    , fun () ->
        Alcotest.(check bool) "eq" true (Row.equal [| Row.V_int 1L |] [| Row.V_int 1L |])
    )
  ; ( "equal ints different"
    , fun () ->
        Alcotest.(check bool)
          "neq"
          false
          (Row.equal [| Row.V_int 1L |] [| Row.V_int 2L |]) )
  ; ( "equal null null"
    , fun () ->
        Alcotest.(check bool) "eq" true (Row.equal [| Row.V_null |] [| Row.V_null |]) )
  ; ( "equal null vs int0"
    , fun () ->
        Alcotest.(check bool) "neq" false (Row.equal [| Row.V_null |] [| Row.V_int 0L |])
    )
  ; ( "equal text same"
    , fun () ->
        Alcotest.(check bool)
          "eq"
          true
          (Row.equal [| Row.V_text "a" |] [| Row.V_text "a" |]) )
  ; ( "equal text different"
    , fun () ->
        Alcotest.(check bool)
          "neq"
          false
          (Row.equal [| Row.V_text "a" |] [| Row.V_text "b" |]) )
  ; ( "equal different lengths"
    , fun () ->
        Alcotest.(check bool)
          "neq"
          false
          (Row.equal [| Row.V_int 1L |] [| Row.V_int 1L; Row.V_int 2L |]) )
  ]
;;

(* ── Category 6: Error conditions ────────────────────────────────────── *)

let encode_arity_too_many () =
  let schema =
    [ { Row.name = "x"
      ; ty = Row.Integer
      ; not_null = false
      ; primary_key = false
      ; pk_desc = false
      ; default = None
      ; check_sql = None
      ; generated_as = None
      }
    ]
  in
  (* Row has 2 elements but schema expects 1 *)
  let row = [| Row.V_int 1L; Row.V_int 2L |] in
  try
    ignore (Row.encode schema row);
    Alcotest.fail "expected invalid_arg for too-many columns"
  with
  | Invalid_argument _ -> ()
;;

let decode_arity_too_few_encoded () =
  (* Encode a 1-column row, decode against a 2-column schema.
     Row.decode now fills missing columns with NULL instead of raising. *)
  let schema1 =
    [ { Row.name = "x"
      ; ty = Row.Integer
      ; not_null = false
      ; primary_key = false
      ; pk_desc = false
      ; default = None
      ; check_sql = None
      ; generated_as = None
      }
    ]
  in
  let schema2 =
    [ { Row.name = "x"
      ; ty = Row.Integer
      ; not_null = false
      ; primary_key = false
      ; pk_desc = false
      ; default = None
      ; check_sql = None
      ; generated_as = None
      }
    ; { Row.name = "y"
      ; ty = Row.Text
      ; not_null = false
      ; primary_key = false
      ; pk_desc = false
      ; default = None
      ; check_sql = None
      ; generated_as = None
      }
    ]
  in
  let encoded = Row.encode schema1 [| Row.V_int 42L |] in
  let decoded = Row.decode schema2 encoded in
  Alcotest.(check int) "2 columns" 2 (Array.length decoded);
  Alcotest.(check bool) "col0=42" true (decoded.(0) = Row.V_int 42L);
  Alcotest.(check bool) "col1=null" true (decoded.(1) = Row.V_null)
;;

let error_tests =
  [ ( "encode arity mismatch: too few cols"
    , fun () ->
        try
          let _ = Row.encode schema_ints [| Row.V_int 1L |] in
          Alcotest.fail "expected Invalid_argument"
        with
        | Invalid_argument _ -> () )
  ; ( "encode arity mismatch: too many cols"
    , fun () ->
        try
          let _ = Row.encode schema_single_int [| Row.V_int 1L; Row.V_int 2L |] in
          Alcotest.fail "expected Invalid_argument"
        with
        | Invalid_argument _ -> () )
  ; ( "decode arity mismatch: wrong varint in bytes"
    , fun () ->
        (* encode a 2-col row, then try to decode as 1-col schema *)
        let encoded = Row.encode schema_ints [| Row.V_int 1L; Row.V_int 2L |] in
        try
          let _ = Row.decode schema_single_int encoded in
          Alcotest.fail "expected Invalid_argument"
        with
        | Invalid_argument _ -> () )
  ; ( "decode short row: extra columns become NULL"
    , fun () -> decode_arity_too_few_encoded () )
  ; ("encode arity mismatch: row too many for schema", fun () -> encode_arity_too_many ())
  ; ( "type mismatch: int in text column"
    , fun () ->
        let schema =
          [ { Row.name = "x"
            ; ty = Row.Text
            ; not_null = false
            ; primary_key = false
            ; pk_desc = false
            ; default = None
            ; check_sql = None
            ; generated_as = None
            }
          ]
        in
        let row = [| Row.V_int 42L |] in
        try
          let _ = Row.encode schema row in
          Alcotest.fail "expected Invalid_argument for int in text column"
        with
        | Invalid_argument _ -> () )
  ; ( "type mismatch: text in integer column"
    , fun () ->
        let schema =
          [ { Row.name = "x"
            ; ty = Row.Integer
            ; not_null = false
            ; primary_key = false
            ; pk_desc = false
            ; default = None
            ; check_sql = None
            ; generated_as = None
            }
          ]
        in
        let row = [| Row.V_text "hello" |] in
        try
          let _ = Row.encode schema row in
          Alcotest.fail "expected Invalid_argument for text in integer column"
        with
        | Invalid_argument _ -> () )
  ]
;;

(* ── Category 7: QCheck property tests ──────────────────────────────── *)

let value_gen (col : Row.column) =
  QCheck.Gen.(
    oneof_weighted
      [ 1, return Row.V_null
      ; ( 9
        , match col.ty with
          | Row.Integer -> map (fun n -> Row.V_int n) int64
          | Row.Text ->
            map
              (fun s -> Row.V_text s)
              (string_size ~gen:(char_range 'a' 'z') (int_range 0 100))
          | Row.Real -> map (fun f -> Row.V_real f) float
          | Row.Blob ->
            map
              (fun s -> Row.V_blob (Bytes.of_string s))
              (string_size ~gen:(char_range '\x00' '\xff') (int_range 0 100)) )
      ])
;;

let row_gen schema =
  QCheck.Gen.(
    let gens = List.map value_gen schema in
    flatten_list gens |> map Array.of_list)
;;

let prop_roundtrip schema name =
  QCheck.Test.make
    ~count:10_000
    ~name
    (QCheck.make (row_gen schema))
    (fun row ->
       let encoded = Row.encode schema row in
       let decoded = Row.decode schema encoded in
       Row.equal row decoded)
;;

let prop_roundtrip_mixed = prop_roundtrip schema_mixed "prop_roundtrip_mixed"
let prop_roundtrip_ints = prop_roundtrip schema_ints "prop_roundtrip_ints"
let prop_roundtrip_texts = prop_roundtrip schema_texts "prop_roundtrip_texts"
let prop_roundtrip_real = prop_roundtrip schema_single_real "prop_roundtrip_real"
let prop_roundtrip_blob = prop_roundtrip schema_single_blob "prop_roundtrip_blob"
let prop_roundtrip_real_blob = prop_roundtrip schema_real_blob "prop_roundtrip_real_blob"

let prop_encode_deterministic =
  QCheck.Test.make
    ~count:5000
    ~name:"prop_encode_deterministic"
    (QCheck.make (row_gen schema_mixed))
    (fun row ->
       let b1 = Row.encode schema_mixed row in
       let b2 = Row.encode schema_mixed row in
       Bytes.equal b1 b2)
;;

let prop_encode_deterministic_real =
  QCheck.Test.make
    ~count:10_000
    ~name:"prop_encode_deterministic_real"
    (QCheck.make (row_gen schema_real_blob))
    (fun row ->
       let b1 = Row.encode schema_real_blob row in
       let b2 = Row.encode schema_real_blob row in
       Bytes.equal b1 b2)
;;

(* ── Category 8: REAL roundtrip ─────────────────────────────────────── *)

let real_roundtrip_tests =
  [ ("real: pi", fun () -> test_roundtrip schema_single_real [| Row.V_real Float.pi |] ())
  ; ("real: zero", fun () -> test_roundtrip schema_single_real [| Row.V_real 0.0 |] ())
  ; ( "real: negative"
    , fun () -> test_roundtrip schema_single_real [| Row.V_real (-1.5) |] () )
  ; ( "real: infinity"
    , fun () -> test_roundtrip schema_single_real [| Row.V_real Float.infinity |] () )
  ; ( "real: neg_infinity"
    , fun () -> test_roundtrip schema_single_real [| Row.V_real Float.neg_infinity |] ()
    )
  ; ( "real: max_float"
    , fun () -> test_roundtrip schema_single_real [| Row.V_real Float.max_float |] () )
  ; ( "real: min_float"
    , fun () -> test_roundtrip schema_single_real [| Row.V_real Float.min_float |] () )
  ; ( "real: null in real col"
    , fun () -> test_roundtrip schema_single_real [| Row.V_null |] () )
  ; ( "real: NaN (bitwise)"
    , fun () ->
        let nan_val = Float.nan in
        let encoded = Row.encode schema_single_real [| Row.V_real nan_val |] in
        let decoded = Row.decode schema_single_real encoded in
        (* NaN != NaN by Float.equal, compare via bits *)
        match decoded.(0) with
        | Row.V_real f ->
          let orig_bits = Int64.bits_of_float nan_val in
          let dec_bits = Int64.bits_of_float f in
          Alcotest.(check bool) "NaN bits preserved" true (Int64.equal orig_bits dec_bits)
        | _ -> Alcotest.fail "expected V_real" )
  ]
;;

(* ── Category 9: BLOB roundtrip ─────────────────────────────────────── *)

let blob_roundtrip_tests =
  [ ( "blob: empty"
    , fun () -> test_roundtrip schema_single_blob [| Row.V_blob Bytes.empty |] () )
  ; ( "blob: single byte"
    , fun () ->
        test_roundtrip schema_single_blob [| Row.V_blob (Bytes.make 1 '\xff') |] () )
  ; ( "blob: binary data"
    , fun () ->
        test_roundtrip
          schema_single_blob
          [| Row.V_blob (Bytes.of_string "\x00\x01\x02\xfe\xff") |]
          () )
  ; ( "blob: 1000 bytes"
    , fun () ->
        test_roundtrip schema_single_blob [| Row.V_blob (Bytes.make 1000 '\xab') |] () )
  ; ( "blob: null in blob col"
    , fun () -> test_roundtrip schema_single_blob [| Row.V_null |] () )
  ; ( "blob: all zero bytes"
    , fun () ->
        test_roundtrip schema_single_blob [| Row.V_blob (Bytes.make 16 '\x00') |] () )
  ]
;;

(* ── Category 10: REAL + BLOB mixed schema ───────────────────────────── *)

let real_blob_mixed_tests =
  [ ( "real+blob: both present"
    , fun () ->
        test_roundtrip
          schema_real_blob
          [| Row.V_real 3.14; Row.V_blob (Bytes.of_string "hello") |]
          () )
  ; ( "real+blob: first null"
    , fun () ->
        test_roundtrip
          schema_real_blob
          [| Row.V_null; Row.V_blob (Bytes.of_string "x") |]
          () )
  ; ( "real+blob: second null"
    , fun () -> test_roundtrip schema_real_blob [| Row.V_real (-0.0); Row.V_null |] () )
  ; ( "real+blob: both null"
    , fun () -> test_roundtrip schema_real_blob [| Row.V_null; Row.V_null |] () )
  ]
;;

(* ── Category 11: value_equal for new types ─────────────────────────── *)

let value_equal_new_tests =
  [ ( "real equal same"
    , fun () ->
        Alcotest.(check bool)
          "eq"
          true
          (Row.equal [| Row.V_real 1.0 |] [| Row.V_real 1.0 |]) )
  ; ( "real equal different"
    , fun () ->
        Alcotest.(check bool)
          "neq"
          false
          (Row.equal [| Row.V_real 1.0 |] [| Row.V_real 2.0 |]) )
  ; ( "real vs null"
    , fun () ->
        Alcotest.(check bool)
          "neq"
          false
          (Row.equal [| Row.V_real 0.0 |] [| Row.V_null |]) )
  ; ( "blob equal same"
    , fun () ->
        Alcotest.(check bool)
          "eq"
          true
          (Row.equal
             [| Row.V_blob (Bytes.of_string "ab") |]
             [| Row.V_blob (Bytes.of_string "ab") |]) )
  ; ( "blob equal different"
    , fun () ->
        Alcotest.(check bool)
          "neq"
          false
          (Row.equal
             [| Row.V_blob (Bytes.of_string "ab") |]
             [| Row.V_blob (Bytes.of_string "ac") |]) )
  ; ( "blob vs null"
    , fun () ->
        Alcotest.(check bool)
          "neq"
          false
          (Row.equal [| Row.V_blob Bytes.empty |] [| Row.V_null |]) )
  ]
;;

(* ── Category 12: Type-mismatch errors for new types ────────────────── *)

let type_mismatch_new_tests =
  [ ( "real in integer col"
    , fun () ->
        let schema =
          [ { Row.name = "x"
            ; ty = Row.Integer
            ; not_null = false
            ; primary_key = false
            ; pk_desc = false
            ; default = None
            ; check_sql = None
            ; generated_as = None
            }
          ]
        in
        let row = [| Row.V_real 1.5 |] in
        match Row.encode schema row with
        | _ -> Alcotest.fail "expected Invalid_argument"
        | exception Invalid_argument _ -> () )
  ; ( "blob in integer col"
    , fun () ->
        let schema =
          [ { Row.name = "x"
            ; ty = Row.Integer
            ; not_null = false
            ; primary_key = false
            ; pk_desc = false
            ; default = None
            ; check_sql = None
            ; generated_as = None
            }
          ]
        in
        let row = [| Row.V_blob Bytes.empty |] in
        match Row.encode schema row with
        | _ -> Alcotest.fail "expected Invalid_argument"
        | exception Invalid_argument _ -> () )
  ; ( "int in real col"
    , fun () ->
        let schema =
          [ { Row.name = "x"
            ; ty = Row.Real
            ; not_null = false
            ; primary_key = false
            ; pk_desc = false
            ; default = None
            ; check_sql = None
            ; generated_as = None
            }
          ]
        in
        let row = [| Row.V_int 1L |] in
        match Row.encode schema row with
        | _ -> Alcotest.fail "expected Invalid_argument"
        | exception Invalid_argument _ -> () )
  ; ( "text in blob col"
    , fun () ->
        let schema =
          [ { Row.name = "x"
            ; ty = Row.Blob
            ; not_null = false
            ; primary_key = false
            ; pk_desc = false
            ; default = None
            ; check_sql = None
            ; generated_as = None
            }
          ]
        in
        let row = [| Row.V_text "hi" |] in
        match Row.encode schema row with
        | _ -> Alcotest.fail "expected Invalid_argument"
        | exception Invalid_argument _ -> () )
  ; ( "real in blob col"
    , fun () ->
        let schema =
          [ { Row.name = "x"
            ; ty = Row.Blob
            ; not_null = false
            ; primary_key = false
            ; pk_desc = false
            ; default = None
            ; check_sql = None
            ; generated_as = None
            }
          ]
        in
        let row = [| Row.V_real 1.0 |] in
        match Row.encode schema row with
        | _ -> Alcotest.fail "expected Invalid_argument"
        | exception Invalid_argument _ -> () )
  ; ( "blob in real col"
    , fun () ->
        let schema =
          [ { Row.name = "x"
            ; ty = Row.Real
            ; not_null = false
            ; primary_key = false
            ; pk_desc = false
            ; default = None
            ; check_sql = None
            ; generated_as = None
            }
          ]
        in
        let row = [| Row.V_blob Bytes.empty |] in
        match Row.encode schema row with
        | _ -> Alcotest.fail "expected Invalid_argument"
        | exception Invalid_argument _ -> () )
  ]
;;

(* ── Category 13: Bitmap correctness ────────────────────────────────── *)

(* Helper: read the null bitmap from an encoded row.
   Wire format: varint(n_cols) ++ bitmap(ceil(n/8) bytes) ++ values *)
let read_bitmap encoded n_cols =
  (* skip the leading varint *)
  let _n, off = Varint.decode_uint64 encoded 0 in
  let bitmap_bytes = (n_cols + 7) / 8 in
  Bytes.sub encoded off bitmap_bytes
;;

let is_null_bit bitmap col_idx =
  let byte_idx = col_idx / 8
  and bit_idx = col_idx mod 8 in
  (Bytes.get_uint8 bitmap byte_idx lsr bit_idx) land 1 = 1
;;

let bitmap_tests =
  [ ( "bitmap: null at 0, present at 1"
    , fun () ->
        let row = [| Row.V_null; Row.V_int 1L |] in
        let encoded = Row.encode schema_ints row in
        let bm = read_bitmap encoded 2 in
        Alcotest.(check bool) "bit0 set" true (is_null_bit bm 0);
        Alcotest.(check bool) "bit1 clear" false (is_null_bit bm 1) )
  ; ( "bitmap: present at 0, null at 1"
    , fun () ->
        let row = [| Row.V_int 1L; Row.V_null |] in
        let encoded = Row.encode schema_ints row in
        let bm = read_bitmap encoded 2 in
        Alcotest.(check bool) "bit0 clear" false (is_null_bit bm 0);
        Alcotest.(check bool) "bit1 set" true (is_null_bit bm 1) )
  ; ( "bitmap: both null"
    , fun () ->
        let row = [| Row.V_null; Row.V_null |] in
        let encoded = Row.encode schema_ints row in
        let bm = read_bitmap encoded 2 in
        Alcotest.(check bool) "bit0 set" true (is_null_bit bm 0);
        Alcotest.(check bool) "bit1 set" true (is_null_bit bm 1) )
  ; ( "bitmap: both present"
    , fun () ->
        let row = [| Row.V_int 1L; Row.V_int 2L |] in
        let encoded = Row.encode schema_ints row in
        let bm = read_bitmap encoded 2 in
        Alcotest.(check bool) "bit0 clear" false (is_null_bit bm 0);
        Alcotest.(check bool) "bit1 clear" false (is_null_bit bm 1) )
  ]
;;

(* ── Category 14: Null_bitmap shared module functions ────────────────── *)

let null_bitmap_shared_tests =
  [ ( "pack_bits_of_bools: all false"
    , fun () ->
        let b = Null_bitmap.pack_bits_of_bools (fun _ -> false) 4 in
        Alcotest.(check int) "1 byte for 4 bits" 1 (Bytes.length b);
        Alcotest.(check bool) "all zeros" true (b = Bytes.make 1 '\x00') )
  ; ( "pack_bits_of_bools: all true"
    , fun () ->
        let b = Null_bitmap.pack_bits_of_bools (fun _ -> true) 4 in
        Alcotest.(check int) "1 byte for 4 bits" 1 (Bytes.length b);
        Alcotest.(check bool) "all ones lower nibble" true (b = Bytes.make 1 '\x0f') )
  ; ( "pack_bits_of_bools: alternating"
    , fun () ->
        let b = Null_bitmap.pack_bits_of_bools (fun i -> i mod 2 = 0) 4 in
        (* bit0=1, bit1=0, bit2=1, bit3=0 => 0b0101 => 5 *)
        Alcotest.(check bool) "0101" true (b = Bytes.make 1 '\x05') )
  ; ( "pack_bits_of_bools: 9 columns spans 2 bytes"
    , fun () ->
        let b = Null_bitmap.pack_bits_of_bools (fun i -> i = 0 || i = 8) 9 in
        Alcotest.(check int) "2 bytes for 9 bits" 2 (Bytes.length b);
        (* byte0: bit0=1, rest=0 => 1; byte1: bit0=1 (col8's bit), rest=0 => 1 *)
        Alcotest.(check bool) "byte0=1" true (Bytes.get_uint8 b 0 = 1);
        Alcotest.(check bool) "byte1=1" true (Bytes.get_uint8 b 1 = 1) )
  ; ( "unpack_bits_to_bools: 4 col, first null"
    , fun () ->
        let bm = Bytes.make 1 '\x01' in
        let arr = Null_bitmap.unpack_bits_to_bools bm 0 4 in
        Alcotest.(check int) "length 4" 4 (Array.length arr);
        Alcotest.(check bool) "col0 null" true arr.(0);
        Alcotest.(check bool) "col1 not null" false arr.(1);
        Alcotest.(check bool) "col2 not null" false arr.(2);
        Alcotest.(check bool) "col3 not null" false arr.(3) )
  ; ( "unpack_bits_to_bools: 4 col, all null"
    , fun () ->
        let bm = Bytes.make 1 '\x0f' in
        let arr = Null_bitmap.unpack_bits_to_bools bm 0 4 in
        Alcotest.(check bool) "col0 null" true arr.(0);
        Alcotest.(check bool) "col1 null" true arr.(1);
        Alcotest.(check bool) "col2 null" true arr.(2);
        Alcotest.(check bool) "col3 null" true arr.(3) )
  ; ( "unpack_bits_to_bools: roundtrip"
    , fun () ->
        let is_null i = i = 0 || i = 3 || i = 7 in
        let packed = Null_bitmap.pack_bits_of_bools is_null 9 in
        let unpacked = Null_bitmap.unpack_bits_to_bools packed 0 9 in
        for i = 0 to 8 do
          Alcotest.(check bool) (Printf.sprintf "col%d" i) (is_null i) unpacked.(i)
        done )
  ]
;;

(* ── Assemble and run ────────────────────────────────────────────────── *)

let make_tests label pairs =
  List.map (fun (name, fn) -> Alcotest.test_case name `Quick fn) pairs
  |> fun cases -> label, cases
;;

let () =
  let qcheck_tests =
    List.map
      QCheck_alcotest.to_alcotest
      [ prop_roundtrip_mixed
      ; prop_roundtrip_ints
      ; prop_roundtrip_texts
      ; prop_roundtrip_real
      ; prop_roundtrip_blob
      ; prop_roundtrip_real_blob
      ; prop_encode_deterministic
      ; prop_encode_deterministic_real
      ]
  in
  Alcotest.run
    "row"
    [ make_tests "basic roundtrip" basic_roundtrip_tests
    ; make_tests "null handling" null_tests
    ; make_tests "single column" single_col_tests
    ; make_tests "edge cases" edge_tests
    ; make_tests "equal" equal_tests
    ; make_tests "error conditions" error_tests
    ; make_tests "real roundtrip" real_roundtrip_tests
    ; make_tests "blob roundtrip" blob_roundtrip_tests
    ; make_tests "real+blob mixed" real_blob_mixed_tests
    ; make_tests "value equal new types" value_equal_new_tests
    ; make_tests "type mismatch new" type_mismatch_new_tests
    ; make_tests "bitmap correctness" bitmap_tests
    ; make_tests "null_bitmap shared" null_bitmap_shared_tests
    ; "qcheck", qcheck_tests
    ]
;;
