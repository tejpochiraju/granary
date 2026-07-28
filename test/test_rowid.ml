open Granary_encoding.Rowid

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let check_bytes_equal label expected actual =
  if not (Bytes.equal expected actual)
  then
    Alcotest.failf
      "%s: expected %s got %s"
      label
      (Bytes.to_seq expected
       |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
       |> List.of_seq
       |> String.concat "")
      (Bytes.to_seq actual
       |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
       |> List.of_seq
       |> String.concat "")
;;

let hex_of_bytes b =
  Bytes.to_seq b
  |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
  |> List.of_seq
  |> String.concat ""
;;

(* ------------------------------------------------------------------ *)
(* Fixed: order-preserving pairs                                        *)
(* ------------------------------------------------------------------ *)

let order_pairs =
  [ Int64.min_int, -1L
  ; Int64.min_int, 0L
  ; Int64.min_int, Int64.max_int
  ; -100L, -50L
  ; -1L, 0L
  ; -1L, 1L
  ; 0L, 1L
  ; 0L, 127L
  ; 0L, 128L
  ; 1L, Int64.max_int
  ; Int64.sub Int64.max_int 1L, Int64.max_int
  ]
;;

let test_order_preserving () =
  List.iter
    (fun (a, b) ->
       let ea = encode a
       and eb = encode b in
       let cmp = Bytes.compare ea eb in
       if not (cmp < 0)
       then
         Alcotest.failf
           "order violated: encode(%Ld)=%s should be < encode(%Ld)=%s"
           a
           (hex_of_bytes ea)
           b
           (hex_of_bytes eb))
    order_pairs
;;

let test_equal_encodes_equal () =
  List.iter
    (fun v ->
       let ea = encode v
       and eb = encode v in
       if Bytes.compare ea eb <> 0 then Alcotest.failf "encode(%Ld) not equal to itself" v)
    [ 0L; -1L; Int64.min_int; Int64.max_int; 42L ]
;;

(* ------------------------------------------------------------------ *)
(* Fixed: roundtrip                                                     *)
(* ------------------------------------------------------------------ *)

let roundtrip_values =
  [ 0L
  ; 1L
  ; -1L
  ; 42L
  ; -42L
  ; 127L
  ; -127L
  ; 128L
  ; -128L
  ; Int64.max_int
  ; Int64.min_int
  ; Int64.sub Int64.max_int 1L
  ; Int64.add Int64.min_int 1L
  ; 1000000L
  ; -1000000L
  ]
;;

let test_roundtrip () =
  List.iter
    (fun v ->
       let v' = decode (encode v) in
       if not (Int64.equal v v') then Alcotest.failf "roundtrip failed: %Ld -> %Ld" v v')
    roundtrip_values
;;

(* ------------------------------------------------------------------ *)
(* Fixed: encoded length always 8                                       *)
(* ------------------------------------------------------------------ *)

let test_encoded_length () =
  List.iter
    (fun v ->
       let len = Bytes.length (encode v) in
       if len <> 8 then Alcotest.failf "encode(%Ld) length=%d, expected 8" v len)
    roundtrip_values
;;

(* ------------------------------------------------------------------ *)
(* Fixed: boundary encoding values                                      *)
(* ------------------------------------------------------------------ *)

let test_boundary_min_int () =
  (* Int64.min_int XOR 0x8000_0000_0000_0000 = 0x0000_0000_0000_0000 *)
  let expected = Bytes.make 8 '\x00' in
  let actual = encode Int64.min_int in
  check_bytes_equal "encode Int64.min_int" expected actual
;;

let test_boundary_neg1 () =
  (* -1L = 0xFFFF_FFFF_FFFF_FFFF XOR 0x8000_0000_0000_0000 = 0x7FFF_FFFF_FFFF_FFFF *)
  let expected = Bytes.of_string "\x7f\xff\xff\xff\xff\xff\xff\xff" in
  let actual = encode (-1L) in
  check_bytes_equal "encode (-1L)" expected actual
;;

let test_boundary_zero () =
  (* 0L XOR 0x8000_0000_0000_0000 = 0x8000_0000_0000_0000 *)
  let expected = Bytes.of_string "\x80\x00\x00\x00\x00\x00\x00\x00" in
  let actual = encode 0L in
  check_bytes_equal "encode 0L" expected actual
;;

let test_boundary_max_int () =
  (* Int64.max_int = 0x7FFF_FFFF_FFFF_FFFF XOR 0x8000_0000_0000_0000 = 0xFFFF_FFFF_FFFF_FFFF *)
  let expected = Bytes.make 8 '\xff' in
  let actual = encode Int64.max_int in
  check_bytes_equal "encode Int64.max_int" expected actual
;;

(* ------------------------------------------------------------------ *)
(* Fixed: encode-then-sort test                                         *)
(* ------------------------------------------------------------------ *)

let test_sort_agrees () =
  let values = [ 42L; -1L; 0L; Int64.min_int; 100L; -100L; Int64.max_int; 1L ] in
  (* Sort int64 values directly *)
  let sorted_int64 = List.sort Int64.compare values in
  (* Encode, sort by bytes, decode *)
  let encoded = List.map (fun v -> encode v, v) values in
  let sorted_by_bytes = List.sort (fun (ea, _) (eb, _) -> Bytes.compare ea eb) encoded in
  let decoded = List.map snd sorted_by_bytes in
  if not (List.equal Int64.equal sorted_int64 decoded)
  then (
    let fmt l = List.map (Printf.sprintf "%Ld") l |> String.concat ", " in
    Alcotest.failf
      "sort mismatch:\n  by int64: [%s]\n  by bytes: [%s]"
      (fmt sorted_int64)
      (fmt decoded))
;;

(* ------------------------------------------------------------------ *)
(* QCheck property tests                                                *)
(* ------------------------------------------------------------------ *)

let prop_roundtrip =
  QCheck.Test.make ~count:10000 ~name:"rowid roundtrip" QCheck.int64 (fun i ->
    Int64.equal i (decode (encode i)))
;;

let prop_8_bytes =
  QCheck.Test.make ~count:10000 ~name:"always 8 bytes" QCheck.int64 (fun i ->
    Bytes.length (encode i) = 8)
;;

let prop_order_preserving =
  QCheck.Test.make
    ~count:10000
    ~name:"order preserving"
    (QCheck.pair QCheck.int64 QCheck.int64)
    (fun (a, b) ->
       let cmp_int = Int64.compare a b in
       let cmp_bytes = Bytes.compare (encode a) (encode b) in
       (cmp_int = 0 && cmp_bytes = 0)
       || (cmp_int < 0 && cmp_bytes < 0)
       || (cmp_int > 0 && cmp_bytes > 0))
;;

let prop_injective =
  QCheck.Test.make
    ~count:10000
    ~name:"injective"
    (QCheck.pair QCheck.int64 QCheck.int64)
    (fun (a, b) ->
       if Int64.equal a b then true else not (Bytes.equal (encode a) (encode b)))
;;

(* ------------------------------------------------------------------ *)
(* Alcotest suite                                                       *)
(* ------------------------------------------------------------------ *)

let () =
  let alcotest_tests =
    [ ( "order_preserving"
      , [ Alcotest.test_case "fixed pairs" `Quick test_order_preserving ] )
    ; ( "equal_encodes"
      , [ Alcotest.test_case "encode = encode" `Quick test_equal_encodes_equal ] )
    ; "roundtrip", [ Alcotest.test_case "fixed values" `Quick test_roundtrip ]
    ; "encoded_length", [ Alcotest.test_case "always 8" `Quick test_encoded_length ]
    ; ( "boundary_min_int"
      , [ Alcotest.test_case "min_int -> 0x00..00" `Quick test_boundary_min_int ] )
    ; "boundary_neg1", [ Alcotest.test_case "-1 -> 0x7fff..ff" `Quick test_boundary_neg1 ]
    ; "boundary_zero", [ Alcotest.test_case "0 -> 0x8000..00" `Quick test_boundary_zero ]
    ; ( "boundary_max_int"
      , [ Alcotest.test_case "max_int -> 0xff..ff" `Quick test_boundary_max_int ] )
    ; "sort_agrees", [ Alcotest.test_case "encode+sort=int sort" `Quick test_sort_agrees ]
    ]
  in
  let qcheck_tests =
    List.map
      QCheck_alcotest.to_alcotest
      [ prop_roundtrip; prop_8_bytes; prop_order_preserving; prop_injective ]
  in
  Alcotest.run "Rowid" (alcotest_tests @ [ "qcheck", qcheck_tests ])
;;
