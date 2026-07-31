let seeded () = Granary_tpc.Tpc_rand.create ~seed:42

let test_int_between_in_range () =
  let r = seeded () in
  for _ = 1 to 10_000 do
    let v = Granary_tpc.Tpc_rand.int_between r ~lo:3 ~hi:7 in
    Alcotest.(check bool) "within [3,7]" true (v >= 3 && v <= 7)
  done
;;

let test_int_between_hits_both_ends () =
  let r = seeded () in
  let saw_lo = ref false
  and saw_hi = ref false in
  for _ = 1 to 10_000 do
    match Granary_tpc.Tpc_rand.int_between r ~lo:3 ~hi:7 with
    | 3 -> saw_lo := true
    | 7 -> saw_hi := true
    | _ -> ()
  done;
  Alcotest.(check bool) "lo is reachable" true !saw_lo;
  Alcotest.(check bool) "hi is reachable" true !saw_hi
;;

let test_int_between_singleton () =
  let r = seeded () in
  Alcotest.(check int)
    "lo = hi yields lo"
    5
    (Granary_tpc.Tpc_rand.int_between r ~lo:5 ~hi:5)
;;

let test_same_seed_same_sequence () =
  let a = Granary_tpc.Tpc_rand.create ~seed:7 in
  let b = Granary_tpc.Tpc_rand.create ~seed:7 in
  for _ = 1 to 1000 do
    Alcotest.(check int)
      "streams agree"
      (Granary_tpc.Tpc_rand.int_between a ~lo:0 ~hi:1_000_000)
      (Granary_tpc.Tpc_rand.int_between b ~lo:0 ~hi:1_000_000)
  done
;;

let test_different_seed_differs () =
  let a = Granary_tpc.Tpc_rand.create ~seed:1 in
  let b = Granary_tpc.Tpc_rand.create ~seed:2 in
  let draw r =
    List.init 50 (fun _ -> Granary_tpc.Tpc_rand.int_between r ~lo:0 ~hi:1_000_000)
  in
  Alcotest.(check bool) "streams differ" false (draw a = draw b)
;;

let test_negative_seed_same_seed_same_sequence () =
  let a = Granary_tpc.Tpc_rand.create ~seed:(-12345) in
  let b = Granary_tpc.Tpc_rand.create ~seed:(-12345) in
  for _ = 1 to 1000 do
    Alcotest.(check int)
      "streams agree for a negative seed"
      (Granary_tpc.Tpc_rand.int_between a ~lo:0 ~hi:1_000_000)
      (Granary_tpc.Tpc_rand.int_between b ~lo:0 ~hi:1_000_000)
  done
;;

let test_negative_seed_produces_in_range_values () =
  let r = Granary_tpc.Tpc_rand.create ~seed:(-987654321) in
  for _ = 1 to 10_000 do
    let v = Granary_tpc.Tpc_rand.int_between r ~lo:3 ~hi:7 in
    Alcotest.(check bool) "within [3,7] for a negative seed" true (v >= 3 && v <= 7)
  done
;;

let test_int_between_lo_gt_hi_raises () =
  let r = seeded () in
  Alcotest.check_raises
    "lo > hi raises Invalid_argument"
    (Invalid_argument "Tpc_rand.int_between: lo > hi")
    (fun () -> ignore (Granary_tpc.Tpc_rand.int_between r ~lo:8 ~hi:3))
;;

(* a_string draws one value off the LCG per character, so its result depends on
   the order in which the characters are filled.  These golden strings pin that
   order: they change if the fill ever stops being left-to-right, which a
   bounds-and-alphabet check alone would not notice. *)
let test_a_string_is_stable_for_a_fixed_seed () =
  let r = Granary_tpc.Tpc_rand.create ~seed:42 in
  let first = Granary_tpc.Tpc_rand.a_string r ~lo:10 ~hi:40 in
  let second = Granary_tpc.Tpc_rand.a_string r ~lo:10 ~hi:40 in
  Alcotest.(check string) "first a_string off seed 42" "7WuYLb2FnEUme8qi" first;
  Alcotest.(check string)
    "second a_string off seed 42"
    "hX43QbRPUclg6KUn5 m9zSlbVb323Nn"
    second;
  (* And a fresh generator on the same seed replays them. *)
  let r2 = Granary_tpc.Tpc_rand.create ~seed:42 in
  Alcotest.(check string)
    "replayed from a fresh generator"
    first
    (Granary_tpc.Tpc_rand.a_string r2 ~lo:10 ~hi:40)
;;

let test_a_string_length_and_alphabet () =
  let r = seeded () in
  for _ = 1 to 2000 do
    let s = Granary_tpc.Tpc_rand.a_string r ~lo:10 ~hi:20 in
    let n = String.length s in
    Alcotest.(check bool) "length within bounds" true (n >= 10 && n <= 20);
    String.iter
      (fun c ->
         let ok =
           (c >= 'a' && c <= 'z')
           || (c >= 'A' && c <= 'Z')
           || (c >= '0' && c <= '9')
           || c = ','
           || c = ' '
         in
         Alcotest.(check bool) "char is in the spec alphabet" true ok)
      s
  done
;;

let test_float_between_decimals () =
  let r = seeded () in
  for _ = 1 to 2000 do
    let v = Granary_tpc.Tpc_rand.float_between r ~lo:1.0 ~hi:2.0 ~decimals:2 in
    Alcotest.(check bool) "within bounds" true (v >= 1.0 && v <= 2.0);
    let scaled = v *. 100.0 in
    Alcotest.(check bool)
      "quantized to 2 decimals"
      true
      (Float.abs (scaled -. Float.round scaled) < 1e-6)
  done
;;

let test_phone_shape () =
  let r = seeded () in
  let s = Granary_tpc.Tpc_rand.phone r ~nation:12 in
  Alcotest.(check int) "phone is 15 chars" 15 (String.length s);
  Alcotest.(check string) "country code is nation + 10" "22" (String.sub s 0 2);
  Alcotest.(check char) "first separator" '-' s.[2];
  Alcotest.(check char) "second separator" '-' s.[6];
  Alcotest.(check char) "third separator" '-' s.[10]
;;

let test_pick_returns_element_from_array () =
  let r = seeded () in
  let choices = [| "a"; "b"; "c"; "d" |] in
  for _ = 1 to 2000 do
    let v = Granary_tpc.Tpc_rand.pick r choices in
    Alcotest.(check bool) "picked value is in the array" true (Array.mem v choices)
  done
;;

let test_pick_reaches_all_elements () =
  let r = seeded () in
  let choices = [| "x"; "y"; "z" |] in
  let seen = Array.map (fun _ -> false) choices in
  for _ = 1 to 5000 do
    let v = Granary_tpc.Tpc_rand.pick r choices in
    Array.iteri (fun i c -> if c = v then seen.(i) <- true) choices
  done;
  Array.iteri
    (fun i _ ->
       Alcotest.(check bool) (Printf.sprintf "index %d reachable" i) true seen.(i))
    choices
;;

let test_pick_empty_array_raises () =
  let r = seeded () in
  Alcotest.check_raises
    "empty choices raises Invalid_argument"
    (Invalid_argument "Tpc_rand.pick: empty choices")
    (fun () -> ignore (Granary_tpc.Tpc_rand.pick r [||]))
;;

let prop_int_between_respects_bounds =
  QCheck.Test.make
    ~count:2000
    ~name:"int_between stays inside its closed interval for any seed and range"
    QCheck.(triple int nat_small nat_small)
    (fun (seed, a, b) ->
       let lo = min a b
       and hi = max a b in
       let r = Granary_tpc.Tpc_rand.create ~seed in
       let v = Granary_tpc.Tpc_rand.int_between r ~lo ~hi in
       v >= lo && v <= hi)
;;

let prop_a_string_length =
  QCheck.Test.make
    ~count:2000
    ~name:"a_string length lands inside the requested bounds for any seed"
    QCheck.(triple int (int_range 0 40) (int_range 0 40))
    (fun (seed, a, b) ->
       let lo = min a b
       and hi = max a b in
       let r = Granary_tpc.Tpc_rand.create ~seed in
       let s = Granary_tpc.Tpc_rand.a_string r ~lo ~hi in
       String.length s >= lo && String.length s <= hi)
;;

let prop_determinism =
  QCheck.Test.make
    ~count:500
    ~name:
      "two generators on the same seed emit identical streams, including negative seeds"
    QCheck.int
    (fun seed ->
       let a = Granary_tpc.Tpc_rand.create ~seed in
       let b = Granary_tpc.Tpc_rand.create ~seed in
       let draw r =
         List.init 20 (fun _ -> Granary_tpc.Tpc_rand.int_between r ~lo:0 ~hi:99999)
       in
       draw a = draw b)
;;

let () =
  Alcotest.run
    "tpc_rand"
    [ ( "int_between"
      , [ Alcotest.test_case "in range" `Quick test_int_between_in_range
        ; Alcotest.test_case "reaches both ends" `Quick test_int_between_hits_both_ends
        ; Alcotest.test_case "singleton range" `Quick test_int_between_singleton
        ; Alcotest.test_case "lo > hi raises" `Quick test_int_between_lo_gt_hi_raises
        ] )
    ; ( "determinism"
      , [ Alcotest.test_case "same seed" `Quick test_same_seed_same_sequence
        ; Alcotest.test_case "different seed" `Quick test_different_seed_differs
        ; Alcotest.test_case
            "negative seed, same seed"
            `Quick
            test_negative_seed_same_seed_same_sequence
        ; Alcotest.test_case
            "negative seed, in range"
            `Quick
            test_negative_seed_produces_in_range_values
        ] )
    ; ( "a_string"
      , [ Alcotest.test_case
            "length and alphabet"
            `Quick
            test_a_string_length_and_alphabet
        ; Alcotest.test_case
            "stable for a fixed seed"
            `Quick
            test_a_string_is_stable_for_a_fixed_seed
        ] )
    ; ( "float"
      , [ Alcotest.test_case "decimal quantization" `Quick test_float_between_decimals ] )
    ; "phone", [ Alcotest.test_case "shape" `Quick test_phone_shape ]
    ; ( "pick"
      , [ Alcotest.test_case
            "returns array element"
            `Quick
            test_pick_returns_element_from_array
        ; Alcotest.test_case "reaches all elements" `Quick test_pick_reaches_all_elements
        ; Alcotest.test_case "empty array raises" `Quick test_pick_empty_array_raises
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_int_between_respects_bounds; prop_a_string_length; prop_determinism ] )
    ]
;;
