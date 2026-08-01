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

(* #504.3: the generator's state is 48 bits wide and is masked with
   [1 lsl 48], so "a seed pins the dataset" holds only where [int] is 63-bit.
   On a 32-bit target the mask silently wraps; the module refuses to load there
   rather than emit a different dataset under the same seed. *)
let test_requires_a_63_bit_int () =
  Alcotest.(check bool)
    "the platform int is wide enough for the 48-bit LCG state"
    true
    (Sys.int_size >= 63)
;;

let test_nurand_within_range () =
  let r = seeded () in
  for _ = 1 to 10_000 do
    let v = Granary_tpc.Tpc_rand.nurand r ~a:1023 ~x:1 ~y:3000 ~c:17 in
    Alcotest.(check bool) "within [1,3000]" true (v >= 1 && v <= 3000)
  done
;;

let test_nurand_singleton_range () =
  let r = seeded () in
  Alcotest.(check int)
    "x = y yields x"
    5
    (Granary_tpc.Tpc_rand.nurand r ~a:255 ~x:5 ~y:5 ~c:3)
;;

let test_nurand_is_skewed () =
  (* The whole point of NURand: it must NOT be uniform, and it must be *this*
     skew, not just "skewed somehow" — a one-sided ">" bar is satisfied by
     several wrong implementations. Exhaustive enumeration over all 1024 *
     3000 (a, random(x,y)) pairs for a:1023, x:1, y:3000, c:0 gives exactly
     36.4591% landing in [1,1000] (1,120,024 / 3,072,000). A uniform draw
     gives 33.33%; a buggy [lxor]-for-[lor] variant gives 35.53%
     (1,091,616 / 3,072,000, enumerated the same way) and would pass a
     one-sided "> 35%" bar; [land] instead of [lor] gives 99.97%
     (3,071,104 / 3,072,000) and would pass too. Anchoring on a tight
     two-sided band around the true 36.4591%
     rejects all three. n = 200_000 for headroom: at that n the sampling
     stderr for a true share of 0.364591 is about 0.0011, so the band
     [0.358, 0.371] (+-0.0065, ~6 sigma) comfortably contains the true value
     while excluding uniform, lxor, and land by wide margins. *)
  let r = seeded () in
  let n = 200_000 in
  let low = ref 0 in
  for _ = 1 to n do
    if Granary_tpc.Tpc_rand.nurand r ~a:1023 ~x:1 ~y:3000 ~c:0 <= 1000 then incr low
  done;
  let share = float_of_int !low /. float_of_int n in
  Alcotest.(check bool)
    (Printf.sprintf
       "skewed toward low keys at the enumerated rate (%d/%d = %.5f)"
       !low
       n
       share)
    true
    (share >= 0.358 && share <= 0.371)
;;

let test_nurand_x_gt_y_raises () =
  let r = seeded () in
  Alcotest.check_raises
    "x > y raises Invalid_argument"
    (Invalid_argument "Tpc_rand.nurand: x > y")
    (fun () -> ignore (Granary_tpc.Tpc_rand.nurand r ~a:1023 ~x:8 ~y:3 ~c:0))
;;

let test_last_name_endpoints () =
  Alcotest.(check string) "0" "BARBARBAR" (Granary_tpc.Tpc_rand.last_name 0);
  Alcotest.(check string) "999" "EINGEINGEING" (Granary_tpc.Tpc_rand.last_name 999);
  (* Pins the 15-character maximum (all three digits pick a 5-char syllable),
     so the [9,15] bound on qcheck_last_name_alphabet cannot silently rot. *)
  Alcotest.(check string) "111" "OUGHTOUGHTOUGHT" (Granary_tpc.Tpc_rand.last_name 111)
;;

let test_last_name_distinct () =
  let names = List.init 1000 Granary_tpc.Tpc_rand.last_name in
  let uniq = List.sort_uniq String.compare names in
  Alcotest.(check int) "1000 distinct names" 1000 (List.length uniq)
;;

let test_last_name_out_of_range () =
  Alcotest.check_raises
    "negative"
    (Invalid_argument "Tpc_rand.last_name: n out of [0,999]")
    (fun () -> ignore (Granary_tpc.Tpc_rand.last_name (-1)));
  Alcotest.check_raises
    "too large"
    (Invalid_argument "Tpc_rand.last_name: n out of [0,999]")
    (fun () -> ignore (Granary_tpc.Tpc_rand.last_name 1000))
;;

let qcheck_nurand_in_range =
  QCheck.Test.make
    ~name:"nurand stays within [x,y] for arbitrary seeds and bounds"
    ~count:2000
    QCheck.(
      tup5 int (int_range 0 4095) (int_range 0 5000) (int_range 0 5000) (int_range 0 8191))
    (fun (seed, a, p, q, c) ->
       let x = min p q
       and y = max p q in
       let r = Granary_tpc.Tpc_rand.create ~seed in
       (* [c] is drawn from its own non-negative [int_range], not derived from
          [seed] via [abs] — [abs min_int] is still negative, which would feed
          [nurand] a [c] it now rejects. *)
       let v = Granary_tpc.Tpc_rand.nurand r ~a ~x ~y ~c in
       v >= x && v <= y)
;;

let qcheck_last_name_alphabet =
  QCheck.Test.make
    ~name:"last_name is a concatenation of three syllables"
    ~count:1000
    QCheck.(int_range 0 999)
    (fun n ->
       let s = Granary_tpc.Tpc_rand.last_name n in
       (* Syllable lengths run 3-5 ("BAR".."OUGHT"/"CALLY"/"ATION"), so three
          concatenated syllables span [9,15], not [9,12] - e.g. n=77 gives
          "BAR" ^ "CALLY" ^ "CALLY" = 13 chars. *)
       String.for_all (fun c -> c >= 'A' && c <= 'Z') s
       && String.length s >= 9
       && String.length s <= 15)
;;

let () =
  Alcotest.run
    "tpc_rand"
    [ ( "platform"
      , [ Alcotest.test_case "63-bit int required" `Quick test_requires_a_63_bit_int ] )
    ; ( "int_between"
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
    ; ( "nurand"
      , [ Alcotest.test_case "within range" `Quick test_nurand_within_range
        ; Alcotest.test_case "singleton range" `Quick test_nurand_singleton_range
        ; Alcotest.test_case "is skewed" `Quick test_nurand_is_skewed
        ; Alcotest.test_case "x > y raises" `Quick test_nurand_x_gt_y_raises
        ] )
    ; ( "last_name"
      , [ Alcotest.test_case "endpoints" `Quick test_last_name_endpoints
        ; Alcotest.test_case "1000 distinct" `Quick test_last_name_distinct
        ; Alcotest.test_case "out of range raises" `Quick test_last_name_out_of_range
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_int_between_respects_bounds
          ; prop_a_string_length
          ; prop_determinism
          ; qcheck_nurand_in_range
          ; qcheck_last_name_alphabet
          ] )
    ]
;;
