let build () =
  let r = Granary_tpc.Tpc_rand.create ~seed:1 in
  Granary_tpc.Tpch_text.pool r ~size:200_000
;;

let test_pool_reaches_requested_size () =
  let p = build () in
  Alcotest.(check bool)
    "pool is at least the requested size"
    true
    (String.length p >= 200_000)
;;

let test_pool_is_deterministic () =
  let a = build () in
  let b = build () in
  Alcotest.(check string) "same seed gives the same pool" a b
;;

let test_pool_contains_grammar_words () =
  let p = build () in
  let contains needle =
    let nl = String.length needle in
    let rec go i =
      i + nl <= String.length p && (String.sub p i nl = needle || go (i + 1))
    in
    go 0
  in
  Alcotest.(check bool) "grammar noun appears" true (contains "packages");
  Alcotest.(check bool) "grammar verb appears" true (contains "sleep")
;;

let test_substring_respects_bounds () =
  let p = build () in
  let r = Granary_tpc.Tpc_rand.create ~seed:9 in
  for _ = 1 to 2000 do
    let s = Granary_tpc.Tpch_text.substring ~pool:p r ~lo:10 ~hi:40 in
    let n = String.length s in
    Alcotest.(check bool) "length within bounds" true (n >= 10 && n <= 40)
  done
;;

let count_occurrences haystack needle =
  let nl = String.length needle in
  let n = ref 0 in
  for i = 0 to String.length haystack - nl do
    if String.sub haystack i nl = needle then incr n
  done;
  !n
;;

let contains_in_order haystack ~first ~second =
  let find_from sub start =
    let nl = String.length sub in
    let hl = String.length haystack in
    let rec go i =
      if i + nl > hl
      then None
      else if String.sub haystack i nl = sub
      then Some i
      else go (i + 1)
    in
    go start
  in
  match find_from first 0 with
  | None -> false
  | Some i ->
    (match find_from second (i + String.length first) with
     | None -> false
     | Some _ -> true)
;;

(* Q13's predicate is `NOT LIKE '%special%requests%'`, evaluated against a
   single order-comment substring (spec range ~19-78 chars), not against the
   whole pool. The bigram comes from noun_phrase's `adjective ^ " " ^ noun`
   form picking "special" then "requests" — rare per draw, but the pool and
   sample sizes below are large enough to make the assertions reliable
   rather than marginal (verified empirically: a 2MB pool at this seed
   contains the literal bigram 38 times, and ~1-in-500 comment-length
   substrings drawn from it exhibit the same shape Q13 filters on). *)
let big_pool () =
  let r = Granary_tpc.Tpc_rand.create ~seed:1 in
  Granary_tpc.Tpch_text.pool r ~size:2_000_000
;;

let test_special_requests_bigram_appears () =
  let p = big_pool () in
  Alcotest.(check bool)
    "the literal bigram 'special requests' occurs in a 2MB pool"
    true
    (count_occurrences p "special requests" > 0)
;;

let test_special_requests_cooccur_in_comment_window () =
  (* Draw several thousand order-comment-length substrings (spec bounds
     ~lo:19 ~hi:78) and require at least one to contain "special" followed
     later by "requests" — the exact shape Q13's LIKE predicate tests. *)
  let p = big_pool () in
  let r = Granary_tpc.Tpc_rand.create ~seed:9 in
  let hit = ref false in
  for _ = 1 to 20_000 do
    let s = Granary_tpc.Tpch_text.substring ~pool:p r ~lo:19 ~hi:78 in
    if contains_in_order s ~first:"special" ~second:"requests" then hit := true
  done;
  Alcotest.(check bool)
    "at least one comment-length substring contains 'special ... requests' in order"
    true
    !hit
;;

let () =
  Alcotest.run
    "tpch_text"
    [ ( "pool"
      , [ Alcotest.test_case "size" `Quick test_pool_reaches_requested_size
        ; Alcotest.test_case "deterministic" `Quick test_pool_is_deterministic
        ; Alcotest.test_case "grammar words" `Quick test_pool_contains_grammar_words
        ; Alcotest.test_case
            "special-requests bigram"
            `Quick
            test_special_requests_bigram_appears
        ; Alcotest.test_case
            "special-requests co-occur in comment window"
            `Quick
            test_special_requests_cooccur_in_comment_window
        ] )
    ; "substring", [ Alcotest.test_case "bounds" `Quick test_substring_respects_bounds ]
    ]
;;
