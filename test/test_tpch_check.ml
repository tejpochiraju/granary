(* #482 — unit tests for the TPC-H answer cross-check (#502, #504). *)

module C = Granary_tpc.Tpch_check

(* One boolean argument each, so a call site cannot transpose expected and
   actual. *)
let is_true msg v = Alcotest.(check bool) msg true v
let is_false msg v = Alcotest.(check bool) msg false v

let contains haystack needle =
  let n = String.length needle in
  let rec scan i =
    i + n <= String.length haystack && (String.sub haystack i n = needle || scan (i + 1))
  in
  scan 0
;;

(* ── field_eq ─────────────────────────────────────────────────────────────── *)

let test_identical_text_is_equal () =
  is_true "identical strings" (C.field_eq "13" "13");
  is_true "identical text" (C.field_eq "BUILDING" "BUILDING")
;;

let test_float_noise_absorbed () =
  is_true
    "float renderings within the relative epsilon"
    (C.field_eq "3.0000000000000001" "3.0");
  is_false "differing floats" (C.field_eq "3.5" "3.6")
;;

(* #504.1: the float fallback exists to absorb arithmetic noise between two
   engines' float rendering.  Two *integer* literals that differ as text but
   parse to the same number can only come from a TEXT column rendered
   differently (Q22's cntrycode '07' vs '7'), which is a real disagreement. *)
let test_integer_literals_compare_as_text () =
  is_false "leading zero is not numeric equality" (C.field_eq "07" "7");
  is_false "explicit plus is not numeric equality" (C.field_eq "+7" "7");
  is_true "equal integer literals" (C.field_eq "7" "7")
;;

let test_integer_against_float_uses_numbers () =
  is_true "1 vs 1.0 is arithmetic noise" (C.field_eq "1" "1.0");
  is_false "1 vs 2.0" (C.field_eq "1" "2.0")
;;

let test_non_numeric_text_never_numeric () =
  is_false "text is compared verbatim" (C.field_eq "Brand#12" "Brand#13");
  is_false "NULL vs 0" (C.field_eq "NULL" "0")
;;

(* ── compare_rows ─────────────────────────────────────────────────────────── *)

let test_sequence_compare_detects_order () =
  let a = [ [ "1" ]; [ "2" ] ] in
  let b = [ [ "2" ]; [ "1" ] ] in
  is_true
    "row order is a defect under ordered compare"
    (C.compare_rows ~unordered:false a b <> None);
  is_true "same rows in the same order agree" (C.compare_rows ~unordered:false a a = None)
;;

let test_multiset_compare_ignores_order () =
  let a = [ [ "1" ]; [ "2" ] ] in
  let b = [ [ "2" ]; [ "1" ] ] in
  is_true
    "tie-prone queries compare as multisets"
    (C.compare_rows ~unordered:true a b = None);
  is_true
    "a missing row is still a disagreement"
    (C.compare_rows ~unordered:true a [ [ "1" ] ] <> None)
;;

let test_compare_reports_counts () =
  match C.compare_rows ~unordered:false [ [ "1" ] ] [] with
  | None -> Alcotest.fail "differing answers must report a disagreement"
  | Some report ->
    is_true
      "report names both row counts"
      (contains report "granary 1 rows" && contains report "sqlite 0 rows")
;;

(* ── classify ─────────────────────────────────────────────────────────────── *)

let rows r = Some r
let no_rows = None

let test_classify_agreement () =
  is_true
    "agreeing non-empty answers"
    (C.classify
       ~number:1
       ~runnable:true
       ~granary:(rows [ [ "1" ] ])
       ~sqlite:(rows [ [ "1" ] ])
     = C.Agree);
  is_true
    "two empty answers get their own token"
    (C.classify ~number:1 ~runnable:true ~granary:(rows []) ~sqlite:(rows [])
     = C.Agree_both_empty)
;;

let test_classify_mismatch () =
  match
    C.classify
      ~number:1
      ~runnable:true
      ~granary:(rows [ [ "1" ] ])
      ~sqlite:(rows [ [ "2" ] ])
  with
  | C.Mismatch _ -> ()
  | other -> Alcotest.failf "expected MISMATCH, got %s" (C.label other)
;;

(* #502: a query the catalogue asserts is runnable that returns no rows because
   it *errored* must not render as `skipped` — that token is reserved for the
   deliberately-skipped queries and is not counted as a failure. *)
let test_classify_error_distinct_from_skip () =
  is_true
    "a runnable query with no rows is an error"
    (C.classify ~number:1 ~runnable:true ~granary:no_rows ~sqlite:(rows [ [ "1" ] ])
     = C.Errored);
  is_true
    "a sqlite-side failure is an error too"
    (C.classify ~number:1 ~runnable:true ~granary:(rows [ [ "1" ] ]) ~sqlite:no_rows
     = C.Errored);
  is_true
    "only a Skipped query renders as skipped"
    (C.classify ~number:1 ~runnable:false ~granary:no_rows ~sqlite:no_rows = C.Skipped)
;;

let test_labels () =
  Alcotest.(check string) "agree" "ok" (C.label C.Agree);
  Alcotest.(check string) "both empty" "ok-both-empty" (C.label C.Agree_both_empty);
  Alcotest.(check string) "mismatch" "MISMATCH" (C.label (C.Mismatch "x"));
  Alcotest.(check string) "error" "error" (C.label C.Errored);
  Alcotest.(check string) "skipped" "skipped" (C.label C.Skipped)
;;

(* Both a wrong answer and an error on a runnable query fail the run. *)
let test_failure_classification () =
  is_true "mismatch fails" (C.is_failure (C.Mismatch "x"));
  is_true "error fails" (C.is_failure C.Errored);
  is_false "agreement passes" (C.is_failure C.Agree);
  is_false "both-empty passes" (C.is_failure C.Agree_both_empty);
  is_false "skip passes" (C.is_failure C.Skipped)
;;

(* Q10 ties on revenue alone under a LIMIT, so its comparison must be a
   multiset compare; Q1 has a total order and must not be. *)
let test_tie_prone_membership () =
  is_true "Q10 is tie-prone" (C.unordered_compare 10);
  is_false "Q1 is not tie-prone" (C.unordered_compare 1)
;;

let () =
  Alcotest.run
    "tpch_check"
    [ ( "field_eq"
      , [ Alcotest.test_case "identical text" `Quick test_identical_text_is_equal
        ; Alcotest.test_case "float noise absorbed" `Quick test_float_noise_absorbed
        ; Alcotest.test_case
            "integer literals compare as text"
            `Quick
            test_integer_literals_compare_as_text
        ; Alcotest.test_case
            "integer against float compares numerically"
            `Quick
            test_integer_against_float_uses_numbers
        ; Alcotest.test_case "non-numeric text" `Quick test_non_numeric_text_never_numeric
        ] )
    ; ( "compare_rows"
      , [ Alcotest.test_case "ordered compare" `Quick test_sequence_compare_detects_order
        ; Alcotest.test_case "multiset compare" `Quick test_multiset_compare_ignores_order
        ; Alcotest.test_case "report names counts" `Quick test_compare_reports_counts
        ] )
    ; ( "classify"
      , [ Alcotest.test_case "agreement" `Quick test_classify_agreement
        ; Alcotest.test_case "mismatch" `Quick test_classify_mismatch
        ; Alcotest.test_case
            "error is distinct from skip"
            `Quick
            test_classify_error_distinct_from_skip
        ; Alcotest.test_case "labels" `Quick test_labels
        ; Alcotest.test_case "failure classification" `Quick test_failure_classification
        ; Alcotest.test_case "tie-prone membership" `Quick test_tie_prone_membership
        ] )
    ]
;;
