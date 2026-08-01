module C = Granary_tpc.Tpcc_check

let cond n = List.find (fun c -> c.C.number = n) C.conditions

let contains_substring haystack needle =
  let hn = String.length needle in
  let hl = String.length haystack in
  let rec loop i =
    if i + hn > hl
    then false
    else if String.sub haystack i hn = needle
    then true
    else loop (i + 1)
  in
  loop 0
;;

let check_contains ~test_name ~report needles =
  List.iter
    (fun needle ->
       Alcotest.(check bool)
         (Printf.sprintf "%s: report mentions %S" test_name needle)
         true
         (contains_substring report needle))
    needles
;;

let test_all_four_present () =
  Alcotest.(check (list int))
    "conditions 1-4"
    [ 1; 2; 3; 4 ]
    (List.map (fun c -> c.C.number) C.conditions)
;;

let test_every_condition_has_a_description () =
  List.iter
    (fun c ->
       Alcotest.(check bool)
         (Printf.sprintf "condition %d described" c.C.number)
         true
         (String.length c.C.description > 0))
    C.conditions
;;

let test_matching_rows_mean_holds () =
  let rows = [ [ [ "1"; "300000" ] ]; [ [ "1"; "300000" ] ] ] in
  Alcotest.(check bool) "holds" true (C.classify (cond 1) ~rows = C.Holds)
;;

let test_rows_mean_violated () =
  let rows = [ [ [ "1"; "300000" ] ]; [ [ "1"; "299990" ] ] ] in
  match C.classify (cond 1) ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 1 violation"
      ~report
      [ "condition 1"; "300000"; "299990" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated"
;;

let test_violated_is_a_failure () =
  Alcotest.(check bool) "violated fails the run" true (C.is_failure (C.Violated "x"));
  Alcotest.(check bool) "holds does not" false (C.is_failure C.Holds);
  (* Not_run is a failure too: a condition that silently did not execute must
     not read as a pass.  This is the #502 lesson — a check that stopped
     running rendered as `skipped` and still exited 0. *)
  Alcotest.(check bool) "not_run fails the run" true (C.is_failure C.Not_run)
;;

let test_labels () =
  Alcotest.(check string) "holds" "ok" (C.label C.Holds);
  Alcotest.(check string) "violated" "VIOLATED" (C.label (C.Violated "x"));
  Alcotest.(check string) "not run" "not-run" (C.label C.Not_run)
;;

let test_queries_mention_their_tables () =
  let mentions c sub = List.exists (fun q -> contains_substring q sub) c.C.queries in
  Alcotest.(check bool) "1 uses warehouse" true (mentions (cond 1) "warehouse");
  Alcotest.(check bool) "1 uses district" true (mentions (cond 1) "district");
  Alcotest.(check bool) "2 uses district" true (mentions (cond 2) "district");
  Alcotest.(check bool) "2 uses orders" true (mentions (cond 2) "orders");
  Alcotest.(check bool) "2 uses new_order" true (mentions (cond 2) "new_order");
  Alcotest.(check bool) "3 uses district" true (mentions (cond 3) "district");
  Alcotest.(check bool) "3 uses new_order" true (mentions (cond 3) "new_order");
  Alcotest.(check bool) "4 uses orders" true (mentions (cond 4) "orders");
  Alcotest.(check bool) "4 uses order_line" true (mentions (cond 4) "order_line");
  Alcotest.(check int) "1 has two queries" 2 (List.length (cond 1).C.queries);
  Alcotest.(check int) "2 has three queries" 3 (List.length (cond 2).C.queries);
  Alcotest.(check int) "3 has two queries" 2 (List.length (cond 3).C.queries);
  Alcotest.(check int) "4 has three queries" 3 (List.length (cond 4).C.queries)
;;

(* No condition may express itself as `x <> (SELECT ...)`: that predicate is
   NULL — and so not returned, and so a pass — whenever the subquery matches
   zero rows, which is exactly the vacuous-pass hole this module's
   client-side joins exist to close.  It is also the shape #485 breaks a
   second way, through an unqualified outer reference.  Asserting on the SQL
   text is crude, but it fails loudly the moment someone "simplifies" a
   condition back into that form, which nothing else here would notice. *)
let test_no_condition_uses_a_scalar_subquery_predicate () =
  List.iter
    (fun c ->
       List.iter
         (fun q ->
            Alcotest.(check bool)
              (Printf.sprintf "condition %d has no subquery in its SQL" c.C.number)
              false
              (contains_substring q "(SELECT"))
         c.C.queries)
    C.conditions
;;

(* --- condition 1 ------------------------------------------------------- *)

(* HOLE A, the reason condition 1 is a client-side join at all: a warehouse
   with no district rows makes SUM(d_ytd) NULL, so the old
   `w_ytd <> (SELECT SUM ...)` predicate was NULL and the row was never
   returned — condition 1 passed for any w_ytd whatsoever. *)
let test_cond1_warehouse_with_no_districts_violates () =
  let rows = [ [ [ "1"; "300000" ] ]; [] ] in
  match C.classify (cond 1) ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 1 warehouse with no districts"
      ~report
      [ "w=1"; "no district rows" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated"
;;

let test_cond1_districts_with_no_warehouse_violates () =
  let rows = [ [ [ "1"; "300000" ] ]; [ [ "1"; "300000" ]; [ "2"; "300000" ] ] ] in
  match C.classify (cond 1) ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 1 orphan district group"
      ~report
      [ "w=2"; "no warehouse row" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated"
;;

let test_cond1_empty_warehouse_violates () =
  match C.classify (cond 1) ~rows:[ []; [] ] with
  | C.Violated report ->
    check_contains ~test_name:"condition 1 empty warehouse" ~report [ "zero rows" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated on an empty warehouse table"
;;

(* Pins the half-cent money tolerance at its boundary.  Nothing else here
   exercises it: the load test's negative case perturbs d_ytd by a whole 1.0,
   so loosening 0.005 to 0.9 would go completely unnoticed.  The tolerance is
   necessary — w_ytd is one accumulator while SUM(d_ytd) re-sums ten
   separately accumulated values, so an exact [=] would report false
   violations after a handful of Payments — but "necessary" is not a licence
   to widen it. *)
let test_cond1_money_tolerance_boundary () =
  let rows ~sum = [ [ [ "1"; "300000" ] ]; [ [ "1"; sum ] ] ] in
  Alcotest.(check bool)
    "0.004 off is within the half-cent tolerance"
    true
    (C.classify (cond 1) ~rows:(rows ~sum:"300000.004") = C.Holds);
  match C.classify (cond 1) ~rows:(rows ~sum:"300000.006") with
  | C.Violated _ -> ()
  | C.Holds | C.Not_run ->
    Alcotest.fail "0.006 off is more than half a cent and must be Violated"
;;

let test_cond1_malformed_row_violates () =
  let rows = [ [ [ "1" ] ]; [ [ "1"; "300000" ] ] ] in
  match C.classify (cond 1) ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 1 malformed row"
      ~report
      [ "unexpected row shape" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated on malformed row"
;;

let test_cond1_unparseable_aggregate_violates () =
  let rows = [ [ [ "1"; "300000" ] ]; [ [ "1"; "three hundred thousand" ] ] ] in
  match C.classify (cond 1) ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 1 unparseable aggregate"
      ~report
      [ "unparseable aggregate" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated on unparseable aggregate"
;;

(* --- condition 2 ------------------------------------------------------- *)

let cond2_rows ~districts ~orders ~new_order = [ districts; orders; new_order ]

let test_cond2_consistent_rows_hold () =
  let rows =
    cond2_rows
      ~districts:[ [ "1"; "1"; "3001" ]; [ "1"; "2"; "3001" ] ]
      ~orders:[ [ "1"; "1"; "3000" ]; [ "1"; "2"; "3000" ] ]
      ~new_order:[ [ "1"; "1"; "3000" ]; [ "1"; "2"; "3000" ] ]
  in
  Alcotest.(check bool) "holds" true (C.classify (cond 2) ~rows = C.Holds)
;;

let test_cond2_orders_max_mismatch_violates () =
  let rows =
    cond2_rows
      ~districts:[ [ "1"; "1"; "3001" ] ]
      ~orders:[ [ "1"; "1"; "2999" ] ]
      ~new_order:[ [ "1"; "1"; "3000" ] ]
  in
  match C.classify (cond 2) ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 2 orders max"
      ~report
      [ "1,1"; "max(o_id) = 2999" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated"
;;

let test_cond2_new_order_max_mismatch_violates () =
  let rows =
    cond2_rows
      ~districts:[ [ "1"; "1"; "3001" ] ]
      ~orders:[ [ "1"; "1"; "3000" ] ]
      ~new_order:[ [ "1"; "1"; "2999" ] ]
  in
  match C.classify (cond 2) ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 2 new_order max"
      ~report
      [ "1,1"; "max(no_o_id) = 2999" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated"
;;

(* HOLE B's orders half: both MAX subqueries were NULL for a district with no
   orders and no new_order rows, so the old form passed for any
   d_next_o_id.  Orders are never deleted in TPC-C, so this one is a
   violation. *)
let test_cond2_district_with_no_orders_violates () =
  let rows = cond2_rows ~districts:[ [ "1"; "1"; "3001" ] ] ~orders:[] ~new_order:[] in
  match C.classify (cond 2) ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 2 district with no orders"
      ~report
      [ "1,1"; "no orders rows" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated"
;;

(* HOLE B's new_order half, and the deliberate decision recorded in
   Tpcc_check: Delivery deletes new_order rows, so a fully delivered district
   legitimately has none.  The max(no_o_id) disjunct is therefore checked only
   when the district HAS new_order rows — examined and consciously skipped,
   not invisibly absent.  The orders disjunct still binds. *)
let test_cond2_district_with_a_drained_queue_holds () =
  let rows =
    cond2_rows
      ~districts:[ [ "1"; "1"; "3001" ] ]
      ~orders:[ [ "1"; "1"; "3000" ] ]
      ~new_order:[]
  in
  Alcotest.(check bool)
    "drained new-order queue holds"
    true
    (C.classify (cond 2) ~rows = C.Holds)
;;

let test_cond2_orphan_aggregate_group_violates () =
  let rows =
    cond2_rows
      ~districts:[ [ "1"; "1"; "3001" ] ]
      ~orders:[ [ "1"; "1"; "3000" ]; [ "1"; "2"; "3000" ] ]
      ~new_order:[ [ "1"; "1"; "3000" ] ]
  in
  match C.classify (cond 2) ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 2 orphan orders group"
      ~report
      [ "1,2"; "no district row" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated"
;;

let test_cond2_empty_district_violates () =
  match C.classify (cond 2) ~rows:[ []; []; [] ] with
  | C.Violated report ->
    check_contains ~test_name:"condition 2 empty district" ~report [ "zero rows" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated on an empty district table"
;;

let test_cond2_malformed_row_violates () =
  let rows =
    cond2_rows
      ~districts:[ [ "1"; "1" ] ]
      ~orders:[ [ "1"; "1"; "3000" ] ]
      ~new_order:[ [ "1"; "1"; "3000" ] ]
  in
  match C.classify (cond 2) ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 2 malformed row"
      ~report
      [ "unexpected row shape" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated on malformed row"
;;

(* Condition 3's check runs entirely in OCaml over the district list and bare
   per-district new_order aggregates, so every branch is cheap to exercise
   directly without an engine.  Rows are [district_rows; new_order_rows]. *)
let test_cond3_check_consistent_rows_hold () =
  let c = cond 3 in
  let rows =
    [ [ [ "1"; "1" ]; [ "1"; "2" ] ]
    ; [ [ "1"; "1"; "3100"; "2201"; "900" ]; [ "1"; "2"; "3100"; "2201"; "900" ] ]
    ]
  in
  Alcotest.(check bool) "holds" true (C.classify c ~rows = C.Holds)
;;

let test_cond3_check_off_by_one_range_violates () =
  let c = cond 3 in
  (* max - min + 1 = 899, but the row count is only 898: an off-by-one gap in
     the new_order id range for district 1 (a hole in the id sequence). *)
  let rows = [ [ [ "1"; "1" ] ]; [ [ "1"; "1"; "3100"; "2202"; "898" ] ] ] in
  match C.classify c ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 3 off-by-one"
      ~report
      [ "1,1"; "3100"; "2202"; "898"; "899" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated"
;;

let test_cond3_check_wrong_shape_row_violates () =
  let c = cond 3 in
  let rows = [ [ [ "1"; "1" ] ]; [ [ "1"; "1"; "not"; "enough"; "cols"; "!" ] ] ] in
  match C.classify c ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 3 malformed row"
      ~report
      [ "unexpected row shape" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated on malformed row"
;;

let test_cond3_check_unparseable_aggregate_violates () =
  let c = cond 3 in
  (* Latent today (granary renders these aggregates as ints), but a SUM/MAX
     that ever came back as a real (e.g. "9500.0") must not raise out of
     classify — it must report as a violation instead. *)
  let rows = [ [ [ "1"; "1" ] ]; [ [ "1"; "1"; "3100.0"; "2201"; "900" ] ] ] in
  match C.classify c ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 3 unparseable aggregate"
      ~report
      [ "unparseable aggregate" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated on unparseable aggregate"
;;

let test_cond3_check_empty_result_violates () =
  let c = cond 3 in
  (* district always has rows in a real run; an empty DRIVING result is a sign
     the query stopped seeing real data, not a vacuously satisfied
     invariant. *)
  let rows = [ []; [] ] in
  match C.classify c ~rows with
  | C.Violated report ->
    check_contains ~test_name:"condition 3 empty result" ~report [ "zero rows" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated on an empty result"
;;

(* The deliberate decision recorded in Tpcc_check: a district with no
   new_order rows is a legitimate steady state (Delivery deletes them), so it
   is examined and consciously skipped.  Under the old new_order-driven
   GROUP BY it produced no group and was never examined at all — same
   outcome, but silently and for the wrong reason. *)
let test_cond3_district_with_a_drained_queue_holds () =
  let c = cond 3 in
  let rows =
    [ [ [ "1"; "1" ]; [ "1"; "2" ] ]; [ [ "1"; "1"; "3100"; "2201"; "900" ] ] ]
  in
  Alcotest.(check bool) "drained district holds" true (C.classify c ~rows = C.Holds)
;;

let test_cond3_new_order_group_with_no_district_violates () =
  let c = cond 3 in
  let rows =
    [ [ [ "1"; "1" ] ]
    ; [ [ "1"; "1"; "3100"; "2201"; "900" ]; [ "1"; "2"; "3100"; "2201"; "900" ] ]
    ]
  in
  match C.classify c ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 3 orphan new_order group"
      ~report
      [ "1,2"; "no district row" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated"
;;

let cond4_rows ~districts ~orders ~lines = [ districts; orders; lines ]

let test_cond4_check_consistent_rows_hold () =
  let c = cond 4 in
  let rows =
    cond4_rows
      ~districts:[ [ "1"; "1" ]; [ "1"; "2" ] ]
      ~orders:[ [ "1"; "1"; "9500" ]; [ "1"; "2"; "9600" ] ]
      ~lines:[ [ "1"; "1"; "9500" ]; [ "1"; "2"; "9600" ] ]
  in
  Alcotest.(check bool) "holds" true (C.classify c ~rows = C.Holds)
;;

let test_cond4_check_mismatched_sum_violates () =
  let c = cond 4 in
  (* district (1,1): orders says 9500 order_line rows, order_line table has
     9499. *)
  let rows =
    cond4_rows
      ~districts:[ [ "1"; "1" ] ]
      ~orders:[ [ "1"; "1"; "9500" ] ]
      ~lines:[ [ "1"; "1"; "9499" ] ]
  in
  match C.classify c ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 4 mismatched sum"
      ~report
      [ "1,1"; "9500"; "9499" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated"
;;

let test_cond4_check_district_missing_on_one_side_violates () =
  let c = cond 4 in
  (* district (1,2) has orders but no order_line rows at all: a missing
     group on one side must not be silently treated as zero and skipped. *)
  let rows =
    cond4_rows
      ~districts:[ [ "1"; "1" ]; [ "1"; "2" ] ]
      ~orders:[ [ "1"; "1"; "9500" ]; [ "1"; "2"; "9600" ] ]
      ~lines:[ [ "1"; "1"; "9500" ] ]
  in
  match C.classify c ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 4 missing from order_line"
      ~report
      [ "1,2"; "9600" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated"
;;

let test_cond4_check_district_missing_on_the_other_side_violates () =
  let c = cond 4 in
  (* the reverse: order_line has a district that orders doesn't. *)
  let rows =
    cond4_rows
      ~districts:[ [ "1"; "1" ]; [ "1"; "2" ] ]
      ~orders:[ [ "1"; "1"; "9500" ] ]
      ~lines:[ [ "1"; "1"; "9500" ]; [ "1"; "2"; "9600" ] ]
  in
  match C.classify c ~rows with
  | C.Violated report ->
    check_contains ~test_name:"condition 4 missing from orders" ~report [ "1,2"; "9600" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated"
;;

let test_cond4_check_malformed_row_violates () =
  let c = cond 4 in
  (* An orders row with the wrong arity must be reported as a violation, not
     silently dropped — dropping it is exactly how this check could go
     vacuously Holds if every row on both sides were malformed. *)
  let rows =
    cond4_rows
      ~districts:[ [ "1"; "1" ] ]
      ~orders:[ [ "1"; "1" ] ]
      ~lines:[ [ "1"; "1"; "9500" ] ]
  in
  match C.classify c ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 4 malformed row"
      ~report
      [ "unexpected row shape" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated on malformed row"
;;

let test_cond4_check_unparseable_aggregate_violates () =
  let c = cond 4 in
  let rows =
    cond4_rows
      ~districts:[ [ "1"; "1" ] ]
      ~orders:[ [ "1"; "1"; "9500.0" ] ]
      ~lines:[ [ "1"; "1"; "9500" ] ]
  in
  match C.classify c ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 4 unparseable aggregate"
      ~report
      [ "unparseable aggregate" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated on unparseable aggregate"
;;

let test_cond4_check_empty_district_violates () =
  let c = cond 4 in
  (* district always has rows in a real run; an empty DRIVING result is a sign
     the check stopped seeing real data, not a vacuous pass. *)
  let rows = cond4_rows ~districts:[] ~orders:[] ~lines:[] in
  match C.classify c ~rows with
  | C.Violated report ->
    check_contains ~test_name:"condition 4 empty district" ~report [ "zero rows" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated on an empty district table"
;;

(* The hole condition 4 had until it was given a driving table: joined against
   each other alone, a district that lost BOTH its orders and its order_line
   rows contributes no key to either side, so the union join had nothing to
   compare and the condition passed vacuously.  Condition 2's orders half
   catches that district too, but condition 4 must not need it to. *)
let test_cond4_district_with_neither_side_violates () =
  let c = cond 4 in
  let rows =
    cond4_rows
      ~districts:[ [ "1"; "1" ]; [ "1"; "2" ] ]
      ~orders:[ [ "1"; "1"; "9500" ] ]
      ~lines:[ [ "1"; "1"; "9500" ] ]
  in
  match C.classify c ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 4 district with neither side"
      ~report
      [ "1,2"; "neither orders nor order_line" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated"
;;

let test_cond4_aggregates_with_no_district_violate () =
  let c = cond 4 in
  let rows =
    cond4_rows
      ~districts:[ [ "1"; "1" ] ]
      ~orders:[ [ "1"; "1"; "9500" ]; [ "1"; "2"; "9600" ] ]
      ~lines:[ [ "1"; "1"; "9500" ]; [ "1"; "2"; "9600" ] ]
  in
  match C.classify c ~rows with
  | C.Violated report ->
    check_contains
      ~test_name:"condition 4 orphan groups"
      ~report
      [ "1,2"; "no district row" ]
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated"
;;

let () =
  Alcotest.run
    "tpcc_check"
    [ ( "catalogue"
      , [ Alcotest.test_case "all four present" `Quick test_all_four_present
        ; Alcotest.test_case "described" `Quick test_every_condition_has_a_description
        ; Alcotest.test_case
            "queries mention their tables"
            `Quick
            test_queries_mention_their_tables
        ; Alcotest.test_case
            "no condition uses a scalar subquery predicate"
            `Quick
            test_no_condition_uses_a_scalar_subquery_predicate
        ] )
    ; ( "classify"
      , [ Alcotest.test_case "matching rows hold" `Quick test_matching_rows_mean_holds
        ; Alcotest.test_case "rows violate" `Quick test_rows_mean_violated
        ; Alcotest.test_case "violated is a failure" `Quick test_violated_is_a_failure
        ; Alcotest.test_case "labels" `Quick test_labels
        ] )
    ; ( "condition 1 (empty groups)"
      , [ Alcotest.test_case
            "a warehouse with no districts violates"
            `Quick
            test_cond1_warehouse_with_no_districts_violates
        ; Alcotest.test_case
            "districts with no warehouse violate"
            `Quick
            test_cond1_districts_with_no_warehouse_violates
        ; Alcotest.test_case
            "an empty warehouse table violates"
            `Quick
            test_cond1_empty_warehouse_violates
        ; Alcotest.test_case
            "the money tolerance boundary"
            `Quick
            test_cond1_money_tolerance_boundary
        ; Alcotest.test_case
            "malformed row violates"
            `Quick
            test_cond1_malformed_row_violates
        ; Alcotest.test_case
            "unparseable aggregate violates"
            `Quick
            test_cond1_unparseable_aggregate_violates
        ] )
    ; ( "condition 2"
      , [ Alcotest.test_case "consistent rows hold" `Quick test_cond2_consistent_rows_hold
        ; Alcotest.test_case
            "an orders max mismatch violates"
            `Quick
            test_cond2_orders_max_mismatch_violates
        ; Alcotest.test_case
            "a new_order max mismatch violates"
            `Quick
            test_cond2_new_order_max_mismatch_violates
        ; Alcotest.test_case
            "a district with no orders violates"
            `Quick
            test_cond2_district_with_no_orders_violates
        ; Alcotest.test_case
            "a district with a drained new-order queue holds"
            `Quick
            test_cond2_district_with_a_drained_queue_holds
        ; Alcotest.test_case
            "an aggregate group with no district violates"
            `Quick
            test_cond2_orphan_aggregate_group_violates
        ; Alcotest.test_case
            "an empty district table violates"
            `Quick
            test_cond2_empty_district_violates
        ; Alcotest.test_case
            "malformed row violates"
            `Quick
            test_cond2_malformed_row_violates
        ] )
    ; ( "condition 3 (#507 workaround)"
      , [ Alcotest.test_case
            "consistent rows hold"
            `Quick
            test_cond3_check_consistent_rows_hold
        ; Alcotest.test_case
            "off-by-one range violates"
            `Quick
            test_cond3_check_off_by_one_range_violates
        ; Alcotest.test_case
            "malformed row violates"
            `Quick
            test_cond3_check_wrong_shape_row_violates
        ; Alcotest.test_case
            "unparseable aggregate violates"
            `Quick
            test_cond3_check_unparseable_aggregate_violates
        ; Alcotest.test_case
            "empty result violates"
            `Quick
            test_cond3_check_empty_result_violates
        ; Alcotest.test_case
            "a district with a drained new-order queue holds"
            `Quick
            test_cond3_district_with_a_drained_queue_holds
        ; Alcotest.test_case
            "a new_order group with no district violates"
            `Quick
            test_cond3_new_order_group_with_no_district_violates
        ] )
    ; ( "condition 4 (#507 workaround)"
      , [ Alcotest.test_case
            "consistent rows hold"
            `Quick
            test_cond4_check_consistent_rows_hold
        ; Alcotest.test_case
            "mismatched sum violates"
            `Quick
            test_cond4_check_mismatched_sum_violates
        ; Alcotest.test_case
            "district missing from order_line violates"
            `Quick
            test_cond4_check_district_missing_on_one_side_violates
        ; Alcotest.test_case
            "district missing from orders violates"
            `Quick
            test_cond4_check_district_missing_on_the_other_side_violates
        ; Alcotest.test_case
            "malformed row violates"
            `Quick
            test_cond4_check_malformed_row_violates
        ; Alcotest.test_case
            "unparseable aggregate violates"
            `Quick
            test_cond4_check_unparseable_aggregate_violates
        ; Alcotest.test_case
            "an empty district table violates"
            `Quick
            test_cond4_check_empty_district_violates
        ; Alcotest.test_case
            "a district with neither orders nor order_line violates"
            `Quick
            test_cond4_district_with_neither_side_violates
        ; Alcotest.test_case
            "aggregate groups with no district violate"
            `Quick
            test_cond4_aggregates_with_no_district_violate
        ] )
    ]
;;
