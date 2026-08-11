open Granary_tpc
module P = Tpcc_stmt_profile

let approx name expected actual =
  Alcotest.(check bool)
    (Printf.sprintf "%s: expected ~%g, got %g" name expected actual)
    true
    (Float.abs (expected -. actual) <= 1e-6)
;;

(* Every test starts from a clean table: the accumulator is global, and
   Alcotest runs cases in one process. *)
let fresh () = P.reset ()

let test_aggregates_repeated_calls () =
  fresh ();
  P.record ~profile:"new_order" ~sql:"SELECT 1" ~rows:2 ~secs:0.001;
  P.record ~profile:"new_order" ~sql:"SELECT 1" ~rows:3 ~secs:0.003;
  match P.ranked () with
  | [ e ] ->
    Alcotest.(check int) "calls" 2 e.P.calls;
    Alcotest.(check int) "rows" 5 e.P.rows;
    approx "total_ms" 4.0 e.P.total_ms;
    approx "mean_ms" 2.0 e.P.mean_ms
  | other -> Alcotest.failf "expected 1 entry, got %d" (List.length other)
;;

let test_same_sql_under_two_profiles_stays_distinct () =
  fresh ();
  P.record ~profile:"new_order" ~sql:"COMMIT" ~rows:0 ~secs:0.005;
  P.record ~profile:"payment" ~sql:"COMMIT" ~rows:0 ~secs:0.001;
  let entries = P.ranked () in
  Alcotest.(check int) "two entries" 2 (List.length entries);
  (* new_order is the costlier profile, so it ranks first. *)
  Alcotest.(check string) "first profile" "new_order" (List.hd entries).P.profile
;;

let test_ranking_within_a_profile_is_by_total_desc () =
  fresh ();
  P.record ~profile:"delivery" ~sql:"cheap" ~rows:0 ~secs:0.001;
  P.record ~profile:"delivery" ~sql:"dear" ~rows:0 ~secs:0.009;
  let sqls = List.map (fun e -> e.P.sql) (P.ranked ()) in
  Alcotest.(check (list string)) "dear first" [ "dear"; "cheap" ] sqls
;;

let test_equal_totals_break_ties_on_sql_for_stability () =
  fresh ();
  P.record ~profile:"delivery" ~sql:"bbb" ~rows:0 ~secs:0.002;
  P.record ~profile:"delivery" ~sql:"aaa" ~rows:0 ~secs:0.002;
  let sqls = List.map (fun e -> e.P.sql) (P.ranked ()) in
  Alcotest.(check (list string)) "alphabetical on a tie" [ "aaa"; "bbb" ] sqls
;;

let test_pct_of_profile_sums_to_100_per_profile () =
  fresh ();
  P.record ~profile:"payment" ~sql:"a" ~rows:0 ~secs:0.003;
  P.record ~profile:"payment" ~sql:"b" ~rows:0 ~secs:0.001;
  P.record ~profile:"stock_level" ~sql:"c" ~rows:0 ~secs:0.005;
  let sum p =
    List.fold_left
      (fun acc (e : P.entry) ->
         if String.equal e.P.profile p then acc +. e.P.pct_of_profile else acc)
      0.0
      (P.ranked ())
  in
  approx "payment sums to 100" 100.0 (sum "payment");
  approx "stock_level sums to 100" 100.0 (sum "stock_level")
;;

let test_attributed_pct_against_driver_service_ms () =
  fresh ();
  P.record ~profile:"new_order" ~sql:"a" ~rows:0 ~secs:0.006;
  match P.summaries ~service_ms:[ "new_order", 10.0 ] with
  | [ s ] ->
    approx "statements_total_ms" 6.0 s.P.statements_total_ms;
    approx "driver_service_ms" 10.0 s.P.driver_service_ms;
    approx "attributed_pct" 60.0 s.P.attributed_pct
  | other -> Alcotest.failf "expected 1 summary, got %d" (List.length other)
;;

(* Statement time exceeding the driver's own service time is a real signal —
   clock skew, or a driver window that does not enclose every statement — and
   must render rather than be clamped or treated as impossible. *)
let test_attributed_pct_over_100_renders () =
  fresh ();
  P.record ~profile:"new_order" ~sql:"a" ~rows:0 ~secs:0.020;
  match P.summaries ~service_ms:[ "new_order", 10.0 ] with
  | [ s ] -> approx "attributed_pct" 200.0 s.P.attributed_pct
  | other -> Alcotest.failf "expected 1 summary, got %d" (List.length other)
;;

(* #715 review: a zero denominator used to render [attributed_pct] as [0.0],
   which is indistinguishable from "fully attributed and then some was
   clamped away" — actually it means the opposite, that the driver never
   recorded any service time for this profile at all (e.g. a profile absent
   from [~service_ms], or one whose every attempt raised and exhausted
   [GRANARY_TPCC_RETRIES] so [Tpcc_driver.record_success] never ran). The
   fix reports [Float.infinity] instead, which is unmistakable in the
   rendered table ([inf]) rather than reading as "nothing to see". *)
let test_missing_driver_service_ms_renders_infinity_not_zero () =
  fresh ();
  P.record ~profile:"delivery" ~sql:"a" ~rows:0 ~secs:0.004;
  match P.summaries ~service_ms:[] with
  | [ s ] ->
    approx "driver_service_ms" 0.0 s.P.driver_service_ms;
    Alcotest.(check bool)
      "attributed_pct is +infinity, not 0.0, when the denominator is zero but statement \
       time was recorded"
      true
      (Float.is_infinite s.P.attributed_pct && s.P.attributed_pct > 0.0)
  | other -> Alcotest.failf "expected 1 summary, got %d" (List.length other)
;;

(* #715 review: a middle-eliding [elide] (keeping a head and a tail of the SQL
   around a fixed "..." marker) still collapsed two distinct statement shapes
   into identical-looking rows in the stderr table (the table's only
   human-readable output), because the two shapes below share both a long
   common prefix AND a long common suffix and differ only in a clause sitting
   in the middle — exactly what a fixed-offset window discards. These are the
   actual two shapes from [Tpcc_txn.payment_customer_update]
   (test/tpc/tpcc_txn.ml:748,753) confirmed colliding in the published
   2026-08-11 artifact. [elide] is gone; [report] now prints the full SQL, so
   this asserts the property directly rather than the mechanism: the SQL
   column of the two rendered lines must differ, and must be exactly what was
   recorded. Tested through [report] rather than a private helper, since
   [report] is the function whose output the finding is about. *)
let payment_update_no_data =
  "UPDATE customer SET c_balance = c_balance - ?, c_ytd_payment = c_ytd_payment + ?, \
   c_payment_cnt = c_payment_cnt + 1 WHERE c_w_id = ? AND c_d_id = ? AND c_id = ?"
;;

let payment_update_with_data =
  "UPDATE customer SET c_balance = c_balance - ?, c_ytd_payment = c_ytd_payment + ?, \
   c_payment_cnt = c_payment_cnt + 1, c_data = ? WHERE c_w_id = ? AND c_d_id = ? AND \
   c_id = ?"
;;

let find_substring needle haystack =
  let nl = String.length needle
  and hl = String.length haystack in
  let rec go i =
    if i + nl > hl
    then None
    else if String.sub haystack i nl = needle
    then Some i
    else go (i + 1)
  in
  go 0
;;

let contains needle haystack = Option.is_some (find_substring needle haystack)

(* [report]'s per-entry format is
   "  %-12s %9.1f ms %5.1f%% %6d calls %9d rows %8.3f ms/call  %s\n", so the
   SQL is everything after the fixed "ms/call  " marker (two spaces) — the
   last column, regardless of what the SQL text itself contains. *)
let sql_column_of_line line =
  let marker = "ms/call  " in
  match find_substring marker line with
  | Some i ->
    let start = i + String.length marker in
    String.sub line start (String.length line - start)
  | None -> Alcotest.failf "line has no %S marker: %s" marker line
;;

let test_colliding_prefix_and_suffix_shapes_render_distinct_sql () =
  fresh ();
  P.record ~profile:"payment" ~sql:payment_update_no_data ~rows:0 ~secs:0.288;
  P.record ~profile:"payment" ~sql:payment_update_with_data ~rows:0 ~secs:0.044;
  let report = P.report ~service_ms:[] in
  let lines_mentioning_update =
    String.split_on_char '\n' report
    |> List.filter (fun l -> contains "UPDATE customer" l)
  in
  Alcotest.(check int)
    "two distinct rows, not collapsed to one"
    2
    (List.length lines_mentioning_update);
  match lines_mentioning_update with
  | [ a; b ] ->
    let sql_a = sql_column_of_line a
    and sql_b = sql_column_of_line b in
    Alcotest.(check bool) "the SQL text alone differs" true (sql_a <> sql_b);
    Alcotest.(check string)
      "costlier row keeps its full, unelided SQL"
      payment_update_no_data
      sql_a;
    Alcotest.(check string)
      "cheaper row keeps its full, unelided SQL"
      payment_update_with_data
      sql_b
  | _ -> Alcotest.fail "expected exactly two rows"
;;

(* #715 review: [record] keys on the raw SQL, and [Tpcc_txn.stock_select_sql]
   generates one spelling per district ([s_dist_%02d]), so one logical statement
   fans out into ten keys that [ranked] shows as ten small rows.  [families]
   rolls them back up; these pin that it fires on the fan-out, that it does not
   fire on genuinely distinct shapes, and that it never merges across
   profiles. *)
let stock_select d =
  Printf.sprintf
    "SELECT s_quantity, s_dist_%02d, s_data FROM stock WHERE s_w_id = ? AND s_i_id = ?"
    d
;;

let test_generated_sql_fans_out_and_families_rolls_it_up () =
  fresh ();
  P.record ~profile:"new_order" ~sql:(stock_select 1) ~rows:10 ~secs:0.010;
  P.record ~profile:"new_order" ~sql:(stock_select 2) ~rows:20 ~secs:0.020;
  P.record ~profile:"new_order" ~sql:(stock_select 3) ~rows:30 ~secs:0.030;
  Alcotest.(check int) "ranked still shows the raw keys" 3 (List.length (P.ranked ()));
  match P.families () with
  | [ f ] ->
    Alcotest.(check int) "all three keys merged" 3 f.P.members;
    Alcotest.(check int) "calls summed" 3 f.P.calls;
    Alcotest.(check int) "rows summed" 60 f.P.rows;
    Alcotest.(check (float 0.001)) "total_ms summed" 60.0 f.P.total_ms;
    Alcotest.(check (float 0.001))
      "the family is the whole profile"
      100.0
      f.P.pct_of_profile;
    Alcotest.(check bool)
      "the digits are collapsed in the shape"
      true
      (contains "s_dist_#" f.P.shape)
  | fs -> Alcotest.failf "expected exactly one family, got %d" (List.length fs)
;;

let test_families_ignores_shapes_that_do_not_fan_out () =
  fresh ();
  P.record ~profile:"payment" ~sql:payment_update_no_data ~rows:0 ~secs:0.288;
  P.record ~profile:"payment" ~sql:payment_update_with_data ~rows:0 ~secs:0.044;
  P.record ~profile:"payment" ~sql:"BEGIN" ~rows:0 ~secs:0.010;
  Alcotest.(check int) "no family of one is reported" 0 (List.length (P.families ()));
  Alcotest.(check bool)
    "and the report prints no family section"
    false
    (contains "generated-SQL families" (P.report ~service_ms:[]))
;;

let test_families_never_merge_across_profiles () =
  fresh ();
  P.record ~profile:"new_order" ~sql:(stock_select 1) ~rows:1 ~secs:0.010;
  P.record ~profile:"new_order" ~sql:(stock_select 2) ~rows:1 ~secs:0.010;
  P.record ~profile:"stock_level" ~sql:(stock_select 1) ~rows:1 ~secs:0.010;
  P.record ~profile:"stock_level" ~sql:(stock_select 2) ~rows:1 ~secs:0.010;
  let fams = P.families () in
  Alcotest.(check int) "one family per profile, not one overall" 2 (List.length fams);
  Alcotest.(check (list string))
    "and each keeps its own profile"
    [ "new_order"; "stock_level" ]
    (List.map (fun (f : P.family) -> f.P.profile) fams |> List.sort String.compare);
  Alcotest.(check bool)
    "the report names the rollup"
    true
    (contains "generated-SQL families" (P.report ~service_ms:[]))
;;

let test_empty_table_renders () =
  fresh ();
  Alcotest.(check (list string))
    "no entries"
    []
    (List.map (fun e -> e.P.sql) (P.ranked ()));
  Alcotest.(check bool) "report says so" true (String.length (P.report ~service_ms:[]) > 0)
;;

let test_reset_clears () =
  fresh ();
  P.record ~profile:"payment" ~sql:"a" ~rows:1 ~secs:0.001;
  P.reset ();
  Alcotest.(check int) "cleared" 0 (List.length (P.ranked ()))
;;

(* The gate is off unless the environment asks for it; a benchmark run must
   never be perturbed by instrumentation that shipped enabled by accident. *)
let test_disabled_by_default () =
  Alcotest.(check bool) "GRANARY_TPCC_STMT_PROFILE unset" false P.enabled
;;

(* A mock [ops] that answers every query with one row carrying a single
   numeric column and records nothing of its own.  [Tpcc_txn.run]'s
   StockLevel path reads column 0 of every query's first row twice (the
   district's [d_next_o_id], then the low-stock count) via [required_int],
   which raises [Missing_value] on an empty result set, so an empty-row
   stub (as originally sketched) cannot in fact drive [run] to completion —
   verified against this tree 2026-08-11. One shared stub row is enough for
   both call sites since [run]'s return value is discarded either way. *)
let mock_ops () =
  { Tpcc_txn.query = (fun _ -> Lwt.return [ [ "0" ] ])
  ; Tpcc_txn.exec = (fun _ -> Lwt.return_unit)
  }
;;

let a_stock_level_input =
  Tpcc_txn.Stock_level_input { w_id = 1; d_id = 1; threshold = 15 }
;;

(* The gate is at [Tpcc_txn.run]'s single call site, so with the environment
   variable unset a normal benchmark run records nothing at all. *)
let test_run_is_inert_when_disabled () =
  fresh ();
  Lwt_main.run (Tpcc_txn.run (mock_ops ()) a_stock_level_input);
  Alcotest.(check int) "nothing recorded" 0 (List.length (P.ranked ()))
;;

let () =
  Alcotest.run
    "tpcc_stmt_profile"
    [ ( "accumulator"
      , [ Alcotest.test_case
            "aggregates repeated calls"
            `Quick
            test_aggregates_repeated_calls
        ; Alcotest.test_case
            "same sql under two profiles stays distinct"
            `Quick
            test_same_sql_under_two_profiles_stays_distinct
        ; Alcotest.test_case
            "ranking within a profile is by total desc"
            `Quick
            test_ranking_within_a_profile_is_by_total_desc
        ; Alcotest.test_case
            "equal totals break ties on sql"
            `Quick
            test_equal_totals_break_ties_on_sql_for_stability
        ; Alcotest.test_case
            "pct_of_profile sums to 100"
            `Quick
            test_pct_of_profile_sums_to_100_per_profile
        ; Alcotest.test_case "reset clears" `Quick test_reset_clears
        ; Alcotest.test_case "empty table renders" `Quick test_empty_table_renders
        ; Alcotest.test_case
            "colliding prefix and suffix shapes render distinct sql"
            `Quick
            test_colliding_prefix_and_suffix_shapes_render_distinct_sql
        ; Alcotest.test_case
            "generated sql fans out and families rolls it up"
            `Quick
            test_generated_sql_fans_out_and_families_rolls_it_up
        ; Alcotest.test_case
            "families ignores shapes that do not fan out"
            `Quick
            test_families_ignores_shapes_that_do_not_fan_out
        ; Alcotest.test_case
            "families never merge across profiles"
            `Quick
            test_families_never_merge_across_profiles
        ] )
    ; ( "coverage"
      , [ Alcotest.test_case
            "attributed_pct against driver service_ms"
            `Quick
            test_attributed_pct_against_driver_service_ms
        ; Alcotest.test_case
            "attributed_pct over 100 renders"
            `Quick
            test_attributed_pct_over_100_renders
        ; Alcotest.test_case
            "missing driver service_ms renders infinity, not zero"
            `Quick
            test_missing_driver_service_ms_renders_infinity_not_zero
        ] )
    ; ( "gate"
      , [ Alcotest.test_case "disabled by default" `Quick test_disabled_by_default
        ; Alcotest.test_case
            "run is inert when disabled"
            `Quick
            test_run_is_inert_when_disabled
        ] )
    ]
;;
