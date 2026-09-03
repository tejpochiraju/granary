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
  let report = P.report ~service_ms:[] () in
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
    (contains "generated-SQL families" (P.report ~service_ms:[] ()))
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
    (contains "generated-SQL families" (P.report ~service_ms:[] ()))
;;

let test_empty_table_renders () =
  fresh ();
  Alcotest.(check (list string))
    "no entries"
    []
    (List.map (fun e -> e.P.sql) (P.ranked ()));
  Alcotest.(check bool)
    "report says so"
    true
    (String.length (P.report ~service_ms:[] ()) > 0)
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

(* ── #718: the writer-lock section ───────────────────────────────────────── *)

(* The profiler's own numbers are service time, which spans the writer lock in
   both directions: a COMMIT releases it before the fsync, and a BEGIN that
   finds it held is waiting rather than working.  The lock section is the other
   half of the measurement, and these pin that it is carried through both
   renderings — and, just as importantly, that omitting it leaves the reference
   SQLite arm's output byte-for-byte what it was. *)

let a_lock_report () =
  let ls = Granary_store.Lock_stats.create () in
  Granary_store.Lock_stats.set_clock ls (fun () -> 0.0);
  Granary_store.Lock_stats.note_acquired
    ls
    Granary_store.Lock_stats.Autocheckpoint
    ~waited:0.0
    ~contended:false
    ~blocked_by:None;
  Granary_store.Lock_stats.note_released ls ~at:0.25;
  Granary_store.Lock_stats.note_acquired
    ls
    Granary_store.Lock_stats.Txn
    ~waited:0.125
    ~contended:true
    ~blocked_by:(Some Granary_store.Lock_stats.Autocheckpoint);
  Granary_store.Lock_stats.note_released ls ~at:0.5;
  Granary_store.Lock_stats.report ls
;;

let test_report_carries_the_lock_section () =
  fresh ();
  P.record ~profile:"new_order" ~sql:"BEGIN" ~rows:0 ~secs:0.01;
  let with_lock = P.report ~lock:(a_lock_report ()) ~service_ms:[] () in
  let without = P.report ~service_ms:[] () in
  Alcotest.(check bool)
    "the section is there"
    true
    (contains "writer-lock accounting" with_lock);
  Alcotest.(check bool)
    "naming who the waiter queued behind"
    true
    (contains "blocked by autocheckpoint" with_lock);
  Alcotest.(check bool)
    "and absent for an engine that has no such accounting"
    false
    (contains "writer-lock accounting" without)
;;

(* An empty accumulator renders every duration as 0.000, which is
   indistinguishable from a busy run that never contended — unless the report
   says which.  It does, and the profiler must not swallow that line. *)
let test_report_carries_the_no_clock_disclosure () =
  fresh ();
  let lock = Granary_store.Lock_stats.report (Granary_store.Lock_stats.create ()) in
  Alcotest.(check bool)
    "the missing clock is disclosed through the profiler too"
    true
    (contains "NO CLOCK INSTALLED" (P.report ~lock ~service_ms:[] ()))
;;

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
;;

let with_tmp_csv f =
  let path = Printf.sprintf "/tmp/granary_tpcc_profile_718_%d.csv" (Unix.getpid ()) in
  let cleanup () =
    try Sys.remove path with
    | _ -> ()
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () -> f path)
;;

let test_csv_appends_the_three_lock_tables () =
  fresh ();
  P.record ~profile:"new_order" ~sql:"BEGIN" ~rows:0 ~secs:0.01;
  with_tmp_csv (fun path ->
    P.to_csv ~lock:(a_lock_report ()) ~path ~service_ms:[] ();
    let csv = read_file path in
    Alcotest.(check bool)
      "per-site table"
      true
      (contains "site,clock_installed,acquisitions,contended" csv);
    Alcotest.(check bool)
      "contention matrix"
      true
      (contains "waiter,blocked_by_holder,waits,wait_ms" csv);
    Alcotest.(check bool)
      "integrity row"
      true
      (contains "unattributed_waits,unbalanced_releases,held_at_snapshot" csv);
    Alcotest.(check bool)
      "one row per site, whether or not it acquired anything"
      true
      (contains "commit_sink,true,0,0" csv);
    (* Repeated on every site row on purpose: a row of this file is read on its
       own, cut out by grep, far more often than the file is read whole. *)
    Alcotest.(check int)
      "clock_installed repeated on every site row"
      (List.length Granary_store.Lock_stats.all_sites)
      (List.length
         (List.filter (fun l -> contains ",true," l) (String.split_on_char '\n' csv))))
;;

let test_csv_without_a_lock_report_is_unchanged () =
  fresh ();
  P.record ~profile:"new_order" ~sql:"BEGIN" ~rows:0 ~secs:0.01;
  with_tmp_csv (fun path ->
    P.to_csv ~path ~service_ms:[] ();
    let csv = read_file path in
    Alcotest.(check bool)
      "no lock tables for the reference engine"
      false
      (contains "clock_installed" csv);
    Alcotest.(check bool) "the statement table is still there" true (contains "BEGIN" csv))
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
    ; ( "lock_section_718"
      , [ Alcotest.test_case
            "report carries the lock section"
            `Quick
            test_report_carries_the_lock_section
        ; Alcotest.test_case
            "report carries the no-clock disclosure"
            `Quick
            test_report_carries_the_no_clock_disclosure
        ; Alcotest.test_case
            "csv appends the three lock tables"
            `Quick
            test_csv_appends_the_three_lock_tables
        ; Alcotest.test_case
            "csv without a lock report is unchanged"
            `Quick
            test_csv_without_a_lock_report_is_unchanged
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
