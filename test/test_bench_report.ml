let test_csv_row_quotes_commas () =
  Alcotest.(check string)
    "comma-bearing field is quoted"
    {|a,"b,c",d|}
    (Granary_tpc.Bench_report.Csv.row [ "a"; "b,c"; "d" ])
;;

let test_csv_row_escapes_quotes () =
  Alcotest.(check string)
    "embedded quote is doubled"
    {|a,"say ""hi""",c|}
    (Granary_tpc.Bench_report.Csv.row [ "a"; {|say "hi"|}; "c" ])
;;

let test_csv_row_quotes_newline () =
  Alcotest.(check string)
    "embedded newline is quoted"
    "a,\"b\nc\",d"
    (Granary_tpc.Bench_report.Csv.row [ "a"; "b\nc"; "d" ])
;;

let test_real_eq_relative () =
  Alcotest.(check bool)
    "1e9 vs 1e9+1e-3 is equal within relative epsilon"
    true
    (Granary_tpc.Bench_report.real_eq 1e9 (1e9 +. 1e-3));
  Alcotest.(check bool)
    "1.0 vs 1.1 is not equal"
    false
    (Granary_tpc.Bench_report.real_eq 1.0 1.1);
  Alcotest.(check bool)
    "-1e9 vs -1e9-1e-3 is equal within relative epsilon"
    true
    (Granary_tpc.Bench_report.real_eq (-1e9) (-1e9 -. 1e-3));
  Alcotest.(check bool)
    "-1.0 vs 1.0 is not equal"
    false
    (Granary_tpc.Bench_report.real_eq (-1.0) 1.0)
;;

let test_real_eq_near_zero () =
  Alcotest.(check bool)
    "values under the absolute floor compare equal"
    true
    (Granary_tpc.Bench_report.real_eq 0.0 1e-9)
;;

let test_real_eq_infinities () =
  Alcotest.(check bool)
    "infinity vs infinity is equal"
    true
    (Granary_tpc.Bench_report.real_eq Float.infinity Float.infinity);
  Alcotest.(check bool)
    "neg_infinity vs neg_infinity is equal"
    true
    (Granary_tpc.Bench_report.real_eq Float.neg_infinity Float.neg_infinity);
  Alcotest.(check bool)
    "infinity vs neg_infinity is not equal"
    false
    (Granary_tpc.Bench_report.real_eq Float.infinity Float.neg_infinity);
  Alcotest.(check bool)
    "infinity vs a large finite value is not equal"
    false
    (Granary_tpc.Bench_report.real_eq Float.infinity 1e308);
  Alcotest.(check bool)
    "nan vs nan is not equal"
    false
    (Granary_tpc.Bench_report.real_eq Float.nan Float.nan)
;;

let test_time_it_returns_result () =
  (* Burns a measurable amount of both wall and CPU time so a stub returning
     (v, 0.0, 0.0) would fail this test, while staying well under a second. *)
  let n = 20_000_000 in
  let burn () =
    let acc = ref 0 in
    for i = 1 to n do
      acc := !acc + i
    done;
    !acc
  in
  let v, wall, cpu = Granary_tpc.Bench_report.time_it burn in
  let expected = n * (n + 1) / 2 in
  Alcotest.(check int) "result passes through" expected v;
  Alcotest.(check bool) "wall is strictly positive" true (wall > 0.0);
  Alcotest.(check bool) "cpu is non-negative" true (cpu >= 0.0)
;;

let test_csv_header_matches_row () =
  Alcotest.(check string)
    "header renders identically to row"
    (Granary_tpc.Bench_report.Csv.row [ "a"; "b,c"; "d" ])
    (Granary_tpc.Bench_report.Csv.header [ "a"; "b,c"; "d" ])
;;

let test_env_int_default () =
  Alcotest.(check int)
    "absent var falls back to default"
    99
    (Granary_tpc.Bench_report.env_int "GRANARY_TPC_DEFINITELY_UNSET" 99)
;;

let test_env_int_set_and_garbage () =
  let key = "GRANARY_TPC_TEST_ENV_INT" in
  Unix.putenv key "42";
  Alcotest.(check int) "set var parses" 42 (Granary_tpc.Bench_report.env_int key 99);
  Unix.putenv key "not-an-int";
  Alcotest.(check int)
    "unparseable value falls back to default"
    99
    (Granary_tpc.Bench_report.env_int key 99)
;;

let test_env_float_default () =
  Alcotest.(check (float 0.0))
    "absent var falls back to default"
    1.5
    (Granary_tpc.Bench_report.env_float "GRANARY_TPC_DEFINITELY_UNSET" 1.5)
;;

let test_env_float_set_and_garbage () =
  let key = "GRANARY_TPC_TEST_ENV_FLOAT" in
  Unix.putenv key "0.01";
  Alcotest.(check (float 0.0))
    "set var parses"
    0.01
    (Granary_tpc.Bench_report.env_float key 1.5);
  Unix.putenv key "not-a-float";
  Alcotest.(check (float 0.0))
    "unparseable value falls back to default"
    1.5
    (Granary_tpc.Bench_report.env_float key 1.5)
;;

let test_env_str_default () =
  Alcotest.(check string)
    "absent var falls back to default"
    "fallback"
    (Granary_tpc.Bench_report.env_str "GRANARY_TPC_DEFINITELY_UNSET" "fallback")
;;

let test_env_str_set_and_empty () =
  let key = "GRANARY_TPC_TEST_ENV_STR" in
  Unix.putenv key "value";
  Alcotest.(check string)
    "set var passes through"
    "value"
    (Granary_tpc.Bench_report.env_str key "fallback");
  Unix.putenv key "";
  Alcotest.(check string)
    "empty value falls back to default"
    "fallback"
    (Granary_tpc.Bench_report.env_str key "fallback")
;;

let test_host_label_uses_env_override () =
  let key = "GRANARY_TPC_HOST" in
  Unix.putenv key "bench-host-42";
  Alcotest.(check string)
    "GRANARY_TPC_HOST wins over the system hostname"
    "bench-host-42"
    (Granary_tpc.Bench_report.host_label ())
;;

let test_host_label_falls_back_to_hostname () =
  let key = "GRANARY_TPC_HOST" in
  Unix.putenv key "";
  Alcotest.(check string)
    "unset GRANARY_TPC_HOST falls back to the system hostname"
    (try Unix.gethostname () with
     | _ -> "unknown")
    (Granary_tpc.Bench_report.host_label ())
;;

let () =
  Alcotest.run
    "bench_report"
    [ ( "csv"
      , [ Alcotest.test_case "quotes commas" `Quick test_csv_row_quotes_commas
        ; Alcotest.test_case "escapes quotes" `Quick test_csv_row_escapes_quotes
        ; Alcotest.test_case "quotes newline" `Quick test_csv_row_quotes_newline
        ; Alcotest.test_case "header matches row" `Quick test_csv_header_matches_row
        ] )
    ; ( "compare"
      , [ Alcotest.test_case "relative epsilon" `Quick test_real_eq_relative
        ; Alcotest.test_case "absolute floor" `Quick test_real_eq_near_zero
        ; Alcotest.test_case "infinities and nan" `Quick test_real_eq_infinities
        ] )
    ; ( "timing"
      , [ Alcotest.test_case "passes result through" `Quick test_time_it_returns_result ]
      )
    ; ( "env"
      , [ Alcotest.test_case "int default" `Quick test_env_int_default
        ; Alcotest.test_case "int set and garbage" `Quick test_env_int_set_and_garbage
        ; Alcotest.test_case "float default" `Quick test_env_float_default
        ; Alcotest.test_case "float set and garbage" `Quick test_env_float_set_and_garbage
        ; Alcotest.test_case "str default" `Quick test_env_str_default
        ; Alcotest.test_case "str set and empty" `Quick test_env_str_set_and_empty
        ; Alcotest.test_case
            "host_label env override"
            `Quick
            test_host_label_uses_env_override
        ; Alcotest.test_case
            "host_label falls back to hostname"
            `Quick
            test_host_label_falls_back_to_hostname
        ] )
    ]
;;
