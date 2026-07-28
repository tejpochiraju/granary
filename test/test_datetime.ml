open Granary_sql.Datetime

let check_ok msg expected actual =
  match actual with
  | Error e -> Alcotest.failf "%s: unexpected error: %s" msg e
  | Ok dt -> Alcotest.(check string) msg expected (to_date dt)
;;

let check_err msg actual =
  match actual with
  | Error _ -> ()
  | Ok dt -> Alcotest.failf "%s: expected error but got date %s" msg (to_date dt)
;;

(* ------------------------------------------------------------------ *)
(* Parse tests                                                          *)
(* ------------------------------------------------------------------ *)

let test_parse_date_only () = check_ok "date only" "2024-01-15" (parse "2024-01-15")

let test_parse_datetime_space () =
  let r = parse "2024-06-15 09:05:03" in
  match r with
  | Ok dt ->
    Alcotest.(check string) "date part" "2024-06-15" (to_date dt);
    Alcotest.(check string) "time part" "09:05:03" (to_time dt)
  | Error e -> Alcotest.fail e
;;

let test_parse_datetime_t () =
  let r = parse "2024-06-15T09:05:03" in
  match r with
  | Ok dt -> Alcotest.(check string) "T sep date" "2024-06-15" (to_date dt)
  | Error e -> Alcotest.fail e
;;

let test_parse_date_only_no_time () =
  (* A date string exactly 10 chars — no time portion *)
  match parse "2023-12-25" with
  | Ok dt -> Alcotest.(check string) "time defaults to 00:00:00" "00:00:00" (to_time dt)
  | Error e -> Alcotest.fail e
;;

let test_parse_time_only () =
  let r = parse "14:30:45" in
  match r with
  | Ok dt ->
    Alcotest.(check string) "date defaults to 2000-01-01" "2000-01-01" (to_date dt);
    Alcotest.(check string) "time" "14:30:45" (to_time dt)
  | Error e -> Alcotest.fail e
;;

let test_parse_time_hhmm () =
  let r = parse "09:15" in
  match r with
  | Ok dt -> Alcotest.(check string) "hhmm time" "09:15:00" (to_time dt)
  | Error e -> Alcotest.fail e
;;

let test_parse_time_bad_sep () =
  (* Valid-length time string but wrong separator after HH:MM *)
  match parse "09:15x00" with
  | Error _ -> ()
  | Ok dt -> Alcotest.failf "expected error but got %s" (to_time dt)
;;

let test_parse_now_no_clock () = check_err "now without clock" (parse "now")

let test_parse_now_with_clock () =
  let fixed = 1705276800.0 in
  (* 2024-01-15 00:00:00 UTC *)
  match parse ~now:(fun () -> fixed) "now" with
  | Ok dt -> Alcotest.(check string) "date from now" "2024-01-15" (to_date dt)
  | Error e -> Alcotest.fail e
;;

let test_parse_unix_epoch_int () =
  match parse "0" with
  | Ok dt -> Alcotest.(check string) "unix epoch 0" "1970-01-01" (to_date dt)
  | Error e -> Alcotest.fail e
;;

let test_parse_unix_large_int () =
  (* A large integer without a decimal point -> unix epoch interpretation *)
  match parse "1705276800" with
  | Ok dt -> Alcotest.(check string) "unix ts 2024-01-15" "2024-01-15" (to_date dt)
  | Error e -> Alcotest.fail e
;;

let test_parse_julian_day () =
  (* 2451544.5 = 2000-01-01 00:00:00 *)
  match parse "2451544.5" with
  | Ok dt -> Alcotest.(check string) "julian day" "2000-01-01" (to_date dt)
  | Error e -> Alcotest.fail e
;;

let test_parse_float_small () =
  (* A float with a decimal point but f <= 1000.0 should use unix epoch path *)
  match parse "0.0" with
  | Ok dt -> Alcotest.(check string) "float 0.0" "1970-01-01" (to_date dt)
  | Error e -> Alcotest.fail e
;;

let test_parse_invalid_date () =
  (* The module doesn't validate month ranges — only garbage strings fail *)
  check_err "garbage" (parse "not-a-date")
;;

let test_parse_invalid_date_bad_separators () =
  (* Length >= 10 but wrong separators *)
  check_err "bad sep1" (parse "2024x01-01");
  check_err "bad sep2" (parse "2024-01x01")
;;

let test_parse_invalid_time_in_datetime () =
  (* Valid date part but invalid time part *)
  check_err "bad time in datetime" (parse "2024-01-01 nottime")
;;

let test_parse_invalid_numeric () =
  (* A numeric string that parses to infinity *)
  check_err "infinity" (parse "inf");
  check_err "nan" (parse "nan")
;;

let test_parse_whitespace_trimmed () =
  (* Leading/trailing spaces should be trimmed *)
  match parse "  2024-01-15  " with
  | Ok dt -> Alcotest.(check string) "trimmed" "2024-01-15" (to_date dt)
  | Error e -> Alcotest.fail e
;;

(* ------------------------------------------------------------------ *)
(* Conversion tests                                                     *)
(* ------------------------------------------------------------------ *)

let test_julianday_known () =
  match parse "2000-01-01" with
  | Error e -> Alcotest.fail e
  | Ok dt ->
    let jd = to_julianday dt in
    Alcotest.(check bool) "jd 2000-01-01" true (abs_float (jd -. 2451544.5) < 0.0001)
;;

let test_unixepoch_epoch () =
  match parse "1970-01-01" with
  | Error e -> Alcotest.fail e
  | Ok dt -> Alcotest.(check int64) "unixepoch 1970" 0L (to_unixepoch dt)
;;

let test_unixepoch_known () =
  (* 2024-01-15 00:00:00 UTC = 1705276800 *)
  match parse "2024-01-15" with
  | Error e -> Alcotest.fail e
  | Ok dt -> Alcotest.(check int64) "unixepoch 2024-01-15" 1705276800L (to_unixepoch dt)
;;

let test_to_datetime () =
  match parse "2024-06-15 09:05:03" with
  | Error e -> Alcotest.fail e
  | Ok dt -> Alcotest.(check string) "to_datetime" "2024-06-15 09:05:03" (to_datetime dt)
;;

let test_datetime_roundtrip () =
  let cases = [ "2024-01-01"; "2024-12-31"; "2000-02-29"; "1999-12-31" ] in
  List.iter
    (fun s ->
       match parse s with
       | Error e -> Alcotest.failf "parse %s: %s" s e
       | Ok dt -> Alcotest.(check string) ("roundtrip " ^ s) s (to_date dt))
    cases
;;

(* ------------------------------------------------------------------ *)
(* strftime tests                                                       *)
(* ------------------------------------------------------------------ *)

let test_strftime_specifiers () =
  match parse "2024-06-15 09:05:03" with
  | Error e -> Alcotest.fail e
  | Ok dt ->
    Alcotest.(check string) "%Y" "2024" (strftime "%Y" dt);
    Alcotest.(check string) "%m" "06" (strftime "%m" dt);
    Alcotest.(check string) "%d" "15" (strftime "%d" dt);
    Alcotest.(check string) "%H" "09" (strftime "%H" dt);
    Alcotest.(check string) "%M" "05" (strftime "%M" dt);
    Alcotest.(check string) "%S" "03" (strftime "%S" dt);
    Alcotest.(check string) "%%" "%" (strftime "%%" dt)
;;

let test_strftime_fractional_seconds () =
  (* %f = seconds with 6 decimal places *)
  match parse "2024-01-01 00:00:03" with
  | Error e -> Alcotest.fail e
  | Ok dt ->
    let r = strftime "%f" dt in
    (* should start with "03" *)
    Alcotest.(check bool)
      "%f starts with 03"
      true
      (String.length r >= 2 && String.sub r 0 2 = "03")
;;

let test_strftime_unknown_specifier () =
  (* Unknown % specifier should pass through as %x *)
  match parse "2024-01-01" with
  | Error e -> Alcotest.fail e
  | Ok dt ->
    let r = strftime "%z" dt in
    Alcotest.(check string) "unknown specifier %z" "%z" r
;;

let test_strftime_literal_chars () =
  (* Characters that are not % are passed through verbatim *)
  match parse "2024-01-01" with
  | Error e -> Alcotest.fail e
  | Ok dt ->
    let r = strftime "date: %Y-%m-%d" dt in
    Alcotest.(check string) "literal chars" "date: 2024-01-01" r
;;

let test_strftime_trailing_percent () =
  (* A format ending with % (no following char) — should not crash *)
  match parse "2024-01-01" with
  | Error e -> Alcotest.fail e
  | Ok dt ->
    let r = strftime "%Y%" dt in
    Alcotest.(check string) "trailing %" "2024%" r
;;

let test_strftime_day_of_year () =
  (* 2024-01-01 = day 1; 2024-03-01 = day 61 (2024 is a leap year) *)
  (match parse "2024-01-01" with
   | Error e -> Alcotest.fail e
   | Ok dt -> Alcotest.(check string) "%j jan1" "001" (strftime "%j" dt));
  (match parse "2024-03-01" with
   | Error e -> Alcotest.fail e
   | Ok dt -> Alcotest.(check string) "%j mar1 leap" "061" (strftime "%j" dt));
  match parse "2023-03-01" with
  | Error e -> Alcotest.fail e
  | Ok dt -> Alcotest.(check string) "%j mar1 non-leap" "060" (strftime "%j" dt)
;;

let test_strftime_unix_s () =
  match parse "1970-01-01" with
  | Error e -> Alcotest.fail e
  | Ok dt -> Alcotest.(check string) "%s epoch" "0" (strftime "%s" dt)
;;

(* ------------------------------------------------------------------ *)
(* pad helpers — test pad4 and pad3 with edge cases                    *)
(* ------------------------------------------------------------------ *)

let test_pad4_large_year () =
  (* A year >= 1000 should not get the extra "0" prefix *)
  match parse "9999-12-31" with
  | Error e -> Alcotest.fail e
  | Ok dt -> Alcotest.(check string) "year 9999" "9999" (strftime "%Y" dt)
;;

let test_pad3_values () =
  (* %j: day 9 → "009", day 99 → "099", day 100 → "100" *)
  (match parse "2023-01-09" with
   | Error e -> Alcotest.fail e
   | Ok dt -> Alcotest.(check string) "%j day 9" "009" (strftime "%j" dt));
  (match parse "2023-04-09" with
   (* April 9 = day 99 *)
   | Error e -> Alcotest.fail e
   | Ok dt -> Alcotest.(check string) "%j day 99" "099" (strftime "%j" dt));
  match parse "2023-04-10" with
  (* April 10 = day 100 *)
  | Error e -> Alcotest.fail e
  | Ok dt -> Alcotest.(check string) "%j day 100" "100" (strftime "%j" dt)
;;

(* ------------------------------------------------------------------ *)
(* Runner                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "datetime"
    [ ( "parse"
      , [ Alcotest.test_case "date only" `Quick test_parse_date_only
        ; Alcotest.test_case "datetime space sep" `Quick test_parse_datetime_space
        ; Alcotest.test_case "datetime T sep" `Quick test_parse_datetime_t
        ; Alcotest.test_case "date only no time" `Quick test_parse_date_only_no_time
        ; Alcotest.test_case "time only HH:MM:SS" `Quick test_parse_time_only
        ; Alcotest.test_case "time only HH:MM" `Quick test_parse_time_hhmm
        ; Alcotest.test_case "time bad sep" `Quick test_parse_time_bad_sep
        ; Alcotest.test_case "now no clock" `Quick test_parse_now_no_clock
        ; Alcotest.test_case "now with clock" `Quick test_parse_now_with_clock
        ; Alcotest.test_case "unix epoch int" `Quick test_parse_unix_epoch_int
        ; Alcotest.test_case "unix large int" `Quick test_parse_unix_large_int
        ; Alcotest.test_case "julian day float" `Quick test_parse_julian_day
        ; Alcotest.test_case "float small" `Quick test_parse_float_small
        ; Alcotest.test_case "invalid date" `Quick test_parse_invalid_date
        ; Alcotest.test_case
            "invalid date separators"
            `Quick
            test_parse_invalid_date_bad_separators
        ; Alcotest.test_case
            "invalid time in datetime"
            `Quick
            test_parse_invalid_time_in_datetime
        ; Alcotest.test_case "invalid numeric" `Quick test_parse_invalid_numeric
        ; Alcotest.test_case "whitespace trimmed" `Quick test_parse_whitespace_trimmed
        ] )
    ; ( "conversions"
      , [ Alcotest.test_case "julianday known" `Quick test_julianday_known
        ; Alcotest.test_case "unixepoch epoch" `Quick test_unixepoch_epoch
        ; Alcotest.test_case "unixepoch 2024" `Quick test_unixepoch_known
        ; Alcotest.test_case "to_datetime" `Quick test_to_datetime
        ; Alcotest.test_case "roundtrip dates" `Quick test_datetime_roundtrip
        ] )
    ; ( "strftime"
      , [ Alcotest.test_case "all standard specifiers" `Quick test_strftime_specifiers
        ; Alcotest.test_case
            "fractional seconds %f"
            `Quick
            test_strftime_fractional_seconds
        ; Alcotest.test_case "unknown specifier" `Quick test_strftime_unknown_specifier
        ; Alcotest.test_case "literal characters" `Quick test_strftime_literal_chars
        ; Alcotest.test_case "trailing percent" `Quick test_strftime_trailing_percent
        ; Alcotest.test_case "day of year" `Quick test_strftime_day_of_year
        ; Alcotest.test_case "%s unix epoch" `Quick test_strftime_unix_s
        ] )
    ; ( "padding"
      , [ Alcotest.test_case "pad4 large year" `Quick test_pad4_large_year
        ; Alcotest.test_case "pad3 values" `Quick test_pad3_values
        ] )
    ]
;;
