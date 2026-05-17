open Lwt.Syntax
module D = Sqlocaml.Db

let run f = Lwt_main.run (f ())

let db () =
  let* db = D.open_in_memory () in
  let* _ = D.execute db "CREATE TABLE t (id INTEGER, name TEXT, val REAL)" in
  let* _ = D.execute db "INSERT INTO t (id, name, val) VALUES (1, 'Hello', 3.14)" in
  let* _ = D.execute db "INSERT INTO t (id, name, val) VALUES (2, 'World', -2.7)" in
  Lwt.return db

let check_single_value name sql expected =
  run (fun () ->
    let* d = db () in
    let* r = D.query d sql in
    match r with
    | Error _ -> Alcotest.failf "%s: query error for: %s" name sql
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      match rows with
      | row :: _ ->
        let got = match row.(0) with
          | D.V_text s -> s
          | D.V_int  n -> Int64.to_string n
          | D.V_real f -> Printf.sprintf "%.2f" f
          | D.V_null   -> "NULL"
          | D.V_blob _ -> "BLOB"
        in
        Alcotest.(check string) name expected got;
        Lwt.return_unit
      | [] -> Alcotest.failf "%s: no rows returned" name)

let test_length () =
  check_single_value "length_hello" "SELECT LENGTH(name) FROM t WHERE id = 1" "5";
  check_single_value "length_world" "SELECT LENGTH(name) FROM t WHERE id = 2" "5"

let test_lower () =
  check_single_value "lower_hello" "SELECT LOWER(name) FROM t WHERE id = 1" "hello";
  check_single_value "lower_world" "SELECT LOWER(name) FROM t WHERE id = 2" "world"

let test_upper () =
  check_single_value "upper_hello" "SELECT UPPER(name) FROM t WHERE id = 1" "HELLO";
  check_single_value "upper_world" "SELECT UPPER(name) FROM t WHERE id = 2" "WORLD"

let test_abs () =
  check_single_value "abs_neg" "SELECT ABS(val) FROM t WHERE id = 2" "2.70";
  check_single_value "abs_pos" "SELECT ABS(val) FROM t WHERE id = 1" "3.14"

let test_coalesce () =
  run (fun () ->
    let* d = db () in
    let* _ = D.execute d "INSERT INTO t (id, name, val) VALUES (3, NULL, 0.0)" in
    let* r = D.query d "SELECT COALESCE(name, 'fallback') FROM t WHERE id = 3" in
    match r with
    | Error _ -> Alcotest.failf "coalesce: query error"
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      match rows with
      | [| D.V_text s |] :: _ ->
        Alcotest.(check string) "coalesce_null" "fallback" s;
        Lwt.return_unit
      | _ -> Alcotest.failf "coalesce: unexpected result")

let test_ifnull () =
  run (fun () ->
    let* d = db () in
    let* _ = D.execute d "INSERT INTO t (id, name, val) VALUES (4, NULL, 0.0)" in
    let* r = D.query d "SELECT IFNULL(name, 'default') FROM t WHERE id = 4" in
    match r with
    | Error _ -> Alcotest.failf "ifnull: query error"
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      match rows with
      | [| D.V_text s |] :: _ ->
        Alcotest.(check string) "ifnull_null" "default" s;
        Lwt.return_unit
      | _ -> Alcotest.failf "ifnull: unexpected result")

let test_substr () =
  check_single_value "substr_from"     "SELECT SUBSTR(name, 2) FROM t WHERE id = 1"    "ello";
  check_single_value "substr_from_len" "SELECT SUBSTR(name, 2, 3) FROM t WHERE id = 1" "ell"

let test_trim () =
  run (fun () ->
    let* d = D.open_in_memory () in
    let* _ = D.execute d "CREATE TABLE s (v TEXT)" in
    let* _ = D.execute d "INSERT INTO s VALUES ('  hello  ')" in
    let* r = D.query d "SELECT TRIM(v) FROM s" in
    match r with
    | Error e -> Alcotest.failf "query: %a" D.pp_error e
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      (match rows with
       | row :: _ -> Alcotest.(check string) "trim" "hello" (match row.(0) with D.V_text s -> s | _ -> "?")
       | [] -> Alcotest.fail "no rows");
      Lwt.return_unit)

let test_ltrim () =
  run (fun () ->
    let* d = D.open_in_memory () in
    let* _ = D.execute d "CREATE TABLE s (v TEXT)" in
    let* _ = D.execute d "INSERT INTO s VALUES ('  hello  ')" in
    let* r = D.query d "SELECT LTRIM(v) FROM s" in
    match r with
    | Error e -> Alcotest.failf "query: %a" D.pp_error e
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      (match rows with
       | row :: _ -> Alcotest.(check string) "ltrim" "hello  " (match row.(0) with D.V_text s -> s | _ -> "?")
       | [] -> Alcotest.fail "no rows");
      Lwt.return_unit)

let test_rtrim () =
  run (fun () ->
    let* d = D.open_in_memory () in
    let* _ = D.execute d "CREATE TABLE s (v TEXT)" in
    let* _ = D.execute d "INSERT INTO s VALUES ('  hello  ')" in
    let* r = D.query d "SELECT RTRIM(v) FROM s" in
    match r with
    | Error e -> Alcotest.failf "query: %a" D.pp_error e
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      (match rows with
       | row :: _ -> Alcotest.(check string) "rtrim" "  hello" (match row.(0) with D.V_text s -> s | _ -> "?")
       | [] -> Alcotest.fail "no rows");
      Lwt.return_unit)

let test_replace () =
  run (fun () ->
    let* d = D.open_in_memory () in
    let* _ = D.execute d "CREATE TABLE s (v TEXT)" in
    let* _ = D.execute d "INSERT INTO s VALUES ('hello world')" in
    let* r = D.query d "SELECT REPLACE(v, 'world', 'there') FROM s" in
    match r with
    | Error e -> Alcotest.failf "query: %a" D.pp_error e
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      (match rows with
       | row :: _ -> Alcotest.(check string) "replace" "hello there" (match row.(0) with D.V_text s -> s | _ -> "?")
       | [] -> Alcotest.fail "no rows");
      Lwt.return_unit)

let test_instr () =
  run (fun () ->
    let* d = D.open_in_memory () in
    let* _ = D.execute d "CREATE TABLE s (v TEXT)" in
    let* _ = D.execute d "INSERT INTO s VALUES ('hello')" in
    let* r = D.query d "SELECT INSTR(v, 'ell') FROM s" in
    match r with
    | Error e -> Alcotest.failf "query: %a" D.pp_error e
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      (match rows with
       | row :: _ -> Alcotest.(check int64) "instr" 2L (match row.(0) with D.V_int n -> n | _ -> -1L)
       | [] -> Alcotest.fail "no rows");
      Lwt.return_unit)

let test_round () =
  (* check_single_value formats reals as "%.2f": ROUND(3.14) = 3.0 -> "3.00", ROUND(3.14,1) = 3.1 -> "3.10" *)
  check_single_value "round_0"  "SELECT ROUND(val) FROM t WHERE id = 1"    "3.00";
  check_single_value "round_1"  "SELECT ROUND(val, 1) FROM t WHERE id = 1" "3.10"

let test_typeof () =
  check_single_value "typeof_int"  "SELECT TYPEOF(id) FROM t WHERE id = 1"   "integer";
  check_single_value "typeof_text" "SELECT TYPEOF(name) FROM t WHERE id = 1" "text";
  check_single_value "typeof_real" "SELECT TYPEOF(val) FROM t WHERE id = 1"  "real";
  check_single_value "typeof_null" "SELECT TYPEOF(NULL) FROM t WHERE id = 1" "null"

(* Regression test for issue #116: ORDER BY with scalar expression projection.
   Previously, Op_sort was applied after Op_expr_project using col_idx from the
   original schema, causing out-of-bounds access or wrong sort order.
   Fix: sort before projection so col_idx correctly addresses original schema. *)
let test_order_by_after_expr_proj () =
  run (fun () ->
    let* d = D.open_in_memory () in
    let* _ = D.execute d "CREATE TABLE words (id INTEGER, w TEXT)" in
    let* _ = D.execute d "INSERT INTO words (id, w) VALUES (1, 'banana')" in
    let* _ = D.execute d "INSERT INTO words (id, w) VALUES (2, 'apple')" in
    let* _ = D.execute d "INSERT INTO words (id, w) VALUES (3, 'cherry')" in
    (* SELECT UPPER(w) ORDER BY w — w is col 1 in original schema.
       After projection only 1 column exists; old code sorted by col 1 of
       projected row (out of bounds / wrong column). *)
    let* r = D.query d "SELECT UPPER(w) FROM words ORDER BY w" in
    match r with
    | Error e -> Alcotest.failf "order_expr_proj: query error: %a" D.pp_error e
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      let got = List.map (fun row -> match row.(0) with D.V_text s -> s | _ -> "?") rows in
      Alcotest.(check (list string)) "sorted_by_w"
        ["APPLE"; "BANANA"; "CHERRY"] got;
      Lwt.return_unit)

let test_null_args () =
  run (fun () ->
    let* d = D.open_in_memory () in
    let* _ = D.execute d "CREATE TABLE s (v TEXT)" in
    let* _ = D.execute d "INSERT INTO s VALUES ('hello')" in
    let check_null sql =
      let* r = D.query d sql in
      match r with
      | Error e -> Alcotest.failf "query: %a" D.pp_error e
      | Ok stream ->
        let* rows = Lwt_stream.to_list stream in
        (match rows with
         | row :: _ -> Alcotest.(check bool) sql true (row.(0) = D.V_null)
         | [] -> Alcotest.fail "no rows");
        Lwt.return_unit
    in
    let* () = check_null "SELECT TRIM(v, NULL) FROM s" in
    let* () = check_null "SELECT LTRIM(v, NULL) FROM s" in
    let* () = check_null "SELECT RTRIM(v, NULL) FROM s" in
    let* () = check_null "SELECT REPLACE(v, NULL, 'x') FROM s" in
    let* () = check_null "SELECT REPLACE(v, 'l', NULL) FROM s" in
    let* () = check_null "SELECT ROUND(3.14, NULL) FROM s" in
    Lwt.return_unit)

let () =
  Alcotest.run "scalar_fns" [
    "length",   [ Alcotest.test_case "length"   `Quick test_length   ];
    "lower",    [ Alcotest.test_case "lower"    `Quick test_lower    ];
    "upper",    [ Alcotest.test_case "upper"    `Quick test_upper    ];
    "abs",      [ Alcotest.test_case "abs"      `Quick test_abs      ];
    "coalesce", [ Alcotest.test_case "coalesce" `Quick test_coalesce ];
    "ifnull",   [ Alcotest.test_case "ifnull"   `Quick test_ifnull   ];
    "order_by_expr_proj", [ Alcotest.test_case "order_by_after_expr_proj" `Quick test_order_by_after_expr_proj ];
    "new_scalar_fns", [
      Alcotest.test_case "substr"     `Quick test_substr;
      Alcotest.test_case "trim"       `Quick test_trim;
      Alcotest.test_case "ltrim"      `Quick test_ltrim;
      Alcotest.test_case "rtrim"      `Quick test_rtrim;
      Alcotest.test_case "replace"    `Quick test_replace;
      Alcotest.test_case "instr"      `Quick test_instr;
      Alcotest.test_case "round"      `Quick test_round;
      Alcotest.test_case "typeof"     `Quick test_typeof;
      Alcotest.test_case "null_args"  `Quick test_null_args;
    ];
  ]
