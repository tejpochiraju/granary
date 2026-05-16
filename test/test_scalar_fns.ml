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

let () =
  Alcotest.run "scalar_fns" [
    "length",   [ Alcotest.test_case "length"   `Quick test_length   ];
    "lower",    [ Alcotest.test_case "lower"    `Quick test_lower    ];
    "upper",    [ Alcotest.test_case "upper"    `Quick test_upper    ];
    "abs",      [ Alcotest.test_case "abs"      `Quick test_abs      ];
    "coalesce", [ Alcotest.test_case "coalesce" `Quick test_coalesce ];
    "ifnull",   [ Alcotest.test_case "ifnull"   `Quick test_ifnull   ];
  ]
