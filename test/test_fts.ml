open Lwt.Syntax
module D = Sqlocaml.Db

let run f = Lwt_main.run (f ())

let test_create_fts_table () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* r = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(title, body)" in
    match r with
    | Error e -> Alcotest.failf "create fts: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok () -> Lwt.return_unit)

let test_create_fts_duplicate_fails () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(title)" in
    let* r = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(body)" in
    match r with
    | Error _ -> Lwt.return_unit  (* expected: duplicate table error *)
    | Ok () -> Alcotest.failf "expected error for duplicate FTS table")

let test_create_fts_single_col () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* r = D.execute db "CREATE VIRTUAL TABLE articles USING FTS5(content)" in
    match r with
    | Error e -> Alcotest.failf "single col fts: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok () -> Lwt.return_unit)

let () =
  Alcotest.run "fts" [
    "ddl", [
      Alcotest.test_case "create_fts_table"           `Quick test_create_fts_table;
      Alcotest.test_case "create_fts_duplicate_fails" `Quick test_create_fts_duplicate_fails;
      Alcotest.test_case "create_fts_single_col"      `Quick test_create_fts_single_col;
    ];
  ]
