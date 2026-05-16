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

let test_fts_persists_across_reopen () =
  run (fun () ->
    let path = Filename.temp_file "test_fts_persist" ".db" in
    (* Create and close *)
    let* db1_r = D.open_file ~path in
    (match db1_r with
     | Error e -> Alcotest.failf "open1: %s" (Format.asprintf "%a" D.pp_error e)
     | Ok db1 ->
       let* r = D.execute db1 "CREATE VIRTUAL TABLE docs USING FTS5(title, body)" in
       (match r with
        | Error e -> Alcotest.failf "create: %s" (Format.asprintf "%a" D.pp_error e)
        | Ok () ->
          let* () = D.close db1 in
          (* Reopen and verify the FTS DDL succeeds again with a different name *)
          let* db2_r = D.open_file ~path in
          (match db2_r with
           | Error e -> Alcotest.failf "open2: %s" (Format.asprintf "%a" D.pp_error e)
           | Ok db2 ->
             (* Trying to create the same table again should fail (already exists) *)
             let* dup_r = D.execute db2 "CREATE VIRTUAL TABLE docs USING FTS5(content)" in
             let* () = D.close db2 in
             Unix.unlink path;
             (match dup_r with
              | Error _ -> Lwt.return_unit  (* expected: table already exists *)
              | Ok () -> Alcotest.failf "expected duplicate error after reopen")))))

(* Tokenizer tests *)
module Tok = Sqlocaml_sql.Fts_tokenizer

let test_tokenizer_basic () =
  let tokens = Tok.tokenize [(0, "Hello World")] in
  Alcotest.(check int) "count" 2 (List.length tokens);
  Alcotest.(check string) "first"  "hello" (List.nth tokens 0).Tok.term;
  Alcotest.(check string) "second" "world" (List.nth tokens 1).Tok.term;
  Alcotest.(check int) "pos0" 0 (List.nth tokens 0).Tok.pos;
  Alcotest.(check int) "pos1" 1 (List.nth tokens 1).Tok.pos

let test_tokenizer_punctuation () =
  let tokens = Tok.tokenize [(0, "foo,bar.baz!")] in
  Alcotest.(check int) "count" 3 (List.length tokens);
  let terms = List.map (fun t -> t.Tok.term) tokens in
  Alcotest.(check (list string)) "terms" ["foo"; "bar"; "baz"] terms

let test_tokenizer_multicol () =
  let tokens = Tok.tokenize [(0, "hello"); (1, "world")] in
  Alcotest.(check int) "count" 2 (List.length tokens);
  Alcotest.(check int) "col0" 0 (List.nth tokens 0).Tok.col;
  Alcotest.(check int) "col1" 1 (List.nth tokens 1).Tok.col;
  Alcotest.(check int) "pos resets" 0 (List.nth tokens 1).Tok.pos

let test_tokenizer_empty () =
  let tokens = Tok.tokenize [(0, "")] in
  Alcotest.(check int) "empty" 0 (List.length tokens)

let test_tokenizer_only_punct () =
  let tokens = Tok.tokenize [(0, "!@#$%")] in
  Alcotest.(check int) "punct only" 0 (List.length tokens)

let test_tokenizer_numbers () =
  let tokens = Tok.tokenize [(0, "abc123 456def")] in
  Alcotest.(check int) "mixed alphanumeric" 2 (List.length tokens);
  Alcotest.(check string) "first" "abc123" (List.nth tokens 0).Tok.term;
  Alcotest.(check string) "second" "456def" (List.nth tokens 1).Tok.term

let () =
  Alcotest.run "fts" [
    "ddl", [
      Alcotest.test_case "create_fts_table"             `Quick test_create_fts_table;
      Alcotest.test_case "create_fts_duplicate_fails"   `Quick test_create_fts_duplicate_fails;
      Alcotest.test_case "create_fts_single_col"        `Quick test_create_fts_single_col;
      Alcotest.test_case "fts_persists_across_reopen"   `Quick test_fts_persists_across_reopen;
    ];
    "tokenizer", [
      Alcotest.test_case "basic"       `Quick test_tokenizer_basic;
      Alcotest.test_case "punctuation" `Quick test_tokenizer_punctuation;
      Alcotest.test_case "multicol"    `Quick test_tokenizer_multicol;
      Alcotest.test_case "empty"       `Quick test_tokenizer_empty;
      Alcotest.test_case "punct_only"  `Quick test_tokenizer_only_punct;
      Alcotest.test_case "numbers"     `Quick test_tokenizer_numbers;
    ];
  ]
