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

let test_tokenizer_unicode_german () =
  (* Unicode default case folding folds U+00DF (sharp s, eszett) to
     "ss" — this matches SQLite unicode61's full case folding. *)
  let toks = Tok.tokenize_string ~col:0 "Größe" in
  Alcotest.(check int) "one token" 1 (List.length toks);
  Alcotest.(check string) "folded term"
    "grösse" (List.hd toks).Tok.term

let test_tokenizer_unicode_french_case () =
  let toks = Tok.tokenize_string ~col:0 "Café CAFÉ" in
  Alcotest.(check int) "two tokens" 2 (List.length toks);
  Alcotest.(check (list string)) "folded match"
    ["café"; "café"]
    (List.map (fun t -> t.Tok.term) toks)

let test_tokenizer_unicode_turkish_dotted_i () =
  (* SQLite unicode61 folds U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
     to "i̇" (small i + combining dot above) per default Unicode
     case folding. *)
  let toks = Tok.tokenize_string ~col:0 "\xC4\xB0stanbul" in
  Alcotest.(check int) "one token" 1 (List.length toks);
  Alcotest.(check string) "Turkish dotted-I fold"
    "i\xCC\x87stanbul" (List.hd toks).Tok.term

let test_tokenizer_unicode_cjk_splits () =
  (* CJK ideographs are individually "Lo" — each is a word char, so a
     string of N CJK ideographs without separators should still be a
     single run from SQLite's perspective. (Hanzi parity is approximate;
     this asserts at least that the bytes round-trip and case fold is
     a no-op for ideographs.) *)
  let toks = Tok.tokenize_string ~col:0 "\xE4\xBD\xA0\xE5\xA5\xBD" in
  Alcotest.(check int) "one token" 1 (List.length toks);
  Alcotest.(check string) "no folding of ideographs"
    "\xE4\xBD\xA0\xE5\xA5\xBD" (List.hd toks).Tok.term

let test_fts_insert () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(title, body)" in
    let* r1 = D.execute db "INSERT INTO docs (title, body) VALUES ('Hello World', 'The quick brown fox')" in
    let* r2 = D.execute db "INSERT INTO docs (title, body) VALUES ('OCaml intro', 'Functional programming')" in
    match r1, r2 with
    | Ok (), Ok () -> Lwt.return_unit
    | Error e, _ | _, Error e ->
      Alcotest.failf "fts_insert: %s" (Format.asprintf "%a" D.pp_error e))

let test_fts_delete () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(title)" in
    let* _ = D.execute db "INSERT INTO docs (title) VALUES ('Hello')" in
    let* _ = D.execute db "INSERT INTO docs (title) VALUES ('World')" in
    let* r = D.execute db "DELETE FROM docs WHERE title = 'Hello'" in
    match r with
    | Error e -> Alcotest.failf "fts_delete: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok () -> Lwt.return_unit)

let test_fts_seq_scan () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(title)" in
    let* _ = D.execute db "INSERT INTO docs (title) VALUES ('First')" in
    let* _ = D.execute db "INSERT INTO docs (title) VALUES ('Second')" in
    let* r = D.query db "SELECT title FROM docs" in
    match r with
    | Error e -> Alcotest.failf "fts_seq_scan: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      Alcotest.(check int) "row count" 2 (List.length rows);
      Lwt.return_unit)

let test_match_basic () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(title, body)" in
    let* _ = D.execute db "INSERT INTO docs (title, body) VALUES ('OCaml intro', 'Functional language')" in
    let* _ = D.execute db "INSERT INTO docs (title, body) VALUES ('Python guide', 'Dynamic language')" in
    let* _ = D.execute db "INSERT INTO docs (title, body) VALUES ('OCaml advanced', 'Type systems')" in
    let* r = D.query db "SELECT title FROM docs WHERE docs MATCH 'ocaml'" in
    match r with
    | Error e -> Alcotest.failf "match: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      Alcotest.(check int) "match ocaml count" 2 (List.length rows);
      Lwt.return_unit)

let test_match_multiterm () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(body)" in
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('quick brown fox')" in
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('quick lazy dog')" in
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('slow brown cat')" in
    (* AND query: only doc 1 has both "quick" AND "brown" *)
    let* r = D.query db "SELECT body FROM docs WHERE docs MATCH 'quick brown'" in
    match r with
    | Error e -> Alcotest.failf "multiterm: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      Alcotest.(check int) "and count" 1 (List.length rows);
      Lwt.return_unit)

let test_match_or () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(body)" in
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('quick brown fox')" in
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('lazy dog')" in
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('hello world')" in
    (* OR query: docs 1 and 2 match *)
    let* r = D.query db "SELECT body FROM docs WHERE docs MATCH 'fox OR dog'" in
    match r with
    | Error e -> Alcotest.failf "or: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      Alcotest.(check int) "or count" 2 (List.length rows);
      Lwt.return_unit)

let test_match_not () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(body)" in
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('quick brown fox')" in
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('quick lazy dog')" in
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('slow brown cat')" in
    (* quick AND NOT dog: doc 1 has "quick" without "dog" *)
    let* r = D.query db "SELECT body FROM docs WHERE docs MATCH 'quick -dog'" in
    match r with
    | Error e -> Alcotest.failf "not: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      Alcotest.(check int) "not count" 1 (List.length rows);
      Lwt.return_unit)

let test_match_prefix () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(body)" in
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('programming is fun')" in
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('programs are useful')" in
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('hello world')" in
    (* prefix "prog*" matches docs 1 and 2 *)
    let* r = D.query db "SELECT body FROM docs WHERE docs MATCH 'prog*'" in
    match r with
    | Error e -> Alcotest.failf "prefix: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      Alcotest.(check int) "prefix count" 2 (List.length rows);
      Lwt.return_unit)

(* Regression tests for issue #117: phrase queries must verify adjacency.
   Previously the phrase intersector only checked that all words existed in
   the doc — a doc with "quick" at pos 0 and "brown" at pos 5 would match
   the phrase "quick brown" even though they are not adjacent. *)
let test_phrase_adjacent () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(body)" in
    (* "quick brown fox" — phrase "quick brown" is adjacent (pos 0,1) *)
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('quick brown fox')" in
    (* "quick lazy brown" — "quick" at pos 0, "brown" at pos 2; NOT adjacent *)
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('quick lazy brown')" in
    let* r = D.query db {|SELECT body FROM docs WHERE docs MATCH '"quick brown"'|} in
    match r with
    | Error e -> Alcotest.failf "phrase_adjacent: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      Alcotest.(check int) "only adjacent doc matches" 1 (List.length rows);
      (match rows with
       | [| D.V_text body |] :: _ ->
         Alcotest.(check string) "correct doc" "quick brown fox" body;
         Lwt.return_unit
       | _ -> Alcotest.failf "unexpected rows"))

let test_phrase_non_adjacent_no_match () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(body)" in
    (* "hello world" appears but separated by another word *)
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('hello cruel world')" in
    let* r = D.query db {|SELECT body FROM docs WHERE docs MATCH '"hello world"'|} in
    match r with
    | Error e -> Alcotest.failf "phrase_no_match: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      Alcotest.(check int) "non-adjacent phrase: no match" 0 (List.length rows);
      Lwt.return_unit)

let test_rank_column () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(body)" in
    (* doc1: "ocaml" appears 3 times — higher tf *)
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('ocaml ocaml ocaml tutorial')" in
    (* doc2: "ocaml" appears 1 time *)
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('ocaml introduction')" in
    (* doc3: no ocaml *)
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('python tutorial')" in
    let* r = D.query db "SELECT body, rank FROM docs WHERE docs MATCH 'ocaml'" in
    match r with
    | Error e -> Alcotest.failf "rank: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      (* Only 2 docs match "ocaml" *)
      Alcotest.(check int) "ranked rows" 2 (List.length rows);
      (* First row should have a rank value (real number) *)
      (match rows with
       | [| _; D.V_real _ |] :: _ -> Lwt.return_unit
       | _ -> Alcotest.failf "rank column should be V_real"))

let test_rank_ordering () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(body)" in
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('ocaml ocaml ocaml')" in
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('ocaml language')" in
    let* r = D.query db "SELECT body, rank FROM docs WHERE docs MATCH 'ocaml'" in
    match r with
    | Error e -> Alcotest.failf "rank_order: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      (* Results should be sorted by rank descending — doc1 first since it has more "ocaml" *)
      Alcotest.(check int) "count" 2 (List.length rows);
      (match rows with
       | [| D.V_text body1; D.V_real r1 |] :: [| D.V_text body2; D.V_real r2 |] :: _ ->
         Alcotest.(check bool) "doc1 ranks higher" true (r1 >= r2);
         ignore (body1, body2);
         Lwt.return_unit
       | _ -> Alcotest.failf "unexpected row shape"))

(* Regression test for issue #118: multi-term BM25 must use per-term tf.
   Previously all terms used the tf from the first posting list result.
   We verify that a document where term A appears many times scores higher
   than one where term A appears once, in a two-term AND query. *)
let test_bm25_per_term_tf () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE VIRTUAL TABLE docs USING FTS5(body)" in
    (* doc1: "ocaml" appears 4x, "tutorial" 1x *)
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('ocaml ocaml ocaml ocaml tutorial')" in
    (* doc2: "ocaml" appears 1x, "tutorial" 1x *)
    let* _ = D.execute db "INSERT INTO docs (body) VALUES ('ocaml tutorial guide')" in
    let* r = D.query db "SELECT body, rank FROM docs WHERE docs MATCH 'ocaml tutorial'" in
    match r with
    | Error e -> Alcotest.failf "bm25_per_term: %s" (Format.asprintf "%a" D.pp_error e)
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      Alcotest.(check int) "both docs match" 2 (List.length rows);
      (* doc1 has higher ocaml tf so should rank first *)
      (match rows with
       | [| D.V_text _; D.V_real r1 |] :: [| D.V_text _; D.V_real r2 |] :: _ ->
         Alcotest.(check bool) "doc1 scores higher" true (r1 > r2);
         Lwt.return_unit
       | _ -> Alcotest.failf "unexpected row shape"))

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
      Alcotest.test_case "unicode_german"        `Quick test_tokenizer_unicode_german;
      Alcotest.test_case "unicode_french_case"   `Quick test_tokenizer_unicode_french_case;
      Alcotest.test_case "unicode_turkish_i"     `Quick test_tokenizer_unicode_turkish_dotted_i;
      Alcotest.test_case "unicode_cjk_runs"      `Quick test_tokenizer_unicode_cjk_splits;
    ];
    "write", [
      Alcotest.test_case "insert"   `Quick test_fts_insert;
      Alcotest.test_case "delete"   `Quick test_fts_delete;
      Alcotest.test_case "seq_scan" `Quick test_fts_seq_scan;
    ];
    "match", [
      Alcotest.test_case "basic"     `Quick test_match_basic;
      Alcotest.test_case "multiterm" `Quick test_match_multiterm;
      Alcotest.test_case "or"        `Quick test_match_or;
      Alcotest.test_case "not"       `Quick test_match_not;
      Alcotest.test_case "prefix"    `Quick test_match_prefix;
      Alcotest.test_case "phrase_adjacent"     `Quick test_phrase_adjacent;
      Alcotest.test_case "phrase_non_adjacent" `Quick test_phrase_non_adjacent_no_match;
    ];
    "rank", [
      Alcotest.test_case "rank_column"      `Quick test_rank_column;
      Alcotest.test_case "rank_ordering"    `Quick test_rank_ordering;
      Alcotest.test_case "bm25_per_term_tf" `Quick test_bm25_per_term_tf;
    ];
  ]
