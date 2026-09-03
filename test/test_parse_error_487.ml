(** #487: a syntax failure used to surface as the bare string
    ["parse error: syntax error"] — no line, no column, no offending token.
    Triaging a malformed statement against a 600-line grammar meant bisecting
    it by hand, and an application embedding the engine had nothing at all to
    show its user.

    What is pinned here is what the issue asked for and no more: the position
    (line, column and byte offset) and the offending token. Deliberately NOT
    an expected-token set — [parser.mly] resolves ~290 shift/reduce conflicts
    arbitrarily, so the automaton state at failure does not correspond to an
    honest list of what would have been accepted, and a synthesised one would
    be confidently wrong.

    Three sites produced the bare string, one per entry point ([Db.parse]
    itself, and the two INSTEAD OF re-parse arms in [execute_core] and
    [execute_change_count_core]); the error quality must not depend on which
    API the caller reached for, so every public surface that can report a
    [Parse] error is exercised below on the same statement.

    The multi-line cases are load-bearing: [Sql.Lexer] never calls
    {!Lexing.new_line}, so the lexbuf's own [pos_lnum] is 1 everywhere. The
    line is counted from the source text instead, and a single-line-only test
    would pass just as well against a hardcoded 1. *)

open Lwt.Syntax
module Db = Granary.Db

let run = Lwt_main.run

let with_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

(* The [Parse] payload, or a failure naming what came back instead. *)
let parse_msg ~what = function
  | Ok _ -> Alcotest.failf "%s: expected a parse error, got a result" what
  | Error (Db.Parse msg) -> msg
  | Error e -> Alcotest.failf "%s: expected Db.Parse, got %a" what Db.pp_error e
;;

(* ------------------------------------------------------------------ *)
(* One statement, every entry point.                                    *)
(* ------------------------------------------------------------------ *)

(* Each of these reaches a different compile path that can report a parse
   error: [execute_core], [execute_change_count_core], [query]'s own
   [compile_routed], and [prepare] / [query_columns], which call [Db.parse]
   directly without going through [compile_routed] at all. *)
let via_execute db sql = parse_msg ~what:"execute" (run (Db.execute db sql))

let via_change_count db sql =
  parse_msg ~what:"execute_change_count" (run (Db.execute_change_count db sql))
;;

let via_query db sql = parse_msg ~what:"query" (run (Db.query db sql))
let via_prepare db sql = parse_msg ~what:"prepare" (run (Db.prepare db sql))
let via_columns db sql = parse_msg ~what:"query_columns" (run (Db.query_columns db sql))

let entry_points =
  [ "execute", via_execute
  ; "execute_change_count", via_change_count
  ; "query", via_query
  ; "prepare", via_prepare
  ; "query_columns", via_columns
  ]
;;

(* Assert [sql] produces exactly [expected] through every entry point. *)
let check_all db ~name ~sql ~expected =
  List.iter
    (fun (entry, f) ->
       let what = Printf.sprintf "%s via %s" name entry in
       Alcotest.(check string) what expected (f db sql))
    entry_points
;;

let schema = "CREATE TABLE t (a INTEGER, b INTEGER)"

(* ------------------------------------------------------------------ *)

let single_line_position () =
  with_db (fun db ->
    exec db schema;
    (* [FORM] is a plain identifier and nothing may follow the [*] projection,
       so the parser stops on it. It starts at byte 9 — line 1, column 10. *)
    let expected =
      "syntax error at line 1, column 10 (byte offset 9): unexpected token \"FORM\""
    in
    check_all db ~name:"misspelled FROM" ~sql:"SELECT * FORM t" ~expected)
;;

(* The line number is only genuinely exercised when the failure is not on the
   first line — this is the case a hardcoded [line 1] would also pass. *)
let multi_line_position () =
  with_db (fun db ->
    exec db schema;
    (* "SELECT a,\n" is bytes 0-9, "       b\n" 10-18, "FROM t\n" 19-25, so
       line 4 begins at byte 26 and its column 11 is byte 36. *)
    let sql = "SELECT a,\n       b\nFROM t\nWHERE a = = 1" in
    let expected =
      "syntax error at line 4, column 11 (byte offset 36): unexpected token \"=\""
    in
    check_all db ~name:"doubled =" ~sql ~expected)
;;

(* A second multi-line shape, failing on a different line at a different
   token, so the two are not both satisfied by one off-by-one. *)
let multi_line_position_line_two () =
  with_db (fun db ->
    exec db schema;
    let sql = "SELECT a\nFROM FROM t\nWHERE a = 1" in
    let expected =
      "syntax error at line 2, column 6 (byte offset 14): unexpected token \"FROM\""
    in
    check_all db ~name:"doubled FROM" ~sql ~expected)
;;

(* Pins that the column restarts at 1 after each newline rather than tracking
   the byte offset. *)
let column_restarts_each_line () =
  with_db (fun db ->
    exec db schema;
    let sql = "SELECT\na,\nb\nFROM\nt\nWHERE\n)" in
    let expected =
      "syntax error at line 7, column 1 (byte offset 25): unexpected token \")\""
    in
    check_all db ~name:"stray rparen" ~sql ~expected)
;;

(* Running off the end of the input has no token to name, and must say so
   rather than reporting an empty one. *)
let end_of_input () =
  with_db (fun db ->
    exec db schema;
    let expected =
      "syntax error at line 1, column 14 (byte offset 13): unexpected end of input"
    in
    check_all db ~name:"truncated statement" ~sql:"SELECT a FROM" ~expected)
;;

(* The lexer's own failures went through the same handler and were equally
   position-free. They already said what went wrong; they now also say where.
   A bare [@] is not a named parameter (that rule needs a following
   identifier), so it falls through to the catch-all character rule. *)
let lexer_failure_position () =
  with_db (fun db ->
    exec db schema;
    let sql = "SELECT a\nFROM t\nWHERE a = @" in
    let expected = "unexpected char: '@' at line 3, column 11 (byte offset 26)" in
    check_all db ~name:"unknown character" ~sql ~expected)
;;

(* Nothing about this changes a statement that parses. *)
let valid_sql_is_unaffected () =
  with_db (fun db ->
    exec db schema;
    exec db "INSERT INTO t VALUES (1, 2)";
    let rows =
      run
        (let* r = Db.query db "SELECT a, b FROM t" in
         match r with
         | Error e -> Alcotest.failf "query: %a" Db.pp_error e
         | Ok s -> Lwt_stream.to_list s)
    in
    Alcotest.(check int) "one row" 1 (List.length rows))
;;

(* [Db.pp_error] keeps the human-readable "parse error: " prefix, so the
   positioned detail is what every existing caller already prints — which is
   why [Db.Parse] could keep carrying a plain string. *)
let pp_error_renders_the_detail () =
  with_db (fun db ->
    let msg =
      match run (Db.query db "SELECT * FORM t") with
      | Ok _ -> Alcotest.fail "expected a parse error"
      | Error e -> Format.asprintf "%a" Db.pp_error e
    in
    let expected =
      "parse error: syntax error at line 1, column 10 (byte offset 9): unexpected token \
       \"FORM\""
    in
    Alcotest.(check string) "rendered" expected msg)
;;

let () =
  Alcotest.run
    "parse_error_487"
    [ ( "position"
      , [ Alcotest.test_case "single line" `Quick single_line_position
        ; Alcotest.test_case "multi line" `Quick multi_line_position
        ; Alcotest.test_case "multi line, line 2" `Quick multi_line_position_line_two
        ; Alcotest.test_case "column restarts" `Quick column_restarts_each_line
        ; Alcotest.test_case "end of input" `Quick end_of_input
        ] )
    ; "lexer", [ Alcotest.test_case "failure position" `Quick lexer_failure_position ]
    ; ( "rendering"
      , [ Alcotest.test_case "pp_error" `Quick pp_error_renders_the_detail
        ; Alcotest.test_case "valid sql unaffected" `Quick valid_sql_is_unaffected
        ] )
    ]
;;
