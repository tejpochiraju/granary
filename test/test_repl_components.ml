module E = Repl_engine
module L = Event_log
module Ev = Granary.Db.Event
module Db = Granary.Db

let mk_commit id = Ev.Txn_commit { txn_id = id; frames = 0 }

let test_ring_capacity () =
  let l = L.create ~capacity:3 in
  List.iter (fun i -> L.push l (mk_commit (Int64.of_int i))) [ 1; 2; 3; 4; 5 ];
  Alcotest.(check int) "capped at 3" 3 (L.length l);
  let ids = List.filter_map Ev.txn_id (L.visible l) in
  Alcotest.(check (list int64)) "oldest dropped" [ 3L; 4L; 5L ] ids
;;

let mk_page ~txn ~tree = Ev.Page_read { txn_id = Int64.of_int txn; tree; page = 1L }

let test_filter () =
  let l = L.create ~capacity:10 in
  List.iter (fun i -> L.push l (mk_commit (Int64.of_int i))) [ 1; 2; 3 ];
  L.set_filter l (L.By_txn 2L);
  Alcotest.(check (list int64))
    "only txn 2"
    [ 2L ]
    (List.filter_map Ev.txn_id (L.visible l));
  L.set_filter l L.No_filter;
  Alcotest.(check int) "filter cleared" 3 (List.length (L.visible l))
;;

let test_table_filter () =
  let l = L.create ~capacity:10 in
  L.push l (mk_page ~txn:1 ~tree:16);
  L.push l (mk_page ~txn:1 ~tree:32);
  L.push l (mk_commit 1L);
  L.set_filter l (L.By_table { name = "t"; tree = 16 });
  let trees = List.filter_map Ev.tree_id_of (L.visible l) in
  Alcotest.(check (list int)) "only tree 16 page events" [ 16 ] trees;
  Alcotest.(check int)
    "global commit hidden under table filter"
    1
    (List.length (L.visible l))
;;

let test_dump () =
  let l = L.create ~capacity:10 in
  L.push l (mk_commit 1L);
  L.push l (mk_page ~txn:1 ~tree:16);
  let path = Filename.temp_file "granary_dump" ".log" in
  (match L.dump l path with
   | Ok n -> Alcotest.(check int) "dumped 2 events" 2 n
   | Error e -> Alcotest.failf "dump failed: %s" e);
  let ic = open_in path in
  let lines = ref [] in
  (try
     while true do
       lines := input_line ic :: !lines
     done
   with
   | End_of_file -> ());
  close_in ic;
  Sys.remove path;
  Alcotest.(check int) "file has 2 lines" 2 (List.length !lines);
  L.set_filter l (L.By_table { name = "t"; tree = 16 });
  (match L.dump l path with
   | Ok n -> Alcotest.(check int) "filtered dump = 1" 1 n
   | Error e -> Alcotest.failf "dump failed: %s" e);
  Sys.remove path
;;

let test_pause_toggle () =
  let l = L.create ~capacity:10 in
  Alcotest.(check bool) "starts unpaused" false (L.paused l);
  L.toggle_pause l;
  Alcotest.(check bool) "paused" true (L.paused l)
;;

let test_is_query_stmt () =
  Alcotest.(check bool) "select" true (E.is_query_stmt "SELECT 1");
  Alcotest.(check bool) "with" true (E.is_query_stmt "  with x as (..) select ..");
  Alcotest.(check bool) "insert" false (E.is_query_stmt "INSERT INTO t VALUES (1)");
  Alcotest.(check bool) "empty" false (E.is_query_stmt "   ")
;;

let test_split_stmts_respects_quotes () =
  Alcotest.(check (list string))
    "two stmts"
    [ "SELECT 1"; "SELECT 2" ]
    (E.split_stmts "SELECT 1; SELECT 2;");
  Alcotest.(check (list string))
    "semicolon in string is not a split"
    [ "INSERT INTO t VALUES ('a;b')" ]
    (E.split_stmts "INSERT INTO t VALUES ('a;b');")
;;

let test_has_terminator () =
  let b = Buffer.create 16 in
  Buffer.add_string b "SELECT 1";
  Alcotest.(check bool) "no term" false (E.has_terminator b);
  Buffer.add_char b ';';
  Alcotest.(check bool) "term" true (E.has_terminator b)
;;

(* #389: the splitter must ignore [;] inside SQL comments. *)
let test_split_block_comment () =
  Alcotest.(check (list string))
    "semicolon inside a block comment is not a split"
    [ "SELECT 1 /* ; */" ]
    (E.split_stmts "SELECT 1 /* ; */ ;")
;;

let test_split_line_comment () =
  Alcotest.(check (list string))
    "semicolon inside a line comment is not a split"
    [ "SELECT 1 -- ; comment" ]
    (E.split_stmts "SELECT 1 -- ; comment\n;")
;;

(* #389: a quote inside a comment must not corrupt the in-string state, so the
   real terminators after it still split. *)
let test_split_quote_in_comment () =
  Alcotest.(check (list string))
    "apostrophe in a comment does not swallow later statements"
    [ "-- it's fine\nSELECT 1"; "SELECT 2" ]
    (E.split_stmts "-- it's fine\nSELECT 1; SELECT 2;")
;;

(* #389 (guard): comment markers inside a string literal are literal text. *)
let test_split_comment_marker_in_string () =
  Alcotest.(check (list string))
    "comment markers inside a string literal are not comments"
    [ "SELECT '-- ; /* not a comment */'" ]
    (E.split_stmts "SELECT '-- ; /* not a comment */';")
;;

let test_has_terminator_block_comment () =
  let b = Buffer.create 32 in
  Buffer.add_string b "SELECT 1 /* ; */";
  Alcotest.(check bool)
    "no real terminator (; lives in a block comment)"
    false
    (E.has_terminator b)
;;

let test_has_terminator_line_comment () =
  let b = Buffer.create 32 in
  Buffer.add_string b "SELECT 1 -- ;\n";
  Alcotest.(check bool)
    "no real terminator (; lives in a line comment)"
    false
    (E.has_terminator b)
;;

(* #389 follow-up (PR #394 review): [;] inside [...] / `...` quoted identifiers
   must not split — both are valid granary identifier quotes (lexer.mll). *)
let test_split_bracket_identifier () =
  Alcotest.(check (list string))
    "semicolon inside a [..] quoted identifier is not a split"
    [ "SELECT 1 AS [a;b]" ]
    (E.split_stmts "SELECT 1 AS [a;b];")
;;

let test_split_backtick_identifier () =
  Alcotest.(check (list string))
    "semicolon inside a `..` quoted identifier is not a split"
    [ "SELECT 1 AS `a;b`" ]
    (E.split_stmts "SELECT 1 AS `a;b`;")
;;

(* A doubled ]] is an escaped ] inside a bracket identifier (lexer.mll), so the
   identifier does not close there and a following ; stays inside it. *)
let test_split_bracket_escape () =
  Alcotest.(check (list string))
    "]] is an escaped bracket, so ; after it is still inside the identifier"
    [ "SELECT 1 AS [a]];b]" ]
    (E.split_stmts "SELECT 1 AS [a]];b];")
;;

let test_has_terminator_bracket_identifier () =
  let b = Buffer.create 32 in
  Buffer.add_string b "SELECT [a;b]";
  Alcotest.(check bool)
    "no real terminator (; lives in a [..] identifier)"
    false
    (E.has_terminator b)
;;

(* Doubled '' is an escaped quote inside a string literal: it does not close the
   string, so the splitter keeps the whole literal in one statement (the subtle
   parity invariant the source comment reasons about). *)
let test_split_doubled_quote () =
  Alcotest.(check (list string))
    "doubled '' stays inside the string literal"
    [ "SELECT 'it''s'"; "SELECT 2" ]
    (E.split_stmts "SELECT 'it''s'; SELECT 2;")
;;

(* An unterminated block comment / a trailing line comment with no newline runs
   to end-of-input and yields the single statement (no spurious split, no
   dangling empty statement). *)
let test_split_unterminated_block_comment () =
  Alcotest.(check (list string))
    "unterminated block comment runs to EOF"
    [ "SELECT 1 /* unclosed" ]
    (E.split_stmts "SELECT 1 /* unclosed")
;;

let test_split_trailing_line_comment_no_newline () =
  Alcotest.(check (list string))
    "trailing -- comment with no newline runs to EOF"
    [ "SELECT 1 -- tail" ]
    (E.split_stmts "SELECT 1 -- tail")
;;

(* Property: any number of semicolons inside a block comment never split. *)
let prop_block_comment_semis_ignored =
  QCheck.Test.make
    ~count:100
    ~name:"semicolons inside a block comment never split"
    QCheck.(int_range 0 20)
    (fun k ->
       let comment = "/* " ^ String.make k ';' ^ " */" in
       let input = Printf.sprintf "A; %s B;" comment in
       E.split_stmts input = [ "A"; comment ^ " B" ])
;;

(* Property: splitting N simple statements joined by ';' recovers them all. *)
let prop_split_roundtrip =
  QCheck.Test.make
    ~count:200
    ~name:"split_stmts round-trips simple statements"
    QCheck.(
      list_small (make QCheck.Gen.(string_size (int_range 1 8) ~gen:(char_range 'a' 'z'))))
    (fun parts ->
       (* keep only non-empty, non-whitespace tokens (the function drops empties) *)
       let parts = List.filter (fun s -> String.trim s <> "") parts in
       let joined = String.concat ";" parts ^ if parts = [] then "" else ";" in
       E.split_stmts joined = List.map String.trim parts)
;;

(* Property: a leading-keyword query is classified as a query regardless of
   leading whitespace / case. *)
let prop_is_query_whitespace_insensitive =
  QCheck.Test.make
    ~count:200
    ~name:"is_query_stmt ignores leading whitespace and case"
    QCheck.(
      pair
        (oneof_list [ "select"; "WITH"; "Explain"; "values"; "pragma" ])
        (make
           QCheck.Gen.(string_size (int_range 0 5) ~gen:(oneof_list [ ' '; '\t'; '\n' ]))))
    (fun (kw, ws) -> E.is_query_stmt (ws ^ kw ^ " 1") = true)
;;

let test_monitor_renders () =
  let l = Event_log.create ~capacity:10 in
  Event_log.push l (mk_commit 1L);
  let ui_lwd = Monitor_view.render l in
  let root = Lwd.observe ui_lwd in
  let ui = Lwd.quick_sample root in
  Alcotest.(check bool) "renders" true (Nottui.Ui.layout_height ui >= 0)
;;

let test_monitor_table_key () =
  let l = Event_log.create ~capacity:10 in
  let txn_called = ref false in
  let table_called = ref false in
  let r =
    Monitor_view.handle_key
      l
      ~set_filter_prompt:(fun () -> txn_called := true)
      ~set_table_filter_prompt:(fun () -> table_called := true)
      (`ASCII 't', [])
  in
  Alcotest.(check bool) "t handled" true (r = `Handled);
  Alcotest.(check bool) "table prompt opened" true !table_called;
  Alcotest.(check bool) "txn prompt not opened" false !txn_called
;;

let test_shell_renders () =
  let v = Shell_view.create () in
  Shell_view.set_status v "Open: :memory:";
  Shell_view.set_result v ~headers:[ "x" ] ~rows:[ [| Granary.Db.V_int 1L |] ];
  let root = Lwd.observe (Shell_view.render v) in
  let ui = Lwd.quick_sample root in
  Alcotest.(check bool) "renders" true (Nottui.Ui.layout_height ui >= 1)
;;

let test_shell_header_width () =
  let w =
    Shell_view.column_widths_with_headers
      ~headers:[ "total_revenue" ]
      ~rows:[ [| Granary.Db.V_int 42L |] ]
  in
  Alcotest.(check int) "width covers header" (String.length "total_revenue") w.(0)
;;

module C = Repl_command

let test_parse_dot () =
  Alcotest.(check bool) "help" true (C.parse_dot ".help" = C.Help);
  Alcotest.(check bool) "quit" true (C.parse_dot ".quit" = C.Quit);
  Alcotest.(check bool) "exit=quit" true (C.parse_dot ".exit" = C.Quit);
  Alcotest.(check bool) "tables" true (C.parse_dot ".tables" = C.Tables);
  Alcotest.(check bool) "schema none" true (C.parse_dot ".schema" = C.Schema None);
  Alcotest.(check bool) "schema name" true (C.parse_dot ".schema t" = C.Schema (Some "t"));
  Alcotest.(check bool) "databases" true (C.parse_dot ".databases" = C.Databases);
  Alcotest.(check bool) "open" true (C.parse_dot ".open /a/b" = C.Open "/a/b");
  Alcotest.(check bool) "dump none" true (C.parse_dot ".dump" = C.Dump None);
  Alcotest.(check bool)
    "dump path"
    true
    (C.parse_dot ".dump /t/x.log" = C.Dump (Some "/t/x.log"));
  Alcotest.(check bool)
    "import"
    true
    (C.parse_dot ".import /a/b.sqlite" = C.Import "/a/b.sqlite");
  Alcotest.(check bool)
    "unknown"
    true
    (match C.parse_dot ".bogus" with
     | C.Unknown _ -> true
     | _ -> false)
;;

let test_classify () =
  Alcotest.(check bool) "empty" true (C.classify ~prompt:C.No_prompt "  " = C.Empty);
  Alcotest.(check bool)
    "sql"
    true
    (C.classify ~prompt:C.No_prompt "SELECT 1;" = C.Sql [ "SELECT 1" ]);
  Alcotest.(check bool)
    "dot"
    true
    (C.classify ~prompt:C.No_prompt ".tables" = C.Dot C.Tables);
  Alcotest.(check bool)
    "txn filter ok"
    true
    (C.classify ~prompt:C.Txn_prompt "42" = C.Filter (C.Filter_txn (Some 42L)));
  Alcotest.(check bool)
    "txn filter bad"
    true
    (C.classify ~prompt:C.Txn_prompt "xx" = C.Filter (C.Filter_txn None));
  Alcotest.(check bool)
    "table filter"
    true
    (C.classify ~prompt:C.Table_prompt " users " = C.Filter (C.Filter_table "users"))
;;

let test_schema_sql_escapes () =
  Alcotest.(check string)
    "escapes quote"
    "SELECT sql FROM sqlite_master WHERE name = 'a''b' AND sql IS NOT NULL"
    (C.schema_sql (Some "a'b"))
;;

(* ── #91: SQLite .dump import ─────────────────────────────────────── *)

let test_sqlite_dump_stmts () =
  let dump =
    "PRAGMA foreign_keys=OFF;\n\
     BEGIN TRANSACTION;\n\
     CREATE TABLE t(id INTEGER PRIMARY KEY, v TEXT);\n\
     INSERT INTO t VALUES(1,'a');\n\
     DELETE FROM sqlite_sequence;\n\
     INSERT INTO sqlite_sequence VALUES('t',1);\n\
     COMMIT;\n"
  in
  Alcotest.(check (list string))
    "wrapper + sqlite_sequence stripped, data kept"
    [ "CREATE TABLE t(id INTEGER PRIMARY KEY, v TEXT)"; "INSERT INTO t VALUES(1,'a')" ]
    (E.sqlite_dump_stmts dump)
;;

(* Regression (PR #400 review): internal-table maintenance is identified by the
   *target table*, never a substring — so a user row whose value mentions
   "sqlite_…" survives, while ANALYZE / sqlite_stat1 metadata is dropped. *)
let test_sqlite_dump_stmts_internal_tables () =
  let dump =
    "ANALYZE sqlite_schema;\n\
     INSERT INTO sqlite_stat1 VALUES('t','ix','1 1');\n\
     INSERT INTO t VALUES(1,'sqlite_sequence rocks');\n\
     DELETE FROM sqlite_sequence;\n\
     INSERT INTO log VALUES('reset sqlite_sequence now');\n"
  in
  Alcotest.(check (list string))
    "internal-table maintenance dropped; user rows with the substring kept"
    [ "INSERT INTO t VALUES(1,'sqlite_sequence rocks')"
    ; "INSERT INTO log VALUES('reset sqlite_sequence now')"
    ]
    (E.sqlite_dump_stmts dump)
;;

let ( let* ) = Lwt.bind

let test_import_sqlite_dump_ok () =
  Lwt_main.run
    (let* db = Db.open_in_memory () in
     let dump =
       "PRAGMA foreign_keys=OFF;\n\
        BEGIN TRANSACTION;\n\
        CREATE TABLE users(id INTEGER PRIMARY KEY, name TEXT);\n\
        INSERT INTO users VALUES(1,'alice');\n\
        INSERT INTO users VALUES(2,'bob');\n\
        COMMIT;\n"
     in
     let* applied, failures = E.import_sqlite_dump db dump in
     Alcotest.(check int) "3 statements applied" 3 applied;
     Alcotest.(check int) "no failures" 0 (List.length failures);
     let* r = Db.query db "SELECT name FROM users ORDER BY id" in
     match r with
     | Error e -> Alcotest.failf "query failed: %a" Db.pp_error e
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       let names = List.map (fun row -> E.value_to_string row.(0)) rows in
       Alcotest.(check (list string)) "rows imported" [ "alice"; "bob" ] names;
       Db.close db)
;;

let test_import_sqlite_dump_best_effort () =
  (* A typeless-column table (unsupported by strict typing) fails its own
     statements but does not abort import of the rest. *)
  Lwt_main.run
    (let* db = Db.open_in_memory () in
     let dump =
       "CREATE TABLE p(a,b);\n\
        INSERT INTO p VALUES(1,2);\n\
        CREATE TABLE q(x INT);\n\
        INSERT INTO q VALUES(9);\n"
     in
     let* applied, failures = E.import_sqlite_dump db dump in
     Alcotest.(check int) "q create+insert applied" 2 applied;
     Alcotest.(check int) "p create+insert failed" 2 (List.length failures);
     let* r = Db.query db "SELECT x FROM q" in
     match r with
     | Error e -> Alcotest.failf "query failed: %a" Db.pp_error e
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check (list string))
         "q imported"
         [ "9" ]
         (List.map (fun row -> E.value_to_string row.(0)) rows);
       Db.close db)
;;

let () =
  Alcotest.run
    "repl_components"
    [ ( "repl_engine"
      , [ Alcotest.test_case "is_query_stmt" `Quick test_is_query_stmt
        ; Alcotest.test_case "split_stmts" `Quick test_split_stmts_respects_quotes
        ; Alcotest.test_case "has_terminator" `Quick test_has_terminator
        ; Alcotest.test_case "split block comment" `Quick test_split_block_comment
        ; Alcotest.test_case "split line comment" `Quick test_split_line_comment
        ; Alcotest.test_case "split quote in comment" `Quick test_split_quote_in_comment
        ; Alcotest.test_case
            "split comment marker in string"
            `Quick
            test_split_comment_marker_in_string
        ; Alcotest.test_case
            "has_terminator block comment"
            `Quick
            test_has_terminator_block_comment
        ; Alcotest.test_case
            "has_terminator line comment"
            `Quick
            test_has_terminator_line_comment
        ; Alcotest.test_case
            "split bracket identifier"
            `Quick
            test_split_bracket_identifier
        ; Alcotest.test_case
            "split backtick identifier"
            `Quick
            test_split_backtick_identifier
        ; Alcotest.test_case "split bracket escape" `Quick test_split_bracket_escape
        ; Alcotest.test_case
            "has_terminator bracket identifier"
            `Quick
            test_has_terminator_bracket_identifier
        ; Alcotest.test_case "split doubled quote" `Quick test_split_doubled_quote
        ; Alcotest.test_case
            "split unterminated block comment"
            `Quick
            test_split_unterminated_block_comment
        ; Alcotest.test_case
            "split trailing line comment no newline"
            `Quick
            test_split_trailing_line_comment_no_newline
        ] )
    ; ( "props"
      , [ QCheck_alcotest.to_alcotest prop_split_roundtrip
        ; QCheck_alcotest.to_alcotest prop_is_query_whitespace_insensitive
        ; QCheck_alcotest.to_alcotest prop_block_comment_semis_ignored
        ] )
    ; ( "event_log"
      , [ Alcotest.test_case "ring capacity" `Quick test_ring_capacity
        ; Alcotest.test_case "filter" `Quick test_filter
        ; Alcotest.test_case "table filter" `Quick test_table_filter
        ; Alcotest.test_case "dump" `Quick test_dump
        ; Alcotest.test_case "pause toggle" `Quick test_pause_toggle
        ] )
    ; ( "monitor_view"
      , [ Alcotest.test_case "renders" `Quick test_monitor_renders
        ; Alcotest.test_case "table key" `Quick test_monitor_table_key
        ] )
    ; ( "shell_view"
      , [ Alcotest.test_case "renders" `Quick test_shell_renders
        ; Alcotest.test_case "header width" `Quick test_shell_header_width
        ] )
    ; ( "repl_command"
      , [ Alcotest.test_case "parse_dot" `Quick test_parse_dot
        ; Alcotest.test_case "classify" `Quick test_classify
        ; Alcotest.test_case "schema_sql escapes" `Quick test_schema_sql_escapes
        ] )
    ; ( "sqlite_import"
      , [ Alcotest.test_case "dump_stmts filter" `Quick test_sqlite_dump_stmts
        ; Alcotest.test_case
            "dump_stmts internal tables"
            `Quick
            test_sqlite_dump_stmts_internal_tables
        ; Alcotest.test_case "import ok" `Quick test_import_sqlite_dump_ok
        ; Alcotest.test_case
            "import best-effort"
            `Quick
            test_import_sqlite_dump_best_effort
        ] )
    ]
;;
