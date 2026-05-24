(** Phase 40 / #70 — sqlocaml interactive shell tests.

    Drives the REPL binary as a subprocess, pipes SQL + dot commands in
    via stdin, captures stdout, and asserts on substrings.  Tests are
    coarse-grained: they confirm the shell's user-visible contract
    (dot commands work, errors don't terminate the loop, multi-line
    input is accumulated until ';') without binding to exact formatting.

    The binary path is resolved via Sys.executable_name's parent —
    standard dune layout — so the test works both under [dune runtest]
    and via direct [dune exec]. *)

let repl_path () =
  (* test_repl.exe lives in _build/default/test/.  The REPL binary is at
     _build/default/bin/sqlocaml_repl.exe.  Resolve relative to our own
     exec path so we don't depend on the cwd. *)
  let our_path = Sys.executable_name in
  let test_dir = Filename.dirname our_path in
  let build_root = Filename.dirname test_dir in
  Filename.concat (Filename.concat build_root "bin") "sqlocaml_repl.exe"
;;

let run_repl ?(args = []) ~input () =
  let bin = repl_path () in
  let tmp_in = Filename.temp_file "repl_in_" ".sql" in
  let tmp_out = Filename.temp_file "repl_out_" ".txt" in
  let oc = open_out tmp_in in
  output_string oc input;
  close_out oc;
  let cmd =
    Printf.sprintf
      "%s %s < %s > %s 2>&1"
      (Filename.quote bin)
      (String.concat " " (List.map Filename.quote args))
      (Filename.quote tmp_in)
      (Filename.quote tmp_out)
  in
  let _ = Sys.command cmd in
  let ic = open_in tmp_out in
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_string buf (input_line ic);
       Buffer.add_char buf '\n'
     done
   with
   | End_of_file -> ());
  close_in ic;
  (try Unix.unlink tmp_in with
   | _ -> ());
  (try Unix.unlink tmp_out with
   | _ -> ());
  Buffer.contents buf
;;

let contains haystack needle =
  let h = haystack
  and n = needle in
  let hn = String.length h
  and nn = String.length n in
  let rec loop i =
    if i + nn > hn then false else if String.sub h i nn = n then true else loop (i + 1)
  in
  loop 0
;;

(* ------------------------------------------------------------------ *)
(* Tests                                                                *)
(* ------------------------------------------------------------------ *)

let test_banner_on_open () =
  let out = run_repl ~input:".quit\n" () in
  Alcotest.(check bool) "banner shown" true (contains out "sqlocaml interactive shell")
;;

let test_basic_insert_select () =
  let input =
    "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT);\n\
     INSERT INTO t VALUES (1, 'alice'), (2, 'bob');\n\
     SELECT name FROM t ORDER BY id;\n\
     .quit\n"
  in
  let out = run_repl ~input () in
  Alcotest.(check bool) "alice row printed" true (contains out "alice");
  Alcotest.(check bool) "bob row printed" true (contains out "bob");
  Alcotest.(check bool) "no panic" true (not (contains out "Stdlib.Failure"))
;;

let test_multiline_statement () =
  let input =
    "CREATE TABLE\n\
    \  t (id INTEGER\n\
    \   PRIMARY KEY,\n\
    \   v TEXT);\n\
     INSERT INTO t VALUES\n\
    \  (1, 'one'),\n\
    \  (2, 'two');\n\
     SELECT COUNT(*) FROM t;\n\
     .quit\n"
  in
  let out = run_repl ~input () in
  Alcotest.(check bool) "multiline create+insert+count works" true (contains out "2")
;;

let test_dot_tables_and_schema () =
  let input =
    "CREATE TABLE alpha (a INTEGER);\n\
     CREATE TABLE beta  (b TEXT);\n\
     .tables\n\
     .schema alpha\n\
     .quit\n"
  in
  let out = run_repl ~input () in
  Alcotest.(check bool) "alpha listed" true (contains out "alpha");
  Alcotest.(check bool) "beta listed" true (contains out "beta");
  Alcotest.(check bool)
    ".schema printed CREATE for alpha"
    true
    (contains out "CREATE TABLE")
;;

let test_dot_databases_default () =
  let input = ".databases\n.quit\n" in
  let out = run_repl ~input () in
  Alcotest.(check bool) ".databases shows main" true (contains out "main")
;;

let test_error_does_not_exit () =
  let input =
    "CREATE TABLE t (id INTEGER PRIMARY KEY);\n\
     SELECT * FROM nonexistent;\n\
     INSERT INTO t VALUES (1);\n\
     SELECT COUNT(*) FROM t;\n\
     .quit\n"
  in
  let out = run_repl ~input () in
  Alcotest.(check bool)
    "error reported but loop continued"
    true
    (contains out "Error" && contains out "1")
;;

let test_attach_within_repl () =
  let path = Printf.sprintf "/tmp/sqlocaml_phase40_repl_%d.db" (Unix.getpid ()) in
  (try Unix.unlink path with
   | _ -> ());
  let input =
    Printf.sprintf
      "ATTACH DATABASE '%s' AS aux;\n\
       PRAGMA active_database = 'aux';\n\
       CREATE TABLE notes (id INTEGER PRIMARY KEY, body TEXT);\n\
       INSERT INTO notes VALUES (1, 'attached');\n\
       SELECT body FROM notes;\n\
       .databases\n\
       .quit\n"
      path
  in
  let out = run_repl ~input () in
  (try Unix.unlink path with
   | _ -> ());
  Alcotest.(check bool) "attached body queried" true (contains out "attached");
  Alcotest.(check bool) "databases shows aux" true (contains out "aux")
;;

let () =
  Alcotest.run
    "repl"
    [ ( "shell"
      , [ Alcotest.test_case "banner on open" `Quick test_banner_on_open
        ; Alcotest.test_case "insert + select" `Quick test_basic_insert_select
        ; Alcotest.test_case "multi-line statement" `Quick test_multiline_statement
        ; Alcotest.test_case ".tables and .schema" `Quick test_dot_tables_and_schema
        ; Alcotest.test_case ".databases default" `Quick test_dot_databases_default
        ; Alcotest.test_case
            "errors don't terminate the loop"
            `Quick
            test_error_does_not_exit
        ; Alcotest.test_case "attach within REPL" `Quick test_attach_within_repl
        ] )
    ]
;;
