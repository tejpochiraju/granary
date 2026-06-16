module E = Repl_engine

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

let () =
  Alcotest.run
    "repl_components"
    [ ( "repl_engine"
      , [ Alcotest.test_case "is_query_stmt" `Quick test_is_query_stmt
        ; Alcotest.test_case "split_stmts" `Quick test_split_stmts_respects_quotes
        ; Alcotest.test_case "has_terminator" `Quick test_has_terminator
        ] )
    ]
;;
