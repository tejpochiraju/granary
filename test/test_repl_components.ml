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

let () =
  Alcotest.run
    "repl_components"
    [ ( "repl_engine"
      , [ Alcotest.test_case "is_query_stmt" `Quick test_is_query_stmt
        ; Alcotest.test_case "split_stmts" `Quick test_split_stmts_respects_quotes
        ; Alcotest.test_case "has_terminator" `Quick test_has_terminator
        ] )
    ; ( "props"
      , [ QCheck_alcotest.to_alcotest prop_split_roundtrip
        ; QCheck_alcotest.to_alcotest prop_is_query_whitespace_insensitive
        ] )
    ]
;;
