(** #387: the executor/Db API exposes the projected column names for a
    row-returning query so the REPL (and other callers) can render headers.
    {!Db.query_columns} parses and binds [sql] and returns the best-effort
    output column names — SELECT aliases and bare column names where known,
    [*] expanded to the table's columns, [col_N] placeholders otherwise. *)

module Db = Sqlocaml.Db

let run = Lwt_main.run

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "db error: %a" Db.pp_error e
;;

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

let cols_of db sql = unwrap (run (Db.query_columns db sql))

let seed db =
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)";
  exec db "INSERT INTO t VALUES (1, 'a')"
;;

(* An explicit column list reports exactly those names, in order. *)
let test_explicit_columns () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list string))
      "SELECT id, v"
      [ "id"; "v" ]
      (cols_of db "SELECT id, v FROM t"))
;;

(* [*] expands to the table's declared columns. *)
let test_star_expands () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list string)) "SELECT *" [ "id"; "v" ] (cols_of db "SELECT * FROM t"))
;;

(* AS aliases override the underlying column / expression name. *)
let test_aliases () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list string))
      "SELECT id AS x, v AS y"
      [ "x"; "y" ]
      (cols_of db "SELECT id AS x, v AS y FROM t"))
;;

(* A single projected column. *)
let test_single_column () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list string)) "SELECT v" [ "v" ] (cols_of db "SELECT v FROM t"))
;;

(* An aliased aggregate reports its alias (the bound form alone would only give
   a positional placeholder, so this exercises the AST-name fallback). *)
let test_aggregate_alias () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list string))
      "SELECT count(*) AS n"
      [ "n" ]
      (cols_of db "SELECT count(*) AS n FROM t"))
;;

(* A FROM-less constant select reports its aliases. *)
let test_const_select () =
  with_db (fun db ->
    Alcotest.(check (list string))
      "SELECT 1 AS a, 2 AS b"
      [ "a"; "b" ]
      (cols_of db "SELECT 1 AS a, 2 AS b"))
;;

(* A compound query takes its names from the left arm (PR #395 review:
   exercises the BS_compound / S_compound left-branch path). *)
let test_compound_union () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list string))
      "SELECT id AS x FROM t UNION SELECT v FROM t"
      [ "x" ]
      (cols_of db "SELECT id AS x FROM t UNION SELECT v FROM t"))
;;

let () =
  Alcotest.run
    "query_columns_387"
    [ ( "columns"
      , [ Alcotest.test_case "explicit columns" `Quick test_explicit_columns
        ; Alcotest.test_case "star expands" `Quick test_star_expands
        ; Alcotest.test_case "aliases" `Quick test_aliases
        ; Alcotest.test_case "single column" `Quick test_single_column
        ; Alcotest.test_case "aggregate alias" `Quick test_aggregate_alias
        ; Alcotest.test_case "const select" `Quick test_const_select
        ; Alcotest.test_case "compound union" `Quick test_compound_union
        ] )
    ]
;;
