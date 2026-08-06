(** #491 part 2: [CREATE VIEW v (c1, c2) AS SELECT ...] — the explicit
    column-name list.

    The form is standard SQL and SQLite accepts it; granary answered it with a
    bare "parse error: syntax error". TPC-H Q15 defines its [revenue0] view
    that way, and the benchmark harness worked around it by moving the column
    list into the SELECT's own aliases — which is exactly what the column list
    means, and is therefore also how it is implemented.

    The desugaring runs in the PARSER, and that placement is the thing worth
    pinning. A view is persisted as its original SQL text and re-parsed by
    [Db.load_views_into_hashtbl] on open, so a rewrite applied anywhere above
    the parser would evaporate on reopen and the view would come back with its
    body's own column names. [renamed_columns_survive_a_reopen] is the test
    that would catch that.

    Two shapes are refused rather than guessed at:

    - an arity mismatch between the list and the body's projection, which is
      the error the form exists to make possible; and
    - [SELECT *], whose arity the parser cannot know — it has no catalog. That
      is a deliberate divergence from SQLite, which resolves the star later and
      accepts the pairing. *)

open Lwt.Syntax
module Db = Granary.Db
module Row = Granary_encoding.Row

let run = Lwt_main.run

let with_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

let with_tempfile f =
  let path = Filename.temp_file "granary_view_491_" ".db" in
  (try Unix.unlink path with
   | Unix.Unix_error _ -> ());
  let result = f path in
  (try Unix.unlink path with
   | Unix.Unix_error _ -> ());
  result
;;

let open_file path =
  match run (Db.open_file ~path ()) with
  | Ok d -> d
  | Error e -> Alcotest.failf "open_file %S: %a" path Db.pp_error e
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let query db sql =
  run
    (let* r = Db.query db sql in
     match r with
     | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
     | Ok stream -> Lwt_stream.to_list stream)
;;

let query_err db sql =
  match run (Db.query db sql) with
  | Ok _ -> Alcotest.failf "expected an error for %S" sql
  | Error e -> Format.asprintf "%a" Db.pp_error e
;;

let exec_err db sql =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "expected an error for %S" sql
  | Error e -> Format.asprintf "%a" Db.pp_error e
;;

let contains_sub ~needle s =
  let n = String.length needle
  and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = needle || go (i + 1)) in
  go 0
;;

let int_of_value = function
  | Row.V_int i -> Int64.to_int i
  | Row.V_real f -> int_of_float f
  | Row.V_null -> Alcotest.fail "expected an integer, got NULL"
  | Row.V_text s -> Alcotest.failf "expected an integer, got text %S" s
  | Row.V_blob _ -> Alcotest.fail "expected an integer, got a blob"
;;

let pairs rows =
  List.sort
    compare
    (List.map
       (fun (r : Row.t) ->
          match Array.to_list r with
          | [ a; b ] -> int_of_value a, int_of_value b
          | _ -> Alcotest.failf "expected a 2-column row, got %d" (Array.length r))
       rows)
;;

let ints_in_order rows =
  List.map
    (fun (r : Row.t) ->
       match Array.to_list r with
       | [ a ] -> int_of_value a
       | _ -> Alcotest.failf "expected a 1-column row, got %d" (Array.length r))
    rows
;;

let ints rows = List.sort compare (ints_in_order rows)
let pair_list = Alcotest.(list (pair int int))

let seed db =
  exec db "CREATE TABLE t (a INTEGER, x INTEGER)";
  List.iter
    (fun (a, x) -> exec db (Printf.sprintf "INSERT INTO t VALUES (%d, %d)" a x))
    [ 1, 10; 2, 20; 3, 30 ]
;;

(* ------------------------------------------------------------------ *)
(* The reported shape                                                   *)
(* ------------------------------------------------------------------ *)

let column_list_parses_and_renames () =
  with_db (fun db ->
    seed db;
    exec db "CREATE VIEW v (p, q) AS SELECT a, x FROM t";
    let got = pairs (query db "SELECT p, q FROM v") in
    Alcotest.check
      pair_list
      "the view's rows, under the new names"
      [ 1, 10; 2, 20; 3, 30 ]
      got)
;;

(* The rename is a rename: the body's own column names must no longer resolve
   through the view, or the list would be an alias rather than a renaming. *)
let body_names_do_not_leak_through () =
  with_db (fun db ->
    seed db;
    exec db "CREATE VIEW v (p, q) AS SELECT a, x FROM t";
    let msg = query_err db "SELECT a FROM v" in
    Alcotest.(check bool)
      (Printf.sprintf "the body's name is gone, got %S" msg)
      true
      (String.length msg > 0))
;;

let renamed_columns_are_usable_in_predicates () =
  with_db (fun db ->
    seed db;
    exec db "CREATE VIEW v (p, q) AS SELECT a, x FROM t";
    let got = ints (query db "SELECT q FROM v WHERE p >= 2") in
    Alcotest.(check (list int)) "WHERE over a renamed column" [ 20; 30 ] got;
    let got = ints_in_order (query db "SELECT p FROM v ORDER BY q DESC") in
    Alcotest.(check (list int)) "ORDER BY over a renamed column" [ 3; 2; 1 ] got)
;;

(* [`Cols] and [`Exprs] are two different projection shapes in the AST, and the
   desugaring has to handle both: a bare column list parses to the former, a
   projection with any expression in it to the latter. *)
let renames_an_expression_projection () =
  with_db (fun db ->
    seed db;
    exec db "CREATE VIEW v (k, doubled) AS SELECT a, x * 2 FROM t";
    let got = pairs (query db "SELECT k, doubled FROM v") in
    Alcotest.check pair_list "expression projection renamed" [ 1, 20; 2, 40; 3, 60 ] got)
;;

(* An explicit alias in the body is overridden, not fought over: the column
   list is the outer, later word. *)
let column_list_overrides_the_body_alias () =
  with_db (fun db ->
    seed db;
    exec db "CREATE VIEW v (outer_a, outer_x) AS SELECT a AS inner_a, x AS inner_x FROM t";
    let got = pairs (query db "SELECT outer_a, outer_x FROM v") in
    Alcotest.check pair_list "outer names win" [ 1, 10; 2, 20; 3, 30 ] got)
;;

let renames_an_aggregate_projection () =
  with_db (fun db ->
    seed db;
    exec db "CREATE VIEW v (grp, total) AS SELECT a, SUM(x) FROM t GROUP BY a";
    let got = pairs (query db "SELECT grp, total FROM v") in
    Alcotest.check pair_list "aggregate columns renamed" [ 1, 10; 2, 20; 3, 30 ] got)
;;

(* TPC-H Q15's own view, spelled the way the spec spells it. Its column list is
   why this half of #491 was filed. *)
let tpch_q15_view_shape () =
  with_db (fun db ->
    exec db "CREATE TABLE lineitem (l_suppkey INTEGER, l_extendedprice INTEGER)";
    List.iter
      (fun (s, p) -> exec db (Printf.sprintf "INSERT INTO lineitem VALUES (%d, %d)" s p))
      [ 1, 100; 1, 50; 2, 70 ];
    exec
      db
      "CREATE VIEW revenue0 (supplier_no, total_revenue) AS SELECT l_suppkey, \
       SUM(l_extendedprice) FROM lineitem GROUP BY l_suppkey";
    let got = pairs (query db "SELECT supplier_no, total_revenue FROM revenue0") in
    Alcotest.check pair_list "the spec's view, unmodified" [ 1, 150; 2, 70 ] got)
;;

(* The desugaring happens in the parser precisely so this passes: the stored
   SQL text still carries the column list, and is re-parsed on open. *)
let renamed_columns_survive_a_reopen () =
  with_tempfile (fun path ->
    let db = open_file path in
    seed db;
    exec db "CREATE VIEW v (p, q) AS SELECT a, x FROM t";
    Alcotest.check
      pair_list
      "before close"
      [ 1, 10; 2, 20; 3, 30 ]
      (pairs (query db "SELECT p, q FROM v"));
    run (Db.close db);
    let db2 = open_file path in
    Alcotest.check
      pair_list
      "after reopen, still under the new names"
      [ 1, 10; 2, 20; 3, 30 ]
      (pairs (query db2 "SELECT p, q FROM v"));
    run (Db.close db2))
;;

(* ------------------------------------------------------------------ *)
(* Refused shapes                                                       *)
(* ------------------------------------------------------------------ *)

let too_few_names_rejected () =
  with_db (fun db ->
    seed db;
    let msg = exec_err db "CREATE VIEW v (p) AS SELECT a, x FROM t" in
    Alcotest.(check bool)
      (Printf.sprintf "arity mismatch reported, got %S" msg)
      true
      (contains_sub ~needle:"column list" msg))
;;

let too_many_names_rejected () =
  with_db (fun db ->
    seed db;
    let msg = exec_err db "CREATE VIEW v (p, q, r) AS SELECT a, x FROM t" in
    Alcotest.(check bool)
      (Printf.sprintf "arity mismatch reported, got %S" msg)
      true
      (contains_sub ~needle:"column list" msg);
    (* And nothing was created: a refused CREATE must not leave a half-made
       view behind. *)
    let msg = query_err db "SELECT p FROM v" in
    Alcotest.(check bool)
      (Printf.sprintf "no view was created, got %S" msg)
      true
      (String.length msg > 0))
;;

let star_projection_rejected () =
  with_db (fun db ->
    seed db;
    let msg = exec_err db "CREATE VIEW v (p, q) AS SELECT * FROM t" in
    Alcotest.(check bool)
      (Printf.sprintf "names the SELECT * limitation, got %S" msg)
      true
      (contains_sub ~needle:"SELECT *" msg))
;;

(* The bare form is untouched — the new production must not have shadowed it. *)
let bare_create_view_still_works () =
  with_db (fun db ->
    seed db;
    exec db "CREATE VIEW v AS SELECT a, x FROM t";
    let got = pairs (query db "SELECT a, x FROM v") in
    Alcotest.check pair_list "no column list, body names kept" [ 1, 10; 2, 20; 3, 30 ] got)
;;

let () =
  Alcotest.run
    "view_columns_491"
    [ ( "accepted"
      , [ Alcotest.test_case "column list renames" `Quick column_list_parses_and_renames
        ; Alcotest.test_case "body names are gone" `Quick body_names_do_not_leak_through
        ; Alcotest.test_case
            "usable in predicates"
            `Quick
            renamed_columns_are_usable_in_predicates
        ; Alcotest.test_case
            "expression projection"
            `Quick
            renames_an_expression_projection
        ; Alcotest.test_case
            "overrides a body alias"
            `Quick
            column_list_overrides_the_body_alias
        ; Alcotest.test_case "aggregate projection" `Quick renames_an_aggregate_projection
        ; Alcotest.test_case "TPC-H Q15's view" `Quick tpch_q15_view_shape
        ; Alcotest.test_case "survives a reopen" `Quick renamed_columns_survive_a_reopen
        ; Alcotest.test_case "bare form still works" `Quick bare_create_view_still_works
        ] )
    ; ( "rejected"
      , [ Alcotest.test_case "too few names" `Quick too_few_names_rejected
        ; Alcotest.test_case "too many names" `Quick too_many_names_rejected
        ; Alcotest.test_case "SELECT *" `Quick star_projection_rejected
        ] )
    ]
;;
