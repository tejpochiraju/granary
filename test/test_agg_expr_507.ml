(** #507: an aggregated projection may be an arbitrary scalar expression over
    aggregates, not only a bare aggregate call.

    [Sema.project_agg_item] used to accept exactly three shapes — a bare GROUP BY
    column, a bare aggregate call, and a bare window function — and rejected
    everything else with "complex expression in aggregated projection not
    supported". So [SELECT a, MAX(x) - MIN(x) FROM t GROUP BY a], which is
    unremarkable analytic SQL, did not run, and TPC-C's clause-3.3 consistency
    conditions had to be split into several queries with the arithmetic done
    client-side.

    The rule now is the one HAVING already used: bind the item with
    [bind_expr_agg], so any scalar expression is allowed as long as every leaf
    is a grouped column, an aggregate call, a window function, a literal, or a
    parameter. A bare *ungrouped* column is still an error, wherever in the
    expression it appears — that is the whole point of the restriction, and it
    is checked here at both the top level and nested inside arithmetic.

    Subqueries were left rejected here, exactly as they were in HAVING. #558
    closed that half too: they are now bound like any other leaf and resolved
    by [stream_aggregate]. The case that used to pin the rejection is kept as
    an accepted shape; [test_agg_subquery_558.ml] is where the subquery
    behaviour, including what remains refused, is pinned in full. *)

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

(* Rows as (int, int) pairs, sorted, so a test states the whole answer. *)
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

let ints rows =
  List.map
    (fun (r : Row.t) ->
       match Array.to_list r with
       | [ a ] -> int_of_value a
       | _ -> Alcotest.failf "expected a 1-column row, got %d" (Array.length r))
    rows
;;

let pair_list = Alcotest.(list (pair int int))

(* Two groups with distinct spreads, so MAX-MIN cannot coincide with MAX, MIN,
   COUNT or SUM by accident. *)
let seed db =
  exec db "CREATE TABLE t (a INTEGER, x INTEGER)";
  List.iter
    (fun (a, x) -> exec db (Printf.sprintf "INSERT INTO t VALUES (%d, %d)" a x))
    [ 1, 10; 1, 14; 1, 12; 2, 5; 2, 30 ]
;;

(* ------------------------------------------------------------------ *)
(* The reported shape                                                   *)
(* ------------------------------------------------------------------ *)

let arithmetic_over_aggregates () =
  with_db (fun db ->
    seed db;
    let got = pairs (query db "SELECT a, MAX(x) - MIN(x) FROM t GROUP BY a") in
    Alcotest.check pair_list "MAX-MIN per group" [ 1, 4; 2, 25 ] got)
;;

(* TPC-C clause 3.3 condition 3: MAX - MIN + 1 = COUNT-star.  Three aggregates,
   a literal, and a comparison, all in one projection item. *)
let tpcc_condition_3_shape () =
  with_db (fun db ->
    seed db;
    let got =
      pairs (query db "SELECT a, MAX(x) - MIN(x) + 1 = COUNT(*) FROM t GROUP BY a")
    in
    (* Group 1: 14-10+1 = 5 <> 3.  Group 2: 30-5+1 = 26 <> 2.  Both false. *)
    Alcotest.check pair_list "condition holds nowhere in this data" [ 1, 0; 2, 0 ] got;
    (* A gapless run makes it true — the oracle must be able to say yes too. *)
    exec db "CREATE TABLE g (a INTEGER, x INTEGER)";
    List.iter
      (fun (a, x) -> exec db (Printf.sprintf "INSERT INTO g VALUES (%d, %d)" a x))
      [ 1, 1; 1, 2; 1, 3; 2, 7; 2, 8 ];
    let got =
      pairs (query db "SELECT a, MAX(x) - MIN(x) + 1 = COUNT(*) FROM g GROUP BY a")
    in
    Alcotest.check pair_list "gapless runs satisfy it" [ 1, 1; 2, 1 ] got)
;;

(* No GROUP BY: the same expression over the single implicit group.  This is
   the shape the #247 aggregate fast path handles, so it also checks that the
   fast path did not silently start returning a different answer. *)
let arithmetic_over_aggregates_no_group_by () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list int))
      "MAX-MIN over the whole table"
      [ 25 ]
      (ints (query db "SELECT MAX(x) - MIN(x) FROM t"));
    Alcotest.(check (list int))
      "COUNT arithmetic"
      [ 10 ]
      (ints (query db "SELECT COUNT(*) * 2 FROM t")))
;;

(* An empty table still yields the single implicit group, with NULL-propagating
   arithmetic — the oracle in tpcc_check must not pass vacuously here. *)
let empty_table_no_group_by () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, x INTEGER)";
    let rows = query db "SELECT MAX(x) - MIN(x) FROM t" in
    Alcotest.(check int) "one row" 1 (List.length rows);
    match Array.to_list (List.hd rows) with
    | [ Row.V_null ] -> ()
    | [ _ ] -> Alcotest.fail "expected NULL for an empty table"
    | _ -> Alcotest.fail "expected a 1-column row")
;;

(* ------------------------------------------------------------------ *)
(* The other leaf kinds                                                 *)
(* ------------------------------------------------------------------ *)

let grouped_column_inside_an_expression () =
  with_db (fun db ->
    seed db;
    Alcotest.check
      pair_list
      "a * 100 + COUNT(*)"
      [ 1, 103; 2, 202 ]
      (pairs (query db "SELECT a, a * 100 + COUNT(*) FROM t GROUP BY a")))
;;

let function_call_over_aggregates () =
  with_db (fun db ->
    seed db;
    Alcotest.check
      pair_list
      "ABS(MIN-MAX)"
      [ 1, 4; 2, 25 ]
      (pairs (query db "SELECT a, ABS(MIN(x) - MAX(x)) FROM t GROUP BY a")))
;;

let case_over_aggregates () =
  with_db (fun db ->
    seed db;
    Alcotest.check
      pair_list
      "CASE on an aggregate"
      [ 1, 0; 2, 1 ]
      (pairs
         (query db "SELECT a, CASE WHEN COUNT(*) < 3 THEN 1 ELSE 0 END FROM t GROUP BY a")))
;;

let cast_and_between_over_aggregates () =
  with_db (fun db ->
    seed db;
    Alcotest.check
      pair_list
      "BETWEEN over an aggregate"
      [ 1, 1; 2, 0 ]
      (pairs (query db "SELECT a, MAX(x) BETWEEN 1 AND 20 FROM t GROUP BY a")))
;;

let parameter_leaf () =
  with_db (fun db ->
    seed db;
    let rows =
      run
        (let* st = Db.prepare db "SELECT a, COUNT(*) * ? FROM t GROUP BY a" in
         match st with
         | Error e -> Alcotest.failf "prepare: %a" Db.pp_error e
         | Ok st ->
           let* r = Db.iter st ~params:[ Row.V_int 3L ] in
           (match r with
            | Error e -> Alcotest.failf "iter: %a" Db.pp_error e
            | Ok stream -> Lwt_stream.to_list stream))
    in
    Alcotest.check pair_list "COUNT(*) * ?" [ 1, 9; 2, 6 ] (pairs rows))
;;

(* A window function is a legal leaf too: the slot it occupies in the aggregate
   output row is resolved after the aggregate list is final. *)
let window_function_inside_an_expression () =
  with_db (fun db ->
    seed db;
    Alcotest.check
      pair_list
      "ROW_NUMBER() * 10"
      [ 1, 10; 2, 20 ]
      (pairs (query db "SELECT a, ROW_NUMBER() OVER (ORDER BY a) * 10 FROM t GROUP BY a")))
;;

(* A bare aggregate and an expression over aggregates in the same projection:
   the slot numbering of the bare item must survive the expression's own
   aggregates being appended to the same list. *)
let bare_and_expression_items_together () =
  with_db (fun db ->
    seed db;
    let rows = query db "SELECT COUNT(*), MAX(x) - MIN(x), MIN(x) FROM t GROUP BY a" in
    let triples =
      List.sort
        compare
        (List.map
           (fun (r : Row.t) ->
              match Array.to_list r with
              | [ c; d; m ] -> int_of_value c, int_of_value d, int_of_value m
              | _ -> Alcotest.fail "expected 3 columns")
           rows)
    in
    Alcotest.(check (list (triple int int int)))
      "count, spread, min"
      [ 2, 25, 5; 3, 4, 10 ]
      triples)
;;

(* #494 is the same gap seen from TPC-H: arithmetic over aggregates in the
   select list, including the case where a bare literal sits beside an
   aggregate.  All three of its reported shapes are pinned here. *)
let tpch_494_shapes () =
  with_db (fun db ->
    exec db "CREATE TABLE u (a INTEGER, b INTEGER)";
    exec db "INSERT INTO u VALUES (10, 2)";
    exec db "INSERT INTO u VALUES (4, 4)";
    let one sql =
      match query db sql with
      | [ r ] -> Array.to_list r
      | rows -> Alcotest.failf "%S: expected one row, got %d" sql (List.length rows)
    in
    Alcotest.(check (list int))
      "SUM(a) / 7.0"
      [ 2 ]
      (List.map int_of_value (one "SELECT SUM(a) / 7.0 FROM u"));
    Alcotest.(check (list int))
      "100.0 * SUM(a) / SUM(b)"
      [ 233 ]
      (List.map int_of_value (one "SELECT 100.0 * SUM(a) / SUM(b) FROM u"));
    match one "SELECT 'C', MAX(a) FROM u" with
    | [ Row.V_text "C"; Row.V_int 10L ] -> ()
    | _ -> Alcotest.fail "expected ('C', 10) for a literal beside an aggregate")
;;

(* HAVING accepted this shape all along; the projection must now agree with it,
   including when both mention aggregates the other does not. *)
let consistent_with_having () =
  with_db (fun db ->
    seed db;
    Alcotest.check
      pair_list
      "expression in projection and in HAVING"
      [ 2, 25 ]
      (pairs
         (query
            db
            "SELECT a, MAX(x) - MIN(x) FROM t GROUP BY a HAVING MAX(x) - MIN(x) > 10")))
;;

let distinct_over_expression () =
  with_db (fun db ->
    seed db;
    exec db "INSERT INTO t VALUES (3, 100)";
    exec db "INSERT INTO t VALUES (3, 104)";
    (* Groups 1 and 3 both have a spread of 4; DISTINCT collapses them. *)
    Alcotest.(check (list int))
      "distinct spreads"
      [ 4; 25 ]
      (List.sort
         compare
         (ints (query db "SELECT DISTINCT MAX(x) - MIN(x) FROM t GROUP BY a"))))
;;

(* ------------------------------------------------------------------ *)
(* What stays rejected — the expressibility boundary                    *)
(* ------------------------------------------------------------------ *)

let bare_ungrouped_column_rejected () =
  with_db (fun db ->
    seed db;
    let msg = query_err db "SELECT x, COUNT(*) FROM t GROUP BY a" in
    Alcotest.(check bool)
      (Printf.sprintf "names GROUP BY (%S)" msg)
      true
      (contains_sub ~needle:"GROUP BY" msg))
;;

(* The same column, one level down inside arithmetic: the old code never got
   here (it rejected the whole item), and a naive rewrite that bound leaves
   against the input row would silently read an arbitrary row of the group. *)
let ungrouped_column_inside_expression_rejected () =
  with_db (fun db ->
    seed db;
    let msg = query_err db "SELECT a, x + COUNT(*) FROM t GROUP BY a" in
    Alcotest.(check bool)
      (Printf.sprintf "names GROUP BY (%S)" msg)
      true
      (contains_sub ~needle:"GROUP BY" msg))
;;

let ungrouped_qualified_column_inside_expression_rejected () =
  with_db (fun db ->
    seed db;
    let msg = query_err db "SELECT a, t.x + COUNT(*) FROM t GROUP BY a" in
    Alcotest.(check bool)
      (Printf.sprintf "names GROUP BY (%S)" msg)
      true
      (contains_sub ~needle:"GROUP BY" msg))
;;

(* #558 closed the half of #507 this used to pin as rejected: a subquery beside
   an aggregate is bound like any other leaf and resolved by [stream_aggregate]
   — uncorrelated once, correlated per group against the grouped columns. The
   case moves here as an ACCEPTED shape so that the boundary #507 drew is not
   silently re-drawn; [test_agg_subquery_558.ml] carries the full set,
   including what is still refused. *)
let subquery_beside_an_aggregate_accepted () =
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE u (a INTEGER)";
    exec db "INSERT INTO u VALUES (1)";
    (* [t] holds 3 rows in group a=1 and 2 in a=2 (see [seed]); [u] holds one,
       so every group's count gains exactly 1. *)
    Alcotest.check
      pair_list
      "the subquery is resolved, not rejected"
      [ 1, 4; 2, 3 ]
      (pairs (query db "SELECT a, COUNT(*) + (SELECT COUNT(*) FROM u) FROM t GROUP BY a")))
;;

(* ------------------------------------------------------------------ *)
(* Property: the expression is evaluated over the aggregates            *)
(* ------------------------------------------------------------------ *)

(* An expression tree over aggregates, evaluated by the engine and by a model
   in OCaml.  Only total integer operations appear, so the model is exact. *)
type texpr =
  | Lit of int
  | Cnt
  | Mx
  | Mn
  | Sm
  | Add of texpr * texpr
  | Sub of texpr * texpr
  | Mul of texpr * texpr

let rec sql_of = function
  | Lit n -> string_of_int n
  | Cnt -> "COUNT(*)"
  | Mx -> "MAX(x)"
  | Mn -> "MIN(x)"
  | Sm -> "SUM(x)"
  | Add (a, b) -> Printf.sprintf "(%s + %s)" (sql_of a) (sql_of b)
  | Sub (a, b) -> Printf.sprintf "(%s - %s)" (sql_of a) (sql_of b)
  | Mul (a, b) -> Printf.sprintf "(%s * %s)" (sql_of a) (sql_of b)
;;

(* [group] is non-empty, so MAX/MIN are defined. *)
let rec eval_model group = function
  | Lit n -> n
  | Cnt -> List.length group
  | Mx -> List.fold_left max (List.hd group) group
  | Mn -> List.fold_left min (List.hd group) group
  | Sm -> List.fold_left ( + ) 0 group
  | Add (a, b) -> eval_model group a + eval_model group b
  | Sub (a, b) -> eval_model group a - eval_model group b
  | Mul (a, b) -> eval_model group a * eval_model group b
;;

let gen_texpr =
  let open QCheck.Gen in
  sized_size (int_range 0 3)
  @@ fix (fun self n ->
    let leaf = oneof_list [ Cnt; Mx; Mn; Sm ] in
    let lit = map (fun n -> Lit n) (int_range (-5) 5) in
    if n <= 0
    then oneof [ leaf; lit ]
    else
      oneof
        [ leaf
        ; lit
        ; map2 (fun a b -> Add (a, b)) (self (n - 1)) (self (n - 1))
        ; map2 (fun a b -> Sub (a, b)) (self (n - 1)) (self (n - 1))
        ; map2 (fun a b -> Mul (a, b)) (self (n - 1)) (self (n - 1))
        ])
;;

(* (group key, value) rows; keys are small so groups actually collide. *)
let gen_rows =
  let open QCheck.Gen in
  list_size (int_range 1 12) (pair (int_range 0 2) (int_range (-20) 20))
;;

let arbitrary_case =
  QCheck.make
    ~print:(fun (e, rows) ->
      Printf.sprintf
        "%s over %s"
        (sql_of e)
        (String.concat "," (List.map (fun (a, x) -> Printf.sprintf "(%d,%d)" a x) rows)))
    (QCheck.Gen.pair gen_texpr gen_rows)
;;

let prop_matches_model =
  QCheck.Test.make
    ~count:200
    ~name:"aggregated projection expression = model"
    arbitrary_case
  @@ fun (e, rows) ->
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, x INTEGER)";
    List.iter
      (fun (a, x) -> exec db (Printf.sprintf "INSERT INTO t VALUES (%d, %d)" a x))
      rows;
    let sql = Printf.sprintf "SELECT a, %s FROM t GROUP BY a" (sql_of e) in
    let got = pairs (query db sql) in
    let groups =
      List.sort_uniq compare (List.map fst rows)
      |> List.map (fun k ->
        ( k
        , eval_model
            (List.filter_map (fun (a, x) -> if a = k then Some x else None) rows)
            e ))
      |> List.sort compare
    in
    got = groups)
;;

(* The rejection rule is a property too: any expression mentioning the
   ungrouped column is refused, however deeply it is buried. *)
let prop_ungrouped_column_always_rejected =
  QCheck.Test.make
    ~count:100
    ~name:"ungrouped column in a projection expression is always rejected"
    (QCheck.make ~print:sql_of gen_texpr)
  @@ fun e ->
  with_db (fun db ->
    seed db;
    let sql = Printf.sprintf "SELECT a, (%s) + x FROM t GROUP BY a" (sql_of e) in
    match run (Db.query db sql) with
    | Ok _ -> false
    | Error err -> contains_sub ~needle:"GROUP BY" (Format.asprintf "%a" Db.pp_error err))
;;

let () =
  Alcotest.run
    "agg_expr_507"
    [ ( "accepted"
      , [ Alcotest.test_case
            "arithmetic over aggregates"
            `Quick
            arithmetic_over_aggregates
        ; Alcotest.test_case "TPC-C condition 3 shape" `Quick tpcc_condition_3_shape
        ; Alcotest.test_case "no GROUP BY" `Quick arithmetic_over_aggregates_no_group_by
        ; Alcotest.test_case "empty table" `Quick empty_table_no_group_by
        ; Alcotest.test_case
            "grouped column in expression"
            `Quick
            grouped_column_inside_an_expression
        ; Alcotest.test_case "function call" `Quick function_call_over_aggregates
        ; Alcotest.test_case "CASE" `Quick case_over_aggregates
        ; Alcotest.test_case "BETWEEN" `Quick cast_and_between_over_aggregates
        ; Alcotest.test_case "parameter leaf" `Quick parameter_leaf
        ; Alcotest.test_case
            "window function leaf"
            `Quick
            window_function_inside_an_expression
        ; Alcotest.test_case
            "bare and expression items"
            `Quick
            bare_and_expression_items_together
        ; Alcotest.test_case "TPC-H #494 shapes" `Quick tpch_494_shapes
        ; Alcotest.test_case "consistent with HAVING" `Quick consistent_with_having
        ; Alcotest.test_case "DISTINCT" `Quick distinct_over_expression
        ; Alcotest.test_case
            "subquery beside aggregate (#558)"
            `Quick
            subquery_beside_an_aggregate_accepted
        ] )
    ; ( "rejected"
      , [ Alcotest.test_case "bare ungrouped column" `Quick bare_ungrouped_column_rejected
        ; Alcotest.test_case
            "ungrouped column in expression"
            `Quick
            ungrouped_column_inside_expression_rejected
        ; Alcotest.test_case
            "ungrouped qualified column in expression"
            `Quick
            ungrouped_qualified_column_inside_expression_rejected
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_matches_model; prop_ungrouped_column_always_rejected ] )
    ]
;;
