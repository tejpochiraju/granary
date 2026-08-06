(** #488 / #495: aggregates are first-class expressions.

    Two halves of one gap, fixed together because they share the aggregate
    machinery:

    - #488: an aggregate's ARGUMENT is a general expression evaluated per input
      row ([SUM(price * (1 - disc))] — the shape TPC-H calls "revenue"), not
      only a bare column reference.
    - #495: an aggregate's RESULT is usable as an expression in ORDER BY.  The
      sort runs on the POST-aggregation rows, so such a key is bound over the
      aggregate output row and carried to the sort as a hidden projection
      column that is trimmed off again — which is why the queries below check
      the output WIDTH as well as the row order.

    The #247 fast path is the trap here.  It decides whether to decode a row at
    all from the aggregates' [col_ord]s, and an expression argument has none —
    so before the applicability test was tightened, [SELECT SUM(a * b) FROM t]
    would have folded over an undecoded row and answered with a straight face.
    Every expression aggregate below is therefore run with the fast path forced
    ON and forced OFF, and both must give the same answer as arithmetic done by
    hand. *)

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

let rows db sql =
  run
    (let* r = Db.query db sql in
     match r with
     | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
     | Ok stream -> Lwt_stream.to_list stream)
;;

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let rejected db sql ~needle =
  run
    (let* r = Db.query db sql in
     match r with
     | Ok _ -> Alcotest.failf "expected %S to be rejected" sql
     | Error e ->
       let msg = Format.asprintf "%a" Db.pp_error e in
       Alcotest.(check bool)
         (Printf.sprintf "%S rejected mentioning %S (got %S)" sql needle msg)
         true
         (contains ~needle msg);
       Lwt.return_unit)
;;

let num = function
  | Row.V_int n -> Int64.to_float n
  | Row.V_real f -> f
  | Row.V_null -> Alcotest.fail "expected a number, got NULL"
  | Row.V_text s -> Alcotest.failf "expected a number, got text %S" s
  | Row.V_blob _ -> Alcotest.fail "expected a number, got a blob"
;;

let text = function
  | Row.V_text s -> s
  | Row.V_int n -> Alcotest.failf "expected text, got int %Ld" n
  | Row.V_real f -> Alcotest.failf "expected text, got real %g" f
  | Row.V_null -> Alcotest.fail "expected text, got NULL"
  | Row.V_blob _ -> Alcotest.fail "expected text, got a blob"
;;

let close_to ~msg expected got =
  Alcotest.(check bool)
    (Printf.sprintf "%s: expected %.6f, got %.6f" msg expected got)
    true
    (Float.abs (expected -. got) < 1e-6)
;;

let set_fastpath on = Unix.putenv "GRANARY_AGG_FASTPATH" (if on then "1" else "0")

(* Every scalar assertion runs twice: the #247 cursor fast path forced on, then
   forced off.  A tightening that is wrong in one direction shows up as a
   disagreement between the two, not as a plausible-looking number. *)
let scalar_both_paths db sql expected =
  List.iter
    (fun on ->
       set_fastpath on;
       let label = Printf.sprintf "%s (fastpath=%b)" sql on in
       match rows db sql with
       | [ r ] when Array.length r = 1 -> close_to ~msg:label expected (num r.(0))
       | rs ->
         Alcotest.failf "%s: expected one 1-column row, got %d" label (List.length rs))
    [ true; false ];
  set_fastpath true
;;

(*
   id | k | price | disc | qty
    1 | a | 100.0 | 0.10 |  2
    2 | a | 200.0 | 0.20 |  3
    3 | b | 300.0 | 0.00 |  1
    4 | b |  50.0 | 0.50 |  4
    5 | c |  10.0 | 0.00 |  5
*)
let seed db =
  exec
    db
    "CREATE TABLE li (id INTEGER PRIMARY KEY, k TEXT, price REAL, disc REAL, qty INTEGER)";
  List.iter
    (exec db)
    [ "INSERT INTO li VALUES (1, 'a', 100.0, 0.10, 2)"
    ; "INSERT INTO li VALUES (2, 'a', 200.0, 0.20, 3)"
    ; "INSERT INTO li VALUES (3, 'b', 300.0, 0.00, 1)"
    ; "INSERT INTO li VALUES (4, 'b', 50.0, 0.50, 4)"
    ; "INSERT INTO li VALUES (5, 'c', 10.0, 0.00, 5)"
    ]
;;

(* ---------------------------------------------------------------- #488 *)

(* 100*2 + 200*3 + 300*1 + 50*4 + 10*5 = 200+600+300+200+50 = 1350 *)
let sum_of_a_product () =
  with_db (fun db ->
    seed db;
    scalar_both_paths db "SELECT SUM(price * qty) FROM li" 1350.0)
;;

(* The canonical TPC-H measure: 90 + 160 + 300 + 25 + 10 = 585 *)
let sum_of_price_times_one_minus_discount () =
  with_db (fun db ->
    seed db;
    scalar_both_paths db "SELECT SUM(price * (1 - disc)) FROM li" 585.0)
;;

(* (102 + 203 + 301 + 54 + 15) / 5 = 675 / 5 = 135 *)
let avg_of_a_sum () =
  with_db (fun db ->
    seed db;
    scalar_both_paths db "SELECT AVG(price + qty) FROM li" 135.0)
;;

(* CASE inside an aggregate falls out of the same change and is what TPC-H
   Q8/Q12/Q14 need: three of the five rows carry a discount. *)
let aggregate_over_a_case_expression () =
  with_db (fun db ->
    seed db;
    scalar_both_paths db "SELECT SUM(CASE WHEN disc > 0.0 THEN 1 ELSE 0 END) FROM li" 3.0)
;;

(* MIN/MAX/COUNT over an expression too — the getter is shared, so a spec that
   forgot it would show up as a NULL or a crash rather than a wrong number. *)
let min_max_count_over_expressions () =
  with_db (fun db ->
    seed db;
    scalar_both_paths db "SELECT MIN(price * qty) FROM li" 50.0;
    scalar_both_paths db "SELECT MAX(price * qty) FROM li" 600.0;
    scalar_both_paths db "SELECT COUNT(price * qty) FROM li" 5.0)
;;

(* #247 interaction, stated as its own case: a COUNT-star contributes no column
   ordinal and neither does an expression argument, so this is exactly the
   select list whose rows the fast path used to skip decoding. *)
let count_star_beside_an_expression_aggregate () =
  with_db (fun db ->
    seed db;
    let check on =
      set_fastpath on;
      match rows db "SELECT COUNT(*), SUM(price * qty) FROM li" with
      | [ r ] when Array.length r = 2 ->
        close_to ~msg:"COUNT(*)" 5.0 (num r.(0));
        close_to ~msg:"SUM(price*qty)" 1350.0 (num r.(1))
      | rs -> Alcotest.failf "expected one 2-column row, got %d" (List.length rs)
    in
    check true;
    check false;
    set_fastpath true)
;;

(* An aggregate may not contain another aggregate, in either position. *)
let nested_aggregates_are_rejected () =
  with_db (fun db ->
    seed db;
    rejected db "SELECT SUM(SUM(price)) FROM li" ~needle:"nested";
    rejected db "SELECT SUM(price + SUM(qty)) FROM li" ~needle:"nested")
;;

(* ---------------------------------------------------------------- #495 *)

let pairs db sql =
  List.map
    (fun r ->
       if Array.length r <> 2
       then Alcotest.failf "%S: expected 2 output columns, got %d" sql (Array.length r);
       text r.(0), num r.(1))
    (rows db sql)
;;

(* revenue per k: a = 90+160 = 250, b = 300+25 = 325, c = 10 *)
let order_by_a_repeated_aggregate () =
  with_db (fun db ->
    seed db;
    let got =
      pairs
        db
        "SELECT k, SUM(price * (1 - disc)) FROM li GROUP BY k ORDER BY SUM(price * (1 - \
         disc)) DESC"
    in
    Alcotest.(check (list string)) "order" [ "b"; "a"; "c" ] (List.map fst got);
    List.iter2
      (fun expected (k, v) -> close_to ~msg:k expected v)
      [ 325.0; 250.0; 10.0 ]
      got)
;;

(* The spelling every TPC-H query actually uses. *)
let order_by_an_aliased_aggregate () =
  with_db (fun db ->
    seed db;
    let got =
      pairs
        db
        "SELECT k, SUM(price * (1 - disc)) AS revenue FROM li GROUP BY k ORDER BY \
         revenue DESC"
    in
    Alcotest.(check (list string)) "order" [ "b"; "a"; "c" ] (List.map fst got);
    List.iter2
      (fun expected (k, v) -> close_to ~msg:k expected v)
      [ 325.0; 250.0; 10.0 ]
      got)
;;

let order_by_ascending_and_with_limit () =
  with_db (fun db ->
    seed db;
    (* SUM(price*qty): a = 800, b = 500, c = 50 *)
    let asc =
      pairs db "SELECT k, SUM(price * qty) AS rev FROM li GROUP BY k ORDER BY rev"
    in
    Alcotest.(check (list string)) "ascending" [ "c"; "b"; "a" ] (List.map fst asc);
    let top =
      pairs
        db
        "SELECT k, SUM(price * qty) AS rev FROM li GROUP BY k ORDER BY rev DESC LIMIT 2"
    in
    Alcotest.(check (list string)) "top two" [ "a"; "b" ] (List.map fst top))
;;

(* The hidden sort column must not reach the caller: this ORDER BY names an
   aggregate the select list does not project, and the rows must still be one
   column wide. *)
let order_by_an_unprojected_aggregate_keeps_the_output_width () =
  with_db (fun db ->
    seed db;
    let got =
      List.map
        (fun r ->
           if Array.length r <> 1
           then
             Alcotest.failf
               "hidden ORDER BY column leaked: %d output columns"
               (Array.length r);
           text r.(0))
        (rows db "SELECT k FROM li GROUP BY k ORDER BY SUM(price) DESC")
    in
    (* SUM(price): a = 300, b = 350, c = 10 *)
    Alcotest.(check (list string)) "order" [ "b"; "a"; "c" ] got)
;;

(* A grouped column beside an aggregate in the same clause: both keys are bound
   in aggregate-output space, so the plain one must keep working. *)
let order_by_mixes_an_aggregate_and_a_group_column () =
  with_db (fun db ->
    seed db;
    exec db "INSERT INTO li VALUES (6, 'd', 300.0, 0.00, 1)";
    (* SUM(price*qty): a = 800, b = 500, c = 50, d = 300 *)
    let got =
      pairs db "SELECT k, SUM(price * qty) AS rev FROM li GROUP BY k ORDER BY rev DESC, k"
    in
    Alcotest.(check (list string)) "order" [ "a"; "b"; "d"; "c" ] (List.map fst got))
;;

let having_over_an_aggregate_expression () =
  with_db (fun db ->
    seed db;
    let got =
      List.map
        (fun r -> text r.(0))
        (rows
           db
           "SELECT k FROM li GROUP BY k HAVING SUM(price * (1 - disc)) > 100 ORDER BY k")
    in
    Alcotest.(check (list string)) "groups above 100" [ "a"; "b" ] got)
;;

(* HAVING and ORDER BY both contribute aggregates, after the projection's.  The
   slot numbering is what this pins: get it wrong and the sort orders by the
   HAVING aggregate (or by a group column) instead. *)
let having_and_order_by_aggregates_coexist () =
  with_db (fun db ->
    seed db;
    let got =
      pairs
        db
        "SELECT k, COUNT(*) FROM li GROUP BY k HAVING SUM(price * (1 - disc)) > 100 \
         ORDER BY SUM(price * qty) DESC"
    in
    (* a and b survive HAVING; SUM(price*qty) is a = 800, b = 500 *)
    Alcotest.(check (list string)) "order" [ "a"; "b" ] (List.map fst got);
    List.iter2 (fun expected (k, v) -> close_to ~msg:k expected v) [ 2.0; 2.0 ] got)
;;

(* No GROUP BY: one implicit group, and the sort is over that single row.  This
   also keeps the #247 fast path in the picture — it only ever fires when there
   is no GROUP BY. *)
let order_by_an_aggregate_without_group_by () =
  with_db (fun db ->
    seed db;
    match rows db "SELECT SUM(price * qty) AS rev FROM li ORDER BY rev DESC" with
    | [ r ] when Array.length r = 1 -> close_to ~msg:"single group" 1350.0 (num r.(0))
    | rs -> Alcotest.failf "expected one 1-column row, got %d" (List.length rs))
;;

let () =
  Alcotest.run
    "agg_expr_495_488"
    [ ( "#488 aggregate arguments are expressions"
      , [ Alcotest.test_case "SUM(a * b)" `Quick sum_of_a_product
        ; Alcotest.test_case
            "SUM(price * (1 - disc))"
            `Quick
            sum_of_price_times_one_minus_discount
        ; Alcotest.test_case "AVG(a + b)" `Quick avg_of_a_sum
        ; Alcotest.test_case "SUM(CASE ...)" `Quick aggregate_over_a_case_expression
        ; Alcotest.test_case
            "MIN/MAX/COUNT over expressions"
            `Quick
            min_max_count_over_expressions
        ; Alcotest.test_case
            "COUNT(*) beside an expression aggregate (#247 fast path)"
            `Quick
            count_star_beside_an_expression_aggregate
        ; Alcotest.test_case
            "SUM(SUM(x)) is rejected"
            `Quick
            nested_aggregates_are_rejected
        ] )
    ; ( "#495 ORDER BY over an aggregate"
      , [ Alcotest.test_case
            "ORDER BY SUM(expr) DESC"
            `Quick
            order_by_a_repeated_aggregate
        ; Alcotest.test_case "ORDER BY alias" `Quick order_by_an_aliased_aggregate
        ; Alcotest.test_case "ASC and LIMIT" `Quick order_by_ascending_and_with_limit
        ; Alcotest.test_case
            "unprojected aggregate key stays hidden"
            `Quick
            order_by_an_unprojected_aggregate_keeps_the_output_width
        ; Alcotest.test_case
            "aggregate key beside a group column"
            `Quick
            order_by_mixes_an_aggregate_and_a_group_column
        ; Alcotest.test_case
            "HAVING SUM(expr) > k"
            `Quick
            having_over_an_aggregate_expression
        ; Alcotest.test_case
            "HAVING and ORDER BY aggregates coexist"
            `Quick
            having_and_order_by_aggregates_coexist
        ; Alcotest.test_case "no GROUP BY" `Quick order_by_an_aggregate_without_group_by
        ] )
    ]
;;
