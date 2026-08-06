(** #558: a subquery beside an aggregate in a GROUP BY projection was rejected
    at bind time.

    {1 The defect}

    PR #557 closed the {i expression} half of #507: an aggregated projection
    item accepts any scalar expression whose leaves are a grouped column, an
    aggregate call, a window function, a literal or a parameter. A subquery was
    deliberately not on that list, so [Sema.bind_expr_agg] answered
    ["subqueries are not supported in aggregate expressions"] and

    a [COUNT] aggregate added to a scalar [SELECT COUNT] subquery, in a
    [GROUP BY] projection, did not run at all. HAVING shared the binder and the
    same boundary.

    {1 The fix}

    Two halves, and both are needed — relaxing only the binder would have
    turned a clean refusal into a silent [NULL], because [eval_expr] answers
    [Row.V_null] for an unresolved [P_subquery].

    - [bind_expr_agg] binds [E_subquery] / [E_exists] / [E_in_select] like any
      other leaf.
    - [stream_aggregate] pre-evaluates the projection items and HAVING, which
      resolves the uncorrelated case once. What survives is correlated, and its
      only possible source in the aggregate {i output} row is a grouped column
      — the row is [group_cols @ aggs] and holds nothing else of the input.
      [binding_of_group_cols] maps each group slot back to the table and column
      it came from and substitutes per group; anything it cannot resolve is
      refused.

    The no-GROUP-BY fast path ([aggregate_fast_path]) gives a projection
    carrying a subquery back to [stream_aggregate] rather than evaluating it in
    its own pure loop.

    {1 Oracle}

    Every expected value here was read off sqlite3 3.45.1. *)

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

let render = function
  | Db.V_int n -> Int64.to_string n
  | Db.V_text s -> s
  | Db.V_real f -> Printf.sprintf "%h" f
  | Db.V_blob b -> Bytes.to_string b
  | Db.V_null -> "NULL"
;;

let rows_of db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.sort
      compare
      (List.map
         (fun r -> Array.to_list (Array.map render r))
         (run (Lwt_stream.to_list stream)))
;;

let check_rows ~label expected actual =
  Alcotest.(check (list (list string))) label (List.sort compare expected) actual
;;

let err_of db sql =
  (* A refusal surfaces either as a [Db.error] or as an exception, and either
     while the plan is built or while the stream is pulled. All four are the
     same outcome for these tests: the query did not answer. *)
  try
    match run (Db.query db sql) with
    | Error e -> Format.asprintf "%a" Db.pp_error e
    | Ok stream ->
      ignore (run (Lwt_stream.to_list stream));
      ""
  with
  | Failure m -> m
  | e -> Printexc.to_string e
;;

let contains msg needle =
  let n = String.length needle
  and m = String.length msg in
  let rec go i = i + n <= m && (String.sub msg i n = needle || go (i + 1)) in
  go 0
;;

(* The issue's schema, with a population that makes the two groups differ. *)
let seed db =
  exec db "CREATE TABLE t (a INTEGER, x INTEGER)";
  exec db "CREATE TABLE u (a INTEGER)";
  exec db "INSERT INTO t VALUES (1,10),(1,20),(2,30)";
  exec db "INSERT INTO u VALUES (5),(6),(7)"
;;

(* ------------------------------------------------------------------ *)
(* The uncorrelated case                                                *)
(* ------------------------------------------------------------------ *)

(* The issue's query verbatim. Group a=1 has two rows, a=2 has one, and the
   subquery is 3 for both. sqlite3 3.45.1: [1|5], [2|4]. *)
let subquery_beside_an_aggregate_in_a_group_by_projection () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"#558 repro"
      [ [ "1"; "5" ]; [ "2"; "4" ] ]
      (rows_of db "SELECT a, COUNT(*) + (SELECT COUNT(*) FROM u) FROM t GROUP BY a"))
;;

(* The same shape without GROUP BY. This one reaches [aggregate_fast_path],
   which evaluates projection expressions in a pure loop with no subquery
   resolution at all — so it has to hand the query back. sqlite3: [6]. *)
let no_group_by_gives_up_the_fast_path () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"3 rows + 3 rows"
      [ [ "6" ] ]
      (rows_of db "SELECT COUNT(*) + (SELECT COUNT(*) FROM u) FROM t"))
;;

(* A bare subquery as its own projection item, alongside an aggregate — no
   arithmetic wrapping it. sqlite3: [1|2|3], [2|1|3]. *)
let bare_subquery_projection_item () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"the subquery stands alone in the projection"
      [ [ "1"; "2"; "3" ]; [ "2"; "1"; "3" ] ]
      (rows_of db "SELECT a, COUNT(*), (SELECT COUNT(*) FROM u) FROM t GROUP BY a"))
;;

(* HAVING shares [bind_expr_agg], so it was refused for the same reason and is
   fixed by the same change. sqlite3: [1|2], [2|1] — both groups are smaller
   than u's three rows. *)
let having_accepts_a_subquery () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"HAVING against an uncorrelated scalar subquery"
      [ [ "1"; "2" ]; [ "2"; "1" ] ]
      (rows_of
         db
         "SELECT a, COUNT(*) FROM t GROUP BY a HAVING COUNT(*) < (SELECT COUNT(*) FROM u)");
    (* And one that excludes every group, so the predicate is demonstrably
       being evaluated rather than passed through. *)
    check_rows
      ~label:"HAVING that no group satisfies"
      []
      (rows_of
         db
         "SELECT a, COUNT(*) FROM t GROUP BY a HAVING COUNT(*) > (SELECT COUNT(*) FROM u)"))
;;

(* EXISTS and IN, in both clauses. sqlite3 answers [1|2],[2|1] for the EXISTS
   spelling and no rows for [a IN (SELECT a FROM u)] (u holds 5, 6, 7). *)
let exists_and_in_spellings () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"EXISTS in HAVING"
      [ [ "1"; "2" ]; [ "2"; "1" ] ]
      (rows_of
         db
         "SELECT a, COUNT(*) FROM t GROUP BY a HAVING EXISTS (SELECT 1 FROM u WHERE u.a \
          = 5)");
    check_rows
      ~label:"IN (SELECT ...) in HAVING excludes both groups"
      []
      (rows_of db "SELECT a, SUM(x) FROM t GROUP BY a HAVING a IN (SELECT a FROM u)");
    check_rows
      ~label:"EXISTS in the projection"
      [ [ "1"; "1" ]; [ "2"; "1" ] ]
      (rows_of db "SELECT a, EXISTS (SELECT 1 FROM u) FROM t GROUP BY a"))
;;

(* ------------------------------------------------------------------ *)
(* The correlated case                                                  *)
(* ------------------------------------------------------------------ *)

(* The case the issue calls out as the one that matters (TPC-C clause 3.3
   condition 4): the subquery references the grouped column. It resolves
   against the aggregate output row, whose slot 0 is [t.a]. sqlite3 3.45.1:
   [1|5], [2|4] — u has three rows above 1 and three above 2. *)
let correlated_on_the_grouped_column () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"the subquery sees this group's key"
      [ [ "1"; "5" ]; [ "2"; "4" ] ]
      (rows_of
         db
         "SELECT a, COUNT(*) + (SELECT COUNT(*) FROM u WHERE u.a > t.a) FROM t GROUP BY a"))
;;

(* A population where the correlated subquery differs per group, so a fix that
   evaluated it once for all groups would be caught. u has one row above 10 and
   none above 30. sqlite3: [10|1], [30|0]. *)
let correlated_value_differs_per_group () =
  with_db (fun db ->
    exec db "CREATE TABLE g (k INTEGER)";
    exec db "CREATE TABLE h (j INTEGER)";
    exec db "INSERT INTO g VALUES (10),(10),(30)";
    exec db "INSERT INTO h VALUES (20)";
    check_rows
      ~label:"each group gets its own subquery answer"
      [ [ "10"; "1" ]; [ "30"; "0" ] ]
      (rows_of db "SELECT k, (SELECT COUNT(*) FROM h WHERE j > g.k) FROM g GROUP BY k"))
;;

(* Correlated HAVING takes the same binding. Only the group whose key is below
   h's single row survives. sqlite3: [10|2]. *)
let correlated_having () =
  with_db (fun db ->
    exec db "CREATE TABLE g (k INTEGER)";
    exec db "CREATE TABLE h (j INTEGER)";
    exec db "INSERT INTO g VALUES (10),(10),(30)";
    exec db "INSERT INTO h VALUES (20)";
    check_rows
      ~label:"HAVING correlated on the group key"
      [ [ "10"; "2" ] ]
      (rows_of
         db
         "SELECT k, COUNT(*) FROM g GROUP BY k HAVING (SELECT COUNT(*) FROM h WHERE j > \
          g.k) > 0"))
;;

(* ------------------------------------------------------------------ *)
(* What cannot be resolved is refused, not answered NULL                *)
(* ------------------------------------------------------------------ *)

(* [t.x] is not a GROUP BY column, so the aggregate output row does not carry
   it: there is no per-group value to substitute. SQLite happens to answer this
   using an arbitrary row of the group; granary refuses rather than pick one,
   and — the point of the test — does not answer [NULL], which is what a
   binder-only relaxation would have produced. *)
let ungrouped_correlation_is_refused () =
  with_db (fun db ->
    seed db;
    let msg =
      err_of
        db
        "SELECT a, COUNT(*) + (SELECT COUNT(*) FROM u WHERE u.a > t.x) FROM t GROUP BY a"
    in
    Alcotest.(check bool)
      (Printf.sprintf "refused rather than answered (got %S)" msg)
      true
      (msg <> "");
    Alcotest.(check bool)
      (Printf.sprintf "and the message names the cause (got %S)" msg)
      true
      (contains msg "#558" && contains msg "GROUP BY"))
;;

(* No GROUP BY at all means no grouped column to correlate against. *)
let correlated_without_group_by_is_refused () =
  with_db (fun db ->
    seed db;
    let msg =
      err_of db "SELECT COUNT(*) + (SELECT COUNT(*) FROM u WHERE u.a > t.a) FROM t"
    in
    Alcotest.(check bool)
      (Printf.sprintf "refused (got %S)" msg)
      true
      (contains msg "#558"))
;;

(* ------------------------------------------------------------------ *)
(* Controls                                                             *)
(* ------------------------------------------------------------------ *)

(* #507's expression half must be unaffected: an aggregated projection with no
   subquery still takes the pure path, fast path included. *)
let plain_aggregate_expressions_are_untouched () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"#507 expression projection"
      [ [ "1"; "3" ]; [ "2"; "2" ] ]
      (rows_of db "SELECT a, COUNT(*) + 1 FROM t GROUP BY a");
    check_rows
      ~label:"no GROUP BY, fast path"
      [ [ "3"; "60" ] ]
      (rows_of db "SELECT COUNT(*), SUM(x) FROM t");
    check_rows
      ~label:"HAVING with no subquery"
      [ [ "1"; "2" ] ]
      (rows_of db "SELECT a, COUNT(*) FROM t GROUP BY a HAVING COUNT(*) > 1"))
;;

(* #558's relaxation is about the expression AROUND the aggregate, not its
   argument.  Since #488 an aggregate argument IS a general expression, so the
   reason this is still refused is no longer "it is not a column reference" —
   it is [Sema.expr_has_subquery_ast], the one shape #488 kept refusing. The
   assertion is unchanged; only its justification moved. *)
let a_subquery_as_an_aggregate_argument_is_still_refused () =
  with_db (fun db ->
    seed db;
    let msg = err_of db "SELECT a, COUNT((SELECT 1 FROM u)) FROM t GROUP BY a" in
    Alcotest.(check bool) (Printf.sprintf "still refused (got %S)" msg) true (msg <> ""))
;;

let () =
  Alcotest.run
    "agg_subquery_558"
    [ ( "uncorrelated subqueries beside an aggregate (#558)"
      , [ Alcotest.test_case
            "the issue's repro"
            `Quick
            subquery_beside_an_aggregate_in_a_group_by_projection
        ; Alcotest.test_case
            "no GROUP BY gives up the fast path"
            `Quick
            no_group_by_gives_up_the_fast_path
        ; Alcotest.test_case
            "a bare subquery projection item"
            `Quick
            bare_subquery_projection_item
        ; Alcotest.test_case "HAVING accepts a subquery" `Quick having_accepts_a_subquery
        ; Alcotest.test_case "EXISTS and IN spellings" `Quick exists_and_in_spellings
        ] )
    ; ( "correlated subqueries beside an aggregate"
      , [ Alcotest.test_case
            "correlated on the grouped column"
            `Quick
            correlated_on_the_grouped_column
        ; Alcotest.test_case
            "the value differs per group"
            `Quick
            correlated_value_differs_per_group
        ; Alcotest.test_case "correlated HAVING" `Quick correlated_having
        ] )
    ; ( "what cannot be resolved is refused"
      , [ Alcotest.test_case
            "an ungrouped correlation is refused"
            `Quick
            ungrouped_correlation_is_refused
        ; Alcotest.test_case
            "correlation without GROUP BY is refused"
            `Quick
            correlated_without_group_by_is_refused
        ] )
    ; ( "controls"
      , [ Alcotest.test_case
            "plain aggregate expressions are untouched"
            `Quick
            plain_aggregate_expressions_are_untouched
        ; Alcotest.test_case
            "a subquery as an aggregate argument is still refused"
            `Quick
            a_subquery_as_an_aggregate_argument_is_still_refused
        ] )
    ]
;;
