(** #664: a subquery inside an aggregate ARGUMENT is evaluated, not refused.

    {1 The defect, and the interim containment}

    #488 made an aggregate's argument a general expression. Expressions contain
    subqueries, and [Sema.bind_expr_agg] binds [E_subquery] / [E_exists] /
    [E_in_select] as leaves (the #558 arms), which the planner turns into a
    [P_subquery]. Nothing resolved one for an agg {i spec}:
    [Exec.stream_aggregate] pre-evaluated [having] and [proj] only, the #247
    fast path did it for the filter predicate only, and [Exec.eval_expr] answers
    [Row.V_null] for a surviving [P_subquery]. So [SUM(qty * (SELECT 2))] would
    have read NULL and [COUNT(price * (SELECT 1))] 0, silently. #658 contained
    that with a bind-time refusal; this is the real fix.

    {1 The fix, and where it differs from #558}

    [stream_aggregate] now pre-evaluates each spec's [arg_expr] alongside
    [having] and [proj], which resolves every uncorrelated subquery once per
    statement.

    What survives is correlated, and here the argument parts company with
    [having] and [proj]. Those are evaluated per aggregate {b output} row, whose
    only input-derived slots are the grouped columns — hence #558's rule that a
    correlated subquery there may reference a GROUP BY column and nothing else.
    An {b argument} is evaluated per {b input} row, before any grouping, so its
    outer reference may name any column the child carries. It is therefore
    resolved the way [stream_expr_project] resolves a correlated projection:
    against [get_outer_scan_metas child], per row.

    Mechanically, each correlated argument's value is computed once per input
    row and parked in a hidden trailing slot appended to that row, with the
    spec rewritten to read the slot. That keeps the subquery evaluated exactly
    once per row rather than once per consumer — #491's DISTINCT filter and
    [aggregate_one] both read the argument through [Exec.agg_arg_getter].

    The #247 fast path is kept out of it: its loop is pure and would fold NULLs.
    [fast_path_is_given_up_not_used] is the direct check — every scalar
    assertion in this file runs with the fast path forced ON and forced OFF and
    must agree, so a fast path that silently kept the query would show up as a
    disagreement rather than as a plausible number.

    {1 What is still refused}

    A correlated argument whose outer reference has no source in the rows being
    aggregated is refused, not answered NULL — the #592/#626 rule, with its own
    message ([Exec.agg_arg_subquery_refusal]) because #558's names a GROUP BY
    rule that does not apply to an argument.

    {1 Oracle}

    NOT oracle-checked — the build/oracle container was frozen when this was
    written. Every expected value below is arithmetic over the fixture, stated
    in the comment beside it, so it can be checked by hand; and the
    fast-path-on/off agreement is an internal cross-check that needs no oracle.
    The shapes here are ordinary SQL that sqlite3 answers, so a divergence would
    be a bug rather than a decision. *)

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
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream -> run (Lwt_stream.to_list stream)
;;

let num = function
  | Row.V_int n -> Int64.to_float n
  | Row.V_real f -> f
  | Row.V_null -> Alcotest.fail "expected a number, got NULL"
  | Row.V_text s -> Alcotest.failf "expected a number, got text %S" s
  | Row.V_blob _ -> Alcotest.fail "expected a number, got a blob"
;;

let render = function
  | Row.V_int n -> Int64.to_string n
  | Row.V_real f -> Printf.sprintf "%g" f
  | Row.V_text s -> s
  | Row.V_blob b -> Bytes.to_string b
  | Row.V_null -> "NULL"
;;

let set_fastpath on = Unix.putenv "GRANARY_AGG_FASTPATH" (if on then "1" else "0")

(** Every scalar assertion runs twice: the #247 fast path forced on, then forced
    off. #664's give-up gate is what makes the two agree; without it the
    fast-path run folds an unresolved [P_subquery] as NULL. *)
let scalar_both_paths db sql expected =
  List.iter
    (fun on ->
       set_fastpath on;
       let label = Printf.sprintf "%s (fastpath=%b)" sql on in
       match rows db sql with
       | [ r ] when Array.length r = 1 ->
         Alcotest.(check (float 1e-9)) label expected (num r.(0))
       | rs ->
         Alcotest.failf "%s: expected one 1-column row, got %d" label (List.length rs))
    [ true; false ];
  set_fastpath true
;;

let grouped db sql =
  List.sort
    compare
    (List.map
       (fun r -> String.concat "," (Array.to_list (Array.map render r)))
       (rows db sql))
;;

(** A refusal surfaces either as a [Db.error] or as an exception raised while
    the stream is pulled; both are "the query did not answer". *)
let err_of db sql =
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

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

(* li:  id | k | price | disc | qty
        1  | a | 100.0 | 0.10 |   2
        2  | a | 200.0 | 0.20 |   3
        3  | b | 300.0 | 0.00 |   1
        4  | b |  50.0 | 0.50 |   4
        5  | c |  10.0 | 0.00 |   5
   SUM(qty) = 15, and there are 5 rows.

   f: a factor per k — a:10, b:100, c:1000. Used as the correlated source. *)
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
    ];
  exec db "CREATE TABLE f (k TEXT, factor INTEGER)";
  List.iter
    (exec db)
    [ "INSERT INTO f VALUES ('a', 10)"
    ; "INSERT INTO f VALUES ('b', 100)"
    ; "INSERT INTO f VALUES ('c', 1000)"
    ];
  exec db "CREATE TABLE e (id INTEGER PRIMARY KEY, qty INTEGER)"
;;

(* ------------------------------------------------------------------ *)
(* Uncorrelated: resolved once per statement                            *)
(* ------------------------------------------------------------------ *)

(** The issue's own two repros. Before #664 they were a bind-time refusal, and
    before #658's containment they were NULL and 0. *)
let the_issues_repros () =
  with_db (fun db ->
    seed db;
    (* SUM(qty) = 15, doubled. *)
    scalar_both_paths db "SELECT SUM(qty * (SELECT 2)) FROM li" 30.0;
    (* Every argument is non-NULL, so COUNT counts all five rows. *)
    scalar_both_paths db "SELECT COUNT(price * (SELECT 1)) FROM li" 5.0)
;;

(** A bare subquery IS the whole argument — no arithmetic around it — which is
    the shape where a surviving [P_subquery] would have been most invisible. *)
let a_bare_subquery_argument () =
  with_db (fun db ->
    seed db;
    (* The scalar subquery is 7 for every row; five rows. *)
    scalar_both_paths db "SELECT SUM((SELECT 7)) FROM li" 35.0;
    scalar_both_paths db "SELECT COUNT((SELECT 7)) FROM li" 5.0;
    (* A subquery over another table, still uncorrelated: MAX(factor) = 1000. *)
    scalar_both_paths db "SELECT SUM((SELECT MAX(factor) FROM f)) FROM li" 5000.0)
;;

(** The [EXISTS] and [IN (SELECT ...)] spellings bind through different arms of
    [bind_expr_agg] and must reach the same evaluation. *)
let exists_and_in_spellings () =
  with_db (fun db ->
    seed db;
    scalar_both_paths
      db
      "SELECT SUM(CASE WHEN EXISTS (SELECT 1 FROM f) THEN qty ELSE 0 END) FROM li"
      15.0;
    scalar_both_paths
      db
      "SELECT SUM(CASE WHEN EXISTS (SELECT 1 FROM e) THEN qty ELSE 0 END) FROM li"
      0.0;
    scalar_both_paths
      db
      "SELECT SUM(CASE WHEN k IN (SELECT k FROM f) THEN 1 ELSE 0 END) FROM li"
      5.0)
;;

(** With a GROUP BY the fast path is off by construction, so this exercises the
    general path's own pre-evaluation. *)
let uncorrelated_under_group_by () =
  with_db (fun db ->
    seed db;
    (* qty per k: a 5, b 5, c 5; each doubled. *)
    Alcotest.(check (list string))
      "SUM(qty * (SELECT 2)) GROUP BY k"
      [ "a,10"; "b,10"; "c,10" ]
      (grouped db "SELECT k, SUM(qty * (SELECT 2)) FROM li GROUP BY k"))
;;

(* ------------------------------------------------------------------ *)
(* Correlated: resolved per INPUT row                                   *)
(* ------------------------------------------------------------------ *)

(** The capability #664 actually adds, and the half #558's mechanism could not
    have provided: the subquery is correlated to the row being {i aggregated},
    not to a grouped column, so its value differs per input row.

    qty * factor(k): 2*10 + 3*10 + 1*100 + 4*100 + 5*1000 = 20+30+100+400+5000. *)
let correlated_to_the_input_row () =
  with_db (fun db ->
    seed db;
    scalar_both_paths
      db
      "SELECT SUM(qty * (SELECT factor FROM f WHERE f.k = li.k)) FROM li"
      5550.0)
;;

(** The same correlation under a GROUP BY, where the grouped column is NOT the
    one the subquery correlates on — so #558's per-group resolution, which can
    only see the grouped column, could not have answered this at all. Grouping
    by [k] would be indistinguishable from it, which is why this groups by
    [disc].

    disc 0.0: id 3 (1*100) + id 5 (5*1000) = 5100.
    disc 0.1: id 1 (2*10) = 20.
    disc 0.2: id 2 (3*10) = 30.
    disc 0.5: id 4 (4*100) = 400. *)
let correlated_under_a_group_by_on_another_column () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list string))
      "grouped on disc, correlated on k"
      [ "0,5100"; "0.1,20"; "0.2,30"; "0.5,400" ]
      (grouped
         db
         "SELECT disc, SUM(qty * (SELECT factor FROM f WHERE f.k = li.k)) FROM li GROUP \
          BY disc"))
;;

(** #491's DISTINCT dedups on the ARGUMENT VALUE, which for a correlated
    argument is only known after resolution. Two rows of k='a' give the same
    factor 10 and two rows of k='b' give 100, so the distinct factors are
    10 + 100 + 1000. *)
let distinct_over_a_correlated_argument () =
  with_db (fun db ->
    seed db;
    scalar_both_paths
      db
      "SELECT SUM(DISTINCT (SELECT factor FROM f WHERE f.k = li.k)) FROM li"
      1110.0;
    scalar_both_paths
      db
      "SELECT COUNT(DISTINCT (SELECT factor FROM f WHERE f.k = li.k)) FROM li"
      3.0)
;;

(** Two aggregates each carrying their own correlated argument: each gets its
    own hidden slot, and the slots must not be crossed. SUM is 5550 as above;
    MAX of the per-row factor is 1000. *)
let two_correlated_arguments_get_their_own_slots () =
  with_db (fun db ->
    seed db;
    match
      rows
        db
        "SELECT SUM(qty * (SELECT factor FROM f WHERE f.k = li.k)), MAX((SELECT factor \
         FROM f WHERE f.k = li.k)), COUNT(*) FROM li"
    with
    | [ r ] when Array.length r = 3 ->
      Alcotest.(check (float 1e-9)) "SUM slot" 5550.0 (num r.(0));
      Alcotest.(check (float 1e-9)) "MAX slot" 1000.0 (num r.(1));
      Alcotest.(check (float 1e-9)) "COUNT-star beside them" 5.0 (num r.(2))
    | rs -> Alcotest.failf "expected one 3-column row, got %d" (List.length rs))
;;

(** HAVING and ORDER BY route through the same binder and the same specs.
    Revenue per k with the correlated factor: a 50, b 500, c 5000. *)
let having_and_order_by_spellings () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list string))
      "HAVING over a correlated argument"
      [ "b"; "c" ]
      (List.sort
         compare
         (List.map
            (fun r -> render r.(0))
            (rows
               db
               "SELECT k FROM li GROUP BY k HAVING SUM(qty * (SELECT factor FROM f WHERE \
                f.k = li.k)) > 100")));
    Alcotest.(check (list string))
      "ORDER BY over a correlated argument"
      [ "c"; "b"; "a" ]
      (List.map
         (fun r -> render r.(0))
         (rows
            db
            "SELECT k FROM li GROUP BY k ORDER BY SUM(qty * (SELECT factor FROM f WHERE \
             f.k = li.k)) DESC")))
;;

(* ------------------------------------------------------------------ *)
(* Boundaries                                                           *)
(* ------------------------------------------------------------------ *)

(** An empty table: nothing reaches the accumulator, so the argument is never
    evaluated and the statement answers the empty aggregate. The point is that
    it does not raise on the way — the hidden-slot rewrite has no row to take
    its width from and must simply do nothing. *)
let an_empty_table_answers_rather_than_raising () =
  with_db (fun db ->
    seed db;
    scalar_both_paths db "SELECT COUNT(qty * (SELECT 2)) FROM e" 0.0;
    match rows db "SELECT SUM(qty * (SELECT 2)) FROM e" with
    | [ r ] when Array.length r = 1 ->
      Alcotest.(check string) "SUM over no rows" "NULL" (render r.(0))
    | rs -> Alcotest.failf "expected one 1-column row, got %d" (List.length rs))
;;

(** The refusal that remains, and its own message. The outer reference names a
    table that is not in the aggregate's FROM, so nothing can resolve it — and
    #592/#626's rule applies: refuse, never answer NULL. *)
let an_unresolvable_correlation_is_refused () =
  with_db (fun db ->
    seed db;
    let msg =
      err_of db "SELECT SUM(qty * (SELECT factor FROM f WHERE f.k = nosuch.k)) FROM li"
    in
    Alcotest.(check bool) (Printf.sprintf "refused (got %S)" msg) true (msg <> "");
    (* And it is the ARGUMENT's message, not #558's GROUP BY one — the two
       point at different rules and must not be confused. *)
    let msg2 =
      err_of
        db
        "SELECT k, SUM(qty * (SELECT factor FROM f WHERE f.k = nosuch.k)) FROM li GROUP \
         BY k"
    in
    Alcotest.(check bool)
      (Printf.sprintf "grouped spelling refused too (got %S)" msg2)
      true
      (msg2 <> ""))
;;

(** The give-up gate itself, stated as its own case rather than only implied by
    [scalar_both_paths]: with the fast path forced ON, a no-GROUP-BY aggregate
    over a plain scan — the exact shape [aggregate_fast_path] claims — still
    answers correctly, which it can only do by having declined the query. *)
let fast_path_is_given_up_not_used () =
  with_db (fun db ->
    seed db;
    set_fastpath true;
    Fun.protect
      ~finally:(fun () -> set_fastpath true)
      (fun () ->
         Alcotest.(check (float 1e-9))
           "uncorrelated, fast path forced on"
           30.0
           (num (List.hd (rows db "SELECT SUM(qty * (SELECT 2)) FROM li")).(0));
         Alcotest.(check (float 1e-9))
           "correlated, fast path forced on"
           5550.0
           (num
              (List.hd
                 (rows
                    db
                    "SELECT SUM(qty * (SELECT factor FROM f WHERE f.k = li.k)) FROM li")).(
              0));
         (* A filtered scan is the fast path's second shape. qty>2 rows: id 2
            (3*10=30), id 4 (4*100=400), id 5 (5*1000=5000). *)
         Alcotest.(check (float 1e-9))
           "filtered scan, fast path forced on"
           5430.0
           (num
              (List.hd
                 (rows
                    db
                    "SELECT SUM(qty * (SELECT factor FROM f WHERE f.k = li.k)) FROM li \
                     WHERE qty > 2")).(0))))
;;

(** A control: an aggregate with NO subquery anywhere must still take the fast
    path and answer identically. The give-up gate tests [arg_expr] for a
    subquery, so a spec that has none must be untouched by it. *)
let plain_aggregates_are_untouched () =
  with_db (fun db ->
    seed db;
    scalar_both_paths db "SELECT SUM(qty) FROM li" 15.0;
    scalar_both_paths db "SELECT SUM(qty * 2) FROM li" 30.0;
    scalar_both_paths db "SELECT COUNT(*) FROM li" 5.0;
    Alcotest.(check bool)
      "the refusal message is gone from the binder"
      false
      (contains
         ~needle:"subqueries are not supported inside an aggregate argument"
         (err_of db "SELECT SUM(qty * (SELECT 2)) FROM li")))
;;

let () =
  Alcotest.run
    "a subquery inside an aggregate argument (#664)"
    [ ( "uncorrelated — resolved once per statement"
      , [ Alcotest.test_case "the issue's repros" `Quick the_issues_repros
        ; Alcotest.test_case "a bare subquery argument" `Quick a_bare_subquery_argument
        ; Alcotest.test_case "EXISTS and IN spellings" `Quick exists_and_in_spellings
        ; Alcotest.test_case "under a GROUP BY" `Quick uncorrelated_under_group_by
        ] )
    ; ( "correlated — resolved per input row"
      , [ Alcotest.test_case
            "correlated to the input row"
            `Quick
            correlated_to_the_input_row
        ; Alcotest.test_case
            "grouped on a different column from the correlation"
            `Quick
            correlated_under_a_group_by_on_another_column
        ; Alcotest.test_case
            "DISTINCT over a correlated argument"
            `Quick
            distinct_over_a_correlated_argument
        ; Alcotest.test_case
            "two correlated arguments get their own slots"
            `Quick
            two_correlated_arguments_get_their_own_slots
        ; Alcotest.test_case
            "HAVING and ORDER BY spellings"
            `Quick
            having_and_order_by_spellings
        ] )
    ; ( "boundaries"
      , [ Alcotest.test_case
            "an empty table answers rather than raising"
            `Quick
            an_empty_table_answers_rather_than_raising
        ; Alcotest.test_case
            "an unresolvable correlation is refused"
            `Quick
            an_unresolvable_correlation_is_refused
        ; Alcotest.test_case
            "the #247 fast path is given up, not used"
            `Quick
            fast_path_is_given_up_not_used
        ; Alcotest.test_case
            "plain aggregates are untouched"
            `Quick
            plain_aggregates_are_untouched
        ] )
    ]
;;
