(** #670: a correlated subquery under a [COLLATE] is answered, not refused.

    {1 The defect}

    Four functions in [lib/sql/exec.ml] walk a [Plan.expr] looking for
    subquery-bearing nodes.  Three of them —
    [plan_expr_has_subquery], [plan_expr_subqueries_use_param] and
    [plan_expr_embedded_stmts] — descended into [P_collate].  The fourth,
    [substitute_outer_in_plan_expr], did not: it fell to a [| _ -> e]
    catch-all.

    So for

    {v
      SELECT k FROM o WHERE x = (SELECT v FROM i WHERE i.fk = o.k) COLLATE NOCASE
    v}

    the three that {e recognise} a correlated subquery all said yes, and the one
    that would have {e resolved} its outer reference left [o.k] in place.  The
    statement then failed [Sema.bind] and came back as [correlated_filter_refusal].
    A refusal rather than a wrong answer — but for a query the engine is
    perfectly able to run, and the refusal was reached by a walker set
    disagreeing with itself.

    #493 wrote the one-line arm and then reverted it on purpose: turning a
    refusal into rows is an error-to-answer change, and that PR shipped in a
    batch that could not be built, so it could not carry a test.  Nothing in
    [test/] exercised [COLLATE] over a subquery at all.  This file is that test.

    {1 What is pinned, and what makes it a real test}

    The collation has to be load-bearing or the test would pass on a build that
    ignored [COLLATE] entirely.  Every fixture below is chosen so that the same
    query {e without} [COLLATE NOCASE] returns no rows: the values differ only
    in case.  So "answers [a; b]" and "answers nothing" are the fix and its
    absence respectively, and neither can be reached by accident.

    {1 Oracle}

    Every expected value here was taken from the [sqlite3] in the dev image,
    run on the same schema and data (see each test's comment). *)

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

(* The issue's schema.  Every [o.x] matches its [i.v] under NOCASE and under no
   other collation, so the query answers nothing on a build that drops the
   COLLATE — which is what makes each assertion below discriminating. *)
let seed db =
  exec db "CREATE TABLE o (k TEXT, x TEXT)";
  exec db "CREATE TABLE i (fk TEXT, v TEXT)";
  exec db "INSERT INTO o VALUES ('a','HELLO'),('b','world'),('c','zzz')";
  exec db "INSERT INTO i VALUES ('a','hello'),('b','WORLD'),('c','yyy')"
;;

(* sqlite3, same schema and data:

     sqlite> SELECT k FROM o WHERE x = (SELECT v FROM i WHERE i.fk = o.k)
        ...>   COLLATE NOCASE ORDER BY k;
     a
     b

   This is the issue's headline query, and before the fix it was refused. *)
let the_issues_query_answers_rows () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"correlated subquery under COLLATE NOCASE"
      [ [ "a" ]; [ "b" ] ]
      (rows_of
         db
         "SELECT k FROM o WHERE x = (SELECT v FROM i WHERE i.fk = o.k) COLLATE NOCASE"))
;;

(* The control, and the reason the test above is not vacuous.  sqlite3 returns
   no rows for the same query without the COLLATE; so must granary, or the
   collation is being ignored and the assertion above proves nothing about it. *)
let without_the_collate_the_same_query_answers_nothing () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"binary comparison matches none of the three rows"
      []
      (rows_of db "SELECT k FROM o WHERE x = (SELECT v FROM i WHERE i.fk = o.k)"))
;;

(* The substitution is recursive, so a COLLATE anywhere on the path to the
   subquery has to be descended through, not just one sitting directly above it.
   This spelling was refused before the fix too.

   sqlite3:
     SELECT k FROM o WHERE NOT (x <> (SELECT v FROM i WHERE i.fk = o.k)
       COLLATE NOCASE) ORDER BY k;
     a
     b *)
let a_collate_nested_under_another_node_is_descended_through () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"COLLATE under a NOT/<> spine"
      [ [ "a" ]; [ "b" ] ]
      (rows_of
         db
         "SELECT k FROM o WHERE NOT (x <> (SELECT v FROM i WHERE i.fk = o.k) COLLATE \
          NOCASE)"))
;;

(* The outer reference under the COLLATE must still be the OUTER row's, not the
   subquery's own.  A substitution that descended but bound the wrong scope
   would answer rows here too — the wrong ones — so the fix is pinned by the
   VALUES it produces, not merely by the absence of a refusal: three outer rows,
   three different correlated values, each matching its own [i.fk = o.k].

   {b This is also where granary diverges from sqlite3, on the collation and
   not on the correlation.}  sqlite3 answers

     a|hello
     b|WORLD
     c|yyy

   because a collation is a property of a COMPARISON and never rewrites the
   value.  granary evaluates [P_collate (_, NOCASE)] as
   [String.lowercase_ascii] on the value itself ([Exec.eval_expr]), so a
   COLLATE in a PROJECTION changes what comes back: [WORLD] is returned as
   [world].  That is pre-existing, has nothing to do with #670 — the plain
   uncorrelated shape below does the same on unmodified code — and is tracked
   as #722.  Pinned here as granary's current answer rather than sqlite3's, so
   that fixing #722 shows up as a deliberate change to this line. *)
let the_substituted_reference_is_the_outer_rows () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"each outer row gets its own correlated value (#722: b is folded)"
      [ [ "a"; "hello" ]; [ "b"; "world" ]; [ "c"; "yyy" ] ]
      (rows_of db "SELECT k, (SELECT v FROM i WHERE i.fk = o.k) COLLATE NOCASE FROM o"))
;;

(* #722, isolated: no subquery, no correlation, nothing #670 touches — so this
   is what the engine did before this branch and what it still does.  It is
   here to keep the divergence above from being read as something the fix
   introduced.

   sqlite3: a|HELLO / b|world / c|zzz  (the stored values, unchanged). *)
let collate_in_a_projection_folds_the_value_pre_existing_722 () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"granary folds; sqlite3 would return HELLO and zzz unchanged"
      [ [ "a"; "hello" ]; [ "b"; "world" ]; [ "c"; "zzz" ] ]
      (rows_of db "SELECT k, x COLLATE NOCASE FROM o"))
;;

(* An UNcorrelated subquery under a COLLATE was always fine — it is folded to a
   constant before any substitution runs, so it never reached the missing arm.
   Kept as the boundary: the fix must not have moved it.

   sqlite3:
     SELECT k FROM o WHERE x = (SELECT v FROM i WHERE i.fk = 'a') COLLATE NOCASE
       ORDER BY k;
     a *)
let an_uncorrelated_subquery_under_collate_is_unchanged () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"uncorrelated subquery under COLLATE still answers"
      [ [ "a" ] ]
      (rows_of
         db
         "SELECT k FROM o WHERE x = (SELECT v FROM i WHERE i.fk = 'a') COLLATE NOCASE"))
;;

(* EXISTS and IN (SELECT …) are the other two subquery-bearing nodes the walker
   set is about.  Neither can sit under a COLLATE in a way the grammar accepts
   (COLLATE takes a scalar operand), so there is no third and fourth case to
   write — but a correlated EXISTS whose own WHERE carries a COLLATE does reach
   the same substitution, one statement further in.

   sqlite3:
     SELECT k FROM o WHERE EXISTS (SELECT 1 FROM i WHERE i.fk = o.k
       AND i.v = o.x COLLATE NOCASE) ORDER BY k;
     a
     b *)
let a_collate_inside_a_correlated_exists_body () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"COLLATE inside the EXISTS body, outer reference on both sides"
      [ [ "a" ]; [ "b" ] ]
      (rows_of
         db
         "SELECT k FROM o WHERE EXISTS (SELECT 1 FROM i WHERE i.fk = o.k AND i.v = o.x \
          COLLATE NOCASE)"))
;;

(* #721, the boundary this fix used to stop at — {b now resolved}.

   When this file was written, [substitute_outer_in_expr] was exhaustive but
   three constructors that carry sub-expressions ([E_agg], [E_agg_distinct],
   [E_window]) were listed with an explicit [-> e] rather than descended into,
   and this case asserted that the query below was REFUSED.  The comment it
   carried set the rule for that moment: "if a later change resolves it, this
   assertion fails and whoever made it must replace the case rather than delete
   it".  #721 is that change, and this is the replacement — the same query, the
   same fixture, asserting the answer instead of the refusal.

   ([E_fts_snippet] was once counted as a fourth constructor and is not one: it
   is a record of a table name, a column index, three string tags and a token
   count with no [expr] field ([Ast.E_fts_snippet], ast.ml:247), so it is a
   true leaf.)

   sqlite3 answers [a] for this query, which is what it answers now.  The full
   #721 coverage — all three constructors, each with a control whose value
   differs — lives in [test/test_outer_ref_721.ml]; this case stays here so
   that #670's own file keeps recording where its scope ended and what closed
   it. *)
let an_outer_reference_inside_an_aggregate_argument_is_answered_721 () =
  with_db (fun db ->
    exec db "CREATE TABLE oa (k TEXT, n INTEGER)";
    exec db "CREATE TABLE ia (fk TEXT, v INTEGER)";
    exec db "INSERT INTO oa VALUES ('a',1)";
    exec db "INSERT INTO ia VALUES ('a',5)";
    check_rows
      ~label:"#721: answered, not refused"
      [ [ "a" ] ]
      (rows_of
         db
         "SELECT k FROM oa WHERE EXISTS (SELECT 1 FROM ia WHERE ia.fk = oa.k GROUP BY \
          ia.fk HAVING SUM(ia.v + oa.n) > 0)"))
;;

let () =
  Alcotest.run
    "test_collate_outer_ref_670"
    [ ( "answered_not_refused"
      , [ Alcotest.test_case
            "the_issues_query_answers_rows"
            `Quick
            the_issues_query_answers_rows
        ; Alcotest.test_case
            "a_collate_nested_under_another_node_is_descended_through"
            `Quick
            a_collate_nested_under_another_node_is_descended_through
        ; Alcotest.test_case
            "the_substituted_reference_is_the_outer_rows"
            `Quick
            the_substituted_reference_is_the_outer_rows
        ; Alcotest.test_case
            "a_collate_inside_a_correlated_exists_body"
            `Quick
            a_collate_inside_a_correlated_exists_body
        ] )
    ; ( "the_collate_is_load_bearing"
      , [ Alcotest.test_case
            "without_the_collate_the_same_query_answers_nothing"
            `Quick
            without_the_collate_the_same_query_answers_nothing
        ; Alcotest.test_case
            "an_uncorrelated_subquery_under_collate_is_unchanged"
            `Quick
            an_uncorrelated_subquery_under_collate_is_unchanged
        ] )
    ; ( "divergence_722"
      , [ Alcotest.test_case
            "collate_in_a_projection_folds_the_value_pre_existing_722"
            `Quick
            collate_in_a_projection_folds_the_value_pre_existing_722
        ] )
    ; ( "resolved_by_721"
      , [ Alcotest.test_case
            "an_outer_reference_inside_an_aggregate_argument_is_answered_721"
            `Quick
            an_outer_reference_inside_an_aggregate_argument_is_answered_721
        ] )
    ]
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5"]
[@@@ai_provider "Anthropic"]
