(** #662: a compound [ORDER BY <name>] names an OUTPUT column, not a column of
    the leftmost arm's table.

    {1 The defect}

    [Sema.bind_compound_order_keys] resolved an ordinal against the compound's
    output row (#489) and everything else against the leftmost arm's
    [table_meta].  The latter yields [BE_col <table ordinal>], which
    [Planner.plan_order_keys] turns into [P_col i] and [plan_compound]
    evaluates against the {e set-op output row}.  Those two indices coincide
    only when the arm projects a leading prefix of its table, in order.

    Every compound-ORDER-BY test in the tree before this one used a
    single-column table, where table index and output index are trivially
    equal.  That is the whole reason it survived: off that case it produced

    - a silent {b wrong answer} — [SELECT b, a … ORDER BY a] sorted by [b];
    - a {b crash} — [SELECT b … ORDER BY b] indexed column 1 of a one-column
      output row, and [Exec]'s unchecked [row.(i)] raised [Invalid_argument]
      mid-query instead of returning a [Db.error].

    {1 The rule now}

    A compound ORDER BY term is an output column: an ordinal, a name, an arm's
    alias, or a qualified name whose column part is one.  Anything else is an
    error.  That is sqlite3's rule and every expectation below is taken from
    the [sqlite3] in the dev image, with its output quoted at each test.

    {1 The one place it does not apply}

    An {b aggregated} arm carries no output names at all — [agg_proj_item] has
    neither name nor alias — so for that arm the old binding is kept rather
    than refusing a query that works today on the strength of a name list the
    binder knows is incomplete.  That arm keeps both original defects, over a
    strictly smaller surface, and is tracked as #724.  Pinned at the bottom so
    the residual is a recorded fact rather than a gap. *)

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

(* Deliberately NOT sorted afterwards: this file is about ORDER BY, so the row
   order as the engine produced it is the thing under test. *)
let rows_of db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun r -> Array.to_list (Array.map render r))
      (run (Lwt_stream.to_list stream))
;;

let check_rows ~label expected actual =
  Alcotest.(check (list (list string))) label expected actual
;;

(* A failure surfaces either as a [Db.error] or as an exception, and either at
   bind time or while the stream is pulled.  Before this fix the crash case
   arrived as an [Invalid_argument] from deep inside [Exec]; the point of the
   fix is that it now arrives as neither. *)
let outcome db sql =
  try
    match run (Db.query db sql) with
    | Error e -> Error (Format.asprintf "%a" Db.pp_error e)
    | Ok stream ->
      Ok
        (List.map
           (fun r -> Array.to_list (Array.map render r))
           (run (Lwt_stream.to_list stream)))
  with
  | Failure m -> Error m
  | e -> Error (Printexc.to_string e)
;;

let refused db sql ~label =
  match outcome db sql with
  | Error _ -> ()
  | Ok rows ->
    Alcotest.failf "%s: expected a refusal, got %d row(s)" label (List.length rows)
;;

(* The issue's schema.  [b] is deliberately not a leading prefix of the table
   in the projections below, and the two columns hold disjoint value ranges, so
   sorting by the wrong one is visible in the values rather than only in the
   order. *)
let seed db =
  exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
  exec db "CREATE TABLE u (a INTEGER, b INTEGER)";
  exec db "INSERT INTO t VALUES (3,30),(1,10)";
  exec db "INSERT INTO u VALUES (2,20),(4,40)"
;;

(* sqlite3:
     SELECT b, a FROM t UNION ALL SELECT b, a FROM u ORDER BY a;
     10|1
     20|2
     30|3
     40|4

   Before the fix [a] resolved to table index 0 and the sort read output
   column 0, which holds [b] — so the rows came back in b-order (which here
   happens to be the same order, hence the second, discriminating case
   below). *)
let a_name_names_an_output_column () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"sorted by a, the second output column"
      [ [ "10"; "1" ]; [ "20"; "2" ]; [ "30"; "3" ]; [ "40"; "4" ] ]
      (rows_of db "SELECT b, a FROM t UNION ALL SELECT b, a FROM u ORDER BY a"))
;;

(* The discriminating version of the case above: [b] and [a] disagree about the
   order, so binding the name to the wrong column changes the ANSWER and not
   just the index.

   sqlite3, on this test's own t2/u2 data:
     SELECT b, a FROM t2 UNION ALL SELECT b, a FROM u2 ORDER BY a;
     40|1
     30|2
     20|3
     10|4 *)
let the_wrong_column_would_change_the_answer () =
  with_db (fun db ->
    exec db "CREATE TABLE t2 (a INTEGER, b INTEGER)";
    exec db "CREATE TABLE u2 (a INTEGER, b INTEGER)";
    (* a ascending is b descending, so the two orders are opposites. *)
    exec db "INSERT INTO t2 VALUES (1,40),(2,30)";
    exec db "INSERT INTO u2 VALUES (3,20),(4,10)";
    check_rows
      ~label:"sorted by a ascending, which is b descending"
      [ [ "40"; "1" ]; [ "30"; "2" ]; [ "20"; "3" ]; [ "10"; "4" ] ]
      (rows_of db "SELECT b, a FROM t2 UNION ALL SELECT b, a FROM u2 ORDER BY a"))
;;

(* The crash. [b] is table index 1 and the output row is one column wide, so
   the old binding produced [P_col 1] over a one-element array.

   sqlite3:
     SELECT b FROM t UNION ALL SELECT b FROM u ORDER BY b;
     10
     20
     30
     40 *)
let a_projection_narrower_than_the_table_no_longer_indexes_past_the_row () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"one-column output, sorted by its only column"
      [ [ "10" ]; [ "20" ]; [ "30" ]; [ "40" ] ]
      (rows_of db "SELECT b FROM t UNION ALL SELECT b FROM u ORDER BY b"))
;;

(* An arm's ALIAS is an output column name, and used to fail outright with
   "unknown column" because the binder only ever consulted the table.

   sqlite3:
     SELECT a AS z FROM t UNION ALL SELECT a FROM u ORDER BY z;
     1
     2
     3
     4 *)
let an_arms_alias_resolves () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"ORDER BY the leftmost arm's alias"
      [ [ "1" ]; [ "2" ]; [ "3" ]; [ "4" ] ]
      (rows_of db "SELECT a AS z FROM t UNION ALL SELECT a FROM u ORDER BY z"))
;;

(* sqlite3 accepts a qualified term in a compound ORDER BY and resolves it on
   the column part:
     SELECT b, a FROM t UNION ALL SELECT b, a FROM u ORDER BY t.a;
     10|1
     20|2
     30|3
     40|4 *)
let a_qualified_name_resolves_on_its_column_part () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"t.a resolves wherever a does"
      [ [ "10"; "1" ]; [ "20"; "2" ]; [ "30"; "3" ]; [ "40"; "4" ] ]
      (rows_of db "SELECT b, a FROM t UNION ALL SELECT b, a FROM u ORDER BY t.a"))
;;

(* The ordinal path is #489's and is unchanged; kept here so a future edit to
   this function cannot move it without a test noticing.

   sqlite3:
     SELECT b, a FROM t UNION ALL SELECT b, a FROM u ORDER BY 2;
     10|1
     20|2
     30|3
     40|4 *)
let the_ordinal_path_is_unchanged () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"ORDER BY 2 is the second output column"
      [ [ "10"; "1" ]; [ "20"; "2" ]; [ "30"; "3" ]; [ "40"; "4" ] ]
      (rows_of db "SELECT b, a FROM t UNION ALL SELECT b, a FROM u ORDER BY 2"))
;;

(* A name no output column has. sqlite3:
     SELECT b FROM t UNION ALL SELECT b FROM u ORDER BY a;
     Parse error: 1st ORDER BY term does not match any column in the result set

   Before the fix this bound to table index 0 and silently sorted by the
   output's column 0 — which is [b], not [a]. *)
let a_name_that_is_not_an_output_column_is_refused () =
  with_db (fun db ->
    seed db;
    refused
      db
      "SELECT b FROM t UNION ALL SELECT b FROM u ORDER BY a"
      ~label:"ORDER BY a over a b-only output")
;;

(* An expression. sqlite3:
     SELECT b, a FROM t UNION ALL SELECT b, a FROM u ORDER BY a+1;
     Parse error: 1st ORDER BY term does not match any column in the result set *)
let an_expression_term_is_refused () =
  with_db (fun db ->
    seed db;
    refused
      db
      "SELECT b, a FROM t UNION ALL SELECT b, a FROM u ORDER BY a+1"
      ~label:"ORDER BY a+1 in a compound")
;;

(* #724, now CLOSED — this test used to pin the residual and pins its
   replacement.

   An aggregated arm records only WHAT each output column is
   ([AP_group_col]/[AP_agg_slot]/[AP_window_slot]/[AP_expr]) and never what it
   is CALLED, so it exposed no output names and every aggregated compound fell
   back to the pre-#662 table-meta binding.  The fallback kept such queries
   ANSWERING, which is what the old assertion checked — but it also meant a term
   naming no output column at all was bound against the pre-aggregation row and
   sorted the post-aggregation output by nothing.  That is the one arm of three
   that answered silently wrong: sqlite3 refuses the term, and #663 refuses the
   non-compound spelling of the very same query.

   Names now come from the explicit alias and, for an [AP_group_col], from the
   grouped column itself.  An aggregate slot stays unnamed on purpose and
   [compound_out_names_authoritative] accounts for it as UNNAMEABLE rather than
   unknown — sqlite3 names it by rendering the call ([COUNT( * )]), which no
   bare identifier in an ORDER BY can equal.

   All four spellings oracle-checked against sqlite3 3.45.1. *)
let an_aggregated_arm_resolves_its_output_names_724 () =
  with_db (fun db ->
    exec db "CREATE TABLE g (nm TEXT, val INTEGER)";
    exec db "INSERT INTO g VALUES ('a',3),('a',1),('b',2),('b',9)";
    let arm p = Printf.sprintf "SELECT %s FROM g GROUP BY nm" p in
    let comp p ord = Printf.sprintf "%s UNION ALL %s ORDER BY %s" (arm p) (arm p) ord in
    check_rows
      ~label:"a grouped column resolves by name"
      [ [ "a"; "2" ]; [ "a"; "2" ]; [ "b"; "2" ]; [ "b"; "2" ] ]
      (rows_of db (comp "nm, COUNT(*)" "nm"));
    check_rows
      ~label:"an alias on the grouped column resolves"
      [ [ "a"; "2" ]; [ "a"; "2" ]; [ "b"; "2" ]; [ "b"; "2" ] ]
      (rows_of db (comp "nm AS z, COUNT(*)" "z"));
    (* An alias on the AGGREGATE itself: unreachable before, because the
       fallback bound [c] against the table and [g] has no column [c]. *)
    check_rows
      ~label:"an alias on the aggregate resolves, where the fallback refused it"
      [ [ "a"; "2" ]; [ "b"; "2" ]; [ "a"; "2" ]; [ "b"; "2" ] ]
      (rows_of db (comp "nm, COUNT(*) AS c" "c"));
    (* The one that used to answer in an arbitrary order. *)
    refused
      ~label:"a term naming no output column is refused, not silently unsorted"
      db
      (comp "nm, COUNT(*)" "val"))
;;

(* ------------------------------------------------------------------ *)
(* Review follow-ups: the shapes the first revision got wrong           *)
(* ------------------------------------------------------------------ *)

(* COLLATE.  [E_collate (E_col "a", c)] is not [E_col], so classifying the term
   without peeling it first sent it to the "expression" arm and refused it —
   a query that answers on main and in sqlite3.  sqlite3 calls
   [sqlite3ExprSkipCollateAndLikely] before matching the term against the
   result set, precisely so COLLATE disqualifies neither a name nor an ordinal.

   sqlite3: a / B / C for both spellings. *)
let collate_does_not_disqualify_a_term () =
  with_db (fun db ->
    exec db "CREATE TABLE s1 (a TEXT)";
    exec db "CREATE TABLE s2 (a TEXT)";
    exec db "INSERT INTO s1 VALUES ('a'),('C')";
    exec db "INSERT INTO s2 VALUES ('B')";
    check_rows
      ~label:"ORDER BY <name> COLLATE NOCASE"
      [ [ "a" ]; [ "B" ]; [ "C" ] ]
      (rows_of db "SELECT a FROM s1 UNION ALL SELECT a FROM s2 ORDER BY a COLLATE NOCASE");
    check_rows
      ~label:"ORDER BY <ordinal> COLLATE NOCASE"
      [ [ "a" ]; [ "B" ]; [ "C" ] ]
      (rows_of db "SELECT a FROM s1 UNION ALL SELECT a FROM s2 ORDER BY 1 COLLATE NOCASE"))
;;

(* A JOINED arm.  [select_proj_lookup] returns [right_col_offset + i] for a
   right-table column while [table_meta] is the leftmost FROM item alone, so
   resolving the projection against [table_meta] only, every right-table column
   is past its end.  The name list used to DROP those, coming out SHORTER than
   the output row with the surviving names shifted down — and
   [names_are_complete] (an all-[Some] test with no length check) declared that
   authoritative.  [ORDER BY p] then resolved to output index 0, which holds
   [s]: #662's own failure mode in a shape the guard vouched for.

   The fixture is built so the two answers are visibly different — [s]
   descending is [p] ascending — and both directions are pinned, since the
   defect refused [ORDER BY s] while mis-answering [ORDER BY p].

   sqlite3: 200|1 200|1 100|2 100|2 for [ORDER BY p]; the reverse for
   [ORDER BY s]. *)
let a_joined_arms_right_table_columns_resolve () =
  with_db (fun db ->
    exec db "CREATE TABLE j1 (p INTEGER, q INTEGER)";
    exec db "CREATE TABLE j2 (r INTEGER, s INTEGER)";
    exec db "INSERT INTO j1 VALUES (1,9),(2,8)";
    exec db "INSERT INTO j2 VALUES (1,200),(2,100)";
    let arm = "SELECT s, p FROM j1 JOIN j2 ON j1.p=j2.r" in
    check_rows
      ~label:"ORDER BY p sorts by p, not by the output column that shares its index"
      [ [ "200"; "1" ]; [ "200"; "1" ]; [ "100"; "2" ]; [ "100"; "2" ] ]
      (rows_of db (Printf.sprintf "%s UNION ALL %s ORDER BY p" arm arm));
    check_rows
      ~label:"and ORDER BY s resolves at all, where it used to be unknown column"
      [ [ "100"; "2" ]; [ "100"; "2" ]; [ "200"; "1" ]; [ "200"; "1" ] ]
      (rows_of db (Printf.sprintf "%s UNION ALL %s ORDER BY s" arm arm));
    check_rows
      ~label:"a qualified reference to the joined right table resolves too"
      [ [ "100"; "2" ]; [ "100"; "2" ]; [ "200"; "1" ]; [ "200"; "1" ] ]
      (rows_of db (Printf.sprintf "%s UNION ALL %s ORDER BY j2.s" arm arm)))
;;

(* The two preceding shapes COMBINED: a joined arm whose projection also
   carries an alias.  This is the arm the join-awareness was missing from.

   One alias anywhere sends the whole projection through [expr_proj]
   (parser.mly:896-905), so the unaliased right-table column [s] arrives with
   [alias = None] and its name has to be recovered from its ordinal — which is
   [right_col_offset + 1], past the end of [j1]'s meta.  The [expr_proj] arm
   recovered names against [table_meta] alone, answered [None], and the whole
   compound fell back to the pre-#662 binding: [ORDER BY s] was refused with
   "unknown column: j1.s" where sqlite3 answers.  The [proj] arm next door was
   already join-aware; [col_name_at] is now hoisted above both so they cannot
   disagree.

   [ORDER BY q] is the second half and is the one that must NOT move: the alias
   [q] SHADOWS [j1.q], and the two rank oppositely on this fixture ([p] is 1,2
   where [j1.q] is 9,8), so a fallback to the table-meta binding is visible as
   reversed rows rather than as an error.

   sqlite3 3.45.1, oracle-checked: [ORDER BY q] gives 1|200 1|200 2|100 2|100;
   [ORDER BY s] gives 2|100 2|100 1|200 1|200. *)
let a_joined_arm_with_an_alias_resolves_both_halves () =
  with_db (fun db ->
    exec db "CREATE TABLE j1 (p INTEGER, q INTEGER)";
    exec db "CREATE TABLE j2 (r INTEGER, s INTEGER)";
    exec db "INSERT INTO j1 VALUES (1,9),(2,8)";
    exec db "INSERT INTO j2 VALUES (1,200),(2,100)";
    let arm = "SELECT p AS q, s FROM j1 JOIN j2 ON j1.p=j2.r" in
    check_rows
      ~label:"ORDER BY q names the OUTPUT column, not the shadowed j1.q"
      [ [ "1"; "200" ]; [ "1"; "200" ]; [ "2"; "100" ]; [ "2"; "100" ] ]
      (rows_of db (Printf.sprintf "%s UNION ALL %s ORDER BY q" arm arm));
    check_rows
      ~label:"and the unaliased RIGHT-table column resolves instead of refusing"
      [ [ "2"; "100" ]; [ "2"; "100" ]; [ "1"; "200" ]; [ "1"; "200" ] ]
      (rows_of db (Printf.sprintf "%s UNION ALL %s ORDER BY s" arm arm)))
;;

(* A projection that merely MIXES one alias with a plain column.  The parser
   emits [`Exprs] for the whole list as soon as ONE item is aliased
   (parser.mly:896-905), so every other item carries [alias = None] — and if
   that read as "no name", this very common shape fell to the pre-#662
   table-meta fallback and kept the silent wrong answer.  It is not the
   aggregated arm and has nothing to do with #724.

   On this file's [seed] fixture [a] and [b] happen to rank alike, so the
   discriminator is a SEPARATE fixture in which they rank OPPOSITELY — without
   that, sorting by the wrong column produces the same rows and the test proves
   nothing. Both are asserted below.

   sqlite3, on the opposed fixture: 10|4 20|3 30|2 40|1 (sorted by b). Before
   the fix, both this branch and main answered 40|1 30|2 20|3 10|4 — by a. *)
let a_mixed_alias_and_plain_projection_still_resolves_names () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
    exec db "CREATE TABLE u (a INTEGER, b INTEGER)";
    exec db "INSERT INTO t VALUES (1,40),(2,30)";
    exec db "INSERT INTO u VALUES (3,20),(4,10)";
    check_rows
      ~label:"ORDER BY b sorts by b, though the sibling item is aliased"
      [ [ "10"; "4" ]; [ "20"; "3" ]; [ "30"; "2" ]; [ "40"; "1" ] ]
      (rows_of db "SELECT b AS z, a FROM t UNION ALL SELECT b AS z, a FROM u ORDER BY b"))
;;

(* sqlite3 resolves a compound ORDER BY term in two steps: against the result
   column NAMES, then — failing that — by resolving it in the arm own scope and
   comparing it against the result-set EXPRESSIONS.  Only the first step was
   implemented, so a term naming a column that the projection RENAMED was
   refused, where sqlite3 sorts by it: the alias renamed the column, it did not
   hide the column underneath.

   sqlite3: 10 / 20 / 30 / 40. *)
let a_term_matching_an_output_expression_resolves () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"ORDER BY b, where b is projected only as z"
      [ [ "10" ]; [ "20" ]; [ "30" ]; [ "40" ] ]
      (rows_of db "SELECT b AS z FROM t UNION ALL SELECT b AS z FROM u ORDER BY b"))
;;

(* RECORDED DIVERGENCE, not a passing grade.  The qualifier of a qualified term
   is not checked against the arms, so a qualifier naming no arm at all is
   accepted and the term resolves on its column part alone.

   sqlite3 refuses both of these ("1st ORDER BY term does not match any column
   in the result set"), because it applies its name step only to a BARE name
   and resolves a qualified term in each arm own FROM.

   Pinned so the divergence is a decision on record rather than something
   rediscovered later.  See the comment on [by_name] for why the obvious fix —
   trying the expression step first for qualified terms — was implemented and
   rejected: it refuses [ORDER BY j2.s], which sqlite3 answers and which
   [a_joined_arms_right_table_columns_resolve] above pins as working. *)
let an_unknown_qualifier_is_accepted_divergence () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"zzz names no arm, and the term still resolves on 'a' (sqlite3 refuses)"
      [ [ "10"; "1" ]; [ "20"; "2" ]; [ "30"; "3" ]; [ "40"; "4" ] ]
      (rows_of db "SELECT b, a FROM t UNION ALL SELECT b, a FROM u ORDER BY zzz.a"))
;;

let () =
  Alcotest.run
    "test_compound_order_by_662"
    [ ( "a_name_is_an_output_column"
      , [ Alcotest.test_case
            "a_name_names_an_output_column"
            `Quick
            a_name_names_an_output_column
        ; Alcotest.test_case
            "the_wrong_column_would_change_the_answer"
            `Quick
            the_wrong_column_would_change_the_answer
        ; Alcotest.test_case
            "a_projection_narrower_than_the_table_no_longer_indexes_past_the_row"
            `Quick
            a_projection_narrower_than_the_table_no_longer_indexes_past_the_row
        ; Alcotest.test_case "an_arms_alias_resolves" `Quick an_arms_alias_resolves
        ; Alcotest.test_case
            "a_qualified_name_resolves_on_its_column_part"
            `Quick
            a_qualified_name_resolves_on_its_column_part
        ; Alcotest.test_case
            "the_ordinal_path_is_unchanged"
            `Quick
            the_ordinal_path_is_unchanged
        ] )
    ; ( "review_follow_ups"
      , [ Alcotest.test_case
            "collate_does_not_disqualify_a_term"
            `Quick
            collate_does_not_disqualify_a_term
        ; Alcotest.test_case
            "a_joined_arms_right_table_columns_resolve"
            `Quick
            a_joined_arms_right_table_columns_resolve
        ; Alcotest.test_case
            "a_joined_arm_with_an_alias_resolves_both_halves"
            `Quick
            a_joined_arm_with_an_alias_resolves_both_halves
        ; Alcotest.test_case
            "a_mixed_alias_and_plain_projection_still_resolves_names"
            `Quick
            a_mixed_alias_and_plain_projection_still_resolves_names
        ; Alcotest.test_case
            "a_term_matching_an_output_expression_resolves"
            `Quick
            a_term_matching_an_output_expression_resolves
        ; Alcotest.test_case
            "an_unknown_qualifier_is_accepted_divergence"
            `Quick
            an_unknown_qualifier_is_accepted_divergence
        ] )
    ; ( "anything_else_is_refused"
      , [ Alcotest.test_case
            "a_name_that_is_not_an_output_column_is_refused"
            `Quick
            a_name_that_is_not_an_output_column_is_refused
        ; Alcotest.test_case
            "an_expression_term_is_refused"
            `Quick
            an_expression_term_is_refused
        ] )
    ; ( "closed_724"
      , [ Alcotest.test_case
            "an_aggregated_arm_resolves_its_output_names_724"
            `Quick
            an_aggregated_arm_resolves_its_output_names_724
        ] )
    ]
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5"]
[@@@ai_provider "Anthropic"]
