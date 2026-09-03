(** #663: an aggregated SELECT sorts by grouped columns, or it refuses.

    {1 The defect}

    An aggregated SELECT sorts {e after} projection.
    [Planner.plan_post_agg_sort_input_space]'s [remap_e] rewrites a key's column
    index from pre-aggregation space to the aggregated output row — but it only
    rewrote an index it {e found in} [group_by], and only at the root of the
    key expression.  Two holes fell out of that, and both are silent:

    - [Sema.bind_select_order] applied {b no GROUP BY membership check} at all,
      unlike the resolver in [bind_select_having] and unlike
      [project_agg_item].  A known but non-grouped column bound to its INPUT
      index, [remap_e] left it alone, and the key read whatever OUTPUT column
      sat at that index:

      {v
        CREATE TABLE g3 (nm TEXT, pad TEXT);
        SELECT nm, COUNT( * ) FROM g3 GROUP BY nm ORDER BY pad;
      v}

      [pad] is input index 1 and [group_by] is [0], so the key stayed
      [P_col 1] — output column 1, the count.  Rows came back sorted by the
      count, with no mention of [pad] anywhere.  On a table wide enough that
      the input index exceeds the output arity, the same path indexed past the
      end of the output row and raised [Invalid_argument] from [Exec]
      mid-query rather than returning a [Db.error].

    - [remap_e] was not recursive, so an ORDER BY over an {e expression} of a
      {b grouped} column kept the pre-aggregation index {e inside} the
      expression.  That one only looks right when the grouped column sits at
      input index 0, where the two indices coincide — which is what every
      fixture in the tree happened to do.

    {1 The rule, and why the oracle is not sqlite3 here}

    {b sqlite3 accepts [ORDER BY pad] on a grouped select} — verified against
    the [sqlite3] in the dev image, which returns rows rather than an error.
    That is SQLite's permissive bare-column behaviour, and granary already
    declines to follow it: the projection and HAVING both reject a non-grouped
    column with "column '%s' must appear in GROUP BY clause".  ORDER BY was the
    one of the three that did not, and it is the one where the consequence was
    a wrong answer rather than an error.

    So this is a {b deliberate divergence from sqlite3, chosen for internal
    consistency}, exactly as CLAUDE.md prescribes for a pinned difference.  The
    tests below assert granary's rule; the two anchor cases assert that the
    projection and HAVING already had it, which is what makes it consistency
    rather than a new opinion.

    Everything that {e is} legal here is oracle-checked against sqlite3, and
    each such case quotes its output. *)

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

(* Not sorted afterwards: this file is about ORDER BY. *)
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

let rows_of db sql =
  match outcome db sql with
  | Ok rows -> rows
  | Error m -> Alcotest.failf "query %S: %s" sql m
;;

let check_rows ~label expected actual =
  Alcotest.(check (list (list string))) label expected actual
;;

let refused db sql ~label =
  match outcome db sql with
  | Error _ -> ()
  | Ok rows ->
    Alcotest.failf "%s: expected a refusal, got %d row(s)" label (List.length rows)
;;

(* The issue's schema, verbatim: [nm] is grouped, [pad] is not, and [pad] is at
   input index 1 while the output row's column 1 is the count. *)
let seed db =
  exec db "CREATE TABLE g3 (nm TEXT, pad TEXT)";
  exec db "INSERT INTO g3 VALUES ('b','z'),('a','y'),('a','x')"
;;

(* ── the anchors: the rule already exists twice ──────────────────────────── *)

(* These two are what make #663's fix "consistency" rather than a new opinion.
   Both predate it and neither is changed by this branch. *)

let the_projection_already_rejects_a_non_grouped_column () =
  with_db (fun db ->
    seed db;
    refused db "SELECT nm, pad, COUNT(*) FROM g3 GROUP BY nm" ~label:"projection")
;;

let having_already_rejects_a_non_grouped_column () =
  with_db (fun db ->
    seed db;
    refused db "SELECT nm, COUNT(*) FROM g3 GROUP BY nm HAVING pad > 'a'" ~label:"HAVING")
;;

(* ── the fix ─────────────────────────────────────────────────────────────── *)

(* The issue's own repro. Before: sorted by the count, silently. *)
let a_non_grouped_column_is_refused_not_silently_mis_sorted () =
  with_db (fun db ->
    seed db;
    refused
      db
      "SELECT nm, COUNT(*) FROM g3 GROUP BY nm ORDER BY pad"
      ~label:"ORDER BY pad")
;;

let a_qualified_non_grouped_column_is_refused_too () =
  with_db (fun db ->
    seed db;
    refused
      db
      "SELECT nm, COUNT(*) FROM g3 GROUP BY nm ORDER BY g3.pad"
      ~label:"ORDER BY g3.pad")
;;

(* The check is over EVERY column the key names, not only a bare top-level one,
   so a non-grouped column buried in an expression is refused as well. *)
let a_non_grouped_column_inside_an_expression_is_refused () =
  with_db (fun db ->
    seed db;
    refused
      db
      "SELECT nm, COUNT(*) FROM g3 GROUP BY nm ORDER BY pad || 'q'"
      ~label:"ORDER BY pad || 'q'";
    refused
      db
      "SELECT nm, COUNT(*) FROM g3 GROUP BY nm ORDER BY CASE WHEN pad > 'a' THEN 1 ELSE \
       2 END"
      ~label:"ORDER BY CASE ... pad ...")
;;

(* The wide-table shape, which was the CRASH rather than the wrong answer: the
   input index exceeds the output arity, so Exec's unchecked row.(i) raised
   Invalid_argument mid-query. A refusal is a [Db.error]; either way the point
   is that it no longer escapes as an exception from inside the executor —
   [outcome] catches both and [refused] accepts both, so what this pins is that
   the query does not ANSWER. *)
let the_wide_table_crash_is_a_refusal_now () =
  with_db (fun db ->
    exec db "CREATE TABLE w (nm TEXT, c1 TEXT, c2 TEXT, c3 TEXT, c4 TEXT)";
    exec db "INSERT INTO w VALUES ('a','1','2','3','4')";
    refused
      db
      "SELECT nm, COUNT(*) FROM w GROUP BY nm ORDER BY c4"
      ~label:"ORDER BY c4 on a 5-column table with a 2-column output")
;;

(* ── what stays legal ────────────────────────────────────────────────────── *)

(* sqlite3: a|2 / b|1 *)
let a_grouped_column_still_sorts () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"ORDER BY nm"
      [ [ "a"; "2" ]; [ "b"; "1" ] ]
      (rows_of db "SELECT nm, COUNT(*) FROM g3 GROUP BY nm ORDER BY nm"))
;;

(* sqlite3: b|1 / a|2 — the ordinal addresses the OUTPUT row (#489), so it is
   not a column reference and the guard must not see it. *)
let an_ordinal_still_sorts () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"ORDER BY 2"
      [ [ "b"; "1" ]; [ "a"; "2" ] ]
      (rows_of db "SELECT nm, COUNT(*) FROM g3 GROUP BY nm ORDER BY 2"))
;;

(* sqlite3: b|1 / a|2 — an alias likewise addresses the output row. *)
let an_output_alias_still_sorts () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"ORDER BY c, the COUNT alias"
      [ [ "b"; "1" ]; [ "a"; "2" ] ]
      (rows_of db "SELECT nm, COUNT(*) AS c FROM g3 GROUP BY nm ORDER BY c"))
;;

(* The second half of the fix, and the case that needs a table where the
   grouped column is NOT at input index 0 — otherwise pre-aggregation index and
   output index coincide and a non-recursive remap looks correct.

   Here [nm] is input index 1 and output index 0, so an unremapped [P_col 1]
   inside the concatenation would read the count instead.

   sqlite3:
     CREATE TABLE g4 (pad TEXT, nm TEXT);
     INSERT INTO g4 VALUES ('z','b'),('y','a'),('x','a');
     SELECT nm, COUNT( * ) FROM g4 GROUP BY nm ORDER BY nm || 'q' DESC;
     b|1
     a|2 *)
let an_expression_over_a_grouped_column_sorts_by_that_column () =
  with_db (fun db ->
    exec db "CREATE TABLE g4 (pad TEXT, nm TEXT)";
    exec db "INSERT INTO g4 VALUES ('z','b'),('y','a'),('x','a')";
    check_rows
      ~label:"ORDER BY nm || 'q' DESC, with nm at input index 1 and output index 0"
      [ [ "b"; "1" ]; [ "a"; "2" ] ]
      (rows_of db "SELECT nm, COUNT(*) FROM g4 GROUP BY nm ORDER BY nm || 'q' DESC"))
;;

(* A non-aggregated select is untouched: [grouped_check] is [None] there, and a
   bare column is bound and evaluated against the INPUT row as it always was.

   sqlite3: y|a / z|b — sorted by pad, which is not projected at all. *)
let a_non_aggregated_select_can_still_sort_by_an_unprojected_column () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"no GROUP BY, so no rule to apply"
      [ [ "x" ]; [ "y" ]; [ "z" ] ]
      (rows_of db "SELECT pad FROM g3 ORDER BY pad"))
;;

(* Spaces inside COUNT above are deliberate: in an OCaml comment, an open
   parenthesis immediately followed by a star opens a NESTED comment, which
   swallows the rest of this one.

   An ORDER BY that mentions an aggregate takes #495's separate path
   ([bind_select_order_agg]), which already had the GROUP BY discipline. Kept
   so this branch cannot have disturbed it.

   sqlite3: b|1 / a|2 *)
let the_aggregate_mentioning_path_is_unchanged () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"ORDER BY COUNT(*)"
      [ [ "b"; "1" ]; [ "a"; "2" ] ]
      (rows_of db "SELECT nm, COUNT(*) FROM g3 GROUP BY nm ORDER BY COUNT(*)"))
;;

(* ------------------------------------------------------------------ *)
(* Review follow-ups                                                    *)
(* ------------------------------------------------------------------ *)

(* THE blocker. Every aggregated fixture above projects the grouped column as a
   bare column ([SELECT nm, COUNT( * ) ...]), which is exactly the shape that
   hides this: being GROUPED is not enough to be remappable, the column must
   also appear in [agg_proj] as an [AP_group_col]. Group by a column and do not
   project it, and [remap_e] kept the PRE-AGGREGATION index.

   Both of #663's own symptoms came straight back through that one arm, and
   both were reachable on main too — so [Closes #663] was an overclaim until
   now:

   - [SELECT COUNT( * ) FROM g6 GROUP BY nm ORDER BY nm] raised
     [Invalid_argument("index out of bounds")] out of the executor mid-query,
     which is the issue's "indexes past the end of the output row" verbatim;
   - [SELECT UPPER(nm), COUNT( * ) ... ORDER BY nm] did not crash but sorted by
     the COUNT, which is the issue's "rows came back sorted by the count,
     silently".

   Fixed by giving an unprojected grouped column a hidden output slot, the
   mechanism [plan_agg_order_hidden] already uses for #495.

   sqlite3, on this fixture: 2/1/1 ascending, 1/1/2 descending, and
   A|2 B|1 C|1 for the [UPPER] form. *)
let a_grouped_but_unprojected_column_sorts () =
  with_db (fun db ->
    exec db "CREATE TABLE g6 (pad TEXT, nm TEXT)";
    exec db "INSERT INTO g6 VALUES ('z','b'),('y','a'),('x','a'),('w','c')";
    check_rows
      ~label:"ORDER BY a grouped column that is not projected"
      [ [ "2" ]; [ "1" ]; [ "1" ] ]
      (rows_of db "SELECT COUNT(*) FROM g6 GROUP BY nm ORDER BY nm");
    check_rows
      ~label:"and descending"
      [ [ "1" ]; [ "1" ]; [ "2" ] ]
      (rows_of db "SELECT COUNT(*) FROM g6 GROUP BY nm ORDER BY nm DESC");
    check_rows
      ~label:"and inside an expression, which needs the recursion AND the slot"
      [ [ "2" ]; [ "1" ]; [ "1" ] ]
      (rows_of db "SELECT COUNT(*) FROM g6 GROUP BY nm ORDER BY nm || 'q'");
    check_rows
      ~label:"projected as an expression, where it used to sort by the count"
      [ [ "A"; "2" ]; [ "B"; "1" ]; [ "C"; "1" ] ]
      (rows_of db "SELECT UPPER(nm), COUNT(*) FROM g6 GROUP BY nm ORDER BY nm");
    (* One slot per grouped column, not one per mention. *)
    check_rows
      ~label:"two keys naming the same unprojected column"
      [ [ "2" ]; [ "1" ]; [ "1" ] ]
      (rows_of db "SELECT COUNT(*) FROM g6 GROUP BY nm ORDER BY nm, nm || 'q'"))
;;

(* [is_aggregated] is true for an aggregate in the projection with NO GROUP BY
   at all, and [group_cols] is then empty — so every column reference failed
   the membership test and these were refused, with a message naming a grouping
   there is none of. Both answer on main and in sqlite3, so this was a second
   working-query-to-error change and it was not intended.

   Such a statement returns exactly ONE row, so its ORDER BY is a no-op
   whatever it names.

   sqlite3: 4, and c. *)
let a_bare_aggregate_with_no_group_by_still_sorts () =
  with_db (fun db ->
    exec db "CREATE TABLE g6 (pad TEXT, nm TEXT)";
    exec db "INSERT INTO g6 VALUES ('z','b'),('y','a'),('x','a'),('w','c')";
    check_rows
      ~label:"COUNT with no GROUP BY, ordered by a column"
      [ [ "4" ] ]
      (rows_of db "SELECT COUNT(*) FROM g6 ORDER BY nm");
    check_rows
      ~label:"MAX with no GROUP BY, ordered by a different column"
      [ [ "c" ] ]
      (rows_of db "SELECT MAX(nm) FROM g6 ORDER BY pad"))
;;

(* The refusal is still the refusal, and it is the one #663 decided on: a
   GROUP BY is present and the key names a column outside it. Pinned next to
   the exemption above so the boundary between them is explicit.

   sqlite3 answers this one (c|1 a|2 b|1); refusing is the recorded
   divergence, consistent with the projection and HAVING. *)
let a_non_grouped_column_is_still_refused_when_there_is_a_group_by () =
  with_db (fun db ->
    seed db;
    refused
      db
      "SELECT nm, COUNT(*) FROM g3 GROUP BY nm ORDER BY pad"
      ~label:"grouped select, non-grouped key")
;;

let () =
  Alcotest.run
    "test_agg_order_by_663"
    [ ( "the_rule_already_existed_twice"
      , [ Alcotest.test_case
            "the_projection_already_rejects_a_non_grouped_column"
            `Quick
            the_projection_already_rejects_a_non_grouped_column
        ; Alcotest.test_case
            "having_already_rejects_a_non_grouped_column"
            `Quick
            having_already_rejects_a_non_grouped_column
        ] )
    ; ( "order_by_now_has_it_too"
      , [ Alcotest.test_case
            "a_non_grouped_column_is_refused_not_silently_mis_sorted"
            `Quick
            a_non_grouped_column_is_refused_not_silently_mis_sorted
        ; Alcotest.test_case
            "a_qualified_non_grouped_column_is_refused_too"
            `Quick
            a_qualified_non_grouped_column_is_refused_too
        ; Alcotest.test_case
            "a_non_grouped_column_inside_an_expression_is_refused"
            `Quick
            a_non_grouped_column_inside_an_expression_is_refused
        ; Alcotest.test_case
            "the_wide_table_crash_is_a_refusal_now"
            `Quick
            the_wide_table_crash_is_a_refusal_now
        ] )
    ; ( "review_follow_ups"
      , [ Alcotest.test_case
            "a_grouped_but_unprojected_column_sorts"
            `Quick
            a_grouped_but_unprojected_column_sorts
        ; Alcotest.test_case
            "a_bare_aggregate_with_no_group_by_still_sorts"
            `Quick
            a_bare_aggregate_with_no_group_by_still_sorts
        ; Alcotest.test_case
            "a_non_grouped_column_is_still_refused_when_there_is_a_group_by"
            `Quick
            a_non_grouped_column_is_still_refused_when_there_is_a_group_by
        ] )
    ; ( "what_stays_legal"
      , [ Alcotest.test_case
            "a_grouped_column_still_sorts"
            `Quick
            a_grouped_column_still_sorts
        ; Alcotest.test_case "an_ordinal_still_sorts" `Quick an_ordinal_still_sorts
        ; Alcotest.test_case
            "an_output_alias_still_sorts"
            `Quick
            an_output_alias_still_sorts
        ; Alcotest.test_case
            "an_expression_over_a_grouped_column_sorts_by_that_column"
            `Quick
            an_expression_over_a_grouped_column_sorts_by_that_column
        ; Alcotest.test_case
            "a_non_aggregated_select_can_still_sort_by_an_unprojected_column"
            `Quick
            a_non_aggregated_select_can_still_sort_by_an_unprojected_column
        ; Alcotest.test_case
            "the_aggregate_mentioning_path_is_unchanged"
            `Quick
            the_aggregate_mentioning_path_is_unchanged
        ] )
    ]
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5"]
[@@@ai_provider "Anthropic"]
