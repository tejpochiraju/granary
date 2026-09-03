(** #722: COLLATE is a comparison attribute, not a value transformation.

    {1 The defect}

    [Exec.eval_expr]'s [P_collate] arm implemented [COLLATE NOCASE] as
    [String.lowercase_ascii] {e on the value}:

    {v
      | Plan.P_collate (e, Ast.Collate_nocase) ->
        (match eval_expr clock params row e with
         | Row.V_text s -> Row.V_text (String.lowercase_ascii s)
         | o -> o)
    v}

    For an equality test the two models are indistinguishable, which is why it
    held up.  In a {e projection} they are not: [SELECT x COLLATE NOCASE FROM t]
    returned ['hello'] where sqlite3 returns ['HELLO'].  The same leak reached
    every other value-consuming position — [CAST], [||], a function argument,
    [MIN]/[MAX]'s result, and the rows [SELECT DISTINCT] emits.

    {1 The model}

    A COLLATE now carries no value semantics at all.  [Exec.expr_collation]
    reads the collation off an operand {e expression}, and each comparison site
    keys both sides through [Exec.collate_key] before comparing.  The rule for
    what an operand's collation {e is} follows sqlite3: an operand has an
    explicit collating-function assignment if {b any} subexpression of it uses
    the postfix COLLATE operator, leftmost wins.

    Two consequences beyond the headline, both oracle-checked below: the
    collation now propagates through a subexpression (so
    [(x COLLATE NOCASE) || '!' = 'HELLO!'] matches, where before it matched
    nothing), and it no longer folds through a NON-comparison operator (so
    [(x COLLATE NOCASE) || 'B'] is ['HELLOB'], not ['hellob']).

    {1 The index question}

    A collated comparison {b cannot reach an index seek}, by construction, and
    this fix deliberately did not change that.  Every seek decision is made on
    [Sema.bound_expr] before [plan_expr] runs, and both recognisers
    ([Planner.recognise_eq_col_lit], [Planner.recognise_range_col_lit]) pattern-
    match [Sema.BE_col] / [Sema.BE_lit] / [Sema.BE_param] directly with no
    wrapper stripping, so a [BE_collate] falls to their [| _ -> None] arm.
    Declining the index is the safe answer here and it is the one already in
    force: [Index_key.encode_value] emits BINARY bytes, so a NOCASE seek over
    them would skip rows — the rows-lost failure mode CLAUDE.md's NaN section
    describes.  It would also be unrecoverable, because [Planner.plan_base]
    {e drops} the conjunct a seek consumed, deleting the residual predicate that
    would otherwise re-check the collation.  [collated_comparison_declines_the_index]
    pins the plan shape and the rows together.

    There is also no DDL spelling of a collation in this engine — [COLLATE]
    appears in the grammar only as a postfix expression operator — so no column
    and no index carries an implicit non-binary collation for a seek to
    mis-encode.

    {1 Oracle}

    Every expected value in this file was taken from the [sqlite3] in the dev
    image on the same schema and data. *)

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

(* Unsorted: several cases below are about the VALUES a row carries and one is
   about ORDER BY's own ordering, so the caller says when to sort. *)
let raw_rows_of db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun r -> Array.to_list (Array.map render r))
      (run (Lwt_stream.to_list stream))
;;

let rows_of db sql = List.sort compare (raw_rows_of db sql)

let check_rows ~label expected actual =
  Alcotest.(check (list (list string))) label (List.sort compare expected) actual
;;

let check_ordered ~label expected actual =
  Alcotest.(check (list (list string))) label expected actual
;;

(* 'HELLO' and 'Hello' differ only in case, so every assertion below tells a
   NOCASE comparison from a BINARY one, and the two spellings tell a value that
   was rewritten from one that was not. *)
let seed db =
  exec db "CREATE TABLE o (k TEXT, x TEXT)";
  exec db "INSERT INTO o VALUES ('a','HELLO'),('b','world'),('c','zzz'),('d','Hello')"
;;

(* ------------------------------------------------------------------ *)
(* The headline: a COLLATE never rewrites a value                       *)
(* ------------------------------------------------------------------ *)

(* sqlite3:
     SELECT k, x COLLATE NOCASE FROM o;
     a|HELLO  b|world  c|zzz  d|Hello                                    *)
let a_projected_collate_returns_the_stored_value () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"the issue's own query"
      [ [ "a"; "HELLO" ]; [ "b"; "world" ]; [ "c"; "zzz" ]; [ "d"; "Hello" ] ]
      (rows_of db "SELECT k, x COLLATE NOCASE FROM o"))
;;

(* The other value-consuming positions the fold leaked into.  sqlite3:
     SELECT CAST(x COLLATE NOCASE AS TEXT) FROM o;   -> HELLO world zzz Hello
     SELECT (x COLLATE NOCASE) || '!' FROM o;        -> HELLO! world! zzz! Hello!
     SELECT UPPER(x COLLATE NOCASE) FROM o;          -> HELLO WORLD ZZZ HELLO   *)
let cast_concat_and_function_arguments_see_the_stored_value () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"CAST"
      [ [ "HELLO" ]; [ "world" ]; [ "zzz" ]; [ "Hello" ] ]
      (rows_of db "SELECT CAST(x COLLATE NOCASE AS TEXT) FROM o");
    check_rows
      ~label:"|| does not fold either operand"
      [ [ "HELLO!" ]; [ "world!" ]; [ "zzz!" ]; [ "Hello!" ] ]
      (rows_of db "SELECT (x COLLATE NOCASE) || '!' FROM o");
    check_rows
      ~label:"function argument"
      [ [ "HELLO" ]; [ "WORLD" ]; [ "ZZZ" ]; [ "HELLO" ] ]
      (rows_of db "SELECT UPPER(x COLLATE NOCASE) FROM o"))
;;

(* Before #722 the fold ran for EVERY binop, not only comparisons, so the
   literal operand of a concatenation was lower-cased too.

   sqlite3: SELECT (x COLLATE NOCASE) || 'B' FROM o WHERE k='a';  -> HELLOB   *)
let a_non_comparison_operator_does_not_fold_its_other_operand () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"the 'B' stays upper-case"
      [ [ "HELLOB" ] ]
      (rows_of db "SELECT (x COLLATE NOCASE) || 'B' FROM o WHERE k = 'a'"))
;;

(* ------------------------------------------------------------------ *)
(* ... while every comparison still honours it                          *)
(* ------------------------------------------------------------------ *)

(* Each of these answers BOTH 'a' and 'd' under NOCASE and only 'a' (or
   nothing) under BINARY, so none of them can pass on a build that dropped the
   collation.  sqlite3 answers a,d for all five. *)
let every_comparison_form_still_honours_the_collation () =
  with_db (fun db ->
    seed db;
    let both = [ [ "a" ]; [ "d" ] ] in
    check_rows
      ~label:"="
      both
      (rows_of db "SELECT k FROM o WHERE x COLLATE NOCASE = 'HELLO'");
    check_rows
      ~label:"= with the COLLATE on the literal side"
      both
      (rows_of db "SELECT k FROM o WHERE x = 'HELLO' COLLATE NOCASE");
    check_rows
      ~label:"IN over a value list"
      both
      (rows_of db "SELECT k FROM o WHERE x COLLATE NOCASE IN ('hello')");
    check_rows
      ~label:"IN over a subquery"
      both
      (rows_of db "SELECT k FROM o WHERE x COLLATE NOCASE IN (SELECT 'hello')");
    (* A join's ON predicate: [Planner.recognise_eq_col_col] does not match a
       [BE_collate] either, so this cannot become a hash-join key and falls to
       [Op_hash_join.on_pred], which is evaluated through the [P_binop] arm. *)
    exec db "CREATE TABLE j (v TEXT)";
    exec db "INSERT INTO j VALUES ('hello')";
    check_rows
      ~label:"a join's ON predicate"
      both
      (rows_of db "SELECT o.k FROM o JOIN j ON o.x COLLATE NOCASE = j.v"))
;;

(* BETWEEN reached [eval_binop] DIRECTLY, bypassing the [P_binop] arm's
   propagation, so it folded [x] but neither bound and answered NOTHING.  This
   is a second bug the comparison-attribute model fixes rather than preserves.

   sqlite3:
     SELECT k FROM o WHERE x COLLATE NOCASE BETWEEN 'HELLA' AND 'HELLZ';
     a
     d                                                                  *)
let between_now_keys_both_bounds () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"was empty before #722"
      [ [ "a" ]; [ "d" ] ]
      (rows_of db "SELECT k FROM o WHERE x COLLATE NOCASE BETWEEN 'HELLA' AND 'HELLZ'"))
;;

(* sqlite3's rule is that an operand carries an explicit collation if ANY
   subexpression uses postfix COLLATE.  The old top-level-only [is_nocase] test
   missed this and the query answered nothing.

   sqlite3:
     SELECT k FROM o WHERE (x COLLATE NOCASE) || '!' = 'HELLO!';
     a
     d                                                                  *)
let the_collation_propagates_out_of_a_subexpression () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"COLLATE two nodes below the comparison"
      [ [ "a" ]; [ "d" ] ]
      (rows_of db "SELECT k FROM o WHERE (x COLLATE NOCASE) || '!' = 'HELLO!'"))
;;

(* sqlite3:
     SELECT CASE x COLLATE NOCASE WHEN 'hello' THEN 'Y' ELSE 'N' END FROM o;
     Y N N Y   (row order a,b,c,d)                                      *)
let a_case_scrutinee_takes_its_collation () =
  with_db (fun db ->
    seed db;
    check_ordered
      ~label:"CASE x WHEN is an equality"
      [ [ "Y" ]; [ "N" ]; [ "N" ]; [ "Y" ] ]
      (raw_rows_of
         db
         "SELECT CASE x COLLATE NOCASE WHEN 'hello' THEN 'Y' ELSE 'N' END FROM o"))
;;

(* ORDER BY was already correct before #722 — but only because the sort key was
   evaluated through the folding [P_collate] arm.  Removing the fold would have
   broken it, so this is the case that proves [Exec.eval_sort_key] replaced it.

   sqlite3:
     SELECT k, x FROM o ORDER BY x COLLATE NOCASE;
     a|HELLO  d|Hello  b|world  c|zzz                                    *)
let order_by_still_sorts_case_insensitively_and_emits_stored_values () =
  with_db (fun db ->
    seed db;
    check_ordered
      ~label:"NOCASE order, BINARY values"
      [ [ "a"; "HELLO" ]; [ "d"; "Hello" ]; [ "b"; "world" ]; [ "c"; "zzz" ] ]
      (raw_rows_of db "SELECT k, x FROM o ORDER BY x COLLATE NOCASE"))
;;

(* The control for the case above: without the COLLATE, 'HELLO' and 'Hello'
   sort before 'world' by BINARY too, so the discriminating pair is 'Hello' vs
   'zzz' against a lower-case 'aaa'.  Upper-case letters sort BEFORE lower-case
   ones in BINARY, so a build that ignored the collation would put both
   capitalised rows first.

   sqlite3:
     SELECT k FROM o2 ORDER BY x COLLATE NOCASE;   -> p q      (aaa, BBB)
     SELECT k FROM o2 ORDER BY x;                  -> q p      (BBB, aaa)   *)
let the_order_by_collation_is_load_bearing () =
  with_db (fun db ->
    exec db "CREATE TABLE o2 (k TEXT, x TEXT)";
    exec db "INSERT INTO o2 VALUES ('p','aaa'),('q','BBB')";
    check_ordered
      ~label:"NOCASE: aaa before BBB"
      [ [ "p" ]; [ "q" ] ]
      (raw_rows_of db "SELECT k FROM o2 ORDER BY x COLLATE NOCASE");
    check_ordered
      ~label:"BINARY: BBB before aaa"
      [ [ "q" ]; [ "p" ] ]
      (raw_rows_of db "SELECT k FROM o2 ORDER BY x"))
;;

(* ------------------------------------------------------------------ *)
(* Dedup: DISTINCT and the set operations                               *)
(* ------------------------------------------------------------------ *)

(* The site with no expressions of its own.  [Op_distinct] is [{ child : op }],
   so before #722 it deduped case-insensitively only as a side effect of the
   projection having lower-cased the value — and it emitted that lower-cased
   value.  [Exec.output_collations] reads the collation back off the projection
   underneath, so the dedup is unchanged and the value is now the stored one.

   sqlite3:
     SELECT DISTINCT x COLLATE NOCASE FROM o;   -> HELLO world zzz
     SELECT DISTINCT x FROM o;                  -> HELLO world zzz Hello   *)
let distinct_dedups_under_the_collation_and_emits_the_stored_value () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"three rows, first-seen representative, unmodified"
      [ [ "HELLO" ]; [ "world" ]; [ "zzz" ] ]
      (rows_of db "SELECT DISTINCT x COLLATE NOCASE FROM o");
    check_rows
      ~label:"the control: BINARY keeps all four"
      [ [ "HELLO" ]; [ "world" ]; [ "zzz" ]; [ "Hello" ] ]
      (rows_of db "SELECT DISTINCT x FROM o"))
;;

(* The same question one level up, in the three set operations.

   Only the ROW COUNT is asserted for INTERSECT, and deliberately: sqlite3
   sorts a compound's output and therefore reports 'Hello' as the surviving
   representative of the NOCASE-equal pair, while granary streams the left arm
   and reports the first it saw, 'HELLO'.  Both are a left-arm row that
   matched; which one survives is unspecified and granary's answer here is
   pre-existing and unrelated to #722.

   sqlite3:
     SELECT x COLLATE NOCASE FROM o UNION SELECT x COLLATE NOCASE FROM o;
       -> 3 rows (Hello, world, zzz)
     SELECT x COLLATE NOCASE FROM o INTERSECT SELECT 'hello';
       -> 1 row  (Hello)
     SELECT x COLLATE NOCASE FROM o EXCEPT SELECT 'hello';
       -> world, zzz                                                     *)
let the_set_operations_dedup_under_the_collation () =
  with_db (fun db ->
    seed db;
    Alcotest.(check int)
      "UNION folds HELLO and Hello into one row"
      3
      (List.length (rows_of db "SELECT x COLLATE NOCASE FROM o UNION SELECT x FROM o"));
    Alcotest.(check int)
      "the control: UNION without the collation keeps four"
      4
      (List.length (rows_of db "SELECT x FROM o UNION SELECT x FROM o"));
    Alcotest.(check int)
      "INTERSECT matches the NOCASE-equal pair as one row"
      1
      (List.length (rows_of db "SELECT x COLLATE NOCASE FROM o INTERSECT SELECT 'hello'"));
    check_rows
      ~label:"EXCEPT removes BOTH spellings"
      [ [ "world" ]; [ "zzz" ] ]
      (rows_of db "SELECT x COLLATE NOCASE FROM o EXCEPT SELECT 'hello'"))
;;

(* ------------------------------------------------------------------ *)
(* Aggregates and windows                                               *)
(* ------------------------------------------------------------------ *)

(* MIN/MAX compare under the argument's collation and return the RAW winner;
   COUNT(DISTINCT ...) dedups under it.  Before #722 the count was right by
   accident and MIN's VALUE was lower-cased.

   sqlite3:
     SELECT MIN(x COLLATE NOCASE), MAX(x COLLATE NOCASE) FROM o;  -> HELLO|zzz
     SELECT COUNT(DISTINCT x COLLATE NOCASE) FROM o;              -> 3
     SELECT COUNT(DISTINCT x) FROM o;                             -> 4        *)
let aggregates_compare_collated_and_return_raw_values () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"MIN/MAX return the stored spelling"
      [ [ "HELLO"; "zzz" ] ]
      (rows_of db "SELECT MIN(x COLLATE NOCASE), MAX(x COLLATE NOCASE) FROM o");
    check_rows
      ~label:"COUNT(DISTINCT ...) folds the pair"
      [ [ "3" ] ]
      (rows_of db "SELECT COUNT(DISTINCT x COLLATE NOCASE) FROM o");
    check_rows
      ~label:"the control: BINARY counts four"
      [ [ "4" ] ]
      (rows_of db "SELECT COUNT(DISTINCT x) FROM o"))
;;

(* A window PARTITION BY key is a comparison key too.  sqlite3:
     SELECT k, COUNT( * ) OVER (PARTITION BY x COLLATE NOCASE) FROM o;
     a|2  b|1  c|1  d|2
   and without the collation every partition is a singleton.                *)
let a_window_partition_key_takes_its_collation () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"HELLO and Hello are one partition"
      [ [ "a"; "2" ]; [ "b"; "1" ]; [ "c"; "1" ]; [ "d"; "2" ] ]
      (rows_of db "SELECT k, COUNT(*) OVER (PARTITION BY x COLLATE NOCASE) FROM o");
    check_rows
      ~label:"the control: BINARY makes four partitions"
      [ [ "a"; "1" ]; [ "b"; "1" ]; [ "c"; "1" ]; [ "d"; "1" ] ]
      (rows_of db "SELECT k, COUNT(*) OVER (PARTITION BY x) FROM o"))
;;

(* ------------------------------------------------------------------ *)
(* The index boundary                                                   *)
(* ------------------------------------------------------------------ *)

(* The dangerous interaction, pinned as a NON-interaction.  [Index_key] encodes
   BINARY bytes, so a NOCASE seek over an index would skip exactly the rows the
   collation is there to find.  The planner declines the index for a collated
   comparison — not by a new rule, but because both seek recognisers match
   [Sema.BE_col] directly and a [BE_collate] is a different constructor — and
   [Planner.plan_base] drops a consumed conjunct, so there would be no residual
   predicate left to recover the skipped rows.

   Asserted two ways so neither half can rot silently: the EXPLAIN shape shows
   the seek was declined, and the rows show the answer is complete.  The
   uncollated control shows the index IS used for the same column, so the first
   assertion is about the collation and not about the index being unusable.

   sqlite3 answers both HELLO and hello for the collated query.               *)
let collated_comparison_declines_the_index () =
  with_db (fun db ->
    exec db "CREATE TABLE t (x TEXT)";
    exec db "CREATE INDEX ix ON t(x)";
    exec db "INSERT INTO t VALUES ('HELLO'),('hello'),('world')";
    let plan sql = List.concat (raw_rows_of db ("EXPLAIN " ^ sql)) in
    let mentions needle sql =
      List.exists
        (fun cell ->
           let n = String.length needle in
           String.length cell >= n && String.sub cell 0 n = needle)
        (plan sql)
    in
    Alcotest.(check bool)
      "the control: a plain equality DOES seek the index"
      true
      (mentions "IndexLookup" "SELECT x FROM t WHERE x = 'HELLO'");
    Alcotest.(check bool)
      "a collated equality does not"
      false
      (mentions "IndexLookup" "SELECT x FROM t WHERE x COLLATE NOCASE = 'HELLO'");
    Alcotest.(check bool)
      "nor does one with the COLLATE on the literal"
      false
      (mentions "IndexLookup" "SELECT x FROM t WHERE x = 'HELLO' COLLATE NOCASE");
    check_rows
      ~label:"and no row is lost"
      [ [ "HELLO" ]; [ "hello" ] ]
      (rows_of db "SELECT x FROM t WHERE x COLLATE NOCASE = 'HELLO'");
    check_rows
      ~label:"nor by a collated RANGE, which the range recogniser declines too"
      [ [ "HELLO" ]; [ "hello" ] ]
      (rows_of db "SELECT x FROM t WHERE x COLLATE NOCASE BETWEEN 'HELLA' AND 'HELLZ'"))
;;

(* ------------------------------------------------------------------ *)
(* Not covered, on purpose                                              *)
(* ------------------------------------------------------------------ *)

(* [GROUP BY x COLLATE NOCASE] is a PARSE ERROR in this engine and #722 did not
   change that: [Ast.group_by_item] is [string * string option], a name and an
   optional qualifier, not an expression — so a GROUP BY key cannot carry a
   collation for any site to consult.  sqlite3 accepts it and groups
   case-insensitively.

   This is recorded as a test rather than left implicit because the grouping
   comparator ([Exec.aggregate_build_groups], on [group_cols : int list]) is the
   one comparison site #722 could NOT make collation-aware, and the reason is a
   grammar limitation rather than a decision about collation.  Anything that
   widens [group_by_item] to an expression owes that comparator the same
   treatment the sort keys got. *)
let group_by_collate_is_still_a_parse_error () =
  with_db (fun db ->
    seed db;
    match run (Db.query db "SELECT COUNT(*) FROM o GROUP BY x COLLATE NOCASE") with
    | Error _ -> ()
    | Ok _ ->
      Alcotest.fail
        "GROUP BY ... COLLATE now parses — give Exec.aggregate_build_groups the \
         collation before deleting this case")
;;

let () =
  Alcotest.run
    "test_collate_722"
    [ ( "a_collate_never_rewrites_a_value"
      , [ Alcotest.test_case
            "a_projected_collate_returns_the_stored_value"
            `Quick
            a_projected_collate_returns_the_stored_value
        ; Alcotest.test_case
            "cast_concat_and_function_arguments_see_the_stored_value"
            `Quick
            cast_concat_and_function_arguments_see_the_stored_value
        ; Alcotest.test_case
            "a_non_comparison_operator_does_not_fold_its_other_operand"
            `Quick
            a_non_comparison_operator_does_not_fold_its_other_operand
        ] )
    ; ( "every_comparison_still_honours_it"
      , [ Alcotest.test_case
            "every_comparison_form_still_honours_the_collation"
            `Quick
            every_comparison_form_still_honours_the_collation
        ; Alcotest.test_case
            "between_now_keys_both_bounds"
            `Quick
            between_now_keys_both_bounds
        ; Alcotest.test_case
            "the_collation_propagates_out_of_a_subexpression"
            `Quick
            the_collation_propagates_out_of_a_subexpression
        ; Alcotest.test_case
            "a_case_scrutinee_takes_its_collation"
            `Quick
            a_case_scrutinee_takes_its_collation
        ; Alcotest.test_case
            "order_by_still_sorts_case_insensitively_and_emits_stored_values"
            `Quick
            order_by_still_sorts_case_insensitively_and_emits_stored_values
        ; Alcotest.test_case
            "the_order_by_collation_is_load_bearing"
            `Quick
            the_order_by_collation_is_load_bearing
        ] )
    ; ( "dedup_sites"
      , [ Alcotest.test_case
            "distinct_dedups_under_the_collation_and_emits_the_stored_value"
            `Quick
            distinct_dedups_under_the_collation_and_emits_the_stored_value
        ; Alcotest.test_case
            "the_set_operations_dedup_under_the_collation"
            `Quick
            the_set_operations_dedup_under_the_collation
        ] )
    ; ( "aggregates_and_windows"
      , [ Alcotest.test_case
            "aggregates_compare_collated_and_return_raw_values"
            `Quick
            aggregates_compare_collated_and_return_raw_values
        ; Alcotest.test_case
            "a_window_partition_key_takes_its_collation"
            `Quick
            a_window_partition_key_takes_its_collation
        ] )
    ; ( "the_index_boundary"
      , [ Alcotest.test_case
            "collated_comparison_declines_the_index"
            `Quick
            collated_comparison_declines_the_index
        ] )
    ; ( "out_of_scope"
      , [ Alcotest.test_case
            "group_by_collate_is_still_a_parse_error"
            `Quick
            group_by_collate_is_still_a_parse_error
        ] )
    ]
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5"]
[@@@ai_provider "Anthropic"]
