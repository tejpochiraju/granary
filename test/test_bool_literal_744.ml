(** #744: [TRUE] and [FALSE] as boolean literals.

    They did not exist anywhere in the grammar, so both fell through to the
    identifier rule and were bound as column references —
    [SELECT TRUE] answered ["unknown column: __const__.TRUE"]. The sharp
    consequence was that the {b only} spelling sqlite3 accepts for an
    [INSERT ... SELECT] upsert was unrunnable here: sqlite3 needs the
    disambiguating [WHERE true] (the bare form is ambiguous with a join's [ON]
    and is a parse error there), granary accepted only the bare form, and the
    intersection of the two accepted sets was empty.

    {1 What was decided}

    - They are aliases for the integers 1 and 0, {b not} a boolean storage
      class. Oracle-checked: [SELECT TRUE + TRUE] is [2] and
      [typeof(TRUE)] is [integer]. That is what keeps them out of
      [Exec.value_class_rank] and away from the #579/#733/#738 comparator work
      entirely — no comparator learns a new class.
    - They are {b not} keywords, and nothing was added to [lexer.mll]. The rule
      is a name-resolution fallback in {!Granary_sql.Ast.bool_ident_lit},
      consulted only after a column lookup has already failed — which is
      exactly how SQLite resolves them ([sqlite3ExprIdToTrueFalse], reached
      from [lookupName] when the count of matching columns is zero). Two things
      follow: identifier context wins (oracle-checked — [SELECT true FROM t]
      where [t] has a column named ["true"] answers the column, both engines),
      and the #619 lexer/[Sql_ident] drift guard is untouched, so no stored SQL
      starts needing quoting it did not need before (#572).

    {1 What is deliberately NOT here}

    [IS TRUE] / [IS NOT FALSE] are out of scope. They are separate grammar
    productions in SQLite with {i truthiness} semantics, not equality —
    oracle-checked, [2 IS TRUE] is [1] while [2 IS 1] is [0], and
    ['1' IS TRUE] is [1] while ['1' IS 1] is [0]. Granary has no general
    [x IS y] operator at all: [IS] appears only in the [IS NULL] /
    [IS NOT NULL] productions, so [1 IS TRUE] was a {i parse} error before this
    change and still is. That is the important half — the fallback could not
    turn [IS TRUE] into a silently wrong [IS 1], and the case below pins that
    it did not. Adding the productions is a separate, orthogonal gap.

    {1 Recorded divergences}

    - A QUOTED true — double-quoted, backticked or bracketed — is the literal
      here, where sqlite3 keeps quoting semantically significant: it answers
      the text value [true] for the double-quoted spelling, through its
      double-quoted-string misfeature, and an error for the other two. Granary
      implements no such misfeature and already treats all four spellings of a
      non-keyword name as one identifier everywhere else, so it does here
      too.
    - [GROUP BY true] is still refused. A constant GROUP BY key is not
      expressible in granary's AST at all (the clause carries names, not
      expressions — [GROUP BY 1] is a parse error too), so this is that
      orthogonal gap rather than a boolean one. It fails loudly. *)

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

let show_value = function
  | Row.V_text s -> s
  | Row.V_null -> "<null>"
  | Row.V_int n -> Int64.to_string n
  | Row.V_real f -> string_of_float f
  | Row.V_blob _ -> "<blob>"
;;

(* Every row of [sql] rendered as "c1|c2|…", in order. *)
let rows db sql =
  List.map
    (fun (r : Row.t) -> Array.to_list r |> List.map show_value |> String.concat "|")
    (query db sql)
;;

let check_rows db sql want =
  Alcotest.(check (list string)) (Printf.sprintf "%S" sql) want (rows db sql)
;;

(* [Db.execute] refuses a read outright, so a refused SELECT is asked for
   through [Db.query] and a refused write through [Db.execute]. *)
let expect_query_error db sql =
  match run (Db.query db sql) with
  | Error _ -> ()
  | Ok _ -> Alcotest.failf "%S was expected to be refused" sql
;;

let expect_exec_error db sql =
  match run (Db.execute db sql) with
  | Error _ -> ()
  | Ok () -> Alcotest.failf "%S was expected to be refused" sql
;;

(* ------------------------------------------------------------------ *)
(* The literal itself                                                   *)
(* ------------------------------------------------------------------ *)

(* Values, arithmetic and typeof together, because "alias for 1 and 0" is the
   whole decision: any of these answering something else means a storage class
   crept in.  Every expected value here was read off sqlite3, not predicted. *)
let literals_are_one_and_zero () =
  with_db (fun db ->
    check_rows db "SELECT TRUE" [ "1" ];
    check_rows db "SELECT FALSE" [ "0" ];
    check_rows db "SELECT TRUE + TRUE" [ "2" ];
    check_rows db "SELECT typeof(TRUE), typeof(FALSE)" [ "integer|integer" ];
    check_rows db "SELECT TRUE AND FALSE" [ "0" ];
    check_rows db "SELECT NOT TRUE" [ "0" ];
    check_rows db "SELECT TRUE IS NULL, FALSE IS NOT NULL" [ "0|1" ])
;;

(* The lexer folds keywords with [uppercase_ascii]; the fallback folds with
   [lowercase_ascii].  Same insensitivity either way, and it is asserted rather
   than assumed because the two functions are in different modules. *)
let spelling_is_case_insensitive () =
  with_db (fun db ->
    check_rows db "SELECT true, TRUE, True, tRuE" [ "1|1|1|1" ];
    check_rows db "SELECT false, FALSE, False, fAlSe" [ "0|0|0|0" ])
;;

(* ------------------------------------------------------------------ *)
(* The headline: the portable INSERT ... SELECT upsert                  *)
(* ------------------------------------------------------------------ *)

(* sqlite3 REQUIRES the [WHERE true] here — the bare form is ambiguous with a
   join's [ON] and is a parse error there — so this is the only spelling of an
   [INSERT ... SELECT] upsert that both engines can run.  1010 is sqlite3's own
   answer to the identical script. *)
let insert_select_upsert_with_where_true () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    exec db "INSERT INTO src VALUES (1, 10)";
    exec
      db
      "INSERT INTO t SELECT k, v FROM src WHERE true ON CONFLICT(k) DO UPDATE SET v = \
       excluded.v + 1000";
    check_rows db "SELECT k, v FROM t" [ "1|1010" ])
;;

(* granary's acceptance of the bare form is a deliberate SUPERSET of sqlite3 —
   its grammar has no ambiguity to disambiguate — and #744 widens the accepted
   set rather than replacing it.  Pinned so a later "match sqlite3 exactly"
   change has to be a decision. *)
let the_bare_form_granary_already_accepted_still_works () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    exec db "INSERT INTO src VALUES (1, 10)";
    exec
      db
      "INSERT INTO t SELECT k, v FROM src ON CONFLICT(k) DO UPDATE SET v = excluded.v + \
       1000";
    check_rows db "SELECT k, v FROM t" [ "1|1010" ])
;;

(* ------------------------------------------------------------------ *)
(* Identifier context wins                                              *)
(* ------------------------------------------------------------------ *)

(* The reason this is a resolution fallback and not a lexer keyword.  All four
   answers were read off sqlite3 first: the column, in every position. *)
let a_column_named_true_still_wins () =
  with_db (fun db ->
    exec db "CREATE TABLE t (\"true\" INTEGER, \"false\" INTEGER)";
    exec db "INSERT INTO t VALUES (7, 9)";
    check_rows db "SELECT true FROM t" [ "7" ];
    check_rows db "SELECT \"true\" FROM t" [ "7" ];
    check_rows db "SELECT t.true FROM t" [ "7" ];
    check_rows db "SELECT false FROM t" [ "9" ];
    (* and in a predicate, not only a projection *)
    check_rows db "SELECT \"false\" FROM t WHERE true = 7" [ "9" ])
;;

(* A stored CHECK is re-parsed and re-compiled on every write by [Exec], which
   is a different resolver from [Sema]'s.  Both have to agree that the column
   wins, or a table with a column named [true] becomes un-insertable — #572's
   failure mode reached through #744. *)
let a_stored_check_naming_the_column_still_resolves_to_it () =
  with_db (fun db ->
    exec db "CREATE TABLE ct (\"true\" INTEGER CHECK (\"true\" > 0), b INTEGER)";
    exec db "INSERT INTO ct VALUES (5, 1)";
    check_rows db "SELECT \"true\", b FROM ct" [ "5|1" ];
    (* the CHECK is live, not merely parseable *)
    expect_exec_error db "INSERT INTO ct VALUES (0, 1)")
;;

(* Not reserved anywhere a name may be written. *)
let true_and_false_are_still_usable_as_names () =
  with_db (fun db ->
    exec db "CREATE TABLE \"false\" (x INTEGER)";
    exec db "INSERT INTO \"false\" VALUES (5)";
    check_rows db "SELECT x FROM \"false\"" [ "5" ];
    exec db "CREATE TABLE u (true INTEGER, false INTEGER)";
    exec db "INSERT INTO u VALUES (3, 4)";
    check_rows db "SELECT true, false FROM u" [ "3|4" ];
    check_rows db "SELECT 1 AS true, 2 AS false" [ "1|2" ])
;;

(* ------------------------------------------------------------------ *)
(* Every clause a bare name can sit in                                  *)
(* ------------------------------------------------------------------ *)

let seed db =
  exec db "CREATE TABLE q (a INTEGER)";
  exec db "INSERT INTO q VALUES (1)";
  exec db "INSERT INTO q VALUES (2)";
  exec db "INSERT INTO q VALUES (3)"
;;

(* [WHERE true] is the shape the issue is about, so it is checked in the
   filter, the projection, the sort, HAVING and a join's ON. *)
let clauses_that_take_an_expression () =
  with_db (fun db ->
    seed db;
    check_rows db "SELECT a FROM q WHERE TRUE" [ "1"; "2"; "3" ];
    check_rows db "SELECT a FROM q WHERE FALSE" [];
    check_rows db "SELECT a FROM q WHERE NOT false" [ "1"; "2"; "3" ];
    check_rows db "SELECT a FROM q WHERE true AND a > 2" [ "3" ];
    check_rows db "SELECT count(*) FROM q WHERE true" [ "3" ];
    check_rows db "SELECT a FROM q ORDER BY true" [ "1"; "2"; "3" ];
    check_rows
      db
      "SELECT a, count(*) FROM q GROUP BY a HAVING true"
      [ "1|1"; "2|1"; "3|1" ];
    check_rows
      db
      "SELECT a, count(*) FROM q GROUP BY a ORDER BY true"
      [ "1|1"; "2|1"; "3|1" ];
    check_rows db "SELECT CASE WHEN true THEN 'y' ELSE 'n' END" [ "y" ];
    exec db "CREATE TABLE r2 (b INTEGER)";
    exec db "INSERT INTO r2 VALUES (9)";
    check_rows db "SELECT a, b FROM q JOIN r2 ON true WHERE a = 2" [ "2|9" ])
;;

(* The parser emits the [`Cols] projection shape — a plain [string list], which
   cannot hold a literal — whenever EVERY select item is a bare name.  So
   [SELECT true FROM q] took a path no expression ever reaches; the projection
   is promoted to [`Exprs] when a name is one the fallback answers, the same
   promotion #732 already makes in [Exec.substitute_outer_proj]. *)
let a_bare_boolean_projection () =
  with_db (fun db ->
    seed db;
    check_rows db "SELECT true FROM q" [ "1"; "1"; "1" ];
    check_rows db "SELECT true, false FROM q WHERE a = 1" [ "1|0" ];
    check_rows db "SELECT a, true FROM q WHERE a = 2" [ "2|1" ];
    check_rows db "SELECT DISTINCT true FROM q" [ "1" ];
    check_rows
      db
      "SELECT true FROM q WHERE a = 1 UNION ALL SELECT false FROM q WHERE a = 1"
      [ "1"; "0" ])
;;

(* A VALUES list has no row in scope, so [true] is the literal there even on a
   table that HAS a column of that name — oracle-checked:
   [INSERT INTO u VALUES (true, 2)] on [u("true", b)] stores [1|2], not [7|2]. *)
let insert_values_and_defaults () =
  with_db (fun db ->
    exec db "CREATE TABLE m (a INTEGER)";
    exec db "INSERT INTO m VALUES (true)";
    exec db "INSERT INTO m VALUES (false)";
    check_rows db "SELECT a FROM m" [ "1"; "0" ];
    exec db "CREATE TABLE n (a INTEGER, b INTEGER)";
    exec db "INSERT INTO n (a, b) VALUES (true, false)";
    check_rows db "SELECT a, b FROM n" [ "1|0" ];
    exec db "CREATE TABLE u (\"true\" INTEGER, b INTEGER)";
    exec db "INSERT INTO u VALUES (true, 2)";
    check_rows db "SELECT \"true\", b FROM u" [ "1|2" ];
    exec db "CREATE TABLE d (a INTEGER, b INTEGER DEFAULT true, c INTEGER DEFAULT false)";
    exec db "INSERT INTO d (a) VALUES (1)";
    check_rows db "SELECT a, b, c FROM d" [ "1|1|0" ])
;;

let update_and_delete () =
  with_db (fun db ->
    seed db;
    exec db "UPDATE q SET a = true WHERE a = 3";
    check_rows db "SELECT a FROM q" [ "1"; "2"; "1" ];
    exec db "DELETE FROM q WHERE false";
    check_rows db "SELECT count(*) FROM q" [ "3" ];
    exec db "UPDATE q SET a = a + 1 WHERE true";
    check_rows db "SELECT a FROM q" [ "2"; "3"; "2" ])
;;

(* The catalog stores these three as SQL TEXT and [Exec] re-compiles them on
   every write through its own resolver, which is not [Sema]'s.  Each expected
   value was read off sqlite3 running the same script. *)
let stored_sql_round_trips () =
  with_db (fun db ->
    exec db "CREATE TABLE g (a INTEGER, ck INTEGER CHECK (true))";
    exec db "INSERT INTO g VALUES (1, 1)";
    check_rows db "SELECT a, ck FROM g" [ "1|1" ];
    exec
      db
      "CREATE TABLE gc (a INTEGER, b INTEGER GENERATED ALWAYS AS (a + true) VIRTUAL)";
    exec db "INSERT INTO gc (a) VALUES (5)";
    check_rows db "SELECT a, b FROM gc" [ "5|6" ];
    exec db "CREATE TABLE pi (a INTEGER)";
    exec db "CREATE INDEX pix ON pi (a) WHERE true";
    exec db "INSERT INTO pi VALUES (1)";
    exec db "INSERT INTO pi VALUES (2)";
    check_rows db "SELECT a FROM pi WHERE a = true" [ "1" ])
;;

(* #635's correlation detector runs [substitute_outer_in_expr] against a
   binding that resolves NOTHING and records that it was asked, so before the
   walker learned about #744 every [WHERE true] inside a subquery looked like a
   free outer reference.  The subquery was then treated as correlated:
   a scalar subquery counting rows under [WHERE true] answered NULL — silently wrong,
   sqlite3 says 3 — and the [IN] spelling raised out of the executor.  This is
   the case that would come back first if the walker arm is removed. *)
let a_subquery_with_where_true_is_not_correlated () =
  with_db (fun db ->
    seed db;
    check_rows db "SELECT (SELECT count(*) FROM q WHERE true)" [ "3" ];
    check_rows
      db
      "SELECT a FROM q WHERE a IN (SELECT a FROM q WHERE true)"
      [ "1"; "2"; "3" ];
    check_rows
      db
      "SELECT a FROM q WHERE EXISTS (SELECT 1 FROM q q2 WHERE true)"
      [ "1"; "2"; "3" ];
    check_rows db "SELECT (SELECT true)" [ "1" ];
    check_rows
      db
      "WITH c AS (SELECT a FROM q WHERE true) SELECT a FROM c"
      [ "1"; "2"; "3" ];
    exec db "CREATE VIEW v AS SELECT a FROM q WHERE true";
    check_rows db "SELECT a FROM v" [ "1"; "2"; "3" ])
;;

(* ------------------------------------------------------------------ *)
(* The boundaries                                                       *)
(* ------------------------------------------------------------------ *)

(* [x IS TRUE] is truthiness, not equality — oracle-checked, [2 IS TRUE] is 1
   while [2 IS 1] is 0 — so resolving [TRUE] to the literal 1 under a general
   [IS] would answer the wrong thing for every operand outside {0,1}.  granary
   has no general [IS] operator to do that with: [IS] appears only in [IS NULL]
   and [IS NOT NULL], so these stay PARSE errors.  Asserted rather than
   assumed, because the alternative to an error here is a silent wrong
   answer. *)
let is_true_is_still_a_parse_error () =
  with_db (fun db ->
    expect_query_error db "SELECT 1 IS TRUE";
    expect_query_error db "SELECT 0 IS FALSE";
    expect_query_error db "SELECT 1 IS NOT FALSE";
    expect_query_error db "SELECT NULL IS TRUE")
;;

(* A name that is neither a column nor true/false is still an error, and the
   fallback did not turn unknown-column into something quieter. *)
let an_unknown_name_is_still_an_error () =
  with_db (fun db ->
    seed db;
    expect_query_error db "SELECT nope FROM q";
    expect_query_error db "SELECT a FROM q WHERE nope";
    expect_query_error db "SELECT truthy FROM q";
    (* a constant GROUP BY key is not expressible at all — see the header *)
    expect_query_error db "SELECT count(*) FROM q GROUP BY true")
;;

(* An AMBIGUOUS [true] — two joined tables both carrying a column of that name
   — must stay ambiguous rather than quietly becoming the literal.  The
   fallback is taken only on the "no candidate" branch, never on "too many". *)
let an_ambiguous_column_named_true_stays_ambiguous () =
  with_db (fun db ->
    exec db "CREATE TABLE l (\"true\" INTEGER)";
    exec db "CREATE TABLE r (\"true\" INTEGER)";
    exec db "INSERT INTO l VALUES (1)";
    exec db "INSERT INTO r VALUES (2)";
    expect_query_error db "SELECT true FROM l JOIN r ON l.true = r.true")
;;

let () =
  Alcotest.run
    "bool_literal_744"
    [ ( "literal"
      , [ Alcotest.test_case "1 and 0, integer" `Quick literals_are_one_and_zero
        ; Alcotest.test_case "case insensitive" `Quick spelling_is_case_insensitive
        ] )
    ; ( "headline"
      , [ Alcotest.test_case
            "INSERT ... SELECT ... WHERE true ... DO UPDATE"
            `Quick
            insert_select_upsert_with_where_true
        ; Alcotest.test_case
            "granary's bare form still accepted"
            `Quick
            the_bare_form_granary_already_accepted_still_works
        ] )
    ; ( "identifier context"
      , [ Alcotest.test_case
            "a column named true wins"
            `Quick
            a_column_named_true_still_wins
        ; Alcotest.test_case
            "stored CHECK resolves to the column"
            `Quick
            a_stored_check_naming_the_column_still_resolves_to_it
        ; Alcotest.test_case
            "usable as table, column and alias names"
            `Quick
            true_and_false_are_still_usable_as_names
        ] )
    ; ( "clauses"
      , [ Alcotest.test_case "expression positions" `Quick clauses_that_take_an_expression
        ; Alcotest.test_case "bare projection" `Quick a_bare_boolean_projection
        ; Alcotest.test_case "INSERT VALUES and DEFAULT" `Quick insert_values_and_defaults
        ; Alcotest.test_case "UPDATE and DELETE" `Quick update_and_delete
        ; Alcotest.test_case "stored SQL round trips" `Quick stored_sql_round_trips
        ; Alcotest.test_case
            "a subquery's WHERE true is not a correlation"
            `Quick
            a_subquery_with_where_true_is_not_correlated
        ] )
    ; ( "boundaries"
      , [ Alcotest.test_case
            "IS TRUE stays a parse error"
            `Quick
            is_true_is_still_a_parse_error
        ; Alcotest.test_case
            "unknown names still error"
            `Quick
            an_unknown_name_is_still_an_error
        ; Alcotest.test_case
            "ambiguity is not resolved by the fallback"
            `Quick
            an_ambiguous_column_named_true_stays_ambiguous
        ] )
    ]
;;
