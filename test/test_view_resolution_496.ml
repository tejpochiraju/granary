(** #496 / #497: a view is resolved at every FROM position, not only the
    leading one.

    Both issues are the same root cause. [Sema.bind_internal] desugared a view
    into a CTE, and it applied that rewrite to the statement's {i primary} FROM
    table and to nothing else. So:

    - #497 — a view named by a JOIN was never expanded and failed as
      [unknown table]. Same query, operands swapped, two answers.
    - #496 — a view named inside a subquery's FROM was neither expanded nor
      reported. This is the dangerous one, and its mechanism is worth knowing:
      a subquery survives binding as an [Ast.stmt] and is re-bound at execution
      time by [Exec.plan_subquery_cached], which calls [Sema.bind] with {b no}
      view table at all. The bind failed, the failure was memoized as "this
      subquery has no plan", and the statement either answered zero rows or was
      refused as an unresolvable correlation — never as "unknown table".

    The fix rewrites the whole statement instead of its first FROM item, and
    the property that makes it work for #496 is that {b each subquery carries
    its own [WITH] wrapper} rather than leaning on an enclosing one: the
    execution-time re-bind sees a self-contained statement. [WITH] inside a
    parenthesised subquery has no grammar, but the AST node does, and every
    consumer of it already existed.

    Every expected value below was oracle-checked against sqlite3 on the same
    schema and data. *)

open Lwt.Syntax
module Row = Granary_encoding.Row

let run = Lwt_main.run

let with_db f =
  let db = run (Granary.Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Granary.Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

let exec db sql =
  match run (Granary.Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Granary.Db.pp_error e
;;

let query db sql =
  run
    (let* r = Granary.Db.query db sql in
     match r with
     | Error e -> Alcotest.failf "query %S: %a" sql Granary.Db.pp_error e
     | Ok stream -> Lwt_stream.to_list stream)
;;

let query_err db sql =
  match run (Granary.Db.query db sql) with
  | Ok _ -> Alcotest.failf "expected an error for %S" sql
  | Error e -> Format.asprintf "%a" Granary.Db.pp_error e
;;

let contains_sub ~needle s =
  let n = String.length needle
  and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = needle || go (i + 1)) in
  go 0
;;

let value_to_string = function
  | Row.V_null -> "NULL"
  | Row.V_int i -> Int64.to_string i
  | Row.V_real f -> Printf.sprintf "%g" f
  | Row.V_text s -> s
  | Row.V_blob _ -> "<blob>"
;;

(* Rows rendered as pipe-joined strings, sorted, so a test states the whole
   answer rather than one projection of it. *)
let rendered rows =
  List.sort
    compare
    (List.map
       (fun (r : Row.t) -> String.concat "|" (Array.to_list (Array.map value_to_string r)))
       rows)
;;

let strings = Alcotest.(list string)
let check name expected rows = Alcotest.check strings name (List.sort compare expected) (rendered rows)

(* The #496 schema, plus a second table to join against and a view over a
   view. *)
let seed db =
  exec db "CREATE TABLE lineitem (l_orderkey INTEGER, l_quantity REAL)";
  exec db "INSERT INTO lineitem VALUES (1, 5.0)";
  exec db "INSERT INTO lineitem VALUES (2, 7.0)";
  exec db "CREATE TABLE orders (o_orderkey INTEGER, o_name TEXT)";
  exec db "INSERT INTO orders VALUES (1, 'a')";
  exec db "INSERT INTO orders VALUES (2, 'b')";
  exec db "CREATE VIEW plain AS SELECT l_orderkey AS k, l_quantity AS q FROM lineitem";
  exec db "CREATE VIEW vov AS SELECT k AS kk, q AS qq FROM plain WHERE q > 4.0"
;;

(* ------------------------------------------------------------------ *)
(* #496 — a view inside a subquery's FROM                              *)
(* ------------------------------------------------------------------ *)

(* The issue's own repro. sqlite3: 2. *)
let view_in_a_subquery_from () =
  with_db (fun db ->
    seed db;
    check
      "scalar subquery over the same view"
      [ "2" ]
      (query db "SELECT k FROM plain WHERE q = (SELECT MAX(q) FROM plain)"))
;;

(* The equivalent spelling over the base table, which always worked — pinned
   so a failure of the one above is unambiguously about the view. sqlite3: 2. *)
let the_base_table_spelling_still_agrees () =
  with_db (fun db ->
    seed db;
    check
      "scalar subquery over the base table"
      [ "2" ]
      (query db "SELECT k FROM plain WHERE q = (SELECT MAX(l_quantity) FROM lineitem)"))
;;

(* sqlite3: b *)
let view_in_an_in_subquery () =
  with_db (fun db ->
    seed db;
    check
      "IN (SELECT ... FROM view)"
      [ "b" ]
      (query db "SELECT o_name FROM orders WHERE o_orderkey IN (SELECT k FROM plain WHERE q > 6.0)"))
;;

(* A CORRELATED subquery over a view: the outer reference has to survive the
   [WITH] wrapper the fix puts around the subquery, which is what
   [Exec.inner_scope_of] and [substitute_outer_in_stmt] decide. sqlite3: b. *)
let correlated_subquery_over_a_view () =
  with_db (fun db ->
    seed db;
    check
      "EXISTS over a view, correlated to the outer row"
      [ "b" ]
      (query
         db
         "SELECT o_name FROM orders WHERE EXISTS (SELECT 1 FROM plain WHERE plain.k = \
          orders.o_orderkey AND q > 6.0)"))
;;

(* A view named in a JOIN *and* in a subquery of the same statement. sqlite3: b. *)
let view_in_a_join_and_a_subquery_at_once () =
  with_db (fun db ->
    seed db;
    check
      "join and subquery in one statement"
      [ "b" ]
      (query
         db
         "SELECT o_name FROM orders INNER JOIN plain ON k = o_orderkey WHERE q = (SELECT \
          MAX(q) FROM plain)"))
;;

(* ------------------------------------------------------------------ *)
(* #497 — a view in JOIN position                                      *)
(* ------------------------------------------------------------------ *)

(* The issue's point is that the answer must not depend on operand order.
   sqlite3: a, b for both. *)
let view_in_either_join_position () =
  with_db (fun db ->
    seed db;
    let left = query db "SELECT o_name FROM plain INNER JOIN orders ON k = o_orderkey" in
    let right = query db "SELECT o_name FROM orders INNER JOIN plain ON k = o_orderkey" in
    check "view as the leading FROM item" [ "a"; "b" ] left;
    check "view as the JOIN right-hand side" [ "a"; "b" ] right;
    Alcotest.check strings "operand order does not change the answer" (rendered left) (rendered right))
;;

(* A LEFT JOIN too — a different planner arm from the inner one. sqlite3: a|5, b|7. *)
let view_in_a_left_join () =
  with_db (fun db ->
    seed db;
    check
      "view on the right of a LEFT JOIN"
      [ "a|5"; "b|7" ]
      (query db "SELECT o_name, q FROM orders LEFT JOIN plain ON k = o_orderkey"))
;;

(* Two views joined to each other, neither of them a base table. sqlite3: 1|5, 2|7. *)
let two_views_joined () =
  with_db (fun db ->
    seed db;
    check
      "view JOIN view"
      [ "1|5"; "2|7" ]
      (query db "SELECT plain.k, vov.qq FROM plain INNER JOIN vov ON plain.k = vov.kk"))
;;

(* ------------------------------------------------------------------ *)
(* Scope identifiers through an expanded view (#635/#626/#615)         *)
(* ------------------------------------------------------------------ *)

(* An expanded view has to present the same scope identifier a table would:
   its own name when unaliased, the alias when aliased, and the alias must
   REPLACE the name (#635). All four spellings, sqlite3-checked. *)
let qualified_column_through_an_expanded_view () =
  with_db (fun db ->
    seed db;
    check
      "view name qualifies its own column, leading position"
      [ "1"; "2" ]
      (query db "SELECT plain.k FROM plain");
    check
      "view name qualifies its own column, JOIN position"
      [ "1"; "2" ]
      (query db "SELECT plain.k FROM orders INNER JOIN plain ON plain.k = o_orderkey");
    check
      "an alias names the view, leading position"
      [ "1"; "2" ]
      (query db "SELECT p.k FROM plain AS p");
    check
      "an alias names the view, JOIN position"
      [ "1"; "2" ]
      (query db "SELECT p.k FROM orders INNER JOIN plain AS p ON p.k = o_orderkey"))
;;

(* #635: an alias REPLACES the name, so the view's own name no longer resolves
   once it is aliased — the same refusal a base table gets, not an answer.
   sqlite3 also errors ("no such column: plain.k"). *)
let an_alias_hides_the_view_name () =
  with_db (fun db ->
    seed db;
    let msg = query_err db "SELECT plain.k FROM plain AS p" in
    Alcotest.check
      Alcotest.bool
      "the hidden view name is refused, not answered"
      true
      (contains_sub ~needle:"plain" msg))
;;

(* ------------------------------------------------------------------ *)
(* View of view                                                        *)
(* ------------------------------------------------------------------ *)

(* sqlite3: 1|5, 2|7 *)
let view_of_view () =
  with_db (fun db ->
    seed db;
    check "view selecting from a view" [ "1|5"; "2|7" ] (query db "SELECT kk, qq FROM vov"))
;;

(* The nested view has to expand in JOIN position too — that is both issues at
   once. sqlite3: a|5, b|7. *)
let view_of_view_in_join_position () =
  with_db (fun db ->
    seed db;
    check
      "view-of-view as the JOIN right-hand side"
      [ "a|5"; "b|7" ]
      (query db "SELECT o_name, qq FROM orders INNER JOIN vov ON kk = o_orderkey"))
;;

(* ...and in a subquery. sqlite3: 2. *)
let view_of_view_in_a_subquery () =
  with_db (fun db ->
    seed db;
    check
      "view-of-view inside a subquery's FROM"
      [ "2" ]
      (query db "SELECT kk FROM vov WHERE qq = (SELECT MAX(qq) FROM vov)"))
;;

(* ------------------------------------------------------------------ *)
(* Cycles                                                              *)
(* ------------------------------------------------------------------ *)

(* CREATE VIEW validates its body, so the obvious cycles cannot be built. This
   one can: a real table temporarily shadows the view name, the second view
   binds against the TABLE, and dropping the table leaves two views defined in
   terms of each other.

    Expanding it is unbounded, so the guard is not a nicety: without it the
    expansion recurses until the stack goes, taking the process with it. It has
    to be refused. sqlite3 reports "view v1 is circularly defined"; the message
    differs, the refusal does not. *)
let a_view_cycle_is_refused_not_expanded_forever () =
  with_db (fun db ->
    exec db "CREATE TABLE a1 (x INTEGER)";
    exec db "INSERT INTO a1 VALUES (1)";
    exec db "CREATE VIEW v2 AS SELECT x FROM a1";
    exec db "CREATE VIEW v1 AS SELECT x FROM v2";
    check "before the cycle exists" [ "1" ] (query db "SELECT x FROM v1");
    exec db "DROP VIEW v2";
    exec db "CREATE TABLE v2 (x INTEGER)";
    (* Binds against the TABLE v2, so CREATE VIEW's own validation passes. *)
    exec db "CREATE VIEW v2 AS SELECT x FROM v1";
    check "the table still shadows the view" [] (query db "SELECT x FROM v1");
    exec db "DROP TABLE v2";
    let msg = query_err db "SELECT x FROM v1" in
    Alcotest.check
      Alcotest.bool
      "the cycle is named in the error"
      true
      (contains_sub ~needle:"itself" msg);
    (* The refusal must not have left the connection unusable. *)
    check "the connection survives the refusal" [ "1" ] (query db "SELECT x FROM a1"))
;;

(* ------------------------------------------------------------------ *)
(* Shadowing, and what must NOT change                                 *)
(* ------------------------------------------------------------------ *)

(* A CTE of the same name shadows the view, at every FROM position. sqlite3:
   99 for both. *)
let a_cte_shadows_a_same_named_view () =
  with_db (fun db ->
    seed db;
    check
      "CTE shadows the view in the leading FROM"
      [ "99" ]
      (query db "WITH plain AS (SELECT 99 AS k) SELECT k FROM plain");
    (* Distinguishing: the view would answer q = 7 for o_orderkey 2. *)
    check
      "CTE shadows the view in JOIN position"
      [ "99" ]
      (query
         db
         "WITH plain AS (SELECT 2 AS k, 99 AS q) SELECT plain.q FROM orders INNER JOIN \
          plain ON plain.k = o_orderkey"))
;;

(* A real table of the same name wins over the view — the pre-existing
   precedence, which the rewrite consults through [Cat.find_table_cached]
   rather than re-deciding. *)
let a_real_table_still_wins_over_a_view () =
  with_db (fun db ->
    exec db "CREATE TABLE base (a INTEGER)";
    exec db "INSERT INTO base VALUES (1)";
    exec db "CREATE TABLE shadowed (a INTEGER)";
    exec db "INSERT INTO shadowed VALUES (42)";
    exec db "CREATE VIEW shadowed AS SELECT a FROM base";
    check "the table, not the view" [ "42" ] (query db "SELECT a FROM shadowed"))
;;

(* The rewritten binder arm must not turn a genuine unknown table into
   something else — in either FROM position. *)
let an_unknown_table_is_still_unknown () =
  with_db (fun db ->
    seed db;
    List.iter
      (fun sql ->
         let msg = query_err db sql in
         Alcotest.check
           Alcotest.bool
           (Printf.sprintf "unknown table reported for %S" sql)
           true
           (contains_sub ~needle:"nosuch" msg))
      [ "SELECT * FROM nosuch"
      ; "SELECT o_name FROM orders INNER JOIN nosuch ON 1 = 1"
      ; "SELECT o_name FROM plain INNER JOIN nosuch ON 1 = 1"
      ])
;;

(* A view is still usable everywhere it already was: as the leading FROM item,
   under an aggregate, and under a WHERE + ORDER BY. sqlite3: 1|5 and 2|7; 2;
   2|7. *)
let the_leading_from_position_is_unchanged () =
  with_db (fun db ->
    seed db;
    check "plain select from a view" [ "1|5"; "2|7" ] (query db "SELECT k, q FROM plain");
    check
      "aggregate over a view"
      [ "2" ]
      (query db "SELECT COUNT(*) FROM plain");
    check
      "filter and order over a view"
      [ "2|7" ]
      (query db "SELECT k, q FROM plain WHERE q > 6.0 ORDER BY k"))
;;

let () =
  Alcotest.run
    "view_resolution_496"
    [ ( "#496 subquery FROM"
      , [ Alcotest.test_case "the issue's repro" `Quick view_in_a_subquery_from
        ; Alcotest.test_case
            "base-table spelling agrees"
            `Quick
            the_base_table_spelling_still_agrees
        ; Alcotest.test_case "IN (SELECT ... FROM view)" `Quick view_in_an_in_subquery
        ; Alcotest.test_case
            "correlated EXISTS over a view"
            `Quick
            correlated_subquery_over_a_view
        ; Alcotest.test_case
            "join and subquery at once"
            `Quick
            view_in_a_join_and_a_subquery_at_once
        ] )
    ; ( "#497 JOIN position"
      , [ Alcotest.test_case "either operand position" `Quick view_in_either_join_position
        ; Alcotest.test_case "LEFT JOIN" `Quick view_in_a_left_join
        ; Alcotest.test_case "view JOIN view" `Quick two_views_joined
        ] )
    ; ( "scope identifiers"
      , [ Alcotest.test_case
            "qualified through an expanded view"
            `Quick
            qualified_column_through_an_expanded_view
        ; Alcotest.test_case "an alias hides the name" `Quick an_alias_hides_the_view_name
        ] )
    ; ( "view of view"
      , [ Alcotest.test_case "leading position" `Quick view_of_view
        ; Alcotest.test_case "JOIN position" `Quick view_of_view_in_join_position
        ; Alcotest.test_case "subquery FROM" `Quick view_of_view_in_a_subquery
        ] )
    ; ( "refusals and shadowing"
      , [ Alcotest.test_case
            "a cycle is refused"
            `Quick
            a_view_cycle_is_refused_not_expanded_forever
        ; Alcotest.test_case "a CTE shadows a view" `Quick a_cte_shadows_a_same_named_view
        ; Alcotest.test_case
            "a table shadows a view"
            `Quick
            a_real_table_still_wins_over_a_view
        ; Alcotest.test_case
            "unknown is still unknown"
            `Quick
            an_unknown_table_is_still_unknown
        ; Alcotest.test_case
            "leading FROM unchanged"
            `Quick
            the_leading_from_position_is_unchanged
        ] )
    ]
;;
