(** #635: an ALIAS-qualified outer reference in a correlated subquery.

    {1 The rule}

    There is one rule for outer-reference resolution, and it is the same rule at
    every level:

    {v
      an input's SCOPE IDENTIFIER is its FROM item's alias where it has one,
      and its table name otherwise — an alias REPLACES the name.
      A qualified outer reference names a scope identifier.
      An unqualified one names a column exactly one input carries.
      Anything else is an error, never a silent empty or NULL result.
    v}

    Before #635 the rule was implemented three times and agreed nowhere:

    - [Exec.inner_scope_of] applied it correctly to a {i subquery's own} FROM
      (that is what [an_alias_does_not_shadow_the_table_name] in
      [test_join_subquery_592.ml] pins);
    - [Exec.get_outer_scan_metas] could not apply it at all, because
      [Plan.Op_seq_scan] and the other leaf scans carried a [Cat.table_meta] and
      nothing else — the alias was dropped at plan construction.  So [x.a]
      found no outer input named [x] and the query was {b refused};
    - [Sema]'s two qualified lookups accepted the alias {i and} the table name,
      so [SELECT t.x FROM t s] answered rows where sqlite3 says
      "no such column: t.x".  That one is the dangerous half: the binder runs
      first, so an alias-hidden name resolved {i inward}, the subquery was never
      recognised as correlated, and rows were {b silently lost}.

    #635 gives the leaf scans an [alias] and makes all three read the same rule.

    {1 Oracle}

    Every expected value here was taken from sqlite3 3.45.1. *)

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

(* A refusal surfaces either as a [Db.error] or as an exception, and either
   while the plan is built or while the stream is pulled.  All four are the same
   outcome here: the query did not answer. *)
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

let contains msg needle =
  let n = String.length needle
  and m = String.length msg in
  let rec go i = i + n <= m && (String.sub msg i n = needle || go (i + 1)) in
  go 0
;;

let refused db sql ~label =
  let msg = err_of db sql in
  Alcotest.(check bool)
    (Printf.sprintf "%s: refused rather than answered (got %S)" label msg)
    true
    (msg <> "");
  msg
;;

(* The issue's schema, verbatim. *)
let seed db =
  exec db "CREATE TABLE l (a INTEGER)";
  exec db "CREATE TABLE r (b INTEGER)";
  exec db "CREATE TABLE k (v INTEGER)";
  exec db "INSERT INTO l VALUES (1),(9)";
  exec db "INSERT INTO r VALUES (5)";
  exec db "INSERT INTO k VALUES (4)"
;;

(* t(a,x) and s(a,z), the shape #635's comment uses for the binder half. *)
let seed_ts db =
  exec db "CREATE TABLE t (a INTEGER, x INTEGER)";
  exec db "CREATE TABLE s (a INTEGER, z INTEGER)";
  exec db "INSERT INTO t VALUES (1,10),(2,20),(3,30)";
  exec db "INSERT INTO s VALUES (1,5),(2,25),(3,35)"
;;

(* ------------------------------------------------------------------ *)
(* The repro: an alias-qualified outer reference resolves               *)
(* ------------------------------------------------------------------ *)

(* The issue's query, character for character.  sqlite3 3.45.1 answers [9|5];
   granary raised the #592 refusal. *)
let alias_qualified_outer_ref_in_an_on_clause () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"#635 repro: x.a resolves against the aliased outer input"
      [ [ "9"; "5" ] ]
      (rows_of
         db
         "SELECT x.a, b FROM l AS x JOIN r ON b > (SELECT v FROM k WHERE v < x.a)"))
;;

(* The two spellings the issue reports as already working must keep working —
   the fix must widen resolution, not move it. *)
let the_unaliased_spellings_still_agree () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"unqualified"
      [ [ "9"; "5" ] ]
      (rows_of db "SELECT a, b FROM l JOIN r ON b > (SELECT v FROM k WHERE v < a)");
    check_rows
      ~label:"table-qualified"
      [ [ "9"; "5" ] ]
      (rows_of db "SELECT a, b FROM l JOIN r ON b > (SELECT v FROM k WHERE v < l.a)"))
;;

(* The bare-alias form [FROM l x] must behave as [FROM l AS x] does — the AS is
   optional syntax, not a different construct. *)
let the_as_keyword_is_optional () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"FROM l x == FROM l AS x"
      [ [ "9"; "5" ] ]
      (rows_of db "SELECT x.a, b FROM l x JOIN r ON b > (SELECT v FROM k WHERE v < x.a)"))
;;

(* An alias on the join's RIGHT input, whose columns sit at a non-zero offset in
   the joined row: reading them at offset 0 would answer with l's values instead,
   a wrong answer rather than an error.  sqlite3 answers [1|5] and [9|5]. *)
let an_alias_on_the_right_side_of_a_join () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"y.b, at offset 1 in the joined row, resolves as itself"
      [ [ "1"; "5" ]; [ "9"; "5" ] ]
      (rows_of
         db
         "SELECT a, y.b FROM l JOIN r AS y ON EXISTS (SELECT 1 FROM k WHERE v < y.b)"))
;;

(* ------------------------------------------------------------------ *)
(* The same rule in every clause a correlated subquery can sit in       *)
(* ------------------------------------------------------------------ *)

(* WHERE.  sqlite3 answers [9]. *)
let alias_qualified_outer_ref_in_a_where () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"WHERE"
      [ [ "9" ] ]
      (rows_of db "SELECT x.a FROM l AS x WHERE EXISTS (SELECT 1 FROM k WHERE v < x.a)"))
;;

(* Projection.  sqlite3 answers [1|0] and [9|1]. *)
let alias_qualified_outer_ref_in_a_projection () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"projection"
      [ [ "1"; "0" ]; [ "9"; "1" ] ]
      (rows_of db "SELECT x.a, (SELECT COUNT(*) FROM k WHERE v < x.a) FROM l AS x"))
;;

(* HAVING, over a GROUP BY.  The correlation source is an aggregate output row,
   so it resolves through [binding_of_group_cols] rather than
   [binding_of_metas] — a second code path for the same rule, which is why it is
   pinned separately.  sqlite3 answers [9|1]. *)
let alias_qualified_outer_ref_in_a_having () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"HAVING over a grouped, aliased input"
      [ [ "9"; "1" ] ]
      (rows_of
         db
         "SELECT x.a, COUNT(*) FROM l AS x GROUP BY x.a HAVING COUNT(*) > (SELECT \
          COUNT(*) FROM k WHERE v > x.a)"))
;;

(* Two levels of nesting: the innermost subquery references the outermost
   query's aliased input, through an intermediate subquery that mentions neither.
   Until #635 [substitute_outer_in_expr] did not descend into a nested
   [E_exists] at all, so the reference survived and the query was refused.
   sqlite3 answers [9]. *)
let two_nesting_levels () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"a doubly-nested reference to the outermost input"
      [ [ "9" ] ]
      (rows_of
         db
         "SELECT x.a FROM l AS x WHERE EXISTS (SELECT 1 FROM r WHERE EXISTS (SELECT 1 \
          FROM k WHERE v < x.a))"))
;;

(* The INTERMEDIATE level still shadows what it owns.  Both the outer table and
   the middle subquery's table have a column [b], and the innermost [b] is the
   MIDDLE one's — SQL resolves innermost-first, and "innermost" is cumulative.
   Carrying only the innermost scope down (the obvious way to implement nesting)
   would bind it to the outer row instead and answer [9] alone: for [x.b = 1],
   [4 < 1] is false.  sqlite3 answers [1] and [9], because the comparison is
   against [r2.b = 5] for every outer row. *)
let the_intermediate_scope_is_not_skipped () =
  with_db (fun db ->
    exec db "CREATE TABLE l2 (b INTEGER)";
    exec db "CREATE TABLE r2 (b INTEGER)";
    exec db "CREATE TABLE k (v INTEGER)";
    exec db "INSERT INTO l2 VALUES (1),(9)";
    exec db "INSERT INTO r2 VALUES (5)";
    exec db "INSERT INTO k VALUES (4)";
    check_rows
      ~label:"the middle subquery's own column stays its own, two levels down"
      [ [ "1" ]; [ "9" ] ]
      (rows_of
         db
         "SELECT x.b FROM l2 AS x WHERE EXISTS (SELECT 1 FROM r2 WHERE EXISTS (SELECT 1 \
          FROM k WHERE v < b))"))
;;

(* ------------------------------------------------------------------ *)
(* An alias REPLACES the table name — the outer half                    *)
(* ------------------------------------------------------------------ *)

(* [FROM l AS x] puts [x] in scope and takes [l] out of it.  sqlite3 rejects
   [l.a] there outright, so granary must not answer with it either — and
   crucially must not answer an empty result, which is what the pre-#592 code
   did for every unresolvable reference. *)
let the_table_name_is_out_of_scope_once_aliased () =
  with_db (fun db ->
    seed db;
    let msg =
      refused
        db
        "SELECT x.a, b FROM l AS x JOIN r ON b > (SELECT v FROM k WHERE v < l.a)"
        ~label:"l.a under FROM l AS x"
    in
    Alcotest.(check bool)
      (Printf.sprintf "the message names the cause (got %S)" msg)
      true
      (contains msg "correlated subquery"))
;;

(* A self-join with DISTINCT aliases is now resolvable — two inputs, two
   identifiers, no ambiguity.  This is the case #592 had to refuse because
   resolution was by table name, and the case #626's repro depends on.
   sqlite3 answers [9|1] and [9|9]. *)
let an_aliased_self_join_resolves () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"two aliases over one table disambiguate the correlation"
      [ [ "9"; "1" ]; [ "9"; "9" ] ]
      (rows_of
         db
         "SELECT x.a, y.a FROM l AS x JOIN l AS y ON EXISTS (SELECT 1 FROM k WHERE v < \
          x.a)"))
;;

(* …and an UNALIASED self-join is still refused, for the reason it always was:
   two inputs share one identifier, so resolving by it would pick one
   arbitrarily.  The boundary moved from "table name" to "scope identifier"; it
   did not disappear. *)
let an_unaliased_self_join_is_still_refused () =
  with_db (fun db ->
    seed db;
    ignore
      (refused
         db
         "SELECT l.a FROM l JOIN l ON EXISTS (SELECT 1 FROM k WHERE v < l.a)"
         ~label:"unaliased self-join"))
;;

(* An ambiguous UNQUALIFIED outer reference: both inputs carry a column [a], so
   the reference names no single input.  sqlite3 reports "ambiguous column
   name"; granary must not pick one. *)
let an_ambiguous_unqualified_outer_ref_is_refused () =
  with_db (fun db ->
    exec db "CREATE TABLE p (a INTEGER)";
    exec db "CREATE TABLE q (a INTEGER)";
    exec db "CREATE TABLE k2 (v INTEGER)";
    exec db "INSERT INTO p VALUES (1)";
    exec db "INSERT INTO q VALUES (2)";
    exec db "INSERT INTO k2 VALUES (4)";
    ignore
      (refused
         db
         "SELECT p.a, q.a FROM p JOIN q ON EXISTS (SELECT 1 FROM k2 WHERE v < a)"
         ~label:"ambiguous bare `a` across two inputs"))
;;

(* ------------------------------------------------------------------ *)
(* An alias REPLACES the table name — the BINDER half (#635 comment)    *)
(* ------------------------------------------------------------------ *)

(* The binder used to accept the alias AND the table name.  sqlite3 rejects the
   table name once the FROM item is aliased; granary answered rows.  These two
   are the comment's own repros. *)
let the_binder_hides_the_table_name_behind_an_alias () =
  with_db (fun db ->
    seed_ts db;
    ignore (refused db "SELECT t.x FROM t s" ~label:"SELECT t.x FROM t s");
    ignore
      (refused db "SELECT COUNT(*) FROM t s WHERE t.x > 15" ~label:"WHERE t.x FROM t s");
    (* The alias itself still resolves, or the rule would be "neither works". *)
    check_rows
      ~label:"the alias resolves"
      [ [ "10" ]; [ "20" ]; [ "30" ] ]
      (rows_of db "SELECT s.x FROM t s"))
;;

(* The comment's third repro, and the one that costs rows.  The subquery's own
   FROM is [t s], so [s.x] is the INNER t and [t.x] is the OUTER one; the binder
   used to resolve [t.x] inward, the subquery was folded to a constant, and the
   query answered 3 rows.

   sqlite3 3.45.1 answers 6: [1|1 1|3 2|1 2|3 3|1 3|3].  Trace — the inner count
   is [|{t : x > 15}| = 2] when the outer [t.x > 15] and 0 otherwise, so
   [t.a > count] admits every [s] for [t.a = 1] (count 0) and for [t.a = 3]
   (count 2), and none for [t.a = 2]. *)
let the_binder_defect_that_lost_rows () =
  with_db (fun db ->
    seed_ts db;
    check_rows
      ~label:"the inner alias hides the outer table name, and the outer one binds"
      [ [ "1"; "1" ]
      ; [ "1"; "3" ]
      ; [ "2"; "1" ]
      ; [ "2"; "3" ]
      ; [ "3"; "1" ]
      ; [ "3"; "3" ]
      ]
      (rows_of
         db
         "SELECT s.a, t.a FROM s JOIN t ON t.a > (SELECT COUNT(*) FROM t s WHERE s.x > \
          15 AND t.x > 15)"))
;;

(* ------------------------------------------------------------------ *)
(* Regression guards for the same family (#485, #492)                   *)
(* ------------------------------------------------------------------ *)

(* #485: an UNQUALIFIED outer reference was never substituted, so EXISTS was
   uniformly false and the query returned nothing.  The issue's minimal repro,
   verbatim. *)
let unqualified_outer_ref_485 () =
  with_db (fun db ->
    exec db "CREATE TABLE a (x INTEGER)";
    exec db "CREATE TABLE b (x INTEGER)";
    exec db "INSERT INTO a VALUES (1)";
    exec db "INSERT INTO b VALUES (1)";
    check_rows
      ~label:"#485 qualified"
      [ [ "1" ] ]
      (rows_of db "SELECT x FROM a WHERE EXISTS (SELECT * FROM b WHERE b.x = a.x)");
    check_rows
      ~label:"#485 unqualified"
      [ [ "1" ] ]
      (rows_of db "SELECT x FROM a WHERE EXISTS (SELECT * FROM b WHERE b.x = x)"))
;;

(* #485's comment: a correlated SCALAR subquery with an unqualified outer
   reference evaluated to NULL — and a correlated COUNT-star can never
   legitimately be NULL, which is what made it self-evidently wrong.  This is
   the shape that made two TPC-C consistency conditions report a clean database
   over any input. *)
let unqualified_scalar_subquery_485 () =
  with_db (fun db ->
    exec db "CREATE TABLE warehouse (w_id INTEGER)";
    exec db "CREATE TABLE district (d_w_id INTEGER, d_ytd INTEGER)";
    exec db "INSERT INTO warehouse VALUES (1)";
    exec db "INSERT INTO district VALUES (1,20000),(1,40000)";
    check_rows
      ~label:"#485 unqualified scalar correlation"
      [ [ "1"; "60000" ] ]
      (rows_of
         db
         "SELECT w_id, (SELECT SUM(d_ytd) FROM district WHERE d_w_id = w_id) FROM \
          warehouse");
    check_rows
      ~label:"#485 correlated COUNT(*) is never NULL"
      [ [ "1"; "2" ] ]
      (rows_of
         db
         "SELECT w_id, (SELECT COUNT(*) FROM district WHERE d_w_id = w_id) FROM warehouse"))
;;

(* #492: a correlated subquery returned NO rows when the outer FROM was a join,
   even with the reference fully qualified.  All four of the issue's queries. *)
let joined_outer_492 () =
  with_db (fun db ->
    exec db "CREATE TABLE part (p_partkey INTEGER)";
    exec db "CREATE TABLE partsupp (ps_partkey INTEGER, ps_suppkey INTEGER)";
    exec db "INSERT INTO part VALUES (1), (2)";
    exec db "INSERT INTO partsupp VALUES (1, 10), (2, 20)";
    check_rows
      ~label:"#492 (1) single-table outer + EXISTS"
      [ [ "1" ]; [ "2" ] ]
      (rows_of
         db
         "SELECT p_partkey FROM part WHERE EXISTS (SELECT * FROM partsupp WHERE \
          ps_partkey = part.p_partkey)");
    check_rows
      ~label:"#492 (2) joined outer + EXISTS"
      [ [ "1" ]; [ "2" ] ]
      (rows_of
         db
         "SELECT part.p_partkey FROM part INNER JOIN partsupp ON part.p_partkey = \
          partsupp.ps_partkey WHERE EXISTS (SELECT * FROM partsupp x WHERE x.ps_partkey \
          = part.p_partkey)");
    check_rows
      ~label:"#492 (3) single-table outer + scalar"
      [ [ "1" ]; [ "2" ] ]
      (rows_of
         db
         "SELECT p_partkey FROM part WHERE p_partkey = (SELECT MAX(ps_partkey) FROM \
          partsupp WHERE ps_partkey = part.p_partkey)");
    check_rows
      ~label:"#492 (4) joined outer + scalar"
      [ [ "1" ]; [ "2" ] ]
      (rows_of
         db
         "SELECT part.p_partkey FROM part INNER JOIN partsupp ON part.p_partkey = \
          partsupp.ps_partkey WHERE part.p_partkey = (SELECT MAX(x.ps_partkey) FROM \
          partsupp x WHERE x.ps_partkey = part.p_partkey)"))
;;

let () =
  Alcotest.run
    "alias_outer_ref_635"
    [ ( "an alias-qualified outer reference resolves (#635)"
      , [ Alcotest.test_case
            "the issue's repro"
            `Quick
            alias_qualified_outer_ref_in_an_on_clause
        ; Alcotest.test_case
            "the unaliased spellings still agree"
            `Quick
            the_unaliased_spellings_still_agree
        ; Alcotest.test_case "AS is optional" `Quick the_as_keyword_is_optional
        ; Alcotest.test_case
            "an alias on the right side of a join"
            `Quick
            an_alias_on_the_right_side_of_a_join
        ] )
    ; ( "every clause a correlated subquery can sit in"
      , [ Alcotest.test_case "WHERE" `Quick alias_qualified_outer_ref_in_a_where
        ; Alcotest.test_case "projection" `Quick alias_qualified_outer_ref_in_a_projection
        ; Alcotest.test_case "HAVING" `Quick alias_qualified_outer_ref_in_a_having
        ; Alcotest.test_case "two nesting levels" `Quick two_nesting_levels
        ; Alcotest.test_case
            "the intermediate scope is not skipped"
            `Quick
            the_intermediate_scope_is_not_skipped
        ] )
    ; ( "an alias replaces the table name"
      , [ Alcotest.test_case
            "the table name is out of scope once aliased"
            `Quick
            the_table_name_is_out_of_scope_once_aliased
        ; Alcotest.test_case
            "an aliased self-join resolves"
            `Quick
            an_aliased_self_join_resolves
        ; Alcotest.test_case
            "an unaliased self-join is still refused"
            `Quick
            an_unaliased_self_join_is_still_refused
        ; Alcotest.test_case
            "an ambiguous unqualified outer reference is refused"
            `Quick
            an_ambiguous_unqualified_outer_ref_is_refused
        ; Alcotest.test_case
            "the binder hides the table name behind an alias"
            `Quick
            the_binder_hides_the_table_name_behind_an_alias
        ; Alcotest.test_case
            "the binder defect that lost rows"
            `Quick
            the_binder_defect_that_lost_rows
        ] )
    ; ( "the same family, already closed (#485, #492)"
      , [ Alcotest.test_case
            "#485 unqualified outer reference"
            `Quick
            unqualified_outer_ref_485
        ; Alcotest.test_case
            "#485 unqualified scalar subquery"
            `Quick
            unqualified_scalar_subquery_485
        ; Alcotest.test_case "#492 joined outer" `Quick joined_outer_492
        ] )
    ]
;;
