(** #592: a correlated subquery in an INNER join's ON clause silently returned
    no rows.

    {1 The defect}

    [general_on_join] gives an INNER join's ON predicate to an [Op_filter] above
    a cartesian [Op_hash_join] ([on_pred = None] — [on_pred] is set only for
    [`Left]). [stream_filter] pre-evaluates the predicate's subqueries; a
    {i correlated} one cannot be resolved that way and survives, and the
    correlation source was located by [get_outer_scan_meta], which answered
    [None] over a join node. The fallback was
    [Lwt_stream.filter (fun _row -> false)] — every row dropped, with no error.

    That is the worst of the two wrong answers #566 identified: an empty result
    set from a join is indistinguishable from "nothing matched". PR #586 made
    the {i outer} spelling raise; the inner spelling of the same query kept
    returning nothing.

    {1 The fix}

    [get_outer_scan_metas] replaces [get_outer_scan_meta]. It walks the
    layout-preserving spine {i and} both join operators, pairing every base
    table with the offset of its first column in the row being filtered — which
    is all the substitution ever needed, since both joins emit
    [Array.append lrow rrow]. Unqualified outer references are resolved too.

    Resolution is innermost-first in both spellings, and [inner_scope_of]
    supplies both halves: the inner query's own column names guard an
    unqualified reference, and its FROM {i identifiers} — the alias where one is
    given, else the table name — guard a qualified one. The second guard is not
    cosmetic. Widening resolution to joins is what made an inner FROM naming the
    same table as an outer input reachable, and without it that shape answers a
    plausible full-cardinality wrong result instead of the empty one [main]
    gave.

    What is still unresolvable — an {i unaliased} self-join, a projection
    between the filter and its scans — is now a {b raise}, not an empty result.
    (#635 later made the aliased self-join resolvable, and made a reference from
    two levels out resolve rather than raise; #626 gave the projection the same
    refusal instead of a column of NULLs; #615 reopened #566's outer-join
    refusal. Each has its own test file.)

    {1 Oracle}

    Every expected value here was taken from sqlite3 3.45.1 (see
    [test_sqlite_compare.ml] for the cases that re-check it at run time). *)

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

(* The issue's schema, verbatim. *)
let seed db =
  exec db "CREATE TABLE l (a INTEGER)";
  exec db "CREATE TABLE r (b INTEGER)";
  exec db "CREATE TABLE k (v INTEGER)";
  exec db "INSERT INTO l VALUES (1),(9)";
  exec db "INSERT INTO r VALUES (5)";
  exec db "INSERT INTO k VALUES (4)"
;;

(* ------------------------------------------------------------------ *)
(* The repro                                                            *)
(* ------------------------------------------------------------------ *)

(* The issue's query, character for character. sqlite3 3.45.1 answers [9|5]:
   for a=1 the subquery is empty so [5 > NULL] is unknown; for a=9 it is 4 and
   [5 > 4] holds. Granary returned no rows at all. *)
let inner_join_correlated_on_returns_the_row () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"#592 repro: INNER JOIN with a correlated ON subquery"
      [ [ "9"; "5" ] ]
      (rows_of db "SELECT a, b FROM l INNER JOIN r ON b > (SELECT v FROM k WHERE v < a)"))
;;

(* The same query with the outer reference qualified. This spelling exercises
   [bind_qual]; the one above exercises [bind_unqual] plus the inner-scope
   shadow test. Both must agree, or the fix would depend on how the SQL is
   written. *)
let qualified_outer_reference_agrees () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"l.a resolves exactly as bare a does"
      [ [ "9"; "5" ] ]
      (rows_of
         db
         "SELECT a, b FROM l INNER JOIN r ON b > (SELECT v FROM k WHERE v < l.a)"))
;;

(* [JOIN] with no qualifier is INNER, and reaches the same plan. *)
let bare_join_keyword_is_covered () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"JOIN == INNER JOIN"
      [ [ "9"; "5" ] ]
      (rows_of db "SELECT a, b FROM l JOIN r ON b > (SELECT v FROM k WHERE v < l.a)"))
;;

(* EXISTS and IN take the same substitution path as a scalar subquery, through
   [substitute_outer_in_plan_expr]'s [P_exists] / [P_in_select] arms. sqlite3
   answers [9|5] for the EXISTS spelling — [v < 1] holds for no k row. *)
let exists_and_in_spellings () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"EXISTS in an INNER ON"
      [ [ "9"; "5" ] ]
      (rows_of
         db
         "SELECT a, b FROM l INNER JOIN r ON EXISTS (SELECT 1 FROM k WHERE v < l.a)");
    check_rows
      ~label:"IN (SELECT ...) in an INNER ON"
      [ [ "9"; "5" ] ]
      (rows_of
         db
         "SELECT a, b FROM l INNER JOIN r ON 4 IN (SELECT v FROM k WHERE v < l.a)"))
;;

(* Three inputs: the correlation source is the {i first} of three, so the
   offsets [get_outer_scan_metas] computes have to be right for a nested join
   and not just for a single pair. sqlite3 answers [9|5|7]. *)
let three_way_join_offsets () =
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE m (c INTEGER)";
    exec db "INSERT INTO m VALUES (7),(2)";
    check_rows
      ~label:"the outer reference resolves through two joins"
      [ [ "9"; "5"; "7" ] ]
      (rows_of
         db
         "SELECT a,b,c FROM l JOIN r ON 1=1 JOIN m ON c > (SELECT v FROM k WHERE v < l.a)"))
;;

(* The correlation source is the join's RIGHT input, so its columns sit at a
   non-zero offset in the filtered row. Reading them at offset 0 would answer
   with l's values instead — a wrong answer, not an empty one, which is why
   this case is pinned separately. sqlite3 answers [1|5] and [9|5]. *)
let correlation_source_on_the_right_side () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"r.b, at offset 1 in the joined row, resolves as itself"
      [ [ "1"; "5" ]; [ "9"; "5" ] ]
      (rows_of db "SELECT a, b FROM l JOIN r ON EXISTS (SELECT 1 FROM k WHERE v < r.b)"))
;;

(* ------------------------------------------------------------------ *)
(* The single-table WHERE spelling, wrong for the same reason           *)
(* ------------------------------------------------------------------ *)

(* Not a join at all: an {i unqualified} outer reference was never substituted,
   because the old [substitute_outer_in_expr] matched [E_tbl_col] only. So this
   returned nothing where sqlite3 3.45.1 returns [9] — the same silent-empty
   failure, one operator lower down, and the reason the issue's repro could not
   be fixed by the join work alone. *)
let unqualified_outer_reference_in_a_plain_where () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"bare `a` inside the subquery is an outer reference"
      [ [ "9" ] ]
      (rows_of db "SELECT a FROM l WHERE a > (SELECT v FROM k WHERE v < a)");
    check_rows
      ~label:"and the qualified spelling still agrees"
      [ [ "9" ] ]
      (rows_of db "SELECT a FROM l WHERE a > (SELECT v FROM k WHERE v < l.a)"))
;;

(* Rewriting unqualified names is only safe if the INNER scope wins. Both [p]
   and [q] have a column [n] here, and the subquery's [n] is q's — rewriting it
   to p's would change the answer rather than error, so this is the case that
   decides whether [inner_scope_shadow] is doing its job.

   sqlite3 3.45.1 answers [10] and [20]. Rewriting [n] to [p.n] would answer
   [20] alone: p's first row has [n = 1], so [1 = 2] would be false and its
   subquery empty. *)
let inner_scope_wins_over_the_outer_one () =
  with_db (fun db ->
    exec db "CREATE TABLE p (n INTEGER, s INTEGER)";
    exec db "CREATE TABLE q (n INTEGER, mm INTEGER)";
    exec db "INSERT INTO p VALUES (1,10),(2,20)";
    exec db "INSERT INTO q VALUES (1,100),(2,200)";
    check_rows
      ~label:"the subquery's own `n` is not rewritten to the outer one"
      [ [ "10" ]; [ "20" ] ]
      (rows_of db "SELECT s FROM p WHERE s < (SELECT mm FROM q WHERE n = 2 AND mm > p.s)"))
;;

(* A QUALIFIED reference must be shadowed by the inner FROM too, and this is
   the case that made the guard necessary rather than merely tidy: the
   subquery's own FROM names [t], which is also an outer input, so [t.x] is the
   INNER t. Substituting the outer pair's value for it answers a plausible,
   full-cardinality wrong result — one spurious row and one lost — which is the
   exact failure class #592 was filed about, not the empty result [main] gave.

   On [main] this was unreachable: with a single-table outer, the outer [t] and
   the inner [t] are indistinguishable. Widening resolution to joins is what
   made it reachable, so it is pinned here rather than anywhere upstream.

   sqlite3 3.45.1 answers [2|2], [2|3], [3|1], [3|2], [3|3]. Trace for
   [s.a = 2, s.z = 25]: [t.x] ranges over the inner [t], so the count is 1 and
   [t.a > 1] admits [2|2] and [2|3]. With [t.x] pinned to the outer pair, the
   pair carrying [t.x = 10] counts 0 and wrongly emits [2|1], while the pair
   carrying [t.x = 30] counts 3 and wrongly drops [2|3]. *)
let inner_from_shadows_a_qualified_reference () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, x INTEGER)";
    exec db "CREATE TABLE s (a INTEGER, z INTEGER)";
    exec db "INSERT INTO t VALUES (1,10),(2,20),(3,30)";
    exec db "INSERT INTO s VALUES (1,5),(2,25),(3,35)";
    check_rows
      ~label:"t.x inside the subquery is the inner t, not the outer one"
      [ [ "2"; "2" ]; [ "2"; "3" ]; [ "3"; "1" ]; [ "3"; "2" ]; [ "3"; "3" ] ]
      (rows_of
         db
         "SELECT s.a, t.a FROM s JOIN t ON t.a > (SELECT COUNT(*) FROM t WHERE t.x > s.z)");
    (* The same root cause in the safer direction — an empty result rather than
       a plausible one. sqlite3 answers [1]. *)
    check_rows
      ~label:"and in a WHERE-clause subquery over the same shape"
      [ [ "1" ] ]
      (rows_of
         db
         "SELECT s.a FROM s JOIN t ON s.a = t.a WHERE (SELECT COUNT(*) FROM t WHERE t.x \
          > 15 AND t.a > s.a) > 1"))
;;

(* An alias REPLACES the table name in the inner scope rather than adding to
   it: with [FROM t q] the identifier in scope is [q], so [t.x] resolves
   OUTWARD and must still be substituted. sqlite3 agrees — this is the same
   query as the first case above with the inner [t] aliased, and it answers the
   outer-resolution result. Pinned because the obvious over-correction for the
   case above — shadowing the table name whenever it appears in the inner FROM
   — would silently break it. *)
let an_alias_does_not_shadow_the_table_name () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, x INTEGER)";
    exec db "CREATE TABLE s (a INTEGER, z INTEGER)";
    exec db "INSERT INTO t VALUES (1,10),(2,20),(3,30)";
    exec db "INSERT INTO s VALUES (1,5),(2,25),(3,35)";
    check_rows
      ~label:"FROM t q leaves t. resolving outward"
      [ [ "2"; "2" ]; [ "2"; "3" ]; [ "3"; "1" ]; [ "3"; "2" ]; [ "3"; "3" ] ]
      (rows_of
         db
         "SELECT s.a, t.a FROM s JOIN t ON t.a > (SELECT COUNT(*) FROM t q WHERE q.x > \
          s.z)"))
;;

(* ------------------------------------------------------------------ *)
(* What still cannot be resolved is refused, loudly                     *)
(* ------------------------------------------------------------------ *)

(* A self-join gives two inputs with the same SCOPE IDENTIFIER, so resolving a
   reference by it would pick one of them arbitrarily. [get_outer_scan_metas]
   answers [None] and the query is refused — the one thing #592 insists on is
   that an unresolvable correlation must not look like "no rows matched".

   #635 moved the boundary from "the same table name" to "the same scope
   identifier", so this case is now spelled without aliases. The aliased
   spelling it used to carry resolves; [test_alias_outer_ref_635.ml] pins it. *)
let self_join_is_refused_not_emptied () =
  with_db (fun db ->
    seed db;
    let msg =
      err_of db "SELECT l.a FROM l JOIN l ON EXISTS (SELECT 1 FROM k WHERE v < l.a)"
    in
    Alcotest.(check bool)
      (Printf.sprintf "refused rather than answered empty (got %S)" msg)
      true
      (msg <> "");
    Alcotest.(check bool)
      (Printf.sprintf "and the message names the cause (got %S)" msg)
      true
      (contains msg "correlated subquery" && contains msg "#592"))
;;

(* An outer reference to a table that is not in the query at all cannot be
   resolved either, and must not silently drop every row. *)
let unresolvable_reference_is_refused () =
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE z (zz INTEGER)";
    let msg =
      err_of db "SELECT a, b FROM l JOIN r ON b > (SELECT v FROM k WHERE v < z.zz)"
    in
    Alcotest.(check bool) (Printf.sprintf "refused (got %S)" msg) true (msg <> ""))
;;

(* ------------------------------------------------------------------ *)
(* Controls — shapes that were already right must stay right            *)
(* ------------------------------------------------------------------ *)

(* Resolved once, before the scan, by [pre_eval_subquery]. It never reached the
   broken path, and it is what made the wrong answer above invisible from
   outside. sqlite3: [1|5], [9|5]. *)
let uncorrelated_on_subquery_is_untouched () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"an uncorrelated ON subquery still matches both left rows"
      [ [ "1"; "5" ]; [ "9"; "5" ] ]
      (rows_of db "SELECT a, b FROM l INNER JOIN r ON b > (SELECT v FROM k)"))
;;

(* No subquery at all: the filter takes the pure, non-Lwt arm. *)
let plain_general_on_is_untouched () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"a general ON with no subquery is unaffected"
      [ [ "1"; "5" ] ]
      (rows_of db "SELECT a, b FROM l INNER JOIN r ON b > a"))
;;

(* The outer spelling of the same query WAS refused by [stream_hash_join]'s
   cartesian arm (#566, PR #586), and #592 deliberately left that asymmetry
   pinned here rather than reopening a decision that had merged hours earlier.

   #615 reopened it, using exactly the machinery this file's fix introduced:
   the joined row is [lrow @ rrow] and [get_outer_scan_metas] describes both
   inputs, so the correlation resolves inside the join. The two spellings now
   agree, and this test asserts the agreement rather than the asymmetry.
   [test_outer_join_subquery_615.ml] carries the full case set. *)
let outer_spelling_now_agrees () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"#615: the LEFT spelling answers and null-extends the unmatched row"
      [ [ "1"; "NULL" ]; [ "9"; "5" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b > (SELECT v FROM k WHERE v < a)"))
;;

let () =
  Alcotest.run
    "join_subquery_592"
    [ ( "a correlated subquery in an INNER join's ON now evaluates (#592)"
      , [ Alcotest.test_case
            "the issue's repro returns the row"
            `Quick
            inner_join_correlated_on_returns_the_row
        ; Alcotest.test_case
            "the qualified spelling agrees"
            `Quick
            qualified_outer_reference_agrees
        ; Alcotest.test_case "bare JOIN is covered" `Quick bare_join_keyword_is_covered
        ; Alcotest.test_case "EXISTS and IN spellings" `Quick exists_and_in_spellings
        ; Alcotest.test_case "three-way join offsets" `Quick three_way_join_offsets
        ; Alcotest.test_case
            "correlation source on the right side"
            `Quick
            correlation_source_on_the_right_side
        ] )
    ; ( "inner scope wins (unqualified and qualified)"
      , [ Alcotest.test_case
            "an unqualified reference in a plain WHERE resolves"
            `Quick
            unqualified_outer_reference_in_a_plain_where
        ; Alcotest.test_case
            "the inner scope wins over the outer one"
            `Quick
            inner_scope_wins_over_the_outer_one
        ; Alcotest.test_case
            "the inner FROM shadows a qualified reference"
            `Quick
            inner_from_shadows_a_qualified_reference
        ; Alcotest.test_case
            "an alias does not shadow the table name"
            `Quick
            an_alias_does_not_shadow_the_table_name
        ] )
    ; ( "what cannot be resolved is refused"
      , [ Alcotest.test_case
            "a self-join is refused, not emptied"
            `Quick
            self_join_is_refused_not_emptied
        ; Alcotest.test_case
            "an unresolvable reference is refused"
            `Quick
            unresolvable_reference_is_refused
        ] )
    ; ( "controls"
      , [ Alcotest.test_case
            "an uncorrelated ON subquery is untouched"
            `Quick
            uncorrelated_on_subquery_is_untouched
        ; Alcotest.test_case
            "a plain general ON is untouched"
            `Quick
            plain_general_on_is_untouched
        ; Alcotest.test_case
            "the outer spelling now agrees (#615)"
            `Quick
            outer_spelling_now_agrees
        ] )
    ]
;;
