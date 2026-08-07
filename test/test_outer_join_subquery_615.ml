(** #615: a correlated subquery in an OUTER join's ON predicate is now resolved.

    {1 What #566 decided, and why it is reopened}

    #566 refused the shape outright. Its stated ground was that resolving it
    needed a correlation source [get_outer_scan_meta] could not produce over a
    join node — and it was right: a surviving [P_subquery] evaluates to
    [Row.V_null], so the ON predicate is false for every pair, [any] is never
    set, and an outer join null-extends {i every} left row. That is a complete
    result set of the right cardinality with the ON predicate silently
    unevaluated, which is worse than an error.

    PR #614 (#592) built the source: [Exec.get_outer_scan_metas] pairs every
    base table under a plan subtree with the ordinal of its first column in the
    emitted row, and [Op_hash_join]'s cartesian arm has both inputs in hand. The
    blocker is gone, so the two spellings of one query no longer disagree:

    {v
      SELECT a, b FROM l INNER JOIN r ON b > (SELECT v FROM k WHERE v < a);
      SELECT a, b FROM l LEFT  JOIN r ON b > (SELECT v FROM k WHERE v < a);
    v}

    sqlite3 3.45.1 answers [9|5] for the first and [1|NULL], [9|5] for the
    second. Granary answered the first (since #592) and raised on the second.

    {1 The cost, and what it does not cost}

    For an outer join the ON predicate {b is} the match test, so it has to be
    evaluated per (left, right) {i pair} inside the join — a filter above the
    join would reject the very null-extended row it must emit (#552). So the
    substitution and re-[pre_eval_subquery] run once per pair rather than once
    per surviving row, and the pairing loop becomes Lwt.

    The pure loop is kept as the arm taken when no subquery survives, which is
    every join that has no correlated ON at all. [a_plain_general_on_is_pure]
    and the uncorrelated control below are what hold that arm in place.

    {1 The refusal did not disappear}

    It moved to the same boundary the INNER spelling has: an outer reference
    that names no input, or that two inputs answer to, is still refused —
    now naming #615 rather than #566.

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

(* #566's and #592's shared schema. *)
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

(* The issue's query. For [a = 1] the subquery is empty, so [5 > NULL] is
   unknown, no right row matches, and the left row is null-extended. For
   [a = 9] it is 4 and [5 > 4] holds. That the FIRST row survives at all is the
   whole reason the ON predicate must be evaluated inside the join. *)
let outer_spelling_now_answers () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"#615 repro: LEFT JOIN with a correlated ON subquery"
      [ [ "1"; "NULL" ]; [ "9"; "5" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b > (SELECT v FROM k WHERE v < a)"))
;;

(* The two spellings of one query, side by side. This is the asymmetry the issue
   is about, so it is asserted as a pair rather than only as two values. *)
let the_two_spellings_agree () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"INNER keeps only the matching row"
      [ [ "9"; "5" ] ]
      (rows_of db "SELECT a, b FROM l INNER JOIN r ON b > (SELECT v FROM k WHERE v < a)");
    check_rows
      ~label:"LEFT keeps it and null-extends the other"
      [ [ "1"; "NULL" ]; [ "9"; "5" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b > (SELECT v FROM k WHERE v < a)"))
;;

(* Qualified and alias-qualified outer references reach the same binding, so all
   three spellings must agree or the fix would depend on how the SQL is
   written. The alias case is #635's rule applied at this operator. *)
let every_spelling_of_the_outer_reference () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"table-qualified"
      [ [ "1"; "NULL" ]; [ "9"; "5" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b > (SELECT v FROM k WHERE v < l.a)");
    check_rows
      ~label:"alias-qualified"
      [ [ "1"; "NULL" ]; [ "9"; "5" ] ]
      (rows_of
         db
         "SELECT x.a, b FROM l AS x LEFT JOIN r ON b > (SELECT v FROM k WHERE v < x.a)"))
;;

(* EXISTS and IN take the same substitution path through
   [substitute_outer_in_plan_expr]'s [P_exists] / [P_in_select] arms. *)
let exists_and_in_spellings () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"EXISTS in a LEFT ON"
      [ [ "1"; "NULL" ]; [ "9"; "5" ] ]
      (rows_of
         db
         "SELECT a, b FROM l LEFT JOIN r ON EXISTS (SELECT 1 FROM k WHERE v < l.a)");
    check_rows
      ~label:"IN (SELECT ...) in a LEFT ON"
      [ [ "1"; "NULL" ]; [ "9"; "5" ] ]
      (rows_of
         db
         "SELECT a, b FROM l LEFT JOIN r ON 4 IN (SELECT v FROM k WHERE v < l.a)"))
;;

(* The correlation source is the join's RIGHT input, whose columns sit at a
   non-zero offset in the joined row — the offsets have to be re-based by the
   left input's width, which is the one arithmetic step this change adds.
   Reading [r.b] at offset 0 would compare against l's value instead.
   sqlite3 answers [1|5] and [9|5]: [v < 5] holds for k's only row. *)
let the_correlation_source_is_the_right_input () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"r.b resolves at its own offset in the joined row"
      [ [ "1"; "5" ]; [ "9"; "5" ] ]
      (rows_of
         db
         "SELECT a, b FROM l LEFT JOIN r ON EXISTS (SELECT 1 FROM k WHERE v < r.b)"))
;;

(* An ON subquery that matches for no pair must null-extend EVERY left row, not
   drop them — the outer-join contract, checked with a correlated predicate
   rather than an uncorrelated one. sqlite3 answers [1|NULL] and [9|NULL]. *)
let a_correlated_on_that_never_matches_still_null_extends () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"no pair matches, both left rows survive null-extended"
      [ [ "1"; "NULL" ]; [ "9"; "NULL" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b < (SELECT v FROM k WHERE v < l.a)"))
;;

(* ------------------------------------------------------------------ *)
(* The refusal moved to the same boundary the INNER spelling has        *)
(* ------------------------------------------------------------------ *)

(* An outer reference to a table the query does not contain. Still refused —
   and it must not null-extend every left row, which is exactly the wrong
   answer #566 refused in order to avoid. *)
let an_unresolvable_reference_is_still_refused () =
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE zz (q INTEGER)";
    let msg =
      refused
        db
        "SELECT a, b FROM l LEFT JOIN r ON b > (SELECT v FROM k WHERE v < zz.q)"
        ~label:"reference to a table not in the query"
    in
    Alcotest.(check bool)
      (Printf.sprintf "the message names the shape and the issue (got %S)" msg)
      true
      (contains msg "correlated subquery" && contains msg "#615"))
;;

(* An unaliased self-join: two inputs, one scope identifier. Refused for the
   same reason [get_outer_scan_metas] refuses one, and the joined-row rebasing
   applies the same duplicate check across the two inputs. *)
let an_unaliased_self_join_is_refused () =
  with_db (fun db ->
    seed db;
    ignore
      (refused
         db
         "SELECT l.a FROM l LEFT JOIN l ON EXISTS (SELECT 1 FROM k WHERE v < l.a)"
         ~label:"unaliased self-join in a LEFT ON"))
;;

(* …and the aliased one resolves, because #635 gives the two inputs distinct
   identifiers. [x.a = 9] is the only left row whose subquery matches, so it
   pairs with both [y] rows and the other left row null-extends.
   sqlite3 answers [1|NULL], [9|1] and [9|9]. *)
let an_aliased_self_join_resolves () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"two aliases over one table disambiguate the correlation"
      [ [ "1"; "NULL" ]; [ "9"; "1" ]; [ "9"; "9" ] ]
      (rows_of
         db
         "SELECT x.a, y.a FROM l AS x LEFT JOIN l AS y ON EXISTS (SELECT 1 FROM k WHERE \
          v < x.a)"))
;;

(* ------------------------------------------------------------------ *)
(* Controls — the pure arm and the shapes #552/#539 pinned              *)
(* ------------------------------------------------------------------ *)

(* An UNCORRELATED ON subquery is resolved once, before the pairing loop, and
   takes the pure arm. It was already correct under #566 and must stay so. *)
let an_uncorrelated_on_subquery_is_untouched () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"an uncorrelated ON subquery still matches"
      [ [ "1"; "5" ]; [ "9"; "5" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b > (SELECT v FROM k)");
    check_rows
      ~label:"and one that matches nothing null-extends"
      [ [ "1"; "NULL" ]; [ "9"; "NULL" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b > (SELECT v FROM k WHERE v > 99)"))
;;

(* No subquery at all: the #539 shape, which must keep taking the pure loop.
   sqlite3 answers [1|5] and [9|NULL]. *)
let a_plain_general_on_is_pure () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"the #539 shape still null-extends"
      [ [ "1"; "5" ]; [ "9"; "NULL" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b > a"))
;;

let () =
  Alcotest.run
    "outer_join_subquery_615"
    [ ( "a correlated ON subquery in an outer join answers (#615)"
      , [ Alcotest.test_case "the issue's repro" `Quick outer_spelling_now_answers
        ; Alcotest.test_case "the two spellings agree" `Quick the_two_spellings_agree
        ; Alcotest.test_case
            "every spelling of the outer reference"
            `Quick
            every_spelling_of_the_outer_reference
        ; Alcotest.test_case "EXISTS and IN spellings" `Quick exists_and_in_spellings
        ; Alcotest.test_case
            "the correlation source is the right input"
            `Quick
            the_correlation_source_is_the_right_input
        ; Alcotest.test_case
            "a correlated ON that never matches still null-extends"
            `Quick
            a_correlated_on_that_never_matches_still_null_extends
        ] )
    ; ( "what cannot be resolved is still refused"
      , [ Alcotest.test_case
            "an unresolvable reference is still refused"
            `Quick
            an_unresolvable_reference_is_still_refused
        ; Alcotest.test_case
            "an unaliased self-join is refused"
            `Quick
            an_unaliased_self_join_is_refused
        ; Alcotest.test_case
            "an aliased self-join resolves"
            `Quick
            an_aliased_self_join_resolves
        ] )
    ; ( "controls"
      , [ Alcotest.test_case
            "an uncorrelated ON subquery is untouched"
            `Quick
            an_uncorrelated_on_subquery_is_untouched
        ; Alcotest.test_case
            "a plain general ON is pure"
            `Quick
            a_plain_general_on_is_pure
        ] )
    ]
;;
