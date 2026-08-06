(** #626: an unresolvable correlated subquery in a PROJECTION was answered
    [NULL] where the same shape in a WHERE or an ON clause was refused.

    {1 The defect}

    [stream_expr_project] kept a non-raising fallback when
    [get_outer_scan_metas] answered [None]: the correlated [P_subquery] survived
    into [eval_expr], which answers [Row.V_null] for it, and the query returned
    a column of plausible NULLs. [stream_filter] refused the identical shape,
    naming #592.

    A NULL there is indistinguishable from a legitimately-NULL aggregate — the
    issue's own repro has one genuinely-NULL row — so the caller has no way to
    tell "no matching rows" from "the engine could not resolve this
    correlation". That is the #592 failure mode wearing a different hat: not
    zero rows, but a column of plausible NULLs.

    {1 The fix}

    Both of the issue's directions, because #635 makes the second one cheap:

    - resolvable correlations in a projection are {b resolved}, including the
      self-join-with-aliases shape the issue's repro uses (#635 gave the leaf
      scans their alias, so two inputs over one table no longer collide);
    - what is left unresolvable is a {b raise}, with a message naming #626 —
      the same treatment [stream_filter] gives it.

    A surviving subquery is also refused {i after} substitution, not only when
    the correlation source could not be located at all: an outer reference that
    names no input, or an ambiguous one, is left in place by the substitution
    and must not then be evaluated to NULL.

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

(* The issue's repro needs a table joined to itself under two aliases and a
   second table to aggregate over. *)
let seed db =
  exec db "CREATE TABLE t (a INTEGER)";
  exec db "CREATE TABLE s (a INTEGER, z INTEGER)";
  exec db "CREATE TABLE k (v INTEGER)";
  exec db "INSERT INTO t VALUES (1),(2),(3)";
  exec db "INSERT INTO s VALUES (2,3),(2,4),(3,8)";
  exec db "INSERT INTO k VALUES (4)"
;;

(* ------------------------------------------------------------------ *)
(* The repro                                                            *)
(* ------------------------------------------------------------------ *)

(* The issue's query, character for character. sqlite3 3.45.1 answers
   [1|NULL], [2|7], [3|8]; granary answered [1|NULL], [2|NULL], [3|NULL] — three
   plausible NULLs, one of which is even correct.

   Two things have to hold for this to pass, and they are the issue's two
   proposed directions taken together: the self-join's two inputs must be told
   apart (#635's alias on the leaf scan), and the projection must then evaluate
   the correlation rather than fall back. *)
let the_issues_repro_answers_instead_of_nulling () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"#626 repro: a correlated SUM in a projection over a self-join"
      [ [ "1"; "NULL" ]; [ "2"; "7" ]; [ "3"; "8" ] ]
      (rows_of
         db
         "SELECT x.a, (SELECT SUM(z) FROM s WHERE s.a = x.a) FROM t x JOIN t y ON x.a = \
          y.a"))
;;

(* The genuinely-NULL row is the point of the issue: after the fix a NULL in
   this column means "no matching rows" and nothing else. Asserted on its own so
   a future change that turned every unresolvable case into a NULL again would
   not be able to hide behind the row that is legitimately NULL. *)
let a_null_here_now_means_exactly_one_thing () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"an empty correlated aggregate is still NULL"
      [ [ "1"; "NULL" ] ]
      (rows_of db "SELECT a, (SELECT SUM(z) FROM s WHERE s.a = t.a) FROM t WHERE a = 1");
    check_rows
      ~label:"and a non-empty one is not"
      [ [ "3"; "8" ] ]
      (rows_of db "SELECT a, (SELECT SUM(z) FROM s WHERE s.a = t.a) FROM t WHERE a = 3"))
;;

(* ------------------------------------------------------------------ *)
(* What cannot be resolved is refused, in a projection too              *)
(* ------------------------------------------------------------------ *)

let refused_naming_626 db sql ~label =
  let msg = err_of db sql in
  Alcotest.(check bool)
    (Printf.sprintf "%s: refused rather than answered NULL (got %S)" label msg)
    true
    (msg <> "");
  msg
;;

(* An outer reference to a table the query does not contain. Before #626 this
   projected NULL for every row. *)
let an_unresolvable_reference_is_refused () =
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE zz (q INTEGER)";
    let msg =
      refused_naming_626
        db
        "SELECT a, (SELECT COUNT(*) FROM k WHERE v < zz.q) FROM t"
        ~label:"reference to a table not in the query"
    in
    Alcotest.(check bool)
      (Printf.sprintf "the message names the shape (got %S)" msg)
      true
      (contains msg "correlated subquery"))
;;

(* An UNALIASED self-join: two inputs, one identifier, so the correlation source
   cannot be located at all and [get_outer_scan_metas] answers [None]. This is
   the arm the fallback used to take. *)
let an_unaliased_self_join_projection_is_refused () =
  with_db (fun db ->
    seed db;
    let msg =
      refused_naming_626
        db
        "SELECT t.a, (SELECT SUM(z) FROM s WHERE s.a = t.a) FROM t JOIN t ON 1 = 1"
        ~label:"unaliased self-join in a projection"
    in
    Alcotest.(check bool)
      (Printf.sprintf "the message names the issue (got %S)" msg)
      true
      (contains msg "#626"))
;;

(* An AMBIGUOUS unqualified outer reference: both inputs carry [a], so the
   substitution leaves it and the subquery survives. The refusal has to fire on
   the surviving subquery, not only on a missing correlation source — those are
   two different arms. *)
let an_ambiguous_reference_is_refused () =
  with_db (fun db ->
    exec db "CREATE TABLE p (a INTEGER)";
    exec db "CREATE TABLE q (a INTEGER)";
    exec db "CREATE TABLE k2 (v INTEGER)";
    exec db "INSERT INTO p VALUES (1)";
    exec db "INSERT INTO q VALUES (2)";
    exec db "INSERT INTO k2 VALUES (4)";
    ignore
      (refused_naming_626
         db
         "SELECT p.a, (SELECT COUNT(*) FROM k2 WHERE v < a) FROM p JOIN q ON 1 = 1"
         ~label:"ambiguous bare `a` in a projection"))
;;

(* The asymmetry the issue is about, asserted directly: the same unresolvable
   correlation must be refused in a projection and in a WHERE. Before #626 the
   first answered and the second raised. *)
let the_projection_and_the_filter_now_agree () =
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE zz (q INTEGER)";
    let proj = err_of db "SELECT a, (SELECT COUNT(*) FROM k WHERE v < zz.q) FROM t" in
    let filt =
      err_of db "SELECT a FROM t WHERE (SELECT COUNT(*) FROM k WHERE v < zz.q) > 0"
    in
    Alcotest.(check bool)
      (Printf.sprintf "projection refuses (got %S)" proj)
      true
      (proj <> "");
    Alcotest.(check bool) (Printf.sprintf "filter refuses (got %S)" filt) true (filt <> ""))
;;

(* ------------------------------------------------------------------ *)
(* Controls — shapes that were already right must stay right            *)
(* ------------------------------------------------------------------ *)

(* An UNCORRELATED subquery in a projection is resolved once by
   [pre_eval_subquery] and never reaches the correlated arm at all. It must not
   be caught by the new refusal. *)
let an_uncorrelated_projection_subquery_is_untouched () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"an uncorrelated projected subquery still answers"
      [ [ "1"; "3" ]; [ "2"; "3" ]; [ "3"; "3" ] ]
      (rows_of db "SELECT a, (SELECT COUNT(*) FROM s) FROM t"))
;;

(* A projection with no subquery at all takes the pure, non-Lwt arm. *)
let a_plain_projection_is_untouched () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"a plain expression projection is unaffected"
      [ [ "2" ]; [ "3" ]; [ "4" ] ]
      (rows_of db "SELECT a + 1 FROM t"))
;;

(* The single-table correlated projection — the #485 comment's shape — was
   already correct and stays correct. *)
let the_single_table_correlated_projection_still_answers () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"single-table outer, correlated projection"
      [ [ "1"; "NULL" ]; [ "2"; "7" ]; [ "3"; "8" ] ]
      (rows_of db "SELECT a, (SELECT SUM(z) FROM s WHERE s.a = t.a) FROM t"))
;;

let () =
  Alcotest.run
    "projection_subquery_626"
    [ ( "a correlated projection subquery is resolved (#626)"
      , [ Alcotest.test_case
            "the issue's repro answers instead of NULLing"
            `Quick
            the_issues_repro_answers_instead_of_nulling
        ; Alcotest.test_case
            "a NULL here now means exactly one thing"
            `Quick
            a_null_here_now_means_exactly_one_thing
        ] )
    ; ( "what cannot be resolved is refused"
      , [ Alcotest.test_case
            "an unresolvable reference is refused"
            `Quick
            an_unresolvable_reference_is_refused
        ; Alcotest.test_case
            "an unaliased self-join projection is refused"
            `Quick
            an_unaliased_self_join_projection_is_refused
        ; Alcotest.test_case
            "an ambiguous reference is refused"
            `Quick
            an_ambiguous_reference_is_refused
        ; Alcotest.test_case
            "the projection and the filter now agree"
            `Quick
            the_projection_and_the_filter_now_agree
        ] )
    ; ( "controls"
      , [ Alcotest.test_case
            "an uncorrelated projection subquery is untouched"
            `Quick
            an_uncorrelated_projection_subquery_is_untouched
        ; Alcotest.test_case
            "a plain projection is untouched"
            `Quick
            a_plain_projection_is_untouched
        ; Alcotest.test_case
            "the single-table correlated projection still answers"
            `Quick
            the_single_table_correlated_projection_still_answers
        ] )
    ]
;;
