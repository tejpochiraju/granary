(** #721: a correlated outer reference inside an AGGREGATE or WINDOW argument is
    substituted, so the statement is answered instead of refused.

    {1 The defect}

    [Exec.substitute_outer_in_expr] is the Ast-level walker that rewrites a
    correlated subquery's outer column references into values drawn from the
    outer row. #670 made its match exhaustive, which turned an invisible
    [| _ -> e] catch-all into three constructors listed explicitly with
    [-> e]:

    - [E_agg of agg_func * expr option]
    - [E_agg_distinct of agg_func * expr]
    - [E_window of { func; args; window }]

    All three carry sub-expressions. An outer reference sitting inside one of
    them was therefore never rewritten; it survived the substitution, failed
    [Sema.bind] on the substituted statement, and the query came back as
    [correlated_filter_refusal] / [correlated_projection_refusal] — a refusal
    for a query the engine is perfectly able to run, which is the same shape of
    defect as #670 and #732 in a different node.

    #488 is what makes this reachable in the first place: an aggregate's
    argument became a general expression rather than a bare column, so
    [SUM(i.v + o.n)] is a legal spelling with an outer reference nested inside
    it.

    {1 Which spellings are reachable, and why the window ones arrive with #732}

    [E_agg] and [E_agg_distinct] are reachable on their own, in a correlated
    subquery's HAVING — the issue's own repro. [E_window] is not: a window
    function may not appear in WHERE or HAVING, so its only home is the
    subquery's PROJECTION, and the projection was not walked at all until #732
    (the sibling fix in the same commit). The two therefore land together, and
    the window cases below are as much a #732 test as a #721 one.

    {1 Oracle}

    Every expected value marked "oracle" was taken from the [sqlite3] 3.45.1 in
    the dev image, run on the same schema and data. The ones marked
    "hand-computed" are arithmetic over the fixture and are pinned here as
    granary's answer; they were derived rather than observed. *)

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

(* The refusals this issue is about are raised from inside a stream
   ([Lwt.fail_with] in [stream_filter] / [stream_expr_project]), so a failing
   query can come back either as [Error] from [Db.query] or as an exception
   during the drain (#627's documented residual). Both are "refused"; neither
   is a row. *)
let rows_of db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    (match run (Lwt_stream.to_list stream) with
     | rows ->
       List.sort compare (List.map (fun r -> Array.to_list (Array.map render r)) rows)
     | exception e ->
       Alcotest.failf "query %S raised during drain: %s" sql (Printexc.to_string e))
;;

let check_rows ~label expected actual =
  Alcotest.(check (list (list string))) label (List.sort compare expected) actual
;;

(* ------------------------------------------------------------------ *)
(* Fixture 1 — the issue's own shape.                                   *)
(*                                                                      *)
(*   o: a|1  b|2  c|3                                                   *)
(*   i: a|10 a|20 b|-30 c|-100 c|-100                                   *)
(*                                                                      *)
(* Chosen so that adding [o.n] to every [i.v] moves the aggregate by a  *)
(* DIFFERENT amount per outer row (the row counts differ: 2, 1, 2), so  *)
(* a substitution that pinned the wrong outer row's value cannot land   *)
(* on the right answer by accident.                                     *)
(* ------------------------------------------------------------------ *)
let seed db =
  exec db "CREATE TABLE o (k TEXT, n INTEGER)";
  exec db "CREATE TABLE i (fk TEXT, v INTEGER)";
  exec db "INSERT INTO o VALUES ('a',1),('b',2),('c',3)";
  exec db "INSERT INTO i VALUES ('a',10),('a',20),('b',-30),('c',-100),('c',-100)"
;;

(* The issue's headline query. Before the fix, [o.n] inside [SUM(...)] was not
   substituted and this was refused.

   sqlite3, same schema and data (oracle):
     SELECT k FROM o WHERE EXISTS (SELECT 1 FROM i WHERE i.fk = o.k
       GROUP BY i.fk HAVING SUM(i.v + o.n) > 0) ORDER BY k;
     a *)
let an_aggregate_argument_in_a_correlated_having_is_substituted () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"outer ref inside SUM() in a correlated HAVING"
      [ [ "a" ] ]
      (rows_of
         db
         "SELECT k FROM o WHERE EXISTS (SELECT 1 FROM i WHERE i.fk = o.k GROUP BY i.fk \
          HAVING SUM(i.v + o.n) > 0)"))
;;

(* The value-level pin, and the one that makes the fix impossible to satisfy by
   accident: three outer rows, three different sums, each folding in ITS OWN
   [o.n].

   sqlite3 (oracle):
     SELECT k, (SELECT SUM(i.v + o.n) FROM i WHERE i.fk = o.k) FROM o ORDER BY k;
     a|32
     b|-28
     c|-194

   (Reached through the projection, so this exercises #732's clause walk as
   well; the aggregate descent is what supplies the [+ o.n].) *)
let an_aggregate_argument_in_a_projection_carries_the_outer_value () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"SUM(i.v + o.n) per outer row"
      [ [ "a"; "32" ]; [ "b"; "-28" ]; [ "c"; "-194" ] ]
      (rows_of db "SELECT k, (SELECT SUM(i.v + o.n) FROM i WHERE i.fk = o.k) FROM o"))
;;

(* The control for the test above, and the reason it is not vacuous: drop the
   outer reference from the aggregate's argument and every value changes.

   sqlite3 (oracle): a|30  b|-30  c|-200 *)
let the_same_aggregate_without_the_outer_reference_differs () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"SUM(i.v) — no outer reference in the argument"
      [ [ "a"; "30" ]; [ "b"; "-30" ]; [ "c"; "-200" ] ]
      (rows_of db "SELECT k, (SELECT SUM(i.v) FROM i WHERE i.fk = o.k) FROM o"))
;;

(* [E_agg_distinct] is a separate constructor, not a flag on [E_agg] (#491), so
   it needs its own arm and its own test.

   sqlite3 (oracle):
     SELECT k FROM o WHERE EXISTS (SELECT 1 FROM i WHERE i.fk = o.k
       GROUP BY i.fk HAVING COUNT(DISTINCT i.v + o.n) > 1) ORDER BY k;
     a *)
let a_distinct_aggregate_argument_in_a_correlated_having_is_substituted () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"outer ref inside COUNT(DISTINCT ...) in a correlated HAVING"
      [ [ "a" ] ]
      (rows_of
         db
         "SELECT k FROM o WHERE EXISTS (SELECT 1 FROM i WHERE i.fk = o.k GROUP BY i.fk \
          HAVING COUNT(DISTINCT i.v + o.n) > 1)"))
;;

(* The DISTINCT is load-bearing here: [c] has two identical [i.v] rows, so
   [SUM(DISTINCT i.v + o.n)] is -97 where the non-distinct [SUM(i.v + o.n)] two
   tests above is -194. That difference is what distinguishes the
   [E_agg_distinct] arm from the [E_agg] one.

   Hand-computed over the fixture (not observed from sqlite3):
     a: DISTINCT {10+1, 20+1} = {11,21}  -> 32
     b: DISTINCT {-30+2}      = {-28}    -> -28
     c: DISTINCT {-100+3}     = {-97}    -> -97 *)
let a_distinct_aggregate_in_a_projection_carries_the_outer_value () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"SUM(DISTINCT i.v + o.n) per outer row"
      [ [ "a"; "32" ]; [ "b"; "-28" ]; [ "c"; "-97" ] ]
      (rows_of
         db
         "SELECT k, (SELECT SUM(DISTINCT i.v + o.n) FROM i WHERE i.fk = o.k) FROM o"))
;;

(* [E_window]'s ARGUMENT list. A window function cannot appear in WHERE or
   HAVING, so this shape only became reachable once #732 taught
   [substitute_outer_in_stmt] to walk the projection.

   sqlite3 (oracle):
     SELECT k, (SELECT SUM(i.v + o.n) OVER () FROM i WHERE i.fk = o.k LIMIT 1)
       FROM o ORDER BY k;
     a|32
     b|-28
     c|-194 *)
let a_window_argument_carries_the_outer_value () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"SUM(i.v + o.n) OVER () per outer row"
      [ [ "a"; "32" ]; [ "b"; "-28" ]; [ "c"; "-194" ] ]
      (rows_of
         db
         "SELECT k, (SELECT SUM(i.v + o.n) OVER () FROM i WHERE i.fk = o.k LIMIT 1) \
          FROM o"))
;;

(* Its control. sqlite3 (oracle): a|30  b|-30  c|-200 *)
let the_same_window_without_the_outer_reference_differs () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"SUM(i.v) OVER () — no outer reference in the argument"
      [ [ "a"; "30" ]; [ "b"; "-30" ]; [ "c"; "-200" ] ]
      (rows_of
         db
         "SELECT k, (SELECT SUM(i.v) OVER () FROM i WHERE i.fk = o.k LIMIT 1) FROM o"))
;;

(* ------------------------------------------------------------------ *)
(* Fixture 2 — for [window_spec.partition_by].                          *)
(*                                                                      *)
(* Multiplying by [o2.n] is what makes the outer value change the       *)
(* PARTITIONING rather than merely shifting it: [q]'s [n] is 0, so both *)
(* of its rows collapse into one partition, while [p]'s do not.  Adding *)
(* a constant could never do that, which is why the fixture carries a   *)
(* zero.                                                                *)
(*                                                                      *)
(* The assertion is order-independent: within each outer row every      *)
(* input row has the same partition size, so which one the [LIMIT 1]    *)
(* picks does not matter.                                               *)
(* ------------------------------------------------------------------ *)
let seed2 db =
  exec db "CREATE TABLE o2 (k TEXT, n INTEGER)";
  exec db "CREATE TABLE i2 (fk TEXT, v INTEGER)";
  exec db "INSERT INTO o2 VALUES ('p',2),('q',0)";
  exec db "INSERT INTO i2 VALUES ('p',5),('p',7),('q',5),('q',7)"
;;

(* Hand-computed over fixture 2 (not observed from sqlite3):
     p (n=2): partitions {10}, {14} -> every row's COUNT(*) OVER is 1
     q (n=0): partition  {0}        -> every row's COUNT(*) OVER is 2 *)
let a_window_partition_by_carries_the_outer_value () =
  with_db (fun db ->
    seed2 db;
    check_rows
      ~label:"COUNT(*) OVER (PARTITION BY i2.v * o2.n)"
      [ [ "p"; "1" ]; [ "q"; "2" ] ]
      (rows_of
         db
         "SELECT k, (SELECT COUNT(*) OVER (PARTITION BY i2.v * o2.n) FROM i2 WHERE \
          i2.fk = o2.k LIMIT 1) FROM o2"))
;;

(* Its control: without the outer factor both outer rows partition by the two
   distinct [v]s, so [q] answers 1 rather than 2. Hand-computed. *)
let the_same_partition_by_without_the_outer_reference_differs () =
  with_db (fun db ->
    seed2 db;
    check_rows
      ~label:"COUNT(*) OVER (PARTITION BY i2.v)"
      [ [ "p"; "1" ]; [ "q"; "1" ] ]
      (rows_of
         db
         "SELECT k, (SELECT COUNT(*) OVER (PARTITION BY i2.v) FROM i2 WHERE i2.fk = \
          o2.k LIMIT 1) FROM o2"))
;;

(* ------------------------------------------------------------------ *)
(* Fixture 3 — for [window_spec.order_by].                              *)
(*                                                                      *)
(* [FIRST_VALUE] under the default frame (UNBOUNDED PRECEDING TO        *)
(* CURRENT ROW) answers the first row in WINDOW order, which is the     *)
(* same value for every row of the partition — so the assertion does    *)
(* not depend on which row the scalar subquery yields.  The outer [n]   *)
(* is +1 for one row and -1 for the other, which REVERSES the window    *)
(* order, so the two answers differ.                                    *)
(* ------------------------------------------------------------------ *)
let seed3 db =
  exec db "CREATE TABLE o3 (k TEXT, n INTEGER)";
  exec db "CREATE TABLE i3 (fk TEXT, v INTEGER)";
  exec db "INSERT INTO o3 VALUES ('up',1),('dn',-1)";
  exec db "INSERT INTO i3 VALUES ('up',1),('up',10),('dn',1),('dn',10)"
;;

(* Hand-computed over fixture 3 (not observed from sqlite3):
     up (n=1):  ORDER BY v*1  -> 1, 10 -> FIRST_VALUE = 1
     dn (n=-1): ORDER BY v*-1 -> 10, 1 -> FIRST_VALUE = 10 *)
let a_window_order_by_carries_the_outer_value () =
  with_db (fun db ->
    seed3 db;
    check_rows
      ~label:"FIRST_VALUE(i3.v) OVER (ORDER BY i3.v * o3.n)"
      [ [ "up"; "1" ]; [ "dn"; "10" ] ]
      (rows_of
         db
         "SELECT k, (SELECT FIRST_VALUE(i3.v) OVER (ORDER BY i3.v * o3.n) FROM i3 WHERE \
          i3.fk = o3.k) FROM o3"))
;;

(* Its control: without the outer factor the window order is ascending for both
   outer rows, so both answer 1. Hand-computed. *)
let the_same_window_order_by_without_the_outer_reference_differs () =
  with_db (fun db ->
    seed3 db;
    check_rows
      ~label:"FIRST_VALUE(i3.v) OVER (ORDER BY i3.v)"
      [ [ "up"; "1" ]; [ "dn"; "1" ] ]
      (rows_of
         db
         "SELECT k, (SELECT FIRST_VALUE(i3.v) OVER (ORDER BY i3.v) FROM i3 WHERE i3.fk \
          = o3.k) FROM o3"))
;;

(* The boundary this fix must not have moved: an aggregate over the subquery's
   OWN column is not an outer reference and must keep binding to the inner row.
   [inner_scope_of] is what decides that, and the descent added here runs
   underneath it — so if the scope were consulted wrongly this would start
   answering the outer row's [n] instead of the inner [v]s.

   sqlite3 (oracle): a|30  b|-30  c|-200 for the uncorrelated spelling below,
   which pins [i.fk = 'a'] rather than [i.fk = o.k]:
     SELECT k, (SELECT SUM(i.v) FROM i WHERE i.fk = 'a') FROM o -> 30 for every
   outer row. *)
let an_uncorrelated_aggregate_subquery_is_unchanged () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"no correlation: the same value for every outer row"
      [ [ "a"; "30" ]; [ "b"; "30" ]; [ "c"; "30" ] ]
      (rows_of db "SELECT k, (SELECT SUM(i.v) FROM i WHERE i.fk = 'a') FROM o"))
;;

let () =
  Alcotest.run
    "outer_ref_721"
    [ ( "E_agg"
      , [ Alcotest.test_case
            "aggregate argument in a correlated HAVING"
            `Quick
            an_aggregate_argument_in_a_correlated_having_is_substituted
        ; Alcotest.test_case
            "aggregate argument in a projection"
            `Quick
            an_aggregate_argument_in_a_projection_carries_the_outer_value
        ; Alcotest.test_case
            "control: no outer reference in the argument"
            `Quick
            the_same_aggregate_without_the_outer_reference_differs
        ] )
    ; ( "E_agg_distinct"
      , [ Alcotest.test_case
            "DISTINCT aggregate argument in a correlated HAVING"
            `Quick
            a_distinct_aggregate_argument_in_a_correlated_having_is_substituted
        ; Alcotest.test_case
            "DISTINCT aggregate argument in a projection"
            `Quick
            a_distinct_aggregate_in_a_projection_carries_the_outer_value
        ] )
    ; ( "E_window"
      , [ Alcotest.test_case
            "window argument"
            `Quick
            a_window_argument_carries_the_outer_value
        ; Alcotest.test_case
            "control: window argument without an outer reference"
            `Quick
            the_same_window_without_the_outer_reference_differs
        ; Alcotest.test_case
            "window PARTITION BY"
            `Quick
            a_window_partition_by_carries_the_outer_value
        ; Alcotest.test_case
            "control: PARTITION BY without an outer reference"
            `Quick
            the_same_partition_by_without_the_outer_reference_differs
        ; Alcotest.test_case
            "window ORDER BY"
            `Quick
            a_window_order_by_carries_the_outer_value
        ; Alcotest.test_case
            "control: window ORDER BY without an outer reference"
            `Quick
            the_same_window_order_by_without_the_outer_reference_differs
        ] )
    ; ( "boundary"
      , [ Alcotest.test_case
            "an uncorrelated aggregate subquery is unchanged"
            `Quick
            an_uncorrelated_aggregate_subquery_is_unchanged
        ] )
    ]
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5"]
[@@@ai_provider "Anthropic"]
