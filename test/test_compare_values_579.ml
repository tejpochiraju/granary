(** #579: [Exec.compare_values] is a TOTAL order, so ordering a mixed-type
    column is defined.

    {1 The defect}

    [compare_values] ended in

    {[
      | _, _ -> 0 (* cross-type: shouldn't happen *)
    ]}

    and it does happen.  Strict column typing keeps a {e stored} column
    single-typed — [INSERT INTO m (v REAL) VALUES (3)] is refused — but a
    {e computed} column is unconstrained per row, so
    [CASE WHEN i = 0 THEN f ELSE i END] mixes INTEGERs and REALs freely.

    Every such pair compared {b equal}, which made the relation
    {b non-transitive}: [1 = 2.5] and [2.5 = 3] while [1 < 3].  [List.sort] on a
    non-transitive comparator has no defined result, so the issue's repro came
    back {e in scan order, entirely unsorted}, with no error.

    That comparator is behind ORDER BY (via [compare_with_nulls]), GROUP BY
    (which sorts and then groups adjacent runs, so {e which} rows land in
    {e which} group was input-order-dependent too), window PARTITION BY, and
    MIN/MAX (which became first-wins).

    Meanwhile [cmp_result] — the WHERE-predicate comparator — has always
    promoted [int]/[real] through [Float.compare].  The engine held two answers
    for the same pair and the {e ordering} one was the wrong one.

    {1 The rule now}

    - within the numeric class, compare EXACTLY via [Exec.cmp_int_real] — never
      by promoting the int64 to a float, which rounds above 2^53 and would
      leave the comparator non-transitive one magnitude up;
    - across storage classes, order NULL < number < TEXT < BLOB, which is
      SQLite's documented order {e and} the order of
      [Index_key.encode_value]'s tag bytes.

    {1 What this does NOT claim}

    The total order is [compare_values]'s, not the engine's.  [cmp_result], the
    WHERE-predicate comparator, still differs in two ways, and both are filed
    rather than fixed here:

    - it promotes int-vs-real through [Int64.to_float], so above 2^53 a
      predicate still answers equal for a pair this ordering separates (#733);
    - it answers false for {e every} cross-class comparison, so [WHERE] says
      [5 < 'abc'] is false while [ORDER BY] now sorts [5] before ['abc']
      (#734).

    So [the_filter_and_the_sort_now_agree] below is named for the numeric class
    and is pinned only there; [the_filter_and_the_sort_still_disagree_across_classes]
    pins the boundary of that claim so it is not mistaken for a general one.

    {1 Oracle}

    Every expectation is taken from the [sqlite3] in the dev image and quoted at
    its test.  The one thing NOT oracle-checked against sqlite3 is NaN, because
    sqlite3 has no NaN at all (it binds one as NULL); that case follows #536's
    decided order instead, which CLAUDE.md records. *)

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

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%a" Db.pp_error e
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let render = function
  | Db.V_int n -> Int64.to_string n
  | Db.V_text s -> s
  | Db.V_real f -> Printf.sprintf "%g" f
  | Db.V_blob b -> Bytes.to_string b
  | Db.V_null -> "NULL"
;;

(* Row order is the thing under test, so nothing here sorts the result. *)
let rows_of db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun r -> Array.to_list (Array.map render r))
      (run (Lwt_stream.to_list stream))
;;

let rows_of_params db sql params =
  unwrap
    (run
       (let open Lwt.Syntax in
        let* st = Db.prepare db sql in
        match st with
        | Error e -> Lwt.return (Error e)
        | Ok st ->
          let* r = Db.iter st ~params in
          (match r with
           | Error e -> Lwt.return (Error e)
           | Ok stream ->
             let* rows = Lwt_stream.to_list stream in
             Lwt.return (Ok (List.map (fun r -> Array.to_list (Array.map render r)) rows)))))
;;

let check_rows ~label expected actual =
  Alcotest.(check (list (list string))) label expected actual
;;

(* The issue's fixture, verbatim. [v] is INTEGER on two rows and REAL on two,
   and the four values interleave numerically — 1, 1.5, 2.5, 3 — so a
   comparator that answers 0 across the two types cannot order them at all. *)
let seed db =
  exec db "CREATE TABLE n (i INTEGER, f REAL)";
  exec db "INSERT INTO n (i,f) VALUES (3, 0.0)";
  exec db "INSERT INTO n (i,f) VALUES (0, 1.5)";
  exec db "INSERT INTO n (i,f) VALUES (1, 0.0)";
  exec db "INSERT INTO n (i,f) VALUES (0, 2.5)"
;;

let mixed = "CASE WHEN i = 0 THEN f ELSE i END"

(* sqlite3:
     SELECT CASE WHEN i = 0 THEN f ELSE i END AS v FROM n ORDER BY v ASC;
     1
     1.5
     2.5
     3

   On main this returned 3 / 1.5 / 1 / 2.5 — the scan order, untouched, because
   every comparison answered 0. *)
let the_issues_repro_is_ordered () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"mixed INTEGER/REAL column, ascending"
      [ [ "1" ]; [ "1.5" ]; [ "2.5" ]; [ "3" ] ]
      (rows_of db (Printf.sprintf "SELECT %s AS v FROM n ORDER BY v ASC" mixed)))
;;

(* sqlite3: 3 / 2.5 / 1.5 / 1 *)
let descending_too () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"mixed INTEGER/REAL column, descending"
      [ [ "3" ]; [ "2.5" ]; [ "1.5" ]; [ "1" ] ]
      (rows_of db (Printf.sprintf "SELECT %s AS v FROM n ORDER BY v DESC" mixed)))
;;

(* The property that actually failed, stated directly: a non-transitive
   comparator makes [List.sort]'s result a function of the INPUT order.  The
   same four values inserted in a different order therefore came out
   differently — which is what "no defined result" means in practice, and what
   an assertion on one fixed input order alone would not catch. *)
let the_order_no_longer_depends_on_the_insertion_order () =
  let expected = [ [ "1" ]; [ "1.5" ]; [ "2.5" ]; [ "3" ] ] in
  let inserts =
    [ [ "(3, 0.0)"; "(0, 1.5)"; "(1, 0.0)"; "(0, 2.5)" ]
    ; [ "(0, 2.5)"; "(1, 0.0)"; "(0, 1.5)"; "(3, 0.0)" ]
    ; [ "(0, 1.5)"; "(3, 0.0)"; "(0, 2.5)"; "(1, 0.0)" ]
    ; [ "(1, 0.0)"; "(0, 2.5)"; "(3, 0.0)"; "(0, 1.5)" ]
    ]
  in
  List.iteri
    (fun k rows ->
       with_db (fun db ->
         exec db "CREATE TABLE n (i INTEGER, f REAL)";
         List.iter (fun r -> exec db ("INSERT INTO n (i,f) VALUES " ^ r)) rows;
         check_rows
           ~label:(Printf.sprintf "insertion order %d gives the same order" k)
           expected
           (rows_of db (Printf.sprintf "SELECT %s AS v FROM n ORDER BY v ASC" mixed))))
    inserts
;;

(* MIN/MAX use the same comparator and became first-wins under it.

   sqlite3:
     SELECT MIN(CASE WHEN i = 0 THEN f ELSE i END),
            MAX(CASE WHEN i = 0 THEN f ELSE i END) FROM n;
     1|3 *)
let min_and_max_over_a_mixed_column () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"MIN and MAX are numeric across the two types"
      [ [ "1"; "3" ] ]
      (rows_of db (Printf.sprintf "SELECT MIN(%s), MAX(%s) FROM n" mixed mixed)))
;;

(* Cross-CLASS ordering, which the catch-all also answered 0 for. sqlite3:

     CREATE TABLE m (k INTEGER);
     INSERT INTO m VALUES (1),(2),(3);
     SELECT CASE k WHEN 1 THEN 5 WHEN 2 THEN 'abc' ELSE X'01' END AS v
       FROM m ORDER BY v ASC;
     5
     abc
     <the blob, one 0x01 byte>

   i.e. number < TEXT < BLOB, which is SQLite's documented storage-class order
   and also the order of Index_key.encode_value's tag bytes. *)
let numbers_then_text_then_blobs () =
  with_db (fun db ->
    exec db "CREATE TABLE m (k INTEGER)";
    exec db "INSERT INTO m VALUES (1),(2),(3)";
    check_rows
      ~label:"number < text < blob"
      [ [ "5" ]; [ "abc" ]; [ "\001" ] ]
      (rows_of
         db
         "SELECT CASE k WHEN 1 THEN 5 WHEN 2 THEN 'abc' ELSE X'01' END AS v FROM m ORDER \
          BY v ASC"))
;;

(* NULLs still sort first, which the class ranking must not have disturbed —
   they are a lower class than every value, so both the old explicit arms and
   the new ranking agree.

   sqlite3:
     SELECT CASE k WHEN 1 THEN NULL WHEN 2 THEN 2.5 ELSE 1 END AS v
       FROM m ORDER BY v ASC;
     (null)
     1
     2.5 *)
let nulls_still_sort_first () =
  with_db (fun db ->
    exec db "CREATE TABLE m (k INTEGER)";
    exec db "INSERT INTO m VALUES (1),(2),(3)";
    check_rows
      ~label:"NULL below every value, and the rest numerically ordered"
      [ [ "NULL" ]; [ "1" ]; [ "2.5" ] ]
      (rows_of
         db
         "SELECT CASE k WHEN 1 THEN NULL WHEN 2 THEN 2.5 ELSE 1 END AS v FROM m ORDER BY \
          v ASC"))
;;

(* NaN, the one case with no sqlite3 oracle — sqlite3 has no NaN, it binds one
   as NULL. #536 decided granary keeps it as a value with
   [NULL < NaN < every number], and CLAUDE.md records that.

   This case is newly REACHABLE by the fix: the promotion arm compares a NaN
   against an INTEGER, which the old catch-all answered 0 for.
   [Float.compare] puts NaN below [neg_infinity], so NaN sorts below every
   integer as well as below every real — which is what keeps #536's order
   whole rather than true only of reals.

   A NaN cannot be written as SQL, so it arrives as a bound parameter. *)
let nan_sorts_below_every_number_including_integers () =
  with_db (fun db ->
    exec db "CREATE TABLE m (k INTEGER)";
    exec db "INSERT INTO m VALUES (1),(2),(3)";
    check_rows
      ~label:"NaN below the INTEGER 1 and the REAL 2.5"
      [ [ "nan" ]; [ "1" ]; [ "2.5" ] ]
      (rows_of_params
         db
         "SELECT CASE k WHEN 1 THEN ? WHEN 2 THEN 2.5 ELSE 1 END AS v FROM m ORDER BY v \
          ASC"
         [ Db.V_real Float.nan ]))
;;

(* The reason this is a fix and not a re-decision: [cmp_result], the
   WHERE-predicate comparator, has ALWAYS promoted int/real numerically. Before
   #579 a filter and a sort over the same column disagreed. This pins that they
   now answer the same question the same way.

   sqlite3: 1 / 1.5 for the filtered form. *)
let the_filter_and_the_sort_now_agree () =
  with_db (fun db ->
    seed db;
    let sql =
      Printf.sprintf "SELECT %s AS v FROM n WHERE %s < 2 ORDER BY v ASC" mixed mixed
    in
    check_rows
      ~label:"WHERE v < 2 keeps exactly the prefix ORDER BY v puts first"
      [ [ "1" ]; [ "1.5" ] ]
      (rows_of db sql))
;;

(* ------------------------------------------------------------------ *)
(* Above 2^53: the boundary the first revision of the fix got wrong      *)
(* ------------------------------------------------------------------ *)

(* [Int64.to_float] rounds to nearest, so two distinct int64s promote to the
   SAME float above 2^53.  A comparator that promotes therefore answers 0 for
   two pairs it must separate, and is still non-transitive:

     9007199254740993L  vs 9007199254740992.0  ->  0
     9007199254740992.0 vs 9007199254740992L   ->  0
     9007199254740993L  vs 9007199254740992L   ->  1

   which is #579's own defect one magnitude up.  [cmp_int_real] compares
   exactly instead, so this sorts.

   sqlite3, same query on the same fixture:
     9.00719925474099e+15
     9007199254740992
     9007199254740993
   i.e. the REAL, then the equal INTEGER, then the larger one.  Granary agrees
   on the ORDER, which is this test's subject; the REAL renders as
   [9.0072e+15] because this file's [render] uses [%g], a property of the test
   harness and not of the engine. *)
let above_two_pow_53_is_still_ordered () =
  with_db (fun db ->
    exec db "CREATE TABLE big (i INTEGER, f REAL)";
    exec
      db
      "INSERT INTO big (i,f) VALUES (9007199254740993, 0.0),(0, \
       9007199254740992.0),(9007199254740992, 0.0)";
    check_rows
      ~label:"the REAL, then the equal INTEGER, then the larger INTEGER"
      [ [ "9.0072e+15" ]; [ "9007199254740992" ]; [ "9007199254740993" ] ]
      (rows_of db "SELECT CASE WHEN i = 0 THEN f ELSE i END AS v FROM big ORDER BY v ASC"))
;;

(* ------------------------------------------------------------------ *)
(* The boundary of "the filter and the sort agree"                      *)
(* ------------------------------------------------------------------ *)

(* [the_filter_and_the_sort_now_agree] is true WITHIN the numeric class and
   nowhere else.  [cmp_result] ends in [| _ -> Row.V_int 0L], so every
   cross-class predicate is false, while [compare_values] now orders by class.
   Pinned so the narrower claim is not read as a general one — the residual is
   #734.

   sqlite3 answers 1 for [5 < 'abc'], so granary's predicate is the wrong half
   of the disagreement, not its ordering. *)
let the_filter_and_the_sort_still_disagree_across_classes () =
  with_db (fun db ->
    exec db "CREATE TABLE m (k INTEGER)";
    exec db "INSERT INTO m VALUES (1),(2)";
    let mixed = "CASE k WHEN 1 THEN 5 ELSE 'abc' END" in
    check_rows
      ~label:"ORDER BY puts the number before the text (compare_values)"
      [ [ "5" ]; [ "abc" ] ]
      (rows_of db (Printf.sprintf "SELECT %s AS v FROM m ORDER BY v ASC" mixed));
    check_rows
      ~label:"but the predicate answers false in both directions (#734)"
      [ [ "0"; "0"; "0" ] ]
      (rows_of db "SELECT 5 < 'abc', 5 > 'abc', 5 = 'abc'"))
;;

(* ------------------------------------------------------------------ *)
(* [compare_values] is also an EQUALITY test, at nine call sites        *)
(* ------------------------------------------------------------------ *)

(* Recorded so nobody finds it by bisect, per this repo's convention for an
   unremarked improvement.  [compare_values] is not only an ordering function:
   nine call sites read [compare_values a b = 0] as "equal"/"unchanged", and on
   [main] the cross-class catch-all made EVERY cross-class pair satisfy that.

   Two of the nine are FK correctness rather than cosmetics:
   [check_fk_parent_update_restrict]'s [unchanged] fast path and its deferred
   twin treated a parent key moving from one storage class to another as
   "nothing changed" and skipped the child probe; [fk_child_has_ref*]'s match
   tests counted a child row of a different class as a live reference.  Both are
   now right.

   sqlite3 answers 0 and 'no' for these two. *)
let cross_class_equality_is_no_longer_true () =
  with_db (fun db ->
    check_rows
      ~label:"IN over a different class no longer matches (was 1)"
      [ [ "0" ] ]
      (rows_of db "SELECT 1 IN ('abc')");
    check_rows
      ~label:"CASE over a different class no longer matches (was 'matched')"
      [ [ "no" ] ]
      (rows_of db "SELECT CASE 1 WHEN 'abc' THEN 'matched' ELSE 'no' END"))
;;

(* The comparator under test, directly.  Reaching it through SQL cannot
   generate the magnitudes that matter. *)
let cmp = Granary_sql.Exec.compare_values

(* The three comparisons the review of #579 identified, pinned directly rather
   than left to the fuzzer to rediscover.  Under the promoting comparator these
   read 0 / 0 / 1, which is a non-transitive relation and therefore undefined
   behaviour for [List.sort]:

     9007199254740993L  vs 9007199254740992.0
     9007199254740992.0 vs 9007199254740992L
     9007199254740993L  vs 9007199254740992L

   sqlite3 agrees with the exact answers: [SELECT 9007199254740993 =
   9007199254740992.0] is 0 and [>] is 1 there. *)
let the_falsifying_triple_is_ordered_exactly () =
  let a = Db.V_int 9007199254740993L
  and b = Db.V_real 9007199254740992.0
  and c = Db.V_int 9007199254740992L in
  Alcotest.(check int) "the larger int64 is above the float it rounds to" 1 (cmp a b);
  Alcotest.(check int) "the float equals the int64 it is exactly" 0 (cmp b c);
  Alcotest.(check int) "and the two int64s were never equal" 1 (cmp a c)
;;

(* ------------------------------------------------------------------ *)
(* QCheck: the property #579's Note asks for                            *)
(* ------------------------------------------------------------------ *)

(* "worth a QCheck property (sort a mixed int/real list two ways, assert the
   same result) since the current defect is invisible to any test whose input
   happens to be sorted."

   Two properties, over the comparator directly rather than through SQL, so the
   generator can reach the magnitudes that matter:

   - TRANSITIVITY over random triples, which is the property [List.sort]
     actually requires and the one both revisions of this fix broke;
   - SORT STABILITY under shuffling, which is the observable symptom.

   The generator deliberately includes values above 2^53 and the exact
   boundary, since that is where promotion fails and where a hand-written
   fixture will not go on its own. *)
let gen_value =
  let open QCheck in
  (* WEIGHTED, not uniform, and the weights are load-bearing.  A uniform draw
     over these eight branches does NOT catch the promoting comparator: the
     falsifying triple needs an int64, a float and a second int64 all within a
     few ULPs of 2^53, which a uniform generator reaches about once in 4x10^5
     draws.  Verified by mutation — with promotion restored and uniform
     weights, 20 000 cases pass.  With these weights it falsifies in the low
     hundreds. *)
  Gen.oneof_weighted
    [ (* The magnitudes where [Int64.to_float] rounds: a narrow pool so the
         same values recur and collide.  At 2^53 the float spacing is 2, so
         [2^53 + 1] and [2^53] promote to the same float while comparing
         unequal as int64s — that IS the falsifying pair. *)
      ( 6
      , Gen.map
          (fun d -> Db.V_int (Int64.add 9007199254740992L (Int64.of_int d)))
          (Gen.int_range (-2) 2) )
    ; ( 6
      , Gen.map
          (fun d -> Db.V_real (9007199254740992.0 +. (2.0 *. float_of_int d)))
          (Gen.int_range (-2) 2) )
    ; 1, Gen.map (fun n -> Db.V_int (Int64.of_int n)) (Gen.int_range (-1000) 1000)
    ; 1, Gen.map (fun f -> Db.V_real f) (Gen.float_range (-1000.) 1000.)
    ; 1, Gen.return (Db.V_real Float.nan)
    ; 1, Gen.return (Db.V_text "abc")
    ; 1, Gen.return (Db.V_blob (Bytes.of_string "\x01"))
    ; 1, Gen.return Db.V_null
    ]
;;

let prop_transitive =
  QCheck.Test.make
    ~count:20_000
    ~name:"compare_values is transitive over random triples"
    (QCheck.make (QCheck.Gen.triple gen_value gen_value gen_value))
    (fun (a, b, c) ->
       let ab = cmp a b
       and bc = cmp b c
       and ac = cmp a c in
       (* a <= b and b <= c implies a <= c, for every combination of strict and
          non-strict, which is what a total order requires and what the
          promoting comparator violated above 2^53. *)
       if ab <= 0 && bc <= 0
       then ac <= 0
       else if ab >= 0 && bc >= 0
       then ac >= 0
       else true)
;;

let prop_sort_is_order_independent =
  QCheck.Test.make
    ~count:5_000
    ~name:"sorting does not depend on the input order"
    (QCheck.make (QCheck.Gen.list_size (QCheck.Gen.int_range 2 12) gen_value))
    (fun vs ->
       let sorted = List.sort cmp vs in
       let shuffled = List.sort cmp (List.rev vs) in
       List.for_all2 (fun a b -> cmp a b = 0) sorted shuffled)
;;

let () =
  Alcotest.run
    "test_compare_values_579"
    [ ( "mixed_numeric_column"
      , [ Alcotest.test_case
            "the_issues_repro_is_ordered"
            `Quick
            the_issues_repro_is_ordered
        ; Alcotest.test_case "descending_too" `Quick descending_too
        ; Alcotest.test_case
            "the_order_no_longer_depends_on_the_insertion_order"
            `Quick
            the_order_no_longer_depends_on_the_insertion_order
        ; Alcotest.test_case
            "min_and_max_over_a_mixed_column"
            `Quick
            min_and_max_over_a_mixed_column
        ; Alcotest.test_case
            "the_filter_and_the_sort_now_agree"
            `Quick
            the_filter_and_the_sort_now_agree
        ; Alcotest.test_case
            "above_two_pow_53_is_still_ordered"
            `Quick
            above_two_pow_53_is_still_ordered
        ] )
    ; ( "the_boundary_of_the_claim"
      , [ Alcotest.test_case
            "the_filter_and_the_sort_still_disagree_across_classes"
            `Quick
            the_filter_and_the_sort_still_disagree_across_classes
        ; Alcotest.test_case
            "cross_class_equality_is_no_longer_true"
            `Quick
            cross_class_equality_is_no_longer_true
        ; Alcotest.test_case
            "the_falsifying_triple_is_ordered_exactly"
            `Quick
            the_falsifying_triple_is_ordered_exactly
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_transitive; prop_sort_is_order_independent ] )
    ; ( "cross_class_order"
      , [ Alcotest.test_case
            "numbers_then_text_then_blobs"
            `Quick
            numbers_then_text_then_blobs
        ; Alcotest.test_case "nulls_still_sort_first" `Quick nulls_still_sort_first
        ; Alcotest.test_case
            "nan_sorts_below_every_number_including_integers"
            `Quick
            nan_sorts_below_every_number_including_integers
        ] )
    ]
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5"]
[@@@ai_provider "Anthropic"]
