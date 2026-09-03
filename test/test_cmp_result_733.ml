(** #733 / #734: [Exec.cmp_result], the WHERE-predicate comparator, is
    [Exec.compare_values] plus three-valued NULL handling — so a WHERE clause
    and an ORDER BY can no longer disagree about where a value sits.

    {1 The two defects}

    #579 made [compare_values] a total order (exact int-vs-real via
    [cmp_int_real]; NULL < number < TEXT < BLOB via [value_class_rank]) but left
    [cmp_result] alone, so the engine held two answers for the same pair:

    - [cmp_result] promoted int-vs-real through [Int64.to_float], which rounds,
      so above 2^53 the PREDICATE answered equal for pairs the ORDERING
      separated. [9007199254740993 > 9007199254740992.0] was 0 (#733).
    - it ended in a catch-all returning false, applying no cross-CLASS order at
      all, so ORDER BY sorted [5] before ['abc'] while WHERE said [5 < 'abc']
      was false (#734).

    {1 Oracle}

    Every expectation is taken from the [sqlite3] in the dev image; both defects
    were divergences from it, not merely internal inconsistencies. The
    exceptions, both flagged at their tests, are NaN — sqlite3 has none, binding
    one as NULL — and the [=] / [<>] cross-NUMERIC hole this change deliberately
    leaves in place.

    {1 The index path}

    The riskiest part of #733 is that [Exec.range_bound_key]'s [pred]/[succ]
    widening was written to compensate for the INEXACT predicate. Making the
    predicate exact makes it NARROWER than the seek, which is safe only while a
    residual filter runs over every row the seek yields —
    [Planner.range_for_index] never marks a range conjunct consumed, so one
    does. [seeked_and_unseekable_agree_above_two_pow_53] pins that end to end:
    the same predicate through an index seek and through an unoptimizable foil
    ([o + 0], which no access path recognises) must give the same rows. Two of
    its four cases have a seek that yields a key the residual then rejects,
    which is the over-approximation working as intended. *)

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
  | Db.V_real f -> Printf.sprintf "%g" f
  | Db.V_blob b -> Bytes.to_string b
  | Db.V_null -> "NULL"
;;

let rows_of db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun r -> Array.to_list (Array.map render r))
      (run (Lwt_stream.to_list stream))
;;

let check_rows ~label expected actual =
  Alcotest.(check (list (list string))) label expected actual
;;

(* [a < b, a <= b, a > b, a >= b] in one row. Every #733 case is phrased over
   all four, because a fix that got only the strict operators right would pass a
   test of [<] alone. *)
let order_ops = [ "<"; "<="; ">"; ">=" ]

(* The same, plus [=] and [<>]. Used only for cross-CLASS pairs: those two
   operators have their own arms in [eval_binop] and are correct across classes
   but NOT across the numeric types — see
   [cross_numeric_equality_is_unchanged_pending_738]. *)
let all_ops = order_ops @ [ "="; "<>" ]

let select_ops ops a b =
  let one op = Printf.sprintf "%s %s %s" a op b in
  "SELECT " ^ String.concat ", " (List.map one ops)
;;

(* A bare SELECT of the comparisons: no table, so nothing about affinities or
   access paths can be mistaken for the comparator. *)
let check_ops ~label ~ops a b expected =
  with_db (fun db -> check_rows ~label [ expected ] (rows_of db (select_ops ops a b)))
;;

(* ------------------------------------------------------------------ *)
(* #733 — exact int-vs-real above 2^53                                  *)
(* ------------------------------------------------------------------ *)

(* The issue's own repro. At 2^53 the float spacing is 2, so [2^53 + 1] and
   [2^53] promote to the same float — the old predicate therefore called them
   equal and every ordering operator answered from that.

   sqlite3:
     SELECT 9007199254740993 <  9007199254740992.0,
            9007199254740993 <= 9007199254740992.0,
            9007199254740993 >  9007199254740992.0,
            9007199254740993 >= 9007199254740992.0;
     0|0|1|1

   On main this was 0|1|0|1 — the "equal" answers of a rounded comparison. *)
let the_issues_repro_is_exact () =
  check_ops
    ~label:"2^53 + 1 is strictly above 2^53 as a float"
    ~ops:order_ops
    "9007199254740993"
    "9007199254740992.0"
    [ "0"; "0"; "1"; "1" ]
;;

(* The float on the LEFT, which took the other arm of the old promotion and so
   is not covered by the case above.

   sqlite3: 1|1|0|0 *)
let the_reversed_operand_order_is_exact_too () =
  check_ops
    ~label:"real on the left"
    ~ops:order_ops
    "9007199254740992.0"
    "9007199254740993"
    [ "1"; "1"; "0"; "0" ]
;;

(* An int64 that IS exactly representable, so the exact and the promoting
   comparators agree — the control for every case around it, and the pair that
   keeps [<=] / [>=] from being read as blanket-true.

   sqlite3: 0|1|0|1 *)
let an_exactly_representable_pair_is_equal () =
  check_ops
    ~label:"2^53 against itself as a float"
    ~ops:order_ops
    "9007199254740992"
    "9007199254740992.0"
    [ "0"; "1"; "0"; "1" ]
;;

(* Rounding is to NEAREST, so it errs in both directions: [2^53 + 3] rounds UP
   onto [2^53 + 4], and [2^62 + 100] rounds DOWN onto [2^62]. A fix that only
   handled one direction would pass the cases above.

   sqlite3, in order:
     SELECT 9007199254740995 < 9007199254740996.0;      1
     SELECT 4611686018427387804 < 4611686018427387904.0; 1
     SELECT 4611686018427388004 > 4611686018427387904.0; 1 *)
let rounding_up_is_exact () =
  check_ops
    ~label:"2^53 + 3 rounds up onto 2^53 + 4 but is below it"
    ~ops:order_ops
    "9007199254740995"
    "9007199254740996.0"
    [ "1"; "1"; "0"; "0" ]
;;

let rounding_down_is_exact () =
  check_ops
    ~label:"2^62 + 100 rounds down onto 2^62 but is above it"
    ~ops:order_ops
    "4611686018427388004"
    "4611686018427387904.0"
    [ "0"; "0"; "1"; "1" ]
;;

let rounding_up_at_two_pow_62_is_exact () =
  check_ops
    ~label:"2^62 - 100 rounds up onto 2^62 but is below it"
    ~ops:order_ops
    "4611686018427387804"
    "4611686018427387904.0"
    [ "1"; "1"; "0"; "0" ]
;;

(* Negative magnitudes take the same arms with the signs reversed.

   sqlite3:
     SELECT -9007199254740993 < -9007199254740992.0;  1
     SELECT -9007199254740993 > -9007199254740992.0;  0 *)
let negative_magnitudes_are_exact () =
  check_ops
    ~label:"below zero, 2^53 + 1 is still separated from 2^53"
    ~ops:order_ops
    "-9007199254740993"
    "-9007199254740992.0"
    [ "1"; "1"; "0"; "0" ]
;;

(* The int64 edge, where [cmp_int_real] decides by RANGE before converting
   anything: [Int64.max_int] is below 2^63 as a float, and [Int64.of_float] is
   unspecified at or above it.

   sqlite3:
     SELECT 9223372036854775807 < 9.2233720368547758e18;  1
     SELECT 9223372036854775807 > 9.2233720368547758e18;  0 *)
let the_int64_edge () =
  check_ops
    ~label:"int64 max sits below 2^63 as a float"
    ~ops:order_ops
    "9223372036854775807"
    "9.2233720368547758e18"
    [ "1"; "1"; "0"; "0" ]
;;

(* The infinities, decided by the same range test. [9e999] overflows to
   [infinity] in both engines.

   sqlite3: SELECT 1 < 9e999;  1   SELECT 1 > -9e999;  1 *)
let positive_infinity_is_above_every_integer () =
  check_ops
    ~label:"every integer is below +inf"
    ~ops:order_ops
    "1"
    "9e999"
    [ "1"; "1"; "0"; "0" ]
;;

let negative_infinity_is_below_every_integer () =
  check_ops
    ~label:"every integer is above -inf"
    ~ops:order_ops
    "1"
    "-9e999"
    [ "0"; "0"; "1"; "1" ]
;;

(* ------------------------------------------------------------------ *)
(* #733 — the index path, which is why this was its own issue           *)
(* ------------------------------------------------------------------ *)

(* The five-key grid [test_range_bound_517] uses, chosen so that each stored key
   and its float image straddle the bound. [w] pins the index prefix, so the
   bound on [o] becomes a real range seek; [o + 0] is the unoptimizable foil —
   no access path recognises it, so it is a plain filtered scan of the same
   predicate.

   [range_bound_key] rounds a cross-type bound OUTWARD and then widens it by one
   float step. That widening was written against the INEXACT predicate: without
   it, [ceil]/[floor] sought past keys that qualified under rounding and dropped
   their rows. Since #733 the predicate is exact and the widening is a
   deliberate over-approximation — the seek yields keys the residual then
   rejects. All four cases below do exactly that: each seek visits one key more
   than it returns, which is what makes this a test of the seek/residual
   contract rather than of the comparator again. The examined counts that pin
   the widening itself live in [test_range_bound_517].

   The expectations are sqlite3's, over the same five rows:
     o >= 4611686018427387904.0                          -> 4
     o <= 9007199254740992.0                             -> 0
     o >= 9007199254740996.0                             -> 3, 4
     o BETWEEN 9007199254740992.0 AND 4611686018427387904.0
                                                         -> 0, 1, 2, 3

   On main the first returned 3 and 4 and the second returned 0 and 1 — the
   extra row in each is exactly the key whose ROUNDED image met the bound. *)
let seeked_and_unseekable_agree_above_two_pow_53 () =
  with_db (fun db ->
    exec db "CREATE TABLE g (w INTEGER, o INTEGER, v INTEGER, PRIMARY KEY (w, o))";
    List.iteri
      (fun i o -> exec db (Printf.sprintf "INSERT INTO g VALUES (1, %s, %d)" o i))
      [ "9007199254740992"
      ; "9007199254740993"
      ; "9007199254740995"
      ; "4611686018427387804"
      ; "4611686018427388004"
      ];
    let check ~label ~pred expected =
      let bounded = Printf.sprintf "SELECT v FROM g WHERE w = 1 AND o %s" pred in
      let foil = Printf.sprintf "SELECT v FROM g WHERE w = 1 AND o + 0 %s" pred in
      check_rows ~label:(label ^ " (seeked)") expected (rows_of db bounded);
      check_rows ~label:(label ^ " (foil)") expected (rows_of db foil)
    in
    check ~label:"lower bound at 2^62" ~pred:">= 4611686018427387904.0" [ [ "4" ] ];
    check ~label:"upper bound at 2^53" ~pred:"<= 9007199254740992.0" [ [ "0" ] ];
    check
      ~label:"lower bound at 2^53 + 4"
      ~pred:">= 9007199254740996.0"
      [ [ "3" ]; [ "4" ] ];
    check
      ~label:"both ends"
      ~pred:"BETWEEN 9007199254740992.0 AND 4611686018427387904.0"
      [ [ "0" ]; [ "1" ]; [ "2" ]; [ "3" ] ])
;;

(* The mirror direction — an INTEGER bound on a REAL column, where
   [range_bound_key] widens for a different reason: the bound has to be encoded
   as a float and [Int64.to_float] can land on the wrong side of it. That arm is
   untouched by #733, and this is the check that an exact residual has not made
   it unsound.

   The bound 9007199254740993 promotes DOWN to 9007199254740992.0, so the lower
   end needs no widening (rounding down is already outward for a lower bound)
   and the upper end takes the [succ] step. The two stored reals bracket the
   bound, so an inward slip either way would move a row. *)
let an_integer_bound_on_a_real_column_still_agrees () =
  with_db (fun db ->
    exec db "CREATE TABLE r (w INTEGER, x REAL, v INTEGER, PRIMARY KEY (w, x))";
    exec db "INSERT INTO r VALUES (1, 9007199254740992.0, 0)";
    exec db "INSERT INTO r VALUES (1, 9007199254740996.0, 1)";
    let check ~label ~pred expected =
      check_rows
        ~label:(label ^ " (seeked)")
        expected
        (rows_of db (Printf.sprintf "SELECT v FROM r WHERE w = 1 AND x %s" pred));
      check_rows
        ~label:(label ^ " (foil)")
        expected
        (rows_of db (Printf.sprintf "SELECT v FROM r WHERE w = 1 AND x + 0 %s" pred))
    in
    check ~label:"lower bound" ~pred:">= 9007199254740993" [ [ "1" ] ];
    check ~label:"upper bound" ~pred:"<= 9007199254740993" [ [ "0" ] ])
;;

(* ------------------------------------------------------------------ *)
(* #734 — the cross-class order                                         *)
(* ------------------------------------------------------------------ *)

(* NUMBER < TEXT < BLOB: [value_class_rank]'s order, SQLite's documented
   storage-class order, and the order of [Index_key.encode_value]'s tag bytes.
   Every [<], [<=], [>], [>=] and [<>] below answered 0 on main.

   sqlite3, for each pair:
     5     vs 'abc'   ->  <1  <=1  >0  >=0  =0  <>1
     'abc' vs X'01'   ->  <1  <=1  >0  >=0  =0  <>1
     5     vs X'01'   ->  <1  <=1  >0  >=0  =0  <>1 *)
let a_number_is_below_a_text () =
  check_ops
    ~label:"number < text"
    ~ops:all_ops
    "5"
    "'abc'"
    [ "1"; "1"; "0"; "0"; "0"; "1" ]
;;

let a_text_is_below_a_blob () =
  check_ops
    ~label:"text < blob"
    ~ops:all_ops
    "'abc'"
    "X'01'"
    [ "1"; "1"; "0"; "0"; "0"; "1" ]
;;

let a_number_is_below_a_blob () =
  check_ops
    ~label:"number < blob"
    ~ops:all_ops
    "5"
    "X'01'"
    [ "1"; "1"; "0"; "0"; "0"; "1" ]
;;

(* The operands the other way round — a different arm of the match, and the
   direction a one-sided fix would miss.

   sqlite3, for each pair:  <0  <=0  >1  >=1  =0  <>1 *)
let the_reverse_direction_agrees () =
  let expected = [ "0"; "0"; "1"; "1"; "0"; "1" ] in
  check_ops ~label:"text > number" ~ops:all_ops "'abc'" "5" expected;
  check_ops ~label:"blob > text" ~ops:all_ops "X'01'" "'abc'" expected;
  check_ops ~label:"blob > number" ~ops:all_ops "X'01'" "5" expected
;;

(* [=] was already right — different classes are never equal, and sqlite3 agrees
   — but [<>] shared its catch-all and was therefore ALSO false, so [5 = 'abc']
   and [5 <> 'abc'] were both 0. That is not so much a divergence as an
   incoherence, and it is the one part of #734 outside [cmp_result]: no access
   path recognises a [<>] conjunct, so this half moved with no index-side
   counterpart to move with it. *)
let inequality_is_now_true_where_equality_is_false () =
  with_db (fun db ->
    check_rows
      ~label:"never equal, therefore always different"
      [ [ "0"; "1"; "0"; "1"; "0"; "1" ] ]
      (rows_of
         db
         "SELECT 5 = 'abc', 5 <> 'abc', 'abc' = X'01', 'abc' <> X'01', X'01' = 5, X'01' \
          <> 5"))
;;

(* The disagreement #734 named, from both sides at once. The ORDER BY half is
   [test_compare_values_579]'s [numbers_then_text_then_blobs] fixture; the WHERE
   half is what used to contradict it, and is now the same order. *)
let the_filter_and_the_sort_now_agree_across_classes () =
  with_db (fun db ->
    exec db "CREATE TABLE m (k INTEGER)";
    exec db "INSERT INTO m VALUES (1),(2),(3)";
    let mixed = "CASE k WHEN 1 THEN 5 WHEN 2 THEN 'abc' ELSE X'01' END" in
    check_rows
      ~label:"ORDER BY: number, then text, then blob"
      [ [ "5" ]; [ "abc" ]; [ "\001" ] ]
      (rows_of db (Printf.sprintf "SELECT %s AS v FROM m ORDER BY v ASC" mixed));
    check_rows
      ~label:"and the predicate now says the same thing"
      [ [ "1"; "1"; "1" ] ]
      (rows_of db "SELECT 5 < 'abc', 'abc' < X'01', 5 < X'01'"))
;;

(* A cross-class bound over an INDEXED column. [range_bound_key] declines a text
   bound on an integer column, so that end of the seek is left OPEN and every
   row reaches the residual — which is exactly the "declining is always sound"
   claim its doc comment makes, now that declining changes the answer instead of
   being invisible behind a predicate that rejected everything anyway.

   sqlite3, over i in {1,2,3}:
     SELECT i FROM c WHERE i < 'abc';          1, 2, 3
     SELECT i FROM c WHERE i > 'abc';          (none)
     SELECT count( * ) FROM c WHERE i <> 'abc';  3 *)
let a_cross_class_bound_leaves_the_seek_open () =
  with_db (fun db ->
    exec db "CREATE TABLE c (w INTEGER, i INTEGER, PRIMARY KEY (w, i))";
    exec db "INSERT INTO c VALUES (1,1),(1,2),(1,3)";
    let all = [ [ "1" ]; [ "2" ]; [ "3" ] ] in
    check_rows
      ~label:"every integer is below every text (seeked)"
      all
      (rows_of db "SELECT i FROM c WHERE w = 1 AND i < 'abc'");
    check_rows
      ~label:"every integer is below every text (foil)"
      all
      (rows_of db "SELECT i FROM c WHERE w = 1 AND i + 0 < 'abc'");
    check_rows
      ~label:"and none is above one"
      []
      (rows_of db "SELECT i FROM c WHERE w = 1 AND i > 'abc'");
    check_rows
      ~label:"nor equal to one, though all differ from it"
      []
      (rows_of db "SELECT i FROM c WHERE w = 1 AND i = 'abc'");
    check_rows
      ~label:"all differ from it"
      all
      (rows_of db "SELECT i FROM c WHERE w = 1 AND i <> 'abc'"))
;;

(* ------------------------------------------------------------------ *)
(* What did NOT change                                                  *)
(* ------------------------------------------------------------------ *)

(* NULL keeps its own arm ABOVE the delegation, and must: [compare_values]
   ORDERS NULL below everything, because a total order has to answer something,
   whereas a predicate over a NULL is UNKNOWN. Routing NULL through it would
   make [WHERE x < 5] true for a NULL [x].

   sqlite3 returns NULL for all four. *)
let null_is_still_unknown_not_lowest () =
  with_db (fun db ->
    check_rows
      ~label:"three-valued logic survives the delegation"
      [ [ "NULL"; "NULL"; "NULL"; "NULL" ] ]
      (rows_of db "SELECT NULL < 5, 5 < NULL, NULL = NULL, NULL <> 'abc'"))
;;

(* #536's decided order: NaN is a real value and sorts below every number, in
   BOTH the value comparator and [Index_key]'s tag bytes. It used to reach the
   predicate through [Float.compare]'s accident and now reaches it through
   [cmp_int_real]'s explicit rule — the same answer, by construction rather than
   by coincidence. The int-vs-NaN pair is the one that changed mechanism.

   NOT oracle-checked: sqlite3 has no NaN at all (it binds one as NULL, and a
   NaN expression is NULL), so [x >= NaN] there is unknown and returns nothing.
   This is the divergence CLAUDE.md records under #536. *)
let nan_still_sorts_below_every_number () =
  with_db (fun db ->
    check_rows
      ~label:"every number is above NaN, and none is below it"
      [ [ "1"; "0"; "1"; "0" ] ]
      (rows_of
         db
         "SELECT 1.0 > 0.0/0.0, 1.0 < 0.0/0.0, 9007199254740993 > 0.0/0.0, \
          9007199254740993 < 0.0/0.0"))
;;

(* The cross-NUMERIC hole in [=] and [<>], left deliberately. An equality
   conjunct IS consumed by the access path, so [Exec.index_lookup_values] and
   [Exec.stream_rowid_lookup] would have to learn the int-vs-real case in the
   same change or rows would be lost. Pinned so the divergence is a recorded
   decision rather than an oversight, and so #738 has a test to invert.

   sqlite3 answers 1|0|1 here; granary answers 0|0|0. *)
let cross_numeric_equality_is_unchanged_pending_738 () =
  with_db (fun db ->
    check_rows
      ~label:"1 = 1.0 and 1 <> 2.0 are both still false (#738)"
      [ [ "0"; "0"; "0" ] ]
      (rows_of db "SELECT 1 = 1.0, 1 <> 1.0, 1 <> 2.0"))
;;

(* The ordering operators over the SAME pairs are exact, which is what makes the
   case above a hole in [=] / [<>] specifically rather than a numeric gap in
   general.

   sqlite3: 0|0|1|1 *)
let the_ordering_operators_have_no_such_hole () =
  with_db (fun db ->
    check_rows
      ~label:"1 vs 1.0 and 1 vs 2.0, ordered exactly"
      [ [ "0"; "0"; "1"; "1" ] ]
      (rows_of db "SELECT 1 < 1.0, 1 > 1.0, 1 <= 1.0, 1 < 2.0"))
;;

let () =
  Alcotest.run
    "test_cmp_result_733"
    [ ( "exact_above_two_pow_53"
      , [ Alcotest.test_case "the_issues_repro_is_exact" `Quick the_issues_repro_is_exact
        ; Alcotest.test_case
            "the_reversed_operand_order_is_exact_too"
            `Quick
            the_reversed_operand_order_is_exact_too
        ; Alcotest.test_case
            "an_exactly_representable_pair_is_equal"
            `Quick
            an_exactly_representable_pair_is_equal
        ; Alcotest.test_case "rounding_up_is_exact" `Quick rounding_up_is_exact
        ; Alcotest.test_case "rounding_down_is_exact" `Quick rounding_down_is_exact
        ; Alcotest.test_case
            "rounding_up_at_two_pow_62_is_exact"
            `Quick
            rounding_up_at_two_pow_62_is_exact
        ; Alcotest.test_case
            "negative_magnitudes_are_exact"
            `Quick
            negative_magnitudes_are_exact
        ; Alcotest.test_case "the_int64_edge" `Quick the_int64_edge
        ; Alcotest.test_case
            "positive_infinity_is_above_every_integer"
            `Quick
            positive_infinity_is_above_every_integer
        ; Alcotest.test_case
            "negative_infinity_is_below_every_integer"
            `Quick
            negative_infinity_is_below_every_integer
        ] )
    ; ( "the_index_path"
      , [ Alcotest.test_case
            "seeked_and_unseekable_agree_above_two_pow_53"
            `Quick
            seeked_and_unseekable_agree_above_two_pow_53
        ; Alcotest.test_case
            "an_integer_bound_on_a_real_column_still_agrees"
            `Quick
            an_integer_bound_on_a_real_column_still_agrees
        ; Alcotest.test_case
            "a_cross_class_bound_leaves_the_seek_open"
            `Quick
            a_cross_class_bound_leaves_the_seek_open
        ] )
    ; ( "cross_class_order"
      , [ Alcotest.test_case "a_number_is_below_a_text" `Quick a_number_is_below_a_text
        ; Alcotest.test_case "a_text_is_below_a_blob" `Quick a_text_is_below_a_blob
        ; Alcotest.test_case "a_number_is_below_a_blob" `Quick a_number_is_below_a_blob
        ; Alcotest.test_case
            "the_reverse_direction_agrees"
            `Quick
            the_reverse_direction_agrees
        ; Alcotest.test_case
            "inequality_is_now_true_where_equality_is_false"
            `Quick
            inequality_is_now_true_where_equality_is_false
        ; Alcotest.test_case
            "the_filter_and_the_sort_now_agree_across_classes"
            `Quick
            the_filter_and_the_sort_now_agree_across_classes
        ] )
    ; ( "unchanged"
      , [ Alcotest.test_case
            "null_is_still_unknown_not_lowest"
            `Quick
            null_is_still_unknown_not_lowest
        ; Alcotest.test_case
            "nan_still_sorts_below_every_number"
            `Quick
            nan_still_sorts_below_every_number
        ; Alcotest.test_case
            "cross_numeric_equality_is_unchanged_pending_738"
            `Quick
            cross_numeric_equality_is_unchanged_pending_738
        ; Alcotest.test_case
            "the_ordering_operators_have_no_such_hole"
            `Quick
            the_ordering_operators_have_no_such_hole
        ] )
    ]
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5"]
[@@@ai_provider "Anthropic"]
