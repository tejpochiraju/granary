(** #738: [=] and [<>] compare the numeric class EXACTLY, and the three index
    equality sites move with them.

    {1 The defect}

    [Exec.eval_binop]'s [Plan.Eq] and [Plan.Ne] arms did not go through
    [Exec.cmp_result]. Each enumerated the four same-type pairs and fell to a
    catch-all, so a cross-NUMERIC pair answered false in BOTH operators:
    [1 = 1.0] was 0 and [1 <> 1.0] was 0. That is incoherent on its own, and it
    contradicted the ordering operators, which since #733 answer [1 <= 1.0] and
    [1 >= 1.0] both true.

    {1 Why it could not be fixed in [eval_binop] alone}

    Unlike a range conjunct, an EQUALITY conjunct is {i consumed} by the access
    path: [Planner.recognise_eq_col_lit] feeds [access_path_for_eqs], and
    [Planner.residual_filter] removes the consumed positions, so no residual
    predicate re-checks the rows a seek yields. Three sites decide which rows an
    equality seek covers, and all three answered "matches nothing" for a
    cross-numeric pair:

    - [Exec.index_lookup_values] (read and write index paths);
    - [Exec.stream_rowid_lookup] (the [INTEGER PRIMARY KEY] read path);
    - [Exec.seek_candidates]'s [Seek_rowid] arm (the same, for DML).

    Making [=] exact without moving them would have made [WHERE i = 1.0] on an
    indexed INTEGER column return nothing while the predicate says the row
    qualifies — rows lost silently. The [*_seeks_*] cases below are the point of
    this file: each runs the seeking spelling {i and} an unoptimizable foil
    ([col + 0], which no recogniser matches) and requires them to agree, and
    asserts [used_index] so a planner change cannot turn the seek into a scan
    and make the agreement vacuous.

    {1 Oracle}

    Every expectation is [sqlite3]'s, checked in the dev image on 2026-09-03,
    except the NaN cases: sqlite3 has no NaN at all, so those follow #536's
    decided order (NaN is a real value below every number) and are flagged
    where they appear.

    {1 What #738 left open, since closed}

    A JOIN KEY equality is consumed by a different mechanism —
    [Exec.stream_hash_join]'s keyed arm and [Exec.nlj_probe_left] — and #738
    left both behind, so for one release [ON l.a = r.b] and
    [ON 1 = 1 WHERE l.a = r.b] disagreed. That is #743, fixed 2026-09-03 by
    moving both executors at once; [a_cross_numeric_join_key_matches_now_743]
    below keeps the shape #738 filed, and [test/test_join_key_743.ml] carries
    the per-executor coverage. *)

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

let change_count db sql =
  match run (Db.execute_change_count db sql) with
  | Ok n -> n
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

(* Rows plus whether an index/rowid seek was actually used, so a "the seek
   agrees with the filter" assertion cannot pass by the planner quietly having
   chosen a scan. *)
let rows_and_index db sql =
  match run (Db.query_with_stats db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok (stream, stats) ->
    let rows =
      List.map
        (fun r -> Array.to_list (Array.map render r))
        (run (Lwt_stream.to_list stream))
    in
    rows, stats.Db.used_index
;;

let rows_of_param db sql params =
  match
    run
      (let open Lwt.Syntax in
       let* st = Db.prepare db sql in
       match st with
       | Error e -> Lwt.return (Error e)
       | Ok st -> Db.iter st ~params)
  with
  | Error e -> Alcotest.failf "prepared %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun r -> Array.to_list (Array.map render r))
      (run (Lwt_stream.to_list stream))
;;

let check_rows ~label expected actual =
  Alcotest.(check (list (list string))) label expected actual
;;

(* ------------------------------------------------------------------ *)
(* The value level                                                      *)
(* ------------------------------------------------------------------ *)

(* The issue's own repro. sqlite3: 1|0|1|0 *)
let cross_numeric_equality_is_exact () =
  with_db (fun db ->
    check_rows
      ~label:"1 = 1.0 is true, and <> agrees with it"
      [ [ "1"; "0"; "1"; "0" ] ]
      (rows_of db "SELECT 1 = 1.0, 1 <> 1.0, 1 <> 2.0, 1 = 2.0"))
;;

(* [=] must not be exact by PROMOTING through [Int64.to_float] — that rounds, so
   above 2^53 two distinct int64s promote to one float and [=] would answer
   true for a pair [>] separates. #733 made the ordering operators exact via
   [cmp_int_real]; [=] now shares that function rather than a second rule.

   sqlite3: 0|1|1|1|0 *)
let above_two_pow_53_equality_stays_exact () =
  with_db (fun db ->
    check_rows
      ~label:"9007199254740993 <> 9007199254740992.0, but 2^53 itself is equal"
      [ [ "0"; "1"; "1"; "1"; "0" ] ]
      (rows_of
         db
         "SELECT 9007199254740993 = 9007199254740992.0, 9007199254740993 <> \
          9007199254740992.0, 9007199254740993 > 9007199254740992.0, 9007199254740992 = \
          9007199254740992.0, 9007199254740992 <> 9007199254740992.0"))
;;

(* The cross-CLASS answers #734 settled are untouched: different storage classes
   are never equal, so [=] is false and [<>] is true.

   sqlite3: 0|1|0|1|0|1 *)
let cross_class_equality_is_unchanged () =
  with_db (fun db ->
    check_rows
      ~label:"number/text, real/text and text/blob"
      [ [ "0"; "1"; "0"; "1"; "0"; "1" ] ]
      (rows_of
         db
         "SELECT 1 = 'abc', 1 <> 'abc', 1.0 = 'abc', 1.0 <> 'abc', 'a' = x'61', 'a' <> \
          x'61'"))
;;

(* The NULL arm stays ABOVE the delegation to [compare_values], which ORDERS
   NULL below everything. A predicate over a NULL is UNKNOWN, in [=] and [<>]
   exactly as in [<].

   sqlite3 returns NULL for all four. *)
let null_is_still_unknown () =
  with_db (fun db ->
    check_rows
      ~label:"= and <> against NULL are UNKNOWN"
      [ [ "NULL"; "NULL"; "NULL"; "NULL" ] ]
      (rows_of db "SELECT 1 = NULL, 1 <> NULL, NULL = NULL, NULL <> NULL"))
;;

(* #536: NaN is a real value here, below every number, where sqlite3 has no NaN
   at all — so these four are a DIVERGENCE and are not oracle-checked.

   Three of them are unchanged by #738 ([Float.equal nan nan] is true, and it is
   [Float.compare nan nan = 0] under the hood, so the same-type arm answers the
   same thing through [compare_values]). The one that MOVED is [1 <> NaN],
   which was false — the cross-numeric catch-all — while [1 = NaN] was also
   false. That was the same incoherence as [1 <> 1.0], and it is fixed by the
   same change rather than by a NaN rule. *)
let nan_equality_follows_the_decided_order () =
  with_db (fun db ->
    check_rows
      ~label:"NaN = NaN, and NaN differs from every number"
      [ [ "1"; "0"; "0"; "1" ] ]
      (rows_of
         db
         "SELECT 0.0/0.0 = 0.0/0.0, 0.0/0.0 <> 0.0/0.0, 1 = 0.0/0.0, 1 <> 0.0/0.0"))
;;

(* ------------------------------------------------------------------ *)
(* The seek and the residual agree — the point of the issue             *)
(* ------------------------------------------------------------------ *)

let seed_indexed db =
  exec db "CREATE TABLE t (i INTEGER, r REAL, tag TEXT)";
  exec db "CREATE INDEX ti ON t (i)";
  exec db "CREATE INDEX tr ON t (r)";
  exec db "INSERT INTO t VALUES (1, 1.0, 'one')";
  exec db "INSERT INTO t VALUES (2, 2.0, 'two')";
  exec db "INSERT INTO t VALUES (3, 3.5, 'three')"
;;

(* The headline: an indexed INTEGER column, probed with a REAL literal. The
   equality conjunct is consumed, so the seek IS the answer — before #738 this
   returned no rows while [WHERE i + 0 = 1.0] (unconsumed, filtered) returned
   the row.

   sqlite3 returns the row (its affinity stores 1.0 as an integer; granary keeps
   the column strictly typed and reaches the same answer through an exact
   comparison instead). *)
let an_indexed_integer_column_seeks_for_a_real_literal () =
  with_db (fun db ->
    seed_indexed db;
    let seeked, used = rows_and_index db "SELECT tag FROM t WHERE i = 1.0" in
    Alcotest.(check bool) "the seek was actually taken" true used;
    check_rows ~label:"seeked" [ [ "one" ] ] seeked;
    check_rows
      ~label:"the unoptimizable foil agrees"
      [ [ "one" ] ]
      (rows_of db "SELECT tag FROM t WHERE i + 0 = 1.0"))
;;

(* [None] from [index_lookup_values] still means "matches nothing", and for a
   FRACTIONAL real on an integer column that is exactly right: no integer equals
   1.5. The foil proves the seek is not merely losing the row. sqlite3: 0 rows. *)
let a_fractional_real_matches_no_integer () =
  with_db (fun db ->
    seed_indexed db;
    let seeked, used = rows_and_index db "SELECT tag FROM t WHERE i = 1.5" in
    Alcotest.(check bool) "the seek was actually taken" true used;
    check_rows ~label:"seeked" [] seeked;
    check_rows
      ~label:"the unoptimizable foil agrees"
      []
      (rows_of db "SELECT tag FROM t WHERE i + 0 = 1.5"))
;;

(* The mirror direction: an INTEGER literal against an indexed REAL column.
   sqlite3 returns the row. *)
let an_indexed_real_column_seeks_for_an_integer_literal () =
  with_db (fun db ->
    seed_indexed db;
    let seeked, used = rows_and_index db "SELECT tag FROM t WHERE r = 2" in
    Alcotest.(check bool) "the seek was actually taken" true used;
    check_rows ~label:"seeked" [ [ "two" ] ] seeked;
    check_rows
      ~label:"the unoptimizable foil agrees"
      [ [ "two" ] ]
      (rows_of db "SELECT tag FROM t WHERE r + 0 = 2"))
;;

(* An int64 above 2^53 that no double names exactly must produce NO key, not the
   key of the nearest double — otherwise the seek would find the row 2^53 and
   the residual would reject it, or worse, the seek would answer a row the
   predicate calls unequal.

   sqlite3: 0 rows for the [= 2^53+1] probe, 1 row for [= 2^53]. *)
let an_integer_no_double_names_matches_nothing_on_a_real_column () =
  with_db (fun db ->
    exec db "CREATE TABLE h (r REAL, tag TEXT)";
    exec db "CREATE INDEX hr ON h (r)";
    exec db "INSERT INTO h VALUES (9007199254740992.0, 'big')";
    let miss, used_miss =
      rows_and_index db "SELECT tag FROM h WHERE r = 9007199254740993"
    in
    Alcotest.(check bool) "the seek was actually taken" true used_miss;
    check_rows ~label:"2^53+1 names no double" [] miss;
    check_rows
      ~label:"the unoptimizable foil agrees"
      []
      (rows_of db "SELECT tag FROM h WHERE r + 0 = 9007199254740993");
    let hit, used_hit =
      rows_and_index db "SELECT tag FROM h WHERE r = 9007199254740992"
    in
    Alcotest.(check bool) "the seek was actually taken" true used_hit;
    check_rows ~label:"2^53 does" [ [ "big" ] ] hit;
    check_rows
      ~label:"the unoptimizable foil agrees"
      [ [ "big" ] ]
      (rows_of db "SELECT tag FROM h WHERE r + 0 = 9007199254740992"))
;;

(* A NaN probe must not reach [index_lookup_values] as an "integral" real —
   [Float.trunc nan] is [nan] and [Float.equal nan nan] is true, so a naive
   integrality test would have passed it to [Int64.of_float], which is
   unspecified there. [int64_of_exact_real] declines a NaN first.

   A NaN cannot be written as a SQL literal, so this goes through a bound
   parameter — which is also the spelling [recognise_eq_col_lit] accepts, so the
   conjunct really is consumed and the seek really is taken.

   Divergence from sqlite3 (#536): it binds NaN as NULL and would answer no rows
   for a different reason. Granary answers no rows because no INTEGER equals a
   NaN. *)
let a_nan_parameter_matches_no_integer () =
  with_db (fun db ->
    seed_indexed db;
    check_rows
      ~label:"seeked with a NaN parameter"
      []
      (rows_of_param db "SELECT tag FROM t WHERE i = ?" [ Db.V_real Float.nan ]);
    check_rows
      ~label:"the unoptimizable foil agrees"
      []
      (rows_of_param db "SELECT tag FROM t WHERE i + 0 = ?" [ Db.V_real Float.nan ]);
    check_rows
      ~label:"and an infinity likewise"
      []
      (rows_of_param db "SELECT tag FROM t WHERE i = ?" [ Db.V_real Float.infinity ]))
;;

(* ------------------------------------------------------------------ *)
(* The rowid alias — no index, its own two code paths                   *)
(* ------------------------------------------------------------------ *)

let seed_pk db =
  exec db "CREATE TABLE p (id INTEGER PRIMARY KEY, tag TEXT)";
  exec db "INSERT INTO p VALUES (1, 'one')";
  exec db "INSERT INTO p VALUES (2, 'two')";
  exec db "INSERT INTO p VALUES (3, 'three')"
;;

(* [Exec.stream_rowid_lookup]: the alias column IS the table key, so a
   non-[V_int] probe used to return an empty stream outright. sqlite3 returns
   the row. *)
let the_rowid_alias_seeks_for_a_real_literal () =
  with_db (fun db ->
    seed_pk db;
    let seeked, used = rows_and_index db "SELECT tag FROM p WHERE id = 1.0" in
    Alcotest.(check bool) "the rowid seek was actually taken" true used;
    check_rows ~label:"seeked" [ [ "one" ] ] seeked;
    check_rows
      ~label:"the unoptimizable foil agrees"
      [ [ "one" ] ]
      (rows_of db "SELECT tag FROM p WHERE id + 0 = 1.0"))
;;

let a_fractional_real_matches_no_rowid () =
  with_db (fun db ->
    seed_pk db;
    let seeked, used = rows_and_index db "SELECT tag FROM p WHERE id = 1.5" in
    Alcotest.(check bool) "the rowid seek was actually taken" true used;
    check_rows ~label:"seeked" [] seeked;
    check_rows
      ~label:"the unoptimizable foil agrees"
      []
      (rows_of db "SELECT tag FROM p WHERE id + 0 = 1.5"))
;;

(* ------------------------------------------------------------------ *)
(* DML — [Exec.seek_candidates], the fourth site                        *)
(* ------------------------------------------------------------------ *)

(* The DML seek is only a RESTRICTION — the write path re-evaluates the whole
   WHERE predicate on every candidate — so a seek that declines a cross-numeric
   probe does not merely widen the scan, it drops the row before the predicate
   ever sees it. [DELETE ... WHERE id = 2.0] was a silent no-op. *)
let a_delete_on_the_rowid_alias_finds_a_real_literal () =
  with_db (fun db ->
    seed_pk db;
    Alcotest.(check int)
      "one row deleted"
      1
      (change_count db "DELETE FROM p WHERE id = 2.0");
    check_rows
      ~label:"and it was the right one"
      [ [ "one" ]; [ "three" ] ]
      (rows_of db "SELECT tag FROM p ORDER BY id");
    Alcotest.(check int)
      "a fractional rowid deletes nothing"
      0
      (change_count db "DELETE FROM p WHERE id = 1.5"))
;;

(* The same for the index arm of [seek_candidates]. *)
let an_update_through_an_index_finds_a_real_literal () =
  with_db (fun db ->
    seed_indexed db;
    Alcotest.(check int)
      "one row updated"
      1
      (change_count db "UPDATE t SET tag = 'ONE' WHERE i = 1.0");
    check_rows
      ~label:"and it was the right one"
      [ [ "ONE" ]; [ "two" ]; [ "three" ] ]
      (rows_of db "SELECT tag FROM t ORDER BY i");
    Alcotest.(check int)
      "a fractional probe updates nothing"
      0
      (change_count db "UPDATE t SET tag = 'X' WHERE i = 1.5"))
;;

(* ------------------------------------------------------------------ *)
(* Coherence with the other five operators over the same column         *)
(* ------------------------------------------------------------------ *)

(* The whole reason the hole was worth closing: WHERE could say a row satisfies
   [i <= 1.0 AND i >= 1.0] and, of the same row, that [i = 1.0] is false. All
   six operators now answer from [compare_values]. *)
let all_six_operators_agree_on_one_row () =
  with_db (fun db ->
    seed_indexed db;
    check_rows
      ~label:"= agrees with <= and >=, <> with < or >"
      [ [ "1"; "0"; "1"; "1"; "0"; "0" ] ]
      (rows_of
         db
         "SELECT i = 1.0, i <> 1.0, i <= 1.0, i >= 1.0, i < 1.0, i > 1.0 FROM t WHERE \
          tag = 'one'"))
;;

(* ------------------------------------------------------------------ *)
(* What #738 left open, since CLOSED by #743                            *)
(* ------------------------------------------------------------------ *)

(* A JOIN KEY equality is consumed too, by a different mechanism and in two
   different executors, and #738 left BOTH of them behind:

   - the keyed [Exec.stream_hash_join] arm hashed both sides on
     [Index_key.encode_value (row_value_to_index_value v)], where [1] and [1.0]
     are different bytes;
   - [Exec.nlj_probe_left] encoded the probe the same way and seeked the right
     table's index with it.

   So [FROM l JOIN r ON l.a = r.b] answered no rows while [ON 1 = 1 WHERE l.a =
   r.b] — a cartesian join whose predicate is an ordinary filter — answered the
   row. sqlite3 answers the row for BOTH spellings (oracle-checked 2026-09-03,
   with [a INTEGER] / [b REAL] so the storage classes really do differ).

   Both spellings answered NO ROWS before #738, so nothing regressed in either
   ANSWER; what was new is that they disagreed with each other. #743 closed
   that, and had to move both executors in one change — fixing the hash arm
   alone (a canonical key) while the nested-loop arm still seeked a typed index
   with the raw value would have made the answer depend on whether a
   [CREATE INDEX] exists, the "same query, two answers" failure mode this area
   keeps filing issues about. [Plan.probe_part] now carries the index column's
   declared type, the way [Op_index_lookup]'s [keys] always did.

   This case keeps only the SHAPE #738 filed, as the seam between the two
   issues. The executor-by-executor coverage — both arms asserted by
   [used_index], the reversed operand order, above 2^53, NULL, NaN, LEFT JOIN
   and #486's implicit key — lives in [test/test_join_key_743.ml]. *)
let a_cross_numeric_join_key_matches_now_743 () =
  with_db (fun db ->
    exec db "CREATE TABLE l (a INTEGER)";
    exec db "CREATE TABLE r (b REAL)";
    exec db "INSERT INTO l VALUES (1)";
    exec db "INSERT INTO r VALUES (1.0)";
    check_rows
      ~label:"the ON spelling matches, as sqlite3 does (#743)"
      [ [ "1" ] ]
      (rows_of db "SELECT l.a FROM l JOIN r ON l.a = r.b");
    check_rows
      ~label:"and the filter spelling agrees with it"
      [ [ "1" ] ]
      (rows_of db "SELECT l.a FROM l JOIN r ON 1 = 1 WHERE l.a = r.b"))
;;

let () =
  Alcotest.run
    "test_cmp_eq_738"
    [ ( "the_value_level"
      , [ Alcotest.test_case
            "cross_numeric_equality_is_exact"
            `Quick
            cross_numeric_equality_is_exact
        ; Alcotest.test_case
            "above_two_pow_53_equality_stays_exact"
            `Quick
            above_two_pow_53_equality_stays_exact
        ; Alcotest.test_case
            "cross_class_equality_is_unchanged"
            `Quick
            cross_class_equality_is_unchanged
        ; Alcotest.test_case "null_is_still_unknown" `Quick null_is_still_unknown
        ; Alcotest.test_case
            "nan_equality_follows_the_decided_order"
            `Quick
            nan_equality_follows_the_decided_order
        ] )
    ; ( "the_seek_and_the_residual_agree"
      , [ Alcotest.test_case
            "an_indexed_integer_column_seeks_for_a_real_literal"
            `Quick
            an_indexed_integer_column_seeks_for_a_real_literal
        ; Alcotest.test_case
            "a_fractional_real_matches_no_integer"
            `Quick
            a_fractional_real_matches_no_integer
        ; Alcotest.test_case
            "an_indexed_real_column_seeks_for_an_integer_literal"
            `Quick
            an_indexed_real_column_seeks_for_an_integer_literal
        ; Alcotest.test_case
            "an_integer_no_double_names_matches_nothing_on_a_real_column"
            `Quick
            an_integer_no_double_names_matches_nothing_on_a_real_column
        ; Alcotest.test_case
            "a_nan_parameter_matches_no_integer"
            `Quick
            a_nan_parameter_matches_no_integer
        ] )
    ; ( "the_rowid_alias"
      , [ Alcotest.test_case
            "the_rowid_alias_seeks_for_a_real_literal"
            `Quick
            the_rowid_alias_seeks_for_a_real_literal
        ; Alcotest.test_case
            "a_fractional_real_matches_no_rowid"
            `Quick
            a_fractional_real_matches_no_rowid
        ] )
    ; ( "the_dml_seek"
      , [ Alcotest.test_case
            "a_delete_on_the_rowid_alias_finds_a_real_literal"
            `Quick
            a_delete_on_the_rowid_alias_finds_a_real_literal
        ; Alcotest.test_case
            "an_update_through_an_index_finds_a_real_literal"
            `Quick
            an_update_through_an_index_finds_a_real_literal
        ] )
    ; ( "coherence"
      , [ Alcotest.test_case
            "all_six_operators_agree_on_one_row"
            `Quick
            all_six_operators_agree_on_one_row
        ; Alcotest.test_case
            "a_cross_numeric_join_key_matches_now_743"
            `Quick
            a_cross_numeric_join_key_matches_now_743
        ] )
    ]
;;

[@@@ai_disclosure "ai-generated"]
