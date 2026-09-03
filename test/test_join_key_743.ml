(** #743: a cross-numeric JOIN KEY equality matches the rows [=] says are
    equal, in BOTH join executors.

    {1 The defect}

    #738 made [=] compare the numeric class exactly ([1 = 1.0] is [1]) and moved
    the three index sites that consume a [col = literal] equality with it. A
    JOIN KEY equality is consumed too — [Planner.recognise_eq_col_col] picks it
    and nothing re-applies the ON predicate, so the key {b is} the match test —
    but by a different mechanism, in two different executors, and neither
    learned the cross-numeric case:

    - [Exec.stream_hash_join]'s keyed arm built and probed its [Hashtbl] on
      [Index_key.encode_value (row_value_to_index_value v)], where [1] and [1.0]
      are different bytes;
    - [Exec.nlj_probe_left] encoded its probe the same way and seeked the right
      table's {i typed} index with it.

    So [FROM l JOIN r ON l.a = r.b] across an INTEGER and a REAL column answered
    no rows while [FROM l JOIN r ON 1 = 1 WHERE l.a = r.b] answered the row.
    Neither spelling regressed — both answered nothing before #738 — what was
    new is that they disagreed with each other.

    {1 The fix, and why both halves had to move together}

    Which executor runs depends on whether the right table has an index the
    probe can use. Fixing one arm alone would have made the answer depend on
    whether a [CREATE INDEX] exists — the "same query, two answers" failure mode
    #639 and #589 are both about. So:

    - the hash arm keys on [Exec.join_key_value], a CANONICAL key: an integral
      REAL keys as the INTEGER it names, via the same [Exec.int64_of_exact_real]
      the index sites use. Canonical-key equality is then exactly
      [Exec.compare_values … = 0] on non-NULL values;
    - [Plan.probe_part] carries the declared type of the index column each part
      pins (the way [Plan.Op_index_lookup]'s [keys] already did), so
      [Exec.nlj_probe_values] translates the probe through
      [Exec.index_lookup_values] — the very function the [WHERE col = lit] seek
      uses.

    {1 What the cases assert}

    {!check_all_three} runs the SAME data three ways — the ON spelling with no
    index (hash join), the ON spelling with an index (nested-loop probe), and
    the WHERE-filter spelling that consumes no join key at all — and asserts
    [used_index] for each, so a planner change cannot make the agreement vacuous
    by quietly choosing one arm for all three.

    {1 Oracle}

    sqlite3, dev image, 2026-09-03. Two expectations are deliberate divergences
    and are flagged where they appear: TEXT-vs-numeric (sqlite3 applies column
    affinity and joins; granary applies none) and NaN (sqlite3 has no NaN at
    all, so #536's decided order governs). *)

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

(* The storage class is part of the answer here, so it is rendered rather than
   flattened: [1] and [1.0] must be distinguishable in an expectation, and
   [%.17g] also keeps [-0] apart from [0]. *)
let render = function
  | Db.V_int n -> "int:" ^ Int64.to_string n
  | Db.V_real f -> Printf.sprintf "real:%.17g" f
  | Db.V_text s -> "text:" ^ s
  | Db.V_blob b -> "blob:" ^ Bytes.to_string b
  | Db.V_null -> "NULL"
;;

(* Rows plus whether the plan's base access reaches through a seek. A
   nested-loop join always probes its right table by index, so [true] here means
   [Exec.nlj_probe_left] ran and [false] over these fixtures means
   [Exec.stream_hash_join] did. *)
let rows_and_index db sql =
  match run (Db.query_with_stats db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok (stream, stats) ->
    let rows =
      List.sort
        compare
        (List.map
           (fun r -> Array.to_list (Array.map render r))
           (run (Lwt_stream.to_list stream)))
    in
    rows, stats.Db.used_index
;;

let check_rows ~label expected actual =
  Alcotest.(check (list (list string))) label expected actual
;;

let run_join ?index ~seed sql =
  with_db (fun db ->
    List.iter (exec db) seed;
    Option.iter (exec db) index;
    rows_and_index db sql)
;;

(* The point of the file: one equality, three consumers, one answer.

   [on_sql] is run twice over identical data — once with no index, so the
   planner builds a hash join, and once with [index], so it builds a
   nested-loop probe — and [where_sql] spells the same restriction as a filter
   the join never consumes. All three must agree, and the [used_index]
   assertions are what stop a planner change from making that agreement
   vacuous. *)
let check_all_three ~label ~seed ~index ~on_sql ~where_sql expected =
  let hash_rows, hash_idx = run_join ~seed on_sql in
  let nlj_rows, nlj_idx = run_join ~seed ~index on_sql in
  let filter_rows, filter_idx = run_join ~seed where_sql in
  Alcotest.(check bool) (label ^ ": un-indexed ON is a hash join") false hash_idx;
  Alcotest.(check bool) (label ^ ": indexed ON is a nested-loop probe") true nlj_idx;
  Alcotest.(check bool) (label ^ ": the filter spelling seeks nothing") false filter_idx;
  check_rows ~label:(label ^ " [hash join]") expected hash_rows;
  check_rows ~label:(label ^ " [nested-loop index probe]") expected nlj_rows;
  check_rows ~label:(label ^ " [WHERE filter]") expected filter_rows
;;

(* ------------------------------------------------------------------ *)
(* Fixtures                                                             *)
(* ------------------------------------------------------------------ *)

(* INTEGER on the left, REAL on the right. Column typing is strict here — an
   INTEGER column cannot hold [1.0] and a REAL column cannot hold [1] — so a
   cross-numeric join key needs two tables whose join columns are declared with
   different numeric types. That is also why the UNIQUE and index-put byte
   probes are unaffected by this issue: one column is one storage class. *)
let int_real_seed =
  [ "CREATE TABLE l (a INTEGER)"
  ; "CREATE TABLE r (b REAL)"
  ; "INSERT INTO l VALUES (1),(2),(3)"
  ; "INSERT INTO r VALUES (1.0),(2.5),(3.0)"
  ]
;;

let real_int_seed =
  [ "CREATE TABLE l (a REAL)"
  ; "CREATE TABLE r (b INTEGER)"
  ; "INSERT INTO l VALUES (1.0),(2.5),(3.0)"
  ; "INSERT INTO r VALUES (1),(2),(3)"
  ]
;;

let idx_r = "CREATE INDEX r_b ON r(b)"
let on_sql = "SELECT l.a, r.b FROM l JOIN r ON l.a = r.b"

(* [ON 1 = 1] is NOT [Planner.on_is_trivially_true] — that recognises the bare
   [Ast.L_int 1L] the comma/CROSS sugar produces, not an equality between two
   literals — so this really does take the general-ON cartesian arm and leaves
   the restriction to a filter above the join. It is the spelling the issue
   contrasts the ON key against. *)
let where_sql = "SELECT l.a, r.b FROM l JOIN r ON 1 = 1 WHERE l.a = r.b"

(* ------------------------------------------------------------------ *)
(* The headline case                                                    *)
(* ------------------------------------------------------------------ *)

(* The issue's own repro, widened to show what does NOT join. sqlite3 answers
   [1|1.0] and [3|3.0] for every spelling; [2] joins nothing because no integer
   equals [2.5]. *)
let a_cross_numeric_join_key_matches () =
  check_all_three
    ~label:"INTEGER left, REAL right"
    ~seed:int_real_seed
    ~index:idx_r
    ~on_sql
    ~where_sql
    [ [ "int:1"; "real:1" ]; [ "int:3"; "real:3" ] ]
;;

(* The other operand order, which reaches a different arm of
   [Exec.index_lookup_values] ([V_int] against a REAL column, rather than
   [V_real] against an INTEGER one). sqlite3 answers [1.0|1] and [3.0|3]. *)
let the_reversed_operand_order_matches_too () =
  check_all_three
    ~label:"REAL left, INTEGER right"
    ~seed:real_int_seed
    ~index:idx_r
    ~on_sql
    ~where_sql
    [ [ "real:1"; "int:1" ]; [ "real:3"; "int:3" ] ]
;;

(* A non-integral REAL equals no integer, so [None] out of
   [Exec.int64_of_exact_real] must still mean "matches nothing" and never "seek
   wider". sqlite3 answers no rows. *)
let a_non_integral_real_joins_no_integer () =
  check_all_three
    ~label:"no integer equals 1.5 or 2.5"
    ~seed:
      [ "CREATE TABLE l (a INTEGER)"
      ; "CREATE TABLE r (b REAL)"
      ; "INSERT INTO l VALUES (1),(2)"
      ; "INSERT INTO r VALUES (1.5),(2.5)"
      ]
    ~index:idx_r
    ~on_sql
    ~where_sql
    []
;;

(* The exactness must not be spelled as an [Int64.to_float] promotion — that
   rounds, so above 2^53 two distinct int64s promote to one double and a join
   key would pair rows [>] separates. That is #733 reintroduced inside a join
   key, one magnitude up.

   sqlite3 joins [9007199254740992] with [9.00719925474099e+15] and leaves
   [9007199254740993] unmatched, for both spellings. *)
let above_two_pow_53_stays_exact_in_both_executors () =
  check_all_three
    ~label:"only the int64 the double NAMES joins it"
    ~seed:
      [ "CREATE TABLE l (a INTEGER)"
      ; "CREATE TABLE r (b REAL)"
      ; "INSERT INTO l VALUES (9007199254740993),(9007199254740992)"
      ; "INSERT INTO r VALUES (9007199254740992.0)"
      ]
    ~index:idx_r
    ~on_sql
    ~where_sql
    [ [ "int:9007199254740992"; "real:9007199254740992" ] ]
;;

(* A NULL join key matches nothing under three-valued logic, and the
   canonicalisation must not change that: NULL is dropped before keying on both
   sides of the hash join, and [Exec.index_lookup_values] answers [None] for it.
   NULL's tag byte ([0x00]) is also distinct from every canonical key's.

   sqlite3 answers the one matching pair. *)
let null_join_keys_still_match_nothing () =
  check_all_three
    ~label:"NULL never joins, not even another NULL"
    ~seed:
      [ "CREATE TABLE l (a INTEGER)"
      ; "CREATE TABLE r (b REAL)"
      ; "INSERT INTO l VALUES (NULL),(1)"
      ; "INSERT INTO r VALUES (NULL),(1.0)"
      ]
    ~index:idx_r
    ~on_sql
    ~where_sql
    [ [ "int:1"; "real:1" ] ]
;;

(* A LEFT JOIN is where a mis-answered key is loudest: the ON predicate IS the
   match test, so a key that wrongly matches nothing null-extends the row
   instead of dropping it, and the query still returns the right number of rows
   with the wrong contents. Both executors must null-extend exactly [2] (whose
   [2.5] partner does not exist) and the NULL key.

   sqlite3 answers the same four rows. *)
let left_join_null_extends_only_the_genuine_misses () =
  let seed =
    [ "CREATE TABLE l (a INTEGER)"
    ; "CREATE TABLE r (b REAL)"
    ; "INSERT INTO l VALUES (NULL),(1),(2)"
    ; "INSERT INTO r VALUES (1.0),(3.0)"
    ]
  in
  let sql = "SELECT l.a, r.b FROM l LEFT JOIN r ON l.a = r.b" in
  let expected = [ [ "NULL"; "NULL" ]; [ "int:1"; "real:1" ]; [ "int:2"; "NULL" ] ] in
  let hash_rows, hash_idx = run_join ~seed sql in
  let nlj_rows, nlj_idx = run_join ~seed ~index:idx_r sql in
  Alcotest.(check bool) "un-indexed LEFT JOIN is a hash join" false hash_idx;
  Alcotest.(check bool) "indexed LEFT JOIN is a nested-loop probe" true nlj_idx;
  check_rows ~label:"LEFT JOIN [hash join]" expected hash_rows;
  check_rows ~label:"LEFT JOIN [nested-loop index probe]" expected nlj_rows
;;

(* #486's implicit key is a THIRD consumer of the same equality: a
   comma-separated FROM is an unrestricted join whose restriction sits in the
   WHERE clause, and [Planner.where_join_key] hands that conjunct to the join
   rather than building the cartesian product. It reaches the same two
   executors, so it had the same defect and is fixed by the same change.

   sqlite3 answers [1|1.0] and [3|3.0]. *)
let the_implicit_where_join_key_agrees () =
  check_all_three
    ~label:"FROM l, r WHERE l.a = r.b (#486's implicit key)"
    ~seed:int_real_seed
    ~index:idx_r
    ~on_sql:"SELECT l.a, r.b FROM l, r WHERE l.a = r.b"
      (* The foil has to defeat [where_join_key] as well as [recognise_eq_col_col],
         so the restriction is written over an expression no recogniser matches. *)
    ~where_sql:"SELECT l.a, r.b FROM l, r WHERE l.a + 0 = r.b + 0"
    [ [ "int:1"; "real:1" ]; [ "int:3"; "real:3" ] ]
;;

(* ------------------------------------------------------------------ *)
(* Cross-class, which must NOT start matching                           *)
(* ------------------------------------------------------------------ *)

(* {b Divergence from sqlite3, deliberate and pre-existing.} sqlite3 applies
   COLUMN AFFINITY to a comparison operand, so [tl.a = tr.b] across a TEXT and
   an INTEGER column coerces the text and answers one row (oracle-checked
   2026-09-03). Granary applies no affinity anywhere — CLAUDE.md records the
   same divergence for [o BETWEEN 100 AND '119'] — so a cross-CLASS pair is
   never equal, in the join key and in the filter alike.

   What #743 owes is that the three spellings agree with EACH OTHER, and they
   do: the canonical key keeps TEXT's tag byte, and
   [Exec.index_lookup_values] answers [None] for a TEXT probe on a numeric
   column. Changing the affinity rule is a separate decision. *)
let text_and_number_still_match_nothing () =
  check_all_three
    ~label:"no affinity: '1' does not join 1 (sqlite3 joins it)"
    ~seed:
      [ "CREATE TABLE l (a TEXT)"
      ; "CREATE TABLE r (b INTEGER)"
      ; "INSERT INTO l VALUES ('1')"
      ; "INSERT INTO r VALUES (1)"
      ]
    ~index:idx_r
    ~on_sql
    ~where_sql
    []
;;

(* ------------------------------------------------------------------ *)
(* NaN                                                                  *)
(* ------------------------------------------------------------------ *)

(* #536: NaN is a real value here, below every number, where sqlite3 has none at
   all — so this case is a DIVERGENCE and is not oracle-checked.

   Two properties are load-bearing. [Exec.int64_of_exact_real] declines NaN
   FIRST, so the canonicalisation can never call [Int64.of_float] on it
   ([Float.trunc nan] is [nan] and [Float.equal nan nan] is true, so a naive
   integrality test would have): a NaN join key therefore matches no integer.
   And NaN's encoding is the single byte [0x01] (#578), distinct from NULL's
   [0x00], so two NaNs share one hash bucket — matching
   [Float.compare nan nan = 0] — while a NaN never collides with a NULL. *)
let a_nan_join_key_matches_nan_and_no_number () =
  let insert_nan db table =
    match run (Db.prepare db (Printf.sprintf "INSERT INTO %s VALUES (?)" table)) with
    | Error e -> Alcotest.failf "prepare: %a" Db.pp_error e
    | Ok st ->
      (match run (Db.run st ~params:[ Db.V_real Float.nan ]) with
       | Ok _ -> ()
       | Error e -> Alcotest.failf "insert NaN: %a" Db.pp_error e)
  in
  (* NaN against an INTEGER column: no integer equals it, in either executor. *)
  let no_number db =
    exec db "CREATE TABLE l (a INTEGER)";
    exec db "CREATE TABLE r (b REAL)";
    exec db "INSERT INTO l VALUES (0),(1)";
    insert_nan db "r"
  in
  with_db (fun db ->
    no_number db;
    let rows, used = rows_and_index db on_sql in
    Alcotest.(check bool) "hash join" false used;
    check_rows ~label:"a NaN join key matches no integer [hash join]" [] rows);
  with_db (fun db ->
    no_number db;
    exec db idx_r;
    let rows, used = rows_and_index db on_sql in
    Alcotest.(check bool) "nested-loop probe" true used;
    check_rows ~label:"a NaN join key matches no integer [probe]" [] rows);
  (* NaN against a REAL column holding a NaN and a NULL: it joins the NaN and
     not the NULL. *)
  let nan_and_null db =
    exec db "CREATE TABLE l (a REAL)";
    exec db "CREATE TABLE r (b REAL)";
    insert_nan db "l";
    insert_nan db "r";
    exec db "INSERT INTO r VALUES (NULL),(1.0)"
  in
  with_db (fun db ->
    nan_and_null db;
    let rows, used = rows_and_index db on_sql in
    Alcotest.(check bool) "hash join" false used;
    check_rows
      ~label:"NaN joins NaN, never NULL [hash join]"
      [ [ "real:nan"; "real:nan" ] ]
      rows);
  with_db (fun db ->
    nan_and_null db;
    exec db idx_r;
    let rows, used = rows_and_index db on_sql in
    Alcotest.(check bool) "nested-loop probe" true used;
    check_rows
      ~label:"NaN joins NaN, never NULL [probe]"
      [ [ "real:nan"; "real:nan" ] ]
      rows)
;;

(* ------------------------------------------------------------------ *)
(* The residual this fix does NOT close                                 *)
(* ------------------------------------------------------------------ *)

(* {b Negative zero: a known residual, pinned as such rather than endorsed.}

   [Float.compare (-0.) 0.] is [0], so [Exec.compare_values] — and therefore
   [=] — says [0], [0.0] and [-0.0] are all equal. [Index_key.encode_value]
   deliberately does not: it encodes [-0.0] BELOW [+0.0] so the index's byte
   order is IEEE's total order.

   That disagreement is NOT #743's and is not introduced here. It already
   splits an indexed [WHERE] from an unindexed one on [main]: over a REAL
   column holding [-0.0], [WHERE b = 0.0] returns the row with no index and
   nothing with one (verified 2026-09-03, before this change). The join arms
   inherit exactly that split and nothing wider — the canonical hash key agrees
   with [compare_values], and the probe agrees with the indexed [WHERE] because
   it calls the same [Exec.index_lookup_values].

   sqlite3 answers the row for all of these ([-0.0 = 0.0] and [0 = -0.0] are
   both [1]), so the hash arm is the one that is right.

   Change this case as a DECISION: closing it means making the index encoding
   or [compare_values] agree about +/-0, which moves index ordering and is a
   different issue. *)
let negative_zero_is_a_known_residual_of_the_index_encoding () =
  let seed =
    [ "CREATE TABLE l (a INTEGER)"
    ; "CREATE TABLE r (b REAL)"
    ; "INSERT INTO l VALUES (0)"
    ; "INSERT INTO r VALUES (-0.0)"
    ]
  in
  let hash_rows, hash_idx = run_join ~seed on_sql in
  let nlj_rows, nlj_idx = run_join ~seed ~index:idx_r on_sql in
  let filter_rows, _ = run_join ~seed where_sql in
  Alcotest.(check bool) "un-indexed ON is a hash join" false hash_idx;
  Alcotest.(check bool) "indexed ON is a nested-loop probe" true nlj_idx;
  check_rows
    ~label:"the hash key agrees with compare_values, and with sqlite3"
    [ [ "int:0"; "real:-0" ] ]
    hash_rows;
  check_rows ~label:"as does the filter spelling" [ [ "int:0"; "real:-0" ] ] filter_rows;
  check_rows
    ~label:"the index probe does not — the encoding separates -0.0 from +0.0"
    []
    nlj_rows
;;

let () =
  Alcotest.run
    "test_join_key_743"
    [ ( "cross_numeric_join_keys"
      , [ Alcotest.test_case
            "a_cross_numeric_join_key_matches"
            `Quick
            a_cross_numeric_join_key_matches
        ; Alcotest.test_case
            "the_reversed_operand_order_matches_too"
            `Quick
            the_reversed_operand_order_matches_too
        ; Alcotest.test_case
            "a_non_integral_real_joins_no_integer"
            `Quick
            a_non_integral_real_joins_no_integer
        ; Alcotest.test_case
            "above_two_pow_53_stays_exact_in_both_executors"
            `Quick
            above_two_pow_53_stays_exact_in_both_executors
        ; Alcotest.test_case
            "null_join_keys_still_match_nothing"
            `Quick
            null_join_keys_still_match_nothing
        ; Alcotest.test_case
            "left_join_null_extends_only_the_genuine_misses"
            `Quick
            left_join_null_extends_only_the_genuine_misses
        ; Alcotest.test_case
            "the_implicit_where_join_key_agrees"
            `Quick
            the_implicit_where_join_key_agrees
        ] )
    ; ( "what_must_not_start_matching"
      , [ Alcotest.test_case
            "text_and_number_still_match_nothing"
            `Quick
            text_and_number_still_match_nothing
        ; Alcotest.test_case
            "a_nan_join_key_matches_nan_and_no_number"
            `Quick
            a_nan_join_key_matches_nan_and_no_number
        ] )
    ; ( "residuals"
      , [ Alcotest.test_case
            "negative_zero_is_a_known_residual_of_the_index_encoding"
            `Quick
            negative_zero_is_a_known_residual_of_the_index_encoding
        ] )
    ]
;;
