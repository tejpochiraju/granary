(** #754: an indexed equality on a REAL column used to silently miss a stored
    [-0.0].

    {1 The defect}

    [Float.compare (-0.) 0.] is [0], so [Exec.compare_values] — and therefore
    [=] since #738 — has always said [0], [0.0] and [-0.0] are all equal.
    [Index_key.encode_value]'s [IK_real] arm disagreed: it deliberately
    encoded [-0.0] strictly BELOW [+0.0], because the encoding is IEEE's total
    order applied literally to the float's bit pattern.

    An equality conjunct on an indexed column is CONSUMED by the access path
    ([Planner.residual_filter] removes it once the seek is built) — the seek
    IS the answer, nothing re-checks the rows it yields. So [WHERE b = 0.0]
    against an indexed REAL column silently returned zero rows when the only
    matching stored value was [-0.0], while the identical query with no index
    correctly returned the row. Split out of #743, whose cross-numeric JOIN
    KEY fix inherited exactly this residual through the nested-loop probe
    (which seeks the same index) while the hash join's canonical key already
    agreed with [compare_values].

    {1 The fix}

    [Index_key.encode_value] now normalizes [-0.0]'s bit pattern to [+0.0]'s
    before the order-preserving transform, so the two encode IDENTICALLY
    rather than [-0.0] sorting first. That closes the gap for every consumer
    of the encoding at once: the [WHERE] seek, the UNIQUE conflict probe, and
    the join probe, with no separate fix needed at any of those call sites.
    This is a genuine on-disk index-format change — see docs/DECISIONS.md
    (#754) for why that is accepted here with no rebuild/migration path, the
    same call #578 made for its own NaN tag-byte change.

    {1 Oracle}

    sqlite3 answers the row for [-0.0 = 0.0] and [0 = -0.0] (both are [1]),
    oracle-checked 2026-09-03. *)

open Lwt.Syntax
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

let prepare db sql =
  run
    (let* r = Db.prepare db sql in
     match r with
     | Ok st -> Lwt.return st
     | Error e -> Alcotest.failf "prepare %S: %a" sql Db.pp_error e)
;;

let run_stmt db sql params =
  let st = prepare db sql in
  run (Db.run st ~params)
;;

let expect_ok db sql params ~msg =
  match run_stmt db sql params with
  | Ok n -> n
  | Error e -> Alcotest.failf "%s : expected success, got: %a" msg Db.pp_error e
;;

(* Like [expect_ok] but against an already-{!prepare}d statement, for seeding
   several rows through one reusable statement rather than re-preparing the
   same SQL per row. *)
let expect_ok_stmt st params ~msg =
  match run (Db.run st ~params) with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "%s : expected success, got: %a" msg Db.pp_error e
;;

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let expect_unique_violation db sql params ~table ~col ~msg =
  let want = Printf.sprintf "UNIQUE constraint failed: %s.%s" table col in
  match run_stmt db sql params with
  | Ok n -> Alcotest.failf "%s : expected UNIQUE violation, wrote %d rows" msg n
  | Error e ->
    let got = Format.asprintf "%a" Db.pp_error e in
    Alcotest.(check bool)
      (Printf.sprintf "%s : UNIQUE violation (got %S)" msg got)
      true
      (contains ~needle:want got)
;;

(* Like [expect_unique_violation] but for a DDL statement (e.g. [CREATE
   UNIQUE INDEX]) rather than a DML statement with parameters — no [params]
   or [table]/[col], since [execute_create_index]'s build-time duplicate
   probe reports the offending index/column pair however the DDL names it. *)
let expect_ddl_unique_violation db sql ~msg =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "%s : expected UNIQUE violation, but %S succeeded" msg sql
  | Error e ->
    let got = Format.asprintf "%a" Db.pp_error e in
    Alcotest.(check bool)
      (Printf.sprintf "%s : UNIQUE violation (got %S)" msg got)
      true
      (contains ~needle:"UNIQUE constraint failed" got)
;;

let render = function
  | Db.V_int n -> "int:" ^ Int64.to_string n
  | Db.V_real f -> Printf.sprintf "real:%.17g" f
  | Db.V_text s -> "text:" ^ s
  | Db.V_blob b -> "blob:" ^ Bytes.to_string b
  | Db.V_null -> "NULL"
;;

let query_rows_with_stats db sql =
  match run (Db.query_with_stats db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok (stream, stats) ->
    ( List.map
        (fun r -> Array.to_list (Array.map render r))
        (run (Lwt_stream.to_list stream))
    , stats.Db.used_index )
;;

let check_rows ~label expected actual =
  Alcotest.(check (list (list string))) label expected actual
;;

(* ------------------------------------------------------------------ *)
(* The issue's own repro: indexed vs. unindexed WHERE                   *)
(* ------------------------------------------------------------------ *)

let seed_zi db =
  exec db "CREATE TABLE zi (b REAL)";
  exec db "INSERT INTO zi VALUES (-0.0), (1.5)"
;;

let indexed_where_finds_negative_zero () =
  with_db (fun db ->
    seed_zi db;
    exec db "CREATE INDEX zi_b ON zi(b)";
    let rows, used_index = query_rows_with_stats db "SELECT b FROM zi WHERE b = 0.0" in
    Alcotest.(check bool) "the seek goes through the index" true used_index;
    check_rows ~label:"WHERE b = 0.0 finds the -0.0 row" [ [ "real:-0" ] ] rows)
;;

let indexed_where_int_literal_finds_negative_zero () =
  with_db (fun db ->
    seed_zi db;
    exec db "CREATE INDEX zi_b ON zi(b)";
    let rows, used_index = query_rows_with_stats db "SELECT b FROM zi WHERE b = 0" in
    Alcotest.(check bool) "the seek goes through the index" true used_index;
    check_rows ~label:"WHERE b = 0 finds the -0.0 row" [ [ "real:-0" ] ] rows)
;;

let unindexed_where_finds_negative_zero () =
  with_db (fun db ->
    seed_zi db;
    let rows, used_index = query_rows_with_stats db "SELECT b FROM zi WHERE b = 0.0" in
    Alcotest.(check bool) "no index exists to seek" false used_index;
    check_rows ~label:"WHERE b = 0.0 finds the -0.0 row" [ [ "real:-0" ] ] rows)
;;

(* The issue's "unoptimizable foil": [b + 0] can never be recognised as a bare
   column, so the planner has no equality conjunct to consume and this always
   fell back to a residual-checked scan, on [main] and after this fix alike.
   Kept as a control that the indexed case above now matches. *)
let unoptimizable_foil_already_found_it () =
  with_db (fun db ->
    seed_zi db;
    exec db "CREATE INDEX zi_b ON zi(b)";
    let rows, used_index =
      query_rows_with_stats db "SELECT b FROM zi WHERE b + 0 = 0.0"
    in
    Alcotest.(check bool) "not a recognisable equality seek" false used_index;
    check_rows ~label:"the residual scan finds -0.0" [ [ "real:-0" ] ] rows)
;;

(* The indexed and unindexed spellings must never disagree — the #536 "a seek
   and its residual may never disagree" invariant, generalised to +/-0. *)
let indexed_and_unindexed_where_agree () =
  with_db (fun db ->
    seed_zi db;
    let unindexed, _ = query_rows_with_stats db "SELECT b FROM zi WHERE b = 0.0" in
    exec db "CREATE INDEX zi_b ON zi(b)";
    let indexed, used_index = query_rows_with_stats db "SELECT b FROM zi WHERE b = 0.0" in
    Alcotest.(check bool) "this spelling does seek the index" true used_index;
    check_rows ~label:"indexed and unindexed WHERE agree" unindexed indexed)
;;

(* ------------------------------------------------------------------ *)
(* The join-probe residual inherited from #743                          *)
(* ------------------------------------------------------------------ *)

(* A hash join (no index on the right table) keys on [Exec.join_key_value],
   which already canonicalised [-0.0] to [IK_int 0] and so already agreed
   with [compare_values]. A nested-loop join (index on the right table) seeks
   the raw index encoding through [Exec.index_lookup_values] — the same
   function the [WHERE] seek above uses — so before this fix it alone missed
   the row. *)
let seed_join db =
  exec db "CREATE TABLE l (a INTEGER)";
  exec db "CREATE TABLE r (b REAL)";
  exec db "INSERT INTO l VALUES (0)";
  exec db "INSERT INTO r VALUES (-0.0)"
;;

let hash_join_matches_negative_zero () =
  with_db (fun db ->
    seed_join db;
    let rows, used_index =
      query_rows_with_stats db "SELECT l.a, r.b FROM l JOIN r ON l.a = r.b"
    in
    Alcotest.(check bool) "no index on r: this is a hash join" false used_index;
    check_rows ~label:"the hash join matches 0 with -0.0" [ [ "int:0"; "real:-0" ] ] rows)
;;

let nested_loop_probe_now_matches_negative_zero () =
  with_db (fun db ->
    seed_join db;
    exec db "CREATE INDEX r_b ON r(b)";
    let rows, used_index =
      query_rows_with_stats db "SELECT l.a, r.b FROM l JOIN r ON l.a = r.b"
    in
    Alcotest.(check bool) "an index on r: this is a nested-loop probe" true used_index;
    check_rows
      ~label:"the index probe now matches 0 with -0.0 too (#754)"
      [ [ "int:0"; "real:-0" ] ]
      rows)
;;

(* ------------------------------------------------------------------ *)
(* UNIQUE-index probe                                                   *)
(* ------------------------------------------------------------------ *)

(* Before #754, [+0.0] and [-0.0] encoded to different keys, so a UNIQUE
   index over a REAL column would happily hold BOTH — a real constraint hole,
   since [compare_values] (and therefore the UNIQUE semantics every other
   type enforces via the same encoding) has always treated them as the same
   value. After #754 the second insert must conflict with the first. *)
let unique_index_rejects_negative_zero_after_positive_zero () =
  with_db (fun db ->
    exec db "CREATE TABLE u (k INTEGER PRIMARY KEY, r REAL)";
    exec db "CREATE UNIQUE INDEX ui ON u (r)";
    ignore
      (expect_ok
         db
         "INSERT INTO u (k, r) VALUES (1, ?)"
         [ Db.V_real 0.0 ]
         ~msg:"seed +0.0");
    expect_unique_violation
      db
      "INSERT INTO u (k, r) VALUES (2, ?)"
      [ Db.V_real (-0.0) ]
      ~table:"u"
      ~col:"r"
      ~msg:"-0.0 now conflicts with the stored +0.0")
;;

let unique_index_rejects_positive_zero_after_negative_zero () =
  with_db (fun db ->
    exec db "CREATE TABLE u (k INTEGER PRIMARY KEY, r REAL)";
    exec db "CREATE UNIQUE INDEX ui ON u (r)";
    ignore
      (expect_ok
         db
         "INSERT INTO u (k, r) VALUES (1, ?)"
         [ Db.V_real (-0.0) ]
         ~msg:"seed -0.0");
    expect_unique_violation
      db
      "INSERT INTO u (k, r) VALUES (2, ?)"
      [ Db.V_real 0.0 ]
      ~table:"u"
      ~col:"r"
      ~msg:"+0.0 now conflicts with the stored -0.0")
;;

(* ------------------------------------------------------------------ *)
(* CREATE UNIQUE INDEX's own build-time duplicate scan (retroactive build   *)
(* over pre-existing rows), as opposed to the INSERT-time probe above.      *)
(* ------------------------------------------------------------------ *)

(* docs/DECISIONS.md claims #754 closes this "for free" because
   [execute_create_index]'s build-time duplicate scan reads
   [Index_key.encode_value] bytes exactly like [check_insert_unique] does —
   but that claim had no direct test through the RETROACTIVE-build path
   specifically (as opposed to building the index first and then inserting,
   which every other case in this file already covers). This is the
   PR-review-requested repro: seed both signs of zero BEFORE the index
   exists, then attempt to build a UNIQUE index over them. *)
let create_unique_index_build_rejects_existing_negative_zero_pair () =
  with_db (fun db ->
    exec db "CREATE TABLE t (r REAL)";
    let st = prepare db "INSERT INTO t (r) VALUES (?)" in
    expect_ok_stmt st [ Db.V_real 0.0 ] ~msg:"seed +0.0";
    expect_ok_stmt st [ Db.V_real (-0.0) ] ~msg:"seed -0.0";
    expect_ddl_unique_violation
      db
      "CREATE UNIQUE INDEX ui ON t (r)"
      ~msg:"retroactive build over an existing +0.0/-0.0 pair conflicts (#754)")
;;

(* Control, same shape as [unique_index_is_still_per_column_typed] but for
   the BUILD path rather than INSERT: seeding two ordinary distinct REAL
   values before the index exists must still let the build succeed, so the
   test above is pinning a real exclusion rather than "every retroactive
   REAL UNIQUE build fails". *)
let create_unique_index_build_accepts_distinct_reals () =
  with_db (fun db ->
    exec db "CREATE TABLE t (r REAL)";
    let st = prepare db "INSERT INTO t (r) VALUES (?)" in
    expect_ok_stmt st [ Db.V_real 0.0 ] ~msg:"seed 0.0";
    expect_ok_stmt st [ Db.V_real 1.5 ] ~msg:"seed 1.5";
    exec db "CREATE UNIQUE INDEX ui ON t (r)")
;;

(* Control: there is no INTEGER-to-REAL coercion anywhere in this engine to
   exercise here — strict column typing refuses an INTEGER LITERAL into a
   REAL column outright at sema (`INSERT INTO u (k, r) VALUES (2, 1)` fails
   with "type mismatch: expected REAL, got INTEGER", verified directly
   against this table shape), and binding a `V_int` PARAMETER to a REAL
   column reaches an unrelated, unhandled `Row.encode` invariant failure
   further down (a real gap, but not this issue's — out of scope for #754).
   So the only thing this test can and does confirm is the boundary of the
   fix itself: two ORDINARY, distinct REAL values (`0.0` and `1.0`, neither
   of them the `+0.0`/`-0.0` pair #754 is about) still insert into the same
   UNIQUE REAL column without a false conflict, i.e. #754's key-collapsing
   change is scoped to signed zero and does not make every REAL collide. *)
let unique_index_is_still_per_column_typed () =
  with_db (fun db ->
    exec db "CREATE TABLE u (k INTEGER PRIMARY KEY, r REAL)";
    exec db "CREATE UNIQUE INDEX ui ON u (r)";
    ignore
      (expect_ok
         db
         "INSERT INTO u (k, r) VALUES (1, ?)"
         [ Db.V_real 0.0 ]
         ~msg:"seed 0.0");
    ignore
      (expect_ok
         db
         "INSERT INTO u (k, r) VALUES (2, ?)"
         [ Db.V_real 1.0 ]
         ~msg:"1.0 is distinct"))
;;

let () =
  Alcotest.run
    "test_negative_zero_754"
    [ ( "indexed_vs_unindexed_where"
      , [ Alcotest.test_case
            "indexed WHERE b = 0.0 finds -0.0"
            `Quick
            indexed_where_finds_negative_zero
        ; Alcotest.test_case
            "indexed WHERE b = 0 (int literal) finds -0.0"
            `Quick
            indexed_where_int_literal_finds_negative_zero
        ; Alcotest.test_case
            "unindexed WHERE b = 0.0 finds -0.0 (regression control)"
            `Quick
            unindexed_where_finds_negative_zero
        ; Alcotest.test_case
            "the unoptimizable foil already found it (regression control)"
            `Quick
            unoptimizable_foil_already_found_it
        ; Alcotest.test_case
            "indexed and unindexed WHERE agree"
            `Quick
            indexed_and_unindexed_where_agree
        ] )
    ; ( "join_probe_743"
      , [ Alcotest.test_case
            "hash join matches -0.0 (already correct pre-#754)"
            `Quick
            hash_join_matches_negative_zero
        ; Alcotest.test_case
            "nested-loop index probe now matches -0.0 too"
            `Quick
            nested_loop_probe_now_matches_negative_zero
        ] )
    ; ( "unique_index_probe"
      , [ Alcotest.test_case
            "+0.0 then -0.0 conflicts"
            `Quick
            unique_index_rejects_negative_zero_after_positive_zero
        ; Alcotest.test_case
            "-0.0 then +0.0 conflicts"
            `Quick
            unique_index_rejects_positive_zero_after_negative_zero
        ; Alcotest.test_case
            "sanity: distinct reals still insert"
            `Quick
            unique_index_is_still_per_column_typed
        ] )
    ; ( "unique_index_build"
      , [ Alcotest.test_case
            "retroactive build over an existing +0.0/-0.0 pair conflicts"
            `Quick
            create_unique_index_build_rejects_existing_negative_zero_pair
        ; Alcotest.test_case
            "retroactive build over distinct reals still succeeds"
            `Quick
            create_unique_index_build_accepts_distinct_reals
        ] )
    ]
;;
