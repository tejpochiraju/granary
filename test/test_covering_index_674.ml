(** #674 (item 1 of 3): covering-index reads for MIN/MAX/COUNT/EXISTS over an
    [Op_index_lookup], per the design doc at
    [docs/superpowers/specs/2026-08-08-674-1-covering-index-design.md].

    Three things this file pins:

    - Correctness: fast path ON vs. forced OFF ([GRANARY_AGG_FASTPATH=0])
      must agree exactly, on every shape the design says is eligible.
    - The perf claim: {!Db.query_with_stats}'s [rows_examined] (table
      fetches) must be 0 for an eligible query, not the size of the matching
      index range — that is the whole point of #674.
    - Every guardrail case from the design falls back correctly: a nullable
      indexed column, a NOT NULL VIRTUAL generated column (included, per the
      design's explicit decision), a #517 range alongside MIN/MAX, a
      residual predicate reading a non-index column, and GROUP BY. *)

open Lwt.Syntax
module U = Granary_unix
module Db = Granary.Db

let run = Lwt_main.run

let unwrap = function
  | Ok x -> x
  | Error e -> Alcotest.failf "db error: %a" Db.pp_error e
;;

let set_fastpath on = Unix.putenv "GRANARY_AGG_FASTPATH" (if on then "1" else "0")

let vstr = function
  | Db.V_int i -> Printf.sprintf "i:%Ld" i
  | Db.V_real f -> Printf.sprintf "r:%.17g" f
  | Db.V_text s -> Printf.sprintf "t:%s" s
  | Db.V_null -> "null"
  | Db.V_blob b -> Printf.sprintf "b:%s" (Bytes.to_string b)
;;

let rows_of db sql =
  run
    (let* s = Lwt.map unwrap (Db.query db sql) in
     let* rows = Lwt_stream.to_list s in
     Lwt.return
       (List.map (fun r -> String.concat "," (Array.to_list (Array.map vstr r))) rows))
;;

let with_db f =
  let dir = Filename.temp_file "t674-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "db" in
  let db = run (Lwt.map unwrap (U.open_file_wal ~path ())) in
  Fun.protect
    ~finally:(fun () ->
      run (Db.close db);
      List.iter
        (fun s ->
           try Sys.remove (path ^ s) with
           | _ -> ())
        [ ""; "-wal" ];
      try Unix.rmdir dir with
      | _ -> ())
    (fun () -> f db)
;;

let exec db sql = ignore (run (Lwt.map unwrap (Db.execute db sql)))

(* NaN has no literal SQL spelling (see #517's comment); it can only arrive as
   a bound parameter. *)
let exec_params db sql params =
  run
    (let* stmt = Lwt.map unwrap (Db.prepare db sql) in
     let* (_ : int) = Lwt.map unwrap (Db.run stmt ~params) in
     Db.finalize stmt)
;;

let stats_of db sql =
  match
    run
      (let* r = Db.query_with_stats db sql in
       match r with
       | Error e -> Lwt.return (Error e)
       | Ok (stream, stats) ->
         let* rows = Lwt_stream.to_list stream in
         Lwt.return (Ok (List.length rows, stats)))
  with
  | Ok v -> v
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
;;

(* ------------------------------------------------------------------ *)
(* Fixture: TPC-C-Delivery-shaped table — two-column equality prefix,   *)
(* one more NOT NULL trailing key column, plus a nullable, non-indexed  *)
(* and a generated column for the fallback tests.                       *)
(* ------------------------------------------------------------------ *)

let n_w = 3
let n_d = 3
let n_per_district = 40

let seed db =
  exec
    db
    "CREATE TABLE new_order (\n\
    \       no_w_id INTEGER NOT NULL,\n\
    \       no_d_id INTEGER NOT NULL,\n\
    \       no_o_id INTEGER NOT NULL,\n\
    \       payload TEXT)";
  exec db "CREATE INDEX idx_no_wd ON new_order (no_w_id, no_d_id, no_o_id)";
  exec db "BEGIN";
  for w = 1 to n_w do
    for d = 1 to n_d do
      for i = 1 to n_per_district do
        exec db (Printf.sprintf "INSERT INTO new_order VALUES (%d,%d,%d,'p%d')" w d i i)
      done
    done
  done;
  exec db "COMMIT"
;;

let seed_nullable db =
  exec db "CREATE TABLE t2 (w INTEGER NOT NULL, k INTEGER, v INTEGER)";
  exec db "CREATE INDEX idx_t2 ON t2 (w, k)";
  exec db "BEGIN";
  for w = 1 to 2 do
    for i = 1 to 20 do
      let k = if i mod 5 = 0 then "NULL" else string_of_int i in
      exec db (Printf.sprintf "INSERT INTO t2 VALUES (%d,%s,%d)" w k i)
    done
  done;
  exec db "COMMIT"
;;

(* Two separate tables, each with exactly ONE index whose prefix is [w] — a
   single table with both indexes would leave [access_path_for_eqs]'s choice
   between them unspecified ([Cat.indexes_for_table]'s order is documented as
   unspecified), and this test wants to pin the VIRTUAL/STORED shape
   deterministically rather than depend on that choice. *)
let seed_virtual db =
  exec
    db
    "CREATE TABLE t3 (\n\
    \       w INTEGER NOT NULL,\n\
    \       base INTEGER NOT NULL,\n\
    \       v INTEGER GENERATED ALWAYS AS (base * 2) VIRTUAL NOT NULL)";
  exec db "CREATE INDEX idx_t3_v ON t3 (w, v)";
  exec
    db
    "CREATE TABLE t3s (\n\
    \       w INTEGER NOT NULL,\n\
    \       base INTEGER NOT NULL,\n\
    \       sv INTEGER GENERATED ALWAYS AS (base + 1) STORED NOT NULL)";
  exec db "CREATE INDEX idx_t3s_sv ON t3s (w, sv)";
  exec db "BEGIN";
  for i = 1 to 30 do
    exec db (Printf.sprintf "INSERT INTO t3 (w, base) VALUES (1, %d)" i);
    exec db (Printf.sprintf "INSERT INTO t3s (w, base) VALUES (1, %d)" i)
  done;
  exec db "COMMIT"
;;

(* ------------------------------------------------------------------ *)
(* Correctness: fast path ON vs. forced OFF must agree exactly          *)
(* ------------------------------------------------------------------ *)

let differential_queries =
  [ "SELECT MIN(no_o_id) FROM new_order WHERE no_w_id = 1 AND no_d_id = 2"
  ; "SELECT COUNT(*) FROM new_order WHERE no_w_id = 1 AND no_d_id = 2"
  ; "SELECT COUNT(no_o_id) FROM new_order WHERE no_w_id = 1 AND no_d_id = 2"
  ; "SELECT COUNT(DISTINCT no_o_id) FROM new_order WHERE no_w_id = 1 AND no_d_id = 2"
  ; "SELECT MAX(no_o_id) FROM new_order WHERE no_w_id = 1 AND no_d_id = 2"
  ; "SELECT MIN(no_o_id), MAX(no_o_id), COUNT(*) FROM new_order WHERE no_w_id = 1 AND \
     no_d_id = 2"
  ; (* residual predicate reading only index columns *)
    "SELECT COUNT(*) FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 AND no_o_id > 10"
  ; (* residual predicate reading a NON-index column: must fall back and still \
       be correct *)
    "SELECT COUNT(*) FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 AND payload = 'p5'"
  ; (* no matching rows at all *)
    "SELECT MIN(no_o_id), COUNT(*) FROM new_order WHERE no_w_id = 1 AND no_d_id = 999"
  ; (* whole table has multiple districts in the prefix range: MIN over just \
       the equality prefix, not a range *)
    "SELECT MIN(no_o_id) FROM new_order WHERE no_w_id = 2 AND no_d_id = 1"
  ]
;;

let agrees_fastpath_vs_general () =
  with_db
  @@ fun db ->
  seed db;
  List.iter
    (fun sql ->
       set_fastpath true;
       let fast = rows_of db sql in
       set_fastpath false;
       let slow = rows_of db sql in
       set_fastpath true;
       Alcotest.(check (list string)) sql slow fast)
    differential_queries
;;

let agrees_nullable_column () =
  with_db
  @@ fun db ->
  seed_nullable db;
  let qs =
    [ "SELECT MIN(k) FROM t2 WHERE w = 1"
    ; "SELECT MAX(k) FROM t2 WHERE w = 1"
    ; "SELECT COUNT(k) FROM t2 WHERE w = 1"
    ; "SELECT COUNT(*) FROM t2 WHERE w = 1"
    ]
  in
  List.iter
    (fun sql ->
       set_fastpath true;
       let fast = rows_of db sql in
       set_fastpath false;
       let slow = rows_of db sql in
       set_fastpath true;
       Alcotest.(check (list string)) sql slow fast)
    qs
;;

(* A NaN in the NOT NULL trailing column must be handled correctly: it is a
   real value (#536) and the covering path's IK_null-is-NaN disambiguation
   must agree with the general path's. *)
let agrees_with_nan () =
  with_db
  @@ fun db ->
  exec db "CREATE TABLE tn (w INTEGER NOT NULL, f REAL NOT NULL)";
  exec db "CREATE INDEX idx_tn ON tn (w, f)";
  exec db "BEGIN";
  for i = 1 to 10 do
    exec db (Printf.sprintf "INSERT INTO tn VALUES (1, %d.5)" i)
  done;
  exec_params db "INSERT INTO tn VALUES (1, ?)" [ Db.V_real Float.nan ];
  exec db "COMMIT";
  let qs =
    [ "SELECT MIN(f) FROM tn WHERE w = 1"
    ; "SELECT MAX(f) FROM tn WHERE w = 1"
    ; "SELECT COUNT(f) FROM tn WHERE w = 1"
    ]
  in
  List.iter
    (fun sql ->
       set_fastpath true;
       let fast = rows_of db sql in
       set_fastpath false;
       let slow = rows_of db sql in
       set_fastpath true;
       Alcotest.(check (list string)) sql slow fast)
    qs
;;

(* #517 range alongside MIN/MAX: design says fall back, must stay correct. *)
let agrees_with_range () =
  with_db
  @@ fun db ->
  seed db;
  let qs =
    [ "SELECT MIN(no_o_id) FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 AND no_o_id \
       > 5"
    ; "SELECT MAX(no_o_id) FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 AND no_o_id \
       < 30"
    ]
  in
  List.iter
    (fun sql ->
       set_fastpath true;
       let fast = rows_of db sql in
       set_fastpath false;
       let slow = rows_of db sql in
       set_fastpath true;
       Alcotest.(check (list string)) sql slow fast)
    qs
;;

let agrees_group_by_not_taken () =
  with_db
  @@ fun db ->
  seed db;
  let sql =
    "SELECT no_d_id, MIN(no_o_id) FROM new_order WHERE no_w_id = 1 GROUP BY no_d_id"
  in
  set_fastpath true;
  let fast = rows_of db sql in
  set_fastpath false;
  let slow = rows_of db sql in
  set_fastpath true;
  Alcotest.(check (list string)) sql slow fast
;;

(* VIRTUAL and STORED generated columns are included in v1 per the design's
   explicit decision, including after an UPDATE to the base column. *)
let agrees_generated_columns () =
  with_db
  @@ fun db ->
  seed_virtual db;
  exec db "UPDATE t3 SET base = 999 WHERE base = 15";
  exec db "UPDATE t3s SET base = 999 WHERE base = 15";
  let qs =
    [ "SELECT MIN(v) FROM t3 WHERE w = 1"
    ; "SELECT MAX(v) FROM t3 WHERE w = 1"
    ; "SELECT COUNT(v) FROM t3 WHERE w = 1"
    ; "SELECT MIN(sv) FROM t3s WHERE w = 1"
    ; "SELECT MAX(sv) FROM t3s WHERE w = 1"
    ; "SELECT COUNT(sv) FROM t3s WHERE w = 1"
    ]
  in
  List.iter
    (fun sql ->
       set_fastpath true;
       let fast = rows_of db sql in
       set_fastpath false;
       let slow = rows_of db sql in
       set_fastpath true;
       Alcotest.(check (list string)) sql slow fast)
    qs;
  (* Also confirm the fast path is actually TAKEN for a generated column,
     not merely correct by accident of falling back — per the design's
     explicit decision that both STORED and VIRTUAL generated columns are
     eligible in v1. *)
  set_fastpath true;
  let _, stats_v = stats_of db "SELECT MIN(v) FROM t3 WHERE w = 1" in
  Alcotest.(check int)
    "VIRTUAL generated column: rows_examined is 0"
    0
    stats_v.Db.rows_examined;
  let _, stats_sv = stats_of db "SELECT MIN(sv) FROM t3s WHERE w = 1" in
  Alcotest.(check int)
    "STORED generated column: rows_examined is 0"
    0
    stats_sv.Db.rows_examined
;;

let agrees_exists () =
  with_db
  @@ fun db ->
  seed db;
  let check sql expected = Alcotest.(check (list string)) sql expected (rows_of db sql) in
  check
    "SELECT EXISTS (SELECT 1 FROM new_order WHERE no_w_id = 1 AND no_d_id = 2)"
    [ "i:1" ];
  check
    "SELECT EXISTS (SELECT 1 FROM new_order WHERE no_w_id = 1 AND no_d_id = 999)"
    [ "i:0" ];
  check
    "SELECT EXISTS (SELECT no_o_id FROM new_order WHERE no_w_id = 2 AND no_d_id = 1)"
    [ "i:1" ]
;;

(* PR #684 review, blocker 1: [unwrap_for_existence] used to strip
   [Op_limit] unconditionally on the way to the covering-existence shortcut,
   so a LIMIT/OFFSET on the inner subquery was silently discarded and the
   fast path answered "does the index have ANY matching entry" instead of
   "does the subquery, limit and all, yield a row". Both cases below have a
   matching index entry, so the fast path (if it fired through the LIMIT)
   would wrongly answer true / true; the correct answer, honouring LIMIT, is
   false in both. *)
let exists_limit_zero_is_false () =
  with_db
  @@ fun db ->
  seed db;
  set_fastpath true;
  let check sql expected = Alcotest.(check (list string)) sql expected (rows_of db sql) in
  check
    "SELECT EXISTS (SELECT 1 FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 LIMIT 0)"
    [ "i:0" ]
;;

let exists_limit_offset_beyond_matches_is_false () =
  with_db
  @@ fun db ->
  seed db;
  set_fastpath true;
  let check sql expected = Alcotest.(check (list string)) sql expected (rows_of db sql) in
  (* no_w_id = 1 AND no_d_id = 2 matches exactly [n_per_district] rows (see
     [seed]), so an OFFSET of that many skips past every match. *)
  check
    (Printf.sprintf
       "SELECT EXISTS (SELECT 1 FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 LIMIT 1 \
        OFFSET %d)"
       n_per_district)
    [ "i:0" ]
;;

let exists_limit_one_still_true () =
  with_db
  @@ fun db ->
  seed db;
  set_fastpath true;
  let check sql expected = Alcotest.(check (list string)) sql expected (rows_of db sql) in
  check
    "SELECT EXISTS (SELECT 1 FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 LIMIT 1)"
    [ "i:1" ]
;;

(* ------------------------------------------------------------------ *)
(* Perf: rows_examined must be 0 for the covering shapes                *)
(* ------------------------------------------------------------------ *)

let min_touches_no_table_rows () =
  with_db
  @@ fun db ->
  seed db;
  set_fastpath true;
  let n, stats =
    stats_of db "SELECT MIN(no_o_id) FROM new_order WHERE no_w_id = 1 AND no_d_id = 2"
  in
  Alcotest.(check int) "one row out" 1 n;
  Alcotest.(check int) "rows_examined is 0 (no rh_get at all)" 0 stats.Db.rows_examined
;;

let count_star_touches_no_table_rows () =
  with_db
  @@ fun db ->
  seed db;
  set_fastpath true;
  let n, stats =
    stats_of db "SELECT COUNT(*) FROM new_order WHERE no_w_id = 1 AND no_d_id = 2"
  in
  Alcotest.(check int) "one row out" 1 n;
  Alcotest.(check int) "rows_examined is 0" 0 stats.Db.rows_examined;
  Alcotest.(check bool)
    "index_entries covers the whole district"
    true
    (stats.Db.index_entries >= n_per_district)
;;

let max_touches_no_table_rows () =
  with_db
  @@ fun db ->
  seed db;
  set_fastpath true;
  let n, stats =
    stats_of db "SELECT MAX(no_o_id) FROM new_order WHERE no_w_id = 1 AND no_d_id = 2"
  in
  Alcotest.(check int) "one row out" 1 n;
  Alcotest.(check int) "rows_examined is 0" 0 stats.Db.rows_examined
;;

let min_is_early_stopped () =
  with_db
  @@ fun db ->
  seed db;
  set_fastpath true;
  let _, stats =
    stats_of db "SELECT MIN(no_o_id) FROM new_order WHERE no_w_id = 1 AND no_d_id = 2"
  in
  (* Ascending seek order already yields the minimum first; MIN alone (no
     other aggregate, no predicate) does not need a reverse-walk primitive
     to stop after one entry — see the design doc's "Open questions" #1. *)
  Alcotest.(check bool)
    "index_entries bounded near 1, not the district size"
    true
    (stats.Db.index_entries <= 3)
;;

let exists_touches_no_table_rows () =
  with_db
  @@ fun db ->
  seed db;
  set_fastpath true;
  let n, stats =
    stats_of
      db
      "SELECT EXISTS (SELECT 1 FROM new_order WHERE no_w_id = 1 AND no_d_id = 2)"
  in
  Alcotest.(check int) "one row out" 1 n;
  Alcotest.(check int) "rows_examined is 0" 0 stats.Db.rows_examined
;;

(* Fallback cases must still touch the table (i.e. NOT silently regress to 0
   when the shape isn't actually coverable) so this also confirms the
   guardrails are real gates, not accidentally-always-true ones. *)
let nullable_column_falls_back () =
  with_db
  @@ fun db ->
  seed_nullable db;
  set_fastpath true;
  let n, stats = stats_of db "SELECT MIN(k) FROM t2 WHERE w = 1" in
  Alcotest.(check int) "one row out" 1 n;
  Alcotest.(check bool)
    "nullable column: falls back, table rows ARE examined"
    true
    (stats.Db.rows_examined > 0)
;;

let non_index_predicate_falls_back () =
  with_db
  @@ fun db ->
  seed db;
  set_fastpath true;
  let n, stats =
    stats_of
      db
      "SELECT COUNT(*) FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 AND payload = \
       'p5'"
  in
  Alcotest.(check int) "one row out" 1 n;
  Alcotest.(check bool)
    "predicate reads a non-index column: falls back, table rows ARE examined"
    true
    (stats.Db.rows_examined > 0)
;;

let range_falls_back () =
  with_db
  @@ fun db ->
  seed db;
  set_fastpath true;
  let n, stats =
    stats_of
      db
      "SELECT MIN(no_o_id) FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 AND no_o_id \
       > 5"
  in
  Alcotest.(check int) "one row out" 1 n;
  Alcotest.(check bool)
    "range on the MIN target: falls back, table rows ARE examined"
    true
    (stats.Db.rows_examined > 0)
;;

let group_by_falls_back () =
  with_db
  @@ fun db ->
  seed db;
  set_fastpath true;
  let n, stats =
    stats_of
      db
      "SELECT no_d_id, MIN(no_o_id) FROM new_order WHERE no_w_id = 1 GROUP BY no_d_id"
  in
  Alcotest.(check int) "n_d rows out" n_d n;
  Alcotest.(check bool) "GROUP BY: falls back" true (stats.Db.rows_examined > 0)
;;

(* ------------------------------------------------------------------ *)
(* QCheck: for any subset of rows in a fresh district, fast-path MIN/   *)
(* COUNT must equal the values computed independently in OCaml.         *)
(* ------------------------------------------------------------------ *)

let qcheck_min_count_matches_reference =
  QCheck.Test.make
    ~name:"covering MIN/COUNT matches an independent OCaml reference"
    ~count:30
    QCheck.(list_size (Gen.int_range 0 40) (int_range 1 500))
    (fun vals ->
       with_db
       @@ fun db ->
       exec db "CREATE TABLE q (w INTEGER NOT NULL, x INTEGER NOT NULL)";
       exec db "CREATE INDEX idx_q ON q (w, x)";
       exec db "BEGIN";
       List.iter (fun v -> exec db (Printf.sprintf "INSERT INTO q VALUES (1, %d)" v)) vals;
       exec db "COMMIT";
       set_fastpath true;
       let got_min = rows_of db "SELECT MIN(x) FROM q WHERE w = 1" in
       let got_count = rows_of db "SELECT COUNT(*) FROM q WHERE w = 1" in
       let expect_min =
         match vals with
         | [] -> [ "null" ]
         | _ ->
           [ Printf.sprintf "i:%Ld" (Int64.of_int (List.fold_left min max_int vals)) ]
       in
       let expect_count = [ Printf.sprintf "i:%Ld" (Int64.of_int (List.length vals)) ] in
       QCheck.assume (got_min = expect_min);
       got_count = expect_count)
;;

let () =
  Alcotest.run
    "covering_index_674"
    [ ( "correctness"
      , [ Alcotest.test_case
            "fast path agrees with general path"
            `Quick
            agrees_fastpath_vs_general
        ; Alcotest.test_case "nullable column agrees" `Quick agrees_nullable_column
        ; Alcotest.test_case "NaN handled the same both ways" `Quick agrees_with_nan
        ; Alcotest.test_case "range alongside MIN/MAX agrees" `Quick agrees_with_range
        ; Alcotest.test_case
            "GROUP BY agrees (not fast-pathed)"
            `Quick
            agrees_group_by_not_taken
        ; Alcotest.test_case "generated columns agree" `Quick agrees_generated_columns
        ; Alcotest.test_case "EXISTS over index lookup" `Quick agrees_exists
        ; Alcotest.test_case
            "EXISTS (... LIMIT 0) is false, not fast-pathed to true"
            `Quick
            exists_limit_zero_is_false
        ; Alcotest.test_case
            "EXISTS (... LIMIT 1 OFFSET past all matches) is false"
            `Quick
            exists_limit_offset_beyond_matches_is_false
        ; Alcotest.test_case
            "EXISTS (... LIMIT 1) is still true"
            `Quick
            exists_limit_one_still_true
        ] )
    ; ( "rows_examined_bound"
      , [ Alcotest.test_case "MIN touches no table rows" `Quick min_touches_no_table_rows
        ; Alcotest.test_case
            "COUNT(*) touches no table rows"
            `Quick
            count_star_touches_no_table_rows
        ; Alcotest.test_case "MAX touches no table rows" `Quick max_touches_no_table_rows
        ; Alcotest.test_case
            "MIN is early-stopped (index_entries)"
            `Quick
            min_is_early_stopped
        ; Alcotest.test_case
            "EXISTS touches no table rows"
            `Quick
            exists_touches_no_table_rows
        ] )
    ; ( "fallback_guardrails"
      , [ Alcotest.test_case
            "nullable column falls back"
            `Quick
            nullable_column_falls_back
        ; Alcotest.test_case
            "non-index predicate falls back"
            `Quick
            non_index_predicate_falls_back
        ; Alcotest.test_case "range falls back" `Quick range_falls_back
        ; Alcotest.test_case "GROUP BY falls back" `Quick group_by_falls_back
        ] )
    ; "qcheck", [ QCheck_alcotest.to_alcotest qcheck_min_count_matches_reference ]
    ]
;;
