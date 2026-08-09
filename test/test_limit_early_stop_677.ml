(** #677 (item 2 of 3): [Op_limit] must stop pulling its child once it has
    [offset + limit] rows, not drain the child to exhaustion and slice
    afterward. This file pins three things:

    - Correctness: the same rows, in the same order, before and after the
      fix — [LIMIT]/[OFFSET] slicing must not change.
    - The actual perf claim: {!Db.query_with_stats}'s [rows_examined] /
      [index_entries] for a [LIMIT n] query must be bounded near
      [offset + n], not the size of the table or index range it draws from.
      Asserted for all 3 lazy scanners #677 names: a plain table scan
      ({!seq_scan_stops_early}), an index lookup
      ({!index_lookup_stops_early}), and — since #679 wired LIMIT/OFFSET
      through the FTS sema/planner path — an FTS sequential scan
      ({!fts_seq_scan_stops_early}, {!fts_seq_scan_offset_stops_early}).
      #687 adds the FTS MATCH scan's content-fetch pass to this list —
      it cannot stop the score-sorting pass early (sorting needs every
      score), but the content fetch that follows is now bounded to the
      sliced window ({!fts_match_scan_stops_early},
      {!fts_match_scan_offset_stops_early}).
    - No leak: after a [LIMIT]-bounded query returns, on disk,
      {!Store.active_reader_count} / {!Store.live_read_locks} /
      {!Store.pinned_page_count} must be back at their pre-query baseline —
      the #164/#493/#546 discipline this change must not violate.
      [Mem] answers 0 for all three unconditionally, so this needs an
      on-disk store, same as {!Store.active_reader_count} in
      [test_correlated_exists_493.ml]. *)

open Lwt.Syntax
module Db = Granary.Db
module Store = Granary_store.Store

let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/granary_test_limit_early_stop_677_%04d.db" n
;;

let cleanup path =
  List.iter
    (fun p ->
       try Unix.unlink p with
       | _ -> ())
    [ path; path ^ "-wal"; path ^ ".aslog" ]
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
    List.map
      (fun r -> Array.to_list (Array.map render r))
      (run (Lwt_stream.to_list stream))
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

let with_mem_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

(* On-disk, with the raw [Store.t] kept in hand for the leak assertions —
   the [Mem] backend answers 0 for all three counters no matter what leaks. *)
let with_disk_db f =
  let path = fresh_path () in
  cleanup path;
  let store =
    match run (Granary_unix.Store.open_file ~path ()) with
    | Ok s -> s
    | Error e -> Alcotest.failf "open_file: %a" Store.pp_error e
  in
  let db = run (Db.of_store ~file_path:path store) in
  Fun.protect
    ~finally:(fun () ->
      (try run (Db.close db) with
       | _ -> ());
      cleanup path)
    (fun () -> f db store)
;;

let probes store =
  ( Store.active_reader_count store
  , Store.live_read_locks store
  , Store.pinned_page_count store )
;;

let check_released ~label store baseline =
  let br, bl, bp = baseline in
  let ar, al, ap = probes store in
  Alcotest.(check int) (label ^ ": live readers") br ar;
  Alcotest.(check int) (label ^ ": read locks") bl al;
  Alcotest.(check int) (label ^ ": pinned pages") bp ap
;;

(* ------------------------------------------------------------------ *)
(* Correctness: LIMIT/OFFSET slicing must not change                    *)
(* ------------------------------------------------------------------ *)

let n_rows = 200

let seed_plain db =
  exec db "CREATE TABLE wide (id INTEGER PRIMARY KEY, v INTEGER)";
  exec db "BEGIN";
  for id = 1 to n_rows do
    exec db (Printf.sprintf "INSERT INTO wide VALUES (%d, %d)" id (id * 10))
  done;
  exec db "COMMIT"
;;

let limit_returns_first_n () =
  with_mem_db
  @@ fun db ->
  seed_plain db;
  let got = rows_of db "SELECT v FROM wide LIMIT 5" in
  let expected = List.init 5 (fun i -> [ string_of_int ((i + 1) * 10) ]) in
  Alcotest.(check (list (list string))) "first 5" expected got
;;

let limit_offset_slices_middle () =
  with_mem_db
  @@ fun db ->
  seed_plain db;
  let got = rows_of db "SELECT v FROM wide LIMIT 3 OFFSET 2" in
  let expected = List.init 3 (fun i -> [ string_of_int ((i + 3) * 10) ]) in
  Alcotest.(check (list (list string))) "offset 2, limit 3" expected got
;;

let limit_past_end_returns_remainder () =
  with_mem_db
  @@ fun db ->
  seed_plain db;
  let got =
    rows_of db (Printf.sprintf "SELECT v FROM wide LIMIT 10 OFFSET %d" (n_rows - 3))
  in
  let expected = List.init 3 (fun i -> [ string_of_int ((n_rows - 3 + i + 1) * 10) ]) in
  Alcotest.(check (list (list string))) "offset near end" expected got
;;

let limit_zero_returns_nothing () =
  with_mem_db
  @@ fun db ->
  seed_plain db;
  let got = rows_of db "SELECT v FROM wide LIMIT 0" in
  Alcotest.(check (list (list string))) "limit 0" [] got
;;

(* ------------------------------------------------------------------ *)
(* rows_examined / index_entries bound — the perf claim, expected RED   *)
(* until Task 2's Op_limit change lands.                                *)
(* ------------------------------------------------------------------ *)

let seq_scan_stops_early () =
  with_mem_db
  @@ fun db ->
  seed_plain db;
  let n, stats = stats_of db "SELECT v FROM wide LIMIT 5" in
  Alcotest.(check int) "rows returned" 5 n;
  Alcotest.(check bool)
    "rows_examined bounded near the limit, not the table size"
    true
    (stats.Db.rows_examined <= 10)
;;

let seed_indexed db =
  exec db "CREATE TABLE t2 (id INTEGER PRIMARY KEY, k INTEGER, v INTEGER)";
  exec db "CREATE INDEX idx_t2_k ON t2 (k)";
  exec db "BEGIN";
  for id = 1 to n_rows do
    exec db (Printf.sprintf "INSERT INTO t2 VALUES (%d, 1, %d)" id id)
  done;
  exec db "COMMIT"
;;

let index_lookup_stops_early () =
  with_mem_db
  @@ fun db ->
  seed_indexed db;
  let n, stats = stats_of db "SELECT v FROM t2 WHERE k = 1 LIMIT 3 OFFSET 2" in
  let expected = List.init 3 (fun i -> string_of_int (i + 3)) in
  Alcotest.(check int) "rows returned" 3 n;
  Alcotest.(check bool)
    "index_entries bounded near offset+limit, not the matching-key count"
    true
    (stats.Db.index_entries <= 10);
  ignore expected
;;

let seed_fts db =
  exec db "CREATE VIRTUAL TABLE doc USING fts5(body)";
  exec db "BEGIN";
  for id = 1 to n_rows do
    exec db (Printf.sprintf "INSERT INTO doc VALUES ('widget %d')" id)
  done;
  exec db "COMMIT"
;;

(* [stream_fts_seq_scan] was wired into the #677 cleanup registry (it
   registers its [finish] via [register_stream_cleanup], same as the other
   two lazy scanners) so that once an FTS SELECT could reach [Op_limit],
   early stop would already be safe for it. #679 closed the remaining gap —
   [Sema.bind_select]'s FTS branch used to require
   [joins, group_by, having, order, limit, offset] to ALL be empty/None to
   bind at all, so no SQL spelling could ever put an [Op_limit] above an
   [Op_fts_seq_scan]/[Op_fts_match_scan] node. [BS_fts_seq_scan] and
   [BS_fts_match_scan] now carry their own [limit]/[offset], and the planner
   wraps their base op with [Op_limit] via [finalize_select], exactly like
   the plain-table path. This test now exercises the real early-stop
   behavior [seq_scan_stops_early] and [index_lookup_stops_early] already
   pin for their scanners. *)
let fts_seq_scan_stops_early () =
  with_mem_db
  @@ fun db ->
  seed_fts db;
  let n, stats = stats_of db "SELECT body FROM doc LIMIT 4" in
  Alcotest.(check int) "rows returned" 4 n;
  Alcotest.(check bool)
    "rows_examined bounded near the limit, not the table size"
    true
    (stats.Db.rows_examined <= 10)
;;

(* #679: OFFSET must also stop early, not drain the cursor to exhaustion. *)
let fts_seq_scan_offset_stops_early () =
  with_mem_db
  @@ fun db ->
  seed_fts db;
  let n, stats = stats_of db "SELECT body FROM doc LIMIT 3 OFFSET 5" in
  Alcotest.(check int) "rows returned" 3 n;
  Alcotest.(check bool)
    "rows_examined bounded near offset+limit, not the table size"
    true
    (stats.Db.rows_examined <= 15)
;;

(* #687: [stream_fts_match_scan] used to sort by score (score only, no
   content) and then fetch content + compute snippets for EVERY matched row
   via [Lwt_list.filter_map_s] over the full sorted list, before [Op_limit]
   ever sliced the stream. So [LIMIT 3] against 200 matches did 200
   content-tree fetches to return 3 rows. #687 threads [limit]/[offset] into
   [Op_fts_match_scan] itself and slices the sorted-by-score list to the
   [offset..offset+limit) window BEFORE the content-fetch loop, so
   [rows_examined] (incremented once per fetch attempt, ahead of the
   [S.get]) is now bounded near [offset + limit], not the full match
   count. *)
let fts_match_scan_stops_early () =
  with_mem_db
  @@ fun db ->
  seed_fts db;
  let n, stats = stats_of db "SELECT body FROM doc WHERE doc MATCH 'widget' LIMIT 3" in
  Alcotest.(check int) "rows returned" 3 n;
  Alcotest.(check bool)
    "rows_examined bounded near the limit, not the full match count"
    true
    (stats.Db.rows_examined <= 10)
;;

(* #687: OFFSET must also be respected before the content fetch, not just
   LIMIT. *)
let fts_match_scan_offset_stops_early () =
  with_mem_db
  @@ fun db ->
  seed_fts db;
  let n, stats =
    stats_of db "SELECT body FROM doc WHERE doc MATCH 'widget' LIMIT 3 OFFSET 5"
  in
  Alcotest.(check int) "rows returned" 3 n;
  Alcotest.(check bool)
    "rows_examined bounded near offset+limit, not the full match count"
    true
    (stats.Db.rows_examined <= 15)
;;

(* #679 correctness: LIMIT/OFFSET on a plain (no MATCH) FTS scan must slice
   the same way the plain-table path does — pin against the unlimited
   result, same run so no scan-order assumption is needed. *)
let fts_seq_scan_limit_offset_correctness () =
  with_mem_db
  @@ fun db ->
  seed_fts db;
  let full = rows_of db "SELECT body FROM doc" in
  Alcotest.(check int) "full count" n_rows (List.length full);
  let first4 = rows_of db "SELECT body FROM doc LIMIT 4" in
  Alcotest.(check (list (list string)))
    "limit 4"
    (List.filteri (fun i _ -> i < 4) full)
    first4;
  let middle = rows_of db "SELECT body FROM doc LIMIT 3 OFFSET 5" in
  Alcotest.(check (list (list string)))
    "offset 5 limit 3"
    (List.filteri (fun i _ -> i >= 5 && i < 8) full)
    middle
;;

(* #679 correctness: LIMIT/OFFSET on a MATCH query. [stream_fts_match_scan]
   sorts and materializes its whole match set up front (score sorting needs
   the full set) — there is still no early-stop claim for the SCORING pass —
   but #687 bounds the CONTENT-FETCH pass that follows to the sliced window,
   so this pins that the answer is still exactly [Op_limit]'s slice of the
   unlimited MATCH result, same run. See [fts_match_scan_stops_early] below
   for the fetch-count bound itself. *)
let fts_match_scan_limit_offset_correctness () =
  with_mem_db
  @@ fun db ->
  seed_fts db;
  let full = rows_of db "SELECT body FROM doc WHERE doc MATCH 'widget'" in
  Alcotest.(check int) "full match count" n_rows (List.length full);
  let first3 = rows_of db "SELECT body FROM doc WHERE doc MATCH 'widget' LIMIT 3" in
  Alcotest.(check (list (list string)))
    "limit 3"
    (List.filteri (fun i _ -> i < 3) full)
    first3;
  let mid = rows_of db "SELECT body FROM doc WHERE doc MATCH 'widget' LIMIT 4 OFFSET 5" in
  Alcotest.(check (list (list string)))
    "offset 5 limit 4"
    (List.filteri (fun i _ -> i >= 5 && i < 9) full)
    mid
;;

(* ------------------------------------------------------------------ *)
(* No leak: readers/pins/locks return to baseline after a LIMIT query   *)
(* ------------------------------------------------------------------ *)

let limit_query_leaves_no_reader_behind () =
  with_disk_db
  @@ fun db store ->
  seed_plain db;
  let baseline = probes store in
  let n = List.length (rows_of db "SELECT v FROM wide LIMIT 5") in
  Alcotest.(check int) "rows returned" 5 n;
  check_released ~label:"seq scan LIMIT" store baseline
;;

let index_lookup_limit_leaves_no_reader_behind () =
  with_disk_db
  @@ fun db store ->
  seed_indexed db;
  let baseline = probes store in
  let n = List.length (rows_of db "SELECT v FROM t2 WHERE k = 1 LIMIT 3 OFFSET 2") in
  Alcotest.(check int) "rows returned" 3 n;
  check_released ~label:"index lookup LIMIT" store baseline
;;

let () =
  Alcotest.run
    "limit_early_stop_677"
    [ ( "correctness"
      , [ Alcotest.test_case "limit returns first n" `Quick limit_returns_first_n
        ; Alcotest.test_case
            "limit+offset slices middle"
            `Quick
            limit_offset_slices_middle
        ; Alcotest.test_case
            "limit past end returns remainder"
            `Quick
            limit_past_end_returns_remainder
        ; Alcotest.test_case "limit 0 returns nothing" `Quick limit_zero_returns_nothing
        ; Alcotest.test_case
            "fts seq scan limit/offset correctness"
            `Quick
            fts_seq_scan_limit_offset_correctness
        ; Alcotest.test_case
            "fts match scan limit/offset correctness"
            `Quick
            fts_match_scan_limit_offset_correctness
        ] )
    ; ( "rows_examined_bound"
      , [ Alcotest.test_case "seq scan stops early" `Quick seq_scan_stops_early
        ; Alcotest.test_case "index lookup stops early" `Quick index_lookup_stops_early
        ; Alcotest.test_case "fts seq scan stops early" `Quick fts_seq_scan_stops_early
        ; Alcotest.test_case
            "fts seq scan offset stops early"
            `Quick
            fts_seq_scan_offset_stops_early
        ; Alcotest.test_case
            "fts match scan stops early"
            `Quick
            fts_match_scan_stops_early
        ; Alcotest.test_case
            "fts match scan offset stops early"
            `Quick
            fts_match_scan_offset_stops_early
        ] )
    ; ( "no_leak"
      , [ Alcotest.test_case
            "limit query leaves no reader behind"
            `Quick
            limit_query_leaves_no_reader_behind
        ; Alcotest.test_case
            "index lookup limit leaves no reader behind"
            `Quick
            index_lookup_limit_leaves_no_reader_behind
        ] )
    ]
;;
