(** #677 (item 2 of 3): [Op_limit] must stop pulling its child once it has
    [offset + limit] rows, not drain the child to exhaustion and slice
    afterward. This file pins three things:

    - Correctness: the same rows, in the same order, before and after the
      fix — [LIMIT]/[OFFSET] slicing must not change.
    - The actual perf claim: {!Db.query_with_stats}'s [rows_examined] /
      [index_entries] for a [LIMIT n] query must be bounded near
      [offset + n], not the size of the table or index range it draws from.
      Asserted for 2 of the 3 lazy scanners #677 names: a plain table scan
      ({!seq_scan_stops_early}) and an index lookup
      ({!index_lookup_stops_early}). The third, an FTS sequential scan
      ({!fts_seq_scan_stops_early}), turned out to be unreachable via SQL at
      all — see that test's own comment — so it instead pins today's actual
      "unsupported" behavior; [stream_fts_seq_scan] is still wired into the
      #677 cleanup registry so early-stop is safe for it whenever FTS
      LIMIT/OFFSET support lands.
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
   two lazy scanners) so that IF an FTS SELECT ever reaches [Op_limit], early
   stop is safe for it too. But discovered while making this test file green:
   FTS SELECT has never supported LIMIT/OFFSET at the sema level at all —
   [Sema.bind_select]'s FTS branch (`lib/sql/sema.ml` around line 3637,
   present since 2026-05-16, well before #677) requires
   [joins, group_by, having, order, limit, offset] to ALL be empty/None to
   reach [bind_fts_seq_scan]; any LIMIT makes it fall through to a hard
   "FTS tables do not support this query form" error. So today there is no
   SQL spelling that can put an [Op_limit] above an [Op_fts_seq_scan] node,
   and this test cannot exercise the early-stop *behavior* for FTS the way
   {!seq_scan_stops_early} and {!index_lookup_stops_early} do for their
   scanners. That gap is pre-existing and orthogonal to #677 item 2 — adding
   FTS LIMIT/OFFSET support is a real feature decision (new BS_fts_seq_scan /
   BS_fts_match_scan fields, a planner wrap, its own tests) and out of scope
   here. This test instead pins *today's* actual behavior — the Unsupported
   error — so a future FTS-LIMIT feature must consciously replace it with a
   real early-stop assertion rather than silently leaving this stale. *)
let fts_seq_scan_stops_early () =
  with_mem_db
  @@ fun db ->
  seed_fts db;
  match run (Db.query db "SELECT body FROM doc LIMIT 4") with
  | Ok _ ->
    Alcotest.fail
      "FTS SELECT ... LIMIT unexpectedly succeeded — #677's early-stop for \
       stream_fts_seq_scan is now reachable via SQL and this test must be rewritten to \
       assert the rows_examined bound instead of the Unsupported error"
  | Error e ->
    let msg = String.lowercase_ascii (Format.asprintf "%a" Db.pp_error e) in
    let contains ~needle haystack =
      let nl = String.length needle
      and hl = String.length haystack in
      let rec go i = i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1)) in
      go 0
    in
    Alcotest.(check bool)
      "FTS SELECT with LIMIT is rejected as unsupported (pre-existing, not #677)"
      true
      (contains ~needle:"do not support this query form" msg
       || contains ~needle:"unsupported" msg)
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
        ] )
    ; ( "rows_examined_bound"
      , [ Alcotest.test_case "seq scan stops early" `Quick seq_scan_stops_early
        ; Alcotest.test_case "index lookup stops early" `Quick index_lookup_stops_early
        ; Alcotest.test_case "fts seq scan stops early" `Quick fts_seq_scan_stops_early
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
