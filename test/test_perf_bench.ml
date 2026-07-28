(** Phase 39 / #98 — Performance benchmark harness.

    Measures throughput / latency for representative SQL operations on
    both in-memory and file-backed (WAL-less and WAL) backends.  Numbers
    are printed to stderr — the test only fails on correctness errors,
    never on a slow measurement, so CI stability is preserved.

    Use [GRANARY_BENCH_N] to override the workload size (default 1000).

    What is measured:
    - bulk INSERT (1 commit)
    - point SELECT by indexed key
    - full-table scan SELECT
    - UPDATE by indexed key
    - DELETE by indexed key

    The output format is human-readable rather than machine-parseable;
    consumers wanting CI-tracked numbers can grep "BENCH:" lines.  This
    matches the SQLite practice of reporting numbers via stderr rather
    than encoding them in the test exit status. *)

module Db = struct
  include Granary.Db

  let open_file = Granary_unix.open_file
  let open_file_wal = Granary_unix.open_file_wal
end

let run = Lwt_main.run

let env_int key default =
  match Sys.getenv_opt key with
  | None -> default
  | Some s ->
    (try int_of_string s with
     | _ -> default)
;;

(* Default kept small (100) so the bench fits in default CI budgets.
   Override [GRANARY_BENCH_N=10000] for serious measurements. *)
let n = env_int "GRANARY_BENCH_N" 100

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error _ -> Alcotest.failf "bench exec failed: %s" sql
;;

let consume_query db sql =
  match run (Db.query db sql) with
  | Error _ -> Alcotest.failf "bench query failed: %s" sql
  | Ok stream -> ignore (run (Lwt_stream.to_list stream))
;;

let time_it label f =
  let t0 = Unix.gettimeofday () in
  f ();
  let dt = Unix.gettimeofday () -. t0 in
  Printf.eprintf "BENCH: %-50s %8.4f s\n%!" label dt;
  dt
;;

let throughput label ops dt =
  let rate = float_of_int ops /. dt in
  Printf.eprintf "BENCH: %-50s %10.0f ops/s\n%!" label rate
;;

let bench_inserts_bulk ~label db =
  let dt =
    time_it (label ^ ": bulk INSERT in 1 txn") (fun () ->
      exec db "BEGIN";
      for i = 0 to n - 1 do
        exec
          db
          (Printf.sprintf
             "INSERT INTO t (id, name, val) VALUES (%d, 'row-%d', %d)"
             i
             i
             (i * 3))
      done;
      exec db "COMMIT")
  in
  throughput (label ^ ": INSERT") n dt
;;

let bench_inserts_autocommit ~label db =
  let dt =
    time_it (label ^ ": INSERT autocommit") (fun () ->
      for i = n to (2 * n) - 1 do
        exec
          db
          (Printf.sprintf
             "INSERT INTO t (id, name, val) VALUES (%d, 'row-%d', %d)"
             i
             i
             (i * 3))
      done)
  in
  throughput (label ^ ": INSERT autocommit") n dt
;;

let bench_point_select ~label db =
  let dt =
    time_it (label ^ ": point SELECT by PK") (fun () ->
      for i = 0 to n - 1 do
        consume_query db (Printf.sprintf "SELECT name FROM t WHERE id = %d" i)
      done)
  in
  throughput (label ^ ": point SELECT") n dt
;;

let bench_full_scan ~label db =
  let dt =
    time_it (label ^ ": full SELECT scan ×10") (fun () ->
      for _ = 0 to 9 do
        consume_query db "SELECT id, name, val FROM t"
      done)
  in
  throughput (label ^ ": full scan rows") (10 * 2 * n) dt
;;

let bench_updates ~label db =
  let dt =
    time_it (label ^ ": UPDATE by PK") (fun () ->
      exec db "BEGIN";
      for i = 0 to n - 1 do
        exec db (Printf.sprintf "UPDATE t SET val = val + 1 WHERE id = %d" i)
      done;
      exec db "COMMIT")
  in
  throughput (label ^ ": UPDATE") n dt
;;

let bench_deletes ~label db =
  let dt =
    time_it (label ^ ": DELETE by PK") (fun () ->
      exec db "BEGIN";
      for i = 0 to n - 1 do
        exec db (Printf.sprintf "DELETE FROM t WHERE id = %d" i)
      done;
      exec db "COMMIT")
  in
  throughput (label ^ ": DELETE") n dt
;;

let setup_schema db =
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT, val INTEGER)"
;;

let run_suite ~label db =
  setup_schema db;
  bench_inserts_bulk ~label db;
  bench_inserts_autocommit ~label db;
  bench_point_select ~label db;
  bench_full_scan ~label db;
  bench_updates ~label db;
  bench_deletes ~label db
;;

let test_bench_mem () =
  Printf.eprintf "\n=== granary in-memory backend, N=%d ===\n%!" n;
  let db = run (Db.open_in_memory ()) in
  run_suite ~label:"mem" db;
  run (Db.close db)
;;

let test_bench_file () =
  Printf.eprintf "\n=== granary file backend, N=%d ===\n%!" n;
  let path = "/tmp/granary_phase39_bench.db" in
  (try Unix.unlink path with
   | _ -> ());
  let db =
    match run (Db.open_file ~path ()) with
    | Ok d -> d
    | Error _ -> Alcotest.fail "open_file failed"
  in
  run_suite ~label:"file" db;
  run (Db.close db);
  try Unix.unlink path with
  | _ -> ()
;;

let test_bench_wal () =
  Printf.eprintf "\n=== granary WAL backend, N=%d ===\n%!" n;
  let path = "/tmp/granary_phase39_bench_wal.db" in
  let wal = path ^ "-wal" in
  (try Unix.unlink path with
   | _ -> ());
  (try Unix.unlink wal with
   | _ -> ());
  let db =
    match run (Db.open_file_wal ~path ()) with
    | Ok d -> d
    | Error _ -> Alcotest.fail "open_file_wal failed"
  in
  run_suite ~label:"wal" db;
  run (Db.close db);
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink wal with
  | _ -> ()
;;

let () =
  Alcotest.run
    "perf_bench"
    [ ( "bench"
      , [ Alcotest.test_case "in-memory" `Slow test_bench_mem
        ; Alcotest.test_case "file backend" `Slow test_bench_file
        ; Alcotest.test_case "WAL backend" `Slow test_bench_wal
        ] )
    ]
;;
