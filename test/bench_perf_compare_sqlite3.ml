(** Performance baseline against the sqlite3 CLI binary.
    Extracted from test_perf_bench.ml in #370 to gate the real-SQLite
    invocation behind (optional) at build time rather than skipping at
    runtime.  Identical measurement logic; run manually when sqlite3 is
    available in PATH:

      dune exec test/bench_perf_compare_sqlite3.exe

    Use GRANARY_BENCH_N to override the workload size (default 100). *)

let env_int key default =
  match Sys.getenv_opt key with
  | None -> default
  | Some s ->
    (try int_of_string s with
     | _ -> default)
;;

let n = env_int "GRANARY_BENCH_N" 100

let throughput label ops dt =
  let rate = float_of_int ops /. dt in
  Printf.eprintf "BENCH: %-50s %10.0f ops/s\n%!" label rate
;;

let () =
  let path = "/tmp/granary_phase39_sqlite3_bench.db" in
  (try Unix.unlink path with
   | _ -> ());
  let sqlite3 =
    match Sys.command "command -v sqlite3 > /dev/null 2>&1" with
    | 0 -> Some "sqlite3"
    | _ -> None
  in
  (match sqlite3 with
   | None -> Printf.eprintf "\n=== sqlite3 CLI not available — skipping baseline ===\n%!"
   | Some bin ->
     Printf.eprintf "\n=== sqlite3 baseline (subprocess), N=%d ===\n%!" n;
     let script_path = "/tmp/granary_phase39_sqlite3_bench.sql" in
     let oc = open_out script_path in
     output_string oc "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT, val INTEGER);\n";
     output_string oc "BEGIN;\n";
     for i = 0 to n - 1 do
       Printf.fprintf
         oc
         "INSERT INTO t (id, name, val) VALUES (%d, 'row-%d', %d);\n"
         i
         i
         (i * 3)
     done;
     output_string oc "COMMIT;\n";
     close_out oc;
     let t0 = Unix.gettimeofday () in
     let exit_code =
       Sys.command (Printf.sprintf "%s %s < %s > /dev/null" bin path script_path)
     in
     let dt = Unix.gettimeofday () -. t0 in
     if exit_code = 0
     then (
       Printf.eprintf "BENCH: %-50s %8.4f s\n%!" "sqlite3 CLI: bulk INSERT in 1 txn" dt;
       throughput "sqlite3 CLI: INSERT" n dt)
     else Printf.eprintf "BENCH: sqlite3 subprocess exited %d (skipping)\n%!" exit_code;
     (try Unix.unlink script_path with
      | _ -> ()));
  try Unix.unlink path with
  | _ -> ()
;;
