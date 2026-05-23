(** Phase 38 / #149 -- WAL reader-scaling benchmark.

    Compares serial vs Lwt-parallel SELECT throughput on a WAL-backed
    DB.  Under single-threaded Lwt the goal is "parallel time <=
    PARALLEL_MAX x serial time" -- i.e. cooperative interleaving does
    not regress vs sequential execution.

    Env vars:
      SQLOCAML_BENCH_READERS  (default 8)
      SQLOCAML_BENCH_PER      (default 200)
      SQLOCAML_BENCH_PARALLEL_MAX (default 2.0)
*)

module Db = Sqlocaml.Db
let run = Lwt_main.run

let getenv_int k d = try int_of_string (Sys.getenv k) with _ -> d
let getenv_float k d = try float_of_string (Sys.getenv k) with _ -> d

let path = "/tmp/sqlocaml_phase38_bench.db"

let setup () =
  (try Unix.unlink path with _ -> ());
  (try Unix.unlink (path ^ "-wal") with _ -> ());
  let db = match run (Db.open_file_wal ~path) with
    | Ok d -> d | Error _ -> failwith "open failed"
  in
  (match run (Db.execute db "CREATE TABLE t (n INTEGER)") with
   | Ok _ -> () | Error _ -> failwith "ddl");
  for i = 0 to 999 do
    match run (Db.execute db (Printf.sprintf "INSERT INTO t VALUES (%d)" i))
    with Ok _ -> () | Error _ -> failwith "seed"
  done;
  db

let bench db ~n_readers ~per =
  let open Lwt.Infix in
  let make_reader _ =
    let rec loop i =
      if i >= per then Lwt.return ()
      else
        Db.query db "SELECT COUNT(*) FROM t" >>= function
        | Error _ -> failwith "select"
        | Ok stream ->
          Lwt_stream.to_list stream >>= fun _ -> loop (i + 1)
    in loop 0
  in
  let t0 = Unix.gettimeofday () in
  run (Lwt.join (List.init n_readers make_reader));
  Unix.gettimeofday () -. t0

let test_scaling () =
  let n_readers = getenv_int "SQLOCAML_BENCH_READERS" 8 in
  let per = getenv_int "SQLOCAML_BENCH_PER" 200 in
  let parallel_max = getenv_float "SQLOCAML_BENCH_PARALLEL_MAX" 2.0 in
  let db = setup () in
  let t_serial = bench db ~n_readers:1 ~per:(n_readers * per) in
  let t_par    = bench db ~n_readers ~per in
  let ratio = t_par /. t_serial in
  Printf.printf "serial=%.3fs parallel=%.3fs ratio=%.2f (max %.2f)\n%!"
    t_serial t_par ratio parallel_max;
  run (Db.close db);
  (try Unix.unlink path with _ -> ());
  (try Unix.unlink (path ^ "-wal") with _ -> ());
  Alcotest.(check bool)
    (Printf.sprintf "parallel/serial ratio %.2f <= %.2f" ratio parallel_max)
    true (ratio <= parallel_max)

let () =
  Alcotest.run "wal_reader_scaling" [
    "bench", [
      Alcotest.test_case "parallel readers do not regress vs serial"
        `Slow test_scaling;
    ]
  ]
