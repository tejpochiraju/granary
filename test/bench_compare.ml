(** #222 — cross-engine benchmark: sqlocaml vs reference C SQLite.

    Synchronous timing harness over a common {!ENGINE} interface.  Each workload
    is timed with wall-clock ([Unix.gettimeofday]) and CPU time ([Unix.times]:
    utime+stime); [cpu/wall] is the CPU-bound vs I/O-bound signal for #156.

    NOT a CI pass/fail gate — it is a measurement tool.  Correctness is guarded
    by [SQLOCAML_BENCH_SMOKE=1] (cross-engine result equality on a tiny dataset).

    Output: CSV to stdout; human summary to stderr.  Tunables via env:
      SQLOCAML_BENCH_ROWS    dataset rows         (default 10000)
      SQLOCAML_BENCH_OPS     point-lookup ops     (default 5000)
      SQLOCAML_BENCH_SCANS   scan/agg repeats     (default 50)
      SQLOCAML_BENCH_COMMITS single-row txns      (default 200)
      SQLOCAML_BENCH_REPEATS timed repeats        (default 5; best of N reported)
      SQLOCAML_BENCH_PAGE_CACHE  page-cache pages (default 1024; mirrored to both engines)
      SQLOCAML_BENCH_HOST    host label for CSV   (default from Unix.gethostname)

    Each workload wraps its whole loop in ONE [Lwt_main.run] so sqlocaml's
    numbers reflect engine cost, not per-op event-loop entry overhead (the C
    reference has no scheduler).  CPU time comes from [Unix.times], whose ~10ms
    tick quantization means very light workloads can read [cpu_s ≈ 0]; size the
    op counts so each workload runs long enough for the ratio to be meaningful. *)

let run = Lwt_main.run

let env_int key default =
  match Sys.getenv_opt key with
  | Some s -> ( try int_of_string s with _ -> default)
  | None -> default

let rows_n = env_int "SQLOCAML_BENCH_ROWS" 10000
let ops_n = env_int "SQLOCAML_BENCH_OPS" 5000
let scans_n = env_int "SQLOCAML_BENCH_SCANS" 50
let commits_n = env_int "SQLOCAML_BENCH_COMMITS" 200
let repeats_n = env_int "SQLOCAML_BENCH_REPEATS" 5
let page_cache = env_int "SQLOCAML_BENCH_PAGE_CACHE" 1024

let host_label =
  match Sys.getenv_opt "SQLOCAML_BENCH_HOST" with
  | Some h when h <> "" -> h
  | _ -> ( try Unix.gethostname () with _ -> "unknown")

(* Deterministic pk sequence for point lookups — no wall-clock/random seed so
   both engines hit the SAME keys in the SAME order. Simple LCG mod rows. *)
let lookup_keys ~rows ~n =
  let a = Array.make n 0 in
  let x = ref 1234567 in
  for i = 0 to n - 1 do
    x := ((!x * 1103515245) + 12345) land 0x3FFFFFFF;
    a.(i) <- !x mod rows
  done;
  a

(* ── Common engine interface (synchronous from the harness's view) ────────── *)
module type ENGINE = sig
  type t

  val name : string

  (* [key=Some k] opens encrypted (32 bytes); [None] plaintext. May raise if the
     engine does not support encryption (the reference engine does not). *)
  val open_db : dir:string -> key:string option -> t
  val seed : t -> rows:int -> unit

  (* Each workload runs its full loop internally and returns ops performed. *)
  val w_point_lookup : t -> keys:int array -> int
  val w_scan_agg : t -> repeats:int -> int (* returns rows summed-over *)
  val w_insert_one : t -> n:int -> base:int -> int (* n autocommit inserts *)
  val w_insert_batch : t -> rows:int -> base:int -> int (* one txn of [rows] inserts *)
  val w_commit_n : t -> n:int -> base:int -> int (* n single-row txns (fsync each) *)
  val close : t -> unit
end

(* ── sqlocaml engine ──────────────────────────────────────────────────────── *)
module Sqlocaml : ENGINE = struct
  open Sqlocaml

  type t = { db : Db.t }

  let name = "sqlocaml"

  let unwrap = function
    | Ok v -> v
    | Error e -> Alcotest.failf "sqlocaml: %a" Db.pp_error e

  let open_db ~dir ~key =
    let path = Filename.concat dir "bench.db" in
    (try Unix.unlink path with _ -> ());
    (try Unix.unlink (path ^ "-wal") with _ -> ());
    let db =
      match key with
      | None -> unwrap (run (Sqlocaml_unix.open_file_wal ~path ()))
      | Some k -> (
        match run (Sqlocaml_unix.Store.open_file_wal ~key:k ~path ()) with
        | Error _ -> Alcotest.fail "sqlocaml: encrypted open failed"
        | Ok store -> run (Db.of_store ~file_path:path store))
    in
    { db }

  let exec t sql = ignore (unwrap (run (Db.execute t.db sql)))

  (* Lwt-native exec, for wrapping a whole workload loop in ONE [Lwt_main.run]
     so the per-statement event-loop entry cost of [exec] does not inflate
     sqlocaml's write-workload numbers against the scheduler-free C reference. *)
  let exec_lwt db sql =
    let open Lwt.Syntax in
    let* r = Db.execute db sql in
    match r with
    | Ok () -> Lwt.return_unit
    | Error e -> Alcotest.failf "sqlocaml: %a" Db.pp_error e

  let seed t ~rows =
    exec t "CREATE TABLE t (id INTEGER PRIMARY KEY, k INTEGER, payload TEXT)";
    exec t "BEGIN";
    for i = 0 to rows - 1 do
      exec t
        (Printf.sprintf
           "INSERT INTO t (id, k, payload) VALUES (%d, %d, 'payload-row-%d')" i
           (i * 7 mod rows) i)
    done;
    exec t "COMMIT"

  let w_point_lookup t ~keys =
    run
      (let open Lwt.Syntax in
       let* stmt =
         Lwt.map unwrap (Db.prepare t.db "SELECT payload FROM t WHERE id = ?")
       in
       let* () =
         Lwt_list.iter_s
           (fun pk ->
             let* stream =
               Lwt.map unwrap
                 (Db.iter stmt ~params:[ Db.V_int (Int64.of_int pk) ])
             in
             let* _ = Lwt_stream.to_list stream in
             Lwt.return_unit)
           (Array.to_list keys)
       in
       let* () = Db.finalize stmt in
       Lwt.return (Array.length keys))

  let w_scan_agg t ~repeats =
    let n = ref 0 in
    for _ = 1 to repeats do
      run
        (let open Lwt.Syntax in
         let* stream =
           Lwt.map unwrap (Db.query t.db "SELECT COUNT(*), SUM(k) FROM t")
         in
         let* rows = Lwt_stream.to_list stream in
         n := !n + List.length rows;
         Lwt.return_unit)
    done;
    !n

  let w_insert_one t ~n ~base =
    run
      (let open Lwt.Syntax in
       let* () =
         Lwt_list.iter_s
           (fun i ->
             let id = base + i in
             exec_lwt t.db
               (Printf.sprintf
                  "INSERT INTO t (id, k, payload) VALUES (%d, %d, 'ins-%d')" id id
                  id))
           (List.init n Fun.id)
       in
       Lwt.return n)

  let w_insert_batch t ~rows ~base =
    run
      (let open Lwt.Syntax in
       let* () = exec_lwt t.db "BEGIN" in
       let* () =
         Lwt_list.iter_s
           (fun i ->
             let id = base + i in
             exec_lwt t.db
               (Printf.sprintf
                  "INSERT INTO t (id, k, payload) VALUES (%d, %d, 'batch-%d')" id
                  id id))
           (List.init rows Fun.id)
       in
       let* () = exec_lwt t.db "COMMIT" in
       Lwt.return rows)

  let w_commit_n t ~n ~base =
    run
      (let open Lwt.Syntax in
       let* () =
         Lwt_list.iter_s
           (fun i ->
             let id = base + i in
             let* () = exec_lwt t.db "BEGIN" in
             let* () =
               exec_lwt t.db
                 (Printf.sprintf
                    "INSERT INTO t (id, k, payload) VALUES (%d, %d, 'commit-%d')"
                    id id id)
             in
             exec_lwt t.db "COMMIT")
           (List.init n Fun.id)
       in
       Lwt.return n)

  let close t = run (Db.close t.db)
end

(* ── timing harness ───────────────────────────────────────────────────────── *)
type sample = { ops : int; wall : float; cpu : float }

let cpu_now () =
  let tm = Unix.times () in
  tm.Unix.tms_utime +. tm.Unix.tms_stime

(* [warmup]: run [f] once (discarded) before timing.  Disabled for the
   single-shot write workloads, whose fixed id range would otherwise be
   inserted twice and trip the UNIQUE constraint on [id]. *)
let measure ?(warmup = true) ~repeats (f : unit -> int) : sample =
  if warmup then ignore (f ());
  let best = ref None in
  for _ = 1 to repeats do
    let c0 = cpu_now () and t0 = Unix.gettimeofday () in
    let ops = f () in
    let wall = Unix.gettimeofday () -. t0 in
    let cpu = cpu_now () -. c0 in
    let s = { ops; wall; cpu } in
    match !best with
    | Some b when b.wall <= wall -> ()
    | _ -> best := Some s
  done;
  match !best with Some s -> s | None -> { ops = 0; wall = 0.; cpu = 0. }

(* ── CSV emission ─────────────────────────────────────────────────────────── *)
let csv_header =
  "host,engine,workload,variant,rows,ops,wall_s,cpu_s,cpu_wall_ratio,ops_per_s"

let emit_row ~engine ~workload ~variant ~rows (s : sample) =
  let ratio = if s.wall > 0. then s.cpu /. s.wall else 0. in
  let ops_s = if s.wall > 0. then float_of_int s.ops /. s.wall else 0. in
  Printf.printf "%s,%s,%s,%s,%d,%d,%.6f,%.6f,%.3f,%.1f\n%!" host_label engine
    workload variant rows s.ops s.wall s.cpu ratio ops_s

(* ── workload driver for one engine ───────────────────────────────────────── *)
let run_engine (module E : ENGINE) ~variant ~key =
  let dir = Filename.temp_file "bench222-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let cleanup () =
    Array.iter
      (fun f -> try Sys.remove (Filename.concat dir f) with _ -> ())
      (try Sys.readdir dir with _ -> [||]);
    try Unix.rmdir dir with _ -> ()
  in
  Fun.protect ~finally:cleanup (fun () ->
      let t = E.open_db ~dir ~key in
      E.seed t ~rows:rows_n;
      let keys = lookup_keys ~rows:rows_n ~n:ops_n in
      emit_row ~engine:E.name ~workload:"point_lookup" ~variant ~rows:rows_n
        (measure ~repeats:repeats_n (fun () -> E.w_point_lookup t ~keys));
      emit_row ~engine:E.name ~workload:"scan_agg" ~variant ~rows:rows_n
        (measure ~repeats:repeats_n (fun () -> E.w_scan_agg t ~repeats:scans_n));
      (* write workloads use disjoint id ranges per repeat to avoid PK clashes;
         measured single-shot (repeats=1) since they mutate state. *)
      emit_row ~engine:E.name ~workload:"insert_one" ~variant ~rows:ops_n
        (measure ~warmup:false ~repeats:1 (fun () ->
             E.w_insert_one t ~n:ops_n ~base:rows_n));
      emit_row ~engine:E.name ~workload:"insert_batch" ~variant ~rows:rows_n
        (measure ~warmup:false ~repeats:1 (fun () ->
             E.w_insert_batch t ~rows:rows_n ~base:(rows_n + ops_n)));
      emit_row ~engine:E.name ~workload:"commit_n" ~variant ~rows:commits_n
        (measure ~warmup:false ~repeats:1 (fun () ->
             E.w_commit_n t ~n:commits_n ~base:((2 * rows_n) + ops_n)));
      E.close t)

(* ── main ─────────────────────────────────────────────────────────────────── *)
let () =
  (* Seed the RNG: the encrypted-store open path derives nonces/keys from it
     and fails closed if no generator is installed. *)
  Mirage_crypto_rng_unix.use_default ();
  (* pin sqlocaml page cache for parity (SQLite mirrored in Ref_sqlite, Task 3) *)
  Unix.putenv "SQLOCAML_PAGE_CACHE" (string_of_int page_cache);
  print_string (csv_header ^ "\n");
  run_engine (module Sqlocaml) ~variant:"plaintext" ~key:None;
  run_engine (module Sqlocaml) ~variant:"encrypted" ~key:(Some (String.make 32 'K'))
