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
  | Some s ->
    (try int_of_string s with
     | _ -> default)
  | None -> default
;;

let rows_n = env_int "SQLOCAML_BENCH_ROWS" 10000
let ops_n = env_int "SQLOCAML_BENCH_OPS" 5000
let scans_n = env_int "SQLOCAML_BENCH_SCANS" 50
let commits_n = env_int "SQLOCAML_BENCH_COMMITS" 200
let repeats_n = env_int "SQLOCAML_BENCH_REPEATS" 5
let page_cache = env_int "SQLOCAML_BENCH_PAGE_CACHE" 1024

let host_label =
  match Sys.getenv_opt "SQLOCAML_BENCH_HOST" with
  | Some h when h <> "" -> h
  | _ ->
    (try Unix.gethostname () with
     | _ -> "unknown")
;;

(* #332: per-deployment durability mode for the sqlocaml engine's WAL commits.
   Selected by [SQLOCAML_BENCH_DURABILITY] (full|batched|off|sweep); the
   SQLite reference always runs synchronous=FULL.  Held in a ref so the sweep
   mode can re-open the same engine under each mode in turn.  The default
   ([None]) preserves the original #222 behaviour exactly (Full, untagged
   variants). *)
let sqlocaml_durability = ref Sqlocaml_store.Store.Full

let durability_label (d : Sqlocaml_store.Store.durability) =
  match d with
  | Sqlocaml_store.Store.Full -> "full"
  | Sqlocaml_store.Store.Off -> "off"
  | Sqlocaml_store.Store.Batched { commits; interval_ms } ->
    Printf.sprintf "batched-n%d-t%dms" commits interval_ms
;;

let batched_from_env () =
  Sqlocaml_store.Store.Batched
    { commits = env_int "SQLOCAML_BENCH_BATCH_N" 256
    ; interval_ms = env_int "SQLOCAML_BENCH_BATCH_T_MS" 100
    }
;;

let parse_durability s : Sqlocaml_store.Store.durability =
  match String.lowercase_ascii s with
  | "full" -> Sqlocaml_store.Store.Full
  | "off" -> Sqlocaml_store.Store.Off
  | "batched" -> batched_from_env ()
  | other ->
    failwith
      (Printf.sprintf
         "SQLOCAML_BENCH_DURABILITY: unknown mode %s (full|batched|off|sweep)"
         other)
;;

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
;;

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
  val fingerprint : t -> string (* canonical table digest for cross-engine equality *)
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
  ;;

  let open_db ~dir ~key =
    let path = Filename.concat dir "bench.db" in
    (try Unix.unlink path with
     | _ -> ());
    (try Unix.unlink (path ^ "-wal") with
     | _ -> ());
    (* #332: a real wall-clock so the batched T trigger fires (the default clock
       returns 0, disabling the time-based fsync); forward the selected
       durability mode to the WAL commit path. Harmless under Full. *)
    let db =
      match key with
      | None ->
        unwrap
          (run
             (Sqlocaml_unix.open_file_wal
                ~durability:!sqlocaml_durability
                ~clock:Unix.gettimeofday
                ~path
                ()))
      | Some k ->
        (match run (Sqlocaml_unix.Store.open_file_wal ~key:k ~path ()) with
         | Error _ -> Alcotest.fail "sqlocaml: encrypted open failed"
         | Ok store ->
           run
             (Db.of_store
                ~file_path:path
                ~durability:!sqlocaml_durability
                ~clock:Unix.gettimeofday
                store))
    in
    { db }
  ;;

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
  ;;

  let seed t ~rows =
    exec t "CREATE TABLE t (id INTEGER PRIMARY KEY, k INTEGER, payload TEXT)";
    exec t "BEGIN";
    for i = 0 to rows - 1 do
      exec
        t
        (Printf.sprintf
           "INSERT INTO t (id, k, payload) VALUES (%d, %d, 'payload-row-%d')"
           i
           (i * 7 mod rows)
           i)
    done;
    exec t "COMMIT"
  ;;

  let scalar_str t sql =
    run
      (let open Lwt.Syntax in
       let* s = Lwt.map unwrap (Db.query t.db sql) in
       let* rows = Lwt_stream.to_list s in
       match rows with
       | r :: _ when Array.length r > 0 ->
         Lwt.return
           (match r.(0) with
            | Db.V_int i -> Int64.to_string i
            | Db.V_text s -> s
            | Db.V_null -> "NULL"
            | Db.V_real f -> Printf.sprintf "%.0f" f
            | Db.V_blob _ -> "<blob>")
       | _ -> Lwt.return "NULL")
  ;;

  let fingerprint t =
    Printf.sprintf
      "count=%s sum=%s p0=%s"
      (scalar_str t "SELECT COUNT(*) FROM t")
      (scalar_str t "SELECT SUM(k) FROM t")
      (scalar_str t "SELECT payload FROM t WHERE id = 0")
  ;;

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
                Lwt.map unwrap (Db.iter stmt ~params:[ Db.V_int (Int64.of_int pk) ])
              in
              let* _ = Lwt_stream.to_list stream in
              Lwt.return_unit)
           (Array.to_list keys)
       in
       let* () = Db.finalize stmt in
       Lwt.return (Array.length keys))
  ;;

  let w_scan_agg t ~repeats =
    let n = ref 0 in
    for _ = 1 to repeats do
      run
        (let open Lwt.Syntax in
         let* stream = Lwt.map unwrap (Db.query t.db "SELECT COUNT(*), SUM(k) FROM t") in
         let* rows = Lwt_stream.to_list stream in
         n := !n + List.length rows;
         Lwt.return_unit)
    done;
    !n
  ;;

  (* Prepare once, bind+run per row — same shape as the SQLite reference, so the
     comparison isolates engine cost rather than sqlocaml's re-parse/re-plan. *)
  let insert_sql = "INSERT INTO t (id, k, payload) VALUES (?, ?, ?)"

  let run_insert stmt ~id ~tag =
    let open Lwt.Syntax in
    let* _ =
      Lwt.map
        unwrap
        (Db.run
           stmt
           ~params:
             [ Db.V_int (Int64.of_int id)
             ; Db.V_int (Int64.of_int id)
             ; Db.V_text (Printf.sprintf "%s-%d" tag id)
             ])
    in
    Lwt.return_unit
  ;;

  let w_insert_one t ~n ~base =
    run
      (let open Lwt.Syntax in
       let* stmt = Lwt.map unwrap (Db.prepare t.db insert_sql) in
       let* () =
         Lwt_list.iter_s
           (fun i -> run_insert stmt ~id:(base + i) ~tag:"ins")
           (List.init n Fun.id)
       in
       let* () = Db.finalize stmt in
       Lwt.return n)
  ;;

  let w_insert_batch t ~rows ~base =
    run
      (let open Lwt.Syntax in
       let* () = exec_lwt t.db "BEGIN" in
       let* stmt = Lwt.map unwrap (Db.prepare t.db insert_sql) in
       let* () =
         Lwt_list.iter_s
           (fun i -> run_insert stmt ~id:(base + i) ~tag:"batch")
           (List.init rows Fun.id)
       in
       let* () = Db.finalize stmt in
       let* () = exec_lwt t.db "COMMIT" in
       Lwt.return rows)
  ;;

  let w_commit_n t ~n ~base =
    run
      (let open Lwt.Syntax in
       let* stmt = Lwt.map unwrap (Db.prepare t.db insert_sql) in
       let* () =
         Lwt_list.iter_s
           (fun i ->
              let* () = exec_lwt t.db "BEGIN" in
              let* () = run_insert stmt ~id:(base + i) ~tag:"commit" in
              exec_lwt t.db "COMMIT")
           (List.init n Fun.id)
       in
       let* () = Db.finalize stmt in
       Lwt.return n)
  ;;

  let close t = run (Db.close t.db)
end

(* ── reference C SQLite engine (in-process bindings) ──────────────────────── *)
module Ref_sqlite : ENGINE = struct
  type t = { db : Sqlite3.db }

  let name = "sqlite"

  let ok rc =
    match rc with
    | Sqlite3.Rc.OK | Sqlite3.Rc.DONE | Sqlite3.Rc.ROW -> ()
    | r -> failwith ("sqlite3: " ^ Sqlite3.Rc.to_string r)
  ;;

  let exec t sql = ok (Sqlite3.exec t.db sql)

  let open_db ~dir ~key =
    (match key with
     | Some _ -> failwith "Ref_sqlite: encrypted reference not supported (no sqlcipher)"
     | None -> ());
    let path = Filename.concat dir "ref.db" in
    (try Unix.unlink path with
     | _ -> ());
    let db = Sqlite3.db_open path in
    let t = { db } in
    (* parity: same page size + cache page count as sqlocaml; WAL like sqlocaml. *)
    exec t "PRAGMA page_size=4096";
    exec t (Printf.sprintf "PRAGMA cache_size=%d" page_cache);
    exec t "PRAGMA journal_mode=WAL";
    exec t "PRAGMA synchronous=FULL";
    (* match sqlocaml's fsync-per-commit durability *)
    t
  ;;

  let seed t ~rows =
    exec t "CREATE TABLE t (id INTEGER PRIMARY KEY, k INTEGER, payload TEXT)";
    exec t "BEGIN";
    let stmt = Sqlite3.prepare t.db "INSERT INTO t (id,k,payload) VALUES (?,?,?)" in
    for i = 0 to rows - 1 do
      ok (Sqlite3.reset stmt);
      ok (Sqlite3.bind_int64 stmt 1 (Int64.of_int i));
      ok (Sqlite3.bind_int64 stmt 2 (Int64.of_int (i * 7 mod rows)));
      ok (Sqlite3.bind_text stmt 3 (Printf.sprintf "payload-row-%d" i));
      ok (Sqlite3.step stmt)
    done;
    ok (Sqlite3.finalize stmt);
    exec t "COMMIT"
  ;;

  let drain stmt =
    let n = ref 0 in
    let rec loop () =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
        incr n;
        loop ()
      | Sqlite3.Rc.DONE -> ()
      | r -> failwith ("sqlite3 step: " ^ Sqlite3.Rc.to_string r)
    in
    loop ();
    !n
  ;;

  let w_point_lookup t ~keys =
    let stmt = Sqlite3.prepare t.db "SELECT payload FROM t WHERE id = ?" in
    Array.iter
      (fun pk ->
         ok (Sqlite3.reset stmt);
         ok (Sqlite3.bind_int64 stmt 1 (Int64.of_int pk));
         ignore (drain stmt))
      keys;
    ok (Sqlite3.finalize stmt);
    Array.length keys
  ;;

  let w_scan_agg t ~repeats =
    let n = ref 0 in
    for _ = 1 to repeats do
      let stmt = Sqlite3.prepare t.db "SELECT COUNT(*), SUM(k) FROM t" in
      n := !n + drain stmt;
      ok (Sqlite3.finalize stmt)
    done;
    !n
  ;;

  let w_insert_one t ~n ~base =
    let stmt = Sqlite3.prepare t.db "INSERT INTO t (id,k,payload) VALUES (?,?,?)" in
    for i = 0 to n - 1 do
      let id = base + i in
      ok (Sqlite3.reset stmt);
      ok (Sqlite3.bind_int64 stmt 1 (Int64.of_int id));
      ok (Sqlite3.bind_int64 stmt 2 (Int64.of_int id));
      ok (Sqlite3.bind_text stmt 3 (Printf.sprintf "ins-%d" id));
      ok (Sqlite3.step stmt)
    done;
    ok (Sqlite3.finalize stmt);
    n
  ;;

  let w_insert_batch t ~rows ~base =
    exec t "BEGIN";
    let stmt = Sqlite3.prepare t.db "INSERT INTO t (id,k,payload) VALUES (?,?,?)" in
    for i = 0 to rows - 1 do
      let id = base + i in
      ok (Sqlite3.reset stmt);
      ok (Sqlite3.bind_int64 stmt 1 (Int64.of_int id));
      ok (Sqlite3.bind_int64 stmt 2 (Int64.of_int id));
      ok (Sqlite3.bind_text stmt 3 (Printf.sprintf "batch-%d" id));
      ok (Sqlite3.step stmt)
    done;
    ok (Sqlite3.finalize stmt);
    exec t "COMMIT";
    rows
  ;;

  let w_commit_n t ~n ~base =
    let stmt = Sqlite3.prepare t.db "INSERT INTO t (id,k,payload) VALUES (?,?,?)" in
    for i = 0 to n - 1 do
      let id = base + i in
      exec t "BEGIN";
      ok (Sqlite3.reset stmt);
      ok (Sqlite3.bind_int64 stmt 1 (Int64.of_int id));
      ok (Sqlite3.bind_int64 stmt 2 (Int64.of_int id));
      ok (Sqlite3.bind_text stmt 3 (Printf.sprintf "commit-%d" id));
      ok (Sqlite3.step stmt);
      exec t "COMMIT"
    done;
    ok (Sqlite3.finalize stmt);
    n
  ;;

  let scalar_str t sql =
    let stmt = Sqlite3.prepare t.db sql in
    let v =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
        (match Sqlite3.column stmt 0 with
         | Sqlite3.Data.NULL -> "NULL"
         | d -> Sqlite3.Data.to_string_coerce d)
      | _ -> "NULL"
    in
    ok (Sqlite3.finalize stmt);
    v
  ;;

  let fingerprint t =
    Printf.sprintf
      "count=%s sum=%s p0=%s"
      (scalar_str t "SELECT COUNT(*) FROM t")
      (scalar_str t "SELECT SUM(k) FROM t")
      (scalar_str t "SELECT payload FROM t WHERE id = 0")
  ;;

  let close t = ignore (Sqlite3.db_close t.db)
end

(* ── timing harness ───────────────────────────────────────────────────────── *)
type sample =
  { ops : int
  ; wall : float
  ; cpu : float
  }

let cpu_now () =
  let tm = Unix.times () in
  tm.Unix.tms_utime +. tm.Unix.tms_stime
;;

(* [warmup]: run [f] once (discarded) before timing.  Disabled for the
   single-shot write workloads, whose fixed id range would otherwise be
   inserted twice and trip the UNIQUE constraint on [id]. *)
let measure ?(warmup = true) ~repeats (f : unit -> int) : sample =
  if warmup then ignore (f ());
  let best = ref None in
  for _ = 1 to repeats do
    let c0 = cpu_now ()
    and t0 = Unix.gettimeofday () in
    let ops = f () in
    let wall = Unix.gettimeofday () -. t0 in
    let cpu = cpu_now () -. c0 in
    let s = { ops; wall; cpu } in
    match !best with
    | Some b when b.wall <= wall -> ()
    | _ -> best := Some s
  done;
  match !best with
  | Some s -> s
  | None -> { ops = 0; wall = 0.; cpu = 0. }
;;

(* ── CSV emission ─────────────────────────────────────────────────────────── *)
let csv_header =
  "host,engine,workload,variant,rows,ops,wall_s,cpu_s,cpu_wall_ratio,ops_per_s"
;;

let emit_row ~engine ~workload ~variant ~rows (s : sample) =
  let ratio = if s.wall > 0. then s.cpu /. s.wall else 0. in
  let ops_s = if s.wall > 0. then float_of_int s.ops /. s.wall else 0. in
  Printf.printf
    "%s,%s,%s,%s,%d,%d,%.6f,%.6f,%.3f,%.1f\n%!"
    host_label
    engine
    workload
    variant
    rows
    s.ops
    s.wall
    s.cpu
    ratio
    ops_s
;;

(* ── workload driver for one engine ───────────────────────────────────────── *)
let run_engine (module E : ENGINE) ~variant ~key =
  let dir = Filename.temp_file "bench222-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let cleanup () =
    Array.iter
      (fun f ->
         try Sys.remove (Filename.concat dir f) with
         | _ -> ())
      (try Sys.readdir dir with
       | _ -> [||]);
    try Unix.rmdir dir with
    | _ -> ()
  in
  Fun.protect ~finally:cleanup (fun () ->
    let t = E.open_db ~dir ~key in
    E.seed t ~rows:rows_n;
    let keys = lookup_keys ~rows:rows_n ~n:ops_n in
    emit_row
      ~engine:E.name
      ~workload:"point_lookup"
      ~variant
      ~rows:rows_n
      (measure ~repeats:repeats_n (fun () -> E.w_point_lookup t ~keys));
    emit_row
      ~engine:E.name
      ~workload:"scan_agg"
      ~variant
      ~rows:rows_n
      (measure ~repeats:repeats_n (fun () -> E.w_scan_agg t ~repeats:scans_n));
    (* write workloads use disjoint id ranges per repeat to avoid PK clashes;
         measured single-shot (repeats=1) since they mutate state. *)
    emit_row
      ~engine:E.name
      ~workload:"insert_one"
      ~variant
      ~rows:ops_n
      (measure ~warmup:false ~repeats:1 (fun () -> E.w_insert_one t ~n:ops_n ~base:rows_n));
    emit_row
      ~engine:E.name
      ~workload:"insert_batch"
      ~variant
      ~rows:rows_n
      (measure ~warmup:false ~repeats:1 (fun () ->
         E.w_insert_batch t ~rows:rows_n ~base:(rows_n + ops_n)));
    emit_row
      ~engine:E.name
      ~workload:"commit_n"
      ~variant
      ~rows:commits_n
      (measure ~warmup:false ~repeats:1 (fun () ->
         E.w_commit_n t ~n:commits_n ~base:((2 * rows_n) + ops_n)));
    E.close t)
;;

(* ── cross-engine correctness smoke check (SQLOCAML_BENCH_SMOKE=1) ─────────── *)
let smoke () =
  let mk (module E : ENGINE) =
    let dir = Filename.temp_file "bench222smoke-" "" in
    Sys.remove dir;
    Unix.mkdir dir 0o755;
    let t = E.open_db ~dir ~key:None in
    E.seed t ~rows:100;
    let fp = E.fingerprint t in
    E.close t;
    E.name, fp
  in
  let _, a = mk (module Sqlocaml) in
  let _, b = mk (module Ref_sqlite) in
  if a <> b
  then (
    Printf.eprintf "SMOKE FAIL: sqlocaml=[%s] sqlite=[%s]\n%!" a b;
    exit 1);
  let cols = String.split_on_char ',' csv_header in
  if List.length cols <> 10
  then (
    Printf.eprintf "SMOKE FAIL: csv header has %d cols\n%!" (List.length cols);
    exit 1);
  Printf.eprintf
    "SMOKE OK: engines agree [%s]; csv header %d cols\n%!"
    a
    (List.length cols)
;;

(* ── main ─────────────────────────────────────────────────────────────────── *)
let () =
  (* Seed the RNG: the encrypted-store open path derives nonces/keys from it
     and fails closed if no generator is installed. *)
  Mirage_crypto_rng_unix.use_default ();
  (* pin sqlocaml page cache for parity (SQLite mirrored in Ref_sqlite, Task 3) *)
  Unix.putenv "SQLOCAML_PAGE_CACHE" (string_of_int page_cache);
  match Sys.getenv_opt "SQLOCAML_BENCH_SMOKE" with
  | Some ("1" | "true") -> smoke ()
  | _ ->
    (match Sys.getenv_opt "SQLOCAML_BENCH_DURABILITY" with
     | None ->
       (* Original #222 behaviour, unchanged: sqlocaml runs under Full. *)
       print_string (csv_header ^ "\n");
       run_engine (module Ref_sqlite) ~variant:"plaintext" ~key:None;
       run_engine (module Sqlocaml) ~variant:"plaintext" ~key:None;
       run_engine (module Sqlocaml) ~variant:"encrypted" ~key:(Some (String.make 32 'K'))
     | Some ("sweep" | "SWEEP") ->
       (* #332: one CSV comparing commit throughput across all three durability
          modes (plaintext only — the durability knob is orthogonal to
          encryption).  SQLite's reference (always FULL) is emitted once as the
          cross-engine baseline. *)
       print_string (csv_header ^ "\n");
       run_engine (module Ref_sqlite) ~variant:"plaintext" ~key:None;
       List.iter
         (fun d ->
            sqlocaml_durability := d;
            run_engine
              (module Sqlocaml)
              ~variant:("plaintext/" ^ durability_label d)
              ~key:None)
         [ Sqlocaml_store.Store.Full; batched_from_env (); Sqlocaml_store.Store.Off ]
     | Some mode ->
       (* A single explicit mode; variant tagged so the CSV is self-describing. *)
       sqlocaml_durability := parse_durability mode;
       let lbl = durability_label !sqlocaml_durability in
       print_string (csv_header ^ "\n");
       run_engine (module Ref_sqlite) ~variant:"plaintext" ~key:None;
       run_engine (module Sqlocaml) ~variant:("plaintext/" ^ lbl) ~key:None;
       run_engine
         (module Sqlocaml)
         ~variant:("encrypted/" ^ lbl)
         ~key:(Some (String.make 32 'K')))
;;
