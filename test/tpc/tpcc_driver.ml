open Lwt.Syntax

type config =
  { warehouses : int
  ; terminals : int
  ; seconds : float
  ; warmup_seconds : float
  ; seed : int
  ; max_retries : int
  }

let pp_config fmt c =
  Format.fprintf
    fmt
    "warehouses=%d terminals=%d seconds=%g warmup=%g seed=%d retries=%d"
    c.warehouses
    c.terminals
    c.seconds
    c.warmup_seconds
    c.seed
    c.max_retries
;;

let config_from_env () =
  { warehouses = max 1 (Bench_report.env_int "GRANARY_TPCC_WAREHOUSES" 1)
  ; terminals = max 1 (Bench_report.env_int "GRANARY_TPCC_TERMINALS" 4)
  ; seconds = Float.max 0.0 (Bench_report.env_float "GRANARY_TPCC_SECONDS" 10.0)
  ; warmup_seconds =
      Float.max 0.0 (Bench_report.env_float "GRANARY_TPCC_WARMUP_SECONDS" 1.0)
  ; seed = Bench_report.env_int "GRANARY_TPC_SEED" 42
  ; max_retries = max 0 (Bench_report.env_int "GRANARY_TPCC_RETRIES" 3)
  }
;;

type worker = Tpcc_txn.input -> unit Lwt.t

type percentiles =
  { p50 : float
  ; p95 : float
  ; p99 : float
  ; max : float
  }

type profile_stats =
  { name : string
  ; attempted : int
  ; committed : int
  ; rolled_back : int
  ; retried : int
  ; failed : int
  ; latency : percentiles
  ; mean_ms : float
  ; wait_ms : float
  ; service_ms : float
  ; service_total_ms : float
  }

type result =
  { config : config
  ; workers : int
  ; elapsed_s : float
  ; per_profile : profile_stats list
  ; errors : (string * int) list
  }

let tps s ~elapsed_s =
  if elapsed_s > 0.0 then float_of_int s.committed /. elapsed_s else 0.0
;;

let new_order_per_sec r =
  match
    List.find_opt (fun s -> s.name = "new_order") r.per_profile
    (* Matched by name rather than by position: the profile list's order is
       Tpcc_txn.all's, and a reordering there must not silently retarget the
       headline metric onto another profile. *)
  with
  | Some s -> tps s ~elapsed_s:r.elapsed_s
  | None -> 0.0
;;

(* Nearest rank (the spec's own definition for its response-time
   percentiles): the smallest sample at or above the p-th position, never an
   interpolation between two samples that were never observed. *)
let nearest_rank sorted p =
  let n = Array.length sorted in
  if n = 0
  then 0.0
  else (
    let idx = int_of_float (Float.ceil (p *. float_of_int n)) - 1 in
    sorted.(max 0 (min (n - 1) idx)))
;;

let percentiles samples =
  let sorted = Array.copy samples in
  Array.sort compare sorted;
  { p50 = nearest_rank sorted 0.50
  ; p95 = nearest_rank sorted 0.95
  ; p99 = nearest_rank sorted 0.99
  ; max = nearest_rank sorted 1.0
  }
;;

(* ── accumulation ─────────────────────────────────────────────────────────
   One mutable accumulator per profile, plus one shared error tally.  Kept
   internal: the immutable {!profile_stats} is what leaves this module. *)

type acc =
  { a_name : string
  ; mutable a_attempted : int
  ; mutable a_committed : int
  ; mutable a_rolled_back : int
  ; mutable a_retried : int
  ; mutable a_failed : int
  ; mutable a_latencies : float list
  ; mutable a_wait_total : float
  ; mutable a_service_total : float
  }

type recorder =
  { accs : (string, acc) Hashtbl.t
  ; errors : (string, int) Hashtbl.t
  ; record : bool (** false during warm-up: the interval runs but is discarded *)
  }

let make_recorder ~record profiles =
  let accs = Hashtbl.create 8 in
  List.iter
    (fun (p : Tpcc_txn.profile) ->
       Hashtbl.replace
         accs
         p.Tpcc_txn.name
         { a_name = p.Tpcc_txn.name
         ; a_attempted = 0
         ; a_committed = 0
         ; a_rolled_back = 0
         ; a_retried = 0
         ; a_failed = 0
         ; a_latencies = []
         ; a_wait_total = 0.0
         ; a_service_total = 0.0
         })
    profiles;
  { accs; errors = Hashtbl.create 8; record }
;;

let acc_of rec_ name =
  match Hashtbl.find_opt rec_.accs name with
  | Some a -> a
  | None ->
    (* Every profile the driver can draw is registered up front, so this is
       unreachable; raising rather than creating one on the fly keeps a
       silently unaccounted profile from being possible at all. *)
    invalid_arg (Printf.sprintf "Tpcc_driver: no accumulator for profile %S" name)
;;

let note_error rec_ exn =
  if rec_.record
  then (
    let key = Printexc.to_string exn in
    let n = Option.value ~default:0 (Hashtbl.find_opt rec_.errors key) in
    Hashtbl.replace rec_.errors key (n + 1))
;;

let is_intentional_rollback (input : Tpcc_txn.input) =
  match input with
  | Tpcc_txn.New_order_input { rollback; _ } -> rollback
  | Tpcc_txn.Payment_input _
  | Tpcc_txn.Order_status_input _
  | Tpcc_txn.Delivery_input _
  | Tpcc_txn.Stock_level_input _ -> false
;;

let stats_of_acc a =
  let samples = Array.of_list a.a_latencies in
  let n = float_of_int (max 1 (Array.length samples)) in
  { name = a.a_name
  ; attempted = a.a_attempted
  ; committed = a.a_committed
  ; rolled_back = a.a_rolled_back
  ; retried = a.a_retried
  ; failed = a.a_failed
  ; latency = percentiles samples
  ; mean_ms = Array.fold_left ( +. ) 0.0 samples /. n
  ; wait_ms = a.a_wait_total /. n
  ; service_ms = a.a_service_total /. n
  ; service_total_ms = a.a_service_total
  }
;;

(* ── worker pool ──────────────────────────────────────────────────────────
   The pool is what enforces "one transaction at a time per worker" (see the
   module header: a granary Db.t has one explicit-transaction slot, and two
   terminals sharing it would let one COMMIT the other's half-done work).

   Hand-rolled rather than [Lwt_pool], for two reasons.

   {b An exception must not evict the worker.} [Lwt_pool] disposes of an
   element whose user raised. On a one-deep pool that ends the run at the
   first failed transaction — while still reporting a rate for the truncated
   interval, which is the worst kind of wrong number. Here a worker is
   returned on every path and the caller classifies the failure.

   {b And a released worker is handed on with [Lwt.wakeup_later], not
   [Lwt.wakeup].} The later form resolves the next waiter from the scheduler
   rather than inline, so a terminal's transaction is never run inside the
   stack frame of the transaction that released the worker to it. That bound
   matters exactly when the engine never awaits anything real — the
   reference SQLite bindings are blocking calls wrapped in [Lwt.return] —
   because then nothing else in the chain returns to the scheduler either.
   See [terminal_loop] below for the other half of the same concern. *)

type 'a pool =
  { mutable free : 'a list
  ; waiting : 'a Lwt.u Queue.t
  }

let make_pool workers = { free = workers; waiting = Queue.create () }

let pool_acquire p =
  match p.free with
  | w :: rest ->
    p.free <- rest;
    Lwt.return w
  | [] ->
    let t, u = Lwt.task () in
    Queue.add u p.waiting;
    t
;;

let pool_release p w =
  match Queue.take_opt p.waiting with
  | Some u -> Lwt.wakeup_later u w
  | None -> p.free <- w :: p.free
;;

let now = Unix.gettimeofday

(* One attempt, timed in two halves: [wait_ms] up to acquiring a worker,
   [service_ms] inside it. Nothing escapes — the worker is released on the
   failure path too, or a one-deep pool would deadlock on the first raise. *)
let attempt_once pool ~input =
  let submitted = now () in
  let* w = pool_acquire pool in
  let started = now () in
  let+ outcome =
    Lwt.catch
      (fun () -> Lwt.map (fun () -> Ok ()) (w input))
      (fun exn -> Lwt.return (Error exn))
  in
  let finished = now () in
  pool_release pool w;
  outcome, 1000.0 *. (started -. submitted), 1000.0 *. (finished -. started)
;;

let record_success rec_ a ~input ~wait_ms ~service_ms =
  if rec_.record
  then (
    a.a_committed <- a.a_committed + 1;
    if is_intentional_rollback input then a.a_rolled_back <- a.a_rolled_back + 1;
    a.a_latencies <- (wait_ms +. service_ms) :: a.a_latencies;
    a.a_wait_total <- a.a_wait_total +. wait_ms;
    a.a_service_total <- a.a_service_total +. service_ms)
;;

(* Retries redraw the input rather than replaying it: a deterministically bad
   input would otherwise burn every retry to no purpose, and a contention
   failure is served just as well by the next transaction of the same
   profile. *)
let rec attempt pool rec_ r ~config ~profile ~tries =
  let a = acc_of rec_ profile.Tpcc_txn.name in
  let input =
    Tpcc_txn.gen_input
      r
      ~warehouses:config.warehouses
      ~constants:Tpcc_txn.default_run_constants
      profile
  in
  if rec_.record then a.a_attempted <- a.a_attempted + 1;
  let* outcome, wait_ms, service_ms = attempt_once pool ~input in
  match outcome with
  | Ok () ->
    record_success rec_ a ~input ~wait_ms ~service_ms;
    Lwt.return_unit
  | Error exn ->
    note_error rec_ exn;
    if tries >= config.max_retries
    then (
      if rec_.record then a.a_failed <- a.a_failed + 1;
      Lwt.return_unit)
    else (
      if rec_.record then a.a_retried <- a.a_retried + 1;
      attempt pool rec_ r ~config ~profile ~tries:(tries + 1))
;;

(* The [Lwt.pause] is not a politeness; the loop is incorrect without it, in
   two independent ways, both of which bite exactly when the engine under
   test never awaits anything real.

   The reference SQLite bindings are blocking calls wrapped in
   [Lwt.return], so a whole transaction resolves without ever reaching the
   scheduler. Then (a) [Lwt.bind] on an already-resolved promise runs its
   continuation immediately, so this recursion grows the native stack once
   per transaction until the process takes SIGSEGV — no exception, no
   message, exit 139 — which is what killed the first cross-engine run,
   between its pre-run consistency check and any output; and (b) the first
   terminal would keep the
   CPU for the whole interval and the other terminals would never start, so
   a run at N terminals would in truth be a run at one.

   [Lwt.pause] returns to the event loop between transactions, which
   trampolines the recursion and round-robins the terminals. It costs one
   scheduler tick per transaction — noise beside a transaction, and it is
   inside neither the wait nor the service window, which are timed around
   the pool acquisition below. *)
let rec terminal_loop pool rec_ r ~config ~profiles ~deadline =
  let* () = Lwt.pause () in
  if now () >= deadline
  then Lwt.return_unit
  else (
    let profile = Tpcc_txn.pick r profiles in
    let* () = attempt pool rec_ r ~config ~profile ~tries:0 in
    terminal_loop pool rec_ r ~config ~profiles ~deadline)
;;

let is_runnable (p : Tpcc_txn.profile) =
  match p.Tpcc_txn.verdict with
  | Tpcc_txn.Skipped _ -> false
  | Tpcc_txn.Native | Tpcc_txn.Rewritten _ -> true
;;

(* Every terminal gets its own stream, seeded from its index, so what a
   terminal issues does not depend on how the scheduler interleaved it with
   its peers. *)
let terminal_rand ~config ~index = Tpc_rand.create ~seed:(config.seed + index)

let run_interval pool rec_ ~config ~profiles ~duration =
  if duration <= 0.0
  then Lwt.return 0.0
  else (
    let t0 = now () in
    let deadline = t0 +. duration in
    let terminals =
      List.init config.terminals (fun i ->
        let r = terminal_rand ~config ~index:i in
        terminal_loop pool rec_ r ~config ~profiles ~deadline)
    in
    let+ () = Lwt.join terminals in
    now () -. t0)
;;

let sorted_errors rec_ =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) rec_.errors []
  (* Ties broken on the text so the report is stable run to run. *)
  |> List.sort (fun (ka, va) (kb, vb) ->
    if va <> vb then compare vb va else compare ka kb)
;;

let run config ~workers =
  if workers = [] then invalid_arg "Tpcc_driver.run: empty worker pool";
  if config.terminals < 1 then invalid_arg "Tpcc_driver.run: terminals must be >= 1";
  let profiles = List.filter is_runnable Tpcc_txn.all in
  let pool = make_pool workers in
  let warm = make_recorder ~record:false profiles in
  let* _ = run_interval pool warm ~config ~profiles ~duration:config.warmup_seconds in
  (* #714: discard the warm-up window's statement observations, so cold-cache
     first touches and the one-off [Db.prepare] per shape do not contaminate
     the steady-state means.  Unconditional: on a non-profiling run this is
     [Hashtbl.reset] on an empty table. *)
  Tpcc_stmt_profile.reset ();
  let rec_ = make_recorder ~record:true profiles in
  let+ elapsed_s = run_interval pool rec_ ~config ~profiles ~duration:config.seconds in
  { config
  ; workers = List.length workers
  ; elapsed_s
  ; per_profile =
      List.map
        (fun (p : Tpcc_txn.profile) -> stats_of_acc (acc_of rec_ p.Tpcc_txn.name))
        profiles
  ; errors = sorted_errors rec_
  }
;;

(* ── reporting ────────────────────────────────────────────────────────── *)

let summary_header buf r =
  Buffer.add_string
    buf
    (Format.asprintf "tpcc: %a workers=%d\n" pp_config r.config r.workers);
  Buffer.add_string buf (Printf.sprintf "measured interval: %.3f s\n\n" r.elapsed_s);
  Buffer.add_string
    buf
    (Printf.sprintf
       "%-14s %8s %8s %7s %7s %9s %9s %9s %9s %9s %9s\n"
       "profile"
       "attempt"
       "commit"
       "retry"
       "fail"
       "tps"
       "mean(ms)"
       "p50(ms)"
       "p95(ms)"
       "p99(ms)"
       "wait(ms)")
;;

let summary_row buf ~elapsed_s s =
  Buffer.add_string
    buf
    (Printf.sprintf
       "%-14s %8d %8d %7d %7d %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f\n"
       s.name
       s.attempted
       s.committed
       s.retried
       s.failed
       (tps s ~elapsed_s)
       s.mean_ms
       s.latency.p50
       s.latency.p95
       s.latency.p99
       s.wait_ms)
;;

let summary_errors buf (r : result) =
  if r.errors <> []
  then (
    Buffer.add_string buf "\nfailures:\n";
    List.iter
      (fun (msg, n) -> Buffer.add_string buf (Printf.sprintf "  %6d x %s\n" n msg))
      r.errors)
;;

let single_worker_note =
  "note: the worker pool is one deep, so terminals queue rather than overlap and the\n\
  \      terminal count cannot raise throughput — only wait(ms). A granary Db.t holds\n\
  \      one explicit-transaction slot, which is the bound being reported here.\n"
;;

let summary r =
  let buf = Buffer.create 1024 in
  summary_header buf r;
  List.iter (summary_row buf ~elapsed_s:r.elapsed_s) r.per_profile;
  Buffer.add_string
    buf
    (Printf.sprintf
       "\n\
        NewOrder/sec: %.3f  (derived TPC-C; NOT tpmC — no audit, no pricing, rewritten \
        SQL, zero think time)\n"
       (new_order_per_sec r));
  if r.workers <= 1 then Buffer.add_string buf single_worker_note;
  summary_errors buf r;
  Buffer.contents buf
;;

let csv_columns =
  [ "host"
  ; "engine"
  ; "warehouses"
  ; "terminals"
  ; "workers"
  ; "elapsed_s"
  ; "profile"
  ; "attempted"
  ; "committed"
  ; "rolled_back"
  ; "retried"
  ; "failed"
  ; "tps"
  ; "mean_ms"
  ; "p50_ms"
  ; "p95_ms"
  ; "p99_ms"
  ; "max_ms"
  ; "wait_ms"
  ; "service_ms"
  ]
;;

let csv_row r ~host ~engine s =
  Bench_report.Csv.row
    [ host
    ; engine
    ; string_of_int r.config.warehouses
    ; string_of_int r.config.terminals
    ; string_of_int r.workers
    ; Printf.sprintf "%.3f" r.elapsed_s
    ; s.name
    ; string_of_int s.attempted
    ; string_of_int s.committed
    ; string_of_int s.rolled_back
    ; string_of_int s.retried
    ; string_of_int s.failed
    ; Printf.sprintf "%.4f" (tps s ~elapsed_s:r.elapsed_s)
    ; Printf.sprintf "%.3f" s.mean_ms
    ; Printf.sprintf "%.3f" s.latency.p50
    ; Printf.sprintf "%.3f" s.latency.p95
    ; Printf.sprintf "%.3f" s.latency.p99
    ; Printf.sprintf "%.3f" s.latency.max
    ; Printf.sprintf "%.3f" s.wait_ms
    ; Printf.sprintf "%.3f" s.service_ms
    ]
;;

let csv_rows r ~host ~engine = List.map (csv_row r ~host ~engine) r.per_profile
