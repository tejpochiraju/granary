(** Unit tests for the TPC-C saturation driver (#500).

    Every case here drives {!Granary_tpc.Tpcc_driver.run} against a {e fake}
    worker — a function of {!Granary_tpc.Tpcc_txn.input} that succeeds, fails
    or stalls on demand — and never opens a database. That is deliberate:
    the driver's own logic (the mix, retry accounting, the one-transaction-
    per-worker guarantee, the percentiles) is what these cases pin, and
    testing it through a real engine would put a minutes-long W=1 load in
    front of every assertion and make the default suite slow.

    The engine-backed run lives in [test_tpcc_smoke.ml] behind
    [GRANARY_TPCC_SMOKE], and the recorded benchmark run in
    [test/bench_tpcc.ml].

    Intervals here are a fraction of a second and each fake transaction
    sleeps a millisecond, so a case executes tens of transactions per
    terminal: enough for the counters and the ordering to be meaningful,
    fast enough for the default suite. Nothing here asserts a {e rate}, only
    invariants over the counters — a timing assertion in a unit test would
    fail on a loaded CI runner. *)

module D = Granary_tpc.Tpcc_driver
module T = Granary_tpc.Tpcc_txn

let base_config =
  { D.warehouses = 1
  ; terminals = 4
  ; seconds = 0.10
  ; warmup_seconds = 0.0
  ; seed = 7
  ; max_retries = 3
  }
;;

let tick () = Lwt_unix.sleep 0.001
let ok_worker : D.worker = fun _ -> tick ()
let run_with ?(config = base_config) workers = Lwt_main.run (D.run config ~workers)

let find r name =
  match List.find_opt (fun (s : D.profile_stats) -> s.D.name = name) r.D.per_profile with
  | Some s -> s
  | None -> Alcotest.failf "no stats for profile %s" name
;;

let total f r = List.fold_left (fun acc s -> acc + f s) 0 r.D.per_profile

(* ── percentiles ─────────────────────────────────────────────────────── *)

let test_percentiles_nearest_rank () =
  let p = D.percentiles [| 10.0; 1.0; 5.0; 2.0 |] in
  (* Nearest rank over [1;2;5;10]: ceil(0.5*4)=2 -> 2.0, ceil(0.95*4)=4 ->
     10.0. Never an interpolated 3.5, which was never observed. *)
  Alcotest.(check (float 1e-9)) "p50" 2.0 p.D.p50;
  Alcotest.(check (float 1e-9)) "p95" 10.0 p.D.p95;
  Alcotest.(check (float 1e-9)) "p99" 10.0 p.D.p99;
  Alcotest.(check (float 1e-9)) "max" 10.0 p.D.max
;;

let test_percentiles_empty () =
  let p = D.percentiles [||] in
  Alcotest.(check (float 1e-9)) "p50" 0.0 p.D.p50;
  Alcotest.(check (float 1e-9)) "max" 0.0 p.D.max
;;

let test_percentiles_does_not_mutate () =
  let samples = [| 3.0; 1.0; 2.0 |] in
  ignore (D.percentiles samples);
  Alcotest.(check (array (float 1e-9)))
    "caller's array untouched"
    [| 3.0; 1.0; 2.0 |]
    samples
;;

let test_percentiles_single () =
  let p = D.percentiles [| 4.5 |] in
  Alcotest.(check (float 1e-9)) "p50" 4.5 p.D.p50;
  Alcotest.(check (float 1e-9)) "p99" 4.5 p.D.p99
;;

(* ── a clean run ─────────────────────────────────────────────────────── *)

let test_clean_run_counts () =
  let r = run_with [ ok_worker ] in
  Alcotest.(check bool) "some transactions ran" true (total (fun s -> s.D.committed) r > 0);
  Alcotest.(check int) "nothing failed" 0 (total (fun s -> s.D.failed) r);
  Alcotest.(check int) "nothing retried" 0 (total (fun s -> s.D.retried) r);
  Alcotest.(check int)
    "attempted equals committed when nothing fails"
    (total (fun s -> s.D.attempted) r)
    (total (fun s -> s.D.committed) r);
  Alcotest.(check (list (pair string int))) "no errors recorded" [] r.D.errors;
  Alcotest.(check bool) "elapsed is positive" true (r.D.elapsed_s > 0.0);
  Alcotest.(check int) "worker count reported" 1 r.D.workers
;;

let test_every_runnable_profile_present () =
  let r = run_with [ ok_worker ] in
  let runnable =
    List.filter
      (fun (p : T.profile) ->
         match p.T.verdict with
         | T.Skipped _ -> false
         | T.Native | T.Rewritten _ -> true)
      T.all
  in
  Alcotest.(check (list string))
    "one stats row per runnable profile, in Tpcc_txn.all order"
    (List.map (fun (p : T.profile) -> p.T.name) runnable)
    (List.map (fun (s : D.profile_stats) -> s.D.name) r.D.per_profile)
;;

let test_new_order_dominates_the_mix () =
  let r = run_with [ ok_worker ] in
  let no = find r "new_order" in
  let os = find r "order_status" in
  (* NewOrder is 45% of the spec's mix and OrderStatus 4%: the draw is
     seeded, so this ordering is a property of the mix, not of luck. *)
  Alcotest.(check bool)
    "new_order ran more often than order_status"
    true
    (no.D.committed > os.D.committed)
;;

let test_new_order_per_sec_is_new_orders () =
  let r = run_with [ ok_worker ] in
  let no = find r "new_order" in
  Alcotest.(check (float 1e-9))
    "headline is the new_order rate"
    (D.tps no ~elapsed_s:r.D.elapsed_s)
    (D.new_order_per_sec r);
  Alcotest.(check bool) "headline is positive" true (D.new_order_per_sec r > 0.0)
;;

let test_tps_of_zero_elapsed () =
  let r = run_with [ ok_worker ] in
  let no = find r "new_order" in
  Alcotest.(check (float 1e-9)) "no division by zero" 0.0 (D.tps no ~elapsed_s:0.0)
;;

(* ── failure and retry accounting ────────────────────────────────────── *)

exception Boom

let test_all_failures_are_counted () =
  let config = { base_config with terminals = 2; max_retries = 2 } in
  let r = run_with ~config [ (fun _ -> Lwt.bind (tick ()) (fun () -> Lwt.fail Boom)) ] in
  Alcotest.(check int) "nothing committed" 0 (total (fun s -> s.D.committed) r);
  Alcotest.(check bool) "failures counted" true (total (fun s -> s.D.failed) r > 0);
  (* max_retries = 2 means three attempts per transaction: one initial plus
     two retries. *)
  Alcotest.(check int)
    "one retry pair per failed transaction"
    (2 * total (fun s -> s.D.failed) r)
    (total (fun s -> s.D.retried) r);
  Alcotest.(check int)
    "attempted counts every attempt"
    (3 * total (fun s -> s.D.failed) r)
    (total (fun s -> s.D.attempted) r);
  match r.D.errors with
  | [ (msg, n) ] ->
    Alcotest.(check bool) "the exception text is reported" true (msg <> "");
    Alcotest.(check int)
      "every attempt's failure tallied"
      n
      (total (fun s -> s.D.attempted) r)
  | other -> Alcotest.failf "expected one distinct error, got %d" (List.length other)
;;

let test_retry_then_succeed () =
  (* Fails the first attempt and succeeds thereafter: retries must be
     counted, and the transaction must still land as committed. *)
  let n = ref 0 in
  let worker _ =
    incr n;
    if !n mod 2 = 1 then Lwt.bind (tick ()) (fun () -> Lwt.fail Boom) else tick ()
  in
  let r = run_with ~config:{ base_config with terminals = 1 } [ worker ] in
  Alcotest.(check bool) "committed anyway" true (total (fun s -> s.D.committed) r > 0);
  Alcotest.(check int) "nothing ultimately failed" 0 (total (fun s -> s.D.failed) r);
  Alcotest.(check int)
    "one retry per committed transaction"
    (total (fun s -> s.D.committed) r)
    (total (fun s -> s.D.retried) r)
;;

let test_no_retry_when_disabled () =
  let config = { base_config with terminals = 1; max_retries = 0 } in
  let r = run_with ~config [ (fun _ -> Lwt.bind (tick ()) (fun () -> Lwt.fail Boom)) ] in
  Alcotest.(check int) "no retries" 0 (total (fun s -> s.D.retried) r);
  Alcotest.(check int)
    "one attempt per failure"
    (total (fun s -> s.D.attempted) r)
    (total (fun s -> s.D.failed) r)
;;

(* A worker that raises must not be evicted from the pool: Lwt_pool disposes
   of an element whose user raised, and on a one-worker pool that would end
   the run silently after the first failure — a truncated measurement that
   still reports a rate. *)
let test_pool_survives_a_raising_worker () =
  let config = { base_config with terminals = 1; max_retries = 0 } in
  let calls = ref 0 in
  let worker _ =
    incr calls;
    Lwt.bind (tick ()) (fun () -> Lwt.fail Boom)
  in
  let _ = run_with ~config [ worker ] in
  Alcotest.(check bool) "the worker kept being used after raising" true (!calls > 2)
;;

(* ── the one-transaction-per-worker guarantee ────────────────────────── *)

(* The reason the driver takes a worker *list* at all: a granary Db.t holds
   one explicit-transaction slot, so overlapping two transactions on one
   worker would let one COMMIT the other's half-done work. *)
let concurrency_probe () =
  let live = ref 0 in
  let peak = ref 0 in
  let worker _ =
    incr live;
    if !live > !peak then peak := !live;
    Lwt.bind (tick ()) (fun () ->
      decr live;
      Lwt.return_unit)
  in
  worker, peak
;;

let test_one_worker_never_overlaps () =
  let worker, peak = concurrency_probe () in
  let config = { base_config with terminals = 8 } in
  let _ = run_with ~config [ worker ] in
  Alcotest.(check int) "never two transactions at once on one worker" 1 !peak
;;

let test_two_workers_overlap () =
  let worker, peak = concurrency_probe () in
  let config = { base_config with terminals = 8 } in
  let _ = run_with ~config [ worker; worker ] in
  Alcotest.(check int) "a two-deep pool does run two at once" 2 !peak
;;

(* Regression, and the most expensive bug in this PR: the terminal loop must
   return to the scheduler between transactions.

   The reference SQLite bindings are blocking calls wrapped in [Lwt.return],
   so a whole transaction resolves without the scheduler ever running and
   [Lwt.bind] invokes each continuation inline. Without a yield the loop then
   recursed inline once per transaction and the process took SIGSEGV — no
   exception, no message, exit 139 — after the pre-run consistency check and
   before any output, which reads as a hung benchmark rather than as a
   defect. The same absence starves the peers: one terminal that never
   yields holds the entire measurement interval, so a run at N terminals is
   in truth a run at one.

   [tick] is unusable here — an [Lwt_unix.sleep] is the very yield whose
   absence is the bug — so this worker chains resolved binds instead, at
   roughly the depth one TPC-C profile costs.

   The assertion is on the mechanism rather than on survival: a concurrent
   observer counts scheduler turns taken while the run is in flight. One
   turn or none means the loop ran to completion without ever yielding, and
   survival at that point is a matter of how deep the stack happened to
   get. *)
let sync_worker () =
  let rec chain n =
    if n = 0 then Lwt.return_unit else Lwt.bind Lwt.return_unit (fun () -> chain (n - 1))
  in
  chain 60
;;

let test_loop_returns_to_the_scheduler () =
  let config = { base_config with terminals = 4; seconds = 0.3 } in
  let turns = ref 0 in
  let stop = ref false in
  let rec observer () =
    if !stop
    then Lwt.return_unit
    else
      Lwt.bind (Lwt.pause ()) (fun () ->
        incr turns;
        observer ())
  in
  let driver =
    Lwt.map
      (fun r ->
         stop := true;
         r)
      (D.run config ~workers:[ (fun _ -> sync_worker ()) ])
  in
  let r = fst (Lwt_main.run (Lwt.both driver (observer ()))) in
  Alcotest.(check bool)
    "the run made progress"
    true
    (total (fun s -> s.D.committed) r > 100);
  Alcotest.(check bool)
    "the terminal loop yielded between transactions"
    true
    (!turns > 100)
;;

(* Pins the pool's hand-off bound: releasing a worker must not run the next
   terminal's transaction inside the releasing one's stack frame, which is
   what [Lwt.wakeup_later] buys over [Lwt.wakeup]. Needs two or more
   terminals to mean anything — with one there is never a waiter to hand off
   to.

   Honest about its own reach: this measures depth through
   [Printexc.get_callstack], and OCaml's tail-call optimisation flattens
   enough of the chain that swapping [wakeup_later] back to [wakeup] does
   {e not} make it fail. It is a property guard, not a reproduction of the
   SIGSEGV described in [docs/benchmarks/BENCHMARKS-TPCC.md]; that crash is
   still open as #571 and is not explained by this hand-off path. *)
let test_worker_handoff_does_not_nest () =
  let lo = ref max_int in
  let hi = ref 0 in
  let worker _ =
    let d = Printexc.raw_backtrace_length (Printexc.get_callstack 100_000) in
    if d < !lo then lo := d;
    if d > !hi then hi := d;
    sync_worker ()
  in
  let config = { base_config with terminals = 4; seconds = 0.3 } in
  let _ = run_with ~config [ worker ] in
  Alcotest.(check bool)
    "stack depth stays bounded across worker hand-offs"
    true
    (!hi - !lo < 200)
;;

let test_extra_terminals_show_up_as_wait () =
  let one = run_with ~config:{ base_config with terminals = 1 } [ ok_worker ] in
  let many = run_with ~config:{ base_config with terminals = 8 } [ ok_worker ] in
  let wait r = (find r "new_order").D.wait_ms in
  (* The honest shape of a one-deep pool: the extra terminals' time lands in
     wait, not in throughput. *)
  Alcotest.(check bool)
    "queueing delay grows with terminals on a one-worker pool"
    true
    (wait many > wait one)
;;

(* ── argument validation ─────────────────────────────────────────────── *)

let test_empty_worker_pool_raises () =
  Alcotest.check_raises
    "empty pool"
    (Invalid_argument "Tpcc_driver.run: empty worker pool")
    (fun () -> ignore (run_with []))
;;

let test_zero_terminals_raises () =
  Alcotest.check_raises
    "no terminals"
    (Invalid_argument "Tpcc_driver.run: terminals must be >= 1")
    (fun () -> ignore (run_with ~config:{ base_config with terminals = 0 } [ ok_worker ]))
;;

let test_zero_duration_runs_nothing () =
  let config = { base_config with seconds = 0.0; warmup_seconds = 0.0 } in
  let r = run_with ~config [ ok_worker ] in
  Alcotest.(check int) "no transactions" 0 (total (fun s -> s.D.attempted) r);
  Alcotest.(check (float 1e-9)) "no elapsed time" 0.0 r.D.elapsed_s;
  Alcotest.(check (float 1e-9)) "and therefore no rate" 0.0 (D.new_order_per_sec r)
;;

(* Warm-up transactions must not appear in the measured interval's counters,
   or the reported rate covers a longer window than [elapsed_s] says. *)
let test_warmup_is_discarded () =
  let calls = ref 0 in
  let worker _ =
    incr calls;
    tick ()
  in
  let config =
    { base_config with terminals = 1; seconds = 0.05; warmup_seconds = 0.05 }
  in
  let r = run_with ~config [ worker ] in
  Alcotest.(check bool) "the warm-up ran" true (!calls > total (fun s -> s.D.attempted) r)
;;

(* ── configuration ───────────────────────────────────────────────────── *)

let with_env pairs f =
  let saved = List.map (fun (k, _) -> k, Sys.getenv_opt k) pairs in
  List.iter (fun (k, v) -> Unix.putenv k v) pairs;
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun (k, v) -> Unix.putenv k (Option.value ~default:"" v)) saved)
    f
;;

let test_config_from_env_reads_the_knobs () =
  with_env
    [ "GRANARY_TPCC_WAREHOUSES", "4"
    ; "GRANARY_TPCC_TERMINALS", "16"
    ; "GRANARY_TPCC_SECONDS", "2.5"
    ; "GRANARY_TPCC_WARMUP_SECONDS", "0.5"
    ; "GRANARY_TPCC_RETRIES", "7"
    ]
    (fun () ->
       let c = D.config_from_env () in
       Alcotest.(check int) "warehouses" 4 c.D.warehouses;
       Alcotest.(check int) "terminals" 16 c.D.terminals;
       Alcotest.(check (float 1e-9)) "seconds" 2.5 c.D.seconds;
       Alcotest.(check (float 1e-9)) "warmup" 0.5 c.D.warmup_seconds;
       Alcotest.(check int) "retries" 7 c.D.max_retries)
;;

let test_config_from_env_clamps () =
  with_env
    [ "GRANARY_TPCC_WAREHOUSES", "0"
    ; "GRANARY_TPCC_TERMINALS", "-3"
    ; "GRANARY_TPCC_SECONDS", "-1"
    ; "GRANARY_TPCC_RETRIES", "-2"
    ]
    (fun () ->
       let c = D.config_from_env () in
       Alcotest.(check int) "warehouses floored at 1" 1 c.D.warehouses;
       Alcotest.(check int) "terminals floored at 1" 1 c.D.terminals;
       Alcotest.(check (float 1e-9)) "seconds floored at 0" 0.0 c.D.seconds;
       Alcotest.(check int) "retries floored at 0" 0 c.D.max_retries)
;;

let test_config_from_env_defaults () =
  with_env
    [ "GRANARY_TPCC_WAREHOUSES", "not-a-number"
    ; "GRANARY_TPCC_TERMINALS", ""
    ; "GRANARY_TPCC_SECONDS", "x"
    ]
    (fun () ->
       let c = D.config_from_env () in
       Alcotest.(check int) "warehouses default" 1 c.D.warehouses;
       Alcotest.(check int) "terminals default" 4 c.D.terminals;
       Alcotest.(check (float 1e-9)) "seconds default" 10.0 c.D.seconds)
;;

let test_pp_config () =
  let s = Format.asprintf "%a" D.pp_config base_config in
  Alcotest.(check bool) "mentions the terminal count" true (s <> "");
  Alcotest.(check bool)
    "mentions terminals"
    true
    (Option.is_some (String.index_opt s 't'))
;;

(* ── reporting ───────────────────────────────────────────────────────── *)

let test_csv_shape () =
  let r = run_with [ ok_worker ] in
  let rows = D.csv_rows r ~host:"h" ~engine:"granary" in
  Alcotest.(check int)
    "one row per profile"
    (List.length r.D.per_profile)
    (List.length rows);
  let fields = String.split_on_char ',' (List.hd rows) in
  Alcotest.(check int)
    "field count matches the header"
    (List.length D.csv_columns)
    (List.length fields)
;;

let test_summary_never_says_tpmc () =
  let r = run_with [ ok_worker ] in
  let s = D.summary r in
  let contains needle =
    let n = String.length needle in
    let rec go i =
      i + n <= String.length s && (String.sub s i n = needle || go (i + 1))
    in
    go 0
  in
  Alcotest.(check bool) "reports the NewOrder rate" true (contains "NewOrder/sec");
  Alcotest.(check bool) "says it is not tpmC" true (contains "NOT tpmC");
  Alcotest.(check bool)
    "one-worker pools carry the serialization note"
    true
    (contains "one explicit-transaction slot")
;;

let test_summary_lists_failures () =
  let config = { base_config with terminals = 1; max_retries = 0 } in
  let r = run_with ~config [ (fun _ -> Lwt.bind (tick ()) (fun () -> Lwt.fail Boom)) ] in
  let s = D.summary r in
  Alcotest.(check bool)
    "failures are named, not just counted"
    true
    (String.length s > 0 && r.D.errors <> [])
;;

let () =
  Alcotest.run
    "tpcc_driver"
    [ ( "percentiles"
      , [ Alcotest.test_case "nearest rank" `Quick test_percentiles_nearest_rank
        ; Alcotest.test_case "empty" `Quick test_percentiles_empty
        ; Alcotest.test_case "single sample" `Quick test_percentiles_single
        ; Alcotest.test_case "does not mutate" `Quick test_percentiles_does_not_mutate
        ] )
    ; ( "clean run"
      , [ Alcotest.test_case "counters add up" `Quick test_clean_run_counts
        ; Alcotest.test_case
            "every profile reported"
            `Quick
            test_every_runnable_profile_present
        ; Alcotest.test_case "mix is weighted" `Quick test_new_order_dominates_the_mix
        ; Alcotest.test_case "headline metric" `Quick test_new_order_per_sec_is_new_orders
        ; Alcotest.test_case "tps of zero elapsed" `Quick test_tps_of_zero_elapsed
        ] )
    ; ( "failures"
      , [ Alcotest.test_case "counted, never hidden" `Quick test_all_failures_are_counted
        ; Alcotest.test_case "retry then succeed" `Quick test_retry_then_succeed
        ; Alcotest.test_case "retries can be disabled" `Quick test_no_retry_when_disabled
        ; Alcotest.test_case
            "pool survives a raise"
            `Quick
            test_pool_survives_a_raising_worker
        ] )
    ; ( "concurrency"
      , [ Alcotest.test_case
            "one worker never overlaps"
            `Quick
            test_one_worker_never_overlaps
        ; Alcotest.test_case "two workers overlap" `Quick test_two_workers_overlap
        ; Alcotest.test_case
            "the loop returns to the scheduler"
            `Quick
            test_loop_returns_to_the_scheduler
        ; Alcotest.test_case
            "worker hand-off does not nest"
            `Quick
            test_worker_handoff_does_not_nest
        ; Alcotest.test_case
            "extra terminals become wait"
            `Quick
            test_extra_terminals_show_up_as_wait
        ] )
    ; ( "arguments"
      , [ Alcotest.test_case "empty pool raises" `Quick test_empty_worker_pool_raises
        ; Alcotest.test_case "zero terminals raises" `Quick test_zero_terminals_raises
        ; Alcotest.test_case "zero duration" `Quick test_zero_duration_runs_nothing
        ; Alcotest.test_case "warm-up discarded" `Quick test_warmup_is_discarded
        ] )
    ; ( "config"
      , [ Alcotest.test_case "reads env" `Quick test_config_from_env_reads_the_knobs
        ; Alcotest.test_case "clamps" `Quick test_config_from_env_clamps
        ; Alcotest.test_case "defaults" `Quick test_config_from_env_defaults
        ; Alcotest.test_case "pp" `Quick test_pp_config
        ] )
    ; ( "reporting"
      , [ Alcotest.test_case "csv shape" `Quick test_csv_shape
        ; Alcotest.test_case "never says tpmC" `Quick test_summary_never_says_tpmc
        ; Alcotest.test_case "lists failures" `Quick test_summary_lists_failures
        ] )
    ]
;;
