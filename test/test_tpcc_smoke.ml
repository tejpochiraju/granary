(** TPC-C profile smoke run (#500).

    Runs every non-skipped profile against a loaded W=1 granary database and
    asserts the four clause-3.3 consistency conditions still hold afterwards.
    This is the test that determines the [verdict] recorded for each profile in
    {!Granary_tpc.Tpcc_txn.all}.

    {b This case is gated by the [GRANARY_TPCC_SMOKE] environment variable} and
    is skipped without it, because the W=1 load alone costs minutes and the
    default suite must stay fast. To run it:

    {v
podman run --rm --user 0 -v "$(pwd):/workspace:z" -w /workspace \
  -e GRANARY_TPCC_SMOKE=1 granary-dev dune test test/test_tpcc_smoke.exe
    v}

    [--user 0] is the project's convention for a container that must write into
    the checkout. Set [GRANARY_TPCC_SMOKE_TXNS] to change the per-profile
    transaction count from its default; anything that is not a positive integer
    — unparseable, zero, or negative — falls back to that default rather than
    failing, so a typo shortens nothing and silently skips no coverage. *)

module E = Granary_tpc.Granary_engine
module T = Granary_tpc.Tpcc_txn
module C = Granary_tpc.Tpcc_check
module Load = Granary_tpc.Tpcc_schema.Load (Granary_tpc.Granary_engine)

(* One W=1 population, loaded once and shared by all five profiles: the load is
   by far the most expensive thing this test does, and reloading per profile
   would multiply it by five for no extra coverage. [f] receives the load's
   wall-clock cost so the report can separate it from the transactions. *)
let with_loaded f =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "test-tpcc-smoke-%d" (Unix.getpid ()))
  in
  (try Unix.mkdir dir 0o755 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let e = E.open_db ~dir in
  let t0 = Unix.gettimeofday () in
  Load.run e (Granary_tpc.Tpcc_gen.create ~seed:42 ~warehouses:1);
  let load_s = Unix.gettimeofday () -. t0 in
  Fun.protect ~finally:(fun () -> E.close e) (fun () -> f e ~load_s)
;;

(* Granary_engine is synchronous and this test is single-threaded: the ops
   record wraps its calls back up in already-resolved promises. Concurrency is
   PR 2's problem; this test isolates "do the profiles work at all" from "do
   they work under contention". *)
let engine_ops e =
  { T.query = (fun s -> Lwt.return (E.query_rows e (T.render s)))
  ; T.exec =
      (fun s ->
        E.exec e (T.render s);
        Lwt.return_unit)
  }
;;

let warehouses = 1

(* Deliberately small. Each transaction is tens of statements against a
   ~500k-row population, and what this test is for is coverage of every
   profile path, not throughput — throughput is the benchmark driver's job.
   Twenty-five per profile still exercises every statement in every profile
   many times over. *)
let default_per_profile = 25

let per_profile =
  match Sys.getenv_opt "GRANARY_TPCC_SMOKE_TXNS" with
  | None -> default_per_profile
  | Some s ->
    (match int_of_string_opt s with
     | Some n when n > 0 -> n
     | Some _ | None -> default_per_profile)
;;

type timing =
  { name : string
  ; executed : int
  ; total_s : float
  ; slowest_s : float
  }

let fail_on_exn profile ~n ~input exn =
  Alcotest.failf
    "%s: transaction %d raised %s on input %s"
    profile.T.name
    n
    (Printexc.to_string exn)
    (Format.asprintf "%a" T.pp input)
;;

(* Runs [input] and returns how long it took. A raise fails the test rather
   than being caught and counted: Task 7 removed every tolerant default
   precisely so a wrong read surfaces here instead of becoming a plausible
   number that gets committed. *)
let time_one ops profile ~n ~input =
  let t0 = Unix.gettimeofday () in
  (try Lwt_main.run (T.run ops input) with
   | exn -> fail_on_exn profile ~n ~input exn);
  Unix.gettimeofday () -. t0
;;

(* Seeded per kind, not per weight. Three profiles share [weight = 4], so
   keying the seed off the weight handed Order_status, Delivery and Stock_level
   a single input stream between them. [kind] is the one field distinct by
   construction, and matching on it exhaustively means a profile added later
   has to choose a seed rather than silently collide with an existing one.
   Keying off the position in [T.all] would not do: reordering the list would
   then swap two profiles' streams. *)
let seed_of_kind = function
  | T.New_order -> 1001
  | T.Payment -> 1002
  | T.Order_status -> 1003
  | T.Delivery -> 1004
  | T.Stock_level -> 1005
;;

(* Each profile draws from its own generator, so adding or reordering profiles
   cannot perturb another profile's input stream. *)
let run_profile ops profile =
  let r = Granary_tpc.Tpc_rand.create ~seed:(seed_of_kind profile.T.kind) in
  let executed = ref 0 in
  let total = ref 0.0 in
  let slowest = ref 0.0 in
  for n = 1 to per_profile do
    let input = T.gen_input r ~warehouses ~constants:T.default_run_constants profile in
    let dt = time_one ops profile ~n ~input in
    total := !total +. dt;
    if dt > !slowest then slowest := dt;
    incr executed
  done;
  { name = profile.T.name; executed = !executed; total_s = !total; slowest_s = !slowest }
;;

(* The spec's 1% intentional-rollback NewOrder, forced rather than waited for:
   at twenty-five transactions a 1-in-100 draw would usually never come up, and
   the rollback path is exactly the one that must leave the database untouched
   — which is what the consistency conditions afterwards verify. *)
let forced_rollback_input =
  T.New_order_input
    { w_id = 1
    ; d_id = 1
    ; c_id = 1
    ; lines =
        [ { T.ol_i_id = 100; ol_supply_w_id = 1; ol_quantity = 5 }
        ; { T.ol_i_id = T.invalid_item_id; ol_supply_w_id = 1; ol_quantity = 5 }
        ]
    ; rollback = true
    }
;;

let check_conditions e ~where =
  List.iter
    (fun c ->
       let rows = List.map (E.query_rows e) c.C.queries in
       match C.classify c ~rows with
       | C.Holds -> ()
       | C.Violated report ->
         Alcotest.failf "%s: condition %d: %s" where c.C.number report
       | C.Not_run -> Alcotest.failf "%s: condition %d did not run" where c.C.number)
    C.conditions
;;

(* [forced_rollback_input] is a New_order, so the profile whose name labels a
   failure of it must be New_order — not whichever profile happens to sit at
   the head of the runnable list. *)
let profile_of_kind kind =
  match List.find_opt (fun p -> p.T.kind = kind) T.all with
  | Some p -> p
  | None -> Alcotest.fail "no profile with the requested kind in Tpcc_txn.all"
;;

let is_runnable p =
  match p.T.verdict with
  | T.Skipped _ -> false
  | T.Native | T.Rewritten _ -> true
;;

(* Printed so the per-profile cost lands in the test log: the profiles differ
   by more than an order of magnitude, which the benchmark driver needs to
   know when it sizes its run. *)
let report ~load_s timings =
  Printf.printf "\nW=1 load: %.2f s\n" load_s;
  Printf.printf
    "\n%-14s %5s %10s %10s %10s\n"
    "profile"
    "txns"
    "total(s)"
    "mean(ms)"
    "max(ms)";
  List.iter
    (fun t ->
       Printf.printf
         "%-14s %5d %10.3f %10.2f %10.2f\n"
         t.name
         t.executed
         t.total_s
         (1000.0 *. t.total_s /. float_of_int (max 1 t.executed))
         (1000.0 *. t.slowest_s))
    timings;
  Printf.printf
    "%-14s %5d %10.3f\n"
    "ALL"
    (List.fold_left (fun a t -> a + t.executed) 0 timings)
    (List.fold_left (fun a t -> a +. t.total_s) 0.0 timings);
  flush stdout
;;

(* The payoff test of the whole PR: run every non-skipped profile against a
   real granary database and assert the four clause-3.3 consistency conditions
   still hold afterwards. *)
let test_profiles_run_and_stay_consistent () =
  with_loaded (fun e ~load_s ->
    let ops = engine_ops e in
    check_conditions e ~where:"before";
    let runnable = List.filter is_runnable T.all in
    Alcotest.(check bool) "at least one profile is runnable" true (runnable <> []);
    let timings = List.map (run_profile ops) runnable in
    report ~load_s timings;
    (* The mix ran what it claims. *)
    List.iter
      (fun t ->
         Alcotest.(check int) (t.name ^ ": transactions executed") per_profile t.executed)
      timings;
    Alcotest.(check int)
      "total transactions executed"
      (per_profile * List.length runnable)
      (List.fold_left (fun a t -> a + t.executed) 0 timings);
    let dt =
      time_one ops (profile_of_kind T.New_order) ~n:0 ~input:forced_rollback_input
    in
    Printf.printf "forced new_order rollback: %.2f ms\n%!" (1000.0 *. dt);
    check_conditions e ~where:"after")
;;

let gate = "GRANARY_TPCC_SMOKE"

(* Gated rather than shrunk, on measurement rather than guess. At
   [per_profile = 25] the run costs ~1120 s, of which the W=1 load is only
   ~40 s: the cost is per-transaction, and it is dominated by the two
   write-heavy profiles at ~18 s (new_order) and ~20 s (delivery) each. The
   reason is that every TPC-C table except [item] is keyed by a composite
   PRIMARY KEY, and a composite-key equality lookup currently full-scans —
   [SELECT s_quantity FROM stock WHERE s_w_id = 1 AND s_i_id = 500] takes
   ~790 ms over 100k rows where the rowid-aliased [SELECT i_price FROM item
   WHERE i_id = 500] over the same 100k rows takes ~0.1 ms.

   So shrinking [per_profile] does not buy a fast default suite: even a single
   transaction per profile still costs ~40 s of load plus ~40 s of
   transactions, and it would spend the coverage that is the entire point of
   this test. The whole case therefore sits behind an env var — the convention
   CLAUDE.md documents for GRANARY_TEST_SQLITE — and keeps all five profiles
   and all four consistency conditions intact whenever it is set. When the
   composite-key seek lands, revisit this: the run should get cheap enough to
   ungate. *)
let skipped_notice () =
  Printf.printf
    "skipped: set %s=1 to run the TPC-C profile smoke run (it loads a full W=1 \
     population; minutes, not seconds)\n\
     %!"
    gate;
  Alcotest.skip ()
;;

let () =
  let enabled = Sys.getenv_opt gate <> None in
  Alcotest.run
    "tpcc_smoke"
    [ ( "profiles"
      , [ Alcotest.test_case
            "every profile runs and leaves the database consistent"
            `Slow
            (if enabled then test_profiles_run_and_stay_consistent else skipped_notice)
        ] )
    ]
;;
