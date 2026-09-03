(** Phase 38 / #155 — WAL fsync-overlap benchmark.

    Quantifies the win from #149: readers no longer wait for the
    writer's WAL fsync to complete.  The default [Unix.fsync] on local
    SSDs is fast (<1 ms), so [bench_wal_reader_scaling] can't see the
    win — its ratio sits at ~1.0 regardless.  Here we substitute a
    [wal_sync] callback that sleeps for [FSYNC_DELAY_MS] before
    returning, synthesising slow / remote storage so the writer's
    commit spends a measurable amount of time parked.

    Synthetic comparison:
    - {b Baseline (pre-#149 semantics)}: writer runs to completion,
      then readers execute.  This is what an [Lwt_mutex]-style
      ro_begin gave us — readers could not start until every writer
      fsync had returned.  Wall time = [T_writer + T_readers].
    - {b Current (post-#149)}: writer and readers run concurrently
      via [Lwt.join]; readers acquire ro snapshots and walk while the
      writer is parked inside [wal_sync].  Wall time approaches
      [max(T_writer, T_readers)].

    Two gates, in order of robustness (#468):

    - {b Primary — the overlap invariant.}  In the parallel run, readers
      must finish no later than the writer ([reader_done <= writer_done]).
      Serialised readers cannot start until the writer is done, so they
      necessarily finish after it; overlapping readers finish inside its
      window.  Both terms scale with host speed, so this holds on any
      hardware and needs no assumption about the reader/writer balance.
    - {b Secondary — the wall-clock win}, speedup = baseline / parallel,
      floor 1.2x.  Ceiling for perfect overlap with balanced durations is
      2x.  This one assumes [parallel ~ max (T_writer, T_readers)], which
      needs CPU headroom; where the host has none it is reported
      INCONCLUSIVE instead of failing (the invariant still applies).

    Because the secondary ceiling is [1 + T_readers / T_writer], the reader
    workload is {b calibrated} at startup rather than fixed: see
    [calibrate_read_ops].  Setting GRANARY_BENCH_READ_OPS pins it
    instead and skips calibration.

    The premise is a writer parked in fsync for a measurable time, so
    DELAY_MS x N_COMMITS should stay well above a second (default 1.5s);
    at a few tens of ms, fixed per-run overhead dominates both walls.

    Both gates are wall-clock measurements, so both were single-shot and
    flaked on a co-scheduled box (#538: "readers finish inside the writer
    window: 1.70 <= 1.15" on an 8-core host running several suites at once;
    reproduced here as the secondary gate at 1.19x against a 1.20x floor).
    Unlike [test_fts_scaling] (#537) the timed region is NOT too short — the
    writer phase is ~1.5 s by construction — so the first half of the fix is
    the other half of that treatment: a BEST-OF-N over whole trials.

    Best-of-N alone is NOT enough here, and finding out why located the actual
    root cause (#538).  Reproduced under a parallel [dune test] at load
    average 13: [calibrate_read_ops] chose read_ops=22639 targeting
    T_r = 0.45 x T_w = 0.68 s, and the measured reader phase came out 4.4-6.4 s
    against a 1.5 s writer — a 6-9x overshoot, so ALL THREE trials failed
    identically at overlap 2.04.  Every part of that is a CALIBRATION error, not
    a measurement one: a workload sized from a per-op cost the measurement never
    pays is deterministically wrong for every trial that follows.  Erring low on
    per-op cost errs HIGH on the workload, which is the one direction
    [calibrate_read_ops] documents as unsafe.

    So the fix is layered, in order of importance — all three at the source, one
    safety net:
    - {b representative probes} (#569): the probes now run WITH THE WRITER
      ACTIVE, so per-op cost is measured under the contention the measurement
      actually sees.  They used to run readers alone at [delay:0.0], which made
      the estimate depend on how busy the box happened to be between the probe
      and the measurement — the model error that produced the blowouts.  See
      [probe_reader_phase].
    - {b median, not minimum} (#538): the probes are repeated and reduced with
      the MEDIAN, not the most optimistic draw.  A divisor is not a verdict —
      see the comment in [calibrate_read_ops] for why min is right in [measure]
      and wrong in calibration.  Since #623 the fold is a symmetric TRIMMED MEAN
      over seven paired reps rather than a median over three: same centre, same
      rejection of a lucky idle draw, lower sampling variance.  On three draws
      the two folds are the same number, so this narrows the spread without
      re-deciding anything #538 or #590 decided.
    - {b compose, then reduce} (#590): the per-op cost is the MEDIAN OF THE
      PER-REP MARGINALS.  #569 instead took [max (marginal, average)] over
      independently-medianed probe sizes, which is biased high by construction
      ([average = p + F/2n] always exceeds [p], so the max always does too) and
      undersized [read_ops] systematically, roughly halving the primary gate's
      margin against a serialisation regression.  Pairing the probes and
      reducing over the composed marginals discards the lucky-idle draw the
      floor was defending against, without moving the distribution.  See
      [per_op_estimate].
    - {b as a safety net}: a trial whose readers overran the writer window
      resizes read_ops by the observed overshoot and restarts the statistic
      (durations from different workloads are not comparable).  Bounded, and
      the bounds are load-bearing — see [measure].  It is run-wide rather than
      per-config, so both configs keep sharing one calibration.

    Measured effect of the two #569 changes, 6 runs each (12 config
    measurements) on an 8-core box at load average 8-16, i.e. exactly the
    co-scheduled conditions that produced #538:

    {v
      probes            per-op estimator   overlap ratio     worst / 1.15 gate
      idle (pre-#569)   marginal           0.22 - 1.08       94%
      contended         marginal           0.22 - 1.05       91%
      contended         max(marginal,avg)  0.35 - 0.79       69%
    v}

    The chosen read_ops spread narrowed from ~4x to ~2x run-to-run at the same
    time.  The residual spread is genuine load variation between calibration and
    measurement, not model error, which is why the target ratio stays at 0.45 —
    see [reader_ratio].

    #590 bought that sensitivity back: the third row above came from an
    estimator that was biased high on purpose, and the same robustness is
    available unbiased by composing per rep before reducing.  The full
    accounting is at [per_op_estimate].

    The undersizing had a second consequence, handled at [speedup_unreachable]
    and still reachable on a fast-reading host: when the workload comes out
    small enough, the SECONDARY gate becomes arithmetically unsatisfiable — even
    perfect overlap cannot reach the 1.2x floor — and the run must say so rather
    than report a 1.02x "regression" that no engine could have avoided.  Raising
    GRANARY_BENCH_TRIALS does not help there; the sizing is run-wide.  Per
    config that verdict is a PASS; a run in which EVERY config reaches it
    measured nothing and fails, or the gate could stop guarding while CI stayed
    green (#603 review).

    {b Reducing the trials (#603).}  Every reported ratio is composed from one
    trial's own durations and only then compared across trials.  Reducing each
    of the five durations with [min] independently and composing the minima
    afterwards — what this did until #603 — yields a figure no trial produced,
    and one that drifts DOWNWARD as trials are added, so bench-nightly's
    GRANARY_BENCH_TRIALS=15 was the configuration most exposed to it.

    {b The reduction OPERATOR is then chosen per metric, and this is the part
    that is easy to get wrong.}  Best-of-N is sound only for a metric whose
    noise pushes it toward FAILING.  That is true of the speedup ratio and
    FALSE of the overlap invariant, where load inflating the writer inflates
    the denominator and pushes the metric toward passing — so the overlap
    invariant takes the MEDIAN and the speedup takes the best.  The full
    argument, and the measured false-acquittal rates that forced it, are at
    [verdict].

    So "on a loaded box, raise the trials rather than disarming the ceiling" is
    now safe for this gate in BOTH directions — more draws neither drift a
    healthy engine toward failing (the median does not drift with N) nor drift a
    regressed one toward passing (the escape that did exactly that is the B1
    review finding, pinned by
    [one_unmeasurable_trial_cannot_acquit_a_regression]).  What is still NOT
    true is that raising trials fixes an INCONCLUSIVE verdict: sizing is
    run-wide and identical in every trial.

    Related, filed, and deliberately NOT fixed here: #591 (no shellcheck gate in
    CI, which is why the sibling policy-script defects were reviewable-only),
    #604 (policy scope: bench/ and .mli unscanned, symlinks evade), #605 (the
    ["Sqlite" ^ "3"] evasion pattern).

    {b Where these gates actually run armed.}  Every automated job that
    invokes [dune runtest] sets [GRANARY_BENCH_MIN_SPEEDUP=0], which
    neutralizes both gates (see the [min_speedup <= 0.0] branch in [measure]).
    The one scheduled job that runs them ARMED is
    [.forgejo/workflows/bench-nightly.yml], which deliberately sets no
    [GRANARY_BENCH_*] neutralizer and runs [dune runtest --force -j 1] on the
    self-hosted runner.  It reports (files/updates an issue) rather than
    blocking PRs.  Its [.github/] mirror is manual-only ([workflow_dispatch],
    no [schedule]) on purpose: a shared 2-core hosted VM is no place to assert
    a wall-clock ceiling nightly.  Outside that nightly this gate is armed only
    for a developer running the suite locally (#549).

    Env vars (all optional):
      GRANARY_BENCH_FSYNC_DELAY_MS  injected per-fsync sleep (default 50)
      GRANARY_BENCH_N_COMMITS       writer commits (default 30)
      GRANARY_BENCH_N_READERS       parallel reader fibers (default 4)
      GRANARY_BENCH_READ_OPS        cursor walks per reader (default: calibrated)
      GRANARY_BENCH_READER_RATIO    target T_readers / T_writer (default 0.45)
      GRANARY_BENCH_SEED_ROWS       initial tree size (default 200)
      GRANARY_BENCH_MIN_SPEEDUP     pass/fail threshold (default 1.2)
      GRANARY_BENCH_TRIALS          max measurement trials per config (default 3)
*)

open Lwt.Syntax
module S = Granary_store.Store
module UF = Granary_unix.Unix_file

let run = Lwt_main.run
let bs = Bytes.of_string

let getenv_int k d =
  try int_of_string (Sys.getenv k) with
  | _ -> d
;;

let getenv_float k d =
  try float_of_string (Sys.getenv k) with
  | _ -> d
;;

(* Maximum measurement trials per config (#538); the verdict is the best of
   them (see [measure]).  3, not 9 as in [test_fts_scaling] — a trial here
   costs ~4 s of real fsync-delay wall time rather than ~15 ms, and the loop
   exits at the first passing aggregate, so 3 buys two retries for a bad draw
   without a routine 3x runtime.  Same env-var name as [test_fts_scaling] so
   one setting tunes every timing gate in the suite; a nightly on a quiet
   runner can raise it instead of turning the gate off. *)
let trials =
  match Option.bind (Sys.getenv_opt "GRANARY_BENCH_TRIALS") int_of_string_opt with
  | Some n when n > 0 -> n
  | _ -> 3
;;

(* WAL I/O helpers: identical to those in [test_group_commit] — keep
   in sync if the syscall layer changes.  Vendored rather than shared
   because the bench is intentionally a leaf executable. *)
let unix_read_at fd ~offset (out : Cstruct.t) =
  let len = Cstruct.length out in
  try
    let _ = Unix.lseek fd (Int64.to_int offset) Unix.SEEK_SET in
    let tmp = Bytes.create len in
    let rec loop o r =
      if r = 0
      then ()
      else (
        let n = Unix.read fd tmp o r in
        if n = 0 then Bytes.fill tmp o r '\x00' else loop (o + n) (r - n))
    in
    loop 0 len;
    Cstruct.blit_from_bytes tmp 0 out 0 len;
    Lwt.return (Ok ())
  with
  | Unix.Unix_error (e, _, _) -> Lwt.return (Error (Unix.error_message e))
;;

let unix_write_at fd ~offset (src : Cstruct.t) =
  let len = Cstruct.length src in
  try
    let _ = Unix.lseek fd (Int64.to_int offset) Unix.SEEK_SET in
    let tmp = Bytes.create len in
    Cstruct.blit_to_bytes src 0 tmp 0 len;
    let rec loop o r =
      if r = 0
      then ()
      else (
        let n = Unix.write fd tmp o r in
        if n = 0 then failwith "short write" else loop (o + n) (r - n))
    in
    loop 0 len;
    Lwt.return (Ok ())
  with
  | Unix.Unix_error (e, _, _) -> Lwt.return (Error (Unix.error_message e))
;;

(* Open a WAL store whose [wal_sync] sleeps [delay] seconds before
   completing.  [Lwt_unix.sleep] yields the scheduler, so reader
   fibers parked on [ro_begin] / [cursor_open] can interleave during
   the simulated stall — which is exactly the #149 invariant we want
   to measure. *)
let open_slow_wal ~path ~delay =
  let* fr = UF.open_ ~path () in
  let file =
    match fr with
    | Ok f -> f
    | Error e -> Alcotest.failf "UF.open_: %a" UF.pp_error e
  in
  let wal_path = path ^ "-wal" in
  let wal_fd = Unix.openfile wal_path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  let wal_size_bytes = Int64.of_int (Unix.lseek wal_fd 0 Unix.SEEK_END) in
  let read_page ~page_id buf =
    let* r = UF.read_page file ~page_id buf in
    match r with
    | Ok () -> Lwt.return_ok ()
    | Error e -> Lwt.return_error (Format.asprintf "%a" UF.pp_error e)
  in
  let write_page ~page_id buf =
    let* r = UF.write_page file ~page_id buf in
    match r with
    | Ok () -> Lwt.return_ok ()
    | Error e -> Lwt.return_error (Format.asprintf "%a" UF.pp_error e)
  in
  let sync () =
    let* r = UF.sync file in
    match r with
    | Ok () -> Lwt.return_ok ()
    | Error e -> Lwt.return_error (Format.asprintf "%a" UF.pp_error e)
  in
  let resize ~n_pages =
    let* r = UF.resize file ~n_pages in
    match r with
    | Ok () -> Lwt.return_ok ()
    | Error e -> Lwt.return_error (Format.asprintf "%a" UF.pp_error e)
  in
  let n_pages = UF.n_pages file in
  let* () =
    if Int64.equal n_pages 0L
    then
      let* _ = UF.resize file ~n_pages:2L in
      Lwt.return_unit
    else Lwt.return_unit
  in
  let n_pages = UF.n_pages file in
  let wal_read_at = unix_read_at wal_fd in
  let wal_write_at = unix_write_at wal_fd in
  let wal_sync () =
    let* () = if delay > 0.0 then Lwt_unix.sleep delay else Lwt.return_unit in
    try
      Unix.fsync wal_fd;
      Lwt.return_ok ()
    with
    | Unix.Unix_error (e, _, _) -> Lwt.return_error (Unix.error_message e)
  in
  let close () =
    let* _ = UF.close file in
    Lwt.return_unit
  in
  let wal_close () =
    (try Unix.close wal_fd with
     | Unix.Unix_error _ -> ());
    Lwt.return_unit
  in
  let* sr =
    S.open_block_wal
      ~read_page
      ~write_page
      ~sync
      ~resize
      ~n_pages
      ~wal_read_at
      ~wal_write_at
      ~wal_sync
      ~wal_size_bytes
      ~close
      ~wal_close
      ()
  in
  match sr with
  | Ok s -> Lwt.return s
  | Error e -> Alcotest.failf "open_block_wal: %a" S.pp_error e
;;

(* Reader walks [tid_read]; the writer commits into [tid_write].
   Historically these had to be disjoint trees: the writer's CoW path
   evicted the reader's working set from the small page cache on every
   commit, and the per-page re-fetch swamped any fsync-overlap win.
   Since #159 a held RO snapshot pins its pages, so the shared-tree
   config (tid_read = tid_write) clears the floor too — [test_fsync_overlap]
   exercises both.  Mutable so the two configs can reuse the workloads. *)
let tid_read = ref 16
let tid_write = ref 99

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

(* Walk every entry under [tid_read] visible to [tx].  The result is
   discarded; only the time spent walking matters for the bench. *)
let walk_count : type a. a S.txn -> int Lwt.t =
  fun tx ->
  let* cur = S.cursor_open tx !tid_read in
  let _ = S.cursor_first cur in
  let rec loop n =
    match S.cursor_next cur with
    | None -> n
    | Some _ -> loop (n + 1)
  in
  let n = loop 0 in
  S.cursor_close cur;
  Lwt.return n
;;

let seed st n =
  let* tx = S.rw_begin st in
  let rec loop i =
    if i >= n
    then Lwt.return_unit
    else
      let* () =
        S.put tx !tid_read (bs (Printf.sprintf "k%06d" i)) (bs (Printf.sprintf "v%06d" i))
      in
      loop (i + 1)
  in
  let* () = loop 0 in
  S.commit tx
;;

(* Writer: [n_commits] commits, each appending one fresh row.  Each
   commit triggers one [wal_sync] — the parameter [tag] keeps baseline
   and parallel runs from colliding on key space, since [seed] is
   re-run between configurations on a fresh DB. *)
(* Writer for the calibration probes (#569): commits until [stop] is set,
   rather than a fixed count.  The probe wants the readers timed *under the
   writer's contention*, but it must not also pay for the writer's full
   [n_commits * delay] phase — so the readers set [stop] when they are done and
   the writer exits after finishing at most one more commit (~[delay]).  A flag
   rather than [Lwt.cancel] because cancelling mid-commit would leave the store
   in a state [S.close] has no reason to tolerate. *)
let writer_workload_until st ~stop ~tag =
  let rec loop i =
    if !stop
    then Lwt.return_unit
    else
      let* tx = S.rw_begin st in
      let* () =
        S.put
          tx
          !tid_write
          (bs (Printf.sprintf "%s%06d" tag i))
          (bs (Printf.sprintf "%sv%06d" tag i))
      in
      let* () = S.commit tx in
      loop (i + 1)
  in
  loop 0
;;

let writer_workload st ~n_commits ~tag =
  let rec loop i =
    if i >= n_commits
    then Lwt.return_unit
    else
      let* tx = S.rw_begin st in
      let* () =
        S.put
          tx
          !tid_write
          (bs (Printf.sprintf "%s%06d" tag i))
          (bs (Printf.sprintf "%sv%06d" tag i))
      in
      let* () = S.commit tx in
      loop (i + 1)
  in
  loop 0
;;

(* Reader: a single [ro_begin] held across [read_ops] cursor walks.

   [Lwt.pause] between walks is load-bearing on small trees.  Since
   #158 the [Unix_file] backend yields the scheduler whenever it does
   real I/O (verified by [test_unix_file]'s "read_page yields to
   concurrent timer" case), but the bench's [tid_read] tree fits
   entirely in the 64-page [Pager] cache after the first walk — so
   subsequent walks are pure cache hits and never re-enter
   [Unix_file].  Without an explicit yield in that hit-only loop the
   reader monopolises the scheduler and the writer's [wal_sync] sleep
   timer never fires.  Bumping [SEED_ROWS] high enough to overflow
   the cache would force the reader into the genuine-yield path, but
   then writer CoW evicts the reader's working set (#159) and
   swamps the win we're trying to measure.  Keeping the pause keeps
   this bench focused on the fsync-overlap question. *)
let reader_workload st ~read_ops =
  let* ro = S.ro_begin st in
  let rec loop i =
    if i >= read_ops
    then Lwt.return_unit
    else
      let* _ = walk_count ro in
      let* () = Lwt.pause () in
      loop (i + 1)
  in
  let* () = loop 0 in
  S.ro_end ro
;;

(* Run writer first, then readers — emulates pre-#149 reader-on-lock
   semantics where readers couldn't start until every writer fsync
   returned. *)
let baseline_run st ~n_commits ~n_readers ~read_ops =
  let tw0 = Unix.gettimeofday () in
  let* () = writer_workload st ~n_commits ~tag:"b" in
  let tw1 = Unix.gettimeofday () in
  let readers = List.init n_readers (fun _ -> reader_workload st ~read_ops) in
  let* () = Lwt.join readers in
  let tr1 = Unix.gettimeofday () in
  Lwt.return (tw1 -. tw0, tr1 -. tw1)
;;

(* Writer and readers concurrent — current #149 semantics, readers
   acquire ro snapshots while the writer is parked in [wal_sync]. *)
let parallel_run st ~n_commits ~n_readers ~read_ops =
  let t0 = Unix.gettimeofday () in
  let writer_done = ref 0.0 in
  let reader_done = ref 0.0 in
  let writer =
    let* () = writer_workload st ~n_commits ~tag:"p" in
    writer_done := Unix.gettimeofday () -. t0;
    Lwt.return_unit
  in
  let readers =
    List.init n_readers (fun _ ->
      let* () = reader_workload st ~read_ops in
      reader_done := Unix.gettimeofday () -. t0;
      Lwt.return_unit)
  in
  let* () = Lwt.join (writer :: readers) in
  Lwt.return (!writer_done, !reader_done)
;;

(* The database this bench writes, drawn PER PROCESS (#628).

   It used to be the fixed constant [/tmp/granary_bench_fsync_overlap.db], and
   CLAUDE.md states plainly that sibling agents run suites in parallel on this
   box as normal practice — the gates' own troubleshooting advice is to check
   [uptime] first for exactly that reason.  So the documented normal working
   mode was the one that corrupted this bench's input: two runs shared one
   database and overwrote each other's mid-measurement.  A reviewer hit it
   during the review of PR #608, losing two measurements before working out why.

   The failure is badly shaped for diagnosis, which is why it is worth more than
   a one-line fix: it does not error, it produces a WRONG TIMING, which then
   reads as either a flaky gate or a genuine regression.  That is #549's
   category — a gate people learn to distrust — and the same class as the shared
   scratchpad collision CLAUDE.md documents at length.  A fixed name in a shared
   namespace is the pattern; [refs/stash] and this were two instances.

   [Filename.temp_file] rather than a pid, following [bench_slow_read_yield],
   which already did this: a pid is reused, and a crashed predecessor would hand
   its successor the same collision.  The file is unlinked immediately because
   the store creates it; [at_exit] removes both it and its WAL sidecar, so a
   full suite run does not leave a trail behind. *)
let fresh_db_path () =
  let f = Filename.temp_file "granary_bench_fsync_overlap" ".db" in
  (try Unix.unlink f with
   | _ -> ());
  f
;;

let path = fresh_db_path ()
let () = at_exit (fun () -> cleanup path)

type config_result =
  { wall : float
  ; writer_phase : float
  ; reader_phase : float
  }

let run_config ~delay ~n_seed mode ~n_commits ~n_readers ~read_ops =
  cleanup path;
  run
    (let* st = open_slow_wal ~path ~delay in
     let* () = seed st n_seed in
     let t0 = Unix.gettimeofday () in
     let* w, r =
       match mode with
       | `Baseline -> baseline_run st ~n_commits ~n_readers ~read_ops
       | `Parallel -> parallel_run st ~n_commits ~n_readers ~read_ops
     in
     let t1 = Unix.gettimeofday () in
     let* () = S.close st in
     Lwt.return { wall = t1 -. t0; writer_phase = w; reader_phase = r })
;;

(* Ops per probe run.  A probe size only — deliberately NOT reused as a
   floor on the calibrated workload: on a slow host a 1000-op floor would
   override calibration and push T_r past T_w, which is #468 mirrored. *)
let probe_ops = 1000

(* Time the reader phase — [n_readers] fibers joined — WITH THE WRITER ACTIVE
   (#569).  Used by [calibrate_read_ops].

   This probe used to run readers alone on an idle scheduler at [delay:0.0],
   which is the model error #569 records: the calibration measured a per-op cost
   the real measurement never sees, so the chosen workload was systematically
   oversized and the target ratio had to be held at a deliberately low 0.45 to
   absorb the overshoot.  Worse, the size of the overshoot depended on how busy
   the box happened to be *between* the probe and the measurement, which is what
   produced the uncorrectable blowout band in [(1.15, 1.667]].

   Measuring under the same contention the measurement runs under removes the
   error at its source rather than correcting for it downstream.  The writer
   runs with the real [delay], so readers interleave with its [wal_sync] sleeps
   exactly as they will in [parallel_run].

   The probe writer targets [!tid_read], i.e. the SHARED-tree config, which is
   the more contended of the two configs the run measures (writer CoW churns the
   pages the readers walk, #159).  read_ops is shared by both configs, so
   calibrating on the harder one errs the documented-safe way: a higher per-op
   cost yields a SMALLER workload. *)
let probe_reader_phase ~n_seed ~n_readers ~read_ops ~delay =
  cleanup path;
  run
    (let* st = open_slow_wal ~path ~delay in
     let* () = seed st n_seed in
     let stop = ref false in
     let t0 = Unix.gettimeofday () in
     let writer = writer_workload_until st ~stop ~tag:"c" in
     let readers = List.init n_readers (fun _ -> reader_workload st ~read_ops) in
     (* [stop] must be set even if a reader raises, or the probe writer keeps
        committing.  ([Lwt_main.run] does raise as soon as this promise rejects,
        so the *exception* is never lost — the earlier comment here claimed
        otherwise and was wrong; the leak is the still-scheduled writer.) *)
     let* () =
       Lwt.finalize
         (fun () -> Lwt.join readers)
         (fun () ->
            stop := true;
            Lwt.return_unit)
     in
     let t1 = Unix.gettimeofday () in
     let* () = writer in
     let* () = S.close st in
     Lwt.return (t1 -. t0))
;;

(* Median of a list.  [infinity] on the empty list: a per-op cost of infinity
   yields the smallest possible workload, which is the safe direction. *)
let median = function
  | [] -> infinity
  | l ->
    let a = Array.of_list l in
    Array.sort compare a;
    a.(Array.length a / 2)
;;

(* Symmetric trimmed mean: drop the single lowest and single highest draw, then
   average what is left (#623).  [infinity] on the empty list for the same
   reason [median] does.

   {b This is a strict GENERALISATION of [median], not a replacement for it.}
   On three draws the trim leaves exactly the middle one, so [trimmed_mean l =
   median l] for every list of three or fewer — which is what the calibration
   used before #623, and why every pinned [per_op_estimate] case still reads the
   same number.  It only starts to differ once there are more draws to average,
   which is the point: see [probe_reps].

   {b Why this fold and not the median, on the same draws.}  Both are estimators
   of the same quantity and neither is biased, but the trimmed mean averages the
   middle [n-2] draws where the median keeps one, so its sampling variance is
   lower — under a normal parent, 0.15 sigma^2 at the seven draws [probe_reps]
   now takes, against 0.21 for the median of the same seven and 0.45 for the
   median of the three this replaces.  Variance is exactly what #623 is about:
   [read_ops] is [target / per_op], so the spread of the divisor IS the spread
   of the chosen workload.

   {b And why it does not move the estimate DOWN on a loaded box}, which is the
   direction that matters — a low per-op cost oversizes [read_ops], the
   direction [calibrate_read_ops] calls unsafe and #538's root cause.  The
   quantity being reduced is a per-rep MARGINAL, [p + (e2 - e1) / n], where
   [e1] and [e2] are the two probes' one-sided scheduling delays.  A difference
   of two like-distributed one-sided noises is symmetric, so the marginal's
   noise is symmetric even though each probe's is not, and a symmetric trim of a
   symmetric distribution has the same centre as its median.  Load widens
   [e1] and [e2] together and so widens that symmetric noise; it does not tilt
   it.  Where the parent IS right-skewed the trimmed mean sits ABOVE the median
   (it keeps the upper middle draws the median discards), which is the safe
   side.  Neither case moves it down.  Pinned by
   [trimmed_mean_is_the_median_generalised] and
   [trimmed_mean_does_not_err_low_against_the_median]. *)
let trimmed_mean = function
  | [] -> infinity
  | ([ _ ] | [ _; _ ]) as l -> median l
  | l ->
    let a = Array.of_list l in
    Array.sort compare a;
    let n = Array.length a in
    let total = ref 0.0 in
    for i = 1 to n - 2 do
      total := !total +. a.(i)
    done;
    !total /. float_of_int (n - 2)
;;

(* Per-op reader cost from PAIRED probes (#590).  Pure, so
   [statistics_tests] can feed it synthetic probe numbers.

   Model: a probe of [n] ops costs [F + n * p], with [F] the fixed
   cold-cache/setup cost.  [pairs] holds one [(t_n, t_2n)] measurement per
   rep — both sizes timed back to back inside the same rep, so the two
   halves see the same load.

   {b What this replaced, and why (#590).}  The #569 estimator was
   [max (marginal, average)] over per-SIZE medians, justified as "keep the
   unbiased estimator, remove its dangerous low tail".  That justification does
   not survive the arithmetic.  [average = t_2n / 2n = p + F/2n] is biased HIGH
   and never low, so [max (marginal, average) >= average > p] {i always}: the
   floor is not a floor, it is an unconditional upward shift of the whole
   distribution.  An overestimated per-op cost undersizes [read_ops], which
   makes the PRIMARY overlap gate systematically easier to pass — measured at
   overlap 0.27-0.53 against main's 0.42-0.63 at the same load, i.e. roughly
   half the margin against a serialisation regression (under serialisation the
   invariant reads [1 + T_r/T_w], so 0.27 presents as ~1.27 against a 1.15 gate
   where 0.42 presents as ~1.42).

   The noise the floor was defending against is real, but it is a REDUCTION
   defect, not an estimator defect: the old code took the median of the [t_n]
   draws and the median of the [t_2n] draws INDEPENDENTLY and then differenced
   them, so the reported marginal was a difference between two different reps
   and need not be any rep's actual marginal.  That is the same composition
   error as #603, one level down.  Composing first — one marginal per rep, then
   the median over those — discards the lucky-idle draw that produced the
   dangerous low tail (the 103 us estimate in a run whose workload actually ran
   at 246-369 us) while keeping the estimator itself unbiased.

   [average] survives only in its OTHER #569 role: the fallback for when the
   MEDIAN rep's marginal came out non-positive — i.e. a MAJORITY of reps were
   swamped by jitter and the difference carries no signal.  (Not "every rep":
   the guard tests the median, so 2 of 3 is enough and 1 of 3 is not.  Pinned
   by [per_op_falls_back_when_the_median_rep_is_swamped].)  Erring high there is
   right — it undersizes the workload, and the opposing secondary gate makes an
   undersized workload fail loudly rather than silently (see the lower-clamp
   comment in [calibrate_read_ops]).

   {b What removing the floor gives up, stated plainly.}  [max (_, average)] was
   also the only guard against the estimate coming out too LOW, and a low per-op
   cost yields an OVERSIZED workload — the direction [calibrate_read_ops]'s own
   header calls unsafe and the root cause of #538 (a lucky idle probe chose
   22639 ops for a 0.68 s target and produced a 4.4-6.4 s reader phase, failing
   every trial identically).  Three things carry that load instead, and none of
   them is a bias:
   - the median over per-rep marginals, which discards a single lucky draw —
     the specific failure the floor was reaching for;
   - contended probes (#569), which removed the systematic component of the
     error at its source;
   - the run-wide resize safety net in [test_fsync_overlap], which exists
     precisely for a calibration blowout that survives the first two.
   Measured on a quiet 12-core host, the chosen [read_ops] still spanned ~3x
   run-to-run (3324-10906 over three runs).  That residual is #623, and it is
   addressed at the SAME place the rest of this is — in the reduction, not by
   loosening a gate: seven paired reps instead of three, folded with a trimmed
   mean instead of a median.  See [probe_reps] for the arithmetic and
   [trimmed_mean] for why the fold does not move the estimate down. *)
let per_op_estimate ~probe_ops pairs =
  let n = float_of_int probe_ops in
  let marginals = List.map (fun (t1, t2) -> (t2 -. t1) /. n) pairs in
  let est = trimmed_mean marginals in
  (* Two conditions, not one, and the second is the ORIGINAL guard kept
     verbatim (#623).  The documented trigger for the fallback is "a MAJORITY of
     reps were swamped by jitter", and on three draws the median IS that
     majority test.  Once there are more draws the trimmed mean can come out
     positive while the median does not (three of five swamped, and the fourth
     large enough to outweigh them), so testing the estimate alone would quietly
     widen the guard.  Requiring both keeps it at least as conservative as it
     was, and every failure of either sends the estimate to the fallback — which
     reads HIGH and therefore undersizes the workload, the safe direction.  On
     three draws the two conditions coincide exactly, so nothing about the
     pinned boundary cases moved. *)
  if est > 0.0 && median marginals > 0.0
  then est
  else median (List.map (fun (_, t2) -> t2 /. (2.0 *. n)) pairs)
;;

(* Reader-workload calibration (#468).

   The metric is baseline / parallel = (T_w + T_r) / max (T_w, T_r), so when
   readers are the shorter phase its arithmetic ceiling is 1 + T_r / T_w — the
   floor is only reachable if T_r is a real fraction of T_w.  A fixed
   READ_OPS pins T_r in absolute terms, and once the read path got ~40x
   faster (#228 / #229 seek_ge, cursor_next, frame cache) T_r collapsed to
   ~0.04s against a 1.5s writer: a 1.02x ceiling, i.e. the 1.2x gate failed
   on hosts with fast reads *while overlap was in fact perfect*.

   So size the reader workload against the writer instead of hard-coding it.
   Two probes give the marginal per-op cost of the reader phase on this host
   (the difference cancels the fixed cold-cache cost of the first walk), and
   READ_OPS is chosen so T_r lands near [ratio] x T_w.  The floor then
   measures overlap quality rather than host read speed.

   Err LOW, deliberately.  The metric is not monotonic in T_r: past T_r = T_w
   the expression becomes 1 + T_w / T_r and the speedup falls again.  Hence the
   conservative default ratio and the hard writer-relative cap below: T_r is
   kept clear of T_w so the writer always bounds the parallel wall.

   A second effect used to compound this and no longer does.  The probes ran
   with no writer on an idle scheduler, so actual T_r overshot the target by
   ~1.5x once contention was added, and that inflation landed directly on the
   denominator (a cold run measured 1.27x).  Since #569 the probes run WITH the
   writer active — see [probe_reader_phase] — so the per-op cost is measured
   under the contention the measurement actually sees and the systematic
   overshoot is gone. *)
let calibrate_read_ops ~n_seed ~n_readers ~writer_secs ~ratio ~delay =
  (* Probes are repeated because single-shot ones drifted read_ops by up to
     1.5x run to run, which showed up directly as speedup spread (1.25x on the
     unlucky draw).  The reduction is the MEDIAN, not the minimum (#538 review).

     The minimum is the right statistic when you want the unperturbed cost of
     one operation, since noise can only ever ADD time.  It is the wrong
     statistic here, because
     this number is not a verdict, it is a divisor: [read_ops] is chosen as
     [target_seconds / per_op], so a per-op estimate that is too LOW makes the
     workload too BIG.  That is the direction this function's own header calls
     unsafe, and it is what broke #538: a min-of-three that happened to land in
     an idle window while the measurement then ran under load chose 22639 ops
     for a 0.68 s target and produced a 4.4-6.4 s reader phase, failing all
     three trials identically at overlap 2.04.

     The median tracks the load the box is actually under, so it errs the
     documented-safe way (slightly oversized per_op -> slightly undersized
     workload) and it discards the one lucky idle probe that causes the large
     blowouts.  It costs nothing extra: the probes were already being repeated,
     only the fold changed.  Since #569 the probes are also contended, which
     shrinks the spread the median has to survive in the first place.

     Since #590 the median is taken over the composed PER-REP marginals rather
     than over each probe size independently — same fold, applied after the
     composition instead of before it.  See [per_op_estimate].

     {b Since #623 there are SEVEN reps and the fold is a trimmed mean.}  #590
     left a residual it put on the record rather than leaving to be
     rediscovered: three reps reduced by a median still let the chosen
     [read_ops] span ~3.3x run to run on a quiet 12-core box (3324-10906), wider
     than #590's own "narrowed to ~2x" claim.  That is a SPREAD, not a bias —
     #608 removed the bias — and its consequence is loud (an oversized workload
     fails the gate) rather than silent, which is why it was filed to be watched
     instead of blocking that PR.

     Both halves of the narrowing are #623's own suggested remedies, taken
     together rather than one or the other, because neither alone reaches ~2x:

     - {b more paired reps}, 3 -> 7.  The estimator's sampling variance falls
       like 1/reps, and the probes are cheap relative to the measurement they
       size: a rep is one 1000-op probe plus one 2000-op probe, ~0.5 s together
       against the ~10 s the two configs then spend, so four extra reps cost
       ~2 s on a run that already takes ~10 s and up to ~30 s when it retries.
     - {b a trimmed mean rather than a median} — see [trimmed_mean] for why
       that lowers variance further without moving the centre, and in
       particular without moving it DOWN, which is the direction that oversizes
       the workload.

     Order-statistic arithmetic for the pair: sigma falls to ~0.59 of the
     median-of-3's, so a 3.3x multiplicative spread predicts ~3.3^0.59 = 2.0x.
     That is a prediction about the ESTIMATOR's contribution and no more.  Part
     of the observed spread is genuine load tracking between calibration and
     measurement — the per-op cost really is different on a busy box, and
     [reader_ratio]'s comment above turns on exactly that — and no estimator
     should remove it.

     {b Why the spread is worth narrowing at all, given it fails LOUDLY.}
     Because the resize safety net in [measure] cannot rescue every oversize:
     its trigger cannot be lowered below 1.667 without letting a resize walk a
     real serialisation regression down to passing, so a draw landing in
     [(1.15, 1.667]] fails the gate and gets no resize, and a retry cannot help
     because the calibration is deterministic across a run's trials.  Narrowing
     the divisor's spread narrows the chance of landing in that band, which is
     the only thing that can.  And the OTHER tail fails too, by a different
     route: an UNDERsized workload drives the secondary ceiling [1 + T_r/T_w]
     towards the 1.2 floor, which [speedup_unreachable] reports INCONCLUSIVE —
     a per-config pass, but a run whose every config lands there measured
     nothing and fails.  So this is not a one-sided "fails loudly, therefore
     harmless" situation.

     {b What this change was NOT observed to do, recorded so nobody re-runs the
     experiment.}  Measured on THIS box — 12-core, shared with another agent,
     1-minute load average 2.3-4.6 across the sweep — 10 interleaved
     old/new pairs of the calibration alone gave max/min 2.46x for the median
     of 3 and 2.69x for the trimmed mean of 7, i.e. no narrowing, well inside
     what 10 draws per arm can resolve.  That is not evidence the arithmetic
     above is wrong; it is evidence that on a box under varying load the
     ESTIMATOR is not the dominant variance term.  The dominant term is the one
     [reader_ratio]'s comment already names — the per-op cost genuinely differs
     between calibration and measurement — and no fold can or should remove it.
     Closing #623 needs the quiet box #623 asks for.

     The untried lever, if that measurement is ever taken and still shows a
     wide spread, is [probe_ops]: the marginal's signal is [n * p] against a
     fixed cost of comparable size, so raising the probe sizes improves the
     difference's signal-to-noise linearly where more reps only improve it as
     the square root.  It was not taken here because it costs proportionally
     more probe time and, on the evidence above, there was nothing to show for
     it. *)
  let probe_reps = 7 in
  (* One rep = BOTH sizes, timed back to back (#590).  The marginal difference
     is then taken WITHIN a rep, and the fold is applied to the composed
     per-rep estimates rather than to each size independently — see
     [per_op_estimate] for why that ordering is the whole point. *)
  let probe ~read_ops = probe_reader_phase ~n_seed ~n_readers ~read_ops ~delay in
  let pair () =
    let t1 = probe ~read_ops:probe_ops in
    let t2 = probe ~read_ops:(2 * probe_ops) in
    t1, t2
  in
  (* Probe under the shared-tree config — the harder of the two the run
     measures — then restore.  [Fun.protect] so a raising probe cannot leave the
     global mutated for whatever runs next (#574 review). *)
  let pairs =
    let saved_write = !tid_write in
    tid_write := !tid_read;
    Fun.protect
      ~finally:(fun () -> tid_write := saved_write)
      (fun () -> List.init probe_reps (fun _ -> pair ()))
  in
  (* #590: the estimator itself lives in [per_op_estimate], both so it can be
     unit-tested against synthetic probe numbers and so the argument for its
     shape sits next to it rather than buried in this function. *)
  let per_op = per_op_estimate ~probe_ops pairs in
  (* Unreachable: the marginal branch is guarded positive and the fallback is a
     median of positive durations — so this branch is defence only and will not
     be covered. *)
  if per_op <= 0.0
  then probe_ops
  else (
    let ops_for secs = int_of_float (secs /. per_op) in
    let want = ops_for (writer_secs *. ratio) in
    (* Upper clamps: predicted T_r stays at most 0.75 x T_w (see above), and
       200k ops keeps a pathologically slow host from running for minutes.

       Lower clamp is 1, not [probe_ops], and deliberately stays that way — the
       answer to #590 item 1.  Removing the [max (marginal, average)] floor
       removes the systematic undersizing that prompted the question, but even
       if it returned, a clamp would be the wrong instrument.  Any absolute
       lower clamp is
       a fixed op count, which is exactly what #468 was: on a slow host it
       overrides calibration and pushes T_r past T_w, where the metric is not
       even monotonic.  The protection against undersizing is not a clamp, it is
       the opposing SECONDARY gate — its ceiling is [1 + T_r/T_w], so a workload
       driven too small fails the 1.2x speedup floor loudly.  A relative clamp
       (some fraction of [want]) would be circular, since [want] is the quantity
       under suspicion. *)
    max 1 (min (min 200_000 (ops_for (writer_secs *. 0.75))) want))
;;

(* One TRIAL: the five durations one baseline/parallel pair produced.

   {b Every metric below composes ONE trial's durations; the reduction across
   trials happens afterwards, over the composed numbers (#603).}  The
   accumulator this replaced reduced each of the five durations with [min]
   INDEPENDENTLY and composed the minima, which need not be any trial's actual
   ratio — numerator and denominator could come from different trials.  Measured
   during the PR #582 re-review, three trials at 0.98, 0.96 and 1.44 composed to
   1.02: worse than every one of them, including the 1.44.  And since both
   minima shrink as N rises, the composed speedup DRIFTS DOWNWARD with more
   trials, so raising GRANARY_BENCH_TRIALS made a marginal run more likely to
   fail rather than less.  bench-nightly runs TRIALS=15, the most exposed
   configuration there is.

   The comment this replaced defended min-per-phase on the grounds that timing
   noise is one-sided, so a duration's minimum is the best estimate of its
   unperturbed cost while a ratio has a noisy denominator too.  That is a sound
   argument about ESTIMATING A COST and the wrong argument here: this number is
   a verdict, and a verdict has to be one the machine actually delivered.
   (#537's trap was the mirror-image error — reducing per-op COSTS with
   min-over-ratios — which is why [calibrate_read_ops] still reduces its divisor
   with a median, only now per rep rather than per probe size.)

   Both directions of the defect are real and they are not symmetric:

   - {b speedup} = [base_wall / par_wall] can land BELOW every trial's ratio,
     reporting a regression that nothing measured.  False failure.
   - {b overlap} = [par_reader / par_writer] is provably never optimistic under
     min-per-phase: with [min_r] drawn from trial a and [min_w] from trial b,
     [w_b <= w_a] gives [min_r / min_w >= r_a / w_a >= min_i (r_i / w_i)].  So
     it too can only manufacture false failures, never hide a serialisation
     regression, and fixing it costs no detection.

   Neither direction can mask the regression these gates exist for: serialised
   readers cannot start until the writer has finished, so [reader_done >
   writer_done] in EVERY trial and every trial's own overlap ratio exceeds 1. *)
type trial =
  { base_wall : float
  ; par_wall : float
  ; base_writer : float
  ; par_writer : float
  ; par_reader : float
  }

let trial_of ~base ~par =
  { base_wall = base.wall
  ; par_wall = par.wall
  ; base_writer = base.writer_phase
  ; par_writer = par.writer_phase
  ; par_reader = par.reader_phase
  }
;;

(* Primary metric: the overlap invariant.  If readers were serialised behind
   the writer they could not start until it finished, so [reader_done] would
   necessarily exceed [writer_done] by the whole reader phase (measured:
   1.5x).  Overlapping readers finish inside the writer's window (measured:
   0.37-0.51, and unchanged by host speed, since both terms scale together).
   Unlike the speedup ratio this needs no assumption about T_r / T_w. *)
let overlap_ratio t = t.par_reader /. t.par_writer

(* Secondary metric: the wall-clock win. *)
let speedup t = t.base_wall /. t.par_wall

(* This one DOES assume parallel ~ max (T_w, T_r), which needs enough CPU
   headroom for the readers' work to fit inside the writer's fsync sleeps.  On
   a starved host the writer's own phase inflates (measured: 1.61s -> 2.23s at
   0.15 CPU) and the ratio collapses below the floor with overlap perfectly
   intact — so report that inconclusive rather than failing.  A serialisation
   regression does NOT inflate the writer's phase, and the invariant above
   catches it unconditionally. *)
let writer_inflation t = t.par_writer /. t.base_writer
let inconclusive t = writer_inflation t > 1.25
let overlap_max = 1.15

(* The second way the secondary gate can be UNSATISFIABLE rather than failed
   (#590 review).

   The parallel wall can never drop below the writer's own window, so the very
   best this trial's numbers could produce is [base_wall / par_writer] — that is
   the speedup at PERFECT overlap, computed from measured phases only.  If that
   ceiling is already under [min_speedup], no engine could have passed: the
   reader workload was sized below what the gate can resolve.  Observed at load
   11.7 on an armed run, roughly 1 in 15:

     [disjoint tid] read_ops=1906 overlap=0.14 speedup=1.02x  FAIL (gate 1.20x)

   which reads as a performance regression and is not one.

   Why the measured ceiling and not the obvious [overlap_ratio < min_speedup -
   1.0]: on the SAME armed run the other config read overlap 0.16 — under that
   threshold — yet actually achieved 1.46x and passed.  The overlap ratio is a
   parallel-run quantity and the speedup's headroom comes from the BASELINE
   wall, so the proxy suppresses gates that would have passed.  [base_wall /.
   par_writer] is exact by construction and does not.

   The resulting rule is sharper than "skip pathological runs", and worth
   stating exactly, because it is what makes the guard safe.  When the readers
   finish inside the writer window, [par_wall = par_writer], so the ceiling and
   the achieved speedup are THE SAME NUMBER and every shortfall is declared
   unreachable.  That is correct: in that regime the overlap is already perfect
   and [base_wall / par_wall] measures nothing but how big T_r was — it carries
   no information about overlap quality, which is the only thing this gate
   exists to judge.  The secondary gate therefore asserts exactly when the
   readers set the parallel wall, i.e. [par_wall > par_writer] — which is
   precisely the serialisation regime: serialised readers give
   [par_wall = par_writer + par_reader], the ceiling sits strictly above the
   achieved speedup at [1 + T_r/T_w] = 1.45 for a workload at the intended
   ratio, and the gate fails at ~1.0 as it should.  So the regression the gate
   exists for is still caught, and the primary overlap invariant — which does
   carry overlap information — remains unconditional either way.

   The #569 estimator floor used to make this path more likely by undersizing
   [read_ops] on purpose; #590 removed the floor, so it should now be reached
   only when the host genuinely reads that fast. *)
let speedup_ceiling t = t.base_wall /. t.par_writer
let speedup_unreachable t ~min_speedup = speedup_ceiling t < min_speedup

(* The verdict ONE trial's own numbers support.  Ordered exactly as
   [assert_gates] applies them: the overlap invariant is unconditional, and the
   two inconclusive cases only ever suppress the secondary gate. *)
type outcome =
  | Fail_overlap
  | Fail_speedup
  | Inconclusive_headroom
  | Inconclusive_unreachable
  | Pass

let outcome t ~min_speedup =
  if overlap_ratio t > overlap_max
  then Fail_overlap
  else if inconclusive t
  then Inconclusive_headroom
  else if speedup_unreachable t ~min_speedup
  then Inconclusive_unreachable
  else if speedup t >= min_speedup
  then Pass
  else Fail_speedup
;;

(* An INCONCLUSIVE trial is one the code itself says it could not measure.  Per
   config it must remain a PASS (the #582 guard, kept deliberately: failing on
   "unresolvable" brings back the false failures that guard removed) — but it
   must never be allowed to OUTRANK a trial that measured a real failure.  That
   distinction is the whole of [conclusive] below. *)
let is_inconclusive = function
  | Inconclusive_headroom | Inconclusive_unreachable -> true
  | Pass | Fail_overlap | Fail_speedup -> false
;;

let conclusive ~min_speedup t = not (is_inconclusive (outcome t ~min_speedup))

(* {b Reducing N trials to one verdict — and why the operator is chosen PER
   METRIC.}

   #603's prescription was "compute the ratio per trial, then reduce over the
   composed ratios — the best {i or the median}".  The "or" is load-bearing and
   an earlier revision of this file ignored it, taking the best of everything.
   That is sound for exactly one class of metric:

   {b Best-of-N is a sound reduction only for a metric whose noise pushes it
   toward FAILING.}  Otherwise more trials means more chances to draw a
   spuriously passing one, and raising GRANARY_BENCH_TRIALS — which the nightly
   sets to 15 — monotonically weakens the gate.

   - The SECONDARY metric, [speedup = base_wall / par_wall], qualifies.  A
     serialisation regression drives it to ~1.0 and load can only inflate the
     parallel wall further, i.e. downward.  Best-of-N it is.
   - The PRIMARY invariant, [overlap = par_reader / par_writer], does NOT.  Load
     inflating the WRITER inflates the denominator, so noise pushes this metric
     toward PASSING.  Concretely: a fully serialised engine at the 0.45 target
     ratio reads 1.45, but one trial whose writer was inflated 3x reads
     [1 + 0.45/3] = 1.15 — exactly on the gate.  Take the best of fifteen and
     one such trial in fifteen acquits a broken engine.  Measured [writer_done]
     on the dev box spans 7.5-18.2 s for identical 1.5 s-nominal work, so that
     draw is inside observed variance, not a thought experiment.

     So the overlap invariant is reduced with the {b MEDIAN} rather than the
     best, and the pool it is reduced over EXCLUDES trials the code could not
     measure.  Both, and it is worth being exact about which one does the work,
     because a future reader who drops the filter believing the median carries
     B1 would reopen it.

   {b The FILTER is what closes B1.}  For a serialised engine no trial can be
   both conclusive and pass the overlap gate {i at all}.  Passing needs
   [1 + r/f <= 1.15], i.e. [r <= 0.15 f]; conclusive needs [f <= 1.25] and a
   reachable ceiling [(1 + r)/f >= 1.2].  Substituting the first into the third
   gives [1 + 0.15 f >= 1 + r >= 1.2 f], i.e. [1.05 f <= 1], i.e.
   [f <= 0.952] — impossible for an inflation [f >= 1].  Checked exhaustively
   over a 1200x301 grid of [(r, f)] as well: zero such trials exist.  So every
   trial that could acquit a serialised engine is an INCONCLUSIVE one, the
   filter removes exactly those, and with the filter in place any reduction over
   the survivors fails.

   {b The median is defence in depth, not the load-bearing guard} — an earlier
   revision of this comment claimed the reverse, on a worked example that was
   simply misread (at [r = 0.30] a 1.24x inflation gives overlap 1.2419, which
   FAILS the 1.15 gate; it does not sneak under it).  Measured with k of 15
   trials inflated 3.2x:

   {v
     k        filter+min   median only   filter+median (this code)
     0-7      1.450 fail   1.450 fail    1.450 fail
     8-14     1.450 fail   1.141 PASS    1.450 fail
     15       1.141 pass   1.141 pass    1.141 pass   (all-inconclusive: correct)
   v}

   The median ALONE leaks from k >= 8, where the inflated trials become the
   majority and take the middle.  It earns its place for the other direction:
   it removes best-of-N's drift on a HEALTHY engine without introducing a tail,
   and unlike the filter it does not depend on the serialised algebra above
   holding exactly.  Two guards, one of which survives the model being wrong.
   On an even count [median] takes the upper of the two middles, i.e. errs
   toward failing.

   The third guard is the {b early exit}, extracted as [should_stop]: it stops
   only on a full [Pass], never on an inconclusive trial.  A [Pass] is a
   complete conclusive measurement of success, so stopping there cannot
   manufacture one; stopping on an INCONCLUSIVE trial — which an earlier
   revision did — throws away the very draws that could have measured
   something, and was the second half of B1.  It showed up in the field as
   [best-of-1] printed for both configs with both inconclusive. *)
type verdict =
  { v_overlap : float (* MEDIAN over the pool *)
  ; v_speedup : float (* BEST over the pool *)
  ; v_status : outcome
  ; v_rep : trial (* whose raw durations get printed *)
  ; v_trials : trial list (* the pool itself, for the diagnostics *)
  ; v_inconclusive : int
  }

let verdict ~min_speedup ts =
  match ts with
  | [] -> None
  | _ ->
    let measured = List.filter (conclusive ~min_speedup) ts in
    let pool = if measured = [] then ts else measured in
    let v_overlap = median (List.map overlap_ratio pool) in
    let v_speedup =
      List.fold_left (fun acc t -> Float.max acc (speedup t)) neg_infinity pool
    in
    (* Reported durations come from the pool's best trial by speedup.  Purely
       cosmetic: both gate numbers above are reductions over the whole pool, and
       the INCONCLUSIVE diagnostics pick their own representative rather than
       reusing this one — [v_rep] is the best-by-SPEEDUP trial and need not be
       the trial whose writer inflated. *)
    let v_rep =
      List.fold_left
        (fun a b -> if speedup b > speedup a then b else a)
        (List.hd pool)
        pool
    in
    let inconclusive_kind =
      if List.exists (fun t -> outcome t ~min_speedup = Inconclusive_headroom) ts
      then Inconclusive_headroom
      else Inconclusive_unreachable
    in
    let v_status =
      if v_overlap > overlap_max
      then Fail_overlap
      else if measured = []
      then inconclusive_kind
      else if v_speedup >= min_speedup
      then Pass
      else Fail_speedup
    in
    Some
      { v_overlap
      ; v_speedup
      ; v_status
      ; v_rep
      ; v_trials = pool
      ; v_inconclusive = List.length ts - List.length measured
      }
;;

(* The early-exit rule, extracted from [measure]'s loop so it is reachable from
   the pure tests — it was half of the B1 fix and was pinned by nothing.  See
   the third guard in [verdict]. *)
let should_stop = function
  | Pass -> true
  | Fail_overlap | Fail_speedup | Inconclusive_headroom | Inconclusive_unreachable ->
    false
;;

let verdict_passes v = not (v.v_status = Fail_overlap || v.v_status = Fail_speedup)

(* The two gates for one config.  See the header for why there are two.
   Returns the verdict's status so the run can tell whether it measured
   anything at all (see the all-inconclusive guard in [test_fsync_overlap]).

   Each INCONCLUSIVE diagnostic picks its OWN representative out of the pool
   rather than reusing [v_rep], which is the best-by-speedup trial and need not
   be the trial that inflated.  The r2 revision did reuse it and printed
   "every trial's writer phase inflated under load (best 0.96x)" — a line that
   contradicts itself in two ways at once, and it is the line a human reads off
   an auto-filed nightly issue (#549). *)
let assert_gates ~label ~v ~min_speedup =
  Alcotest.(check bool)
    (Printf.sprintf
       "[%s] readers finish inside the writer window: median reader_done/writer_done \
        %.2f <= %.2f"
       label
       v.v_overlap
       overlap_max)
    true
    (v.v_overlap <= overlap_max);
  (match v.v_status with
   | Inconclusive_headroom ->
     let inflated = List.filter inconclusive v.v_trials in
     Printf.printf
       "  INCONCLUSIVE [%s]: %d of %d trials had the writer phase inflated under load \
        (worst %.2fx; no CPU headroom to overlap); speedup floor not asserted\n\
        %!"
       label
       (List.length inflated)
       (List.length v.v_trials)
       (List.fold_left (fun acc t -> Float.max acc (writer_inflation t)) 0.0 inflated)
   | Inconclusive_unreachable ->
     (* The most FAVOURABLE unreachable trial: "even the best draw could only
        have reached this". *)
     let best =
       List.fold_left
         (fun a t -> if speedup_ceiling t > speedup_ceiling a then t else a)
         (List.hd v.v_trials)
         v.v_trials
     in
     Printf.printf
       "  INCONCLUSIVE [%s]: reader workload sized below what this gate can resolve — \
        across %d trial(s) even PERFECT overlap caps the speedup at %.2fx (baseline \
        %.3fs / writer window %.3fs), under the %.2fx floor, at median overlap %.2f.  \
        Not a performance result; the calibration undersized read_ops (#590).  The \
        overlap invariant above still applies and passed.\n\
        %!"
       label
       (List.length v.v_trials)
       (speedup_ceiling best)
       best.base_wall
       best.par_writer
       min_speedup
       v.v_overlap
   | Fail_overlap | Fail_speedup | Pass ->
     Alcotest.(check bool)
       (Printf.sprintf "[%s] speedup %.2fx >= %.2fx" label v.v_speedup min_speedup)
       true
       (v.v_speedup >= min_speedup));
  v.v_status
;;

let test_fsync_overlap () =
  let delay_ms = getenv_int "GRANARY_BENCH_FSYNC_DELAY_MS" 50 in
  let n_commits = getenv_int "GRANARY_BENCH_N_COMMITS" 30 in
  let n_readers = getenv_int "GRANARY_BENCH_N_READERS" 4 in
  let n_seed = getenv_int "GRANARY_BENCH_SEED_ROWS" 200 in
  (* Target reader phase as a fraction of the writer phase.  0.45 lands the
     expected speedup near 1.45x — well over the 1.2x floor while keeping T_r
     clear of T_w (see [calibrate_read_ops]).  A non-positive or unparseable
     value means "unset".

     #569 asked whether making the probes representative would let this rise off
     its deliberately-low 0.45.  Measured: no, not on evidence.  With contended
     probes the MEAN observed overlap does land on target (0.47 over 12 config
     measurements, against 0.59 before), but the WORST case is 0.79 — 1.8x the
     target — because load still varies between calibration and measurement.
     Raising the target to 0.6 would put that worst case at ~1.05 against a 1.15
     gate.  The number that would justify raising this is a tighter worst case,
     not a better mean. *)
  let reader_ratio =
    let default = 0.45 in
    let r = getenv_float "GRANARY_BENCH_READER_RATIO" default in
    if r > 0.0 then r else default
  in
  (* Set conservatively at 1.2.  Observed across 8 runs in the
     granary-dev podman image at default parameters: min 1.38, mean
     1.51, max 1.63.  A regression that reintroduces reader-on-writer-
     lock serialisation would collapse the speedup to ~1.0x (parallel
     ≈ baseline), so 1.2x gives clear separation while tolerating
     jitter. *)
  let min_speedup = getenv_float "GRANARY_BENCH_MIN_SPEEDUP" 1.2 in
  let delay = float_of_int delay_ms /. 1000.0 in
  (* Each commit parks the writer for [delay], so this is the writer phase
     to within scheduling and B-tree overhead. *)
  let writer_secs = float_of_int n_commits *. delay in
  (* An unparseable override falls through to calibration rather than to a
     hard-coded count — a fixed count is exactly what #468 was.  A
     non-positive [min_speedup] means the gate is neutralized (coverage and
     bench-disabled CI runs do this), so don't burn the probes on a number
     nothing asserts on. *)
  let read_ops, read_ops_src =
    match Option.bind (Sys.getenv_opt "GRANARY_BENCH_READ_OPS") int_of_string_opt with
    | Some n -> n, "env"
    | None when min_speedup <= 0.0 -> probe_ops, "gate-off"
    | None ->
      ( calibrate_read_ops ~n_seed ~n_readers ~writer_secs ~ratio:reader_ratio ~delay
      , "calibrated" )
  in
  (* Run baseline vs parallel under the currently-configured tid pair and
     assert the overlap win clears the floor.  Called once per config.  Both
     configs deliberately share one calibration: per-op cost differs between
     them (shared-tid pays writer CoW eviction), and holding read_ops fixed
     is what makes the two speedups comparable.  A resize (see [measure])
     preserves that invariant by being run-wide rather than per-config: it
     changes the shared number, it does not give each config its own. *)
  let trial label t ~read_ops ~read_ops_src =
    let base = run_config ~delay ~n_seed `Baseline ~n_commits ~n_readers ~read_ops in
    let par = run_config ~delay ~n_seed `Parallel ~n_commits ~n_readers ~read_ops in
    Printf.printf
      "fsync-overlap bench [%s] trial %d/%d: delay=%dms commits=%d readers=%d \
       read_ops=%d (%s) seed=%d\n\
      \  baseline=%.3fs (writer=%.3fs, readers=%.3fs) parallel=%.3fs (writer_done=%.3fs \
       reader_done=%.3fs) speedup=%.2fx (min %.2fx)\n\
       %!"
      label
      t
      trials
      delay_ms
      n_commits
      n_readers
      read_ops
      read_ops_src
      n_seed
      base.wall
      base.writer_phase
      base.reader_phase
      par.wall
      par.writer_phase
      par.reader_phase
      (base.wall /. par.wall)
      min_speedup;
    cleanup path;
    base, par
  in
  (* Best-of-N over whole trials, with read_ops feedback (#538).

     Both gates are wall-clock ratios and both were single-shot; on a
     co-scheduled box a reader fiber that loses the CPU for one scheduling
     quantum lands directly on the verdict.  The loop stops as soon as the
     aggregate passes, so an unloaded host still pays for exactly one trial
     (~4 s per config) and only a noisy one pays more.

     Retries also RESIZE the reader workload when a trial's readers overran
     the writer window, because that is the failure mode that repeating alone
     cannot fix: [calibrate_read_ops] probes once, without a writer, and takes
     the fastest probe, so a calibration that landed in an idle window
     oversizes read_ops for every later trial (measured: target T_r 0.68 s,
     actual 4.4-6.4 s, all three trials failing identically).  T_r is ~linear
     in read_ops, so scaling by [overlap_target / observed] converges in one
     step.  The accumulator is reset on a resize: minima taken across two
     different workloads would not describe either.

     {b The resize is deliberately bounded, and this is the load-bearing part.}
     Shrinking the reader workload moves the primary gate's own metric, so an
     unbounded loop would walk a REAL serialisation regression down to passing:
     serialised readers show ratio 1 + T_r/T_w, and driving T_r towards zero
     drives that towards 1.0, which is under the 1.15 gate.  Two bounds keep
     that out of reach — at most ONE resize per RUN (not per config; see
     below), and a trigger chosen so that the resize is safe by construction
     rather than by a fudge factor:

     Under the serialised model the observed ratio is [1 + r] with
     [r = T_r / T_w], so scaling the workload by [f] leaves [1 + f * (obs - 1)].
     The resize picks [f = overlap_target / obs], which gives
     [1.5 - 0.5 / obs] — increasing in [obs], so its worst case is exactly at
     the trigger.  Requiring that worst case to stay above the 1.15 gate gives
     [obs >= 1.667]; [resize_trigger] is 1.7, for which a serialised run lands
     at 1.21 and still fails.  A well-calibrated regression never triggers a
     resize at all: at the ~0.45 target ratio it shows 1.45.

     That derivation is why there is no arbitrary floor on the resized count.
     An earlier draft clamped it at a quarter of the calibrated workload, which
     bought the same safety but could not correct a large blowout (a forced
     8.5x overshoot resized to the clamp and still failed).  With the trigger
     doing the work, an arbitrarily oversized calibration is corrected in one
     step and a serialised one still fails.

     {b The resize is a safety net, not the primary fix, and it does not cover
     everything.}  The trigger cannot be lowered — 1.667 is exactly where the
     derivation stops holding, and below it the resize starts being able to walk
     a genuine serialisation regression down to passing — so a calibration
     blowout landing in [(1.15, 1.667]] (roughly a 2.5-3.7x oversize) fails the
     gate and gets no resize, and retrying cannot help because a bad calibration
     is deterministic across trials.  That band is uncovered here BY
     CONSTRUCTION.

     That is why every fix for #538 and #569 is at the SOURCE rather than here:
     representative (contended) probes, the median fold, and the average-cost
     floor on the per-op estimate, all in [calibrate_read_ops].  #569 chose that
     route over the alternative of discriminating blowout from serialisation
     directly ([par.reader_phase ~ par.writer_phase + T_r] under serialisation
     vs [~ max] under overlap), which would decouple the trigger from the safety
     derivation entirely but was investigated during #559 and NOT adopted: on
     the reproduced failure it read 0.99 on a trial that was a blowout, because
     a reader-dominated workload leaves almost no overlap to detect.  It would
     have to be sized so readers never dominate before it could be trusted.

     After #569 the measured worst-case overlap on a load-8-16 box is 0.79 —
     comfortably below the 1.15 gate, let alone inside the uncorrectable band.
     The resize only has to catch what survives the source fixes. *)
  let overlap_target = 0.5 in
  let resize_trigger = 1.7 in
  (* The resized workload is RUN-scoped, not [measure]-scoped (#538 review).
     The calibration site above states that both configs deliberately share one
     calibration, because holding read_ops fixed is what makes their two
     speedups comparable.  A resize that lived inside [measure] would silently
     break that: config A would run at the resized count and config B at the
     original one, and the two printed speedups would no longer be comparable
     while still looking as though they were.  So the resize updates these
     refs, config B inherits it, and [resized] is a single run-wide budget —
     which is also the stricter reading of the "at most one resize" bound,
     since it prevents two shrinks compounding across configs. *)
  let cur_read_ops = ref read_ops in
  let cur_read_ops_src = ref read_ops_src in
  let resized = ref false in
  (* Outcome of each config that actually asserted, for the run-wide
     "measured nothing" check below. *)
  let outcomes = ref [] in
  let measure label =
    (* Neutralized runs take exactly one trial: nothing is asserted, so extra
       trials would only burn time. *)
    let budget = if min_speedup <= 0.0 then 1 else trials in
    (* [acc] holds whole trials (#603); the reduction is [verdict] over their
       COMPOSED metrics, applied once at the end, with the operator chosen per
       metric (see [verdict]).

       The early exit stops only on a full [Pass] — a complete, conclusive
       measurement of success.  It deliberately does NOT stop on an
       INCONCLUSIVE trial: that would throw away the very draws that could have
       measured something, and it is how one unmeasurable trial used to acquit
       a run. *)
    let rec loop t acc =
      let read_ops = !cur_read_ops
      and read_ops_src = !cur_read_ops_src in
      let base, par = trial label t ~read_ops ~read_ops_src in
      let this = trial_of ~base ~par in
      let acc = this :: acc in
      let observed = overlap_ratio this in
      if t >= budget || should_stop (outcome this ~min_speedup)
      then t, acc
      else if !resized || observed <= resize_trigger
      then loop (t + 1) acc
      else (
        (* Oversized workload rather than bad luck — shrink it once, run-wide,
           and restart the statistic.  See the header for why the bounds
           matter. *)
        let scaled =
          max 1 (int_of_float (float_of_int read_ops *. overlap_target /. observed))
        in
        Printf.printf
          "  [%s] readers overran the writer window (%.2f > %.2f); calibration was \
           oversized — resizing read_ops %d -> %d for the rest of the run and restarting \
           the statistic\n\
           %!"
          label
          observed
          resize_trigger
          read_ops
          scaled;
        cur_read_ops := scaled;
        cur_read_ops_src := "resized";
        resized := true;
        loop (t + 1) [])
    in
    let n_trials, acc = loop 1 [] in
    let v =
      match verdict ~min_speedup acc with
      | Some v -> v
      (* [loop] conses the trial it has just run before returning, so the list
         is never empty. *)
      | None -> Alcotest.failf "[%s] no trial recorded" label
    in
    Printf.printf
      "  [%s] %d trial(s), pool of %d, %d inconclusive (#603): median overlap=%.2f (gate \
       %.2f) best speedup=%.2fx (gate %.2fx); best trial baseline=%.3fs parallel=%.3fs \
       writer_done=%.3fs reader_done=%.3fs\n\
       %!"
      label
      n_trials
      (List.length v.v_trials)
      v.v_inconclusive
      v.v_overlap
      overlap_max
      v.v_speedup
      min_speedup
      v.v_rep.base_wall
      v.v_rep.par_wall
      v.v_rep.par_writer
      v.v_rep.par_reader;
    (* GRANARY_BENCH_MIN_SPEEDUP=0 is the documented way to neutralize this
       bench (coverage runs use it — instrumentation slows everything enough to
       make timing gates near-deterministic failures).  Honour it for BOTH
       gates: the workload is left uncalibrated in that mode, so an
       instrumented reader phase can legitimately outlast the writer. *)
    if min_speedup <= 0.0
    then
      Printf.printf
        "  [%s] gates neutralized (GRANARY_BENCH_MIN_SPEEDUP=%.2f)\n%!"
        label
        min_speedup
    else outcomes := assert_gates ~label ~v ~min_speedup :: !outcomes
  in
  (* Config A: shared tree (reader and writer on the SAME tree).  This is
     the #159 regression case — every writer commit CoW-paths the reader's
     tree, so only snapshot page-pinning keeps the reader's working set
     resident and the fsync-overlap win measurable. *)
  tid_read := 16;
  tid_write := 16;
  measure "shared tid";
  (* Config B: disjoint trees (the original config). *)
  tid_read := 16;
  tid_write := 99;
  measure "disjoint tid";
  (* Per-config INCONCLUSIVE is a pass and must stay one — that case is real
     (observed at load 11.7 during the #582 review: one config unresolvable, the
     other asserted and passed), and failing on it would reintroduce exactly the
     false failures the guard was added to remove.

     A run in which EVERY config was inconclusive, however, measured nothing.
     If sizing or host headroom ever collapses systematically the bench would
     otherwise report INCONCLUSIVE forever and the job would stay green — "the
     gate stops guarding, CI stays green" is precisely the failure #574/#582
     exist to eliminate on the shell side, and it must not be reintroduced here
     on the timing side.

     {b It must test [is_inconclusive], not [= Inconclusive_unreachable].}  An
     earlier revision tested only the unreachable kind and therefore missed the
     path a LOADED box actually takes: [outcome] tests writer inflation BEFORE
     the ceiling, so a starved host reports [Inconclusive_headroom].  Observed
     live at load ~19 — shared tid unreachable, disjoint tid headroom, nothing
     asserted, RC=0 — which is exactly the hole this guard exists to close. *)
  if !outcomes <> [] && List.for_all is_inconclusive !outcomes
  then
    Alcotest.failf
      "every config was INCONCLUSIVE, so this run measured nothing.  Per-config \
       INCONCLUSIVE is a pass; ALL configs inconclusive is a sizing or headroom failure, \
       not a result (#603)."
;;

(* ------------------------------------------------------------------ *)
(* Statistics, tested WITHOUT the clock.                              *)
(*                                                                    *)
(* Both #603 and #590 are reduction-order defects: the code composed a *)
(* ratio out of independently-reduced parts instead of reducing over   *)
(* composed ratios.  Neither needs a stopwatch to demonstrate, and     *)
(* neither should be pinned with one — a wall-clock reproduction of a  *)
(* statistics bug is a flake generator.  So the reductions live in     *)
(* pure functions ([verdict], [per_op_estimate]) and these cases feed  *)
(* them the exact numbers from the two issues and from the review.     *)
(*                                                                    *)
(* These run in EVERY job, including the ones that neutralize the      *)
(* timing gates with GRANARY_BENCH_MIN_SPEEDUP=0.                      *)
(* ------------------------------------------------------------------ *)

let min_speedup_ref = 1.2

(* A trial with the requested composed speedup, no writer inflation, and an
   overlap of 0.5 — comfortably inside the 1.15 gate, so [outcome] turns on the
   speedup alone. *)
let trial_with ~speedup:s ~base_wall ~par_writer =
  { base_wall
  ; par_wall = base_wall /. s
  ; base_writer = par_writer
  ; par_writer
  ; par_reader = par_writer *. 0.5
  }
;;

(* A FULLY SERIALISED engine: readers cannot start until the writer's last
   fsync has returned, so the parallel run is the sum of the two phases and the
   overlap ratio is [1 + r/inflation].  [inflation] is the writer phase's own
   load perturbation, which is the noise direction that pushes this metric
   toward PASSING — the whole subject of the B1 review finding. *)
let serialised ~r ~tw ~inflation =
  let par_writer = inflation *. tw in
  { base_wall = tw +. (r *. tw)
  ; par_wall = par_writer +. (r *. tw)
  ; base_writer = tw
  ; par_writer
  ; par_reader = par_writer +. (r *. tw)
  }
;;

let min_over f ts = List.fold_left (fun acc t -> Float.min acc (f t)) infinity ts

let vd ts =
  match verdict ~min_speedup:min_speedup_ref ts with
  | Some v -> v
  | None -> Alcotest.fail "no trial"
;;

let status ts = (vd ts).v_status
let outcome_of t = outcome t ~min_speedup:min_speedup_ref

(* #603, the exact case from the issue: three trials at 0.98, 0.96 and 1.44.
   Reducing each phase with [min] and composing the minima afterwards yields
   1.02 — a number no trial produced, below the 1.20 floor, from a run in which
   one trial reached 1.44. *)
let composed_speedup_must_be_a_trials_own () =
  let ts =
    [ trial_with ~speedup:0.98 ~base_wall:2.04 ~par_writer:1.6
    ; trial_with ~speedup:0.96 ~base_wall:2.40 ~par_writer:1.9
    ; trial_with ~speedup:1.44 ~base_wall:2.88 ~par_writer:2.0
    ]
  in
  let minima = min_over (fun t -> t.base_wall) ts /. min_over (fun t -> t.par_wall) ts in
  Alcotest.(check (float 1e-9)) "min-per-phase composes to the 1.02 from #603" 1.02 minima;
  Alcotest.(check bool)
    "1.02 is no trial's own speedup"
    true
    (List.for_all (fun t -> Float.abs (speedup t -. minima) > 1e-6) ts);
  Alcotest.(check bool) "and it fails the 1.20 floor" false (minima >= min_speedup_ref);
  let reported = (vd ts).v_speedup in
  Alcotest.(check (float 1e-9)) "reducing over composed ratios reports 1.44" 1.44 reported;
  Alcotest.(check bool)
    "which is a ratio some trial actually achieved"
    true
    (List.exists (fun t -> Float.abs (speedup t -. reported) < 1e-9) ts);
  Alcotest.(check bool) "and the config passes" true (status ts = Pass)
;;

(* {b The B1 regression pin.}  A fully serialised engine, calibration exactly on
   the 0.45 target, 15 trials — and ONE trial whose writer was inflated by load.
   That trial's own overlap is [1 + 0.45/f], which drops INSIDE the 1.15 gate
   for any [f] past ~3, and the code classifies it [Inconclusive_headroom]
   because its writer inflated past 1.25x.  (The headline case uses 3.2x rather
   than the review's 3.0x for one reason only: at exactly 3.0x the ratio is
   1.15 to within one ulp and the comparison is a coin toss.  The sweep at the
   end covers 3.0x anyway.)

   Any reduction that ranks "unmeasurable" above "measured a failure" — which
   an earlier revision of this file did, scoring Inconclusive above Fail and
   taking a maximum — lets that ONE trial in fifteen acquit a broken engine.
   The escape gets MONOTONICALLY easier as GRANARY_BENCH_TRIALS rises, and the
   nightly runs 15. *)
let one_unmeasurable_trial_cannot_acquit_a_regression () =
  let clean i = serialised ~r:0.45 ~tw:(1.0 +. (0.03 *. float_of_int i)) ~inflation:1.0 in
  let inflated = serialised ~r:0.45 ~tw:1.0 ~inflation:3.2 in
  let ts = List.init 14 clean @ [ inflated ] in
  (* The escape hatch, stated precisely so it cannot be reintroduced silently. *)
  Alcotest.(check (float 1e-9))
    "the inflated trial reads inside the gate"
    1.140625
    (overlap_ratio inflated);
  Alcotest.(check bool)
    "and is classified unmeasurable, not failing"
    true
    (outcome_of inflated = Inconclusive_headroom);
  Alcotest.(check bool)
    "so on its own numbers it would have passed"
    true
    (overlap_ratio inflated <= overlap_max);
  (* What the fixed reduction does with it. *)
  Alcotest.(check int)
    "the pool excludes it, leaving the 14 conclusive trials"
    14
    (List.length (vd ts).v_trials);
  Alcotest.(check int) "inconclusive count" 1 (vd ts).v_inconclusive;
  Alcotest.(check (float 1e-9))
    "median overlap is the serialised 1.45, not the inflated 1.15"
    1.45
    (vd ts).v_overlap;
  Alcotest.(check bool) "and the run FAILS" true (status ts = Fail_overlap);
  (* Not a fluke of one inflation level: sweep the escape closed. *)
  List.iter
    (fun f ->
       let ts = List.init 14 clean @ [ serialised ~r:0.45 ~tw:1.0 ~inflation:f ] in
       Alcotest.(check bool)
         (Printf.sprintf "still fails with a %.2fx-inflated trial" f)
         true
         (status ts = Fail_overlap))
    [ 1.0; 1.2; 1.3; 1.5; 2.0; 2.5; 3.0; 3.5 ]
;;

(* B2's other half: the serialisation pin must contain DICE.  Every trial here
   has its own writer duration AND its own inflation, so the fifteen overlap
   ratios are genuinely different numbers; a reduction that silently changed
   from median to min, or that let the most-perturbed draw win, would move the
   verdict.  (The predecessor of this test used [par_reader = 1.45 * par_writer]
   for every trial, so [par_writer] cancelled and all fifteen ratios were
   identically 1.4500 — it passed under best, worst, median, or a deliberately
   broken reduction, and it was the only test backing the safety claim.) *)
let serialisation_is_still_caught () =
  let ts =
    List.init 15 (fun i ->
      let tw = 1.0 +. (0.11 *. float_of_int (i mod 7)) in
      let inflation = 1.0 +. (0.02 *. float_of_int (i mod 11)) in
      serialised ~r:0.45 ~tw ~inflation)
  in
  let ratios = List.map overlap_ratio ts in
  let spread =
    List.fold_left Float.max neg_infinity ratios
    -. List.fold_left Float.min infinity ratios
  in
  Alcotest.(check bool) "the trials genuinely differ" true (spread > 0.05);
  Alcotest.(check bool)
    "every trial is over the gate"
    true
    (List.for_all (fun x -> x > overlap_max) ratios);
  Alcotest.(check bool) "and the run fails" true (status ts = Fail_overlap);
  (* The claim this test exists for, stated as the reduction property rather
     than as a slogan: no draw of the dice acquits a serialised engine, because
     the reduction cannot report a number below the population it is drawn
     from. *)
  Alcotest.(check bool)
    "the reported overlap is inside the observed range"
    true
    (List.exists (fun x -> Float.abs (x -. (vd ts).v_overlap) < 1e-9) ratios)
;;

(* The overlap statistic under min-per-phase could land BELOW every trial's own
   ratio, manufacturing a failure nothing measured.  Four healthy trials plus
   one perturbed draw: the minima compose to 1.25 and fail the 1.15 gate, while
   the median over the composed ratios reports the 0.50 that four of the five
   trials actually achieved. *)
let overlap_reduction_must_not_manufacture_a_failure () =
  let healthy =
    { base_wall = 3.0
    ; par_wall = 2.0
    ; base_writer = 2.0
    ; par_writer = 2.0
    ; par_reader = 1.0
    }
  and perturbed =
    { base_wall = 3.0
    ; par_wall = 2.4
    ; base_writer = 2.0
    ; par_writer = 0.8
    ; par_reader = 1.6
    }
  in
  let ts = [ healthy; healthy; healthy; healthy; perturbed ] in
  let minima =
    min_over (fun t -> t.par_reader) ts /. min_over (fun t -> t.par_writer) ts
  in
  Alcotest.(check (float 1e-9)) "min-per-phase overlap" 1.25 minima;
  Alcotest.(check bool) "which fails the 1.15 gate" false (minima <= overlap_max);
  Alcotest.(check bool)
    "no trial actually measured 1.25"
    true
    (List.for_all (fun t -> Float.abs (overlap_ratio t -. minima) > 1e-6) ts);
  Alcotest.(check (float 1e-9)) "median over composed ratios" 0.5 (vd ts).v_overlap;
  Alcotest.(check bool) "and the config passes" true (status ts = Pass)
;;

(* Raising GRANARY_BENCH_TRIALS must not drift the verdict in EITHER direction:
   not toward failing for a healthy engine (which is what min-per-phase did, and
   why "raise the trials" was bad advice for this gate), and not toward passing
   for a regressed one (which is what best-of-everything did — B1).

   Note what is NOT claimed: the reported speedup is not monotone in N, and
   neither is the pass score.  Adding a conclusive trial to an all-inconclusive
   pool can turn an INCONCLUSIVE pass into a FAIL, and that is the correct
   direction — it is more measurement, not less. *)
let raising_trials_does_not_drift_the_verdict () =
  let healthy i =
    let w = 1.0 +. (0.07 *. float_of_int (i mod 5)) in
    { base_wall = 1.5 *. w
    ; par_wall = 1.02 *. w
    ; base_writer = w
    ; par_writer = 1.02 *. w
    ; par_reader = 0.45 *. w
    }
  in
  let regressed i =
    serialised ~r:0.45 ~tw:(1.0 +. (0.05 *. float_of_int (i mod 4))) ~inflation:1.0
  in
  List.iter
    (fun n ->
       Alcotest.(check bool)
         (Printf.sprintf "healthy engine still passes at N=%d" n)
         true
         (status (List.init n healthy) = Pass);
       Alcotest.(check bool)
         (Printf.sprintf "regressed engine still fails at N=%d" n)
         true
         (status (List.init n regressed) = Fail_overlap))
    [ 1; 2; 3; 5; 9; 15; 31 ]
;;

(* The overlap invariant is declared UNCONDITIONAL: it is asserted before either
   inconclusive case can suppress anything.  Nothing tested that ordering, and a
   mutant that moved the check below them survived the suite.  A trial can be
   both wildly over the gate AND have an inflated writer; it must read
   [Fail_overlap]. *)
let overlap_invariant_outranks_inconclusive () =
  let t =
    { base_wall = 2.0
    ; par_wall = 5.0
    ; base_writer = 1.0
    ; par_writer = 3.0
    ; par_reader = 5.0
    }
  in
  Alcotest.(check bool) "writer is inflated past the threshold" true (inconclusive t);
  Alcotest.(check bool)
    "ceiling is under the floor too"
    true
    (speedup_unreachable t ~min_speedup:min_speedup_ref);
  Alcotest.(check bool) "yet the overlap invariant wins" true (outcome_of t = Fail_overlap);
  Alcotest.(check bool) "and the config fails" true (status [ t ] = Fail_overlap)
;;

(* Per-config INCONCLUSIVE stays a pass (#582).  The run-wide guard in
   [test_fsync_overlap] is what stops ALL configs going inconclusive forever,
   and it must recognise BOTH kinds: [outcome] tests writer inflation before the
   ceiling, so a loaded box reports HEADROOM, which an earlier revision of the
   guard did not look for (B3). *)
let inconclusive_is_a_pass_per_config_but_not_run_wide () =
  let unreachable =
    { base_wall = 1.02
    ; par_wall = 1.00
    ; base_writer = 1.0
    ; par_writer = 1.0
    ; par_reader = 0.14
    }
  and headroom =
    { base_wall = 2.0
    ; par_wall = 2.0
    ; base_writer = 1.0
    ; par_writer = 2.0
    ; par_reader = 0.4
    }
  in
  Alcotest.(check bool)
    "ceiling under the floor is reported unreachable"
    true
    (outcome_of unreachable = Inconclusive_unreachable);
  Alcotest.(check bool)
    "an inflated writer is reported as headroom"
    true
    (outcome_of headroom = Inconclusive_headroom);
  Alcotest.(check bool)
    "both are per-config passes"
    true
    (verdict_passes (vd [ unreachable ]) && verdict_passes (vd [ headroom ]));
  (* The run-wide guard's predicate, exactly as [test_fsync_overlap] applies it
     across the two configs. *)
  Alcotest.(check bool)
    "and a run of nothing but these two measured nothing"
    true
    (List.for_all is_inconclusive [ status [ unreachable ]; status [ headroom ] ]);
  (* One conclusive config is enough to make the run a result again. *)
  let real = trial_with ~speedup:1.44 ~base_wall:2.88 ~par_writer:2.0 in
  Alcotest.(check bool)
    "one conclusive config makes it a result"
    false
    (List.for_all is_inconclusive [ status [ unreachable ]; status [ real ] ])
;;

(* #590, the draw quoted in [calibrate_read_ops]: one rep landed in an idle
   window and reads 103 us where the workload really costs ~300 us.  Composing
   per rep and taking the median discards it.  The [max (marginal, average)]
   floor it replaces returns 325 us here — above every rep's true cost — which
   is the systematic undersizing of read_ops that halved the gate's margin. *)
let per_op_rejects_the_lucky_draw_without_the_high_bias () =
  let pairs = [ 0.40, 0.503; 0.35, 0.65; 0.42, 0.76 ] in
  let est = per_op_estimate ~probe_ops:1000 pairs in
  Alcotest.(check (float 1e-12)) "median of per-rep marginals" 3.0e-4 est;
  Alcotest.(check bool) "the 103 us draw is rejected" true (est > 1.03e-4);
  (* What #569's [max (median t1 marginal, average)] would have said. *)
  let old_marginal = (0.65 -. 0.40) /. 1000.0
  and old_average = 0.65 /. 2000.0 in
  let old_est = Float.max old_marginal old_average in
  Alcotest.(check (float 1e-12)) "the estimator this replaced" 3.25e-4 old_est;
  Alcotest.(check bool) "which was biased above the true cost" true (old_est > est)
;;

(* Why [max (_, average)] cannot be a "floor that only removes a low tail":
   [average = p + F/2n] exceeds [p] for any positive fixed cost, so the max is
   ALWAYS at least a biased-high quantity.  With a 0.20-0.30 s cold cost over
   2000 probe ops it overestimates a 250 us workload by 50%. *)
let per_op_is_unbiased_under_varying_fixed_cost () =
  (* t = F + n * 250us, for F in {0.20, 0.30, 0.25} and n in {1000, 2000}. *)
  let pairs = [ 0.45, 0.70; 0.55, 0.80; 0.50, 0.75 ] in
  Alcotest.(check (float 1e-12))
    "recovers the true per-op cost exactly"
    2.5e-4
    (per_op_estimate ~probe_ops:1000 pairs);
  Alcotest.(check (float 1e-12))
    "where the average alone reads 50% high"
    3.75e-4
    (0.75 /. 2000.0)
;;

(* The average survives in its other #569 role: the fallback for when the
   difference carries no signal.  The exact trigger is the MEDIAN marginal being
   non-positive — i.e. a MAJORITY of reps, not all of them, which is what the
   comment used to say.  Both sides of that boundary are pinned here. *)
let per_op_falls_back_when_the_median_rep_is_swamped () =
  let swamped = [ 0.5, 0.4; 0.6, 0.5; 0.55, 0.45 ] in
  Alcotest.(check (float 1e-12))
    "all three swamped: median of the per-rep averages"
    2.25e-4
    (per_op_estimate ~probe_ops:1000 swamped);
  (* 2 of 3 non-positive -> the median is non-positive -> fallback. *)
  let two_of_three = [ 0.5, 0.4; 0.6, 0.5; 0.30, 0.60 ] in
  Alcotest.(check (float 1e-12))
    "2 of 3 swamped: still the fallback"
    2.5e-4
    (per_op_estimate ~probe_ops:1000 two_of_three);
  (* 1 of 3 non-positive -> the median is a real marginal -> no fallback. *)
  let one_of_three = [ 0.5, 0.4; 0.30, 0.62; 0.30, 0.60 ] in
  Alcotest.(check (float 1e-12))
    "1 of 3 swamped: the median marginal stands"
    3.0e-4
    (per_op_estimate ~probe_ops:1000 one_of_three)
;;

(* #623, part 1 of 3: the new fold is the old one generalised.  [trimmed_mean]
   drops one draw from each end, so on three draws it leaves exactly the middle
   one and IS [median].  That identity is what makes #623 a variance change
   rather than a re-decision of #538's and #590's fold — and it is why every
   pinned [per_op_estimate] case above, all of which feed three pairs, still
   reads the number it always did.  Delete this and the two folds could drift
   apart on the small-sample path with nothing to say so. *)
let trimmed_mean_is_the_median_generalised () =
  let same label l = Alcotest.(check (float 1e-12)) label (median l) (trimmed_mean l) in
  same "empty" [];
  same "one draw" [ 3.0e-4 ];
  same "two draws" [ 3.0e-4; 5.0e-4 ];
  same "three draws, sorted" [ 1.0e-4; 3.0e-4; 9.0e-4 ];
  same "three draws, unsorted" [ 9.0e-4; 1.0e-4; 3.0e-4 ];
  (* Four is where they part company: the median takes the upper middle, the
     trim averages both middles. *)
  Alcotest.(check (float 1e-12))
    "four draws: the mean of the two middles"
    3.5e-4
    (trimmed_mean [ 1.0e-4; 3.0e-4; 4.0e-4; 9.0e-4 ]);
  Alcotest.(check (float 1e-12))
    "four draws: the median takes the upper middle"
    4.0e-4
    (median [ 1.0e-4; 3.0e-4; 4.0e-4; 9.0e-4 ])
;;

(* #623, part 2 of 3: the fold must not err LOW.  A per-op estimate that is too
   low oversizes [read_ops] — the direction [calibrate_read_ops] calls unsafe
   and #538's root cause — so a variance reduction that bought its narrowness by
   shifting the centre downward would be a regression dressed as a fix.

   Two parents, and they bracket what a loaded box does to the marginals:
   symmetric noise (which is what a difference of two one-sided delays actually
   has) leaves the two folds on the same centre, and right-skewed noise puts the
   trimmed mean ABOVE the median.  Neither is below.  Both halves are needed:
   the symmetric case alone would not notice a fold that drifted down under
   skew, and the skewed case alone would not notice one that drifted down when
   the box is quiet. *)
let trimmed_mean_does_not_err_low_against_the_median () =
  (* Symmetric about 3.0e-4. *)
  let symmetric = [ 1.0e-4; 2.0e-4; 3.0e-4; 4.0e-4; 5.0e-4 ] in
  Alcotest.(check (float 1e-12))
    "symmetric parent: same centre as the median"
    (median symmetric)
    (trimmed_mean symmetric);
  (* Right-skewed: one long upper tail, which is what load adds. *)
  let skewed = [ 2.0e-4; 2.5e-4; 3.0e-4; 6.0e-4; 20.0e-4 ] in
  Alcotest.(check bool)
    "right-skewed parent: at or above the median, never below"
    true
    (trimmed_mean skewed >= median skewed);
  Alcotest.(check (float 1e-12))
    "and the extreme draw is still discarded"
    ((2.5e-4 +. 3.0e-4 +. 6.0e-4) /. 3.0)
    (trimmed_mean skewed);
  (* The same statement at the estimator level, where it is what actually
     reaches [read_ops]: seven reps whose marginals are the skewed set above
     plus two more, and the estimate stays at or above what the median says. *)
  let pairs = List.map (fun m -> 0.5, 0.5 +. (m *. 1000.0)) skewed in
  let marginals = List.map (fun (t1, t2) -> (t2 -. t1) /. 1000.0) pairs in
  Alcotest.(check bool)
    "per_op_estimate inherits it"
    true
    (per_op_estimate ~probe_ops:1000 pairs >= median marginals)
;;

(* #623, part 3 of 3, and the load-bearing one: the pair of changes actually
   NARROWS the spread of the chosen workload.

   [read_ops] is [target_seconds / per_op], so the run-to-run spread of the
   divisor IS the run-to-run spread of the workload — the 3324-10906 (~3.3x)
   band #623 was filed about.  This measures that spread directly and
   deterministically, with no clock and no box involved: a fixed LCG supplies
   iid symmetric noise around a known true per-op cost, and each simulated
   "run" is reduced both the old way (median of 3 reps) and the new way
   (trimmed mean of 7).  Asserting on the RATIO of the two spreads rather than
   on either absolute number is what keeps it independent of the noise scale
   chosen here.

   A wall-clock measurement could not stand in for this.  #623 asks for a
   quiet box and this repo's own guidance says sibling agents make that
   unavailable; the estimator's variance, unlike a false-failure rate, is a
   property of the arithmetic and is therefore measurable anywhere. *)
let trimmed_mean_narrows_the_calibration_spread () =
  let seed = ref 20260903 in
  (* Uniform on [-1, 1); an LCG so the sample is identical on every machine and
     every run.  Symmetric, per [trimmed_mean]'s header: the marginal is a
     difference of two one-sided delays. *)
  let noise () =
    seed := ((1103515245 * !seed) + 12345) land 0x3FFFFFFF;
    (float_of_int !seed /. 536870912.0) -. 1.0
  in
  let p = 3.0e-4 in
  let draw () = p +. (0.6 *. p *. noise ()) in
  let runs = 500 in
  (* An explicit loop, not [List.init]: the generator is STATEFUL, so a fold
     whose application order the stdlib leaves unspecified would make the
     printed numbers depend on the compiler.  Both folds sort their input, so
     the order within one simulated run is irrelevant to the result — but the
     reproducibility of the number this prints is not. *)
  let rec repeat n f acc = if n = 0 then acc else repeat (n - 1) f (f () :: acc) in
  let spread_of f =
    let xs = repeat runs f [] in
    let lo = List.fold_left Float.min infinity xs
    and hi = List.fold_left Float.max neg_infinity xs in
    hi /. lo
  in
  (* Drawn from one stream, so the two estimators see the same noise process;
     the OLD one is measured first so it cannot be handed a tamer tail. *)
  let old_spread = spread_of (fun () -> median (repeat 3 draw [])) in
  let new_spread = spread_of (fun () -> trimmed_mean (repeat 7 draw [])) in
  Printf.printf
    "  [#623] simulated per_op spread: median-of-3 %.3fx -> trimmed-mean-of-7 %.3fx\n%!"
    old_spread
    new_spread;
  Alcotest.(check bool)
    "the old fold's spread is the wider one"
    true
    (new_spread < old_spread);
  (* The prediction in [probe_reps] is sigma ~0.59x, i.e. a multiplicative
     spread of [s] becoming about [s ** 0.59].  Assert something weaker and
     unambiguous — a third of the excess over 1.0 removed — so this pins the
     direction and the order of magnitude without pinning an LCG's tail. *)
  Alcotest.(check bool)
    "and by a wide margin, not a rounding"
    true
    (new_spread -. 1.0 < 0.67 *. (old_spread -. 1.0))
;;

(* {b The median itself, pinned.}  Mutation testing found that changing
   [v_overlap]'s reduction from MEDIAN to MIN survived the whole suite — the one
   reduction choice this revision exists to make was pinned by nothing.  Every
   trial here is CONCLUSIVE, so the pool filter is not what is under test: the
   reported number must be the middle of the composed ratios, not the smallest,
   largest, first or last. *)
let overlap_is_the_median_of_the_pool () =
  (* Conclusive by construction: inflation 1.0, ceiling 1.5, speedup 1.5. *)
  let with_overlap o =
    { base_wall = 3.0
    ; par_wall = 2.0
    ; base_writer = 2.0
    ; par_writer = 2.0
    ; par_reader = 2.0 *. o
    }
  in
  let ts = List.map with_overlap [ 0.30; 1.10; 0.50; 0.40; 0.90 ] in
  Alcotest.(check int) "all five are conclusive" 0 (vd ts).v_inconclusive;
  Alcotest.(check (float 1e-9)) "reports the median" 0.5 (vd ts).v_overlap;
  (* Each of the plausible wrong reductions, named so the mutant is dead. *)
  Alcotest.(check bool)
    "not the minimum"
    false
    (Float.abs ((vd ts).v_overlap -. 0.30) < 1e-9);
  Alcotest.(check bool)
    "not the maximum"
    false
    (Float.abs ((vd ts).v_overlap -. 1.10) < 1e-9);
  (* And the choice changes the VERDICT, not just the printed number: the
     minimum would report 0.40 and pass where the median reports 1.30. *)
  let leaky = List.map with_overlap [ 0.40; 1.20; 1.30; 1.40; 1.50 ] in
  Alcotest.(check (float 1e-9))
    "median over a failing population"
    1.3
    (vd leaky).v_overlap;
  Alcotest.(check bool) "which fails" true (status leaky = Fail_overlap);
  Alcotest.(check bool)
    "where the minimum would have passed"
    true
    (min_over overlap_ratio leaky <= overlap_max);
  (* Even counts take the UPPER of the two middles, i.e. err toward failing. *)
  Alcotest.(check (float 1e-9))
    "even count: upper middle"
    0.6
    (vd (List.map with_overlap [ 0.40; 0.60 ])).v_overlap;
  Alcotest.(check (float 1e-9))
    "even count of four: upper middle"
    0.6
    (vd (List.map with_overlap [ 0.20; 0.40; 0.60; 0.80 ])).v_overlap
;;

(* {b The early exit, pinned.}  It was the second half of B1 — an earlier
   revision stopped on the first trial with a non-failing outcome, i.e. on the
   first INCONCLUSIVE one, which is why its armed runs printed [best-of-1] for
   both configs with both inconclusive.  It lived inside [measure]'s loop where
   no pure test could reach it; [should_stop] exists so this is one line. *)
let early_exit_stops_only_on_a_pass () =
  Alcotest.(check bool) "a full Pass stops the loop" true (should_stop Pass);
  List.iter
    (fun (o, name) ->
       Alcotest.(check bool)
         (Printf.sprintf "%s keeps drawing" name)
         false
         (should_stop o))
    [ Fail_overlap, "Fail_overlap"
    ; Fail_speedup, "Fail_speedup"
    ; Inconclusive_headroom, "Inconclusive_headroom"
    ; Inconclusive_unreachable, "Inconclusive_unreachable"
    ]
;;

(* #628: the database path must be drawn per run, not shared.  Two concurrent
   suite runs on this box is the DOCUMENTED normal working mode, and a shared
   path corrupts the input silently — the run still completes and reports a
   number no configuration actually produced.  Cheap to pin, and the pin is what
   stops the constant coming back as a "simplification". *)
let the_db_path_is_drawn_per_run () =
  Alcotest.(check bool)
    "not the fixed shared name #628 was about"
    false
    (String.equal path "/tmp/granary_bench_fsync_overlap.db");
  let a = fresh_db_path () in
  let b = fresh_db_path () in
  Alcotest.(check bool) "two draws differ" false (String.equal a b);
  Alcotest.(check bool)
    "this run's path is not a name another run would draw"
    false
    (String.equal path a)
;;

let statistics_tests =
  [ Alcotest.test_case
      "the db path is drawn per run (#628)"
      `Quick
      the_db_path_is_drawn_per_run
  ; Alcotest.test_case
      "composed speedup is a trial's own (#603)"
      `Quick
      composed_speedup_must_be_a_trials_own
  ; Alcotest.test_case
      "one unmeasurable trial cannot acquit a regression (B1)"
      `Quick
      one_unmeasurable_trial_cannot_acquit_a_regression
  ; Alcotest.test_case
      "serialisation still caught, with variance (B2)"
      `Quick
      serialisation_is_still_caught
  ; Alcotest.test_case
      "overlap reduction manufactures no failure (#603)"
      `Quick
      overlap_reduction_must_not_manufacture_a_failure
  ; Alcotest.test_case
      "raising trials does not drift the verdict (#603)"
      `Quick
      raising_trials_does_not_drift_the_verdict
  ; Alcotest.test_case
      "overlap is the median of the pool"
      `Quick
      overlap_is_the_median_of_the_pool
  ; Alcotest.test_case
      "early exit stops only on a Pass (B1)"
      `Quick
      early_exit_stops_only_on_a_pass
  ; Alcotest.test_case
      "overlap invariant outranks inconclusive"
      `Quick
      overlap_invariant_outranks_inconclusive
  ; Alcotest.test_case
      "inconclusive: per-config pass, run-wide guard (B3)"
      `Quick
      inconclusive_is_a_pass_per_config_but_not_run_wide
  ; Alcotest.test_case
      "per-op estimate: no lucky draw, no high bias (#590)"
      `Quick
      per_op_rejects_the_lucky_draw_without_the_high_bias
  ; Alcotest.test_case
      "per-op estimate is unbiased in the fixed cost (#590)"
      `Quick
      per_op_is_unbiased_under_varying_fixed_cost
  ; Alcotest.test_case
      "per-op fallback fires on the median rep (#590)"
      `Quick
      per_op_falls_back_when_the_median_rep_is_swamped
  ; Alcotest.test_case
      "trimmed mean generalises the median (#623)"
      `Quick
      trimmed_mean_is_the_median_generalised
  ; Alcotest.test_case
      "trimmed mean never errs low (#623)"
      `Quick
      trimmed_mean_does_not_err_low_against_the_median
  ; Alcotest.test_case
      "trimmed mean narrows the calibration spread (#623)"
      `Quick
      trimmed_mean_narrows_the_calibration_spread
  ]
;;

let () =
  Alcotest.run
    "wal_fsync_overlap"
    [ "statistics", statistics_tests
    ; ( "bench"
      , [ Alcotest.test_case
            "parallel reads overlap writer fsync"
            `Slow
            test_fsync_overlap
        ] )
    ]
;;
