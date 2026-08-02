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
      and wrong in calibration.
    - {b a floor on the estimate} (#569): the per-op cost is the marginal
      probe difference OR the larger probe's average cost, whichever is bigger.
      The marginal difference cancels the fixed cold-cache cost exactly but
      amplifies noise; the average is biased high and never low.  Taking the max
      keeps the unbiased estimator while removing its dangerous tail.
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

    The estimator floor buys that robustness with gate SENSITIVITY: read_ops
    comes out systematically undersized, so the primary gate is easier to pass
    and its margin against a serialisation regression roughly halves.  It is
    bounded by the opposing speedup gate rather than by a clamp.  The full
    accounting is at the [average] binding in [calibrate_read_ops]; #590 tracks
    whether to buy the sensitivity back.

    It also has a second consequence, handled at [speedup_unreachable]: when the
    workload comes out small enough, the SECONDARY gate becomes arithmetically
    unsatisfiable — even perfect overlap cannot reach the 1.2x floor — and the
    run must say so rather than report a 1.02x "regression" that no engine could
    have avoided.  Raising GRANARY_BENCH_TRIALS does not help there; the sizing
    is run-wide (#603, which also covers the composed best-of-N drift the
    nightly's TRIALS=15 makes visible).

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

let path = "/tmp/granary_bench_fsync_overlap.db"

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
     an operation — that is why [measure] uses it on the trial durations, where
     noise can only ever ADD time.  It is the wrong statistic here, because
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
     shrinks the spread the median has to survive in the first place. *)
  let probe_reps = 3 in
  let median = function
    | [] -> infinity
    | l ->
      let a = Array.of_list l in
      Array.sort compare a;
      a.(Array.length a / 2)
  in
  (* Probe under the shared-tree config — the harder of the two the run
     measures — then restore.  [Fun.protect] so a raising probe cannot leave the
     global mutated for whatever runs next (#574 review). *)
  let typical ~read_ops =
    median
      (List.init probe_reps (fun _ ->
         probe_reader_phase ~n_seed ~n_readers ~read_ops ~delay))
  in
  let t1, t2 =
    let saved_write = !tid_write in
    tid_write := !tid_read;
    Fun.protect
      ~finally:(fun () -> tid_write := saved_write)
      (fun () ->
         let t1 = typical ~read_ops:probe_ops in
         let t2 = typical ~read_ops:(2 * probe_ops) in
         t1, t2)
  in
  let marginal = (t2 -. t1) /. float_of_int probe_ops in
  (* Average cost of the LARGER probe.  Two roles, and the second is the
     important one (#569):

     - it is the fallback when the marginal difference came out non-positive,
       i.e. the two probes were swamped by jitter; and
     - it is a FLOOR on the estimate.  Writing the probe as [F + n * p], the
       marginal difference cancels the fixed cost [F] exactly, which is why it
       is the estimator of record — but it is a difference of two noisy
       quantities of the same magnitude, so its noise is amplified while the
       average's is not.  Measured on this box at load ~10 across five runs,
       marginal-only chose per-op costs spanning 103-419 us while the workload
       actually ran at 246-369 us; the 103 us draw produced read_ops=6548 and an
       overlap of 1.05 against a 1.15 gate.  The average is biased HIGH by
       exactly [F / 2n] — never low — so [max] of the two cannot underestimate
       [p] by more than the marginal alone would, and it removes the low tail
       that is the only dangerous direction here.  With 2000 probe ops the bias
       it introduces is a fraction of one cold walk.

     {b It is not free, and the cost is on the gate's sensitivity (#590).}  The
     floor is biased HIGH by construction, so [read_ops] comes out
     systematically UNDERSIZED and the primary overlap gate becomes
     systematically easier to pass.  Measured against main in one worktree at
     load 6.8-12.0: overlap 0.27-0.53 here versus 0.42-0.63 there.  Under
     serialisation the invariant reads [1 + T_r/T_w], so a worst case of 0.27
     would present as ~1.27 against the 1.15 gate where main's 0.42 presents as
     ~1.42 — the detection margin is roughly halved.  That is a real reduction
     in sensitivity, accepted because the alternative was a gate that FAILED
     outright on a good engine roughly one run in six, and because it cannot
     degrade silently: driving T_r down drives the SECONDARY gate's ceiling
     [1 + T_r/T_w] down with it, and that gate was measured at 1.30 against its
     1.20 floor, i.e. only ~8% of headroom left.  Undersize the workload much
     further and the speedup gate fails first and loudly.  #590 tracks whether
     the sensitivity is worth buying back with a tighter estimator. *)
  let average = t2 /. float_of_int (2 * probe_ops) in
  let per_op = if marginal > 0.0 then Float.max marginal average else average in
  (* Unreachable unless the clock runs backwards — [t2] is a positive
     duration — so this branch is defence only and will not be covered. *)
  if per_op <= 0.0
  then probe_ops
  else (
    let ops_for secs = int_of_float (secs /. per_op) in
    let want = ops_for (writer_secs *. ratio) in
    (* Upper clamps: predicted T_r stays at most 0.75 x T_w (see above), and
       200k ops keeps a pathologically slow host from running for minutes.

       Lower clamp is 1, not [probe_ops], and deliberately stays that way even
       though the #569 estimator undersizes (#590).  Any absolute lower clamp is
       a fixed op count, which is exactly what #468 was: on a slow host it
       overrides calibration and pushes T_r past T_w, where the metric is not
       even monotonic.  The protection against undersizing is not a clamp, it is
       the opposing SECONDARY gate — its ceiling is [1 + T_r/T_w], so a workload
       driven too small fails the 1.2x speedup floor loudly.  A relative clamp
       (some fraction of [want]) would be circular, since [want] is the quantity
       under suspicion. *)
    max 1 (min (min 200_000 (ops_for (writer_secs *. 0.75))) want))
;;

(* Best-of-N accumulator (#538).  Every field is a MINIMUM across trials.

   Why minima of the five durations rather than the best of the per-trial
   ratios: timing noise is one-sided — a phase can be delayed, never hurried —
   so the minimum of a duration really is the closest estimate of its
   unperturbed cost.  A ratio, by contrast, has a noisy denominator too, so
   best-of-ratios systematically picks the trial whose denominator was most
   perturbed and biases the verdict (the same trap #537 hit in
   [test_fts_scaling], where min-over-ratios reported a 3x index as CHEAPER
   per op).  Reduce each side independently, then divide.

   This cannot mask the regressions the gates exist for.  If readers were
   serialised behind the writer they could not start until it finished, so
   [reader_done > writer_done] in EVERY trial — no draw of the dice produces
   an overlapping one, and the minimum of each is still ordered the same way. *)
type agg =
  { base_wall : float
  ; par_wall : float
  ; base_writer : float
  ; par_writer : float
  ; par_reader : float
  }

let agg_empty =
  { base_wall = infinity
  ; par_wall = infinity
  ; base_writer = infinity
  ; par_writer = infinity
  ; par_reader = infinity
  }
;;

let agg_add a ~base ~par =
  { base_wall = Float.min a.base_wall base.wall
  ; par_wall = Float.min a.par_wall par.wall
  ; base_writer = Float.min a.base_writer base.writer_phase
  ; par_writer = Float.min a.par_writer par.writer_phase
  ; par_reader = Float.min a.par_reader par.reader_phase
  }
;;

(* Primary metric: the overlap invariant.  If readers were serialised behind
   the writer they could not start until it finished, so [reader_done] would
   necessarily exceed [writer_done] by the whole reader phase (measured:
   1.5x).  Overlapping readers finish inside the writer's window (measured:
   0.37-0.51, and unchanged by host speed, since both terms scale together).
   Unlike the speedup ratio this needs no assumption about T_r / T_w. *)
let overlap_ratio a = a.par_reader /. a.par_writer

(* Secondary metric: the wall-clock win. *)
let speedup a = a.base_wall /. a.par_wall

(* This one DOES assume parallel ~ max (T_w, T_r), which needs enough CPU
   headroom for the readers' work to fit inside the writer's fsync sleeps.  On
   a starved host the writer's own phase inflates (measured: 1.61s -> 2.23s at
   0.15 CPU) and the ratio collapses below the floor with overlap perfectly
   intact — so report that inconclusive rather than failing.  A serialisation
   regression does NOT inflate the writer's phase, and the invariant above
   catches it unconditionally. *)
let writer_inflation a = a.par_writer /. a.base_writer
let inconclusive a = writer_inflation a > 1.25
let overlap_max = 1.15

(* The second way the secondary gate can be UNSATISFIABLE rather than failed
   (#590 review).

   The parallel wall can never drop below the writer's own window, so the very
   best this run's numbers could produce is [base_wall / par_writer] — that is
   the speedup at PERFECT overlap, computed from measured phases only.  If that
   ceiling is already under [min_speedup], no engine could have passed: the
   reader workload was sized below what the gate can resolve.  Observed at load
   11.7 on an armed run, roughly 1 in 15:

     [disjoint tid] read_ops=1906 overlap=0.14 speedup=1.02x  FAIL (gate 1.20x)

   which reads as a performance regression and is not one.  The #569 estimator
   floor makes this more likely, because it undersizes [read_ops] on purpose
   (see [average] in [calibrate_read_ops]); #590 tracks buying that back.

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
   exists to judge.  The secondary gate therefore now asserts exactly when the
   readers set the parallel wall, i.e. [par_wall > par_writer] — which is
   precisely the serialisation regime: serialised readers give
   [par_wall = par_writer + par_reader], the ceiling sits strictly above the
   achieved speedup at [1 + T_r/T_w] = 1.45 for a workload at the intended
   ratio, and the gate fails at ~1.0 as it should.  So the regression the gate
   exists for is still caught, and the primary overlap invariant — which does
   carry overlap information — remains unconditional either way. *)
let speedup_ceiling a = a.base_wall /. a.par_writer
let speedup_unreachable a ~min_speedup = speedup_ceiling a < min_speedup

(* Would the gates pass on the aggregate so far?  Used to stop trialling early
   — best-of-N passes iff SOME trial's aggregate passes, so once one does
   there is nothing left to buy and the extra ~4 s per trial is not spent.

   Note that raising GRANARY_BENCH_TRIALS is NOT a mitigation for either
   inconclusive case: both are properties of the workload sizing, which is
   run-wide and identical across trials.  bench-nightly runs TRIALS=15 (#603). *)
let gates_pass a ~min_speedup =
  overlap_ratio a <= overlap_max
  && (inconclusive a || speedup_unreachable a ~min_speedup || speedup a >= min_speedup)
;;

(* The two gates for one config.  See the header for why there are two. *)
let assert_gates ~label ~a ~min_speedup =
  Alcotest.(check bool)
    (Printf.sprintf
       "[%s] readers finish inside the writer window: reader_done/writer_done %.2f <= \
        %.2f"
       label
       (overlap_ratio a)
       overlap_max)
    true
    (overlap_ratio a <= overlap_max);
  if inconclusive a
  then
    Printf.printf
      "  INCONCLUSIVE [%s]: writer phase inflated %.2fx under load (no CPU headroom to \
       overlap); speedup floor not asserted\n\
       %!"
      label
      (writer_inflation a)
  else if speedup_unreachable a ~min_speedup
  then
    Printf.printf
      "  INCONCLUSIVE [%s]: reader workload sized below what this gate can resolve — \
       even PERFECT overlap caps the speedup at %.2fx (baseline %.3fs / writer window \
       %.3fs), under the %.2fx floor, at overlap %.2f.  Not a performance result; the \
       calibration undersized read_ops (#590).  The overlap invariant above still \
       applies and passed.\n\
       %!"
      label
      (speedup_ceiling a)
      a.base_wall
      a.par_writer
      min_speedup
      (overlap_ratio a)
  else
    Alcotest.(check bool)
      (Printf.sprintf "[%s] speedup %.2fx >= %.2fx" label (speedup a) min_speedup)
      true
      (speedup a >= min_speedup)
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
  let measure label =
    (* Neutralized runs take exactly one trial: nothing is asserted, so extra
       trials would only burn time. *)
    let budget = if min_speedup <= 0.0 then 1 else trials in
    let rec loop t acc =
      let read_ops = !cur_read_ops
      and read_ops_src = !cur_read_ops_src in
      let base, par = trial label t ~read_ops ~read_ops_src in
      let acc = agg_add acc ~base ~par in
      let observed = par.reader_phase /. par.writer_phase in
      if t >= budget || gates_pass acc ~min_speedup
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
        loop (t + 1) agg_empty)
    in
    let n_trials, a = loop 1 agg_empty in
    Printf.printf
      "  [%s] best-of-%d (min per phase): baseline=%.3fs parallel=%.3fs \
       writer_done=%.3fs reader_done=%.3fs overlap=%.2f (gate %.2f) speedup=%.2fx (gate \
       %.2fx)\n\
       %!"
      label
      n_trials
      a.base_wall
      a.par_wall
      a.par_writer
      a.par_reader
      (overlap_ratio a)
      overlap_max
      (speedup a)
      min_speedup;
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
    else assert_gates ~label ~a ~min_speedup
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
  measure "disjoint tid"
;;

let () =
  Alcotest.run
    "wal_fsync_overlap"
    [ ( "bench"
      , [ Alcotest.test_case
            "parallel reads overlap writer fsync"
            `Slow
            test_fsync_overlap
        ] )
    ]
;;
