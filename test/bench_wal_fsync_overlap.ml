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
    writer phase is ~1.5 s by construction — so the fix is the other half of
    that treatment: a BEST-OF-N over whole trials.  See [measure].

    {b Where these gates actually run armed.}  Every automated job that
    invokes [dune runtest] sets [GRANARY_BENCH_MIN_SPEEDUP=0], which
    neutralizes both gates (see the [min_speedup <= 0.0] branch in [measure]).
    The one job that runs them ARMED is the nightly
    [.forgejo/workflows/bench-nightly.yml] (mirrored to [.github/]), which
    deliberately sets no [GRANARY_BENCH_*] neutralizer and runs [dune runtest
    -j 1] on an otherwise-idle self-hosted runner.  It reports (files/updates
    an issue) rather than blocking PRs.  Outside that nightly this gate is
    armed only for a developer running the suite locally (#549).

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

(* Time the baseline reader phase alone — [n_readers] fibers joined, no
   writer, no fsync delay.  Used by [calibrate_read_ops]. *)
let probe_reader_phase ~n_seed ~n_readers ~read_ops =
  cleanup path;
  run
    (let* st = open_slow_wal ~path ~delay:0.0 in
     let* () = seed st n_seed in
     let t0 = Unix.gettimeofday () in
     let readers = List.init n_readers (fun _ -> reader_workload st ~read_ops) in
     let* () = Lwt.join readers in
     let t1 = Unix.gettimeofday () in
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

   Err LOW, deliberately.  Two effects punish T_r near T_w:
   - the metric is not monotonic in T_r.  Past T_r = T_w the expression
     becomes 1 + T_w / T_r and the speedup falls again; and
   - readers are slower under contention than the probes (which run with no
     writer on an idle scheduler), so actual T_r overshoots target by ~1.5x
     and, once readers set the parallel wall, that inflation lands directly
     on the denominator.  A cold run measured 1.27x this way.
   Hence the low default ratio and the hard writer-relative cap below: T_r is
   kept clear of T_w so the writer always bounds the parallel wall. *)
let calibrate_read_ops ~n_seed ~n_readers ~writer_secs ~ratio =
  (* Timing noise is one-sided — a probe can be delayed, never hurried — so
     take the fastest of [probe_reps] repeats rather than a single shot.
     Single-shot probes drifted read_ops by up to 1.5x run to run, which
     showed up directly as speedup spread (1.25x on the unlucky draw). *)
  let probe_reps = 3 in
  let best ~read_ops =
    let times =
      List.init probe_reps (fun _ -> probe_reader_phase ~n_seed ~n_readers ~read_ops)
    in
    List.fold_left min infinity times
  in
  let t1 = best ~read_ops:probe_ops in
  let t2 = best ~read_ops:(2 * probe_ops) in
  let marginal = (t2 -. t1) /. float_of_int probe_ops in
  (* Fall back to the average cost if the two probes were swamped by jitter. *)
  let per_op = if marginal > 0.0 then marginal else t2 /. float_of_int (2 * probe_ops) in
  (* Unreachable unless the clock runs backwards — [t2] is a positive
     duration — so this branch is defence only and will not be covered. *)
  if per_op <= 0.0
  then probe_ops
  else (
    let ops_for secs = int_of_float (secs /. per_op) in
    let want = ops_for (writer_secs *. ratio) in
    (* Upper clamps: predicted T_r stays at most 0.75 x T_w (see above), and
       200k ops keeps a pathologically slow host from running for minutes.
       Lower clamp is 1, not [probe_ops]: a probe size is not a workload
       floor, and forcing one on a slow host recreates #468 with the
       inequality flipped. *)
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

(* Would the gates pass on the aggregate so far?  Used to stop trialling early
   — best-of-N passes iff SOME trial's aggregate passes, so once one does
   there is nothing left to buy and the extra ~4 s per trial is not spent. *)
let gates_pass a ~min_speedup =
  overlap_ratio a <= overlap_max && (inconclusive a || speedup a >= min_speedup)
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
  (* Target reader phase as a fraction of the writer phase.  Contention makes
     the real T_r overshoot this somewhat, so 0.45 lands the effective ratio
     near 0.5 and the expected speedup near 1.5x — well over the 1.2x floor
     while keeping T_r clear of T_w (see [calibrate_read_ops]).  A
     non-positive or unparseable value means "unset". *)
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
      calibrate_read_ops ~n_seed ~n_readers ~writer_secs ~ratio:reader_ratio, "calibrated"
  in
  (* Run baseline vs parallel under the currently-configured tid pair and
     assert the overlap win clears the floor.  Called once per config.  Both
     configs deliberately share one calibration: per-op cost differs between
     them (shared-tid pays writer CoW eviction), and holding read_ops fixed
     is what makes the two speedups comparable. *)
  let trial label t =
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
  (* Best-of-N over whole trials (#538).  Both gates are wall-clock ratios and
     both were single-shot; on a co-scheduled box a reader fiber that loses the
     CPU for one scheduling quantum lands directly on the verdict.  The loop
     stops as soon as the aggregate passes, so an unloaded host still pays for
     exactly one trial (~4 s per config) and only a noisy one pays more. *)
  let measure label =
    (* Neutralized runs take exactly one trial: nothing is asserted, so extra
       trials would only burn time. *)
    let budget = if min_speedup <= 0.0 then 1 else trials in
    let rec loop t acc =
      let base, par = trial label t in
      let acc = agg_add acc ~base ~par in
      if t >= budget || gates_pass acc ~min_speedup then t, acc else loop (t + 1) acc
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
