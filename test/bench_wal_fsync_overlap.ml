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

    Reports speedup = baseline / parallel.  Theoretical ceiling for
    perfect overlap with balanced reader/writer durations is 2x.  We
    assert a modest 1.2x floor so the test fails if the overlap
    invariant is broken (e.g. someone reintroduces a writer-lock
    acquire on the reader path) while tolerating environmental jitter.

    Env vars (all optional):
      GRANARY_BENCH_FSYNC_DELAY_MS  injected per-fsync sleep (default 50)
      GRANARY_BENCH_N_COMMITS       writer commits (default 30)
      GRANARY_BENCH_N_READERS       parallel reader fibers (default 4)
      GRANARY_BENCH_READ_OPS        cursor walks per reader (default 100)
      GRANARY_BENCH_SEED_ROWS       initial tree size (default 200)
      GRANARY_BENCH_MIN_SPEEDUP     pass/fail threshold (default 1.2)
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

let test_fsync_overlap () =
  let delay_ms = getenv_int "GRANARY_BENCH_FSYNC_DELAY_MS" 50 in
  let n_commits = getenv_int "GRANARY_BENCH_N_COMMITS" 30 in
  let n_readers = getenv_int "GRANARY_BENCH_N_READERS" 4 in
  let read_ops = getenv_int "GRANARY_BENCH_READ_OPS" 100 in
  let n_seed = getenv_int "GRANARY_BENCH_SEED_ROWS" 200 in
  (* Set conservatively at 1.2.  Observed across 8 runs in the
     granary-dev podman image at default parameters: min 1.38, mean
     1.51, max 1.63.  A regression that reintroduces reader-on-writer-
     lock serialisation would collapse the speedup to ~1.0x (parallel
     ≈ baseline), so 1.2x gives clear separation while tolerating
     jitter. *)
  let min_speedup = getenv_float "GRANARY_BENCH_MIN_SPEEDUP" 1.2 in
  let delay = float_of_int delay_ms /. 1000.0 in
  (* Run baseline vs parallel under the currently-configured tid pair and
     assert the overlap win clears the floor.  Called once per config. *)
  let measure label =
    let base = run_config ~delay ~n_seed `Baseline ~n_commits ~n_readers ~read_ops in
    let par = run_config ~delay ~n_seed `Parallel ~n_commits ~n_readers ~read_ops in
    let speedup = base.wall /. par.wall in
    Printf.printf
      "fsync-overlap bench [%s]: delay=%dms commits=%d readers=%d read_ops=%d seed=%d\n\
      \  baseline=%.3fs (writer=%.3fs, readers=%.3fs) parallel=%.3fs (writer_done=%.3fs \
       reader_done=%.3fs) speedup=%.2fx (min %.2fx)\n\
       %!"
      label
      delay_ms
      n_commits
      n_readers
      read_ops
      n_seed
      base.wall
      base.writer_phase
      base.reader_phase
      par.wall
      par.writer_phase
      par.reader_phase
      speedup
      min_speedup;
    cleanup path;
    Alcotest.(check bool)
      (Printf.sprintf "[%s] speedup %.2fx >= %.2fx" label speedup min_speedup)
      true
      (speedup >= min_speedup)
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
