(** Phase 41 / #162 — slow-read cooperativity smoke bench.

    End-to-end concurrency canary for the Pager + Btree + Cursor stack
    under cold-cache, slow-I/O conditions.  Complements
    [bench_wal_fsync_overlap], which only exercises writer-fsync overlap.

    [bench_wal_fsync_overlap]'s 200-row tree fits entirely in the 64-page
    [Pager] cache after the first walk, so subsequent walks are pure
    cache hits and the [Unix_file] layer is never re-entered.  A
    regression that re-introduces a synchronous blocking code path
    *above* [Unix_file] (in [Pager.read], [Btree.find], or the cursor
    stack) wouldn't trip that bench.

    This bench fixes the gap:

    1. Seed must overflow the cache (default 5000 rows ≈ 60+ leaf
       pages, well over the 64-page boundary) so each cursor walk
       continuously forces real BLOCK [read_page] callbacks.
    2. Inject a per-read [Lwt_unix.sleep] (default 2 ms) in front of the
       BLOCK [read_page] callback to simulate slow / remote storage.
       The reader thus spends ~[seed_pages * n_reads * read_delay] in
       slow I/O, dwarfing the writer's own [n_commits * wal_delay] work.
    3. Run a reader fiber alongside a writer fiber with a fast
       [wal_sync] (1 ms).  Assert the writer's wall time stays bounded
       — proving it makes forward progress concurrent with the reader's
       slow I/O rather than serialising behind it.

    {b Limitation.}  This is *not* a strict regression detector for #158
    (the [Lwt_unix.pread] switch in [Unix_file]).  The injected
    [Lwt_unix.sleep] in the BLOCK callback always yields the scheduler
    regardless of whether the layer below uses [Unix.read] or
    [Lwt_unix.pread] — the sleep masks the yield-vs-noyield distinction.
    For that level of detection, see the [test_unix_file] /
    "cooperation / read_page yields to concurrent timer" unit test.

    What this bench *does* catch: any regression in [Pager], [Btree], or
    the cursor stack that introduces a synchronous lock / mutex / blocking
    call between successive [read_page] yields.  That would starve the
    writer behind the reader's I/O sleeps and blow the wall bound.

    Env vars (all optional):
      SQLOCAML_BENCH_READ_DELAY_MS  per-read injected sleep   (default 2)
      SQLOCAML_BENCH_WAL_DELAY_MS   per-fsync injected sleep  (default 1)
      SQLOCAML_BENCH_N_COMMITS      writer commits            (default 30)
      SQLOCAML_BENCH_N_READS        reader cursor walks       (default 8)
      SQLOCAML_BENCH_SEED_ROWS      initial tree size         (default 5000)
      SQLOCAML_BENCH_MAX_WRITER_S   pass/fail upper bound (s) (default 3.0)
*)

open Lwt.Syntax
module S = Sqlocaml_store.Store
module UF = Sqlocaml_unix.Unix_file

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

(* Open a WAL store whose BLOCK [read_page] callback sleeps [read_delay]
   seconds before delegating to the real [UF.read_page].  This is the
   moment of truth for #158: the [Lwt_unix.sleep] yields the scheduler,
   so a properly-cooperating Pager + Btree + cursor stack will let the
   writer fiber make progress during that stall. *)
let open_slow_store ~path ~read_delay ~wal_delay =
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
    let* () = if read_delay > 0.0 then Lwt_unix.sleep read_delay else Lwt.return_unit in
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
    let* () = if wal_delay > 0.0 then Lwt_unix.sleep wal_delay else Lwt.return_unit in
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
   Disjoint trees so writer CoW doesn't churn reader pages — same
   reasoning as bench_wal_fsync_overlap. *)
let tid_read = 16
let tid_write = 99

let fresh_path () =
  let f = Filename.temp_file "bench_slow_read_yield" ".db" in
  (try Unix.unlink f with
   | _ -> ());
  (try Unix.unlink (f ^ "-wal") with
   | _ -> ());
  f
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

let seed_store ~path ~rows =
  let* st = open_slow_store ~path ~read_delay:0.0 ~wal_delay:0.0 in
  let* tx = S.rw_begin st in
  let rec ins i =
    if i >= rows
    then Lwt.return_unit
    else
      let* () =
        S.put tx tid_read (bs (Printf.sprintf "k%08d" i)) (bs (Printf.sprintf "v%08d" i))
      in
      ins (i + 1)
  in
  let* () = ins 0 in
  let* () = S.commit tx in
  let* () = S.close st in
  Lwt.return_unit
;;

(* Walk every entry under [tid_read] visible to [tx]. *)
let walk_count : type a. a S.txn -> int Lwt.t =
  fun tx ->
  let* cur = S.cursor_open tx tid_read in
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

let main () =
  let read_delay = getenv_float "SQLOCAML_BENCH_READ_DELAY_MS" 2.0 /. 1000.0 in
  let wal_delay = getenv_float "SQLOCAML_BENCH_WAL_DELAY_MS" 1.0 /. 1000.0 in
  let n_commits = getenv_int "SQLOCAML_BENCH_N_COMMITS" 30 in
  let n_reads = getenv_int "SQLOCAML_BENCH_N_READS" 8 in
  let seed_rows = getenv_int "SQLOCAML_BENCH_SEED_ROWS" 5000 in
  let max_writer_s = getenv_float "SQLOCAML_BENCH_MAX_WRITER_S" 3.0 in
  let path = fresh_path () in
  let* () = seed_store ~path ~rows:seed_rows in
  (* Re-open with the slow callbacks engaged.  The Pager cache starts
     cold here — first reference to each page forces a real BLOCK
     [read_page] that sleeps [read_delay] before completing. *)
  let* st = open_slow_store ~path ~read_delay ~wal_delay in
  let writer_start = ref 0.0 in
  let writer_done = ref 0.0 in
  let reader_done = ref 0.0 in
  let t0 = Unix.gettimeofday () in
  let writer () =
    writer_start := Unix.gettimeofday () -. t0;
    let rec loop i =
      if i >= n_commits
      then Lwt.return_unit
      else
        let* tx = S.rw_begin st in
        let* () =
          S.put
            tx
            tid_write
            (bs (Printf.sprintf "w%08d" i))
            (bs (Printf.sprintf "vv%08d" i))
        in
        let* () = S.commit tx in
        loop (i + 1)
    in
    let* () = loop 0 in
    writer_done := Unix.gettimeofday () -. t0;
    Lwt.return_unit
  in
  let reader () =
    let* ro = S.ro_begin st in
    let rec loop i =
      if i >= n_reads
      then Lwt.return_unit
      else
        let* _ = walk_count ro in
        loop (i + 1)
    in
    let* () = loop 0 in
    let* () = S.ro_end ro in
    reader_done := Unix.gettimeofday () -. t0;
    Lwt.return_unit
  in
  let* () = Lwt.join [ writer (); reader () ] in
  let* () = S.close st in
  cleanup path;
  let writer_wall = !writer_done -. !writer_start in
  let reader_wall = !reader_done in
  Printf.printf
    "slow-read yield bench: read_delay=%.0fms wal_delay=%.0fms commits=%d reads=%d \
     seed_rows=%d\n\
     %!"
    (read_delay *. 1000.0)
    (wal_delay *. 1000.0)
    n_commits
    n_reads
    seed_rows;
  Printf.printf "  writer wall: %.3fs (bound: %.3fs)\n" writer_wall max_writer_s;
  Printf.printf "  reader wall: %.3fs\n%!" reader_wall;
  (* Acceptance: writer wall stays bounded.  If a regression in Pager /
     Btree / Cursor blocked the scheduler across the reader's slow I/O,
     the writer would be parked behind the reader's
     [n_reads * pages_per_walk * read_delay] of sleep time and the wall
     would blow the bound.  With cooperative interleaving the writer runs
     concurrently with the reader.
     3.0 s is intentionally generous — pass-case observed wall is
     ~0.6 s standalone but balloons to ~1.4 s when this bench runs
     concurrently with the rest of [dune runtest] (load-induced Lwt
     scheduler jitter affects the writer's many small commits more
     than the reader's few large walks).  A true serialisation
     regression would push the writer wall well past 3 s (it would
     equal reader_wall + writer's own slow-I/O work, both at full
     scale on a 5000-row tree). *)
  Alcotest.(check bool)
    (Printf.sprintf
       "writer wall (%.3fs) bounded by %.3fs (reader yields during slow I/O)"
       writer_wall
       max_writer_s)
    true
    (writer_wall <= max_writer_s);
  Lwt.return_unit
;;

let () =
  Alcotest.run
    "bench_slow_read_yield"
    [ ( "slow-read"
      , [ Alcotest.test_case "writer makes progress during reader I/O" `Slow (fun () ->
            run (main ()))
        ] )
    ]
;;
