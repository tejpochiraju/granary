(** #562 — where do the pager resolutions on an index-driven access path
    actually go?

    A measurement tool, not a gate.  Derived from the #546 harness
    ([test/bench_build_side_seek_546.ml]) but cut down to the question #562
    asks: of the pager-level page resolutions an on-disk query performs, how
    many are served from the WAL overlay ([Wal_read]) rather than the main file
    ([Page_read]), and — the part the [Wal_read] counter alone cannot say — how
    many of those actually touch the device.

    {b The answer, measured 2026-08-02 (5,000 rows, before the fix):} a point
    lookup costs exactly 3.00 pager resolutions, every one of them a
    [Wal_read], and — measured with a temporary hit/miss counter inside
    [Wal.read_frame] — every one of those 600 resolutions over 200 lookups was
    a hit in the WAL's own decrypted frame cache (#246). Zero device reads.
    So the 3 is the {b B-tree descent depth}, not WAL re-resolution: root,
    interior, leaf, re-resolved per lookup because the pager cache does not
    participate. Serving them from the pager cache instead would replace one
    hashtable lookup with another and save no I/O — which is why #562 did NOT
    add a pager-level cache over WAL-resolved frames.

    What it {e did} find is that the [checkpointed] and [not checkpointed]
    rows were IDENTICAL before the fix, down to the WAL file size:
    [Wal.reset] left the whole generation on disk under an unchanged header, so
    recovery replayed it and the reopen undid the checkpoint.

    {b Before/after, both on the quiet 12-core build host (engine-2), 20,000
    rows, default 1024-page pager cache:}

    {v
                                    before                 after
      checkpointed / open           6.1 ms                 0.5 ms
      checkpointed / cold scan      584 wal_read           291 page_read
      checkpointed / warm scan      584 wal_read           0 resolutions
      checkpointed / 200 lookups    600 wal_read (3.00)    0 (0.00)
      NOT checkpointed              unchanged — 584 wal_read, 3.00/lookup
    v}

    The cold-scan row is the one to read twice: 584 -> 291 is not fewer pages,
    it is the same pages stopping being re-resolved, because the pager cache
    now holds them. That is the whole of the pager-cache inertness #562
    observed, and it comes free with the correctness fix.

    The un-checkpointed row was deliberately unchanged by #562: inside a live
    generation the pager cache still did not participate. Measured cost was
    CPU only (the WAL frame cache absorbs the I/O), which is why it was a
    follow-up rather than part of that change.

    {b #611 closed that follow-up}, so the un-checkpointed row is no longer a
    control: WAL-resolved pages are now cached under [(page_id, frame_idx)]
    and the un-checkpointed warm scan and repeated point lookups should
    resolve nothing. The numbers above are pre-#611 and are kept as the
    historical baseline this harness was written to produce — re-run it to see
    the current ones. Correctness of the caching (the three invalidation
    triggers) is pinned by [test_pager_wal_cache_611.ml] and
    [test_wal_cache_e2e_611.ml]; this file remains a measurement tool, not a
    gate.

    Tunables: B562_ROWS (default 20000), B562_REPS (default 3). *)

open Granary

let run = Lwt_main.run

let unwrap = function
  | Ok v -> v
  | Error e -> failwith (Format.asprintf "granary: %a" Db.pp_error e)
;;

let env_int key default =
  match Sys.getenv_opt key with
  | Some s ->
    (try int_of_string s with
     | _ -> default)
  | None -> default
;;

let n_rows = env_int "B562_ROWS" 20_000
let reps = env_int "B562_REPS" 3
let exec db sql = ignore (unwrap (run (Db.execute db sql)))
let open_at path = unwrap (run (Granary_unix.open_file_wal ~path ()))

let close db =
  try ignore (run (Db.close db)) with
  | _ -> ()
;;

let with_file_path f =
  let dir = Filename.temp_file "b562-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "db" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun sfx ->
           try Sys.remove (path ^ sfx) with
           | _ -> ())
        [ ""; "-wal" ];
      try Unix.rmdir dir with
      | _ -> ())
    (fun () -> f path)
;;

(* One drained execution with every counter collected. *)
let once db sql =
  let page = ref 0
  and wal = ref 0 in
  Db.set_event_callback
    db
    (Some
       (function
         | Db.Event.Page_read _ -> incr page
         | Db.Event.Wal_read _ -> incr wal
         | _ -> ()));
  let t0 = Unix.gettimeofday () in
  let n =
    run
      (let open Lwt.Syntax in
       let* stream = Lwt.map unwrap (Db.query db sql) in
       let* rows = Lwt_stream.to_list stream in
       Lwt.return (List.length rows))
  in
  let ms = (Unix.gettimeofday () -. t0) *. 1000. in
  Db.set_event_callback db None;
  ms, !page, !wal, n
;;

let row label (ms, page, wal, rows) =
  Printf.printf
    "  %-26s %8.1f ms  page_read=%-7d wal_read=%-7d rows=%d\n%!"
    label
    ms
    page
    wal
    rows
;;

let seed db =
  exec db "PRAGMA synchronous = off";
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b INTEGER, c TEXT)";
  exec db "CREATE INDEX t_b ON t (b)";
  exec db "BEGIN";
  for i = 1 to n_rows do
    exec
      db
      (Printf.sprintf "INSERT INTO t VALUES (%d, %d, 'pad-%d')" i (i * 7 mod n_rows) i)
  done;
  exec db "COMMIT"
;;

(* A point-lookup loop: [reps] descents of the same tree, so every resolution
   after the first is a RE-resolution of a page the pager could have cached. *)
let point_lookups db =
  let page = ref 0
  and wal = ref 0 in
  Db.set_event_callback
    db
    (Some
       (function
         | Db.Event.Page_read _ -> incr page
         | Db.Event.Wal_read _ -> incr wal
         | _ -> ()));
  let t0 = Unix.gettimeofday () in
  let n = 200 in
  for i = 1 to n do
    let sql = Printf.sprintf "SELECT c FROM t WHERE a = %d" ((i * 97 mod n_rows) + 1) in
    ignore
      (run
         (let open Lwt.Syntax in
          let* stream = Lwt.map unwrap (Db.query db sql) in
          Lwt_stream.to_list stream))
  done;
  let ms = (Unix.gettimeofday () -. t0) *. 1000. in
  Db.set_event_callback db None;
  Printf.printf
    "  %-26s %8.1f ms  page_read=%-7d wal_read=%-7d (%d lookups, %.2f resolutions/lookup)\n\
     %!"
    "200 point lookups"
    ms
    !page
    !wal
    n
    (float_of_int (!page + !wal) /. float_of_int n)
;;

let scenario ~label ~checkpoint =
  with_file_path (fun path ->
    let db = open_at path in
    seed db;
    if checkpoint then exec db "PRAGMA wal_checkpoint";
    close db;
    Printf.printf
      "%s (db=%d bytes, wal=%d bytes)\n%!"
      label
      (try (Unix.stat path).Unix.st_size with
       | _ -> -1)
      (try (Unix.stat (path ^ "-wal")).Unix.st_size with
       | _ -> -1);
    let t0 = Unix.gettimeofday () in
    let db = open_at path in
    Printf.printf
      "  %-26s %8.1f ms  (WAL recovery scan)\n%!"
      "open"
      ((Unix.gettimeofday () -. t0) *. 1000.);
    row "cold full scan" (once db "SELECT count(*) FROM t");
    for _ = 1 to reps do
      row "warm full scan" (once db "SELECT count(*) FROM t")
    done;
    point_lookups db;
    point_lookups db;
    close db)
;;

let () =
  Printf.printf "#562: pager resolutions on disk (%d rows)\n%!" n_rows;
  scenario ~label:"NOT checkpointed" ~checkpoint:false;
  scenario ~label:"checkpointed before close" ~checkpoint:true
;;
