(** #481 — scan-path cost probe: attributes the [scan_agg] gap vs C SQLite.

    A measurement tool, NOT a pass/fail gate (hence [executable], not [test] —
    seeding is far too slow for [dune test]).  It exists so the #481 attribution
    is reproducible on any host rather than a one-off number in an issue.

    The lever that makes attribution possible: [SELECT COUNT-star] sets
    [need_decode = false] in the #247 aggregate fast path, so it drives the
    storage cursor with *zero* row decoding.  Differencing it against
    [COUNT-star, SUM(k)] therefore splits total scan cost into

      cursor cost        = the COUNT-star number
      row-decode cost    = the difference

    without needing a profiler.  On the #222 reference config (100k rows,
    [page_cache=1024]) the split measured ~400 ns/row cursor vs ~177 ns/row
    decode — i.e. the cursor, not [Row.decode_prefix], dominates.

    [Gc.minor_words] deltas are reported alongside because the cursor's cost is
    largely allocation: ~169 words/row for a [COUNT-star] that reads no columns.
    Note that blocks larger than 256 words (e.g. a 4 KB page copy) bypass the
    minor heap and so do NOT appear in [words/row] — a rising [minorGC] count
    against flat [words/row] is the signature of major-heap traffic.

    Tunables (all optional):
      PROBE_ROWS     rows seeded                (default 100000)
      PROBE_REPS     timed repeats, best-of     (default 20)
      PROBE_PAYLOAD  TEXT payload width, bytes  (default 12)

    Also honours the engine's own [GRANARY_PAGE_CACHE] (pager capacity, in
    4 KB pages) and [GRANARY_AGG_FASTPATH] (set to 0 to disable the #247 path).

    Useful sweeps:
      - vary [GRANARY_PAGE_CACHE] at fixed rows to find the cache knee;
      - vary [PROBE_PAYLOAD] with a large cache to expose per-row copying of
        columns the query never reads. *)

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

let rows_n = env_int "PROBE_ROWS" 100_000
let reps_n = env_int "PROBE_REPS" 20
let payload = String.make (env_int "PROBE_PAYLOAD" 12) 'p'
let exec db sql = ignore (unwrap (run (Db.execute db sql)))

(* One full scan, driven to completion so the aggregate actually folds every
   row rather than leaving the stream suspended. *)
let scan_once db sql =
  run
    (let open Lwt.Syntax in
     let* stream = Lwt.map unwrap (Db.query db sql) in
     let* rows = Lwt_stream.to_list stream in
     Lwt.return (List.length rows))
;;

(* Best-of-[reps_n] wall time, plus allocation from one separately measured
   run.  Best-of (not mean) matches the #222 harness and suppresses scheduler
   noise; allocation is deterministic enough that a single run suffices. *)
let measure db sql =
  ignore (scan_once db sql);
  let w0 = Gc.minor_words () in
  let g0 = Gc.quick_stat () in
  ignore (scan_once db sql);
  let words = Gc.minor_words () -. w0 in
  let minor_gcs = (Gc.quick_stat ()).Gc.minor_collections - g0.Gc.minor_collections in
  let best = ref infinity in
  for _ = 1 to reps_n do
    let t0 = Unix.gettimeofday () in
    ignore (scan_once db sql);
    let dt = Unix.gettimeofday () -. t0 in
    if dt < !best then best := dt
  done;
  !best, words, minor_gcs
;;

let seed db =
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, k INTEGER, payload TEXT)";
  exec db "BEGIN";
  for i = 0 to rows_n - 1 do
    exec
      db
      (Printf.sprintf
         "INSERT INTO t (id, k, payload) VALUES (%d, %d, '%s-%d')"
         i
         (i * 7 mod rows_n)
         payload
         i)
  done;
  exec db "COMMIT"
;;

(* [decode<=N]: the highest column ordinal the aggregate forces the fast path to
   decode.  COUNT-star touches no column at all, so it decodes nothing. *)
let queries =
  [ "COUNT(*)             [no decode]", "SELECT COUNT(*) FROM t"
  ; "SUM(id)              [decode<=0]", "SELECT SUM(id) FROM t"
  ; "COUNT(*), SUM(k)     [decode<=1]", "SELECT COUNT(*), SUM(k) FROM t"
  ]
;;

let () =
  (* The store's open path derives nonces from the RNG and fails closed
     without one, even for a plaintext database. *)
  Mirage_crypto_rng_unix.use_default ();
  let dir = Filename.temp_file "scanprobe-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "probe.db" in
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
    let db =
      unwrap (run (Granary_unix.open_file_wal ~clock:Unix.gettimeofday ~path ()))
    in
    seed db;
    Printf.printf
      "rows=%d reps=%d payload=%dB page_cache=%s agg_fastpath=%s\n"
      rows_n
      reps_n
      (String.length payload)
      (Option.value (Sys.getenv_opt "GRANARY_PAGE_CACHE") ~default:"default")
      (match Sys.getenv_opt "GRANARY_AGG_FASTPATH" with
       | Some ("0" | "false" | "off") -> "off"
       | _ -> "on");
    Printf.printf
      "%-34s %9s %8s %11s %8s\n"
      "query"
      "ms/scan"
      "ns/row"
      "words/row"
      "minorGC";
    List.iter
      (fun (label, sql) ->
         let secs, words, gcs = measure db sql in
         Printf.printf
           "%-34s %9.2f %8.0f %11.1f %8d\n%!"
           label
           (secs *. 1000.)
           (secs /. float_of_int rows_n *. 1e9)
           (words /. float_of_int rows_n)
           gcs)
      queries)
;;
