(** #532 — does the join-strategy flip a range-bounded build side causes make
    anything slower, on disk?

    A measurement tool, NOT a pass/fail gate (hence [executable], not [test]).

    Giving [build_side] a range bound changes more than what the build side
    reads. [estimate_rows] answers {!Granary_sql.Planner.range_seek_rows} (a flat
    100) rather than [table_rows_estimate] for a seek that carries a range, so R
    shrinks — and [probe_is_worth_it] takes the nested-loop probe iff
    [driving_rows <= right_rows / 8], so a SMALLER R makes the probe HARDER to
    justify. Joins with more than [nlj_min_driving_rows] = 1,000 driving rows and
    a right table of N therefore move from probe to hash join across the whole
    band D <= N/8 (12,500 for N = 100,000).

    Two things make that worth measuring rather than asserting.

    - 100 is a made-up constant with no relation to the span the range covers, so
      a BETWEEN selecting 20,000 rows is estimated at 100 just like one selecting
      21. Where the window is genuinely large, the flip builds a large hash table
      in place of D seeks.
    - It is the same D band [nlj_probe_cost_ratio] was calibrated on in
      #520/#526, where getting the choice wrong measured 36x.

    Both plans are reachable from one binary, with the same foil trick the tests
    use: [si BETWEEN lo AND hi] is recognised, shrinks R and picks the hash join;
    [si + 0 BETWEEN lo AND hi] is not recognised, leaves R at the whole table and
    picks the probe. Identical answers, identical data — only the strategy
    differs.

    Tunables:
      B532_STOCK  stock rows                (default 100000)
      B532_REPS   warm repeats, best-of     (default 3)
    Also honours GRANARY_PAGE_CACHE. *)

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

let n_stock = env_int "B532_STOCK" 100_000
let reps = env_int "B532_REPS" 3
let exec db sql = ignore (unwrap (run (Db.execute db sql)))
let open_at path = unwrap (run (Granary_unix.open_file_wal ~path ()))

let close db =
  try ignore (run (Db.close db)) with
  | _ -> ()
;;

let with_file_path f =
  let dir = Filename.temp_file "b532-" "" in
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

(* One driving table per (driving rows, window) pair, each seeded so that EVERY
   driving row matches one stock row inside its window — otherwise the probe
   gets to skip work the hash join still does, and the comparison stops being
   about the strategy. [stock] is seeded once and shared. *)
let cases =
  [ "W=21     D=1200", "line_a", 1_200, 21
  ; "W=2000   D=1200", "line_b", 1_200, 2_000
  ; "W=20000  D=1200", "line_c", 1_200, 20_000
  ; "W=20000  D=5000", "line_d", 5_000, 20_000
  ; "W=20000  D=12000", "line_e", 12_000, 20_000
  ]
;;

let lo = 20

let seed db =
  exec db "PRAGMA synchronous = off";
  exec db "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
  List.iter
    (fun (_, tbl, _, _) ->
       exec
         db
         (Printf.sprintf
            "CREATE TABLE %s (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))"
            tbl))
    cases;
  exec db "BEGIN";
  for si = 1 to n_stock do
    exec db (Printf.sprintf "INSERT INTO stock VALUES (1, %d, %d)" si (si * 10))
  done;
  List.iter
    (fun (_, tbl, d, w) ->
       for o = 1 to d do
         exec
           db
           (Printf.sprintf "INSERT INTO %s VALUES (1, %d, %d)" tbl o (lo + (o mod w)))
       done)
    cases;
  exec db "COMMIT";
  exec db "PRAGMA wal_checkpoint"
;;

let once db sql =
  let reads = ref 0 in
  Db.set_event_callback
    db
    (Some
       (function
         | Db.Event.Page_read _ | Db.Event.Wal_read _ -> incr reads
         | _ -> ()));
  let t0 = Unix.gettimeofday () in
  let n, st =
    run
      (let open Lwt.Syntax in
       let* stream, stats = Lwt.map unwrap (Db.query_with_stats db sql) in
       let* rows = Lwt_stream.to_list stream in
       Lwt.return (List.length rows, stats))
  in
  let ms = (Unix.gettimeofday () -. t0) *. 1000. in
  Db.set_event_callback db None;
  ms, !reads, st.Db.rows_examined, n
;;

(* Cold reads from a freshly opened handle, then best-of warm wall time. *)
let measure path sql =
  let db = open_at path in
  let cold, reads, examined, rows = once db sql in
  let best = ref cold in
  for _ = 1 to reps do
    let ms, _, _, _ = once db sql in
    if ms < !best then best := ms
  done;
  close db;
  cold, !best, reads, examined, rows
;;

let () =
  Printf.printf
    "#532: join strategy with a range-bounded build side, on disk (%d stock rows, \
     GRANARY_PAGE_CACHE=%s, best of %d)\n"
    n_stock
    (Option.value ~default:"unset (1024)" (Sys.getenv_opt "GRANARY_PAGE_CACHE"))
    reps;
  Printf.printf
    "%-18s | %7s %7s %7s %8s | %7s %7s %7s %8s | %s\n"
    "case"
    "C:reads"
    "cold"
    "warm"
    "exam"
    "F:reads"
    "cold"
    "warm"
    "exam"
    "chosen/foil";
  with_file_path (fun path ->
    let db = open_at path in
    seed db;
    close db;
    List.iter
      (fun (label, tbl, _, w) ->
         let q pred =
           Printf.sprintf
             "SELECT qty FROM %s INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 1 \
              AND %s BETWEEN %d AND %d"
             tbl
             pred
             lo
             (lo + w - 1)
         in
         let hcold, hwarm, hreads, hex, hrows = measure path (q "si") in
         let pcold, pwarm, preads, pex, prows = measure path (q "si + 0") in
         if hrows <> prows
         then
           failwith
             (Printf.sprintf "%s: chosen returned %d rows, foil %d" label hrows prows);
         Printf.printf
           "%-18s | %7d %7.0f %7.0f %8d | %7d %7.0f %7.0f %8d | %5.2fx %5.2fx\n%!"
           label
           hreads
           hcold
           hwarm
           hex
           preads
           pcold
           pwarm
           pex
           (hcold /. pcold)
           (hwarm /. pwarm))
      cases)
;;
