(** #576 tier 3 — is a nested-loop probe's per-driving-row cost the same order
    as a hash join's per-row cost when its build side SEEKS (rather than
    scans), on disk?

    A measurement tool, NOT a pass/fail gate (hence [executable], not [test]).

    {!Granary_sql.Planner.nlj_probe_cost_ratio} = 8 was calibrated in #520
    against a build side that is SCANNED, at ~9.5 us/hashed row. #546/#606
    later measured a build-side SEEK at ~3.03 pager reads/row (100,000-row
    table) — the same order as a probe, which is also one B-tree descent per
    row. #576 asks whether that intuition is right, directly, rather than by
    inference from two other benches measured for different reasons.

    This isolates the two costs so they can be compared without the planner's
    own cost model choosing between them:

      - {b probe cost/row}: [line INNER JOIN stock ON si = i_id], driving rows
        held at or under {!Granary_sql.Planner.nlj_min_driving_rows} = 1000, so
        the floor guarantees a probe regardless of the ratio. D is varied and
        [B:reads] regressed against it gives reads/driving-row.
      - {b hash-with-seeked-build cost/row}: [line INNER JOIN stock ON k =
        i_id] — [k] is an extra stock column equal to [si] but UNINDEXED, so
        {!Granary_sql.Planner.best_probe} can never find a probe and the join
        is unconditionally a hash join, whatever D is. [stock]'s build side
        still narrows on [WHERE sw = 1 AND si BETWEEN lo AND hi], which is
        exactly the #575/#586 unambiguous-seek shape ([sw, si] is the PK, the
        window is bounded by literal integers). D is held fixed and the window
        R is varied; [H:reads] regressed against R gives reads/build-row.

    Both queries return the same rows for the same (D, R) — [i_id] and [k] hold
    the same values — so this is a clean A/B of the operation cost, not of the
    answer.

    Tunables:
      B576_STOCK   stock rows                     (default 100000)
      B576_REPS    warm repeats, best-of          (default 3)
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

let n_stock = env_int "B576_STOCK" 100_000
let reps = env_int "B576_REPS" 3
let exec db sql = ignore (unwrap (run (Db.execute db sql)))
let open_at path = unwrap (run (Granary_unix.open_file_wal ~path ()))

let close db =
  try ignore (run (Db.close db)) with
  | _ -> ()
;;

let with_file_path f =
  let dir = Filename.temp_file "b576-" "" in
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

(* [n_stock] rows, [k] a second copy of [si] with no index of its own so a
   join on [k] can never find a probe. One driving table per D, seeded so
   every row lands inside [lo, lo+w-1] and matches exactly one stock row. *)
let lo = 20
let d_values_probe = [ 100; 300; 600; 1000 ]
let r_values_hash = [ 50; 150; 300; 450 ]

(* #575/#586: the seek stays unambiguous only within
   [table_rows_estimate / build_side_seek_break_even_ratio] = 100,000 / 200 =
   500, so every [r_values_hash] window is comfortably inside that budget. *)
let d_fixed_for_hash = 2000

let seed db =
  exec db "PRAGMA synchronous = off";
  exec
    db
    "CREATE TABLE stock (sw INTEGER, si INTEGER, k INTEGER, qty INTEGER, PRIMARY KEY \
     (sw, si))";
  List.iter
    (fun d ->
       exec
         db
         (Printf.sprintf
            "CREATE TABLE line_p_%d (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, \
             o))"
            d))
    d_values_probe;
  exec db "CREATE TABLE line_h (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
  exec db "BEGIN";
  for si = 1 to n_stock do
    exec db (Printf.sprintf "INSERT INTO stock VALUES (1, %d, %d, %d)" si si (si * 10))
  done;
  (* Each probe driving row targets a DISTINCT stock key, {b scattered across
     the whole [n_stock]-row table} rather than a contiguous window — [scatter]
     is coprime with [n_stock] (both prime), so [o] in [1, d] visits [d]
     distinct keys with no small-range locality. A contiguous window (adjacent
     keys) lets the pager cache absorb every probe past the first few, however
     small the cache, because so few leaf pages cover the whole window; this is
     the #520/#526 TPC-C shape, where [i_id] has no relation to insertion
     order. *)
  let scatter = 99_991 in
  List.iter
    (fun d ->
       for o = 1 to d do
         let key = (o * scatter mod n_stock) + 1 in
         exec db (Printf.sprintf "INSERT INTO line_p_%d VALUES (1, %d, %d)" d o key)
       done)
    d_values_probe;
  for o = 1 to d_fixed_for_hash do
    let w = List.nth r_values_hash (List.length r_values_hash - 1) in
    exec db (Printf.sprintf "INSERT INTO line_h VALUES (1, %d, %d)" o (lo + (o mod w)))
  done;
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
    "#576: probe cost/row vs hash-with-seeked-build cost/row, on disk (%d stock rows, \
     GRANARY_PAGE_CACHE=%s, best of %d)\n"
    n_stock
    (Option.value ~default:"unset (1024)" (Sys.getenv_opt "GRANARY_PAGE_CACHE"))
    reps;
  with_file_path (fun path ->
    let db = open_at path in
    seed db;
    close db;
    Printf.printf "\n-- probe: D varies, targets scattered across the table --\n";
    Printf.printf "%8s | %8s %8s %8s\n" "D" "reads" "cold" "warm";
    let probe_pts =
      List.map
        (fun d ->
           let sql =
             Printf.sprintf
               "SELECT qty FROM line_p_%d INNER JOIN stock ON si = i_id WHERE w = 1 AND \
                sw = 1"
               d
           in
           let cold, warm, reads, _, _ = measure path sql in
           Printf.printf "%8d | %8d %8.2f %8.2f\n%!" d reads cold warm;
           d, reads)
        d_values_probe
    in
    Printf.printf
      "\n-- hash (unindexed join col, build side seeks): R varies, D fixed at %d --\n"
      d_fixed_for_hash;
    Printf.printf "%8s | %8s %8s %8s\n" "R" "reads" "cold" "warm";
    let hash_pts =
      List.map
        (fun w ->
           let sql =
             Printf.sprintf
               "SELECT qty FROM line_h INNER JOIN stock ON k = i_id WHERE w = 1 AND sw = \
                1 AND si BETWEEN %d AND %d"
               lo
               (lo + w - 1)
           in
           let cold, warm, reads, _, _ = measure path sql in
           Printf.printf "%8d | %8d %8.2f %8.2f\n%!" w reads cold warm;
           w, reads)
        r_values_hash
    in
    (* Least-squares slope through the origin-shifted points: reads-per-unit
       from the first to the last sample, which is what the doc comments above
       quote for the two sibling benches. *)
    let slope pts =
      let d0, r0 = List.hd pts in
      let dn, rn = List.nth pts (List.length pts - 1) in
      float_of_int (rn - r0) /. float_of_int (dn - d0)
    in
    let probe_slope = slope probe_pts in
    let hash_slope = slope hash_pts in
    Printf.printf "\nprobe reads/driving-row : %6.3f\n" probe_slope;
    Printf.printf "hash  reads/build-row    : %6.3f\n" hash_slope;
    Printf.printf "ratio (probe / hash)     : %6.3f\n" (probe_slope /. hash_slope))
;;
