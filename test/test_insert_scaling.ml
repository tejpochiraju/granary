(** #228 / #229 regression: point lookups and bulk inserts must not be O(n) /
    O(n^2).

    Root cause (pre-fix): [Store.cursor_open] drained the entire B+-tree into a
    list and [cursor_seek] linear-scanned it, so every index lookup and every
    per-insert UNIQUE pre-check was O(n).  That made [WHERE pk = ?] a full table
    scan (#228) and an n-row bulk insert O(n^2) (#229).  The fix routes those
    probes through [Store.seek_ge] (native O(log n) descent + lazy streaming).

    These assertions are deliberately machine-independent: they compare the
    cost of a later work chunk against an earlier one as a ratio, so a slow or
    fast host does not change the verdict.  A genuinely O(n^2) insert makes the
    second half-chunk cost ~3x the first; the fix keeps it well under 2x.

    The ratios are still wall-clock measurements, so on a shared, heavily loaded
    CI runner (these [`Slow] tests run concurrently with ~90 other suites and
    the IO-hammering [bench_*] jobs) GC/scheduler noise can exceed the headroom
    and flake.  As with the [bench_*] suites and [test_fts_scaling], CI
    neutralizes the gates via the same env vars — [GRANARY_BENCH_MAX_RATIO]
    (raised high) for the insert ceiling and [GRANARY_BENCH_MIN_SPEEDUP]
    (lowered to 0) for the lookup floor — so the suite still runs and prints
    without failing on load. *)

module Db = Granary.Db

let run = Lwt_main.run

(* Insert second/first ratio ceiling.  Default 2.5 (generous headroom over the
   ~2.0 a correct O(log n) insert shows); raised via [GRANARY_BENCH_MAX_RATIO]
   to neutralize the gate on loaded CI. *)
let max_ratio =
  match Sys.getenv_opt "GRANARY_BENCH_MAX_RATIO" with
  | Some v ->
    (try float_of_string v with
     | _ -> 2.5)
  | None -> 2.5
;;

(* Point-lookup vs full-scan speedup floor.  Default 4.0 (the real ratio is
   ~15-50x); lowered to 0 via [GRANARY_BENCH_MIN_SPEEDUP] to neutralize the
   gate on loaded CI. *)
let min_speedup =
  match Sys.getenv_opt "GRANARY_BENCH_MIN_SPEEDUP" with
  | Some v ->
    (try float_of_string v with
     | _ -> 4.0)
  | None -> 4.0
;;

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "db error: %a" Db.pp_error e
;;

let with_db f =
  let dir = Filename.temp_file "granary_scaling" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "scaling.db" in
  let db = unwrap (run (Granary_unix.open_file_wal ~path ())) in
  Fun.protect
    ~finally:(fun () ->
      (try run (Db.close db) with
       | _ -> ());
      List.iter
        (fun s ->
           try Sys.remove (Filename.concat dir s) with
           | _ -> ())
        [ "scaling.db"; "scaling.db-wal" ];
      try Unix.rmdir dir with
      | _ -> ())
    (fun () -> f db)
;;

let now () = Unix.gettimeofday ()

let exec_lwt db sql =
  let open Lwt.Syntax in
  let* r = Db.execute db sql in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

(* Insert [total] rows in one transaction (used to populate before lookups). *)
let bulk_insert db ~total =
  run
    (let open Lwt.Syntax in
     let* () = exec_lwt db "BEGIN" in
     let* stmt =
       Lwt.map unwrap (Db.prepare db "INSERT INTO t (id, k, payload) VALUES (?, ?, ?)")
     in
     let rec loop i =
       if i >= total
       then Lwt.return_unit
       else
         let* _ =
           Lwt.map
             unwrap
             (Db.run
                stmt
                ~params:
                  [ Db.V_int (Int64.of_int i)
                  ; Db.V_int (Int64.of_int (i mod 97))
                  ; Db.V_text (Printf.sprintf "payload-row-%d" i)
                  ])
         in
         loop (i + 1)
     in
     let* () = loop 0 in
     let* () = Db.finalize stmt in
     exec_lwt db "COMMIT")
;;

(* Time inserting rows [0..half) then [half..total) as two separate timed loops
   inside ONE transaction; compare the halves' costs. *)
let measure_halves db ~total =
  let half = total / 2 in
  let res = ref (0.0, 0.0) in
  run
    (let open Lwt.Syntax in
     let* () = exec_lwt db "BEGIN" in
     let* stmt =
       Lwt.map unwrap (Db.prepare db "INSERT INTO t (id, k, payload) VALUES (?, ?, ?)")
     in
     let do_range lo hi =
       let rec loop i =
         if i >= hi
         then Lwt.return_unit
         else
           let* _ =
             Lwt.map
               unwrap
               (Db.run
                  stmt
                  ~params:
                    [ Db.V_int (Int64.of_int i)
                    ; Db.V_int (Int64.of_int (i mod 97))
                    ; Db.V_text (Printf.sprintf "payload-row-%d" i)
                    ])
           in
           loop (i + 1)
       in
       loop lo
     in
     let t0 = now () in
     let* () = do_range 0 half in
     let t1 = now () in
     let* () = do_range half total in
     let t2 = now () in
     res := t1 -. t0, t2 -. t1;
     let* () = Db.finalize stmt in
     exec_lwt db "COMMIT");
  !res
;;

let test_insert_not_quadratic () =
  with_db (fun db ->
    run (exec_lwt db "CREATE TABLE t (id INTEGER PRIMARY KEY, k INTEGER, payload TEXT)");
    let total = 6000 in
    let first, second = measure_halves db ~total in
    Printf.eprintf
      "SCALING: insert first-half=%.3fs second-half=%.3fs ratio=%.2f\n%!"
      first
      second
      (second /. first);
    (* O(n^2) would give a ratio ~3.0 (second half walks a table 1.5x larger on
       average and is twice as far along).  The O(log n) fix keeps it well under
       2.0; we assert < [max_ratio] (default 2.5) for generous headroom against
       GC/scheduler noise, neutralized on loaded CI via GRANARY_BENCH_MAX_RATIO. *)
    Alcotest.(check bool)
      (Printf.sprintf "insert second/first ratio %.2f < %.1f" (second /. first) max_ratio)
      true
      (second /. first < max_ratio))
;;

(* Time [reps] executions of [sql] (each drained to completion); return the
   mean wall-clock per execution in seconds. *)
let time_query db sql ~reps =
  let t0 = now () in
  run
    (let open Lwt.Syntax in
     let rec loop i =
       if i >= reps
       then Lwt.return_unit
       else
         let* s = Lwt.map unwrap (Db.query db sql) in
         let* _ = Lwt_stream.to_list s in
         loop (i + 1)
     in
     loop 0);
  (now () -. t0) /. float_of_int reps
;;

let test_point_lookup_fast () =
  with_db (fun db ->
    run (exec_lwt db "CREATE TABLE t (id INTEGER PRIMARY KEY, k INTEGER, payload TEXT)");
    let total = 6000 in
    bulk_insert db ~total;
    (* Correctness: a parameterised point lookup (the prepared-statement path
       #228 fixed) returns each probed row's exact payload. *)
    let m = 300 in
    let t0 = now () in
    run
      (let open Lwt.Syntax in
       let* stmt = Lwt.map unwrap (Db.prepare db "SELECT payload FROM t WHERE id = ?") in
       let rec loop i =
         if i >= m
         then Lwt.return_unit
         else (
           let k = i * 2654435761 mod total in
           let* s = Lwt.map unwrap (Db.iter stmt ~params:[ Db.V_int (Int64.of_int k) ]) in
           let* rows = Lwt_stream.to_list s in
           (match rows with
            | [ [| Db.V_text p |] ] ->
              Alcotest.(check string)
                "payload matches"
                (Printf.sprintf "payload-row-%d" k)
                p
            | _ -> Alcotest.failf "lookup id=%d returned %d rows" k (List.length rows));
           loop (i + 1))
       in
       let* () = loop 0 in
       Db.finalize stmt);
    let lookup_s = (now () -. t0) /. float_of_int m in
    (* Speed, machine-independently (#228: "point-lookup time << full-scan
       time"): compare the lookup against a genuine full-table aggregate scan of
       the SAME table.  An O(log n) seek touches a handful of pages; an O(n)
       scan touches all 6000 rows.  If the lookup regressed to a scan the ratio
       collapses to ~1.  Require the seek to be at least [min_speedup]x cheaper
       (default 4.0) — the real ratio is ~15-50x, so this is a wide margin with
       no absolute threshold; neutralized on loaded CI via
       GRANARY_BENCH_MIN_SPEEDUP. *)
    let scan_s = time_query db "SELECT COUNT(*), SUM(k) FROM t" ~reps:20 in
    let ratio = scan_s /. lookup_s in
    Printf.eprintf
      "SCALING: point lookup %.3f ms/op vs full scan %.3f ms/op = %.1fx cheaper\n%!"
      (lookup_s *. 1000.)
      (scan_s *. 1000.)
      ratio;
    Alcotest.(check bool)
      (Printf.sprintf
         "point lookup >=%.1fx cheaper than full scan (got %.1fx)"
         min_speedup
         ratio)
      true
      (ratio >= min_speedup))
;;

let () =
  Alcotest.run
    "insert_scaling"
    [ ( "scaling"
      , [ Alcotest.test_case "bulk insert is not O(n^2)" `Slow test_insert_not_quadratic
        ; Alcotest.test_case "point lookup is O(log n)" `Slow test_point_lookup_fast
        ] )
    ]
;;
