(** #233 regression: FTS term/prefix queries must be O(log n) in the index size,
    not O(n).

    Before the fix [fts_posting_list] / [fts_prefix_posting_list] opened a
    [Store] cursor that materialised the ENTIRE FTS index tree into a list per
    query (the same drain that made #228/#229 O(n)).  They now use the native
    streaming [Store.seek_ge].

    The assertions are machine-independent: each times the *same* query against
    two index sizes (1x and 3x) and requires the per-op cost to stay roughly
    flat.  A query that drains the whole index grows ~linearly with size; an
    O(log n) seek does not.  The ratio is a BEST-OF-N over interleaved trials
    (#529) — see {!test_fts_queries_flat}.  Two paths are covered:
      - exact term ([fts_posting_list]) via [MATCH 'unique<k>'] (one match);
      - prefix ([fts_prefix_posting_list]) via [MATCH 'zebra*'] over a FIXED
        small set of zebra docs present in both tables (so the match count is
        constant and only the surrounding index size varies).

    Validated by actually reverting the fix, under the current best-of-9
    statistic (#537 review): with [fts_posting_list] back on the [cursor_open]
    drain the exact-term ratio is 3.67x / 3.89x (two runs), and with
    [fts_prefix_posting_list] back on it the prefix ratio is 3.75x / 3.63x —
    against 1.1-1.35x for the unmodified code and a 2.0x gate.  A separate
    negative control (pointing the term probe at [MATCH 'alpha'], which matches
    every padding doc and so is genuinely O(n) by construction) lands at 3.45x /
    3.49x.

    The ratio gate is a wall-clock measurement: on a shared, loaded CI runner a
    sub-millisecond 1k baseline is noise-dominated and the ratio flakes.  As with
    the [bench_*] suites, CI neutralizes it via [GRANARY_BENCH_MAX_RATIO] (set
    high) so the benches still run and print, without failing on load — which
    means this gate is armed only for developers running the suite locally, and
    protects #233 on the honour system (#549).

    #529: a single measurement flaked under a parallel [dune test] (observed up
    to 10.5x with no code change).  Two mitigations: best-of-N over interleaved
    trials (minimum per size, then divide — see {!test_fts_queries_flat}) and
    longer timing loops (500 reps, so the timed region is tens of ms rather than
    ~1.6 ms).  The gate stays at 2.0x — the de-flake comes from the statistic
    and the sample size, not from loosening the ceiling. *)

module Db = Granary.Db

let run = Lwt_main.run

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "db error: %a" Db.pp_error e
;;

let now () = Unix.gettimeofday ()

(* Timing-ratio ceiling for the O(log n) gate, applied to the best-of-N ratio.
   Stays at 2.0 — comfortably over the ~1.1-1.35x a healthy best-of-9 shows
   (worst of 14 measurements: 1.33x, under a full parallel [dune test] at load
   average 15 and under 8 CPU burners), and comfortably under the 3.45-3.89x an
   actually re-introduced O(n) drain produces.  Raised via
   [GRANARY_BENCH_MAX_RATIO] to neutralize the gate on loaded CI (the same knob
   the [bench_*] suites and [test_insert_scaling] use). *)
let max_ratio =
  match Sys.getenv_opt "GRANARY_BENCH_MAX_RATIO" with
  | Some v ->
    (try float_of_string v with
     | _ -> 2.0)
  | None -> 2.0
;;

(* Number of interleaved timing trials; the verdict is the best (minimum) per
   size across them.  9, not 5: measured on a loaded 8-core box, 5 trials spread
   term 0.87-1.54 / prefix 0.94-1.99 and produced a genuine false FAIL at 2.01x
   under a full parallel [dune test]; 9 trials at [reps] below spread
   1.08-1.33.  Env-overridable so a loaded runner can buy still more accuracy
   instead of turning the gate off entirely (#537 review). *)
let trials =
  match Sys.getenv_opt "GRANARY_BENCH_TRIALS" with
  | Some v ->
    (match int_of_string_opt v with
     | Some n when n > 0 -> n
     | _ -> 9)
  | None -> 9
;;

(* Iterations inside one timed loop.  500, so even the cheap 1k side spends
   ~15 ms per sample and a millisecond-scale scheduling hiccup is averaged
   rather than decisive.  This is the other half of the tail fix: at 250 reps,
   9 trials still spread 1.11-1.37 under load; at 500 they spread 1.19-1.29,
   and 1000 buys nothing further.  Whole test still runs in ~1.0 s.
   Env-overridable alongside [GRANARY_BENCH_TRIALS]. *)
let reps =
  match Sys.getenv_opt "GRANARY_BENCH_REPS" with
  | Some v ->
    (match int_of_string_opt v with
     | Some n when n > 0 -> n
     | _ -> 500)
  | None -> 500
;;

let with_db f =
  let dir = Filename.temp_file "granary_fts_scaling" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "fts.db" in
  let db = unwrap (run (Granary_unix.open_file_wal ~path ())) in
  Fun.protect
    ~finally:(fun () ->
      (try run (Db.close db) with
       | _ -> ());
      List.iter
        (fun s ->
           try Sys.remove (Filename.concat dir s) with
           | _ -> ())
        [ "fts.db"; "fts.db-wal" ];
      try Unix.rmdir dir with
      | _ -> ())
    (fun () -> f db)
;;

let exec_lwt db sql =
  let open Lwt.Syntax in
  let* r = Db.execute db sql in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let query_rowids db sql =
  run
    (let open Lwt.Syntax in
     let* s = Lwt.map unwrap (Db.query db sql) in
     Lwt_stream.to_list s)
;;

(* A fixed, size-independent set of docs matched by the prefix query, so
   [MATCH 'zebra*'] returns the same count regardless of the index size. *)
let n_zebra = 5

let mean_per_op f ~reps =
  let t0 = now () in
  for i = 0 to reps - 1 do
    f i
  done;
  (now () -. t0) /. float_of_int reps
;;

(* Build an FTS index: [n_zebra] fixed zebra docs (matched by the prefix query)
   plus [docs] padding docs each with a shared term [alpha] and a unique term
   [unique<i>].  Asserts the correctness of all three query shapes. *)
let build db ~docs =
  run (exec_lwt db "CREATE VIRTUAL TABLE docs USING FTS5(body)");
  run
    (let open Lwt.Syntax in
     let* () = exec_lwt db "BEGIN" in
     let* () =
       let rec loop j =
         if j >= n_zebra
         then Lwt.return_unit
         else
           let* () =
             exec_lwt db (Printf.sprintf "INSERT INTO docs (body) VALUES ('zebra%d')" j)
           in
           loop (j + 1)
       in
       loop 0
     in
     let rec loop i =
       if i >= docs
       then Lwt.return_unit
       else
         let* () =
           exec_lwt
             db
             (Printf.sprintf "INSERT INTO docs (body) VALUES ('alpha unique%d')" i)
         in
         loop (i + 1)
     in
     let* () = loop 0 in
     exec_lwt db "COMMIT");
  (* Correctness: a rare term matches its one doc; the shared term matches all
     padding docs; the prefix matches exactly the fixed zebra set. *)
  let one = query_rowids db "SELECT body FROM docs WHERE docs MATCH 'unique7'" in
  Alcotest.(check int) "rare term matches exactly 1 doc" 1 (List.length one);
  let all = query_rowids db "SELECT body FROM docs WHERE docs MATCH 'alpha'" in
  Alcotest.(check int) "shared term matches all padding docs" docs (List.length all);
  let zs = query_rowids db "SELECT body FROM docs WHERE docs MATCH 'zebra*'" in
  Alcotest.(check int) "prefix matches the fixed zebra set" n_zebra (List.length zs)
;;

(* One timing sample of both query paths against an already-built index:
   the exact-term path (one match, varying term) and the prefix path (fixed
   match set).  Returns (term_per_op, prefix_per_op) in seconds. *)
let time_once db ~docs ~reps =
  Gc.full_major ();
  let term_per_op =
    mean_per_op ~reps (fun i ->
      let k = i * 2654435761 mod docs in
      let rows =
        query_rowids
          db
          (Printf.sprintf "SELECT body FROM docs WHERE docs MATCH 'unique%d'" k)
      in
      if List.length rows <> 1
      then Alcotest.failf "rare term unique%d matched %d docs" k (List.length rows))
  in
  Gc.full_major ();
  let prefix_per_op =
    mean_per_op ~reps (fun _ ->
      let rows = query_rowids db "SELECT body FROM docs WHERE docs MATCH 'zebra*'" in
      if List.length rows <> n_zebra
      then Alcotest.failf "prefix zebra* matched %d docs" (List.length rows))
  in
  term_per_op, prefix_per_op
;;

(* A discarded warm-up pass: first-touch page-cache and Lwt/parser allocation
   costs land here rather than in trial 1. *)
let warm_up db ~docs = ignore (time_once db ~docs ~reps:20 : float * float)

let assert_flat label ratio =
  (* 3x the surrounding index.  A full drain costs 3.6-3.9x (measured by
     actually reverting the fix); an O(log n) seek is ~flat.  Gate at <
     [max_ratio] (default 2.0): comfortably above the 1.1-1.35x a healthy
     best-of-9 shows, comfortably below a re-introduced drain; neutralized on
     loaded CI via GRANARY_BENCH_MAX_RATIO. *)
  Alcotest.(check bool)
    (Printf.sprintf "%s: 3x index < %.1fx slower (got %.2fx)" label max_ratio ratio)
    true
    (ratio < max_ratio)
;;

let test_fts_queries_flat () =
  (* Both indexes are held open at once and the two sizes are timed back to back
     inside every trial, so a load spike that inflates one size inflates its
     partner too (#529).

     The verdict takes the minimum PER SIZE across [trials] and divides the two
     minima — NOT the minimum of the per-trial ratios.  Noise only ever adds
     time, so min-of-durations really is the closest estimate of each side's
     true cost; but a ratio has the noisy denominator too, so min-over-ratios
     systematically picks the trial where the *small* side was most perturbed
     and biases the verdict downward (observed: 0.49x, i.e. a 3x index measured
     as twice as cheap per op).  Dividing the minima cancels each side's noise
     independently (#537 review). *)
  with_db (fun small ->
    with_db (fun large ->
      build small ~docs:1000;
      build large ~docs:3000;
      warm_up small ~docs:1000;
      warm_up large ~docs:3000;
      let best_term_s = ref infinity
      and best_term_l = ref infinity
      and best_prefix_s = ref infinity
      and best_prefix_l = ref infinity in
      for t = 1 to trials do
        let term_s, prefix_s = time_once small ~docs:1000 ~reps in
        let term_l, prefix_l = time_once large ~docs:3000 ~reps in
        let term_ratio = term_l /. term_s
        and prefix_ratio = prefix_l /. prefix_s in
        Printf.eprintf
          "FTS-SCALING: trial %d/%d  term 1k=%.3f 3k=%.3f ratio=%.2f | prefix 1k=%.3f \
           3k=%.3f ratio=%.2f  (ms/op)\n\
           %!"
          t
          trials
          (term_s *. 1000.)
          (term_l *. 1000.)
          term_ratio
          (prefix_s *. 1000.)
          (prefix_l *. 1000.)
          prefix_ratio;
        if term_s < !best_term_s then best_term_s := term_s;
        if term_l < !best_term_l then best_term_l := term_l;
        if prefix_s < !best_prefix_s then best_prefix_s := prefix_s;
        if prefix_l < !best_prefix_l then best_prefix_l := prefix_l
      done;
      let best_term = !best_term_l /. !best_term_s
      and best_prefix = !best_prefix_l /. !best_prefix_s in
      Printf.eprintf
        "FTS-SCALING: best-of-%d (min per size)  term 1k=%.3f 3k=%.3f ratio=%.2f | \
         prefix 1k=%.3f 3k=%.3f ratio=%.2f  (ms/op; gate %.2f)\n\
         %!"
        trials
        (!best_term_s *. 1000.)
        (!best_term_l *. 1000.)
        best_term
        (!best_prefix_s *. 1000.)
        (!best_prefix_l *. 1000.)
        best_prefix
        max_ratio;
      assert_flat "exact-term query" best_term;
      assert_flat "prefix query" best_prefix))
;;

let () =
  Alcotest.run
    "fts_scaling"
    [ ( "scaling"
      , [ Alcotest.test_case
            "FTS term + prefix queries are O(log n)"
            `Slow
            test_fts_queries_flat
        ] )
    ]
;;
