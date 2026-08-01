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

    Validated: reverting either site to the old [cursor_open] drain makes the
    corresponding ratio ~3.0x (1x->3x index), tripping the gate.

    The ratio gate is a wall-clock measurement: on a shared, loaded CI runner a
    sub-millisecond 1k baseline is noise-dominated and the ratio flakes.  As with
    the [bench_*] suites, CI neutralizes it via [GRANARY_BENCH_MAX_RATIO] (set
    high) so the benches still run and print, without failing on load.

    #529: a single measurement flaked under a parallel [dune test] (observed up
    to 10.5x with no code change).  Three mitigations, in order of importance:
    best-of-N over interleaved trials, longer timing loops (250 reps, so the
    timed region is tens of ms rather than ~1.6 ms), and a 2.5x default gate. *)

module Db = Granary.Db

let run = Lwt_main.run

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "db error: %a" Db.pp_error e
;;

let now () = Unix.gettimeofday ()

(* Timing-ratio ceiling for the O(log n) gate, applied to the best-of-N ratio.
   Defaults to 2.5 — a wide margin over the ~1.4x a healthy best-of-N shows,
   still below the ~3x a re-introduced O(n) drain produces.  Raised via
   [GRANARY_BENCH_MAX_RATIO] to neutralize the gate on loaded CI (the same knob
   the [bench_*] suites and [test_insert_scaling] use). *)
let max_ratio =
  match Sys.getenv_opt "GRANARY_BENCH_MAX_RATIO" with
  | Some v ->
    (try float_of_string v with
     | _ -> 2.5)
  | None -> 2.5
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

let assert_flat label ratio =
  (* 3x the surrounding index.  A full drain costs ~3x (validated); an O(log n)
     seek is ~flat.  Gate at < [max_ratio] (default 2.5): wide margin over the
     ~1.4x a healthy best-of-N shows, still under the ~3x a re-introduced drain
     produces; neutralized on loaded CI via GRANARY_BENCH_MAX_RATIO. *)
  Alcotest.(check bool)
    (Printf.sprintf "%s: 3x index < %.1fx slower (got %.2fx)" label max_ratio ratio)
    true
    (ratio < max_ratio)
;;

let test_fts_queries_flat () =
  (* Both indexes are held open at once and the two sizes are timed back to back
     inside every trial, so a load spike that inflates one size inflates its
     partner too.  The verdict is the BEST (minimum) ratio over [trials]: noise
     only ever adds time, so the minimum is the closest estimate of the true
     cost ratio, and a single co-scheduled outlier can no longer decide the
     outcome (#529). *)
  let trials = 5 in
  let reps = 250 in
  with_db (fun small ->
    with_db (fun large ->
      build small ~docs:1000;
      build large ~docs:3000;
      (* Discarded warm-up: first-touch page-cache and Lwt/parser allocation
         costs land here rather than in trial 1. *)
      ignore (time_once small ~docs:1000 ~reps:20 : float * float);
      ignore (time_once large ~docs:3000 ~reps:20 : float * float);
      let best_term = ref infinity
      and best_prefix = ref infinity in
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
        if term_ratio < !best_term then best_term := term_ratio;
        if prefix_ratio < !best_prefix then best_prefix := prefix_ratio
      done;
      Printf.eprintf
        "FTS-SCALING: best-of-%d  term ratio=%.2f  prefix ratio=%.2f  (gate %.2f)\n%!"
        trials
        !best_term
        !best_prefix
        max_ratio;
      assert_flat "exact-term query" !best_term;
      assert_flat "prefix query" !best_prefix))
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
