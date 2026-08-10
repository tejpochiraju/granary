(** #416: a warm point lookup (`Op_rowid_lookup`, all pages cache-resident,
    a tiny payload so this is not payload-copy cost) allocates far more than
    the one row it returns.

    The #414 multicore-headroom bench measured ~1775 minor-heap words/lookup
    for `SELECT payload FROM t WHERE id = ?`, and #416's own investigation
    (issue comment, 2026-06-19) decomposed it: ~958 words in the B-tree
    descent itself, ~207 in the per-lookup RO snapshot lifecycle
    ([ro_begin]/[ro_end]'s two hashtables plus reader-count bookkeeping),
    ~540 in the SQL/exec layer ([Lwt_stream] wrapping and row decode above
    the store), ~49 in [Rowid.encode]. That investigation also found no
    single cheap fix for the dominant ~958 (an async multi-level descent
    diffused across Lwt binds) or the ~207 (removing it needs
    statement/transaction-level snapshot reuse, a usage-level change) — see
    the issue for why both were left as the larger, separate #156-adjacent
    rearchitecture.

    This test pins the one contained win taken from the ranked list that
    #416 proposed that doesn't require either of those larger changes:
    [Store.ro_begin_at]'s [rs_pinned] pin-set started at a 64-slot
    [Hashtbl], sized for a scan, though a point lookup pins only a handful
    of pages. [Hashtbl.create] rounds up to a 16-slot minimum array
    regardless, so shrinking the request to 8 saves the 64-to-16-slot
    difference (48 words) on every snapshot open and lets [Hashtbl]'s own
    resizing pay for a wide scan only when one actually happens.

    A second candidate from the same list — replacing
    [Exec.stream_rowid_lookup]'s [Lwt_stream.of_list] with a hand-written
    [Lwt_stream.from_direct] single-row constructor, on the theory that
    [of_list]'s push-stream machinery is needlessly heavy for one element —
    was tried and MEASURED WORSE (+7 words/lookup, reproducibly), so it was
    reverted rather than landed on the strength of the theory alone. Left as
    a note so nobody retries the same idea expecting the same reasoning to
    hold: [Lwt_stream.of_list] on a one-element list is, empirically, already
    about as cheap as this codebase's [Lwt_stream] gets.

    Measured here with [Gc.allocated_bytes] (an exact counter, not a timer),
    over a warm on-disk WAL store with a small (~14-byte) payload, so the
    number is dominated by lookup machinery rather than payload copying — the
    same shape #416 measured. The gate is deliberately loose: this one fix is
    48 words out of the ~1775 the issue measured, nowhere near the dominant
    ~958 (B-tree descent) + ~207 (snapshot lifecycle) terms the issue's own
    investigation found no cheap fix for, so this tracks a MARGIN between the
    measured pre-fix and post-fix baselines rather than the issue's "well
    under ~500 words/lookup" target, which remains open pending the sync
    fast-path rearchitecture the issue re-scoped itself to. *)

module Db = Granary.Db

let run = Lwt_main.run
let word_size = Sys.word_size / 8

let ok_db = function
  | Ok db -> db
  | Error e -> Alcotest.failf "open_file_wal error: %a" Db.pp_error e
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let n_rows = 2000

(* Plain synchronous helper, not an [Lwt.t]-returning one: [run] is called
   once per step here, never nested inside another [run] — [f] below is free
   to call [run] itself. *)
let with_warm_db f =
  let path = Filename.temp_file "granary_416" ".db" in
  (try Unix.unlink path with
   | _ -> ());
  Fun.protect
    ~finally:(fun () ->
      (try Unix.unlink path with
       | _ -> ());
      (try Unix.unlink (path ^ "-wal") with
       | _ -> ());
      try Unix.unlink (path ^ "-shm") with
      | _ -> ())
    (fun () ->
       let db = ok_db (run (Granary_unix.open_file_wal ~path ())) in
       Fun.protect
         ~finally:(fun () ->
           try run (Db.close db) with
           | _ -> ())
         (fun () ->
            exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, payload TEXT)";
            exec db "BEGIN";
            for i = 1 to n_rows do
              exec db (Printf.sprintf "INSERT INTO t VALUES (%d, 'p%03d')" i (i mod 999))
            done;
            exec db "COMMIT";
            f db))
;;

(* Average words allocated per re-execution of a prepared point-lookup
   statement, all pages already cache-resident. *)
let words_per_lookup db ~reps =
  let stmt =
    match run (Db.prepare db "SELECT payload FROM t WHERE id = ?") with
    | Ok s -> s
    | Error e -> Alcotest.failf "prepare: %a" Db.pp_error e
  in
  let one id =
    match run (Db.iter stmt ~params:[ Db.V_int (Int64.of_int id) ]) with
    | Ok stream -> ignore (run (Lwt_stream.to_list stream))
    | Error e -> Alcotest.failf "iter: %a" Db.pp_error e
  in
  (* Warm up: first executions touch cold Lwt-scheduler/allocator paths that
     settle after a handful of calls, and are not what #416 is about. *)
  for i = 1 to 50 do
    one (1 + (i mod n_rows))
  done;
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  for i = 1 to reps do
    one (1 + (i mod n_rows))
  done;
  let after = Gc.allocated_bytes () in
  run (Db.finalize stmt);
  (after -. before) /. float_of_int reps /. float_of_int word_size
;;

(* Measured on this box, same methodology, three ways (word size 8):
   - pre-fix ([rs_pinned = Hashtbl.create 64]):    1465.9 words/lookup
   - post-fix ([rs_pinned = Hashtbl.create 8]):    1417.9 words/lookup
   - difference:                                     48.0 words/lookup,
     exactly the 64-to-16-slot [Hashtbl] bucket-array saving the doc comment
     above derives — not a coincidence, and good evidence this gate tracks
     the real mechanism rather than incidental noise.

   The table is far smaller than #414's workload, so the B-tree descent (the
   dominant, unaddressed term) is shallower here and the absolute number is
   lower than the issue's ~1775; what matters for this gate is the 48-word
   delta, which is platform-independent (an OCaml [Hashtbl] bucket-array size
   is not word-size- or arch-sensitive the way a pointer count would be).

   The ceiling sits between the two measurements, with a little headroom on
   each side for run-to-run row-shape/warm-up variance: comfortably below the
   pre-fix number (so reverting the resize fails this test) and above the
   post-fix number (so normal variance does not). It is NOT anywhere near the
   issue's "well under 500" target — that needs the sync fast-path
   rearchitecture the issue left open; see the module doc comment. *)
let max_words_per_lookup =
  match Sys.getenv_opt "GRANARY_BENCH_MAX_WORDS_PER_LOOKUP" with
  | Some s ->
    (try float_of_string s with
     | _ -> 1440.0)
  | None -> 1440.0
;;

let point_lookup_allocation_has_headroom () =
  with_warm_db (fun db ->
    let w = words_per_lookup db ~reps:2000 in
    Printf.printf
      "\n  [#416] warm Op_rowid_lookup point lookup: %.1f words/lookup (ceiling %.0f)\n%!"
      w
      max_words_per_lookup;
    Alcotest.(check bool)
      (Printf.sprintf "%.1f words/lookup < ceiling %.0f" w max_words_per_lookup)
      true
      (w < max_words_per_lookup))
;;

let () =
  Granary_unix.install ();
  Alcotest.run
    "point_lookup_alloc_416"
    [ ( "point_lookup_alloc"
      , [ ( "warm lookup allocation has headroom under the pre-fix baseline"
          , `Quick
          , point_lookup_allocation_has_headroom )
        ] )
    ]
;;
