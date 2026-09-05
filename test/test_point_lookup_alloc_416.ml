(** #416: a warm point lookup (`Op_rowid_lookup`, all pages cache-resident,
    a tiny payload so this is not payload-copy cost) allocates far more than
    the one row it returns.

    The #414 multicore-headroom bench measured ~1775 minor-heap words/lookup
    for [SELECT payload FROM t WHERE id = ?].  #416's own first investigation
    (issue comment, 2026-06-19) decomposed that as ~958 words in the B-tree
    descent, ~207 in the per-lookup RO snapshot lifecycle, ~540 in the
    SQL/exec layer and ~49 in [Rowid.encode], and concluded there was no cheap
    high-impact fix.  A re-measurement (2026-09-03, this file's own harness at
    2000 rows) found the attribution had MOVED, and two of the terms it moved
    into were contained:

    {ul
    {- ~231 words in an [Lwt_stream] layer nobody had costed.  The statement
       plans as [Op_project { child = Op_rowid_lookup }], and [to_stream]'s
       generic [Op_project] arm wraps the child in [Lwt_stream.map] — whose
       source is the ASYNC [Lwt_stream.from] one, so a single row costs a
       promise chain on top of the second stream record.  Measured directly:
       [of_list [x] + to_list] is 122 words, [map (of_list [x]) + to_list] is
       353.  The projection is now fused into the lookup, which yields at most
       one row, so [project_row] is applied to that row instead.}
    {- ~214 words re-resolving the table's B-tree ROOT through the meta tree on
       every lookup.  This is not the descent the earlier investigation priced
       and dismissed; it is the fact that [ro_begin] hands out an empty
       [rs_snap_trees], so a statement that opens a snapshot, resolves one tree
       and closes it again pays a whole asynchronous meta-tree descent per
       execution.  [Store] now memoizes [tree_id -> root page] across snapshots
       at one committed generation — see [bt_get_tree_ro] and
       [test/test_ro_root_memo_416.ml], which pins the invalidation.}}

    Together: 1419.9 -> 950.9 words/lookup on this harness, a 33% cut.  What
    is left is still dominated by the terms the issue re-scoped itself to and
    this file does NOT address: ~420 words in the data-tree descent itself
    (Lwt bind/closure chains across an async multi-level descent, needing the
    synchronous cache-resident fast path) and ~160 in [ro_begin]/[ro_end].
    The issue's "well under 500 words/lookup" target is therefore still open.

    Two earlier notes worth keeping, so nobody re-derives them:

    - #699 shrank [Store.ro_begin_at]'s [rs_pinned] pin-set request from 64 to
      8.  [Hashtbl.create] rounds up to a 16-slot minimum array either way, so
      that saved the 64-to-16-slot difference (48 words) on every snapshot
      open and let [Hashtbl]'s own resizing pay for a wide scan only when one
      happens.
    - Replacing [stream_rowid_lookup]'s [Lwt_stream.of_list] with a
      hand-written [Lwt_stream.from_direct] single-row constructor was tried
      and MEASURED WORSE (+7 words/lookup, reproducibly).  [of_list] on a
      one-element list is already about as cheap as this codebase's
      [Lwt_stream] gets; the ~98 words it costs are [Lwt_stream.from_source]'s
      floor (a queue node, a [Lwt.wait] pair and the stream record), not
      [of_list]'s overhead.  Do not retry it.  The win above came from
      removing a stream LAYER, not from building one more cheaply.

    Measured with [Gc.allocated_bytes] (an exact counter, not a timer) over a
    warm on-disk WAL store with a small (~14-byte) payload, so the number is
    dominated by lookup machinery rather than payload copying — the same shape
    #416 measured.  Being a counter rather than a clock, it is insensitive to
    machine load: three consecutive runs on a box with sibling agents building
    reported 950.9 every time, to the decimal. *)

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

(* Measured on this box, same methodology (word size 8, 2000 rows, 2000
   re-executions of one prepared statement):

     main (c9f52a0)                        : 1419.9 words/lookup
     + Op_project fused into the lookup    : 1162.4
     + RO tree-root memo across snapshots  :  950.9

   Reverting EITHER change lands back above 1160, so a single ceiling below
   that catches both.  The measurement is deterministic — three consecutive
   runs reported 950.9 exactly — so the headroom below is for a different
   allocator or word size, not for run-to-run noise; ~10% is already far more
   slack than the 1.5% the pre-#416 ceiling carried.

   The knob is the escape hatch for a platform whose word size or allocator
   moves the number (cross-arch's arm64 arm has never run this armed).  It
   raises the ceiling without disabling the measurement, which is printed
   either way. *)
let max_words_per_lookup =
  match Sys.getenv_opt "GRANARY_MEM_MAX_WORDS_PER_LOOKUP" with
  | Some s ->
    (try float_of_string s with
     | _ -> 1050.0)
  | None -> 1050.0
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
