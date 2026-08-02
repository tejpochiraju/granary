(** #546 — is the hash join's unconditional build-side seek (#528) a regression
    when the pinned prefix is NOT selective, on disk?

    A measurement tool, NOT a pass/fail gate (hence [executable], not [test] —
    seeding 130,000 rows on a real file is far too slow for [dune test]).

    #528 gave the build side an access path whenever the WHERE clause pins a
    leading index prefix. [stream_index_lookup] does one [rh_get] on the TABLE
    tree per index entry, so where the prefix selects nearly every row the change
    trades one ordered leaf walk for an index traversal plus a random table probe
    per row. Every number in PR #531 came from [Db.open_in_memory], where that
    asymmetry does not exist — so nothing measured said whether it is safe on a
    file. This does.

    The comparison needs no code toggle: [WHERE sw = 1] is the seeked plan and
    [WHERE sw + 0 = 1] is the same query with the same answer whose prefix the
    planner cannot recognise, i.e. exactly the pre-#528 plan. That is the foil
    pattern the #528/#532 tests already use.

    Six populations, each 100,000 stock rows, varying two axes:

      - {b prefix selectivity}: 1 warehouse ([sw = 1] selects 100% — the
        pathological case) through 4, 16 and 100 ([sw = 1] selects 25%, 6%, 1%).
        #531's in-memory row 1f is the 4-warehouse one.
      - {b rowid correlation}: stock inserted in [(sw, si)] order, so index order
        and rowid order agree and the per-entry [rh_get] walks the table almost
        sequentially; versus inserted with [si] scrambled, so the index walk
        visits the table at random. #541 measured that axis on the DML drain and
        found more than a page read per row on the uncorrelated side. Here it
        turns out to make almost no difference (393,980 reads against 393,075),
        which is itself the finding: the cost is the per-entry descent from the
        root, not where in the table it lands, so an ordered fetch of the kind
        #541 landed for the DML drain would NOT recover it.

    Reads are pager-level backend resolutions — [Page_read] (main file) plus
    [Wal_read] (WAL overlay) — i.e. pager-cache misses, cold from a freshly
    opened handle. They are near-deterministic and are the load-bearing number;
    wall time is reported cold and warm but this engine's dev host is shared, so
    read a 3x as real and a 1.1x as noise. Every read observed here is in fact a
    [Wal_read], with or without the post-seeding checkpoint, so
    [GRANARY_PAGE_CACHE] barely participates — worth knowing before tuning it.

    Tunables:
      B546_STOCK       stock rows                  (default 100000)
      B546_LINE        driving rows                (default 30000)
      B546_REPS        warm repeats, best-of       (default 3)
      B546_CHECKPOINT  checkpoint after seeding    (default 1)
      B546_EXTRA       extra WHERE conjunct, both  (default none; see #575 below)
    Also honours the engine's own GRANARY_PAGE_CACHE (pager capacity, 4 KB
    pages; the engine's own default is 1024).

    {b Result} (2026-08-02, 100,000 stock / 30,000 driving, cache 1024):

    {v
      selectivity        seek reads   scan reads   seek/scan cold ms
      100% (1 wh)           393,980       93,067   2.6x - 3.5x  SLOWER
       25% (4 wh)           166,691       93,03x   ~1.0x - 1.2x
        6% (16 wh)          109,869       93,049   0.6x - 0.7x  faster
        1% (100 wh)          93,960       93,103   0.6x - 1.1x  faster
    v}

    So the regression is real: the seek costs about 3 pager resolutions per row
    it fetches, where the sequential walk it replaces costs about 0.008, and it
    only reaches read-parity near 1% selectivity. [rows_examined] reports 130,000
    for both plans in the 100% row — see [index_entries] (#546 part 3, landed)
    for the counter that does not.

    {b #575 acted on this and the table above is now history.} The decision was
    option B: take the build-side seek only where it reaches at most one row (a
    rowid alias, or a UNIQUE index with every key column pinned), or where a #532
    range has BOTH ends as integer literals and [range_rows_estimate] puts the
    window below the table's size. [WHERE sw = 1] is a bare prefix pin of
    [stock]'s key, so every row of the table above is now declined — [seek_sql]
    and [scan_sql] below are the same plan, and the ratio columns should read
    1.00x.

    That makes this file a REGRESSION HARNESS rather than a comparison: run it to
    confirm the pessimisation is gone, and to re-measure if #576 (per-index
    leading-column cardinality) ever restores a conditional seek. The
    [index_entries] column, added for that purpose, is the one that says which
    plan was actually taken — it reads 30,000 (the driving side alone) for both
    spellings today, and would read 130,000 for the seeked one if the guard were
    removed. [rows_examined] cannot tell them apart, which is the whole point of
    the counter.

    {b [B546_EXTRA] is how the shapes the gate LETS THROUGH get measured}, and it
    was added because the first version of the #575 gate admitted any range at
    all and nothing here could express that. Three runs pin the gate's three
    outcomes (row 1, the 100% population; 100,000 stock / 30,000 driving for the
    first, 10,000 / 3,000 for the others):

    {v
      B546_EXTRA                seek reads  idxent | scan reads  idxent | verdict
      (unset)                       93,067  30,000 |     93,067  30,000 | declined
      si >= 0                       93,067  30,000 |     93,067  30,000 | declined
      si BETWEEN 20 AND 40           6,146   3,021 |      6,299   3,000 | SEEKED, 0.51x
      si BETWEEN 0 AND 1000000       6,299   3,000 |      6,299   3,000 | declined
    v}

    Row 2 is the one that matters: [si >= 0] is a tautology, and before the gate
    was corrected it turned the declined pin into a one-ended seek costing
    393,980 reads and 3.6x the scan's wall time — a worse regression than the one
    #575 was written to remove, reachable by appending a no-op to a WHERE clause.
    Row 3 shows #532's narrow-window win is still taken; row 4 shows a window
    the estimator can read but which covers the table is declined anyway. *)

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

let n_stock = env_int "B546_STOCK" 100_000
let n_line = env_int "B546_LINE" 30_000
let reps = env_int "B546_REPS" 3

(* Checkpoint the WAL after seeding, by default.  It matters more than it
   looks: {!Granary_storage.Pager} deliberately does NOT cache WAL-resolved
   pages (frame indices are recycled on reset, so a cached entry could go
   stale), so on an unchecked WAL every page ACCESS emits a [Wal_read] and the
   pager cache is inert.  Checkpointed is the steady state a long-lived database
   is in and the only configuration in which the cache is exercised at all; set
   [B546_CHECKPOINT=0] for the other one. *)
let checkpoint_after_seed = env_int "B546_CHECKPOINT" 1 <> 0
let exec db sql = ignore (unwrap (run (Db.execute db sql)))
let open_at path = unwrap (run (Granary_unix.open_file_wal ~path ()))

let close db =
  try ignore (run (Db.close db)) with
  | _ -> ()
;;

(* [f] gets the file's path and opens the database itself, because the pager
   cache lives in the handle: a query run twice against one handle reads almost
   no pages the second time, so a cold read count means a freshly opened
   database. *)
let with_file_path f =
  let dir = Filename.temp_file "b546-" "" in
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

(* [si] values in a fixed pseudo-random order, so rowid order (insertion order)
   and index order [(sw, si)] disagree.  A fixed multiplier rather than
   [Random] keeps the run reproducible. *)
let scrambled n =
  let a = Array.init n (fun i -> i + 1) in
  let s = ref 12_345 in
  for i = n - 1 downto 1 do
    s := ((!s * 1_103_515_245) + 12_345) land 0x3FFFFFFF;
    let j = !s mod (i + 1) in
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  done;
  a
;;

let seed db ~n_w ~correlated ~overlap =
  exec db "PRAGMA synchronous = off";
  exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
  exec db "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
  let per_w = n_stock / n_w in
  exec db "BEGIN";
  for o = 1 to n_line do
    exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o ((o mod per_w) + 1))
  done;
  let order = if correlated then Array.init per_w (fun i -> i + 1) else scrambled per_w in
  for w = 1 to n_w do
    let base = if overlap then 0 else (w - 1) * per_w in
    Array.iter
      (fun si ->
         exec
           db
           (Printf.sprintf
              "INSERT INTO stock VALUES (%d, %d, %d)"
              w
              (base + si)
              ((w * 1000) + si)))
      order
  done;
  exec db "COMMIT";
  if checkpoint_after_seed then exec db "PRAGMA wal_checkpoint"
;;

(* One full execution, drained, with page reads and rows examined collected. *)
let once db sql =
  let reads = ref 0
  and wal = ref 0 in
  Db.set_event_callback
    db
    (Some
       (function
         | Db.Event.Page_read _ -> incr reads
         | Db.Event.Wal_read _ ->
           incr reads;
           incr wal
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
  ms, !reads, !wal, st.Db.rows_examined, st.Db.index_entries, n
;;

(* Cold reads from a freshly opened handle, then best-of-[reps] warm wall time
   on that same handle.  The two are reported separately on purpose: the cold
   count is the physical I/O the plan costs and is near-deterministic, while the
   warm time is CPU and is the only figure a loaded host can move. *)
let measure path sql =
  let db = open_at path in
  let cold_ms, reads, wal, examined, entries, rows = once db sql in
  let best = ref cold_ms in
  for _ = 1 to reps do
    let ms, _, _, _, _, _ = once db sql in
    if ms < !best then best := ms
  done;
  close db;
  cold_ms, !best, reads, wal, examined, entries, rows
;;

(* #575: an extra conjunct appended to BOTH predicates, so the seeked and
   unseekable spellings stay the same query.  Empty by default.

   This exists because the first #575 gate admitted any range, and the shape that
   broke it — [B546_EXTRA='si >= 0'], a tautology that turns the pin into a
   one-ended range — had to be constructed by hand to be measured.  A harness
   that can only express the shape a decision was made on cannot check the shapes
   the decision lets through. *)
let extra_pred =
  match Sys.getenv_opt "B546_EXTRA" with
  | Some s when String.trim s <> "" -> " AND " ^ s
  | _ -> ""
;;

let seek_sql =
  "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 1" ^ extra_pred
;;

let scan_sql =
  "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw + 0 = 1"
  ^ extra_pred
;;

let scenario ~label ~n_w ~correlated ~overlap =
  with_file_path (fun path ->
    let db = open_at path in
    seed db ~n_w ~correlated ~overlap;
    close db;
    let scold, swarm, sreads, swal, sex, sent, srows = measure path seek_sql in
    let fcold, fwarm, freads, fwal, fex, fent, frows = measure path scan_sql in
    if srows <> frows
    then failwith (Printf.sprintf "%s: seek returned %d rows, scan %d" label srows frows);
    Printf.printf
      "%-32s | %7d %6d %7.0f %7.0f %7d %7d | %7d %6d %7.0f %7.0f %7d %7d | %5.2fx %5.2fx\n\
       %!"
      label
      sreads
      swal
      scold
      swarm
      sex
      sent
      freads
      fwal
      fcold
      fwarm
      fex
      fent
      (scold /. fcold)
      (swarm /. fwarm))
;;

let () =
  Printf.printf
    "#546: hash-join build side, on disk (%d stock rows, %d driving rows, \
     GRANARY_PAGE_CACHE=%s, checkpointed=%b, best of %d)\n"
    n_stock
    n_line
    (Option.value ~default:"unset (1024)" (Sys.getenv_opt "GRANARY_PAGE_CACHE"))
    checkpoint_after_seed
    reps;
  Printf.printf
    "%-32s | %7s %6s %7s %7s %7s %7s | %7s %6s %7s %7s %7s %7s | %s\n"
    "population"
    "S:reads"
    "wal"
    "cold ms"
    "warm"
    "exam"
    "idxent"
    "F:reads"
    "wal"
    "cold ms"
    "warm"
    "exam"
    "idxent"
    "cold/warm ratio";
  (* [si] ranges overlap between warehouses, as TPC-C's do — every warehouse
     stocks the same items.  That means the SCANNED build side hashes n_w rows
     per join key where the seeked one hashes 1, so above n_w = 1 the ratio
     credits the seek with avoiding join fan-out as well as with reading less.
     The [disjoint] rows below give each warehouse its own [si] range, which
     removes the fan-out and isolates the access path. *)
  scenario ~label:"1 wh (100%), correlated" ~n_w:1 ~correlated:true ~overlap:true;
  scenario ~label:"1 wh (100%), scrambled" ~n_w:1 ~correlated:false ~overlap:true;
  scenario ~label:"4 wh (25%), TPC-C overlap" ~n_w:4 ~correlated:true ~overlap:true;
  scenario ~label:"4 wh (25%), disjoint" ~n_w:4 ~correlated:true ~overlap:false;
  scenario ~label:"16 wh (6%), disjoint" ~n_w:16 ~correlated:true ~overlap:false;
  scenario ~label:"100 wh (1%), disjoint" ~n_w:100 ~correlated:true ~overlap:false
;;
