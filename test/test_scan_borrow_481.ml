(** #481: the storage cursor must stop copying what the scan never reads.

    #481's profile found that 69% of [scan_agg]'s cost is BELOW the executor: a
    "SELECT COUNT(*)" — which decodes no column at all — cost 400 ns/row and
    allocated 169 words (~1.35 KB) per row on ~44-byte rows.  Three copies were
    responsible, all on the scan path and all of them of bytes nobody asked for:

    - [Page.leaf_entry_at] copied the entry KEY out of the leaf page on every
      row, and every aggregate scan (and [Exec.stream_seq_scan] itself) binds it
      as [_key] and drops it;
    - [Btree.decode_leaf_value] then copied the VALUE a second time, solely to
      strip its leading one-byte inline tag;
    - [Btree.read_cur_leaf] used [Pager.read], which returns a fresh ~4 KB
      [cstruct_dup] of the page even on a cache hit, so every leaf advance
      allocated a whole page into the major heap.

    Together those made scan cost grow with the width of columns the query never
    touches — #481's Finding 4, where counting rows got 13x slower as an unread
    TEXT column grew from 12 B to 400 B.

    The fix is the codebase's existing borrow contract, extended one step:
    {!Granary_storage.Pager.read_shared} hands the cursor the pager's own page
    buffer (retained, read-only, dirty pages still copied),
    {!Granary_storage.Page.leaf_span_at} reports where the key and value LIE in
    that page instead of copying them, and
    {!Granary_store.Store.seek_next_value} lets a key-discarding consumer skip
    the key entirely.

    What is measured here, and why these two shapes:

    - {b the payload slope} — bytes allocated per row per byte of payload the
      scan never reads.  This is the machine-independent form of Finding 4:
      three copies of the payload per row show up as a slope near 3, one copy as
      a slope near 1.  It uses [Gc.allocated_bytes], an exact counter, so a
      loaded runner cannot move it.  {b It is DISARMED by default} — the ceiling
      has a derivation but no measurement yet; see [max_payload_slope] below for
      why that matters and what the benchmark pass should do with it.
    - {b value-only < key-and-value} — [seek_next_value] must allocate strictly
      less per row than [seek_next] over the same tree.  ARMED: it contains no
      constant to calibrate, and it is the assertion that fails if someone
      re-adds the key copy.

    Correctness is pinned separately and more strictly than allocation is: the
    borrowed path must return byte-identical values to the copying path for
    full scans, mid-tree seeks, overflow (out-of-line) values, a scan long
    enough that its own leaves are evicted underneath it — run over BOTH a WAL
    store and a plaintext one, because {!Granary_storage.Pager.read_shared} has
    two distinct borrow sources — and a read-your-own-writes scan inside an open
    RW transaction (the one path where [read_shared] deliberately still copies,
    because a dirty page is mutated in place).

    {b Deliberately NOT tested here: a checkpoint landing mid-scan.}  It is the
    one remaining way a retained WAL-frame buffer could in principle be
    invalidated, and review traced it as sound ([Wal.reset] resets the frame
    hashtable and never mutates a buffer, and the retained plaintext frame is
    byte-equal to the checkpointed main-DB page).  Writing the test needs a
    checkpoint to run concurrently with a live cursor holding an RO snapshot,
    and a checkpoint that parks on that snapshot would hang the suite — not
    something to introduce in a branch that has never been compiled.  The
    benchmark/verification pass should add it; it is listed as an open item on
    the PR. *)

open Lwt.Syntax

module S = struct
  include Granary_store.Store

  let open_file = Granary_unix.Store.open_file
  let open_file_wal = Granary_unix.Store.open_file_wal
end

module Db = Granary.Db
module Row = Granary_encoding.Row

let run = Lwt_main.run

(* Fixed-width ascending keys so [seek_ge ""] streams in insertion order and
   leaves pack densely. *)
let key_of i = Bytes.of_string (Printf.sprintf "%012d" i)

(* A value of exactly [width] bytes whose content depends on [i], so a copy that
   silently reads the wrong offset cannot pass. *)
let val_of ~width i =
  let prefix = Printf.sprintf "row-%d:" i in
  let b = Bytes.make width '.' in
  let n = min width (String.length prefix) in
  Bytes.blit_string prefix 0 b 0 n;
  b
;;

let ok_store : (S.t, S.error) result -> S.t = function
  | Ok s -> s
  | Error e -> Alcotest.failf "open_file error: %a" S.pp_error e
;;

let with_store_gen ~open_db ~f =
  let path = Filename.temp_file "granary_481" ".db" in
  (try Unix.unlink path with
   | _ -> ());
  Lwt.finalize
    (fun () ->
       let* r = open_db ~path () in
       let s = ok_store r in
       Lwt.finalize (fun () -> f s) (fun () -> S.close s))
    (fun () ->
       (try Unix.unlink path with
        | _ -> ());
       (try Unix.unlink (path ^ "-wal") with
        | _ -> ());
       Lwt.return_unit)
;;

let with_wal_store ~f =
  with_store_gen ~open_db:(fun ~path () -> S.open_file_wal ~path ()) ~f
;;

(* Plaintext file, no WAL: every leaf resolves through
   [Pager.load_main_page_borrow] and the pager's own [t.cache], which is the
   borrow source a WAL store may never touch.  See
   [scan_outlives_page_eviction]. *)
let with_plain_store ~f =
  with_store_gen ~open_db:(fun ~path () -> S.open_file ~path ()) ~f
;;

let populate s ~tid ~n ~width =
  let* tx = S.rw_begin s in
  let rec ins i =
    if i >= n
    then Lwt.return_unit
    else
      let* () = S.put tx tid (key_of i) (val_of ~width i) in
      ins (i + 1)
  in
  let* () = ins 0 in
  S.commit tx
;;

(* Drain with the key-and-value cursor, collecting values in order. *)
let drain_kv s ~tid ~from_key =
  let* tx = S.ro_begin s in
  let* cur = S.seek_ge tx tid from_key in
  let acc = ref [] in
  let rec g () =
    let* kv = S.seek_next cur in
    match kv with
    | None -> Lwt.return_unit
    | Some (_k, v) ->
      acc := v :: !acc;
      g ()
  in
  let* () = g () in
  S.seek_close cur;
  let* () = S.ro_end tx in
  Lwt.return (List.rev !acc)
;;

(* Drain with the value-only cursor added by #481. *)
let drain_values s ~tid ~from_key =
  let* tx = S.ro_begin s in
  let* cur = S.seek_ge tx tid from_key in
  let acc = ref [] in
  let rec g () =
    let* v = S.seek_next_value cur in
    match v with
    | None -> Lwt.return_unit
    | Some v ->
      acc := v :: !acc;
      g ()
  in
  let* () = g () in
  S.seek_close cur;
  let* () = S.ro_end tx in
  Lwt.return (List.rev !acc)
;;

(* Count only, returning bytes allocated by the drain itself.  The tree is
   drained once first so the pager cache is warm and the measured pass is
   steady-state — the cost being measured is copying, not I/O. *)
let count_only s ~tid =
  let* tx = S.ro_begin s in
  let* cur = S.seek_ge tx tid Bytes.empty in
  let n = ref 0 in
  let rec g () =
    let* v = S.seek_next_value cur in
    match v with
    | None -> Lwt.return_unit
    | Some _ ->
      incr n;
      g ()
  in
  let* () = g () in
  S.seek_close cur;
  let* () = S.ro_end tx in
  Lwt.return !n
;;

let count_only_kv s ~tid =
  let* tx = S.ro_begin s in
  let* cur = S.seek_ge tx tid Bytes.empty in
  let n = ref 0 in
  let rec g () =
    let* kv = S.seek_next cur in
    match kv with
    | None -> Lwt.return_unit
    | Some _ ->
      incr n;
      g ()
  in
  let* () = g () in
  S.seek_close cur;
  let* () = S.ro_end tx in
  Lwt.return !n
;;

(* Bytes allocated per row by [drain], measured after a warm-up pass. *)
let bytes_per_row ~expect drain =
  let* warm = drain () in
  Alcotest.(check int) "warm-up drained every row" expect warm;
  let a0 = Gc.allocated_bytes () in
  let* got = drain () in
  let a1 = Gc.allocated_bytes () in
  Alcotest.(check int) "measured drain drained every row" expect got;
  Lwt.return ((a1 -. a0) /. float_of_int got)
;;

let bytes_eq name expected actual =
  Alcotest.(check int)
    (name ^ ": same number of values")
    (List.length expected)
    (List.length actual);
  List.iteri
    (fun i (e, a) ->
       if not (Bytes.equal e a)
       then
         Alcotest.failf
           "%s: value %d differs: expected %S, got %S"
           name
           i
           (Bytes.to_string e)
           (Bytes.to_string a))
    (List.combine expected actual)
;;

(* ------------------------------------------------------------------ *)
(* Correctness: borrowed == copying                                     *)
(* ------------------------------------------------------------------ *)

let n_small = 4000

(* Inline values (well under [Btree.inline_value_threshold] = 800) exercise the
   span path's [tag_inline] branch — the one that now copies the payload once
   instead of twice. *)
let test_value_only_matches_kv_inline () =
  run
    (with_wal_store ~f:(fun s ->
       let tid = 0 in
       let* () = populate s ~tid ~n:n_small ~width:64 in
       let expected = List.init n_small (fun i -> val_of ~width:64 i) in
       let* kv = drain_kv s ~tid ~from_key:Bytes.empty in
       bytes_eq "copying full scan" expected kv;
       let* vals = drain_values s ~tid ~from_key:Bytes.empty in
       bytes_eq "borrowed full scan" expected vals;
       (* And from the middle of the tree, which enters through
          [cursor_scan_for_key] — the seek probe that now compares keys in place
          instead of copying each one it steps over. *)
       let from = n_small / 3 in
       let tail = List.filteri (fun i _ -> i >= from) expected in
       let* kv_tail = drain_kv s ~tid ~from_key:(key_of from) in
       bytes_eq "copying mid-tree seek" tail kv_tail;
       let* v_tail = drain_values s ~tid ~from_key:(key_of from) in
       bytes_eq "borrowed mid-tree seek" tail v_tail;
       Lwt.return_unit))
;;

(* Values above [inline_value_threshold] spill to an overflow chain, so the
   span's [tag_overflow] branch reads the 17-byte marker's fields straight out
   of the page instead of copying the marker to parse it.  Getting that wrong
   would corrupt the head page-id or the total size — neither is subtle. *)
let test_value_only_matches_kv_overflow () =
  run
    (with_wal_store ~f:(fun s ->
       let tid = 0 in
       let n = 300 in
       let* () = populate s ~tid ~n ~width:5000 in
       let expected = List.init n (fun i -> val_of ~width:5000 i) in
       let* kv = drain_kv s ~tid ~from_key:Bytes.empty in
       bytes_eq "copying overflow scan" expected kv;
       let* vals = drain_values s ~tid ~from_key:Bytes.empty in
       bytes_eq "borrowed overflow scan" expected vals;
       Lwt.return_unit))
;;

(* A mixed tree: inline and overflow values interleaved, so the two branches
   alternate within a single leaf walk. *)
let test_mixed_inline_and_overflow () =
  run
    (with_wal_store ~f:(fun s ->
       let tid = 0 in
       let n = 600 in
       let width i = if i mod 3 = 0 then 4000 else 40 in
       let* tx = S.rw_begin s in
       let rec ins i =
         if i >= n
         then Lwt.return_unit
         else
           let* () = S.put tx tid (key_of i) (val_of ~width:(width i) i) in
           ins (i + 1)
       in
       let* () = ins 0 in
       let* () = S.commit tx in
       let expected = List.init n (fun i -> val_of ~width:(width i) i) in
       let* vals = drain_values s ~tid ~from_key:Bytes.empty in
       bytes_eq "borrowed mixed scan" expected vals;
       Lwt.return_unit))
;;

(* THE LIFETIME TEST.  The cursor now RETAINS the pager's own page buffer across
   the many [seek_next_value] calls that consume one leaf, and across the
   [Lwt.pause] [Store.seek_next] splices in every 256 reads.  This table's
   leaves outnumber the pager cache several times over, so leaves the cursor is
   holding are certainly evicted from the cache mid-scan — the exact condition
   under which a borrowed buffer that were reused or invalidated would return
   another page's bytes.  Every value is checked, so a single wrong page shows
   up as a wrong value rather than a crash.

   RUN OVER BOTH STORE FLAVOURS, because [Pager.read_shared] has TWO borrow
   sources and only one of them is a WAL store's.  On a WAL store every leaf
   resolves through [resolve_wal_page_borrow] → [Wal.read_frame] and comes from
   the WAL's own bounded frame cache; the pager's [t.cache] is only reached once
   a checkpoint has moved the page to the main file, which an
   auto-checkpoint may or may not have done by the time this runs.  The
   plaintext-file store has no WAL at all, so every leaf necessarily comes from
   [load_main_page_borrow] — that arm's retention is pinned only by the non-WAL
   run.  (Found in review: every store in the first draft of this file was a WAL
   store, so the [t.cache] arm was untested.) *)
let scan_outlives_page_eviction with_store_of_flavour () =
  run
    (with_store_of_flavour ~f:(fun s ->
       let tid = 0 in
       (* ~420 B/entry over 4080 usable bytes ≈ 9-10 rows per leaf, so 20 000
          rows need ~2100 leaves against a default 1024-page cache. *)
       let n = 20_000 in
       let* () = populate s ~tid ~n ~width:400 in
       let expected = List.init n (fun i -> val_of ~width:400 i) in
       let* vals = drain_values s ~tid ~from_key:Bytes.empty in
       bytes_eq "borrowed scan across evictions" expected vals;
       (* Two live cursors interleaved over the same tree, each retaining its own
          leaf while the other's reads evict pages, and each stepping far enough
          apart that they never share a leaf. *)
       let* tx = S.ro_begin s in
       let* c1 = S.seek_ge tx tid Bytes.empty in
       let* c2 = S.seek_ge tx tid (key_of (n / 2)) in
       let rec both i acc1 acc2 =
         if i >= n / 2
         then Lwt.return (List.rev acc1, List.rev acc2)
         else
           let* v1 = S.seek_next_value c1 in
           let* v2 = S.seek_next_value c2 in
           both (i + 1) (Option.get v1 :: acc1) (Option.get v2 :: acc2)
       in
       let* got1, got2 = both 0 [] [] in
       S.seek_close c1;
       S.seek_close c2;
       let* () = S.ro_end tx in
       bytes_eq
         "interleaved cursor 1 (head half)"
         (List.filteri (fun i _ -> i < n / 2) expected)
         got1;
       bytes_eq
         "interleaved cursor 2 (tail half)"
         (List.filteri (fun i _ -> i >= n / 2) expected)
         got2;
       Lwt.return_unit))
;;

(* The dirty-page path (#262 read-your-own-writes).  [Pager.read_shared] still
   COPIES a dirty page, because a dirty buffer is the one the writer may mutate
   in place (#356) and the cursor retains what it is handed.  So a scan inside
   an open RW transaction must see that transaction's own writes and must see
   them consistently — which is what this asserts, on both cursor flavours. *)
let test_read_your_own_writes_in_rw_txn () =
  run
    (with_wal_store ~f:(fun s ->
       let tid = 0 in
       let n = 500 in
       let* () = populate s ~tid ~n ~width:48 in
       let* tx = S.rw_begin s in
       (* Overwrite every third row and append 100 more, all uncommitted. *)
       let rec upd i =
         if i >= n
         then Lwt.return_unit
         else
           let* () =
             if i mod 3 = 0
             then S.put tx tid (key_of i) (val_of ~width:48 (i + 1_000_000))
             else Lwt.return_unit
           in
           upd (i + 1)
       in
       let* () = upd 0 in
       let rec app i =
         if i >= n + 100
         then Lwt.return_unit
         else
           let* () = S.put tx tid (key_of i) (val_of ~width:48 i) in
           app (i + 1)
       in
       let* () = app n in
       let expected =
         List.init (n + 100) (fun i ->
           if i < n && i mod 3 = 0
           then val_of ~width:48 (i + 1_000_000)
           else val_of ~width:48 i)
       in
       let* cur = S.seek_ge tx tid Bytes.empty in
       let acc = ref [] in
       let rec g () =
         let* v = S.seek_next_value cur in
         match v with
         | None -> Lwt.return_unit
         | Some v ->
           acc := v :: !acc;
           g ()
       in
       let* () = g () in
       S.seek_close cur;
       bytes_eq "in-txn borrowed scan sees own writes" expected (List.rev !acc);
       S.commit tx))
;;

(* ------------------------------------------------------------------ *)
(* Allocation: the payload slope (#481 Finding 4)                       *)
(* ------------------------------------------------------------------ *)

(* Bytes allocated per row, per byte of payload the scan never looks at.

   THE UNIT IS "COPIES OF THE PAYLOAD".  Copying an [n]-byte payload out of the
   page allocates [n] bytes rounded up to a word plus a header, so over a
   384-byte spread the rounding contributes at most ~8 bytes/row ≈ 0.02 to the
   slope.  The quantity is therefore very nearly an integer and means exactly
   what it says: 1.0 = the payload is copied once, 3.0 = three times.

   Pre-fix it was copied THREE times on the scan path — the [leaf_entry_at]
   value copy, [decode_leaf_value]'s tag-strip copy, and the per-leaf 4 KB page
   dup, which amortises to almost exactly one payload copy per row because a
   leaf holds a page's worth of payload.  #481's Finding 4 measured 0.514 WORDS
   per payload byte ≈ 4.1 bytes/byte through the SQL pipeline, i.e. those three
   plus something else the SQL layer adds.  Post-fix exactly one copy remains:
   [Page.copy_span] lifting the payload out of the leaf, which is the copy the
   caller actually asked for.

   So the ceiling below is not a tuned magic number: 2.0 means "fewer than TWO
   copies of the payload per row", the smallest integer boundary that separates
   correct from regressed, and word-size/allocator differences cannot move a
   quantity by a whole copy.

   {b It is nevertheless DISARMED by default, and that is the point.}  This
   branch has never been built, let alone measured, so the derivation above is
   reasoning and not evidence — and CLAUDE.md is explicit that
   [test_not_null_600] earns its always-armed status by having been measured
   across three runs.  An unmeasured gate armed in `ci.yml` / `coverage.yml` /
   `cross-arch.yml` would make the first CI run a coin flip for every open PR,
   and would then be quietly neutralized — the honour system #549 removed.

   Unarmed, the test still measures and PRINTS the slope, and still asserts
   every correctness property around it; it just does not block.  It is armed
   in exactly one place, `.forgejo/workflows/bench-nightly.yml`, which sets
   [GRANARY_MEM_MAX_PAYLOAD_SLOPE=2.0] — the job that reports via an auto-filed
   issue rather than failing a PR, which is the right blast radius for a
   ceiling whose first real measurement has not happened yet.

   {b For whoever runs the deferred benchmark pass:} read the printed slope.  If
   it is near 1, promote this to armed-by-default (flip [None -> Some 2.0]
   below) and add the row to CLAUDE.md's non-wall-clock gate table next to
   [GRANARY_MEM_MAX_WORDS_PER_ROW].  If it is near 3, a copy came back and the
   PR did not do what it claims.  Do NOT widen the ceiling to make it pass. *)
let max_payload_slope =
  match Sys.getenv_opt "GRANARY_MEM_MAX_PAYLOAD_SLOPE" with
  | None | Some "" -> None
  | Some ("off" | "0") -> None
  | Some s ->
    (match float_of_string_opt s with
     | Some f -> Some f
     | None ->
       Alcotest.failf "GRANARY_MEM_MAX_PAYLOAD_SLOPE=%S is not a number (or \"off\")" s)
;;

let narrow_width = 16
let wide_width = 400
let n_slope = 4000

(* One store per width — same row count, same keys, same tree shape modulo the
   payload, so the difference between the two numbers is the payload and
   nothing else. *)
let count_only_bytes_per_row ~width =
  with_wal_store ~f:(fun s ->
    let* () = populate s ~tid:0 ~n:n_slope ~width in
    bytes_per_row ~expect:n_slope (fun () -> count_only s ~tid:0))
;;

let test_count_is_flat_in_unread_payload_width () =
  run
    (let* narrow = count_only_bytes_per_row ~width:narrow_width in
     let* wide = count_only_bytes_per_row ~width:wide_width in
     let slope = (wide -. narrow) /. float_of_int (wide_width - narrow_width) in
     Printf.eprintf
       "[#481] count-only allocation: %d B payload -> %.0f B/row, %d B payload -> %.0f \
        B/row; slope %.2f copies of the payload per row (%s)\n\
        %!"
       narrow_width
       narrow
       wide_width
       wide
       slope
       (match max_payload_slope with
        | None ->
          "REPORT-ONLY: unarmed, set GRANARY_MEM_MAX_PAYLOAD_SLOPE to gate; expect ~1, \
           ~3 means a copy came back"
        | Some c -> Printf.sprintf "ARMED, ceiling %.2f" c);
     (match max_payload_slope with
      | None -> ()
      | Some ceiling ->
        Alcotest.(check bool)
          (Printf.sprintf
             "counting rows must not pay for columns it never reads (slope %.2f, ceiling \
              %.2f)"
             slope
             ceiling)
          true
          (slope < ceiling));
     Lwt.return_unit)
;;

(* The direct assertion that the key copy is gone: over the SAME tree,
   [seek_next_value] must allocate strictly less per row than [seek_next].

   THIS ONE IS ARMED, and unlike the slope above it does not need a measurement
   first, because it contains no constant to calibrate.  It compares two numbers
   produced by the same process, over the same warm tree, from the same exact
   counter, by two code paths that differ only in one [Page.copy_span] of the
   12-byte key.  There is no ceiling to guess and nothing for a different
   allocator or word size to shift: a platform on which copying a key allocates
   nothing at all is not one this codebase runs on.  If it ever does fail, the
   key copy is back — that is the only thing it can mean. *)
let test_value_only_allocates_less_than_key_and_value () =
  run
    (with_wal_store ~f:(fun s ->
       let tid = 0 in
       let* () = populate s ~tid ~n:n_slope ~width:64 in
       let* kv = bytes_per_row ~expect:n_slope (fun () -> count_only_kv s ~tid) in
       let* vo = bytes_per_row ~expect:n_slope (fun () -> count_only s ~tid) in
       Printf.eprintf
         "[#481] seek_next %.0f B/row vs seek_next_value %.0f B/row (saved %.0f)\n%!"
         kv
         vo
         (kv -. vo);
       Alcotest.(check bool)
         (Printf.sprintf
            "seek_next_value allocates less than seek_next (%.0f vs %.0f B/row)"
            vo
            kv)
         true
         (vo < kv);
       Lwt.return_unit))
;;

(* ------------------------------------------------------------------ *)
(* End-to-end: the SQL answers are unchanged                            *)
(* ------------------------------------------------------------------ *)

let with_db f =
  let path = Filename.temp_file "granary_481_sql" ".db" in
  Sys.remove path;
  let db =
    match run (Granary_unix.open_file ~path ()) with
    | Ok db -> db
    | Error e -> Alcotest.failf "open %s: %a" path Db.pp_error e
  in
  Fun.protect
    ~finally:(fun () ->
      (try run (Db.close db) with
       | _ -> ());
      try Sys.remove path with
      | _ -> ())
    (fun () -> f db)
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let query db sql =
  run
    (let* r = Db.query db sql in
     match r with
     | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
     | Ok stream -> Lwt_stream.to_list stream)
;;

let show = function
  | Row.V_text s -> s
  | Row.V_null -> "<null>"
  | Row.V_int n -> Int64.to_string n
  | Row.V_real f -> string_of_float f
  | Row.V_blob _ -> "<blob>"
;;

let rows db sql =
  List.map
    (fun (r : Row.t) -> Array.to_list r |> List.map show |> String.concat "|")
    (query db sql)
;;

(* [Exec.stream_seq_scan] is the call site that switched to [seek_next_value].
   These are the query shapes that ride it: the aggregate fast path (#247, which
   decodes nothing), an aggregate that decodes one column, a filtered scan, and
   a projection of every column — the last being the one that would notice a
   value copied from the wrong offset. *)
let test_sql_scan_answers_unchanged () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, k INTEGER, payload TEXT)";
    exec db "BEGIN";
    for i = 0 to 999 do
      exec
        db
        (Printf.sprintf
           "INSERT INTO t (id, k, payload) VALUES (%d, %d, 'p-%d-%s')"
           i
           (i * 7 mod 1000)
           i
           (String.make 60 'x'))
    done;
    exec db "COMMIT";
    Alcotest.(check (list string))
      "COUNT(*)"
      [ "1000" ]
      (rows db "SELECT COUNT(*) FROM t");
    Alcotest.(check (list string))
      "COUNT(*), SUM(k)"
      [ Printf.sprintf
          "1000|%d"
          (List.fold_left ( + ) 0 (List.init 1000 (fun i -> i * 7 mod 1000)))
      ]
      (rows db "SELECT COUNT(*), SUM(k) FROM t");
    Alcotest.(check (list string))
      "SUM(id)"
      [ Printf.sprintf "%d" (999 * 1000 / 2) ]
      (rows db "SELECT SUM(id) FROM t");
    Alcotest.(check (list string))
      "MIN/MAX over a scan"
      [ "0|999" ]
      (rows db "SELECT MIN(id), MAX(id) FROM t");
    Alcotest.(check (list string))
      "filtered scan"
      [ "3" ]
      (rows db "SELECT COUNT(*) FROM t WHERE id < 3");
    Alcotest.(check (list string))
      "full projection of a scanned row"
      [ Printf.sprintf "7|49|p-7-%s" (String.make 60 'x') ]
      (rows db "SELECT id, k, payload FROM t WHERE id = 7");
    Alcotest.(check (list string))
      "ordered tail of a scan"
      [ "999"; "998"; "997" ]
      (rows db "SELECT id FROM t ORDER BY id DESC LIMIT 3");
    (* An uncommitted write must be visible to a scan in the same transaction —
       the dirty-page path [read_shared] deliberately still copies. *)
    exec db "BEGIN";
    exec db "INSERT INTO t (id, k, payload) VALUES (5000, 1, 'fresh')";
    Alcotest.(check (list string))
      "in-txn scan sees the uncommitted row"
      [ "1001" ]
      (rows db "SELECT COUNT(*) FROM t");
    Alcotest.(check (list string))
      "in-txn scan reads the uncommitted value"
      [ "fresh" ]
      (rows db "SELECT payload FROM t WHERE id = 5000");
    exec db "ROLLBACK";
    Alcotest.(check (list string))
      "rollback restores the count"
      [ "1000" ]
      (rows db "SELECT COUNT(*) FROM t"))
;;

let () =
  Alcotest.run
    "scan_borrow_481"
    [ ( "correctness"
      , [ Alcotest.test_case
            "value-only scan matches copying scan (inline values)"
            `Quick
            test_value_only_matches_kv_inline
        ; Alcotest.test_case
            "value-only scan matches copying scan (overflow values)"
            `Quick
            test_value_only_matches_kv_overflow
        ; Alcotest.test_case
            "inline and overflow values interleaved"
            `Quick
            test_mixed_inline_and_overflow
        ; Alcotest.test_case
            "scan outlives eviction of the leaves it borrowed (WAL frame cache)"
            `Slow
            (scan_outlives_page_eviction with_wal_store)
        ; Alcotest.test_case
            "scan outlives eviction of the leaves it borrowed (pager page cache)"
            `Slow
            (scan_outlives_page_eviction with_plain_store)
        ; Alcotest.test_case
            "in-txn scan sees its own writes (dirty pages still copied)"
            `Quick
            test_read_your_own_writes_in_rw_txn
        ; Alcotest.test_case
            "SQL scan answers unchanged"
            `Quick
            test_sql_scan_answers_unchanged
        ] )
    ; ( "allocation"
      , [ Alcotest.test_case
            "counting is flat in the width of unread columns"
            `Slow
            test_count_is_flat_in_unread_payload_width
        ; Alcotest.test_case
            "seek_next_value allocates less than seek_next"
            `Slow
            test_value_only_allocates_less_than_key_and_value
        ] )
    ]
;;
