(** #514 (second half): what the DML index-seek path does with its candidate
    rowids, and — the part that took a measurement to settle — what order it
    reads the matching rows in.

    The seek walks an index range and then reads each candidate out of the table
    tree.  Those two trees are ordered differently, so the drain has to choose:
    read each row the instant the index walk names it (no candidate buffer, but
    index-key order against the table), or buffer every candidate, sort by
    rowid, and read them in table order.

    It buffers.  Reading in index-key order is random access against the table
    for any index uncorrelated with rowid, and the pager cache is a bounded FIFO
    ({!Granary_storage.Pager}), so a table larger than the cache re-reads a page
    per row.  {!prefix_delete_reads_each_table_page_about_once} is the run that
    settled it: on disk, with the cache squeezed and index order scrambled
    against rowid order, sorting first reads ~1 page per 20 rows where fetching
    as it walks reads more than 1 page {e per row}.  The numbers themselves live
    in one place, [Granary_sql.Exec]'s [rowid_buf] comment, next to the code they
    justify.

    What #514 actually bought is a cheaper buffer — a flat [Bigarray] of rowids
    rather than an [int64 list] — not a bound.  The bound would be streaming the
    mutations, which the callers' shape forbids, and the {e other} half of #514,
    the selectivity guard, is not addressed here either.

    {!Granary_sql.Exec.dml_seek_stats} makes the shape observable:
    [dss_peak_buffered] equals the match count precisely because the drain sorts
    before it fetches, so a drop to 1 is the regression, not the goal.

    Correctness is the bigger half of this file.  Buffering the candidates means
    the index cursor is closed before any row is read and long before anything
    is written, but the drain order still has to match what a table scan would
    produce (an unordered [DELETE … LIMIT n] depends on it), and an UPDATE that
    moves the very keys being walked must still visit each row exactly once.
    Those cases run on both backends: in memory, and — since [Store.seek_ge] on
    [Mem] snapshots an immutable map while on [Btree] it holds live page ids —
    against a real file-backed B-tree too. *)

module Db = Granary.Db
module Exec = Granary_sql.Exec

let run = Lwt_main.run

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "db error: %a" Db.pp_error e
;;

let with_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

(* The Mem backend cannot see half of what this file reasons about: its
   [seek_ge] snapshots an immutable map, so the cursor is isolated from the tree
   by construction and there are no pages to read.  The B-tree cursor holds live
   page ids and a cached leaf.  Cases that care run on both. *)
let with_file_db ?page_cache f =
  let dir = Filename.temp_file "t514-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "db" in
  let restore =
    match page_cache with
    | None -> fun () -> ()
    | Some n ->
      let prev = Sys.getenv_opt "GRANARY_PAGE_CACHE" in
      Unix.putenv "GRANARY_PAGE_CACHE" (string_of_int n);
      fun () ->
        (match prev with
         | Some v -> Unix.putenv "GRANARY_PAGE_CACHE" v
         | None ->
           (* [Unix] has no [unsetenv], so an absent variable is emulated by
              setting it empty: {!Granary_storage.Pager.cache_capacity_from_env}
              reads [int_of_string_opt ""] as [None] and falls back to the
              default, which is the behaviour that matters here.  It is a parse
              fallback standing in for absence, not absence — anything testing
              PRESENCE rather than value (e.g. [bench_scan_probe]'s banner)
              would see an empty string where it expected "unset". *)
           Unix.putenv "GRANARY_PAGE_CACHE" "");
        ()
  in
  let db =
    match run (Granary_unix.open_file_wal ~path ()) with
    | Ok db -> db
    | Error e ->
      restore ();
      Alcotest.failf "open_file_wal: %a" Db.pp_error e
  in
  Fun.protect
    ~finally:(fun () ->
      restore ();
      (try run (Db.close db) with
       | _ -> ());
      List.iter
        (fun sfx ->
           try Sys.remove (path ^ sfx) with
           | _ -> ())
        [ ""; "-wal" ];
      try Unix.rmdir dir with
      | _ -> ())
    (fun () -> f db)
;;

(* Physical I/O of one statement: the write path has no [rows_examined], so page
   reads are how its access pattern is observed (the #508 trick). *)
let reads_during db sql =
  let n = ref 0 in
  Db.set_event_callback
    db
    (Some
       (function
         | Db.Event.Page_read _ | Db.Event.Wal_read _ -> incr n
         | _ -> ()));
  exec db sql;
  Db.set_event_callback db None;
  !n
;;

(* Run [sql] with the DML seek counters installed. *)
let exec_stats db sql =
  let st = Exec.make_dml_seek_stats () in
  (match run (Exec.with_dml_seek_stats st (fun () -> Db.execute db sql)) with
   | Ok () -> ()
   | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
  st
;;

let rows_of db sql =
  unwrap
    (run
       (let open Lwt.Syntax in
        let* r = Db.query db sql in
        match r with
        | Error e -> Lwt.return (Error e)
        | Ok stream ->
          let* rows = Lwt_stream.to_list stream in
          Lwt.return (Ok rows)))
;;

let ints db sql =
  List.map
    (function
      | [| Db.V_int n |] -> Int64.to_int n
      | _ -> Alcotest.fail "expected a single-int row")
    (rows_of db sql)
;;

let one_int db sql =
  match ints db sql with
  | [ n ] -> n
  | l -> Alcotest.failf "expected one row, got %d" (List.length l)
;;

(* [t(w, i, v)] with composite PRIMARY KEY (w, i): the #508 seek shape.  Rows
   are inserted with [i] DESCENDING inside each [w], so index-key order and
   rowid order disagree — which is what makes the drain-order guarantee worth
   pinning. *)
let seed db ~n_w ~n_i =
  exec db "CREATE TABLE t (w INTEGER, i INTEGER, v INTEGER, PRIMARY KEY (w, i))";
  exec db "BEGIN";
  for w = 1 to n_w do
    for i = n_i downto 1 do
      exec db (Printf.sprintf "INSERT INTO t VALUES (%d, %d, %d)" w i ((w * 100000) + i))
    done
  done;
  exec db "COMMIT"
;;

(* ------------------------------------------------------------------ *)
(* Candidate handling: what the seek walks, buffers, and reads          *)
(* ------------------------------------------------------------------ *)

(* Enough matches that a per-row regression is unmistakable in the counters. *)
let many = 3000

(* A large-but-not-round row count for the correctness cases below, chosen well
   above any plausible internal batch so a batching regression shows up. *)
let large = 1224

(* Every index entry in the prefix is walked, every one of them is read, and
   none is read before the walk finishes — [dss_peak_buffered = many] is the
   sort-then-fetch shape.  It dropping to 1 would mean the drain had gone back
   to fetching in index-key order; see
   [prefix_delete_reads_each_table_page_about_once] for what that costs. *)
let delete_seek_buffers_then_fetches ~open_db () =
  open_db (fun db ->
    seed db ~n_w:2 ~n_i:many;
    let st = exec_stats db "DELETE FROM t WHERE w = 1" in
    Alcotest.(check int) "candidates walked" many st.Exec.dss_candidates;
    Alcotest.(check int) "rows fetched" many st.Exec.dss_fetched;
    Alcotest.(check int)
      "all candidates buffered before any fetch"
      many
      st.Exec.dss_peak_buffered;
    Alcotest.(check int)
      "deleted all of w = 1"
      0
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 1");
    Alcotest.(check int)
      "left w = 2 alone"
      many
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 2"))
;;

let update_seek_buffers_then_fetches ~open_db () =
  open_db (fun db ->
    seed db ~n_w:2 ~n_i:many;
    let st = exec_stats db "UPDATE t SET v = 0 WHERE w = 2" in
    Alcotest.(check int) "candidates walked" many st.Exec.dss_candidates;
    Alcotest.(check int)
      "all candidates buffered before any fetch"
      many
      st.Exec.dss_peak_buffered;
    Alcotest.(check int)
      "all of w = 2 updated"
      many
      (one_int db "SELECT COUNT(*) FROM t WHERE v = 0");
    Alcotest.(check int)
      "w = 1 untouched"
      0
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 1 AND v = 0"))
;;

(* A single-row match walks and fetches exactly one candidate — the #512 shape,
   in counter form: the seek must not have degraded into a table scan. *)
let single_row_match_buffers_one () =
  with_db (fun db ->
    seed db ~n_w:2 ~n_i:50;
    let st = exec_stats db "DELETE FROM t WHERE w = 1 AND i = 25" in
    Alcotest.(check int) "one candidate" 1 st.Exec.dss_candidates;
    Alcotest.(check int) "one fetch" 1 st.Exec.dss_fetched;
    Alcotest.(check int) "peak buffered" 1 st.Exec.dss_peak_buffered;
    Alcotest.(check int) "99 rows left" 99 (one_int db "SELECT COUNT(*) FROM t"))
;;

(* A seek that matches nothing walks nothing.  Zeroed counters alone would not
   say that — a statement the planner sent to the scan branch reports 0/0/0 too
   — so this runs on disk and pins the access path physically: the no-match
   seek must touch far fewer pages than the same statement spelled so the
   planner cannot use the index. *)
let zero_row_match_walks_nothing () =
  with_file_db (fun db ->
    seed db ~n_w:2 ~n_i:2000;
    let st = exec_stats db "DELETE FROM t WHERE w = 1 AND i = 99999" in
    Alcotest.(check int) "no candidates" 0 st.Exec.dss_candidates;
    Alcotest.(check int) "no fetches" 0 st.Exec.dss_fetched;
    Alcotest.(check int) "nothing buffered" 0 st.Exec.dss_peak_buffered;
    Alcotest.(check int) "4000 rows left" 4000 (one_int db "SELECT COUNT(*) FROM t");
    let seek_reads = reads_during db "DELETE FROM t WHERE w = 1 AND i = 99998" in
    let scan_reads = reads_during db "DELETE FROM t WHERE w + 0 = 1 AND i + 0 = 99998" in
    Alcotest.(check bool)
      (Printf.sprintf "foil really scans (got %d reads)" scan_reads)
      true
      (scan_reads > 50);
    Alcotest.(check bool)
      (Printf.sprintf "seek beats the scan (%d vs %d reads)" seek_reads scan_reads)
      true
      (seek_reads * 4 < scan_reads);
    (* Same for a prefix that matches no index entry at all. *)
    let st = exec_stats db "UPDATE t SET v = 1 WHERE w = 77" in
    Alcotest.(check int) "no candidates" 0 st.Exec.dss_candidates;
    Alcotest.(check int)
      "nothing updated"
      0
      (one_int db "SELECT COUNT(*) FROM t WHERE v = 1"))
;;

(* ------------------------------------------------------------------ *)
(* Why the candidates are sorted before any row is read                 *)
(* ------------------------------------------------------------------ *)

(* [i] is a pseudo-random permutation rather than a descending run: reverse
   order is still sequential against the table tree and hides the effect
   entirely.  [7919] is coprime to [n], so [j -> j * 7919 mod n] is a
   permutation of [0, n).  Rows go in batched — the layout is identical to
   one-INSERT-per-row (rowid order is insertion order), but seeding 60 000 rows
   takes ~4s instead of minutes. *)
let seed_scrambled db ~n_w ~n =
  exec db "CREATE TABLE t (w INTEGER, i INTEGER, v INTEGER, PRIMARY KEY (w, i))";
  exec db "BEGIN";
  let buf = Buffer.create 8192 in
  for w = 1 to n_w do
    let j = ref 0 in
    while !j < n do
      Buffer.clear buf;
      Buffer.add_string buf "INSERT INTO t VALUES ";
      let stop = min n (!j + 200) in
      let first = ref true in
      while !j < stop do
        let i = (!j * 7919 mod n) + 1 in
        if not !first then Buffer.add_char buf ',';
        first := false;
        Buffer.add_string buf (Printf.sprintf "(%d,%d,%d)" w i ((w * 100000) + i));
        incr j
      done;
      exec db (Buffer.contents buf)
    done
  done;
  exec db "COMMIT"
;;

(* The measurement that decides the drain's design (#514 review).

   A wide prefix DELETE on disk, over a table far larger than the page cache,
   with index-key order scrambled against rowid order.  The drain sorts its
   candidates into rowid order before reading any row, so the table tree is
   walked ascending and each leaf is read about once.  Reading them in the order
   the index walk produces them instead is random access against the table, and
   the pager cache is a bounded FIFO, so it costs about a page read per row.

   The [4 000] bound below is the live copy of that measurement: it sits an
   order of magnitude above what a sorted drain reads on this workload and well
   below what an unsorted one does.  Both figures, and the chunked variants that
   were tried and rejected, are tabulated once in [Granary_sql.Exec]'s [rowid_buf]
   comment; they are deliberately not restated here, so that the assertion and
   the prose cannot drift apart.

   Two things about the shape are load-bearing and easy to get wrong: the
   scramble (a descending [i] is still sequential, and measures ~1 000 reads
   whichever way the drain fetches), and the size.  At 6 000 rows per prefix
   both orders measure the same — the pages the DELETE dirties are pinned in
   cache and the working set never turns over — so a smaller, faster version of
   this test would pass against a drain that had regressed completely.

   The [+ 0] foil calibrates: the same DELETE spelled so the planner cannot
   seek, showing the measurement is live rather than served from cache. *)
let prefix_delete_reads_each_table_page_about_once () =
  let n = 20_000 in
  with_file_db ~page_cache:64 (fun db ->
    seed_scrambled db ~n_w:3 ~n;
    let st = Exec.make_dml_seek_stats () in
    let seek_reads = ref 0 in
    Db.set_event_callback
      db
      (Some
         (function
           | Db.Event.Page_read _ | Db.Event.Wal_read _ -> incr seek_reads
           | _ -> ()));
    (match
       run
         (Exec.with_dml_seek_stats st (fun () ->
            Db.execute db "DELETE FROM t WHERE w = 1"))
     with
     | Ok () -> ()
     | Error e -> Alcotest.failf "seeked DELETE: %a" Db.pp_error e);
    Db.set_event_callback db None;
    Alcotest.(check int) "the whole prefix was walked" n st.Exec.dss_candidates;
    Alcotest.(check int) "every candidate row was read" n st.Exec.dss_fetched;
    Alcotest.(check int)
      "and the prefix is gone"
      (2 * n)
      (one_int db "SELECT COUNT(*) FROM t");
    let foil_reads = reads_during db "DELETE FROM t WHERE w + 0 = 2" in
    Alcotest.(check bool)
      (Printf.sprintf "measurement is live (foil read %d pages)" foil_reads)
      true
      (foil_reads > 100);
    Alcotest.(check bool)
      (Printf.sprintf
         "rowid-ordered fetch reads ~a page per leaf, not per row (%d reads for %d rows)"
         !seek_reads
         n)
      true
      (!seek_reads < 4000))
;;

(* ------------------------------------------------------------------ *)
(* Mutation during iteration                                            *)
(* ------------------------------------------------------------------ *)

(* The nastiest case: UPDATE the very column the seek walks.  The old row's
   index entry is removed and a new one inserted at a DIFFERENT key, so a naive
   "mutate as you walk" implementation would either revisit the moved row (it
   lands further along the same range) or skip its neighbour.

   Shifting [i] by +[n_i] inside one [w] keeps every new key inside the walked
   prefix range and strictly after the old one — precisely the shape that
   revisits a row.  Each row must be updated exactly once, so the final [i]
   values are the originals plus the offset, with no doubles. *)
let update_seeked_column_visits_each_row_once ~open_db () =
  open_db (fun db ->
    let n = 40 in
    seed db ~n_w:2 ~n_i:n;
    exec db (Printf.sprintf "UPDATE t SET i = i + %d WHERE w = 1" n);
    Alcotest.(check (list int))
      "each row moved exactly once"
      (List.init n (fun k -> k + n + 1))
      (List.sort compare (ints db "SELECT i FROM t WHERE w = 1"));
    Alcotest.(check int)
      "row count unchanged"
      n
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 1");
    Alcotest.(check int)
      "other warehouse untouched"
      n
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 2");
    (* And at scale, where a revisit is far likelier to show up. *)
    exec db "DROP TABLE t";
    let big = large + 200 in
    seed db ~n_w:1 ~n_i:big;
    exec db (Printf.sprintf "UPDATE t SET i = i + %d WHERE w = 1" big);
    Alcotest.(check int)
      "row count unchanged at scale"
      big
      (one_int db "SELECT COUNT(*) FROM t");
    Alcotest.(check int)
      "every row moved exactly once"
      big
      (one_int db (Printf.sprintf "SELECT COUNT(*) FROM t WHERE i > %d" big)))
;;

(* Moving the seeked column BACKWARDS (new key before the cursor) is the mirror
   image: a naive walk would skip rows it has yet to reach. *)
let update_seeked_column_backwards () =
  with_db (fun db ->
    let n = large + 50 in
    seed db ~n_w:2 ~n_i:n;
    (* i in [1..n] becomes i - n in [1-n .. 0], all before the walked range. *)
    exec db (Printf.sprintf "UPDATE t SET i = i - %d WHERE w = 2" n);
    Alcotest.(check int)
      "all rows moved"
      n
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 2 AND i <= 0");
    Alcotest.(check int)
      "none left behind"
      0
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 2 AND i > 0"))
;;

(* An UPDATE that moves rows OUT of the seeked prefix entirely. *)
let update_leading_seek_column () =
  with_db (fun db ->
    let n = large + 10 in
    seed db ~n_w:2 ~n_i:n;
    exec db "UPDATE t SET w = 3 WHERE w = 1";
    Alcotest.(check int) "moved out" 0 (one_int db "SELECT COUNT(*) FROM t WHERE w = 1");
    Alcotest.(check int) "arrived" n (one_int db "SELECT COUNT(*) FROM t WHERE w = 3");
    Alcotest.(check int) "total preserved" (2 * n) (one_int db "SELECT COUNT(*) FROM t"))
;;

(* A seeked DELETE inside an explicit transaction that is rolled back must leave
   nothing behind — the streaming walk runs inside the caller's RW txn, so a
   partially-applied delete would show up here. *)
let delete_in_rolled_back_txn () =
  with_db (fun db ->
    let n = large + 300 in
    seed db ~n_w:2 ~n_i:n;
    exec db "BEGIN";
    exec db "DELETE FROM t WHERE w = 1";
    Alcotest.(check int)
      "gone inside the txn"
      0
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 1");
    exec db "ROLLBACK";
    Alcotest.(check int)
      "all back after ROLLBACK"
      n
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 1");
    Alcotest.(check int) "total intact" (2 * n) (one_int db "SELECT COUNT(*) FROM t");
    (* The same statement committed, for contrast. *)
    exec db "BEGIN";
    exec db "DELETE FROM t WHERE w = 1";
    exec db "COMMIT";
    Alcotest.(check int) "committed delete sticks" n (one_int db "SELECT COUNT(*) FROM t"))
;;

(* Drain order is load-bearing: an unordered [DELETE … LIMIT n] over a prefix
   spanning several full keys must take the same n rows a table scan would, i.e.
   the lowest rowids.  Chunking must not leak index-key order into that (#512
   review).  Rows were inserted with i descending, so rowid order is i = n_i
   downward; a LIMIT 3 must therefore take the three HIGHEST i. *)
let limit_without_order_still_matches_scan_order () =
  with_db (fun db ->
    seed db ~n_w:1 ~n_i:10;
    exec db "DELETE FROM t WHERE w = 1 LIMIT 3";
    Alcotest.(check (list int))
      "the three lowest rowids went"
      [ 1; 2; 3; 4; 5; 6; 7 ]
      (List.sort compare (ints db "SELECT i FROM t")))
;;

(* ------------------------------------------------------------------ *)
(* Property: seeked DML == unoptimizable foil                           *)
(* ------------------------------------------------------------------ *)

(* [w + 0 = ?] is semantically identical to [w = ?] but the planner cannot match
   it to an index column, so it takes the filtered-scan path.  Applying the same
   DML through both spellings, to two identically seeded tables, must leave the
   same rows behind. *)
let survivors db table =
  ints db (Printf.sprintf "SELECT (w * 100000) + i FROM %s ORDER BY w, i" table)
;;

let seed_pair db ~n_w ~n_i =
  List.iter
    (fun name ->
       exec
         db
         (Printf.sprintf
            "CREATE TABLE %s (w INTEGER, i INTEGER, v INTEGER, PRIMARY KEY (w, i))"
            name);
       exec db "BEGIN";
       for w = 1 to n_w do
         for i = n_i downto 1 do
           exec
             db
             (Printf.sprintf
                "INSERT INTO %s VALUES (%d, %d, %d)"
                name
                w
                i
                ((w * 100000) + i))
         done
       done;
       exec db "COMMIT")
    [ "seeked"; "foil" ]
;;

let prop_seeked_delete_matches_foil =
  QCheck.Test.make
    ~count:120
    ~name:"seeked DELETE affects exactly the foil's row set"
    QCheck.(triple (int_range 0 4) (int_range 0 12) bool)
    (fun (w, i, with_i) ->
       with_db (fun db ->
         seed_pair db ~n_w:3 ~n_i:8;
         let pred tbl =
           if with_i
           then Printf.sprintf "DELETE FROM %s WHERE w = %d AND i = %d" tbl w i
           else Printf.sprintf "DELETE FROM %s WHERE w = %d" tbl w
         in
         let foil_pred =
           if with_i
           then Printf.sprintf "DELETE FROM foil WHERE w + 0 = %d AND i + 0 = %d" w i
           else Printf.sprintf "DELETE FROM foil WHERE w + 0 = %d" w
         in
         exec db (pred "seeked");
         exec db foil_pred;
         survivors db "seeked" = survivors db "foil"))
;;

let prop_seeked_update_matches_foil =
  QCheck.Test.make
    ~count:120
    ~name:"seeked UPDATE affects exactly the foil's row set"
    QCheck.(triple (int_range 0 4) (int_range 0 12) (int_range 1 3))
    (fun (w, i, shift) ->
       with_db (fun db ->
         seed_pair db ~n_w:3 ~n_i:8;
         (* Shifting [i] mutates the seeked column itself, so this also fuzzes
            the visit-once property against a path that cannot get it wrong. *)
         exec
           db
           (Printf.sprintf
              "UPDATE seeked SET i = i + %d, v = v + 1 WHERE w = %d AND i >= %d"
              (shift * 100)
              w
              i);
         exec
           db
           (Printf.sprintf
              "UPDATE foil SET i = i + %d, v = v + 1 WHERE w + 0 = %d AND i + 0 >= %d"
              (shift * 100)
              w
              i);
         let vals tbl = ints db (Printf.sprintf "SELECT v FROM %s ORDER BY w, i" tbl) in
         survivors db "seeked" = survivors db "foil" && vals "seeked" = vals "foil"))
;;

(* The drain's sort is hand-rolled (a Bigarray has no [Array.sort]), so it wants
   a property rather than the handful of fixed orders the cases above happen to
   produce.  [LIMIT k] takes a PREFIX of the drain, so it reads the sort out
   directly: the seeked path must delete the k lowest rowids among the matches,
   which is exactly what the foil's table-scan drain does by construction.  [i]
   goes in as a permutation so index-key order and rowid order disagree — the
   heap actually has to move things — and the survivor sets can only agree for
   every k if the sort is right. *)
let seed_permuted_pair db ~n =
  List.iter
    (fun name ->
       exec
         db
         (Printf.sprintf
            "CREATE TABLE %s (w INTEGER, i INTEGER, v INTEGER, PRIMARY KEY (w, i))"
            name);
       exec db "BEGIN";
       for w = 1 to 2 do
         for j = 0 to n - 1 do
           let i = (j * 7919 mod n) + 1 in
           exec
             db
             (Printf.sprintf
                "INSERT INTO %s VALUES (%d, %d, %d)"
                name
                w
                i
                ((w * 1000) + i))
         done
       done;
       exec db "COMMIT")
    [ "seeked"; "foil" ]
;;

let prop_seeked_delete_limit_drains_in_rowid_order =
  QCheck.Test.make
    ~count:60
    ~name:"seeked DELETE .. LIMIT k takes the same rows the scan foil does"
    QCheck.(pair (int_range 1 48) (int_range 0 49))
    (fun (n, k) ->
       with_db (fun db ->
         seed_permuted_pair db ~n;
         exec db (Printf.sprintf "DELETE FROM seeked WHERE w = 1 LIMIT %d" k);
         exec db (Printf.sprintf "DELETE FROM foil WHERE w + 0 = 1 LIMIT %d" k);
         survivors db "seeked" = survivors db "foil"))
;;

let () =
  Alcotest.run
    "bounded_drain_514"
    [ ( "candidate handling"
      , [ Alcotest.test_case
            "DELETE buffers its candidates, then fetches"
            `Quick
            (delete_seek_buffers_then_fetches ~open_db:with_db)
        ; Alcotest.test_case
            "UPDATE buffers its candidates, then fetches"
            `Quick
            (update_seek_buffers_then_fetches ~open_db:with_db)
        ; Alcotest.test_case
            "DELETE buffers its candidates, then fetches (file-backed)"
            `Quick
            (delete_seek_buffers_then_fetches ~open_db:(fun f -> with_file_db f))
        ; Alcotest.test_case
            "single-row match buffers one"
            `Quick
            single_row_match_buffers_one
        ; Alcotest.test_case
            "zero-row match walks nothing"
            `Quick
            zero_row_match_walks_nothing
        ] )
    ; ( "drain order"
      , [ Alcotest.test_case
            "a prefix DELETE reads each table page about once"
            (* ~7s: it seeds 60 000 rows, and both the scramble and the size are
               load-bearing (see the docstring), so it cannot be shrunk. *)
            `Slow
            prefix_delete_reads_each_table_page_about_once
        ] )
    ; ( "mutation during iteration"
      , [ Alcotest.test_case
            "UPDATE of the seeked column visits each row once"
            `Quick
            (update_seeked_column_visits_each_row_once ~open_db:with_db)
        ; Alcotest.test_case
            "UPDATE of the seeked column visits each row once (file-backed)"
            `Quick
            (update_seeked_column_visits_each_row_once ~open_db:(fun f -> with_file_db f))
        ; Alcotest.test_case
            "UPDATE moving the seeked column backwards"
            `Quick
            update_seeked_column_backwards
        ; Alcotest.test_case
            "UPDATE of the leading seek column"
            `Quick
            update_leading_seek_column
        ; Alcotest.test_case
            "DELETE in a rolled-back transaction"
            `Quick
            delete_in_rolled_back_txn
        ; Alcotest.test_case
            "LIMIT without ORDER BY keeps scan order"
            `Quick
            limit_without_order_still_matches_scan_order
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_seeked_delete_matches_foil
          ; prop_seeked_update_matches_foil
          ; prop_seeked_delete_limit_drains_in_rowid_order
          ] )
    ]
;;
