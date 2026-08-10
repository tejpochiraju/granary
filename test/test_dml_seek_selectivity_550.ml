(** #550: a DML (UPDATE/DELETE) seek over a NON-UNIQUE index's leading columns
    has no bound on how many rows it can match — unlike a UNIQUE index, whose
    worst case is one row per distinct key, a non-unique index's equality
    prefix can match an unbounded fraction of the table.  That is the
    [WHERE tenant_id = 1] shape the issue names, and it costs one random
    table-tree descent per matched row ([Granary_sql.Exec.rowid_buf]'s
    comment has the measurement) where the full scan it replaced would have
    read the same rows sequentially.

    [Planner.dml_seek_bail_out_at] gives the seek a runtime budget instead of
    a plan-time answer, because an equality prefix (unlike a #532 literal
    range) has no window the planner can read off the query: the guard is
    NEVER applied to a UNIQUE index (that case is #514's own territory, and
    stays exactly as pinned there), and it exempts a table below
    {!Granary_sql.Planner.build_side_seek_break_even_ratio} rows outright,
    because below that floor the budget would floor to 0 and bail out on the
    very first candidate regardless of selectivity.

    [Granary_sql.Exec.dml_seek_stats] is what makes "did it seek, or did it
    bail to a scan" observable from outside: a seek that ran to completion
    reports [dss_candidates = dss_fetched = dss_peak_buffered] equal to the
    match count (the #514 shape); a seek that bailed out reports
    [dss_fetched = dss_peak_buffered = 0] — the fetch phase never started —
    while [dss_candidates] stops exactly at the budget, since the entry that
    crosses it is never emitted.

    [staleness] below (the "guard tracks a growing table across a prepared
    statement's life" group) is a review finding on this same issue, not a
    duplicate of the groups above: [Db.prepare]'s plan is computed once and
    reused for [stmt]'s whole life, and an earlier revision computed the
    budget once, at THAT prepare, from the table's row count at that instant.
    A statement prepared while the table was small (or below the floor above,
    giving no guard at all) then reproduced #550's own O(n) pessimization for
    every later execution, however large the table grew — the fix
    ([Planner.dml_seek_bail_out_at] is now called by [Exec.seek_index_candidates]
    itself, once per execution, against a table_meta freshly re-read from the
    catalog) is pinned by running the SAME prepared [stmt] once before and once
    after the table crosses the floor. *)

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

(* [t(id, g, v)], [g] indexed but NOT unique.  [n_low] rows carry [g = 0] (the
   non-selective value) and [n_high] carry [g = 999] (the selective one), so
   one table serves both sides of the guard without changing the row count or
   index it consults. *)
let n_total = 3000
let n_high = 5
let n_low = n_total - n_high

let seed db =
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER, v INTEGER)";
  exec db "CREATE INDEX idx_g ON t (g)";
  exec db "BEGIN";
  for i = 1 to n_total do
    let g = if i <= n_low then 0 else 999 in
    exec db (Printf.sprintf "INSERT INTO t VALUES (%d, %d, %d)" i g i)
  done;
  exec db "COMMIT"
;;

(* [foil] carries the same rows as [t] under a name the planner has no index
   for, so a [g + 0 = ...] predicate against it can never be seeked — the
   non-vacuity check for the foil comparisons below. *)
let seed_foil db =
  exec db "CREATE TABLE foil (id INTEGER PRIMARY KEY, g INTEGER, v INTEGER)";
  exec db "BEGIN";
  for i = 1 to n_total do
    let g = if i <= n_low then 0 else 999 in
    exec db (Printf.sprintf "INSERT INTO foil VALUES (%d, %d, %d)" i g i)
  done;
  exec db "COMMIT"
;;

(* The budget the guard above should compute for [t]: [n_total] rows, well
   above the floor, so [n_total / build_side_seek_break_even_ratio]. Restated
   here rather than imported so a change to either number shows up as this
   test failing loudly instead of the assertions below silently drifting. *)
let expected_budget = n_total / 200
let () = assert (expected_budget > n_high && expected_budget < n_low)

(* A non-selective prefix on a non-unique index bails to a full scan: the
   fetch phase never starts, and the walk stops at the budget rather than
   running to [n_low]. *)
let non_selective_bails_to_scan () =
  with_db (fun db ->
    seed db;
    let st = exec_stats db "DELETE FROM t WHERE g = 0" in
    Alcotest.(check int)
      "walk stopped at the budget"
      expected_budget
      st.Exec.dss_candidates;
    Alcotest.(check int) "fetch phase never ran" 0 st.Exec.dss_fetched;
    (* [dss_peak_buffered] is [dss_candidates - dss_fetched]'s high-water mark
       (see [Exec.note_seek_candidate]): it reports what the abandoned walk
       put in flight before bailing, not what the fetch phase later drained,
       so it tracks [dss_candidates] here rather than dropping to 0. *)
    Alcotest.(check int)
      "backlog at the point of bail-out"
      expected_budget
      st.Exec.dss_peak_buffered;
    Alcotest.(check int)
      "all g = 0 rows deleted anyway"
      0
      (one_int db "SELECT COUNT(*) FROM t WHERE g = 0");
    Alcotest.(check int)
      "g = 999 rows untouched"
      n_high
      (one_int db "SELECT COUNT(*) FROM t WHERE g = 999"))
;;

(* Same statement, spelled so the planner cannot use the index: the fallback
   must affect exactly the rows the seek did. *)
let non_selective_matches_unoptimizable_foil () =
  with_db (fun db ->
    seed db;
    seed_foil db;
    exec db "DELETE FROM t WHERE g = 0";
    exec db "DELETE FROM foil WHERE g + 0 = 0";
    Alcotest.(check int)
      "same row count survives"
      (one_int db "SELECT COUNT(*) FROM foil")
      (one_int db "SELECT COUNT(*) FROM t");
    Alcotest.(check (list int))
      "same ids survive"
      (ints db "SELECT id FROM foil ORDER BY id")
      (ints db "SELECT id FROM t ORDER BY id"))
;;

(* A selective full-key prefix through the SAME non-unique index, on the SAME
   table, still seeks unconditionally: the guard is a budget the walk can stay
   under, not a blanket refusal of non-unique indexes. *)
let selective_still_seeks () =
  with_db (fun db ->
    seed db;
    let st = exec_stats db "UPDATE t SET v = -1 WHERE g = 999" in
    Alcotest.(check int) "one candidate per match" n_high st.Exec.dss_candidates;
    Alcotest.(check int) "one fetch per match" n_high st.Exec.dss_fetched;
    Alcotest.(check int) "all buffered before any fetch" n_high st.Exec.dss_peak_buffered;
    Alcotest.(check int)
      "exactly the g = 999 rows were updated"
      n_high
      (one_int db "SELECT COUNT(*) FROM t WHERE v = -1"))
;;

(* A UNIQUE index gets no budget at all, however unselective the prefix is —
   this is #514's own contract ([delete_seek_buffers_then_fetches]); pinned
   again here, on a table shaped like this file's rather than imported,
   because the point being pinned is "the gate is uniqueness, not size," and a
   shared fixture would hide that the two tables differ only in [idx_unique]. *)
let seed_unique db ~half =
  exec db "CREATE TABLE u (w INTEGER, i INTEGER, v INTEGER, PRIMARY KEY (w, i))";
  exec db "BEGIN";
  for i = 1 to half do
    exec db (Printf.sprintf "INSERT INTO u VALUES (1, %d, %d)" i i)
  done;
  for i = 1 to half do
    exec db (Printf.sprintf "INSERT INTO u VALUES (2, %d, %d)" i i)
  done;
  exec db "COMMIT"
;;

let unique_index_never_bails () =
  with_db (fun db ->
    let half = n_total / 2 in
    seed_unique db ~half;
    (* [w = 1] is a STRICT (non-full) prefix of the unique PK matching exactly
       half the table -- comfortably past any selectivity threshold the #550
       guard could plausibly use -- and it must still seek and buffer
       everything, because the index is unique. *)
    let st = exec_stats db "DELETE FROM u WHERE w = 1" in
    Alcotest.(check int) "candidates walked" half st.Exec.dss_candidates;
    Alcotest.(check int) "rows fetched" half st.Exec.dss_fetched;
    Alcotest.(check int) "all buffered before any fetch" half st.Exec.dss_peak_buffered;
    Alcotest.(check int)
      "w = 1 all gone"
      0
      (one_int db "SELECT COUNT(*) FROM u WHERE w = 1");
    Alcotest.(check int)
      "w = 2 untouched"
      half
      (one_int db "SELECT COUNT(*) FROM u WHERE w = 2"))
;;

(* A table below [build_side_seek_break_even_ratio] rows is exempt outright,
   even through a genuinely non-unique index and at 100% selectivity: below
   the floor the budget would round to 0 and bail on the very first
   candidate, which would silently turn every small-table DML seek into a
   scan for no measurable benefit. Mirrors
   [test_composite_seek_508.ml]'s [dml_seeks_through_a_secondary_index]. *)
let small_table_is_exempt () =
  with_db (fun db ->
    exec db "CREATE TABLE s (id INTEGER PRIMARY KEY, g INTEGER, v INTEGER)";
    exec db "CREATE INDEX idx_sg ON s (g)";
    exec db "BEGIN";
    let n = 60 in
    for i = 1 to n do
      exec db (Printf.sprintf "INSERT INTO s VALUES (%d, %d, %d)" i 0 i)
    done;
    exec db "COMMIT";
    let st = exec_stats db "DELETE FROM s WHERE g = 0" in
    Alcotest.(check int) "walked every match" n st.Exec.dss_candidates;
    Alcotest.(check int) "fetched every match" n st.Exec.dss_fetched;
    Alcotest.(check int) "buffered every match" n st.Exec.dss_peak_buffered;
    Alcotest.(check int) "table emptied" 0 (one_int db "SELECT COUNT(*) FROM s"))
;;

(* Run a prepared [stmt] with no parameters, under [Exec.dml_seek_stats].
   [Db.run] rather than [Db.execute] is the whole point of this group: it
   reuses the SAME compiled [Plan.op] the staleness bug baked a budget into. *)
let run_prepared_stats pr =
  let st = Exec.make_dml_seek_stats () in
  (match run (Exec.with_dml_seek_stats st (fun () -> Db.run pr ~params:[])) with
   | Ok _ -> ()
   | Error e -> Alcotest.failf "prepared run: %a" Db.pp_error e);
  st
;;

(* #550 review: the bail-out budget must be recomputed on every execution of a
   prepared statement, not baked in once at [Db.prepare] time from whatever
   the table's row count was then.

   [w] carries [n_floor] rows, all [g = 0] — comfortably below
   {!Granary_sql.Planner.build_side_seek_break_even_ratio} (200), so the guard
   is exempt and the FIRST run of the prepared [UPDATE ... WHERE g = 0] seeks
   and updates every row unconditionally, exactly like [small_table_is_exempt]
   above. [n_grow] more rows are then inserted, all [g = 1] — the [g = 0]
   match count never changes, but the TABLE does, past the floor and past the
   point where [n_floor] rows is itself more than [1/200] of it.

   Running the SAME prepared [stmt] again must now see the guard apply: with a
   STALE, prepare-time budget (bugged behaviour) it would still answer [None]
   and seek unconditionally a second time, exactly as the first run did, for
   the life of the statement. With a LIVE budget it answers
   [Some ((n_floor + n_grow) / 200)], strictly below [n_floor], and the second
   run must bail to a scan instead — the same observable shape
   [non_selective_bails_to_scan] pins for a single execution, here pinned
   across two executions of one [Db.stmt]. *)
let guard_recomputes_across_prepared_executions () =
  with_db (fun db ->
    let n_floor = 60 in
    let n_grow = 3000 in
    exec db "CREATE TABLE w (id INTEGER PRIMARY KEY, g INTEGER, v INTEGER)";
    exec db "CREATE INDEX idx_wg ON w (g)";
    exec db "BEGIN";
    for i = 1 to n_floor do
      exec db (Printf.sprintf "INSERT INTO w VALUES (%d, 0, %d)" i i)
    done;
    exec db "COMMIT";
    let pr =
      match run (Db.prepare db "UPDATE w SET v = v + 1 WHERE g = 0") with
      | Ok pr -> pr
      | Error e -> Alcotest.failf "prepare: %a" Db.pp_error e
    in
    let st_before = run_prepared_stats pr in
    Alcotest.(check int)
      "below the floor: seeks every g = 0 row unconditionally"
      n_floor
      st_before.Exec.dss_candidates;
    Alcotest.(check int)
      "below the floor: fetch phase ran"
      n_floor
      st_before.Exec.dss_fetched;
    exec db "BEGIN";
    for i = 1 to n_grow do
      exec db (Printf.sprintf "INSERT INTO w VALUES (%d, 1, %d)" (n_floor + i) i)
    done;
    exec db "COMMIT";
    let total = n_floor + n_grow in
    let expected_budget = total / 200 in
    assert (expected_budget < n_floor);
    let st_after = run_prepared_stats pr in
    Alcotest.(check int)
      "same prepared statement, grown table: walk now stops at the live budget"
      expected_budget
      st_after.Exec.dss_candidates;
    Alcotest.(check int)
      "same prepared statement, grown table: fetch phase never ran"
      0
      st_after.Exec.dss_fetched;
    (* Correctness backstop: whichever path each run took (unconditional seek
       the first time, a bailed-to-scan seek the second), every g = 0 row's
       [v] was incremented by exactly 2 (once per run: [v = i] seeded, so
       [v = i + 2] after both), and every g = 1 row is untouched ([v = i]
       still, where [id = n_floor + i]). *)
    Alcotest.(check int)
      "every g = 0 row updated by both runs"
      n_floor
      (one_int db "SELECT COUNT(*) FROM w WHERE g = 0 AND v = id + 2");
    Alcotest.(check int)
      "no g = 1 row touched"
      n_grow
      (one_int
         db
         (Printf.sprintf "SELECT COUNT(*) FROM w WHERE g = 1 AND v = id - %d" n_floor));
    run (Db.finalize pr))
;;

(* QCheck: whatever the split between the non-selective and selective values,
   and whatever the guard decides, the affected row set never diverges from
   the unoptimizable foil's. This is the correctness backstop for the whole
   file — the stats assertions above pin WHICH path ran, this pins that
   either path answers the same thing. *)
let prop_bail_out_matches_foil =
  QCheck.Test.make
    ~count:20
    ~name:"whichever path the guard takes, the affected rows agree with the foil"
    QCheck.(pair (int_range 0 3000) nat_small)
    (fun (n_matching, seed_extra) ->
       with_db (fun db ->
         exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER, v INTEGER)";
         exec db "CREATE INDEX idx_g ON t (g)";
         exec db "CREATE TABLE foil (id INTEGER PRIMARY KEY, g INTEGER, v INTEGER)";
         let n_other = 200 + (seed_extra mod 50) in
         exec db "BEGIN";
         for i = 1 to n_matching do
           exec db (Printf.sprintf "INSERT INTO t VALUES (%d, 1, %d)" i i);
           exec db (Printf.sprintf "INSERT INTO foil VALUES (%d, 1, %d)" i i)
         done;
         for i = 1 to n_other do
           exec db (Printf.sprintf "INSERT INTO t VALUES (%d, 2, %d)" (n_matching + i) i);
           exec
             db
             (Printf.sprintf "INSERT INTO foil VALUES (%d, 2, %d)" (n_matching + i) i)
         done;
         exec db "COMMIT";
         exec db "DELETE FROM t WHERE g = 1";
         exec db "DELETE FROM foil WHERE g + 0 = 1";
         ints db "SELECT id FROM foil ORDER BY id"
         = ints db "SELECT id FROM t ORDER BY id"))
;;

let () =
  Alcotest.run
    "dml_seek_selectivity_550"
    [ ( "guard"
      , [ Alcotest.test_case
            "non-selective prefix bails to a full scan"
            `Quick
            non_selective_bails_to_scan
        ; Alcotest.test_case
            "bailed-out DELETE matches the unoptimizable foil"
            `Quick
            non_selective_matches_unoptimizable_foil
        ; Alcotest.test_case
            "selective prefix through the same index still seeks"
            `Quick
            selective_still_seeks
        ; Alcotest.test_case
            "a UNIQUE index never bails, however unselective"
            `Quick
            unique_index_never_bails
        ; Alcotest.test_case
            "a table below the floor is exempt"
            `Quick
            small_table_is_exempt
        ] )
    ; ( "staleness"
      , [ Alcotest.test_case
            "the guard tracks a growing table across a prepared statement's life"
            `Quick
            guard_recomputes_across_prepared_executions
        ] )
    ; "property", List.map QCheck_alcotest.to_alcotest [ prop_bail_out_matches_foil ]
    ]
;;
