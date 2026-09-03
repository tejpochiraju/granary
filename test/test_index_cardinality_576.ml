(** #576 tier 1: CREATE INDEX on a non-unique column computes and persists
    the leading column's distinct-value count, piggybacked on the index's
    existing full-table population walk (no new scan). A UNIQUE index, and
    any index on a WITHOUT ROWID table, is exempt -- see the design doc's
    "Data model" section for why. *)

module Db = Granary.Db
module Cat = Granary_catalog.Catalog
module Schema = Granary.Schema

let run = Lwt_main.run

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

let idx_stats db name =
  match Schema.find_index (Db.schema db) ~name with
  | None -> Alcotest.failf "index %S not found" name
  | Some i -> i.Cat.idx_stats
;;

(* 100,000 rows, 50 distinct tenant_id values -> 2,000 rows/value. *)
let n_rows = 100_000
let n_distinct = 50

let seed_skewed db =
  exec db "CREATE TABLE t (tenant_id INTEGER, v INTEGER)";
  exec db "BEGIN";
  for i = 1 to n_rows do
    exec db (Printf.sprintf "INSERT INTO t VALUES (%d, %d)" (i mod n_distinct) i)
  done;
  exec db "COMMIT"
;;

let non_unique_index_gets_analyzed () =
  with_db (fun db ->
    seed_skewed db;
    exec db "CREATE INDEX idx_t_tenant ON t(tenant_id)";
    match idx_stats db "idx_t_tenant" with
    | None -> Alcotest.fail "expected idx_stats to be populated"
    | Some s ->
      Alcotest.(check int) "distinct_count" n_distinct s.Cat.distinct_count;
      Alcotest.(check int) "rows_at_analysis" n_rows s.Cat.rows_at_analysis)
;;

(* #576 tier 2 (corrected): a single-column index has no position AFTER
   its leading column, so it can never carry a range histogram --
   range_histograms is always [| None |], however skewed or uniform
   tenant_id is. distinct_count is untouched by this redesign. *)
let non_unique_single_column_index_never_gets_a_range_histogram () =
  with_db (fun db ->
    seed_skewed db;
    exec db "CREATE INDEX idx_t_tenant ON t(tenant_id)";
    match idx_stats db "idx_t_tenant" with
    | None -> Alcotest.fail "expected idx_stats to be populated"
    | Some { Cat.distinct_count; range_histograms; _ } ->
      Alcotest.(check int) "distinct_count" n_distinct distinct_count;
      Alcotest.(check int) "range_histograms length" 1 (Array.length range_histograms);
      Alcotest.(check bool) "slot 0 is None" true (range_histograms.(0) = None))
;;

let unique_index_is_exempt () =
  with_db (fun db ->
    exec db "CREATE TABLE u (k INTEGER, v INTEGER)";
    exec db "INSERT INTO u VALUES (1, 10), (2, 20), (3, 30)";
    exec db "CREATE UNIQUE INDEX idx_u_k ON u(k)";
    match idx_stats db "idx_u_k" with
    | None -> ()
    | Some _ -> Alcotest.fail "UNIQUE index must not carry idx_stats")
;;

let without_rowid_index_is_exempt () =
  with_db (fun db ->
    exec db "CREATE TABLE w (k INTEGER PRIMARY KEY, v INTEGER) WITHOUT ROWID";
    exec db "INSERT INTO w VALUES (1, 10)";
    exec db "INSERT INTO w VALUES (2, 20)";
    exec db "CREATE INDEX idx_w_v ON w(v)";
    match idx_stats db "idx_w_v" with
    | None -> ()
    | Some _ -> Alcotest.fail "WITHOUT ROWID table's index must not carry idx_stats")
;;

let expr_leading_column_is_exempt () =
  with_db (fun db ->
    exec db "CREATE TABLE t2 (a INTEGER, b TEXT)";
    exec db "INSERT INTO t2 VALUES (1, 'X'), (2, 'Y'), (3, 'x')";
    exec db "CREATE INDEX idx_expr ON t2(lower(b))";
    match idx_stats db "idx_expr" with
    | None -> ()
    | Some _ -> Alcotest.fail "expression-column index must not carry idx_stats")
;;

(* #576 final review: [execute_create_index]'s [Hashtbl] is capped at
   [Exec.index_stats_cardinality_cap] (100,000, kept private to exec.ml --
   this test hardcodes the same number rather than exposing a test-only
   parameter, per the review's own instruction not to add one) to bound the
   walk's peak retained memory. A near-unique column -- more distinct values
   than the cap -- must persist [idx_stats = None] rather than a stat
   computed from a partial, under-reported count: an under-reported
   [distinct_count] makes [estimate_rows_from_stats]'s estimate too SMALL,
   which is the ADMITTING direction (the unsafe one this cap exists to keep
   out of), so the safe fallback is no stat at all -- identical to an
   unanalyzed index. *)
let n_over_cap_rows = 100_005

let seed_over_cap db =
  exec db "CREATE TABLE big (v INTEGER)";
  exec db "BEGIN";
  for i = 1 to n_over_cap_rows do
    exec db (Printf.sprintf "INSERT INTO big VALUES (%d)" i)
  done;
  exec db "COMMIT"
;;

let cardinality_above_the_cap_falls_back_to_no_stat () =
  with_db (fun db ->
    seed_over_cap db;
    exec db "CREATE INDEX idx_big_v ON big(v)";
    match idx_stats db "idx_big_v" with
    | None -> ()
    | Some s ->
      Alcotest.failf
        "expected idx_stats = None once distinct values exceed the cardinality cap, got \
         distinct_count=%d"
        s.Cat.distinct_count)
;;

(* #576 tier 2: same over-cap seed as [cardinality_above_the_cap_falls_back_to_no_stat]
   -- above the cap, idx_stats is None entirely, so obviously no histogram
   either. This pins that the histogram code path doesn't somehow run on the
   partial, capped table. *)
let cardinality_above_the_cap_has_no_histogram_either () =
  with_db (fun db ->
    seed_over_cap db;
    exec db "CREATE INDEX idx_big_v2 ON big(v)";
    match idx_stats db "idx_big_v2" with
    | None -> ()
    | Some _ ->
      Alcotest.fail "expected idx_stats = None (and hence no histogram) above the cap")
;;

(* #576 tier 2 (corrected): a 3-column composite index gets a histogram at
   positions 1 and 2 (never position 0), independently shaped per column. *)
let n_composite_rows = 100_000

let seed_composite db =
  exec db "CREATE TABLE comp (a INTEGER, b INTEGER, c INTEGER)";
  exec db "BEGIN";
  for i = 1 to n_composite_rows do
    exec
      db
      (Printf.sprintf
         "INSERT INTO comp VALUES (%d, %d, %d)"
         (i mod 3)
         (i mod 40)
         (i mod 25))
  done;
  exec db "COMMIT"
;;

let composite_index_gets_histograms_at_non_leading_positions () =
  with_db (fun db ->
    seed_composite db;
    exec db "CREATE INDEX idx_comp_abc ON comp(a, b, c)";
    match idx_stats db "idx_comp_abc" with
    | None -> Alcotest.fail "expected idx_stats to be populated"
    | Some { Cat.range_histograms; _ } ->
      Alcotest.(check int) "range_histograms length" 3 (Array.length range_histograms);
      Alcotest.(check bool)
        "slot 0 (leading col a) is None"
        true
        (range_histograms.(0) = None);
      (match range_histograms.(1), range_histograms.(2) with
       | Some { Cat.boundaries = bb }, Some { Cat.boundaries = cb } ->
         Alcotest.(check bool) "slot 1 has boundaries" true (Array.length bb >= 2);
         Alcotest.(check bool) "slot 2 has boundaries" true (Array.length cb >= 2)
       | _ -> Alcotest.fail "expected both slot 1 and slot 2 to have histograms"))
;;

(* #576 tier 2 (corrected): a per-position cap hit degrades only that
   position -- a sibling low-cardinality column keeps its histogram, and
   distinct_count (column 0) is unaffected either way. [low_card]'s
   cardinality (40) is deliberately picked above
   [Exec.histogram_bucket_count] (20, [build_histogram]'s own floor for
   producing a histogram at all) so that this test isolates the CAP
   fallback from that unrelated floor -- [i mod 10] was tried first and
   produced no histogram regardless of the cap, since 10 distinct values
   never clears the bucket-count floor. *)
let n_cap_rows = 100_005

let seed_one_near_unique_column db =
  exec db "CREATE TABLE capt (a INTEGER, near_uniq INTEGER, low_card INTEGER)";
  exec db "BEGIN";
  for i = 1 to n_cap_rows do
    exec db (Printf.sprintf "INSERT INTO capt VALUES (%d, %d, %d)" (i mod 3) i (i mod 40))
  done;
  exec db "COMMIT"
;;

let per_position_cap_only_degrades_that_position () =
  with_db (fun db ->
    seed_one_near_unique_column db;
    exec db "CREATE INDEX idx_capt ON capt(a, near_uniq, low_card)";
    match idx_stats db "idx_capt" with
    | None ->
      Alcotest.fail "expected idx_stats to be populated (column 0 is low-cardinality)"
    | Some { Cat.distinct_count; range_histograms; _ } ->
      Alcotest.(check int) "distinct_count (column a)" 3 distinct_count;
      Alcotest.(check bool)
        "slot 1 (near_uniq, over cap) is None"
        true
        (range_histograms.(1) = None);
      (match range_histograms.(2) with
       | Some _ -> ()
       | None ->
         Alcotest.fail "slot 2 (low_card, under cap) should still have a histogram"))
;;

(* #576 tier 2 (corrected): an expression column at a non-leading position
   never gets a histogram, mirroring tier 1's leading-column exemption. *)
let expr_non_leading_column_is_exempt () =
  with_db (fun db ->
    exec db "CREATE TABLE exprt (a INTEGER, b TEXT)";
    exec db "BEGIN";
    for i = 1 to 1000 do
      exec db (Printf.sprintf "INSERT INTO exprt VALUES (%d, 'v%d')" (i mod 3) (i mod 30))
    done;
    exec db "COMMIT";
    exec db "CREATE INDEX idx_exprt ON exprt(a, lower(b))";
    match idx_stats db "idx_exprt" with
    | None -> Alcotest.fail "expected idx_stats to be populated"
    | Some { Cat.range_histograms; _ } ->
      Alcotest.(check bool)
        "slot 1 (expression column) is None"
        true
        (range_histograms.(1) = None))
;;

(* Regression for the [build_histogram] threshold-cascade bug: a single
   dominant value followed immediately (in sort order) by a run of distinct
   singleton values used to consume nearly every interior-boundary slot on
   those near-empty neighbors, because [next_threshold] advanced by a fixed
   [step] per crossing instead of catching up to the smallest multiple of
   [step] strictly greater than the running total. Column [b] here holds
   1,000 rows at value 0 and one row each at values 1..19 -- 20 distinct
   values, meeting [histogram_bucket_count]'s (20) floor for producing a
   histogram at all, with bucket count 20 giving step = 1019/20 = 50.
   Hand-traced (and confirmed via a standalone simulation of both the buggy
   and fixed threshold update): the buggy version emits a boundary for value
   0 and then AGAIN for nearly every one of the 19 singleton values right
   behind it (21 boundaries total, [0;0;1;2;...;19]), while the fix emits
   only one boundary for value 0 (its running total of 1,000 jumps
   [next_threshold] straight past 1,000 up to 1,050, which none of the
   singleton values reach) plus the mandatory last-key boundary -- 3
   boundaries total ([0;0;19]). *)
let n_cascade_dominant = 1000
let n_cascade_tail = 19

(* Mirrors [Exec.histogram_bucket_count] (20), hardcoded per this file's own
   precedent for [Exec.index_stats_cardinality_cap] above -- neither is
   exposed outside [exec.ml]. *)
let cascade_histogram_bucket_count = 20

let seed_cascade_skew db =
  exec db "CREATE TABLE cascade_t (a INTEGER, b INTEGER)";
  exec db "BEGIN";
  for i = 1 to n_cascade_dominant do
    exec db (Printf.sprintf "INSERT INTO cascade_t VALUES (%d, 0)" (i mod 3))
  done;
  for i = 1 to n_cascade_tail do
    exec db (Printf.sprintf "INSERT INTO cascade_t VALUES (%d, %d)" (i mod 3) i)
  done;
  exec db "COMMIT"
;;

let a_skewed_key_does_not_cascade_boundaries_onto_its_neighbors () =
  with_db (fun db ->
    seed_cascade_skew db;
    exec db "CREATE INDEX idx_cascade_ab ON cascade_t(a, b)";
    match idx_stats db "idx_cascade_ab" with
    | None -> Alcotest.fail "expected idx_stats to be populated"
    | Some { Cat.range_histograms; _ } ->
      (match range_histograms.(1) with
       | None -> Alcotest.fail "expected slot 1 (b) to have a histogram"
       | Some { Cat.boundaries } ->
         (* Buggy behaviour produces 21 boundaries (the dominant key plus a
            near-consecutive run for each of the 19 singleton neighbors);
            the fix produces 3 (first key, one interior boundary for the
            dominant key, last key). Assert well below the buggy count so
            this fails loudly if the cascade regresses, without hardcoding
            the exact fixed count in case unrelated boundary logic changes
            (e.g. deduplication of first_key/interior). *)
         Alcotest.(check bool)
           (Printf.sprintf
              "expected far fewer than %d boundaries (cascade bug reproduced), got %d"
              cascade_histogram_bucket_count
              (Array.length boundaries))
           true
           (Array.length boundaries < cascade_histogram_bucket_count)))
;;

(* #576 tier 2 review fix: a distinct regression guard from
   [a_skewed_key_does_not_cascade_boundaries_onto_its_neighbors] above,
   though it happens to share the same shape of seed data -- both scenarios
   need the LEXICOGRAPHICALLY SMALLEST distinct value in a non-leading
   column to carry >= total_rows / histogram_bucket_count rows on its own.
   That one guards the cascade bug (a fixed-step [next_threshold] re-firing
   on every near-empty neighbor key after the dominant one); this one guards
   [build_histogram] unconditionally prepending [first_key] while ALSO
   letting the interior-boundary loop re-emit that same first sorted key,
   which duplicates it at [boundaries.(0) = boundaries.(1)] -- a zero-width
   phantom bucket the cascade test's "boundaries.length < 20" check does not
   catch (a 2- or 3-element array both satisfy it). This test instead walks
   every adjacent pair and asserts none of them repeat, which is a general
   guard against ANY duplicate boundary, not just one at the start. *)
let n_first_boundary_dominant = 1000
let n_first_boundary_tail = 19

let seed_first_boundary_skew db =
  exec db "CREATE TABLE first_boundary_t (a INTEGER, b INTEGER)";
  exec db "BEGIN";
  for i = 1 to n_first_boundary_dominant do
    exec db (Printf.sprintf "INSERT INTO first_boundary_t VALUES (%d, 0)" (i mod 3))
  done;
  for i = 1 to n_first_boundary_tail do
    exec db (Printf.sprintf "INSERT INTO first_boundary_t VALUES (%d, %d)" (i mod 3) i)
  done;
  exec db "COMMIT"
;;

let the_smallest_key_does_not_duplicate_the_first_boundary () =
  with_db (fun db ->
    seed_first_boundary_skew db;
    exec db "CREATE INDEX idx_first_boundary_ab ON first_boundary_t(a, b)";
    match idx_stats db "idx_first_boundary_ab" with
    | None -> Alcotest.fail "expected idx_stats to be populated"
    | Some { Cat.range_histograms; _ } ->
      (match range_histograms.(1) with
       | None -> Alcotest.fail "expected slot 1 (b) to have a histogram"
       | Some { Cat.boundaries } ->
         Alcotest.(check bool)
           "boundaries has at least 2 entries"
           true
           (Array.length boundaries >= 2);
         for i = 1 to Array.length boundaries - 1 do
           Alcotest.(check bool)
             (Printf.sprintf
                "boundaries.(%d) = %S must differ from boundaries.(%d) = %S (no \
                 zero-width bucket)"
                (i - 1)
                boundaries.(i - 1)
                i
                boundaries.(i))
             true
             (String.compare boundaries.(i - 1) boundaries.(i) <> 0)
         done))
;;

(* #576 tier 2 review fix, round 2: the mirror-image bug to
   [the_smallest_key_does_not_duplicate_the_first_boundary] above, at the
   OTHER end of [build_histogram]'s [sorted] list. That fix excluded the
   first sorted key from the interior-boundary loop but left the loop
   walking all the way through the LAST sorted key, so a threshold crossing
   on the last key's own turn pushes it onto [interior] -- and it is then
   ALSO unconditionally appended as [last_key], duplicating
   [boundaries.(n-2) = boundaries.(n-1)].

   Concrete repro: 20 distinct singleton-count keys (total_rows = 20,
   histogram_bucket_count = 20, so step = 1). From the 2nd key onward,
   [running] and [next_threshold] both increment by 1 in lockstep, so every
   key from the 2nd through the 20th satisfies [running >= next_threshold]
   on its own turn -- including the 20th (last) key, which the interior cap
   (< 19) does not exclude by the time it is reached (18 entries
   accumulated from keys 2..19, still under the cap of 19). *)
let n_last_boundary_keys = 20

let seed_last_boundary_distinct db =
  exec db "CREATE TABLE last_boundary_t (a INTEGER, b INTEGER)";
  exec db "BEGIN";
  for i = 1 to n_last_boundary_keys do
    exec db (Printf.sprintf "INSERT INTO last_boundary_t VALUES (%d, %d)" (i mod 3) i)
  done;
  exec db "COMMIT"
;;

let the_largest_key_does_not_duplicate_the_last_boundary () =
  with_db (fun db ->
    seed_last_boundary_distinct db;
    exec db "CREATE INDEX idx_last_boundary_ab ON last_boundary_t(a, b)";
    match idx_stats db "idx_last_boundary_ab" with
    | None -> Alcotest.fail "expected idx_stats to be populated"
    | Some { Cat.range_histograms; _ } ->
      (match range_histograms.(1) with
       | None -> Alcotest.fail "expected slot 1 (b) to have a histogram"
       | Some { Cat.boundaries } ->
         Alcotest.(check bool)
           "boundaries has at least 2 entries"
           true
           (Array.length boundaries >= 2);
         for i = 1 to Array.length boundaries - 1 do
           Alcotest.(check bool)
             (Printf.sprintf
                "boundaries.(%d) = %S must differ from boundaries.(%d) = %S (no \
                 zero-width bucket)"
                (i - 1)
                boundaries.(i - 1)
                i
                boundaries.(i))
             true
             (String.compare boundaries.(i - 1) boundaries.(i) <> 0)
         done))
;;

let a_row_excluded_by_a_partial_index_where_is_not_counted () =
  with_db (fun db ->
    exec db "CREATE TABLE p (tenant_id INTEGER, active INTEGER)";
    exec db "BEGIN";
    for i = 1 to 100 do
      exec
        db
        (Printf.sprintf
           "INSERT INTO p VALUES (%d, %d)"
           (i mod 10)
           (if i <= 50 then 1 else 0))
    done;
    exec db "COMMIT";
    exec db "CREATE INDEX idx_p_tenant ON p(tenant_id) WHERE active = 1";
    match idx_stats db "idx_p_tenant" with
    | None -> Alcotest.fail "expected idx_stats to be populated"
    | Some s ->
      Alcotest.(check int)
        "rows_at_analysis excludes filtered-out rows"
        50
        s.Cat.rows_at_analysis)
;;

(* #576 tier 1: estimate_rows' new selectivity estimate for a non-unique
   equality-prefix DRIVING-side seek should let a nested-loop probe win where
   the old unbounded_rows estimate could never justify one.

   d: 100,000 rows, 500 distinct k values (200 rows/value) -- WHERE d.k = 7
   seeks a selective, non-unique prefix on the DRIVING (left) side. r: 300
   rows, WITH an index on the join column x -- best_probe can only ever
   propose a nested-loop probe against a table that has a matching index, so
   without idx_r_x the join is ALWAYS a HashJoin regardless of how selective
   d's seek is. Before this task, estimate_rows answers unbounded_rows for
   d's seek (the driving side), so probe_is_worth_it's
   "driving_rows <= nlj_min_driving_rows" guard is false, the right_rows/ratio
   fallback also fails because right_rows (300) is tiny, and the join is a
   HashJoin. After this task, driving_rows ~= 100_000/500 = 200, which is
   <= nlj_min_driving_rows (1000), so the join becomes a NestedLoopJoin(r). *)
let n_d_rows = 100_000
let n_d_distinct = 500
let n_r_rows = 300

let seed_driving_seek db =
  (* CREATE INDEX analyzes the table as of its own walk, so the table must be
     populated FIRST -- an index created over an empty table records no
     usable stats (0 rows means no distinct-value count to persist), which is
     exactly the population tests' own ordering above. *)
  exec db "CREATE TABLE d (k INTEGER, v INTEGER)";
  exec db "CREATE TABLE r (x INTEGER)";
  exec db "BEGIN";
  for i = 1 to n_d_rows do
    exec db (Printf.sprintf "INSERT INTO d VALUES (%d, %d)" (i mod n_d_distinct) i)
  done;
  for i = 1 to n_r_rows do
    exec db (Printf.sprintf "INSERT INTO r VALUES (%d)" i)
  done;
  exec db "COMMIT";
  exec db "CREATE INDEX idx_d_k ON d(k)";
  exec db "CREATE INDEX idx_r_x ON r(x)"
;;

let plan_text db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    let rows = run (Lwt_stream.to_list stream) in
    String.concat
      "\n"
      (List.map
         (fun r ->
            String.concat
              " "
              (Array.to_list
                 (Array.map
                    (function
                      | Db.V_text s -> s
                      | Db.V_int n -> Int64.to_string n
                      | Db.V_real f -> Printf.sprintf "%h" f
                      | Db.V_blob b -> Bytes.to_string b
                      | Db.V_null -> "NULL")
                    r)))
         rows)
;;

let contains ~needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec go i =
    i + nlen <= hlen && (String.sub haystack i nlen = needle || go (i + 1))
  in
  go 0
;;

let selective_driving_seek_wins_the_probe () =
  with_db (fun db ->
    seed_driving_seek db;
    let plan = plan_text db "EXPLAIN SELECT * FROM d JOIN r ON d.v = r.x WHERE d.k = 7" in
    Alcotest.(check bool)
      ("selective driving seek should choose NestedLoopJoin, got:\n" ^ plan)
      true
      (contains ~needle:"NestedLoopJoin" plan))
;;

(* #576 tier 1: build_side_seek_is_unambiguous should ADMIT a non-unique
   equality-prefix seek when idx_stats says it is selective enough, and still
   DECLINE it when the stat says it is not.

   t: 100,000 rows. table_seek_budget = table_rows_estimate /
   build_side_seek_break_even_ratio(200) = 500 (private constant, stated here
   rather than referenced). driver: enough rows to sit comfortably above
   nlj_min_driving_rows so the strategy is decided by the cost comparison, not
   the floor -- mirroring test_join_cost_model_576.ml's own setup. The join
   key (driver.x = t.v) carries no selectivity information itself; only the
   WHERE-pinned prefix on t does. *)
let n_t_rows = 100_000
let n_driver_rows = 1_200

let seed_build_side ~n_distinct db =
  (* CREATE INDEX analyzes the table as of its own walk, so the table must be
     populated FIRST -- an index created over an empty table records no
     usable stats, same ordering requirement as seed_driving_seek above. *)
  exec db "CREATE TABLE t (tenant_id INTEGER, v INTEGER)";
  exec db "CREATE TABLE driver (x INTEGER)";
  exec db "BEGIN";
  for i = 1 to n_t_rows do
    exec db (Printf.sprintf "INSERT INTO t VALUES (%d, %d)" (i mod n_distinct) i)
  done;
  for i = 1 to n_driver_rows do
    exec db (Printf.sprintf "INSERT INTO driver VALUES (%d)" i)
  done;
  exec db "COMMIT";
  exec db "CREATE INDEX idx_t_tenant ON t(tenant_id)"
;;

(* 500 distinct tenant_id -> estimate = 100_000/500 = 200 <= budget (500): ADMIT. *)
let selective_prefix_is_admitted () =
  with_db (fun db ->
    seed_build_side ~n_distinct:500 db;
    let plan =
      plan_text
        db
        "EXPLAIN SELECT * FROM driver JOIN t ON driver.x = t.v WHERE t.tenant_id = 5"
    in
    Alcotest.(check bool)
      ("selective prefix should seek t via IndexLookup, got:\n" ^ plan)
      true
      (contains ~needle:"IndexLookup(t)" plan))
;;

(* 2 distinct tenant_id -> estimate = 100_000/2 = 50_000 > budget (500): DECLINE. *)
let non_selective_prefix_still_declines () =
  with_db (fun db ->
    seed_build_side ~n_distinct:2 db;
    let plan =
      plan_text
        db
        "EXPLAIN SELECT * FROM driver JOIN t ON driver.x = t.v WHERE t.tenant_id = 1"
    in
    Alcotest.(check bool)
      ("non-selective prefix should still scan t via SeqScan, got:\n" ^ plan)
      true
      (contains ~needle:"SeqScan(t)" plan))
;;

(* #576 waste fix: a non-leading TEXT column can never carry a
   [Plan.range] bound (see [Planner.bounded_type]), so [execute_create_index]
   must not build a histogram for it -- even though it has plenty of distinct
   values and would clear [histogram_bucket_count] (20) if a histogram would
   otherwise have been built. tenant_id (position 0, numeric) is the leading
   column and always stays [None] regardless; email (position 1, TEXT) is the
   one this test targets. *)
let n_text_rows = 1000

let seed_text_col db =
  exec db "CREATE TABLE t3 (tenant_id INTEGER, email TEXT)";
  exec db "BEGIN";
  for i = 1 to n_text_rows do
    exec
      db
      (Printf.sprintf "INSERT INTO t3 VALUES (%d, 'user%d@example.com')" (i mod 3) i)
  done;
  exec db "COMMIT"
;;

let non_leading_text_column_never_gets_a_range_histogram () =
  with_db (fun db ->
    seed_text_col db;
    exec db "CREATE INDEX idx_t3_tenant_email ON t3(tenant_id, email)";
    match idx_stats db "idx_t3_tenant_email" with
    | None -> Alcotest.fail "expected idx_stats to be populated"
    | Some { Cat.range_histograms; _ } ->
      Alcotest.(check int) "range_histograms length" 2 (Array.length range_histograms);
      Alcotest.(check bool)
        "slot 0 (leading col tenant_id) is None"
        true
        (range_histograms.(0) = None);
      Alcotest.(check bool)
        "slot 1 (TEXT col email) is None despite high cardinality"
        true
        (range_histograms.(1) = None))
;;

let () =
  Alcotest.run
    "index_cardinality_576"
    [ ( "population"
      , [ "non-unique index is analyzed", `Quick, non_unique_index_gets_analyzed
        ; ( "non-unique single-column index never gets a range histogram"
          , `Quick
          , non_unique_single_column_index_never_gets_a_range_histogram )
        ; "UNIQUE index is exempt", `Quick, unique_index_is_exempt
        ; "WITHOUT ROWID index is exempt", `Quick, without_rowid_index_is_exempt
        ; ( "expression-column leading index is exempt"
          , `Quick
          , expr_leading_column_is_exempt )
        ; ( "partial index respects WHERE"
          , `Quick
          , a_row_excluded_by_a_partial_index_where_is_not_counted )
        ; ( "cardinality above the cap falls back to no stat"
          , `Quick
          , cardinality_above_the_cap_falls_back_to_no_stat )
        ; ( "cardinality above the cap has no histogram either"
          , `Quick
          , cardinality_above_the_cap_has_no_histogram_either )
        ; ( "composite index gets histograms at non-leading positions"
          , `Quick
          , composite_index_gets_histograms_at_non_leading_positions )
        ; ( "per-position cap only degrades that position"
          , `Quick
          , per_position_cap_only_degrades_that_position )
        ; ( "expression column at non-leading position is exempt"
          , `Quick
          , expr_non_leading_column_is_exempt )
        ; ( "a skewed key does not cascade boundaries onto its neighbors"
          , `Quick
          , a_skewed_key_does_not_cascade_boundaries_onto_its_neighbors )
        ; ( "the smallest key does not duplicate the first boundary"
          , `Quick
          , the_smallest_key_does_not_duplicate_the_first_boundary )
        ; ( "the largest key does not duplicate the last boundary"
          , `Quick
          , the_largest_key_does_not_duplicate_the_last_boundary )
        ; ( "non-leading TEXT column never gets a range histogram"
          , `Quick
          , non_leading_text_column_never_gets_a_range_histogram )
        ] )
    ; ( "estimate_rows"
      , [ ( "selective driving seek wins the probe"
          , `Quick
          , selective_driving_seek_wins_the_probe )
        ] )
    ; ( "build_side_seek_is_unambiguous"
      , [ "selective prefix is admitted", `Quick, selective_prefix_is_admitted
        ; ( "non-selective prefix still declines"
          , `Quick
          , non_selective_prefix_still_declines )
        ] )
    ]
;;
