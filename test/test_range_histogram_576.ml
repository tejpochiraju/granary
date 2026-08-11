(** #576 tier 2 (corrected): [Planner.range_rows_estimate] must consult the
    histogram for the column a [Plan.range] ACTUALLY describes -- index
    column position [n_eq] (the first column after an [n_eq]-long
    equality-covered prefix), never column 0. The original version of this
    file targeted a range directly on an index's leading column, which
    [access_path_for_eqs] (planner.ml:578-607) can never construct: it
    refuses to pick any index at all when there is no equality-prefix
    conjunct, and every reachable [Plan.range] therefore sits at position
    [n_eq >= 1]. See
    docs/superpowers/specs/2026-08-11-576-tier2-per-position-histograms-design.md
    for the full reachability proof.

    This file uses the #532/#561 pattern the whole tier was motivated by: a
    2-column index, column 0 equality-bound in the query's WHERE clause, a
    literal range on column 1. The table with the composite index sits on
    the DRIVING (left) side of the join -- [Planner.build_side]'s range arm
    is gated to UNIQUE indexes only (see its doc comment), and a UNIQUE
    index never gets a histogram at all (it is exempt from
    [Exec.execute_create_index]'s walk), so the build/probe side is
    unreachable for this feature; [plan_base] (the driving side) carries no
    such gate and is this file's only route in. *)

module Db = Granary.Db
module Cat = Granary_catalog.Catalog
module Sema = Granary_sql.Sema
module Plan = Granary_sql.Plan
module Planner = Granary_sql.Planner

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
  match Cat.find_index (Db.catalog db) ~name with
  | None -> Alcotest.failf "index %S not found" name
  | Some i -> i.Cat.idx_stats
;;

let bind_and_plan cat sql =
  let ast =
    match
      let lexbuf = Lexing.from_string sql in
      Granary_sql.Parser.stmt_eof Granary_sql.Lexer.token lexbuf
    with
    | stmt -> stmt
    | exception Granary_sql.Parser.Error -> Alcotest.failf "parse %S: syntax error" sql
    | exception Failure msg -> Alcotest.failf "parse %S: %s" sql msg
  in
  match run (Sema.bind cat ast) with
  | Error _ -> Alcotest.failf "bind %S failed" sql
  | Ok b -> Planner.plan ~cat b
;;

(* [Op_hash_join]'s build side is its [right : op] field; [Op_nested_loop_join]'s
   driving side is its [left : op] field. Walking to the top join and
   classifying it as hash vs. nested-loop is what this file actually
   pins -- see the header for why the composite-index table must be the
   DRIVING (left) side for [range_rows_estimate]'s output to be reachable
   at all. *)
let rec top_join_shape (op : Plan.op) =
  match op with
  | Plan.Op_project { child; _ } -> top_join_shape child
  | Plan.Op_expr_project { child; _ } -> top_join_shape child
  | Plan.Op_filter { child; _ } -> top_join_shape child
  | Plan.Op_hash_join _ -> `Hash_join
  | Plan.Op_nested_loop_join _ -> `Nested_loop_join
  | _ -> `Other
;;

(* Column 0 (w) equality-bound, column 1 (v) skewed real, 20,000 rows. *)
let n_target = 20_000

let seed_target db =
  exec db "CREATE TABLE tgt (w INTEGER, v REAL, payload INTEGER)";
  exec db "BEGIN";
  for i = 0 to n_target - 1 do
    exec db (Printf.sprintf "INSERT INTO tgt VALUES (%d, %d.0, %d)" (i mod 2) i i)
  done;
  exec db "COMMIT"
;;

let n_driving = 1_200

let seed_driving db =
  exec db "CREATE TABLE drv (v REAL)";
  exec db "BEGIN";
  for i = 0 to n_driving - 1 do
    exec db (Printf.sprintf "INSERT INTO drv VALUES (%d.0)" i)
  done;
  exec db "COMMIT"
;;

let seed_and_index db =
  seed_target db;
  seed_driving db;
  exec db "CREATE INDEX idx_tgt_wv ON tgt(w, v)";
  exec db "CREATE INDEX idx_drv_v ON drv(v)"
;;

(* Population-side: the composite index gets a histogram at position 1 (v),
   never position 0 (w) -- direct catalog inspection, no query planning
   needed. This is the population half already covered more thoroughly by
   test_index_cardinality_576.ml's Task-3 tests; kept here too as a fixture
   sanity check for the tests below that DO plan queries against this exact
   table. *)
let histogram_is_populated_at_position_1_not_0 () =
  with_db (fun db ->
    seed_target db;
    exec db "CREATE INDEX idx_tgt_wv ON tgt(w, v)";
    match idx_stats db "idx_tgt_wv" with
    | None -> Alcotest.fail "expected idx_stats"
    | Some { Cat.range_histograms; _ } ->
      Alcotest.(check int) "array length" 2 (Array.length range_histograms);
      Alcotest.(check bool) "slot 0 is None" true (range_histograms.(0) = None);
      (match range_histograms.(1) with
       | Some { Cat.boundaries } ->
         Alcotest.(check bool) "slot 1 spans a range" true (Array.length boundaries >= 2)
       | None -> Alcotest.fail "expected slot 1 to have a histogram"))
;;

(* [tgt] (the composite-indexed table) is the DRIVING (left) side of the
   join, with [drv] joined second via [tgt.payload = drv.v]. A narrow
   window on [v] (spanning zero of its histogram buckets) keeps the
   nested-loop probe -- the driving-side estimate stays at the
   [range_seek_rows] floor; a wide window (spanning most of the histogram)
   flips to the hash join, since the histogram-driven estimate clears
   [Planner.nlj_min_driving_rows] where the pre-#576-tier-2 flat REAL-range
   fallback (a constant [range_seek_rows] = 100) never could. *)
let narrow_sql =
  "SELECT tgt.v FROM tgt JOIN drv ON tgt.payload = drv.v WHERE tgt.w = 0 AND tgt.v \
   BETWEEN 0.0 AND 199.0"
;;

let wide_sql =
  "SELECT tgt.v FROM tgt JOIN drv ON tgt.payload = drv.v WHERE tgt.w = 0 AND tgt.v \
   BETWEEN 0.0 AND 19999.0"
;;

let narrow_window_takes_the_nested_loop_probe () =
  with_db (fun db ->
    seed_and_index db;
    let cat = Db.catalog db in
    let op = bind_and_plan cat narrow_sql in
    match top_join_shape op with
    | `Nested_loop_join -> ()
    | `Hash_join -> Alcotest.fail "expected a nested-loop probe for the narrow window"
    | `Other -> Alcotest.fail "expected a join at the top of the plan")
;;

(* Non-vacuousness: forcing [range_histogram_estimate] to always answer
   [None] (simulating "no histogram consulted") flips this test's ADMIT
   decision back to a nested-loop probe -- proving this test fails without
   the fix, not just that it passes with it. Manual check performed while
   writing this file: temporarily changed [range_histogram_estimate]'s
   first line to `let _ = cat, meta, idx_tree, n_eq, r in None`, rebuilt,
   reran this file -- this test failed (the wide window fell back to the
   flat REAL-range estimate, [range_seek_rows] = 100, which is
   [<= nlj_min_driving_rows] and so kept the nested-loop probe for both
   windows). Reverted; the test passes again. See the task report for the
   exact before/after output. *)
let wide_window_flips_to_the_hash_join_via_the_histogram () =
  with_db (fun db ->
    seed_and_index db;
    let cat = Db.catalog db in
    let op = bind_and_plan cat wide_sql in
    match top_join_shape op with
    | `Hash_join -> ()
    | `Nested_loop_join ->
      Alcotest.fail
        "expected the wide window's histogram-driven estimate to clear the nested-loop \
         probe's break-even and take the hash join instead"
    | `Other -> Alcotest.fail "expected a join at the top of the plan")
;;

(* Parameter bound: same shape, but the range's upper bound is a parameter.
   Must be UNCHANGED from a run with no histogram at all -- [Planner.plan]
   runs once at prepare time, before any parameter is bound, so the
   histogram cannot know the runtime value. Pinned via the SAME wide window,
   which flips to the hash join when both bounds are literals but stays on
   the nested-loop probe when the upper bound is a parameter instead: with
   no literal to look up, [range_histogram_estimate] falls back whole to
   the flat [range_seek_rows] estimate, exactly as if no histogram
   existed. *)
(* Same wide window as [wide_sql], but spelled with bare integer literals
   (no decimal point) against the REAL column [v] -- the exact repro shape
   for the [lit_key] bug: the AST parses `0`/`19999` as [Ast.L_int], and
   without coercing to the column's declared type ([r.Plan.r_ty] = [Real])
   before encoding, an [IK_int]-encoded probe key always sorts below every
   [IK_real]-encoded histogram boundary (different leading tag byte -- see
   {!Planner.range_histogram_estimate}'s doc comment), landing at bucket 0
   regardless of the literal's value. That silently defeats the histogram
   and keeps the nested-loop probe even for this wide window, where the
   equivalent [.0]-literal query above correctly flips to the hash join. *)
let wide_sql_bare_int_literals =
  "SELECT tgt.v FROM tgt JOIN drv ON tgt.payload = drv.v WHERE tgt.w = 0 AND tgt.v \
   BETWEEN 0 AND 19999"
;;

let bare_int_literals_flip_to_the_hash_join_same_as_real_literals () =
  with_db (fun db ->
    seed_and_index db;
    let cat = Db.catalog db in
    let op = bind_and_plan cat wide_sql_bare_int_literals in
    match top_join_shape op with
    | `Hash_join -> ()
    | `Nested_loop_join ->
      Alcotest.fail
        "bare integer literals against the REAL column must be coerced to the column's \
         declared type before the histogram lookup, exactly like the .0-suffixed literal \
         spelling of the same window -- got a nested-loop probe instead of the expected \
         hash join"
    | `Other -> Alcotest.fail "expected a join at the top of the plan")
;;

let parameter_bound_range_is_unaffected () =
  with_db (fun db ->
    seed_and_index db;
    let cat = Db.catalog db in
    let param_sql =
      "SELECT tgt.v FROM tgt JOIN drv ON tgt.payload = drv.v WHERE tgt.w = 0 AND tgt.v \
       BETWEEN 0.0 AND ?"
    in
    let op = bind_and_plan cat param_sql in
    match top_join_shape op with
    | `Nested_loop_join -> ()
    | `Hash_join ->
      Alcotest.fail
        "a parameterized upper bound must not be admitted via the histogram -- the value \
         is unknown at plan time"
    | `Other -> Alcotest.fail "expected a join at the top of the plan")
;;

let () =
  Alcotest.run
    "range_histogram_576"
    [ ( "range_histogram"
      , [ Alcotest.test_case
            "histogram populated at position 1 not 0"
            `Quick
            histogram_is_populated_at_position_1_not_0
        ; Alcotest.test_case
            "narrow window stays a probe"
            `Quick
            narrow_window_takes_the_nested_loop_probe
        ; Alcotest.test_case
            "wide window flips to the hash join"
            `Quick
            wide_window_flips_to_the_hash_join_via_the_histogram
        ; Alcotest.test_case
            "bare int literals flip to the hash join, same as real literals"
            `Quick
            bare_int_literals_flip_to_the_hash_join_same_as_real_literals
        ; Alcotest.test_case
            "parameter bound range is unaffected"
            `Quick
            parameter_bound_range_is_unaffected
        ] )
    ]
;;
