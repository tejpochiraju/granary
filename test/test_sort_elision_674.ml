(** #674 (3 of 3) / #677: sort elision when an access path already satisfies
    ORDER BY.

    [plan_select]'s [make_sort] (`lib/sql/planner.ml`) used to insert an
    [Op_sort] node whenever a SELECT had an ORDER BY, even when the chosen
    access path ([Op_index_lookup] / [Op_rowid_lookup]) already walks its
    rows in exactly that order — the issue's own case is
    [SELECT no_o_id FROM new_order WHERE no_w_id = ? AND no_d_id = ? ORDER BY
    no_o_id LIMIT 1] against the composite index [(no_w_id, no_d_id,
    no_o_id)]: the seek already returns this district's rows in [no_o_id]
    order, so the [Op_sort] materialized all 900 rows for nothing, and — once
    #677 landed — defeated [Op_limit]'s early-stop by sitting between it and
    the scanner.

    Two halves, modeled on {!test_limit_early_stop_677}'s shape:

    - **plan-shape / correctness** — [EXPLAIN] must show no [Op_sort] node
      exactly when the design doc's eligibility rule says it should not, and
      row output must be correct (and identical to an unoptimizable [+ 0]
      foil) whether or not elision fired.
    - **the perf claim** — [Db.query_with_stats]'s [rows_examined] for the
      issue's own query stays near the [LIMIT], now that both #674 (this
      file) and #677 (already on [main], see [df3e5b4]) are in place. *)

module Db = Granary.Db

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

let insert_params db sql params =
  match
    run
      (let open Lwt.Syntax in
       let* st = Db.prepare db sql in
       match st with
       | Error e -> Lwt.return (Error e)
       | Ok st ->
         let* r = Db.run st ~params in
         (match r with
          | Ok _ -> Lwt.return (Ok ())
          | Error e -> Lwt.return (Error e)))
  with
  | Ok () -> ()
  | Error e -> Alcotest.failf "insert %S: %a" sql Db.pp_error e
;;

let render = function
  | Db.V_int n -> Int64.to_string n
  | Db.V_text s -> s
  | Db.V_real f -> Printf.sprintf "%h" f
  | Db.V_blob b -> Bytes.to_string b
  | Db.V_null -> "NULL"
;;

(* Order-SENSITIVE — do not sort. Several tests here exist to check that row
   order is (or is not) what an elided/un-elided sort should produce. *)
let rows_of db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun r -> Array.to_list (Array.map render r))
      (run (Lwt_stream.to_list stream))
;;

let stats_of db sql =
  match
    run
      (let open Lwt.Syntax in
       let* r = Db.query_with_stats db sql in
       match r with
       | Error e -> Lwt.return (Error e)
       | Ok (stream, stats) ->
         let* rows = Lwt_stream.to_list stream in
         Lwt.return (Ok (List.length rows, stats)))
  with
  | Ok v -> v
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
;;

(* EXPLAIN's rows, one plan-tree line each, joined so a substring check can
   find "Sort" (or fail to). *)
let plan_of db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    run (Lwt_stream.to_list stream)
    |> List.map (fun r -> String.concat " " (Array.to_list (Array.map render r)))
    |> String.concat "\n"
;;

let contains ~needle haystack =
  let nl = String.length needle
  and hl = String.length haystack in
  let rec go i = i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1)) in
  go 0
;;

let has_sort plan = contains ~needle:"Sort" plan

(* ------------------------------------------------------------------ *)
(* Schema: mirrors the issue's own new_order shape — composite key
   (w, d, o), seekable via the PRIMARY KEY's own index. *)
(* ------------------------------------------------------------------ *)

let n_o = 300

let seed_new_order db =
  exec
    db
    "CREATE TABLE new_order (no_w_id INTEGER, no_d_id INTEGER, no_o_id INTEGER, no_v \
     TEXT, PRIMARY KEY (no_w_id, no_d_id, no_o_id))";
  exec db "BEGIN";
  for w = 1 to 2 do
    for d = 1 to 2 do
      for o = 1 to n_o do
        exec db (Printf.sprintf "INSERT INTO new_order VALUES (%d, %d, %d, 'x')" w d o)
      done
    done
  done;
  exec db "COMMIT"
;;

(* ------------------------------------------------------------------ *)
(* Plan-shape + correctness                                             *)
(* ------------------------------------------------------------------ *)

(* The issue's own query: prefix-pinned composite key, ORDER BY on the
   trailing suffix column ASC, no NULLS clause — must elide. *)
let issue_case_elides () =
  with_db (fun db ->
    seed_new_order db;
    let sql =
      "SELECT no_o_id FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 ORDER BY no_o_id \
       LIMIT 1"
    in
    let plan = plan_of db ("EXPLAIN " ^ sql) in
    Alcotest.(check bool) "no Sort node in the plan" false (has_sort plan);
    Alcotest.(check (list (list string))) "min no_o_id" [ [ "1" ] ] (rows_of db sql))
;;

(* #517: a range bound on the first suffix column narrows the walk's span but
   not its order — elision must not be forfeited. *)
let range_bounded_variant_still_elides () =
  with_db (fun db ->
    seed_new_order db;
    let sql =
      "SELECT no_o_id FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 AND no_o_id > 250 \
       ORDER BY no_o_id"
    and foil =
      "SELECT no_o_id FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 AND no_o_id > 250 \
       ORDER BY no_o_id + 0"
    in
    let plan = plan_of db ("EXPLAIN " ^ sql) in
    Alcotest.(check bool) "no Sort node in the plan" false (has_sort plan);
    Alcotest.(check (list (list string)))
      "matches the unoptimizable + 0 foil, in order"
      (rows_of db foil)
      (rows_of db sql))
;;

(* NULLS LAST disagrees with the index's own NULL-sorts-first walk order and
   must not elide, even though direction and column both match. *)
let nulls_last_variant_does_not_elide () =
  with_db (fun db ->
    seed_new_order db;
    let sql =
      "SELECT no_o_id FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 ORDER BY no_o_id \
       ASC NULLS LAST"
    in
    let plan = plan_of db ("EXPLAIN " ^ sql) in
    Alcotest.(check bool) "Sort node present" true (has_sort plan);
    Alcotest.(check (list (list string)))
      "still correctly ascending"
      (rows_of
         db
         "SELECT no_o_id FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 ORDER BY \
          no_o_id")
      (rows_of db sql))
;;

(* DESC has no reverse index walk to elide onto (out of scope per the design
   doc) — must not elide, and must still sort correctly. *)
let desc_variant_does_not_elide () =
  with_db (fun db ->
    seed_new_order db;
    let sql =
      "SELECT no_o_id FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 ORDER BY no_o_id \
       DESC"
    in
    let plan = plan_of db ("EXPLAIN " ^ sql) in
    Alcotest.(check bool) "Sort node present" true (has_sort plan);
    let asc =
      rows_of
        db
        "SELECT no_o_id FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 ORDER BY \
         no_o_id ASC"
    in
    Alcotest.(check (list (list string)))
      "DESC is the reverse of ASC"
      (List.rev asc)
      (rows_of db sql))
;;

(* has_joins must gate elision off entirely, even when the ORDER BY names an
   otherwise-eligible column of the driving table. *)
let joined_variant_does_not_elide () =
  with_db (fun db ->
    seed_new_order db;
    exec db "CREATE TABLE flag (no_w_id INTEGER, f INTEGER)";
    exec db "INSERT INTO flag VALUES (1, 1)";
    let sql =
      "SELECT new_order.no_o_id FROM new_order JOIN flag ON flag.no_w_id = \
       new_order.no_w_id WHERE new_order.no_w_id = 1 AND new_order.no_d_id = 2 ORDER BY \
       new_order.no_o_id"
    in
    let plan = plan_of db ("EXPLAIN " ^ sql) in
    Alcotest.(check bool) "Sort node present" true (has_sort plan))
;;

(* ORDER BY naming more columns than the index's suffix has — the last key
   has nothing to fall back on and must not elide. *)
let order_by_exceeding_suffix_does_not_elide () =
  with_db (fun db ->
    seed_new_order db;
    let sql =
      "SELECT no_o_id FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 ORDER BY no_o_id, \
       no_v"
    in
    let plan = plan_of db ("EXPLAIN " ^ sql) in
    Alcotest.(check bool) "Sort node present" true (has_sort plan))
;;

(* ORDER BY on an expression, not a bare column — key_ok's [BE_col] match
   already refuses this; pin it explicitly (#579's class of mistake). *)
let expression_order_by_does_not_elide () =
  with_db (fun db ->
    seed_new_order db;
    let sql =
      "SELECT no_o_id FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 ORDER BY no_o_id \
       + 0"
    in
    let plan = plan_of db ("EXPLAIN " ^ sql) in
    Alcotest.(check bool) "Sort node present" true (has_sort plan))
;;

(* ------------------------------------------------------------------ *)
(* The NaN/NULL index-encoding hazard: a nullable REAL suffix column must
   never elide; a NOT NULL REAL suffix column must still be eligible. *)
(* ------------------------------------------------------------------ *)

(* Deliberately a plain (non-UNIQUE) index: a UNIQUE index over a nullable
   REAL column hits the pre-existing #578 NaN/NULL conflict-probe bug on
   INSERT, which is a different, already-tracked defect and not what this
   test is about. *)
let seed_hazard db =
  exec db "CREATE TABLE hazard (w INTEGER, d INTEGER, r REAL, id INTEGER)";
  exec db "CREATE INDEX idx_hazard ON hazard (w, d, r)";
  insert_params db "INSERT INTO hazard VALUES (1, 1, ?, 1)" [ Db.V_null ];
  insert_params db "INSERT INTO hazard VALUES (1, 1, ?, 2)" [ Db.V_real Float.nan ];
  insert_params db "INSERT INTO hazard VALUES (1, 1, ?, 3)" [ Db.V_real 2.0 ];
  insert_params db "INSERT INTO hazard VALUES (1, 1, ?, 4)" [ Db.V_real (-1.0) ]
;;

let nullable_real_suffix_does_not_elide () =
  with_db (fun db ->
    seed_hazard db;
    let sql = "SELECT id FROM hazard WHERE w = 1 AND d = 1 ORDER BY r" in
    let plan = plan_of db ("EXPLAIN " ^ sql) in
    Alcotest.(check bool) "Sort node present" true (has_sort plan);
    (* NULL sorts first (#495/compare_with_nulls' default NULLS FIRST); among
       the reals, NaN sorts below every number (#536); so: NULL, NaN, -1.0,
       2.0 — ids 1, 2, 4, 3. *)
    Alcotest.(check (list (list string)))
      "NULL, then NaN, then reals ascending"
      [ [ "1" ]; [ "2" ]; [ "4" ]; [ "3" ] ]
      (rows_of db sql))
;;

let seed_hazard_not_null db =
  exec db "CREATE TABLE hazard_nn (w INTEGER, d INTEGER, r REAL NOT NULL, id INTEGER)";
  exec db "CREATE INDEX idx_hazard_nn ON hazard_nn (w, d, r)";
  exec db "INSERT INTO hazard_nn VALUES (1, 1, 2.0, 1)";
  exec db "INSERT INTO hazard_nn VALUES (1, 1, -1.0, 2)";
  exec db "INSERT INTO hazard_nn VALUES (1, 1, 0.5, 3)"
;;

(* Pins that the restriction is scoped to NULLABLE REAL specifically, not a
   blanket "no REAL suffix column" rule — a NOT NULL REAL column has no NULL
   to collide with NaN on. *)
let not_null_real_suffix_elides () =
  with_db (fun db ->
    seed_hazard_not_null db;
    let sql = "SELECT id FROM hazard_nn WHERE w = 1 AND d = 1 ORDER BY r" in
    let plan = plan_of db ("EXPLAIN " ^ sql) in
    Alcotest.(check bool) "no Sort node in the plan" false (has_sort plan);
    Alcotest.(check (list (list string)))
      "ascending on r"
      [ [ "2" ]; [ "3" ]; [ "1" ] ]
      (rows_of db sql))
;;

(* ------------------------------------------------------------------ *)
(* The perf claim: rows_examined bounded near the LIMIT, not the district's
   row count. Both #674 (this file) and #677 (Op_limit early-stop, already
   on main) are needed for this to hold. *)
(* ------------------------------------------------------------------ *)

let rows_examined_bounded_near_limit () =
  with_db (fun db ->
    seed_new_order db;
    let n, stats =
      stats_of
        db
        "SELECT no_o_id FROM new_order WHERE no_w_id = 1 AND no_d_id = 2 ORDER BY \
         no_o_id LIMIT 1"
    in
    Alcotest.(check int) "rows returned" 1 n;
    Alcotest.(check bool)
      "rows_examined bounded near the limit, not the district's row count"
      true
      (stats.Db.rows_examined <= 5))
;;

(* ------------------------------------------------------------------ *)
(* Property: whether or not elision fires, the result must match the
   unoptimizable [+ 0] foil, in order — for both the elided direction (ASC)
   and the never-elided one (DESC). *)
(* ------------------------------------------------------------------ *)

let prop_db =
  lazy
    (let db = run (Db.open_in_memory ()) in
     seed_new_order db;
     db)
;;

let prop_order_matches_foil =
  QCheck.Test.make
    ~count:100
    ~name:"ORDER BY (elided or not) matches the +0 foil"
    QCheck.(quad (int_range 1 2) (int_range 1 2) (int_range 1 n_o) bool)
    (fun (w, d, lim, desc) ->
       let db = Lazy.force prop_db in
       let dir = if desc then "DESC" else "ASC" in
       let sql =
         Printf.sprintf
           "SELECT no_o_id FROM new_order WHERE no_w_id = %d AND no_d_id = %d ORDER BY \
            no_o_id %s LIMIT %d"
           w
           d
           dir
           lim
       in
       let foil =
         Printf.sprintf
           "SELECT no_o_id FROM new_order WHERE no_w_id = %d AND no_d_id = %d ORDER BY \
            no_o_id + 0 %s LIMIT %d"
           w
           d
           dir
           lim
       in
       rows_of db sql = rows_of db foil)
;;

let () =
  Alcotest.run
    "sort_elision_674"
    [ ( "plan_shape_and_correctness"
      , [ Alcotest.test_case "issue case elides" `Quick issue_case_elides
        ; Alcotest.test_case
            "range-bounded variant still elides"
            `Quick
            range_bounded_variant_still_elides
        ; Alcotest.test_case
            "NULLS LAST variant does not elide"
            `Quick
            nulls_last_variant_does_not_elide
        ; Alcotest.test_case
            "DESC variant does not elide"
            `Quick
            desc_variant_does_not_elide
        ; Alcotest.test_case
            "joined variant does not elide"
            `Quick
            joined_variant_does_not_elide
        ; Alcotest.test_case
            "ORDER BY exceeding suffix does not elide"
            `Quick
            order_by_exceeding_suffix_does_not_elide
        ; Alcotest.test_case
            "expression ORDER BY does not elide"
            `Quick
            expression_order_by_does_not_elide
        ; Alcotest.test_case
            "nullable REAL suffix does not elide"
            `Quick
            nullable_real_suffix_does_not_elide
        ; Alcotest.test_case
            "NOT NULL REAL suffix elides"
            `Quick
            not_null_real_suffix_elides
        ] )
    ; ( "perf_claim"
      , [ Alcotest.test_case
            "rows_examined bounded near the limit"
            `Quick
            rows_examined_bounded_near_limit
        ] )
    ; "property", [ QCheck_alcotest.to_alcotest prop_order_matches_foil ]
    ]
;;
