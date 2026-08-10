(** #576 tier 1: CREATE INDEX on a non-unique column computes and persists
    the leading column's distinct-value count, piggybacked on the index's
    existing full-table population walk (no new scan). A UNIQUE index, and
    any index on a WITHOUT ROWID table, is exempt -- see the design doc's
    "Data model" section for why. *)

module Db = Granary.Db
module Cat = Granary_catalog.Catalog

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

let () =
  Alcotest.run
    "index_cardinality_576"
    [ ( "population"
      , [ "non-unique index is analyzed", `Quick, non_unique_index_gets_analyzed
        ; "UNIQUE index is exempt", `Quick, unique_index_is_exempt
        ; "WITHOUT ROWID index is exempt", `Quick, without_rowid_index_is_exempt
        ; ( "expression-column leading index is exempt"
          , `Quick
          , expr_leading_column_is_exempt )
        ; ( "partial index respects WHERE"
          , `Quick
          , a_row_excluded_by_a_partial_index_where_is_not_counted )
        ] )
    ; ( "estimate_rows"
      , [ ( "selective driving seek wins the probe"
          , `Quick
          , selective_driving_seek_wins_the_probe )
        ] )
    ]
;;
