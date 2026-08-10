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
    ]
;;
