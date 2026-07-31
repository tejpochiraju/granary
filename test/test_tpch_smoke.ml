(* #482 — smoke tier: SF 0.001 through the real granary engine.  Asserts
   correctness and completion; never a latency bound. *)

module Q = Granary_tpc.Tpch_queries
module Granary_engine = Granary_tpc.Granary_engine
module Load = Granary_tpc.Tpch_schema.Load (Granary_engine)

let with_tmp_dir f =
  let dir = Filename.temp_file "tpch-smoke" "" in
  Unix.unlink dir;
  Unix.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)
;;

let test_load_row_counts () =
  with_tmp_dir (fun dir ->
    let gen = Granary_tpc.Tpch_gen.create ~seed:42 ~sf:0.001 in
    let e = Granary_engine.open_db ~dir in
    Load.run e gen;
    List.iter
      (fun table ->
         let expected = Granary_tpc.Tpch_gen.row_count gen ~table in
         match
           Granary_engine.query_rows e (Printf.sprintf "SELECT COUNT(*) FROM %s" table)
         with
         | [ [ n ] ] ->
           Alcotest.(check int)
             (table ^ ": loaded row count matches generated")
             expected
             (int_of_string n)
         | _ -> Alcotest.failf "%s: COUNT(*) returned an unexpected shape" table)
      Granary_tpc.Tpch_gen.tables;
    Granary_engine.close e)
;;

let check_well_formed q rows =
  match rows with
  | [] -> () (* an empty result is legitimate for several queries at SF 0.001 *)
  | first :: _ ->
    let arity = List.length first in
    Alcotest.(check bool)
      (Printf.sprintf "Q%d projects at least one column" q.Q.number)
      true
      (arity > 0);
    List.iteri
      (fun i row ->
         Alcotest.(check int)
           (Printf.sprintf "Q%d row %d has the same arity as row 0" q.Q.number i)
           arity
           (List.length row))
      rows
;;

(* A query's [setup] creates views (Q15's revenue0) on a database that outlives
   the query, because reloading the data per query is not affordable.  The spec's
   trailing DROP VIEW was omitted on the assumption that the database is
   discarded between queries; that assumption does not hold here, so the harness
   drops the view itself — before setup, making setup re-runnable, and after the
   query, so no view leaks into a later one. *)
let drop_setup_views exec q =
  List.iter
    (fun stmt ->
       match Q.view_name_of_setup stmt with
       | Some v -> exec (Printf.sprintf "DROP VIEW IF EXISTS %s" v)
       | None -> ())
    q.Q.setup
;;

(* Unconditional since Task 8b: the verdicts in Tpch_queries are established by
   measurement, so every [Native] or [Rewritten] query must actually run. *)
let test_every_runnable_query_executes () =
  with_tmp_dir (fun dir ->
    let gen = Granary_tpc.Tpch_gen.create ~seed:42 ~sf:0.001 in
    let e = Granary_engine.open_db ~dir in
    Load.run e gen;
    let exec = Granary_engine.exec e in
    List.iter
      (fun q ->
         match q.Q.verdict with
         (* [Rewritten_pending] means "transformed, but granary still cannot
            run it" — asserting it executes would assert a known failure. *)
         | Q.Skipped _ | Q.Rewritten_pending _ -> ()
         | Q.Native | Q.Rewritten _ ->
           drop_setup_views exec q;
           List.iter exec q.Q.setup;
           check_well_formed q (Granary_engine.query_rows e q.Q.sql);
           drop_setup_views exec q)
      Q.all;
    Granary_engine.close e)
;;

let () =
  Alcotest.run
    "tpch_smoke"
    [ ( "load"
      , [ Alcotest.test_case "row counts match generator" `Slow test_load_row_counts ] )
    ; ( "queries"
      , [ Alcotest.test_case
            "every runnable query executes"
            `Slow
            test_every_runnable_query_executes
        ] )
    ]
;;
