(* #482 — smoke tier: SF 0.001 through the real granary engine.  Asserts
   correctness and completion; never a latency bound. *)

module Q = Granary_tpc.Tpch_queries
module Granary_engine = Granary_tpc.Granary_engine
module Load = Granary_tpc.Tpch_schema.Load (Granary_engine)

(* [Filename.temp_dir] creates the directory itself, with no create-then-replace
   window of the [temp_file]/[unlink]/[mkdir] sequence it replaced. *)
let with_tmp_dir f =
  let dir = Filename.temp_dir "tpch-smoke" "" in
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

(* Runs both before setup and after the query — see {!Q.drop_setup_sql}, which
   is where the rationale lives and where the runner gets its drops too. *)
let drop_setup_views exec q = List.iter exec (Q.drop_setup_sql q)

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
