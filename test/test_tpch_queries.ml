module Q = Granary_tpc.Tpch_queries

let test_exactly_22_queries () =
  Alcotest.(check int) "22 queries present" 22 (List.length Q.all)
;;

let test_numbered_one_to_22_in_order () =
  Alcotest.(check (list int))
    "numbers are 1..22 in order"
    (List.init 22 (fun i -> i + 1))
    (List.map (fun q -> q.Q.number) Q.all)
;;

let test_every_query_findable () =
  for i = 1 to 22 do
    match Q.find i with
    | Some q -> Alcotest.(check int) "find returns the right query" i q.Q.number
    | None -> Alcotest.failf "query %d not found" i
  done;
  Alcotest.(check bool) "out-of-range returns None" true (Q.find 23 = None)
;;

let test_runnable_queries_have_sql () =
  List.iter
    (fun q ->
       match q.Q.verdict with
       | Q.Skipped _ -> ()
       | Q.Native | Q.Rewritten _ | Q.Rewritten_pending _ ->
         Alcotest.(check bool)
           (Printf.sprintf "Q%d has non-empty SQL" q.Q.number)
           true
           (String.trim q.Q.sql <> ""))
    Q.all
;;

let test_skipped_queries_cite_an_issue () =
  List.iter
    (fun q ->
       match q.Q.verdict with
       | Q.Skipped reason ->
         Alcotest.(check bool)
           (Printf.sprintf "Q%d's skip reason cites a Forgejo issue" q.Q.number)
           true
           (String.contains reason '#')
       | _ -> ())
    Q.all
;;

let test_rewritten_queries_explain_themselves () =
  List.iter
    (fun q ->
       match q.Q.verdict with
       | Q.Rewritten why | Q.Rewritten_pending why ->
         Alcotest.(check bool)
           (Printf.sprintf "Q%d's rewrite has a rationale" q.Q.number)
           true
           (String.length (String.trim why) > 10)
       | _ -> ())
    Q.all
;;

(* #503: both callers of the drop — the runner and the smoke test — go through
   this one function, so the "the database is not discarded between queries"
   rationale lives in exactly one place. *)
let test_drop_setup_sql_covers_every_setup_view () =
  List.iter
    (fun q ->
       let drops = Q.drop_setup_sql q in
       let creates =
         List.filter
           (fun stmt ->
              let up = String.uppercase_ascii stmt in
              String.length up >= 11 && String.sub (String.trim up) 0 11 = "CREATE VIEW")
           q.Q.setup
       in
       Alcotest.(check int)
         (Printf.sprintf "Q%d drops one view per CREATE VIEW in setup" q.Q.number)
         (List.length creates)
         (List.length drops);
       List.iter
         (fun d ->
            Alcotest.(check bool)
              (Printf.sprintf "Q%d's drop is idempotent" q.Q.number)
              true
              (String.length d > 20 && String.sub d 0 20 = "DROP VIEW IF EXISTS "))
         drops)
    Q.all
;;

let test_q15_drops_its_revenue_view () =
  match Q.find 15 with
  | None -> Alcotest.fail "Q15 missing"
  | Some q ->
    Alcotest.(check (list string))
      "Q15's setup view is dropped by name"
      [ "DROP VIEW IF EXISTS revenue0" ]
      (Q.drop_setup_sql q)
;;

(* #504.5: setup text is normalized before the view name is read, so a
   carriage return in the statement cannot leave a view behind. *)
let test_drop_setup_sql_tolerates_carriage_returns () =
  let q =
    { Q.number = 0
    ; sql = "SELECT 1"
    ; setup = [ "CREATE\r\nVIEW\r\nv0 (a) AS SELECT 1" ]
    ; verdict = Q.Native
    }
  in
  Alcotest.(check (list string))
    "CRLF-separated CREATE VIEW still yields its drop"
    [ "DROP VIEW IF EXISTS v0" ]
    (Q.drop_setup_sql q)
;;

let test_non_view_setup_yields_no_drop () =
  let q =
    { Q.number = 0
    ; sql = "SELECT 1"
    ; setup = [ "CREATE INDEX i0 ON lineitem (l_shipdate)" ]
    ; verdict = Q.Native
    }
  in
  Alcotest.(check (list string))
    "a non-CREATE-VIEW setup statement is not dropped"
    []
    (Q.drop_setup_sql q)
;;

let () =
  Alcotest.run
    "tpch_queries"
    [ ( "catalogue"
      , [ Alcotest.test_case "22 queries" `Quick test_exactly_22_queries
        ; Alcotest.test_case "numbered in order" `Quick test_numbered_one_to_22_in_order
        ; Alcotest.test_case "findable" `Quick test_every_query_findable
        ] )
    ; ( "verdicts"
      , [ Alcotest.test_case "runnable have SQL" `Quick test_runnable_queries_have_sql
        ; Alcotest.test_case
            "skipped cite an issue"
            `Quick
            test_skipped_queries_cite_an_issue
        ; Alcotest.test_case
            "rewritten explain themselves"
            `Quick
            test_rewritten_queries_explain_themselves
        ] )
    ; ( "setup views"
      , [ Alcotest.test_case
            "one drop per setup view"
            `Quick
            test_drop_setup_sql_covers_every_setup_view
        ; Alcotest.test_case "Q15's view" `Quick test_q15_drops_its_revenue_view
        ; Alcotest.test_case
            "carriage returns"
            `Quick
            test_drop_setup_sql_tolerates_carriage_returns
        ; Alcotest.test_case "non-view setup" `Quick test_non_view_setup_yields_no_drop
        ] )
    ]
;;
