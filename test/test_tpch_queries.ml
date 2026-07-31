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
    ]
;;
