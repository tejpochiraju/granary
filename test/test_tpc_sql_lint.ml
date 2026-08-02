(* #510 — the lint that guards the #485 workaround, and the proof it would have
   caught both incidents that motivated it.

   The "would have caught it" cases below are the point of the file: a lint that
   only ever passes is indistinguishable from no lint, which is exactly the
   failure mode #485 produced twice in the harness it now watches. *)

module L = Granary_tpc.Tpc_sql_lint

let show findings =
  String.concat "; " (List.map (Format.asprintf "%a" L.pp_finding) findings)
;;

let columns_of findings = List.map (fun (f : L.finding) -> f.column) findings

(* --- the harness itself is clean -------------------------------------- *)

let test_harness_is_clean () =
  List.iter
    (fun (s : L.source) ->
       let findings = L.check s in
       if findings <> []
       then
         Alcotest.failf
           "%s has an unqualified outer column reference: %s"
           s.label
           (show findings))
    L.harness
;;

let test_harness_is_not_empty () =
  (* A lint over nothing passes trivially. 22 queries plus their setup views
     plus the four TPC-C conditions' queries is comfortably over 20. *)
  Alcotest.(check bool) "harness has sources" true (List.length L.harness > 20)
;;

(* A lint that passes because it never looked is the exact failure it exists to
   prevent, so prove it looks at the shipped text: strip the [table.] qualifiers
   out of the real harness SQL — the tidy-up that re-disables a check — and
   require that the lint then complains about it. *)

let starts_at s ~sub ~at =
  at + String.length sub <= String.length s
  && String.equal (String.sub s at (String.length sub)) sub
;;

let replace s ~sub ~by =
  let buf = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i < String.length s do
    if starts_at s ~sub ~at:!i
    then (
      Buffer.add_string buf by;
      i := !i + String.length sub)
    else (
      Buffer.add_char buf s.[!i];
      incr i)
  done;
  Buffer.contents buf
;;

let unqualify ~columns sql =
  let strip acc (column, table) = replace acc ~sub:(table ^ "." ^ column) ~by:column in
  List.fold_left strip sql columns
;;

let test_mutating_the_harness_is_caught () =
  let mutated =
    List.filter_map
      (fun (s : L.source) ->
         let sql = unqualify ~columns:s.columns s.sql in
         if String.equal sql s.sql then None else Some { s with sql })
      L.harness
  in
  Alcotest.(check bool)
    "some harness SQL carries qualified outer references"
    true
    (List.length mutated >= 5);
  List.iter
    (fun (s : L.source) ->
       if L.check s = []
       then
         Alcotest.failf
           "%s: unqualifying every outer reference produced no finding, so the lint is \
            not reading this query"
           s.label)
    mutated
;;

(* --- incident 1: TPC-H Q4 returned 0 rows instead of 5 ----------------- *)

let q4_unqualified =
  { L.label = "q4 as originally transcribed"
  ; columns = L.tpch_columns
  ; sql =
      {|SELECT o_orderpriority, COUNT(*) AS order_count
FROM orders
WHERE o_orderdate >= '1993-07-01'
  AND EXISTS (
        SELECT *
        FROM lineitem
        WHERE l_orderkey = o_orderkey
          AND l_commitdate < l_receiptdate)
GROUP BY o_orderpriority|}
  }
;;

let test_catches_q4 () =
  let findings = L.check q4_unqualified in
  Alcotest.(check (list string))
    "flags the bare o_orderkey"
    [ "o_orderkey" ]
    (columns_of findings);
  match findings with
  | [ f ] -> Alcotest.(check string) "names the outer table" "orders" f.table
  | _ -> Alcotest.fail "expected exactly one finding"
;;

let test_passes_q4_as_shipped () =
  let q4 = Option.get (Granary_tpc.Tpch_queries.find 4) in
  let fixed = { q4_unqualified with sql = q4.sql } in
  Alcotest.(check (list string)) "shipped Q4 is clean" [] (columns_of (L.check fixed))
;;

(* --- incident 2: TPC-C conditions 1 and 2 were vacuous ----------------- *)

(* The single-query shape the conditions had before #500's review. Both
   reported a clean database over any input, corrupted ones included. *)
let condition_1_vacuous =
  { L.label = "tpcc condition 1, single-query form"
  ; columns = L.tpcc_columns
  ; sql =
      {|SELECT w_id FROM warehouse
WHERE w_ytd <> (SELECT SUM(d_ytd) FROM district WHERE d_w_id = w_id)|}
  }
;;

let condition_2_vacuous =
  { L.label = "tpcc condition 2, single-query form"
  ; columns = L.tpcc_columns
  ; sql =
      {|SELECT d_w_id, d_id FROM district
WHERE d_next_o_id - 1 <> (SELECT MAX(o_id) FROM orders
                          WHERE o_w_id = d_w_id AND o_d_id = d_id)|}
  }
;;

let test_catches_condition_1 () =
  Alcotest.(check (list string))
    "flags the bare w_id"
    [ "w_id" ]
    (columns_of (L.check condition_1_vacuous))
;;

let test_catches_condition_2 () =
  Alcotest.(check (list string))
    "flags both bare district columns"
    [ "d_w_id"; "d_id" ]
    (columns_of (L.check condition_2_vacuous))
;;

(* --- it does not fire on the qualified form --------------------------- *)

let test_qualified_is_clean () =
  let s =
    { L.label = "condition 1 qualified"
    ; columns = L.tpcc_columns
    ; sql =
        {|SELECT w_id FROM warehouse
WHERE w_ytd <> (SELECT SUM(d_ytd) FROM district WHERE d_w_id = warehouse.w_id)|}
    }
  in
  Alcotest.(check (list string)) "no findings" [] (columns_of (L.check s))
;;

let test_uncorrelated_subquery_is_clean () =
  let s =
    { L.label = "uncorrelated"
    ; columns = L.tpch_columns
    ; sql =
        {|SELECT o_orderkey FROM orders
WHERE o_totalprice > (SELECT AVG(l_extendedprice) FROM lineitem)|}
    }
  in
  Alcotest.(check (list string)) "no findings" [] (columns_of (L.check s))
;;

let test_aliased_table_is_clean () =
  (* Q21's shape: the subquery's own table is aliased, and its bare-prefixed
     columns still belong to a table it selects from. *)
  let s =
    { L.label = "aliased self-join"
    ; columns = L.tpch_columns
    ; sql =
        {|SELECT s_name FROM supplier
INNER JOIN lineitem l1 ON s_suppkey = l1.l_suppkey
WHERE EXISTS (SELECT * FROM lineitem l2
              WHERE l2.l_orderkey = l1.l_orderkey AND l_suppkey <> l1.l_suppkey)|}
    }
  in
  Alcotest.(check (list string)) "no findings" [] (columns_of (L.check s))
;;

let test_string_literals_are_not_scanned () =
  (* A column name inside a literal must not be mistaken for a reference. *)
  let s =
    { L.label = "literal mentioning a column name"
    ; columns = L.tpch_columns
    ; sql =
        {|SELECT c_name FROM customer
WHERE EXISTS (SELECT * FROM orders WHERE o_comment = 'about c_name here')|}
    }
  in
  Alcotest.(check (list string)) "no findings" [] (columns_of (L.check s))
;;

let test_nested_two_deep () =
  (* A grandparent's column referenced bare from two levels down. *)
  let s =
    { L.label = "two levels of correlation"
    ; columns = L.tpch_columns
    ; sql =
        {|SELECT c_custkey FROM customer
WHERE EXISTS (SELECT * FROM orders
              WHERE o_custkey = customer.c_custkey
                AND EXISTS (SELECT * FROM lineitem WHERE l_orderkey = o_orderkey))|}
    }
  in
  Alcotest.(check (list string))
    "flags the grandparent-level reference"
    [ "o_orderkey" ]
    (columns_of (L.check s))
;;

let test_column_maps_are_populated () =
  Alcotest.(check bool) "tpch columns" true (List.length L.tpch_columns > 50);
  Alcotest.(check bool) "tpcc columns" true (List.length L.tpcc_columns > 50);
  Alcotest.(check (option string))
    "l_orderkey belongs to lineitem"
    (Some "lineitem")
    (List.assoc_opt "l_orderkey" L.tpch_columns);
  Alcotest.(check (option string))
    "no_o_id belongs to new_order"
    (Some "new_order")
    (List.assoc_opt "no_o_id" L.tpcc_columns)
;;

let () =
  Alcotest.run
    "tpc_sql_lint"
    [ ( "harness"
      , [ Alcotest.test_case "no unqualified outer refs" `Quick test_harness_is_clean
        ; Alcotest.test_case "covers real SQL" `Quick test_harness_is_not_empty
        ; Alcotest.test_case
            "unqualifying it is caught"
            `Quick
            test_mutating_the_harness_is_caught
        ] )
    ; ( "historical incidents"
      , [ Alcotest.test_case "tpch q4" `Quick test_catches_q4
        ; Alcotest.test_case "tpch q4 as shipped" `Quick test_passes_q4_as_shipped
        ; Alcotest.test_case "tpcc condition 1" `Quick test_catches_condition_1
        ; Alcotest.test_case "tpcc condition 2" `Quick test_catches_condition_2
        ] )
    ; ( "no false positives"
      , [ Alcotest.test_case "qualified" `Quick test_qualified_is_clean
        ; Alcotest.test_case "uncorrelated" `Quick test_uncorrelated_subquery_is_clean
        ; Alcotest.test_case "aliased table" `Quick test_aliased_table_is_clean
        ; Alcotest.test_case "string literals" `Quick test_string_literals_are_not_scanned
        ] )
    ; ( "structure"
      , [ Alcotest.test_case "two levels deep" `Quick test_nested_two_deep
        ; Alcotest.test_case "column maps" `Quick test_column_maps_are_populated
        ] )
    ]
;;
