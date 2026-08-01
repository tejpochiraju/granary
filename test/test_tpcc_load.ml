module Load = Granary_tpc.Tpcc_schema.Load (Granary_tpc.Granary_engine)
module C = Granary_tpc.Tpcc_check

(* One warehouse is ~500k rows and takes a while to load; the columns and the
   consistency conditions are what this test is for, and they do not need more
   than one warehouse.

   The load is LOADED ONCE and SHARED by every case below. It is by far the
   most expensive thing in this executable — roughly forty seconds — and
   nothing here needs a pristine database: the two cases that deliberately
   break an invariant restore it before returning, which the last case then
   re-verifies. Loading per case simply multiplied the cost by the case
   count. Closed at exit rather than by [Fun.protect], since no single case
   owns the database's lifetime any more. *)
let loaded =
  lazy
    (let dir =
       Filename.concat
         (Filename.get_temp_dir_name ())
         (Printf.sprintf "test-tpcc-load-%d" (Unix.getpid ()))
     in
     (try Unix.mkdir dir 0o755 with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
     let e = Granary_tpc.Granary_engine.open_db ~dir in
     let gen = Granary_tpc.Tpcc_gen.create ~seed:42 ~warehouses:1 in
     Load.run e gen;
     e, gen)
;;

let () =
  at_exit (fun () ->
    if Lazy.is_val loaded
    then (
      let e, _ = Lazy.force loaded in
      Granary_tpc.Granary_engine.close e))
;;

let with_loaded f =
  let e, gen = Lazy.force loaded in
  f e gen
;;

(* Extracts the column names from a single `CREATE TABLE table (...)`
   statement, in order: split the body on commas at depth zero, take the
   first whitespace-delimited token of each clause, and skip clauses that
   open with PRIMARY KEY (a table-level constraint, not a column). Mirrors
   test/test_tpch_load.ml's approach for the TPC-H sibling. *)
let ddl_columns_for stmt =
  let open_paren = String.index stmt '(' in
  let close_paren = String.rindex stmt ')' in
  let body = String.sub stmt (open_paren + 1) (close_paren - open_paren - 1) in
  (* Split on commas at paren depth zero, so composite PRIMARY KEY (a, b, c)
     doesn't get chopped into extra clauses. *)
  let clauses =
    let buf = Buffer.create (String.length body) in
    let depth = ref 0 in
    let acc = ref [] in
    String.iter
      (fun c ->
         match c with
         | '(' ->
           incr depth;
           Buffer.add_char buf c
         | ')' ->
           decr depth;
           Buffer.add_char buf c
         | ',' when !depth = 0 ->
           acc := Buffer.contents buf :: !acc;
           Buffer.clear buf
         | c -> Buffer.add_char buf c)
      body;
    acc := Buffer.contents buf :: !acc;
    List.rev !acc
  in
  clauses
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> List.filter_map (fun clause ->
    let flat = String.map (fun c -> if c = '\n' || c = '\t' then ' ' else c) clause in
    let tokens = String.split_on_char ' ' flat |> List.filter (fun s -> s <> "") in
    match tokens with
    | "PRIMARY" :: "KEY" :: _ -> None
    | name :: _ -> Some name
    | [] -> None)
;;

let ddl_table_of stmt =
  let prefix = "CREATE TABLE " in
  let start = String.length prefix in
  let rest = String.sub stmt start (String.length stmt - start) in
  match String.index_opt rest ' ' with
  | Some sp -> String.sub rest 0 sp
  | None -> rest
;;

let test_columns_match_gen () =
  (* The DDL and the generator's column order must agree, or the load writes
     values into the wrong columns and every downstream number is nonsense. *)
  List.iter
    (fun table ->
       let from_gen = Granary_tpc.Tpcc_gen.column_names ~table in
       let ddl =
         List.find
           (fun s ->
              let needle = "CREATE TABLE " ^ table ^ " " in
              String.length s > String.length needle
              && String.equal (String.sub s 0 (String.length needle)) needle)
           Granary_tpc.Tpcc_schema.ddl
       in
       let ddl_table = ddl_table_of ddl in
       Alcotest.(check string) (table ^ ": DDL table name") table ddl_table;
       let ddl_cols = ddl_columns_for ddl in
       Alcotest.(check (list string))
         (table ^ ": DDL column order matches Tpcc_gen.column_names")
         from_gen
         ddl_cols)
    Granary_tpc.Tpcc_schema.tables
;;

let test_gen_and_schema_tables_agree () =
  (* Tpcc_gen.tables and Tpcc_schema.tables must be the exact same list — see
     Tpcc_schema.ml, which now derives its `tables` from Tpcc_gen.tables
     directly, so this also guards against a future edit reintroducing a
     second copy. *)
  Alcotest.(check (list string))
    "Tpcc_schema.tables = Tpcc_gen.tables"
    Granary_tpc.Tpcc_gen.tables
    Granary_tpc.Tpcc_schema.tables
;;

let test_row_counts_landed () =
  with_loaded (fun e gen ->
    List.iter
      (fun table ->
         let expected = Granary_tpc.Tpcc_gen.row_count gen ~table in
         let actual =
           match
             Granary_tpc.Granary_engine.query_rows
               e
               (Printf.sprintf "SELECT COUNT(*) FROM %s" table)
           with
           | [ [ n ] ] -> int_of_string n
           | _ -> Alcotest.fail (table ^ ": unexpected COUNT shape")
         in
         Alcotest.(check int) (table ^ ": rows loaded") expected actual)
      Granary_tpc.Tpcc_schema.tables)
;;

let test_initial_state_is_consistent () =
  with_loaded (fun e _gen ->
    List.iter
      (fun c ->
         let rows = List.map (Granary_tpc.Granary_engine.query_rows e) c.C.queries in
         match C.classify c ~rows with
         | C.Holds -> ()
         | C.Violated report -> Alcotest.fail report
         | C.Not_run -> Alcotest.fail "condition did not run")
      C.conditions)
;;

(* --- the conditions must be capable of FAILING ------------------------ *)

(* Every condition asserted above reports [Holds]. On its own that proves
   nothing — a check that cannot fail reports [Holds] over a corrupted
   database just as readily — so each condition is also driven against a
   database whose invariant has been DELIBERATELY broken, and must report
   [Violated].

   THAT IS NOT HYPOTHETICAL. These cases have caught the oracle passing
   vacuously twice, and the two failures had different mechanisms:

   1. Conditions 1 and 2 were once single queries under the zero-rows-means-
      pass convention, resting on a scalar correlated subquery in a WHERE
      clause, with their outer references written UNQUALIFIED. #485 made both
      subqueries evaluate to NULL, so `w_ytd <> NULL` was never true, no rows
      came back, and both reported [Holds] against a database this file had
      deliberately corrupted.

   2. Qualifying the references fixed that but left the same convention's
      other hole: `x <> (SELECT SUM/MAX ...)` is NULL, and so a pass, whenever
      the subquery matches ZERO ROWS. A warehouse with no district rows, a
      district with no orders and no new_order rows, a district that vanished
      from a GROUP BY over new_order — none of them was reported.

   Tpcc_check now has no subqueries at all. Every condition drives from the
   table that must have the row and joins the aggregates client-side over the
   UNION of both sides' keys, so a group that has vanished entirely is an
   explicit outcome rather than an absence of evidence. THAT is what the cases
   below guard — not a qualification rule, which no longer has anything to
   apply to. (The #485 rule remains correct and remains worth knowing for any
   future rewrite that reintroduces a subquery; it is simply no longer what
   protects this module.)

   The two mechanisms need two kinds of break, and the second is why the group
   below exists: perturbing a value in a row that STAYS PRESENT cannot reach a
   hole that only opens when a group disappears. So the invariants are broken
   both ways — by moving a number, and by deleting every row behind an
   aggregate.

   Every break is undone immediately afterwards, since these cases share the
   loaded database with every other case in this file, and each group's final
   case re-asserts that all four conditions hold again, so a restore that
   failed cannot pass unnoticed. *)

let classify_condition e number =
  let c = List.find (fun c -> c.C.number = number) C.conditions in
  C.classify c ~rows:(List.map (Granary_tpc.Granary_engine.query_rows e) c.C.queries)
;;

let check_violated e ~number ~broken_by =
  match classify_condition e number with
  | C.Violated _ -> ()
  | C.Holds ->
    Alcotest.failf
      "condition %d reported Holds after %s — the condition cannot detect its own \
       violation, so its Holds elsewhere proves nothing"
      number
      broken_by
  | C.Not_run -> Alcotest.failf "condition %d did not run" number
;;

let check_holds e ~number ~after =
  match classify_condition e number with
  | C.Holds -> ()
  | C.Violated report ->
    Alcotest.failf "condition %d still violated %s: %s" number after report
  | C.Not_run -> Alcotest.failf "condition %d did not run" number
;;

(* Breaks the invariant with [break], asserts the condition catches it,
   restores with [restore], and asserts it holds again. Restoring inside the
   same case keeps the shared database consistent for every later case
   regardless of Alcotest's ordering. *)
let violating e ~number ~broken_by ~break ~restore =
  Granary_tpc.Granary_engine.exec e break;
  Fun.protect
    ~finally:(fun () -> Granary_tpc.Granary_engine.exec e restore)
    (fun () -> check_violated e ~number ~broken_by);
  check_holds e ~number ~after:"after the break was undone"
;;

(* Runs [f] with [statements] applied inside a transaction that is ALWAYS
   rolled back, so the shared database is left bit-identical afterwards.

   This is how the DELETE cases below undo themselves. Re-inserting the
   deleted rows by hand would mean reconstructing every column of every row
   and would leave the restore itself untested; ROLLBACK restores exactly.
   Reads inside the window see the uncommitted deletes — verified, not
   assumed: the "holds again" assertion after each case would fail loudly if
   the writes were invisible (the condition would never have reported
   Violated) or if the rollback did not take. *)
let in_rolled_back_txn e statements f =
  Granary_tpc.Granary_engine.exec e "BEGIN";
  Fun.protect
    ~finally:(fun () -> Granary_tpc.Granary_engine.exec e "ROLLBACK")
    (fun () ->
       List.iter (Granary_tpc.Granary_engine.exec e) statements;
       f ())
;;

(* Same shape as [violating], but the break is a DELETE that removes the LAST
   ROW OF A GROUP rather than a perturbation of a row that stays present.

   That axis is the one the perturbation cases above cannot reach, and it is
   why the vacuous-pass hole survived them: with the conditions written as
   `x <> (SELECT SUM/MAX ...)`, an aggregate over zero rows is NULL, the
   predicate is NULL, the row is not returned — and zero rows is that
   convention's pass. Condition 3 had the same defect from the other end:
   grouping new_order by district produced no group for a district that had
   vanished from it, so the district was never examined. Every case below
   deletes an entire group and requires Violated. *)
let violating_by_delete e ~number ~broken_by ~deletes =
  in_rolled_back_txn e deletes (fun () -> check_violated e ~number ~broken_by);
  check_holds e ~number ~after:"after the deleted rows were rolled back"
;;

let test_condition1_detects_a_missing_district_group () =
  with_loaded (fun e _gen ->
    violating_by_delete
      e
      ~number:1
      ~broken_by:"every district row of warehouse 1 was deleted"
      ~deletes:[ "DELETE FROM district WHERE d_w_id = 1" ])
;;

let test_condition2_detects_a_missing_orders_group () =
  with_loaded (fun e _gen ->
    violating_by_delete
      e
      ~number:2
      ~broken_by:"every orders row of district (1,1) was deleted"
      ~deletes:[ "DELETE FROM orders WHERE o_w_id = 1 AND o_d_id = 1" ])
;;

(* The other half of condition 2's empty-group decision, asserted as a PASS.
   Delivery deletes new_order rows, so a fully delivered district legitimately
   has none; the max(no_o_id) disjunct is checked only when the district has
   new_order rows, while the max(o_id) disjunct still binds. If that skip were
   ever changed to a violation this case fails, which is what makes the
   decision explicit rather than incidental. Condition 3 makes the same
   judgement for the same district, so it is asserted here too. *)
let count_rows e sql =
  match Granary_tpc.Granary_engine.query_rows e sql with
  | [ [ n ] ] -> int_of_string n
  | _ -> Alcotest.failf "unexpected COUNT shape from %s" sql
;;

let district_1_1_new_orders =
  "SELECT COUNT(*) FROM new_order WHERE no_w_id = 1 AND no_d_id = 1"
;;

let test_a_drained_new_order_queue_is_legitimate () =
  with_loaded (fun e _gen ->
    let before = count_rows e district_1_1_new_orders in
    Alcotest.(check bool) "district (1,1) starts with a non-empty queue" true (before > 0);
    in_rolled_back_txn
      e
      [ "DELETE FROM new_order WHERE no_w_id = 1 AND no_d_id = 1" ]
      (fun () ->
         (* The deletes must actually be visible, or this case would assert
            Holds over an untouched database and prove nothing. *)
         Alcotest.(check int)
           "the queue is drained inside the window"
           0
           (count_rows e district_1_1_new_orders);
         check_holds e ~number:2 ~after:"with district (1,1)'s new-order queue drained";
         check_holds e ~number:3 ~after:"with district (1,1)'s new-order queue drained");
    (* Unlike the delete-a-group cases, this one asserts Holds on both sides of
       the window, and a permanently drained queue would leave the final
       whole-oracle re-check green too — so nothing here would notice a failed
       ROLLBACK. The row count is what makes the restore self-detecting. *)
    Alcotest.(check int)
      "the rolled-back deletes restored every new_order row"
      before
      (count_rows e district_1_1_new_orders);
    check_holds e ~number:2 ~after:"after the deleted rows were rolled back";
    check_holds e ~number:3 ~after:"after the deleted rows were rolled back")
;;

(* Condition 3 driven the other way: a new_order group whose district row is
   gone. Under the old new_order-driven GROUP BY there was nothing to notice —
   the group was still there and still contiguous. *)
let test_condition3_detects_a_missing_district_row () =
  with_loaded (fun e _gen ->
    violating_by_delete
      e
      ~number:3
      ~broken_by:"district (1,1) was deleted while its new_order rows remain"
      ~deletes:[ "DELETE FROM district WHERE d_w_id = 1 AND d_id = 1" ])
;;

let test_condition4_detects_a_missing_order_line_group () =
  with_loaded (fun e _gen ->
    violating_by_delete
      e
      ~number:4
      ~broken_by:"every order_line row of district (1,1) was deleted"
      ~deletes:[ "DELETE FROM order_line WHERE ol_w_id = 1 AND ol_d_id = 1" ])
;;

(* The case that needed condition 4 to grow a driving table. With both sides
   gone the district contributes no key to either aggregate, so a join of
   orders against order_line alone had nothing to compare and passed. *)
let test_condition4_detects_a_district_that_lost_both_sides () =
  with_loaded (fun e _gen ->
    violating_by_delete
      e
      ~number:4
      ~broken_by:"every orders AND order_line row of district (1,1) was deleted"
      ~deletes:
        [ "DELETE FROM order_line WHERE ol_w_id = 1 AND ol_d_id = 1"
        ; "DELETE FROM orders WHERE o_w_id = 1 AND o_d_id = 1"
        ])
;;

let test_condition1_detects_a_violation () =
  with_loaded (fun e _gen ->
    violating
      e
      ~number:1
      ~broken_by:"d_ytd was moved without w_ytd"
      ~break:"UPDATE district SET d_ytd = d_ytd + 1 WHERE d_w_id = 1 AND d_id = 1"
      ~restore:"UPDATE district SET d_ytd = d_ytd - 1 WHERE d_w_id = 1 AND d_id = 1")
;;

let test_condition2_detects_a_violation () =
  with_loaded (fun e _gen ->
    violating
      e
      ~number:2
      ~broken_by:"d_next_o_id was advanced without an order"
      ~break:
        "UPDATE district SET d_next_o_id = d_next_o_id + 1 WHERE d_w_id = 1 AND d_id = 1"
      ~restore:
        "UPDATE district SET d_next_o_id = d_next_o_id - 1 WHERE d_w_id = 1 AND d_id = 1")
;;

(* Condition 3 checks that a district's new_order ids are a contiguous run.
   Deleting the smallest or largest would keep max - min + 1 = count, so the
   row removed has to be an interior one. *)
let test_condition3_detects_a_violation () =
  with_loaded (fun e _gen ->
    violating
      e
      ~number:3
      ~broken_by:"an interior new_order row was deleted, breaking contiguity"
      ~break:"DELETE FROM new_order WHERE no_w_id = 1 AND no_d_id = 1 AND no_o_id = 2500"
      ~restore:"INSERT INTO new_order (no_o_id, no_d_id, no_w_id) VALUES (2500, 1, 1)")
;;

let test_condition4_detects_a_violation () =
  with_loaded (fun e _gen ->
    violating
      e
      ~number:4
      ~broken_by:"o_ol_cnt was raised without adding an order_line row"
      ~break:
        "UPDATE orders SET o_ol_cnt = o_ol_cnt + 1 WHERE o_w_id = 1 AND o_d_id = 1 AND \
         o_id = 1"
      ~restore:
        "UPDATE orders SET o_ol_cnt = o_ol_cnt - 1 WHERE o_w_id = 1 AND o_d_id = 1 AND \
         o_id = 1")
;;

(* Ordered last: every break above is undone by its own case, and this
   re-runs the whole oracle to prove the database the earlier cases shared is
   back exactly where it started. *)
let test_state_is_consistent_again () = test_initial_state_is_consistent ()

let () =
  Alcotest.run
    "tpcc_load"
    [ ( "schema"
      , [ Alcotest.test_case "columns match the generator" `Quick test_columns_match_gen
        ; Alcotest.test_case
            "Tpcc_gen.tables and Tpcc_schema.tables agree"
            `Quick
            test_gen_and_schema_tables_agree
        ] )
    ; ( "load"
      , [ Alcotest.test_case "row counts landed" `Slow test_row_counts_landed
        ; Alcotest.test_case
            "initial state is consistent"
            `Slow
            test_initial_state_is_consistent
        ] )
    ; ( "the conditions can fail"
      , [ Alcotest.test_case
            "condition 1 catches a d_ytd that moved alone"
            `Slow
            test_condition1_detects_a_violation
        ; Alcotest.test_case
            "condition 2 catches an advanced d_next_o_id"
            `Slow
            test_condition2_detects_a_violation
        ; Alcotest.test_case
            "condition 3 catches a hole in the new_order run"
            `Slow
            test_condition3_detects_a_violation
        ; Alcotest.test_case
            "condition 4 catches an o_ol_cnt with no order_line"
            `Slow
            test_condition4_detects_a_violation
        ; Alcotest.test_case
            "and the database is consistent again afterwards"
            `Slow
            test_state_is_consistent_again
        ] )
    ; ( "the conditions can fail when a whole group is deleted"
      , [ Alcotest.test_case
            "condition 1 catches a warehouse with no districts"
            `Slow
            test_condition1_detects_a_missing_district_group
        ; Alcotest.test_case
            "condition 2 catches a district with no orders"
            `Slow
            test_condition2_detects_a_missing_orders_group
        ; Alcotest.test_case
            "conditions 2 and 3 accept a drained new-order queue"
            `Slow
            test_a_drained_new_order_queue_is_legitimate
        ; Alcotest.test_case
            "condition 3 catches new_order rows with no district"
            `Slow
            test_condition3_detects_a_missing_district_row
        ; Alcotest.test_case
            "condition 4 catches a district with no order_line rows"
            `Slow
            test_condition4_detects_a_missing_order_line_group
        ; Alcotest.test_case
            "condition 4 catches a district that lost both sides"
            `Slow
            test_condition4_detects_a_district_that_lost_both_sides
        ; Alcotest.test_case
            "and the database is consistent again afterwards"
            `Slow
            test_state_is_consistent_again
        ] )
    ]
;;
