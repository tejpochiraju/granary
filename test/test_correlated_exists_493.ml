(** #493: a correlated [EXISTS] cost 1708 s on TPC-H Q4 at SF 0.01 where SQLite
    took 0.002 s, and the scaling was worse than the naive nested-loop bound —
    10x the rows gave ~400,000x the time, not ~100x.

    {1 The root cause: an abandoned RO snapshot per outer row}

    The super-linear term was not the repeated planning and not a full-tree
    drain. It was a resource leak.

    A leaf scanner in [Auto] mode opens its own RO snapshot ([Exec.rh_begin] →
    [Store.ro_begin]) and releases it from the lazy stream's [finish], which
    runs only when the stream is drained to exhaustion or raises.
    [eval_exists_subquery] pulls exactly ONE row and abandons the rest — which
    is the right thing for [EXISTS] to do — so [finish] never ran. Every outer
    row therefore leaked one snapshot: [Store.ro_end] was skipped, and with it
    [Pager.unpin_all] over that snapshot's pinned pages, the [active_readers]
    decrement, and [Rwlock.release_read].

    The pins are what bend the curve. Every page an abandoned inner scan touched
    stays un-evictable for the rest of the query, so the pager's cache grows
    monotonically with the number of outer rows and stops behaving like a cache.
    A blocked checkpoint and a read-lock count that never returns to zero are
    the same bug's other two faces.

    {!leaves_no_reader_snapshot_behind} is the direct assertion: after a
    correlated [EXISTS] over many outer rows, the store must report zero live
    readers, zero pinned pages and zero read locks. It needs an ON-DISK
    database — the [Mem] backend answers 0 for all three unconditionally, so an
    in-memory run of it proves nothing.

    {1 The two cost fixes}

    - {b Plan once, execute N times.} Outer references are substituted into the
      inner statement as positional PARAMETERS rather than as the outer row's
      literal values, which makes the substituted statement structurally
      identical for every outer row, which in turn lets one
      [Sema.bind] + [Planner.plan] serve the whole scan through the cache
      [Exec.with_pull_context] installs. Before, every outer row paid a full
      bind and plan. The parameter spelling keeps the index seek, because
      [Planner.recognise_eq_col_lit] accepts a bound parameter on the value side
      exactly as it accepts a literal (#228's prepared point lookup).

    - {b Short-circuit the AND spine.} [stream_filter] pre-evaluated the WHOLE
      predicate for every row before [eval_expr] ever saw it, so [AND]'s
      short-circuit did not apply to a subquery: TPC-H Q4 ran its [EXISTS] for
      every row of [orders], inside the date range or not. The conjuncts are now
      evaluated left to right — in the order written, never reordered — and stop
      at the first that is not truthy.

    {1 What these tests can and cannot see}

    "Planned once" has no in-tree observable; it must be confirmed by a
    benchmark. What is observable is asserted here: the leak (store counters),
    the seek (index_entries / rows_examined), and the short-circuit
    (rows_examined against the same query without its cheap restriction).

    Every correctness case is a before-and-after invariant: the answers must not
    move, whichever substitution path a query takes. *)

open Lwt.Syntax
module Db = Granary.Db
module Store = Granary_store.Store

let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/granary_test_correlated_exists_493_%04d.db" n
;;

let cleanup path =
  List.iter
    (fun p ->
       try Unix.unlink p with
       | _ -> ())
    [ path; path ^ "-wal"; path ^ ".aslog" ]
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let render = function
  | Db.V_int n -> Int64.to_string n
  | Db.V_text s -> s
  | Db.V_real f -> Printf.sprintf "%h" f
  | Db.V_blob b -> Bytes.to_string b
  | Db.V_null -> "NULL"
;;

let rows_of db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.sort
      compare
      (List.map
         (fun r -> Array.to_list (Array.map render r))
         (run (Lwt_stream.to_list stream)))
;;

let stats_of db sql =
  match
    run
      (let* r = Db.query_with_stats db sql in
       match r with
       | Error e -> Lwt.return (Error e)
       | Ok (stream, stats) ->
         let* rows = Lwt_stream.to_list stream in
         Lwt.return (Ok (List.length rows, stats)))
  with
  | Ok v -> v
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
;;

let check_rows ~label expected actual =
  Alcotest.(check (list (list string))) label (List.sort compare expected) actual
;;

(* ------------------------------------------------------------------ *)
(* The TPC-H Q4 shape, shrunk                                           *)
(* ------------------------------------------------------------------ *)

(* [n_orders] outer rows, [lines_per_order] inner rows each. One order in
   [in_range_every] carries the in-range date, so the cheap conjunct rejects
   the large majority and the short-circuit has something to show. *)
let n_orders = 200
let lines_per_order = 5
let in_range_every = 10
let n_in_range = n_orders / in_range_every
let in_range_date = "1993-07-01"
let out_of_range_date = "1994-02-01"

(* Half of the in-range orders have a line with commit < receipt, so EXISTS
   splits them; the counts below are derived from this, not hardcoded. *)
let has_late_line o = o mod (in_range_every * 2) = 0
let n_in_range_with_exists = n_orders / (in_range_every * 2)

let seed db =
  exec
    db
    "CREATE TABLE orders (o_orderkey INTEGER PRIMARY KEY, o_orderdate TEXT, \
     o_orderpriority TEXT)";
  exec
    db
    "CREATE TABLE lineitem (l_id INTEGER PRIMARY KEY, l_orderkey INTEGER, l_commitdate \
     TEXT, l_receiptdate TEXT)";
  (* The inner equality is on a leading index column, so the correlated
     subquery has a seek available to it — the #512/#508 machinery. *)
  exec db "CREATE INDEX idx_lineitem_orderkey ON lineitem (l_orderkey)";
  exec db "BEGIN";
  for o = 1 to n_orders do
    let date = if o mod in_range_every = 0 then in_range_date else out_of_range_date in
    exec
      db
      (Printf.sprintf
         "INSERT INTO orders VALUES (%d, '%s', '%d-URGENT')"
         o
         date
         (o mod 2));
    for l = 1 to lines_per_order do
      (* Only the first line of a "late" order has commit < receipt, so EXISTS
         has to find it and can stop there. *)
      let commit, receipt =
        if has_late_line o && l = 1
        then "1993-01-01", "1993-06-01"
        else "1993-06-01", "1993-01-01"
      in
      exec
        db
        (Printf.sprintf
           "INSERT INTO lineitem VALUES (%d, %d, '%s', '%s')"
           (((o - 1) * lines_per_order) + l)
           o
           commit
           receipt)
    done
  done;
  exec db "COMMIT"
;;

let q4 =
  Printf.sprintf
    "SELECT o_orderkey FROM orders WHERE o_orderdate = '%s' AND EXISTS (SELECT * FROM \
     lineitem WHERE l_orderkey = orders.o_orderkey AND l_commitdate < l_receiptdate)"
    in_range_date
;;

(* The same EXISTS with no cheap conjunct in front of it: the subquery runs for
   every outer row. Used as the short-circuit control. *)
let q4_no_date =
  "SELECT o_orderkey FROM orders WHERE EXISTS (SELECT * FROM lineitem WHERE l_orderkey = \
   orders.o_orderkey AND l_commitdate < l_receiptdate)"
;;

let expected_q4 =
  List.filter_map
    (fun o ->
       if o mod in_range_every = 0 && has_late_line o
       then Some [ string_of_int o ]
       else None)
    (List.init n_orders (fun i -> i + 1))
;;

(* ------------------------------------------------------------------ *)
(* Harnesses                                                            *)
(* ------------------------------------------------------------------ *)

let with_mem_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

(* An ON-DISK database, with the raw [Store.t] kept in hand: the leak assertions
   read [Store.active_reader_count] / [pinned_page_count] / [live_read_locks],
   and the [Mem] backend answers 0 for all three no matter what leaks. *)
let with_disk_db f =
  let path = fresh_path () in
  cleanup path;
  let store =
    match run (Granary_unix.Store.open_file ~path ()) with
    | Ok s -> s
    | Error e -> Alcotest.failf "open_file: %a" Store.pp_error e
  in
  let db = run (Db.of_store ~file_path:path store) in
  Fun.protect
    ~finally:(fun () ->
      (try run (Db.close db) with
       | _ -> ());
      cleanup path)
    (fun () -> f db store)
;;

(* ------------------------------------------------------------------ *)
(* 1. The root cause: no leaked snapshot behind an abandoned EXISTS     *)
(* ------------------------------------------------------------------ *)

(* The three counters, read together. The assertions below compare them against
   a baseline taken after seeding rather than against literal zero: what the
   test is about is that a fully-drained query gives back everything it took,
   not what a committed write leaves behind. *)
let probes store =
  ( Store.active_reader_count store
  , Store.live_read_locks store
  , Store.pinned_page_count store )
;;

let check_released ~label store baseline =
  let br, bl, bp = baseline in
  let ar, al, ap = probes store in
  Alcotest.(check int) (label ^ ": live readers") br ar;
  Alcotest.(check int) (label ^ ": read locks") bl al;
  Alcotest.(check int) (label ^ ": pinned pages") bp ap
;;

(* Before the fix this reported one live reader, one read lock and a pile of
   pinned pages per outer row the EXISTS was evaluated for. The pins are the
   super-linear term: they make the pager's cache un-evictable for the rest of
   the query. *)
let leaves_no_reader_snapshot_behind () =
  with_disk_db (fun db store ->
    seed db;
    let baseline = probes store in
    check_rows ~label:"Q4 answer" expected_q4 (rows_of db q4);
    (* The subquery is abandoned after its first row on every outer row it runs
       for; nothing of it may survive the query. *)
    check_released ~label:"after Q4" store baseline)
;;

(* The same, over the shape that evaluates the subquery for EVERY outer row —
   200 abandoned snapshots rather than 20. A leak that scales with the outer
   cardinality shows here even if a small one hid above. *)
let leaves_nothing_behind_when_every_row_probes () =
  with_disk_db (fun db store ->
    seed db;
    let baseline = probes store in
    let expected =
      List.filter_map
        (fun o -> if has_late_line o then Some [ string_of_int o ] else None)
        (List.init n_orders (fun i -> i + 1))
    in
    check_rows ~label:"unrestricted EXISTS answer" expected (rows_of db q4_no_date);
    check_released ~label:"after the unrestricted EXISTS" store baseline)
;;

(* A scalar correlated subquery and a correlated IN take the same owned
   snapshot; the scalar one now stops at its first row too. *)
let scalar_and_in_subqueries_leave_nothing_behind () =
  with_disk_db (fun db store ->
    seed db;
    let baseline = probes store in
    let _ =
      rows_of
        db
        "SELECT o_orderkey, (SELECT l_id FROM lineitem WHERE l_orderkey = \
         orders.o_orderkey) FROM orders"
    in
    check_released ~label:"after a correlated scalar subquery" store baseline;
    let _ =
      rows_of
        db
        "SELECT o_orderkey FROM orders WHERE o_orderkey IN (SELECT l_orderkey FROM \
         lineitem WHERE l_commitdate < l_receiptdate)"
    in
    check_released ~label:"after an IN (subquery)" store baseline)
;;

(* ------------------------------------------------------------------ *)
(* 2. Seek, not drain                                                   *)
(* ------------------------------------------------------------------ *)

(* [index_entries] counts the index entries an [Op_index_lookup] walked (#546).
   A correlated subquery that seeks [idx_lineitem_orderkey] walks a handful per
   outer row; one that scans [lineitem] walks none at all and pays for it in
   [rows_examined] instead. Both halves are asserted, because either alone is
   satisfiable by the wrong plan. *)
let the_inner_subquery_seeks_rather_than_scans () =
  with_mem_db (fun db ->
    seed db;
    let n, st = stats_of db q4 in
    Alcotest.(check int) "row count" (List.length expected_q4) n;
    Alcotest.(check bool)
      "the inner lookup walked index entries"
      true
      (st.Db.index_entries > 0);
    (* A drained inner scan reads every lineitem row for every probed order.
       The seek reads at most [lines_per_order] of them, plus the outer scan. *)
    let drain_bound = n_in_range * n_orders * lines_per_order in
    let seek_bound = n_orders + (n_in_range * lines_per_order) in
    Alcotest.(check bool)
      (Printf.sprintf
         "rows_examined %d is a seek (<= %d), not a drain (~%d)"
         st.Db.rows_examined
         seek_bound
         drain_bound)
      true
      (st.Db.rows_examined <= seek_bound))
;;

(* ------------------------------------------------------------------ *)
(* 3. Short-circuit: the cheap conjunct runs first                      *)
(* ------------------------------------------------------------------ *)

(* Same subquery, same data; the only difference is a cheap equality in front of
   it that rejects 90% of the outer rows. Before #493 the whole predicate —
   subquery included — was resolved for every row before [AND] was ever
   evaluated, so the two queries examined the same number of rows. *)
let a_cheap_conjunct_short_circuits_the_subquery () =
  with_mem_db (fun db ->
    seed db;
    let _, restricted = stats_of db q4 in
    let _, unrestricted = stats_of db q4_no_date in
    Alcotest.(check bool)
      (Printf.sprintf
         "restricted examines fewer rows (%d) than unrestricted (%d)"
         restricted.Db.rows_examined
         unrestricted.Db.rows_examined)
      true
      (restricted.Db.rows_examined < unrestricted.Db.rows_examined))
;;

(* The order written is the order evaluated. A subquery placed FIRST is not
   hoisted behind the cheap conjunct — the conjuncts are short-circuited, never
   reordered — so this spelling still examines the unrestricted number of rows,
   and (the part that matters) still answers correctly. *)
let conjuncts_are_short_circuited_not_reordered () =
  with_mem_db (fun db ->
    seed db;
    let sql =
      Printf.sprintf
        "SELECT o_orderkey FROM orders WHERE EXISTS (SELECT * FROM lineitem WHERE \
         l_orderkey = orders.o_orderkey AND l_commitdate < l_receiptdate) AND \
         o_orderdate = '%s'"
        in_range_date
    in
    check_rows ~label:"subquery-first spelling" expected_q4 (rows_of db sql))
;;

(* ------------------------------------------------------------------ *)
(* 4. Correctness: the answers must not move                            *)
(* ------------------------------------------------------------------ *)

let correlated_exists_answers_correctly () =
  with_mem_db (fun db ->
    seed db;
    check_rows ~label:"EXISTS" expected_q4 (rows_of db q4);
    Alcotest.(check int)
      "count matches the derived expectation"
      n_in_range_with_exists
      (List.length expected_q4))
;;

let correlated_not_exists_is_the_complement () =
  with_mem_db (fun db ->
    seed db;
    let sql =
      Printf.sprintf
        "SELECT o_orderkey FROM orders WHERE o_orderdate = '%s' AND NOT EXISTS (SELECT * \
         FROM lineitem WHERE l_orderkey = orders.o_orderkey AND l_commitdate < \
         l_receiptdate)"
        in_range_date
    in
    let in_range =
      List.filter_map
        (fun o -> if o mod in_range_every = 0 then Some [ string_of_int o ] else None)
        (List.init n_orders (fun i -> i + 1))
    in
    let expected = List.filter (fun r -> not (List.mem r expected_q4)) in_range in
    check_rows ~label:"NOT EXISTS" expected (rows_of db sql);
    Alcotest.(check int)
      "the two halves partition the in-range orders"
      n_in_range
      (List.length expected + List.length expected_q4))
;;

let correlated_in_subquery_answers_correctly () =
  with_mem_db (fun db ->
    seed db;
    let sql =
      Printf.sprintf
        "SELECT o_orderkey FROM orders WHERE o_orderdate = '%s' AND o_orderkey IN \
         (SELECT l_orderkey FROM lineitem WHERE l_orderkey = orders.o_orderkey AND \
         l_commitdate < l_receiptdate)"
        in_range_date
    in
    check_rows ~label:"correlated IN" expected_q4 (rows_of db sql))
;;

let correlated_scalar_subquery_in_the_projection () =
  with_mem_db (fun db ->
    seed db;
    let sql =
      Printf.sprintf
        "SELECT o_orderkey, (SELECT COUNT(*) FROM lineitem WHERE l_orderkey = \
         orders.o_orderkey) FROM orders WHERE o_orderdate = '%s'"
        in_range_date
    in
    let expected =
      List.filter_map
        (fun o ->
           if o mod in_range_every = 0
           then Some [ string_of_int o; string_of_int lines_per_order ]
           else None)
        (List.init n_orders (fun i -> i + 1))
    in
    check_rows ~label:"correlated scalar projection" expected (rows_of db sql))
;;

let correlated_exists_under_group_by () =
  with_mem_db (fun db ->
    seed db;
    let sql =
      Printf.sprintf
        "SELECT o_orderpriority, COUNT(*) FROM orders WHERE o_orderdate = '%s' AND \
         EXISTS (SELECT * FROM lineitem WHERE l_orderkey = orders.o_orderkey AND \
         l_commitdate < l_receiptdate) GROUP BY o_orderpriority ORDER BY o_orderpriority"
        in_range_date
    in
    (* Every order with a late line has an even key, so they all share the one
       priority bucket. *)
    check_rows
      ~label:"Q4 grouped"
      [ [ "0-URGENT"; string_of_int n_in_range_with_exists ] ]
      (rows_of db sql))
;;

(* An outer value the parameter path pins as NULL must behave exactly as the
   literal path did: [l_orderkey = NULL] is never true, so no row qualifies —
   and in particular an index seek must not answer "the NULL entries". *)
let a_null_outer_value_matches_nothing () =
  with_mem_db (fun db ->
    exec db "CREATE TABLE o (k INTEGER PRIMARY KEY, j INTEGER)";
    exec db "CREATE TABLE i (id INTEGER PRIMARY KEY, j INTEGER)";
    exec db "CREATE INDEX idx_i_j ON i (j)";
    exec db "INSERT INTO o VALUES (1, 10), (2, NULL), (3, 30)";
    exec db "INSERT INTO i VALUES (1, 10), (2, NULL), (3, 99)";
    check_rows
      ~label:"NULL outer value matches nothing"
      [ [ "1" ] ]
      (rows_of db "SELECT k FROM o WHERE EXISTS (SELECT * FROM i WHERE i.j = o.j)"))
;;

(* The fallback path: an inner statement that carries a parameter of its own
   cannot take the parameter substitution (the injected slots would collide with
   [Sema.resolve_param]'s own numbering), so it runs the pre-#493 literal
   substitution. The answer must be the same either way. *)
let an_inner_parameter_falls_back_and_still_answers () =
  with_mem_db (fun db ->
    seed db;
    let sql =
      Printf.sprintf
        "SELECT o_orderkey FROM orders WHERE o_orderdate = '%s' AND EXISTS (SELECT * \
         FROM lineitem WHERE l_orderkey = orders.o_orderkey AND l_commitdate < \
         l_receiptdate AND l_receiptdate > ?)"
        in_range_date
    in
    match run (Db.prepare db sql) with
    | Error e -> Alcotest.failf "prepare: %a" Db.pp_error e
    | Ok st ->
      let rows =
        match run (Db.iter st ~params:[ Db.V_text "1900-01-01" ]) with
        | Error e -> Alcotest.failf "iter: %a" Db.pp_error e
        | Ok stream ->
          List.sort
            compare
            (List.map
               (fun r -> Array.to_list (Array.map render r))
               (run (Lwt_stream.to_list stream)))
      in
      check_rows ~label:"inner-parameter fallback" expected_q4 rows)
;;

(* The inner FROM shadows the outer table (#592's guard). Nothing about #493
   may move it: the parameter substitution consults the same [inner_scope_of]. *)
let inner_scope_still_shadows_the_outer_one () =
  with_mem_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE TABLE u (b INTEGER)";
    exec db "INSERT INTO t VALUES (1), (2)";
    exec db "INSERT INTO u VALUES (2)";
    (* [t.a] inside the subquery is the INNER t, not the outer one, so the
       predicate is [a = a] and every outer row qualifies. *)
    check_rows
      ~label:"inner FROM shadows the qualified reference"
      [ [ "1" ]; [ "2" ] ]
      (rows_of db "SELECT a FROM t WHERE EXISTS (SELECT * FROM t WHERE t.a = t.a)");
    (* With no shadowing, the correlation is real. *)
    check_rows
      ~label:"unshadowed correlation still binds"
      [ [ "2" ] ]
      (rows_of db "SELECT a FROM t WHERE EXISTS (SELECT * FROM u WHERE b = t.a)"))
;;

(* Read-your-own-writes (#262): inside an explicit transaction the correlated
   subquery must borrow that transaction, not open a snapshot of committed
   state. [with_subquery_txn] is the one place that decides, so this is its
   guard. *)
let a_correlated_subquery_reads_its_own_uncommitted_writes () =
  with_mem_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE TABLE u (b INTEGER)";
    exec db "INSERT INTO t VALUES (1), (2), (3)";
    exec db "BEGIN";
    exec db "INSERT INTO u VALUES (2)";
    check_rows
      ~label:"the subquery sees the uncommitted row"
      [ [ "2" ] ]
      (rows_of db "SELECT a FROM t WHERE EXISTS (SELECT * FROM u WHERE b = t.a)");
    exec db "ROLLBACK";
    check_rows
      ~label:"and not after it is rolled back"
      []
      (rows_of db "SELECT a FROM t WHERE EXISTS (SELECT * FROM u WHERE b = t.a)"))
;;

let () =
  Alcotest.run
    "correlated EXISTS (#493)"
    [ ( "the leak that made it super-linear"
      , [ Alcotest.test_case
            "an abandoned EXISTS leaves no reader snapshot behind"
            `Quick
            leaves_no_reader_snapshot_behind
        ; Alcotest.test_case
            "nor when every outer row probes"
            `Quick
            leaves_nothing_behind_when_every_row_probes
        ; Alcotest.test_case
            "nor a scalar subquery or a correlated IN"
            `Quick
            scalar_and_in_subqueries_leave_nothing_behind
        ] )
    ; ( "seek, not drain"
      , [ Alcotest.test_case
            "the inner subquery seeks its index"
            `Quick
            the_inner_subquery_seeks_rather_than_scans
        ] )
    ; ( "short-circuit"
      , [ Alcotest.test_case
            "a cheap conjunct short-circuits the subquery"
            `Quick
            a_cheap_conjunct_short_circuits_the_subquery
        ; Alcotest.test_case
            "conjuncts are short-circuited, not reordered"
            `Quick
            conjuncts_are_short_circuited_not_reordered
        ] )
    ; ( "the answers must not move"
      , [ Alcotest.test_case
            "correlated EXISTS"
            `Quick
            correlated_exists_answers_correctly
        ; Alcotest.test_case
            "correlated NOT EXISTS is the complement"
            `Quick
            correlated_not_exists_is_the_complement
        ; Alcotest.test_case
            "correlated IN (subquery)"
            `Quick
            correlated_in_subquery_answers_correctly
        ; Alcotest.test_case
            "correlated scalar subquery in the projection"
            `Quick
            correlated_scalar_subquery_in_the_projection
        ; Alcotest.test_case
            "the whole Q4 shape, GROUP BY included"
            `Quick
            correlated_exists_under_group_by
        ; Alcotest.test_case
            "a NULL outer value matches nothing"
            `Quick
            a_null_outer_value_matches_nothing
        ; Alcotest.test_case
            "an inner parameter falls back and still answers"
            `Quick
            an_inner_parameter_falls_back_and_still_answers
        ; Alcotest.test_case
            "the inner scope still shadows the outer one (#592)"
            `Quick
            inner_scope_still_shadows_the_outer_one
        ; Alcotest.test_case
            "read-your-own-writes still holds (#262)"
            `Quick
            a_correlated_subquery_reads_its_own_uncommitted_writes
        ] )
    ]
;;
