module T = Granary_tpc.Tpcc_txn

(* TPC-C 2.1.6.1: the three NURand run constants are chosen INDEPENDENTLY,
   each within [0, A] for its own A, and the c_last RUN constant must differ
   from the LOAD constant by a delta in [65, 119] excluding 96 and 112 — so
   that the run's hot surnames do not coincide with the load's. An earlier
   version of this harness threaded ONE constant into all three uses, which
   violates the last rule by construction (delta 0). *)
let test_run_constants_are_in_range () =
  let c = T.default_run_constants in
  let in_range name v hi =
    Alcotest.(check bool)
      (Printf.sprintf "%s = %d is within [0, %d]" name v hi)
      true
      (v >= 0 && v <= hi)
  in
  in_range "nurand_c_id" c.T.nurand_c_id 1023;
  in_range "nurand_ol_i_id" c.T.nurand_ol_i_id 8191;
  in_range "nurand_c_last" c.T.nurand_c_last 255
;;

let test_c_last_run_constant_obeys_the_delta_rule () =
  let delta =
    abs (T.default_run_constants.T.nurand_c_last - Granary_tpc.Tpcc_gen.c_load)
  in
  Alcotest.(check int) "the delta is the documented one" T.c_last_run_delta delta;
  Alcotest.(check bool)
    (Printf.sprintf "delta %d is within [65, 119]" delta)
    true
    (delta >= 65 && delta <= 119);
  Alcotest.(check bool)
    (Printf.sprintf "delta %d is neither 96 nor 112" delta)
    true
    (delta <> 96 && delta <> 112)
;;

(* The three constants must not be the same value: sharing one is exactly the
   bug this replaced, and for c_last it is what the delta rule forbids. *)
let test_run_constants_are_independent () =
  let c = T.default_run_constants in
  Alcotest.(check bool)
    "c_id and ol_i_id differ"
    true
    (c.T.nurand_c_id <> c.T.nurand_ol_i_id);
  Alcotest.(check bool)
    "c_last differs from Tpcc_gen.c_load"
    true
    (c.T.nurand_c_last <> Granary_tpc.Tpcc_gen.c_load)
;;

let test_all_five_profiles () =
  Alcotest.(check (list string))
    "the spec's five profiles"
    [ "new_order"; "payment"; "order_status"; "delivery"; "stock_level" ]
    (List.map (fun p -> p.T.name) T.all)
;;

let test_weights_sum_to_100 () =
  Alcotest.(check int)
    "weights sum to 100"
    100
    (List.fold_left (fun a p -> a + p.T.weight) 0 T.all)
;;

let test_spec_weights () =
  let w name = (List.find (fun p -> p.T.name = name) T.all).T.weight in
  Alcotest.(check int) "new_order" 45 (w "new_order");
  Alcotest.(check int) "payment" 43 (w "payment");
  Alcotest.(check int) "order_status" 4 (w "order_status");
  Alcotest.(check int) "delivery" 4 (w "delivery");
  Alcotest.(check int) "stock_level" 4 (w "stock_level")
;;

let test_pick_respects_weights () =
  let r = Granary_tpc.Tpc_rand.create ~seed:42 in
  let n = 20_000 in
  let counts = Hashtbl.create 5 in
  for _ = 1 to n do
    let p = T.pick r T.all in
    Hashtbl.replace
      counts
      p.T.name
      (1 + Option.value ~default:0 (Hashtbl.find_opt counts p.T.name))
  done;
  let share name = float_of_int (Hashtbl.find counts name) /. float_of_int n in
  Alcotest.(check bool)
    (Printf.sprintf "new_order near 0.45 (got %.3f)" (share "new_order"))
    true
    (Float.abs (share "new_order" -. 0.45) < 0.02)
;;

let test_pick_skips_skipped_and_redistributes () =
  (* A profile granary cannot express must drop out of the mix entirely, and
     the survivors must keep their ratios to one another — otherwise the
     reported mix has a silent hole in it. *)
  let runnable =
    List.filter (fun p -> p.T.name <> "delivery") T.all
    @ [ { (List.find (fun p -> p.T.name = "delivery") T.all) with
          T.verdict = T.Skipped "#NNN"
        }
      ]
  in
  let r = Granary_tpc.Tpc_rand.create ~seed:7 in
  for _ = 1 to 5_000 do
    Alcotest.(check bool)
      "never picks a skipped profile"
      true
      ((T.pick r runnable).T.name <> "delivery")
  done
;;

let test_pick_all_skipped_raises () =
  let none = List.map (fun p -> { p with T.verdict = T.Skipped "#NNN" }) T.all in
  let r = Granary_tpc.Tpc_rand.create ~seed:1 in
  Alcotest.check_raises
    "no runnable profile"
    (Invalid_argument "Tpcc_txn.pick: no runnable profile")
    (fun () -> ignore (T.pick r none))
;;

let test_gen_input_is_deterministic () =
  let draw () =
    let r = Granary_tpc.Tpc_rand.create ~seed:99 in
    List.map
      (fun p -> T.gen_input r ~warehouses:2 ~constants:T.default_run_constants p)
      T.all
  in
  Alcotest.(check bool) "same seed, same inputs" true (draw () = draw ())
;;

(* --- transaction control flow ----------------------------------------- *)

module V = Granary_tpc.Tpc_value

(* Records every statement and returns canned rows, so the profiles' control
   flow is testable with no engine at all. *)
let mock ~rows =
  let log = ref [] in
  let next = ref rows in
  let record s = log := T.render s :: !log in
  let ops =
    { T.query =
        (fun s ->
          record s;
          match !next with
          | [] -> Lwt.return []
          | r :: rest ->
            next := rest;
            Lwt.return r)
    ; T.exec =
        (fun s ->
          record s;
          Lwt.return_unit)
    }
  in
  ops, fun () -> List.rev !log
;;

let starts_with prefix s =
  String.length s >= String.length prefix
  && String.equal (String.sub s 0 (String.length prefix)) prefix
;;

(* [contains needle s] — no Str dependency in this test executable. *)
let contains needle s =
  let n = String.length needle
  and m = String.length s in
  let rec at i = i + n <= m && (String.equal (String.sub s i n) needle || at (i + 1)) in
  n = 0 || at 0
;;

let count_matching p log = List.length (List.filter p log)
let issued needle log = List.exists (contains needle) log

(* [in_order needles log] — every needle appears, each after the previous. *)
let in_order needles log =
  let rec walk needles log =
    match needles, log with
    | [], _ -> true
    | _ :: _, [] -> false
    | n :: ns, s :: rest -> if contains n s then walk ns rest else walk (n :: ns) rest
  in
  walk needles log
;;

let last log = List.nth log (List.length log - 1)

(* Walks the WHOLE log rather than only its ends: every statement must sit
   inside an open transaction, every BEGIN must find none open, and the log
   must end with none open.  Checking only the first and last entries would
   let a stray mid-log BEGIN — a transaction opened twice, or one left open
   while the next is started — pass unnoticed. *)
let transaction_error log =
  let rec walk open_ n = function
    | [] -> if open_ then Some "ends with a transaction still open" else None
    | s :: rest when starts_with "BEGIN" s ->
      if open_
      then Some (Printf.sprintf "statement %d: BEGIN while a transaction is open" n)
      else walk true (n + 1) rest
    | s :: rest when starts_with "COMMIT" s || starts_with "ROLLBACK" s ->
      if open_
      then walk false (n + 1) rest
      else Some (Printf.sprintf "statement %d: close with no transaction open" n)
    | _ :: rest when open_ -> walk true (n + 1) rest
    | _ :: _ -> Some (Printf.sprintf "statement %d: outside any transaction" n)
  in
  walk false 0 log
;;

let check_transactional what log =
  if log = [] then Alcotest.fail ("no statements issued by " ^ what);
  Alcotest.(check bool)
    (what ^ " opens at least one transaction")
    true
    (List.exists (starts_with "BEGIN") log);
  match transaction_error log with
  | None -> ()
  | Some why -> Alcotest.fail (what ^ ": " ^ why)
;;

(* An [ops] whose [exec] rejects the first statement matching [fail_on],
   for exercising the mid-transaction failure path. *)
exception Engine_rejected

let failing_mock ~rows ~fail_on =
  let ops, log = mock ~rows in
  let already = ref false in
  let exec s =
    if (not !already) && contains fail_on (T.render s)
    then (
      already := true;
      ignore (ops.T.exec s : unit Lwt.t);
      Lwt.fail Engine_rejected)
    else ops.T.exec s
  in
  { T.query = ops.T.query; T.exec }, log
;;

let test_new_order_is_wrapped_in_a_transaction () =
  (* w_tax, then (d_tax, d_next_o_id), then the customer — all three are
     required reads and must be answered. Only the item lookups go
     unanswered, so this run takes the ROLLBACK arm. *)
  let ops, log =
    mock
      ~rows:
        [ [ [ "0.1000" ] ]
        ; [ [ "0.0500"; "3001" ] ]
        ; [ [ "0.1000"; "BARBARBAR"; "GC" ] ]
        ]
  in
  let r = Granary_tpc.Tpc_rand.create ~seed:5 in
  let p = List.find (fun p -> p.T.name = "new_order") T.all in
  Lwt_main.run
    (T.run ops (T.gen_input r ~warehouses:1 ~constants:T.default_run_constants p));
  match log () with
  | [] -> Alcotest.fail "no statements issued"
  | first :: _ as all ->
    Alcotest.(check bool) "opens with BEGIN" true (starts_with "BEGIN" first);
    let last = List.nth all (List.length all - 1) in
    Alcotest.(check bool)
      "closes with COMMIT or ROLLBACK"
      true
      (starts_with "COMMIT" last || starts_with "ROLLBACK" last)
;;

let test_render_substitutes_params () =
  Alcotest.(check string)
    "params substituted in order"
    "INSERT INTO t VALUES (1,'a',2.5)"
    (T.render
       { T.sql = "INSERT INTO t VALUES (?,?,?)"
       ; T.params = [ V.VInt 1; V.VText "a"; V.VReal 2.5 ]
       })
;;

let test_render_arity_mismatch_raises () =
  Alcotest.check_raises
    "too few params"
    (Invalid_argument "Tpcc_txn.render: 2 placeholders but 1 parameter(s)")
    (fun () -> ignore (T.render { T.sql = "SELECT ?, ?"; T.params = [ V.VInt 1 ] }))
;;

let test_render_too_many_params_raises () =
  Alcotest.check_raises
    "too many params"
    (Invalid_argument "Tpcc_txn.render: 1 placeholders but 2 parameter(s)")
    (fun () ->
       ignore (T.render { T.sql = "SELECT ?"; T.params = [ V.VInt 1; V.VInt 2 ] }))
;;

let test_render_escapes_text_and_null () =
  Alcotest.(check string)
    "text quoted, NULL bare"
    "VALUES ('O''Hara',NULL)"
    (T.render { T.sql = "VALUES (?,?)"; T.params = [ V.VText "O'Hara"; V.VNull ] })
;;

(* A single-line NewOrder with every lookup answered, so the whole body runs
   and the transaction commits. *)
let new_order_rows =
  [ [ [ "0.1000" ] ] (* w_tax *)
  ; [ [ "0.0500"; "3001" ] ] (* d_tax, d_next_o_id *)
  ; [ [ "0.1000"; "BARBARBAR"; "GC" ] ] (* c_discount, c_last, c_credit *)
  ; [ [ "10.0"; "item"; "data" ] ] (* i_price, i_name, i_data *)
  ; [ [ "50"; "dist-info"; "s_data" ] ] (* s_quantity, s_dist_NN, s_data *)
  ]
;;

let one_line_new_order ~rollback =
  T.New_order_input
    { w_id = 1
    ; d_id = 3
    ; c_id = 7
    ; lines = [ { T.ol_i_id = 42; ol_supply_w_id = 1; ol_quantity = 5 } ]
    ; rollback
    }
;;

let test_new_order_statement_sequence () =
  let ops, log = mock ~rows:new_order_rows in
  Lwt_main.run (T.run ops (one_line_new_order ~rollback:false));
  let log = log () in
  check_transactional "new_order" log;
  Alcotest.(check bool)
    "spec statement sequence"
    true
    (in_order
       [ "BEGIN"
       ; "SELECT w_tax"
       ; "FROM district"
       ; "UPDATE district SET d_next_o_id = d_next_o_id + 1"
       ; "FROM customer"
       ; "INSERT INTO orders"
       ; "INSERT INTO new_order"
       ; "FROM item"
       ; "FROM stock"
       ; "UPDATE stock"
       ; "INSERT INTO order_line"
       ; "COMMIT"
       ]
       log);
  Alcotest.(check bool)
    "reads the district's dist column for d_id = 3"
    true
    (issued "s_dist_03" log);
  Alcotest.(check bool)
    "inserts the order at the district's d_next_o_id"
    true
    (issued
       "INSERT INTO orders (o_id, o_d_id, o_w_id, o_c_id, o_entry_d, o_carrier_id, \
        o_ol_cnt, o_all_local) VALUES (3001,3,1,7,"
       log)
;;

let test_new_order_rollback_path () =
  (* No item row is served for the (deliberately invalid) line, so the item
     lookup returns no rows and the profile must roll back and return. *)
  let ops, log =
    mock
      ~rows:
        [ [ [ "0.1000" ] ]
        ; [ [ "0.0500"; "3001" ] ]
        ; [ [ "0.1000"; "BARBARBAR"; "GC" ] ]
        ; [] (* the invalid item id matches nothing *)
        ]
  in
  Lwt_main.run (T.run ops (one_line_new_order ~rollback:true));
  let log = log () in
  Alcotest.(check bool) "ends in ROLLBACK" true (starts_with "ROLLBACK" (last log));
  Alcotest.(check bool) "no COMMIT" false (issued "COMMIT" log);
  Alcotest.(check bool)
    "no order_line written for the failed line"
    false
    (issued "INSERT INTO order_line" log)
;;

let payment_rows ~credit =
  [ [ [ "warehouse-name" ] ]
  ; [ [ "district-name" ] ]
  ; [ [ "7"; "Fi"; "OE"; "BARBARBAR"; "-10.0"; "10.0"; "1"; credit; "old-data" ] ]
  ]
;;

let payment_input customer =
  T.Payment_input
    { w_id = 1; d_id = 3; customer_w_id = 1; customer_d_id = 3; customer; amount = 25.5 }
;;

let test_payment_statement_sequence () =
  let ops, log = mock ~rows:(payment_rows ~credit:"GC") in
  Lwt_main.run (T.run ops (payment_input (T.By_id 7)));
  let log = log () in
  check_transactional "payment" log;
  Alcotest.(check bool)
    "spec statement sequence"
    true
    (in_order
       [ "BEGIN"
       ; "UPDATE warehouse SET w_ytd = w_ytd + 25.5"
       ; "UPDATE district SET d_ytd = d_ytd + 25.5"
       ; "UPDATE customer"
       ; "INSERT INTO history"
       ; "COMMIT"
       ]
       log);
  Alcotest.(check bool) "good-credit customer keeps c_data" false (issued "c_data =" log)
;;

let test_payment_bad_credit_rewrites_c_data () =
  let ops, log = mock ~rows:(payment_rows ~credit:"BC") in
  Lwt_main.run (T.run ops (payment_input (T.By_id 7)));
  let log = log () in
  Alcotest.(check bool) "BC customer's c_data is rewritten" true (issued "c_data =" log);
  Alcotest.(check bool) "new c_data keeps the old" true (issued "old-data" log)
;;

let test_payment_by_last_name_takes_the_middle_row () =
  let ops, log =
    mock
      ~rows:
        [ [ [ "warehouse-name" ] ]
        ; [ [ "district-name" ] ]
        ; [ [ "11"; "Aa"; "OE"; "BARBARBAR"; "-1.0"; "1.0"; "1"; "GC"; "d1" ]
          ; [ "22"; "Bb"; "OE"; "BARBARBAR"; "-2.0"; "2.0"; "1"; "GC"; "d2" ]
          ; [ "33"; "Cc"; "OE"; "BARBARBAR"; "-3.0"; "3.0"; "1"; "GC"; "d3" ]
          ]
        ]
  in
  Lwt_main.run (T.run ops (payment_input (T.By_last_name "BARBARBAR")));
  let log = log () in
  Alcotest.(check bool)
    "looks the customer up by last name, ordered by c_first"
    true
    (issued "c_last = 'BARBARBAR' ORDER BY c_first" log);
  Alcotest.(check bool)
    "updates the middle customer of the three"
    true
    (issued "c_id = 22" log)
;;

let test_order_status_is_read_only () =
  let ops, log =
    mock
      ~rows:
        [ [ [ "7"; "Fi"; "OE"; "BARBARBAR"; "-10.0" ] ]
        ; [ [ "2001"; "2026-01-01 00:00:00"; "NULL" ] ]
        ; [ [ "1"; "1"; "5"; "50.0"; "NULL" ] ]
        ]
  in
  Lwt_main.run
    (T.run ops (T.Order_status_input { w_id = 1; d_id = 3; customer = T.By_id 7 }));
  let log = log () in
  check_transactional "order_status" log;
  Alcotest.(check bool)
    "reads the customer, their newest order, then its lines"
    true
    (in_order
       [ "BEGIN"; "FROM customer"; "FROM orders"; "FROM order_line"; "COMMIT" ]
       log);
  Alcotest.(check bool)
    "takes the most recent order"
    true
    (issued "ORDER BY o_id DESC" log);
  List.iter
    (fun forbidden ->
       Alcotest.(check bool) ("read-only: no " ^ forbidden) false (issued forbidden log))
    [ "INSERT"; "UPDATE"; "DELETE" ]
;;

let delivery_district_rows =
  [ [ [ "2001" ] ] (* MIN(no_o_id) *)
  ; [ [ "7" ] ] (* o_c_id *)
  ; [ [ "123.45" ] ] (* SUM(ol_amount) *)
  ]
;;

let test_delivery_runs_ten_transactions () =
  let rows = List.concat (List.init 10 (fun _ -> delivery_district_rows)) in
  let ops, log = mock ~rows in
  Lwt_main.run (T.run ops (T.Delivery_input { w_id = 1; carrier_id = 4 }));
  let log = log () in
  Alcotest.(check int) "ten BEGINs" 10 (count_matching (starts_with "BEGIN") log);
  Alcotest.(check int) "ten COMMITs" 10 (count_matching (starts_with "COMMIT") log);
  Alcotest.(check int)
    "ten deletes from new_order"
    10
    (count_matching (contains "DELETE FROM new_order") log);
  Alcotest.(check bool)
    "per-district sequence"
    true
    (in_order
       [ "BEGIN"
       ; "SELECT MIN(no_o_id)"
       ; "DELETE FROM new_order"
       ; "UPDATE orders SET o_carrier_id = 4"
       ; "UPDATE order_line SET ol_delivery_d"
       ; "SELECT SUM(ol_amount)"
       ; "UPDATE customer SET c_balance = c_balance + 123.45"
       ; "COMMIT"
       ]
       log)
;;

let test_delivery_skips_a_district_with_no_new_order () =
  (* MIN over an empty district yields one row holding SQL NULL. *)
  let ops, log = mock ~rows:(List.init 10 (fun _ -> [ [ "NULL" ] ])) in
  Lwt_main.run (T.run ops (T.Delivery_input { w_id = 2; carrier_id = 9 }));
  let log = log () in
  Alcotest.(check int) "still ten BEGINs" 10 (count_matching (starts_with "BEGIN") log);
  Alcotest.(check int) "still ten COMMITs" 10 (count_matching (starts_with "COMMIT") log);
  Alcotest.(check bool) "nothing delivered" false (issued "DELETE FROM new_order" log)
;;

(* The counterpart to the case above: [MIN] over a district always returns
   exactly one row, so ZERO rows cannot mean "nothing to deliver" — it means
   the query broke. Absorbing it would make Delivery commit an empty
   transaction, report success, and leave every consistency condition
   holding while having delivered nothing. *)
let test_delivery_empty_result_raises () =
  let ops, log = mock ~rows:[ [] ] in
  let raised =
    try
      Lwt_main.run (T.run ops (T.Delivery_input { w_id = 2; carrier_id = 9 }));
      None
    with
    | T.Missing_value why -> Some why
  in
  match raised with
  | None -> Alcotest.fail "a zero-row MIN(no_o_id) was treated as nothing to deliver"
  | Some why ->
    Alcotest.(check bool)
      (Printf.sprintf "the failure names the aggregate (%s)" why)
      true
      (contains "MIN(no_o_id)" why);
    let log = log () in
    Alcotest.(check bool)
      "rolls back before raising"
      true
      (starts_with "ROLLBACK" (last log));
    Alcotest.(check bool) "delivers nothing" false (issued "DELETE FROM new_order" log)
;;

(* NewOrder reads w_tax but reports no total, so the value goes unused — the
   read is still required, because the warehouse being ordered from always
   exists and zero rows means the lookup broke. *)
let test_new_order_empty_warehouse_read_raises () =
  let ops, _log = mock ~rows:[ [] ] in
  let raised =
    try
      Lwt_main.run (T.run ops (one_line_new_order ~rollback:false));
      None
    with
    | T.Missing_value why -> Some why
  in
  match raised with
  | None -> Alcotest.fail "a zero-row warehouse lookup was discarded"
  | Some why ->
    Alcotest.(check bool)
      (Printf.sprintf "the failure names the column (%s)" why)
      true
      (contains "w_tax" why)
;;

(* Same for NewOrder's customer lookup, whose c_discount is likewise read and
   unused: c_id comes from a NURand over the loaded customers, so no row
   means the composite-key lookup broke (the read #508 makes most likely to
   regress). *)
let test_new_order_empty_customer_read_raises () =
  let ops, _log = mock ~rows:[ [ [ "0.1000" ] ]; [ [ "0.0500"; "3001" ] ]; [] ] in
  let raised =
    try
      Lwt_main.run (T.run ops (one_line_new_order ~rollback:false));
      None
    with
    | T.Missing_value why -> Some why
  in
  match raised with
  | None -> Alcotest.fail "a zero-row customer lookup was discarded"
  | Some why ->
    Alcotest.(check bool)
      (Printf.sprintf "the failure names the column (%s)" why)
      true
      (contains "c_discount" why)
;;

(* OrderStatus reports its order lines to the terminal in the spec and not at
   all here, but every order carries 5 to 15 of them, so zero rows is a
   broken read rather than an order without lines. *)
let test_order_status_empty_lines_raises () =
  let ops, _log =
    mock
      ~rows:
        [ [ [ "7"; "Fi"; "OE"; "BARBARBAR"; "-10.0" ] ]
        ; [ [ "2001"; "2026-01-01 00:00:00"; "NULL" ] ]
        ; []
        ]
  in
  let raised =
    try
      Lwt_main.run
        (T.run ops (T.Order_status_input { w_id = 1; d_id = 3; customer = T.By_id 7 }));
      None
    with
    | T.Missing_value why -> Some why
  in
  match raised with
  | None -> Alcotest.fail "a zero-row order_line lookup was discarded"
  | Some why ->
    Alcotest.(check bool)
      (Printf.sprintf "the failure names the lookup (%s)" why)
      true
      (contains "order_line" why)
;;

let test_stock_level_is_read_only () =
  let ops, log = mock ~rows:[ [ [ "3001" ] ]; [ [ "10" ]; [ "20" ]; [ "30" ] ] ] in
  Lwt_main.run (T.run ops (T.Stock_level_input { w_id = 1; d_id = 3; threshold = 15 }));
  let log = log () in
  check_transactional "stock_level" log;
  Alcotest.(check bool)
    "reads d_next_o_id then the low-stock items of the last 20 orders"
    true
    (in_order [ "BEGIN"; "SELECT d_next_o_id"; "SELECT DISTINCT s_i_id"; "COMMIT" ] log);
  Alcotest.(check bool)
    "windows the last 20 orders below d_next_o_id"
    true
    (issued "ol_o_id < 3001 AND ol_o_id >= 2981" log);
  Alcotest.(check bool) "applies the threshold" true (issued "s_quantity < 15" log);
  List.iter
    (fun forbidden ->
       Alcotest.(check bool) ("read-only: no " ^ forbidden) false (issued forbidden log))
    [ "INSERT"; "UPDATE"; "DELETE" ]
;;

let run_profile_ignoring_failure ops input =
  try Lwt_main.run (T.run ops input) with
  | T.Missing_value _ -> ()
;;

let test_every_profile_is_wrapped () =
  (* Whatever the canned rows — here, none at all, so every required read
     raises — no profile may leave a transaction open.  With the required_*
     policy in place this is precisely the mid-transaction failure path. *)
  let r = Granary_tpc.Tpc_rand.create ~seed:123 in
  List.iter
    (fun p ->
       let ops, log = mock ~rows:[] in
       run_profile_ignoring_failure
         ops
         (T.gen_input r ~warehouses:2 ~constants:T.default_run_constants p);
       let log = log () in
       Alcotest.(check int)
         (p.T.name ^ ": one close per open")
         (count_matching (starts_with "BEGIN") log)
         (count_matching (starts_with "COMMIT") log
          + count_matching (starts_with "ROLLBACK") log);
       check_transactional p.T.name log)
    T.all
;;

(* --- required reads and the failure path ------------------------------ *)

let test_missing_required_column_raises () =
  (* The district lookup answers with only d_tax: d_next_o_id is absent, so
     NewOrder would otherwise insert an order at a defaulted, colliding o_id
     AFTER having already bumped the counter. *)
  let ops, log = mock ~rows:[ [ [ "0.1000" ] ]; [ [ "0.0500" ] ] ] in
  let raised =
    try
      Lwt_main.run (T.run ops (one_line_new_order ~rollback:false));
      None
    with
    | T.Missing_value why -> Some why
  in
  match raised with
  | None -> Alcotest.fail "a missing d_next_o_id was silently defaulted"
  | Some why ->
    Alcotest.(check bool)
      (Printf.sprintf "the failure names the column (%s)" why)
      true
      (contains "d_next_o_id" why);
    Alcotest.(check bool) "and names the lookup" true (contains "district" why);
    let log = log () in
    Alcotest.(check bool)
      "rolls back before raising"
      true
      (starts_with "ROLLBACK" (last log));
    Alcotest.(check bool) "and does not commit" false (issued "COMMIT" log);
    check_transactional "new_order" log
;;

let test_unparseable_required_column_raises () =
  let ops, log = mock ~rows:[ [ [ "0.1000" ] ]; [ [ "0.0500"; "not-a-number" ] ] ] in
  let raised =
    try
      Lwt_main.run (T.run ops (one_line_new_order ~rollback:false));
      false
    with
    | T.Missing_value _ -> true
  in
  Alcotest.(check bool) "an unparseable d_next_o_id raises too" true raised;
  Alcotest.(check bool) "rolls back" true (starts_with "ROLLBACK" (last (log ())))
;;

let test_engine_failure_rolls_back_and_reraises () =
  (* A statement the engine rejects mid-transaction must not escape with the
     transaction still open: granary's writer lock is not reentrant, and an
     abandoned open transaction holds it for the rest of the run. *)
  let ops, log = failing_mock ~rows:new_order_rows ~fail_on:"UPDATE district" in
  let raised =
    try
      Lwt_main.run (T.run ops (one_line_new_order ~rollback:false));
      false
    with
    | Engine_rejected -> true
  in
  Alcotest.(check bool) "the original exception propagates" true raised;
  let log = log () in
  Alcotest.(check bool) "rolls back first" true (starts_with "ROLLBACK" (last log));
  Alcotest.(check bool) "no COMMIT" false (issued "COMMIT" log);
  check_transactional "new_order" log
;;

let test_delivery_failure_rolls_back_only_its_district () =
  (* Delivery is ten transactions; a failure in a later one must not reopen or
     roll back the districts that already committed. *)
  let rows =
    List.concat (List.init 3 (fun _ -> delivery_district_rows))
    @ [ [ [ "2001" ] ]; [] (* the fourth district's o_c_id lookup comes back empty *) ]
  in
  let ops, log = mock ~rows in
  let raised =
    try
      Lwt_main.run (T.run ops (T.Delivery_input { w_id = 1; carrier_id = 4 }));
      false
    with
    | T.Missing_value _ -> true
  in
  Alcotest.(check bool) "the failure propagates out of run" true raised;
  let log = log () in
  Alcotest.(check int)
    "three districts committed"
    3
    (count_matching (starts_with "COMMIT") log);
  Alcotest.(check int)
    "the fourth rolled back"
    1
    (count_matching (starts_with "ROLLBACK") log);
  Alcotest.(check int)
    "and no district was left open"
    4
    (count_matching (starts_with "BEGIN") log);
  check_transactional "delivery" log
;;

(* --- consistency condition 4: o_ol_cnt = the order's line count -------- *)

let test_new_order_line_count_matches_o_ol_cnt () =
  let n = 4 in
  let lines =
    List.init n (fun i -> { T.ol_i_id = 40 + i; ol_supply_w_id = 1; ol_quantity = 1 + i })
  in
  let rows =
    [ [ [ "0.1000" ] ]; [ [ "0.0500"; "3001" ] ]; [ [ "0.1000"; "BARBARBAR"; "GC" ] ] ]
    @ List.concat
        (List.init n (fun _ ->
           [ [ [ "10.0"; "item"; "data" ] ]; [ [ "50"; "dist-info"; "s_data" ] ] ]))
  in
  let ops, log = mock ~rows in
  Lwt_main.run
    (T.run
       ops
       (T.New_order_input { w_id = 1; d_id = 3; c_id = 7; lines; rollback = false }));
  let log = log () in
  check_transactional "new_order" log;
  Alcotest.(check bool) "commits" true (starts_with "COMMIT" (last log));
  Alcotest.(check int)
    "one order_line insert per line"
    n
    (count_matching (contains "INSERT INTO order_line") log);
  Alcotest.(check bool)
    "o_ol_cnt equals the line count"
    true
    (issued (Printf.sprintf ",NULL,%d,1)" n) log);
  Alcotest.(check bool) "and is not the single-line value" false (issued ",NULL,1,1)" log);
  List.iter
    (fun number ->
       Alcotest.(check bool)
         (Printf.sprintf "ol_number %d is written" number)
         true
         (issued (Printf.sprintf "VALUES (3001,3,1,%d," number) log))
    (List.init n (fun i -> i + 1))
;;

(* --- properties ------------------------------------------------------- *)

(* Structural guard on the Tpcc_gen/Tpcc_txn seam: every input [gen_input]
   draws must address rows the generator actually produced. The bounds below
   are read from Tpcc_gen rather than written as literals ON PURPOSE — that
   is what makes this a guard on the seam and not a second copy of the spec
   constants. Change [customers_per_district] and this property follows it;
   spell it 3000 here and the property would keep passing while the workload
   silently retargeted. *)
let districts = Granary_tpc.Tpcc_gen.districts_per_warehouse
let items = Granary_tpc.Tpcc_gen.items

(* The item id is either a real one or the deliberately invalid one; the
   parentheses matter, since [||] binds looser than [&&] and dropping them
   would let a valid item id short-circuit past every other check. *)
let line_is_valid ~warehouses line =
  ((line.T.ol_i_id >= 1 && line.T.ol_i_id <= items) || line.T.ol_i_id = T.invalid_item_id)
  && line.T.ol_supply_w_id >= 1
  && line.T.ol_supply_w_id <= warehouses
  && line.T.ol_quantity >= 1
  && line.T.ol_quantity <= 10
;;

let input_is_in_range ~warehouses input =
  let w_ok w = w >= 1 && w <= warehouses in
  let d_ok d = d >= 1 && d <= districts in
  match input with
  | T.New_order_input { w_id; d_id; c_id; lines; rollback = _ } ->
    w_ok w_id
    && d_ok d_id
    && c_id >= 1
    && c_id <= Granary_tpc.Tpcc_gen.customers_per_district
    && List.length lines >= 5
    && List.length lines <= 15
    && List.for_all (line_is_valid ~warehouses) lines
  | T.Payment_input { w_id; d_id; customer_w_id; customer_d_id; customer = _; amount } ->
    w_ok w_id && d_ok d_id && w_ok customer_w_id && d_ok customer_d_id && amount >= 1.0
  | T.Order_status_input { w_id; d_id; customer = _ } -> w_ok w_id && d_ok d_id
  | T.Delivery_input { w_id; carrier_id } ->
    w_ok w_id && carrier_id >= 1 && carrier_id <= 10
  | T.Stock_level_input { w_id; d_id; threshold } ->
    w_ok w_id && d_ok d_id && threshold >= 10 && threshold <= 20
;;

let prop_gen_input_stays_in_the_generated_population =
  QCheck.Test.make
    ~count:500
    ~name:"gen_input only ever addresses rows Tpcc_gen generated"
    QCheck.(pair int (int_range 1 2))
    (fun (seed, warehouses) ->
       let r = Granary_tpc.Tpc_rand.create ~seed in
       List.for_all
         (fun profile ->
            let input =
              T.gen_input r ~warehouses ~constants:T.default_run_constants profile
            in
            input_is_in_range ~warehouses input)
         T.all)
;;

let () =
  Alcotest.run
    "tpcc_txn"
    [ ( "catalogue"
      , [ Alcotest.test_case "five profiles" `Quick test_all_five_profiles
        ; Alcotest.test_case "weights sum to 100" `Quick test_weights_sum_to_100
        ; Alcotest.test_case "spec weights" `Quick test_spec_weights
        ] )
    ; ( "mix"
      , [ Alcotest.test_case "respects weights" `Quick test_pick_respects_weights
        ; Alcotest.test_case
            "skips skipped"
            `Quick
            test_pick_skips_skipped_and_redistributes
        ; Alcotest.test_case "all skipped raises" `Quick test_pick_all_skipped_raises
        ] )
    ; ( "run constants (2.1.6.1)"
      , [ Alcotest.test_case
            "each is within [0, A]"
            `Quick
            test_run_constants_are_in_range
        ; Alcotest.test_case
            "c_last obeys the delta rule"
            `Quick
            test_c_last_run_constant_obeys_the_delta_rule
        ; Alcotest.test_case
            "the three are not one shared value"
            `Quick
            test_run_constants_are_independent
        ] )
    ; ( "inputs"
      , [ Alcotest.test_case "deterministic" `Quick test_gen_input_is_deterministic ] )
    ; ( "render"
      , [ Alcotest.test_case "substitutes params" `Quick test_render_substitutes_params
        ; Alcotest.test_case "too few params" `Quick test_render_arity_mismatch_raises
        ; Alcotest.test_case "too many params" `Quick test_render_too_many_params_raises
        ; Alcotest.test_case "escapes text, NULL" `Quick test_render_escapes_text_and_null
        ] )
    ; ( "new_order"
      , [ Alcotest.test_case
            "wrapped in a transaction"
            `Quick
            test_new_order_is_wrapped_in_a_transaction
        ; Alcotest.test_case "statement sequence" `Quick test_new_order_statement_sequence
        ; Alcotest.test_case "rollback path" `Quick test_new_order_rollback_path
        ; Alcotest.test_case
            "o_ol_cnt matches the line count"
            `Quick
            test_new_order_line_count_matches_o_ol_cnt
        ] )
    ; ( "payment"
      , [ Alcotest.test_case "statement sequence" `Quick test_payment_statement_sequence
        ; Alcotest.test_case
            "bad credit rewrites c_data"
            `Quick
            test_payment_bad_credit_rewrites_c_data
        ; Alcotest.test_case
            "by last name takes the middle row"
            `Quick
            test_payment_by_last_name_takes_the_middle_row
        ] )
    ; ( "order_status"
      , [ Alcotest.test_case "read-only" `Quick test_order_status_is_read_only ] )
    ; ( "delivery"
      , [ Alcotest.test_case "ten transactions" `Quick test_delivery_runs_ten_transactions
        ; Alcotest.test_case
            "skips an empty district"
            `Quick
            test_delivery_skips_a_district_with_no_new_order
        ; Alcotest.test_case
            "an empty MIN result raises"
            `Quick
            test_delivery_empty_result_raises
        ] )
    ; ( "stock_level"
      , [ Alcotest.test_case "read-only" `Quick test_stock_level_is_read_only ] )
    ; ( "all profiles"
      , [ Alcotest.test_case
            "wrapped in a transaction"
            `Quick
            test_every_profile_is_wrapped
        ] )
    ; ( "required reads"
      , [ Alcotest.test_case
            "missing column raises"
            `Quick
            test_missing_required_column_raises
        ; Alcotest.test_case
            "unparseable column raises"
            `Quick
            test_unparseable_required_column_raises
        ; Alcotest.test_case
            "engine failure rolls back and re-raises"
            `Quick
            test_engine_failure_rolls_back_and_reraises
        ; Alcotest.test_case
            "delivery rolls back only its district"
            `Quick
            test_delivery_failure_rolls_back_only_its_district
        ; Alcotest.test_case
            "an unused-but-required warehouse read raises when empty"
            `Quick
            test_new_order_empty_warehouse_read_raises
        ; Alcotest.test_case
            "an unused-but-required customer read raises when empty"
            `Quick
            test_new_order_empty_customer_read_raises
        ; Alcotest.test_case
            "an unused-but-required order_line read raises when empty"
            `Quick
            test_order_status_empty_lines_raises
        ] )
    ; ( "properties"
      , [ QCheck_alcotest.to_alcotest prop_gen_input_stays_in_the_generated_population ] )
    ]
;;
