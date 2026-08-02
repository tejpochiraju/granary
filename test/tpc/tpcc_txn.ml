type verdict =
  | Native
  | Rewritten of string
  | Skipped of string

type kind =
  | New_order
  | Payment
  | Order_status
  | Delivery
  | Stock_level

type profile =
  { kind : kind
  ; name : string
  ; weight : int
  ; verdict : verdict
  }

(* MEASURED, not assumed: every verdict below is what test_tpcc_smoke.ml
   observed running the profile 25 times — its default per-profile count —
   against a loaded W=1 granary database, with the four clause-3.3 consistency
   conditions checked before and after and the intentional-rollback NewOrder
   forced.  That test is gated behind GRANARY_TPCC_SMOKE; its header comment
   gives the command.  Four profiles run the spec's statements untouched; only
   StockLevel needed a documented rewrite. *)
let stock_level_verdict =
  Rewritten
    "#491: granary's parser rejects DISTINCT as an aggregate argument, so the spec's \
     COUNT(DISTINCT s_i_id) is a parse error and there are no derived tables to wrap it \
     in. The join, the 20-order window, the s_quantity threshold and the duplicate \
     elimination all still run in the engine as SELECT DISTINCT s_i_id; only the final \
     COUNT of the deduplicated ids is taken client-side as the row count. The spec's \
     comma-join is also spelled INNER JOIN, since granary's FROM clause takes one table \
     plus explicit joins."
;;

let all =
  [ { kind = New_order; name = "new_order"; weight = 45; verdict = Native }
  ; { kind = Payment; name = "payment"; weight = 43; verdict = Native }
  ; { kind = Order_status; name = "order_status"; weight = 4; verdict = Native }
  ; { kind = Delivery; name = "delivery"; weight = 4; verdict = Native }
  ; { kind = Stock_level
    ; name = "stock_level"
    ; weight = 4
    ; verdict = stock_level_verdict
    }
  ]
;;

let verdict_label = function
  | Native -> "native"
  | Rewritten _ -> "rewritten"
  | Skipped _ -> "skipped"
;;

let is_skipped p =
  match p.verdict with
  | Skipped _ -> true
  | Native | Rewritten _ -> false
;;

let pick r profiles =
  let runnable = List.filter (fun p -> not (is_skipped p)) profiles in
  if runnable = []
  then invalid_arg "Tpcc_txn.pick: no runnable profile"
  else (
    let total = List.fold_left (fun a p -> a + p.weight) 0 runnable in
    let draw = Tpc_rand.int_between r ~lo:1 ~hi:total in
    let rec walk remaining = function
      | [] -> assert false (* total was the sum of these weights *)
      | p :: rest ->
        let remaining = remaining - p.weight in
        if remaining <= 0 then p else walk remaining rest
    in
    walk draw runnable)
;;

type new_order_line =
  { ol_i_id : int
  ; ol_supply_w_id : int
  ; ol_quantity : int
  }

type customer_selector =
  | By_id of int
  | By_last_name of string

type input =
  | New_order_input of
      { w_id : int
      ; d_id : int
      ; c_id : int
      ; lines : new_order_line list
      ; rollback : bool
      }
  | Payment_input of
      { w_id : int
      ; d_id : int
      ; customer_w_id : int
      ; customer_d_id : int
      ; customer : customer_selector
      ; amount : float
      }
  | Order_status_input of
      { w_id : int
      ; d_id : int
      ; customer : customer_selector
      }
  | Delivery_input of
      { w_id : int
      ; carrier_id : int
      }
  | Stock_level_input of
      { w_id : int
      ; d_id : int
      ; threshold : int
      }

let pp_customer_selector fmt = function
  | By_id id -> Format.fprintf fmt "By_id %d" id
  | By_last_name name -> Format.fprintf fmt "By_last_name %S" name
;;

let pp_line fmt { ol_i_id; ol_supply_w_id; ol_quantity } =
  Format.fprintf
    fmt
    "{ ol_i_id = %d; ol_supply_w_id = %d; ol_quantity = %d }"
    ol_i_id
    ol_supply_w_id
    ol_quantity
;;

let pp fmt = function
  | New_order_input { w_id; d_id; c_id; lines; rollback } ->
    Format.fprintf
      fmt
      "New_order_input { w_id = %d; d_id = %d; c_id = %d; lines = [%a]; rollback = %b }"
      w_id
      d_id
      c_id
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") pp_line)
      lines
      rollback
  | Payment_input { w_id; d_id; customer_w_id; customer_d_id; customer; amount } ->
    Format.fprintf
      fmt
      "Payment_input { w_id = %d; d_id = %d; customer_w_id = %d; customer_d_id = %d; \
       customer = %a; amount = %f }"
      w_id
      d_id
      customer_w_id
      customer_d_id
      pp_customer_selector
      customer
      amount
  | Order_status_input { w_id; d_id; customer } ->
    Format.fprintf
      fmt
      "Order_status_input { w_id = %d; d_id = %d; customer = %a }"
      w_id
      d_id
      pp_customer_selector
      customer
  | Delivery_input { w_id; carrier_id } ->
    Format.fprintf fmt "Delivery_input { w_id = %d; carrier_id = %d }" w_id carrier_id
  | Stock_level_input { w_id; d_id; threshold } ->
    Format.fprintf
      fmt
      "Stock_level_input { w_id = %d; d_id = %d; threshold = %d }"
      w_id
      d_id
      threshold
;;

(* --- the Tpcc_gen seam ------------------------------------------------ *)

(* Every cardinality the input generator draws against is DERIVED from
   Tpcc_gen rather than re-spelled here. The load and the workload have to
   agree: a district id past Tpcc_gen.districts_per_warehouse, or a customer
   id past Tpcc_gen.customers_per_district, addresses a row that was never
   generated, and the profile would measure misses while still reporting
   throughput. Aliasing them means a change to the population retargets the
   workload instead of silently splitting the two apart. *)
let districts_per_warehouse = Tpcc_gen.districts_per_warehouse
let customers_per_district = Tpcc_gen.customers_per_district
let items = Tpcc_gen.items

(* An item id one past Tpcc_gen.items, so it cannot match any generated row.
   Used only to build New_order's intentional-rollback case. *)
let invalid_item_id = items + 1

(* --- the NURand run constants (TPC-C 2.1.6.1) -------------------------- *)

type run_constants =
  { nurand_c_id : int
  ; nurand_ol_i_id : int
  ; nurand_c_last : int
  }

(* Clause 2.1.6.1 chooses the three NURand run constants INDEPENDENTLY, each
   uniformly within [0, A] for its own A. An earlier version of this module
   threaded one shared [constant_c] into all three uses, which is not what the
   spec says and, for c_last, is precisely what it forbids.

   c_id (A = 1023) and ol_i_id (A = 8191) have no constraint beyond their
   range. The values below are arbitrary but FIXED rather than drawn from the
   generator: a run constant drawn from the same stream as the workload would
   make the constant depend on how many draws preceded it, so an unrelated
   change to a profile's draw order would silently retarget the hot set. Fixed
   constants keep a seed pinning the workload. *)
let c_id_run = 987
let ol_i_id_run = 5711

(* c_last (A = 255) is the constrained one: the RUN constant must differ from
   the LOAD constant C_LOAD by a delta in [65, 119], excluding 96 and 112. The
   point of the rule is that the run's hot surnames must NOT coincide with the
   load's — a benchmark whose lookups concentrate on exactly the names the
   loader concentrated on measures a cache-friendlier workload than TPC-C
   intends.

   85 is chosen: it sits mid-range in [65, 119] and is far from both excluded
   values (96 and 112), so a future change to Tpcc_gen.c_load cannot drift the
   delta onto one of them. The sign is chosen to keep the result inside
   [0, 255] — subtract when there is room below, add otherwise — which for
   c_load = 173 gives 88. *)
let c_last_run_delta = 85

let c_last_run_of_load ~c_load =
  if c_load >= c_last_run_delta
  then c_load - c_last_run_delta
  else c_load + c_last_run_delta
;;

let default_run_constants =
  { nurand_c_id = c_id_run
  ; nurand_ol_i_id = ol_i_id_run
  ; nurand_c_last = c_last_run_of_load ~c_load:Tpcc_gen.c_load
  }
;;

(* [other_warehouse r ~warehouses ~w_id] is uniform over
   [{1..warehouses} \ {w_id}]. Requires [warehouses > 1]; callers only reach
   this after checking that. *)
let other_warehouse r ~warehouses ~w_id =
  let n = Tpc_rand.int_between r ~lo:1 ~hi:(warehouses - 1) in
  if n < w_id then n else n + 1
;;

(* Every draw below is bound to its own [let], in the order it is meant to
   happen — see Tpcc_gen's identical discipline and the reasoning at the top
   of the .mli. *)

let customer_by_id r ~constants =
  Tpc_rand.nurand r ~a:1023 ~x:1 ~y:customers_per_district ~c:constants.nurand_c_id
;;

let customer_by_last_name r ~constants =
  Tpc_rand.last_name (Tpc_rand.nurand r ~a:255 ~x:0 ~y:999 ~c:constants.nurand_c_last)
;;

let gen_customer_selector r ~constants =
  let by_id_roll = Tpc_rand.int_between r ~lo:1 ~hi:100 in
  if by_id_roll <= 40
  then By_id (customer_by_id r ~constants)
  else By_last_name (customer_by_last_name r ~constants)
;;

let gen_new_order_line r ~warehouses ~w_id ~constants =
  let i_id = Tpc_rand.nurand r ~a:8191 ~x:1 ~y:items ~c:constants.nurand_ol_i_id in
  let is_remote = warehouses > 1 && Tpc_rand.int_between r ~lo:1 ~hi:100 = 1 in
  let supply_w_id = if is_remote then other_warehouse r ~warehouses ~w_id else w_id in
  let quantity = Tpc_rand.int_between r ~lo:1 ~hi:10 in
  { ol_i_id = i_id; ol_supply_w_id = supply_w_id; ol_quantity = quantity }
;;

(* Explicit recursion rather than [List.init].  Each line consumes draws from
   [r], so the list's contents depend on the order the elements are built in.
   [List.init] is documented "evaluated left to right" on the toolchain this
   project pins, but stating the order here keeps the reproducibility contract
   local rather than inherited from a stdlib guarantee (#509) — the same choice
   Tpch_gen.build_lines and Tpc_rand.a_string make. *)
let rec build_lines r ~warehouses ~w_id ~constants ~remaining acc =
  if remaining <= 0
  then List.rev acc
  else (
    let line = gen_new_order_line r ~warehouses ~w_id ~constants in
    build_lines r ~warehouses ~w_id ~constants ~remaining:(remaining - 1) (line :: acc))
;;

let invalidate_last lines =
  match List.rev lines with
  | [] -> lines
  | last :: rest -> List.rev ({ last with ol_i_id = invalid_item_id } :: rest)
;;

let gen_new_order r ~warehouses ~constants =
  let w_id = Tpc_rand.int_between r ~lo:1 ~hi:warehouses in
  let d_id = Tpc_rand.int_between r ~lo:1 ~hi:districts_per_warehouse in
  let c_id =
    Tpc_rand.nurand r ~a:1023 ~x:1 ~y:customers_per_district ~c:constants.nurand_c_id
  in
  let ol_cnt = Tpc_rand.int_between r ~lo:5 ~hi:15 in
  let lines = build_lines r ~warehouses ~w_id ~constants ~remaining:ol_cnt [] in
  let rollback_roll = Tpc_rand.int_between r ~lo:1 ~hi:100 in
  let rollback = rollback_roll = 1 in
  let lines = if rollback then invalidate_last lines else lines in
  New_order_input { w_id; d_id; c_id; lines; rollback }
;;

let gen_payment r ~warehouses ~constants =
  let w_id = Tpc_rand.int_between r ~lo:1 ~hi:warehouses in
  let d_id = Tpc_rand.int_between r ~lo:1 ~hi:districts_per_warehouse in
  let is_remote = warehouses > 1 && Tpc_rand.int_between r ~lo:1 ~hi:100 <= 15 in
  let customer_w_id = if is_remote then other_warehouse r ~warehouses ~w_id else w_id in
  let customer_d_id =
    if is_remote then Tpc_rand.int_between r ~lo:1 ~hi:districts_per_warehouse else d_id
  in
  let customer = gen_customer_selector r ~constants in
  let amount = Tpc_rand.float_between r ~lo:1.0 ~hi:5_000.0 ~decimals:2 in
  Payment_input { w_id; d_id; customer_w_id; customer_d_id; customer; amount }
;;

let gen_order_status r ~warehouses ~constants =
  let w_id = Tpc_rand.int_between r ~lo:1 ~hi:warehouses in
  let d_id = Tpc_rand.int_between r ~lo:1 ~hi:districts_per_warehouse in
  let customer = gen_customer_selector r ~constants in
  Order_status_input { w_id; d_id; customer }
;;

let gen_delivery r ~warehouses =
  let w_id = Tpc_rand.int_between r ~lo:1 ~hi:warehouses in
  let carrier_id = Tpc_rand.int_between r ~lo:1 ~hi:10 in
  Delivery_input { w_id; carrier_id }
;;

let gen_stock_level r ~warehouses =
  let w_id = Tpc_rand.int_between r ~lo:1 ~hi:warehouses in
  let d_id = Tpc_rand.int_between r ~lo:1 ~hi:districts_per_warehouse in
  let threshold = Tpc_rand.int_between r ~lo:10 ~hi:20 in
  Stock_level_input { w_id; d_id; threshold }
;;

let gen_input r ~warehouses ~constants profile =
  match profile.kind with
  | New_order -> gen_new_order r ~warehouses ~constants
  | Payment -> gen_payment r ~warehouses ~constants
  | Order_status -> gen_order_status r ~warehouses ~constants
  | Delivery -> gen_delivery r ~warehouses
  | Stock_level -> gen_stock_level r ~warehouses
;;

(* --- statements ------------------------------------------------------- *)

type stmt =
  { sql : string
  ; params : Tpc_value.t list
  }

type row = string list

type ops =
  { query : stmt -> row list Lwt.t
  ; exec : stmt -> unit Lwt.t
  }

let count_placeholders sql =
  String.fold_left (fun n c -> if c = '?' then n + 1 else n) 0 sql
;;

(* [substitute] and [subst_param] walk [sql] once; the arity check in [render]
   has already established that a parameter is available at every [?], so the
   empty-list case below is unreachable. *)
let rec substitute buf sql i params =
  if i >= String.length sql
  then ()
  else if Char.equal sql.[i] '?'
  then subst_param buf sql i params
  else (
    Buffer.add_char buf sql.[i];
    substitute buf sql (i + 1) params)

and subst_param buf sql i params =
  match params with
  | [] -> ()
  | v :: rest ->
    Buffer.add_string buf (Tpc_value.literal v);
    substitute buf sql (i + 1) rest
;;

let render { sql; params } =
  let n_placeholders = count_placeholders sql in
  let n_params = List.length params in
  if n_placeholders <> n_params
  then
    invalid_arg
      (Printf.sprintf
         "Tpcc_txn.render: %d placeholders but %d parameter(s)"
         n_placeholders
         n_params);
  let buf = Buffer.create (String.length sql + (16 * n_params)) in
  substitute buf sql 0 params;
  Buffer.contents buf
;;

let stmt sql params = { sql; params }
let begin_txn = { sql = "BEGIN"; params = [] }
let commit_txn = { sql = "COMMIT"; params = [] }
let rollback_txn = { sql = "ROLLBACK"; params = [] }

(* --- reading rows back ------------------------------------------------ *)

let ( let* ) = Lwt.bind

(* An engine renders every column as text, so the profiles parse the few
   values they actually feed back into later statements.

   Every such read is REQUIRED: it is reached only where the spec guarantees a
   row exists (the district of a warehouse being ordered from, the item the
   line lookup just matched, the order the new_order row just named).  A
   default here would not keep a drifted population running — it would turn a
   broken read into a plausible number and commit it.  A missing d_next_o_id
   defaulting to 1 inserts an order at a colliding id after the counter was
   already bumped; a missing customer id defaulting to 0 moves w_ytd and d_ytd
   while crediting nobody, which even Tpcc_check's condition 1 cannot see
   because both columns moved together.  So these raise, naming the column and
   the statement that should have produced it.

   The one genuine absence — Delivery's MIN(no_o_id) over a district with no
   undelivered order — is SQL NULL by design and is read through
   [delivery_oldest_o_id], which reports that one NULL as [None]. It still
   raises on zero rows: MIN always returns exactly one row, so no rows means
   the query broke, not that the queue is empty. *)

exception Missing_value of string

let first = function
  | r :: _ -> r
  | [] -> []
;;

let missing_value ~source ~column =
  raise
    (Missing_value (Printf.sprintf "Tpcc_txn: %s returned no usable %s" source column))
;;

let required_text row n ~source ~column =
  match List.nth_opt row n with
  | None -> missing_value ~source ~column
  | Some s -> s
;;

let required_int row n ~source ~column =
  match int_of_string_opt (String.trim (required_text row n ~source ~column)) with
  | None -> missing_value ~source ~column
  | Some i -> i
;;

let required_float row n ~source ~column =
  match float_of_string_opt (String.trim (required_text row n ~source ~column)) with
  | None -> missing_value ~source ~column
  | Some f -> f
;;

(* Some statements are run for their rows without any column being fed into a
   later statement — NewOrder's w_tax and customer lookups, OrderStatus's
   order lines. Discarding the result entirely would absorb a zero-row read
   silently, which is exactly the failure the required_* family exists to
   surface: the profile would commit, report success, and have done nothing.
   Every one of these reads a row the spec guarantees exists, so an empty
   result is a broken read and must raise like any other. *)
let required_rows rows ~source ~column =
  match rows with
  | [] -> missing_value ~source ~column
  | _ :: _ -> rows
;;

(* A profile owns its transaction boundary, so it owns the failure path too:
   if any statement rejects — including a [Missing_value] raised by the reads
   above — the transaction must be closed before the exception leaves [run].
   Otherwise it escapes with the transaction still open and granary's
   non-reentrant single-writer lock still held, which is the same permanent
   hang the .mli warns about, arriving by a different door.  A failure of the
   ROLLBACK itself is swallowed (there may be no transaction to roll back, if
   the BEGIN is what failed) so that the original exception is what
   propagates. *)
let with_rollback ops body =
  Lwt.catch body (fun exn ->
    let* () = Lwt.catch (fun () -> ops.exec rollback_txn) (fun _ -> Lwt.return_unit) in
    Lwt.reraise exn)
;;

(* Every timestamp column the transactions write.  UTC, and in the same
   "YYYY-MM-DD hh:mm:ss" shape Tpcc_gen's load timestamps use, so a delivered
   order's dates sort against the loaded population. *)
let timestamp () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  Printf.sprintf
    "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec
;;

(* --- New_order -------------------------------------------------------- *)

(* stock carries one 24-character district string per district; the order line
   copies the one belonging to the ordering district.  The column name is part
   of the statement text rather than a parameter because it is an identifier,
   and d_id is always in [1,10] (Tpcc_txn.gen_input and Tpcc_gen agree). *)
let stock_select_sql ~d_id =
  Printf.sprintf
    "SELECT s_quantity, s_dist_%02d, s_data FROM stock WHERE s_w_id = ? AND s_i_id = ?"
    d_id
;;

let order_line_insert_sql =
  "INSERT INTO order_line (ol_o_id, ol_d_id, ol_w_id, ol_number, ol_i_id, \
   ol_supply_w_id, ol_delivery_d, ol_quantity, ol_amount, ol_dist_info) VALUES \
   (?,?,?,?,?,?,?,?,?,?)"
;;

(* The spec's stock rule: draw the quantity down, and top the item back up by
   91 when it would fall below 10. *)
let new_stock_quantity ~s_quantity ~ol_quantity =
  let drawn = s_quantity - ol_quantity in
  if drawn >= 10 then drawn else drawn + 91
;;

let new_order_line_found ops ~w_id ~d_id ~o_id ~number ~line ~item =
  let i_price = required_float item 0 ~source:"NewOrder item lookup" ~column:"i_price" in
  let* stock_rows =
    ops.query
      (stmt
         (stock_select_sql ~d_id)
         [ Tpc_value.VInt line.ol_supply_w_id; Tpc_value.VInt line.ol_i_id ])
  in
  let stock = first stock_rows in
  let quantity =
    new_stock_quantity
      ~s_quantity:
        (required_int stock 0 ~source:"NewOrder stock lookup" ~column:"s_quantity")
      ~ol_quantity:line.ol_quantity
  in
  let remote = if line.ol_supply_w_id = w_id then 0 else 1 in
  let* () =
    ops.exec
      (stmt
         "UPDATE stock SET s_quantity = ?, s_ytd = s_ytd + ?, s_order_cnt = s_order_cnt \
          + 1, s_remote_cnt = s_remote_cnt + ? WHERE s_w_id = ? AND s_i_id = ?"
         [ Tpc_value.VInt quantity
         ; Tpc_value.VInt line.ol_quantity
         ; Tpc_value.VInt remote
         ; Tpc_value.VInt line.ol_supply_w_id
         ; Tpc_value.VInt line.ol_i_id
         ])
  in
  let* () =
    ops.exec
      (stmt
         order_line_insert_sql
         [ Tpc_value.VInt o_id
         ; Tpc_value.VInt d_id
         ; Tpc_value.VInt w_id
         ; Tpc_value.VInt number
         ; Tpc_value.VInt line.ol_i_id
         ; Tpc_value.VInt line.ol_supply_w_id
         ; Tpc_value.VNull (* ol_delivery_d: set by Delivery, not here *)
         ; Tpc_value.VInt line.ol_quantity
         ; Tpc_value.VReal (float_of_int line.ol_quantity *. i_price)
         ; Tpc_value.VText
             (required_text stock 1 ~source:"NewOrder stock lookup" ~column:"s_dist_NN")
         ])
  in
  Lwt.return_true
;;

(* [false] means the item id matched no row — the spec's 1% rollback case. *)
let new_order_line ops ~w_id ~d_id ~o_id ~number ~line =
  let* item_rows =
    ops.query
      (stmt
         "SELECT i_price, i_name, i_data FROM item WHERE i_id = ?"
         [ Tpc_value.VInt line.ol_i_id ])
  in
  match item_rows with
  | [] -> Lwt.return_false
  | item :: _ -> new_order_line_found ops ~w_id ~d_id ~o_id ~number ~line ~item
;;

let rec new_order_lines ops ~w_id ~d_id ~o_id ~number lines =
  match lines with
  | [] -> Lwt.return_true
  | line :: rest ->
    let* ok = new_order_line ops ~w_id ~d_id ~o_id ~number ~line in
    if ok
    then new_order_lines ops ~w_id ~d_id ~o_id ~number:(number + 1) rest
    else Lwt.return_false
;;

let orders_insert_sql =
  "INSERT INTO orders (o_id, o_d_id, o_w_id, o_c_id, o_entry_d, o_carrier_id, o_ol_cnt, \
   o_all_local) VALUES (?,?,?,?,?,?,?,?)"
;;

let new_order_body ops ~w_id ~d_id ~c_id ~lines =
  let* () = ops.exec begin_txn in
  let* warehouse =
    ops.query (stmt "SELECT w_tax FROM warehouse WHERE w_id = ?" [ Tpc_value.VInt w_id ])
  in
  (* w_tax feeds the spec's order total, which this profile does not report —
     but the read is still required: the warehouse being ordered from always
     exists, so no row here means the lookup broke. *)
  let (_ : float) =
    required_float (first warehouse) 0 ~source:"NewOrder warehouse lookup" ~column:"w_tax"
  in
  let* district =
    ops.query
      (stmt
         "SELECT d_tax, d_next_o_id FROM district WHERE d_w_id = ? AND d_id = ?"
         [ Tpc_value.VInt w_id; Tpc_value.VInt d_id ])
  in
  let o_id =
    required_int
      (first district)
      1
      ~source:"NewOrder district lookup"
      ~column:"d_next_o_id"
  in
  let* () =
    ops.exec
      (stmt
         "UPDATE district SET d_next_o_id = d_next_o_id + 1 WHERE d_w_id = ? AND d_id = ?"
         [ Tpc_value.VInt w_id; Tpc_value.VInt d_id ])
  in
  let* customer =
    ops.query
      (stmt
         "SELECT c_discount, c_last, c_credit FROM customer WHERE c_w_id = ? AND c_d_id \
          = ? AND c_id = ?"
         [ Tpc_value.VInt w_id; Tpc_value.VInt d_id; Tpc_value.VInt c_id ])
  in
  (* c_discount likewise feeds only the unreported total, but c_id came from
     gen_input's NURand over [1, customers_per_district] and every such
     customer was loaded, so an empty result is a broken read — and, given
     #508, exactly the composite-key lookup most likely to regress. *)
  let (_ : float) =
    required_float
      (first customer)
      0
      ~source:"NewOrder customer lookup"
      ~column:"c_discount"
  in
  let all_local =
    if List.for_all (fun l -> l.ol_supply_w_id = w_id) lines then 1 else 0
  in
  let* () =
    ops.exec
      (stmt
         orders_insert_sql
         [ Tpc_value.VInt o_id
         ; Tpc_value.VInt d_id
         ; Tpc_value.VInt w_id
         ; Tpc_value.VInt c_id
         ; Tpc_value.VText (timestamp ())
         ; Tpc_value.VNull (* o_carrier_id: undelivered until Delivery runs *)
         ; Tpc_value.VInt (List.length lines)
         ; Tpc_value.VInt all_local
         ])
  in
  let* () =
    ops.exec
      (stmt
         "INSERT INTO new_order (no_o_id, no_d_id, no_w_id) VALUES (?,?,?)"
         [ Tpc_value.VInt o_id; Tpc_value.VInt d_id; Tpc_value.VInt w_id ])
  in
  let* ok = new_order_lines ops ~w_id ~d_id ~o_id ~number:1 lines in
  ops.exec (if ok then commit_txn else rollback_txn)
;;

let run_new_order ops ~w_id ~d_id ~c_id ~lines =
  with_rollback ops (fun () -> new_order_body ops ~w_id ~d_id ~c_id ~lines)
;;

(* --- customer resolution (Payment, Order_status) ---------------------- *)

(* The by-name lookup returns every customer sharing the last name within the
   district, ordered by first name; the spec takes the middle one. *)
let middle_row rows =
  let n = List.length rows in
  if n = 0 then [] else List.nth rows ((n - 1) / 2)
;;

let resolve_customer ops ~w_id ~d_id ~cols customer =
  match customer with
  | By_id c_id ->
    let* rows =
      ops.query
        (stmt
           (Printf.sprintf
              "SELECT %s FROM customer WHERE c_w_id = ? AND c_d_id = ? AND c_id = ?"
              cols)
           [ Tpc_value.VInt w_id; Tpc_value.VInt d_id; Tpc_value.VInt c_id ])
    in
    Lwt.return (first rows)
  | By_last_name name ->
    let* rows =
      ops.query
        (stmt
           (Printf.sprintf
              "SELECT %s FROM customer WHERE c_w_id = ? AND c_d_id = ? AND c_last = ? \
               ORDER BY c_first"
              cols)
           [ Tpc_value.VInt w_id; Tpc_value.VInt d_id; Tpc_value.VText name ])
    in
    Lwt.return (middle_row rows)
;;

(* --- Payment ---------------------------------------------------------- *)

let payment_customer_cols =
  "c_id, c_first, c_middle, c_last, c_balance, c_ytd_payment, c_payment_cnt, c_credit, \
   c_data"
;;

(* A bad-credit customer's payment history is prepended to c_data, which the
   spec caps at 500 characters. *)
let bad_credit_data ~c_id ~c_d_id ~c_w_id ~d_id ~w_id ~amount ~old =
  let s =
    Printf.sprintf "%d %d %d %d %d %.2f %s" c_id c_d_id c_w_id d_id w_id amount old
  in
  if String.length s > 500 then String.sub s 0 500 else s
;;

let payment_customer_update ~c_w_id ~c_d_id ~c_id ~amount ~new_data =
  let where = [ Tpc_value.VInt c_w_id; Tpc_value.VInt c_d_id; Tpc_value.VInt c_id ] in
  match new_data with
  | None ->
    stmt
      "UPDATE customer SET c_balance = c_balance - ?, c_ytd_payment = c_ytd_payment + ?, \
       c_payment_cnt = c_payment_cnt + 1 WHERE c_w_id = ? AND c_d_id = ? AND c_id = ?"
      (Tpc_value.VReal amount :: Tpc_value.VReal amount :: where)
  | Some data ->
    stmt
      "UPDATE customer SET c_balance = c_balance - ?, c_ytd_payment = c_ytd_payment + ?, \
       c_payment_cnt = c_payment_cnt + 1, c_data = ? WHERE c_w_id = ? AND c_d_id = ? AND \
       c_id = ?"
      (Tpc_value.VReal amount :: Tpc_value.VReal amount :: Tpc_value.VText data :: where)
;;

let payment_body ops ~w_id ~d_id ~customer_w_id ~customer_d_id ~customer ~amount =
  let* () = ops.exec begin_txn in
  let* () =
    ops.exec
      (stmt
         "UPDATE warehouse SET w_ytd = w_ytd + ? WHERE w_id = ?"
         [ Tpc_value.VReal amount; Tpc_value.VInt w_id ])
  in
  let* w =
    ops.query (stmt "SELECT w_name FROM warehouse WHERE w_id = ?" [ Tpc_value.VInt w_id ])
  in
  let* () =
    ops.exec
      (stmt
         "UPDATE district SET d_ytd = d_ytd + ? WHERE d_w_id = ? AND d_id = ?"
         [ Tpc_value.VReal amount; Tpc_value.VInt w_id; Tpc_value.VInt d_id ])
  in
  let* d =
    ops.query
      (stmt
         "SELECT d_name FROM district WHERE d_w_id = ? AND d_id = ?"
         [ Tpc_value.VInt w_id; Tpc_value.VInt d_id ])
  in
  let* c =
    resolve_customer
      ops
      ~w_id:customer_w_id
      ~d_id:customer_d_id
      ~cols:payment_customer_cols
      customer
  in
  let c_id = required_int c 0 ~source:"Payment customer lookup" ~column:"c_id" in
  let credit = required_text c 7 ~source:"Payment customer lookup" ~column:"c_credit" in
  let new_data =
    if String.equal credit "BC"
    then
      Some
        (bad_credit_data
           ~c_id
           ~c_d_id:customer_d_id
           ~c_w_id:customer_w_id
           ~d_id
           ~w_id
           ~amount
           ~old:(required_text c 8 ~source:"Payment customer lookup" ~column:"c_data"))
    else None
  in
  let* () =
    ops.exec
      (payment_customer_update
         ~c_w_id:customer_w_id
         ~c_d_id:customer_d_id
         ~c_id
         ~amount
         ~new_data)
  in
  let h_data =
    required_text (first w) 0 ~source:"Payment warehouse lookup" ~column:"w_name"
    ^ "    "
    ^ required_text (first d) 0 ~source:"Payment district lookup" ~column:"d_name"
  in
  let* () =
    ops.exec
      (stmt
         "INSERT INTO history (h_c_id, h_c_d_id, h_c_w_id, h_d_id, h_w_id, h_date, \
          h_amount, h_data) VALUES (?,?,?,?,?,?,?,?)"
         [ Tpc_value.VInt c_id
         ; Tpc_value.VInt customer_d_id
         ; Tpc_value.VInt customer_w_id
         ; Tpc_value.VInt d_id
         ; Tpc_value.VInt w_id
         ; Tpc_value.VText (timestamp ())
         ; Tpc_value.VReal amount
         ; Tpc_value.VText h_data
         ])
  in
  ops.exec commit_txn
;;

let run_payment ops ~w_id ~d_id ~customer_w_id ~customer_d_id ~customer ~amount =
  with_rollback ops (fun () ->
    payment_body ops ~w_id ~d_id ~customer_w_id ~customer_d_id ~customer ~amount)
;;

(* --- Order_status ----------------------------------------------------- *)

let order_status_customer_cols = "c_id, c_first, c_middle, c_last, c_balance"

let order_status_body ops ~w_id ~d_id ~customer =
  let* () = ops.exec begin_txn in
  let* c = resolve_customer ops ~w_id ~d_id ~cols:order_status_customer_cols customer in
  let c_id = required_int c 0 ~source:"OrderStatus customer lookup" ~column:"c_id" in
  let* orders =
    ops.query
      (stmt
         "SELECT o_id, o_entry_d, o_carrier_id FROM orders WHERE o_w_id = ? AND o_d_id = \
          ? AND o_c_id = ? ORDER BY o_id DESC LIMIT 1"
         [ Tpc_value.VInt w_id; Tpc_value.VInt d_id; Tpc_value.VInt c_id ])
  in
  (* Every customer in the loaded population owns at least one order — o_c_id
     is a permutation of the district's customers — so an empty result here is
     a broken read, not a customer who has never ordered. *)
  let o_id =
    required_int (first orders) 0 ~source:"OrderStatus order lookup" ~column:"o_id"
  in
  let* lines =
    ops.query
      (stmt
         "SELECT ol_i_id, ol_supply_w_id, ol_quantity, ol_amount, ol_delivery_d FROM \
          order_line WHERE ol_w_id = ? AND ol_d_id = ? AND ol_o_id = ?"
         [ Tpc_value.VInt w_id; Tpc_value.VInt d_id; Tpc_value.VInt o_id ])
  in
  (* The spec reports these lines to the terminal; this profile does not, but
     every order carries between 5 and 15 of them, so zero rows is a broken
     read rather than an order without lines. *)
  let (_ : row list) =
    required_rows lines ~source:"OrderStatus order_line lookup" ~column:"order line"
  in
  ops.exec commit_txn
;;

let run_order_status ops ~w_id ~d_id ~customer =
  with_rollback ops (fun () -> order_status_body ops ~w_id ~d_id ~customer)
;;

(* --- Delivery --------------------------------------------------------- *)

let delivery_order ops ~w_id ~d_id ~carrier_id ~o_id =
  let order_key = [ Tpc_value.VInt w_id; Tpc_value.VInt d_id; Tpc_value.VInt o_id ] in
  let* () =
    ops.exec
      (stmt
         "DELETE FROM new_order WHERE no_w_id = ? AND no_d_id = ? AND no_o_id = ?"
         order_key)
  in
  let* owner =
    ops.query
      (stmt
         "SELECT o_c_id FROM orders WHERE o_w_id = ? AND o_d_id = ? AND o_id = ?"
         order_key)
  in
  (* The new_order row naming this order has already been deleted above, so a
     missing orders row here means the delete has orphaned it; crediting a
     defaulted customer would commit that corruption silently. *)
  let c_id =
    required_int (first owner) 0 ~source:"Delivery order lookup" ~column:"o_c_id"
  in
  let* () =
    ops.exec
      (stmt
         "UPDATE orders SET o_carrier_id = ? WHERE o_w_id = ? AND o_d_id = ? AND o_id = ?"
         (Tpc_value.VInt carrier_id :: order_key))
  in
  let* () =
    ops.exec
      (stmt
         "UPDATE order_line SET ol_delivery_d = ? WHERE ol_w_id = ? AND ol_d_id = ? AND \
          ol_o_id = ?"
         (Tpc_value.VText (timestamp ()) :: order_key))
  in
  let* total =
    ops.query
      (stmt
         "SELECT SUM(ol_amount) FROM order_line WHERE ol_w_id = ? AND ol_d_id = ? AND \
          ol_o_id = ?"
         order_key)
  in
  let* () =
    ops.exec
      (stmt
         "UPDATE customer SET c_balance = c_balance + ?, c_delivery_cnt = c_delivery_cnt \
          + 1 WHERE c_w_id = ? AND c_d_id = ? AND c_id = ?"
         [ Tpc_value.VReal
             (required_float
                (first total)
                0
                ~source:"Delivery order_line SUM"
                ~column:"SUM(ol_amount)")
         ; Tpc_value.VInt w_id
         ; Tpc_value.VInt d_id
         ; Tpc_value.VInt c_id
         ])
  in
  ops.exec commit_txn
;;

(* [MIN(no_o_id)] over a district always returns EXACTLY ONE row: a value
   when the district has an undelivered order, SQL NULL when it does not.
   Those two must not be conflated with a third case — zero rows — which can
   only mean the query itself broke. Reading them all through [int_opt_at]
   made a broken aggregate look identical to an empty queue, and Delivery
   would then commit an empty transaction, report success at full
   throughput, and leave all four consistency conditions holding while
   delivering nothing. So: no rows raises like any other required read, and
   only the NULL is the "nothing to deliver" branch. *)
let delivery_oldest_o_id rows =
  match rows with
  | [] ->
    missing_value ~source:"Delivery MIN(no_o_id)" ~column:"MIN(no_o_id) row"
    (* One row, holding either the id or SQL NULL. A value that is present
       but will not parse as an int is neither, and raises. *)
  | row :: _ ->
    (match List.nth_opt row 0 with
     | None -> missing_value ~source:"Delivery MIN(no_o_id)" ~column:"MIN(no_o_id)"
     | Some s ->
       let s = String.trim s in
       if String.equal s "NULL" || String.equal s ""
       then None
       else (
         match int_of_string_opt s with
         | Some o_id -> Some o_id
         | None -> missing_value ~source:"Delivery MIN(no_o_id)" ~column:"MIN(no_o_id)"))
;;

(* One district, in its own transaction: the spec makes Delivery ten separate
   transactions, not one. *)
let delivery_district_body ops ~w_id ~d_id ~carrier_id =
  let* () = ops.exec begin_txn in
  let* oldest =
    ops.query
      (stmt
         "SELECT MIN(no_o_id) FROM new_order WHERE no_w_id = ? AND no_d_id = ?"
         [ Tpc_value.VInt w_id; Tpc_value.VInt d_id ])
  in
  match delivery_oldest_o_id oldest with
  | None -> ops.exec commit_txn
  | Some o_id -> delivery_order ops ~w_id ~d_id ~carrier_id ~o_id
;;

let delivery_district ops ~w_id ~d_id ~carrier_id =
  with_rollback ops (fun () -> delivery_district_body ops ~w_id ~d_id ~carrier_id)
;;

let rec delivery_from ops ~w_id ~carrier_id ~d_id =
  if d_id > districts_per_warehouse
  then Lwt.return_unit
  else
    let* () = delivery_district ops ~w_id ~d_id ~carrier_id in
    delivery_from ops ~w_id ~carrier_id ~d_id:(d_id + 1)
;;

let run_delivery ops ~w_id ~carrier_id = delivery_from ops ~w_id ~carrier_id ~d_id:1

(* --- Stock_level ------------------------------------------------------ *)

(* REWRITTEN (#491): the spec's query is
   [SELECT COUNT(DISTINCT s_i_id) FROM order_line, stock WHERE ...].  granary
   accepts DISTINCT only in the SELECT-list position, not as an aggregate
   argument — [COUNT(DISTINCT ...)] is a parse error — and has no derived
   tables to wrap the DISTINCT in.  So the join, the 20-order window, the
   threshold filter and the duplicate elimination all still run in the engine;
   only the final COUNT of the deduplicated ids is taken as the row count on
   the client.  The comma-join is spelled INNER JOIN because granary's
   FROM clause takes a single table plus explicit joins. *)
let stock_level_sql =
  "SELECT DISTINCT s_i_id FROM order_line INNER JOIN stock ON s_i_id = ol_i_id WHERE \
   ol_w_id = ? AND ol_d_id = ? AND ol_o_id < ? AND ol_o_id >= ? AND s_w_id = ? AND \
   s_quantity < ?"
;;

let stock_level_window = 20

let stock_level_body ops ~w_id ~d_id ~threshold =
  let* () = ops.exec begin_txn in
  let* district =
    ops.query
      (stmt
         "SELECT d_next_o_id FROM district WHERE d_w_id = ? AND d_id = ?"
         [ Tpc_value.VInt w_id; Tpc_value.VInt d_id ])
  in
  let next_o_id =
    required_int
      (first district)
      0
      ~source:"StockLevel district lookup"
      ~column:"d_next_o_id"
  in
  let* low_stock =
    ops.query
      (stmt
         stock_level_sql
         [ Tpc_value.VInt w_id
         ; Tpc_value.VInt d_id
         ; Tpc_value.VInt next_o_id
         ; Tpc_value.VInt (next_o_id - stock_level_window)
         ; Tpc_value.VInt w_id
         ; Tpc_value.VInt threshold
         ])
  in
  (* StockLevel's answer is this count of distinct low-stock items.  [run]
     returns unit — the harness times the transaction rather than checking its
     result — so the count is computed and then deliberately discarded.  It is
     computed rather than skipped because forcing the row list is part of the
     work the profile exists to measure, and because a shape that cannot be
     counted is now a raise rather than a plausible number. *)
  let low_stock_count = List.length low_stock in
  ignore (low_stock_count : int);
  ops.exec commit_txn
;;

let run_stock_level ops ~w_id ~d_id ~threshold =
  with_rollback ops (fun () -> stock_level_body ops ~w_id ~d_id ~threshold)
;;

(* --- entry point ------------------------------------------------------ *)

let run ops input =
  match input with
  | New_order_input { w_id; d_id; c_id; lines; rollback = _ } ->
    (* [rollback] needs no branch here: gen_input has already replaced the
       last line's item id with one that matches no row, so the profile
       reaches its ROLLBACK through the same path a real miss would. *)
    run_new_order ops ~w_id ~d_id ~c_id ~lines
  | Payment_input { w_id; d_id; customer_w_id; customer_d_id; customer; amount } ->
    run_payment ops ~w_id ~d_id ~customer_w_id ~customer_d_id ~customer ~amount
  | Order_status_input { w_id; d_id; customer } ->
    run_order_status ops ~w_id ~d_id ~customer
  | Delivery_input { w_id; carrier_id } -> run_delivery ops ~w_id ~carrier_id
  | Stock_level_input { w_id; d_id; threshold } ->
    run_stock_level ops ~w_id ~d_id ~threshold
;;
