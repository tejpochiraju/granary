module V = Tpc_value

type t =
  { seed : int
  ; warehouses : int
  }

let tables =
  [ "warehouse"
  ; "district"
  ; "customer"
  ; "history"
  ; "item"
  ; "stock"
  ; "orders"
  ; "new_order"
  ; "order_line"
  ]
;;

let table_index table =
  match List.find_index (String.equal table) tables with
  | Some i -> i
  | None -> invalid_arg ("Tpcc_gen: unknown table " ^ table)
;;

(* Each table draws from its own stream so that generating one table alone
   yields the same rows as generating it inside a full load. *)
let rand_for t ~table = Tpc_rand.create ~seed:(t.seed + table_index table)

let create ~seed ~warehouses =
  if warehouses < 1 then invalid_arg "Tpcc_gen.create: warehouses must be positive";
  { seed; warehouses }
;;

let pp fmt t =
  Format.fprintf fmt "Tpcc_gen.t { seed = %d; warehouses = %d }" t.seed t.warehouses
;;

let warehouses t = t.warehouses

(* --- spec constants --------------------------------------------------- *)

let districts_per_warehouse = 10
let customers_per_district = 3_000
let orders_per_district = 3_000
let items = 100_000
let stock_per_warehouse = 100_000

(* Orders from this o_id up are undelivered: they carry no carrier id and are
   the ones with a new_order row.  3000 - 2101 + 1 = 900 per district, which is
   consistency condition 3. *)
let first_undelivered_o_id = 2_101
let new_orders_per_district = orders_per_district - first_undelivered_o_id + 1

(* Consistency condition 1: w_ytd = sum of the warehouse's ten d_ytd. *)
let d_ytd = 30_000.0
let w_ytd = d_ytd *. float_of_int districts_per_warehouse

(* Consistency condition 2: d_next_o_id - 1 = max o_id in the district. *)
let d_next_o_id = orders_per_district + 1

(* Every timestamp in the initial population is the same load time.  TPC-C
   gives no meaning to the spread of load timestamps, so a constant avoids
   pulling date arithmetic into a generator that has no use for it. *)
let load_time = "2026-01-01 00:00:00"

(* The spec's C_LOAD: the per-run NURand constant used for c_last.  Fixed here
   rather than drawn, so that the transaction drivers can use the same value
   without threading generator state through them. *)
let c_load = 173

(* --- column names ----------------------------------------------------- *)

let column_names ~table =
  match table with
  | "warehouse" ->
    [ "w_id"
    ; "w_name"
    ; "w_street_1"
    ; "w_street_2"
    ; "w_city"
    ; "w_state"
    ; "w_zip"
    ; "w_tax"
    ; "w_ytd"
    ]
  | "district" ->
    [ "d_id"
    ; "d_w_id"
    ; "d_name"
    ; "d_street_1"
    ; "d_street_2"
    ; "d_city"
    ; "d_state"
    ; "d_zip"
    ; "d_tax"
    ; "d_ytd"
    ; "d_next_o_id"
    ]
  | "customer" ->
    [ "c_id"
    ; "c_d_id"
    ; "c_w_id"
    ; "c_first"
    ; "c_middle"
    ; "c_last"
    ; "c_street_1"
    ; "c_street_2"
    ; "c_city"
    ; "c_state"
    ; "c_zip"
    ; "c_phone"
    ; "c_since"
    ; "c_credit"
    ; "c_credit_lim"
    ; "c_discount"
    ; "c_balance"
    ; "c_ytd_payment"
    ; "c_payment_cnt"
    ; "c_delivery_cnt"
    ; "c_data"
    ]
  | "history" ->
    [ "h_c_id"
    ; "h_c_d_id"
    ; "h_c_w_id"
    ; "h_d_id"
    ; "h_w_id"
    ; "h_date"
    ; "h_amount"
    ; "h_data"
    ]
  | "item" -> [ "i_id"; "i_im_id"; "i_name"; "i_price"; "i_data" ]
  | "stock" ->
    [ "s_i_id"
    ; "s_w_id"
    ; "s_quantity"
    ; "s_dist_01"
    ; "s_dist_02"
    ; "s_dist_03"
    ; "s_dist_04"
    ; "s_dist_05"
    ; "s_dist_06"
    ; "s_dist_07"
    ; "s_dist_08"
    ; "s_dist_09"
    ; "s_dist_10"
    ; "s_ytd"
    ; "s_order_cnt"
    ; "s_remote_cnt"
    ; "s_data"
    ]
  | "orders" ->
    [ "o_id"
    ; "o_d_id"
    ; "o_w_id"
    ; "o_c_id"
    ; "o_entry_d"
    ; "o_carrier_id"
    ; "o_ol_cnt"
    ; "o_all_local"
    ]
  | "new_order" -> [ "no_o_id"; "no_d_id"; "no_w_id" ]
  | "order_line" ->
    [ "ol_o_id"
    ; "ol_d_id"
    ; "ol_w_id"
    ; "ol_number"
    ; "ol_i_id"
    ; "ol_supply_w_id"
    ; "ol_delivery_d"
    ; "ol_quantity"
    ; "ol_amount"
    ; "ol_dist_info"
    ]
  | other -> invalid_arg ("Tpcc_gen: unknown table " ^ other)
;;

(* --- shared field builders -------------------------------------------- *)

(* Every draw below is bound to its own [let], in the order it is meant to
   happen.  Two draws written as operands of one expression — arguments of a
   call, elements of a tuple, array or list, fields of a record — would be
   evaluated in a toolchain-dependent order, and each draw advances the LCG, so
   the seed would no longer pin the dataset across platforms. *)

let zip_code r =
  let n = Tpc_rand.int_between r ~lo:0 ~hi:9_999 in
  Printf.sprintf "%04d11111" n
;;

(* street_1, street_2, city, state, zip — the address shape shared by
   warehouse, district and customer. *)
let address r =
  let street_1 = Tpc_rand.a_string r ~lo:10 ~hi:20 in
  let street_2 = Tpc_rand.a_string r ~lo:10 ~hi:20 in
  let city = Tpc_rand.a_string r ~lo:10 ~hi:20 in
  let state = Tpc_rand.a_string r ~lo:2 ~hi:2 in
  let zip = zip_code r in
  street_1, street_2, city, state, zip
;;

(* i_data and s_data: a random string carrying the literal "ORIGINAL" at a
   random position in 10% of rows, which is what the StockLevel and NewOrder
   "brand-generic" reporting keys off.  [lo] is at least 26 everywhere this is
   called, so the marker always fits. *)
let original = "ORIGINAL"

let data_string r ~lo ~hi =
  let s = Tpc_rand.a_string r ~lo ~hi in
  let is_original = Tpc_rand.int_between r ~lo:1 ~hi:10 = 1 in
  if not is_original
  then s
  else (
    let n = String.length s in
    let m = String.length original in
    let pos = Tpc_rand.int_between r ~lo:0 ~hi:(n - m) in
    String.sub s 0 pos ^ original ^ String.sub s (pos + m) (n - pos - m))
;;

(* --- warehouse -------------------------------------------------------- *)

let warehouse_row r ~w_id =
  let name = Tpc_rand.a_string r ~lo:6 ~hi:10 in
  let street_1, street_2, city, state, zip = address r in
  let tax = Tpc_rand.float_between r ~lo:0.0 ~hi:0.2 ~decimals:4 in
  [| V.VInt w_id
   ; V.VText name
   ; V.VText street_1
   ; V.VText street_2
   ; V.VText city
   ; V.VText state
   ; V.VText zip
   ; V.VReal tax
   ; V.VReal w_ytd
  |]
;;

let gen_warehouse t ~f =
  let r = rand_for t ~table:"warehouse" in
  for w_id = 1 to t.warehouses do
    f (warehouse_row r ~w_id)
  done
;;

(* --- district --------------------------------------------------------- *)

let district_row r ~w_id ~d_id =
  let name = Tpc_rand.a_string r ~lo:6 ~hi:10 in
  let street_1, street_2, city, state, zip = address r in
  let tax = Tpc_rand.float_between r ~lo:0.0 ~hi:0.2 ~decimals:4 in
  [| V.VInt d_id
   ; V.VInt w_id
   ; V.VText name
   ; V.VText street_1
   ; V.VText street_2
   ; V.VText city
   ; V.VText state
   ; V.VText zip
   ; V.VReal tax
   ; V.VReal d_ytd
   ; V.VInt d_next_o_id
  |]
;;

let gen_district t ~f =
  let r = rand_for t ~table:"district" in
  for w_id = 1 to t.warehouses do
    for d_id = 1 to districts_per_warehouse do
      f (district_row r ~w_id ~d_id)
    done
  done
;;

(* --- customer --------------------------------------------------------- *)

(* The spec's c_last rule: the first 1,000 customers of a district cover the
   1,000 surnames exactly once, so that a name lookup always resolves; the rest
   draw from the same skewed NURand distribution the Payment and OrderStatus
   profiles use. *)
let customer_last_name r ~c_id =
  if c_id <= 1_000
  then Tpc_rand.last_name (c_id - 1)
  else Tpc_rand.last_name (Tpc_rand.nurand r ~a:255 ~x:0 ~y:999 ~c:c_load)
;;

let customer_row r ~w_id ~d_id ~c_id =
  let first = Tpc_rand.a_string r ~lo:8 ~hi:16 in
  let last = customer_last_name r ~c_id in
  let street_1, street_2, city, state, zip = address r in
  let phone = Tpc_rand.a_string r ~lo:16 ~hi:16 in
  let bad_credit = Tpc_rand.int_between r ~lo:1 ~hi:10 = 1 in
  let discount = Tpc_rand.float_between r ~lo:0.0 ~hi:0.5 ~decimals:4 in
  let data = Tpc_rand.a_string r ~lo:300 ~hi:500 in
  [| V.VInt c_id
   ; V.VInt d_id
   ; V.VInt w_id
   ; V.VText first
   ; V.VText "OE"
   ; V.VText last
   ; V.VText street_1
   ; V.VText street_2
   ; V.VText city
   ; V.VText state
   ; V.VText zip
   ; V.VText phone
   ; V.VText load_time
   ; V.VText (if bad_credit then "BC" else "GC")
   ; V.VReal 50_000.0
   ; V.VReal discount
   ; V.VReal (-10.0)
   ; V.VReal 10.0
   ; V.VInt 1
   ; V.VInt 0
   ; V.VText data
  |]
;;

let gen_customer_district r ~w_id ~d_id ~f =
  for c_id = 1 to customers_per_district do
    f (customer_row r ~w_id ~d_id ~c_id)
  done
;;

let gen_customer t ~f =
  let r = rand_for t ~table:"customer" in
  for w_id = 1 to t.warehouses do
    for d_id = 1 to districts_per_warehouse do
      gen_customer_district r ~w_id ~d_id ~f
    done
  done
;;

(* --- history ---------------------------------------------------------- *)

let history_row r ~w_id ~d_id ~c_id =
  let data = Tpc_rand.a_string r ~lo:12 ~hi:24 in
  [| V.VInt c_id
   ; V.VInt d_id
   ; V.VInt w_id
   ; V.VInt d_id
   ; V.VInt w_id
   ; V.VText load_time
   ; V.VReal 10.0
   ; V.VText data
  |]
;;

let gen_history_district r ~w_id ~d_id ~f =
  for c_id = 1 to customers_per_district do
    f (history_row r ~w_id ~d_id ~c_id)
  done
;;

let gen_history t ~f =
  let r = rand_for t ~table:"history" in
  for w_id = 1 to t.warehouses do
    for d_id = 1 to districts_per_warehouse do
      gen_history_district r ~w_id ~d_id ~f
    done
  done
;;

(* --- item ------------------------------------------------------------- *)

let item_row r ~i_id =
  let im_id = Tpc_rand.int_between r ~lo:1 ~hi:10_000 in
  let name = Tpc_rand.a_string r ~lo:14 ~hi:24 in
  let price = Tpc_rand.float_between r ~lo:1.0 ~hi:100.0 ~decimals:2 in
  let data = data_string r ~lo:26 ~hi:50 in
  [| V.VInt i_id; V.VInt im_id; V.VText name; V.VReal price; V.VText data |]
;;

(* The item table is fixed at 100,000 rows by the spec; it is the one table
   that does NOT scale with the warehouse count. *)
let gen_item t ~f =
  let r = rand_for t ~table:"item" in
  for i_id = 1 to items do
    f (item_row r ~i_id)
  done
;;

(* --- stock ------------------------------------------------------------ *)

let stock_row r ~w_id ~i_id =
  let quantity = Tpc_rand.int_between r ~lo:10 ~hi:100 in
  let dists = Array.make districts_per_warehouse "" in
  for i = 0 to districts_per_warehouse - 1 do
    dists.(i) <- Tpc_rand.a_string r ~lo:24 ~hi:24
  done;
  let data = data_string r ~lo:26 ~hi:50 in
  [| V.VInt i_id
   ; V.VInt w_id
   ; V.VInt quantity
   ; V.VText dists.(0)
   ; V.VText dists.(1)
   ; V.VText dists.(2)
   ; V.VText dists.(3)
   ; V.VText dists.(4)
   ; V.VText dists.(5)
   ; V.VText dists.(6)
   ; V.VText dists.(7)
   ; V.VText dists.(8)
   ; V.VText dists.(9)
   ; V.VInt 0
   ; V.VInt 0
   ; V.VInt 0
   ; V.VText data
  |]
;;

let gen_stock_warehouse r ~w_id ~f =
  for i_id = 1 to stock_per_warehouse do
    f (stock_row r ~w_id ~i_id)
  done
;;

let gen_stock t ~f =
  let r = rand_for t ~table:"stock" in
  for w_id = 1 to t.warehouses do
    gen_stock_warehouse r ~w_id ~f
  done
;;

(* --- orders ----------------------------------------------------------- *)

(* o_c_id is a permutation of 1..3000, not 3,000 independent draws: the spec
   gives every customer exactly one order, and the Delivery and OrderStatus
   profiles rely on that.  Fisher-Yates with an explicit index draw per step,
   so the draw order is stated rather than inherited from an argument
   evaluation order. *)
let customer_permutation r =
  let a = Array.init customers_per_district (fun i -> i + 1) in
  for i = customers_per_district - 1 downto 1 do
    let j = Tpc_rand.int_between r ~lo:0 ~hi:i in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  done;
  a
;;

let iter_orders_district r ~w_id ~d_id ~f =
  let perm = customer_permutation r in
  for o_id = 1 to orders_per_district do
    let c_id = perm.(o_id - 1) in
    let carrier =
      if o_id < first_undelivered_o_id
      then V.VInt (Tpc_rand.int_between r ~lo:1 ~hi:10)
      else V.VNull
    in
    let ol_cnt = Tpc_rand.int_between r ~lo:5 ~hi:15 in
    f ~w_id ~d_id ~o_id ~c_id ~carrier ~ol_cnt
  done
;;

(* The single definition of the order stream.  Both the orders table and the
   order_line table walk it, so the o_ol_cnt written into an orders row and the
   number of order_line rows emitted for that order are the same draw by
   construction — consistency condition 4.  order_line takes its own stream for
   the line columns and replays this one only for the counts. *)
let iter_orders t ~f =
  let r = rand_for t ~table:"orders" in
  for w_id = 1 to t.warehouses do
    for d_id = 1 to districts_per_warehouse do
      iter_orders_district r ~w_id ~d_id ~f
    done
  done
;;

let order_row ~w_id ~d_id ~o_id ~c_id ~carrier ~ol_cnt =
  [| V.VInt o_id
   ; V.VInt d_id
   ; V.VInt w_id
   ; V.VInt c_id
   ; V.VText load_time
   ; carrier
   ; V.VInt ol_cnt
   ; V.VInt 1
  |]
;;

let gen_orders t ~f =
  iter_orders t ~f:(fun ~w_id ~d_id ~o_id ~c_id ~carrier ~ol_cnt ->
    f (order_row ~w_id ~d_id ~o_id ~c_id ~carrier ~ol_cnt))
;;

(* --- new_order -------------------------------------------------------- *)

let gen_new_order_district ~w_id ~d_id ~f =
  for o_id = first_undelivered_o_id to orders_per_district do
    f [| V.VInt o_id; V.VInt d_id; V.VInt w_id |]
  done
;;

let gen_new_order t ~f =
  for w_id = 1 to t.warehouses do
    for d_id = 1 to districts_per_warehouse do
      gen_new_order_district ~w_id ~d_id ~f
    done
  done
;;

(* --- order_line ------------------------------------------------------- *)

(* A delivered order (o_id below the new_order watermark) has a delivery date
   and a zero amount; an undelivered one has neither. *)
let order_line_row r ~w_id ~d_id ~o_id ~number ~delivered =
  let i_id = Tpc_rand.int_between r ~lo:1 ~hi:items in
  let amount =
    if delivered then 0.0 else Tpc_rand.float_between r ~lo:0.01 ~hi:9_999.99 ~decimals:2
  in
  let dist_info = Tpc_rand.a_string r ~lo:24 ~hi:24 in
  [| V.VInt o_id
   ; V.VInt d_id
   ; V.VInt w_id
   ; V.VInt number
   ; V.VInt i_id
   ; V.VInt w_id
   ; (if delivered then V.VText load_time else V.VNull)
   ; V.VInt 5
   ; V.VReal amount
   ; V.VText dist_info
  |]
;;

let order_lines r ~w_id ~d_id ~o_id ~ol_cnt ~f =
  let delivered = o_id < first_undelivered_o_id in
  for number = 1 to ol_cnt do
    f (order_line_row r ~w_id ~d_id ~o_id ~number ~delivered)
  done
;;

let gen_order_line t ~f =
  let r = rand_for t ~table:"order_line" in
  iter_orders t ~f:(fun ~w_id ~d_id ~o_id ~c_id:_ ~carrier:_ ~ol_cnt ->
    order_lines r ~w_id ~d_id ~o_id ~ol_cnt ~f)
;;

(* --- dispatch --------------------------------------------------------- *)

let iter_rows t ~table ~f =
  match table with
  | "warehouse" -> gen_warehouse t ~f
  | "district" -> gen_district t ~f
  | "customer" -> gen_customer t ~f
  | "history" -> gen_history t ~f
  | "item" -> gen_item t ~f
  | "stock" -> gen_stock t ~f
  | "orders" -> gen_orders t ~f
  | "new_order" -> gen_new_order t ~f
  | "order_line" -> gen_order_line t ~f
  | other -> invalid_arg ("Tpcc_gen: unknown table " ^ other)
;;

let districts t = t.warehouses * districts_per_warehouse

let row_count t ~table =
  match table with
  | "warehouse" -> t.warehouses
  | "district" -> districts t
  | "customer" | "history" -> districts t * customers_per_district
  | "item" -> items
  | "stock" -> t.warehouses * stock_per_warehouse
  | "orders" -> districts t * orders_per_district
  | "new_order" -> districts t * new_orders_per_district
  | "order_line" ->
    (* Not a fixed multiple: it is the sum of o_ol_cnt, so count by replaying
       the order stream rather than the far larger line stream. *)
    let n = ref 0 in
    iter_orders t ~f:(fun ~w_id:_ ~d_id:_ ~o_id:_ ~c_id:_ ~carrier:_ ~ol_cnt ->
      n := !n + ol_cnt);
    !n
  | other -> invalid_arg ("Tpcc_gen: unknown table " ^ other)
;;
