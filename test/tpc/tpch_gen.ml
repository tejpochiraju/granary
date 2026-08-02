type value =
  | VInt of int
  | VReal of float
  | VText of string

type t =
  { seed : int
  ; sf : float
  ; pool : string
  }

let tables =
  [ "region"; "nation"; "supplier"; "part"; "partsupp"; "customer"; "orders"; "lineitem" ]
;;

let table_index table =
  match List.find_index (String.equal table) tables with
  | Some i -> i
  | None -> invalid_arg ("Tpch_gen: unknown table " ^ table)
;;

(* Each table draws from its own stream so that generating one table alone
   yields the same rows as generating it inside a full load. *)
let rand_for t ~table = Tpc_rand.create ~seed:(t.seed + table_index table)

let create ~seed ~sf =
  if sf <= 0.0 then invalid_arg "Tpch_gen.create: sf must be positive";
  let pool_size = Stdlib.max 200_000 (int_of_float (2_000_000.0 *. sf)) in
  { seed; sf; pool = Tpch_text.pool (Tpc_rand.create ~seed) ~size:pool_size }
;;

let pp fmt t = Format.fprintf fmt "Tpch_gen.t { seed = %d; sf = %g }" t.seed t.sf
let scaled t n = int_of_float (Float.round (float_of_int n *. t.sf))

(* Row counts.  Clamped to at least one row so that a very small [sf] cannot
   produce an empty parent table and then a division by zero downstream. *)
let n_supplier t = Stdlib.max 1 (scaled t 10_000)
let n_part t = Stdlib.max 1 (scaled t 200_000)
let n_customer t = Stdlib.max 1 (scaled t 150_000)
let n_orders t = Stdlib.max 1 (scaled t 1_500_000)
let round2 x = Float.round (x *. 100.0) /. 100.0

(* --- civil dates ----------------------------------------------------- *)

(* Days since 1970-01-01, by Howard Hinnant's civil_from_days algorithm. *)
let days_from_civil ~y ~m ~d =
  let y = if m <= 2 then y - 1 else y in
  let era = (if y >= 0 then y else y - 399) / 400 in
  let yoe = y - (era * 400) in
  let mp = (m + 9) mod 12 in
  let doy = (((153 * mp) + 2) / 5) + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468
;;

let civil_from_days z =
  let z = z + 719468 in
  let era = (if z >= 0 then z else z - 146096) / 146097 in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let y = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  (if m <= 2 then y + 1 else y), m, d
;;

let date_of_day n =
  let y, m, d = civil_from_days n in
  Printf.sprintf "%04d-%02d-%02d" y m d
;;

(* The spec's date window: 1992-01-01 through 1998-12-31. *)
let date_lo = days_from_civil ~y:1992 ~m:1 ~d:1
let date_hi = days_from_civil ~y:1998 ~m:12 ~d:31

(* The spec's CURRENTDATE, from which l_returnflag and l_linestatus derive. *)
let current_date = days_from_civil ~y:1995 ~m:6 ~d:17

(* An order date leaves room for the largest lineitem offset: shipdate is at
   most +121 days and receiptdate at most +30 beyond that. *)
let max_line_offset = 151

(* --- column names ---------------------------------------------------- *)

let column_names ~table =
  match table with
  | "region" -> [ "r_regionkey"; "r_name"; "r_comment" ]
  | "nation" -> [ "n_nationkey"; "n_name"; "n_regionkey"; "n_comment" ]
  | "supplier" ->
    [ "s_suppkey"
    ; "s_name"
    ; "s_address"
    ; "s_nationkey"
    ; "s_phone"
    ; "s_acctbal"
    ; "s_comment"
    ]
  | "part" ->
    [ "p_partkey"
    ; "p_name"
    ; "p_mfgr"
    ; "p_brand"
    ; "p_type"
    ; "p_size"
    ; "p_container"
    ; "p_retailprice"
    ; "p_comment"
    ]
  | "partsupp" ->
    [ "ps_partkey"; "ps_suppkey"; "ps_availqty"; "ps_supplycost"; "ps_comment" ]
  | "customer" ->
    [ "c_custkey"
    ; "c_name"
    ; "c_address"
    ; "c_nationkey"
    ; "c_phone"
    ; "c_acctbal"
    ; "c_mktsegment"
    ; "c_comment"
    ]
  | "orders" ->
    [ "o_orderkey"
    ; "o_custkey"
    ; "o_orderstatus"
    ; "o_totalprice"
    ; "o_orderdate"
    ; "o_orderpriority"
    ; "o_clerk"
    ; "o_shippriority"
    ; "o_comment"
    ]
  | "lineitem" ->
    [ "l_orderkey"
    ; "l_partkey"
    ; "l_suppkey"
    ; "l_linenumber"
    ; "l_quantity"
    ; "l_extendedprice"
    ; "l_discount"
    ; "l_tax"
    ; "l_returnflag"
    ; "l_linestatus"
    ; "l_shipdate"
    ; "l_commitdate"
    ; "l_receiptdate"
    ; "l_shipinstruct"
    ; "l_shipmode"
    ; "l_comment"
    ]
  | other -> invalid_arg ("Tpch_gen.column_names: unknown table " ^ other)
;;

(* --- region and nation ----------------------------------------------- *)

let region_names = [| "AFRICA"; "AMERICA"; "ASIA"; "EUROPE"; "MIDDLE EAST" |]

let gen_region t ~f =
  let r = rand_for t ~table:"region" in
  Array.iteri
    (fun i name ->
       f
         [| VInt i
          ; VText name
          ; VText (Tpch_text.substring ~pool:t.pool r ~lo:31 ~hi:115)
         |])
    region_names
;;

(* Nation name and its region key, in nationkey order. *)
let nation_table =
  [| "ALGERIA", 0
   ; "ARGENTINA", 1
   ; "BRAZIL", 1
   ; "CANADA", 1
   ; "EGYPT", 4
   ; "ETHIOPIA", 0
   ; "FRANCE", 3
   ; "GERMANY", 3
   ; "INDIA", 2
   ; "INDONESIA", 2
   ; "IRAN", 4
   ; "IRAQ", 4
   ; "JAPAN", 2
   ; "JORDAN", 4
   ; "KENYA", 0
   ; "MOROCCO", 0
   ; "MOZAMBIQUE", 0
   ; "PERU", 1
   ; "CHINA", 2
   ; "ROMANIA", 3
   ; "SAUDI ARABIA", 4
   ; "VIETNAM", 2
   ; "RUSSIA", 3
   ; "UNITED KINGDOM", 3
   ; "UNITED STATES", 1
  |]
;;

let gen_nation t ~f =
  let r = rand_for t ~table:"nation" in
  Array.iteri
    (fun i (name, region) ->
       f
         [| VInt i
          ; VText name
          ; VInt region
          ; VText (Tpch_text.substring ~pool:t.pool r ~lo:31 ~hi:114)
         |])
    nation_table
;;

(* --- supplier -------------------------------------------------------- *)

(* Per 10 000 suppliers the spec plants 5 "Customer ... Complaints" markers
   and 5 "Customer ... Recommends" markers, which is what makes Q16's
   NOT LIKE filter select a meaningful subset.  Five evenly spaced slots for
   each marker inside every 10 000-row block, never overlapping.  Below
   10 000 suppliers no slot is reached, which the spec expects. *)
let supplier_marker index =
  match index mod 10_000 mod 2_000 with
  | 500 -> Some "Customer Complaints"
  | 1_500 -> Some "Customer Recommends"
  | _ -> None
;;

(* Overwrite a window in the middle of [comment] with [marker], preserving
   the comment's length so it stays inside its declared bound. *)
let splice_marker comment marker =
  let m = String.length marker
  and n = String.length comment in
  if m >= n
  then marker
  else (
    let pos = (n - m) / 2 in
    String.sub comment 0 pos ^ marker ^ String.sub comment (pos + m) (n - pos - m))
;;

let supplier_comment t r ~index =
  let base = Tpch_text.substring ~pool:t.pool r ~lo:25 ~hi:100 in
  match supplier_marker index with
  | None -> base
  | Some marker -> splice_marker base marker
;;

(* Every draw below is bound to its own [let], in the order it is meant to
   happen, and the row array is built only from already-bound values.  Two draws
   written as elements of one array — as the rows here were until #509 — are
   evaluated in a toolchain-dependent order, and each draw advances the LCG, so
   the seed no longer pinned the dataset across compilers.  Same discipline as
   Tpcc_gen's, and the same hazard Tpc_rand.phone had. *)
let gen_supplier t ~f =
  let r = rand_for t ~table:"supplier" in
  let n = n_supplier t in
  for i = 0 to n - 1 do
    let nation = Tpc_rand.int_between r ~lo:0 ~hi:24 in
    let address = Tpc_rand.a_string r ~lo:10 ~hi:40 in
    let phone = Tpc_rand.phone r ~nation in
    let acctbal = Tpc_rand.float_between r ~lo:(-999.99) ~hi:9999.99 ~decimals:2 in
    let comment = supplier_comment t r ~index:i in
    f
      [| VInt i
       ; VText (Printf.sprintf "Supplier#%09d" i)
       ; VText address
       ; VInt nation
       ; VText phone
       ; VReal acctbal
       ; VText comment
      |]
  done
;;

(* --- part ------------------------------------------------------------ *)

let colours =
  [| "almond"
   ; "antique"
   ; "aquamarine"
   ; "azure"
   ; "beige"
   ; "bisque"
   ; "black"
   ; "blanched"
   ; "blue"
   ; "blush"
   ; "brown"
   ; "burlywood"
   ; "burnished"
   ; "chartreuse"
   ; "chiffon"
   ; "chocolate"
   ; "coral"
   ; "cornflower"
   ; "cornsilk"
   ; "cream"
   ; "cyan"
   ; "dark"
   ; "deep"
   ; "dim"
   ; "dodger"
   ; "drab"
   ; "firebrick"
   ; "floral"
   ; "forest"
   ; "frosted"
   ; "gainsboro"
   ; "ghost"
   ; "goldenrod"
   ; "green"
   ; "grey"
   ; "honeydew"
   ; "hot"
   ; "indian"
   ; "ivory"
   ; "khaki"
   ; "lace"
   ; "lavender"
   ; "lawn"
   ; "lemon"
   ; "light"
   ; "lime"
   ; "linen"
   ; "magenta"
   ; "maroon"
   ; "medium"
   ; "metallic"
   ; "midnight"
   ; "mint"
   ; "misty"
   ; "moccasin"
   ; "navajo"
   ; "navy"
   ; "olive"
   ; "orange"
   ; "orchid"
   ; "pale"
   ; "papaya"
   ; "peach"
   ; "peru"
   ; "pink"
   ; "plum"
   ; "powder"
   ; "puff"
   ; "purple"
   ; "red"
   ; "rose"
   ; "rosy"
   ; "royal"
   ; "saddle"
   ; "salmon"
   ; "sandy"
   ; "seashell"
   ; "sienna"
   ; "sky"
   ; "slate"
   ; "smoke"
   ; "snow"
   ; "spring"
   ; "steel"
   ; "tan"
   ; "thistle"
   ; "tomato"
   ; "turquoise"
   ; "violet"
   ; "wheat"
   ; "white"
   ; "yellow"
  |]
;;

let type_syllable_1 = [| "STANDARD"; "SMALL"; "MEDIUM"; "LARGE"; "ECONOMY"; "PROMO" |]
let type_syllable_2 = [| "ANODIZED"; "BURNISHED"; "PLATED"; "POLISHED"; "BRUSHED" |]
let type_syllable_3 = [| "TIN"; "NICKEL"; "BRASS"; "STEEL"; "COPPER" |]
let container_1 = [| "SM"; "LG"; "MED"; "JUMBO"; "WRAP" |]
let container_2 = [| "CASE"; "BOX"; "BAG"; "JAR"; "PKG"; "PACK"; "CAN"; "DRUM" |]

(* Rejection sampling: with 92 colours and 5 draws, collisions are rare. *)
let rec fill_distinct r choices chosen i k =
  if i >= k
  then ()
  else (
    let c = Tpc_rand.pick r choices in
    if Array.exists (String.equal c) (Array.sub chosen 0 i)
    then fill_distinct r choices chosen i k
    else (
      chosen.(i) <- c;
      fill_distinct r choices chosen (i + 1) k))
;;

let part_name r =
  let chosen = Array.make 5 "" in
  fill_distinct r colours chosen 0 5;
  String.concat " " (Array.to_list chosen)
;;

(* Deterministic in the key, not random — the spec defines it this way and
   l_extendedprice is derived from it, so it must be reproducible from the
   partkey alone. *)
let retail_price key =
  float_of_int (90_000 + (key / 10 mod 20_001) + (100 * (key mod 1_000))) /. 100.0
;;

let gen_part t ~f =
  let r = rand_for t ~table:"part" in
  let n = n_part t in
  for i = 0 to n - 1 do
    let mfgr = Tpc_rand.int_between r ~lo:1 ~hi:5 in
    let brand = Tpc_rand.int_between r ~lo:1 ~hi:5 in
    let name = part_name r in
    (* The three type syllables were elements of one list and the two container
       words operands of one [^]; both are unspecified-order positions (#509). *)
    let syl_1 = Tpc_rand.pick r type_syllable_1 in
    let syl_2 = Tpc_rand.pick r type_syllable_2 in
    let syl_3 = Tpc_rand.pick r type_syllable_3 in
    let ptype = String.concat " " [ syl_1; syl_2; syl_3 ] in
    let cont_1 = Tpc_rand.pick r container_1 in
    let cont_2 = Tpc_rand.pick r container_2 in
    let container = String.concat " " [ cont_1; cont_2 ] in
    let size = Tpc_rand.int_between r ~lo:1 ~hi:50 in
    let comment = Tpch_text.substring ~pool:t.pool r ~lo:5 ~hi:22 in
    f
      [| VInt i
       ; VText name
       ; VText (Printf.sprintf "Manufacturer#%d" mfgr)
       ; VText (Printf.sprintf "Brand#%d%d" mfgr brand)
       ; VText ptype
       ; VInt size
       ; VText container
       ; VReal (retail_price i)
       ; VText comment
      |]
  done
;;

(* --- partsupp -------------------------------------------------------- *)

(* The four suppliers of a part, spread across the supplier range.  The
   spec's own formula can collide once the stride reaches S/3, which is
   reachable with dense 0-based keys at some scale factors; a stride of
   floor(S/4) is always < S/3 and so keeps all four distinct for S >= 4.

   A fixed stride does mean parts p and p+q share an identical supplier set,
   so there are only about S/4 distinct supplier sets.  That introduces no
   systematic bias in Q2 or Q11: ps_supplycost, ps_availqty and s_nationkey
   are all drawn independently, and every supplier still appears in exactly
   the same number of partsupp rows. *)
let ps_suppkey ~partkey ~index ~suppliers =
  (* max 1 only matters for S < 4, where four distinct suppliers are
     impossible; that degenerate case is knowingly left unhandled, being
     unreachable at any usable scale factor. *)
  let stride = Stdlib.max 1 (suppliers / 4) in
  (partkey + (index * stride)) mod suppliers
;;

let gen_partsupp t ~f =
  let r = rand_for t ~table:"partsupp" in
  let n = n_part t in
  let suppliers = n_supplier t in
  for i = 0 to n - 1 do
    for j = 0 to 3 do
      let availqty = Tpc_rand.int_between r ~lo:1 ~hi:9_999 in
      let supplycost = Tpc_rand.float_between r ~lo:1.00 ~hi:1_000.00 ~decimals:2 in
      let comment = Tpch_text.substring ~pool:t.pool r ~lo:49 ~hi:198 in
      f
        [| VInt i
         ; VInt (ps_suppkey ~partkey:i ~index:j ~suppliers)
         ; VInt availqty
         ; VReal supplycost
         ; VText comment
        |]
    done
  done
;;

(* --- customer -------------------------------------------------------- *)

let segments = [| "AUTOMOBILE"; "BUILDING"; "FURNITURE"; "HOUSEHOLD"; "MACHINERY" |]

let gen_customer t ~f =
  let r = rand_for t ~table:"customer" in
  let n = n_customer t in
  for i = 0 to n - 1 do
    let nation = Tpc_rand.int_between r ~lo:0 ~hi:24 in
    let address = Tpc_rand.a_string r ~lo:10 ~hi:40 in
    let phone = Tpc_rand.phone r ~nation in
    let acctbal = Tpc_rand.float_between r ~lo:(-999.99) ~hi:9999.99 ~decimals:2 in
    let segment = Tpc_rand.pick r segments in
    let comment = Tpch_text.substring ~pool:t.pool r ~lo:29 ~hi:116 in
    f
      [| VInt i
       ; VText (Printf.sprintf "Customer#%09d" i)
       ; VText address
       ; VInt nation
       ; VText phone
       ; VReal acctbal
       ; VText segment
       ; VText comment
      |]
  done
;;

(* --- orders and lineitem --------------------------------------------- *)

(* orders and lineitem are generated together: o_orderstatus and
   o_totalprice are defined in terms of the order's lines, and Q4, Q13, Q18
   and Q21 read them, so the two tables must agree exactly.  Both public
   generators drive the same walk, with the same two per-table streams, so
   either can be generated alone. *)

type order =
  { o_orderkey : int
  ; o_custkey : int
  ; o_orderdate : int
  ; o_orderpriority : string
  ; o_clerk : string
  ; o_comment : string
  }

type line =
  { l_partkey : int
  ; l_suppkey : int
  ; l_linenumber : int
  ; l_quantity : int
  ; l_extendedprice : float
  ; l_discount : float
  ; l_tax : float
  ; l_returnflag : string
  ; l_linestatus : string
  ; l_shipdate : int
  ; l_commitdate : int
  ; l_receiptdate : int
  ; l_shipinstruct : string
  ; l_shipmode : string
  ; l_comment : string
  }

let priorities = [| "1-URGENT"; "2-HIGH"; "3-MEDIUM"; "4-NOT SPECIFIED"; "5-LOW" |]
let ship_instructs = [| "DELIVER IN PERSON"; "COLLECT COD"; "NONE"; "TAKE BACK RETURN" |]
let ship_modes = [| "REG AIR"; "AIR"; "RAIL"; "SHIP"; "TRUCK"; "MAIL"; "FOB" |]
let return_flags = [| "R"; "A" |]

(* The spec excludes every customer whose key is a multiple of 3 from
   placing orders.  Draw an index into the surviving keys so the draw stays
   uniform and never needs a retry loop. *)
let order_custkey r ~customers =
  let eligible = customers - ((customers + 2) / 3) in
  (* eligible <= 0 only at C <= 1, where no customer may place an order at
     all; that degenerate case is knowingly left unhandled (it returns key 0,
     which the spec would exclude) as it is unreachable at any usable scale
     factor. *)
  if eligible <= 0
  then 0
  else (
    let j = Tpc_rand.int_between r ~lo:0 ~hi:(eligible - 1) in
    (3 * (j / 2)) + 1 + (j mod 2))
;;

(* A record's fields are an unspecified-order position exactly as a call's
   arguments are, so the five draws below are bound first and the record built
   from names only (#509). *)
let gen_order_core t r ~key =
  let clerks = Stdlib.max 1 (scaled t 1_000) in
  let custkey = order_custkey r ~customers:(n_customer t) in
  let orderdate = Tpc_rand.int_between r ~lo:date_lo ~hi:(date_hi - max_line_offset) in
  let priority = Tpc_rand.pick r priorities in
  let clerk = Tpc_rand.int_between r ~lo:1 ~hi:clerks in
  let comment = Tpch_text.substring ~pool:t.pool r ~lo:19 ~hi:78 in
  { o_orderkey = key
  ; o_custkey = custkey
  ; o_orderdate = orderdate
  ; o_orderpriority = priority
  ; o_clerk = Printf.sprintf "Clerk#%09d" clerk
  ; o_comment = comment
  }
;;

let gen_line t r ~orderdate ~lineno =
  let partkey = Tpc_rand.int_between r ~lo:0 ~hi:(n_part t - 1) in
  let which = Tpc_rand.int_between r ~lo:0 ~hi:3 in
  let quantity = Tpc_rand.int_between r ~lo:1 ~hi:50 in
  let discount = Tpc_rand.float_between r ~lo:0.00 ~hi:0.10 ~decimals:2 in
  let tax = Tpc_rand.float_between r ~lo:0.00 ~hi:0.08 ~decimals:2 in
  let shipdate = orderdate + Tpc_rand.int_between r ~lo:1 ~hi:121 in
  let commitdate = orderdate + Tpc_rand.int_between r ~lo:30 ~hi:90 in
  let receiptdate = shipdate + Tpc_rand.int_between r ~lo:1 ~hi:30 in
  let returnflag =
    if receiptdate <= current_date then Tpc_rand.pick r return_flags else "N"
  in
  let shipinstruct = Tpc_rand.pick r ship_instructs in
  let shipmode = Tpc_rand.pick r ship_modes in
  let comment = Tpch_text.substring ~pool:t.pool r ~lo:10 ~hi:43 in
  { l_partkey = partkey
  ; l_suppkey = ps_suppkey ~partkey ~index:which ~suppliers:(n_supplier t)
  ; l_linenumber = lineno
  ; l_quantity = quantity
  ; l_extendedprice = round2 (float_of_int quantity *. retail_price partkey)
  ; l_discount = discount
  ; l_tax = tax
  ; l_returnflag = returnflag
  ; l_linestatus = (if shipdate > current_date then "O" else "F")
  ; l_shipdate = shipdate
  ; l_commitdate = commitdate
  ; l_receiptdate = receiptdate
  ; l_shipinstruct = shipinstruct
  ; l_shipmode = shipmode
  ; l_comment = comment
  }
;;

(* Explicit recursion rather than List.init.  Each line consumes values from
   [r], so the list's contents depend on the order in which the elements are
   built.  List.init is documented "evaluated left to right" on the toolchain
   this project pins, but building the order in explicitly keeps the
   reproducibility contract local rather than inherited. *)
let rec build_lines t r ~orderdate ~lineno ~count acc =
  if lineno > count
  then List.rev acc
  else (
    let l = gen_line t r ~orderdate ~lineno in
    build_lines t r ~orderdate ~lineno:(lineno + 1) ~count (l :: acc))
;;

let iter_orders t ~f =
  let ro = rand_for t ~table:"orders" in
  let rl = rand_for t ~table:"lineitem" in
  let n = n_orders t in
  for key = 0 to n - 1 do
    let o = gen_order_core t ro ~key in
    let count = Tpc_rand.int_between rl ~lo:1 ~hi:7 in
    f o (build_lines t rl ~orderdate:o.o_orderdate ~lineno:1 ~count [])
  done
;;

(* 'O' while every line is still unshipped, 'F' once all have shipped,
   'P' in between. *)
let order_status lines =
  let merge acc l =
    match acc with
    | None -> Some l.l_linestatus
    | Some s when String.equal s l.l_linestatus -> Some s
    | Some _ -> Some "P"
  in
  match List.fold_left merge None lines with
  | Some s -> s
  | None -> "O"
;;

let order_total lines =
  let add acc l =
    acc +. (l.l_extendedprice *. (1.0 -. l.l_discount) *. (1.0 +. l.l_tax))
  in
  round2 (List.fold_left add 0.0 lines)
;;

let order_row o lines =
  [| VInt o.o_orderkey
   ; VInt o.o_custkey
   ; VText (order_status lines)
   ; VReal (order_total lines)
   ; VText (date_of_day o.o_orderdate)
   ; VText o.o_orderpriority
   ; VText o.o_clerk
   ; VInt 0
   ; VText o.o_comment
  |]
;;

let line_row ~orderkey l =
  [| VInt orderkey
   ; VInt l.l_partkey
   ; VInt l.l_suppkey
   ; VInt l.l_linenumber
   ; VReal (float_of_int l.l_quantity)
   ; VReal l.l_extendedprice
   ; VReal l.l_discount
   ; VReal l.l_tax
   ; VText l.l_returnflag
   ; VText l.l_linestatus
   ; VText (date_of_day l.l_shipdate)
   ; VText (date_of_day l.l_commitdate)
   ; VText (date_of_day l.l_receiptdate)
   ; VText l.l_shipinstruct
   ; VText l.l_shipmode
   ; VText l.l_comment
  |]
;;

let gen_orders t ~f = iter_orders t ~f:(fun o lines -> f (order_row o lines))

let gen_lineitem t ~f =
  iter_orders t ~f:(fun o lines ->
    List.iter (fun l -> f (line_row ~orderkey:o.o_orderkey l)) lines)
;;

(* --- dispatch -------------------------------------------------------- *)

let iter_rows t ~table ~f =
  match table with
  | "region" -> gen_region t ~f
  | "nation" -> gen_nation t ~f
  | "supplier" -> gen_supplier t ~f
  | "part" -> gen_part t ~f
  | "partsupp" -> gen_partsupp t ~f
  | "customer" -> gen_customer t ~f
  | "orders" -> gen_orders t ~f
  | "lineitem" -> gen_lineitem t ~f
  | other -> invalid_arg ("Tpch_gen.iter_rows: unknown table " ^ other)
;;

let row_count t ~table =
  match table with
  | "region" -> 5
  | "nation" -> 25
  | "supplier" -> n_supplier t
  | "part" -> n_part t
  | "partsupp" -> n_part t * 4
  | "customer" -> n_customer t
  | "orders" -> n_orders t
  | "lineitem" ->
    (* Not a fixed multiple — count by generating the stream. *)
    let n = ref 0 in
    iter_rows t ~table ~f:(fun _ -> incr n);
    !n
  | other -> invalid_arg ("Tpch_gen.row_count: unknown table " ^ other)
;;
