module G = Granary_tpc.Tpch_gen

let ctx ?(seed = 42) ?(sf = 0.01) () = G.create ~seed ~sf

let count g ~table =
  let n = ref 0 in
  G.iter_rows g ~table ~f:(fun _ -> incr n);
  !n
;;

let test_fixed_cardinalities () =
  let g = ctx () in
  Alcotest.(check int) "region has 5 rows" 5 (count g ~table:"region");
  Alcotest.(check int) "nation has 25 rows" 25 (count g ~table:"nation")
;;

let test_scaled_cardinalities () =
  let g = ctx ~sf:0.01 () in
  Alcotest.(check int) "supplier" 100 (count g ~table:"supplier");
  Alcotest.(check int) "part" 2000 (count g ~table:"part");
  Alcotest.(check int) "partsupp" 8000 (count g ~table:"partsupp");
  Alcotest.(check int) "customer" 1500 (count g ~table:"customer");
  Alcotest.(check int) "orders" 15000 (count g ~table:"orders")
;;

let test_lineitem_in_spec_range () =
  let g = ctx ~sf:0.01 () in
  let orders = count g ~table:"orders" in
  let li = count g ~table:"lineitem" in
  Alcotest.(check bool) "at least one line per order" true (li >= orders);
  Alcotest.(check bool) "at most seven lines per order" true (li <= orders * 7);
  let avg = float_of_int li /. float_of_int orders in
  Alcotest.(check bool)
    (Printf.sprintf "average lines per order near 4 (got %.2f)" avg)
    true
    (avg >= 3.5 && avg <= 4.5)
;;

let test_row_count_matches_iteration () =
  let g = ctx () in
  List.iter
    (fun table ->
       Alcotest.(check int)
         (table ^ ": row_count agrees with iter_rows")
         (count g ~table)
         (G.row_count g ~table))
    [ "region"; "nation"; "supplier"; "part"; "partsupp"; "customer"; "orders" ]
;;

let test_row_count_lineitem_exact () =
  let g = ctx ~sf:0.01 () in
  Alcotest.(check int)
    "lineitem: row_count agrees with iter_rows"
    (count g ~table:"lineitem")
    (G.row_count g ~table:"lineitem")
;;

let digest g ~table =
  let buf = Buffer.create 4096 in
  G.iter_rows g ~table ~f:(fun row ->
    Array.iter
      (fun v ->
         Buffer.add_string
           buf
           (match v with
            | G.VInt i -> string_of_int i
            | G.VReal f -> Printf.sprintf "%.4f" f
            | G.VText s -> s);
         Buffer.add_char buf '\031')
      row;
    Buffer.add_char buf '\030');
  Digest.to_hex (Digest.string (Buffer.contents buf))
;;

let test_determinism_same_seed () =
  let a = ctx ~seed:7 () in
  let b = ctx ~seed:7 () in
  List.iter
    (fun table ->
       Alcotest.(check string)
         (table ^ ": identical for identical seed")
         (digest a ~table)
         (digest b ~table))
    [ "part"; "customer"; "orders"; "lineitem" ]
;;

let test_different_seed_differs () =
  let a = ctx ~seed:1 () in
  let b = ctx ~seed:2 () in
  Alcotest.(check bool)
    "different seeds give different data"
    false
    (digest a ~table:"customer" = digest b ~table:"customer")
;;

(* Structural decision 3: a table's rows must not depend on which other
   tables were generated before it, so per-table streams must be independent. *)
let test_table_streams_are_independent () =
  let a = ctx ~seed:3 () in
  let before = digest a ~table:"orders" in
  List.iter (fun table -> ignore (digest a ~table)) [ "region"; "part"; "customer" ];
  Alcotest.(check string)
    "orders unaffected by other tables"
    before
    (digest a ~table:"orders")
;;

let test_nation_references_region () =
  let g = ctx () in
  let bad = ref 0 in
  G.iter_rows g ~table:"nation" ~f:(fun row ->
    match row.(2) with
    | G.VInt rk when rk >= 0 && rk < 5 -> ()
    | _ -> incr bad);
  Alcotest.(check int) "every n_regionkey is a valid region" 0 !bad;
  Alcotest.(check int) "and all 25 rows were inspected" 25 (count g ~table:"nation")
;;

let test_lineitem_dates_are_ordered () =
  let g = ctx ~sf:0.01 () in
  (* l_shipdate (col 10), l_commitdate (11), l_receiptdate (12) are TEXT dates;
     the spec requires ship <= receipt. *)
  let bad = ref 0 in
  G.iter_rows g ~table:"lineitem" ~f:(fun row ->
    match row.(10), row.(12) with
    | G.VText ship, G.VText receipt -> if String.compare ship receipt > 0 then incr bad
    | _ -> incr bad);
  Alcotest.(check int) "shipdate never after receiptdate" 0 !bad;
  Alcotest.(check bool)
    "and rows were actually inspected"
    true
    (count g ~table:"lineitem" > 0)
;;

let re_ok s =
  String.length s = 10
  && s.[4] = '-'
  && s.[7] = '-'
  && String.for_all (fun c -> (c >= '0' && c <= '9') || c = '-') s
;;

let test_date_format () =
  let g = ctx ~sf:0.01 () in
  let bad = ref 0 in
  G.iter_rows g ~table:"orders" ~f:(fun row ->
    match row.(4) with
    | G.VText d -> if not (re_ok d) then incr bad
    | _ -> incr bad);
  Alcotest.(check int) "o_orderdate is YYYY-MM-DD" 0 !bad;
  Alcotest.(check int) "and every order was inspected" 15000 (count g ~table:"orders")
;;

(* Every date column of every table must be well formed and inside the spec's
   1992-01-01 .. 1998-12-31 window. *)
let test_all_dates_in_window () =
  let g = ctx ~sf:0.01 () in
  let bad = ref 0 in
  let checked = ref 0 in
  let check_date s =
    incr checked;
    if not (re_ok s && s >= "1992-01-01" && s <= "1998-12-31") then incr bad
  in
  G.iter_rows g ~table:"orders" ~f:(fun row ->
    match row.(4) with
    | G.VText d -> check_date d
    | _ -> incr bad);
  G.iter_rows g ~table:"lineitem" ~f:(fun row ->
    List.iter
      (fun i ->
         match row.(i) with
         | G.VText d -> check_date d
         | _ -> incr bad)
      [ 10; 11; 12 ]);
  Alcotest.(check int) "all dates well formed and in window" 0 !bad;
  Alcotest.(check bool) "and dates were actually inspected" true (!checked > 0)
;;

(* --- column_names ---------------------------------------------------- *)

let test_column_names_match_arity () =
  let g = ctx ~sf:0.01 () in
  List.iter
    (fun table ->
       let names = G.column_names ~table in
       let seen = ref false in
       G.iter_rows g ~table ~f:(fun row ->
         if not !seen
         then (
           seen := true;
           Alcotest.(check int)
             (table ^ ": column_names length matches row arity")
             (Array.length row)
             (List.length names)));
       Alcotest.(check bool) (table ^ ": produced at least one row") true !seen)
    G.tables
;;

let test_tables_list () =
  Alcotest.(check (list string))
    "tables in load order, parents first"
    [ "region"
    ; "nation"
    ; "supplier"
    ; "part"
    ; "partsupp"
    ; "customer"
    ; "orders"
    ; "lineitem"
    ]
    G.tables
;;

let test_unknown_table_raises () =
  let g = ctx () in
  Alcotest.check_raises
    "iter_rows"
    (Invalid_argument "Tpch_gen.iter_rows: unknown table nope")
    (fun () -> G.iter_rows g ~table:"nope" ~f:(fun _ -> ()));
  Alcotest.check_raises
    "row_count"
    (Invalid_argument "Tpch_gen.row_count: unknown table nope")
    (fun () -> ignore (G.row_count g ~table:"nope"));
  Alcotest.check_raises
    "column_names"
    (Invalid_argument "Tpch_gen.column_names: unknown table nope")
    (fun () -> ignore (G.column_names ~table:"nope"))
;;

let test_bad_sf_raises () =
  Alcotest.check_raises
    "sf must be positive"
    (Invalid_argument "Tpch_gen.create: sf must be positive")
    (fun () -> ignore (G.create ~seed:1 ~sf:0.0));
  Alcotest.check_raises
    "negative sf too"
    (Invalid_argument "Tpch_gen.create: sf must be positive")
    (fun () -> ignore (G.create ~seed:1 ~sf:(-1.0)))
;;

(* --- per-table value rules ------------------------------------------- *)

let text = function
  | G.VText s -> s
  | _ -> Alcotest.fail "expected VText"
;;

let int_of = function
  | G.VInt i -> i
  | _ -> Alcotest.fail "expected VInt"
;;

let real_of = function
  | G.VReal f -> f
  | _ -> Alcotest.fail "expected VReal"
;;

let test_region_and_nation_names () =
  let g = ctx () in
  let regions = ref [] in
  G.iter_rows g ~table:"region" ~f:(fun row -> regions := text row.(1) :: !regions);
  Alcotest.(check (list string))
    "region names in spec order"
    [ "AFRICA"; "AMERICA"; "ASIA"; "EUROPE"; "MIDDLE EAST" ]
    (List.rev !regions);
  let nations = ref [] in
  G.iter_rows g ~table:"nation" ~f:(fun row ->
    nations := (int_of row.(0), text row.(1), int_of row.(2)) :: !nations);
  let nations = List.rev !nations in
  Alcotest.(check int) "25 nations" 25 (List.length nations);
  let key, name, region = List.nth nations 23 in
  Alcotest.(check int) "nationkey 23" 23 key;
  Alcotest.(check string) "nation 23 is UNITED KINGDOM" "UNITED KINGDOM" name;
  Alcotest.(check int) "UNITED KINGDOM is in EUROPE" 3 region
;;

let test_supplier_rows () =
  let g = ctx ~sf:0.01 () in
  let i = ref 0 in
  G.iter_rows g ~table:"supplier" ~f:(fun row ->
    Alcotest.(check int) "s_suppkey dense" !i (int_of row.(0));
    Alcotest.(check string) "s_name" (Printf.sprintf "Supplier#%09d" !i) (text row.(1));
    let nation = int_of row.(3) in
    Alcotest.(check bool) "s_nationkey in range" true (nation >= 0 && nation < 25);
    Alcotest.(check string)
      "s_phone country code matches nation"
      (Printf.sprintf "%02d" (nation + 10))
      (String.sub (text row.(4)) 0 2);
    let bal = real_of row.(5) in
    Alcotest.(check bool) "s_acctbal in range" true (bal >= -999.99 && bal <= 9999.99);
    let c = String.length (text row.(6)) in
    Alcotest.(check bool) "s_comment length" true (c >= 25 && c <= 100);
    incr i);
  Alcotest.(check int) "supplier emitted every row" 100 !i
;;

(* The Complaints/Recommends markers Q16 depends on: 5 of each per 10 000
   suppliers.  SF 0.01 is far below the first marker slot, so the counts can
   only be exercised at SF 1.0, which is exactly one 10 000-supplier block. *)
let test_supplier_comment_markers () =
  let has needle hay =
    let n = String.length needle
    and h = String.length hay in
    let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
    go 0
  in
  let complaints = ref 0
  and recommends = ref 0
  and both = ref 0
  and oversize = ref 0 in
  let g = G.create ~seed:5 ~sf:1.0 in
  G.iter_rows g ~table:"supplier" ~f:(fun row ->
    let c = text row.(6) in
    let a = has "Complaints" c
    and b = has "Recommends" c in
    if a then incr complaints;
    if b then incr recommends;
    if a && b then incr both;
    if String.length c > 100 then incr oversize);
  Alcotest.(check int) "5 Complaints markers per 10 000 suppliers" 5 !complaints;
  Alcotest.(check int) "5 Recommends markers per 10 000 suppliers" 5 !recommends;
  Alcotest.(check int) "no supplier carries both markers" 0 !both;
  Alcotest.(check int) "markers keep comments within 100 chars" 0 !oversize
;;

let test_supplier_markers_absent_at_small_sf () =
  let g = ctx ~sf:0.01 () in
  let marked = ref 0 in
  G.iter_rows g ~table:"supplier" ~f:(fun row ->
    let c = text row.(6) in
    let n = String.length c in
    let rec go i =
      i + 10 <= n
      && (String.sub c i 10 = "Complaints"
          || String.sub c i 10 = "Recommends"
          || go (i + 1))
    in
    if go 0 then incr marked);
  Alcotest.(check int) "no markers forced in at SF 0.01" 0 !marked
;;

let colours =
  "almond antique aquamarine azure beige bisque black blanched blue blush brown \
   burlywood burnished chartreuse chiffon chocolate coral cornflower cornsilk cream cyan \
   dark deep dim dodger drab firebrick floral forest frosted gainsboro ghost goldenrod \
   green grey honeydew hot indian ivory khaki lace lavender lawn lemon light lime linen \
   magenta maroon medium metallic midnight mint misty moccasin navajo navy olive orange \
   orchid pale papaya peach peru pink plum powder puff purple red rose rosy royal saddle \
   salmon sandy seashell sienna sky slate smoke snow spring steel tan thistle tomato \
   turquoise violet wheat white yellow"
  |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
;;

let retail_price key =
  float_of_int (90000 + (key / 10 mod 20001) + (100 * (key mod 1000))) /. 100.0
;;

let test_part_rows () =
  let g = ctx ~sf:0.01 () in
  Alcotest.(check int) "92 colours in the list" 92 (List.length colours);
  let i = ref 0 in
  G.iter_rows g ~table:"part" ~f:(fun row ->
    Alcotest.(check int) "p_partkey dense" !i (int_of row.(0));
    let words = String.split_on_char ' ' (text row.(1)) in
    Alcotest.(check int) "p_name has 5 words" 5 (List.length words);
    Alcotest.(check bool)
      "p_name words are distinct colours"
      true
      (List.for_all (fun w -> List.mem w colours) words
       && List.length (List.sort_uniq String.compare words) = 5);
    let mfgr = text row.(2) in
    let digit = mfgr.[String.length mfgr - 1] in
    Alcotest.(check bool)
      "p_mfgr shape"
      true
      (String.length mfgr = 14 && digit >= '1' && digit <= '5');
    let brand = text row.(3) in
    Alcotest.(check bool)
      "p_brand shares the manufacturer digit"
      true
      (String.length brand = 8
       && brand.[6] = digit
       && brand.[7] >= '1'
       && brand.[7] <= '5');
    Alcotest.(check int)
      "p_type has 3 syllables"
      3
      (List.length (String.split_on_char ' ' (text row.(4))));
    let size = int_of row.(5) in
    Alcotest.(check bool) "p_size in 1..50" true (size >= 1 && size <= 50);
    Alcotest.(check int)
      "p_container has 2 syllables"
      2
      (List.length (String.split_on_char ' ' (text row.(6))));
    Alcotest.(check (float 1e-9))
      "p_retailprice formula"
      (retail_price !i)
      (real_of row.(7));
    let c = String.length (text row.(8)) in
    Alcotest.(check bool) "p_comment length" true (c >= 5 && c <= 22);
    incr i);
  Alcotest.(check int) "part emitted every row" 2000 !i
;;

let partsupp_map g =
  let tbl = Hashtbl.create 4096 in
  G.iter_rows g ~table:"partsupp" ~f:(fun row ->
    let pk = int_of row.(0) in
    Hashtbl.replace
      tbl
      pk
      (int_of row.(1)
       ::
       (try Hashtbl.find tbl pk with
        | Not_found -> [])));
  tbl
;;

let test_partsupp_rows () =
  let g = ctx ~sf:0.01 () in
  let suppliers = 100 in
  let seen = ref [] in
  G.iter_rows g ~table:"partsupp" ~f:(fun row ->
    let qty = int_of row.(2) in
    Alcotest.(check bool) "ps_availqty in 1..9999" true (qty >= 1 && qty <= 9999);
    let cost = real_of row.(3) in
    Alcotest.(check bool) "ps_supplycost in 1..1000" true (cost >= 1.0 && cost <= 1000.0);
    let c = String.length (text row.(4)) in
    Alcotest.(check bool) "ps_comment length" true (c >= 49 && c <= 198);
    seen := int_of row.(0) :: !seen);
  let tbl = partsupp_map g in
  Alcotest.(check int) "one entry per part" 2000 (Hashtbl.length tbl);
  Hashtbl.iter
    (fun _pk sks ->
       Alcotest.(check int) "4 suppliers per part" 4 (List.length sks);
       Alcotest.(check int)
         "and they are distinct"
         4
         (List.length (List.sort_uniq compare sks));
       List.iter
         (fun s ->
            Alcotest.(check bool) "suppkey in range" true (s >= 0 && s < suppliers))
         sks)
    tbl
;;

(* The spec's own suppkey formula reaches a stride of S/3 at some scale
   factors, at which point the 4 picks collide.  SF 0.012 gives S = 120,
   where S/3 = 40 and the fourth pick would wrap onto the first; our
   floor(S/4) stride must stay distinct there. *)
let test_partsupp_distinct_at_awkward_sf () =
  let g = ctx ~sf:0.012 () in
  let tbl = partsupp_map g in
  Alcotest.(check int) "2400 parts at sf 0.012" 2400 (Hashtbl.length tbl);
  let collisions = ref 0 in
  Hashtbl.iter
    (fun _pk sks -> if List.length (List.sort_uniq compare sks) <> 4 then incr collisions)
    tbl;
  Alcotest.(check int) "4 distinct suppliers per part at sf 0.012" 0 !collisions
;;

let test_customer_rows () =
  let g = ctx ~sf:0.01 () in
  let segments = [ "AUTOMOBILE"; "BUILDING"; "FURNITURE"; "HOUSEHOLD"; "MACHINERY" ] in
  let i = ref 0 in
  G.iter_rows g ~table:"customer" ~f:(fun row ->
    Alcotest.(check int) "c_custkey dense" !i (int_of row.(0));
    Alcotest.(check string) "c_name" (Printf.sprintf "Customer#%09d" !i) (text row.(1));
    Alcotest.(check bool) "c_mktsegment" true (List.mem (text row.(6)) segments);
    incr i);
  Alcotest.(check int) "customer emitted every row" 1500 !i
;;

let priorities = [ "1-URGENT"; "2-HIGH"; "3-MEDIUM"; "4-NOT SPECIFIED"; "5-LOW" ]

let test_orders_rows () =
  let g = ctx ~sf:0.01 () in
  let customers = 1500 in
  let i = ref 0 in
  G.iter_rows g ~table:"orders" ~f:(fun row ->
    Alcotest.(check int) "o_orderkey dense" !i (int_of row.(0));
    let ck = int_of row.(1) in
    Alcotest.(check bool) "o_custkey in range" true (ck >= 0 && ck < customers);
    Alcotest.(check bool) "o_custkey never a multiple of 3" true (ck mod 3 <> 0);
    Alcotest.(check bool)
      "o_orderstatus in OFP"
      true
      (List.mem (text row.(2)) [ "O"; "F"; "P" ]);
    Alcotest.(check bool) "o_totalprice positive" true (real_of row.(3) > 0.0);
    Alcotest.(check bool) "o_orderpriority" true (List.mem (text row.(5)) priorities);
    Alcotest.(check bool)
      "o_clerk shape"
      true
      (String.length (text row.(6)) = 15 && String.sub (text row.(6)) 0 6 = "Clerk#");
    Alcotest.(check int) "o_shippriority always 0" 0 (int_of row.(7));
    incr i);
  Alcotest.(check int) "orders emitted every row" 15000 !i
;;

(* o_orderstatus and o_totalprice are defined in terms of the order's
   lineitems; Q4/Q13/Q18/Q21 read them, so they must agree exactly with what
   gen_lineitem emits. *)
let test_orders_agree_with_lineitem () =
  let g = ctx ~sf:0.01 () in
  let totals = Hashtbl.create 4096 in
  let statuses = Hashtbl.create 4096 in
  let lines = Hashtbl.create 4096 in
  G.iter_rows g ~table:"lineitem" ~f:(fun row ->
    let ok = int_of row.(0) in
    let ep = real_of row.(5)
    and d = real_of row.(6)
    and tx = real_of row.(7) in
    Hashtbl.replace
      totals
      ok
      ((try Hashtbl.find totals ok with
        | Not_found -> 0.0)
       +. (ep *. (1.0 -. d) *. (1.0 +. tx)));
    Hashtbl.replace
      lines
      ok
      ((try Hashtbl.find lines ok with
        | Not_found -> 0)
       + 1);
    let st = text row.(9) in
    let prev =
      try Hashtbl.find statuses ok with
      | Not_found -> st
    in
    Hashtbl.replace statuses ok (if prev = st then st else "P"));
  let bad_total = ref 0
  and bad_status = ref 0
  and bad_lines = ref 0 in
  G.iter_rows g ~table:"orders" ~f:(fun row ->
    let ok = int_of row.(0) in
    let n =
      try Hashtbl.find lines ok with
      | Not_found -> 0
    in
    if n < 1 || n > 7 then incr bad_lines;
    let expected = Float.round (Hashtbl.find totals ok *. 100.0) /. 100.0 in
    if Float.abs (expected -. real_of row.(3)) > 1e-6 then incr bad_total;
    if text row.(2) <> Hashtbl.find statuses ok then incr bad_status);
  Alcotest.(check int) "every order has 1..7 lines" 0 !bad_lines;
  Alcotest.(check int) "o_totalprice matches its lineitems" 0 !bad_total;
  Alcotest.(check int) "o_orderstatus matches its lineitems" 0 !bad_status;
  Alcotest.(check int) "and every order was compared" 15000 (Hashtbl.length lines)
;;

let test_lineitem_rows () =
  let g = ctx ~sf:0.01 () in
  let ps = partsupp_map g in
  let modes = [ "REG AIR"; "AIR"; "RAIL"; "SHIP"; "TRUCK"; "MAIL"; "FOB" ] in
  let instructs = [ "DELIVER IN PERSON"; "COLLECT COD"; "NONE"; "TAKE BACK RETURN" ] in
  let bad_supp = ref 0
  and bad_price = ref 0
  and bad_flag = ref 0
  and bad_num = ref 0 in
  let expect_line = ref 0
  and cur = ref (-1)
  and seen = ref 0 in
  G.iter_rows g ~table:"lineitem" ~f:(fun row ->
    incr seen;
    let ok = int_of row.(0)
    and pk = int_of row.(1)
    and sk = int_of row.(2) in
    if ok <> !cur
    then (
      cur := ok;
      expect_line := 0);
    incr expect_line;
    if int_of row.(3) <> !expect_line then incr bad_num;
    if not (List.mem sk (Hashtbl.find ps pk)) then incr bad_supp;
    let qty = real_of row.(4) in
    Alcotest.(check bool) "l_quantity in 1..50" true (qty >= 1.0 && qty <= 50.0);
    if Float.abs (real_of row.(5) -. (qty *. retail_price pk)) > 1e-6 then incr bad_price;
    let d = real_of row.(6)
    and tx = real_of row.(7) in
    Alcotest.(check bool) "l_discount in 0..0.10" true (d >= 0.0 && d <= 0.10);
    Alcotest.(check bool) "l_tax in 0..0.08" true (tx >= 0.0 && tx <= 0.08);
    let ship = text row.(10)
    and receipt = text row.(12) in
    let flag = text row.(8)
    and status = text row.(9) in
    let want_flag =
      if receipt <= "1995-06-17" then flag = "R" || flag = "A" else flag = "N"
    in
    let want_status = if ship > "1995-06-17" then status = "O" else status = "F" in
    if not (want_flag && want_status) then incr bad_flag;
    Alcotest.(check bool) "l_shipinstruct" true (List.mem (text row.(13)) instructs);
    Alcotest.(check bool) "l_shipmode" true (List.mem (text row.(14)) modes);
    let c = String.length (text row.(15)) in
    Alcotest.(check bool) "l_comment length" true (c >= 10 && c <= 43));
  Alcotest.(check int) "l_linenumber sequential from 1" 0 !bad_num;
  Alcotest.(check int) "l_suppkey supplies l_partkey" 0 !bad_supp;
  Alcotest.(check int) "l_extendedprice = quantity * retailprice" 0 !bad_price;
  Alcotest.(check int) "returnflag/linestatus derived from dates" 0 !bad_flag;
  (* Without this the four zero-checks above would all pass on an empty
     stream.  15 000 orders at 1..7 lines each. *)
  Alcotest.(check bool)
    (Printf.sprintf "lineitem emitted rows (got %d)" !seen)
    true
    (!seen >= 15000 && !seen <= 15000 * 7)
;;

(* Q4 and Q12 compare commitdate against receiptdate in both directions, so
   the data must contain rows on each side. *)
let test_commitdate_straddles_receiptdate () =
  let g = ctx ~sf:0.01 () in
  let late = ref 0
  and early = ref 0 in
  G.iter_rows g ~table:"lineitem" ~f:(fun row ->
    if text row.(11) < text row.(12) then incr late else incr early);
  Alcotest.(check bool) "some commit < receipt" true (!late > 0);
  Alcotest.(check bool) "some commit >= receipt" true (!early > 0)
;;

let () =
  Alcotest.run
    "tpch_gen"
    [ ( "cardinality"
      , [ Alcotest.test_case "fixed tables" `Quick test_fixed_cardinalities
        ; Alcotest.test_case "scaled tables" `Quick test_scaled_cardinalities
        ; Alcotest.test_case "lineitem range" `Quick test_lineitem_in_spec_range
        ; Alcotest.test_case "row_count agrees" `Quick test_row_count_matches_iteration
        ; Alcotest.test_case "row_count lineitem" `Quick test_row_count_lineitem_exact
        ] )
    ; ( "determinism"
      , [ Alcotest.test_case "same seed" `Quick test_determinism_same_seed
        ; Alcotest.test_case "different seed" `Quick test_different_seed_differs
        ; Alcotest.test_case
            "independent streams"
            `Quick
            test_table_streams_are_independent
        ] )
    ; ( "integrity"
      , [ Alcotest.test_case "nation -> region" `Quick test_nation_references_region
        ; Alcotest.test_case "lineitem date order" `Quick test_lineitem_dates_are_ordered
        ; Alcotest.test_case "date format" `Quick test_date_format
        ; Alcotest.test_case "dates in window" `Quick test_all_dates_in_window
        ; Alcotest.test_case "orders vs lineitem" `Quick test_orders_agree_with_lineitem
        ; Alcotest.test_case
            "commit straddles receipt"
            `Quick
            test_commitdate_straddles_receiptdate
        ] )
    ; ( "api"
      , [ Alcotest.test_case "tables" `Quick test_tables_list
        ; Alcotest.test_case "column_names arity" `Quick test_column_names_match_arity
        ; Alcotest.test_case "unknown table" `Quick test_unknown_table_raises
        ; Alcotest.test_case "bad sf" `Quick test_bad_sf_raises
        ] )
    ; ( "columns"
      , [ Alcotest.test_case "region and nation" `Quick test_region_and_nation_names
        ; Alcotest.test_case "supplier" `Quick test_supplier_rows
        ; Alcotest.test_case "supplier markers" `Slow test_supplier_comment_markers
        ; Alcotest.test_case
            "no markers at small sf"
            `Quick
            test_supplier_markers_absent_at_small_sf
        ; Alcotest.test_case "part" `Quick test_part_rows
        ; Alcotest.test_case "partsupp" `Quick test_partsupp_rows
        ; Alcotest.test_case
            "partsupp distinct at sf 0.012"
            `Quick
            test_partsupp_distinct_at_awkward_sf
        ; Alcotest.test_case "customer" `Quick test_customer_rows
        ; Alcotest.test_case "orders" `Quick test_orders_rows
        ; Alcotest.test_case "lineitem" `Quick test_lineitem_rows
        ] )
    ]
;;
