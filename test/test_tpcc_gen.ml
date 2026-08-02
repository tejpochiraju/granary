module V = Granary_tpc.Tpc_value

let test_literal_int () = Alcotest.(check string) "int" "42" (V.literal (V.VInt 42))
let test_literal_null () = Alcotest.(check string) "null" "NULL" (V.literal V.VNull)

(* granary types a literal by its text: a REAL column rejects "100" under
   strict column typing, so a whole-valued REAL must keep a fractional part. *)
let test_literal_whole_real_keeps_point () =
  Alcotest.(check string) "whole real" "100.0" (V.literal (V.VReal 100.0))
;;

let test_literal_fractional_real () =
  Alcotest.(check string) "fractional real" "1.5" (V.literal (V.VReal 1.5))
;;

let test_literal_text_quoted () =
  Alcotest.(check string) "text" "'abc'" (V.literal (V.VText "abc"))
;;

let test_literal_text_escapes_quote () =
  Alcotest.(check string) "embedded quote" "'O''Hara'" (V.literal (V.VText "O'Hara"))
;;

(* A non-finite REAL has no SQL literal, and the whole-valued rule would turn
   one into a plausible-looking wrong one: "nan"/"inf" contain no '.', so the
   ".0" suffix would be appended and `nan.0` emitted as SQL.  Unreachable from
   the generators today; the guard is what keeps it that way. *)
let test_literal_non_finite_real_raises () =
  List.iter
    (fun (f, rendered) ->
       Alcotest.check_raises
         (Printf.sprintf "%s has no SQL literal" rendered)
         (Invalid_argument
            (Printf.sprintf "Tpc_value.literal: %s has no SQL literal" rendered))
         (fun () -> ignore (V.literal (V.VReal f))))
    [ Float.nan, Float.to_string Float.nan
    ; Float.infinity, Float.to_string Float.infinity
    ; Float.neg_infinity, Float.to_string Float.neg_infinity
    ]
;;

module G = Granary_tpc.Tpcc_gen

let gen ?(warehouses = 1) () = G.create ~seed:42 ~warehouses

let test_row_counts () =
  let g = gen ~warehouses:2 () in
  let expect table n = Alcotest.(check int) table n (G.row_count g ~table) in
  expect "warehouse" 2;
  expect "district" 20;
  expect "customer" 60_000;
  expect "history" 60_000;
  (* item is fixed by the spec and must NOT scale with warehouses *)
  expect "item" 100_000;
  expect "stock" 200_000;
  expect "orders" 60_000;
  expect "new_order" 18_000
;;

let test_item_does_not_scale () =
  Alcotest.(check int)
    "item is 100k at any warehouse count"
    (G.row_count (gen ~warehouses:1 ()) ~table:"item")
    (G.row_count (gen ~warehouses:4 ()) ~table:"item")
;;

let test_warehouses_accessor () =
  Alcotest.(check int) "warehouses" 3 (G.warehouses (gen ~warehouses:3 ()))
;;

let test_create_rejects_zero () =
  Alcotest.check_raises
    "warehouses must be positive"
    (Invalid_argument "Tpcc_gen.create: warehouses must be positive")
    (fun () -> ignore (G.create ~seed:1 ~warehouses:0))
;;

let collect g ~table =
  let acc = ref [] in
  G.iter_rows g ~table ~f:(fun row -> acc := row :: !acc);
  List.rev !acc
;;

(* "item" and "stock" are 100k rows each; the cheap [row_count] assertions
   above cover them, and materialising them here would only slow the suite. *)
let test_iter_rows_matches_row_count () =
  let g = gen () in
  List.iter
    (fun table ->
       Alcotest.(check int)
         (table ^ ": iter_rows agrees with row_count")
         (G.row_count g ~table)
         (List.length (collect g ~table)))
    [ "warehouse"
    ; "district"
    ; "customer"
    ; "history"
    ; "orders"
    ; "new_order"
    ; "order_line"
    ]
;;

let test_item_spot_check () =
  let g = gen () in
  let first = ref None
  and n = ref 0 in
  G.iter_rows g ~table:"item" ~f:(fun row ->
    incr n;
    if !n = 1 then first := Some row);
  Alcotest.(check int) "item rows" 100_000 !n;
  match !first with
  | None -> Alcotest.fail "item: no rows"
  | Some row ->
    Alcotest.(check int) "item arity" 5 (Array.length row);
    Alcotest.(check bool) "i_id starts at 1" true (row.(0) = Granary_tpc.Tpc_value.VInt 1)
;;

let test_same_seed_identical_output () =
  let render g ~table =
    String.concat
      "\n"
      (List.map
         (fun row ->
            String.concat "," (List.map Granary_tpc.Tpc_value.literal (Array.to_list row)))
         (collect g ~table))
  in
  let a = G.create ~seed:7 ~warehouses:1
  and b = G.create ~seed:7 ~warehouses:1 in
  List.iter
    (fun table ->
       Alcotest.(check string)
         (table ^ ": byte-identical at one seed")
         (render a ~table)
         (render b ~table))
    [ "district"; "customer"; "orders"; "order_line" ]
;;

let test_different_seed_differs () =
  let a = G.create ~seed:1 ~warehouses:1
  and b = G.create ~seed:2 ~warehouses:1 in
  Alcotest.(check bool)
    "customer rows differ at different seeds"
    false
    (collect a ~table:"customer" = collect b ~table:"customer")
;;

let test_columns_agree_with_arity () =
  let g = gen () in
  List.iter
    (fun table ->
       let cols = List.length (G.column_names ~table) in
       match collect g ~table with
       | [] -> Alcotest.fail (table ^ ": no rows")
       | row :: _ -> Alcotest.(check int) (table ^ ": arity") cols (Array.length row))
    Granary_tpc.Tpcc_schema.tables
;;

let test_unknown_table_raises () =
  let g = gen () in
  Alcotest.check_raises
    "unknown table"
    (Invalid_argument "Tpcc_gen: unknown table nope")
    (fun () -> ignore (G.row_count g ~table:"nope"))
;;

let test_district_initial_state () =
  (* Consistency conditions 1-3 must hold on the generated state before any
     transaction runs; a generator bug would otherwise be indistinguishable
     from a driver that corrupts a good database. *)
  let g = gen () in
  List.iter
    (fun row ->
       Alcotest.(check bool)
         "d_ytd = 30000, d_next_o_id = 3001"
         true
         (row.(9) = Granary_tpc.Tpc_value.VReal 30000.0
          && row.(10) = Granary_tpc.Tpc_value.VInt 3001))
    (collect g ~table:"district")
;;

let test_warehouse_ytd_matches_districts () =
  (* Consistency condition 1: w_ytd = sum of the warehouse's ten d_ytd. *)
  let g = gen ~warehouses:2 () in
  let sums = Hashtbl.create 4 in
  List.iter
    (fun row ->
       match row.(1), row.(9) with
       | Granary_tpc.Tpc_value.VInt w, Granary_tpc.Tpc_value.VReal ytd ->
         let prev =
           try Hashtbl.find sums w with
           | Not_found -> 0.0
         in
         Hashtbl.replace sums w (prev +. ytd)
       | _ -> Alcotest.fail "district: unexpected column types")
    (collect g ~table:"district");
  List.iter
    (fun row ->
       match row.(0), row.(8) with
       | Granary_tpc.Tpc_value.VInt w, Granary_tpc.Tpc_value.VReal w_ytd ->
         Alcotest.(check (float 1e-9)) "w_ytd = sum d_ytd" w_ytd (Hashtbl.find sums w)
       | _ -> Alcotest.fail "warehouse: unexpected column types")
    (collect g ~table:"warehouse")
;;

let test_order_carrier_null_above_2100 () =
  (* An order at or above the new_order watermark is undelivered: it has no
     carrier.  Below it, the carrier is a route id in [1,10]. *)
  let g = gen () in
  List.iter
    (fun row ->
       match row.(0) with
       | Granary_tpc.Tpc_value.VInt o_id when o_id >= 2101 ->
         Alcotest.(check bool)
           (Printf.sprintf "o_id %d has no carrier" o_id)
           true
           (row.(5) = Granary_tpc.Tpc_value.VNull)
       | Granary_tpc.Tpc_value.VInt o_id ->
         (match row.(5) with
          | Granary_tpc.Tpc_value.VInt c ->
            Alcotest.(check bool)
              (Printf.sprintf "o_id %d carrier in [1,10]" o_id)
              true
              (c >= 1 && c <= 10)
          | _ -> Alcotest.fail (Printf.sprintf "o_id %d: carrier should be set" o_id))
       | _ -> Alcotest.fail "orders: o_id should be an integer")
    (collect g ~table:"orders")
;;

let test_new_order_is_last_900_per_district () =
  (* Consistency condition 3: the new_order rows of a district are exactly the
     contiguous run 2101..3000. *)
  let g = gen ~warehouses:2 () in
  let per_district = Hashtbl.create 32 in
  List.iter
    (fun row ->
       match row.(0), row.(1), row.(2) with
       | ( Granary_tpc.Tpc_value.VInt o_id
         , Granary_tpc.Tpc_value.VInt d_id
         , Granary_tpc.Tpc_value.VInt w_id ) ->
         let key = w_id, d_id in
         let prev =
           try Hashtbl.find per_district key with
           | Not_found -> []
         in
         Hashtbl.replace per_district key (o_id :: prev)
       | _ -> Alcotest.fail "new_order: unexpected column types")
    (collect g ~table:"new_order");
  Alcotest.(check int) "district count" 20 (Hashtbl.length per_district);
  let expected = List.init 900 (fun i -> i + 2101) in
  Hashtbl.iter
    (fun _ ids ->
       Alcotest.(check (list int)) "o_ids 2101..3000" expected (List.sort compare ids))
    per_district
;;

let test_order_lines_match_ol_cnt () =
  (* Consistency condition 4: the sum of o_ol_cnt is the order_line row count.
     Checked here, at the generator, where a mismatch is cheap to localise. *)
  let g = gen () in
  let total = ref 0 in
  List.iter
    (fun row ->
       match row.(6) with
       | Granary_tpc.Tpc_value.VInt n -> total := !total + n
       | _ -> Alcotest.fail "orders: o_ol_cnt should be an integer")
    (collect g ~table:"orders");
  Alcotest.(check int)
    "sum o_ol_cnt = order_line rows"
    !total
    (G.row_count g ~table:"order_line");
  Alcotest.(check int)
    "sum o_ol_cnt = emitted order_line rows"
    !total
    (List.length (collect g ~table:"order_line"))
;;

let test_customer_ids_are_a_permutation () =
  (* The spec gives every customer exactly one order; o_c_id must therefore be
     a permutation of 1..3000 per district, not 3,000 independent draws. *)
  let g = gen () in
  let seen = Hashtbl.create 3001 in
  List.iter
    (fun row ->
       match row.(1), row.(3) with
       | Granary_tpc.Tpc_value.VInt 1, Granary_tpc.Tpc_value.VInt c_id ->
         Hashtbl.replace seen c_id ()
       | _ -> ())
    (collect g ~table:"orders");
  Alcotest.(check int) "3000 distinct o_c_id in district 1" 3000 (Hashtbl.length seen)
;;

(* --- properties ------------------------------------------------------- *)

(* The structural contract every consumer depends on, over arbitrary seeds
   and small scale factors: each emitted row has exactly as many cells as
   [column_names] has names, and [row_count] — which the loader trusts to
   size its batches and [test_row_counts_landed] compares the database
   against — agrees with what [iter_rows] actually emits. An arity that drifts
   from [column_names] writes values into the wrong columns; a [row_count]
   that drifts from [iter_rows] makes every downstream count check compare
   two numbers that were never about the same thing.

   The warehouse count is capped at 2 and the case count kept low: one
   warehouse is already ~500k rows, and this property has to iterate all of
   them (twice for order_line, whose [row_count] replays the order stream). *)
let prop_rows_match_columns_and_counts =
  QCheck.Test.make
    ~count:4
    ~name:"every generated row has its table's arity, and row_count matches iter_rows"
    QCheck.(pair int (int_range 1 2))
    (fun (seed, warehouses) ->
       let gen = Granary_tpc.Tpcc_gen.create ~seed ~warehouses in
       List.for_all
         (fun table ->
            let arity = List.length (Granary_tpc.Tpcc_gen.column_names ~table) in
            let seen = ref 0 in
            let ok = ref true in
            Granary_tpc.Tpcc_gen.iter_rows gen ~table ~f:(fun row ->
              incr seen;
              if Array.length row <> arity then ok := false);
            !ok && !seen = Granary_tpc.Tpcc_gen.row_count gen ~table)
         Granary_tpc.Tpcc_gen.tables)
;;

(* --- #509: the reproducibility contract, pinned ----------------------- *)

(* A seed must pin the dataset across compilers, flambda settings and OCaml
   versions. This digest is what enforces it: a change means the generated data
   changed, either deliberately — recompute it and say so — or because some
   expression went back to drawing twice in a position OCaml leaves unordered,
   which is the #509 bug. Floats go in by their exact bit pattern so the pin
   cannot be loosened by a printing difference.

   The item and stock tables are excluded only because they are 100,000 rows
   each and fixed by the spec regardless of warehouse count; every table whose
   generator this change touched is covered. *)
let dataset_digest ~seed =
  let g = Granary_tpc.Tpcc_gen.create ~seed ~warehouses:1 in
  let buf = Buffer.create 4096 in
  let add = function
    | V.VInt i -> Buffer.add_string buf (string_of_int i)
    | V.VReal f -> Buffer.add_string buf (Int64.to_string (Int64.bits_of_float f))
    | V.VText s -> Buffer.add_string buf s
    | V.VNull -> Buffer.add_string buf "NULL"
  in
  List.iter
    (fun table ->
       Buffer.add_string buf table;
       Granary_tpc.Tpcc_gen.iter_rows g ~table ~f:(fun row ->
         Array.iter
           (fun v ->
              add v;
              Buffer.add_char buf '\001')
           row);
       Buffer.add_char buf '\002')
    [ "warehouse"
    ; "district"
    ; "customer"
    ; "history"
    ; "orders"
    ; "new_order"
    ; "order_line"
    ];
  Digest.to_hex (Digest.string (Buffer.contents buf))
;;

let test_dataset_digest_is_pinned () =
  Alcotest.(check string)
    "seed 42, W=1 dataset digest"
    "72b3401c41eca4ece2bada3be4a6ea17"
    (dataset_digest ~seed:42)
;;

let test_dataset_digest_depends_on_seed () =
  Alcotest.(check bool)
    "a different seed gives a different dataset"
    false
    (String.equal (dataset_digest ~seed:42) (dataset_digest ~seed:43))
;;

(* Clause 4.3.2.2 specifies c_phone as n_string(16,16) — sixteen digits. It was
   generated with the 64-symbol alphanumeric a_string until #509. *)
let test_c_phone_is_sixteen_digits () =
  let g = Granary_tpc.Tpcc_gen.create ~seed:42 ~warehouses:1 in
  let seen = ref 0 in
  let bad = ref 0 in
  Granary_tpc.Tpcc_gen.iter_rows g ~table:"customer" ~f:(fun row ->
    incr seen;
    match row.(11) with
    | V.VText s ->
      if not (String.length s = 16 && String.for_all (fun c -> c >= '0' && c <= '9') s)
      then incr bad
    | _ -> incr bad);
  Alcotest.(check int) "every c_phone is 16 digits" 0 !bad;
  Alcotest.(check int) "and every customer was inspected" 30_000 !seen
;;

let () =
  Alcotest.run
    "tpcc_gen"
    [ ( "literal"
      , [ Alcotest.test_case "int" `Quick test_literal_int
        ; Alcotest.test_case "null" `Quick test_literal_null
        ; Alcotest.test_case
            "whole real keeps a point"
            `Quick
            test_literal_whole_real_keeps_point
        ; Alcotest.test_case "fractional real" `Quick test_literal_fractional_real
        ; Alcotest.test_case "text quoted" `Quick test_literal_text_quoted
        ; Alcotest.test_case "text escapes quote" `Quick test_literal_text_escapes_quote
        ; Alcotest.test_case
            "non-finite real raises"
            `Quick
            test_literal_non_finite_real_raises
        ] )
    ; ( "shape"
      , [ Alcotest.test_case "row counts" `Quick test_row_counts
        ; Alcotest.test_case "item does not scale" `Quick test_item_does_not_scale
        ; Alcotest.test_case "warehouses accessor" `Quick test_warehouses_accessor
        ; Alcotest.test_case "create rejects zero" `Quick test_create_rejects_zero
        ; Alcotest.test_case
            "iter_rows matches row_count"
            `Quick
            test_iter_rows_matches_row_count
        ; Alcotest.test_case "item spot check" `Quick test_item_spot_check
        ; Alcotest.test_case
            "columns agree with arity"
            `Quick
            test_columns_agree_with_arity
        ; Alcotest.test_case "unknown table raises" `Quick test_unknown_table_raises
        ] )
    ; ( "determinism"
      , [ Alcotest.test_case
            "same seed is identical"
            `Quick
            test_same_seed_identical_output
        ; Alcotest.test_case "different seed differs" `Quick test_different_seed_differs
        ] )
    ; ( "consistency"
      , [ Alcotest.test_case "district initial state" `Quick test_district_initial_state
        ; Alcotest.test_case
            "w_ytd = sum d_ytd"
            `Quick
            test_warehouse_ytd_matches_districts
        ; Alcotest.test_case
            "carrier null above 2100"
            `Quick
            test_order_carrier_null_above_2100
        ; Alcotest.test_case
            "new_order is the last 900"
            `Quick
            test_new_order_is_last_900_per_district
        ; Alcotest.test_case
            "order lines match o_ol_cnt"
            `Quick
            test_order_lines_match_ol_cnt
        ; Alcotest.test_case
            "o_c_id is a permutation"
            `Quick
            test_customer_ids_are_a_permutation
        ] )
    ; ( "reproducibility 509"
      , [ Alcotest.test_case "dataset digest pinned" `Quick test_dataset_digest_is_pinned
        ; Alcotest.test_case
            "depends on the seed"
            `Quick
            test_dataset_digest_depends_on_seed
        ; Alcotest.test_case
            "c_phone is n_string(16,16)"
            `Quick
            test_c_phone_is_sixteen_digits
        ] )
    ; "properties", [ QCheck_alcotest.to_alcotest prop_rows_match_columns_and_counts ]
    ]
;;
