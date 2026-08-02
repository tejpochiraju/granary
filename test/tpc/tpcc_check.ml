type condition =
  { number : int
  ; description : string
  ; queries : string list
  ; check : string list list list -> string option
  }

type outcome =
  | Holds
  | Violated of string
  | Not_run

(* ALL FOUR CONDITIONS ARE JOINED CLIENT-SIDE, AND THAT IS LOAD-BEARING.
   Every condition asks the engine only for plain rows and bare GROUP BY
   aggregates, then joins them in OCaml over the UNION of both sides' keys.
   Do not "simplify" any of them back into a single SQL query with a
   correlated scalar subquery in the WHERE clause. Two independent holes make
   that shape a vacuous oracle:

   1. #485 — an UNQUALIFIED outer-column reference inside a correlated
      subquery does not fail; it silently evaluates to NULL. Written that way,
      condition 1's predicate becomes `w_ytd <> NULL`, never true, zero rows,
      and zero rows is that convention's pass. Verified on a four-row repro:
      `(SELECT SUM(d_ytd) FROM district WHERE d_w_id = w_id)` yields NULL
      while `... WHERE d_w_id = warehouse.w_id` yields the right sum. It is
      the correlation, not the aggregate — a correlated row count, which can
      never legitimately be NULL, came back NULL too.

   2. Even fully qualified, `x <> (SELECT SUM/MAX ...)` is NULL — and so not
      returned, and so a pass — whenever the subquery matches ZERO ROWS. A
      warehouse with no district rows passed condition 1 for any [w_ytd]; a
      district with no orders and no new_order rows passed condition 2 for
      any [d_next_o_id]. Likewise a GROUP BY over new_order alone never
      produces a group for a district that has vanished from new_order, so
      condition 3 simply stopped examining it. Deleting the last row of a
      group is exactly the case the perturb-a-value negative tests could not
      reach, and PR 2's Delivery profile DELETES new_order rows, so a drained
      district is reachable in the real mix.

   The fix for both is the same and is what this module now does everywhere:
   drive each condition from the table that must have the row (warehouse for
   1, district for 2, 3 and 4), aggregate the other side with a plain GROUP
   BY, and treat a key present on one side but missing from the other as an
   explicit outcome rather than as an absence of evidence.

   Condition 4 needs the driving side for the same reason even though its two
   aggregates are symmetric: joined against each other alone, a district that
   lost BOTH its orders and its order_line rows produces no key on either side
   and passes vacuously. Condition 2's orders half happens to catch that
   district today, so the oracle as a whole is not blind — but a condition
   must not depend on a different condition to notice its own subject
   disappearing.

   The guard is test/test_tpcc_load.ml's "the conditions can fail" cases,
   which break each invariant against a real loaded population — by
   perturbing a value AND by deleting a whole group — and require Violated.
   They are what caught both holes, and they will catch them again.

   (#507 used to be a separate, unrelated reason conditions 3 and 4 could not
   be one query: the aggregation planner rejected any GROUP BY projection item
   that was not a bare grouped column, a bare aggregate call, or a window
   function. `MAX(x) - MIN(x) + 1 = COUNT-star` is now accepted, so that reason
   is gone — and the client-side join still stays, because it is what closes
   hole 2. A subquery beside an aggregate in the projection remains rejected.) *)

module Key_map = Map.Make (struct
    type t = string list

    let compare = compare
  end)

let render_row row = String.concat "; " row

let shape_error ~label row =
  Printf.sprintf "%s: unexpected row shape [%s]" label (render_row row)
;;

let value_error ~label row =
  Printf.sprintf "%s: unparseable aggregate in row [%s]" label (render_row row)
;;

(* Folds one query's rows into a key -> value map, collecting a report string
   for every row [split] rejects instead of dropping it. A dropped row is
   exactly how a check could go vacuously quiet: the key it should have
   compared would simply be absent, and an absent key on the aggregate side
   is indistinguishable from a group that legitimately has no rows. *)
let map_of_rows ~split rows =
  List.fold_left
    (fun (acc, bad) row ->
       match split row with
       | Ok (key, v) -> Key_map.add key v acc, bad
       | Error msg -> acc, msg :: bad)
    (Key_map.empty, [])
    rows
;;

let keys m = Key_map.fold (fun k _ acc -> k :: acc) m []

(* The union of both sides' keys, not either side's alone: a key present on
   one side and missing from the other is the case the old single-query form
   could not see, so it must be visited. *)
let union_keys key_lists = List.concat key_lists |> List.sort_uniq compare

let report ~number ~description bad =
  match bad with
  | [] -> None
  | first :: _ ->
    Some
      (Printf.sprintf
         "condition %d (%s): %d offending district(s)/row(s), first = %s"
         number
         description
         (List.length bad)
         first)
;;

let empty_driver ~number ~description ~table =
  Printf.sprintf
    "condition %d (%s): the %s query returned zero rows; %s is never empty in a real \
     run, so this is a check that stopped seeing real data, not a satisfied invariant"
    number
    description
    table
    table
;;

let int_split ~label ~key_arity row =
  match key_arity, row with
  | 1, [ k; v ] ->
    (match int_of_string_opt v with
     | Some i -> Ok ([ k ], i)
     | None -> Error (value_error ~label row))
  | 2, [ k1; k2; v ] ->
    (match int_of_string_opt v with
     | Some i -> Ok ([ k1; k2 ], i)
     | None -> Error (value_error ~label row))
  | _ -> Error (shape_error ~label row)
;;

let float_split ~label row =
  match row with
  | [ k; v ] ->
    (match float_of_string_opt v with
     | Some f -> Ok ([ k ], f)
     | None -> Error (value_error ~label row))
  | _ -> Error (shape_error ~label row)
;;

let wd_key ~label row =
  match row with
  | [ w; d ] -> Ok ([ w; d ], ())
  | _ -> Error (shape_error ~label row)
;;

let key_str key = String.concat "," key

(* --- condition 1 ------------------------------------------------------- *)

let cond1_description = "w_ytd equals the sum of its districts' d_ytd"
let cond1_warehouse_query = {|SELECT w_id, w_ytd FROM warehouse|}
let cond1_district_query = {|SELECT d_w_id, SUM(d_ytd) FROM district GROUP BY d_w_id|}

(* w_ytd and d_ytd are money carried in REAL columns, so the comparison needs
   a tolerance — this is NECESSARY, not a weakening of the check. w_ytd is a
   single accumulator while SUM(d_ytd) re-sums ten separately accumulated
   values, so the two take different rounding paths over a run and an exact
   [=] would report a violation after a handful of Payments even though the
   invariant holds exactly in decimal.

   Half a cent is below the smallest movement the workload can make (Payment's
   minimum amount is 1.00) and far above any plausible summation drift at
   TPC-C's magnitudes, so it cannot hide a real discrepancy.

   The tolerance is ABSOLUTE while double rounding error is proportional to
   magnitude, so a relative tolerance would be the scale-proof form. Absolute
   is chosen deliberately rather than by omission: at this harness's
   magnitudes — w_ytd starts at 300,000 and grows by at most 5,000 per
   Payment — a double's ulp is around 1e-11, six orders of magnitude below
   half a cent, and stays far below it until w_ytd passes roughly 1e13. That
   is billions of maximum-sized Payments against one warehouse, far outside
   anything this benchmark reaches; should it ever get there the fix is to
   make this relative, not to widen it. The boundary is pinned by a unit test
   in test_tpcc_check.ml so widening it cannot pass unnoticed. *)
let money_epsilon = 0.005
let money_eq a b = Float.abs (a -. b) <= money_epsilon

let cond1_check rows =
  match rows with
  | [ warehouse_rows; district_rows ] ->
    let w_map, w_bad =
      map_of_rows ~split:(float_split ~label:"condition 1 (warehouse)") warehouse_rows
    in
    let d_map, d_bad =
      map_of_rows ~split:(float_split ~label:"condition 1 (district)") district_rows
    in
    let mismatches =
      List.filter_map
        (fun key ->
           match Key_map.find_opt key w_map, Key_map.find_opt key d_map with
           | Some w, Some s when money_eq w s -> None
           | Some w, Some s ->
             Some
               (Printf.sprintf
                  "warehouse (w=%s): w_ytd=%.17g <> sum(d_ytd)=%.17g"
                  (key_str key)
                  w
                  s)
           | Some w, None ->
             (* Hole A: this is the case the old single-query form reported as
                a pass, because SUM over zero rows is NULL and `w_ytd <> NULL`
                is NULL. Every warehouse has ten districts. *)
             Some
               (Printf.sprintf
                  "warehouse (w=%s): w_ytd=%.17g but the warehouse has no district rows \
                   at all"
                  (key_str key)
                  w)
           | None, Some s ->
             Some
               (Printf.sprintf
                  "warehouse (w=%s): districts sum to d_ytd=%.17g but there is no \
                   warehouse row"
                  (key_str key)
                  s)
           | None, None -> None)
        (union_keys [ keys w_map; keys d_map ])
    in
    (match warehouse_rows with
     | [] ->
       Some (empty_driver ~number:1 ~description:cond1_description ~table:"warehouse")
     | _ :: _ ->
       (* Malformed rows first: the comparison could not even be attempted for
          them, so they are the more actionable "first offender". *)
       report ~number:1 ~description:cond1_description (w_bad @ d_bad @ mismatches))
  | _ -> Some "condition 1: expected exactly two query results"
;;

(* --- condition 2 ------------------------------------------------------- *)

let cond2_description = "d_next_o_id - 1 equals max(o_id) and max(no_o_id) per district"
let cond2_district_query = {|SELECT d_w_id, d_id, d_next_o_id FROM district|}

let cond2_orders_query =
  {|SELECT o_w_id, o_d_id, MAX(o_id) FROM orders GROUP BY o_w_id, o_d_id|}
;;

let cond2_new_order_query =
  {|SELECT no_w_id, no_d_id, MAX(no_o_id) FROM new_order GROUP BY no_w_id, no_d_id|}
;;

(* The two halves of condition 2 are NOT symmetric, and the asymmetry is a
   deliberate decision about what an empty group means on each side.

   orders: TPC-C never deletes an order. Delivery sets o_carrier_id on an
   existing row; nothing removes one. So a district that has a d_next_o_id and
   NO orders rows is a genuine violation — the counter says orders were
   issued and the table disagrees — and it is reported.

   new_order: Delivery DELETES the new_order row of every order it retires. A
   district whose queue has fully drained therefore has no new_order rows at
   all, and that is a legitimate steady state, not a corruption. So the
   max(no_o_id) disjunct is checked only when the district HAS new_order rows.
   It is skipped consciously here rather than being invisibly absent, which is
   what the old GROUP-BY-driven form did: with no group for the district, the
   condition simply never looked, and nothing in the output said so.

   Where the queue is non-empty the equality still binds: Delivery removes the
   LOWEST no_o_id, so the maximum is always the most recently inserted order,
   which is d_next_o_id - 1. *)
let cond2_check rows =
  match rows with
  | [ district_rows; orders_rows; new_order_rows ] ->
    let d_map, d_bad =
      map_of_rows
        ~split:(int_split ~label:"condition 2 (district)" ~key_arity:2)
        district_rows
    in
    let o_map, o_bad =
      map_of_rows
        ~split:(int_split ~label:"condition 2 (orders)" ~key_arity:2)
        orders_rows
    in
    let n_map, n_bad =
      map_of_rows
        ~split:(int_split ~label:"condition 2 (new_order)" ~key_arity:2)
        new_order_rows
    in
    let check_district key next_o_id =
      let expected = next_o_id - 1 in
      let orders_report =
        match Key_map.find_opt key o_map with
        | Some mx when mx = expected -> None
        | Some mx ->
          Some
            (Printf.sprintf
               "district (%s): d_next_o_id - 1 = %d <> max(o_id) = %d"
               (key_str key)
               expected
               mx)
        | None ->
          Some
            (Printf.sprintf
               "district (%s): d_next_o_id - 1 = %d but the district has no orders rows \
                at all (orders are never deleted)"
               (key_str key)
               expected)
      in
      let new_order_report =
        match Key_map.find_opt key n_map with
        | Some mx when mx = expected -> None
        | Some mx ->
          Some
            (Printf.sprintf
               "district (%s): d_next_o_id - 1 = %d <> max(no_o_id) = %d"
               (key_str key)
               expected
               mx)
        (* Legitimate: every order delivered, queue empty.  See the note
           above; examined and skipped, not skipped by omission. *)
        | None -> None
      in
      List.filter_map Fun.id [ orders_report; new_order_report ]
    in
    let orphan label map =
      Key_map.fold
        (fun key _ acc ->
           if Key_map.mem key d_map
           then acc
           else
             Printf.sprintf
               "district (%s): %s rows exist but there is no district row"
               (key_str key)
               label
             :: acc)
        map
        []
    in
    let mismatches =
      Key_map.fold (fun key v acc -> check_district key v @ acc) d_map []
      @ orphan "orders" o_map
      @ orphan "new_order" n_map
    in
    (match district_rows with
     | [] ->
       Some (empty_driver ~number:2 ~description:cond2_description ~table:"district")
     | _ :: _ ->
       report ~number:2 ~description:cond2_description (d_bad @ o_bad @ n_bad @ mismatches))
  | _ -> Some "condition 2: expected exactly three query results"
;;

(* --- condition 3 ------------------------------------------------------- *)

let cond3_description =
  "max(no_o_id) - min(no_o_id) + 1 equals the new_order row count per district"
;;

let cond3_district_query = {|SELECT d_w_id, d_id FROM district|}

let cond3_new_order_query =
  {|SELECT no_w_id, no_d_id, MAX(no_o_id), MIN(no_o_id), COUNT(*)
    FROM new_order
    GROUP BY no_w_id, no_d_id|}
;;

let cond3_split row =
  let label = "condition 3 (new_order)" in
  match row with
  | [ w; d; mx_s; mn_s; cnt_s ] ->
    (match int_of_string_opt mx_s, int_of_string_opt mn_s, int_of_string_opt cnt_s with
     | Some mx, Some mn, Some cnt -> Ok ([ w; d ], (mx, mn, cnt))
     | _ -> Error (value_error ~label row))
  | _ -> Error (shape_error ~label row)
;;

(* Driven from district, not from the new_order GROUP BY, so that a district
   which has vanished from new_order is EXAMINED rather than invisibly
   absent. A district with an empty new-order queue is legitimate — Delivery
   deletes new_order rows, and a fully delivered district has none — so it is
   consciously skipped here, exactly as condition 2's new_order half is. The
   contiguity claim is about the ids that ARE present; there is nothing to
   claim about a district with none. The anti-vacuity guard is on the DRIVING
   side instead: an empty district table means the check stopped seeing real
   data and is reported. *)
let cond3_check rows =
  match rows with
  | [ district_rows; new_order_rows ] ->
    let d_map, d_bad =
      map_of_rows ~split:(wd_key ~label:"condition 3 (district)") district_rows
    in
    let n_map, n_bad = map_of_rows ~split:cond3_split new_order_rows in
    let ranges =
      List.filter_map
        (fun key ->
           match Key_map.find_opt key d_map, Key_map.find_opt key n_map with
           | Some (), Some (mx, mn, cnt) when mx - mn + 1 = cnt -> None
           | Some (), Some (mx, mn, cnt) ->
             Some
               (Printf.sprintf
                  "district (%s): max=%d min=%d count=%d (max-min+1=%d)"
                  (key_str key)
                  mx
                  mn
                  cnt
                  (mx - mn + 1))
           | Some (), None -> None (* empty queue: legitimate, see above *)
           | None, Some (mx, mn, cnt) ->
             Some
               (Printf.sprintf
                  "district (%s): new_order rows exist (max=%d min=%d count=%d) but \
                   there is no district row"
                  (key_str key)
                  mx
                  mn
                  cnt)
           | None, None -> None)
        (union_keys [ keys d_map; keys n_map ])
    in
    (match district_rows with
     | [] ->
       Some (empty_driver ~number:3 ~description:cond3_description ~table:"district")
     | _ :: _ -> report ~number:3 ~description:cond3_description (d_bad @ n_bad @ ranges))
  | _ -> Some "condition 3: expected exactly two query results"
;;

(* --- condition 4 ------------------------------------------------------- *)

let cond4_description = "the sum of o_ol_cnt equals the order_line row count per district"
let cond4_district_query = {|SELECT d_w_id, d_id FROM district|}

let cond4_orders_query =
  {|SELECT o_w_id, o_d_id, SUM(o_ol_cnt) FROM orders GROUP BY o_w_id, o_d_id|}
;;

let cond4_lines_query =
  {|SELECT ol_w_id, ol_d_id, COUNT(*) FROM order_line GROUP BY ol_w_id, ol_d_id|}
;;

(* Driven from district, like conditions 2 and 3, even though its two
   aggregates are symmetric. Joined against each other alone, a district that
   lost BOTH its orders and its order_line rows contributes no key to either
   map and the union join has nothing to compare — it would pass vacuously,
   which is the same hole the other three conditions were just restructured to
   close. A district always has orders (they are never deleted) and every
   order always has its lines, so [None, None] under a real district row is a
   violation, not an empty case. *)
let cond4_check rows =
  match rows with
  | [ district_rows; orders_rows; lines_rows ] ->
    let d_map, d_bad =
      map_of_rows ~split:(wd_key ~label:"condition 4 (district)") district_rows
    in
    let orders_map, orders_bad =
      map_of_rows
        ~split:(int_split ~label:"condition 4 (orders)" ~key_arity:2)
        orders_rows
    in
    let lines_map, lines_bad =
      map_of_rows
        ~split:(int_split ~label:"condition 4 (order_line)" ~key_arity:2)
        lines_rows
    in
    let compare_district key =
      match Key_map.find_opt key orders_map, Key_map.find_opt key lines_map with
      | Some o, Some l when o = l -> None
      | Some o, Some l ->
        Some
          (Printf.sprintf
             "district (%s): sum(o_ol_cnt)=%d <> order_line count=%d"
             (key_str key)
             o
             l)
      | Some o, None ->
        Some
          (Printf.sprintf
             "district (%s): orders sum(o_ol_cnt)=%d but no order_line rows for this \
              district"
             (key_str key)
             o)
      | None, Some l ->
        Some
          (Printf.sprintf
             "district (%s): order_line count=%d but no orders rows for this district"
             (key_str key)
             l)
      | None, None ->
        Some
          (Printf.sprintf
             "district (%s): the district exists but has neither orders nor order_line \
              rows"
             (key_str key))
    in
    let mismatches =
      List.filter_map
        (fun key ->
           if Key_map.mem key d_map
           then compare_district key
           else
             Some
               (Printf.sprintf
                  "district (%s): orders and/or order_line rows exist but there is no \
                   district row"
                  (key_str key)))
        (union_keys [ keys d_map; keys orders_map; keys lines_map ])
    in
    (match district_rows with
     | [] ->
       Some (empty_driver ~number:4 ~description:cond4_description ~table:"district")
     | _ :: _ ->
       report
         ~number:4
         ~description:cond4_description
         (d_bad @ orders_bad @ lines_bad @ mismatches))
  | _ -> Some "condition 4: expected exactly three query results"
;;

let conditions =
  [ { number = 1
    ; description = cond1_description
    ; queries = [ cond1_warehouse_query; cond1_district_query ]
    ; check = cond1_check
    }
  ; { number = 2
    ; description = cond2_description
    ; queries = [ cond2_district_query; cond2_orders_query; cond2_new_order_query ]
    ; check = cond2_check
    }
  ; { number = 3
    ; description = cond3_description
    ; queries = [ cond3_district_query; cond3_new_order_query ]
    ; check = cond3_check
    }
  ; { number = 4
    ; description = cond4_description
    ; queries = [ cond4_district_query; cond4_orders_query; cond4_lines_query ]
    ; check = cond4_check
    }
  ]
;;

let classify c ~rows =
  match c.check rows with
  | None -> Holds
  | Some report -> Violated report
;;

let label = function
  | Holds -> "ok"
  | Violated _ -> "VIOLATED"
  | Not_run -> "not-run"
;;

(* [Not_run] is a failure: a condition that stopped executing must not read as
   a pass.  Rendering a check that no longer runs as a benign token, and still
   exiting 0, is exactly the hole #502 closed on the TPC-H side. *)
let is_failure = function
  | Holds -> false
  | Violated _ | Not_run -> true
;;

let pp fmt = function
  | Holds -> Format.fprintf fmt "Holds"
  | Not_run -> Format.fprintf fmt "Not_run"
  | Violated report -> Format.fprintf fmt "Violated(%s)" report
;;
