(** #417 Phase 2: incremental relational operators over Z-sets.

    The load-bearing property for every operator is {b incremental == batch}: for
    any sequence of input deltas, folding the operator's per-delta outputs equals
    recomputing the operator over the summed inputs.  That is what makes a view
    maintainable from the delta feed (#419) rather than recomputed from scratch. *)

module Zset = Sqlocaml_ivm.Zset

(* (id, attr) tuples for the two join inputs; (attr, attr) for the output. *)
module IntStr = struct
  type t = int * string

  let compare = compare
  let pp ppf (i, s) = Format.fprintf ppf "(%d,%s)" i s
end

module StrStr = struct
  type t = string * string

  let compare = compare
  let pp ppf (a, b) = Format.fprintf ppf "(%s,%s)" a b
end

module ZL = Zset.Make (IntStr)
module ZR = Zset.Make (IntStr)
module ZO = Zset.Make (StrStr)

module J = Sqlocaml_ivm.Join.Make (struct
    module Left = ZL
    module Right = ZR
    module Out = ZO

    type key = int

    let compare_key = Int.compare
    let key_left (i, _) = i
    let key_right (i, _) = i
    let combine (_, name) (_, dept) = name, dept
  end)

(* Reference batch equi-join: for matching keys, weight is the product. *)
let batch_join (l : ZL.t) (r : ZR.t) : ZO.t =
  ZL.fold
    (fun (li, ln) wl acc ->
       ZR.fold
         (fun (ri, rd) wr acc2 ->
            if li = ri then ZO.add acc2 (ZO.singleton (ln, rd) (wl * wr)) else acc2)
         r
         acc)
    l
    ZO.zero
;;

let test_join_basic () =
  let j = J.create () in
  let out =
    J.step
      j
      ~left:(ZL.of_list [ (1, "a"), 1; (2, "b"), 1 ])
      ~right:(ZR.of_list [ (1, "x"), 1 ])
  in
  Alcotest.(check (list (pair (pair string string) int)))
    "join 1 matches, 2 does not"
    [ ("a", "x"), 1 ]
    (List.sort compare (ZO.to_list out));
  Alcotest.(check bool) "output materialized" true (ZO.equal (J.output j) out)
;;

(* A later right-side insert must join against the already-integrated left. *)
let test_join_incremental_right_arrives_late () =
  let j = J.create () in
  let _ = J.step j ~left:(ZL.of_list [ (1, "a"), 1 ]) ~right:ZR.zero in
  let out2 = J.step j ~left:ZL.zero ~right:(ZR.of_list [ (1, "x"), 1 ]) in
  Alcotest.(check (list (pair (pair string string) int)))
    "late right joins integrated left"
    [ ("a", "x"), 1 ]
    (List.sort compare (ZO.to_list out2))
;;

(* A retraction on one side removes the previously emitted output rows. *)
let test_join_retraction () =
  let j = J.create () in
  let _ =
    J.step j ~left:(ZL.of_list [ (1, "a"), 1 ]) ~right:(ZR.of_list [ (1, "x"), 1 ])
  in
  let _ = J.step j ~left:(ZL.of_list [ (1, "a"), -1 ]) ~right:ZR.zero in
  Alcotest.(check bool)
    "output empty after retracting left"
    true
    (ZO.is_zero (J.output j))
;;

let str_gen = QCheck.oneof_list [ "a"; "b"; "c" ]
let arb_side = QCheck.(list (pair (pair (int_range 0 3) str_gen) (int_range (-2) 2)))

let arb_steps =
  QCheck.(list_size (Gen.int_range 0 6) (pair arb_side arb_side))
  |> QCheck.map (List.map (fun (l, r) -> ZL.of_list l, ZR.of_list r))
;;

(* Stronger than comparing only the final total: after EVERY step, both the
   accumulated returned deltas and the materialized output must equal the batch
   join of the inputs seen so far.  This pins every intermediate snapshot, so a
   bug that telescopes to the right final total but diverges mid-stream fails. *)
let prop_join_incremental_eq_batch =
  QCheck.Test.make
    ~count:400
    ~name:"incremental join == batch at every prefix"
    arb_steps
    (fun steps ->
       let j = J.create () in
       let ok, _, _, _ =
         List.fold_left
           (fun (ok, accl, accr, accout) (dl, dr) ->
              let out = J.step j ~left:dl ~right:dr in
              let accl = ZL.add accl dl
              and accr = ZR.add accr dr in
              let accout = ZO.add accout out in
              let expected = batch_join accl accr in
              ( ok && ZO.equal accout expected && ZO.equal (J.output j) expected
              , accl
              , accr
              , accout ))
           (true, ZL.zero, ZR.zero, ZO.zero)
           steps
       in
       ok)
;;

(* ---- Incremental grouped aggregates (COUNT, SUM) ---- *)

module Str = struct
  type t = string

  let compare = compare
  let pp = Format.pp_print_string
end

module SInt = struct
  type t = string * int

  let compare = compare
  let pp ppf (s, i) = Format.fprintf ppf "(%s,%d)" s i
end

(* Output relation: (group, aggregate-value) rows. *)
module ZG = Zset.Make (SInt)

(* COUNT input: bare group labels. *)
module ZIc = Zset.Make (Str)

(* SUM input: (group, value) rows. *)
module ZIs = Zset.Make (SInt)

module Cnt = Sqlocaml_ivm.Aggregate.Make (struct
    module In = ZIc
    module Out = ZG

    type group = string

    let compare_group = compare
    let group_of g = g
    let measure _ = 1
    let result g c = g, c
  end)

module Sm = Sqlocaml_ivm.Aggregate.Make (struct
    module In = ZIs
    module Out = ZG

    type group = string

    let compare_group = compare
    let group_of (g, _) = g
    let measure (_, v) = v
    let result g s = g, s
  end)

(* Reference batch aggregates: a group's row exists iff its total weight > 0. *)
let batch_count (z : ZIc.t) : ZG.t =
  let tbl = Hashtbl.create 8 in
  ZIc.iter
    (fun g w ->
       Hashtbl.replace
         tbl
         g
         (w
          +
          try Hashtbl.find tbl g with
          | Not_found -> 0))
    z;
  Hashtbl.fold
    (fun g c acc -> if c > 0 then ZG.add acc (ZG.singleton (g, c) 1) else acc)
    tbl
    ZG.zero
;;

let batch_sum (z : ZIs.t) : ZG.t =
  let mult = Hashtbl.create 8
  and sumv = Hashtbl.create 8 in
  ZIs.iter
    (fun (g, v) w ->
       Hashtbl.replace
         mult
         g
         (w
          +
          try Hashtbl.find mult g with
          | Not_found -> 0);
       Hashtbl.replace
         sumv
         g
         ((v * w)
          +
          try Hashtbl.find sumv g with
          | Not_found -> 0))
    z;
  Hashtbl.fold
    (fun g m acc ->
       if m > 0 then ZG.add acc (ZG.singleton (g, Hashtbl.find sumv g) 1) else acc)
    mult
    ZG.zero
;;

let test_count_basic () =
  let c = Cnt.create () in
  let _ = Cnt.step c (ZIc.of_list [ "a", 1; "a", 1; "b", 1 ]) in
  Alcotest.(check (list (pair (pair string int) int)))
    "count: a=2, b=1"
    [ ("a", 2), 1; ("b", 1), 1 ]
    (List.sort compare (ZG.to_list (Cnt.output c)))
;;

let test_count_group_disappears () =
  let c = Cnt.create () in
  let _ = Cnt.step c (ZIc.of_list [ "a", 1 ]) in
  let _ = Cnt.step c (ZIc.of_list [ "a", -1 ]) in
  Alcotest.(check bool) "group gone when count hits 0" true (ZG.is_zero (Cnt.output c))
;;

let arb_count_steps =
  QCheck.(list_size (Gen.int_range 0 6) (list (pair str_gen (int_range (-2) 2))))
  |> QCheck.map (List.map ZIc.of_list)
;;

let prop_count_incremental_eq_batch =
  QCheck.Test.make
    ~count:400
    ~name:"incremental COUNT == batch at every prefix"
    arb_count_steps
    (fun steps ->
       let c = Cnt.create () in
       let ok, _, _ =
         List.fold_left
           (fun (ok, seen, accout) d ->
              let out = Cnt.step c d in
              let seen = ZIc.add seen d in
              let accout = ZG.add accout out in
              let expected = batch_count seen in
              ( ok && ZG.equal accout expected && ZG.equal (Cnt.output c) expected
              , seen
              , accout ))
           (true, ZIc.zero, ZG.zero)
           steps
       in
       ok)
;;

let arb_sum_steps =
  QCheck.(
    list_size
      (Gen.int_range 0 6)
      (list (pair (pair str_gen (int_range (-3) 3)) (int_range (-2) 2))))
  |> QCheck.map (List.map ZIs.of_list)
;;

let prop_sum_incremental_eq_batch =
  QCheck.Test.make
    ~count:400
    ~name:"incremental SUM == batch at every prefix"
    arb_sum_steps
    (fun steps ->
       let s = Sm.create () in
       let ok, _, _ =
         List.fold_left
           (fun (ok, seen, accout) d ->
              let out = Sm.step s d in
              let seen = ZIs.add seen d in
              let accout = ZG.add accout out in
              let expected = batch_sum seen in
              ( ok && ZG.equal accout expected && ZG.equal (Sm.output s) expected
              , seen
              , accout ))
           (true, ZIs.zero, ZG.zero)
           steps
       in
       ok)
;;

(* ---- Linear operators (select/project) are delta-transparent: applying them
   per-delta then integrating equals applying them to the integrated input. ---- *)
let prop_filter_delta_transparent =
  QCheck.Test.make
    ~count:300
    ~name:"filter is linear: integrate(filter deltas) == filter(integrate)"
    QCheck.(list arb_side)
    (fun deltas ->
       let zs = List.map ZL.of_list deltas in
       let keep ((i, _) : int * string) = i mod 2 = 0 in
       let incremental =
         List.fold_left (fun a z -> ZL.add a (ZL.filter keep z)) ZL.zero zs
       in
       let batch = ZL.filter keep (List.fold_left ZL.add ZL.zero zs) in
       ZL.equal incremental batch)
;;

let () =
  Alcotest.run
    "ivm_operators_417"
    [ ( "join"
      , [ Alcotest.test_case "basic" `Quick test_join_basic
        ; Alcotest.test_case "late right" `Quick test_join_incremental_right_arrives_late
        ; Alcotest.test_case "retraction" `Quick test_join_retraction
        ; QCheck_alcotest.to_alcotest prop_join_incremental_eq_batch
        ] )
    ; ( "aggregate"
      , [ Alcotest.test_case "count basic" `Quick test_count_basic
        ; Alcotest.test_case "count group disappears" `Quick test_count_group_disappears
        ; QCheck_alcotest.to_alcotest prop_count_incremental_eq_batch
        ; QCheck_alcotest.to_alcotest prop_sum_incremental_eq_batch
        ] )
    ; "linear", [ QCheck_alcotest.to_alcotest prop_filter_delta_transparent ]
    ]
;;
