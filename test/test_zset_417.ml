(** #417 Phase 1: the Z-set core (DBSP).  A Z-set is a finitely-supported map
    from elements to signed integer weights — the data structure the incremental
    operators (Phase 2) compute over.  +1 = inserted, -1 = retracted; the delta
    feed (#419) lifts to a Z-set delta. *)

module Z = Sqlocaml_ivm.Zset.Make (struct
    type t = int

    let compare = Int.compare
    let pp = Format.pp_print_int
  end)

(* A Z-set is canonical: equal iff same (elt, nonzero-weight) entries.  Use a
   sorted [to_list] for deterministic comparison in tests. *)
let zlist z = List.sort compare (Z.to_list z)

let check_zlist msg expected z =
  Alcotest.(check (list (pair int int))) msg (List.sort compare expected) (zlist z)
;;

let test_zero () =
  Alcotest.(check bool) "zero is_zero" true (Z.is_zero Z.zero);
  Alcotest.(check int) "weight of absent is 0" 0 (Z.weight Z.zero 7);
  check_zlist "zero has empty support" [] Z.zero
;;

let test_of_list_coalesces () =
  (* repeated elements sum; resulting zero-weight entries are dropped *)
  let z = Z.of_list [ 1, 2; 2, 1; 1, -2; 3, 5 ] in
  check_zlist "of_list coalesces and drops zeros" [ 2, 1; 3, 5 ] z;
  Alcotest.(check int) "weight 1 cancelled" 0 (Z.weight z 1);
  Alcotest.(check int) "weight 3" 5 (Z.weight z 3)
;;

let test_add_cancels () =
  let a = Z.of_list [ 1, 1; 2, 1 ] in
  let b = Z.of_list [ 1, -1; 3, 4 ] in
  check_zlist "add sums pointwise, drops zero" [ 2, 1; 3, 4 ] (Z.add a b)
;;

let test_negate_sub () =
  let a = Z.of_list [ 1, 2; 2, -3 ] in
  check_zlist "negate flips weights" [ 1, -2; 2, 3 ] (Z.negate a);
  Alcotest.(check bool) "a - a = zero" true (Z.is_zero (Z.sub a a));
  check_zlist "a - zero = a" [ 1, 2; 2, -3 ] (Z.sub a Z.zero)
;;

let test_scale () =
  let a = Z.of_list [ 1, 2; 2, -1 ] in
  check_zlist "scale 3" [ 1, 6; 2, -3 ] (Z.scale 3 a);
  Alcotest.(check bool) "scale 0 = zero" true (Z.is_zero (Z.scale 0 a));
  check_zlist "scale 1 = id" [ 1, 2; 2, -1 ] (Z.scale 1 a)
;;

let test_filter () =
  let a = Z.of_list [ 1, 1; 2, 2; 3, 3; 4, 4 ] in
  check_zlist
    "filter even keeps weights"
    [ 2, 2; 4, 4 ]
    (Z.filter (fun x -> x mod 2 = 0) a)
;;

let test_map_collisions () =
  (* mapping many elements onto one key sums their weights *)
  let a = Z.of_list [ 1, 1; 2, 1; 3, 1; 4, 1 ] in
  check_zlist "map x->x mod 2 sums collisions" [ 0, 2; 1, 2 ] (Z.map (fun x -> x mod 2) a);
  (* a map that cancels to zero drops the entry *)
  let b = Z.of_list [ 1, 1; 2, -1 ] in
  check_zlist "map collision cancels to zero" [] (Z.map (fun _ -> 0) b)
;;

let test_distinct () =
  (* DBSP distinct: weight > 0 -> 1, else absent (negatives/zero dropped) *)
  let a = Z.of_list [ 1, 3; 2, 1; 3, -2 ] in
  check_zlist "distinct: positives -> 1, negatives dropped" [ 1, 1; 2, 1 ] (Z.distinct a);
  Alcotest.(check bool) "distinct of zero is zero" true (Z.is_zero (Z.distinct Z.zero))
;;

let test_aggregates () =
  let a = Z.of_list [ 1, 2; 2, -3; 3, 1 ] in
  Alcotest.(check int) "cardinality (distinct nonzero elts)" 3 (Z.cardinality a);
  Alcotest.(check int) "total_weight" 0 (Z.total_weight a);
  Alcotest.(check (list int))
    "support sorted"
    [ 1; 2; 3 ]
    (List.sort compare (Z.support a))
;;

let test_equal_canonical () =
  let a = Z.of_list [ 1, 1; 2, 0; 3, 2 ] in
  let b = Z.of_list [ 3, 2; 1, 1 ] in
  Alcotest.(check bool) "equal ignores zero-weight entry" true (Z.equal a b);
  Alcotest.(check bool) "unequal weights" false (Z.equal a (Z.of_list [ 1, 1; 3, 3 ]))
;;

let test_pp () =
  let s = Format.asprintf "%a" Z.pp (Z.of_list [ 1, 2 ]) in
  Alcotest.(check bool) "pp mentions element and weight" true (String.length s > 0)
;;

(* ---- QCheck properties ---- *)

let arb_zset =
  QCheck.(list (pair (int_range (-5) 5) (int_range (-4) 4)))
  |> QCheck.map (fun l -> Z.of_list l)
;;

let prop_add_comm =
  QCheck.Test.make
    ~count:200
    ~name:"add is commutative"
    QCheck.(pair arb_zset arb_zset)
    (fun (a, b) -> Z.equal (Z.add a b) (Z.add b a))
;;

let prop_add_assoc =
  QCheck.Test.make
    ~count:200
    ~name:"add is associative"
    QCheck.(triple arb_zset arb_zset arb_zset)
    (fun (a, b, c) -> Z.equal (Z.add (Z.add a b) c) (Z.add a (Z.add b c)))
;;

let prop_add_zero_id =
  QCheck.Test.make ~count:200 ~name:"add zero is identity" arb_zset (fun a ->
    Z.equal (Z.add a Z.zero) a)
;;

let prop_negate_inverse =
  QCheck.Test.make ~count:200 ~name:"a + (-a) = 0" arb_zset (fun a ->
    Z.is_zero (Z.add a (Z.negate a)))
;;

let prop_distinct_idempotent =
  QCheck.Test.make ~count:200 ~name:"distinct is idempotent" arb_zset (fun a ->
    Z.equal (Z.distinct a) (Z.distinct (Z.distinct a)))
;;

let prop_distinct_nonneg =
  QCheck.Test.make ~count:200 ~name:"distinct weights are all 1" arb_zset (fun a ->
    List.for_all (fun (_, w) -> w = 1) (Z.to_list (Z.distinct a)))
;;

let no_zero z = List.for_all (fun (_, w) -> w <> 0) (Z.to_list z)

let prop_no_zero_entries =
  QCheck.Test.make
    ~count:200
    ~name:"canonical: no zero-weight entries stored"
    arb_zset
    (fun a -> no_zero a)
;;

(* The ops that build maps directly (bypassing [add_weight]) must each preserve
   the no-zero-weight invariant by construction; check their outputs, not just
   [of_list]'s. *)
let prop_ops_preserve_canonical =
  QCheck.Test.make
    ~count:200
    ~name:"direct-build operators preserve canonical form"
    QCheck.(pair (int_range (-3) 3) arb_zset)
    (fun (n, a) ->
       no_zero (Z.negate a)
       && no_zero (Z.scale n a)
       && no_zero (Z.map (fun x -> x mod 3) a)
       && no_zero (Z.filter (fun x -> x > 0) a)
       && no_zero (Z.distinct a)
       && no_zero (Z.sub a a))
;;

let prop_scale_distributes =
  QCheck.Test.make
    ~count:200
    ~name:"scale distributes over add"
    QCheck.(pair (int_range (-3) 3) (pair arb_zset arb_zset))
    (fun (n, (a, b)) ->
       Z.equal (Z.scale n (Z.add a b)) (Z.add (Z.scale n a) (Z.scale n b)))
;;

let () =
  Alcotest.run
    "zset_417"
    [ ( "core"
      , [ Alcotest.test_case "zero" `Quick test_zero
        ; Alcotest.test_case "of_list coalesces" `Quick test_of_list_coalesces
        ; Alcotest.test_case "add cancels" `Quick test_add_cancels
        ; Alcotest.test_case "negate/sub" `Quick test_negate_sub
        ; Alcotest.test_case "scale" `Quick test_scale
        ; Alcotest.test_case "filter" `Quick test_filter
        ; Alcotest.test_case "map collisions" `Quick test_map_collisions
        ; Alcotest.test_case "distinct" `Quick test_distinct
        ; Alcotest.test_case "aggregates" `Quick test_aggregates
        ; Alcotest.test_case "equal canonical" `Quick test_equal_canonical
        ; Alcotest.test_case "pp" `Quick test_pp
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_add_comm
          ; prop_add_assoc
          ; prop_add_zero_id
          ; prop_negate_inverse
          ; prop_distinct_idempotent
          ; prop_distinct_nonneg
          ; prop_no_zero_entries
          ; prop_ops_preserve_canonical
          ; prop_scale_distributes
          ] )
    ]
;;
