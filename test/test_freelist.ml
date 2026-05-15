(** Tests for Sqlocaml_storage.Freelist *)

module FL = Sqlocaml_storage.Freelist

(* ------------------------------------------------------------------ *)
(* Unit tests                                                          *)
(* ------------------------------------------------------------------ *)

(* empty has size = 0 *)
let test_empty_size () =
  Alcotest.(check int) "empty size = 0" 0 (FL.size FL.empty)

(* pop on empty returns None *)
let test_pop_empty () =
  Alcotest.(check bool) "pop empty = None" true
    (FL.pop FL.empty ~current_txn_id:1L = None)

(* add then pop with current_txn_id = freed_at_txn_id + 1 returns the page *)
let test_add_pop_reusable () =
  let freed_at = 5L in
  let current  = 6L in
  let t = FL.add FL.empty ~page_id:42l ~freed_at_txn_id:freed_at in
  match FL.pop t ~current_txn_id:current with
  | None -> Alcotest.fail "expected Some"
  | Some (pid, t') ->
    Alcotest.(check int32) "page_id" 42l pid;
    Alcotest.(check int)   "size after pop" 0 (FL.size t')

(* pop with current_txn_id = freed_at_txn_id returns None (equal, not yet reusable) *)
let test_pop_equal_not_reusable () =
  let txn = 5L in
  let t = FL.add FL.empty ~page_id:10l ~freed_at_txn_id:txn in
  Alcotest.(check bool) "pop with equal txn = None" true
    (FL.pop t ~current_txn_id:txn = None)

(* pop with current_txn_id < freed_at_txn_id returns None *)
let test_pop_too_early () =
  let t = FL.add FL.empty ~page_id:7l ~freed_at_txn_id:10L in
  Alcotest.(check bool) "pop with current < freed = None" true
    (FL.pop t ~current_txn_id:9L = None)

(* add 3 pages; pop 3 times succeeds; 4th pop returns None *)
let test_pop_three_pages () =
  let t =
    FL.add
      (FL.add
         (FL.add FL.empty ~page_id:1l ~freed_at_txn_id:1L)
         ~page_id:2l ~freed_at_txn_id:2L)
      ~page_id:3l ~freed_at_txn_id:3L
  in
  let current = 100L in
  (match FL.pop t ~current_txn_id:current with
   | None -> Alcotest.fail "pop 1: expected Some"
   | Some (_, t1) ->
     (match FL.pop t1 ~current_txn_id:current with
      | None -> Alcotest.fail "pop 2: expected Some"
      | Some (_, t2) ->
        (match FL.pop t2 ~current_txn_id:current with
         | None -> Alcotest.fail "pop 3: expected Some"
         | Some (_, t3) ->
           Alcotest.(check bool) "pop 4: None" true
             (FL.pop t3 ~current_txn_id:current = None))))

(* oldest-first: add page freed at txn 5 and page freed at txn 2;
   pop ~current_txn_id:3 returns the page freed at txn 2, not txn 5 *)
let test_pop_oldest_first () =
  let t =
    FL.add
      (FL.add FL.empty ~page_id:100l ~freed_at_txn_id:5L)
      ~page_id:200l ~freed_at_txn_id:2L
  in
  match FL.pop t ~current_txn_id:3L with
  | None -> Alcotest.fail "expected Some"
  | Some (pid, t') ->
    Alcotest.(check int32) "oldest page (freed at 2) returned" 200l pid;
    (* the page freed at txn 5 is not yet reusable at current_txn_id=3 *)
    Alcotest.(check int) "remaining size = 1" 1 (FL.size t');
    Alcotest.(check bool) "page freed at 5 not reusable yet" true
      (FL.pop t' ~current_txn_id:3L = None)

(* size after N adds equals N; decreases by 1 after each pop *)
let test_size_tracking () =
  let t0 = FL.empty in
  Alcotest.(check int) "size 0" 0 (FL.size t0);
  let t1 = FL.add t0 ~page_id:1l ~freed_at_txn_id:1L in
  Alcotest.(check int) "size 1" 1 (FL.size t1);
  let t2 = FL.add t1 ~page_id:2l ~freed_at_txn_id:2L in
  Alcotest.(check int) "size 2" 2 (FL.size t2);
  let t3 = FL.add t2 ~page_id:3l ~freed_at_txn_id:3L in
  Alcotest.(check int) "size 3" 3 (FL.size t3);
  (match FL.pop t3 ~current_txn_id:100L with
   | None -> Alcotest.fail "pop 1"
   | Some (_, t3') ->
     Alcotest.(check int) "size after pop 1" 2 (FL.size t3');
     (match FL.pop t3' ~current_txn_id:100L with
      | None -> Alcotest.fail "pop 2"
      | Some (_, t3'') ->
        Alcotest.(check int) "size after pop 2" 1 (FL.size t3'')))

(* to_list then of_list is idempotent *)
let test_to_of_list_idempotent () =
  let t =
    FL.add
      (FL.add
         (FL.add FL.empty ~page_id:10l ~freed_at_txn_id:1L)
         ~page_id:20l ~freed_at_txn_id:2L)
      ~page_id:30l ~freed_at_txn_id:3L
  in
  let t' = FL.of_list (FL.to_list t) in
  Alcotest.(check int) "same size" (FL.size t) (FL.size t');
  (* pop behavior should be identical *)
  (match FL.pop t ~current_txn_id:100L, FL.pop t' ~current_txn_id:100L with
   | None, None -> ()
   | Some (pid1, _), Some (pid2, _) ->
     Alcotest.(check int32) "same first pop page_id" pid1 pid2
   | _ -> Alcotest.fail "pop results differ")

(* reusable_count on empty = 0 *)
let test_reusable_count_empty () =
  Alcotest.(check int) "reusable_count empty = 0" 0
    (FL.reusable_count FL.empty ~current_txn_id:99L)

(* add 3 entries freed at txns 1, 2, 3; reusable_count ~current_txn_id:3 = 2 *)
let test_reusable_count_partial () =
  let t =
    FL.add
      (FL.add
         (FL.add FL.empty ~page_id:1l ~freed_at_txn_id:1L)
         ~page_id:2l ~freed_at_txn_id:2L)
      ~page_id:3l ~freed_at_txn_id:3L
  in
  (* txns 1 and 2 < 3; txn 3 is not < 3 *)
  Alcotest.(check int) "reusable_count = 2" 2
    (FL.reusable_count t ~current_txn_id:3L)

(* add then pop returns that exact page_id *)
let test_pop_returns_correct_page () =
  let t = FL.add FL.empty ~page_id:0xDEADBEEFl ~freed_at_txn_id:1L in
  match FL.pop t ~current_txn_id:2L with
  | None -> Alcotest.fail "expected Some"
  | Some (pid, _) ->
    Alcotest.(check int32) "exact page_id" 0xDEADBEEFl pid

(* ------------------------------------------------------------------ *)
(* QCheck property tests                                               *)
(* ------------------------------------------------------------------ *)

(* add N random pages; pop with current_txn_id = Int64.max_int N times returns
   N distinct page_ids; final pop returns None *)
let prop_pop_all_distinct =
  QCheck.Test.make
    ~name:"prop_pop_all_distinct"
    ~count:10_000
    QCheck.(list_size (Gen.int_range 0 20) (pair int32 int64))
    (fun entries ->
       (* Build freelist from entries, ensuring txn_ids are non-negative
          (use absolute value to avoid Int64.min_int issues) *)
       let entries =
         List.map (fun (pid, txn) ->
           (pid, if Int64.compare txn 0L < 0 then Int64.neg txn else txn))
           entries
       in
       let t = List.fold_left
         (fun acc (pid, txn) -> FL.add acc ~page_id:pid ~freed_at_txn_id:txn)
         FL.empty
         entries
       in
       let n = List.length entries in
       (* Pop all with max_int *)
       let rec pop_all acc t_cur count =
         match FL.pop t_cur ~current_txn_id:Int64.max_int with
         | None -> (List.length acc = count, t_cur)
         | Some (pid, t') -> pop_all (pid :: acc) t' count
       in
       let (all_popped, t_final) = pop_all [] t n in
       (* final pop must return None *)
       let final_none = FL.pop t_final ~current_txn_id:Int64.max_int = None in
       all_popped && final_none)

(* of_list (to_list t) produces identical pop results *)
let prop_roundtrip_pop_identical =
  QCheck.Test.make
    ~name:"prop_roundtrip_pop_identical"
    ~count:10_000
    QCheck.(list_size (Gen.int_range 0 10) (pair int32 int64))
    (fun entries ->
       let t = List.fold_left
         (fun acc (pid, txn) -> FL.add acc ~page_id:pid ~freed_at_txn_id:txn)
         FL.empty
         entries
       in
       let t' = FL.of_list (FL.to_list t) in
       (* Compare pop sequences *)
       let rec compare_pops t1 t2 =
         match FL.pop t1 ~current_txn_id:Int64.max_int,
               FL.pop t2 ~current_txn_id:Int64.max_int with
         | None, None -> true
         | Some (p1, t1'), Some (p2, t2') ->
           p1 = p2 && compare_pops t1' t2'
         | _ -> false
       in
       compare_pops t t')

(* pop result page_id is always a page_id that was added; never invents new ids *)
let prop_pop_only_added_ids =
  QCheck.Test.make
    ~name:"prop_pop_only_added_ids"
    ~count:10_000
    QCheck.(list_size (Gen.int_range 1 15) (pair int32 int64))
    (fun entries ->
       let page_ids = List.map fst entries in
       let t = List.fold_left
         (fun acc (pid, txn) -> FL.add acc ~page_id:pid ~freed_at_txn_id:txn)
         FL.empty
         entries
       in
       (* Pop all and verify each is in the original set *)
       let rec check t_cur =
         match FL.pop t_cur ~current_txn_id:Int64.max_int with
         | None -> true
         | Some (pid, t') ->
           List.mem pid page_ids && check t'
       in
       check t)

(* after add page freed_at T:
   pop ~current_txn_id:T returns None;
   pop ~current_txn_id:(T+1L) returns Some *)
let prop_txn_gating =
  (* Use non-negative txn_ids that won't overflow when +1 is applied *)
  let gen = QCheck.Gen.(
    let* pid = int32 in
    (* keep txn < Int64.max_int so T+1 doesn't overflow *)
    let* txn = map (fun x -> Int64.of_int (abs x mod 1_000_000_000)) int in
    return (pid, txn)
  ) in
  QCheck.Test.make
    ~name:"prop_txn_gating"
    ~count:10_000
    (QCheck.make gen)
    (fun (pid, txn) ->
       let t = FL.add FL.empty ~page_id:pid ~freed_at_txn_id:txn in
       let at_equal = FL.pop t ~current_txn_id:txn = None in
       let at_plus1 = FL.pop t ~current_txn_id:(Int64.add txn 1L) <> None in
       at_equal && at_plus1)

(* ------------------------------------------------------------------ *)
(* RUNNER                                                              *)
(* ------------------------------------------------------------------ *)

let () =
  let qcheck_tests =
    List.map QCheck_alcotest.to_alcotest [
      prop_pop_all_distinct;
      prop_roundtrip_pop_identical;
      prop_pop_only_added_ids;
      prop_txn_gating;
    ]
  in
  Alcotest.run "freelist" [
    "basic", [
      Alcotest.test_case "empty size = 0"                     `Quick test_empty_size;
      Alcotest.test_case "pop empty = None"                   `Quick test_pop_empty;
      Alcotest.test_case "add then pop reusable"              `Quick test_add_pop_reusable;
      Alcotest.test_case "pop equal txn = None"               `Quick test_pop_equal_not_reusable;
      Alcotest.test_case "pop too early = None"               `Quick test_pop_too_early;
      Alcotest.test_case "pop three pages"                    `Quick test_pop_three_pages;
      Alcotest.test_case "pop oldest first"                   `Quick test_pop_oldest_first;
      Alcotest.test_case "size tracking"                      `Quick test_size_tracking;
    ];
    "serialisation", [
      Alcotest.test_case "to_list / of_list idempotent"       `Quick test_to_of_list_idempotent;
    ];
    "reusable_count", [
      Alcotest.test_case "reusable_count empty = 0"           `Quick test_reusable_count_empty;
      Alcotest.test_case "reusable_count partial"             `Quick test_reusable_count_partial;
    ];
    "correctness", [
      Alcotest.test_case "pop returns correct page_id"        `Quick test_pop_returns_correct_page;
    ];
    "qcheck", qcheck_tests;
  ]
