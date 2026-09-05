(** #757: a reactive view's callback registry answers liveness ("is a view
    named [x] live right now?") but not identity ("is this the SAME view I
    registered on?"). After

    {[
      DROP REACTIVE VIEW v;
      CREATE REACTIVE VIEW v AS ...;
    ]}

    every liveness query ({!Db.reactive_view_names}, {!Db.is_reactive_view})
    still answers "[v] is live", even though the entry underneath the name is
    a brand-new [rv_entry] with an empty callback list. A caller holding a
    handle registered against the old [v] has no signal that it should
    re-register.

    [Db.reactive_view_generation] closes that gap: it is minted from a
    store-wide, never-reset counter every time a name gets a new registry
    entry, so two different incarnations of the same name never compare equal
    even though every liveness query says "alive" throughout.

    Pinned here:

    - [None] for a name that was never created;
    - a fresh [CREATE REACTIVE VIEW] gives [Some g];
    - a real drop-and-recreate through SQL gives a DIFFERENT generation than
      before the drop;
    - a callback registered on the old generation does not receive
      notifications from the new view's activity — proving a caller genuinely
      needs to re-register, not just that the number changed;
    - repeated drop/recreate cycles never repeat a generation for that name;
    - [register_view_callback]'s returned generation matches what
      [reactive_view_generation] reports immediately after;
    - a QCheck property: for any sequence of create/drop operations on one
      view name interspersed with creates/drops of OTHER names, the sequence
      of generations observed for the target name is strictly increasing;
    - {b review finding, fixed}: a worker handle produced by
      [Db.create_worker_handle] over the SAME store reconstructs its own copy
      of a view's registry entry independently ([Db.of_store]'s [rv_load]
      runs again), which used to mint a BRAND NEW generation for a view the
      worker never dropped or recreated — falsely signalling churn that never
      happened. The generation is now minted from a counter and a
      name→generation map that live on the underlying store
      ([Granary_store.Store.rv_next_generation] /
      [Granary_store.Store.rv_generations]), not on [Db.t], so a worker
      handle created after the parent already has a view live reuses the
      SAME generation the parent recorded. Pinned below by creating the view
      on a parent handle, then deriving a worker and checking both
      [reactive_view_generation] and [register_view_callback]'s returned
      generation agree with the parent's.

    A companion fault-injection test for the OTHER review finding — a failed
    [CREATE REACTIVE VIEW] used to leave a ghost registry entry with a live
    generation when the durable [persist_reactive_view] write failed — lives
    in [test_reactive_view_catalog_error_476.ml], which already has the
    store-fault-injection harness this needs. *)

module Db = Granary.Db

let run = Lwt_main.run

let with_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let create_view db name =
  exec
    db
    (Printf.sprintf
       "CREATE REACTIVE VIEW %s AS SELECT grp, COUNT(*) FROM t GROUP BY grp"
       name)
;;

let drop_view db name = exec db (Printf.sprintf "DROP REACTIVE VIEW %s" name)

let attach db ~view_name cb =
  match Db.register_view_callback db ~view_name cb with
  | Ok (h, g) -> h, g
  | Error (`Unknown_view v) -> Alcotest.failf "expected %S to be a live view" v
;;

(* ------------------------------------------------------------------ *)

let generation_is_none_for_a_name_never_created () =
  with_db (fun db ->
    Alcotest.(check (option int))
      "never-created name has no generation"
      None
      (Db.reactive_view_generation db "never_seen"))
;;

let fresh_create_gives_some_generation () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT)";
    create_view db "v";
    match Db.reactive_view_generation db "v" with
    | Some _ -> ()
    | None -> Alcotest.fail "a freshly created view must report a generation")
;;

(* The actual bug repro: liveness says "v is alive" on both sides of the
   drop-and-recreate, but identity must differ. *)
let drop_and_recreate_gives_a_different_generation () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT)";
    create_view db "v";
    let g1 =
      match Db.reactive_view_generation db "v" with
      | Some g -> g
      | None -> Alcotest.fail "expected v to be live before the drop"
    in
    drop_view db "v";
    create_view db "v";
    let g2 =
      match Db.reactive_view_generation db "v" with
      | Some g -> g
      | None -> Alcotest.fail "expected v to be live after the recreate"
    in
    Alcotest.(check bool)
      "liveness says alive throughout (sanity: this is the bug's own premise)"
      true
      (Db.is_reactive_view db "v");
    Alcotest.(check bool) "the two incarnations have distinct generations" true (g1 <> g2))
;;

(* Proves the caller genuinely NEEDS to re-register: a callback captured
   against the old incarnation never sees the new incarnation's activity, even
   though it is sitting on a name that [is_reactive_view] still calls live. *)
let stale_callback_does_not_receive_new_view_updates () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT)";
    create_view db "v";
    let old_fired = ref 0 in
    let _h_old, g_old =
      attach db ~view_name:"v" (fun _ ->
        incr old_fired;
        Lwt.return_unit)
    in
    drop_view db "v";
    create_view db "v";
    let g_new =
      match Db.reactive_view_generation db "v" with
      | Some g -> g
      | None -> Alcotest.fail "expected v to be live after the recreate"
    in
    Alcotest.(check bool) "the captured generation is now stale" true (g_old <> g_new);
    let new_fired = ref 0 in
    let _h_new, _g_new2 =
      attach db ~view_name:"v" (fun _ ->
        incr new_fired;
        Lwt.return_unit)
    in
    exec db "INSERT INTO t VALUES (1, 'a')";
    Alcotest.(check int) "the stale callback never fires again" 0 !old_fired;
    Alcotest.(check int) "the freshly re-registered callback does fire" 1 !new_fired)
;;

let repeated_cycles_never_repeat_a_generation () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT)";
    let seen = ref [] in
    for _ = 1 to 20 do
      create_view db "v";
      (match Db.reactive_view_generation db "v" with
       | Some g -> seen := g :: !seen
       | None -> Alcotest.fail "expected v to be live right after creating it");
      drop_view db "v"
    done;
    let gens = !seen in
    let uniq = List.sort_uniq compare gens in
    Alcotest.(check int)
      "20 drop/recreate cycles produce 20 distinct generations"
      (List.length gens)
      (List.length uniq))
;;

let register_returns_the_generation_the_accessor_reports () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT)";
    create_view db "v";
    let _h, g_returned = attach db ~view_name:"v" (fun _ -> Lwt.return_unit) in
    match Db.reactive_view_generation db "v" with
    | Some g_accessor ->
      Alcotest.(check int)
        "register_view_callback's generation matches the accessor immediately after"
        g_accessor
        g_returned
    | None -> Alcotest.fail "v must still be live immediately after registering")
;;

(* Review finding on PR #761: [Db.create_worker_handle] derives a second
   [Db.t] over the SAME underlying store, and that derivation reconstructs
   the reactive-view registry independently ([Db.of_store]'s [rv_load] runs
   again for the worker). Before the fix, [rv_load] always minted a NEW
   generation via a [Db.ml]-local counter, so a worker handle created after
   the parent already had view [v] live would report a DIFFERENT generation
   for [v] than the parent — a false "dropped and recreated" signal for a
   view that was never touched. The fix moved the counter (and a
   name -> current-generation map) onto the underlying store, shared by
   construction between every handle over it, so a worker's independently
   rebuilt registry entry for an unchanged view agrees with every sibling's. *)
let worker_handle_over_the_same_store_reuses_the_parents_generation () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT)";
    create_view db "v";
    let g_parent =
      match Db.reactive_view_generation db "v" with
      | Some g -> g
      | None -> Alcotest.fail "v should be live on the parent handle"
    in
    let worker = run (Db.create_worker_handle db) in
    Fun.protect
      ~finally:(fun () ->
        try run (Db.close worker) with
        | _ -> ())
      (fun () ->
         (match Db.reactive_view_generation worker "v" with
          | Some g_worker ->
            Alcotest.(check int)
              "a worker handle spawned after v's CREATE reuses the parent's generation \
               rather than minting a fresh one from its own independent rv_load"
              g_parent
              g_worker
          | None -> Alcotest.fail "v should be visible to the worker handle too");
         let _h, g_registered = attach worker ~view_name:"v" (fun _ -> Lwt.return_unit) in
         Alcotest.(check int)
           "register_view_callback on the worker reports that same shared generation"
           g_parent
           g_registered))
;;

(* ------------------------------------------------------------------ *)
(* QCheck property: interleaved churn on the target name and on OTHER names
   must never let two incarnations of the target collide, and the target's own
   generation sequence must be strictly increasing across its lifetime. *)

type op =
  | Toggle_target
  | Toggle_other of int (* index into a small fixed pool of other names *)

let other_pool_size = 3
let other_name i = Printf.sprintf "other%d" i

let op_gen =
  QCheck.Gen.(
    oneof_weighted
      [ 2, return Toggle_target
      ; 3, map (fun i -> Toggle_other i) (int_bound (other_pool_size - 1))
      ])
;;

let ops_gen = QCheck.Gen.(list_size (int_range 1 40) op_gen)

let ops_arbitrary =
  QCheck.make ops_gen ~print:(fun ops ->
    String.concat
      ";"
      (List.map
         (function
           | Toggle_target -> "target"
           | Toggle_other i -> Printf.sprintf "other%d" i)
         ops))
;;

let target_generation_sequence_is_strictly_increasing =
  QCheck.Test.make
    ~count:200
    ~name:
      "interleaved create/drop churn on other names never disturbs the target's \
       strictly-increasing generation sequence"
    ops_arbitrary
    (fun ops ->
       with_db (fun db ->
         exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT)";
         let target_live = ref false in
         let others_live = Array.make other_pool_size false in
         let observed = ref [] in
         List.iter
           (fun op ->
              match op with
              | Toggle_target ->
                if !target_live
                then (
                  drop_view db "target";
                  target_live := false)
                else (
                  create_view db "target";
                  target_live := true;
                  match Db.reactive_view_generation db "target" with
                  | Some g -> observed := g :: !observed
                  | None -> Alcotest.fail "target must be live right after creating it")
              | Toggle_other i ->
                let name = other_name i in
                if others_live.(i)
                then (
                  drop_view db name;
                  others_live.(i) <- false)
                else (
                  create_view db name;
                  others_live.(i) <- true))
           ops;
         let gens = List.rev !observed in
         let rec strictly_increasing = function
           | [] | [ _ ] -> true
           | a :: (b :: _ as rest) -> a < b && strictly_increasing rest
         in
         strictly_increasing gens))
;;

let () =
  Alcotest.run
    "reactive view generation identity (#757)"
    [ ( "basics"
      , [ Alcotest.test_case
            "None for a name never created"
            `Quick
            generation_is_none_for_a_name_never_created
        ; Alcotest.test_case
            "Some for a fresh create"
            `Quick
            fresh_create_gives_some_generation
        ; Alcotest.test_case
            "drop+recreate differs"
            `Quick
            drop_and_recreate_gives_a_different_generation
        ; Alcotest.test_case
            "register's returned generation matches the accessor"
            `Quick
            register_returns_the_generation_the_accessor_reports
        ] )
    ; ( "the caller genuinely needs to re-register"
      , [ Alcotest.test_case
            "stale callback is silent on the new incarnation"
            `Quick
            stale_callback_does_not_receive_new_view_updates
        ] )
    ; ( "monotonicity"
      , [ Alcotest.test_case
            "repeated cycles never repeat a generation"
            `Quick
            repeated_cycles_never_repeat_a_generation
        ; QCheck_alcotest.to_alcotest target_generation_sequence_is_strictly_increasing
        ] )
    ; ( "shared across sibling handles over one store"
      , [ Alcotest.test_case
            "a worker handle reuses the parent's generation"
            `Quick
            worker_handle_over_the_same_store_reuses_the_parents_generation
        ] )
    ]
;;
