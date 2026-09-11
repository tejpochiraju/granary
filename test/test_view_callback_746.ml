(** #746: per-callback removal for reactive views, and an O(1) registration.

    Two gaps, both found auditing hook hot-reload downstream:

    - [register_view_callback] appended and nothing ever removed. #469 gave a
      reactive {e view} a removal path ([DROP REACTIVE VIEW]); a single
      {e callback} still had none, so a caller re-wiring callbacks from data
      leaked a dead closure per re-wire that was still invoked on every change.
    - the append was [rv_callbacks <- rv_callbacks @ [ cb ]], copying the whole
      list per registration, so [n] registrations cost O(n^2).

    What is pinned here:

    - the handle round-trip: register, fire, unregister, stop firing, and the
      other callbacks on the same view keep firing;
    - [unregister_view_callback] is idempotent and, since #766, names WHICH of
      the non-removal cases it hit: [`Unknown_view] for a handle whose view was
      dropped, [`Not_registered] for one already removed or minted on another
      [Db.t]. (The fourth case, [`Stale_generation], and the
      [?expected_generation] registration argument are pinned in
      [test_view_callback_identity_766.ml].)
    - callbacks fire in {e registration order} — contractual since #746, and
      the thing the O(1) prepend must not quietly reverse;
    - the mid-flush semantics: the callback set is snapshotted per notification
      batch, so a callback may unregister itself (or a sibling) from inside its
      own invocation without disturbing the batch it is in, and is silent from
      the next batch on;
    - the allocation slope of [n] registrations. This is an {e allocation} gate,
      not a wall-clock one: [Gc.minor_words] over a fixed code path is
      deterministic, so a loaded runner cannot move it. Linear growth doubles
      the words when [n] doubles; the quadratic append quadrupled them
      (measured on this box before the fix: 376 750 words at n=500 rising to
      96 028 000 at n=8000 — 4.00x per doubling, every step). The ceiling of
      2.5 sits between those two integers rather than being tuned; after the
      fix the measured cost is a flat 13 words per registration, i.e. exactly
      2.00x per doubling. [GRANARY_MEM_MAX_CALLBACK_SLOPE] raises it. *)

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

(* #757: [register_view_callback]'s [Ok] case is now [handle * generation];
   this file only ever needs the handle. *)
let attach db ~view_name cb =
  match Db.register_view_callback db ~view_name cb with
  | Ok (h, (_ : int)) -> h
  | Error (`Unknown_view v) -> Alcotest.failf "expected %S to be a live view" v
  | Error (`Stale_generation g) ->
    (* #766 widened the error type, but [`Stale_generation] is reachable only
       when [?expected_generation] is supplied, which this file never does. *)
    Alcotest.failf "unexpected `Stale_generation %d from a plain registration" g
;;

(* #766: [unregister_view_callback] answers a four-way variant rather than a
   [bool].  Rendering it keeps the failure messages informative — [check bool]
   could only ever say "expected true, got false", which is exactly the
   conflation #766 removed. *)
let unreg db h =
  match Db.unregister_view_callback db h with
  | `Removed -> "`Removed"
  | `Not_registered -> "`Not_registered"
  | `Stale_generation g -> Printf.sprintf "`Stale_generation %d" g
  | `Unknown_view v -> Printf.sprintf "`Unknown_view %S" v
;;

(* A one-column-group COUNT view over [t]: every INSERT moves it, so every
   INSERT is one notification batch. *)
let setup db =
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
  exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp"
;;

let counter () =
  let n = ref 0 in
  ( n
  , fun _ ->
      incr n;
      Lwt.return_unit )
;;

(* ------------------------------------------------------------------ *)

let test_unregister_detaches_one_callback () =
  with_db (fun db ->
    setup db;
    let a, cb_a = counter () in
    let b, cb_b = counter () in
    let ha = attach db ~view_name:"cnt" cb_a in
    let _hb = attach db ~view_name:"cnt" cb_b in
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check (pair int int)) "both fire while registered" (1, 1) (!a, !b);
    Alcotest.(check string)
      "unregistering a live callback reports it was removed"
      "`Removed"
      (unreg db ha);
    exec db "INSERT INTO t VALUES (2, 'b', 20)";
    Alcotest.(check (pair int int))
      "the detached one is silent; its sibling is untouched"
      (1, 2)
      (!a, !b))
;;

let test_unregister_is_idempotent () =
  with_db (fun db ->
    setup db;
    let _, cb = counter () in
    let h = attach db ~view_name:"cnt" cb in
    Alcotest.(check string) "first removal" "`Removed" (unreg db h);
    Alcotest.(check string)
      "second removal answers `Not_registered rather than raising"
      "`Not_registered"
      (unreg db h))
;;

let test_unregister_after_drop_and_across_handles () =
  with_db (fun db ->
    setup db;
    let n, cb = counter () in
    let h = attach db ~view_name:"cnt" cb in
    exec db "DROP REACTIVE VIEW cnt";
    Alcotest.(check string)
      "a dropped view took its callbacks with it (#469), so there is nothing to remove"
      "`Unknown_view \"cnt\""
      (unreg db h);
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "and it does not fire" 0 !n);
  (* A handle minted on one [Db.t] and presented to another matches nothing:
     ids come from one process-global counter, so it cannot collide with a
     callback the second handle registered under the same view name. *)
  with_db (fun db1 ->
    setup db1;
    let n1, cb1 = counter () in
    let h1 = attach db1 ~view_name:"cnt" cb1 in
    with_db (fun db2 ->
      setup db2;
      let n2, cb2 = counter () in
      let _ = attach db2 ~view_name:"cnt" cb2 in
      (* #766: [`Not_registered], not [`Unknown_view] — [cnt] IS live on [db2],
         and [db2]'s own store minted it the same generation [db1]'s did (each
         counter starts fresh per store), so the handle is not stale either.  It
         is simply not in this registry's list, because ids are process-global
         and [h1]'s belongs to [db1]. *)
      Alcotest.(check string)
        "a foreign handle removes nothing"
        "`Not_registered"
        (unreg db2 h1);
      exec db2 "INSERT INTO t VALUES (1, 'a', 10)";
      Alcotest.(check int) "db2's own callback still fires" 1 !n2);
    exec db1 "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "and db1's was never touched" 1 !n1)
;;

(* #746: registration order is contractual.  The list is held newest-first for
   the O(1) prepend and reversed at the firing site; if that reverse is ever
   dropped this is what catches it. *)
let test_callbacks_fire_in_registration_order () =
  with_db (fun db ->
    setup db;
    let log = ref [] in
    let mk tag _ =
      log := tag :: !log;
      Lwt.return_unit
    in
    List.iter (fun tag -> ignore (attach db ~view_name:"cnt" (mk tag))) [ "a"; "b"; "c" ];
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check (list string))
      "invoked in the order they were registered"
      [ "a"; "b"; "c" ]
      (List.rev !log))
;;

(* The subtle case the issue flags: [rv_flush_inner] reads [rv_callbacks] while
   a callback is running, so removal mid-flush needed a decided semantics.  It
   is a per-batch snapshot — see the [unregister_view_callback] doc comment. *)
let test_self_unregister_mid_flush () =
  with_db (fun db ->
    setup db;
    let fired = ref 0 in
    let slot = ref None in
    let removed = ref None in
    let cb _ =
      incr fired;
      (match !slot with
       | Some h -> removed := Some (unreg db h)
       | None -> Alcotest.fail "handle not stored before the first fire");
      Lwt.return_unit
    in
    slot := Some (attach db ~view_name:"cnt" cb);
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "the callback ran the invocation it was in" 1 !fired;
    Alcotest.(check (option string))
      "removing itself from inside its own invocation is accepted"
      (Some "`Removed")
      !removed;
    exec db "INSERT INTO t VALUES (2, 'b', 20)";
    Alcotest.(check int) "and it is silent from the next batch on" 1 !fired)
;;

(* The other half of the snapshot rule: a callback removed by an EARLIER
   callback in the same batch still runs for that batch.  A live read of
   [rv_callbacks] would skip it, which is the undefined behaviour #746 replaces
   — and a tombstone-at-once design would too, so this pins the choice rather
   than just the absence of a crash. *)
let test_removal_by_a_sibling_takes_effect_next_batch () =
  with_db (fun db ->
    setup db;
    let victim_fired = ref 0 in
    let slot = ref None in
    let killer _ =
      (match !slot with
       | Some h -> ignore (Db.unregister_view_callback db h)
       | None -> Alcotest.fail "victim handle not stored");
      Lwt.return_unit
    in
    ignore (attach db ~view_name:"cnt" killer);
    slot
    := Some
         (attach db ~view_name:"cnt" (fun _ ->
            incr victim_fired;
            Lwt.return_unit));
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int)
      "the victim still runs for the batch that was already snapshotted"
      1
      !victim_fired;
    exec db "INSERT INTO t VALUES (2, 'b', 20)";
    Alcotest.(check int) "and not for the one after it" 1 !victim_fired)
;;

(* A registration made from inside a callback obeys the same rule: it starts
   firing from the next batch, and — critically — does not turn the batch it
   was made in into an unbounded loop. *)
let test_registration_mid_flush_starts_next_batch () =
  with_db (fun db ->
    setup db;
    let late, late_cb = counter () in
    let once = ref false in
    let register_late () =
      once := true;
      ignore (attach db ~view_name:"cnt" late_cb)
    in
    let cb _ =
      if not !once then register_late ();
      Lwt.return_unit
    in
    ignore (attach db ~view_name:"cnt" cb);
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "the new callback does not fire for the batch it joined" 0 !late;
    exec db "INSERT INTO t VALUES (2, 'b', 20)";
    Alcotest.(check int) "it fires from the next one" 1 !late)
;;

(* ---------------------------- allocation gate ---------------------------- *)

let words_to_register db n =
  let cb _ = Lwt.return_unit in
  Gc.full_major ();
  let before = Gc.minor_words () in
  for _ = 1 to n do
    ignore (attach db ~view_name:"cnt" cb)
  done;
  Gc.minor_words () -. before
;;

let max_slope () =
  match Sys.getenv_opt "GRANARY_MEM_MAX_CALLBACK_SLOPE" with
  | Some s ->
    (match float_of_string_opt s with
     | Some f -> f
     | None -> 2.5)
  | None -> 2.5
;;

let test_registration_allocation_is_linear () =
  with_db (fun db ->
    setup db;
    let n = 2000 in
    let w1 = words_to_register db n in
    (* Registering into a registry that already holds [n] callbacks is where the
       quadratic append hurt most, so the second sample deliberately continues
       on the same view rather than starting from empty. *)
    let w2 = words_to_register db (2 * n) in
    let slope = w2 /. w1 in
    Printf.printf
      "\n\
       #746 registration allocation: n=%d %.0f words, n=%d %.0f words, slope %.2f \
       (linear 2.0, quadratic 4.0)\n\
       %!"
      n
      w1
      (2 * n)
      w2
      slope;
    Alcotest.(check bool)
      (Printf.sprintf
         "doubling n must not more than double the words allocated (slope %.2f)"
         slope)
      true
      (slope <= max_slope ()))
;;

let () =
  Alcotest.run
    "reactive view callback registry (#746)"
    [ ( "removal"
      , [ Alcotest.test_case
            "unregister detaches one"
            `Quick
            test_unregister_detaches_one_callback
        ; Alcotest.test_case "idempotent" `Quick test_unregister_is_idempotent
        ; Alcotest.test_case
            "dropped view / foreign handle"
            `Quick
            test_unregister_after_drop_and_across_handles
        ] )
    ; ( "ordering"
      , [ Alcotest.test_case
            "registration order"
            `Quick
            test_callbacks_fire_in_registration_order
        ] )
    ; ( "mid-flush"
      , [ Alcotest.test_case "self-unregister" `Quick test_self_unregister_mid_flush
        ; Alcotest.test_case
            "sibling removal defers to next batch"
            `Quick
            test_removal_by_a_sibling_takes_effect_next_batch
        ; Alcotest.test_case
            "registration defers to next batch"
            `Quick
            test_registration_mid_flush_starts_next_batch
        ] )
    ; ( "cost"
      , [ Alcotest.test_case
            "registration allocation is linear"
            `Quick
            test_registration_allocation_is_linear
        ] )
    ]
;;
