(** Tests for replication reader gating in the Store. *)

open Lwt.Syntax
module Store = Granary_store.Store

(* ------------------------------------------------------------------ *)
(* Bounded yield helper: polls f up to max_pauses times, yielding each *)
(* iteration.  Replace fixed-count Lwt.pause() chains for robustness.  *)
(* ------------------------------------------------------------------ *)
let rec wait_for f max_pauses =
  if max_pauses <= 0
  then Lwt.return_unit
  else if f ()
  then Lwt.return_unit
  else
    let* () = Lwt.pause () in
    wait_for f (max_pauses - 1)
;;

(* ------------------------------------------------------------------ *)
(* WAL-backed in-memory store helpers                                  *)
(* ------------------------------------------------------------------ *)

type dev = { mutable buf : Bytes.t }

let mk_dev size = { buf = Bytes.make size '\x00' }

let dev_grow d need =
  let cur = Bytes.length d.buf in
  if need > cur
  then (
    let nb = Bytes.make (max need (cur * 2)) '\x00' in
    Bytes.blit d.buf 0 nb 0 cur;
    d.buf <- nb)
;;

let read_at d ~offset out =
  let off = Int64.to_int offset in
  let len = Cstruct.length out in
  if off + len > Bytes.length d.buf
  then Lwt.return (Error "read past EOF")
  else (
    Cstruct.blit_from_bytes d.buf off out 0 len;
    Lwt.return (Ok ()))
;;

let write_at d ~offset src =
  let off = Int64.to_int offset in
  let len = Cstruct.length src in
  dev_grow d (off + len);
  Cstruct.blit_to_bytes src 0 d.buf off len;
  Lwt.return (Ok ())
;;

let sync_ok () = Lwt.return (Ok ())

(* ------------------------------------------------------------------ *)
(* Open a WAL store in memory                                          *)
(* ------------------------------------------------------------------ *)

let open_test_store ?(wal_sync = sync_ok) () =
  let main_dev = mk_dev (1024 * 4096) in
  let wal_dev = mk_dev 65536 in
  let main_n_pages = Int64.of_int (Bytes.length main_dev.buf / 4096) in
  let read_page ~page_id buf =
    let off = Int64.to_int (Int64.mul page_id 4096L) in
    let len = Cstruct.length buf in
    if off + len > Bytes.length main_dev.buf
    then Lwt.return (Error "read past EOF")
    else (
      Cstruct.blit_from_bytes main_dev.buf off buf 0 len;
      Lwt.return (Ok ()))
  in
  let write_page ~page_id buf =
    let off = Int64.to_int (Int64.mul page_id 4096L) in
    let len = Cstruct.length buf in
    dev_grow main_dev (off + len);
    Cstruct.blit_to_bytes buf 0 main_dev.buf off len;
    Lwt.return (Ok ())
  in
  let resize ~n_pages =
    dev_grow main_dev (Int64.to_int n_pages * 4096);
    Lwt.return (Ok ())
  in
  Store.open_block_wal
    ~read_page
    ~write_page
    ~sync:sync_ok
    ~resize
    ~n_pages:main_n_pages
    ~wal_read_at:(read_at wal_dev)
    ~wal_write_at:(write_at wal_dev)
    ~wal_sync
    ~wal_size_bytes:(Int64.of_int (Bytes.length wal_dev.buf))
    ~close:(fun () -> Lwt.return_unit)
    ~wal_close:(fun () -> Lwt.return_unit)
    ()
;;

(* ------------------------------------------------------------------ *)
(* Test: replication position blocks and unblocks checkpoint            *)
(* ------------------------------------------------------------------ *)

let test_replication_gating_blocks_checkpoint () =
  Lwt_main.run
    (let* sr = open_test_store () in
     let st =
       match sr with
       | Ok s -> s
       | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
     in
     (* Commit some data to build up WAL frames.  Keep the default (high)
        autocheckpoint threshold for this first commit so it does not trigger
        an ungated checkpoint that would reset the WAL before we gate it. *)
     let* rw = Store.rw_begin st in
     let* () = Store.put rw 16 (Bytes.of_string "k1") (Bytes.of_string "v1") in
     let* () = Store.put rw 16 (Bytes.of_string "k2") (Bytes.of_string "v2") in
     let* () = Store.commit rw in
     let epoch_before, frames_before =
       match Store.replication_state st with
       | Some s -> s
       | None -> Alcotest.failf "expected WAL mode"
     in
     Alcotest.(check bool) "WAL has frames" true (frames_before > 0);
     (* Pin replication position at 0 so checkpoint cannot proceed, then lower
        the autocheckpoint threshold so the following commit crosses it and the
        (now gated) autocheckpoint parks instead of resetting the WAL. *)
     Store.update_replication_position st ~shipped:0;
     Store.set_wal_autocheckpoint st 3;
     (* Commit enough to cross threshold — the background autocheckpoint
        (dispatched via Lwt.async in maybe_autockpt_after_commit) will park
        on wait_for_readers_past because shipped=0 < committed_frames. *)
     let* rw2 = Store.rw_begin st in
     let* () = Store.put rw2 16 (Bytes.of_string "k3") (Bytes.of_string "v3") in
     let* () = Store.put rw2 16 (Bytes.of_string "k4") (Bytes.of_string "v4") in
     let* () = Store.commit rw2 in
     (* Yield several times to let any pending Lwt.async fibers (including
        the parked autocheckpoint) run. *)
     let* () = wait_for (fun () -> false) 20 in
     (* Verify checkpoint is still blocked — epoch unchanged, frames present. *)
     let epoch_blocked, frames_blocked =
       match Store.replication_state st with
       | Some s -> s
       | None -> Alcotest.failf "expected WAL mode"
     in
     Alcotest.(check int64)
       "epoch unchanged (checkpoint parked)"
       epoch_before
       epoch_blocked;
     Alcotest.(check bool)
       "frames not reset (checkpoint parked)"
       true
       (frames_blocked >= frames_before);
     (* Now advance replication position — this broadcasts reader_done_cond
        and wakes the parked checkpoint. *)
     Store.update_replication_position st ~shipped:max_int;
     (* Wait for checkpoint to complete: frames drained from WAL.
        With a real predicate the cap can be generous — loop exits
        the instant the condition holds. *)
     let* () =
       wait_for
         (fun () ->
            match Store.replication_state st with
            | Some (_, frames) -> frames < frames_before
            | None -> false)
         50
     in
     let _, frames_after =
       match Store.replication_state st with
       | Some s -> s
       | None -> Alcotest.failf "expected WAL mode"
     in
     Alcotest.(check bool) "WAL reset after unblock" true (frames_after < frames_before);
     let* () = Store.close st in
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Test: gating survives epoch bump after checkpoint                     *)
(* ------------------------------------------------------------------ *)

(** Verify that after a checkpoint resets the WAL (epoch++, committed→0),
    the replication floor is re-pinned so the next checkpoint still waits
    for the sink to ship new-epoch frames. *)
let test_replication_gating_survives_epoch_bump () =
  Lwt_main.run
    (let* sr = open_test_store () in
     let st =
       match sr with
       | Ok s -> s
       | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
     in
     (* Register callback so checkpoint_unlocked re-pins the floor. *)
     let* () =
       Store.set_commit_callback
         st
         (Some (fun ~epoch:_ ~base_idx:_ ~count:_ -> Lwt.return_unit))
     in
     Store.set_wal_autocheckpoint st 3;
     (* Epoch 0: commit, ship, let checkpoint proceed. *)
     let* rw = Store.rw_begin st in
     let* () = Store.put rw 16 (Bytes.of_string "a") (Bytes.of_string "1") in
     let* () = Store.put rw 16 (Bytes.of_string "b") (Bytes.of_string "2") in
     let* () = Store.put rw 16 (Bytes.of_string "c") (Bytes.of_string "3") in
     let* () = Store.commit rw in
     let epoch0, frames0 =
       match Store.replication_state st with
       | Some s -> s
       | None -> Alcotest.failf "expected WAL mode"
     in
     Store.update_replication_position st ~shipped:max_int;
     (* Wait for checkpoint to complete: epoch bumped and WAL reset. *)
     let* () =
       wait_for
         (fun () ->
            match Store.replication_state st with
            | Some (epoch, frames) -> epoch > epoch0 && frames < frames0
            | None -> false)
         50
     in
     let epoch1, frames1 =
       match Store.replication_state st with
       | Some s -> s
       | None -> Alcotest.failf "expected WAL mode"
     in
     Alcotest.(check bool) "epoch bumped after ckpt" true (epoch1 > epoch0);
     Alcotest.(check bool) "WAL reset after ckpt" true (frames1 < frames0);
     (* Epoch 1: commit again WITHOUT explicitly setting the floor.
        If the re-pin in checkpoint_unlocked is working, the floor was
        reset to 0 after Wal.reset and the autocheckpoint parks.
        If the re-pin is missing, the floor is stale at max_int and
        the autocheckpoint proceeds without waiting — observable as
        an epoch bump despite no position advance. *)
     let* rw2 = Store.rw_begin st in
     let* () = Store.put rw2 16 (Bytes.of_string "d") (Bytes.of_string "4") in
     let* () = Store.put rw2 16 (Bytes.of_string "e") (Bytes.of_string "5") in
     let* () = Store.put rw2 16 (Bytes.of_string "f") (Bytes.of_string "6") in
     let* () = Store.commit rw2 in
     let* () = wait_for (fun () -> false) 20 in
     let epoch2, frames2 =
       match Store.replication_state st with
       | Some s -> s
       | None -> Alcotest.failf "expected WAL mode"
     in
     Alcotest.(check int64) "epoch unchanged (gated after epoch bump)" epoch1 epoch2;
     Alcotest.(check bool) "frames not reset (gated)" true (frames2 > 0);
     (* Unblock and verify checkpoint completes. *)
     Store.update_replication_position st ~shipped:max_int;
     let* () =
       wait_for
         (fun () ->
            match Store.replication_state st with
            | Some (epoch, frames) -> epoch > epoch2 && frames < frames2
            | None -> false)
         50
     in
     let epoch3, frames3 =
       match Store.replication_state st with
       | Some s -> s
       | None -> Alcotest.failf "expected WAL mode"
     in
     Alcotest.(check bool) "epoch bumped after unblock" true (epoch3 > epoch2);
     Alcotest.(check bool) "WAL reset after unblock" true (frames3 < frames2);
     let* () = Store.set_commit_callback st None in
     let* () = Store.close st in
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* #207: bounded-yield timeout on the checkpoint replication gate       *)
(* ------------------------------------------------------------------ *)

(** Getter/setter contract: default unbounded on the B+-tree backend, [0]
    on the in-memory backend, negative inputs clamp to [0], no-op on Mem. *)
let test_gate_yields_getter_setter () =
  Lwt_main.run
    (let* sr = open_test_store () in
     let st =
       match sr with
       | Ok s -> s
       | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
     in
     Alcotest.(check int)
       "default unbounded"
       max_int
       (Store.replication_gate_max_yields st);
     Store.set_replication_gate_max_yields st (-5);
     Alcotest.(check int) "negative clamps to 0" 0 (Store.replication_gate_max_yields st);
     Store.set_replication_gate_max_yields st 7;
     Alcotest.(check int) "set to 7" 7 (Store.replication_gate_max_yields st);
     let mem = Store.create () in
     Alcotest.(check int) "Mem reports 0" 0 (Store.replication_gate_max_yields mem);
     Store.set_replication_gate_max_yields mem 9;
     Alcotest.(check int)
       "Mem setter is a no-op"
       0
       (Store.replication_gate_max_yields mem);
     let* () = Store.close st in
     Lwt.return_unit)
;;

(** With a finite budget, a checkpoint blocked only by a stranded
    replication floor proceeds anyway once the budget is spent — the
    standby never advances, yet the WAL is recycled (epoch bumps). *)
let test_gate_timeout_proceeds_past_stranded_floor () =
  Lwt_main.run
    (let* sr = open_test_store () in
     let st =
       match sr with
       | Ok s -> s
       | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
     in
     let* rw = Store.rw_begin st in
     let* () = Store.put rw 16 (Bytes.of_string "k1") (Bytes.of_string "v1") in
     let* () = Store.put rw 16 (Bytes.of_string "k2") (Bytes.of_string "v2") in
     let* () = Store.commit rw in
     let epoch_before, frames_before =
       match Store.replication_state st with
       | Some s -> s
       | None -> Alcotest.failf "expected WAL mode"
     in
     Alcotest.(check bool) "WAL has frames" true (frames_before > 0);
     (* Pin the floor behind the committed frames so it would gate forever,
        then give the gate a small finite budget. *)
     Store.update_replication_position st ~shipped:0;
     Store.set_replication_gate_max_yields st 5;
     (* Direct checkpoint: floor (0) < target, so the gate spends its 5-yield
        budget and then proceeds WITHOUT the floor ever advancing. *)
     let* () = Store.checkpoint st in
     let epoch_after, frames_after =
       match Store.replication_state st with
       | Some s -> s
       | None -> Alcotest.failf "expected WAL mode"
     in
     Alcotest.(check bool)
       "epoch bumped despite stranded floor"
       true
       (epoch_after > epoch_before);
     Alcotest.(check bool) "WAL recycled despite stranded floor" true (frames_after = 0);
     let* () = Store.close st in
     Lwt.return_unit)
;;

(** The #207 budget governs ONLY the replication floor.  A local RO reader
    pinned below the checkpoint target must never be abandoned: the
    checkpoint stays parked through a finite budget and only completes once
    the reader ends. *)
let test_gate_timeout_never_abandons_ro_reader () =
  Lwt_main.run
    (let* sr = open_test_store () in
     let st =
       match sr with
       | Ok s -> s
       | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
     in
     (* Commit, then open an RO snapshot pinned at this (lower) frame count. *)
     let* rw = Store.rw_begin st in
     let* () = Store.put rw 16 (Bytes.of_string "a") (Bytes.of_string "1") in
     let* () = Store.commit rw in
     let* snap = Store.ro_begin st in
     let epoch_before, _ =
       match Store.replication_state st with
       | Some s -> s
       | None -> Alcotest.failf "expected WAL mode"
     in
     (* Commit more so the checkpoint target moves past the snapshot's pin. *)
     let* rw2 = Store.rw_begin st in
     let* () = Store.put rw2 16 (Bytes.of_string "b") (Bytes.of_string "2") in
     let* () = Store.commit rw2 in
     (* No replication floor active; a finite budget would let the gate give
        up on the floor — but the RO reader is an unconditional gate. *)
     Store.set_replication_gate_max_yields st 3;
     let ckpt = Store.checkpoint st in
     (* Let the checkpoint fiber reach its parked state. *)
     let* () = wait_for (fun () -> false) 20 in
     let epoch_parked, frames_parked =
       match Store.replication_state st with
       | Some s -> s
       | None -> Alcotest.failf "expected WAL mode"
     in
     Alcotest.(check int64)
       "checkpoint parked on RO reader (epoch unchanged)"
       epoch_before
       epoch_parked;
     Alcotest.(check bool) "WAL not recycled while reader active" true (frames_parked > 0);
     (* Release the reader: the checkpoint must now complete. *)
     let* () = Store.ro_end snap in
     let* () = ckpt in
     let epoch_after, frames_after =
       match Store.replication_state st with
       | Some s -> s
       | None -> Alcotest.failf "expected WAL mode"
     in
     Alcotest.(check bool)
       "epoch bumped after reader ended"
       true
       (epoch_after > epoch_before);
     Alcotest.(check bool) "WAL recycled after reader ended" true (frames_after = 0);
     let* () = Store.close st in
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Test: commit callback fired                                          *)
(* ------------------------------------------------------------------ *)

let test_commit_callback_fired () =
  Lwt_main.run
    (let* sr = open_test_store () in
     let st =
       match sr with
       | Ok s -> s
       | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
     in
     let cb_fired = ref false in
     let cb_count = ref 0 in
     let cb_promise, cb_resolver = Lwt.wait () in
     let* () =
       Store.set_commit_callback
         st
         (Some
            (fun ~epoch:_ ~base_idx:_ ~count ->
              cb_fired := true;
              cb_count := count;
              Lwt.wakeup cb_resolver ();
              Lwt.return_unit))
     in
     let* rw = Store.rw_begin st in
     let* () = Store.put rw 16 (Bytes.of_string "hello") (Bytes.of_string "world") in
     let* () = Store.commit rw in
     let* () = cb_promise in
     Alcotest.(check bool) "callback fired" true !cb_fired;
     Alcotest.(check bool) "non-zero count" true (!cb_count > 0);
     let* () = Store.set_commit_callback st None in
     let* () = Store.close st in
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* #337: an autocheckpoint must not Wal.reset out from under a sink     *)
(* ship that is still in flight (the async cb reads frames lazily).     *)
(* ------------------------------------------------------------------ *)

let test_checkpoint_waits_for_in_flight_ship () =
  Lwt_main.run
    (let* sr = open_test_store () in
     let st =
       match sr with
       | Ok s -> s
       | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
     in
     let epoch_changed_under_cb = ref false in
     let cb_completed = ref false in
     (* A slow lazy reader: yield repeatedly, each time checking that the epoch
        we were shipped is still the live epoch.  If a checkpoint resets the WAL
        while we are in flight, [replication_state] reports a bumped epoch — the
        exact corruption #337 describes (a real reader would get Corrupt_frame
        with a stale epoch). *)
     let cb ~epoch ~base_idx:_ ~count:_ =
       let rec spin n =
         if n = 0
         then (
           cb_completed := true;
           Lwt.return_unit)
         else (
           (match Store.replication_state st with
            | Some (e, _) when not (Int64.equal e epoch) -> epoch_changed_under_cb := true
            | _ -> ());
           let* () = Lwt.pause () in
           spin (n - 1))
       in
       spin 12
     in
     let* () = Store.set_commit_callback st (Some cb) in
     Store.set_wal_autocheckpoint st 1;
     (* Lift the acked-position floor to max_int so the checkpoint is gated ONLY
        by the in-flight-ship guard, not by the floor. *)
     Store.update_replication_position st ~shipped:max_int;
     let* rw = Store.rw_begin st in
     let* () = Store.put rw 16 (Bytes.of_string "a") (Bytes.of_string "1") in
     let* () = Store.put rw 16 (Bytes.of_string "b") (Bytes.of_string "2") in
     let* () = Store.commit rw in
     let* () = wait_for (fun () -> !cb_completed) 100 in
     Alcotest.(check bool) "ship callback ran to completion" true !cb_completed;
     Alcotest.(check bool)
       "WAL not reset out from under in-flight sink ship"
       false
       !epoch_changed_under_cb;
     let* () = Store.set_commit_callback st None in
     let* () = Store.close st in
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* #338: close drains an in-flight autocheckpoint without tearing down  *)
(* fds underneath it — and without acquiring t.lock (which an abandoned  *)
(* write txn or a floor-stranded checkpoint would hang it on).          *)
(* ------------------------------------------------------------------ *)

(* #338 (review #3): close must NOT take the write lock — an abandoned write txn
   holds it until commit/rollback (reachable from Db.close, which does not finish
   active txns), so close would deadlock.  Here a txn is left open; close must
   still complete. *)
let test_close_does_not_hang_on_open_txn () =
  Lwt_main.run
    (let* sr = open_test_store () in
     let st =
       match sr with
       | Ok s -> s
       | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
     in
     let* (_ : Store.rw Store.txn) = Store.rw_begin st in
     (* txn deliberately neither committed nor rolled back: it holds the write
        lock. *)
     let close_p = Store.close st in
     let* () =
       wait_for
         (fun () ->
            match Lwt.state close_p with
            | Lwt.Sleep -> false
            | _ -> true)
         50
     in
     Alcotest.(check bool)
       "close completes despite an open write txn (no deadlock on t.lock)"
       true
       (match Lwt.state close_p with
        | Lwt.Return () -> true
        | _ -> false);
     Lwt.return_unit)
;;

(* #338 (review #2/#4): close drains an in-flight autocheckpoint that is parked
   on a stranded replication floor.  It must complete WITHOUT advancing the floor
   (it signals teardown so the checkpoint unwinds) and must not checkpoint under
   teardown (epoch unchanged — no Wal.reset on the about-to-close fds). *)
let test_close_drains_in_flight_checkpoint () =
  Lwt_main.run
    (let* sr = open_test_store () in
     let st =
       match sr with
       | Ok s -> s
       | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
     in
     let* rw = Store.rw_begin st in
     let* () = Store.put rw 16 (Bytes.of_string "k1") (Bytes.of_string "v1") in
     let* () = Store.put rw 16 (Bytes.of_string "k2") (Bytes.of_string "v2") in
     let* () = Store.commit rw in
     let epoch0, _ =
       match Store.replication_state st with
       | Some s -> s
       | None -> Alcotest.failf "expected WAL mode"
     in
     (* Pin the floor low and lower the threshold so the next commit dispatches
        an autocheckpoint that PARKS (holding the write lock) on the floor gate,
        leaving [autockpt_in_flight = true].  The floor is never advanced. *)
     Store.update_replication_position st ~shipped:0;
     Store.set_wal_autocheckpoint st 3;
     let* rw2 = Store.rw_begin st in
     let* () = Store.put rw2 16 (Bytes.of_string "k3") (Bytes.of_string "v3") in
     let* () = Store.put rw2 16 (Bytes.of_string "k4") (Bytes.of_string "v4") in
     let* () = Store.commit rw2 in
     let* () = wait_for (fun () -> false) 20 in
     let close_p = Store.close st in
     let* () =
       wait_for
         (fun () ->
            match Lwt.state close_p with
            | Lwt.Sleep -> false
            | _ -> true)
         50
     in
     Alcotest.(check bool)
       "close completes despite a floor-stranded in-flight checkpoint"
       true
       (match Lwt.state close_p with
        | Lwt.Return () -> true
        | _ -> false);
     let epoch1, _ =
       match Store.replication_state st with
       | Some s -> s
       | None -> Alcotest.failf "expected WAL mode"
     in
     Alcotest.(check int64)
       "in-flight checkpoint aborted at close (epoch unchanged)"
       epoch0
       epoch1;
     Lwt.return_unit)
;;

(* #338 (review r2 #2): close must drain in-flight async sink ships before
   tearing down the WAL fd — their callbacks read frames lazily, so a torn-down
   fd loses the standby's tail.  Here the ship callback blocks on a resolver we
   control; close must NOT complete until the ship finishes. *)
let test_close_drains_in_flight_ship () =
  Lwt_main.run
    (let* sr = open_test_store () in
     let st =
       match sr with
       | Ok s -> s
       | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
     in
     Store.set_wal_autocheckpoint st 0;
     (* no checkpoint interference *)
     let ship_gate, release_ship = Lwt.wait () in
     let* () =
       Store.set_commit_callback
         st
         (Some (fun ~epoch:_ ~base_idx:_ ~count:_ -> ship_gate))
     in
     let* rw = Store.rw_begin st in
     let* () = Store.put rw 16 (Bytes.of_string "a") (Bytes.of_string "1") in
     let* () = Store.commit rw in
     (* ship is dispatched and now blocked in the callback. *)
     let close_p = Store.close st in
     let* () = wait_for (fun () -> false) 15 in
     Alcotest.(check bool)
       "close blocks until the in-flight ship completes"
       true
       (match Lwt.state close_p with
        | Lwt.Sleep -> true
        | _ -> false);
     (* let the ship finish; close then drains and completes. *)
     Lwt.wakeup_later release_ship ();
     let* () =
       wait_for
         (fun () ->
            match Lwt.state close_p with
            | Lwt.Sleep -> false
            | _ -> true)
         50
     in
     Alcotest.(check bool)
       "close completes after the ship drains"
       true
       (match Lwt.state close_p with
        | Lwt.Return () -> true
        | _ -> false);
     Lwt.return_unit)
;;

(* #338 (review r2 #1): an autocheckpoint dispatched by a committer can park on
   [acquire_write] behind a write lock another (abandoned) txn grabbed during the
   committer's fsync window.  close must not wait on such a not-yet-fd-active
   checkpoint (it would hang forever).  We reproduce by starting a threshold-1
   commit, then taking the lock with a second txn while the first is mid-commit,
   then leaving that txn abandoned. *)
let test_close_does_not_hang_on_lock_parked_checkpoint () =
  Lwt_main.run
    (let* sr = open_test_store () in
     let st =
       match sr with
       | Ok s -> s
       | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
     in
     Store.set_wal_autocheckpoint st 1;
     let* rw_a = Store.rw_begin st in
     let* () = Store.put rw_a 16 (Bytes.of_string "a") (Bytes.of_string "1") in
     let commit_a = Store.commit rw_a in
     (* While A is committing (it releases the write lock for its fsync), B grabs
        the lock and is then abandoned — A's post-fsync autockpt dispatch parks on
        [acquire_write]. *)
     let* (_ : Store.rw Store.txn) = Store.rw_begin st in
     let* () = commit_a in
     let* () = wait_for (fun () -> false) 15 in
     let close_p = Store.close st in
     let* () =
       wait_for
         (fun () ->
            match Lwt.state close_p with
            | Lwt.Sleep -> false
            | _ -> true)
         50
     in
     Alcotest.(check bool)
       "close completes despite an autockpt parked on the write lock"
       true
       (match Lwt.state close_p with
        | Lwt.Return () -> true
        | _ -> false);
     Lwt.return_unit)
;;

(* #338 (review r3 #2): a commit mid-fsync when [close] starts must NOT dispatch
   a fresh ship after close's drain has passed — its lazy reader would race
   [wal_close].  Gate the commit's fsync so it is still in flight when close
   drains (sees no ship), then resume it; the ship dispatch must observe
   [closing] and be skipped (callback never fires). *)
let test_close_suppresses_post_drain_ship () =
  Lwt_main.run
    (let sync_gate, release_sync = Lwt.wait () in
     let gated = ref false in
     let wal_sync () =
       if !gated
       then
         let* () = sync_gate in
         Lwt.return (Ok ())
       else Lwt.return (Ok ())
     in
     let* sr = open_test_store ~wal_sync () in
     let st =
       match sr with
       | Ok s -> s
       | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
     in
     Store.set_wal_autocheckpoint st 0;
     let shipped = ref 0 in
     let* () =
       Store.set_commit_callback
         st
         (Some
            (fun ~epoch:_ ~base_idx:_ ~count:_ ->
              incr shipped;
              Lwt.return_unit))
     in
     (* Arm the gate so the NEXT commit's fsync blocks mid-flight. *)
     gated := true;
     let* rw = Store.rw_begin st in
     let* () = Store.put rw 16 (Bytes.of_string "a") (Bytes.of_string "1") in
     let commit_p = Store.commit rw in
     (* commit is now parked in [wal_sync]; lock released, ship not yet dispatched. *)
     let* () = wait_for (fun () -> false) 10 in
     let close_p = Store.close st in
     let* () =
       wait_for
         (fun () ->
            match Lwt.state close_p with
            | Lwt.Sleep -> false
            | _ -> true)
         50
     in
     Alcotest.(check bool)
       "close completes while the gated commit is still mid-fsync"
       true
       (match Lwt.state close_p with
        | Lwt.Return () -> true
        | _ -> false);
     (* Release the fsync; the commit resumes and reaches its ship block. *)
     Lwt.wakeup_later release_sync ();
     let* () = commit_p in
     let* () = wait_for (fun () -> false) 10 in
     Alcotest.(check int)
       "ship suppressed once closing (no read against closed fd)"
       0
       !shipped;
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "replication-gating"
    [ ( "gating"
      , [ Alcotest.test_case
            "position blocks checkpoint"
            `Quick
            test_replication_gating_blocks_checkpoint
        ; Alcotest.test_case
            "survives epoch bump"
            `Quick
            test_replication_gating_survives_epoch_bump
        ; Alcotest.test_case "commit callback fires" `Quick test_commit_callback_fired
        ] )
    ; ( "gate_timeout"
      , [ Alcotest.test_case
            "gate yields getter/setter"
            `Quick
            test_gate_yields_getter_setter
        ; Alcotest.test_case
            "timeout proceeds past stranded floor"
            `Quick
            test_gate_timeout_proceeds_past_stranded_floor
        ; Alcotest.test_case
            "timeout never abandons RO reader"
            `Quick
            test_gate_timeout_never_abandons_ro_reader
        ] )
    ; ( "in_flight_safety"
      , [ Alcotest.test_case
            "#337 checkpoint waits for in-flight ship"
            `Quick
            test_checkpoint_waits_for_in_flight_ship
        ; Alcotest.test_case
            "#338 close does not hang on an open txn"
            `Quick
            test_close_does_not_hang_on_open_txn
        ; Alcotest.test_case
            "#338 close drains in-flight checkpoint"
            `Quick
            test_close_drains_in_flight_checkpoint
        ; Alcotest.test_case
            "#338 close drains in-flight ship"
            `Quick
            test_close_drains_in_flight_ship
        ; Alcotest.test_case
            "#338 close does not hang on lock-parked checkpoint"
            `Quick
            test_close_does_not_hang_on_lock_parked_checkpoint
        ; Alcotest.test_case
            "#338 close suppresses post-drain ship"
            `Quick
            test_close_suppresses_post_drain_ship
        ] )
    ]
;;
