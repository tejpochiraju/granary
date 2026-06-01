(** Tests for replication reader gating in the Store. *)

open Lwt.Syntax
module Store = Sqlocaml_store.Store

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

let open_test_store () =
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
    ~wal_sync:sync_ok
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
     Store.set_commit_callback
       st
       (Some (fun ~epoch:_ ~base_idx:_ ~count:_ -> Lwt.return_unit));
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
     Store.set_commit_callback st None;
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
     Store.set_commit_callback
       st
       (Some
          (fun ~epoch:_ ~base_idx:_ ~count ->
            cb_fired := true;
            cb_count := count;
            Lwt.wakeup cb_resolver ();
            Lwt.return_unit));
     let* rw = Store.rw_begin st in
     let* () = Store.put rw 16 (Bytes.of_string "hello") (Bytes.of_string "world") in
     let* () = Store.commit rw in
     let* () = cb_promise in
     Alcotest.(check bool) "callback fired" true !cb_fired;
     Alcotest.(check bool) "non-zero count" true (!cb_count > 0);
     Store.set_commit_callback st None;
     let* () = Store.close st in
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
    ]
;;
