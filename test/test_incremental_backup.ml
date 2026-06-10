(** Tests for #265 — incremental backup (WAL-frame based). *)

open Lwt.Syntax
module Store = Sqlocaml_store.Store
module Replication = Sqlocaml_replication.Replication
module Pager = Sqlocaml_storage.Pager
module Freelist = Sqlocaml_storage.Freelist

(* ------------------------------------------------------------------ *)
(* In-memory device helpers                                           *)
(* ------------------------------------------------------------------ *)

type dev = { mutable buf : Bytes.t }

let mk_dev size = { buf = Bytes.make size '\x00' }
let dev_size d = Int64.of_int (Bytes.length d.buf)

let dev_grow d need =
  let cur = Bytes.length d.buf in
  if need > cur
  then (
    let new_size = max need (cur * 2) in
    let nb = Bytes.make new_size '\x00' in
    Bytes.blit d.buf 0 nb 0 cur;
    d.buf <- nb)
;;

let read_page d ~page_id out =
  let page_size = 4096 in
  let off = Int64.to_int (Int64.mul page_id (Int64.of_int page_size)) in
  let len = Cstruct.length out in
  if off + len > Bytes.length d.buf
  then Lwt.return (Error "read past EOF")
  else (
    Cstruct.blit_from_bytes d.buf off out 0 len;
    Lwt.return (Ok ()))
;;

let write_page d ~page_id src =
  let page_size = 4096 in
  let off = Int64.to_int (Int64.mul page_id (Int64.of_int page_size)) in
  let len = Cstruct.length src in
  dev_grow d (off + len);
  let tmp = Bytes.create len in
  Cstruct.blit_to_bytes src 0 tmp 0 len;
  Bytes.blit tmp 0 d.buf off len;
  Lwt.return (Ok ())
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
  let tmp = Bytes.create len in
  Cstruct.blit_to_bytes src 0 tmp 0 len;
  Bytes.blit tmp 0 d.buf off len;
  Lwt.return (Ok ())
;;

let sync_ok () = Lwt.return (Ok ())
let resize_ok ~n_pages:_ = Lwt.return (Ok ())
let close_ok () = Lwt.return_unit

let store_with_wal () =
  let main_d = mk_dev 65536 in
  let wal_d = mk_dev 65536 in
  let* r =
    Store.open_block_wal
      ~read_page:(read_page main_d)
      ~write_page:(write_page main_d)
      ~sync:sync_ok
      ~resize:resize_ok
      ~n_pages:(dev_size main_d)
      ~wal_read_at:(read_at wal_d)
      ~wal_write_at:(write_at wal_d)
      ~wal_sync:sync_ok
      ~wal_size_bytes:(dev_size wal_d)
      ~close:close_ok
      ~wal_close:close_ok
      ()
  in
  match r with
  | Ok store -> Lwt.return (store, main_d, wal_d)
  | Error e -> Alcotest.failf "store_with_wal: %a" Store.pp_error e
;;

let bs s = Bytes.of_string s

let run = Lwt_main.run

(* ------------------------------------------------------------------ *)
(* capture_frames_since — basic frame capture                         *)
(* ------------------------------------------------------------------ *)

let test_capture_basic () =
  let* store, _main_d, _wal_d = store_with_wal () in
  let* () =
    let* tx = Store.rw_begin store in
    let* () = Store.put tx 16 (bs "k1") (bs "v1") in
    let* () = Store.put tx 16 (bs "k2") (bs "v2") in
    Store.commit tx
  in
  let epoch0, frames0 =
    match Store.replication_state store with
    | Some s -> s
    | None -> Alcotest.fail "expected WAL mode"
  in
  Alcotest.(check bool) "frames committed after first write" true (frames0 > 0);
  let* r = Store.capture_frames_since store ~since_epoch:epoch0 ~since_idx:(-1) in
  match r with
  | None -> Alcotest.fail "capture returned None (epoch changed unexpectedly)"
  | Some (Error e) -> Alcotest.failf "capture error: %s" (match e with `Capture_error s -> s)
  | Some (Ok frames) ->
    Alcotest.(check int) "captured all frames since idx -1" frames0 (List.length frames);
    (* Only the last frame of each commit batch has is_commit=true *)
    let last_is_commit = (List.nth frames (frames0 - 1)).is_commit in
    Alcotest.(check bool) "last frame is commit" true last_is_commit;
    (* Verify all frames have correct epoch and sequential indices *)
    let check_frame (i : int) (f : Store.backup_frame) =
      Alcotest.(check int64) (Printf.sprintf "frame %d epoch" i) epoch0 (f.epoch : int64);
      Alcotest.(check int) (Printf.sprintf "frame %d idx" i) i (f.frame_idx : int)
    in
    List.iteri check_frame frames;
    (* Verify checksums via the transport check *)
    List.iteri
      (fun i f ->
         let rf = Replication.backup_frame_to_replicated f in
         Alcotest.(check bool) (Printf.sprintf "frame %d checksum valid" i)
           true (Replication.verify_checksum rf))
      frames;
    let* () = Store.close store in
    Lwt.return_unit
;;

(* ------------------------------------------------------------------ *)
(* capture_frames_since — empty capture when no new frames            *)
(* ------------------------------------------------------------------ *)

let test_capture_empty () =
  let* store, _main_d, _wal_d = store_with_wal () in
  let epoch0, frames0 =
    match Store.replication_state store with
    | Some s -> s
    | None -> Alcotest.fail "expected WAL mode"
  in
  let* r = Store.capture_frames_since store ~since_epoch:epoch0 ~since_idx:(frames0 - 1) in
  match r with
  | None -> Alcotest.fail "capture returned None"
  | Some (Error e) -> Alcotest.failf "capture error: %s" (match e with `Capture_error s -> s)
  | Some (Ok frames) ->
    Alcotest.(check int) "no new frames to capture" 0 (List.length frames);
    let* () = Store.close store in
    Lwt.return_unit
;;

(* ------------------------------------------------------------------ *)
(* capture_frames_since — stale epoch returns None                    *)
(* ------------------------------------------------------------------ *)

let test_capture_stale_epoch () =
  let* store, _main_d, _wal_d = store_with_wal () in
  let* r = Store.capture_frames_since store ~since_epoch:999L ~since_idx:0 in
  match r with
  | None ->
    (* Expected: epoch changed (watermark is stale) *)
    let* () = Store.close store in
    Lwt.return_unit
  | Some _ ->
    Alcotest.fail "expected None for stale epoch"
;;

(* ------------------------------------------------------------------ *)
(* backup floor pin: checkpoint is gated until backup position is set *)
(* ------------------------------------------------------------------ *)

let test_backup_floor_gates_checkpoint () =
  let* store, _main_d, _wal_d = store_with_wal () in
  Store.set_wal_autocheckpoint store 3;
  (* Register the backup consumer with position at current committed_frames. *)
  let epoch0, committed0 =
    match Store.replication_state store with
    | Some s -> s
    | None -> Alcotest.fail "expected WAL mode"
  in
  Store.update_backup_position store ~shipped:committed0;
  (* Write enough to trigger autocheckpoint (threshold=3). *)
  let* () =
    let* tx = Store.rw_begin store in
    let* () = Store.put tx 16 (bs "a") (bs "1") in
    let* () = Store.put tx 16 (bs "b") (bs "2") in
    let* () = Store.put tx 16 (bs "c") (bs "3") in
    Store.commit tx
  in
  (* Give the checkpoint fiber (dispatched via Lwt.async from
     commit) enough scheduler turns that it would reach the backup
     floor gate if the gate were broken.  Then assert the epoch
     is unchanged — proving the gate actually held. *)
  let rec pause_n n =
    if n <= 0 then Lwt.return_unit
    else let* () = Lwt.pause () in pause_n (n - 1)
  in
  let* () = pause_n 50 in
  let epoch1, _frames1 =
    match Store.replication_state store with
    | Some s -> s
    | None -> Alcotest.fail "expected WAL mode"
  in
  (* Without advancing the backup floor, checkpoint must not have
     completed (epoch should be unchanged because the backup floor
     is set to committed0, which is below the target). *)
  Alcotest.(check int64) "epoch unchanged (gated by backup floor)" epoch0 epoch1;
  (* Now advance the backup floor past the checkpoint target. *)
  let _, committed1 =
    match Store.replication_state store with
    | Some s -> s
    | None -> Alcotest.fail "expected WAL mode"
  in
  Store.update_backup_position store ~shipped:committed1;
  let rec wait_advanced n =
    if n <= 0
    then Alcotest.fail "timeout: checkpoint did not complete after floor advance"
    else
      match Store.replication_state store with
      | Some (epoch, frames) when Int64.compare epoch epoch0 > 0 && frames < committed1 ->
        Lwt.return_unit
      | _ ->
        let* () = Lwt.pause () in
        wait_advanced (n - 1)
  in
  let* () = wait_advanced 100 in
  let epoch2, frames2 =
    match Store.replication_state store with
    | Some s -> s
    | None -> Alcotest.fail "expected WAL mode"
  in
  Alcotest.(check bool) "epoch bumped after floor advance" true (Int64.compare epoch2 epoch0 > 0);
  Alcotest.(check bool) "frames reset after checkpoint" true (frames2 < committed1);
  let* () = Store.close store in
  Lwt.return_unit
;;

(* ------------------------------------------------------------------ *)
(* backup_frame_to_replicated conversion round-trip                   *)
(* ------------------------------------------------------------------ *)

let test_frame_conversion_round_trip () =
  let* store, _main_d, _wal_d = store_with_wal () in
  let* () =
    let* tx = Store.rw_begin store in
    let* () = Store.put tx 16 (bs "k1") (bs "v1") in
    Store.commit tx
  in
  let epoch, _committed =
    match Store.replication_state store with
    | Some s -> s
    | None -> Alcotest.fail "expected WAL mode"
  in
  let* r = Store.capture_frames_since store ~since_epoch:epoch ~since_idx:(-1) in
  let frames =
    match r with
    | Some (Ok fs) -> fs
    | _ -> Alcotest.fail "capture failed"
  in
  Alcotest.(check bool) "frames captured" true (List.length frames > 0);
  (* Convert each backup_frame to replicated_frame and back *)
  List.iteri
    (fun i bf ->
       let rf = Replication.backup_frame_to_replicated bf in
       Alcotest.(check int64) (Printf.sprintf "rf %d: epoch" i) bf.epoch rf.epoch;
       Alcotest.(check int) (Printf.sprintf "rf %d: frame_idx" i) bf.frame_idx rf.frame_idx;
       Alcotest.(check int64) (Printf.sprintf "rf %d: page_id" i) bf.page_id rf.page_id;
       Alcotest.(check bool) (Printf.sprintf "rf %d: is_commit" i) bf.is_commit rf.is_commit;
       Alcotest.(check bool) (Printf.sprintf "rf %d: checksum valid" i)
         true (Replication.verify_checksum rf))
    frames;
  let* () = Store.close store in
  Lwt.return_unit
;;

(* ------------------------------------------------------------------ *)
(* ------------------------------------------------------------------ *)
(* incremental_restore round-trip (#265)                               *)
(* ------------------------------------------------------------------ *)

let test_incremental_restore_round_trip () =
  let* store, main_d, _wal_d = store_with_wal () in
  let* () =
    let* tx = Store.rw_begin store in
    let* () = Store.put tx 16 (bs "k1") (bs "v1") in
    let* () = Store.put tx 16 (bs "k2") (bs "v2") in
    Store.commit tx
  in
  let epoch0, frames0 =
    match Store.replication_state store with
    | Some s -> s
    | None -> Alcotest.fail "expected WAL mode"
  in
  Alcotest.(check bool) "frames committed" true (frames0 > 0);
  let* r = Store.capture_frames_since store ~since_epoch:epoch0 ~since_idx:(-1) in
  let frames =
    match r with
    | Some (Ok fs) -> fs
    | _ -> Alcotest.fail "capture failed"
  in
  Alcotest.(check int) "captured frames count" frames0 (List.length frames);
  let* () = Store.close store in
  (* Minimal pager for the restore target (same main device as source). *)
  let pager =
    Pager.create
      ~read_page:(read_page main_d)
      ~write_page:(write_page main_d)
      ~sync:sync_ok
      ~resize:resize_ok
      ~n_pages:16L
      ~freelist:Freelist.empty
  in
  let restore_wal_d = mk_dev 65536 in
  let* r =
    Replication.incremental_restore
      ~read_at:(read_at restore_wal_d)
      ~write_at:(write_at restore_wal_d)
      ~sync:sync_ok
      ~wal_size_bytes:(dev_size restore_wal_d)
      ~pager
      ~incremental_sets:[frames]
      ()
  in
  (match r with
   | Error (`Restore_error msg) ->
     Alcotest.failf "incremental_restore failed: %s" msg
   | Ok (epoch, idx) ->
     Alcotest.(check int64) "restore epoch" epoch0 epoch;
     Alcotest.(check int) "restore final idx" (List.length frames - 1) idx);
  (* Verify the restored data is readable by opening a new Store over
     the same main device and the restore-target WAL device. *)
  let* r2 =
    Store.open_block_wal
      ~read_page:(read_page main_d)
      ~write_page:(write_page main_d)
      ~sync:sync_ok
      ~resize:resize_ok
      ~n_pages:(dev_size main_d)
      ~wal_read_at:(read_at restore_wal_d)
      ~wal_write_at:(write_at restore_wal_d)
      ~wal_sync:sync_ok
      ~wal_size_bytes:(dev_size restore_wal_d)
      ~close:close_ok
      ~wal_close:close_ok
      ()
  in
  match r2 with
  | Error e -> Alcotest.failf "re-open store failed: %a" Store.pp_error e
  | Ok restored_store ->
    let* () =
      Store.with_ro restored_store (fun ro_tx ->
        let* v1 = Store.get ro_tx 16 (bs "k1") in
        (match v1 with
         | None -> Alcotest.fail "k1 not found after restore"
         | Some v -> Alcotest.(check string) "k1 value after restore" "v1" (Bytes.to_string v));
        let* v2 = Store.get ro_tx 16 (bs "k2") in
        (match v2 with
         | None -> Alcotest.fail "k2 not found after restore"
         | Some v -> Alcotest.(check string) "k2 value after restore" "v2" (Bytes.to_string v));
        Lwt.return_unit)
    in
    let* () = Store.close restored_store in
    Lwt.return_unit
;;

(* ------------------------------------------------------------------ *)
(* Test suite                                                           *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "incremental_backup"
    [ ( "capture"
      , [ Alcotest.test_case "basic frame capture" `Quick (fun () -> run (test_capture_basic ()))
        ; Alcotest.test_case "empty capture" `Quick (fun () -> run (test_capture_empty ()))
        ; Alcotest.test_case "stale epoch" `Quick (fun () -> run (test_capture_stale_epoch ()))
        ] )
    ; ( "floor"
      , [ Alcotest.test_case "backup floor gates checkpoint" `Quick
            (fun () -> run (test_backup_floor_gates_checkpoint ()))
        ] )
    ; ( "frame_conversion"
      , [ Alcotest.test_case "backup_frame to replicated_frame" `Quick
            (fun () -> run (test_frame_conversion_round_trip ()))
        ; Alcotest.test_case "incremental_restore round-trip" `Quick
            (fun () -> run (test_incremental_restore_round_trip ()))
        ] )
    ]
;;
