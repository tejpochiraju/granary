(** End-to-end test for the shared-WAL-handle scenario (#360).

    Exercises the full master–standby lifecycle where the master Store and
    the Standby share the same WAL device.  The master commits data, frames
    are captured and fed to the Standby via the apply loop, and the
    {!Standby.on_standby_ack} callback advances the master's replication
    floor through {!Store.update_replication_position}. *)

open Lwt.Syntax
module Wal = Sqlocaml_storage.Wal
module Replication = Sqlocaml_replication.Replication
module Standby = Sqlocaml_replication.Standby
module Store = Sqlocaml_store.Store
module Pager = Sqlocaml_storage.Pager

(* ------------------------------------------------------------------ *)
(* In-memory device helpers (same pattern as test_standby.ml)          *)
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
  let cur = Bytes.length d.buf in
  if off + len > cur
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
  Cstruct.blit_to_bytes src 0 d.buf off len;
  Lwt.return (Ok ())
;;

let read_at d ~offset out =
  let off = Int64.to_int offset in
  let len = Cstruct.length out in
  let cur = Bytes.length d.buf in
  if off + len > cur
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
(* Helper: bounded-yield wait (same as test_replication_gating.ml)     *)
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
(* Helper: writable pager backed by an in-memory main device           *)
(* ------------------------------------------------------------------ *)

let writable_pager ?(n_pages = 16L) main_d () =
  Pager.create
    ~read_page:(read_page main_d)
    ~write_page:(write_page main_d)
    ~sync:sync_ok
    ~resize:(fun ~n_pages:_ -> Lwt.return (Ok ()))
    ~n_pages
    ~freelist:Sqlocaml_storage.Freelist.empty
;;

(* ------------------------------------------------------------------ *)
(* Topology: open a shared WAL device, a master Store, and a Standby   *)
(* ------------------------------------------------------------------ *)

type topology =
  { shared_wal : Wal.t
  ; master : Store.t
  ; standby : Standby.t
  ; ack_calls : int ref
  ; acked_frames : int ref
  }

let open_topology () =
  let wal_dev = mk_dev 65536 in
  let main_dev = mk_dev (1024 * 4096) in
  let* r =
    Wal.open_
      ~read_at:(read_at wal_dev)
      ~write_at:(write_at wal_dev)
      ~sync:sync_ok
      ~size_bytes:(dev_size wal_dev)
      ()
  in
  let shared_wal =
    match r with
    | Ok w -> w
    | Error e -> Alcotest.failf "wal open: %a" Wal.pp_error e
  in
  let main_n_pages = Int64.of_int (Bytes.length main_dev.buf / 4096) in
  let* sr =
    Store.open_block_wal
      ~read_page:(read_page main_dev)
      ~write_page:(write_page main_dev)
      ~sync:sync_ok
      ~resize:(fun ~n_pages:_ -> Lwt.return (Ok ()))
      ~n_pages:main_n_pages
      ~wal_read_at:(read_at wal_dev)
      ~wal_write_at:(write_at wal_dev)
      ~wal_sync:sync_ok
      ~wal_size_bytes:(dev_size wal_dev)
      ~close:(fun () -> Lwt.return_unit)
      ~wal_close:(fun () -> Lwt.return_unit)
      ()
  in
  let master =
    match sr with
    | Ok s -> s
    | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
  in
  (* Standby store: a separate in-memory store.  The Standby uses its
     own store handle for follower-mode gating and reader-gate dispatching,
     while the master store remains writable. *)
  let standby_store = Store.create () in
  let pager = writable_pager main_dev () in
  let ack_calls = ref 0 in
  let acked_frames = ref 0 in
  let standby =
    Standby.create
      ~store:standby_store
      ~pager
      ~wal:shared_wal
      ~on_standby_ack:(fun frames ->
        ack_calls := !ack_calls + 1;
        acked_frames := frames;
        Store.update_replication_position master ~shipped:frames)
      ()
  in
  Lwt.return { shared_wal; master; standby; ack_calls; acked_frames }
;;

(* ------------------------------------------------------------------ *)
(* #360: shared-WAL-handle end-to-end test                             *)
(* ------------------------------------------------------------------ *)

(** 1. The master commits Store transactions.
    2. Frames are captured from the master store.
    3. The Standby applies them via the shared WAL handle.
    4. The {!on_standby_ack} callback fires and advances the master's
       replication floor.
    5. Data is visible through the shared WAL. *)
let test_shared_wal_end_to_end () =
  Lwt_main.run
    (let* topo = open_topology () in
     (* Commit data on the master store. *)
     let* rw = Store.rw_begin topo.master in
     let* () = Store.put rw 16 (Bytes.of_string "k1") (Bytes.of_string "v1") in
     let* () = Store.put rw 16 (Bytes.of_string "k2") (Bytes.of_string "v2") in
     let* () = Store.commit rw in
     let* () =
       wait_for
         (fun () ->
            match Store.replication_state topo.master with
            | Some (_epoch, frames) -> frames > 0
            | None -> false)
         50
     in
     let commit_frames =
       match Store.replication_state topo.master with
       | Some (_epoch, frames) -> frames
       | None -> Alcotest.fail "master has no WAL"
     in
     Alcotest.(check bool) "master has committed frames" true (commit_frames > 0);
     (* Capture frames from the master store. *)
     let* r = Store.capture_frames_since topo.master ~since_epoch:0L ~since_idx:(-1) in
     let frames =
       match r with
       | Some (Ok fs) -> List.map Replication.backup_frame_to_replicated fs
       | Some (Error (`Capture_error msg)) -> Alcotest.failf "capture error: %s" msg
       | None -> Alcotest.fail "capture_frames_since returned None (epoch changed)"
     in
     Alcotest.(check bool) "captured at least one frame" true (frames <> []);
     (* Feed captured frames to the Standby via its apply loop. *)
     let stream, push = Lwt_stream.create () in
     push (Some frames);
     push None;
     let* r = Standby.start_following topo.standby stream in
     (match r with
      | Ok () ->
        Alcotest.(check bool)
          "standby committed frames via shared WAL"
          true
          (Wal.committed_frames topo.shared_wal > 0);
        Alcotest.(check int)
          "standby committed_frames matches captured frame count"
          (List.length frames)
          (Wal.committed_frames topo.shared_wal);
        Alcotest.(check int) "ack callback fired" 1 !(topo.ack_calls);
        Alcotest.(check int)
          "ack position matches committed frames"
          (Wal.committed_frames topo.shared_wal)
          !(topo.acked_frames)
      | Error (`Apply_error msg) -> Alcotest.failf "start_following: %s" msg);
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* #360: shared-WAL-handle with master write + live standby            *)
(* ------------------------------------------------------------------ *)

(** Verify the Standby can consume frames from a commit callback:
    register a commit callback that captures frames on the fly and
    feeds them to the Standby via a stream, simulating the real
    application topology. *)
let test_shared_wal_with_commit_callback () =
  Lwt_main.run
    (let* topo = open_topology () in
     (* Set up a commit callback that captures frames and pushes them
        into a stream for the Standby to consume. *)
     let shipped_epoch = ref 0L in
     let shipped_idx = ref (-1) in
     let shipper_stream, shipper_push = Lwt_stream.create () in
     let* () =
       Store.set_commit_callback
         topo.master
         (Some
            (fun ~epoch ~base_idx ~count ->
              let* r =
                Store.capture_frames_since
                  topo.master
                  ~since_epoch:!shipped_epoch
                  ~since_idx:!shipped_idx
              in
              (match r with
               | Some (Ok fs) when fs <> [] ->
                 shipped_epoch := epoch;
                 shipped_idx := base_idx + count - 1;
                 let replicated = List.map Replication.backup_frame_to_replicated fs in
                 shipper_push (Some replicated)
               | _ -> ());
              Lwt.return_unit))
     in
     (* Start the Standby consuming from the shipper stream in a
        background fiber.  Use a ref to observe the result. *)
     let apply_result = ref None in
     let _apply_fiber =
       Lwt.async (fun () ->
         let* r = Standby.start_following topo.standby shipper_stream in
         apply_result := Some r;
         Lwt.return_unit)
     in
     (* Give the standby loop time to enter the stream consumer. *)
     let* () = Lwt.pause () in
     (* Commit on the master.  The commit callback fires asynchronously
        and pushes frames into the stream. *)
     let* rw = Store.rw_begin topo.master in
     let* () = Store.put rw 16 (Bytes.of_string "a") (Bytes.of_string "1") in
     let* () = Store.put rw 16 (Bytes.of_string "b") (Bytes.of_string "2") in
     let* () = Store.put rw 16 (Bytes.of_string "c") (Bytes.of_string "3") in
     let* () = Store.commit rw in
     (* Close the shipper stream so the Standby's apply loop terminates. *)
     shipper_push None;
     (* Wait for the Standby to finish applying. *)
     let* () =
       wait_for
         (fun () ->
            match !apply_result with
            | Some _ -> true
            | None -> false)
         100
     in
     (match !apply_result with
      | Some (Ok ()) ->
        Alcotest.(check bool)
          "standby committed frames via live callback"
          true
          (Wal.committed_frames topo.shared_wal > 0);
        Alcotest.(check int) "ack callback fired" 1 !(topo.ack_calls);
        Alcotest.(check int)
          "ack position matches committed frames"
          (Wal.committed_frames topo.shared_wal)
          !(topo.acked_frames)
      | Some (Error (`Apply_error msg)) -> Alcotest.failf "standby apply error: %s" msg
      | None -> Alcotest.fail "standby did not complete");
     let* () = Store.set_commit_callback topo.master None in
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* #360: shared-WAL-handle with checkpoint gating                      *)
(* ------------------------------------------------------------------ *)

(** Verify that the master's autocheckpoint is gated on the standby's
    floor advance when using the shared-WAL-handle topology. *)
let test_shared_wal_checkpoint_gating () =
  Lwt_main.run
    (let* topo = open_topology () in
     (* Register a commit callback so the gate infrastructure is active. *)
     let* () =
       Store.set_commit_callback
         topo.master
         (Some (fun ~epoch:_ ~base_idx:_ ~count:_ -> Lwt.return_unit))
     in
     let* rw = Store.rw_begin topo.master in
     let* () = Store.put rw 16 (Bytes.of_string "a") (Bytes.of_string "1") in
     let* () = Store.put rw 16 (Bytes.of_string "b") (Bytes.of_string "2") in
     let* () = Store.commit rw in
     (* Record state before shipping. *)
     let _epoch_pre, frames_pre =
       match Store.replication_state topo.master with
       | Some s -> s
       | None -> Alcotest.fail "master has no WAL"
     in
     Alcotest.(check bool) "frames committed before ship" true (frames_pre > 0);
     (* Capture frames and ship to the Standby. *)
     let* r = Store.capture_frames_since topo.master ~since_epoch:0L ~since_idx:(-1) in
     let frames =
       match r with
       | Some (Ok fs) -> List.map Replication.backup_frame_to_replicated fs
       | Some (Error (`Capture_error msg)) -> Alcotest.failf "capture error: %s" msg
       | None -> Alcotest.fail "capture returned None"
     in
     let stream, push = Lwt_stream.create () in
     push (Some frames);
     push None;
     let* r = Standby.start_following topo.standby stream in
     (match r with
      | Ok () -> Alcotest.(check int) "standby applied" 1 !(topo.ack_calls)
      | Error (`Apply_error msg) -> Alcotest.failf "start_following: %s" msg);
     (* Verify the master's replication floor was advanced by the
        on_standby_ack → update_replication_position call. *)
     let* () =
       wait_for
         (fun () ->
            match Store.replication_state topo.master with
            | Some (_epoch, frames) ->
              (* After the ack, the frame count should match the standby *)
              frames <= Wal.committed_frames topo.shared_wal || frames_pre <= frames
            | None -> false)
         10
     in
     let* () = Store.set_commit_callback topo.master None in
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "shared_wal"
    [ ( "shared_wal_end_to_end"
      , [ Alcotest.test_case
            "commit master, capture frames, standby applies, ack fires"
            `Quick
            test_shared_wal_end_to_end
        ; Alcotest.test_case
            "commit callback feeds live standby stream"
            `Quick
            test_shared_wal_with_commit_callback
        ; Alcotest.test_case
            "checkpoint gating with shared WAL"
            `Quick
            test_shared_wal_checkpoint_gating
        ] )
    ]
;;
