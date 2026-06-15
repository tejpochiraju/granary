(** End-to-end test for the shared-WAL-device scenario (#360).

    Exercises the full master–standby lifecycle where the master Store and
    the Standby open separate {!Wal.t} handles over the same WAL device.
    The master commits data, frames are captured and fed to the Standby via
    the apply loop, and the {!Standby.on_standby_ack} callback advances the
    master's replication floor through {!Store.update_replication_position}. *)

open Lwt.Syntax
module Wal = Sqlocaml_storage.Wal
module Replication = Sqlocaml_replication.Replication
module Standby = Sqlocaml_replication.Standby
module Store = Sqlocaml_store.Store
module Pager = Sqlocaml_storage.Pager
module Db = Sqlocaml.Db
module Row = Sqlocaml_encoding.Row

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
(* #360: shared-WAL-device end-to-end test                             *)
(* ------------------------------------------------------------------ *)

(** 1. The master commits Store transactions.
    2. Frames are captured from the master store.
    3. The Standby applies them via its WAL handle over the same device.
    4. The {!on_standby_ack} callback fires and advances the master's
       replication floor.
    5. Data is visible through the standby WAL handle. *)
let test_master_commit_standby_applies () =
  Lwt_main.run
    (let* topo = open_topology () in
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
     let* r = Store.capture_frames_since topo.master ~since_epoch:0L ~since_idx:(-1) in
     let frames =
       match r with
       | Some (Ok fs) -> List.map Replication.backup_frame_to_replicated fs
       | Some (Error (`Capture_error msg)) -> Alcotest.failf "capture error: %s" msg
       | None -> Alcotest.fail "capture_frames_since returned None (epoch changed)"
     in
     Alcotest.(check bool) "captured at least one frame" true (frames <> []);
     let stream, push = Lwt_stream.create () in
     push (Some frames);
     push None;
     let* r = Standby.start_following topo.standby stream in
     (match r with
      | Ok () ->
        Alcotest.(check bool)
          "standby committed frames via standalone WAL handle"
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
(* #360: shared-WAL-device with commit callback + live standby         *)
(* ------------------------------------------------------------------ *)

(** Verify the Standby can consume frames captured via the commit
    callback.  The callback stores captured frames in a ref; after
    the commit the main fiber reads the ref and feeds them to the
    Standby.  This explicitly avoids the ordering dependency of the
    callback pushing directly into a live stream (the callback fires
    via {!Lwt.async} and may not have completed when the main fiber
    reaches the next line). *)
let test_commit_callback_feeds_standby () =
  Lwt_main.run
    (let* topo = open_topology () in
     let shipped_epoch = ref 0L in
     let shipped_idx = ref (-1) in
     let captured_ref = ref [] in
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
                 captured_ref := List.map Replication.backup_frame_to_replicated fs
               | _ -> ());
              Lwt.return_unit))
     in
     let* rw = Store.rw_begin topo.master in
     let* () = Store.put rw 16 (Bytes.of_string "a") (Bytes.of_string "1") in
     let* () = Store.put rw 16 (Bytes.of_string "b") (Bytes.of_string "2") in
     let* () = Store.put rw 16 (Bytes.of_string "c") (Bytes.of_string "3") in
     let* () = Store.commit rw in
     let* () = wait_for (fun () -> !captured_ref <> []) 50 in
     let stream, push = Lwt_stream.create () in
     push (Some !captured_ref);
     push None;
     let* r = Standby.start_following topo.standby stream in
     (match r with
      | Ok () ->
        Alcotest.(check bool)
          "standby committed frames via live callback"
          true
          (Wal.committed_frames topo.shared_wal > 0);
        Alcotest.(check int) "ack callback fired" 1 !(topo.ack_calls);
        Alcotest.(check int)
          "ack position matches committed frames"
          (Wal.committed_frames topo.shared_wal)
          !(topo.acked_frames)
      | Error (`Apply_error msg) -> Alcotest.failf "start_following: %s" msg);
     let* () = Store.set_commit_callback topo.master None in
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* #360: shared-WAL-device checkpoint gating                           *)
(* ------------------------------------------------------------------ *)

(** Verify that the master's autocheckpoint is gated on the standby's
    floor advance:
    1. Register a commit callback to activate the gate infrastructure.
    2. Set autocheckpoint to 2 frames and commit 2+ keys.
       The autocheckpoint dispatches but parks waiting for the floor.
    3. Ship frames to the Standby.
    4. The {!on_standby_ack} fires {!Store.update_replication_position},
       advancing the floor.
    5. The checkpoint completes: master epoch advances. *)
let test_checkpoint_gating_via_standby_ack () =
  Lwt_main.run
    (let* topo = open_topology () in
     let* () =
       Store.set_commit_callback
         topo.master
         (Some (fun ~epoch:_ ~base_idx:_ ~count:_ -> Lwt.return_unit))
     in
     Store.set_wal_autocheckpoint topo.master 2;
     let* rw = Store.rw_begin topo.master in
     let* () = Store.put rw 16 (Bytes.of_string "a") (Bytes.of_string "1") in
     let* () = Store.put rw 16 (Bytes.of_string "b") (Bytes.of_string "2") in
     let* () = Store.commit rw in
     (* Yield so the autocheckpoint async fiber can start parking. *)
     let* () = Lwt.pause () in
     let* () = Lwt.pause () in
     let epoch_pre, frames_pre =
       match Store.replication_state topo.master with
       | Some s -> s
       | None -> Alcotest.fail "master has no WAL"
     in
     Alcotest.(check bool)
       "checkpoint has not reset the WAL (epoch unchanged)"
       true
       (epoch_pre = 0L);
     Alcotest.(check bool) "frames committed before ship" true (frames_pre > 0);
     let* r = Store.capture_frames_since topo.master ~since_epoch:0L ~since_idx:(-1) in
     let frames =
       match r with
       | Some (Ok fs) -> List.map Replication.backup_frame_to_replicated fs
       | Some (Error (`Capture_error msg)) -> Alcotest.failf "capture error: %s" msg
       | None -> Alcotest.fail "capture returned None (checkpoint advanced before ship)"
     in
     let stream, push = Lwt_stream.create () in
     push (Some frames);
     push None;
     let* r = Standby.start_following topo.standby stream in
     (match r with
      | Ok () -> Alcotest.(check int) "standby ack callback fired" 1 !(topo.ack_calls)
      | Error (`Apply_error msg) -> Alcotest.failf "start_following: %s" msg);
     let* () =
       wait_for
         (fun () ->
            match Store.replication_state topo.master with
            | Some (epoch, _frames) -> epoch > epoch_pre
            | None -> false)
         50
     in
     let epoch_post, _frames_post =
       match Store.replication_state topo.master with
       | Some s -> s
       | None -> Alcotest.fail "master has no WAL"
     in
     Alcotest.(check bool)
       "checkpoint completed after floor advance (epoch bumped)"
       true
       (epoch_post > epoch_pre);
     let* () = Store.set_commit_callback topo.master None in
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* #368 / T3: verify columnar data survives WAL replay on standby      *)
(* ------------------------------------------------------------------ *)

(** End-to-end test: create columnar data on the master via SQL, ship
    WAL frames to the standby via the apply loop, then open a fresh
    Store+Db on the shared devices and verify the columnar data is
    visible. *)
let test_columnar_replication_via_wal () =
  Lwt_main.run
    (let main_dev = mk_dev (1024 * 4096) in
     let wal_dev = mk_dev 65536 in
     (* Shared WAL + main device, same pattern as open_topology. *)
     let* wal_r =
       Wal.open_
         ~read_at:(read_at wal_dev)
         ~write_at:(write_at wal_dev)
         ~sync:sync_ok
         ~size_bytes:(dev_size wal_dev)
         ()
     in
     let wal =
       match wal_r with
       | Ok w -> w
       | Error e -> Alcotest.failf "wal open: %a" Wal.pp_error e
     in
     let main_n_pages = Int64.of_int (Bytes.length main_dev.buf / 4096) in
     let* master_sr =
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
     let master_store =
       match master_sr with
       | Ok s -> s
       | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
     in
     let exec db sql =
       let* r = Db.execute db sql in
       match r with
       | Ok () -> Lwt.return_unit
       | Error e -> Alcotest.failf "exec: %a" Db.pp_error e
     in
     let query db sql =
       let* r = Db.query db sql in
       match r with
       | Ok stream -> Lwt_stream.to_list stream
       | Error e -> Alcotest.failf "query: %a" Db.pp_error e
     in
     (* Master: create columnar table + insert data. *)
     let* master_db = Db.of_store master_store in
     let* () = exec master_db "CREATE TABLE t (a INTEGER, b TEXT) USING COLUMNSTORE" in
     let* () = exec master_db "INSERT INTO t VALUES (10, 'hello'), (20, 'world')" in
     let* rows = query master_db "SELECT COUNT(*) FROM t" in
     (match rows with
      | [ [| Row.V_int 2L |] ] -> ()
      | _ -> Alcotest.fail "master insert failed");
     (* Capture WAL frames. *)
     let* () =
       wait_for
         (fun () ->
            match Store.replication_state master_store with
            | Some (_epoch, frames) -> frames > 0
            | None -> false)
         50
     in
     let* capt_r =
       Store.capture_frames_since master_store ~since_epoch:0L ~since_idx:(-1)
     in
     let frames =
       match capt_r with
       | Some (Ok fs) -> List.map Replication.backup_frame_to_replicated fs
       | _ -> Alcotest.fail "capture frames failed"
     in
     Alcotest.(check bool) "captured frames" true (frames <> []);
     (* Feed frames to a standby (uses the shared wal device). *)
     let standby_store = Store.create () in
     let pager = writable_pager main_dev () in
     let ack_called = ref false in
     let standby =
       Standby.create
         ~store:standby_store
         ~pager
         ~wal
         ~on_standby_ack:(fun _ -> ack_called := true)
         ()
     in
     let stream, push = Lwt_stream.create () in
     push (Some frames);
     push None;
     let* apply_r = Standby.start_following standby stream in
     (match apply_r with
      | Ok () -> Alcotest.(check bool) "standby ack fired" true !ack_called
      | Error (`Apply_error msg) -> Alcotest.failf "start_following: %s" msg);
     (* Open a FRESH Store+Db on the shared devices — simulates a new
        connection or a standby promotion that recovers the WAL. *)
     let* fresh_sr =
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
     let fresh_store =
       match fresh_sr with
       | Ok s -> s
       | Error e -> Alcotest.failf "fresh open_block_wal: %a" Store.pp_error e
     in
     let* fresh_db = Db.of_store fresh_store in
     let* fresh_rows = query fresh_db "SELECT a, b FROM t ORDER BY a" in
     Alcotest.(check int)
       "standby sees 2 columnar rows via fresh store"
       2
       (List.length fresh_rows);
     (match fresh_rows with
      | [ [| Row.V_int 10L; Row.V_text "hello" |]
        ; [| Row.V_int 20L; Row.V_text "world" |]
        ] -> ()
      | _ -> Alcotest.fail "unexpected columnar data on fresh-store standby");
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "shared_wal"
    [ ( "shared_wal"
      , [ Alcotest.test_case
            "master commit → standby apply → ack fires"
            `Quick
            test_master_commit_standby_applies
        ; Alcotest.test_case
            "commit callback captures frames for standby"
            `Quick
            test_commit_callback_feeds_standby
        ; Alcotest.test_case
            "checkpoint gating via standby ack"
            `Quick
            test_checkpoint_gating_via_standby_ack
        ; Alcotest.test_case
            "columnar data survives WAL replay on fresh store"
            `Quick
            test_columnar_replication_via_wal
        ] )
    ]
;;
