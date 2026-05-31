(** Tests for the standby follower driver (#172). *)

open Lwt.Syntax
module Wal = Sqlocaml_storage.Wal
module Replication = Sqlocaml_replication.Replication
module Standby = Sqlocaml_replication.Standby
module Store = Sqlocaml_store.Store
module Pager = Sqlocaml_storage.Pager

(* ------------------------------------------------------------------ *)
(* In-memory device (same as test_replication.ml)                      *)
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
  let tmp = Bytes.create len in
  Cstruct.blit_to_bytes src 0 tmp 0 len;
  Bytes.blit tmp 0 d.buf off len;
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
  let tmp = Bytes.create len in
  Cstruct.blit_to_bytes src 0 tmp 0 len;
  Bytes.blit tmp 0 d.buf off len;
  Lwt.return (Ok ())
;;

let sync_ok () = Lwt.return (Ok ())

let fresh_wal ?(size = 65536) () =
  let d = mk_dev size in
  let* r =
    Wal.open_
      ~read_at:(read_at d)
      ~write_at:(write_at d)
      ~sync:sync_ok
      ~size_bytes:(dev_size d)
      ()
  in
  match r with
  | Ok w -> Lwt.return (d, w)
  | Error e -> Alcotest.failf "fresh_wal: %a" Wal.pp_error e
;;

let page_with c =
  let p = Cstruct.create 4096 in
  Cstruct.memset p (Char.code c);
  p
;;

let make_frame ~epoch ~frame_idx ~page_id ~is_commit ~page =
  let flags = if is_commit then 1L else 0L in
  let checksum = Wal.frame_checksum ~salt:1L ~seed:2L ~page_id ~flags ~page in
  Replication.
    { epoch
    ; frame_idx
    ; page_id
    ; is_commit
    ; page
    ; checksum
    ; source_salt = 1L
    ; source_seed = 2L
    }
;;

let minimal_pager ?(n_pages = 10L) () =
  Pager.create
    ~read_page:(fun ~page_id:_ _ -> Lwt.return (Error "not needed"))
    ~write_page:(fun ~page_id:_ _ -> Lwt.return (Error "not needed"))
    ~sync:sync_ok
    ~resize:(fun ~n_pages:_ -> Lwt.return (Ok ()))
    ~n_pages
    ~freelist:Sqlocaml_storage.Freelist.empty
;;

(* A write-capable pager backed by an in-memory main device.  Needed for any
   test that exercises a checkpoint (epoch change or promotion), since
   checkpointing migrates WAL frames to the main DB via [flush_one_to_main]. *)
let writable_pager ?(n_pages = 16L) main_d () =
  Pager.create
    ~read_page:(read_page main_d)
    ~write_page:(write_page main_d)
    ~sync:sync_ok
    ~resize:(fun ~n_pages:_ -> Lwt.return (Ok ()))
    ~n_pages
    ~freelist:Sqlocaml_storage.Freelist.empty
;;

(* Read the first byte of a main-DB page directly off the device, so tests can
   assert what was (or was not) migrated to main. *)
let main_page_byte main_d page_id =
  let out = Cstruct.create 4096 in
  let* r = read_page main_d ~page_id out in
  match r with
  | Ok () -> Lwt.return (Cstruct.get_char out 0)
  | Error e -> Alcotest.failf "main_page_byte: %s" e
;;

(* ------------------------------------------------------------------ *)
(* Helper: create a store in follower mode for testing                 *)
(* ------------------------------------------------------------------ *)

let store_with_wal () =
  let open Sqlocaml_store.Store in
  let main_d = mk_dev 65536 in
  let wal_d = mk_dev 65536 in
  let* r =
    open_block_wal
      ~read_page:(read_page main_d)
      ~write_page:(write_page main_d)
      ~sync:sync_ok
      ~resize:(fun ~n_pages:_ -> Lwt.return (Ok ()))
      ~n_pages:(dev_size main_d)
      ~wal_read_at:(read_at wal_d)
      ~wal_write_at:(write_at wal_d)
      ~wal_sync:sync_ok
      ~wal_size_bytes:(dev_size wal_d)
      ~close:(fun () -> Lwt.return_unit)
      ~wal_close:(fun () -> Lwt.return_unit)
      ()
  in
  match r with
  | Ok store -> Lwt.return (store, wal_d, main_d)
  | Error e -> Alcotest.failf "store_with_wal: %a" pp_error e
;;

(* ------------------------------------------------------------------ *)
(* Follower-mode gate tests                                            *)
(* ------------------------------------------------------------------ *)

let test_follower_rejects_rw_begin () =
  try
    Lwt_main.run
      (let* store, _, _ = store_with_wal () in
       Store.set_follower store true;
       Alcotest.(check bool) "is_follower" true (Store.is_follower store);
       let* _ = Store.rw_begin store in
       Alcotest.fail "rw_begin should have been rejected")
  with
  | Failure _ -> ()
;;

let test_follower_allows_rw_begin_after_clear () =
  Lwt_main.run
    (let* store, _, _ = store_with_wal () in
     Store.set_follower store true;
     Store.set_follower store false;
     Alcotest.(check bool) "is_follower false" false (Store.is_follower store);
     let* _ = Store.rw_begin store in
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* epoch-aware apply tests                                             *)
(* ------------------------------------------------------------------ *)

let test_epoch_aware_same_epoch () =
  Lwt_main.run
    (let* _, wal = fresh_wal () in
     let pager = minimal_pager () in
     let frames =
       [ make_frame
           ~epoch:0L
           ~frame_idx:0
           ~page_id:1L
           ~is_commit:true
           ~page:(page_with 'A')
       ]
     in
     let* r =
       Replication.apply_frames_epoch_aware
         ~wal
         ~pager
         ~last_epoch:0L
         ~last_idx:(-1)
         frames
     in
     (match r with
      | Ok (epoch, idx) ->
        Alcotest.(check int64) "epoch 0" 0L epoch;
        Alcotest.(check int) "frame_idx 0" 0 idx;
        Alcotest.(check int) "committed_frames" 1 (Wal.committed_frames wal)
      | Error (`Apply_error msg) -> Alcotest.failf "apply: %s" msg);
     Lwt.return_unit)
;;

let test_epoch_aware_epoch_change_triggers_checkpoint () =
  Lwt_main.run
    (let* _, wal = fresh_wal () in
     let main_d = mk_dev 65536 in
     let pager = writable_pager main_d () in
     (* Apply epoch 0 batch *)
     let frames0 =
       [ make_frame
           ~epoch:0L
           ~frame_idx:0
           ~page_id:1L
           ~is_commit:true
           ~page:(page_with 'A')
       ]
     in
     let* r0 =
       Replication.apply_frames_epoch_aware
         ~wal
         ~pager
         ~last_epoch:0L
         ~last_idx:(-1)
         frames0
     in
     (match r0 with
      | Ok (_, _) -> ()
      | Error _ -> Alcotest.fail "epoch 0 apply failed");
     (* Same epoch so far: the page lives in the WAL, not yet in main. *)
     let* before = main_page_byte main_d 1L in
     Alcotest.(check char) "page not yet migrated before epoch change" '\x00' before;
     let frames1 =
       [ make_frame
           ~epoch:1L
           ~frame_idx:0
           ~page_id:2L
           ~is_commit:true
           ~page:(page_with 'B')
       ]
     in
     let* r1 =
       Replication.apply_frames_epoch_aware ~wal ~pager ~last_epoch:0L ~last_idx:0 frames1
     in
     match r1 with
     | Ok (epoch, idx) ->
       Alcotest.(check int64) "epoch 1" 1L epoch;
       Alcotest.(check int) "frame_idx 0 new epoch" 0 idx;
       Alcotest.(check int)
         "committed_frames after epoch change"
         1
         (Wal.committed_frames wal);
       (* The epoch-change checkpoint must have migrated the epoch-0 page. *)
       let* migrated = main_page_byte main_d 1L in
       Alcotest.(check char) "epoch-0 page migrated to main on epoch change" 'A' migrated;
       Lwt.return_unit
     | Error (`Apply_error msg) -> Alcotest.failf "apply: %s" msg)
;;

let test_epoch_aware_empty_frames () =
  Lwt_main.run
    (let* _, wal = fresh_wal () in
     let pager = minimal_pager () in
     let* r =
       Replication.apply_frames_epoch_aware ~wal ~pager ~last_epoch:0L ~last_idx:5 []
     in
     (match r with
      | Ok (epoch, idx) ->
        Alcotest.(check int64) "epoch unchanged" 0L epoch;
        Alcotest.(check int) "idx unchanged" 5 idx;
        Alcotest.(check int) "no frames committed" 0 (Wal.committed_frames wal)
      | Error (`Apply_error msg) -> Alcotest.failf "apply: %s" msg);
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Standby create, start_following, promote                            *)
(* ------------------------------------------------------------------ *)

let test_standby_create_mode () =
  Lwt_main.run
    (let* _, wal = fresh_wal () in
     let pager = minimal_pager () in
     let store = Store.create () in
     let st = Standby.create ~store ~pager ~wal in
     Alcotest.(check bool) "initial mode Following" true (Standby.mode st = Following);
     let pos = Standby.acked_position st in
     Alcotest.(check int64) "initial epoch 0" 0L pos.epoch;
     Alcotest.(check int) "initial frame_idx -1" (-1) pos.frame_idx;
     Lwt.return_unit)
;;

let test_standby_promote () =
  Lwt_main.run
    (let* store, _, _ = store_with_wal () in
     let pager = minimal_pager () in
     let wal_d = mk_dev 65536 in
     let* wr =
       Wal.open_
         ~read_at:(read_at wal_d)
         ~write_at:(write_at wal_d)
         ~sync:sync_ok
         ~size_bytes:(dev_size wal_d)
         ()
     in
     match wr with
     | Error e -> Alcotest.failf "wal open: %a" Wal.pp_error e
     | Ok wal ->
       let st = Standby.create ~store ~pager ~wal in
       Alcotest.(check bool) "mode Following" true (Standby.mode st = Following);
       let* () = Standby.promote st in
       Alcotest.(check bool) "mode Promoted" true (Standby.mode st = Promoted);
       Alcotest.(check bool) "follower cleared" false (Store.is_follower store);
       let* () = Standby.promote st in
       Alcotest.(check bool)
         "still Promoted after double promote"
         true
         (Standby.mode st = Promoted);
       Lwt.return_unit)
;;

let test_standby_start_following_applies_frames () =
  Lwt_main.run
    (let* _, wal = fresh_wal () in
     let pager = minimal_pager () in
     let store = Store.create () in
     let st = Standby.create ~store ~pager ~wal in
     let frames =
       [ make_frame
           ~epoch:0L
           ~frame_idx:0
           ~page_id:1L
           ~is_commit:true
           ~page:(page_with 'A')
       ]
     in
     let stream, push = Lwt_stream.create () in
     push (Some frames);
     push None;
     let* r = Standby.start_following st stream in
     (match r with
      | Ok () ->
        let pos = Standby.acked_position st in
        Alcotest.(check int64) "epoch after apply" 0L pos.epoch;
        Alcotest.(check int) "frame_idx after apply" 0 pos.frame_idx;
        Alcotest.(check int) "wal committed_frames" 1 (Wal.committed_frames wal)
      | Error (`Apply_error msg) -> Alcotest.failf "start_following: %s" msg);
     Lwt.return_unit)
;;

let test_standby_promote_drains_wal_to_main () =
  Lwt_main.run
    (let* _, wal = fresh_wal () in
     let main_d = mk_dev 65536 in
     let pager = writable_pager main_d () in
     let store = Store.create () in
     let st = Standby.create ~store ~pager ~wal in
     let frames =
       [ make_frame
           ~epoch:0L
           ~frame_idx:0
           ~page_id:3L
           ~is_commit:true
           ~page:(page_with 'C')
       ]
     in
     let stream, push = Lwt_stream.create () in
     push (Some frames);
     push None;
     let* r = Standby.start_following st stream in
     (match r with
      | Ok () -> ()
      | Error (`Apply_error msg) -> Alcotest.failf "start_following: %s" msg);
     (* Applied to the WAL but, being committed only via append_commit, not
        yet present in the main DB. *)
     let* before = main_page_byte main_d 3L in
     Alcotest.(check char) "page only in WAL before promote" '\x00' before;
     Alcotest.(check int) "wal holds the committed frame" 1 (Wal.committed_frames wal);
     let* () = Standby.promote st in
     Alcotest.(check bool) "mode Promoted" true (Standby.mode st = Promoted);
     (* Promotion must have drained the WAL to main and reset it. *)
     let* after = main_page_byte main_d 3L in
     Alcotest.(check char) "committed frame drained to main on promote" 'C' after;
     Alcotest.(check int) "wal reset after promote" 0 (Wal.committed_frames wal);
     Lwt.return_unit)
;;

let test_standby_follower_mode_enforced_during_loop () =
  Lwt_main.run
    (let* store, _, _ = store_with_wal () in
     let pager = minimal_pager () in
     let wal_d = mk_dev 65536 in
     let* wr =
       Wal.open_
         ~read_at:(read_at wal_d)
         ~write_at:(write_at wal_d)
         ~sync:sync_ok
         ~size_bytes:(dev_size wal_d)
         ()
     in
     match wr with
     | Error e -> Alcotest.failf "wal open: %a" Wal.pp_error e
     | Ok wal ->
       let st = Standby.create ~store ~pager ~wal in
       let stream, push = Lwt_stream.create () in
       (* Start the follower loop and immediately end it *)
       push None;
       let* r = Standby.start_following st stream in
       (match r with
        | Ok () ->
          (* After the loop exits (not promoted), follower mode should be cleared *)
          Alcotest.(check bool)
            "follower cleared after loop exit"
            false
            (Store.is_follower store)
        | Error _ -> Alcotest.fail "start_following should have returned Ok");
       Lwt.return_unit)
;;

let () =
  Alcotest.run
    "standby"
    [ ( "follower_mode"
      , [ Alcotest.test_case "rejects rw_begin" `Quick test_follower_rejects_rw_begin
        ; Alcotest.test_case
            "allows rw_begin after clear"
            `Quick
            test_follower_allows_rw_begin_after_clear
        ] )
    ; ( "epoch_aware_apply"
      , [ Alcotest.test_case "same epoch" `Quick test_epoch_aware_same_epoch
        ; Alcotest.test_case
            "epoch change triggers checkpoint"
            `Quick
            test_epoch_aware_epoch_change_triggers_checkpoint
        ; Alcotest.test_case "empty frames" `Quick test_epoch_aware_empty_frames
        ] )
    ; ( "standby_lifecycle"
      , [ Alcotest.test_case "create mode" `Quick test_standby_create_mode
        ; Alcotest.test_case "promote" `Quick test_standby_promote
        ; Alcotest.test_case
            "promote drains wal to main"
            `Quick
            test_standby_promote_drains_wal_to_main
        ; Alcotest.test_case
            "start_following applies frames"
            `Quick
            test_standby_start_following_applies_frames
        ; Alcotest.test_case
            "follower mode enforced during loop"
            `Quick
            test_standby_follower_mode_enforced_during_loop
        ] )
    ]
;;
