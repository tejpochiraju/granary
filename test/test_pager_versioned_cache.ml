(** Phase 38 / #149 — Versioned page cache safety tests.

    Verify:
    1. A reader's [Pager.read ~snapshot_frames:n] only sees frames < n.
    2. The writer's [Pager.write] does NOT contaminate the shared cache
       — a concurrent reader still sees pre-write content. *)

open Lwt.Syntax
module Pager = Granary_storage.Pager

let mkdev n_pages =
  let bytes = Bytes.make (n_pages * 4096) '\x00' in
  let read_page ~page_id buf =
    let off = Int64.to_int page_id * 4096 in
    Cstruct.blit_from_bytes bytes off buf 0 4096;
    Lwt.return_ok ()
  in
  let write_page ~page_id buf =
    let off = Int64.to_int page_id * 4096 in
    Cstruct.blit_to_bytes buf 0 bytes off 4096;
    Lwt.return_ok ()
  in
  let sync () = Lwt.return_ok () in
  let resize ~n_pages:_ = Lwt.return_ok () in
  read_page, write_page, sync, resize, bytes
;;

let with_byte b =
  let c = Cstruct.create 4096 in
  Cstruct.memset c b;
  c
;;

let read_byte = function
  | Ok c -> Cstruct.get_uint8 c 0
  | Error _ -> failwith "read failed"
;;

(* Tiny in-process WAL stub. *)
type stub =
  { mutable frames : (int64 * Cstruct.t) array
  ; mutable committed : int
  }

let make_stub_callbacks (s : stub) : Pager.wal_callbacks =
  let find_page pid =
    let r = ref None in
    Array.iteri
      (fun i (p, _) -> if Int64.equal p pid && i < s.committed then r := Some i)
      s.frames;
    !r
  in
  let find_page_at pid ~max_frame =
    let r = ref None in
    Array.iteri
      (fun i (p, _) ->
         if Int64.equal p pid && i < s.committed && i < max_frame then r := Some i)
      s.frames;
    !r
  in
  let read_frame i =
    let _, page = s.frames.(i) in
    let c = Cstruct.create 4096 in
    Cstruct.blit page 0 c 0 4096;
    Lwt.return_ok c
  in
  let append_commit pages =
    s.frames <- Array.append s.frames (Array.of_list pages);
    s.committed <- Array.length s.frames;
    Lwt.return_ok ()
  in
  let append_commit_no_sync = append_commit in
  let wal_sync () = Lwt.return_ok () in
  { wal_find_page = find_page
  ; wal_find_page_at = find_page_at
  ; wal_read_frame = read_frame
  ; wal_append_commit = append_commit
  ; wal_append_commit_no_sync = append_commit_no_sync
  ; wal_sync
  }
;;

let test_snapshot_reads_isolated_from_newer_frames () =
  Lwt_main.run
    (let read_page, write_page, sync, resize, _ = mkdev 4 in
     let pager =
       Pager.create
         ~read_page
         ~write_page
         ~sync
         ~resize
         ~n_pages:4L
         ~freelist:Granary_storage.Freelist.empty
     in
     let stub = { frames = [||]; committed = 0 } in
     Pager.set_wal pager (Some (make_stub_callbacks stub));
     (* Seed main-DB page 1 with byte 0x10 by direct write_page. *)
     let* _ = write_page ~page_id:1L (with_byte 0x10) in
     (* Reader captures snapshot = 0 committed_frames. *)
     let snap0 = 0 in
     let* r1 = Pager.read ~snapshot_frames:snap0 pager 1L in
     Alcotest.(check int) "snapshot=0 sees main" 0x10 (read_byte r1);
     (* Writer (no snapshot) writes new content, then commits via WAL. *)
     Pager.write pager 1L (with_byte 0xFE);
     let* fr = Pager.flush pager in
     (match fr with
      | Ok () -> ()
      | Error _ -> Alcotest.fail "flush");
     (* Older reader still sees main. *)
     let* r2 = Pager.read ~snapshot_frames:snap0 pager 1L in
     Alcotest.(check int) "older snapshot still sees main" 0x10 (read_byte r2);
     (* Newer reader with snapshot=1 sees frame 0. *)
     let* r3 = Pager.read ~snapshot_frames:1 pager 1L in
     Alcotest.(check int) "newer snapshot sees frame 0" 0xFE (read_byte r3);
     Lwt.return_unit)
;;

let test_writer_dirty_invisible_to_concurrent_readers () =
  Lwt_main.run
    (let read_page, write_page, sync, resize, _ = mkdev 4 in
     let pager =
       Pager.create
         ~read_page
         ~write_page
         ~sync
         ~resize
         ~n_pages:4L
         ~freelist:Granary_storage.Freelist.empty
     in
     let stub = { frames = [||]; committed = 0 } in
     Pager.set_wal pager (Some (make_stub_callbacks stub));
     let* _ = write_page ~page_id:2L (with_byte 0x20) in
     (* Writer dirties the page (no commit yet). *)
     Pager.write pager 2L (with_byte 0x99);
     (* Concurrent reader at snapshot 0 must NOT see 0x99. *)
     let* r = Pager.read ~snapshot_frames:0 pager 2L in
     Alcotest.(check int) "reader does not see writer dirty" 0x20 (read_byte r);
     (* Writer's own read still sees its dirty (no snapshot). *)
     let* rw = Pager.read pager 2L in
     Alcotest.(check int) "writer sees its own dirty" 0x99 (read_byte rw);
     Lwt.return_unit)
;;

let test_main_cache_invalidated_after_clear_dirty () =
  (* After clear_dirty (rollback), the main-key cache entry must be
     dropped so the next read re-fetches from main DB. *)
  Lwt_main.run
    (let read_page, write_page, sync, resize, bytes = mkdev 4 in
     let pager =
       Pager.create
         ~read_page
         ~write_page
         ~sync
         ~resize
         ~n_pages:4L
         ~freelist:Granary_storage.Freelist.empty
     in
     let* _ = write_page ~page_id:3L (with_byte 0x30) in
     let* r1 = Pager.read pager 3L in
     Alcotest.(check int) "first read" 0x30 (read_byte r1);
     (* Mutate device under the cache; without invalidation, cache would
       still report 0x30. *)
     Bytes.set bytes (3 * 4096) '\xCC';
     let* r2 = Pager.read pager 3L in
     Alcotest.(check int) "cache still serves old" 0x30 (read_byte r2);
     (* clear_dirty should not affect a page that was never dirtied,
       so the cache entry for 3L stays. *)
     Pager.clear_dirty pager;
     let* r3 = Pager.read pager 3L in
     Alcotest.(check int) "clear_dirty without dirty keeps cache" 0x30 (read_byte r3);
     (* Dirty page 3, clear_dirty, then read — must get device contents. *)
     Pager.write pager 3L (with_byte 0xAA);
     Pager.clear_dirty pager;
     let* r4 = Pager.read pager 3L in
     Alcotest.(check int)
       "clear_dirty drops main-key cache for dirtied page"
       0xCC
       (read_byte r4);
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "pager_versioned_cache"
    [ ( "snapshot"
      , [ Alcotest.test_case
            "older reader isolated from newer WAL frames"
            `Quick
            test_snapshot_reads_isolated_from_newer_frames
        ; Alcotest.test_case
            "writer dirty invisible to concurrent reader"
            `Quick
            test_writer_dirty_invisible_to_concurrent_readers
        ; Alcotest.test_case
            "main cache invalidated after clear_dirty"
            `Quick
            test_main_cache_invalidated_after_clear_dirty
        ] )
    ]
;;
