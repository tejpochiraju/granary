(** #611 — the pager cache participates for WAL-resolved pages.

    Before this change [Pager.resolve_wal_page] / [resolve_wal_page_borrow]
    went to [wal_read_frame] on {e every} access, so inside a live WAL
    generation the pager cache was inert: a point lookup re-resolved root,
    interior and leaf per lookup (3.00 resolutions/lookup, measured in
    [bench_wal_resolve_562.ml]) and a repeated full scan re-resolved
    everything.

    The win is CPU only — the WAL's own decrypted-frame cache (#246) already
    absorbed the I/O — so the entire risk of this change is {b invalidation}:
    serving one stale frame is a silent wrong-answer bug, which no counter test
    would catch. Hence this file leads with the three triggers and asserts
    {e content}, not just counters:

    - {b (a) a newer frame for the same page.} Handled by the key: WAL-resolved
      pages are cached under [(page_id, frame_idx)], so a newer frame is a
      different key and cannot hit the older entry.
    - {b (b) [Wal.reset] (a checkpoint).} NOT handled by the key — a checkpoint
      recycles frame indices, so [(page, 0)] of the new generation and
      [(page, 0)] of the old one are the same key with different bytes. Handled
      instead by [Pager.sync_wal_epoch], which compares the WAL's generation
      counter on every resolution and purges the WAL-keyed entries when it
      moves. [Wal.reset] bumps [epoch] on both of its success arms and on
      neither failure path, and nothing else in [Wal] ever clears the index, so
      the counter is exactly the "frame indices no longer mean what they meant"
      signal.
    - {b (c) a reader on an older snapshot} — [wal_find_page_at], which is the
      RO-snapshot path and the #266 as-of/time-travel path. Handled by the key
      again, but only because the key carries the frame index the {e snapshot}
      resolved to rather than the page id: a reader bounded at an older frame
      looks up the older frame, and the newest reader's entry is invisible to
      it in both directions and in either arrival order.

    Every trigger is exercised on the copying path ([Pager.read]) and the
    zero-copy borrow path ([Pager.read_borrow]), because they resolve through
    two different functions. *)

open Lwt.Syntax
module Pager = Granary_storage.Pager

let page_size = 4096

(* ------------------------------------------------------------------ *)
(* A block device and a WAL stub with an instrumented frame reader.     *)
(* ------------------------------------------------------------------ *)

let mkdev n_pages =
  let bytes = Bytes.make (n_pages * page_size) '\x00' in
  let read_page ~page_id buf =
    let off = Int64.to_int page_id * page_size in
    Cstruct.blit_from_bytes bytes off buf 0 page_size;
    Lwt.return_ok ()
  in
  let write_page ~page_id buf =
    let off = Int64.to_int page_id * page_size in
    Cstruct.blit_to_bytes buf 0 bytes off page_size;
    Lwt.return_ok ()
  in
  let sync () = Lwt.return_ok () in
  let resize ~n_pages:_ = Lwt.return_ok () in
  read_page, write_page, sync, resize, bytes
;;

let with_byte b =
  let c = Cstruct.create page_size in
  Cstruct.memset c b;
  c
;;

let byte_of = function
  | Ok c -> Cstruct.get_uint8 c 0
  | Error _ -> Alcotest.fail "pager read failed"
;;

(* A WAL stub: an append-only frame array plus a generation counter, mirroring
   the two things the pager depends on — frame indices are stable within a
   generation, and [epoch] moves exactly when they stop being. *)
type stub =
  { mutable frames : (int64 * Cstruct.t) array
  ; mutable epoch : int64
  ; mutable frame_reads : int (* how many times [wal_read_frame] ran *)
  }

let new_stub () = { frames = [||]; epoch = 0L; frame_reads = 0 }

(* Append one frame for [pid] holding [byte]; returns its frame index. *)
let append stub pid byte =
  let idx = Array.length stub.frames in
  stub.frames <- Array.append stub.frames [| pid, with_byte byte |];
  idx
;;

(* Simulate a checkpoint: the index is discarded and the generation moves, so
   the next append lands at frame 0 again — exactly [Wal.reset]. *)
let checkpoint stub =
  stub.frames <- [||];
  stub.epoch <- Int64.succ stub.epoch
;;

let callbacks (s : stub) : Pager.wal_callbacks =
  (* Latest frame for [pid], like [Wal.find_page]. *)
  let find_page pid =
    let r = ref None in
    Array.iteri (fun i (p, _) -> if Int64.equal p pid then r := Some i) s.frames;
    !r
  in
  (* Latest frame for [pid] strictly below [max_frame], like
     [Wal.find_page_at] — the RO-snapshot and #266 as-of path. *)
  let find_page_at pid ~max_frame =
    let r = ref None in
    Array.iteri
      (fun i (p, _) -> if Int64.equal p pid && i < max_frame then r := Some i)
      s.frames;
    !r
  in
  let read_frame i =
    s.frame_reads <- s.frame_reads + 1;
    let _, page = s.frames.(i) in
    (* Return the stub's own buffer: [Wal.read_frame] may hand back a buffer it
       retains in its decrypted-frame cache (#246), and the pager must be
       correct against that stronger case, not only against a fresh copy. *)
    Lwt.return_ok page
  in
  let append_commit _ = Lwt.return_ok () in
  { wal_find_page = find_page
  ; wal_find_page_at = find_page_at
  ; wal_read_frame = read_frame
  ; wal_append_commit = append_commit
  ; wal_append_commit_no_sync = append_commit
  ; wal_sync = (fun () -> Lwt.return_ok ())
  ; wal_epoch = (fun () -> s.epoch)
  }
;;

let make_pager () =
  let read_page, write_page, sync, resize, _ = mkdev 8 in
  let pager =
    Pager.create
      ~read_page
      ~write_page
      ~sync
      ~resize
      ~n_pages:8L
      ~freelist:Granary_storage.Freelist.empty
  in
  let stub = new_stub () in
  Pager.set_wal pager (Some (callbacks stub));
  pager, stub, write_page
;;

(* Migrate one page to the main file the way [Store.checkpoint_wal_to_main]
   does — through [Pager.flush_one_to_main], so the main-file cache key is
   refreshed too.  Writing behind the pager's back would leave a stale
   main-keyed entry and test the wrong thing. *)
let migrate pager page_id byte =
  let* r = Pager.flush_one_to_main pager ~page_id ~buf:(with_byte byte) in
  match r with
  | Ok () -> Lwt.return_unit
  | Error _ -> Alcotest.fail "flush_one_to_main failed"
;;

(* Borrow-read one page and hand back its first byte. *)
let borrow_byte pager ?snapshot_frames pid =
  let* r =
    Pager.read_borrow ?snapshot_frames pager pid (fun buf ->
      Lwt.return (Cstruct.get_uint8 buf 0))
  in
  match r with
  | Ok b -> Lwt.return b
  | Error _ -> Alcotest.fail "pager borrow read failed"
;;

let check_int = Alcotest.(check int)

(* ------------------------------------------------------------------ *)
(* The win: a WAL-resident page is resolved once, not once per access.  *)
(* ------------------------------------------------------------------ *)

let test_repeat_read_hits_the_cache () =
  Lwt_main.run
    (let pager, stub, _ = make_pager () in
     ignore (append stub 1L 0xA1);
     let* r1 = Pager.read pager 1L in
     check_int "first read sees the frame" 0xA1 (byte_of r1);
     check_int "first read resolves the frame" 1 stub.frame_reads;
     check_int "frame is now cached" 1 (Pager.wal_cached_count pager);
     let* r2 = Pager.read pager 1L in
     let* r3 = Pager.read pager 1L in
     check_int "second read sees the frame" 0xA1 (byte_of r2);
     check_int "third read sees the frame" 0xA1 (byte_of r3);
     check_int "no further frame resolutions" 1 stub.frame_reads;
     Lwt.return_unit)
;;

let test_repeat_borrow_hits_the_cache () =
  Lwt_main.run
    (let pager, stub, _ = make_pager () in
     ignore (append stub 2L 0xB2);
     let* b1 = borrow_byte pager 2L in
     check_int "first borrow sees the frame" 0xB2 b1;
     check_int "first borrow resolves the frame" 1 stub.frame_reads;
     let* b2 = borrow_byte pager 2L in
     check_int "second borrow sees the frame" 0xB2 b2;
     check_int "borrow path hits the cache" 1 stub.frame_reads;
     (* The two paths share one cache: a copying read after a borrow read of
        the same frame must not re-resolve either. *)
     let* r = Pager.read pager 2L in
     check_int "copying read sees the frame" 0xB2 (byte_of r);
     check_int "copying read shares the borrow path's entry" 1 stub.frame_reads;
     Lwt.return_unit)
;;

(* A cached frame must never be handed out as a mutable alias: [Pager.read]
   returns a copy, so scribbling on it cannot poison the next reader. *)
let test_copying_read_does_not_alias_the_cache () =
  Lwt_main.run
    (let pager, stub, _ = make_pager () in
     ignore (append stub 3L 0xC3);
     let* r1 = Pager.read pager 3L in
     (match r1 with
      | Ok c -> Cstruct.memset c 0xEE
      | Error _ -> Alcotest.fail "read failed");
     let* r2 = Pager.read pager 3L in
     check_int "cache survives a caller mutating its copy" 0xC3 (byte_of r2);
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Trigger (a): a newer frame for the same page.                        *)
(* ------------------------------------------------------------------ *)

let test_newer_frame_supersedes_cached_frame () =
  Lwt_main.run
    (let pager, stub, _ = make_pager () in
     ignore (append stub 1L 0x11);
     let* r1 = Pager.read pager 1L in
     check_int "reads frame 0" 0x11 (byte_of r1);
     (* A second commit for the same page lands at a new frame index. *)
     ignore (append stub 1L 0x22);
     let* r2 = Pager.read pager 1L in
     check_int "newer frame wins over the cached one" 0x22 (byte_of r2);
     check_int "the newer frame was actually resolved" 2 stub.frame_reads;
     check_int
       "both frames are cached under distinct keys"
       2
       (Pager.wal_cached_count pager);
     (* And a third, to show it is not a one-shot. *)
     ignore (append stub 1L 0x33);
     let* r3 = Pager.read pager 1L in
     check_int "newest frame wins again" 0x33 (byte_of r3);
     Lwt.return_unit)
;;

let test_newer_frame_supersedes_on_borrow_path () =
  Lwt_main.run
    (let pager, stub, _ = make_pager () in
     ignore (append stub 4L 0x44);
     let* b1 = borrow_byte pager 4L in
     check_int "borrow reads frame 0" 0x44 b1;
     ignore (append stub 4L 0x55);
     let* b2 = borrow_byte pager 4L in
     check_int "borrow sees the newer frame" 0x55 b2;
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Trigger (b): Wal.reset — a checkpoint recycles frame indices.        *)
(* ------------------------------------------------------------------ *)

(* After a checkpoint the pages live in the main file, and the WAL-keyed
   entries must be gone rather than merely unreachable: they name frame slots
   the next generation will overwrite. *)
let test_checkpoint_purges_wal_entries () =
  Lwt_main.run
    (let pager, stub, _ = make_pager () in
     ignore (append stub 1L 0x10);
     ignore (append stub 2L 0x20);
     let* _ = Pager.read pager 1L in
     let* _ = Pager.read pager 2L in
     check_int "two frames cached" 2 (Pager.wal_cached_count pager);
     (* The checkpoint migrates both pages to the main file, then resets. *)
     let* () = migrate pager 1L 0x10 in
     let* () = migrate pager 2L 0x20 in
     checkpoint stub;
     let* r1 = Pager.read pager 1L in
     check_int "page 1 now served from the main file" 0x10 (byte_of r1);
     check_int "checkpoint purged the WAL-keyed entries" 0 (Pager.wal_cached_count pager);
     Lwt.return_unit)
;;

(* The wrong-answer case the epoch guard exists for: the new generation reuses
   frame index 0 for the SAME page with DIFFERENT bytes.  The cache key alone
   cannot tell the two apart — only the generation counter can. *)
let test_recycled_frame_index_is_not_served_stale () =
  Lwt_main.run
    (let pager, stub, _ = make_pager () in
     let idx_old = append stub 1L 0x10 in
     check_int "old generation used frame 0" 0 idx_old;
     let* r1 = Pager.read pager 1L in
     check_int "cached under (page 1, frame 0)" 0x10 (byte_of r1);
     let* () = migrate pager 1L 0x10 in
     checkpoint stub;
     (* New generation, same page, same frame index, different bytes. *)
     let idx_new = append stub 1L 0x99 in
     check_int "new generation reuses frame 0" 0 idx_new;
     let* r2 = Pager.read pager 1L in
     check_int "recycled frame 0 is NOT served from the old generation" 0x99 (byte_of r2);
     (* Idempotent: the epoch is now in sync, so the new entry is cached and
        the next read hits it rather than purging again. *)
     let reads = stub.frame_reads in
     let* r3 = Pager.read pager 1L in
     check_int "new generation's frame is cached" 0x99 (byte_of r3);
     check_int "no re-resolution once the epoch is in sync" reads stub.frame_reads;
     Lwt.return_unit)
;;

let test_recycled_frame_index_on_borrow_path () =
  Lwt_main.run
    (let pager, stub, _ = make_pager () in
     ignore (append stub 5L 0x50);
     let* b1 = borrow_byte pager 5L in
     check_int "borrow caches the old generation's frame" 0x50 b1;
     let* () = migrate pager 5L 0x50 in
     checkpoint stub;
     ignore (append stub 5L 0x5F);
     let* b2 = borrow_byte pager 5L in
     check_int "borrow does not serve the recycled frame stale" 0x5F b2;
     Lwt.return_unit)
;;

(* Several generations in a row, with the page moving between frame indices,
   so a single off-by-one epoch comparison would show up. *)
let test_repeated_checkpoints_stay_correct () =
  Lwt_main.run
    (let pager, stub, _ = make_pager () in
     let rec round i =
       if i > 6
       then Lwt.return_unit
       else (
         (* Vary the frame index the page lands on within the generation. *)
         for pad = 1 to i mod 3 do
           ignore (append stub (Int64.of_int (pad + 5)) 0x00)
         done;
         ignore (append stub 1L i);
         let* r = Pager.read pager 1L in
         check_int (Printf.sprintf "generation %d serves its own bytes" i) i (byte_of r);
         let* () = migrate pager 1L i in
         checkpoint stub;
         let* rc = Pager.read pager 1L in
         check_int (Printf.sprintf "generation %d checkpointed to main" i) i (byte_of rc);
         round (i + 1))
     in
     round 1)
;;

(* Detaching the overlay must drop the WAL-keyed entries too: without a WAL
   there are no frame indices, and every page comes from the main file. *)
let test_detaching_the_wal_purges_wal_entries () =
  Lwt_main.run
    (let pager, stub, write_page = make_pager () in
     ignore (append stub 1L 0x10);
     let* r1 = Pager.read pager 1L in
     check_int "cached from the WAL" 0x10 (byte_of r1);
     let* _ = write_page ~page_id:1L (with_byte 0x77) in
     Pager.set_wal pager None;
     check_int "detach purged the WAL-keyed entries" 0 (Pager.wal_cached_count pager);
     let* r2 = Pager.read pager 1L in
     check_int "reads fall through to the main file" 0x77 (byte_of r2);
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Trigger (c): a reader on an older snapshot (#266 as-of / RO reader). *)
(* ------------------------------------------------------------------ *)

(* [~snapshot_frames] is the [wal_find_page_at] path: the RO-snapshot reader
   and the #266 as-of/time-travel reader both arrive here. The cache key must
   carry the frame the SNAPSHOT resolved to, or an as-of reader would be served
   the newest frame the moment a current reader had cached it. *)
let test_as_of_reader_is_not_served_a_newer_frame () =
  Lwt_main.run
    (let pager, stub, _ = make_pager () in
     ignore (append stub 1L 0x10);
     (* v1 at frame 0 *)
     ignore (append stub 1L 0x20);
     (* v2 at frame 1 *)
     ignore (append stub 1L 0x30);
     (* v3 at frame 2 *)
     (* The CURRENT reader goes first and caches the newest frame. *)
     let* now = Pager.read pager 1L in
     check_int "current reader sees v3" 0x30 (byte_of now);
     (* An as-of reader bounded before v3 must still see v2 ... *)
     let* at2 = Pager.read ~snapshot_frames:2 pager 1L in
     check_int "as-of reader bounded at frame 2 sees v2" 0x20 (byte_of at2);
     (* ... and one bounded before v2 must still see v1. *)
     let* at1 = Pager.read ~snapshot_frames:1 pager 1L in
     check_int "as-of reader bounded at frame 1 sees v1" 0x10 (byte_of at1);
     (* And the current reader is not contaminated by either of them. *)
     let* now2 = Pager.read pager 1L in
     check_int "current reader still sees v3" 0x30 (byte_of now2);
     check_int "three distinct frames cached" 3 (Pager.wal_cached_count pager);
     Lwt.return_unit)
;;

(* Same three readers in the opposite arrival order: oldest first. The bug
   this rules out is direction-dependent — a page-id-keyed cache leaks the
   newest frame forward AND the oldest frame backward. *)
let test_as_of_reader_does_not_contaminate_the_current_reader () =
  Lwt_main.run
    (let pager, stub, _ = make_pager () in
     ignore (append stub 1L 0x10);
     ignore (append stub 1L 0x20);
     ignore (append stub 1L 0x30);
     let* at1 = Pager.read ~snapshot_frames:1 pager 1L in
     check_int "as-of reader sees v1" 0x10 (byte_of at1);
     let* at2 = Pager.read ~snapshot_frames:2 pager 1L in
     check_int "as-of reader sees v2" 0x20 (byte_of at2);
     let* now = Pager.read pager 1L in
     check_int "current reader is not served the older cached frame" 0x30 (byte_of now);
     (* Two as-of readers bounded to the same frame DO share an entry: same
        frame index, same bytes, so the sharing is the point. *)
     let reads = stub.frame_reads in
     let* at1' = Pager.read ~snapshot_frames:1 pager 1L in
     check_int "repeat as-of read sees v1" 0x10 (byte_of at1');
     check_int "repeat as-of read hits the cache" reads stub.frame_reads;
     Lwt.return_unit)
;;

let test_as_of_reader_on_borrow_path () =
  Lwt_main.run
    (let pager, stub, _ = make_pager () in
     ignore (append stub 6L 0x60);
     ignore (append stub 6L 0x61);
     let* now = borrow_byte pager 6L in
     check_int "borrow current reader sees the newest frame" 0x61 now;
     let* old = borrow_byte pager ~snapshot_frames:1 6L in
     check_int "borrow as-of reader sees the older frame" 0x60 old;
     let* now2 = borrow_byte pager 6L in
     check_int "borrow current reader is uncontaminated" 0x61 now2;
     Lwt.return_unit)
;;

(* An as-of reader must also survive a checkpoint: its bound refers to frame
   indices of a generation that no longer exists. *)
let test_as_of_reader_across_a_checkpoint () =
  Lwt_main.run
    (let pager, stub, _ = make_pager () in
     ignore (append stub 1L 0x10);
     ignore (append stub 1L 0x20);
     let* at1 = Pager.read ~snapshot_frames:1 pager 1L in
     check_int "as-of reader caches frame 0" 0x10 (byte_of at1);
     let* () = migrate pager 1L 0x20 in
     checkpoint stub;
     ignore (append stub 1L 0x90);
     (* Frame 0 of the NEW generation, reached through the same bound. *)
     let* after = Pager.read ~snapshot_frames:1 pager 1L in
     check_int
       "as-of bound resolves in the new generation, not the old"
       0x90
       (byte_of after);
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Interactions with the rest of the cache.                             *)
(* ------------------------------------------------------------------ *)

(* The writer's read-your-own-writes path still wins over any cached frame:
   [dirty] is consulted before the WAL on the no-snapshot path. *)
let test_dirty_still_beats_a_cached_frame () =
  Lwt_main.run
    (let pager, stub, _ = make_pager () in
     ignore (append stub 1L 0x10);
     let* r1 = Pager.read pager 1L in
     check_int "frame cached" 0x10 (byte_of r1);
     Pager.write pager 1L (with_byte 0xDD);
     let* r2 = Pager.read pager 1L in
     check_int "writer sees its own dirty page, not the cached frame" 0xDD (byte_of r2);
     (* A snapshot reader still sees the committed frame. *)
     let* r3 = Pager.read ~snapshot_frames:1 pager 1L in
     check_int "snapshot reader still sees the frame" 0x10 (byte_of r3);
     (* Rolling back must not disturb the WAL-keyed entry: frames are
        committed, and [clear_dirty] only owns main-file keys. *)
     Pager.clear_dirty pager;
     let reads = stub.frame_reads in
     let* r4 = Pager.read pager 1L in
     check_int "after rollback the committed frame is visible again" 0x10 (byte_of r4);
     check_int "rollback did not purge the WAL entry" reads stub.frame_reads;
     Lwt.return_unit)
;;

(* [~bypass_cache] must suppress insertion on the WAL path too — that is what
   keeps a BLOB overflow walk from evicting the hot B-tree pages. *)
let test_bypass_cache_does_not_populate () =
  Lwt_main.run
    (let pager, stub, _ = make_pager () in
     ignore (append stub 1L 0x10);
     let* r1 = Pager.read ~bypass_cache:true pager 1L in
     check_int "bypass read still returns the frame" 0x10 (byte_of r1);
     check_int "bypass read cached nothing" 0 (Pager.wal_cached_count pager);
     let* r2 = Pager.read ~bypass_cache:true pager 1L in
     check_int "bypass read repeats the resolution" 2 stub.frame_reads;
     check_int "and still returns the frame" 0x10 (byte_of r2);
     Lwt.return_unit)
;;

(* The cache stays bounded once WAL frames occupy it — a hot page whose frames
   keep being superseded must not accumulate one entry per frame forever, and a
   WAL entry for a page that is also dirty must stay evictable. *)
let test_wal_entries_are_evictable_and_bounded () =
  let saved = Sys.getenv_opt "GRANARY_PAGE_CACHE" in
  Unix.putenv "GRANARY_PAGE_CACHE" "16";
  Fun.protect
    ~finally:(fun () ->
      match saved with
      | Some v -> Unix.putenv "GRANARY_PAGE_CACHE" v
      | None -> Unix.putenv "GRANARY_PAGE_CACHE" "")
    (fun () ->
       Lwt_main.run
         (let read_page, write_page, sync, resize, _ = mkdev 8 in
          let pager =
            Pager.create
              ~read_page
              ~write_page
              ~sync
              ~resize
              ~n_pages:8L
              ~freelist:Granary_storage.Freelist.empty
          in
          let stub = new_stub () in
          Pager.set_wal pager (Some (callbacks stub));
          (* Page 1 is dirty for the whole loop: before #611 the eviction guard
             tested [dirty] by page id, which would have made every one of its
             superseded frames un-evictable. *)
          Pager.write pager 1L (with_byte 0xDD);
          let rec loop i =
            if i > 200
            then Lwt.return_unit
            else (
              ignore (append stub 1L (i mod 251));
              (* Read at a snapshot, so [dirty] is skipped and the frame is
                 actually resolved and cached. *)
              let* _ = Pager.read ~snapshot_frames:(i + 1) pager 1L in
              loop (i + 1))
          in
          let* () = loop 1 in
          Alcotest.(check bool)
            "WAL entries respect the cache capacity"
            true
            (Pager.wal_cached_count pager <= 16);
          Lwt.return_unit))
;;

let () =
  Alcotest.run
    "pager_wal_cache_611"
    [ ( "participation"
      , [ Alcotest.test_case
            "repeat read hits the cache"
            `Quick
            test_repeat_read_hits_the_cache
        ; Alcotest.test_case
            "repeat borrow hits the cache"
            `Quick
            test_repeat_borrow_hits_the_cache
        ; Alcotest.test_case
            "copying read does not alias the cache"
            `Quick
            test_copying_read_does_not_alias_the_cache
        ] )
    ; ( "invalidation (a) newer frame"
      , [ Alcotest.test_case
            "newer frame supersedes the cached one"
            `Quick
            test_newer_frame_supersedes_cached_frame
        ; Alcotest.test_case
            "newer frame supersedes on the borrow path"
            `Quick
            test_newer_frame_supersedes_on_borrow_path
        ] )
    ; ( "invalidation (b) checkpoint"
      , [ Alcotest.test_case
            "checkpoint purges the WAL-keyed entries"
            `Quick
            test_checkpoint_purges_wal_entries
        ; Alcotest.test_case
            "a recycled frame index is not served stale"
            `Quick
            test_recycled_frame_index_is_not_served_stale
        ; Alcotest.test_case
            "a recycled frame index is not served stale on the borrow path"
            `Quick
            test_recycled_frame_index_on_borrow_path
        ; Alcotest.test_case
            "repeated checkpoints stay correct"
            `Quick
            test_repeated_checkpoints_stay_correct
        ; Alcotest.test_case
            "detaching the WAL purges the WAL-keyed entries"
            `Quick
            test_detaching_the_wal_purges_wal_entries
        ] )
    ; ( "invalidation (c) as-of reader"
      , [ Alcotest.test_case
            "as-of reader is not served a newer frame"
            `Quick
            test_as_of_reader_is_not_served_a_newer_frame
        ; Alcotest.test_case
            "as-of reader does not contaminate the current reader"
            `Quick
            test_as_of_reader_does_not_contaminate_the_current_reader
        ; Alcotest.test_case
            "as-of reader on the borrow path"
            `Quick
            test_as_of_reader_on_borrow_path
        ; Alcotest.test_case
            "as-of reader across a checkpoint"
            `Quick
            test_as_of_reader_across_a_checkpoint
        ] )
    ; ( "cache interactions"
      , [ Alcotest.test_case
            "dirty still beats a cached frame"
            `Quick
            test_dirty_still_beats_a_cached_frame
        ; Alcotest.test_case
            "bypass_cache does not populate"
            `Quick
            test_bypass_cache_does_not_populate
        ; Alcotest.test_case
            "WAL entries are evictable and bounded"
            `Quick
            test_wal_entries_are_evictable_and_bounded
        ] )
    ]
;;
