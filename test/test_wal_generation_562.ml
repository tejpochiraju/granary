(** #562 — a checkpoint must invalidate the previous WAL generation ON DISK,
    not just in memory.

    Before #562 {!Granary_storage.Wal.reset} cleared the index, the frame cache
    and [committed_frames] and bumped the epoch, but left the frames themselves
    on the device under an unchanged [(salt, seed)] header. Recovery walks
    forward from frame 0 and stops at the first frame that fails its checksum,
    so those frames all still verified and were all replayed. Two consequences,
    one per test group below:

    - {b Perf} (what #562 was filed for): a checkpointed database was
      indistinguishable from an un-checkpointed one after a reopen. Every page
      access resolved through the WAL overlay forever, the WAL file grew
      without bound, and every [open] paid a full checksum-verifying scan of
      every frame ever written.
    - {b Correctness}: when the post-checkpoint generation was SHORTER than the
      one it replaced, recovery walked past its tail into the stale frames and
      replayed them {e over} the new ones — silently resurrecting
      pre-checkpoint page contents. [stale_tail_is_not_replayed] is that
      canary.

    The fix rotates the header's [(salt, seed)] generation marker inside
    [reset] and fsyncs it before dropping the in-memory state, so every frame
    of the old generation fails recovery's checksum.

    {b What invalidates what}, since this is cache-invalidation territory and
    getting it wrong is a silent wrong-answer bug:

    - a NEW frame for the same page: [find_page] returns the newest frame index
      (frames are prepended), so the old frame is shadowed, never dropped.
      Covered by [test_wal.ml]'s ordering tests.
    - a CHECKPOINT: [reset] — in memory (index, frame cache, committed_frames)
      and now on disk (generation marker). This file.
    - a ROLLBACK or SAVEPOINT revert: neither reaches the WAL. Uncommitted
      pages live in [Pager.dirty], which [Pager.clear_dirty] /
      [Pager.dirty_restore] handle; nothing is appended until commit.
    - a READER on an older snapshot (as-of, #266): [find_page_at ~max_frame]
      never returns a frame the reader's snapshot cannot see, and the store
      gates a checkpoint behind live readers so [reset] cannot recycle frames
      out from under one. Covered by [test_wal_reader_snapshot.ml] and
      [test_pager_snapshot_pin.ml]. *)

open Lwt.Syntax
module Wal = Granary_storage.Wal

(* ------------------------------------------------------------------ *)
(* In-memory byte-addressable device (same shape as test_wal.ml)        *)
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

let open_on ?(cipher = None) d =
  let* r =
    Wal.open_
      ~cipher
      ~read_at:(read_at d)
      ~write_at:(write_at d)
      ~sync:sync_ok
      ~size_bytes:(dev_size d)
      ()
  in
  match r with
  | Ok w -> Lwt.return w
  | Error e -> Alcotest.failf "Wal.open_: %a" Wal.pp_error e
;;

(* A device whose writes and syncs can be failed on demand.  [fail_sync]
   models the case the two blockers turn on: the bytes reach the platter, the
   durability barrier does not. *)
type faulty =
  { dev : dev
  ; mutable fail_sync : bool
  ; mutable fail_write_at_offset : int64 option
  }

let mk_faulty size = { dev = mk_dev size; fail_sync = false; fail_write_at_offset = None }

let open_faulty f =
  let write_at ~offset src =
    match f.fail_write_at_offset with
    | Some o when Int64.equal o offset -> Lwt.return (Error "injected write failure")
    | _ -> write_at f.dev ~offset src
  in
  let sync () =
    if f.fail_sync then Lwt.return (Error "injected sync failure") else Lwt.return (Ok ())
  in
  let* r =
    Wal.open_ ~read_at:(read_at f.dev) ~write_at ~sync ~size_bytes:(dev_size f.dev) ()
  in
  match r with
  | Ok w -> Lwt.return w
  | Error e -> Alcotest.failf "Wal.open_: %a" Wal.pp_error e
;;

let page b =
  let c = Cstruct.create 4096 in
  Cstruct.memset c b;
  c
;;

let commit w pid b =
  let* r = Wal.append_commit w [ Int64.of_int pid, page b ] in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Alcotest.failf "append_commit: %a" Wal.pp_error e
;;

let reset w =
  let* r = Wal.reset w in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Alcotest.failf "reset: %a" Wal.pp_error e
;;

(* Byte at offset 0 of whatever the WAL currently resolves [pid] to, or [None]
   if the page is not in the WAL index at all. *)
let peek w pid =
  match Wal.find_page w (Int64.of_int pid) with
  | None -> Lwt.return_none
  | Some idx ->
    let* r = Wal.read_frame w idx in
    (match r with
     | Ok p -> Lwt.return_some (Cstruct.get_uint8 p 0)
     | Error e -> Alcotest.failf "read_frame %d: %a" idx Wal.pp_error e)
;;

(* ------------------------------------------------------------------ *)
(* 1. A reset generation does not come back on reopen                   *)
(* ------------------------------------------------------------------ *)

let reset_generation_is_gone_after_reopen () =
  Lwt_main.run
    (let d = mk_dev 65536 in
     let* w = open_on d in
     let* () = Lwt_list.iter_s (fun p -> commit w p 0xAA) [ 1; 2; 3; 4; 5 ] in
     Alcotest.(check int) "5 frames before reset" 5 (Wal.committed_frames w);
     let* () = reset w in
     Alcotest.(check int) "0 frames after reset" 0 (Wal.committed_frames w);
     (* Reopen on the very same bytes — this is what a crash, or simply a
        close/open cycle, does. *)
     let* w2 = open_on d in
     Alcotest.(check int)
       "recovery must NOT resurrect the checkpointed generation"
       0
       (Wal.committed_frames w2);
     let* v = peek w2 1 in
     Alcotest.(check (option int)) "page 1 absent from the WAL index" None v;
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 2. The canary: a SHORTER new generation must not be overwritten by   *)
(*    the stale tail of the old one.                                    *)
(* ------------------------------------------------------------------ *)

let stale_tail_is_not_replayed () =
  Lwt_main.run
    (let d = mk_dev 65536 in
     let* w = open_on d in
     (* Generation 1: five commits, page 1 written LAST (frame 4). *)
     let* () = Lwt_list.iter_s (fun p -> commit w p 0xAA) [ 2; 3; 4; 5; 1 ] in
     let* () = reset w in
     (* Generation 2: one commit, page 1 at frame 0. *)
     let* () = commit w 1 0xBB in
     let* live = peek w 1 in
     Alcotest.(check (option int)) "live handle sees the new page" (Some 0xBB) live;
     let* w2 = open_on d in
     Alcotest.(check int)
       "recovered generation is 1 frame, not 5"
       1
       (Wal.committed_frames w2);
     let* v = peek w2 1 in
     Alcotest.(check (option int))
       "page 1 must be the POST-checkpoint value, not the stale one"
       (Some 0xBB)
       v;
     (* The old generation's other pages must not be in the index either: the
        main DB owns them after the checkpoint. *)
     let* v2 = peek w2 2 in
     Alcotest.(check (option int)) "stale page 2 not resurrected" None v2;
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 3. Rotation must not lose the NEW generation                         *)
(* ------------------------------------------------------------------ *)

let post_reset_frames_survive_reopen () =
  Lwt_main.run
    (let d = mk_dev 65536 in
     let* w = open_on d in
     let* () = Lwt_list.iter_s (fun p -> commit w p 0xAA) [ 1; 2; 3 ] in
     let* () = reset w in
     let* () = Lwt_list.iter_s (fun p -> commit w p 0xCC) [ 7; 8 ] in
     let* w2 = open_on d in
     Alcotest.(check int) "2 frames recovered" 2 (Wal.committed_frames w2);
     let* a = peek w2 7 in
     let* b = peek w2 8 in
     Alcotest.(check (option int)) "page 7 recovered" (Some 0xCC) a;
     Alcotest.(check (option int)) "page 8 recovered" (Some 0xCC) b;
     Lwt.return_unit)
;;

(* Several generations in a row: each reset must invalidate what came before,
   including a generation that was itself written after a reset. *)
let repeated_resets () =
  Lwt_main.run
    (let d = mk_dev 65536 in
     let* w = open_on d in
     let* () = Lwt_list.iter_s (fun p -> commit w p 0x11) [ 1; 2; 3; 4 ] in
     let* () = reset w in
     let* () = Lwt_list.iter_s (fun p -> commit w p 0x22) [ 1; 2; 3 ] in
     let* () = reset w in
     let* () = commit w 1 0x33 in
     let* w2 = open_on d in
     Alcotest.(check int) "1 frame recovered" 1 (Wal.committed_frames w2);
     let* v = peek w2 1 in
     Alcotest.(check (option int)) "newest generation wins" (Some 0x33) v;
     Lwt.return_unit)
;;

(* The marker must actually change: two resets in a row must not land on the
   same (salt, seed), or the second would not invalidate the first. *)
let marker_rotates () =
  Lwt_main.run
    (* #636 E: size 0 so [open_] takes the one PROVABLY fresh branch (device too
       small to hold even a header, hence too small to hold a frame).  A
       zero-filled 65536-byte device takes the [Ok None] branch instead, which
       is deliberately assumed written-to and would rotate. *)
    (let d = mk_dev 0 in
     let* w = open_on d in
     let s0 = Wal.salt w
     and e0 = Wal.seed w in
     (* An EMPTY generation has nothing to invalidate, so [reset] deliberately
        leaves the marker (and the device) alone. *)
     let* () = reset w in
     Alcotest.(check bool)
       "reset over an empty WAL does not rotate"
       true
       (Int64.equal s0 (Wal.salt w) && Int64.equal e0 (Wal.seed w));
     Alcotest.(check int64) "but the epoch still bumps" 1L (Wal.epoch w);
     let* () = commit w 1 0xAA in
     let* () = reset w in
     let s1 = Wal.salt w
     and e1 = Wal.seed w in
     Alcotest.(check bool)
       "a non-empty generation rotates the marker"
       true
       (not (Int64.equal s0 s1 && Int64.equal e0 e1));
     let* () = commit w 1 0xBB in
     let* () = reset w in
     Alcotest.(check bool)
       "and rotates again"
       true
       (not (Int64.equal s1 (Wal.salt w) && Int64.equal e1 (Wal.seed w)));
     Lwt.return_unit)
;;

(* The rotation must not depend on process-local PRNG state.  Nothing in the
   tree calls [Random.self_init], so a freshly started process walks the same
   [Random] sequence: a checkpoint taken by a process that did NOT create the
   file would draw exactly the pair [init_header] drew, rotating the marker to
   the value it already had.  Simulated here by restoring the PRNG state the
   creating "process" started from. *)
let rotation_does_not_depend_on_prng_state () =
  Lwt_main.run
    (let d = mk_dev 65536 in
     let st = Random.get_state () in
     let* w = open_on d in
     let* () = Lwt_list.iter_s (fun p -> commit w p 0xAA) [ 2; 3; 4; 5; 1 ] in
     (* A different process opens the file and checkpoints it. *)
     Random.set_state st;
     let* w2 = open_on d in
     Alcotest.(check int) "reopen sees the generation" 5 (Wal.committed_frames w2);
     let* () = reset w2 in
     let* () = commit w2 1 0xBB in
     let* w3 = open_on d in
     Alcotest.(check int) "1 frame recovered, not 5" 1 (Wal.committed_frames w3);
     let* v = peek w3 1 in
     Alcotest.(check (option int)) "no stale replay across processes" (Some 0xBB) v;
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 3b. #636 B1 — a failed rotation must not let acked commits be lost    *)
(* ------------------------------------------------------------------ *)

(* The hazard: [write_header] writes the new marker and then fsyncs.  If the
   fsync fails, the bytes may or may not be durable.  Leaving the OLD marker in
   memory and carrying on means every later commit is written and fsynced under
   a marker that recovery will reject — the application is told the commit is
   durable and it is not.  The store's autocheckpoint wrappers swallow
   checkpoint errors, so nothing else stops it.

   [reset] therefore poisons the WAL, and every append after that is refused. *)
let failed_rotation_poisons_the_wal () =
  Lwt_main.run
    (let f = mk_faulty 262144 in
     let* w = open_faulty f in
     let* () = Lwt_list.iter_s (fun p -> commit w p 0xAA) [ 1; 2; 3; 4; 5 ] in
     Alcotest.(check int) "5 frames committed" 5 (Wal.committed_frames w);
     (* The header write lands; its fsync does not. *)
     f.fail_sync <- true;
     let* r = Wal.reset w in
     Alcotest.(check bool)
       "reset surfaces the failure"
       true
       (match r with
        | Error _ -> true
        | Ok () -> false);
     Alcotest.(check bool) "and poisons the WAL" true (Wal.is_poisoned w);
     Alcotest.(check int)
       "the old generation is still addressable in memory"
       5
       (Wal.committed_frames w);
     (* The device recovers, but the WAL must NOT resume appending: we still do
        not know which marker is on the platter. *)
     f.fail_sync <- false;
     let* r = Wal.append_commit w [ 9L, page 0xBB ] in
     Alcotest.(check bool)
       "a post-failure commit is REFUSED, not acked"
       true
       (match r with
        | Error _ -> true
        | Ok () -> false);
     Alcotest.(check int) "and did not advance the frame count" 5 (Wal.committed_frames w);
     let* r = Wal.append_commit_no_sync w [ 9L, page 0xBB ] in
     Alcotest.(check bool)
       "the group-commit entry point is refused too"
       true
       (match r with
        | Error _ -> true
        | Ok () -> false);
     (* Reads keep working — they never re-verify a checksum. *)
     let* v = peek w 1 in
     Alcotest.(check (option int)) "reads are unaffected" (Some 0xAA) v;
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 3c. #636 B2 — orphan frames from a failed sync must still rotate     *)
(* ------------------------------------------------------------------ *)

(* [append_commit] writes every frame — the commit-flagged one included —
   BEFORE [flush_sync], and bumps [committed_frames] only after.  So a batch
   whose bytes land and whose sync fails leaves valid, commit-flagged frames on
   disk with [committed_frames] still 0.  A [reset] that skipped rotation on
   [committed_frames = 0] would leave them verifying, and the next shorter
   generation would be overwritten by them on recovery. *)
let orphan_frames_still_force_a_rotation () =
  Lwt_main.run
    (let f = mk_faulty 262144 in
     let* w = open_faulty f in
     (* Three frames reach the device; the sync fails, so nothing is counted. *)
     f.fail_sync <- true;
     let* r = Wal.append_commit w [ 7L, page 0xEE; 8L, page 0xEE; 9L, page 0xEE ] in
     Alcotest.(check bool)
       "the append surfaces the sync failure"
       true
       (match r with
        | Error _ -> true
        | Ok () -> false);
     Alcotest.(check int) "and commits nothing" 0 (Wal.committed_frames w);
     f.fail_sync <- false;
     (* The checkpoint that follows must NOT take the empty-generation fast
        path: there are three valid frames on the device. *)
     let* r = Wal.reset w in
     Alcotest.(check bool)
       "reset succeeds"
       true
       (match r with
        | Error _ -> false
        | Ok () -> true);
     let* () = commit w 9 0xBB in
     let* w2 = open_faulty { f with dev = f.dev } in
     Alcotest.(check int)
       "recovery sees only the new generation, not the orphans"
       1
       (Wal.committed_frames w2);
     let* v = peek w2 9 in
     Alcotest.(check (option int)) "page 9 is the new value" (Some 0xBB) v;
     let* v = peek w2 7 in
     Alcotest.(check (option int)) "orphan page 7 is not resurrected" None v;
     Lwt.return_unit)
;;

(* Scenario E — the [Ok None] open branch (magic missing) is NOT provably
   fresh, and treating it as such reinstates #636 through the fast path.

   A torn 24-byte header write can leave the salt/seed words intact while the
   magic no longer matches.  Reopening takes the "magic missing — initialise"
   branch, and [init_header] draws from an un-self-init'd [Random] (#613), so a
   freshly started process re-draws the ORIGINAL (salt, seed).  The old frames
   then verify under the supposedly-new marker.  If that branch started with
   [wrote_since_rotation = false], the first [reset] would skip the rotation and
   the next shorter generation would be overwritten on recovery. *)
let magic_damaged_open_is_not_treated_as_fresh () =
  Lwt_main.run
    (let d = mk_dev 65536 in
     let st = Random.get_state () in
     let* w = open_on d in
     let s0 = Wal.salt w in
     let* () = Lwt_list.iter_s (fun p -> commit w p 0xEE) [ 7; 8; 9 ] in
     Alcotest.(check int) "3 frames committed" 3 (Wal.committed_frames w);
     (* Tear the magic; leave salt and seed untouched. *)
     Bytes.set d.buf 0 '\xFF';
     (* A freshly started process reopens it. *)
     Random.set_state st;
     let* w2 = open_on d in
     Alcotest.(check int) "recovery counts nothing" 0 (Wal.committed_frames w2);
     Alcotest.(check bool)
       "and #613 hands back the ORIGINAL marker, so the old frames still verify"
       true
       (Int64.equal s0 (Wal.salt w2));
     let* () = reset w2 in
     let* () = commit w2 9 0xBB in
     let* w3 = open_on d in
     Alcotest.(check int)
       "recovery sees only the new generation, not the pre-tear frames"
       1
       (Wal.committed_frames w3);
     let* v = peek w3 9 in
     Alcotest.(check (option int)) "page 9 is the new value" (Some 0xBB) v;
     let* v = peek w3 7 in
     Alcotest.(check (option int)) "page 7 is not resurrected" None v;
     Lwt.return_unit)
;;

(* The fast path must still be free where it is genuinely sound: a WAL nothing
   has been written to since the last rotation. *)
let untouched_wal_skips_the_rotation () =
  Lwt_main.run
    (* Size 0 — the provably-fresh open branch; see [marker_rotates]. *)
    (let f = mk_faulty 0 in
     let* w = open_faulty f in
     let s0 = Wal.salt w in
     (* Fresh file, nothing written: reset must not touch the device at all —
        proven by failing every write to the header offset. *)
     f.fail_write_at_offset <- Some 0L;
     let* r = Wal.reset w in
     Alcotest.(check bool)
       "reset over an untouched WAL performs no header write"
       true
       (match r with
        | Error _ -> false
        | Ok () -> true);
     Alcotest.(check bool) "marker unchanged" true (Int64.equal s0 (Wal.salt w));
     Alcotest.(check bool) "not poisoned" false (Wal.is_poisoned w);
     (* Once a frame has been written, the next reset must rotate. *)
     f.fail_write_at_offset <- None;
     let* () = commit w 1 0xAA in
     let* () = reset w in
     Alcotest.(check bool)
       "a written-to WAL does rotate"
       true
       (not (Int64.equal s0 (Wal.salt w)));
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 4. Encrypted WAL: the checksum is over ciphertext, so the rotation   *)
(*    works the same way — but assert it rather than assume it.         *)
(* ------------------------------------------------------------------ *)

let encrypted_reset_invalidates () =
  let key = String.init 32 (fun i -> Char.chr (i * 7 land 0xff)) in
  let cipher =
    match Granary_storage.Crypto.create ~key with
    | Ok c -> Some c
    | Error `Bad_key_length -> Alcotest.fail "bad key length"
  in
  Mirage_crypto_rng_unix.use_default ();
  Lwt_main.run
    (let d = mk_dev 262144 in
     let* w = open_on ~cipher d in
     let* () = Lwt_list.iter_s (fun p -> commit w p 0xAA) [ 2; 3; 4; 5; 1 ] in
     let* () = reset w in
     let* () = commit w 1 0xBB in
     let* w2 = open_on ~cipher d in
     Alcotest.(check int) "encrypted: 1 frame recovered" 1 (Wal.committed_frames w2);
     let* v = peek w2 1 in
     Alcotest.(check (option int)) "encrypted: no stale replay" (Some 0xBB) v;
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "wal generation (#562)"
    [ ( "reset"
      , [ Alcotest.test_case
            "checkpointed generation is gone after reopen"
            `Quick
            reset_generation_is_gone_after_reopen
        ; Alcotest.test_case
            "stale tail is not replayed over a shorter generation"
            `Quick
            stale_tail_is_not_replayed
        ; Alcotest.test_case
            "post-reset frames survive reopen"
            `Quick
            post_reset_frames_survive_reopen
        ; Alcotest.test_case "repeated resets" `Quick repeated_resets
        ; Alcotest.test_case "generation marker rotates" `Quick marker_rotates
        ; Alcotest.test_case
            "rotation does not depend on PRNG state"
            `Quick
            rotation_does_not_depend_on_prng_state
        ; Alcotest.test_case
            "#636 B1: a failed rotation poisons the WAL"
            `Quick
            failed_rotation_poisons_the_wal
        ; Alcotest.test_case
            "#636 B2: orphan frames from a failed sync still force a rotation"
            `Quick
            orphan_frames_still_force_a_rotation
        ; Alcotest.test_case
            "#636 E: a magic-damaged open is not treated as fresh"
            `Quick
            magic_damaged_open_is_not_treated_as_fresh
        ; Alcotest.test_case
            "an untouched WAL still skips the rotation"
            `Quick
            untouched_wal_skips_the_rotation
        ; Alcotest.test_case
            "encrypted WAL: reset invalidates"
            `Quick
            encrypted_reset_invalidates
        ] )
    ]
;;
