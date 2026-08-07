(** #612 — the WAL file must be physically reclaimed at checkpoint, not merely
    logically emptied.

    Before #612 {!Granary_storage.Wal.reset} rotated the on-disk generation
    marker (#562), cleared the index and zeroed [committed_frames] — but
    [Wal.t] had no resize callback at all, so the file itself could only ever
    grow. Its length was a permanent high-water mark: load ten million rows
    once and the [-wal] sidecar stayed at its peak for the life of the database,
    even though the steady state needs a few dozen frames. On a unikernel with
    a small volume that is the difference between running and not.

    The fix is an optional [?resize] callback on {!Wal.open_}, invoked from
    [reset] {b after} the marker rotation is durable, truncating back to the
    24-byte header.

    Three things are load-bearing and each has a test below:

    - {b [size_bytes] and the file length move together.} [read_frame_raw]
      bounds-checks against [size_bytes], so lowering it while the file is still
      long would reject frames the next generation legitimately reuses, and
      lowering it without truncating reclaims nothing. They are updated in the
      same place, under the caller's writer lock.
    - {b The order is rotate-then-truncate, and it is the whole crash-safety
      argument.} Whatever fraction of the truncation reaches the device, the
      surviving trailing bytes are old-generation frames that fail the NEW
      marker, so recovery reads the WAL as empty. The reverse order leaves a
      short file under the OLD marker, where the next generation's frames are
      indistinguishable from the old one's survivors —
      [crash_mid_truncation_is_recoverable] is that canary.
    - {b The floor is [header_size_bytes], never 0.} A zero-length WAL re-inits
      a fresh marker on the next open, breaking the chain
      [next_generation_marker] derives the next generation from.
      [header_survives_truncation] pins it.

    And one thing that is deliberately NOT load-bearing: a truncation failure is
    swallowed. It costs disk space, not correctness, and [reset] returning an
    error fails the entire checkpoint ([Store.checkpoint_unlocked] raises).
    [truncation_failure_is_not_fatal] pins that too.

    Readers: a checkpoint is already gated behind live RO snapshots
    ([Store.wait_for_readers_past] honours [ro_readers_below]
    unconditionally), so the truncation cannot pull frames out from under an
    older snapshot. #612 makes that gate matter more than it did — before,
    a racing reader got stale-but-present bytes; now it would get EOF — so
    [snapshot_reader_is_not_broken_by_a_checkpoint] asserts the gate directly
    rather than trusting it. *)

open Lwt.Syntax
module Wal = Granary_storage.Wal
module S = Granary_store.Store

(* ------------------------------------------------------------------ *)
(* A shrinkable in-memory byte device.                                  *)
(*                                                                      *)
(* Unlike [test_wal_generation_562.ml]'s device this one zero-fills a    *)
(* read past EOF rather than erroring, matching the real sidecar         *)
(* ([Granary_unix.Store.wal_read_at]) — which is exactly the behaviour   *)
(* a truncation test has to model.                                      *)
(* ------------------------------------------------------------------ *)

type dev = { mutable buf : Bytes.t }

let mk_dev () = { buf = Bytes.create 0 }
let dev_len d = Bytes.length d.buf
let dev_size d = Int64.of_int (Bytes.length d.buf)

let dev_set_len d n =
  let cur = Bytes.length d.buf in
  if n <> cur
  then (
    let nb = Bytes.make n '\x00' in
    Bytes.blit d.buf 0 nb 0 (min cur n);
    d.buf <- nb)
;;

let read_at d ~offset out =
  let off = Int64.to_int offset in
  let len = Cstruct.length out in
  let avail = max 0 (min len (Bytes.length d.buf - off)) in
  let tmp = Bytes.make len '\x00' in
  if avail > 0 then Bytes.blit d.buf off tmp 0 avail;
  Cstruct.blit_from_bytes tmp 0 out 0 len;
  Lwt.return (Ok ())
;;

let write_at d ~offset src =
  let off = Int64.to_int offset in
  let len = Cstruct.length src in
  if off + len > Bytes.length d.buf then dev_set_len d (off + len);
  let tmp = Bytes.create len in
  Cstruct.blit_to_bytes src 0 tmp 0 len;
  Bytes.blit tmp 0 d.buf off len;
  Lwt.return (Ok ())
;;

let sync_ok () = Lwt.return (Ok ())

let resize_ok d n =
  dev_set_len d (Int64.to_int n);
  Lwt.return (Ok ())
;;

let resize_fails _n = Lwt.return (Error "injected ftruncate failure")

(* [resize]: [None] means "opened without the callback", i.e. pre-#612
   behaviour. *)
let open_on ?resize d =
  let* r =
    Wal.open_
      ?resize
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

let peek w pid =
  match Wal.find_page w (Int64.of_int pid) with
  | None -> Lwt.return_none
  | Some idx ->
    let* r = Wal.read_frame w idx in
    (match r with
     | Ok p -> Lwt.return_some (Cstruct.get_uint8 p 0)
     | Error e -> Alcotest.failf "read_frame %d: %a" idx Wal.pp_error e)
;;

let hdr = Wal.header_size_bytes
let frame = Wal.frame_size_bytes
let expected_len n_frames = hdr + (n_frames * frame)

(* ------------------------------------------------------------------ *)
(* 1. The file actually shrinks — and [size_bytes] shrinks with it.     *)
(* ------------------------------------------------------------------ *)

let reset_truncates_the_file () =
  Lwt_main.run
    (let d = mk_dev () in
     let* w = open_on ~resize:(resize_ok d) d in
     let* () = Lwt_list.iter_s (fun p -> commit w p 0xAA) [ 1; 2; 3; 4; 5; 6; 7; 8 ] in
     Alcotest.(check int) "8 frames" 8 (Wal.committed_frames w);
     Alcotest.(check int) "file grew to 8 frames" (expected_len 8) (dev_len d);
     Alcotest.(check int64)
       "size_bytes tracks the 8 frames"
       (Int64.of_int (expected_len 8))
       (Wal.size_bytes w);
     let* () = reset w in
     Alcotest.(check int) "file is back to the bare header" hdr (dev_len d);
     Alcotest.(check int64)
       "size_bytes came down with it"
       (Int64.of_int hdr)
       (Wal.size_bytes w);
     Alcotest.(check bool) "not poisoned by the truncation" false (Wal.is_poisoned w);
     Lwt.return_unit)
;;

(* Repeated cycles must not creep: this is the "high-water mark" property
   the issue is about, so assert it over several generations of differing
   length rather than one. *)
let repeated_checkpoints_do_not_creep () =
  Lwt_main.run
    (let d = mk_dev () in
     let* w = open_on ~resize:(resize_ok d) d in
     let* () =
       Lwt_list.iter_s
         (fun n ->
            let* () =
              Lwt_list.iter_s
                (fun p -> commit w p (0x10 + n))
                (List.init n (fun i -> i + 1))
            in
            Alcotest.(check int)
              (Printf.sprintf "generation of %d frames sized correctly" n)
              (expected_len n)
              (dev_len d);
            let* () = reset w in
            Alcotest.(check int)
              (Printf.sprintf "reclaimed after a %d-frame generation" n)
              hdr
              (dev_len d);
            Lwt.return_unit)
         [ 12; 3; 20; 1; 7 ]
     in
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 2. A reopen after truncation reads correct data.                     *)
(* ------------------------------------------------------------------ *)

let reopen_after_truncation_reads_correct_data () =
  Lwt_main.run
    (let d = mk_dev () in
     let* w = open_on ~resize:(resize_ok d) d in
     (* Generation 1: five frames, then checkpointed away. *)
     let* () = Lwt_list.iter_s (fun p -> commit w p 0xAA) [ 1; 2; 3; 4; 5 ] in
     let* () = reset w in
     (* Generation 2: deliberately SHORTER than its predecessor — the shape
        that #562 showed can be overwritten by a stale tail.  After #612 there
        is no stale tail left to overwrite it with, but assert the content, not
        the mechanism. *)
     let* () = commit w 1 0xBB in
     let* () = commit w 9 0xCC in
     Alcotest.(check int)
       "file holds exactly the new generation"
       (expected_len 2)
       (dev_len d);
     let* w2 = open_on ~resize:(resize_ok d) d in
     Alcotest.(check int) "recovers exactly 2 frames" 2 (Wal.committed_frames w2);
     let* v1 = peek w2 1 in
     Alcotest.(check (option int)) "page 1 is the post-checkpoint value" (Some 0xBB) v1;
     let* v9 = peek w2 9 in
     Alcotest.(check (option int)) "page 9" (Some 0xCC) v9;
     let* v2 = peek w2 2 in
     Alcotest.(check (option int)) "a checkpointed page is gone, not resurrected" None v2;
     Lwt.return_unit)
;;

(* The header — and with it the generation chain — must survive the
   truncation.  If the floor were 0 the next open would find no magic,
   re-init a fresh marker, and [next_generation_marker] would be chaining
   off a value it did not produce. *)
let header_survives_truncation () =
  Lwt_main.run
    (let d = mk_dev () in
     let* w = open_on ~resize:(resize_ok d) d in
     let* () = commit w 1 0xAA in
     let* () = reset w in
     Alcotest.(check int) "truncated to exactly the header" hdr (dev_len d);
     let salt = Wal.salt w
     and seed = Wal.seed w in
     let* w2 = open_on ~resize:(resize_ok d) d in
     Alcotest.(check bool)
       "the reopened WAL read the rotated marker, it did not mint a new one"
       true
       (Int64.equal salt (Wal.salt w2) && Int64.equal seed (Wal.seed w2));
     Alcotest.(check int) "and the WAL is empty" 0 (Wal.committed_frames w2);
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 3. A crash mid-truncation is recoverable.                            *)
(* ------------------------------------------------------------------ *)

(* Model the crash window precisely.  [reset] fsyncs the rotated marker and
   only then truncates, so a crash inside the truncation leaves: the NEW
   24-byte header, followed by some arbitrary prefix of the OLD generation's
   frames.  Build exactly that image for every cut point of interest and assert
   recovery reads it as empty — and that a fresh generation written on top is
   not overwritten by whatever survived. *)
let crash_mid_truncation_is_recoverable () =
  let cut_points = [ 0; 1; 3; 4; 5 ] in
  (* frames of the old generation left behind *)
  let one_cut survivors =
    Lwt_main.run
      (let d = mk_dev () in
       let* w = open_on ~resize:(resize_ok d) d in
       let* () = Lwt_list.iter_s (fun p -> commit w p 0xAA) [ 1; 2; 3; 4; 5 ] in
       let old_image = Bytes.copy d.buf in
       let* () = reset w in
       (* Post-crash image: the durable new header + a partial old tail. *)
       let target = expected_len survivors in
       dev_set_len d target;
       if survivors > 0 then Bytes.blit old_image hdr d.buf hdr (target - hdr);
       let* w2 = open_on ~resize:(resize_ok d) d in
       Alcotest.(check int)
         (Printf.sprintf
            "crash leaving %d old frames: recovery reads the WAL as empty"
            survivors)
         0
         (Wal.committed_frames w2);
       let* v = peek w2 1 in
       Alcotest.(check (option int)) "no stale page is indexed" None v;
       (* And the next generation, written over the survivors, wins. *)
       let* () = commit w2 1 0xBB in
       let* w3 = open_on ~resize:(resize_ok d) d in
       Alcotest.(check int) "successor generation recovers" 1 (Wal.committed_frames w3);
       let* v = peek w3 1 in
       Alcotest.(check (option int)) "successor content" (Some 0xBB) v;
       Lwt.return_unit)
  in
  List.iter one_cut cut_points
;;

(* The reverse of the crash above: the rotation is durable and the truncation
   never happened at all (the device ignored it / the process died before
   issuing it).  This is the pre-#612 on-disk state and must stay recoverable —
   it is the state every WAL written before this change is already in. *)
let crash_before_truncation_is_recoverable () =
  Lwt_main.run
    (let d = mk_dev () in
     (* [resize] that reports success without doing anything: the device
        acknowledged the truncation, the bytes did not go away. *)
     let* w = open_on ~resize:(fun _ -> Lwt.return (Ok ())) d in
     let* () = Lwt_list.iter_s (fun p -> commit w p 0xAA) [ 1; 2; 3; 4; 5 ] in
     let* () = reset w in
     Alcotest.(check int) "file untouched" (expected_len 5) (dev_len d);
     let* w2 = open_on d in
     Alcotest.(check int) "old generation does not verify" 0 (Wal.committed_frames w2);
     let* () = commit w2 1 0xBB in
     let* w3 = open_on d in
     Alcotest.(check int) "short successor recovers" 1 (Wal.committed_frames w3);
     let* v = peek w3 1 in
     Alcotest.(check (option int))
       "and is not overwritten by the stale tail"
       (Some 0xBB)
       v;
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 4. Degradation: a failing or absent resize costs space, nothing else.*)
(* ------------------------------------------------------------------ *)

let truncation_failure_is_not_fatal () =
  Lwt_main.run
    (let d = mk_dev () in
     let* w = open_on ~resize:resize_fails d in
     let* () = Lwt_list.iter_s (fun p -> commit w p 0xAA) [ 1; 2; 3; 4; 5 ] in
     let* () = reset w in
     Alcotest.(check bool) "reset still succeeded" false (Wal.is_poisoned w);
     Alcotest.(check int) "file did not shrink" (expected_len 5) (dev_len d);
     (* The critical half: [size_bytes] must NOT have been lowered, or the next
        generation's frames would fail the bounds check in [read_frame_raw]. *)
     Alcotest.(check int64)
       "size_bytes stayed at the real file length"
       (Int64.of_int (expected_len 5))
       (Wal.size_bytes w);
     let* () = commit w 1 0xBB in
     let* v = peek w 1 in
     Alcotest.(check (option int)) "the next generation still reads back" (Some 0xBB) v;
     let* w2 = open_on d in
     Alcotest.(check int) "and recovers" 1 (Wal.committed_frames w2);
     let* v = peek w2 1 in
     Alcotest.(check (option int)) "with the right content" (Some 0xBB) v;
     Lwt.return_unit)
;;

let no_resize_callback_keeps_the_old_behaviour () =
  Lwt_main.run
    (let d = mk_dev () in
     let* w = open_on d in
     let* () = Lwt_list.iter_s (fun p -> commit w p 0xAA) [ 1; 2; 3; 4; 5 ] in
     let* () = reset w in
     Alcotest.(check int)
       "no callback: file keeps its high-water mark"
       (expected_len 5)
       (dev_len d);
     Alcotest.(check int64)
       "no callback: size_bytes keeps it too"
       (Int64.of_int (expected_len 5))
       (Wal.size_bytes w);
     let* () = commit w 7 0xBB in
     let* w2 = open_on d in
     Alcotest.(check int) "still correct, just larger" 1 (Wal.committed_frames w2);
     let* v = peek w2 7 in
     Alcotest.(check (option int)) "content" (Some 0xBB) v;
     Lwt.return_unit)
;;

(* An untouched WAL takes [reset]'s no-device-work fast path (#562/#636), which
   must stay free — no truncation call, no error. *)
let untouched_wal_is_still_free () =
  Lwt_main.run
    (let d = mk_dev () in
     let calls = ref 0 in
     let resize n =
       incr calls;
       resize_ok d n
     in
     let* w = open_on ~resize d in
     let* () = reset w in
     Alcotest.(check int) "fresh WAL: no truncation issued" 0 !calls;
     let* () = commit w 1 0xAA in
     let* () = reset w in
     Alcotest.(check int) "written-to WAL: truncated once" 1 !calls;
     let* () = reset w in
     Alcotest.(check int) "already-reclaimed WAL: not truncated again" 1 !calls;
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 5. End to end over a real file.                                      *)
(* ------------------------------------------------------------------ *)

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "granary: %a" Granary.Db.pp_error e
;;

let run = Lwt_main.run
let exec db sql = ignore (unwrap (run (Granary.Db.execute db sql)))
let open_at path = unwrap (run (Granary_unix.open_file_wal ~path ()))

let close db =
  try ignore (run (Granary.Db.close db)) with
  | _ -> ()
;;

let with_path f =
  let dir = Filename.temp_file "t612-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "db" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun sfx ->
           try Sys.remove (path ^ sfx) with
           | _ -> ())
        [ ""; "-wal" ];
      try Unix.rmdir dir with
      | _ -> ())
    (fun () -> f path)
;;

let wal_size path =
  try (Unix.stat (path ^ "-wal")).Unix.st_size with
  | _ -> 0
;;

let rows_of db sql =
  run
    (let* stream = Lwt.map unwrap (Granary.Db.query db sql) in
     Lwt_stream.to_list stream)
;;

let int_at row i =
  match row.(i) with
  | Granary_encoding.Row.V_int n -> Int64.to_int n
  | _ -> Alcotest.failf "expected an integer at column %d" i
;;

let text_at row i =
  match row.(i) with
  | Granary_encoding.Row.V_text s -> s
  | _ -> Alcotest.failf "expected text at column %d" i
;;

let checkpoint_shrinks_the_wal_file () =
  with_path (fun path ->
    let db = open_at path in
    exec db "PRAGMA synchronous = off";
    exec db "PRAGMA wal_autocheckpoint = 0";
    exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
    exec db "BEGIN";
    for i = 1 to 3000 do
      exec db (Printf.sprintf "INSERT INTO t VALUES (%d, 'v%d')" i i)
    done;
    exec db "COMMIT";
    let before = wal_size path in
    Alcotest.(check bool)
      (Printf.sprintf "the load inflated the WAL (%d bytes)" before)
      true
      (before > 10 * frame);
    exec db "PRAGMA wal_checkpoint";
    let after = wal_size path in
    Alcotest.(check int) "checkpoint reclaimed the file down to the header" hdr after;
    Alcotest.(check bool)
      (Printf.sprintf "which is a real reclaim, not a rounding (%d -> %d)" before after)
      true
      (after * 100 < before);
    (* Correct data on the live handle... *)
    let rows = rows_of db "SELECT count(*) FROM t" in
    Alcotest.(check int) "row count on the live handle" 3000 (int_at (List.hd rows) 0);
    (* ...and across a reopen. *)
    close db;
    let db = open_at path in
    let rows = rows_of db "SELECT count(*) FROM t" in
    Alcotest.(check int) "row count after reopen" 3000 (int_at (List.hd rows) 0);
    let rows = rows_of db "SELECT b FROM t WHERE a = 2999" in
    Alcotest.(check string) "a sampled value" "v2999" (text_at (List.hd rows) 0);
    (* A post-checkpoint write re-grows the file from the header, and the next
       checkpoint reclaims it again — the steady state the issue asks for. *)
    exec db "PRAGMA wal_autocheckpoint = 0";
    exec db "UPDATE t SET b = 'AFTER' WHERE a = 1";
    Alcotest.(check bool) "a write re-grows the WAL" true (wal_size path > hdr);
    exec db "PRAGMA wal_checkpoint";
    Alcotest.(check int) "and the next checkpoint reclaims it" hdr (wal_size path);
    close db;
    let db = open_at path in
    let rows = rows_of db "SELECT b FROM t WHERE a = 1" in
    Alcotest.(check string)
      "the post-checkpoint UPDATE survived the second reclaim"
      "AFTER"
      (text_at (List.hd rows) 0);
    let rows = rows_of db "SELECT count(*) FROM t" in
    Alcotest.(check int) "and nothing was lost" 3000 (int_at (List.hd rows) 0);
    close db)
;;

(* ------------------------------------------------------------------ *)
(* 6. An outstanding older-snapshot reader is not broken.               *)
(* ------------------------------------------------------------------ *)

let bs = Bytes.of_string

let count_via_cursor : type a. a S.txn -> S.tree_id -> int Lwt.t =
  fun tx tid ->
  let* cur = S.cursor_open tx tid in
  let _ = S.cursor_first cur in
  let rec loop n =
    match S.cursor_next cur with
    | None -> n
    | Some _ -> loop (n + 1)
  in
  let n = loop 0 in
  S.cursor_close cur;
  Lwt.return n
;;

(* The checkpoint gate ([Store.wait_for_readers_past]) existed before #612, but
   #612 raises the stakes: a reader that raced [Wal.reset] used to find
   stale-but-present bytes past the tail; now it would find EOF.  So assert the
   gate, and assert it by OBSERVING that the file has not been reclaimed while
   the snapshot is live — a truncation that slipped through would be visible as
   a shrunken file with the reader still holding frames below the target. *)
let snapshot_reader_is_not_broken_by_a_checkpoint () =
  let tid = 16 in
  let n_seed = 30 in
  let n_extra = 12 in
  with_path (fun path ->
    run
      (let* sr = Granary_unix.Store.open_file_wal ~path () in
       let st =
         match sr with
         | Ok s -> s
         | Error e -> Alcotest.failf "open_file_wal: %a" S.pp_error e
       in
       (* No autocheckpoint: this test wants exactly one, at a time it chooses. *)
       S.set_wal_autocheckpoint st 0;
       let put tx pfx i =
         S.put tx tid (bs (Printf.sprintf "%s%04d" pfx i)) (bs (Printf.sprintf "v%04d" i))
       in
       let* tx = S.rw_begin st in
       let* () =
         Lwt_list.iter_s (fun i -> put tx "k" i) (List.init n_seed (fun i -> i))
       in
       let* () = S.commit tx in
       (* The snapshot that must survive the checkpoint. *)
       let* ro = S.ro_begin st in
       let* initial = count_via_cursor ro tid in
       Alcotest.(check int) "snapshot sees the seed" n_seed initial;
       (* Frames the snapshot cannot see, so the checkpoint's target is strictly
          above the snapshot's floor and the gate actually engages. *)
       let* () =
         Lwt_list.iter_s
           (fun i ->
              let* tx = S.rw_begin st in
              let* () = put tx "x" i in
              S.commit tx)
           (List.init n_extra (fun i -> i))
       in
       let size_before = wal_size path in
       Alcotest.(check bool) "the WAL holds frames" true (size_before > hdr);
       (* Launch the checkpoint; it must park on the reader gate. *)
       let ckpt = S.checkpoint st in
       let rec settle n =
         if n = 0
         then Lwt.return_unit
         else
           let* () = Lwt.pause () in
           settle (n - 1)
       in
       let* () = settle 20 in
       Alcotest.(check bool)
         "the checkpoint has NOT reclaimed the file while a reader is below it"
         true
         (wal_size path = size_before);
       (* And the reader keeps answering from its own snapshot throughout. *)
       let* c = count_via_cursor ro tid in
       Alcotest.(check int) "snapshot stable while a checkpoint waits" n_seed c;
       (* Release it; the checkpoint completes and reclaims. *)
       let* () = S.ro_end ro in
       let* () = ckpt in
       Alcotest.(check int) "reclaimed once the reader left" hdr (wal_size path);
       (* A fresh snapshot sees everything, served from the main file. *)
       let* ro2 = S.ro_begin st in
       let* fresh = count_via_cursor ro2 tid in
       Alcotest.(check int)
         "post-checkpoint snapshot sees every row"
         (n_seed + n_extra)
         fresh;
       let* () = S.ro_end ro2 in
       let* () = S.close st in
       Lwt.return_unit))
;;

let () =
  Alcotest.run
    "wal truncation (#612)"
    [ ( "reclaim"
      , [ Alcotest.test_case "reset truncates the file" `Quick reset_truncates_the_file
        ; Alcotest.test_case
            "repeated checkpoints do not creep"
            `Quick
            repeated_checkpoints_do_not_creep
        ; Alcotest.test_case
            "reopen after truncation reads correct data"
            `Quick
            reopen_after_truncation_reads_correct_data
        ; Alcotest.test_case
            "the header survives truncation"
            `Quick
            header_survives_truncation
        ] )
    ; ( "crash"
      , [ Alcotest.test_case
            "a crash mid-truncation is recoverable"
            `Quick
            crash_mid_truncation_is_recoverable
        ; Alcotest.test_case
            "a crash before truncation is recoverable"
            `Quick
            crash_before_truncation_is_recoverable
        ] )
    ; ( "degradation"
      , [ Alcotest.test_case
            "a failed truncation is not fatal"
            `Quick
            truncation_failure_is_not_fatal
        ; Alcotest.test_case
            "no resize callback keeps the old behaviour"
            `Quick
            no_resize_callback_keeps_the_old_behaviour
        ; Alcotest.test_case
            "an untouched WAL is still free"
            `Quick
            untouched_wal_is_still_free
        ] )
    ; ( "end to end"
      , [ Alcotest.test_case
            "a checkpoint shrinks the WAL file"
            `Quick
            checkpoint_shrinks_the_wal_file
        ; Alcotest.test_case
            "an older-snapshot reader is not broken by a checkpoint"
            `Quick
            snapshot_reader_is_not_broken_by_a_checkpoint
        ] )
    ]
;;
