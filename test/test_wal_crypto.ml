open Lwt.Syntax
module Wal = Sqlocaml_storage.Wal
module C = Sqlocaml_storage.Crypto

let seed () = Mirage_crypto_rng_unix.use_default ()

(* in-memory growable byte device *)
let make_dev () =
  let store = ref (Bytes.create 0) in
  let ensure n =
    if Bytes.length !store < n
    then (
      let b = Bytes.make n '\000' in
      Bytes.blit !store 0 b 0 (Bytes.length !store);
      store := b)
  in
  let read_at ~offset buf =
    let o = Int64.to_int offset in
    ensure (o + Cstruct.length buf);
    Cstruct.blit_from_bytes !store o buf 0 (Cstruct.length buf);
    Lwt.return_ok ()
  in
  let write_at ~offset buf =
    let o = Int64.to_int offset in
    ensure (o + Cstruct.length buf);
    Cstruct.blit_to_bytes buf 0 !store o (Cstruct.length buf);
    Lwt.return_ok ()
  in
  let sync () = Lwt.return_ok () in
  read_at, write_at, sync, fun () -> Int64.of_int (Bytes.length !store)
;;

let cipher () =
  match C.create ~key:(String.make 32 'k') with
  | Ok t -> t
  | _ -> assert false
;;

let mk_page tag =
  let p = Cstruct.create 4096 in
  for i = 0 to 4095 do
    Cstruct.set_uint8 p i ((i + tag) land 0xff)
  done;
  p
;;

let open_enc_result read_at write_at sync size =
  Wal.open_ ~cipher:(Some (cipher ())) ~read_at ~write_at ~sync ~size_bytes:(size ()) ()
;;

let open_enc read_at write_at sync size =
  let* r = open_enc_result read_at write_at sync size in
  match r with
  | Ok w -> Lwt.return w
  | Error _ -> Alcotest.fail "open"
;;

let test_encrypted_wal_roundtrip _ () =
  let read_at, write_at, sync, size = make_dev () in
  let* w = open_enc read_at write_at sync size in
  let p = mk_page 1 in
  let* () =
    let* r = Wal.append_commit w [ 5L, p ] in
    match r with
    | Ok () -> Lwt.return_unit
    | Error _ -> Alcotest.fail "append"
  in
  let* got =
    let* r = Wal.read_frame w 0 in
    match r with
    | Ok pg -> Lwt.return pg
    | Error _ -> Alcotest.fail "read"
  in
  Alcotest.(check bool) "decrypted page matches" true (Cstruct.equal p got);
  Lwt.return_unit
;;

let test_ciphertext_on_disk _ () =
  let read_at, write_at, sync, size = make_dev () in
  let* w = open_enc read_at write_at sync size in
  let p = mk_page 7 in
  let* () =
    let* r = Wal.append_commit w [ 2L, p ] in
    match r with
    | Ok () -> Lwt.return_unit
    | Error _ -> Alcotest.fail "append"
  in
  (* frame 0 payload starts at: WAL header (24) + frame meta (24) = 48 *)
  let raw = Cstruct.create 4096 in
  let* () =
    let* r = read_at ~offset:48L raw in
    match r with
    | Ok () -> Lwt.return_unit
    | Error _ -> Alcotest.fail "raw read"
  in
  Alcotest.(check bool) "payload is encrypted on disk" false (Cstruct.equal p raw);
  Lwt.return_unit
;;

let test_recovery_after_reopen _ () =
  (* commit two frames, reopen the WAL over the same device, ensure frames recovered & decrypt *)
  let read_at, write_at, sync, size = make_dev () in
  let* w = open_enc read_at write_at sync size in
  let p1 = mk_page 1
  and p2 = mk_page 2 in
  let* () =
    let* r = Wal.append_commit w [ 1L, p1; 2L, p2 ] in
    match r with
    | Ok () -> Lwt.return_unit
    | Error _ -> Alcotest.fail "append"
  in
  let* w2 = open_enc read_at write_at sync size in
  Alcotest.(check int) "recovered committed frames" 2 (Wal.committed_frames w2);
  let* got =
    let* r = Wal.read_frame w2 1 in
    match r with
    | Ok pg -> Lwt.return pg
    | Error _ -> Alcotest.fail "read"
  in
  Alcotest.(check bool) "recovered frame decrypts" true (Cstruct.equal p2 got);
  Lwt.return_unit
;;

(* #219: a frame whose FNV checksum passes but whose GCM tag fails is genuine
   tampering, not a torn tail.  We forge exactly that: commit one frame, flip a
   ciphertext byte on disk, then *repair* the (non-key-derived) checksum over the
   tampered payload so it passes.  Recovery must hard-fail with [Corrupt_frame]
   rather than silently treating it as end-of-WAL and dropping the frame. *)
let test_tamper_detected _ () =
  let read_at, write_at, sync, size = make_dev () in
  let* w = open_enc read_at write_at sync size in
  let page_id = 5L in
  let* () =
    let* r = Wal.append_commit w [ page_id, mk_page 3 ] in
    match r with
    | Ok () -> Lwt.return_unit
    | Error _ -> Alcotest.fail "append"
  in
  (* Frame 0 layout on disk: WAL header (24) + frame meta (24); payload (the
     ciphertext + nonce + tag) starts at 48, the checksum field at 24 + 16 = 40. *)
  let payload_len = 4096 + C.overhead in
  let payload = Cstruct.create payload_len in
  let* () =
    let* r = read_at ~offset:48L payload in
    match r with
    | Ok () -> Lwt.return_unit
    | Error _ -> Alcotest.fail "raw read"
  in
  (* Flip a ciphertext byte and write it back. *)
  Cstruct.set_uint8 payload 0 (Cstruct.get_uint8 payload 0 lxor 0xff);
  let* () =
    let* r = write_at ~offset:48L payload in
    match r with
    | Ok () -> Lwt.return_unit
    | Error _ -> Alcotest.fail "raw write"
  in
  (* Repair the checksum over the tampered payload (flags = 1 for a commit). *)
  let ck =
    Wal.frame_checksum
      ~salt:(Wal.salt w)
      ~seed:(Wal.seed w)
      ~page_id
      ~flags:1L
      ~page:payload
  in
  let ckbuf = Cstruct.create 8 in
  Cstruct.BE.set_uint64 ckbuf 0 ck;
  let* () =
    let* r = write_at ~offset:40L ckbuf in
    match r with
    | Ok () -> Lwt.return_unit
    | Error _ -> Alcotest.fail "checksum write"
  in
  (* Reopen: checksum passes, GCM tag fails → tampering surfaced as a hard error. *)
  let* r = open_enc_result read_at write_at sync size in
  match r with
  | Error (Wal.Corrupt_frame 0) -> Lwt.return_unit
  | Error e -> Alcotest.failf "expected Corrupt_frame 0, got %a" Wal.pp_error e
  | Ok _ -> Alcotest.fail "expected tamper to be detected, got Ok"
;;

(* #246: the decrypted-frame cache must serve a repeated read of a WAL-resident
   frame WITHOUT re-running AES-GCM, while still authenticating once on the
   filling read.  We assert this by counting [Crypto.decrypt_frame] calls. *)
let test_frame_cache_no_redecrypt _ () =
  let read_at, write_at, sync, size = make_dev () in
  let* w = open_enc read_at write_at sync size in
  let p = mk_page 9 in
  let* () =
    let* r = Wal.append_commit w [ 3L, p ] in
    match r with
    | Ok () -> Lwt.return_unit
    | Error _ -> Alcotest.fail "append"
  in
  let read0 () =
    let* r = Wal.read_frame w 0 in
    match r with
    | Ok pg -> Lwt.return pg
    | Error _ -> Alcotest.fail "read"
  in
  let before = C.decrypt_frame_count () in
  let* g1 = read0 () in
  let after_fill = C.decrypt_frame_count () in
  let* g2 = read0 () in
  let after_hit = C.decrypt_frame_count () in
  Alcotest.(check bool) "fill returns correct plaintext" true (Cstruct.equal p g1);
  Alcotest.(check bool) "cache hit returns correct plaintext" true (Cstruct.equal p g2);
  Alcotest.(check int) "fill authenticates exactly once" 1 (after_fill - before);
  Alcotest.(check int) "repeated read does 0 extra decrypts" 0 (after_hit - after_fill);
  Lwt.return_unit
;;

(* #246: [reset] (checkpoint) recycles frame indices.  A cached (idx -> bytes)
   entry from the previous generation MUST NOT be served for the new frame that
   reuses that index — otherwise a reader sees a stale page.  Commit page A at
   idx 0, read it (caching it), reset, commit a DIFFERENT page B at idx 0, and
   require the read to return B (and to re-decrypt, proving the cache was
   dropped). *)
let test_frame_cache_reset_invalidates _ () =
  let read_at, write_at, sync, size = make_dev () in
  let* w = open_enc read_at write_at sync size in
  let pa = mk_page 11
  and pb = mk_page 22 in
  let commit p =
    let* r = Wal.append_commit w [ 4L, p ] in
    match r with
    | Ok () -> Lwt.return_unit
    | Error _ -> Alcotest.fail "append"
  in
  let read0 () =
    let* r = Wal.read_frame w 0 in
    match r with
    | Ok pg -> Lwt.return pg
    | Error _ -> Alcotest.fail "read"
  in
  let* () = commit pa in
  let* a = read0 () in
  Alcotest.(check bool) "pre-reset reads A" true (Cstruct.equal pa a);
  Wal.reset w;
  let* () = commit pb in
  let before = C.decrypt_frame_count () in
  let* b = read0 () in
  let after = C.decrypt_frame_count () in
  Alcotest.(check bool)
    "post-reset idx 0 returns the NEW page B (not stale A)"
    true
    (Cstruct.equal pb b);
  Alcotest.(check bool) "A and B differ (test is meaningful)" false (Cstruct.equal pa pb);
  Alcotest.(check int) "post-reset read re-decrypts (cache was dropped)" 1 (after - before);
  Lwt.return_unit
;;

(* #246: the bounded FIFO must evict the OLDEST-inserted frame once full, so an
   evicted frame re-decrypts on its next read while a still-resident one does
   not.  Open with a tiny capacity (2), fill three distinct frames (idx 0 is
   evicted when idx 2 is inserted), then probe: the newest stays cached (0 extra
   decrypts), the evicted oldest re-decrypts (+1).  Locks the dual-structure
   eviction loop against silent regression. *)
let test_frame_cache_eviction _ () =
  let read_at, write_at, sync, size = make_dev () in
  let* w =
    let* r =
      Wal.open_
        ~cipher:(Some (cipher ()))
        ~frame_cache_capacity:2
        ~read_at
        ~write_at
        ~sync
        ~size_bytes:(size ())
        ()
    in
    match r with
    | Ok w -> Lwt.return w
    | Error _ -> Alcotest.fail "open"
  in
  let pages = [| mk_page 30; mk_page 31; mk_page 32 |] in
  let* () =
    Lwt_list.iteri_s
      (fun i p ->
         let* r = Wal.append_commit w [ Int64.of_int (10 + i), p ] in
         match r with
         | Ok () -> Lwt.return_unit
         | Error _ -> Alcotest.fail "append")
      (Array.to_list pages)
  in
  let read idx =
    let* r = Wal.read_frame w idx in
    match r with
    | Ok pg -> Lwt.return pg
    | Error _ -> Alcotest.fail "read"
  in
  (* Fill: read 0,1,2 with cap 2 -> cache holds {1,2}, idx 0 evicted. *)
  let* _ = read 0 in
  let* _ = read 1 in
  let* _ = read 2 in
  (* Newest (idx 2) is still cached: 0 extra decrypts, correct bytes. *)
  let b0 = C.decrypt_frame_count () in
  let* g2 = read 2 in
  let b1 = C.decrypt_frame_count () in
  Alcotest.(check int) "resident newest frame: no re-decrypt" 0 (b1 - b0);
  Alcotest.(check bool) "resident frame bytes correct" true (Cstruct.equal pages.(2) g2);
  (* Evicted oldest (idx 0) re-decrypts and still returns the right page. *)
  let* g0 = read 0 in
  let b2 = C.decrypt_frame_count () in
  Alcotest.(check int) "evicted oldest frame: re-decrypts once" 1 (b2 - b1);
  Alcotest.(check bool) "evicted frame bytes correct" true (Cstruct.equal pages.(0) g0);
  Lwt.return_unit
;;

let () =
  seed ();
  Lwt_main.run
    (Alcotest_lwt.run
       "wal_crypto"
       [ ( "encrypted"
         , [ Alcotest_lwt.test_case "roundtrip" `Quick test_encrypted_wal_roundtrip
           ; Alcotest_lwt.test_case "ciphertext on disk" `Quick test_ciphertext_on_disk
           ; Alcotest_lwt.test_case
               "recovery after reopen"
               `Quick
               test_recovery_after_reopen
           ; Alcotest_lwt.test_case "tamper detected (#219)" `Quick test_tamper_detected
           ; Alcotest_lwt.test_case
               "frame cache: no re-decrypt on hit (#246)"
               `Quick
               test_frame_cache_no_redecrypt
           ; Alcotest_lwt.test_case
               "frame cache: reset invalidates (#246)"
               `Quick
               test_frame_cache_reset_invalidates
           ; Alcotest_lwt.test_case
               "frame cache: FIFO eviction (#246)"
               `Quick
               test_frame_cache_eviction
           ] )
       ])
;;
