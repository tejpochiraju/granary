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
           ] )
       ])
;;
