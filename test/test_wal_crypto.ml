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

let open_enc read_at write_at sync size =
  let* r =
    Wal.open_ ~cipher:(Some (cipher ())) ~read_at ~write_at ~sync ~size_bytes:(size ()) ()
  in
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
           ] )
       ])
;;
