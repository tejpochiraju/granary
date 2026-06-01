(** Integration tests for opt-in page encryption in the store open paths (#84).

    Exercises [Store.open_block]'s [?key] parameter: a round-trip over a
    persistent (shared-Hashtbl) device, plus the three key/header mismatch
    error cases.  The pager and B+-tree only ever see plaintext; the device
    holds ciphertext for pages >= 2. *)

open Lwt.Syntax
module S = Sqlocaml_store.Store
module Geometry = Sqlocaml_storage.Geometry

let bs s = Bytes.of_string s
let run = Lwt_main.run

let bytes_opt_eq =
  Alcotest.(
    option
      (testable (fun ppf b -> Format.fprintf ppf "%S" (Bytes.to_string b)) Bytes.equal))
;;

(* ------------------------------------------------------------------ *)
(* In-memory page device, shared across reopen via one Hashtbl.        *)
(* Pages are [page_size] bytes; the geometry's reserved tail lives      *)
(* inside that fixed-size page.                                         *)
(* ------------------------------------------------------------------ *)

let page_size = 4096

type device = { store : (int64, Bytes.t) Hashtbl.t }

let make_device () = { store = Hashtbl.create 16 }

let callbacks dev =
  let read_page ~page_id buf =
    (match Hashtbl.find_opt dev.store page_id with
     | None -> Cstruct.memset buf 0
     | Some bytes -> Cstruct.blit_from_bytes bytes 0 buf 0 page_size);
    Lwt.return_ok ()
  in
  let write_page ~page_id buf =
    let bytes = Bytes.create page_size in
    Cstruct.blit_to_bytes buf 0 bytes 0 page_size;
    Hashtbl.replace dev.store page_id bytes;
    Lwt.return_ok ()
  in
  let sync () = Lwt.return_ok () in
  let resize ~n_pages:_ = Lwt.return_ok () in
  read_page, write_page, sync, resize
;;

let enc_geom = Geometry.create ~page_size ~reserved_bytes_per_page:32 |> Result.get_ok

(* Open the device, optionally encrypted.  [init] toggles init_if_corrupt. *)
let open_dev ?key ?(init = true) dev =
  let read_page, write_page, sync, resize = callbacks dev in
  S.open_block
    ?key
    ~geom:enc_geom
    ~init_if_corrupt:init
    ~read_page
    ~write_page
    ~sync
    ~resize
    ~n_pages:0L
    ~close:(fun () -> Lwt.return_unit)
    ()
;;

let k32 = String.make 32 'k'

(* ------------------------------------------------------------------ *)
(* 1. round-trip                                                       *)
(* ------------------------------------------------------------------ *)

let test_round_trip () =
  run
    (let dev = make_device () in
     let* r = open_dev ~key:k32 dev in
     let s =
       match r with
       | Ok s -> s
       | Error e -> Alcotest.failf "fresh encrypted open failed: %a" S.pp_error e
     in
     let* tx = S.rw_begin s in
     let* () = S.put tx 16 (bs "k") (bs "v") in
     let* () = S.commit tx in
     let* () = S.close s in
     (* Reopen the SAME device with the SAME key. *)
     let* r2 = open_dev ~key:k32 dev in
     let s2 =
       match r2 with
       | Ok s -> s
       | Error e -> Alcotest.failf "reopen encrypted failed: %a" S.pp_error e
     in
     let* got = S.with_ro s2 (fun tx -> S.get tx 16 (bs "k")) in
     Alcotest.check bytes_opt_eq "round-trip value" (Some (bs "v")) got;
     S.close s2)
;;

(* ------------------------------------------------------------------ *)
(* 2. key required                                                     *)
(* ------------------------------------------------------------------ *)

let seed_encrypted_dev () =
  let dev = make_device () in
  run
    (let* r = open_dev ~key:k32 dev in
     let s =
       match r with
       | Ok s -> s
       | Error e -> Alcotest.failf "seed encrypted open failed: %a" S.pp_error e
     in
     let* tx = S.rw_begin s in
     let* () = S.put tx 16 (bs "k") (bs "v") in
     let* () = S.commit tx in
     S.close s);
  dev
;;

let test_key_required () =
  let dev = seed_encrypted_dev () in
  run
    (let* r = open_dev ~init:false dev in
     match r with
     | Error S.Encryption_key_required -> Lwt.return_unit
     | Error e -> Alcotest.failf "expected Encryption_key_required, got %a" S.pp_error e
     | Ok _ -> Alcotest.fail "expected Encryption_key_required, got Ok")
;;

(* ------------------------------------------------------------------ *)
(* 3. key mismatch                                                     *)
(* ------------------------------------------------------------------ *)

let test_key_mismatch () =
  let dev = seed_encrypted_dev () in
  run
    (let* r = open_dev ~key:(String.make 32 'x') ~init:false dev in
     match r with
     | Error S.Encryption_key_mismatch -> Lwt.return_unit
     | Error e -> Alcotest.failf "expected Encryption_key_mismatch, got %a" S.pp_error e
     | Ok _ -> Alcotest.fail "expected Encryption_key_mismatch, got Ok")
;;

(* ------------------------------------------------------------------ *)
(* 4. not encrypted                                                    *)
(* ------------------------------------------------------------------ *)

let test_not_encrypted () =
  let dev = make_device () in
  run
    (let* r = open_dev dev in
     let s =
       match r with
       | Ok s -> s
       | Error e -> Alcotest.failf "fresh plaintext open failed: %a" S.pp_error e
     in
     let* tx = S.rw_begin s in
     let* () = S.put tx 16 (bs "k") (bs "v") in
     let* () = S.commit tx in
     let* () = S.close s in
     let* r2 = open_dev ~key:k32 ~init:false dev in
     match r2 with
     | Error S.Not_encrypted -> Lwt.return_unit
     | Error e -> Alcotest.failf "expected Not_encrypted, got %a" S.pp_error e
     | Ok _ -> Alcotest.fail "expected Not_encrypted, got Ok")
;;

let () =
  Mirage_crypto_rng_unix.use_default ();
  Alcotest.run
    "store_crypto"
    [ ( "open_block"
      , [ Alcotest.test_case "round_trip" `Quick test_round_trip
        ; Alcotest.test_case "key_required" `Quick test_key_required
        ; Alcotest.test_case "key_mismatch" `Quick test_key_mismatch
        ; Alcotest.test_case "not_encrypted" `Quick test_not_encrypted
        ] )
    ]
;;
