(** #217: opening (or creating) an encrypted store needs the {!Mirage_crypto_rng}
    to be seeded so per-page nonces can be drawn.  [lib/] never seeds (it stays
    Mirage-clean), so a forgetful application would otherwise hit a raw
    [Unseeded_generator] exception on the first encrypted write.  The open paths
    probe the RNG up front and map that to a clean [Encryption_rng_unseeded]
    error.

    This executable deliberately {b never seeds the RNG} (note the absence of
    [Mirage_crypto_rng_unix.use_default ()]), so a fresh encrypted open must
    fail cleanly rather than raise. *)

open Lwt.Syntax
module S = Granary_store.Store
module Geometry = Granary_storage.Geometry

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
let k32 = String.make 32 'k'

(* A plaintext open must still succeed with an unseeded RNG (no nonce needed). *)
let test_plaintext_ok_unseeded () =
  Lwt_main.run
    (let dev = make_device () in
     let read_page, write_page, sync, resize = callbacks dev in
     let* r =
       S.open_block
         ~geom:enc_geom
         ~init_if_corrupt:true
         ~read_page
         ~write_page
         ~sync
         ~resize
         ~n_pages:0L
         ~close:(fun () -> Lwt.return_unit)
         ()
     in
     match r with
     | Ok s -> S.close s
     | Error e -> Alcotest.failf "plaintext open should not need the RNG: %a" S.pp_error e)
;;

(* An encrypted open with the RNG unseeded must fail with the clean error. *)
let test_encrypted_unseeded () =
  Lwt_main.run
    (let dev = make_device () in
     let read_page, write_page, sync, resize = callbacks dev in
     let* r =
       S.open_block
         ~key:k32
         ~geom:enc_geom
         ~init_if_corrupt:true
         ~read_page
         ~write_page
         ~sync
         ~resize
         ~n_pages:0L
         ~close:(fun () -> Lwt.return_unit)
         ()
     in
     match r with
     | Error S.Encryption_rng_unseeded -> Lwt.return_unit
     | Error e -> Alcotest.failf "expected Encryption_rng_unseeded, got %a" S.pp_error e
     | Ok _ -> Alcotest.fail "expected Encryption_rng_unseeded, got Ok")
;;

let () =
  (* Intentionally NOT seeding the RNG. *)
  Alcotest.run
    "store_crypto_unseeded"
    [ ( "open_block"
      , [ Alcotest.test_case "plaintext ok unseeded" `Quick test_plaintext_ok_unseeded
        ; Alcotest.test_case "encrypted unseeded errors" `Quick test_encrypted_unseeded
        ] )
    ]
;;
