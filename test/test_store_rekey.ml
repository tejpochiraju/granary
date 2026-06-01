(** #215: offline key rotation tests. *)

open Lwt.Syntax
module S = Sqlocaml_store.Store
module UnixStore = Sqlocaml_unix.Store

let bs s = Bytes.of_string s
let run = Lwt_main.run
let counter = ref 0

let fresh_path tag =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_rekey_%04d_%s.db" n tag
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

let bytes_opt_eq =
  Alcotest.(
    option
      (testable (fun ppf b -> Format.fprintf ppf "%S" (Bytes.to_string b)) Bytes.equal))
;;

let ok_store : (S.t, S.error) result -> S.t = function
  | Ok t -> t
  | Error e -> Alcotest.failf "open error: %a" S.pp_error e
;;

let k32 c = String.make 32 c

let raw_contains path needle =
  let ic = open_in_bin path in
  let raw = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let nl = String.length needle
  and hl = String.length raw in
  let rec go i = i + nl <= hl && (String.sub raw i nl = needle || go (i + 1)) in
  nl > 0 && go 0
;;

(* rotate K1 -> K2: new key reads, old key fails, third key fails, no plaintext. *)
let test_rotation_round_trip () =
  let src = fresh_path "src" in
  let dst = fresh_path "dst" in
  cleanup src;
  cleanup dst;
  run
  @@ Lwt.finalize
       (fun () ->
          let k1 = k32 '1'
          and k2 = k32 '2'
          and k3 = k32 '3' in
          let* s = UnixStore.open_file ~key:k1 ~path:src () in
          let s = ok_store s in
          let* () =
            let* tx = S.rw_begin s in
            let* () = S.put tx 16 (bs "kk") (bs "ROTATE_MARKER_99") in
            S.commit tx
          in
          let* () = S.close s in
          let* rr =
            UnixStore.rotate_key_file ~src_path:src ~old_key:k1 ~new_key:k2 ~dest:dst
          in
          (match rr with
           | Error e -> Alcotest.failf "rotate error: %a" S.pp_error e
           | Ok () -> ());
          (* new key round-trips *)
          let* d = UnixStore.open_file ~key:k2 ~path:dst () in
          let d = ok_store d in
          let* got = S.with_ro d (fun tx -> S.get tx 16 (bs "kk")) in
          Alcotest.check
            bytes_opt_eq
            "value under new key"
            (Some (bs "ROTATE_MARKER_99"))
            got;
          let* () = S.close d in
          (* old key now fails *)
          let* o = UnixStore.open_file ~key:k1 ~path:dst () in
          (match o with
           | Error S.Encryption_key_mismatch -> ()
           | Error e ->
             Alcotest.failf "expected mismatch under old key, got %a" S.pp_error e
           | Ok _ -> Alcotest.fail "old key should fail after rotation");
          (* unrelated third key fails *)
          let* o3 = UnixStore.open_file ~key:k3 ~path:dst () in
          (match o3 with
           | Error S.Encryption_key_mismatch -> ()
           | Error e -> Alcotest.failf "expected mismatch under k3, got %a" S.pp_error e
           | Ok _ -> Alcotest.fail "k3 should fail");
          Alcotest.(check bool)
            "no plaintext on disk"
            false
            (raw_contains dst "ROTATE_MARKER_99");
          Lwt.return_unit)
       (fun () ->
          cleanup src;
          cleanup dst;
          Lwt.return_unit)
;;

(* A wrong old_key surfaces Encryption_key_mismatch from the source open. *)
let test_rotation_wrong_old_key () =
  let src = fresh_path "wsrc" in
  let dst = fresh_path "wdst" in
  cleanup src;
  cleanup dst;
  run
  @@ Lwt.finalize
       (fun () ->
          let* s = UnixStore.open_file ~key:(k32 '1') ~path:src () in
          let s = ok_store s in
          let* () =
            let* tx = S.rw_begin s in
            let* () = S.put tx 16 (bs "k") (bs "v") in
            S.commit tx
          in
          let* () = S.close s in
          let* rr =
            UnixStore.rotate_key_file
              ~src_path:src
              ~old_key:(k32 'x')
              ~new_key:(k32 '2')
              ~dest:dst
          in
          (match rr with
           | Error S.Encryption_key_mismatch -> ()
           | Error e -> Alcotest.failf "expected key_mismatch, got %a" S.pp_error e
           | Ok () -> Alcotest.fail "expected key_mismatch on wrong old key");
          Lwt.return_unit)
       (fun () ->
          cleanup src;
          cleanup dst;
          Lwt.return_unit)
;;

(* rekey_to rejects a plaintext source. *)
let test_reject_plaintext () =
  let src = fresh_path "plain" in
  cleanup src;
  run
  @@ Lwt.finalize
       (fun () ->
          let* s = UnixStore.open_file ~path:src () in
          let s = ok_store s in
          let* () =
            let* tx = S.rw_begin s in
            let* () = S.put tx 16 (bs "k") (bs "v") in
            S.commit tx
          in
          let sink : S.page_sink = fun ~page_id:_ ~page:_ -> Lwt.return_unit in
          let* r = S.rekey_to s ~new_key:(k32 '2') sink in
          let* () = S.close s in
          (match r with
           | Error S.Not_encrypted -> ()
           | Error e -> Alcotest.failf "expected Not_encrypted, got %a" S.pp_error e
           | Ok () -> Alcotest.fail "expected Not_encrypted on plaintext source");
          Lwt.return_unit)
       (fun () ->
          cleanup src;
          Lwt.return_unit)
;;

(* rekey_to rejects a wrong-length new key. *)
let test_reject_bad_key_len () =
  let src = fresh_path "enc" in
  cleanup src;
  run
  @@ Lwt.finalize
       (fun () ->
          let* s = UnixStore.open_file ~key:(k32 '1') ~path:src () in
          let s = ok_store s in
          let* () =
            let* tx = S.rw_begin s in
            let* () = S.put tx 16 (bs "k") (bs "v") in
            S.commit tx
          in
          let sink : S.page_sink = fun ~page_id:_ ~page:_ -> Lwt.return_unit in
          let* r = S.rekey_to s ~new_key:"too-short" sink in
          let* () = S.close s in
          (match r with
           | Error (S.Block_error _) -> ()
           | Error e -> Alcotest.failf "expected Block_error, got %a" S.pp_error e
           | Ok () -> Alcotest.fail "expected Block_error on bad key length");
          Lwt.return_unit)
       (fun () ->
          cleanup src;
          Lwt.return_unit)
;;

let () =
  Mirage_crypto_rng_unix.use_default ();
  Alcotest.run
    "store_rekey"
    [ ( "rotation"
      , [ Alcotest.test_case "round_trip" `Quick test_rotation_round_trip
        ; Alcotest.test_case "wrong_old_key" `Quick test_rotation_wrong_old_key
        ; Alcotest.test_case "reject_plaintext" `Quick test_reject_plaintext
        ; Alcotest.test_case "reject_bad_key_len" `Quick test_reject_bad_key_len
        ] )
    ]
;;
