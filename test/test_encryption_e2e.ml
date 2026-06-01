(** End-to-end encryption-at-rest tests for the Unix file driver (#84).

    These are the real security validation for opt-in page encryption: they
    drive the full Unix-file open paths ({!Sqlocaml_unix.Store.open_file} and
    [open_file_wal]) with a [?key], round-trip data across reopen, exercise the
    three key/header error cases, recover an encrypted WAL after a crash (no
    checkpoint), and — critically — assert that a known plaintext canary value
    never appears in the raw bytes of either the main DB file or its WAL
    sidecar. *)

open Lwt.Syntax
module S = Sqlocaml_store.Store
module UnixStore = Sqlocaml_unix.Store

let bs s = Bytes.of_string s
let run = Lwt_main.run

let bytes_opt_eq =
  Alcotest.(
    option
      (testable (fun ppf b -> Format.fprintf ppf "%S" (Bytes.to_string b)) Bytes.equal))
;;

let secret = "TOPSECRET_CANARY_VALUE_8675309"
let k = String.make 32 'K'
let k2 = String.make 32 'Z'
let counter = ref 0

(* A fresh temp path.  [Filename.temp_file] leaves a zero-byte file behind,
   which the driver treats as fresh, but removing it is cleaner. *)
let tmp_path () =
  incr counter;
  let p = Printf.sprintf "/tmp/sqlocaml_enc_%d_%d.db" (Unix.getpid ()) !counter in
  (try Sys.remove p with
   | _ -> ());
  p
;;

let cleanup path =
  (try Sys.remove path with
   | _ -> ());
  try Sys.remove (path ^ "-wal") with
  | _ -> ()
;;

let ok_store ~what : (S.t, S.error) result -> S.t = function
  | Ok t -> t
  | Error e -> Alcotest.failf "%s: open error: %a" what S.pp_error e
;;

(* Substring search: does [needle] occur literally anywhere in the file at
   [path]?  Reads the raw on-disk bytes. *)
let file_contains path needle =
  In_channel.with_open_bin path (fun ic ->
    let s = In_channel.input_all ic in
    let nl = String.length needle
    and sl = String.length s in
    let rec scan i = i + nl <= sl && (String.sub s i nl = needle || scan (i + 1)) in
    nl <= sl && scan 0)
;;

(* ------------------------------------------------------------------ *)
(* 1. non-WAL round-trip across reopen                                 *)
(* ------------------------------------------------------------------ *)

let test_non_wal_round_trip () =
  let path = tmp_path () in
  run
  @@ Lwt.finalize
       (fun () ->
          let* r = UnixStore.open_file ~key:k ~path () in
          let s = ok_store ~what:"fresh encrypted open" r in
          let* tx = S.rw_begin s in
          let* () = S.put tx 16 (bs "row1") (bs secret) in
          let* () = S.commit tx in
          let* () = S.close s in
          let* r2 = UnixStore.open_file ~key:k ~path () in
          let s2 = ok_store ~what:"reopen encrypted" r2 in
          let* got = S.with_ro s2 (fun tx -> S.get tx 16 (bs "row1")) in
          Alcotest.check bytes_opt_eq "round-trip value" (Some (bs secret)) got;
          S.close s2)
       (fun () ->
          cleanup path;
          Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 2. key required                                                     *)
(* ------------------------------------------------------------------ *)

let test_key_required () =
  let path = tmp_path () in
  run
  @@ Lwt.finalize
       (fun () ->
          let* r = UnixStore.open_file ~key:k ~path () in
          let s = ok_store ~what:"seed encrypted open" r in
          let* tx = S.rw_begin s in
          let* () = S.put tx 16 (bs "row1") (bs secret) in
          let* () = S.commit tx in
          let* () = S.close s in
          let* r2 = UnixStore.open_file ~path () in
          match r2 with
          | Error S.Encryption_key_required -> Lwt.return_unit
          | Error e ->
            Alcotest.failf "expected Encryption_key_required, got %a" S.pp_error e
          | Ok _ -> Alcotest.fail "expected Encryption_key_required, got Ok")
       (fun () ->
          cleanup path;
          Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 3. key mismatch                                                     *)
(* ------------------------------------------------------------------ *)

let test_key_mismatch () =
  let path = tmp_path () in
  run
  @@ Lwt.finalize
       (fun () ->
          let* r = UnixStore.open_file ~key:k ~path () in
          let s = ok_store ~what:"seed encrypted open" r in
          let* tx = S.rw_begin s in
          let* () = S.put tx 16 (bs "row1") (bs secret) in
          let* () = S.commit tx in
          let* () = S.close s in
          let* r2 = UnixStore.open_file ~key:k2 ~path () in
          match r2 with
          | Error S.Encryption_key_mismatch -> Lwt.return_unit
          | Error e ->
            Alcotest.failf "expected Encryption_key_mismatch, got %a" S.pp_error e
          | Ok _ -> Alcotest.fail "expected Encryption_key_mismatch, got Ok")
       (fun () ->
          cleanup path;
          Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 4. not encrypted                                                    *)
(* ------------------------------------------------------------------ *)

let test_not_encrypted () =
  let path = tmp_path () in
  run
  @@ Lwt.finalize
       (fun () ->
          let* r = UnixStore.open_file ~path () in
          let s = ok_store ~what:"fresh plaintext open" r in
          let* tx = S.rw_begin s in
          let* () = S.put tx 16 (bs "row1") (bs "value1") in
          let* () = S.commit tx in
          let* () = S.close s in
          let* r2 = UnixStore.open_file ~key:k ~path () in
          match r2 with
          | Error S.Not_encrypted -> Lwt.return_unit
          | Error e -> Alcotest.failf "expected Not_encrypted, got %a" S.pp_error e
          | Ok _ -> Alcotest.fail "expected Not_encrypted, got Ok")
       (fun () ->
          cleanup path;
          Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 5. WAL round-trip + checkpoint                                      *)
(* ------------------------------------------------------------------ *)

let test_wal_round_trip () =
  let path = tmp_path () in
  run
  @@ Lwt.finalize
       (fun () ->
          let* r = UnixStore.open_file_wal ~key:k ~path () in
          let s = ok_store ~what:"fresh encrypted WAL open" r in
          let* tx = S.rw_begin s in
          let* () = S.put tx 16 (bs "row1") (bs secret) in
          let* () = S.put tx 16 (bs "row2") (bs "second") in
          let* () = S.commit tx in
          let* () = S.checkpoint s in
          let* () = S.close s in
          let* r2 = UnixStore.open_file_wal ~key:k ~path () in
          let s2 = ok_store ~what:"reopen encrypted WAL" r2 in
          let* () =
            S.with_ro s2 (fun tx ->
              let* v1 = S.get tx 16 (bs "row1") in
              Alcotest.check bytes_opt_eq "row1" (Some (bs secret)) v1;
              let* v2 = S.get tx 16 (bs "row2") in
              Alcotest.check bytes_opt_eq "row2" (Some (bs "second")) v2;
              Lwt.return_unit)
          in
          S.close s2)
       (fun () ->
          cleanup path;
          Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 6. WAL crash recovery (no checkpoint) + 7. plaintext-leak guard      *)
(* ------------------------------------------------------------------ *)

let test_wal_crash_recovery_and_leak_guard () =
  let path = tmp_path () in
  let wal_path = path ^ "-wal" in
  run
  @@ Lwt.finalize
       (fun () ->
          let* r = UnixStore.open_file_wal ~key:k ~path () in
          let s = ok_store ~what:"fresh encrypted WAL open" r in
          let* tx = S.rw_begin s in
          let* () = S.put tx 16 (bs "row1") (bs secret) in
          let* () = S.put tx 16 (bs "row2") (bs "second") in
          let* () = S.commit tx in
          (* Close WITHOUT checkpoint: data lives only in the encrypted WAL. *)
          let* () = S.close s in
          (* --- plaintext-leak guard (case 7), before recovery rewrites
             anything --- *)
          Alcotest.(check bool)
            "secret absent from main DB file"
            false
            (file_contains path secret);
          let wal_exists =
            try
              let st = Unix.stat wal_path in
              st.Unix.st_size > 0
            with
            | Unix.Unix_error _ -> false
          in
          if wal_exists
          then
            Alcotest.(check bool)
              "secret absent from WAL file"
              false
              (file_contains wal_path secret);
          (* --- recovery from the encrypted WAL --- *)
          let* r2 = UnixStore.open_file_wal ~key:k ~path () in
          let s2 = ok_store ~what:"reopen encrypted WAL (recovery)" r2 in
          let* () =
            S.with_ro s2 (fun tx ->
              let* v1 = S.get tx 16 (bs "row1") in
              Alcotest.check bytes_opt_eq "recovered row1" (Some (bs secret)) v1;
              let* v2 = S.get tx 16 (bs "row2") in
              Alcotest.check bytes_opt_eq "recovered row2" (Some (bs "second")) v2;
              Lwt.return_unit)
          in
          let* () = S.close s2 in
          (* Re-check the leak guard after recovery rewrote the main DB. *)
          Alcotest.(check bool)
            "secret absent from main DB after recovery"
            false
            (file_contains path secret);
          let wal_exists2 =
            try
              let st = Unix.stat wal_path in
              st.Unix.st_size > 0
            with
            | Unix.Unix_error _ -> false
          in
          if wal_exists2
          then
            Alcotest.(check bool)
              "secret absent from WAL after recovery"
              false
              (file_contains wal_path secret);
          Lwt.return_unit)
       (fun () ->
          cleanup path;
          Lwt.return_unit)
;;

let () =
  Mirage_crypto_rng_unix.use_default ();
  Alcotest.run
    "encryption_e2e"
    [ ( "non-wal"
      , [ Alcotest.test_case "round_trip" `Quick test_non_wal_round_trip
        ; Alcotest.test_case "key_required" `Quick test_key_required
        ; Alcotest.test_case "key_mismatch" `Quick test_key_mismatch
        ; Alcotest.test_case "not_encrypted" `Quick test_not_encrypted
        ] )
    ; ( "wal"
      , [ Alcotest.test_case "round_trip_checkpoint" `Quick test_wal_round_trip
        ; Alcotest.test_case
            "crash_recovery_and_leak_guard"
            `Quick
            test_wal_crash_recovery_and_leak_guard
        ] )
    ]
;;
