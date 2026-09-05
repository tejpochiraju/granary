(** #753: [Db.open_block]'s [init_if_corrupt] parameter.

    Before #753, [Db.open_block] hardcoded [~init_if_corrupt:true] when
    calling {!Granary_store.Store.open_block}, so an existing-but-corrupt
    block device was silently re-initialised as a fresh empty store instead
    of surfacing an error -- invisible data loss for a persisted database.

    This suite pins:
    - the new default ([false]): a corrupt header is refused, and the
      on-disk bytes are left untouched;
    - [~init_if_corrupt:true] explicit: still reinitialises (regression
      guard for the pre-#753 behaviour, now opt-in);
    - the crux finding from the issue's investigation -- {!Store.open_block}
      cannot tell "corrupt" from "genuinely fresh/zeroed": both hit the same
      [Header.Both_headers_corrupt] arm (see [lib/storage/header.ml]'s
      [decode_page], which rejects any page whose CRC does not verify, and a
      zeroed page fails that check exactly like a garbage page).  So a
      brand-new, never-initialised device ALSO needs
      [~init_if_corrupt:true] under the new default -- it is not a
      distinct, "just works" case;
    - a QCheck property: an [Error]-returning open never mutates the
      device's on-disk bytes, for arbitrary (non-header-shaped) garbage. *)

open Lwt.Syntax
module FI = Granary_unix.Fault_inject

let page_size = 4096
let size_bytes = 4 * 1024 * 1024
let counter = ref 0

let tmp_path () =
  incr counter;
  Printf.sprintf
    "/tmp/granary_open_block_753_%d_%d_%d.db"
    (Unix.getpid ())
    !counter
    (Random.int 1_000_000)
;;

let safe_unlink path =
  try Unix.unlink path with
  | _ -> ()
;;

(* A fresh, [size_bytes]-sized, zero-filled file: a genuinely new device
   that has never had a header written to it. *)
let ensure_sized path =
  let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  Unix.ftruncate fd size_bytes;
  Unix.close fd
;;

(* Overwrite the two header pages (0 and 1) with [buf0] / [buf1], each
   exactly [page_size] bytes. Simulates an existing device whose header is
   unreadable (media fault, partial write, wrong device, ...). *)
let write_headers path buf0 buf1 =
  let fd = Unix.openfile path [ Unix.O_RDWR ] 0o644 in
  let _ = Unix.lseek fd 0 Unix.SEEK_SET in
  let _ = Unix.write fd buf0 0 page_size in
  let _ = Unix.lseek fd page_size Unix.SEEK_SET in
  let _ = Unix.write fd buf1 0 page_size in
  Unix.close fd
;;

let corrupt_headers path =
  let garbage = Bytes.make page_size '\xFF' in
  write_headers path garbage garbage
;;

let read_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let b = Bytes.create len in
  really_input ic b 0 len;
  close_in ic;
  b
;;

(* Open [path] through the same read_page/write_page/sync/resize/close
   callback shape [Db.open_block] expects, via the fault-injection wrapper
   with no faults configured (it is used here purely as a Unix-file-backed
   callback source, matching how other tests in this suite reuse it). *)
let open_db ?init_if_corrupt path =
  let* _handle, read_page, write_page, sync, resize, n_pages, close =
    FI.open_with_faults ~path ~size_bytes ~config:FI.default_config
  in
  let* r =
    Granary.Db.open_block
      ?init_if_corrupt
      ~read_page
      ~write_page
      ~sync
      ~resize
      ~n_pages
      ~close
      ()
  in
  match r with
  | Ok db -> Lwt.return (Ok db)
  | Error e ->
    (* No [Db.t] was constructed, so [close] was never wired to anything
       that will call it (see [Store.open_block]: the [close_fn] is stashed
       on the store record it never gets to build).  Close the fd here so
       the test suite doesn't leak file descriptors across dozens of
       QCheck iterations. *)
    let* () = close () in
    Lwt.return (Error e)
;;

let contains hay needle =
  let hl = String.length hay
  and nl = String.length needle in
  let rec go i =
    if i > hl - nl
    then false
    else if String.sub hay i nl = needle
    then true
    else go (i + 1)
  in
  go 0
;;

let error_msg = function
  | Granary.Db.Runtime s -> s
  | e -> Format.asprintf "%a" Granary.Db.pp_error e
;;

(* ------------------------------------------------------------------ *)
(* 1. Default (false): corrupt header -> Error, bytes untouched         *)
(* ------------------------------------------------------------------ *)

let test_corrupt_default_errors_and_preserves_bytes () =
  let path = tmp_path () in
  Lwt_main.run
    (Lwt.finalize
       (fun () ->
          ensure_sized path;
          corrupt_headers path;
          let before = read_file path in
          let* r = open_db path in
          let* () =
            match r with
            | Ok db ->
              let* () = Granary.Db.close db in
              Alcotest.fail
                "expected Error (Header_error via Runtime) for a corrupt header, got Ok"
            | Error e ->
              let msg = error_msg e in
              Alcotest.(check bool)
                "error message names Header_error"
                true
                (contains msg "Header_error");
              Lwt.return_unit
          in
          let after = read_file path in
          Alcotest.(check bool)
            "on-disk bytes unchanged after a refused open"
            true
            (Bytes.equal before after);
          Lwt.return_unit)
       (fun () ->
          safe_unlink path;
          Lwt.return_unit))
;;

(* ------------------------------------------------------------------ *)
(* 2. init_if_corrupt:true explicit: still reinitialises (regression)  *)
(* ------------------------------------------------------------------ *)

let test_corrupt_explicit_true_reinitializes () =
  let path = tmp_path () in
  Lwt_main.run
    (Lwt.finalize
       (fun () ->
          ensure_sized path;
          corrupt_headers path;
          let before = read_file path in
          let* r = open_db ~init_if_corrupt:true path in
          let* () =
            match r with
            | Error e ->
              Alcotest.failf
                "expected Ok with ~init_if_corrupt:true, got Error: %s"
                (error_msg e)
            | Ok db ->
              (* Prove the reinitialised store is actually usable, not just
                 an [Ok] that immediately breaks. *)
              let* exec_r =
                Granary.Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY)"
              in
              let* () =
                match exec_r with
                | Ok () -> Lwt.return_unit
                | Error e ->
                  Alcotest.failf
                    "CREATE TABLE on reinitialised store failed: %s"
                    (error_msg e)
              in
              Granary.Db.close db
          in
          let after = read_file path in
          Alcotest.(check bool)
            "header bytes changed: device was actually reinitialised"
            false
            (Bytes.equal before after);
          (* Reopening with the new default now succeeds: the header is a
             valid, non-corrupt header after the explicit reinit above. *)
          let* r2 = open_db path in
          match r2 with
          | Ok db -> Granary.Db.close db
          | Error e ->
            Alcotest.failf
              "expected default open to succeed on a now-valid header, got: %s"
              (error_msg e))
       (fun () ->
          safe_unlink path;
          Lwt.return_unit))
;;

(* ------------------------------------------------------------------ *)
(* 3. A genuinely fresh (zeroed) device also needs init_if_corrupt:true *)
(* ------------------------------------------------------------------ *)

let test_fresh_zeroed_device_needs_explicit_init () =
  let path = tmp_path () in
  Lwt_main.run
    (Lwt.finalize
       (fun () ->
          ensure_sized path;
          (* No corruption applied: this is a plain zeroed, never-opened
             device -- the "first boot of a new device" case. *)
          let before = read_file path in
          let* r = open_db path in
          let* () =
            match r with
            | Ok db ->
              let* () = Granary.Db.close db in
              Alcotest.fail
                "expected the default (false) to refuse a fresh zeroed device too"
            | Error e ->
              let msg = error_msg e in
              Alcotest.(check bool)
                "fresh-device error also names Header_error (same code path as corrupt)"
                true
                (contains msg "Header_error");
              Lwt.return_unit
          in
          let after = read_file path in
          Alcotest.(check bool)
            "fresh device bytes unchanged after a refused open"
            true
            (Bytes.equal before after);
          (* The legitimate provisioning path: explicit true succeeds. *)
          let* r2 = open_db ~init_if_corrupt:true path in
          match r2 with
          | Ok db -> Granary.Db.close db
          | Error e ->
            Alcotest.failf
              "expected ~init_if_corrupt:true to provision a fresh device, got: %s"
              (error_msg e))
       (fun () ->
          safe_unlink path;
          Lwt.return_unit))
;;

(* ------------------------------------------------------------------ *)
(* 4. QCheck: an Error-returning open never mutates on-disk bytes       *)
(* ------------------------------------------------------------------ *)

let arb_header_pair =
  let open QCheck.Gen in
  let page_bytes = map Bytes.of_string (string_size ~gen:char (return page_size)) in
  QCheck.make
    ~print:(fun (a, b) ->
      Printf.sprintf
        "header0[0..8)=%S header1[0..8)=%S"
        (Bytes.sub_string a 0 8)
        (Bytes.sub_string b 0 8))
    (map2 (fun a b -> a, b) page_bytes page_bytes)
;;

let prop_error_open_preserves_bytes =
  QCheck.Test.make
    ~count:200
    ~name:"a refused Db.open_block never mutates the device's on-disk bytes"
    arb_header_pair
    (fun (buf0, buf1) ->
       Lwt_main.run
         (let path = tmp_path () in
          Lwt.finalize
            (fun () ->
               ensure_sized path;
               write_headers path buf0 buf1;
               let before = read_file path in
               let* r = open_db path in
               match r with
               | Ok db ->
                 (* Astronomically unlikely (random bytes happening to
                    decode as a valid, CRC-correct header page): treat as
                    an inconclusive draw rather than fail the property. *)
                 let* () = Granary.Db.close db in
                 Lwt.return true
               | Error _ ->
                 let after = read_file path in
                 Lwt.return (Bytes.equal before after))
            (fun () ->
               safe_unlink path;
               Lwt.return_unit)))
;;

(* ------------------------------------------------------------------ *)
(* Runner                                                                *)
(* ------------------------------------------------------------------ *)

let () =
  let qcheck_tests =
    List.map QCheck_alcotest.to_alcotest [ prop_error_open_preserves_bytes ]
  in
  Alcotest.run
    "open_block_init_753"
    [ ( "init_if_corrupt"
      , [ Alcotest.test_case
            "corrupt header, default false: Error + bytes preserved"
            `Quick
            test_corrupt_default_errors_and_preserves_bytes
        ; Alcotest.test_case
            "corrupt header, explicit true: reinitialises (regression)"
            `Quick
            test_corrupt_explicit_true_reinitializes
        ; Alcotest.test_case
            "fresh zeroed device also needs explicit true"
            `Quick
            test_fresh_zeroed_device_needs_explicit_init
        ] )
    ; "properties", qcheck_tests
    ]
;;
