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
      device's on-disk bytes, for arbitrary (non-header-shaped) garbage;
    - #763 review finding: [Db.open_block] must invoke the caller's
      [~close] callback exactly once when it returns [Error] (there is no
      [Db.t] for [Db.close] to reach later), and must NOT invoke it on an
      [Ok] return -- that stays the caller's responsibility via [Db.close];
    - #763 second review finding: a [~close] that raises must not clobber
      the [Header_error] [Db.open_block] is in the middle of returning --
      the call is guarded, and the store error still surfaces. *)

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
   callback source, matching how other tests in this suite reuse it).

   #753/#763: [Db.open_block] itself closes [~close] on any [Error] return
   (there is no [Db.t] to hang it off), so callers must NOT close again on
   that path -- this helper deliberately does nothing extra here, and
   [test_close_invoked_once_on_error] below pins that the callback is
   actually invoked rather than merely trusting the doc comment. *)
let open_db ?init_if_corrupt path =
  let* _handle, read_page, write_page, sync, resize, n_pages, close =
    FI.open_with_faults ~path ~size_bytes ~config:FI.default_config
  in
  Granary.Db.open_block
    ?init_if_corrupt
    ~read_page
    ~write_page
    ~sync
    ~resize
    ~n_pages
    ~close
    ()
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
(* 4. #763: close-on-error is wired, and only on the error path         *)
(* ------------------------------------------------------------------ *)

(* Wrap the callbacks from [FI.open_with_faults] with a counting [close] so
   we can observe how many times [Db.open_block] itself invokes it,
   independent of any [Db.close] the test calls afterwards. *)
let open_db_counting_closes ?init_if_corrupt path =
  let* _handle, read_page, write_page, sync, resize, n_pages, close =
    FI.open_with_faults ~path ~size_bytes ~config:FI.default_config
  in
  let close_calls = ref 0 in
  let counting_close () =
    incr close_calls;
    close ()
  in
  let* r =
    Granary.Db.open_block
      ?init_if_corrupt
      ~read_page
      ~write_page
      ~sync
      ~resize
      ~n_pages
      ~close:counting_close
      ()
  in
  Lwt.return (r, close_calls)
;;

let test_close_invoked_once_on_error () =
  let path = tmp_path () in
  Lwt_main.run
    (Lwt.finalize
       (fun () ->
          ensure_sized path;
          corrupt_headers path;
          let* r, close_calls = open_db_counting_closes path in
          let* () =
            match r with
            | Ok db ->
              let* () = Granary.Db.close db in
              Alcotest.fail "expected Error for a corrupt header"
            | Error _ -> Lwt.return_unit
          in
          Alcotest.(check int)
            "Db.open_block invokes ~close exactly once when it returns Error (no Db.t \
             exists to hang the caller's handle off, so this is the only place it can be \
             released -- otherwise every refused open under the new default leaks the \
             fd)"
            1
            !close_calls;
          Lwt.return_unit)
       (fun () ->
          safe_unlink path;
          Lwt.return_unit))
;;

let test_close_not_invoked_early_on_ok () =
  let path = tmp_path () in
  Lwt_main.run
    (Lwt.finalize
       (fun () ->
          ensure_sized path;
          let* r, close_calls = open_db_counting_closes ~init_if_corrupt:true path in
          match r with
          | Error e ->
            Alcotest.failf
              "expected Ok provisioning a fresh device, got: %s"
              (error_msg e)
          | Ok db ->
            Alcotest.(check int)
              "~close is not invoked by Db.open_block itself on an Ok return"
              0
              !close_calls;
            let* () = Granary.Db.close db in
            Alcotest.(check int)
              "~close is invoked exactly once, by Db.close, once the caller is done"
              1
              !close_calls;
            Lwt.return_unit)
       (fun () ->
          safe_unlink path;
          Lwt.return_unit))
;;

(* A caller's [~close] is not required to swallow its own errors the way
   this repo's [Unix_file] / [Fault_inject] callbacks do.  If it raises
   synchronously, [Db.open_block] must still return the store's
   [Header_error] rather than letting the exception escape and clobber it
   (#763 review). *)
let test_close_raising_does_not_clobber_error () =
  let path = tmp_path () in
  Lwt_main.run
    (Lwt.finalize
       (fun () ->
          ensure_sized path;
          corrupt_headers path;
          let* _handle, read_page, write_page, sync, resize, n_pages, real_close =
            FI.open_with_faults ~path ~size_bytes ~config:FI.default_config
          in
          let raising_close () : unit Lwt.t = failwith "close blew up (test double)" in
          let* r =
            Granary.Db.open_block
              ~read_page
              ~write_page
              ~sync
              ~resize
              ~n_pages
              ~close:raising_close
              ()
          in
          let* () =
            match r with
            | Ok db ->
              let* () = Granary.Db.close db in
              Alcotest.fail "expected Error for a corrupt header"
            | Error e ->
              let msg = error_msg e in
              Alcotest.(check bool)
                "a raising ~close is swallowed; the store's Header_error still surfaces"
                true
                (contains msg "Header_error");
              Lwt.return_unit
          in
          (* [raising_close] never actually released the underlying fd, by
             design; do it here so this test doesn't leak it. *)
          Lwt.catch (fun () -> real_close ()) (fun _ -> Lwt.return_unit))
       (fun () ->
          safe_unlink path;
          Lwt.return_unit))
;;

(* ------------------------------------------------------------------ *)
(* 5. QCheck: an Error-returning open never mutates on-disk bytes       *)
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
    ; ( "close-on-error (#763)"
      , [ Alcotest.test_case
            "close invoked once on Error"
            `Quick
            test_close_invoked_once_on_error
        ; Alcotest.test_case
            "close not invoked early on Ok"
            `Quick
            test_close_not_invoked_early_on_ok
        ; Alcotest.test_case
            "a raising close does not clobber the Header_error"
            `Quick
            test_close_raising_does_not_clobber_error
        ] )
    ; "properties", qcheck_tests
    ]
;;
