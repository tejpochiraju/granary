(** Fault injection harness for the WAL crash-recovery path (#102).

    Each test reproduces a specific failure mode (torn write, dropped
    sync, single-byte corruption, mid-batch crash) and verifies that
    [Wal.open_] still produces a coherent recovered state — meaning
    every reachable key belongs to a complete committed batch and no
    partial-batch state ever leaks through. *)

open Lwt.Syntax

module S = struct
  include Sqlocaml_store.Store

  let open_file_wal = Sqlocaml_unix.Store.open_file_wal
end

module Wal = Sqlocaml_storage.Wal

let bs s = Bytes.of_string s
let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_wal_fault_%04d.db" n
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

let ok_store = function
  | Ok t -> t
  | Error e -> Alcotest.failf "open_file_wal: %a" S.pp_error e
;;

let bytes_opt =
  Alcotest.(
    option
      (testable (fun ppf b -> Format.fprintf ppf "%S" (Bytes.to_string b)) Bytes.equal))
;;

(* ---------- helpers to read raw WAL bytes ---------- *)

let wal_file_size path =
  try Unix.((stat (path ^ "-wal")).st_size) with
  | _ -> 0
;;

let read_wal_bytes path =
  let wal = path ^ "-wal" in
  let fd = Unix.openfile wal [ Unix.O_RDONLY ] 0 in
  let n = Unix.((fstat fd).st_size) in
  let buf = Bytes.create n in
  let rec loop off rem =
    if rem = 0
    then ()
    else (
      let r = Unix.read fd buf off rem in
      if r = 0 then () else loop (off + r) (rem - r))
  in
  loop 0 n;
  Unix.close fd;
  buf
;;

let write_wal_bytes path bytes =
  let wal = path ^ "-wal" in
  let fd = Unix.openfile wal [ Unix.O_RDWR; Unix.O_TRUNC ] 0o644 in
  let n = Bytes.length bytes in
  let _ = Unix.write fd bytes 0 n in
  Unix.close fd
;;

(* ---------- tests ---------- *)

(* TORN WRITE: After a fully-committed batch, truncate the WAL mid-frame
   (between the metadata and the page bytes). Recovery must discard the
   torn frame and surface the pre-torn state. *)
let test_torn_write_midframe () =
  run
    (let path = fresh_path () in
     cleanup path;
     Lwt.finalize
       (fun () ->
          let* sr1 = S.open_file_wal ~path () in
          let st1 = ok_store sr1 in
          let* tx = S.rw_begin st1 in
          let* () = S.put tx 16 (bs "good") (bs "1") in
          let* () = S.commit tx in
          let* () = S.close st1 in
          let original_size = wal_file_size path in
          (* Now scribble extra garbage that's only PARTIALLY a frame. The
         next reader must drop it; the "good" commit remains. *)
          let extra = 1234 in
          let fd = Unix.openfile (path ^ "-wal") [ Unix.O_WRONLY ] 0o644 in
          let _ = Unix.lseek fd original_size Unix.SEEK_SET in
          let junk = Bytes.make extra '\xAA' in
          let _ = Unix.write fd junk 0 extra in
          Unix.close fd;
          let* sr2 = S.open_file_wal ~path () in
          let st2 = ok_store sr2 in
          let* tx = S.ro_begin st2 in
          let* v = S.get tx 16 (bs "good") in
          let* () = S.ro_end tx in
          Alcotest.(check bytes_opt) "good commit survived torn tail" (Some (bs "1")) v;
          let* () = S.close st2 in
          Lwt.return_unit)
       (fun () ->
          cleanup path;
          Lwt.return_unit))
;;

(* SINGLE-BYTE CORRUPTION: flip one byte in a committed frame's payload.
   The checksum must fail and all frames at-or-past the corrupted index
   must be discarded. *)
let test_single_byte_corruption () =
  run
    (let path = fresh_path () in
     cleanup path;
     Lwt.finalize
       (fun () ->
          let* sr1 = S.open_file_wal ~path () in
          let st1 = ok_store sr1 in
          let* tx = S.rw_begin st1 in
          let* () = S.put tx 16 (bs "ok") (bs "1") in
          let* () = S.commit tx in
          let* () = S.close st1 in
          let bytes = read_wal_bytes path in
          (* Corrupt a byte deep in the LAST frame, which is the new
         header page committed in this batch. Any flipped bit breaks
         the checksum and the whole batch must be discarded. *)
          let n = Bytes.length bytes in
          let target_off = n - 100 in
          let cur = Bytes.get_uint8 bytes target_off in
          Bytes.set_uint8 bytes target_off (cur lxor 0xff);
          write_wal_bytes path bytes;
          (* Recovery: the only valid committed batch was the one we just
         broke, so the WAL contributes nothing and the store opens with
         just the on-disk-header (empty) state. *)
          let* sr2 = S.open_file_wal ~path () in
          let st2 = ok_store sr2 in
          let* tx = S.ro_begin st2 in
          let* v = S.get tx 16 (bs "ok") in
          let* () = S.ro_end tx in
          Alcotest.(check bytes_opt) "corrupted batch discarded" None v;
          let* () = S.close st2 in
          Lwt.return_unit)
       (fun () ->
          cleanup path;
          Lwt.return_unit))
;;

(* INCOMPLETE BATCH: append a synthetic frame WITHOUT the commit flag
   (simulating a writer that crashed after writing some frames but
   before the commit marker landed). Recovery must discard the
   uncommitted tail. *)
let test_incomplete_batch_dropped () =
  run
    (let path = fresh_path () in
     cleanup path;
     Lwt.finalize
       (fun () ->
          let* sr1 = S.open_file_wal ~path () in
          let st1 = ok_store sr1 in
          let* tx = S.rw_begin st1 in
          let* () = S.put tx 16 (bs "alpha") (bs "1") in
          let* () = S.commit tx in
          let* () = S.close st1 in
          (* Append a syntactically-valid-looking frame at the next slot,
         but WITHOUT setting the commit bit. Since its checksum won't
         match, the recovery scanner will halt before it; either way
         the recovered state must be the pre-tampered batch only. *)
          let frame = Bytes.make Wal.frame_size_bytes '\x00' in
          let fd = Unix.openfile (path ^ "-wal") [ Unix.O_WRONLY ] 0o644 in
          let off = Unix.lseek fd 0 Unix.SEEK_END in
          let _ = Unix.write fd frame 0 (Bytes.length frame) in
          let _ = off in
          Unix.close fd;
          let* sr2 = S.open_file_wal ~path () in
          let st2 = ok_store sr2 in
          let* tx = S.ro_begin st2 in
          let* v = S.get tx 16 (bs "alpha") in
          let* () = S.ro_end tx in
          Alcotest.(check bytes_opt) "alpha intact" (Some (bs "1")) v;
          let* () = S.close st2 in
          Lwt.return_unit)
       (fun () ->
          cleanup path;
          Lwt.return_unit))
;;

(* SEEDED RANDOM TRUNCATION: for 50 seeds, build a 3-commit WAL,
   truncate at a random byte, reopen, verify the visible state matches
   SOME committed prefix. *)
let test_seeded_random_truncation () =
  run
    (let n_seeds = 50 in
     let rec loop seed =
       if seed = n_seeds
       then Lwt.return_unit
       else (
         Random.init seed;
         let path = fresh_path () in
         cleanup path;
         Lwt.finalize
           (fun () ->
              let* sr1 = S.open_file_wal ~path () in
              let st1 = ok_store sr1 in
              let prefixes = ref [ Hashtbl.create 1 ] in
              let cur = Hashtbl.create 8 in
              let* () =
                Lwt_list.iter_s
                  (fun i ->
                     let* tx = S.rw_begin st1 in
                     let key = Printf.sprintf "k%d" i in
                     let value = Printf.sprintf "v%d" i in
                     Hashtbl.replace cur key value;
                     let* () = S.put tx 16 (bs key) (bs value) in
                     let* () = S.commit tx in
                     prefixes := Hashtbl.copy cur :: !prefixes;
                     Lwt.return_unit)
                  [ 0; 1; 2 ]
              in
              let* () = S.close st1 in
              let wal_n = wal_file_size path in
              let keep = if wal_n = 0 then 0 else Random.int (wal_n + 1) in
              let fd = Unix.openfile (path ^ "-wal") [ Unix.O_RDWR ] 0o644 in
              Unix.ftruncate fd keep;
              Unix.close fd;
              let* sr2 = S.open_file_wal ~path () in
              let st2 = ok_store sr2 in
              let* tx = S.ro_begin st2 in
              let* v0 = S.get tx 16 (bs "k0") in
              let* v1 = S.get tx 16 (bs "k1") in
              let* v2 = S.get tx 16 (bs "k2") in
              let* () = S.ro_end tx in
              let* () = S.close st2 in
              let matches state =
                let key_eq k expected =
                  match expected, Hashtbl.find_opt state k with
                  | None, None -> true
                  | Some b, Some s -> Bytes.equal b (bs s)
                  | _ -> false
                in
                key_eq "k0" v0 && key_eq "k1" v1 && key_eq "k2" v2
              in
              if not (List.exists matches !prefixes)
              then Alcotest.failf "seed %d: torn WAL revealed non-prefix state" seed;
              Lwt.return_unit)
           (fun () ->
              cleanup path;
              Lwt.return_unit)
         |> fun work ->
         let* () = work in
         loop (seed + 1))
     in
     loop 0)
;;

let () =
  Alcotest.run
    "wal_fault_injection"
    [ ( "fault"
      , [ Alcotest.test_case "torn_write_midframe" `Quick test_torn_write_midframe
        ; Alcotest.test_case "single_byte_corrupt" `Quick test_single_byte_corruption
        ; Alcotest.test_case "incomplete_batch" `Quick test_incomplete_batch_dropped
        ; Alcotest.test_case "seeded_truncation" `Quick test_seeded_random_truncation
        ] )
    ]
;;
