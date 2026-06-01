(** Tests for the overflow-page chain support added in phase 37 (#43).

    Verifies large blob roundtrip via chains of 1, N, and many pages;
    replace-frees-old-chain; del-frees-chain; freelist reuse; WAL
    transparency; SAVEPOINT rollback discards a partial chain. *)

open Lwt.Syntax

module S = struct
  include Sqlocaml_store.Store

  let open_file = Sqlocaml_unix.Store.open_file
  let open_file_wal = Sqlocaml_unix.Store.open_file_wal
end

module Page = Sqlocaml_storage.Page

let bs s = Bytes.of_string s
let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_overflow_%04d.db" n
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

let ok_store = function
  | Ok t -> t
  | Error e -> Alcotest.failf "open: %a" S.pp_error e
;;

let bytes_opt =
  Alcotest.(
    option
      (testable
         (fun ppf b ->
            Format.fprintf ppf "<%d bytes h=%d>" (Bytes.length b) (Hashtbl.hash b))
         Bytes.equal))
;;

(* ------------------------------------------------------------------ *)

(* 1. A value just over the inline threshold (801 bytes) spills to a
      single-page overflow chain and round-trips intact. *)
let test_small_overflow () =
  run
    (let path = fresh_path () in
     cleanup path;
     Lwt.finalize
       (fun () ->
          let* sr = S.open_file ~path () in
          let st = ok_store sr in
          let v = Bytes.make 801 'X' in
          let* tx = S.rw_begin st in
          let* () = S.put tx 16 (bs "key1") v in
          let* () = S.commit tx in
          let* () = S.close st in
          let* sr2 = S.open_file ~path () in
          let st2 = ok_store sr2 in
          let* tx2 = S.ro_begin st2 in
          let* g = S.get tx2 16 (bs "key1") in
          let* () = S.ro_end tx2 in
          let* () = S.close st2 in
          Alcotest.(check bytes_opt) "801-byte value" (Some v) g;
          Lwt.return_unit)
       (fun () ->
          cleanup path;
          Lwt.return_unit))
;;

(* 2. A 12 KiB value (3 full overflow pages) round-trips. *)
let test_multi_page_overflow () =
  run
    (let path = fresh_path () in
     cleanup path;
     Lwt.finalize
       (fun () ->
          let* sr = S.open_file ~path () in
          let st = ok_store sr in
          let v = Bytes.init 12000 (fun i -> Char.chr (i land 0xff)) in
          let* tx = S.rw_begin st in
          let* () = S.put tx 16 (bs "blob") v in
          let* () = S.commit tx in
          let* tx2 = S.ro_begin st in
          let* g = S.get tx2 16 (bs "blob") in
          let* () = S.ro_end tx2 in
          let* () = S.close st in
          Alcotest.(check bytes_opt) "12000-byte value round-trip" (Some v) g;
          Lwt.return_unit)
       (fun () ->
          cleanup path;
          Lwt.return_unit))
;;

(* 3. A 200 KiB value (50+ pages) round-trips. *)
let test_huge_overflow () =
  run
    (let path = fresh_path () in
     cleanup path;
     Lwt.finalize
       (fun () ->
          let* sr = S.open_file ~path () in
          let st = ok_store sr in
          let v = Bytes.init 200_000 (fun i -> Char.chr (i * 7 land 0xff)) in
          let* tx = S.rw_begin st in
          let* () = S.put tx 16 (bs "huge") v in
          let* () = S.commit tx in
          let* tx2 = S.ro_begin st in
          let* g = S.get tx2 16 (bs "huge") in
          let* () = S.ro_end tx2 in
          let* () = S.close st in
          Alcotest.(check bytes_opt) "200KB blob round-trips" (Some v) g;
          Lwt.return_unit)
       (fun () ->
          cleanup path;
          Lwt.return_unit))
;;

(* 4. An inline-sized value does NOT allocate any overflow page.  Compare
      page allocations for a single inline put vs. a single 5 KB put. *)
let test_inline_does_not_overflow () =
  run
    (let p1 = fresh_path () in
     let p2 = fresh_path () in
     cleanup p1;
     cleanup p2;
     Lwt.finalize
       (fun () ->
          let single_put_pages ~value ~path =
            let* sr = S.open_file ~path () in
            let st = ok_store sr in
            let* tx = S.rw_begin st in
            let* () = S.put tx 16 (bs "k") value in
            let* () = S.commit tx in
            let n = S.n_pages st in
            let* () = S.close st in
            Lwt.return n
          in
          let* inline_pages = single_put_pages ~value:(Bytes.make 100 'a') ~path:p1 in
          let* overflow_pages = single_put_pages ~value:(Bytes.make 5000 'b') ~path:p2 in
          (* Inline path = 2 header pages + 1 leaf = 3 pages.
         Overflow path = header pages + 1 leaf + ≥2 overflow pages. *)
          Alcotest.(check bool)
            (Printf.sprintf
               "inline put uses fewer pages than overflow (inline=%Ld, ovf=%Ld)"
               inline_pages
               overflow_pages)
            true
            (Int64.compare inline_pages overflow_pages < 0);
          Alcotest.(check bool)
            (Printf.sprintf "single inline put fits in ≤6 pages (got %Ld)" inline_pages)
            true
            (Int64.compare inline_pages 7L < 0);
          Lwt.return_unit)
       (fun () ->
          cleanup p1;
          cleanup p2;
          Lwt.return_unit))
;;

(* 5. Overwriting an overflow key frees the previous chain — the next
      transaction can reuse those pages.  Validated by file-size
      stability after a sequence of replacements. *)
let test_replace_frees_chain () =
  run
    (let path = fresh_path () in
     cleanup path;
     Lwt.finalize
       (fun () ->
          let* sr = S.open_file ~path () in
          let st = ok_store sr in
          let mk i = Bytes.init 5000 (fun j -> Char.chr ((i + j) land 0xff)) in
          let* tx = S.rw_begin st in
          let* () = S.put tx 16 (bs "k") (mk 1) in
          let* () = S.commit tx in
          let* tx_ro = S.ro_begin st in
          let* () = S.ro_end tx_ro in
          let size_after_first = Unix.((stat path).st_size) in
          let* tx = S.rw_begin st in
          let* () = S.put tx 16 (bs "k") (mk 2) in
          let* () = S.commit tx in
          let* tx_ro2 = S.ro_begin st in
          let* () = S.ro_end tx_ro2 in
          let* tx = S.rw_begin st in
          let* () = S.put tx 16 (bs "k") (mk 3) in
          let* () = S.commit tx in
          let size_after_third = Unix.((stat path).st_size) in
          let* tx_ro3 = S.ro_begin st in
          let* g = S.get tx_ro3 16 (bs "k") in
          let* () = S.ro_end tx_ro3 in
          Alcotest.(check bytes_opt) "latest value present" (Some (mk 3)) g;
          (* File should not have grown by more than ~3x — chains get freed
         and re-used.  Without chain freeing the file would grow linearly
         with every replace. *)
          Alcotest.(check bool)
            "file size grew bounded"
            true
            (size_after_third <= size_after_first * 4);
          let* () = S.close st in
          Lwt.return_unit)
       (fun () ->
          cleanup path;
          Lwt.return_unit))
;;

(* 6. del frees the chain — key is gone, file does not blow up after
      many delete/re-insert cycles. *)
let test_del_frees_chain () =
  run
    (let path = fresh_path () in
     cleanup path;
     Lwt.finalize
       (fun () ->
          let* sr = S.open_file ~path () in
          let st = ok_store sr in
          let v = Bytes.make 7000 'Z' in
          let initial_size = ref 0 in
          let* () =
            let* tx = S.rw_begin st in
            let* () = S.put tx 16 (bs "key") v in
            let* () = S.commit tx in
            (initial_size := Unix.((stat path).st_size));
            Lwt.return_unit
          in
          let* () =
            Lwt_list.iter_s
              (fun _ ->
                 let* tx = S.rw_begin st in
                 let* () = S.del tx 16 (bs "key") in
                 let* () = S.commit tx in
                 let* tx = S.rw_begin st in
                 let* () = S.put tx 16 (bs "key") v in
                 let* () = S.commit tx in
                 Lwt.return_unit)
              (List.init 5 Fun.id)
          in
          let final_size = Unix.((stat path).st_size) in
          let* tx = S.ro_begin st in
          let* g = S.get tx 16 (bs "key") in
          let* () = S.ro_end tx in
          Alcotest.(check bytes_opt) "final value present" (Some v) g;
          Alcotest.(check bool)
            "file did not balloon over 5 del/put cycles"
            true
            (final_size <= !initial_size * 3);
          let* () = S.close st in
          Lwt.return_unit)
       (fun () ->
          cleanup path;
          Lwt.return_unit))
;;

(* 7. Cursor over a tree containing overflow values yields the decoded
      payloads (not the raw marker). *)
let test_cursor_decodes_overflow () =
  run
    (let path = fresh_path () in
     cleanup path;
     Lwt.finalize
       (fun () ->
          let* sr = S.open_file ~path () in
          let st = ok_store sr in
          let big = Bytes.init 10_000 (fun i -> Char.chr (i land 0xff)) in
          let small = bs "tiny" in
          let* tx = S.rw_begin st in
          let* () = S.put tx 16 (bs "a") small in
          let* () = S.put tx 16 (bs "b") big in
          let* () = S.put tx 16 (bs "c") small in
          let* () = S.commit tx in
          let* tx_ro = S.ro_begin st in
          let* cur = S.cursor_open tx_ro 16 in
          let _ = S.cursor_first cur in
          let rec drain acc =
            match S.cursor_next cur with
            | None -> List.rev acc
            | Some kv -> drain (kv :: acc)
          in
          let entries = drain [] in
          S.cursor_close cur;
          let* () = S.ro_end tx_ro in
          let* () = S.close st in
          Alcotest.(check int) "3 entries seen" 3 (List.length entries);
          let _, vb = List.nth entries 1 in
          Alcotest.(check bool)
            "cursor returns decoded 10KB value"
            true
            (Bytes.equal vb big);
          Lwt.return_unit)
       (fun () ->
          cleanup path;
          Lwt.return_unit))
;;

(* 8. Overflow values work transparently in WAL mode. *)
let test_overflow_wal_mode () =
  run
    (let path = fresh_path () in
     cleanup path;
     Lwt.finalize
       (fun () ->
          let* sr = S.open_file_wal ~path () in
          let st = ok_store sr in
          let v = Bytes.init 30_000 (fun i -> Char.chr (i * 13 land 0xff)) in
          let* tx = S.rw_begin st in
          let* () = S.put tx 16 (bs "wal-blob") v in
          let* () = S.commit tx in
          let* () = S.close st in
          let* sr2 = S.open_file_wal ~path () in
          let st2 = ok_store sr2 in
          let* tx2 = S.ro_begin st2 in
          let* g = S.get tx2 16 (bs "wal-blob") in
          let* () = S.ro_end tx2 in
          let* () = S.close st2 in
          Alcotest.(check bytes_opt) "30KB blob via WAL" (Some v) g;
          Lwt.return_unit)
       (fun () ->
          cleanup path;
          Lwt.return_unit))
;;

(* 9. SAVEPOINT rollback discards an in-flight overflow chain. *)
let test_savepoint_rollback_overflow () =
  run
    (let path = fresh_path () in
     cleanup path;
     Lwt.finalize
       (fun () ->
          let* sr = S.open_file ~path () in
          let st = ok_store sr in
          let pre = Bytes.make 6000 'A' in
          let post = Bytes.make 6000 'B' in
          let* tx = S.rw_begin st in
          let* () = S.put tx 16 (bs "k") pre in
          let* () = S.savepoint_begin tx "sp1" in
          let* () = S.put tx 16 (bs "k") post in
          let* () = S.savepoint_rollback tx "sp1" in
          let* () = S.commit tx in
          let* tx_ro = S.ro_begin st in
          let* g = S.get tx_ro 16 (bs "k") in
          let* () = S.ro_end tx_ro in
          let* () = S.close st in
          Alcotest.(check bytes_opt) "rollback restored pre-savepoint value" (Some pre) g;
          Lwt.return_unit)
       (fun () ->
          cleanup path;
          Lwt.return_unit))
;;

(* 10. The overflow page kind round-trips its payload at the codec layer. *)
let test_page_codec_overflow () =
  let buf = Cstruct.create Page.page_size in
  let payload = Bytes.init 100 (fun i -> Char.chr (i mod 13)) in
  Page.write_overflow
    buf
    ~next_pid:42l
    ~payload
    ~payload_off:0
    ~payload_len:(Bytes.length payload);
  Page.seal buf;
  Alcotest.(check bool) "verify_crc" true (Page.verify_crc buf);
  let common = Page.read_common buf in
  Alcotest.(check bool) "kind is Overflow" true (common.kind = Page.Overflow);
  Alcotest.(check int) "right_page = 42" 42 (Int32.to_int common.right_page);
  Alcotest.(check int) "payload_len" 100 (Page.overflow_payload_len buf);
  Alcotest.(check bool)
    "payload bytes match"
    true
    (Bytes.equal payload (Page.overflow_payload buf))
;;

let () =
  Alcotest.run
    "overflow"
    [ ( "page-codec"
      , [ Alcotest.test_case "overflow page codec" `Quick test_page_codec_overflow ] )
    ; ( "store"
      , [ Alcotest.test_case "small overflow (801 B)" `Quick test_small_overflow
        ; Alcotest.test_case "multi-page overflow" `Quick test_multi_page_overflow
        ; Alcotest.test_case "huge overflow (200 KB)" `Quick test_huge_overflow
        ; Alcotest.test_case "inline below threshold" `Quick test_inline_does_not_overflow
        ; Alcotest.test_case "replace frees old chain" `Quick test_replace_frees_chain
        ; Alcotest.test_case "del frees chain" `Quick test_del_frees_chain
        ; Alcotest.test_case "cursor decodes overflow" `Quick test_cursor_decodes_overflow
        ; Alcotest.test_case "WAL mode overflow" `Quick test_overflow_wal_mode
        ; Alcotest.test_case "SAVEPOINT rollback" `Quick test_savepoint_rollback_overflow
        ] )
    ]
;;
