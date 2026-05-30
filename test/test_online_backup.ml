(** Tests for #93 — online backup / hot copy. *)

open Lwt.Syntax

module S = struct
  include Sqlocaml_store.Store
end

module UnixStore = Sqlocaml_unix.Store

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let bs s = Bytes.of_string s
let run = Lwt_main.run
let counter = ref 0

let fresh_path tag =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_backup_%04d_%s.db" n tag
;;

let cleanup path =
  try Unix.unlink path with
  | _ -> ()
;;

let bytes_eq =
  Alcotest.testable
    (fun ppf b -> Format.fprintf ppf "%S" (Bytes.to_string b))
    Bytes.equal
;;

let bytes_opt_eq = Alcotest.(option bytes_eq)

let ok_store : (S.t, S.error) result -> S.t = function
  | Ok t -> t
  | Error e -> Alcotest.failf "open error: %a" S.pp_error e
;;


(* ------------------------------------------------------------------ *)
(* Basic copy: non-WAL source                                           *)
(* ------------------------------------------------------------------ *)

let test_copy_non_wal () =
  let src_path = fresh_path "src" in
  let dst_path = fresh_path "dst" in
  cleanup src_path;
  cleanup dst_path;
  run @@
  Lwt.finalize
    (fun () ->
       let* src_r = UnixStore.open_file ~path:src_path () in
       let src = ok_store src_r in
       let* () =
         let* tx = S.rw_begin src in
         let* () = S.put tx 16 (bs "key1") (bs "value1") in
         let* () = S.put tx 16 (bs "key2") (bs "value2") in
         S.commit tx
       in
       let n_src = S.n_pages src in
       let has_pages = Int64.compare n_src 0L > 0 in
       Alcotest.(check bool) "source has pages" true has_pages;
       let* cr = UnixStore.copy_to_file src ~dest:dst_path in
       (match cr with
        | Error e -> Alcotest.failf "copy_to_file error: %a" S.pp_error e
        | Ok () ->
          let* dst_r = UnixStore.open_file ~path:dst_path () in
          let dst = ok_store dst_r in
          let* () =
            S.with_ro dst (fun tx ->
              let* v1 = S.get tx 16 (bs "key1") in
              Alcotest.check bytes_opt_eq "key1" (Some (bs "value1")) v1;
              let* v2 = S.get tx 16 (bs "key2") in
              Alcotest.check bytes_opt_eq "key2" (Some (bs "value2")) v2;
              Lwt.return_unit)
          in
          let* () = S.close dst in
          let* () = S.close src in
          Lwt.return_unit))
    (fun () ->
       cleanup src_path;
       cleanup dst_path;
       Lwt.return_unit)
;;


(* ------------------------------------------------------------------ *)
(* WAL-mode copy: un-checkpointed data included                         *)
(* ------------------------------------------------------------------ *)

let test_copy_wal () =
  let src_path = fresh_path "wal_src" in
  let dst_path = fresh_path "wal_dst" in
  let wal_path = src_path ^ "-wal" in
  cleanup src_path;
  cleanup wal_path;
  cleanup dst_path;
  run @@
  Lwt.finalize
    (fun () ->
       let* src_r = UnixStore.open_file_wal ~path:src_path () in
       let src = ok_store src_r in
       let* () =
         let* tx = S.rw_begin src in
         let* () = S.put tx 16 (bs "wal_key") (bs "wal_value") in
         let* () = S.put tx 16 (bs "k2") (bs "v2") in
         let* () = S.put tx 16 (bs "k3") (bs "v3") in
         S.commit tx
       in
       Alcotest.(check bool) "wal_mode" true (S.wal_mode src);
       let* cr = UnixStore.copy_to_file src ~dest:dst_path in
       (match cr with
        | Error e -> Alcotest.failf "copy_to_file (WAL): %a" S.pp_error e
        | Ok () ->
          let* dst_r = UnixStore.open_file ~path:dst_path () in
          let dst = ok_store dst_r in
          Alcotest.(check bool) "dst not wal" false (S.wal_mode dst);
          let* () =
            S.with_ro dst (fun tx ->
              let* v = S.get tx 16 (bs "wal_key") in
              Alcotest.check bytes_opt_eq "wal_key" (Some (bs "wal_value")) v;
              Lwt.return_unit)
          in
          let* () = S.close dst in
          let* () = S.close src in
          Lwt.return_unit))
    (fun () ->
       cleanup src_path;
       cleanup wal_path;
       cleanup dst_path;
       Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Copy empty DB                                                        *)
(* ------------------------------------------------------------------ *)

let test_copy_empty () =
  let src_path = fresh_path "empty_src" in
  let dst_path = fresh_path "empty_dst" in
  cleanup src_path;
  cleanup dst_path;
  run @@
  Lwt.finalize
    (fun () ->
       let* src_r = UnixStore.open_file ~path:src_path () in
       let src = ok_store src_r in
       let* cr = UnixStore.copy_to_file src ~dest:dst_path in
       (match cr with
        | Error e -> Alcotest.failf "copy empty: %a" S.pp_error e
        | Ok () ->
          let* dst_r = UnixStore.open_file ~path:dst_path () in
          (match dst_r with
           | Error e -> Alcotest.failf "open copied empty: %a" S.pp_error e
           | Ok dst ->
             let* () = S.close dst in
             let* () = S.close src in
             Lwt.return_unit)))
    (fun () ->
       cleanup src_path;
       cleanup dst_path;
       Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Copy via page_sink directly (programmatic sink)                      *)
(* ------------------------------------------------------------------ *)

let test_copy_to_sink () =
  let src_path = fresh_path "sink_src" in
  cleanup src_path;
  run @@
  Lwt.finalize
    (fun () ->
       let* src_r = UnixStore.open_file ~path:src_path () in
       let src = ok_store src_r in
       let n_src = S.n_pages src in
       let pages : (int64, Cstruct.t) Hashtbl.t = Hashtbl.create 16 in
       let count = ref 0 in
       let sink : S.page_sink =
        fun ~page_id ~page ->
         let copy = Cstruct.create (Cstruct.length page) in
         Cstruct.blit page 0 copy 0 (Cstruct.length page);
         Hashtbl.replace pages page_id copy;
         incr count;
         Lwt.return_unit
       in
       let* () = S.copy_to src sink in
       Alcotest.(check int) "page count" (Int64.to_int n_src) !count;
       Alcotest.(check bool) "has page 0" true (Hashtbl.mem pages 0L);
       Alcotest.(check bool) "has page 1" true (Hashtbl.mem pages 1L);
       let* () = S.close src in
       Lwt.return_unit)
    (fun () ->
       cleanup src_path;
       Lwt.return_unit)
;;


(* ------------------------------------------------------------------ *)
(* In-memory backend: no-op                                             *)
(* ------------------------------------------------------------------ *)

let test_copy_mem () =
  let t = S.create () in
  let count = ref 0 in
  let sink : S.page_sink =
   fun ~page_id:_ ~page:_ ->
    incr count;
    Lwt.return_unit
  in
  run @@
  let* () = S.copy_to t sink in
  Alcotest.(check int) "mem backend no pages" 0 !count;
  Lwt.return_unit
;;


(* ------------------------------------------------------------------ *)
(* Concurrent writer: snapshot isolation after copy                    *)
(* ------------------------------------------------------------------ *)

let test_copy_concurrent_writer () =
  let src_path = fresh_path "conc_src" in
  let dst_path = fresh_path "conc_dst" in
  let wal_path = src_path ^ "-wal" in
  cleanup src_path; cleanup wal_path; cleanup dst_path;
  run @@
  Lwt.finalize
    (fun () ->
       let* src_r = UnixStore.open_file_wal ~path:src_path () in
       let src = ok_store src_r in
       let* () =
         let* tx = S.rw_begin src in
         let* () = S.put tx 16 (bs "pre_copy_key") (bs "pre_copy_val") in
         S.commit tx
       in
       let* cr = UnixStore.copy_to_file src ~dest:dst_path in
       (match cr with
        | Error e -> Alcotest.failf "copy_to_file: %a" S.pp_error e
        | Ok () -> ());
       (* Write post-copy data through the same src handle; the copy
          snapshot is already frozen, so this commit must be absent
          from the destination. *)
       let* () =
         let* tx = S.rw_begin src in
         let* () = S.put tx 16 (bs "post_copy_key") (bs "post_copy_val") in
         S.commit tx
       in
       let* dst_r = UnixStore.open_file ~path:dst_path () in
       let dst = ok_store dst_r in
       let* () =
         S.with_ro dst (fun tx ->
           let* v1 = S.get tx 16 (bs "pre_copy_key") in
           Alcotest.check bytes_opt_eq "pre_copy_key" (Some (bs "pre_copy_val")) v1;
           let* v2 = S.get tx 16 (bs "post_copy_key") in
           Alcotest.check bytes_opt_eq "post_copy_key" None v2;
           Lwt.return_unit)
       in
       let* () = S.close dst in
       let* () = S.close src in
       Lwt.return_unit)
    (fun () ->
       cleanup src_path; cleanup wal_path; cleanup dst_path;
       Lwt.return_unit)


(* ------------------------------------------------------------------ *)
(* Physical byte equality of source and dest files                     *)
(* ------------------------------------------------------------------ *)

let test_copy_physical_equality () =
  let src_path = fresh_path "phys_src" in
  let dst_path = fresh_path "phys_dst" in
  cleanup src_path; cleanup dst_path;
  run @@
  Lwt.finalize
    (fun () ->
       let* src_r = UnixStore.open_file ~path:src_path () in
       let src = ok_store src_r in
       let* () =
         let* tx = S.rw_begin src in
         let* () = S.put tx 16 (bs "k1") (bs "v1") in
         let* () = S.put tx 16 (bs "k2") (bs "v2") in
         S.commit tx
       in
       let* cr = UnixStore.copy_to_file src ~dest:dst_path in
       (match cr with Error e -> Alcotest.failf "copy: %a" S.pp_error e | Ok () -> ());
       let* () = S.close src in
       let read_file path =
         let ic = open_in_bin path in
         Fun.protect
           ~finally:(fun () -> close_in ic)
           (fun () ->
              let len = in_channel_length ic in
              let buf = Bytes.create len in
              really_input ic buf 0 len;
              buf)
       in
       let src_bytes = read_file src_path in
       let dst_bytes = read_file dst_path in
       Alcotest.check bytes_eq "physical equality" src_bytes dst_bytes;
       let src_stat = Unix.stat src_path in
       let dst_stat = Unix.stat dst_path in
       Alcotest.(check int) "dest file size" src_stat.Unix.st_size dst_stat.Unix.st_size;
       Lwt.return_unit)
    (fun () -> cleanup src_path; cleanup dst_path; Lwt.return_unit)
;;


(* ------------------------------------------------------------------ *)
(* Multi-page DB copy                                                  *)
(* ------------------------------------------------------------------ *)

let test_copy_multi_page_db () =
  let src_path = fresh_path "multi_src" in
  let dst_path = fresh_path "multi_dst" in
  cleanup src_path; cleanup dst_path;
  run @@
  Lwt.finalize
    (fun () ->
       let* src_r = UnixStore.open_file ~path:src_path () in
       let src = ok_store src_r in
       let* () =
         let* tx = S.rw_begin src in
         let* () =
           let rec loop i =
             if i >= 200 then Lwt.return_unit
             else
               let key = bs (Printf.sprintf "key%04d" i) in
               let val_ = bs (String.make 200 'X' ^ Printf.sprintf "%04d" i) in
               let* () = S.put tx 16 key val_ in
               loop (i + 1)
           in
           loop 0
         in
         S.commit tx
       in
       let n_src = S.n_pages src in
       Alcotest.(check bool) "multi-page" true (Int64.compare n_src 2L > 0);
       let* cr = UnixStore.copy_to_file src ~dest:dst_path in
       (match cr with Error e -> Alcotest.failf "copy multi: %a" S.pp_error e | Ok () -> ());
       let* () = S.close src in
       let* dst_r = UnixStore.open_file ~path:dst_path () in
       let dst = ok_store dst_r in
       let* () =
         S.with_ro dst (fun tx ->
           let* v = S.get tx 16 (bs "key0000") in
           Alcotest.check bytes_opt_eq "key0000" (Some (bs (String.make 200 'X' ^ "0000"))) v;
           let* v199 = S.get tx 16 (bs "key0199") in
           Alcotest.check bytes_opt_eq "key0199" (Some (bs (String.make 200 'X' ^ "0199"))) v199;
           Lwt.return_unit)
       in
       let* () = S.close dst in
       Lwt.return_unit)
    (fun () -> cleanup src_path; cleanup dst_path; Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Sink that fails mid-copy                                             *)
(* ------------------------------------------------------------------ *)

let test_copy_sink_error () =
  let src_path = fresh_path "err_src" in
  cleanup src_path;
  run @@
  Lwt.finalize
    (fun () ->
       let* src_r = UnixStore.open_file ~path:src_path () in
       let src = ok_store src_r in
       let* () =
         let* tx = S.rw_begin src in
         let* () = S.put tx 16 (bs "k") (bs "v") in
         S.commit tx
       in
       let count = ref 0 in
       let sink : S.page_sink =
        fun ~page_id:_ ~page:_ ->
         incr count;
         if !count >= 2 then Lwt.fail (Failure "sink error test")
         else Lwt.return_unit
       in
       let* result =
         Lwt.catch
           (fun () ->
              let* () = S.copy_to src sink in
              Lwt.return (Ok ()))
           (fun exn ->
              Lwt.return (Error (Printexc.to_string exn)))
       in
       (match result with
        | Ok () -> Alcotest.fail "expected sink error"
        | Error msg -> Alcotest.(check bool) "sink error message" true (String.length msg > 0));
       let* () = S.close src in
       Lwt.return_unit)
    (fun () -> cleanup src_path; Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Copy overwrites existing dest                                        *)
(* ------------------------------------------------------------------ *)

let test_copy_over_existing_dest () =
  let dst_path = fresh_path "over_dst" in
  cleanup dst_path;
  run @@
  Lwt.finalize
    (fun () ->
       let src_path1 = fresh_path "over_src1" in
       cleanup src_path1;
       let* src1_r = UnixStore.open_file ~path:src_path1 () in
       let src1 = ok_store src1_r in
       let* () =
         let* tx = S.rw_begin src1 in
         let* () = S.put tx 16 (bs "a") (bs "val_a") in
         let* () = S.put tx 16 (bs "b") (bs "val_b") in
         S.commit tx
       in
       let* cr1 = UnixStore.copy_to_file src1 ~dest:dst_path in
       (match cr1 with Error e -> Alcotest.failf "copy1: %a" S.pp_error e | Ok () -> ());
       let* () = S.close src1 in
       cleanup src_path1;
       let src_path2 = fresh_path "over_src2" in
       cleanup src_path2;
       let* src2_r = UnixStore.open_file ~path:src_path2 () in
       let src2 = ok_store src2_r in
       let* () =
         let* tx = S.rw_begin src2 in
         let* () = S.put tx 16 (bs "x") (bs "val_x") in
         S.commit tx
       in
       let* cr2 = UnixStore.copy_to_file src2 ~dest:dst_path in
       (match cr2 with Error e -> Alcotest.failf "copy2: %a" S.pp_error e | Ok () -> ());
       let* () = S.close src2 in
       cleanup src_path2;
       let* dst_r = UnixStore.open_file ~path:dst_path () in
       let dst = ok_store dst_r in
       let* () =
         S.with_ro dst (fun tx ->
           let* vx = S.get tx 16 (bs "x") in
           Alcotest.check bytes_opt_eq "key x" (Some (bs "val_x")) vx;
           let* va = S.get tx 16 (bs "a") in
           Alcotest.check bytes_opt_eq "key a absent" None va;
           Lwt.return_unit)
       in
       let* () = S.close dst in
       Lwt.return_unit)
    (fun () -> cleanup dst_path; Lwt.return_unit)
;;


(* ------------------------------------------------------------------ *)
(* Dest file size matches n_pages * page_size, no WAL sidecar          *)
(* ------------------------------------------------------------------ *)

let test_copy_dest_file_size () =
  let src_path = fresh_path "size_src" in
  let dst_path = fresh_path "size_dst" in
  cleanup src_path; cleanup dst_path;
  run @@
  Lwt.finalize
    (fun () ->
       let* src_r = UnixStore.open_file ~path:src_path () in
       let src = ok_store src_r in
       let* () =
         let* tx = S.rw_begin src in
         let* () = S.put tx 16 (bs "k") (bs "v") in
         S.commit tx
       in
       let n_pages = S.n_pages src in
       let page_size = 4096 in
       let* cr = UnixStore.copy_to_file src ~dest:dst_path in
       (match cr with Error e -> Alcotest.failf "copy: %a" S.pp_error e | Ok () -> ());
       let* () = S.close src in
       let dst_stat = Unix.stat dst_path in
       let expected_size = (Int64.to_int n_pages) * page_size in
       Alcotest.(check int) "dest file size" expected_size dst_stat.Unix.st_size;
       let wal_path = dst_path ^ "-wal" in
       let has_wal = try ignore (Unix.stat wal_path); true with Unix.Unix_error _ -> false in
       Alcotest.(check bool) "no wal sidecar" false has_wal;
       Lwt.return_unit)
    (fun () -> cleanup src_path; cleanup dst_path; Lwt.return_unit)
;;


(* ------------------------------------------------------------------ *)
(* Test suite                                                           *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "online_backup"
    [ ( "copy"
      , [ Alcotest.test_case "non-WAL" `Quick test_copy_non_wal
        ; Alcotest.test_case "WAL (un-checkpointed)" `Quick test_copy_wal
        ; Alcotest.test_case "empty" `Quick test_copy_empty
        ] )
    ; ( "sink"
      , [ Alcotest.test_case "page_sink" `Quick test_copy_to_sink
        ; Alcotest.test_case "mem (no-op)" `Quick test_copy_mem
        ] )
    ; ( "concurrent"
      , [ Alcotest.test_case "writer after copy" `Quick test_copy_concurrent_writer
        ] )
    ; ( "correctness"
      , [ Alcotest.test_case "physical equality" `Quick test_copy_physical_equality
        ; Alcotest.test_case "multi-page DB" `Quick test_copy_multi_page_db
        ; Alcotest.test_case "sink error" `Quick test_copy_sink_error
        ; Alcotest.test_case "overwrite existing" `Quick test_copy_over_existing_dest
        ; Alcotest.test_case "dest file size" `Quick test_copy_dest_file_size
        ] )
    ]
;;
