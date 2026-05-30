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
  Lwt.finalize
    (fun () ->
       let* src_r = UnixStore.open_file ~path:src_path () in
       let src = ok_store src_r in
       let n_src = S.n_pages src in
       let pages : (int64, Cstruct.t) Hashtbl.t = Hashtbl.create 16 in
       let count = ref 0 in
       let sink : S.page_sink =
        fun ~page_id ~page ->
         Hashtbl.replace pages page_id page;
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
  let* () = S.copy_to t sink in
  Alcotest.(check int) "mem backend no pages" 0 !count;
  Lwt.return_unit
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
    ]
;;

