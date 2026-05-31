(** Unit tests for the replication apply primitive. *)

open Lwt.Syntax
module Wal = Sqlocaml_storage.Wal
module Replication = Sqlocaml_replication.Replication

(* ------------------------------------------------------------------ *)
(* In-memory byte-addressable device (same pattern as test_wal.ml)      *)
(* ------------------------------------------------------------------ *)

type dev = { mutable buf : Bytes.t }

let mk_dev size = { buf = Bytes.make size '\x00' }
let dev_size d = Int64.of_int (Bytes.length d.buf)

let dev_grow d need =
  let cur = Bytes.length d.buf in
  if need > cur
  then (
    let new_size = max need (cur * 2) in
    let nb = Bytes.make new_size '\x00' in
    Bytes.blit d.buf 0 nb 0 cur;
    d.buf <- nb)
;;

let read_at d ~offset out =
  let off = Int64.to_int offset in
  let len = Cstruct.length out in
  let cur = Bytes.length d.buf in
  if off + len > cur
  then Lwt.return (Error "read past EOF")
  else (
    Cstruct.blit_from_bytes d.buf off out 0 len;
    Lwt.return (Ok ()))
;;

let write_at d ~offset src =
  let off = Int64.to_int offset in
  let len = Cstruct.length src in
  dev_grow d (off + len);
  let tmp = Bytes.create len in
  Cstruct.blit_to_bytes src 0 tmp 0 len;
  Bytes.blit tmp 0 d.buf off len;
  Lwt.return (Ok ())
;;

let sync_ok () = Lwt.return (Ok ())

let fresh_wal ?(size = 65536) () =
  let d = mk_dev size in
  let* r =
    Wal.open_
      ~read_at:(read_at d)
      ~write_at:(write_at d)
      ~sync:sync_ok
      ~size_bytes:(dev_size d)
      ()
  in
  match r with
  | Ok w -> Lwt.return (d, w)
  | Error e -> Alcotest.failf "fresh_wal: %a" Wal.pp_error e
;;

let page_with c =
  let p = Cstruct.create 4096 in
  Cstruct.memset p (Char.code c);
  p
;;

(* ------------------------------------------------------------------ *)
(* Helper: create a minimal pager for apply_frames                     *)
(* ------------------------------------------------------------------ *)

let minimal_pager ?(n_pages = 10L) () =
  Sqlocaml_storage.Pager.create
    ~read_page:(fun ~page_id:_ _ -> Lwt.return (Error "not needed"))
    ~write_page:(fun ~page_id:_ _ -> Lwt.return (Error "not needed"))
    ~sync:sync_ok
    ~resize:(fun ~n_pages:_ -> Lwt.return (Ok ()))
    ~n_pages
    ~freelist:Sqlocaml_storage.Freelist.empty
;;

(* ------------------------------------------------------------------ *)
(* apply_frames tests                                                   *)
(* ------------------------------------------------------------------ *)

(** Write frames via apply_frames and verify they appear in the WAL. *)
let test_apply_single_batch () =
  Lwt_main.run
    (let* _, wal = fresh_wal () in
     let pager = minimal_pager () in
     let frames =
       [ Replication.
           { epoch = 0L
           ; frame_idx = 0
           ; page_id = 1L
           ; is_commit = false
           ; page = page_with 'A'
           }
       ; Replication.
           { epoch = 0L
           ; frame_idx = 1
           ; page_id = 2L
           ; is_commit = true
           ; page = page_with 'B'
           }
       ]
     in
     let* r = Replication.apply_frames ~wal ~pager frames in
     (match r with
      | Ok () ->
        Alcotest.(check int) "2 committed frames" 2 (Wal.committed_frames wal);
        Alcotest.(check (option int)) "page 1 present" (Some 0) (Wal.find_page wal 1L);
        Alcotest.(check (option int)) "page 2 present" (Some 1) (Wal.find_page wal 2L)
      | Error (`Apply_error msg) -> Alcotest.failf "apply_frames: %s" msg);
     Lwt.return_unit)
;;

(** Multiple commit batches in one apply_frames call. *)
let test_apply_multiple_batches () =
  Lwt_main.run
    (let* _, wal = fresh_wal () in
     let pager = minimal_pager () in
     let frames =
       [ Replication.
           { epoch = 0L
           ; frame_idx = 0
           ; page_id = 1L
           ; is_commit = false
           ; page = page_with 'A'
           }
       ; Replication.
           { epoch = 0L
           ; frame_idx = 1
           ; page_id = 2L
           ; is_commit = true
           ; page = page_with 'B'
           }
       ; Replication.
           { epoch = 0L
           ; frame_idx = 2
           ; page_id = 3L
           ; is_commit = true
           ; page = page_with 'C'
           }
       ]
     in
     let* r = Replication.apply_frames ~wal ~pager frames in
     (match r with
      | Ok () ->
        Alcotest.(check int) "3 committed frames" 3 (Wal.committed_frames wal);
        Alcotest.(check (option int)) "page 2 at idx 1" (Some 1) (Wal.find_page wal 2L);
        Alcotest.(check (option int)) "page 3 at idx 2" (Some 2) (Wal.find_page wal 3L)
      | Error (`Apply_error msg) -> Alcotest.failf "apply_frames: %s" msg);
     Lwt.return_unit)
;;

(** Trailing non-commit frames are silently ignored. *)
let test_apply_trailing_non_commit_ignored () =
  Lwt_main.run
    (let* _, wal = fresh_wal () in
     let pager = minimal_pager () in
     let frames =
       [ Replication.
           { epoch = 0L
           ; frame_idx = 0
           ; page_id = 1L
           ; is_commit = true
           ; page = page_with 'A'
           }
       ; Replication.
           { epoch = 0L
           ; frame_idx = 1
           ; page_id = 2L
           ; is_commit = false
           ; page = page_with 'B'
           }
       ; Replication.
           { epoch = 0L
           ; frame_idx = 2
           ; page_id = 3L
           ; is_commit = false
           ; page = page_with 'C'
           }
       ]
     in
     let* r = Replication.apply_frames ~wal ~pager frames in
     (match r with
      | Ok () ->
        Alcotest.(check int) "1 committed frame" 1 (Wal.committed_frames wal);
        Alcotest.(check (option int)) "page 1 present" (Some 0) (Wal.find_page wal 1L);
        Alcotest.(check (option int)) "page 2 absent" None (Wal.find_page wal 2L)
      | Error (`Apply_error msg) -> Alcotest.failf "apply_frames: %s" msg);
     Lwt.return_unit)
;;

(** apply_frames should grow the device if page_id exceeds current capacity. *)
let test_apply_grows_device () =
  Lwt_main.run
    (let* _, wal = fresh_wal () in
     let pager = minimal_pager ~n_pages:2L () in
     let frames =
       [ Replication.
           { epoch = 0L
           ; frame_idx = 0
           ; page_id = 50L
           ; is_commit = true
           ; page = page_with 'Z'
           }
       ]
     in
     let* r = Replication.apply_frames ~wal ~pager frames in
     (match r with
      | Ok () ->
        Alcotest.(check int) "1 committed frame" 1 (Wal.committed_frames wal);
        let new_pages = Sqlocaml_storage.Pager.n_pages pager in
        Alcotest.(check bool) "device grew" true (new_pages >= 51L)
      | Error (`Apply_error msg) -> Alcotest.failf "apply_frames: %s" msg);
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "replication"
    [ ( "apply_frames"
      , [ Alcotest.test_case "single batch" `Quick test_apply_single_batch
        ; Alcotest.test_case "multiple batches" `Quick test_apply_multiple_batches
        ; Alcotest.test_case
            "trailing non-commit ignored"
            `Quick
            test_apply_trailing_non_commit_ignored
        ; Alcotest.test_case "grows device" `Quick test_apply_grows_device
        ] )
    ]
;;
