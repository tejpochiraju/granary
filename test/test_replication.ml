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

let make_frame ~epoch ~frame_idx ~page_id ~is_commit ~page =
  let flags = if is_commit then 1L else 0L in
  let checksum = Wal.frame_checksum ~salt:1L ~seed:2L ~page_id ~flags ~page in
  Replication.
    { epoch
    ; frame_idx
    ; page_id
    ; is_commit
    ; page
    ; checksum
    ; source_salt = 1L
    ; source_seed = 2L
    }
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
       [ make_frame
           ~epoch:0L
           ~frame_idx:0
           ~page_id:1L
           ~is_commit:false
           ~page:(page_with 'A')
       ; make_frame
           ~epoch:0L
           ~frame_idx:1
           ~page_id:2L
           ~is_commit:true
           ~page:(page_with 'B')
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
       [ make_frame
           ~epoch:0L
           ~frame_idx:0
           ~page_id:1L
           ~is_commit:false
           ~page:(page_with 'A')
       ; make_frame
           ~epoch:0L
           ~frame_idx:1
           ~page_id:2L
           ~is_commit:true
           ~page:(page_with 'B')
       ; make_frame
           ~epoch:0L
           ~frame_idx:2
           ~page_id:3L
           ~is_commit:true
           ~page:(page_with 'C')
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
       [ make_frame
           ~epoch:0L
           ~frame_idx:0
           ~page_id:1L
           ~is_commit:true
           ~page:(page_with 'A')
       ; make_frame
           ~epoch:0L
           ~frame_idx:1
           ~page_id:2L
           ~is_commit:false
           ~page:(page_with 'B')
       ; make_frame
           ~epoch:0L
           ~frame_idx:2
           ~page_id:3L
           ~is_commit:false
           ~page:(page_with 'C')
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

(* ------------------------------------------------------------------ *)
(* verify_checksum unit tests                                          *)
(* ------------------------------------------------------------------ *)

(** verify_checksum returns true on a well-formed frame. *)
let test_verify_checksum_valid () =
  let frame =
    make_frame
      ~epoch:0L
      ~frame_idx:0
      ~page_id:1L
      ~is_commit:true
      ~page:(page_with 'A')
  in
  Alcotest.(check bool) "valid frame" true (Replication.verify_checksum frame)
;;

(** verify_checksum returns false on a frame with corrupted checksum. *)
let test_verify_checksum_corrupted_checksum () =
  let frame =
    make_frame
      ~epoch:0L
      ~frame_idx:0
      ~page_id:1L
      ~is_commit:true
      ~page:(page_with 'A')
  in
  let corrupted = { frame with checksum = Int64.succ frame.checksum } in
  Alcotest.(check bool) "corrupted checksum" false (Replication.verify_checksum corrupted)
;;

(** verify_checksum returns false on a frame with corrupted page bytes. *)
let test_verify_checksum_corrupted_page () =
  let frame =
    make_frame
      ~epoch:0L
      ~frame_idx:0
      ~page_id:1L
      ~is_commit:true
      ~page:(page_with 'A')
  in
  let bad = Cstruct.create (Cstruct.length frame.page) in
  Cstruct.blit frame.page 0 bad 0 (Cstruct.length frame.page);
  Cstruct.set_uint8 bad 0 (Cstruct.get_uint8 bad 0 lxor 0xff);
  let corrupted = { frame with page = bad } in
  Alcotest.(check bool) "corrupted page" false (Replication.verify_checksum corrupted)
;;

(** verify_checksum returns false on a frame with corrupted source_salt. *)
let test_verify_checksum_corrupted_salt () =
  let frame =
    make_frame
      ~epoch:0L
      ~frame_idx:0
      ~page_id:1L
      ~is_commit:true
      ~page:(page_with 'A')
  in
  let corrupted = { frame with source_salt = Int64.succ frame.source_salt } in
  Alcotest.(check bool) "corrupted salt" false (Replication.verify_checksum corrupted)
;;

(** verify_checksum returns false on a frame with corrupted source_seed. *)
let test_verify_checksum_corrupted_seed () =
  let frame =
    make_frame
      ~epoch:0L
      ~frame_idx:0
      ~page_id:1L
      ~is_commit:true
      ~page:(page_with 'A')
  in
  let corrupted = { frame with source_seed = Int64.succ frame.source_seed } in
  Alcotest.(check bool) "corrupted seed" false (Replication.verify_checksum corrupted)
;;

(* ------------------------------------------------------------------ *)
(* apply_frames: transport checksum error path                         *)
(* ------------------------------------------------------------------ *)

(** Corrupted checksum → apply_frames returns transport checksum error. *)
let test_apply_corrupted_checksum () =
  Lwt_main.run
    (let* _, wal = fresh_wal () in
     let pager = minimal_pager () in
     let frame =
       make_frame
         ~epoch:0L
         ~frame_idx:0
         ~page_id:1L
         ~is_commit:true
         ~page:(page_with 'A')
     in
     let corrupted = { frame with checksum = Int64.succ frame.checksum } in
     let* r = Replication.apply_frames ~wal ~pager [ corrupted ] in
     (match r with
      | Error (`Apply_error msg) ->
        Alcotest.(check string)
          "error message"
          "transport checksum verification failed"
          msg
      | Ok () -> Alcotest.fail "apply_frames should have rejected corrupted checksum");
     Lwt.return_unit)
;;

(** Corrupted page bytes → apply_frames returns transport checksum error. *)
let test_apply_corrupted_page () =
  Lwt_main.run
    (let* _, wal = fresh_wal () in
     let pager = minimal_pager () in
     let frame =
       make_frame
         ~epoch:0L
         ~frame_idx:0
         ~page_id:1L
         ~is_commit:true
         ~page:(page_with 'A')
     in
     let bad = Cstruct.create (Cstruct.length frame.page) in
     Cstruct.blit frame.page 0 bad 0 (Cstruct.length frame.page);
     Cstruct.set_uint8 bad 0 (Cstruct.get_uint8 bad 0 lxor 0xff);
     let corrupted = { frame with page = bad } in
     let* r = Replication.apply_frames ~wal ~pager [ corrupted ] in
     (match r with
      | Error (`Apply_error msg) ->
        Alcotest.(check string)
          "error message"
          "transport checksum verification failed"
          msg
      | Ok () -> Alcotest.fail "apply_frames should have rejected corrupted page");
     Lwt.return_unit)
;;

(** Corrupted source_salt → apply_frames returns transport checksum error. *)
let test_apply_corrupted_salt () =
  Lwt_main.run
    (let* _, wal = fresh_wal () in
     let pager = minimal_pager () in
     let frame =
       make_frame
         ~epoch:0L
         ~frame_idx:0
         ~page_id:1L
         ~is_commit:true
         ~page:(page_with 'A')
     in
     let corrupted = { frame with source_salt = Int64.succ frame.source_salt } in
     let* r = Replication.apply_frames ~wal ~pager [ corrupted ] in
     (match r with
      | Error (`Apply_error msg) ->
        Alcotest.(check string)
          "error message"
          "transport checksum verification failed"
          msg
      | Ok () -> Alcotest.fail "apply_frames should have rejected corrupted salt");
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* QCheck property: verify_checksum round-trip                          *)
(* ------------------------------------------------------------------ *)

let qcheck_verify_checksum =
  let open QCheck in
  Test.make
    ~name:"verify_checksum: valid frame passes, any corruption fails"
    ~count:1_000
    (tup5
       (map Int64.of_int (int_range 0 0xFFFF))
       (map Int64.of_int (int_range 0 0xFFFF))
       (map Int64.of_int (int_range 0 0xFFFF))
       bool
       (map Cstruct.of_string (string_size Gen.(1 -- 256))))
    (fun (salt, seed, page_id, is_commit, page) ->
       let flags = if is_commit then 1L else 0L in
       let checksum = Wal.frame_checksum ~salt ~seed ~page_id ~flags ~page in
       let frame =
         Replication.
           { epoch = 0L
           ; frame_idx = 0
           ; page_id
           ; is_commit
           ; page
           ; checksum
           ; source_salt = salt
           ; source_seed = seed
           }
       in
       (* 1. Well-formed frame passes *)
       let valid = Replication.verify_checksum frame in
       (* 2. Corrupted checksum fails *)
       let bad_checksum =
         not
           (Replication.verify_checksum { frame with checksum = Int64.succ checksum })
       in
       (* 3. Corrupted page byte fails *)
       let bad_page =
         let bad = Cstruct.create (Cstruct.length page) in
         Cstruct.blit page 0 bad 0 (Cstruct.length page);
         Cstruct.set_uint8 bad 0 (Cstruct.get_uint8 bad 0 lxor 0xff);
         not (Replication.verify_checksum { frame with page = bad })
       in
       (* 4. Corrupted salt fails *)
       let bad_salt =
         not
           (Replication.verify_checksum { frame with source_salt = Int64.succ salt })
       in
       valid && bad_checksum && bad_page && bad_salt)
;;

(** apply_frames should grow the device if page_id exceeds current capacity. *)
let test_apply_grows_device () =
  Lwt_main.run
    (let* _, wal = fresh_wal () in
     let pager = minimal_pager ~n_pages:2L () in
     let frames =
       [ make_frame
           ~epoch:0L
           ~frame_idx:0
           ~page_id:50L
           ~is_commit:true
           ~page:(page_with 'Z')
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
        ; Alcotest.test_case
            "corrupted checksum -> error"
            `Quick
            test_apply_corrupted_checksum
        ; Alcotest.test_case
            "corrupted page -> error"
            `Quick
            test_apply_corrupted_page
        ; Alcotest.test_case
            "corrupted salt -> error"
            `Quick
            test_apply_corrupted_salt
        ] )
    ; ( "verify_checksum"
      , [ Alcotest.test_case "valid frame" `Quick test_verify_checksum_valid
        ; Alcotest.test_case
            "corrupted checksum"
            `Quick
            test_verify_checksum_corrupted_checksum
        ; Alcotest.test_case
            "corrupted page"
            `Quick
            test_verify_checksum_corrupted_page
        ; Alcotest.test_case
            "corrupted salt"
            `Quick
            test_verify_checksum_corrupted_salt
        ; Alcotest.test_case
            "corrupted seed"
            `Quick
            test_verify_checksum_corrupted_seed
        ] )
    ; ( "qcheck"
      , List.map QCheck_alcotest.to_alcotest [ qcheck_verify_checksum ] )
    ]
;;
