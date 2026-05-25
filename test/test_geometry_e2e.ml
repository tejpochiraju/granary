(** #95 — end-to-end page geometry: a store created at a non-default page_size
    (and/or reserved_bytes_per_page) must survive create → leaf/branch split →
    overflow chain → commit → REOPEN, with the geometry self-describing on
    reopen (read back from the header, no geometry passed by the caller). *)

open Lwt.Syntax
module S = Sqlocaml_store.Store
module Geometry = Sqlocaml_storage.Geometry
module Header = Sqlocaml_storage.Header
module Page = Sqlocaml_storage.Page

let run = Lwt_main.run

(* In-memory block device whose transfers follow the caller's buffer length,
   so the 4096-byte open-time geometry peek of page 0 works at any page size
   and stored pages keep their true (e.g. 16K) size. *)
type dev = { pages : (int64, bytes) Hashtbl.t }

let make_dev () = { pages = Hashtbl.create 256 }

let dev_callbacks d =
  let read_page ~page_id buf =
    (match Hashtbl.find_opt d.pages page_id with
     | Some b ->
       Cstruct.blit_from_bytes b 0 buf 0 (min (Bytes.length b) (Cstruct.length buf))
     | None -> Cstruct.memset buf 0);
    Lwt.return_ok ()
  in
  let write_page ~page_id buf =
    let n = Cstruct.length buf in
    let b = Bytes.create n in
    Cstruct.blit_to_bytes buf 0 b 0 n;
    Hashtbl.replace d.pages page_id b;
    Lwt.return_ok ()
  in
  let sync () = Lwt.return_ok () in
  let resize ~n_pages:_ = Lwt.return_ok () in
  read_page, write_page, sync, resize
;;

let geom ?(reserved = 0) ps =
  match Geometry.create ~page_size:ps ~reserved_bytes_per_page:reserved with
  | Ok g -> g
  | Error e -> Alcotest.failf "bad geometry: %a" Geometry.pp_error e
;;

(* Open the store over [d].  [geom] is honoured only when CREATING; on reopen
   it is ignored and the stored geometry is peeked from page 0. *)
let open_store ?geom d =
  let read_page, write_page, sync, resize = dev_callbacks d in
  let* r =
    S.open_block
      ?geom
      ~init_if_corrupt:true
      ~read_page
      ~write_page
      ~sync
      ~resize
      ~n_pages:0L
      ~close:(fun () -> Lwt.return_unit)
      ()
  in
  match r with
  | Ok st -> Lwt.return st
  | Error e -> Alcotest.failf "open_block: %a" S.pp_error e
;;

let user_tree = 16
let k i = Bytes.of_string (Printf.sprintf "key%06d" i)
let v i = Bytes.of_string (Printf.sprintf "val%06d-payload" i)

(* Insert [n] keys (forcing many leaf and branch splits) plus one large value
   (forcing an overflow chain), commit. *)
let populate st ~n ~big_key ~big_size =
  let* tx = S.rw_begin st in
  let rec ins i =
    if i >= n
    then Lwt.return_unit
    else
      let* () = S.put tx user_tree (k i) (v i) in
      ins (i + 1)
  in
  let* () = ins 0 in
  let* () = S.put tx user_tree big_key (Bytes.make big_size 'Z') in
  S.commit tx
;;

(* Read every key back and check it round-trips. *)
let verify st ~n ~big_key ~big_size =
  S.with_ro st (fun tx ->
    let rec chk i =
      if i >= n
      then Lwt.return true
      else
        let* got = S.get tx user_tree (k i) in
        if got = Some (v i) then chk (i + 1) else Lwt.return false
    in
    let* small_ok = chk 0 in
    let* big = S.get tx user_tree big_key in
    let big_ok =
      match big with
      | Some b -> Bytes.length b = big_size && Bytes.for_all (fun c -> c = 'Z') b
      | None -> false
    in
    Lwt.return (small_ok && big_ok))
;;

(* Peek the device's stored geometry straight from page 0. *)
let peek_dev_geometry d =
  let buf = Cstruct.create 4096 in
  (match Hashtbl.find_opt d.pages 0L with
   | Some b -> Cstruct.blit_from_bytes b 0 buf 0 (min (Bytes.length b) 4096)
   | None -> ());
  Header.peek_geometry buf
;;

let big_key = Bytes.of_string "the-big-overflow-value"

let roundtrip_at ?(reserved = 0) ~page_size ~n ~big_size () =
  run
    (let d = make_dev () in
     (* Create at the chosen geometry, populate, commit, close. *)
     let* st = open_store ~geom:(geom ~reserved page_size) d in
     let* () = populate st ~n ~big_key ~big_size in
     let* () = S.close st in
     (* The header self-describes the geometry. *)
     (match peek_dev_geometry d with
      | Some g ->
        Alcotest.(check int) "persisted page_size" page_size g.Geometry.page_size;
        Alcotest.(check int) "persisted reserved" reserved g.reserved_bytes_per_page
      | None -> Alcotest.fail "could not peek persisted geometry from page 0");
     (* REOPEN with no geometry hint: it must be recovered from the header. *)
     let* st2 = open_store d in
     let* ok = verify st2 ~n ~big_size ~big_key in
     let* () = S.close st2 in
     Alcotest.(check bool) "all keys + overflow value round-trip after reopen" true ok;
     Lwt.return_unit)
;;

let test_16k () = roundtrip_at ~page_size:16384 ~n:3000 ~big_size:70000 ()
let test_8k () = roundtrip_at ~page_size:8192 ~n:2000 ~big_size:40000 ()

let test_16k_reserved () =
  roundtrip_at ~page_size:16384 ~reserved:64 ~n:1500 ~big_size:50000 ()
;;

(* A page stamped at 16K must NOT be readable as if it were 4096: the peek
   reports the true geometry so reopen uses correctly-sized buffers. *)
let test_default_still_works () = roundtrip_at ~page_size:4096 ~n:1500 ~big_size:20000 ()

(* Real Unix-file path: exercises the open-time peek + [Unix_file.set_page_size]
   + resize-at-16K glue, persistence across a true close/reopen, and the
   geometry-mismatch rejection. *)
module USt = Sqlocaml_unix.Store

let open_or_fail what r =
  match r with
  | Ok s -> Lwt.return s
  | Error e -> Alcotest.failf "%s: %a" what S.pp_error e
;;

let test_unix_file_persistence () =
  run
    (let path = Filename.temp_file "sqlocaml_geom_" ".db" in
     let n = 1000 in
     let big_size = 50000 in
     (* Create at 16K (explicit), populate, close. *)
     let* r = USt.open_file ~page_size:16384 ~explicit_geometry:true ~path () in
     let* st = open_or_fail "create" r in
     let* () = populate st ~n ~big_key ~big_size in
     let* () = S.close st in
     (* Reopen with NO geometry hint — recovered from the header. *)
     let* r2 = USt.open_file ~path () in
     let* st2 = open_or_fail "reopen" r2 in
     let* ok = verify st2 ~n ~big_size ~big_key in
     let* () = S.close st2 in
     Alcotest.(check bool) "unix 16K file round-trips after reopen" true ok;
     (* Reopening with a conflicting explicit geometry is rejected. *)
     let* mism = USt.open_file ~page_size:8192 ~explicit_geometry:true ~path () in
     let* () =
       match mism with
       | Error _ -> Lwt.return_unit
       | Ok s ->
         let* () = S.close s in
         Alcotest.fail "expected geometry-mismatch rejection on reopen"
     in
     (try Sys.remove path with
      | _ -> ());
     (try Sys.remove (path ^ "-wal") with
      | _ -> ());
     Lwt.return_unit)
;;

(* WAL-mode at 16K: the WAL frame size is [24 + page_size], so this exercises
   16K-sized frames written, recovered, and read back across a reopen. *)
let test_unix_wal_persistence () =
  run
    (let path = Filename.temp_file "sqlocaml_geomwal_" ".db" in
     let n = 800 in
     let big_size = 60000 in
     let* r = USt.open_file_wal ~page_size:16384 ~explicit_geometry:true ~path () in
     let* st = open_or_fail "create wal" r in
     let* () = populate st ~n ~big_key ~big_size in
     let* () = S.close st in
     let* r2 = USt.open_file_wal ~path () in
     let* st2 = open_or_fail "reopen wal" r2 in
     let* ok = verify st2 ~n ~big_size ~big_key in
     let* () = S.close st2 in
     Alcotest.(check bool) "unix 16K WAL round-trips after reopen" true ok;
     (try Sys.remove path with
      | _ -> ());
     (try Sys.remove (path ^ "-wal") with
      | _ -> ());
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "geometry_e2e"
    [ ( "roundtrip"
      , [ Alcotest.test_case "default 4096" `Quick test_default_still_works
        ; Alcotest.test_case "8K page" `Quick test_8k
        ; Alcotest.test_case "16K page" `Quick test_16k
        ; Alcotest.test_case "16K page + 64 reserved" `Quick test_16k_reserved
        ] )
    ; ( "unix_file"
      , [ Alcotest.test_case
            "16K file persists + mismatch rejected"
            `Quick
            test_unix_file_persistence
        ; Alcotest.test_case "16K WAL-mode persists" `Quick test_unix_wal_persistence
        ] )
    ]
;;
