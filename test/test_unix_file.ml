open Lwt.Syntax

module UF = Sqlocaml_block.Unix_file

let page_size = UF.page_size

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let make_buf () = Cstruct.create page_size

let buf_of_bytes b =
  let cs = Cstruct.create (Bytes.length b) in
  Cstruct.blit_from_bytes b 0 cs 0 (Bytes.length b);
  cs

(* Counter for unique test file names *)
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_unix_%04d.db" n

let cleanup path =
  (try Unix.unlink path with _ -> ())

(* ------------------------------------------------------------------ *)
(* 1. OPEN / N_PAGES                                                   *)
(* ------------------------------------------------------------------ *)

let open_creates_file_test () =
  Lwt_main.run (
    let path = fresh_path () in
    (try Unix.unlink path with _ -> ());
    let* r = UF.open_ ~path in
    match r with
    | Error e -> Alcotest.failf "open_ error: %a" UF.pp_error e
    | Ok t ->
      Alcotest.(check int64) "n_pages = 0 on new file" 0L (UF.n_pages t);
      (* file exists on disk *)
      let exists = Sys.file_exists path in
      Alcotest.(check bool) "file created on disk" true exists;
      let* r2 = UF.close t in
      (match r2 with
       | Error e -> Alcotest.failf "close error: %a" UF.pp_error e
       | Ok () -> ());
      cleanup path;
      Lwt.return_unit
  )

let open_existing_file_test () =
  Lwt_main.run (
    let path = fresh_path () in
    (* Create file with 2 pages via resize *)
    let* r = UF.open_ ~path in
    let* () = match r with
     | Error e -> Alcotest.failf "open_ error: %a" UF.pp_error e
     | Ok t ->
       let* _ = UF.resize t ~n_pages:2L in
       let* _ = UF.close t in
       Lwt.return_unit
    in
    (* Re-open; should see 2 pages *)
    let* r2 = UF.open_ ~path in
    let* () = match r2 with
     | Error e -> Alcotest.failf "re-open error: %a" UF.pp_error e
     | Ok t2 ->
       Alcotest.(check int64) "n_pages preserved across open" 2L (UF.n_pages t2);
       let* _ = UF.close t2 in
       Lwt.return_unit
    in
    cleanup path;
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* 2. RESIZE → WRITE → READ ROUND-TRIP                                 *)
(* ------------------------------------------------------------------ *)

let resize_write_read_roundtrip_test () =
  Lwt_main.run (
    let path = fresh_path () in
    let* r = UF.open_ ~path in
    match r with
    | Error e -> Alcotest.failf "open_ error: %a" UF.pp_error e
    | Ok t ->
      let* rz = UF.resize t ~n_pages:4L in
      (match rz with
       | Error e -> Alcotest.failf "resize error: %a" UF.pp_error e
       | Ok () -> ());
      Alcotest.(check int64) "n_pages after resize" 4L (UF.n_pages t);
      let payload = Bytes.init page_size (fun i -> Char.chr (i mod 256)) in
      let wbuf = buf_of_bytes payload in
      let* wr = UF.write_page t ~page_id:2L wbuf in
      (match wr with
       | Error e -> Alcotest.failf "write error: %a" UF.pp_error e
       | Ok () -> ());
      let rbuf = make_buf () in
      let* rr = UF.read_page t ~page_id:2L rbuf in
      (match rr with
       | Error e -> Alcotest.failf "read error: %a" UF.pp_error e
       | Ok () ->
         let got = Cstruct.to_bytes rbuf in
         Alcotest.(check bool) "round-trip exact" true (Bytes.equal payload got));
      let* _ = UF.close t in
      cleanup path;
      Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* 3. READ PAGE OUT OF BOUNDS                                          *)
(* ------------------------------------------------------------------ *)

let read_oob_test () =
  Lwt_main.run (
    let path = fresh_path () in
    let* r = UF.open_ ~path in
    match r with
    | Error e -> Alcotest.failf "open_ error: %a" UF.pp_error e
    | Ok t ->
      let* rz = UF.resize t ~n_pages:2L in
      (match rz with
       | Error e -> Alcotest.failf "resize error: %a" UF.pp_error e
       | Ok () -> ());
      let buf = make_buf () in
      (* page 2 is out-of-bounds when n_pages=2 (valid: 0..1) *)
      let* r2 = UF.read_page t ~page_id:2L buf in
      (match r2 with
       | Ok () -> Alcotest.fail "expected Out_of_bounds, got Ok"
       | Error (UF.Out_of_bounds _) -> ()
       | Error (UF.Io msg) ->
         Alcotest.failf "expected Out_of_bounds, got Io %s" msg);
      (* negative page id *)
      let* r3 = UF.read_page t ~page_id:(-1L) buf in
      (match r3 with
       | Ok () -> Alcotest.fail "expected Out_of_bounds for negative, got Ok"
       | Error (UF.Out_of_bounds _) -> ()
       | Error (UF.Io msg) ->
         Alcotest.failf "expected Out_of_bounds for negative, got Io %s" msg);
      let* _ = UF.close t in
      cleanup path;
      Lwt.return_unit
  )

let write_oob_test () =
  Lwt_main.run (
    let path = fresh_path () in
    let* r = UF.open_ ~path in
    match r with
    | Error e -> Alcotest.failf "open_ error: %a" UF.pp_error e
    | Ok t ->
      let* _ = UF.resize t ~n_pages:2L in
      let buf = make_buf () in
      let* r2 = UF.write_page t ~page_id:2L buf in
      (match r2 with
       | Ok () -> Alcotest.fail "expected Out_of_bounds, got Ok"
       | Error (UF.Out_of_bounds _) -> ()
       | Error (UF.Io msg) ->
         Alcotest.failf "expected Out_of_bounds, got Io %s" msg);
      let* _ = UF.close t in
      cleanup path;
      Lwt.return_unit
  )

let oob_error_fields_test () =
  Lwt_main.run (
    let path = fresh_path () in
    let* r = UF.open_ ~path in
    match r with
    | Error e -> Alcotest.failf "open_ error: %a" UF.pp_error e
    | Ok t ->
      let* _ = UF.resize t ~n_pages:3L in
      let buf = make_buf () in
      let* r2 = UF.read_page t ~page_id:5L buf in
      (match r2 with
       | Ok () -> Alcotest.fail "expected OOB error"
       | Error (UF.Out_of_bounds { page_id; n_pages }) ->
         Alcotest.(check int64) "page_id" 5L page_id;
         Alcotest.(check int64) "n_pages" 3L n_pages
       | Error (UF.Io msg) ->
         Alcotest.failf "expected Out_of_bounds, got Io %s" msg);
      let* _ = UF.close t in
      cleanup path;
      Lwt.return_unit
  )

let read_oob_empty_test () =
  Lwt_main.run (
    let path = fresh_path () in
    let* r = UF.open_ ~path in
    match r with
    | Error e -> Alcotest.failf "open_ error: %a" UF.pp_error e
    | Ok t ->
      (* n_pages = 0, any read is OOB *)
      let buf = make_buf () in
      let* r2 = UF.read_page t ~page_id:0L buf in
      (match r2 with
       | Ok () -> Alcotest.fail "expected OOB on empty file"
       | Error (UF.Out_of_bounds _) -> ()
       | Error (UF.Io msg) ->
         Alcotest.failf "expected Out_of_bounds, got Io %s" msg);
      let* _ = UF.close t in
      cleanup path;
      Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* 4. SYNC                                                             *)
(* ------------------------------------------------------------------ *)

let sync_after_write_test () =
  Lwt_main.run (
    let path = fresh_path () in
    let* r = UF.open_ ~path in
    match r with
    | Error e -> Alcotest.failf "open_ error: %a" UF.pp_error e
    | Ok t ->
      let* _ = UF.resize t ~n_pages:1L in
      let wbuf = make_buf () in
      Cstruct.set_char wbuf 0 'S';
      let* wr = UF.write_page t ~page_id:0L wbuf in
      (match wr with
       | Error e -> Alcotest.failf "write error: %a" UF.pp_error e
       | Ok () -> ());
      let* sr = UF.sync t in
      (match sr with
       | Error e -> Alcotest.failf "sync error: %a" UF.pp_error e
       | Ok () -> ());
      (* data still readable after sync *)
      let rbuf = make_buf () in
      let* rr = UF.read_page t ~page_id:0L rbuf in
      (match rr with
       | Error e -> Alcotest.failf "read error: %a" UF.pp_error e
       | Ok () ->
         Alcotest.(check char) "data intact after sync" 'S' (Cstruct.get_char rbuf 0));
      let* _ = UF.close t in
      cleanup path;
      Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* 5. FLOCK: second open same path while first is open                 *)
(* ------------------------------------------------------------------ *)

let flock_conflict_test () =
  Lwt_main.run (
    let path = fresh_path () in
    let* r1 = UF.open_ ~path in
    match r1 with
    | Error e -> Alcotest.failf "first open_ error: %a" UF.pp_error e
    | Ok t1 ->
      let* r2 = UF.open_ ~path in
      (match r2 with
       | Ok _ ->
         Alcotest.fail "expected lock conflict on second open_, got Ok"
       | Error (UF.Io _) -> () (* expected *)
       | Error (UF.Out_of_bounds _) ->
         Alcotest.fail "expected Io error for lock conflict, got Out_of_bounds");
      let* _ = UF.close t1 in
      cleanup path;
      Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* 6. CLOSE RELEASES LOCK; third open succeeds                         *)
(* ------------------------------------------------------------------ *)

let close_releases_lock_test () =
  Lwt_main.run (
    let path = fresh_path () in
    let* r1 = UF.open_ ~path in
    let t1 = match r1 with
      | Error e -> Alcotest.failf "first open_ error: %a" UF.pp_error e
      | Ok t -> t
    in
    (* second open should fail *)
    let* r2 = UF.open_ ~path in
    (match r2 with
     | Ok _ -> Alcotest.fail "expected conflict before close"
     | Error _ -> ());
    (* close t1 *)
    let* cl = UF.close t1 in
    (match cl with
     | Error e -> Alcotest.failf "close error: %a" UF.pp_error e
     | Ok () -> ());
    (* third open after close should succeed *)
    let* r3 = UF.open_ ~path in
    let* () = match r3 with
     | Error e -> Alcotest.failf "open after close error: %a" UF.pp_error e
     | Ok t3 ->
       let* _ = UF.close t3 in
       Lwt.return_unit
    in
    cleanup path;
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* 7. PP_ERROR FORMATTING                                              *)
(* ------------------------------------------------------------------ *)

let string_contains haystack needle =
  let hl = String.length haystack and nl = String.length needle in
  let rec go i =
    if i > hl - nl then false
    else if String.sub haystack i nl = needle then true
    else go (i + 1)
  in
  go 0

let pp_error_io_test () =
  let err = UF.Io "something went wrong" in
  let s = Format.asprintf "%a" UF.pp_error err in
  Alcotest.(check bool) "contains message" true
    (string_contains s "something went wrong")

let pp_error_oob_test () =
  let err = UF.Out_of_bounds { page_id = 42L; n_pages = 10L } in
  let s = Format.asprintf "%a" UF.pp_error err in
  Alcotest.(check bool) "contains page_id" true (string_contains s "42");
  Alcotest.(check bool) "contains n_pages" true (string_contains s "10")

(* ------------------------------------------------------------------ *)
(* 8. RESIZE SPECIFIC CASES                                            *)
(* ------------------------------------------------------------------ *)

let resize_grow_test () =
  Lwt_main.run (
    let path = fresh_path () in
    let* r = UF.open_ ~path in
    match r with
    | Error e -> Alcotest.failf "open_ error: %a" UF.pp_error e
    | Ok t ->
      let* _ = UF.resize t ~n_pages:2L in
      let* rz = UF.resize t ~n_pages:8L in
      (match rz with
       | Error e -> Alcotest.failf "resize grow error: %a" UF.pp_error e
       | Ok () -> ());
      Alcotest.(check int64) "n_pages after grow" 8L (UF.n_pages t);
      (* New pages should be readable (file was truncated to bigger size, bytes are 0) *)
      let buf = make_buf () in
      let* rr = UF.read_page t ~page_id:7L buf in
      (match rr with
       | Error e -> Alcotest.failf "read page 7 error: %a" UF.pp_error e
       | Ok () -> ());
      let* _ = UF.close t in
      cleanup path;
      Lwt.return_unit
  )

let resize_shrink_test () =
  Lwt_main.run (
    let path = fresh_path () in
    let* r = UF.open_ ~path in
    match r with
    | Error e -> Alcotest.failf "open_ error: %a" UF.pp_error e
    | Ok t ->
      let* _ = UF.resize t ~n_pages:8L in
      let* rz = UF.resize t ~n_pages:4L in
      (match rz with
       | Error e -> Alcotest.failf "resize shrink error: %a" UF.pp_error e
       | Ok () -> ());
      Alcotest.(check int64) "n_pages after shrink" 4L (UF.n_pages t);
      let buf = make_buf () in
      let* rr = UF.read_page t ~page_id:7L buf in
      (match rr with
       | Ok () -> Alcotest.fail "expected OOB after shrink"
       | Error (UF.Out_of_bounds _) -> ()
       | Error e -> Alcotest.failf "unexpected error: %a" UF.pp_error e);
      let* _ = UF.close t in
      cleanup path;
      Lwt.return_unit
  )

let resize_preserves_data_test () =
  Lwt_main.run (
    let path = fresh_path () in
    let* r = UF.open_ ~path in
    match r with
    | Error e -> Alcotest.failf "open_ error: %a" UF.pp_error e
    | Ok t ->
      let* _ = UF.resize t ~n_pages:2L in
      let wbuf = make_buf () in
      Cstruct.set_char wbuf 0 'P';
      let* _ = UF.write_page t ~page_id:0L wbuf in
      let* _ = UF.resize t ~n_pages:6L in
      let rbuf = make_buf () in
      let* rr = UF.read_page t ~page_id:0L rbuf in
      (match rr with
       | Error e -> Alcotest.failf "read error: %a" UF.pp_error e
       | Ok () ->
         Alcotest.(check char) "page 0 preserved after grow" 'P' (Cstruct.get_char rbuf 0));
      let* _ = UF.close t in
      cleanup path;
      Lwt.return_unit
  )

let resize_to_zero_test () =
  Lwt_main.run (
    let path = fresh_path () in
    let* r = UF.open_ ~path in
    match r with
    | Error e -> Alcotest.failf "open_ error: %a" UF.pp_error e
    | Ok t ->
      let* _ = UF.resize t ~n_pages:4L in
      let* rz = UF.resize t ~n_pages:0L in
      (match rz with
       | Error e -> Alcotest.failf "resize to 0 error: %a" UF.pp_error e
       | Ok () -> ());
      Alcotest.(check int64) "n_pages = 0" 0L (UF.n_pages t);
      let buf = make_buf () in
      let* rr = UF.read_page t ~page_id:0L buf in
      (match rr with
       | Ok () -> Alcotest.fail "expected OOB after resize to 0"
       | Error (UF.Out_of_bounds _) -> ()
       | Error e -> Alcotest.failf "unexpected error: %a" UF.pp_error e);
      let* _ = UF.close t in
      cleanup path;
      Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* 9. PAGE ISOLATION                                                   *)
(* ------------------------------------------------------------------ *)

let page_isolation_test () =
  Lwt_main.run (
    let path = fresh_path () in
    let* r = UF.open_ ~path in
    match r with
    | Error e -> Alcotest.failf "open_ error: %a" UF.pp_error e
    | Ok t ->
      let* _ = UF.resize t ~n_pages:4L in
      (* Write 0xAB to page 0 *)
      let wbuf = make_buf () in
      Cstruct.memset wbuf 0xAB;
      let* _ = UF.write_page t ~page_id:0L wbuf in
      (* Write 0xCD to page 3 *)
      let wbuf2 = make_buf () in
      Cstruct.memset wbuf2 0xCD;
      let* _ = UF.write_page t ~page_id:3L wbuf2 in
      (* Page 1 should not be 0xAB or 0xCD *)
      let rbuf = make_buf () in
      let* rr = UF.read_page t ~page_id:1L rbuf in
      (match rr with
       | Error e -> Alcotest.failf "read page 1 error: %a" UF.pp_error e
       | Ok () ->
         (* page 1 should be all zeros (ftruncate extends with zeros) *)
         let all_zero = Cstruct.for_all (fun c -> c = '\x00') rbuf in
         Alcotest.(check bool) "page 1 not corrupted" true all_zero);
      (* Verify page 0 still intact *)
      let rbuf0 = make_buf () in
      let* rr0 = UF.read_page t ~page_id:0L rbuf0 in
      (match rr0 with
       | Error e -> Alcotest.failf "read page 0 error: %a" UF.pp_error e
       | Ok () ->
         Alcotest.(check char) "page 0 byte 0 = 0xAB" '\xAB' (Cstruct.get_char rbuf0 0));
      let* _ = UF.close t in
      cleanup path;
      Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* 9b. Cross-process file-lock conflict (sub-process holding the lock) *)
(* ------------------------------------------------------------------ *)

(* Spawn a child that locks the file and stays alive; the parent then
   tries to open the same path and must see an "already locked" error. *)
let cross_process_lock_test () =
  let path = fresh_path () in
  (try Unix.unlink path with _ -> ());
  (* Ensure the file exists with at least one page so it has bytes for lockf *)
  Lwt_main.run (
    let* r = UF.open_ ~path in
    match r with
    | Error e -> Alcotest.failf "prepare: %a" UF.pp_error e
    | Ok t ->
      let* _ = UF.resize t ~n_pages:1L in
      let* _ = UF.close t in
      Lwt.return_unit
  );
  let r_pipe, w_pipe = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
    (* Child: open the file, lock it, signal parent, sleep, exit. *)
    Unix.close r_pipe;
    let fd = Unix.openfile path [Unix.O_RDWR] 0o644 in
    Unix.lockf fd Unix.F_TLOCK 0;
    let _ = Unix.write w_pipe (Bytes.of_string "x") 0 1 in
    Unix.close w_pipe;
    Unix.sleep 2;
    Unix.close fd;
    exit 0
  | child_pid ->
    Unix.close w_pipe;
    (* Wait for child to signal it has the lock *)
    let buf = Bytes.create 1 in
    let _ = Unix.read r_pipe buf 0 1 in
    Unix.close r_pipe;
    let result =
      Lwt_main.run (
        let* r = UF.open_ ~path in
        match r with
        | Ok t ->
          let* _ = UF.close t in
          Lwt.return `Success
        | Error _ -> Lwt.return `Locked
      )
    in
    let _ = Unix.waitpid [] child_pid in
    cleanup path;
    (match result with
     | `Locked -> ()
     | `Success ->
       Alcotest.fail "expected open to fail while child holds the lock")

(* Trigger Unix errors on operations against a file whose fd is already
   closed.  We do this by reaching into the public API: open, immediately
   close (which releases the lock), then call read/write/sync/resize on
   the closed handle.  These all hit the Unix.Unix_error / Failure arms. *)
let ops_on_closed_fd_test () =
  Lwt_main.run (
    let path = fresh_path () in
    let* r = UF.open_ ~path in
    match r with
    | Error e -> Alcotest.failf "open: %a" UF.pp_error e
    | Ok t ->
      let* _ = UF.resize t ~n_pages:1L in
      let* _ = UF.close t in
      (* All subsequent operations against [t] hit a closed fd → Unix errors *)
      let buf = make_buf () in
      let* r_read = UF.read_page t ~page_id:0L buf in
      let read_io = match r_read with
        | Error (UF.Io _) -> true
        | _ -> false
      in
      let* r_write = UF.write_page t ~page_id:0L buf in
      let write_io = match r_write with
        | Error (UF.Io _) -> true
        | _ -> false
      in
      let* r_sync = UF.sync t in
      let sync_io = match r_sync with
        | Error (UF.Io _) -> true
        | _ -> false
      in
      let* r_resize = UF.resize t ~n_pages:2L in
      let resize_io = match r_resize with
        | Error (UF.Io _) -> true
        | _ -> false
      in
      let* r_close2 = UF.close t in
      let close_io = match r_close2 with
        | Error (UF.Io _) -> true
        | _ -> false
      in
      Alcotest.(check bool) "read on closed fd: Io"   true read_io;
      Alcotest.(check bool) "write on closed fd: Io"  true write_io;
      Alcotest.(check bool) "sync on closed fd: Io"   true sync_io;
      Alcotest.(check bool) "resize on closed fd: Io" true resize_io;
      Alcotest.(check bool) "close on closed fd: Io"  true close_io;
      cleanup path;
      Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* 10. QCHECK PROPERTY TESTS                                           *)
(* ------------------------------------------------------------------ *)

let prop_write_read_roundtrip =
  let gen =
    QCheck.Gen.(
      let* n_pages = int_range 1 8 in
      let* page_id = int_range 0 (n_pages - 1) in
      let* payload = bytes_size (return 4096) in
      return (n_pages, page_id, payload)
    )
  in
  QCheck.Test.make
    ~name:"prop_unix_write_read_roundtrip"
    ~count:10_000
    (QCheck.make gen)
    (fun (n_pages, page_id, payload) ->
       let path = fresh_path () in
       let result =
         Lwt_main.run (
           let* ro = UF.open_ ~path in
           match ro with
           | Error _ -> Lwt.return false
           | Ok t ->
             let* _ = UF.resize t ~n_pages:(Int64.of_int n_pages) in
             let wbuf = buf_of_bytes payload in
             let* wr = UF.write_page t ~page_id:(Int64.of_int page_id) wbuf in
             (match wr with
              | Error _ ->
                let* _ = UF.close t in
                Lwt.return false
              | Ok () ->
                let rbuf = make_buf () in
                let* rr = UF.read_page t ~page_id:(Int64.of_int page_id) rbuf in
                let ok = match rr with
                  | Error _ -> false
                  | Ok () ->
                    let got = Cstruct.to_bytes rbuf in
                    Bytes.equal payload got
                in
                let* _ = UF.close t in
                Lwt.return ok)
         )
       in
       cleanup path;
       result)

let prop_page_independence =
  let gen =
    QCheck.Gen.(
      let* n_pages = int_range 2 8 in
      (* Two different pages *)
      let* p1 = int_range 0 (n_pages - 1) in
      let* p2_offset = int_range 1 (n_pages - 1) in
      let p2 = (p1 + p2_offset) mod n_pages in
      let* payload1 = bytes_size (return 4096) in
      let* payload2 = bytes_size (return 4096) in
      return (n_pages, p1, p2, payload1, payload2)
    )
  in
  QCheck.Test.make
    ~name:"prop_unix_page_independence"
    ~count:10_000
    (QCheck.make gen)
    (fun (n_pages, p1, p2, payload1, payload2) ->
       let path = fresh_path () in
       let result =
         Lwt_main.run (
           let* ro = UF.open_ ~path in
           match ro with
           | Error _ -> Lwt.return false
           | Ok t ->
             let* _ = UF.resize t ~n_pages:(Int64.of_int n_pages) in
             let wbuf1 = buf_of_bytes payload1 in
             let wbuf2 = buf_of_bytes payload2 in
             let* _ = UF.write_page t ~page_id:(Int64.of_int p1) wbuf1 in
             let* _ = UF.write_page t ~page_id:(Int64.of_int p2) wbuf2 in
             (* Read both back *)
             let rbuf1 = make_buf () in
             let rbuf2 = make_buf () in
             let* rr1 = UF.read_page t ~page_id:(Int64.of_int p1) rbuf1 in
             let* rr2 = UF.read_page t ~page_id:(Int64.of_int p2) rbuf2 in
             let ok = match rr1, rr2 with
               | Ok (), Ok () ->
                 let got1 = Cstruct.to_bytes rbuf1 in
                 let got2 = Cstruct.to_bytes rbuf2 in
                 Bytes.equal payload1 got1 && Bytes.equal payload2 got2
               | _ -> false
             in
             let* _ = UF.close t in
             Lwt.return ok
         )
       in
       cleanup path;
       result)

let prop_oob_always_errors =
  let gen =
    QCheck.Gen.(
      let* n_pages = int_range 1 8 in
      let oob_gen = oneof [
        map (fun n -> Int64.of_int (-(n + 1))) (int_range 0 100);
        map (fun n -> Int64.of_int (n + n_pages)) (int_range 0 100);
      ] in
      let* page_id = oob_gen in
      return (n_pages, page_id)
    )
  in
  QCheck.Test.make
    ~name:"prop_unix_oob_always_errors"
    ~count:10_000
    (QCheck.make gen)
    (fun (n_pages, page_id) ->
       let path = fresh_path () in
       let result =
         Lwt_main.run (
           let* ro = UF.open_ ~path in
           match ro with
           | Error _ -> Lwt.return false
           | Ok t ->
             let* _ = UF.resize t ~n_pages:(Int64.of_int n_pages) in
             let buf = make_buf () in
             let* rr = UF.read_page t ~page_id buf in
             let* wr = UF.write_page t ~page_id buf in
             let r_err = match rr with Error _ -> true | Ok () -> false in
             let w_err = match wr with Error _ -> true | Ok () -> false in
             let* _ = UF.close t in
             Lwt.return (r_err && w_err)
         )
       in
       cleanup path;
       result)

let prop_resize_n_pages_correct =
  QCheck.Test.make
    ~name:"prop_unix_resize_n_pages_correct"
    ~count:10_000
    (QCheck.make (QCheck.Gen.int_range 0 16))
    (fun n ->
       let path = fresh_path () in
       let result =
         Lwt_main.run (
           let* ro = UF.open_ ~path in
           match ro with
           | Error _ -> Lwt.return false
           | Ok t ->
             let* r = UF.resize t ~n_pages:(Int64.of_int n) in
             let ok = match r with
               | Error _ -> false
               | Ok () -> UF.n_pages t = Int64.of_int n
             in
             let* _ = UF.close t in
             Lwt.return ok
         )
       in
       cleanup path;
       result)

(* ------------------------------------------------------------------ *)
(* COOPERATION (#158)                                                  *)
(* ------------------------------------------------------------------ *)

(* [read_page] must hand the scheduler off to other Lwt fibers while the
   syscall is in flight.  Before #158, [Unix_file] wrapped [Unix.read]
   in [Lwt.return], so the bind never yielded — a tight reader loop
   monopolised the scheduler and any concurrently posted timer (e.g.
   the writer's [wal_sync] sleep) waited until the reader was done.

   Verification: post a 50 ms timer alongside a 10 000-iteration
   [read_page] loop.  If [read_page] is genuinely async the timer fires
   close to its scheduled deadline and well before the reader finishes;
   if it isn't, the timer is delayed until after the entire read loop.
   The 5x slack on the timer-fired-before-reader-done assertion absorbs
   container/CI jitter.  *)
let read_page_yields_to_timer_test () =
  Lwt_main.run (
    let path = fresh_path () in
    let* r = UF.open_ ~path in
    match r with
    | Error e -> Alcotest.failf "open_ error: %a" UF.pp_error e
    | Ok t ->
      let n_pages = 16L in
      let* rz = UF.resize t ~n_pages in
      (match rz with
       | Error e -> Alcotest.failf "resize error: %a" UF.pp_error e
       | Ok () -> ());
      (* Seed every page so [read_page] succeeds. *)
      let wbuf = make_buf () in
      Cstruct.set_char wbuf 0 'X';
      let rec seed i =
        if Int64.compare i n_pages >= 0 then Lwt.return_unit
        else
          let* _ = UF.write_page t ~page_id:i wbuf in
          seed (Int64.add i 1L)
      in
      let* () = seed 0L in
      let* _ = UF.sync t in

      let t0 = Unix.gettimeofday () in
      let timer_fired_at = ref None in
      let reader_done_at = ref None in

      let timer =
        let* () = Lwt_unix.sleep 0.05 in
        timer_fired_at := Some (Unix.gettimeofday () -. t0);
        Lwt.return_unit
      in
      let reader =
        let rbuf = make_buf () in
        let rec loop i =
          if i >= 10_000 then Lwt.return_unit
          else
            let pid = Int64.of_int (i mod 16) in
            let* _ = UF.read_page t ~page_id:pid rbuf in
            loop (i + 1)
        in
        let* () = loop 0 in
        reader_done_at := Some (Unix.gettimeofday () -. t0);
        Lwt.return_unit
      in
      let* () = Lwt.join [timer; reader] in

      let* _ = UF.close t in
      cleanup path;

      (* The timer must have fired during the reader's run, not after it
         completed.  If the reader monopolised the scheduler, both would
         land at the same wall-time, so we require the timer to fire by
         5x its nominal deadline (250 ms) — wide enough to absorb the
         worst container/CI jitter we've observed, tight enough to fail
         loudly if cooperation regresses to "never yields". *)
      let timer_at =
        match !timer_fired_at with
        | Some t -> t
        | None -> Alcotest.fail "timer never fired"
      in
      let reader_at =
        match !reader_done_at with
        | Some t -> t
        | None -> Alcotest.fail "reader never finished"
      in
      Alcotest.(check bool)
        (Printf.sprintf
           "timer fired (%.3fs) inside Lwt-budget (≤ 0.25s)" timer_at)
        true (timer_at <= 0.25);
      Alcotest.(check bool)
        (Printf.sprintf
           "timer fired (%.3fs) before reader finished (%.3fs)"
           timer_at reader_at)
        true (timer_at < reader_at);
      Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* RUNNER                                                              *)
(* ------------------------------------------------------------------ *)

let () =
  let qcheck_tests =
    List.map QCheck_alcotest.to_alcotest [
      prop_write_read_roundtrip;
      prop_page_independence;
      prop_oob_always_errors;
      prop_resize_n_pages_correct;
    ]
  in
  Alcotest.run "unix_file" [
    "open", [
      Alcotest.test_case "open_ creates file"              `Quick open_creates_file_test;
      Alcotest.test_case "open_ preserves n_pages"         `Quick open_existing_file_test;
    ];
    "read_write", [
      Alcotest.test_case "resize_write_read_roundtrip"     `Quick resize_write_read_roundtrip_test;
      Alcotest.test_case "page_isolation"                  `Quick page_isolation_test;
    ];
    "oob", [
      Alcotest.test_case "read_oob"                        `Quick read_oob_test;
      Alcotest.test_case "write_oob"                       `Quick write_oob_test;
      Alcotest.test_case "oob_error_fields"                `Quick oob_error_fields_test;
      Alcotest.test_case "read_oob_empty"                  `Quick read_oob_empty_test;
    ];
    "sync", [
      Alcotest.test_case "sync_after_write"                `Quick sync_after_write_test;
    ];
    "flock", [
      Alcotest.test_case "flock_conflict"                  `Quick flock_conflict_test;
      Alcotest.test_case "close_releases_lock"             `Quick close_releases_lock_test;
      Alcotest.test_case "cross_process_lock"              `Slow  cross_process_lock_test;
    ];
    "closed_fd", [
      Alcotest.test_case "ops_on_closed_fd"                `Quick ops_on_closed_fd_test;
    ];
    "resize", [
      Alcotest.test_case "resize_grow"                     `Quick resize_grow_test;
      Alcotest.test_case "resize_shrink"                   `Quick resize_shrink_test;
      Alcotest.test_case "resize_preserves_data"           `Quick resize_preserves_data_test;
      Alcotest.test_case "resize_to_zero"                  `Quick resize_to_zero_test;
    ];
    "pp_error", [
      Alcotest.test_case "pp_error_io"                     `Quick pp_error_io_test;
      Alcotest.test_case "pp_error_oob"                    `Quick pp_error_oob_test;
    ];
    "cooperation", [
      Alcotest.test_case "read_page yields to concurrent timer"
        `Quick read_page_yields_to_timer_test;
    ];
    "qcheck", qcheck_tests;
  ]
