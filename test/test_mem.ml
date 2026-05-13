open Lwt.Syntax

module Mem = Sqlocaml_block.Mem

(* Helper: check if [needle] appears anywhere in [haystack] *)
let string_contains haystack needle =
  let hl = String.length haystack and nl = String.length needle in
  let rec go i =
    if i > hl - nl then false
    else if String.sub haystack i nl = needle then true
    else go (i + 1)
  in
  go 0

let page_size = Mem.page_size

(* Helper: create a fresh Cstruct buffer of page_size *)
let make_buf () = Cstruct.create page_size

(* Helper: fill a Cstruct with bytes from a Bytes.t *)
let buf_of_bytes b =
  let cs = Cstruct.create (Bytes.length b) in
  Cstruct.blit_from_bytes b 0 cs 0 (Bytes.length b);
  cs

(* ------------------------------------------------------------------ *)
(* 1. BASIC READ / WRITE                                               *)
(* ------------------------------------------------------------------ *)

let read_zeros_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:4L in
    let buf = make_buf () in
    let* r = Mem.read_page m ~page_id:0L buf in
    match r with
    | Error e ->
      Alcotest.failf "unexpected error: %a" Mem.pp_error e
    | Ok () ->
      let all_zero = Cstruct.for_all (fun c -> c = '\x00') buf in
      Alcotest.(check bool) "fresh page is zero" true all_zero;
      Lwt.return_unit
  )

let write_then_read_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:4L in
    let hello = Bytes.of_string "hello" in
    let wbuf = make_buf () in
    Cstruct.blit_from_bytes hello 0 wbuf 0 (Bytes.length hello);
    let* wr = Mem.write_page m ~page_id:2L wbuf in
    (match wr with
     | Error e -> Alcotest.failf "write error: %a" Mem.pp_error e
     | Ok () -> ());
    let rbuf = make_buf () in
    let* rr = Mem.read_page m ~page_id:2L rbuf in
    (match rr with
     | Error e -> Alcotest.failf "read error: %a" Mem.pp_error e
     | Ok () ->
       let got = Cstruct.to_bytes rbuf in
       for i = 0 to 4 do
         Alcotest.(check char)
           (Printf.sprintf "byte %d" i)
           (Bytes.get hello i)
           (Bytes.get got i)
       done);
    Lwt.return_unit
  )

let page_isolation_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:4L in
    let wbuf = make_buf () in
    Cstruct.memset wbuf 0xAB;
    let* wr = Mem.write_page m ~page_id:0L wbuf in
    (match wr with
     | Error e -> Alcotest.failf "write error: %a" Mem.pp_error e
     | Ok () -> ());
    let rbuf = make_buf () in
    let* rr = Mem.read_page m ~page_id:1L rbuf in
    (match rr with
     | Error e -> Alcotest.failf "read error: %a" Mem.pp_error e
     | Ok () ->
       let all_zero = Cstruct.for_all (fun c -> c = '\x00') rbuf in
       Alcotest.(check bool) "page 1 untouched" true all_zero);
    Lwt.return_unit
  )

let full_page_write_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:2L in
    let payload = Bytes.init page_size (fun i -> Char.chr (i mod 256)) in
    let wbuf = buf_of_bytes payload in
    let* wr = Mem.write_page m ~page_id:0L wbuf in
    (match wr with
     | Error e -> Alcotest.failf "write error: %a" Mem.pp_error e
     | Ok () -> ());
    let rbuf = make_buf () in
    let* rr = Mem.read_page m ~page_id:0L rbuf in
    (match rr with
     | Error e -> Alcotest.failf "read error: %a" Mem.pp_error e
     | Ok () ->
       let got = Cstruct.to_bytes rbuf in
       Alcotest.(check bool) "full page matches" true (Bytes.equal payload got));
    Lwt.return_unit
  )

let overwrite_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:2L in
    let mk s =
      let b = Bytes.make page_size '\x00' in
      String.iteri (fun i c -> Bytes.set b i c) s;
      buf_of_bytes b
    in
    let* _ = Mem.write_page m ~page_id:0L (mk "aaa") in
    let* _ = Mem.write_page m ~page_id:0L (mk "bbb") in
    let rbuf = make_buf () in
    let* rr = Mem.read_page m ~page_id:0L rbuf in
    (match rr with
     | Error e -> Alcotest.failf "read error: %a" Mem.pp_error e
     | Ok () ->
       Alcotest.(check char) "byte 0 = b" 'b' (Cstruct.get_char rbuf 0);
       Alcotest.(check char) "byte 1 = b" 'b' (Cstruct.get_char rbuf 1);
       Alcotest.(check char) "byte 2 = b" 'b' (Cstruct.get_char rbuf 2));
    Lwt.return_unit
  )

let write_last_valid_page_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:4L in
    let wbuf = make_buf () in
    Cstruct.set_char wbuf 0 'Z';
    let* wr = Mem.write_page m ~page_id:3L wbuf in
    (match wr with
     | Error e -> Alcotest.failf "write error: %a" Mem.pp_error e
     | Ok () -> ());
    let rbuf = make_buf () in
    let* rr = Mem.read_page m ~page_id:3L rbuf in
    (match rr with
     | Error e -> Alcotest.failf "read error: %a" Mem.pp_error e
     | Ok () ->
       Alcotest.(check char) "byte 0 = Z" 'Z' (Cstruct.get_char rbuf 0));
    Lwt.return_unit
  )

let write_page_0_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:1L in
    let wbuf = make_buf () in
    Cstruct.set_char wbuf 0 'X';
    let* wr = Mem.write_page m ~page_id:0L wbuf in
    (match wr with
     | Error e -> Alcotest.failf "write error: %a" Mem.pp_error e
     | Ok () -> ());
    let rbuf = make_buf () in
    let* rr = Mem.read_page m ~page_id:0L rbuf in
    (match rr with
     | Error e -> Alcotest.failf "read error: %a" Mem.pp_error e
     | Ok () ->
       Alcotest.(check char) "byte 0 = X" 'X' (Cstruct.get_char rbuf 0));
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* 2. OUT-OF-BOUNDS ERRORS                                             *)
(* ------------------------------------------------------------------ *)

let read_oob_exact_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:4L in
    let buf = make_buf () in
    let* r = Mem.read_page m ~page_id:4L buf in
    (match r with
     | Ok () -> Alcotest.fail "expected OOB error, got Ok"
     | Error (Mem.Out_of_bounds _) -> ());
    Lwt.return_unit
  )

let read_oob_high_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:4L in
    let buf = make_buf () in
    let* r = Mem.read_page m ~page_id:100L buf in
    (match r with
     | Ok () -> Alcotest.fail "expected OOB error, got Ok"
     | Error (Mem.Out_of_bounds _) -> ());
    Lwt.return_unit
  )

let read_oob_negative_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:4L in
    let buf = make_buf () in
    let* r = Mem.read_page m ~page_id:(-1L) buf in
    (match r with
     | Ok () -> Alcotest.fail "expected OOB error, got Ok"
     | Error (Mem.Out_of_bounds _) -> ());
    Lwt.return_unit
  )

let write_oob_exact_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:4L in
    let buf = make_buf () in
    let* r = Mem.write_page m ~page_id:4L buf in
    (match r with
     | Ok () -> Alcotest.fail "expected OOB error, got Ok"
     | Error (Mem.Out_of_bounds _) -> ());
    Lwt.return_unit
  )

let write_oob_negative_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:4L in
    let buf = make_buf () in
    let* r = Mem.write_page m ~page_id:(-1L) buf in
    (match r with
     | Ok () -> Alcotest.fail "expected OOB error, got Ok"
     | Error (Mem.Out_of_bounds _) -> ());
    Lwt.return_unit
  )

let oob_error_fields_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:4L in
    let buf = make_buf () in
    let* r = Mem.read_page m ~page_id:5L buf in
    (match r with
     | Ok () -> Alcotest.fail "expected OOB error, got Ok"
     | Error (Mem.Out_of_bounds { page_id; n_pages }) ->
       Alcotest.(check int64) "page_id" 5L page_id;
       Alcotest.(check int64) "n_pages" 4L n_pages);
    Lwt.return_unit
  )

let empty_backend_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:0L in
    let buf = make_buf () in
    let* rr = Mem.read_page m ~page_id:0L buf in
    (match rr with
     | Ok () -> Alcotest.fail "read: expected OOB on empty backend"
     | Error (Mem.Out_of_bounds _) -> ());
    let* wr = Mem.write_page m ~page_id:0L buf in
    (match wr with
     | Ok () -> Alcotest.fail "write: expected OOB on empty backend"
     | Error (Mem.Out_of_bounds _) -> ());
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* 3. RESIZE                                                           *)
(* ------------------------------------------------------------------ *)

let resize_grow_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:2L in
    let* r = Mem.resize m ~n_pages:8L in
    (match r with
     | Error e -> Alcotest.failf "resize error: %a" Mem.pp_error e
     | Ok () -> ());
    Alcotest.(check int64) "n_pages after grow" 8L (Mem.n_pages m);
    let buf = make_buf () in
    let* rr = Mem.read_page m ~page_id:7L buf in
    (match rr with
     | Error e -> Alcotest.failf "read page 7 error: %a" Mem.pp_error e
     | Ok () ->
       let all_zero = Cstruct.for_all (fun c -> c = '\x00') buf in
       Alcotest.(check bool) "new page is zero" true all_zero);
    Lwt.return_unit
  )

let resize_shrink_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:8L in
    let wbuf = make_buf () in
    Cstruct.set_char wbuf 0 'S';
    let* _ = Mem.write_page m ~page_id:7L wbuf in
    let* r = Mem.resize m ~n_pages:4L in
    (match r with
     | Error e -> Alcotest.failf "resize error: %a" Mem.pp_error e
     | Ok () -> ());
    Alcotest.(check int64) "n_pages after shrink" 4L (Mem.n_pages m);
    let rbuf = make_buf () in
    let* rr = Mem.read_page m ~page_id:7L rbuf in
    (match rr with
     | Ok () -> Alcotest.fail "expected OOB after shrink"
     | Error (Mem.Out_of_bounds _) -> ());
    Lwt.return_unit
  )

let resize_grow_preserves_data_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:2L in
    let wbuf = make_buf () in
    Cstruct.set_char wbuf 0 'P';
    let* _ = Mem.write_page m ~page_id:0L wbuf in
    let* _ = Mem.resize m ~n_pages:6L in
    let rbuf = make_buf () in
    let* rr = Mem.read_page m ~page_id:0L rbuf in
    (match rr with
     | Error e -> Alcotest.failf "read error: %a" Mem.pp_error e
     | Ok () ->
       Alcotest.(check char) "page 0 data preserved" 'P' (Cstruct.get_char rbuf 0));
    Lwt.return_unit
  )

let resize_shrink_then_grow_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:8L in
    (* Write to all pages *)
    let wbuf = make_buf () in
    Cstruct.memset wbuf 0xFF;
    let* () = Lwt_list.iter_s (fun i ->
      let* _ = Mem.write_page m ~page_id:(Int64.of_int i) wbuf in
      Lwt.return_unit
    ) [0; 1; 2; 3; 4; 5; 6; 7] in
    let* _ = Mem.resize m ~n_pages:2L in
    let* _ = Mem.resize m ~n_pages:6L in
    Alcotest.(check int64) "n_pages" 6L (Mem.n_pages m);
    (* New pages 2..5 must be zero *)
    let* () = Lwt_list.iter_s (fun i ->
      let rbuf = make_buf () in
      let* rr = Mem.read_page m ~page_id:(Int64.of_int i) rbuf in
      (match rr with
       | Error e -> Alcotest.failf "read page %d error: %a" i Mem.pp_error e
       | Ok () ->
         Alcotest.(check bool)
           (Printf.sprintf "page %d zero after regrow" i)
           true
           (Cstruct.for_all (fun c -> c = '\x00') rbuf));
      Lwt.return_unit
    ) [2; 3; 4; 5] in
    Lwt.return_unit
  )

let resize_to_same_size_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:4L in
    let wbuf = make_buf () in
    Cstruct.set_char wbuf 0 'Q';
    let* _ = Mem.write_page m ~page_id:0L wbuf in
    let* r = Mem.resize m ~n_pages:4L in
    (match r with
     | Error e -> Alcotest.failf "resize error: %a" Mem.pp_error e
     | Ok () -> ());
    Alcotest.(check int64) "n_pages unchanged" 4L (Mem.n_pages m);
    let rbuf = make_buf () in
    let* rr = Mem.read_page m ~page_id:0L rbuf in
    (match rr with
     | Error e -> Alcotest.failf "read error: %a" Mem.pp_error e
     | Ok () ->
       Alcotest.(check char) "data preserved" 'Q' (Cstruct.get_char rbuf 0));
    Lwt.return_unit
  )

let resize_to_zero_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:4L in
    let* r = Mem.resize m ~n_pages:0L in
    (match r with
     | Error e -> Alcotest.failf "resize error: %a" Mem.pp_error e
     | Ok () -> ());
    Alcotest.(check int64) "n_pages = 0" 0L (Mem.n_pages m);
    let buf = make_buf () in
    let* rr = Mem.read_page m ~page_id:0L buf in
    (match rr with
     | Ok () -> Alcotest.fail "read: expected OOB after resize to 0"
     | Error (Mem.Out_of_bounds _) -> ());
    let* wr = Mem.write_page m ~page_id:0L buf in
    (match wr with
     | Ok () -> Alcotest.fail "write: expected OOB after resize to 0"
     | Error (Mem.Out_of_bounds _) -> ());
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* 4. SYNC                                                             *)
(* ------------------------------------------------------------------ *)

let sync_always_ok_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:4L in
    let* r = Mem.sync m in
    (match r with
     | Error e -> Alcotest.failf "sync error: %a" Mem.pp_error e
     | Ok () -> ());
    Lwt.return_unit
  )

let sync_after_write_test () =
  Lwt_main.run (
    let m = Mem.create ~n_pages:2L in
    let wbuf = make_buf () in
    Cstruct.set_char wbuf 0 'Y';
    let* _ = Mem.write_page m ~page_id:0L wbuf in
    let* sr = Mem.sync m in
    (match sr with
     | Error e -> Alcotest.failf "sync error: %a" Mem.pp_error e
     | Ok () -> ());
    let rbuf = make_buf () in
    let* rr = Mem.read_page m ~page_id:0L rbuf in
    (match rr with
     | Error e -> Alcotest.failf "read error: %a" Mem.pp_error e
     | Ok () ->
       Alcotest.(check char) "data intact after sync" 'Y' (Cstruct.get_char rbuf 0));
    Lwt.return_unit
  )

(* ------------------------------------------------------------------ *)
(* 5. PP_ERROR FORMATTING                                              *)
(* ------------------------------------------------------------------ *)

let pp_error_format_test () =
  let err = Mem.Out_of_bounds { page_id = 42L; n_pages = 10L } in
  let s = Format.asprintf "%a" Mem.pp_error err in
  Alcotest.(check bool) "contains page_id"
    true (string_contains s "42");
  Alcotest.(check bool) "contains n_pages"
    true (string_contains s "10")

(* ------------------------------------------------------------------ *)
(* 6. QCHECK PROPERTY TESTS                                            *)
(* ------------------------------------------------------------------ *)

let prop_write_read_roundtrip =
  let gen =
    QCheck.Gen.(
      let* page_id = int_range 0 9 in
      let* payload = bytes_size (return 4096) in
      return (page_id, payload)
    )
  in
  QCheck.Test.make
    ~name:"prop_write_read_roundtrip"
    ~count:200
    (QCheck.make gen)
    (fun (page_id, payload) ->
       let m = Mem.create ~n_pages:10L in
       let wbuf = buf_of_bytes payload in
       let ok =
         Lwt_main.run (
           let* wr = Mem.write_page m ~page_id:(Int64.of_int page_id) wbuf in
           match wr with
           | Error _ -> Lwt.return false
           | Ok () ->
             let rbuf = make_buf () in
             let* rr = Mem.read_page m ~page_id:(Int64.of_int page_id) rbuf in
             match rr with
             | Error _ -> Lwt.return false
             | Ok () ->
               let got = Cstruct.to_bytes rbuf in
               Lwt.return (got = payload)
         )
       in
       ok)

let prop_oob_always_errors =
  (* Generate page_id outside [0, 3] by picking either a negative or >= 4 *)
  let gen =
    QCheck.Gen.(
      oneof [
        map (fun n -> Int64.of_int (-(n + 1))) (int_range 0 1000);
        map (fun n -> Int64.of_int (n + 4)) (int_range 0 1000);
      ]
    )
  in
  QCheck.Test.make
    ~name:"prop_oob_always_errors"
    ~count:500
    (QCheck.make gen)
    (fun page_id ->
       let m = Mem.create ~n_pages:4L in
       Lwt_main.run (
         let buf = make_buf () in
         let* rr = Mem.read_page m ~page_id buf in
         let* wr = Mem.write_page m ~page_id buf in
         let r_err = match rr with Error _ -> true | Ok () -> false in
         let w_err = match wr with Error _ -> true | Ok () -> false in
         Lwt.return (r_err && w_err)
       ))

let prop_resize_n_pages_correct =
  QCheck.Test.make
    ~name:"prop_resize_n_pages_correct"
    ~count:1000
    (QCheck.make (QCheck.Gen.int_range 0 20))
    (fun n ->
       let m = Mem.create ~n_pages:1L in
       Lwt_main.run (
         let* r = Mem.resize m ~n_pages:(Int64.of_int n) in
         match r with
         | Error _ -> Lwt.return false
         | Ok () ->
           Lwt.return (Mem.n_pages m = Int64.of_int n)
       ))

let prop_fresh_pages_are_zero =
  let gen =
    QCheck.Gen.(
      let* size = int_range 1 16 in
      let* page_id = int_range 0 (size - 1) in
      return (size, page_id)
    )
  in
  QCheck.Test.make
    ~name:"prop_fresh_pages_are_zero"
    ~count:500
    (QCheck.make gen)
    (fun (size, page_id) ->
       let m = Mem.create ~n_pages:(Int64.of_int size) in
       Lwt_main.run (
         let buf = make_buf () in
         let* rr = Mem.read_page m ~page_id:(Int64.of_int page_id) buf in
         match rr with
         | Error _ -> Lwt.return false
         | Ok () ->
           Lwt.return (Cstruct.for_all (fun c -> c = '\x00') buf)
       ))

(* ------------------------------------------------------------------ *)
(* RUNNER                                                              *)
(* ------------------------------------------------------------------ *)

let () =
  let qcheck_tests =
    List.map QCheck_alcotest.to_alcotest [
      prop_write_read_roundtrip;
      prop_oob_always_errors;
      prop_resize_n_pages_correct;
      prop_fresh_pages_are_zero;
    ]
  in
  Alcotest.run "mem" [
    "basic", [
      Alcotest.test_case "fresh page reads zero"        `Quick read_zeros_test;
      Alcotest.test_case "write_then_read"              `Quick write_then_read_test;
      Alcotest.test_case "page_isolation"               `Quick page_isolation_test;
      Alcotest.test_case "full_page_write"              `Quick full_page_write_test;
      Alcotest.test_case "overwrite"                    `Quick overwrite_test;
      Alcotest.test_case "write_last_valid_page"        `Quick write_last_valid_page_test;
      Alcotest.test_case "write_page_0"                 `Quick write_page_0_test;
    ];
    "oob", [
      Alcotest.test_case "read_oob_exact"               `Quick read_oob_exact_test;
      Alcotest.test_case "read_oob_high"                `Quick read_oob_high_test;
      Alcotest.test_case "read_oob_negative"            `Quick read_oob_negative_test;
      Alcotest.test_case "write_oob_exact"              `Quick write_oob_exact_test;
      Alcotest.test_case "write_oob_negative"           `Quick write_oob_negative_test;
      Alcotest.test_case "oob_error_fields"             `Quick oob_error_fields_test;
      Alcotest.test_case "empty_backend"                `Quick empty_backend_test;
    ];
    "resize", [
      Alcotest.test_case "resize_grow"                  `Quick resize_grow_test;
      Alcotest.test_case "resize_shrink"                `Quick resize_shrink_test;
      Alcotest.test_case "resize_grow_preserves_data"   `Quick resize_grow_preserves_data_test;
      Alcotest.test_case "resize_shrink_then_grow"      `Quick resize_shrink_then_grow_test;
      Alcotest.test_case "resize_to_same_size"          `Quick resize_to_same_size_test;
      Alcotest.test_case "resize_to_zero"               `Quick resize_to_zero_test;
    ];
    "sync", [
      Alcotest.test_case "sync_always_ok"               `Quick sync_always_ok_test;
      Alcotest.test_case "sync_after_write"             `Quick sync_after_write_test;
    ];
    "pp_error", [
      Alcotest.test_case "pp_error_format"              `Quick pp_error_format_test;
    ];
    "qcheck", qcheck_tests;
  ]
