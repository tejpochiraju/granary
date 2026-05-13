open Lwt.Syntax

let read_zeros_test () =
  Lwt_main.run (
    let m = Sqlocaml_block.Mem.create ~n_pages:4L in
    let buf = Cstruct.create Sqlocaml_block.Mem.page_size in
    let* r = Sqlocaml_block.Mem.read_page m ~page_id:0L buf in
    match r with
    | Error e ->
      Alcotest.failf "unexpected error: %a" Sqlocaml_block.Mem.pp_error e
    | Ok () ->
      let all_zero = Cstruct.for_all (fun c -> c = '\x00') buf in
      Alcotest.(check bool) "fresh page is zero" true all_zero;
      Lwt.return_unit
  )

let () =
  Alcotest.run "mem" [
    "basic", [
      Alcotest.test_case "fresh page reads zero" `Quick read_zeros_test
    ]
  ]
