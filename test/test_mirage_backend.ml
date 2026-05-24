open Lwt.Syntax
module MB = Sqlocaml_mirage_block.Mirage_backend.Make (Block)

let tmp_file () =
  let path = Filename.temp_file "sqlocaml_mb_test" ".raw" in
  let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  Unix.ftruncate fd (1024 * 1024);
  Unix.close fd;
  path
;;

let test_connect_fresh () =
  let path = tmp_file () in
  Fun.protect
    ~finally:(fun () -> Unix.unlink path)
    (fun () ->
       Lwt_main.run
         (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
          let* adapter = MB.connect dev in
          let n = MB.n_pages adapter in
          Alcotest.(check int64) "n_pages starts at 0" 0L n;
          MB.close adapter))
;;

let test_read_write_roundtrip () =
  let path = tmp_file () in
  Fun.protect
    ~finally:(fun () -> Unix.unlink path)
    (fun () ->
       Lwt_main.run
         (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
          let* adapter = MB.connect dev in
          let buf_w = Cstruct.create 4096 in
          Cstruct.set_uint8 buf_w 0 0xAB;
          Cstruct.set_uint8 buf_w 4095 0xCD;
          let* wr = MB.write_page adapter ~page_id:0L buf_w in
          Alcotest.(check (result unit string)) "write ok" (Ok ()) wr;
          let buf_r = Cstruct.create 4096 in
          let* rr = MB.read_page adapter ~page_id:0L buf_r in
          Alcotest.(check (result unit string)) "read ok" (Ok ()) rr;
          Alcotest.(check int) "byte 0" 0xAB (Cstruct.get_uint8 buf_r 0);
          Alcotest.(check int) "byte 4095" 0xCD (Cstruct.get_uint8 buf_r 4095);
          MB.close adapter))
;;

let test_out_of_capacity () =
  let path = tmp_file () in
  Fun.protect
    ~finally:(fun () -> Unix.unlink path)
    (fun () ->
       Lwt_main.run
         (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
          let* adapter = MB.connect dev in
          (* 1 MB / 4096 = 256 pages; page 256 is out of bounds *)
          let buf = Cstruct.create 4096 in
          let* rr = MB.read_page adapter ~page_id:256L buf in
          Alcotest.(check bool) "read OOB is error" true (Result.is_error rr);
          let* wr = MB.write_page adapter ~page_id:256L buf in
          Alcotest.(check bool) "write OOB is error" true (Result.is_error wr);
          MB.close adapter))
;;

let test_resize_within_capacity () =
  let path = tmp_file () in
  Fun.protect
    ~finally:(fun () -> Unix.unlink path)
    (fun () ->
       Lwt_main.run
         (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
          let* adapter = MB.connect dev in
          let* rr = MB.resize adapter ~n_pages:10L in
          Alcotest.(check (result unit string)) "resize within cap" (Ok ()) rr;
          Alcotest.(check int64) "n_pages updated" 10L (MB.n_pages adapter);
          MB.close adapter))
;;

let test_resize_beyond_capacity () =
  let path = tmp_file () in
  Fun.protect
    ~finally:(fun () -> Unix.unlink path)
    (fun () ->
       Lwt_main.run
         (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
          let* adapter = MB.connect dev in
          (* 1 MB = 256 pages; requesting 300 fails *)
          let* rr = MB.resize adapter ~n_pages:300L in
          Alcotest.(check bool) "resize beyond cap is error" true (Result.is_error rr);
          MB.close adapter))
;;

let test_sync_always_ok () =
  let path = tmp_file () in
  Fun.protect
    ~finally:(fun () -> Unix.unlink path)
    (fun () ->
       Lwt_main.run
         (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
          let* adapter = MB.connect dev in
          let* sr = MB.sync adapter () in
          Alcotest.(check (result unit string)) "sync ok" (Ok ()) sr;
          MB.close adapter))
;;

let () =
  let open Alcotest in
  run
    "mirage_backend"
    [ ( "adapter"
      , [ test_case "connect_fresh" `Quick test_connect_fresh
        ; test_case "read_write_roundtrip" `Quick test_read_write_roundtrip
        ; test_case "out_of_capacity" `Quick test_out_of_capacity
        ; test_case "resize_within_capacity" `Quick test_resize_within_capacity
        ; test_case "resize_beyond_capacity" `Quick test_resize_beyond_capacity
        ; test_case "sync_always_ok" `Quick test_sync_always_ok
        ] )
    ]
;;
