open Lwt.Syntax
module MB = Granary_mirage_block.Mirage_backend.Make (Block)

let tmp_file () =
  let path = Filename.temp_file "granary_mb_test" ".raw" in
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

(* #772: this test used to be [test_sync_always_ok], asserting [Ok ()].  That
   [Ok] was the bug: [Mirage_block.S] has no flush operation, so the adapter had
   nothing to call and reported success anyway, which made every commit look
   durable under the default [synchronous = full] while the bytes were still in
   the host page cache.  With no [~barrier] the adapter now refuses. *)
let test_sync_refuses_without_barrier () =
  let path = tmp_file () in
  Fun.protect
    ~finally:(fun () -> Unix.unlink path)
    (fun () ->
       Lwt_main.run
         (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
          let* adapter = MB.connect dev in
          let* sr = MB.sync adapter () in
          Alcotest.(check (result unit string))
            "sync reports the missing barrier instead of success"
            (Error Granary_mirage_block.Mirage_backend.no_barrier_reason)
            sr;
          MB.close adapter))
;;

(* #772: the seam.  A platform that CAN flush supplies it here, and then the
   adapter reports a barrier and [sync] is that function. *)
let test_sync_uses_supplied_barrier () =
  let path = tmp_file () in
  Fun.protect
    ~finally:(fun () -> Unix.unlink path)
    (fun () ->
       Lwt_main.run
         (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
          let calls = ref 0 in
          let* adapter =
            MB.connect
              ~barrier:(fun () ->
                incr calls;
                Lwt.return (Ok ()))
              dev
          in
          Alcotest.(check bool)
            "a supplied barrier is reported available"
            true
            (match MB.durability_barrier adapter with
             | `Available -> true
             | `Unavailable _ -> false);
          let* sr = MB.sync adapter () in
          Alcotest.(check (result unit string)) "sync ok" (Ok ()) sr;
          Alcotest.(check int) "the supplied barrier ran" 1 !calls;
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
        ; test_case "sync_refuses_without_barrier" `Quick
            test_sync_refuses_without_barrier
        ; test_case "sync_uses_supplied_barrier" `Quick
            test_sync_uses_supplied_barrier
        ] )
    ]
;;
