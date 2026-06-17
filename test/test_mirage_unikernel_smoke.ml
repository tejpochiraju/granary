(** Host smoke test for the sample MirageOS unikernel wiring (#403).

    Opens a {!Sqlocaml_store.Store} in WAL mode with the main DB on a real
    [mirage-block-unix] device (wrapped by {!Sqlocaml_mirage_block.Mirage_backend})
    and an in-memory, byte-addressed WAL buffer — the exact shape the sample
    unikernel uses — then runs the shared {!Sqlocaml_sample.Sample.run_demo}
    workload and asserts the WAL fsync / commit path fired.  Runs in the plain
    [sqlocaml-dev] image (no mirage CLI / solo5 needed), so CI guards the
    Mirage_backend <-> Store wiring against bit-rot. *)

open Lwt.Syntax
module MB = Sqlocaml_mirage_block.Mirage_backend.Make (Block)
module Store = Sqlocaml_store.Store
module Db = Sqlocaml.Db

(* ------------------------------------------------------------------ *)
(* In-memory, byte-addressed WAL device                                *)
(*                                                                     *)
(* The WAL needs positioned byte I/O (offset = header + idx*frame_size,*)
(* not sector-aligned), so it cannot ride a page-addressed Mirage_block*)
(* device directly.  The sample keeps it in memory; see mirage/README. *)
(* ------------------------------------------------------------------ *)

type wal_dev = { mutable buf : Bytes.t }

let wal_grow d need =
  let cur = Bytes.length d.buf in
  if need > cur
  then (
    let nb = Bytes.make (max need (cur * 2)) '\x00' in
    Bytes.blit d.buf 0 nb 0 cur;
    d.buf <- nb)
;;

let wal_read_at d ~offset out =
  let off = Int64.to_int offset in
  let len = Cstruct.length out in
  (* Reading past the written region yields zeros (the WAL grows lazily). *)
  wal_grow d (off + len);
  Cstruct.blit_from_bytes d.buf off out 0 len;
  Lwt.return (Ok ())
;;

let wal_write_at d ~offset src =
  let off = Int64.to_int offset in
  let len = Cstruct.length src in
  wal_grow d (off + len);
  let tmp = Bytes.create len in
  Cstruct.blit_to_bytes src 0 tmp 0 len;
  Bytes.blit tmp 0 d.buf off len;
  Lwt.return (Ok ())
;;

let wal_sync () = Lwt.return (Ok ())

(* A 4 MiB zero-filled file = 1024 pages of 4096 bytes; ample for the demo. *)
let tmp_file () =
  let path = Filename.temp_file "sqlocaml_uni_smoke" ".raw" in
  let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  Unix.ftruncate fd (4 * 1024 * 1024);
  Unix.close fd;
  path
;;

let test_smoke () =
  let path = tmp_file () in
  Fun.protect
    ~finally:(fun () -> Unix.unlink path)
    (fun () ->
       Lwt_main.run
         (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
          let* adapter = MB.connect dev in
          let wal = { buf = Bytes.make 65536 '\x00' } in
          let* sr =
            Store.open_block_wal
              ~read_page:(MB.read_page adapter)
              ~write_page:(MB.write_page adapter)
              ~sync:(MB.sync adapter)
              ~resize:(MB.resize adapter)
              ~n_pages:0L
              ~wal_read_at:(wal_read_at wal)
              ~wal_write_at:(wal_write_at wal)
              ~wal_sync
              ~wal_size_bytes:(Int64.of_int (Bytes.length wal.buf))
              ~close:(fun () -> MB.close adapter)
              ~wal_close:(fun () -> Lwt.return_unit)
              ()
          in
          match sr with
          | Error e -> Alcotest.failf "open_block_wal: %a" Store.pp_error e
          | Ok store ->
            let* db = Db.of_store store in
            let* r = Sqlocaml_sample.Sample.run_demo db in
            (match r with
             | Error e -> Alcotest.failf "run_demo: %a" Db.pp_error e
             | Ok n ->
               Alcotest.(check int) "rows read back through WAL store" 3 n;
               Alcotest.(check bool)
                 "WAL fsync/commit path fired"
                 true
                 (Db.wal_sync_count db > 0);
               Db.close db)))
;;

let () =
  let open Alcotest in
  run
    "mirage_unikernel_smoke"
    [ "wiring", [ test_case "open_block_wal + commit + read-back" `Quick test_smoke ] ]
;;
