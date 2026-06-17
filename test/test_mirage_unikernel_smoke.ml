(** Host smoke test for the sample MirageOS unikernel wiring (#403).

    Opens a {!Sqlocaml_store.Store} in WAL mode with the main DB on a real
    [mirage-block-unix] device (wrapped by {!Sqlocaml_mirage_block.Mirage_backend})
    and the shared in-memory {!Sqlocaml_sample.Mem_wal} WAL buffer — the exact
    shape the sample unikernel uses — then runs the shared
    {!Sqlocaml_sample.Sample.run_demo} workload and asserts the WAL fsync /
    commit path fired. Runs in the plain [sqlocaml-dev] image (no mirage CLI /
    solo5 needed), so CI guards the Mirage_backend <-> Store wiring against
    bit-rot. *)

open Lwt.Syntax
module MB = Sqlocaml_mirage_block.Mirage_backend.Make (Block)
module Store = Sqlocaml_store.Store
module Db = Sqlocaml.Db
module Mem_wal = Sqlocaml_sample.Mem_wal

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
          let wal = Mem_wal.create () in
          let* sr =
            Store.open_block_wal
              ~read_page:(MB.read_page adapter)
              ~write_page:(MB.write_page adapter)
              ~sync:(MB.sync adapter)
              ~resize:(MB.resize adapter)
              ~n_pages:0L
              ~wal_read_at:(Mem_wal.read_at wal)
              ~wal_write_at:(Mem_wal.write_at wal)
              ~wal_sync:Mem_wal.sync
              ~wal_size_bytes:(Mem_wal.size_bytes wal)
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
               (* The demo commits exactly twice — the implicit auto-commit of
                  the CREATE TABLE DDL, then the explicit BEGIN/COMMIT — and
                  synchronous=full fsyncs each. An exact count guards against a
                  regression that double-syncs or skips the commit fsync. *)
               Alcotest.(check int)
                 "WAL fsync/commit path fired exactly twice"
                 2
                 (Db.wal_sync_count db);
               Db.close db)))
;;

let () =
  let open Alcotest in
  run
    "mirage_unikernel_smoke"
    [ "wiring", [ test_case "open_block_wal + commit + read-back" `Quick test_smoke ] ]
;;
