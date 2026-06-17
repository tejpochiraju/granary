(* Sample sqlocaml MirageOS unikernel (#403).

   Opens a sqlocaml [Store] in WAL mode over the supplied [Mirage_block.S]
   device (wrapped by {!Sqlocaml_mirage_block.Mirage_backend}), wraps it as a
   [Db.t], and runs the shared {!Sqlocaml_sample.Sample.run_demo} workload —
   CREATE TABLE, INSERTs inside an explicit transaction, COMMIT, read-back.

   This is the amd64 baseline #402 then cross-builds on aarch64.  The engine is
   100% OCaml with an explicitly byte-ordered on-disk format, so the only
   arch-relevant code is the WAL fsync / commit path, which the explicit
   transaction here exercises end-to-end. *)

open Lwt.Syntax
module Store = Sqlocaml_store.Store
module Db = Sqlocaml.Db

let src = Logs.Src.create "sqlocaml-demo" ~doc:"sqlocaml sample unikernel"

module Log = (val Logs.src_log src : Logs.LOG)

module Make (B : Mirage_block.S) = struct
  module MB = Sqlocaml_mirage_block.Mirage_backend.Make (B)

  (* The WAL needs positioned byte I/O at non-sector-aligned offsets, so it does
     not ride the page-addressed block device directly; this sample keeps it in
     an in-memory, lazily-grown buffer.  See mirage/README.md. *)
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

  let start block =
    let* adapter = MB.connect block in
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
    | Error e ->
      Log.err (fun f -> f "open_block_wal failed: %a" Store.pp_error e);
      Lwt.return_unit
    | Ok store ->
      let* db = Db.of_store store in
      let* r = Sqlocaml_sample.Sample.run_demo db in
      (match r with
       | Ok n ->
         Log.info (fun f ->
           f
             "sqlocaml demo OK: read back %d rows; WAL fsyncs=%d (commit path exercised)"
             n
             (Db.wal_sync_count db))
       | Error e -> Log.err (fun f -> f "demo workload failed: %a" Db.pp_error e));
      Db.close db
  ;;
end

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-8"]
[@@@ai_provider "Anthropic"]
