(* Sample granary MirageOS unikernel (#403).

   Opens a granary [Store] in WAL mode over the supplied [Mirage_block.S]
   device (wrapped by {!Granary_mirage_block.Mirage_backend}), wraps it as a
   [Db.t], and runs the shared {!Granary_sample.Sample.run_demo} workload —
   CREATE TABLE, INSERTs inside an explicit transaction, COMMIT, read-back.

   This is the amd64 baseline #402 then cross-builds on aarch64.  The engine is
   100% OCaml with an explicitly byte-ordered on-disk format, so the only
   arch-relevant code is the WAL fsync / commit path, which the explicit
   transaction here exercises end-to-end.

   The main DB lives on the block device; the WAL uses the shared in-memory
   {!Granary_sample.Mem_wal} buffer (also driven by the host smoke test, so the
   two cannot drift). See mirage/README.md. *)

open Lwt.Syntax
module Store = Granary_store.Store
module Db = Granary.Db
module Mem_wal = Granary_sample.Mem_wal

let src = Logs.Src.create "granary-demo" ~doc:"granary sample unikernel"

module Log = (val Logs.src_log src : Logs.LOG)

module Make (B : Mirage_block.S) = struct
  module MB = Granary_mirage_block.Mirage_backend.Make (B)

  let start block =
    let* adapter = MB.connect block in
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
    | Error e ->
      Log.err (fun f -> f "open_block_wal failed: %a" Store.pp_error e);
      Lwt.return_unit
    | Ok store ->
      let* db = Db.of_store store in
      let* r = Granary_sample.Sample.run_demo db in
      (match r with
       | Ok n ->
         Log.info (fun f ->
           f
             "granary demo OK: read back %d rows; WAL fsyncs=%d (commit path exercised)"
             n
             (Db.wal_sync_count db))
       | Error e -> Log.err (fun f -> f "demo workload failed: %a" Db.pp_error e));
      Db.close db
  ;;
end

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-8"]
[@@@ai_provider "Anthropic"]
