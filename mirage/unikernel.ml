(* Sample granary MirageOS unikernel (#403).

   Opens a granary [Store] in WAL mode over the supplied [Mirage_block.S]
   device (wrapped by {!Granary_mirage_block.Mirage_backend}), wraps it as a
   [Db.t], and runs the shared {!Granary_sample.Sample.run_demo} workload —
   CREATE TABLE, INSERTs inside an explicit transaction, COMMIT, read-back.

   This is the amd64 baseline #402 then cross-builds on aarch64.  The engine is
   100% OCaml with an explicitly byte-ordered on-disk format, so the only
   arch-relevant code is the WAL fsync / commit path, which the explicit
   transaction here exercises end-to-end.

   {b Durability (#772) — behaviour break.}  [Mirage_block.S] has no flush or
   barrier operation, so the adapter cannot make a write durable.  It used to
   report every commit durable anyway; since #772 it declares
   [`Unavailable] and the store refuses any durability level above [Off].  This
   sample therefore opens with [~durability:Store.Off] EXPLICITLY: the demo is a
   wiring/portability check, not a durability demonstration, and [Off] is the
   only truthful description of what a stock Solo5 or Mirage_block device
   provides today.  Nothing this unikernel writes survives a power loss.  To get
   real durability here, supply [Mirage_backend.connect]'s [~barrier] with a
   platform flush (see mirage/README.md) and drop the [~durability] argument.

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
    (* #772: declare the device's (absent) durability capability and pick the
       only level it can honour.  Both arguments are deliberate and neither has
       a safe default: drop [~barrier] and the store believes a barrier exists;
       drop [~durability] and the open is refused, which is the point. *)
    let barrier = MB.durability_barrier adapter in
    (match barrier with
     | `Available -> ()
     | `Unavailable reason ->
       Log.warn (fun f ->
         f
           "durability: opening with synchronous=off -- this device has no write \
            barrier, so committed data does NOT survive a power loss (%s)"
           reason));
    let* sr =
      Store.open_block_wal
        ~barrier
        ~durability:Store.Off
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
             "granary demo OK: read back %d rows; WAL fsyncs=%d (commit path \
              exercised, durability=%s)"
             n
             (Db.wal_sync_count db)
             (Store.string_of_durability (Store.durability store)))
       | Error e -> Log.err (fun f -> f "demo workload failed: %a" Db.pp_error e));
      Db.close db
  ;;
end

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-8"]
[@@@ai_provider "Anthropic"]
