(** Host smoke test for the sample MirageOS unikernel wiring (#403, #772).

    Opens a {!Granary_store.Store} in WAL mode with the main DB on a real
    [mirage-block-unix] device (wrapped by {!Granary_mirage_block.Mirage_backend})
    and the shared in-memory {!Granary_sample.Mem_wal} WAL buffer — the exact
    shape the sample unikernel uses — then runs the shared
    {!Granary_sample.Sample.run_demo} workload. Runs in the plain [granary-dev]
    image (no mirage CLI / solo5 needed), so CI guards the Mirage_backend <->
    Store wiring against bit-rot.

    Two cases since #772, because the wiring now has two legitimate shapes:

    - {b unikernel shape} — no barrier, so [~durability:Off]. This is what
      [mirage/unikernel.ml] does verbatim, and it is the shape a stock Solo5 or
      [Mirage_block] device can actually deliver. No commit fsync happens,
      because there is nothing to fsync.
    - {b barrier shape} — the platform supplies a real [fsync] through
      [Mirage_backend.connect]'s [~barrier], so the default [full] durability is
      accepted and every group-commit syncs. This is the seam a barrier-capable
      Solo5 backend would fill, and the case that still pins the exact
      commit-fsync count. *)

open Lwt.Syntax
module MB = Granary_mirage_block.Mirage_backend.Make (Block)
module Store = Granary_store.Store
module Db = Granary.Db
module Mem_wal = Granary_sample.Mem_wal

(* #772: [Mirage_block.S] has no flush operation, so [Mirage_backend] declares
   no barrier unless the platform supplies one -- and a store told it has no
   barrier refuses every durability level above [off].  A test that wants the
   ordinary [full] durability over a real file hands the adapter a genuine
   [fsync].  That is also precisely the seam a barrier-capable Solo5 block
   backend would fill.  [fsync] is per-inode, so a second descriptor on the same
   path flushes the writes [Block] issued through its own. *)
let fsync_barrier path () : (unit, string) result Lwt.t =
  let fd = Unix.openfile path [ Unix.O_WRONLY ] 0o644 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> Unix.fsync fd);
  Lwt.return (Ok ())
;;

(* A 4 MiB zero-filled file = 1024 pages of 4096 bytes; ample for the demo. *)
let tmp_file () =
  let path = Filename.temp_file "granary_uni_smoke" ".raw" in
  let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  Unix.ftruncate fd (4 * 1024 * 1024);
  Unix.close fd;
  path
;;

(* Shared wiring for both cases.  [barrier] and [durability] are the only
   difference, which is the point: everything else about the unikernel's
   Mirage_backend <-> Store.open_block_wal wiring is identical. *)
let with_unikernel_store path ~barrier ~durability f =
  let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
  let* adapter = MB.connect ?barrier dev in
  let wal = Mem_wal.create () in
  let* sr =
    Store.open_block_wal
      ~barrier:(MB.durability_barrier adapter)
      ~durability
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
  | Ok store -> f store
;;

(* The unikernel's own shape: no platform flush, so [Off] and no commit fsync. *)
let test_unikernel_shape () =
  let path = tmp_file () in
  Fun.protect
    ~finally:(fun () -> Unix.unlink path)
    (fun () ->
       Lwt_main.run
         (with_unikernel_store path ~barrier:None ~durability:Store.Off
          @@ fun store ->
          (match Store.barrier store with
           | `Unavailable _ -> ()
           | `Available ->
             Alcotest.fail "a bare Mirage_block device must not claim a barrier");
          Alcotest.(check string)
            "durability stays off"
            "off"
            (Store.string_of_durability (Store.durability store));
          let* db = Db.of_store store in
          let* r = Granary_sample.Sample.run_demo db in
          match r with
          | Error e -> Alcotest.failf "run_demo: %a" Db.pp_error e
          | Ok n ->
            Alcotest.(check int) "rows read back through WAL store" 3 n;
            (* #772: under [off] no commit fsyncs, and the demo's two commits
               are far below the 1000-frame autocheckpoint threshold, so no
               checkpoint anchor fires either.  Before #772 this configuration
               reported two fsyncs that never reached the device. *)
            Alcotest.(check int)
              "no commit fsync is issued under synchronous=off"
              0
              (Db.wal_sync_count db);
            Db.close db))
;;

(* The barrier shape: a platform [fsync] is supplied, so [full] is accepted. *)
let test_barrier_shape () =
  let path = tmp_file () in
  Fun.protect
    ~finally:(fun () -> Unix.unlink path)
    (fun () ->
       Lwt_main.run
         (with_unikernel_store
            path
            ~barrier:(Some (fsync_barrier path))
            ~durability:Store.Full
          @@ fun store ->
          (match Store.barrier store with
           | `Available -> ()
           | `Unavailable r ->
             Alcotest.failf "a supplied ~barrier must be reported available: %s" r);
          let* db = Db.of_store store in
          let* r = Granary_sample.Sample.run_demo db in
          match r with
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
            Db.close db))
;;

let () =
  let open Alcotest in
  run
    "mirage_unikernel_smoke"
    [ ( "wiring"
      , [ test_case "unikernel shape: no barrier, synchronous=off" `Quick
            test_unikernel_shape
        ; test_case "barrier shape: platform fsync, synchronous=full" `Quick
            test_barrier_shape
        ] )
    ]
;;
