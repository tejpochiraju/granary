(** #772 — a backend with no write barrier must refuse the durability it cannot
    honour, instead of acking every commit durable.

    [lib/block/mirage_backend.ml]'s [sync] used to be [Lwt.return (Ok ())].
    [Mirage_block.S] has exactly four operations — [get_info], [read], [write],
    [disconnect] — and no flush of any kind, so there was nothing for it to
    call; but it reported success anyway, which under the default
    [PRAGMA synchronous = full] made every commit look durable while the bytes
    were still in the host's page cache. Crash recovery had nothing to recover
    to.

    What this pins:

    1. [Mirage_backend.sync] with no [~barrier] returns [Error], never [Ok].
    2. [Mirage_backend.connect]'s [~barrier] — the seam a barrier-capable
       platform fills — is used when supplied, in both directions.
    3. [Store.open_block] / [open_block_wal] refuse [Full] {e and} [Batched] on
       a barrier-less backend, with a message naming the backend and the missing
       capability.
    4. [Off] — the only truthful level on such a device — opens, and a store
       opened that way actually works (commit, checkpoint and close must not
       trip over the adapter's [Error]).
    5. [Store.set_durability] and [PRAGMA synchronous = full|batched] keep
       refusing after open; [= off] is accepted and reads back.
    6. A replication commit-sink, which pins [Full], is refused.
    7. [Unix_file] is completely unaffected: it has a real [fsync], reports
       [`Available], and every durability level still works.
    8. (#785) The declared barrier governs the WAL device's [wal_sync] as well
       as the main-DB [sync]: with none, the store stops calling it, so the
       [Off] escape hatch actually opens, checkpoints and CLOSES on a WAL that
       lives on the same barrier-less device; with one declared, a failing
       [wal_sync] still surfaces. The same section pins the residual on the
       other side of the seam -- [Store.open_block]'s [?barrier] default. *)

open Lwt.Syntax
module MB = Granary_mirage_block.Mirage_backend.Make (Block)
module Store = Granary_store.Store
module Db = Granary.Db
module Mem_wal = Granary_sample.Mem_wal

let run = Lwt_main.run

(* Seeds Mirage_crypto_rng and installs the file provider (#613); the raw
   [Granary_unix.Store] constructors do not do it themselves. *)
let () = Granary_unix.install ()

(* A 4 MiB zero-filled file = 1024 pages of 4096 bytes. *)
let tmp_file () =
  let path = Filename.temp_file "granary_772" ".raw" in
  let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
  Unix.ftruncate fd (4 * 1024 * 1024);
  Unix.close fd;
  path
;;

let with_tmp f =
  let path = tmp_file () in
  Fun.protect ~finally:(fun () -> Unix.unlink path) (fun () -> f path)
;;

(* The seam: a platform-supplied flush.  On Unix that is [fsync]; on a
   flush-aware Solo5 it would be the block-flush hypercall stub. *)
let fsync_barrier path () : (unit, string) result Lwt.t =
  let fd = Unix.openfile path [ Unix.O_WRONLY ] 0o644 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> Unix.fsync fd);
  Lwt.return (Ok ())
;;

let contains ~needle s =
  let nl = String.length needle
  and sl = String.length s in
  let rec go i = i + nl <= sl && (String.sub s i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let check_contains what ~needle s =
  if not (contains ~needle s)
  then Alcotest.failf "%s: expected the message to mention %S, got: %s" what needle s
;;

(* ------------------------------------------------------------------ *)
(* 1 + 2: the adapter itself                                            *)
(* ------------------------------------------------------------------ *)

let test_sync_refuses_without_barrier () =
  with_tmp (fun path ->
    run
      (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
       let* adapter = MB.connect ~barrier:None dev in
       let* sr = MB.sync adapter () in
       (match sr with
        | Ok () ->
          Alcotest.fail
            "#772: a barrier-less Mirage_block adapter must not report sync success"
        | Error msg ->
          check_contains "sync error" ~needle:"no flush or barrier operation" msg;
          check_contains "sync error" ~needle:"Mirage_block.S" msg);
       (match MB.durability_barrier adapter with
        | `Available -> Alcotest.fail "durability_barrier must report `Unavailable"
        | `Unavailable r ->
          Alcotest.(check string)
            "the reason is the shared one"
            Granary_mirage_block.Mirage_backend.no_barrier_reason
            r);
       MB.close adapter))
;;

let test_sync_uses_supplied_barrier () =
  with_tmp (fun path ->
    run
      (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
       let calls = ref 0 in
       let counted_barrier () =
         incr calls;
         fsync_barrier path ()
       in
       let* adapter = MB.connect ~barrier:(Some counted_barrier) dev in
       (match MB.durability_barrier adapter with
        | `Available -> ()
        | `Unavailable r ->
          Alcotest.failf "a supplied ~barrier must report `Available, got: %s" r);
       let* sr = MB.sync adapter () in
       Alcotest.(check (result unit string)) "supplied barrier ran" (Ok ()) sr;
       Alcotest.(check int) "and was actually called" 1 !calls;
       MB.close adapter))
;;

(* A supplied barrier that fails must surface, not be swallowed. *)
let test_supplied_barrier_error_propagates () =
  with_tmp (fun path ->
    run
      (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
       let failing_barrier () = Lwt.return (Error "EIO from the platform") in
       let* adapter = MB.connect ~barrier:(Some failing_barrier) dev in
       let* sr = MB.sync adapter () in
       (match sr with
        | Ok () -> Alcotest.fail "a failing barrier must not be reported as success"
        | Error m -> Alcotest.(check string) "verbatim" "EIO from the platform" m);
       MB.close adapter))
;;

(* ------------------------------------------------------------------ *)
(* 3 + 4: the open-time refusal, and the escape hatch                   *)
(* ------------------------------------------------------------------ *)

(* On a refusal [Store.open_block] never wires [close] into a store record (there
   is no store), so the adapter is ours to release -- the same contract
   #753/#763 gave [Db.open_block]. *)
let open_mirage path ~durability =
  let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
  let* adapter = MB.connect ~barrier:None dev in
  let* r =
    Store.open_block
      ~barrier:(MB.durability_barrier adapter)
      ~durability
      ~init_if_corrupt:true
      ~read_page:(MB.read_page adapter)
      ~write_page:(MB.write_page adapter)
      ~sync:(MB.sync adapter)
      ~resize:(MB.resize adapter)
      ~n_pages:(MB.n_pages adapter)
      ~close:(fun () -> MB.close adapter)
      ()
  in
  match r with
  | Error _ ->
    let* () = MB.close adapter in
    Lwt.return r
  | Ok _ -> Lwt.return r
;;

let expect_refusal ~what result =
  match result with
  | Ok _ -> Alcotest.failf "%s: expected a refusal, got Ok" what
  | Error (Store.Durability_unavailable msg) ->
    (* The message must be specific enough to act on: which level was asked
       for, which backend cannot serve it, and the way out. *)
    check_contains what ~needle:"write barrier" msg;
    check_contains what ~needle:"Mirage_block.S" msg;
    check_contains what ~needle:"synchronous = off" msg;
    msg
  | Error e ->
    Alcotest.failf "%s: expected Durability_unavailable, got %a" what Store.pp_error e
;;

let test_full_is_refused () =
  with_tmp (fun path ->
    run
      (let* r = open_mirage path ~durability:Store.Full in
       let msg = expect_refusal ~what:"full" r in
       check_contains "full" ~needle:"'full'" msg;
       Lwt.return_unit))
;;

(* [Batched] is refused too, and that is the point: it still promises a barrier,
   just a deferred one.  Only [Off] promises nothing. *)
let test_batched_is_refused () =
  with_tmp (fun path ->
    run
      (let* r =
         open_mirage path ~durability:(Store.Batched { commits = 8; interval_ms = 50 })
       in
       let msg = expect_refusal ~what:"batched" r in
       check_contains "batched" ~needle:"'batched'" msg;
       Lwt.return_unit))
;;

(* The default path selects [Full], so an open that says nothing is refused. *)
let test_default_is_refused () =
  with_tmp (fun path ->
    run
      (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
       let* adapter = MB.connect ~barrier:None dev in
       let* r =
         Store.open_block
           ~barrier:(MB.durability_barrier adapter)
           ~init_if_corrupt:true
           ~read_page:(MB.read_page adapter)
           ~write_page:(MB.write_page adapter)
           ~sync:(MB.sync adapter)
           ~resize:(MB.resize adapter)
           ~n_pages:(MB.n_pages adapter)
           ~close:(fun () -> MB.close adapter)
           ()
       in
       ignore (expect_refusal ~what:"default" r : string);
       (* The refusal happens before any device work, so nothing was opened for
          us to close -- but the adapter's own device is still ours. *)
       MB.close adapter))
;;

(* [Off] is the escape hatch, and a store opened that way must actually WORK:
   the adapter's [sync] returns [Error], so a store that still reached for a
   barrier would fail at the first commit or checkpoint. *)
let test_off_opens_and_works () =
  with_tmp (fun path ->
    run
      (let* r = open_mirage path ~durability:Store.Off in
       match r with
       | Error e -> Alcotest.failf "off must open, got %a" Store.pp_error e
       | Ok store ->
         (match Store.barrier store with
          | `Available -> Alcotest.fail "barrier must still read back as `Unavailable"
          | `Unavailable _ -> ());
         Alcotest.(check string)
           "durability reads back as off"
           "off"
           (Store.string_of_durability (Store.durability store));
         let* db = Db.of_store store in
         let* er = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)" in
         (match er with
          | Error e -> Alcotest.failf "CREATE: %a" Db.pp_error e
          | Ok () -> ());
         let* er = Db.execute db "INSERT INTO t VALUES (1, 'a')" in
         (match er with
          | Error e -> Alcotest.failf "INSERT: %a" Db.pp_error e
          | Ok () -> ());
         let* qr = Db.query db "SELECT v FROM t WHERE id = 1" in
         let* () =
           match qr with
           | Error e -> Alcotest.failf "SELECT: %a" Db.pp_error e
           | Ok stream ->
             let* rows = Lwt_stream.to_list stream in
             Alcotest.(check int) "one row back" 1 (List.length rows);
             Lwt.return_unit
         in
         Db.close db))
;;

(* ------------------------------------------------------------------ *)
(* 5 + 6: refusals that outlive the open                                *)
(* ------------------------------------------------------------------ *)

let test_set_durability_refuses_after_open () =
  with_tmp (fun path ->
    run
      (let* r = open_mirage path ~durability:Store.Off in
       match r with
       | Error e -> Alcotest.failf "off must open, got %a" Store.pp_error e
       | Ok store ->
         (try
            Store.set_durability store Store.Full;
            Alcotest.fail "set_durability Full must raise on a barrier-less store"
          with
          | Failure m -> check_contains "set_durability" ~needle:"write barrier" m);
         (try
            Store.set_durability store (Store.Batched { commits = 4; interval_ms = 10 });
            Alcotest.fail "set_durability Batched must raise on a barrier-less store"
          with
          | Failure m -> check_contains "set_durability" ~needle:"write barrier" m);
         (* Off -> Off is always fine. *)
         Store.set_durability store Store.Off;
         Alcotest.(check string)
           "still off"
           "off"
           (Store.string_of_durability (Store.durability store));
         Store.close store))
;;

let test_pragma_synchronous_refuses () =
  with_tmp (fun path ->
    run
      (let* r = open_mirage path ~durability:Store.Off in
       match r with
       | Error e -> Alcotest.failf "off must open, got %a" Store.pp_error e
       | Ok store ->
         let* db = Db.of_store store in
         let expect_pragma_refused sql =
           let* er = Db.execute db sql in
           match er with
           | Ok () -> Alcotest.failf "%S must be refused on a barrier-less backend" sql
           | Error (Db.Runtime m) ->
             check_contains sql ~needle:"PRAGMA synchronous" m;
             check_contains sql ~needle:"write barrier" m;
             check_contains sql ~needle:"Mirage_block.S" m;
             Lwt.return_unit
           | Error e -> Alcotest.failf "%S: expected Runtime, got %a" sql Db.pp_error e
         in
         let* () = expect_pragma_refused "PRAGMA synchronous = full" in
         let* () = expect_pragma_refused "PRAGMA synchronous = batched" in
         (* ... and the honest level is accepted. *)
         let* er = Db.execute db "PRAGMA synchronous = off" in
         (match er with
          | Error e -> Alcotest.failf "PRAGMA synchronous = off: %a" Db.pp_error e
          | Ok () -> ());
         let* qr = Db.query db "PRAGMA synchronous" in
         let* () =
           match qr with
           | Error e -> Alcotest.failf "PRAGMA synchronous read: %a" Db.pp_error e
           | Ok stream ->
             let* rows = Lwt_stream.to_list stream in
             (match rows with
              | [ [| Db.V_text s |] ] -> Alcotest.(check string) "reads back off" "off" s
              | _ -> Alcotest.fail "PRAGMA synchronous returned an unexpected shape");
             Lwt.return_unit
         in
         Db.close db))
;;

let test_commit_sink_refused () =
  with_tmp (fun path ->
    run
      (let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
       let* adapter = MB.connect ~barrier:None dev in
       let wal = Mem_wal.create () in
       let* r =
         Store.open_block_wal
           ~barrier:(MB.durability_barrier adapter)
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
       match r with
       | Error e -> Alcotest.failf "off + WAL must open, got %a" Store.pp_error e
       | Ok store ->
         let* raised =
           Lwt.catch
             (fun () ->
                let* () =
                  Store.set_commit_callback
                    store
                    (Some (fun ~epoch:_ ~base_idx:_ ~count:_ -> Lwt.return_unit))
                in
                Lwt.return_none)
             (fun exn -> Lwt.return_some (Printexc.to_string exn))
         in
         (match raised with
          | None ->
            Alcotest.fail
              "a replication sink pins synchronous=full and must be refused here"
          | Some m -> check_contains "commit sink" ~needle:"write barrier" m);
         (* Unregistering is always allowed. *)
         let* () = Store.set_commit_callback store None in
         Store.close store))
;;

(* ------------------------------------------------------------------ *)
(* 7: Unix_file is untouched                                            *)
(* ------------------------------------------------------------------ *)

let test_unix_file_unchanged () =
  let path = Filename.temp_file "granary_772_unix" ".db" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun p ->
           try Unix.unlink p with
           | Unix.Unix_error _ -> ())
        [ path; path ^ "-wal"; path ^ ".aslog" ])
    (fun () ->
       run
         (let* r = Granary_unix.Store.open_file_wal ~path () in
          match r with
          | Error e -> Alcotest.failf "open_file_wal: %a" Store.pp_error e
          | Ok store ->
            (* A real fsync exists, so nothing about #772 applies: the store is
               [`Available] and every durability level still works. *)
            (match Store.barrier store with
             | `Available -> ()
             | `Unavailable r ->
               Alcotest.failf "Unix_file must report a barrier, got: %s" r);
            Alcotest.(check string)
              "Unix_file still defaults to full"
              "full"
              (Store.string_of_durability (Store.durability store));
            let* db = Db.of_store store in
            let expect_ok sql =
              let* er = Db.execute db sql in
              match er with
              | Ok () -> Lwt.return_unit
              | Error e -> Alcotest.failf "%S: %a" sql Db.pp_error e
            in
            let* () = expect_ok "PRAGMA synchronous = full" in
            let* () = expect_ok "PRAGMA synchronous = batched" in
            let* () = expect_ok "PRAGMA synchronous = off" in
            let* () = expect_ok "PRAGMA synchronous = full" in
            let* () = expect_ok "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)" in
            let* () = expect_ok "INSERT INTO t VALUES (1, 'a')" in
            let* qr = Db.query db "SELECT v FROM t WHERE id = 1" in
            let* () =
              match qr with
              | Error e -> Alcotest.failf "SELECT: %a" Db.pp_error e
              | Ok stream ->
                let* rows = Lwt_stream.to_list stream in
                Alcotest.(check int) "one row back" 1 (List.length rows);
                Lwt.return_unit
            in
            Db.close db))
;;

(* ------------------------------------------------------------------ *)
(* 8 (#785): the declared barrier governs [wal_sync] too                *)
(* ------------------------------------------------------------------ *)

(* The flush of a WAL that lives on the same barrier-less device as the main DB
   -- the byte-over-sector shim mirage/README.md names as the follow-up.  Such a
   device is required to return [Error], exactly as [Mirage_backend.sync] does;
   [calls] records whether the engine reached for it at all. *)
let failing_wal_flush calls () =
  incr calls;
  Lwt.return (Error "wal device: no flush or barrier operation")
;;

(* [barrier] is [Mirage_backend.connect]'s -- the device's own capability.  The
   store-side [~barrier] is always derived from it, which is the wiring every
   caller is told to use. *)
let open_mirage_wal path ~barrier ~durability ~wal_sync =
  let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
  let* adapter = MB.connect ~barrier dev in
  let wal = Mem_wal.create () in
  let* r =
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
      ~wal_sync
      ~wal_size_bytes:(Mem_wal.size_bytes wal)
      ~close:(fun () -> MB.close adapter)
      ~wal_close:(fun () -> Lwt.return_unit)
      ()
  in
  match r with
  | Error _ ->
    (* A refusal builds no store, so the adapter is ours to release (#753/#763). *)
    let* () = MB.close adapter in
    Lwt.return r
  | Ok _ -> Lwt.return r
;;

(* #785: with no barrier the store must not CALL [wal_sync] either -- the same
   rule #772 applied to the main-DB [sync], and for the same reason.  [Off] is
   the only level such a device is permitted, yet choosing it still produced a
   store that could not be opened ([Wal] fsyncs the header it creates) and,
   once anything had been committed, could not be CLOSED ([Store.close]'s final
   flush is unconditional below [Full]).  The call count is the direct
   assertion: a store that promises nothing asks for no barriers. *)
let test_wal_flush_is_substituted_under_off () =
  with_tmp (fun path ->
    run
      (let calls = ref 0 in
       let* r =
         open_mirage_wal
           path
           ~barrier:None
           ~durability:Store.Off
           ~wal_sync:(failing_wal_flush calls)
       in
       match r with
       | Error e ->
         Alcotest.failf
           "#785: off over a barrier-less WAL must open, got %a"
           Store.pp_error
           e
       | Ok store ->
         let* db = Db.of_store store in
         let expect_ok sql =
           let* er = Db.execute db sql in
           match er with
           | Ok () -> Lwt.return_unit
           | Error e -> Alcotest.failf "%S: %a" sql Db.pp_error e
         in
         let* () = expect_ok "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)" in
         let* () = expect_ok "INSERT INTO t VALUES (1, 'a')" in
         (* A checkpoint rewrites the WAL's generation marker, which fsyncs. *)
         let* () = expect_ok "PRAGMA wal_checkpoint" in
         let* () = expect_ok "INSERT INTO t VALUES (2, 'b')" in
         (* Close is the case the review found: it flushes whenever the level
            is below [Full] and the WAL has committed frames, which under
            [Off] is always, after any write. *)
         let* failed =
           Lwt.catch
             (fun () ->
                let* () = Db.close db in
                Lwt.return_none)
             (fun exn -> Lwt.return_some (Printexc.to_string exn))
         in
         (match failed with
          | None -> ()
          | Some m -> Alcotest.failf "#785: close must not fail: %s" m);
         Alcotest.(check int)
           "#785: the engine never reached for the barrier-less device's flush"
           0
           !calls;
         Lwt.return_unit))
;;

(* The complement, and the reason the substitution is not "ignore WAL flush
   errors": with a barrier DECLARED, a failing [wal_sync] is a real failure and
   must surface.  [Wal.open_] fsyncs the header it creates, so it surfaces here
   at open. *)
let test_wal_flush_error_surfaces_when_a_barrier_is_declared () =
  with_tmp (fun path ->
    run
      (let calls = ref 0 in
       let* r =
         open_mirage_wal
           path
           ~barrier:(Some (fsync_barrier path))
           ~durability:Store.Full
           ~wal_sync:(failing_wal_flush calls)
       in
       (match r with
        | Ok _ ->
          Alcotest.fail
            "a declared barrier whose WAL flush fails must not open successfully"
        | Error e ->
          check_contains
            "declared barrier"
            ~needle:"no flush or barrier operation"
            (Format.asprintf "%a" Store.pp_error e));
       Alcotest.(check bool)
         "and the barrier-less WAL flush really was called"
         true
         (!calls > 0);
       Lwt.return_unit))
;;

(* Seed a valid header using a device that really can flush, so the reopen in
   the next test cannot fail at initialisation for an unrelated reason. *)
let seed_mirage_db path =
  let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
  let* adapter = MB.connect ~barrier:(Some (fsync_barrier path)) dev in
  let* r =
    Store.open_block
      ~barrier:(MB.durability_barrier adapter)
      ~init_if_corrupt:true
      ~read_page:(MB.read_page adapter)
      ~write_page:(MB.write_page adapter)
      ~sync:(MB.sync adapter)
      ~resize:(MB.resize adapter)
      ~n_pages:(MB.n_pages adapter)
      ~close:(fun () -> MB.close adapter)
      ()
  in
  match r with
  | Error e -> Alcotest.failf "seed open: %a" Store.pp_error e
  | Ok store -> Store.close store
;;

(* #785 (the second finding): the residual that the now-mandatory [~barrier] at
   [Mirage_backend.connect] keeps a caller away from.  [Store.open_block]'s own
   [?barrier] still defaults to [`Available] -- right for [Unix_file], wrong for
   this adapter -- so a store wired up without it BELIEVES in a barrier that
   every [sync] then refuses, and the failure lands on a later write instead of
   at open.  Pinned rather than left implicit, so that changing the store-side
   default is a deliberate act with a red test behind it. *)
let test_store_barrier_default_still_believes_the_adapter () =
  with_tmp (fun path ->
    run
      (let* () = seed_mirage_db path in
       let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
       let* adapter = MB.connect ~barrier:None dev in
       let* r =
         Store.open_block
           ~init_if_corrupt:false
           ~read_page:(MB.read_page adapter)
           ~write_page:(MB.write_page adapter)
           ~sync:(MB.sync adapter)
           ~resize:(MB.resize adapter)
           ~n_pages:(MB.n_pages adapter)
           ~close:(fun () -> MB.close adapter)
           ()
       in
       match r with
       | Error e ->
         (* Strictly better than what happens today -- but say so rather than
            passing silently, because #785's note would then be stale. *)
         Alcotest.failf
           "the store-side default no longer lets this open; update #785's note: %a"
           Store.pp_error
           e
       | Ok store ->
         (match Store.barrier store with
          | `Available -> ()
          | `Unavailable _ ->
            Alcotest.fail "Store.open_block's ?barrier still defaults to `Available");
         let* db = Db.of_store store in
         let* er = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY)" in
         (match er with
          | Ok () ->
            Alcotest.fail
              "the adapter's sync returns Error, so a write must not report success"
          | Error _ -> ());
         Lwt.catch (fun () -> Db.close db) (fun _ -> Lwt.return_unit)))
;;

let () =
  let open Alcotest in
  run
    "mirage_sync_durability_772"
    [ ( "adapter"
      , [ test_case
            "sync refuses without a barrier"
            `Quick
            test_sync_refuses_without_barrier
        ; test_case "supplied barrier is used" `Quick test_sync_uses_supplied_barrier
        ; test_case
            "supplied barrier's error propagates"
            `Quick
            test_supplied_barrier_error_propagates
        ] )
    ; ( "open"
      , [ test_case "full is refused" `Quick test_full_is_refused
        ; test_case "batched is refused" `Quick test_batched_is_refused
        ; test_case "the default (full) is refused" `Quick test_default_is_refused
        ; test_case "off opens and works" `Quick test_off_opens_and_works
        ] )
    ; ( "after open"
      , [ test_case
            "set_durability keeps refusing"
            `Quick
            test_set_durability_refuses_after_open
        ; test_case
            "PRAGMA synchronous refuses full/batched"
            `Quick
            test_pragma_synchronous_refuses
        ; test_case "a replication commit-sink is refused" `Quick test_commit_sink_refused
        ] )
    ; "unix_file", [ test_case "unaffected" `Quick test_unix_file_unchanged ]
    ; ( "wal barrier (#785)"
      , [ test_case
            "no barrier: wal_sync is substituted, and close works"
            `Quick
            test_wal_flush_is_substituted_under_off
        ; test_case
            "declared barrier: a failing wal_sync still surfaces"
            `Quick
            test_wal_flush_error_surfaces_when_a_barrier_is_declared
        ; test_case
            "Store.open_block's ?barrier default still believes the adapter"
            `Quick
            test_store_barrier_default_still_believes_the_adapter
        ] )
    ]
;;
