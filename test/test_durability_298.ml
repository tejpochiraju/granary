(** Tests for #298 — per-deployment durability knob (full/batched/off). *)

open Lwt.Syntax

module S = struct
  include Sqlocaml_store.Store

  let open_file_wal = Sqlocaml_unix.Store.open_file_wal
end

module D = struct
  include Sqlocaml.Db

  let open_file_wal = Sqlocaml_unix.open_file_wal
end

let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_dura_298_%04d.db" n
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

let with_fresh ~f =
  let path = fresh_path () in
  cleanup path;
  Lwt.finalize
    (fun () -> f path)
    (fun () ->
       cleanup path;
       Lwt.return_unit)
;;

let bs = Bytes.of_string

let open_st path =
  let* sr = S.open_file_wal ~path () in
  match sr with
  | Ok t -> Lwt.return t
  | Error e -> Alcotest.failf "open_file_wal: %a" S.pp_error e
;;

(* --- accessors --- *)

let test_default_is_full () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    Alcotest.(check bool)
      "default Full"
      true
      (match S.durability st with
       | S.Full -> true
       | _ -> false);
    Alcotest.(check int) "default batch commits" 256 (S.sync_batch_commits st);
    Alcotest.(check int) "default batch interval" 100 (S.sync_batch_interval_ms st);
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_set_get_round_trip () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    S.set_durability st (S.Batched { commits = 7; interval_ms = 33 });
    Alcotest.(check bool)
      "now Batched"
      true
      (match S.durability st with
       | S.Batched _ -> true
       | _ -> false);
    Alcotest.(check int) "commits stored" 7 (S.sync_batch_commits st);
    Alcotest.(check int) "interval stored" 33 (S.sync_batch_interval_ms st);
    S.set_durability st S.Full;
    Alcotest.(check int) "commits survive mode switch" 7 (S.sync_batch_commits st);
    S.set_durability st (S.Batched { commits = 7; interval_ms = 33 });
    S.set_sync_batch_commits st 99;
    Alcotest.(check int) "granular N" 99 (S.sync_batch_commits st);
    Alcotest.(check bool)
      "still Batched"
      true
      (match S.durability st with
       | S.Batched _ -> true
       | _ -> false);
    S.set_durability st S.Full;
    Alcotest.(check int)
      "interval_ms survives switch to Full"
      33
      (S.sync_batch_interval_ms st);
    S.set_durability st S.Off;
    Alcotest.(check bool)
      "Off round-trip"
      true
      (match S.durability st with
       | S.Off -> true
       | _ -> false);
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_mem_backend_noop () =
  let st = S.create () in
  (* Mem backend: setters are no-ops, getters return defaults *)
  S.set_durability st (S.Batched { commits = 5; interval_ms = 5 });
  Alcotest.(check bool)
    "mem stays Full"
    true
    (match S.durability st with
     | S.Full -> true
     | _ -> false);
  Alcotest.(check int) "mem commits default" 256 (S.sync_batch_commits st);
  Alcotest.(check int) "mem interval default" 100 (S.sync_batch_interval_ms st);
  run (S.close st)
;;

(* --- fsync accounting --- *)

let commit_kv st i =
  let* tx = S.rw_begin st in
  let* () = S.put tx 16 (bs (Printf.sprintf "k%04d" i)) (bs (Printf.sprintf "v%04d" i)) in
  S.commit tx
;;

let do_commits st n =
  let rec loop i =
    if i = n
    then Lwt.return_unit
    else
      let* () = commit_kv st i in
      loop (i + 1)
  in
  loop 0
;;

let test_full_syncs_each_commit () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    S.set_durability st S.Full;
    S.set_wal_autocheckpoint st 0;
    let s0 = S.wal_sync_count st in
    let* () = do_commits st 20 in
    let delta = S.wal_sync_count st - s0 in
    Alcotest.(check bool)
      (Printf.sprintf "full: ~1 fsync/commit (got %d for 20)" delta)
      true
      (delta >= 20);
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_off_never_syncs_on_commit () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    S.set_durability st S.Off;
    S.set_wal_autocheckpoint st 0;
    let s0 = S.wal_sync_count st in
    let* () = do_commits st 50 in
    let delta = S.wal_sync_count st - s0 in
    Alcotest.(check int) "off: zero commit fsyncs" 0 delta;
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_batched_syncs_every_n () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    S.set_durability
      st
      (S.Batched
         { commits = 10
         ; interval_ms = 1_000_000 (* effectively infinite: disables the T trigger *)
         });
    S.set_wal_autocheckpoint st 0;
    let s0 = S.wal_sync_count st in
    let* () = do_commits st 30 in
    let delta = S.wal_sync_count st - s0 in
    Alcotest.(check int) "batched N=10 over 30 commits => exactly 3 fsyncs" 3 delta;
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_batched_syncs_on_time () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    let now = ref 0. in
    S.set_clock st (fun () -> !now);
    S.set_durability st (S.Batched { commits = 1_000_000; interval_ms = 100 });
    S.set_wal_autocheckpoint st 0;
    let s0 = S.wal_sync_count st in
    let* () = do_commits st 5 in
    Alcotest.(check int) "no sync before T elapses" 0 (S.wal_sync_count st - s0);
    now := 0.5;
    (* 500ms > 100ms threshold *)
    let* () = commit_kv st 999 in
    Alcotest.(check bool) "sync after T elapses" true (S.wal_sync_count st - s0 >= 1);
    let* () = S.close st in
    Lwt.return_unit)
;;

(* --- durability anchors --- *)

let test_off_durable_after_close () =
  let path = fresh_path () in
  cleanup path;
  (* Write in off mode, then close (which must flush), reopen, read. *)
  run
    (let* st = open_st path in
     S.set_durability st S.Off;
     S.set_wal_autocheckpoint st 0;
     let* () = do_commits st 25 in
     S.close st);
  run
    (let* st = open_st path in
     let* tx = S.ro_begin st in
     let* v = S.get tx 16 (bs "k0010") in
     let* () = S.ro_end tx in
     Alcotest.(check (option string))
       "off-mode data durable after clean close"
       (Some "v0010")
       (Option.map Bytes.to_string v);
     S.close st);
  cleanup path
;;

let test_batched_durable_after_close () =
  let path = fresh_path () in
  cleanup path;
  run
    (let* st = open_st path in
     (* High N + no clock => neither N nor T fires; commits stay unsynced. *)
     S.set_durability st (S.Batched { commits = 1_000_000; interval_ms = 1_000_000 });
     S.set_wal_autocheckpoint st 0;
     let* () = do_commits st 15 in
     S.close st);
  run
    (let* st = open_st path in
     let* tx = S.ro_begin st in
     let* v = S.get tx 16 (bs "k0007") in
     let* () = S.ro_end tx in
     Alcotest.(check (option string))
       "batched data durable after clean close"
       (Some "v0007")
       (Option.map Bytes.to_string v);
     S.close st);
  cleanup path
;;

(* --- PRAGMA surface --- *)

let open_db path =
  let* db = D.open_file_wal ~path () in
  match db with
  | Ok d -> Lwt.return d
  | Error e -> Alcotest.failf "Db.open_file_wal: %a" D.pp_error e
;;

let query1_text db sql =
  let* s = D.query db sql in
  let* s =
    match s with
    | Ok s -> Lwt.return s
    | Error e -> Alcotest.failf "query: %a" D.pp_error e
  in
  let* rows = Lwt_stream.to_list s in
  match rows with
  | [ [| D.V_text t |] ] -> Lwt.return t
  | _ -> Alcotest.failf "expected one text row for %s" sql
;;

let query1_int db sql =
  let* s = D.query db sql in
  let* s =
    match s with
    | Ok s -> Lwt.return s
    | Error e -> Alcotest.failf "query: %a" D.pp_error e
  in
  let* rows = Lwt_stream.to_list s in
  match rows with
  | [ [| D.V_int n |] ] -> Lwt.return (Int64.to_int n)
  | _ -> Alcotest.failf "expected one int row for %s" sql
;;

let exec_ok db sql =
  let* r = D.execute db sql in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Alcotest.failf "execute %s: %a" sql D.pp_error e
;;

let test_pragma_round_trip () =
  run
  @@ with_fresh ~f:(fun path ->
    let* db = open_db path in
    let* v = query1_text db "PRAGMA synchronous" in
    Alcotest.(check string) "default full" "full" v;
    let* () = exec_ok db "PRAGMA synchronous = batched" in
    let* v = query1_text db "PRAGMA synchronous" in
    Alcotest.(check string) "set batched" "batched" v;
    let* () = exec_ok db "PRAGMA wal_batch_commits = 42" in
    let* n = query1_int db "PRAGMA wal_batch_commits" in
    Alcotest.(check int) "N round-trip" 42 n;
    let* () = exec_ok db "PRAGMA wal_batch_interval_ms = 250" in
    let* n = query1_int db "PRAGMA wal_batch_interval_ms" in
    Alcotest.(check int) "T round-trip" 250 n;
    let* () = exec_ok db "PRAGMA synchronous = off" in
    let* v = query1_text db "PRAGMA synchronous" in
    Alcotest.(check string) "set off" "off" v;
    let* () = exec_ok db "PRAGMA synchronous = full" in
    let* v = query1_text db "PRAGMA synchronous" in
    Alcotest.(check string) "back to full" "full" v;
    let* () = D.close db in
    Lwt.return_unit)
;;

let test_pragma_invalid_value () =
  run
  @@ with_fresh ~f:(fun path ->
    let* db = open_db path in
    let* r = D.execute db "PRAGMA synchronous = wat" in
    Alcotest.(check bool)
      "invalid mode rejected"
      true
      (match r with
       | Error _ -> true
       | Ok () -> false);
    let* () = D.close db in
    Lwt.return_unit)
;;

(* --- Db open-option --- *)

let test_of_store_durability_option () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    let* db =
      D.of_store
        ~durability:(Sqlocaml_store.Store.Batched { commits = 8; interval_ms = 20 })
        st
    in
    let* v = query1_text db "PRAGMA synchronous" in
    Alcotest.(check string) "of_store option applied" "batched" v;
    let* n = query1_int db "PRAGMA wal_batch_commits" in
    Alcotest.(check int) "of_store N applied" 8 n;
    let* () = D.close db in
    Lwt.return_unit)
;;

(* WAL frame size constant (header 32 bytes + page 4096 bytes = 4128? no:
   the WAL uses 4096-byte pages and 24-byte frame headers = 4120 bytes/frame,
   consistent with test_wal_autocheckpoint.ml's frame_size = 4120). *)
let frame_size = 4120

(* Simulate a crash by truncating the WAL to a frame-aligned length that
   drops some trailing frames.  This mirrors the idiom in test_crash_property.ml
   (truncate_wal / Unix.ftruncate) and represents kernel-buffer data that was
   never flushed to disk because no fsync was issued in Off mode.

   We keep the WAL header (32 bytes) plus [keep_frames] complete frames.
   If the WAL file is smaller than expected we still truncate to whatever
   frame-aligned size is possible (possibly 0 usable frames). *)
let simulate_crash_truncate path ~keep_frames =
  let wal_path = path ^ "-wal" in
  (* WAL file layout: 32-byte WAL header + N * frame_size bytes of frames *)
  let wal_header_size = 32 in
  let target = wal_header_size + (keep_frames * frame_size) in
  try
    let actual = (Unix.stat wal_path).Unix.st_size in
    let truncate_to = min target actual in
    (* Round down to a frame boundary relative to the header *)
    let usable = truncate_to - wal_header_size in
    let aligned =
      wal_header_size + if usable < 0 then 0 else usable - (usable mod frame_size)
    in
    let fd = Unix.openfile wal_path [ Unix.O_RDWR ] 0o644 in
    Unix.ftruncate fd aligned;
    Unix.close fd
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
    (* WAL does not exist yet — nothing to truncate.
        This can happen if the engine checkpointed everything back to the
        main file; in that case the data is already durable. *)
    ()
;;

(* Test: in Off mode, after a simulated crash (WAL tail truncation at a
   frame boundary — representing unflushed kernel buffers lost on process
   death), recovery must yield a COMMIT-PREFIX of the acknowledged commits.
   That is: the recovered keys form a contiguous range k0000..k{j-1} for
   some j in [0,40].  No holes are permitted (a missing key followed by a
   present one would indicate structural inconsistency in the WAL replay).

   Crash simulation methodology (mirrors test_crash_property.ml):
   - Write commits in Off mode (no fsync on commit — data sits in OS page cache).
   - Close the store normally (the fd is released but no fsync was ever issued
     for the commit payloads, so the WAL tail may or may not be on stable storage).
   - Truncate the WAL to a frame-aligned offset keeping roughly the first half of
     frames.  This models the kernel discarding the trailing OS page-cache pages
     that were never fsynced to disk.
   - Reopen and assert the prefix property. *)
let test_off_recovers_prefix () =
  let path = fresh_path () in
  cleanup path;
  (* Phase 1: write 40 commits in Off mode (no fsyncs issued on commit).
     Close the store normally — this releases the lock in locked_inodes so
     we can reopen below.  The "crash" is modelled by the WAL truncation
     that follows: we discard the trailing frames that were never fsynced. *)
  run
    (let* st = open_st path in
     S.set_durability st S.Off;
     S.set_wal_autocheckpoint st 0;
     let* () = do_commits st 40 in
     S.close st);
  (* Phase 2: truncate the WAL to lose some trailing frames.
     We keep roughly half the frames to exercise partial-recovery, while
     still leaving enough for prefix-consistency to be testable.  The exact
     cut point does not matter; what matters is that the remaining frames
     decode to a valid prefix of commits. *)
  (let wal_path = path ^ "-wal" in
   let wal_total =
     try (Unix.stat wal_path).Unix.st_size with
     | _ -> 0
   in
   (* Compute total frame count and keep roughly the first half. *)
   let wal_header_size = 32 in
   let total_frames = (wal_total - wal_header_size) / frame_size in
   let keep_frames = total_frames / 2 in
   simulate_crash_truncate path ~keep_frames);
  (* Phase 3: reopen the DB and verify the prefix property. *)
  run
    (let* st = open_st path in
     let* tx = S.ro_begin st in
     let* first = S.get tx 16 (bs "k0000") in
     (match first with
      | Some _ -> ()
      | None ->
        Alcotest.fail
          "k0000 missing after half-truncation — total loss; prefix test would be vacuous");
     let rec scan i seen_gap =
       if i = 40
       then Lwt.return_unit
       else
         let* v = S.get tx 16 (bs (Printf.sprintf "k%04d" i)) in
         match v, seen_gap with
         | Some _, true ->
           Alcotest.failf "hole then key at %d — not a prefix (gap before i=%d)" i i
         | None, _ -> scan (i + 1) true
         | Some _, false -> scan (i + 1) false
     in
     let* () = scan 0 false in
     let* () = S.ro_end tx in
     S.close st);
  cleanup path
;;

(* --- review fixes (#298 Group A) --- *)

(* #8: a non-positive count threshold DISABLES the count trigger rather than
   firing every commit.  With T effectively infinite and no clock advance, no
   commit should ever sync. *)
let test_n_zero_disables_count_trigger () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    S.set_durability st (S.Batched { commits = 0; interval_ms = 1_000_000 });
    S.set_wal_autocheckpoint st 0;
    let s0 = S.wal_sync_count st in
    let* () = do_commits st 30 in
    let delta = S.wal_sync_count st - s0 in
    Alcotest.(check int) "N=0 disables count trigger: zero commit fsyncs" 0 delta;
    (* Control: a positive N still works (N=10 over 30 => 3 syncs). *)
    S.set_durability st (S.Batched { commits = 10; interval_ms = 1_000_000 });
    let s1 = S.wal_sync_count st in
    let* () = do_commits st 30 in
    let delta2 = S.wal_sync_count st - s1 in
    Alcotest.(check int) "control N=10 over 30 commits => 3 fsyncs" 3 delta2;
    let* () = S.close st in
    Lwt.return_unit)
;;

(* #4: switching modes resets the unsynced-commit counter, so a long [Off]
   run does not carry stale accrual into [Batched] and trip N immediately. *)
let test_mode_switch_resets_counter () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    S.set_wal_autocheckpoint st 0;
    S.set_durability st S.Off;
    let* () = do_commits st 20 in
    (* Now switch to Batched N=10.  If the 20 Off-commits had carried over,
       the very first batched commit would immediately trip N=10. *)
    S.set_durability st (S.Batched { commits = 10; interval_ms = 1_000_000 });
    let s0 = S.wal_sync_count st in
    let* () = do_commits st 5 in
    let delta = S.wal_sync_count st - s0 in
    Alcotest.(check int) "mode switch reset counter: 5 batched commits, no sync" 0 delta;
    let* () = S.close st in
    Lwt.return_unit)
;;

(* #1 (headline): the replication sink must NEVER fire for unsynced frames.
   In batched mode with N high and no clock, commits accumulate unsynced, so
   the sink callback must fire 0 times.  A checkpoint then makes them durable
   (via its own path), after which the sink ship cursor is reset for the new
   epoch. *)
(* Follow-up (#298): a registered sink pins durability to [Full], so the
   batched/off "withhold unsynced frames from the sink" scenario is now
   architecturally disallowed.  With a sink registered, attempting [Batched]
   is ignored, every commit fsyncs, and the sink ships every committed frame.
   (Previously this test exercised the now-removed gated-ship path.) *)
let test_sink_gated_to_synced () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    S.set_wal_autocheckpoint st 0;
    let fired = ref 0 in
    let* () =
      S.set_commit_callback
        st
        (Some
           (fun ~epoch:_ ~base_idx:_ ~count:_ ->
             incr fired;
             Lwt.return_unit))
    in
    (* Sink forces Full; the relax request is ignored. *)
    S.set_durability st (S.Batched { commits = 1_000_000; interval_ms = 1_000_000 });
    Alcotest.(check bool) "sink pins Full" true (S.durability st = S.Full);
    let* () = do_commits st 5 in
    (* Let the async sink callbacks run. *)
    let* () = Lwt.pause () in
    Alcotest.(check bool) "sink ships every committed frame" true (!fired >= 1);
    let* () = S.close st in
    Lwt.return_unit)
;;

(* --- review fixes (#298 Group B) --- *)

(* B1 (#3): switching to [full] FIRST flushes pending unsynced frames, so
   commits acked under a relaxed mode become durable immediately.  We open a
   WAL store, set batched with a high N and no clock so commits stay unsynced
   (wal_sync_count does not advance), do a few commits, then flip to full via
   the store-level [flush_unsynced] (the exact mechanism [PRAGMA synchronous =
   full] runs before [set_durability]).  The sync count must increase. *)
let test_switch_to_full_flushes_store () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    S.set_wal_autocheckpoint st 0;
    S.set_durability st (S.Batched { commits = 1_000_000; interval_ms = 1_000_000 });
    let s0 = S.wal_sync_count st in
    let* () = do_commits st 5 in
    Alcotest.(check int)
      "commits stayed unsynced under high-N batched"
      0
      (S.wal_sync_count st - s0);
    let* () = S.flush_unsynced st in
    Alcotest.(check bool)
      "flush_unsynced flushed pending frames"
      true
      (S.wal_sync_count st - s0 >= 1);
    let* () = S.close st in
    Lwt.return_unit)
;;

(* B1 (#3) at the Db/SQL layer: PRAGMA synchronous = full flushes pending
   unsynced commits made under batched mode before switching. *)
let test_switch_to_full_flushes_sql () =
  run
  @@ with_fresh ~f:(fun path ->
    let* db = open_db path in
    let* () = exec_ok db "PRAGMA wal_autocheckpoint = 0" in
    let* () = exec_ok db "PRAGMA wal_batch_commits = 1000000" in
    let* () = exec_ok db "PRAGMA wal_batch_interval_ms = 1000000" in
    let* () = exec_ok db "CREATE TABLE t (k INTEGER PRIMARY KEY, v TEXT)" in
    let* () = exec_ok db "PRAGMA synchronous = batched" in
    let s0 = D.wal_sync_count db in
    let* () = exec_ok db "INSERT INTO t (k, v) VALUES (1, 'a')" in
    let* () = exec_ok db "INSERT INTO t (k, v) VALUES (2, 'b')" in
    let* () = exec_ok db "INSERT INTO t (k, v) VALUES (3, 'c')" in
    let pending = D.wal_sync_count db - s0 in
    let* () = exec_ok db "PRAGMA synchronous = full" in
    Alcotest.(check bool)
      "PRAGMA synchronous = full flushed pending unsynced commits"
      true
      (D.wal_sync_count db - s0 > pending);
    let* () = D.close db in
    Lwt.return_unit)
;;

(* --- replication-guard (follow-up): a sink pins durability to Full --- *)

let noop_sink = Some (fun ~epoch:_ ~base_idx:_ ~count:_ -> Lwt.return_unit)

(* 1. Registering a sink forces Full even from Off, and reports active. *)
let test_sink_forces_full () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    S.set_durability st S.Off;
    Alcotest.(check bool) "Off before sink" true (S.durability st = S.Off);
    let* () = S.set_commit_callback st noop_sink in
    Alcotest.(check bool) "sink forced Full" true (S.durability st = S.Full);
    Alcotest.(check bool) "callback active" true (S.commit_callback_active st);
    let* () = S.close st in
    Lwt.return_unit)
;;

(* 2. While a sink is active, relaxing via the Store API is ignored. *)
let test_sink_rejects_relax_store () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    let* () = S.set_commit_callback st noop_sink in
    Alcotest.(check bool) "Full after sink" true (S.durability st = S.Full);
    S.set_durability st (S.Batched { commits = 10; interval_ms = 10 });
    Alcotest.(check bool) "relax ignored while sink active" true (S.durability st = S.Full);
    let* () = S.close st in
    Lwt.return_unit)
;;

(* 3. Removing the sink lets durability relax again (and Batched params that
   were recorded while pinned take effect). *)
let test_sink_removal_allows_relax () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    let* () = S.set_commit_callback st noop_sink in
    (* recorded while pinned, not applied yet *)
    S.set_durability st (S.Batched { commits = 7; interval_ms = 13 });
    Alcotest.(check bool) "still Full" true (S.durability st = S.Full);
    let* () = S.set_commit_callback st None in
    Alcotest.(check bool) "no longer active" false (S.commit_callback_active st);
    (* recorded params are available for later use *)
    Alcotest.(check int) "batch N recorded" 7 (S.sync_batch_commits st);
    Alcotest.(check int) "batch T recorded" 13 (S.sync_batch_interval_ms st);
    S.set_durability st (S.Batched { commits = 7; interval_ms = 13 });
    Alcotest.(check bool)
      "relax allowed after removal"
      true
      (S.durability st = S.Batched { commits = 7; interval_ms = 13 });
    let* () = S.close st in
    Lwt.return_unit)
;;

(* #336/1: open(off/batched) → commit (acked but unsynced) → register a sink.
   Registration must flush the pending unsynced frames NOW (it pins Full going
   forward but ships only NEW frames, so these historical frames would otherwise
   linger OS-crash-exposed until the next commit/checkpoint/close). *)
let test_sink_registration_flushes () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    S.set_wal_autocheckpoint st 0;
    S.set_durability st (S.Batched { commits = 1_000_000; interval_ms = 1_000_000 });
    let s0 = S.wal_sync_count st in
    let* () = do_commits st 5 in
    Alcotest.(check int)
      "commits stayed unsynced under high-N batched"
      0
      (S.wal_sync_count st - s0);
    let* () = S.set_commit_callback st noop_sink in
    Alcotest.(check bool)
      "sink registration flushed pending unsynced frames"
      true
      (S.wal_sync_count st - s0 >= 1);
    let* () = S.close st in
    Lwt.return_unit)
;;

(* #336/3: the SQL layer rejects relaxing durability while a sink is active
   (companion to the Store-level [test_sink_rejects_relax_store], which checks
   the silently-ignored Store-API contract — see set_durability docs). *)
let test_sink_rejects_relax_sql () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    let* () = S.set_commit_callback st noop_sink in
    let* db = D.of_store st in
    let* r_off = D.execute db "PRAGMA synchronous = off" in
    Alcotest.(check bool)
      "PRAGMA synchronous=off rejected while sink active"
      true
      (match r_off with
       | Error _ -> true
       | Ok () -> false);
    let* r_batched = D.execute db "PRAGMA synchronous = batched" in
    Alcotest.(check bool)
      "PRAGMA synchronous=batched rejected while sink active"
      true
      (match r_batched with
       | Error _ -> true
       | Ok () -> false);
    (* synchronous=full is still allowed (it's a no-op tighten). *)
    let* r_full = D.execute db "PRAGMA synchronous = full" in
    Alcotest.(check bool)
      "PRAGMA synchronous=full allowed while sink active"
      true
      (match r_full with
       | Ok () -> true
       | Error _ -> false);
    let* () = D.close db in
    Lwt.return_unit)
;;

let () =
  Alcotest.run
    "durability_298"
    [ ( "accessors"
      , [ Alcotest.test_case "default is full" `Quick test_default_is_full
        ; Alcotest.test_case "set/get round-trip" `Quick test_set_get_round_trip
        ; Alcotest.test_case "mem backend no-op" `Quick test_mem_backend_noop
        ] )
    ; ( "fsync-accounting"
      , [ Alcotest.test_case "full syncs each commit" `Quick test_full_syncs_each_commit
        ; Alcotest.test_case
            "off never syncs on commit"
            `Quick
            test_off_never_syncs_on_commit
        ; Alcotest.test_case "batched syncs every N" `Quick test_batched_syncs_every_n
        ; Alcotest.test_case "batched syncs on time" `Quick test_batched_syncs_on_time
        ] )
    ; ( "durability-anchors"
      , [ Alcotest.test_case "off durable after close" `Quick test_off_durable_after_close
        ; Alcotest.test_case
            "batched durable after close"
            `Quick
            test_batched_durable_after_close
        ] )
    ; ( "pragma"
      , [ Alcotest.test_case "synchronous/N/T round-trip" `Quick test_pragma_round_trip
        ; Alcotest.test_case "invalid mode rejected" `Quick test_pragma_invalid_value
        ] )
    ; ( "open-option"
      , [ Alcotest.test_case "of_store ?durability" `Quick test_of_store_durability_option
        ] )
    ; ( "recovery"
      , [ Alcotest.test_case
            "off recovers a commit-prefix"
            `Quick
            test_off_recovers_prefix
        ] )
    ; ( "review-fixes"
      , [ Alcotest.test_case
            "#8 N=0 disables count trigger"
            `Quick
            test_n_zero_disables_count_trigger
        ; Alcotest.test_case
            "#4 mode switch resets counter"
            `Quick
            test_mode_switch_resets_counter
        ; Alcotest.test_case
            "#1 sink pins Full and ships every frame"
            `Quick
            test_sink_gated_to_synced
        ] )
    ; ( "review-fixes-b"
      , [ Alcotest.test_case
            "#3 switch-to-full flushes (store)"
            `Quick
            test_switch_to_full_flushes_store
        ; Alcotest.test_case
            "#3 switch-to-full flushes (SQL)"
            `Quick
            test_switch_to_full_flushes_sql
        ] )
    ; ( "replication-guard"
      , [ Alcotest.test_case "sink forces Full" `Quick test_sink_forces_full
        ; Alcotest.test_case
            "relax rejected while sink active (Store)"
            `Quick
            test_sink_rejects_relax_store
        ; Alcotest.test_case
            "removal allows relax again"
            `Quick
            test_sink_removal_allows_relax
        ; Alcotest.test_case
            "#336/1 sink registration flushes pending unsynced"
            `Quick
            test_sink_registration_flushes
        ; Alcotest.test_case
            "#336/3 relax rejected while sink active (SQL)"
            `Quick
            test_sink_rejects_relax_sql
        ] )
    ]
;;
