(** Tests for #298 — per-deployment durability knob (full/batched/off). *)

open Lwt.Syntax

module S = struct
  include Sqlocaml_store.Store

  let open_file_wal = Sqlocaml_unix.Store.open_file_wal
end

module D = struct
  include Sqlocaml.Db

  (* used by later-task tests (#298) *)
  let open_file_wal = Sqlocaml_unix.open_file_wal [@@warning "-32"]
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

(* used by later-task tests (#298) *)
let bs = Bytes.of_string [@@warning "-32"]

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
        ] )
    ]
;;
