(** Tests for #150 — auto-checkpoint when WAL crosses a per-connection
    threshold.

    Goals:
    - Sustained writes under a low threshold keep the WAL file bounded.
    - Threshold = 0 reproduces the legacy unbounded-WAL behaviour.
    - PRAGMA wal_autocheckpoint round-trips through SQL.
    - Data written across many auto-checkpoints is still readable
      (correctness regression guard). *)

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
  Printf.sprintf "/tmp/sqlocaml_test_wal_autockpt_%04d.db" n
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

let wal_size path =
  try (Unix.stat (path ^ "-wal")).Unix.st_size with
  | _ -> 0
;;

let frame_size = 4120
let bs = Bytes.of_string

(* ----------------------------------------------------------------- *)

let test_bounded_under_low_threshold () =
  run
  @@ with_fresh ~f:(fun path ->
    let* sr = S.open_file_wal ~path () in
    let st =
      match sr with
      | Ok t -> t
      | Error e -> Alcotest.failf "open_file_wal: %a" S.pp_error e
    in
    (* Tight threshold so the bound is easy to observe. *)
    S.set_wal_autocheckpoint st 10;
    (* 500 single-key autocommits — well past the threshold. *)
    let rec loop i =
      if i = 500
      then Lwt.return_unit
      else
        let* tx = S.rw_begin st in
        let* () =
          S.put tx 16 (bs (Printf.sprintf "k%04d" i)) (bs (Printf.sprintf "v%04d" i))
        in
        let* () = S.commit tx in
        loop (i + 1)
    in
    let* () = loop 0 in
    let bytes = wal_size path in
    (* Each commit writes a handful of frames; with threshold = 10 the WAL
       resets often. Bound it generously at 200 frames to absorb the
       overshoot from the final batch. *)
    let upper = 200 * frame_size in
    Alcotest.(check bool)
      (Printf.sprintf "WAL bounded (got %d bytes, upper=%d)" bytes upper)
      true
      (bytes <= upper);
    (* Sanity: data still readable after many auto-checkpoints. *)
    let* tx = S.ro_begin st in
    let* v = S.get tx 16 (bs "k0250") in
    let* () = S.ro_end tx in
    Alcotest.(check (option string))
      "data still readable"
      (Some "v0250")
      (Option.map Bytes.to_string v);
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_zero_threshold_disables () =
  run
  @@ with_fresh ~f:(fun path ->
    let* sr = S.open_file_wal ~path () in
    let st =
      match sr with
      | Ok t -> t
      | Error e -> Alcotest.failf "open_file_wal: %a" S.pp_error e
    in
    S.set_wal_autocheckpoint st 0;
    let rec loop i =
      if i = 300
      then Lwt.return_unit
      else
        let* tx = S.rw_begin st in
        let* () =
          S.put tx 16 (bs (Printf.sprintf "k%04d" i)) (bs (Printf.sprintf "v%04d" i))
        in
        let* () = S.commit tx in
        loop (i + 1)
    in
    let* () = loop 0 in
    let bytes = wal_size path in
    (* With auto-checkpoint disabled the WAL must keep accumulating —
       300 commits at >=1 frame each clears the threshold-on bound. *)
    let lower = 300 * frame_size in
    Alcotest.(check bool)
      (Printf.sprintf "WAL grew unbounded (got %d bytes, lower=%d)" bytes lower)
      true
      (bytes >= lower);
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_default_threshold () =
  run
  @@ with_fresh ~f:(fun path ->
    let* sr = S.open_file_wal ~path () in
    let st =
      match sr with
      | Ok t -> t
      | Error e -> Alcotest.failf "open_file_wal: %a" S.pp_error e
    in
    Alcotest.(check int) "default threshold = 1000" 1000 (S.wal_autocheckpoint st);
    let* () = S.close st in
    Lwt.return_unit)
;;

(* ----------------------------------------------------------------- *)
(* PRAGMA round-trip via the SQL layer.                                *)
(* ----------------------------------------------------------------- *)

let row_int_first stream =
  let* rows = Lwt_stream.to_list stream in
  match rows with
  | [ [| D.V_int n |] ] -> Lwt.return (Int64.to_int n)
  | _ -> Alcotest.failf "expected one int row, got %d rows" (List.length rows)
;;

let test_pragma_round_trip () =
  run
  @@ with_fresh ~f:(fun path ->
    let* db = D.open_file_wal ~path () in
    let db =
      match db with
      | Ok d -> d
      | Error e -> Alcotest.failf "Db.open_file_wal: %a" D.pp_error e
    in
    (* Default value visible through PRAGMA *)
    let* s = D.query db "PRAGMA wal_autocheckpoint" in
    let s =
      match s with
      | Ok s -> s
      | Error e -> Alcotest.failf "query: %a" D.pp_error e
    in
    let* v = row_int_first s in
    Alcotest.(check int) "PRAGMA reads default" 1000 v;
    (* Set new value *)
    let* r = D.execute db "PRAGMA wal_autocheckpoint = 250" in
    (match r with
     | Ok () -> ()
     | Error e -> Alcotest.failf "execute set: %a" D.pp_error e);
    let* s = D.query db "PRAGMA wal_autocheckpoint" in
    let s =
      match s with
      | Ok s -> s
      | Error e -> Alcotest.failf "query: %a" D.pp_error e
    in
    let* v = row_int_first s in
    Alcotest.(check int) "PRAGMA = N updates" 250 v;
    (* Disable *)
    let* r = D.execute db "PRAGMA wal_autocheckpoint = 0" in
    (match r with
     | Ok () -> ()
     | Error e -> Alcotest.failf "execute disable: %a" D.pp_error e);
    let* s = D.query db "PRAGMA wal_autocheckpoint" in
    let s =
      match s with
      | Ok s -> s
      | Error e -> Alcotest.failf "query: %a" D.pp_error e
    in
    let* v = row_int_first s in
    Alcotest.(check int) "PRAGMA = 0 disables" 0 v;
    let* () = D.close db in
    Lwt.return_unit)
;;

(* ----------------------------------------------------------------- *)
(* Background autocheckpoint: writer must not block while checkpoint  *)
(* runs asynchronously in the background.                              *)
(* ----------------------------------------------------------------- *)

let test_writer_not_blocked_by_autocheckpoint () =
  (* After crossing threshold, the writer's commit must return promptly;
     checkpoint runs in the background.  Sanity check: 10 inserts past
     threshold complete in well under 1 second on a small DB. *)
  let path = "/tmp/sqlocaml_phase38_actk_async.db" in
  (try Unix.unlink path with
   | _ -> ());
  (try Unix.unlink (path ^ "-wal") with
   | _ -> ());
  let db =
    match run (D.open_file_wal ~path ()) with
    | Ok d -> d
    | Error _ -> Alcotest.fail "open"
  in
  let exec sql =
    match run (D.execute db sql) with
    | Ok () -> ()
    | Error _ -> Alcotest.failf "exec failed: %s" sql
  in
  exec "PRAGMA wal_autocheckpoint = 50";
  exec "CREATE TABLE t (n INTEGER)";
  for i = 0 to 60 do
    exec (Printf.sprintf "INSERT INTO t VALUES (%d)" i)
  done;
  let t0 = Unix.gettimeofday () in
  for i = 100 to 109 do
    exec (Printf.sprintf "INSERT INTO t VALUES (%d)" i)
  done;
  let elapsed = Unix.gettimeofday () -. t0 in
  Alcotest.(check bool)
    (Printf.sprintf "10 inserts past threshold under 1s (was %.3fs)" elapsed)
    true
    (elapsed < 1.0);
  (* Drive a single Lwt scheduler tick so any pending Lwt.async fiber
     gets a chance to start.  D.close below also flushes the scheduler;
     the assertion above already passed regardless. *)
  run (Lwt.pause ());
  run (D.close db);
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

(* ----------------------------------------------------------------- *)

let () =
  Alcotest.run
    "wal_autocheckpoint"
    [ ( "auto-checkpoint"
      , [ Alcotest.test_case "default threshold is 1000" `Quick test_default_threshold
        ; Alcotest.test_case
            "low threshold keeps WAL bounded"
            `Quick
            test_bounded_under_low_threshold
        ; Alcotest.test_case "threshold = 0 disables" `Quick test_zero_threshold_disables
        ; Alcotest.test_case "PRAGMA round-trip" `Quick test_pragma_round_trip
        ; Alcotest.test_case
            "writer not blocked by background autocheckpoint"
            `Quick
            test_writer_not_blocked_by_autocheckpoint
        ] )
    ]
;;
