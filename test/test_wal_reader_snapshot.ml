(** Phase 38 / #149 — RO snapshot reads under concurrent WAL writes.

    The Db handle's reader-snapshot is captured per-query (each
    `Db.query` opens its own ro_snapshot internally), so we can't
    easily assert "the SAME ro_snapshot stays stable" through the
    public Db API.  What we CAN verify here is the integration-level
    monotonic-non-decreasing property: even under concurrent writes,
    a reader fiber's successive observations never go backwards.  This
    is the same property `test_multifiber_stress.ml` already verifies
    for the Mem backend; we add it for the WAL backend. *)

module Db = Sqlocaml.Db
let run = Lwt_main.run

let setup () =
  let path = "/tmp/sqlocaml_phase38_wal_snap.db" in
  (try Unix.unlink path with _ -> ());
  (try Unix.unlink (path ^ "-wal") with _ -> ());
  let db = match run (Db.open_file_wal ~path) with
    | Ok d -> d
    | Error _ -> Alcotest.fail "open_file_wal failed"
  in
  db, path

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error _ -> Alcotest.failf "exec failed: %s" sql

let count_lwt db sql =
  let open Lwt.Infix in
  Db.query db sql >>= function
  | Error _ -> Alcotest.failf "query failed: %s" sql
  | Ok stream ->
    Lwt_stream.to_list stream >|= List.length

let test_concurrent_reader_sees_monotonic_count () =
  let db, path = setup () in
  exec db "CREATE TABLE t (n INTEGER)";
  for i = 0 to 9 do
    exec db (Printf.sprintf "INSERT INTO t VALUES (%d)" i)
  done;
  let open Lwt.Infix in
  let writer =
    let rec loop i =
      if i >= 40 then Lwt.return ()
      else
        Db.execute db (Printf.sprintf "INSERT INTO t VALUES (%d)" (100+i))
        >>= function
        | Ok () -> Lwt.pause () >>= fun () -> loop (i+1)
        | Error _ -> Alcotest.failf "writer failed at i=%d" i
    in
    loop 0
  in
  let reader =
    let rec loop i seen =
      if i >= 20 then Lwt.return seen
      else
        count_lwt db "SELECT n FROM t" >>= fun c ->
        if c < seen then Alcotest.failf "reader regress %d -> %d" seen c;
        Lwt.pause () >>= fun () -> loop (i+1) c
    in
    loop 0 0
  in
  let _ = run (Lwt.both writer reader) in
  let final = run (count_lwt db "SELECT n FROM t") in
  Alcotest.(check int) "final" 50 final;
  run (Db.close db);
  (try Unix.unlink path with _ -> ());
  (try Unix.unlink (path ^ "-wal") with _ -> ())

let () =
  Alcotest.run "wal_reader_snapshot" [
    "snapshot", [
      Alcotest.test_case "concurrent reader sees monotonic count"
        `Slow test_concurrent_reader_sees_monotonic_count;
    ]
  ]
