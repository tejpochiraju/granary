(** Phase 38 / #149 — RO snapshot reads under concurrent WAL writes.

    Two layers of guarantee are exercised here:

    - [test_concurrent_reader_sees_monotonic_count] is the
      integration-level property through the [Db] API.  Because each
      [Db.query] opens its own ro_snapshot internally, the property
      it can directly verify is monotonic-non-decreasing across
      successive queries, not within-snapshot stability.

    - [test_within_snapshot_stable_count] (added for #154) reaches
      one layer down to [Sqlocaml_store.Store] and holds a single
      [ro_begin] handle while a writer fiber commits new rows.  Every
      cursor walk on that handle must return the count captured at
      [ro_begin] time — this is the actual snapshot-isolation
      invariant the #149 plumbing delivers. *)

module Db = Sqlocaml.Db
module S = Sqlocaml_store.Store
let run = Lwt_main.run
let bs = Bytes.of_string

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

(* ---------- #154: within-snapshot stable count under writes ---------- *)

(* Walk every (k, v) under [tid] visible to the snapshot/txn [tx] and
   return how many rows we saw.  Reads route through
   [Pager.read ~snapshot_frames] for RO transactions; if that bound is
   ignored anywhere, we will observe rows the writer appended after
   [ro_begin]. *)
let count_via_cursor : type a. a S.txn -> S.tree_id -> int Lwt.t =
 fun tx tid ->
  let open Lwt.Syntax in
  let* cur = S.cursor_open tx tid in
  let _ = S.cursor_first cur in
  let rec loop n =
    match S.cursor_next cur with
    | None -> n
    | Some _ -> loop (n + 1)
  in
  let n = loop 0 in
  S.cursor_close cur;
  Lwt.return n

let test_within_snapshot_stable_count () =
  let open Lwt.Syntax in
  let tid = 16 in
  let n_seed = 20 in
  let n_extra = 15 in
  let path = "/tmp/sqlocaml_phase38_snap_stable.db" in
  (try Unix.unlink path with _ -> ());
  (try Unix.unlink (path ^ "-wal") with _ -> ());
  run (
    let* sr = S.open_file_wal ~path in
    let st = match sr with
      | Ok s -> s
      | Error e -> Alcotest.failf "open_file_wal: %a" S.pp_error e
    in
    (* Seed. *)
    let* tx = S.rw_begin st in
    let rec seed i =
      if i >= n_seed then Lwt.return_unit
      else
        let* () =
          S.put tx tid
            (bs (Printf.sprintf "k%04d" i))
            (bs (Printf.sprintf "v%04d" i))
        in
        seed (i + 1)
    in
    let* () = seed 0 in
    let* () = S.commit tx in

    (* One ro_snapshot held across the entire writer batch. *)
    let* ro = S.ro_begin st in
    let* initial = count_via_cursor ro tid in
    Alcotest.(check int) "snapshot at ro_begin sees seed" n_seed initial;

    (* Writer fiber appends [n_extra] rows, one row per commit, yielding
       between commits so the reader fiber gets a chance to interleave. *)
    let writer =
      let rec loop i =
        if i >= n_extra then Lwt.return_unit
        else
          let* tx = S.rw_begin st in
          let* () =
            S.put tx tid
              (bs (Printf.sprintf "x%04d" i))
              (bs (Printf.sprintf "y%04d" i))
          in
          let* () = S.commit tx in
          let* () = Lwt.pause () in
          loop (i + 1)
      in
      loop 0
    in

    (* Reader fiber re-walks the SAME ro_snapshot repeatedly.  Every walk
       must observe exactly [n_seed] rows; any deviation means the snapshot
       isolation plumbing is broken. *)
    let reader =
      let rec loop i =
        if i >= 20 then Lwt.return_unit
        else
          let* () = Lwt.pause () in
          let* c = count_via_cursor ro tid in
          if c <> n_seed then
            Alcotest.failf
              "within-snapshot regression: walk %d saw %d rows, expected %d"
              i c n_seed;
          loop (i + 1)
      in
      loop 0
    in
    let* () =
      let* (), () = Lwt.both writer reader in
      Lwt.return_unit
    in

    (* And once more after the writer is fully drained. *)
    let* after = count_via_cursor ro tid in
    Alcotest.(check int) "snapshot stable post-writer" n_seed after;
    let* () = S.ro_end ro in

    (* A fresh snapshot must now see every committed row. *)
    let* ro2 = S.ro_begin st in
    let* fresh = count_via_cursor ro2 tid in
    Alcotest.(check int) "fresh snapshot sees all writes"
      (n_seed + n_extra) fresh;
    let* () = S.ro_end ro2 in

    let* () = S.close st in
    Lwt.return_unit);
  (try Unix.unlink path with _ -> ());
  (try Unix.unlink (path ^ "-wal") with _ -> ())

let () =
  Alcotest.run "wal_reader_snapshot" [
    "snapshot", [
      Alcotest.test_case "concurrent reader sees monotonic count"
        `Slow test_concurrent_reader_sees_monotonic_count;
      Alcotest.test_case "within-snapshot stable count under writes"
        `Quick test_within_snapshot_stable_count;
    ]
  ]
