(** Phase 39 / #100 — Multi-fiber stress tests.

    Many concurrent reader fibers + one writer fiber against the same
    [Db.t].  Verifies that the Lwt-level concurrency model holds up under
    contention:

    - Readers never see torn rows (MVCC snapshot isolation).
    - The writer's [Lwt_mutex] serialization never deadlocks against
      readers.
    - No fiber raises an exception.
    - Final row count matches the writer's expectation.

    These are correctness tests, not benchmarks — the iteration counts
    are small enough to run in CI but large enough to expose obvious
    races. *)

module Db = struct
  include Granary.Db

  let open_file = Granary_unix.open_file
  let open_file_wal = Granary_unix.open_file_wal
end

let run = Lwt_main.run
let fresh_mem_db () = run (Db.open_in_memory ())

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error _ -> Alcotest.failf "exec failed: %s" sql
;;

let exec_lwt db sql =
  let open Lwt.Infix in
  Db.execute db sql
  >|= function
  | Ok () -> ()
  | Error _ -> Alcotest.failf "exec_lwt failed: %s" sql
;;

let query_count_lwt db sql =
  let open Lwt.Infix in
  Db.query db sql
  >>= function
  | Error _ -> Alcotest.failf "query failed: %s" sql
  | Ok stream -> Lwt_stream.to_list stream >|= fun rows -> List.length rows
;;

let select_first_int_lwt db sql =
  let open Lwt.Infix in
  Db.query db sql
  >>= function
  | Error _ -> Alcotest.failf "query failed: %s" sql
  | Ok stream ->
    Lwt_stream.to_list stream
    >|= (function
     | [] -> -1
     | row :: _ ->
       (match row.(0) with
        | Db.V_int n -> Int64.to_int n
        | _ -> -1))
;;

(** Multi-reader stress: N reader fibers each run M SELECTs while a
    writer fiber inserts rows.  Each reader counts rows; the count must
    monotonically grow (or stay equal) within a single reader's view —
    snapshots are not refreshed mid-query but successive queries see at
    least as many rows as the previous one. *)
let test_many_readers_one_writer () =
  let db = fresh_mem_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  let n_readers = 8 in
  let n_writes = 200 in
  let n_reads_each = 100 in
  let open Lwt.Infix in
  let writer =
    let rec loop i =
      if i >= n_writes
      then Lwt.return ()
      else
        exec_lwt db (Printf.sprintf "INSERT INTO t (n) VALUES (%d)" i)
        >>= fun () -> loop (i + 1)
    in
    loop 0
  in
  let make_reader _id =
    let rec loop seen i =
      if i >= n_reads_each
      then Lwt.return ()
      else
        query_count_lwt db "SELECT n FROM t"
        >>= fun c ->
        if c < seen then Alcotest.failf "reader saw count decrease: %d → %d" seen c;
        Lwt.pause () >>= fun () -> loop c (i + 1)
    in
    loop 0 0
  in
  let readers = List.init n_readers make_reader in
  run (Lwt.join (writer :: readers));
  let final = run (query_count_lwt db "SELECT n FROM t") in
  Alcotest.(check int) "all writes visible after join" n_writes final;
  run (Db.close db)
;;

(** Concurrent inserts inside a single fiber but interleaved with reader
    pauses.  Validates that [Lwt.pause]-driven cooperative scheduling
    inside the writer loop does not corrupt the index. *)
let test_writer_yields_between_inserts () =
  let db = fresh_mem_db () in
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, n INTEGER)";
  exec db "CREATE INDEX ix_t_n ON t (n)";
  let open Lwt.Infix in
  let n_writes = 300 in
  let writer =
    let rec loop i =
      if i >= n_writes
      then Lwt.return ()
      else
        exec_lwt db (Printf.sprintf "INSERT INTO t (id, n) VALUES (%d, %d)" i (i mod 50))
        >>= fun () -> Lwt.pause () >>= fun () -> loop (i + 1)
    in
    loop 0
  in
  let reader =
    let rec loop i acc =
      if i >= 50
      then Lwt.return acc
      else
        query_count_lwt db "SELECT id FROM t WHERE n = 7"
        >>= fun c -> Lwt.pause () >>= fun () -> loop (i + 1) (c :: acc)
    in
    loop 0 []
  in
  let counts = run (Lwt.both writer reader) |> snd in
  (* Each entry must be monotonic (counts only grow within the run). *)
  let rec mono = function
    | [] | [ _ ] -> true
    | a :: (b :: _ as r) -> a >= b && mono r
  in
  Alcotest.(check bool) "reader saw monotonic counts" true (mono counts);
  let final = run (query_count_lwt db "SELECT id FROM t WHERE n = 7") in
  let expected =
    List.length (List.filter (fun i -> i mod 50 = 7) (List.init n_writes Fun.id))
  in
  Alcotest.(check int) "final filtered count" expected final;
  run (Db.close db)
;;

(** Mixed workload: one writer alternating INSERT/UPDATE/DELETE while
    readers query an aggregate.  The aggregate must always be a valid
    int (no exceptions, no torn reads). *)
let test_mixed_writer_with_aggregating_readers () =
  let db = fresh_mem_db () in
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, n INTEGER)";
  for i = 0 to 49 do
    exec db (Printf.sprintf "INSERT INTO t (id, n) VALUES (%d, %d)" i (i * 2))
  done;
  let open Lwt.Infix in
  let n_iter = 150 in
  let writer =
    let rec loop i =
      if i >= n_iter
      then Lwt.return ()
      else (
        let op =
          match i mod 3 with
          | 0 -> Printf.sprintf "UPDATE t SET n = n + 1 WHERE id = %d" (i mod 50)
          | 1 -> Printf.sprintf "INSERT INTO t (id, n) VALUES (%d, %d)" (1000 + i) i
          | _ -> Printf.sprintf "DELETE FROM t WHERE id = %d" (1000 + (i - 2))
        in
        exec_lwt db op >>= fun () -> Lwt.pause () >>= fun () -> loop (i + 1))
    in
    loop 0
  in
  let n_readers = 4 in
  let make_reader _ =
    let rec loop i =
      if i >= 80
      then Lwt.return ()
      else
        select_first_int_lwt db "SELECT SUM(n) FROM t"
        >>= fun s ->
        if s < 0 then Alcotest.failf "reader got negative sum %d" s;
        Lwt.pause () >>= fun () -> loop (i + 1)
    in
    loop 0
  in
  run (Lwt.join (writer :: List.init n_readers make_reader));
  let final = run (select_first_int_lwt db "SELECT COUNT(*) FROM t") in
  Alcotest.(check bool) "final count nonneg" true (final >= 0);
  run (Db.close db)
;;

(** Stress the explicit BEGIN/COMMIT path: writer wraps a batch of
    inserts in a single transaction; readers query throughout via the
    same handle.

    A single [Db.t] is a single connection — the explicit_txn slot is
    shared across fibers, so readers running through the same handle do
    observe uncommitted state from the writer's open txn.  What we
    verify here:

    - Counts grow monotonically (no torn reads, no missed inserts).
    - The writer completes BEGIN/COMMIT without exception.
    - The final committed count is exactly the number of inserts. *)
let test_explicit_txn_isolation () =
  let db = fresh_mem_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  let open Lwt.Infix in
  let writer =
    exec_lwt db "BEGIN"
    >>= fun () ->
    let rec loop i =
      if i >= 30
      then Lwt.return ()
      else
        exec_lwt db (Printf.sprintf "INSERT INTO t (n) VALUES (%d)" i)
        >>= fun () -> Lwt.pause () >>= fun () -> loop (i + 1)
    in
    loop 0 >>= fun () -> exec_lwt db "COMMIT"
  in
  let counts = ref [] in
  let reader =
    let rec loop i =
      if i >= 30
      then Lwt.return ()
      else
        query_count_lwt db "SELECT n FROM t"
        >>= fun c ->
        counts := c :: !counts;
        Lwt.pause () >>= fun () -> loop (i + 1)
    in
    loop 0
  in
  run (Lwt.join [ writer; reader ]);
  let observed = List.rev !counts in
  (* Counts must be monotonically non-decreasing. *)
  let rec mono prev = function
    | [] -> ()
    | c :: rest ->
      if c < prev then Alcotest.failf "reader saw count regress: %d → %d" prev c;
      mono c rest
  in
  mono 0 observed;
  let final = run (query_count_lwt db "SELECT n FROM t") in
  Alcotest.(check int) "final count after commit" 30 final;
  run (Db.close db)
;;

(** WAL-backed multi-fiber stress: 8 reader fibers each run 100 SELECTs
    while a writer fiber inserts 200 rows through [Db.open_file_wal].
    Exercises the snapshot-isolation path: each reader count must be
    monotonically non-decreasing across successive queries. *)
let test_wal_backend_concurrent_readers_writer () =
  let path = "/tmp/granary_phase38_wal_mfs.db" in
  (try Unix.unlink path with
   | _ -> ());
  (try Unix.unlink (path ^ "-wal") with
   | _ -> ());
  let db =
    match run (Db.open_file_wal ~path ()) with
    | Ok d -> d
    | Error _ -> Alcotest.fail "open_file_wal failed"
  in
  exec db "CREATE TABLE t (n INTEGER)";
  let n_readers = 8 in
  let n_writes = 200 in
  let n_reads_each = 100 in
  let open Lwt.Infix in
  let writer =
    let rec loop i =
      if i >= n_writes
      then Lwt.return ()
      else
        exec_lwt db (Printf.sprintf "INSERT INTO t (n) VALUES (%d)" i)
        >>= fun () -> loop (i + 1)
    in
    loop 0
  in
  let make_reader _ =
    let rec loop seen i =
      if i >= n_reads_each
      then Lwt.return ()
      else
        query_count_lwt db "SELECT n FROM t"
        >>= fun c ->
        if c < seen then Alcotest.failf "WAL reader regress %d -> %d" seen c;
        Lwt.pause () >>= fun () -> loop c (i + 1)
    in
    loop 0 0
  in
  run (Lwt.join (writer :: List.init n_readers make_reader));
  let final = run (query_count_lwt db "SELECT n FROM t") in
  Alcotest.(check int) "WAL final count" n_writes final;
  run (Db.close db);
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

(** File-backed multi-fiber stress: same as the in-memory version but
    on the persistent backend.  Exercises the pager + WAL-less codepath
    under concurrency. *)
let test_file_backend_concurrent () =
  let path = "/tmp/granary_phase39_mfs.db" in
  (try Unix.unlink path with
   | _ -> ());
  let db =
    match run (Db.open_file ~path ()) with
    | Ok d -> d
    | Error _ -> Alcotest.fail "open_file failed"
  in
  exec db "CREATE TABLE t (n INTEGER)";
  let open Lwt.Infix in
  let n_writes = 100 in
  let writer =
    let rec loop i =
      if i >= n_writes
      then Lwt.return ()
      else
        exec_lwt db (Printf.sprintf "INSERT INTO t (n) VALUES (%d)" i)
        >>= fun () -> loop (i + 1)
    in
    loop 0
  in
  let reader =
    let rec loop i seen =
      if i >= 60
      then Lwt.return seen
      else
        query_count_lwt db "SELECT n FROM t"
        >>= fun c ->
        if c < seen
        then Alcotest.failf "file-backend reader saw count drop %d → %d" seen c;
        Lwt.pause () >>= fun () -> loop (i + 1) c
    in
    loop 0 0
  in
  ignore (run (Lwt.both writer reader));
  let final = run (query_count_lwt db "SELECT n FROM t") in
  Alcotest.(check int) "file-backend final count" n_writes final;
  run (Db.close db);
  try Unix.unlink path with
  | _ -> ()
;;

let () =
  Alcotest.run
    "multifiber_stress"
    [ ( "concurrency"
      , [ Alcotest.test_case
            "many readers + one writer"
            `Slow
            test_many_readers_one_writer
        ; Alcotest.test_case
            "writer yields between inserts"
            `Slow
            test_writer_yields_between_inserts
        ; Alcotest.test_case
            "mixed writer with aggregating readers"
            `Slow
            test_mixed_writer_with_aggregating_readers
        ; Alcotest.test_case
            "explicit txn isolation under concurrency"
            `Slow
            test_explicit_txn_isolation
        ; Alcotest.test_case "file backend concurrent" `Slow test_file_backend_concurrent
        ; Alcotest.test_case
            "WAL backend concurrent readers + writer"
            `Slow
            test_wal_backend_concurrent_readers_writer
        ] )
    ]
;;
