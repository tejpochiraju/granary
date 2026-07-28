(** #223 — concurrent auto-commit read-modify-write must not lose updates.

    N fibers share one [Db.t] (WAL backend) and each issues M
    [UPDATE c SET n = n + 1] statements in auto-commit.  The increment is
    server-side atomic, so every statement that returns [Ok] is one committed
    +1; starting from 0 the final value MUST equal N*M.

    Before the fix, [execute_update]/[execute_delete] in [Auto] mode read the
    matching rows through a *separate* RO snapshot and only then acquired the
    write lock, computing the new row from the stale snapshot value.  On the
    WAL backend the commit path yields (fsync), so a second fiber could drain a
    stale row between another fiber's RO read and its write-lock acquisition and
    clobber a committed increment — a lost update.  These tests pin that the
    read-modify-write is atomic under the write lock. *)

module Db = struct
  include Granary.Db

  let open_file_wal = Granary_unix.open_file_wal
  let open_file = Granary_unix.open_file
end

let run = Lwt_main.run

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec failed (%s): %a" sql Db.pp_error e
;;

let exec_lwt db sql =
  let open Lwt.Infix in
  Db.execute db sql
  >|= function
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec_lwt failed (%s): %a" sql Db.pp_error e
;;

let select_int db sql =
  let open Lwt.Infix in
  run
    (Db.query db sql
     >>= function
     | Error e -> Alcotest.failf "query failed (%s): %a" sql Db.pp_error e
     | Ok stream ->
       Lwt_stream.to_list stream
       >|= (function
        | [| Db.V_int n |] :: _ -> Int64.to_int n
        | _ -> Alcotest.failf "unexpected result for %s" sql))
;;

(* Run a statement that returns exactly one row of one int (e.g.
   [UPDATE ... RETURNING n]) as an Lwt promise, for use inside worker fibers. *)
let query_int_lwt db sql =
  let open Lwt.Infix in
  Db.query db sql
  >>= function
  | Error e -> Alcotest.failf "query failed (%s): %a" sql Db.pp_error e
  | Ok stream ->
    Lwt_stream.to_list stream
    >|= (function
     | [| Db.V_int n |] :: _ -> Int64.to_int n
     | _ -> Alcotest.failf "unexpected RETURNING result for %s" sql)
;;

(* Run a statement and collect the first int of every returned row (e.g. all
   keys from [DELETE ... RETURNING k], or a whole [SELECT k] column). *)
let query_ints_lwt db sql =
  let open Lwt.Infix in
  Db.query db sql
  >>= function
  | Error e -> Alcotest.failf "query failed (%s): %a" sql Db.pp_error e
  | Ok stream ->
    Lwt_stream.to_list stream
    >|= List.map (function
      | [| Db.V_int n |] -> Int64.to_int n
      | _ -> Alcotest.failf "unexpected row shape for %s" sql)
;;

(* Per-process temp path so parallel test runs don't collide (review nit). *)
let tmp name = Printf.sprintf "/tmp/granary_rmw223_%s_%d.db" name (Unix.getpid ())

let unlink p =
  (try Unix.unlink p with
   | _ -> ());
  try Unix.unlink (p ^ "-wal") with
  | _ -> ()
;;

(* N fibers, M increments each, sharing one handle on the given backend. *)
let run_concurrent_increments ~open_db ~path ~n_fibers ~m_each =
  unlink path;
  let db =
    match run (open_db ~path ()) with
    | Ok d -> d
    | Error e -> Alcotest.failf "open failed: %a" Db.pp_error e
  in
  exec db "CREATE TABLE c (k INTEGER PRIMARY KEY, n INTEGER)";
  exec db "INSERT INTO c (k, n) VALUES (0, 0)";
  let open Lwt.Infix in
  let worker () =
    let rec loop i =
      if i >= m_each
      then Lwt.return_unit
      else
        exec_lwt db "UPDATE c SET n = n + 1 WHERE k = 0"
        >>= fun () -> Lwt.pause () >>= fun () -> loop (i + 1)
    in
    loop 0
  in
  run (Lwt.join (List.init n_fibers (fun _ -> worker ())));
  let final = select_int db "SELECT n FROM c WHERE k = 0" in
  run (Db.close db);
  unlink path;
  final
;;

let test_wal_no_lost_increments () =
  let n_fibers = 4
  and m_each = 50 in
  let final =
    run_concurrent_increments
      ~open_db:(fun ~path () -> Db.open_file_wal ~path ())
      ~path:(tmp "rmw_wal")
      ~n_fibers
      ~m_each
  in
  Alcotest.(check int) "WAL: every acked increment persisted" (n_fibers * m_each) final
;;

let test_file_no_lost_increments () =
  let n_fibers = 4
  and m_each = 50 in
  let final =
    run_concurrent_increments
      ~open_db:(fun ~path () -> Db.open_file ~path ())
      ~path:(tmp "rmw_file")
      ~n_fibers
      ~m_each
  in
  Alcotest.(check int) "file: every acked increment persisted" (n_fibers * m_each) final
;;

(* Shared single-row counter: 4 fibers each do 50 UPDATEs of one row. Final
   must equal the acked count (4*50). A second UPDATE-RMW shape on WAL. *)
let test_wal_shared_counter_exact () =
  let path = tmp "shared_counter" in
  unlink path;
  let db =
    match run (Db.open_file_wal ~path ()) with
    | Ok d -> d
    | Error e -> Alcotest.failf "open failed: %a" Db.pp_error e
  in
  exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, n INTEGER)";
  exec db "INSERT INTO t (k, n) VALUES (0, 0)";
  let n_fibers = 4
  and m_each = 50 in
  let open Lwt.Infix in
  let worker () =
    let rec loop i =
      if i >= m_each
      then Lwt.return_unit
      else
        exec_lwt db "UPDATE t SET n = n + 1 WHERE k = 0"
        >>= fun () -> Lwt.pause () >>= fun () -> loop (i + 1)
    in
    loop 0
  in
  run (Lwt.join (List.init n_fibers (fun _ -> worker ())));
  let final = select_int db "SELECT n FROM t WHERE k = 0" in
  run (Db.close db);
  unlink path;
  Alcotest.(check int) "WAL: shared counter exact" (n_fibers * m_each) final
;;

(* #223 + #226 (DELETE path) — concurrent DELETE ... RETURNING: every row must
   be deleted and returned by EXACTLY ONE fiber. 4 fibers race
   `DELETE ... WHERE tag = 0 RETURNING k`; the union of returned k must be
   exactly {0..total-1} with no duplicates.

   This exercises both DELETE-path fixes and fails pre-fix (verified by swapping
   in the pre-fix exec.ml):
   - #223: without the in-txn drain, a fiber deletes by rowid from a stale
     snapshot — apply_delete_row is a no-op for rows another fiber already
     removed, yet the row would still surface;
   - #226: without [collect], RETURNING is projected from that pre-lock snapshot,
     so several fibers return the same already-deleted rows (duplicates).
   With the fix the drain is under the write lock and RETURNING projects only the
   rows this fiber actually deleted, so the union is each row exactly once.

   (A dedicated "row moved out of the predicate mid-DELETE survives" race proved
   non-deterministic under cooperative Lwt — it did not reproduce pre-fix — so it
   is intentionally not shipped as a flaky test; this no-dupes property covers
   the same execute_delete in-txn-drain path reliably.) *)
let test_wal_delete_returning_no_dupes () =
  let path = tmp "del_returning" in
  unlink path;
  let db =
    match run (Db.open_file_wal ~path ()) with
    | Ok d -> d
    | Error e -> Alcotest.failf "open failed: %a" Db.pp_error e
  in
  exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, tag INTEGER)";
  let total = 200 in
  for k = 0 to total - 1 do
    exec db (Printf.sprintf "INSERT INTO t (k, tag) VALUES (%d, 0)" k)
  done;
  let returned = ref [] in
  let open Lwt.Infix in
  let worker () =
    let rec loop i =
      if i >= 5
      then Lwt.return_unit
      else
        query_ints_lwt db "DELETE FROM t WHERE tag = 0 RETURNING k"
        >>= fun ks ->
        returned := ks @ !returned;
        Lwt.pause () >>= fun () -> loop (i + 1)
    in
    loop 0
  in
  run (Lwt.join (List.init 4 (fun _ -> worker ())));
  let got = List.sort compare !returned in
  let expected = List.init total (fun i -> i) in
  let remaining = select_int db "SELECT count(*) FROM t" in
  run (Db.close db);
  unlink path;
  Alcotest.(check int) "DELETE RETURNING: table fully drained" 0 remaining;
  Alcotest.(check (list int))
    "DELETE RETURNING: each row deleted+returned exactly once (no dupes)"
    expected
    got
;;

(* #226 — UPDATE ... RETURNING must read-from-the-write: under concurrency the
   multiset of returned values must be exactly {1, …, N*M} with no duplicates or
   gaps (each acked increment returns its own committed value).  Before the fix
   RETURNING was projected from a pre-lock RO snapshot, so concurrent callers
   could be handed the same/stale n even though the table ended correct. *)
let test_wal_returning_reads_from_write () =
  let path = tmp "upd_returning" in
  unlink path;
  let db =
    match run (Db.open_file_wal ~path ()) with
    | Ok d -> d
    | Error e -> Alcotest.failf "open failed: %a" Db.pp_error e
  in
  exec db "CREATE TABLE c (k INTEGER PRIMARY KEY, n INTEGER)";
  exec db "INSERT INTO c (k, n) VALUES (0, 0)";
  let n_fibers = 4
  and m_each = 50 in
  let returned = ref [] in
  let open Lwt.Infix in
  let worker () =
    let rec loop i =
      if i >= m_each
      then Lwt.return_unit
      else
        query_int_lwt db "UPDATE c SET n = n + 1 WHERE k = 0 RETURNING n"
        >>= fun v ->
        returned := v :: !returned;
        Lwt.pause () >>= fun () -> loop (i + 1)
    in
    loop 0
  in
  run (Lwt.join (List.init n_fibers (fun _ -> worker ())));
  let total = n_fibers * m_each in
  let got = List.sort compare !returned in
  let expected = List.init total (fun i -> i + 1) in
  let final = select_int db "SELECT n FROM c WHERE k = 0" in
  run (Db.close db);
  unlink path;
  Alcotest.(check int) "RETURNING: final counter" total final;
  Alcotest.(check (list int))
    "RETURNING: each value returned exactly once (no dupes/gaps)"
    expected
    got
;;

let () =
  Alcotest.run
    "concurrent_rmw_223"
    [ ( "lost-update"
      , [ Alcotest.test_case "WAL: no lost increments" `Slow test_wal_no_lost_increments
        ; Alcotest.test_case "file: no lost increments" `Slow test_file_no_lost_increments
        ; Alcotest.test_case
            "WAL: shared counter exact"
            `Slow
            test_wal_shared_counter_exact
        ; Alcotest.test_case
            "WAL: UPDATE RETURNING reads-from-write (#226)"
            `Slow
            test_wal_returning_reads_from_write
        ; Alcotest.test_case
            "WAL: concurrent DELETE RETURNING, each row once (#223/#226)"
            `Slow
            test_wal_delete_returning_no_dupes
        ] )
    ]
;;
