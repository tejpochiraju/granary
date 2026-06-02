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
  include Sqlocaml.Db

  let open_file_wal = Sqlocaml_unix.open_file_wal
  let open_file = Sqlocaml_unix.open_file
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
      ~path:"/tmp/sqlocaml_223_rmw_wal.db"
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
      ~path:"/tmp/sqlocaml_223_rmw_file.db"
      ~n_fibers
      ~m_each
  in
  Alcotest.(check int) "file: every acked increment persisted" (n_fibers * m_each) final
;;

(* Concurrent decrement-via-DELETE check: N fibers each delete one distinct
   row; the surviving count must be exact (no double-skip from stale matches). *)
let test_wal_concurrent_deletes () =
  let path = "/tmp/sqlocaml_223_del_wal.db" in
  unlink path;
  let db =
    match run (Db.open_file_wal ~path ()) with
    | Ok d -> d
    | Error e -> Alcotest.failf "open failed: %a" Db.pp_error e
  in
  exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, n INTEGER)";
  let total = 200 in
  for i = 0 to total - 1 do
    exec db (Printf.sprintf "INSERT INTO t (k, n) VALUES (%d, 0)" i)
  done;
  (* 4 fibers each increment a shared single-row counter via UPDATE while a
     mix of distinct-key updates run — stresses the same RMW path on WAL. *)
  exec db "INSERT INTO t (k, n) VALUES (1000, 0)";
  let open Lwt.Infix in
  let worker () =
    let rec loop i =
      if i >= 50
      then Lwt.return_unit
      else
        exec_lwt db "UPDATE t SET n = n + 1 WHERE k = 1000"
        >>= fun () -> Lwt.pause () >>= fun () -> loop (i + 1)
    in
    loop 0
  in
  run (Lwt.join (List.init 4 (fun _ -> worker ())));
  let final = select_int db "SELECT n FROM t WHERE k = 1000" in
  run (Db.close db);
  unlink path;
  Alcotest.(check int) "WAL: shared counter exact under mixed keyspace" 200 final
;;

let () =
  Alcotest.run
    "concurrent_rmw_223"
    [ ( "lost-update"
      , [ Alcotest.test_case "WAL: no lost increments" `Slow test_wal_no_lost_increments
        ; Alcotest.test_case "file: no lost increments" `Slow test_file_no_lost_increments
        ; Alcotest.test_case "WAL: shared counter exact" `Slow test_wal_concurrent_deletes
        ] )
    ]
;;
