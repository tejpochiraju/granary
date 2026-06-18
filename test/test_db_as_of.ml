(** SQL-level time travel: [Db.query_as_of] reads the database as it existed at
    a past txn id, while a live [Db.query] sees the current state (#266). *)

module D = struct
  include Sqlocaml.Db

  let open_file = Sqlocaml_unix.open_file
end

module H = Sqlocaml_store.History
open Lwt.Syntax

let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_db_as_of_%04d.db" n
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  (try Unix.unlink (path ^ "-wal") with
   | _ -> ());
  try Unix.unlink (path ^ ".aslog") with
  | _ -> ()
;;

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected error: %a" D.pp_error e
;;

let texts rows =
  List.map
    (fun (row : D.row) ->
       match row.(0) with
       | D.V_text s -> s
       | _ -> Alcotest.fail "expected a TEXT value in column 0")
    rows
;;

let test_query_as_of () =
  let path = fresh_path () in
  cleanup path;
  let only_a, both =
    Lwt.finalize
      (fun () ->
         let* db = D.open_file ~as_of_history:true ~path () in
         let db = ok db in
         let* _ = D.execute db "CREATE TABLE t(id INTEGER, v TEXT)" in
         let* _ = D.execute db "INSERT INTO t VALUES (1,'a')" in
         (* Capture the head txn id AFTER inserting 'a' (and its DDL). *)
         let* log = D.history_log db in
         let t1 =
           match List.rev log with
           | last :: _ -> last.H.txn_id
           | [] -> Alcotest.fail "history log is empty after a commit"
         in
         let* _ = D.execute db "INSERT INTO t VALUES (2,'b')" in
         (* Pin the retention floor at [t1] so the historical root is retained. *)
         D.history_pin db ~txn_id:t1;
         (* Historical read: the database as of [t1] holds only 'a'. *)
         let* hist = D.query_as_of db (`Txn t1) "SELECT v FROM t" in
         let* hist_rows = Lwt_stream.to_list (ok hist) in
         (* Live read: the current database holds both 'a' and 'b'. *)
         let* live = D.query db "SELECT v FROM t" in
         let* live_rows = Lwt_stream.to_list (ok live) in
         let* () = D.close db in
         Lwt.return (texts hist_rows, List.sort compare (texts live_rows)))
      (fun () ->
         cleanup path;
         Lwt.return_unit)
    |> run
  in
  Alcotest.(check (list string)) "as-of t1 yields only 'a'" [ "a" ] only_a;
  Alcotest.(check (list string)) "live yields both 'a' and 'b'" [ "a"; "b" ] both
;;

(* Negative path: a Db opened WITHOUT [~as_of_history] cannot serve a historical
   read, so [query_as_of] resolves to [Error History_unavailable]. *)
let test_history_unavailable () =
  let path = fresh_path () in
  cleanup path;
  let result =
    Lwt.finalize
      (fun () ->
         let* db = D.open_file ~path () in
         let db = ok db in
         let* _ = D.execute db "CREATE TABLE t(id INTEGER)" in
         let* r = D.query_as_of db (`Txn 1L) "SELECT 1" in
         let* () = D.close db in
         Lwt.return r)
      (fun () ->
         cleanup path;
         Lwt.return_unit)
    |> run
  in
  match result with
  | Error D.History_unavailable -> ()
  | Error e -> Alcotest.failf "expected History_unavailable, got %a" D.pp_error e
  | Ok _ -> Alcotest.fail "expected History_unavailable, got Ok"
;;

(* Negative path: with history enabled and a non-empty log whose earliest txn is
   > 0, resolving [`Txn 0L] finds no record <= 0 → [Error History_pruned]. *)
let test_history_pruned () =
  let path = fresh_path () in
  cleanup path;
  let result =
    Lwt.finalize
      (fun () ->
         let* db = D.open_file ~as_of_history:true ~path () in
         let db = ok db in
         let* _ = D.execute db "CREATE TABLE t(id INTEGER)" in
         let* _ = D.execute db "INSERT INTO t VALUES (1)" in
         (* Log is now non-empty; its earliest txn id is > 0, so a [`Txn 0L]
            target resolves to no record → pruned. *)
         let* r = D.query_as_of db (`Txn 0L) "SELECT id FROM t" in
         let* () = D.close db in
         Lwt.return r)
      (fun () ->
         cleanup path;
         Lwt.return_unit)
    |> run
  in
  match result with
  | Error D.History_pruned -> ()
  | Error e -> Alcotest.failf "expected History_pruned, got %a" D.pp_error e
  | Ok _ -> Alcotest.fail "expected History_pruned, got Ok"
;;

(* #412: as-of now routes to whichever store the query targets.  When the active
   schema routes a query to an ATTACHed sub-handle with no retention floor pinned,
   [query_as_of] resolves to [History_pruned] (not a [Runtime] rejection). *)
let test_attached_rejected () =
  let path = fresh_path () in
  let aux = fresh_path () in
  cleanup path;
  cleanup aux;
  let result =
    Lwt.finalize
      (fun () ->
         let* db = D.open_file ~as_of_history:true ~path () in
         let db = ok db in
         let* _ = D.execute db "CREATE TABLE t(id INTEGER, v TEXT)" in
         let* _ = D.execute db "INSERT INTO t VALUES (1,'a')" in
         let* log = D.history_log db in
         let t1 =
           match List.rev log with
           | last :: _ -> last.H.txn_id
           | [] -> Alcotest.fail "history log is empty after a commit"
         in
         D.history_pin db ~txn_id:t1;
         let* _ = D.execute db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux) in
         let* _ = D.execute db "PRAGMA active_database = 'aux'" in
         let* _ = D.execute db "CREATE TABLE u(id INTEGER, v TEXT)" in
         (* Now active schema is 'aux'; an unqualified SELECT routes to the
            attached store.  No floor is pinned on aux → History_pruned. *)
         let* r = D.query_as_of db (`Txn t1) "SELECT v FROM u" in
         let* () = D.close db in
         Lwt.return r)
      (fun () ->
         cleanup path;
         cleanup aux;
         Lwt.return_unit)
    |> run
  in
  match result with
  | Error D.History_pruned -> ()
  | Error e ->
    Alcotest.failf "expected History_pruned (aux has no floor), got %a" D.pp_error e
  | Ok _ -> Alcotest.fail "expected History_pruned (aux has no floor), got Ok"
;;

(* #266 (review, BLOCKER): with as-of enabled but NO retention floor pinned,
   query_as_of must resolve to [Error History_pruned] — never serve a snapshot
   against recycled pages.  Insert 'a' (capture t1), insert more WITHOUT pinning,
   then read as of t1. *)
let test_unpinned_query_as_of_pruned () =
  let path = fresh_path () in
  cleanup path;
  let result =
    Lwt.finalize
      (fun () ->
         let* db = D.open_file ~as_of_history:true ~path () in
         let db = ok db in
         let* _ = D.execute db "CREATE TABLE t(id INTEGER, v TEXT)" in
         let* _ = D.execute db "INSERT INTO t VALUES (1,'a')" in
         let* log = D.history_log db in
         let t1 =
           match List.rev log with
           | last :: _ -> last.H.txn_id
           | [] -> Alcotest.fail "history log is empty after a commit"
         in
         (* more writes, NO history_pin *)
         let* _ = D.execute db "INSERT INTO t VALUES (2,'b')" in
         let* _ = D.execute db "INSERT INTO t VALUES (3,'c')" in
         let* r = D.query_as_of db (`Txn t1) "SELECT v FROM t" in
         let* () = D.close db in
         Lwt.return r)
      (fun () ->
         cleanup path;
         Lwt.return_unit)
    |> run
  in
  match result with
  | Error D.History_pruned -> ()
  | Error e -> Alcotest.failf "expected History_pruned (unpinned), got %a" D.pp_error e
  | Ok _ -> Alcotest.fail "expected History_pruned (unpinned), got Ok"
;;

let () =
  Alcotest.run
    "db_as_of"
    [ ( "query_as_of"
      , [ Alcotest.test_case "historical vs live" `Quick test_query_as_of
        ; Alcotest.test_case "history_unavailable" `Quick test_history_unavailable
        ; Alcotest.test_case "history_pruned" `Quick test_history_pruned
        ; Alcotest.test_case "attached_rejected" `Quick test_attached_rejected
        ; Alcotest.test_case "unpinned_pruned" `Quick test_unpinned_query_as_of_pruned
        ] )
    ]
;;
