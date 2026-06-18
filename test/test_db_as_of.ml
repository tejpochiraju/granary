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

let () =
  Alcotest.run
    "db_as_of"
    [ "query_as_of", [ Alcotest.test_case "historical vs live" `Quick test_query_as_of ] ]
;;
