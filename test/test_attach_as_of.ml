(** #412 — as-of time travel against ATTACHed databases.  The top handle is a
    file db opened with [~as_of_history:true]; attached dbs inherit that setting
    and get their own [<path>.aslog].  Retention is per-schema. *)

module D = struct
  include Sqlocaml.Db

  let open_file = Sqlocaml_unix.open_file
end

module H = Sqlocaml_store.History
open Lwt.Syntax

let () = Sqlocaml_unix.install ()
let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_attach_as_of_%04d.db" n
;;

let cleanup path =
  List.iter
    (fun p ->
       try Unix.unlink p with
       | _ -> ())
    [ path; path ^ "-wal"; path ^ ".aslog" ]
;;

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected error: %a" D.pp_error e
;;

let exec db sql =
  let* r = D.execute db sql in
  (match r with
   | Ok () -> ()
   | Error e -> Alcotest.failf "exec failed (%s): %a" sql D.pp_error e);
  Lwt.return_unit
;;

(* head txn id of [schema]'s commit log after the latest commit. *)
let head_txn db ~schema =
  let* records = D.history_log ~schema db in
  match List.rev records with
  | last :: _ -> Lwt.return last.H.txn_id
  | [] -> Alcotest.failf "history log for %s is empty" schema
;;

let test_pin_default_schema () =
  let main_path = fresh_path () in
  cleanup main_path;
  Lwt.finalize
    (fun () ->
       let* db = D.open_file ~as_of_history:true ~path:main_path () in
       let db = ok db in
       let* _ = D.execute db "CREATE TABLE t(id INTEGER)" in
       let* t1 = head_txn db ~schema:"main" in
       D.history_pin db ~txn_id:t1;
       Alcotest.(check (option int64)) "floor pinned" (Some t1) (D.history_floor db);
       D.history_release db;
       Alcotest.(check (option int64)) "floor cleared" None (D.history_floor db);
       D.close db)
    (fun () ->
       cleanup main_path;
       Lwt.return_unit)
  |> run
;;

let test_unknown_schema_raises () =
  let main_path = fresh_path () in
  cleanup main_path;
  Lwt.finalize
    (fun () ->
       let* db = D.open_file ~as_of_history:true ~path:main_path () in
       let db = ok db in
       let raised f =
         match f () with
         | exception Invalid_argument _ -> true
         | _ -> false
       in
       Alcotest.(check bool)
         "pin unknown raises"
         true
         (raised (fun () -> D.history_pin ~schema:"nope" db ~txn_id:1L));
       Alcotest.(check bool)
         "floor unknown raises"
         true
         (raised (fun () -> ignore (D.history_floor ~schema:"nope" db)));
       Alcotest.(check bool)
         "release unknown raises"
         true
         (raised (fun () -> D.history_release ~schema:"nope" db));
       D.close db)
    (fun () ->
       cleanup main_path;
       Lwt.return_unit)
  |> run
;;

(* Attaching under a history-enabled top inherits as-of: writes to the attached
   db land in its own commit log. *)
let test_attach_inherits_history () =
  let main_path = fresh_path () in
  let aux_path = main_path ^ ".aux" in
  cleanup main_path;
  cleanup aux_path;
  Lwt.finalize
    (fun () ->
       let* db = D.open_file ~as_of_history:true ~path:main_path () in
       let db = ok db in
       let* () = exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux_path) in
       let* () = exec db "PRAGMA active_database = 'aux'" in
       let* () = exec db "CREATE TABLE t(id INTEGER)" in
       let* () = exec db "INSERT INTO t VALUES (1)" in
       let* log = D.history_log ~schema:"aux" db in
       Alcotest.(check bool) "aux log non-empty" true (log <> []);
       D.close db)
    (fun () ->
       cleanup main_path;
       cleanup aux_path;
       Lwt.return_unit)
  |> run
;;

let () =
  Alcotest.run
    "attach_as_of"
    [ ( "retention"
      , [ Alcotest.test_case "pin default schema" `Quick test_pin_default_schema
        ; Alcotest.test_case "unknown schema raises" `Quick test_unknown_schema_raises
        ] )
    ; ( "attach"
      , [ Alcotest.test_case "attach inherits history" `Quick test_attach_inherits_history
        ] )
    ]
;;
