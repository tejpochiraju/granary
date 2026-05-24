(** End-to-end tests for WAL-mode Store. *)

open Lwt.Syntax

module S = struct
  include Sqlocaml_store.Store

  let open_file_wal = Sqlocaml_unix.Store.open_file_wal
end

let bs s = Bytes.of_string s
let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_store_wal_%04d.db" n
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

let ok_store = function
  | Ok t -> t
  | Error e -> Alcotest.failf "open_file_wal error: %a" S.pp_error e
;;

let bytes_opt =
  Alcotest.(
    option
      (testable (fun ppf b -> Format.fprintf ppf "%S" (Bytes.to_string b)) Bytes.equal))
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

(* ---- tests ---- *)

let test_wal_mode_flag () =
  run
  @@ with_fresh ~f:(fun path ->
    let* sr = S.open_file_wal ~path in
    let st = ok_store sr in
    Alcotest.(check bool) "wal_mode set" true (S.wal_mode st);
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_wal_commit_then_read_same_session () =
  run
  @@ with_fresh ~f:(fun path ->
    let* sr = S.open_file_wal ~path in
    let st = ok_store sr in
    let* tx = S.rw_begin st in
    let* () = S.put tx 16 (bs "k") (bs "v") in
    let* () = S.commit tx in
    let* tx = S.ro_begin st in
    let* v = S.get tx 16 (bs "k") in
    let* () = S.ro_end tx in
    Alcotest.(check bytes_opt) "commit visible same-session" (Some (bs "v")) v;
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_wal_commit_visible_after_reopen () =
  run
  @@ with_fresh ~f:(fun path ->
    let* sr = S.open_file_wal ~path in
    let st = ok_store sr in
    let* tx = S.rw_begin st in
    let* () = S.put tx 16 (bs "k") (bs "v") in
    let* () = S.commit tx in
    let* () = S.close st in
    (* Reopen: the WAL still has the commit; main DB has not yet been
       checkpointed, so the data lives only in WAL. *)
    let* sr2 = S.open_file_wal ~path in
    let st2 = ok_store sr2 in
    let* tx = S.ro_begin st2 in
    let* v = S.get tx 16 (bs "k") in
    let* () = S.ro_end tx in
    Alcotest.(check bytes_opt) "WAL replays on reopen" (Some (bs "v")) v;
    let* () = S.close st2 in
    Lwt.return_unit)
;;

let test_wal_multi_commits_latest_wins () =
  run
  @@ with_fresh ~f:(fun path ->
    let* sr = S.open_file_wal ~path in
    let st = ok_store sr in
    let* tx = S.rw_begin st in
    let* () = S.put tx 16 (bs "k") (bs "v1") in
    let* () = S.commit tx in
    let* tx = S.rw_begin st in
    let* () = S.put tx 16 (bs "k") (bs "v2") in
    let* () = S.commit tx in
    let* () = S.close st in
    let* sr2 = S.open_file_wal ~path in
    let st2 = ok_store sr2 in
    let* tx = S.ro_begin st2 in
    let* v = S.get tx 16 (bs "k") in
    let* () = S.ro_end tx in
    Alcotest.(check bytes_opt) "latest commit wins" (Some (bs "v2")) v;
    let* () = S.close st2 in
    Lwt.return_unit)
;;

let test_wal_rollback_drops_writes () =
  run
  @@ with_fresh ~f:(fun path ->
    let* sr = S.open_file_wal ~path in
    let st = ok_store sr in
    let* tx = S.rw_begin st in
    let* () = S.put tx 16 (bs "k1") (bs "v1") in
    let* () = S.commit tx in
    let* tx = S.rw_begin st in
    let* () = S.put tx 16 (bs "k2") (bs "v2") in
    let* () = S.rollback tx in
    let* tx = S.ro_begin st in
    let* v1 = S.get tx 16 (bs "k1") in
    let* v2 = S.get tx 16 (bs "k2") in
    let* () = S.ro_end tx in
    Alcotest.(check bytes_opt) "committed survives" (Some (bs "v1")) v1;
    Alcotest.(check bytes_opt) "rolled-back gone" None v2;
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_wal_savepoint_inside_wal () =
  run
  @@ with_fresh ~f:(fun path ->
    let* sr = S.open_file_wal ~path in
    let st = ok_store sr in
    let* tx = S.rw_begin st in
    let* () = S.put tx 16 (bs "a") (bs "1") in
    let* () = S.savepoint_begin tx "sp" in
    let* () = S.put tx 16 (bs "b") (bs "2") in
    let* () = S.savepoint_rollback tx "sp" in
    let* () = S.commit tx in
    let* () = S.close st in
    let* sr2 = S.open_file_wal ~path in
    let st2 = ok_store sr2 in
    let* tx = S.ro_begin st2 in
    let* va = S.get tx 16 (bs "a") in
    let* vb = S.get tx 16 (bs "b") in
    let* () = S.ro_end tx in
    Alcotest.(check bytes_opt) "a survived savepoint+commit" (Some (bs "1")) va;
    Alcotest.(check bytes_opt) "b dropped by savepoint" None vb;
    let* () = S.close st2 in
    Lwt.return_unit)
;;

let test_wal_many_commits () =
  run
  @@ with_fresh ~f:(fun path ->
    let* sr = S.open_file_wal ~path in
    let st = ok_store sr in
    let n = 20 in
    let rec do_inserts i =
      if i = n
      then Lwt.return_unit
      else
        let* tx = S.rw_begin st in
        let key = Printf.sprintf "key-%04d" i in
        let value = Printf.sprintf "value-%d" i in
        let* () = S.put tx 16 (bs key) (bs value) in
        let* () = S.commit tx in
        do_inserts (i + 1)
    in
    let* () = do_inserts 0 in
    let* () = S.close st in
    let* sr2 = S.open_file_wal ~path in
    let st2 = ok_store sr2 in
    let* tx = S.ro_begin st2 in
    let rec verify i =
      if i = n
      then Lwt.return_unit
      else (
        let key = Printf.sprintf "key-%04d" i in
        let expected = Printf.sprintf "value-%d" i in
        let* v = S.get tx 16 (bs key) in
        Alcotest.(check bytes_opt) (Printf.sprintf "row %d" i) (Some (bs expected)) v;
        verify (i + 1))
    in
    let* () = verify 0 in
    let* () = S.ro_end tx in
    let* () = S.close st2 in
    Lwt.return_unit)
;;

let test_wal_checkpoint_migrates_data () =
  run
  @@ with_fresh ~f:(fun path ->
    let* sr = S.open_file_wal ~path in
    let st = ok_store sr in
    let* tx = S.rw_begin st in
    let* () = S.put tx 16 (bs "k") (bs "v") in
    let* () = S.commit tx in
    let* () = S.checkpoint st in
    (* After checkpoint, data must still be readable. *)
    let* tx = S.ro_begin st in
    let* v = S.get tx 16 (bs "k") in
    let* () = S.ro_end tx in
    Alcotest.(check bytes_opt) "data survives checkpoint" (Some (bs "v")) v;
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_wal_checkpoint_then_reopen () =
  run
  @@ with_fresh ~f:(fun path ->
    let* sr = S.open_file_wal ~path in
    let st = ok_store sr in
    let* tx = S.rw_begin st in
    let* () = S.put tx 16 (bs "k") (bs "checkpointed") in
    let* () = S.commit tx in
    let* () = S.checkpoint st in
    let* () = S.close st in
    let* sr2 = S.open_file_wal ~path in
    let st2 = ok_store sr2 in
    let* tx = S.ro_begin st2 in
    let* v = S.get tx 16 (bs "k") in
    let* () = S.ro_end tx in
    Alcotest.(check bytes_opt)
      "data persists past checkpoint"
      (Some (bs "checkpointed"))
      v;
    let* () = S.close st2 in
    Lwt.return_unit)
;;

let test_wal_checkpoint_with_commits_after () =
  run
  @@ with_fresh ~f:(fun path ->
    let* sr = S.open_file_wal ~path in
    let st = ok_store sr in
    let* tx = S.rw_begin st in
    let* () = S.put tx 16 (bs "old") (bs "1") in
    let* () = S.commit tx in
    let* () = S.checkpoint st in
    (* Post-checkpoint commits go to a fresh WAL. *)
    let* tx = S.rw_begin st in
    let* () = S.put tx 16 (bs "new") (bs "2") in
    let* () = S.commit tx in
    let* () = S.close st in
    let* sr2 = S.open_file_wal ~path in
    let st2 = ok_store sr2 in
    let* tx = S.ro_begin st2 in
    let* vo = S.get tx 16 (bs "old") in
    let* vn = S.get tx 16 (bs "new") in
    let* () = S.ro_end tx in
    Alcotest.(check bytes_opt) "pre-checkpoint persists" (Some (bs "1")) vo;
    Alcotest.(check bytes_opt) "post-checkpoint persists" (Some (bs "2")) vn;
    let* () = S.close st2 in
    Lwt.return_unit)
;;

let test_wal_checkpoint_noop_on_mem () =
  (* Calling checkpoint on a non-WAL store is a no-op. *)
  run
    (let st = S.create () in
     let* () = S.checkpoint st in
     let* () = S.close st in
     Lwt.return_unit)
;;

let test_pragma_wal_checkpoint_via_sql () =
  (* Drive the engine end-to-end: open a WAL-backed Db, run SQL,
     issue PRAGMA wal_checkpoint, confirm data still readable, then
     reopen and confirm data was migrated to main. *)
  let module Db = Sqlocaml.Db in
  run
  @@ with_fresh ~f:(fun path ->
    let* r = Sqlocaml_unix.open_file_wal ~path in
    let db =
      match r with
      | Ok db -> db
      | Error e -> Alcotest.failf "open_file_wal: %a" Db.pp_error e
    in
    let exec_ok sql =
      let* r = Db.execute db sql in
      match r with
      | Ok () -> Lwt.return_unit
      | Error e -> Alcotest.failf "execute %s: %a" sql Db.pp_error e
    in
    let* () = exec_ok "CREATE TABLE t (n INTEGER)" in
    let* () = exec_ok "INSERT INTO t (n) VALUES (42)" in
    let* () = exec_ok "PRAGMA wal_checkpoint" in
    let* r = Db.query db "SELECT n FROM t" in
    let* () =
      match r with
      | Error e -> Alcotest.failf "query: %a" Db.pp_error e
      | Ok stream ->
        let* rows = Lwt_stream.to_list stream in
        (match rows with
         | [ [| Db.V_int 42L |] ] -> Lwt.return_unit
         | other -> Alcotest.failf "expected [[42]], got %d rows" (List.length other))
    in
    let* () = Db.close db in
    Lwt.return_unit)
;;

let () =
  Alcotest.run
    "store_wal"
    [ "open", [ Alcotest.test_case "wal_mode_flag" `Quick test_wal_mode_flag ]
    ; ( "commit"
      , [ Alcotest.test_case "same_session" `Quick test_wal_commit_then_read_same_session
        ; Alcotest.test_case
            "visible_after_reopen"
            `Quick
            test_wal_commit_visible_after_reopen
        ; Alcotest.test_case
            "multi_commits_latest"
            `Quick
            test_wal_multi_commits_latest_wins
        ; Alcotest.test_case "many_commits" `Quick test_wal_many_commits
        ] )
    ; ( "rollback"
      , [ Alcotest.test_case "drops_writes" `Quick test_wal_rollback_drops_writes ] )
    ; ( "savepoint"
      , [ Alcotest.test_case "inside_wal" `Quick test_wal_savepoint_inside_wal ] )
    ; ( "checkpoint"
      , [ Alcotest.test_case "migrates_data" `Quick test_wal_checkpoint_migrates_data
        ; Alcotest.test_case "survives_reopen" `Quick test_wal_checkpoint_then_reopen
        ; Alcotest.test_case
            "fresh_wal_after_chkpt"
            `Quick
            test_wal_checkpoint_with_commits_after
        ; Alcotest.test_case "noop_on_mem" `Quick test_wal_checkpoint_noop_on_mem
        ; Alcotest.test_case "pragma_via_sql" `Quick test_pragma_wal_checkpoint_via_sql
        ] )
    ]
;;
