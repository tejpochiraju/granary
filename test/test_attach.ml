(** Phase 40 / #64 — multi-database ATTACH/DETACH tests.

    Validates the sqlocaml flavour of multi-database support:
    [ATTACH DATABASE 'path' AS schema], [DETACH DATABASE schema], and the
    [PRAGMA database_list] / [PRAGMA active_database] routing surface.

    Notes on semantics tested here:
    - Top-level handle holds the attached map; sub-handles never route.
    - [PRAGMA active_database = name] switches subsequent statements to
      the named schema; "main" restores routing to the top-level store.
    - DETACH closes the sub-handle and forces [active_database] back to
      "main" when the detached schema was the active one. *)

module Db = Sqlocaml.Db

(* ATTACH opens a file-backed sub-db, so the engine needs the Unix file
   provider (the parent here is in-memory, so opening it does not install
   it automatically). *)
let () = Sqlocaml_unix.install ()
let run = Lwt_main.run

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec failed: %s — %a" sql Db.pp_error e
;;

let exec_err db sql =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "expected error from: %s" sql
  | Error _ -> ()
;;

let collect_query db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query failed: %s — %a" sql Db.pp_error e
  | Ok stream -> run (Lwt_stream.to_list stream)
;;

let count_rows db sql = List.length (collect_query db sql)

let single_int db sql =
  match collect_query db sql with
  | [ row ] ->
    (match row.(0) with
     | Db.V_int n -> Int64.to_int n
     | _ -> -1)
  | _ -> -1
;;

let single_text db sql =
  match collect_query db sql with
  | [ row ] ->
    (match row.(0) with
     | Db.V_text s -> s
     | _ -> "")
  | _ -> ""
;;

let scratch_file suffix =
  let path = Printf.sprintf "/tmp/sqlocaml_phase40_%s_%d.db" suffix (Unix.getpid ()) in
  (try Unix.unlink path with
   | _ -> ());
  (try Unix.unlink (path ^ "-wal") with
   | _ -> ());
  path
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

(* ------------------------------------------------------------------ *)
(* Lifecycle: ATTACH adds, DETACH removes, database_list reflects it.  *)
(* ------------------------------------------------------------------ *)

let test_attach_detach_lifecycle () =
  let aux = scratch_file "lifecycle" in
  let db = run (Db.open_in_memory ()) in
  Alcotest.(check int) "no schemas before attach" 1 (count_rows db "PRAGMA database_list");
  exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux);
  Alcotest.(check int) "two schemas after attach" 2 (count_rows db "PRAGMA database_list");
  Alcotest.(check string)
    "active is main by default"
    "main"
    (single_text db "PRAGMA active_database");
  exec db "DETACH DATABASE aux";
  Alcotest.(check int) "one schema after detach" 1 (count_rows db "PRAGMA database_list");
  run (Db.close db);
  cleanup aux
;;

(* ------------------------------------------------------------------ *)
(* Isolation: rows in aux are NOT visible from main, and vice versa.   *)
(* ------------------------------------------------------------------ *)

let test_isolation_between_schemas () =
  let aux = scratch_file "isolation" in
  let db = run (Db.open_in_memory ()) in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1), (2), (3)";
  exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux);
  exec db "PRAGMA active_database = 'aux'";
  Alcotest.(check string)
    "switched to aux"
    "aux"
    (single_text db "PRAGMA active_database");
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (10), (20)";
  Alcotest.(check int) "aux has 2 rows" 2 (single_int db "SELECT COUNT(*) FROM t");
  exec db "PRAGMA active_database = 'main'";
  Alcotest.(check int) "main still has 3 rows" 3 (single_int db "SELECT COUNT(*) FROM t");
  Alcotest.(check int) "main sum is 6" 6 (single_int db "SELECT SUM(n) FROM t");
  (* Switch back to aux and verify its data is intact. *)
  exec db "PRAGMA active_database = 'aux'";
  Alcotest.(check int) "aux sum is 30" 30 (single_int db "SELECT SUM(n) FROM t");
  run (Db.close db);
  cleanup aux
;;

(* ------------------------------------------------------------------ *)
(* database_list shape: each row is (seq, name, file).                 *)
(* ------------------------------------------------------------------ *)

let test_database_list_shape () =
  let aux = scratch_file "shape" in
  let db = run (Db.open_in_memory ()) in
  exec db (Printf.sprintf "ATTACH DATABASE '%s' AS extra" aux);
  let rows = collect_query db "PRAGMA database_list" in
  Alcotest.(check int) "two schemas" 2 (List.length rows);
  let names =
    List.map
      (fun row ->
         match row.(1) with
         | Db.V_text s -> s
         | _ -> "?")
      rows
  in
  Alcotest.(check bool) "main present" true (List.mem "main" names);
  Alcotest.(check bool) "extra present" true (List.mem "extra" names);
  (* File path on the attached row matches what we passed in. *)
  List.iter
    (fun row ->
       match row.(1), row.(2) with
       | Db.V_text "extra", Db.V_text path -> Alcotest.(check string) "aux path" aux path
       | _ -> ())
    rows;
  run (Db.close db);
  cleanup aux
;;

(* ------------------------------------------------------------------ *)
(* Error cases: duplicate attach, detach unknown, detach main,         *)
(* switch to unknown.                                                  *)
(* ------------------------------------------------------------------ *)

let test_error_cases () =
  let aux = scratch_file "errors" in
  let db = run (Db.open_in_memory ()) in
  exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux);
  exec_err db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux);
  exec_err db "ATTACH DATABASE '/tmp/whatever.db' AS main";
  exec_err db "DETACH DATABASE main";
  exec_err db "DETACH DATABASE nonexistent";
  exec_err db "PRAGMA active_database = 'nonexistent'";
  run (Db.close db);
  cleanup aux
;;

(* ------------------------------------------------------------------ *)
(* DETACH while attached schema is active resets routing back to main. *)
(* ------------------------------------------------------------------ *)

let test_detach_active_resets () =
  let aux = scratch_file "detach_reset" in
  let db = run (Db.open_in_memory ()) in
  exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux);
  exec db "PRAGMA active_database = 'aux'";
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "DETACH DATABASE aux";
  Alcotest.(check string)
    "active back to main after detach"
    "main"
    (single_text db "PRAGMA active_database");
  run (Db.close db);
  cleanup aux
;;

(* ------------------------------------------------------------------ *)
(* Persistence: data written to an attached file survives DETACH +     *)
(* re-ATTACH within the same connection.                               *)
(* ------------------------------------------------------------------ *)

let test_persistence_across_detach () =
  let aux = scratch_file "persist" in
  let db = run (Db.open_in_memory ()) in
  exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux);
  exec db "PRAGMA active_database = 'aux'";
  exec db "CREATE TABLE notes (id INTEGER PRIMARY KEY, body TEXT)";
  exec db "INSERT INTO notes (id, body) VALUES (1, 'first')";
  exec db "INSERT INTO notes (id, body) VALUES (2, 'second')";
  exec db "PRAGMA active_database = 'main'";
  exec db "DETACH DATABASE aux";
  exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux2" aux);
  exec db "PRAGMA active_database = 'aux2'";
  Alcotest.(check int)
    "rows survive detach/reattach"
    2
    (single_int db "SELECT COUNT(*) FROM notes");
  Alcotest.(check string)
    "body persisted"
    "first"
    (single_text db "SELECT body FROM notes WHERE id = 1");
  run (Db.close db);
  cleanup aux
;;

(* ------------------------------------------------------------------ *)
(* Per-schema transactions: BEGIN/COMMIT scoped to the active schema.  *)
(* ------------------------------------------------------------------ *)

let test_per_schema_transactions () =
  let aux = scratch_file "txn" in
  let db = run (Db.open_in_memory ()) in
  exec db "CREATE TABLE m (n INTEGER)";
  exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux);
  exec db "PRAGMA active_database = 'aux'";
  exec db "CREATE TABLE a (n INTEGER)";
  exec db "BEGIN";
  exec db "INSERT INTO a (n) VALUES (100)";
  (* Roll back the aux txn — should not affect main. *)
  exec db "ROLLBACK";
  Alcotest.(check int) "aux rollback observed" 0 (single_int db "SELECT COUNT(*) FROM a");
  exec db "PRAGMA active_database = 'main'";
  exec db "INSERT INTO m (n) VALUES (1)";
  Alcotest.(check int) "main unaffected" 1 (single_int db "SELECT COUNT(*) FROM m");
  run (Db.close db);
  cleanup aux
;;

let () =
  Alcotest.run
    "attach"
    [ ( "lifecycle"
      , [ Alcotest.test_case
            "attach/detach + database_list"
            `Quick
            test_attach_detach_lifecycle
        ; Alcotest.test_case
            "isolation between schemas"
            `Quick
            test_isolation_between_schemas
        ; Alcotest.test_case "database_list shape" `Quick test_database_list_shape
        ; Alcotest.test_case "error cases" `Quick test_error_cases
        ; Alcotest.test_case
            "detach active schema resets routing"
            `Quick
            test_detach_active_resets
        ; Alcotest.test_case
            "persistence across detach + reattach"
            `Quick
            test_persistence_across_detach
        ; Alcotest.test_case "per-schema transactions" `Quick test_per_schema_transactions
        ] )
    ]
;;
