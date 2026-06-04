(** #262: read-your-own-writes inside an explicit transaction.

    A read issued after [BEGIN] and a write — but before [COMMIT] — must observe
    the transaction's own uncommitted writes.  The executor's base scanners open
    a fresh RO snapshot which, by snapshot-isolation design, is blind to the
    active writer's in-flight mutations; so reads inside a [BEGIN … COMMIT] used
    to see the pre-[BEGIN] committed state instead of the just-written rows.

    This was originally reported (#262) as an [iter]-only gap fixable by
    threading the txn mode into [Db.iter].  It is not: [Db.query] had the exact
    same hole, because the base scanners ignored the mode entirely and always
    snapshotted.  These cases pin the property for BOTH the one-shot [query] path
    and the prepared-statement [iter] path, across the scan/index/rowid/agg/join
    read shapes, and confirm a [ROLLBACK] is still honoured. *)

open Lwt.Syntax
module Db = Sqlocaml.Db

let run = Lwt_main.run

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "db error: %a" Db.pp_error e
;;

let with_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

(* A file-backed (B-tree/pager) database, to exercise the store's RW read path
   for the on-disk backend — distinct from the in-memory shadow-map path. *)
let with_file_db f =
  let path = Filename.temp_file "sqlocaml_ryw_262" ".db" in
  let db =
    match run (Sqlocaml_unix.open_file ~path ()) with
    | Ok db -> db
    | Error e -> Alcotest.failf "open_file: %a" Db.pp_error e
  in
  Fun.protect
    ~finally:(fun () ->
      (try run (Db.close db) with
       | _ -> ());
      try Sys.remove path with
      | _ -> ())
    (fun () -> f db)
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

(* Drain [sql] via the one-shot [Db.query] path and return the row count. *)
let query_count db sql =
  run
    (let* r = Db.query db sql in
     let stream = unwrap r in
     let* rows = Lwt_stream.to_list stream in
     Lwt.return (List.length rows))
;;

(* Drain [sql] via the prepared-statement [Db.iter] path and return the count. *)
let iter_count db sql =
  run
    (let* sr = Db.prepare db sql in
     let st = unwrap sr in
     let* r = Db.iter st ~params:[] in
     let stream = unwrap r in
     let* rows = Lwt_stream.to_list stream in
     Lwt.return (List.length rows))
;;

(* Drain [sql] via [query] and return the single integer cell of the first row. *)
let query_int db sql =
  run
    (let* r = Db.query db sql in
     let stream = unwrap r in
     let* rows = Lwt_stream.to_list stream in
     match rows with
     | row :: _ ->
       (match row.(0) with
        | Db.V_int n -> Lwt.return (Int64.to_int n)
        | _ -> Alcotest.failf "%S: first cell not an int" sql)
     | [] -> Alcotest.failf "%S: no rows" sql)
;;

let check name expected got = Alcotest.(check int) name expected got

(* Both read paths see rows inserted earlier in the same open transaction. *)
let test_ryw_seq_scan () =
  with_db (fun db ->
    exec db "CREATE TABLE t (n INTEGER)";
    exec db "INSERT INTO t (n) VALUES (1)";
    exec db "BEGIN";
    exec db "INSERT INTO t (n) VALUES (2)";
    exec db "INSERT INTO t (n) VALUES (3)";
    check "in-txn query seq-scan" 3 (query_count db "SELECT n FROM t");
    check "in-txn iter seq-scan" 3 (iter_count db "SELECT n FROM t");
    exec db "COMMIT";
    check "post-commit query" 3 (query_count db "SELECT n FROM t");
    check "post-commit iter" 3 (iter_count db "SELECT n FROM t"))
;;

(* A filtered scan in-txn still sees (and filters over) the uncommitted rows. *)
let test_ryw_filter () =
  with_db (fun db ->
    exec db "CREATE TABLE t (n INTEGER, tag TEXT)";
    exec db "INSERT INTO t VALUES (1, 'old')";
    exec db "BEGIN";
    exec db "INSERT INTO t VALUES (2, 'new')";
    exec db "INSERT INTO t VALUES (3, 'new')";
    check "in-txn query filter" 2 (query_count db "SELECT n FROM t WHERE tag = 'new'");
    check "in-txn iter filter" 2 (iter_count db "SELECT n FROM t WHERE tag = 'new'");
    exec db "COMMIT")
;;

(* Point lookup on an INTEGER PRIMARY KEY rowid alias: the row written in-txn is
   visible via the single-key table-tree seek. *)
let test_ryw_rowid_lookup () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)";
    exec db "INSERT INTO t VALUES (1, 'one')";
    exec db "BEGIN";
    exec db "INSERT INTO t VALUES (2, 'two')";
    check "in-txn query rowid" 1 (query_count db "SELECT v FROM t WHERE id = 2");
    check "in-txn iter rowid" 1 (iter_count db "SELECT v FROM t WHERE id = 2");
    exec db "COMMIT")
;;

(* Secondary-index lookup: a row inserted in-txn is found through the index. *)
let test_ryw_index_lookup () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, c INTEGER)";
    exec db "CREATE INDEX t_c ON t (c)";
    exec db "INSERT INTO t VALUES (1, 100)";
    exec db "BEGIN";
    exec db "INSERT INTO t VALUES (2, 200)";
    check "in-txn query index" 1 (query_count db "SELECT id FROM t WHERE c = 200");
    check "in-txn iter index" 1 (iter_count db "SELECT id FROM t WHERE c = 200");
    exec db "COMMIT")
;;

(* COUNT-star aggregate fast path reflects in-txn writes. *)
let test_ryw_aggregate () =
  with_db (fun db ->
    exec db "CREATE TABLE t (n INTEGER)";
    exec db "INSERT INTO t (n) VALUES (1)";
    exec db "BEGIN";
    exec db "INSERT INTO t (n) VALUES (2)";
    exec db "INSERT INTO t (n) VALUES (3)";
    check "in-txn query count(*)" 3 (query_int db "SELECT COUNT(*) FROM t");
    exec db "COMMIT")
;;

(* Indexed nested-loop join sees an in-txn row on the inner (probed) side. *)
let test_ryw_join () =
  with_db (fun db ->
    exec db "CREATE TABLE a (id INTEGER PRIMARY KEY, k INTEGER)";
    exec db "CREATE TABLE b (id INTEGER PRIMARY KEY, k INTEGER)";
    exec db "CREATE INDEX b_k ON b (k)";
    exec db "INSERT INTO a VALUES (1, 7)";
    exec db "BEGIN";
    exec db "INSERT INTO b VALUES (1, 7)";
    check "in-txn query join" 1 (query_count db "SELECT a.id FROM a JOIN b ON a.k = b.k");
    check "in-txn iter join" 1 (iter_count db "SELECT a.id FROM a JOIN b ON a.k = b.k");
    exec db "COMMIT")
;;

(* An UPDATE inside the txn is visible to a subsequent in-txn read. *)
let test_ryw_update () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    exec db "BEGIN";
    exec db "UPDATE t SET v = 99 WHERE id = 1";
    check "in-txn query sees update" 99 (query_int db "SELECT v FROM t WHERE id = 1");
    check "in-txn iter sees update" 1 (iter_count db "SELECT id FROM t WHERE v = 99");
    exec db "COMMIT")
;;

(* A DELETE inside the txn hides the row from a subsequent in-txn read. *)
let test_ryw_delete () =
  with_db (fun db ->
    exec db "CREATE TABLE t (n INTEGER)";
    exec db "INSERT INTO t (n) VALUES (1)";
    exec db "INSERT INTO t (n) VALUES (2)";
    exec db "BEGIN";
    exec db "DELETE FROM t WHERE n = 1";
    check "in-txn query after delete" 1 (query_count db "SELECT n FROM t");
    check "in-txn iter after delete" 1 (iter_count db "SELECT n FROM t");
    exec db "COMMIT")
;;

(* A non-correlated IN-subquery, evaluated against an in-txn write, sees it. *)
let test_ryw_in_subquery () =
  with_db (fun db ->
    exec db "CREATE TABLE t (n INTEGER)";
    exec db "CREATE TABLE allow (n INTEGER)";
    exec db "INSERT INTO t (n) VALUES (1)";
    exec db "INSERT INTO t (n) VALUES (2)";
    exec db "BEGIN";
    exec db "INSERT INTO allow (n) VALUES (2)";
    check
      "in-txn query IN-subquery"
      1
      (query_count db "SELECT n FROM t WHERE n IN (SELECT n FROM allow)");
    check
      "in-txn iter IN-subquery"
      1
      (iter_count db "SELECT n FROM t WHERE n IN (SELECT n FROM allow)");
    exec db "COMMIT")
;;

(* A correlated EXISTS subquery (re-evaluated per outer row at pull time) sees
   the in-txn write to the inner table. *)
let test_ryw_correlated_exists () =
  with_db (fun db ->
    exec db "CREATE TABLE o (id INTEGER)";
    exec db "CREATE TABLE inr (g INTEGER)";
    exec db "INSERT INTO o (id) VALUES (1)";
    exec db "INSERT INTO o (id) VALUES (2)";
    exec db "BEGIN";
    exec db "INSERT INTO inr (g) VALUES (2)";
    check
      "in-txn query correlated EXISTS"
      1
      (query_count db "SELECT id FROM o WHERE EXISTS (SELECT 1 FROM inr WHERE g = o.id)");
    check
      "in-txn iter correlated EXISTS"
      1
      (iter_count db "SELECT id FROM o WHERE EXISTS (SELECT 1 FROM inr WHERE g = o.id)");
    exec db "COMMIT")
;;

(* Same read-your-own-writes property on the on-disk B-tree backend, whose store
   RW read path (working-tree reads) differs from the in-memory shadow map. *)
let test_ryw_file_backend () =
  with_file_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, c INTEGER)";
    exec db "CREATE INDEX t_c ON t (c)";
    exec db "INSERT INTO t VALUES (1, 100)";
    exec db "BEGIN";
    exec db "INSERT INTO t VALUES (2, 200)";
    check "file in-txn query seq-scan" 2 (query_count db "SELECT id FROM t");
    check "file in-txn iter seq-scan" 2 (iter_count db "SELECT id FROM t");
    check "file in-txn rowid lookup" 1 (query_count db "SELECT c FROM t WHERE id = 2");
    check "file in-txn index lookup" 1 (query_count db "SELECT id FROM t WHERE c = 200");
    check "file in-txn count(*)" 2 (query_int db "SELECT COUNT(*) FROM t");
    exec db "COMMIT";
    check "file post-commit" 2 (query_count db "SELECT id FROM t"))
;;

(* ROLLBACK still discards in-txn writes — the read after rollback sees the
   pre-BEGIN committed state, confirming the fix did not leak writes. *)
let test_rollback_discards () =
  with_db (fun db ->
    exec db "CREATE TABLE t (n INTEGER)";
    exec db "INSERT INTO t (n) VALUES (1)";
    exec db "BEGIN";
    exec db "INSERT INTO t (n) VALUES (2)";
    check "in-txn sees write" 2 (query_count db "SELECT n FROM t");
    exec db "ROLLBACK";
    check "post-rollback query" 1 (query_count db "SELECT n FROM t");
    check "post-rollback iter" 1 (iter_count db "SELECT n FROM t"))
;;

let () =
  Alcotest.run
    "txn_ryw_262"
    [ ( "read-your-own-writes"
      , [ Alcotest.test_case "seq scan" `Quick test_ryw_seq_scan
        ; Alcotest.test_case "filtered scan" `Quick test_ryw_filter
        ; Alcotest.test_case "rowid lookup" `Quick test_ryw_rowid_lookup
        ; Alcotest.test_case "index lookup" `Quick test_ryw_index_lookup
        ; Alcotest.test_case "aggregate count" `Quick test_ryw_aggregate
        ; Alcotest.test_case "nested-loop join" `Quick test_ryw_join
        ; Alcotest.test_case "update visible" `Quick test_ryw_update
        ; Alcotest.test_case "delete visible" `Quick test_ryw_delete
        ; Alcotest.test_case "IN-subquery" `Quick test_ryw_in_subquery
        ; Alcotest.test_case "correlated EXISTS" `Quick test_ryw_correlated_exists
        ; Alcotest.test_case "file backend" `Quick test_ryw_file_backend
        ; Alcotest.test_case "rollback discards" `Quick test_rollback_discards
        ] )
    ]
;;
