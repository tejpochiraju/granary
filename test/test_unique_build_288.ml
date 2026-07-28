(** #288: [CREATE UNIQUE INDEX] over a table that already contains rows
    violating the uniqueness must FAIL, not silently build a "UNIQUE" index
    whose data is not unique.

    Root cause (pre-fix): [execute_create_index] built the index by encoding
    each row's key WITH its rowid suffix, so two rows sharing the indexed value
    produced distinct keys and never collided — the build never detected the
    duplicate.  Uniqueness was only enforced at INSERT time.

    The contract these tests pin: build-time and insert-time uniqueness AGREE.
    For any dataset, "INSERT rows then CREATE UNIQUE INDEX" and "CREATE UNIQUE
    INDEX then INSERT rows" must reach the same accept/reject outcome — and the
    in-txn failure path must poison/abort cleanly (#286/#287), leaving no
    partial index entries behind. *)

open Lwt.Syntax
module Db = Granary.Db

let run = Lwt_main.run

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

(* Run a statement; return [true] on success, [false] on error. *)
let exec_ok db sql =
  match run (Db.execute db sql) with
  | Ok () -> true
  | Error _ -> false
;;

let exec_err db sql =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "expected %S to fail, but it succeeded" sql
  | Error e -> Format.asprintf "%a" Db.pp_error e
;;

let contains ~needle haystack =
  let nl = String.length needle
  and hl = String.length haystack in
  let rec go i =
    i + nl <= hl && (String.equal (String.sub haystack i nl) needle || go (i + 1))
  in
  nl = 0 || go 0
;;

let vstr = function
  | Db.V_int i -> Printf.sprintf "i:%Ld" i
  | Db.V_real f -> Printf.sprintf "r:%.17g" f
  | Db.V_text s -> Printf.sprintf "t:%s" s
  | Db.V_null -> "null"
  | Db.V_blob b -> Printf.sprintf "b:%s" (String.escaped (Bytes.to_string b))
;;

let rows db sql =
  run
    (let* s = Db.query db sql in
     match s with
     | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
     | Ok s ->
       let* rs = Lwt_stream.to_list s in
       Lwt.return
         (List.map (fun r -> String.concat "," (Array.to_list (Array.map vstr r))) rs))
;;

let table_absent db tbl =
  match run (Db.query db (Printf.sprintf "SELECT * FROM \"%s\"" tbl)) with
  | Error _ -> true
  | Ok s ->
    (try
       let _ = run (Lwt_stream.to_list s) in
       false
     with
     | _ -> true)
;;

(* ------------------------------------------------------------------ *)
(* Core repro (#288)                                                    *)
(* ------------------------------------------------------------------ *)

(* The exact repro from the issue: pre-existing duplicate, then build. *)
let test_build_rejects_preexisting_duplicate () =
  let db = run (Db.open_in_memory ()) in
  exec db "CREATE TABLE t (a INTEGER)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (1)";
  let err = exec_err db "CREATE UNIQUE INDEX ix ON t (a)" in
  Alcotest.(check bool)
    "build over duplicate data is rejected as UNIQUE violation"
    true
    (contains ~needle:"UNIQUE" err);
  (* The failed build must leave NO index and no half-built state behind: the
     data is intact (both rows still there) and the index name is free — a
     non-unique index of the same name now builds cleanly (a stale catalog entry
     would raise "already exists", partial tree entries would corrupt it). *)
  Alcotest.(check (list string))
    "both rows intact"
    [ "i:1"; "i:1" ]
    (rows db "SELECT a FROM t");
  Alcotest.(check bool)
    "index name free after failed build"
    true
    (exec_ok db "CREATE INDEX ix ON t (a)")
;;

(* Distinct pre-existing values still build, and the index is usable. *)
let test_build_allows_distinct () =
  let db = run (Db.open_in_memory ()) in
  exec db "CREATE TABLE t (a INTEGER, b TEXT)";
  exec db "INSERT INTO t VALUES (1, 'x')";
  exec db "INSERT INTO t VALUES (2, 'y')";
  exec db "CREATE UNIQUE INDEX ix ON t (a)";
  Alcotest.(check (list string))
    "index lookup returns the matching row"
    [ "i:2,t:y" ]
    (rows db "SELECT a, b FROM t WHERE a = 2");
  (* And the freshly-built unique index now enforces inserts. *)
  Alcotest.(check bool)
    "new duplicate insert rejected by the built index"
    false
    (exec_ok db "INSERT INTO t VALUES (1, 'z')")
;;

(* Composite UNIQUE index: rows that share only ONE column are distinct tuples
   and build fine; rows duplicating the FULL tuple are rejected.  (The duplicate
   data is created before any unique index is live, so the INSERTs themselves
   succeed — the build is what must catch it.) *)
let test_build_multicol_duplicate () =
  (* distinct tuples (share a only) build cleanly *)
  let db = run (Db.open_in_memory ()) in
  exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
  exec db "INSERT INTO t VALUES (1, 10)";
  exec db "INSERT INTO t VALUES (1, 20)";
  exec db "CREATE UNIQUE INDEX ix ON t (a, b)";
  Alcotest.(check int)
    "two distinct tuples kept"
    2
    (List.length (rows db "SELECT * FROM t"));
  (* full-tuple duplicate is rejected at build time *)
  let db2 = run (Db.open_in_memory ()) in
  exec db2 "CREATE TABLE t (a INTEGER, b INTEGER)";
  exec db2 "INSERT INTO t VALUES (2, 10)";
  exec db2 "INSERT INTO t VALUES (2, 10)";
  let err = exec_err db2 "CREATE UNIQUE INDEX ix ON t (a, b)" in
  Alcotest.(check bool)
    "composite build over a full-tuple duplicate is rejected"
    true
    (contains ~needle:"UNIQUE" err)
;;

(* Partial UNIQUE index: duplicates EXCLUDED by the WHERE must not block the
   build; duplicates INCLUDED by it must.  Each case starts from a fresh table
   whose rows are inserted before any unique index exists. *)
let test_build_partial_where () =
  (* duplicate exists but is outside the predicate -> build succeeds *)
  let db = run (Db.open_in_memory ()) in
  exec db "CREATE TABLE t (a INTEGER, active INTEGER)";
  exec db "INSERT INTO t VALUES (1, 0)";
  exec db "INSERT INTO t VALUES (1, 0)";
  Alcotest.(check bool)
    "partial build ignores duplicates outside the predicate"
    true
    (exec_ok db "CREATE UNIQUE INDEX ix ON t (a) WHERE active = 1");
  (* duplicate is inside the predicate -> build fails *)
  let db2 = run (Db.open_in_memory ()) in
  exec db2 "CREATE TABLE t (a INTEGER, active INTEGER)";
  exec db2 "INSERT INTO t VALUES (2, 1)";
  exec db2 "INSERT INTO t VALUES (2, 1)";
  let err = exec_err db2 "CREATE UNIQUE INDEX ix ON t (a) WHERE active = 1" in
  Alcotest.(check bool)
    "partial build rejects duplicates inside the predicate"
    true
    (contains ~needle:"UNIQUE" err)
;;

(* ------------------------------------------------------------------ *)
(* In-txn failure must poison the transaction (#286/#287)               *)
(* ------------------------------------------------------------------ *)

let test_build_in_txn_duplicate_poisons_commit () =
  let db = run (Db.open_in_memory ()) in
  exec db "BEGIN";
  exec db "CREATE TABLE t (a INTEGER)";
  exec db "INSERT INTO t VALUES (5)";
  exec db "INSERT INTO t VALUES (5)";
  let err = exec_err db "CREATE UNIQUE INDEX ix ON t (a)" in
  Alcotest.(check bool)
    "in-txn build over duplicate rejected"
    true
    (contains ~needle:"UNIQUE" err);
  let commit_err = exec_err db "COMMIT" in
  Alcotest.(check bool)
    "COMMIT rejected: a partially-built UNIQUE index left the txn uncommittable"
    true
    (contains ~needle:"uncommittable" commit_err);
  (* Whole txn rolled back: the table is gone and the connection is clean. *)
  Alcotest.(check bool) "table discarded by forced rollback" true (table_absent db "t");
  exec db "CREATE TABLE t (a INTEGER)";
  exec db "INSERT INTO t VALUES (9)";
  Alcotest.(check (list string))
    "recreated table usable"
    [ "i:9" ]
    (rows db "SELECT a FROM t")
;;

(* An explicit ROLLBACK after the failed in-txn build also unwinds cleanly and
   leaves no partial index entries. *)
let test_build_in_txn_duplicate_then_rollback_clean () =
  let db = run (Db.open_in_memory ()) in
  exec db "BEGIN";
  exec db "CREATE TABLE t (a INTEGER)";
  exec db "INSERT INTO t VALUES (5)";
  exec db "INSERT INTO t VALUES (5)";
  let _ = exec_err db "CREATE UNIQUE INDEX ix ON t (a)" in
  exec db "ROLLBACK";
  Alcotest.(check bool) "table gone after rollback" true (table_absent db "t");
  (* The index name is free and the connection works in a fresh txn. *)
  exec db "BEGIN";
  exec db "CREATE TABLE t (a INTEGER)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (2)";
  exec db "CREATE UNIQUE INDEX ix ON t (a)";
  exec db "COMMIT";
  Alcotest.(check int)
    "fresh txn commits cleanly"
    2
    (List.length (rows db "SELECT * FROM t"))
;;

(* ------------------------------------------------------------------ *)
(* Build/insert agreement invariant — pins NULL handling without        *)
(* hard-coding which semantics the engine uses (#288 is about agreement) *)
(* ------------------------------------------------------------------ *)

(* For a dataset, does "INSERT then CREATE UNIQUE INDEX" succeed? *)
let build_after_insert_ok vals =
  let db = run (Db.open_in_memory ()) in
  exec db "CREATE TABLE t (a INTEGER)";
  List.iter (fun v -> exec db (Printf.sprintf "INSERT INTO t VALUES (%s)" v)) vals;
  exec_ok db "CREATE UNIQUE INDEX ix ON t (a)"
;;

(* For the same dataset, does "CREATE UNIQUE INDEX then INSERT" accept every
   row?  ([true] = all inserts succeeded under the live unique index.) *)
let insert_after_build_ok vals =
  let db = run (Db.open_in_memory ()) in
  exec db "CREATE TABLE t (a INTEGER)";
  exec db "CREATE UNIQUE INDEX ix ON t (a)";
  List.for_all (fun v -> exec_ok db (Printf.sprintf "INSERT INTO t VALUES (%s)" v)) vals
;;

let check_agreement name vals =
  let a = build_after_insert_ok vals in
  let b = insert_after_build_ok vals in
  Alcotest.(check bool)
    (Printf.sprintf "%s: build-time and insert-time uniqueness agree" name)
    b
    a
;;

let test_build_insert_agreement () =
  check_agreement "distinct ints" [ "1"; "2"; "3" ];
  check_agreement "duplicate ints" [ "1"; "2"; "1" ];
  check_agreement "single null" [ "1"; "NULL"; "2" ];
  check_agreement "two nulls" [ "1"; "NULL"; "NULL" ];
  (* #290: SQLite treats every NULL as distinct in a UNIQUE index, so two NULLs
     are NOT a violation — build-time and insert-time must BOTH accept (agreement
     alone is necessary but not sufficient; pin the absolute outcome here). *)
  Alcotest.(check bool)
    "two nulls: build over multiple NULLs is accepted (NULLs distinct)"
    true
    (build_after_insert_ok [ "1"; "NULL"; "NULL" ]);
  Alcotest.(check bool)
    "two nulls: insert under a live unique index accepts multiple NULLs"
    true
    (insert_after_build_ok [ "1"; "NULL"; "NULL" ])
;;

let () =
  Alcotest.run
    "unique_build_288"
    [ ( "build_rejects_duplicate"
      , [ Alcotest.test_case
            "preexisting dup"
            `Quick
            test_build_rejects_preexisting_duplicate
        ; Alcotest.test_case "distinct ok" `Quick test_build_allows_distinct
        ; Alcotest.test_case "composite dup" `Quick test_build_multicol_duplicate
        ; Alcotest.test_case "partial where" `Quick test_build_partial_where
        ] )
    ; ( "in_txn"
      , [ Alcotest.test_case
            "dup poisons commit"
            `Quick
            test_build_in_txn_duplicate_poisons_commit
        ; Alcotest.test_case
            "dup then rollback clean"
            `Quick
            test_build_in_txn_duplicate_then_rollback_clean
        ] )
    ; ( "agreement"
      , [ Alcotest.test_case "build == insert outcome" `Quick test_build_insert_agreement
        ] )
    ]
;;
