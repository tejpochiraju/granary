(** Phase 38 atomicity tests for triggers.  Covers issues #138 and #139:

    - #138: An AFTER trigger that raises rolls back the originating DML.
    - #139: BEFORE-DELETE triggers fire on rows displaced by INSERT OR
            REPLACE, and BEFORE-UPDATE triggers fire on the conflict row
            in an UPSERT DO UPDATE — both inside the parent txn so the
            trigger body's writes (and the parent's writes) commit
            atomically together. *)

open Lwt.Syntax
module Db = Granary.Db
module Row = Granary_encoding.Row

let run = Lwt_main.run

let unwrap_db_err pfx = function
  | Ok x -> x
  | Error e -> Alcotest.failf "%s: %a" pfx Db.pp_error e
;;

let open_mem () = Db.open_in_memory ()

let exec db sql =
  let* r = Db.execute db sql in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Lwt.fail_with (Format.asprintf "execute %S: %a" sql Db.pp_error e)
;;

let query db sql =
  let* r = Db.query db sql in
  let stream = unwrap_db_err "query" r in
  Lwt_stream.to_list stream
;;

let int_of_value = function
  | Row.V_int n -> Some (Int64.to_int n)
  | _ -> None
;;

let scalar_int db sql =
  let* rows = query db sql in
  match rows with
  | [ row ] when Array.length row >= 1 -> Lwt.return (int_of_value row.(0))
  | _ -> Lwt.return None
;;

(* ------------------------------------------------------------------ *)
(* #138 — AFTER trigger atomicity                                     *)
(* ------------------------------------------------------------------ *)

(* If an AFTER INSERT trigger raises (e.g. nested DML hits an unknown
   table), the originating INSERT must NOT remain committed.  Pre-phase-38
   this committed the row because AFTER fired after release_txn. *)
let test_after_insert_failure_rolls_back () =
  run
    (let* db = open_mem () in
     let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)" in
     let* () =
       exec
         db
         "CREATE TRIGGER t_ai AFTER INSERT ON t BEGIN INSERT INTO no_such_table VALUES \
          (NEW.id); END"
     in
     let* r = Db.execute db "INSERT INTO t (id, name) VALUES (1, 'foo')" in
     Alcotest.(check bool) "INSERT errored" true (Result.is_error r);
     let* count = scalar_int db "SELECT COUNT(*) FROM t" in
     Alcotest.(check (option int)) "row was rolled back" (Some 0) count;
     Lwt.return_unit)
;;

(* Same for AFTER UPDATE. *)
let test_after_update_failure_rolls_back () =
  run
    (let* db = open_mem () in
     let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, val INTEGER)" in
     let* () = exec db "INSERT INTO t VALUES (1, 100)" in
     let* () =
       exec
         db
         "CREATE TRIGGER t_au AFTER UPDATE ON t BEGIN INSERT INTO no_such_table VALUES \
          (NEW.val); END"
     in
     let* r = Db.execute db "UPDATE t SET val = 200 WHERE id = 1" in
     Alcotest.(check bool) "UPDATE errored" true (Result.is_error r);
     let* v = scalar_int db "SELECT val FROM t WHERE id = 1" in
     Alcotest.(check (option int)) "val unchanged after rollback" (Some 100) v;
     Lwt.return_unit)
;;

(* Same for AFTER DELETE. *)
let test_after_delete_failure_rolls_back () =
  run
    (let* db = open_mem () in
     let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, val INTEGER)" in
     let* () = exec db "INSERT INTO t VALUES (1, 100)" in
     let* () =
       exec
         db
         "CREATE TRIGGER t_ad AFTER DELETE ON t BEGIN INSERT INTO no_such_table VALUES \
          (OLD.val); END"
     in
     let* r = Db.execute db "DELETE FROM t WHERE id = 1" in
     Alcotest.(check bool) "DELETE errored" true (Result.is_error r);
     let* count = scalar_int db "SELECT COUNT(*) FROM t" in
     Alcotest.(check (option int)) "row still present" (Some 1) count;
     Lwt.return_unit)
;;

(* AFTER trigger body writes and the originating DML's writes must commit
   atomically: when the AFTER trigger succeeds, both are visible. *)
let test_after_insert_writes_in_same_txn () =
  run
    (let* db = open_mem () in
     let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)" in
     let* () = exec db "CREATE TABLE audit (tid INTEGER, action TEXT)" in
     let* () =
       exec
         db
         "CREATE TRIGGER t_ai AFTER INSERT ON t BEGIN INSERT INTO audit VALUES (NEW.id, \
          'INSERT'); END"
     in
     let* () = exec db "INSERT INTO t VALUES (1, 'foo')" in
     let* tc = scalar_int db "SELECT COUNT(*) FROM t" in
     let* ac = scalar_int db "SELECT COUNT(*) FROM audit" in
     Alcotest.(check (option int)) "row inserted in t" (Some 1) tc;
     Alcotest.(check (option int)) "audit row written by trigger" (Some 1) ac;
     Lwt.return_unit)
;;

(* If a nested DML inside an AFTER trigger body succeeds but a SUBSEQUENT
   trigger body statement raises, both the parent row AND the earlier
   trigger writes must be rolled back. *)
let test_after_trigger_multi_stmt_atomicity () =
  run
    (let* db = open_mem () in
     let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)" in
     let* () = exec db "CREATE TABLE audit (tid INTEGER)" in
     let* () =
       exec
         db
         "CREATE TRIGGER t_ai AFTER INSERT ON t BEGIN INSERT INTO audit VALUES (NEW.id); \
          INSERT INTO no_such_table VALUES (NEW.id); END"
     in
     let* r = Db.execute db "INSERT INTO t VALUES (1, 'foo')" in
     Alcotest.(check bool) "INSERT errored" true (Result.is_error r);
     let* tc = scalar_int db "SELECT COUNT(*) FROM t" in
     let* ac = scalar_int db "SELECT COUNT(*) FROM audit" in
     Alcotest.(check (option int)) "t rolled back" (Some 0) tc;
     Alcotest.(check (option int)) "audit also rolled back" (Some 0) ac;
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* #139 — BEFORE-DELETE / BEFORE-UPDATE on displaced rows             *)
(* ------------------------------------------------------------------ *)

(* INSERT OR REPLACE on a UNIQUE conflict must fire BEFORE-DELETE for the
   displaced row, inside the parent txn. *)
let test_before_delete_fires_on_replace () =
  run
    (let* db = open_mem () in
     let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)" in
     let* () = exec db "CREATE UNIQUE INDEX idx_t_name ON t (name)" in
     let* () = exec db "CREATE TABLE del_log (old_id INTEGER, old_name TEXT)" in
     let* () =
       exec
         db
         "CREATE TRIGGER t_bd BEFORE DELETE ON t BEGIN INSERT INTO del_log VALUES \
          (OLD.id, OLD.name); END"
     in
     let* () = exec db "INSERT INTO t VALUES (1, 'alice')" in
     (* Same UNIQUE name → conflict → REPLACE displaces row id=1. *)
     let* () = exec db "INSERT OR REPLACE INTO t (id, name) VALUES (2, 'alice')" in
     let* rows = query db "SELECT old_id, old_name FROM del_log" in
     Alcotest.(check int) "BEFORE DELETE fired once" 1 (List.length rows);
     (match rows with
      | [ r ] when Array.length r >= 2 ->
        Alcotest.(check (option int)) "displaced id" (Some 1) (int_of_value r.(0));
        (match r.(1) with
         | Row.V_text s -> Alcotest.(check string) "displaced name" "alice" s
         | _ -> Alcotest.fail "expected V_text for displaced name")
      | _ -> Alcotest.fail "expected exactly one log row");
     (* Replacement should be present. *)
     let* count = scalar_int db "SELECT COUNT(*) FROM t WHERE id = 2" in
     Alcotest.(check (option int)) "new row present" (Some 1) count;
     Lwt.return_unit)
;;

(* If BEFORE DELETE on the displaced row raises, the whole REPLACE
   (including the new row) must be rolled back. *)
let test_before_delete_failure_rolls_back_replace () =
  run
    (let* db = open_mem () in
     let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)" in
     let* () = exec db "CREATE UNIQUE INDEX idx_t_name ON t (name)" in
     let* () =
       exec
         db
         "CREATE TRIGGER t_bd BEFORE DELETE ON t BEGIN INSERT INTO no_such_table VALUES \
          (OLD.id); END"
     in
     let* () = exec db "INSERT INTO t VALUES (1, 'alice')" in
     let* r = Db.execute db "INSERT OR REPLACE INTO t (id, name) VALUES (2, 'alice')" in
     Alcotest.(check bool) "REPLACE errored" true (Result.is_error r);
     (* Original row survives, new row absent. *)
     let* old_count = scalar_int db "SELECT COUNT(*) FROM t WHERE id = 1" in
     let* new_count = scalar_int db "SELECT COUNT(*) FROM t WHERE id = 2" in
     Alcotest.(check (option int)) "original row present" (Some 1) old_count;
     Alcotest.(check (option int)) "new row not present" (Some 0) new_count;
     Lwt.return_unit)
;;

(* UPSERT DO UPDATE on a UNIQUE conflict must fire BEFORE-UPDATE on the
   conflict row, inside the parent txn. *)
let test_before_update_fires_on_upsert () =
  run
    (let* db = open_mem () in
     let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, val INTEGER)" in
     let* () =
       exec db "CREATE TABLE upd_log (rid INTEGER, old_val INTEGER, new_val INTEGER)"
     in
     let* () =
       exec
         db
         "CREATE TRIGGER t_bu BEFORE UPDATE ON t BEGIN INSERT INTO upd_log VALUES \
          (OLD.id, OLD.val, NEW.val); END"
     in
     let* () = exec db "INSERT INTO t VALUES (1, 100)" in
     let* () =
       exec
         db
         "INSERT INTO t (id, val) VALUES (1, 999) ON CONFLICT(id) DO UPDATE SET val = 200"
     in
     let* rows = query db "SELECT rid, old_val, new_val FROM upd_log" in
     Alcotest.(check int) "BEFORE UPDATE fired once" 1 (List.length rows);
     (match rows with
      | [ r ] when Array.length r >= 3 ->
        Alcotest.(check (option int)) "rid" (Some 1) (int_of_value r.(0));
        Alcotest.(check (option int)) "old_val" (Some 100) (int_of_value r.(1));
        Alcotest.(check (option int)) "new_val" (Some 200) (int_of_value r.(2))
      | _ -> Alcotest.fail "expected exactly one upd_log row");
     let* v = scalar_int db "SELECT val FROM t WHERE id = 1" in
     Alcotest.(check (option int)) "upserted row holds new val" (Some 200) v;
     Lwt.return_unit)
;;

(* If BEFORE UPDATE on the upsert conflict row raises, the whole UPSERT
   must roll back (the conflict row keeps its old value). *)
let test_before_update_failure_rolls_back_upsert () =
  run
    (let* db = open_mem () in
     let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, val INTEGER)" in
     let* () =
       exec
         db
         "CREATE TRIGGER t_bu BEFORE UPDATE ON t BEGIN INSERT INTO no_such_table VALUES \
          (OLD.id); END"
     in
     let* () = exec db "INSERT INTO t VALUES (1, 100)" in
     let* r =
       Db.execute
         db
         "INSERT INTO t (id, val) VALUES (1, 999) ON CONFLICT(id) DO UPDATE SET val = 200"
     in
     Alcotest.(check bool) "UPSERT errored" true (Result.is_error r);
     let* v = scalar_int db "SELECT val FROM t WHERE id = 1" in
     Alcotest.(check (option int)) "val unchanged" (Some 100) v;
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* Nested trigger sharing the parent txn (no deadlock)                *)
(* ------------------------------------------------------------------ *)

(* An AFTER trigger whose body itself fires an AFTER trigger must not
   deadlock and must commit all writes atomically with the parent. *)
let test_nested_after_trigger_no_deadlock () =
  run
    (let* db = open_mem () in
     let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY)" in
     let* () = exec db "CREATE TABLE mid (tid INTEGER)" in
     let* () = exec db "CREATE TABLE deep (mid_tid INTEGER)" in
     let* () =
       exec
         db
         "CREATE TRIGGER t_ai AFTER INSERT ON t BEGIN INSERT INTO mid VALUES (NEW.id); \
          END"
     in
     let* () =
       exec
         db
         "CREATE TRIGGER mid_ai AFTER INSERT ON mid BEGIN INSERT INTO deep VALUES \
          (NEW.tid); END"
     in
     let* () = exec db "INSERT INTO t VALUES (42)" in
     let* tc = scalar_int db "SELECT COUNT(*) FROM t" in
     let* mc = scalar_int db "SELECT COUNT(*) FROM mid" in
     let* dc = scalar_int db "SELECT COUNT(*) FROM deep" in
     let* deep = scalar_int db "SELECT mid_tid FROM deep" in
     Alcotest.(check (option int)) "t" (Some 1) tc;
     Alcotest.(check (option int)) "mid" (Some 1) mc;
     Alcotest.(check (option int)) "deep" (Some 1) dc;
     Alcotest.(check (option int)) "deep value" (Some 42) deep;
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
let () =
  Alcotest.run
    "trigger_atomicity"
    [ ( "after_atomicity"
      , [ Alcotest.test_case
            "AFTER INSERT failure rolls back DML"
            `Quick
            test_after_insert_failure_rolls_back
        ; Alcotest.test_case
            "AFTER UPDATE failure rolls back DML"
            `Quick
            test_after_update_failure_rolls_back
        ; Alcotest.test_case
            "AFTER DELETE failure rolls back DML"
            `Quick
            test_after_delete_failure_rolls_back
        ; Alcotest.test_case
            "AFTER trigger writes share parent txn"
            `Quick
            test_after_insert_writes_in_same_txn
        ; Alcotest.test_case
            "AFTER multi-stmt rollback is atomic"
            `Quick
            test_after_trigger_multi_stmt_atomicity
        ] )
    ; ( "before_displaced"
      , [ Alcotest.test_case
            "BEFORE DELETE fires on REPLACE-displaced row"
            `Quick
            test_before_delete_fires_on_replace
        ; Alcotest.test_case
            "BEFORE DELETE failure rolls back REPLACE"
            `Quick
            test_before_delete_failure_rolls_back_replace
        ; Alcotest.test_case
            "BEFORE UPDATE fires on UPSERT-displaced row"
            `Quick
            test_before_update_fires_on_upsert
        ; Alcotest.test_case
            "BEFORE UPDATE failure rolls back UPSERT"
            `Quick
            test_before_update_failure_rolls_back_upsert
        ] )
    ; ( "nested"
      , [ Alcotest.test_case
            "nested AFTER triggers (no deadlock)"
            `Quick
            test_nested_after_trigger_no_deadlock
        ] )
    ]
;;
