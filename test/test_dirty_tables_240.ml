(** #240: the set of user tables a write statement actually mutated, exposed via
    {!Db.execute_with_dirty}.  The load-bearing property for an external read
    cache is completeness under FK cascades and triggers: a write to one table
    that silently mutates another (inside the engine) reports BOTH. *)

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

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

(* Run [sql] via [execute_with_dirty] and return the dirtied user tables. *)
let dirty db sql : string list = unwrap (run (Db.execute_with_dirty db sql))

let check_dirty msg expected sql_result =
  Alcotest.(check (list string)) msg expected sql_result
;;

let test_insert_marks_table () =
  with_db (fun db ->
    exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, n TEXT)";
    check_dirty "plain insert" [ "users" ] (dirty db "INSERT INTO users VALUES (1, 'a')"))
;;

let test_insert_or_ignore_noop_is_empty () =
  with_db (fun db ->
    exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, n TEXT)";
    exec db "INSERT INTO users VALUES (1, 'a')";
    (* PK conflict + OR IGNORE inserts nothing → no table dirtied. *)
    check_dirty
      "insert-or-ignore no-op"
      []
      (dirty db "INSERT OR IGNORE INTO users VALUES (1, 'b')"))
;;

let test_ddl_is_empty () =
  with_db (fun db ->
    check_dirty "create table dirties nothing" [] (dirty db "CREATE TABLE t (x INTEGER)"))
;;

let test_update_hit_and_miss () =
  with_db (fun db ->
    exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, n TEXT)";
    exec db "INSERT INTO users VALUES (1, 'a')";
    check_dirty
      "update hit"
      [ "users" ]
      (dirty db "UPDATE users SET n = 'b' WHERE id = 1");
    check_dirty "update miss" [] (dirty db "UPDATE users SET n = 'c' WHERE id = 999"))
;;

let test_delete_hit_and_miss () =
  with_db (fun db ->
    exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, n TEXT)";
    exec db "INSERT INTO users VALUES (1, 'a')";
    check_dirty "delete miss" [] (dirty db "DELETE FROM users WHERE id = 999");
    check_dirty "delete hit" [ "users" ] (dirty db "DELETE FROM users WHERE id = 1"))
;;

(* execute_change_count_with_dirty returns BOTH the rows-affected count and the set. *)
let test_change_count_with_dirty () =
  with_db (fun db ->
    exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, n TEXT)";
    exec db "INSERT INTO users VALUES (1, 'a')";
    exec db "INSERT INTO users VALUES (2, 'a')";
    let n, tables =
      unwrap (run (Db.execute_change_count_with_dirty db "UPDATE users SET n = 'z'"))
    in
    Alcotest.(check int) "rows changed" 2 n;
    Alcotest.(check (list string)) "dirtied once (deduped)" [ "users" ] tables)
;;

let test_run_with_dirty () =
  with_db (fun db ->
    exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, n TEXT)";
    let st = unwrap (run (Db.prepare db "INSERT INTO users VALUES (?, ?)")) in
    let n, tables =
      unwrap
        (run
           (Db.run_with_dirty
              st
              ~params:[ Sqlocaml_encoding.Row.V_int 1L; Sqlocaml_encoding.Row.V_text "a" ]))
    in
    Alcotest.(check int) "one row" 1 n;
    Alcotest.(check (list string)) "run dirtied users" [ "users" ] tables)
;;

(* ON DELETE CASCADE: deleting the parent silently deletes child rows inside the
   engine — the set must include BOTH tables. *)
let test_on_delete_cascade () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE dept (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE emp (id INTEGER PRIMARY KEY, d INTEGER REFERENCES dept(id) ON DELETE \
       CASCADE)";
    exec db "INSERT INTO dept VALUES (1)";
    exec db "INSERT INTO emp VALUES (10, 1)";
    (* sorted, deduplicated: dept before emp *)
    check_dirty
      "delete cascade marks parent+child"
      [ "dept"; "emp" ]
      (dirty db "DELETE FROM dept WHERE id = 1"))
;;

(* ON DELETE SET NULL: the child row is UPDATEd (FK col set NULL), not deleted. *)
let test_on_delete_set_null () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE dept (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE emp (id INTEGER PRIMARY KEY, d INTEGER REFERENCES dept(id) ON DELETE \
       SET NULL)";
    exec db "INSERT INTO dept VALUES (1)";
    exec db "INSERT INTO emp VALUES (10, 1)";
    check_dirty
      "set-null marks parent+child"
      [ "dept"; "emp" ]
      (dirty db "DELETE FROM dept WHERE id = 1"))
;;

(* AFTER INSERT trigger whose body writes a DIFFERENT table: both must appear. *)
let test_trigger_marks_both_tables () =
  with_db (fun db ->
    exec db "CREATE TABLE a (id INTEGER PRIMARY KEY)";
    exec db "CREATE TABLE b (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TRIGGER a_ai AFTER INSERT ON a BEGIN INSERT INTO b VALUES (NEW.id); END";
    check_dirty "trigger fans out to b" [ "a"; "b" ] (dirty db "INSERT INTO a VALUES (1)"))
;;

(* AUTOINCREMENT bumps the internal rowid counter (sqlite_sequence is a view);
   the internal name must NOT leak into the set. *)
let test_autoincrement_excludes_internal () =
  with_db (fun db ->
    exec db "CREATE TABLE s (id INTEGER PRIMARY KEY AUTOINCREMENT, n TEXT)";
    check_dirty
      "autoincrement insert: user table only"
      [ "s" ]
      (dirty db "INSERT INTO s (n) VALUES ('x')"))
;;

(* A statement that fails to parse propagates [Error] through the wrapper
   (covers the [Error] arm; no accumulator is drained). *)
let test_error_propagates () =
  with_db (fun db ->
    match run (Db.execute_with_dirty db "INSERT INTphony") with
    | Error _ -> ()
    | Ok tables ->
      Alcotest.failf "expected Error, got dirty set [%s]" (String.concat "; " tables))
;;

(* QCheck: a chain t0 → t1 → … → t(n-1) of AFTER INSERT triggers means one
   insert into t0 mutates every table in the chain. The reported set must be
   exactly those tables, sorted and duplicate-free, for any chain length. *)
let trigger_chain_property =
  QCheck.Test.make
    ~count:50
    ~name:"trigger chain: dirty set is the sorted unique chain"
    QCheck.(int_range 1 6)
    (fun n ->
       with_db (fun db ->
         let names = List.init n (fun i -> Printf.sprintf "t%d" i) in
         List.iter
           (fun nm ->
              exec db (Printf.sprintf "CREATE TABLE %s (id INTEGER PRIMARY KEY)" nm))
           names;
         (* chain: inserting into t(i) inserts into t(i+1) *)
         for i = 0 to n - 2 do
           exec
             db
             (Printf.sprintf
                "CREATE TRIGGER tr%d AFTER INSERT ON t%d BEGIN INSERT INTO t%d VALUES \
                 (NEW.id); END"
                i
                i
                (i + 1))
         done;
         let got = dirty db "INSERT INTO t0 VALUES (1)" in
         let expected = List.sort_uniq String.compare names in
         (* sorted + unique + complete *)
         got = expected))
;;

let () =
  Alcotest.run
    "dirty_tables_240"
    [ ( "core"
      , [ Alcotest.test_case "insert marks table" `Quick test_insert_marks_table
        ; Alcotest.test_case
            "insert-or-ignore no-op empty"
            `Quick
            test_insert_or_ignore_noop_is_empty
        ; Alcotest.test_case "ddl empty" `Quick test_ddl_is_empty
        ; Alcotest.test_case "update hit/miss" `Quick test_update_hit_and_miss
        ; Alcotest.test_case "delete hit/miss" `Quick test_delete_hit_and_miss
        ; Alcotest.test_case "change_count_with_dirty" `Quick test_change_count_with_dirty
        ; Alcotest.test_case "run_with_dirty" `Quick test_run_with_dirty
        ; Alcotest.test_case "error propagates" `Quick test_error_propagates
        ] )
    ; ( "cascades"
      , [ Alcotest.test_case "on delete cascade" `Quick test_on_delete_cascade
        ; Alcotest.test_case "on delete set null" `Quick test_on_delete_set_null
        ] )
    ; ( "semantics"
      , [ Alcotest.test_case "trigger marks both" `Quick test_trigger_marks_both_tables
        ; Alcotest.test_case
            "autoincrement excludes internal"
            `Quick
            test_autoincrement_excludes_internal
        ] )
    ; "property", [ QCheck_alcotest.to_alcotest trigger_chain_property ]
    ]
;;
