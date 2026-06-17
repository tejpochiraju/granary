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
        ] )
    ]
;;
