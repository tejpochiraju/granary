(** #240: the set of user tables a write statement actually mutated, exposed via
    {!Db.execute_with_dirty}.  The load-bearing property for an external read
    cache is completeness under FK cascades and triggers: a write to one table
    that silently mutates another (inside the engine) reports BOTH. *)

module Db = Granary.Db

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

(* #240 / PR #404 review: the signal is row-level DML only.  Schema-changing and
   destructive DDL reports an EMPTY set — even [ALTER TABLE … DROP COLUMN], which
   physically rewrites every stored row.  camel invalidates schema changes out of
   band (a schema-version / fingerprint signal, cf. #174).  These tests pin that
   boundary so the [] contract can't silently regress into "row-neutral". *)
let test_alter_drop_column_is_empty () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT, c TEXT)";
    exec db "INSERT INTO t VALUES (1, 'x', 'y')";
    (* DROP COLUMN drains + re-puts every row (reshaped), yet is out of scope. *)
    check_dirty "drop column reports nothing" [] (dirty db "ALTER TABLE t DROP COLUMN c"))
;;

let test_drop_table_is_empty () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER PRIMARY KEY)";
    exec db "INSERT INTO t VALUES (1)";
    check_dirty "drop table reports nothing" [] (dirty db "DROP TABLE t"))
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
              ~params:[ Granary_encoding.Row.V_int 1L; Granary_encoding.Row.V_text "a" ]))
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

(* Gap 1 — secondary-index UPSERT (ON CONFLICT(<unique index>) DO UPDATE).
   The conflict→update arm writes via [write_row_rekeyed] (raw put/del), not the
   marked normal-insert path, so the mutated table must still be reported. *)
let test_secondary_index_upsert_marks () =
  with_db (fun db ->
    exec db "CREATE TABLE u (id INTEGER PRIMARY KEY, k TEXT, v TEXT)";
    exec db "CREATE UNIQUE INDEX u_k ON u (k)";
    exec db "INSERT INTO u VALUES (1, 'key', 'a')";
    (* conflict on the UNIQUE index over k (NOT the PK alias) → DO UPDATE path *)
    check_dirty
      "secondary-index upsert update marks table"
      [ "u" ]
      (dirty
         db
         "INSERT INTO u (id, k, v) VALUES (2, 'key', 'b') ON CONFLICT(k) DO UPDATE SET v \
          = excluded.v"))
;;

(* Gap 2 — columnar (COLUMNSTORE) INSERT writes via Col_store.insert_rows with
   no marked primitive; a successful insert must report the table. *)
let test_columnar_insert_marks () =
  with_db (fun db ->
    exec db "CREATE TABLE c (a INTEGER, b TEXT) USING COLUMNSTORE";
    check_dirty
      "columnar insert marks table"
      [ "c" ]
      (dirty db "INSERT INTO c VALUES (1, 'x')"))
;;

(* Gap 2 — columnar INSERT…SELECT with no source rows writes nothing → no mark. *)
let test_columnar_insert_select_noop_is_empty () =
  with_db (fun db ->
    exec db "CREATE TABLE src (a INTEGER, b TEXT)";
    exec db "CREATE TABLE c (a INTEGER, b TEXT) USING COLUMNSTORE";
    check_dirty
      "columnar insert-select no source rows: empty"
      []
      (dirty db "INSERT INTO c SELECT a, b FROM src"))
;;

(* Gap 3 — FTS5 virtual-table INSERT mutates the user table via raw put. *)
let test_fts_insert_marks () =
  with_db (fun db ->
    exec db "CREATE VIRTUAL TABLE docs USING FTS5(title, body)";
    check_dirty
      "fts insert marks table"
      [ "docs" ]
      (dirty db "INSERT INTO docs (title, body) VALUES ('t', 'hello world')"))
;;

(* Gap 3 — FTS5 DELETE that removes a row must report; a no-op delete must not. *)
let test_fts_delete_marks () =
  with_db (fun db ->
    exec db "CREATE VIRTUAL TABLE docs USING FTS5(title, body)";
    exec db "INSERT INTO docs (title, body) VALUES ('t', 'hello world')";
    check_dirty
      "fts delete no-op: empty"
      []
      (dirty db "DELETE FROM docs WHERE title = 'absent'");
    check_dirty
      "fts delete marks table"
      [ "docs" ]
      (dirty db "DELETE FROM docs WHERE title = 't'"))
;;

(* ON UPDATE CASCADE: updating the parent key cascades to the child via the
   [update_col_in_tx] mark — should already PASS with the existing cascade code. *)
let test_on_update_cascade () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE dept (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE emp (id INTEGER PRIMARY KEY, d INTEGER REFERENCES dept(id) ON UPDATE \
       CASCADE)";
    exec db "INSERT INTO dept VALUES (1)";
    exec db "INSERT INTO emp VALUES (10, 1)";
    check_dirty
      "update cascade marks parent+child"
      [ "dept"; "emp" ]
      (dirty db "UPDATE dept SET id = 2 WHERE id = 1"))
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
    ; ( "ddl boundary (out of scope)"
      , [ Alcotest.test_case
            "alter drop column empty"
            `Quick
            test_alter_drop_column_is_empty
        ; Alcotest.test_case "drop table empty" `Quick test_drop_table_is_empty
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
    ; ( "write-path coverage"
      , [ Alcotest.test_case
            "secondary-index upsert"
            `Quick
            test_secondary_index_upsert_marks
        ; Alcotest.test_case "columnar insert" `Quick test_columnar_insert_marks
        ; Alcotest.test_case
            "columnar insert-select no-op"
            `Quick
            test_columnar_insert_select_noop_is_empty
        ; Alcotest.test_case "fts insert" `Quick test_fts_insert_marks
        ; Alcotest.test_case "fts delete hit/miss" `Quick test_fts_delete_marks
        ; Alcotest.test_case "on update cascade" `Quick test_on_update_cascade
        ] )
    ; "property", [ QCheck_alcotest.to_alcotest trigger_chain_property ]
    ]
;;
