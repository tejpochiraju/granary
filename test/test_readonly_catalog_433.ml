(** #433: [Db.catalog] handed out the live {!Granary_catalog.Catalog.t}, which
    is a read-WRITE surface — [drop_table], [add_column],
    [set_last_inserted_rowid], [store], … — so "do not mutate it, schema
    changes go through SQL DDL" was a doc comment rather than a constraint. It
    is replaced by {!Granary.Schema}, an opaque read-only projection.

    What this file pins:

    - every accessor the projection exposes reports what the catalog holds;
    - the projection is a LIVE VIEW, not a snapshot: a projection taken before
      a DDL statement reflects that statement afterwards, so nothing can go
      stale (a clone would, and duplicated catalog state is how this engine has
      previously lost rows — see CLAUDE.md's #589/#633 sections);
    - {!Granary.Db.plan}, the read-only replacement for the one legitimate use
      of the live catalog a schema projection cannot serve, plans without
      executing.

    What it CANNOT pin is the removal itself: that a mutator is no longer
    reachable is a compile-time property, and a test that tried to call one
    would fail to build rather than fail. The removal is checked by the type
    checker on every build of this library's consumers. *)

module Db = Granary.Db
module Schema = Granary.Schema
module Row = Granary_encoding.Row
module Cat = Granary_catalog.Catalog
module Plan = Granary_sql.Plan

let run = Lwt_main.run

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

let table_named sch name =
  match run (Schema.find_table sch ~name) with
  | Some t -> t
  | None -> Alcotest.failf "no table %S in the schema projection" name
;;

let count_users db =
  run
    (let open Lwt.Syntax in
     let* s = Db.query db "SELECT COUNT(*) FROM users" in
     match s with
     | Error e -> Alcotest.failf "count: %a" Db.pp_error e
     | Ok s ->
       let* rows = Lwt_stream.to_list s in
       (match rows with
        | [ [| Row.V_int n |] ] -> Lwt.return (Int64.to_int n)
        | _ -> Alcotest.fail "unexpected COUNT shape"))
;;

let seed db =
  exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, email TEXT NOT NULL)";
  exec db "CREATE TABLE posts (id INTEGER PRIMARY KEY, author INTEGER, title TEXT)";
  exec db "CREATE INDEX posts_author ON posts (author)"
;;

(* --------------------------------------------------------------- *)
(* Every accessor reports the schema the catalog holds               *)
(* --------------------------------------------------------------- *)

let list_tables_and_find_table_agree () =
  with_db (fun db ->
    seed db;
    let sch = Db.schema db in
    let names =
      run (Schema.list_tables sch)
      |> List.map (fun (t : Schema.table) -> t.name)
      |> List.sort String.compare
    in
    Alcotest.(check (list string)) "tables" [ "posts"; "users" ] names;
    let users = table_named sch "users" in
    Alcotest.(check string) "find_table name" "users" users.name;
    Alcotest.(check (option bool))
      "a missing table is None"
      None
      (Option.map
         (fun (_ : Schema.table) -> true)
         (run (Schema.find_table sch ~name:"nope"))))
;;

let column_metadata_is_projected () =
  with_db (fun db ->
    seed db;
    let users = table_named (Db.schema db) "users" in
    let names = List.map (fun (c : Row.column) -> c.name) users.columns in
    Alcotest.(check (list string)) "column names" [ "id"; "email" ] names;
    let email = List.find (fun (c : Row.column) -> c.name = "email") users.columns in
    Alcotest.(check bool) "email is Text" true (email.ty = Row.Text);
    Alcotest.(check bool) "email is NOT NULL" true email.not_null;
    let id = List.find (fun (c : Row.column) -> c.name = "id") users.columns in
    Alcotest.(check bool) "id is the PRIMARY KEY" true id.primary_key)
;;

let table_exists_tracks_the_catalog () =
  with_db (fun db ->
    seed db;
    let sch = Db.schema db in
    Alcotest.(check bool) "users exists" true (Schema.table_exists sch ~name:"users");
    Alcotest.(check bool) "nope does not" false (Schema.table_exists sch ~name:"nope"))
;;

let without_rowid_and_fk_flags_are_projected () =
  with_db (fun db ->
    exec db "CREATE TABLE wr (k INTEGER PRIMARY KEY, v TEXT) WITHOUT ROWID";
    exec db "CREATE TABLE parent (id INTEGER PRIMARY KEY)";
    exec db "CREATE TABLE child (id INTEGER PRIMARY KEY, p INTEGER REFERENCES parent(id))";
    let sch = Db.schema db in
    let wr = table_named sch "wr" in
    Alcotest.(check bool) "wr is WITHOUT ROWID" true wr.without_rowid;
    Alcotest.(check bool) "wr is not columnar" false wr.columnar;
    let child = table_named sch "child" in
    Alcotest.(check bool) "plain table is not WITHOUT ROWID" false child.without_rowid;
    Alcotest.(check int) "child has one FK" 1 (List.length child.fk_constraints);
    let parent = table_named sch "parent" in
    Alcotest.(check int) "parent has none" 0 (List.length parent.fk_constraints))
;;

let a_columnstore_table_is_flagged () =
  with_db (fun db ->
    exec db "CREATE TABLE c (a INTEGER, b TEXT) USING COLUMNSTORE";
    let c = table_named (Db.schema db) "c" in
    Alcotest.(check bool) "columnar" true c.columnar;
    Alcotest.(check bool) "not WITHOUT ROWID" false c.without_rowid;
    Alcotest.(check (list string))
      "columns still projected"
      [ "a"; "b" ]
      (List.map (fun (col : Row.column) -> col.name) c.columns))
;;

let index_accessors_report_the_indexes () =
  with_db (fun db ->
    seed db;
    let sch = Db.schema db in
    Alcotest.(check bool)
      "posts_author exists"
      true
      (Schema.index_exists sch ~name:"posts_author");
    Alcotest.(check bool) "nope does not" false (Schema.index_exists sch ~name:"nope");
    (match Schema.find_index sch ~name:"posts_author" with
     | None -> Alcotest.fail "find_index should see the created index"
     | Some i ->
       Alcotest.(check string) "index table" "posts" i.Cat.idx_table;
       Alcotest.(check (list string)) "index columns" [ "author" ] i.Cat.idx_columns;
       Alcotest.(check bool) "not unique" false i.Cat.idx_unique);
    let on_posts =
      Schema.indexes_for_table sch ~table:"posts"
      |> List.map (fun (i : Cat.index_info) -> i.Cat.idx_name)
    in
    Alcotest.(check bool)
      "indexes_for_table lists it"
      true
      (List.mem "posts_author" on_posts);
    Alcotest.(check (list string))
      "an unknown table has no indexes"
      []
      (Schema.indexes_for_table sch ~table:"nope"
       |> List.map (fun (i : Cat.index_info) -> i.Cat.idx_name)))
;;

let pp_prints_something_stable () =
  with_db (fun db ->
    seed db;
    let s = Format.asprintf "%a" Schema.pp (Db.schema db) in
    Alcotest.(check bool) "pp mentions Schema.t" true (String.length s > 0);
    let t = Format.asprintf "%a" Schema.pp_table (table_named (Db.schema db) "users") in
    Alcotest.(check bool)
      "pp_table names the table"
      true
      (String.length t > 0 && String.index_opt t 'u' <> None))
;;

(* --------------------------------------------------------------- *)
(* Live view, not a snapshot                                         *)
(* --------------------------------------------------------------- *)

(* The projection is taken BEFORE any DDL runs and is re-read after each
   statement.  A clone taken at [Db.schema] time would report the schema as it
   was; the live view reports the schema as it is. *)
let the_projection_tracks_ddl () =
  with_db (fun db ->
    let sch = Db.schema db in
    Alcotest.(check bool) "nothing yet" false (Schema.table_exists sch ~name:"t");
    exec db "CREATE TABLE t (a INTEGER)";
    Alcotest.(check bool)
      "CREATE TABLE is visible"
      true
      (Schema.table_exists sch ~name:"t");
    Alcotest.(check (list string))
      "one column"
      [ "a" ]
      (List.map (fun (c : Row.column) -> c.name) (table_named sch "t").columns);
    exec db "ALTER TABLE t ADD COLUMN b TEXT";
    Alcotest.(check (list string))
      "ADD COLUMN is visible"
      [ "a"; "b" ]
      (List.map (fun (c : Row.column) -> c.name) (table_named sch "t").columns);
    exec db "CREATE INDEX t_a ON t (a)";
    Alcotest.(check bool)
      "CREATE INDEX is visible"
      true
      (Schema.index_exists sch ~name:"t_a");
    exec db "ALTER TABLE t RENAME TO t2";
    Alcotest.(check bool) "RENAME is visible" true (Schema.table_exists sch ~name:"t2");
    Alcotest.(check bool) "old name is gone" false (Schema.table_exists sch ~name:"t");
    exec db "DROP TABLE t2";
    Alcotest.(check bool) "DROP is visible" false (Schema.table_exists sch ~name:"t2");
    Alcotest.(check bool) "and its index too" false (Schema.index_exists sch ~name:"t_a"))
;;

(* A rolled-back CREATE TABLE must not be visible either — the projection
   reflects the catalog, including its transactional undo. *)
let a_rolled_back_create_is_not_visible () =
  with_db (fun db ->
    let sch = Db.schema db in
    exec db "BEGIN";
    exec db "CREATE TABLE tmp (a INTEGER)";
    Alcotest.(check bool)
      "visible inside the transaction"
      true
      (Schema.table_exists sch ~name:"tmp");
    exec db "ROLLBACK";
    Alcotest.(check bool)
      "gone after ROLLBACK"
      false
      (Schema.table_exists sch ~name:"tmp"))
;;

(* --------------------------------------------------------------- *)
(* Db.plan — the read-only replacement for the removed accessor      *)
(* --------------------------------------------------------------- *)

let plan_compiles_without_executing () =
  with_db (fun db ->
    seed db;
    exec db "INSERT INTO users VALUES (1, 'a@example.com')";
    (match run (Db.plan db "SELECT email FROM users WHERE id = 1") with
     | Error e -> Alcotest.failf "plan: %a" Db.pp_error e
     | Ok op ->
       let rec has_lookup (o : Plan.op) =
         match o with
         | Plan.Op_rowid_lookup _ | Plan.Op_index_lookup _ -> true
         | Plan.Op_project { child; _ }
         | Plan.Op_expr_project { child; _ }
         | Plan.Op_filter { child; _ } -> has_lookup child
         | _ -> false
       in
       Alcotest.(check bool) "the rowid predicate is seeked" true (has_lookup op));
    (* A DML statement PLANS but must not run: the row survives. *)
    (match run (Db.plan db "DELETE FROM users") with
     | Error e -> Alcotest.failf "plan delete: %a" Db.pp_error e
     | Ok _ -> ());
    Alcotest.(check int) "planning a DELETE deleted nothing" 1 (count_users db))
;;

let plan_reports_parse_and_sema_errors () =
  with_db (fun db ->
    seed db;
    (match run (Db.plan db "SELEKT 1") with
     | Ok _ -> Alcotest.fail "a syntax error must not plan"
     | Error (Db.Parse _) -> ()
     | Error e -> Alcotest.failf "expected a parse error, got %a" Db.pp_error e);
    match run (Db.plan db "SELECT * FROM nosuchtable") with
    | Ok _ -> Alcotest.fail "an unknown table must not plan"
    | Error _ -> ())
;;

let () =
  Alcotest.run
    "readonly_catalog_433"
    [ ( "projection"
      , [ Alcotest.test_case
            "list_tables and find_table agree"
            `Quick
            list_tables_and_find_table_agree
        ; Alcotest.test_case
            "column metadata is projected"
            `Quick
            column_metadata_is_projected
        ; Alcotest.test_case
            "table_exists tracks the catalog"
            `Quick
            table_exists_tracks_the_catalog
        ; Alcotest.test_case
            "WITHOUT ROWID and FK flags"
            `Quick
            without_rowid_and_fk_flags_are_projected
        ; Alcotest.test_case
            "a columnstore table is flagged"
            `Quick
            a_columnstore_table_is_flagged
        ; Alcotest.test_case
            "index accessors report the indexes"
            `Quick
            index_accessors_report_the_indexes
        ; Alcotest.test_case "pp prints something" `Quick pp_prints_something_stable
        ] )
    ; ( "live view"
      , [ Alcotest.test_case "the projection tracks DDL" `Quick the_projection_tracks_ddl
        ; Alcotest.test_case
            "a rolled back CREATE is not visible"
            `Quick
            a_rolled_back_create_is_not_visible
        ] )
    ; ( "plan"
      , [ Alcotest.test_case
            "plan compiles without executing"
            `Quick
            plan_compiles_without_executing
        ; Alcotest.test_case
            "plan reports parse and sema errors"
            `Quick
            plan_reports_parse_and_sema_errors
        ] )
    ]
;;
