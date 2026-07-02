(** #431: [Db.catalog] exposes the live in-memory catalog for read-only
    schema/type projection. A downstream consumer (camel's hook type
    environment) projects every table's columns — name, type, NOT NULL — from
    the catalog a [Db.t] already holds, so it needs an accessor to reach it. *)

open Lwt.Syntax
module Row = Sqlocaml_encoding.Row
module Catalog = Sqlocaml_catalog.Catalog

let run = Lwt_main.run

let test_catalog_sees_created_table () =
  run
    (let* db = Sqlocaml.Db.open_in_memory () in
     let* r =
       Sqlocaml.Db.execute
         db
         "CREATE TABLE users (id INTEGER PRIMARY KEY, email TEXT NOT NULL)"
     in
     (match r with
      | Ok () -> ()
      | Error e -> Alcotest.failf "create table: %a" Sqlocaml.Db.pp_error e);
     let* tables = Catalog.list_tables (Sqlocaml.Db.catalog db) in
     let users =
       List.find_opt (fun (m : Catalog.table_meta) -> m.name = "users") tables
     in
     match users with
     | None -> Alcotest.fail "catalog should list the created table"
     | Some m ->
       let names = List.map (fun (c : Row.column) -> c.name) m.columns in
       Alcotest.(check (list string)) "column names" [ "id"; "email" ] names;
       let email = List.find (fun (c : Row.column) -> c.name = "email") m.columns in
       Alcotest.(check bool) "email is Text" true (email.ty = Row.Text);
       Alcotest.(check bool) "email is NOT NULL" true email.not_null;
       Lwt.return_unit)
;;

let () =
  Alcotest.run
    "db_catalog_431"
    [ ( "accessor"
      , [ Alcotest.test_case
            "catalog sees created table"
            `Quick
            test_catalog_sees_created_table
        ] )
    ]
;;
