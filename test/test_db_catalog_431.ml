(** #431: the engine exposes a database's schema for read-only schema/type
    projection. A downstream consumer (camel's hook type environment) projects
    every table's columns — name, type, NOT NULL — from the catalog a [Db.t]
    already holds, so it needs an accessor to reach it.

    #433: that accessor used to be [Db.catalog], handing out the live mutable
    [Catalog.t]; it is now [Db.schema], handing out the read-only
    {!Granary.Schema.t}. This file keeps #431's own assertion — the projection
    sees a table created through SQL, with its columns' names, types and NOT
    NULL flags — against the new accessor. *)

open Lwt.Syntax
module Row = Granary_encoding.Row
module Schema = Granary.Schema

let run = Lwt_main.run

let test_catalog_sees_created_table () =
  run
    (let* db = Granary.Db.open_in_memory () in
     let* r =
       Granary.Db.execute
         db
         "CREATE TABLE users (id INTEGER PRIMARY KEY, email TEXT NOT NULL)"
     in
     (match r with
      | Ok () -> ()
      | Error e -> Alcotest.failf "create table: %a" Granary.Db.pp_error e);
     let* tables = Schema.list_tables (Granary.Db.schema db) in
     let users = List.find_opt (fun (m : Schema.table) -> m.name = "users") tables in
     match users with
     | None -> Alcotest.fail "schema should list the created table"
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
            "schema sees created table"
            `Quick
            test_catalog_sees_created_table
        ] )
    ]
;;
