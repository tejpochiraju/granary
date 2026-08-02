(** #548: [Db.dump] of a pre-#530 database emits a NOT NULL its own rows violate.

    #530 made every PRIMARY KEY column imply NOT NULL, and [Catalog.open_]
    re-derives that flag from the implicit PK index when it loads an older file
    (#533). Reading such a file is fine — the drift check compares both catalog
    copies exactly as stored, and nothing is written back. DUMPING it is not:
    [Db.dump] renders the CURRENT declaration ([k TEXT NOT NULL PRIMARY KEY]) and
    then emits the stored rows, NULLs included. The schema line and the data
    lines contradict each other, and the script dies on the first offending row.

    Three spellings could produce such a file, all of them nullable before #530:
    a table-level [PRIMARY KEY (k)], every column of a composite
    [PRIMARY KEY (a, b)], and a column added by
    [ALTER TABLE ... ADD COLUMN ... PRIMARY KEY].

    {b The decision: refuse.} [Db.dump] fails with a diagnostic naming the table,
    the column, and the [DELETE] that repairs it, rather than emitting a script
    that cannot replay. The considered alternative was to emit the offending
    column {e without} its NOT NULL — that restores, but by downgrading the
    schema without saying so, and a silent degradation in this exact area is what
    #533 (a dumped key quietly becoming no key) and #553 (a dumped index quietly
    naming a dead column) were both about. A database whose rows contradict its
    own schema is not describable in SQL; refusing is the only answer that never
    lies. [~data_only:true] remains the escape hatch: it emits no schema, so
    there is nothing for the rows to contradict, and the data can still be
    extracted from an unrepaired file.

    The fixture is built the only way it can be: the rows go in through SQL while
    the column is genuinely nullable, and the implicit PK index — the record
    [Catalog.open_] re-derives the flag from — is added afterwards through the
    catalog API. Going through [Db] alone cannot produce it, because the engine
    now (correctly) refuses the NULL. *)

module Db = Granary.Db
module Cat = Granary_catalog.Catalog

let run = Lwt_main.run

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let open_db path =
  match run (Granary_unix.open_file ~path ()) with
  | Ok db -> db
  | Error e -> Alcotest.failf "open %s: %a" path Db.pp_error e
;;

let close_db db =
  try run (Db.close db) with
  | _ -> ()
;;

(* Register [cols] as the implicit PRIMARY KEY index of [table], directly in the
   catalog of an already-populated file.  This is the pre-#530 shape: the columns
   were stored with [not_null = false] (SQL created them nullable), and the index
   is the only record that they are the key — which is exactly what
   [Catalog.open_] re-derives the NOT NULL from on the next open. *)
let add_implicit_pk_index path ~table ~cols =
  run
    (let open Lwt.Syntax in
     let* store =
       let* r = Granary_unix.Store.open_file ~path () in
       match r with
       | Ok s -> Lwt.return s
       | Error _ -> Alcotest.failf "cannot reopen store %s" path
     in
     let* cat = Cat.open_ store in
     let* r =
       Cat.create_index
         cat
         ~name:(Printf.sprintf "__pk_%s_%s_0" table (String.concat "_" cols))
         ~table
         ~columns:cols
         ~unique:true
         ~expr_flags:(List.map (fun _ -> false) cols)
         ~where_sql:None
         ~origin:`Implicit_pk
     in
     match r with
     | Ok _ -> Granary_store.Store.close store
     | Error m -> Alcotest.failf "create_index: %s" m)
;;

(* Build a file in which [stock.si] is declared NOT NULL (re-derived from the
   composite PK index) while one stored row holds NULL there, and hand the
   reopened database to [f]. *)
let with_legacy_null_key_db f =
  let path = Filename.temp_file "granary_548_" ".db" in
  Sys.remove path;
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () ->
       let db = open_db path in
       exec db "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER)";
       exec db "INSERT INTO stock VALUES (1, 2, 50)";
       exec db "INSERT INTO stock VALUES (1, NULL, 60)";
       close_db db;
       add_implicit_pk_index path ~table:"stock" ~cols:[ "sw"; "si" ];
       let db = open_db path in
       Fun.protect ~finally:(fun () -> close_db db) (fun () -> f db))
;;

(* The premise: the reopened database really does declare the column NOT NULL
   while holding a NULL in it.  Without this the refusal below would be vacuous. *)
let the_fixture_is_actually_inconsistent () =
  with_legacy_null_key_db (fun db ->
    let ddl =
      match run (Db.dump_to_string db ~schema_only:true ()) with
      | Ok s -> s
      | Error e -> Alcotest.failf "schema_only dump: %a" Db.pp_error e
    in
    Alcotest.(check bool)
      (Printf.sprintf "the schema declares si NOT NULL (%s)" ddl)
      true
      (contains ~needle:"si INTEGER NOT NULL" ddl);
    let data =
      match run (Db.dump_to_string db ~data_only:true ()) with
      | Ok s -> s
      | Error e -> Alcotest.failf "data_only dump: %a" Db.pp_error e
    in
    Alcotest.(check bool)
      (Printf.sprintf "and a stored row holds NULL there (%s)" data)
      true
      (contains ~needle:"NULL" data))
;;

(* What the refusal is protecting against, demonstrated rather than asserted:
   the script the dump WOULD have emitted — its own schema section followed by
   its own data section — does not replay.  The two halves are each produced by
   [Db.dump] and each individually fine; it is only their combination, which is
   what a default dump emits, that is unrestorable. *)
let replay_has_a_failing_statement script =
  let dst = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () -> close_db dst)
    (fun () ->
       List.exists
         (fun stmt ->
            let s = String.trim stmt in
            s <> "" && Result.is_error (run (Db.execute dst s)))
         (String.split_on_char ';' script))
;;

let the_script_the_refusal_replaces_does_not_replay () =
  with_legacy_null_key_db (fun db ->
    let part ~schema_only ~data_only =
      match run (Db.dump_to_string db ~schema_only ~data_only ()) with
      | Ok s -> s
      | Error e -> Alcotest.failf "partial dump: %a" Db.pp_error e
    in
    let script =
      part ~schema_only:true ~data_only:false ^ part ~schema_only:false ~data_only:true
    in
    Alcotest.(check bool)
      "replaying schema-then-data fails on the row that violates NOT NULL"
      true
      (replay_has_a_failing_statement script))
;;

(* The refusal itself, and that it says something an operator can act on. *)
let full_dump_is_refused_with_a_specific_diagnostic () =
  with_legacy_null_key_db (fun db ->
    match run (Db.dump_to_string db ()) with
    | Ok script ->
      Alcotest.failf "expected a refusal, got a dump that cannot replay:\n%s" script
    | Error e ->
      let msg = Format.asprintf "%a" Db.pp_error e in
      List.iter
        (fun needle ->
           Alcotest.(check bool)
             (Printf.sprintf "the diagnostic mentions %S (got %S)" needle msg)
             true
             (contains ~needle msg))
        (* Both repairs are named.  Refusing exists to stop information being
           destroyed silently, so a message that offers only the destructive
           repair works against its own reason for existing. *)
        [ "stock"; "si"; "NOT NULL"; "#548"; "UPDATE stock SET si"; "DELETE FROM stock" ])
;;

(* The escape hatch: the data can still be got out of an unrepaired file. *)
let data_only_dump_still_works () =
  with_legacy_null_key_db (fun db ->
    match run (Db.dump_to_string db ~data_only:true ()) with
    | Error e -> Alcotest.failf "data_only dump refused: %a" Db.pp_error e
    | Ok script ->
      Alcotest.(check bool)
        "both rows are present"
        true
        (contains ~needle:"INSERT INTO stock VALUES(1,2,50)" script
         && contains ~needle:"INSERT INTO stock VALUES(1,NULL,60)" script))
;;

(* A schema-only dump emits no rows, so it has nothing to contradict. *)
let schema_only_dump_still_works () =
  with_legacy_null_key_db (fun db ->
    match run (Db.dump_to_string db ~schema_only:true ()) with
    | Error e -> Alcotest.failf "schema_only dump refused: %a" Db.pp_error e
    | Ok _ -> ())
;;

(* And the repair the diagnostic names actually works: after it, the same
   database dumps and the script replays clean. *)
let dump_works_after_the_named_repair () =
  with_legacy_null_key_db (fun db ->
    exec db "DELETE FROM stock WHERE si IS NULL";
    match run (Db.dump_to_string db ()) with
    | Error e -> Alcotest.failf "dump still refused after repair: %a" Db.pp_error e
    | Ok script ->
      let dst = run (Db.open_in_memory ()) in
      Fun.protect
        ~finally:(fun () -> close_db dst)
        (fun () ->
           List.iter
             (fun stmt ->
                let s = String.trim stmt in
                if s <> "" then exec dst s)
             (String.split_on_char ';' script)))
;;

(* The check must not fire on a healthy database: a NOT NULL column with no NULL
   in it dumps exactly as before.  A refusal that triggers on well-formed files
   would be worse than the bug. *)
let healthy_not_null_database_dumps_normally () =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () -> close_db db)
    (fun () ->
       exec db "CREATE TABLE t (a INTEGER NOT NULL, b TEXT, PRIMARY KEY (a))";
       exec db "INSERT INTO t VALUES (1, 'x')";
       exec db "INSERT INTO t VALUES (2, NULL)";
       match run (Db.dump_to_string db ()) with
       | Error e -> Alcotest.failf "healthy dump refused: %a" Db.pp_error e
       | Ok script ->
         Alcotest.(check bool)
           "the NULL in the NULLABLE column is still dumped"
           true
           (contains ~needle:"INSERT INTO t VALUES(2,NULL)" script))
;;

let () =
  Alcotest.run
    "dump_null_pk_548"
    [ ( "548"
      , List.map
          (fun (n, f) -> Alcotest.test_case n `Quick f)
          [ "the fixture is inconsistent", the_fixture_is_actually_inconsistent
          ; ( "the replaced script does not replay"
            , the_script_the_refusal_replaces_does_not_replay )
          ; "full dump is refused", full_dump_is_refused_with_a_specific_diagnostic
          ; "data_only still works", data_only_dump_still_works
          ; "schema_only still works", schema_only_dump_still_works
          ; "dump works after the repair", dump_works_after_the_named_repair
          ; "healthy database is unaffected", healthy_not_null_database_dumps_normally
          ] )
    ]
;;
