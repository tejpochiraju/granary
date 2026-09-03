(** #553: [ALTER TABLE ... RENAME COLUMN] must remap every stored reference to
    the renamed column, not just the column record itself.

    A column name is recorded in more places than [_sys_columns]:

    {v
      index_info.idx_columns      column names of an index (plain columns)
      index_info.idx_columns      expression SQL (expression indexes)
      index_info.idx_where_sql    WHERE clause of a partial index
      Row.column.check_sql        CHECK constraint expression
      Row.column.generated_as     GENERATED ALWAYS AS expression
      fk_constraint.fk_local_cols this table's side of a FOREIGN KEY
      fk_constraint.fk_parent_cols any OTHER table's reference INTO this one
    v}

    Before the fix only the [_sys_columns] record and the in-memory
    [table_meta] were rewritten, so every one of the above kept naming a column
    the table no longer has. The implicit PRIMARY KEY index is the worst case:
    since #533 the DDL renderer reads it as the record of the table's key, so a
    stale one either emits [PRIMARY KEY (k, j)] naming a dead column (losing
    every row on restore) or — with #533's guard — silently degrades the dumped
    schema to no key at all.

    The oracle throughout is [sqlite_master.sql] plus a dump/replay round trip:
    the DDL a database reports must be valid SQL against the database it
    describes, and replaying it must reproduce the same schema and rows.

    Also covers [ALTER TABLE ... RENAME TO], which had the analogous misses: a
    child table's [fk_parent_table] still named the old table, and the primary
    FK record was keyed by the old table name so the constraints vanished on
    the next open. *)

module Db = Granary.Db
module Cat = Granary_catalog.Catalog
module Schema = Granary.Schema

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

let vstr = function
  | Db.V_int i -> Printf.sprintf "i:%Ld" i
  | Db.V_real f -> Printf.sprintf "r:%.17g" f
  | Db.V_text s -> Printf.sprintf "t:%s" s
  | Db.V_null -> "null"
  | Db.V_blob b -> Printf.sprintf "b:%s" (String.escaped (Bytes.to_string b))
;;

let rows db sql =
  run
    (let open Lwt.Syntax in
     let* s = Db.query db sql in
     let* rows = Lwt_stream.to_list (unwrap s) in
     Lwt.return
       (List.map (fun r -> String.concat "," (Array.to_list (Array.map vstr r))) rows))
;;

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

(* Every [sqlite_master.sql] of the database, one string per object, sorted. *)
let all_ddl db =
  List.sort compare (rows db "SELECT sql FROM sqlite_master WHERE sql IS NOT NULL")
;;

(* The [sqlite_master.sql] of one named object. *)
let ddl_of db name =
  match
    rows db (Printf.sprintf "SELECT sql FROM sqlite_master WHERE name = '%s'" name)
  with
  | [ s ] -> s
  | [] -> Alcotest.failf "no sqlite_master row named %s" name
  | l -> Alcotest.failf "%d sqlite_master rows named %s" (List.length l) name
;;

(* The crux for a whole class of these: no stored DDL may name a column its
   table does not have.  Rather than spell out which spelling is expected, we
   assert the absence of the OLD name across every DDL string — a stale
   reference is by definition the old name surviving somewhere it is read as a
   column. *)
let no_ddl_mentions db word =
  List.iter
    (fun sql ->
       if contains ~needle:word sql
       then Alcotest.failf "stale reference to %S survives in DDL: %s" word sql)
    (all_ddl db)
;;

(* ------------------------------------------------------------------ *)
(* Splitting and replaying a dump — the round-trip oracle              *)
(* ------------------------------------------------------------------ *)

let split_statements sql =
  let n = String.length sql in
  let buf = Buffer.create 256 in
  let out = ref [] in
  let in_str = ref false in
  let i = ref 0 in
  while !i < n do
    let c = sql.[!i] in
    if !in_str
    then
      if c = '\''
      then
        if !i + 1 < n && sql.[!i + 1] = '\''
        then (
          Buffer.add_string buf "''";
          incr i)
        else (
          Buffer.add_char buf c;
          in_str := false)
      else Buffer.add_char buf c
    else if c = '\''
    then (
      Buffer.add_char buf c;
      in_str := true)
    else if c = ';'
    then (
      let s = String.trim (Buffer.contents buf) in
      if s <> "" then out := s :: !out;
      Buffer.clear buf)
    else Buffer.add_char buf c;
    incr i
  done;
  let s = String.trim (Buffer.contents buf) in
  if s <> "" then out := s :: !out;
  List.rev !out
;;

let user_tables db =
  rows db "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
  |> List.map (fun s ->
    match String.split_on_char ':' s with
    | "t" :: rest -> String.concat ":" rest
    | _ -> s)
;;

let table_data db name =
  List.sort compare (rows db (Printf.sprintf "SELECT * FROM \"%s\"" name))
;;

(* DDL of everything but indexes.  Index NAMES may legitimately differ across a
   round trip — a table-level PRIMARY KEY is re-emitted as a table constraint
   whose backing index is minted afresh from the CURRENT column names — so index
   fidelity is compared on shape (see [index_shapes]) rather than on text. *)
let object_ddl db =
  List.sort
    compare
    (rows
       db
       "SELECT sql FROM sqlite_master WHERE type IN ('table','view','trigger') ORDER BY \
        sql")
;;

(* Each index reduced to what it means: everything from " ON " onwards. *)
let index_shapes db =
  rows db "SELECT sql FROM sqlite_master WHERE type='index'"
  |> List.map (fun sql ->
    let n = String.length sql in
    let rec find i =
      if i + 4 > n
      then sql
      else if String.sub sql i 4 = " ON "
      then String.sub sql i (n - i)
      else find (i + 1)
    in
    find 0)
  |> List.sort compare
;;

(* Dump [orig], replay into a fresh in-memory database, and assert both agree
   on their whole reported DDL and on every table's rows. *)
let assert_roundtrip orig =
  let script = unwrap (run (Db.dump_to_string orig ())) in
  let restored = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close restored) with
      | _ -> ())
    (fun () ->
       List.iter
         (fun stmt ->
            match run (Db.execute restored stmt) with
            | Ok () -> ()
            | Error e ->
              Alcotest.failf
                "replay %S: %a\n--- full script ---\n%s"
                stmt
                Db.pp_error
                e
                script)
         (split_statements script);
       Alcotest.(check (list string))
         "same tables"
         (user_tables orig)
         (user_tables restored);
       List.iter
         (fun t ->
            Alcotest.(check (list string))
              (Printf.sprintf "rows of %s" t)
              (table_data orig t)
              (table_data restored t))
         (user_tables orig);
       Alcotest.(check (list string))
         "same table/view/trigger DDL"
         (object_ddl orig)
         (object_ddl restored);
       Alcotest.(check (list string))
         "same index shapes"
         (index_shapes orig)
         (index_shapes restored))
;;

(* ------------------------------------------------------------------ *)
(* idx_columns — the confirmed case                                     *)
(* ------------------------------------------------------------------ *)

(* The column list of an index, read back out of its rendered DDL: everything
   between the first '(' after " ON " and its matching ')'.  Index NAMES are
   minted at CREATE time and deliberately not rewritten, so asserting on the
   name would pin a non-fact; the column list is what readers resolve. *)
let index_columns db name =
  let sql = ddl_of db name in
  let n = String.length sql in
  let rec find_on i =
    if i + 4 > n
    then Alcotest.failf "no ON clause in %s" sql
    else if String.sub sql i 4 = " ON "
    then i + 4
    else find_on (i + 1)
  in
  let rec find_open i =
    if i >= n
    then Alcotest.failf "no column list in %s" sql
    else if sql.[i] = '('
    then i + 1
    else find_open (i + 1)
  in
  let start = find_open (find_on 0) in
  let rec find_close i =
    if i >= n
    then Alcotest.failf "unterminated column list in %s" sql
    else if sql.[i] = ')'
    then i
    else find_close (i + 1)
  in
  String.sub sql start (find_close start - start)
;;

(* The name of the sole `Implicit_pk index of a table, whatever it is called. *)
let pk_index_name db table =
  match
    rows
      db
      (Printf.sprintf
         "SELECT name FROM sqlite_master WHERE type='index' AND tbl_name='%s'"
         table)
  with
  | [ s ] ->
    (match String.split_on_char ':' s with
     | "t" :: rest -> String.concat ":" rest
     | _ -> s)
  | l -> Alcotest.failf "expected exactly one index on %s, got %d" table (List.length l)
;;

(* The issue's own reproduction: the implicit index of a composite PRIMARY KEY
   keeps naming the pre-rename column. *)
let implicit_pk_index_follows_the_rename () =
  with_db (fun db ->
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    let ix = pk_index_name db "c" in
    exec db "ALTER TABLE c RENAME COLUMN j TO jj";
    Alcotest.(check string)
      "the implicit PK index names the new column"
      "k, jj"
      (index_columns db ix);
    Alcotest.(check bool)
      "the table's key is still composite and now names jj"
      true
      (contains ~needle:"PRIMARY KEY (k, jj)" (ddl_of db "c")))
;;

(* The single-column table-level spelling: one implicit index, one column. *)
let implicit_pk_index_single_column_follows_the_rename () =
  with_db (fun db ->
    exec db "CREATE TABLE b (k TEXT, v INTEGER, PRIMARY KEY (k))";
    let ix = pk_index_name db "b" in
    exec db "ALTER TABLE b RENAME COLUMN k TO kk";
    Alcotest.(check string) "index follows" "kk" (index_columns db ix))
;;

let user_index_follows_the_rename () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
    exec db "CREATE INDEX ix ON t (b)";
    exec db "ALTER TABLE t RENAME COLUMN b TO bb";
    Alcotest.(check string) "index names the new column" "bb" (index_columns db "ix"))
;;

let implicit_unique_index_follows_the_rename () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER, UNIQUE (b))";
    let ix = pk_index_name db "t" in
    exec db "ALTER TABLE t RENAME COLUMN b TO bb";
    Alcotest.(check string) "index follows" "bb" (index_columns db ix);
    (* and the constraint still bites, on the new name *)
    exec db "INSERT INTO t VALUES (1, 1)";
    match run (Db.execute db "INSERT INTO t VALUES (2, 1)") with
    | Ok () -> Alcotest.fail "UNIQUE no longer enforced after RENAME COLUMN"
    | Error _ -> ())
;;

let multi_column_user_index_follows_the_rename () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER, c INTEGER)";
    exec db "CREATE INDEX ix ON t (a, b, c)";
    exec db "ALTER TABLE t RENAME COLUMN b TO bb";
    Alcotest.(check string)
      "only the renamed position changes"
      "a, bb, c"
      (index_columns db "ix"))
;;

(* ------------------------------------------------------------------ *)
(* SQL text: partial-index WHERE, CHECK, generated columns              *)
(* ------------------------------------------------------------------ *)

let partial_index_where_follows_the_rename () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
    exec db "CREATE INDEX ix ON t (a) WHERE b > 0";
    exec db "ALTER TABLE t RENAME COLUMN b TO bb";
    let sql = ddl_of db "ix" in
    Alcotest.(check bool)
      (Printf.sprintf "WHERE names bb, not b (%s)" sql)
      true
      (contains ~needle:"bb" sql))
;;

let check_constraint_follows_the_rename () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER CHECK (b > 0))";
    exec db "ALTER TABLE t RENAME COLUMN b TO bb";
    no_ddl_mentions db "(b ";
    (* the constraint must still be enforced under the new name *)
    (match run (Db.execute db "INSERT INTO t VALUES (1, -1)") with
     | Ok () -> Alcotest.fail "CHECK no longer enforced after RENAME COLUMN"
     | Error _ -> ());
    exec db "INSERT INTO t VALUES (1, 5)")
;;

let generated_column_follows_the_rename () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, g INTEGER GENERATED ALWAYS AS (a * 2) VIRTUAL)";
    exec db "ALTER TABLE t RENAME COLUMN a TO aa";
    exec db "INSERT INTO t (aa) VALUES (21)";
    Alcotest.(check (list string))
      "generated value still computed"
      [ "i:21,i:42" ]
      (table_data db "t"))
;;

(* ------------------------------------------------------------------ *)
(* Foreign keys                                                         *)
(* ------------------------------------------------------------------ *)

let fk_local_cols_follow_the_rename () =
  with_db (fun db ->
    exec db "CREATE TABLE p (id INTEGER PRIMARY KEY)";
    exec db "CREATE TABLE c (pid INTEGER REFERENCES p(id))";
    exec db "ALTER TABLE c RENAME COLUMN pid TO parent";
    let sql = ddl_of db "c" in
    Alcotest.(check bool)
      (Printf.sprintf "FOREIGN KEY names the new local column (%s)" sql)
      true
      (contains ~needle:"FOREIGN KEY (parent)" sql))
;;

let fk_parent_cols_follow_the_rename () =
  with_db (fun db ->
    exec db "CREATE TABLE p (id INTEGER PRIMARY KEY)";
    exec db "CREATE TABLE c (pid INTEGER REFERENCES p(id))";
    exec db "ALTER TABLE p RENAME COLUMN id TO ident";
    let sql = ddl_of db "c" in
    Alcotest.(check bool)
      (Printf.sprintf "the child's REFERENCES names the new parent column (%s)" sql)
      true
      (contains ~needle:"REFERENCES p(ident)" sql))
;;

(* ------------------------------------------------------------------ *)
(* RENAME TO — the analogous misses on a table rename                   *)
(* ------------------------------------------------------------------ *)

let rename_table_updates_child_fk_parent_table () =
  with_db (fun db ->
    exec db "CREATE TABLE p (id INTEGER PRIMARY KEY)";
    exec db "CREATE TABLE c (pid INTEGER REFERENCES p(id))";
    exec db "ALTER TABLE p RENAME TO parent";
    let sql = ddl_of db "c" in
    Alcotest.(check bool)
      (Printf.sprintf "the child references the new table name (%s)" sql)
      true
      (contains ~needle:"REFERENCES parent(" sql))
;;

(* The one FK shape the child sweep cannot reach: a table that references
   ITSELF.  Its FK is in its own record, not in any other table's, so a rename
   that only re-points children leaves it naming a parent table that no longer
   exists — and the table becomes permanently un-insertable, because the FK
   check looks the parent up by name. *)
let rename_table_updates_its_own_self_reference () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE e (id INTEGER PRIMARY KEY, mgr INTEGER REFERENCES e(id))";
    exec db "INSERT INTO e (id, mgr) VALUES (1, NULL)";
    exec db "ALTER TABLE e RENAME TO emp";
    let sql = ddl_of db "emp" in
    Alcotest.(check bool)
      (Printf.sprintf "the self-reference follows the table (%s)" sql)
      true
      (contains ~needle:"REFERENCES emp(" sql);
    (* and the constraint is live in both directions on the new name *)
    exec db "INSERT INTO emp (id, mgr) VALUES (2, 1)";
    match run (Db.execute db "INSERT INTO emp (id, mgr) VALUES (3, 99)") with
    | Ok () -> Alcotest.fail "the self-referencing FK stopped being enforced"
    | Error _ -> ())
;;

let rename_table_self_reference_survives_reopen () =
  let path = Filename.temp_file "granary_553_self_" ".db" in
  Sys.remove path;
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () ->
       let open_it () =
         match run (Granary_unix.open_file ~path ()) with
         | Ok db -> db
         | Error e -> Alcotest.failf "open %s: %a" path Db.pp_error e
       in
       let db = open_it () in
       exec db "CREATE TABLE e (id INTEGER PRIMARY KEY, mgr INTEGER REFERENCES e(id))";
       exec db "ALTER TABLE e RENAME TO emp";
       let before = ddl_of db "emp" in
       (try run (Db.close db) with
        | _ -> ());
       let db = open_it () in
       Fun.protect
         ~finally:(fun () ->
           try run (Db.close db) with
           | _ -> ())
         (fun () ->
            Alcotest.(check string)
              "the re-pointed self-reference is durable"
              before
              (ddl_of db "emp")))
;;

let rename_table_keeps_its_own_fks_across_reopen () =
  let path = Filename.temp_file "granary_553_" ".db" in
  Sys.remove path;
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () ->
       let db =
         match run (Granary_unix.open_file ~path ()) with
         | Ok db -> db
         | Error e -> Alcotest.failf "open_file: %a" Db.pp_error e
       in
       exec db "CREATE TABLE p (id INTEGER PRIMARY KEY)";
       exec db "CREATE TABLE c (pid INTEGER REFERENCES p(id))";
       exec db "ALTER TABLE c RENAME TO child";
       let before = ddl_of db "child" in
       (try run (Db.close db) with
        | _ -> ());
       let db =
         match run (Granary_unix.open_file ~path ()) with
         | Ok db -> db
         | Error e -> Alcotest.failf "reopen: %a" Db.pp_error e
       in
       Fun.protect
         ~finally:(fun () ->
           try run (Db.close db) with
           | _ -> ())
         (fun () ->
            Alcotest.(check string)
              "the renamed table keeps its FOREIGN KEY across a reopen"
              before
              (ddl_of db "child")))
;;

(* ------------------------------------------------------------------ *)
(* Durability and transactionality of the remap                         *)
(* ------------------------------------------------------------------ *)

let rename_column_remap_survives_reopen () =
  let path = Filename.temp_file "granary_553_" ".db" in
  Sys.remove path;
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () ->
       let db =
         match run (Granary_unix.open_file ~path ()) with
         | Ok db -> db
         | Error e -> Alcotest.failf "open_file: %a" Db.pp_error e
       in
       exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
       exec db "CREATE INDEX ix ON c (v) WHERE j IS NOT NULL";
       exec db "ALTER TABLE c RENAME COLUMN j TO jj";
       let before = all_ddl db in
       (try run (Db.close db) with
        | _ -> ());
       let db =
         match run (Granary_unix.open_file ~path ()) with
         | Ok db -> db
         | Error e -> Alcotest.failf "reopen: %a" Db.pp_error e
       in
       Fun.protect
         ~finally:(fun () ->
           try run (Db.close db) with
           | _ -> ())
         (fun () -> Alcotest.(check (list string)) "remap is durable" before (all_ddl db)))
;;

(* The remap must be part of the SAME transaction as the column rewrite: a
   ROLLBACK must leave the index naming the original column, not a half-applied
   mixture. *)
let rename_column_rolls_back_with_its_transaction () =
  with_db (fun db ->
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    exec db "CREATE INDEX ix ON c (j)";
    let before = all_ddl db in
    exec db "BEGIN";
    exec db "ALTER TABLE c RENAME COLUMN j TO jj";
    exec db "ROLLBACK";
    Alcotest.(check (list string))
      "rollback restores the whole rename"
      before
      (all_ddl db))
;;

let rename_column_commits_with_its_transaction () =
  with_db (fun db ->
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    exec db "CREATE INDEX ix ON c (j)";
    exec db "BEGIN";
    exec db "ALTER TABLE c RENAME COLUMN j TO jj";
    exec db "COMMIT";
    no_ddl_mentions db "\"j\"")
;;

(* ------------------------------------------------------------------ *)
(* The consequence the issue is really about: dump/restore              *)
(* ------------------------------------------------------------------ *)

let roundtrip_after_rename_column_composite_pk () =
  with_db (fun db ->
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    exec db "INSERT INTO c VALUES ('a', 'b', 1)";
    exec db "INSERT INTO c VALUES ('a', 'c', 2)";
    exec db "ALTER TABLE c RENAME COLUMN j TO jj";
    assert_roundtrip db)
;;

let roundtrip_after_rename_column_user_index () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
    exec db "CREATE INDEX ix ON t (b)";
    exec db "CREATE UNIQUE INDEX ux ON t (a, b)";
    exec db "INSERT INTO t VALUES (1, 2)";
    exec db "ALTER TABLE t RENAME COLUMN b TO bb";
    assert_roundtrip db)
;;

let roundtrip_after_rename_table () =
  with_db (fun db ->
    exec db "CREATE TABLE p (id INTEGER PRIMARY KEY)";
    exec db "CREATE TABLE c (pid INTEGER REFERENCES p(id))";
    exec db "INSERT INTO p VALUES (1)";
    exec db "INSERT INTO c VALUES (1)";
    exec db "ALTER TABLE p RENAME TO parent";
    assert_roundtrip db)
;;

(* The restored database must still enforce the composite key it was dumped
   with — a round trip that keeps the rows but drops the constraint is exactly
   the silent degradation #533's guard leaves behind. *)
let roundtrip_after_rename_preserves_the_key () =
  with_db (fun db ->
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    exec db "INSERT INTO c VALUES ('a', 'b', 1)";
    exec db "ALTER TABLE c RENAME COLUMN j TO jj";
    let script = unwrap (run (Db.dump_to_string db ())) in
    let restored = run (Db.open_in_memory ()) in
    Fun.protect
      ~finally:(fun () ->
        try run (Db.close restored) with
        | _ -> ())
      (fun () ->
         List.iter (fun s -> exec restored s) (split_statements script);
         (match run (Db.execute restored "INSERT INTO c VALUES ('a', 'b', 9)") with
          | Ok () -> Alcotest.fail "restored table no longer enforces its PRIMARY KEY"
          | Error _ -> ());
         match run (Db.execute restored "INSERT INTO c VALUES ('a', NULL, 9)") with
         | Ok () -> Alcotest.fail "restored key column no longer rejects NULL"
         | Error _ -> ()))
;;

(* ------------------------------------------------------------------ *)
(* Catalog-level view of the same facts                                 *)
(* ------------------------------------------------------------------ *)

(* Names of the indexes of [table] whose [idx_columns] still mention [col]. *)
let indexes_naming sch ~table ~col =
  Schema.indexes_for_table sch ~table
  |> List.filter (fun (i : Cat.index_info) -> List.mem col i.Cat.idx_columns)
  |> List.map (fun (i : Cat.index_info) -> i.Cat.idx_name)
;;

let catalog_index_columns_are_remapped () =
  with_db (fun db ->
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    exec db "ALTER TABLE c RENAME COLUMN j TO jj";
    let sch = Db.schema db in
    Alcotest.(check bool)
      "the table has its implicit PK index"
      true
      (Schema.indexes_for_table sch ~table:"c" <> []);
    Alcotest.(check (list string))
      "no index still names the old column"
      []
      (indexes_naming sch ~table:"c" ~col:"j");
    Alcotest.(check bool)
      "and one names the new one"
      true
      (indexes_naming sch ~table:"c" ~col:"jj" <> []))
;;

(* ------------------------------------------------------------------ *)
(* The SQL-text rewriter itself                                         *)
(* ------------------------------------------------------------------ *)

let rw ~old_name ~new_name s = Cat.rewrite_ident_in_sql ~old_name ~new_name s

(* The positions a bare word can occupy that are NOT a column reference, and
   the two delimiter forms that are one.  Each of these was a way to corrupt an
   expression while renaming it. *)
let rewriter_examples () =
  let case ~old_name ~new_name input want =
    Alcotest.(check string)
      (Printf.sprintf "%s [%s -> %s]" input old_name new_name)
      want
      (rw ~old_name ~new_name input)
  in
  case ~old_name:"a" ~new_name:"b" "(a > 0)" "(b > 0)";
  (* a function name is not a column, even when a column shares its name *)
  case ~old_name:"abs" ~new_name:"z" "(abs(a) > 0)" "(abs(a) > 0)";
  (* ... but the argument still is *)
  case ~old_name:"a" ~new_name:"z" "(abs(a) > 0)" "(abs(z) > 0)";
  (* string literals are data, not identifiers *)
  case ~old_name:"a" ~new_name:"z" "(x <> 'a')" "(x <> 'a')";
  case ~old_name:"x" ~new_name:"z" "(x <> 'x')" "(z <> 'x')";
  (* x'..' is a BLOB literal, not the column x *)
  case ~old_name:"x" ~new_name:"z" "(y <> x'6a')" "(y <> x'6a')";
  (* a qualifier names a table, the part after the dot names the column *)
  case ~old_name:"t" ~new_name:"z" "(t.a > 0)" "(t.a > 0)";
  case ~old_name:"a" ~new_name:"z" "(t.a > 0)" "(t.z > 0)";
  (* delimited identifiers are references and are renamed in place *)
  case ~old_name:"a" ~new_name:"z" "(\"a\" > 0)" "(\"z\" > 0)";
  case ~old_name:"a" ~new_name:"z" "(`a` > 0)" "(`z` > 0)";
  (* ... and obey the same position rules as a bare word: a delimited QUALIFIER
     names a table, so ("t"."a") must keep its "t" when renaming a column t *)
  case ~old_name:"t" ~new_name:"z" "(\"t\".\"a\" > 0)" "(\"t\".\"a\" > 0)";
  case ~old_name:"a" ~new_name:"z" "(\"t\".\"a\" > 0)" "(\"t\".\"z\" > 0)";
  case ~old_name:"abs" ~new_name:"z" "(\"abs\"(a) > 0)" "(\"abs\"(a) > 0)";
  (* a longer word that merely starts with the name is a different identifier *)
  case ~old_name:"a" ~new_name:"z" "(a1 > a)" "(a1 > z)";
  case ~old_name:"a" ~new_name:"z" "(ab > a)" "(ab > z)"
;;

let fragments =
  [ "a"
  ; "b"
  ; "ab"
  ; "a1"
  ; "_a"
  ; "abs("
  ; "x'6a'"
  ; "'a'"
  ; "\"a\""
  ; "`b`"
  ; "t.a"
  ; "\"t\".\"a\""
  ; " + "
  ; " > "
  ; "1"
  ; "("
  ; ")"
  ; " AND "
  ; " NOT "
  ; ""
  ]
;;

let gen_sql =
  let open QCheck2.Gen in
  let+ parts = list_size (int_range 0 12) (oneof_list fragments) in
  String.concat "" parts
;;

(* Every string literal of [s], in order.  The rewriter must never touch one. *)
let string_literals s =
  let n = String.length s in
  let out = ref [] in
  let buf = Buffer.create 16 in
  let rec go i in_str =
    if i >= n
    then ()
    else if s.[i] <> '\''
    then (
      if in_str then Buffer.add_char buf s.[i];
      go (i + 1) in_str)
    else if in_str
    then (
      out := Buffer.contents buf :: !out;
      Buffer.clear buf;
      go (i + 1) false)
    else go (i + 1) true
  in
  go 0 false;
  List.rev !out
;;

let prop_absent_name_is_a_no_op =
  QCheck2.Test.make
    ~count:500
    ~name:"a name that does not occur is a no-op"
    gen_sql
    (fun sql -> String.equal sql (rw ~old_name:"zzq" ~new_name:"other" sql))
;;

let prop_rename_to_self_is_identity =
  QCheck2.Test.make
    ~count:500
    ~name:"renaming a name to itself is the identity"
    gen_sql
    (fun sql -> String.equal sql (rw ~old_name:"a" ~new_name:"a" sql))
;;

(* The property that matters for a rename: nothing is lost.  Renaming to a name
   that does not occur, then back, must reproduce the input byte for byte — so
   the rewriter can neither drop a reference nor invent one. *)
let prop_round_trips_through_a_fresh_name =
  QCheck2.Test.make
    ~count:1000
    ~name:"a -> fresh -> a is the identity"
    gen_sql
    (fun sql ->
       let there = rw ~old_name:"a" ~new_name:"zzq" sql in
       String.equal sql (rw ~old_name:"zzq" ~new_name:"a" there))
;;

let prop_string_literals_are_preserved =
  QCheck2.Test.make
    ~count:1000
    ~name:"string literals survive the rewrite unchanged"
    gen_sql
    (fun sql ->
       string_literals sql = string_literals (rw ~old_name:"a" ~new_name:"zzq" sql))
;;

let prop_is_idempotent =
  QCheck2.Test.make
    ~count:500
    ~name:"rewriting twice is rewriting once"
    gen_sql
    (fun sql ->
       let once = rw ~old_name:"a" ~new_name:"zzq" sql in
       String.equal once (rw ~old_name:"a" ~new_name:"zzq" once))
;;

let suite =
  [ ( "553-rewriter"
    , Alcotest.test_case "examples" `Quick rewriter_examples
      :: List.map
           (QCheck_alcotest.to_alcotest ~verbose:false)
           [ prop_absent_name_is_a_no_op
           ; prop_rename_to_self_is_identity
           ; prop_round_trips_through_a_fresh_name
           ; prop_string_literals_are_preserved
           ; prop_is_idempotent
           ] )
  ; ( "553-idx-columns"
    , List.map
        (fun (n, f) -> Alcotest.test_case n `Quick f)
        [ "implicit pk index", implicit_pk_index_follows_the_rename
        ; ( "implicit pk index, single column"
          , implicit_pk_index_single_column_follows_the_rename )
        ; "user index", user_index_follows_the_rename
        ; "implicit unique index", implicit_unique_index_follows_the_rename
        ; "multi-column user index", multi_column_user_index_follows_the_rename
        ; "catalog view", catalog_index_columns_are_remapped
        ] )
  ; ( "553-sql-text"
    , List.map
        (fun (n, f) -> Alcotest.test_case n `Quick f)
        [ "partial index WHERE", partial_index_where_follows_the_rename
        ; "CHECK constraint", check_constraint_follows_the_rename
        ; "generated column", generated_column_follows_the_rename
        ] )
  ; ( "553-foreign-keys"
    , List.map
        (fun (n, f) -> Alcotest.test_case n `Quick f)
        [ "local cols", fk_local_cols_follow_the_rename
        ; "parent cols", fk_parent_cols_follow_the_rename
        ; "rename table updates child", rename_table_updates_child_fk_parent_table
        ; ( "rename table updates its self-reference"
          , rename_table_updates_its_own_self_reference )
        ; ( "the self-reference survives a reopen"
          , rename_table_self_reference_survives_reopen )
        ; "rename table keeps own fks", rename_table_keeps_its_own_fks_across_reopen
        ] )
  ; ( "553-durability"
    , List.map
        (fun (n, f) -> Alcotest.test_case n `Quick f)
        [ "survives reopen", rename_column_remap_survives_reopen
        ; "rolls back", rename_column_rolls_back_with_its_transaction
        ; "commits", rename_column_commits_with_its_transaction
        ] )
  ; ( "553-roundtrip"
    , List.map
        (fun (n, f) -> Alcotest.test_case n `Quick f)
        [ "composite pk", roundtrip_after_rename_column_composite_pk
        ; "user indexes", roundtrip_after_rename_column_user_index
        ; "rename table", roundtrip_after_rename_table
        ; "key preserved", roundtrip_after_rename_preserves_the_key
        ] )
  ]
;;

let () = Alcotest.run "rename_column_553" suite
