(** #609: [ALTER TABLE ... RENAME COLUMN] / [... RENAME TO] must not silently
    break a view or a trigger that names what is being renamed.

    #553 taught the rename to remap every piece of stored SQL the CATALOG owns —
    a CHECK expression, a GENERATED expression, an expression index's column
    SQL, a partial index's WHERE, both sides of a FOREIGN KEY. It did not touch
    the two trees that hold whole [CREATE ...] statements as raw text:
    [_sys_views] and [_sys_triggers] (nor [_sys_reactive_views]). So

    {v
      CREATE VIEW vv AS SELECT a FROM v0 WHERE a > 0;
      ALTER TABLE v0 RENAME COLUMN a TO z;     -- succeeded, silently
      SELECT * FROM vv;                        -- unknown column: v0.a
    v}

    left the view permanently unreadable, and its dumped DDL restored into the
    same broken state.

    {1 The decision: REFUSE, not remap}

    Views and triggers are persisted as raw SQL TEXT. The catalog sits BELOW the
    parser in the dependency graph, so there is no AST here to walk and
    re-render, and a lexical rewrite of the text cannot scope a name the way
    SQLite's rewriter does — a view body legitimately names other tables'
    columns, and renaming one of those turns a working view into a quietly wrong
    one. Guessing is the failure mode this issue is about, so a rename that
    would touch a stored definition is refused, naming the dependent objects.

    Remapping remains the eventual fix; it needs a parser-side rewriter, not
    more string surgery.

    {1 What the detector matches}

    A definition blocks a COLUMN rename only when it names BOTH the table and
    the column — otherwise an unrelated view over a different table with a
    same-named column would block it. It cannot under-refuse: to reach a column
    of [t] a statement must name [t] somewhere, and a definition that reaches it
    only through another view is blocked transitively by that other view.

    The match is over identifier TOKENS, so a string literal spelling the column
    name does not count; and it is position-blind, so a qualified [v0.a] counts
    where the (column-position-filtered) #553 rewriter would have skipped it. *)

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

let exec_err db sql =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "expected %S to fail, but it succeeded" sql
  | Error e -> Format.asprintf "%a" Db.pp_error e
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

let check_mentions ~what msg needle =
  if not (contains ~needle msg)
  then Alcotest.failf "%s: error %S does not name %S" what msg needle
;;

(* ------------------------------------------------------------------ *)
(* RENAME COLUMN — refused when a view or trigger depends on it        *)
(* ------------------------------------------------------------------ *)

(* The issue's own repro. Before the fix the ALTER succeeded and the view was
   dead; now the ALTER fails and the view still answers. *)
let view_blocks_rename_column () =
  with_db (fun db ->
    exec db "CREATE TABLE v0 (a INTEGER, b INTEGER)";
    exec db "CREATE VIEW vv AS SELECT a FROM v0 WHERE a > 0";
    exec db "INSERT INTO v0 VALUES (1, 1)";
    Alcotest.(check (list string))
      "view works before"
      [ "i:1" ]
      (rows db "SELECT * FROM vv");
    let msg = exec_err db "ALTER TABLE v0 RENAME COLUMN a TO z" in
    check_mentions ~what:"view refusal" msg "view vv";
    (* Nothing was half-applied: the old name still resolves, the new one does
       not, and the view is still readable. *)
    Alcotest.(check (list string))
      "old column name intact"
      [ "i:1" ]
      (rows db "SELECT a FROM v0");
    Alcotest.(check (list string))
      "view still works"
      [ "i:1" ]
      (rows db "SELECT * FROM vv"))
;;

(* A QUALIFIED reference. #553's rewriter deliberately skips a word followed by
   '.', because there it is a table qualifier; the detector must not inherit
   that filter or every [v0.a] in a view body would slip past. *)
let view_qualified_reference_blocks_rename_column () =
  with_db (fun db ->
    exec db "CREATE TABLE v0 (a INTEGER, b INTEGER)";
    exec db "CREATE VIEW vq AS SELECT v0.a FROM v0";
    let msg = exec_err db "ALTER TABLE v0 RENAME COLUMN a TO z" in
    check_mentions ~what:"qualified view refusal" msg "view vq")
;;

let trigger_when_clause_blocks_rename_column () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
    exec db "CREATE TABLE log (x INTEGER)";
    exec
      db
      "CREATE TRIGGER tw AFTER INSERT ON t WHEN NEW.a > 5 BEGIN INSERT INTO log VALUES \
       (NEW.b); END";
    let msg = exec_err db "ALTER TABLE t RENAME COLUMN a TO z" in
    check_mentions ~what:"WHEN-clause refusal" msg "trigger tw";
    (* The trigger is still live. *)
    exec db "INSERT INTO t VALUES (9, 42)";
    Alcotest.(check (list string))
      "trigger still fires"
      [ "i:42" ]
      (rows db "SELECT x FROM log"))
;;

(* The column appears only in the trigger's BODY, never in its WHEN clause. *)
let trigger_body_blocks_rename_column () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
    exec db "CREATE TABLE log (x INTEGER)";
    exec
      db
      "CREATE TRIGGER tb AFTER INSERT ON t BEGIN INSERT INTO log VALUES (NEW.a); END";
    let msg = exec_err db "ALTER TABLE t RENAME COLUMN a TO z" in
    check_mentions ~what:"body refusal" msg "trigger tb")
;;

(* A trigger declared ON ANOTHER table whose body writes ours. The [ON] clause
   names [s], but the body names both [t] and [a], which is what the detector
   keys on. *)
let foreign_trigger_body_blocks_rename_column () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
    exec db "CREATE TABLE s (n INTEGER)";
    exec db "CREATE TRIGGER ts AFTER INSERT ON s BEGIN UPDATE t SET a = NEW.n; END";
    let msg = exec_err db "ALTER TABLE t RENAME COLUMN a TO z" in
    check_mentions ~what:"cross-table trigger refusal" msg "trigger ts")
;;

(* ------------------------------------------------------------------ *)
(* RENAME COLUMN — still succeeds for everything #553 already remaps   *)
(* ------------------------------------------------------------------ *)

(* An INDEX on the renamed column is catalog state, not stored statement text:
   #553 remaps it and the rename must still go through. *)
let index_does_not_block_rename_column () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
    exec db "CREATE INDEX ix ON t (a)";
    exec db "CREATE UNIQUE INDEX ux ON t (a, b)";
    exec db "INSERT INTO t VALUES (1, 2)";
    exec db "ALTER TABLE t RENAME COLUMN a TO z";
    Alcotest.(check (list string))
      "indexed lookup under the new name"
      [ "i:1,i:2" ]
      (rows db "SELECT z, b FROM t WHERE z = 1"))
;;

(* A CHECK constraint likewise: remapped by #553, and still enforced after. *)
let check_constraint_does_not_block_rename_column () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER CHECK (a > 0), b INTEGER)";
    exec db "ALTER TABLE t RENAME COLUMN a TO z";
    exec db "INSERT INTO t VALUES (1, 2)";
    (match run (Db.execute db "INSERT INTO t VALUES (-1, 3)") with
     | Error _ -> ()
     | Ok () -> Alcotest.fail "CHECK stopped firing after RENAME COLUMN");
    Alcotest.(check (list string))
      "the good row is there"
      [ "i:1" ]
      (rows db "SELECT z FROM t"))
;;

(* The control: a column nothing references renames as it always did, even with
   views and triggers present elsewhere in the database. *)
let unreferenced_column_still_renames () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
    exec db "CREATE TABLE log (x INTEGER)";
    exec db "CREATE VIEW vv AS SELECT a FROM t";
    exec
      db
      "CREATE TRIGGER tr AFTER INSERT ON t BEGIN INSERT INTO log VALUES (NEW.a); END";
    exec db "INSERT INTO t VALUES (1, 2)";
    (* [b] is named by neither the view nor the trigger. *)
    exec db "ALTER TABLE t RENAME COLUMN b TO bb";
    Alcotest.(check (list string)) "renamed" [ "i:1,i:2" ] (rows db "SELECT a, bb FROM t");
    Alcotest.(check (list string)) "view untouched" [ "i:1" ] (rows db "SELECT * FROM vv"))
;;

(* A view over a DIFFERENT table that happens to use the same column name must
   not block: the detector requires the table name AND the column name. *)
let same_named_column_on_another_table_does_not_block () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE TABLE other (a INTEGER)";
    exec db "CREATE VIEW ov AS SELECT a FROM other WHERE a > 0";
    exec db "INSERT INTO t VALUES (1)";
    exec db "ALTER TABLE t RENAME COLUMN a TO z";
    Alcotest.(check (list string)) "renamed" [ "i:1" ] (rows db "SELECT z FROM t"))
;;

(* A STRING LITERAL spelling the column name is not an identifier reference, so
   it must not block — this is what makes the scan a token scan rather than a
   substring search. *)
let string_literal_does_not_block_rename_column () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b TEXT)";
    exec db "CREATE VIEW lv AS SELECT b FROM t WHERE b = 'a'";
    exec db "INSERT INTO t VALUES (1, 'a')";
    exec db "ALTER TABLE t RENAME COLUMN a TO z";
    Alcotest.(check (list string)) "renamed" [ "i:1" ] (rows db "SELECT z FROM t");
    Alcotest.(check (list string)) "view untouched" [ "t:a" ] (rows db "SELECT * FROM lv"))
;;

(* ------------------------------------------------------------------ *)
(* RENAME TO — the same hole, the same site                            *)
(* ------------------------------------------------------------------ *)

let view_blocks_rename_table () =
  with_db (fun db ->
    exec db "CREATE TABLE v0 (a INTEGER)";
    exec db "CREATE VIEW vv AS SELECT a FROM v0";
    exec db "INSERT INTO v0 VALUES (1)";
    let msg = exec_err db "ALTER TABLE v0 RENAME TO v1" in
    check_mentions ~what:"table refusal" msg "view vv";
    Alcotest.(check (list string))
      "old table name intact"
      [ "i:1" ]
      (rows db "SELECT a FROM v0");
    Alcotest.(check (list string))
      "view still works"
      [ "i:1" ]
      (rows db "SELECT * FROM vv"))
;;

let trigger_blocks_rename_table () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE TABLE log (x INTEGER)";
    exec
      db
      "CREATE TRIGGER tr AFTER INSERT ON t BEGIN INSERT INTO log VALUES (NEW.a); END";
    let msg = exec_err db "ALTER TABLE t RENAME TO u" in
    check_mentions ~what:"table refusal" msg "trigger tr")
;;

let unreferenced_table_still_renames () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE TABLE other (a INTEGER)";
    exec db "CREATE VIEW ov AS SELECT a FROM other";
    exec db "INSERT INTO t VALUES (1)";
    exec db "ALTER TABLE t RENAME TO u";
    Alcotest.(check (list string)) "renamed" [ "i:1" ] (rows db "SELECT a FROM u"))
;;

(* ------------------------------------------------------------------ *)
(* Durability: the refusal is a property of what is ON DISK             *)
(* ------------------------------------------------------------------ *)

(* The issue asks for a reopen rather than an in-session check, because the
   stored text is only re-read on a later open. A refused rename must leave the
   file exactly as it was, and must still be refused after reopening — the
   detector reads the persisted definitions, not an in-memory cache. *)
let refusal_survives_reopen () =
  let path = Filename.temp_file "granary_609_" ".db" in
  Sys.remove path;
  let open_db () =
    match run (Granary_unix.open_file ~path ()) with
    | Ok db -> db
    | Error e -> Alcotest.failf "open_file: %a" Db.pp_error e
  in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () ->
       let db = open_db () in
       exec db "CREATE TABLE v0 (a INTEGER, b INTEGER)";
       exec db "CREATE VIEW vv AS SELECT a FROM v0 WHERE a > 0";
       exec db "INSERT INTO v0 VALUES (1, 1)";
       let msg = exec_err db "ALTER TABLE v0 RENAME COLUMN a TO z" in
       check_mentions ~what:"refusal before reopen" msg "view vv";
       (try run (Db.close db) with
        | _ -> ());
       let db = open_db () in
       Fun.protect
         ~finally:(fun () ->
           try run (Db.close db) with
           | _ -> ())
         (fun () ->
            Alcotest.(check (list string))
              "view readable after reopen"
              [ "i:1" ]
              (rows db "SELECT * FROM vv");
            let msg = exec_err db "ALTER TABLE v0 RENAME COLUMN a TO z" in
            check_mentions ~what:"refusal after reopen" msg "view vv";
            let msg = exec_err db "ALTER TABLE v0 RENAME TO v1" in
            check_mentions ~what:"table refusal after reopen" msg "view vv"))
;;

(* A view dropped after the fact stops blocking — the refusal tracks the live
   set of definitions, so the documented way out (drop, rename, recreate) works. *)
let dropping_the_view_unblocks_the_rename () =
  with_db (fun db ->
    exec db "CREATE TABLE v0 (a INTEGER)";
    exec db "CREATE VIEW vv AS SELECT a FROM v0";
    exec db "INSERT INTO v0 VALUES (1)";
    let _ = exec_err db "ALTER TABLE v0 RENAME COLUMN a TO z" in
    exec db "DROP VIEW vv";
    exec db "ALTER TABLE v0 RENAME COLUMN a TO z";
    exec db "CREATE VIEW vv AS SELECT z FROM v0";
    Alcotest.(check (list string))
      "recreated view reads the new name"
      [ "i:1" ]
      (rows db "SELECT * FROM vv"))
;;

let suite =
  [ ( "rename_deps_609"
    , [ Alcotest.test_case "view blocks RENAME COLUMN" `Quick view_blocks_rename_column
      ; Alcotest.test_case
          "qualified view reference blocks RENAME COLUMN"
          `Quick
          view_qualified_reference_blocks_rename_column
      ; Alcotest.test_case
          "trigger WHEN clause blocks RENAME COLUMN"
          `Quick
          trigger_when_clause_blocks_rename_column
      ; Alcotest.test_case
          "trigger body blocks RENAME COLUMN"
          `Quick
          trigger_body_blocks_rename_column
      ; Alcotest.test_case
          "trigger on another table blocks RENAME COLUMN"
          `Quick
          foreign_trigger_body_blocks_rename_column
      ; Alcotest.test_case
          "an index does not block RENAME COLUMN"
          `Quick
          index_does_not_block_rename_column
      ; Alcotest.test_case
          "a CHECK constraint does not block RENAME COLUMN"
          `Quick
          check_constraint_does_not_block_rename_column
      ; Alcotest.test_case
          "an unreferenced column still renames"
          `Quick
          unreferenced_column_still_renames
      ; Alcotest.test_case
          "a same-named column on another table does not block"
          `Quick
          same_named_column_on_another_table_does_not_block
      ; Alcotest.test_case
          "a string literal does not block RENAME COLUMN"
          `Quick
          string_literal_does_not_block_rename_column
      ; Alcotest.test_case "view blocks RENAME TO" `Quick view_blocks_rename_table
      ; Alcotest.test_case "trigger blocks RENAME TO" `Quick trigger_blocks_rename_table
      ; Alcotest.test_case
          "an unreferenced table still renames"
          `Quick
          unreferenced_table_still_renames
      ; Alcotest.test_case "the refusal survives a reopen" `Quick refusal_survives_reopen
      ; Alcotest.test_case
          "dropping the view unblocks the rename"
          `Quick
          dropping_the_view_unblocks_the_rename
      ] )
  ]
;;

let () = Alcotest.run "rename_deps_609" suite
