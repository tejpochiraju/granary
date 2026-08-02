(** #567 / #563: NOT NULL is enforced where the row values are known, and a
    legacy file that already violates one can be found and repaired.

    Before #567, [Sema] was the only place NOT NULL was enforced on the write
    path, and both of its enforcement sites tested [rhs_expr = E_lit L_null].
    Every other spelling of NULL — a bound parameter, [v = NULL + 1], a
    subquery, an FK [ON DELETE SET NULL] — reached the encoder untouched and
    was stored. The parameter spelling is the one that matters in practice:
    an application using prepared statements never writes a literal NULL.

    The resulting row is one the table's own rendered DDL will not restore, so
    [Db.dump] refuses the whole script over it (#548) — which is why the fix
    lives where the row values are finally known ([Exec.enforce_not_null]),
    rather than being spread over the binders. It runs at the four places a
    row assembled from user input is committed to storage:
    [execute_insert_write] (INSERT), [write_row_rekeyed] (UPDATE, UPSERT DO
    UPDATE, ON UPDATE CASCADE), and the two columnstore [Op_insert] /
    [Op_insert_select] arms. "Before every [Row.encode]" is NOT the rule and
    must not be restated as one: the columnstore arms hand their row array
    straight to [Col_store.insert_rows] and never encode at all, which is
    exactly how the first revision of this fix sailed past them. The static
    binder checks stay as the earlier, better-located error for the literal;
    the tests below pin which of the two fires.

    Since #530/#533 every PRIMARY KEY column carries [not_null = true], so a
    primary key is reachable the same way and is covered here too.

    The #563 half is the other direction: a file written before #530 can
    already hold such a row, and closing the write path cannot retract it.
    [PRAGMA not_null_check] reports every (table, column) with a count so an
    operator sees the scope, and [PRAGMA not_null_repair] deletes the rows —
    kept a separate statement precisely because it is destructive. The fixture
    is the #548 one: the rows go in through SQL while the column is genuinely
    nullable, and the implicit PK index is added afterwards through the
    catalog API, which is what [Catalog.open_] re-derives the NOT NULL from. *)

open Lwt.Syntax
module Db = Granary.Db
module Cat = Granary_catalog.Catalog
module Row = Granary_encoding.Row
module Index_key = Granary_encoding.Index_key

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

let query db sql =
  run
    (let* r = Db.query db sql in
     match r with
     | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
     | Ok stream -> Lwt_stream.to_list stream)
;;

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let show_value = function
  | Row.V_text s -> s
  | Row.V_null -> "<null>"
  | Row.V_int n -> Int64.to_string n
  | Row.V_real f -> string_of_float f
  | Row.V_blob _ -> "<blob>"
;;

let texts db sql =
  List.map
    (fun (r : Row.t) -> Array.to_list r |> List.map show_value |> String.concat "|")
    (query db sql)
;;

(* Rejected specifically as a NOT NULL violation on [t.c]: a rejection for any
   other reason would satisfy a bare "is an error" check while proving nothing.
   The encode-time check reports SQLite's wording, so this also pins that it is
   the runtime half — not a binder — that fired. *)
let rejects_at_encode_time db ~table ~col sql =
  let want = Printf.sprintf "NOT NULL constraint failed: %s.%s" table col in
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "expected %S on %S, got success" want sql
  | Error e ->
    let msg = Format.asprintf "%a" Db.pp_error e in
    Alcotest.(check bool)
      (Printf.sprintf "%S rejected with %S (got %S)" sql want msg)
      true
      (contains ~needle:want msg)
;;

(* Same, for a prepared statement run with explicit parameter values. *)
let run_stmt_rejects db ~table ~col sql params =
  let want = Printf.sprintf "NOT NULL constraint failed: %s.%s" table col in
  run
    (let* st =
       let* r = Db.prepare db sql in
       match r with
       | Ok st -> Lwt.return st
       | Error e -> Alcotest.failf "prepare %S: %a" sql Db.pp_error e
     in
     let* r = Db.run st ~params in
     match r with
     | Ok n -> Alcotest.failf "expected %S on %S, got Ok %d" want sql n
     | Error e ->
       let msg = Format.asprintf "%a" Db.pp_error e in
       Alcotest.(check bool)
         (Printf.sprintf "%S rejected with %S (got %S)" sql want msg)
         true
         (contains ~needle:want msg);
       Lwt.return_unit)
;;

let seed db =
  exec db "CREATE TABLE u (k INTEGER PRIMARY KEY, v INTEGER NOT NULL)";
  exec db "INSERT INTO u VALUES (1, 5)"
;;

(* ------------------------------------------------------------------ *)
(* #567: the two verified repros from the issue                         *)
(* ------------------------------------------------------------------ *)

(* [UPDATE u SET v = NULL + 1] — a NULL-valued expression that is not a literal
   NULL, so no binder sees it. *)
let update_null_expression_rejected () =
  with_db (fun db ->
    seed db;
    rejects_at_encode_time db ~table:"u" ~col:"v" "UPDATE u SET v = NULL + 1";
    Alcotest.(check (list string)) "row untouched" [ "1|5" ] (texts db "SELECT * FROM u"))
;;

(* The spelling that matters in practice: NULL bound to a parameter. *)
let upsert_parameter_null_rejected () =
  with_db (fun db ->
    seed db;
    run_stmt_rejects
      db
      ~table:"u"
      ~col:"v"
      "INSERT INTO u VALUES (1, 9) ON CONFLICT(k) DO UPDATE SET v = ?"
      [ Db.V_null ];
    Alcotest.(check (list string)) "row untouched" [ "1|5" ] (texts db "SELECT * FROM u"))
;;

let update_parameter_null_rejected () =
  with_db (fun db ->
    seed db;
    run_stmt_rejects db ~table:"u" ~col:"v" "UPDATE u SET v = ?" [ Db.V_null ];
    Alcotest.(check (list string)) "row untouched" [ "1|5" ] (texts db "SELECT * FROM u"))
;;

let insert_parameter_null_rejected () =
  with_db (fun db ->
    seed db;
    run_stmt_rejects
      db
      ~table:"u"
      ~col:"v"
      "INSERT INTO u VALUES (?, ?)"
      [ Db.V_int 2L; Db.V_null ];
    Alcotest.(check (list string))
      "nothing inserted"
      [ "1|5" ]
      (texts db "SELECT * FROM u"))
;;

(* Since #530 every PRIMARY KEY column is NOT NULL, so the key is reachable the
   same way.  A non-alias (TEXT) key, because an INTEGER PRIMARY KEY alias is
   refused earlier for a different reason — it must be an integer. *)
let primary_key_parameter_null_rejected () =
  with_db (fun db ->
    exec db "CREATE TABLE p (k TEXT PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO p VALUES ('a', 1)";
    run_stmt_rejects db ~table:"p" ~col:"k" "UPDATE p SET k = ?" [ Db.V_null ];
    run_stmt_rejects
      db
      ~table:"p"
      ~col:"k"
      "INSERT INTO p VALUES (?, ?)"
      [ Db.V_null; Db.V_int 2L ];
    rejects_at_encode_time db ~table:"p" ~col:"k" "UPDATE p SET k = NULL || 'x'";
    Alcotest.(check (list string)) "row untouched" [ "a|1" ] (texts db "SELECT * FROM p"))
;;

(* Composite key: every member carries the flag, so both are covered. *)
let composite_key_expression_null_rejected () =
  with_db (fun db ->
    exec db "CREATE TABLE c (a TEXT, b TEXT, v INTEGER, PRIMARY KEY (a, b))";
    exec db "INSERT INTO c VALUES ('x', 'y', 1)";
    rejects_at_encode_time db ~table:"c" ~col:"b" "UPDATE c SET b = NULL || 'z'";
    Alcotest.(check (list string))
      "row untouched"
      [ "x|y|1" ]
      (texts db "SELECT * FROM c"))
;;

(* INSERT ... SELECT never goes near the VALUES binder at all. *)
let insert_select_null_rejected () =
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO src VALUES (7, NULL)";
    rejects_at_encode_time db ~table:"u" ~col:"v" "INSERT INTO u SELECT k, v FROM src";
    Alcotest.(check (list string))
      "nothing inserted"
      [ "1|5" ]
      (texts db "SELECT * FROM u"))
;;

(* An FK cascade is a write like any other, and [ON DELETE SET NULL] onto a NOT
   NULL child column must not succeed.  This one was already covered — the FK
   machinery refuses it by name, earlier and with a better message than the
   encode-time check would give — so it is pinned here as the boundary of what
   #567 had to add rather than as something #567 changed. *)
let cascade_set_null_onto_not_null_rejected () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE parent (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE child (id INTEGER PRIMARY KEY, pid INTEGER NOT NULL REFERENCES \
       parent(id) ON DELETE SET NULL)";
    exec db "INSERT INTO parent VALUES (1)";
    exec db "INSERT INTO child VALUES (10, 1)";
    (match run (Db.execute db "DELETE FROM parent WHERE id = 1") with
     | Ok () -> Alcotest.fail "SET NULL onto a NOT NULL column was accepted"
     | Error e ->
       let msg = Format.asprintf "%a" Db.pp_error e in
       Alcotest.(check bool)
         (Printf.sprintf "refused by the FK path, naming the column (%S)" msg)
         true
         (contains ~needle:"ON DELETE SET NULL on NOT NULL column 'child.pid'" msg));
    Alcotest.(check (list string))
      "child untouched"
      [ "10|1" ]
      (texts db "SELECT * FROM child");
    Alcotest.(check (list string))
      "parent untouched"
      [ "1" ]
      (texts db "SELECT * FROM parent"))
;;

(* ------------------------------------------------------------------ *)
(* #567: what must NOT change                                           *)
(* ------------------------------------------------------------------ *)

(* A nullable column still takes NULL by every spelling — the tightening is
   about the declaration, not about NULL. *)
let nullable_columns_unaffected () =
  with_db (fun db ->
    exec db "CREATE TABLE n (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO n VALUES (1, 1)";
    exec db "UPDATE n SET v = NULL + 1";
    Alcotest.(check (list string))
      "expression NULL lands"
      [ "1|<null>" ]
      (texts db "SELECT * FROM n"));
  with_db (fun db ->
    exec db "CREATE TABLE n (k INTEGER PRIMARY KEY, v INTEGER)";
    run
      (let* st =
         let* r = Db.prepare db "INSERT INTO n VALUES (?, ?)" in
         match r with
         | Ok st -> Lwt.return st
         | Error e -> Alcotest.failf "prepare: %a" Db.pp_error e
       in
       let* r = Db.run st ~params:[ Db.V_int 2L; Db.V_null ] in
       match r with
       | Ok _ -> Lwt.return_unit
       | Error e ->
         Alcotest.failf "parameter NULL into a nullable column: %a" Db.pp_error e);
    Alcotest.(check (list string))
      "parameter NULL lands"
      [ "2|<null>" ]
      (texts db "SELECT * FROM n"))
;;

(* An omitted INTEGER PRIMARY KEY is auto-assigned by [insert_rowid], which runs
   BEFORE the encode-time check — so the alias column must not trip it. *)
let rowid_alias_auto_assignment_still_works () =
  with_db (fun db ->
    exec db "CREATE TABLE r (id INTEGER PRIMARY KEY, v TEXT NOT NULL)";
    exec db "INSERT INTO r (v) VALUES ('a')";
    exec db "INSERT INTO r VALUES (NULL, 'b')";
    Alcotest.(check (list string))
      "both rowids allocated"
      [ "1|a"; "2|b" ]
      (texts db "SELECT * FROM r"))
;;

(* A VIRTUAL generated column is stored as NULL and recomputed on read
   ([decode_with_virtual]), so its stored cell says nothing about the declared
   value.  [enforce_not_null] therefore exempts VIRTUAL columns — without that
   exemption every write to such a table would fail on a cell that is NULL by
   design.  A STORED one is materialised before the check runs, so it is not
   exempt and its computed value is what gets checked. *)
let generated_columns_still_writable () =
  with_db (fun db ->
    exec
      db
      "CREATE TABLE g (a INTEGER NOT NULL, v INTEGER GENERATED ALWAYS AS (a * 2) \
       VIRTUAL, s INTEGER GENERATED ALWAYS AS (a + 1) STORED)";
    exec db "INSERT INTO g (a) VALUES (3)";
    exec db "UPDATE g SET a = 4";
    Alcotest.(check (list string))
      "both generated columns readable after insert and update"
      [ "4|8|5" ]
      (texts db "SELECT a, v, s FROM g"))
;;

(* Pre-existing and unrelated to #567, pinned because the tests above are the
   natural place to look for it: a NOT NULL generated column is unreachable
   through INSERT at all.  [Sema.bind_insert_row] fills an omitted column with
   a literal NULL and applies its own NOT NULL check to that placeholder, so it
   rejects the row before the generated value is ever computed — and the column
   cannot be supplied explicitly either, since writing a generated column is
   refused.  #567 does not change this; the encode-time check never sees the
   statement. *)
let not_null_generated_column_is_unreachable () =
  with_db (fun db ->
    exec
      db
      "CREATE TABLE gn (a INTEGER, b INTEGER NOT NULL GENERATED ALWAYS AS (a + 1) STORED)";
    match run (Db.execute db "INSERT INTO gn (a) VALUES (3)") with
    | Ok () -> Alcotest.fail "a NOT NULL generated column became reachable"
    | Error e ->
      let msg = Format.asprintf "%a" Db.pp_error e in
      Alcotest.(check bool)
        (Printf.sprintf "refused by the binder's placeholder check (%S)" msg)
        true
        (contains ~needle:"NOT NULL violation: b" msg))
;;

(* The literal spellings must still fail in the BINDER — earlier, and with the
   better-located message — rather than being demoted to the runtime check. *)
let literal_null_still_a_bind_error () =
  with_db (fun db ->
    seed db;
    match run (Db.execute db "UPDATE u SET v = NULL") with
    | Ok () -> Alcotest.fail "literal NULL accepted"
    | Error e ->
      let msg = Format.asprintf "%a" Db.pp_error e in
      Alcotest.(check bool)
        (Printf.sprintf "binder message, not the runtime one (%S)" msg)
        true
        (contains ~needle:"NOT NULL violation: v" msg))
;;

(* The whole point (#548): after the fix a database built through SQL can
   always be dumped, because no write could have contradicted its schema. *)
let dump_survives_every_spelling () =
  with_db (fun db ->
    seed db;
    List.iter
      (fun sql ->
         match run (Db.execute db sql) with
         | Ok () | Error _ -> ())
      [ "UPDATE u SET v = NULL + 1"
      ; "INSERT INTO u VALUES (2, NULL)"
      ; "INSERT INTO u VALUES (1, 9) ON CONFLICT(k) DO UPDATE SET v = NULL * 2"
      ];
    match run (Db.dump_to_string db ()) with
    | Ok s ->
      Alcotest.(check bool)
        "dump emits the surviving row"
        true
        (contains ~needle:"INSERT INTO u VALUES(1,5)" s)
    | Error e ->
      Alcotest.failf "dump refused a database it built itself: %a" Db.pp_error e)
;;

(* ------------------------------------------------------------------ *)
(* #567: property — no accepted write leaves a NULL in a NOT NULL column *)
(* ------------------------------------------------------------------ *)

(* Expressions that evaluate to NULL without being the literal [NULL], plus a
   couple that do not, so the property is not vacuously satisfied by everything
   being rejected. *)
let rhs_gen =
  QCheck2.Gen.oneof_list
    [ "NULL + 1"
    ; "NULL * 2"
    ; "NULL || 'x'"
    ; "(SELECT NULL)"
    ; "abs(NULL)"
    ; "CASE WHEN 1 THEN NULL ELSE 7 END"
    ; "7"
    ; "1 + 1"
    ; "CASE WHEN 0 THEN NULL ELSE 7 END"
    ]
;;

let stmt_gen =
  QCheck2.Gen.(
    map2
      (fun rhs which ->
         match which with
         | 0 -> Printf.sprintf "UPDATE u SET v = %s" rhs
         | 1 -> Printf.sprintf "INSERT INTO u VALUES (2, %s)" rhs
         | _ ->
           Printf.sprintf
             "INSERT INTO u VALUES (1, 9) ON CONFLICT(k) DO UPDATE SET v = %s"
             rhs)
      rhs_gen
      (int_range 0 2))
;;

let no_null_survives_in_a_not_null_column =
  QCheck2.Test.make
    ~count:200
    ~name:"#567: an accepted write never leaves NULL in a NOT NULL column"
    stmt_gen
    (fun sql ->
       let db = run (Db.open_in_memory ()) in
       Fun.protect
         ~finally:(fun () ->
           try run (Db.close db) with
           | _ -> ())
         (fun () ->
            seed db;
            (* Accepted or refused, either is a legal outcome; what may never
               happen is a stored NULL in [v]. *)
            (match run (Db.execute db sql) with
             | Ok () | Error _ -> ());
            let nulls = query db "SELECT k FROM u WHERE v IS NULL" in
            List.length nulls = 0))
;;

(* ------------------------------------------------------------------ *)
(* #563: report and repair for a file that already violates            *)
(* ------------------------------------------------------------------ *)

let open_db path =
  match run (Granary_unix.open_file ~path ()) with
  | Ok db -> db
  | Error e -> Alcotest.failf "open %s: %a" path Db.pp_error e
;;

let close_db db =
  try run (Db.close db) with
  | _ -> ()
;;

(* The #548 fixture recipe, extended: register [cols] as the implicit PRIMARY
   KEY index of an already-populated table, directly in the catalog.  That is
   the pre-#530 shape — the columns were stored nullable, and the index is the
   only record that they are the key, which is what [Catalog.open_] re-derives
   the NOT NULL from on the next open (#533).  Going through [Db] alone cannot
   produce it, which is the whole reason #563 exists: the engine now refuses
   the NULL, but a file written before #530 already holds one.

   [Cat.create_index] only REGISTERS the index — population lives in
   [Exec.execute_create_index], which is not exported — so the #548 fixture
   left the index tree empty.  A real pre-#530 file has a POPULATED key index
   whose entries include the NULL keys, and the difference matters here in a
   way it did not for #548: with no entries at all there is nothing for the
   repair's delete path to maintain, so the property this PR advertises about
   repair would go untested.  The entries are therefore written by hand, in the
   same key format the engine uses ([Index_key.encode … ~rowid], empty value),
   and [PRAGMA integrity_check] below confirms the fixture starts consistent. *)
let add_populated_implicit_pk_index path ~table ~cols ~entries =
  run
    (let* store =
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
     | Error m -> Alcotest.failf "create_index: %s" m
     | Ok (idx : Cat.index_info) ->
       let* tx = Granary_store.Store.rw_begin store in
       let* () =
         Lwt_list.iter_s
           (fun (rowid, key_vals) ->
              Granary_store.Store.put
                tx
                idx.Cat.idx_tree_id
                (Index_key.encode key_vals ~rowid)
                Bytes.empty)
           entries
       in
       let* () = Granary_store.Store.commit tx in
       Granary_store.Store.close store)
;;

let with_legacy_null_key_db f =
  let path = Filename.temp_file "granary_563_" ".db" in
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
       exec db "INSERT INTO stock VALUES (2, NULL, 70)";
       close_db db;
       (* Rowids 1..3, in insertion order, on a plain rowid table. *)
       add_populated_implicit_pk_index
         path
         ~table:"stock"
         ~cols:[ "sw"; "si" ]
         ~entries:
           [ 1L, [ Index_key.IK_int 1L; Index_key.IK_int 2L ]
           ; 2L, [ Index_key.IK_int 1L; Index_key.IK_null ]
           ; 3L, [ Index_key.IK_int 2L; Index_key.IK_null ]
           ];
       let db = open_db path in
       Fun.protect ~finally:(fun () -> close_db db) (fun () -> f db))
;;

(* The fixture must actually be the legacy state it claims to be, or everything
   below is vacuous: the schema declares the column NOT NULL, rows violate it,
   AND the key index is populated so the repair has entries to maintain. *)
let the_fixture_is_a_real_legacy_file () =
  with_legacy_null_key_db (fun db ->
    Alcotest.(check (list string))
      "the key index is populated and consistent with the rows"
      [ "ok" ]
      (texts db "PRAGMA integrity_check");
    Alcotest.(check (list string))
      "and the index is usable — the point lookup finds its row"
      [ "1|2|50" ]
      (texts db "SELECT * FROM stock WHERE sw = 1 AND si = 2");
    let ddl =
      match run (Db.dump_to_string db ~schema_only:true ()) with
      | Ok s -> s
      | Error e -> Alcotest.failf "schema_only dump: %a" Db.pp_error e
    in
    Alcotest.(check bool)
      (Printf.sprintf "while the schema declares si NOT NULL (%s)" ddl)
      true
      (contains ~needle:"si INTEGER NOT NULL" ddl))
;;

(* A clean database reports nothing at all — the report is a list of problems,
   not a status line, so silence is the healthy answer. *)
let check_reports_nothing_when_clean () =
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE t2 (a TEXT PRIMARY KEY, b TEXT NOT NULL, c TEXT)";
    exec db "INSERT INTO t2 VALUES ('x', 'y', NULL)";
    Alcotest.(check (list string)) "no findings" [] (texts db "PRAGMA not_null_check"))
;;

let check_reports_nothing_on_an_empty_database () =
  with_db (fun db ->
    Alcotest.(check (list string)) "no findings" [] (texts db "PRAGMA not_null_check"))
;;

(* Report mode is the important half: table, column and how many rows, so the
   operator sees the whole scope before touching anything. *)
let check_reports_table_column_and_count () =
  with_legacy_null_key_db (fun db ->
    Alcotest.(check (list string))
      "one finding, two offending rows"
      [ "stock|si|2" ]
      (texts db "PRAGMA not_null_check");
    (* And it really is read-only: nothing was deleted. *)
    Alcotest.(check (list string))
      "rows still present after the report"
      [ "1|2|50"; "1|<null>|60"; "2|<null>|70" ]
      (texts db "SELECT * FROM stock"))
;;

(* The report and the dump refusal (#548) describe the same rows: running the
   repair the report names makes the dump succeed. *)
let repair_deletes_exactly_the_reported_rows () =
  with_legacy_null_key_db (fun db ->
    (match run (Db.dump_to_string db ()) with
     | Ok _ -> Alcotest.fail "the #548 refusal did not fire on the fixture"
     | Error e ->
       let msg = Format.asprintf "%a" Db.pp_error e in
       Alcotest.(check bool)
         (Printf.sprintf "dump refuses first (%S)" msg)
         true
         (contains ~needle:"si" msg));
    Alcotest.(check (list string))
      "repair reports what it removed"
      [ "stock|si|2" ]
      (texts db "PRAGMA not_null_repair");
    Alcotest.(check (list string))
      "only the clean row survives"
      [ "1|2|50" ]
      (texts db "SELECT * FROM stock");
    Alcotest.(check (list string))
      "and the report is now silent"
      []
      (texts db "PRAGMA not_null_check");
    match run (Db.dump_to_string db ()) with
    | Ok s ->
      Alcotest.(check bool)
        "dump succeeds once repaired"
        true
        (contains ~needle:"INSERT INTO stock VALUES(1,2,50)" s)
    | Error e -> Alcotest.failf "dump still refuses after repair: %a" Db.pp_error e)
;;

(* Repair on a clean database is a no-op that deletes nothing — the destructive
   statement must be safe to run when there is nothing to do. *)
let repair_is_a_no_op_when_clean () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list string)) "no findings" [] (texts db "PRAGMA not_null_repair");
    Alcotest.(check (list string)) "row untouched" [ "1|5" ] (texts db "SELECT * FROM u"))
;;

(* Repair must be indistinguishable from typing the [DELETE] that #548's
   diagnostic tells the operator to write — that is the whole ergonomic claim.
   It goes through the same delete path, so index entries go with the rows; the
   fixture's key index is populated (see [the_fixture_is_a_real_legacy_file]),
   so [integrity_check] can actually observe that maintenance rather than
   passing because there was nothing to maintain. *)
let repair_matches_the_hand_written_delete () =
  let after ~pragma =
    with_legacy_null_key_db (fun db ->
      if pragma
      then ignore (texts db "PRAGMA not_null_repair" : string list)
      else exec db "DELETE FROM stock WHERE si IS NULL";
      texts db "SELECT * FROM stock", texts db "PRAGMA integrity_check")
  in
  let by_pragma = after ~pragma:true in
  let by_hand = after ~pragma:false in
  Alcotest.(check (pair (list string) (list string)))
    "repair == the DELETE the operator would have written"
    by_hand
    by_pragma;
  Alcotest.(check (list string))
    "and the index entries went with the rows"
    [ "ok" ]
    (snd by_pragma)
;;

(* #262 read-your-own-writes: inside an explicit transaction the report must see
   that transaction's own deletes.  Reading a fresh RO snapshot instead made
   [BEGIN; repair; check] contradict a plain SELECT in the same transaction and
   tell the operator the repair had failed. *)
let check_sees_the_repair_inside_a_transaction () =
  with_legacy_null_key_db (fun db ->
    exec db "BEGIN";
    Alcotest.(check (list string))
      "repair runs in the transaction"
      [ "stock|si|2" ]
      (texts db "PRAGMA not_null_repair");
    Alcotest.(check (list string))
      "SELECT sees the deletes"
      [ "1|2|50" ]
      (texts db "SELECT * FROM stock");
    Alcotest.(check (list string))
      "and so does the report"
      []
      (texts db "PRAGMA not_null_check");
    exec db "ROLLBACK";
    Alcotest.(check (list string))
      "rollback restores the rows"
      [ "1|2|50"; "1|<null>|60"; "2|<null>|70" ]
      (texts db "SELECT * FROM stock");
    Alcotest.(check (list string))
      "and the report sees them again"
      [ "stock|si|2" ]
      (texts db "PRAGMA not_null_check"))
;;

(* ------------------------------------------------------------------ *)
(* #567 / #563: columnstore tables                                      *)
(* ------------------------------------------------------------------ *)

(* A columnstore INSERT hands its row array straight to [Col_store.insert_rows]
   and never reaches [Row.encode], so it needs its own [enforce_not_null] call.
   Without one the literal spelling was still caught by the binder while the
   bound-parameter spelling — the one #567 says matters in practice — wrote the
   NULL, and [PRAGMA not_null_check] then reported the file clean. *)
let columnar_parameter_null_rejected () =
  with_db (fun db ->
    exec db "CREATE TABLE cs (k INTEGER, v INTEGER NOT NULL) USING COLUMNSTORE";
    run_stmt_rejects
      db
      ~table:"cs"
      ~col:"v"
      "INSERT INTO cs VALUES (?, ?)"
      [ Db.V_int 2L; Db.V_null ];
    Alcotest.(check (list string)) "nothing stored" [] (texts db "SELECT * FROM cs");
    Alcotest.(check (list string))
      "and nothing to report"
      []
      (texts db "PRAGMA not_null_check"))
;;

let columnar_insert_select_null_rejected () =
  with_db (fun db ->
    exec db "CREATE TABLE cs (k INTEGER, v INTEGER NOT NULL) USING COLUMNSTORE";
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO src VALUES (4, NULL)";
    rejects_at_encode_time db ~table:"cs" ~col:"v" "INSERT INTO cs SELECT k, v FROM src";
    Alcotest.(check (list string)) "nothing stored" [] (texts db "SELECT * FROM cs"))
;;

(* The compounding half of the columnstore gap: a report that skips a storage
   engine lies about it.  Built the same way as the row-store legacy fixture —
   the rows go in while the column is genuinely nullable, then an implicit-PK
   index is registered so [Catalog.open_] re-derives NOT NULL on reopen (#533,
   which is storage-agnostic).  No index tree is populated here because a
   columnstore is not B-tree indexed; [integrity_check] skips Columnar tables
   for the same reason. *)
let check_sees_a_columnstore_violation () =
  let path = Filename.temp_file "granary_563_cs_" ".db" in
  Sys.remove path;
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () ->
       let db = open_db path in
       exec db "CREATE TABLE cs (k INTEGER, v INTEGER) USING COLUMNSTORE";
       exec db "INSERT INTO cs VALUES (1, 10)";
       exec db "INSERT INTO cs VALUES (2, NULL)";
       exec db "INSERT INTO cs VALUES (3, NULL)";
       close_db db;
       add_populated_implicit_pk_index path ~table:"cs" ~cols:[ "v" ] ~entries:[];
       let db = open_db path in
       Fun.protect
         ~finally:(fun () -> close_db db)
         (fun () ->
            Alcotest.(check (list string))
              "the reopened schema declares v NOT NULL"
              [ "0|k|INTEGER|0|<null>|0"; "1|v|INTEGER|1|<null>|1" ]
              (texts db "PRAGMA table_info(cs)");
            Alcotest.(check (list string))
              "and the report names the columnstore rows"
              [ "cs|v|2" ]
              (texts db "PRAGMA not_null_check");
            (* A columnstore is append-only, so repair cannot delete its rows.
               It says so through the result rather than by raising, and a
               count-0 row is the whole signal on its own: a table with nothing
               to fix emits no row at all (see "clean table works" below), so 0
               can only mean "found, not fixed".  The check then gives the size
               of what was left behind. *)
            Alcotest.(check (list string))
              "repair reports 0 deleted, not a repair it did not perform"
              [ "cs|v|0" ]
              (texts db "PRAGMA not_null_repair");
            Alcotest.(check (list string))
              "the rows are untouched"
              [ "1|10"; "2|<null>"; "3|<null>" ]
              (texts db "SELECT * FROM cs");
            Alcotest.(check (list string))
              "and the check still reports the true count"
              [ "cs|v|2" ]
              (texts db "PRAGMA not_null_check")))
;;

(* A columnstore that honours its schema must still write normally, and must be
   scanned by the report rather than skipped. *)
let columnar_clean_table_still_works () =
  with_db (fun db ->
    exec db "CREATE TABLE cs (k INTEGER, v INTEGER NOT NULL) USING COLUMNSTORE";
    exec db "INSERT INTO cs VALUES (1, 10)";
    exec db "INSERT INTO cs SELECT 2, 20";
    Alcotest.(check (list string))
      "rows land"
      [ "1|10"; "2|20" ]
      (texts db "SELECT * FROM cs");
    Alcotest.(check (list string))
      "report is silent"
      []
      (texts db "PRAGMA not_null_check");
    (* NO row, not a count-0 row.  This is the other half of the signal the
       unrepairable-columnstore case relies on: since a clean table is silent,
       a 0-count row can only ever mean "found but not fixed". *)
    Alcotest.(check (list string))
      "repair is a no-op, and emits nothing at all"
      []
      (texts db "PRAGMA not_null_repair"))
;;

let () =
  Alcotest.run
    "not_null_567"
    [ ( "567-repros"
      , [ Alcotest.test_case
            "NULL-valued expression"
            `Quick
            update_null_expression_rejected
        ; Alcotest.test_case "upsert parameter" `Quick upsert_parameter_null_rejected
        ; Alcotest.test_case "update parameter" `Quick update_parameter_null_rejected
        ; Alcotest.test_case "insert parameter" `Quick insert_parameter_null_rejected
        ; Alcotest.test_case "primary key" `Quick primary_key_parameter_null_rejected
        ; Alcotest.test_case "composite key" `Quick composite_key_expression_null_rejected
        ; Alcotest.test_case "INSERT ... SELECT" `Quick insert_select_null_rejected
        ; Alcotest.test_case
            "FK SET NULL cascade"
            `Quick
            cascade_set_null_onto_not_null_rejected
        ] )
    ; ( "567-unchanged"
      , [ Alcotest.test_case "nullable columns" `Quick nullable_columns_unaffected
        ; Alcotest.test_case "rowid alias" `Quick rowid_alias_auto_assignment_still_works
        ; Alcotest.test_case "generated columns" `Quick generated_columns_still_writable
        ; Alcotest.test_case
            "NOT NULL generated is unreachable"
            `Quick
            not_null_generated_column_is_unreachable
        ; Alcotest.test_case
            "literal stays a bind error"
            `Quick
            literal_null_still_a_bind_error
        ; Alcotest.test_case "dump survives" `Quick dump_survives_every_spelling
        ] )
    ; ( "567-property"
      , List.map QCheck_alcotest.to_alcotest [ no_null_survives_in_a_not_null_column ] )
    ; ( "563-report"
      , [ Alcotest.test_case "clean database" `Quick check_reports_nothing_when_clean
        ; Alcotest.test_case
            "empty database"
            `Quick
            check_reports_nothing_on_an_empty_database
        ; Alcotest.test_case
            "table, column, count"
            `Quick
            check_reports_table_column_and_count
        ; Alcotest.test_case
            "the fixture is a real legacy file"
            `Quick
            the_fixture_is_a_real_legacy_file
        ] )
    ; ( "columnstore"
      , [ Alcotest.test_case
            "parameter NULL rejected"
            `Quick
            columnar_parameter_null_rejected
        ; Alcotest.test_case
            "INSERT ... SELECT rejected"
            `Quick
            columnar_insert_select_null_rejected
        ; Alcotest.test_case "clean table works" `Quick columnar_clean_table_still_works
        ; Alcotest.test_case
            "report sees a columnstore violation"
            `Quick
            check_sees_a_columnstore_violation
        ] )
    ; ( "563-repair"
      , [ Alcotest.test_case
            "deletes the reported rows"
            `Quick
            repair_deletes_exactly_the_reported_rows
        ; Alcotest.test_case "no-op when clean" `Quick repair_is_a_no_op_when_clean
        ; Alcotest.test_case
            "same as the hand-written DELETE"
            `Quick
            repair_matches_the_hand_written_delete
        ; Alcotest.test_case
            "check sees the repair in-transaction"
            `Quick
            check_sees_the_repair_inside_a_transaction
        ] )
    ]
;;
