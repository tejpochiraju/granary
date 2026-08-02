(** #530: every PRIMARY KEY column implies NOT NULL, whichever way it is spelled.

    Before this, whether a PRIMARY KEY column rejected NULL depended on the
    spelling rather than the meaning. [column_of_def] derives
    [not_null = c.not_null || c.primary_key] while walking the column
    definitions; [mark_table_pk] applied table-level constraints only
    afterwards, so its [primary_key = true] never reached that [||]. And
    [mark_table_pk] matched a single-column [PRIMARY KEY (k)] only, so no column
    of a composite [PRIMARY KEY (k, j)] was ever marked at all. Net effect:

    {v
      CREATE TABLE a (k TEXT PRIMARY KEY, v INTEGER)            NULL -> ERR
      CREATE TABLE b (k TEXT, v INTEGER, PRIMARY KEY (k))       NULL -> OK
      CREATE TABLE c (k TEXT, j TEXT, PRIMARY KEY (k, j))       NULL -> OK
    v}

    [a] and [b] declare the same primary key and disagreed about NULL.

    {b This is an intentional divergence from SQLite.} A rowid table in SQLite
    enforces NOT NULL on no PRIMARY KEY column except [INTEGER PRIMARY KEY], so
    SQLite accepts all three NULLs above. Granary is inspired by SQLite, not a
    port of it, and the strict reading was chosen deliberately: a primary key is
    the row's identity, and an unknown identity is not one. The direction taken
    tightens [b] and [c] rather than relaxing [a] — so INSERTs that previously
    succeeded now fail.

    [INTEGER PRIMARY KEY] keeps its #243 (T1) exemption: NULL there requests an
    auto-assigned rowid and can never be stored NULL, so it is not a violation.
    That exemption lives at the enforcement site (the rowid-alias column is
    skipped), not in the column's [not_null] flag, which is why marking more
    columns NOT NULL cannot disturb it. *)

module Db = Granary.Db
module Cat = Granary_catalog.Catalog
module Row = Granary_encoding.Row
module Sema = Granary_sql.Sema
module Ast = Granary_sql.Ast

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

let contains_sub ~needle s =
  let n = String.length needle
  and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = needle || go (i + 1)) in
  go 0
;;

(* [sql] must be rejected, and specifically for a NOT NULL violation naming
   [col] — a rejection for any other reason would pass a bare "is an error"
   assertion while proving nothing about this issue. *)
let rejects_null db ~col sql =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "expected NOT NULL violation on %S, got success" col
  | Error e ->
    let msg = Format.asprintf "%a" Db.pp_error e in
    let want = "NOT NULL violation: " ^ col in
    Alcotest.(check bool)
      (Printf.sprintf "%S rejected with %S (got %S)" sql want msg)
      true
      (contains_sub ~needle:want msg)
;;

let count db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream -> List.length (run (Lwt_stream.to_list stream))
;;

(* ------------------------------------------------------------------ *)
(* The three spellings                                                  *)
(* ------------------------------------------------------------------ *)

(* Unchanged from before #530 — the spelling that was already strict. *)
let column_constraint_pk_rejects_null () =
  with_db (fun db ->
    exec db "CREATE TABLE a (k TEXT PRIMARY KEY, v INTEGER)";
    rejects_null db ~col:"k" "INSERT INTO a VALUES (NULL, 1)")
;;

(* The change: a single-column table constraint means the same key as above and
   must now reject the same NULL. *)
let table_constraint_pk_rejects_null () =
  with_db (fun db ->
    exec db "CREATE TABLE b (k TEXT, v INTEGER, PRIMARY KEY (k))";
    rejects_null db ~col:"k" "INSERT INTO b VALUES (NULL, 1)")
;;

(* Every column of a composite key, independently — [mark_table_pk] marked none
   of them, so both arms were permissive and the second is the one a "mark the
   first column" half-fix would still miss. *)
let composite_pk_rejects_null_in_first_column () =
  with_db (fun db ->
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    rejects_null db ~col:"k" "INSERT INTO c VALUES (NULL, 'x', 1)")
;;

let composite_pk_rejects_null_in_second_column () =
  with_db (fun db ->
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    rejects_null db ~col:"j" "INSERT INTO c VALUES ('x', NULL, 1)")
;;

(* Omitting a PK column is the same as writing NULL into it: the default is
   NULL, so the enforcement must see it. *)
let composite_pk_rejects_omitted_column () =
  with_db (fun db ->
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    rejects_null db ~col:"j" "INSERT INTO c (k, v) VALUES ('x', 1)")
;;

(* The three spellings must also agree on what they ACCEPT — the tightening is
   confined to NULL. *)
let all_spellings_accept_non_null () =
  with_db (fun db ->
    exec db "CREATE TABLE a (k TEXT PRIMARY KEY, v INTEGER)";
    exec db "CREATE TABLE b (k TEXT, v INTEGER, PRIMARY KEY (k))";
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    exec db "INSERT INTO a VALUES ('x', 1)";
    exec db "INSERT INTO b VALUES ('x', 1)";
    exec db "INSERT INTO c VALUES ('x', 'y', 1)";
    Alcotest.(check int) "a" 1 (count db "SELECT * FROM a");
    Alcotest.(check int) "b" 1 (count db "SELECT * FROM b");
    Alcotest.(check int) "c" 1 (count db "SELECT * FROM c"))
;;

(* A non-key column is untouched: it still takes NULL freely. *)
let non_key_column_still_accepts_null () =
  with_db (fun db ->
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    exec db "INSERT INTO c VALUES ('x', 'y', NULL)";
    Alcotest.(check int) "one row" 1 (count db "SELECT * FROM c"))
;;

(* An explicit NOT NULL alongside the key is redundant, not a conflict. *)
let explicit_not_null_alongside_pk_is_fine () =
  with_db (fun db ->
    exec db "CREATE TABLE d (k TEXT NOT NULL, j TEXT NOT NULL, PRIMARY KEY (k, j))";
    exec db "INSERT INTO d VALUES ('x', 'y')";
    Alcotest.(check int) "one row" 1 (count db "SELECT * FROM d");
    rejects_null db ~col:"k" "INSERT INTO d VALUES (NULL, 'y')")
;;

(* UPDATE shares the [not_null] flag, so it tightens with it. *)
let update_to_null_is_rejected () =
  with_db (fun db ->
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    exec db "INSERT INTO c VALUES ('x', 'y', 1)";
    rejects_null db ~col:"j" "UPDATE c SET j = NULL")
;;

(* ------------------------------------------------------------------ *)
(* What must NOT change                                                 *)
(* ------------------------------------------------------------------ *)

(* #243 (T1): NULL in a rowid alias requests the next rowid. The alias column
   carries [not_null = true] and always has; the exemption is at the
   enforcement site. Both spellings of the alias are checked, because the table
   form is the one whose [not_null] this change flips. *)
let rowid_alias_null_still_auto_assigns () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)";
    exec db "CREATE TABLE u (id INTEGER, v INTEGER, PRIMARY KEY (id))";
    exec db "INSERT INTO t VALUES (NULL, 1)";
    exec db "INSERT INTO u VALUES (NULL, 1)";
    Alcotest.(check int) "column-form alias" 1 (count db "SELECT id FROM t WHERE id >= 1");
    Alcotest.(check int) "table-form alias" 1 (count db "SELECT id FROM u WHERE id >= 1"))
;;

(* An omitted rowid alias is the same request. *)
let rowid_alias_omitted_still_auto_assigns () =
  with_db (fun db ->
    exec db "CREATE TABLE u (id INTEGER, v INTEGER, PRIMARY KEY (id))";
    exec db "INSERT INTO u (v) VALUES (1)";
    Alcotest.(check int) "auto-assigned" 1 (count db "SELECT id FROM u WHERE id >= 1"))
;;

(* INTEGER PRIMARY KEY DESC is NOT an alias (#312), so it gets no exemption and
   the strict rule applies to it.  Only the column form is exercised: the
   grammar's [pk_col_spec] accepts no ASC/DESC inside a table-level
   [PRIMARY KEY (...)], so there is no table spelling of this to compare
   against.  The column form already rejected this NULL before #530, so this is
   a regression guard rather than one of the RED cases. *)
let integer_pk_desc_rejects_null () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY DESC, v INTEGER)";
    rejects_null db ~col:"id" "INSERT INTO t VALUES (NULL, 1)")
;;

(* A WITHOUT ROWID table has no rowid to fall back on, so nothing is exempt: its
   INTEGER key column rejects the NULL that a rowid table's alias would have
   turned into an auto-assigned id.  Before #530 this was rejected too, but only
   at RUNTIME by the storage layer; the key is now NOT NULL, so sema rejects it
   statically and the message changes accordingly. *)
let without_rowid_single_pk_rejects_null () =
  with_db (fun db ->
    exec db "CREATE TABLE w (k INTEGER, v INTEGER, PRIMARY KEY (k)) WITHOUT ROWID";
    exec db "INSERT INTO w VALUES (1, 1)";
    Alcotest.(check int) "one row" 1 (count db "SELECT * FROM w");
    rejects_null db ~col:"k" "INSERT INTO w VALUES (NULL, 1)")
;;

(* A composite PK on a WITHOUT ROWID table is unsupported (phase 37), and the
   marking fix changes WHICH refusal it gets: with no column marked, the table
   looked like it had no PRIMARY KEY at all and was rejected as such.  The PR
   claimed the wording improved, so the wording is asserted — otherwise the
   improvement could silently regress back to "requires a PRIMARY KEY column". *)
let without_rowid_composite_pk_is_still_unsupported () =
  with_db (fun db ->
    match
      run
        (Db.execute
           db
           "CREATE TABLE w (k INTEGER, j TEXT, v INTEGER, PRIMARY KEY (k, j)) WITHOUT \
            ROWID")
    with
    | Ok () -> Alcotest.fail "expected composite WITHOUT ROWID to be unsupported"
    | Error e ->
      let msg = Format.asprintf "%a" Db.pp_error e in
      Alcotest.(check bool)
        (Printf.sprintf "refused as unsupported (got %S)" msg)
        true
        (contains_sub ~needle:"unsupported" msg);
      Alcotest.(check bool)
        (Printf.sprintf "refusal names the real reason (got %S)" msg)
        true
        (contains_sub ~needle:"exactly one PRIMARY KEY column" msg))
;;

(* ------------------------------------------------------------------ *)
(* The flag itself                                                      *)
(* ------------------------------------------------------------------ *)

(* PRAGMA table_info reports (cid, name, type, notnull, dflt, pk). Reading it
   pins the catalog-level fact — both flags on every PK column, whatever the
   spelling — rather than only its INSERT-time consequence. *)
let table_info_flags db table =
  match run (Db.query db (Printf.sprintf "PRAGMA table_info(%s)" table)) with
  | Error e -> Alcotest.failf "pragma on %s: %a" table Db.pp_error e
  | Ok stream ->
    List.map
      (fun row ->
         let s = function
           | Db.V_text t -> t
           | Db.V_int n -> Int64.to_string n
           | _ -> "?"
         in
         s row.(1), s row.(3), s row.(5))
      (run (Lwt_stream.to_list stream))
;;

let pragma_reports_pk_columns_not_null () =
  with_db (fun db ->
    exec db "CREATE TABLE a (k TEXT PRIMARY KEY, v INTEGER)";
    exec db "CREATE TABLE b (k TEXT, v INTEGER, PRIMARY KEY (k))";
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    let want_a = [ "k", "1", "1"; "v", "0", "0" ] in
    let want_c = [ "k", "1", "1"; "j", "1", "1"; "v", "0", "0" ] in
    let t = Alcotest.(list (triple string string string)) in
    Alcotest.check t "column constraint" want_a (table_info_flags db "a");
    Alcotest.check t "table constraint" want_a (table_info_flags db "b");
    Alcotest.check t "composite" want_c (table_info_flags db "c"))
;;

(* The rendered DDL (sqlite_master.sql, and hence [Db.dump]) must not turn a
   composite key into two single-column keys now that both its columns are
   marked.  #533: it is rendered as the table-level constraint it is, recovered
   from the implicit PK index — the only record of the key's column ORDER, which
   [Row.column] does not carry. *)
let ddl_of db table =
  match
    run
      (Db.query
         db
         (Printf.sprintf "SELECT sql FROM sqlite_master WHERE name = '%s'" table))
  with
  | Error e -> Alcotest.failf "sqlite_master: %a" Db.pp_error e
  | Ok stream ->
    (match run (Lwt_stream.to_list stream) with
     | [ [| Db.V_text sql |] ] -> sql
     | _ -> Alcotest.failf "no DDL row for %s" table)
;;

let composite_pk_ddl_renders_the_table_level_key () =
  with_db (fun db ->
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    Alcotest.(check string)
      "composite key rendered once, in key order"
      "CREATE TABLE c (k TEXT NOT NULL, j TEXT NOT NULL, v INTEGER, PRIMARY KEY (k, j))"
      (ddl_of db "c"))
;;

(* A single-column table-level PK still renders inline and still reopens as the
   rowid alias it was — the DDL path is only changed for the composite case. *)
let single_pk_ddl_still_inline () =
  with_db (fun db ->
    exec db "CREATE TABLE u (id INTEGER, v INTEGER, PRIMARY KEY (id))";
    let sql = ddl_of db "u" in
    Alcotest.(check bool)
      (Printf.sprintf "inline PRIMARY KEY in %S" sql)
      true
      (contains_sub ~needle:"PRIMARY KEY" sql))
;;

(* #533: this engine accepts more than one PRIMARY KEY declaration (SQLite
   rejects it).  The first cut of #530 suppressed the inline suffix whenever
   MORE THAN ONE column was marked, which is true here for two entirely separate
   single-column keys — so both lost their PRIMARY KEY from the DDL. *)
let two_primary_key_declarations_both_render () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k TEXT PRIMARY KEY, j TEXT PRIMARY KEY)";
    Alcotest.(check string)
      "both single-column keys survive"
      "CREATE TABLE t (k TEXT NOT NULL PRIMARY KEY, j TEXT NOT NULL PRIMARY KEY)"
      (ddl_of db "t"))
;;

(* And the mixed shape: one column key plus one composite table constraint.  The
   suppressed set is the composite's MEMBERS, so [k] keeps its inline suffix. *)
let column_pk_beside_composite_pk_renders_both () =
  with_db (fun db ->
    exec db "CREATE TABLE s (k TEXT PRIMARY KEY, a TEXT, b TEXT, PRIMARY KEY (a, b))";
    Alcotest.(check string)
      "inline key kept, composite rendered separately"
      "CREATE TABLE s (k TEXT NOT NULL PRIMARY KEY, a TEXT NOT NULL, b TEXT NOT NULL, \
       PRIMARY KEY (a, b))"
      (ddl_of db "s"))
;;

(* ------------------------------------------------------------------ *)
(* Dump / restore round-trip                                            *)
(* ------------------------------------------------------------------ *)

(* #533: the DDL renderer and [Db.dump]'s "is this index already implied?" used
   to answer independently, and drifted — a table whose inline PRIMARY KEY was
   suppressed still had its backing index suppressed as "implied", so the dump
   carried NEITHER and a restore silently accepted duplicates.  These replay a
   dump into a fresh database and assert the constraint is still there. *)
let dump_of db =
  match run (Db.dump_to_string db ()) with
  | Error e -> Alcotest.failf "dump: %a" Db.pp_error e
  | Ok s -> s
;;

let restore sql =
  let db = run (Db.open_in_memory ()) in
  List.iter
    (fun stmt ->
       let s = String.trim stmt in
       if s <> "" then exec db s)
    (String.split_on_char ';' sql);
  db
;;

(* [seed] builds the table and one row; [dup] must then be refused by the
   RESTORED database, exactly as it is by the original. *)
let round_trips_the_key ~seed ~dup () =
  let src = run (Db.open_in_memory ()) in
  exec src seed;
  exec src dup;
  (match run (Db.execute src dup) with
   | Error _ -> ()
   | Ok () -> Alcotest.fail "source database did not enforce the key — test is void");
  let script = dump_of src in
  run (Db.close src);
  let dst = restore script in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close dst) with
      | _ -> ())
    (fun () ->
       Alcotest.(check int) "row restored" 1 (count dst "SELECT * FROM t");
       match run (Db.execute dst dup) with
       | Error _ -> ()
       | Ok () ->
         Alcotest.failf "restored database accepted a duplicate key; dump was:\n%s" script)
;;

let dump_round_trips_single_table_level_pk =
  round_trips_the_key
    ~seed:"CREATE TABLE t (k TEXT, v INTEGER, PRIMARY KEY (k))"
    ~dup:"INSERT INTO t VALUES ('x', 1)"
;;

let dump_round_trips_two_pk_declarations =
  round_trips_the_key
    ~seed:"CREATE TABLE t (k TEXT PRIMARY KEY, j TEXT PRIMARY KEY)"
    ~dup:"INSERT INTO t VALUES ('x', 'y')"
;;

let dump_round_trips_composite_pk =
  round_trips_the_key
    ~seed:"CREATE TABLE t (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))"
    ~dup:"INSERT INTO t VALUES ('x', 'y', 1)"
;;

(* The composite key must come back as a PRIMARY KEY, not as the bare UNIQUE
   index it used to be downgraded to — otherwise the restored schema reports no
   key and permits NULLs where the original did not. *)
let dump_restores_composite_pk_as_a_primary_key () =
  let src = run (Db.open_in_memory ()) in
  exec src "CREATE TABLE t (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
  let script = dump_of src in
  run (Db.close src);
  let dst = restore script in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close dst) with
      | _ -> ())
    (fun () ->
       Alcotest.(check string)
         "restored DDL matches the original"
         "CREATE TABLE t (k TEXT NOT NULL, j TEXT NOT NULL, v INTEGER, PRIMARY KEY (k, \
          j))"
         (ddl_of dst "t");
       rejects_null dst ~col:"j" "INSERT INTO t VALUES ('a', NULL, 1)")
;;

(* ------------------------------------------------------------------ *)
(* ALTER TABLE ADD COLUMN                                               *)
(* ------------------------------------------------------------------ *)

(* #533: [ALTER TABLE ... ADD COLUMN ... PRIMARY KEY] used to be accepted and
   was a lie.  No [__pk] index was built for the added column, so it enforced
   neither uniqueness nor NULL-ness — and once #530 made a restored PRIMARY KEY
   column NOT NULL, the DDL it rendered stopped round-tripping (the NULLs it had
   happily stored would not re-insert).  It is refused now, as in SQLite. *)
let alter_add_primary_key_column_is_refused () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    match run (Db.execute db "ALTER TABLE t ADD COLUMN k TEXT PRIMARY KEY") with
    | Ok () -> Alcotest.fail "expected ADD COLUMN ... PRIMARY KEY to be refused"
    | Error e ->
      let msg = Format.asprintf "%a" Db.pp_error e in
      Alcotest.(check bool)
        (Printf.sprintf "refusal names PRIMARY KEY (got %S)" msg)
        true
        (contains_sub ~needle:"cannot add a PRIMARY KEY column" msg))
;;

(* The rest of ADD COLUMN is untouched. *)
let alter_add_plain_column_still_works () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "INSERT INTO t VALUES (1)";
    exec db "ALTER TABLE t ADD COLUMN k TEXT";
    Alcotest.(check int) "still one row" 1 (count db "SELECT * FROM t"))
;;

(* ------------------------------------------------------------------ *)
(* Pre-existing databases (#533 / #542)                                 *)
(* ------------------------------------------------------------------ *)

(* [primary_key]/[not_null] are stored PER COLUMN, so #530's fix reaches only
   tables created after it.  A file written by an older build stores
   [primary_key = false] for every column of a table-level PRIMARY KEY (and the
   oldest column encoding has no flag bytes at all, so [decode_column] defaults
   BOTH to false for every column of every table).  Reopening such a file used
   to keep the old permissive INSERT semantics — and, once #526's exemption was
   removed, to silently drop the composite-PK point lookup out of the planner's
   one-row class, reverting the #513 StockLevel win on every existing database.
   [Catalog.open_] now re-derives the flags from the implicit PK index.

   The fixture is built through the CATALOG API, not through SQL: SQL now
   (correctly) sets the flags, so writing the legacy shape is the only way to
   test against it rather than assume it. *)

let legacy_col name : Row.column =
  { Row.name
  ; ty = Row.Integer
  ; not_null = false
  ; primary_key = false
  ; pk_desc = false
  ; default = None
  ; check_sql = None
  ; generated_as = None
  }
;;

let write_legacy_pk_db path =
  run
    (let open Lwt.Syntax in
     let* store =
       let* r = Granary_unix.Store.open_file ~path () in
       match r with
       | Ok s -> Lwt.return s
       | Error _ -> Alcotest.failf "cannot create %s" path
     in
     let* cat = Cat.open_ store in
     let table name cols pk =
       let* _tid =
         Cat.create_table
           cat
           ~name
           ~columns:(List.map legacy_col cols)
           ~without_rowid:false
           ~autoincrement:false
       in
       let* r =
         Cat.create_index
           cat
           ~name:(Printf.sprintf "__pk_%s_%s_0" name (String.concat "_" pk))
           ~table:name
           ~columns:pk
           ~unique:true
           ~expr_flags:(List.map (fun _ -> false) pk)
           ~where_sql:None
           ~origin:`Implicit_pk
       in
       match r with
       | Ok _ -> Lwt.return_unit
       | Error m -> Alcotest.failf "create_index %s: %s" name m
     in
     let* () = table "line" [ "w"; "o"; "i_id" ] [ "w"; "o" ] in
     let* () = table "stock" [ "sw"; "si"; "qty" ] [ "sw"; "si" ] in
     Granary_store.Store.close store)
;;

let with_legacy_db f =
  let path = Filename.temp_file "granary_530_legacy_" ".db" in
  Sys.remove path;
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () ->
       write_legacy_pk_db path;
       let db =
         match run (Granary_unix.open_file ~path ()) with
         | Ok db -> db
         | Error e -> Alcotest.failf "reopen %s: %a" path Db.pp_error e
       in
       Fun.protect
         ~finally:(fun () ->
           try run (Db.close db) with
           | _ -> ())
         (fun () -> f db))
;;

(* The flags themselves: reopening re-derives both from the PK index. *)
let legacy_db_reports_pk_columns_not_null () =
  with_legacy_db (fun db ->
    Alcotest.(check (list (triple string string string)))
      "flags re-derived on load"
      [ "sw", "1", "1"; "si", "1", "1"; "qty", "0", "0" ]
      (table_info_flags db "stock"))
;;

(* Which is what #530 actually asked for: the file's own INSERT semantics. *)
let legacy_db_rejects_null_in_a_pk_column () =
  with_legacy_db (fun db ->
    rejects_null db ~col:"si" "INSERT INTO stock VALUES (1, NULL, 5)";
    exec db "INSERT INTO stock VALUES (1, 2, 5)";
    Alcotest.(check int) "the non-NULL row is fine" 1 (count db "SELECT * FROM stock"))
;;

(* And the planner consequence, in the #513 StockLevel shape: a full-key seek on
   the composite PK reaches one row, so the join must keep its probe.  Losing it
   reads all of [stock] instead — 201 rows examined against 2. *)
let legacy_db_keeps_the_composite_pk_probe () =
  with_legacy_db (fun db ->
    exec db "BEGIN";
    for o = 1 to 2000 do
      exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o ((o mod 200) + 1))
    done;
    for si = 1 to 200 do
      exec db (Printf.sprintf "INSERT INTO stock VALUES (1, %d, %d)" si (si * 10))
    done;
    exec db "COMMIT";
    let sql =
      "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND o = 5 AND sw = \
       1"
    in
    let rows, stats =
      match run (Db.query_with_stats db sql) with
      | Error e -> Alcotest.failf "query: %a" Db.pp_error e
      | Ok (stream, stats) -> run (Lwt_stream.to_list stream), stats
    in
    Alcotest.(check int) "one joined row" 1 (List.length rows);
    Alcotest.(check int)
      "one driving row and one probed stock row, not a scan of 200"
      2
      stats.Db.rows_examined)
;;

(* ------------------------------------------------------------------ *)
(* The marking function itself                                          *)
(* ------------------------------------------------------------------ *)

(* Property: [mark_table_pk] sets [primary_key] on exactly the union of every
   [TC_primary_key]'s [pk_cols] that names a real column, keeps every column in
   place, and changes nothing else about any of them.

   The example-based cases above all use one or two constraints; a global
   "more than one marked column" predicate satisfies them and is still wrong
   (#533 item 1).  A generated constraint list catches that class. *)
let col name : Ast.column_def =
  { Ast.name
  ; ty = Ast.Ty_int
  ; not_null = false
  ; primary_key = false
  ; autoincrement = false
  ; pk_desc = false
  ; default = None
  ; check = None
  ; fk_ref = None
  ; generated_as = None
  }
;;

let names = [ "a"; "b"; "c"; "d" ]
let gen_name = QCheck2.Gen.oneof_list names

let gen_constraint =
  let open QCheck2.Gen in
  oneof_weighted
    [ ( 3
      , let+ cols = list_size (int_range 1 3) gen_name in
        Ast.TC_primary_key { pk_cols = cols; autoincrement = false } )
    ; ( 1
      , let+ cols = list_size (int_range 1 3) gen_name in
        Ast.TC_unique cols )
    ]
;;

let gen_case =
  let open QCheck2.Gen in
  let* cols = list_size (int_range 0 4) gen_name in
  let+ constraints = list_size (int_range 0 3) gen_constraint in
  List.map col cols, constraints
;;

let mark_table_pk_marks_exactly_the_key_columns =
  QCheck2.Test.make
    ~count:500
    ~name:"mark_table_pk marks exactly the union of every table-level PRIMARY KEY"
    gen_case
    (fun (columns, constraints) ->
       let marked = Sema.mark_table_pk constraints columns in
       let want =
         List.concat_map
           (function
             | Ast.TC_primary_key { pk_cols; _ } -> pk_cols
             | _ -> [])
           constraints
       in
       List.length marked = List.length columns
       && List.for_all2
            (fun (before : Ast.column_def) (after : Ast.column_def) ->
               String.equal before.Ast.name after.Ast.name
               && after.Ast.primary_key
                  = (before.Ast.primary_key || List.mem before.Ast.name want)
               && { before with Ast.primary_key = after.Ast.primary_key } = after)
            columns
            marked)
;;

let () =
  Alcotest.run
    "pk_not_null_530"
    [ ( "spellings agree"
      , [ Alcotest.test_case
            "column constraint PK rejects NULL"
            `Quick
            column_constraint_pk_rejects_null
        ; Alcotest.test_case
            "table constraint PK rejects NULL"
            `Quick
            table_constraint_pk_rejects_null
        ; Alcotest.test_case
            "composite PK rejects NULL in first column"
            `Quick
            composite_pk_rejects_null_in_first_column
        ; Alcotest.test_case
            "composite PK rejects NULL in second column"
            `Quick
            composite_pk_rejects_null_in_second_column
        ; Alcotest.test_case
            "composite PK rejects omitted column"
            `Quick
            composite_pk_rejects_omitted_column
        ; Alcotest.test_case
            "all spellings accept non-NULL"
            `Quick
            all_spellings_accept_non_null
        ; Alcotest.test_case
            "non-key column still accepts NULL"
            `Quick
            non_key_column_still_accepts_null
        ; Alcotest.test_case
            "explicit NOT NULL alongside PK is fine"
            `Quick
            explicit_not_null_alongside_pk_is_fine
        ; Alcotest.test_case
            "UPDATE to NULL is rejected"
            `Quick
            update_to_null_is_rejected
        ] )
    ; ( "rowid alias unchanged"
      , [ Alcotest.test_case
            "rowid alias NULL still auto-assigns"
            `Quick
            rowid_alias_null_still_auto_assigns
        ; Alcotest.test_case
            "rowid alias omitted still auto-assigns"
            `Quick
            rowid_alias_omitted_still_auto_assigns
        ; Alcotest.test_case
            "INTEGER PRIMARY KEY DESC rejects NULL"
            `Quick
            integer_pk_desc_rejects_null
        ] )
    ; ( "WITHOUT ROWID"
      , [ Alcotest.test_case
            "single-column PK rejects NULL"
            `Quick
            without_rowid_single_pk_rejects_null
        ; Alcotest.test_case
            "composite PK is still unsupported"
            `Quick
            without_rowid_composite_pk_is_still_unsupported
        ] )
    ; ( "catalog flags"
      , [ Alcotest.test_case
            "PRAGMA table_info reports PK columns NOT NULL"
            `Quick
            pragma_reports_pk_columns_not_null
        ; Alcotest.test_case
            "composite PK DDL renders the table-level key"
            `Quick
            composite_pk_ddl_renders_the_table_level_key
        ; Alcotest.test_case
            "single-column PK DDL still inline"
            `Quick
            single_pk_ddl_still_inline
        ; Alcotest.test_case
            "two PRIMARY KEY declarations both render"
            `Quick
            two_primary_key_declarations_both_render
        ; Alcotest.test_case
            "column PK beside a composite PK renders both"
            `Quick
            column_pk_beside_composite_pk_renders_both
        ] )
    ; ( "dump round-trip"
      , [ Alcotest.test_case
            "single-column table-level PK"
            `Quick
            dump_round_trips_single_table_level_pk
        ; Alcotest.test_case
            "two PRIMARY KEY declarations"
            `Quick
            dump_round_trips_two_pk_declarations
        ; Alcotest.test_case "composite PK" `Quick dump_round_trips_composite_pk
        ; Alcotest.test_case
            "composite PK restores as a PRIMARY KEY"
            `Quick
            dump_restores_composite_pk_as_a_primary_key
        ] )
    ; ( "ALTER TABLE ADD COLUMN"
      , [ Alcotest.test_case
            "PRIMARY KEY column is refused"
            `Quick
            alter_add_primary_key_column_is_refused
        ; Alcotest.test_case
            "plain column still works"
            `Quick
            alter_add_plain_column_still_works
        ] )
    ; ( "pre-existing databases"
      , [ Alcotest.test_case
            "legacy catalog flags are re-derived on load"
            `Quick
            legacy_db_reports_pk_columns_not_null
        ; Alcotest.test_case
            "legacy database rejects NULL in a PK column"
            `Quick
            legacy_db_rejects_null_in_a_pk_column
        ; Alcotest.test_case
            "legacy database keeps the composite PK probe"
            `Slow
            legacy_db_keeps_the_composite_pk_probe
        ] )
    ; ( "mark_table_pk"
      , List.map
          (QCheck_alcotest.to_alcotest ~verbose:false)
          [ mark_table_pk_marks_exactly_the_key_columns ] )
    ]
;;
