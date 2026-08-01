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
   looked like it had no PRIMARY KEY at all and was rejected as such.  Both
   messages refuse the table, so this pins only that it is still refused —
   asserting the exact text would make the check about the wording. *)
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
        (contains_sub ~needle:"unsupported" msg))
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
   marked.  [Row.column] records no key ordinal, so the table-level form cannot
   be reconstructed faithfully either; a composite PK therefore stays absent
   from the DDL and keeps round-tripping as the CREATE UNIQUE INDEX documented
   in [Db.dump] — but the NOT NULL it now implies IS rendered, so the restored
   table keeps that much. *)
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

let composite_pk_ddl_has_no_per_column_primary_key () =
  with_db (fun db ->
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    let sql = ddl_of db "c" in
    Alcotest.(check bool)
      (Printf.sprintf "no inline PRIMARY KEY in %S" sql)
      false
      (contains_sub ~needle:"PRIMARY KEY" sql);
    Alcotest.(check bool)
      (Printf.sprintf "NOT NULL rendered in %S" sql)
      true
      (contains_sub ~needle:"NOT NULL" sql))
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
            "composite PK DDL has no per-column PRIMARY KEY"
            `Quick
            composite_pk_ddl_has_no_per_column_primary_key
        ; Alcotest.test_case
            "single-column PK DDL still inline"
            `Quick
            single_pk_ddl_still_inline
        ] )
    ]
;;
