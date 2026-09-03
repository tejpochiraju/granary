(** #577: [ALTER TABLE ... RENAME COLUMN] to a RESERVED WORD corrupted every
    piece of SQL text the catalog stores about the table.

    #553 (PR #564) made the rename remap stored SQL — a CHECK expression, a
    GENERATED column's body, an expression index's column, a partial index's
    WHERE. #572 (PR #587) made the substitution delimit a new name that is not
    a plain [[A-Za-z_][A-Za-z0-9_]*] word. A reserved word IS a plain word, so
    it slipped through bare:

    {v
      CREATE TABLE r (a INTEGER, b INTEGER CHECK (a > 0));
      ALTER TABLE r RENAME COLUMN a TO "order";
      INSERT INTO r VALUES (1, 1);
        -> runtime error: CHECK constraint parse error for r.col1: (order > 0)
    v}

    The rename succeeded, silently, and left the table permanently
    un-insertable — its stored text and its dump DDL no longer parsed.

    The fix hoists the whole quoting rule, keyword table included, into
    {!Granary_encoding.Sql_ident}, which [granary.catalog] and [granary.sql]
    both depend on. Options 2-4 in the issue all leave two copies of the rule
    or refuse a rename SQLite allows; the oracle below is real sqlite3, which
    writes [CHECK ("order" > 0)].

    What is tested, per surface (CHECK, GENERATED VIRTUAL, GENERATED STORED,
    partial-index WHERE, expression index):

    - the constraint is still LIVE after the rename — a violating row is
      rejected as a constraint failure, never as a parse error, because a CHECK
      that quietly stopped being enforced passes an insert-only test;
    - the same, after close and REOPEN. A corrupted [generated_as] is only
      re-parsed on a later open, so an in-session assertion misses it entirely
      (noted on the issue);
    - [sqlite_master] DDL and [Db.dump] output replay into a fresh database.

    {2 Soft and hard reserved words — pick a HARD one, or the case is inert}

    A reserved word here is not automatically a load-bearing test input. The
    corruption this file guards against writes the new name into a bare
    position, so a case only goes red if the resulting text FAILS to re-parse.
    Measured against a pre-fix catalog: of the 197 words in
    {!Granary_encoding.Sql_ident.sql_keywords}, {b 126 are "soft"} — the
    grammar still accepts them bare in the position they land in, so the
    corrupted text re-parses and the test passes {i against the bug} — and only
    71 are "hard".

    [key] is soft. [order], [select] and [group] are hard, which is why every
    case below is written with one of those three. #617 was exactly this trap:
    [build_everything] renamed the GENERATED table's column to ["key"], so that
    surface contributed nothing to the sensitivity of
    [sqlite_master_ddl_replays] or [dump_round_trips] — both stayed red pre-fix
    only through the CHECK and partial-index surfaces, and a future change that
    broke GENERATED alone would have left both green.

    The QCheck property at the bottom is the exception, and deliberately so: it
    draws [~count:60] over all 197 words, so P(a green run against the bug) is
    about [0.64 ** 60] — around [2e-12]. *)

open Lwt.Syntax
open Granary_sql
module Db = Granary.Db
module Cat = Granary_catalog.Catalog

let run = Lwt_main.run

(* ── helpers ──────────────────────────────────────────────────────── *)

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let exec_err db sql =
  match run (Db.execute db sql) with
  | Ok () -> None
  | Error e -> Some (Format.asprintf "%a" Db.pp_error e)
;;

let rows db sql =
  run
    (let* s = Db.query db sql in
     match s with
     | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
     | Ok s -> Lwt_stream.to_list s)
;;

let one_int db sql =
  match rows db sql with
  | [ [| Db.V_int i |] ] -> i
  | _ -> Alcotest.failf "expected one integer row from %S" sql
;;

let lower_contains hay needle =
  let hay = String.lowercase_ascii hay in
  let lh = String.length hay
  and ln = String.length needle in
  let rec go i = i + ln <= lh && (String.sub hay i ln = needle || go (i + 1)) in
  go 0
;;

(* The failure this issue is about announces itself as a PARSE error on stored
   text.  Anything that mentions "parse" is #577 returning, whichever statement
   provoked it. *)
let refute_parse_error where = function
  | None -> ()
  | Some msg ->
    if lower_contains msg "parse"
    then Alcotest.failf "%s: stored SQL no longer parses (#577): %s" where msg
;;

let expect_ok db where sql =
  match exec_err db sql with
  | None -> ()
  | Some msg ->
    refute_parse_error where (Some msg);
    Alcotest.failf "%s: %S failed: %s" where sql msg
;;

(* A constraint must FIRE, and fire as a constraint — not as a parse error. *)
let expect_constraint_violation db where sql =
  match exec_err db sql with
  | None -> Alcotest.failf "%s: %S was accepted; the constraint is not live" where sql
  | Some msg -> refute_parse_error where (Some msg)
;;

let with_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

(* A file-backed database, handed to [f] as an [open_it] the test can call more
   than once — the reopen is the whole point of these cases. *)
let with_file_db name f =
  let path = Filename.temp_file ("granary_577_" ^ name ^ "_") ".db" in
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
       f open_it)
;;

let close db =
  try run (Db.close db) with
  | _ -> ()
;;

let all_ddl db =
  rows db "SELECT sql FROM sqlite_master ORDER BY name"
  |> List.filter_map (function
    | [| Db.V_text s |] -> Some s
    | _ -> None)
;;

(* ── the rule itself ──────────────────────────────────────────────── *)

(* The rewriter is what #577 broke, and it is reachable directly.  Renaming
   INTO a reserved word must produce the delimited spelling in a bare
   position — the same answer [Ast.quote_ident] gives. *)
let rewrite_delimits_reserved_words () =
  let rw = Cat.rewrite_ident_in_sql ~old_name:"a" ~new_name:"order" in
  Alcotest.(check string) "bare position" "(\"order\" > 0)" (rw "(a > 0)");
  Alcotest.(check string)
    "already delimited stays delimited"
    "(\"order\" > 0)"
    (Cat.rewrite_ident_in_sql ~old_name:"a" ~new_name:"order" "(\"a\" > 0)");
  Alcotest.(check string)
    "an ordinary rename is still bare — no cosmetic churn"
    "(b > 0)"
    (Cat.rewrite_ident_in_sql ~old_name:"a" ~new_name:"b" "(a > 0)");
  Alcotest.(check string)
    "a name needing quotes for other reasons (#572)"
    "(\"my col\" > 0)"
    (Cat.rewrite_ident_in_sql ~old_name:"a" ~new_name:"my col" "(a > 0)");
  (* a keyword in a NON-column position is left alone: this is a token scan,
     and [order] here is the SQL keyword, not the column being renamed *)
  Alcotest.(check string)
    "keywords already in the text are untouched"
    "SELECT \"order\" FROM t ORDER BY \"order\""
    (rw "SELECT a FROM t ORDER BY a")
;;

(* The catalog and the parser must answer the same question.  Two copies of the
   rule at two levels of the dependency graph was the bug; one implementation
   in [granary.encoding] is the fix, and this is the assertion that says so. *)
let one_rule_across_the_libraries () =
  let names =
    [ "a"; "plain"; ""; "my col"; "1col"; "a\"b"; "order"; "KEY"; "Select"; "log2" ]
    @ Ast.sql_keywords
  in
  List.iter
    (fun s ->
       let want = Ast.quote_ident s in
       Alcotest.(check string)
         (Printf.sprintf "Sql_ident.quote_ident %S" s)
         want
         (Granary_encoding.Sql_ident.quote_ident s);
       Alcotest.(check string)
         (Printf.sprintf "Exec.quote_ident %S" s)
         want
         (Exec.quote_ident s);
       (* and the rewriter, which is what the catalog reaches for *)
       Alcotest.(check string)
         (Printf.sprintf "rewrite_ident_in_sql into %S" s)
         (Printf.sprintf "(%s > 0)" want)
         (Cat.rewrite_ident_in_sql ~old_name:"a" ~new_name:s "(a > 0)"))
    names
;;

(* ── surface 1: CHECK ─────────────────────────────────────────────── *)

let check_survives_rename_to name =
  with_db (fun db ->
    exec db "CREATE TABLE r (a INTEGER, b INTEGER CHECK (a > 0))";
    exec db (Printf.sprintf "ALTER TABLE r RENAME COLUMN a TO %s" (Ast.quote_ident name));
    expect_ok db "CHECK after rename" "INSERT INTO r VALUES (1, 1)";
    Alcotest.(check int64) "row landed" 1L (one_int db "SELECT COUNT(*) FROM r");
    expect_constraint_violation db "CHECK after rename" "INSERT INTO r VALUES (0, 1)")
;;

let check_issue_repro () = check_survives_rename_to "order"

(* Every reserved word, not a hand-picked handful: the class is the list. *)
let check_every_keyword () =
  List.iter
    (fun k ->
       (* the table's own columns are [a] and [b]; nothing in the list collides *)
       check_survives_rename_to (String.lowercase_ascii k))
    Ast.sql_keywords
;;

let check_survives_reopen () =
  with_file_db "check" (fun open_it ->
    let db = open_it () in
    exec db "CREATE TABLE r (a INTEGER, b INTEGER CHECK (a > 0))";
    exec db "ALTER TABLE r RENAME COLUMN a TO \"order\"";
    let before = all_ddl db in
    close db;
    let db = open_it () in
    Fun.protect
      ~finally:(fun () -> close db)
      (fun () ->
         Alcotest.(check (list string)) "DDL is durable" before (all_ddl db);
         expect_ok db "CHECK after reopen" "INSERT INTO r VALUES (1, 1)";
         expect_constraint_violation db "CHECK after reopen" "INSERT INTO r VALUES (0, 1)"))
;;

(* ── surface 2: GENERATED ─────────────────────────────────────────── *)

(* The issue's own note: a corrupted [generated_as] does NOT fail the next
   INSERT in-session — the rows land — so this has to be a reopen test or the
   failure mode is missed entirely. *)
let generated_survives_reopen kind word =
  let q = Ast.quote_ident word in
  with_file_db
    ("gen_" ^ String.lowercase_ascii kind)
    (fun open_it ->
       let db = open_it () in
       exec
         db
         (Printf.sprintf
            "CREATE TABLE g (a INTEGER, d INTEGER GENERATED ALWAYS AS (a + 1) %s)"
            kind);
       exec db (Printf.sprintf "ALTER TABLE g RENAME COLUMN a TO %s" q);
       expect_ok
         db
         "GENERATED before reopen"
         (Printf.sprintf "INSERT INTO g (%s) VALUES (7)" q);
       Alcotest.(check int64) "computed in session" 8L (one_int db "SELECT d FROM g");
       let before = all_ddl db in
       close db;
       let db = open_it () in
       Fun.protect
         ~finally:(fun () -> close db)
         (fun () ->
            Alcotest.(check (list string)) "DDL is durable" before (all_ddl db);
            Alcotest.(check int64)
              "the pre-reopen row still computes"
              8L
              (one_int db (Printf.sprintf "SELECT d FROM g WHERE %s = 7" q));
            expect_ok
              db
              "GENERATED after reopen"
              (Printf.sprintf "INSERT INTO g (%s) VALUES (41)" q);
            Alcotest.(check int64)
              "computed after reopen"
              42L
              (one_int db (Printf.sprintf "SELECT d FROM g WHERE %s = 41" q))))
;;

(* [order] and [select], not [key]: the lexer lets some reserved words through
   in expression position, so a soft one would let the corrupted text re-parse
   and the test would pass against the bug. *)
let generated_virtual_survives_reopen () = generated_survives_reopen "VIRTUAL" "order"
let generated_stored_survives_reopen () = generated_survives_reopen "STORED" "select"

(* ── surface 3: partial index WHERE ───────────────────────────────── *)

let partial_index_survives_reopen () =
  with_file_db "partial" (fun open_it ->
    let db = open_it () in
    exec db "CREATE TABLE p (a INTEGER, v INTEGER)";
    exec db "CREATE INDEX pi ON p (v) WHERE a > 0";
    exec db "ALTER TABLE p RENAME COLUMN a TO \"group\"";
    expect_ok db "partial index" "INSERT INTO p VALUES (1, 5)";
    expect_ok db "partial index" "INSERT INTO p VALUES (0, 6)";
    let before = all_ddl db in
    close db;
    let db = open_it () in
    Fun.protect
      ~finally:(fun () -> close db)
      (fun () ->
         Alcotest.(check (list string)) "DDL is durable" before (all_ddl db);
         expect_ok db "partial index after reopen" "INSERT INTO p VALUES (2, 7)";
         Alcotest.(check int64) "all rows" 3L (one_int db "SELECT COUNT(*) FROM p");
         (* the index must still answer for the rows it covers *)
         Alcotest.(check int64)
           "indexed row readable"
           5L
           (one_int db "SELECT v FROM p WHERE v = 5")))
;;

(* ── surface 4: expression index ──────────────────────────────────── *)

let expression_index_survives_reopen () =
  with_file_db "expr" (fun open_it ->
    let db = open_it () in
    exec db "CREATE TABLE e (a INTEGER, v INTEGER)";
    exec db "CREATE INDEX ei ON e (a + 1)";
    exec db "ALTER TABLE e RENAME COLUMN a TO \"order\"";
    expect_ok db "expression index" "INSERT INTO e VALUES (1, 5)";
    let before = all_ddl db in
    close db;
    let db = open_it () in
    Fun.protect
      ~finally:(fun () -> close db)
      (fun () ->
         Alcotest.(check (list string)) "DDL is durable" before (all_ddl db);
         expect_ok db "expression index after reopen" "INSERT INTO e VALUES (2, 6)";
         Alcotest.(check int64) "rows landed" 2L (one_int db "SELECT COUNT(*) FROM e")))
;;

(* ── the DDL itself must replay ───────────────────────────────────── *)

(* Everything above proves the ENGINE can read its own text back.  This proves
   the text it hands out is SQL: [sqlite_master] and [Db.dump] both go to a
   user, and a dump that will not restore is the same corruption one step
   later. *)
let build_everything db =
  exec db "CREATE TABLE r (a INTEGER, b INTEGER CHECK (a > 0))";
  exec db "CREATE TABLE g (a INTEGER, d INTEGER GENERATED ALWAYS AS (a + 1) STORED)";
  exec db "CREATE TABLE p (a INTEGER, v INTEGER)";
  exec db "CREATE INDEX pi ON p (v) WHERE a > 0";
  exec db "CREATE INDEX ei ON p (a + 1)";
  List.iter
    (fun (t, w) -> exec db (Printf.sprintf "ALTER TABLE %s RENAME COLUMN a TO %s" t w))
    [ "r", "\"order\""; "g", "\"select\""; "p", "\"group\"" ]
;;

(* Replay a DDL list into a fresh database and hand it to [f].  Tables before
   indexes — [all_ddl] sorts by name, which interleaves them — and [order] is
   an optional pre-sort for the cases where one table references another. *)
let replay_ddl ?(order = fun l -> l) ddl f =
  let is_index s =
    lower_contains s "create index" || lower_contains s "create unique index"
  in
  let tables, indexes = List.partition (fun s -> not (is_index s)) ddl in
  with_db (fun db ->
    List.iter (fun s -> expect_ok db "replayed DDL" s) (order tables @ indexes);
    f db)
;;

let sqlite_master_ddl_replays () =
  let ddl =
    with_db (fun db ->
      build_everything db;
      all_ddl db)
  in
  replay_ddl ddl (fun db ->
    expect_ok db "replayed CHECK" "INSERT INTO r VALUES (1, 1)";
    expect_constraint_violation db "replayed CHECK" "INSERT INTO r VALUES (0, 1)";
    expect_ok db "replayed GENERATED" "INSERT INTO g (\"select\") VALUES (7)";
    Alcotest.(check int64) "generated value" 8L (one_int db "SELECT d FROM g"))
;;

let dump_round_trips () =
  let script =
    with_db (fun db ->
      build_everything db;
      exec db "INSERT INTO r VALUES (1, 1)";
      exec db "INSERT INTO g (\"select\") VALUES (7)";
      exec db "INSERT INTO p VALUES (1, 5)";
      match run (Db.dump_to_string db ()) with
      | Ok s -> s
      | Error e -> Alcotest.failf "dump: %a" Db.pp_error e)
  in
  (* the dump must not have lost a delimiter anywhere *)
  List.iter
    (fun needle ->
       if not (lower_contains script needle)
       then Alcotest.failf "dump lost the delimiters around %s:\n%s" needle script)
    [ "\"order\""; "\"select\""; "\"group\"" ];
  with_db (fun db ->
    String.split_on_char '\n' script
    |> List.iter (fun line ->
      let line = String.trim line in
      if line <> "" then expect_ok db "replayed dump" line);
    Alcotest.(check int64) "r restored" 1L (one_int db "SELECT COUNT(*) FROM r");
    Alcotest.(check int64) "g restored" 8L (one_int db "SELECT d FROM g");
    Alcotest.(check int64) "p restored" 1L (one_int db "SELECT COUNT(*) FROM p");
    (* and the restored constraints are live, not decorative *)
    expect_constraint_violation db "restored CHECK" "INSERT INTO r VALUES (0, 1)")
;;

(* ── the other direction, and the no-churn guarantee ──────────────── *)

(* Renaming AWAY from a reserved word: the stored text held [ "order" ], and
   the new name is ordinary, so it must come back out bare. *)
let rename_away_from_reserved () =
  with_db (fun db ->
    exec db "CREATE TABLE r (\"order\" INTEGER, b INTEGER CHECK (\"order\" > 0))";
    exec db "ALTER TABLE r RENAME COLUMN \"order\" TO a";
    expect_ok db "renamed away from a keyword" "INSERT INTO r VALUES (1, 1)";
    expect_constraint_violation db "CHECK still live" "INSERT INTO r VALUES (0, 1)";
    Alcotest.(check int64) "readable under the new name" 1L (one_int db "SELECT a FROM r"))
;;

(* Option 3 on the issue — delimit unconditionally — was rejected because it
   rewrites [(a > 0)] to [("b" > 0)] on every ordinary rename.  This is the
   assertion that the shipped fix did not quietly do that. *)
let ordinary_rename_stays_bare () =
  with_db (fun db ->
    exec db "CREATE TABLE r (a INTEGER, b INTEGER CHECK (a > 0))";
    exec db "ALTER TABLE r RENAME COLUMN a TO c";
    match all_ddl db with
    | [ sql ] ->
      if lower_contains sql "\"c\""
      then Alcotest.failf "an ordinary rename over-quoted the stored text: %s" sql
    | l -> Alcotest.failf "expected one DDL row, got %d" (List.length l))
;;

(* QCheck: for any reserved word, the whole CHECK cycle holds. *)
let arb_keyword =
  QCheck.make
    ~print:(Printf.sprintf "%S")
    QCheck.Gen.(oneof_list (List.map String.lowercase_ascii Ast.sql_keywords))
;;

let prop_check_rename =
  QCheck.Test.make
    ~count:60
    ~name:"#577 RENAME COLUMN to a reserved word keeps the CHECK live"
    arb_keyword
    (fun k ->
       check_survives_rename_to k;
       true)
;;

(* ── #618: four stored-SQL surfaces PR #610 verified only by hand ──── *)

(* All four route through [Exec.quote_ident] and so were correct by
   construction — which is exactly the kind of coverage that quietly stops
   being true.  Each closes and REOPENS, because a reopen is the only thing
   that re-parses stored text; each asserts the constraint still FIRES, because
   "the DDL re-parses" and "the DDL still means what it said" are different
   claims and only the second is worth having; and each replays its own
   [sqlite_master] DDL into a fresh database.

   Every case is written with a HARD reserved word — see the header.  With a
   soft one the corrupted text re-parses and the case passes against the bug. *)

(* Surface 1: a FOREIGN KEY's parent-column list ([Exec] renders it at the
   [FOREIGN KEY (...) REFERENCES t(...)] clause, one [quote_ident] per side). *)
let fk_parent_cols_survive_reopen () =
  with_file_db "fk" (fun open_it ->
    let seed db =
      exec db "PRAGMA foreign_keys = 1";
      exec db "CREATE TABLE par (\"order\" INTEGER NOT NULL PRIMARY KEY, v INTEGER)";
      exec db "CREATE TABLE ch (x INTEGER, FOREIGN KEY (x) REFERENCES par(\"order\"))"
    in
    let db = open_it () in
    seed db;
    exec db "INSERT INTO par VALUES (1, 10)";
    expect_ok db "FK satisfied" "INSERT INTO ch VALUES (1)";
    expect_constraint_violation db "FK live" "INSERT INTO ch VALUES (99)";
    let before = all_ddl db in
    close db;
    let db = open_it () in
    Fun.protect
      ~finally:(fun () -> close db)
      (fun () ->
         exec db "PRAGMA foreign_keys = 1";
         Alcotest.(check (list string)) "DDL is durable" before (all_ddl db);
         expect_ok db "FK satisfied after reopen" "INSERT INTO ch VALUES (1)";
         expect_constraint_violation
           db
           "FK live after reopen"
           "INSERT INTO ch VALUES (98)");
    (* and the emitted DDL is SQL: parent before child, or the FK dangles *)
    replay_ddl
      ~order:
        (List.sort (fun a b ->
           compare (lower_contains b "table par") (lower_contains a "table par")))
      before
      (fun db ->
         exec db "PRAGMA foreign_keys = 1";
         exec db "INSERT INTO par VALUES (1, 10)";
         expect_ok db "replayed FK satisfied" "INSERT INTO ch VALUES (1)";
         expect_constraint_violation db "replayed FK live" "INSERT INTO ch VALUES (99)"))
;;

(* Surface 2: the index a table-level UNIQUE constraint creates behind the
   caller's back — its column list is emitted from the catalog, not echoed. *)
let table_level_unique_survives_reopen () =
  with_file_db "uniq" (fun open_it ->
    let db = open_it () in
    exec db "CREATE TABLE tc (\"group\" INTEGER, b INTEGER, UNIQUE (\"group\", b))";
    exec db "INSERT INTO tc VALUES (1, 2)";
    expect_constraint_violation db "UNIQUE live" "INSERT INTO tc VALUES (1, 2)";
    expect_ok db "a different key is fine" "INSERT INTO tc VALUES (1, 3)";
    let before = all_ddl db in
    close db;
    let db = open_it () in
    Fun.protect
      ~finally:(fun () -> close db)
      (fun () ->
         Alcotest.(check (list string)) "DDL is durable" before (all_ddl db);
         expect_constraint_violation
           db
           "UNIQUE live after reopen"
           "INSERT INTO tc VALUES (1, 2)";
         expect_ok db "a different key after reopen" "INSERT INTO tc VALUES (1, 4)";
         Alcotest.(check int64) "rows landed" 3L (one_int db "SELECT COUNT(*) FROM tc"));
    replay_ddl before (fun db ->
      exec db "INSERT INTO tc VALUES (1, 2)";
      expect_constraint_violation db "replayed UNIQUE live" "INSERT INTO tc VALUES (1, 2)"))
;;

(* Surface 3: WITHOUT ROWID.  Two tables, because the PRIMARY KEY reaches the
   emitted DDL by two different routes: inline on the column ([wr], the issue's
   own shape) and as a table-level [PRIMARY KEY (...)] clause ([wr2]).  The
   trailing WITHOUT ROWID has to survive with both, and so does the backing
   [__pk_] index's column list. *)
let without_rowid_survives_reopen () =
  with_file_db "wr" (fun open_it ->
    let create db =
      exec
        db
        "CREATE TABLE wr (\"select\" INTEGER NOT NULL PRIMARY KEY, \"order\" INTEGER) \
         WITHOUT ROWID";
      exec
        db
        "CREATE TABLE wr2 (\"select\" INTEGER NOT NULL, \"order\" INTEGER, PRIMARY KEY \
         (\"select\")) WITHOUT ROWID"
    in
    let seed db =
      exec db "INSERT INTO wr VALUES (1, 10)";
      exec db "INSERT INTO wr2 VALUES (1, 10)"
    in
    let pk_is_live db where =
      expect_constraint_violation db (where ^ " wr PK") "INSERT INTO wr VALUES (1, 11)";
      expect_constraint_violation db (where ^ " wr2 PK") "INSERT INTO wr2 VALUES (1, 11)"
    in
    let db = open_it () in
    create db;
    seed db;
    pk_is_live db "before reopen";
    let before = all_ddl db in
    close db;
    let db = open_it () in
    Fun.protect
      ~finally:(fun () -> close db)
      (fun () ->
         Alcotest.(check (list string)) "DDL is durable" before (all_ddl db);
         let tables, indexes =
           List.partition (fun sql -> lower_contains sql "create table") before
         in
         (* both CREATE TABLEs keep their delimiters and their WITHOUT ROWID *)
         List.iter
           (fun sql ->
              if not (lower_contains sql "\"select\"" && lower_contains sql "\"order\"")
              then Alcotest.failf "a reserved name lost its delimiters: %s" sql;
              if not (lower_contains sql "without rowid")
              then Alcotest.failf "the WITHOUT ROWID clause was lost: %s" sql)
           tables;
         (* and so do the two backing __pk_ indexes' column lists *)
         Alcotest.(check int) "two PK indexes" 2 (List.length indexes);
         List.iter
           (fun sql ->
              if not (lower_contains sql "\"select\"")
              then Alcotest.failf "the PK index lost its delimiters: %s" sql)
           indexes;
         pk_is_live db "after reopen";
         expect_ok db "a different key after reopen" "INSERT INTO wr VALUES (2, 20)";
         Alcotest.(check int64)
           "seek by the reserved key column"
           10L
           (one_int db "SELECT \"order\" FROM wr WHERE \"select\" = 1"));
    (* A PRIMARY KEY's backing index is re-created by the CREATE TABLE, so
       replaying its own [sqlite_master] row on top would collide; the point
       here is the table DDL, and the index rows' delimiters are checked
       above. *)
    replay_ddl
      (List.filter (fun sql -> not (lower_contains sql "index __pk_")) before)
      (fun db ->
         seed db;
         pk_is_live db "replayed"))
;;

(* Surface 4: ALTER TABLE ... RENAME TO a reserved word.  The new name has to
   be delimited in the table's own DDL AND in every dependent index's. *)
let rename_table_to_reserved_survives_reopen () =
  with_file_db "rename_to" (fun open_it ->
    let db = open_it () in
    exec db "CREATE TABLE tr (a INTEGER, b INTEGER CHECK (a > 0))";
    exec db "CREATE INDEX tri ON tr ((a + 1))";
    exec db "ALTER TABLE tr RENAME TO \"order\"";
    expect_ok db "renamed table" "INSERT INTO \"order\" VALUES (1, 1)";
    expect_constraint_violation db "CHECK live" "INSERT INTO \"order\" VALUES (0, 1)";
    let before = all_ddl db in
    close db;
    let db = open_it () in
    Fun.protect
      ~finally:(fun () -> close db)
      (fun () ->
         Alcotest.(check (list string)) "DDL is durable" before (all_ddl db);
         (* both the table's DDL and the index's must name it delimited *)
         List.iter
           (fun sql ->
              if not (lower_contains sql "\"order\"")
              then Alcotest.failf "the renamed table lost its delimiters: %s" sql)
           before;
         expect_ok db "renamed table after reopen" "INSERT INTO \"order\" VALUES (2, 1)";
         expect_constraint_violation
           db
           "CHECK live after reopen"
           "INSERT INTO \"order\" VALUES (0, 1)";
         Alcotest.(check int64)
           "rows landed"
           2L
           (one_int db "SELECT COUNT(*) FROM \"order\""));
    replay_ddl before (fun db ->
      expect_ok db "replayed rename" "INSERT INTO \"order\" VALUES (1, 1)";
      expect_constraint_violation
        db
        "replayed CHECK live"
        "INSERT INTO \"order\" VALUES (0, 1)"))
;;

let () =
  Alcotest.run
    "rename_reserved_577"
    [ ( "rule"
      , [ Alcotest.test_case
            "rewrite_ident_in_sql delimits reserved words"
            `Quick
            rewrite_delimits_reserved_words
        ; Alcotest.test_case
            "catalog, parser and DDL renderer share one rule"
            `Quick
            one_rule_across_the_libraries
        ] )
    ; ( "CHECK"
      , [ Alcotest.test_case "issue repro" `Quick check_issue_repro
        ; Alcotest.test_case "every reserved word" `Slow check_every_keyword
        ; Alcotest.test_case "survives reopen" `Quick check_survives_reopen
        ; QCheck_alcotest.to_alcotest prop_check_rename
        ] )
    ; ( "GENERATED"
      , [ Alcotest.test_case "VIRTUAL, reopen" `Quick generated_virtual_survives_reopen
        ; Alcotest.test_case "STORED, reopen" `Quick generated_stored_survives_reopen
        ] )
    ; ( "indexes"
      , [ Alcotest.test_case "partial WHERE, reopen" `Quick partial_index_survives_reopen
        ; Alcotest.test_case
            "expression index, reopen"
            `Quick
            expression_index_survives_reopen
        ] )
    ; ( "emitted SQL"
      , [ Alcotest.test_case "sqlite_master replays" `Quick sqlite_master_ddl_replays
        ; Alcotest.test_case "Db.dump round-trips" `Quick dump_round_trips
        ] )
    ; ( "stored SQL surfaces (#618)"
      , [ Alcotest.test_case
            "FOREIGN KEY parent columns, reopen"
            `Quick
            fk_parent_cols_survive_reopen
        ; Alcotest.test_case
            "table-level UNIQUE index, reopen"
            `Quick
            table_level_unique_survives_reopen
        ; Alcotest.test_case "WITHOUT ROWID, reopen" `Quick without_rowid_survives_reopen
        ; Alcotest.test_case
            "ALTER TABLE RENAME TO a keyword, reopen"
            `Quick
            rename_table_to_reserved_survives_reopen
        ] )
    ; ( "no regressions"
      , [ Alcotest.test_case "rename away from a keyword" `Quick rename_away_from_reserved
        ; Alcotest.test_case
            "ordinary rename stays bare"
            `Quick
            ordinary_rename_stays_bare
        ] )
    ]
;;
