(** #653: [INSERT ... SELECT ... ON CONFLICT (cols) DO UPDATE SET ...].

    Before this, the clause had no grammar at all: [Ast.S_insert_select] carried
    no [upsert_update], [Plan.Op_insert_select] carried none either, and
    [parser.mly] attached [opt_upsert] only to the two VALUES arms. The statement
    was a parse error, which was the {i right} failure mode — a clause that
    parsed and was then dropped is #639 in a new place — but it left the SELECT
    form of INSERT without an upsert at all.

    {b The shape of the fix is the point.} [execute_insert_select_op] already
    drove the source stream one row at a time through the {i same}
    [execute_insert] the VALUES form uses; the only thing missing below the
    grammar was that it did not take [~upsert_update] and so could not pass one.
    So this is a threading change through five layers (grammar → AST → Sema →
    Plan → the one Exec call site), not a second implementation of upsert. That
    matters: #639's whole subject is what happens when the conflict-resolution
    modifier and the explicit target are resolved by code that has drifted, and a
    parallel implementation for the SELECT form would be a standing invitation to
    exactly that. Everything the VALUES form earned therefore applies here by
    construction rather than by two implementations agreeing —
    - #639's target-resolution pass, run before the modifier is consulted
      ([or_ignore_defers_to_the_explicit_target],
      [or_replace_defers_to_the_explicit_target]);
    - the rowid-alias pre-probe, since the alias PK carries no index
      ([alias_pk_target_upserts]);
    - #599/#639's NOT NULL ordering, which an ON CONFLICT clause never intercepts
      ([not_null_in_the_source_row_follows_the_modifier],
      [a_null_assigned_by_the_do_update_is_refused]);
    - #667's pre-write uniqueness probe over the {i other} unique indexes
      ([a_do_update_duplicating_another_unique_index_raises]);
    - and [last_insert_rowid()] not moving for a row that was updated, not
      inserted ([a_do_update_does_not_move_last_insert_rowid]).

    Binding goes through one shared [Sema.bind_upsert_clause] for the same
    reason, which is where the two spellings' conflict-target validation and DO
    UPDATE assignment rules now live once instead of twice.

    {b Oracle.} Every answer below marked "oracle" was checked against sqlite3
    and matches it: the plain conflict, [excluded.<col>], [DO UPDATE] under both
    [OR IGNORE] and [OR REPLACE], a source whose own rows conflict with each
    other (last one wins), and [last_insert_rowid()].

    {b Two deliberate divergences from sqlite3, both recorded rather than
    accidental:}

    - {b No [WHERE] is needed between the SELECT and the [ON CONFLICT].} SQLite
      cannot parse [INSERT INTO t SELECT k, v FROM src ON CONFLICT(k) DO UPDATE
      ...] — its manual tells you to write [WHERE true] to separate the upsert's
      [ON] from a join's. Granary's [join_clause] requires the [JOIN] keyword
      before its [ON], so once the join list is complete no [ON] can be shifted
      into it and the trailing one can only begin the upsert. Adding [opt_upsert]
      to the three SELECT arms left menhir's conflict counts unchanged (35 states
      / 290 conflicts). [no_where_is_needed_before_on_conflict] pins the
      permissive spelling {i and} the join spelling together, because the second
      is what makes the first safe rather than lucky.
    - {b An [ON CONFLICT ... DO UPDATE] on a COLUMNSTORE table is now refused,}
      in {i both} spellings — this closes a pre-existing hole rather than opening
      one. The columnar write arms hand the row straight to
      [Col_store.insert_rows] and probe for no conflict at all, so on [main] the
      VALUES form accepted the clause, ignored it, and inserted a duplicate key:
      #639's exact failure mode, reached through the storage engine instead of
      through a modifier. [columnstore_refuses_the_clause_in_both_spellings].

    Out of scope and unchanged, both true of the VALUES form too, so neither is
    a SELECT-form regression: [DO NOTHING] and [DO UPDATE ... WHERE ...] have no
    grammar in this engine, and a nested [excluded.<col>] (one buried inside a
    larger expression, e.g. [excluded.v + 1]) binds to the {i target} row's
    column because [Sema.bind_expr] drops the qualifier — a silent wrong answer
    that predates this work and is filed as #741. Only the top-level form is
    exercised here. *)

open Lwt.Syntax
module Db = Granary.Db
module Row = Granary_encoding.Row

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

let expect_rows db ~msg want sql = Alcotest.(check (list string)) msg want (texts db sql)

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

(* [Db.execute_change_count] is the affected-row count the statement reports —
   for an upsert that is inserts + DO UPDATEs, which is what sqlite3's
   [changes()] reports too. *)
let change_count db sql =
  match run (Db.execute_change_count db sql) with
  | Ok n -> n
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let expect_error db ~needle sql =
  match run (Db.execute_change_count db sql) with
  | Ok n -> Alcotest.failf "%S was expected to fail with %S, wrote %d rows" sql needle n
  | Error e ->
    let msg = Format.asprintf "%a" Db.pp_error e in
    Alcotest.(check bool)
      (Printf.sprintf "%S failed with %S (got %S)" sql needle msg)
      true
      (contains ~needle msg)
;;

(* The two conflict shapes #639 is written around, because they are resolved by
   two different functions: [t] conflicts on the rowid-alias PRIMARY KEY (no
   index — it needs [execute_insert]'s explicit [S.get] pre-probe), [s] on a
   secondary UNIQUE index (resolved by [check_insert_unique]'s target pass). *)
let seed_alias db =
  exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER)";
  exec db "INSERT INTO t VALUES (1, 5)";
  exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
  exec db "INSERT INTO src VALUES (1, 10), (2, 20)"
;;

let seed_secondary db =
  exec db "CREATE TABLE s (id INTEGER PRIMARY KEY, k INTEGER, v INTEGER)";
  exec db "CREATE UNIQUE INDEX s_k ON s (k)";
  exec db "INSERT INTO s VALUES (1, 100, 5)";
  exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
  exec db "INSERT INTO src VALUES (100, 10), (200, 20)"
;;

(* ------------------------------------------------------------------ *)
(* The feature itself                                                  *)
(* ------------------------------------------------------------------ *)

(* The issue's own statement. On main this was a parse error; the answer below
   is sqlite3's, oracle-checked: the conflicting source row runs the DO UPDATE,
   the non-conflicting one inserts, and both count towards changes(). *)
let alias_pk_target_upserts () =
  with_db (fun db ->
    seed_alias db;
    let n =
      change_count
        db
        "INSERT INTO t SELECT k, v FROM src WHERE 1 ON CONFLICT(k) DO UPDATE SET v = 99"
    in
    Alcotest.(check int) "one upsert + one insert = 2 rows affected (oracle)" 2 n;
    expect_rows
      db
      ~msg:"row 1 updated in place, row 2 inserted (oracle)"
      [ "1|99"; "2|20" ]
      "SELECT k, v FROM t ORDER BY k")
;;

(* The same statement against a secondary UNIQUE index, which reaches the target
   through [check_insert_unique]'s own pass rather than the alias pre-probe. The
   two must not diverge — a split answer by conflict shape is the defect #639
   spends two pages on. *)
let secondary_index_target_upserts () =
  with_db (fun db ->
    seed_secondary db;
    let n =
      change_count
        db
        "INSERT INTO s (k, v) SELECT k, v FROM src WHERE 1 ON CONFLICT(k) DO UPDATE SET \
         v = 99"
    in
    Alcotest.(check int) "one upsert + one insert" 2 n;
    (* The inserted row gets id 3, not 2: [execute_insert] allocates a rowid
       before it resolves the conflict, and the allocation the DO UPDATE branch
       then discards is not given back. sqlite3 answers 2. This is a
       {b pre-existing divergence, identical in the VALUES form} — the same
       statement written [VALUES (100,10),(200,20) ON CONFLICT(k) DO UPDATE]
       also lands id 3 on main — so it is a property of the shared write path,
       not something the SELECT spelling introduced. Asserted as-is precisely
       because agreeing with the VALUES form is what #653 is for; filed as
       #742. *)
    expect_rows
      db
      ~msg:"the conflicting row was updated, not duplicated"
      [ "1|100|99"; "3|200|20" ]
      "SELECT id, k, v FROM s ORDER BY id")
;;

(* [excluded.<col>] names the row the SELECT proposed, not the row already
   stored. Oracle-checked. Only the top-level spelling is exercised — see the
   header on the nested one. *)
let excluded_names_the_source_row () =
  with_db (fun db ->
    seed_alias db;
    exec
      db
      "INSERT INTO t SELECT k, v FROM src WHERE 1 ON CONFLICT(k) DO UPDATE SET v = \
       excluded.v";
    expect_rows
      db
      ~msg:"v took the source row's 10, not the stored 5 (oracle)"
      [ "1|10"; "2|20" ]
      "SELECT k, v FROM t ORDER BY k")
;;

(* A source whose own rows collide. The per-row loop means each row sees the
   effect of the previous one, so the last write wins — which is exactly what
   sqlite3 answers (oracle: a single row, 1|30). A set-at-a-time implementation
   would have had to decide this separately; reusing [execute_insert] means it
   falls out. *)
let source_rows_conflicting_with_each_other () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO src VALUES (1, 10), (1, 20), (1, 30)";
    let n =
      change_count
        db
        "INSERT INTO t SELECT k, v FROM src WHERE 1 ON CONFLICT(k) DO UPDATE SET v = \
         excluded.v"
    in
    Alcotest.(check int) "one insert + two upserts" 3 n;
    expect_rows
      db
      ~msg:"one row, last source row wins (oracle)"
      [ "1|30" ]
      "SELECT k, v FROM t ORDER BY k")
;;

(* All three SELECT-shaped grammar arms carry the clause, not just the bare one:
   the explicit column list, and [INSERT INTO t WITH ... SELECT ...]. A clause
   accepted on one arm and dropped on another is the failure mode this issue
   exists to avoid. *)
let every_select_arm_carries_the_clause () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 5)";
    exec db "CREATE TABLE src (a INTEGER, b INTEGER)";
    exec db "INSERT INTO src VALUES (1, 60)";
    (* explicit column list *)
    exec
      db
      "INSERT INTO t (k, v) SELECT a, b FROM src WHERE 1 ON CONFLICT(k) DO UPDATE SET v \
       = 88";
    expect_rows db ~msg:"column-list arm upserted" [ "1|88" ] "SELECT k, v FROM t";
    (* WITH ... SELECT *)
    exec
      db
      "INSERT INTO t WITH c AS (SELECT 1 AS k, 10 AS v) SELECT k, v FROM c WHERE 1 ON \
       CONFLICT(k) DO UPDATE SET v = 77";
    expect_rows db ~msg:"with-CTE arm upserted" [ "1|77" ] "SELECT k, v FROM t")
;;

(* ------------------------------------------------------------------ *)
(* #639's rule, now for the SELECT form                                *)
(* ------------------------------------------------------------------ *)

(* An explicit target beats the modifier for the index it names. On main the
   VALUES spelling of this skipped silently; the SELECT spelling could not be
   written at all. Oracle-checked: sqlite3 runs the DO UPDATE. *)
let or_ignore_defers_to_the_explicit_target () =
  with_db (fun db ->
    seed_alias db;
    exec
      db
      "INSERT OR IGNORE INTO t SELECT k, v FROM src WHERE 1 ON CONFLICT(k) DO UPDATE SET \
       v = 99";
    expect_rows
      db
      ~msg:"OR IGNORE did not eat the DO UPDATE (oracle)"
      [ "1|99"; "2|20" ]
      "SELECT k, v FROM t ORDER BY k")
;;

(* The same for [OR REPLACE], which #639 decided deliberately and recorded as
   believed-but-not-oracle-checked. It is oracle-checked now, on this spelling:
   sqlite3 also updates the conflicting row in place (id 1 survives) rather than
   deleting it and inserting a new one (which would allocate a new id). *)
let or_replace_defers_to_the_explicit_target () =
  with_db (fun db ->
    seed_secondary db;
    exec
      db
      "INSERT OR REPLACE INTO s (k, v) SELECT k, v FROM src WHERE k = 100 ON CONFLICT(k) \
       DO UPDATE SET v = 42";
    expect_rows
      db
      ~msg:"the row was updated in place, keeping id 1 (oracle)"
      [ "1|100|42" ]
      "SELECT id, k, v FROM s ORDER BY id")
;;

(* [last_insert_rowid()] is not moved by a DO UPDATE — no row was inserted.
   Seeded at 7 and upserted at 1 so the assertion cannot pass vacuously, which
   is the trap #639 found in the VALUES form's own test. Oracle-checked. *)
let a_do_update_does_not_move_last_insert_rowid () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (7, 1)";
    exec db "INSERT INTO t VALUES (1, 5)";
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO src VALUES (1, 10)";
    expect_rows db ~msg:"seeded at 1" [ "1" ] "SELECT last_insert_rowid()";
    exec
      db
      "INSERT INTO t SELECT k, v FROM src WHERE 1 ON CONFLICT(k) DO UPDATE SET v = 99";
    expect_rows
      db
      ~msg:"a DO UPDATE inserted nothing, so it moved nothing (oracle)"
      [ "1" ]
      "SELECT last_insert_rowid()";
    expect_rows
      db
      ~msg:"but it did update"
      [ "1|99"; "7|1" ]
      "SELECT k, v FROM t ORDER BY k")
;;

(* #667: a DO UPDATE that writes a duplicate into a DIFFERENT unique index
   raises. [execute_upsert_update] runs [check_indexes_unique_on_update] before
   [write_row_rekeyed]; the SELECT form reaches it through the same call. *)
let a_do_update_duplicating_another_unique_index_raises () =
  with_db (fun db ->
    exec db "CREATE TABLE s (id INTEGER PRIMARY KEY, k INTEGER, u INTEGER)";
    exec db "CREATE UNIQUE INDEX s_k ON s (k)";
    exec db "CREATE UNIQUE INDEX s_u ON s (u)";
    exec db "INSERT INTO s VALUES (1, 100, 500), (2, 200, 600)";
    exec db "CREATE TABLE src (k INTEGER, u INTEGER)";
    exec db "INSERT INTO src VALUES (100, 999)";
    expect_error
      db
      ~needle:"UNIQUE constraint failed"
      "INSERT INTO s (k, u) SELECT k, u FROM src WHERE 1 ON CONFLICT(k) DO UPDATE SET u \
       = 600";
    expect_rows
      db
      ~msg:"nothing was written"
      [ "1|100|500"; "2|200|600" ]
      "SELECT id, k, u FROM s ORDER BY id")
;;

(* ------------------------------------------------------------------ *)
(* NOT NULL: the modifier governs it, the ON CONFLICT clause never does *)
(* ------------------------------------------------------------------ *)

(* #599/#639: a NULL in the row being INSERTED is decided by the modifier, and
   the ON CONFLICT clause does not intercept it. Under [OR IGNORE] the NULL row
   is skipped while the conflicting row still runs its DO UPDATE and the clean
   row still lands; under a raising modifier it raises. Both halves matter — the
   first is what makes the clause and the modifier compose, the second is what
   keeps the clause from being a way to smuggle a NULL past NOT NULL. *)
let not_null_in_the_source_row_follows_the_modifier () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER NOT NULL)";
    exec db "INSERT INTO t VALUES (1, 5)";
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO src VALUES (1, 10), (2, NULL), (3, 30)";
    let n =
      change_count
        db
        "INSERT OR IGNORE INTO t SELECT k, v FROM src WHERE 1 ON CONFLICT(k) DO UPDATE \
         SET v = 99"
    in
    Alcotest.(check int) "the NULL row skipped; the other two wrote" 2 n;
    expect_rows
      db
      ~msg:"upsert ran, NULL skipped, clean row landed"
      [ "1|99"; "3|30" ]
      "SELECT k, v FROM t ORDER BY k");
  (* The raising half, on a fresh database. *)
  with_db (fun db ->
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER NOT NULL)";
    exec db "INSERT INTO t VALUES (1, 5)";
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO src VALUES (2, NULL)";
    expect_error
      db
      ~needle:"NOT NULL constraint failed"
      "INSERT INTO t SELECT k, v FROM src WHERE 1 ON CONFLICT(k) DO UPDATE SET v = 99")
;;

(* The other direction: a NULL assigned BY the DO UPDATE is refused whatever the
   modifier says, because that write funnels through [write_row_rekeyed] →
   [enforce_not_null], which #599 requires to stay unconditional. Caught here by
   [Sema.bind_upsert_assignments]'s static literal-NULL check, which #639 keeps
   armed (unlike [bind_insert_row]'s) precisely because the runtime answer below
   it is "raise" — so the two levels agree instead of disagreeing. *)
let a_null_assigned_by_the_do_update_is_refused () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER NOT NULL)";
    exec db "INSERT INTO t VALUES (1, 5)";
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO src VALUES (1, 10)";
    expect_error
      db
      ~needle:"NOT NULL"
      "INSERT OR IGNORE INTO t SELECT k, v FROM src WHERE 1 ON CONFLICT(k) DO UPDATE SET \
       v = NULL";
    expect_rows db ~msg:"the stored row is untouched" [ "1|5" ] "SELECT k, v FROM t")
;;

(* ------------------------------------------------------------------ *)
(* Refusals, shared with the VALUES form by construction               *)
(* ------------------------------------------------------------------ *)

(* Both spellings bind through one [Sema.bind_upsert_clause], so a target naming
   no PRIMARY KEY or UNIQUE constraint, and a generated column on the left of a
   DO UPDATE assignment (#629), are refused identically. Asserting them on the
   SELECT form is what proves the shared helper is actually reached. *)
let shared_binder_refusals () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k INTEGER, v INTEGER)";
    exec db "CREATE INDEX t_k ON t (k)";
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO src VALUES (1, 10)";
    expect_error
      db
      ~needle:"does not match any PRIMARY KEY or UNIQUE constraint"
      "INSERT INTO t SELECT k, v FROM src WHERE 1 ON CONFLICT(k) DO UPDATE SET v = 1");
  with_db (fun db ->
    exec
      db
      "CREATE TABLE g (k INTEGER PRIMARY KEY, v INTEGER, d INTEGER GENERATED ALWAYS AS \
       (v * 2))";
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO src VALUES (1, 10)";
    expect_error
      db
      ~needle:"cannot UPDATE generated column"
      "INSERT INTO g SELECT k, v FROM src WHERE 1 ON CONFLICT(k) DO UPDATE SET d = 5")
;;

(* A COLUMNSTORE table's write arms hand the row straight to
   [Col_store.insert_rows] and probe for no conflict, so the clause could only
   ever be parsed and dropped. Refusing it closes a hole that was already open in
   the VALUES form on main, where this statement silently inserted a second row
   with the same key. Both spellings, because a refusal on one and a silent drop
   on the other would be the same defect wearing a different hat. *)
let columnstore_refuses_the_clause_in_both_spellings () =
  with_db (fun db ->
    exec db "CREATE TABLE ct (k INTEGER PRIMARY KEY, v INTEGER) USING COLUMNSTORE";
    exec db "INSERT INTO ct VALUES (1, 5)";
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO src VALUES (1, 9)";
    expect_error
      db
      ~needle:"COLUMNSTORE"
      "INSERT INTO ct VALUES (1, 9) ON CONFLICT(k) DO UPDATE SET v = 99";
    expect_error
      db
      ~needle:"COLUMNSTORE"
      "INSERT INTO ct SELECT k, v FROM src WHERE 1 ON CONFLICT(k) DO UPDATE SET v = 99";
    expect_rows db ~msg:"neither refusal wrote anything" [ "1|5" ] "SELECT k, v FROM ct")
;;

(* ------------------------------------------------------------------ *)
(* Grammar                                                             *)
(* ------------------------------------------------------------------ *)

(* The divergence from sqlite3, and the reason it is safe. sqlite3 cannot parse
   the first statement below (its manual prescribes a [WHERE true] to separate
   the upsert's [ON] from a join's); granary can, because [join_clause] requires
   [JOIN] before its [ON] and so cannot shift a trailing [ON] once the join list
   is complete. The second statement is the control: a real join with a real
   [ON], followed by the upsert's [ON], must still bind the way the writer meant.
   If either half ever regresses, the other tells you which way. *)
let no_where_is_needed_before_on_conflict () =
  with_db (fun db ->
    seed_alias db;
    exec db "INSERT INTO t SELECT k, v FROM src ON CONFLICT(k) DO UPDATE SET v = 99";
    expect_rows
      db
      ~msg:"no WHERE needed between the SELECT and the ON CONFLICT"
      [ "1|99"; "2|20" ]
      "SELECT k, v FROM t ORDER BY k");
  with_db (fun db ->
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 5)";
    exec db "CREATE TABLE a (k INTEGER, v INTEGER)";
    exec db "CREATE TABLE b (k INTEGER, w INTEGER)";
    exec db "INSERT INTO a VALUES (1, 10), (2, 20)";
    exec db "INSERT INTO b VALUES (1, 111), (2, 222)";
    exec
      db
      "INSERT INTO t SELECT a.k, b.w FROM a JOIN b ON a.k = b.k ON CONFLICT(k) DO UPDATE \
       SET v = 777";
    expect_rows
      db
      ~msg:"the join's ON and the upsert's ON both bound correctly"
      [ "1|777"; "2|222" ]
      "SELECT k, v FROM t ORDER BY k")
;;

(* Without the clause the SELECT form behaves exactly as it did before — the
   modifier alone governs, and the conflicting row is skipped rather than
   updated. This is the row #653 must not have disturbed. *)
let the_clauseless_form_is_unchanged () =
  with_db (fun db ->
    seed_alias db;
    let n = change_count db "INSERT OR IGNORE INTO t SELECT k, v FROM src" in
    Alcotest.(check int) "only the non-conflicting row wrote" 1 n;
    expect_rows
      db
      ~msg:"the conflict was skipped, not upserted"
      [ "1|5"; "2|20" ]
      "SELECT k, v FROM t ORDER BY k")
;;

let () =
  Alcotest.run
    "insert-select-upsert-653"
    [ ( "653-the-feature"
      , [ Alcotest.test_case
            "rowid-alias PK target upserts"
            `Quick
            alias_pk_target_upserts
        ; Alcotest.test_case
            "secondary UNIQUE index target upserts"
            `Quick
            secondary_index_target_upserts
        ; Alcotest.test_case
            "excluded.<col> names the source row"
            `Quick
            excluded_names_the_source_row
        ; Alcotest.test_case
            "source rows conflicting with each other: last wins"
            `Quick
            source_rows_conflicting_with_each_other
        ; Alcotest.test_case
            "every SELECT-shaped grammar arm carries the clause"
            `Quick
            every_select_arm_carries_the_clause
        ] )
    ; ( "653-639s-rule"
      , [ Alcotest.test_case
            "OR IGNORE defers to the explicit target"
            `Quick
            or_ignore_defers_to_the_explicit_target
        ; Alcotest.test_case
            "OR REPLACE defers to the explicit target"
            `Quick
            or_replace_defers_to_the_explicit_target
        ; Alcotest.test_case
            "a DO UPDATE does not move last_insert_rowid()"
            `Quick
            a_do_update_does_not_move_last_insert_rowid
        ; Alcotest.test_case
            "667: a DO UPDATE duplicating another unique index raises"
            `Quick
            a_do_update_duplicating_another_unique_index_raises
        ] )
    ; ( "653-not-null"
      , [ Alcotest.test_case
            "a NULL in the source row follows the modifier"
            `Quick
            not_null_in_the_source_row_follows_the_modifier
        ; Alcotest.test_case
            "a NULL assigned by the DO UPDATE is refused"
            `Quick
            a_null_assigned_by_the_do_update_is_refused
        ] )
    ; ( "653-refusals"
      , [ Alcotest.test_case
            "the shared binder's refusals reach the SELECT form"
            `Quick
            shared_binder_refusals
        ; Alcotest.test_case
            "COLUMNSTORE refuses the clause in both spellings"
            `Quick
            columnstore_refuses_the_clause_in_both_spellings
        ] )
    ; ( "653-grammar"
      , [ Alcotest.test_case
            "no WHERE is needed before ON CONFLICT"
            `Quick
            no_where_is_needed_before_on_conflict
        ; Alcotest.test_case
            "the clauseless SELECT form is unchanged"
            `Quick
            the_clauseless_form_is_unchanged
        ] )
    ]
;;
