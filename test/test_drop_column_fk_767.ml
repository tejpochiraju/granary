(** #767: [ALTER TABLE ... DROP COLUMN] silently corrupted any FOREIGN KEY
    constraint naming the dropped column.

    {1 The defect}

    [Cat.drop_column] rebuilds [table_meta] as [{ meta with columns =
    List.filteri ... }] and carries [fk_constraints] through completely
    unchanged. [Exec.alter_drop_column] compensates for the column's INDEXES
    ([Cat.drop_index]) and its stale PRIMARY KEY flags ([Cat.clear_pk_flags]),
    but has no equivalent for FK metadata — unlike [Cat.rename_column], which
    rewrites [fk_local_cols] via [rename_col_in_fk] AND walks every other
    table's constraints via [rewrite_child_fks_tx].

    So dropping a column that participates in a FOREIGN KEY — as the child's
    own local column, or as the parent column some other table's FK references
    — succeeded and left a permanently dangling column name in
    [fk_constraints]. The issue's own repro: after [ALTER TABLE c DROP COLUMN
    pid], every [INSERT INTO c] fails ("some local columns not found"), and
    every [DELETE]/[UPDATE] of the parent key that would have to check or
    cascade fails too. There is no [ALTER TABLE ... DROP CONSTRAINT] in this
    engine, so nothing can repair it short of dropping and recreating the
    table.

    {1 The fix}

    Conservative refusal, matching this project's established answer to a
    mutation it cannot make coherent ([ALTER TABLE ... RENAME] refusing when a
    view or trigger depends on the table, #673/#645; #765 round 3's
    [Exec.fk_obligation_conflict] refusing a RENAME/DROP COLUMN with a
    deferred FK obligation pending). [Cat.fk_column_dependents] lists every
    constraint in the catalog that names the column on either side;
    [Exec.execute_alter_table] refuses the DROP when that list is non-empty,
    and [Cat.drop_column] repeats the check as defence in depth.

    The refusal is deliberately NOT gated on [PRAGMA foreign_keys]: that
    pragma decides whether a constraint is ENFORCED, not whether it is
    DECLARED, and the catalog damage is identical and permanent either way. *)

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

let exec_result db sql : (unit, string) result =
  try
    match run (Db.execute db sql) with
    | Ok () -> Ok ()
    | Error e -> Error (Format.asprintf "%a" Db.pp_error e)
  with
  | exn -> Error (Printf.sprintf "uncaught exception: %s" (Printexc.to_string exn))
;;

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let query_texts db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun (r : Db.row) ->
         Array.to_list r
         |> List.map (function
           | Db.V_int n -> Int64.to_string n
           | Db.V_real f -> Printf.sprintf "%.17g" f
           | Db.V_text s -> s
           | Db.V_blob b -> Bytes.to_string b
           | Db.V_null -> "NULL")
         |> String.concat "|")
      (run (Lwt_stream.to_list stream))
;;

(* The refusal, asserted by shape rather than by exact text: it must name
   FOREIGN KEY (so the user knows WHY) and say what to do instead. *)
let expect_drop_refused db sql =
  match exec_result db sql with
  | Ok () -> Alcotest.failf "%S was expected to be refused: it strands a FOREIGN KEY" sql
  | Error msg ->
    Alcotest.(check bool)
      (Printf.sprintf "%S refused with an FK-specific message (got %S)" sql msg)
      true
      (contains ~needle:"FOREIGN KEY" msg
       && contains ~needle:"DROP CONSTRAINT" msg
       && contains ~needle:"cannot drop column" msg)
;;

let expect_ok db sql =
  match exec_result db sql with
  | Ok () -> ()
  | Error msg -> Alcotest.failf "%S was expected to succeed, but failed: %s" sql msg
;;

(* ------------------------------------------------------------------ *)
(* The issue's own repro                                               *)
(* ------------------------------------------------------------------ *)

(* Verified in the issue (2026-09-05) as ALLOWED before this fix, after which
   [c] was permanently un-insertable-into and [p]'s key permanently
   un-updatable and un-deletable. *)
let issue_repro_drop_of_local_fk_column_is_refused () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c (pid INTEGER REFERENCES p(id) ON UPDATE CASCADE, junk INTEGER)";
    exec db "INSERT INTO p VALUES (1)";
    exec db "INSERT INTO c VALUES (1, 0)";
    expect_drop_refused db "ALTER TABLE c DROP COLUMN pid";
    (* Everything the issue reported as broken afterwards still works, because
       the drop never happened: the column is still there, the constraint is
       still evaluable, and all three DML shapes from the repro succeed. *)
    Alcotest.(check (list string))
      "the column survives the refusal"
      [ "1|0" ]
      (query_texts db "SELECT pid, junk FROM c");
    expect_ok db "INSERT INTO c (junk) VALUES (99)";
    expect_ok db "UPDATE p SET id = 2 WHERE id = 1";
    Alcotest.(check (list string))
      "ON UPDATE CASCADE still reaches the child"
      [ "2"; "NULL" ]
      (query_texts db "SELECT pid FROM c ORDER BY junk");
    expect_ok db "DELETE FROM c";
    expect_ok db "DELETE FROM p WHERE id = 2")
;;

(* ------------------------------------------------------------------ *)
(* The parent side: another table's FK references the dropped column   *)
(* ------------------------------------------------------------------ *)

(* [alter_drop_column] only ever operates on the table named in the ALTER
   statement, so before this fix the parent-side corruption was not even
   theoretically reachable by the executor — it left [fk_parent_cols] dangling
   on a table it never looked at. The gate walks the whole catalog for exactly
   this case. *)
let drop_of_parent_side_column_is_refused () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec
      db
      "CREATE TABLE pp (id INTEGER PRIMARY KEY, code TEXT, note TEXT, UNIQUE (code))";
    exec db "CREATE TABLE cc (pcode TEXT REFERENCES pp(code))";
    exec db "INSERT INTO pp VALUES (1, 'x', 'n')";
    exec db "INSERT INTO cc VALUES ('x')";
    expect_drop_refused db "ALTER TABLE pp DROP COLUMN code";
    Alcotest.(check (list string))
      "the parent column survives"
      [ "x" ]
      (query_texts db "SELECT code FROM pp"))
;;

(* The refusal message must name the OTHER table's constraint, since that is
   the object the user has to go and recreate. *)
let parent_side_refusal_names_the_referencing_table () =
  with_db (fun db ->
    exec
      db
      "CREATE TABLE pn (id INTEGER PRIMARY KEY, code TEXT, note TEXT, UNIQUE (code))";
    exec db "CREATE TABLE cn (pcode TEXT REFERENCES pn(code))";
    match exec_result db "ALTER TABLE pn DROP COLUMN code" with
    | Ok () -> Alcotest.fail "expected a refusal"
    | Error msg ->
      Alcotest.(check bool)
        (Printf.sprintf "names cn's constraint (got %S)" msg)
        true
        (contains ~needle:"FOREIGN KEY (pcode) REFERENCES pn (code) on table 'cn'" msg))
;;

(* ------------------------------------------------------------------ *)
(* Composite (multi-column) FOREIGN KEYs                               *)
(* ------------------------------------------------------------------ *)

(* Either member of a two-column FK, on either side, strands the whole
   constraint: a partially-resolvable composite FK is the shape #765 round 2
   had to teach [make_fk_recheck] to fail loudly on. *)
let drop_of_a_composite_fk_member_is_refused_on_both_sides () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE pk2 (x INTEGER, y INTEGER, PRIMARY KEY (x, y))";
    exec
      db
      "CREATE TABLE ck2 (a INTEGER, b INTEGER, junk INTEGER, FOREIGN KEY (a, b) \
       REFERENCES pk2(x, y))";
    exec db "INSERT INTO pk2 VALUES (1, 2)";
    exec db "INSERT INTO ck2 VALUES (1, 2, 0)";
    (* Local side, first and second member. *)
    expect_drop_refused db "ALTER TABLE ck2 DROP COLUMN a";
    expect_drop_refused db "ALTER TABLE ck2 DROP COLUMN b";
    (* Parent side, first and second member. *)
    expect_drop_refused db "ALTER TABLE pk2 DROP COLUMN x";
    expect_drop_refused db "ALTER TABLE pk2 DROP COLUMN y";
    Alcotest.(check (list string))
      "nothing was dropped"
      [ "1|2|0" ]
      (query_texts db "SELECT a, b, junk FROM ck2"))
;;

(* ------------------------------------------------------------------ *)
(* Self-referential FOREIGN KEYs                                       *)
(* ------------------------------------------------------------------ *)

(* One table, one constraint, both sides on it: the catalog walk must find the
   table's own record for the PARENT side too, which is the case
   [Cat.child_tables_of] deliberately excludes. *)
let self_referential_fk_is_refused_on_both_sides () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec
      db
      "CREATE TABLE e (id INTEGER PRIMARY KEY, mgr INTEGER REFERENCES e(id), nm TEXT)";
    exec db "INSERT INTO e VALUES (1, NULL, 'root')";
    exec db "INSERT INTO e VALUES (2, 1, 'leaf')";
    expect_drop_refused db "ALTER TABLE e DROP COLUMN mgr";
    expect_drop_refused db "ALTER TABLE e DROP COLUMN id";
    (* The column that participates in nothing still drops. *)
    expect_ok db "ALTER TABLE e DROP COLUMN nm";
    Alcotest.(check (list string))
      "rows survive the successful drop, FK intact"
      [ "1|NULL"; "2|1" ]
      (query_texts db "SELECT id, mgr FROM e ORDER BY id"))
;;

(* ------------------------------------------------------------------ *)
(* No over-refusal                                                     *)
(* ------------------------------------------------------------------ *)

(* The negative test the fix needs most: a column no constraint names still
   drops, and the constraint that shares the table keeps working afterwards. *)
let drop_of_an_unrelated_column_still_works () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE pu (id INTEGER PRIMARY KEY)";
    exec db "CREATE TABLE cu (pid INTEGER REFERENCES pu(id), junk INTEGER, more TEXT)";
    exec db "INSERT INTO pu VALUES (1)";
    exec db "INSERT INTO cu VALUES (1, 7, 'keep')";
    expect_ok db "ALTER TABLE cu DROP COLUMN junk";
    Alcotest.(check (list string))
      "the row is reshaped, not lost"
      [ "1|keep" ]
      (query_texts db "SELECT pid, more FROM cu");
    (* The FK is still declared AND still enforced after the drop. *)
    (match exec_result db "INSERT INTO cu VALUES (99, 'orphan')" with
     | Ok () -> Alcotest.fail "the surviving FK stopped enforcing after DROP COLUMN"
     | Error msg ->
       Alcotest.(check bool)
         (Printf.sprintf "still a FOREIGN KEY violation (got %S)" msg)
         true
         (contains ~needle:"FOREIGN KEY" msg));
    expect_ok db "INSERT INTO cu VALUES (1, 'ok')")
;;

(* Name-blindness would be a false-refusal machine: [column_dependents_tx]'s
   lexical scan cannot tell one table's [pid] from another's, but this gate
   reads structured FK metadata and can. *)
let unrelated_table_with_the_same_column_name_still_drops () =
  with_db (fun db ->
    exec db "CREATE TABLE ps (id INTEGER PRIMARY KEY)";
    exec db "CREATE TABLE cs (pid INTEGER REFERENCES ps(id), filler INTEGER)";
    (* [unrel.pid] shares only the NAME with [cs.pid]. *)
    exec db "CREATE TABLE unrel (pid INTEGER, keep INTEGER)";
    exec db "INSERT INTO unrel VALUES (5, 6)";
    expect_ok db "ALTER TABLE unrel DROP COLUMN pid";
    Alcotest.(check (list string))
      "unrel lost only its own column"
      [ "6" ]
      (query_texts db "SELECT keep FROM unrel");
    (* And cs.pid is still refused, so the walk did not merely go blind. *)
    expect_drop_refused db "ALTER TABLE cs DROP COLUMN pid")
;;

(* ------------------------------------------------------------------ *)
(* Independence from PRAGMA foreign_keys                               *)
(* ------------------------------------------------------------------ *)

(* The pragma governs ENFORCEMENT; the corruption is to the DECLARATION, and is
   permanent either way — a database dropped-through with the pragma off would
   still be broken when it is turned back on. *)
let refusal_does_not_depend_on_pragma_foreign_keys () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 0";
    exec db "CREATE TABLE pf (id INTEGER PRIMARY KEY, code TEXT, UNIQUE (code))";
    exec db "CREATE TABLE cf (pid INTEGER REFERENCES pf(id), junk INTEGER)";
    (* Enforcement really is off: the orphan insert is accepted. *)
    expect_ok db "INSERT INTO cf VALUES (404, 0)";
    expect_drop_refused db "ALTER TABLE cf DROP COLUMN pid";
    expect_drop_refused db "ALTER TABLE pf DROP COLUMN id";
    (* Turning it back on finds a table whose constraint is still evaluable. *)
    exec db "PRAGMA foreign_keys = 1";
    exec db "INSERT INTO pf VALUES (1, 'x')";
    expect_ok db "INSERT INTO cf VALUES (1, 1)")
;;

(* ------------------------------------------------------------------ *)
(* Inside an explicit transaction                                      *)
(* ------------------------------------------------------------------ *)

(* Refused the same way inside [BEGIN], with nothing applied: the gate runs
   before [alter_drop_column] migrates a single row, so ROLLBACK finds the
   schema exactly as it was. (Like every other DDL refusal in this engine, the
   raise happens inside [with_ddl_txn] and so poisons the ambient transaction
   per #286 — ROLLBACK, not COMMIT, is the exit; see docs/DECISIONS.md.) *)
let drop_is_refused_inside_an_explicit_transaction () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE pt (id INTEGER PRIMARY KEY)";
    exec db "CREATE TABLE ct (pid INTEGER REFERENCES pt(id), junk INTEGER)";
    exec db "INSERT INTO pt VALUES (1)";
    exec db "BEGIN";
    expect_drop_refused db "ALTER TABLE ct DROP COLUMN pid";
    exec db "ROLLBACK";
    Alcotest.(check (list string))
      "the schema is untouched after the refusal"
      []
      (query_texts db "SELECT pid, junk FROM ct");
    expect_ok db "INSERT INTO ct VALUES (1, 0)")
;;

(* ------------------------------------------------------------------ *)
(* An FK added by ALTER TABLE ... ADD COLUMN is covered too            *)
(* ------------------------------------------------------------------ *)

(* [alter_add_column] appends an inline REFERENCES to [fk_constraints] at the
   list's end (see [Exec.fk_ordinal]'s note). The gate reads the live catalog,
   so a constraint that was not present at CREATE TABLE time is found the
   same way. *)
let fk_added_by_add_column_is_also_protected () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE pa (id INTEGER PRIMARY KEY, filler INTEGER)";
    exec db "CREATE TABLE ca (junk INTEGER)";
    exec db "ALTER TABLE ca ADD COLUMN pid INTEGER REFERENCES pa(id)";
    expect_drop_refused db "ALTER TABLE ca DROP COLUMN pid";
    expect_drop_refused db "ALTER TABLE pa DROP COLUMN id";
    expect_ok db "ALTER TABLE ca DROP COLUMN junk")
;;

let () =
  Alcotest.run
    "drop_column_fk_767"
    [ ( "issue_repro"
      , [ Alcotest.test_case
            "DROP COLUMN of a local FK column is refused"
            `Quick
            issue_repro_drop_of_local_fk_column_is_refused
        ] )
    ; ( "parent_side"
      , [ Alcotest.test_case
            "DROP COLUMN of a referenced parent column is refused"
            `Quick
            drop_of_parent_side_column_is_refused
        ; Alcotest.test_case
            "the refusal names the referencing table's constraint"
            `Quick
            parent_side_refusal_names_the_referencing_table
        ] )
    ; ( "composite"
      , [ Alcotest.test_case
            "either member of a composite FK is refused on either side"
            `Quick
            drop_of_a_composite_fk_member_is_refused_on_both_sides
        ] )
    ; ( "self_reference"
      , [ Alcotest.test_case
            "a self-referential FK is refused on both of its sides"
            `Quick
            self_referential_fk_is_refused_on_both_sides
        ] )
    ; ( "no_over_refusal"
      , [ Alcotest.test_case
            "an unrelated column still drops, and the FK still enforces"
            `Quick
            drop_of_an_unrelated_column_still_works
        ; Alcotest.test_case
            "a same-named column on an unrelated table still drops"
            `Quick
            unrelated_table_with_the_same_column_name_still_drops
        ] )
    ; ( "pragma_independence"
      , [ Alcotest.test_case
            "refused with PRAGMA foreign_keys = 0 as well"
            `Quick
            refusal_does_not_depend_on_pragma_foreign_keys
        ] )
    ; ( "explicit_transaction"
      , [ Alcotest.test_case
            "refused inside BEGIN with nothing applied"
            `Quick
            drop_is_refused_inside_an_explicit_transaction
        ] )
    ; ( "add_column_fk"
      , [ Alcotest.test_case
            "an FK added by ADD COLUMN is protected too"
            `Quick
            fk_added_by_add_column_is_also_protected
        ] )
    ]
;;
