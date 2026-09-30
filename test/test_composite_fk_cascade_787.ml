(** #787 / #790: a COMPOSITE foreign key's [ON DELETE]/[ON UPDATE]
    [SET NULL]/[SET DEFAULT] used to write only the FK's FIRST local column.

    [Exec.cascade_apply_set_null], [Exec.cascade_apply_set_default],
    [Exec.cascade_delete_set_null] and [Exec.cascade_delete_set_default]
    looped columns-OUTER, rows-INNER, handing each (column, row) pair to
    [Exec.cascade_update_col_in_tx], which short-circuits on [(table, rowid)]
    in a [visited] hashtable it shares across the whole loop: the first
    column's pass inserted the rowid, so the second column's pass for the
    same row returned immediately without writing anything. For a two-column
    FK, [SET NULL] nulled the first local column and silently left the
    second holding its old (now-dangling-reference) value.

    The fix inverts every one of those four loops to rows-OUTER,
    columns-INNER: for each row, every local column the FK names is written
    together in ONE call to the new [Exec.cascade_update_cols_in_tx] (the
    multi-column generalization of [Exec.cascade_update_col_in_tx]), which
    builds ONE post-image from ONE pre-image via the new
    [Exec.cascade_updated_row_multi] and makes ONE
    [Exec.update_cols_in_tx] write — recomputing STORED generated columns,
    probing UNIQUE indexes, and re-keying an alias PK exactly once against
    the fully-updated row, the same shape [Exec.apply_update_row] already
    uses for an ordinary multi-column [UPDATE].

    A NOT NULL (or NOT NULL-with-no-DEFAULT) rejection on any one of the
    target columns must still be caught BEFORE any row is touched, for the
    whole composite write — not merely before that one column's own write,
    which is all the pre-fix code actually guaranteed once more than one
    column was involved.

    {1 Round 2 (PR #793 review): the same defect one level down, and one
    level up}

    The fix above did not close every instance of the bug family. Two more
    call sites dispatched a composite FK's ON UPDATE action ONCE PER CHANGED
    COLUMN rather than once per row, each write landing on the SAME shared
    [visited] short-circuit and silently dropping every dispatch after the
    first for that row:

    - The re-cascade inside [Exec.cascade_update_cols_in_tx] (walked when a
      cascade write to a parent-of-a-parent changes 2+ columns a CHILD's
      composite FK matches) called the old single-column [Exec.cascade_update_fk]
      once per changed column. Reproduced with a 3-table chain: [gp(a,b)] PK,
      [p(id,x,y)] with [FOREIGN KEY (x,y) REFERENCES gp(a,b) ON DELETE SET NULL],
      [c(id,cx,cy)] with [FOREIGN KEY (cx,cy) REFERENCES p(x,y) ON UPDATE ...].
      [DELETE FROM gp] correctly NULLs both [p.x] and [p.y] together (round 1's
      fix), but the downstream re-cascade to [c] used to write only [c.cx] or
      only [c.cy], not both.
    - [Exec.apply_update_cascade_fk] — the TOP-LEVEL entry point for a direct
      [UPDATE parent SET a = .., b = ..] reaching an [ON UPDATE CASCADE] (not
      reached by round 1's DELETE-triggered repro at all) — had a literal
      "single-col FK compat" comment: it took [List.hd] of the resolved local
      columns and new values, discarding every column past the first even
      though it had already resolved the full list.

    Fixed by [Exec.cascade_update_fk_multi], which the re-cascade caller now
    dispatches ONCE per (child table, fk) pair with every matching changed
    column bundled together, and by making [apply_update_cascade_fk]'s
    CASCADE branch use its already-fully-resolved column/value lists instead
    of their heads. Both now route through [Exec.cascade_update_cols_in_tx]
    for CASCADE, and through the existing [Exec.cascade_apply_set_null]/
    [Exec.cascade_apply_set_default] (the same all-local-columns,
    one-write-per-row functions the ON DELETE side already used) for SET
    NULL/SET DEFAULT, rather than a third reimplementation.

    {1 Round 3 (PR #793 review): one dispatch per FK still isn't one write
    per row}

    Round 2's own fix grouped a SINGLE composite FK's changed columns into
    one dispatch, but two SEPARATE fk constraints from the same child table
    -- each individually well-behaved after round 2 -- still each ran their
    own independent {!Exec.cascade_update_cols_in_tx} call against the SAME
    shared [visited] set when they resolved to the same child row: the first
    fk's call marked the row visited and wrote its own column, and the
    second fk's call was silently skipped by the guard, losing its column.
    Reproduced exactly as the review gave it: [p(id)] PK, [c(id, ref1, ref2)]
    with TWO independent single-column FKs (not one composite FK) both
    referencing [p(id)] [ON UPDATE CASCADE]; [UPDATE p SET id = 2] wrote
    [ref1] but left [ref2] dangling at the old value.

    Fixed by replacing the per-fk dispatch with [Exec.dispatch_update_cascades],
    which fans out over every fk matching a child table, computes each one's
    writes WITHOUT applying them ([Exec.cascade_update_fk_writes]), merges
    the results by child rowid, and only then issues one
    [Exec.cascade_update_cols_in_tx] call per distinct row -- so two fks
    reaching the same row always merge into one write instead of racing the
    [visited] guard. [Exec.apply_update_cascade_fk] (the top-level UPDATE
    entry point) was folded into the same [dispatch_update_cascades] path
    rather than kept as an independently-maintained duplicate, since it had
    the identical multi-fk defect one call site further out. *)

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

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let expect_rows db ~msg want sql = Alcotest.(check (list string)) msg want (texts db sql)

let expect_error db ~needle sql =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "%S was expected to fail with %S, but succeeded" sql needle
  | Error e ->
    let msg = Format.asprintf "%a" Db.pp_error e in
    Alcotest.(check bool)
      (Printf.sprintf "%S failed with %S (got %S)" sql needle msg)
      true
      (contains ~needle msg)
;;

(* ------------------------------------------------------------------ *)
(* The issue's own repro: ON DELETE SET NULL over a two-column FK must  *)
(* NULL both local columns, not just the first.                        *)
(* ------------------------------------------------------------------ *)

let two_col_setup db ~action =
  exec db "PRAGMA foreign_keys = ON";
  exec db "CREATE TABLE p (a INTEGER, b INTEGER, PRIMARY KEY (a, b))";
  exec
    db
    (Printf.sprintf
       "CREATE TABLE c (id INTEGER PRIMARY KEY, x INTEGER DEFAULT 91, y INTEGER DEFAULT \
        92, FOREIGN KEY (x, y) REFERENCES p(a, b) %s)"
       action);
  exec db "INSERT INTO p VALUES (1, 2)";
  exec db "INSERT INTO c (id, x, y) VALUES (100, 1, 2)"
;;

let test_delete_set_null_writes_both_columns () =
  with_db (fun db ->
    two_col_setup db ~action:"ON DELETE SET NULL";
    exec db "DELETE FROM p WHERE a = 1";
    expect_rows
      db
      ~msg:"both x and y became NULL, not just x"
      [ "<null>|<null>" ]
      "SELECT x, y FROM c WHERE id = 100")
;;

let test_delete_set_default_writes_both_columns () =
  with_db (fun db ->
    two_col_setup db ~action:"ON DELETE SET DEFAULT";
    exec db "DELETE FROM p WHERE a = 1";
    expect_rows
      db
      ~msg:"both x and y took their own DEFAULT, not just x"
      [ "91|92" ]
      "SELECT x, y FROM c WHERE id = 100")
;;

(* ------------------------------------------------------------------ *)
(* ON UPDATE analogue: changing only ONE component of the parent's      *)
(* composite key still cascades a write to EVERY local column.          *)
(* ------------------------------------------------------------------ *)

let test_update_set_null_writes_both_columns () =
  with_db (fun db ->
    two_col_setup db ~action:"ON UPDATE SET NULL";
    exec db "UPDATE p SET a = 10 WHERE a = 1";
    expect_rows
      db
      ~msg:"both x and y became NULL, not just x"
      [ "<null>|<null>" ]
      "SELECT x, y FROM c WHERE id = 100")
;;

let test_update_set_default_writes_both_columns () =
  with_db (fun db ->
    two_col_setup db ~action:"ON UPDATE SET DEFAULT";
    exec db "UPDATE p SET a = 10 WHERE a = 1";
    expect_rows
      db
      ~msg:"both x and y took their own DEFAULT, not just x"
      [ "91|92" ]
      "SELECT x, y FROM c WHERE id = 100")
;;

(* ------------------------------------------------------------------ *)
(* A 3+-column composite key: every local column must be written, not   *)
(* just the first two.                                                  *)
(* ------------------------------------------------------------------ *)

let test_three_column_key_delete_set_null_writes_every_column () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE p3 (a INTEGER, b INTEGER, c INTEGER, PRIMARY KEY (a, b, c))";
    exec
      db
      "CREATE TABLE ch3 (id INTEGER PRIMARY KEY, x INTEGER, y INTEGER, z INTEGER, \
       FOREIGN KEY (x, y, z) REFERENCES p3(a, b, c) ON DELETE SET NULL)";
    exec db "INSERT INTO p3 VALUES (1, 2, 3)";
    exec db "INSERT INTO ch3 (id, x, y, z) VALUES (500, 1, 2, 3)";
    exec db "DELETE FROM p3 WHERE a = 1";
    expect_rows
      db
      ~msg:"all three local columns became NULL"
      [ "<null>|<null>|<null>" ]
      "SELECT x, y, z FROM ch3 WHERE id = 500")
;;

(* ------------------------------------------------------------------ *)
(* NOT NULL on one target column: the whole composite write must be     *)
(* rejected ATOMICALLY, before ANY row is touched -- not just before    *)
(* that one column's own (would-be) write, which is all a columns-outer *)
(* loop actually guaranteed once a nullable column preceded it.         *)
(* ------------------------------------------------------------------ *)

(* [y] is NOT NULL and sorts AFTER the nullable [x] in the FK's own local
   column list, so a columns-outer loop would already have written NULL
   into [x] for this row before ever reaching [y]'s rejection. *)
let not_null_second_setup db ~action =
  exec db "PRAGMA foreign_keys = ON";
  exec db "CREATE TABLE p (a INTEGER, b INTEGER, PRIMARY KEY (a, b))";
  exec
    db
    (Printf.sprintf
       "CREATE TABLE c (id INTEGER PRIMARY KEY, x INTEGER, y INTEGER NOT NULL, FOREIGN \
        KEY (x, y) REFERENCES p(a, b) %s)"
       action);
  exec db "INSERT INTO p VALUES (1, 2)";
  exec db "INSERT INTO c (id, x, y) VALUES (600, 1, 2)"
;;

let test_delete_set_null_not_null_column_rejects_atomically () =
  with_db (fun db ->
    not_null_second_setup db ~action:"ON DELETE SET NULL";
    expect_error
      db
      ~needle:"FOREIGN KEY constraint failed: ON DELETE SET NULL on NOT NULL column 'c.y'"
      "DELETE FROM p WHERE a = 1";
    expect_rows
      db
      ~msg:"x was NOT nulled either -- nothing partially written"
      [ "1|2" ]
      "SELECT x, y FROM c WHERE id = 600";
    expect_rows
      db
      ~msg:"the parent row was not removed either"
      [ "1|2" ]
      "SELECT * FROM p")
;;

let test_update_set_null_not_null_column_rejects_atomically () =
  with_db (fun db ->
    not_null_second_setup db ~action:"ON UPDATE SET NULL";
    expect_error
      db
      ~needle:"FOREIGN KEY constraint failed: ON UPDATE SET NULL on NOT NULL column 'c.y'"
      "UPDATE p SET a = 10 WHERE a = 1";
    expect_rows
      db
      ~msg:"x was NOT nulled either -- nothing partially written"
      [ "1|2" ]
      "SELECT x, y FROM c WHERE id = 600")
;;

(* Same shape for SET DEFAULT: [y] is NOT NULL with no DEFAULT clause, so
   its resolved default is NULL and the whole write must be rejected before
   [x] (which does have a usable default) is touched. *)
let not_null_no_default_setup db ~action =
  exec db "PRAGMA foreign_keys = ON";
  exec db "CREATE TABLE p (a INTEGER, b INTEGER, PRIMARY KEY (a, b))";
  exec
    db
    (Printf.sprintf
       "CREATE TABLE c (id INTEGER PRIMARY KEY, x INTEGER DEFAULT 91, y INTEGER NOT \
        NULL, FOREIGN KEY (x, y) REFERENCES p(a, b) %s)"
       action);
  exec db "INSERT INTO p VALUES (1, 2)";
  exec db "INSERT INTO c (id, x, y) VALUES (700, 1, 2)"
;;

let test_delete_set_default_not_null_no_default_rejects_atomically () =
  with_db (fun db ->
    not_null_no_default_setup db ~action:"ON DELETE SET DEFAULT";
    expect_error
      db
      ~needle:
        "FOREIGN KEY constraint failed: ON DELETE SET DEFAULT on NOT NULL column 'c.y' \
         with no default"
      "DELETE FROM p WHERE a = 1";
    expect_rows
      db
      ~msg:"x was NOT defaulted either -- nothing partially written"
      [ "1|2" ]
      "SELECT x, y FROM c WHERE id = 700")
;;

let test_update_set_default_not_null_no_default_rejects_atomically () =
  with_db (fun db ->
    not_null_no_default_setup db ~action:"ON UPDATE SET DEFAULT";
    expect_error
      db
      ~needle:
        "FOREIGN KEY constraint failed: ON UPDATE SET DEFAULT on NOT NULL column 'c.y' \
         with no default"
      "UPDATE p SET a = 10 WHERE a = 1";
    expect_rows
      db
      ~msg:"x was NOT defaulted either -- nothing partially written"
      [ "1|2" ]
      "SELECT x, y FROM c WHERE id = 700")
;;

(* ------------------------------------------------------------------ *)
(* A two-level (grandchild) cascade: the parent DELETE cascades into the *)
(* middle table via ON DELETE CASCADE, and THAT recursive row removal   *)
(* is what drives the grandchild's SET NULL / SET DEFAULT -- exercising *)
(* Exec.cascade_delete_set_null / cascade_delete_set_default directly   *)
(* rather than the top-level Exec.cascade_apply_set_null /              *)
(* cascade_apply_set_default every other test above goes through.       *)
(* ------------------------------------------------------------------ *)

let nested_setup db ~grandchild_action =
  exec db "PRAGMA foreign_keys = ON";
  exec db "CREATE TABLE p (a INTEGER, b INTEGER, PRIMARY KEY (a, b))";
  exec
    db
    "CREATE TABLE m (x INTEGER, y INTEGER, PRIMARY KEY (x, y), FOREIGN KEY (x, y) \
     REFERENCES p(a, b) ON DELETE CASCADE)";
  exec
    db
    (Printf.sprintf
       "CREATE TABLE gc (id INTEGER PRIMARY KEY, u INTEGER DEFAULT 93, v INTEGER DEFAULT \
        94, FOREIGN KEY (u, v) REFERENCES m(x, y) %s)"
       grandchild_action);
  exec db "INSERT INTO p VALUES (1, 2)";
  exec db "INSERT INTO m VALUES (1, 2)";
  exec db "INSERT INTO gc (id, u, v) VALUES (900, 1, 2)"
;;

let test_nested_cascade_set_null_writes_both_columns () =
  with_db (fun db ->
    nested_setup db ~grandchild_action:"ON DELETE SET NULL";
    exec db "DELETE FROM p WHERE a = 1";
    expect_rows db ~msg:"the middle row is gone (ON DELETE CASCADE)" [] "SELECT * FROM m";
    expect_rows
      db
      ~msg:"both u and v became NULL via the nested SET NULL cascade"
      [ "<null>|<null>" ]
      "SELECT u, v FROM gc WHERE id = 900")
;;

let test_nested_cascade_set_default_writes_both_columns () =
  with_db (fun db ->
    nested_setup db ~grandchild_action:"ON DELETE SET DEFAULT";
    exec db "DELETE FROM p WHERE a = 1";
    expect_rows
      db
      ~msg:"both u and v took their own DEFAULT via the nested SET DEFAULT cascade"
      [ "93|94" ]
      "SELECT u, v FROM gc WHERE id = 900")
;;

(* ------------------------------------------------------------------ *)
(* A STORED generated column computed from BOTH target columns must see *)
(* the post-image with BOTH already applied, not an intermediate state  *)
(* where only the first has been written yet.                          *)
(* ------------------------------------------------------------------ *)

let test_stored_generated_column_sees_both_new_values () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE p (a INTEGER, b INTEGER, PRIMARY KEY (a, b))";
    exec
      db
      "CREATE TABLE c (id INTEGER PRIMARY KEY, x INTEGER, y INTEGER, both_null INTEGER \
       GENERATED ALWAYS AS (CASE WHEN x IS NULL AND y IS NULL THEN 1 ELSE 0 END) STORED, \
       FOREIGN KEY (x, y) REFERENCES p(a, b) ON DELETE SET NULL)";
    exec db "INSERT INTO p VALUES (1, 2)";
    exec db "INSERT INTO c (id, x, y) VALUES (800, 1, 2)";
    exec db "DELETE FROM p WHERE a = 1";
    expect_rows
      db
      ~msg:"both_null was computed from the row with x AND y already NULL"
      [ "<null>|<null>|1" ]
      "SELECT x, y, both_null FROM c WHERE id = 800")
;;

(* ------------------------------------------------------------------ *)
(* A composite UNIQUE index spanning both target columns must be probed *)
(* against the row's FINAL values (both new values together), not an    *)
(* intermediate one-column-changed state that would miss a real         *)
(* collision.                                                           *)
(* ------------------------------------------------------------------ *)

let test_unique_index_probe_sees_the_full_new_row () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE p (a INTEGER, b INTEGER, PRIMARY KEY (a, b))";
    exec
      db
      "CREATE TABLE c (id INTEGER PRIMARY KEY, x INTEGER DEFAULT 9, y INTEGER DEFAULT 9, \
       UNIQUE (x, y), FOREIGN KEY (x, y) REFERENCES p(a, b) ON DELETE SET DEFAULT)";
    exec db "INSERT INTO p VALUES (1, 2)";
    exec db "INSERT INTO p VALUES (9, 9)";
    exec db "INSERT INTO c (id, x, y) VALUES (100, 1, 2)";
    (* Row 200 already occupies the (9, 9) combo that row 100's SET DEFAULT
       would produce -- only detectable once BOTH x and y take their
       defaults together in the same probe. *)
    exec db "INSERT INTO c (id, x, y) VALUES (200, 9, 9)";
    expect_error
      db
      ~needle:"UNIQUE constraint failed: c.x, c.y"
      "DELETE FROM p WHERE a = 1";
    expect_rows
      db
      ~msg:"nothing moved: the cascade wrote nothing"
      [ "100|1|2"; "200|9|9" ]
      "SELECT id, x, y FROM c ORDER BY id";
    expect_rows
      db
      ~msg:"the parent delete itself was rolled back too"
      [ "1|2"; "9|9" ]
      "SELECT * FROM p ORDER BY a")
;;

(* ------------------------------------------------------------------ *)
(* Round 2 (#793 review): a composite FK matching 2+ columns changed     *)
(* TOGETHER must be dispatched ONCE, not once per column -- both at the  *)
(* downstream re-cascade level and at the top-level UPDATE CASCADE entry *)
(* point.                                                                *)
(* ------------------------------------------------------------------ *)

(* gp(a,b) --[ON DELETE SET NULL]--> p(x,y) --[ON UPDATE <action>]--> c(cx,cy)
   [DELETE FROM gp] nulls p.x and p.y TOGETHER (round 1's fix), which must
   then re-cascade to c as ONE dispatch carrying both changed columns, not
   two separate single-column dispatches that collide on the shared
   [visited] short-circuit. *)
let three_level_chain_setup db ~grandchild_action =
  exec db "PRAGMA foreign_keys = ON";
  exec db "CREATE TABLE gp (a INTEGER, b INTEGER, PRIMARY KEY (a, b))";
  exec
    db
    "CREATE TABLE p (id INTEGER PRIMARY KEY, x INTEGER DEFAULT 91, y INTEGER DEFAULT 92, \
     FOREIGN KEY (x, y) REFERENCES gp(a, b) ON DELETE SET NULL)";
  exec
    db
    (Printf.sprintf
       "CREATE TABLE c (id INTEGER PRIMARY KEY, cx INTEGER DEFAULT 93, cy INTEGER \
        DEFAULT 94, FOREIGN KEY (cx, cy) REFERENCES p(x, y) %s)"
       grandchild_action);
  exec db "INSERT INTO gp VALUES (1, 2)";
  exec db "INSERT INTO p (id, x, y) VALUES (10, 1, 2)";
  exec db "INSERT INTO c (id, cx, cy) VALUES (100, 1, 2)"
;;

let test_downstream_recascade_cascade_writes_both_columns () =
  with_db (fun db ->
    three_level_chain_setup db ~grandchild_action:"ON UPDATE CASCADE";
    exec db "DELETE FROM gp WHERE a = 1";
    expect_rows
      db
      ~msg:"p.x and p.y both became NULL (round 1)"
      [ "<null>|<null>" ]
      "SELECT x, y FROM p WHERE id = 10";
    expect_rows
      db
      ~msg:"the re-cascade to c wrote BOTH cx and cy, not just one"
      [ "<null>|<null>" ]
      "SELECT cx, cy FROM c WHERE id = 100")
;;

let test_downstream_recascade_set_null_writes_both_columns () =
  with_db (fun db ->
    three_level_chain_setup db ~grandchild_action:"ON UPDATE SET NULL";
    exec db "DELETE FROM gp WHERE a = 1";
    expect_rows
      db
      ~msg:"the re-cascade's SET NULL wrote BOTH cx and cy, not just one"
      [ "<null>|<null>" ]
      "SELECT cx, cy FROM c WHERE id = 100")
;;

let test_downstream_recascade_set_default_writes_both_columns () =
  with_db (fun db ->
    three_level_chain_setup db ~grandchild_action:"ON UPDATE SET DEFAULT";
    exec db "DELETE FROM gp WHERE a = 1";
    expect_rows
      db
      ~msg:"the re-cascade's SET DEFAULT wrote BOTH cx and cy, not just one"
      [ "93|94" ]
      "SELECT cx, cy FROM c WHERE id = 100")
;;

(* A direct top-level [UPDATE parent SET a = .., b = ..] reaching an
   [ON UPDATE CASCADE] over a composite FK -- [Exec.apply_update_cascade_fk],
   not the downstream re-cascade above. This is a different call site from
   round 1's DELETE-triggered repro and was not exercised by it at all. *)
let test_direct_update_cascade_writes_both_columns () =
  with_db (fun db ->
    two_col_setup db ~action:"ON UPDATE CASCADE";
    exec db "UPDATE p SET a = 10, b = 20 WHERE a = 1";
    expect_rows
      db
      ~msg:"the child's x and y both followed the parent's new key, not just x"
      [ "10|20" ]
      "SELECT x, y FROM c WHERE id = 100")
;;

(* ------------------------------------------------------------------ *)
(* Round 3 (#793 review): TWO SEPARATE fk constraints from the same     *)
(* child table, each individually single-column (not a composite key),  *)
(* both matching the SAME child row via the SAME parent table.          *)
(* ------------------------------------------------------------------ *)

let two_independent_fks_setup db ~action =
  exec db "PRAGMA foreign_keys = ON";
  exec db "CREATE TABLE p (id INTEGER PRIMARY KEY)";
  exec
    db
    (Printf.sprintf
       "CREATE TABLE c (id INTEGER PRIMARY KEY, ref1 INTEGER, ref2 INTEGER, FOREIGN KEY \
        (ref1) REFERENCES p(id) %s, FOREIGN KEY (ref2) REFERENCES p(id) %s)"
       action
       action);
  exec db "INSERT INTO p VALUES (1)";
  exec db "INSERT INTO c VALUES (100, 1, 1)"
;;

let test_two_separate_fks_same_row_cascade_writes_both () =
  with_db (fun db ->
    two_independent_fks_setup db ~action:"ON UPDATE CASCADE";
    exec db "UPDATE p SET id = 2 WHERE id = 1";
    expect_rows
      db
      ~msg:"both ref1 and ref2 followed the parent's new key, not just ref1"
      [ "2|2" ]
      "SELECT ref1, ref2 FROM c WHERE id = 100")
;;

let test_two_separate_fks_same_row_set_null_writes_both () =
  with_db (fun db ->
    two_independent_fks_setup db ~action:"ON UPDATE SET NULL";
    exec db "UPDATE p SET id = 2 WHERE id = 1";
    expect_rows
      db
      ~msg:"both ref1 and ref2 were nulled, not just ref1"
      [ "<null>|<null>" ]
      "SELECT ref1, ref2 FROM c WHERE id = 100")
;;

(* The identical collision on the ON DELETE side: {!cascade_delete_row_in_tx}
   used to dispatch each matching fk through its own independent
   [cascade_delete_fk] call, so two SET NULL/SET DEFAULT fks reaching the
   same child row raced the same shared [visited] set the ON UPDATE side
   did. *)
let test_two_separate_fks_same_row_delete_set_null_writes_both () =
  with_db (fun db ->
    two_independent_fks_setup db ~action:"ON DELETE SET NULL";
    exec db "DELETE FROM p WHERE id = 1";
    expect_rows
      db
      ~msg:"both ref1 and ref2 were nulled, not just ref1"
      [ "<null>|<null>" ]
      "SELECT ref1, ref2 FROM c WHERE id = 100")
;;

let test_two_separate_fks_same_row_delete_set_default_writes_both () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE p (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c (id INTEGER PRIMARY KEY, ref1 INTEGER DEFAULT 901, ref2 INTEGER \
       DEFAULT 902, FOREIGN KEY (ref1) REFERENCES p(id) ON DELETE SET DEFAULT, FOREIGN \
       KEY (ref2) REFERENCES p(id) ON DELETE SET DEFAULT)";
    exec db "INSERT INTO p VALUES (1)";
    exec db "INSERT INTO c VALUES (100, 1, 1)";
    exec db "DELETE FROM p WHERE id = 1";
    expect_rows
      db
      ~msg:"both ref1 and ref2 reset to their own DEFAULT, not just ref1"
      [ "901|902" ]
      "SELECT ref1, ref2 FROM c WHERE id = 100")
;;

(* ------------------------------------------------------------------ *)
(* Round 4 (#793 review): TWO DIFFERENT child tables converging on the  *)
(* same descendant row ("diamond" convergence) -- distinct from round   *)
(* 3's two-fks-on-ONE-child-table fix.                                  *)
(* ------------------------------------------------------------------ *)

(* P(id); C1(id REFERENCES P(id) ON UPDATE CASCADE) -- C1's OWN primary key
   IS the FK to P, so updating P.id cascades C1.id to match; C2 the same.
   G(id,from_c1,from_c2) with FK(from_c1)->C1(id) ON UPDATE SET NULL and
   FK(from_c2)->C2(id) ON UPDATE SET NULL. The two paths P->C1->G and
   P->C2->G converge on the SAME G row: before this round's fix, the second
   path to reach G found it already [visited] (marked by the first path)
   and no-op'd entirely, silently leaving G.from_c2 stale. *)
let test_diamond_convergence_writes_both_columns () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE p (id INTEGER PRIMARY KEY)";
    exec db "CREATE TABLE c1 (id INTEGER PRIMARY KEY REFERENCES p(id) ON UPDATE CASCADE)";
    exec db "CREATE TABLE c2 (id INTEGER PRIMARY KEY REFERENCES p(id) ON UPDATE CASCADE)";
    exec
      db
      "CREATE TABLE g (id INTEGER PRIMARY KEY, from_c1 INTEGER, from_c2 INTEGER, FOREIGN \
       KEY (from_c1) REFERENCES c1(id) ON UPDATE SET NULL, FOREIGN KEY (from_c2) \
       REFERENCES c2(id) ON UPDATE SET NULL)";
    exec db "INSERT INTO p VALUES (1)";
    exec db "INSERT INTO c1 VALUES (1)";
    exec db "INSERT INTO c2 VALUES (1)";
    exec db "INSERT INTO g VALUES (100, 1, 1)";
    exec db "UPDATE p SET id = 2 WHERE id = 1";
    expect_rows
      db
      ~msg:"both c1 and c2 followed p's new key"
      [ "2"; "2" ]
      "SELECT id FROM c1 UNION ALL SELECT id FROM c2";
    expect_rows
      db
      ~msg:
        "g's from_c1 AND from_c2 were both nulled by the two separate cascade paths \
         converging on g, not just the first one to reach it"
      [ "<null>|<null>" ]
      "SELECT from_c1, from_c2 FROM g WHERE id = 100")
;;

(* ------------------------------------------------------------------ *)
(* Round 4 (#793 review): two fks disagreeing on the same column's new  *)
(* value must raise, not silently pick one by incidental list order.    *)
(* ------------------------------------------------------------------ *)

let test_conflicting_fk_writes_to_the_same_column_raise () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE p (id INTEGER PRIMARY KEY, y INTEGER)";
    exec db "CREATE UNIQUE INDEX p_y ON p (y)";
    exec
      db
      "CREATE TABLE c (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, FOREIGN KEY (a) \
       REFERENCES p(id) ON UPDATE CASCADE, FOREIGN KEY (a, b) REFERENCES p(id, y) ON \
       UPDATE SET NULL)";
    exec db "INSERT INTO p VALUES (1, 100)";
    exec db "INSERT INTO c VALUES (10, 1, 100)";
    expect_error db ~needle:"disagree" "UPDATE p SET id = 2, y = 200 WHERE id = 1")
;;

(* ------------------------------------------------------------------ *)
(* Round 4 (#793 review): a NOT NULL target column raises regardless of *)
(* whether any child row currently matches -- restores the deleted      *)
(* single-column function's unconditional-precheck contract, which the  *)
(* round-2/3 rewrite had accidentally narrowed to "only if some row      *)
(* actually needs the write".                                           *)
(* ------------------------------------------------------------------ *)

let test_set_null_not_null_raises_even_with_no_matching_child_rows () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE p (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c (id INTEGER PRIMARY KEY, x INTEGER NOT NULL, FOREIGN KEY (x) \
       REFERENCES p(id) ON UPDATE SET NULL)";
    exec db "INSERT INTO p VALUES (1)";
    (* c is empty -- no child row references p at all. *)
    expect_error db ~needle:"NOT NULL" "UPDATE p SET id = 2 WHERE id = 1")
;;

(* ------------------------------------------------------------------ *)
(* Round 5 (#793 re-review): round 4's revisit-still-writes rule was     *)
(* too wide in three ways.                                              *)
(* ------------------------------------------------------------------ *)

let expect_integrity_ok db ~msg = expect_rows db ~msg [ "ok" ] "PRAGMA integrity_check"

(* The top-level DELETE seeds [visited] with the row it is about to delete
   (so a self-reference cannot cascade back into it). Round 4 let a revisit
   write through anyway: the SET NULL rewrote the doomed row's index entry
   to NULL, and the top-level delete then removed the entry for its STALE
   image (pid = 1), leaving an orphan index entry behind. *)
let test_self_ref_delete_set_null_leaves_index_consistent () =
  with_db (fun db ->
    exec
      db
      "CREATE TABLE t (id INTEGER PRIMARY KEY, pid INTEGER REFERENCES t(id) ON DELETE \
       SET NULL)";
    exec db "CREATE INDEX t_pid ON t (pid)";
    exec db "INSERT INTO t VALUES (1, 1)";
    exec db "INSERT INTO t VALUES (2, 1)";
    (* A self-reference cannot be inserted with enforcement on; enable it
       only for the statement under test. *)
    exec db "PRAGMA foreign_keys = ON";
    exec db "DELETE FROM t WHERE id = 1";
    expect_rows db ~msg:"the surviving child was nulled" [ "2|<null>" ] "SELECT * FROM t";
    expect_integrity_ok db ~msg:"no orphan index entry for the deleted row")
;;

(* Same for the top-level UPDATE, which seeds [visited] with the row it is
   about to rewrite from its own [old_row]. *)
let test_self_ref_update_cascade_leaves_index_consistent () =
  with_db (fun db ->
    exec
      db
      "CREATE TABLE t (id INTEGER PRIMARY KEY, pid INTEGER REFERENCES t(id) ON UPDATE \
       CASCADE, v INTEGER)";
    exec db "CREATE INDEX t_pid ON t (pid)";
    exec db "INSERT INTO t VALUES (1, 1, 0)";
    (* A self-reference cannot be inserted with enforcement on; enable it
       only for the statement under test. *)
    exec db "PRAGMA foreign_keys = ON";
    exec db "UPDATE t SET v = 5, id = 2 WHERE id = 1";
    expect_integrity_ok db ~msg:"the top-level row's index entries match its stored image")
;;

(* Within ONE dispatch the per-fk scans all run before any write, so a row
   later in the merged list can be modified by an earlier entry's recursive
   re-cascade. Round 4 then rewrote it from the pre-recursion snapshot,
   reverting the recursive write (here: [c] back to the dangling 1). *)
let test_revisit_write_uses_the_current_row_not_the_snapshot () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE p (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, c INTEGER, UNIQUE \
       (a), FOREIGN KEY (a) REFERENCES p(id) ON UPDATE CASCADE, FOREIGN KEY (b) \
       REFERENCES p(id) ON UPDATE CASCADE, FOREIGN KEY (c) REFERENCES c(a) ON UPDATE \
       CASCADE)";
    exec db "INSERT INTO p VALUES (1)";
    exec db "INSERT INTO c VALUES (10, 1, NULL, NULL)";
    exec db "INSERT INTO c VALUES (20, NULL, 1, 1)";
    exec db "UPDATE p SET id = 2 WHERE id = 1";
    expect_rows
      db
      ~msg:"row 20 carries BOTH the direct write (b) and the recursive one (c)"
      [ "10|2|<null>|<null>"; "20|<null>|2|2" ]
      "SELECT id, a, b, c FROM c ORDER BY id")
;;

(* Round 4 moved the NOT NULL precheck ahead of the child-row scan for all
   four SET NULL/SET DEFAULT paths, but only [ON UPDATE SET NULL] ever did
   that on [main]; the other three scanned first. Deleting a parent with NO
   referencing child must not fail just because the child's schema could
   not absorb the action. *)
let test_delete_with_no_matching_child_does_not_precheck () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE p (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c1 (id INTEGER PRIMARY KEY, x INTEGER NOT NULL REFERENCES p(id) ON \
       DELETE SET NULL)";
    exec
      db
      "CREATE TABLE c2 (id INTEGER PRIMARY KEY, x INTEGER NOT NULL REFERENCES p(id) ON \
       DELETE SET DEFAULT ON UPDATE SET DEFAULT)";
    exec db "INSERT INTO p VALUES (1)";
    exec db "INSERT INTO p VALUES (2)";
    exec db "UPDATE p SET id = 3 WHERE id = 2";
    exec db "DELETE FROM p WHERE id = 1";
    expect_rows db ~msg:"only the updated parent remains" [ "3" ] "SELECT id FROM p";
    (* ...but a row that DOES match still refuses. *)
    exec db "INSERT INTO c1 VALUES (1, 3)";
    expect_error db ~needle:"NOT NULL" "DELETE FROM p WHERE id = 3")
;;

(* ------------------------------------------------------------------ *)
(* Interaction with #778 (RETURNING fires row hooks) and #786/#789      *)
(* (a failing statement rolls the deferred-FK queue back to its mark).  *)
(* ------------------------------------------------------------------ *)

(* A [DELETE ... RETURNING] on the parent goes through the same cascade as
   the plain spelling, so the child's [`After] update hook fires once for
   the merged write and sees EVERY composite column nulled together. *)
let test_returning_delete_hook_sees_full_composite_write () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE p (a INTEGER, b INTEGER, PRIMARY KEY (a, b))";
    exec
      db
      "CREATE TABLE c (id INTEGER PRIMARY KEY, x INTEGER, y INTEGER, FOREIGN KEY (x, y) \
       REFERENCES p(a, b) ON DELETE SET NULL)";
    exec db "INSERT INTO p VALUES (1, 2)";
    exec db "INSERT INTO c VALUES (10, 1, 2)";
    let seen = ref [] in
    (match
       Db.register_row_hook db ~table:"c" ~timing:`After ~event:`Update (fun m ->
         (match m.Db.new_row with
          | Some r ->
            seen := !seen @ [ String.concat "|" (Array.to_list (Array.map show_value r)) ]
          | None -> ());
         Lwt.return (Ok ()))
     with
     | Ok (_ : Db.row_hook) -> ()
     | Error _ -> Alcotest.fail "could not register the row hook");
    expect_rows
      db
      ~msg:"RETURNING reports the deleted parent"
      [ "1" ]
      "DELETE FROM p WHERE a = 1 RETURNING a";
    Alcotest.(check (list string))
      "one hook firing, both composite columns NULL"
      [ "10|<null>|<null>" ]
      !seen;
    expect_rows db ~msg:"stored row matches" [ "10|<null>|<null>" ] "SELECT * FROM c")
;;

(* The round-4 conflict raise is an ordinary statement failure: inside an
   explicit transaction it must discard only its own deferred-FK additions,
   so an earlier statement's obligation still makes COMMIT refuse. *)
let test_conflict_raise_keeps_earlier_deferred_obligation () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE q (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE d (id INTEGER PRIMARY KEY, qid INTEGER REFERENCES q(id) DEFERRABLE \
       INITIALLY DEFERRED)";
    exec db "CREATE TABLE p (id INTEGER PRIMARY KEY, y INTEGER)";
    exec db "CREATE UNIQUE INDEX p_y ON p (y)";
    exec
      db
      "CREATE TABLE c (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, FOREIGN KEY (a) \
       REFERENCES p(id) ON UPDATE CASCADE, FOREIGN KEY (a, b) REFERENCES p(id, y) ON \
       UPDATE SET NULL)";
    exec db "INSERT INTO p VALUES (1, 100)";
    exec db "INSERT INTO c VALUES (10, 1, 100)";
    exec db "BEGIN";
    exec db "INSERT INTO d VALUES (1, 999)";
    expect_error db ~needle:"disagree" "UPDATE p SET id = 2, y = 200 WHERE id = 1";
    expect_error db ~needle:"FOREIGN KEY" "COMMIT")
;;

(* ------------------------------------------------------------------ *)

(* Builds a fresh gp(col0..col{n-1}) / c(id, cc0..cc{n-1}) pair with an
   n-column [ON DELETE SET NULL]/[ON DELETE SET DEFAULT] FK, inserts one
   matching row, deletes the parent, and reports whether every one of the n
   child columns ended up at the expected value ([<null>] for SET NULL, or
   its declared DEFAULT for SET DEFAULT). *)
let cascades_every_column ~action ~expect n =
  with_db (fun db ->
    let cols = List.init n (fun i -> Printf.sprintf "col%d" i) in
    let ccols = List.init n (fun i -> Printf.sprintf "cc%d" i) in
    let vals = List.init n (fun i -> string_of_int (i + 1)) in
    exec db "PRAGMA foreign_keys = ON";
    exec
      db
      (Printf.sprintf
         "CREATE TABLE gp (%s, PRIMARY KEY (%s))"
         (String.concat ", " (List.map (fun c -> c ^ " INTEGER") cols))
         (String.concat ", " cols));
    exec
      db
      (Printf.sprintf
         "CREATE TABLE c (id INTEGER PRIMARY KEY, %s, FOREIGN KEY (%s) REFERENCES gp(%s) \
          %s)"
         (String.concat
            ", "
            (List.mapi
               (fun i c -> Printf.sprintf "%s INTEGER DEFAULT %d" c (900 + i))
               ccols))
         (String.concat ", " ccols)
         (String.concat ", " cols)
         action);
    exec db (Printf.sprintf "INSERT INTO gp VALUES (%s)" (String.concat ", " vals));
    exec
      db
      (Printf.sprintf
         "INSERT INTO c (id, %s) VALUES (1, %s)"
         (String.concat ", " ccols)
         (String.concat ", " vals));
    exec db (Printf.sprintf "DELETE FROM gp WHERE %s = 1" (List.hd cols));
    let row =
      match
        texts
          db
          (Printf.sprintf "SELECT %s FROM c WHERE id = 1" (String.concat ", " ccols))
      with
      | [ r ] -> r
      | _ -> Alcotest.fail "expected exactly one row"
    in
    String.equal row (String.concat "|" (List.init n expect)))
;;

let prop_set_null_cascades_every_column_regardless_of_arity =
  QCheck.Test.make
    ~count:200
    ~name:
      "an N-column composite FK's ON DELETE SET NULL nulls every one of its N local \
       columns"
    (QCheck.int_range 2 8)
    (cascades_every_column ~action:"ON DELETE SET NULL" ~expect:(fun _ -> "<null>"))
;;

let prop_set_default_cascades_every_column_regardless_of_arity =
  QCheck.Test.make
    ~count:200
    ~name:
      "an N-column composite FK's ON DELETE SET DEFAULT resets every one of its N local \
       columns to its own DEFAULT"
    (QCheck.int_range 2 8)
    (cascades_every_column ~action:"ON DELETE SET DEFAULT" ~expect:(fun i ->
       string_of_int (900 + i)))
;;

let () =
  Alcotest.run
    "composite FK cascade SET NULL / SET DEFAULT writes every local column (#787/#790)"
    [ ( "two-column FK: every local column is written"
      , [ Alcotest.test_case
            "ON DELETE SET NULL"
            `Quick
            test_delete_set_null_writes_both_columns
        ; Alcotest.test_case
            "ON DELETE SET DEFAULT"
            `Quick
            test_delete_set_default_writes_both_columns
        ; Alcotest.test_case
            "ON UPDATE SET NULL"
            `Quick
            test_update_set_null_writes_both_columns
        ; Alcotest.test_case
            "ON UPDATE SET DEFAULT"
            `Quick
            test_update_set_default_writes_both_columns
        ] )
    ; ( "3+-column composite key"
      , [ Alcotest.test_case
            "ON DELETE SET NULL writes every column"
            `Quick
            test_three_column_key_delete_set_null_writes_every_column
        ] )
    ; ( "NOT NULL on one target column rejects the whole write atomically"
      , [ Alcotest.test_case
            "ON DELETE SET NULL"
            `Quick
            test_delete_set_null_not_null_column_rejects_atomically
        ; Alcotest.test_case
            "ON UPDATE SET NULL"
            `Quick
            test_update_set_null_not_null_column_rejects_atomically
        ; Alcotest.test_case
            "ON DELETE SET DEFAULT, no default"
            `Quick
            test_delete_set_default_not_null_no_default_rejects_atomically
        ; Alcotest.test_case
            "ON UPDATE SET DEFAULT, no default"
            `Quick
            test_update_set_default_not_null_no_default_rejects_atomically
        ] )
    ; ( "nested (grandchild) cascade exercises cascade_delete_set_null/_set_default \
         directly"
      , [ Alcotest.test_case
            "ON DELETE SET NULL"
            `Quick
            test_nested_cascade_set_null_writes_both_columns
        ; Alcotest.test_case
            "ON DELETE SET DEFAULT"
            `Quick
            test_nested_cascade_set_default_writes_both_columns
        ] )
    ; ( "the single per-row write is what every dependent computation sees"
      , [ Alcotest.test_case
            "a STORED generated column sees both new values"
            `Quick
            test_stored_generated_column_sees_both_new_values
        ; Alcotest.test_case
            "a composite UNIQUE index is probed against the full new row"
            `Quick
            test_unique_index_probe_sees_the_full_new_row
        ] )
    ; ( "round 2 (#793 review): 2+ changed columns matching one FK dispatch once, not \
         once per column"
      , [ Alcotest.test_case
            "downstream re-cascade: ON UPDATE CASCADE"
            `Quick
            test_downstream_recascade_cascade_writes_both_columns
        ; Alcotest.test_case
            "downstream re-cascade: ON UPDATE SET NULL"
            `Quick
            test_downstream_recascade_set_null_writes_both_columns
        ; Alcotest.test_case
            "downstream re-cascade: ON UPDATE SET DEFAULT"
            `Quick
            test_downstream_recascade_set_default_writes_both_columns
        ; Alcotest.test_case
            "top-level UPDATE ... CASCADE (apply_update_cascade_fk)"
            `Quick
            test_direct_update_cascade_writes_both_columns
        ] )
    ; ( "round 3 (#793 review): two SEPARATE fk constraints from the same child table \
         matching the same row"
      , [ Alcotest.test_case
            "ON UPDATE CASCADE"
            `Quick
            test_two_separate_fks_same_row_cascade_writes_both
        ; Alcotest.test_case
            "ON UPDATE SET NULL"
            `Quick
            test_two_separate_fks_same_row_set_null_writes_both
        ; Alcotest.test_case
            "ON DELETE SET NULL"
            `Quick
            test_two_separate_fks_same_row_delete_set_null_writes_both
        ; Alcotest.test_case
            "ON DELETE SET DEFAULT"
            `Quick
            test_two_separate_fks_same_row_delete_set_default_writes_both
        ] )
    ; ( "round 4 (#793 review): diamond convergence, conflicting writes, and NOT NULL \
         precheck ordering"
      , [ Alcotest.test_case
            "two different child tables converging on one descendant both write"
            `Quick
            test_diamond_convergence_writes_both_columns
        ; Alcotest.test_case
            "two fks disagreeing on the same column's value raise"
            `Quick
            test_conflicting_fk_writes_to_the_same_column_raise
        ; Alcotest.test_case
            "SET NULL's NOT NULL precheck fires even with zero matching child rows"
            `Quick
            test_set_null_not_null_raises_even_with_no_matching_child_rows
        ] )
    ; ( "round 5 (#793 re-review): revisit writes and precheck scope"
      , [ Alcotest.test_case
            "self-referencing DELETE SET NULL keeps the index consistent"
            `Quick
            test_self_ref_delete_set_null_leaves_index_consistent
        ; Alcotest.test_case
            "self-referencing UPDATE CASCADE keeps the index consistent"
            `Quick
            test_self_ref_update_cascade_leaves_index_consistent
        ; Alcotest.test_case
            "a revisit write starts from the current row, not the scan snapshot"
            `Quick
            test_revisit_write_uses_the_current_row_not_the_snapshot
        ; Alcotest.test_case
            "DELETE/UPDATE SET DEFAULT and DELETE SET NULL precheck only a matched row"
            `Quick
            test_delete_with_no_matching_child_does_not_precheck
        ; Alcotest.test_case
            "DELETE ... RETURNING fires the child hook with the full composite write"
            `Quick
            test_returning_delete_hook_sees_full_composite_write
        ; Alcotest.test_case
            "a conflict raise keeps an earlier deferred FK obligation for COMMIT"
            `Quick
            test_conflict_raise_keeps_earlier_deferred_obligation
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_set_null_cascades_every_column_regardless_of_arity
          ; prop_set_default_cascades_every_column_regardless_of_arity
          ] )
    ]
;;
