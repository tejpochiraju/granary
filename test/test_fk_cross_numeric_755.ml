(** #755: a cross-numeric FOREIGN KEY reference is missed when the child
    column is indexed, so RESTRICT orphans the row.

    {1 The defect}

    [Exec.fk_child_has_ref_multi] (and its deferred twin,
    [fk_child_has_ref_multi_in_tx]) has two arms. When
    [Cat.find_index_covering_cols] finds an index on the child's FK columns it
    seeked that index with [encode_index_key_prefix (List.map
    row_value_to_index_value parent_vals)] — RAW bytes, so [1] and [1.0] are
    different keys and the seek can walk straight past a child row that
    [=] (via {!Exec.compare_values}, exact since #579/#738) says matches. When
    no index covers the columns, the fallback scan compares with
    [compare_values] and was already correct.

    A child column and its parent column may be declared with different
    numeric types — nothing requires them to match — so the byte-exact seek is
    reachable without the "one column, one storage class" protection that
    keeps the UNIQUE conflict probe safe.

    {1 The fix}

    Both arms now resolve the child index's DECLARED column types from
    [child_meta.Cat.columns] and route the seek through
    [Exec.index_lookup_values] — the same exact cross-numeric translation
    #743 gave the nested-loop join probe ([Plan.probe_part] /
    [Exec.nlj_probe_values]). [None] from that translation means "no key of
    the child column's type can equal this parent value", which for the FK
    case is the honest "no child row can reference this, so acting on the
    parent row is safe" answer — not a reason to fall back to the full scan.

    {1 Cascade-path audit (per the issue's own instruction)}

    [Exec.scan_child_rows_multi_tx] had the identical defect and is the
    function that actually LOCATES the child rows for every FK action, not
    just RESTRICT's existence probe: [cascade_delete_restrict]'s immediate
    (non-deferred) check, [cascade_delete_set_null],
    [cascade_delete_set_default], [FA_cascade]'s delete cascade, and
    [cascade_update_fk]'s CASCADE-on-UPDATE all call it. So the bug was not
    confined to RESTRICT: a cross-numeric-indexed child row was silently
    skipped by SET NULL / SET DEFAULT / CASCADE too, leaving a stale FK value
    behind exactly like the RESTRICT case, just without an error. Both
    functions are fixed in this change; [update_col_in_tx] itself updates an
    already-located row by rowid and needed no change. *)

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

let expect_ok db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e ->
    Alcotest.failf "%S was expected to succeed, but failed: %a" sql Db.pp_error e
;;

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let expect_fk_refused db sql =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "%S was expected to be refused by FOREIGN KEY RESTRICT" sql
  | Error e ->
    let msg = Format.asprintf "%a" Db.pp_error e in
    Alcotest.(check bool)
      (Printf.sprintf "%S refused with a FOREIGN KEY error (got %S)" sql msg)
      true
      (contains ~needle:"FOREIGN KEY" msg)
;;

(* Like [Db.execute], but a non-[Failure] exception escaping the call (the
   review's item 1 failure mode: an [Invalid_argument]/[Failure "nth"] from a
   stale ordinal) is caught and reported as an [Error] string too, rather than
   aborting the whole test binary — so a regression here shows up as an
   Alcotest failure with the exception's message, not a crash. See
   [Db.commit_txn]: it converts [Failure] to [Error (Runtime _)] itself but
   re-raises everything else. *)
let exec_result db sql : (unit, string) result =
  try
    match run (Db.execute db sql) with
    | Ok () -> Ok ()
    | Error e -> Error (Format.asprintf "%a" Db.pp_error e)
  with
  | exn -> Error (Printf.sprintf "uncaught exception: %s" (Printexc.to_string exn))
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

(* ------------------------------------------------------------------ *)
(* The issue's own repro                                               *)
(* ------------------------------------------------------------------ *)

(* Verified 2026-09-03 as ALLOWED before this fix (orphaning c.x = 1.0). *)
let indexed_cross_numeric_restrict_refuses () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p (k INTEGER PRIMARY KEY, y INTEGER)";
    exec db "CREATE UNIQUE INDEX p_y ON p(y)";
    exec db "INSERT INTO p VALUES (7, 1)";
    exec db "CREATE TABLE c (x REAL REFERENCES p(y))";
    exec db "CREATE INDEX c_x ON c(x)";
    (* Accepted, correctly: 1.0 = 1 (the insert direction was never broken). *)
    expect_ok db "INSERT INTO c VALUES (1.0)";
    expect_fk_refused db "DELETE FROM p WHERE y = 1";
    Alcotest.(check (list string))
      "the parent row survives"
      [ "7|1" ]
      (query_texts db "SELECT * FROM p"))
;;

(* Regression guard: drop the index and the same DELETE was already correctly
   refused (the fallback scan uses [compare_values], exact since #738). This
   pins that the fix did not touch — or worse, invert — the arm that was
   already right. *)
let unindexed_cross_numeric_restrict_still_refuses () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p (k INTEGER PRIMARY KEY, y INTEGER)";
    exec db "CREATE UNIQUE INDEX p_y ON p(y)";
    exec db "INSERT INTO p VALUES (7, 1)";
    exec db "CREATE TABLE c (x REAL REFERENCES p(y))";
    (* No CREATE INDEX c_x here. *)
    expect_ok db "INSERT INTO c VALUES (1.0)";
    expect_fk_refused db "DELETE FROM p WHERE y = 1")
;;

(* The reversed type order: an INTEGER child column referencing a REAL parent
   column, indexed. Exercises the [Row.V_int, Row.Real] arm of
   [Exec.index_lookup_values] rather than [Row.V_real, Row.Integer]. *)
let indexed_cross_numeric_restrict_refuses_reversed_types () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p (k INTEGER PRIMARY KEY, y REAL)";
    exec db "CREATE UNIQUE INDEX p_y ON p(y)";
    exec db "INSERT INTO p VALUES (7, 1.0)";
    exec db "CREATE TABLE c (x INTEGER REFERENCES p(y))";
    exec db "CREATE INDEX c_x ON c(x)";
    expect_ok db "INSERT INTO c VALUES (1)";
    expect_fk_refused db "DELETE FROM p WHERE y = 1.0")
;;

(* ------------------------------------------------------------------ *)
(* No false refusals: same-type FK columns, indexed, in both directions *)
(* ------------------------------------------------------------------ *)

let same_type_case_still_works () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p (k INTEGER PRIMARY KEY, y INTEGER)";
    exec db "CREATE UNIQUE INDEX p_y ON p(y)";
    exec db "INSERT INTO p VALUES (1, 10)";
    exec db "INSERT INTO p VALUES (2, 20)";
    exec db "CREATE TABLE c (x INTEGER REFERENCES p(y))";
    exec db "CREATE INDEX c_x ON c(x)";
    exec db "INSERT INTO c VALUES (10)";
    (* Truly referenced: refused. *)
    expect_fk_refused db "DELETE FROM p WHERE y = 10";
    (* Not referenced by anything: allowed. *)
    expect_ok db "DELETE FROM p WHERE y = 20";
    Alcotest.(check (list string))
      "row 1 (referenced) survives, row 2 (unreferenced) is gone"
      [ "1|10" ]
      (query_texts db "SELECT * FROM p ORDER BY k"))
;;

(* ------------------------------------------------------------------ *)
(* Cascade paths share the defect (the issue's own instruction to audit) *)
(* ------------------------------------------------------------------ *)

(* ON DELETE SET NULL, indexed cross-numeric child column: before the fix,
   [Exec.scan_child_rows_multi_tx]'s indexed arm missed the child row the same
   way the RESTRICT probe did, so the cascade silently found nothing to
   null out and the parent delete proceeded, leaving a dangling REAL value
   behind with no error at all — arguably worse than RESTRICT's loud refusal. *)
let indexed_cross_numeric_set_null_cascade_fires () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p (k INTEGER PRIMARY KEY, y INTEGER)";
    exec db "CREATE UNIQUE INDEX p_y ON p(y)";
    exec db "INSERT INTO p VALUES (7, 1)";
    exec
      db
      "CREATE TABLE c (id INTEGER PRIMARY KEY, x REAL REFERENCES p(y) ON DELETE SET NULL)";
    exec db "CREATE INDEX c_x ON c(x)";
    expect_ok db "INSERT INTO c VALUES (1, 1.0)";
    expect_ok db "DELETE FROM p WHERE y = 1";
    Alcotest.(check (list string))
      "the cascade actually ran: c.x is NULL, not still 1.0"
      [ "1|NULL" ]
      (query_texts db "SELECT * FROM c"))
;;

(* ON DELETE CASCADE, indexed cross-numeric child column: same missed-seek
   failure mode, but the observable symptom is an orphan row that should have
   been deleted rather than a stale value. *)
let indexed_cross_numeric_delete_cascade_fires () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p (k INTEGER PRIMARY KEY, y INTEGER)";
    exec db "CREATE UNIQUE INDEX p_y ON p(y)";
    exec db "INSERT INTO p VALUES (7, 1)";
    exec
      db
      "CREATE TABLE c (id INTEGER PRIMARY KEY, x REAL REFERENCES p(y) ON DELETE CASCADE)";
    exec db "CREATE INDEX c_x ON c(x)";
    expect_ok db "INSERT INTO c VALUES (1, 1.0)";
    expect_ok db "DELETE FROM p WHERE y = 1";
    Alcotest.(check (list string))
      "the child row was cascaded away too"
      []
      (query_texts db "SELECT * FROM c"))
;;

(* ------------------------------------------------------------------ *)
(* PR #765 review item 1: a deferred recheck must not reuse a column     *)
(* ordinal that a mid-transaction DROP COLUMN has shifted or removed.    *)
(* ------------------------------------------------------------------ *)

(* [enforce_insert_fk]'s deferred recheck used to capture [child_col_idxs]
   (the FK column's ordinal) at INSERT time and reuse it unchanged when the
   recheck ran at COMMIT. [junk] sits before [pid] in the child's column
   list, so dropping it shifts [pid] from ordinal 1 to ordinal 0 -- and the
   fix ([Exec.make_fk_recheck], reused here instead of a hand-rolled
   closure) re-resolves the FK's column NAMES against the schema at recheck
   time instead of trusting a captured ordinal. The parent row is never
   inserted, so the violation is genuine and COMMIT must still refuse it --
   without crashing on the stale ordinal in between. *)
let deferred_recheck_survives_drop_column_and_still_refuses () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p1 (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c1 (junk INTEGER, pid INTEGER REFERENCES p1(id) DEFERRABLE INITIALLY \
       DEFERRED)";
    exec db "BEGIN";
    exec db "INSERT INTO c1 VALUES (0, 999)";
    (* Shifts pid from ordinal 1 to ordinal 0 in c1's column list. *)
    exec db "ALTER TABLE c1 DROP COLUMN junk";
    match exec_result db "COMMIT" with
    | Ok () -> Alcotest.fail "COMMIT should refuse: parent 999 was never inserted"
    | Error msg ->
      Alcotest.(check bool)
        (Printf.sprintf
           "COMMIT failed with a FOREIGN KEY error, not a crash (got %S)"
           msg)
        true
        (contains ~needle:"FOREIGN KEY" msg))
;;

(* Same schema drift, but the parent row DOES arrive before COMMIT: the
   recheck must resolve [pid]'s new ordinal (0) correctly and let the
   transaction through, not misread a stale ordinal into a false violation
   (or into comparing the wrong column entirely). *)
let deferred_recheck_survives_drop_column_and_still_allows () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p2 (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c2 (junk INTEGER, pid INTEGER REFERENCES p2(id) DEFERRABLE INITIALLY \
       DEFERRED)";
    exec db "BEGIN";
    exec db "INSERT INTO c2 VALUES (0, 999)";
    exec db "ALTER TABLE c2 DROP COLUMN junk";
    exec db "INSERT INTO p2 VALUES (999)";
    (match exec_result db "COMMIT" with
     | Ok () -> ()
     | Error msg -> Alcotest.failf "COMMIT should have succeeded, got %S" msg);
    Alcotest.(check (list string))
      "the child row is intact"
      [ "999" ]
      (query_texts db "SELECT pid FROM c2"))
;;

(* ------------------------------------------------------------------ *)
(* PR #765 review item 2: a VIRTUAL generated FK-child column's DECLARED *)
(* type may not match its expression's ACTUAL runtime storage class.     *)
(* ------------------------------------------------------------------ *)

(* [x] is declared REAL but its generator [(y)] simply forwards an INTEGER
   column, so the value the index physically stores is [Index_key.IK_int],
   never [IK_real] -- [Row.encode_col_value] enforces that agreement for an
   ordinary or STORED column (a mismatch there raises), but a VIRTUAL column
   is always encoded as NULL and recomputed on read with no such check.
   Before the fix, translating the seek through [x]'s DECLARED type (REAL)
   produced an [IK_real] key that could never match the stored [IK_int]
   entry, so RESTRICT silently missed a real reference. The fix treats a
   VIRTUAL generated FK-child column as ineligible for the indexed fast path
   ([Exec.child_index_key_types] returns [None]), falling back to the scan,
   which recomputes the row and compares by VALUE regardless of the index's
   physical key encoding. *)
let generated_virtual_fk_child_column_restrict_refuses () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p3 (z INTEGER PRIMARY KEY)";
    exec db "INSERT INTO p3 VALUES (1)";
    exec
      db
      "CREATE TABLE c3 (y INTEGER, x REAL GENERATED ALWAYS AS (y) VIRTUAL, FOREIGN KEY \
       (x) REFERENCES p3(z))";
    exec db "CREATE INDEX c3_x ON c3(x)";
    expect_ok db "INSERT INTO c3(y) VALUES (1)";
    expect_fk_refused db "DELETE FROM p3 WHERE z = 1")
;;

(* Same generated-column shape, but the child's virtual value does NOT equal
   the parent row being deleted: no false refusal from treating the index as
   unusable (the scan must still discriminate correctly by value). *)
let generated_virtual_fk_child_column_restrict_allows_when_unreferenced () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p4 (z INTEGER PRIMARY KEY)";
    exec db "INSERT INTO p4 VALUES (1)";
    exec db "INSERT INTO p4 VALUES (2)";
    exec
      db
      "CREATE TABLE c4 (y INTEGER, x REAL GENERATED ALWAYS AS (y) VIRTUAL, FOREIGN KEY \
       (x) REFERENCES p4(z))";
    exec db "CREATE INDEX c4_x ON c4(x)";
    expect_ok db "INSERT INTO c4(y) VALUES (2)";
    expect_ok db "DELETE FROM p4 WHERE z = 1";
    Alcotest.(check (list string))
      "row 2 (referenced) survives"
      [ "2" ]
      (query_texts db "SELECT z FROM p4"))
;;

(* ------------------------------------------------------------------ *)
(* PR #765 review, round 2: the by-NAME re-resolution round 1 added to    *)
(* make_fk_recheck has the same failure class one level up (RENAME), and *)
(* a genuinely broken composite FK must fail loudly, not silently.       *)
(*                                                                        *)
(* Superseded by round 3's structural fix below: renaming or dropping a  *)
(* column a pending deferred FK check still needs is now REFUSED         *)
(* outright at the ALTER TABLE statement itself, so these two mutations  *)
(* never reach COMMIT to exercise make_fk_recheck's by-ordinal/loud-      *)
(* failure logic at all. That logic stays (defence in depth, and the     *)
(* home of #765's own reasoning about WHY ordinal beats name), but the   *)
(* observable behaviour these tests pin moved earlier -- to the ALTER    *)
(* itself -- so they're rewritten to match rather than deleted.          *)
(* ------------------------------------------------------------------ *)

(* Renaming the FK's own local column while its deferred check is still
   pending must be REFUSED outright (round 3's structural fix), not
   attempted-and-chased on the recheck side: round 1 chased [DROP COLUMN]
   by ordinal, round 2 chased [RENAME COLUMN] by name-at-recheck-time, and
   round 3 found a THIRD shape (DROP+ADD of the identical name) that fools
   both. Refusing the mutation while the obligation is live closes the
   whole class instead of predicting its next shape. *)
let alter_table_refuses_rename_of_pending_fk_column () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p8 (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c8 (pid INTEGER REFERENCES p8(id) DEFERRABLE INITIALLY DEFERRED)";
    exec db "BEGIN";
    exec db "INSERT INTO c8 VALUES (999)";
    (match exec_result db "ALTER TABLE c8 RENAME COLUMN pid TO pid2" with
     | Ok () ->
       Alcotest.fail
         "RENAME COLUMN should be refused: pid has a deferred FK check pending"
     | Error msg ->
       Alcotest.(check bool)
         (Printf.sprintf "refused with a clear conflict message (got %S)" msg)
         true
         (contains ~needle:"deferred check still pending" msg));
    exec db "ROLLBACK";
    let col_names =
      match run (Db.query db "PRAGMA table_info(c8)") with
      | Error e -> Alcotest.failf "query: %a" Db.pp_error e
      | Ok stream ->
        List.map
          (fun (r : Db.row) ->
             match r.(1) with
             | Db.V_text s -> s
             | _ -> Alcotest.fail "unexpected PRAGMA table_info shape")
          (run (Lwt_stream.to_list stream))
    in
    Alcotest.(check (list string)) "the column was never renamed" [ "pid" ] col_names)
;;

(* No over-refusal: renaming an UNRELATED column on the SAME table, while a
   deferred FK check is genuinely pending against a DIFFERENT column of
   that table, must succeed normally -- the conflict check is keyed on the
   specific column the pending obligation needs, not on "any ALTER touching
   a table with any pending check". *)
let alter_table_allows_rename_of_unrelated_column_with_pending_fk () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p9 (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c9 (pid INTEGER REFERENCES p9(id) DEFERRABLE INITIALLY DEFERRED, \
       junk INTEGER)";
    exec db "BEGIN";
    exec db "INSERT INTO c9 VALUES (999, 0)";
    (* junk is unrelated to the pending FK check on pid -- must succeed. *)
    expect_ok db "ALTER TABLE c9 RENAME COLUMN junk TO junk2";
    exec db "INSERT INTO p9 VALUES (999)";
    (match exec_result db "COMMIT" with
     | Ok () -> ()
     | Error msg -> Alcotest.failf "COMMIT should have succeeded, got %S" msg);
    Alcotest.(check (list string))
      "the child row is intact"
      [ "999" ]
      (query_texts db "SELECT pid FROM c9"))
;;

(* A composite FK, DEFERRABLE, with a DROP COLUMN attempted on one of its
   two local columns while its deferred check is pending: refused outright
   (round 3), for the same reason as the plain RENAME case above -- this is
   the exact shape round 2 fixed on the recheck side (fail loudly instead
   of silently reporting "not violated"), and round 3's structural fix
   means COMMIT is never reached to exercise that fallback at all. *)
let alter_table_refuses_drop_of_pending_composite_fk_column () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p10 (x INTEGER, y INTEGER, PRIMARY KEY (x, y))";
    exec
      db
      "CREATE TABLE c10 (a INTEGER, b INTEGER, FOREIGN KEY (a, b) REFERENCES p10(x, y) \
       DEFERRABLE INITIALLY DEFERRED)";
    exec db "BEGIN";
    exec db "INSERT INTO c10 VALUES (1, 2)";
    (match exec_result db "ALTER TABLE c10 DROP COLUMN a" with
     | Ok () ->
       Alcotest.fail
         "DROP COLUMN should be refused: (a,b) has a deferred composite FK check pending"
     | Error msg ->
       Alcotest.(check bool)
         (Printf.sprintf "refused with a clear conflict message (got %S)" msg)
         true
         (contains ~needle:"deferred check still pending" msg));
    exec db "ROLLBACK")
;;

(* Round 3's own repro: DROP the FK's local column, then ADD a column back
   under the IDENTICAL name -- a semantically unrelated column that
   happens to fool a name-based (or count-based) staleness check, since
   the name resolves again and the column count is unchanged. The first
   statement (the DROP) is what round 3's structural fix refuses, so the
   ADD is never reached at all; this pins that the two-statement sequence
   is caught at its FIRST step, not "eventually, some other way". *)
let alter_table_refuses_drop_that_would_precede_a_same_name_readd () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p11b (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c11b (pid INTEGER REFERENCES p11b(id) DEFERRABLE INITIALLY DEFERRED, \
       junk INTEGER)";
    exec db "BEGIN";
    exec db "INSERT INTO c11b VALUES (999, 0)";
    (match exec_result db "ALTER TABLE c11b DROP COLUMN pid" with
     | Ok () ->
       Alcotest.fail
         "DROP COLUMN should be refused before the DROP+ADD desync can even be attempted"
     | Error msg ->
       Alcotest.(check bool)
         (Printf.sprintf "refused with a clear conflict message (got %S)" msg)
         true
         (contains ~needle:"deferred check still pending" msg));
    (* The would-be second statement (ADD COLUMN pid TEXT DEFAULT 'x') is
       never reached: the DROP above already failed the transaction's first
       ALTER, and [pid] is untouched -- there is nothing left for an ADD to
       desync. *)
    exec db "ROLLBACK")
;;

(* ------------------------------------------------------------------ *)
(* PR #765 review round 3, item 2: precheck_update_fk/precheck_delete_fk    *)
(* must fail loudly with an FK-specific message, matching the deferred    *)
(* path, rather than a bare find_col_idx_by_name "column not found".      *)
(*                                                                        *)
(* Round 3's own structural refusal (above) prevents this from arising   *)
(* via a mid-transaction race, but it is still reachable through #767's  *)
(* standalone, non-transactional Cat.drop_column corruption: an autocommit *)
(* DROP COLUMN with NO deferred check pending succeeds (out of scope to   *)
(* prevent here -- that is #767's own remit), and a LATER, separate       *)
(* UPDATE/DELETE against the now-corrupted constraint must not crash with *)
(* an internal-looking message. *)
(* ------------------------------------------------------------------ *)

let precheck_update_fk_fails_loudly_not_with_bare_column_not_found () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p12 (id INTEGER PRIMARY KEY)";
    exec db "CREATE TABLE c12 (pid INTEGER REFERENCES p12(id), junk INTEGER)";
    exec db "INSERT INTO p12 VALUES (1)";
    exec db "INSERT INTO c12 VALUES (1, 0)";
    (* No pending deferred check here (autocommit, no BEGIN) -- #767's
       corruption, reached deliberately so this test can pin the SEPARATE
       fix (item 2) rather than round 3's refusal (which only fires while
       an obligation is actually pending). *)
    expect_ok db "ALTER TABLE c12 DROP COLUMN pid";
    match exec_result db "UPDATE p12 SET id = 2 WHERE id = 1" with
    | Ok () -> Alcotest.fail "expected an error: the FK constraint is now unresolvable"
    | Error msg ->
      Alcotest.(check bool)
        (Printf.sprintf
           "a loud, FK-specific message, not a bare \"column not found\" (got %S)"
           msg)
        true
        (contains ~needle:"FOREIGN KEY" msg))
;;

let precheck_delete_fk_fails_loudly_not_with_bare_column_not_found () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p13 (id INTEGER PRIMARY KEY)";
    exec db "CREATE TABLE c13 (pid INTEGER REFERENCES p13(id), junk INTEGER)";
    exec db "INSERT INTO p13 VALUES (1)";
    exec db "INSERT INTO c13 VALUES (1, 0)";
    expect_ok db "ALTER TABLE c13 DROP COLUMN pid";
    match exec_result db "DELETE FROM p13 WHERE id = 1" with
    | Ok () -> Alcotest.fail "expected an error: the FK constraint is now unresolvable"
    | Error msg ->
      Alcotest.(check bool)
        (Printf.sprintf
           "a loud, FK-specific message, not a bare \"column not found\" (got %S)"
           msg)
        true
        (contains ~needle:"FOREIGN KEY" msg))
;;

(* ------------------------------------------------------------------ *)
(* PR #765 review item 5: the new seek path over a WITHOUT ROWID child   *)
(* table, which addresses rows by their PK value rather than a rowid.   *)
(* ------------------------------------------------------------------ *)

let indexed_cross_numeric_restrict_refuses_without_rowid_child () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p11 (k INTEGER PRIMARY KEY, y INTEGER)";
    exec db "CREATE UNIQUE INDEX p11_y ON p11(y)";
    exec db "INSERT INTO p11 VALUES (1, 1)";
    exec
      db
      "CREATE TABLE c11 (id INTEGER PRIMARY KEY, x REAL REFERENCES p11(y)) WITHOUT ROWID";
    exec db "CREATE INDEX c11_x ON c11(x)";
    expect_ok db "INSERT INTO c11 VALUES (1, 1.0)";
    expect_fk_refused db "DELETE FROM p11 WHERE y = 1")
;;

(* ------------------------------------------------------------------ *)
(* Property: RESTRICT agrees with [=] regardless of indexing            *)
(* ------------------------------------------------------------------ *)

(* For any integer [n] and its exact double, a child row equal to it under
   cross-numeric [=] must refuse the parent's delete, and a child row that is
   NOT equal to it must allow the delete — identically whether or not the
   child FK column is indexed. [n] and [n + 1] both stay comfortably inside
   the exact-double range, so no rounding can make this generator itself
   ambiguous about which case it constructed. *)
let restrict_matches_equality ~with_index (n, matches) =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p (k INTEGER PRIMARY KEY, y INTEGER)";
    exec db "CREATE UNIQUE INDEX p_y ON p(y)";
    exec db (Printf.sprintf "INSERT INTO p VALUES (1, %Ld)" n);
    exec db (Printf.sprintf "INSERT INTO p VALUES (2, %Ld)" (Int64.add n 1L));
    exec db "CREATE TABLE c (x REAL REFERENCES p(y))";
    if with_index then exec db "CREATE INDEX c_x ON c(x)";
    (* When [matches], the child equals row 1's [y = n]; otherwise it equals
       row 2's [y = n + 1] instead, so the INSERT itself never violates the
       child's own FK (no need to disable enforcement to build a "no
       reference" fixture). *)
    let child_n = if matches then n else Int64.add n 1L in
    (* Spelled as "<int>.0" rather than through [%.17g] on the float: an
       integral float like [0.0] prints as ["0"] under [%g], which the parser
       reads back as an INTEGER literal and a REAL column refuses. *)
    exec db (Printf.sprintf "INSERT INTO c VALUES (%Ld.0)" child_n);
    match run (Db.execute db "DELETE FROM p WHERE k = 1") with
    | Ok () -> not matches
    | Error _ -> matches)
;;

let prop_restrict_matches_equality =
  QCheck.Test.make
    ~count:200
    ~name:"RESTRICT refuses iff a cross-numeric reference exists, indexed or not"
    QCheck.(pair (int_range (-1_000_000) 1_000_000) bool)
    (fun (n_int, matches) ->
       let n = Int64.of_int n_int in
       restrict_matches_equality ~with_index:true (n, matches)
       && restrict_matches_equality ~with_index:false (n, matches))
;;

let () =
  Alcotest.run
    "test_fk_cross_numeric_755"
    [ ( "restrict"
      , [ Alcotest.test_case
            "indexed cross-numeric RESTRICT refuses (issue repro)"
            `Quick
            indexed_cross_numeric_restrict_refuses
        ; Alcotest.test_case
            "unindexed cross-numeric RESTRICT still refuses"
            `Quick
            unindexed_cross_numeric_restrict_still_refuses
        ; Alcotest.test_case
            "indexed cross-numeric RESTRICT refuses (reversed types)"
            `Quick
            indexed_cross_numeric_restrict_refuses_reversed_types
        ; Alcotest.test_case
            "same-type case still works"
            `Quick
            same_type_case_still_works
        ] )
    ; ( "cascade_paths"
      , [ Alcotest.test_case
            "indexed cross-numeric ON DELETE SET NULL fires"
            `Quick
            indexed_cross_numeric_set_null_cascade_fires
        ; Alcotest.test_case
            "indexed cross-numeric ON DELETE CASCADE fires"
            `Quick
            indexed_cross_numeric_delete_cascade_fires
        ] )
    ; ( "review_item_1_deferred_recheck_schema_drift"
      , [ Alcotest.test_case
            "DROP COLUMN before COMMIT: deferred recheck still refuses"
            `Quick
            deferred_recheck_survives_drop_column_and_still_refuses
        ; Alcotest.test_case
            "DROP COLUMN before COMMIT: deferred recheck still allows"
            `Quick
            deferred_recheck_survives_drop_column_and_still_allows
        ] )
    ; ( "review_item_2_generated_virtual_fk_child"
      , [ Alcotest.test_case
            "VIRTUAL generated FK-child column: RESTRICT refuses"
            `Quick
            generated_virtual_fk_child_column_restrict_refuses
        ; Alcotest.test_case
            "VIRTUAL generated FK-child column: RESTRICT allows when unreferenced"
            `Quick
            generated_virtual_fk_child_column_restrict_allows_when_unreferenced
        ] )
    ; ( "review_round_3_structural_refusal"
      , [ Alcotest.test_case
            "ALTER TABLE refuses RENAME of a pending-FK column"
            `Quick
            alter_table_refuses_rename_of_pending_fk_column
        ; Alcotest.test_case
            "ALTER TABLE allows RENAME of an unrelated column (no over-refusal)"
            `Quick
            alter_table_allows_rename_of_unrelated_column_with_pending_fk
        ; Alcotest.test_case
            "ALTER TABLE refuses DROP of a pending composite-FK column"
            `Quick
            alter_table_refuses_drop_of_pending_composite_fk_column
        ; Alcotest.test_case
            "ALTER TABLE refuses the DROP that would precede a same-name re-ADD"
            `Quick
            alter_table_refuses_drop_that_would_precede_a_same_name_readd
        ] )
    ; ( "review_round_3_item_2_immediate_path_loud_error"
      , [ Alcotest.test_case
            "precheck_update_fk fails loudly, not with bare \"column not found\""
            `Quick
            precheck_update_fk_fails_loudly_not_with_bare_column_not_found
        ; Alcotest.test_case
            "precheck_delete_fk fails loudly, not with bare \"column not found\""
            `Quick
            precheck_delete_fk_fails_loudly_not_with_bare_column_not_found
        ] )
    ; ( "review_item_5_without_rowid_child"
      , [ Alcotest.test_case
            "indexed cross-numeric RESTRICT refuses (WITHOUT ROWID child)"
            `Quick
            indexed_cross_numeric_restrict_refuses_without_rowid_child
        ] )
    ; "property", List.map QCheck_alcotest.to_alcotest [ prop_restrict_matches_equality ]
    ]
;;
