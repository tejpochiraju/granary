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
    ; "property", List.map QCheck_alcotest.to_alcotest [ prop_restrict_matches_equality ]
    ]
;;
