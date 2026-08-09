(** #667: an UPSERT ... DO UPDATE writes through [Exec.write_row_rekeyed],
    whose index loop is an unconditional del/put with no uniqueness probe.
    [Exec.check_index_unique_on_update] is the primitive that would catch a
    collision, and before this fix it had exactly one call site —
    [validate_update_unique], the plain-UPDATE pre-pass — so a DO UPDATE that
    assigns a value colliding with another row's entry in a UNIQUE index wrote
    it silently, leaving two rows with the same key in an index declared
    unique.

    The fix runs the same check, per unique index, inside
    [Exec.execute_upsert_update] before [write_row_rekeyed] moves the row —
    the narrower of the two options the issue named (the other being to push
    it into [write_row_rekeyed] itself, which is also shared by a plain
    UPDATE and ON UPDATE CASCADE and would have changed those paths too).

    [Exec.check_index_unique_on_update] already excludes the row being
    updated from its own conflict probe (by rowid) and exempts an unchanged
    key and a NULL-containing key (#290) — this file does not re-pin those,
    only that the upsert path now calls it at all, for both conflict shapes
    ([DO UPDATE] reached via a secondary UNIQUE index and via the rowid-alias
    PRIMARY KEY, since both funnel through [execute_upsert_update]). *)

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
(* The issue's own repro: a DO UPDATE reached via a secondary UNIQUE     *)
(* index conflict writes a duplicate into a DIFFERENT unique index.      *)
(* ------------------------------------------------------------------ *)

let secondary_conflict_do_update_raises_on_other_unique () =
  with_db (fun db ->
    exec
      db
      "CREATE TABLE m (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, v INTEGER NOT NULL)";
    exec db "CREATE UNIQUE INDEX m_a ON m (a)";
    exec db "CREATE UNIQUE INDEX m_b ON m (b)";
    exec db "INSERT INTO m VALUES (1, 10, 20, 5)";
    exec db "INSERT INTO m VALUES (2, 11, 21, 6)";
    (* conflicts on a -> DO UPDATE runs -> would write b = 21, colliding with
       row 2's b = 21 in the m_b unique index. *)
    expect_error
      db
      ~needle:"UNIQUE constraint failed: m.b"
      "INSERT INTO m VALUES (3, 10, 21, 9) ON CONFLICT(a) DO UPDATE SET b = excluded.b";
    (* Nothing moved: row 2 keeps its original b, row 1 is untouched. *)
    expect_rows
      db
      ~msg:"the conflicting DO UPDATE wrote nothing"
      [ "1|10|20|5"; "2|11|21|6" ]
      "SELECT * FROM m ORDER BY id")
;;

(* A DO UPDATE that does NOT touch the colliding column succeeds — the
   uniqueness probe must not fire on a column the assignment never changes. *)
let do_update_not_touching_unique_column_succeeds () =
  with_db (fun db ->
    exec
      db
      "CREATE TABLE m (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, v INTEGER NOT NULL)";
    exec db "CREATE UNIQUE INDEX m_a ON m (a)";
    exec db "CREATE UNIQUE INDEX m_b ON m (b)";
    exec db "INSERT INTO m VALUES (1, 10, 20, 5)";
    exec db "INSERT INTO m VALUES (2, 11, 21, 6)";
    exec db "INSERT INTO m VALUES (3, 10, 99, 9) ON CONFLICT(a) DO UPDATE SET v = 42";
    expect_rows
      db
      ~msg:"the DO UPDATE ran, v changed, b untouched"
      [ "1|10|20|42"; "2|11|21|6" ]
      "SELECT * FROM m ORDER BY id")
;;

(* A DO UPDATE that moves the colliding column to a value nothing else holds
   succeeds normally. *)
let do_update_to_a_free_value_succeeds () =
  with_db (fun db ->
    exec
      db
      "CREATE TABLE m (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, v INTEGER NOT NULL)";
    exec db "CREATE UNIQUE INDEX m_a ON m (a)";
    exec db "CREATE UNIQUE INDEX m_b ON m (b)";
    exec db "INSERT INTO m VALUES (1, 10, 20, 5)";
    exec db "INSERT INTO m VALUES (2, 11, 21, 6)";
    exec db "INSERT INTO m VALUES (3, 10, 999, 9) ON CONFLICT(a) DO UPDATE SET b = 999";
    expect_rows
      db
      ~msg:"the DO UPDATE ran, moving b to a free value"
      [ "1|10|999|5"; "2|11|21|6" ]
      "SELECT * FROM m ORDER BY id")
;;

(* ------------------------------------------------------------------ *)
(* The alias-PK conflict shape — resolved by a DIFFERENT pass          *)
(* (execute_insert's alias probe) but the same execute_upsert_update.  *)
(* ------------------------------------------------------------------ *)

let alias_pk_conflict_do_update_raises_on_other_unique () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, b INTEGER, v INTEGER NOT NULL)";
    exec db "CREATE UNIQUE INDEX t_b ON t (b)";
    exec db "INSERT INTO t VALUES (1, 20, 5)";
    exec db "INSERT INTO t VALUES (2, 21, 6)";
    expect_error
      db
      ~needle:"UNIQUE constraint failed: t.b"
      "INSERT INTO t VALUES (1, 21, 9) ON CONFLICT(k) DO UPDATE SET b = excluded.b";
    expect_rows
      db
      ~msg:"the conflicting DO UPDATE wrote nothing"
      [ "1|20|5"; "2|21|6" ]
      "SELECT * FROM t ORDER BY k")
;;

(* ------------------------------------------------------------------ *)
(* Regression: a plain UPDATE was already checked (validate_update_unique) *)
(* and must stay checked — this fix must not have disturbed that path. *)
(* ------------------------------------------------------------------ *)

let plain_update_still_raises_on_unique () =
  with_db (fun db ->
    exec
      db
      "CREATE TABLE m (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, v INTEGER NOT NULL)";
    exec db "CREATE UNIQUE INDEX m_b ON m (b)";
    exec db "INSERT INTO m VALUES (1, 10, 20, 5)";
    exec db "INSERT INTO m VALUES (2, 11, 21, 6)";
    expect_error
      db
      ~needle:"UNIQUE constraint failed: m.b"
      "UPDATE m SET b = 21 WHERE id = 1")
;;

(* ------------------------------------------------------------------ *)
(* Review finding: check_index_unique_on_update's "unchanged" fast     *)
(* path compares only the indexed column values, never whether the     *)
(* row's membership in a PARTIAL unique index's WHERE predicate         *)
(* changed. A row that moves into a partial index's domain WITHOUT      *)
(* touching the indexed column skips the probe entirely — the "old_vs   *)
(* = new_vs" comparison says nothing moved, but the row is now a live   *)
(* member of an index it was previously exempt from.                    *)
(* ------------------------------------------------------------------ *)

let partial_index_membership_transition_upsert_do_update () =
  with_db (fun db ->
    exec db "CREATE TABLE p (id INTEGER PRIMARY KEY, a INTEGER, active INTEGER NOT NULL)";
    exec db "CREATE UNIQUE INDEX p_a ON p (a) WHERE active = 1";
    (* row 1: active, occupies key a=5 in the partial index. *)
    exec db "INSERT INTO p VALUES (1, 5, 1)";
    (* row 2: same a=5, but inactive -> exempt from the partial index. *)
    exec db "INSERT INTO p VALUES (2, 5, 0)";
    (* Upsert on row 2's own PK: DO UPDATE flips active 0 -> 1 without
       touching [a]. old_vs = new_vs = [5], so the "unchanged" fast path
       must not skip this — row 2 becomes a second live a=5 member. *)
    expect_error
      db
      ~needle:"UNIQUE constraint failed: p.a"
      "INSERT INTO p VALUES (2, 5, 1) ON CONFLICT(id) DO UPDATE SET active = 1";
    expect_rows
      db
      ~msg:"the conflicting DO UPDATE wrote nothing"
      [ "1|5|1"; "2|5|0" ]
      "SELECT * FROM p ORDER BY id")
;;

let partial_index_membership_transition_plain_update () =
  with_db (fun db ->
    exec db "CREATE TABLE p (id INTEGER PRIMARY KEY, a INTEGER, active INTEGER NOT NULL)";
    exec db "CREATE UNIQUE INDEX p_a ON p (a) WHERE active = 1";
    exec db "INSERT INTO p VALUES (1, 5, 1)";
    exec db "INSERT INTO p VALUES (2, 5, 0)";
    expect_error
      db
      ~needle:"UNIQUE constraint failed: p.a"
      "UPDATE p SET active = 1 WHERE id = 2")
;;

let () =
  Alcotest.run
    "upsert_unique_667"
    [ ( "secondary-index conflict shape"
      , [ Alcotest.test_case
            "DO UPDATE writing a duplicate into another UNIQUE index raises"
            `Quick
            secondary_conflict_do_update_raises_on_other_unique
        ; Alcotest.test_case
            "DO UPDATE not touching the colliding column succeeds"
            `Quick
            do_update_not_touching_unique_column_succeeds
        ; Alcotest.test_case
            "DO UPDATE to a free value succeeds"
            `Quick
            do_update_to_a_free_value_succeeds
        ] )
    ; ( "alias-PK conflict shape"
      , [ Alcotest.test_case
            "DO UPDATE writing a duplicate into another UNIQUE index raises"
            `Quick
            alias_pk_conflict_do_update_raises_on_other_unique
        ] )
    ; ( "regression"
      , [ Alcotest.test_case
            "a plain UPDATE still raises on UNIQUE"
            `Quick
            plain_update_still_raises_on_unique
        ] )
    ; ( "partial-index membership transition"
      , [ Alcotest.test_case
            "UPSERT DO UPDATE flipping a row into a partial index's WHERE raises"
            `Quick
            partial_index_membership_transition_upsert_do_update
        ; Alcotest.test_case
            "plain UPDATE flipping a row into a partial index's WHERE raises"
            `Quick
            partial_index_membership_transition_plain_update
        ] )
    ]
;;
