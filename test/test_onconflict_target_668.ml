(** #668: an ON CONFLICT target naming no PRIMARY KEY or UNIQUE constraint is
    now a bind-time error, matching sqlite3's
    "ON CONFLICT clause does not match any PRIMARY KEY or UNIQUE constraint".

    Pre-existing divergence, filed in re-review of PR #652: [Exec.
    index_is_conflict_target] already requires [idx_unique], so a non-unique
    index could never be *promoted* into a target at execution time, but
    nothing rejected the statement when the named column list matched no
    constraint at all — it silently fell back to a plain INSERT and the DO
    UPDATE clause was dropped.

    The fix, [Sema.conflict_target_matches_constraint], resolves the target
    against the table's PRIMARY KEY columns (composite [WITHOUT ROWID] keys
    and the rowid-alias `INTEGER PRIMARY KEY` column both carry
    [primary_key = true] since #530, with no [Cat.index_info] of their own)
    and its UNIQUE indexes, using the same column-set-equality test
    [Exec.index_is_conflict_target] uses at runtime ([idx_where_sql] is not
    consulted by either, so a partial unique index is still a valid target —
    consistent with the pre-existing runtime behaviour this issue's scope note
    says not to touch). *)

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

(* [Db.execute] runs the statement through [Sema] first, so a bind-time
   rejection surfaces here as [Error], not as an exception or a silently
   dropped clause. *)
let expect_bind_error db sql ~needle =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "%S was expected to fail to bind" sql
  | Error e ->
    let msg = Format.asprintf "%a" Db.pp_error e in
    Alcotest.(check bool) (Printf.sprintf "%S -> %S" sql msg) true (contains ~needle msg)
;;

let needle = "ON CONFLICT clause does not match any PRIMARY KEY or UNIQUE constraint"

(* The issue's exact case: a conflict target naming a column covered only by a
   non-unique index. *)
let target_on_non_unique_index_is_rejected () =
  with_db (fun db ->
    exec db "CREATE TABLE n (id INTEGER PRIMARY KEY, a INTEGER, v INTEGER NOT NULL)";
    exec db "CREATE INDEX n_a ON n (a)";
    expect_bind_error
      db
      "INSERT INTO n VALUES (2, 10, 9) ON CONFLICT(a) DO UPDATE SET v = 42"
      ~needle)
;;

(* A target naming a column that carries no index at all. *)
let target_on_unindexed_column_is_rejected () =
  with_db (fun db ->
    exec db "CREATE TABLE n (id INTEGER PRIMARY KEY, a INTEGER, v INTEGER NOT NULL)";
    expect_bind_error
      db
      "INSERT INTO n VALUES (2, 10, 9) ON CONFLICT(a) DO UPDATE SET v = 42"
      ~needle)
;;

(* A target naming a column that does not exist in the table at all. *)
let target_on_nonexistent_column_is_rejected () =
  with_db (fun db ->
    exec db "CREATE TABLE n (id INTEGER PRIMARY KEY, a INTEGER, v INTEGER NOT NULL)";
    expect_bind_error
      db
      "INSERT INTO n VALUES (2, 10, 9) ON CONFLICT(nope) DO UPDATE SET v = 42"
      ~needle)
;;

(* A target naming a multi-column list where the columns exist but the set as
   a whole matches no PK or UNIQUE constraint. *)
let target_on_wrong_column_set_is_rejected () =
  with_db (fun db ->
    exec db "CREATE TABLE m (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, v INTEGER)";
    exec db "CREATE UNIQUE INDEX m_ab ON m (a, b)";
    expect_bind_error
      db
      "INSERT INTO m VALUES (2, 10, 20, 9) ON CONFLICT(a) DO UPDATE SET v = 42"
      ~needle)
;;

(* Regression: a target naming the PRIMARY KEY (rowid alias) still upserts. *)
let target_on_primary_key_still_upserts () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER NOT NULL)";
    exec db "INSERT INTO t VALUES (1, 10)";
    exec db "INSERT INTO t VALUES (1, 99) ON CONFLICT(id) DO UPDATE SET v = 42";
    expect_rows db ~msg:"upserted via PK target" [ "1|42" ] "SELECT * FROM t")
;;

(* Regression: a target naming a secondary UNIQUE index still upserts. *)
let target_on_unique_index_still_upserts () =
  with_db (fun db ->
    exec db "CREATE TABLE u (id INTEGER PRIMARY KEY, a INTEGER, v INTEGER NOT NULL)";
    exec db "CREATE UNIQUE INDEX u_a ON u (a)";
    exec db "INSERT INTO u VALUES (1, 10, 5)";
    exec db "INSERT INTO u VALUES (2, 10, 9) ON CONFLICT(a) DO UPDATE SET v = 42";
    expect_rows db ~msg:"upserted via unique-index target" [ "1|10|42" ] "SELECT * FROM u")
;;

(* Regression: a target naming a composite UNIQUE index still upserts, as a
   column-set (order-independent) match — the target list need not repeat the
   index's own column order.

   (Composite PRIMARY KEY is not exercised here: WITHOUT ROWID tables in this
   engine only support a single PRIMARY KEY column today — "phase 37" per the
   bind-time error — so the composite case is covered via a UNIQUE index
   instead.) *)
let target_on_composite_unique_index_still_upserts () =
  with_db (fun db ->
    exec db "CREATE TABLE w (a INTEGER, b INTEGER, v INTEGER NOT NULL)";
    exec db "CREATE UNIQUE INDEX w_ab ON w (a, b)";
    exec db "INSERT INTO w VALUES (1, 2, 5)";
    exec db "INSERT INTO w VALUES (1, 2, 9) ON CONFLICT(b, a) DO UPDATE SET v = 42";
    expect_rows
      db
      ~msg:"upserted via order-independent composite unique-index target"
      [ "1|2|42" ]
      "SELECT * FROM w")
;;

let () =
  Alcotest.run
    "onconflict_target_668"
    [ ( "668-rejected"
      , [ Alcotest.test_case
            "target on a non-unique index is rejected"
            `Quick
            target_on_non_unique_index_is_rejected
        ; Alcotest.test_case
            "target on an unindexed column is rejected"
            `Quick
            target_on_unindexed_column_is_rejected
        ; Alcotest.test_case
            "target on a nonexistent column is rejected"
            `Quick
            target_on_nonexistent_column_is_rejected
        ; Alcotest.test_case
            "target on the wrong column set is rejected"
            `Quick
            target_on_wrong_column_set_is_rejected
        ] )
    ; ( "668-regression"
      , [ Alcotest.test_case
            "target on the primary key still upserts"
            `Quick
            target_on_primary_key_still_upserts
        ; Alcotest.test_case
            "target on a unique index still upserts"
            `Quick
            target_on_unique_index_still_upserts
        ; Alcotest.test_case
            "target on a composite unique index still upserts"
            `Quick
            target_on_composite_unique_index_still_upserts
        ] )
    ]
;;
