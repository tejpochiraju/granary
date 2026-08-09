(** #693: an ON UPDATE CASCADE / SET NULL / SET DEFAULT write goes through
    [Exec.update_col_in_tx], which calls [Exec.write_row_rekeyed] directly
    with no uniqueness probe at all — unlike plain UPDATE
    ([validate_update_unique]) and UPSERT ... DO UPDATE
    ([execute_upsert_update]'s #667/PR #692 pass). Before this fix, a
    cascade that pushed a value into a UNIQUE child column another row
    already held was accepted silently, corrupting the index the same way
    #667 did for the upsert path.

    The fix runs the same shared [Exec.check_indexes_unique_on_update] helper
    from [update_col_in_tx] before [write_row_rekeyed] moves the row, mirroring
    the pattern at the other two call sites exactly.

    SET NULL cascades can never trip this: #290 exempts a NULL-containing key
    from the UNIQUE probe. SET DEFAULT can, when the column's default value
    collides with a value already held by another live row — pinned here
    alongside the issue's own CASCADE repro. *)

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
(* The issue's own repro: ON UPDATE CASCADE pushing a value into a      *)
(* UNIQUE child column another row already holds must raise.           *)
(* ------------------------------------------------------------------ *)

let cascade_raises_on_unique_child_collision () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE parent (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE child (id INTEGER PRIMARY KEY, parent_id INTEGER, UNIQUE \
       (parent_id), FOREIGN KEY (parent_id) REFERENCES parent(id) ON UPDATE CASCADE)";
    exec db "INSERT INTO parent VALUES (1)";
    exec db "INSERT INTO parent VALUES (2)";
    exec db "INSERT INTO child VALUES (1, 1)";
    exec db "INSERT INTO child VALUES (2, 2)";
    (* Renumbering parent 1's id to 2 cascades child row 1's parent_id to 2,
       colliding with child row 2's existing parent_id = 2 under the UNIQUE
       constraint. *)
    expect_error
      db
      ~needle:"UNIQUE constraint failed: child.parent_id"
      "UPDATE parent SET id = 2 WHERE id = 1";
    expect_rows
      db
      ~msg:"nothing moved: the cascade wrote nothing"
      [ "1|1"; "2|2" ]
      "SELECT * FROM child ORDER BY id";
    expect_rows
      db
      ~msg:"the parent update itself was rolled back too"
      [ "1"; "2" ]
      "SELECT * FROM parent ORDER BY id")
;;

(* A cascade that moves the child's UNIQUE column to a value nothing else
   holds must still succeed. *)
let cascade_to_a_free_value_succeeds () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE parent (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE child (id INTEGER PRIMARY KEY, parent_id INTEGER, UNIQUE \
       (parent_id), FOREIGN KEY (parent_id) REFERENCES parent(id) ON UPDATE CASCADE)";
    exec db "INSERT INTO parent VALUES (1)";
    exec db "INSERT INTO parent VALUES (2)";
    exec db "INSERT INTO child VALUES (1, 1)";
    exec db "INSERT INTO child VALUES (2, 2)";
    exec db "UPDATE parent SET id = 3 WHERE id = 1";
    expect_rows
      db
      ~msg:"the cascade moved parent_id to the free value 3"
      [ "1|3"; "2|2" ]
      "SELECT * FROM child ORDER BY id")
;;

(* ------------------------------------------------------------------ *)
(* ON UPDATE SET DEFAULT: the default value can collide with another   *)
(* live row's value in a UNIQUE child column.                          *)
(* ------------------------------------------------------------------ *)

let set_default_raises_on_unique_child_collision () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE parent (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE child (id INTEGER PRIMARY KEY, parent_id INTEGER DEFAULT 9, UNIQUE \
       (parent_id), FOREIGN KEY (parent_id) REFERENCES parent(id) ON UPDATE SET DEFAULT)";
    exec db "INSERT INTO parent VALUES (1)";
    exec db "INSERT INTO parent VALUES (9)";
    exec db "INSERT INTO child VALUES (1, 1)";
    (* child row 2 already occupies the default value 9. *)
    exec db "INSERT INTO child VALUES (2, 9)";
    (* Changing parent 1's id triggers SET DEFAULT on child row 1's
       parent_id, setting it to 9 - colliding with child row 2. *)
    expect_error
      db
      ~needle:"UNIQUE constraint failed: child.parent_id"
      "UPDATE parent SET id = 5 WHERE id = 1";
    expect_rows
      db
      ~msg:"nothing moved: the cascade wrote nothing"
      [ "1|1"; "2|9" ]
      "SELECT * FROM child ORDER BY id")
;;

(* ------------------------------------------------------------------ *)
(* ON UPDATE SET NULL can never trip the probe: #290 exempts any        *)
(* NULL-containing key from a UNIQUE index, so this is a regression     *)
(* check that the new probe doesn't spuriously fire on it.              *)
(* ------------------------------------------------------------------ *)

let set_null_never_raises_on_unique () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE parent (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE child (id INTEGER PRIMARY KEY, parent_id INTEGER, UNIQUE \
       (parent_id), FOREIGN KEY (parent_id) REFERENCES parent(id) ON UPDATE SET NULL)";
    exec db "INSERT INTO parent VALUES (1)";
    exec db "INSERT INTO parent VALUES (2)";
    exec db "INSERT INTO child VALUES (1, 1)";
    exec db "INSERT INTO child VALUES (2, 2)";
    exec db "UPDATE parent SET id = 3 WHERE id = 1";
    exec db "UPDATE parent SET id = 4 WHERE id = 2";
    expect_rows
      db
      ~msg:"both children set to NULL without a spurious UNIQUE error"
      [ "1|<null>"; "2|<null>" ]
      "SELECT * FROM child ORDER BY id")
;;

(* ------------------------------------------------------------------ *)
(* Regression: plain UPDATE and UPSERT DO UPDATE unique probes stay in *)
(* place — this fix must not have disturbed either.                   *)
(* ------------------------------------------------------------------ *)

let plain_update_still_raises_on_unique () =
  with_db (fun db ->
    exec db "CREATE TABLE m (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, UNIQUE (b))";
    exec db "INSERT INTO m VALUES (1, 10, 20)";
    exec db "INSERT INTO m VALUES (2, 11, 21)";
    expect_error
      db
      ~needle:"UNIQUE constraint failed: m.b"
      "UPDATE m SET b = 21 WHERE id = 1")
;;

let upsert_do_update_still_raises_on_unique () =
  with_db (fun db ->
    exec
      db
      "CREATE TABLE m (id INTEGER PRIMARY KEY, a INTEGER, b INTEGER, UNIQUE (a), UNIQUE \
       (b))";
    exec db "INSERT INTO m VALUES (1, 10, 20)";
    exec db "INSERT INTO m VALUES (2, 11, 21)";
    expect_error
      db
      ~needle:"UNIQUE constraint failed: m.b"
      "INSERT INTO m VALUES (3, 10, 21) ON CONFLICT(a) DO UPDATE SET b = excluded.b")
;;

let () =
  Alcotest.run
    "cascade_unique_693"
    [ ( "ON UPDATE CASCADE"
      , [ Alcotest.test_case
            "a cascade writing a duplicate into a UNIQUE child column raises"
            `Quick
            cascade_raises_on_unique_child_collision
        ; Alcotest.test_case
            "a cascade to a free value succeeds"
            `Quick
            cascade_to_a_free_value_succeeds
        ] )
    ; ( "ON UPDATE SET DEFAULT"
      , [ Alcotest.test_case
            "a SET DEFAULT collision with a UNIQUE child column raises"
            `Quick
            set_default_raises_on_unique_child_collision
        ] )
    ; ( "ON UPDATE SET NULL"
      , [ Alcotest.test_case
            "SET NULL never spuriously raises on a UNIQUE child column"
            `Quick
            set_null_never_raises_on_unique
        ] )
    ; ( "regression"
      , [ Alcotest.test_case
            "a plain UPDATE still raises on UNIQUE"
            `Quick
            plain_update_still_raises_on_unique
        ; Alcotest.test_case
            "an UPSERT DO UPDATE still raises on UNIQUE"
            `Quick
            upsert_do_update_still_raises_on_unique
        ] )
    ]
;;
