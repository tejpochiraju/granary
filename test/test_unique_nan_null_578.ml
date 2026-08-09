(** #578: NaN and NULL used to share the index-key byte [0x00], so a UNIQUE
    index over a nullable REAL column gave order-dependent answers for the
    same two rows:

    {v
      INSERT NULL; INSERT NaN;   ->  UNIQUE constraint failed  (wrong)
      INSERT NaN;  INSERT NULL;  ->  succeeds
    v}

    `any_null_val` (the NULL exemption) tests the VALUE and was always
    right — a [V_real nan] is correctly not exempted. `probe_unique_conflict`
    (nee `check_insert_unique`) then compared ENCODED BYTES, where NaN's key
    was byte-identical to NULL's, so a NaN insert spuriously conflicted with
    ANY pre-existing NULL — the first index entry, since a NULL/NaN key sorts
    to the very front.

    The fix is entirely in {!Granary_encoding.Index_key.encode_value}: NaN
    now gets its own single-byte tag [0x01], between NULL's [0x00] and
    INTEGER's [0x02]. That keeps every documented property from #536
    (NaN sorts below every number, both at the value level via
    [Exec.compare_values]'s [Float.compare] and at the index-key level) while
    making NaN and NULL byte-DIFFERENT, so `probe_unique_conflict` — and every
    other site that reads encoded bytes as an equality (the `CREATE UNIQUE
    INDEX` build, and `unique_violation_on_update`) — no longer confuses them.
    None of those call sites needed a code change; they all route uniqueness
    through {!Granary_encoding.Index_key.encode_value} already.

    NaN reaches the engine via a bound parameter — SQL here has no NaN
    literal, matching SQLite; `0.0/0.0` is refused by sema as a complex VALUES
    expression (see the issue's own repro notes). *)

open Lwt.Syntax
module Db = Granary.Db

let run = Lwt_main.run
let nan = Float.nan

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

let prepare db sql =
  run
    (let* r = Db.prepare db sql in
     match r with
     | Ok st -> Lwt.return st
     | Error e -> Alcotest.failf "prepare %S: %a" sql Db.pp_error e)
;;

let run_stmt db sql params =
  let st = prepare db sql in
  run (Db.run st ~params)
;;

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let expect_ok db sql params ~msg =
  match run_stmt db sql params with
  | Ok n -> Alcotest.(check int) (msg ^ " : wrote 1 row") 1 n
  | Error e -> Alcotest.failf "%s : expected success, got: %a" msg Db.pp_error e
;;

let expect_unique_violation db sql params ~table ~col ~msg =
  let want = Printf.sprintf "UNIQUE constraint failed: %s.%s" table col in
  match run_stmt db sql params with
  | Ok n -> Alcotest.failf "%s : expected UNIQUE violation, wrote %d rows" msg n
  | Error e ->
    let got = Format.asprintf "%a" Db.pp_error e in
    Alcotest.(check bool)
      (Printf.sprintf "%s : UNIQUE violation (got %S)" msg got)
      true
      (contains ~needle:want got)
;;

let query_reals db sql =
  run
    (let* r = Db.query db sql in
     match r with
     | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
     | Ok stream -> Lwt_stream.to_list stream)
  |> List.map (fun row -> row.(0))
;;

let render_r = function
  | Granary_encoding.Row.V_null -> "NULL"
  | Granary_encoding.Row.V_real f when Float.is_nan f -> "NaN"
  | Granary_encoding.Row.V_real f -> Printf.sprintf "%h" f
  | _ -> Alcotest.fail "unexpected value kind"
;;

let seed_unique_real db =
  exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, r REAL)";
  exec db "CREATE UNIQUE INDEX u ON t (r)"
;;

(* ------------------------------------------------------------------ *)
(* The issue's headline table, both orders                              *)
(* ------------------------------------------------------------------ *)

(* Order 1: NULL first, then NaN. On main this raised a spurious UNIQUE
   violation — the NaN key collided (byte-identical) with the NULL already in
   the index. *)
let null_then_nan_both_insert () =
  with_db (fun db ->
    seed_unique_real db;
    expect_ok db "INSERT INTO t (k, r) VALUES (1, ?)" [ Db.V_null ] ~msg:"NULL first";
    expect_ok db "INSERT INTO t (k, r) VALUES (2, ?)" [ Db.V_real nan ] ~msg:"NaN second";
    Alcotest.(check (list string))
      "both rows present"
      [ "NULL"; "NaN" ]
      (List.sort
         compare
         (List.map render_r (query_reals db "SELECT r FROM t ORDER BY k"))))
;;

(* Order 2: NaN first, then NULL — this direction already worked on main
   (the seek from a NaN insert never found the not-yet-inserted NULL), and
   must keep working. *)
let nan_then_null_both_insert () =
  with_db (fun db ->
    seed_unique_real db;
    expect_ok db "INSERT INTO t (k, r) VALUES (1, ?)" [ Db.V_real nan ] ~msg:"NaN first";
    expect_ok db "INSERT INTO t (k, r) VALUES (2, ?)" [ Db.V_null ] ~msg:"NULL second";
    Alcotest.(check (list string))
      "both rows present"
      [ "NULL"; "NaN" ]
      (List.sort
         compare
         (List.map render_r (query_reals db "SELECT r FROM t ORDER BY k"))))
;;

(* Control: two NaNs still conflict — defensible, since NaN = NaN everywhere
   else in the engine (DISTINCT, GROUP BY, hash-join keys). Must NOT regress:
   the fix is "give NaN its own tag", not "exempt NaN from UNIQUE". *)
let nan_then_nan_conflicts () =
  with_db (fun db ->
    seed_unique_real db;
    expect_ok db "INSERT INTO t (k, r) VALUES (1, ?)" [ Db.V_real nan ] ~msg:"first NaN";
    expect_unique_violation
      db
      "INSERT INTO t (k, r) VALUES (2, ?)"
      [ Db.V_real nan ]
      ~table:"t"
      ~col:"r"
      ~msg:"second NaN")
;;

(* Control: NaN then an ordinary real both insert fine. *)
let nan_then_real_number_both_insert () =
  with_db (fun db ->
    seed_unique_real db;
    expect_ok db "INSERT INTO t (k, r) VALUES (1, ?)" [ Db.V_real nan ] ~msg:"NaN";
    expect_ok db "INSERT INTO t (k, r) VALUES (2, ?)" [ Db.V_real 1.0 ] ~msg:"1.0")
;;

(* Control: NULL then NULL both insert fine — the exemption itself, unaffected
   by this fix. *)
let null_then_null_both_insert () =
  with_db (fun db ->
    seed_unique_real db;
    expect_ok db "INSERT INTO t (k, r) VALUES (1, ?)" [ Db.V_null ] ~msg:"first NULL";
    expect_ok db "INSERT INTO t (k, r) VALUES (2, ?)" [ Db.V_null ] ~msg:"second NULL")
;;

(* ------------------------------------------------------------------ *)
(* The other two enforcement sites the issue names                      *)
(* ------------------------------------------------------------------ *)

(* CREATE UNIQUE INDEX's own build-time duplicate probe (exec.ml ~4600-4640)
   uses the identical byte-prefix mechanism. Built over a table that already
   holds one NULL and one NaN, it must succeed, not report a false
   duplicate. *)
let create_unique_index_over_existing_null_and_nan () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, r REAL)";
    let st = prepare db "INSERT INTO t (k, r) VALUES (?, ?)" in
    (match run (Db.run st ~params:[ Db.V_int 1L; Db.V_null ]) with
     | Ok _ -> ()
     | Error e -> Alcotest.failf "seed NULL row: %a" Db.pp_error e);
    (match run (Db.run st ~params:[ Db.V_int 2L; Db.V_real nan ]) with
     | Ok _ -> ()
     | Error e -> Alcotest.failf "seed NaN row: %a" Db.pp_error e);
    exec db "CREATE UNIQUE INDEX u ON t (r)")
;;

(* [unique_violation_on_update] (the plain-UPDATE pre-pass) uses its own probe
   over the same encoded bytes with the same collision. Updating one row's
   column to NaN while a NULL already sits in the index must succeed; updating
   a second row's column to NaN afterwards must then conflict with the first
   NaN (not with the NULL). *)
let update_to_nan_does_not_collide_with_existing_null () =
  with_db (fun db ->
    seed_unique_real db;
    let st = prepare db "INSERT INTO t (k, r) VALUES (?, ?)" in
    (match run (Db.run st ~params:[ Db.V_int 1L; Db.V_null ]) with
     | Ok _ -> ()
     | Error e -> Alcotest.failf "seed NULL row: %a" Db.pp_error e);
    (match run (Db.run st ~params:[ Db.V_int 2L; Db.V_real 5.0 ]) with
     | Ok _ -> ()
     | Error e -> Alcotest.failf "seed real row: %a" Db.pp_error e);
    (match run (Db.run st ~params:[ Db.V_int 3L; Db.V_real 6.0 ]) with
     | Ok _ -> ()
     | Error e -> Alcotest.failf "seed second real row: %a" Db.pp_error e);
    expect_ok
      db
      "UPDATE t SET r = ? WHERE k = 2"
      [ Db.V_real nan ]
      ~msg:"UPDATE to NaN, NULL present";
    expect_unique_violation
      db
      "UPDATE t SET r = ? WHERE k = 3"
      [ Db.V_real nan ]
      ~table:"t"
      ~col:"r"
      ~msg:"second UPDATE to NaN conflicts with the first")
;;

(* ------------------------------------------------------------------ *)
(* Composite index: the issue notes [unique_violation_on_update]'s prefix is
   only the LEADING column, so a leading NaN column collides the same way. *)
(* ------------------------------------------------------------------ *)

let composite_index_leading_nan_column () =
  with_db (fun db ->
    exec db "CREATE TABLE c (k INTEGER PRIMARY KEY, r REAL, s TEXT)";
    exec db "CREATE UNIQUE INDEX cu ON c (r, s)";
    let st = prepare db "INSERT INTO c (k, r, s) VALUES (?, ?, ?)" in
    (match run (Db.run st ~params:[ Db.V_int 1L; Db.V_null; Db.V_text "a" ]) with
     | Ok _ -> ()
     | Error e -> Alcotest.failf "seed NULL row: %a" Db.pp_error e);
    (* A leading NaN with any trailing value must insert, not collide with
       the leading-NULL row above. *)
    match run (Db.run st ~params:[ Db.V_int 2L; Db.V_real nan; Db.V_text "b" ]) with
    | Ok n -> Alcotest.(check int) "leading NaN inserts" 1 n
    | Error e -> Alcotest.failf "leading NaN insert: %a" Db.pp_error e)
;;

(* ------------------------------------------------------------------ *)
(* Ordering regression: NaN must still sort below every real number in an
   index range scan — #578's fix must not disturb #536's decided order. *)
(* ------------------------------------------------------------------ *)

let nan_still_sorts_below_every_number_in_a_range_scan () =
  with_db (fun db ->
    exec db "CREATE TABLE r (k INTEGER PRIMARY KEY, v REAL)";
    exec db "CREATE INDEX ri ON r (v)";
    exec db "BEGIN";
    for i = 1 to 20 do
      exec db (Printf.sprintf "INSERT INTO r (k, v) VALUES (%d, %d.0)" i i)
    done;
    exec db "COMMIT";
    let st = prepare db "INSERT INTO r (k, v) VALUES (100, ?)" in
    (match run (Db.run st ~params:[ Db.V_real nan ]) with
     | Ok _ -> ()
     | Error e -> Alcotest.failf "seed NaN row: %a" Db.pp_error e);
    (* A NaN lower bound admits every row (NaN sorts below every number, so
       [v >= NaN] is trivially satisfied by every real too, matching #536's
       decided divergence from SQLite). *)
    let ge =
      run
        (let* r = Db.prepare db "SELECT v FROM r WHERE v >= ?" in
         match r with
         | Error e -> Alcotest.failf "prepare: %a" Db.pp_error e
         | Ok st ->
           let* r = Db.iter st ~params:[ Db.V_real nan ] in
           (match r with
            | Error e -> Alcotest.failf "iter: %a" Db.pp_error e
            | Ok stream -> Lwt_stream.to_list stream))
    in
    Alcotest.(check int) "NaN lower bound admits every row" 21 (List.length ge);
    (* A NaN upper bound admits only the NaN row itself. *)
    let le =
      run
        (let* r = Db.prepare db "SELECT v FROM r WHERE v <= ?" in
         match r with
         | Error e -> Alcotest.failf "prepare: %a" Db.pp_error e
         | Ok st ->
           let* r = Db.iter st ~params:[ Db.V_real nan ] in
           (match r with
            | Error e -> Alcotest.failf "iter: %a" Db.pp_error e
            | Ok stream -> Lwt_stream.to_list stream))
    in
    Alcotest.(check int) "NaN upper bound admits only NaN" 1 (List.length le))
;;

let () =
  Alcotest.run
    "unique_nan_null_578"
    [ ( "headline"
      , [ Alcotest.test_case
            "NULL then NaN both insert (was: spurious UNIQUE violation)"
            `Quick
            null_then_nan_both_insert
        ; Alcotest.test_case
            "NaN then NULL both insert (was already correct)"
            `Quick
            nan_then_null_both_insert
        ] )
    ; ( "controls"
      , [ Alcotest.test_case "NaN then NaN still conflicts" `Quick nan_then_nan_conflicts
        ; Alcotest.test_case
            "NaN then a real number both insert"
            `Quick
            nan_then_real_number_both_insert
        ; Alcotest.test_case
            "NULL then NULL both insert"
            `Quick
            null_then_null_both_insert
        ] )
    ; ( "other-enforcement-sites"
      , [ Alcotest.test_case
            "CREATE UNIQUE INDEX build over existing NULL+NaN"
            `Quick
            create_unique_index_over_existing_null_and_nan
        ; Alcotest.test_case
            "UPDATE to NaN does not collide with an existing NULL"
            `Quick
            update_to_nan_does_not_collide_with_existing_null
        ; Alcotest.test_case
            "composite index, leading NaN column"
            `Quick
            composite_index_leading_nan_column
        ] )
    ; ( "ordering-regression"
      , [ Alcotest.test_case
            "NaN still sorts below every number in a range scan"
            `Quick
            nan_still_sorts_below_every_number_in_a_range_scan
        ] )
    ]
;;
