(** #629: NOT NULL on a GENERATED column rejected every INSERT, including the
    ones whose generated expression yields a perfectly good non-NULL value.

    {v
      CREATE TABLE g (k INTEGER PRIMARY KEY, v INTEGER,
                      w INTEGER NOT NULL GENERATED ALWAYS AS (v + 1) STORED);
      INSERT INTO g (k, v) VALUES (1, 1);   -- NOT NULL violation: w
    v}

    The table was {b uninsertable}: writing a generated column explicitly is
    refused ([bind_explicit_insert_cols]), so [w] is always omitted, and
    [Sema.bind_insert_row] fills an omitted column with a [BE_lit L_null]
    placeholder and then ran its static NOT NULL check over that placeholder —
    before [compute_stored_generated_cols] had produced the real value. There
    was no spelling of the statement that could succeed.

    This is the third instance of the same shape, and the reason the fix is
    stated as {i when} rather than {i whether}: #567 (NOT NULL enforced too
    late to see a parameter), #599 (OR IGNORE honoured on one level and not the
    other), and now a check running before the value it judges exists. Neither
    enforcement chokepoint is relaxed:

    - [Sema.bind_insert_row] exempts generated columns of BOTH storage classes,
      because at bind time neither has a value. It keeps its literal-NULL error
      for every other column, which stays the earlier and better-located one.
    - [Exec.not_null_violation] now recomputes VIRTUAL generated columns into a
      copy of the row before judging it, instead of exempting them wholesale as
      #567 did. A STORED column was already materialised before the check ran.

    So the enforcement moved to where the computed value exists rather than
    disappearing, and the negative cases below are what prove it: a generated
    expression that genuinely evaluates to NULL on a NOT NULL column is still
    rejected — on INSERT, on UPDATE of the base column it reads, and for both
    storage classes. Under [OR IGNORE] it is skipped instead, per #599's
    decided semantics, because that decision lives at the runtime call site and
    the row now reaches it. *)

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

let open_db path =
  match run (Granary_unix.open_file ~path ()) with
  | Ok db -> db
  | Error e -> Alcotest.failf "open %s: %a" path Db.pp_error e
;;

let expect_error db sql ~needle =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "%S was expected to fail" sql
  | Error e ->
    let msg = Format.asprintf "%a" Db.pp_error e in
    Alcotest.(check bool) (Printf.sprintf "%S -> %S" sql msg) true (contains ~needle msg)
;;

(* The two shapes under test, spelled exactly as the issue does. *)
let create_stored db =
  exec
    db
    "CREATE TABLE g (k INTEGER PRIMARY KEY, v INTEGER, w INTEGER NOT NULL GENERATED \
     ALWAYS AS (v + 1) STORED)"
;;

let create_virtual db =
  exec
    db
    "CREATE TABLE gv (k INTEGER PRIMARY KEY, v INTEGER, w INTEGER NOT NULL GENERATED \
     ALWAYS AS (v + 1) VIRTUAL)"
;;

(* ------------------------------------------------------------------ *)
(* The bug: a valid INSERT must succeed                                 *)
(* ------------------------------------------------------------------ *)

(* The issue's exact repro. Before #629 this raised "NOT NULL violation: w"
   from the binder, having judged the placeholder rather than [v + 1]. *)
let stored_generated_not_null_accepts_a_good_row () =
  with_db (fun db ->
    create_stored db;
    exec db "INSERT INTO g (k, v) VALUES (1, 1)";
    Alcotest.(check (list string))
      "the computed value is stored"
      [ "1|1|2" ]
      (texts db "SELECT k, v, w FROM g"))
;;

let virtual_generated_not_null_accepts_a_good_row () =
  with_db (fun db ->
    create_virtual db;
    exec db "INSERT INTO gv (k, v) VALUES (1, 1)";
    Alcotest.(check (list string))
      "the recomputed value is returned"
      [ "1|1|2" ]
      (texts db "SELECT k, v, w FROM gv"))
;;

(* The column list may be omitted only if the table has no generated column, so
   the reachable spellings are the ones that name the base columns.  Both must
   work, and a multi-row VALUES list must not be an exception — the static check
   fired per STATEMENT, so before the fix one bad row was never the issue: every
   row died. *)
let every_reachable_insert_spelling_works () =
  with_db (fun db ->
    create_stored db;
    exec db "INSERT INTO g (k, v) VALUES (1, 10), (2, 20), (3, 30)";
    Alcotest.(check int) "three rows written" 3 (List.length (texts db "SELECT * FROM g"));
    (* parameter spelling *)
    (match
       run_stmt db "INSERT INTO g (k, v) VALUES (?, ?)" [ Db.V_int 4L; Db.V_int 40L ]
     with
     | Ok n -> Alcotest.(check int) "parameter spelling writes one row" 1 n
     | Error e -> Alcotest.failf "parameter INSERT: %a" Db.pp_error e);
    (* INSERT ... SELECT spelling *)
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO src VALUES (5, 50)";
    exec db "INSERT INTO g (k, v) SELECT k, v FROM src";
    Alcotest.(check (list string))
      "every spelling computed w"
      [ "1|10|11"; "2|20|21"; "3|30|31"; "4|40|41"; "5|50|51" ]
      (texts db "SELECT k, v, w FROM g ORDER BY k"))
;;

(* Writing the generated column is still refused — the fix exempts it from the
   NOT NULL check, it does not make it assignable. *)
let generated_column_is_still_unassignable () =
  with_db (fun db ->
    create_stored db;
    expect_error
      db
      "INSERT INTO g (k, v, w) VALUES (1, 1, 2)"
      ~needle:"cannot INSERT into generated column";
    exec db "INSERT INTO g (k, v) VALUES (1, 1)";
    expect_error db "UPDATE g SET w = 99" ~needle:"cannot UPDATE generated column")
;;

(* ------------------------------------------------------------------ *)
(* Enforcement moved, not dropped                                       *)
(* ------------------------------------------------------------------ *)

(* [v] is nullable, so nothing static objects; [v + 1] then evaluates to NULL
   and the NOT NULL on [w] must bite — at the runtime site, with the runtime
   message ("NOT NULL constraint failed: g.w"), not the binder's. *)
let stored_generated_null_value_is_rejected () =
  with_db (fun db ->
    create_stored db;
    expect_error
      db
      "INSERT INTO g (k, v) VALUES (1, NULL)"
      ~needle:"NOT NULL constraint failed: g.w";
    Alcotest.(check (list string)) "nothing written" [] (texts db "SELECT * FROM g"))
;;

(* The VIRTUAL half is the one #567's blanket exemption made unenforceable: the
   stored cell is NULL by design, so exempting it meant never checking it.
   [not_null_violation] recomputes it now. *)
let virtual_generated_null_value_is_rejected () =
  with_db (fun db ->
    create_virtual db;
    expect_error
      db
      "INSERT INTO gv (k, v) VALUES (1, NULL)"
      ~needle:"NOT NULL constraint failed: gv.w";
    Alcotest.(check (list string)) "nothing written" [] (texts db "SELECT * FROM gv"))
;;

(* The bound-parameter spelling reaches the runtime check with [Sema] having no
   opinion at all, so it pins the runtime half on its own. *)
let parameter_null_is_rejected_on_both_storage_classes () =
  with_db (fun db ->
    create_stored db;
    create_virtual db;
    List.iter
      (fun (sql, table) ->
         match run_stmt db sql [ Db.V_int 1L; Db.V_null ] with
         | Ok _ -> Alcotest.failf "%S was expected to fail" sql
         | Error e ->
           let msg = Format.asprintf "%a" Db.pp_error e in
           let needle = Printf.sprintf "NOT NULL constraint failed: %s.w" table in
           Alcotest.(check bool)
             (Printf.sprintf "%S -> %S" sql msg)
             true
             (contains ~needle msg))
      [ "INSERT INTO g (k, v) VALUES (?, ?)", "g"
      ; "INSERT INTO gv (k, v) VALUES (?, ?)", "gv"
      ])
;;

(* An UPDATE of the BASE column is the other way a generated value turns NULL.
   It funnels through [write_row_rekeyed] -> [enforce_not_null], which has no
   [OR IGNORE] form to consult and therefore always raises. The row must be
   unchanged afterwards. *)
let update_that_nulls_the_generated_value_is_rejected () =
  with_db (fun db ->
    create_stored db;
    create_virtual db;
    exec db "INSERT INTO g (k, v) VALUES (1, 1)";
    exec db "INSERT INTO gv (k, v) VALUES (1, 1)";
    expect_error db "UPDATE g SET v = NULL" ~needle:"NOT NULL constraint failed: g.w";
    expect_error db "UPDATE gv SET v = NULL" ~needle:"NOT NULL constraint failed: gv.w";
    Alcotest.(check (list string))
      "STORED row untouched"
      [ "1|1|2" ]
      (texts db "SELECT k, v, w FROM g");
    Alcotest.(check (list string))
      "VIRTUAL row untouched"
      [ "1|1|2" ]
      (texts db "SELECT k, v, w FROM gv");
    (* An UPDATE that keeps it non-NULL still works. *)
    exec db "UPDATE g SET v = 7";
    exec db "UPDATE gv SET v = 7";
    Alcotest.(check (list string))
      "both recomputed"
      [ "1|7|8" ]
      (texts db "SELECT k, v, w FROM g");
    Alcotest.(check (list string))
      "both recomputed"
      [ "1|7|8" ]
      (texts db "SELECT k, v, w FROM gv"))
;;

(* ------------------------------------------------------------------ *)
(* #599 semantics, now that the row reaches the site that decides them  *)
(* ------------------------------------------------------------------ *)

(* [OR IGNORE] skips the offending row and keeps the rest, exactly as it does
   for a NOT NULL base column. This only works because the binder stopped
   pre-empting the runtime check — a per-STATEMENT static rejection would have
   lost the good rows too. *)
let or_ignore_skips_only_the_bad_row () =
  with_db (fun db ->
    create_stored db;
    create_virtual db;
    exec db "INSERT OR IGNORE INTO g (k, v) VALUES (1, 10), (2, NULL), (3, 30)";
    Alcotest.(check (list string))
      "the NULL-generating row is skipped, the others kept"
      [ "1|10|11"; "3|30|31" ]
      (texts db "SELECT k, v, w FROM g ORDER BY k");
    exec db "INSERT OR IGNORE INTO gv (k, v) VALUES (1, 10), (2, NULL), (3, 30)";
    Alcotest.(check (list string))
      "same on the VIRTUAL shape"
      [ "1|10|11"; "3|30|31" ]
      (texts db "SELECT k, v, w FROM gv ORDER BY k");
    (* and the parameter spelling reports 0 rows written rather than raising *)
    match
      run_stmt
        db
        "INSERT OR IGNORE INTO g (k, v) VALUES (?, ?)"
        [ Db.V_int 9L; Db.V_null ]
    with
    | Ok n -> Alcotest.(check int) "0 rows written" 0 n
    | Error e -> Alcotest.failf "OR IGNORE was expected to skip: %a" Db.pp_error e)
;;

(* Every other resolution raises, per #599 — including [OR REPLACE], granary's
   deliberate divergence from SQLite. *)
let other_resolutions_still_raise () =
  with_db (fun db ->
    create_stored db;
    List.iter
      (fun kw ->
         expect_error
           db
           (Printf.sprintf "INSERT %sINTO g (k, v) VALUES (1, NULL)" kw)
           ~needle:"NOT NULL constraint failed: g.w")
      [ ""; "OR ABORT "; "OR FAIL "; "OR ROLLBACK "; "OR REPLACE " ];
    Alcotest.(check (list string)) "nothing written" [] (texts db "SELECT * FROM g"))
;;

(* ------------------------------------------------------------------ *)
(* The narrowing is only about generated columns                        *)
(* ------------------------------------------------------------------ *)

(* The binder's static check is untouched for every other column: a literal
   NULL into a plain NOT NULL column still fails EARLY, with [Sema]'s message
   ("NOT NULL violation: a") rather than the runtime one. If this starts
   reporting "NOT NULL constraint failed", the exemption has been widened past
   generated columns. *)
let plain_not_null_still_a_bind_error () =
  with_db (fun db ->
    exec db "CREATE TABLE h (a INTEGER NOT NULL, b INTEGER)";
    expect_error
      db
      "INSERT INTO h (a, b) VALUES (NULL, 1)"
      ~needle:"NOT NULL violation: a";
    (* An omitted NOT NULL column with no DEFAULT is still a bind error too. *)
    expect_error db "INSERT INTO h (b) VALUES (1)" ~needle:"NOT NULL violation: a")
;;

(* A nullable generated column keeps working — the exemption must not have
   turned into an implicit NOT NULL, nor broken the ordinary read path. *)
let nullable_generated_columns_unaffected () =
  with_db (fun db ->
    exec
      db
      "CREATE TABLE n (k INTEGER PRIMARY KEY, v INTEGER, s INTEGER GENERATED ALWAYS AS \
       (v + 1) STORED, t INTEGER GENERATED ALWAYS AS (v * 2) VIRTUAL)";
    exec db "INSERT INTO n (k, v) VALUES (1, 3)";
    exec db "INSERT INTO n (k, v) VALUES (2, NULL)";
    Alcotest.(check (list string))
      "NULL generated values are fine when the column is nullable"
      [ "1|3|4|6"; "2|<null>|<null>|<null>" ]
      (texts db "SELECT k, v, s, t FROM n ORDER BY k"))
;;

(* A NOT NULL generated column whose expression cannot be NULL is insertable
   even when the base column is: the check judges the value, not the input. *)
let generated_expression_that_cannot_be_null () =
  with_db (fun db ->
    exec
      db
      "CREATE TABLE c (k INTEGER PRIMARY KEY, v INTEGER, w INTEGER NOT NULL GENERATED \
       ALWAYS AS (coalesce(v, 0)) STORED)";
    exec db "INSERT INTO c (k, v) VALUES (1, 5)";
    exec db "INSERT INTO c (k, v) VALUES (2, NULL)";
    Alcotest.(check (list string))
      "coalesce keeps it non-NULL for both rows"
      [ "1|5|5"; "2|<null>|0" ]
      (texts db "SELECT k, v, w FROM c ORDER BY k"))
;;

(* Persistence: the STORED value on disk is the computed one, and a reopened
   database recomputes the VIRTUAL one the same way. Uses a temp file rather
   than the in-memory handle so the row round-trips through [Row.encode]. *)
let survives_close_and_reopen () =
  let path = Filename.temp_file "granary629" ".db" in
  Sys.remove path;
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () ->
       let db = open_db path in
       create_stored db;
       create_virtual db;
       exec db "INSERT INTO g (k, v) VALUES (1, 41)";
       exec db "INSERT INTO gv (k, v) VALUES (1, 41)";
       run (Db.close db);
       let db = open_db path in
       Fun.protect
         ~finally:(fun () ->
           try run (Db.close db) with
           | _ -> ())
         (fun () ->
            Alcotest.(check (list string))
              "STORED value read back"
              [ "1|41|42" ]
              (texts db "SELECT k, v, w FROM g");
            Alcotest.(check (list string))
              "VIRTUAL value recomputed"
              [ "1|41|42" ]
              (texts db "SELECT k, v, w FROM gv")))
;;

(* ------------------------------------------------------------------ *)
(* Property                                                             *)
(* ------------------------------------------------------------------ *)

(* Over a random batch, [INSERT OR IGNORE] keeps exactly the rows whose base
   value is non-NULL, and every surviving row's generated column equals the
   computed value. Both directions matter: dropping a good row is the #629 bug,
   keeping a bad one is the #599 semantics broken in the other direction. *)
let prop_or_ignore_keeps_exactly_the_computable_rows =
  QCheck.Test.make
    ~count:60
    ~name:"#629: OR IGNORE keeps exactly the rows whose generated value is non-NULL"
    QCheck.(small_list (option (int_range (-1000) 1000)))
    (fun vs ->
       with_db (fun db ->
         create_stored db;
         List.iteri
           (fun i v ->
              let params =
                [ Db.V_int (Int64.of_int i)
                ; (match v with
                   | None -> Db.V_null
                   | Some n -> Db.V_int (Int64.of_int n))
                ]
              in
              match run_stmt db "INSERT OR IGNORE INTO g (k, v) VALUES (?, ?)" params with
              | Ok _ -> ()
              | Error e -> Alcotest.failf "OR IGNORE raised: %a" Db.pp_error e)
           vs;
         let want =
           List.filter_map (Option.map (fun n -> Printf.sprintf "%d|%d" n (n + 1))) vs
         in
         want = texts db "SELECT v, w FROM g ORDER BY k"))
;;

let () =
  Alcotest.run
    "not_null_629"
    [ ( "the-bug"
      , [ Alcotest.test_case
            "STORED: a good row inserts"
            `Quick
            stored_generated_not_null_accepts_a_good_row
        ; Alcotest.test_case
            "VIRTUAL: a good row inserts"
            `Quick
            virtual_generated_not_null_accepts_a_good_row
        ; Alcotest.test_case
            "every reachable INSERT spelling works"
            `Quick
            every_reachable_insert_spelling_works
        ; Alcotest.test_case
            "the column is still unassignable"
            `Quick
            generated_column_is_still_unassignable
        ] )
    ; ( "enforcement-moved-not-dropped"
      , [ Alcotest.test_case
            "STORED: a NULL generated value is rejected"
            `Quick
            stored_generated_null_value_is_rejected
        ; Alcotest.test_case
            "VIRTUAL: a NULL generated value is rejected"
            `Quick
            virtual_generated_null_value_is_rejected
        ; Alcotest.test_case
            "the parameter spelling is rejected on both"
            `Quick
            parameter_null_is_rejected_on_both_storage_classes
        ; Alcotest.test_case
            "UPDATE of the base column is rejected"
            `Quick
            update_that_nulls_the_generated_value_is_rejected
        ] )
    ; ( "conflict-resolution"
      , [ Alcotest.test_case
            "OR IGNORE skips only the bad row"
            `Quick
            or_ignore_skips_only_the_bad_row
        ; Alcotest.test_case
            "every other resolution raises"
            `Quick
            other_resolutions_still_raise
        ] )
    ; ( "unchanged"
      , [ Alcotest.test_case
            "a plain NOT NULL is still a bind error"
            `Quick
            plain_not_null_still_a_bind_error
        ; Alcotest.test_case
            "nullable generated columns unaffected"
            `Quick
            nullable_generated_columns_unaffected
        ; Alcotest.test_case
            "a non-NULLable generated expression"
            `Quick
            generated_expression_that_cannot_be_null
        ; Alcotest.test_case "survives close and reopen" `Quick survives_close_and_reopen
        ] )
    ; ( "property"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_or_ignore_keeps_exactly_the_computable_rows ] )
    ]
;;
