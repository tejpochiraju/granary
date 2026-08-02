(** #547: the two assignment binders must agree about a literal NULL.

    [Sema.bind_update_assignments] has always refused a literal NULL assigned to
    a NOT NULL column. Its twin on the upsert path,
    [Sema.bind_upsert_assignments], had no such check, so
    [ON CONFLICT ... DO UPDATE SET c = NULL] wrote the NULL. Since #530 every
    PRIMARY KEY column carries [not_null = true], so the same hole let a NULL
    into a primary key: the catalog said NOT NULL, [PRAGMA table_info] said NOT
    NULL, [Db.dump] rendered NOT NULL into the restored DDL, and the row would
    then fail to restore.

    The two tests that matter are the two spellings of the write — an ordinary
    NOT NULL column and a PRIMARY KEY column — each checked to be rejected *and*
    to have left the pre-existing row untouched. Enforcement is literal-only in
    both binders, so [SET c = (SELECT NULL)] still gets through; that is a known
    hole recorded in [Planner.seek_is_unique_point]'s doc comment, and it is
    pinned below so a later fix notices this test rather than the other way
    round. *)

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

let contains_sub ~needle s =
  let n = String.length needle
  and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = needle || go (i + 1)) in
  go 0
;;

(* Rejected specifically for a NOT NULL violation naming [col]: a rejection for
   any other reason would satisfy a bare "is an error" check while proving
   nothing. *)
let rejects_null db ~col sql =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "expected a NOT NULL violation on %S, got success" col
  | Error e ->
    let msg = Format.asprintf "%a" Db.pp_error e in
    let want = "NOT NULL violation: " ^ col in
    Alcotest.(check bool)
      (Printf.sprintf "%S rejected with %S (got %S)" sql want msg)
      true
      (contains_sub ~needle:want msg)
;;

let texts db sql =
  List.map
    (fun (r : Row.t) ->
       Array.to_list r
       |> List.map (function
         | Row.V_text s -> s
         | Row.V_null -> "<null>"
         | Row.V_int n -> Int64.to_string n
         | Row.V_real f -> string_of_float f
         | Row.V_blob _ -> "<blob>")
       |> String.concat "|")
    (query db sql)
;;

let seed db =
  exec db "CREATE TABLE a (k TEXT PRIMARY KEY, j TEXT NOT NULL, v INTEGER)";
  exec db "INSERT INTO a VALUES ('x', 'y', 1)"
;;

(* The reported case: an ordinary NOT NULL column. *)
let upsert_null_into_not_null_column_rejected () =
  with_db (fun db ->
    seed db;
    rejects_null
      db
      ~col:"j"
      "INSERT INTO a VALUES ('x','z',2) ON CONFLICT (k) DO UPDATE SET j = NULL";
    Alcotest.(check (list string))
      "row untouched"
      [ "x|y|1" ]
      (texts db "SELECT * FROM a"))
;;

(* The sharp case: the primary key itself. *)
let upsert_null_into_primary_key_rejected () =
  with_db (fun db ->
    seed db;
    rejects_null
      db
      ~col:"k"
      "INSERT INTO a VALUES ('x','z',3) ON CONFLICT (k) DO UPDATE SET k = NULL";
    Alcotest.(check (list string))
      "row untouched"
      [ "x|y|1" ]
      (texts db "SELECT * FROM a"))
;;

(* The same NULL through plain UPDATE was already rejected; the point of the fix
   is that the two paths now say the same thing. *)
let update_and_upsert_agree () =
  with_db (fun db ->
    seed db;
    rejects_null db ~col:"j" "UPDATE a SET j = NULL";
    rejects_null
      db
      ~col:"j"
      "INSERT INTO a VALUES ('x','z',2) ON CONFLICT (k) DO UPDATE SET j = NULL")
;;

(* Every column of a composite key is NOT NULL after #530, so both are covered
   by the same check. *)
let upsert_null_into_composite_key_rejected () =
  with_db (fun db ->
    exec db "CREATE TABLE c (k TEXT, j TEXT, v INTEGER, PRIMARY KEY (k, j))";
    exec db "INSERT INTO c VALUES ('x', 'y', 1)";
    rejects_null
      db
      ~col:"j"
      "INSERT INTO c VALUES ('x','y',2) ON CONFLICT (k, j) DO UPDATE SET j = NULL")
;;

(* The tightening is confined to a literal NULL: a nullable column, and a
   non-NULL assignment to a NOT NULL column, still work. *)
let upsert_still_updates_normally () =
  with_db (fun db ->
    seed db;
    exec db "INSERT INTO a VALUES ('x','z',2) ON CONFLICT (k) DO UPDATE SET j = 'w'";
    Alcotest.(check (list string)) "j updated" [ "x|w|1" ] (texts db "SELECT * FROM a");
    exec db "INSERT INTO a VALUES ('x','z',2) ON CONFLICT (k) DO UPDATE SET v = NULL";
    Alcotest.(check (list string))
      "nullable column takes NULL"
      [ "x|w|<null>" ]
      (texts db "SELECT * FROM a");
    exec
      db
      "INSERT INTO a VALUES ('x','z',5) ON CONFLICT (k) DO UPDATE SET v = excluded.v";
    Alcotest.(check (list string))
      "excluded.v still works"
      [ "x|w|5" ]
      (texts db "SELECT * FROM a"))
;;

(* Known hole, pinned deliberately, and pinned as it actually behaves rather
   than as one would like it to.  Enforcement is literal-only, so a NULL that
   arrives via a subquery is not seen by either binder.  UPDATE happens to
   escape it anyway — for an unrelated reason, subqueries are not supported in
   UPDATE SET at all — while the upsert path evaluates the subquery and stores
   the NULL.  #547 asked only for parity on the literal; this stays open.  If
   this test starts failing, the hole has been closed: delete the test and the
   note in [Planner.seek_is_unique_point]. *)
let subquery_null_is_a_known_hole () =
  with_db (fun db ->
    seed db;
    (match run (Db.execute db "UPDATE a SET j = (SELECT NULL)") with
     | Ok () -> Alcotest.fail "UPDATE unexpectedly accepted a subquery in SET"
     | Error e ->
       let msg = Format.asprintf "%a" Db.pp_error e in
       Alcotest.(check bool)
         (Printf.sprintf "UPDATE refuses the subquery itself (%S)" msg)
         true
         (contains_sub ~needle:"subqueries in UPDATE" msg));
    (match
       run
         (Db.execute
            db
            "INSERT INTO a VALUES ('x','z',2) ON CONFLICT (k) DO UPDATE SET j = (SELECT \
             NULL)")
     with
     | Ok () -> ()
     | Error e -> Alcotest.failf "upsert with a subquery: %a" Db.pp_error e);
    Alcotest.(check (list string))
      "the non-literal NULL still lands — the known hole"
      [ "x|<null>|1" ]
      (texts db "SELECT * FROM a"))
;;

let () =
  Alcotest.run
    "upsert_not_null_547"
    [ ( "rejected"
      , [ Alcotest.test_case
            "NOT NULL column"
            `Quick
            upsert_null_into_not_null_column_rejected
        ; Alcotest.test_case
            "PRIMARY KEY column"
            `Quick
            upsert_null_into_primary_key_rejected
        ; Alcotest.test_case
            "composite key column"
            `Quick
            upsert_null_into_composite_key_rejected
        ; Alcotest.test_case "UPDATE and UPSERT agree" `Quick update_and_upsert_agree
        ] )
    ; ( "unchanged"
      , [ Alcotest.test_case "normal upserts" `Quick upsert_still_updates_normally
        ; Alcotest.test_case "subquery NULL hole" `Quick subquery_null_is_a_known_hole
        ] )
    ]
;;
