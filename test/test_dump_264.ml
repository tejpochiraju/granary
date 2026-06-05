(** #264: logical SQL dump (.dump-style export).

    The dump emits a self-contained SQL script that recreates the database when
    replayed through the executor.  The central oracle here is a {b round-trip}:
    build a database, [Db.dump_to_string] it, replay the script into a fresh
    database, and assert the two are equivalent — identical row data and
    identical table/view/trigger DDL.  This catches serialization bugs (float
    precision, blob/NULL/quote escaping, generated columns, WITHOUT ROWID) and
    schema-replay bugs (double-created implicit indexes, dependency ordering)
    that no amount of staring at the emitter would.

    Implicit PRIMARY KEY index {e names} may legitimately differ after a
    round-trip (a table-level PK is re-emitted as a column-level clause whose
    backing index is renamed), so index fidelity is pinned separately by proving
    the restored database still enforces the constraint. *)

open Lwt.Syntax
module Db = Sqlocaml.Db

let run = Lwt_main.run

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "db error: %a" Db.pp_error e
;;

let contains_substr ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let with_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

let with_file_db f =
  let path = Filename.temp_file "sqlocaml_dump_264" ".db" in
  let db =
    match run (Sqlocaml_unix.open_file ~path ()) with
    | Ok db -> db
    | Error e -> Alcotest.failf "open_file: %a" Db.pp_error e
  in
  Fun.protect
    ~finally:(fun () ->
      (try run (Db.close db) with
       | _ -> ());
      try Sys.remove path with
      | _ -> ())
    (fun () -> f db)
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let dump ?schema_only ?data_only db =
  unwrap (run (Db.dump_to_string db ?schema_only ?data_only ()))
;;

(* Split a SQL script into statements, respecting single-quoted string literals
   (where '' is an escaped quote) and CREATE TRIGGER bodies (whose BEGIN..END
   block contains its own statement-terminating semicolons).  Mirrors how the
   sqlite3 shell decides statement boundaries; the dump emits no comments, so
   this is sufficient to replay it. *)
let split_statements sql =
  let n = String.length sql in
  let buf = Buffer.create 256 in
  let out = ref [] in
  let in_str = ref false in
  let i = ref 0 in
  (* A CREATE TRIGGER statement only ends at the ';' following its final END. *)
  let upper_ends_with_end () =
    let s = String.uppercase_ascii (String.trim (Buffer.contents buf)) in
    let l = String.length s in
    l >= 3 && String.sub s (l - 3) 3 = "END"
  in
  let is_trigger () =
    let s = String.uppercase_ascii (String.trim (Buffer.contents buf)) in
    String.length s >= 14 && String.sub s 0 14 = "CREATE TRIGGER"
  in
  while !i < n do
    let c = sql.[!i] in
    if !in_str
    then
      if c = '\''
      then
        if !i + 1 < n && sql.[!i + 1] = '\''
        then (
          Buffer.add_string buf "''";
          incr i)
        else (
          Buffer.add_char buf c;
          in_str := false)
      else Buffer.add_char buf c
    else if c = '\''
    then (
      Buffer.add_char buf c;
      in_str := true)
    else if c = ';'
    then
      if is_trigger () && not (upper_ends_with_end ())
      then Buffer.add_char buf ';' (* inside the trigger body — keep it *)
      else (
        let s = String.trim (Buffer.contents buf) in
        if s <> "" then out := s :: !out;
        Buffer.clear buf)
    else Buffer.add_char buf c;
    incr i
  done;
  let s = String.trim (Buffer.contents buf) in
  if s <> "" then out := s :: !out;
  List.rev !out
;;

(* Replay a dump script into a fresh in-memory database. *)
let restore script =
  let db = run (Db.open_in_memory ()) in
  List.iter (fun stmt -> exec db stmt) (split_statements script);
  db
;;

let vstr = function
  | Db.V_int i -> Printf.sprintf "i:%Ld" i
  | Db.V_real f -> Printf.sprintf "r:%.17g" f
  | Db.V_text s -> Printf.sprintf "t:%s" s
  | Db.V_null -> "null"
  | Db.V_blob b -> Printf.sprintf "b:%s" (String.escaped (Bytes.to_string b))
;;

let rows db sql =
  run
    (let* s = Db.query db sql in
     let* rows = Lwt_stream.to_list (unwrap s) in
     Lwt.return
       (List.map (fun r -> String.concat "," (Array.to_list (Array.map vstr r))) rows))
;;

(* User table names (the leading "t:" is [vstr]'s tag for a text value). *)
let user_tables db =
  rows db "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
  |> List.map (fun s ->
    match String.split_on_char ':' s with
    | "t" :: rest -> String.concat ":" rest
    | _ -> s)
;;

(* Sorted row data of one table — order-independent so two databases with the
   same multiset of rows compare equal regardless of physical ordering. *)
let table_data db name =
  List.sort compare (rows db (Printf.sprintf "SELECT * FROM \"%s\"" name))
;;

(* DDL for tables/views/triggers (NOT indexes — see file header). *)
let schema_ddl db =
  List.sort
    compare
    (rows
       db
       "SELECT sql FROM sqlite_master WHERE type IN ('table','view','trigger') ORDER BY \
        sql")
;;

(* The crux: original and a fresh replay of its dump must agree on every table's
   data and on table/view/trigger DDL. *)
let assert_roundtrip orig =
  let script = dump orig in
  let restored = restore script in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close restored) with
      | _ -> ())
    (fun () ->
       Alcotest.(check (list string))
         "table/view/trigger DDL preserved"
         (schema_ddl orig)
         (schema_ddl restored);
       let tables = user_tables orig in
       Alcotest.(check (list string)) "same set of tables" tables (user_tables restored);
       List.iter
         (fun tbl ->
            Alcotest.(check (list string))
              (Printf.sprintf "row data preserved: %s" tbl)
              (table_data orig tbl)
              (table_data restored tbl))
         tables)
;;

(* ---------------------------------------------------------------- *)
(* Round-trip across a rich schema, on both backends                 *)
(* ---------------------------------------------------------------- *)

let populate db =
  exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT NOT NULL, age INTEGER)";
  exec db "INSERT INTO users VALUES (1, 'Alice', 30)";
  exec db "INSERT INTO users VALUES (2, 'O''Brien', NULL)";
  exec db "INSERT INTO users VALUES (3, 'tab\there', -7)";
  (* floats needing full round-trip precision + large magnitude *)
  exec db "CREATE TABLE nums (k INTEGER PRIMARY KEY, f REAL)";
  exec db "INSERT INTO nums VALUES (1, 0.1)";
  exec db "INSERT INTO nums VALUES (2, 3.141592653589793)";
  exec db "INSERT INTO nums VALUES (3, -2.5e-30)";
  exec db "INSERT INTO nums VALUES (4, 5.0)";
  exec db "INSERT INTO nums VALUES (5, 1e308)";
  (* blobs, including the empty blob *)
  exec db "CREATE TABLE blobs (k INTEGER PRIMARY KEY, b BLOB)";
  exec db "INSERT INTO blobs VALUES (1, X'00FF1080')";
  exec db "INSERT INTO blobs VALUES (2, X'')";
  (* table-level UNIQUE + an explicit secondary index *)
  exec db "CREATE TABLE codes (a TEXT, b INTEGER, UNIQUE(a, b))";
  exec db "INSERT INTO codes VALUES ('x', 1)";
  exec db "INSERT INTO codes VALUES ('y', 2)";
  exec db "CREATE INDEX idx_codes_b ON codes (b)";
  (* non-alias text PRIMARY KEY (its implicit index must not double-create) *)
  exec db "CREATE TABLE kv (k TEXT PRIMARY KEY, v TEXT)";
  exec db "INSERT INTO kv VALUES ('greeting', 'hello')";
  (* generated column: its value must NOT be dumped, only recomputed *)
  exec
    db
    "CREATE TABLE rect (w INTEGER, h INTEGER, area INTEGER GENERATED ALWAYS AS (w * h) \
     VIRTUAL)";
  exec db "INSERT INTO rect (w, h) VALUES (3, 4)";
  exec db "INSERT INTO rect (w, h) VALUES (5, 6)";
  (* view + trigger reference base tables *)
  exec db "CREATE VIEW adults AS SELECT name FROM users WHERE age >= 18";
  exec db "CREATE TABLE audit (msg TEXT)";
  exec
    db
    "CREATE TRIGGER trg AFTER INSERT ON users BEGIN INSERT INTO audit VALUES ('new \
     user'); END"
;;

let test_roundtrip_mem () =
  with_db (fun db ->
    populate db;
    assert_roundtrip db)
;;

let test_roundtrip_file () =
  with_file_db (fun db ->
    populate db;
    assert_roundtrip db)
;;

let test_roundtrip_empty () = with_db (fun db -> assert_roundtrip db)

(* WITHOUT ROWID tables must dump the clause, or the restore silently becomes a
   rowid table. *)
let test_without_rowid () =
  with_db (fun db ->
    (* our WITHOUT ROWID requires an INTEGER PRIMARY KEY (phase-37 limit) *)
    exec db "CREATE TABLE wr (k INTEGER PRIMARY KEY, v INTEGER) WITHOUT ROWID";
    exec db "INSERT INTO wr VALUES (10, 1)";
    exec db "INSERT INTO wr VALUES (20, 2)";
    let script = dump db in
    Alcotest.(check bool)
      "dump carries WITHOUT ROWID"
      true
      (contains_substr ~needle:"WITHOUT ROWID" script);
    assert_roundtrip db)
;;

(* ---------------------------------------------------------------- *)
(* Targeted serialization & toggle behaviour                         *)
(* ---------------------------------------------------------------- *)

(* Every value shape survives the literal encoder bit-for-bit.  Columns are
   strictly typed, so each literal is inserted into a column of its own storage
   class; unfilled columns default to NULL (also exercised). *)

(* #270: -9223372036854775808 is Int64.min_int and must parse, store, and
   round-trip exactly, even though its bare magnitude (2^63) is one past
   Int64.max_int.  The bare positive magnitude stays out of range. *)
let test_int64_min_literal () =
  with_db (fun db ->
    exec db "CREATE TABLE t (i INTEGER)";
    exec db "INSERT INTO t (i) VALUES (-9223372036854775808)";
    Alcotest.(check (list string))
      "INT64_MIN stored exactly"
      [ Printf.sprintf "i:%Ld" Int64.min_int ]
      (rows db "SELECT i FROM t");
    (* the same fold applies in a general SELECT expression (distinct grammar
       production from INSERT ... VALUES) *)
    Alcotest.(check (list string))
      "INT64_MIN as a SELECT expression"
      [ Printf.sprintf "i:%Ld" Int64.min_int ]
      (rows db "SELECT -9223372036854775808");
    (* the bare positive magnitude 2^63 is out of Int64 range and is rejected
       in both INSERT and SELECT contexts *)
    (match run (Db.execute db "INSERT INTO t (i) VALUES (9223372036854775808)") with
     | Ok () -> Alcotest.fail "bare positive 2^63 should be rejected, not accepted"
     | Error _ -> ());
    (match run (Db.query db "SELECT 9223372036854775808") with
     | Ok _ -> Alcotest.fail "bare positive 2^63 in SELECT should be rejected"
     | Error _ -> ());
    (* a negative magnitude beyond 2^63 is genuinely out of range: the unary
       minus has nothing representable to fold onto, so it is rejected too *)
    (match run (Db.execute db "INSERT INTO t (i) VALUES (-99999999999999999999)") with
     | Ok () -> Alcotest.fail "negative magnitude > 2^63 should be rejected"
     | Error _ -> ());
    (match run (Db.query db "SELECT -99999999999999999999") with
     | Ok _ -> Alcotest.fail "negative magnitude > 2^63 in SELECT should be rejected"
     | Error _ -> ());
    (* the row must survive a logical-dump round-trip *)
    assert_roundtrip db)
;;

(* #284: a leading zero on the INT64_MIN magnitude (09223372036854775808) must
   not defeat the 2^63 overflow-fold.  The magnitude is numerically still 2^63
   and, once negated, is exactly Int64.min_int — just like the no-leading-zero
   form.  Over-range magnitudes stay rejected even with leading zeros. *)
let test_int64_min_literal_leading_zero () =
  with_db (fun db ->
    Alcotest.(check (list string))
      "leading-zero INT64_MIN folds to min_int"
      [ Printf.sprintf "i:%Ld" Int64.min_int ]
      (rows db "SELECT -09223372036854775808");
    Alcotest.(check (list string))
      "many leading zeros INT64_MIN folds to min_int"
      [ Printf.sprintf "i:%Ld" Int64.min_int ]
      (rows db "SELECT -0009223372036854775808");
    (* regression: the no-leading-zero form still folds to min_int *)
    Alcotest.(check (list string))
      "no-leading-zero INT64_MIN still folds to min_int"
      [ Printf.sprintf "i:%Ld" Int64.min_int ]
      (rows db "SELECT -9223372036854775808");
    (* regression: over-by-one with a leading zero is still genuinely out of
       range and must be rejected *)
    (match run (Db.query db "SELECT -09223372036854775809") with
     | Ok _ -> Alcotest.fail "leading-zero magnitude 2^63+1 in SELECT should be rejected"
     | Error _ -> ());
    (* sanity: a small leading-zero int is unaffected and parses normally *)
    Alcotest.(check (list string))
      "small leading-zero int parses as its value"
      [ "i:-7" ]
      (rows db "SELECT -007"))
;;

let test_value_literals () =
  with_db (fun db ->
    exec db "CREATE TABLE lit (k INTEGER PRIMARY KEY, i INTEGER, r REAL, t TEXT, b BLOB)";
    let row k col v =
      exec db (Printf.sprintf "INSERT INTO lit (k, %s) VALUES (%d, %s)" col k v)
    in
    let cases =
      [ "i", "0"
      ; "i", "-1"
      ; "i", "9223372036854775807" (* Int64.max_int *)
      ; "i", "-9223372036854775807"
      ; "i", "-9223372036854775808" (* Int64.min_int — see #270 *)
      ; "r", "0.0"
      ; "r", "-0.5"
      ; "r", "1.7976931348623157e308"
      ; "r", "0.30000000000000004"
      ; "r", "-2.5e-30"
      ; "t", "'plain'"
      ; "t", "''"
      ; "t", "'a''b'"
      ; "b", "X'DEADBEEF'"
      ; "b", "X''"
      ]
    in
    List.iteri (fun k (col, v) -> row k col v) cases;
    (* an all-NULL row *)
    exec db "INSERT INTO lit (k) VALUES (1000)";
    assert_roundtrip db)
;;

(* schema_only omits all data; data_only omits all DDL and framing. *)
let test_schema_only_data_only () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
    exec db "INSERT INTO t VALUES (1, 'x')";
    let s = dump ~schema_only:true db in
    Alcotest.(check bool)
      "schema_only has CREATE"
      true
      (contains_substr ~needle:"CREATE TABLE" s);
    Alcotest.(check bool)
      "schema_only has no INSERT"
      false
      (contains_substr ~needle:"INSERT INTO" s);
    let d = dump ~data_only:true db in
    Alcotest.(check bool)
      "data_only has INSERT"
      true
      (contains_substr ~needle:"INSERT INTO" d);
    Alcotest.(check bool)
      "data_only has no CREATE"
      false
      (contains_substr ~needle:"CREATE" d);
    (* data_only is pure DML, so it IS wrapped in a transaction *)
    Alcotest.(check bool)
      "data_only wrapped in BEGIN"
      true
      (contains_substr ~needle:"BEGIN" d))
;;

(* Generated-column values are recomputed on restore, never dumped as data. *)
let test_generated_columns () =
  with_db (fun db ->
    exec
      db
      "CREATE TABLE g (a INTEGER, b INTEGER, c INTEGER GENERATED ALWAYS AS (a + b) \
       STORED)";
    exec db "INSERT INTO g (a, b) VALUES (2, 3)";
    let s = dump db in
    (* the INSERT must carry only a and b, never the derived c=5 *)
    Alcotest.(check bool)
      "generated column excluded from INSERT column list"
      true
      (contains_substr ~needle:"INSERT INTO g (a, b)" s);
    assert_roundtrip db)
;;

(* A UNIQUE constraint must still be enforced after a dump/restore — proves the
   backing index survives, independent of its (possibly renamed) identifier. *)
let test_unique_enforced_after_restore () =
  with_db (fun db ->
    exec db "CREATE TABLE u (a TEXT, b INTEGER, UNIQUE(a))";
    exec db "INSERT INTO u VALUES ('k', 1)";
    let restored = restore (dump db) in
    Fun.protect
      ~finally:(fun () ->
        try run (Db.close restored) with
        | _ -> ())
      (fun () ->
         (* duplicate key must be rejected by the restored unique index *)
         match run (Db.execute restored "INSERT INTO u VALUES ('k', 2)") with
         | Ok () -> Alcotest.fail "UNIQUE not enforced after restore"
         | Error _ -> ()))
;;

(* #273: a user index whose name collides with the [__pk_] prefix that the
   engine assigns to auto-created PRIMARY KEY indexes must NOT be mistaken for an
   implicit index and silently dropped from the dump.  The implicit-vs-user
   distinction is now an explicit [idx_origin] field on [index_info], so the
   classifier is name-independent.  The decoy index sits on the PK column (the
   only place the old name-sniffing heuristic fired), proving origin — not name —
   now decides what the dump omits. *)
let test_user_index_pk_prefix_survives () =
  with_db (fun db ->
    exec db "CREATE TABLE kv (k TEXT PRIMARY KEY, v TEXT)";
    exec db "INSERT INTO kv VALUES ('a', '1')";
    exec db "CREATE UNIQUE INDEX __pk_kv_decoy ON kv (k)";
    let s = dump db in
    Alcotest.(check bool)
      "user index with __pk_ prefix is emitted in the dump"
      true
      (contains_substr ~needle:"__pk_kv_decoy" s);
    let restored = restore s in
    Fun.protect
      ~finally:(fun () ->
        try run (Db.close restored) with
        | _ -> ())
      (fun () ->
         let names =
           rows
             restored
             "SELECT name FROM sqlite_master WHERE type='index' AND name='__pk_kv_decoy'"
         in
         Alcotest.(check int) "user index survived dump/restore" 1 (List.length names)))
;;

(* A view that selects from another view must be emitted AFTER its dependency:
   view bodies are bound at CREATE time, so the wrong order aborts the restore.
   The dependency view is named to sort LAST alphabetically, so a naive
   name-ordered dump would emit the dependent view first and fail. *)
let test_view_on_view () =
  with_db (fun db ->
    exec db "CREATE TABLE base (x INTEGER)";
    exec db "INSERT INTO base VALUES (1)";
    exec db "INSERT INTO base VALUES (2)";
    (* 'z_inner' sorts after 'a_outer', yet a_outer depends on z_inner *)
    exec db "CREATE VIEW z_inner AS SELECT x FROM base WHERE x > 0";
    exec db "CREATE VIEW a_outer AS SELECT x FROM z_inner";
    assert_roundtrip db)
;;

(* FTS5 virtual tables: the dump recreates them (empty) via DDL and emits no row
   INSERTs against them (content dump is a planned follow-up). *)
let test_fts_ddl_only () =
  with_db (fun db ->
    exec db "CREATE VIRTUAL TABLE docs USING fts5(title, body)";
    exec db "INSERT INTO docs (title, body) VALUES ('hi', 'hello world')";
    let s = dump db in
    Alcotest.(check bool)
      "FTS CREATE VIRTUAL TABLE emitted"
      true
      (contains_substr ~needle:"CREATE VIRTUAL TABLE" s);
    Alcotest.(check bool)
      "no INSERT against the FTS table"
      false
      (contains_substr ~needle:"INSERT INTO docs" s);
    (* and the script replays cleanly into an empty FTS table *)
    let restored = restore s in
    try run (Db.close restored) with
    | _ -> ())
;;

let () =
  Alcotest.run
    "dump_264"
    [ ( "roundtrip"
      , [ Alcotest.test_case "rich schema (mem)" `Quick test_roundtrip_mem
        ; Alcotest.test_case "rich schema (file)" `Quick test_roundtrip_file
        ; Alcotest.test_case "empty database" `Quick test_roundtrip_empty
        ; Alcotest.test_case "without rowid" `Quick test_without_rowid
        ; Alcotest.test_case "value literals" `Quick test_value_literals
        ; Alcotest.test_case "int64 min literal (#270)" `Quick test_int64_min_literal
        ; Alcotest.test_case
            "int64 min literal leading zero (#284)"
            `Quick
            test_int64_min_literal_leading_zero
        ] )
    ; ( "serialization"
      , [ Alcotest.test_case "schema_only / data_only" `Quick test_schema_only_data_only
        ; Alcotest.test_case "generated columns" `Quick test_generated_columns
        ; Alcotest.test_case
            "unique enforced after restore"
            `Quick
            test_unique_enforced_after_restore
        ; Alcotest.test_case
            "user index with __pk_ prefix survives"
            `Quick
            test_user_index_pk_prefix_survives
        ; Alcotest.test_case "view depending on view" `Quick test_view_on_view
        ; Alcotest.test_case "fts ddl only" `Quick test_fts_ddl_only
        ] )
    ]
;;
