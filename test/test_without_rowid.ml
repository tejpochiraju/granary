(** Tests for the WITHOUT ROWID table option added in phase 37 (#122).

    In sqlocaml, WITHOUT ROWID requires a single INTEGER PRIMARY KEY column,
    and that column's value is used as the rowid (no auto-allocation).
    More general WITHOUT ROWID semantics (composite or non-integer PKs) are
    deferred behind a clear sema error. *)

open Lwt.Syntax

module Db = struct
  include Sqlocaml.Db

  let open_file = Sqlocaml_unix.open_file
end

let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_without_rowid_%04d.db" n
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

let unwrap_db_err pfx = function
  | Ok x -> x
  | Error e -> Alcotest.failf "%s: %a" pfx Db.pp_error e
;;

let open_path path =
  let* dbr = Db.open_file ~path () in
  Lwt.return (unwrap_db_err "open" dbr)
;;

let exec db sql =
  let* r = Db.execute db sql in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Lwt.fail_with (Format.asprintf "execute %S: %a" sql Db.pp_error e)
;;

let query db sql =
  let* r = Db.query db sql in
  let stream = unwrap_db_err "query" r in
  Lwt_stream.to_list stream
;;

let value_to_string = function
  | Sqlocaml_encoding.Row.V_int n -> Int64.to_string n
  | Sqlocaml_encoding.Row.V_text s -> s
  | Sqlocaml_encoding.Row.V_null -> "NULL"
  | Sqlocaml_encoding.Row.V_real f -> Printf.sprintf "%g" f
  | Sqlocaml_encoding.Row.V_blob b -> "BLOB(" ^ string_of_int (Bytes.length b) ^ ")"
;;

let row_to_string row = String.concat "|" (List.map value_to_string (Array.to_list row))

(* ------------------------------------------------------------------ *)

(* 1. CREATE TABLE … WITHOUT ROWID with single INTEGER PRIMARY KEY works. *)
let test_create_basic () =
  run
    (let path = fresh_path () in
     cleanup path;
     let* db = open_path path in
     let* () =
       exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT) WITHOUT ROWID"
     in
     let* () = exec db "INSERT INTO t (id, name) VALUES (42, 'foo')" in
     let* () = exec db "INSERT INTO t (id, name) VALUES (10, 'bar')" in
     let* rows = query db "SELECT id, name FROM t ORDER BY id" in
     Alcotest.(check int) "two rows" 2 (List.length rows);
     Alcotest.(check string) "first row by id" "10|bar" (row_to_string (List.nth rows 0));
     Alcotest.(check string) "second row by id" "42|foo" (row_to_string (List.nth rows 1));
     let* () = Db.close db in
     cleanup path;
     Lwt.return_unit)
;;

(* 2. WITHOUT ROWID without any PRIMARY KEY is rejected. *)
let test_no_pk_rejected () =
  run
    (let path = fresh_path () in
     cleanup path;
     let* db = open_path path in
     let* failed =
       Lwt.catch
         (fun () ->
            let* () = exec db "CREATE TABLE t (a INTEGER, b TEXT) WITHOUT ROWID" in
            Lwt.return_false)
         (fun _ -> Lwt.return_true)
     in
     Alcotest.(check bool) "WITHOUT ROWID without PK rejected" true failed;
     let* () = Db.close db in
     cleanup path;
     Lwt.return_unit)
;;

(* 3. WITHOUT ROWID with a TEXT primary key is rejected (phase 37 limit). *)
let test_text_pk_rejected () =
  run
    (let path = fresh_path () in
     cleanup path;
     let* db = open_path path in
     let* failed =
       Lwt.catch
         (fun () ->
            let* () =
              exec db "CREATE TABLE t (k TEXT PRIMARY KEY, v INT) WITHOUT ROWID"
            in
            Lwt.return_false)
         (fun _ -> Lwt.return_true)
     in
     Alcotest.(check bool) "TEXT PK + WITHOUT ROWID rejected" true failed;
     let* () = Db.close db in
     cleanup path;
     Lwt.return_unit)
;;

(* 4. Inserting two rows with the same explicit PK fails (unique). *)
let test_duplicate_pk_rejected () =
  run
    (let path = fresh_path () in
     cleanup path;
     let* db = open_path path in
     let* () =
       exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT) WITHOUT ROWID"
     in
     let* () = exec db "INSERT INTO t (id, name) VALUES (1, 'a')" in
     let* failed =
       Lwt.catch
         (fun () ->
            let* () = exec db "INSERT INTO t (id, name) VALUES (1, 'b')" in
            Lwt.return_false)
         (fun _ -> Lwt.return_true)
     in
     Alcotest.(check bool) "duplicate PK rejected" true failed;
     let* () = Db.close db in
     cleanup path;
     Lwt.return_unit)
;;

(* 5. NULL PK in WITHOUT ROWID table is rejected. *)
let test_null_pk_rejected () =
  run
    (let path = fresh_path () in
     cleanup path;
     let* db = open_path path in
     let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, x INT) WITHOUT ROWID" in
     let* failed =
       Lwt.catch
         (fun () ->
            let* () = exec db "INSERT INTO t (x) VALUES (42)" in
            Lwt.return_false)
         (fun _ -> Lwt.return_true)
     in
     Alcotest.(check bool) "INSERT without PK value rejected" true failed;
     let* () = Db.close db in
     cleanup path;
     Lwt.return_unit)
;;

(* 6. WITHOUT ROWID schema survives reopen. *)
let test_persistence () =
  run
    (let path = fresh_path () in
     cleanup path;
     let* db = open_path path in
     let* () =
       exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT) WITHOUT ROWID"
     in
     let* () = exec db "INSERT INTO t (id, name) VALUES (7, 'lucky')" in
     let* () = Db.close db in
     let* db2 = open_path path in
     (* After reopen, INSERT must continue to use the user-supplied PK as
       the rowid — otherwise next_rowid_in_txn would be invoked. *)
     let* () = exec db2 "INSERT INTO t (id, name) VALUES (8, 'eight')" in
     let* rows = query db2 "SELECT id, name FROM t ORDER BY id" in
     Alcotest.(check int) "two rows after reopen" 2 (List.length rows);
     Alcotest.(check string) "row 0" "7|lucky" (row_to_string (List.nth rows 0));
     Alcotest.(check string) "row 1" "8|eight" (row_to_string (List.nth rows 1));
     let* () = Db.close db2 in
     cleanup path;
     Lwt.return_unit)
;;

(* 7. UPDATE / DELETE on a WITHOUT ROWID table work via the existing
      rowid plumbing.  Internally the PK column's value IS the rowid. *)
let test_update_and_delete () =
  run
    (let path = fresh_path () in
     cleanup path;
     let* db = open_path path in
     let* () =
       exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT) WITHOUT ROWID"
     in
     let* () = exec db "INSERT INTO t (id, name) VALUES (5, 'x')" in
     let* () = exec db "INSERT INTO t (id, name) VALUES (6, 'y')" in
     let* () = exec db "UPDATE t SET name = 'Y' WHERE id = 6" in
     let* () = exec db "DELETE FROM t WHERE id = 5" in
     let* rows = query db "SELECT id, name FROM t" in
     Alcotest.(check int) "one row remains" 1 (List.length rows);
     Alcotest.(check string) "remaining row" "6|Y" (row_to_string (List.nth rows 0));
     let* () = Db.close db in
     cleanup path;
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "without_rowid"
    [ ( "create"
      , [ Alcotest.test_case "basic create + insert + select" `Quick test_create_basic
        ; Alcotest.test_case "no PK rejected" `Quick test_no_pk_rejected
        ; Alcotest.test_case "TEXT PK rejected" `Quick test_text_pk_rejected
        ] )
    ; ( "constraint"
      , [ Alcotest.test_case "duplicate PK rejected" `Quick test_duplicate_pk_rejected
        ; Alcotest.test_case "NULL PK rejected" `Quick test_null_pk_rejected
        ] )
    ; ( "persistence"
      , [ Alcotest.test_case "schema persists across reopen" `Quick test_persistence ] )
    ; ( "dml"
      , [ Alcotest.test_case "UPDATE and DELETE by PK" `Quick test_update_and_delete ] )
    ]
;;
