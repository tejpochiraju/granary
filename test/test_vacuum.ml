(** Tests for the VACUUM SQL statement added in phase 37 (#120).

    VACUUM rebuilds the database file in place by copying every tree
    from the source into a fresh sibling file and atomically replacing
    the original.  After VACUUM, the freelist is empty and the file is
    densely packed. *)

open Lwt.Syntax

module Db = struct
  include Sqlocaml.Db

  let open_file = Sqlocaml_unix.open_file
end

module Row = Sqlocaml_encoding.Row
module Geometry = Sqlocaml_storage.Geometry
module Header = Sqlocaml_storage.Header

let run = Lwt_main.run

(* Read the persisted geometry straight off page 0 of a closed file (header
   fields at bytes 56/64 always sit within the first 4096 bytes), so a test can
   assert what page size a file is actually stored at. *)
let peek_file_geometry path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
       let len = 4096 in
       let b = Bytes.create len in
       really_input ic b 0 len;
       Header.peek_geometry (Cstruct.of_bytes b))
;;

let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_vacuum_%04d.db" n
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  (try Unix.unlink (path ^ "-wal") with
   | _ -> ());
  (try Unix.unlink (path ^ ".vacuum-tmp") with
   | _ -> ());
  try Unix.unlink (path ^ ".vacuum-tmp-wal") with
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

let row_text (row : Row.t) =
  String.concat
    "|"
    (Array.to_list
       (Array.map
          (function
            | Row.V_int n -> Int64.to_string n
            | Row.V_text s -> s
            | Row.V_null -> "NULL"
            | Row.V_real f -> Printf.sprintf "%g" f
            | Row.V_blob b -> "BLOB(" ^ string_of_int (Bytes.length b) ^ ")")
          row))
;;

(* ------------------------------------------------------------------ *)

(* 1. VACUUM on a fresh, empty database succeeds and yields no
      observable change (idempotent baseline). *)
let test_vacuum_empty () =
  run
    (let path = fresh_path () in
     cleanup path;
     let* db = open_path path in
     let* () = exec db "VACUUM" in
     let* () = Db.close db in
     cleanup path;
     Lwt.return_unit)
;;

(* 2. After bulk insert + delete + VACUUM, the file is smaller AND the
      remaining rows are intact. *)
let test_vacuum_after_bulk_delete () =
  run
    (let path = fresh_path () in
     cleanup path;
     let* db = open_path path in
     let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, payload TEXT)" in
     let* () =
       Lwt_list.iter_s
         (fun i ->
            let payload = String.make 200 'x' in
            let sql =
              Printf.sprintf "INSERT INTO t (id, payload) VALUES (%d, '%s')" i payload
            in
            exec db sql)
         (List.init 500 Fun.id)
     in
     let size_full = Unix.((stat path).st_size) in
     let* () = exec db "DELETE FROM t WHERE id >= 50" in
     let* () = exec db "VACUUM" in
     let size_after = Unix.((stat path).st_size) in
     let* rows = query db "SELECT id FROM t ORDER BY id" in
     Alcotest.(check int) "50 rows remain after vacuum" 50 (List.length rows);
     Alcotest.(check bool)
       (Printf.sprintf "vacuum shrank file (full=%d, after=%d)" size_full size_after)
       true
       (size_after < size_full);
     let* () = Db.close db in
     cleanup path;
     Lwt.return_unit)
;;

(* 3. VACUUM preserves multiple tables and a secondary index. *)
let test_vacuum_preserves_tables_and_indexes () =
  run
    (let path = fresh_path () in
     cleanup path;
     let* db = open_path path in
     let* () = exec db "CREATE TABLE users (id INTEGER, name TEXT)" in
     let* () = exec db "CREATE INDEX idx_users_name ON users (name)" in
     let* () = exec db "INSERT INTO users (id, name) VALUES (1, 'alice')" in
     let* () = exec db "INSERT INTO users (id, name) VALUES (2, 'bob')" in
     let* () = exec db "INSERT INTO users (id, name) VALUES (3, 'carol')" in
     let* () = exec db "VACUUM" in
     let* rows = query db "SELECT id, name FROM users ORDER BY id" in
     Alcotest.(check int) "3 rows after vacuum" 3 (List.length rows);
     Alcotest.(check string) "first row" "1|alice" (row_text (List.nth rows 0));
     Alcotest.(check string) "third row" "3|carol" (row_text (List.nth rows 2));
     (* Index reachable after vacuum *)
     let* by_name = query db "SELECT id FROM users WHERE name = 'bob'" in
     Alcotest.(check int) "name lookup returns one row" 1 (List.length by_name);
     Alcotest.(check string) "bob's id" "2" (row_text (List.hd by_name));
     let* () = Db.close db in
     cleanup path;
     Lwt.return_unit)
;;

(* 4. VACUUM keeps large-blob payloads (overflow chains) intact. *)
let test_vacuum_with_overflow_blobs () =
  run
    (let path = fresh_path () in
     cleanup path;
     let* db = open_path path in
     let* () = exec db "CREATE TABLE t (id INTEGER, b TEXT)" in
     (* Insert a long literal — the encoded row will be > 800 B and spill
       to an overflow chain inside the rowid tree. *)
     let big_text = String.make 5000 'q' in
     let sql = Printf.sprintf "INSERT INTO t (id, b) VALUES (1, '%s')" big_text in
     let* () = exec db sql in
     let* () = exec db "VACUUM" in
     let* rows = query db "SELECT id FROM t" in
     Alcotest.(check int) "row survived vacuum" 1 (List.length rows);
     let* () = Db.close db in
     cleanup path;
     Lwt.return_unit)
;;

(* 4b. VACUUM of a non-default-geometry database must preserve the chosen
       page_size (#176): the rebuild used to open the temp file at the default
       4096, silently shrinking a 16K database. *)
let test_vacuum_preserves_geometry () =
  run
    (let path = fresh_path () in
     cleanup path;
     let* dbr = Db.open_file ~page_size:16384 ~path () in
     let db = unwrap_db_err "open 16k" dbr in
     let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, payload TEXT)" in
     let* () =
       Lwt_list.iter_s
         (fun i ->
            let payload = String.make 200 'x' in
            exec
              db
              (Printf.sprintf "INSERT INTO t (id, payload) VALUES (%d, '%s')" i payload))
         (List.init 500 Fun.id)
     in
     (* Delete most rows so the freelist is non-empty when VACUUM runs. *)
     let* () = exec db "DELETE FROM t WHERE id >= 50" in
     let before =
       match peek_file_geometry path with
       | Some g -> g
       | None -> Alcotest.fail "could not peek geometry before vacuum"
     in
     Alcotest.(check int) "created at 16K" 16384 before.Geometry.page_size;
     let* () = exec db "VACUUM" in
     let* rows = query db "SELECT id FROM t ORDER BY id" in
     Alcotest.(check int) "50 rows survive vacuum" 50 (List.length rows);
     let* () = Db.close db in
     let after =
       match peek_file_geometry path with
       | Some g -> g
       | None -> Alcotest.fail "could not peek geometry after vacuum"
     in
     Alcotest.(check int)
       "page_size preserved across vacuum"
       16384
       after.Geometry.page_size;
     Alcotest.(check int)
       "reserved_bytes preserved across vacuum"
       before.Geometry.reserved_bytes_per_page
       after.Geometry.reserved_bytes_per_page;
     cleanup path;
     Lwt.return_unit)
;;

(* 5. VACUUM inside an explicit transaction fails cleanly. *)
let test_vacuum_inside_txn_fails () =
  run
    (let path = fresh_path () in
     cleanup path;
     let* db = open_path path in
     let* () = exec db "CREATE TABLE t (x INT)" in
     let* () = exec db "BEGIN" in
     let* r = Db.execute db "VACUUM" in
     Alcotest.(check bool) "VACUUM in txn fails" true (Result.is_error r);
     let* () = exec db "ROLLBACK" in
     let* () = Db.close db in
     cleanup path;
     Lwt.return_unit)
;;

(* 6. VACUUM on an in-memory database is rejected. *)
let test_vacuum_in_memory_fails () =
  run
    (let* db = Db.open_in_memory () in
     let* () = exec db "CREATE TABLE t (x INT)" in
     let* r = Db.execute db "VACUUM" in
     Alcotest.(check bool) "VACUUM on in-memory fails" true (Result.is_error r);
     let* () = Db.close db in
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "vacuum"
    [ ( "basic"
      , [ Alcotest.test_case "empty database" `Quick test_vacuum_empty
        ; Alcotest.test_case "shrinks after bulk del" `Quick test_vacuum_after_bulk_delete
        ; Alcotest.test_case
            "preserves indexes"
            `Quick
            test_vacuum_preserves_tables_and_indexes
        ; Alcotest.test_case
            "overflow blobs survive"
            `Quick
            test_vacuum_with_overflow_blobs
        ; Alcotest.test_case
            "preserves page geometry"
            `Quick
            test_vacuum_preserves_geometry
        ] )
    ; ( "guards"
      , [ Alcotest.test_case "rejects inside txn" `Quick test_vacuum_inside_txn_fails
        ; Alcotest.test_case "rejects on in-memory" `Quick test_vacuum_in_memory_fails
        ] )
    ]
;;
