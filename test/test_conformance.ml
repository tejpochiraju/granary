open Lwt.Syntax
module Db = Sqlocaml.Db

(* ------------------------------------------------------------------ *)
(* Test case type                                                        *)
(* ------------------------------------------------------------------ *)

type sql_test = {
  name     : string;
  setup    : string list;
  query    : string;
  expected : Db.row list;
}

(* ------------------------------------------------------------------ *)
(* Testables                                                             *)
(* ------------------------------------------------------------------ *)

let value_testable : Db.value Alcotest.testable =
  let pp ppf v = match v with
    | Db.V_int  n -> Format.fprintf ppf "V_int(%Ld)" n
    | Db.V_text s -> Format.fprintf ppf "V_text(%S)" s
    | Db.V_null   -> Format.fprintf ppf "V_null"
    | Db.V_real f -> Format.fprintf ppf "V_real(%h)" f
    | Db.V_blob b -> Format.fprintf ppf "V_blob(%d bytes)" (Bytes.length b)
  in
  let eq a b = match a, b with
    | Db.V_int  x, Db.V_int  y -> Int64.equal x y
    | Db.V_text x, Db.V_text y -> String.equal x y
    | Db.V_null,   Db.V_null   -> true
    | Db.V_real x, Db.V_real y ->
      Int64.equal (Int64.bits_of_float x) (Int64.bits_of_float y)
    | Db.V_blob x, Db.V_blob y -> Bytes.equal x y
    | _,           _           -> false
  in
  Alcotest.testable pp eq

let row_testable : Db.row Alcotest.testable =
  let pp ppf arr =
    Format.fprintf ppf "[|";
    Array.iter (fun v ->
      Format.fprintf ppf " ";
      (Alcotest.pp value_testable) ppf v
    ) arr;
    Format.fprintf ppf " |]"
  in
  let eq a b =
    Array.length a = Array.length b &&
    Array.for_all2 (fun x y -> Alcotest.equal value_testable x y) a b
  in
  Alcotest.testable pp eq

(* ------------------------------------------------------------------ *)
(* Helpers                                                               *)
(* ------------------------------------------------------------------ *)

let check_rows expected actual =
  Alcotest.(check int) "row count" (List.length expected) (List.length actual);
  List.iteri (fun i (exp, got) ->
    Alcotest.check row_testable (Printf.sprintf "row %d" i) exp got
  ) (List.combine expected actual)

let exec_ok db sql =
  let* result = Db.execute db sql in
  (match result with
   | Ok () -> ()
   | Error _ -> Alcotest.failf "exec_ok: unexpected error for: %s" sql);
  Lwt.return_unit

let query_rows db sql =
  let* result = Db.query db sql in
  match result with
  | Error _ -> Alcotest.failf "query_rows: unexpected error for: %s" sql
  | Ok stream -> Lwt_stream.to_list stream

(* Unique temp path per invocation to avoid collisions. *)
let temp_counter = ref 0

let fresh_temp_path () =
  incr temp_counter;
  Printf.sprintf "/tmp/sqlocaml_conformance_%d_%d.db"
    (Unix.getpid ()) !temp_counter

(* ------------------------------------------------------------------ *)
(* Suite runner                                                          *)
(* ------------------------------------------------------------------ *)

let run_suite open_db close_db tests =
  List.map (fun t ->
    Alcotest.test_case t.name `Quick (fun () ->
      Lwt_main.run (
        let* db = open_db () in
        let* () = Lwt_list.iter_s (exec_ok db) t.setup in
        let* rows = query_rows db t.query in
        let* () = close_db db in
        check_rows t.expected rows;
        Lwt.return_unit
      )
    )
  ) tests

(* ------------------------------------------------------------------ *)
(* Backend openers                                                       *)
(* ------------------------------------------------------------------ *)

let open_mem () = Db.open_in_memory ()

let open_unix_file path () =
  let* result = Db.open_file ~path in
  match result with
  | Ok db -> Lwt.return db
  | Error _ -> Alcotest.fail "open_unix_file: failed to open db"

let close_and_delete_file path db =
  let* () = Db.close db in
  (try Unix.unlink path with Unix.Unix_error _ -> ());
  Lwt.return_unit

(* ------------------------------------------------------------------ *)
(* Test cases                                                            *)
(* ------------------------------------------------------------------ *)

let tests = [

  { name = "create_insert_select_all";
    setup = [
      "CREATE TABLE t (n INTEGER, s TEXT)";
      "INSERT INTO t (n, s) VALUES (1, 'hello')";
      "INSERT INTO t (n, s) VALUES (2, 'world')";
    ];
    query = "SELECT * FROM t";
    expected = [
      [| Db.V_int 1L; Db.V_text "hello" |];
      [| Db.V_int 2L; Db.V_text "world" |];
    ] };

  { name = "select_where_equality";
    setup = [
      "CREATE TABLE t (id INTEGER, name TEXT)";
      "INSERT INTO t (id, name) VALUES (10, 'alice')";
      "INSERT INTO t (id, name) VALUES (20, 'bob')";
    ];
    query = "SELECT * FROM t WHERE id = 10";
    expected = [
      [| Db.V_int 10L; Db.V_text "alice" |];
    ] };

  { name = "order_by_asc";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t (n) VALUES (3)";
      "INSERT INTO t (n) VALUES (1)";
      "INSERT INTO t (n) VALUES (2)";
    ];
    query = "SELECT * FROM t ORDER BY n ASC";
    expected = [
      [| Db.V_int 1L |];
      [| Db.V_int 2L |];
      [| Db.V_int 3L |];
    ] };

  { name = "order_by_desc";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t (n) VALUES (3)";
      "INSERT INTO t (n) VALUES (1)";
      "INSERT INTO t (n) VALUES (2)";
    ];
    query = "SELECT * FROM t ORDER BY n DESC";
    expected = [
      [| Db.V_int 3L |];
      [| Db.V_int 2L |];
      [| Db.V_int 1L |];
    ] };

  { name = "limit_basic";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t (n) VALUES (10)";
      "INSERT INTO t (n) VALUES (20)";
      "INSERT INTO t (n) VALUES (30)";
      "INSERT INTO t (n) VALUES (40)";
      "INSERT INTO t (n) VALUES (50)";
    ];
    query = "SELECT * FROM t LIMIT 3";
    expected = [
      [| Db.V_int 10L |];
      [| Db.V_int 20L |];
      [| Db.V_int 30L |];
    ] };

  { name = "limit_offset";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t (n) VALUES (10)";
      "INSERT INTO t (n) VALUES (20)";
      "INSERT INTO t (n) VALUES (30)";
      "INSERT INTO t (n) VALUES (40)";
      "INSERT INTO t (n) VALUES (50)";
    ];
    query = "SELECT * FROM t LIMIT 2 OFFSET 2";
    expected = [
      [| Db.V_int 30L |];
      [| Db.V_int 40L |];
    ] };

  { name = "real_values";
    setup = [
      "CREATE TABLE t (x REAL)";
      "INSERT INTO t (x) VALUES (3.14)";
      "INSERT INTO t (x) VALUES (-1.5)";
    ];
    query = "SELECT * FROM t ORDER BY x ASC";
    expected = [
      [| Db.V_real (-1.5) |];
      [| Db.V_real 3.14   |];
    ] };

  { name = "null_values";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t (n) VALUES (NULL)";
      "INSERT INTO t (n) VALUES (42)";
    ];
    query = "SELECT * FROM t WHERE n = 42";
    expected = [
      [| Db.V_int 42L |];
    ] };

  { name = "null_order_by_last";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t (n) VALUES (NULL)";
      "INSERT INTO t (n) VALUES (1)";
      "INSERT INTO t (n) VALUES (2)";
    ];
    query = "SELECT * FROM t ORDER BY n ASC";
    expected = [
      [| Db.V_int 1L |];
      [| Db.V_int 2L |];
      [| Db.V_null   |];
    ] };

  { name = "index_lookup";
    setup = [
      "CREATE TABLE t (id INTEGER, name TEXT)";
      "INSERT INTO t (id, name) VALUES (1, 'alice')";
      "INSERT INTO t (id, name) VALUES (2, 'bob')";
      "CREATE INDEX idx ON t(id)";
    ];
    query = "SELECT * FROM t WHERE id = 1";
    expected = [
      [| Db.V_int 1L; Db.V_text "alice" |];
    ] };

  { name = "empty_table";
    setup = [
      "CREATE TABLE t (n INTEGER)";
    ];
    query = "SELECT * FROM t";
    expected = [] };

  { name = "column_projection";
    setup = [
      "CREATE TABLE t (a INTEGER, b TEXT, c INTEGER)";
      "INSERT INTO t (a, b, c) VALUES (1, 'x', 100)";
    ];
    query = "SELECT b FROM t";
    expected = [
      [| Db.V_text "x" |];
    ] };

]

(* ------------------------------------------------------------------ *)
(* Unique-index rejection — special case (checks Error, not rows)       *)
(* ------------------------------------------------------------------ *)

let unique_index_duplicate_rejected_case backend_name open_db close_db =
  Alcotest.test_case "unique_index_duplicate_rejected" `Quick (fun () ->
    Lwt_main.run (
      let* db = open_db () in
      let* () = exec_ok db "CREATE TABLE t (id INTEGER, name TEXT)" in
      let* () = exec_ok db "CREATE UNIQUE INDEX idx ON t (id)" in
      let* () = exec_ok db "INSERT INTO t (id, name) VALUES (1, 'alice')" in
      let* result = Db.execute db "INSERT INTO t (id, name) VALUES (1, 'bob')" in
      (match result with
       | Error (Db.Runtime _) -> ()
       | Ok () ->
         Alcotest.failf "%s: expected Runtime error for UNIQUE violation, got Ok"
           backend_name
       | Error e ->
         let msg = match e with
           | Db.Parse s  -> "Parse: " ^ s
           | Db.Sema _   -> "Sema"
           | Db.Runtime s -> "Runtime: " ^ s
         in
         Alcotest.failf "%s: expected Runtime error, got %s" backend_name msg);
      let* () = close_db db in
      Lwt.return_unit
    )
  )

(* ------------------------------------------------------------------ *)
(* Persistence test — Unix_file only                                     *)
(* ------------------------------------------------------------------ *)

let persistence_test path =
  Alcotest.test_case "persistence_survive_close_reopen" `Quick (fun () ->
    Lwt_main.run (
      (* Phase 1: open, insert, close *)
      let* db1 = open_unix_file path () in
      let* () = exec_ok db1 "CREATE TABLE t (id INTEGER, name TEXT)" in
      let* () = exec_ok db1 "INSERT INTO t (id, name) VALUES (1, 'alice')" in
      let* () = exec_ok db1 "INSERT INTO t (id, name) VALUES (2, 'bob')" in
      let* () = Db.close db1 in
      (* Phase 2: reopen, query, verify *)
      let* db2 = open_unix_file path () in
      let* rows = query_rows db2 "SELECT * FROM t ORDER BY id ASC" in
      let* () = Db.close db2 in
      (try Unix.unlink path with Unix.Unix_error _ -> ());
      let expected = [
        [| Db.V_int 1L; Db.V_text "alice" |];
        [| Db.V_int 2L; Db.V_text "bob"   |];
      ] in
      check_rows expected rows;
      Lwt.return_unit
    )
  )

(* ------------------------------------------------------------------ *)
(* Build per-backend test suites                                         *)
(* ------------------------------------------------------------------ *)

let mem_suite () =
  let open_db = open_mem in
  let close_db = Db.close in
  run_suite open_db close_db tests
  @ [ unique_index_duplicate_rejected_case "mem" open_db close_db ]

let unix_file_suite () =
  (* Each test gets its own fresh temp file path. *)
  let cases =
    List.map (fun t ->
      let path = fresh_temp_path () in
      let open_db = open_unix_file path in
      let close_db = close_and_delete_file path in
      let cases = run_suite open_db close_db [t] in
      List.hd cases
    ) tests
  in
  let uniq_path = fresh_temp_path () in
  let open_db   = open_unix_file uniq_path in
  let close_db  = close_and_delete_file uniq_path in
  let uniq_case = unique_index_duplicate_rejected_case "unix_file" open_db close_db in
  let persist_path = fresh_temp_path () in
  cases @ [ uniq_case; persistence_test persist_path ]

(* ------------------------------------------------------------------ *)
(* Runner                                                                *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "conformance" [
    "mem",       mem_suite ();
    "unix_file", unix_file_suite ();
  ]
