open Lwt.Syntax
module Db  = Sqlocaml.Db
module Row = Sqlocaml_encoding.Row

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let run = Lwt_main.run

let fresh_db () = run (Db.open_in_memory ())

let exec db sql =
  run (
    let* result = Db.execute db sql in
    (match result with
     | Ok () -> ()
     | Error _ -> Alcotest.failf "exec: unexpected error for: %s" sql);
    Lwt.return_unit
  )

let exec_err db sql =
  run (
    let* result = Db.execute db sql in
    (match result with
     | Ok () -> Alcotest.failf "exec_err: expected Error for: %s" sql
     | Error e -> Lwt.return e)
  )

let query_ok db sql =
  run (
    let* result = Db.query db sql in
    match result with
    | Error _ -> Alcotest.failf "query_ok: unexpected error for: %s" sql
    | Ok stream ->
      Lwt_stream.to_list stream
  )

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
    | Db.V_real x, Db.V_real y -> Float.equal x y
    | Db.V_blob x, Db.V_blob y -> Bytes.equal x y
    | _,           _           -> false
  in
  Alcotest.testable pp eq

let int_of_v = function
  | Db.V_int n -> Int64.to_int n
  | _ -> -1

(* ------------------------------------------------------------------ *)
(* Group 1: Basic DELETE semantics                                      *)
(* ------------------------------------------------------------------ *)

let delete_all_no_where () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  exec db "INSERT INTO t (id, n) VALUES (2, 20)";
  exec db "INSERT INTO t (id, n) VALUES (3, 30)";
  exec db "DELETE FROM t";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "0 rows remain" 0 (List.length rows)

let delete_with_where_id () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  exec db "INSERT INTO t (id, n) VALUES (2, 20)";
  exec db "INSERT INTO t (id, n) VALUES (3, 30)";
  exec db "DELETE FROM t WHERE id = 2";
  let rows = query_ok db "SELECT id FROM t ORDER BY id ASC" in
  let ids = List.map (fun r -> int_of_v r.(0)) rows in
  Alcotest.(check (list int)) "rows 1 and 3 remain" [1; 3] ids

let delete_with_where_gt () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "INSERT INTO t (n) VALUES (3)";
  exec db "INSERT INTO t (n) VALUES (4)";
  exec db "INSERT INTO t (n) VALUES (5)";
  exec db "DELETE FROM t WHERE n > 3";
  (* Use SELECT * to avoid projection/sort ordinal mismatch (known planner limitation) *)
  let rows = query_ok db "SELECT * FROM t ORDER BY n ASC" in
  let ns = List.map (fun r -> int_of_v r.(0)) rows in
  Alcotest.(check (list int)) "only n<=3 remain" [1; 2; 3] ns

let delete_where_is_null () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, s TEXT)";
  exec db "INSERT INTO t (id, s) VALUES (1, 'a')";
  exec db "INSERT INTO t (id, s) VALUES (2, NULL)";
  exec db "INSERT INTO t (id, s) VALUES (3, 'c')";
  exec db "INSERT INTO t (id, s) VALUES (4, NULL)";
  exec db "DELETE FROM t WHERE s IS NULL";
  let rows = query_ok db "SELECT id FROM t ORDER BY id ASC" in
  let ids = List.map (fun r -> int_of_v r.(0)) rows in
  Alcotest.(check (list int)) "non-null rows remain" [1; 3] ids

let delete_literal_true () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "INSERT INTO t (n) VALUES (3)";
  exec db "DELETE FROM t WHERE 1 = 1";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "0 rows remain" 0 (List.length rows)

let delete_where_no_match () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  exec db "INSERT INTO t (id, n) VALUES (2, 20)";
  exec db "DELETE FROM t WHERE id = 999";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "2 rows remain" 2 (List.length rows)

let delete_empty_table () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "DELETE FROM t";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "still 0 rows" 0 (List.length rows)

(* ------------------------------------------------------------------ *)
(* Group 2: Semantic errors                                             *)
(* ------------------------------------------------------------------ *)

let delete_unknown_table () =
  let db = fresh_db () in
  let result = run (Db.execute db "DELETE FROM ghost") in
  match result with
  | Error (Db.Sema (Sqlocaml_sql.Sema.Unknown_table tbl)) ->
    Alcotest.(check string) "table name" "ghost" tbl
  | _ -> Alcotest.fail "expected Sema(Unknown_table \"ghost\")"

let delete_unknown_column_in_where () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  let e = exec_err db "DELETE FROM t WHERE bad_col = 1" in
  match e with
  | Db.Sema (Sqlocaml_sql.Sema.Unknown_column _) -> ()
  | _ -> Alcotest.fail "expected Sema(Unknown_column)"

(* ------------------------------------------------------------------ *)
(* Group 3: DELETE via query is a runtime error                         *)
(* ------------------------------------------------------------------ *)

let delete_via_query_returns_runtime_error () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  let result = run (Db.query db "DELETE FROM t") in
  match result with
  | Error (Db.Runtime _) -> ()
  | _ -> Alcotest.fail "expected Runtime error for DELETE via query"

(* ------------------------------------------------------------------ *)
(* Group 4: Index interactions                                          *)
(* ------------------------------------------------------------------ *)

let delete_removes_index_entries () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  exec db "INSERT INTO t (id, n) VALUES (2, 20)";
  exec db "INSERT INTO t (id, n) VALUES (3, 30)";
  exec db "CREATE INDEX idx ON t (n)";
  exec db "DELETE FROM t WHERE id = 2";
  (* The deleted row's indexed value should no longer be findable. *)
  let found = query_ok db "SELECT id FROM t WHERE n = 20" in
  Alcotest.(check int) "n=20 not found via index" 0 (List.length found);
  (* Other rows' index entries must still be intact. *)
  let found10 = query_ok db "SELECT id FROM t WHERE n = 10" in
  Alcotest.(check int) "n=10 still indexed" 1 (List.length found10);
  Alcotest.check value_testable "id=1" (Db.V_int 1L) (List.hd found10).(0)

let delete_then_reinsert_unique () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 42)";
  exec db "INSERT INTO t (id, n) VALUES (2, 99)";
  exec db "CREATE UNIQUE INDEX idx ON t (n)";
  (* Delete the row with n=42. *)
  exec db "DELETE FROM t WHERE id = 1";
  (* Re-inserting n=42 must now succeed (index entry was removed). *)
  exec db "INSERT INTO t (id, n) VALUES (3, 42)";
  let found = query_ok db "SELECT id FROM t WHERE n = 42" in
  Alcotest.(check int) "n=42 back via index" 1 (List.length found);
  Alcotest.check value_testable "id=3" (Db.V_int 3L) (List.hd found).(0)

let delete_removes_all_index_entries () =
  (* Delete all rows from a table with an index; verify the index is empty. *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  exec db "INSERT INTO t (id, n) VALUES (2, 20)";
  exec db "INSERT INTO t (id, n) VALUES (3, 30)";
  exec db "CREATE INDEX idx ON t (n)";
  exec db "DELETE FROM t";
  let found10 = query_ok db "SELECT id FROM t WHERE n = 10" in
  let found20 = query_ok db "SELECT id FROM t WHERE n = 20" in
  let found30 = query_ok db "SELECT id FROM t WHERE n = 30" in
  Alcotest.(check int) "no n=10" 0 (List.length found10);
  Alcotest.(check int) "no n=20" 0 (List.length found20);
  Alcotest.(check int) "no n=30" 0 (List.length found30)

(* ------------------------------------------------------------------ *)
(* Group 5: Rows-deleted count                                          *)
(* ------------------------------------------------------------------ *)

let delete_returns_count () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  exec db "INSERT INTO t (id, n) VALUES (2, 20)";
  exec db "INSERT INTO t (id, n) VALUES (3, 30)";
  exec db "INSERT INTO t (id, n) VALUES (4, 40)";
  let result = run (Db.execute_change_count db "DELETE FROM t WHERE n > 15") in
  match result with
  | Ok n -> Alcotest.(check int) "3 rows deleted" 3 n
  | Error _ -> Alcotest.fail "expected Ok count"

let delete_returns_zero_when_no_match () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, n INTEGER)";
  exec db "INSERT INTO t (id, n) VALUES (1, 10)";
  let result = run (Db.execute_change_count db "DELETE FROM t WHERE id = 999") in
  match result with
  | Ok n -> Alcotest.(check int) "0 rows deleted" 0 n
  | Error _ -> Alcotest.fail "expected Ok 0"

let delete_all_returns_total_count () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  for i = 1 to 7 do
    exec db (Printf.sprintf "INSERT INTO t (n) VALUES (%d)" i)
  done;
  let result = run (Db.execute_change_count db "DELETE FROM t") in
  match result with
  | Ok n -> Alcotest.(check int) "7 rows deleted" 7 n
  | Error _ -> Alcotest.fail "expected Ok 7"

(* ------------------------------------------------------------------ *)
(* Group 6: QCheck                                                      *)
(* ------------------------------------------------------------------ *)

let qcheck_delete_by_id =
  QCheck.Test.make
    ~name:"DELETE WHERE id = k: deleted row absent, others present"
    ~count:10_000
    QCheck.(pair
              (list_size Gen.(1 -- 20) (int_range 1 1000))
              (int_range 1 1000))
    (fun (ids, k) ->
       (* Deduplicate ids to avoid duplicate primary-key-like entries. *)
       let unique_ids =
         List.sort_uniq compare ids
       in
       if unique_ids = [] then true
       else
         let db = Lwt_main.run (Db.open_in_memory ()) in
         Lwt_main.run (
           let* _ = Db.execute db "CREATE TABLE t (id INTEGER)" in
           let* () = Lwt_list.iter_s (fun id ->
             let sql = Printf.sprintf "INSERT INTO t (id) VALUES (%d)" id in
             let* _ = Db.execute db sql in
             Lwt.return_unit
           ) unique_ids in
           let sql = Printf.sprintf "DELETE FROM t WHERE id = %d" k in
           let* _ = Db.execute db sql in
           let* result = Db.query db "SELECT id FROM t" in
           match result with
           | Error _ -> Lwt.return false
           | Ok stream ->
             let* rows = Lwt_stream.to_list stream in
             let remaining = List.map (fun r -> match r.(0) with
               | Row.V_int x -> Int64.to_int x
               | _ -> -1) rows in
             let expected =
               List.filter (fun id -> id <> k) unique_ids
             in
             (* deleted row absent *)
             let no_k = not (List.mem k remaining) in
             (* all other rows present *)
             let all_others = List.for_all (fun id -> List.mem id remaining) expected in
             let count_ok = List.length remaining = List.length expected in
             Lwt.return (no_k && all_others && count_ok)
         ))

let qcheck_delete_all_count_matches =
  QCheck.Test.make
    ~name:"DELETE FROM t: count matches number of inserted rows"
    ~count:10_000
    QCheck.(list_size Gen.(0 -- 20) (int_range 0 10000))
    (fun ns ->
       let db = Lwt_main.run (Db.open_in_memory ()) in
       Lwt_main.run (
         let* _ = Db.execute db "CREATE TABLE t (n INTEGER)" in
         let* () = Lwt_list.iter_s (fun n ->
           let sql = Printf.sprintf "INSERT INTO t (n) VALUES (%d)" n in
           let* _ = Db.execute db sql in
           Lwt.return_unit
         ) ns in
         let* result = Db.execute_change_count db "DELETE FROM t" in
         match result with
         | Error _ -> Lwt.return false
         | Ok got_count ->
           Lwt.return (got_count = List.length ns)
       ))

(* ------------------------------------------------------------------ *)
(* Runner                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "Delete" [
    "basic", [
      Alcotest.test_case "delete_all_no_where"       `Quick delete_all_no_where;
      Alcotest.test_case "delete_with_where_id"      `Quick delete_with_where_id;
      Alcotest.test_case "delete_with_where_gt"      `Quick delete_with_where_gt;
      Alcotest.test_case "delete_where_is_null"      `Quick delete_where_is_null;
      Alcotest.test_case "delete_literal_true"       `Quick delete_literal_true;
      Alcotest.test_case "delete_where_no_match"     `Quick delete_where_no_match;
      Alcotest.test_case "delete_empty_table"        `Quick delete_empty_table;
    ];
    "sema_errors", [
      Alcotest.test_case "unknown_table"             `Quick delete_unknown_table;
      Alcotest.test_case "unknown_column_in_where"   `Quick delete_unknown_column_in_where;
    ];
    "runtime_routing", [
      Alcotest.test_case "delete_via_query_is_runtime_error"
        `Quick delete_via_query_returns_runtime_error;
    ];
    "indexes", [
      Alcotest.test_case "delete_removes_index_entries"      `Quick delete_removes_index_entries;
      Alcotest.test_case "delete_then_reinsert_unique"       `Quick delete_then_reinsert_unique;
      Alcotest.test_case "delete_removes_all_index_entries"  `Quick delete_removes_all_index_entries;
    ];
    "row_count", [
      Alcotest.test_case "delete_returns_count"              `Quick delete_returns_count;
      Alcotest.test_case "delete_returns_zero_when_no_match" `Quick delete_returns_zero_when_no_match;
      Alcotest.test_case "delete_all_returns_total_count"    `Quick delete_all_returns_total_count;
    ];
    "qcheck", (
      List.map QCheck_alcotest.to_alcotest [
        qcheck_delete_by_id;
        qcheck_delete_all_count_matches;
      ]
    );
  ]
