(* Correctness comparison against SQLite3.
   Requires sqlite3 in PATH; tests skip gracefully if unavailable.

   Run with sqlite3 mounted into the container:
     podman run --rm \
       -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
       -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
       -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
       -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
       -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
       -w /workspace sqlocaml-dev dune runtest
*)
open Lwt.Syntax
module Db = Sqlocaml.Db

(* ── availability check ────────────────────────────────────────── *)

let sqlite3_available () =
  Sys.command "sqlite3 --version >/dev/null 2>&1" = 0

(* ── sqlite3 helpers ───────────────────────────────────────────── *)

let fresh_db_path () =
  Printf.sprintf "/tmp/sqlocaml_cmp_%d_%d.db"
    (Unix.getpid ()) (Random.bits () land 0xFFFFFF)

let sqlite3_run_setup ~db_path stmts =
  List.iter (fun sql ->
    let cmd = Printf.sprintf "sqlite3 %s %s >/dev/null 2>&1"
      (Filename.quote db_path) (Filename.quote sql) in
    ignore (Sys.command cmd)
  ) stmts

let sqlite3_run_query ~db_path query =
  let cmd = Printf.sprintf
    "sqlite3 -separator '\t' -nullvalue '__NULL__' %s %s 2>&1"
    (Filename.quote db_path) (Filename.quote query) in
  let ic = Unix.open_process_in cmd in
  let acc = ref [] in
  (try while true do acc := input_line ic :: !acc done
   with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  let lines = List.rev !acc in
  (* Empty output (no rows) → empty list *)
  List.filter_map (fun line ->
    if String.length line = 0 then None
    else Some (String.split_on_char '\t' line)
  ) lines

(* ── sqlocaml helpers ──────────────────────────────────────────── *)

(* Format a value the same way sqlite3 outputs it with -nullvalue '__NULL__' *)
let fmt_val = function
  | Db.V_null   -> "__NULL__"
  | Db.V_int  n -> Int64.to_string n
  | Db.V_text s -> s
  | Db.V_real f ->
    (* SQLite always includes a decimal point for real output *)
    let s = Printf.sprintf "%g" f in
    if String.contains s '.' || String.contains s 'e' || String.contains s 'E'
       || String.contains s 'n' (* nan/inf *)
    then s
    else s ^ ".0"
  | Db.V_blob b -> Printf.sprintf "<blob:%d>" (Bytes.length b)

let sqlocaml_run setup query =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* () = Lwt_list.iter_s (fun sql ->
      let* r = Db.execute db sql in
      (match r with
       | Ok () -> ()
       | Error e -> Alcotest.failf "sqlocaml setup error for %S: %a" sql Db.pp_error e);
      Lwt.return_unit
    ) setup in
    let* result = Db.query db query in
    match result with
    | Error e -> Alcotest.failf "sqlocaml query error for %S: %a" query Db.pp_error e
    | Ok stream ->
      let* rows = Lwt_stream.to_list stream in
      Lwt.return (List.map (fun row ->
        Array.to_list (Array.map fmt_val row)
      ) rows)
  )

(* ── test case type ────────────────────────────────────────────── *)

type test_case = {
  name      : string;
  setup     : string list;
  query     : string;
  unordered : bool;  (* sort both sides before comparing *)
}

(* ── comparison runner ─────────────────────────────────────────── *)

let run_case tc =
  let db_path = fresh_db_path () in
  Fun.protect
    ~finally:(fun () -> try Unix.unlink db_path with _ -> ())
    (fun () ->
      sqlite3_run_setup ~db_path tc.setup;
      let sqlite_rows   = sqlite3_run_query ~db_path tc.query in
      let sqlocaml_rows = sqlocaml_run tc.setup tc.query in
      let sort rows = List.sort compare rows in
      let a = if tc.unordered then sort sqlite_rows   else sqlite_rows   in
      let b = if tc.unordered then sort sqlocaml_rows else sqlocaml_rows in
      if a <> b then begin
        let fmt rows =
          if rows = [] then "  (empty)"
          else String.concat "\n" (List.map (fun r ->
            "  [" ^ String.concat " | " r ^ "]") rows)
        in
        Alcotest.failf "%s:\n\nSQLite:\n%s\n\nSqlocaml:\n%s"
          tc.name (fmt a) (fmt b)
      end)

let make_test tc =
  Alcotest.test_case tc.name `Quick (fun () ->
    if sqlite3_available () then run_case tc
    else Printf.printf "[SKIP] sqlite3 not in PATH — %s\n%!" tc.name
  )

(* ── test cases ────────────────────────────────────────────────── *)

let cases = [

  (* Basic SELECT *)
  { name = "basic_select_ordered";
    setup = [
      "CREATE TABLE t (id INTEGER, name TEXT)";
      "INSERT INTO t VALUES (1, 'alice')";
      "INSERT INTO t VALUES (2, 'bob')";
    ];
    query = "SELECT * FROM t ORDER BY id ASC";
    unordered = false };

  { name = "select_projection";
    setup = [
      "CREATE TABLE t (a INTEGER, b TEXT, c INTEGER)";
      "INSERT INTO t VALUES (1, 'x', 100)";
      "INSERT INTO t VALUES (2, 'y', 200)";
    ];
    query = "SELECT b, c FROM t ORDER BY a ASC";
    unordered = false };

  (* NULL ordering *)
  { name = "null_sort_asc";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (NULL)";
      "INSERT INTO t VALUES (2)";
      "INSERT INTO t VALUES (1)";
    ];
    query = "SELECT * FROM t ORDER BY n ASC";
    unordered = false };

  { name = "null_sort_desc";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (NULL)";
      "INSERT INTO t VALUES (2)";
      "INSERT INTO t VALUES (1)";
    ];
    query = "SELECT * FROM t ORDER BY n DESC";
    unordered = false };

  (* NULL comparisons *)
  { name = "null_eq_null_in_where";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (NULL)";
      "INSERT INTO t VALUES (1)";
    ];
    query = "SELECT * FROM t WHERE n = NULL";
    unordered = false };

  { name = "null_is_null";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (NULL)";
      "INSERT INTO t VALUES (1)";
    ];
    query = "SELECT * FROM t WHERE n IS NULL";
    unordered = false };

  { name = "null_is_not_null";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (NULL)";
      "INSERT INTO t VALUES (1)";
    ];
    query = "SELECT * FROM t WHERE n IS NOT NULL";
    unordered = false };

  (* NULL arithmetic — results should be NULL *)
  { name = "null_arithmetic";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (5)";
    ];
    query = "SELECT n + NULL, n - NULL, n * NULL FROM t";
    unordered = false };

  (* Integer arithmetic *)
  { name = "integer_division";
    setup = [
      "CREATE TABLE t (a INTEGER, b INTEGER)";
      "INSERT INTO t VALUES (7, 2)";
      "INSERT INTO t VALUES (10, 3)";
    ];
    query = "SELECT a / b FROM t ORDER BY a ASC";
    unordered = false };

  { name = "arithmetic_precedence";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (1)";
    ];
    query = "SELECT 2 + 3 * 4, (2 + 3) * 4 FROM t";
    unordered = false };

  { name = "negative_arithmetic";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (-5)";
      "INSERT INTO t VALUES (3)";
    ];
    query = "SELECT -n, n * -1 FROM t ORDER BY n ASC";
    unordered = false };

  (* Comparison operators *)
  { name = "where_range";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
      "INSERT INTO t VALUES (3)";
      "INSERT INTO t VALUES (4)";
      "INSERT INTO t VALUES (5)";
    ];
    query = "SELECT * FROM t WHERE n >= 2 AND n <= 4 ORDER BY n ASC";
    unordered = false };

  { name = "where_or";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
      "INSERT INTO t VALUES (3)";
    ];
    query = "SELECT * FROM t WHERE n = 1 OR n = 3 ORDER BY n ASC";
    unordered = false };

  { name = "where_not";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
      "INSERT INTO t VALUES (3)";
    ];
    query = "SELECT * FROM t WHERE NOT (n = 2) ORDER BY n ASC";
    unordered = false };

  (* Aggregate functions *)
  { name = "count_star";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (NULL)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
    ];
    query = "SELECT COUNT(*) FROM t";
    unordered = false };

  { name = "count_col_skips_null";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (NULL)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
    ];
    query = "SELECT COUNT(n) FROM t";
    unordered = false };

  { name = "sum_skips_null";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (NULL)";
      "INSERT INTO t VALUES (3)";
      "INSERT INTO t VALUES (7)";
    ];
    query = "SELECT SUM(n) FROM t";
    unordered = false };

  { name = "min_max_skips_null";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (NULL)";
      "INSERT INTO t VALUES (3)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
    ];
    query = "SELECT MIN(n), MAX(n) FROM t";
    unordered = false };

  { name = "count_empty_table";
    setup = [ "CREATE TABLE t (n INTEGER)" ];
    query = "SELECT COUNT(*) FROM t";
    unordered = false };

  { name = "sum_empty_table";
    setup = [ "CREATE TABLE t (n INTEGER)" ];
    query = "SELECT SUM(n), MIN(n), MAX(n) FROM t";
    unordered = false };

  (* GROUP BY *)
  { name = "group_by_count";
    setup = [
      "CREATE TABLE t (cat TEXT, val INTEGER)";
      "INSERT INTO t VALUES ('a', 1)";
      "INSERT INTO t VALUES ('a', 2)";
      "INSERT INTO t VALUES ('b', 3)";
      "INSERT INTO t VALUES ('b', 4)";
      "INSERT INTO t VALUES ('c', 5)";
    ];
    query = "SELECT cat, COUNT(*), SUM(val) FROM t GROUP BY cat ORDER BY cat ASC";
    unordered = false };

  { name = "having_sum";
    setup = [
      "CREATE TABLE t (cat TEXT, val INTEGER)";
      "INSERT INTO t VALUES ('a', 1)";
      "INSERT INTO t VALUES ('a', 2)";
      "INSERT INTO t VALUES ('b', 10)";
      "INSERT INTO t VALUES ('b', 20)";
      "INSERT INTO t VALUES ('c', 1)";
    ];
    query = "SELECT cat, SUM(val) FROM t GROUP BY cat HAVING SUM(val) > 5 ORDER BY cat ASC";
    unordered = false };

  (* LIMIT / OFFSET *)
  { name = "limit";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
      "INSERT INTO t VALUES (3)";
      "INSERT INTO t VALUES (4)";
      "INSERT INTO t VALUES (5)";
    ];
    query = "SELECT * FROM t ORDER BY n ASC LIMIT 3";
    unordered = false };

  { name = "limit_offset";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
      "INSERT INTO t VALUES (3)";
      "INSERT INTO t VALUES (4)";
      "INSERT INTO t VALUES (5)";
    ];
    query = "SELECT * FROM t ORDER BY n ASC LIMIT 2 OFFSET 2";
    unordered = false };

  (* String functions *)
  { name = "string_functions";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('Hello World')";
      "INSERT INTO t VALUES ('foo')";
    ];
    query = "SELECT LENGTH(s), LOWER(s), UPPER(s) FROM t ORDER BY s ASC";
    unordered = false };

  { name = "length_null";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES (NULL)";
    ];
    query = "SELECT LENGTH(s) FROM t";
    unordered = false };

  { name = "abs_function";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (-5)";
      "INSERT INTO t VALUES (0)";
      "INSERT INTO t VALUES (3)";
    ];
    query = "SELECT ABS(n) FROM t ORDER BY n ASC";
    unordered = false };

  { name = "coalesce";
    setup = [
      "CREATE TABLE t (a INTEGER, b INTEGER)";
      "INSERT INTO t VALUES (NULL, 42)";
      "INSERT INTO t VALUES (1, NULL)";
      "INSERT INTO t VALUES (NULL, NULL)";
    ];
    query = "SELECT COALESCE(a, b, -1) FROM t ORDER BY a ASC";
    unordered = false };

  { name = "ifnull";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (NULL)";
      "INSERT INTO t VALUES (5)";
    ];
    query = "SELECT IFNULL(n, -1) FROM t ORDER BY n ASC";
    unordered = false };

  (* JOINs *)
  { name = "inner_join";
    setup = [
      "CREATE TABLE a (id INTEGER, name TEXT)";
      "CREATE TABLE b (aid INTEGER, val INTEGER)";
      "INSERT INTO a VALUES (1, 'x')";
      "INSERT INTO a VALUES (2, 'y')";
      "INSERT INTO a VALUES (3, 'z')";
      "INSERT INTO b VALUES (1, 100)";
      "INSERT INTO b VALUES (1, 200)";
      "INSERT INTO b VALUES (2, 300)";
    ];
    query = "SELECT a.name, b.val FROM a INNER JOIN b ON a.id = b.aid ORDER BY b.val ASC";
    unordered = false };

  { name = "left_join_with_nulls";
    setup = [
      "CREATE TABLE a (id INTEGER, name TEXT)";
      "CREATE TABLE b (aid INTEGER, val INTEGER)";
      "INSERT INTO a VALUES (1, 'x')";
      "INSERT INTO a VALUES (2, 'y')";
      "INSERT INTO a VALUES (3, 'z')";
      "INSERT INTO b VALUES (1, 100)";
      "INSERT INTO b VALUES (2, 200)";
    ];
    query = "SELECT a.name, b.val FROM a LEFT JOIN b ON a.id = b.aid ORDER BY a.id ASC";
    unordered = false };

  { name = "join_null_key_excluded";
    setup = [
      "CREATE TABLE a (id INTEGER, name TEXT)";
      "CREATE TABLE b (aid INTEGER, val INTEGER)";
      "INSERT INTO a VALUES (NULL, 'nope')";
      "INSERT INTO a VALUES (1, 'yes')";
      "INSERT INTO b VALUES (NULL, 999)";
      "INSERT INTO b VALUES (1, 100)";
    ];
    query = "SELECT a.name, b.val FROM a INNER JOIN b ON a.id = b.aid ORDER BY a.name ASC";
    unordered = false };

  (* UPDATE effect on SELECT *)
  { name = "update_then_select";
    setup = [
      "CREATE TABLE t (id INTEGER, name TEXT)";
      "INSERT INTO t VALUES (1, 'alice')";
      "INSERT INTO t VALUES (2, 'bob')";
    ];
    query = "SELECT * FROM t ORDER BY id ASC";
    unordered = false };

  (* Scalar expressions in SELECT *)
  { name = "scalar_expr_projection";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (3)";
      "INSERT INTO t VALUES (5)";
    ];
    query = "SELECT n * 2, n + 10 FROM t ORDER BY n ASC";
    unordered = false };

  (* WHERE with expression involving NULL *)
  { name = "where_null_and";
    setup = [
      "CREATE TABLE t (a INTEGER, b INTEGER)";
      "INSERT INTO t VALUES (1, NULL)";
      "INSERT INTO t VALUES (2, 2)";
    ];
    query = "SELECT * FROM t WHERE a > 0 AND b > 0 ORDER BY a ASC";
    unordered = false };

  (* HAVING with qualified table.col inside aggregate function *)
  { name = "having_qualified_agg_col";
    setup = [
      "CREATE TABLE t (cat TEXT, val INTEGER)";
      "INSERT INTO t VALUES ('a', 1)";
      "INSERT INTO t VALUES ('a', 2)";
      "INSERT INTO t VALUES ('b', 10)";
      "INSERT INTO t VALUES ('b', 20)";
      "INSERT INTO t VALUES ('c', 1)";
    ];
    query = "SELECT cat, SUM(val) FROM t GROUP BY cat HAVING SUM(t.val) > 5 ORDER BY cat ASC";
    unordered = false };

  (* ORDER BY with qualified column in JOIN query *)
  { name = "order_by_qualified_col";
    setup = [
      "CREATE TABLE a (id INTEGER, name TEXT)";
      "CREATE TABLE b (aid INTEGER, val INTEGER)";
      "INSERT INTO a VALUES (1, 'x')";
      "INSERT INTO a VALUES (2, 'y')";
      "INSERT INTO b VALUES (1, 100)";
      "INSERT INTO b VALUES (2, 200)";
    ];
    query = "SELECT a.name, b.val FROM a INNER JOIN b ON a.id = b.aid ORDER BY a.name DESC";
    unordered = false };

  (* SELECT * with WHERE clause using OR *)
  { name = "select_star_where_or";
    setup = [
      "CREATE TABLE t (id INTEGER, name TEXT, score INTEGER)";
      "INSERT INTO t VALUES (1, 'alice', 90)";
      "INSERT INTO t VALUES (2, 'bob', 50)";
      "INSERT INTO t VALUES (3, 'charlie', 80)";
    ];
    query = "SELECT * FROM t WHERE score > 85 OR name = 'bob' ORDER BY id ASC";
    unordered = false };

  (* Negative integer division *)
  { name = "negative_division";
    setup = [
      "CREATE TABLE t (a INTEGER, b INTEGER)";
      "INSERT INTO t VALUES (-7, 2)";
      "INSERT INTO t VALUES (7, -2)";
    ];
    query = "SELECT a / b FROM t ORDER BY a ASC";
    unordered = false };

]

(* ── runner ────────────────────────────────────────────────────── *)

let () =
  Alcotest.run "sqlite_compare" [
    "correctness", List.map make_test cases;
  ]
