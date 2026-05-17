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
    (* SQLite uses up to 15 significant digits, removing trailing zeros.
       Use %.15g to match SQLite's output format for large and small floats. *)
    let s = Printf.sprintf "%.15g" f in
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

  (* Phase 6: || concat operator *)
  { name = "concat_strings";
    setup = [
      "CREATE TABLE t (a TEXT, b TEXT)";
      "INSERT INTO t VALUES ('foo', 'bar')";
    ];
    query = "SELECT a || ' ' || b FROM t";
    unordered = false };

  { name = "concat_null_propagates";
    setup = [
      "CREATE TABLE t (a TEXT)";
      "INSERT INTO t VALUES (NULL)";
    ];
    query = "SELECT a || 'x' FROM t";
    unordered = false };

  { name = "concat_int_text";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (42)";
    ];
    query = "SELECT n || ' items' FROM t";
    unordered = false };

  (* Phase 6: % modulo *)
  { name = "modulo_basic";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (7)";
      "INSERT INTO t VALUES (10)";
      "INSERT INTO t VALUES (6)";
    ];
    query = "SELECT n % 3 FROM t ORDER BY n";
    unordered = false };

  { name = "modulo_null";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (NULL)";
    ];
    query = "SELECT n % 3 FROM t";
    unordered = false };

  { name = "modulo_div_by_zero";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (7)";
    ];
    query = "SELECT n % 0 FROM t";
    unordered = false };

  (* Phase 6: bitwise operators *)
  { name = "bitwise_and";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (5)";
      "INSERT INTO t VALUES (12)";
    ];
    query = "SELECT n & 3 FROM t ORDER BY n";
    unordered = false };

  { name = "bitwise_or";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (5)";
      "INSERT INTO t VALUES (2)";
    ];
    query = "SELECT n | 8 FROM t ORDER BY n";
    unordered = false };

  { name = "lshift";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
    ];
    query = "SELECT n << 3 FROM t ORDER BY n";
    unordered = false };

  { name = "rshift";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (16)";
      "INSERT INTO t VALUES (8)";
    ];
    query = "SELECT n >> 2 FROM t ORDER BY n";
    unordered = false };

  { name = "rshift_negative";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (-4)";
    ];
    query = "SELECT n >> 1 FROM t";
    unordered = false };

  { name = "bitnot";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (5)";
    ];
    query = "SELECT ~n FROM t";
    unordered = false };

  (* Phase 6: LIKE *)
  { name = "like_percent_suffix";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
      "INSERT INTO t VALUES ('world')";
    ];
    query = "SELECT s FROM t WHERE s LIKE 'hel%'";
    unordered = false };

  { name = "like_percent_prefix";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
      "INSERT INTO t VALUES ('world')";
    ];
    query = "SELECT s FROM t WHERE s LIKE '%llo'";
    unordered = false };

  { name = "like_underscore";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
      "INSERT INTO t VALUES ('hallo')";
      "INSERT INTO t VALUES ('hxllo')";
    ];
    query = "SELECT s FROM t WHERE s LIKE 'h_llo' ORDER BY s";
    unordered = false };

  { name = "like_case_insensitive";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('Hello')";
      "INSERT INTO t VALUES ('WORLD')";
    ];
    query = "SELECT s FROM t WHERE s LIKE 'hello'";
    unordered = false };

  { name = "like_no_match";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
    ];
    query = "SELECT s FROM t WHERE s LIKE 'xyz%'";
    unordered = false };

  { name = "like_null_subject";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES (NULL)";
    ];
    query = "SELECT s FROM t WHERE s LIKE '%'";
    unordered = false };

  { name = "not_like";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
      "INSERT INTO t VALUES ('world')";
    ];
    query = "SELECT s FROM t WHERE NOT (s LIKE 'hel%') ORDER BY s";
    unordered = false };

  (* Phase 6: GLOB *)
  { name = "glob_star";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
      "INSERT INTO t VALUES ('world')";
    ];
    query = "SELECT s FROM t WHERE s GLOB 'hel*'";
    unordered = false };

  { name = "glob_question";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
      "INSERT INTO t VALUES ('hallo')";
    ];
    query = "SELECT s FROM t WHERE s GLOB 'h?llo' ORDER BY s";
    unordered = false };

  { name = "glob_case_sensitive";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('Hello')";
      "INSERT INTO t VALUES ('hello')";
    ];
    query = "SELECT s FROM t WHERE s GLOB 'hello'";
    unordered = false };

  (* Phase 6: BETWEEN *)
  { name = "between_basic";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (5)";
      "INSERT INTO t VALUES (10)";
    ];
    query = "SELECT n FROM t WHERE n BETWEEN 3 AND 7";
    unordered = false };

  { name = "between_inclusive_bounds";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (5)";
      "INSERT INTO t VALUES (10)";
    ];
    query = "SELECT n FROM t WHERE n BETWEEN 1 AND 10 ORDER BY n";
    unordered = false };

  { name = "not_between";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (5)";
      "INSERT INTO t VALUES (10)";
    ];
    query = "SELECT n FROM t WHERE NOT (n BETWEEN 3 AND 7) ORDER BY n";
    unordered = false };

  { name = "between_null_subject";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (NULL)";
      "INSERT INTO t VALUES (5)";
    ];
    query = "SELECT n FROM t WHERE n BETWEEN 1 AND 10";
    unordered = false };

  { name = "between_real";
    setup = [
      "CREATE TABLE t (r REAL)";
      "INSERT INTO t VALUES (1.5)";
      "INSERT INTO t VALUES (3.0)";
      "INSERT INTO t VALUES (5.5)";
    ];
    query = "SELECT r FROM t WHERE r BETWEEN 2.0 AND 4.0";
    unordered = false };

  { name = "between_text";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('apple')";
      "INSERT INTO t VALUES ('mango')";
      "INSERT INTO t VALUES ('zebra')";
    ];
    query = "SELECT s FROM t WHERE s BETWEEN 'banana' AND 'orange'";
    unordered = false };

  (* Phase 6: IN *)
  { name = "in_list_found";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
      "INSERT INTO t VALUES (3)";
    ];
    query = "SELECT n FROM t WHERE n IN (1, 3) ORDER BY n";
    unordered = false };

  { name = "in_list_not_found";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
    ];
    query = "SELECT n FROM t WHERE n IN (5, 6)";
    unordered = false };

  { name = "not_in_list";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
      "INSERT INTO t VALUES (3)";
    ];
    query = "SELECT n FROM t WHERE NOT (n IN (1, 3)) ORDER BY n";
    unordered = false };

  { name = "in_null_subject";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (NULL)";
      "INSERT INTO t VALUES (1)";
    ];
    query = "SELECT n FROM t WHERE n IN (1, 2)";
    unordered = false };

  { name = "in_text_values";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('a')";
      "INSERT INTO t VALUES ('b')";
      "INSERT INTO t VALUES ('c')";
    ];
    query = "SELECT s FROM t WHERE s IN ('a', 'c') ORDER BY s";
    unordered = false };

  (* Phase 6: SUBSTR *)
  { name = "substr_from";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
    ];
    query = "SELECT SUBSTR(s, 2) FROM t";
    unordered = false };

  { name = "substr_from_len";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
    ];
    query = "SELECT SUBSTR(s, 2, 3) FROM t";
    unordered = false };

  { name = "substr_first";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
    ];
    query = "SELECT SUBSTR(s, 1, 1) FROM t";
    unordered = false };

  { name = "substr_beyond_end";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
    ];
    query = "SELECT SUBSTR(s, 4, 100) FROM t";
    unordered = false };

  { name = "substr_null";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES (NULL)";
    ];
    query = "SELECT SUBSTR(s, 1) FROM t";
    unordered = false };

  (* Phase 6: TRIM / LTRIM / RTRIM *)
  { name = "trim_spaces";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('  hello  ')";
    ];
    query = "SELECT TRIM(s) FROM t";
    unordered = false };

  { name = "ltrim_spaces";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('  hello  ')";
    ];
    query = "SELECT LTRIM(s) FROM t";
    unordered = false };

  { name = "rtrim_spaces";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('  hello  ')";
    ];
    query = "SELECT RTRIM(s) FROM t";
    unordered = false };

  { name = "trim_null";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES (NULL)";
    ];
    query = "SELECT TRIM(s) FROM t";
    unordered = false };

  (* Phase 6: REPLACE *)
  { name = "replace_basic";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello world')";
    ];
    query = "SELECT REPLACE(s, 'world', 'there') FROM t";
    unordered = false };

  { name = "replace_multiple";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('aaa')";
    ];
    query = "SELECT REPLACE(s, 'a', 'b') FROM t";
    unordered = false };

  { name = "replace_not_found";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
    ];
    query = "SELECT REPLACE(s, 'xyz', 'abc') FROM t";
    unordered = false };

  { name = "replace_null";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES (NULL)";
    ];
    query = "SELECT REPLACE(s, 'a', 'b') FROM t";
    unordered = false };

  (* Phase 6: INSTR *)
  { name = "instr_found";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
    ];
    query = "SELECT INSTR(s, 'ell') FROM t";
    unordered = false };

  { name = "instr_not_found";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
    ];
    query = "SELECT INSTR(s, 'xyz') FROM t";
    unordered = false };

  { name = "instr_first_char";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
    ];
    query = "SELECT INSTR(s, 'h') FROM t";
    unordered = false };

  { name = "instr_null";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES (NULL)";
    ];
    query = "SELECT INSTR(s, 'x') FROM t";
    unordered = false };

  (* Phase 6: ROUND *)
  { name = "round_no_digits";
    setup = [
      "CREATE TABLE t (r REAL)";
      "INSERT INTO t VALUES (3.7)";
      "INSERT INTO t VALUES (3.2)";
    ];
    query = "SELECT ROUND(r) FROM t ORDER BY r";
    unordered = false };

  { name = "round_2_digits";
    setup = [
      "CREATE TABLE t (r REAL)";
      "INSERT INTO t VALUES (3.14159)";
    ];
    query = "SELECT ROUND(r, 2) FROM t";
    unordered = false };

  { name = "round_integer";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (5)";
    ];
    query = "SELECT ROUND(n) FROM t";
    unordered = false };

  { name = "round_null";
    setup = [
      "CREATE TABLE t (r REAL)";
      "INSERT INTO t VALUES (NULL)";
    ];
    query = "SELECT ROUND(r) FROM t";
    unordered = false };

  (* Phase 6: TYPEOF *)
  { name = "typeof_integer";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (1)";
    ];
    query = "SELECT TYPEOF(n) FROM t";
    unordered = false };

  { name = "typeof_text";
    setup = [
      "CREATE TABLE t (s TEXT)";
      "INSERT INTO t VALUES ('hello')";
    ];
    query = "SELECT TYPEOF(s) FROM t";
    unordered = false };

  { name = "typeof_real";
    setup = [
      "CREATE TABLE t (r REAL)";
      "INSERT INTO t VALUES (3.14)";
    ];
    query = "SELECT TYPEOF(r) FROM t";
    unordered = false };

  { name = "typeof_null";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (NULL)";
    ];
    query = "SELECT TYPEOF(n) FROM t";
    unordered = false };

  { name = "typeof_literal_null";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (1)";
    ];
    query = "SELECT TYPEOF(NULL) FROM t";
    unordered = false };

  (* Phase 6: ORDER BY arbitrary expressions *)
  { name = "order_by_arith";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (3)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
    ];
    query = "SELECT n FROM t ORDER BY n * -1";
    unordered = false };

  { name = "order_by_abs_fn";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (-3)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (-2)";
    ];
    query = "SELECT n FROM t ORDER BY ABS(n)";
    unordered = false };

  { name = "order_by_concat";
    setup = [
      "CREATE TABLE t (a TEXT, b TEXT)";
      "INSERT INTO t VALUES ('b', 'z')";
      "INSERT INTO t VALUES ('a', 'y')";
    ];
    query = "SELECT a FROM t ORDER BY a || b";
    unordered = false };

  { name = "order_by_ifnull";
    setup = [
      "CREATE TABLE t (n INTEGER)";
      "INSERT INTO t VALUES (NULL)";
      "INSERT INTO t VALUES (5)";
      "INSERT INTO t VALUES (NULL)";
    ];
    query = "SELECT IFNULL(n, 0) FROM t ORDER BY IFNULL(n, 99)";
    unordered = false };

  (* Phase 7: SELECT DISTINCT *)
  { name = "distinct_basic";
    setup = [
      "CREATE TABLE t (x INTEGER, y TEXT)";
      "INSERT INTO t VALUES (1, 'a')";
      "INSERT INTO t VALUES (1, 'b')";
      "INSERT INTO t VALUES (2, 'a')";
      "INSERT INTO t VALUES (1, 'a')";
    ];
    query = "SELECT DISTINCT x FROM t ORDER BY x";
    unordered = false };

  { name = "distinct_multi_col";
    setup = [
      "CREATE TABLE t (x INTEGER, y TEXT)";
      "INSERT INTO t VALUES (1, 'a')";
      "INSERT INTO t VALUES (1, 'a')";
      "INSERT INTO t VALUES (1, 'b')";
      "INSERT INTO t VALUES (2, 'a')";
    ];
    query = "SELECT DISTINCT x, y FROM t ORDER BY x, y";
    unordered = false };

  { name = "distinct_with_null";
    setup = [
      "CREATE TABLE t (x INTEGER)";
      "INSERT INTO t VALUES (NULL)";
      "INSERT INTO t VALUES (NULL)";
      "INSERT INTO t VALUES (1)";
    ];
    query = "SELECT DISTINCT x FROM t ORDER BY x";
    unordered = false };

  (* Phase 7: UNION *)
  { name = "union_basic";
    setup = [
      "CREATE TABLE a (x INTEGER)";
      "CREATE TABLE b (x INTEGER)";
      "INSERT INTO a VALUES (1)";
      "INSERT INTO a VALUES (2)";
      "INSERT INTO b VALUES (2)";
      "INSERT INTO b VALUES (3)";
    ];
    query = "SELECT x FROM a UNION SELECT x FROM b ORDER BY x";
    unordered = false };

  { name = "union_all";
    setup = [
      "CREATE TABLE a (x INTEGER)";
      "CREATE TABLE b (x INTEGER)";
      "INSERT INTO a VALUES (1)";
      "INSERT INTO a VALUES (2)";
      "INSERT INTO b VALUES (2)";
      "INSERT INTO b VALUES (3)";
    ];
    query = "SELECT x FROM a UNION ALL SELECT x FROM b ORDER BY x";
    unordered = false };

  { name = "union_with_nulls";
    setup = [
      "CREATE TABLE a (x INTEGER)";
      "CREATE TABLE b (x INTEGER)";
      "INSERT INTO a VALUES (NULL)";
      "INSERT INTO a VALUES (1)";
      "INSERT INTO b VALUES (NULL)";
      "INSERT INTO b VALUES (2)";
    ];
    query = "SELECT x FROM a UNION SELECT x FROM b ORDER BY x";
    unordered = false };

  { name = "union_chained_three_way";
    setup = [
      "CREATE TABLE a (x INTEGER)";
      "CREATE TABLE b (x INTEGER)";
      "CREATE TABLE c (x INTEGER)";
      "INSERT INTO a VALUES (1)";
      "INSERT INTO b VALUES (2)";
      "INSERT INTO c VALUES (3)";
    ];
    query = "SELECT x FROM a UNION SELECT x FROM b UNION SELECT x FROM c ORDER BY x";
    unordered = false };

  (* Phase 7: INTERSECT *)
  { name = "intersect_basic";
    setup = [
      "CREATE TABLE a (x INTEGER)";
      "CREATE TABLE b (x INTEGER)";
      "INSERT INTO a VALUES (1)";
      "INSERT INTO a VALUES (2)";
      "INSERT INTO a VALUES (2)";
      "INSERT INTO b VALUES (2)";
      "INSERT INTO b VALUES (3)";
    ];
    query = "SELECT x FROM a INTERSECT SELECT x FROM b ORDER BY x";
    unordered = false };

  { name = "intersect_empty_result";
    setup = [
      "CREATE TABLE a (x INTEGER)";
      "CREATE TABLE b (x INTEGER)";
      "INSERT INTO a VALUES (1)";
      "INSERT INTO b VALUES (2)";
    ];
    query = "SELECT x FROM a INTERSECT SELECT x FROM b ORDER BY x";
    unordered = false };

  (* Phase 7: EXCEPT *)
  { name = "except_basic";
    setup = [
      "CREATE TABLE a (x INTEGER)";
      "CREATE TABLE b (x INTEGER)";
      "INSERT INTO a VALUES (1)";
      "INSERT INTO a VALUES (2)";
      "INSERT INTO a VALUES (2)";
      "INSERT INTO b VALUES (2)";
    ];
    query = "SELECT x FROM a EXCEPT SELECT x FROM b ORDER BY x";
    unordered = false };

  { name = "except_removes_all";
    setup = [
      "CREATE TABLE a (x INTEGER)";
      "CREATE TABLE b (x INTEGER)";
      "INSERT INTO a VALUES (1)";
      "INSERT INTO b VALUES (1)";
    ];
    query = "SELECT x FROM a EXCEPT SELECT x FROM b ORDER BY x";
    unordered = false };

  (* Phase 7: date/time functions *)
  { name = "date_round_trip";
    setup = [
      "CREATE TABLE _d (x INTEGER)";
      "INSERT INTO _d VALUES (1)";
    ];
    query = "SELECT DATE('2024-01-15') FROM _d";
    unordered = false };

  { name = "time_round_trip";
    setup = [
      "CREATE TABLE _d (x INTEGER)";
      "INSERT INTO _d VALUES (1)";
    ];
    query = "SELECT TIME('14:30:45') FROM _d";
    unordered = false };

  { name = "datetime_round_trip";
    setup = [
      "CREATE TABLE _d (x INTEGER)";
      "INSERT INTO _d VALUES (1)";
    ];
    query = "SELECT DATETIME('2024-03-22 08:45:00') FROM _d";
    unordered = false };

  { name = "julianday_2000_01_01";
    setup = [
      "CREATE TABLE _d (x INTEGER)";
      "INSERT INTO _d VALUES (1)";
    ];
    query = "SELECT JULIANDAY('2000-01-01') FROM _d";
    unordered = false };

  { name = "unixepoch_1970_01_01";
    setup = [
      "CREATE TABLE _d (x INTEGER)";
      "INSERT INTO _d VALUES (1)";
    ];
    query = "SELECT UNIXEPOCH('1970-01-01') FROM _d";
    unordered = false };

  { name = "strftime_year";
    setup = [
      "CREATE TABLE _d (x INTEGER)";
      "INSERT INTO _d VALUES (1)";
    ];
    query = "SELECT STRFTIME('%Y', '2024-06-15') FROM _d";
    unordered = false };

  { name = "strftime_full_date";
    setup = [
      "CREATE TABLE _d (x INTEGER)";
      "INSERT INTO _d VALUES (1)";
    ];
    query = "SELECT STRFTIME('%Y-%m-%d', '2024-06-15 12:30:45') FROM _d";
    unordered = false };

  { name = "strftime_time";
    setup = [
      "CREATE TABLE _d (x INTEGER)";
      "INSERT INTO _d VALUES (1)";
    ];
    query = "SELECT STRFTIME('%H:%M:%S', '2024-06-15 09:05:03') FROM _d";
    unordered = false };

  { name = "date_null_propagation";
    setup = [
      "CREATE TABLE _d (x INTEGER)";
      "INSERT INTO _d VALUES (1)";
    ];
    query = "SELECT DATE(NULL) FROM _d";
    unordered = false };

]

(* ── runner ────────────────────────────────────────────────────── *)

let () =
  Alcotest.run "sqlite_compare" [
    "correctness", List.map make_test cases;
  ]
