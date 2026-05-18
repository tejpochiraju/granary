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

  (* ── ON CONFLICT tests ─────────────────────────────────────────── *)

  { name = "insert_or_ignore_basic";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
      "CREATE UNIQUE INDEX idx_id ON t (id)";
      "INSERT INTO t VALUES (1, 'original')";
      "INSERT OR IGNORE INTO t VALUES (1, 'ignored')";
    ];
    query = "SELECT id, v FROM t ORDER BY id";
    unordered = false };

  { name = "insert_or_ignore_unique_idx";
    setup = [
      "CREATE TABLE t (id INTEGER, name TEXT)";
      "CREATE UNIQUE INDEX idx_name ON t (name)";
      "INSERT INTO t VALUES (1, 'alice')";
      "INSERT OR IGNORE INTO t VALUES (2, 'alice')";
    ];
    query = "SELECT COUNT(*) FROM t";
    unordered = false };

  { name = "insert_or_replace_basic";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
      "CREATE UNIQUE INDEX idx_id ON t (id)";
      "INSERT INTO t VALUES (1, 'old')";
      "INSERT OR REPLACE INTO t VALUES (1, 'new')";
    ];
    query = "SELECT v FROM t WHERE id = 1";
    unordered = false };

  { name = "insert_or_replace_count";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
      "CREATE UNIQUE INDEX idx_id ON t (id)";
      "INSERT INTO t VALUES (1, 'first')";
      "INSERT OR REPLACE INTO t VALUES (1, 'second')";
    ];
    query = "SELECT COUNT(*) FROM t";
    unordered = false };

  { name = "insert_or_replace_no_conflict";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
      "CREATE UNIQUE INDEX idx_id ON t (id)";
      "INSERT OR REPLACE INTO t VALUES (42, 'inserted')";
    ];
    query = "SELECT id, v FROM t";
    unordered = false };

  { name = "insert_or_ignore_no_conflict";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
      "CREATE UNIQUE INDEX idx_id ON t (id)";
      "INSERT OR IGNORE INTO t VALUES (7, 'hello')";
    ];
    query = "SELECT id, v FROM t";
    unordered = false };

  { name = "insert_or_replace_multi_unique";
    setup = [
      "CREATE TABLE t (a INTEGER, b INTEGER)";
      "CREATE UNIQUE INDEX idx_a ON t (a)";
      "CREATE UNIQUE INDEX idx_b ON t (b)";
      "INSERT INTO t VALUES (1, 100)";
      "INSERT INTO t VALUES (2, 200)";
      "INSERT OR REPLACE INTO t VALUES (1, 200)";
    ];
    query = "SELECT COUNT(*) FROM t";
    unordered = false };

  { name = "insert_or_ignore_multiple_rows";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
      "CREATE UNIQUE INDEX idx_id ON t (id)";
      "INSERT INTO t VALUES (1, 'a')";
      "INSERT INTO t VALUES (2, 'b')";
      "INSERT OR IGNORE INTO t VALUES (1, 'conflict')";
      "INSERT OR IGNORE INTO t VALUES (3, 'new')";
    ];
    query = "SELECT id, v FROM t ORDER BY id";
    unordered = false };

  (* ── RETURNING tests ───────────────────────────────────────────── *)

  { name = "insert_returning_cols";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
    ];
    query = "INSERT INTO t VALUES (5, 'hello') RETURNING id, v";
    unordered = false };

  { name = "insert_returning_expr";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
    ];
    query = "INSERT INTO t VALUES (3, 'x') RETURNING id * 2";
    unordered = false };

  { name = "update_returning_single";
    setup = [
      "CREATE TABLE t (id INTEGER, val TEXT)";
      "INSERT INTO t VALUES (1, 'old')";
      "INSERT INTO t VALUES (2, 'keep')";
    ];
    query = "UPDATE t SET val = 'new' WHERE id = 1 RETURNING id, val";
    unordered = false };

  { name = "update_returning_multi";
    setup = [
      "CREATE TABLE t (id INTEGER, v INTEGER)";
      "INSERT INTO t VALUES (1, 10)";
      "INSERT INTO t VALUES (2, 20)";
      "INSERT INTO t VALUES (3, 30)";
    ];
    query = "UPDATE t SET v = v + 1 RETURNING id, v";
    unordered = true };

  { name = "delete_returning_single";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
      "INSERT INTO t VALUES (1, 'gone')";
      "INSERT INTO t VALUES (2, 'stays')";
    ];
    query = "DELETE FROM t WHERE id = 1 RETURNING id, v";
    unordered = false };

  { name = "delete_returning_all";
    setup = [
      "CREATE TABLE t (id INTEGER)";
      "INSERT INTO t VALUES (3)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
    ];
    query = "DELETE FROM t RETURNING id";
    unordered = true };

  { name = "insert_or_ignore_returning_empty";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
      "CREATE UNIQUE INDEX idx_id ON t (id)";
      "INSERT INTO t VALUES (1, 'orig')";
    ];
    query = "INSERT OR IGNORE INTO t VALUES (1, 'new') RETURNING id, v";
    unordered = false };

  { name = "update_returning_new_values";
    setup = [
      "CREATE TABLE t (id INTEGER, score INTEGER)";
      "INSERT INTO t VALUES (1, 100)";
    ];
    query = "UPDATE t SET score = score * 2 WHERE id = 1 RETURNING id, score";
    unordered = false };

  (* ── ALTER TABLE tests ─────────────────────────────────────────── *)

  { name = "alter_add_column_default";
    setup = [
      "CREATE TABLE t (id INTEGER, name TEXT)";
      "INSERT INTO t VALUES (1, 'alice')";
      "INSERT INTO t VALUES (2, 'bob')";
      "ALTER TABLE t ADD COLUMN tag TEXT DEFAULT 'unknown'";
    ];
    query = "SELECT id, name, tag FROM t ORDER BY id";
    unordered = false };

  { name = "alter_add_column_null";
    setup = [
      "CREATE TABLE t (id INTEGER)";
      "INSERT INTO t VALUES (1)";
      "ALTER TABLE t ADD COLUMN extra TEXT";
    ];
    query = "SELECT id, extra FROM t";
    unordered = false };

  { name = "alter_add_then_insert";
    setup = [
      "CREATE TABLE t (id INTEGER, name TEXT)";
      "INSERT INTO t VALUES (1, 'first')";
      "ALTER TABLE t ADD COLUMN score INTEGER DEFAULT 0";
      "INSERT INTO t VALUES (2, 'second', 99)";
    ];
    query = "SELECT id, name, score FROM t ORDER BY id";
    unordered = false };

  { name = "alter_add_multiple";
    setup = [
      "CREATE TABLE t (id INTEGER)";
      "INSERT INTO t VALUES (42)";
      "ALTER TABLE t ADD COLUMN a TEXT DEFAULT 'foo'";
      "ALTER TABLE t ADD COLUMN b INTEGER DEFAULT 7";
    ];
    query = "SELECT id, a, b FROM t";
    unordered = false };

  { name = "alter_rename_table";
    setup = [
      "CREATE TABLE old_name (id INTEGER, v TEXT)";
      "INSERT INTO old_name VALUES (1, 'hello')";
      "ALTER TABLE old_name RENAME TO new_name";
    ];
    query = "SELECT id, v FROM new_name ORDER BY id";
    unordered = false };

  { name = "alter_rename_column";
    setup = [
      "CREATE TABLE t (id INTEGER, old_col TEXT)";
      "INSERT INTO t VALUES (1, 'data')";
      "ALTER TABLE t RENAME COLUMN old_col TO new_col";
    ];
    query = "SELECT id, new_col FROM t";
    unordered = false };

  { name = "alter_rename_column_no_keyword";
    setup = [
      "CREATE TABLE t (x INTEGER, y TEXT)";
      "INSERT INTO t VALUES (5, 'abc')";
      "ALTER TABLE t RENAME y TO z";
    ];
    query = "SELECT x, z FROM t";
    unordered = false };

  { name = "alter_add_select_star";
    setup = [
      "CREATE TABLE t (a INTEGER)";
      "INSERT INTO t VALUES (10)";
      "ALTER TABLE t ADD COLUMN b TEXT DEFAULT 'yes'";
    ];
    query = "SELECT * FROM t";
    unordered = false };

  (* ── Table-level constraints tests ────────────────────────────── *)

  { name = "table_unique_single";
    setup = [
      "CREATE TABLE t (a INTEGER, b TEXT, UNIQUE(a))";
      "INSERT INTO t VALUES (1, 'x')";
    ];
    query = "SELECT COUNT(*) FROM t";
    unordered = false };

  { name = "table_unique_multi";
    setup = [
      "CREATE TABLE t (a INTEGER, b INTEGER, UNIQUE(a, b))";
      "INSERT INTO t VALUES (1, 1)";
      "INSERT INTO t VALUES (1, 2)";
      "INSERT INTO t VALUES (2, 1)";
    ];
    query = "SELECT COUNT(*) FROM t";
    unordered = false };

  { name = "table_unique_allows_different_values";
    setup = [
      "CREATE TABLE t (a INTEGER, b TEXT, UNIQUE(a))";
      "INSERT INTO t VALUES (1, 'first')";
      "INSERT INTO t VALUES (2, 'second')";
      "INSERT INTO t VALUES (3, 'third')";
    ];
    query = "SELECT COUNT(*) FROM t";
    unordered = false };

  { name = "table_pk_constraint";
    setup = [
      "CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY(id))";
      "INSERT INTO t VALUES (1, 'alice')";
    ];
    query = "SELECT COUNT(*) FROM t";
    unordered = false };

  { name = "table_unique_replace";
    setup = [
      "CREATE TABLE t (a INTEGER, b TEXT, UNIQUE(a))";
      "INSERT INTO t VALUES (1, 'old')";
      "INSERT OR REPLACE INTO t VALUES (1, 'new')";
    ];
    query = "SELECT a, b FROM t";
    unordered = false };

  { name = "table_unique_ignore";
    setup = [
      "CREATE TABLE t (a INTEGER, b TEXT, UNIQUE(a))";
      "INSERT INTO t VALUES (1, 'first')";
      "INSERT OR IGNORE INTO t VALUES (1, 'second')";
    ];
    query = "SELECT a, b FROM t";
    unordered = false };

  { name = "table_multi_constraint";
    setup = [
      "CREATE TABLE t (a INTEGER, b INTEGER, UNIQUE(a), UNIQUE(b))";
      "INSERT INTO t VALUES (1, 10)";
      "INSERT INTO t VALUES (2, 20)";
    ];
    query = "SELECT COUNT(*) FROM t";
    unordered = false };

  { name = "table_unique_then_index";
    setup = [
      "CREATE TABLE t (a INTEGER, b TEXT, UNIQUE(a))";
      "CREATE INDEX idx_b ON t (b)";
      "INSERT INTO t VALUES (1, 'hello')";
      "INSERT INTO t VALUES (2, 'world')";
    ];
    query = "SELECT a, b FROM t ORDER BY a";
    unordered = false };

]

(* ── phase9_subqueries test cases ─────────────────────────────── *)

let phase9_subquery_cases = [

  { name = "scalar_max";
    setup = [
      "CREATE TABLE t (id INTEGER PRIMARY KEY)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
      "INSERT INTO t VALUES (3)";
    ];
    query = "SELECT (SELECT max(id) FROM t) FROM t LIMIT 1";
    unordered = false };

  { name = "scalar_min_where";
    setup = [
      "CREATE TABLE t (id INTEGER PRIMARY KEY)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
      "INSERT INTO t VALUES (3)";
    ];
    query = "SELECT id FROM t WHERE id = (SELECT min(id) FROM t)";
    unordered = false };

  { name = "exists_true";
    setup = [
      "CREATE TABLE t (id INTEGER PRIMARY KEY)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
    ];
    query = "SELECT count(*) FROM t WHERE EXISTS (SELECT 1 FROM t WHERE id = 1)";
    unordered = false };

  { name = "exists_false";
    setup = [
      "CREATE TABLE empty (id INTEGER PRIMARY KEY)";
    ];
    query = "SELECT count(*) FROM empty WHERE EXISTS (SELECT 1 FROM empty)";
    unordered = false };

  { name = "in_select";
    setup = [
      "CREATE TABLE t (id INTEGER PRIMARY KEY)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
      "INSERT INTO t VALUES (3)";
      "CREATE TABLE ids (id INTEGER)";
      "INSERT INTO ids VALUES (1)";
      "INSERT INTO ids VALUES (3)";
    ];
    query = "SELECT id FROM t WHERE id IN (SELECT id FROM ids) ORDER BY id ASC";
    unordered = false };

  { name = "not_in_select";
    setup = [
      "CREATE TABLE t (id INTEGER PRIMARY KEY)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
      "INSERT INTO t VALUES (3)";
      "CREATE TABLE excluded (id INTEGER)";
      "INSERT INTO excluded VALUES (2)";
    ];
    query = "SELECT id FROM t WHERE id NOT IN (SELECT id FROM excluded) ORDER BY id ASC";
    unordered = false };

  { name = "in_select_empty";
    setup = [
      "CREATE TABLE t (id INTEGER PRIMARY KEY)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
      "CREATE TABLE empty (id INTEGER)";
    ];
    query = "SELECT id FROM t WHERE id IN (SELECT id FROM empty)";
    unordered = false };

  { name = "from_less_select";
    setup = [];
    query = "SELECT 1 + 1";
    unordered = false };

]

(* ── phase9_fk test cases ──────────────────────────────────────── *)

let phase9_fk_cases = [

  { name = "fk_accepted";
    setup = [
      "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)";
      "INSERT INTO users VALUES (1, 'alice')";
      "CREATE TABLE orders (id INTEGER PRIMARY KEY, user_id INTEGER REFERENCES users(id))";
    ];
    query = "SELECT count(*) FROM orders";
    unordered = false };

  { name = "fk_no_col";
    setup = [
      "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)";
      "INSERT INTO users VALUES (1, 'alice')";
      "CREATE TABLE orders (id INTEGER PRIMARY KEY, user_id INTEGER REFERENCES users)";
    ];
    query = "SELECT count(*) FROM orders";
    unordered = false };

  { name = "fk_no_enforcement";
    setup = [
      "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)";
      "CREATE TABLE orders (id INTEGER PRIMARY KEY, user_id INTEGER REFERENCES users(id))";
      "INSERT INTO orders VALUES (1, 999)";
    ];
    query = "SELECT id, user_id FROM orders";
    unordered = false };

  { name = "fk_with_not_null";
    setup = [
      "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)";
      "INSERT INTO users VALUES (1, 'bob')";
      "CREATE TABLE orders (id INTEGER PRIMARY KEY, user_id INTEGER NOT NULL REFERENCES users(id))";
      "INSERT INTO orders VALUES (1, 1)";
    ];
    query = "SELECT id, user_id FROM orders";
    unordered = false };

]

(* ── phase9_check error-comparison infrastructure ─────────────── *)
(* These tests verify that BOTH sqlocaml and SQLite raise an error.  *)
(* We do not compare exact error messages.                           *)

(** Run [setup] statements in sqlocaml; return true if the last one raises an
    error (all preceding ones must succeed). *)
let sqlocaml_last_setup_fails setup =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let n = List.length setup in
    let prefix = List.filteri (fun i _ -> i < n - 1) setup in
    let last   = List.nth setup (n - 1) in
    let* () = Lwt_list.iter_s (fun sql ->
      let* r = Db.execute db sql in
      (match r with
       | Ok () -> ()
       | Error e -> Alcotest.failf "sqlocaml unexpected setup error for %S: %a" sql Db.pp_error e);
      Lwt.return_unit
    ) prefix in
    let* r = Db.execute db last in
    Lwt.return (match r with Error _ -> true | Ok () -> false)
  )

(** Run [setup] statements via sqlite3 up to the last; return true if the
    last one produces non-empty output (i.e. an error line). *)
let sqlite3_last_setup_fails ~db_path setup =
  let n = List.length setup in
  let prefix = List.filteri (fun i _ -> i < n - 1) setup in
  let last   = List.nth setup (n - 1) in
  sqlite3_run_setup ~db_path prefix;
  (* Run last statement and capture stderr/stdout *)
  let cmd = Printf.sprintf
    "sqlite3 %s %s 2>&1"
    (Filename.quote db_path) (Filename.quote last) in
  let ic = Unix.open_process_in cmd in
  let output = ref [] in
  (try while true do output := input_line ic :: !output done
   with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  (* If sqlite3 printed anything, it was an error *)
  !output <> []

type check_error_case = {
  ce_name  : string;
  ce_setup : string list;  (* last statement must fail *)
}

let run_check_error_case ce =
  let db_path = fresh_db_path () in
  Fun.protect
    ~finally:(fun () -> try Unix.unlink db_path with _ -> ())
    (fun () ->
      let sq_fails = sqlocaml_last_setup_fails ce.ce_setup in
      let sl_fails = sqlite3_last_setup_fails ~db_path ce.ce_setup in
      if not sq_fails then
        Alcotest.failf "%s: sqlocaml did not raise an error (expected CHECK violation)"
          ce.ce_name;
      if not sl_fails then
        Alcotest.failf "%s: sqlite3 did not raise an error (expected CHECK violation)"
          ce.ce_name)

let make_check_error_test ce =
  Alcotest.test_case ce.ce_name `Quick (fun () ->
    if sqlite3_available () then run_check_error_case ce
    else begin
      (* Without sqlite3, at least verify sqlocaml raises an error *)
      let sq_fails = sqlocaml_last_setup_fails ce.ce_setup in
      if not sq_fails then
        Alcotest.failf "%s: sqlocaml did not raise an error (expected CHECK violation)"
          ce.ce_name
    end
  )

(* ── phase9_check test cases ───────────────────────────────────── *)

let phase9_check_cases = [

  { name = "check_valid_insert";
    setup = [
      "CREATE TABLE t (id INTEGER PRIMARY KEY, price REAL CHECK (price > 0))";
      "INSERT INTO t VALUES (1, 9.99)";
    ];
    query = "SELECT count(*) FROM t";
    unordered = false };

  { name = "check_null_passes";
    setup = [
      "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER CHECK (v > 0))";
      "INSERT INTO t VALUES (1, NULL)";
    ];
    query = "SELECT count(*) FROM t";
    unordered = false };

  { name = "check_valid_update";
    setup = [
      "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER CHECK (v >= 0))";
      "INSERT INTO t VALUES (1, 5)";
      "UPDATE t SET v = 10 WHERE id = 1";
    ];
    query = "SELECT v FROM t WHERE id = 1";
    unordered = false };

  { name = "check_compound_expr";
    setup = [
      "CREATE TABLE t (id INTEGER PRIMARY KEY, price REAL CHECK (price > 0 AND price < 10000))";
      "INSERT INTO t VALUES (1, 99.0)";
      "INSERT INTO t VALUES (2, 5000.0)";
    ];
    query = "SELECT count(*) FROM t";
    unordered = false };

  { name = "check_between";
    setup = [
      "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER CHECK (v BETWEEN 0 AND 100))";
      "INSERT INTO t VALUES (1, 50)";
      "INSERT INTO t VALUES (2, 0)";
      "INSERT INTO t VALUES (3, 100)";
    ];
    query = "SELECT count(*) FROM t";
    unordered = false };

]

let phase9_check_error_cases = [

  { ce_name  = "check_violation_insert";
    ce_setup = [
      "CREATE TABLE t (id INTEGER PRIMARY KEY, price REAL CHECK (price > 0))";
      "INSERT INTO t VALUES (1, -5.0)";
    ] };

  { ce_name  = "check_violation_update";
    ce_setup = [
      "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER CHECK (v >= 0))";
      "INSERT INTO t VALUES (1, 5)";
      "UPDATE t SET v = -1 WHERE id = 1";
    ] };

  { ce_name  = "check_function_expr";
    ce_setup = [
      "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT CHECK (LENGTH(name) > 0))";
      "INSERT INTO t VALUES (1, '')";
    ] };

]

(* ── phase10_case_when comparison cases ───────────────────────── *)

let phase10_case_when_cases = [
  { name    = "searched_basic";
    setup   = ["CREATE TABLE t (v INTEGER)";
               "INSERT INTO t VALUES (3)";
               "INSERT INTO t VALUES (-1)";
               "INSERT INTO t VALUES (0)"];
    query   = "SELECT CASE WHEN v > 0 THEN 'pos' WHEN v < 0 THEN 'neg' ELSE 'zero' END FROM t ORDER BY v";
    unordered = false };

  { name    = "simple_basic";
    setup   = ["CREATE TABLE t (v INTEGER)";
               "INSERT INTO t VALUES (1)";
               "INSERT INTO t VALUES (2)";
               "INSERT INTO t VALUES (99)"];
    query   = "SELECT CASE v WHEN 1 THEN 'one' WHEN 2 THEN 'two' ELSE 'other' END FROM t ORDER BY v";
    unordered = false };

  { name    = "no_else_no_match";
    setup   = ["CREATE TABLE t (v INTEGER)"; "INSERT INTO t VALUES (5)"];
    query   = "SELECT CASE WHEN v = 0 THEN 'zero' END FROM t";
    unordered = false };

  { name    = "case_in_where";
    setup   = ["CREATE TABLE t (v INTEGER)";
               "INSERT INTO t VALUES (1)";
               "INSERT INTO t VALUES (2)";
               "INSERT INTO t VALUES (3)"];
    query   = "SELECT v FROM t WHERE CASE WHEN v > 1 THEN 1 ELSE 0 END = 1 ORDER BY v";
    unordered = false };

  { name    = "case_arithmetic_result";
    setup   = ["CREATE TABLE t (v INTEGER)"; "INSERT INTO t VALUES (4)"];
    query   = "SELECT CASE WHEN v > 2 THEN v * 10 ELSE v END FROM t";
    unordered = false };

  { name    = "case_with_null";
    setup   = ["CREATE TABLE t (v INTEGER)"; "INSERT INTO t VALUES (NULL)"];
    query   = "SELECT CASE WHEN v IS NULL THEN 'is_null' ELSE 'not_null' END FROM t";
    unordered = false };

  { name    = "case_simple_null_eq_semantics";
    setup   = ["CREATE TABLE t (v INTEGER)"; "INSERT INTO t VALUES (NULL)"];
    query   = "SELECT CASE v WHEN NULL THEN 1 ELSE 0 END FROM t";
    unordered = false };

  { name    = "case_in_select_and_where";
    setup   = ["CREATE TABLE t (n INTEGER, s TEXT)";
               "INSERT INTO t VALUES (1, 'a')";
               "INSERT INTO t VALUES (2, 'b')"];
    query   = "SELECT CASE n WHEN 1 THEN 'first' ELSE 'rest' END, s FROM t ORDER BY n";
    unordered = false };
]

(* ── phase10_multi_join comparison cases ─────────────────────── *)

let phase10_multi_join_cases = [
  { name    = "three_table_join";
    setup   = ["CREATE TABLE a (id INTEGER, name TEXT)";
               "CREATE TABLE b (aid INTEGER, val INTEGER)";
               "CREATE TABLE c (bid INTEGER, extra TEXT)";
               "INSERT INTO a VALUES (1, 'alice')";
               "INSERT INTO b VALUES (1, 42)";
               "INSERT INTO c VALUES (42, 'extra')"];
    query   = "SELECT a.name, b.val, c.extra FROM a JOIN b ON a.id = b.aid JOIN c ON b.val = c.bid";
    unordered = false };

  { name    = "two_joins_filtered";
    setup   = ["CREATE TABLE u (id INTEGER, name TEXT)";
               "CREATE TABLE o (uid INTEGER, item TEXT)";
               "CREATE TABLE p (item TEXT, price INTEGER)";
               "INSERT INTO u VALUES (1, 'alice')";
               "INSERT INTO u VALUES (2, 'bob')";
               "INSERT INTO o VALUES (1, 'hat')";
               "INSERT INTO o VALUES (2, 'book')";
               "INSERT INTO p VALUES ('hat', 10)";
               "INSERT INTO p VALUES ('book', 5)"];
    query   = "SELECT u.name, p.price FROM u JOIN o ON u.id = o.uid JOIN p ON o.item = p.item ORDER BY u.name";
    unordered = false };

  { name    = "two_left_joins";
    setup   = ["CREATE TABLE a (id INTEGER)";
               "CREATE TABLE b (aid INTEGER, v TEXT)";
               "CREATE TABLE c (aid INTEGER, w TEXT)";
               "INSERT INTO a VALUES (1), (2)";
               "INSERT INTO b VALUES (1, 'B1')";
               "INSERT INTO c VALUES (2, 'C2')"];
    query   = "SELECT a.id, b.v, c.w FROM a LEFT JOIN b ON a.id = b.aid LEFT JOIN c ON a.id = c.aid ORDER BY a.id";
    unordered = false };

  { name    = "three_join_count";
    setup   = ["CREATE TABLE dept (id INTEGER, dname TEXT)";
               "CREATE TABLE emp (id INTEGER, dept_id INTEGER)";
               "CREATE TABLE sal (emp_id INTEGER, amount INTEGER)";
               "INSERT INTO dept VALUES (1, 'eng')";
               "INSERT INTO dept VALUES (2, 'sales')";
               "INSERT INTO emp VALUES (1, 1)";
               "INSERT INTO emp VALUES (2, 1)";
               "INSERT INTO emp VALUES (3, 2)";
               "INSERT INTO sal VALUES (1, 100)";
               "INSERT INTO sal VALUES (2, 90)";
               "INSERT INTO sal VALUES (3, 80)"];
    query   = "SELECT dept.dname, COUNT(emp.id) FROM dept JOIN emp ON dept.id = emp.dept_id JOIN sal ON emp.id = sal.emp_id GROUP BY dname ORDER BY dname";
    unordered = false };

  { name    = "case_in_multi_join";
    setup   = ["CREATE TABLE a (id INTEGER)";
               "CREATE TABLE b (aid INTEGER, v INTEGER)";
               "INSERT INTO a VALUES (1), (2)";
               "INSERT INTO b VALUES (1, 10), (2, 20)"];
    query   = "SELECT a.id, CASE WHEN b.v > 15 THEN 'big' ELSE 'small' END FROM a JOIN b ON a.id = b.aid ORDER BY a.id";
    unordered = false };
]

(* ── Phase 11: CAST, NULLIF, IIF ──────────────────────────────── *)

let phase11_cast_cases = [
  { name    = "cast_int_to_text";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (42)"];
    query   = "SELECT CAST(n AS TEXT) FROM t";
    unordered = false };

  { name    = "cast_text_to_int";
    setup   = ["CREATE TABLE t (s TEXT)"; "INSERT INTO t VALUES ('99')"];
    query   = "SELECT CAST(s AS INTEGER) FROM t";
    unordered = false };

  { name    = "cast_float_prefix_to_int";
    setup   = ["CREATE TABLE t (s TEXT)"; "INSERT INTO t VALUES ('3.7')"];
    query   = "SELECT CAST(s AS INTEGER) FROM t";
    unordered = false };

  { name    = "cast_real_to_int";
    setup   = ["CREATE TABLE t (r REAL)"; "INSERT INTO t VALUES (7.9)"];
    query   = "SELECT CAST(r AS INTEGER) FROM t";
    unordered = false };

  { name    = "cast_int_to_real";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (3)"];
    query   = "SELECT CAST(n AS REAL) FROM t";
    unordered = false };

  { name    = "cast_null_is_null";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (NULL)"];
    query   = "SELECT CAST(n AS TEXT) FROM t";
    unordered = false };

  { name    = "cast_in_where";
    setup   = ["CREATE TABLE t (s TEXT)";
               "INSERT INTO t VALUES ('10')";
               "INSERT INTO t VALUES ('3')"];
    query   = "SELECT s FROM t WHERE CAST(s AS INTEGER) > 5 ORDER BY s";
    unordered = false };

  { name    = "nullif_equal";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (5)"];
    query   = "SELECT NULLIF(n, 5) FROM t";
    unordered = false };

  { name    = "nullif_unequal";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (5)"];
    query   = "SELECT NULLIF(n, 3) FROM t";
    unordered = false };

  { name    = "iif_true";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (10)"];
    query   = "SELECT IIF(n > 5, 'big', 'small') FROM t";
    unordered = false };

  { name    = "iif_false";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (2)"];
    query   = "SELECT IIF(n > 5, 'big', 'small') FROM t";
    unordered = false };

  { name    = "cast_text_invalid_to_int";
    setup   = ["CREATE TABLE t (s TEXT)"; "INSERT INTO t VALUES ('abc')"];
    query   = "SELECT CAST(s AS INTEGER) FROM t";
    unordered = false };
]

(* ── Phase 11: aliases ─────────────────────────────────────────── *)

let phase11_alias_cases = [
  { name    = "col_alias_basic";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (4)"];
    query   = "SELECT n * 2 AS doubled FROM t";
    unordered = false };

  { name    = "col_alias_order_by";
    setup   = ["CREATE TABLE t (n INTEGER)";
               "INSERT INTO t VALUES (3)";
               "INSERT INTO t VALUES (1)";
               "INSERT INTO t VALUES (2)"];
    query   = "SELECT n * 10 AS big FROM t ORDER BY big";
    unordered = false };

  { name    = "col_alias_multiple";
    setup   = ["CREATE TABLE t (a INTEGER, b INTEGER)";
               "INSERT INTO t VALUES (2, 3)"];
    query   = "SELECT a + b AS total, a * b AS product FROM t";
    unordered = false };

  { name    = "tbl_alias_from";
    setup   = ["CREATE TABLE products (id INTEGER, name TEXT)";
               "INSERT INTO products VALUES (1, 'apple')"];
    query   = "SELECT p.id, p.name FROM products AS p";
    unordered = false };

  { name    = "tbl_alias_join";
    setup   = ["CREATE TABLE a (id INTEGER, val TEXT)";
               "CREATE TABLE b (aid INTEGER, extra TEXT)";
               "INSERT INTO a VALUES (1, 'x')";
               "INSERT INTO b VALUES (1, 'y')"];
    query   = "SELECT x.val, y.extra FROM a AS x JOIN b AS y ON x.id = y.aid";
    unordered = false };

  { name    = "tbl_alias_where";
    setup   = ["CREATE TABLE t (n INTEGER)";
               "INSERT INTO t VALUES (1)";
               "INSERT INTO t VALUES (2)"];
    query   = "SELECT r.n FROM t AS r WHERE r.n > 1 ORDER BY r.n";
    unordered = false };

  { name    = "nullif_null_arg";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (NULL)"];
    query   = "SELECT NULLIF(n, 0) FROM t";
    unordered = false };

  { name    = "cast_in_join_on";
    setup   = ["CREATE TABLE t (id INTEGER)";
               "CREATE TABLE u (sid TEXT)";
               "INSERT INTO t VALUES (1)";
               "INSERT INTO u VALUES ('1')"];
    query   = "SELECT t.id FROM t JOIN u ON t.id = CAST(u.sid AS INTEGER)";
    unordered = false };
]

(* ── runner ────────────────────────────────────────────────────── *)

let () =
  Alcotest.run "sqlite_compare" [
    "correctness",       List.map make_test cases;
    "phase9_subqueries", List.map make_test phase9_subquery_cases;
    "phase9_check",      (List.map make_test phase9_check_cases
                          @ List.map make_check_error_test phase9_check_error_cases);
    "phase9_fk",         List.map make_test phase9_fk_cases;
    "phase10_case_when", List.map make_test phase10_case_when_cases;
    "phase10_multi_join", List.map make_test phase10_multi_join_cases;
    "phase11_cast",      List.map make_test phase11_cast_cases;
    "phase11_alias",     List.map make_test phase11_alias_cases;
  ]
