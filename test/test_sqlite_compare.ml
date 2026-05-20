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
  (* Run all setup statements in a single sqlite3 invocation so that
     transactional statements (BEGIN/SAVEPOINT/…) share one connection. *)
  match stmts with
  | [] -> ()
  | _ ->
    let batch = String.concat "; " stmts in
    let cmd = Printf.sprintf "sqlite3 %s %s >/dev/null 2>&1"
      (Filename.quote db_path) (Filename.quote batch) in
    ignore (Sys.command cmd)

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

(* ── Phase 12: CTEs ────────────────────────────────────────────── *)

let phase12_cte_cases = [
  { name = "cte_basic";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
      "INSERT INTO t VALUES (1, 'a')";
      "INSERT INTO t VALUES (2, 'b')";
    ];
    query = "WITH cte AS (SELECT id, v FROM t) SELECT * FROM cte ORDER BY id";
    unordered = false };

  { name = "cte_filter";
    setup = [
      "CREATE TABLE t (id INTEGER, v INTEGER)";
      "INSERT INTO t VALUES (1, 10)";
      "INSERT INTO t VALUES (2, 20)";
      "INSERT INTO t VALUES (3, 30)";
    ];
    query = "WITH cte AS (SELECT id, v FROM t) SELECT * FROM cte WHERE v > 10 ORDER BY id";
    unordered = false };

  { name = "cte_agg";
    setup = [
      "CREATE TABLE orders (uid INTEGER, amt INTEGER)";
      "INSERT INTO orders VALUES (1, 100)";
      "INSERT INTO orders VALUES (1, 50)";
      "INSERT INTO orders VALUES (2, 200)";
    ];
    query = "WITH totals AS (SELECT uid, SUM(amt) AS total FROM orders GROUP BY uid) SELECT * FROM totals ORDER BY uid";
    unordered = false };

  { name = "cte_order";
    setup = [
      "CREATE TABLE t (x INTEGER)";
      "INSERT INTO t VALUES (3)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
    ];
    query = "WITH cte AS (SELECT x FROM t) SELECT * FROM cte ORDER BY x";
    unordered = false };

  { name = "cte_limit";
    setup = [
      "CREATE TABLE t (x INTEGER)";
      "INSERT INTO t VALUES (3)";
      "INSERT INTO t VALUES (1)";
      "INSERT INTO t VALUES (2)";
    ];
    query = "WITH cte AS (SELECT x FROM t ORDER BY x) SELECT * FROM cte LIMIT 2";
    unordered = false };
]

(* ── Phase 12: multi-row INSERT ────────────────────────────────── *)

let phase12_multirow_cases = [
  { name = "multirow_basic";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
      "INSERT INTO t VALUES (1, 'a'), (2, 'b'), (3, 'c')";
    ];
    query = "SELECT * FROM t ORDER BY id";
    unordered = false };

  { name = "multirow_single";
    setup = [
      "CREATE TABLE t (x INTEGER)";
      "INSERT INTO t VALUES (42)";
    ];
    query = "SELECT * FROM t";
    unordered = false };

  { name = "multirow_count";
    setup = [
      "CREATE TABLE t (a INTEGER, b INTEGER)";
      "INSERT INTO t VALUES (1,10),(2,20),(3,30),(4,40),(5,50)";
    ];
    query = "SELECT COUNT(*), SUM(a), SUM(b) FROM t";
    unordered = false };

  { name = "multirow_on_conflict_ignore";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
      "CREATE UNIQUE INDEX u ON t (id)";
      "INSERT OR IGNORE INTO t VALUES (1,'a'),(1,'b'),(2,'c')";
    ];
    query = "SELECT * FROM t ORDER BY id";
    unordered = false };
]

(* ── Phase 12: correlated subqueries ───────────────────────────── *)

let phase12_correlated_cases = [
  { name = "corr_exists";
    setup = [
      "CREATE TABLE u (id INTEGER, name TEXT)";
      "CREATE TABLE o (uid INTEGER)";
      "INSERT INTO u VALUES (1,'alice'),(2,'bob')";
      "INSERT INTO o VALUES (1)";
    ];
    query = "SELECT name FROM u WHERE EXISTS (SELECT 1 FROM o WHERE o.uid = u.id) ORDER BY name";
    unordered = false };

  { name = "corr_not_exists";
    setup = [
      "CREATE TABLE u (id INTEGER, name TEXT)";
      "CREATE TABLE o (uid INTEGER)";
      "INSERT INTO u VALUES (1,'alice'),(2,'bob')";
      "INSERT INTO o VALUES (1)";
    ];
    query = "SELECT name FROM u WHERE NOT EXISTS (SELECT 1 FROM o WHERE o.uid = u.id) ORDER BY name";
    unordered = false };

  { name = "corr_in_select";
    setup = [
      "CREATE TABLE u (id INTEGER, name TEXT)";
      "CREATE TABLE o (uid INTEGER)";
      "INSERT INTO u VALUES (1,'alice'),(2,'bob')";
      "INSERT INTO o VALUES (1)";
    ];
    query = "SELECT name FROM u WHERE u.id IN (SELECT uid FROM o WHERE o.uid = u.id) ORDER BY name";
    unordered = false };

  { name = "corr_not_in";
    setup = [
      "CREATE TABLE u (id INTEGER, name TEXT)";
      "CREATE TABLE blocked (uid INTEGER)";
      "INSERT INTO u VALUES (1,'alice'),(2,'bob')";
      "INSERT INTO blocked VALUES (1)";
    ];
    query = "SELECT name FROM u WHERE u.id NOT IN (SELECT uid FROM blocked WHERE blocked.uid = u.id) ORDER BY name";
    unordered = false };
]

(* ── Phase 13: UPSERT comparison tests ─────────────────────────── *)

let phase13_upsert_cases = [
  { name = "upsert_basic";
    setup = [
      "CREATE TABLE kv (k INTEGER NOT NULL, v TEXT NOT NULL)";
      "CREATE UNIQUE INDEX kv_k ON kv (k)";
      "INSERT INTO kv VALUES (1, 'old')";
      "INSERT INTO kv VALUES (1, 'new') ON CONFLICT(k) DO UPDATE SET v = excluded.v";
    ];
    query = "SELECT k, v FROM kv";
    unordered = false };

  { name = "upsert_no_conflict";
    setup = [
      "CREATE TABLE kv (k INTEGER NOT NULL, v TEXT NOT NULL)";
      "CREATE UNIQUE INDEX kv_k ON kv (k)";
      "INSERT INTO kv VALUES (1, 'first')";
      "INSERT INTO kv VALUES (2, 'second') ON CONFLICT(k) DO UPDATE SET v = excluded.v";
    ];
    query = "SELECT k, v FROM kv ORDER BY k";
    unordered = false };

  { name = "upsert_increment";
    setup = [
      "CREATE TABLE counters (name TEXT NOT NULL, cnt INTEGER NOT NULL)";
      "CREATE UNIQUE INDEX cnt_name ON counters (name)";
      "INSERT INTO counters VALUES ('hits', 1)";
      "INSERT INTO counters VALUES ('hits', 0) ON CONFLICT(name) DO UPDATE SET cnt = cnt + 1";
    ];
    query = "SELECT name, cnt FROM counters";
    unordered = false };

  { name = "upsert_multi_assign";
    setup = [
      "CREATE TABLE t (id INTEGER, a TEXT, b TEXT)";
      "CREATE UNIQUE INDEX t_id ON t (id)";
      "INSERT INTO t VALUES (1, 'a1', 'b1')";
      "INSERT INTO t VALUES (1, 'a2', 'b2') ON CONFLICT(id) DO UPDATE SET a = excluded.a, b = excluded.b";
    ];
    query = "SELECT a, b FROM t WHERE id = 1";
    unordered = false };

  { name = "upsert_preserves_others";
    setup = [
      "CREATE TABLE kv (k INTEGER, v TEXT)";
      "CREATE UNIQUE INDEX kv_k2 ON kv (k)";
      "INSERT INTO kv VALUES (1, 'a'), (2, 'b'), (3, 'c')";
      "INSERT INTO kv VALUES (2, 'B') ON CONFLICT(k) DO UPDATE SET v = excluded.v";
    ];
    query = "SELECT k, v FROM kv ORDER BY k";
    unordered = false };
]

(* ── Phase 13: Views comparison tests ──────────────────────────── *)

let phase13_view_cases = [
  { name = "view_basic";
    setup = [
      "CREATE TABLE users (id INTEGER, name TEXT, active INTEGER)";
      "INSERT INTO users VALUES (1, 'Alice', 1), (2, 'Bob', 0), (3, 'Carol', 1)";
      "CREATE VIEW active_users AS SELECT id, name FROM users WHERE active = 1";
    ];
    query = "SELECT name FROM active_users ORDER BY id";
    unordered = false };

  { name = "view_filter";
    setup = [
      "CREATE TABLE products (id INTEGER, name TEXT, price INTEGER)";
      "INSERT INTO products VALUES (1, 'A', 10), (2, 'B', 20), (3, 'C', 5)";
      "CREATE VIEW cheap AS SELECT id, name, price FROM products WHERE price < 15";
    ];
    query = "SELECT name FROM cheap ORDER BY price";
    unordered = false };

  { name = "view_aggregation";
    setup = [
      "CREATE TABLE orders (customer TEXT, amount INTEGER)";
      "INSERT INTO orders VALUES ('Alice', 100), ('Alice', 200), ('Bob', 50)";
      "CREATE VIEW order_counts AS SELECT customer, COUNT(*) AS cnt FROM orders GROUP BY customer";
    ];
    query = "SELECT customer, cnt FROM order_counts ORDER BY customer";
    unordered = false };

  { name = "view_count";
    setup = [
      "CREATE TABLE t (x INTEGER)";
      "INSERT INTO t VALUES (1), (2), (3), (4), (5)";
      "CREATE VIEW big AS SELECT x FROM t WHERE x > 2";
    ];
    query = "SELECT COUNT(*) FROM big";
    unordered = false };

  { name = "view_sum";
    setup = [
      "CREATE TABLE t (x INTEGER)";
      "INSERT INTO t VALUES (10), (20), (30)";
      "CREATE VIEW all_t AS SELECT x FROM t";
    ];
    query = "SELECT SUM(x) FROM all_t";
    unordered = false };
]

let phase14_window_cases = [
  { name = "window_row_number";
    setup = [
      "CREATE TABLE emp (dept TEXT, name TEXT, salary INTEGER)";
      "INSERT INTO emp VALUES ('eng','Alice',90000),('eng','Bob',80000),('hr','Carol',70000),('hr','Dave',60000)";
    ];
    query = "SELECT name, ROW_NUMBER() OVER (PARTITION BY dept ORDER BY salary DESC) AS rn FROM emp ORDER BY dept, salary DESC";
    unordered = false };

  { name = "window_rank";
    setup = [
      "CREATE TABLE scores (name TEXT, score INTEGER)";
      "INSERT INTO scores VALUES ('A',100),('B',100),('C',90),('D',80)";
    ];
    query = "SELECT name, RANK() OVER (ORDER BY score DESC) AS r FROM scores ORDER BY name";
    unordered = false };

  { name = "window_dense_rank";
    setup = [
      "CREATE TABLE scores (name TEXT, score INTEGER)";
      "INSERT INTO scores VALUES ('A',100),('B',100),('C',90),('D',80)";
    ];
    query = "SELECT name, DENSE_RANK() OVER (ORDER BY score DESC) AS dr FROM scores ORDER BY name";
    unordered = false };

  { name = "window_lag";
    setup = [
      "CREATE TABLE vals (id INTEGER, v INTEGER)";
      "INSERT INTO vals VALUES (1,10),(2,20),(3,30)";
    ];
    query = "SELECT id, v, LAG(v, 1, 0) OVER (ORDER BY id) AS prev FROM vals ORDER BY id";
    unordered = false };

  { name = "window_lead";
    setup = [
      "CREATE TABLE vals (id INTEGER, v INTEGER)";
      "INSERT INTO vals VALUES (1,10),(2,20),(3,30)";
    ];
    query = "SELECT id, v, LEAD(v, 1, 0) OVER (ORDER BY id) AS nxt FROM vals ORDER BY id";
    unordered = false };

  { name = "window_sum_running";
    setup = [
      "CREATE TABLE sales (id INTEGER, amount INTEGER)";
      "INSERT INTO sales VALUES (1,100),(2,200),(3,300)";
    ];
    query = "SELECT id, SUM(amount) OVER (ORDER BY id) AS running FROM sales ORDER BY id";
    unordered = false };

  { name = "window_partition_sum";
    setup = [
      "CREATE TABLE t (dept TEXT, v INTEGER)";
      "INSERT INTO t VALUES ('a',1),('a',2),('b',10),('b',20)";
    ];
    query = "SELECT dept, v, SUM(v) OVER (PARTITION BY dept) AS dept_total FROM t ORDER BY dept, v";
    unordered = false };

  { name = "window_count_star";
    setup = [
      "CREATE TABLE t (x INTEGER)";
      "INSERT INTO t VALUES (3),(1),(2)";
    ];
    query = "SELECT x, COUNT(*) OVER () AS total FROM t ORDER BY x";
    unordered = false };
]

let phase14_recursive_cte_cases = [
  { name = "recursive_series";
    setup = [];
    query = "WITH RECURSIVE cnt AS (SELECT 1 AS n UNION ALL SELECT n + 1 FROM cnt WHERE n < 5) SELECT n FROM cnt ORDER BY n";
    unordered = false };

  { name = "recursive_sum";
    setup = [];
    query = "WITH RECURSIVE nums AS (SELECT 1 AS i, 1 AS s UNION ALL SELECT i + 1, s + (i + 1) FROM nums WHERE i < 4) SELECT s FROM nums WHERE i = 4";
    unordered = false };

  { name = "recursive_tree";
    setup = [
      "CREATE TABLE tree (id INTEGER, parent INTEGER)";
      "INSERT INTO tree VALUES (1,0),(2,1),(3,1),(4,2)";
    ];
    query = "WITH RECURSIVE anc AS (SELECT id FROM tree WHERE parent = 1 UNION ALL SELECT t.id FROM tree AS t INNER JOIN anc AS a ON t.parent = a.id) SELECT id FROM anc ORDER BY id";
    unordered = false };

  { name = "recursive_fibonacci";
    setup = [];
    query = "WITH RECURSIVE fib AS (SELECT 0 AS a, 1 AS b UNION ALL SELECT b, a + b FROM fib WHERE a < 10) SELECT a FROM fib ORDER BY a";
    unordered = false };
]

let phase16_window_alias_cases = [
  { name = "window_orderby_alias_rn";
    setup = [
      "CREATE TABLE emp (dept TEXT, name TEXT, salary INTEGER)";
      "INSERT INTO emp VALUES ('eng','Alice',90000),('eng','Bob',80000),('hr','Carol',70000),('hr','Dave',60000)";
    ];
    query = "SELECT name, ROW_NUMBER() OVER (PARTITION BY dept ORDER BY salary DESC) AS rn FROM emp ORDER BY dept, rn";
    unordered = false };

  { name = "window_orderby_alias_dr";
    setup = [
      "CREATE TABLE scores (name TEXT, score INTEGER)";
      "INSERT INTO scores VALUES ('A',100),('B',100),('C',90),('D',80)";
    ];
    query = "SELECT name, DENSE_RANK() OVER (ORDER BY score DESC) AS dr FROM scores ORDER BY dr, name";
    unordered = false };
]

let phase16_collate_cases = [
  { name = "collate_nocase_eq";
    setup = [
      "CREATE TABLE t (name TEXT)";
      "INSERT INTO t VALUES ('Alice'),('BOB'),('charlie')";
    ];
    query = "SELECT name FROM t WHERE name COLLATE NOCASE = 'alice' ORDER BY name";
    unordered = false };

  { name = "collate_nocase_order";
    setup = [
      "CREATE TABLE t (name TEXT)";
      "INSERT INTO t VALUES ('banana'),('Apple'),('cherry')";
    ];
    query = "SELECT name FROM t ORDER BY name COLLATE NOCASE";
    unordered = false };
]

let phase16_drop_column_cases = [
  { name = "drop_column_basic";
    setup = [
      "CREATE TABLE t (id INTEGER, name TEXT, age INTEGER)";
      "INSERT INTO t VALUES (1,'Alice',30),(2,'Bob',25)";
      "ALTER TABLE t DROP COLUMN age";
    ];
    query = "SELECT id, name FROM t ORDER BY id";
    unordered = false };

  { name = "drop_column_then_insert";
    setup = [
      "CREATE TABLE t (id INTEGER, name TEXT, score REAL)";
      "ALTER TABLE t DROP COLUMN score";
      "INSERT INTO t (id, name) VALUES (1,'Alice'),(2,'Bob')";
    ];
    query = "SELECT id, name FROM t ORDER BY id";
    unordered = false };
]

(* ── Phase 17 tests ────────────────────────────────────────────── *)

let phase17_frame_cases = [
  { name = "rows_1_preceding_sum";
    setup = [
      "CREATE TABLE fr1 (n INTEGER)";
      "INSERT INTO fr1 VALUES (1)";
      "INSERT INTO fr1 VALUES (2)";
      "INSERT INTO fr1 VALUES (3)";
      "INSERT INTO fr1 VALUES (4)";
      "INSERT INTO fr1 VALUES (5)";
    ];
    query = "SELECT n, SUM(n) OVER (ORDER BY n ROWS BETWEEN 1 PRECEDING AND CURRENT ROW) AS s FROM fr1 ORDER BY n";
    unordered = false };

  { name = "rows_unbounded_preceding_sum";
    setup = [
      "CREATE TABLE fr2 (n INTEGER)";
      "INSERT INTO fr2 VALUES (10)";
      "INSERT INTO fr2 VALUES (20)";
      "INSERT INTO fr2 VALUES (30)";
    ];
    query = "SELECT n, SUM(n) OVER (ORDER BY n ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS s FROM fr2 ORDER BY n";
    unordered = false };

  { name = "rows_centered_sum";
    setup = [
      "CREATE TABLE fr3 (n INTEGER)";
      "INSERT INTO fr3 VALUES (1)";
      "INSERT INTO fr3 VALUES (2)";
      "INSERT INTO fr3 VALUES (3)";
      "INSERT INTO fr3 VALUES (4)";
      "INSERT INTO fr3 VALUES (5)";
    ];
    query = "SELECT n, SUM(n) OVER (ORDER BY n ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS s FROM fr3 ORDER BY n";
    unordered = false };
]

let phase17_pctrank_cases = [
  { name = "percent_rank";
    setup = [
      "CREATE TABLE pr1 (s INTEGER)";
      "INSERT INTO pr1 VALUES (10)";
      "INSERT INTO pr1 VALUES (20)";
      "INSERT INTO pr1 VALUES (20)";
      "INSERT INTO pr1 VALUES (30)";
    ];
    query = "SELECT s, CAST(ROUND(PERCENT_RANK() OVER (ORDER BY s), 3) AS TEXT) AS pr FROM pr1 ORDER BY s";
    unordered = false };

  { name = "cume_dist";
    setup = [
      "CREATE TABLE cd1 (s INTEGER)";
      "INSERT INTO cd1 VALUES (10)";
      "INSERT INTO cd1 VALUES (20)";
      "INSERT INTO cd1 VALUES (20)";
      "INSERT INTO cd1 VALUES (30)";
    ];
    query = "SELECT s, CAST(ROUND(CUME_DIST() OVER (ORDER BY s), 2) AS TEXT) AS cd FROM cd1 ORDER BY s";
    unordered = false };

  { name = "percent_rank_desc";
    setup = [
      "CREATE TABLE pr2 (s INTEGER)";
      "INSERT INTO pr2 VALUES (10)";
      "INSERT INTO pr2 VALUES (20)";
      "INSERT INTO pr2 VALUES (20)";
      "INSERT INTO pr2 VALUES (30)";
    ];
    query = "SELECT s, CAST(ROUND(PERCENT_RANK() OVER (ORDER BY s DESC), 3) AS TEXT) AS pr FROM pr2 ORDER BY s DESC";
    unordered = false };
]

let phase17_ine_cases = [
  { name = "create_table_ine";
    setup = [
      "CREATE TABLE ine1 (id INTEGER)";
      "INSERT INTO ine1 VALUES (1)";
      "CREATE TABLE IF NOT EXISTS ine1 (id INTEGER, extra TEXT)";
    ];
    query = "SELECT COUNT(*) FROM ine1";
    unordered = false };

  { name = "create_index_ine";
    setup = [
      "CREATE TABLE ine2 (id INTEGER)";
      "CREATE INDEX ine2_idx ON ine2 (id)";
      "CREATE INDEX IF NOT EXISTS ine2_idx ON ine2 (id)";
    ];
    query = "SELECT COUNT(*) FROM ine2";
    unordered = false };
]

let phase17_cast_real_cases = [
  { name = "cast_real_to_text";
    setup = [
      "CREATE TABLE cast_t (v REAL)";
      "INSERT INTO cast_t VALUES (1.0)";
      "INSERT INTO cast_t VALUES (10.0)";
      "INSERT INTO cast_t VALUES (-5.0)";
      "INSERT INTO cast_t VALUES (3.14)";
    ];
    query = "SELECT CAST(v AS TEXT) FROM cast_t ORDER BY v";
    unordered = false };
]

(* ── Phase 18 tests ────────────────────────────────────────────── *)

let phase18_multigroup_cases = [
  (* Two-column GROUP BY with COUNT *)
  { name = "two_col_count";
    setup = [
      "CREATE TABLE mg1 (dept TEXT, role TEXT, n INTEGER)";
      "INSERT INTO mg1 VALUES ('eng','dev',1)";
      "INSERT INTO mg1 VALUES ('eng','dev',2)";
      "INSERT INTO mg1 VALUES ('eng','mgr',3)";
      "INSERT INTO mg1 VALUES ('hr','dev',4)";
    ];
    query = "SELECT dept, role, COUNT(*) FROM mg1 GROUP BY dept, role ORDER BY dept, role";
    unordered = false };

  (* Two-column GROUP BY with SUM *)
  { name = "two_col_sum";
    setup = [
      "CREATE TABLE mg2 (a TEXT, b TEXT, v INTEGER)";
      "INSERT INTO mg2 VALUES ('x','p',10)";
      "INSERT INTO mg2 VALUES ('x','p',20)";
      "INSERT INTO mg2 VALUES ('x','q',30)";
      "INSERT INTO mg2 VALUES ('y','p',40)";
    ];
    query = "SELECT a, b, SUM(v) FROM mg2 GROUP BY a, b ORDER BY a, b";
    unordered = false };

  (* Two-column GROUP BY with HAVING *)
  { name = "two_col_having";
    setup = [
      "CREATE TABLE mg3 (dept TEXT, role TEXT)";
      "INSERT INTO mg3 VALUES ('eng','dev')";
      "INSERT INTO mg3 VALUES ('eng','dev')";
      "INSERT INTO mg3 VALUES ('eng','mgr')";
      "INSERT INTO mg3 VALUES ('hr','dev')";
    ];
    query = "SELECT dept, role, COUNT(*) FROM mg3 GROUP BY dept, role HAVING COUNT(*) > 1 ORDER BY dept, role";
    unordered = false };

  (* Three-column GROUP BY *)
  { name = "three_col_sum";
    setup = [
      "CREATE TABLE mg4 (a TEXT, b TEXT, c TEXT, n INTEGER)";
      "INSERT INTO mg4 VALUES ('x','y','z',1)";
      "INSERT INTO mg4 VALUES ('x','y','z',2)";
      "INSERT INTO mg4 VALUES ('x','y','w',3)";
    ];
    query = "SELECT a, b, c, SUM(n) FROM mg4 GROUP BY a, b, c ORDER BY c, a, b";
    unordered = false };
]

let phase18_fk_cases = [
  (* Valid FK inserts succeed *)
  { name = "fk_valid_inserts";
    setup = [
      "CREATE TABLE fk_p1 (id INTEGER PRIMARY KEY)";
      "INSERT INTO fk_p1 VALUES (1)";
      "INSERT INTO fk_p1 VALUES (2)";
      "CREATE TABLE fk_c1 (id INTEGER, pid INTEGER REFERENCES fk_p1(id))";
      "INSERT INTO fk_c1 VALUES (10, 1)";
      "INSERT INTO fk_c1 VALUES (20, 2)";
    ];
    query = "SELECT COUNT(*) FROM fk_c1";
    unordered = false };

  (* FK with NULL allowed *)
  { name = "fk_null_allowed";
    setup = [
      "CREATE TABLE fk_p2 (id INTEGER PRIMARY KEY)";
      "INSERT INTO fk_p2 VALUES (1)";
      "CREATE TABLE fk_c2 (id INTEGER, pid INTEGER REFERENCES fk_p2(id))";
      "INSERT INTO fk_c2 VALUES (10, NULL)";
      "INSERT INTO fk_c2 VALUES (20, 1)";
    ];
    query = "SELECT COUNT(*) FROM fk_c2";
    unordered = false };
]

(* ── Phase 19 tests ────────────────────────────────────────────── *)

let phase19_math_cases = [
  { name = "ceil_pos";    setup = []; query = "SELECT CEIL(2.3)";       unordered = false };
  { name = "ceil_neg";    setup = []; query = "SELECT CEIL(-2.3)";      unordered = false };
  { name = "floor_pos";   setup = []; query = "SELECT FLOOR(2.7)";      unordered = false };
  { name = "floor_neg";   setup = []; query = "SELECT FLOOR(-2.7)";     unordered = false };
  { name = "sqrt_exact";  setup = []; query = "SELECT SQRT(4.0)";       unordered = false };
  { name = "pow_exact";   setup = []; query = "SELECT POW(2.0, 10.0)";  unordered = false };
  { name = "sign_neg";    setup = []; query = "SELECT SIGN(-5)";        unordered = false };
  { name = "sign_zero";   setup = []; query = "SELECT SIGN(0)";         unordered = false };
  { name = "sign_pos";    setup = []; query = "SELECT SIGN(5)";         unordered = false };
  { name = "trunc_pos";   setup = []; query = "SELECT TRUNC(3.7)";      unordered = false };
  { name = "trunc_neg";   setup = []; query = "SELECT TRUNC(-3.7)";     unordered = false };
  { name = "log2_exact";  setup = []; query = "SELECT LOG2(8.0)";       unordered = false };
  { name = "log10_exact"; setup = []; query = "SELECT LOG10(1000.0)";   unordered = false };
  { name = "sin_zero";    setup = []; query = "SELECT SIN(0.0)";        unordered = false };
  { name = "cos_zero";    setup = []; query = "SELECT COS(0.0)";        unordered = false };
  { name = "tan_zero";    setup = []; query = "SELECT TAN(0.0)";        unordered = false };
  { name = "exp_zero";    setup = []; query = "SELECT EXP(0.0)";        unordered = false };
  { name = "ln_one";      setup = []; query = "SELECT LN(1.0)";         unordered = false };
  { name = "ceil_alias";  setup = []; query = "SELECT CEILING(2.3)";    unordered = false };
  { name = "pow_alias";   setup = []; query = "SELECT POWER(2.0, 3.0)"; unordered = false };
  (* TRUNCATE is a sqlocaml extension not present in SQLite, skipped from comparison *)
]

let phase19_nulls_cases =
  let setup = ["CREATE TABLE tn (x INTEGER)";
               "INSERT INTO tn VALUES (1)";
               "INSERT INTO tn VALUES (NULL)";
               "INSERT INTO tn VALUES (3)"] in
  [
    { name = "nulls last asc";   setup; query = "SELECT x FROM tn ORDER BY x ASC NULLS LAST";   unordered = false };
    { name = "nulls first asc";  setup; query = "SELECT x FROM tn ORDER BY x ASC NULLS FIRST";  unordered = false };
    { name = "nulls first desc"; setup; query = "SELECT x FROM tn ORDER BY x DESC NULLS FIRST"; unordered = false };
    { name = "nulls last desc";  setup; query = "SELECT x FROM tn ORDER BY x DESC NULLS LAST";  unordered = false };
  ]

let phase19_window_agg_cases =
  let setup = ["CREATE TABLE wagg (dept TEXT, sal INTEGER)";
               "INSERT INTO wagg VALUES ('eng', 100)";
               "INSERT INTO wagg VALUES ('eng', 200)";
               "INSERT INTO wagg VALUES ('mkt', 150)";
               "INSERT INTO wagg VALUES ('mkt', 50)"] in
  [
    { name = "rank over sum";
      setup;
      query = "SELECT dept, SUM(sal), RANK() OVER (ORDER BY SUM(sal) DESC) FROM wagg GROUP BY dept ORDER BY dept";
      unordered = false };
    { name = "dense_rank over count";
      setup = ["CREATE TABLE wagg (dept TEXT, sal INTEGER)";
               "INSERT INTO wagg VALUES ('eng', 100)";
               "INSERT INTO wagg VALUES ('eng', 200)";
               "INSERT INTO wagg VALUES ('mkt', 150)"];
      query = "SELECT dept, COUNT(*), DENSE_RANK() OVER (ORDER BY COUNT(*) DESC) FROM wagg GROUP BY dept ORDER BY dept";
      unordered = false };
  ]

let phase20_json_cases = [
  { name = "extract_int";
    setup = []; unordered = false;
    query = {|SELECT json_extract('{"a":1,"b":2}', '$.a')|} };
  { name = "extract_text";
    setup = []; unordered = false;
    query = {|SELECT json_extract('{"a":"hi"}', '$.a')|} };
  { name = "extract_array_idx";
    setup = []; unordered = false;
    query = {|SELECT json_extract('[10,20,30]', '$[1]')|} };
  { name = "extract_nested";
    setup = []; unordered = false;
    query = {|SELECT json_extract('{"a":{"b":99}}', '$.a.b')|} };
  { name = "extract_missing";
    setup = []; unordered = false;
    query = {|SELECT json_extract('{"a":1}', '$.z')|} };
  { name = "json_object_int";
    setup = []; unordered = false;
    query = {|SELECT json_object('a', 1, 'b', 2)|} };
  { name = "json_object_text";
    setup = []; unordered = false;
    query = {|SELECT json_object('k', 'hello')|} };
  { name = "json_array_ints";
    setup = []; unordered = false;
    query = {|SELECT json_array(1, 2, 3)|} };
  { name = "json_array_empty";
    setup = []; unordered = false;
    query = {|SELECT json_array()|} };
  { name = "json_type_object";
    setup = []; unordered = false;
    query = {|SELECT json_type('{"a":1}')|} };
  { name = "json_type_array";
    setup = []; unordered = false;
    query = {|SELECT json_type('[1,2]')|} };
  { name = "json_type_text";
    setup = []; unordered = false;
    query = {|SELECT json_type('"hello"')|} };
  { name = "json_type_integer";
    setup = []; unordered = false;
    query = {|SELECT json_type('42')|} };
  { name = "json_type_null";
    setup = []; unordered = false;
    query = {|SELECT json_type('null')|} };
  { name = "json_type_with_path";
    setup = []; unordered = false;
    query = {|SELECT json_type('{"a":1}', '$.a')|} };
  { name = "json_valid_true";
    setup = []; unordered = false;
    query = {|SELECT json_valid('{"a":1}')|} };
  { name = "json_valid_false";
    setup = []; unordered = false;
    query = {|SELECT json_valid('not json')|} };
  { name = "json_type_true";
    setup = []; unordered = false;
    query = {|SELECT json_type('true')|} };
  { name = "json_type_false";
    setup = []; unordered = false;
    query = {|SELECT json_type('false')|} };
]

let phase20_json_mutation_cases = [
  { name = "json_set_existing";
    setup = []; unordered = false;
    query = {|SELECT json_set('{"a":1}', '$.a', 99)|} };
  { name = "json_set_new";
    setup = []; unordered = false;
    query = {|SELECT json_set('{"a":1}', '$.b', 2)|} };
  { name = "json_insert_existing_noop";
    setup = []; unordered = false;
    query = {|SELECT json_insert('{"a":1}', '$.a', 99)|} };
  { name = "json_insert_new";
    setup = []; unordered = false;
    query = {|SELECT json_insert('{"a":1}', '$.b', 2)|} };
  { name = "json_replace_existing";
    setup = []; unordered = false;
    query = {|SELECT json_replace('{"a":1}', '$.a', 99)|} };
  { name = "json_replace_missing_noop";
    setup = []; unordered = false;
    query = {|SELECT json_replace('{"a":1}', '$.b', 2)|} };
  { name = "json_remove_key";
    setup = []; unordered = false;
    query = {|SELECT json_remove('{"a":1,"b":2}', '$.a')|} };
  { name = "json_remove_array_elem";
    setup = []; unordered = false;
    query = {|SELECT json_remove('[1,2,3]', '$[1]')|} };
  { name = "json_set_null_input";
    setup = []; unordered = false;
    query = {|SELECT json_set(NULL, '$.a', 1)|} };
  { name = "json_set_array_append";
    setup = []; unordered = false;
    query = {|SELECT json_set('[1,2,3]', '$[3]', 4)|} };
  { name = "json_remove_nonexistent";
    setup = []; unordered = false;
    query = {|SELECT json_remove('{"a":1}', '$.b')|} };
]

(* ── Phase 21: SAVEPOINT ────────────────────────────────────────── *)

let phase21_savepoint_cases = [
  { name = "rollback_undoes_insert";
    setup = [
      "CREATE TABLE sp1 (x INTEGER)";
      "BEGIN";
      "SAVEPOINT s";
      "INSERT INTO sp1 VALUES (1)";
      "ROLLBACK TO s";
      "COMMIT";
    ];
    query = "SELECT COUNT(*) FROM sp1";
    unordered = false };

  { name = "release_keeps_insert";
    setup = [
      "CREATE TABLE sp2 (x INTEGER)";
      "BEGIN";
      "SAVEPOINT s";
      "INSERT INTO sp2 VALUES (42)";
      "RELEASE s";
      "COMMIT";
    ];
    query = "SELECT x FROM sp2";
    unordered = false };

  { name = "partial_rollback";
    setup = [
      "CREATE TABLE sp3 (x INTEGER)";
      "BEGIN";
      "INSERT INTO sp3 VALUES (1)";
      "SAVEPOINT s";
      "INSERT INTO sp3 VALUES (2)";
      "ROLLBACK TO s";
      "COMMIT";
    ];
    query = "SELECT COUNT(*) FROM sp3";
    unordered = false };

  { name = "nested_savepoints";
    setup = [
      "CREATE TABLE sp4 (x INTEGER)";
      "BEGIN";
      "SAVEPOINT outer";
      "INSERT INTO sp4 VALUES (1)";
      "SAVEPOINT inner";
      "INSERT INTO sp4 VALUES (2)";
      "ROLLBACK TO inner";
      "RELEASE inner";
      "COMMIT";
    ];
    query = "SELECT COUNT(*) FROM sp4";
    unordered = false };
]

(* ── Phase 21: FK DELETE/UPDATE parent-side ─────────────────────── *)

let phase21_fk_cases = [
  { name = "delete_unreferenced_parent_ok";
    setup = [
      "CREATE TABLE fkpd (id INTEGER PRIMARY KEY)";
      "INSERT INTO fkpd VALUES (1)";
      "INSERT INTO fkpd VALUES (2)";
      "CREATE TABLE fkcd (pid INTEGER REFERENCES fkpd(id))";
      "INSERT INTO fkcd VALUES (2)";
      "DELETE FROM fkpd WHERE id = 1";
    ];
    query = "SELECT COUNT(*) FROM fkpd";
    unordered = false };

  { name = "update_unreferenced_parent_ok";
    setup = [
      "CREATE TABLE fkpu (id INTEGER PRIMARY KEY)";
      "INSERT INTO fkpu VALUES (1)";
      "INSERT INTO fkpu VALUES (2)";
      "CREATE TABLE fkcu (pid INTEGER REFERENCES fkpu(id))";
      "INSERT INTO fkcu VALUES (1)";
      "UPDATE fkpu SET id = 99 WHERE id = 2";
    ];
    query = "SELECT COUNT(*) FROM fkpu";
    unordered = false };

  { name = "delete_parent_null_child_ok";
    setup = [
      "CREATE TABLE fkpn (id INTEGER PRIMARY KEY)";
      "INSERT INTO fkpn VALUES (1)";
      "CREATE TABLE fkcn (pid INTEGER REFERENCES fkpn(id))";
      "INSERT INTO fkcn VALUES (NULL)";
      "DELETE FROM fkpn WHERE id = 1";
    ];
    query = "SELECT COUNT(*) FROM fkpn";
    unordered = false };
]

(* ── Phase 22: Triggers ─────────────────────────────────────────── *)

let phase22_trigger_cases = [
  { name = "after_insert_trigger";
    setup = [
      "CREATE TABLE t (id INTEGER, val TEXT)";
      "CREATE TABLE audit (t_id INTEGER, action TEXT)";
      "CREATE TRIGGER t_ai AFTER INSERT ON t BEGIN INSERT INTO audit VALUES (NEW.id, 'INSERT'); END";
      "INSERT INTO t VALUES (1, 'hello')";
      "INSERT INTO t VALUES (2, 'world')";
    ];
    query = "SELECT t_id, action FROM audit ORDER BY t_id";
    unordered = false };

  { name = "after_delete_trigger";
    setup = [
      "CREATE TABLE t (id INTEGER)";
      "CREATE TABLE del_log (old_id INTEGER)";
      "INSERT INTO t VALUES (10)";
      "INSERT INTO t VALUES (20)";
      "CREATE TRIGGER t_ad AFTER DELETE ON t BEGIN INSERT INTO del_log VALUES (OLD.id); END";
      "DELETE FROM t WHERE id = 10";
    ];
    query = "SELECT old_id FROM del_log";
    unordered = false };

  { name = "after_update_trigger";
    setup = [
      "CREATE TABLE t (id INTEGER, val TEXT)";
      "CREATE TABLE changes (t_id INTEGER, old_val TEXT, new_val TEXT)";
      "INSERT INTO t VALUES (1, 'original')";
      "CREATE TRIGGER t_au AFTER UPDATE ON t BEGIN INSERT INTO changes VALUES (OLD.id, OLD.val, NEW.val); END";
      "UPDATE t SET val = 'updated' WHERE id = 1";
    ];
    query = "SELECT t_id, old_val, new_val FROM changes";
    unordered = false };

  { name = "trigger_when_clause";
    setup = [
      "CREATE TABLE t (id INTEGER, score INTEGER)";
      "CREATE TABLE high_scores (t_id INTEGER)";
      "CREATE TRIGGER t_when AFTER INSERT ON t WHEN NEW.score > 100 BEGIN INSERT INTO high_scores VALUES (NEW.id); END";
      "INSERT INTO t VALUES (1, 50)";
      "INSERT INTO t VALUES (2, 150)";
    ];
    query = "SELECT t_id FROM high_scores";
    unordered = false };

  { name = "drop_trigger";
    setup = [
      "CREATE TABLE t (id INTEGER)";
      "CREATE TABLE audit (id INTEGER)";
      "CREATE TRIGGER t_ai AFTER INSERT ON t BEGIN INSERT INTO audit VALUES (NEW.id); END";
      "INSERT INTO t VALUES (1)";
      "DROP TRIGGER t_ai";
      "INSERT INTO t VALUES (2)";
    ];
    query = "SELECT id FROM audit ORDER BY id";
    unordered = false };
]

(* ── Phase 23: FK referential actions (CASCADE, SET NULL) ──────── *)

let phase23_cascade_cases = [
  { name = "cascade_delete_basic";
    setup = [
      "PRAGMA foreign_keys = ON";
      "CREATE TABLE par (id INTEGER PRIMARY KEY)";
      "CREATE TABLE chi (id INTEGER, pid INTEGER REFERENCES par(id) ON DELETE CASCADE)";
      "INSERT INTO par VALUES (1)";
      "INSERT INTO par VALUES (2)";
      "INSERT INTO chi VALUES (10, 1)";
      "INSERT INTO chi VALUES (11, 1)";
      "INSERT INTO chi VALUES (12, 2)";
      "DELETE FROM par WHERE id = 1";
    ];
    query = "SELECT COUNT(*) FROM chi";
    unordered = false };

  { name = "cascade_delete_all";
    setup = [
      "PRAGMA foreign_keys = ON";
      "CREATE TABLE par2 (id INTEGER PRIMARY KEY)";
      "CREATE TABLE chi2 (id INTEGER, pid INTEGER REFERENCES par2(id) ON DELETE CASCADE)";
      "INSERT INTO par2 VALUES (1)";
      "INSERT INTO par2 VALUES (2)";
      "INSERT INTO chi2 VALUES (10, 1)";
      "INSERT INTO chi2 VALUES (20, 2)";
      "DELETE FROM par2";
    ];
    query = "SELECT COUNT(*) FROM chi2";
    unordered = false };

  { name = "cascade_update";
    setup = [
      "PRAGMA foreign_keys = ON";
      "CREATE TABLE par3 (id INTEGER PRIMARY KEY)";
      "CREATE TABLE chi3 (id INTEGER, pid INTEGER REFERENCES par3(id) ON UPDATE CASCADE)";
      "INSERT INTO par3 VALUES (1)";
      "INSERT INTO chi3 VALUES (10, 1)";
      "INSERT INTO chi3 VALUES (11, 1)";
      "UPDATE par3 SET id = 99 WHERE id = 1";
    ];
    query = "SELECT pid FROM chi3 ORDER BY id";
    unordered = false };

  { name = "set_null_on_delete";
    setup = [
      "PRAGMA foreign_keys = ON";
      "CREATE TABLE par4 (id INTEGER PRIMARY KEY)";
      "CREATE TABLE chi4 (id INTEGER, pid INTEGER REFERENCES par4(id) ON DELETE SET NULL)";
      "INSERT INTO par4 VALUES (1)";
      "INSERT INTO chi4 VALUES (10, 1)";
      "DELETE FROM par4 WHERE id = 1";
    ];
    query = "SELECT pid IS NULL FROM chi4";
    unordered = false };

  { name = "cascade_table_level_fk";
    setup = [
      "PRAGMA foreign_keys = ON";
      "CREATE TABLE par5 (id INTEGER PRIMARY KEY)";
      "CREATE TABLE chi5 (id INTEGER, pid INTEGER, FOREIGN KEY (pid) REFERENCES par5(id) ON DELETE CASCADE)";
      "INSERT INTO par5 VALUES (1)";
      "INSERT INTO chi5 VALUES (10, 1)";
      "INSERT INTO chi5 VALUES (11, 1)";
      "DELETE FROM par5";
    ];
    query = "SELECT COUNT(*) FROM chi5";
    unordered = false };
]

let phase25_generated_cases = [
  { name = "generated_stored_basic";
    setup = [
      {|CREATE TABLE t (
        id INTEGER, name TEXT,
        upper_name TEXT GENERATED ALWAYS AS (upper(name)) STORED
      )|};
      "INSERT INTO t (id, name) VALUES (1, 'hello')";
    ];
    query = "SELECT upper_name FROM t WHERE id = 1";
    unordered = false };

  { name = "generated_arithmetic";
    setup = [
      {|CREATE TABLE t (a INTEGER, b INTEGER,
        c INTEGER GENERATED ALWAYS AS (a + b) STORED)|};
      "INSERT INTO t (a, b) VALUES (3, 7)";
    ];
    query = "SELECT c FROM t";
    unordered = false };

  { name = "generated_update_recomputes";
    setup = [
      {|CREATE TABLE t (x INTEGER,
        y INTEGER GENERATED ALWAYS AS (x * 2) STORED)|};
      "INSERT INTO t (x) VALUES (5)";
      "UPDATE t SET x = 10";
    ];
    query = "SELECT y FROM t";
    unordered = false };
]

let phase24_limit_cases = [
  { name = "delete_order_limit";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
      "INSERT INTO t VALUES (1,'a'),(2,'b'),(3,'c')";
      "DELETE FROM t ORDER BY id ASC LIMIT 1";
    ];
    query = "SELECT id FROM t ORDER BY id";
    unordered = false };

  { name = "update_order_limit";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
      "INSERT INTO t VALUES (1,'x'),(2,'y'),(3,'z')";
      "UPDATE t SET v='w' ORDER BY id DESC LIMIT 1";
    ];
    query = "SELECT id, v FROM t ORDER BY id";
    unordered = false };
]

(* ── Phase 26: PRAGMA cases ────────────────────────────────────── *)

let phase26_pragma_cases = [
  (* NOTE: pragma_foreign_keys is intentionally excluded from SQLite comparison:
     SQLite returns 0 by default (FK enforcement off), while sqlocaml always
     returns 1 (FK enforcement always on). This difference is intentional. *)

  { name = "pragma_journal_mode";
    setup = [];
    query = "PRAGMA journal_mode";
    unordered = false };

  { name = "pragma_user_version_default";
    setup = [];
    query = "PRAGMA user_version";
    unordered = false };

  { name = "pragma_user_version_set_get";
    setup = [ "PRAGMA user_version = 7" ];
    query = "PRAGMA user_version";
    unordered = false };

  { name = "pragma_fk_list_single";
    setup = [
      "CREATE TABLE p (id INTEGER PRIMARY KEY)";
      "CREATE TABLE c (pid INTEGER REFERENCES p(id))";
    ];
    query = "PRAGMA foreign_key_list(c)";
    unordered = false };

  { name = "pragma_fk_list_empty";
    setup = [
      "CREATE TABLE t (id INTEGER PRIMARY KEY, name TEXT)";
    ];
    query = "PRAGMA foreign_key_list(t)";
    unordered = false };
]

let phase27_cases = [
  { name = "group_concat_basic";
    setup = [ "CREATE TABLE t (v TEXT)";
              "INSERT INTO t VALUES ('a')";
              "INSERT INTO t VALUES ('b')";
              "INSERT INTO t VALUES ('c')" ];
    query = "SELECT GROUP_CONCAT(v, ',') FROM t";
    unordered = false };

  { name = "group_concat_sep";
    setup = [ "CREATE TABLE t (v TEXT)";
              "INSERT INTO t VALUES ('x')";
              "INSERT INTO t VALUES ('y')" ];
    query = "SELECT GROUP_CONCAT(v, '|') FROM t";
    unordered = false };

  { name = "group_concat_null_skip";
    setup = [ "CREATE TABLE t (v TEXT)";
              "INSERT INTO t VALUES ('hi')";
              "INSERT INTO t VALUES (NULL)";
              "INSERT INTO t VALUES ('bye')" ];
    query = "SELECT GROUP_CONCAT(v, ',') FROM t";
    unordered = false };

  { name = "group_concat_empty";
    setup = [ "CREATE TABLE t (v TEXT)" ];
    query = "SELECT GROUP_CONCAT(v) FROM t";
    unordered = false };

  { name = "group_concat_group_by";
    setup = [ "CREATE TABLE t (k TEXT, v TEXT)";
              "INSERT INTO t VALUES ('a', '1')";
              "INSERT INTO t VALUES ('a', '2')";
              "INSERT INTO t VALUES ('b', '3')" ];
    query = "SELECT k, GROUP_CONCAT(v, ',') FROM t GROUP BY k ORDER BY k";
    unordered = false };

  { name = "pk_col_unique_violation";
    setup = [ "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)";
              "INSERT INTO t VALUES (1, 'a')" ];
    query = "SELECT id FROM t";
    unordered = false };
]

let phase30_cascade_cases = [
  { name = "cascade_delete_three_levels";
    setup = [
      "PRAGMA foreign_keys = 1";
      "CREATE TABLE a (id INTEGER PRIMARY KEY)";
      "CREATE TABLE b (id INTEGER PRIMARY KEY, aid INTEGER REFERENCES a(id) ON DELETE CASCADE)";
      "CREATE TABLE c (id INTEGER, bid INTEGER REFERENCES b(id) ON DELETE CASCADE)";
      "INSERT INTO a VALUES (1)";
      "INSERT INTO b VALUES (10, 1)";
      "INSERT INTO c VALUES (100, 10)";
      "DELETE FROM a WHERE id = 1";
    ];
    query = "SELECT COUNT(*) FROM b UNION ALL SELECT COUNT(*) FROM c";
    unordered = false };

  { name = "cascade_update_three_levels";
    setup = [
      "PRAGMA foreign_keys = 1";
      "CREATE TABLE a (id INTEGER PRIMARY KEY)";
      "CREATE TABLE b (bid INTEGER PRIMARY KEY, aid INTEGER, UNIQUE (aid), FOREIGN KEY (aid) REFERENCES a(id) ON UPDATE CASCADE)";
      "CREATE TABLE c (cid INTEGER, c_ref INTEGER REFERENCES b(aid) ON UPDATE CASCADE)";
      "INSERT INTO a VALUES (1)";
      "INSERT INTO b VALUES (10, 1)";
      "INSERT INTO c VALUES (100, 1)";
      "UPDATE a SET id = 99 WHERE id = 1";
    ];
    query = "SELECT c_ref FROM c";
    unordered = false };

  { name = "cascade_delete_set_null_grandchild";
    setup = [
      "PRAGMA foreign_keys = 1";
      "CREATE TABLE a (id INTEGER PRIMARY KEY)";
      "CREATE TABLE b (id INTEGER PRIMARY KEY, aid INTEGER REFERENCES a(id) ON DELETE CASCADE)";
      "CREATE TABLE c (id INTEGER, bid INTEGER REFERENCES b(id) ON DELETE SET NULL)";
      "INSERT INTO a VALUES (1)";
      "INSERT INTO b VALUES (10, 1)";
      "INSERT INTO c VALUES (100, 10)";
      "DELETE FROM a WHERE id = 1";
    ];
    query = "SELECT id, bid FROM c";
    unordered = false };
]

let phase30_snippet_cases = [
  { name = "snippet_fts_basic_match";
    setup = [
      "CREATE VIRTUAL TABLE docs USING fts5(body)";
      "INSERT INTO docs VALUES ('hello world')";
      "INSERT INTO docs VALUES ('goodbye world')";
    ];
    query = "SELECT body FROM docs WHERE docs MATCH 'hello'";
    unordered = false };

  { name = "snippet_col_name";
    setup = [
      "CREATE VIRTUAL TABLE docs USING fts5(title, body)";
      "INSERT INTO docs VALUES ('guide', 'learn programming')";
    ];
    query = "SELECT title FROM docs WHERE docs MATCH 'guide'";
    unordered = false };
]

let phase30_sqlite_master_cases = [
  { name = "sqlite_master_table_type";
    setup = [ "CREATE TABLE users (id INTEGER, name TEXT)" ];
    query = "SELECT type, name FROM sqlite_master \
             WHERE type='table' AND name='users'";
    unordered = false };

  { name = "sqlite_master_index_type";
    setup = [
      "CREATE TABLE t (id INTEGER, v TEXT)";
      "CREATE INDEX idx_v ON t (v)";
    ];
    query = "SELECT type, name, tbl_name FROM sqlite_master WHERE type='index'";
    unordered = false };

  { name = "sqlite_master_count_tables";
    setup = [
      "CREATE TABLE a (id INTEGER)";
      "CREATE TABLE b (id INTEGER)";
    ];
    query = "SELECT COUNT(*) FROM sqlite_master WHERE type='table'";
    unordered = false };

  { name = "sqlite_schema_alias";
    setup = [ "CREATE TABLE t (id INTEGER)" ];
    query = "SELECT COUNT(*) FROM sqlite_schema WHERE type='table'";
    unordered = false };

  { name = "sqlite_master_view_type";
    setup = [
      "CREATE TABLE t (id INTEGER)";
      "CREATE VIEW v AS SELECT id FROM t";
    ];
    query = "SELECT type, name FROM sqlite_master WHERE type='view'";
    unordered = false };
]

let phase31_alter_fk_cases = [
  { name = "alter_add_fk_valid_insert";
    setup = [
      "CREATE TABLE parent (id INTEGER PRIMARY KEY)";
      "INSERT INTO parent VALUES (1)";
      "INSERT INTO parent VALUES (2)";
      "CREATE TABLE child (id INTEGER)";
      "ALTER TABLE child ADD COLUMN parent_id INTEGER REFERENCES parent(id)";
      "INSERT INTO child VALUES (10, 1)";
      "INSERT INTO child VALUES (20, 2)";
    ];
    query = "SELECT COUNT(*) FROM child";
    unordered = false };

  { name = "alter_add_fk_null_allowed";
    setup = [
      "PRAGMA foreign_keys = 1";
      "CREATE TABLE parent (id INTEGER PRIMARY KEY)";
      "CREATE TABLE child (id INTEGER)";
      "ALTER TABLE child ADD COLUMN parent_id INTEGER REFERENCES parent(id)";
      "INSERT INTO child VALUES (1, NULL)";
    ];
    query = "SELECT id, parent_id FROM child";
    unordered = false };

  { name = "alter_add_fk_pragma_list";
    setup = [
      "CREATE TABLE parent (id INTEGER PRIMARY KEY)";
      "CREATE TABLE child (id INTEGER)";
      "ALTER TABLE child ADD COLUMN parent_id INTEGER REFERENCES parent(id)";
    ];
    query = "PRAGMA foreign_key_list(child)";
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
    "phase12_cte",        List.map make_test phase12_cte_cases;
    "phase12_multirow",   List.map make_test phase12_multirow_cases;
    "phase12_correlated", List.map make_test phase12_correlated_cases;
    "phase13_upsert",     List.map make_test phase13_upsert_cases;
    "phase13_view",       List.map make_test phase13_view_cases;
    "phase14_window",          List.map make_test phase14_window_cases;
    "phase14_recursive_cte",   List.map make_test phase14_recursive_cte_cases;
    "phase16_window_alias",    List.map make_test phase16_window_alias_cases;
    "phase16_collate",         List.map make_test phase16_collate_cases;
    "phase16_drop_column",     List.map make_test phase16_drop_column_cases;
    "phase17_frame",           List.map make_test phase17_frame_cases;
    "phase17_pctrank",         List.map make_test phase17_pctrank_cases;
    "phase17_ine",             List.map make_test phase17_ine_cases;
    "phase17_cast_real",       List.map make_test phase17_cast_real_cases;
    "phase18_multigroup",      List.map make_test phase18_multigroup_cases;
    "phase18_fk",              List.map make_test phase18_fk_cases;
    "phase19_math",            List.map make_test phase19_math_cases;
    "phase19_nulls",           List.map make_test phase19_nulls_cases;
    "phase19_window_agg",      List.map make_test phase19_window_agg_cases;
    "phase20_json",            List.map make_test phase20_json_cases;
    "phase20_json_mut",        List.map make_test phase20_json_mutation_cases;
    "phase21_savepoint",       List.map make_test phase21_savepoint_cases;
    "phase21_fk",              List.map make_test phase21_fk_cases;
    "phase22_trigger",         List.map make_test phase22_trigger_cases;
    "phase23_cascade",         List.map make_test phase23_cascade_cases;
    "phase24_limit",           List.map make_test phase24_limit_cases;
    "phase25_generated",       List.map make_test phase25_generated_cases;
    "phase26_pragma",          List.map make_test phase26_pragma_cases;
    "phase27",                 List.map make_test phase27_cases;
    "phase30_cascade",         List.map make_test phase30_cascade_cases;
    "phase30_snippet",         List.map make_test phase30_snippet_cases;
    "phase30_sqlite_master",   List.map make_test phase30_sqlite_master_cases;
    "phase31_alter_fk",        List.map make_test phase31_alter_fk_cases;
  ]
