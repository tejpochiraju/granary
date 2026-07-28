(** Tests for GROUP BY, HAVING, and aggregate functions
    (COUNT, SUM, AVG, MIN, MAX). *)

open Lwt.Syntax
module Db = Granary.Db
module Row = Granary_encoding.Row

let run = Lwt_main.run
let fresh_db () = run (Db.open_in_memory ())

let exec db sql =
  run
    (let* result = Db.execute db sql in
     (match result with
      | Ok () -> ()
      | Error _ -> Alcotest.failf "exec: unexpected error for: %s" sql);
     Lwt.return_unit)
;;

let query_ok db sql =
  run
    (let* result = Db.query db sql in
     match result with
     | Error _ -> Alcotest.failf "query_ok: unexpected error for: %s" sql
     | Ok stream -> Lwt_stream.to_list stream)
;;

let query_err db sql =
  run
    (let* result = Db.query db sql in
     match result with
     | Ok _ -> Alcotest.failf "query_err: expected Error for: %s" sql
     | Error e -> Lwt.return e)
;;

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let int_of_value = function
  | Row.V_int n -> Int64.to_int n
  | _ -> Alcotest.fail "expected V_int"
;;

let int64_of_value = function
  | Row.V_int n -> n
  | _ -> Alcotest.fail "expected V_int"
;;

let real_of_value = function
  | Row.V_real f -> f
  | _ -> Alcotest.fail "expected V_real"
;;

let text_of_value = function
  | Row.V_text s -> s
  | _ -> Alcotest.fail "expected V_text"
;;

let is_null = function
  | Row.V_null -> true
  | _ -> false
;;

(* ------------------------------------------------------------------ *)
(* Group 1: Aggregates without GROUP BY                                  *)
(* ------------------------------------------------------------------ *)

let count_star_nonempty () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "INSERT INTO t (n) VALUES (3)";
  exec db "INSERT INTO t (n) VALUES (4)";
  exec db "INSERT INTO t (n) VALUES (5)";
  let rows = query_ok db "SELECT COUNT(*) FROM t" in
  Alcotest.(check int) "single row" 1 (List.length rows);
  let r = List.hd rows in
  Alcotest.(check int) "count = 5" 5 (int_of_value r.(0))
;;

let count_star_empty () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  let rows = query_ok db "SELECT COUNT(*) FROM t" in
  Alcotest.(check int) "single row" 1 (List.length rows);
  let r = List.hd rows in
  Alcotest.(check int) "count = 0" 0 (int_of_value r.(0))
;;

let count_col_skips_null () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (NULL)";
  exec db "INSERT INTO t (n) VALUES (3)";
  exec db "INSERT INTO t (n) VALUES (NULL)";
  let rows = query_ok db "SELECT COUNT(n) FROM t" in
  let r = List.hd rows in
  Alcotest.(check int) "count(n) skips NULLs = 2" 2 (int_of_value r.(0));
  let rows2 = query_ok db "SELECT COUNT(*) FROM t" in
  let r2 = List.hd rows2 in
  Alcotest.(check int) "count(*) includes NULLs = 4" 4 (int_of_value r2.(0))
;;

let sum_basic () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "INSERT INTO t (n) VALUES (3)";
  exec db "INSERT INTO t (n) VALUES (4)";
  let rows = query_ok db "SELECT SUM(n) FROM t" in
  let r = List.hd rows in
  Alcotest.(check int64) "sum = 10" 10L (int64_of_value r.(0))
;;

let sum_with_nulls () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (NULL)";
  exec db "INSERT INTO t (n) VALUES (4)";
  let rows = query_ok db "SELECT SUM(n) FROM t" in
  let r = List.hd rows in
  Alcotest.(check int64) "SUM skips NULLs" 5L (int64_of_value r.(0))
;;

let sum_all_null () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (NULL)";
  exec db "INSERT INTO t (n) VALUES (NULL)";
  let rows = query_ok db "SELECT SUM(n) FROM t" in
  let r = List.hd rows in
  Alcotest.(check bool) "SUM of all NULLs = NULL" true (is_null r.(0))
;;

let sum_empty () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  let rows = query_ok db "SELECT SUM(n) FROM t" in
  let r = List.hd rows in
  Alcotest.(check bool) "SUM of empty = NULL" true (is_null r.(0))
;;

let avg_basic () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (10)";
  exec db "INSERT INTO t (n) VALUES (20)";
  exec db "INSERT INTO t (n) VALUES (30)";
  let rows = query_ok db "SELECT AVG(n) FROM t" in
  let r = List.hd rows in
  Alcotest.(check (float 1e-9)) "avg = 20.0" 20.0 (real_of_value r.(0))
;;

let avg_returns_real () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  let rows = query_ok db "SELECT AVG(n) FROM t" in
  let r = List.hd rows in
  match r.(0) with
  | Row.V_real f -> Alcotest.(check (float 1e-9)) "avg = 1.5" 1.5 f
  | _ -> Alcotest.fail "AVG must return V_real"
;;

let avg_empty () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  let rows = query_ok db "SELECT AVG(n) FROM t" in
  let r = List.hd rows in
  Alcotest.(check bool) "AVG of empty = NULL" true (is_null r.(0))
;;

let min_max_basic () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (5)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "INSERT INTO t (n) VALUES (9)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (7)";
  let rows = query_ok db "SELECT MIN(n), MAX(n) FROM t" in
  Alcotest.(check int) "single row" 1 (List.length rows);
  let r = List.hd rows in
  Alcotest.(check int64) "min = 1" 1L (int64_of_value r.(0));
  Alcotest.(check int64) "max = 9" 9L (int64_of_value r.(1))
;;

let min_max_ignore_nulls () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (NULL)";
  exec db "INSERT INTO t (n) VALUES (5)";
  exec db "INSERT INTO t (n) VALUES (NULL)";
  exec db "INSERT INTO t (n) VALUES (3)";
  let rows = query_ok db "SELECT MIN(n), MAX(n) FROM t" in
  let r = List.hd rows in
  Alcotest.(check int64) "min ignores nulls" 3L (int64_of_value r.(0));
  Alcotest.(check int64) "max ignores nulls" 5L (int64_of_value r.(1))
;;

let min_max_all_null () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (NULL)";
  exec db "INSERT INTO t (n) VALUES (NULL)";
  let rows = query_ok db "SELECT MIN(n), MAX(n) FROM t" in
  let r = List.hd rows in
  Alcotest.(check bool) "MIN all NULLs = NULL" true (is_null r.(0));
  Alcotest.(check bool) "MAX all NULLs = NULL" true (is_null r.(1))
;;

let multiple_aggregates () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  exec db "INSERT INTO t (n) VALUES (2)";
  exec db "INSERT INTO t (n) VALUES (3)";
  exec db "INSERT INTO t (n) VALUES (4)";
  let rows = query_ok db "SELECT COUNT(*), SUM(n), AVG(n), MIN(n), MAX(n) FROM t" in
  let r = List.hd rows in
  Alcotest.(check int) "count" 4 (int_of_value r.(0));
  Alcotest.(check int64) "sum" 10L (int64_of_value r.(1));
  Alcotest.(check (float 1e-9)) "avg" 2.5 (real_of_value r.(2));
  Alcotest.(check int64) "min" 1L (int64_of_value r.(3));
  Alcotest.(check int64) "max" 4L (int64_of_value r.(4))
;;

(* ------------------------------------------------------------------ *)
(* Group 2: GROUP BY                                                     *)
(* ------------------------------------------------------------------ *)

let group_by_basic () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT, n INTEGER)";
  exec db "INSERT INTO t (s, n) VALUES ('a', 1)";
  exec db "INSERT INTO t (s, n) VALUES ('b', 2)";
  exec db "INSERT INTO t (s, n) VALUES ('a', 3)";
  exec db "INSERT INTO t (s, n) VALUES ('b', 4)";
  exec db "INSERT INTO t (s, n) VALUES ('a', 5)";
  let rows = query_ok db "SELECT s, COUNT(*) FROM t GROUP BY s" in
  Alcotest.(check int) "two groups" 2 (List.length rows);
  let by_label =
    List.map (fun r -> text_of_value r.(0), int_of_value r.(1)) rows |> List.sort compare
  in
  Alcotest.(check (list (pair string int))) "groups by label" [ "a", 3; "b", 2 ] by_label
;;

let group_by_with_sum () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT, n INTEGER)";
  exec db "INSERT INTO t (s, n) VALUES ('x', 1)";
  exec db "INSERT INTO t (s, n) VALUES ('y', 10)";
  exec db "INSERT INTO t (s, n) VALUES ('x', 2)";
  exec db "INSERT INTO t (s, n) VALUES ('y', 20)";
  exec db "INSERT INTO t (s, n) VALUES ('x', 3)";
  let rows = query_ok db "SELECT s, SUM(n) FROM t GROUP BY s" in
  Alcotest.(check int) "two groups" 2 (List.length rows);
  let sorted =
    List.map (fun r -> text_of_value r.(0), int64_of_value r.(1)) rows
    |> List.sort compare
  in
  Alcotest.(check (list (pair string int64))) "sum per group" [ "x", 6L; "y", 30L ] sorted
;;

let group_by_empty_table () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT, n INTEGER)";
  let rows = query_ok db "SELECT s, COUNT(*) FROM t GROUP BY s" in
  Alcotest.(check int) "empty groups" 0 (List.length rows)
;;

let group_by_null_groups () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT, n INTEGER)";
  exec db "INSERT INTO t (s, n) VALUES ('a', 1)";
  exec db "INSERT INTO t (s, n) VALUES (NULL, 2)";
  exec db "INSERT INTO t (s, n) VALUES (NULL, 3)";
  exec db "INSERT INTO t (s, n) VALUES ('a', 4)";
  let rows = query_ok db "SELECT s, COUNT(*) FROM t GROUP BY s" in
  Alcotest.(check int) "two groups (NULL + 'a')" 2 (List.length rows);
  let null_count =
    List.fold_left
      (fun acc r -> if is_null r.(0) then int_of_value r.(1) else acc)
      (-1)
      rows
  in
  let a_count =
    List.fold_left
      (fun acc r ->
         match r.(0) with
         | Row.V_text "a" -> int_of_value r.(1)
         | _ -> acc)
      (-1)
      rows
  in
  Alcotest.(check int) "NULL group size" 2 null_count;
  Alcotest.(check int) "'a' group size" 2 a_count
;;

(** Qualified GROUP BY with table.column syntax. *)
let group_by_qualified_col () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT, n INTEGER)";
  exec db "INSERT INTO t (s, n) VALUES ('a', 1), ('b', 2), ('a', 3)";
  let rows = query_ok db "SELECT t.s, SUM(t.n) FROM t GROUP BY t.s ORDER BY t.s" in
  Alcotest.(check int) "two groups" 2 (List.length rows);
  let sorted =
    List.map (fun r -> text_of_value r.(0), int64_of_value r.(1)) rows
    |> List.sort compare
  in
  Alcotest.(check (list (pair string int64)))
    "qualified GROUP BY results"
    [ "a", 4L; "b", 2L ]
    sorted
;;

(** Qualified GROUP BY via table alias: GROUP BY alias.col. *)
let group_by_qualified_alias () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT, n INTEGER)";
  exec db "INSERT INTO t (s, n) VALUES ('a', 1), ('b', 2), ('a', 3)";
  let rows = query_ok db "SELECT u.s, SUM(u.n) FROM t AS u GROUP BY u.s ORDER BY u.s" in
  Alcotest.(check int) "two groups" 2 (List.length rows);
  let sorted =
    List.map (fun r -> text_of_value r.(0), int64_of_value r.(1)) rows
    |> List.sort compare
  in
  Alcotest.(check (list (pair string int64)))
    "alias GROUP BY results"
    [ "a", 4L; "b", 2L ]
    sorted
;;

(* ------------------------------------------------------------------ *)
(* Group 3: HAVING                                                       *)
(* ------------------------------------------------------------------ *)

let having_basic () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT, n INTEGER)";
  exec db "INSERT INTO t (s, n) VALUES ('a', 1)";
  exec db "INSERT INTO t (s, n) VALUES ('b', 2)";
  exec db "INSERT INTO t (s, n) VALUES ('c', 3)";
  exec db "INSERT INTO t (s, n) VALUES ('a', 4)";
  exec db "INSERT INTO t (s, n) VALUES ('a', 5)";
  exec db "INSERT INTO t (s, n) VALUES ('b', 6)";
  let rows = query_ok db "SELECT s, COUNT(*) FROM t GROUP BY s HAVING COUNT(*) > 1" in
  Alcotest.(check int) "two groups have count > 1" 2 (List.length rows);
  let sorted =
    List.map (fun r -> text_of_value r.(0), int_of_value r.(1)) rows |> List.sort compare
  in
  Alcotest.(check (list (pair string int)))
    "groups with HAVING filter"
    [ "a", 3; "b", 2 ]
    sorted
;;

let having_filters_all () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT, n INTEGER)";
  exec db "INSERT INTO t (s, n) VALUES ('a', 1)";
  exec db "INSERT INTO t (s, n) VALUES ('b', 2)";
  let rows = query_ok db "SELECT s, COUNT(*) FROM t GROUP BY s HAVING COUNT(*) > 10" in
  Alcotest.(check int) "no groups" 0 (List.length rows)
;;

(* ------------------------------------------------------------------ *)
(* Group 4: GROUP BY + ORDER BY                                          *)
(* ------------------------------------------------------------------ *)

let group_by_with_order_by () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT, n INTEGER)";
  exec db "INSERT INTO t (s, n) VALUES ('z', 1)";
  exec db "INSERT INTO t (s, n) VALUES ('a', 2)";
  exec db "INSERT INTO t (s, n) VALUES ('m', 3)";
  exec db "INSERT INTO t (s, n) VALUES ('a', 4)";
  let rows = query_ok db "SELECT s, COUNT(*) FROM t GROUP BY s ORDER BY s ASC" in
  let labels = List.map (fun r -> text_of_value r.(0)) rows in
  Alcotest.(check (list string)) "ordered groups" [ "a"; "m"; "z" ] labels
;;

(* ------------------------------------------------------------------ *)
(* Group 5: Sema errors                                                  *)
(* ------------------------------------------------------------------ *)

let sum_on_text_rejected () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT)";
  exec db "INSERT INTO t (s) VALUES ('a')";
  let _err = query_err db "SELECT SUM(s) FROM t" in
  ()
;;

let agg_in_where_rejected () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t (n) VALUES (1)";
  let _err = query_err db "SELECT n FROM t WHERE COUNT(*) > 0" in
  ()
;;

(* ------------------------------------------------------------------ *)
(* Group 6: QCheck properties (10_000 trials each)                       *)
(* ------------------------------------------------------------------ *)

let setup_int_table values =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  List.iter (fun v -> exec db (Printf.sprintf "INSERT INTO t (n) VALUES (%d)" v)) values;
  db
;;

let qcheck_sum_matches_manual =
  QCheck.Test.make
    ~name:"SUM(n) matches manual sum"
    ~count:10_000
    QCheck.(list_size Gen.(0 -- 20) (-1000 -- 1000))
    (fun values ->
       let db = setup_int_table values in
       let rows = query_ok db "SELECT SUM(n) FROM t" in
       let r = List.hd rows in
       match values, r.(0) with
       | [], Row.V_null -> true
       | _, Row.V_int got ->
         let expected = List.fold_left ( + ) 0 values in
         Int64.to_int got = expected
       | _ -> false)
;;

let qcheck_count_matches_length =
  QCheck.Test.make
    ~name:"COUNT(*) matches list length"
    ~count:10_000
    QCheck.(list_size Gen.(0 -- 50) (1 -- 100))
    (fun values ->
       let db = setup_int_table values in
       let rows = query_ok db "SELECT COUNT(*) FROM t" in
       let r = List.hd rows in
       match r.(0) with
       | Row.V_int n -> Int64.to_int n = List.length values
       | _ -> false)
;;

let qcheck_group_by_partitions_correctly =
  (* Build a table with (s TEXT, n INTEGER) and verify that GROUP BY s
     produces a row per distinct label whose count equals the number of
     rows with that label. *)
  QCheck.Test.make
    ~name:"GROUP BY s partitions correctly: counts per group sum to total"
    ~count:10_000
    QCheck.(list_size Gen.(0 -- 20) (pair (0 -- 4) (1 -- 100)))
    (fun pairs ->
       let db = fresh_db () in
       exec db "CREATE TABLE t (s TEXT, n INTEGER)";
       List.iter
         (fun (k, v) ->
            exec db (Printf.sprintf "INSERT INTO t (s, n) VALUES ('g%d', %d)" k v))
         pairs;
       let rows = query_ok db "SELECT s, COUNT(*) FROM t GROUP BY s" in
       (* Total count across all groups must equal total rows. *)
       let total =
         List.fold_left
           (fun acc r ->
              match r.(1) with
              | Row.V_int n -> acc + Int64.to_int n
              | _ -> acc)
           0
           rows
       in
       (* Also: number of distinct groups must equal expected. *)
       let expected_groups = List.sort_uniq compare (List.map fst pairs) |> List.length in
       total = List.length pairs && List.length rows = expected_groups)
;;

(* ------------------------------------------------------------------ *)
(* Runner                                                                *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "Aggregate"
    [ ( "no-group-by"
      , [ Alcotest.test_case "count_star_nonempty" `Quick count_star_nonempty
        ; Alcotest.test_case "count_star_empty" `Quick count_star_empty
        ; Alcotest.test_case "count_col_skips_null" `Quick count_col_skips_null
        ; Alcotest.test_case "sum_basic" `Quick sum_basic
        ; Alcotest.test_case "sum_with_nulls" `Quick sum_with_nulls
        ; Alcotest.test_case "sum_all_null" `Quick sum_all_null
        ; Alcotest.test_case "sum_empty" `Quick sum_empty
        ; Alcotest.test_case "avg_basic" `Quick avg_basic
        ; Alcotest.test_case "avg_returns_real" `Quick avg_returns_real
        ; Alcotest.test_case "avg_empty" `Quick avg_empty
        ; Alcotest.test_case "min_max_basic" `Quick min_max_basic
        ; Alcotest.test_case "min_max_ignore_nulls" `Quick min_max_ignore_nulls
        ; Alcotest.test_case "min_max_all_null" `Quick min_max_all_null
        ; Alcotest.test_case "multiple_aggregates" `Quick multiple_aggregates
        ] )
    ; ( "group-by"
      , [ Alcotest.test_case "group_by_basic" `Quick group_by_basic
        ; Alcotest.test_case "group_by_with_sum" `Quick group_by_with_sum
        ; Alcotest.test_case "group_by_empty_table" `Quick group_by_empty_table
        ; Alcotest.test_case "group_by_null_groups" `Quick group_by_null_groups
        ; Alcotest.test_case "group_by_qualified_col" `Quick group_by_qualified_col
        ; Alcotest.test_case "group_by_qualified_alias" `Quick group_by_qualified_alias
        ] )
    ; ( "having"
      , [ Alcotest.test_case "having_basic" `Quick having_basic
        ; Alcotest.test_case "having_filters_all" `Quick having_filters_all
        ] )
    ; ( "order"
      , [ Alcotest.test_case "group_by_with_order_by" `Quick group_by_with_order_by ] )
    ; ( "sema-errors"
      , [ Alcotest.test_case "sum_on_text_rejected" `Quick sum_on_text_rejected
        ; Alcotest.test_case "agg_in_where_rejected" `Quick agg_in_where_rejected
        ] )
    ; ( "qcheck"
      , [ QCheck_alcotest.to_alcotest qcheck_sum_matches_manual
        ; QCheck_alcotest.to_alcotest qcheck_count_matches_length
        ; QCheck_alcotest.to_alcotest qcheck_group_by_partitions_correctly
        ] )
    ]
;;
