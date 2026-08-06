(** #491 part 1: [DISTINCT] inside an aggregate's argument list.

    Before this, [DISTINCT] appeared in the grammar only as a SELECT-list
    modifier, so [COUNT(DISTINCT x)] was a bare "parse error: syntax error".
    That blocked TPC-H Q16 and forced TPC-C's StockLevel to be documented as
    [Rewritten]: the spec's [SELECT COUNT(DISTINCT s_i_id) ...] was shipped as
    [SELECT DISTINCT s_i_id ...] with the count taken client-side.

    Two things are pinned here beyond "it parses":

    - The dedup key is [Exec.row_key]'s rendering of the single argument value
      — the same key [SELECT DISTINCT] and the hash joins already use. #536
      records what that key decides (all NaNs collapse to one, none collapses
      into NULL); reusing it is what keeps this from becoming a fifth value
      comparator in a tree that already disagrees across four.
    - NULL is dropped, exactly as [COUNT(x)] drops it. The dedup keeps one
      NULL and the aggregate's own NULL handling then skips it, so
      [COUNT(DISTINCT x)] never counts a NULL and [SUM(DISTINCT x)] never adds
      one.

    The modifier is general — [SUM], [AVG], [MIN], [MAX] and [GROUP_CONCAT]
    take it too, even though [MIN]/[MAX] cannot be changed by it — rather than
    special-cased to [COUNT]. Two spellings are refused with a real message
    instead of a syntax error: more than one argument, and a window function. *)

open Lwt.Syntax
module Db = Granary.Db
module Row = Granary_encoding.Row

let run = Lwt_main.run

let with_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let query db sql =
  run
    (let* r = Db.query db sql in
     match r with
     | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
     | Ok stream -> Lwt_stream.to_list stream)
;;

let query_err db sql =
  match run (Db.query db sql) with
  | Ok _ -> Alcotest.failf "expected an error for %S" sql
  | Error e -> Format.asprintf "%a" Db.pp_error e
;;

let contains_sub ~needle s =
  let n = String.length needle
  and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = needle || go (i + 1)) in
  go 0
;;

let int_of_value = function
  | Row.V_int i -> Int64.to_int i
  | Row.V_real f -> int_of_float f
  | Row.V_null -> Alcotest.fail "expected an integer, got NULL"
  | Row.V_text s -> Alcotest.failf "expected an integer, got text %S" s
  | Row.V_blob _ -> Alcotest.fail "expected an integer, got a blob"
;;

let one_int rows =
  match rows with
  | [ (r : Row.t) ] when Array.length r = 1 -> int_of_value r.(0)
  | rows -> Alcotest.failf "expected one 1-column row, got %d rows" (List.length rows)
;;

let one_value rows =
  match rows with
  | [ (r : Row.t) ] when Array.length r = 1 -> r.(0)
  | rows -> Alcotest.failf "expected one 1-column row, got %d rows" (List.length rows)
;;

let ints rows =
  List.map
    (fun (r : Row.t) ->
       match Array.to_list r with
       | [ a ] -> int_of_value a
       | _ -> Alcotest.failf "expected a 1-column row, got %d" (Array.length r))
    rows
;;

let pairs rows =
  List.sort
    compare
    (List.map
       (fun (r : Row.t) ->
          match Array.to_list r with
          | [ a; b ] -> int_of_value a, int_of_value b
          | _ -> Alcotest.failf "expected a 2-column row, got %d" (Array.length r))
       rows)
;;

let pair_list = Alcotest.(list (pair int int))

(* Group 1: x = 10, 10, 20 — 3 rows, 2 distinct.
   Group 2: x = 5, 5, 5    — 3 rows, 1 distinct.
   Every distinct/non-distinct pair therefore differs, in both groups and over
   the whole table (6 rows, 3 distinct values). *)
let seed db =
  exec db "CREATE TABLE t (a INTEGER, x INTEGER)";
  List.iter
    (fun (a, x) -> exec db (Printf.sprintf "INSERT INTO t VALUES (%d, %d)" a x))
    [ 1, 10; 1, 10; 1, 20; 2, 5; 2, 5; 2, 5 ]
;;

(* A NULL x in each group, plus a duplicate, so "skips NULL" and "dedups" are
   independently visible. *)
let seed_with_nulls db =
  exec db "CREATE TABLE n (a INTEGER, x INTEGER)";
  exec db "INSERT INTO n VALUES (1, 10)";
  exec db "INSERT INTO n VALUES (1, 10)";
  exec db "INSERT INTO n VALUES (1, NULL)";
  exec db "INSERT INTO n VALUES (2, NULL)";
  exec db "INSERT INTO n VALUES (2, NULL)"
;;

(* ------------------------------------------------------------------ *)
(* The reported shape                                                   *)
(* ------------------------------------------------------------------ *)

let count_distinct_dedups () =
  with_db (fun db ->
    seed db;
    Alcotest.(check int)
      "COUNT(*) counts rows"
      6
      (one_int (query db "SELECT COUNT(*) FROM t"));
    Alcotest.(check int)
      "COUNT(x) counts non-null values"
      6
      (one_int (query db "SELECT COUNT(x) FROM t"));
    Alcotest.(check int)
      "COUNT(DISTINCT x) counts distinct values"
      3
      (one_int (query db "SELECT COUNT(DISTINCT x) FROM t")))
;;

(* Lower-case and mixed-case spellings reach the same production: the lexer
   folds keywords, so this is a cheap guard against a hand-written rule that
   only matched the upper-case token. *)
let count_distinct_is_case_insensitive () =
  with_db (fun db ->
    seed db;
    Alcotest.(check int)
      "lower case"
      3
      (one_int (query db "select count(distinct x) from t"));
    Alcotest.(check int)
      "mixed case"
      3
      (one_int (query db "SELECT Count(Distinct x) FROM t")))
;;

let count_distinct_skips_nulls () =
  with_db (fun db ->
    seed_with_nulls db;
    (* 5 rows, 2 non-null (both 10), 1 distinct non-null value. *)
    Alcotest.(check int) "COUNT(*)" 5 (one_int (query db "SELECT COUNT(*) FROM n"));
    Alcotest.(check int) "COUNT(x)" 2 (one_int (query db "SELECT COUNT(x) FROM n"));
    Alcotest.(check int)
      "COUNT(DISTINCT x) does not count the NULL"
      1
      (one_int (query db "SELECT COUNT(DISTINCT x) FROM n")))
;;

(* A group whose every value is NULL: the dedup keeps one NULL, and the
   aggregate must still drop it rather than count it as a distinct value. *)
let all_null_group_counts_zero () =
  with_db (fun db ->
    seed_with_nulls db;
    let got = pairs (query db "SELECT a, COUNT(DISTINCT x) FROM n GROUP BY a") in
    Alcotest.check pair_list "group 2 is all NULL and counts 0" [ 1, 1; 2, 0 ] got)
;;

let count_distinct_under_group_by () =
  with_db (fun db ->
    seed db;
    let got = pairs (query db "SELECT a, COUNT(DISTINCT x) FROM t GROUP BY a") in
    Alcotest.check pair_list "per-group distinct counts" [ 1, 2; 2, 1 ] got;
    (* The non-distinct answer differs in both groups, so a spec that silently
       ignored DISTINCT could not produce the list above. *)
    let plain = pairs (query db "SELECT a, COUNT(x) FROM t GROUP BY a") in
    Alcotest.check pair_list "and differs from the plain count" [ 1, 3; 2, 3 ] plain)
;;

(* Two aggregates over the same column in one projection, one DISTINCT and one
   not. They must occupy DIFFERENT slots: the slot-reuse test in
   [Sema.project_window] compares func and col_ord, and [distinct] had to join
   that comparison or the two would collapse onto one accumulator. *)
let distinct_and_plain_in_one_select_list () =
  with_db (fun db ->
    seed db;
    let rows = query db "SELECT COUNT(*), COUNT(x), COUNT(DISTINCT x) FROM t" in
    match rows with
    | [ (r : Row.t) ] when Array.length r = 3 ->
      Alcotest.(check (list int))
        "star, plain and distinct counts are three separate answers"
        [ 6; 6; 3 ]
        [ int_of_value r.(0); int_of_value r.(1); int_of_value r.(2) ]
    | _ -> Alcotest.fail "expected one 3-column row")
;;

let distinct_and_plain_under_group_by () =
  with_db (fun db ->
    seed db;
    let rows =
      query db "SELECT a, COUNT(x), COUNT(DISTINCT x) FROM t GROUP BY a ORDER BY a"
    in
    let triples =
      List.map
        (fun (r : Row.t) ->
           match Array.to_list r with
           | [ a; b; c ] -> int_of_value a, int_of_value b, int_of_value c
           | _ -> Alcotest.fail "expected a 3-column row")
        rows
    in
    Alcotest.(check (list (triple int int int)))
      "both aggregates computed per group"
      [ 1, 3, 2; 2, 3, 1 ]
      triples)
;;

(* ------------------------------------------------------------------ *)
(* The other aggregates                                                 *)
(* ------------------------------------------------------------------ *)

let sum_distinct () =
  with_db (fun db ->
    seed db;
    (* 10 + 10 + 20 + 5 + 5 + 5 = 55; distinct 10 + 20 + 5 = 35. *)
    Alcotest.(check int) "SUM(x)" 55 (one_int (query db "SELECT SUM(x) FROM t"));
    Alcotest.(check int)
      "SUM(DISTINCT x)"
      35
      (one_int (query db "SELECT SUM(DISTINCT x) FROM t")))
;;

let avg_distinct () =
  with_db (fun db ->
    seed db;
    (* AVG over the three distinct values 10, 20, 5 is 35/3. *)
    match one_value (query db "SELECT AVG(DISTINCT x) FROM t") with
    | Row.V_real f ->
      Alcotest.(check (float 1e-9)) "AVG(DISTINCT x) = 35/3" (35.0 /. 3.0) f
    | _ -> Alcotest.fail "expected AVG(DISTINCT x) to be a REAL")
;;

(* MIN/MAX cannot be changed by DISTINCT — deduplicating the input leaves the
   extremes where they were. The modifier is accepted anyway, because
   special-casing which aggregates may carry it is a second rule to keep in
   sync with the grammar; this pins that accepting it is harmless. *)
let min_max_distinct_agree_with_plain () =
  with_db (fun db ->
    seed db;
    Alcotest.(check int) "MIN" 5 (one_int (query db "SELECT MIN(x) FROM t"));
    Alcotest.(check int)
      "MIN(DISTINCT x)"
      5
      (one_int (query db "SELECT MIN(DISTINCT x) FROM t"));
    Alcotest.(check int) "MAX" 20 (one_int (query db "SELECT MAX(x) FROM t"));
    Alcotest.(check int)
      "MAX(DISTINCT x)"
      20
      (one_int (query db "SELECT MAX(DISTINCT x) FROM t")))
;;

let group_concat_distinct () =
  with_db (fun db ->
    seed db;
    match one_value (query db "SELECT GROUP_CONCAT(DISTINCT x) FROM t") with
    | Row.V_text s ->
      (* Order follows the scan, so assert the multiset of parts rather than a
         string: what DISTINCT decides is that each value appears once. *)
      let parts = List.sort compare (String.split_on_char ',' s) in
      Alcotest.(check (list string)) "each value once" [ "10"; "20"; "5" ] parts
    | _ -> Alcotest.fail "expected GROUP_CONCAT(DISTINCT x) to be TEXT")
;;

(* ------------------------------------------------------------------ *)
(* Composition with the rest of the aggregate machinery                 *)
(* ------------------------------------------------------------------ *)

(* #507's expression-over-aggregates path binds through [bind_expr_agg], a
   different site from the bare-aggregate projection binder. Both had to learn
   the new node. *)
let distinct_inside_an_expression () =
  with_db (fun db ->
    seed db;
    Alcotest.(check int)
      "COUNT(*) - COUNT(DISTINCT x)"
      3
      (one_int (query db "SELECT COUNT(*) - COUNT(DISTINCT x) FROM t")))
;;

let distinct_in_having () =
  with_db (fun db ->
    seed db;
    let got = ints (query db "SELECT a FROM t GROUP BY a HAVING COUNT(DISTINCT x) > 1") in
    Alcotest.(check (list int)) "only group 1 has two distinct values" [ 1 ] got)
;;

(* A WHERE clause routes the no-GROUP-BY case through the filtered arm of the
   #247 fast path, which builds an accumulator per aggregate rather than
   materialising the group. The DISTINCT accumulator lives there too. *)
let distinct_with_a_filter () =
  with_db (fun db ->
    seed db;
    Alcotest.(check int)
      "distinct count over the filtered rows"
      2
      (one_int (query db "SELECT COUNT(DISTINCT x) FROM t WHERE x >= 10"));
    Alcotest.(check int)
      "filter that selects the duplicated group"
      1
      (one_int (query db "SELECT COUNT(DISTINCT x) FROM t WHERE a = 2")))
;;

let distinct_over_a_join () =
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE u (a INTEGER, tag TEXT)";
    exec db "INSERT INTO u VALUES (1, 'one')";
    exec db "INSERT INTO u VALUES (2, 'two')";
    (* The TPC-C StockLevel shape: a distinct count of one side's column over a
       join, with no GROUP BY. *)
    Alcotest.(check int)
      "COUNT(DISTINCT x) over a join"
      3
      (one_int (query db "SELECT COUNT(DISTINCT x) FROM t INNER JOIN u ON t.a = u.a")))
;;

let distinct_over_an_empty_table () =
  with_db (fun db ->
    exec db "CREATE TABLE e (x INTEGER)";
    Alcotest.(check int)
      "COUNT(DISTINCT x) over no rows is 0"
      0
      (one_int (query db "SELECT COUNT(DISTINCT x) FROM e"));
    Alcotest.(check bool)
      "SUM(DISTINCT x) over no rows is NULL"
      true
      (one_value (query db "SELECT SUM(DISTINCT x) FROM e") = Row.V_null))
;;

(* ------------------------------------------------------------------ *)
(* Refused spellings                                                    *)
(* ------------------------------------------------------------------ *)

let multiple_arguments_rejected () =
  with_db (fun db ->
    seed db;
    let msg = query_err db "SELECT COUNT(DISTINCT a, x) FROM t" in
    Alcotest.(check bool)
      (Printf.sprintf "names the arity rule, got %S" msg)
      true
      (contains_sub ~needle:"exactly one argument" msg);
    (* GROUP_CONCAT is the one aggregate with a legal two-argument form, so its
       DISTINCT spelling is the one a bare grammar would have accepted by
       accident. *)
    let msg = query_err db "SELECT GROUP_CONCAT(DISTINCT x, '-') FROM t" in
    Alcotest.(check bool)
      (Printf.sprintf "GROUP_CONCAT(DISTINCT x, sep) refused, got %S" msg)
      true
      (contains_sub ~needle:"exactly one argument" msg))
;;

let distinct_window_function_rejected () =
  with_db (fun db ->
    seed db;
    let msg = query_err db "SELECT COUNT(DISTINCT x) OVER () FROM t" in
    Alcotest.(check bool)
      (Printf.sprintf "names the window rule, got %S" msg)
      true
      (contains_sub ~needle:"window function" msg))
;;

let distinct_aggregate_in_where_rejected () =
  with_db (fun db ->
    seed db;
    let msg = query_err db "SELECT a FROM t WHERE COUNT(DISTINCT x) > 1" in
    Alcotest.(check bool)
      (Printf.sprintf "refused like any aggregate in WHERE, got %S" msg)
      true
      (contains_sub ~needle:"aggregate in WHERE" msg))
;;

(* ------------------------------------------------------------------ *)
(* Property: COUNT(DISTINCT x) equals the model over the same rows      *)
(* ------------------------------------------------------------------ *)

let prop_matches_model =
  QCheck.Test.make
    ~count:60
    ~name:"COUNT(DISTINCT x) equals the cardinality of the non-null value set"
    QCheck.(list_of_size Gen.(int_range 0 40) (option (int_range (-5) 5)))
    (fun values ->
       with_db (fun db ->
         exec db "CREATE TABLE p (x INTEGER)";
         List.iter
           (fun v ->
              match v with
              | None -> exec db "INSERT INTO p VALUES (NULL)"
              | Some i -> exec db (Printf.sprintf "INSERT INTO p VALUES (%d)" i))
           values;
         let expected =
           List.sort_uniq compare (List.filter_map Fun.id values) |> List.length
         in
         let got = one_int (query db "SELECT COUNT(DISTINCT x) FROM p") in
         got = expected))
;;

let () =
  Alcotest.run
    "count_distinct_491"
    [ ( "counting"
      , [ Alcotest.test_case "dedups" `Quick count_distinct_dedups
        ; Alcotest.test_case "case insensitive" `Quick count_distinct_is_case_insensitive
        ; Alcotest.test_case "skips NULL" `Quick count_distinct_skips_nulls
        ; Alcotest.test_case "all-NULL group" `Quick all_null_group_counts_zero
        ; Alcotest.test_case "GROUP BY" `Quick count_distinct_under_group_by
        ; Alcotest.test_case
            "distinct beside plain"
            `Quick
            distinct_and_plain_in_one_select_list
        ; Alcotest.test_case
            "distinct beside plain, grouped"
            `Quick
            distinct_and_plain_under_group_by
        ] )
    ; ( "other aggregates"
      , [ Alcotest.test_case "SUM" `Quick sum_distinct
        ; Alcotest.test_case "AVG" `Quick avg_distinct
        ; Alcotest.test_case "MIN/MAX" `Quick min_max_distinct_agree_with_plain
        ; Alcotest.test_case "GROUP_CONCAT" `Quick group_concat_distinct
        ] )
    ; ( "composition"
      , [ Alcotest.test_case "inside an expression" `Quick distinct_inside_an_expression
        ; Alcotest.test_case "HAVING" `Quick distinct_in_having
        ; Alcotest.test_case "with a filter" `Quick distinct_with_a_filter
        ; Alcotest.test_case "over a join" `Quick distinct_over_a_join
        ; Alcotest.test_case "empty table" `Quick distinct_over_an_empty_table
        ] )
    ; ( "rejected"
      , [ Alcotest.test_case "multiple arguments" `Quick multiple_arguments_rejected
        ; Alcotest.test_case "window function" `Quick distinct_window_function_rejected
        ; Alcotest.test_case "in WHERE" `Quick distinct_aggregate_in_where_rejected
        ] )
    ; "properties", List.map QCheck_alcotest.to_alcotest [ prop_matches_model ]
    ]
;;
