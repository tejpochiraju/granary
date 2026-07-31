module Engine = struct
  type t = { mutable stmts : string list }

  let name = "recording"

  let open_db ~dir =
    ignore dir;
    { stmts = [] }
  ;;

  let exec t sql = t.stmts <- sql :: t.stmts
  let query_rows _ _ = []
  let close _ = ()
end

module L = Granary_tpc.Tpch_schema.Load (Engine)

let test_literal_escapes_quotes () =
  Alcotest.(check string)
    "single quotes are doubled"
    "'O''Brien'"
    (Granary_tpc.Tpch_schema.literal (Granary_tpc.Tpch_gen.VText "O'Brien"))
;;

let test_literal_int_and_real () =
  Alcotest.(check string)
    "int"
    "42"
    (Granary_tpc.Tpch_schema.literal (Granary_tpc.Tpch_gen.VInt 42));
  Alcotest.(check bool)
    "real round-trips"
    true
    (Float.abs
       (float_of_string
          (Granary_tpc.Tpch_schema.literal (Granary_tpc.Tpch_gen.VReal 1234.56))
        -. 1234.56)
     < 1e-9)
;;

(* Regression net for the bug that broke every load (#482, task 7): "%.17g"
   renders 100.0 as "100", an INTEGER literal, and granary's strict column
   typing rejects it for a REAL column.  A whole-valued REAL must keep a
   fractional part.  Pinned as exact text rather than round-tripped, because a
   round-trip check is exactly what failed to catch the original bug. *)
let test_literal_whole_reals_keep_a_decimal_point () =
  let literal f = Granary_tpc.Tpch_schema.literal (Granary_tpc.Tpch_gen.VReal f) in
  Alcotest.(check string) "positive whole" "100.0" (literal 100.0);
  Alcotest.(check string) "negative whole" "-100.0" (literal (-100.0));
  Alcotest.(check string) "zero" "0.0" (literal 0.0);
  Alcotest.(check string) "negative zero" "-0.0" (literal (-0.0));
  (* already fractional: left exactly as "%.17g" renders it *)
  Alcotest.(check string) "fractional" "1234.5" (literal 1234.5)
;;

let test_ddl_covers_every_table () =
  let ddl = String.concat " " Granary_tpc.Tpch_schema.ddl in
  List.iter
    (fun table ->
       let needle = "CREATE TABLE " ^ table in
       let found =
         let nl = String.length needle in
         let rec go i =
           i + nl <= String.length ddl && (String.sub ddl i nl = needle || go (i + 1))
         in
         go 0
       in
       Alcotest.(check bool) (table ^ " has DDL") true found)
    Granary_tpc.Tpch_gen.tables
;;

(* Extracts (column name, declared type) pairs from a single
   `CREATE TABLE table (...)` statement, in order. Column lines look like
   `  col_name TYPE ...` (e.g. `s_acctbal   REAL NOT NULL`); we only need the
   first two whitespace-delimited tokens per line inside the parens. *)
let ddl_columns_and_types_for stmt =
  let open_paren = String.index stmt '(' in
  let close_paren = String.rindex stmt ')' in
  let body = String.sub stmt (open_paren + 1) (close_paren - open_paren - 1) in
  String.split_on_char ',' body
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> List.map (fun line ->
    let flat = String.map (fun c -> if c = '\n' || c = '\t' then ' ' else c) line in
    let tokens = String.split_on_char ' ' flat |> List.filter (fun s -> s <> "") in
    match tokens with
    | name :: ty :: _ -> name, ty
    | [ name ] -> name, ""
    | [] -> "", "")
;;

(* Column names alone, in order — used by the DDL/`column_names` agreement
   test below. *)
let ddl_columns_for stmt = List.map fst (ddl_columns_and_types_for stmt)

let ddl_table_of stmt =
  (* "CREATE TABLE <name> (" -> <name> *)
  let prefix = "CREATE TABLE " in
  let start = String.length prefix in
  let rest = String.sub stmt start (String.length stmt - start) in
  match String.index_opt rest ' ', String.index_opt rest '(' with
  | Some sp, Some par -> String.sub rest 0 (min sp par)
  | Some sp, None -> String.sub rest 0 sp
  | None, Some par -> String.sub rest 0 par
  | None, None -> rest
;;

let test_ddl_columns_match_tpch_gen_column_names () =
  List.iter
    (fun stmt ->
       let table = ddl_table_of stmt in
       let ddl_cols = ddl_columns_for stmt in
       let gen_cols = Granary_tpc.Tpch_gen.column_names ~table in
       Alcotest.(check (list string))
         (table ^ ": DDL column order matches Tpch_gen.column_names")
         gen_cols
         ddl_cols)
    Granary_tpc.Tpch_schema.ddl
;;

let value_matches_ddl_type ty v =
  match ty, v with
  | "INTEGER", Granary_tpc.Tpch_gen.VInt _ -> true
  | "REAL", Granary_tpc.Tpch_gen.VReal _ -> true
  | "TEXT", Granary_tpc.Tpch_gen.VText _ -> true
  | _ -> false
;;

let count_ddl_type_mismatches g ~table ~types =
  let bad = ref 0
  and seen = ref 0 in
  Granary_tpc.Tpch_gen.iter_rows g ~table ~f:(fun row ->
    incr seen;
    Array.iteri (fun i v -> if not (value_matches_ddl_type types.(i) v) then incr bad) row);
  !bad, !seen
;;

(* Guards against a `Tpch_gen` row emitting a value whose constructor doesn't
   match the DDL's declared type for that column position — e.g. a `VInt`
   flowing into a `REAL` column. On granary this is a load-time
   [Type_mismatch] rather than a silent coercion (there is no affinity
   coercion on INSERT), so a recording-engine test like the others in this
   file can't see it; this test inspects the generated values directly
   instead. Checks every row at a small scale factor. *)
let test_row_values_match_ddl_column_types () =
  let g = Granary_tpc.Tpch_gen.create ~seed:5 ~sf:0.01 in
  List.iter
    (fun stmt ->
       let table = ddl_table_of stmt in
       let types = Array.of_list (List.map snd (ddl_columns_and_types_for stmt)) in
       let bad, seen = count_ddl_type_mismatches g ~table ~types in
       Alcotest.(check int)
         (table ^ ": every value's constructor matches its DDL column type")
         0
         bad;
       Alcotest.(check bool) (table ^ ": rows were actually inspected") true (seen > 0))
    Granary_tpc.Tpch_schema.ddl
;;

let test_load_wraps_each_table_in_a_transaction () =
  let g = Granary_tpc.Tpch_gen.create ~seed:1 ~sf:0.001 in
  let e = Engine.open_db ~dir:"/tmp" in
  L.run e g;
  let stmts = List.rev e.Engine.stmts in
  let begins = List.length (List.filter (fun s -> s = "BEGIN") stmts) in
  let commits = List.length (List.filter (fun s -> s = "COMMIT") stmts) in
  Alcotest.(check int)
    "one BEGIN per table"
    (List.length Granary_tpc.Tpch_gen.tables)
    begins;
  Alcotest.(check int) "BEGIN and COMMIT are balanced" begins commits
;;

let test_indexes_are_created_after_inserts () =
  let g = Granary_tpc.Tpch_gen.create ~seed:1 ~sf:0.001 in
  let e = Engine.open_db ~dir:"/tmp" in
  L.run e g;
  let stmts = Array.of_list (List.rev e.Engine.stmts) in
  let starts_with p s =
    String.length s >= String.length p && String.sub s 0 (String.length p) = p
  in
  let last_insert = ref (-1)
  and first_index = ref max_int in
  Array.iteri
    (fun i s ->
       if starts_with "INSERT" s then last_insert := i;
       if starts_with "CREATE INDEX" s && i < !first_index then first_index := i)
    stmts;
  Alcotest.(check bool)
    "every index is created after the last insert"
    true
    (!first_index > !last_insert)
;;

let () =
  Alcotest.run
    "tpch_load"
    [ ( "literal"
      , [ Alcotest.test_case "escapes quotes" `Quick test_literal_escapes_quotes
        ; Alcotest.test_case "int and real" `Quick test_literal_int_and_real
        ; Alcotest.test_case
            "whole reals keep a decimal point"
            `Quick
            test_literal_whole_reals_keep_a_decimal_point
        ] )
    ; ( "ddl"
      , [ Alcotest.test_case "covers every table" `Quick test_ddl_covers_every_table
        ; Alcotest.test_case
            "columns match Tpch_gen.column_names"
            `Quick
            test_ddl_columns_match_tpch_gen_column_names
        ; Alcotest.test_case
            "row value constructors match column types"
            `Quick
            test_row_values_match_ddl_column_types
        ] )
    ; ( "load"
      , [ Alcotest.test_case
            "transaction per table"
            `Quick
            test_load_wraps_each_table_in_a_transaction
        ; Alcotest.test_case "indexes last" `Quick test_indexes_are_created_after_inserts
        ] )
    ]
;;
