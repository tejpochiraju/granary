(** #489: ORDER BY <ordinal> — an integer literal naming the Nth column of the
    SELECT list — was silently ignored.

    It parsed, bound as an ordinary integer literal, and became a sort key with
    the same constant value for every row, so the rows came back in scan order
    with no ordering applied and no error raised. A silent wrong answer is the
    worst of the three possible outcomes, and this is one of four the TPC-H
    harness surfaced.

    THE RULE this pins, which is SQL:92 and what SQLite does:

    - a BARE INTEGER LITERAL is an ordinal, the 1-based position in the SELECT
      list; out of range (0, negative, past the end) is an ERROR, never a
      silent no-op;
    - anything else is an expression, evaluated and never matched against the
      SELECT list: [ORDER BY 1+1] and [ORDER BY -1] are constants, not
      ordinals.

    Every test below asserts the actual ROW ORDER. A test that only counted
    rows is exactly what let this ship: the buggy engine returned the right
    rows in the wrong order. Where a case is about a term that is deliberately
    NOT an ordinal (a constant expression), the constant is paired with a
    second, real sort key, so the assertion stays deterministic without
    depending on whether the sort is stable. *)

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

let render = function
  | Row.V_int n -> Int64.to_string n
  | Row.V_text s -> s
  | Row.V_real f -> Printf.sprintf "%h" f
  | Row.V_blob b -> Bytes.to_string b
  | Row.V_null -> "NULL"
;;

(* One row per string, columns joined by '|' — so an assertion states the whole
   answer, in order, in one literal. *)
let rows db sql =
  run
    (let open Lwt.Syntax in
     let* r = Db.query db sql in
     match r with
     | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
     | Ok stream ->
       let* rs = Lwt_stream.to_list stream in
       Lwt.return
         (List.map
            (fun (r : Row.t) -> String.concat "|" (Array.to_list (Array.map render r)))
            rs))
;;

let query_err db sql =
  match run (Db.query db sql) with
  | Ok _ -> Alcotest.failf "expected an error for %S, got rows" sql
  | Error e -> Format.asprintf "%a" Db.pp_error e
;;

let check_rows msg want got = Alcotest.(check (list string)) msg want got

let contains_sub ~needle s =
  let n = String.length needle
  and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = needle || go (i + 1)) in
  go 0
;;

(* An out-of-range ordinal must be refused, and the message must name the
   thing that is wrong. *)
let check_ordinal_error db sql =
  let msg = query_err db sql in
  if not (contains_sub ~needle:"ORDER BY position" msg)
  then Alcotest.failf "%S: expected an ORDER BY position error, got %S" sql msg
;;

(* ── The issue's own repro ────────────────────────────────────────── *)

let seed_t db =
  exec db "CREATE TABLE t (k INTEGER, nm TEXT)";
  exec db "INSERT INTO t VALUES (3,'a')";
  exec db "INSERT INTO t VALUES (1,'c')";
  exec db "INSERT INTO t VALUES (2,'b')"
;;

let test_489_repro () =
  with_db (fun db ->
    seed_t db;
    (* Both of these came back as 3|a 1|c 2|b before the fix. *)
    check_rows
      "ORDER BY 2 sorts by nm"
      [ "3|a"; "2|b"; "1|c" ]
      (rows db "SELECT k, nm FROM t ORDER BY 2");
    check_rows
      "ORDER BY 1 DESC sorts by k descending"
      [ "3|a"; "2|b"; "1|c" ]
      (rows db "SELECT k, nm FROM t ORDER BY 1 DESC");
    check_rows
      "ORDER BY 1 sorts by k ascending"
      [ "1|c"; "2|b"; "3|a" ]
      (rows db "SELECT k, nm FROM t ORDER BY 1");
    check_rows
      "ORDER BY 2 DESC sorts by nm descending"
      [ "1|c"; "2|b"; "3|a" ]
      (rows db "SELECT k, nm FROM t ORDER BY 2 DESC"))
;;

let test_ordinal_over_star () =
  with_db (fun db ->
    seed_t db;
    (* [SELECT *] has no expression projection at all; the ordinal has to be
       resolved against the expanded column list. *)
    check_rows
      "SELECT * ORDER BY 2 DESC"
      [ "1|c"; "2|b"; "3|a" ]
      (rows db "SELECT * FROM t ORDER BY 2 DESC");
    check_rows
      "SELECT * ORDER BY 1"
      [ "1|c"; "2|b"; "3|a" ]
      (rows db "SELECT * FROM t ORDER BY 1"))
;;

let test_ordinal_out_of_range () =
  with_db (fun db ->
    seed_t db;
    (* Past the end. *)
    check_ordinal_error db "SELECT k, nm FROM t ORDER BY 3";
    check_ordinal_error db "SELECT k FROM t ORDER BY 2";
    check_ordinal_error db "SELECT * FROM t ORDER BY 99";
    (* Zero: SQL ordinals are 1-based, so 0 names nothing. *)
    check_ordinal_error db "SELECT k, nm FROM t ORDER BY 0";
    (* An aggregated SELECT resolves ordinals against its OUTPUT list, whose
       arity is the projection's, not the table's. *)
    check_ordinal_error db "SELECT nm, COUNT(*) FROM t GROUP BY nm ORDER BY 3")
;;

(* A term that is not a *bare* integer literal is an expression, not an
   ordinal — so it never errors for being "out of range" and never sorts by a
   select-list column.  Each case pairs the constant with a real second key so
   the expected order is exact whether or not the sort is stable. *)
let test_expression_is_not_an_ordinal () =
  with_db (fun db ->
    seed_t db;
    (* If [1+1] were folded to the ordinal 2 the answer would be nm-ordered
       (3|a 2|b 1|c); as a constant, the k key decides. *)
    check_rows
      "1+1 is a constant, not the ordinal 2"
      [ "1|c"; "2|b"; "3|a" ]
      (rows db "SELECT k, nm FROM t ORDER BY 1+1, 1");
    (* A negative literal is [E_neg (E_lit ...)], not a bare literal: it is a
       constant, and must not be rejected as an out-of-range ordinal. *)
    check_rows
      "-1 is a constant, not an ordinal"
      [ "3|a"; "2|b"; "1|c" ]
      (rows db "SELECT k, nm FROM t ORDER BY -1, 2");
    (* A compound expression over real columns is evaluated as written. *)
    check_rows
      "ORDER BY an expression"
      [ "1|c"; "2|b"; "3|a" ]
      (rows db "SELECT k, nm FROM t ORDER BY k * 2");
    check_rows
      "ORDER BY an expression, descending"
      [ "3|a"; "2|b"; "1|c" ]
      (rows db "SELECT k, nm FROM t ORDER BY k * 2 DESC"))
;;

(* ── Over an aggregate ───────────────────────────── *)

(* An aggregated SELECT sorts AFTER projection, and the planner remaps
   pre-aggregation column indices into output positions on the way.  An
   ordinal already addresses the OUTPUT row and must NOT go through that
   remapping.  This shape makes the difference observable: the grouping column
   sits at INPUT index 1 and the count sits at OUTPUT index 1, so a key that
   was remapped would collide with the GROUP BY column and sort by [nm]
   instead — silently, which is the very failure mode this issue is about. *)
let test_aggregate_ordinal () =
  with_db (fun db ->
    exec db "CREATE TABLE g2 (pad TEXT, nm TEXT)";
    List.iter
      (fun v -> exec db (Printf.sprintf "INSERT INTO g2 VALUES ('p','%s')" v))
      [ "x"; "y"; "x"; "z"; "x"; "z" ];
    (* Counts are x=3, y=1, z=2.  Sorted by nm it would be x, y, z. *)
    check_rows
      "ordinal key survives the post-aggregate remap"
      [ "y|1"; "z|2"; "x|3" ]
      (rows db "SELECT nm, COUNT(*) FROM g2 GROUP BY nm ORDER BY 2");
    check_rows
      "ordinal 1 names the grouping column"
      [ "z|2"; "y|1"; "x|3" ]
      (rows db "SELECT nm, COUNT(*) FROM g2 GROUP BY nm ORDER BY 1 DESC");
    (* The grouping column spelled by name still resolves and still remaps. *)
    check_rows
      "grouping column key still remaps"
      [ "x|3"; "y|1"; "z|2" ]
      (rows db "SELECT nm, COUNT(*) FROM g2 GROUP BY nm ORDER BY nm"))
;;

(* ── Over a join ─────────────────────────────────── *)

let test_join_ordinal () =
  with_db (fun db ->
    exec db "CREATE TABLE emp (eid INTEGER, dno INTEGER, enm TEXT)";
    exec db "CREATE TABLE dpt (dkey INTEGER, dnm TEXT)";
    exec db "INSERT INTO emp VALUES (1, 10, 'zoe')";
    exec db "INSERT INTO emp VALUES (2, 20, 'amy')";
    exec db "INSERT INTO emp VALUES (3, 10, 'bob')";
    exec db "INSERT INTO dpt VALUES (10, 'eng')";
    exec db "INSERT INTO dpt VALUES (20, 'ops')";
    let q order =
      Printf.sprintf
        "SELECT enm, dnm FROM emp JOIN dpt ON emp.dno = dpt.dkey ORDER BY %s"
        order
    in
    check_rows
      "join, ordinal keys"
      [ "bob|eng"; "zoe|eng"; "amy|ops" ]
      (rows db (q "2, 1"));
    check_rows
      "join, ordinal over the left table's column"
      [ "amy|ops"; "bob|eng"; "zoe|eng" ]
      (rows db (q "1"));
    (* An ordinal counts OUTPUT columns, not the combined join row's — the
       projection here is 2 wide even though the joined row is 5 wide. *)
    check_ordinal_error
      db
      "SELECT enm, dnm FROM emp JOIN dpt ON emp.dno = dpt.dkey ORDER BY 3")
;;

(* ── Compound (UNION/…) ordinals ──────────────────────────────────── *)

let test_compound_ordinal () =
  with_db (fun db ->
    exec db "CREATE TABLE u1 (x INTEGER)";
    exec db "CREATE TABLE u2 (x INTEGER)";
    exec db "INSERT INTO u1 VALUES (3)";
    exec db "INSERT INTO u1 VALUES (1)";
    exec db "INSERT INTO u2 VALUES (2)";
    exec db "INSERT INTO u2 VALUES (4)";
    check_rows
      "UNION ALL ORDER BY 1"
      [ "1"; "2"; "3"; "4" ]
      (rows db "SELECT x FROM u1 UNION ALL SELECT x FROM u2 ORDER BY 1");
    check_rows
      "UNION ALL ORDER BY 1 DESC"
      [ "4"; "3"; "2"; "1" ]
      (rows db "SELECT x FROM u1 UNION ALL SELECT x FROM u2 ORDER BY 1 DESC");
    check_ordinal_error db "SELECT x FROM u1 UNION ALL SELECT x FROM u2 ORDER BY 2";
    check_ordinal_error db "SELECT x FROM u1 UNION ALL SELECT x FROM u2 ORDER BY 0")
;;

let () =
  Alcotest.run
    "order_by_ordinal_489"
    [ ( "#489 ordinals"
      , [ Alcotest.test_case "issue repro" `Quick test_489_repro
        ; Alcotest.test_case "over SELECT *" `Quick test_ordinal_over_star
        ; Alcotest.test_case "out of range raises" `Quick test_ordinal_out_of_range
        ; Alcotest.test_case
            "an expression is not an ordinal"
            `Quick
            test_expression_is_not_an_ordinal
        ; Alcotest.test_case "over an aggregate" `Quick test_aggregate_ordinal
        ; Alcotest.test_case "over a join" `Quick test_join_ordinal
        ; Alcotest.test_case "compound" `Quick test_compound_ordinal
        ] )
    ]
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5"]
[@@@ai_provider "Anthropic"]
