(** #489 / #490: ORDER BY terms that name the SELECT list — an ordinal, or an
    output alias.

    #489 was the worse of the two because it was silent. [ORDER BY 2] parsed,
    bound as an ordinary integer literal, and became a sort key with the same
    constant value for every row, so the rows came back in scan order with no
    ordering applied and no error raised. #490 was loud but blocked five TPC-H
    queries: [SELECT nm, COUNT(x) AS n ... ORDER BY n] answered
    "unknown column: t.n", because ORDER BY resolved names against the base
    tables only.

    THE PRECEDENCE this pins, which is SQL:92 / SQL:2003 §7.13 and what SQLite
    does:

    - a BARE INTEGER LITERAL is an ordinal, the 1-based position in the SELECT
      list; out of range (0, negative, past the end) is an ERROR, never a
      silent no-op;
    - a BARE IDENTIFIER resolves to an output ALIAS FIRST and to an input
      column only if no alias matches — so an alias shadowing a real column
      name wins.  A QUALIFIED name is not a bare identifier and always means
      the input column;
    - anything else is an expression, evaluated and never matched against the
      SELECT list: [ORDER BY 1+1] and [ORDER BY -1] are constants, not
      ordinals.

    Every test below asserts the actual ROW ORDER. A test that only counted
    rows is exactly what let #489 ship: the buggy engine returned the right
    rows in the wrong order. Where a case is about a term that is deliberately
    NOT an output reference (a constant expression), the constant is paired
    with a second, real sort key, so the assertion stays deterministic without
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
      (rows db "SELECT nm, COUNT(*) FROM g2 GROUP BY nm ORDER BY nm");
    (* #490: the alias spelling of the same key, on the same collision. *)
    check_rows
      "alias key survives the post-aggregate remap"
      [ "y|1"; "z|2"; "x|3" ]
      (rows db "SELECT nm, COUNT(*) AS n FROM g2 GROUP BY nm ORDER BY n"))
;;

(* ── #490: output aliases ─────────────────────────── *)

let test_490_repro () =
  with_db (fun db ->
    exec db "CREATE TABLE g (nm TEXT)";
    List.iter
      (fun v -> exec db (Printf.sprintf "INSERT INTO g VALUES ('%s')" v))
      [ "x"; "y"; "x"; "z"; "x"; "z" ];
    (* The issue's query: "sema error: unknown column: g.n" before the fix. *)
    check_rows
      "ORDER BY an aggregate alias"
      [ "y|1"; "z|2"; "x|3" ]
      (rows db "SELECT nm, COUNT(*) AS n FROM g GROUP BY nm ORDER BY n");
    check_rows
      "ORDER BY an aggregate alias, descending"
      [ "x|3"; "z|2"; "y|1" ]
      (rows db "SELECT nm, COUNT(*) AS n FROM g GROUP BY nm ORDER BY n DESC");
    (* The ordinal spelling of the same thing (#489). *)
    check_rows
      "ORDER BY 2 DESC over an aggregate"
      [ "x|3"; "z|2"; "y|1" ]
      (rows db "SELECT nm, COUNT(*) AS n FROM g GROUP BY nm ORDER BY 2 DESC");
    (* An alias that does not exist is still an unknown column, not a
       mysterious no-op. *)
    match run (Db.query db "SELECT nm, COUNT(*) AS n FROM g GROUP BY nm ORDER BY zz") with
    | Ok _ -> Alcotest.fail "expected an error for an unknown ORDER BY name"
    | Error _ -> ())
;;

let test_alias_over_expression () =
  with_db (fun db ->
    exec db "CREATE TABLE e (a INTEGER, b INTEGER)";
    exec db "INSERT INTO e VALUES (1, 5)";
    exec db "INSERT INTO e VALUES (2, 1)";
    exec db "INSERT INTO e VALUES (3, 0)";
    (* a+b = 6, 3, 3 *)
    check_rows
      "ORDER BY an expression alias"
      [ "2|3"; "3|3"; "1|6" ]
      (rows db "SELECT a, a + b AS s FROM e ORDER BY s, a");
    check_rows
      "ORDER BY an expression alias, descending"
      [ "1|6"; "3|3"; "2|3" ]
      (rows db "SELECT a, a + b AS s FROM e ORDER BY s DESC, a DESC");
    (* The ordinal spelling of the same key. *)
    check_rows
      "ORDER BY 2 over the same expression"
      [ "2|3"; "3|3"; "1|6" ]
      (rows db "SELECT a, a + b AS s FROM e ORDER BY 2, 1"))
;;

(* THE PRECEDENCE CASE.  [b AS a] means the name [a] in ORDER BY refers to the
   OUTPUT column (whose values are b's), not to the input column [a] it
   shadows.  The two orders are exact reverses of each other here, so this
   cannot pass by accident. *)
let test_alias_shadows_a_real_column () =
  with_db (fun db ->
    exec db "CREATE TABLE sh (a INTEGER, b INTEGER)";
    exec db "INSERT INTO sh VALUES (1, 30)";
    exec db "INSERT INTO sh VALUES (2, 20)";
    exec db "INSERT INTO sh VALUES (3, 10)";
    (* Output row is (b, a).  ORDER BY a = the alias = b ascending. *)
    check_rows
      "output alias wins over the input column it shadows"
      [ "10|3"; "20|2"; "30|1" ]
      (rows db "SELECT b AS a, a AS b FROM sh ORDER BY a");
    (* And symmetrically for the other name. *)
    check_rows
      "the other alias too"
      [ "30|1"; "20|2"; "10|3" ]
      (rows db "SELECT b AS a, a AS b FROM sh ORDER BY b");
    (* A qualified reference is NOT a bare identifier, so it never matches an
       alias and always names the input column. *)
    check_rows
      "a qualified name still means the input column"
      [ "30|1"; "20|2"; "10|3" ]
      (rows db "SELECT b AS a, a AS b FROM sh ORDER BY sh.a");
    (* No alias to match: the input column, exactly as before. *)
    check_rows
      "no alias to match: input column"
      [ "3|10"; "2|20"; "1|30" ]
      (rows db "SELECT a, b FROM sh ORDER BY b"))
;;

(* ── Several keys at once, mixing all three kinds ── *)

let test_mixed_keys () =
  with_db (fun db ->
    exec db "CREATE TABLE m (a INTEGER, b INTEGER, c INTEGER)";
    exec db "INSERT INTO m VALUES (1, 1, 2)";
    exec db "INSERT INTO m VALUES (1, 2, 1)";
    exec db "INSERT INTO m VALUES (1, 1, 1)";
    exec db "INSERT INTO m VALUES (2, 5, 5)";
    (* ordinal, then alias DESC, then a plain input column. *)
    check_rows
      "ordinal + alias + column"
      [ "1|2|1"; "1|1|1"; "1|1|2"; "2|5|5" ]
      (rows db "SELECT a, b AS bb, c FROM m ORDER BY 1, bb DESC, c");
    (* The same three keys, spelled the other way round. *)
    check_rows
      "column + ordinal DESC + ordinal"
      [ "1|2|1"; "1|1|1"; "1|1|2"; "2|5|5" ]
      (rows db "SELECT a, b AS bb, c FROM m ORDER BY a, 2 DESC, 3"))
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
        "SELECT enm, dnm AS d FROM emp JOIN dpt ON emp.dno = dpt.dkey ORDER BY %s"
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
    (* #490 over a join: the issue reported "unknown column: supplier.numwait"
       for exactly this shape. *)
    check_rows
      "join, alias then ordinal"
      [ "amy|ops"; "bob|eng"; "zoe|eng" ]
      (rows db (q "d DESC, 1"));
    (* An ordinal counts OUTPUT columns, not the combined join row's — the
       projection here is 2 wide even though the joined row is 5 wide. *)
    check_ordinal_error
      db
      "SELECT enm, dnm AS d FROM emp JOIN dpt ON emp.dno = dpt.dkey ORDER BY 3")
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
    "order_by_ref_489_490"
    [ ( "#489 ordinals"
      , [ Alcotest.test_case "issue repro" `Quick test_489_repro
        ; Alcotest.test_case "over SELECT *" `Quick test_ordinal_over_star
        ; Alcotest.test_case "out of range raises" `Quick test_ordinal_out_of_range
        ; Alcotest.test_case
            "an expression is not an ordinal"
            `Quick
            test_expression_is_not_an_ordinal
        ; Alcotest.test_case "over an aggregate" `Quick test_aggregate_ordinal
        ; Alcotest.test_case "compound" `Quick test_compound_ordinal
        ] )
    ; ( "#490 output aliases"
      , [ Alcotest.test_case "issue repro" `Quick test_490_repro
        ; Alcotest.test_case "expression alias" `Quick test_alias_over_expression
        ; Alcotest.test_case
            "alias shadows a column"
            `Quick
            test_alias_shadows_a_real_column
        ] )
    ; ( "combined"
      , [ Alcotest.test_case "mixed key kinds" `Quick test_mixed_keys
        ; Alcotest.test_case "over a join" `Quick test_join_ordinal
        ] )
    ]
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5"]
[@@@ai_provider "Anthropic"]
