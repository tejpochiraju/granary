(** #486: comma-separated FROM lists and derived tables.

    {1 What was missing}

    [from_tail] accepted exactly one bare identifier plus a chain of
    [JOIN t ON e] clauses, so neither

    {v
      SELECT * FROM a, b WHERE a.y = b.y;
      SELECT * FROM (SELECT x FROM a) AS t;
    v}

    parsed at all.  Between them they account for ~18 of the 20 TPC-H queries
    that failed to parse.

    {1 How each is closed}

    Both are closed in the parser, with no new [Ast] node and no new kind of
    FROM item for the layers below to learn:

    - a comma is an INNER join whose ON predicate is the literal [1].  The
      restriction of an implicit join lives in WHERE, so at the join itself the
      pairing is unrestricted.  [Planner.on_is_trivially_true] recognises that
      literal and [Planner.where_join_key] hands the join one WHERE equality
      that spans its two sides, so [FROM a, b WHERE a.y = b.y] plans to the
      same keyed hash join [FROM a JOIN b ON a.y = b.y] does rather than to a
      cartesian product with a filter on top.
    - a derived table is desugared into a non-recursive CTE wrapped around the
      SELECT that names it — the shape [Sema.expand_views] already builds for a
      view named in FROM position.

    {1 Scope}

    CLAUDE.md's #635 rule is that an input's {b scope identifier} is its FROM
    item's alias where it has one and its table name otherwise, an alias
    {i replacing} the name.  A derived table has no underlying name, so its
    alias becomes the CTE's NAME and the FROM item carries no alias — which is
    what makes all three levels ([Sema.from_ident], [Exec.inner_scope_of],
    [Exec.scan_ident] / [get_outer_scan_metas]) answer the alias with no new
    case.  An unaliased derived table is legal (sqlite3 accepts it) and is
    named after its byte offset in the statement.

    {1 Oracle}

    Every expected value below was taken from sqlite3 3.45.1 (in the dev
    image), including the two refusals. *)

open Lwt.Syntax
module Db = Granary.Db
module Cat = Granary_catalog.Catalog
module S = Granary_store.Store
module Row = Granary_encoding.Row
module Ast = Granary_sql.Ast
module Parser = Granary_sql.Parser
module Lexer = Granary_sql.Lexer
module Sema = Granary_sql.Sema
module Plan = Granary_sql.Plan
module Planner = Granary_sql.Planner

let run = Lwt_main.run

let parse s =
  let lexbuf = Lexing.from_string s in
  Parser.stmt_eof Lexer.token lexbuf
;;

(* ------------------------------------------------------------------ *)
(* End-to-end harness                                                   *)
(* ------------------------------------------------------------------ *)

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
  | Db.V_int n -> Int64.to_string n
  | Db.V_text s -> s
  | Db.V_real f -> Printf.sprintf "%h" f
  | Db.V_blob b -> Bytes.to_string b
  | Db.V_null -> "NULL"
;;

(* Rows are compared unsorted where order is the point (ORDER BY cases) and
   sorted otherwise; [rows_of] preserves order and the callers sort. *)
let rows_of db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun r -> Array.to_list (Array.map render r))
      (run (Lwt_stream.to_list stream))
;;

let check_rows ~label expected actual =
  Alcotest.(check (list (list string))) label expected actual
;;

let check_bag ~label expected actual =
  Alcotest.(check (list (list string)))
    label
    (List.sort compare expected)
    (List.sort compare actual)
;;

(* A refusal surfaces either as a [Db.error] or as an exception, and either
   while the plan is built or while the stream is pulled.  All four mean the
   same thing here: the query did not answer. *)
let err_of db sql =
  try
    match run (Db.query db sql) with
    | Error e -> Format.asprintf "%a" Db.pp_error e
    | Ok stream ->
      ignore (run (Lwt_stream.to_list stream) : Row.t list);
      ""
  with
  | Failure m -> m
  | e -> Printexc.to_string e
;;

let contains msg needle =
  let n = String.length needle
  and m = String.length msg in
  let rec go i = i + n <= m && (String.sub msg i n = needle || go (i + 1)) in
  go 0
;;

let refused db sql ~label =
  let msg = err_of db sql in
  Alcotest.(check bool)
    (Printf.sprintf "%s: refused rather than answered (got %S)" label msg)
    true
    (msg <> "");
  msg
;;

let seed db =
  exec db "CREATE TABLE a (x INTEGER, y INTEGER)";
  exec db "CREATE TABLE b (y INTEGER, z INTEGER)";
  exec db "INSERT INTO a VALUES (1, 10), (2, 20), (3, 30)";
  exec db "INSERT INTO b VALUES (10, 100), (20, 200)"
;;

(* ------------------------------------------------------------------ *)
(* Gap 1 — comma-separated FROM lists                                   *)
(* ------------------------------------------------------------------ *)

(* The comma desugars to an INNER join with the trivially-true ON predicate.
   Asserting the AST here is what pins the representation the planner's
   recogniser depends on. *)
let a_comma_is_an_inner_join_on_the_true_literal () =
  match parse "SELECT * FROM a, b" with
  | Ast.S_select { table = "a"; table_alias = None; joins = [ j ]; _ } ->
    Alcotest.(check bool) "inner" true (j.Ast.kind = Ast.Inner);
    Alcotest.(check string) "right table" "b" j.Ast.table;
    Alcotest.(check bool) "no alias" true (j.Ast.alias = None);
    Alcotest.(check bool) "ON 1" true (j.Ast.on = Ast.E_lit (Ast.L_int 1L))
  | _ -> Alcotest.fail "unexpected AST"
;;

(* [FROM a JOIN b ON ..., c JOIN d ON ...] flattens in source order, so the
   column offsets stay a, b, c, d. *)
let commas_and_explicit_joins_flatten_in_source_order () =
  match parse "SELECT * FROM a JOIN b ON a.y = b.y, c JOIN d ON c.k = d.k" with
  | Ast.S_select { table = "a"; joins = [ j1; j2; j3 ]; _ } ->
    Alcotest.(check string) "1st" "b" j1.Ast.table;
    Alcotest.(check string) "2nd" "c" j2.Ast.table;
    Alcotest.(check string) "3rd" "d" j3.Ast.table;
    Alcotest.(check bool)
      "the comma is the trivially-true one"
      true
      (j2.Ast.on = Ast.E_lit (Ast.L_int 1L))
  | _ -> Alcotest.fail "unexpected AST"
;;

let an_implicit_join_answers_what_the_explicit_one_does () =
  with_db (fun db ->
    seed db;
    (* sqlite3: 1|10|10|100 and 2|20|20|200 *)
    let expected = [ [ "1"; "10"; "10"; "100" ]; [ "2"; "20"; "20"; "200" ] ] in
    check_bag ~label:"implicit" expected (rows_of db "SELECT * FROM a, b WHERE a.y = b.y");
    check_bag
      ~label:"explicit"
      expected
      (rows_of db "SELECT * FROM a JOIN b ON a.y = b.y"))
;;

let a_three_way_implicit_join () =
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE c (z INTEGER, w INTEGER)";
    exec db "INSERT INTO c VALUES (100, 1000), (200, 2000)";
    (* sqlite3: 1|10|10|100|100|1000 and 2|20|20|200|200|2000 *)
    check_bag
      ~label:"three-way"
      [ [ "1"; "10"; "10"; "100"; "100"; "1000" ]
      ; [ "2"; "20"; "20"; "200"; "200"; "2000" ]
      ]
      (rows_of db "SELECT * FROM a, b, c WHERE a.y = b.y AND b.z = c.z"))
;;

let a_comma_with_no_where_is_the_cartesian_product () =
  with_db (fun db ->
    seed db;
    (* sqlite3: 3 x 2 = 6 rows. *)
    Alcotest.(check int) "6 pairs" 6 (List.length (rows_of db "SELECT * FROM a, b")))
;;

(* CROSS JOIN falls out of the same production: it is the comma spelled out,
   and takes no ON clause. *)
let cross_join_is_the_comma_spelled_out () =
  with_db (fun db ->
    seed db;
    check_bag
      ~label:"CROSS JOIN = comma"
      (rows_of db "SELECT * FROM a, b")
      (rows_of db "SELECT * FROM a CROSS JOIN b");
    check_bag
      ~label:"CROSS JOIN takes a WHERE"
      [ [ "1"; "10"; "10"; "100" ]; [ "2"; "20"; "20"; "200" ] ]
      (rows_of db "SELECT * FROM a CROSS JOIN b WHERE a.y = b.y"))
;;

(* sqlite3 reserves CROSS in bare-alias position ("near \";\": syntax error"
   for [SELECT * FROM a cross;]) but accepts it as a quoted/AS-introduced
   name.  Granary now matches on both counts. *)
let cross_is_still_usable_as_a_name () =
  with_db (fun db ->
    exec db "CREATE TABLE cross (n INTEGER)";
    exec db "INSERT INTO cross VALUES (7)";
    check_rows ~label:"table named cross" [ [ "7" ] ] (rows_of db "SELECT n FROM cross");
    check_rows
      ~label:"AS cross"
      [ [ "7" ] ]
      (rows_of db "SELECT q.n FROM cross AS q, cross AS cross WHERE q.n = cross.n"))
;;

(* ------------------------------------------------------------------ *)
(* Gap 1 — the planner does not build the cartesian product            *)
(* ------------------------------------------------------------------ *)

let col name ty : Row.column =
  { Row.name
  ; ty
  ; not_null = false
  ; primary_key = false
  ; pk_desc = false
  ; default = None
  ; check_sql = None
  ; generated_as = None
  }
;;

let join_cat () =
  run
    (let store = S.create () in
     let* cat = Cat.open_ store in
     let* _ =
       Cat.create_table
         cat
         ~name:"a"
         ~columns:[ col "x" Row.Integer; col "y" Row.Integer ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* _ =
       Cat.create_table
         cat
         ~name:"b"
         ~columns:[ col "y" Row.Integer; col "z" Row.Integer ]
         ~without_rowid:false
         ~autoincrement:false
     in
     Lwt.return cat)
;;

let plan_of cat sql = Planner.plan ~cat (Result.get_ok (run (Sema.bind cat (parse sql))))

(* The load-bearing assertion for gap 1: an implicit join whose restriction is
   in WHERE borrows that equality as its join key, so it is the SAME plan the
   explicit spelling gets.  [left_key = -1] would be the cartesian product the
   issue warns about. *)
let an_implicit_join_borrows_its_key_from_where () =
  let cat = join_cat () in
  let keys sql =
    let rec go : Plan.op -> (int * int) option = function
      | Plan.Op_hash_join { left_key; right_key; _ } -> Some (left_key, right_key)
      | Plan.Op_project { child; _ }
      | Plan.Op_filter { child; _ }
      | Plan.Op_sort { child; _ }
      | Plan.Op_limit { child; _ } -> go child
      | _ -> None
    in
    go (plan_of cat sql)
  in
  let explicit = keys "SELECT * FROM a JOIN b ON a.y = b.y" in
  let implicit = keys "SELECT * FROM a, b WHERE a.y = b.y" in
  Alcotest.(check bool) "the explicit join is keyed" true (explicit = Some (1, 0));
  Alcotest.(check bool)
    (Printf.sprintf
       "the implicit join is keyed the same way (got %s)"
       (match implicit with
        | None -> "no hash join"
        | Some (l, r) -> Printf.sprintf "(%d, %d)" l r))
    true
    (implicit = explicit)
;;

(* The complement: with nothing in WHERE to borrow, the join stays the
   cartesian product it genuinely is — and carries no [Op_filter] evaluating
   the constant [1] once per row. *)
let a_keyless_implicit_join_stays_cartesian_and_unfiltered () =
  let cat = join_cat () in
  match plan_of cat "SELECT * FROM a, b" with
  | Plan.Op_project { child = Plan.Op_hash_join { left_key = -1; right_key = -1; _ }; _ }
    -> ()
  | _ -> Alcotest.fail "expected an unfiltered cartesian Op_hash_join"
;;

(* A WHERE equality that does NOT span the two sides is not a join key. *)
let a_single_sided_where_is_not_a_join_key () =
  let cat = join_cat () in
  match plan_of cat "SELECT * FROM a, b WHERE a.x = a.y" with
  | Plan.Op_project
      { child = Plan.Op_filter { child = Plan.Op_hash_join { left_key = -1; _ }; _ }; _ }
    -> ()
  | _ -> Alcotest.fail "expected a cartesian Op_hash_join under the WHERE filter"
;;

(* ------------------------------------------------------------------ *)
(* Gap 2 — derived tables                                               *)
(* ------------------------------------------------------------------ *)

(* The alias becomes the CTE's NAME and the FROM item carries no alias.  That
   is the whole of the #635 scope story for a derived table: there is no
   underlying name for the alias to replace, so nothing downstream needs a new
   case. *)
let a_derived_table_desugars_into_a_cte () =
  match parse "SELECT * FROM (SELECT x FROM a) AS t" with
  | Ast.S_with_cte { name = "t"; recursive = false; def; query } ->
    (match def with
     | Ast.S_select { table = "a"; _ } -> ()
     | _ -> Alcotest.fail "unexpected CTE def");
    (match query with
     | Ast.S_select { table = "t"; table_alias = None; joins = []; _ } -> ()
     | _ -> Alcotest.fail "unexpected CTE query")
  | _ -> Alcotest.fail "unexpected AST"
;;

let an_unaliased_derived_table_is_named_after_its_offset () =
  match parse "SELECT * FROM (SELECT x FROM a)" with
  | Ast.S_with_cte { name; _ } ->
    Alcotest.(check bool)
      (Printf.sprintf "generated name (got %S)" name)
      true
      (String.length name > 10 && String.sub name 0 10 = "__derived_")
  | _ -> Alcotest.fail "unexpected AST"
;;

let a_derived_table_answers () =
  with_db (fun db ->
    seed db;
    (* sqlite3: 1, 2, 3 for all three spellings. *)
    let expected = [ [ "1" ]; [ "2" ]; [ "3" ] ] in
    check_bag
      ~label:"aliased"
      expected
      (rows_of db "SELECT * FROM (SELECT x FROM a) AS t");
    check_bag
      ~label:"qualified"
      expected
      (rows_of db "SELECT t.x FROM (SELECT x FROM a) AS t");
    check_bag ~label:"unaliased" expected (rows_of db "SELECT * FROM (SELECT x FROM a)");
    (* sqlite3: 2 *)
    check_rows
      ~label:"outer WHERE over a derived table"
      [ [ "2" ] ]
      (rows_of db "SELECT t.x FROM (SELECT x FROM a) AS t WHERE t.x = 2");
    (* sqlite3: 3 *)
    check_rows
      ~label:"COUNT( * ) over a derived table"
      [ [ "3" ] ]
      (rows_of db "SELECT COUNT(*) FROM (SELECT x FROM a) t"))
;;

let a_derived_table_on_a_joins_right_side () =
  with_db (fun db ->
    seed db;
    (* sqlite3: 1|10|10|100 and 2|20|20|200 *)
    let expected = [ [ "1"; "10"; "10"; "100" ]; [ "2"; "20"; "20"; "200" ] ] in
    check_bag
      ~label:"JOIN (derived)"
      expected
      (rows_of db "SELECT * FROM a JOIN (SELECT y AS yy, z FROM b) d ON a.y = d.yy");
    check_bag
      ~label:", (derived)"
      expected
      (rows_of db "SELECT * FROM a, (SELECT y AS yy, z FROM b) d WHERE a.y = d.yy"))
;;

let several_derived_tables_in_one_from () =
  with_db (fun db ->
    seed db;
    (* sqlite3: 1|5|6, 2|5|6, 3|5|6 *)
    check_bag
      ~label:"three FROM items, two derived"
      [ [ "1"; "5"; "6" ]; [ "2"; "5"; "6" ]; [ "3"; "5"; "6" ] ]
      (rows_of db "SELECT a.x, f.f, g.g FROM a, (SELECT 5 AS f) f, (SELECT 6 AS g) g"))
;;

let a_derived_table_may_be_a_compound_or_carry_an_aggregate () =
  with_db (fun db ->
    seed db;
    (* sqlite3: 1, 2, 3 (10, 20 already present) *)
    check_bag
      ~label:"UNION inside a derived table"
      [ [ "1" ]; [ "2" ]; [ "3" ] ]
      (rows_of db "SELECT * FROM (SELECT x FROM a UNION SELECT x FROM a) u");
    (* sqlite3: 60 *)
    check_rows
      ~label:"aggregate over a derived table"
      [ [ "60" ] ]
      (rows_of db "SELECT SUM(t.y) FROM (SELECT x, y FROM a) t"))
;;

(* [wrap_derived_ctes] puts the CTE wrapper OUTSIDE the SELECT, so the
   compound's trailing ORDER BY is no longer at the root of the right arm.
   [lift_compound_tail] recurses through it; without that the sort would apply
   to the right arm alone. *)
let a_compound_orders_over_both_arms () =
  with_db (fun db ->
    seed db;
    (* sqlite3: 30, 20, 10, 3, 2, 1 *)
    check_rows
      ~label:"ORDER BY DESC over the compound"
      [ [ "30" ]; [ "20" ]; [ "10" ]; [ "3" ]; [ "2" ]; [ "1" ] ]
      (rows_of
         db
         "SELECT x FROM a UNION SELECT y FROM (SELECT y FROM a) t ORDER BY x DESC"))
;;

(* [Ast.rename_view_columns] grew an [S_with_cte] arm for this: the view body
   is now a WITH wrapper, and before #486 it fell to the catch-all
   "unsupported view body". *)
let a_view_may_be_defined_over_a_derived_table () =
  with_db (fun db ->
    seed db;
    exec db "CREATE VIEW v AS SELECT * FROM (SELECT x, y FROM a) t";
    check_bag
      ~label:"plain view"
      [ [ "1"; "10" ]; [ "2"; "20" ]; [ "3"; "30" ] ]
      (rows_of db "SELECT * FROM v");
    exec db "CREATE VIEW w (p, q) AS SELECT x, y FROM (SELECT x, y FROM a) t";
    check_bag
      ~label:"view with a column list"
      [ [ "1" ]; [ "2" ]; [ "3" ] ]
      (rows_of db "SELECT p FROM w"))
;;

(* ------------------------------------------------------------------ *)
(* Scope — the #635 rule over the new FROM items                        *)
(* ------------------------------------------------------------------ *)

(* The duplicate guard in [Exec.get_outer_scan_metas] is by IDENTIFIER, not by
   table name, so an aliased self-join in an implicit FROM list resolves while
   the unaliased one is still refused.  Both halves matter: the aliased one is
   the answer #635 unlocked, and the unaliased one is the refusal that must not
   silently become a wrong answer. *)
let the_duplicate_guard_is_by_identifier () =
  with_db (fun db ->
    seed db;
    (* sqlite3: 1, 2 — a.y = 30 has no match in b. *)
    check_bag
      ~label:"FROM a AS p, a AS q resolves"
      [ [ "1" ]; [ "2" ] ]
      (rows_of
         db
         "SELECT p.x FROM a AS p, a AS q WHERE p.x = q.x AND EXISTS (SELECT 1 FROM b \
          WHERE b.y = p.y)");
    (* sqlite3 refuses the unaliased self-join outright:
       "ambiguous column name: main.a.x". *)
    ignore
      (refused
         db
         "SELECT x FROM a, a WHERE EXISTS (SELECT 1 FROM b WHERE b.y = a.y)"
         ~label:"unaliased self-join"
       : string))
;;

let a_correlated_subquery_over_an_implicit_join_resolves () =
  with_db (fun db ->
    seed db;
    (* sqlite3: 1|10|10|100 and 2|20|20|200 *)
    check_bag
      ~label:"EXISTS correlated to both sides of an implicit join"
      [ [ "1"; "10"; "10"; "100" ]; [ "2"; "20"; "20"; "200" ] ]
      (rows_of
         db
         "SELECT * FROM a, b WHERE a.y = b.y AND EXISTS (SELECT 1 FROM b AS b2 WHERE \
          b2.y = a.y AND b2.z = b.z)"))
;;

let a_correlated_subquery_inside_a_derived_table_resolves () =
  with_db (fun db ->
    seed db;
    (* sqlite3: 1, 2 — the correlation is entirely inside the derived table. *)
    check_bag
      ~label:"correlation contained in the derived table"
      [ [ "1" ]; [ "2" ] ]
      (rows_of
         db
         "SELECT t.x FROM (SELECT x, y FROM a WHERE EXISTS (SELECT 1 FROM b WHERE b.y = \
          a.y)) t"))
;;

(* A derived table cannot see the FROM items beside it: that is LATERAL, which
   neither engine implements.  sqlite3 answers "no such column: a.y"; granary
   refuses too, with "unknown table". *)
let a_derived_table_is_not_lateral () =
  with_db (fun db ->
    seed db;
    ignore
      (refused
         db
         "SELECT * FROM a, (SELECT * FROM b WHERE b.y = a.y) d"
         ~label:"LATERAL correlation"
       : string))
;;

(* ACCEPTED LIMITATION, pinned so it is found deliberately rather than by
   surprise: a subquery correlated to a DERIVED TABLE in the enclosing FROM is
   refused.  This is not new and is not about derived tables — a plain
   [WITH c AS (...) SELECT ... WHERE EXISTS (... c.y ...)] is refused
   identically on the tree before #486.  The cause is that
   [Exec.get_outer_scan_metas] resolves an outer input from the PLAN, and a CTE
   scan is materialized into [Op_pragma_rows] before the filter runs — an op
   that carries no [Cat.table_meta] and so no scope identifier.  The refusal is
   loud, which is what keeps it a limitation rather than a wrong answer.  If
   [Op_pragma_rows] ever learns its source, this test should start passing as
   an answer and must be rewritten, not deleted. *)
let a_subquery_correlated_to_a_derived_table_is_refused () =
  with_db (fun db ->
    seed db;
    let derived =
      refused
        db
        "SELECT t.x FROM (SELECT x, y FROM a) t WHERE EXISTS (SELECT 1 FROM b WHERE b.y \
         = t.y)"
        ~label:"correlated to a derived table"
    in
    let cte =
      refused
        db
        "WITH c AS (SELECT x, y FROM a) SELECT c.x FROM c WHERE EXISTS (SELECT 1 FROM b \
         WHERE b.y = c.y)"
        ~label:"correlated to a plain CTE"
    in
    Alcotest.(check string) "the same refusal as a plain CTE" cte derived)
;;

(* A REACTIVE view over a derived table is REFUSED, and the refusal is the
   point.  [Reactive_view.base_tables_of] reads the base tables off the
   [S_select] at the root of the body; the desugaring puts an [S_with_cte]
   there, so it would answer [] and the view would be registered with nothing
   to invalidate it -- a snapshot frozen at creation, served forever with no
   error.  A plain CREATE VIEW is unaffected: its body is re-bound on every
   use, which is what
   [a_view_may_be_defined_over_a_derived_table] above pins. *)
let a_reactive_view_over_a_derived_table_is_refused () =
  with_db (fun db ->
    seed db;
    let msg =
      refused
        db
        "CREATE REACTIVE VIEW rv AS SELECT * FROM (SELECT x FROM a) d"
        ~label:"reactive view over a derived table"
    in
    Alcotest.(check bool)
      (Printf.sprintf "the refusal names the issue (got %S)" msg)
      true
      (contains msg "#486");
    (* The control: the same view over a plain table is still accepted. *)
    exec db "CREATE REACTIVE VIEW rv2 AS SELECT x FROM a")
;;

(* ------------------------------------------------------------------ *)
(* Runner                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "from_list_derived_486"
    [ ( "gap 1 — comma-separated FROM lists"
      , [ Alcotest.test_case
            "a comma is an INNER join on the true literal"
            `Quick
            a_comma_is_an_inner_join_on_the_true_literal
        ; Alcotest.test_case
            "commas and explicit joins flatten in source order"
            `Quick
            commas_and_explicit_joins_flatten_in_source_order
        ; Alcotest.test_case
            "an implicit join answers what the explicit one does"
            `Quick
            an_implicit_join_answers_what_the_explicit_one_does
        ; Alcotest.test_case "a three-way implicit join" `Quick a_three_way_implicit_join
        ; Alcotest.test_case
            "a comma with no WHERE is the cartesian product"
            `Quick
            a_comma_with_no_where_is_the_cartesian_product
        ; Alcotest.test_case
            "CROSS JOIN is the comma spelled out"
            `Quick
            cross_join_is_the_comma_spelled_out
        ; Alcotest.test_case
            "cross is still usable as a name"
            `Quick
            cross_is_still_usable_as_a_name
        ] )
    ; ( "gap 1 — the planner does not build the product"
      , [ Alcotest.test_case
            "an implicit join borrows its key from WHERE"
            `Quick
            an_implicit_join_borrows_its_key_from_where
        ; Alcotest.test_case
            "a keyless implicit join stays cartesian and unfiltered"
            `Quick
            a_keyless_implicit_join_stays_cartesian_and_unfiltered
        ; Alcotest.test_case
            "a single-sided WHERE is not a join key"
            `Quick
            a_single_sided_where_is_not_a_join_key
        ] )
    ; ( "gap 2 — derived tables"
      , [ Alcotest.test_case
            "a derived table desugars into a CTE"
            `Quick
            a_derived_table_desugars_into_a_cte
        ; Alcotest.test_case
            "an unaliased derived table is named after its offset"
            `Quick
            an_unaliased_derived_table_is_named_after_its_offset
        ; Alcotest.test_case "a derived table answers" `Quick a_derived_table_answers
        ; Alcotest.test_case
            "a derived table on a join's right side"
            `Quick
            a_derived_table_on_a_joins_right_side
        ; Alcotest.test_case
            "several derived tables in one FROM"
            `Quick
            several_derived_tables_in_one_from
        ; Alcotest.test_case
            "a derived table may be a compound or carry an aggregate"
            `Quick
            a_derived_table_may_be_a_compound_or_carry_an_aggregate
        ; Alcotest.test_case
            "a compound orders over both arms"
            `Quick
            a_compound_orders_over_both_arms
        ; Alcotest.test_case
            "a view may be defined over a derived table"
            `Quick
            a_view_may_be_defined_over_a_derived_table
        ] )
    ; ( "scope — the #635 rule over the new FROM items"
      , [ Alcotest.test_case
            "the duplicate guard is by identifier"
            `Quick
            the_duplicate_guard_is_by_identifier
        ; Alcotest.test_case
            "a correlated subquery over an implicit join resolves"
            `Quick
            a_correlated_subquery_over_an_implicit_join_resolves
        ; Alcotest.test_case
            "a correlated subquery inside a derived table resolves"
            `Quick
            a_correlated_subquery_inside_a_derived_table_resolves
        ; Alcotest.test_case
            "a derived table is not LATERAL"
            `Quick
            a_derived_table_is_not_lateral
        ; Alcotest.test_case
            "a subquery correlated to a derived table is refused"
            `Quick
            a_subquery_correlated_to_a_derived_table_is_refused
        ; Alcotest.test_case
            "a reactive view over a derived table is refused"
            `Quick
            a_reactive_view_over_a_derived_table_is_refused
        ] )
    ]
;;
