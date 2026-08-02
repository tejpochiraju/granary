(** #552 / #539 / #551 / #565 / #566: join-planning defects that produce wrong
    answers.

    {1 #552 and #539 — a LEFT JOIN with a general ON loses its null-extended rows}

    When the ON predicate is not a simple [col = col], [plan_join] built a
    cartesian [Op_hash_join] ([left_key = -1]) and wrapped it in an
    [Op_filter] carrying the ON predicate. In the cartesian arm of
    [stream_hash_join] every right row counts as a match, so the
    [`Left when not !any] null-extension fired only when the build side was
    {i entirely} empty; every left row was paired with every right row and the
    ON predicate was then applied {i above} the join.

    A left row matching no right row therefore produced nothing at all instead
    of one null-extended row. Filtering above the join is equivalent to
    filtering inside it only for an INNER join — for an outer join the ON
    predicate {b is} the match test, and a row that fails it must still appear,
    null-extended.

    Both spellings in the issues are pinned here, with SQLite as the oracle
    (checked by hand against sqlite3 3.45.1; these are ordinary SQL-92
    semantics, not an implementation quirk).

    {1 #551 — a CTE shadowing a real table takes the probe against the real tree}

    [best_probe] resolved the right table's indexes by {i name}
    ([Cat.indexes_for_table cat ~table:right_meta.name]) with no [tree_id]
    guard, while [Cat.register_ephemeral] replaces only the {i table} entry and
    leaves [indexes_by_table] alone. A CTE shadowing a real table therefore got
    that table's indexes back, [best_probe] answered [Some], and [plan_join]
    emitted an [Op_nested_loop_join] probing a B-tree the CTE has nothing to do
    with — silently zero rows.

    #531 added this guard on the {i build} side. The probe side needs a driving
    row count {b below} [nlj_min_driving_rows] to be reached at all: above the
    floor the cost model lands on the hash join, which is why #531's
    [cte_shadowing_a_real_table_still_scans] (1,200 driving rows) does not
    catch it. Every case here seeds 10.

    {1 #565 — the same defect on the BASE SCAN, with no join at all}

    #551 and #531 guarded two of the three sites that reach the catalog by name.
    The third is [access_path_for_eqs], which [plan_base], [choose_access_path]
    and [plan_dml_seek] all pass through: a CTE shadowing a real table, with a
    WHERE equality on a leading prefix of that table's index, planned an
    [Op_index_lookup] whose [table_tree] was the CTE's sentinel [-1] and returned
    nothing. #565 moved the guard into that one chokepoint and deleted
    [build_side]'s inline copy — two copies of one guard is what let #551 exist
    unnoticed.

    {1 #566 — a correlated subquery in an outer join's ON predicate}

    [pre_eval_subquery] resolves only the uncorrelated case. A correlated
    [P_subquery] survives into [eval_expr], which answers [Row.V_null] for it, so
    an outer join's ON predicate is false for every pair and every left row
    null-extends. #566 chose to refuse the query rather than return that. *)

module Db = Granary.Db
module Cat = Granary_catalog.Catalog
module Store = Granary_store.Store
module Row = Granary_encoding.Row
module Sema = Granary_sql.Sema
module Parser = Granary_sql.Parser
module Lexer = Granary_sql.Lexer
module Planner = Granary_sql.Planner
module Exec = Granary_sql.Exec
module Plan = Granary_sql.Plan

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
  | Db.V_int n -> Int64.to_string n
  | Db.V_text s -> s
  | Db.V_real f -> Printf.sprintf "%h" f
  | Db.V_blob b -> Bytes.to_string b
  | Db.V_null -> "NULL"
;;

(* Sorted, so the assertions do not depend on join order. *)
let rows_of db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.sort
      compare
      (List.map
         (fun r -> Array.to_list (Array.map render r))
         (run (Lwt_stream.to_list stream)))
;;

let check_rows ~label expected actual =
  Alcotest.(check (list (list string))) label (List.sort compare expected) actual
;;

(* ------------------------------------------------------------------ *)
(* #552 / #539: null extension under a general ON                       *)
(* ------------------------------------------------------------------ *)

(* The #539 repro verbatim. The equi spelling ([ON b = a]) was always right —
   it reaches the keyed hash join, which null-extends properly. Only the
   general-ON fallback was broken, so the equi form is kept alongside as the
   control. *)
let non_equi_on_null_extends () =
  with_db (fun db ->
    exec db "CREATE TABLE l (a INTEGER)";
    exec db "CREATE TABLE r (b INTEGER)";
    exec db "INSERT INTO l VALUES (1)";
    exec db "INSERT INTO l VALUES (9)";
    exec db "INSERT INTO r VALUES (5)";
    check_rows
      ~label:"LEFT JOIN ON b > a: the unmatched left row survives with NULLs"
      [ [ "1"; "5" ]; [ "9"; "NULL" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b > a");
    check_rows
      ~label:"the equi spelling was never affected"
      [ [ "1"; "NULL" ]; [ "9"; "NULL" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b = a");
    check_rows
      ~label:"INNER keeps only the pair"
      [ [ "1"; "5" ] ]
      (rows_of db "SELECT a, b FROM l INNER JOIN r ON b > a"))
;;

(* The #552 repro verbatim: [si IS NULL] is true of no stock row, so every left
   row null-extends. This is the case an ON filter above the join can never
   produce, because it is false of every cartesian pair AND the filter also sees
   (and rejects) the null-extended row it would have to let through. *)
let is_null_on_null_extends_every_left_row () =
  with_db (fun db ->
    exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
    exec
      db
      "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
    exec db "INSERT INTO line VALUES (1,1,1),(1,2,2),(1,3,3)";
    exec db "INSERT INTO stock VALUES (1,1,11)";
    check_rows
      ~label:"ON si IS NULL null-extends all three left rows"
      [ [ "1"; "NULL" ]; [ "2"; "NULL" ]; [ "3"; "NULL" ] ]
      (rows_of db "SELECT o, qty FROM line LEFT JOIN stock ON si IS NULL");
    check_rows
      ~label:"ON si > i_id: no stock row exceeds any i_id here either"
      [ [ "1"; "NULL" ]; [ "2"; "NULL" ]; [ "3"; "NULL" ] ]
      (rows_of db "SELECT o, qty FROM line LEFT JOIN stock ON si > i_id");
    check_rows
      ~label:"INNER with the same general ON stays empty"
      []
      (rows_of db "SELECT o, qty FROM line INNER JOIN stock ON si IS NULL"))
;;

(* A population where the same general ON both matches and does not, so the two
   halves of the fix are exercised by one query: a left row must keep all of its
   matches, and must null-extend only when it has none. *)
let mixed_population_keeps_matches_and_null_extends () =
  with_db (fun db ->
    exec db "CREATE TABLE l (a INTEGER)";
    exec db "CREATE TABLE r (b INTEGER)";
    exec db "INSERT INTO l VALUES (1),(4),(9)";
    exec db "INSERT INTO r VALUES (2),(5),(7)";
    (* a=1 matches 2,5,7; a=4 matches 5,7; a=9 matches nothing. *)
    check_rows
      ~label:"every match kept, unmatched left row null-extended exactly once"
      [ [ "1"; "2" ]
      ; [ "1"; "5" ]
      ; [ "1"; "7" ]
      ; [ "4"; "5" ]
      ; [ "4"; "7" ]
      ; [ "9"; "NULL" ]
      ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b > a");
    (* The INNER answer is the same minus the null-extended row — the property
       that made filtering above the join look correct. *)
    check_rows
      ~label:"INNER is the same rows minus the null extension"
      [ [ "1"; "2" ]; [ "1"; "5" ]; [ "1"; "7" ]; [ "4"; "5" ]; [ "4"; "7" ] ]
      (rows_of db "SELECT a, b FROM l INNER JOIN r ON b > a"))
;;

(* An empty right table is the one case the join operator itself got right —
   [any] stayed false for every left row, so it did null-extend — and the rows
   were then thrown away by the ON filter sitting above it, because [b > a] is
   NULL on a null-extended row. It is both halves of the defect in one query. *)
let empty_right_table_still_null_extends () =
  with_db (fun db ->
    exec db "CREATE TABLE l (a INTEGER)";
    exec db "CREATE TABLE r (b INTEGER)";
    exec db "INSERT INTO l VALUES (1),(2)";
    check_rows
      ~label:"empty right table null-extends every left row"
      [ [ "1"; "NULL" ]; [ "2"; "NULL" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b > a"))
;;

(* A general ON that is a constant, so the planner's [recognise_eq_col_col]
   returns [None] on a predicate mentioning no column at all. [1 = 0] is the
   degenerate "no pair ever matches" shape. *)
let constant_false_on_null_extends () =
  with_db (fun db ->
    exec db "CREATE TABLE l (a INTEGER)";
    exec db "CREATE TABLE r (b INTEGER)";
    exec db "INSERT INTO l VALUES (1),(2)";
    exec db "INSERT INTO r VALUES (7)";
    check_rows
      ~label:"ON 1 = 0 null-extends every left row"
      [ [ "1"; "NULL" ]; [ "2"; "NULL" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON 1 = 0");
    check_rows
      ~label:"ON 1 = 1 is the full cartesian product"
      [ [ "1"; "7" ]; [ "2"; "7" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON 1 = 1"))
;;

(* The no-catalog planner path ([chain_joins_no_cat]) has the same shape and had
   the same defect. Nothing in [Db] reaches it — every caller passes [~cat] —
   so it is driven here the only way it can be: plan without [~cat], then run
   the resulting op against a real store.

   The plan is built from the parsed SQL rather than a hand-written AST so the
   two paths are compared on exactly the same query. *)
let no_cat_general_on_null_extends () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let store = Store.create () in
     let* cat = Cat.open_ store in
     let parse sql = Parser.stmt_eof Lexer.token (Lexing.from_string sql) in
     let bind sql =
       let* b = Sema.bind cat (parse sql) in
       match b with
       | Ok b -> Lwt.return b
       | Error e -> Alcotest.failf "bind %S: %a" sql Sema.pp_error e
     in
     let write sql =
       let* b = bind sql in
       let* _ = Exec.execute store cat (Planner.plan ~cat b) in
       Lwt.return_unit
     in
     let* () = write "CREATE TABLE l (a INTEGER)" in
     let* () = write "CREATE TABLE r (b INTEGER)" in
     let* () = write "INSERT INTO l VALUES (1)" in
     let* () = write "INSERT INTO l VALUES (9)" in
     let* () = write "INSERT INTO r VALUES (5)" in
     let* bound = bind "SELECT a, b FROM l LEFT JOIN r ON b > a" in
     (* No [~cat]: this is the [chain_joins_no_cat] plan. *)
     let* stream = Exec.query store cat (Planner.plan bound) in
     let* rows = Lwt_stream.to_list stream in
     let render_value = function
       | Row.V_int n -> Int64.to_string n
       | Row.V_null -> "NULL"
       | _ -> Alcotest.fail "unexpected value in the no-catalog join"
     in
     check_rows
       ~label:"the no-catalog join chain null-extends too"
       [ [ "1"; "5" ]; [ "9"; "NULL" ] ]
       (List.sort
          compare
          (List.map (fun r -> Array.to_list (Array.map render_value r)) rows));
     Lwt.return_unit)
;;

(* WHERE runs above the join, so a conjunct on the right table drops the
   null-extended rows again — that is correct SQL, and the reason #528's
   narrowing test cannot observe null extension. Pinned so the fix is not
   mistaken for "LEFT JOIN rows always survive". *)
let where_on_the_right_table_still_drops_null_extended_rows () =
  with_db (fun db ->
    exec db "CREATE TABLE l (a INTEGER)";
    exec db "CREATE TABLE r (b INTEGER)";
    exec db "INSERT INTO l VALUES (1),(9)";
    exec db "INSERT INTO r VALUES (5)";
    check_rows
      ~label:"WHERE b > 0 drops the null-extended row"
      [ [ "1"; "5" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b > a WHERE b > 0");
    check_rows
      ~label:"WHERE b IS NULL keeps only the null-extended row"
      [ [ "9"; "NULL" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b > a WHERE b IS NULL"))
;;

(* A general ON on the SECOND join of a chain: the ON predicate's ordinals
   address the combined row, so evaluating it inside the join must use the
   left-plus-right row and not the right row alone. *)
let general_on_as_the_second_join_in_a_chain () =
  with_db (fun db ->
    exec db "CREATE TABLE l (a INTEGER)";
    exec db "CREATE TABLE m (c INTEGER, d INTEGER)";
    exec db "CREATE TABLE r (b INTEGER)";
    exec db "INSERT INTO l VALUES (1),(9)";
    exec db "INSERT INTO m VALUES (1, 10)";
    exec db "INSERT INTO m VALUES (9, 90)";
    exec db "INSERT INTO r VALUES (50)";
    check_rows
      ~label:"chained general ON null-extends the row with no match"
      [ [ "1"; "10"; "50" ]; [ "9"; "90"; "NULL" ] ]
      (rows_of db "SELECT a, d, b FROM l JOIN m ON c = a LEFT JOIN r ON b > d"))
;;

(* Moving the ON predicate inside the join removed the [Op_filter] node that
   used to represent it in EXPLAIN, so the plan text has to name it instead —
   otherwise the predicate is invisible and the INNER and LEFT spellings of one
   query explain differently for no visible reason. *)
let explain_names_the_general_on () =
  with_db (fun db ->
    exec db "CREATE TABLE l (a INTEGER)";
    exec db "CREATE TABLE r (b INTEGER)";
    let plan sql = String.concat "\n" (List.map (String.concat " ") (rows_of db sql)) in
    let left = plan "EXPLAIN SELECT a, b FROM l LEFT JOIN r ON b > a" in
    let inner = plan "EXPLAIN SELECT a, b FROM l INNER JOIN r ON b > a" in
    let equi = plan "EXPLAIN SELECT a, b FROM l LEFT JOIN r ON b = a" in
    let contains needle s =
      let n = String.length needle in
      let rec go i =
        i + n <= String.length s && (String.sub s i n = needle || go (i + 1))
      in
      go 0
    in
    Alcotest.(check bool)
      "the general-ON LEFT join names its ON predicate"
      true
      (contains "LeftHashJoin(ON)" left);
    Alcotest.(check bool)
      "the INNER spelling keeps its Filter node instead"
      true
      (contains "HashJoin" inner && contains "Filter" inner);
    Alcotest.(check bool)
      "a keyed LEFT join carries no ON predicate to name"
      true
      (contains "LeftHashJoin" equi && not (contains "LeftHashJoin(ON)" equi)))
;;

(* [on_pred] is read only by the cartesian arm, so a keyed join carrying one
   would drop its match test and answer wrongly.  [Op_hash_join] is public, so
   the invariant is enforced rather than merely documented. *)
let keyed_join_rejects_an_on_pred () =
  let store = Store.create () in
  let cat = run (Cat.open_ store) in
  let parse sql = Parser.stmt_eof Lexer.token (Lexing.from_string sql) in
  let bound =
    match run (Sema.bind cat (parse "CREATE TABLE t (x INTEGER)")) with
    | Ok b -> b
    | Error e -> Alcotest.failf "bind: %a" Sema.pp_error e
  in
  ignore (run (Exec.execute store cat (Planner.plan ~cat bound)));
  let meta =
    match run (Cat.find_table cat ~name:"t") with
    | Some m -> m
    | None -> Alcotest.fail "table t was not created"
  in
  let scan = Plan.Op_seq_scan { table_meta = meta } in
  let op =
    Plan.Op_hash_join
      { left = scan
      ; right = scan
      ; left_key = 0
      ; right_key = 0
      ; on_pred = Some (Plan.P_lit (Granary_sql.Ast.L_int 1L))
      ; join_kind = `Left
      ; right_col_offset = 1
      ; n_right_cols = 1
      }
  in
  Alcotest.check_raises
    "a keyed hash join with an on_pred is rejected"
    (Invalid_argument
       "Exec.stream_hash_join: on_pred is only meaningful on the cartesian arm (left_key \
        < 0 || right_key < 0)")
    (fun () -> ignore (run (Exec.query store cat op)))
;;

(* ------------------------------------------------------------------ *)
(* The invariant the cases above enumerate by example                   *)
(* ------------------------------------------------------------------ *)

(* The eight cases above are eight points; this is the rule they are points of,
   stated once and checked against a model rather than against hand-written
   expectations:

     a LEFT JOIN under a general ON emits, for each left row, one joined row per
     right row satisfying the ON — and exactly one null-extended row when there
     are none.

   The model is the definition, evaluated in OCaml over the same populations.
   Written before the fix it fails on any population with an unmatched left row,
   which is most of them; the enumerated cases would each have had to be thought
   of.

   NULLs are generated on both sides because the ON predicates are comparisons
   and three-valued logic is where "no match" and "null extension" meet: a NULL
   on either side makes [b > a] NULL, which is not a match, so a left row with
   [a = NULL] must null-extend under every operator here. *)
let cmp_of_op op (a : int option) (b : int option) =
  match a, b with
  (* NULL on either side: the comparison is NULL, which is not a match. *)
  | None, _ | _, None -> false
  | Some a, Some b ->
    (match op with
     | ">" -> b > a
     | "<" -> b < a
     | ">=" -> b >= a
     | "<=" -> b <= a
     | "<>" -> b <> a
     | _ -> Alcotest.failf "unknown operator %S" op)
;;

let sql_of = function
  | None -> "NULL"
  | Some n -> string_of_int n
;;

(* [SELECT a, b FROM l LEFT JOIN r ON b <op> a], by definition. *)
let model_left_join op ls rs =
  List.concat_map
    (fun a ->
       match List.filter (fun b -> cmp_of_op op a b) rs with
       | [] -> [ [ sql_of a; "NULL" ] ]
       | ms -> List.map (fun b -> [ sql_of a; sql_of b ]) ms)
    ls
;;

(* The same model minus every null extension: the INNER answer, and the property
   that made filtering above the join look correct — it holds, which is exactly
   why it is not sufficient. *)
let model_inner_join op ls rs =
  List.concat_map
    (fun a ->
       List.filter_map
         (fun b -> if cmp_of_op op a b then Some [ sql_of a; sql_of b ] else None)
         rs)
    ls
;;

let ops = [ ">"; "<"; ">="; "<="; "<>" ]

(* A small value domain, so matches and non-matches both occur often. *)
let gen_side = QCheck.(list_size (Gen.int_range 0 4) (option (int_range 0 4)))

let insert_all db tbl col vs =
  List.iter
    (fun v ->
       exec db (Printf.sprintf "INSERT INTO %s (%s) VALUES (%s)" tbl col (sql_of v)))
    vs
;;

let prop_left_join_general_on_matches_the_model =
  QCheck.Test.make
    ~count:80
    ~name:"LEFT JOIN under a general ON agrees with the model on every operator"
    QCheck.(triple gen_side gen_side (int_range 0 (List.length ops - 1)))
    (fun (ls, rs, op_i) ->
       let op = List.nth ops op_i in
       with_db (fun db ->
         exec db "CREATE TABLE l (a INTEGER)";
         exec db "CREATE TABLE r (b INTEGER)";
         insert_all db "l" "a" ls;
         insert_all db "r" "b" rs;
         let answer kind =
           rows_of db (Printf.sprintf "SELECT a, b FROM l %s JOIN r ON b %s a" kind op)
         in
         answer "LEFT" = List.sort compare (model_left_join op ls rs)
         && answer "INNER" = List.sort compare (model_inner_join op ls rs)))
;;

(* ------------------------------------------------------------------ *)
(* #551: the nested-loop probe against a shadowed table                 *)
(* ------------------------------------------------------------------ *)

(* [n_driving] is deliberately far below [nlj_min_driving_rows] (1000), so
   [probe_is_worth_it] returns true unconditionally and the plan really is the
   unguarded [Op_nested_loop_join] this issue is about. With 1,200 rows — the
   count #531's own CTE test uses — the cost model falls to the hash join
   instead and the defect is invisible. *)
let n_driving = 10

let seed_shadowable db =
  exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
  exec db "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
  exec db "BEGIN";
  for o = 1 to n_driving do
    exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, 3)" o)
  done;
  for w = 1 to 2 do
    for si = 1 to 4 do
      exec
        db
        (Printf.sprintf "INSERT INTO stock VALUES (%d, %d, %d)" w si ((w * 100) + si))
    done
  done;
  exec db "COMMIT"
;;

(* The #551 repro. [sw = 1] is the WHERE equality that lets [probe_key_for_index]
   complete a key over [stock]'s [(sw, si)] primary key — without it
   [right_eqs] is empty, no probe key is built, and the query is already
   correct. The [sw + 0 = 1] foil is unrecognisable as an equality and so can
   never take a probe; it is the reference answer. *)
let cte_shadowing_a_real_table_is_not_probed () =
  with_db (fun db ->
    seed_shadowable db;
    let cte = "WITH stock AS (SELECT 1 AS sw, 3 AS si, 999 AS qty) " in
    let probe =
      cte ^ "SELECT qty FROM line JOIN stock ON si = i_id WHERE w = 1 AND sw = 1"
    in
    let foil =
      cte ^ "SELECT qty FROM line JOIN stock ON si = i_id WHERE w = 1 AND sw + 0 = 1"
    in
    let expected = List.init n_driving (fun _ -> [ "999" ]) in
    check_rows
      ~label:"the foil sees the CTE's row once per driving row"
      expected
      (rows_of db foil);
    check_rows
      ~label:"and so does the shape that could take a probe"
      expected
      (rows_of db probe))
;;

(* The same hazard on a LEFT JOIN: a wrong probe would null-extend every driving
   row rather than return nothing, which is a different wrong answer from the
   INNER case and would survive a test that only checked for emptiness. *)
let cte_shadowing_a_real_table_is_not_probed_in_a_left_join () =
  with_db (fun db ->
    seed_shadowable db;
    let cte = "WITH stock AS (SELECT 1 AS sw, 3 AS si, 999 AS qty) " in
    let probe =
      cte ^ "SELECT qty FROM line LEFT JOIN stock ON si = i_id WHERE w = 1 AND sw = 1"
    in
    check_rows
      ~label:"the LEFT spelling finds the CTE row too"
      (List.init n_driving (fun _ -> [ "999" ]))
      (rows_of db probe))
;;

(* A CTE that shadows nothing must be unaffected — the guard keys on the
   synthesized [tree_id], not on the name colliding. *)
let non_shadowing_cte_is_unaffected () =
  with_db (fun db ->
    seed_shadowable db;
    check_rows
      ~label:"a CTE under its own name joins normally"
      (List.init n_driving (fun _ -> [ "999" ]))
      (rows_of
         db
         "WITH c AS (SELECT 1 AS cw, 3 AS ci, 999 AS cq) SELECT cq FROM line JOIN c ON \
          ci = i_id WHERE w = 1 AND cw = 1"))
;;

(* The real table under the same query must still take the probe and still be
   right: the guard must not disable the optimisation it is protecting. *)
let the_real_table_still_joins_correctly () =
  with_db (fun db ->
    seed_shadowable db;
    check_rows
      ~label:"the unshadowed join still returns warehouse 1's row"
      (List.init n_driving (fun _ -> [ "103" ]))
      (rows_of db "SELECT qty FROM line JOIN stock ON si = i_id WHERE w = 1 AND sw = 1"))
;;

(* ------------------------------------------------------------------ *)
(* #565: the BASE-SCAN seek against a shadowed table                    *)
(* ------------------------------------------------------------------ *)

(* #551 (above) was [best_probe] reaching the catalog by name. #531 had already
   guarded [build_side]. Neither covered the third site, and it needs no join at
   all: [plan_base] -> [choose_access_path] -> [access_path_for_eqs] ->
   [find_index_for_eqs], which resolves the shadowed table's indexes and hands
   [seek_op] a [table_tree] of -1 taken from the CTE's synthesized meta.

   The trigger is a WHERE equality on a column that happens to be a leading
   index prefix OF THE SHADOWED TABLE. The CTE's own shape is irrelevant; only
   the name collides. The three spellings below are the issue's repro: no WHERE
   (no access path is chosen, so it was always right), a prefix equality (the
   bug), and an equality on a non-prefix column (nothing is seekable, so it was
   always right too). Keeping all three is what shows the guard did not simply
   disable the WHERE clause.

   #565 put the guard at the top of [access_path_for_eqs] rather than at its
   index branch. The issue's reason was that [Seek_rowid] is reachable through
   the same function; #594 established that arm cannot actually leak a shadowed
   identity, since [Cat.rowid_alias_col] reads the meta's own columns rather than
   the catalog — see [cte_shadowing_a_rowid_alias_table_scans]. The placement
   stands on the cheaper argument: one guard at the entry covers every arm the
   function grows, and the alternative is remembering to add one per arm. *)
let cte = "WITH stock AS (SELECT 1 AS sw, 3 AS si, 999 AS qty) "

let cte_shadowing_a_real_table_is_not_seeked_by_a_base_scan () =
  with_db (fun db ->
    seed_shadowable db;
    check_rows
      ~label:"no WHERE clause: no access path is chosen"
      [ [ "999" ] ]
      (rows_of db (cte ^ "SELECT qty FROM stock"));
    check_rows
      ~label:"a WHERE equality on a SHADOWED leading prefix (the #565 bug)"
      [ [ "999" ] ]
      (rows_of db (cte ^ "SELECT qty FROM stock WHERE sw = 1"));
    check_rows
      ~label:"and one on a column that is no index prefix at all"
      [ [ "999" ] ]
      (rows_of db (cte ^ "SELECT qty FROM stock WHERE qty = 999"));
    (* The whole composite key pinned reaches the same chooser by a longer
       prefix; it must be declined for the same reason. *)
    check_rows
      ~label:"the whole shadowed key pinned"
      [ [ "999" ] ]
      (rows_of db (cte ^ "SELECT qty FROM stock WHERE sw = 1 AND si = 3")))
;;

(* #594: a CONTROL, not a regression pin — this passes on `main` too, and the
   PR that added it said otherwise.

   The reasoning it was added on was wrong: [Cat.rowid_alias_col] is computed
   from the meta's OWN columns, not by a catalog lookup by name, so a CTE's
   synthesized meta answers [None] and the [Seek_rowid] arm is never reached with
   a shadowed table's identity. Unlike [find_index_for_eqs], which does reach the
   catalog by name, there is nothing here to leak.

   The guard's PLACEMENT is still right — putting it at the top of
   [access_path_for_eqs] rather than at the index branch costs nothing and
   removes a class of question — but it is defensive there, not load-bearing, and
   this case documents the boundary rather than pinning a fix. *)
let cte_shadowing_a_rowid_alias_table_scans () =
  with_db (fun db ->
    exec db "CREATE TABLE item (it_id INTEGER PRIMARY KEY, qty INTEGER)";
    exec db "INSERT INTO item VALUES (7, 70)";
    check_rows
      ~label:"the CTE's own row, not a rowid seek into the real table"
      [ [ "999" ] ]
      (rows_of
         db
         "WITH item AS (SELECT 7 AS it_id, 999 AS qty) SELECT qty FROM item WHERE it_id \
          = 7");
    check_rows
      ~label:"and a rowid that the real table has but the CTE does not"
      []
      (rows_of
         db
         "WITH item AS (SELECT 7 AS it_id, 999 AS qty) SELECT qty FROM item WHERE it_id \
          = 8"))
;;

(* [plan_dml_seek] reaches the same chooser, so DELETE and UPDATE inherit the
   fix. A CTE is not a writable target, so the shape that exercises this is a
   DML statement whose WHERE clause pins a prefix — the assertion is that the
   guard did not break the seek it protects. Both the seeked and unseekable
   spellings must affect the same rows. *)
let dml_seek_still_seeks_a_real_table () =
  with_db (fun db ->
    seed_shadowable db;
    exec db "DELETE FROM stock WHERE sw = 1 AND si = 2";
    check_rows
      ~label:"DELETE through the seek removed exactly its row"
      [ [ "1"; "1" ]; [ "1"; "3" ]; [ "1"; "4" ] ]
      (rows_of db "SELECT sw, si FROM stock WHERE sw = 1");
    exec db "UPDATE stock SET qty = 555 WHERE sw = 2 AND si = 2";
    check_rows
      ~label:"UPDATE through the seek touched exactly its row"
      [ [ "201" ]; [ "555" ]; [ "203" ]; [ "204" ] ]
      (rows_of db "SELECT qty FROM stock WHERE sw = 2");
    (* The unrecognisable spelling of the same predicate, as the control. *)
    exec db "DELETE FROM stock WHERE sw + 0 = 1 AND si = 3";
    check_rows
      ~label:"and the unseekable spelling agrees"
      [ [ "1"; "1" ]; [ "1"; "4" ] ]
      (rows_of db "SELECT sw, si FROM stock WHERE sw = 1"))
;;

(* The control that proves the guard keys on the synthesized [tree_id] and not
   on a name collision: an identically-shaped CTE under a name no table uses
   must behave the same, and the REAL table's own seek must still work. *)
let base_scan_seek_still_works_on_the_real_table () =
  with_db (fun db ->
    seed_shadowable db;
    check_rows
      ~label:"a CTE under its own name"
      [ [ "999" ] ]
      (rows_of
         db
         "WITH c AS (SELECT 1 AS cw, 3 AS ci, 999 AS cq) SELECT cq FROM c WHERE cw = 1");
    check_rows
      ~label:"and the unshadowed table still seeks its prefix"
      [ [ "101" ]; [ "102" ]; [ "103" ]; [ "104" ] ]
      (rows_of db "SELECT qty FROM stock WHERE sw = 1"))
;;

(* ------------------------------------------------------------------ *)
(* #566: a correlated subquery in an outer join's ON predicate          *)
(* ------------------------------------------------------------------ *)

(* [pre_eval_subquery] resolves an UNCORRELATED subquery once, before the pairing
   loop. A CORRELATED one survives it, and [eval_expr] answers [Row.V_null] for
   any [P_subquery] that reaches it — so the ON predicate is false for every
   pair, [any] is never set, and the join null-extends EVERY left row.

   That is a complete result set of the right cardinality with the ON predicate
   silently unevaluated, which reads from outside exactly like the correct
   answer. #566 chose to refuse it: [stream_hash_join] raises when a subquery
   survives [pre_eval_subquery] on the cartesian arm. Supporting it needs per-row
   re-evaluation against a correlation source [get_outer_scan_meta] cannot
   resolve over a join node — the same blocker the [Op_filter] path has. *)
let seed_correlated db =
  exec db "CREATE TABLE l (a INTEGER)";
  exec db "CREATE TABLE r (b INTEGER)";
  exec db "CREATE TABLE k (v INTEGER)";
  exec db "INSERT INTO l VALUES (1),(9)";
  exec db "INSERT INTO r VALUES (5)";
  exec db "INSERT INTO k VALUES (4)"
;;

let err_of db sql =
  match run (Db.query db sql) with
  | Error e -> Format.asprintf "%a" Db.pp_error e
  | Ok stream ->
    (try
       ignore (run (Lwt_stream.to_list stream));
       ""
     with
     | Failure m -> m
     | e -> Printexc.to_string e)
;;

let correlated_on_subquery_is_refused () =
  with_db (fun db ->
    seed_correlated db;
    let msg =
      err_of db "SELECT a, b FROM l LEFT JOIN r ON b > (SELECT v FROM k WHERE v < a)"
    in
    Alcotest.(check bool)
      (Printf.sprintf "the query is refused, not answered (got %S)" msg)
      true
      (msg <> "");
    (* The message has to name the shape, or a caller cannot act on it. *)
    Alcotest.(check bool)
      (Printf.sprintf "and the message names the cause (got %S)" msg)
      true
      (let has needle =
         let n = String.length needle
         and m = String.length msg in
         let rec go i = i + n <= m && (String.sub msg i n = needle || go (i + 1)) in
         go 0
       in
       has "correlated subquery" && has "#566"))
;;

(* The uncorrelated spelling was already correct and must stay correct — it is
   what makes the wrong answer above invisible from outside, so a refusal that
   swallowed this too would be no better. SQLite 3.45.1 answers [1|5], [9|5]. *)
let uncorrelated_on_subquery_still_works () =
  with_db (fun db ->
    seed_correlated db;
    check_rows
      ~label:"an uncorrelated ON subquery is resolved once and matches"
      [ [ "1"; "5" ]; [ "9"; "5" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b > (SELECT v FROM k)");
    (* And one that matches nothing still null-extends rather than raising. *)
    check_rows
      ~label:"an uncorrelated ON subquery that matches nothing null-extends"
      [ [ "1"; "NULL" ]; [ "9"; "NULL" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b > (SELECT v FROM k WHERE v > 99)"))
;;

(* #592: the INNER spelling of the refused query. It is NOT refused, because
   [general_on_join] gives an INNER join's ON predicate to an [Op_filter] above
   the join rather than to [on_pred]. It is also not correct: [stream_filter]'s
   [get_outer_scan_meta] answers [None] over a join and drops every row, so this
   returns nothing where sqlite3 3.45.1 returns [9|5].

   Pinned as-is, wrong answer and all, so that #592 has a case to flip and so
   that "#566 left inner joins untouched" is not mistaken for "inner joins are
   fine". *)
let inner_correlated_on_subquery_is_still_wrong_592 () =
  with_db (fun db ->
    seed_correlated db;
    check_rows
      ~label:"#592: INNER returns nothing; sqlite3 returns 9|5"
      []
      (rows_of db "SELECT a, b FROM l INNER JOIN r ON b > (SELECT v FROM k WHERE v < a)"))
;;

(* An outer join whose ON predicate has no subquery at all is untouched: the
   refusal must key on a surviving [P_subquery], not on the general-ON arm. *)
let plain_general_on_is_untouched_by_the_refusal () =
  with_db (fun db ->
    seed_correlated db;
    check_rows
      ~label:"the #539 shape still null-extends"
      [ [ "1"; "5" ]; [ "9"; "NULL" ] ]
      (rows_of db "SELECT a, b FROM l LEFT JOIN r ON b > a"))
;;

let () =
  Alcotest.run
    "join_general_on_552"
    [ ( "general ON null-extends (#552, #539)"
      , [ Alcotest.test_case "non-equi ON null-extends" `Quick non_equi_on_null_extends
        ; Alcotest.test_case
            "IS NULL ON null-extends every left row"
            `Quick
            is_null_on_null_extends_every_left_row
        ; Alcotest.test_case
            "mixed population keeps matches and null-extends"
            `Quick
            mixed_population_keeps_matches_and_null_extends
        ; Alcotest.test_case
            "empty right table still null-extends"
            `Quick
            empty_right_table_still_null_extends
        ; Alcotest.test_case
            "constant-false ON null-extends"
            `Quick
            constant_false_on_null_extends
        ; Alcotest.test_case
            "the no-catalog join chain null-extends"
            `Quick
            no_cat_general_on_null_extends
        ; Alcotest.test_case
            "WHERE on the right table still drops null-extended rows"
            `Quick
            where_on_the_right_table_still_drops_null_extended_rows
        ; Alcotest.test_case
            "general ON as the second join in a chain"
            `Quick
            general_on_as_the_second_join_in_a_chain
        ; Alcotest.test_case
            "EXPLAIN names the general ON predicate"
            `Quick
            explain_names_the_general_on
        ; Alcotest.test_case
            "a keyed hash join rejects an on_pred"
            `Quick
            keyed_join_rejects_an_on_pred
        ] )
    ; ( "property"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_left_join_general_on_matches_the_model ] )
    ; ( "probe against a shadowed table (#551)"
      , [ Alcotest.test_case
            "CTE shadowing a real table is not probed"
            `Quick
            cte_shadowing_a_real_table_is_not_probed
        ; Alcotest.test_case
            "CTE shadowing a real table is not probed in a LEFT JOIN"
            `Quick
            cte_shadowing_a_real_table_is_not_probed_in_a_left_join
        ; Alcotest.test_case
            "non-shadowing CTE is unaffected"
            `Quick
            non_shadowing_cte_is_unaffected
        ; Alcotest.test_case
            "the real table still joins correctly"
            `Quick
            the_real_table_still_joins_correctly
        ] )
    ; ( "base-scan seek against a shadowed table (#565)"
      , [ Alcotest.test_case
            "CTE shadowing a real table is not seeked by a base scan"
            `Quick
            cte_shadowing_a_real_table_is_not_seeked_by_a_base_scan
        ; Alcotest.test_case
            "CTE shadowing a rowid-alias table scans"
            `Quick
            cte_shadowing_a_rowid_alias_table_scans
        ; Alcotest.test_case
            "DML seek still seeks a real table"
            `Quick
            dml_seek_still_seeks_a_real_table
        ; Alcotest.test_case
            "base-scan seek still works on the real table"
            `Quick
            base_scan_seek_still_works_on_the_real_table
        ] )
    ; ( "correlated ON subquery is refused (#566)"
      , [ Alcotest.test_case
            "a correlated ON subquery is refused"
            `Quick
            correlated_on_subquery_is_refused
        ; Alcotest.test_case
            "an uncorrelated ON subquery still works"
            `Quick
            uncorrelated_on_subquery_still_works
        ; Alcotest.test_case
            "INNER correlated ON subquery is still wrong (#592)"
            `Quick
            inner_correlated_on_subquery_is_still_wrong_592
        ; Alcotest.test_case
            "a plain general ON is untouched by the refusal"
            `Quick
            plain_general_on_is_untouched_by_the_refusal
        ] )
    ]
;;
