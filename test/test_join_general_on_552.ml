(** #552 / #539 / #551: two join-planning defects that produce wrong answers.

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
    catch it. Every case here seeds 10. *)

module Db = Granary.Db
module Cat = Granary_catalog.Catalog
module Store = Granary_store.Store
module Row = Granary_encoding.Row
module Sema = Granary_sql.Sema
module Parser = Granary_sql.Parser
module Lexer = Granary_sql.Lexer
module Planner = Granary_sql.Planner
module Exec = Granary_sql.Exec

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
        ] )
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
    ]
;;
