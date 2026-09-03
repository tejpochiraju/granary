(** #732: a correlated outer reference in a subquery's own PROJECTION — or in a
    FROM-less subquery — is substituted, so the statement is answered instead of
    refused.

    {1 The defect}

    [Exec.substitute_outer_in_expr] became exhaustive in #670/#723, but its
    caller [substitute_outer_in_stmt] enumerated {i clauses} by hand: for
    [S_select] it rewrote only [where], [having] and [joins.*.on], passed
    [proj] / [group_by] / [order] straight through with [{ r with … }], and
    ended in a [| _ -> s] catch-all covering every other statement form.

    So the issue's own query

    {v
      SELECT k, (SELECT i.m + o.n FROM i WHERE i.fk = o.k) FROM o
    v}

    left [o.n] in the inner projection unsubstituted and came back as
    [correlated_projection_refusal] (#626), while sqlite3 answers
    [a|11 b|22 c|33]. Dropping the [+ o.n] answered correctly, so the
    unrewritten clause was the whole of it.

    {1 What this fix does and does not widen}

    Rewritten now: [proj] (see [substitute_outer_proj]) and the FROM-less
    [S_const_select] that [(SELECT o.n * 10)] parses to. The statement match is
    exhaustive rather than a catch-all, so a new statement form that can appear
    as a subquery body is a compile error rather than a silent refusal.

    NOT rewritten, deliberately:

    - [limit] / [offset] are [int option] and [group_by] is a
      [(string * string option) list]. Neither can hold the literal a
      substitution produces, so there is no spelling in which an outer
      reference reaches them at all.
    - [order] is left alone on purpose, and it is the one judgement call here.
      sqlite3 {i also} refuses a correlated reference in a subquery's ORDER BY
      ("no such column: o.n", oracle-checked on 3.45.1), so rewriting it would
      create a divergence rather than remove one; and an ORDER BY key may name
      an OUTPUT ALIAS (#489/#663), which [inner_scope_of] does not know about,
      so a subquery whose alias collided with an outer column name would have
      its sort key rewritten to a constant and silently return unsorted rows.
      A refusal is the better of those two. The two cases below pin the
      refusal so that anything which later resolves them is a deliberate
      change.

    {1 Oracle}

    Values marked "oracle" were observed from the [sqlite3] 3.45.1 in the dev
    image on the same schema and data. Values marked "hand-computed" are
    arithmetic over the fixture, derived rather than observed. *)

module Db = Granary.Db

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

(* The refusals in this area are [Lwt.fail_with] from inside a stream, so one
   can arrive either as [Error] from [Db.query] or as an exception during the
   drain — #627's documented residual, and not a distinction this issue is
   about. Both are [Refused]. *)
type outcome =
  | Refused
  | Rows of string list list

let outcome_of db sql =
  match run (Db.query db sql) with
  | Error _ -> Refused
  | exception _ -> Refused
  | Ok stream ->
    (match run (Lwt_stream.to_list stream) with
     | rows ->
       Rows
         (List.sort compare (List.map (fun r -> Array.to_list (Array.map render r)) rows))
     | exception _ -> Refused)
;;

let check_rows ~label expected db sql =
  match outcome_of db sql with
  | Refused -> Alcotest.failf "%s: %S was REFUSED; expected rows" label sql
  | Rows actual ->
    Alcotest.(check (list (list string))) label (List.sort compare expected) actual
;;

let check_refused ~label db sql =
  match outcome_of db sql with
  | Refused -> ()
  | Rows rows ->
    Alcotest.failf
      "%s: %S answered %d row(s); expected a refusal"
      label
      sql
      (List.length rows)
;;

(* The issue's own fixture, verbatim. One [i] row per [fk], so every scalar
   subquery below has exactly one candidate row and no assertion depends on
   scan order. *)
let seed db =
  exec db "CREATE TABLE o (k TEXT, x TEXT, n INTEGER)";
  exec db "CREATE TABLE i (fk TEXT, v TEXT, m INTEGER)";
  exec db "INSERT INTO o VALUES ('a','HELLO',1),('b','world',2),('c','zzz',3)";
  exec db "INSERT INTO i VALUES ('a','hello',10),('b','WORLD',20),('c','yyy',30)"
;;

(* The issue's headline query. sqlite3 (oracle): a|11  b|22  c|33 *)
let the_issues_query_answers_rows () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"outer ref in the subquery's projection"
      [ [ "a"; "11" ]; [ "b"; "22" ]; [ "c"; "33" ] ]
      db
      "SELECT k, (SELECT i.m + o.n FROM i WHERE i.fk = o.k) FROM o")
;;

(* The control named in the issue: drop the [+ o.n] and the same statement
   already answered before the fix. It is what makes the values above a
   statement about the substitution rather than about the join.

   sqlite3 (oracle): a|10  b|20  c|30 *)
let the_same_query_without_the_outer_reference_was_already_answered () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"no outer ref in the projection"
      [ [ "a"; "10" ]; [ "b"; "20" ]; [ "c"; "30" ] ]
      db
      "SELECT k, (SELECT i.m FROM i WHERE i.fk = o.k) FROM o")
;;

(* An UNQUALIFIED outer reference is the [`Cols] shape: the parser emits
   [`Cols] only when every projected item is a bare [E_col] (parser.mly:897),
   and [`Cols] is a [string list] that cannot hold a substituted literal. So
   [substitute_outer_proj] promotes the projection to [`Exprs] when — and only
   when — a name actually resolves against the outer row. [i] has no column
   [n], so [n] here is [o]'s.

   sqlite3 (oracle): a|1  b|2  c|3 *)
let an_unqualified_outer_reference_promotes_the_cols_projection () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"bare `n` in the inner projection resolves outward"
      [ [ "a"; "1" ]; [ "b"; "2" ]; [ "c"; "3" ] ]
      db
      "SELECT k, (SELECT n FROM i WHERE i.fk = o.k) FROM o")
;;

(* Its boundary: a name the inner FROM DOES own must keep resolving inward.
   [m] is [i]'s, so the promotion must not fire and the value is the inner
   row's, not any outer row's.

   sqlite3 (oracle): a|10  b|20  c|30 — identical to the qualified spelling. *)
let an_unqualified_inner_name_still_resolves_inward () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"bare `m` is the subquery's own column"
      [ [ "a"; "10" ]; [ "b"; "20" ]; [ "c"; "30" ] ]
      db
      "SELECT k, (SELECT m FROM i WHERE i.fk = o.k) FROM o")
;;

(* A FROM-less subquery parses to [S_const_select], which the old catch-all
   passed through untouched. It owns no input at all, so every column
   reference in it is by construction the enclosing query's — which is why
   [inner_scope_of] answers {!no_inner_scope} for it rather than the
   "owns everything" default it gives other non-SELECT forms.

   sqlite3 (oracle): a|10  b|20  c|30 *)
let a_from_less_subquery_is_substituted () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"(SELECT o.n * 10)"
      [ [ "a"; "10" ]; [ "b"; "20" ]; [ "c"; "30" ] ]
      db
      "SELECT k, (SELECT o.n * 10) FROM o")
;;

(* Two levels, with the second one inside the first's PROJECTION: the descent
   has to carry the union of every enclosing scope down through a clause that
   was not walked at all before (#635's rule, reached through #732's clause).

   Hand-computed: a|100  b|200  c|300 *)
let a_nested_from_less_subquery_in_a_projection_is_substituted () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"(SELECT (SELECT o.n * 100) FROM i WHERE i.fk = o.k)"
      [ [ "a"; "100" ]; [ "b"; "200" ]; [ "c"; "300" ] ]
      db
      "SELECT k, (SELECT (SELECT o.n * 100) FROM i WHERE i.fk = o.k) FROM o")
;;

(* The same hole reached through [E_in_select] rather than [E_subquery]: the
   projection being rewritten is the IN-list's.

   Hand-computed: only [a]'s subquery yields 11 (10 + 1); [b] yields 22 and
   [c] yields 33. *)
let an_outer_reference_in_an_in_selects_projection_is_substituted () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"11 IN (SELECT i.m + o.n ...)"
      [ [ "a" ] ]
      db
      "SELECT k FROM o WHERE 11 IN (SELECT i.m + o.n FROM i WHERE i.fk = o.k)")
;;

(* Its control: without the outer term no subquery yields 11. Hand-computed. *)
let the_same_in_select_without_the_outer_reference_matches_nothing () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"11 IN (SELECT i.m ...)"
      []
      db
      "SELECT k FROM o WHERE 11 IN (SELECT i.m FROM i WHERE i.fk = o.k)")
;;

(* The detector moves with the substituter, and this is the shape that shows
   it. Here the ONLY free reference is in the inner projection, so before the
   fix [stmt_has_free_column_ref] answered [false], the subquery was evaluated
   eagerly — before any outer row existed — and [Sema.bind] refused
   [SELECT o.n FROM i]. Now it is classified as correlated, substituted, and
   answered. EXISTS ignores the projected value, so every outer row qualifies.

   sqlite3 (oracle): a  b  c *)
let a_free_reference_only_in_the_projection_makes_the_subquery_correlated () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"EXISTS (SELECT o.n FROM i)"
      [ [ "a" ]; [ "b" ]; [ "c" ] ]
      db
      "SELECT k FROM o WHERE EXISTS (SELECT o.n FROM i)")
;;

(* An uncorrelated projection subquery is untouched: no free reference, so the
   detector still answers [false] and it is folded once rather than per row.

   sqlite3 (oracle): a|10  b|10  c|10 *)
let an_uncorrelated_projection_subquery_is_unchanged () =
  with_db (fun db ->
    seed db;
    check_rows
      ~label:"no correlation: one value for every outer row"
      [ [ "a"; "10" ]; [ "b"; "10" ]; [ "c"; "10" ] ]
      db
      "SELECT k, (SELECT m FROM i WHERE i.fk = 'a') FROM o")
;;

(* A name NEITHER scope carries is still refused, not answered NULL. The
   promotion in [substitute_outer_proj] is conditional on the binding actually
   resolving the name, so an unresolvable one keeps the [`Cols] shape and
   reaches the same refusal as before. *)
let a_name_no_scope_carries_is_still_refused () =
  with_db (fun db ->
    seed db;
    check_refused
      ~label:"unresolvable name in the inner projection"
      db
      "SELECT k, (SELECT nosuch FROM i WHERE i.fk = o.k) FROM o")
;;

(* ORDER BY is deliberately NOT rewritten — see the file header. sqlite3
   refuses this too (oracle):

     sqlite> SELECT k, (SELECT i.m FROM i WHERE i.fk = o.k ORDER BY o.n LIMIT 1)
        ...>   FROM o;
     Parse error: no such column: o.n

   Pinned so that anything which later resolves it is a deliberate change and
   arrives with the output-alias hazard answered. *)
let an_outer_reference_in_the_subquerys_order_by_is_still_refused () =
  with_db (fun db ->
    seed db;
    check_refused
      ~label:"outer ref in the subquery's ORDER BY"
      db
      "SELECT k, (SELECT i.m FROM i WHERE i.fk = o.k ORDER BY o.n LIMIT 1) FROM o")
;;

(* GROUP BY cannot hold a substituted value at all — [Ast.group_by_item] is
   [string * string option], not an [expr]. sqlite3 also refuses (oracle:
   "no such column: o.n"). *)
let an_outer_reference_in_the_subquerys_group_by_is_still_refused () =
  with_db (fun db ->
    seed db;
    check_refused
      ~label:"outer ref in the subquery's GROUP BY"
      db
      "SELECT k, (SELECT COUNT(*) FROM i WHERE i.fk = o.k GROUP BY o.n) FROM o")
;;

let () =
  Alcotest.run
    "outer_ref_732"
    [ ( "projection"
      , [ Alcotest.test_case "the issue's query" `Quick the_issues_query_answers_rows
        ; Alcotest.test_case
            "control: no outer reference"
            `Quick
            the_same_query_without_the_outer_reference_was_already_answered
        ; Alcotest.test_case
            "unqualified outer reference promotes `Cols"
            `Quick
            an_unqualified_outer_reference_promotes_the_cols_projection
        ; Alcotest.test_case
            "unqualified inner name still resolves inward"
            `Quick
            an_unqualified_inner_name_still_resolves_inward
        ; Alcotest.test_case
            "IN (SELECT ...) projection"
            `Quick
            an_outer_reference_in_an_in_selects_projection_is_substituted
        ; Alcotest.test_case
            "control: IN (SELECT ...) without the outer reference"
            `Quick
            the_same_in_select_without_the_outer_reference_matches_nothing
        ; Alcotest.test_case
            "a free reference only in the projection is correlation"
            `Quick
            a_free_reference_only_in_the_projection_makes_the_subquery_correlated
        ] )
    ; ( "S_const_select"
      , [ Alcotest.test_case
            "FROM-less subquery"
            `Quick
            a_from_less_subquery_is_substituted
        ; Alcotest.test_case
            "nested FROM-less subquery in a projection"
            `Quick
            a_nested_from_less_subquery_in_a_projection_is_substituted
        ] )
    ; ( "boundaries"
      , [ Alcotest.test_case
            "uncorrelated projection subquery unchanged"
            `Quick
            an_uncorrelated_projection_subquery_is_unchanged
        ; Alcotest.test_case
            "unresolvable name still refused"
            `Quick
            a_name_no_scope_carries_is_still_refused
        ; Alcotest.test_case
            "ORDER BY still refused (matches sqlite3)"
            `Quick
            an_outer_reference_in_the_subquerys_order_by_is_still_refused
        ; Alcotest.test_case
            "GROUP BY still refused (matches sqlite3)"
            `Quick
            an_outer_reference_in_the_subquerys_group_by_is_still_refused
        ] )
    ]
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5"]
[@@@ai_provider "Anthropic"]
