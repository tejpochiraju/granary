(** #741: [excluded] is a SCOPE, not a shape.

    [Sema.bind_upsert_rhs_expr] used to recognise [excluded.<col>] only when the
    [E_tbl_col ("EXCLUDED", c)] sat at the ROOT of a DO UPDATE assignment RHS.
    Anything nested fell through to [bind_expr], whose [E_tbl_col] arm ignores
    the qualifier entirely (the documented single-table fallback shared by
    INSERT/UPDATE/DELETE/CHECK/DEFAULT), so [SET v = excluded.v + 1000] silently
    read the TARGET row's [v] — a wrong answer with no error.

    The Exec side was already right: [Plan.P_excluded_col] is a general
    expression leaf and [Exec.substitute_excluded] recurses through the whole
    plan expression. The gap was purely that nested occurrences never became
    [BE_excluded_col] in the first place. The fix threads an [excluded] scope
    flag through [bind_expr]'s own recursion and sets it for the whole RHS, so
    the resolution is exhaustive by construction rather than shape-matched.

    Every expected value below was oracle-checked against sqlite3 3.45.1 before
    it was written down.

    The scope must stay narrow: [excluded.x] is still unresolvable in a plain
    SELECT, which is what sqlite3 does. The one place granary still diverges is
    pinned at the bottom — a plain [UPDATE t SET v = excluded.v] resolves to the
    target's own [v] through the single-table fallback, where sqlite3 errors.
    That is pre-existing and was deliberately NOT changed here: widening or
    narrowing that fallback moves CHECK, DEFAULT, INSERT and DELETE with it.

    #742 is pinned at the bottom too, as an accepted divergence rather than a
    fix — see the comment there. *)

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

let texts db sql =
  List.map
    (fun (r : Row.t) ->
       Array.to_list r
       |> List.map (function
         | Row.V_text s -> s
         | Row.V_null -> "<null>"
         | Row.V_int n -> Int64.to_string n
         | Row.V_real f -> string_of_float f
         | Row.V_blob _ -> "<blob>")
       |> String.concat "|")
    (query db sql)
;;

let contains_sub ~needle s =
  let n = String.length needle
  and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = needle || go (i + 1)) in
  go 0
;;

let rejects db ~needle sql =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "expected %S to be rejected, it succeeded" sql
  | Error e ->
    let msg = Format.asprintf "%a" Db.pp_error e in
    Alcotest.(check bool)
      (Printf.sprintf "%S rejected mentioning %S (got %S)" sql needle msg)
      true
      (contains_sub ~needle msg)
;;

(* One target row, one proposed row, one assignment: whatever [rhs] evaluates to
   is what lands in [v].  Every case below is the same shape so the only moving
   part is the expression under test. *)
let upsert_v db ~stored ~proposed ~rhs =
  exec db "DROP TABLE IF EXISTS t";
  exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER)";
  exec db (Printf.sprintf "INSERT INTO t VALUES (1, %d)" stored);
  exec
    db
    (Printf.sprintf
       "INSERT INTO t VALUES (1, %d) ON CONFLICT(k) DO UPDATE SET v = %s"
       proposed
       rhs);
  texts db "SELECT v FROM t"
;;

let check_v db ~name ~stored ~proposed ~rhs ~expect =
  Alcotest.(check (list string)) name [ expect ] (upsert_v db ~stored ~proposed ~rhs)
;;

(* The issue's own repro.  sqlite3 3.45.1: 1010.  Before the fix granary
   answered 1005 — the stored row's 5, not the proposed row's 10. *)
let the_issue_repro () =
  with_db (fun db ->
    check_v
      db
      ~name:"excluded.v + 1000 reads the PROPOSED row"
      ~stored:5
      ~proposed:10
      ~rhs:"excluded.v + 1000"
      ~expect:"1010")
;;

(* The bare root spelling was always correct; it must stay correct, since the
   fix replaced the arm that handled it. *)
let bare_root_excluded_still_works () =
  with_db (fun db ->
    check_v
      db
      ~name:"SET v = excluded.v"
      ~stored:5
      ~proposed:10
      ~rhs:"excluded.v"
      ~expect:"10")
;;

(* The idiom the issue names: accumulate the proposed value onto the stored one.
   Both sides of the binop resolve, and to DIFFERENT rows — this is the case a
   root-only match cannot express at all.  sqlite3: 15. *)
let accumulate_idiom () =
  with_db (fun db ->
    check_v
      db
      ~name:"SET v = v + excluded.v"
      ~stored:5
      ~proposed:10
      ~rhs:"v + excluded.v"
      ~expect:"15")
;;

(* Both operands are excluded refs, so a fix that only rewrote (say) the right
   operand would still pass the case above.  sqlite3: (10-5)*(10+5) = 75. *)
let excluded_on_both_sides_of_a_binop () =
  with_db (fun db ->
    check_v
      db
      ~name:"SET v = (excluded.v - v) * (excluded.v + v)"
      ~stored:5
      ~proposed:10
      ~rhs:"(excluded.v - v) * (excluded.v + v)"
      ~expect:"75")
;;

(* CASE binds through a separate helper ([bind_case]) that takes the binder as a
   callback, so it is a distinct path from the plain recursive arms.  The ref
   appears in the branch condition AND in the result.  sqlite3: 20. *)
let excluded_inside_case () =
  with_db (fun db ->
    check_v
      db
      ~name:"SET v = CASE WHEN excluded.v > v THEN excluded.v * 2 ELSE v END"
      ~stored:5
      ~proposed:10
      ~rhs:"CASE WHEN excluded.v > v THEN excluded.v * 2 ELSE v END"
      ~expect:"20")
;;

(* Function arguments go through [bind_func], the other callback-taking helper.
   sqlite3: coalesce(10, 50) * 10 + abs(10 - 50) = 100 + 40 = 140.  The [* 10]
   is load-bearing: without it the buggy and the correct readings both give 50,
   so the case passed against the unfixed binder.  (Note granary has
   no scalar two-argument max(), so the equally idiomatic
   [SET v = max(v, excluded.v)] is not expressible here — that is a separate,
   pre-existing gap in the parser, unrelated to #741.) *)
let excluded_inside_function_calls () =
  with_db (fun db ->
    check_v
      db
      ~name:"SET v = coalesce(excluded.v, v) * 10 + abs(excluded.v - v)"
      ~stored:50
      ~proposed:10
      ~rhs:"coalesce(excluded.v, v) * 10 + abs(excluded.v - v)"
      ~expect:"140")
;;

(* CAST, BETWEEN and IN each have their own arm.  sqlite3:
   CAST(10 AS INTEGER) + 1 = 11, and (10 BETWEEN 1 AND 20) + (10 IN (10,11))*10
   = 1 + 10 = 11. *)
let excluded_inside_cast_between_and_in () =
  with_db (fun db ->
    check_v
      db
      ~name:"SET v = CAST(excluded.v AS INTEGER) + 1"
      ~stored:5
      ~proposed:10
      ~rhs:"CAST(excluded.v AS INTEGER) + 1"
      ~expect:"11";
    check_v
      db
      ~name:"SET v = (excluded.v BETWEEN 1 AND 20) + (excluded.v IN (10, 11)) * 10"
      ~stored:5
      ~proposed:10
      ~rhs:"(excluded.v BETWEEN 1 AND 20) + (excluded.v IN (10, 11)) * 10"
      ~expect:"11")
;;

(* A nested ref to the CONFLICT KEY column itself, not just to a payload column.
   sqlite3: 1*100 + 10 = 110. *)
let excluded_names_the_conflict_key () =
  with_db (fun db ->
    check_v
      db
      ~name:"SET v = excluded.k * 100 + excluded.v"
      ~stored:5
      ~proposed:10
      ~rhs:"excluded.k * 100 + excluded.v"
      ~expect:"110")
;;

(* Non-integer payload, so a wrong resolution cannot be masked by arithmetic
   coincidence.  sqlite3: NEW. *)
let excluded_in_a_text_expression () =
  with_db (fun db ->
    exec db "CREATE TABLE f (k INTEGER PRIMARY KEY, v TEXT)";
    exec db "INSERT INTO f VALUES (1, 'old')";
    exec
      db
      "INSERT INTO f VALUES (1, 'new') ON CONFLICT(k) DO UPDATE SET v = upper(excluded.v)";
    Alcotest.(check (list string))
      "upper(excluded.v)"
      [ "NEW" ]
      (texts db "SELECT v FROM f"))
;;

(* Two assignments in one DO UPDATE, each reading a DIFFERENT excluded column,
   swapped relative to the target, and each nested so neither is a root match.
   sqlite3: 1020|11. *)
let several_assignments_each_with_an_excluded_ref () =
  with_db (fun db ->
    exec db "CREATE TABLE p (k INTEGER PRIMARY KEY, v INTEGER, w INTEGER)";
    exec db "INSERT INTO p VALUES (1, 5, 7)";
    exec
      db
      "INSERT INTO p VALUES (1, 10, 20) ON CONFLICT(k) DO UPDATE SET v = excluded.w + \
       1000, w = excluded.v + 1";
    Alcotest.(check (list string))
      "v, w swapped from the proposed row"
      [ "1020|11" ]
      (texts db "SELECT v, w FROM p"))
;;

(* A nested [excluded.<col>] naming a column the table does not have must be a
   clean bind error, not a silent fallthrough to the target row.  sqlite3
   rejects with "no such column: excluded.nosuch". *)
let unknown_excluded_column_is_a_clean_bind_error () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 5)";
    rejects
      db
      ~needle:"excluded.nosuch"
      "INSERT INTO t VALUES (1, 10) ON CONFLICT(k) DO UPDATE SET v = excluded.nosuch + 1";
    rejects
      db
      ~needle:"excluded.nosuch"
      "INSERT INTO t VALUES (1, 10) ON CONFLICT(k) DO UPDATE SET v = excluded.nosuch";
    Alcotest.(check (list string)) "row untouched" [ "5" ] (texts db "SELECT v FROM t"))
;;

(* The scope is not global: [excluded] stays unresolvable in a plain SELECT, and
   the fix must not have widened [bind_expr]'s single-table fallback. *)
let excluded_is_not_in_scope_in_a_select () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 5)";
    (match run (Db.query db "SELECT excluded.v FROM t") with
     | Error e ->
       let msg = Format.asprintf "%a" Db.pp_error e in
       Alcotest.(check bool)
         (Printf.sprintf "SELECT excluded.v rejected (got %S)" msg)
         true
         (contains_sub ~needle:"excluded" msg)
     | Ok _ -> Alcotest.fail "expected SELECT excluded.v to be rejected");
    (* And a WHERE over it, which reaches the same alias-aware resolution. *)
    match run (Db.query db "SELECT v FROM t WHERE excluded.v = 5") with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "expected WHERE excluded.v to be rejected")
;;

(* ACCEPTED RESIDUAL, recorded rather than fixed.  A plain UPDATE binds through
   [bind_expr] with the scope flag unset, so [excluded.v] hits the single-table
   fallback, which ignores the qualifier and resolves the TARGET's own [v].
   sqlite3 errors ("no such column: excluded.v").  #741 deliberately did not
   touch that fallback: it is shared by INSERT, UPDATE, DELETE, CHECK and
   DEFAULT, and narrowing it is a much larger, separate decision.  If that
   changes, this test changes with it — it is not asserting the divergence is
   right, only that #741 did not silently alter it. *)
let plain_update_still_takes_the_single_table_fallback () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 5)";
    exec db "UPDATE t SET v = excluded.v + 1";
    Alcotest.(check (list string))
      "excluded.v resolved to the target's own v (divergence from sqlite3)"
      [ "6" ]
      (texts db "SELECT v FROM t"))
;;

(* #742, ACCEPTED DIVERGENCE, pinned rather than fixed.  [Exec.execute_insert]
   allocates the rowid before conflict resolution, so a row that is then
   discarded — by a DO UPDATE, or by an OR IGNORE skip — leaves its allocation
   burnt.  In autocommit each row is its own committed transaction, so the bump
   is durable and the next row gets 3 where sqlite3 gives 2.

   Not fixed, and the reason is an ordering the engine depends on:
   [insert_rowid] writes the auto-assigned INTEGER PRIMARY KEY value back into
   the row, and BOTH the #639 NOT NULL check and the index-key extraction that
   feeds [check_insert_unique] read that row afterwards.  Deferring the
   allocation past conflict resolution would make an auto-assigned PK — NOT NULL
   since #530 — read NULL at that check and raise on the commonest INSERT shape.
   Handing the allocation back instead is the "too LOW" direction that #632/#706
   forbid, while a skipped id is the residual direction those writeups already
   accept.  Note also that the burn is NOT specific to the upsert branch, so a
   fix scoped there would leave the identical divergence reachable through
   [INSERT OR IGNORE], which predates #639 entirely — both spellings are pinned
   below. *)
let burnt_rowid_on_a_discarded_insert_row () =
  with_db (fun db ->
    exec db "CREATE TABLE s (id INTEGER PRIMARY KEY, k INTEGER, v INTEGER)";
    exec db "CREATE UNIQUE INDEX s_k ON s(k)";
    exec db "INSERT INTO s VALUES (1, 100, 5)";
    exec
      db
      "INSERT INTO s (k, v) VALUES (100, 10), (200, 20) ON CONFLICT(k) DO UPDATE SET v = \
       99";
    Alcotest.(check (list string))
      "the DO UPDATE burnt id 2 (sqlite3 gives 1|100|99, 2|200|20)"
      [ "1|100|99"; "3|200|20" ]
      (texts db "SELECT id, k, v FROM s ORDER BY id");
    exec db "CREATE TABLE u (id INTEGER PRIMARY KEY, k INTEGER, v INTEGER)";
    exec db "CREATE UNIQUE INDEX u_k ON u(k)";
    exec db "INSERT INTO u VALUES (1, 100, 5)";
    exec db "INSERT OR IGNORE INTO u (k, v) VALUES (100, 10)";
    exec db "INSERT INTO u (k, v) VALUES (200, 20)";
    Alcotest.(check (list string))
      "OR IGNORE burns it identically, with no upsert clause in sight"
      [ "1|100|5"; "3|200|20" ]
      (texts db "SELECT id, k, v FROM u ORDER BY id"))
;;

let () =
  Alcotest.run
    "nested excluded (#741)"
    [ ( "nested excluded resolves to the proposed row"
      , [ Alcotest.test_case "issue repro: excluded.v + 1000" `Quick the_issue_repro
        ; Alcotest.test_case "v + excluded.v" `Quick accumulate_idiom
        ; Alcotest.test_case
            "both sides of a binop"
            `Quick
            excluded_on_both_sides_of_a_binop
        ; Alcotest.test_case "inside CASE" `Quick excluded_inside_case
        ; Alcotest.test_case "inside function calls" `Quick excluded_inside_function_calls
        ; Alcotest.test_case
            "inside CAST / BETWEEN / IN"
            `Quick
            excluded_inside_cast_between_and_in
        ; Alcotest.test_case
            "the conflict key column"
            `Quick
            excluded_names_the_conflict_key
        ; Alcotest.test_case "a TEXT expression" `Quick excluded_in_a_text_expression
        ; Alcotest.test_case
            "several assignments"
            `Quick
            several_assignments_each_with_an_excluded_ref
        ] )
    ; ( "unchanged"
      , [ Alcotest.test_case "bare root excluded.v" `Quick bare_root_excluded_still_works
        ; Alcotest.test_case
            "unknown excluded column is a bind error"
            `Quick
            unknown_excluded_column_is_a_clean_bind_error
        ; Alcotest.test_case
            "not in scope in a SELECT"
            `Quick
            excluded_is_not_in_scope_in_a_select
        ] )
    ; ( "accepted divergences"
      , [ Alcotest.test_case
            "plain UPDATE takes the single-table fallback"
            `Quick
            plain_update_still_takes_the_single_table_fallback
        ; Alcotest.test_case
            "#742: a discarded insert row burns its rowid"
            `Quick
            burnt_rowid_on_a_discarded_insert_row
        ] )
    ]
;;
