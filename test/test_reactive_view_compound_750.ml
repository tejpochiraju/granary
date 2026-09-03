(** #750 — a compound-root [CREATE REACTIVE VIEW] is refused.

    [Reactive_view.base_tables_of] reads the base tables off the [S_select] at
    the root of the view body and answers [[]] for anything else. An
    [S_compound] root landed there, so the view registered with NO base tables
    and nothing ever marked it dirty: it materialised correctly once and then
    served that first snapshot forever, with no error at any point.

    This is the third instance of one pattern — a [Reactive_view] helper
    matching [S_select] and answering a benign-looking default for everything
    else — after #486 ([base_tables_of], derived table) and #747 ([proj_of],
    [SELECT *]). All three are closed the same way, by refusing the shape at
    bind time.

    Supporting it properly is a separate decision: it needs [base_tables_of] to
    union both arms {b and} a correct incremental rule per set operation, and
    neither UNION (distinct) nor EXCEPT is an additive merge over Z-sets.

    Every refusal test here is paired with a {b control} that exercises the same
    machinery on a supported shape, so a future change that breaks reactive
    views generally cannot make this file pass vacuously. *)

module Db = Granary.Db

let run = Lwt_main.run

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let exec_err db sql =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "expected an error from %S" sql
  | Error e -> Format.asprintf "%a" Db.pp_error e
;;

let contains ~needle haystack =
  let nl = String.length needle
  and hl = String.length haystack in
  let rec go i = i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1)) in
  go 0
;;

(* Read a one-integer-column result as a sorted int list, so an assertion does
   not depend on the materialisation's row order. *)
let ints db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    run (Lwt_stream.to_list stream)
    |> List.map (fun r ->
      match r.(0) with
      | Db.V_int v -> Int64.to_int v
      | _ -> Alcotest.failf "unexpected row shape for %S" sql)
    |> List.sort compare
;;

let with_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect ~finally:(fun () -> run (Db.close db)) (fun () -> f db)
;;

let seed db =
  exec db "CREATE TABLE t (a INTEGER)";
  exec db "CREATE TABLE u (b INTEGER)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO u VALUES (2)"
;;

let check_750 label msg =
  Alcotest.(check bool)
    (Printf.sprintf "%s: names the issue (got %S)" label msg)
    true
    (contains ~needle:"#750" msg);
  Alcotest.(check bool)
    (Printf.sprintf "%s: says the base tables are the problem (got %S)" label msg)
    true
    (contains ~needle:"statically" msg)
;;

(* ------------------------------------------------------------------ *)
(* The issue's own repro, and the control that proves it discriminates   *)
(* ------------------------------------------------------------------ *)

(* The repro. Before the fix this CREATE succeeded, [_rv_cv] read [1; 2], the
   INSERT was accepted, and [_rv_cv] still read [1; 2] — 99 silently missing. *)
let the_repro_is_refused_at_create () =
  with_db (fun db ->
    seed db;
    check_750
      "the repro"
      (exec_err db "CREATE REACTIVE VIEW cv AS SELECT a FROM t UNION SELECT b FROM u");
    (* Nothing was registered: no materialisation was left behind. *)
    (match run (Db.query db "SELECT * FROM _rv_cv") with
     | Ok _ -> Alcotest.fail "a materialisation was left behind for the refused view"
     | Error _ -> ()))
;;

(* THE CONTROL. The same seed, the same INSERT, a supported view shape — and it
   MUST track the write. Without this, a future change that simply broke
   reactive-view maintenance outright would make every refusal test above pass
   while the feature was dead. *)
let the_control_a_supported_view_still_tracks_writes () =
  with_db (fun db ->
    seed db;
    exec db "CREATE REACTIVE VIEW sv AS SELECT a FROM t";
    Alcotest.(check (list int)) "materialised at creation" [ 1 ] (ints db "SELECT a FROM _rv_sv");
    exec db "INSERT INTO t VALUES (99)";
    Alcotest.(check (list int))
      "and tracked the insert — the thing the compound view did not do"
      [ 1; 99 ]
      (ints db "SELECT a FROM _rv_sv"))
;;

(* ------------------------------------------------------------------ *)
(* All four set operations, and the data-independence of the verdict     *)
(* ------------------------------------------------------------------ *)

let every_set_operation_is_refused () =
  with_db (fun db ->
    seed db;
    check_750
      "UNION"
      (exec_err db "CREATE REACTIVE VIEW v AS SELECT a FROM t UNION SELECT b FROM u");
    check_750
      "UNION ALL"
      (exec_err db "CREATE REACTIVE VIEW v AS SELECT a FROM t UNION ALL SELECT b FROM u");
    check_750
      "INTERSECT"
      (exec_err db "CREATE REACTIVE VIEW v AS SELECT a FROM t INTERSECT SELECT b FROM u");
    check_750
      "EXCEPT"
      (exec_err db "CREATE REACTIVE VIEW v AS SELECT a FROM t EXCEPT SELECT b FROM u"))
;;

(* [Db.rv_create]'s arity-zero guard used to decide compound views by DATA: an
   empty compound was refused, a non-empty one accepted and left to go stale.
   That is #747's split one level up, and the static check removes it — the two
   spellings now give the identical answer. *)
let emptiness_is_not_the_criterion () =
  let empty =
    with_db (fun db ->
      exec db "CREATE TABLE t (a INTEGER)";
      exec db "CREATE TABLE u (b INTEGER)";
      exec_err db "CREATE REACTIVE VIEW v AS SELECT a FROM t UNION SELECT b FROM u")
  in
  let non_empty =
    with_db (fun db ->
      seed db;
      exec_err db "CREATE REACTIVE VIEW v AS SELECT a FROM t UNION SELECT b FROM u")
  in
  Alcotest.(check string) "the same refusal either way" empty non_empty;
  Alcotest.(check bool)
    (Printf.sprintf "and it is the static one, not the arity guard (got %S)" empty)
    true
    (contains ~needle:"#750" empty)
;;

(* ------------------------------------------------------------------ *)
(* Precedence among the three reactive-view refusals                     *)
(* ------------------------------------------------------------------ *)

(* The three checks run body-shape first, projection last:
   #486 (derived-table root) -> #750 (compound root) -> #747 (star).
   A user who hits an outer one cannot fix it by editing the inner one, so the
   outer message is the useful one. Pinned from every side that can differ. *)
let the_486_root_refusal_still_wins () =
  with_db (fun db ->
    seed db;
    let msg = exec_err db "CREATE REACTIVE VIEW v AS SELECT * FROM (SELECT a FROM t) d" in
    Alcotest.(check bool)
      (Printf.sprintf "a derived-table root is still #486 (got %S)" msg)
      true
      (contains ~needle:"#486" msg))
;;

let the_compound_refusal_wins_over_the_star_one () =
  with_db (fun db ->
    seed db;
    check_750
      "star in the left arm"
      (exec_err db "CREATE REACTIVE VIEW v AS SELECT * FROM t UNION SELECT b FROM u");
    check_750
      "star in the right arm"
      (exec_err db "CREATE REACTIVE VIEW v AS SELECT a FROM t UNION SELECT * FROM u"))
;;

(* A derived table inside a compound ARM leaves [S_compound] at the ROOT — the
   desugaring wraps the arm, not the whole statement — so the root check wins
   and the message is #750, not #486. *)
let a_derived_table_in_an_arm_is_still_the_compound_refusal () =
  with_db (fun db ->
    seed db;
    check_750
      "derived table in an arm"
      (exec_err
         db
         "CREATE REACTIVE VIEW v AS SELECT x FROM (SELECT a AS x FROM t) d UNION SELECT \
          b FROM u"))
;;

(* ------------------------------------------------------------------ *)
(* What is unaffected                                                    *)
(* ------------------------------------------------------------------ *)

(* The control the decision rests on: a PLAIN view over a compound is not
   maintained — its body is re-bound on every use — so it is untouched and
   reflects a later write. Confirmed, not assumed. *)
let a_plain_view_over_a_compound_is_unaffected () =
  with_db (fun db ->
    seed db;
    exec db "CREATE VIEW pv AS SELECT a FROM t UNION SELECT b FROM u";
    Alcotest.(check (list int)) "reads both arms" [ 1; 2 ] (ints db "SELECT * FROM pv");
    exec db "INSERT INTO t VALUES (99)";
    Alcotest.(check (list int))
      "and a plain view sees the new row, where the reactive one could not"
      [ 1; 2; 99 ]
      (ints db "SELECT * FROM pv"))
;;

(* #750 point 4: once compound roots are refused, what ELSE still reaches
   [base_tables_of]'s and [proj_of]'s shared [| _ -> None/[]] arm?  Exactly one
   constructor: [S_const_select], a FROM-less SELECT.  Both defaults are
   correct there rather than benign-looking — a FROM-less SELECT is backed by
   no table, so [[]] base tables is the truth and there is nothing that could
   go stale.  Recorded as behaviour so that a future change which routes real
   tables through [S_const_select] fails here. *)
let a_from_less_select_is_the_only_other_shape_and_is_sound () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "INSERT INTO t VALUES (1)";
    exec db "CREATE REACTIVE VIEW k AS SELECT 1";
    Alcotest.(check (list int)) "a constant view materialises" [ 1 ] (ints db "SELECT * FROM _rv_k");
    (* A write to an unrelated table cannot change it, so having no base table
       costs nothing here. *)
    exec db "INSERT INTO t VALUES (99)";
    Alcotest.(check (list int)) "and stays what it is" [ 1 ] (ints db "SELECT * FROM _rv_k");
    (* The one spelling that still reaches [Db.rv_create]'s arity-zero guard:
       a FROM-less star projects nothing at all, so there is no column list to
       determine and no table to determine it from. *)
    let msg = exec_err db "CREATE REACTIVE VIEW z AS SELECT *" in
    Alcotest.(check bool)
      (Printf.sprintf "the arity guard, not a Sema refusal (got %S)" msg)
      true
      (contains ~needle:"cannot determine the output columns" msg))
;;

let () =
  Alcotest.run
    "reactive_view_compound_750"
    [ ( "the repro and its control"
      , [ Alcotest.test_case
            "the repro is refused at CREATE"
            `Quick
            the_repro_is_refused_at_create
        ; Alcotest.test_case
            "CONTROL: a supported view still tracks writes"
            `Quick
            the_control_a_supported_view_still_tracks_writes
        ] )
    ; ( "coverage"
      , [ Alcotest.test_case
            "every set operation is refused"
            `Quick
            every_set_operation_is_refused
        ; Alcotest.test_case
            "emptiness is not the criterion"
            `Quick
            emptiness_is_not_the_criterion
        ] )
    ; ( "precedence among the three refusals"
      , [ Alcotest.test_case
            "a derived-table root is still #486"
            `Quick
            the_486_root_refusal_still_wins
        ; Alcotest.test_case
            "compound beats star"
            `Quick
            the_compound_refusal_wins_over_the_star_one
        ; Alcotest.test_case
            "a derived table in an arm is the compound refusal"
            `Quick
            a_derived_table_in_an_arm_is_still_the_compound_refusal
        ] )
    ; ( "the residual shape"
      , [ Alcotest.test_case
            "a FROM-less SELECT is the only other shape and is sound"
            `Quick
            a_from_less_select_is_the_only_other_shape_and_is_sound
        ] )
    ; ( "what is unaffected"
      , [ Alcotest.test_case
            "a plain view over a compound is unaffected"
            `Quick
            a_plain_view_over_a_compound_is_unaffected
        ] )
    ]
;;
