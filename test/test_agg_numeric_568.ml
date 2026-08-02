(** #568: the SUM/AVG non-numeric argument check must not depend on how the
    aggregate is spelled.

    [Sema.project_agg] — the binder for a {e bare} aggregate projection item —
    ran its own [validate_numeric] and rejected [SUM(text_col)] at bind time.
    Every other binder resolves an aggregate through [Sema.agg_col_ord], which
    did not type-check, so wrapping the same aggregate in an expression
    ([SUM(s) + 0]) or putting it in HAVING turned the check off. #507 widened
    the reach of that untyped path from HAVING into the projection.

    Two things were wrong, not one. The error arrived {e later} — at runtime
    instead of bind time — and for an empty group it did not arrive {e at all},
    because the runtime accumulator only sees rows that exist. So the same
    query succeeded or failed depending on the data.

    Granary's strict column typing is a deliberate divergence from SQLite (see
    CLAUDE.md, "inspired by, not a port"), which is exactly why the check has
    to be one a pair of parentheses cannot remove. The fix moves it into
    [agg_col_ord], the single ordinal-resolution path every binder shares;
    [project_agg] now delegates there rather than carrying a duplicate. *)

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

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

(* Rejected at BIND time: [Db.query] returns the error before any row is read,
   so the failure cannot depend on the data.  A runtime failure would surface
   while draining the stream instead, which is what this pins against. *)
let rejected_at_bind_time db sql =
  run
    (let* r = Db.query db sql in
     match r with
     | Ok _ -> Alcotest.failf "expected a bind-time type error for %S" sql
     | Error e ->
       let msg = Format.asprintf "%a" Db.pp_error e in
       Alcotest.(check bool)
         (Printf.sprintf "%S rejected as a type mismatch (got %S)" sql msg)
         true
         (contains ~needle:"type mismatch" msg);
       Lwt.return_unit)
;;

let rows db sql =
  run
    (let* r = Db.query db sql in
     match r with
     | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
     | Ok stream -> Lwt_stream.to_list stream)
;;

let show_value = function
  | Row.V_text s -> Printf.sprintf "V_text %S" s
  | Row.V_null -> "V_null"
  | Row.V_int n -> Printf.sprintf "V_int %Ld" n
  | Row.V_real f -> Printf.sprintf "V_real %g" f
  | Row.V_blob b -> Printf.sprintf "V_blob(%d)" (Bytes.length b)
;;

let seed db =
  exec db "CREATE TABLE t (a INTEGER, s TEXT, b BLOB, r REAL)";
  exec db "INSERT INTO t VALUES (1, 'abc', x'00', 1.5)"
;;

(* The bare spelling was already checked; it must stay checked. *)
let bare_aggregate_still_rejected () =
  with_db (fun db ->
    seed db;
    rejected_at_bind_time db "SELECT SUM(s) FROM t GROUP BY a";
    rejected_at_bind_time db "SELECT AVG(s) FROM t GROUP BY a")
;;

(* The reported case: a pair of parentheses used to turn the check off. *)
let aggregate_in_an_expression_now_rejected () =
  with_db (fun db ->
    seed db;
    rejected_at_bind_time db "SELECT SUM(s) + 0 FROM t GROUP BY a";
    rejected_at_bind_time db "SELECT AVG(s) * 2 FROM t GROUP BY a";
    rejected_at_bind_time db "SELECT a, SUM(a) + SUM(s) FROM t GROUP BY a")
;;

(* The sharper half of the report: the runtime check only fired on a row it
   actually saw, so an empty group succeeded and returned nothing.  A bind-time
   check cannot be data-dependent. *)
let empty_result_is_rejected_too () =
  with_db (fun db ->
    seed db;
    rejected_at_bind_time db "SELECT SUM(s) FROM t WHERE a = 99 GROUP BY a";
    rejected_at_bind_time db "SELECT SUM(s) + 0 FROM t WHERE a = 99 GROUP BY a")
;;

(* HAVING has always used the untyped path. *)
let having_aggregate_rejected () =
  with_db (fun db ->
    seed db;
    rejected_at_bind_time db "SELECT a FROM t GROUP BY a HAVING SUM(s) > 0";
    rejected_at_bind_time db "SELECT a FROM t GROUP BY a HAVING AVG(s) + 1 > 0")
;;

(* Qualified argument spelling resolves through the other resolver field. *)
let qualified_argument_rejected () =
  with_db (fun db ->
    seed db;
    rejected_at_bind_time db "SELECT SUM(t.s) + 0 FROM t GROUP BY t.a")
;;

(* BLOB as well as TEXT. *)
let blob_argument_rejected () =
  with_db (fun db ->
    seed db;
    rejected_at_bind_time db "SELECT SUM(b) + 0 FROM t GROUP BY a")
;;

(* Ungrouped aggregation (no GROUP BY) goes through the same binder. *)
let ungrouped_aggregate_rejected () =
  with_db (fun db ->
    seed db;
    rejected_at_bind_time db "SELECT SUM(s) FROM t";
    rejected_at_bind_time db "SELECT SUM(s) + 0 FROM t")
;;

(* What must keep working: numeric arguments in both spellings, and the
   aggregates that legitimately take anything. *)
let numeric_and_untyped_aggregates_unaffected () =
  with_db (fun db ->
    seed db;
    exec db "INSERT INTO t VALUES (1, 'def', x'01', 2.5)";
    let one sql =
      match rows db sql with
      | [ r ] -> r
      | l -> Alcotest.failf "%S: expected 1 row, got %d" sql (List.length l)
    in
    (match (one "SELECT SUM(a) + 0 FROM t GROUP BY a").(0) with
     | Row.V_int 2L -> ()
     | v -> Alcotest.failf "SUM(a) + 0 = %s" (show_value v));
    (match (one "SELECT AVG(r) FROM t GROUP BY a").(0) with
     | Row.V_real f when Float.abs (f -. 2.0) < 1e-9 -> ()
     | v -> Alcotest.failf "AVG(r) = %s" (show_value v));
    (* COUNT / MIN / MAX over TEXT are not numeric aggregates and must stay
       legal in both spellings. *)
    (match (one "SELECT COUNT(s) + 0 FROM t GROUP BY a").(0) with
     | Row.V_int 2L -> ()
     | v -> Alcotest.failf "COUNT(s) + 0 = %s" (show_value v));
    (match (one "SELECT MIN(s) FROM t GROUP BY a").(0) with
     | Row.V_text "abc" -> ()
     | v -> Alcotest.failf "MIN(s) = %s" (show_value v));
    (match (one "SELECT MAX(s) FROM t GROUP BY a").(0) with
     | Row.V_text "def" -> ()
     | v -> Alcotest.failf "MAX(s) = %s" (show_value v));
    (* COUNT-star has no argument at all. *)
    match (one "SELECT COUNT(*) * 2 FROM t GROUP BY a").(0) with
    | Row.V_int 4L -> ()
    | v -> Alcotest.failf "COUNT(*) * 2 = %s" (show_value v))
;;

(* ------------------------------------------------------------------ *)
(* Property: the two spellings agree, for every aggregate and column    *)
(* ------------------------------------------------------------------ *)

let agg_gen = QCheck2.Gen.oneof_list [ "SUM"; "AVG"; "COUNT"; "MIN"; "MAX" ]
let col_gen = QCheck2.Gen.oneof_list [ "a"; "s"; "b"; "r" ]

(* Four spellings of the same aggregate call: bare, parenthesised, inside a
   CASE, and in HAVING.  All four are type-NEUTRAL about the aggregate's result
   — deliberately.  The obvious fourth shell, [%s + 0], is the issue's own
   repro and is covered by the unit tests above, but it cannot be used here:
   arithmetic imposes its own operand typing, so [MIN(text) + 0] is refused for
   a reason that has nothing to do with #568 and the shells would legitimately
   disagree. *)
let shells =
  [ (fun call -> Printf.sprintf "SELECT %s FROM t GROUP BY a" call)
  ; (fun call -> Printf.sprintf "SELECT (%s) FROM t GROUP BY a" call)
  ; (fun call ->
      Printf.sprintf "SELECT CASE WHEN 1 THEN %s ELSE NULL END FROM t GROUP BY a" call)
  ; (fun call -> Printf.sprintf "SELECT a FROM t GROUP BY a HAVING %s IS NOT NULL" call)
  ]
;;

(* Whether a query BINDS must depend only on the aggregate and the column type,
   never on the syntactic shell the call sits in.  That equivalence is the whole
   defect: before the fix, wrapping [SUM(s)] in an expression flipped the answer
   from "rejected" to "accepted".  Only the bind is compared — what a later
   [MIN(blob) + 0] does at evaluation time is a different question with its own
   typing rules, and draining would conflate the two. *)
let spelling_does_not_change_acceptance =
  QCheck2.Test.make
    ~count:200
    ~name:"#568: an aggregate binds the same in every spelling"
    ~print:(fun (agg, col) -> Printf.sprintf "%s(%s)" agg col)
    QCheck2.Gen.(pair agg_gen col_gen)
    (fun (agg, col) ->
       let db = run (Db.open_in_memory ()) in
       Fun.protect
         ~finally:(fun () ->
           try run (Db.close db) with
           | _ -> ())
         (fun () ->
            seed db;
            let call = Printf.sprintf "%s(%s)" agg col in
            let binds sql =
              run
                (let* r = Db.query db sql in
                 match r with
                 | Ok _ -> Lwt.return true
                 | Error _ -> Lwt.return false)
            in
            let outcomes = List.map (fun shell -> binds (shell call)) shells in
            let all_agree =
              match outcomes with
              | [] -> true
              | first :: rest -> List.for_all (fun o -> o = first) rest
            in
            (* And for SUM/AVG that shared answer is exactly "the column is
               numeric" — otherwise "they all agree" could be satisfied by the
               check having been dropped everywhere. *)
            let numeric = col = "a" || col = "r" in
            let sum_or_avg = agg = "SUM" || agg = "AVG" in
            all_agree && ((not sum_or_avg) || List.for_all (fun o -> o = numeric) outcomes)))
;;

let () =
  Alcotest.run
    "agg_numeric_568"
    [ ( "rejected"
      , [ Alcotest.test_case "bare aggregate" `Quick bare_aggregate_still_rejected
        ; Alcotest.test_case
            "wrapped in an expression"
            `Quick
            aggregate_in_an_expression_now_rejected
        ; Alcotest.test_case "empty result" `Quick empty_result_is_rejected_too
        ; Alcotest.test_case "HAVING" `Quick having_aggregate_rejected
        ; Alcotest.test_case "qualified argument" `Quick qualified_argument_rejected
        ; Alcotest.test_case "BLOB argument" `Quick blob_argument_rejected
        ; Alcotest.test_case "no GROUP BY" `Quick ungrouped_aggregate_rejected
        ] )
    ; ( "unchanged"
      , [ Alcotest.test_case
            "numeric and untyped aggregates"
            `Quick
            numeric_and_untyped_aggregates_unaffected
        ] )
    ; ( "property"
      , List.map QCheck_alcotest.to_alcotest [ spelling_does_not_change_acceptance ] )
    ]
;;
