(** #627: a post-plan refusal must reach the caller as a structured
    {!Granary.Db.error}, never as a raw [Failure] escaping the [Ok]/[Error]
    contract.

    {1 The defect}

    Every refusal added by #558, #566 and #592 exists to turn a silently wrong
    answer into a loud one. They are spelled [Lwt.fail_with] (or a [failwith]
    inside an Lwt callback) at a point where the plan has already been built, so
    they arrive at [Db] as a {b rejected promise}, not as a synchronous
    exception.

    [Db.query_impl] matched only the synchronous spelling —

    {[
      match Sql.Exec.query ... with
      | exception Failure msg -> Lwt.return (Error (Runtime msg))
      | lwt_stream ->
        let* stream = lwt_stream in
        ...
    ]}

    — so the rejection sailed past that arm and out of [Db.query] itself. A
    caller writing [match Db.query db sql with Ok _ | Error _] got an unhandled
    exception; the CLI printed [Fatal error: exception Failure(...)] instead of
    [Error: runtime error: ...]. [Db.query_as_of] had the same hole in its inner
    handler.

    That shape of loudness is the wrong one for granary's target — one Mirage
    image per embedding application — and it is actively harmful to any harness
    that {i classifies} errors: the TPC-C driver retries on a refusal and must
    not treat one as an internal crash, but a raw [Failure] makes the two
    indistinguishable.

    {1 What is pinned here}

    For each refusal category, and across every public surface that wraps
    execution — [query], [query_with_stats], [query_as_of], [execute],
    [execute_change_count], and the prepared [run] / [iter] — the outcome is a
    structured error {b matched by constructor}, not by message text. The
    messages themselves are unchanged by #627 and stay pinned by
    [test_agg_subquery_558.ml] and [test_join_subquery_592.ml].

    {1 The residual}

    One refusal site is genuinely {i drain}-time: [stream_filter]'s per-row
    [Lwt_stream.filter_s] arm re-checks the substituted predicate for every row,
    long after [Db.query] has returned [Ok stream]. No entry-point handler can
    convert that into an [Error] — the result was already handed over. The
    helpers below therefore drain each stream and report an exception raised
    during the drain as its own outcome, so a category that moves from
    construction-time to drain-time shows up here rather than passing quietly. *)

module Db = Granary.Db
module H = Granary_store.History

let run = Lwt_main.run

(* What a public entry point actually did with a refusal. *)
type outcome =
  | Structured of Db.error (* the contract held *)
  | Escaped of exn (* #627: the bug — an exception, not an [Error] *)
  | Answered (* the refusal never fired at all *)

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
  | Error e -> Alcotest.failf "setup %S: %a" sql Db.pp_error e
;;

(* Draining is part of the observation: a refusal that has moved to pull time
   would return [Ok stream] and then raise, which is [Escaped], not [Answered]. *)
let drain stream =
  match run (Lwt_stream.to_list stream) with
  | _ -> Answered
  | exception e -> Escaped e
;;

let query_outcome db sql =
  match run (Db.query db sql) with
  | Error e -> Structured e
  | Ok stream -> drain stream
  | exception e -> Escaped e
;;

let query_with_stats_outcome db sql =
  match run (Db.query_with_stats db sql) with
  | Error e -> Structured e
  | Ok (stream, _stats) -> drain stream
  | exception e -> Escaped e
;;

let query_as_of_outcome db target sql =
  match run (Db.query_as_of db target sql) with
  | Error e -> Structured e
  | Ok stream -> drain stream
  | exception e -> Escaped e
;;

let execute_outcome db sql =
  match run (Db.execute db sql) with
  | Error e -> Structured e
  | Ok () -> Answered
  | exception e -> Escaped e
;;

let execute_change_count_outcome db sql =
  match run (Db.execute_change_count db sql) with
  | Error e -> Structured e
  | Ok _ -> Answered
  | exception e -> Escaped e
;;

(* The prepared surfaces. [prepare] itself only binds and plans, so a post-plan
   refusal must not appear there — if it did, [Structured] would still be the
   right answer and the assertion below would still hold. *)
let prepared_iter_outcome db sql =
  match run (Db.prepare db sql) with
  | Error e -> Structured e
  | exception e -> Escaped e
  | Ok st ->
    let o =
      match run (Db.iter st ~params:[]) with
      | Error e -> Structured e
      | Ok stream -> drain stream
      | exception e -> Escaped e
    in
    run (Db.finalize st);
    o
;;

let prepared_run_outcome db sql =
  match run (Db.prepare db sql) with
  | Error e -> Structured e
  | exception e -> Escaped e
  | Ok st ->
    let o =
      match run (Db.run st ~params:[]) with
      | Error e -> Structured e
      | Ok _ -> Answered
      | exception e -> Escaped e
    in
    run (Db.finalize st);
    o
;;

(* ------------------------------------------------------------------ *)
(* Assertions — by constructor, never by message text                   *)
(* ------------------------------------------------------------------ *)

let describe = function
  | Structured e -> Format.asprintf "Structured (%a)" Db.pp_error e
  | Escaped e -> Printf.sprintf "Escaped (%s)" (Printexc.to_string e)
  | Answered -> "Answered"
;;

(* The #627 assertion proper: the engine's own [Runtime] constructor. *)
let check_runtime ~label o =
  match o with
  | Structured (Db.Runtime _) -> ()
  | Structured _ ->
    Alcotest.failf "%s: structured, but not Db.Runtime — got %s" label (describe o)
  | Escaped (Failure _) ->
    Alcotest.failf
      "%s: #627 — the refusal escaped as a raw Failure instead of Db.Runtime (%s)"
      label
      (describe o)
  | Escaped _ ->
    Alcotest.failf "%s: the refusal escaped as an exception (%s)" label (describe o)
  | Answered -> Alcotest.failf "%s: the refusal did not fire at all" label
;;

(* ------------------------------------------------------------------ *)
(* The corpus                                                           *)
(* ------------------------------------------------------------------ *)

let seed db =
  (* #592 / #566 shapes. *)
  exec db "CREATE TABLE l (a INTEGER)";
  exec db "CREATE TABLE r (b INTEGER)";
  exec db "CREATE TABLE k (v INTEGER)";
  exec db "INSERT INTO l VALUES (1),(9)";
  exec db "INSERT INTO r VALUES (5)";
  exec db "INSERT INTO k VALUES (4)";
  (* #558 shapes. *)
  exec db "CREATE TABLE t (a INTEGER, x INTEGER)";
  exec db "CREATE TABLE u (a INTEGER)";
  exec db "INSERT INTO t VALUES (1,10),(1,20),(2,30)";
  exec db "INSERT INTO u VALUES (5),(6),(7)";
  (* Write targets, so the same refusals can be reached through a write op. *)
  exec db "CREATE TABLE cap1 (c INTEGER)";
  exec db "CREATE TABLE cap2 (c INTEGER, d INTEGER)"
;;

(* One row per refusal category: a label, the read spelling, and the write
   spelling that reaches the same site through [Op_insert_select].

   {b Two of these shapes were rewritten on 2026-08-06.} #627 was written in
   parallel with #635 and #615, and both of those turned a refusal into an
   answer:

   - the ALIASED self-join
     [FROM l AS x JOIN l AS y ON EXISTS (… v < x.a)] now RESOLVES — #635 moved
     the duplicate guard in [get_outer_scan_metas] from "two inputs share a
     table name" to "two inputs share a scope IDENTIFIER", and two aliases are
     two identifiers. The unaliased spelling below is what still trips that
     guard, and it is the spelling #592's own
     [self_join_is_refused_not_emptied] was rewritten to for the same reason;
   - a correlated subquery in an OUTER join's ON now RESOLVES — that was the
     whole of #615, which reopened #566 once #592 gave the join node a
     correlation source. The site is still a refusal site, so the category is
     kept, but it has to be reached with a reference that genuinely cannot be
     resolved: [l.a] under [FROM l AS x], where the alias has taken the table
     name out of scope (#635).

   Neither rewrite weakens the category. Both still enter execution with a
   planned statement and refuse from inside a stream, which is the only
   property #627 is about; what changed is which correlation is unresolvable,
   not whether an unresolvable one is refused. If a future change resolves
   these spellings too, replace them again rather than deleting the row — a
   surface that stops being covered must be noticed. *)
let categories =
  [ ( "#592/#635 unresolvable correlation (unaliased self-join)"
    , "SELECT l.a FROM l JOIN l ON EXISTS (SELECT 1 FROM k WHERE v < l.a)"
    , "INSERT INTO cap1 SELECT l.a FROM l JOIN l ON EXISTS (SELECT 1 FROM k WHERE v < \
       l.a)" )
  ; ( "#615/#635 unresolvable correlation in an outer join's ON"
    , "SELECT x.a, b FROM l AS x LEFT JOIN r ON b > (SELECT v FROM k WHERE v < l.a)"
    , "INSERT INTO cap2 SELECT x.a, b FROM l AS x LEFT JOIN r ON b > (SELECT v FROM k \
       WHERE v < l.a)" )
  ; ( "#558 ungrouped correlation beside an aggregate"
    , "SELECT a, COUNT(*) + (SELECT COUNT(*) FROM u WHERE u.a > t.x) FROM t GROUP BY a"
    , "INSERT INTO cap2 SELECT a, COUNT(*) + (SELECT COUNT(*) FROM u WHERE u.a > t.x) \
       FROM t GROUP BY a" )
  ; ( "#558 correlation with no GROUP BY"
    , "SELECT COUNT(*) + (SELECT COUNT(*) FROM u WHERE u.a > t.a) FROM t"
    , "INSERT INTO cap1 SELECT COUNT(*) + (SELECT COUNT(*) FROM u WHERE u.a > t.a) FROM t"
    )
  ]
;;

(* ------------------------------------------------------------------ *)
(* Db.query — the surface named in the issue                            *)
(* ------------------------------------------------------------------ *)

let query_surfaces_runtime () =
  with_db (fun db ->
    seed db;
    List.iter
      (fun (label, read_sql, _) ->
         check_runtime
           ~label:(Printf.sprintf "Db.query / %s" label)
           (query_outcome db read_sql))
      categories)
;;

let query_with_stats_surfaces_runtime () =
  with_db (fun db ->
    seed db;
    List.iter
      (fun (label, read_sql, _) ->
         check_runtime
           ~label:(Printf.sprintf "Db.query_with_stats / %s" label)
           (query_with_stats_outcome db read_sql))
      categories)
;;

(* ------------------------------------------------------------------ *)
(* Db.execute — the write surface                                       *)
(* ------------------------------------------------------------------ *)

let execute_surfaces_runtime () =
  with_db (fun db ->
    seed db;
    List.iter
      (fun (label, _, write_sql) ->
         check_runtime
           ~label:(Printf.sprintf "Db.execute / %s" label)
           (execute_outcome db write_sql))
      categories)
;;

let execute_change_count_surfaces_runtime () =
  with_db (fun db ->
    seed db;
    List.iter
      (fun (label, _, write_sql) ->
         check_runtime
           ~label:(Printf.sprintf "Db.execute_change_count / %s" label)
           (execute_change_count_outcome db write_sql))
      categories)
;;

(* A refused write must also leave nothing behind — the refusal is the whole
   point, so a partially-applied INSERT would be worse than the raw [Failure]. *)
let a_refused_write_stores_nothing () =
  with_db (fun db ->
    seed db;
    List.iter (fun (_, _, write_sql) -> ignore (execute_outcome db write_sql)) categories;
    List.iter
      (fun tbl ->
         match run (Db.query db (Printf.sprintf "SELECT COUNT(*) FROM %s" tbl)) with
         | Error e -> Alcotest.failf "counting %s: %a" tbl Db.pp_error e
         | Ok stream ->
           (match run (Lwt_stream.to_list stream) with
            | [ [| Db.V_int n |] ] ->
              Alcotest.(check int64)
                (Printf.sprintf "%s is empty after the refused writes" tbl)
                0L
                n
            | _ -> Alcotest.failf "unexpected COUNT(*) shape for %s" tbl))
      [ "cap1"; "cap2" ])
;;

(* ------------------------------------------------------------------ *)
(* The prepared-statement surfaces                                      *)
(* ------------------------------------------------------------------ *)

let prepared_iter_surfaces_runtime () =
  with_db (fun db ->
    seed db;
    List.iter
      (fun (label, read_sql, _) ->
         check_runtime
           ~label:(Printf.sprintf "Db.prepare+iter / %s" label)
           (prepared_iter_outcome db read_sql))
      categories)
;;

let prepared_run_surfaces_runtime () =
  with_db (fun db ->
    seed db;
    List.iter
      (fun (label, _, write_sql) ->
         check_runtime
           ~label:(Printf.sprintf "Db.prepare+run / %s" label)
           (prepared_run_outcome db write_sql))
      categories)
;;

(* ------------------------------------------------------------------ *)
(* Db.query_as_of — the second hole #627 closed                         *)
(* ------------------------------------------------------------------ *)

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  (try Unix.unlink (path ^ "-wal") with
   | _ -> ());
  try Unix.unlink (path ^ ".aslog") with
  | _ -> ()
;;

(* [query_as_of]'s inner handler converted only the synchronous [Failure]; the
   rejected-promise spelling reached its [fun exn -> ... Lwt.fail exn] arm and
   was re-raised, so a historical read of a refused query crashed the caller. *)
let query_as_of_surfaces_runtime () =
  let path = Printf.sprintf "/tmp/granary_test_refusal_627_%d.db" (Unix.getpid ()) in
  cleanup path;
  Fun.protect
    ~finally:(fun () -> cleanup path)
    (fun () ->
       let db =
         match run (Granary_unix.open_file ~as_of_history:true ~path ()) with
         | Ok db -> db
         | Error e -> Alcotest.failf "open_file: %a" Db.pp_error e
       in
       exec db "CREATE TABLE l (a INTEGER)";
       exec db "CREATE TABLE k (v INTEGER)";
       exec db "INSERT INTO l VALUES (1),(9)";
       exec db "INSERT INTO k VALUES (4)";
       let t1 =
         match List.rev (run (Db.history_log db)) with
         | last :: _ -> last.H.txn_id
         | [] -> Alcotest.fail "history log is empty after a commit"
       in
       exec db "INSERT INTO l VALUES (11)";
       Db.history_pin db ~txn_id:t1;
       let o =
         query_as_of_outcome
           db
           (`Txn t1)
           "SELECT l.a FROM l JOIN l ON EXISTS (SELECT 1 FROM k WHERE v < l.a)"
       in
       (try run (Db.close db) with
        | _ -> ());
       check_runtime ~label:"Db.query_as_of / #592 unresolvable correlation" o)
;;

(* ------------------------------------------------------------------ *)
(* Controls                                                             *)
(* ------------------------------------------------------------------ *)

(* The handler must not swallow anything that is not a refusal: a query with no
   refusal in it still answers, on every surface. *)
let ordinary_statements_are_unaffected () =
  with_db (fun db ->
    seed db;
    Alcotest.(check string)
      "a plain join still answers"
      "Answered"
      (describe (query_outcome db "SELECT a, b FROM l INNER JOIN r ON b > a"));
    Alcotest.(check string)
      "an uncorrelated subquery beside an aggregate still answers"
      "Answered"
      (describe
         (query_outcome
            db
            "SELECT a, COUNT(*) + (SELECT COUNT(*) FROM u) FROM t GROUP BY a"));
    Alcotest.(check string)
      "a plain INSERT ... SELECT still answers"
      "Answered"
      (describe (execute_outcome db "INSERT INTO cap1 SELECT a FROM l"));
    Alcotest.(check string)
      "a prepared SELECT still answers"
      "Answered"
      (describe (prepared_iter_outcome db "SELECT a FROM l")))
;;

(* A semantic error is still a [Sema], not flattened into [Runtime] — the fix
   must not turn every failure into the same constructor. *)
let other_error_constructors_are_untouched () =
  with_db (fun db ->
    seed db;
    (match query_outcome db "SELECT * FROM no_such_table" with
     | Structured (Db.Sema _) -> ()
     | o -> Alcotest.failf "unknown table should stay a Sema error — got %s" (describe o));
    match query_outcome db "SELEC 1" with
    | Structured (Db.Parse _) -> ()
    | o -> Alcotest.failf "a syntax error should stay a Parse error — got %s" (describe o))
;;

let () =
  Alcotest.run
    "refusal_error_627"
    [ ( "a post-plan refusal is a Db.error, not a raw Failure (#627)"
      , [ Alcotest.test_case "Db.query" `Quick query_surfaces_runtime
        ; Alcotest.test_case
            "Db.query_with_stats"
            `Quick
            query_with_stats_surfaces_runtime
        ; Alcotest.test_case "Db.query_as_of" `Quick query_as_of_surfaces_runtime
        ; Alcotest.test_case "Db.execute" `Quick execute_surfaces_runtime
        ; Alcotest.test_case
            "Db.execute_change_count"
            `Quick
            execute_change_count_surfaces_runtime
        ; Alcotest.test_case "prepared Db.iter" `Quick prepared_iter_surfaces_runtime
        ; Alcotest.test_case "prepared Db.run" `Quick prepared_run_surfaces_runtime
        ] )
    ; ( "the refusal still refuses"
      , [ Alcotest.test_case
            "a refused write stores nothing"
            `Quick
            a_refused_write_stores_nothing
        ] )
    ; ( "controls"
      , [ Alcotest.test_case
            "ordinary statements are unaffected"
            `Quick
            ordinary_statements_are_unaffected
        ; Alcotest.test_case
            "Parse and Sema constructors are untouched"
            `Quick
            other_error_constructors_are_untouched
        ] )
    ]
;;
