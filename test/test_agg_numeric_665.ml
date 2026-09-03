(** #665: #568's SUM/AVG numeric check now reaches an EXPRESSION argument.

    {1 The defect}

    #568 lifted the SUM/AVG non-numeric check onto [Sema.agg_col_ord], so that a
    bare [SUM(text_col)] and a wrapped [SUM(text_col) + 0] failed identically,
    at bind time. #488 then made an aggregate's argument a general expression —
    a third spelling, with no column ordinal at all. [agg_numeric_check] keys
    off a stored column's declared type, so it simply did not run on that arm,
    and

    {v SELECT SUM(CASE WHEN disc > 0.0 THEN 'a' ELSE 'b' END) FROM li; v}

    reached [failwith "SUM on non-numeric value"] mid-scan instead of failing at
    bind time — and, exactly like the defect #568 fixed, {b succeeded on an
    empty table}, because no row ever reached the accumulator.

    {1 The fix}

    [Sema.agg_arg_static_ty] infers the bound argument expression's type where
    it can, and hands it to the same [Sema.agg_numeric_ty_check] the ordinal
    spelling reaches through [agg_col_ord]. One verdict function, three
    spellings.

    It is deliberately not [Sema.infer_type]: that one indexes a
    [Row.column list] (the binder has only an ordinal lookup) and must not
    descend into [BE_case], because its other caller is
    [bind_update_assignments] where a CASE arm would newly reject
    [UPDATE t SET real_col = CASE WHEN c THEN 1 ELSE 2 END]. The CASE descent is
    the whole point here — it is the issue's own headline shape.

    {1 The boundary, pinned deliberately}

    An argument whose type is {i statically indeterminate} — a scalar function,
    a bound parameter, a CASE whose arms disagree — still reaches the runtime
    accumulator. That is the honest answer for it, and
    [indeterminate_argument_still_fails_at_runtime] pins it as a [Db.Runtime]
    error rather than letting a future change quietly turn it into a bind error
    or, worse, into a wrong answer.

    {1 Oracle}

    NOT oracle-checked, and it does not need to be: SQLite is untyped here and
    sums a TEXT column to [0.0] without complaint. Granary's strict column
    typing is a deliberate divergence recorded by #568; #665 only makes the
    third spelling of that divergence agree with the other two. *)

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
  | Db.V_real f -> Printf.sprintf "%g" f
  | Db.V_blob b -> Bytes.to_string b
  | Db.V_null -> "NULL"
;;

(** Run [sql] to completion, classifying the outcome as one of the three things
    #665 is about: a bind-time refusal, a runtime failure, or rows. *)
let outcome db sql =
  match run (Db.query db sql) with
  | Error (Db.Sema _) -> `Sema
  | Error (Db.Runtime _) -> `Runtime
  | Error e -> Alcotest.failf "query %S: unexpected error %a" sql Db.pp_error e
  | Ok stream ->
    (match
       run
         (Lwt.catch
            (fun () -> Lwt.map (fun r -> Ok r) (Lwt_stream.to_list stream))
            (fun ex -> Lwt.return (Error (Printexc.to_string ex))))
     with
     | Ok rows ->
       `Rows
         (List.map (fun r -> String.concat "," (Array.to_list (Array.map render r))) rows)
     | Error _ -> `Runtime)
;;

let pp_outcome = function
  | `Sema -> "sema"
  | `Runtime -> "runtime"
  | `Rows rs -> "rows[" ^ String.concat ";" rs ^ "]"
;;

let check_outcome ~label expected actual =
  Alcotest.(check string) label (pp_outcome expected) (pp_outcome actual)
;;

let seed db =
  exec db "CREATE TABLE li (id INTEGER PRIMARY KEY, qty INTEGER, disc REAL, s TEXT)";
  exec db "INSERT INTO li VALUES (1, 5, 0.5, 'x'), (2, 7, 0.0, 'y')";
  exec db "CREATE TABLE empty_li (id INTEGER PRIMARY KEY, qty INTEGER, disc REAL, s TEXT)"
;;

(* ------------------------------------------------------------------ *)

(** The issue's own repro. Before #665 this was [`Runtime]. *)
let case_expression_is_refused_at_bind_time () =
  with_db (fun db ->
    seed db;
    check_outcome
      ~label:"SUM over a CASE whose every arm is TEXT"
      `Sema
      (outcome db "SELECT SUM(CASE WHEN disc > 0.0 THEN 'a' ELSE 'b' END) FROM li"))
;;

(** The half a runtime check can never have: with no rows, the accumulator is
    never entered, so before #665 this SUCCEEDED and answered NULL. That is
    precisely the shape #568 was filed about, one spelling along. *)
let empty_table_is_refused_too () =
  with_db (fun db ->
    seed db;
    check_outcome
      ~label:"empty table, SUM over an all-TEXT CASE"
      `Sema
      (outcome db "SELECT SUM(CASE WHEN disc > 0.0 THEN 'a' ELSE 'b' END) FROM empty_li");
    check_outcome
      ~label:"empty table, SUM over a concatenation"
      `Sema
      (outcome db "SELECT SUM(s || 'x') FROM empty_li"))
;;

(** The other statically-typed shapes the issue names: a concatenation is TEXT
    whatever its operands are, and a CAST names its own type. *)
let statically_typed_expressions_are_refused () =
  with_db (fun db ->
    seed db;
    check_outcome ~label:"SUM(s || 'x')" `Sema (outcome db "SELECT SUM(s || 'x') FROM li");
    check_outcome ~label:"AVG(s || 'x')" `Sema (outcome db "SELECT AVG(s || 'x') FROM li");
    check_outcome
      ~label:"SUM over a concatenation of two numbers — still TEXT"
      `Sema
      (outcome db "SELECT SUM(qty || disc) FROM li");
    check_outcome
      ~label:"SUM(CAST(qty AS TEXT))"
      `Sema
      (outcome db "SELECT SUM(CAST(qty AS TEXT)) FROM li");
    check_outcome
      ~label:"AVG(CAST(qty AS TEXT))"
      `Sema
      (outcome db "SELECT AVG(CAST(qty AS TEXT)) FROM li");
    check_outcome
      ~label:"SUM over a negated TEXT column"
      `Sema
      (outcome db "SELECT SUM(-s) FROM li"))
;;

(** #568's whole point was that a pair of parentheses must not turn the check
    off. Every binder that can carry an aggregate routes through the same
    [bind_agg_arg], so every spelling must agree — including #491's DISTINCT
    argument, which is an argument like any other. *)
let every_spelling_agrees () =
  with_db (fun db ->
    seed db;
    check_outcome
      ~label:"wrapped in an expression"
      `Sema
      (outcome db "SELECT SUM(s || 'x') + 0 FROM li");
    check_outcome
      ~label:"GROUP BY projection"
      `Sema
      (outcome db "SELECT qty, SUM(s || 'x') FROM li GROUP BY qty");
    check_outcome
      ~label:"HAVING"
      `Sema
      (outcome db "SELECT qty FROM li GROUP BY qty HAVING SUM(s || 'x') > 0");
    check_outcome
      ~label:"ORDER BY over an aggregate"
      `Sema
      (outcome db "SELECT qty FROM li GROUP BY qty ORDER BY SUM(s || 'x')");
    check_outcome
      ~label:"DISTINCT argument"
      `Sema
      (outcome db "SELECT SUM(DISTINCT s || 'x') FROM li");
    (* The bare-column spelling #568 already covered, as the control. *)
    check_outcome
      ~label:"bare column, unchanged"
      `Sema
      (outcome db "SELECT SUM(s) FROM li"))
;;

(** The deliberate boundary. A CASE whose arms disagree, and a scalar function,
    have no static type; the check stays silent and the runtime accumulator is
    the backstop. It must still FAIL — a wrong answer here would be worse than
    the late error — and it must fail as a [Db.Runtime], not as a bind error, or
    the boundary has moved without anyone deciding to move it. *)
let indeterminate_argument_still_fails_at_runtime () =
  with_db (fun db ->
    seed db;
    check_outcome
      ~label:"CASE with disagreeing arms"
      `Runtime
      (outcome db "SELECT SUM(CASE WHEN qty > 6 THEN 'a' ELSE 1 END) FROM li");
    check_outcome
      ~label:"scalar function"
      `Runtime
      (outcome db "SELECT SUM(upper(s)) FROM li");
    (* And on an EMPTY table the same statement answers NULL, because nothing
       reaches the accumulator. This is the residual #665 does not close, and
       it is pinned so that closing it later is a visible change. *)
    check_outcome
      ~label:"indeterminate argument, empty table: still succeeds"
      (`Rows [ "NULL" ])
      (outcome db "SELECT SUM(upper(s)) FROM empty_li"))
;;

(** Negative controls: nothing numeric may be caught by the new check, in any of
    the shapes [agg_arg_static_ty] walks. A false positive here would be a
    working query newly refused. *)
let numeric_expression_arguments_still_work () =
  with_db (fun db ->
    seed db;
    check_outcome
      ~label:"integer arithmetic"
      (`Rows [ "24" ])
      (outcome db "SELECT SUM(qty * 2) FROM li");
    check_outcome
      ~label:"mixed arithmetic is REAL"
      (`Rows [ "12.5" ])
      (outcome db "SELECT SUM(qty + disc) FROM li");
    check_outcome
      ~label:"CASE with numeric arms"
      (`Rows [ "3" ])
      (outcome db "SELECT SUM(CASE WHEN qty > 6 THEN 1 ELSE 2 END) FROM li");
    check_outcome
      ~label:"CAST to a numeric type"
      (`Rows [ "12" ])
      (outcome db "SELECT SUM(CAST(qty AS INTEGER)) FROM li");
    check_outcome
      ~label:"a comparison is an INTEGER 0/1"
      (`Rows [ "1" ])
      (outcome db "SELECT SUM(qty > 6) FROM li");
    check_outcome
      ~label:"AVG over arithmetic"
      (`Rows [ "6" ])
      (outcome db "SELECT AVG(qty * 1.0) FROM li"))
;;

(** The check is SUM/AVG only. Every other aggregate consumes a TEXT argument
    legitimately, and #665 must not have widened the refusal to them. *)
let other_aggregates_are_untouched () =
  with_db (fun db ->
    seed db;
    check_outcome
      ~label:"COUNT over TEXT"
      (`Rows [ "2" ])
      (outcome db "SELECT COUNT(s || 'x') FROM li");
    check_outcome
      ~label:"MIN over TEXT"
      (`Rows [ "xx" ])
      (outcome db "SELECT MIN(s || 'x') FROM li");
    check_outcome
      ~label:"MAX over TEXT"
      (`Rows [ "yx" ])
      (outcome db "SELECT MAX(s || 'x') FROM li");
    check_outcome
      ~label:"GROUP_CONCAT over TEXT"
      (`Rows [ "xx,yx" ])
      (outcome db "SELECT GROUP_CONCAT(s || 'x') FROM li"))
;;

let () =
  Alcotest.run
    "agg numeric check over an expression argument (#665)"
    [ ( "665"
      , [ Alcotest.test_case
            "the issue's CASE repro is refused at bind time"
            `Quick
            case_expression_is_refused_at_bind_time
        ; Alcotest.test_case
            "an empty table is refused too"
            `Quick
            empty_table_is_refused_too
        ; Alcotest.test_case
            "every statically-typed non-numeric expression is refused"
            `Quick
            statically_typed_expressions_are_refused
        ; Alcotest.test_case
            "every spelling of the aggregate agrees"
            `Quick
            every_spelling_agrees
        ; Alcotest.test_case
            "a statically indeterminate argument still fails at runtime"
            `Quick
            indeterminate_argument_still_fails_at_runtime
        ; Alcotest.test_case
            "numeric expression arguments still work"
            `Quick
            numeric_expression_arguments_still_work
        ; Alcotest.test_case
            "COUNT/MIN/MAX/GROUP_CONCAT are untouched"
            `Quick
            other_aggregates_are_untouched
        ] )
    ]
;;
