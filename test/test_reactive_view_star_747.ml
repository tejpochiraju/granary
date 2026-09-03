(** #747 — [CREATE REACTIVE VIEW ... AS SELECT *] is refused unconditionally.

    The guard used to be a runtime one in [Db.rv_create]: the view's arity was
    read off the first row of the initial result, and only an arity of zero — an
    EMPTY result — was refused. So a star view over a NON-empty table was
    accepted, froze its column list at creation time, and thereafter (a) dropped
    any column a later [ALTER TABLE ... ADD COLUMN] added, silently, and
    (b) fired on every write to the base table including writes idempotent for
    the columns it actually projects.

    The refusal is now static, in [Sema], and does not consult the data. Its
    message no longer mentions emptiness, because emptiness stopped being the
    criterion.

    A plain [CREATE VIEW] is deliberately untouched — it is not maintained, its
    body is re-bound on every use, so a star there picks up an added column the
    way a bare SELECT would. That is asserted here rather than assumed. *)

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

let rows db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream -> run (Lwt_stream.to_list stream)
;;

let with_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect ~finally:(fun () -> run (Db.close db)) (fun () -> f db)
;;

(* Every refusal in this file must carry the new reasoning and must NOT carry
   the old one — "empty result" was the whole defect. *)
let check_message label msg =
  Alcotest.(check bool)
    (Printf.sprintf "%s: names the issue (got %S)" label msg)
    true
    (contains ~needle:"#747" msg);
  Alcotest.(check bool)
    (Printf.sprintf "%s: asks for an explicit projection (got %S)" label msg)
    true
    (contains ~needle:"explicit projection" msg);
  Alcotest.(check bool)
    (Printf.sprintf "%s: no longer blames emptiness (got %S)" label msg)
    false
    (contains ~needle:"empty result" msg)
;;

(* The regression proper: this table has rows, so the old code accepted it. *)
let star_over_a_non_empty_table_is_refused () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    exec db "INSERT INTO t VALUES (2, 20)";
    let msg = exec_err db "CREATE REACTIVE VIEW v AS SELECT * FROM t" in
    check_message "non-empty" msg;
    (* And nothing was registered: the name is free, and no materialisation
       table was left behind. *)
    exec db "CREATE REACTIVE VIEW v AS SELECT a FROM t";
    Alcotest.(check int)
      "the explicit view materialised both rows"
      2
      (List.length (rows db "SELECT a FROM _rv_v")))
;;

(* The half that was already refused stays refused — with the new message. *)
let star_over_an_empty_table_is_still_refused () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
    let msg = exec_err db "CREATE REACTIVE VIEW v AS SELECT * FROM t" in
    check_message "empty" msg)
;;

(* The two spellings now give the SAME answer. That equality is the fix: the
   old behaviour made the verdict depend on whether a row happened to exist. *)
let both_halves_give_the_same_refusal () =
  let empty_msg =
    with_db (fun db ->
      exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
      exec_err db "CREATE REACTIVE VIEW v AS SELECT * FROM t")
  in
  let nonempty_msg =
    with_db (fun db ->
      exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
      exec db "INSERT INTO t VALUES (1, 10)";
      exec_err db "CREATE REACTIVE VIEW v AS SELECT * FROM t")
  in
  Alcotest.(check string) "emptiness is no longer the criterion" empty_msg nonempty_msg
;;

(* The refusal is about the view's own OUTPUT shape, so it follows a compound's
   arms — either side is enough. *)
let a_star_in_either_compound_arm_is_refused () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE TABLE u (a INTEGER)";
    exec db "INSERT INTO t VALUES (1)";
    exec db "INSERT INTO u VALUES (2)";
    check_message
      "left arm"
      (exec_err db "CREATE REACTIVE VIEW v AS SELECT * FROM t UNION SELECT a FROM u");
    check_message
      "right arm"
      (exec_err db "CREATE REACTIVE VIEW v AS SELECT a FROM t UNION SELECT * FROM u"))
;;

(* A star that does NOT determine the view's arity is not the target. A star
   under EXISTS is a row test; the view still projects exactly [a]. *)
let a_star_inside_a_subquery_is_not_the_target () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE TABLE u (a INTEGER)";
    exec db "INSERT INTO t VALUES (1)";
    exec db "INSERT INTO u VALUES (1)";
    exec db "CREATE REACTIVE VIEW v AS SELECT a FROM t WHERE EXISTS (SELECT * FROM u)";
    Alcotest.(check int)
      "the view materialised"
      1
      (List.length (rows db "SELECT a FROM _rv_v")))
;;

(* The derived-table refusal (#486) is the earlier, more specific one and keeps
   precedence — [test_from_list_derived_486] asserts the same statement names
   #486, so the two guards must not race. *)
let the_486_refusal_still_wins_over_the_747_one () =
  with_db (fun db ->
    exec db "CREATE TABLE a (x INTEGER)";
    let msg =
      exec_err db "CREATE REACTIVE VIEW rv AS SELECT * FROM (SELECT x FROM a) d"
    in
    Alcotest.(check bool)
      (Printf.sprintf "still the #486 message (got %S)" msg)
      true
      (contains ~needle:"#486" msg))
;;

(* An explicit projection is unaffected, including one that survives an
   ALTER TABLE ADD COLUMN on the base table: the view's shape was never in
   doubt, so the added column is simply not projected. *)
let an_explicit_projection_still_works_across_alter_table () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    exec db "CREATE REACTIVE VIEW v AS SELECT a, b FROM t";
    Alcotest.(check int)
      "two columns before"
      2
      (Array.length (List.hd (rows db "SELECT * FROM _rv_v")));
    exec db "ALTER TABLE t ADD COLUMN c INTEGER";
    exec db "INSERT INTO t VALUES (2, 20, 30)";
    let r = rows db "SELECT a, b FROM _rv_v" in
    Alcotest.(check int) "the view tracked the new row" 2 (List.length r);
    Alcotest.(check int)
      "and still has exactly its two declared columns"
      2
      (Array.length (List.hd (rows db "SELECT * FROM _rv_v"))))
;;

(* The control the issue's decision rests on: a PLAIN view is not maintained,
   so a star there is fine and picks up an added column on the next use.
   Confirmed, not assumed. *)
let a_plain_create_view_with_a_star_is_unaffected () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    exec db "CREATE VIEW pv AS SELECT * FROM t";
    Alcotest.(check int)
      "two columns"
      2
      (Array.length (List.hd (rows db "SELECT * FROM pv")));
    exec db "ALTER TABLE t ADD COLUMN c INTEGER";
    Alcotest.(check int)
      "the plain view widened with the base table"
      3
      (Array.length (List.hd (rows db "SELECT * FROM pv"))));
  (* And an empty plain star view is fine too — the case the reactive guard
     used to be about. *)
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER)";
    exec db "CREATE VIEW pv AS SELECT * FROM t";
    Alcotest.(check int) "no rows, no error" 0 (List.length (rows db "SELECT * FROM pv")))
;;

let () =
  Alcotest.run
    "reactive_view_star_747"
    [ ( "the refusal is unconditional"
      , [ Alcotest.test_case
            "star over a non-empty table is refused"
            `Quick
            star_over_a_non_empty_table_is_refused
        ; Alcotest.test_case
            "star over an empty table is still refused"
            `Quick
            star_over_an_empty_table_is_still_refused
        ; Alcotest.test_case
            "both halves give the same refusal"
            `Quick
            both_halves_give_the_same_refusal
        ] )
    ; ( "which star spellings it covers"
      , [ Alcotest.test_case
            "a star in either compound arm is refused"
            `Quick
            a_star_in_either_compound_arm_is_refused
        ; Alcotest.test_case
            "a star inside a subquery is not the target"
            `Quick
            a_star_inside_a_subquery_is_not_the_target
        ; Alcotest.test_case
            "the #486 refusal still wins"
            `Quick
            the_486_refusal_still_wins_over_the_747_one
        ] )
    ; ( "what is unaffected"
      , [ Alcotest.test_case
            "an explicit projection still works across ALTER TABLE"
            `Quick
            an_explicit_projection_still_works_across_alter_table
        ; Alcotest.test_case
            "a plain CREATE VIEW with a star is unaffected"
            `Quick
            a_plain_create_view_with_a_star_is_unaffected
        ] )
    ]
;;
