(** #768: [ALTER TABLE ... RENAME TO] can silently defeat a pending deferred
    FK check.

    {1 The defect}

    [Exec.make_fk_recheck] identifies BOTH sides of a queued deferred FK
    obligation by TABLE NAME — [child_name] and [parent_name] are plain
    strings closed over at enqueue time — and at COMMIT resolves them with
    [Cat.find_table_cached], treating [None] as "not violated":

    {[
      match find_table_cached cat ~name:child_name,
            find_table_cached cat ~name:parent_name with
      | None, _ | _, None -> Lwt.return false
    ]}

    [ALTER TABLE ... RENAME TO] rewrites a table's catalog name with nothing
    keeping those captured strings in sync. So renaming either the child or
    the parent of a pending obligation, mid-transaction, made the recheck
    look up a name that no longer exists, answer "not violated", and let
    COMMIT accept a genuine violation. The child row survives under the new
    name as a permanent orphan and no FK error is ever raised.

    Both repros are the issue's own, verified 2026-09-05 against the
    post-#765 tree.

    {1 The fix}

    The same structural refusal round 3 of #765 gave [RENAME COLUMN] /
    [DROP COLUMN], one scope wider: [Exec.fk_obligation_table_conflict]
    resolves every pending check FRESH (via [Cat.peek_pending_fk_checks] and
    its [pfk_fk_ordinal], exactly as the recheck does at COMMIT) and the
    [Ast.AA_rename_table] arm of [Exec.execute_alter_table] refuses the
    rename outright while the obligation names this table on either side.
    Fixing the RECHECK side instead can never get ahead of the next mutation
    shape, because the recheck only ever sees the schema AFTER the mutation —
    #765's rounds 1, 2 and 3 are three consecutive demonstrations of that.

    [make_fk_recheck]'s by-ordinal resolution and loud failure stay in place
    as defence in depth; this test file pins the behaviour that is now
    observable, which is the refusal at the ALTER itself.

    {1 Not covered, deliberately}

    [DROP TABLE] is named in the issue as part of the same residual class and
    is NOT guarded here. Dropping the CHILD removes the referencing rows, so
    "nothing left to enforce" is the honest answer. Dropping the PARENT does
    leave a dangling reference — but it leaves the identical one with no
    transaction and no pending obligation at all, so that is a wider,
    pre-existing gap in [DROP TABLE] rather than the identity-desync bug this
    file is about. Pinned as such by
    {!drop_parent_table_is_unguarded_in_and_out_of_a_transaction} below, and
    tracked separately as #776. *)

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

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

(* Like [Db.execute], but an escaping non-[Failure] exception is reported as
   an [Error] string rather than aborting the binary — same helper, and same
   reason, as [test_fk_cross_numeric_755.ml]'s. *)
let exec_result db sql : (unit, string) result =
  try
    match run (Db.execute db sql) with
    | Ok () -> Ok ()
    | Error e -> Error (Format.asprintf "%a" Db.pp_error e)
  with
  | exn -> Error (Printf.sprintf "uncaught exception: %s" (Printexc.to_string exn))
;;

let query_texts db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun (r : Db.row) ->
         Array.to_list r
         |> List.map (function
           | Db.V_int n -> Int64.to_string n
           | Db.V_real f -> Printf.sprintf "%.17g" f
           | Db.V_text s -> s
           | Db.V_blob b -> Bytes.to_string b
           | Db.V_null -> "NULL")
         |> String.concat "|")
      (run (Lwt_stream.to_list stream))
;;

(* A query whose table must not exist. Both the planning error and any
   exception raised while draining the stream count as "absent"; the point is
   only that the name does not resolve. *)
let expect_absent ~what db sql =
  let outcome =
    try
      match run (Db.query db sql) with
      | Ok stream -> Ok (run (Lwt_stream.to_list stream))
      | Error e -> Error (Format.asprintf "%a" Db.pp_error e)
    with
    | exn -> Error (Printexc.to_string exn)
  in
  match outcome with
  | Ok _ -> Alcotest.failf "%s: %S resolved, but the table must not exist" what sql
  | Error _ -> ()
;;

let expect_refused ~what db sql =
  match exec_result db sql with
  | Ok () -> Alcotest.failf "%s: %S was expected to be refused, but succeeded" what sql
  | Error msg ->
    Alcotest.(check bool)
      (Printf.sprintf "%s refused with a clear conflict message (got %S)" what msg)
      true
      (contains ~needle:"deferred check still pending" msg);
    msg
;;

(* ------------------------------------------------------------------ *)
(* The issue's two repros                                              *)
(* ------------------------------------------------------------------ *)

(* Repro 1: rename the CHILD table while its own deferred check is pending.
   Before the fix this succeeded, COMMIT succeeded, and [SELECT pid FROM c2]
   returned the orphan 999 with no error ever raised. *)
let rename_child_table_with_pending_fk_is_refused () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c (pid INTEGER REFERENCES p(id) DEFERRABLE INITIALLY DEFERRED)";
    exec db "BEGIN";
    exec db "INSERT INTO c VALUES (999)";
    let msg =
      expect_refused ~what:"rename of the child table" db "ALTER TABLE c RENAME TO c2"
    in
    Alcotest.(check bool)
      (Printf.sprintf "the message names the child side and the parent (got %S)" msg)
      true
      (contains ~needle:"child side" msg && contains ~needle:"'p'" msg);
    exec db "ROLLBACK";
    (* The rename never happened: the old name still resolves and the new one
       does not exist. *)
    Alcotest.(check (list string))
      "the table kept its old name, with the rolled-back insert gone"
      []
      (query_texts db "SELECT pid FROM c");
    expect_absent
      ~what:"c2 must not exist: the rename was refused"
      db
      "SELECT pid FROM c2")
;;

(* Repro 2: rename the PARENT table instead. Identical silent-orphan effect
   before the fix, through the [parent_name] half of the same capture. *)
let rename_parent_table_with_pending_fk_is_refused () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p3 (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c3 (pid INTEGER REFERENCES p3(id) DEFERRABLE INITIALLY DEFERRED)";
    exec db "BEGIN";
    exec db "INSERT INTO c3 VALUES (999)";
    let msg =
      expect_refused ~what:"rename of the parent table" db "ALTER TABLE p3 RENAME TO p4"
    in
    Alcotest.(check bool)
      (Printf.sprintf "the message names the parent side and the child (got %S)" msg)
      true
      (contains ~needle:"is referenced by a FOREIGN KEY on 'c3'" msg);
    exec db "ROLLBACK";
    expect_absent
      ~what:"p4 must not exist: the rename was refused"
      db
      "SELECT id FROM p4")
;;

(* The control for both repros: with NO rename attempted, the very same
   transaction is refused at COMMIT. This is what makes the two tests above
   evidence of a silenced check rather than of a violation that was never
   queued -- the obligation is genuinely live, and before the fix the rename
   is all it took to make COMMIT forget it. *)
let commit_still_refuses_when_no_rename_is_attempted () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p5 (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c5 (pid INTEGER REFERENCES p5(id) DEFERRABLE INITIALLY DEFERRED)";
    exec db "BEGIN";
    exec db "INSERT INTO c5 VALUES (999)";
    match exec_result db "COMMIT" with
    | Ok () -> Alcotest.fail "COMMIT must refuse: parent row 999 does not exist"
    | Error msg ->
      Alcotest.(check bool)
        (Printf.sprintf "refused with a FOREIGN KEY error (got %S)" msg)
        true
        (contains ~needle:"FOREIGN KEY" msg))
;;

(* A self-referencing FK: child and parent are the SAME table, so the
   child-side branch of the guard is the one that must fire. Worth its own
   case because the two branches are an if/else-if — a guard that only
   checked the parent side would still pass repro 1, and vice versa. *)
let rename_self_referencing_table_with_pending_fk_is_refused () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec
      db
      "CREATE TABLE n (id INTEGER PRIMARY KEY, parent INTEGER REFERENCES n(id) \
       DEFERRABLE INITIALLY DEFERRED)";
    exec db "BEGIN";
    exec db "INSERT INTO n VALUES (1, 999)";
    ignore
      (expect_refused
         ~what:"rename of a self-referencing table"
         db
         "ALTER TABLE n RENAME TO n2");
    exec db "ROLLBACK")
;;

(* ------------------------------------------------------------------ *)
(* Negative tests: no over-refusal                                     *)
(* ------------------------------------------------------------------ *)

(* An UNRELATED table renamed inside the same transaction, while a genuine
   deferred obligation is pending on two other tables, must succeed. The
   check is keyed on the tables the obligation actually names, not on "any
   RENAME in a transaction that has any pending check". *)
let rename_of_an_unrelated_table_still_succeeds () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p6 (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c6 (pid INTEGER REFERENCES p6(id) DEFERRABLE INITIALLY DEFERRED)";
    exec db "CREATE TABLE unrelated (v INTEGER)";
    exec db "BEGIN";
    exec db "INSERT INTO c6 VALUES (999)";
    exec db "INSERT INTO unrelated VALUES (7)";
    (match exec_result db "ALTER TABLE unrelated RENAME TO unrelated2" with
     | Ok () -> ()
     | Error msg ->
       Alcotest.failf "renaming an unrelated table must not be refused, got %S" msg);
    exec db "INSERT INTO p6 VALUES (999)";
    (match exec_result db "COMMIT" with
     | Ok () -> ()
     | Error msg -> Alcotest.failf "COMMIT should have succeeded, got %S" msg);
    Alcotest.(check (list string))
      "the unrelated table really was renamed"
      [ "7" ]
      (query_texts db "SELECT v FROM unrelated2"))
;;

(* The child table of a DEFERRABLE FK renamed inside a transaction that has
   NO pending obligation at all (the inserted reference is satisfied, so
   nothing was ever queued) must succeed, and the rows must be readable
   under the new name after COMMIT. This is the case a blunt "refuse any
   rename of an FK-bearing table inside a transaction" guard would have
   broken. *)
let rename_child_table_with_no_pending_obligation_succeeds () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p7 (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c7 (pid INTEGER REFERENCES p7(id) DEFERRABLE INITIALLY DEFERRED)";
    exec db "INSERT INTO p7 VALUES (42)";
    exec db "BEGIN";
    (* Satisfied on the spot: no deferred violation is queued. *)
    exec db "INSERT INTO c7 VALUES (42)";
    (match exec_result db "ALTER TABLE c7 RENAME TO c7b" with
     | Ok () -> ()
     | Error msg ->
       Alcotest.failf
         "a rename with no pending obligation must not be refused, got %S"
         msg);
    (match exec_result db "COMMIT" with
     | Ok () -> ()
     | Error msg -> Alcotest.failf "COMMIT should have succeeded, got %S" msg);
    Alcotest.(check (list string))
      "the child rows are readable under the new name"
      [ "42" ]
      (query_texts db "SELECT pid FROM c7b"))
;;

(* The same rename with no transaction at all: an autocommit
   [ALTER TABLE ... RENAME TO] on a table that has FK constraints is
   untouched by the guard, because the pending queue is empty outside a
   transaction. *)
let autocommit_rename_of_an_fk_table_still_succeeds () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p12 (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c12 (pid INTEGER REFERENCES p12(id) DEFERRABLE INITIALLY DEFERRED)";
    exec db "INSERT INTO p12 VALUES (5)";
    exec db "INSERT INTO c12 VALUES (5)";
    exec db "ALTER TABLE c12 RENAME TO c12b";
    exec db "ALTER TABLE p12 RENAME TO p12b";
    Alcotest.(check (list string))
      "both tables renamed, rows intact"
      [ "5" ]
      (query_texts db "SELECT pid FROM c12b"))
;;

(* A ROLLBACK clears the pending queue, so a rename in a LATER transaction is
   unaffected by an obligation that was live in an earlier one. Pins that the
   guard reads the live queue rather than any sticky per-table state. *)
let rename_after_rollback_of_the_pending_transaction_succeeds () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    exec db "CREATE TABLE p13 (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c13 (pid INTEGER REFERENCES p13(id) DEFERRABLE INITIALLY DEFERRED)";
    exec db "BEGIN";
    exec db "INSERT INTO c13 VALUES (999)";
    ignore
      (expect_refused ~what:"rename while pending" db "ALTER TABLE c13 RENAME TO c13b");
    exec db "ROLLBACK";
    (* Nothing pending any more. *)
    exec db "BEGIN";
    (match exec_result db "ALTER TABLE c13 RENAME TO c13b" with
     | Ok () -> ()
     | Error msg ->
       Alcotest.failf
         "the queue is empty after ROLLBACK; rename must succeed, got %S"
         msg);
    (match exec_result db "COMMIT" with
     | Ok () -> ()
     | Error msg -> Alcotest.failf "COMMIT should have succeeded, got %S" msg);
    Alcotest.(check (list string))
      "the renamed table exists and is empty"
      []
      (query_texts db "SELECT pid FROM c13b"))
;;

(* ------------------------------------------------------------------ *)
(* The scoped-out neighbour, pinned rather than assumed                 *)
(* ------------------------------------------------------------------ *)

(* [DROP TABLE] is NOT guarded, and this pins the reasoning that keeps it out
   of scope rather than leaving it to a comment. Dropping the CHILD removes
   the referencing rows outright — "nothing left to enforce" is honest.
   Dropping the PARENT leaves a dangling reference, but leaves the IDENTICAL
   dangling reference outside a transaction with no obligation involved, so
   it is a wider pre-existing [DROP TABLE] gap (#776) and not the identity
   desync #768 is about. If either half of this ever changes, this test
   fails and the decision gets re-made deliberately. *)
let drop_parent_table_is_unguarded_in_and_out_of_a_transaction () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = 1";
    (* (a) autocommit, no obligation anywhere: dropping a referenced parent
       already leaves the child row behind, unchecked. *)
    exec db "CREATE TABLE pa (id INTEGER PRIMARY KEY)";
    exec db "CREATE TABLE ca (pid INTEGER REFERENCES pa(id))";
    exec db "INSERT INTO pa VALUES (1)";
    exec db "INSERT INTO ca VALUES (1)";
    (match exec_result db "DROP TABLE pa" with
     | Ok () -> ()
     | Error msg ->
       Alcotest.failf
         "baseline changed: autocommit DROP TABLE of a referenced parent now fails (%S) \
          — re-decide #776"
         msg);
    Alcotest.(check (list string))
      "the child row outlives its parent table outside any transaction"
      [ "1" ]
      (query_texts db "SELECT pid FROM ca");
    (* (b) the child side, mid-transaction, with an obligation pending: the
       rows go with the table, so there is nothing left to enforce. *)
    exec db "CREATE TABLE pb (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE cb (pid INTEGER REFERENCES pb(id) DEFERRABLE INITIALLY DEFERRED)";
    exec db "BEGIN";
    exec db "INSERT INTO cb VALUES (999)";
    (match exec_result db "DROP TABLE cb" with
     | Ok () -> ()
     | Error msg ->
       Alcotest.failf "DROP TABLE of the child must stay allowed, got %S" msg);
    (match exec_result db "COMMIT" with
     | Ok () -> ()
     | Error msg -> Alcotest.failf "COMMIT should have succeeded, got %S" msg))
;;

let () =
  Alcotest.run
    "fk_rename_table_768"
    [ ( "rename_to_defeating_a_pending_deferred_check"
      , [ Alcotest.test_case
            "renaming the CHILD table is refused"
            `Quick
            rename_child_table_with_pending_fk_is_refused
        ; Alcotest.test_case
            "renaming the PARENT table is refused"
            `Quick
            rename_parent_table_with_pending_fk_is_refused
        ; Alcotest.test_case
            "control: COMMIT still refuses when no rename is attempted"
            `Quick
            commit_still_refuses_when_no_rename_is_attempted
        ; Alcotest.test_case
            "renaming a self-referencing table is refused"
            `Quick
            rename_self_referencing_table_with_pending_fk_is_refused
        ] )
    ; ( "no_over_refusal"
      , [ Alcotest.test_case
            "an unrelated table still renames inside the same transaction"
            `Quick
            rename_of_an_unrelated_table_still_succeeds
        ; Alcotest.test_case
            "a rename with no pending obligation is unaffected"
            `Quick
            rename_child_table_with_no_pending_obligation_succeeds
        ; Alcotest.test_case
            "an autocommit rename of an FK table is unaffected"
            `Quick
            autocommit_rename_of_an_fk_table_still_succeeds
        ; Alcotest.test_case
            "a rename after ROLLBACK of the pending transaction succeeds"
            `Quick
            rename_after_rollback_of_the_pending_transaction_succeeds
        ] )
    ; ( "scoped_out_neighbour"
      , [ Alcotest.test_case
            "DROP TABLE stays unguarded, in and out of a transaction"
            `Quick
            drop_parent_table_is_unguarded_in_and_out_of_a_transaction
        ] )
    ]
;;
