(** #740: [PRAGMA wal_checkpoint] issued inside an explicit transaction used to
    SELF-DEADLOCK the connection.

    [BEGIN] -> [Store.rw_begin] takes the store's writer lock and holds it for
    the whole transaction; only COMMIT/ROLLBACK release it. [Store.checkpoint]
    takes that same lock, and [Rwlock] is deliberately not re-entrant
    ([rwlock.mli]: "a fiber that holds the writer lock must not call
    [acquire_write] again"), so the fiber parked on itself and every later
    statement on the handle was unreachable.

    Pre-existing, not a #719 regression. Before the phase split the checkpoint
    parked on the lock it took first; since #719 it parks at [ckpt_finish]'s
    single [acquire_writer], having already done its (harmless) unlocked
    migration — and, worse, holding [st.ckpt_mutex], so every later checkpoint
    on the store queues behind the wedged one instead of latching on
    [autockpt_in_flight]. Either way the connection was unusable.

    The fix refuses the statement up front, the same shape as #473's
    [DROP REACTIVE VIEW] refusal and #598's routing refusals. It is not a
    divergence: sqlite3 declines the same statement (oracle-checked 2026-09-03,
    "Runtime error ...: database table is locked (6)"), and in both engines the
    caller's transaction survives the refusal intact.

    {1 Why these tests are file-backed and WAL-mode}

    The Mem backend has no WAL: [Store.checkpoint] returns before it acquires
    anything, so an in-memory version of every test below passes while proving
    nothing — there is no second acquisition to deadlock on.

    {1 The two spellings}

    A one-shot [Db.execute] reaches [Db.execute_control_op]; a PREPARED
    statement does not — [run_core] hands [st.plan] straight to
    [Sql.Exec.execute_with_count]. Both are pinned, because a guard on one alone
    leaves the other deadlocking. *)

module Db = struct
  include Granary.Db

  let open_file_wal = Granary_unix.open_file_wal
end

let () = Granary_unix.install ()
let run = Lwt_main.run

(* Sibling worktrees run suites concurrently, so the path carries the pid — the
   convention in test_lock_stats_718.ml and test_autocheckpoint_lock_719.ml. *)
let path_counter = ref 0

let with_wal_db f =
  let n = !path_counter in
  incr path_counter;
  let path = Printf.sprintf "/tmp/granary_wal_ckpt_txn_740_%d_%d.db" (Unix.getpid ()) n in
  let cleanup () =
    List.iter
      (fun p ->
         try Unix.unlink p with
         | _ -> ())
      [ path; path ^ "-wal" ]
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () ->
    match run (Db.open_file_wal ~path ()) with
    | Ok db -> Fun.protect ~finally:(fun () -> run (Db.close db)) (fun () -> f db)
    | Error e -> Alcotest.failf "open_file_wal %S: %a" path Db.pp_error e)
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "error in %S: %a" sql Db.pp_error e
;;

let err_msg db sql =
  match run (Db.execute db sql) with
  | Ok () -> None
  | Error (Db.Runtime m) -> Some m
  | Error e -> Some (Format.asprintf "%a" Db.pp_error e)
;;

(* Substring test, so the file needs no [str] dependency. *)
let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let count db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    (match run (Lwt_stream.to_list stream) with
     | [ [| Db.V_int n |] ] -> Int64.to_int n
     | _ -> Alcotest.failf "query %S: unexpected shape" sql)
;;

let seed db =
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let rec go k =
    if k > 40
    then ()
    else (
      exec db (Printf.sprintf "INSERT INTO t VALUES (%d, 'v%d')" k k);
      go (k + 1))
  in
  go 1
;;

let refusal msg =
  Alcotest.(check bool)
    "refusal names the PRAGMA"
    true
    (contains ~needle:"wal_checkpoint" msg);
  Alcotest.(check bool) "refusal names the issue" true (contains ~needle:"#740" msg);
  Alcotest.(check bool) "refusal names the way out" true (contains ~needle:"ROLLBACK" msg)
;;

(* ------------------------------------------------------------------ *)
(* 1. The headline: refused, not deadlocked.                            *)
(* ------------------------------------------------------------------ *)

(* On the unfixed code this test does not fail — it HANGS, which is the whole
   point of the issue. Reaching the assertion at all is half the result. *)
let refused_inside_an_explicit_transaction () =
  with_wal_db (fun db ->
    seed db;
    exec db "BEGIN";
    exec db "INSERT INTO t VALUES (100, 'in-txn')";
    (match err_msg db "PRAGMA wal_checkpoint" with
     | None -> Alcotest.fail "PRAGMA wal_checkpoint was accepted inside a transaction"
     | Some m -> refusal m);
    (* The refusal costs the caller nothing: the transaction is intact and
       commits, exactly as sqlite3's SQLITE_LOCKED leaves it. *)
    exec db "COMMIT";
    Alcotest.(check int)
      "the transaction survived the refusal"
      41
      (count db "SELECT COUNT(*) FROM t"))
;;

(* ------------------------------------------------------------------ *)
(* 2. A SAVEPOINT's auto-begin opens the same slot.                     *)
(* ------------------------------------------------------------------ *)

(* [savepoint_txn] auto-begins a transaction when none is open, so a bare
   [SAVEPOINT] holds the writer lock just as [BEGIN] does. A guard written
   against the BEGIN spelling alone would leave this one deadlocking. *)
let refused_under_a_bare_savepoint () =
  with_wal_db (fun db ->
    seed db;
    exec db "SAVEPOINT s1";
    (match err_msg db "PRAGMA wal_checkpoint" with
     | None -> Alcotest.fail "PRAGMA wal_checkpoint was accepted under a SAVEPOINT"
     | Some m -> refusal m);
    exec db "RELEASE s1")
;;

(* ------------------------------------------------------------------ *)
(* 3. The prepared spelling bypasses execute_control_op.                *)
(* ------------------------------------------------------------------ *)

let refused_when_prepared () =
  with_wal_db (fun db ->
    seed db;
    exec db "BEGIN";
    let st =
      match run (Db.prepare db "PRAGMA wal_checkpoint") with
      | Ok st -> st
      | Error e -> Alcotest.failf "prepare: %a" Db.pp_error e
    in
    (match run (Db.run st ~params:[]) with
     | Ok _ -> Alcotest.fail "a prepared PRAGMA wal_checkpoint ran inside a transaction"
     | Error (Db.Runtime m) -> refusal m
     | Error e -> Alcotest.failf "unexpected error: %a" Db.pp_error e);
    exec db "ROLLBACK";
    (* Outside the transaction the same prepared statement is fine. *)
    (match run (Db.run st ~params:[]) with
     | Ok _ -> ()
     | Error e -> Alcotest.failf "prepared checkpoint in autocommit: %a" Db.pp_error e);
    run (Db.finalize st))
;;

(* ------------------------------------------------------------------ *)
(* 4. Autocommit is untouched, and ROLLBACK restores the statement.     *)
(* ------------------------------------------------------------------ *)

let autocommit_still_checkpoints () =
  with_wal_db (fun db ->
    seed db;
    exec db "PRAGMA wal_checkpoint";
    exec db "INSERT INTO t VALUES (200, 'after')";
    exec db "PRAGMA wal_checkpoint";
    Alcotest.(check int)
      "rows survive two checkpoints"
      41
      (count db "SELECT COUNT(*) FROM t"))
;;

let rollback_makes_it_available_again () =
  with_wal_db (fun db ->
    seed db;
    exec db "BEGIN";
    (match err_msg db "PRAGMA wal_checkpoint" with
     | None -> Alcotest.fail "accepted inside a transaction"
     | Some m -> refusal m);
    exec db "ROLLBACK";
    exec db "PRAGMA wal_checkpoint";
    exec db "BEGIN";
    (match err_msg db "PRAGMA wal_checkpoint" with
     | None -> Alcotest.fail "accepted inside the second transaction"
     | Some m -> refusal m);
    exec db "COMMIT";
    exec db "PRAGMA wal_checkpoint";
    Alcotest.(check int) "data intact throughout" 40 (count db "SELECT COUNT(*) FROM t"))
;;

(* ------------------------------------------------------------------ *)
(* 5. The refusal is NOT the #555 poison.                               *)
(* ------------------------------------------------------------------ *)

(* Nothing is doomed and nothing is contaminated: the refusal is a statement
   that could not be honoured, in the spirit of #598's routing refusals. A
   caller that reads [transaction_poisoned] to decide whether it must ROLLBACK
   must not be told to. *)
let the_refusal_does_not_poison () =
  with_wal_db (fun db ->
    seed db;
    exec db "BEGIN";
    (match err_msg db "PRAGMA wal_checkpoint" with
     | None -> Alcotest.fail "accepted inside a transaction"
     | Some _ -> ());
    Alcotest.(check bool) "the handle is not poisoned" false (Db.transaction_poisoned db);
    (* And ordinary statements still work inside the same transaction. *)
    exec db "INSERT INTO t VALUES (300, 'still-usable')";
    Alcotest.(check int)
      "read-your-own-writes still works"
      41
      (count db "SELECT COUNT(*) FROM t");
    exec db "COMMIT")
;;

let () =
  Alcotest.run
    "wal_checkpoint_txn_740"
    [ ( "refusal"
      , [ Alcotest.test_case
            "refused inside an explicit transaction"
            `Quick
            refused_inside_an_explicit_transaction
        ; Alcotest.test_case
            "refused under a bare SAVEPOINT"
            `Quick
            refused_under_a_bare_savepoint
        ; Alcotest.test_case "refused when prepared" `Quick refused_when_prepared
        ; Alcotest.test_case
            "the refusal does not poison the handle"
            `Quick
            the_refusal_does_not_poison
        ] )
    ; ( "unchanged"
      , [ Alcotest.test_case
            "autocommit still checkpoints"
            `Quick
            autocommit_still_checkpoints
        ; Alcotest.test_case
            "ROLLBACK makes it available again"
            `Quick
            rollback_makes_it_available_again
        ] )
    ]
;;
