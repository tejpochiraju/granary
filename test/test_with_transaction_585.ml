(** #585: [Db.with_transaction] — a scoped explicit-transaction extent.

    The combinator BEGINs, runs the body, COMMITs on success and ROLLBACKs and
    re-raises on exception. The point of it is not the convenience: it is that a
    transaction now has a dynamic {e extent} an owner token can live in, which
    #555/#584 both name as the missing prerequisite for telling transaction
    owners apart.

    What is pinned here, in order:

    - commit on success, rollback on exception with the exception propagating,
      and a rolled-back scope leaving the database byte-for-byte as it was;
    - the owner token's one live decision today — a nested [with_transaction],
      including from a fiber spawned inside the body, is refused cleanly and
      does {b not} poison the handle or disturb the outer transaction;
    - that nothing about the #555 poison was routed around: a second fiber on a
      {e shared} handle still fails and still poisons, [with_transaction] issues
      no [ROLLBACK] of its own on that path, and [ROLLBACK] remains the sole
      exit;
    - the #584 displacement guard, in {b all three} shapes the slot can be left
      in (refilled by a raw [BEGIN], refilled by another scope, left empty) and
      on {b both} exit arms (commit and rollback). The raw-[BEGIN] shape is the
      one #584 is written in and the one the first cut of this feature got
      wrong: the owner token was written only by [with_transaction], so it
      outlived its transaction and the guard false-matched, committing the other
      fiber's work and returning [Ok]. These four cases exist because a guard
      that only fires for the spelling nobody uses is worse than no guard;
    - that two fibers with their own {!Db.create_worker_handle} handles both run
      scoped transactions to completion. *)

open Lwt.Syntax

(* No [open_file] shim here: every case in this file builds its handle through
   [Granary.Db.open_in_memory] or [create_worker_handle]. *)
module Db = Granary.Db

let () = Granary_unix.install ()
let run = Lwt_main.run
let fresh_db () = run (Db.open_in_memory ())
let rows_of stream = run (Lwt_stream.to_list stream)

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "error in %S: %a" sql Db.pp_error e
;;

let ok_exn label = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%s: %a" label Db.pp_error e
;;

let is_runtime_err = function
  | Error (Db.Runtime _) -> true
  | _ -> false
;;

let err_msg = function
  | Error (Db.Runtime m) -> m
  | Error e -> Format.asprintf "%a" Db.pp_error e
  | Ok _ -> "<ok>"
;;

(* Substring search without pulling [str] into this test's libraries. *)
let contains ~needle haystack =
  let nl = String.length needle
  and hl = String.length haystack in
  let rec at i = i + nl <= hl && (String.sub haystack i nl = needle || at (i + 1)) in
  nl = 0 || at 0
;;

let query_ints db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query error in %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun row ->
         match row.(0) with
         | Db.V_int n -> Int64.to_int n
         | _ -> -1)
      (rows_of stream)
;;

let ns db = query_ints db "SELECT n FROM t ORDER BY n ASC"

let seed db =
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, n INTEGER)";
  db
;;

exception Boom

(* ------------------------------------------------------------------ *)
(* Commit on success                                                    *)
(* ------------------------------------------------------------------ *)

(* The body's value comes back wrapped in [Ok], and it comes back only once the
   COMMIT has succeeded — so a caller that sees [Ok] may treat the writes as
   durable. Both statements land, atomically. *)
let test_commit_on_success () =
  let db = seed (fresh_db ()) in
  let r =
    run
      (Db.with_transaction db (fun db ->
         let* r = Db.execute db "INSERT INTO t (id, n) VALUES (1, 10)" in
         ok_exn "insert 1" r;
         let* r = Db.execute db "INSERT INTO t (id, n) VALUES (2, 20)" in
         ok_exn "insert 2" r;
         Lwt.return "payload"))
  in
  Alcotest.(check string) "body's value is returned" "payload" (ok_exn "scope" r);
  Alcotest.(check (list int)) "both rows committed" [ 10; 20 ] (ns db);
  Alcotest.(check bool) "handle not poisoned" false (Db.transaction_poisoned db);
  (* The slot was released: a plain COMMIT now has nothing to commit. *)
  Alcotest.(check bool)
    "transaction slot released"
    true
    (is_runtime_err (run (Db.execute db "COMMIT")))
;;

(* A second scope on the same handle, one after the other, is ordinary use — the
   token is minted per scope, so an exited scope never blocks the next one. *)
let test_sequential_scopes () =
  let db = seed (fresh_db ()) in
  let one n =
    run
      (Db.with_transaction db (fun db ->
         let* r =
           Db.execute db (Printf.sprintf "INSERT INTO t (id, n) VALUES (%d, %d)" n n)
         in
         ok_exn "insert" r;
         Lwt.return_unit))
  in
  ok_exn "first scope" (one 1);
  ok_exn "second scope" (one 2);
  ok_exn "third scope" (one 3);
  Alcotest.(check (list int)) "three committed scopes" [ 1; 2; 3 ] (ns db)
;;

(* ------------------------------------------------------------------ *)
(* Rollback on exception                                                *)
(* ------------------------------------------------------------------ *)

(* The exception is re-raised, NOT converted into [Error] — a caller who wants
   it as a value catches it inside the body. And the writes made before it are
   gone. *)
let test_rollback_on_exception_propagates () =
  let db = seed (fresh_db ()) in
  exec db "INSERT INTO t (id, n) VALUES (1, 1)";
  let raised =
    try
      ignore
        (run
           (Db.with_transaction db (fun db ->
              let* r = Db.execute db "INSERT INTO t (id, n) VALUES (2, 2)" in
              ok_exn "insert inside doomed scope" r;
              Lwt.fail Boom)));
      false
    with
    | Boom -> true
  in
  Alcotest.(check bool) "the body's exception propagates" true raised;
  Alcotest.(check (list int)) "only the pre-committed row survives" [ 1 ] (ns db);
  Alcotest.(check bool)
    "handle not poisoned by a rollback"
    false
    (Db.transaction_poisoned db)
;;

(* Rollback leaves the database exactly as the scope found it, and leaves the
   handle fully usable: the next scope commits normally. Nothing about the
   failure is sticky. *)
let test_rollback_leaves_db_unchanged () =
  let db = seed (fresh_db ()) in
  exec db "INSERT INTO t (id, n) VALUES (1, 1)";
  exec db "INSERT INTO t (id, n) VALUES (2, 2)";
  let before = ns db in
  (try
     ignore
       (run
          (Db.with_transaction db (fun db ->
             let* r = Db.execute db "INSERT INTO t (id, n) VALUES (3, 3)" in
             ok_exn "insert" r;
             let* r = Db.execute db "DELETE FROM t WHERE id = 1" in
             ok_exn "delete" r;
             let* r = Db.execute db "UPDATE t SET n = 99 WHERE id = 2" in
             ok_exn "update" r;
             Lwt.fail Boom)))
   with
   | Boom -> ());
  Alcotest.(check (list int)) "insert, delete and update all reverted" before (ns db);
  (* Still usable. *)
  ok_exn
    "a later scope still commits"
    (run
       (Db.with_transaction db (fun db ->
          let* r = Db.execute db "INSERT INTO t (id, n) VALUES (4, 4)" in
          ok_exn "insert" r;
          Lwt.return_unit)));
  Alcotest.(check (list int)) "and only that scope's row was added" [ 1; 2; 4 ] (ns db)
;;

(* A body that returns an [Error] result rather than raising is NOT a failure as
   far as the combinator is concerned — the transaction commits. Pinned because
   it is the obvious wrong expectation: [with_transaction] rolls back on
   exceptions, and a result value is just a value. *)
let test_error_result_from_body_still_commits () =
  let db = seed (fresh_db ()) in
  let r =
    run
      (Db.with_transaction db (fun db ->
         let* r = Db.execute db "INSERT INTO t (id, n) VALUES (1, 1)" in
         ok_exn "insert" r;
         (* A statement that fails, whose Error the body chooses to swallow. *)
         let* bad = Db.execute db "INSERT INTO t (id, n) VALUES (1, 1)" in
         Lwt.return (is_runtime_err bad)))
  in
  Alcotest.(check bool) "the duplicate insert did fail" true (ok_exn "scope" r);
  Alcotest.(check (list int)) "the scope still committed" [ 1 ] (ns db)
;;

(* ------------------------------------------------------------------ *)
(* in_transaction_scope                                                 *)
(* ------------------------------------------------------------------ *)

(* The observable half of the owner token: true only for the fiber inside the
   extent, and only while it is inside. A transaction opened by a bare BEGIN has
   no owner and reports false. *)
let test_in_transaction_scope () =
  let db = seed (fresh_db ()) in
  Alcotest.(check bool) "false before any scope" false (Db.in_transaction_scope db);
  let inside = ref false in
  ok_exn
    "scope"
    (run
       (Db.with_transaction db (fun db ->
          inside := Db.in_transaction_scope db;
          Lwt.return_unit)));
  Alcotest.(check bool) "true inside the scope" true !inside;
  Alcotest.(check bool) "false after the scope exits" false (Db.in_transaction_scope db);
  (* A bare BEGIN opens a transaction with no owner. *)
  exec db "BEGIN";
  Alcotest.(check bool)
    "false inside an unscoped BEGIN"
    false
    (Db.in_transaction_scope db);
  exec db "ROLLBACK"
;;

(* ------------------------------------------------------------------ *)
(* Nesting: refused, cleanly                                            *)
(* ------------------------------------------------------------------ *)

(* Nesting is neither a savepoint nor a join — see the .mli for why either would
   have to lie to the inner caller. It is refused, and the refusal costs the
   outer scope nothing: no rollback, no poison, outer transaction intact and its
   COMMIT still succeeds. *)
let test_nested_scope_refused () =
  let db = seed (fresh_db ()) in
  let inner = ref (Ok ()) in
  let poisoned_during = ref true in
  ok_exn
    "outer scope"
    (run
       (Db.with_transaction db (fun db ->
          let* r = Db.execute db "INSERT INTO t (id, n) VALUES (1, 1)" in
          ok_exn "outer insert" r;
          let* nested =
            Db.with_transaction db (fun db ->
              let* r = Db.execute db "INSERT INTO t (id, n) VALUES (2, 2)" in
              ok_exn "inner insert" r;
              Lwt.return_unit)
          in
          inner := nested;
          poisoned_during := Db.transaction_poisoned db;
          let* r = Db.execute db "INSERT INTO t (id, n) VALUES (3, 3)" in
          ok_exn "outer insert after refusal" r;
          Lwt.return_unit)));
  Alcotest.(check bool) "nested call refused" true (is_runtime_err !inner);
  Alcotest.(check bool)
    "refusal names #585"
    true
    (contains ~needle:"#585" (err_msg !inner));
  Alcotest.(check bool) "handle NOT poisoned by the refusal" false !poisoned_during;
  Alcotest.(check bool) "still not poisoned afterwards" false (Db.transaction_poisoned db);
  (* The inner body never ran, so row 2 is absent; the outer scope committed
     everything it did do. *)
  Alcotest.(check (list int)) "outer committed, inner body never ran" [ 1; 3 ] (ns db)
;;

(* The token is inherited through [Lwt] storage, so a fiber spawned INSIDE the
   body is the owner too and gets the same clean refusal — not the #555 poison
   a genuinely foreign fiber would get.

   This has to be a REAL fiber boundary to prove anything. An [Lwt.join] over a
   list literal built inside the body evaluates its elements eagerly, on the
   body's own stack, so it would run in the same dynamic extent as
   [nested_scope_refused] and say nothing about [Lwt.with_value] propagation.
   Here the work is detached with [Lwt.async] AND resumed after [Lwt.pause], so
   the continuation that calls [with_transaction] is invoked by the scheduler
   from outside the [with_value] stack frame — which is exactly the propagation
   mechanism (storage captured when a callback is registered) under test. *)
let test_nested_scope_from_spawned_fiber_refused () =
  let db = seed (fresh_db ()) in
  let inner = ref (Ok ()) in
  let inner_sees_scope = ref false in
  ok_exn
    "outer scope"
    (run
       (Db.with_transaction db (fun db ->
          let* r = Db.execute db "INSERT INTO t (id, n) VALUES (1, 1)" in
          ok_exn "outer insert" r;
          let inner_done, wake_inner = Lwt.wait () in
          Lwt.async (fun () ->
            let* () = Lwt.pause () in
            inner_sees_scope := Db.in_transaction_scope db;
            let* nested = Db.with_transaction db (fun _ -> Lwt.return_unit) in
            inner := nested;
            Lwt.wakeup wake_inner ();
            Lwt.return_unit);
          inner_done)));
  Alcotest.(check bool) "spawned fiber inherits the token" true !inner_sees_scope;
  Alcotest.(check bool) "and is refused, not poisoned" true (is_runtime_err !inner);
  Alcotest.(check bool) "handle not poisoned" false (Db.transaction_poisoned db);
  Alcotest.(check (list int)) "outer scope committed" [ 1 ] (ns db)
;;

(* SAVEPOINT is the supported way to get a partial undo point inside a scope,
   and the refusal message says so. Pinned so the advice stays true. *)
let test_savepoint_inside_scope_works () =
  let db = seed (fresh_db ()) in
  ok_exn
    "scope"
    (run
       (Db.with_transaction db (fun db ->
          let* r = Db.execute db "INSERT INTO t (id, n) VALUES (1, 1)" in
          ok_exn "insert 1" r;
          let* r = Db.execute db "SAVEPOINT sp" in
          ok_exn "savepoint" r;
          let* r = Db.execute db "INSERT INTO t (id, n) VALUES (2, 2)" in
          ok_exn "insert 2" r;
          let* r = Db.execute db "ROLLBACK TO sp" in
          ok_exn "rollback to" r;
          let* r = Db.execute db "INSERT INTO t (id, n) VALUES (3, 3)" in
          ok_exn "insert 3" r;
          Lwt.return_unit)));
  Alcotest.(check (list int)) "savepoint undo is partial, scope commits" [ 1; 3 ] (ns db)
;;

(* ------------------------------------------------------------------ *)
(* The #555 poison is not routed around                                 *)
(* ------------------------------------------------------------------ *)

(* On an ALREADY poisoned handle, [with_transaction]'s BEGIN is rejected like
   every other statement. Two things matter and both are asserted: the body
   never runs, and the combinator does NOT issue a ROLLBACK of its own — so it
   does not become a second exit from the poisoned state. The caller's own
   ROLLBACK is still required, and still works. *)
let test_begin_on_poisoned_handle () =
  let db = seed (fresh_db ()) in
  exec db "BEGIN";
  exec db "INSERT INTO t (id, n) VALUES (1, 1)";
  Alcotest.(check bool)
    "second BEGIN poisons"
    true
    (is_runtime_err (run (Db.execute db "BEGIN")));
  Alcotest.(check bool) "poisoned" true (Db.transaction_poisoned db);
  let body_ran = ref false in
  let r =
    run
      (Db.with_transaction db (fun _ ->
         body_ran := true;
         Lwt.return_unit))
  in
  Alcotest.(check bool) "with_transaction refused" true (is_runtime_err r);
  Alcotest.(check bool) "body never ran" false !body_ran;
  Alcotest.(check bool)
    "STILL poisoned - with_transaction is not a second exit"
    true
    (Db.transaction_poisoned db);
  Alcotest.(check bool) "no scope was entered" false (Db.in_transaction_scope db);
  (* ROLLBACK remains the sole exit, and after it the handle is fully usable. *)
  ok_exn "ROLLBACK recovers" (run (Db.execute db "ROLLBACK"));
  Alcotest.(check bool) "poison cleared" false (Db.transaction_poisoned db);
  Alcotest.(check (list int)) "the doomed transaction committed nothing" [] (ns db);
  ok_exn
    "scope works after recovery"
    (run
       (Db.with_transaction db (fun db ->
          let* r = Db.execute db "INSERT INTO t (id, n) VALUES (9, 9)" in
          ok_exn "insert" r;
          Lwt.return_unit)));
  Alcotest.(check (list int)) "and commits" [ 9 ] (ns db)
;;

(* A second FIBER on a SHARED handle is not the nesting case — it holds no
   token, so its BEGIN collides and poisons exactly as a bare BEGIN would. The
   winner's scope is doomed with it (its COMMIT is rejected), which is #555's
   documented and deliberate cost. What [with_transaction] adds here is only
   that both fibers are told; it does not make a shared handle safe. *)
let test_shared_handle_across_fibers_still_poisons () =
  let db = seed (fresh_db ()) in
  let a_inside, wake_a_inside = Lwt.wait () in
  let b_done, wake_b_done = Lwt.wait () in
  let a_result = ref (Ok ()) in
  let b_result = ref (Ok ()) in
  run
    (Lwt.join
       [ (let* r =
            Db.with_transaction db (fun db ->
              let* r = Db.execute db "INSERT INTO t (id, n) VALUES (1, 1)" in
              ok_exn "A insert" r;
              Lwt.wakeup wake_a_inside ();
              b_done)
          in
          a_result := r;
          Lwt.return_unit)
       ; (let* () = a_inside in
          let* r = Db.with_transaction db (fun _ -> Lwt.return_unit) in
          b_result := r;
          Lwt.wakeup wake_b_done ();
          Lwt.return_unit)
       ]);
  Alcotest.(check bool) "B's scope refused" true (is_runtime_err !b_result);
  Alcotest.(check bool) "A's scope doomed too (#555)" true (is_runtime_err !a_result);
  Alcotest.(check bool) "handle poisoned" true (Db.transaction_poisoned db);
  ok_exn "ROLLBACK is still the sole exit" (run (Db.execute db "ROLLBACK"));
  Alcotest.(check bool) "poison cleared" false (Db.transaction_poisoned db);
  Alcotest.(check (list int)) "nothing committed" [] (ns db)
;;

(* ------------------------------------------------------------------ *)
(* #584 displacement — the boundary guard                               *)
(* ------------------------------------------------------------------ *)

(* #584's own sequence, with A on the combinator and B on RAW STATEMENTS. This
   is the spelling the issue is written in and the one the guard originally did
   NOT catch: [txn_scope] was written only by [with_transaction], so after B's
   ROLLBACK freed the slot and B's second BEGIN refilled it, A's token still
   equalled the handle's and A's scope exit COMMITTED B's transaction and
   returned [Ok]. The fix invalidates the token from [begin_txn] /
   [force_rollback_txn] / [commit_txn] / [rollback_txn] as well.

   What must happen now: A's scope issues neither COMMIT nor ROLLBACK, reports
   [Error] naming #584, and B's transaction is left open and still B's. *)
let test_displacement_by_raw_begin_is_caught () =
  let db = seed (fresh_db ()) in
  let a_inside, wake_a_inside = Lwt.wait () in
  let b_done, wake_b_done = Lwt.wait () in
  let a_result = ref (Ok ()) in
  run
    (Lwt.join
       [ (let* r =
            Db.with_transaction db (fun db ->
              let* r = Db.execute db "INSERT INTO t (id, n) VALUES (1, 1)" in
              ok_exn "A insert" r;
              Lwt.wakeup wake_a_inside ();
              b_done)
          in
          a_result := r;
          Lwt.return_unit)
       ; (let* () = a_inside in
          let* r = Db.execute db "BEGIN" in
          Alcotest.(check bool) "B's BEGIN collides and poisons" true (is_runtime_err r);
          let* r = Db.execute db "ROLLBACK" in
          ok_exn "B recovers, aborting A's transaction" r;
          let* r = Db.execute db "BEGIN" in
          ok_exn "B opens its own transaction in the freed slot" r;
          let* r = Db.execute db "INSERT INTO t (id, n) VALUES (99, 99)" in
          ok_exn "B insert" r;
          Lwt.wakeup wake_b_done ();
          Lwt.return_unit)
       ]);
  Alcotest.(check bool)
    "A's displaced scope refused to commit"
    true
    (is_runtime_err !a_result);
  Alcotest.(check bool)
    "and says which hazard it is (#584)"
    true
    (contains ~needle:"#584" (err_msg !a_result));
  (* A issued NEITHER statement, so B's transaction is untouched: still open,
     still B's, and B can commit it itself. Before the fix, A's scope had
     already committed it. *)
  ok_exn "B's transaction is still open and still B's" (run (Db.execute db "COMMIT"));
  Alcotest.(check (list int))
    "only B's row - A's was aborted by B's ROLLBACK"
    [ 99 ]
    (ns db)
;;

(* The other spelling: the displacing fiber also uses [with_transaction]. Before
   the fix this was the ONLY spelling [stolen_txn_msg] fired for, which is why
   the hole survived. Both spellings must now behave identically. *)
let test_displacement_by_scope_is_caught () =
  let db = seed (fresh_db ()) in
  let a_inside, wake_a_inside = Lwt.wait () in
  let b_done, wake_b_done = Lwt.wait () in
  let a_result = ref (Ok ()) in
  let b_result = ref (Ok ()) in
  run
    (Lwt.join
       [ (let* r =
            Db.with_transaction db (fun db ->
              let* r = Db.execute db "INSERT INTO t (id, n) VALUES (1, 1)" in
              ok_exn "A insert" r;
              Lwt.wakeup wake_a_inside ();
              b_done)
          in
          a_result := r;
          Lwt.return_unit)
       ; (let* () = a_inside in
          let* r = Db.execute db "BEGIN" in
          Alcotest.(check bool) "B's BEGIN collides and poisons" true (is_runtime_err r);
          let* r = Db.execute db "ROLLBACK" in
          ok_exn "B recovers, aborting A's transaction" r;
          let* r =
            Db.with_transaction db (fun db ->
              let* r = Db.execute db "INSERT INTO t (id, n) VALUES (99, 99)" in
              ok_exn "B insert" r;
              Lwt.return_unit)
          in
          b_result := r;
          Lwt.wakeup wake_b_done ();
          Lwt.return_unit)
       ]);
  ok_exn "B's own scope committed normally" !b_result;
  Alcotest.(check bool)
    "A's displaced scope refused to commit"
    true
    (is_runtime_err !a_result);
  Alcotest.(check bool)
    "same error as the raw-BEGIN spelling"
    true
    (contains ~needle:"#584" (err_msg !a_result));
  Alcotest.(check (list int)) "only B's row committed" [ 99 ] (ns db);
  Alcotest.(check bool)
    "no transaction left behind"
    true
    (is_runtime_err (run (Db.execute db "COMMIT")))
;;

(* The empty-slot half of the same guard: B rolls A's transaction back and does
   NOT open one of its own, so A's scope exits onto an empty slot. It must still
   refuse rather than fall through to a COMMIT that would report
   "no active transaction" (or, worse, autocommit something). *)
let test_displacement_into_empty_slot_is_caught () =
  let db = seed (fresh_db ()) in
  let a_inside, wake_a_inside = Lwt.wait () in
  let b_done, wake_b_done = Lwt.wait () in
  let a_result = ref (Ok ()) in
  run
    (Lwt.join
       [ (let* r =
            Db.with_transaction db (fun db ->
              let* r = Db.execute db "INSERT INTO t (id, n) VALUES (1, 1)" in
              ok_exn "A insert" r;
              Lwt.wakeup wake_a_inside ();
              b_done)
          in
          a_result := r;
          Lwt.return_unit)
       ; (let* () = a_inside in
          let* r = Db.execute db "BEGIN" in
          Alcotest.(check bool) "B's BEGIN collides and poisons" true (is_runtime_err r);
          let* r = Db.execute db "ROLLBACK" in
          ok_exn "B recovers, aborting A's transaction" r;
          Lwt.wakeup wake_b_done ();
          Lwt.return_unit)
       ]);
  Alcotest.(check bool) "A's scope refused, slot empty" true (is_runtime_err !a_result);
  Alcotest.(check bool)
    "reported as displacement, not as 'no active transaction'"
    true
    (contains ~needle:"#584" (err_msg !a_result));
  Alcotest.(check (list int)) "nothing committed" [] (ns db);
  Alcotest.(check bool) "handle clean and usable" false (Db.transaction_poisoned db)
;;

(* The failure arm of the same guard. A displaced scope whose body then RAISES
   must not issue its ROLLBACK either — that statement would abort the other
   fiber's transaction, destroying committed-intent work that has nothing to do
   with the exception. The exception must still propagate. *)
let test_displaced_scope_does_not_roll_back_the_other_fiber () =
  let db = seed (fresh_db ()) in
  let a_inside, wake_a_inside = Lwt.wait () in
  let b_done, wake_b_done = Lwt.wait () in
  let a_raised = ref false in
  run
    (Lwt.join
       [ Lwt.catch
           (fun () ->
              Lwt.map
                ignore
                (Db.with_transaction db (fun db ->
                   let* r = Db.execute db "INSERT INTO t (id, n) VALUES (1, 1)" in
                   ok_exn "A insert" r;
                   Lwt.wakeup wake_a_inside ();
                   let* () = b_done in
                   Lwt.fail Boom)))
           (function
             | Boom ->
               a_raised := true;
               Lwt.return_unit
             | exn -> Lwt.fail exn)
       ; (let* () = a_inside in
          let* r = Db.execute db "BEGIN" in
          Alcotest.(check bool) "B's BEGIN collides and poisons" true (is_runtime_err r);
          let* r = Db.execute db "ROLLBACK" in
          ok_exn "B recovers, aborting A's transaction" r;
          let* r = Db.execute db "BEGIN" in
          ok_exn "B opens its own transaction" r;
          let* r = Db.execute db "INSERT INTO t (id, n) VALUES (99, 99)" in
          ok_exn "B insert" r;
          Lwt.wakeup wake_b_done ();
          Lwt.return_unit)
       ]);
  Alcotest.(check bool) "A's exception still propagates" true !a_raised;
  (* If A had issued its ROLLBACK, B's row would be gone and this COMMIT would
     answer "no active transaction". *)
  ok_exn "B's transaction survived A's unwind" (run (Db.execute db "COMMIT"));
  Alcotest.(check (list int)) "B's row intact" [ 99 ] (ns db)
;;

(* ------------------------------------------------------------------ *)
(* Two fibers, two handles                                              *)
(* ------------------------------------------------------------------ *)

(* The supported shape. Each fiber gets its own handle via
   [create_worker_handle], so each gets its own transaction slot and its own
   owner token; they share one [Store.t] and therefore one single-writer lock,
   so the second BLOCKS rather than colliding. Both scopes commit and neither
   handle is ever poisoned. *)
let test_two_fibers_two_worker_handles () =
  let db = seed (fresh_db ()) in
  let wdb = run (Db.create_worker_handle db) in
  let a_inside, wake_a_inside = Lwt.wait () in
  let a_may_finish, wake_a_may_finish = Lwt.wait () in
  let b_blocked = ref false in
  let a_result = ref (Ok ()) in
  let b_result = ref (Ok ()) in
  let rec spin n =
    if n = 0 then Lwt.return_unit else Lwt.bind (Lwt.pause ()) (fun () -> spin (n - 1))
  in
  run
    (Lwt.join
       [ (let* r =
            Db.with_transaction db (fun db ->
              let* r = Db.execute db "INSERT INTO t (id, n) VALUES (10, 1)" in
              ok_exn "A insert" r;
              Lwt.wakeup wake_a_inside ();
              a_may_finish)
          in
          a_result := r;
          Lwt.return_unit)
       ; (let* () = a_inside in
          let p =
            Db.with_transaction wdb (fun wdb ->
              let* r = Db.execute wdb "INSERT INTO t (id, n) VALUES (20, 2)" in
              ok_exn "B insert" r;
              Lwt.return_unit)
          in
          let* () = spin 50 in
          (b_blocked
           := match Lwt.state p with
              | Lwt.Sleep -> true
              | _ -> false);
          Lwt.wakeup wake_a_may_finish ();
          let* r = p in
          b_result := r;
          Lwt.return_unit)
       ]);
  Alcotest.(check bool) "B blocked on the shared writer lock" true !b_blocked;
  ok_exn "A's scope committed" !a_result;
  ok_exn "B's scope committed" !b_result;
  Alcotest.(check bool) "parent never poisoned" false (Db.transaction_poisoned db);
  Alcotest.(check bool) "worker never poisoned" false (Db.transaction_poisoned wdb);
  Alcotest.(check (list int)) "both scopes' rows committed" [ 1; 2 ] (ns db);
  Alcotest.(check (list int)) "and both handles agree" [ 1; 2 ] (ns wdb)
;;

(* A rollback in one fiber's scope must not touch the other's committed work.
   Same two-handle shape, but A's scope raises. *)
let test_two_handles_one_rolls_back () =
  let db = seed (fresh_db ()) in
  let wdb = run (Db.create_worker_handle db) in
  ok_exn
    "worker scope commits"
    (run
       (Db.with_transaction wdb (fun wdb ->
          let* r = Db.execute wdb "INSERT INTO t (id, n) VALUES (1, 1)" in
          ok_exn "insert" r;
          Lwt.return_unit)));
  (try
     ignore
       (run
          (Db.with_transaction db (fun db ->
             let* r = Db.execute db "INSERT INTO t (id, n) VALUES (2, 2)" in
             ok_exn "insert" r;
             Lwt.fail Boom)))
   with
   | Boom -> ());
  Alcotest.(check (list int)) "the committed row survives on the parent" [ 1 ] (ns db);
  Alcotest.(check (list int)) "and on the worker" [ 1 ] (ns wdb);
  Alcotest.(check bool) "parent not poisoned" false (Db.transaction_poisoned db);
  Alcotest.(check bool) "worker not poisoned" false (Db.transaction_poisoned wdb)
;;

let () =
  Alcotest.run
    "with_transaction_585"
    [ ( "commit"
      , [ Alcotest.test_case "commit_on_success" `Quick test_commit_on_success
        ; Alcotest.test_case "sequential_scopes" `Quick test_sequential_scopes
        ; Alcotest.test_case
            "error_result_from_body_still_commits"
            `Quick
            test_error_result_from_body_still_commits
        ] )
    ; ( "rollback"
      , [ Alcotest.test_case
            "rollback_on_exception_propagates"
            `Quick
            test_rollback_on_exception_propagates
        ; Alcotest.test_case
            "rollback_leaves_db_unchanged"
            `Quick
            test_rollback_leaves_db_unchanged
        ] )
    ; ( "scope"
      , [ Alcotest.test_case "in_transaction_scope" `Quick test_in_transaction_scope
        ; Alcotest.test_case "nested_scope_refused" `Quick test_nested_scope_refused
        ; Alcotest.test_case
            "nested_scope_from_spawned_fiber_refused"
            `Quick
            test_nested_scope_from_spawned_fiber_refused
        ; Alcotest.test_case
            "savepoint_inside_scope_works"
            `Quick
            test_savepoint_inside_scope_works
        ] )
    ; ( "poison"
      , [ Alcotest.test_case
            "begin_on_poisoned_handle"
            `Quick
            test_begin_on_poisoned_handle
        ; Alcotest.test_case
            "shared_handle_across_fibers_still_poisons"
            `Quick
            test_shared_handle_across_fibers_still_poisons
        ] )
    ; ( "displacement_584"
      , [ Alcotest.test_case
            "displacement_by_raw_begin_is_caught"
            `Quick
            test_displacement_by_raw_begin_is_caught
        ; Alcotest.test_case
            "displacement_by_scope_is_caught"
            `Quick
            test_displacement_by_scope_is_caught
        ; Alcotest.test_case
            "displacement_into_empty_slot_is_caught"
            `Quick
            test_displacement_into_empty_slot_is_caught
        ; Alcotest.test_case
            "displaced_scope_does_not_roll_back_the_other_fiber"
            `Quick
            test_displaced_scope_does_not_roll_back_the_other_fiber
        ] )
    ; ( "worker_handles"
      , [ Alcotest.test_case
            "two_fibers_two_worker_handles"
            `Quick
            test_two_fibers_two_worker_handles
        ; Alcotest.test_case
            "two_handles_one_rolls_back"
            `Quick
            test_two_handles_one_rolls_back
        ] )
    ]
;;
