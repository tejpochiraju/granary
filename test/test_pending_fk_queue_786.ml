(** #786/#789: an unrelated statement failure inside an open explicit
    transaction must discard only ITS OWN additions to the pending
    deferred-FK-check queue, not the whole queue.

    {1 The defect}

    [Cat.clear_pending_fk_checks] is [t.pending_fk_checks <- []] -- the
    entire queue. [Db.run_dml] (one-shot DML) and [Db.run_core] (a prepared
    statement) both called it, unconditionally, on ANY [Failure] raised by
    the statement they had just run, including while an explicit transaction
    was still open and even though the comment at each call site claimed it
    was discarding "any pending deferred FK checks queued by the failed
    statement". It was not statement-scoped at all:

    {[
      BEGIN;
      INSERT INTO c VALUES (1, 999);   -- queues a deferred FK obligation
      INSERT INTO u VALUES (1);        -- fails: UNIQUE, unrelated to FKs
      COMMIT;                          -- (pre-fix) succeeds; c(1,999) is a
                                        -- permanent orphan
    ]}

    Both issues describe the identical defect; #786's own body claims it was
    "Fixed in that PR" (#785) via a [Cat.pending_fk_mark] /
    [Cat.rollback_pending_fk_checks] watermark mechanism -- that claim was
    verified false before this file was written: neither symbol existed on
    [main], and [Cat.clear_pending_fk_checks] was still the blunt
    [t.pending_fk_checks <- []]. This file is the actual fix's test, filed
    against both issue numbers.

    {1 The fix}

    [Cat.pending_fk_mark] records a physical snapshot of the pending-FK queue
    (the list value itself, not a length) before a statement runs;
    [Cat.rollback_pending_fk_checks ~mark] restores that exact snapshot on
    that statement's own failure -- an O(1) list-value swap (round 2's rewrite
    of round 1's original length/[drop_n] design, matching the pattern
    [Schema_cache.savepoint_begin]/[savepoint_rollback] already use), not a
    length-based truncation. [queue_pending_fk_check] prepends (newest at the
    head), so everything the statement itself queued sits ahead of the mark
    in the list at failure time, and swapping back to the mark discards
    exactly that prefix with no per-check identity needed. The three genuine
    transaction-BOUNDARY call sites ([force_rollback_txn], the two
    [Lwt.finalize] arms of [drain_pending_fks_or_fail] /
    [drain_pending_fks_autocommit]) are untouched -- wiping the whole queue is
    correct there. *)

module Db = Granary.Db
module C = Granary_catalog.Catalog
module S = Granary_store.Store

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

(* Like [exec], but reports [Error] instead of aborting the binary -- used
   for statements the test expects might fail. *)
let exec_result db sql : (unit, string) result =
  try
    match run (Db.execute db sql) with
    | Ok () -> Ok ()
    | Error e -> Error (Format.asprintf "%a" Db.pp_error e)
  with
  | exn -> Error (Printf.sprintf "uncaught exception: %s" (Printexc.to_string exn))
;;

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let query_ints db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun (r : Db.row) ->
         match r.(0) with
         | Db.V_int n -> Int64.to_int n
         | _ -> Alcotest.fail "expected an int column")
      (run (Lwt_stream.to_list stream))
;;

let create_schema db =
  exec db "PRAGMA foreign_keys = 1";
  exec db "CREATE TABLE p (id INTEGER PRIMARY KEY)";
  exec
    db
    "CREATE TABLE c (id INTEGER PRIMARY KEY, pid INTEGER REFERENCES p(id) DEFERRABLE \
     INITIALLY DEFERRED)";
  exec db "CREATE TABLE u (k INTEGER, UNIQUE (k))";
  exec db "INSERT INTO u VALUES (1)"
;;

(* ------------------------------------------------------------------ *)
(* The issue's own repro                                               *)
(* ------------------------------------------------------------------ *)

(* Before the fix: COMMIT succeeded and left c(1, 999) as a permanent
   orphan. The unrelated UNIQUE failure emptied the queue that COMMIT's
   deferred-FK drain depended on. *)
let orphan_row_repro_now_fails_at_commit () =
  with_db (fun db ->
    create_schema db;
    exec db "BEGIN";
    exec db "INSERT INTO c VALUES (1, 999)";
    (* queues a deferred FK obligation; 999 has no parent *)
    (match exec_result db "INSERT INTO u VALUES (1)" with
     | Ok () -> Alcotest.fail "the UNIQUE insert was expected to fail"
     | Error msg ->
       Alcotest.(check bool)
         (Printf.sprintf "fails with a UNIQUE conflict, not an FK message (got %S)" msg)
         true
         (contains ~needle:"UNIQUE" msg || contains ~needle:"unique" msg));
    match exec_result db "COMMIT" with
    | Ok () ->
      Alcotest.fail
        "COMMIT must refuse: the deferred FK obligation queued earlier in this \
         transaction is still violated"
    | Error msg ->
      Alcotest.(check bool)
        (Printf.sprintf "COMMIT refused with an FK message (got %S)" msg)
        true
        (contains ~needle:"FOREIGN KEY" msg))
;;

(* The failed COMMIT above rolls the whole transaction back (same path as
   any other deferred-FK violation at COMMIT) -- confirm the orphan row
   really is gone, not just that COMMIT returned an error. *)
let orphan_row_repro_leaves_no_row_after_the_refused_commit () =
  with_db (fun db ->
    create_schema db;
    exec db "BEGIN";
    exec db "INSERT INTO c VALUES (1, 999)";
    ignore (exec_result db "INSERT INTO u VALUES (1)");
    (match exec_result db "COMMIT" with
     | Ok () -> Alcotest.fail "expected COMMIT to be refused"
     | Error _ -> ());
    Alcotest.(check (list int))
      "no orphan row survives"
      []
      (query_ints db "SELECT id FROM c"))
;;

(* Two successful statements each queue their own obligation before the
   unrelated failure; both must still be recheckable at COMMIT, not just
   the most recent one -- pins that the fix is not "keep the last check"
   but "keep everything before the mark". *)
let commit_still_fails_with_two_pending_obligations_before_the_failure () =
  with_db (fun db ->
    create_schema db;
    exec db "CREATE TABLE p2 (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE c2 (id INTEGER PRIMARY KEY, pid INTEGER REFERENCES p2(id) DEFERRABLE \
       INITIALLY DEFERRED)";
    exec db "INSERT INTO p2 VALUES (1)";
    exec db "BEGIN";
    exec db "INSERT INTO c VALUES (1, 999)";
    (* obligation #1: violated, on table p/c *)
    exec db "INSERT INTO c2 VALUES (1, 1)";
    (* obligation #2: satisfied against p2 -- must not itself cause a failure *)
    ignore (exec_result db "INSERT INTO u VALUES (1)");
    (* unrelated UNIQUE failure *)
    match exec_result db "COMMIT" with
    | Ok () -> Alcotest.fail "COMMIT must refuse: obligation #1 is still violated"
    | Error msg ->
      Alcotest.(check bool)
        (Printf.sprintf "refused with an FK message naming table 'c' (got %S)" msg)
        true
        (contains ~needle:"FOREIGN KEY" msg))
;;

(* ------------------------------------------------------------------ *)
(* No over-refusal                                                     *)
(* ------------------------------------------------------------------ *)

(* An unrelated failure with NOTHING pending must not itself block a later,
   legitimate COMMIT -- the mark/rollback must be a true no-op
   when there is nothing to discard. *)
let unrelated_failure_with_nothing_pending_does_not_block_commit () =
  with_db (fun db ->
    create_schema db;
    exec db "BEGIN";
    ignore (exec_result db "INSERT INTO u VALUES (1)");
    (* fails, nothing was ever queued *)
    exec db "INSERT INTO p VALUES (999)";
    exec db "INSERT INTO c VALUES (1, 999)";
    (* now satisfied *)
    match exec_result db "COMMIT" with
    | Ok () -> ()
    | Error msg -> Alcotest.failf "COMMIT should have succeeded, got %S" msg)
;;

(* A deferred obligation queued BEFORE the unrelated failure, but later
   satisfied by a statement AFTER the failure, must COMMIT cleanly -- the
   rollback must not touch obligations it should keep just because a
   failure happened somewhere in between. *)
let obligation_queued_before_failure_and_satisfied_after_still_commits () =
  with_db (fun db ->
    create_schema db;
    exec db "BEGIN";
    exec db "INSERT INTO c VALUES (1, 999)";
    (* queued, violated for now *)
    ignore (exec_result db "INSERT INTO u VALUES (1)");
    (* unrelated failure *)
    exec db "INSERT INTO p VALUES (999)";
    (* satisfies the earlier obligation *)
    (match exec_result db "COMMIT" with
     | Ok () -> ()
     | Error msg -> Alcotest.failf "COMMIT should have succeeded, got %S" msg);
    Alcotest.(check (list int)) "row committed" [ 1 ] (query_ints db "SELECT id FROM c"))
;;

(* ------------------------------------------------------------------ *)
(* The prepared-statement path ([Db.run_core])                         *)
(* ------------------------------------------------------------------ *)

(* The same defect existed on the prepared-statement failure arm
   independently of the one-shot [run_dml] arm -- exercise it via
   [Db.prepare]/[Db.run] to pin that fix too. *)
let prepared_statement_failure_does_not_clear_earlier_obligations () =
  with_db (fun db ->
    create_schema db;
    exec db "BEGIN";
    exec db "INSERT INTO c VALUES (1, 999)";
    (match run (Db.prepare db "INSERT INTO u VALUES (?)") with
     | Error e -> Alcotest.failf "prepare failed: %a" Db.pp_error e
     | Ok stmt ->
       (match run (Db.run stmt ~params:[ Db.V_int 1L ]) with
        | Ok _ -> Alcotest.fail "the prepared UNIQUE insert was expected to fail"
        | Error _ -> ()));
    match exec_result db "COMMIT" with
    | Ok () ->
      Alcotest.fail
        "COMMIT must refuse: a prepared statement's failure must not have cleared the \
         earlier deferred obligation"
    | Error msg ->
      Alcotest.(check bool)
        (Printf.sprintf "refused with an FK message (got %S)" msg)
        true
        (contains ~needle:"FOREIGN KEY" msg))
;;

(* ------------------------------------------------------------------ *)
(* Transaction-boundary clears stay full clears                        *)
(* ------------------------------------------------------------------ *)

(* An explicit ROLLBACK is a real transaction boundary and must still wipe
   the WHOLE queue, unaffected by the new watermark machinery: a later,
   unrelated transaction must not see any trace of the rolled-back
   obligation. *)
let explicit_rollback_still_fully_clears_the_queue () =
  with_db (fun db ->
    create_schema db;
    exec db "BEGIN";
    exec db "INSERT INTO c VALUES (1, 999)";
    ignore (exec_result db "INSERT INTO u VALUES (1)");
    exec db "ROLLBACK";
    Alcotest.(check (list int))
      "rolled back: no rows"
      []
      (query_ints db "SELECT id FROM c");
    (* A fresh transaction with nothing pending must commit cleanly -- if
       ROLLBACK left anything behind in the queue this would spuriously
       fail or behave oddly. *)
    exec db "BEGIN";
    exec db "INSERT INTO p VALUES (5)";
    exec db "INSERT INTO c VALUES (2, 5)";
    match exec_result db "COMMIT" with
    | Ok () ->
      Alcotest.(check (list int)) "row committed" [ 2 ] (query_ints db "SELECT id FROM c")
    | Error msg -> Alcotest.failf "COMMIT should have succeeded, got %S" msg)
;;

(* A COMMIT that itself fails on the deferred-FK drain also rolls the
   transaction back (same [force_rollback_txn] / [drain_pending_fks_or_fail]
   boundary), and that clear must be equally complete: nothing survives into
   the next transaction. *)
let commit_time_fk_failure_also_fully_clears_the_queue () =
  with_db (fun db ->
    create_schema db;
    exec db "BEGIN";
    exec db "INSERT INTO c VALUES (1, 999)";
    (match exec_result db "COMMIT" with
     | Ok () -> Alcotest.fail "expected the deferred FK violation to fail COMMIT"
     | Error _ -> ());
    exec db "BEGIN";
    exec db "INSERT INTO p VALUES (7)";
    exec db "INSERT INTO c VALUES (3, 7)";
    match exec_result db "COMMIT" with
    | Ok () -> ()
    | Error msg -> Alcotest.failf "COMMIT should have succeeded, got %S" msg)
;;

(* ------------------------------------------------------------------ *)
(* Documented boundary: no statement-level atomicity                   *)
(* ------------------------------------------------------------------ *)

(* A single multi-row INSERT that queues an obligation for its first row and
   then fails on its second (a UNIQUE conflict within the SAME statement)
   still discards the obligation IT queued -- there is no statement-level
   savepoint on the ordinary DML path (#280/#283), so the first row's write
   already survives in the borrowed transaction and the fix does not (and
   is not meant to) give that row's own deferred check statement-level
   atomicity. This is the pre-fix behaviour for a statement's OWN additions,
   preserved deliberately; only OTHER statements' obligations are now
   protected. *)
let a_failed_statements_own_obligations_are_still_discarded () =
  with_db (fun db ->
    create_schema db;
    exec db "BEGIN";
    (* Row (1, 999): queues a deferred, violated obligation.
       Row (1, 1): same rowid 1 -- fails, since id is the PRIMARY KEY. *)
    (match exec_result db "INSERT INTO c VALUES (1, 999), (1, 1)" with
     | Ok () -> Alcotest.fail "expected the second row's PRIMARY KEY collision to fail"
     | Error _ -> ());
    (* The first row's write survives the borrowed transaction (no
       statement-level atomicity), but its own deferred obligation was
       discarded along with the rest of the failed statement's additions --
       so COMMIT sees nothing pending and succeeds, leaving the orphan. This
       is the documented, unchanged boundary, not a new bug: if it ever
       stops holding, re-derive this test from #786/#789's comments rather
       than just loosening the assertion. *)
    (match exec_result db "COMMIT" with
     | Ok () -> ()
     | Error msg -> Alcotest.failf "COMMIT unexpectedly refused: %s" msg);
    Alcotest.(check (list int))
      "the orphan row from the failed statement's own first insert survives (documented, \
       unchanged boundary)"
      [ 1 ]
      (query_ints db "SELECT id FROM c"))
;;

(* #791 review round 3: [query_impl]/[iter_impl] (reached by RETURNING DML,
   #778) had no [fk_mark]/rollback wiring at all -- round 1/2 only
   touched [run_dml]/[run_core]. [Sql.Exec.stream_insert_returning] processes
   every VALUES row eagerly inside the promise [Db.query] awaits (via
   [Lwt_list.map_s]), before ever returning a stream, so the second row's
   PRIMARY KEY collision below fails DURING [Db.query]'s own call, landing in
   [query_impl]'s [Lwt.catch] exactly like [Db.execute]'s spelling lands in
   [run_dml]'s -- same repro as the test above, same assertions, RETURNING
   instead of a plain INSERT.

   This case DOES discriminate, and in the direction that is easy to get
   backwards: with [query_impl]'s rollback removed (verified by mutation),
   the failed statement's own obligation stays queued and COMMIT REFUSES
   the orphan -- so before round 3 the query path was the stricter of the
   two spellings, and round 3's wiring made it admit the same orphan
   [run_dml] admits. What this pins is parity with the documented boundary
   above, not an improvement in safety on this path. *)
let a_failed_returning_insert_via_query_path_discards_its_own_obligations () =
  with_db (fun db ->
    create_schema db;
    exec db "BEGIN";
    (match run (Db.query db "INSERT INTO c VALUES (1, 999), (1, 1) RETURNING id") with
     | Ok _stream ->
       Alcotest.fail "expected the second row's PRIMARY KEY collision to fail"
     | Error _ -> ());
    (match exec_result db "COMMIT" with
     | Ok () -> ()
     | Error msg -> Alcotest.failf "COMMIT unexpectedly refused: %s" msg);
    Alcotest.(check (list int))
      "the orphan row from the failed RETURNING statement's own first insert survives \
       (same documented boundary as the Db.execute spelling)"
      [ 1 ]
      (query_ints db "SELECT id FROM c"))
;;

(* #792 (#778) made RETURNING DML fire OCaml row hooks, and a [`Before] veto
   surfaces as [Lwt.fail_with msg] -- a [Failure] -- inside the promise
   [Db.query] awaits, so it must land in {!Db.with_fk_rollback_on_failure}
   like any other statement failure. The statement below queues its own
   obligation for row 2 (parent 998 missing) and is then vetoed on row 3.
   Two separate transactions pin the two halves of the contract: the
   rollback does not wipe an EARLIER statement's obligation (COMMIT still
   refuses 999), and it does discard the vetoed statement's OWN obligation
   (once 999 exists, COMMIT goes through -- the documented boundary). *)
let is_id_3 (r : Db.row) =
  match r.(0) with
  | Db.V_int 3L -> true
  | _ -> false
;;

let veto_id_3 (m : Db.row_mutation) =
  match m.Db.new_row with
  | Some r when is_id_3 r -> Lwt.return (Error "veto id 3")
  | _ -> Lwt.return (Ok ())
;;

let with_vetoed_returning_insert f =
  with_db (fun db ->
    create_schema db;
    (match
       Db.register_row_hook db ~table:"c" ~timing:`Before ~event:`Insert veto_id_3
     with
     | Ok _ -> ()
     | Error _ -> Alcotest.fail "could not register the veto hook");
    exec db "BEGIN";
    exec db "INSERT INTO c VALUES (1, 999)";
    (match run (Db.query db "INSERT INTO c VALUES (2, 998), (3, 1) RETURNING id") with
     | Ok _ -> Alcotest.fail "expected the row-hook veto on id 3 to fail the statement"
     | Error e ->
       let msg = Format.asprintf "%a" Db.pp_error e in
       Alcotest.(check bool)
         (Printf.sprintf "fails with the veto message (got %S)" msg)
         true
         (contains ~needle:"veto id 3" msg));
    f db)
;;

let returning_veto_keeps_an_earlier_statements_obligation () =
  with_vetoed_returning_insert (fun db ->
    match exec_result db "COMMIT" with
    | Ok () ->
      Alcotest.fail "COMMIT must refuse: c(1, 999)'s obligation predates the veto"
    | Error msg ->
      Alcotest.(check bool)
        (Printf.sprintf "COMMIT refused with an FK message (got %S)" msg)
        true
        (contains ~needle:"FOREIGN KEY" msg))
;;

let returning_veto_discards_its_own_obligation () =
  with_vetoed_returning_insert (fun db ->
    exec db "INSERT INTO p VALUES (999)";
    match exec_result db "COMMIT" with
    | Ok () -> ()
    | Error msg ->
      Alcotest.failf
        "COMMIT should succeed: the vetoed statement's own obligation is discarded with \
         it, got %S"
        msg)
;;

(* ------------------------------------------------------------------ *)
(* QCheck: the mark/rollback snapshot invariant, over the catalog      *)
(* primitives directly rather than through SQL (#786/#789 review)       *)
(* ------------------------------------------------------------------ *)

(* A dummy check tagged only by an int in [pfk_rowid], so a list of ints
   round-trips through queue/mark/rollback/peek and can be compared for
   identity and order with plain [=]. The recheck callback is never invoked
   by any of the functions under test. *)
let mk_check n : C.pending_fk_check =
  { C.pfk_kind = `Insert
  ; pfk_table = "t"
  ; pfk_rowid = Int64.of_int n
  ; pfk_message = ""
  ; pfk_recheck = { recheck = (fun _ -> Lwt.return_false) }
  ; pfk_child_table = "t"
  ; pfk_fk_ordinal = 0
  }
;;

let tags_of cat =
  List.map
    (fun (c : C.pending_fk_check) -> Int64.to_int c.pfk_rowid)
    (C.peek_pending_fk_checks cat)
;;

(* For any [pre]/[post] pair: queuing [pre], taking a mark, queuing [post],
   then rolling back to that mark must leave exactly [pre], in its original
   enqueue order -- regardless of how many (or how few) checks were queued
   on either side of the mark. This is the property the review
   asked for directly on [pending_fk_mark]/[rollback_pending_fk_checks],
   rather than only indirectly through the SQL scenarios above. *)
let prop_rollback_restores_exactly_the_pre_mark_prefix =
  QCheck.Test.make
    ~count:2_000
    ~name:"rollback_pending_fk_checks restores exactly what was queued before the mark"
    QCheck.(
      pair
        (list_size (Gen.int_range 0 20) nat_small)
        (list_size (Gen.int_range 0 20) nat_small))
    (fun (pre, post) ->
       let store = S.create () in
       let cat = run (C.open_ store) in
       List.iter (fun n -> C.queue_pending_fk_check cat (mk_check n)) pre;
       let mark = C.pending_fk_mark cat in
       List.iter (fun n -> C.queue_pending_fk_check cat (mk_check n)) post;
       C.rollback_pending_fk_checks cat ~mark;
       tags_of cat = pre)
;;

let () =
  Alcotest.run
    "pending_fk_queue_786"
    [ ( "orphan_row_repro"
      , [ Alcotest.test_case
            "the issue's own repro now fails at COMMIT"
            `Quick
            orphan_row_repro_now_fails_at_commit
        ; Alcotest.test_case
            "the refused COMMIT leaves no orphan row"
            `Quick
            orphan_row_repro_leaves_no_row_after_the_refused_commit
        ; Alcotest.test_case
            "two pending obligations both survive an unrelated failure"
            `Quick
            commit_still_fails_with_two_pending_obligations_before_the_failure
        ] )
    ; ( "no_over_refusal"
      , [ Alcotest.test_case
            "an unrelated failure with nothing pending does not block COMMIT"
            `Quick
            unrelated_failure_with_nothing_pending_does_not_block_commit
        ; Alcotest.test_case
            "an obligation satisfied after the failure still commits"
            `Quick
            obligation_queued_before_failure_and_satisfied_after_still_commits
        ] )
    ; ( "prepared_statement_path"
      , [ Alcotest.test_case
            "a prepared statement's failure does not clear earlier obligations"
            `Quick
            prepared_statement_failure_does_not_clear_earlier_obligations
        ] )
    ; ( "transaction_boundary_clears_unchanged"
      , [ Alcotest.test_case
            "an explicit ROLLBACK still fully clears the queue"
            `Quick
            explicit_rollback_still_fully_clears_the_queue
        ; Alcotest.test_case
            "a COMMIT-time deferred-FK failure also fully clears the queue"
            `Quick
            commit_time_fk_failure_also_fully_clears_the_queue
        ] )
    ; ( "documented_boundary"
      , [ Alcotest.test_case
            "a failed statement's own obligations are still discarded"
            `Quick
            a_failed_statements_own_obligations_are_still_discarded
        ] )
    ; ( "round 3 (#791 review): query_impl/iter_impl (RETURNING DML) parity with \
         run_dml/run_core"
      , [ Alcotest.test_case
            "a failed RETURNING insert via Db.query discards its own obligations, like \
             Db.execute"
            `Quick
            a_failed_returning_insert_via_query_path_discards_its_own_obligations
        ; Alcotest.test_case
            "a RETURNING row-hook veto (#778) keeps an earlier statement's obligation"
            `Quick
            returning_veto_keeps_an_earlier_statements_obligation
        ; Alcotest.test_case
            "a RETURNING row-hook veto (#778) discards its own obligation"
            `Quick
            returning_veto_discards_its_own_obligation
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_rollback_restores_exactly_the_pre_mark_prefix ] )
    ]
;;
