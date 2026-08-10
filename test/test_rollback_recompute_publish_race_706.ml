(** #706: a ROLLBACK's rowid-counter recompute must publish its correction
    under a compare-and-swap, not a blind overwrite.

    Background (see CLAUDE.md's #632/#633 sections): [Store.rowid_counters]
    is the ONE allocator table every catalog opened over a store shares.
    [Catalog.next_rowid] (the autocommit allocator) was fixed by #632 to
    allocate AND publish with the writer lock held throughout, closing a
    window where a concurrent ROLLBACK's recompute could clobber an
    allocation already handed out.

    [Cat.recompute_rowid_counters_after_rollback] — the ROLLBACK side of that
    same shared table — was never brought under the same discipline. It runs
    its RO scan (correctly) AFTER [S.rollback] released the writer lock, so
    the tree it reads is the reverted, last-committed state and its own RO
    transaction cannot deadlock against the writer lock it would otherwise
    still be holding. But it then PUBLISHED the recomputed value with NO lock
    at all: a second worker handle can [rw_begin], read the still-stale
    (too-high) shared counter this rollback hasn't fixed yet, allocate from
    it, and COMMIT — all before the recompute's publish lands. A blind
    publish after that clobbers the counter back down below an id that
    handle's commit already used, and the next allocation reissues it,
    silently overwriting the row (#589's symptom, reached by a new route:
    the #706 TPC-C repro at [GRANARY_TPCC_TERMINALS>=2]).

    This file pins the fix directly, rather than relying on the TPC-C
    driver's ~1% NewOrder rollback rate under real timing (probabilistic, and
    only reproducible with #703's not-yet-merged concurrent-terminal driver).
    It uses [Cat.rollback_recompute_publish_hook] — a test-only seam awaited
    at exactly the point between the recompute's (unlocked) RO scan finishing
    and it re-acquiring the writer lock to publish — to deterministically run
    a second handle's competing allocation to completion inside that window,
    every time, with no dependence on Lwt scheduling luck.

    Must run ON DISK, in WAL mode: CLAUDE.md's own note on
    [test_rowid_counter_ownership_632.ml] applies unchanged here. The
    in-memory backend's RO scan does no real (yielding) I/O, so nothing would
    ever interleave with it even without the hook, and separately its commit
    resolves synchronously, which would mask the ordering bug even were the
    RO scan slow. Neither property is what this test wants to depend on: the
    hook makes the interleaving explicit regardless of backend, but keeping
    the reproduction on the same WAL-mode, on-disk footing as #632's own
    tests keeps this test honest about the class of bug it guards. *)

open Lwt.Syntax
module Cat = Granary_catalog.Catalog

module Db = struct
  include Granary.Db

  let open_file_wal = Granary_unix.open_file_wal
end

let () = Granary_unix.install ()
let run = Lwt_main.run

(* Sibling worktrees run suites concurrently — carry the pid, the convention
   used by test_rowid_counter_ownership_632.ml and friends. *)
let tmp_path name =
  Printf.sprintf "/tmp/granary_rollback_recompute_706_%s_%d.db" name (Unix.getpid ())
;;

let with_tmp_path name f =
  let path = tmp_path name in
  let cleanup () =
    List.iter
      (fun p ->
         try Unix.unlink p with
         | _ -> ())
      [ path; path ^ "-wal"; path ^ ".aslog" ]
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () -> f path)
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "error in %S: %a" sql Db.pp_error e
;;

let render (v : Db.value) =
  match v with
  | Db.V_int n -> Int64.to_string n
  | Db.V_text s -> s
  | Db.V_real f -> Printf.sprintf "%g" f
  | Db.V_null -> "NULL"
  | Db.V_blob b -> Bytes.to_string b
;;

let rows db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query error in %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun row -> String.concat "|" (Array.to_list (Array.map render row)))
      (run (Lwt_stream.to_list stream))
;;

let check name expected actual = Alcotest.(check (list string)) name expected actual

(* Always restore the hook to its no-op default, even if [f] raises — it is a
   single GLOBAL ref (like [Db.rv_load_hook]), and leaving it armed would
   corrupt every rollback in every test that runs after this one in the same
   process. *)
let noop_hook () = Lwt.return_unit

let with_hook hook f =
  Cat.rollback_recompute_publish_hook := hook;
  Fun.protect ~finally:(fun () -> Cat.rollback_recompute_publish_hook := noop_hook) f
;;

(* THE #706 TEST.

   Setup gives the doomed transaction exactly one allocation on top of one
   already-committed row, so the recompute's correct answer (2) sits exactly
   one below the stale value the doomed transaction left behind (3) — the
   general shape (see the file header): the racing handle's commit uses the
   STALE value, so the very next allocation after an unfixed clobber is safe
   (id 2, never used) and the SECOND is where an unfixed publish reissues the
   racing handle's own committed id and silently overwrites its row.

   1. seed: one committed row, id 1, counter -> 2.
   2. BEGIN; INSERT the doomed row: id 2 allocated, counter (shared, published
      in-txn per #632's own note that [next_rowid_in_txn] publishes under the
      lock it already holds) -> 3.
   3. ROLLBACK. [S.rollback] reverts the tree (id 2's row gone) and releases
      the writer lock. The recompute's RO scan (unlocked) sees only id 1 and
      computes recovered = 2 — correct, if published before anyone else
      allocates.
   4. The hook fires in that exact window: a SECOND handle allocates from the
      still-stale shared counter (3), gets id 3, and COMMITS a row labelled
      'w'. Shared counter is now 4.
   5. The recompute resumes and tries to publish 2.
      - Fixed: a CAS against the [expected] value it captured before the RO
        scan (3) — the live counter is now 4, not 3, so the publish is
        skipped and the counter is left at 4 (the value the racing commit
        established, and the only value consistent with row id 3 already
        being taken).
      - Unfixed: the counter is blindly overwritten to 2, discarding the
        racing commit's advance.
   6. Two more autocommit inserts on the ORIGINAL handle. Unfixed, the first
      gets the harmless id 2 (never used) and the SECOND gets id 3 — the
      racing handle's row id — silently overwriting its 'w' label. Fixed,
      both continue from 4 and 5, colliding with nothing. *)
let test_rollback_recompute_cas_survives_a_concurrent_commit () =
  with_tmp_path "cas" (fun path ->
    let db =
      match run (Db.open_file_wal ~path ()) with
      | Ok d -> d
      | Error _ -> Alcotest.fail "open_file_wal failed"
    in
    exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
    let wdb = run (Db.create_worker_handle db) in
    exec db "INSERT INTO t (b) VALUES ('seed')";
    exec db "BEGIN";
    exec db "INSERT INTO t (b) VALUES ('doomed')";
    with_hook
      (fun () ->
         let* r = Db.execute wdb "INSERT INTO t (b) VALUES ('w')" in
         match r with
         | Ok () -> Lwt.return_unit
         | Error e -> Alcotest.failf "racing insert failed: %a" Db.pp_error e)
      (fun () -> exec db "ROLLBACK");
    exec db "INSERT INTO t (b) VALUES ('p1')";
    exec db "INSERT INTO t (b) VALUES ('p2')";
    check
      "the racing handle's committed row survives the rollback's recompute"
      [ "1|seed"; "3|w"; "4|p1"; "5|p2" ]
      (rows db "SELECT a, b FROM t ORDER BY a");
    run (Db.close db))
;;

(* Same shape, through the AUTOINCREMENT branch ([read_committed_next_rowid]
   rather than [recover_next_rowid]) — #706's fix touches both call sites in
   [recompute_rowid_counters_after_rollback] and they must not drift apart. *)
let test_rollback_recompute_cas_survives_autoincrement () =
  with_tmp_path "cas_autoinc" (fun path ->
    let db =
      match run (Db.open_file_wal ~path ()) with
      | Ok d -> d
      | Error _ -> Alcotest.fail "open_file_wal failed"
    in
    exec db "CREATE TABLE t (a INTEGER PRIMARY KEY AUTOINCREMENT, b TEXT)";
    let wdb = run (Db.create_worker_handle db) in
    exec db "INSERT INTO t (b) VALUES ('seed')";
    exec db "BEGIN";
    exec db "INSERT INTO t (b) VALUES ('doomed')";
    with_hook
      (fun () ->
         let* r = Db.execute wdb "INSERT INTO t (b) VALUES ('w')" in
         match r with
         | Ok () -> Lwt.return_unit
         | Error e -> Alcotest.failf "racing insert failed: %a" Db.pp_error e)
      (fun () -> exec db "ROLLBACK");
    exec db "INSERT INTO t (b) VALUES ('p1')";
    exec db "INSERT INTO t (b) VALUES ('p2')";
    check
      "AUTOINCREMENT: the racing handle's committed row survives too"
      [ "1|seed"; "3|w"; "4|p1"; "5|p2" ]
      (rows db "SELECT a, b FROM t ORDER BY a");
    run (Db.close db))
;;

(* A control: with no racing handle, the recompute must still lower the
   counter back to the correct value (the whole point of #293/#589) — the
   CAS must not turn into a permanent refusal to correct anything. *)
let test_rollback_recompute_still_corrects_with_no_race () =
  with_tmp_path "no_race" (fun path ->
    let db =
      match run (Db.open_file_wal ~path ()) with
      | Ok d -> d
      | Error _ -> Alcotest.fail "open_file_wal failed"
    in
    exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
    exec db "INSERT INTO t (b) VALUES ('seed')";
    exec db "BEGIN";
    exec db "INSERT INTO t (b) VALUES ('doomed')";
    exec db "ROLLBACK";
    exec db "INSERT INTO t (b) VALUES ('p1')";
    check
      "the doomed id (2) is reused, matching pre-#706 (and SQLite) behaviour"
      [ "1|seed"; "2|p1" ]
      (rows db "SELECT a, b FROM t ORDER BY a");
    run (Db.close db))
;;

let () =
  Alcotest.run
    "test_rollback_recompute_publish_race_706"
    [ ( "706"
      , [ Alcotest.test_case
            "rollback_recompute_cas_survives_a_concurrent_commit"
            `Quick
            test_rollback_recompute_cas_survives_a_concurrent_commit
        ; Alcotest.test_case
            "rollback_recompute_cas_survives_autoincrement"
            `Quick
            test_rollback_recompute_cas_survives_autoincrement
        ; Alcotest.test_case
            "rollback_recompute_still_corrects_with_no_race"
            `Quick
            test_rollback_recompute_still_corrects_with_no_race
        ] )
    ]
;;
