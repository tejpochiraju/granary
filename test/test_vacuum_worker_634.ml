(** #634: [Db.vacuum] silently invalidated every outstanding worker handle.

    VACUUM closes the handle's [Store.t], rebuilds the database file and swaps a
    freshly opened store into the handle that ran it. Every handle produced by
    [Db.create_worker_handle] shares that {e same} [Store.t], so after a VACUUM
    it points at a CLOSED store — and, since #589/PR #616, also at the
    pre-VACUUM [Schema_cache.rowid_counters] table, which describes a file that
    no longer exists. Nothing detected that and nothing told the worker: the
    failure surfaced later and somewhere else, as a closed-store error or a bad
    rowid.

    Since #634 the invalidation is LOUD (the issue's option 2). Handles over one
    store share a [Db.store_cohort]; VACUUM bumps it and re-stamps only the
    handle that ran it, so every sibling is stale and every statement on it —
    read, write, DDL, prepared [run]/[iter], [BEGIN], [COMMIT] {e and}
    [ROLLBACK] — is refused with an error naming VACUUM. Unlike #555's poison
    there is no recovery: [ROLLBACK] is refused too, because the store the
    transaction lived in is gone. [close] is the only thing left to do with a
    stale handle, and it is a no-op on the already-closed store.

    Every assertion below fails before the fix, in the two ways the issue
    describes: the statements either succeed against a dead store or fail with
    an error that says nothing about VACUUM. *)

open Lwt.Syntax

module Db = struct
  include Granary.Db

  let open_file = Granary_unix.open_file
end

let () = Granary_unix.install ()
let run = Lwt_main.run
let db_counter = ref 0

let fresh_file_db () =
  let n = !db_counter in
  incr db_counter;
  let path = Printf.sprintf "/tmp/granary_vac634_test_%04d.db" n in
  List.iter
    (fun suffix ->
       try Unix.unlink (path ^ suffix) with
       | _ -> ())
    [ ""; "-wal"; ".aslog"; ".vacuum-tmp"; ".vacuum-tmp-wal" ];
  match run (Db.open_file ~path ()) with
  | Ok db -> db, path
  | Error e -> Alcotest.failf "open_file: %a" Db.pp_error e
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

(* Does [hay] contain [needle]?  The gate's whole value is that the message
   names VACUUM, so the tests assert on the text and not merely on [Error]. *)
let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i =
    i + nl <= hl && (String.equal (String.sub hay i nl) needle || go (i + 1))
  in
  go 0
;;

(* Render the error of a result that MUST be an error. *)
let err_msg what = function
  | Ok _ -> Alcotest.failf "%s: expected an error, got Ok" what
  | Error e -> Format.asprintf "%a" Db.pp_error e
;;

(* Assert that [r] failed with the #634 stale-handle error. *)
let check_stale what r =
  let msg = err_msg what r in
  Alcotest.(check bool) (what ^ ": names VACUUM") true (contains ~needle:"VACUUM" msg);
  Alcotest.(check bool) (what ^ ": cites #634") true (contains ~needle:"#634" msg)
;;

(* ------------------------------------------------------------------ *)
(* The core case from the issue                                         *)
(* ------------------------------------------------------------------ *)

(* Every statement shape on an outstanding worker is refused after the parent
   vacuums, and the refusal names VACUUM.  Before the fix these ran against a
   closed store. *)
let test_worker_refused_after_vacuum () =
  let db, _path = fresh_file_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "INSERT INTO t (b) VALUES ('x')";
  let wdb = run (Db.create_worker_handle db) in
  check "worker sees the row before VACUUM" [ "1|x" ] (rows wdb "SELECT a, b FROM t");
  Alcotest.(check bool) "worker live before VACUUM" false (Db.stale_after_vacuum wdb);
  exec db "VACUUM";
  Alcotest.(check bool) "worker stale after VACUUM" true (Db.stale_after_vacuum wdb);
  check_stale "worker SELECT" (run (Db.query wdb "SELECT a, b FROM t"));
  check_stale "worker INSERT" (run (Db.execute wdb "INSERT INTO t (b) VALUES ('y')"));
  check_stale "worker DDL" (run (Db.execute wdb "CREATE TABLE u (x INT)"));
  check_stale "worker BEGIN" (run (Db.execute wdb "BEGIN"));
  check_stale "worker COMMIT" (run (Db.execute wdb "COMMIT"));
  (* The one that differs from #555's poison: ROLLBACK is NOT an exit here. *)
  check_stale "worker ROLLBACK" (run (Db.execute wdb "ROLLBACK"));
  check_stale "worker still refused after ROLLBACK" (run (Db.query wdb "SELECT a FROM t"));
  check_stale
    "worker change-count path"
    (run (Db.execute_change_count wdb "DELETE FROM t"));
  (* And the parent, which ran the VACUUM, is untouched. *)
  Alcotest.(check bool) "parent not stale" false (Db.stale_after_vacuum db);
  check "parent still reads" [ "1|x" ] (rows db "SELECT a, b FROM t");
  exec db "INSERT INTO t (b) VALUES ('z')";
  check "parent still writes" [ "1|x"; "2|z" ] (rows db "SELECT a, b FROM t ORDER BY a");
  run (Db.close wdb);
  run (Db.close db)
;;

(* A worker taken AFTER the VACUUM is live, and shares the post-VACUUM rowid
   allocator with the parent (the #589 invariant, re-established over the
   rebuilt file). *)
let test_worker_created_after_vacuum_is_live () =
  let db, _path = fresh_file_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "INSERT INTO t (b) VALUES ('x')";
  let dead = run (Db.create_worker_handle db) in
  exec db "VACUUM";
  Alcotest.(check bool) "pre-VACUUM worker is stale" true (Db.stale_after_vacuum dead);
  let fresh = run (Db.create_worker_handle db) in
  Alcotest.(check bool) "post-VACUUM worker is live" false (Db.stale_after_vacuum fresh);
  exec fresh "INSERT INTO t (b) VALUES ('y')";
  exec db "INSERT INTO t (b) VALUES ('z')";
  (* #589: one allocator over one data tree.  If the fresh worker and the parent
     each had their own counter, 'z' would have reused rowid 2 and overwritten
     'y' — one row where there should be three. *)
  check
    "three rows, three distinct rowids"
    [ "1|x"; "2|y"; "3|z" ]
    (rows db "SELECT a, b FROM t ORDER BY a ASC");
  check
    "and the fresh worker agrees"
    [ "1|x"; "2|y"; "3|z" ]
    (rows fresh "SELECT a, b FROM t ORDER BY a ASC");
  run (Db.close dead);
  run (Db.close fresh);
  run (Db.close db)
;;

(* The rowid counter across the VACUUM boundary, in the style of
   [test_worker_handle_589.ml]: a plain rowid table (no INTEGER PRIMARY KEY) is
   the shape whose counter is recomputed from the data tree at open time, so it
   is the one that shows whether the rebuilt file's allocator picked up where
   the old one left off.  VACUUM preserves tree ids ([copy_all_trees] writes
   each tid to the same tid), so the "counters are keyed by tree id" rule is not
   disturbed — the whole counter TABLE is replaced, not remapped. *)
let test_rowid_counter_survives_vacuum () =
  let db, _path = fresh_file_db () in
  exec db "CREATE TABLE t (b TEXT)";
  exec db "INSERT INTO t (b) VALUES ('x')";
  exec db "INSERT INTO t (b) VALUES ('y')";
  let before = rows db "SELECT rowid, b FROM t ORDER BY rowid ASC" in
  check "two rows before" [ "1|x"; "2|y" ] before;
  let dead = run (Db.create_worker_handle db) in
  exec db "VACUUM";
  check_stale
    "stale worker cannot allocate a rowid"
    (run (Db.execute dead "INSERT INTO t (b) VALUES ('dead')"));
  let fresh = run (Db.create_worker_handle db) in
  exec fresh "INSERT INTO t (b) VALUES ('z')";
  check
    "the post-VACUUM allocator continued at 3, and nothing was overwritten"
    [ "1|x"; "2|y"; "3|z" ]
    (rows db "SELECT rowid, b FROM t ORDER BY rowid ASC");
  run (Db.close dead);
  run (Db.close fresh);
  run (Db.close db)
;;

(* AUTOINCREMENT: the high-water mark is persisted in [_sys_tables] and copied
   by the rebuild, so it must survive the VACUUM rather than restart. *)
let test_autoincrement_counter_survives_vacuum () =
  let db, _path = fresh_file_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY AUTOINCREMENT, b TEXT)";
  exec db "INSERT INTO t (b) VALUES ('x')";
  exec db "INSERT INTO t (b) VALUES ('y')";
  exec db "DELETE FROM t WHERE a = 2";
  let _dead = run (Db.create_worker_handle db) in
  exec db "VACUUM";
  let fresh = run (Db.create_worker_handle db) in
  exec fresh "INSERT INTO t (b) VALUES ('z')";
  check
    "AUTOINCREMENT stayed sticky across VACUUM"
    [ "1|x"; "3|z" ]
    (rows db "SELECT a, b FROM t ORDER BY a ASC");
  run (Db.close fresh);
  run (Db.close db)
;;

(* ------------------------------------------------------------------ *)
(* Prepared statements                                                  *)
(* ------------------------------------------------------------------ *)

(* A statement prepared on a worker before the VACUUM holds [db_ref] to the dead
   handle and a plan carrying the pre-VACUUM [table_meta].  db.mli's "prepared
   statements continue to work logically" note applies only to statements
   prepared on the handle that RAN the vacuum. *)
let test_prepared_on_worker_refused () =
  let db, _path = fresh_file_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "INSERT INTO t (b) VALUES ('x')";
  let wdb = run (Db.create_worker_handle db) in
  let sel =
    match run (Db.prepare wdb "SELECT a, b FROM t") with
    | Ok st -> st
    | Error e -> Alcotest.failf "prepare: %a" Db.pp_error e
  in
  let ins =
    match run (Db.prepare wdb "INSERT INTO t (b) VALUES (?)") with
    | Ok st -> st
    | Error e -> Alcotest.failf "prepare: %a" Db.pp_error e
  in
  exec db "VACUUM";
  check_stale "prepared iter on stale worker" (run (Db.iter sel ~params:[]));
  check_stale "prepared run on stale worker" (run (Db.run ins ~params:[ Db.V_text "y" ]));
  check_stale "prepare on stale worker" (run (Db.prepare wdb "SELECT 1"));
  (* A statement prepared on the vacuuming handle itself keeps working. *)
  let parent_sel =
    match run (Db.prepare db "SELECT a, b FROM t") with
    | Ok st -> st
    | Error e -> Alcotest.failf "prepare on parent: %a" Db.pp_error e
  in
  (match run (Db.iter parent_sel ~params:[]) with
   | Error e -> Alcotest.failf "parent iter: %a" Db.pp_error e
   | Ok stream ->
     let got =
       List.map
         (fun row -> String.concat "|" (Array.to_list (Array.map render row)))
         (run (Lwt_stream.to_list stream))
     in
     check "parent's prepared statement still runs" [ "1|x" ] got);
  run (Db.finalize sel);
  run (Db.finalize ins);
  run (Db.finalize parent_sel);
  run (Db.close wdb);
  run (Db.close db)
;;

(* ------------------------------------------------------------------ *)
(* Operations ON a stale handle                                         *)
(* ------------------------------------------------------------------ *)

(* VACUUM issued from a stale worker is refused rather than rebuilding the file
   out from under the live parent. *)
let test_vacuum_from_stale_worker_refused () =
  let db, _path = fresh_file_db () in
  exec db "CREATE TABLE t (x INT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "VACUUM";
  check_stale "VACUUM statement on stale worker" (run (Db.execute wdb "VACUUM"));
  let direct_raised =
    try
      run (Db.vacuum wdb);
      false
    with
    | Failure _ -> true
  in
  Alcotest.(check bool) "Db.vacuum on stale worker raises" true direct_raised;
  run (Db.close wdb);
  run (Db.close db)
;;

(* Deriving a worker from a stale handle would hand back a second handle over
   the same closed store. *)
let test_worker_of_stale_worker_refused () =
  let db, _path = fresh_file_db () in
  exec db "CREATE TABLE t (x INT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "VACUUM";
  let raised =
    try
      let _ = run (Db.create_worker_handle wdb) in
      false
    with
    | Failure _ -> true
  in
  Alcotest.(check bool) "create_worker_handle on stale handle raises" true raised;
  run (Db.close wdb);
  run (Db.close db)
;;

(* [close] is the ONE supported operation on a stale handle: the store it points
   at was already closed by the VACUUM, so closing must be a safe no-op rather
   than a second teardown of the same pager/WAL fds. *)
let test_close_stale_worker_is_safe () =
  let db, _path = fresh_file_db () in
  exec db "CREATE TABLE t (x INT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "VACUUM";
  run (Db.close wdb);
  (* Closing the stale worker must not have disturbed the live parent. *)
  exec db "INSERT INTO t (x) VALUES (7)";
  check "parent unaffected by the stale close" [ "7" ] (rows db "SELECT x FROM t");
  run (Db.close db)
;;

(* Two workers, one VACUUM: both die.  The cohort is shared by reference, so a
   worker OF a worker is in it too — the mechanism is per-store, not
   per-parent. *)
let test_whole_cohort_dies () =
  let db, _path = fresh_file_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let w1 = run (Db.create_worker_handle db) in
  let w2 = run (Db.create_worker_handle w1) in
  Alcotest.(check bool) "w1 live" false (Db.stale_after_vacuum w1);
  Alcotest.(check bool) "w2 live" false (Db.stale_after_vacuum w2);
  exec db "VACUUM";
  Alcotest.(check bool) "w1 stale" true (Db.stale_after_vacuum w1);
  Alcotest.(check bool) "w2 stale" true (Db.stale_after_vacuum w2);
  check_stale "w1 refused" (run (Db.query w1 "SELECT a FROM t"));
  check_stale "w2 refused" (run (Db.query w2 "SELECT a FROM t"));
  run (Db.close w1);
  run (Db.close w2);
  run (Db.close db)
;;

(* Pre-existing reality, pinned here because it bounds the fix: a worker handle
   is [of_store] WITHOUT [~file_path], so it has no path to rebuild and cannot
   run a VACUUM at all — the refusal is the file-backed one, not the #634 stale
   one.  That is why there is no "worker vacuums, parent goes stale" case in
   this file: the cohort would handle it symmetrically, but the statement never
   gets that far.  If [create_worker_handle] ever forwards [file_path], the
   symmetric case becomes reachable and wants its own test. *)
let test_live_worker_cannot_vacuum () =
  let db, _path = fresh_file_db () in
  exec db "CREATE TABLE t (x INT)";
  let wdb = run (Db.create_worker_handle db) in
  let msg = err_msg "VACUUM on a live worker" (run (Db.execute wdb "VACUUM")) in
  Alcotest.(check bool)
    "refused as non-file-backed, not as stale"
    true
    (contains ~needle:"file-backed" msg);
  Alcotest.(check bool) "worker is not stale" false (Db.stale_after_vacuum wdb);
  run (Db.close wdb);
  run (Db.close db)
;;

(* A worker that has been running explicit transactions — the reason
   [create_worker_handle] exists — is refused after the VACUUM at every point of
   the transaction protocol, ROLLBACK included.  That is the deliberate
   difference from #555's poison, where ROLLBACK is the prescribed recovery:
   here there is no store left to roll back into.

   The worker's transaction is committed BEFORE the vacuum on purpose.  VACUUM
   only refuses to run while the {e vacuuming} handle has a transaction open; it
   cannot see another handle's, and closing the store under a live writer is a
   separate hazard this test is not the place to provoke. *)
let test_worker_transaction_protocol_refused () =
  let db, _path = fresh_file_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "INSERT INTO t (b) VALUES ('x')";
  let wdb = run (Db.create_worker_handle db) in
  exec wdb "BEGIN";
  exec wdb "INSERT INTO t (b) VALUES ('committed')";
  exec wdb "COMMIT";
  exec db "VACUUM";
  check_stale "worker BEGIN after VACUUM" (run (Db.execute wdb "BEGIN"));
  check_stale "worker COMMIT after VACUUM" (run (Db.execute wdb "COMMIT"));
  check_stale "worker ROLLBACK after VACUUM" (run (Db.execute wdb "ROLLBACK"));
  check_stale "worker SAVEPOINT after VACUUM" (run (Db.execute wdb "SAVEPOINT s1"));
  Alcotest.(check bool)
    "worker still stale after ROLLBACK"
    true
    (Db.stale_after_vacuum wdb);
  (* The worker's committed row survived the rebuild. *)
  check
    "committed work survived the VACUUM"
    [ "1|x"; "2|committed" ]
    (rows db "SELECT a, b FROM t ORDER BY a ASC");
  run (Db.close wdb);
  run (Db.close db)
;;

let () =
  Alcotest.run
    "vacuum_worker_634"
    [ ( "invalidation"
      , [ Alcotest.test_case
            "every statement refused"
            `Quick
            test_worker_refused_after_vacuum
        ; Alcotest.test_case
            "worker taken after VACUUM is live"
            `Quick
            test_worker_created_after_vacuum_is_live
        ; Alcotest.test_case "whole cohort dies" `Quick test_whole_cohort_dies
        ; Alcotest.test_case
            "transaction protocol refused"
            `Quick
            test_worker_transaction_protocol_refused
        ] )
    ; ( "rowid counters"
      , [ Alcotest.test_case
            "plain rowid counter survives"
            `Quick
            test_rowid_counter_survives_vacuum
        ; Alcotest.test_case
            "AUTOINCREMENT high-water survives"
            `Quick
            test_autoincrement_counter_survives_vacuum
        ] )
    ; ( "prepared statements"
      , [ Alcotest.test_case
            "prepared on worker refused"
            `Quick
            test_prepared_on_worker_refused
        ] )
    ; ( "operations on a stale handle"
      , [ Alcotest.test_case "VACUUM refused" `Quick test_vacuum_from_stale_worker_refused
        ; Alcotest.test_case
            "create_worker_handle refused"
            `Quick
            test_worker_of_stale_worker_refused
        ; Alcotest.test_case "close is safe" `Quick test_close_stale_worker_is_safe
        ; Alcotest.test_case
            "a live worker cannot vacuum"
            `Quick
            test_live_worker_cannot_vacuum
        ] )
    ]
;;
