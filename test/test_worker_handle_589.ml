(** #589: [Db.create_worker_handle] must not give each handle its own rowid
    counter.

    A worker handle is [of_store] over the SAME [Store.t], but [of_store] builds
    a fresh catalog, and a catalog used to cache each rowid table's
    [next_rowid]. Two handles then held two independent counters over one shared
    data tree, so an engine-assigned rowid could be allocated twice and the
    second write silently overwrote the first — one row where there should be
    two, and for a [TEXT PRIMARY KEY] table an index left pointing at the wrong
    row.

    This file walks the full table-shape matrix from the issue, both in memory
    and on disk, and pins the fixed behaviour. Every case here failed before the
    fix except the two the issue records as already-correct
    ([INTEGER PRIMARY KEY] with caller-supplied ids, and [WITHOUT ROWID]) —
    those are kept as regression guards. *)

open Lwt.Syntax

module Db = struct
  include Granary.Db

  let open_file = Granary_unix.open_file
end

let () = Granary_unix.install ()
let run = Lwt_main.run
let fresh_db () = run (Db.open_in_memory ())
let db_counter = ref 0

let fresh_file_db () =
  let n = !db_counter in
  incr db_counter;
  let path = Printf.sprintf "/tmp/granary_wh589_test_%04d.db" n in
  (try Unix.unlink path with
   | _ -> ());
  match run (Db.open_file ~path ()) with
  | Ok db -> db, path
  | Error _ -> Alcotest.fail "open_file failed"
;;

let rows_of stream = run (Lwt_stream.to_list stream)

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "error in %S: %a" sql Db.pp_error e
;;

let is_err = function
  | Error _ -> true
  | Ok _ -> false
;;

let render (v : Db.value) =
  match v with
  | Db.V_int n -> Int64.to_string n
  | Db.V_text s -> s
  | Db.V_real f -> Printf.sprintf "%g" f
  | Db.V_null -> "NULL"
  | Db.V_blob b -> Bytes.to_string b
;;

(* Every row rendered as a "|"-joined string, so one helper covers every shape
   in the matrix. *)
let rows db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query error in %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map
      (fun row -> String.concat "|" (Array.to_list (Array.map render row)))
      (rows_of stream)
;;

let check name expected actual = Alcotest.(check (list string)) name expected actual

(* ------------------------------------------------------------------ *)
(* The shape matrix                                                     *)
(* ------------------------------------------------------------------ *)

(* Shape 1: plain rowid table — the commonest shape, and the one the issue
   measured as leaving a single row. *)
let test_plain_rowid () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (b) VALUES ('x')";
  exec wdb "INSERT INTO t (b) VALUES ('y')";
  check "both rows survive" [ "x"; "y" ] (rows db "SELECT b FROM t ORDER BY b ASC");
  check
    "and the worker sees both"
    [ "x"; "y" ]
    (rows wdb "SELECT b FROM t ORDER BY b ASC")
;;

(* Shape 2: INTEGER PRIMARY KEY, engine-assigned. *)
let test_integer_pk_engine_assigned () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (b) VALUES ('x')";
  exec wdb "INSERT INTO t (b) VALUES ('y')";
  check
    "the worker allocated 2, not 1 again"
    [ "1|x"; "2|y" ]
    (rows db "SELECT a, b FROM t ORDER BY a ASC")
;;

(* Shape 3: INTEGER PRIMARY KEY with caller-supplied ids — correct before the
   fix; kept as a regression guard. *)
let test_integer_pk_caller_supplied () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (a, b) VALUES (1, 'x')";
  exec wdb "INSERT INTO t (a, b) VALUES (2, 'y')";
  check
    "caller-supplied ids still fine"
    [ "1|x"; "2|y" ]
    (rows db "SELECT a, b FROM t ORDER BY a ASC")
;;

(* Shape 4: AUTOINCREMENT. [sqlite_sequence] is a view over the same counter, so
   it must read the same fresh value on BOTH handles. *)
let test_autoincrement () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY AUTOINCREMENT, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (b) VALUES ('x')";
  exec wdb "INSERT INTO t (b) VALUES ('y')";
  check
    "both rows, monotonic ids"
    [ "1|x"; "2|y" ]
    (rows db "SELECT a, b FROM t ORDER BY a ASC");
  check
    "sqlite_sequence on the parent"
    [ "t|2" ]
    (rows db "SELECT name, seq FROM sqlite_sequence");
  check
    "sqlite_sequence on the worker"
    [ "t|2" ]
    (rows wdb "SELECT name, seq FROM sqlite_sequence")
;;

(* Shape 5: TEXT PRIMARY KEY — the worst case, because the damage was wrong
   ANSWERS rather than a missing row: the PK index kept a phantom entry for the
   overwritten key pointing at the reused rowid. *)
let test_text_pk () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (k TEXT PRIMARY KEY, b TEXT)";
  exec db "CREATE INDEX t_b ON t (b)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (k, b) VALUES ('k1', 'x')";
  exec wdb "INSERT INTO t (k, b) VALUES ('k2', 'y')";
  check "scan" [ "k1|x"; "k2|y" ] (rows db "SELECT k, b FROM t ORDER BY k ASC");
  check
    "PK-index seek returns its own row"
    [ "k1|x" ]
    (rows db "SELECT k, b FROM t WHERE k = 'k1'");
  check
    "secondary-index seek returns its own row"
    [ "k1|x" ]
    (rows db "SELECT k, b FROM t WHERE b = 'x'");
  (* The UNIQUE violation here is now the real one: 'k1' genuinely exists. *)
  Alcotest.(check bool)
    "re-inserting an existing key still conflicts"
    true
    (is_err (run (Db.execute db "INSERT INTO t (k, b) VALUES ('k1', 'z')")));
  (* …and a key that does NOT exist inserts cleanly, which it could not while
     the phantom index entry was there. *)
  exec db "INSERT INTO t (k, b) VALUES ('k3', 'z')";
  check
    "third key lands"
    [ "k1|x"; "k2|y"; "k3|z" ]
    (rows db "SELECT k, b FROM t ORDER BY k ASC")
;;

(* Shape 6: WITHOUT ROWID — correct before the fix (it has no engine-assigned
   rowid at all); kept as a regression guard. *)
let test_without_rowid () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT) WITHOUT ROWID";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (a, b) VALUES (1, 'x')";
  exec wdb "INSERT INTO t (a, b) VALUES (2, 'y')";
  check
    "WITHOUT ROWID unaffected"
    [ "1|x"; "2|y" ]
    (rows db "SELECT a, b FROM t ORDER BY a ASC")
;;

(* ------------------------------------------------------------------ *)
(* The properties the issue calls out beyond the matrix                 *)
(* ------------------------------------------------------------------ *)

(* Symmetric and unbounded: it was never a one-shot worker-is-stale bug. A
   worker created AFTER the parent's rows snapshotted correctly and then the
   PARENT went stale and overwrote the worker's row. Three handles, five
   inserts, two surviving rows. *)
let test_three_handles_five_inserts () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let w1 = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (b) VALUES ('p1')";
  exec w1 "INSERT INTO t (b) VALUES ('w1a')";
  (* w2 is created here, i.e. AFTER rows already exist. *)
  let w2 = run (Db.create_worker_handle db) in
  exec w2 "INSERT INTO t (b) VALUES ('w2a')";
  exec db "INSERT INTO t (b) VALUES ('p2')";
  exec w1 "INSERT INTO t (b) VALUES ('w1b')";
  check
    "all five rows, five distinct rowids"
    [ "1|p1"; "2|w1a"; "3|w2a"; "4|p2"; "5|w1b" ]
    (rows db "SELECT a, b FROM t ORDER BY a ASC");
  check "count" [ "5" ] (rows w2 "SELECT COUNT(*) FROM t")
;;

(* THE TREE-ID IDENTITY TESTS.

   The shared counter is keyed by tree id, and it is tempting to justify that
   with "tree ids are unique". They are not. [next_user_tid_tx] writes the
   bumped counter INSIDE the transaction, so a rolled-back CREATE TABLE reverts
   it and the NEXT create gets the same id the doomed table had (measured:
   doomed 17, rolled back, fresh 17 — whereas a committed DROP of tid 16 leaves
   the next create at 18).

   What keeps a recreated table from inheriting a dead table's counter is that
   the entry is cleared or overwritten before the reused id is allocated from,
   and TWO REDUNDANT MECHANISMS do that: the DDL undo's [del_meta] ->
   [unpublish], and the replacement CREATE publishing [empty_next_rowid] under
   the same tree id. Verified by mutation: removing either one alone still
   passes these tests; removing BOTH makes them fail. So they are a guard on
   the PAIR — do not read them as proof that either mechanism is individually
   load-bearing. Both pass on main too (main shares no counter at all, so it
   has nothing to leak); their value is as a mutation guard on this file. *)
let test_tid_reuse_after_rolled_back_create () =
  let db = fresh_db () in
  exec db "BEGIN";
  exec db "CREATE TABLE doomed (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "INSERT INTO doomed (b) VALUES ('p')";
  exec db "INSERT INTO doomed (b) VALUES ('q')";
  exec db "INSERT INTO doomed (b) VALUES ('r')";
  exec db "ROLLBACK";
  (* [fresh] gets the SAME tree id [doomed] had. If the rollback had left
     [doomed]'s counter published, [fresh] would start at rowid 4. *)
  exec db "CREATE TABLE fresh (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "INSERT INTO fresh (b) VALUES ('x')";
  check "a fresh table starts at rowid 1" [ "1|x" ] (rows db "SELECT a, b FROM fresh")
;;

let test_tid_reuse_worker () =
  let db = fresh_db () in
  exec db "BEGIN";
  exec db "CREATE TABLE doomed (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "INSERT INTO doomed (b) VALUES ('p')";
  exec db "INSERT INTO doomed (b) VALUES ('q')";
  exec db "ROLLBACK";
  exec db "CREATE TABLE fresh (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO fresh (b) VALUES ('x')";
  exec wdb "INSERT INTO fresh (b) VALUES ('y')";
  check
    "reused tree id, both handles, no collision"
    [ "1|x"; "2|y" ]
    (rows db "SELECT a, b FROM fresh ORDER BY a")
;;

(* RENAME must carry the counter, because the key is the tree id and the tree id
   does not move. A worker opened after the rename must land on the same
   entry. *)
let test_rename_then_new_worker () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "INSERT INTO t (b) VALUES ('x')";
  exec db "ALTER TABLE t RENAME TO t2";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t2 (b) VALUES ('y')";
  exec wdb "INSERT INTO t2 (b) VALUES ('z')";
  check
    "no collision after rename"
    [ "1|x"; "2|y"; "3|z" ]
    (rows db "SELECT a, b FROM t2 ORDER BY a")
;;

(* DROP then CREATE with the same NAME must not inherit the counter — the whole
   reason the key is a tree id rather than a name. *)
let test_drop_create_worker () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "INSERT INTO t (b) VALUES ('x')";
  exec db "DROP TABLE t";
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (b) VALUES ('p')";
  exec wdb "INSERT INTO t (b) VALUES ('q')";
  check
    "recreated table starts at 1 on both handles"
    [ "1|p"; "2|q" ]
    (rows db "SELECT a, b FROM t ORDER BY a")
;;

(* #303's savepoint counter snapshot/restore runs through [restore_rowids],
   which is one of the chokepoints. ROLLBACK TO must lower the SHARED counter,
   and must not disturb an unrelated table the worker is using. *)
let test_savepoint_rollback_to () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "CREATE TABLE u (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec wdb "INSERT INTO u (b) VALUES ('u1')";
  exec db "BEGIN";
  exec db "SAVEPOINT sp";
  exec db "INSERT INTO t (b) VALUES ('doomed')";
  exec db "ROLLBACK TO sp";
  exec db "INSERT INTO t (b) VALUES ('kept')";
  exec db "COMMIT";
  exec wdb "INSERT INTO u (b) VALUES ('u2')";
  exec wdb "INSERT INTO t (b) VALUES ('t2')";
  check
    "unrelated table untouched"
    [ "1|u1"; "2|u2" ]
    (rows db "SELECT a, b FROM u ORDER BY a");
  check
    "rolled-back rowid reused, then the worker continues"
    [ "1|kept"; "2|t2" ]
    (rows db "SELECT a, b FROM t ORDER BY a")
;;

(* #299: AUTOINCREMENT's high-water is sticky across a COMMITTED delete. That is
   a property of the same counter, so it has to survive the sharing — and be
   visible to the handle that did not do the deleting. *)
let test_autoincrement_sticky_delete () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY AUTOINCREMENT, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (b) VALUES ('x')";
  exec db "INSERT INTO t (b) VALUES ('y')";
  exec db "DELETE FROM t";
  exec wdb "INSERT INTO t (b) VALUES ('z')";
  check "high-water stayed, on the other handle" [ "3|z" ] (rows db "SELECT a, b FROM t")
;;

(* TWO FIBERS, TWO HANDLES — the path #589 was actually found through, and the
   one the sequential tests below do NOT exercise: the second fiber blocks on
   the store's [Rwlock] rather than running to completion first.

   "row count = max rowid = 2n" is the assertion that matters: any reused rowid
   makes the count smaller than the max. *)
let test_two_fiber_autocommit () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  let n = 50 in
  run
    (let fiber h tag =
       let rec loop i =
         if i > n
         then Lwt.return_unit
         else
           let* r =
             Db.execute h (Printf.sprintf "INSERT INTO t (b) VALUES ('%s%d')" tag i)
           in
           match r with
           | Ok () -> loop (i + 1)
           | Error e -> Alcotest.failf "%s %d: %a" tag i Db.pp_error e
       in
       loop 1
     in
     Lwt.join [ fiber db "p"; fiber wdb "w" ]);
  check "every row landed" [ string_of_int (2 * n) ] (rows db "SELECT COUNT(*) FROM t");
  check "no rowid reused" [ string_of_int (2 * n) ] (rows db "SELECT MAX(a) FROM t")
;;

(* Same, with an explicit transaction per insert on both handles: the second
   fiber's BEGIN blocks on the shared writer lock, and neither handle is ever
   poisoned (#555). *)
let test_two_fiber_explicit_txn () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  let n = 20 in
  run
    (let fiber h tag =
       let rec loop i =
         if i > n
         then Lwt.return_unit
         else
           let* _ = Db.execute h "BEGIN" in
           let* r =
             Db.execute h (Printf.sprintf "INSERT INTO t (b) VALUES ('%s%d')" tag i)
           in
           let* c = Db.execute h "COMMIT" in
           match r, c with
           | Ok (), Ok () -> loop (i + 1)
           | _ -> Alcotest.failf "%s %d failed" tag i
       in
       loop 1
     in
     Lwt.join [ fiber db "p"; fiber wdb "w" ]);
  check "every row landed" [ string_of_int (2 * n) ] (rows db "SELECT COUNT(*) FROM t");
  check "no rowid reused" [ string_of_int (2 * n) ] (rows db "SELECT MAX(a) FROM t");
  Alcotest.(check bool) "parent not poisoned" false (Db.transaction_poisoned db);
  Alcotest.(check bool) "worker not poisoned" false (Db.transaction_poisoned wdb)
;;

(* Explicit transactions on both handles, STRICTLY SEQUENTIAL on one fiber: this
   is the issue's own sequence, and it deliberately does NOT exercise the
   [Rwlock] blocking path — [test_two_fiber_explicit_txn] above does that. Each
   handle keeps its own transaction slot (always sound — see
   [test_worker_handle_serializes_without_poison] in test_txn.ml); what is new
   is that the engine-assigned rowids no longer collide. *)
let test_explicit_transactions_both_handles () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "BEGIN";
  exec db "INSERT INTO t (b) VALUES ('x')";
  exec db "COMMIT";
  exec wdb "BEGIN";
  exec wdb "INSERT INTO t (b) VALUES ('y')";
  exec wdb "COMMIT";
  check
    "both transactions' rows survive"
    [ "1|x"; "2|y" ]
    (rows db "SELECT a, b FROM t ORDER BY a ASC");
  Alcotest.(check bool) "parent not poisoned" false (Db.transaction_poisoned db);
  Alcotest.(check bool) "worker not poisoned" false (Db.transaction_poisoned wdb)
;;

(* A ROLLBACK on one handle must still release the rowid it allocated (#293),
   and the OTHER handle must then reuse it rather than skip past it — i.e. the
   shared counter follows the rollback recompute too. *)
let test_rollback_reverts_shared_counter () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (b) VALUES ('x')";
  exec wdb "BEGIN";
  exec wdb "INSERT INTO t (b) VALUES ('doomed')";
  exec wdb "ROLLBACK";
  exec db "INSERT INTO t (b) VALUES ('y')";
  check
    "the rolled-back rowid 2 is reused, not skipped"
    [ "1|x"; "2|y" ]
    (rows db "SELECT a, b FROM t ORDER BY a ASC")
;;

(* On disk, and across a close/reopen — the issue records that the corruption
   persisted and survived reopening. *)
let test_file_backed_and_reopen () =
  let db, path = fresh_file_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (b) VALUES ('x')";
  exec wdb "INSERT INTO t (b) VALUES ('y')";
  exec wdb "INSERT INTO t (b) VALUES ('z')";
  check
    "on disk, live"
    [ "1|x"; "2|y"; "3|z" ]
    (rows db "SELECT a, b FROM t ORDER BY a ASC");
  run (Db.close db);
  let db2 =
    match run (Db.open_file ~path ()) with
    | Ok d -> d
    | Error _ -> Alcotest.fail "reopen failed"
  in
  check
    "after close/reopen"
    [ "1|x"; "2|y"; "3|z" ]
    (rows db2 "SELECT a, b FROM t ORDER BY a ASC");
  exec db2 "INSERT INTO t (b) VALUES ('w')";
  check
    "the reopened counter continues from the tree"
    [ "1|x"; "2|y"; "3|z"; "4|w" ]
    (rows db2 "SELECT a, b FROM t ORDER BY a ASC");
  run (Db.close db2);
  try Unix.unlink path with
  | _ -> ()
;;

(* #620 x #589. Since #599/#620, `OR IGNORE` skips a NOT NULL violation at
   RUNTIME rather than rejecting the statement at bind time, so a multi-row
   INSERT can now skip individual rows mid-statement — on the very insert path
   whose rowid allocator is shared between handles.

   A skipped row still CONSUMES its rowid, leaving a gap (1, 3 below). That is
   #620's single-handle behaviour and is not what this pins. What this pins is
   that the consumption is published to the SHARED counter, so the other handle
   continues past the gap instead of reusing it: the worker's first insert lands
   at 4, not at 1 or 2. With a per-handle counter it would have gone back to 1
   and overwritten. Interleaved on both handles the rowids stay distinct. *)
let test_or_ignore_skip_consumes_shared_rowid () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT NOT NULL)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT OR IGNORE INTO t (b) VALUES ('x'), (NULL), ('y')";
  check
    "the skipped row consumed rowid 2"
    [ "1|x"; "3|y" ]
    (rows db "SELECT a, b FROM t ORDER BY a");
  exec wdb "INSERT INTO t (b) VALUES ('w1')";
  check
    "the worker continues past the gap, it does not reuse it"
    [ "1|x"; "3|y"; "4|w1" ]
    (rows db "SELECT a, b FROM t ORDER BY a");
  exec wdb "INSERT OR IGNORE INTO t (b) VALUES ('w2'), (NULL), ('w3')";
  exec db "INSERT OR IGNORE INTO t (b) VALUES (NULL), ('p2')";
  check
    "interleaved OR IGNORE on both handles, no rowid reused"
    [ "1|x"; "3|y"; "4|w1"; "5|w2"; "7|w3"; "9|p2" ]
    (rows db "SELECT a, b FROM t ORDER BY a");
  check
    "no duplicate rowids"
    []
    (rows db "SELECT a FROM t GROUP BY a HAVING COUNT(*) > 1")
;;

(* A table CREATEd after the worker exists is invisible to the worker's schema
   cache — that limit is inherent to the per-handle catalog and is NOT fixed
   here (it is documented on [create_worker_handle]). Pinned so a later change
   that quietly fixes or worsens it is noticed. *)
let test_ddl_still_invisible_across_handles () =
  let db = fresh_db () in
  let wdb = run (Db.create_worker_handle db) in
  exec db "CREATE TABLE later (a INTEGER PRIMARY KEY, b TEXT)";
  Alcotest.(check bool)
    "DDL on the parent is still invisible to the worker"
    true
    (is_err (run (Db.query wdb "SELECT a FROM later")))
;;

(* Two handles taking turns for many rows: the counter has to stay shared for
   the whole run, not just the first hand-off. *)
let test_alternating_inserts () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  let n = 25 in
  run
    (let rec loop i =
       if i > n
       then Lwt.return_unit
       else (
         let h = if i mod 2 = 0 then db else wdb in
         let* r = Db.execute h (Printf.sprintf "INSERT INTO t (b) VALUES ('r%d')" i) in
         match r with
         | Ok () -> loop (i + 1)
         | Error e -> Alcotest.failf "insert %d: %a" i Db.pp_error e)
     in
     loop 1);
  check "every row landed" [ string_of_int n ] (rows db "SELECT COUNT(*) FROM t")
;;

let () =
  Alcotest.run
    "test_worker_handle_589"
    [ ( "shape_matrix"
      , [ Alcotest.test_case "plain_rowid" `Quick test_plain_rowid
        ; Alcotest.test_case
            "integer_pk_engine_assigned"
            `Quick
            test_integer_pk_engine_assigned
        ; Alcotest.test_case
            "integer_pk_caller_supplied"
            `Quick
            test_integer_pk_caller_supplied
        ; Alcotest.test_case "autoincrement" `Quick test_autoincrement
        ; Alcotest.test_case "text_pk" `Quick test_text_pk
        ; Alcotest.test_case "without_rowid" `Quick test_without_rowid
        ] )
    ; ( "tree_id_identity"
      , [ Alcotest.test_case
            "tid_reuse_after_rolled_back_create"
            `Quick
            test_tid_reuse_after_rolled_back_create
        ; Alcotest.test_case "tid_reuse_worker" `Quick test_tid_reuse_worker
        ; Alcotest.test_case "rename_then_new_worker" `Quick test_rename_then_new_worker
        ; Alcotest.test_case "drop_create_worker" `Quick test_drop_create_worker
        ] )
    ; ( "txn_boundaries"
      , [ Alcotest.test_case "savepoint_rollback_to" `Quick test_savepoint_rollback_to
        ; Alcotest.test_case
            "autoincrement_sticky_delete"
            `Quick
            test_autoincrement_sticky_delete
        ] )
    ; ( "two_fibers"
      , [ Alcotest.test_case "two_fiber_autocommit" `Quick test_two_fiber_autocommit
        ; Alcotest.test_case "two_fiber_explicit_txn" `Quick test_two_fiber_explicit_txn
        ] )
    ; ( "beyond_the_matrix"
      , [ Alcotest.test_case
            "three_handles_five_inserts"
            `Quick
            test_three_handles_five_inserts
        ; Alcotest.test_case
            "explicit_transactions_both_handles"
            `Quick
            test_explicit_transactions_both_handles
        ; Alcotest.test_case
            "rollback_reverts_shared_counter"
            `Quick
            test_rollback_reverts_shared_counter
        ; Alcotest.test_case "file_backed_and_reopen" `Quick test_file_backed_and_reopen
        ; Alcotest.test_case "alternating_inserts" `Quick test_alternating_inserts
        ; Alcotest.test_case
            "or_ignore_skip_consumes_shared_rowid"
            `Quick
            test_or_ignore_skip_consumes_shared_rowid
        ] )
    ; ( "still_unsafe"
      , [ Alcotest.test_case
            "ddl_still_invisible_across_handles"
            `Quick
            test_ddl_still_invisible_across_handles
        ] )
    ]
;;
