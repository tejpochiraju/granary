(** #632 + #633: who owns the rowid allocator, and when an allocation becomes
    visible.

    Both issues fell out of reviewing PR #616 (which closed #589 by lifting the
    rowid allocator out of the per-handle [table_meta] and into a table two
    catalogs over one store share).

    #633 — the table used to be threaded by hand:
    [Cat.open_ ?rowid_counters] / [Db.of_store ?rowid_counters], with
    [Db.create_worker_handle] as the single in-tree caller that remembered. The
    penalty for the next caller forgetting was #589 verbatim: silent row loss
    plus index corruption that persists to disk. The table now hangs off
    [Store.t], so sharing follows from naming the same store. The tests below
    reach [Db.of_store] and [Catalog.open_] DIRECTLY — no
    [create_worker_handle] — because that is precisely the path that used to be
    unsafe.

    #632 — [Catalog.next_rowid] (the autocommit allocator) published the bumped
    counter to the shared table BEFORE it took the writer lock and committed. In
    that window the allocation was visible to every other handle while not
    having happened: another handle could take the lock, ROLLBACK, and lower the
    counter through #293's recompute, handing the same rowid out twice. It now
    does the whole read-modify-write under the lock and publishes only after the
    commit succeeds.

    What every test here must keep true (see CLAUDE.md):
    - counters keyed by TREE ID, not by table name;
    - a tree id IS reused after a rolled-back CREATE, and the stale entry must be
      gone before the reused id allocates;
    - negative tree ids (-1/-2/-3) are never counted;
    - open-time seeding never clobbers a LIVE counter. *)

open Lwt.Syntax
module Store = Granary_store.Store
module Cat = Granary_catalog.Catalog
module Row = Granary_encoding.Row

module Db = struct
  include Granary.Db

  let open_file = Granary_unix.open_file
end

let () = Granary_unix.install ()
let run = Lwt_main.run
let fresh_db () = run (Db.open_in_memory ())

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

let int_col name : Row.column =
  { name
  ; ty = Row.Integer
  ; not_null = false
  ; primary_key = false
  ; pk_desc = false
  ; default = None
  ; check_sql = None
  ; generated_as = None
  }
;;

(* ------------------------------------------------------------------ *)
(* #632: an allocation is published only once it has committed          *)
(* ------------------------------------------------------------------ *)

(* THE #632 TEST. An engine-assigned rowid allocated inside a transaction that
   ROLLBACKs must not leave the shared counter advanced — and the check is made
   from a SECOND handle, because a per-handle counter would hide the defect
   behind the rolling-back handle's own recompute. *)
let test_rollback_leaves_shared_counter_where_it_was () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "BEGIN";
  exec db "INSERT INTO t (b) VALUES ('doomed')";
  exec db "ROLLBACK";
  (* The worker must see rowid 1 as free: nothing was ever committed. *)
  exec wdb "INSERT INTO t (b) VALUES ('w')";
  check
    "the rolled-back allocation left no mark on the shared counter"
    [ "1|w" ]
    (rows db "SELECT a, b FROM t ORDER BY a")
;;

(* Same, with several rows and a table that already holds committed data, so the
   counter has to come back to the committed mark rather than merely to 1. *)
let test_rollback_returns_to_the_committed_mark () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (b) VALUES ('x')";
  exec db "INSERT INTO t (b) VALUES ('y')";
  exec db "BEGIN";
  exec db "INSERT INTO t (b) VALUES ('d1')";
  exec db "INSERT INTO t (b) VALUES ('d2')";
  exec db "INSERT INTO t (b) VALUES ('d3')";
  exec db "ROLLBACK";
  exec wdb "INSERT INTO t (b) VALUES ('w')";
  check
    "the worker takes 3, the first id the rollback gave back"
    [ "1|x"; "2|y"; "3|w" ]
    (rows db "SELECT a, b FROM t ORDER BY a")
;;

(* The rollback happens on the WORKER and the parent is the one that must not
   have been dragged forward. Symmetric by construction now that there is one
   counter, but it was the asymmetry that made #589 unbounded, so pin both. *)
let test_worker_rollback_does_not_move_the_parent () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec wdb "BEGIN";
  exec wdb "INSERT INTO t (b) VALUES ('doomed')";
  exec wdb "ROLLBACK";
  exec db "INSERT INTO t (b) VALUES ('p')";
  check "the parent starts at 1" [ "1|p" ] (rows db "SELECT a, b FROM t ORDER BY a");
  exec wdb "INSERT INTO t (b) VALUES ('w')";
  check
    "and the worker follows at 2"
    [ "1|p"; "2|w" ]
    (rows db "SELECT a, b FROM t ORDER BY a")
;;

(* AUTOINCREMENT reverts on ROLLBACK (#299/#313: sticky across a COMMITTED
   delete, NOT across a rollback), and the revert has to be visible on the other
   handle — [sqlite_sequence] is a view over the same counter. *)
let test_autoincrement_rollback_visible_on_both_handles () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY AUTOINCREMENT, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (b) VALUES ('x')";
  exec db "BEGIN";
  exec db "INSERT INTO t (b) VALUES ('doomed')";
  exec db "ROLLBACK";
  check
    "sqlite_sequence on the parent is back at 1"
    [ "t|1" ]
    (rows db "SELECT name, seq FROM sqlite_sequence");
  check
    "and on the worker too"
    [ "t|1" ]
    (rows wdb "SELECT name, seq FROM sqlite_sequence");
  exec wdb "INSERT INTO t (b) VALUES ('w')";
  check
    "so the worker allocates 2, not 3"
    [ "1|x"; "2|w" ]
    (rows db "SELECT a, b FROM t ORDER BY a")
;;

(* [Catalog.next_rowid] is the autocommit allocator #632 is actually about. It
   now takes the writer lock, allocates, commits, and only THEN publishes, so
   two catalogs over one store hand out a strictly increasing sequence with no
   repeats and no gaps. (Before #633 these two catalogs had separate counters
   entirely and this read 1,1,2,2.) *)
let test_catalog_next_rowid_alternates_across_catalogs () =
  run
    (let store = Store.create () in
     let* cat1 = Cat.open_ store in
     let* _tid =
       Cat.create_table
         cat1
         ~name:"t"
         ~columns:[ int_col "id" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* cat2 = Cat.open_ store in
     let* a = Cat.next_rowid cat1 ~name:"t" in
     let* b = Cat.next_rowid cat2 ~name:"t" in
     let* c = Cat.next_rowid cat1 ~name:"t" in
     let* d = Cat.next_rowid cat2 ~name:"t" in
     Alcotest.(check (list int64))
       "one allocator, one sequence"
       [ 1L; 2L; 3L; 4L ]
       [ a; b; c; d ];
     Lwt.return_unit)
;;

(* The unknown-table failure stays SYNCHRONOUS. #632 moved the allocation under
   the writer lock, and the temptation is to move this check with it — which
   would turn a raise into a rejected promise and silently change the contract
   [test_catalog]'s [test_rowid_unknown_table] pins. *)
let test_catalog_next_rowid_unknown_table_raises_synchronously () =
  run
    (let store = Store.create () in
     let* cat = Cat.open_ store in
     (try
        ignore (Cat.next_rowid cat ~name:"nope");
        Alcotest.fail "expected a synchronous Failure for an unknown table"
      with
      | Failure _ -> ());
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* #633: the allocator belongs to the store                             *)
(* ------------------------------------------------------------------ *)

(* THE #633 TEST. [Db.of_store] over an already-open store, with NO argument
   passed and no [create_worker_handle] in sight, must still share the
   allocator. This is the exact call shape the old API left unsafe. *)
let test_bare_of_store_shares_the_allocator () =
  let store = Store.create () in
  let db1 = run (Db.of_store store) in
  exec db1 "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  (* Opened AFTER the DDL, so its own catalog load sees the table. *)
  let db2 = run (Db.of_store store) in
  exec db1 "INSERT INTO t (b) VALUES ('x')";
  exec db2 "INSERT INTO t (b) VALUES ('y')";
  check
    "a bare of_store handle does not restart the counter"
    [ "1|x"; "2|y" ]
    (rows db1 "SELECT a, b FROM t ORDER BY a")
;;

(* Plain rowid table — the commonest shape, and the one #589 measured as leaving
   a single row where there should be two. Same bare-[of_store] path. *)
let test_bare_of_store_plain_rowid () =
  let store = Store.create () in
  let db1 = run (Db.of_store store) in
  exec db1 "CREATE TABLE t (b TEXT)";
  let db2 = run (Db.of_store store) in
  exec db1 "INSERT INTO t (b) VALUES ('x')";
  exec db2 "INSERT INTO t (b) VALUES ('y')";
  check "both rows survive" [ "2" ] (rows db1 "SELECT COUNT(*) FROM t")
;;

(* TEXT PRIMARY KEY was #589's worst case: the damage was wrong ANSWERS, not a
   missing row, because the index kept a phantom entry pointing at the reused
   rowid. Re-run it through the bare [of_store] path. *)
let test_bare_of_store_text_pk () =
  let store = Store.create () in
  let db1 = run (Db.of_store store) in
  exec db1 "CREATE TABLE t (k TEXT PRIMARY KEY, b TEXT)";
  let db2 = run (Db.of_store store) in
  exec db1 "INSERT INTO t (k, b) VALUES ('k1', 'x')";
  exec db2 "INSERT INTO t (k, b) VALUES ('k2', 'y')";
  check "scan" [ "k1|x"; "k2|y" ] (rows db1 "SELECT k, b FROM t ORDER BY k");
  check
    "PK-index seek returns its own row"
    [ "k1|x" ]
    (rows db1 "SELECT k, b FROM t WHERE k = 'k1'");
  check "and the other one too" [ "k2|y" ] (rows db2 "SELECT k, b FROM t WHERE k = 'k2'")
;;

(* Two DIFFERENT stores must NOT share. Under the old API this was right by
   accident of how the call site was written; it is now right by type, since the
   table is reached through the store. ATTACH is the real-world case: an
   attached schema is its own [Store.t], so both databases start at rowid 1. *)
let test_two_stores_do_not_share () =
  let store_a = Store.create () in
  let store_b = Store.create () in
  let db_a = run (Db.of_store store_a) in
  let db_b = run (Db.of_store store_b) in
  exec db_a "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db_b "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db_a "INSERT INTO t (b) VALUES ('a1')";
  exec db_a "INSERT INTO t (b) VALUES ('a2')";
  exec db_b "INSERT INTO t (b) VALUES ('b1')";
  check "store A" [ "1|a1"; "2|a2" ] (rows db_a "SELECT a, b FROM t ORDER BY a");
  check
    "store B has its own counter and starts at 1"
    [ "1|b1" ]
    (rows db_b "SELECT a, b FROM t ORDER BY a")
;;

(* ------------------------------------------------------------------ *)
(* The invariants the ownership move must not break                     *)
(* ------------------------------------------------------------------ *)

(* KEYED BY TREE ID, NOT BY NAME (1): a tree id survives RENAME, so the counter
   must follow the table across one — including for a handle opened after it. *)
let test_rename_carries_the_counter () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "INSERT INTO t (b) VALUES ('x')";
  exec db "ALTER TABLE t RENAME TO t2";
  let wdb = run (Db.create_worker_handle db) in
  exec wdb "INSERT INTO t2 (b) VALUES ('y')";
  check
    "the renamed table keeps its counter"
    [ "1|x"; "2|y" ]
    (rows db "SELECT a, b FROM t2 ORDER BY a")
;;

(* KEYED BY TREE ID, NOT BY NAME (2): a DROP+CREATE of the same NAME gets a
   fresh tree id and must not inherit the dead table's counter. Keying by name
   would restart this at 2. *)
let test_drop_create_does_not_inherit () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "INSERT INTO t (b) VALUES ('old')";
  exec db "DROP TABLE t";
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec wdb "INSERT INTO t (b) VALUES ('new')";
  check
    "the recreated table starts at 1"
    [ "1|new" ]
    (rows db "SELECT a, b FROM t ORDER BY a")
;;

(* TREE IDS ARE REUSED AFTER A ROLLED-BACK CREATE. [next_user_tid_tx] bumps the
   id counter INSIDE the transaction, so the rollback gives the id back and the
   next CREATE gets it. TWO redundant mechanisms clear the stale entry first —
   [put_table]'s undo running [del_meta] -> [unpublish], and the replacement
   CREATE publishing [empty_next_rowid] under the same id — and each is
   sufficient alone, so THIS TEST PASSING PROVES NEITHER IS LIVE. It guards the
   pair. Do not delete one because the suite stayed green. *)
let test_tid_reuse_after_rolled_back_create () =
  let db = fresh_db () in
  exec db "BEGIN";
  exec db "CREATE TABLE doomed (a INTEGER PRIMARY KEY, b TEXT)";
  exec db "INSERT INTO doomed (b) VALUES ('p')";
  exec db "INSERT INTO doomed (b) VALUES ('q')";
  exec db "INSERT INTO doomed (b) VALUES ('r')";
  exec db "ROLLBACK";
  exec db "CREATE TABLE fresh (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec wdb "INSERT INTO fresh (b) VALUES ('x')";
  check
    "the reused tree id allocates from 1, not from the doomed table's 4"
    [ "1|x" ]
    (rows db "SELECT a, b FROM fresh")
;;

(* NEGATIVE TREE IDS ARE NEVER COUNTED. [sqlite_master] (-2) and
   [sqlite_sequence] (-3) name no data tree; if the [tree_id >= 0] guard went,
   they would all collide on one entry and drag real counters around with them.
   Read both sentinels from two handles while real allocation is going on. *)
let test_sentinel_tables_do_not_disturb_counters () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY AUTOINCREMENT, b TEXT)";
  exec db "CREATE TABLE u (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (b) VALUES ('t1')";
  ignore (rows db "SELECT name FROM sqlite_master ORDER BY name");
  ignore (rows wdb "SELECT name, seq FROM sqlite_sequence");
  exec wdb "INSERT INTO u (b) VALUES ('u1')";
  ignore (rows db "SELECT name FROM sqlite_master ORDER BY name");
  exec wdb "INSERT INTO t (b) VALUES ('t2')";
  exec db "INSERT INTO u (b) VALUES ('u2')";
  check
    "t is unaffected by the sentinels"
    [ "1|t1"; "2|t2" ]
    (rows db "SELECT a, b FROM t");
  check "and so is u" [ "1|u1"; "2|u2" ] (rows db "SELECT a, b FROM u")
;;

(* OPEN-TIME SEEDING MUST NOT CLOBBER A LIVE COUNTER. The second handle re-reads
   the catalog off disk, and disk is never fresher than a running allocator: if
   [Cat.open_] used [put_table_durable] instead of [seed_table] here, the
   PARENT would go stale and reuse rowid 1.

   Note the deliberate exception (#633): a counter still at [empty_next_rowid]
   has never allocated and carries no information, so seeding DOES overwrite
   that one — which is what keeps mirror recovery working now that a second
   [open_] shares the table. *)
let test_open_does_not_clobber_a_live_counter () =
  let store = Store.create () in
  let db1 = run (Db.of_store store) in
  exec db1 "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  exec db1 "INSERT INTO t (b) VALUES ('x')";
  exec db1 "INSERT INTO t (b) VALUES ('y')";
  exec db1 "INSERT INTO t (b) VALUES ('z')";
  (* The live counter is at 4. Opening a second handle re-reads the catalog. *)
  let _db2 = run (Db.of_store store) in
  exec db1 "INSERT INTO t (b) VALUES ('w')";
  check
    "opening a second handle did not reset the first's allocator"
    [ "1|x"; "2|y"; "3|z"; "4|w" ]
    (rows db1 "SELECT a, b FROM t ORDER BY a")
;;

(* A file-backed round trip: the ownership move must not change what a genuine
   close/reopen does, where the [Store.t] — and therefore the allocator — really
   is new and the counter has to come back from the tree. *)
let test_close_reopen_recovers_from_the_tree () =
  let path = "/tmp/granary_rowid_owner_632.db" in
  (try Unix.unlink path with
   | _ -> ());
  let db =
    match run (Db.open_file ~path ()) with
    | Ok d -> d
    | Error _ -> Alcotest.fail "open_file failed"
  in
  exec db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)";
  let wdb = run (Db.create_worker_handle db) in
  exec db "INSERT INTO t (b) VALUES ('x')";
  exec wdb "INSERT INTO t (b) VALUES ('y')";
  run (Db.close db);
  let db2 =
    match run (Db.open_file ~path ()) with
    | Ok d -> d
    | Error _ -> Alcotest.fail "reopen failed"
  in
  exec db2 "INSERT INTO t (b) VALUES ('z')";
  check
    "a fresh store recovers the counter from the tree"
    [ "1|x"; "2|y"; "3|z" ]
    (rows db2 "SELECT a, b FROM t ORDER BY a");
  run (Db.close db2);
  try Unix.unlink path with
  | _ -> ()
;;

let () =
  Alcotest.run
    "test_rowid_counter_ownership_632"
    [ ( "publish_on_commit_632"
      , [ Alcotest.test_case
            "rollback_leaves_shared_counter_where_it_was"
            `Quick
            test_rollback_leaves_shared_counter_where_it_was
        ; Alcotest.test_case
            "rollback_returns_to_the_committed_mark"
            `Quick
            test_rollback_returns_to_the_committed_mark
        ; Alcotest.test_case
            "worker_rollback_does_not_move_the_parent"
            `Quick
            test_worker_rollback_does_not_move_the_parent
        ; Alcotest.test_case
            "autoincrement_rollback_visible_on_both_handles"
            `Quick
            test_autoincrement_rollback_visible_on_both_handles
        ; Alcotest.test_case
            "catalog_next_rowid_alternates_across_catalogs"
            `Quick
            test_catalog_next_rowid_alternates_across_catalogs
        ; Alcotest.test_case
            "catalog_next_rowid_unknown_table_raises_synchronously"
            `Quick
            test_catalog_next_rowid_unknown_table_raises_synchronously
        ] )
    ; ( "store_owns_the_allocator_633"
      , [ Alcotest.test_case
            "bare_of_store_shares_the_allocator"
            `Quick
            test_bare_of_store_shares_the_allocator
        ; Alcotest.test_case
            "bare_of_store_plain_rowid"
            `Quick
            test_bare_of_store_plain_rowid
        ; Alcotest.test_case "bare_of_store_text_pk" `Quick test_bare_of_store_text_pk
        ; Alcotest.test_case "two_stores_do_not_share" `Quick test_two_stores_do_not_share
        ] )
    ; ( "invariants"
      , [ Alcotest.test_case
            "rename_carries_the_counter"
            `Quick
            test_rename_carries_the_counter
        ; Alcotest.test_case
            "drop_create_does_not_inherit"
            `Quick
            test_drop_create_does_not_inherit
        ; Alcotest.test_case
            "tid_reuse_after_rolled_back_create"
            `Quick
            test_tid_reuse_after_rolled_back_create
        ; Alcotest.test_case
            "sentinel_tables_do_not_disturb_counters"
            `Quick
            test_sentinel_tables_do_not_disturb_counters
        ; Alcotest.test_case
            "open_does_not_clobber_a_live_counter"
            `Quick
            test_open_does_not_clobber_a_live_counter
        ; Alcotest.test_case
            "close_reopen_recovers_from_the_tree"
            `Quick
            test_close_reopen_recovers_from_the_tree
        ] )
    ]
;;
