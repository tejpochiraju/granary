(** #427: CREATE REACTIVE VIEW — declarative maintained views over the IVM
    engine.  Verifies:

    - a COUNT/SUM single-column GROUP BY view is maintained incrementally and its
      materialisation ([_rv_<name>]) equals the authoritative GROUP BY read back
      after arbitrary INSERT/UPDATE/DELETE (property test, mirroring
      {!test_ivm_view_417});
    - an unmaintainable shape (MIN) is accepted and stays correct via
      full-refresh;
    - [register_view_callback] fires native {!Db.row_change} diffs on relevant
      commits and nothing on no-op writes / DDL;
    - [REFRESH DELTA] on an unmaintainable shape is rejected at CREATE;
    - #437: [reactive_view_names] / [is_reactive_view] enumerate the live registry
      (not the [_rv_] catalog naming convention), and [register_view_callback]
      reports an unknown view instead of silently attaching to nothing. *)

module Db = Granary.Db
module Cat = Granary_catalog.Catalog

let run = Lwt_main.run

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "db error: %a" Db.pp_error e
;;

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

let exec_err db sql =
  match run (Db.execute db sql) with
  | Ok () -> None
  | Error e -> Some (Format.asprintf "%a" Db.pp_error e)
;;

(* #469: substring search — error strings are part of the contract. *)
let contains ~needle haystack =
  let nl = String.length needle
  and hl = String.length haystack in
  let rec go i = i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1)) in
  go 0
;;

(* (group, value) rows read from a two-column relation, sorted. *)
let rows2 db sql : (string * int) list =
  let stream = unwrap (run (Db.query db sql)) in
  run (Lwt_stream.to_list stream)
  |> List.map (fun r ->
    match r.(0), r.(1) with
    | Db.V_text g, Db.V_int v -> g, Int64.to_int v
    | Db.V_text g, Db.V_null -> g, min_int (* NULL aggregate sentinel *)
    | _ -> Alcotest.failf "unexpected row shape for %S" sql)
  |> List.sort compare
;;

let mv db name = rows2 db (Printf.sprintf "SELECT * FROM _rv_%s" name)

(* #469: does the materialisation [_rv_<name>] still exist as a table?  Tells
   "the drop happened" apart from "the drop was aimed at another schema". *)
let rv_table_exists db name =
  match run (Db.query db (Printf.sprintf "SELECT * FROM _rv_%s" name)) with
  | Ok stream ->
    ignore (run (Lwt_stream.to_list stream));
    true
  | Error _ -> false
;;

(* single-column text rows, sorted, duplicates preserved (multiset). *)
let rows1 db sql : string list =
  let stream = unwrap (run (Db.query db sql)) in
  run (Lwt_stream.to_list stream)
  |> List.map (fun r ->
    match r.(0) with
    | Db.V_text s -> s
    | _ -> Alcotest.failf "expected text col for %S" sql)
  |> List.sort compare
;;

(* ---- fixed-sequence correctness (delta COUNT + SUM) ---- *)

let test_delta_count_sum () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "CREATE REACTIVE VIEW sm AS SELECT grp, SUM(amt) FROM t GROUP BY grp";
    let check label =
      Alcotest.(check (list (pair string int)))
        (label ^ ": COUNT view = authoritative")
        (rows2 db "SELECT grp, COUNT(*) FROM t GROUP BY grp")
        (mv db "cnt");
      Alcotest.(check (list (pair string int)))
        (label ^ ": SUM view = authoritative")
        (rows2 db "SELECT grp, SUM(amt) FROM t GROUP BY grp")
        (mv db "sm")
    in
    check "empty";
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    check "insert a/10";
    exec db "INSERT INTO t VALUES (2, 'a', 20)";
    check "insert a/20";
    exec db "INSERT INTO t VALUES (3, 'b', 5)";
    check "insert b/5";
    exec db "UPDATE t SET amt = 99 WHERE id = 1";
    check "update amt";
    exec db "UPDATE t SET grp = 'b' WHERE id = 2";
    check "regroup a->b";
    exec db "DELETE FROM t WHERE id = 1";
    check "delete last a";
    exec db "INSERT INTO t VALUES (4, 'a', 7)";
    check "a reappears";
    exec db "DELETE FROM t WHERE grp = 'b'";
    check "multi-row delete")
;;

(* ---- MIN: unmaintainable → accepted, full-refresh, stays correct ---- *)

let test_full_refresh_min () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW mn AS SELECT grp, MIN(amt) FROM t GROUP BY grp";
    let check label =
      Alcotest.(check (list (pair string int)))
        (label ^ ": MIN view = authoritative")
        (rows2 db "SELECT grp, MIN(amt) FROM t GROUP BY grp")
        (mv db "mn")
    in
    check "empty";
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    exec db "INSERT INTO t VALUES (2, 'a', 3)";
    exec db "INSERT INTO t VALUES (3, 'b', 5)";
    check "after inserts";
    exec db "DELETE FROM t WHERE id = 2";
    (* min of 'a' must recompute from 3 up to 10 — only full refresh gets this right *)
    check "after deleting the min";
    exec db "UPDATE t SET amt = 1 WHERE id = 3";
    check "after update")
;;

let test_refresh_full_forces_full () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    (* a delta-maintainable shape, but forced full — still correct *)
    exec
      db
      "CREATE REACTIVE VIEW cf AS SELECT grp, COUNT(*) FROM t GROUP BY grp REFRESH FULL";
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    exec db "INSERT INTO t VALUES (2, 'b', 5)";
    Alcotest.(check (list (pair string int)))
      "forced-full COUNT view = authoritative"
      (rows2 db "SELECT grp, COUNT(*) FROM t GROUP BY grp")
      (mv db "cf"))
;;

(* ---- REFRESH DELTA on an unmaintainable shape is a CREATE error ---- *)

let test_refresh_delta_rejected () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    match
      exec_err
        db
        "CREATE REACTIVE VIEW bad AS SELECT grp, MIN(amt) FROM t GROUP BY grp REFRESH \
         DELTA"
    with
    | None -> Alcotest.fail "expected REFRESH DELTA on MIN to be rejected"
    | Some msg ->
      Alcotest.(check bool)
        "error mentions delta-maintainability"
        true
        (let m = String.lowercase_ascii msg in
         let contains sub =
           let n = String.length sub
           and h = String.length m in
           let rec go i = i + n <= h && (String.sub m i n = sub || go (i + 1)) in
           go 0
         in
         contains "delta"))
;;

(* ---- callbacks: fire native row_change diffs; nothing on no-op ---- *)

let test_callbacks () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    let fired = ref [] in
    (match
       Db.register_view_callback db ~view_name:"cnt" (fun changes ->
         fired := changes :: !fired;
         Lwt.return_unit)
     with
     | Ok (_ : Db.view_callback) -> ()
     | Error (`Unknown_view n) -> Alcotest.failf "expected %S to be a live view" n);
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "one commit fired one batch" 1 (List.length !fired);
    (match !fired with
     | [ [ Db.Inserted { row; _ } ] ] ->
       (match row.(0), row.(1) with
        | Db.V_text "a", Db.V_int 1L -> ()
        | _ -> Alcotest.fail "unexpected inserted view row")
     | _ -> Alcotest.fail "expected a single Inserted change");
    (* no-op write: 0 rows affected → view unchanged → no callback *)
    fired := [];
    exec db "DELETE FROM t WHERE id = 999";
    Alcotest.(check int) "no-op DML fires nothing" 0 (List.length !fired);
    (* write that leaves the view value unchanged (amt only) → no callback *)
    exec db "UPDATE t SET amt = 11 WHERE id = 1";
    Alcotest.(check int) "count-preserving write fires nothing" 0 (List.length !fired);
    (* a real change → Deleted(old count) + Inserted(new count) *)
    fired := [];
    exec db "INSERT INTO t VALUES (2, 'a', 5)";
    Alcotest.(check int) "count change fires one batch" 1 (List.length !fired))
;;

(* ---- #437: enumerate/validate live reactive views ----

   A caller wiring callbacks from config (camel's [Hook_loader]) must be able to
   tell "installed" from "installed against nothing".  Both the accessors and
   [register_view_callback]'s return value read the in-memory registry, so
   neither can be fooled by a catalog-derived heuristic (a user table literally
   named [_rv_<x>], or an [_rv_] table whose view failed to re-load). *)

(* pid + a monotonic counter: deterministic and collision-free, unlike drawing
   from the global [Random] state the QCheck runner also seeds. *)
let tmp_counter = ref 0

let tmp_path () =
  incr tmp_counter;
  Printf.sprintf "/tmp/granary_rv437_%d_%d.db" (Unix.getpid ()) !tmp_counter
;;

(* #746: [register_view_callback] returns an opaque handle now, and a handle has
   no useful equality, so these assertions compare the result with the handle
   discarded.  [unregister_view_callback] has its own file,
   test_view_callback_746.ml. *)
let attach db ~view_name cb =
  Result.map
    (fun (_ : Db.view_callback) -> ())
    (Db.register_view_callback db ~view_name cb)
;;

(* Prints the actual error on failure, unlike comparing against [= Ok ()]. *)
let register_result =
  Alcotest.testable
    (fun fmt -> function
       | Ok () -> Format.pp_print_string fmt "Ok ()"
       | Error (`Unknown_view n) -> Format.fprintf fmt "Error (`Unknown_view %S)" n)
    ( = )
;;

let test_reactive_view_names () =
  with_db (fun db ->
    Alcotest.(check (list string)) "no views on a fresh db" [] (Db.reactive_view_names db);
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    Alcotest.(check (list string))
      "a plain table is not a reactive view"
      []
      (Db.reactive_view_names db);
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "CREATE REACTIVE VIEW sm AS SELECT grp, SUM(amt) FROM t GROUP BY grp";
    Alcotest.(check (list string))
      "both live views are listed, sorted"
      [ "cnt"; "sm" ]
      (Db.reactive_view_names db);
    (* a plain SQL view is not a reactive view *)
    exec db "CREATE VIEW plain AS SELECT grp FROM t";
    Alcotest.(check (list string))
      "a plain CREATE VIEW is not listed"
      [ "cnt"; "sm" ]
      (Db.reactive_view_names db);
    Alcotest.(check bool) "is_reactive_view cnt" true (Db.is_reactive_view db "cnt");
    Alcotest.(check bool) "is_reactive_view plain" false (Db.is_reactive_view db "plain");
    Alcotest.(check bool) "is_reactive_view t" false (Db.is_reactive_view db "t");
    Alcotest.(check bool)
      "is_reactive_view on a typo"
      false
      (Db.is_reactive_view db "cnnt"))
;;

(* A user table named [_rv_<x>] must not make [x] look live — the failure mode
   that motivated the accessor. *)
let test_rv_table_is_not_a_view () =
  with_db (fun db ->
    exec db "CREATE TABLE _rv_ghost (a TEXT)";
    Alcotest.(check bool)
      "an _rv_ table alone does not make a view live"
      false
      (Db.is_reactive_view db "ghost");
    Alcotest.(check (list string)) "…and is not listed" [] (Db.reactive_view_names db))
;;

let test_register_reports_unknown_view () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    let cb _ = Lwt.return_unit in
    Alcotest.check
      register_result
      "registering against a live view succeeds"
      (Ok ())
      (attach db ~view_name:"cnt" cb);
    Alcotest.check
      register_result
      "a typo'd view name is reported, not silently dropped"
      (Error (`Unknown_view "cnnt"))
      (attach db ~view_name:"cnnt" cb);
    Alcotest.check
      register_result
      "a plain table name is reported too"
      (Error (`Unknown_view "t"))
      (attach db ~view_name:"t" cb))
;;

(* #469: DROP REACTIVE VIEW is the supported removal path. *)
let test_drop_removes_view () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check bool) "live before the drop" true (Db.is_reactive_view db "cnt");
    exec db "DROP REACTIVE VIEW cnt";
    Alcotest.(check bool) "not live after" false (Db.is_reactive_view db "cnt");
    Alcotest.(check (list string)) "not listed after" [] (Db.reactive_view_names db);
    (* the materialisation is gone: selecting from it is now an error *)
    Alcotest.(check bool)
      "_rv_cnt no longer exists"
      true
      (Option.is_some (exec_err db "SELECT * FROM _rv_cnt"));
    (* a full-refresh view (MIN is not delta-maintainable) drops the same way *)
    exec db "CREATE REACTIVE VIEW lo AS SELECT grp, MIN(amt) FROM t GROUP BY grp";
    Alcotest.(check (list string))
      "the full-refresh view is live"
      [ "lo" ]
      (Db.reactive_view_names db);
    exec db "DROP REACTIVE VIEW lo";
    Alcotest.(check (list string)) "and drops too" [] (Db.reactive_view_names db);
    (* REACTIVE remains usable as an ordinary identifier *)
    exec db "CREATE TABLE reactive (reactive TEXT)";
    exec db "DROP TABLE reactive")
;;

let test_drop_stops_callbacks () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    let fired = ref 0 in
    (match
       Db.register_view_callback db ~view_name:"cnt" (fun _ ->
         incr fired;
         Lwt.return_unit)
     with
     | Ok (_ : Db.view_callback) -> ()
     | Error (`Unknown_view n) -> Alcotest.failf "expected %S to be a live view" n);
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "callback fires while live" 1 !fired;
    exec db "DROP REACTIVE VIEW cnt";
    exec db "INSERT INTO t VALUES (2, 'b', 20)";
    Alcotest.(check int) "callback is silent after the drop" 1 !fired;
    Alcotest.check
      register_result
      "re-registering reports the view as unknown"
      (Error (`Unknown_view "cnt"))
      (attach db ~view_name:"cnt" (fun _ -> Lwt.return_unit)))
;;

let test_drop_if_exists () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    (match exec_err db "DROP REACTIVE VIEW nosuch" with
     | None -> Alcotest.fail "dropping a missing reactive view must error"
     | Some msg ->
       Alcotest.(check bool) "error names the view" true (contains ~needle:"nosuch" msg));
    exec db "DROP REACTIVE VIEW IF EXISTS nosuch";
    (* IF EXISTS on a live view still drops it *)
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "DROP REACTIVE VIEW IF EXISTS cnt";
    Alcotest.(check (list string)) "dropped" [] (Db.reactive_view_names db))
;;

(* A dropped view must not come back on reopen, and must not leave the delta
   engine or the old materialisation behind for a same-named successor. *)
let test_drop_is_durable_and_recreatable () =
  let path = tmp_path () in
  let cleanup () =
    try Sys.remove path with
    | _ -> ()
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () ->
    let db = unwrap (run (Granary_unix.open_file ~path ())) in
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW v AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    exec db "INSERT INTO t VALUES (2, 'a', 5)";
    exec db "DROP REACTIVE VIEW v";
    run (Db.close db);
    let db = unwrap (run (Granary_unix.open_file ~path ())) in
    Fun.protect
      ~finally:(fun () ->
        try run (Db.close db) with
        | _ -> ())
      (fun () ->
         Alcotest.(check (list string))
           "the drop survived the reopen"
           []
           (Db.reactive_view_names db);
         (* re-create the same name with a *different* aggregate: no stale
            registry, catalog row, or _rv_ table may leak through *)
         exec db "CREATE REACTIVE VIEW v AS SELECT grp, SUM(amt) FROM t GROUP BY grp";
         Alcotest.(check (list (pair string int)))
           "re-created view materialises the new query"
           [ "a", 15 ]
           (mv db "v");
         exec db "INSERT INTO t VALUES (3, 'a', 1)";
         Alcotest.(check (list (pair string int)))
           "and is maintained"
           [ "a", 16 ]
           (mv db "v")))
;;

(* #469: the materialisation is internal — dropping it directly would leave the
   registry claiming a view with no _rv_ table.  A user table that merely starts
   with [_rv_] and has no registry entry stays droppable. *)
let test_rv_table_drop_rejected () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    (match exec_err db "DROP TABLE _rv_cnt" with
     | None -> Alcotest.fail "dropping an internal _rv_ table must be rejected"
     | Some msg ->
       Alcotest.(check bool)
         "error points at DROP REACTIVE VIEW"
         true
         (contains ~needle:"DROP REACTIVE VIEW cnt" msg));
    Alcotest.(check bool) "the view is untouched" true (Db.is_reactive_view db "cnt");
    (* the view still works *)
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check (list (pair string int))) "still maintained" [ "a", 1 ] (mv db "cnt");
    (* a plain user table with an _rv_ prefix and no registry entry still drops *)
    exec db "CREATE TABLE _rv_ghost (a TEXT)";
    exec db "DROP TABLE _rv_ghost";
    (* and after a proper drop, the name is droppable as an ordinary table *)
    exec db "DROP REACTIVE VIEW cnt";
    exec db "CREATE TABLE _rv_cnt (a TEXT)";
    exec db "DROP TABLE _rv_cnt")
;;

(* #469: DROP VIEW used to silently succeed as a no-op on a reactive view —
   reactive views live in their own registry, not in [t.views]. *)
let test_drop_view_on_reactive_view_errors () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    (match exec_err db "DROP VIEW cnt" with
     | None -> Alcotest.fail "DROP VIEW on a reactive view must not silently succeed"
     | Some msg ->
       Alcotest.(check bool)
         "error points at DROP REACTIVE VIEW"
         true
         (contains ~needle:"DROP REACTIVE VIEW cnt" msg));
    Alcotest.(check bool) "the view is untouched" true (Db.is_reactive_view db "cnt");
    (* IF EXISTS does not excuse it: the object exists, the statement is wrong *)
    Alcotest.(check bool)
      "IF EXISTS still errors"
      true
      (Option.is_some (exec_err db "DROP VIEW IF EXISTS cnt"));
    (* plain views are unaffected *)
    exec db "CREATE VIEW plain AS SELECT grp FROM t";
    exec db "DROP VIEW plain")
;;

(* #469/#473: reactive-view DDL is immediate, not staged (symmetric with
   CREATE REACTIVE VIEW), and is unsupported inside an explicit transaction
   at all.

   [rv_drop] (lib/db/db.ml) writes the registry removal through
   [Cat.remove_reactive_view top.store ~name] without threading the ambient
   [t.explicit_txn] as [?txn] — unlike the sibling [staged_schema_change]
   path used by [Op_create_view]/[Op_drop_view]. Because that write always
   acquires its own fresh writer transaction ([borrow_or_autocommit
   ?txn:None] -> [S.rw_begin] -> [Rwlock.acquire_write t.lock]), running it
   while an explicit [BEGIN] already holds that same single-writer lock
   would self-deadlock unconditionally (confirmed empirically: two prior
   runs killed at 120s/280s, both parked at 0% CPU). Threading [?txn] is the
   real fix but would make reactive-view DDL transactional, overturning a
   deliberate design decision (#269 makes that area delicate), so instead
   [rv_drop] rejects the statement outright — before mutating any state —
   whenever [top.explicit_txn] is not [None]. This test pins that rejection:
   the error surfaces immediately (not a hang), mentions the transaction,
   leaves the view live, and the same drop succeeds once back in
   autocommit. Because the failure mode is now a returned [Error] rather
   than a lock wait, a regression here fails an assertion instead of
   blocking the suite. Tracked as #473 (real fix: thread [?txn] through). *)
let test_drop_is_not_rolled_back () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "BEGIN";
    (match exec_err db "DROP REACTIVE VIEW cnt" with
     | None -> Alcotest.fail "DROP REACTIVE VIEW inside BEGIN must not silently succeed"
     | Some msg ->
       Alcotest.(check bool)
         "error mentions the transaction"
         true
         (contains ~needle:"transaction" msg));
    Alcotest.(check bool)
      "the view is still live inside the (still open) transaction"
      true
      (Db.is_reactive_view db "cnt");
    (* IF EXISTS does not excuse it: the statement is unsupported here
       regardless of whether the view exists (mirrors DROP VIEW's guard). *)
    Alcotest.(check bool)
      "IF EXISTS is rejected too, inside a transaction"
      true
      (Option.is_some (exec_err db "DROP REACTIVE VIEW IF EXISTS cnt"));
    Alcotest.(check bool)
      "the view is still live after the IF EXISTS attempt"
      true
      (Db.is_reactive_view db "cnt");
    exec db "ROLLBACK";
    Alcotest.(check bool)
      "the view is still live after ROLLBACK"
      true
      (Db.is_reactive_view db "cnt");
    (* the same drop now succeeds in autocommit *)
    exec db "DROP REACTIVE VIEW cnt";
    Alcotest.(check (list string))
      "autocommit drop is immediate"
      []
      (Db.reactive_view_names db))
;;

(* #469 review finding: reactive views live only on the top-level handle, so
   the guard must not fire for drops routed to an ATTACHed sub-handle — an
   ordinary, unrelated table there that happens to share the [_rv_<name>]
   naming convention with a *main*-schema view must stay droppable. *)
let test_rv_guard_does_not_cross_attach () =
  let () = Granary_unix.install () in
  let aux = tmp_path () in
  let cleanup () =
    try Sys.remove aux with
    | _ -> ()
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () ->
    with_db (fun db ->
      exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
      exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
      exec db "INSERT INTO t VALUES (1, 'a', 10)";
      exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux);
      exec db "PRAGMA active_database = aux";
      (* an ordinary table in [aux] that happens to share the main-schema
         view's materialisation name — no registry entry ties it to [cnt]. *)
      exec db "CREATE TABLE _rv_cnt (a TEXT)";
      exec db "DROP TABLE _rv_cnt";
      exec db "PRAGMA active_database = main";
      Alcotest.(check bool)
        "the main view is untouched"
        true
        (Db.is_reactive_view db "cnt");
      (* #469 review: without this, the test would still pass if the aux-side
         drop had reached through to main's materialisation — the registry
         entry alone does not prove the table survived. *)
      Alcotest.(check (list (pair string int)))
        "main's _rv_cnt survived, contents intact"
        [ "a", 1 ]
        (mv db "cnt");
      exec db "DETACH DATABASE aux"))
;;

(* #469 review: [rv_drop] removes the registry entry and main's catalog row
   directly, but its internal [DROP TABLE IF EXISTS _rv_<name>] re-enters
   [compile_routed], which routes by [active_schema].  Under [active_database =
   aux] that drop would be aimed at [aux] and silently no-op, orphaning main's
   [_rv_cnt] and making a later re-CREATE fail with "table already exists".
   The statement is therefore refused outright off the top-level handle. *)
let test_drop_reactive_view_rejected_off_main () =
  let () = Granary_unix.install () in
  let aux = tmp_path () in
  let cleanup () =
    try Sys.remove aux with
    | _ -> ()
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () ->
    with_db (fun db ->
      exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
      exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
      exec db "INSERT INTO t VALUES (1, 'a', 10)";
      exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux);
      exec db "PRAGMA active_database = aux";
      (match exec_err db "DROP REACTIVE VIEW cnt" with
       | None -> Alcotest.fail "DROP REACTIVE VIEW off the main schema must be rejected"
       | Some msg ->
         Alcotest.(check bool)
           "error says to run it against main"
           true
           (contains ~needle:"active_database = main" msg));
      (* IF EXISTS does not excuse a misrouted statement either. *)
      Alcotest.(check bool)
        "IF EXISTS is rejected off main too"
        true
        (Option.is_some (exec_err db "DROP REACTIVE VIEW IF EXISTS cnt"));
      exec db "PRAGMA active_database = main";
      Alcotest.(check bool) "the view is still live" true (Db.is_reactive_view db "cnt");
      Alcotest.(check (list (pair string int)))
        "and its materialisation is intact"
        [ "a", 1 ]
        (mv db "cnt");
      (* the drop works once routed at main, and the name is re-creatable —
         which it would not be had a stray _rv_cnt been left behind *)
      exec db "DROP REACTIVE VIEW cnt";
      Alcotest.(check bool) "_rv_cnt is gone" false (rv_table_exists db "cnt");
      exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
      Alcotest.(check (list (pair string int)))
        "re-created cleanly"
        [ "a", 1 ]
        (mv db "cnt");
      exec db "DETACH DATABASE aux"))
;;

(* #469 review: two views over one base table.  Dropping one must deregister
   exactly that view and leave the other maintained — this is the invariant the
   registry / [rv_pending] manipulation in [rv_drop] exists to protect. *)
let test_drop_one_of_two_views_on_same_base () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "CREATE REACTIVE VIEW sm AS SELECT grp, SUM(amt) FROM t GROUP BY grp";
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    exec db "DROP REACTIVE VIEW cnt";
    Alcotest.(check (list string))
      "only [sm] remains live"
      [ "sm" ]
      (Db.reactive_view_names db);
    Alcotest.(check bool) "_rv_cnt is gone" false (rv_table_exists db "cnt");
    (* the survivor keeps tracking the shared base table across every op kind *)
    exec db "INSERT INTO t VALUES (2, 'a', 5)";
    Alcotest.(check (list (pair string int))) "insert tracked" [ "a", 15 ] (mv db "sm");
    exec db "UPDATE t SET amt = 1 WHERE id = 1";
    Alcotest.(check (list (pair string int))) "update tracked" [ "a", 6 ] (mv db "sm");
    exec db "INSERT INTO t VALUES (3, 'b', 4)";
    exec db "DELETE FROM t WHERE id = 2";
    Alcotest.(check (list (pair string int)))
      "delete tracked"
      [ "a", 1; "b", 4 ]
      (mv db "sm");
    Alcotest.(check (list (pair string int)))
      "survivor = authoritative"
      (rows2 db "SELECT grp, SUM(amt) FROM t GROUP BY grp")
      (mv db "sm"))
;;

(* #469 review finding: the [t == top] guard on the new [Op_drop_view] arm
   had no direct regression test — [test_rv_guard_does_not_cross_attach]
   only exercises the [Op_drop_table] arm.  A plain view sharing the
   reactive view's name, but living in an ATTACHed schema, must stay
   droppable via ordinary DROP VIEW: the guard must consult [top]'s
   registry only when the drop is actually routed to [top]. *)
let test_drop_view_guard_does_not_cross_attach () =
  let () = Granary_unix.install () in
  let aux = tmp_path () in
  let cleanup () =
    try Sys.remove aux with
    | _ -> ()
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () ->
    with_db (fun db ->
      exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
      exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
      exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux);
      exec db "PRAGMA active_database = aux";
      (* an ordinary plain view in [aux] that happens to share the
         main-schema reactive view's name — no registry entry ties it to
         [cnt] on [top]. *)
      exec db "CREATE TABLE u (a TEXT)";
      exec db "CREATE VIEW cnt AS SELECT a FROM u";
      exec db "DROP VIEW cnt";
      exec db "PRAGMA active_database = main";
      Alcotest.(check bool)
        "the main reactive view is untouched"
        true
        (Db.is_reactive_view db "cnt");
      exec db "DETACH DATABASE aux"))
;;

(* The registry is rebuilt from the catalog on open: names must survive a
   reopen, otherwise a hook wired at startup attaches to nothing. *)
let test_names_survive_reopen () =
  let path = tmp_path () in
  (* [open_file] is non-WAL: one file, no sidecars. *)
  let cleanup () =
    try Sys.remove path with
    | _ -> ()
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () ->
    let db = unwrap (run (Granary_unix.open_file ~path ())) in
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    run (Db.close db);
    let db = unwrap (run (Granary_unix.open_file ~path ())) in
    Fun.protect
      ~finally:(fun () ->
        try run (Db.close db) with
        | _ -> ())
      (fun () ->
         Alcotest.(check (list string))
           "the view is live again after reopen"
           [ "cnt" ]
           (Db.reactive_view_names db);
         let cb _ = Lwt.return_unit in
         Alcotest.check
           register_result
           "and a callback attaches to it"
           (Ok ())
           (attach db ~view_name:"cnt" cb)))
;;

(* The motivating "dead hook reported healthy" state (#437 comment): a persisted
   definition that [rv_load] cannot restore leaves the [_rv_<name>] table in the
   catalog with *no* registry entry.  Any catalog-derived probe reports the view
   live; the registry accessor — and [register_view_callback] — must not.  Both
   skip branches are covered: SQL that fails to parse, and SQL that parses to a
   different statement. *)
let test_unloadable_view_is_not_live stored_sql () =
  let path = tmp_path () in
  let cleanup () =
    try Sys.remove path with
    | _ -> ()
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () ->
    let db = unwrap (run (Granary_unix.open_file ~path ())) in
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    run (Db.close db);
    (* Corrupt the stored definition behind the engine's back, leaving _rv_cnt
       in place — what a downgrade or a definition the parser no longer accepts
       would produce. *)
    (match run (Granary_unix.Store.open_file ~path ()) with
     | Error e -> Alcotest.failf "store open: %a" Granary_store.Store.pp_error e
     | Ok store ->
       (* #476: the catalog writes now return a [result] instead of raising. *)
       (match run (Cat.persist_reactive_view store ~name:"cnt" ~sql:stored_sql) with
        | Ok () -> ()
        | Error e -> Alcotest.failf "persist_reactive_view: %s" e);
       run (Granary_store.Store.close store));
    let db = unwrap (run (Granary_unix.open_file ~path ())) in
    Fun.protect
      ~finally:(fun () ->
        try run (Db.close db) with
        | _ -> ())
      (fun () ->
         (* the materialisation table is still there — this is exactly what
            makes the _rv_ probe report a healthy view *)
         Alcotest.(check (list (pair string int)))
           "_rv_cnt survived the corruption"
           [ "a", 1 ]
           (mv db "cnt");
         Alcotest.(check (list string))
           "…but the view is not live"
           []
           (Db.reactive_view_names db);
         Alcotest.(check bool)
           "…and is_reactive_view says so"
           false
           (Db.is_reactive_view db "cnt");
         Alcotest.check
           register_result
           "…and registering reports it unknown rather than attaching nothing"
           (Error (`Unknown_view "cnt"))
           (attach db ~view_name:"cnt" (fun _ -> Lwt.return_unit))))
;;

(* #469 review: a view left out of the registry by #437 (stored SQL that no
   longer re-parses) still owns a catalog row and an [_rv_] table.  If
   [rv_drop] consulted only the registry it would answer "no such reactive
   view" forever, so that row could never be removed through SQL and every open
   would keep warning.  [DROP REACTIVE VIEW] must fall back to the catalog and
   clean it up, and the reopen after must be quiet. *)
let test_drop_removes_unloadable_view () =
  let path = tmp_path () in
  let cleanup () =
    try Sys.remove path with
    | _ -> ()
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () ->
    let db = unwrap (run (Granary_unix.open_file ~path ())) in
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    run (Db.close db);
    (* corrupt the stored definition behind the engine's back, exactly as
       [test_unloadable_view_is_not_live] does *)
    (match run (Granary_unix.Store.open_file ~path ()) with
     | Error e -> Alcotest.failf "store open: %a" Granary_store.Store.pp_error e
     | Ok store ->
       (match run (Cat.persist_reactive_view store ~name:"cnt" ~sql:"NOT SQL AT ALL") with
        | Ok () -> ()
        | Error e -> Alcotest.failf "persist_reactive_view: %s" e);
       run (Granary_store.Store.close store));
    let db = unwrap (run (Granary_unix.open_file ~path ())) in
    Alcotest.(check (list string))
      "precondition: not live, so the registry alone cannot drop it"
      []
      (Db.reactive_view_names db);
    Alcotest.(check bool)
      "precondition: _rv_cnt is still there"
      true
      (rv_table_exists db "cnt");
    exec db "DROP REACTIVE VIEW cnt";
    Alcotest.(check bool) "the orphan _rv_cnt is gone" false (rv_table_exists db "cnt");
    run (Db.close db);
    (* the catalog row is gone too: the reopen finds nothing to warn about and
       nothing to skip *)
    (match run (Granary_unix.Store.open_file ~path ()) with
     | Error e -> Alcotest.failf "store open: %a" Granary_store.Store.pp_error e
     | Ok store ->
       let pairs =
         match run (Cat.load_all_reactive_views store) with
         | Ok pairs -> pairs
         | Error e -> Alcotest.failf "load_all_reactive_views: %s" e
       in
       Alcotest.(check (list string))
         "no reactive-view catalog rows remain"
         []
         (List.map fst pairs);
       run (Granary_store.Store.close store));
    let db = unwrap (run (Granary_unix.open_file ~path ())) in
    Fun.protect
      ~finally:(fun () ->
        try run (Db.close db) with
        | _ -> ())
      (fun () ->
         Alcotest.(check (list string)) "reopen is quiet" [] (Db.reactive_view_names db);
         (* and the name is fully reusable *)
         exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
         Alcotest.(check (list (pair string int)))
           "re-created over the surviving base data"
           [ "a", 1 ]
           (mv db "cnt")))
;;

(* #469 review: with no registry entry *and* no catalog row, the drop must
   still error (and IF EXISTS must still be silent) — the catalog fallback
   above must not turn an unknown name into a success. *)
let test_drop_unknown_view_still_errors () =
  with_db (fun db ->
    (match exec_err db "DROP REACTIVE VIEW nope" with
     | None -> Alcotest.fail "dropping an unknown reactive view must error"
     | Some msg ->
       Alcotest.(check bool)
         "error names the view"
         true
         (contains ~needle:"no such reactive view: nope" msg));
    exec db "DROP REACTIVE VIEW IF EXISTS nope")
;;

(* ---- regression (review #428): full-refresh must not collapse duplicate
   output rows.  A non-distinct projection can hold several identical rows;
   changing one must delete exactly one copy, not all of them. ---- *)

let test_full_refresh_duplicate_rows () =
  with_db (fun db ->
    exec db "CREATE TABLE orders (id INTEGER PRIMARY KEY, status TEXT)";
    exec db "CREATE REACTIVE VIEW v AS SELECT status FROM orders REFRESH FULL";
    exec db "INSERT INTO orders VALUES (1, 'open')";
    exec db "INSERT INTO orders VALUES (2, 'open')";
    Alcotest.(check (list string))
      "two identical 'open' rows are both materialised"
      [ "open"; "open" ]
      (rows1 db "SELECT * FROM _rv_v");
    exec db "UPDATE orders SET status = 'closed' WHERE id = 1";
    Alcotest.(check (list string))
      "changing one of two duplicates deletes exactly one copy"
      [ "closed"; "open" ]
      (rows1 db "SELECT * FROM _rv_v");
    (* and it must equal the authoritative query at all times *)
    Alcotest.(check (list string))
      "view = authoritative"
      (rows1 db "SELECT status FROM orders")
      (rows1 db "SELECT * FROM _rv_v"))
;;

(* ---- regression (review #428): [FULL] as a keyword must not break
   [PRAGMA synchronous = full] (the #298/#334 durability API). ---- *)

let test_pragma_full_still_parses () =
  with_db (fun db ->
    (match run (Db.execute db "PRAGMA synchronous = full") with
     | Ok () -> ()
     | Error e -> Alcotest.failf "PRAGMA synchronous = full: %a" Db.pp_error e);
    match run (Db.execute db "PRAGMA synchronous = off") with
    | Ok () -> ()
    | Error e -> Alcotest.failf "PRAGMA synchronous = off: %a" Db.pp_error e)
;;

(* ---- regression (review #428): the reactive-view flush must not leak
   synthetic [_rv_…] changes into an ambient change accumulator. ---- *)

let test_no_rv_leak_into_changes () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    let changes =
      unwrap (run (Db.execute_with_changes db "INSERT INTO t VALUES (1, 'a', 10)"))
    in
    let tables = List.map fst changes in
    Alcotest.(check (list string))
      "only the base table appears in the feed"
      [ "t" ]
      tables;
    Alcotest.(check bool)
      "no _rv_ table leaked into the change feed"
      false
      (List.exists (fun n -> String.length n >= 4 && String.sub n 0 4 = "_rv_") tables))
;;

(* ---- property: delta views track the DB across random op sequences ---- *)

type op =
  | Ins of int * string * int
  | Upd_amt of int * int
  | Upd_grp of int * string
  | Del of int

let sql_of_op = function
  | Ins (id, g, a) ->
    Printf.sprintf "INSERT OR IGNORE INTO t VALUES (%d, '%s', %d)" id g a
  | Upd_amt (id, a) -> Printf.sprintf "UPDATE t SET amt = %d WHERE id = %d" a id
  | Upd_grp (id, g) -> Printf.sprintf "UPDATE t SET grp = '%s' WHERE id = %d" g id
  | Del id -> Printf.sprintf "DELETE FROM t WHERE id = %d" id
;;

let gen_op =
  let open QCheck.Gen in
  let id = int_range 1 6 in
  let grp = map (fun i -> [| "a"; "b"; "c" |].(i)) (int_range 0 2) in
  let amt = int_range 0 9 in
  oneof
    [ map3 (fun i g a -> Ins (i, g, a)) id grp amt
    ; map3 (fun i g a -> Ins (i, g, a)) id grp amt
    ; map2 (fun i a -> Upd_amt (i, a)) id amt
    ; map2 (fun i g -> Upd_grp (i, g)) id grp
    ; map (fun i -> Del i) id
    ]
;;

let arb_ops = QCheck.make QCheck.Gen.(list_size (int_range 0 40) gen_op)

let prop_views_track =
  QCheck.Test.make
    ~count:200
    ~name:"delta views track DB across random ops"
    arb_ops
    (fun ops ->
       with_db (fun db ->
         exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
         exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
         exec db "CREATE REACTIVE VIEW sm AS SELECT grp, SUM(amt) FROM t GROUP BY grp";
         List.for_all
           (fun op ->
              exec db (sql_of_op op);
              rows2 db "SELECT grp, COUNT(*) FROM t GROUP BY grp" = mv db "cnt"
              && rows2 db "SELECT grp, SUM(amt) FROM t GROUP BY grp" = mv db "sm")
           ops))
;;

let () =
  Alcotest.run
    "reactive_view_427"
    [ ( "delta"
      , [ Alcotest.test_case "count+sum tracked" `Quick test_delta_count_sum
        ; Alcotest.test_case
            "refresh full forces full"
            `Quick
            test_refresh_full_forces_full
        ] )
    ; ( "full-refresh"
      , [ Alcotest.test_case "min via full refresh" `Quick test_full_refresh_min
        ; Alcotest.test_case
            "duplicate rows preserved"
            `Quick
            test_full_refresh_duplicate_rows
        ] )
    ; ( "regression"
      , [ Alcotest.test_case
            "pragma synchronous=full parses"
            `Quick
            test_pragma_full_still_parses
        ; Alcotest.test_case
            "no _rv_ leak into change feed"
            `Quick
            test_no_rv_leak_into_changes
        ] )
    ; ( "errors"
      , [ Alcotest.test_case
            "refresh delta rejected on min"
            `Quick
            test_refresh_delta_rejected
        ] )
    ; "callbacks", [ Alcotest.test_case "native row_change diffs" `Quick test_callbacks ]
    ; ( "enumeration"
      , [ Alcotest.test_case "reactive_view_names" `Quick test_reactive_view_names
        ; Alcotest.test_case "_rv_ table is not a view" `Quick test_rv_table_is_not_a_view
        ; Alcotest.test_case
            "register reports unknown view"
            `Quick
            test_register_reports_unknown_view
        ; Alcotest.test_case "names survive reopen" `Quick test_names_survive_reopen
        ; Alcotest.test_case
            "unparseable stored SQL is not live"
            `Quick
            (test_unloadable_view_is_not_live "NOT SQL AT ALL")
        ; Alcotest.test_case
            "non-reactive-view stored SQL is not live"
            `Quick
            (test_unloadable_view_is_not_live "SELECT 1")
        ] )
    ; ( "drop"
      , [ Alcotest.test_case "drop removes the view" `Quick test_drop_removes_view
        ; Alcotest.test_case "drop stops callbacks" `Quick test_drop_stops_callbacks
        ; Alcotest.test_case "if exists" `Quick test_drop_if_exists
        ; Alcotest.test_case
            "drop is durable and re-creatable"
            `Quick
            test_drop_is_durable_and_recreatable
        ; Alcotest.test_case "_rv_ table drop rejected" `Quick test_rv_table_drop_rejected
        ; Alcotest.test_case
            "DROP VIEW on a reactive view errors"
            `Quick
            test_drop_view_on_reactive_view_errors
        ; Alcotest.test_case "drop is not rolled back" `Quick test_drop_is_not_rolled_back
        ; Alcotest.test_case
            "_rv_ guard does not cross attach"
            `Quick
            test_rv_guard_does_not_cross_attach
        ; Alcotest.test_case
            "DROP VIEW guard does not cross attach"
            `Quick
            test_drop_view_guard_does_not_cross_attach
        ; Alcotest.test_case
            "DROP REACTIVE VIEW rejected off main"
            `Quick
            test_drop_reactive_view_rejected_off_main
        ; Alcotest.test_case
            "one of two views on the same base"
            `Quick
            test_drop_one_of_two_views_on_same_base
        ; Alcotest.test_case
            "drop cleans up an unloadable view"
            `Quick
            test_drop_removes_unloadable_view
        ; Alcotest.test_case
            "unknown view still errors"
            `Quick
            test_drop_unknown_view_still_errors
        ] )
    ; "property", [ QCheck_alcotest.to_alcotest prop_views_track ]
    ]
;;
