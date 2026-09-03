(** #474 — the reactive-view driver's own SQL is pinned to the schema the view
    lives on.

    Reactive views live on the top-level handle only, but every statement the
    driver issued on its own behalf went through [Db.execute] / [Db.query] /
    [Db.prepare], which route through [resolve_target_ast] and therefore obey
    [top.active_schema]. So under

    {v
      ATTACH DATABASE '…' AS aux;
      PRAGMA active_database = 'aux';
    v}

    the [CREATE TABLE _rv_<name>], its INSERTs and DELETEs, and the
    [SELECT * FROM _rv_<name>] the refresh diffs against were all aimed at
    [aux] — while the registry entry, the catalog row and the view's own query
    stayed on [main]. The visible result was a view registered on [main] whose
    materialisation did not exist there at all.

    The fix pins the driver's statements ([Db.rv_execute] / [rv_query] /
    [rv_prepare], via [compile_routed ?on]). It is deliberately NOT a routing
    statement and mutates no routing state, so it is not a second way to move
    the caller's active schema — the thing #598 closed.

    Two discriminators are asserted together, because either alone is weak:
    {b where} the materialisation ends up, and {b whose rows} are in it. Both
    schemas here carry a table called [t], with different contents. *)

module Db = Granary.Db

(* ATTACH opens a file-backed sub-db, so the engine needs the Unix file
   provider (the parent here is in-memory, so opening it does not install it
   automatically). *)
let () = Granary_unix.install ()

let run = Lwt_main.run

let scratch_file suffix =
  let path = Printf.sprintf "/tmp/granary_rv474_%s_%d.db" suffix (Unix.getpid ()) in
  (try Unix.unlink path with
   | _ -> ());
  (try Unix.unlink (path ^ "-wal") with
   | _ -> ());
  path
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

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

let rows2 db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    run (Lwt_stream.to_list stream)
    |> List.map (fun r ->
      match r.(0), r.(1) with
      | Db.V_text g, Db.V_int v -> g, Int64.to_int v
      | _ -> Alcotest.failf "unexpected row shape for %S" sql)
    |> List.sort compare
;;

let table_exists db name =
  match run (Db.query db (Printf.sprintf "SELECT * FROM %s" name)) with
  | Ok stream ->
    ignore (run (Lwt_stream.to_list stream));
    true
  | Error _ -> false
;;

let with_attached f =
  let path = scratch_file "aux" in
  cleanup path;
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      (try run (Db.close db) with
       | _ -> ());
      cleanup path)
    (fun () ->
       (* main's t — the data the view is actually about *)
       exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT)";
       exec db "INSERT INTO t VALUES (1, 'a')";
       exec db "INSERT INTO t VALUES (2, 'a')";
       exec db "INSERT INTO t VALUES (3, 'b')";
       exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" path);
       (* aux gets a same-shaped, differently-populated t, so that a statement
          that lands in the wrong schema still BINDS — a bind failure would
          mask the routing bug rather than expose it. *)
       exec db "PRAGMA active_database = 'aux'";
       exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT)";
       exec db "INSERT INTO t VALUES (1, 'zz')";
       exec db "PRAGMA active_database = 'main'";
       f db)
;;

(* ------------------------------------------------------------------ *)
(* 1. CREATE REACTIVE VIEW under a non-main active schema.              *)
(* ------------------------------------------------------------------ *)

let create_materialises_into_the_views_own_schema () =
  with_attached (fun db ->
    exec db "PRAGMA active_database = 'aux'";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    (* The registry always lived on main. *)
    Alcotest.(check (list string))
      "the view is registered on the top-level handle"
      [ "cnt" ]
      (Db.reactive_view_names db);
    (* Before #474 the CREATE TABLE _rv_cnt landed in aux, so main had no such
       table at all and this read failed outright. *)
    Alcotest.(check bool)
      "aux did NOT get the materialisation"
      false
      (table_exists db "_rv_cnt");
    exec db "PRAGMA active_database = 'main'";
    Alcotest.(check bool)
      "main — the schema the view belongs to — did"
      true
      (table_exists db "_rv_cnt");
    Alcotest.(check (list (pair string int)))
      "…and it holds MAIN's rows, not aux's"
      [ "a", 2; "b", 1 ]
      (rows2 db "SELECT * FROM _rv_cnt"))
;;

(* ------------------------------------------------------------------ *)
(* 2. A refresh driven while another schema is active.                  *)
(* ------------------------------------------------------------------ *)

(* [drive_reactive] fires on any statement executed through the top-level
   handle, so a write to aux's identically-named [t] schedules a flush of the
   [t]-based view.  The refresh must read and rewrite MAIN's [_rv_cnt] and
   recompute from MAIN's [t]; before #474 it read [_rv_cnt] from aux, where it
   did not exist, and the user's INSERT failed with "no such table".

   REFRESH FULL is deliberate: a delta view would step its engine with aux's
   row change, which is a different (pre-existing, name-keyed) problem that
   #474 does not fix and this test must not appear to. *)
let a_refresh_under_another_active_schema_stays_on_main () =
  with_attached (fun db ->
    exec
      db
      "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp REFRESH FULL";
    Alcotest.(check (list (pair string int)))
      "baseline, created while main is active"
      [ "a", 2; "b", 1 ]
      (rows2 db "SELECT * FROM _rv_cnt");
    exec db "PRAGMA active_database = 'aux'";
    (* This must succeed: the flush it drives reads main's _rv_cnt. *)
    exec db "INSERT INTO t VALUES (2, 'zz')";
    exec db "PRAGMA active_database = 'main'";
    Alcotest.(check (list (pair string int)))
      "main's materialisation is intact and still describes main's t"
      [ "a", 2; "b", 1 ]
      (rows2 db "SELECT * FROM _rv_cnt");
    (* and normal maintenance still works afterwards *)
    exec db "INSERT INTO t VALUES (4, 'b')";
    Alcotest.(check (list (pair string int)))
      "…and a main-side write is still maintained"
      [ "a", 2; "b", 2 ]
      (rows2 db "SELECT * FROM _rv_cnt"))
;;

(* ------------------------------------------------------------------ *)
(* 3. DROP REACTIVE VIEW keeps #469's refusal.                          *)
(* ------------------------------------------------------------------ *)

(* #474 removes the CORRUPTION #469 was worried about (the internal DROP TABLE
   aiming at the wrong schema), but not the refusal itself: falling through to
   [main] would silently ignore the schema the caller selected.  Pinned so the
   retained half of #469's decision is visible as a decision. *)
let drop_from_another_schema_is_still_refused () =
  with_attached (fun db ->
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "PRAGMA active_database = 'aux'";
    let msg = exec_err db "DROP REACTIVE VIEW cnt" in
    Alcotest.(check bool)
      "the refusal says which schema to run it from"
      true
      (contains ~needle:"active_database = main" msg);
    exec db "PRAGMA active_database = 'main'";
    Alcotest.(check bool)
      "nothing was destroyed by the refused statement"
      true
      (table_exists db "_rv_cnt");
    exec db "DROP REACTIVE VIEW cnt";
    Alcotest.(check bool)
      "and from main it removes main's materialisation"
      false
      (table_exists db "_rv_cnt");
    Alcotest.(check (list string))
      "…and deregisters the view"
      []
      (Db.reactive_view_names db))
;;

let () =
  Alcotest.run
    "reactive view schema pinning (#474)"
    [ ( "driver SQL is schema-pinned"
      , [ Alcotest.test_case
            "CREATE materialises into the view's own schema"
            `Quick
            create_materialises_into_the_views_own_schema
        ; Alcotest.test_case
            "a refresh driven under another active schema stays on main"
            `Quick
            a_refresh_under_another_active_schema_stays_on_main
        ; Alcotest.test_case
            "DROP REACTIVE VIEW from another schema is still refused"
            `Quick
            drop_from_another_schema_is_still_refused
        ] )
    ]
;;
