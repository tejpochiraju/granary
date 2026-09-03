(** #475 — the reactive-view driver's re-entrancy guard.

    Two separate things used to ride on one [bool] field, [rv_refreshing]:

    - {b the re-entrancy fast path} in [drive_reactive], which stops the
      driver's own [_rv_…] writes from driving reactive-view maintenance
      recursively. That is genuinely a "am I inside the driver?" question, and
      it is now a nesting {i depth counter} — four (now five) entry points set
      it and they nest, so a bool cleared by whichever finished first re-armed
      reactive driving inside the outer operation's own internal writes.

    - {b the [DROP TABLE _rv_<name>] guard} in [execute_control_op], which
      refuses a user drop of a live view's materialisation while letting the
      driver's own teardown / re-type drops through. That is {i not} the same
      question, and this is the escalation recorded on #475: [rv_refreshing]
      stayed set for the whole of [rv_flush] / [rv_create] / [rv_load], each of
      which awaits the store repeatedly. Lwt being cooperative, a {i user}
      [DROP TABLE _rv_x] scheduled into one of those windows bypassed the guard
      entirely and destroyed a live view's materialisation — including a view
      with nothing to do with the one being refreshed.

    The fix is a set of the table names the driver is {i currently} dropping,
    consulted by the guard instead of the busy flag. A depth counter alone does
    not fix it, whichever way the flag is spelled.

    The interleave here is deterministic rather than timing-dependent: a
    reactive-view callback registered via [register_view_callback] is invoked by
    the driver from {i inside} the refresh, with the guard held, which is
    exactly the window the escalation describes. *)

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

let contains ~needle haystack =
  let nl = String.length needle
  and hl = String.length haystack in
  let rec go i = i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1)) in
  go 0
;;

(* (group, count) rows, sorted. *)
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

let mv db name = rows2 db (Printf.sprintf "SELECT * FROM _rv_%s" name)

let table_exists db name =
  match run (Db.query db (Printf.sprintf "SELECT * FROM %s" name)) with
  | Ok stream ->
    ignore (run (Lwt_stream.to_list stream));
    true
  | Error _ -> false
;;

(* ------------------------------------------------------------------ *)
(* 1. THE escalation: a user drop inside a refresh window is refused.   *)
(* ------------------------------------------------------------------ *)

(* [v1]'s callback runs inside [rv_apply_and_notify], i.e. inside [rv_flush],
   i.e. with the driver's re-entrancy guard held.  Under the old bare bool the
   [DROP TABLE] guard read [not top.rv_refreshing] and therefore did NOT fire,
   so this drop of a DIFFERENT view's materialisation went straight through. *)
let user_drop_inside_a_refresh_window_is_refused () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT)";
    exec db "CREATE REACTIVE VIEW v1 AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "CREATE REACTIVE VIEW v2 AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    let seen = ref None in
    (match
       Db.register_view_callback db ~view_name:"v1" (fun _changes ->
         let open Lwt.Syntax in
         let* r = Db.execute db "DROP TABLE _rv_v2" in
         seen := Some r;
         Lwt.return_unit)
     with
     | Ok (_ : Db.view_callback) -> ()
     | Error (`Unknown_view v) -> Alcotest.failf "register_view_callback: unknown %s" v);
    exec db "INSERT INTO t VALUES (1, 'a')";
    (match !seen with
     | None ->
       Alcotest.fail
         "the v1 callback never ran — this test would prove nothing; the refresh window \
          was not entered"
     | Some (Ok ()) ->
       Alcotest.fail
         "DROP TABLE _rv_v2 was ALLOWED from inside v1's refresh window (#475): the \
          guard is still keyed on 'the driver is busy'"
     | Some (Error e) ->
       let msg = Format.asprintf "%a" Db.pp_error e in
       Alcotest.(check bool)
         "the refusal points at DROP REACTIVE VIEW"
         true
         (contains ~needle:"DROP REACTIVE VIEW" msg));
    Alcotest.(check bool) "_rv_v2 survived the attempt" true (table_exists db "_rv_v2");
    Alcotest.(check (list (pair string int)))
      "…and v2 is still correct"
      [ "a", 1 ]
      (mv db "v2");
    (* The depth counter was restored, so maintenance still runs afterwards — a
       leaked increment would silently park [drive_reactive] on its fast path
       and stop maintaining every view with no error at all. *)
    exec db "INSERT INTO t VALUES (2, 'a')";
    Alcotest.(check (list (pair string int)))
      "the guard was released: v1 is still maintained after the refresh"
      [ "a", 2 ]
      (mv db "v1");
    Alcotest.(check (list (pair string int))) "…and so is v2" [ "a", 2 ] (mv db "v2"))
;;

(* ------------------------------------------------------------------ *)
(* 2. The driver's OWN drop is still permitted.                         *)
(* ------------------------------------------------------------------ *)

(* Narrowing the guard must not close it on the driver.  A view created over an
   empty table gets a placeholder all-TEXT [_rv_…] schema; the first non-empty
   refresh DROPs and re-CREATEs it with the inferred types.  That drop is the
   one the guard has to let through, and it is now admitted by NAME, for the
   extent of that one statement. *)
let the_drivers_own_retype_drop_is_still_permitted () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec
      db
      "CREATE REACTIVE VIEW cf AS SELECT grp, COUNT(*) FROM t GROUP BY grp REFRESH FULL";
    (* provisional (all-TEXT) schema while empty *)
    Alcotest.(check bool) "the placeholder table exists" true (table_exists db "_rv_cf");
    (* the first non-empty refresh must re-type it, which means dropping it *)
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check (list (pair string int)))
      "re-typed and refilled by the driver's own DROP + CREATE"
      [ "a", 1 ]
      (mv db "cf");
    exec db "INSERT INTO t VALUES (2, 'b', 5)";
    Alcotest.(check (list (pair string int)))
      "…and maintained normally afterwards"
      [ "a", 1; "b", 1 ]
      (mv db "cf"))
;;

(* ------------------------------------------------------------------ *)
(* 3. Outside any refresh the guard is closed (the #469 baseline).      *)
(* ------------------------------------------------------------------ *)

let user_drop_outside_a_refresh_is_refused () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT)";
    exec db "CREATE REACTIVE VIEW v AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "INSERT INTO t VALUES (1, 'a')";
    (match run (Db.execute db "DROP TABLE _rv_v") with
     | Ok () -> Alcotest.fail "DROP TABLE _rv_v was allowed outside any refresh"
     | Error e ->
       let msg = Format.asprintf "%a" Db.pp_error e in
       Alcotest.(check bool)
         "refusal names the materialisation"
         true
         (contains ~needle:"internal reactive-view materialisation" msg));
    Alcotest.(check bool) "the table survived" true (table_exists db "_rv_v");
    (* and the supported spelling still works *)
    exec db "DROP REACTIVE VIEW v";
    Alcotest.(check bool) "DROP REACTIVE VIEW removed it" false (table_exists db "_rv_v"))
;;

(* A user table that merely starts with [_rv_] but backs no registered view is
   NOT protected — the registry is the authority (#469).  Pinned here because
   the new per-name permission set must not be mistaken for the naming
   convention. *)
let an_unregistered_rv_prefixed_table_is_droppable () =
  with_db (fun db ->
    exec db "CREATE TABLE _rv_notaview (a INTEGER)";
    exec db "DROP TABLE _rv_notaview";
    Alcotest.(check bool) "dropped" false (table_exists db "_rv_notaview"))
;;

let () =
  Alcotest.run
    "reactive view drop guard (#475)"
    [ ( "guard scope"
      , [ Alcotest.test_case
            "a user drop inside a refresh window is refused"
            `Quick
            user_drop_inside_a_refresh_window_is_refused
        ; Alcotest.test_case
            "the driver's own re-type drop is still permitted"
            `Quick
            the_drivers_own_retype_drop_is_still_permitted
        ; Alcotest.test_case
            "a user drop outside a refresh is refused"
            `Quick
            user_drop_outside_a_refresh_is_refused
        ; Alcotest.test_case
            "an unregistered _rv_ table is droppable"
            `Quick
            an_unregistered_rv_prefixed_table_is_droppable
        ] )
    ]
;;
