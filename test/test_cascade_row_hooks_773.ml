(** #773 / #775: OCaml row hooks ({!Granary.Db.register_row_hook}) fire for
    every row an FK cascade writes, and for every row [PRAGMA not_null_repair]
    deletes — the two write paths that used to remove or modify rows with no
    hook plumbing at all.

    Both issues are one shape: a write that goes through
    [Exec.cascade_delete_row_in_tx] / [Exec.cascade_update_col_in_tx] (the six
    FK actions) or [Exec.apply_delete_row] straight from the repair PRAGMA
    never built the [before_hook]/[after_hook] closures the direct
    DELETE/UPDATE paths construct.  A [`Before] veto registered on a child
    table was therefore silently bypassable by writing to the PARENT, which
    contradicts the veto guarantee [register_row_hook]'s doc comment makes.

    What is pinned here:

    - all six FK actions fire the hook the equivalent direct write fires:
      [ON DELETE CASCADE] fires [`Delete]; [ON DELETE SET NULL],
      [ON DELETE SET DEFAULT], [ON UPDATE CASCADE], [ON UPDATE SET NULL] and
      [ON UPDATE SET DEFAULT] fire [`Update], carrying the post-image the
      write actually stores;
    - a multi-level cascade fires at every level, outermost-[`Before] first;
    - a [`Before] veto on a cascade step FAILS THE WHOLE PARENT STATEMENT
      rather than skipping the child row — skipping it would leave the
      dangling reference the cascade exists to prevent (see
      [docs/DECISIONS.md], #773);
    - an [`After] hook's error aborts the same way, matching #752;
    - the repair PRAGMA's deletes fire [`Delete] hooks on both of its entry
      points ([Db.execute] and [Db.query], #588), honour a veto by rolling the
      WHOLE repair back, and cascade into child tables whose hooks fire too;
    - the #752 semantics that must NOT have changed: registration order, no
      double-firing on the direct path, and the recursion/reentrancy guards a
      cascade-fired hook inherits. *)

open Lwt.Syntax
module Db = Granary.Db
module Cat = Granary_catalog.Catalog
module Row = Granary_encoding.Row
module Index_key = Granary_encoding.Index_key

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

let show_value = function
  | Row.V_text s -> s
  | Row.V_null -> "<null>"
  | Row.V_int n -> Int64.to_string n
  | Row.V_real f -> string_of_float f
  | Row.V_blob _ -> "<blob>"
;;

let show_row (r : Row.t) = Array.to_list r |> List.map show_value |> String.concat "|"

let show_opt_row = function
  | None -> "-"
  | Some r -> show_row r
;;

let texts db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream -> List.map show_row (run (Lwt_stream.to_list stream))
;;

let expect_rows db ~msg want sql = Alcotest.(check (list string)) msg want (texts db sql)

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let expect_error db ~needle sql =
  match run (Db.execute db sql) with
  | Ok () -> Alcotest.failf "%S was expected to fail with %S, but succeeded" sql needle
  | Error e ->
    let msg = Format.asprintf "%a" Db.pp_error e in
    Alcotest.(check bool)
      (Printf.sprintf "%S failed with %S (got %S)" sql needle msg)
      true
      (contains ~needle msg)
;;

let attach db ~table ~timing ~event fn =
  match Db.register_row_hook db ~table ~timing ~event fn with
  | Ok h -> h
  | Error (`Unknown_table t) -> Alcotest.failf "expected %S to be a known table" t
  | Error (`Columnstore_unsupported t) ->
    Alcotest.failf "expected %S to be a non-columnstore table" t
  | Error `Store_closing -> Alcotest.fail "expected the store not to be closing"
;;

(* Append one line per firing to [log], describing the mutation exactly enough
   to tell a [`Before] from an [`After] and a delete from an update. *)
let recorder log label (m : Db.row_mutation) =
  log
  := !log
     @ [ Printf.sprintf
           "%s %s old=%s new=%s"
           label
           m.Db.table
           (show_opt_row m.Db.old_row)
           (show_opt_row m.Db.new_row)
       ];
  Lwt.return (Ok ())
;;

let watch db log ~table ~timing ~event ~label =
  ignore (attach db ~table ~timing ~event (recorder log label) : Db.row_hook)
;;

(* Watch both timings of [event] on [table], labelling each. *)
let watch_both db log ~table ~event ~label =
  watch db log ~table ~timing:`Before ~event ~label:("before:" ^ label);
  watch db log ~table ~timing:`After ~event ~label:("after:" ^ label)
;;

let vetoer msg (_ : Db.row_mutation) = Lwt.return (Error msg)
let check_log ~msg want log = Alcotest.(check (list string)) msg want !log

(* ------------------------------------------------------------------ *)
(* #773: the six FK actions each fire the hook the direct write fires  *)
(* ------------------------------------------------------------------ *)

(* parent(id) <- child(id, pid) with the requested ON DELETE / ON UPDATE
   action, one parent row (1) and one child row (10, 1). *)
let fk_setup db ~action =
  exec db "PRAGMA foreign_keys = ON";
  exec db "CREATE TABLE parent (id INTEGER PRIMARY KEY)";
  exec
    db
    (Printf.sprintf
       "CREATE TABLE child (id INTEGER PRIMARY KEY, pid INTEGER DEFAULT 7, FOREIGN KEY \
        (pid) REFERENCES parent(id) %s)"
       action);
  exec db "INSERT INTO parent VALUES (1)";
  exec db "INSERT INTO child VALUES (10, 1)"
;;

let test_on_delete_cascade_fires_child_delete_hooks () =
  with_db (fun db ->
    fk_setup db ~action:"ON DELETE CASCADE";
    let log = ref [] in
    watch_both db log ~table:"child" ~event:`Delete ~label:"del";
    exec db "DELETE FROM parent WHERE id = 1";
    check_log
      ~msg:"both timings fired once, with the child's pre-image"
      [ "before:del child old=10|1 new=-"; "after:del child old=10|1 new=-" ]
      log;
    expect_rows db ~msg:"the child row is gone" [] "SELECT * FROM child")
;;

let test_on_delete_set_null_fires_child_update_hooks () =
  with_db (fun db ->
    fk_setup db ~action:"ON DELETE SET NULL";
    let log = ref [] in
    watch_both db log ~table:"child" ~event:`Update ~label:"upd";
    exec db "DELETE FROM parent WHERE id = 1";
    check_log
      ~msg:"the post-image carries the NULL the cascade wrote"
      [ "before:upd child old=10|1 new=10|<null>"
      ; "after:upd child old=10|1 new=10|<null>"
      ]
      log;
    expect_rows db ~msg:"and that is what landed" [ "10|<null>" ] "SELECT * FROM child")
;;

let test_on_delete_set_default_fires_child_update_hooks () =
  with_db (fun db ->
    fk_setup db ~action:"ON DELETE SET DEFAULT";
    let log = ref [] in
    watch_both db log ~table:"child" ~event:`Update ~label:"upd";
    exec db "DELETE FROM parent WHERE id = 1";
    check_log
      ~msg:"the post-image carries the column's DEFAULT (7)"
      [ "before:upd child old=10|1 new=10|7"; "after:upd child old=10|1 new=10|7" ]
      log;
    expect_rows db ~msg:"and that is what landed" [ "10|7" ] "SELECT * FROM child")
;;

let test_on_update_cascade_fires_child_update_hooks () =
  with_db (fun db ->
    fk_setup db ~action:"ON UPDATE CASCADE";
    let log = ref [] in
    watch_both db log ~table:"child" ~event:`Update ~label:"upd";
    exec db "UPDATE parent SET id = 2 WHERE id = 1";
    check_log
      ~msg:"the post-image carries the cascaded key"
      [ "before:upd child old=10|1 new=10|2"; "after:upd child old=10|1 new=10|2" ]
      log;
    expect_rows db ~msg:"and that is what landed" [ "10|2" ] "SELECT * FROM child")
;;

let test_on_update_set_null_fires_child_update_hooks () =
  with_db (fun db ->
    fk_setup db ~action:"ON UPDATE SET NULL";
    let log = ref [] in
    watch_both db log ~table:"child" ~event:`Update ~label:"upd";
    exec db "UPDATE parent SET id = 2 WHERE id = 1";
    check_log
      ~msg:"the post-image carries the NULL the cascade wrote"
      [ "before:upd child old=10|1 new=10|<null>"
      ; "after:upd child old=10|1 new=10|<null>"
      ]
      log;
    expect_rows db ~msg:"and that is what landed" [ "10|<null>" ] "SELECT * FROM child")
;;

let test_on_update_set_default_fires_child_update_hooks () =
  with_db (fun db ->
    fk_setup db ~action:"ON UPDATE SET DEFAULT";
    let log = ref [] in
    watch_both db log ~table:"child" ~event:`Update ~label:"upd";
    exec db "UPDATE parent SET id = 2 WHERE id = 1";
    check_log
      ~msg:"the post-image carries the column's DEFAULT (7)"
      [ "before:upd child old=10|1 new=10|7"; "after:upd child old=10|1 new=10|7" ]
      log;
    expect_rows db ~msg:"and that is what landed" [ "10|7" ] "SELECT * FROM child")
;;

(* The parent's OWN hooks are unaffected — they still fire once, from the
   direct path, and the cascade adds the child's inside that bracket rather
   than re-firing the parent's. *)
let test_parent_and_child_hooks_both_fire_exactly_once () =
  with_db (fun db ->
    fk_setup db ~action:"ON DELETE CASCADE";
    let log = ref [] in
    watch_both db log ~table:"parent" ~event:`Delete ~label:"P";
    watch_both db log ~table:"child" ~event:`Delete ~label:"C";
    exec db "DELETE FROM parent WHERE id = 1";
    check_log
      ~msg:"parent BEFORE, then the cascade's child pair, then parent AFTER"
      [ "before:P parent old=1 new=-"
      ; "before:C child old=10|1 new=-"
      ; "after:C child old=10|1 new=-"
      ; "after:P parent old=1 new=-"
      ]
      log)
;;

(* ------------------------------------------------------------------ *)
(* Multi-level cascade                                                 *)
(* ------------------------------------------------------------------ *)

let three_level_setup db =
  exec db "PRAGMA foreign_keys = ON";
  exec db "CREATE TABLE a (id INTEGER PRIMARY KEY)";
  exec
    db
    "CREATE TABLE b (id INTEGER PRIMARY KEY, aid INTEGER, FOREIGN KEY (aid) REFERENCES \
     a(id) ON DELETE CASCADE)";
  exec
    db
    "CREATE TABLE c (id INTEGER PRIMARY KEY, bid INTEGER, FOREIGN KEY (bid) REFERENCES \
     b(id) ON DELETE CASCADE)";
  exec db "INSERT INTO a VALUES (1)";
  exec db "INSERT INTO b VALUES (10, 1)";
  exec db "INSERT INTO c VALUES (100, 10)"
;;

let test_multi_level_cascade_fires_at_every_level () =
  with_db (fun db ->
    three_level_setup db;
    let log = ref [] in
    watch_both db log ~table:"b" ~event:`Delete ~label:"B";
    watch_both db log ~table:"c" ~event:`Delete ~label:"C";
    exec db "DELETE FROM a WHERE id = 1";
    (* [`Before] is the outer gate at every level: b's fires ahead of the
       fan-out that reaches c, and [`After] unwinds in the mirror order. *)
    check_log
      ~msg:"b's BEFORE brackets c's whole pair"
      [ "before:B b old=10|1 new=-"
      ; "before:C c old=100|10 new=-"
      ; "after:C c old=100|10 new=-"
      ; "after:B b old=10|1 new=-"
      ]
      log;
    expect_rows db ~msg:"grandchild gone" [] "SELECT * FROM c";
    expect_rows db ~msg:"child gone" [] "SELECT * FROM b")
;;

let test_grandchild_veto_aborts_the_whole_statement () =
  with_db (fun db ->
    three_level_setup db;
    ignore
      (attach
         db
         ~table:"c"
         ~timing:`Before
         ~event:`Delete
         (vetoer "no grandchildren harmed")
       : Db.row_hook);
    expect_error db ~needle:"no grandchildren harmed" "DELETE FROM a WHERE id = 1";
    expect_rows db ~msg:"grandchild survives" [ "100|10" ] "SELECT * FROM c";
    expect_rows db ~msg:"child survives" [ "10|1" ] "SELECT * FROM b";
    expect_rows
      db
      ~msg:"and so does the row the statement actually named"
      [ "1" ]
      "SELECT * FROM a")
;;

(* ------------------------------------------------------------------ *)
(* Veto: a cascade step cannot be skipped, only refused wholesale      *)
(* ------------------------------------------------------------------ *)

let test_before_veto_on_cascaded_delete_fails_the_parent_statement () =
  with_db (fun db ->
    fk_setup db ~action:"ON DELETE CASCADE";
    ignore
      (attach db ~table:"child" ~timing:`Before ~event:`Delete (vetoer "veto-del")
       : Db.row_hook);
    expect_error db ~needle:"veto-del" "DELETE FROM parent WHERE id = 1";
    (* Not a skip: skipping the child delete would leave child(10) pointing at
       a parent row the statement removed — exactly the dangling reference the
       cascade exists to prevent. *)
    expect_rows db ~msg:"child intact" [ "10|1" ] "SELECT * FROM child";
    expect_rows db ~msg:"parent intact" [ "1" ] "SELECT * FROM parent")
;;

let test_before_veto_on_cascaded_update_fails_the_parent_statement () =
  with_db (fun db ->
    fk_setup db ~action:"ON UPDATE CASCADE";
    ignore
      (attach db ~table:"child" ~timing:`Before ~event:`Update (vetoer "veto-upd")
       : Db.row_hook);
    expect_error db ~needle:"veto-upd" "UPDATE parent SET id = 2 WHERE id = 1";
    expect_rows db ~msg:"child intact" [ "10|1" ] "SELECT * FROM child";
    expect_rows db ~msg:"parent intact" [ "1" ] "SELECT * FROM parent")
;;

let test_before_veto_on_cascaded_set_null_fails_the_parent_statement () =
  with_db (fun db ->
    fk_setup db ~action:"ON DELETE SET NULL";
    ignore
      (attach db ~table:"child" ~timing:`Before ~event:`Update (vetoer "veto-setnull")
       : Db.row_hook);
    expect_error db ~needle:"veto-setnull" "DELETE FROM parent WHERE id = 1";
    expect_rows db ~msg:"child intact" [ "10|1" ] "SELECT * FROM child";
    expect_rows db ~msg:"parent intact" [ "1" ] "SELECT * FROM parent")
;;

(* The error names the cascade, so a caller can tell a vetoed cascade step
   from a veto of the statement's own table. *)
let test_a_vetoed_cascade_error_names_the_cascade_and_the_child_table () =
  with_db (fun db ->
    fk_setup db ~action:"ON DELETE CASCADE";
    ignore
      (attach db ~table:"child" ~timing:`Before ~event:`Delete (vetoer "nope")
       : Db.row_hook);
    expect_error
      db
      ~needle:"FOREIGN KEY cascade on 'child': before row hook on 'child': nope"
      "DELETE FROM parent WHERE id = 1")
;;

(* #752's rule that an [`After] error aborts too, applied to a cascade step. *)
let test_after_hook_error_on_a_cascade_step_aborts_the_statement () =
  with_db (fun db ->
    fk_setup db ~action:"ON DELETE CASCADE";
    ignore
      (attach db ~table:"child" ~timing:`After ~event:`Delete (vetoer "boom")
       : Db.row_hook);
    expect_error db ~needle:"boom" "DELETE FROM parent WHERE id = 1";
    expect_rows db ~msg:"child restored by the rollback" [ "10|1" ] "SELECT * FROM child";
    expect_rows db ~msg:"parent restored too" [ "1" ] "SELECT * FROM parent")
;;

(* A raising hook is normalised the same way #752 normalises one on the direct
   path — [Failure], never an escaped exception. *)
let test_a_raising_cascade_hook_is_normalised_not_escaped () =
  with_db (fun db ->
    fk_setup db ~action:"ON DELETE CASCADE";
    ignore
      (attach db ~table:"child" ~timing:`Before ~event:`Delete (fun _ -> raise Not_found)
       : Db.row_hook);
    expect_error db ~needle:"raised: Not_found" "DELETE FROM parent WHERE id = 1")
;;

(* ------------------------------------------------------------------ *)
(* #752 semantics that must be unchanged                               *)
(* ------------------------------------------------------------------ *)

(* Multiple hooks on one (table, timing, event) fire in registration order —
   #746's contract, reused by #752 — on the cascade path as on the direct one. *)
let test_cascade_fires_multiple_hooks_in_registration_order () =
  with_db (fun db ->
    fk_setup db ~action:"ON DELETE CASCADE";
    let log = ref [] in
    watch db log ~table:"child" ~timing:`Before ~event:`Delete ~label:"first";
    watch db log ~table:"child" ~timing:`Before ~event:`Delete ~label:"second";
    watch db log ~table:"child" ~timing:`Before ~event:`Delete ~label:"third";
    exec db "DELETE FROM parent WHERE id = 1";
    check_log
      ~msg:"registration order, not reverse"
      [ "first child old=10|1 new=-"
      ; "second child old=10|1 new=-"
      ; "third child old=10|1 new=-"
      ]
      log)
;;

(* The direct path must not have gained a second firing from the new lookup:
   a plain DELETE on the child fires its hooks exactly once. *)
let test_direct_delete_on_the_child_still_fires_exactly_once () =
  with_db (fun db ->
    fk_setup db ~action:"ON DELETE CASCADE";
    let log = ref [] in
    watch_both db log ~table:"child" ~event:`Delete ~label:"C";
    exec db "DELETE FROM child WHERE id = 10";
    check_log
      ~msg:"one before, one after — no double fire"
      [ "before:C child old=10|1 new=-"; "after:C child old=10|1 new=-" ]
      log)
;;

(* Same for a direct UPDATE of the child's FK column. *)
let test_direct_update_on_the_child_still_fires_exactly_once () =
  with_db (fun db ->
    fk_setup db ~action:"ON UPDATE CASCADE";
    exec db "INSERT INTO parent VALUES (2)";
    let log = ref [] in
    watch_both db log ~table:"child" ~event:`Update ~label:"C";
    exec db "UPDATE child SET pid = 2 WHERE id = 10";
    check_log
      ~msg:"one before, one after"
      [ "before:C child old=10|1 new=10|2"; "after:C child old=10|1 new=10|2" ]
      log)
;;

(* A cascade over a table with no hooks registered anywhere behaves exactly as
   it did before #773 — the fast path. *)
let test_a_cascade_with_no_hooks_registered_is_unchanged () =
  with_db (fun db ->
    fk_setup db ~action:"ON DELETE CASCADE";
    exec db "DELETE FROM parent WHERE id = 1";
    expect_rows db ~msg:"the cascade still ran" [] "SELECT * FROM child")
;;

(* An unregistered hook stops firing on the cascade path too. *)
let test_unregister_stops_cascade_delivery () =
  with_db (fun db ->
    fk_setup db ~action:"ON DELETE CASCADE";
    exec db "INSERT INTO parent VALUES (2)";
    exec db "INSERT INTO child VALUES (20, 2)";
    let log = ref [] in
    let h = attach db ~table:"child" ~timing:`Before ~event:`Delete (recorder log "C") in
    exec db "DELETE FROM parent WHERE id = 1";
    check_log ~msg:"fired for the first cascade" [ "C child old=10|1 new=-" ] log;
    Db.unregister_row_hook db h;
    exec db "DELETE FROM parent WHERE id = 2";
    check_log ~msg:"and not for the second" [ "C child old=10|1 new=-" ] log)
;;

(* #752 rounds 6/7: a hook's own nested DML needing a FRESH write transaction
   is refused, not deadlocked.  A cascade-fired hook inherits that guard by
   construction — it goes through the same [Db.fire_ocaml_row_hook]. *)
let test_a_cascade_fired_hooks_reentrant_autocommit_dml_fails_fast () =
  with_db (fun db ->
    fk_setup db ~action:"ON DELETE CASCADE";
    exec db "CREATE TABLE audit (id INTEGER)";
    ignore
      (attach db ~table:"child" ~timing:`Before ~event:`Delete (fun _ ->
         let* r = Db.execute db "INSERT INTO audit VALUES (1)" in
         match r with
         | Ok () -> Lwt.return (Ok ())
         | Error e -> Lwt.return (Error (Format.asprintf "%a" Db.pp_error e)))
       : Db.row_hook);
    expect_error
      db
      ~needle:"a row hook callback attempted to open a new write"
      "DELETE FROM parent WHERE id = 1";
    expect_rows db ~msg:"nothing was audited" [] "SELECT * FROM audit")
;;

(* And the documented safe pattern — an explicit transaction opened BEFORE the
   statement that fires the hook — works from a cascade step. *)
let test_a_cascade_fired_hook_can_write_inside_an_ambient_transaction () =
  with_db (fun db ->
    fk_setup db ~action:"ON DELETE CASCADE";
    exec db "CREATE TABLE audit (id INTEGER)";
    ignore
      (attach db ~table:"child" ~timing:`After ~event:`Delete (fun _ ->
         let* r = Db.execute db "INSERT INTO audit VALUES (99)" in
         match r with
         | Ok () -> Lwt.return (Ok ())
         | Error e -> Lwt.return (Error (Format.asprintf "%a" Db.pp_error e)))
       : Db.row_hook);
    exec db "BEGIN";
    exec db "DELETE FROM parent WHERE id = 1";
    exec db "COMMIT";
    expect_rows
      db
      ~msg:"the cascade-fired hook wrote its audit row"
      [ "99" ]
      "SELECT * FROM audit";
    expect_rows db ~msg:"and the cascade still applied" [] "SELECT * FROM child")
;;

(* ------------------------------------------------------------------ *)
(* #775: PRAGMA not_null_repair                                        *)
(* ------------------------------------------------------------------ *)

let open_db path =
  match run (Granary_unix.open_file ~path ()) with
  | Ok db -> db
  | Error e -> Alcotest.failf "open %s: %a" path Db.pp_error e
;;

let close_db db =
  try run (Db.close db) with
  | _ -> ()
;;

(* The #563/#567 fixture recipe, lifted verbatim from [test_not_null_567.ml]:
   register [cols] as the implicit PRIMARY KEY index of an already-populated
   table, directly in the catalog, with the index entries written by hand so
   the index is POPULATED the way a real pre-#530 file's is.  [Catalog.open_]
   re-derives NOT NULL from that index on the next open, which is the only
   route to a stored NULL in a NOT NULL column — the engine now refuses to
   write one, and refusing is exactly why the repair PRAGMA exists. *)
let add_populated_implicit_pk_index path ~table ~cols ~entries =
  run
    (let* store =
       let* r = Granary_unix.Store.open_file ~path () in
       match r with
       | Ok s -> Lwt.return s
       | Error _ -> Alcotest.failf "cannot reopen store %s" path
     in
     let* cat = Cat.open_ store in
     let* r =
       Cat.create_index
         cat
         ~name:(Printf.sprintf "__pk_%s_%s_0" table (String.concat "_" cols))
         ~table
         ~columns:cols
         ~unique:true
         ~expr_flags:(List.map (fun _ -> false) cols)
         ~where_sql:None
         ~origin:`Implicit_pk
     in
     match r with
     | Error m -> Alcotest.failf "create_index: %s" m
     | Ok (idx : Cat.index_info) ->
       let* tx = Granary_store.Store.rw_begin store in
       let* () =
         Lwt_list.iter_s
           (fun (rowid, key_vals) ->
              Granary_store.Store.put
                tx
                idx.Cat.idx_tree_id
                (Index_key.encode key_vals ~rowid)
                Bytes.empty)
           entries
       in
       let* () = Granary_store.Store.commit tx in
       Granary_store.Store.close store)
;;

(* Seed a file-backed database with [seed_sql], plant the legacy implicit PK
   index, reopen, run [after_reopen], then hand the handle to [f]. *)
let with_legacy_null_key_db ~seed_sql ~table ~pk_cols ~entries ~after_reopen f =
  let path = Filename.temp_file "granary_773_" ".db" in
  Sys.remove path;
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () ->
       let db = open_db path in
       List.iter (exec db) seed_sql;
       close_db db;
       add_populated_implicit_pk_index path ~table ~cols:pk_cols ~entries;
       let db = open_db path in
       Fun.protect
         ~finally:(fun () -> close_db db)
         (fun () ->
            List.iter (exec db) after_reopen;
            f db))
;;

(* stock(sw, si, qty) with si NOT NULL by re-derivation and two rows violating
   it — rowids 2 and 3, in ascending rowid order, which is the order the
   repair visits them in (#541). *)
let with_violating_db f =
  with_legacy_null_key_db
    ~seed_sql:
      [ "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER)"
      ; "INSERT INTO stock VALUES (1, 2, 50)"
      ; "INSERT INTO stock VALUES (1, NULL, 60)"
      ; "INSERT INTO stock VALUES (2, NULL, 70)"
      ]
    ~table:"stock"
    ~pk_cols:[ "sw"; "si" ]
    ~entries:
      [ 1L, [ Index_key.IK_int 1L; Index_key.IK_int 2L ]
      ; 2L, [ Index_key.IK_int 1L; Index_key.IK_null ]
      ; 3L, [ Index_key.IK_int 2L; Index_key.IK_null ]
      ]
    ~after_reopen:[]
    f
;;

let test_repair_fires_delete_hooks_for_each_victim () =
  with_violating_db (fun db ->
    let log = ref [] in
    watch_both db log ~table:"stock" ~event:`Delete ~label:"T";
    exec db "PRAGMA not_null_repair";
    (* Every [`Before] runs before any row is removed, then the removals, then
       every [`After] — the same statement-level bracketing [execute_delete]
       gives a plain multi-row DELETE. *)
    check_log
      ~msg:"all the BEFOREs, then all the AFTERs"
      [ "before:T stock old=1|<null>|60 new=-"
      ; "before:T stock old=2|<null>|70 new=-"
      ; "after:T stock old=1|<null>|60 new=-"
      ; "after:T stock old=2|<null>|70 new=-"
      ]
      log;
    expect_rows db ~msg:"only the clean row survives" [ "1|2|50" ] "SELECT * FROM stock")
;;

(* #588 gave the repair two entry points; the hooks must fire on both. *)
let test_repair_via_the_query_path_fires_delete_hooks_too () =
  with_violating_db (fun db ->
    let log = ref [] in
    watch_both db log ~table:"stock" ~event:`Delete ~label:"T";
    Alcotest.(check (list string))
      "the streaming spelling still reports what it removed"
      [ "stock|si|2" ]
      (texts db "PRAGMA not_null_repair");
    check_log
      ~msg:"and fires the same hooks"
      [ "before:T stock old=1|<null>|60 new=-"
      ; "before:T stock old=2|<null>|70 new=-"
      ; "after:T stock old=1|<null>|60 new=-"
      ; "after:T stock old=2|<null>|70 new=-"
      ]
      log)
;;

let test_repair_honours_a_before_veto_and_rolls_the_whole_repair_back () =
  with_violating_db (fun db ->
    ignore
      (attach db ~table:"stock" ~timing:`Before ~event:`Delete (vetoer "keep it")
       : Db.row_hook);
    expect_error db ~needle:"keep it" "PRAGMA not_null_repair";
    expect_rows
      db
      ~msg:"nothing was deleted — the veto refuses the repair, it does not exempt a row"
      [ "1|2|50"; "1|<null>|60"; "2|<null>|70" ]
      "SELECT * FROM stock")
;;

let test_repair_veto_error_names_the_pragma () =
  with_violating_db (fun db ->
    ignore
      (attach db ~table:"stock" ~timing:`Before ~event:`Delete (vetoer "nope")
       : Db.row_hook);
    expect_error
      db
      ~needle:"PRAGMA not_null_repair on 'stock': before row hook on 'stock': nope"
      "PRAGMA not_null_repair")
;;

(* p(pid, v) with v NOT NULL by re-derivation; the violating row (pid = 1) is
   referenced by c, so repairing p cascades into c.  #775 and #773 composed —
   the case neither issue could pin on its own. *)
let with_violating_parent_and_child_db f =
  with_legacy_null_key_db
    ~seed_sql:
      [ "CREATE TABLE p (pid INTEGER, v INTEGER)"
      ; "INSERT INTO p VALUES (1, NULL)"
      ; "INSERT INTO p VALUES (2, 5)"
      ]
    ~table:"p"
    ~pk_cols:[ "v" ]
    ~entries:[ 1L, [ Index_key.IK_null ]; 2L, [ Index_key.IK_int 5L ] ]
    ~after_reopen:
      [ "PRAGMA foreign_keys = ON"
      ; "CREATE UNIQUE INDEX p_pid ON p (pid)"
      ; "CREATE TABLE c (id INTEGER PRIMARY KEY, cpid INTEGER, FOREIGN KEY (cpid) \
         REFERENCES p(pid) ON DELETE CASCADE)"
      ; "INSERT INTO c VALUES (10, 1)"
      ]
    f
;;

let test_repair_cascade_fires_the_childs_hooks () =
  with_violating_parent_and_child_db (fun db ->
    let log = ref [] in
    watch_both db log ~table:"c" ~event:`Delete ~label:"C";
    exec db "PRAGMA not_null_repair";
    check_log
      ~msg:"the cascaded child delete fired its own hooks"
      [ "before:C c old=10|1 new=-"; "after:C c old=10|1 new=-" ]
      log;
    expect_rows db ~msg:"child gone" [] "SELECT * FROM c";
    expect_rows db ~msg:"only the clean parent row survives" [ "2|5" ] "SELECT * FROM p")
;;

(* A veto from the CHILD's hook refuses the repair as a whole, for the same
   reason a veto on any other cascade step does. *)
let test_repair_cascade_veto_rolls_the_repair_back () =
  with_violating_parent_and_child_db (fun db ->
    ignore
      (attach db ~table:"c" ~timing:`Before ~event:`Delete (vetoer "child says no")
       : Db.row_hook);
    expect_error db ~needle:"child says no" "PRAGMA not_null_repair";
    expect_rows db ~msg:"child intact" [ "10|1" ] "SELECT * FROM c";
    expect_rows
      db
      ~msg:"and the parent row the repair would have removed is still there"
      [ "1|<null>"; "2|5" ]
      "SELECT * FROM p")
;;

let () =
  Alcotest.run
    "cascade + repair row hooks (#773/#775)"
    [ ( "FK actions fire child hooks (#773)"
      , [ Alcotest.test_case
            "ON DELETE CASCADE"
            `Quick
            test_on_delete_cascade_fires_child_delete_hooks
        ; Alcotest.test_case
            "ON DELETE SET NULL"
            `Quick
            test_on_delete_set_null_fires_child_update_hooks
        ; Alcotest.test_case
            "ON DELETE SET DEFAULT"
            `Quick
            test_on_delete_set_default_fires_child_update_hooks
        ; Alcotest.test_case
            "ON UPDATE CASCADE"
            `Quick
            test_on_update_cascade_fires_child_update_hooks
        ; Alcotest.test_case
            "ON UPDATE SET NULL"
            `Quick
            test_on_update_set_null_fires_child_update_hooks
        ; Alcotest.test_case
            "ON UPDATE SET DEFAULT"
            `Quick
            test_on_update_set_default_fires_child_update_hooks
        ; Alcotest.test_case
            "parent and child hooks each fire exactly once"
            `Quick
            test_parent_and_child_hooks_both_fire_exactly_once
        ] )
    ; ( "multi-level cascade"
      , [ Alcotest.test_case
            "fires at every level, outermost BEFORE first"
            `Quick
            test_multi_level_cascade_fires_at_every_level
        ; Alcotest.test_case
            "a grandchild's veto aborts the whole statement"
            `Quick
            test_grandchild_veto_aborts_the_whole_statement
        ] )
    ; ( "veto semantics (#773)"
      , [ Alcotest.test_case
            "a vetoed cascaded delete fails the parent statement"
            `Quick
            test_before_veto_on_cascaded_delete_fails_the_parent_statement
        ; Alcotest.test_case
            "a vetoed cascaded update fails the parent statement"
            `Quick
            test_before_veto_on_cascaded_update_fails_the_parent_statement
        ; Alcotest.test_case
            "a vetoed cascaded SET NULL fails the parent statement"
            `Quick
            test_before_veto_on_cascaded_set_null_fails_the_parent_statement
        ; Alcotest.test_case
            "the error names the cascade and the child table"
            `Quick
            test_a_vetoed_cascade_error_names_the_cascade_and_the_child_table
        ; Alcotest.test_case
            "an After error on a cascade step aborts too"
            `Quick
            test_after_hook_error_on_a_cascade_step_aborts_the_statement
        ; Alcotest.test_case
            "a raising cascade hook is normalised, not escaped"
            `Quick
            test_a_raising_cascade_hook_is_normalised_not_escaped
        ] )
    ; ( "#752 semantics unchanged"
      , [ Alcotest.test_case
            "cascade fires multiple hooks in registration order"
            `Quick
            test_cascade_fires_multiple_hooks_in_registration_order
        ; Alcotest.test_case
            "a direct DELETE on the child still fires exactly once"
            `Quick
            test_direct_delete_on_the_child_still_fires_exactly_once
        ; Alcotest.test_case
            "a direct UPDATE on the child still fires exactly once"
            `Quick
            test_direct_update_on_the_child_still_fires_exactly_once
        ; Alcotest.test_case
            "a cascade with no hooks registered is unchanged"
            `Quick
            test_a_cascade_with_no_hooks_registered_is_unchanged
        ; Alcotest.test_case
            "unregister stops cascade delivery"
            `Quick
            test_unregister_stops_cascade_delivery
        ; Alcotest.test_case
            "a cascade-fired hook's reentrant autocommit DML fails fast"
            `Quick
            test_a_cascade_fired_hooks_reentrant_autocommit_dml_fails_fast
        ; Alcotest.test_case
            "a cascade-fired hook can write inside an ambient transaction"
            `Quick
            test_a_cascade_fired_hook_can_write_inside_an_ambient_transaction
        ] )
    ; ( "PRAGMA not_null_repair (#775)"
      , [ Alcotest.test_case
            "fires delete hooks for each victim"
            `Quick
            test_repair_fires_delete_hooks_for_each_victim
        ; Alcotest.test_case
            "the query-path spelling fires them too"
            `Quick
            test_repair_via_the_query_path_fires_delete_hooks_too
        ; Alcotest.test_case
            "a Before veto rolls the whole repair back"
            `Quick
            test_repair_honours_a_before_veto_and_rolls_the_whole_repair_back
        ; Alcotest.test_case
            "the veto error names the PRAGMA"
            `Quick
            test_repair_veto_error_names_the_pragma
        ; Alcotest.test_case
            "the repair's cascade fires the child's hooks"
            `Quick
            test_repair_cascade_fires_the_childs_hooks
        ; Alcotest.test_case
            "a child's veto rolls the repair back"
            `Quick
            test_repair_cascade_veto_rolls_the_repair_back
        ] )
    ]
;;
