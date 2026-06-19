(** #417 Phase 0: the row-level delta feed.  Where {!Db.execute_with_dirty}
    reports only the *names* of mutated tables (#240), {!Db.execute_with_changes}
    carries the per-row deltas — inserted/deleted/updated rows with their rowids —
    that an incremental view-maintenance layer (DBSP-style) consumes. *)

module Db = Sqlocaml.Db
module Row = Sqlocaml_encoding.Row

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

let val_str = function
  | Row.V_int i -> Printf.sprintf "i%Ld" i
  | Row.V_text s -> Printf.sprintf "t%s" s
  | Row.V_null -> "null"
  | Row.V_real f -> Printf.sprintf "r%g" f
  | Row.V_blob b -> Printf.sprintf "b%s" (Bytes.to_string b)
;;

let row_str r = String.concat "," (Array.to_list (Array.map val_str r))

let change_str = function
  | Db.Inserted { rowid; row } -> Printf.sprintf "INS rowid=%Ld [%s]" rowid (row_str row)
  | Db.Deleted { rowid; row } -> Printf.sprintf "DEL rowid=%Ld [%s]" rowid (row_str row)
  | Db.Updated { rowid; old_row; new_row } ->
    Printf.sprintf "UPD rowid=%Ld [%s]->[%s]" rowid (row_str old_row) (row_str new_row)
;;

(* Run [sql] via [execute_with_changes] and stringify the per-table deltas. *)
let changes db sql : (string * string list) list =
  unwrap (run (Db.execute_with_changes db sql))
  |> List.map (fun (tbl, cs) -> tbl, List.map change_str cs)
;;

let check_changes msg expected got =
  Alcotest.(check (list (pair string (list string)))) msg expected got
;;

let test_insert_change () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k TEXT, v INTEGER)";
    check_changes
      "insert carries rowid + full row"
      [ "t", [ "INS rowid=1 [ta,i10]" ] ]
      (changes db "INSERT INTO t VALUES ('a', 10)"))
;;

let test_delete_change () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k TEXT, v INTEGER)";
    exec db "INSERT INTO t VALUES ('a', 10)";
    exec db "INSERT INTO t VALUES ('b', 20)";
    check_changes
      "delete carries rowid + removed row"
      [ "t", [ "DEL rowid=1 [ta,i10]" ] ]
      (changes db "DELETE FROM t WHERE v = 10"))
;;

let test_update_change () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k TEXT, v INTEGER)";
    exec db "INSERT INTO t VALUES ('a', 10)";
    check_changes
      "update carries rowid + pre/post images"
      [ "t", [ "UPD rowid=1 [ta,i10]->[ta,i99]" ] ]
      (changes db "UPDATE t SET v = 99 WHERE k = 'a'"))
;;

(* A multi-row UPDATE emits one change per matched row, in application (rowid)
   order. *)
let test_multi_row_update_order () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k TEXT, v INTEGER)";
    exec db "INSERT INTO t VALUES ('a', 1)";
    exec db "INSERT INTO t VALUES ('b', 2)";
    exec db "INSERT INTO t VALUES ('c', 3)";
    check_changes
      "three updates in rowid order"
      [ ( "t"
        , [ "UPD rowid=1 [ta,i1]->[ta,i0]"
          ; "UPD rowid=2 [tb,i2]->[tb,i0]"
          ; "UPD rowid=3 [tc,i3]->[tc,i0]"
          ] )
      ]
      (changes db "UPDATE t SET v = 0"))
;;

(* A no-op write produces no changes (matches the {!dirty_tables} empty contract). *)
let test_noop_delete_is_empty () =
  with_db (fun db ->
    exec db "CREATE TABLE t (k TEXT, v INTEGER)";
    exec db "INSERT INTO t VALUES ('a', 10)";
    check_changes
      "no row matched: empty feed"
      []
      (changes db "DELETE FROM t WHERE v = 999"))
;;

(* ON DELETE CASCADE: deleting the parent emits the parent's [Deleted] AND the
   child rows the engine silently removed — the whole point for IVM. *)
let test_cascade_delete_changes () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE dept (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE emp (id INTEGER PRIMARY KEY, d INTEGER REFERENCES dept(id) ON DELETE \
       CASCADE)";
    exec db "INSERT INTO dept VALUES (1)";
    exec db "INSERT INTO emp VALUES (10, 1)";
    (* dept sorts before emp; the child Deleted must be present. *)
    let got = changes db "DELETE FROM dept WHERE id = 1" in
    let tables = List.map fst got in
    Alcotest.(check (list string)) "both tables in feed" [ "dept"; "emp" ] tables;
    let emp_changes = List.assoc "emp" got in
    Alcotest.(check int) "one child row deleted" 1 (List.length emp_changes))
;;

(* Operation-kind + rowid summary, for cases where the exact row representation
   (e.g. an INTEGER PRIMARY KEY rowid alias) is not what we want to pin. *)
let kinds db sql : (string * string list) list =
  unwrap (run (Db.execute_with_changes db sql))
  |> List.map (fun (tbl, cs) ->
    ( tbl
    , List.map
        (function
          | Db.Inserted { rowid; _ } -> Printf.sprintf "INS:%Ld" rowid
          | Db.Deleted { rowid; _ } -> Printf.sprintf "DEL:%Ld" rowid
          | Db.Updated { rowid; _ } -> Printf.sprintf "UPD:%Ld" rowid)
        cs ))
;;

let check_kinds msg expected got =
  Alcotest.(check (list (pair string (list string)))) msg expected got
;;

(* REPLACE that displaces a row at a DIFFERENT rowid (here a UNIQUE-index
   conflict) deletes the old row then inserts the new one: the feed must pair the
   [Deleted] (old rowid) with the [Inserted] (new rowid) so an IVM consumer sees
   the net effect, not a phantom insert. *)
let test_replace_pairs_delete_insert () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, k TEXT, v TEXT)";
    exec db "CREATE UNIQUE INDEX t_k ON t (k)";
    exec db "INSERT INTO t VALUES (1, 'key', 'a')";
    (* new row rowid=2 conflicts with rowid=1 on UNIQUE(k) → displace 1, insert 2 *)
    check_kinds
      "replace = delete old rowid + insert new rowid"
      [ "t", [ "DEL:1"; "INS:2" ] ]
      (kinds db "INSERT OR REPLACE INTO t VALUES (2, 'key', 'b')"))
;;

(* A same-rowid REPLACE (conflict on the INTEGER PK alias itself) is a physical
   in-place overwrite — one [put], no separate delete — so it surfaces as a
   single [Inserted] of that rowid.  An IVM consumer treats "Inserted a rowid I
   already hold" as a replace.  Pinned so this contract is explicit. *)
let test_same_pk_replace_is_overwrite () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)";
    exec db "INSERT INTO t VALUES (1, 'a')";
    check_kinds
      "same-PK replace = in-place overwrite (INS only)"
      [ "t", [ "INS:1" ] ]
      (kinds db "INSERT OR REPLACE INTO t VALUES (1, 'b')"))
;;

(* Secondary-index UPSERT (ON CONFLICT(<unique index>) DO UPDATE) writes via the
   raw re-key path; its pre/post images must still reach the feed. *)
let test_secondary_index_upsert_update () =
  with_db (fun db ->
    exec db "CREATE TABLE u (id INTEGER PRIMARY KEY, k TEXT, v TEXT)";
    exec db "CREATE UNIQUE INDEX u_k ON u (k)";
    exec db "INSERT INTO u VALUES (1, 'key', 'a')";
    check_kinds
      "secondary-index upsert update of rowid 1"
      [ "u", [ "UPD:1" ] ]
      (kinds
         db
         "INSERT INTO u (id, k, v) VALUES (2, 'key', 'b') ON CONFLICT(k) DO UPDATE SET v \
          = excluded.v"))
;;

(* ON UPDATE CASCADE: the parent update AND the cascaded child update both reach
   the feed. *)
let test_on_update_cascade_child () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE dept (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE emp (id INTEGER PRIMARY KEY, d INTEGER REFERENCES dept(id) ON UPDATE \
       CASCADE)";
    exec db "INSERT INTO dept VALUES (1)";
    exec db "INSERT INTO emp VALUES (10, 1)";
    let got = kinds db "UPDATE dept SET id = 2 WHERE id = 1" in
    Alcotest.(check (list string))
      "both tables in feed"
      [ "dept"; "emp" ]
      (List.map fst got);
    Alcotest.(check int) "one child cascaded" 1 (List.length (List.assoc "emp" got)))
;;

(* Phase 0 boundary: FTS5 (and columnar) carry only the table NAME via
   {!Db.execute_with_dirty} (#240); the row-level feed does not yet cover them,
   so {!Db.execute_with_changes} reports nothing for an FTS-only write.  Pinned
   so the limitation is a conscious, tested contract — see the #417 follow-up. *)
let test_fts_has_no_row_feed_yet () =
  with_db (fun db ->
    exec db "CREATE VIRTUAL TABLE docs USING FTS5(title, body)";
    check_kinds
      "fts insert: row feed empty (name-only via execute_with_dirty)"
      []
      (kinds db "INSERT INTO docs (title, body) VALUES ('t', 'hello')"))
;;

(* QCheck: for any k, DELETE FROM t emits exactly one [Deleted] per row, with
   rowids 1..k in ascending application order; an empty table emits nothing. *)
let delete_all_feed_property =
  QCheck.Test.make
    ~count:50
    ~name:"DELETE FROM t feeds every row once, in rowid order"
    QCheck.(int_range 0 20)
    (fun k ->
       with_db (fun db ->
         exec db "CREATE TABLE t (v INTEGER)";
         for i = 1 to k do
           exec db (Printf.sprintf "INSERT INTO t VALUES (%d)" i)
         done;
         let got = kinds db "DELETE FROM t" in
         let expected =
           if k = 0
           then []
           else [ "t", List.init k (fun i -> Printf.sprintf "DEL:%d" (i + 1)) ]
         in
         got = expected))
;;

let () =
  Alcotest.run
    "dirty_changes_417"
    [ ( "core"
      , [ Alcotest.test_case "insert change" `Quick test_insert_change
        ; Alcotest.test_case "delete change" `Quick test_delete_change
        ; Alcotest.test_case "update change" `Quick test_update_change
        ; Alcotest.test_case "multi-row update order" `Quick test_multi_row_update_order
        ; Alcotest.test_case "noop delete empty" `Quick test_noop_delete_is_empty
        ] )
    ; ( "conflict paths"
      , [ Alcotest.test_case
            "replace pairs del+ins"
            `Quick
            test_replace_pairs_delete_insert
        ; Alcotest.test_case
            "same-pk replace overwrite"
            `Quick
            test_same_pk_replace_is_overwrite
        ; Alcotest.test_case
            "secondary-index upsert update"
            `Quick
            test_secondary_index_upsert_update
        ] )
    ; ( "cascades"
      , [ Alcotest.test_case "cascade delete changes" `Quick test_cascade_delete_changes
        ; Alcotest.test_case "on update cascade child" `Quick test_on_update_cascade_child
        ] )
    ; ( "phase0 boundary"
      , [ Alcotest.test_case "fts no row feed" `Quick test_fts_has_no_row_feed_yet ] )
    ; "property", [ QCheck_alcotest.to_alcotest delete_all_feed_property ]
    ]
;;
