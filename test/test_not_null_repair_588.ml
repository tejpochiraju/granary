(** #588: [PRAGMA not_null_repair] is destructive, so it is reachable through the
    WRITE api.

    The repair DELETEs rows, but it was dispatched as a query, so
    [Db.execute db "PRAGMA not_null_repair"] answered
    ["Exec.execute: use Exec.query for read operations"] and deleted nothing.
    The natural call for a statement that mutates and returns no interesting
    rows was the one that silently did nothing but error, and a caller had to
    know to route a destructive operation through the read path to make it
    happen. Its read-only half, [PRAGMA not_null_check], is genuinely a read and
    stays on the query path alone — the two now differ in call shape the way
    they differ in effect.

    {b Both entry points perform the repair, and that is deliberate.} They differ
    only in what they hand back: [Db.execute] reports the number of rows DELETED
    as the statement's change count, [Db.query] streams the per-column
    (table, column, count) report an operator reads. Removing the query path
    would have made the report unreachable, which is the opposite of #563's
    point.

    {b The second half of the issue.} Travelling the query path also made the
    repair reachable from contexts that forbid writes, where it failed with
    ["write attempted under a read-only transaction (In_ro_txn)"] — a
    storage-layer message about an internal mode, from a statement whose problem
    is that it is destructive. It is now a statement-level refusal naming the
    read-only half that does work against a snapshot.

    The fixture is #548/#563's recipe: rows go in while the columns are genuinely
    nullable, then the implicit PK index — the record [Catalog.open_] re-derives
    NOT NULL from (#533) — is registered in the catalog afterwards. *)

open Lwt.Syntax
module Db = Granary.Db
module Cat = Granary_catalog.Catalog
module Row = Granary_encoding.Row
module H = Granary_store.History

let run = Lwt_main.run

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let open_db ?(as_of_history = false) path =
  match run (Granary_unix.open_file ~as_of_history ~path ()) with
  | Ok db -> db
  | Error e -> Alcotest.failf "open %s: %a" path Db.pp_error e
;;

let close_db db =
  try run (Db.close db) with
  | _ -> ()
;;

let show_value = function
  | Row.V_text s -> s
  | Row.V_null -> "<null>"
  | Row.V_int n -> Int64.to_string n
  | Row.V_real f -> string_of_float f
  | Row.V_blob _ -> "<blob>"
;;

let texts db sql =
  let rows =
    run
      (let* r = Db.query db sql in
       match r with
       | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
       | Ok stream -> Lwt_stream.to_list stream)
  in
  List.map
    (fun (r : Row.t) -> Array.to_list r |> List.map show_value |> String.concat "|")
    rows
;;

let register_implicit_pk_index path ~table ~cols =
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
     | Ok _ -> Granary_store.Store.close store
     | Error m -> Alcotest.failf "create_index: %s" m)
;;

(* [.aslog] is the as-of commit log the read-only-snapshot cases open with
   [~as_of_history:true]; it is a sibling of the database file and would
   otherwise be left behind. *)
let with_temp_path f =
  let path = Filename.temp_file "granary_588_" ".db" in
  let rm p =
    try Sys.remove p with
    | _ -> ()
  in
  rm path;
  Fun.protect
    ~finally:(fun () ->
      rm path;
      rm (path ^ "-wal");
      rm (path ^ ".aslog"))
    (fun () -> f path)
;;

(* Four rows; [a] and [b] are both re-derived NOT NULL from the composite
   implicit PK index.  Row 2 violates [a] alone, row 3 violates [b] alone, and
   row 4 violates BOTH — which is what separates "rows deleted" from "sum of the
   per-column counts": the report says 2 for [a] and 2 for [b], and three rows
   go. *)
let with_legacy_db f =
  with_temp_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE t (k INTEGER, a INTEGER, b INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10, 20)";
    exec db "INSERT INTO t VALUES (2, NULL, 20)";
    exec db "INSERT INTO t VALUES (3, 10, NULL)";
    exec db "INSERT INTO t VALUES (4, NULL, NULL)";
    close_db db;
    register_implicit_pk_index path ~table:"t" ~cols:[ "a"; "b" ];
    let db = open_db path in
    Fun.protect ~finally:(fun () -> close_db db) (fun () -> f db))
;;

let surviving_keys db = texts db "SELECT k FROM t ORDER BY k"

(* ------------------------------------------------------------------ *)

(* The headline.  On main this returned
   [Error runtime error: Exec.execute: use Exec.query for read operations] and
   deleted nothing. *)
let execute_performs_the_repair () =
  with_legacy_db (fun db ->
    Alcotest.(check (list string))
      "the fixture really does hold violations"
      [ "t|a|2"; "t|b|2" ]
      (texts db "PRAGMA not_null_check");
    (match run (Db.execute db "PRAGMA not_null_repair") with
     | Ok () -> ()
     | Error e -> Alcotest.failf "Db.execute PRAGMA not_null_repair: %a" Db.pp_error e);
    Alcotest.(check (list string))
      "only the clean row survives"
      [ "1" ]
      (surviving_keys db);
    Alcotest.(check (list string))
      "and the survey is silent afterwards"
      []
      (texts db "PRAGMA not_null_check"))
;;

(* The change count is ROWS DELETED, deduplicated — not the sum of the
   per-column counts (which is 4 here for 3 rows). *)
let the_change_count_is_rows_deleted () =
  with_legacy_db (fun db ->
    match run (Db.execute_change_count db "PRAGMA not_null_repair") with
    | Error e -> Alcotest.failf "execute_change_count: %a" Db.pp_error e
    | Ok n ->
      Alcotest.(check int) "three rows deleted, not four column violations" 3 n;
      Alcotest.(check (list string))
        "and CHANGES() agrees"
        [ "3" ]
        (texts db "SELECT CHANGES()"))
;;

(* The query path is unchanged: it still repairs AND still streams the report,
   which is the half [Db.execute] cannot hand back. *)
let query_still_repairs_and_reports () =
  with_legacy_db (fun db ->
    Alcotest.(check (list string))
      "the per-column report"
      [ "t|a|2"; "t|b|2" ]
      (texts db "PRAGMA not_null_repair");
    Alcotest.(check (list string)) "and the rows are gone" [ "1" ] (surviving_keys db))
;;

(* The read-only half stays a read: [Db.execute] must not perform it, or the two
   PRAGMAs would stop differing in call shape the way they differ in effect. *)
let not_null_check_stays_on_the_read_path () =
  with_legacy_db (fun db ->
    match run (Db.execute db "PRAGMA not_null_check") with
    | Ok () -> Alcotest.fail "Db.execute performed PRAGMA not_null_check"
    | Error e ->
      let msg = Format.asprintf "%a" Db.pp_error e in
      Alcotest.(check bool)
        (Printf.sprintf "refused as a read op (got %S)" msg)
        true
        (contains ~needle:"use Exec.query for read operations" msg))
;;

(* The repair participates in an ambient explicit transaction rather than
   opening its own, so a ROLLBACK undoes it. *)
let the_repair_is_transactional () =
  with_legacy_db (fun db ->
    exec db "BEGIN";
    (match run (Db.execute db "PRAGMA not_null_repair") with
     | Ok () -> ()
     | Error e -> Alcotest.failf "repair inside BEGIN: %a" Db.pp_error e);
    Alcotest.(check (list string))
      "read-your-own-writes inside the transaction"
      [ "1" ]
      (surviving_keys db);
    exec db "ROLLBACK";
    Alcotest.(check (list string))
      "ROLLBACK puts every row back"
      [ "1"; "2"; "3"; "4" ]
      (surviving_keys db))
;;

(* The second half of #588: a read-only snapshot gets a statement-level refusal
   naming the destructive operation and the read-only command that does work,
   not the storage layer's [In_ro_txn] message. *)
let a_read_only_snapshot_is_refused_at_the_statement_level () =
  with_temp_path (fun path ->
    let db = open_db ~as_of_history:true path in
    Fun.protect
      ~finally:(fun () -> close_db db)
      (fun () ->
         exec db "CREATE TABLE s (v INTEGER)";
         exec db "INSERT INTO s VALUES (1)";
         let t1 =
           match List.rev (run (Db.history_log db)) with
           | last :: _ -> last.H.txn_id
           | [] -> Alcotest.fail "history log is empty after a commit"
         in
         exec db "INSERT INTO s VALUES (2)";
         Db.history_pin db ~txn_id:t1;
         match run (Db.query_as_of db (`Txn t1) "PRAGMA not_null_repair") with
         | Ok _ -> Alcotest.fail "a repair ran against a historical snapshot"
         | Error e ->
           let msg = Format.asprintf "%a" Db.pp_error e in
           List.iter
             (fun needle ->
                Alcotest.(check bool)
                  (Printf.sprintf "the refusal mentions %S (got %S)" needle msg)
                  true
                  (contains ~needle msg))
             [ "PRAGMA not_null_repair"; "read-only"; "PRAGMA not_null_check" ];
           Alcotest.(check bool)
             (Printf.sprintf "and not the storage-layer mode (got %S)" msg)
             false
             (contains ~needle:"In_ro_txn" msg)))
;;

(* A snapshot read of the REPORT half is still legitimate — surveying history is
   exactly the thing a read-only snapshot is for, so the refusal above must not
   have caught it too. *)
let a_read_only_snapshot_still_serves_the_report () =
  with_temp_path (fun path ->
    let db = open_db ~as_of_history:true path in
    Fun.protect
      ~finally:(fun () -> close_db db)
      (fun () ->
         exec db "CREATE TABLE s (v INTEGER)";
         exec db "INSERT INTO s VALUES (1)";
         let t1 =
           match List.rev (run (Db.history_log db)) with
           | last :: _ -> last.H.txn_id
           | [] -> Alcotest.fail "history log is empty after a commit"
         in
         exec db "INSERT INTO s VALUES (2)";
         Db.history_pin db ~txn_id:t1;
         match run (Db.query_as_of db (`Txn t1) "PRAGMA not_null_check") with
         | Error e ->
           Alcotest.failf "not_null_check refused on a snapshot: %a" Db.pp_error e
         | Ok stream ->
           Alcotest.(check int)
             "a clean snapshot reports nothing"
             0
             (List.length (run (Lwt_stream.to_list stream)))))
;;

let () =
  Alcotest.run
    "not_null_repair_588"
    [ ( "write api"
      , List.map
          (fun (n, f) -> Alcotest.test_case n `Quick f)
          [ "Db.execute performs the repair", execute_performs_the_repair
          ; "the change count is rows deleted", the_change_count_is_rows_deleted
          ; "the query path still repairs and reports", query_still_repairs_and_reports
          ; "not_null_check stays a read", not_null_check_stays_on_the_read_path
          ; "the repair is transactional", the_repair_is_transactional
          ] )
    ; ( "read-only snapshot"
      , List.map
          (fun (n, f) -> Alcotest.test_case n `Quick f)
          [ ( "the repair is refused at the statement level"
            , a_read_only_snapshot_is_refused_at_the_statement_level )
          ; "the report is still served", a_read_only_snapshot_still_serves_the_report
          ] )
    ]
;;
