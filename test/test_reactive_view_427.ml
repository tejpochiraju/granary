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
    - [REFRESH DELTA] on an unmaintainable shape is rejected at CREATE. *)

module Db = Granary.Db

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
    Db.register_view_callback db ~view_name:"cnt" (fun changes ->
      fired := changes :: !fired;
      Lwt.return_unit);
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
    ; "property", [ QCheck_alcotest.to_alcotest prop_views_track ]
    ]
;;
