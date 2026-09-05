(** #416: the RO tree-root memo must never serve a root from a different
    committed state, and fusing [Op_project] into a rowid point lookup must
    project exactly what the wrapper it replaced would have.

    Two perf changes are pinned here, both of which fail LOUDLY (wrong rows,
    not a slow query) if they are wrong:

    - [Store.bt_get_tree_ro] memoizes [tree_id -> root page] on the store,
      tagged with the committed generation it describes (the header [txn_id]
      paired with the meta root page).  Every commit rewrites the meta tree
      copy-on-write, so a tree's root page moves; a memo that outlived its
      generation would hand a fresh snapshot the PREVIOUS root and the reader
      would silently see the database as it was one commit ago.  The tests
      below drive the generation forwards (commits, DDL), backwards (as-of
      reads at a pinned historical txn), and alternately, which is the case a
      single-generation memo has to reset in both directions.

    - [Exec.to_stream] fuses an [Op_project] whose child is an
      [Op_rowid_lookup] into the lookup itself, applying [project_row] to the
      at-most-one row rather than wrapping the stream in [Lwt_stream.map].
      The tests cover a narrowing projection, a reordering one, the whole row,
      and a lookup that matches nothing. *)

module D = struct
  include Granary.Db

  let open_file = Granary_unix.open_file
end

module H = Granary_store.History
open Lwt.Syntax

let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Filename.concat
    (Filename.get_temp_dir_name ())
    (Printf.sprintf "granary_ro_root_memo_416_%04d.db" n)
;;

let cleanup path =
  List.iter
    (fun suffix ->
       try Unix.unlink (path ^ suffix) with
       | _ -> ())
    [ ""; "-wal"; "-shm"; ".aslog" ]
;;

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected error: %a" D.pp_error e
;;

let exec db sql =
  let* r = D.execute db sql in
  ignore (ok r);
  Lwt.return_unit
;;

let rows_of db sql =
  let* s = D.query db sql in
  Lwt_stream.to_list (ok s)
;;

let texts rows =
  List.sort
    compare
    (List.map
       (fun (row : D.row) ->
          match row.(0) with
          | D.V_text s -> s
          | _ -> Alcotest.fail "expected TEXT in column 0")
       rows)
;;

let with_db ~as_of_history f =
  let path = fresh_path () in
  cleanup path;
  Lwt.finalize
    (fun () ->
       let* db = D.open_file ~as_of_history ~path () in
       let db = ok db in
       Lwt.finalize
         (fun () -> f db)
         (fun () -> Lwt.catch (fun () -> D.close db) (fun _ -> Lwt.return_unit)))
    (fun () ->
       cleanup path;
       Lwt.return_unit)
  |> run
;;

(* Each commit rewrites the meta tree copy-on-write, so the data tree's root
   page moves.  A memo that survived its generation would answer the second
   read with the first read's root and lose every row written in between. *)
let a_commit_between_two_reads_is_visible () =
  with_db ~as_of_history:false (fun db ->
    let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)" in
    let* () = exec db "INSERT INTO t VALUES (1, 'a')" in
    let* r1 = rows_of db "SELECT v FROM t" in
    let* () = exec db "INSERT INTO t VALUES (2, 'b')" in
    let* r2 = rows_of db "SELECT v FROM t" in
    let* () = exec db "INSERT INTO t VALUES (3, 'c')" in
    let* r3 = rows_of db "SELECT v FROM t" in
    Alcotest.(check (list string)) "after first insert" [ "a" ] (texts r1);
    Alcotest.(check (list string)) "after second insert" [ "a"; "b" ] (texts r2);
    Alcotest.(check (list string)) "after third insert" [ "a"; "b"; "c" ] (texts r3);
    Lwt.return_unit)
;;

(* The same, reached through the point-lookup path the memo was added for:
   [Op_rowid_lookup] resolves the tree through [bt_get_tree_ro] on every
   execution, so a stale root shows up as a row that was just written being
   invisible, or as a stale value for a row that was just updated. *)
let a_point_lookup_sees_the_latest_commit () =
  with_db ~as_of_history:false (fun db ->
    let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)" in
    let* () = exec db "INSERT INTO t VALUES (1, 'a')" in
    let* r0 = rows_of db "SELECT v FROM t WHERE id = 1" in
    let* () = exec db "UPDATE t SET v = 'a2' WHERE id = 1" in
    let* r1 = rows_of db "SELECT v FROM t WHERE id = 1" in
    let* () = exec db "INSERT INTO t VALUES (2, 'b')" in
    let* r2 = rows_of db "SELECT v FROM t WHERE id = 2" in
    let* () = exec db "DELETE FROM t WHERE id = 1" in
    let* r3 = rows_of db "SELECT v FROM t WHERE id = 1" in
    Alcotest.(check (list string)) "initial" [ "a" ] (texts r0);
    Alcotest.(check (list string)) "after update" [ "a2" ] (texts r1);
    Alcotest.(check (list string)) "row written after the first read" [ "b" ] (texts r2);
    Alcotest.(check (list string)) "after delete" [] (texts r3);
    Lwt.return_unit)
;;

(* Two tables, so the memo holds more than one entry and a reset has to drop
   both.  DDL between the reads moves the meta root as well. *)
let ddl_between_reads_does_not_strand_a_root () =
  with_db ~as_of_history:false (fun db ->
    let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)" in
    let* () = exec db "CREATE TABLE u (id INTEGER PRIMARY KEY, v TEXT)" in
    let* () = exec db "INSERT INTO t VALUES (1, 'a')" in
    let* () = exec db "INSERT INTO u VALUES (1, 'x')" in
    let* rt0 = rows_of db "SELECT v FROM t WHERE id = 1" in
    let* ru0 = rows_of db "SELECT v FROM u WHERE id = 1" in
    (* A third table plus an index: more meta churn, and a fresh tree that the
       memo has never seen. *)
    let* () = exec db "CREATE TABLE w (id INTEGER PRIMARY KEY, v TEXT)" in
    let* () = exec db "CREATE INDEX iu ON u (v)" in
    let* () = exec db "INSERT INTO w VALUES (1, 'q')" in
    let* () = exec db "INSERT INTO t VALUES (2, 'b')" in
    let* () = exec db "INSERT INTO u VALUES (2, 'y')" in
    let* rt1 = rows_of db "SELECT v FROM t" in
    let* ru1 = rows_of db "SELECT v FROM u" in
    let* rw1 = rows_of db "SELECT v FROM w WHERE id = 1" in
    let* ru2 = rows_of db "SELECT v FROM u WHERE v = 'y'" in
    Alcotest.(check (list string)) "t before" [ "a" ] (texts rt0);
    Alcotest.(check (list string)) "u before" [ "x" ] (texts ru0);
    Alcotest.(check (list string)) "t after" [ "a"; "b" ] (texts rt1);
    Alcotest.(check (list string)) "u after" [ "x"; "y" ] (texts ru1);
    Alcotest.(check (list string)) "new table" [ "q" ] (texts rw1);
    Alcotest.(check (list string)) "new index lookup" [ "y" ] (texts ru2);
    Alcotest.(check (list string)) "u after" [ "x"; "y" ] (texts ru1);
    Lwt.return_unit)
;;

(* A rolled-back transaction leaves the header txn id and the meta root
   untouched (#382) while reverting the meta tree to that root, so the memo's
   generation is unchanged and its entries stay correct.  Pinned because the
   OPPOSITE failure — a bumped-then-reverted root cached under the unchanged
   generation — would make the rolled-back rows visible. *)
let a_rollback_leaves_no_stale_root () =
  with_db ~as_of_history:false (fun db ->
    let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)" in
    let* () = exec db "INSERT INTO t VALUES (1, 'a')" in
    let* r0 = rows_of db "SELECT v FROM t WHERE id = 1" in
    let* () = exec db "BEGIN" in
    let* () = exec db "INSERT INTO t VALUES (2, 'b')" in
    (* Read-your-own-writes inside the transaction (#262). *)
    let* rin = rows_of db "SELECT v FROM t" in
    let* () = exec db "ROLLBACK" in
    let* r1 = rows_of db "SELECT v FROM t" in
    let* r2 = rows_of db "SELECT v FROM t WHERE id = 2" in
    (* And the tree still works for a fresh commit afterwards. *)
    let* () = exec db "INSERT INTO t VALUES (3, 'c')" in
    let* r3 = rows_of db "SELECT v FROM t" in
    Alcotest.(check (list string)) "before" [ "a" ] (texts r0);
    Alcotest.(check (list string)) "inside the txn" [ "a"; "b" ] (texts rin);
    Alcotest.(check (list string)) "after rollback" [ "a" ] (texts r1);
    Alcotest.(check (list string)) "rolled-back point lookup" [] (texts r2);
    Alcotest.(check (list string)) "after a later commit" [ "a"; "c" ] (texts r3);
    Lwt.return_unit)
;;

(* The generation has to reset in BOTH directions.  An as-of read resolves its
   snapshot from a history record, so it carries an OLDER (txn_id, meta root)
   than the live state; alternating the two is what a single-generation memo
   has to keep re-deriving.  A memo keyed on the txn id alone, or one that
   never reset backwards, answers the as-of read with live rows. *)
let as_of_and_live_reads_alternate_correctly () =
  with_db ~as_of_history:true (fun db ->
    let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)" in
    let* () = exec db "INSERT INTO t VALUES (1, 'a')" in
    let* log = D.history_log db in
    let t1 =
      match List.rev log with
      | last :: _ -> last.H.txn_id
      | [] -> Alcotest.fail "history log is empty after a commit"
    in
    D.history_pin db ~txn_id:t1;
    let* () = exec db "INSERT INTO t VALUES (2, 'b')" in
    let* () = exec db "INSERT INTO t VALUES (3, 'c')" in
    let as_of () =
      let* s = D.query_as_of db (`Txn t1) "SELECT v FROM t" in
      Lwt_stream.to_list (ok s)
    in
    let as_of_point () =
      let* s = D.query_as_of db (`Txn t1) "SELECT v FROM t WHERE id = 1" in
      Lwt_stream.to_list (ok s)
    in
    (* Alternate several times: each transition must reset the memo. *)
    let* h1 = as_of () in
    let* l1 = rows_of db "SELECT v FROM t" in
    let* h2 = as_of () in
    let* l2 = rows_of db "SELECT v FROM t" in
    let* hp = as_of_point () in
    let* lp = rows_of db "SELECT v FROM t WHERE id = 3" in
    let* h3 = as_of () in
    Alcotest.(check (list string)) "as-of #1" [ "a" ] (texts h1);
    Alcotest.(check (list string)) "live #1" [ "a"; "b"; "c" ] (texts l1);
    Alcotest.(check (list string)) "as-of #2" [ "a" ] (texts h2);
    Alcotest.(check (list string)) "live #2" [ "a"; "b"; "c" ] (texts l2);
    Alcotest.(check (list string)) "as-of point lookup" [ "a" ] (texts hp);
    Alcotest.(check (list string)) "live point lookup" [ "c" ] (texts lp);
    Alcotest.(check (list string)) "as-of #3" [ "a" ] (texts h3);
    Lwt.return_unit)
;;

(* The fused projection: what comes back must be the columns the SELECT names,
   in the order it names them — the wrapper it replaced applied exactly
   [project_row ordinals] to the single row. *)
let fused_projection_selects_the_right_columns () =
  with_db ~as_of_history:false (fun db ->
    let* () = exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, a TEXT, b TEXT, c TEXT)" in
    let* () = exec db "INSERT INTO t VALUES (1, 'A', 'B', 'C')" in
    let* () = exec db "INSERT INTO t VALUES (2, 'D', 'E', 'F')" in
    let cells sql =
      let* s = D.query db sql in
      let* rows = Lwt_stream.to_list (ok s) in
      Lwt.return
        (List.map
           (fun (row : D.row) ->
              Array.to_list
                (Array.map
                   (function
                     | D.V_text s -> s
                     | D.V_int n -> Int64.to_string n
                     | _ -> "?")
                   row))
           rows)
    in
    let* one = cells "SELECT b FROM t WHERE id = 2" in
    let* reordered = cells "SELECT c, a FROM t WHERE id = 1" in
    let* whole = cells "SELECT id, a, b, c FROM t WHERE id = 1" in
    let* star = cells "SELECT * FROM t WHERE id = 2" in
    let* miss = cells "SELECT a FROM t WHERE id = 99" in
    let* dup = cells "SELECT a, a FROM t WHERE id = 1" in
    Alcotest.(check (list (list string))) "single column" [ [ "E" ] ] one;
    Alcotest.(check (list (list string))) "reordered" [ [ "C"; "A" ] ] reordered;
    Alcotest.(check (list (list string))) "whole row" [ [ "1"; "A"; "B"; "C" ] ] whole;
    Alcotest.(check (list (list string))) "star" [ [ "2"; "D"; "E"; "F" ] ] star;
    Alcotest.(check (list (list string))) "no match" [] miss;
    Alcotest.(check (list (list string))) "repeated column" [ [ "A"; "A" ] ] dup;
    Lwt.return_unit)
;;

let () =
  Granary_unix.install ();
  Alcotest.run
    "ro_root_memo_416"
    [ ( "ro_root_memo"
      , [ ( "a commit between two reads is visible"
          , `Quick
          , a_commit_between_two_reads_is_visible )
        ; ( "a point lookup sees the latest commit"
          , `Quick
          , a_point_lookup_sees_the_latest_commit )
        ; ( "DDL between reads does not strand a root"
          , `Quick
          , ddl_between_reads_does_not_strand_a_root )
        ; "a rollback leaves no stale root", `Quick, a_rollback_leaves_no_stale_root
        ; ( "as-of and live reads alternate correctly"
          , `Quick
          , as_of_and_live_reads_alternate_correctly )
        ] )
    ; ( "fused_projection"
      , [ ( "fused projection selects the right columns"
          , `Quick
          , fused_projection_selects_the_right_columns )
        ] )
    ]
;;
