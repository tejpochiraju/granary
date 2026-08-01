(** #514 (second half): the DML index-seek path must not materialise every
    candidate rowid before fetching any of them.

    [Exec.seek_candidate_rowids] used to walk the whole index range into an
    [int64 list], sort it, and only then let [drain_matching_rows_in_tx] fetch
    rows — so peak memory was proportional to the match count, and a
    [DELETE FROM t WHERE tenant = 1] over a large table held the entire
    candidate set at once.  The walk now hands each rowid to the fetch as it is
    decoded, holding one at a time, and the drain sorts its accumulated matches
    by rowid (which the DML path relies on to keep an unordered
    [UPDATE/DELETE … LIMIT n] hitting the same rows a scan would).

    The memory assertion rides {!Granary_sql.Exec.dml_seek_stats}, whose
    [dss_peak_buffered] is the high-water mark of candidates walked but not yet
    fetched: it must stay at 1 however many rows the statement affects, which is
    the same thing as saying rows are fetched interleaved with the seek rather
    than all after it.  Correctness is the bigger half of this file:
    mutation-during-iteration cases (including an UPDATE of the very column
    being seeked on), rollback, and QCheck properties against an unoptimizable
    foil predicate.

    The selectivity guard — the {e other} half of #514 — is deliberately not
    addressed here and #514 stays open for it. *)

module Db = Granary.Db
module Exec = Granary_sql.Exec

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

(* Run [sql] with the DML seek counters installed. *)
let exec_stats db sql =
  let st = Exec.make_dml_seek_stats () in
  (match run (Exec.with_dml_seek_stats st (fun () -> Db.execute db sql)) with
   | Ok () -> ()
   | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
  st
;;

let rows_of db sql =
  unwrap
    (run
       (let open Lwt.Syntax in
        let* r = Db.query db sql in
        match r with
        | Error e -> Lwt.return (Error e)
        | Ok stream ->
          let* rows = Lwt_stream.to_list stream in
          Lwt.return (Ok rows)))
;;

let ints db sql =
  List.map
    (function
      | [| Db.V_int n |] -> Int64.to_int n
      | _ -> Alcotest.fail "expected a single-int row")
    (rows_of db sql)
;;

let one_int db sql =
  match ints db sql with
  | [ n ] -> n
  | l -> Alcotest.failf "expected one row, got %d" (List.length l)
;;

(* [t(w, i, v)] with composite PRIMARY KEY (w, i): the #508 seek shape.  Rows
   are inserted with [i] DESCENDING inside each [w], so index-key order and
   rowid order disagree — which is what makes the drain-order guarantee worth
   pinning. *)
let seed db ~n_w ~n_i =
  exec db "CREATE TABLE t (w INTEGER, i INTEGER, v INTEGER, PRIMARY KEY (w, i))";
  exec db "BEGIN";
  for w = 1 to n_w do
    for i = n_i downto 1 do
      exec db (Printf.sprintf "INSERT INTO t VALUES (%d, %d, %d)" w i ((w * 100000) + i))
    done
  done;
  exec db "COMMIT"
;;

(* ------------------------------------------------------------------ *)
(* The memory bound                                                     *)
(* ------------------------------------------------------------------ *)

(* RED before the fix: the seek buffered all 3000 candidates before fetching
   any, so [dss_peak_buffered] was 3000 rather than 1. *)
let many = 3000

(* A large-but-not-round row count for the correctness cases below, chosen well
   above any plausible internal batch so a batching regression shows up. *)
let large = 1224

let delete_streams_candidates () =
  with_db (fun db ->
    seed db ~n_w:2 ~n_i:many;
    let st = exec_stats db "DELETE FROM t WHERE w = 1" in
    Alcotest.(check int) "candidates walked" many st.Exec.dss_candidates;
    Alcotest.(check int) "rows fetched" many st.Exec.dss_fetched;
    Alcotest.(check int) "peak candidate backlog" 1 st.Exec.dss_peak_buffered;
    Alcotest.(check int)
      "deleted all of w = 1"
      0
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 1");
    Alcotest.(check int)
      "left w = 2 alone"
      many
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 2"))
;;

let update_streams_candidates () =
  with_db (fun db ->
    seed db ~n_w:2 ~n_i:many;
    let st = exec_stats db "UPDATE t SET v = 0 WHERE w = 2" in
    Alcotest.(check int) "candidates walked" many st.Exec.dss_candidates;
    Alcotest.(check int) "peak candidate backlog" 1 st.Exec.dss_peak_buffered;
    Alcotest.(check int)
      "all of w = 2 updated"
      many
      (one_int db "SELECT COUNT(*) FROM t WHERE v = 0");
    Alcotest.(check int)
      "w = 1 untouched"
      0
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 1 AND v = 0"))
;;

(* A single-row match walks and fetches exactly one candidate — the #512 shape,
   in counter form: the seek must not have degraded into a table scan. *)
let single_row_match_buffers_one () =
  with_db (fun db ->
    seed db ~n_w:2 ~n_i:50;
    let st = exec_stats db "DELETE FROM t WHERE w = 1 AND i = 25" in
    Alcotest.(check int) "one candidate" 1 st.Exec.dss_candidates;
    Alcotest.(check int) "one fetch" 1 st.Exec.dss_fetched;
    Alcotest.(check int) "peak buffered" 1 st.Exec.dss_peak_buffered;
    Alcotest.(check int) "99 rows left" 99 (one_int db "SELECT COUNT(*) FROM t"))
;;

let zero_row_match_buffers_none () =
  with_db (fun db ->
    seed db ~n_w:2 ~n_i:50;
    let st = exec_stats db "DELETE FROM t WHERE w = 1 AND i = 999" in
    Alcotest.(check int) "no candidates" 0 st.Exec.dss_candidates;
    Alcotest.(check int) "no fetches" 0 st.Exec.dss_fetched;
    Alcotest.(check int) "nothing buffered" 0 st.Exec.dss_peak_buffered;
    Alcotest.(check int) "100 rows left" 100 (one_int db "SELECT COUNT(*) FROM t");
    (* Same for a prefix that matches no index entry at all. *)
    let st = exec_stats db "UPDATE t SET v = 1 WHERE w = 77" in
    Alcotest.(check int) "no candidates" 0 st.Exec.dss_candidates;
    Alcotest.(check int)
      "nothing updated"
      0
      (one_int db "SELECT COUNT(*) FROM t WHERE v = 1"))
;;

(* ------------------------------------------------------------------ *)
(* Mutation during iteration                                            *)
(* ------------------------------------------------------------------ *)

(* The nastiest case: UPDATE the very column the seek walks.  The old row's
   index entry is removed and a new one inserted at a DIFFERENT key, so a naive
   "mutate as you walk" implementation would either revisit the moved row (it
   lands further along the same range) or skip its neighbour.

   Shifting [i] by +[n_i] inside one [w] keeps every new key inside the walked
   prefix range and strictly after the old one — precisely the shape that
   revisits a row.  Each row must be updated exactly once, so the final [i]
   values are the originals plus the offset, with no doubles. *)
let update_seeked_column_visits_each_row_once () =
  with_db (fun db ->
    let n = 40 in
    seed db ~n_w:2 ~n_i:n;
    exec db (Printf.sprintf "UPDATE t SET i = i + %d WHERE w = 1" n);
    Alcotest.(check (list int))
      "each row moved exactly once"
      (List.init n (fun k -> k + n + 1))
      (List.sort compare (ints db "SELECT i FROM t WHERE w = 1"));
    Alcotest.(check int)
      "row count unchanged"
      n
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 1");
    Alcotest.(check int)
      "other warehouse untouched"
      n
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 2");
    (* And at scale, where a revisit is far likelier to show up. *)
    exec db "DROP TABLE t";
    let big = large + 200 in
    seed db ~n_w:1 ~n_i:big;
    exec db (Printf.sprintf "UPDATE t SET i = i + %d WHERE w = 1" big);
    Alcotest.(check int)
      "row count unchanged at scale"
      big
      (one_int db "SELECT COUNT(*) FROM t");
    Alcotest.(check int)
      "every row moved exactly once"
      big
      (one_int db (Printf.sprintf "SELECT COUNT(*) FROM t WHERE i > %d" big)))
;;

(* Moving the seeked column BACKWARDS (new key before the cursor) is the mirror
   image: a naive walk would skip rows it has yet to reach. *)
let update_seeked_column_backwards () =
  with_db (fun db ->
    let n = large + 50 in
    seed db ~n_w:2 ~n_i:n;
    (* i in [1..n] becomes i - n in [1-n .. 0], all before the walked range. *)
    exec db (Printf.sprintf "UPDATE t SET i = i - %d WHERE w = 2" n);
    Alcotest.(check int)
      "all rows moved"
      n
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 2 AND i <= 0");
    Alcotest.(check int)
      "none left behind"
      0
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 2 AND i > 0"))
;;

(* An UPDATE that moves rows OUT of the seeked prefix entirely. *)
let update_leading_seek_column () =
  with_db (fun db ->
    let n = large + 10 in
    seed db ~n_w:2 ~n_i:n;
    exec db "UPDATE t SET w = 3 WHERE w = 1";
    Alcotest.(check int) "moved out" 0 (one_int db "SELECT COUNT(*) FROM t WHERE w = 1");
    Alcotest.(check int) "arrived" n (one_int db "SELECT COUNT(*) FROM t WHERE w = 3");
    Alcotest.(check int) "total preserved" (2 * n) (one_int db "SELECT COUNT(*) FROM t"))
;;

(* A seeked DELETE inside an explicit transaction that is rolled back must leave
   nothing behind — the streaming walk runs inside the caller's RW txn, so a
   partially-applied delete would show up here. *)
let delete_in_rolled_back_txn () =
  with_db (fun db ->
    let n = large + 300 in
    seed db ~n_w:2 ~n_i:n;
    exec db "BEGIN";
    exec db "DELETE FROM t WHERE w = 1";
    Alcotest.(check int)
      "gone inside the txn"
      0
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 1");
    exec db "ROLLBACK";
    Alcotest.(check int)
      "all back after ROLLBACK"
      n
      (one_int db "SELECT COUNT(*) FROM t WHERE w = 1");
    Alcotest.(check int) "total intact" (2 * n) (one_int db "SELECT COUNT(*) FROM t");
    (* The same statement committed, for contrast. *)
    exec db "BEGIN";
    exec db "DELETE FROM t WHERE w = 1";
    exec db "COMMIT";
    Alcotest.(check int) "committed delete sticks" n (one_int db "SELECT COUNT(*) FROM t"))
;;

(* Drain order is load-bearing: an unordered [DELETE … LIMIT n] over a prefix
   spanning several full keys must take the same n rows a table scan would, i.e.
   the lowest rowids.  Chunking must not leak index-key order into that (#512
   review).  Rows were inserted with i descending, so rowid order is i = n_i
   downward; a LIMIT 3 must therefore take the three HIGHEST i. *)
let limit_without_order_still_matches_scan_order () =
  with_db (fun db ->
    seed db ~n_w:1 ~n_i:10;
    exec db "DELETE FROM t WHERE w = 1 LIMIT 3";
    Alcotest.(check (list int))
      "the three lowest rowids went"
      [ 1; 2; 3; 4; 5; 6; 7 ]
      (List.sort compare (ints db "SELECT i FROM t")))
;;

(* ------------------------------------------------------------------ *)
(* Property: seeked DML == unoptimizable foil                           *)
(* ------------------------------------------------------------------ *)

(* [w + 0 = ?] is semantically identical to [w = ?] but the planner cannot match
   it to an index column, so it takes the filtered-scan path.  Applying the same
   DML through both spellings, to two identically seeded tables, must leave the
   same rows behind. *)
let survivors db table =
  ints db (Printf.sprintf "SELECT (w * 100000) + i FROM %s ORDER BY w, i" table)
;;

let seed_pair db ~n_w ~n_i =
  List.iter
    (fun name ->
       exec
         db
         (Printf.sprintf
            "CREATE TABLE %s (w INTEGER, i INTEGER, v INTEGER, PRIMARY KEY (w, i))"
            name);
       exec db "BEGIN";
       for w = 1 to n_w do
         for i = n_i downto 1 do
           exec
             db
             (Printf.sprintf
                "INSERT INTO %s VALUES (%d, %d, %d)"
                name
                w
                i
                ((w * 100000) + i))
         done
       done;
       exec db "COMMIT")
    [ "seeked"; "foil" ]
;;

let prop_seeked_delete_matches_foil =
  QCheck.Test.make
    ~count:120
    ~name:"seeked DELETE affects exactly the foil's row set"
    QCheck.(triple (int_range 0 4) (int_range 0 12) bool)
    (fun (w, i, with_i) ->
       with_db (fun db ->
         seed_pair db ~n_w:3 ~n_i:8;
         let pred tbl =
           if with_i
           then Printf.sprintf "DELETE FROM %s WHERE w = %d AND i = %d" tbl w i
           else Printf.sprintf "DELETE FROM %s WHERE w = %d" tbl w
         in
         let foil_pred =
           if with_i
           then Printf.sprintf "DELETE FROM foil WHERE w + 0 = %d AND i + 0 = %d" w i
           else Printf.sprintf "DELETE FROM foil WHERE w + 0 = %d" w
         in
         exec db (pred "seeked");
         exec db foil_pred;
         survivors db "seeked" = survivors db "foil"))
;;

let prop_seeked_update_matches_foil =
  QCheck.Test.make
    ~count:120
    ~name:"seeked UPDATE affects exactly the foil's row set"
    QCheck.(triple (int_range 0 4) (int_range 0 12) (int_range 1 3))
    (fun (w, i, shift) ->
       with_db (fun db ->
         seed_pair db ~n_w:3 ~n_i:8;
         (* Shifting [i] mutates the seeked column itself, so this also fuzzes
            the visit-once property against a path that cannot get it wrong. *)
         exec
           db
           (Printf.sprintf
              "UPDATE seeked SET i = i + %d, v = v + 1 WHERE w = %d AND i >= %d"
              (shift * 100)
              w
              i);
         exec
           db
           (Printf.sprintf
              "UPDATE foil SET i = i + %d, v = v + 1 WHERE w + 0 = %d AND i + 0 >= %d"
              (shift * 100)
              w
              i);
         let vals tbl = ints db (Printf.sprintf "SELECT v FROM %s ORDER BY w, i" tbl) in
         survivors db "seeked" = survivors db "foil" && vals "seeked" = vals "foil"))
;;

let () =
  Alcotest.run
    "bounded_drain_514"
    [ ( "memory bound"
      , [ Alcotest.test_case
            "DELETE streams its candidates"
            `Quick
            delete_streams_candidates
        ; Alcotest.test_case
            "UPDATE streams its candidates"
            `Quick
            update_streams_candidates
        ; Alcotest.test_case
            "single-row match buffers one"
            `Quick
            single_row_match_buffers_one
        ; Alcotest.test_case
            "zero-row match buffers none"
            `Quick
            zero_row_match_buffers_none
        ] )
    ; ( "mutation during iteration"
      , [ Alcotest.test_case
            "UPDATE of the seeked column visits each row once"
            `Quick
            update_seeked_column_visits_each_row_once
        ; Alcotest.test_case
            "UPDATE moving the seeked column backwards"
            `Quick
            update_seeked_column_backwards
        ; Alcotest.test_case
            "UPDATE of the leading seek column"
            `Quick
            update_leading_seek_column
        ; Alcotest.test_case
            "DELETE in a rolled-back transaction"
            `Quick
            delete_in_rolled_back_txn
        ; Alcotest.test_case
            "LIMIT without ORDER BY keeps scan order"
            `Quick
            limit_without_order_still_matches_scan_order
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_seeked_delete_matches_foil; prop_seeked_update_matches_foil ] )
    ]
;;
