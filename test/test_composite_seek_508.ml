(** #508: an equality conjunction covering the leading columns of a
    multi-column index must SEEK that index, not full-scan the table.

    The load-bearing assertion is [rows_examined], not wall-clock: a seek
    examines only the rows in the matched prefix range, while the pre-fix
    behaviour examined every row in the table.  [used_index] pins which access
    path the planner chose.

    A QCheck property closes the correctness half: for random composite keys the
    seek path must return exactly what an unoptimizable foil
    ([w + 0 = ? AND i + 0 = ?], which the planner cannot match to an index)
    returns. *)

module Db = Granary.Db

let run = Lwt_main.run

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "db error: %a" Db.pp_error e
;;

let unwrap_open r = Lwt.map unwrap r

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

(* Drain [sql] via [query_with_stats] and return (rows, stats). *)
let stats_of db sql =
  unwrap
    (run
       (let open Lwt.Syntax in
        let* r = Db.query_with_stats db sql in
        match r with
        | Error e -> Lwt.return (Error e)
        | Ok (stream, stats) ->
          let* rows = Lwt_stream.to_list stream in
          Lwt.return (Ok (rows, stats))))
;;

let check_access sql ~examined ~returned ~used_index (rows, st) =
  Alcotest.(check int) (sql ^ " : rows_returned") returned (List.length rows);
  Alcotest.(check int) (sql ^ " : rows_examined") examined st.Granary.Db.rows_examined;
  Alcotest.(check bool) (sql ^ " : used_index") used_index st.Granary.Db.used_index
;;

(* [stock(w, i, q)] keyed by the composite PRIMARY KEY (w, i): [n_w] warehouses
   x [n_i] items, with [q = w * 1000 + i]. *)
let seed_stock db ~n_w ~n_i =
  exec db "CREATE TABLE stock (w INTEGER, i INTEGER, q INTEGER, PRIMARY KEY (w, i))";
  exec db "BEGIN";
  for w = 1 to n_w do
    for i = 1 to n_i do
      exec
        db
        (Printf.sprintf "INSERT INTO stock VALUES (%d, %d, %d)" w i ((w * 1000) + i))
    done
  done;
  exec db "COMMIT"
;;

(* ------------------------------------------------------------------ *)
(* SELECT access paths                                                  *)
(* ------------------------------------------------------------------ *)

(* The whole point of #508: both PK columns pinned by equality must reach the
   row through the implicit __pk index, examining one row rather than 200. *)
let full_key_equality_seeks () =
  with_db (fun db ->
    seed_stock db ~n_w:2 ~n_i:100;
    let res = stats_of db "SELECT q FROM stock WHERE w = 1 AND i = 50" in
    check_access "full key" ~examined:1 ~returned:1 ~used_index:true res;
    Alcotest.(check (list int))
      "value"
      [ 1050 ]
      (List.map
         (function
           | [| Db.V_int n |] -> Int64.to_int n
           | _ -> Alcotest.fail "shape")
         (fst res)))
;;

(* A leading-column-only equality is still a seekable prefix: it must scan the
   index range for w = 1, not the whole table. *)
let leading_prefix_equality_seeks () =
  with_db (fun db ->
    seed_stock db ~n_w:2 ~n_i:100;
    check_access
      "leading prefix"
      ~examined:100
      ~returned:100
      ~used_index:true
      (stats_of db "SELECT q FROM stock WHERE w = 1"))
;;

(* An equality on a NON-leading index column alone is not a seekable prefix:
   the planner must fall back to a filtered scan rather than seek wrongly. *)
let non_leading_equality_falls_back_to_scan () =
  with_db (fun db ->
    seed_stock db ~n_w:2 ~n_i:100;
    check_access
      "non-leading only"
      ~examined:200
      ~returned:2
      ~used_index:false
      (stats_of db "SELECT q FROM stock WHERE i = 50"))
;;

(* Conjuncts beyond the index columns must survive as a residual filter over the
   seek, not be dropped. *)
let extra_conjunct_filters_over_seek () =
  with_db (fun db ->
    seed_stock db ~n_w:2 ~n_i:100;
    check_access
      "residual matches"
      ~examined:1
      ~returned:1
      ~used_index:true
      (stats_of db "SELECT q FROM stock WHERE w = 1 AND i = 50 AND q = 1050");
    check_access
      "residual rejects"
      ~examined:1
      ~returned:0
      ~used_index:true
      (stats_of db "SELECT q FROM stock WHERE w = 1 AND i = 50 AND q = 999999"))
;;

(* Two equalities on the SAME column: only one can be consumed by the seek; the
   other must remain as a filter, so a contradiction returns nothing. *)
let contradictory_equalities_on_one_column_return_nothing () =
  with_db (fun db ->
    seed_stock db ~n_w:2 ~n_i:100;
    Alcotest.(check int)
      "w = 1 AND w = 2"
      0
      (List.length (rows_of db "SELECT q FROM stock WHERE w = 1 AND w = 2"));
    Alcotest.(check int)
      "w = 1 AND i = 50 AND i = 51"
      0
      (List.length (rows_of db "SELECT q FROM stock WHERE w = 1 AND i = 50 AND i = 51")))
;;

(* [col = NULL] never matches, whichever position it takes in the key. *)
let null_in_key_matches_nothing () =
  with_db (fun db ->
    seed_stock db ~n_w:2 ~n_i:100;
    Alcotest.(check int)
      "trailing NULL"
      0
      (List.length (rows_of db "SELECT q FROM stock WHERE w = 1 AND i = NULL"));
    Alcotest.(check int)
      "leading NULL"
      0
      (List.length (rows_of db "SELECT q FROM stock WHERE w = NULL AND i = 50")))
;;

(* A three-column index exercises a two-column prefix seek with the third
   column left to the residual filter. *)
let three_column_index_prefix_seeks () =
  with_db (fun db ->
    exec db "CREATE TABLE t (a INTEGER, b INTEGER, c INTEGER, v INTEGER)";
    exec db "CREATE INDEX idx_abc ON t (a, b, c)";
    exec db "BEGIN";
    for a = 1 to 2 do
      for b = 1 to 5 do
        for c = 1 to 5 do
          exec
            db
            (Printf.sprintf
               "INSERT INTO t VALUES (%d, %d, %d, %d)"
               a
               b
               c
               ((a * 100) + (b * 10) + c))
        done
      done
    done;
    exec db "COMMIT";
    check_access
      "a,b prefix"
      ~examined:5
      ~returned:5
      ~used_index:true
      (stats_of db "SELECT v FROM t WHERE a = 2 AND b = 3");
    check_access
      "a,b,c full"
      ~examined:1
      ~returned:1
      ~used_index:true
      (stats_of db "SELECT v FROM t WHERE a = 2 AND b = 3 AND c = 4"))
;;

(* Text and mixed-type composite keys must seek too — the encoded prefix is
   type-tagged, so a text key column is no different from an integer one. *)
let text_composite_key_seeks () =
  with_db (fun db ->
    exec db "CREATE TABLE p (k TEXT, n INTEGER, v INTEGER, PRIMARY KEY (k, n))";
    exec db "BEGIN";
    for i = 1 to 50 do
      exec db (Printf.sprintf "INSERT INTO p VALUES ('k%d', %d, %d)" (i mod 5) i i)
    done;
    exec db "COMMIT";
    check_access
      "text+int key"
      ~examined:1
      ~returned:1
      ~used_index:true
      (stats_of db "SELECT v FROM p WHERE k = 'k3' AND n = 13"))
;;

(* ------------------------------------------------------------------ *)
(* UPDATE / DELETE access paths                                         *)
(* ------------------------------------------------------------------ *)

(* [query_with_stats] covers reads only, so the write path is measured by its
   physical I/O: count the page/WAL reads a statement provokes, over a
   file-backed DB big enough that a full drain must touch many pages. *)
let with_file_db f =
  let dir = Filename.temp_file "t508-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "db" in
  let db = run (unwrap_open (Granary_unix.open_file_wal ~path ())) in
  Fun.protect
    ~finally:(fun () ->
      run (Db.close db);
      List.iter
        (fun s ->
           try Sys.remove (path ^ s) with
           | _ -> ())
        [ ""; "-wal" ];
      try Unix.rmdir dir with
      | _ -> ())
    (fun () -> f db)
;;

(* Run [sql], returning how many page/WAL reads it caused. *)
let reads_during db sql =
  let n = ref 0 in
  Db.set_event_callback
    db
    (Some
       (function
         | Db.Event.Page_read _ | Db.Event.Wal_read _ -> incr n
         | _ -> ()));
  exec db sql;
  Db.set_event_callback db None;
  !n
;;

let q_at db ~w ~i =
  match rows_of db (Printf.sprintf "SELECT q FROM stock WHERE w = %d AND i = %d" w i) with
  | [ [| Db.V_int n |] ] -> Int64.to_int n
  | _ -> Alcotest.failf "expected exactly one q at (%d,%d)" w i
;;

(* #508's write half: the UPDATE's WHERE clause must narrow through the index
   rather than draining the table.  The [+ 0] foil is the same UPDATE the
   planner cannot seek, run against the same data — it both calibrates the
   comparison and proves the measurement is live (a cache that served
   everything would leave the foil near zero too). *)
let update_by_full_key_seeks () =
  with_file_db (fun db ->
    seed_stock db ~n_w:1 ~n_i:20000;
    let scan_reads =
      reads_during db "UPDATE stock SET q = q + 1 WHERE w + 0 = 1 AND i + 0 = 10000"
    in
    let seek_reads =
      reads_during db "UPDATE stock SET q = q + 1 WHERE w = 1 AND i = 10000"
    in
    Alcotest.(check bool)
      (Printf.sprintf "foil actually reads pages (got %d)" scan_reads)
      true
      (scan_reads > 100);
    Alcotest.(check bool)
      (Printf.sprintf
         "seek reads far fewer pages (seek %d vs scan %d)"
         seek_reads
         scan_reads)
      true
      (seek_reads * 5 < scan_reads);
    (* Both UPDATEs hit exactly the one row, and no other. *)
    Alcotest.(check int) "target updated twice" 11002 (q_at db ~w:1 ~i:10000);
    Alcotest.(check int) "neighbour untouched" 10999 (q_at db ~w:1 ~i:9999))
;;

let delete_by_full_key_seeks () =
  with_file_db (fun db ->
    seed_stock db ~n_w:1 ~n_i:20000;
    let seek_reads = reads_during db "DELETE FROM stock WHERE w = 1 AND i = 10000" in
    let scan_reads =
      reads_during db "DELETE FROM stock WHERE w + 0 = 1 AND i + 0 = 9999"
    in
    Alcotest.(check bool)
      (Printf.sprintf "foil actually reads pages (got %d)" scan_reads)
      true
      (scan_reads > 100);
    Alcotest.(check bool)
      (Printf.sprintf
         "seek reads far fewer pages (seek %d vs scan %d)"
         seek_reads
         scan_reads)
      true
      (seek_reads * 5 < scan_reads);
    Alcotest.(check int)
      "exactly the two rows are gone"
      19998
      (match rows_of db "SELECT COUNT(*) FROM stock" with
       | [ [| Db.V_int n |] ] -> Int64.to_int n
       | _ -> Alcotest.fail "count shape"))
;;

(* A residual conjunct must still gate the write: seeking to the row does not
   license updating it when a non-key predicate rejects it. *)
let update_residual_conjunct_still_gates () =
  with_db (fun db ->
    seed_stock db ~n_w:2 ~n_i:100;
    exec db "UPDATE stock SET q = 0 WHERE w = 1 AND i = 50 AND q = 999999";
    Alcotest.(check int) "rejected by residual" 1050 (q_at db ~w:1 ~i:50);
    exec db "UPDATE stock SET q = 0 WHERE w = 1 AND i = 50 AND q = 1050";
    Alcotest.(check int) "accepted by residual" 0 (q_at db ~w:1 ~i:50))
;;

(* ------------------------------------------------------------------ *)
(* Correctness property: seek path == unoptimizable foil                *)
(* ------------------------------------------------------------------ *)

(* [w + 0 = ?] is semantically identical to [w = ?] but the planner cannot match
   it to an index column, so it always takes the filtered-scan path.  Any
   divergence is a seek bug. *)
let prop_seek_matches_scan =
  QCheck.Test.make
    ~count:200
    ~name:"composite seek agrees with unoptimizable scan"
    QCheck.(pair (int_range 0 4) (int_range 0 12))
    (fun (w, i) ->
       with_db (fun db ->
         seed_stock db ~n_w:3 ~n_i:10;
         let seek =
           rows_of db (Printf.sprintf "SELECT q FROM stock WHERE w = %d AND i = %d" w i)
         in
         let foil =
           rows_of
             db
             (Printf.sprintf "SELECT q FROM stock WHERE w + 0 = %d AND i + 0 = %d" w i)
         in
         List.map Array.to_list seek = List.map Array.to_list foil))
;;

let () =
  Alcotest.run
    "composite_seek_508"
    [ ( "select"
      , [ Alcotest.test_case "full key equality seeks" `Quick full_key_equality_seeks
        ; Alcotest.test_case
            "leading prefix equality seeks"
            `Quick
            leading_prefix_equality_seeks
        ; Alcotest.test_case
            "non-leading equality falls back to scan"
            `Quick
            non_leading_equality_falls_back_to_scan
        ; Alcotest.test_case
            "extra conjunct filters over seek"
            `Quick
            extra_conjunct_filters_over_seek
        ; Alcotest.test_case
            "contradictory equalities return nothing"
            `Quick
            contradictory_equalities_on_one_column_return_nothing
        ; Alcotest.test_case
            "NULL in key matches nothing"
            `Quick
            null_in_key_matches_nothing
        ; Alcotest.test_case
            "three-column index prefix seeks"
            `Quick
            three_column_index_prefix_seeks
        ; Alcotest.test_case "text composite key seeks" `Quick text_composite_key_seeks
        ] )
    ; ( "write"
      , [ Alcotest.test_case "UPDATE by full key seeks" `Quick update_by_full_key_seeks
        ; Alcotest.test_case "DELETE by full key seeks" `Quick delete_by_full_key_seeks
        ; Alcotest.test_case
            "UPDATE residual conjunct still gates"
            `Quick
            update_residual_conjunct_still_gates
        ] )
    ; "property", List.map QCheck_alcotest.to_alcotest [ prop_seek_matches_scan ]
    ]
;;
