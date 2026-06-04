(** #239: per-query cost/stats signal ([rows_examined], [rows_returned],
    [used_index]) exposed via {!Db.query_with_stats}.

    The load-bearing property for an external cost-based cache is that
    [rows_examined] reflects WORK, not output: a full scan behind a selective
    filter reads [rows_examined = N] while returning one row.  These also pin
    the index-vs-scan signal and the count-star fast path. *)

module Db = Sqlocaml.Db

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

(* Drain [sql] via [query_with_stats] and return (#rows, stats). *)
let stats_of db sql =
  unwrap
    (run
       (let open Lwt.Syntax in
        let* r = Db.query_with_stats db sql in
        match r with
        | Error e -> Lwt.return (Error e)
        | Ok (stream, stats) ->
          let* rows = Lwt_stream.to_list stream in
          Lwt.return (Ok (List.length rows, stats))))
;;

let check_stats sql ~examined ~returned ~used_index (n, st) =
  Alcotest.(check int) (sql ^ " : rows_returned (len)") returned n;
  Alcotest.(check int) (sql ^ " : rows_returned") returned st.Sqlocaml.Db.rows_returned;
  Alcotest.(check int) (sql ^ " : rows_examined") examined st.Sqlocaml.Db.rows_examined;
  Alcotest.(check bool) (sql ^ " : used_index") used_index st.Sqlocaml.Db.used_index
;;

(* A 10-row table with a UNIQUE-valued text column [v = 'row<i>']. *)
let seed_10 db =
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)";
  for i = 0 to 9 do
    exec db (Printf.sprintf "INSERT INTO t VALUES (%d, 'row%d')" i i)
  done
;;

(* Full table scan: examined = returned = N, no index. *)
let test_seq_scan_full () =
  with_db (fun db ->
    seed_10 db;
    stats_of db "SELECT * FROM t"
    |> check_stats "SELECT * FROM t" ~examined:10 ~returned:10 ~used_index:false)
;;

(* The canonical signal: a selective filter over a non-indexed column scans the
   whole table (examined = 10) to return one row. *)
let test_filter_full_scan () =
  with_db (fun db ->
    seed_10 db;
    stats_of db "SELECT * FROM t WHERE v = 'row5'"
    |> check_stats
         "SELECT * FROM t WHERE v = 'row5'"
         ~examined:10
         ~returned:1
         ~used_index:false)
;;

(* INTEGER PRIMARY KEY equality = single rowid seek: examined = 1, used_index. *)
let test_rowid_lookup_hit () =
  with_db (fun db ->
    seed_10 db;
    stats_of db "SELECT * FROM t WHERE id = 5"
    |> check_stats "SELECT * FROM t WHERE id = 5" ~examined:1 ~returned:1 ~used_index:true)
;;

(* A miss touches no row but is still an index/rowid access. *)
let test_rowid_lookup_miss () =
  with_db (fun db ->
    seed_10 db;
    stats_of db "SELECT * FROM t WHERE id = 999"
    |> check_stats
         "SELECT * FROM t WHERE id = 999"
         ~examined:0
         ~returned:0
         ~used_index:true)
;;

(* Secondary index equality: examined = #matches via the index, used_index. *)
let test_index_lookup_unique () =
  with_db (fun db ->
    seed_10 db;
    exec db "CREATE INDEX ix_v ON t (v)";
    stats_of db "SELECT * FROM t WHERE v = 'row5'"
    |> check_stats
         "SELECT * FROM t WHERE v = 'row5' (indexed)"
         ~examined:1
         ~returned:1
         ~used_index:true)
;;

(* Index range with duplicate keys: each matching base row is examined. *)
let test_index_lookup_multi () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, g INTEGER)";
    for i = 0 to 9 do
      exec db (Printf.sprintf "INSERT INTO t VALUES (%d, %d)" i (i mod 2))
    done;
    exec db "CREATE INDEX ix_g ON t (g)";
    (* g = 0 matches the five even ids. *)
    stats_of db "SELECT * FROM t WHERE g = 0"
    |> check_stats
         "SELECT * FROM t WHERE g = 0 (indexed, 5 matches)"
         ~examined:5
         ~returned:5
         ~used_index:true)
;;

(* count-star fast path scans every row (examined = 10) and returns one. *)
let test_count_star_fastpath () =
  with_db (fun db ->
    seed_10 db;
    stats_of db "SELECT count(*) FROM t"
    |> check_stats "SELECT count(*) FROM t" ~examined:10 ~returned:1 ~used_index:false)
;;

(* count-star with a non-equality predicate still scans the whole table. *)
let test_count_filtered_fastpath () =
  with_db (fun db ->
    seed_10 db;
    stats_of db "SELECT count(*) FROM t WHERE id > 5"
    |> check_stats
         "SELECT count(*) FROM t WHERE id > 5"
         ~examined:10
         ~returned:1
         ~used_index:false)
;;

(* A fresh record starts zeroed; re-running on the same db does not accumulate
   across queries (each call gets its own record). *)
let test_no_cross_query_accumulation () =
  with_db (fun db ->
    seed_10 db;
    let _ = stats_of db "SELECT * FROM t" in
    stats_of db "SELECT * FROM t WHERE id = 1"
    |> check_stats "second query independent" ~examined:1 ~returned:1 ~used_index:true)
;;

(* A join counts both inputs: the left input's base scan plus the right table's
   rows (index probes for a nested-loop join, or the right scan for a hash join).
   Either plan reads examined = 3 (left) + 3 (right) = 6 for three matches. *)
let test_join_counts_both_sides () =
  with_db (fun db ->
    exec db "CREATE TABLE a (id INTEGER PRIMARY KEY, k INTEGER)";
    exec db "CREATE TABLE b (id INTEGER PRIMARY KEY, k INTEGER)";
    exec db "CREATE INDEX ix_b_k ON b (k)";
    for i = 0 to 2 do
      exec db (Printf.sprintf "INSERT INTO a VALUES (%d, %d)" i i);
      exec db (Printf.sprintf "INSERT INTO b VALUES (%d, %d)" i i)
    done;
    let n, st = stats_of db "SELECT a.id, b.id FROM a JOIN b ON a.k = b.k" in
    Alcotest.(check int) "join rows (len)" 3 n;
    Alcotest.(check int) "join rows_returned" 3 st.Sqlocaml.Db.rows_returned;
    Alcotest.(check int)
      "join rows_examined (left scan + right rows)"
      6
      st.Sqlocaml.Db.rows_examined)
;;

let () =
  Alcotest.run
    "query_stats_239"
    [ ( "stats"
      , [ Alcotest.test_case "seq scan full" `Quick test_seq_scan_full
        ; Alcotest.test_case "filter full scan (work>output)" `Quick test_filter_full_scan
        ; Alcotest.test_case "rowid lookup hit" `Quick test_rowid_lookup_hit
        ; Alcotest.test_case "rowid lookup miss" `Quick test_rowid_lookup_miss
        ; Alcotest.test_case "index lookup unique" `Quick test_index_lookup_unique
        ; Alcotest.test_case "index lookup multi" `Quick test_index_lookup_multi
        ; Alcotest.test_case "count(*) fast path" `Quick test_count_star_fastpath
        ; Alcotest.test_case "count(*) filtered" `Quick test_count_filtered_fastpath
        ; Alcotest.test_case
            "no cross-query accumulation"
            `Quick
            test_no_cross_query_accumulation
        ; Alcotest.test_case "join counts both sides" `Quick test_join_counts_both_sides
        ] )
    ]
;;
