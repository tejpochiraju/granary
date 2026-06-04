(** #257: extend [rows_examined] ({!Db.query_with_stats}) to the two base-read
    paths the #239 first cut missed — FTS scans (plain content scan and MATCH
    index lookup) and rows read inside correlated subqueries.

    As in #239 the load-bearing property is that [rows_examined] reflects WORK:
    an FTS scan behind a filter reads every content row; a correlated subquery
    re-scanned per outer row charges each inner read to the outer query. *)

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

(* Four-document FTS table; 'ocaml' appears in exactly two bodies. *)
let seed_docs db =
  exec db "CREATE VIRTUAL TABLE docs USING FTS5(body)";
  exec db "INSERT INTO docs (body) VALUES ('ocaml rocks')";
  exec db "INSERT INTO docs (body) VALUES ('python rules')";
  exec db "INSERT INTO docs (body) VALUES ('ocaml again')";
  exec db "INSERT INTO docs (body) VALUES ('java stuff')"
;;

(* Plain FTS scan walks every content row: examined = returned = N, no index. *)
let test_fts_seq_scan () =
  with_db (fun db ->
    seed_docs db;
    stats_of db "SELECT * FROM docs"
    |> check_stats "SELECT * FROM docs" ~examined:4 ~returned:4 ~used_index:false)
;;

(* Plain FTS scan behind a content-column filter still reads every content row
   (examined = 4) to return one. *)
let test_fts_seq_scan_filtered () =
  with_db (fun db ->
    seed_docs db;
    stats_of db "SELECT * FROM docs WHERE body = 'java stuff'"
    |> check_stats
         "SELECT * FROM docs WHERE body = 'java stuff'"
         ~examined:4
         ~returned:1
         ~used_index:false)
;;

(* MATCH is an FTS index lookup: only the two matched documents are examined,
   and [used_index] is true. *)
let test_fts_match_scan () =
  with_db (fun db ->
    seed_docs db;
    stats_of db "SELECT * FROM docs WHERE docs MATCH 'ocaml'"
    |> check_stats
         "SELECT * FROM docs WHERE docs MATCH 'ocaml'"
         ~examined:2
         ~returned:2
         ~used_index:true)
;;

(* A correlated subquery in a projected expression is re-scanned once per outer
   row; each inner scan's reads are charged to the outer query.  Outer scans 3
   rows; the inner (no index on [g]) full-scans its 5 rows per outer row, so
   examined = 3 + 3 * 5 = 18. *)
let seed_corr db =
  exec db "CREATE TABLE o (id INTEGER PRIMARY KEY)";
  for i = 0 to 2 do
    exec db (Printf.sprintf "INSERT INTO o VALUES (%d)" i)
  done;
  exec db "CREATE TABLE inr (id INTEGER PRIMARY KEY, g INTEGER)";
  for i = 0 to 4 do
    exec db (Printf.sprintf "INSERT INTO inr VALUES (%d, %d)" i (i mod 3))
  done
;;

let test_correlated_subquery_projection () =
  with_db (fun db ->
    seed_corr db;
    let n, st =
      stats_of db "SELECT id, (SELECT sum(g) FROM inr WHERE g = o.id) AS s FROM o"
    in
    Alcotest.(check int) "corr proj rows" 3 n;
    Alcotest.(check int) "corr proj rows_returned" 3 st.Sqlocaml.Db.rows_returned;
    Alcotest.(check int)
      "corr proj rows_examined (3 outer + 3*5 inner)"
      18
      st.Sqlocaml.Db.rows_examined)
;;

(* A correlated subquery in a WHERE predicate, same accounting: the inner scan
   per outer row is charged to the outer query. *)
let test_correlated_subquery_filter () =
  with_db (fun db ->
    seed_corr db;
    let n, st =
      stats_of db "SELECT id FROM o WHERE (SELECT count(*) FROM inr WHERE g = o.id) > 0"
    in
    (* g in {0,1,2}; every outer id 0..2 has at least one match, so 3 returned. *)
    Alcotest.(check int) "corr filter rows" 3 n;
    Alcotest.(check int)
      "corr filter rows_examined (3 outer + 3*5 inner)"
      18
      st.Sqlocaml.Db.rows_examined)
;;

(* Before #257 the inner scan was invisible: prove the count is strictly greater
   than the outer-only scan, independent of the exact inner total. *)
let test_correlated_charges_inner () =
  with_db (fun db ->
    seed_corr db;
    let _, st =
      stats_of db "SELECT id, (SELECT sum(g) FROM inr WHERE g = o.id) AS s FROM o"
    in
    Alcotest.(check bool)
      "inner reads are charged (examined > 3 outer rows)"
      true
      (st.Sqlocaml.Db.rows_examined > 3))
;;

let () =
  Alcotest.run
    "query_stats_257"
    [ ( "fts"
      , [ Alcotest.test_case "fts seq scan" `Quick test_fts_seq_scan
        ; Alcotest.test_case "fts seq scan filtered" `Quick test_fts_seq_scan_filtered
        ; Alcotest.test_case "fts match scan" `Quick test_fts_match_scan
        ] )
    ; ( "subquery"
      , [ Alcotest.test_case
            "correlated projection"
            `Quick
            test_correlated_subquery_projection
        ; Alcotest.test_case "correlated filter" `Quick test_correlated_subquery_filter
        ; Alcotest.test_case
            "correlated charges inner"
            `Quick
            test_correlated_charges_inner
        ] )
    ]
;;
