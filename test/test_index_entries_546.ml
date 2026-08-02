(** #546 (part 3): the index traversal an index lookup performs must be visible
    in {!Granary_sql.Exec.query_stats}.

    [rows_examined] counts only the TABLE rows the executor pulled.
    [stream_index_lookup] reaches those rows by walking an index and doing one
    [rh_get] on the table tree per entry, so a seek and the sequential scan it
    replaced are charged the same amount for reading the same rows, and the
    per-entry tree descent — the entire cost #546 is about — is invisible.

    That is not a hypothetical: measured on disk at 100,000 stock rows against
    30,000 driving rows (test/bench_build_side_seek_546.ml), a build-side seek
    whose pinned prefix selects every row costs 393,980 pager reads against the
    scan's 93,067, and 2.3-3.5x the wall time, while [rows_examined] reports
    130,000 for both. [index_entries] is the counter that tells them apart, so a
    guard for #546/#550 has something to assert on when one is written.

    Nothing here asserts that the current plan choice is GOOD — #546 is open
    precisely because it often is not. These cases pin what is observable. *)

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

(* The #528 population, so the numbers line up with that file's. *)
let n_line = 1200
let n_w = 4
let n_per_w = 500
let n_stock = n_w * n_per_w

let seed db =
  exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
  exec db "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
  exec db "BEGIN";
  for o = 1 to n_line do
    exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o ((o mod n_per_w) + 1))
  done;
  for w = 1 to n_w do
    for si = 1 to n_per_w do
      exec
        db
        (Printf.sprintf "INSERT INTO stock VALUES (%d, %d, %d)" w si ((w * 1000) + si))
    done
  done;
  exec db "COMMIT"
;;

let q pred =
  Printf.sprintf
    "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND %s"
    pred
;;

(* A plan with no index lookup in it leaves the counter alone. *)
let a_scan_walks_no_index_entries () =
  with_db (fun db ->
    seed db;
    let _, st = stats_of db "SELECT qty FROM stock WHERE qty > 0" in
    Alcotest.(check int) "a seq scan examines every row" n_stock st.Db.rows_examined;
    Alcotest.(check int) "and walks no index" 0 st.Db.index_entries)
;;

(* The counter this issue needs. Both plans read the same 1,200 driving rows
   through [line]'s primary key, so both walk 1,200 index entries for that. The
   seeked build side walks 500 more — one per stock row it fetches — and the
   scanned one walks none.

   [rows_examined] moves the other way (1,700 against 3,200), which is exactly
   why it cannot stand in for this: it says the seek did half the work, and says
   nothing at all about the 500 tree descents that made it, on disk, the slower
   plan. *)
let a_seeked_build_side_walks_one_entry_per_row () =
  with_db (fun db ->
    seed db;
    let _, seek = stats_of db (q "sw = 1") in
    let _, scan = stats_of db (q "sw + 0 = 1") in
    Alcotest.(check int)
      "seek: the driving side's entries plus one per build row"
      (n_line + n_per_w)
      seek.Db.index_entries;
    Alcotest.(check int)
      "scan: the driving side's entries only"
      n_line
      scan.Db.index_entries;
    Alcotest.(check int)
      "seek: rows examined fall"
      (n_line + n_per_w)
      seek.Db.rows_examined;
    Alcotest.(check int)
      "scan: rows examined are the whole of stock"
      (n_line + n_stock)
      scan.Db.rows_examined;
    Alcotest.(check bool)
      "rows examined say the seek is cheaper while it walks MORE index entries"
      true
      (seek.Db.rows_examined < scan.Db.rows_examined
       && seek.Db.index_entries > scan.Db.index_entries))
;;

(* #532's range bound cuts the entries walked, not just the rows fetched — the
   walk stops at the upper bound rather than running to the end of the pinned
   prefix. Without the counter this narrowing and a filter that discarded the
   same rows after fetching them would look identical. *)
let a_range_bound_cuts_the_entries_walked () =
  with_db (fun db ->
    seed db;
    let _, bounded = stats_of db (q "sw = 1 AND si BETWEEN 20 AND 40") in
    let _, unbounded = stats_of db (q "sw = 1 AND si + 0 BETWEEN 20 AND 40") in
    Alcotest.(check int)
      "bounded: the driving side plus the 21-key window"
      (n_line + 21)
      bounded.Db.index_entries;
    Alcotest.(check int)
      "unbounded: the driving side plus the whole pinned prefix"
      (n_line + n_per_w)
      unbounded.Db.index_entries)
;;

(* A fresh record starts zeroed, and the field is part of the public [Db] type
   rather than something only the executor can see. *)
let a_fresh_record_is_zeroed () =
  let st = Granary_sql.Exec.make_query_stats () in
  Alcotest.(check int) "index entries" 0 st.Granary_sql.Exec.index_entries;
  Alcotest.(check int) "rows examined" 0 st.Granary_sql.Exec.rows_examined
;;

let () =
  Alcotest.run
    "index entries visible in query stats (#546)"
    [ ( "counter"
      , [ Alcotest.test_case "a fresh record is zeroed" `Quick a_fresh_record_is_zeroed
        ; Alcotest.test_case
            "a scan walks no index entries"
            `Quick
            a_scan_walks_no_index_entries
        ; Alcotest.test_case
            "a seeked build side walks one entry per row"
            `Quick
            a_seeked_build_side_walks_one_entry_per_row
        ; Alcotest.test_case
            "a range bound cuts the entries walked"
            `Quick
            a_range_bound_cuts_the_entries_walked
        ] )
    ]
;;
