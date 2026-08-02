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

(* #575 chose option B: a BARE prefix pin no longer takes the build-side seek,
   because nothing in the catalog can tell a prefix that selects 1% of the table
   from one that selects 100%, and on disk the second costs 2.6-3.5x the scan it
   replaces. So [sw = 1] and its unrecognisable foil now plan identically.

   This case is the reason [index_entries] was added. [rows_examined] reports
   3,200 for BOTH plans here and would report 3,200 for both if the seek were
   still taken — it counts table rows pulled, and the seek pulls warehouse 1's
   500 where the scan pulls 2,000 but then discards 1,500 above the join. The
   only counter that distinguishes "declined the seek" from "took it" is the one
   that counts the tree descents: 1,200 (the driving side alone) against the
   1,700 the seek would have walked. *)
let a_bare_prefix_pin_no_longer_seeks () =
  with_db (fun db ->
    seed db;
    let _, pinned = stats_of db (q "sw = 1") in
    let _, foil = stats_of db (q "sw + 0 = 1") in
    Alcotest.(check int)
      "the pinned prefix walks the driving side's entries and no more"
      n_line
      pinned.Db.index_entries;
    Alcotest.(check int)
      "the unrecognisable foil walks the same"
      n_line
      foil.Db.index_entries;
    (* The number a taken seek would have shown, spelled out so a regression
       reads as "1700 <> 1200" rather than as an opaque mismatch. *)
    Alcotest.(check bool)
      "and neither walks the 500 extra entries a #528 seek would have"
      true
      (pinned.Db.index_entries < n_line + n_per_w);
    Alcotest.(check int)
      "both read the whole of stock"
      (n_line + n_stock)
      pinned.Db.rows_examined;
    Alcotest.(check int)
      "rows_examined cannot tell the two apart — that is why this file exists"
      foil.Db.rows_examined
      pinned.Db.rows_examined)
;;

(* The converse of the case above, and the half of #528 that #575 KEPT: a pin
   covering a unique index's whole key reaches at most one row, so it is
   unambiguously better than a scan whatever the value distribution is. Here
   [stock]'s primary key is [(sw, si)] and both are pinned. *)
let a_full_unique_key_pin_still_seeks () =
  with_db (fun db ->
    seed db;
    let _, seek = stats_of db (q "sw = 1 AND si = 20") in
    let _, foil = stats_of db (q "sw + 0 = 1 AND si = 20") in
    Alcotest.(check int)
      "the driving side's entries plus the one the point seek walks"
      (n_line + 1)
      seek.Db.index_entries;
    Alcotest.(check int) "the foil walks no build-side entry" n_line foil.Db.index_entries;
    Alcotest.(check int)
      "and it fetches exactly one stock row"
      (n_line + 1)
      seek.Db.rows_examined)
;;

(* #532's range bound cuts the entries walked, not just the rows fetched — the
   walk stops at the upper bound rather than running to the end of the pinned
   prefix. Without the counter this narrowing and a filter that discarded the
   same rows after fetching them would look identical.

   #575 kept this case: a range-bounded seek is not the open-ended prefix walk
   the decision declined, and it is the one shape with a span estimate the
   planner can read. Its foil is now a plain scan rather than a prefix walk,
   because the foil's unrecognisable range leaves a bare prefix pin.

   #606 shrank the window from 21 keys to 6. The gate now admits a literal
   window only below the MEASURED break-even —
   [table_rows / build_side_seek_break_even_ratio], i.e. 2,000 / 200 = 10 keys on
   this population — where before it admitted anything numerically smaller than
   the table. 21 keys over 2,000 rows is 1/95 of the table, which the measurement
   behind that constant puts on the losing side, so the planner declines it and
   this case has to ask for a window that is genuinely small relative to its own
   population. The narrowing under test is unchanged; only its width is. *)
let n_window = 6

let a_range_bound_cuts_the_entries_walked () =
  with_db (fun db ->
    seed db;
    let hi = 20 + n_window - 1 in
    let _, bounded =
      stats_of db (q (Printf.sprintf "sw = 1 AND si BETWEEN 20 AND %d" hi))
    in
    let _, unbounded =
      stats_of db (q (Printf.sprintf "sw = 1 AND si + 0 BETWEEN 20 AND %d" hi))
    in
    Alcotest.(check int)
      "bounded: the driving side plus the 6-key window"
      (n_line + n_window)
      bounded.Db.index_entries;
    Alcotest.(check int)
      "unbounded: a #575-declined bare prefix, so the driving side alone"
      n_line
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
            "a bare prefix pin no longer seeks (#575)"
            `Quick
            a_bare_prefix_pin_no_longer_seeks
        ; Alcotest.test_case
            "a full unique-key pin still seeks (#575)"
            `Quick
            a_full_unique_key_pin_still_seeks
        ; Alcotest.test_case
            "a range bound cuts the entries walked"
            `Quick
            a_range_bound_cuts_the_entries_walked
        ] )
    ]
;;
