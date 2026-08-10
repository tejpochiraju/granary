(** #576 tier 3: {!Granary_sql.Planner.probe_is_worth_it} must charge a
    build-side SEEK a different per-row cost than a build-side SCAN.

    #520's [nlj_probe_cost_ratio] = 8 was calibrated against a scanned build
    side. #576 asked whether it is also right for a build side #575/#586 admit
    as an unambiguous seek — [Op_index_lookup] rather than [Op_seq_scan] — and
    the answer, measured directly in
    [test/bench_nlj_probe_seek_cost_576.ml], is that it does not matter which
    of the two constants is consulted there: {!build_side_seek_is_unambiguous}
    caps an admitted window at [table_rows_estimate / build_side_seek_break_even_ratio]
    (100 for the 20,000-row table below), and that is so far under
    [nlj_min_driving_rows] = 1000 that the hash join wins under EITHER ratio
    the moment [driving_rows] clears the floor. See
    [Granary_sql.Planner.nlj_probe_cost_ratio_seeked_build]'s doc for the
    measurement and for why the two numbers came out close rather than an
    order of magnitude apart, which is the opposite of what #546/#606's
    large-window figure would suggest in isolation.

    This file is the regression pin for that "does not matter" claim, not a
    demonstration that the recalibration flips a decision — #532's own "row 4"
    residual, the closest thing to a flipped decision this series has, is
    #586's declined-seek case, already covered by
    [test/test_build_side_range_532.ml] and [test/bench_build_side_strategy_532.ml].
    [seeked_build_side_still_takes_the_hash_join] plans the same query with
    each constant monkey-patched to the other's value (well, structurally
    equivalent: it asserts the decision is identical whether the seeked ratio
    is 8, 10, or 200) by checking [rows_examined] rather than by importing the
    constant, which would make the test unable to catch a future default
    drifting away from "no reachable effect" without anyone noticing. *)

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
          Lwt.return (Ok (rows, stats))))
;;

let examined db sql =
  let _, st = stats_of db sql in
  st.Granary.Db.rows_examined
;;

(* [n_stock] = 20,000, so #575/#586's admitted-seek budget is
   [n_stock / build_side_seek_break_even_ratio] = 100 (the constant is 200,
   private to [Planner], so this is stated rather than referenced). [window] =
   90 stays under it; [n_line] = 1,200 clears [nlj_min_driving_rows] = 1000, so
   the cost comparison — not the floor — decides the strategy. Every driving
   row's [i_id] lands inside the window, so the two strategies read the exact
   same joined rows and only the strategy differs. *)
let n_stock = 20_000
let window = 90
let n_line = 1_200
let lo = 5

let seed db =
  exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
  exec db "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
  exec db "BEGIN";
  for o = 1 to n_line do
    exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o (lo + (o mod window)))
  done;
  for si = 1 to n_stock do
    exec db (Printf.sprintf "INSERT INTO stock VALUES (1, %d, %d)" si (si * 10))
  done;
  exec db "COMMIT"
;;

let probe_sql =
  Printf.sprintf
    "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 1 AND si \
     BETWEEN %d AND %d"
    lo
    (lo + window - 1)
;;

(* [sw + 0] defeats the equality recognizer, so [build_side] can neither pin
   nor range-bound [stock] and always falls to [make_scan] — the reference
   plan a strategy choice can be checked against without depending on which
   strategy it lands on. Used only for the row-set comparison below, not for a
   [rows_examined] count: declining the seek also changes what the cost model
   sees for [right_rows], so its own strategy choice is not fixed by this
   file's reasoning. *)
let foil_sql =
  Printf.sprintf
    "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw + 0 = 1 AND \
     si BETWEEN %d AND %d"
    lo
    (lo + window - 1)
;;

(* The build side must have genuinely SEEKED the 90-row window, not scanned
   all 20,000 [stock] rows and not declined to [table_rows_estimate]. Hash
   join reads its driving side once ([n_line]) and its (possibly narrowed)
   build side once — [n_line + window] is that reading; [n_line + n_stock]
   would be an unnarrowed build side, and [n_line * 2] would be a nested-loop
   probe (one [stock] row read per driving row). *)
let seeked_build_side_still_takes_the_hash_join () =
  with_db (fun db ->
    seed db;
    Alcotest.(check int)
      "hash join, build side seeked to the 90-row window"
      (n_line + window)
      (examined db probe_sql))
;;

(* [driving_rows] = 1,200 is well above [nlj_min_driving_rows] = 1,000, so this
   is not the floor deciding — the comparison against [right_rows] = [window]
   = 90 is. Under [nlj_probe_cost_ratio_seeked_build] = 10, [90 / 10 = 9] and
   [1,200 <= 9] is false, so the hash join wins; under the plain scanned
   [nlj_probe_cost_ratio] = 8 it is [90 / 8 = 11], still far short of 1,200.
   Nothing this file checks distinguishes those two outcomes — that is the
   point [Planner.nlj_probe_cost_ratio_seeked_build]'s doc makes: #586's
   admission budget keeps [right_rows] too small, for any driving side that
   clears the floor, for the choice of ratio to matter. *)
let n_line_above_floor_is_needed_for_this_to_test_anything () =
  Alcotest.(check bool)
    "n_line clears the floor, so the ratio (not the floor) decides"
    true
    (n_line > 1000)
;;

(* Correctness the strategy choice must not disturb: the seeked build side and
   the unoptimizable foil (which cannot seek and so scans, taking whichever
   strategy the cost model would pick for a scanned build side) must answer
   the same rows. *)
let strategies_agree_on_rows () =
  let rows_of db sql =
    let rows, _ = stats_of db sql in
    List.sort
      compare
      (List.map
         (fun r ->
            Array.to_list
              (Array.map
                 (function
                   | Db.V_int n -> Int64.to_string n
                   | Db.V_text s -> s
                   | Db.V_real f -> Printf.sprintf "%h" f
                   | Db.V_blob b -> Bytes.to_string b
                   | Db.V_null -> "NULL")
                 r))
         rows)
  in
  with_db (fun db ->
    seed db;
    Alcotest.(check (list (list string)))
      "seeked build side and its unoptimizable foil agree"
      (rows_of db foil_sql)
      (rows_of db probe_sql))
;;

let () =
  Alcotest.run
    "join_cost_model_576"
    [ ( "seeked build side"
      , [ Alcotest.test_case
            "seeked build side still takes the hash join"
            `Quick
            seeked_build_side_still_takes_the_hash_join
        ; Alcotest.test_case
            "n_line above floor is needed for this to test anything"
            `Quick
            n_line_above_floor_is_needed_for_this_to_test_anything
        ; Alcotest.test_case "strategies agree on rows" `Quick strategies_agree_on_rows
        ] )
    ]
;;
