(** #532: the hash join's build side must take a range bound, not just an
    equality prefix.

    #528 gave the build side an access path by feeding [right_table_eqs] — the
    WHERE equalities that pin a right-table column, re-based from combined-row
    ordinals to right-table ones — into the shared access-path chooser. It
    passed [~range_conjuncts:[]], because [range_for_index] re-recognises whole
    conjuncts and so needs the ordinal re-based {i inside} the expression, which
    the equality path never has to do.

    The cost of that was a build side that seeked to the start of its pinned
    prefix and then walked the prefix to its end, even when the WHERE clause
    bounded the {i next} index column. The repro (#540, folded into #532):

    {v
      ... JOIN stock ON si = i_id WHERE sw = 1 AND si BETWEEN 20 AND 25
    v}

    seeks [sw = 1] and then reads every row of warehouse 1 rather than stopping
    at [si = 25].

    [right_table_ranges] is the missing half. It shares [rebase_right_col] with
    [right_table_eqs] — the two must agree about which slots the right table owns
    — and re-emits each recognised end as a canonical [col >= v] / [col <= v]
    conjunct over the re-based ordinal, which round-trips through
    [recognise_range_col_lit] to the same ends.

    [rows_examined] is the load-bearing assertion, for the reason
    [test_hash_join_build_seek_528.ml] gives: in-memory databases emit no page
    events, and a bounded build side is told from an unbounded one by counting
    the base rows the executor pulled.

    #595 adds [index_entries] alongside it, because the two counters answer
    different questions and the one this file had could not see the case the
    #575/#606 gate actually decides — see [entries] below.

    Every narrowing case is paired with a foil that cannot be narrowed
    ([sw + 0 = 1], [si + 0 BETWEEN …]), and the two must return identical rows.
    That is what proves the bound only changes what is read, never what is
    answered — the invariant #513/#516/#528 rest on: [chain_joins] applies the
    whole WHERE clause to the joined row. *)

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

let render = function
  | Db.V_int n -> Int64.to_string n
  | Db.V_text s -> s
  | Db.V_real f -> Printf.sprintf "%h" f
  | Db.V_blob b -> Bytes.to_string b
  | Db.V_null -> "NULL"
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

let rows_of db sql =
  let rows, _ = stats_of db sql in
  List.sort compare (List.map (fun r -> Array.to_list (Array.map render r)) rows)
;;

let examined db sql =
  let _, st = stats_of db sql in
  st.Granary.Db.rows_examined
;;

(* #595: index entries walked, the counter [rows_examined] cannot substitute
   for.

   Both are needed and they answer different questions. [rows_examined] counts
   TABLE rows pulled, so it separates a narrowed build side from a full one — but
   it reports the same figure for a seeked build side and a scanned one that
   yields the same rows, which is exactly the pair #575/#606 decide between.
   [index_entries] counts entries walked in an INDEX, so it is zero for a plan
   with no seek anywhere and non-zero the moment one is taken. In this file's
   shape the driving [line] side always seeks, so the reading is: [n_line] on its
   own means the build side scanned, [n_line + w] means it seeked a w-key
   window. *)
let entries db sql =
  let _, st = stats_of db sql in
  st.Granary.Db.index_entries
;;

(* #595: the parameterised counterpart of [stats_of]. A bound parameter reaches
   [range_for_index] exactly as a literal does, so the only way to observe that
   #575 declines it is to prepare and bind rather than to interpolate. *)
let stats_with db sql params =
  unwrap
    (run
       (let open Lwt.Syntax in
        let* s = Db.prepare db sql in
        match s with
        | Error e -> Lwt.return (Error e)
        | Ok stmt ->
          let* r = Db.iter_with_stats stmt ~params in
          (match r with
           | Error e -> Lwt.return (Error e)
           | Ok (stream, stats) ->
             let* rows = Lwt_stream.to_list stream in
             Lwt.return (Ok (rows, stats)))))
;;

let rows_with db sql params =
  let rows, _ = stats_with db sql params in
  List.sort compare (List.map (fun r -> Array.to_list (Array.map render r)) rows)
;;

let examined_with db sql params =
  let _, st = stats_with db sql params in
  st.Granary.Db.rows_examined
;;

let entries_with db sql params =
  let _, st = stats_with db sql params in
  st.Granary.Db.index_entries
;;

let same_rows db ~label ~seek ~foil =
  Alcotest.(check (list (list string))) label (rows_of db foil) (rows_of db seek)
;;

(* ------------------------------------------------------------------ *)
(* The population — the #528 one, so the two files' numbers compare      *)
(* ------------------------------------------------------------------ *)

(* Above the planner's 1000-row absolute floor (#520), so the strategy choice is
   the ratio test and this really is a hash join with a build side. Below the
   floor a probe is taken unconditionally and nothing here runs. *)
let n_line = 1200

(* Four warehouses of 500, so [sw = 1] alone leaves 500 build-side rows for the
   range to cut down. 1200 driving rows lose the ratio test against 2000
   (1200 > 2000/8), and against the range-bounded estimate too (1200 > 100/8),
   so both spellings stay on the hash join. *)
let n_w = 4
let n_per_w = 500
let n_stock = n_w * n_per_w

(* The window the range selects: [si] in 20..25 inclusive.

   #606 is why it is six keys and not the twenty-one this file was written
   around. The #575 gate used to admit any literal window numerically smaller
   than the table; it now admits one only below the MEASURED break-even, which
   is [table_rows / Planner.build_side_seek_break_even_ratio] — 2,000 / 200 = 10
   rows here. A 21-key window over a 2,000-row table is 1/95 of it, which the
   measurement in that constant's doc puts on the losing side of the trade, so
   the planner is right to decline it and this file has to ask for a window that
   is genuinely small relative to its own population.

   Six leaves room for [inequalities_bound_the_build_side]'s strict pair, which
   spells the same window as [si > 19 AND si < 26] and so presents an eight-key
   range to the gate — strictness is dropped before the estimator sees it. *)
let lo = 20
let hi = 25
let n_window = hi - lo + 1

(* #606: the largest window this population's [stock] admits. Derived here
   rather than written as a literal so that a change to either the population or
   the ratio moves the boundary cases below with it. *)
let break_even_window = n_stock / 200

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

(* ------------------------------------------------------------------ *)
(* The narrowing                                                        *)
(* ------------------------------------------------------------------ *)

(* The headline: the #540 repro. [sw = 1] pins the prefix (#528) and
   [si BETWEEN 20 AND 25] bounds the next index column (#532).

   Three plans, three build-side costs:
     - unpinned foil       [sw + 0 = 1]  reads all 2,000 stock rows
     - #528, prefix only   [sw = 1]      reads warehouse 1's 500
     - #532, prefix+range  ... BETWEEN   reads the 6-row window

   #595: the [index_entries] assertions are the ones that say WHICH plan ran.
   [rows_examined] separates the narrowed build side from the wide one, but it
   would report the same 1,206 for a build side that scanned warehouse 1's 500
   rows and filtered them down to 6 — the counters have to be read together. *)
let between_bounds_the_build_side () =
  with_db (fun db ->
    seed db;
    let seek = q (Printf.sprintf "sw = 1 AND si BETWEEN %d AND %d" lo hi) in
    (* [si + 0] is not a recognised range, so this foil's build side can only
       walk the whole pinned prefix — the pre-#532 plan, on the same data. *)
    let foil = q (Printf.sprintf "sw = 1 AND si + 0 BETWEEN %d AND %d" lo hi) in
    (* And this one cannot even pin the prefix — the pre-#528 plan. *)
    let scan = q (Printf.sprintf "sw + 0 = 1 AND si + 0 BETWEEN %d AND %d" lo hi) in
    same_rows db ~label:"bounded and unbounded build sides agree" ~seek ~foil;
    same_rows db ~label:"bounded build side agrees with a full scan" ~seek ~foil:scan;
    Alcotest.(check bool)
      "and the agreement is not between two empty lists"
      true
      (rows_of db seek <> []);
    Alcotest.(check int)
      "unpinned: the driving side plus every warehouse's stock"
      (n_line + n_stock)
      (examined db scan);
    (* #575: a BARE prefix pin no longer seeks — the decision declined every
       open-ended prefix walk, which is what this foil now is.  So it costs the
       same full scan the unpinned one does, and the two upper rows of the
       comparison have collapsed into each other.  The gap this case is really
       about is the one below it: prefix+range against everything else. *)
    Alcotest.(check int)
      "prefix only, #575-declined: the same full scan as the unpinned foil"
      (n_line + n_stock)
      (examined db foil);
    Alcotest.(check int)
      "prefix + range (#532): the driving side plus the 6-row window"
      (n_line + n_window)
      (examined db seek);
    (* #595 *)
    Alcotest.(check int)
      "unpinned: the driving seek's entries and no others"
      n_line
      (entries db scan);
    Alcotest.(check int)
      "prefix only, #575-declined: likewise, no build-side index walk at all"
      n_line
      (entries db foil);
    Alcotest.(check int)
      "prefix + range: the driving seek plus the window's index entries"
      (n_line + n_window)
      (entries db seek))
;;

(* The [BETWEEN] is one conjunct constraining both ends; two inequalities are
   two conjuncts constraining one end each, and reach [range_for_index] through
   a different arm of [recognise_range_col_lit]. Both must bound the walk.

   Strict [>]/[<] is deliberately treated as inclusive — [right_table_ranges]
   re-emits every end as [>=]/[<=], preserving what [recognise_range_col_lit]
   already did — so a strict pair reads exactly one extra key per end and can
   never lose a row. [si > 19 AND si < 41] selects the same 21 rows and reads
   23: rows 19 and 41 are fetched and then dropped by the filter, which is the
   documented price of dropping strictness. *)
let inequalities_bound_the_build_side () =
  with_db (fun db ->
    seed db;
    List.iter
      (fun (pred, extra, label) ->
         let seek = q (Printf.sprintf "sw = 1 AND %s" pred) in
         let foil = q (Printf.sprintf "sw = 1 AND si + 0 BETWEEN %d AND %d" lo hi) in
         same_rows db ~label:(label ^ ": agrees with its unbounded foil") ~seek ~foil;
         Alcotest.(check int)
           (label ^ ": reads only the window")
           (n_line + n_window + extra)
           (examined db seek))
      [ Printf.sprintf "si >= %d AND si <= %d" lo hi, 0, "inclusive pair"
      ; Printf.sprintf "si > %d AND si < %d" (lo - 1) (hi + 1), 2, "strict pair"
      ; Printf.sprintf "%d <= si AND %d >= si" lo hi, 0, "reversed operands"
      ])
;;

(* One end only — DECLINED since #575, and this case is the reason the #575 gate
   is written the way it is.

   #532 admitted these because "a range stops the walk at a bound". A one-ended
   range does not: [range_for_index] answers [Some] when EITHER end is present,
   so [si >= 20] starts the walk 19 keys in and then runs to the end of the
   pinned prefix — 481 of warehouse 1's 500 rows, which is the number this case
   asserted while green. That is #546's open-ended prefix walk with a decoration
   on the front, and it was measured on #546's own 100,000-row population at
   3.6x slower than the scan (1.00-1.06 s against 0.28 s) — worse than the
   2.6-3.5x #575 exists to remove, and reachable by appending a tautology
   ([si >= 0]) to a WHERE clause.

   The second half of the #532 argument fails here too: [range_rows_estimate]
   needs BOTH ends to be integer literals and answers the flat
   [range_seek_rows = 100] otherwise, so a one-ended range has no estimate to
   consult either.

   The rows must still agree with the foil — that is the invariant #576 needs
   intact — so those assertions are unchanged. *)
let one_ended_ranges_are_declined_575 () =
  with_db (fun db ->
    seed db;
    let upper = q (Printf.sprintf "sw = 1 AND si <= %d" hi) in
    let lower = q (Printf.sprintf "sw = 1 AND si >= %d" lo) in
    same_rows
      db
      ~label:"upper bound only agrees with its foil"
      ~seek:upper
      ~foil:(q (Printf.sprintf "sw = 1 AND si + 0 <= %d" hi));
    same_rows
      db
      ~label:"lower bound only agrees with its foil"
      ~seek:lower
      ~foil:(q (Printf.sprintf "sw = 1 AND si + 0 >= %d" lo));
    Alcotest.(check int)
      "upper bound: no seek, so the whole of stock"
      (n_line + n_stock)
      (examined db upper);
    Alcotest.(check int)
      "lower bound: no seek, so the whole of stock"
      (n_line + n_stock)
      (examined db lower))
;;

(* The other half of the same gate: a range whose ends ARE both integer literals
   but whose window is too wide is the same open-ended walk in a different
   spelling, so being able to READ the window is not enough — it has to come back
   below the break-even.

   #606: this case used [n_stock * 100] as its only window, and that exercised
   only the easy half. The gate it was written for declined a window numerically
   larger than the table and admitted everything else, so a span one literal
   short of the row count — [si BETWEEN 0 AND 1999] over 2,000 rows — passed it
   and, measured on disk at 10,000 rows, cost 4.19x the pager reads of the scan
   it replaced. Both windows are asserted now, so the residual cannot come back
   without a red test. *)
let a_table_wide_literal_window_is_declined_575 () =
  with_db (fun db ->
    seed db;
    List.iter
      (fun (upper, label) ->
         let seek = q (Printf.sprintf "sw = 1 AND si BETWEEN 0 AND %d" upper) in
         let foil = q (Printf.sprintf "sw = 1 AND si + 0 BETWEEN 0 AND %d" upper) in
         same_rows db ~label:(label ^ ": agrees with its foil") ~seek ~foil;
         Alcotest.(check int)
           (label ^ ": reads the whole of stock rather than seeking")
           (n_line + n_stock)
           (examined db seek);
         Alcotest.(check int)
           (label ^ ": and walks no build-side index entries (#595)")
           n_line
           (entries db seek))
      [ n_stock * 100, "a window a hundred times the table"
      ; n_stock - 1, "a window one key short of the table (#606)"
      ])
;;

(* #606, B1: a window is sized in key VALUES of the BOUNDED column, so it only
   bounds the entries a seek walks when that column is the LAST one in the index.

   [index_is_unique] makes the whole key unique, not the prefix through the
   bounded column, and [range_for_index] places the range at index position
   [n_eq] with nothing requiring it to be last. On a three-column key every
   column after the bounded one multiplies the entries a window reaches,
   invisibly to the estimate — so the first version of the #606 gate sized this
   query's window at 20, admitted it, and walked all 4,000 entries. That is 200x
   the budget and 100% selectivity: verbatim what #606 was filed to stop.

   [rows_examined] is IDENTICAL for both plans here — 1,200 driving rows plus
   4,000 stock rows either way — which is #546's whole point and the reason this
   case can only be pinned on [index_entries].

   The shape is not exotic. TPC-C's [order_line] key is
   [(ol_w_id, ol_d_id, ol_o_id, ol_number)], so a range on [ol_o_id] under a
   pinned [(ol_w_id, ol_d_id)] is exactly this. *)
let n_si3 = 20
let n_sub3 = 200

let seed_three_col db =
  exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
  exec
    db
    "CREATE TABLE stock (sw INTEGER, si INTEGER, sub INTEGER, qty INTEGER, PRIMARY KEY \
     (sw, si, sub))";
  exec db "BEGIN";
  for o = 1 to n_line do
    exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o ((o mod n_sub3) + 1))
  done;
  for si = 1 to n_si3 do
    for sub = 1 to n_sub3 do
      exec
        db
        (Printf.sprintf
           "INSERT INTO stock VALUES (1, %d, %d, %d)"
           si
           sub
           ((si * 1000) + sub))
    done
  done;
  exec db "COMMIT"
;;

let a_range_before_the_last_index_column_is_declined_606 () =
  with_db (fun db ->
    seed_three_col db;
    let g pred =
      Printf.sprintf
        "SELECT qty FROM line INNER JOIN stock ON sub = i_id WHERE w = 1 AND %s"
        pred
    in
    (* The gate's budget is 4000/200 = 20 and the window is exactly 20 key
       values of [si] — so it passes every OTHER conjunct, and only the
       "bounded column is last" test can decline it. *)
    let seek = g (Printf.sprintf "sw = 1 AND si BETWEEN 1 AND %d" n_si3) in
    let foil = g (Printf.sprintf "sw = 1 AND si + 0 BETWEEN 1 AND %d" n_si3) in
    same_rows db ~label:"the declined plan agrees with its foil" ~seek ~foil;
    Alcotest.(check bool)
      "and the agreement is not between two empty lists"
      true
      (rows_of db seek <> []);
    Alcotest.(check int)
      "rows_examined cannot tell the two plans apart — this is #546's point"
      (examined db foil)
      (examined db seek);
    Alcotest.(check int)
      "declined: the driving seek's entries alone, not all 4,000 stock entries"
      n_line
      (entries db seek);
    Alcotest.(check int) "as the foil is" n_line (entries db foil))
;;

(* #606, B2: the same defect class one TYPE over.

   B1 above fixed the POSITION premise — the bounded column must be last. It did
   not fix the TYPE premise, and the two are independent. [range_int_literal_span]
   inspects the two BOUNDS and never consults [r_ty], while [bounded_type] admits
   [Row.Real] — so on a REAL last column the estimate counts the INTEGERS in
   [lo, hi] while the seek walks the distinct REALS in it, which is unbounded.

   4,000 rows with [sr] spread strictly inside (0,1): [sr BETWEEN 0 AND 1] is
   sized at 2 key values against a budget of 20, admitted, and walks all 4,000
   entries. 2,000x — worse than B1's 200x, and by the same mechanism.

   [rows_examined] is identical for both plans again, so [index_entries] is the
   only counter that can pin it.

   A REAL trailing key column is the canonical time-series shape —
   [PRIMARY KEY (sensor, ts)] with [ts BETWEEN <day> AND <day+1>] — not a
   corner. *)
let n_real = 4_000

let seed_real_key db =
  (* [i_id] is REAL here: column typing is strict, and the join column has to
     match [sr]'s type for [ON sr = i_id] to be an equi-join at all. *)
  exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id REAL, PRIMARY KEY (w, o))";
  exec db "CREATE TABLE stock (sw INTEGER, sr REAL, qty INTEGER, PRIMARY KEY (sw, sr))";
  exec db "BEGIN";
  for o = 1 to n_line do
    exec
      db
      (Printf.sprintf
         "INSERT INTO line VALUES (1, %d, %.9f)"
         o
         (float_of_int ((o mod n_real) + 1) /. float_of_int (n_real + 1)))
  done;
  (* Strictly inside (0,1), so every row falls in the window [0, 1] and NOT ONE
     of them is an integer — the estimate's unit and the walk's unit could not be
     further apart. *)
  for i = 1 to n_real do
    exec
      db
      (Printf.sprintf
         "INSERT INTO stock VALUES (1, %.9f, %d)"
         (float_of_int i /. float_of_int (n_real + 1))
         (i * 10))
  done;
  exec db "COMMIT"
;;

let a_real_bounded_column_is_declined_606 () =
  with_db (fun db ->
    seed_real_key db;
    let g pred =
      Printf.sprintf
        "SELECT qty FROM line INNER JOIN stock ON sr = i_id WHERE w = 1 AND %s"
        pred
    in
    (* Every other conjunct passes: the key is unique, [sr] is its last column,
       and the window the estimator computes is 2 against a budget of 20. Only
       the type test can decline this. *)
    let seek = g "sw = 1 AND sr BETWEEN 0 AND 1" in
    let foil = g "sw = 1 AND sr + 0 BETWEEN 0 AND 1" in
    same_rows db ~label:"the declined plan agrees with its foil" ~seek ~foil;
    Alcotest.(check bool)
      "and the agreement is not between two empty lists"
      true
      (rows_of db seek <> []);
    Alcotest.(check int)
      "rows_examined cannot tell the two plans apart — #546's point again"
      (examined db foil)
      (examined db seek);
    Alcotest.(check int)
      "declined: the driving seek's entries alone, not all 4,000 stock entries"
      n_line
      (entries db seek);
    Alcotest.(check int) "as the foil is" n_line (entries db foil))
;;

(* #606: the break-even boundary itself, from both sides.

   The gate is [window <= table_rows / build_side_seek_break_even_ratio], and
   with 2,000 stock rows and a ratio of 200 that cut is exactly 10 keys. One
   below it seeks, one above it scans, and nothing about the two queries differs
   apart from a single literal — which is the point: before #606 the cut sat at
   the table's row count instead, so [si BETWEEN 0 AND 1999] seeked and only
   [si BETWEEN 0 AND 2000] did not.

   [index_entries] is what separates them; [rows_examined] would too here, but
   only because the window is small enough to change the row count. On the
   declined side both counters agree the build side scanned. *)
let the_break_even_boundary_is_where_the_gate_cuts_606 () =
  with_db (fun db ->
    seed db;
    let at = break_even_window in
    let window upper = Printf.sprintf "sw = 1 AND si BETWEEN 1 AND %d" upper in
    let seek = q (window at) in
    let over = q (window (at + 1)) in
    same_rows
      db
      ~label:"the admitted window agrees with its foil"
      ~seek
      ~foil:(q (Printf.sprintf "sw = 1 AND si + 0 BETWEEN 1 AND %d" at));
    same_rows
      db
      ~label:"the declined window agrees with its foil"
      ~seek:over
      ~foil:(q (Printf.sprintf "sw = 1 AND si + 0 BETWEEN 1 AND %d" (at + 1)));
    Alcotest.(check int)
      "at the break-even: the driving seek plus the window"
      (n_line + at)
      (entries db seek);
    Alcotest.(check int)
      "one key past it: the driving seek alone — the build side scanned"
      n_line
      (entries db over);
    Alcotest.(check int)
      "and rows_examined agrees on the declined side"
      (n_line + n_stock)
      (examined db over))
;;

(* A range on a NON-UNIQUE index, which the #575 gate must also decline.

   [range_rows_estimate] counts distinct KEY VALUES; [table_rows_estimate] counts
   ROWS. Comparing the two is only meaningful when one key value is one row, i.e.
   when the index is unique — [range_rows_estimate]'s own doc says so: "a span
   counts distinct key VALUES, and a non-unique index may hold many rows per
   value, so this can under-state the row count."

   Here [cat] takes 10 values across 500 rows per warehouse, so
   [cat BETWEEN 0 AND 9] estimates 10 rows (floored to [range_seek_rows] = 100)
   for a window that actually holds the entire pinned prefix. Measured on disk at
   100,000 rows this is 4.2x slower than the scan it replaces — the same failure
   mode as the one-ended hole, one level down: an estimate consulted about a
   question it cannot answer. *)
let n_cat = 10

let seed_non_unique db =
  exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
  exec db "CREATE TABLE cats (sw INTEGER, cat INTEGER, qty INTEGER)";
  exec db "CREATE INDEX cats_ix ON cats (sw, cat)";
  exec db "BEGIN";
  for o = 1 to n_line do
    exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o (o mod n_cat))
  done;
  for w = 1 to n_w do
    for i = 1 to n_per_w do
      exec
        db
        (Printf.sprintf
           "INSERT INTO cats VALUES (%d, %d, %d)"
           w
           (i mod n_cat)
           ((w * 1000) + i))
    done
  done;
  exec db "COMMIT"
;;

let a_non_unique_index_range_is_declined_575 () =
  with_db (fun db ->
    seed_non_unique db;
    let seek =
      Printf.sprintf
        "SELECT qty FROM line INNER JOIN cats ON cat = i_id WHERE w = 1 AND sw = 1 AND \
         cat BETWEEN 0 AND %d"
        (n_cat - 1)
    in
    let foil =
      Printf.sprintf
        "SELECT qty FROM line INNER JOIN cats ON cat = i_id WHERE w = 1 AND sw + 0 = 1 \
         AND cat BETWEEN 0 AND %d"
        (n_cat - 1)
    in
    same_rows db ~label:"a non-unique range agrees with its foil" ~seek ~foil;
    Alcotest.(check int)
      "a span of 10 key VALUES over 500 rows is not a window the estimator can size"
      (n_line + n_stock)
      (examined db seek))
;;

(* #595: a PARAMETERISED bound, in both directions, which nothing in this repo
   covered before — which is precisely why the one-ended hole above shipped
   through a full armed suite undetected.

   [range_value] accepts [BE_param], so a bound parameter reaches
   [range_for_index] and produces a [Plan.range] exactly as a literal does. What
   it cannot produce is an estimate: [range_rows_estimate] requires
   [P_lit (L_int _)] at both ends and falls to the flat [range_seek_rows]
   otherwise. So a parameterised window is declined however narrow the value
   passed at run time turns out to be — the planner cannot see it, and #575
   declines what it cannot see. *)
let parameterised_ranges_are_declined_575 () =
  with_db (fun db ->
    seed db;
    List.iter
      (fun (pred, foil_pred, params, label) ->
         let seek = q pred in
         (* #595: the unoptimizable foil, in the parameterised spelling. [si + 0]
            is no recognised range and [sw + 0 = ?] no recognised equality, so
            this is the pre-#528 plan on the same data and the same bindings —
            the #513/#516/#528/#532 soundness invariant, asserted where it was
            never asserted before. *)
         let foil = q foil_pred in
         Alcotest.(check (list (list string)))
           (label ^ ": agrees with the plan that cannot be narrowed")
           (rows_with db foil params)
           (rows_with db seek params);
         Alcotest.(check bool)
           (label ^ ": and the agreement is not between two empty lists")
           true
           (rows_with db seek params <> []);
         Alcotest.(check int)
           (label ^ ": no estimate, so no seek")
           (n_line + n_stock)
           (examined_with db seek params);
         (* The assertion that could not be made with [rows_examined] alone: a
            build side that seeked the window and one that scanned and filtered
            both pull the same TABLE rows here, so only the index-entry count
            says which plan ran. *)
         Alcotest.(check int)
           (label ^ ": and no build-side index entries at all (#595)")
           n_line
           (entries_with db seek params))
      (let p n = Db.V_int (Int64.of_int n) in
       [ ( "sw = 1 AND si BETWEEN ? AND ?"
         , "sw + 0 = 1 AND si + 0 BETWEEN ? AND ?"
         , [ p lo; p hi ]
         , "both ends parameterised" )
       ; ( Printf.sprintf "sw = 1 AND si >= ? AND si <= %d" hi
         , Printf.sprintf "sw + 0 = 1 AND si + 0 >= ? AND si + 0 <= %d" hi
         , [ p lo ]
         , "lower end parameterised" )
       ; ( Printf.sprintf "sw = 1 AND si >= %d AND si <= ?" lo
         , Printf.sprintf "sw + 0 = 1 AND si + 0 >= %d AND si + 0 <= ?" lo
         , [ p hi ]
         , "upper end parameterised" )
       ; ( "sw = 1 AND si >= ?"
         , "sw + 0 = 1 AND si + 0 >= ?"
         , [ p lo ]
         , "one-ended AND parameterised — declined twice over" )
         (* The pinning EQUALITY parameterised too, which is the spelling a
            prepared statement actually produces: [sw = ?] is still recognised
            (a parameter is a fine equality value), so the prefix is pinned and
            only the window is unsizeable. Nothing about the outcome changes,
            and that is what this case is for. *)
       ; ( "sw = ? AND si BETWEEN ? AND ?"
         , "sw + 0 = ? AND si + 0 BETWEEN ? AND ?"
         , [ p 1; p lo; p hi ]
         , "prefix equality parameterised as well" )
       ]))
;;

(* #595's other direction, and the one the issue asks for first: the LITERAL
   spelling of the very same window, bound to the very same numbers, DOES take
   the seek. Without this the case above is compatible with "#575 declines
   everything", which would pass just as green and mean nothing.

   The two queries differ only in whether the bounds arrive as [L_int] or as
   [BE_param], and the gate's answer flips — [range_literal_window_rows] can size
   one and not the other. That is the whole content of the parameterised hole
   #576 records from the cost-model side, pinned here as a plan difference a test
   can see. *)
let the_literal_spelling_of_the_same_window_does_seek_595 () =
  with_db (fun db ->
    seed db;
    let params = [ Db.V_int (Int64.of_int lo); Db.V_int (Int64.of_int hi) ] in
    let bound = q "sw = 1 AND si BETWEEN ? AND ?" in
    let literal = q (Printf.sprintf "sw = 1 AND si BETWEEN %d AND %d" lo hi) in
    Alcotest.(check (list (list string)))
      "the two spellings answer identically"
      (rows_of db literal)
      (rows_with db bound params);
    Alcotest.(check int)
      "literal: the driving seek plus the window"
      (n_line + n_window)
      (entries db literal);
    Alcotest.(check int)
      "parameterised: the driving seek alone"
      n_line
      (entries_with db bound params);
    Alcotest.(check bool)
      "so the same window costs strictly more rows through a prepared statement"
      true
      (examined_with db bound params > examined db literal))
;;

(* #523: several conjuncts may constrain the same end, and the seek must take
   the extremum rather than the first one it meets. The re-emitted conjuncts go
   through the same fold, so this holds on the build side too.

   #575: a lower bound is added, because the two redundant upper bounds alone
   make this a one-ended range and #575 declines those — see
   [one_ended_ranges_are_declined_575]. The fold under test is unchanged: it is
   still two conjuncts constraining the upper end, and the assertion is still
   that the tighter of them is the one that stops the walk. *)
let the_tighter_of_two_bounds_wins () =
  with_db (fun db ->
    seed db;
    let seek =
      q (Printf.sprintf "sw = 1 AND si >= %d AND si <= 400 AND si <= %d" lo hi)
    in
    same_rows
      db
      ~label:"redundant upper bounds agree with the unbounded foil"
      ~seek
      ~foil:
        (q
           (Printf.sprintf
              "sw = 1 AND si + 0 >= %d AND si + 0 <= 400 AND si + 0 <= %d"
              lo
              hi));
    Alcotest.(check int)
      "the tighter of the two bounds is the one that stops the walk"
      (n_line + n_window)
      (examined db seek))
;;

(* The re-basing is the whole point, so it has to be wrong in the visible
   direction if it is wrong at all.

   [line] and [stock] both have 3 columns, so the right table owns combined-row
   ordinals 3..5, and [line.o] is combined ordinal 1 — exactly the right-table
   ordinal of [stock.si]. Were the offset dropped, [o BETWEEN 20 AND 40] would
   be read as a bound on [si] and the build side would read 21 rows instead of
   500, which is the assertion below.

   [line] is created WITHOUT a primary key here, on purpose: with one, [o] is
   the second column of its index and the range narrows the DRIVING side (#517),
   which moves [rows_examined] for a reason that has nothing to do with the
   build side and hides the number this case is about. *)
let a_left_table_range_bounds_nothing () =
  with_db (fun db ->
    exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER)";
    exec
      db
      "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
    exec db "BEGIN";
    for o = 1 to n_line do
      exec
        db
        (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o ((o mod n_per_w) + 1))
    done;
    for w = 1 to n_w do
      for si = 1 to n_per_w do
        exec
          db
          (Printf.sprintf "INSERT INTO stock VALUES (%d, %d, %d)" w si ((w * 1000) + si))
      done
    done;
    exec db "COMMIT";
    let seek = q (Printf.sprintf "sw = 1 AND o BETWEEN %d AND %d" lo hi) in
    let foil = q (Printf.sprintf "sw = 1 AND o + 0 BETWEEN %d AND %d" lo hi) in
    same_rows db ~label:"a left-table range changes no answer" ~seek ~foil;
    Alcotest.(check bool)
      "and the agreement is not between two empty lists"
      true
      (rows_of db seek <> []);
    (* #575: with no range to bound it this is a bare prefix pin, which the
       decision declines — so the build side now scans rather than walking the
       pinned prefix.  Either way the left-table range contributed nothing,
       which is what this case is for. *)
    Alcotest.(check int)
      "the build side scans: no range survived re-basing to bound a seek"
      (n_line + n_stock)
      (examined db seek))
;;

(* A range on a right-table column that is NOT the index column following the
   pinned prefix cannot bound anything: [qty] is not in [stock]'s key at all.
   This is the TPC-C StockLevel shape ([s_quantity < ?]), which #532 explicitly
   does not help. *)
let a_range_off_the_index_bounds_nothing () =
  with_db (fun db ->
    seed db;
    let seek = q "sw = 1 AND qty < 1100" in
    same_rows
      db
      ~label:"an off-index range changes no answer"
      ~seek
      ~foil:(q "sw = 1 AND qty + 0 < 1100");
    (* #575: as above — an off-index range leaves a bare prefix pin, which is
       declined and scans. *)
    Alcotest.(check int)
      "the build side scans: an off-index range bounds no seek"
      (n_line + n_stock)
      (examined db seek))
;;

(* Without a pinned prefix there is no index to bound: [si] is [stock]'s SECOND
   key column, so a range on it alone reaches no seek. The range must not be
   taken as though it addressed the first column. *)
let a_range_without_a_pinned_prefix_still_scans () =
  with_db (fun db ->
    seed db;
    let seek = q (Printf.sprintf "si BETWEEN %d AND %d" lo hi) in
    same_rows
      db
      ~label:"an unpinned range changes no answer"
      ~seek
      ~foil:(q (Printf.sprintf "si + 0 BETWEEN %d AND %d" lo hi));
    Alcotest.(check int) "a full scan of stock" (n_line + n_stock) (examined db seek))
;;

(* LEFT JOIN takes the same treatment for the reason #516 settled on: a narrowed
   build side null-extends left rows a full scan would have matched, those rows
   carry NULL in the very columns the narrowing conjuncts test, and neither
   [sw = 1] nor [si BETWEEN …] is ever true of NULL, so the post-join filter
   drops them exactly as it dropped the wider rows they replaced. *)
let left_join_agrees_with_its_foil () =
  with_db (fun db ->
    seed db;
    let ljq pred =
      Printf.sprintf
        "SELECT o, qty FROM line LEFT JOIN stock ON si = i_id WHERE w = 1 AND %s"
        pred
    in
    let seek = ljq (Printf.sprintf "sw = 1 AND si BETWEEN %d AND %d" lo hi) in
    let foil = ljq (Printf.sprintf "sw + 0 = 1 AND si + 0 BETWEEN %d AND %d" lo hi) in
    same_rows
      db
      ~label:"LEFT JOIN: bounded build side agrees with a full scan"
      ~seek
      ~foil;
    Alcotest.(check bool)
      "and the agreement is not between two empty lists"
      true
      (rows_of db seek <> []);
    Alcotest.(check bool)
      (Printf.sprintf "and it is cheaper (%d < %d)" (examined db seek) (examined db foil))
      true
      (examined db seek < examined db foil))
;;

(* The general-ON path builds a cartesian hash join wrapped in a filter. Its
   build side goes through the same [build_side], so it takes the bound too, and
   the soundness argument does not depend on the ON predicate's shape. *)
let general_on_predicate_build_side_is_bounded () =
  with_db (fun db ->
    seed db;
    let g pred =
      Printf.sprintf
        "SELECT qty FROM line INNER JOIN stock ON si > i_id WHERE w = 1 AND o <= 5 AND %s"
        pred
    in
    let seek = g (Printf.sprintf "sw = 1 AND si BETWEEN %d AND %d" lo hi) in
    let foil = g (Printf.sprintf "sw = 1 AND si + 0 BETWEEN %d AND %d" lo hi) in
    same_rows db ~label:"cartesian build side agrees with its unbounded foil" ~seek ~foil;
    Alcotest.(check bool)
      "and the agreement is not between two empty lists"
      true
      (rows_of db seek <> []);
    let n_seek = examined db seek
    and n_foil = examined db foil in
    Alcotest.(check bool)
      (Printf.sprintf "examined %d is below the unbounded plan's %d" n_seek n_foil)
      true
      (n_seek < n_foil))
;;

(* ------------------------------------------------------------------ *)
(* The strategy flip the range bound causes                             *)
(* ------------------------------------------------------------------ *)

(* Bounding the build side does not only change what it reads — it changes which
   join strategy is chosen, and in the direction that is easy to state
   backwards. [probe_is_worth_it] takes the nested-loop probe iff
   [driving_rows <= right_rows / 8], with R on the RIGHT of the comparison, so a
   SMALLER R makes the probe HARDER to justify. A range-bounded build side
   shrinks R, so it moves joins towards the HASH JOIN, across the whole band
   [nlj_min_driving_rows < D <= N/8].

   That band is exactly where #520/#526 measured a wrong choice at 36x, so it
   needs pinning: [rows_examined] tells the two strategies apart the way
   test_join_cost_model_520.ml does — a probe reads one right row per driving
   row, a hash join reads its whole build side once. Neither the row-equality
   nor the [examined <= foil] assertions above can see it.

   20,000 stock rows against 1,200 driving rows: without a range R is 20,000 and
   [1200 <= 2500] keeps the probe, which is the reference plan here. *)
let n_big_stock = 20_000
let n_big_line = 1_200

(* #606: derived from the same ratio the gate uses, not written as a literal.
   It was [100] against a 20,000-row table, which is EXACTLY
   [n_big_stock / build_side_seek_break_even_ratio] — zero margin, so a change to
   the population or the ratio would silently flip this case from "seeks" to
   "declines" and the assertion below would start pinning the opposite plan while
   still passing for the wrong reason. Half the budget leaves room in the
   direction that matters. *)
let big_window = n_big_stock / 200 / 2

let seed_big db =
  exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
  exec db "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
  exec db "BEGIN";
  (* Every driving row matches one stock row inside the small window, so the
     probe does a full row's work per driving row and the two strategies are
     compared doing the same job. *)
  for o = 1 to n_big_line do
    exec
      db
      (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o (lo + (o mod big_window)))
  done;
  for si = 1 to n_big_stock do
    exec db (Printf.sprintf "INSERT INTO stock VALUES (1, %d, %d)" si (si * 10))
  done;
  exec db "COMMIT"
;;

(* A narrow window: R falls to the [range_seek_rows] floor of 100, [1200 <= 12]
   fails, and the join moves from probe to hash join. It is the right move — the
   hash join reads 100 build rows where the probe did 1,200 seeks, measured on
   disk at 2.8x faster (test/bench_build_side_strategy_532.ml). *)
let a_narrow_window_moves_the_join_to_a_hash_join () =
  with_db (fun db ->
    seed_big db;
    let window = Printf.sprintf "BETWEEN %d AND %d" lo (lo + big_window - 1) in
    let seek = q ("sw = 1 AND si " ^ window) in
    let foil = q ("sw = 1 AND si + 0 " ^ window) in
    same_rows db ~label:"the flipped plan agrees with the probe it replaced" ~seek ~foil;
    Alcotest.(check int)
      "unranged: a probe — one stock row per driving row"
      (n_big_line * 2)
      (examined db foil);
    Alcotest.(check int)
      "ranged: a hash join over the 100-row window"
      (n_big_line + big_window)
      (examined db seek))
;;

(* A window as wide as the table must NOT flip. This is the case the flat
   [range_seek_rows = 100] got wrong: it estimated R = 100 for a window covering
   20,000 rows, chose the hash join, and measured 9.00x SLOWER than the probe on
   disk (bench_build_side_strategy_532.ml, W=20000 D=1200). [range_rows_estimate]
   reads the span off the literal bounds instead, R stays 20,000, [1200 <= 2500]
   holds and the probe survives.

   The assertion is the whole point: 2,400 is the probe, and the plan this
   guards against would examine 1,200 + 19,981. *)
let a_window_as_wide_as_the_table_keeps_the_probe () =
  with_db (fun db ->
    seed_big db;
    let window = Printf.sprintf "BETWEEN %d AND %d" lo (lo + n_big_stock - 1) in
    let seek = q ("sw = 1 AND si " ^ window) in
    let foil = q ("sw = 1 AND si + 0 " ^ window) in
    same_rows db ~label:"the wide window agrees with its unrecognised foil" ~seek ~foil;
    Alcotest.(check int)
      "still a probe — one stock row per driving row, not a 19,981-row build"
      (n_big_line * 2)
      (examined db seek);
    Alcotest.(check int) "as the foil is" (n_big_line * 2) (examined db foil))
;;

(* The span is only readable off literal bounds. A parameter says nothing about
   how wide the window is, so the estimate keeps the flat constant and the plan
   is exactly the one it was before #532 touched the estimator — here, the hash
   join a 100-row R chooses, even though the parameters happen to describe the
   whole table. Deliberate: the alternative is guessing.

   #575 takes that reasoning one step further: if the estimate is a flat constant
   then there is nothing to consult, so the build side does not seek either — and
   THAT MOVES THE STRATEGY, which is worth stating plainly because it is the
   direction [build_side]'s doc warns reads backwards.

   [estimate_rows] takes R from the build-side op. A seeked build side carrying a
   range answered [range_seek_rows] = 100, and [probe_is_worth_it] takes the
   probe iff [driving_rows <= right_rows / 8] — so R = 100 made 1,200 driving
   rows fail 1200 <= 12 and the plan was a hash join. Declining the seek makes
   the build side an [Op_seq_scan], R becomes the table's 20,001, and 1200 <=
   2500 now HOLDS: this query is a nested-loop probe again, exactly as it was
   before #532 touched the estimator.

   That is the better plan by #520's own measurements — 2,400 rows examined
   against the hash join's 21,200 — but it is a plan change, not just an access
   path change, and #575's issue text did not anticipate it. Recorded here rather
   than left for someone to trip over. *)
let a_parameterised_window_keeps_the_flat_estimate () =
  with_db (fun db ->
    seed_big db;
    let sql =
      "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 1 AND si \
       BETWEEN ? AND ?"
    in
    let rows, st =
      unwrap
        (run
           (let open Lwt.Syntax in
            let* st = Db.prepare db sql in
            match st with
            | Error e -> Lwt.return (Error e)
            | Ok st ->
              let* r =
                Db.iter_with_stats
                  st
                  ~params:
                    [ Db.V_int (Int64.of_int lo); Db.V_int (Int64.of_int n_big_stock) ]
              in
              (match r with
               | Error e -> Lwt.return (Error e)
               | Ok (stream, stats) ->
                 let* rows = Lwt_stream.to_list stream in
                 Lwt.return (Ok (List.length rows, stats)))))
    in
    Alcotest.(check int) "every driving row still joins" n_big_line rows;
    Alcotest.(check int)
      "#575 declines the seek, R rises to the table, and the probe wins again"
      (n_big_line * 2)
      st.Granary.Db.rows_examined)
;;

(* ------------------------------------------------------------------ *)
(* Property: no window changes an answer                                *)
(* ------------------------------------------------------------------ *)

(* A bound is a narrowing of what is READ. Over arbitrary windows — empty ones,
   ones that fall off both ends of the table, ones that cover it — the seeked
   spelling must return exactly what the unbounded foil returns, and never read
   more than it. A smaller population keeps the generator affordable; it is
   still a hash join for the same reason (300 driving rows is under the #520
   floor, so drive it above with 1,100). *)
let prop_window_narrows_but_does_not_change =
  QCheck.Test.make
    ~count:40
    ~name:"#532: every window agrees with its unbounded foil and reads no more"
    QCheck.(pair (int_range (-5) 60) (int_range (-5) 60))
    (fun (a, b) ->
       let lo = min a b
       and hi = max a b in
       with_db (fun db ->
         exec
           db
           "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
         exec
           db
           "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, \
            si))";
         exec db "BEGIN";
         for o = 1 to 1_100 do
           exec
             db
             (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o ((o mod 50) + 1))
         done;
         for w = 1 to 4 do
           for si = 1 to 50 do
             exec
               db
               (Printf.sprintf
                  "INSERT INTO stock VALUES (%d, %d, %d)"
                  w
                  si
                  ((w * 1000) + si))
           done
         done;
         exec db "COMMIT";
         let seek = q (Printf.sprintf "sw = 1 AND si BETWEEN %d AND %d" lo hi) in
         let foil = q (Printf.sprintf "sw = 1 AND si + 0 BETWEEN %d AND %d" lo hi) in
         rows_of db seek = rows_of db foil && examined db seek <= examined db foil))
;;

let () =
  Alcotest.run
    "build side range bound (#532)"
    [ ( "narrowing"
      , [ Alcotest.test_case
            "BETWEEN bounds the build side"
            `Quick
            between_bounds_the_build_side
        ; Alcotest.test_case
            "inequalities bound the build side"
            `Quick
            inequalities_bound_the_build_side
        ; Alcotest.test_case
            "one-ended ranges are declined (#575)"
            `Quick
            one_ended_ranges_are_declined_575
        ; Alcotest.test_case
            "a table-wide literal window is declined (#575)"
            `Quick
            a_table_wide_literal_window_is_declined_575
        ; Alcotest.test_case
            "a non-unique index range is declined (#575)"
            `Quick
            a_non_unique_index_range_is_declined_575
        ; Alcotest.test_case
            "parameterised ranges are declined (#575, #595)"
            `Quick
            parameterised_ranges_are_declined_575
        ; Alcotest.test_case
            "the literal spelling of the same window does seek (#595)"
            `Quick
            the_literal_spelling_of_the_same_window_does_seek_595
        ; Alcotest.test_case
            "the break-even boundary is where the gate cuts (#606)"
            `Quick
            the_break_even_boundary_is_where_the_gate_cuts_606
        ; Alcotest.test_case
            "a range before the last index column is declined (#606)"
            `Quick
            a_range_before_the_last_index_column_is_declined_606
        ; Alcotest.test_case
            "a REAL bounded column is declined (#606)"
            `Quick
            a_real_bounded_column_is_declined_606
        ; Alcotest.test_case
            "the tighter of two bounds wins"
            `Quick
            the_tighter_of_two_bounds_wins
        ] )
    ; ( "re-basing"
      , [ Alcotest.test_case
            "a left-table range bounds nothing"
            `Quick
            a_left_table_range_bounds_nothing
        ; Alcotest.test_case
            "a range off the index bounds nothing"
            `Quick
            a_range_off_the_index_bounds_nothing
        ; Alcotest.test_case
            "a range without a pinned prefix still scans"
            `Quick
            a_range_without_a_pinned_prefix_still_scans
        ] )
    ; ( "join shapes"
      , [ Alcotest.test_case
            "LEFT JOIN agrees with its foil"
            `Quick
            left_join_agrees_with_its_foil
        ; Alcotest.test_case
            "general ON predicate build side is bounded"
            `Quick
            general_on_predicate_build_side_is_bounded
        ] )
    ; ( "strategy"
      , [ Alcotest.test_case
            "a narrow window moves the join to a hash join"
            `Quick
            a_narrow_window_moves_the_join_to_a_hash_join
        ; Alcotest.test_case
            "a window as wide as the table keeps the probe"
            `Quick
            a_window_as_wide_as_the_table_keeps_the_probe
        ; Alcotest.test_case
            "a parameterised window keeps the flat estimate"
            `Quick
            a_parameterised_window_keeps_the_flat_estimate
        ] )
    ; ( "properties"
      , List.map QCheck_alcotest.to_alcotest [ prop_window_narrows_but_does_not_change ] )
    ]
;;
