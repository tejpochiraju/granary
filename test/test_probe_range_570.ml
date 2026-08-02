(** #570: a nested-loop probe must take the #532 range bound too.

    #532 gave the hash join's {i build side} a range: [right_table_ranges]
    re-bases the WHERE range conjuncts into right-table ordinals and feeds them
    to [access_path_for_eqs], so a build side whose index prefix is pinned by
    equalities stops its walk at the range's upper bound instead of running to
    the end of the prefix.

    The {i probe} got none of that. [best_probe] was built from [right_eqs]
    alone, [probe_key_for_index] pins each index column from the left row or a
    WHERE equality and stops at the first column pinned by neither, and
    [Plan.probe_part] had no representation for a bound on the column after it.
    So on the same query, the same table and the same index, whether the range
    narrowed the read depended on which strategy the cost model happened to
    pick — and #561 established that the band where the probe survives is
    exactly the band where a wide window makes the hash join 9x worse, i.e. the
    two facts point at each other.

    {2 The shape that shows it}

    The window has to fall on the index column AFTER the probe key's last pinned
    column, so the index needs three columns:

    {v
      stock (sw, si, sub, qty)  PRIMARY KEY (sw, si, sub)
      ... JOIN stock ON si = i_id WHERE sw = 1 AND sub BETWEEN 2 AND 3
    v}

    [sw] is pinned by the WHERE equality and [si] by the left row, so the probe
    key is [(sw, si)] — a strict prefix — and every probe walks all
    [n_sub] entries under it. #570 stops that walk at [sub = 3].

    A two-column index is the case #570's own text calls out as showing nothing:
    there the probe key is the whole key, the residual span is 1, and the range
    can only narrow what is already a point. [a_full_key_probe_has_nothing_left_to_narrow]
    keeps that boundary honest.

    {2 What is asserted}

    [rows_examined] is the load-bearing counter, as everywhere in this family:
    in-memory databases emit no page events, and a narrowed probe is told from a
    wide one by counting the table rows the executor pulled. [index_entries]
    (#546) is asserted alongside it, because after #570 the probe reports its
    walk there too and it is the counter that separates "read fewer rows" from
    "walked fewer entries to read them".

    Every case is paired with a foil the planner cannot narrow ([sub + 0
    BETWEEN …]) and the two must return identical rows — the #513/#516/#528/#532
    invariant: [chain_joins] applies the whole WHERE clause to the joined row,
    so narrowing what a probe reads cannot change which joined rows survive. *)

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

let entries db sql =
  let _, st = stats_of db sql in
  st.Granary.Db.index_entries
;;

(* Prepared-and-bound counterparts. A parameterised bound reaches
   [range_seek_bounds] on the probe path exactly as a literal does — and unlike
   the build side there is no #575/#606 gate in front of it to decline what the
   planner cannot size, so these are the spelling that actually exercises the
   run-time end of the range. *)
let stats_with db sql params =
  unwrap
    (run
       (let open Lwt.Syntax in
        let* st = Db.prepare db sql in
        match st with
        | Error e -> Lwt.return (Error e)
        | Ok st ->
          let* r = Db.iter_with_stats st ~params in
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

let same_rows db ~label ~seek ~foil =
  Alcotest.(check (list (list string))) label (rows_of db foil) (rows_of db seek);
  Alcotest.(check bool)
    (label ^ ": and not between two empty lists")
    true
    (rows_of db seek <> [])
;;

(* ------------------------------------------------------------------ *)
(* The population                                                       *)
(* ------------------------------------------------------------------ *)

(* BELOW [nlj_min_driving_rows] = 1000, so [probe_is_worth_it] short-circuits on
   the floor and the strategy is a nested-loop probe regardless of what the
   range does to R. That matters: #570 is about the probe, and the whole reason
   the asymmetry existed is that the range moves the strategy. Keeping the
   driving side under the floor pins the probe in place so the access path is
   the only thing under test. *)
let n_line = 200
let n_si = 50
let n_sub = 20

(* The window on [sub], the index column after the probe key. *)
let lo = 2
let hi = 3
let n_window = hi - lo + 1

let seed db =
  exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
  exec
    db
    "CREATE TABLE stock (sw INTEGER, si INTEGER, sub INTEGER, qty INTEGER, PRIMARY KEY \
     (sw, si, sub))";
  exec db "BEGIN";
  for o = 1 to n_line do
    exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o ((o mod n_si) + 1))
  done;
  for si = 1 to n_si do
    for sub = 1 to n_sub do
      exec
        db
        (Printf.sprintf
           "INSERT INTO stock VALUES (1, %d, %d, %d)"
           si
           sub
           ((si * 100) + sub))
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

(* The headline. The probe key is [(sw = 1, si = left.i_id)] and the range falls
   on [sub], the next index column.

   Before #570 every probe walked all 20 [sub] entries under its key and pulled
   all 20 rows, leaving the post-join filter to drop 18 of them: 200 + 200*20 =
   4,200 rows examined. After it, 200 + 200*2 = 600. The foil — [sub + 0], no
   recognised range — is the pre-#570 plan on the same data, so it is both the
   correctness oracle and the before number. *)
let a_range_after_the_probe_key_narrows_the_probe () =
  with_db (fun db ->
    seed db;
    let seek = q (Printf.sprintf "sw = 1 AND sub BETWEEN %d AND %d" lo hi) in
    let foil = q (Printf.sprintf "sw = 1 AND sub + 0 BETWEEN %d AND %d" lo hi) in
    same_rows db ~label:"a narrowed probe agrees with its unnarrowed foil" ~seek ~foil;
    Alcotest.(check int)
      "unnarrowed: every sub under each probe key"
      (n_line + (n_line * n_sub))
      (examined db foil);
    Alcotest.(check int)
      "narrowed: only the window's subs"
      (n_line + (n_line * n_window))
      (examined db seek);
    Alcotest.(check int)
      "and the index walk shrinks with it (#546's counter)"
      (n_line + (n_line * n_window))
      (entries db seek))
;;

(* Two inequalities rather than one BETWEEN reach [recognise_range_col_lit]
   through a different arm, and a strict pair is deliberately treated as
   inclusive — [right_table_ranges] re-emits every end as [>=]/[<=]. So
   [sub > 1 AND sub < 4] reads one extra key per end and drops them in the
   filter, which is the documented price. *)
let inequalities_narrow_the_probe () =
  with_db (fun db ->
    seed db;
    List.iter
      (fun (pred, extra, label) ->
         let seek = q (Printf.sprintf "sw = 1 AND %s" pred) in
         let foil = q (Printf.sprintf "sw = 1 AND sub + 0 BETWEEN %d AND %d" lo hi) in
         same_rows db ~label:(label ^ ": agrees with its unnarrowed foil") ~seek ~foil;
         Alcotest.(check int)
           (label ^ ": reads only the window")
           (n_line + (n_line * (n_window + extra)))
           (examined db seek))
      [ Printf.sprintf "sub >= %d AND sub <= %d" lo hi, 0, "inclusive pair"
      ; Printf.sprintf "sub > %d AND sub < %d" (lo - 1) (hi + 1), 2, "strict pair"
      ; Printf.sprintf "%d <= sub AND %d >= sub" lo hi, 0, "reversed operands"
      ])
;;

(* One end only. Unlike the BUILD side — where #575 declines a one-ended range
   because it is #546's open-ended prefix walk in disguise — a probe has nothing
   to decline: it is a seek either way, and a lower bound simply moves its start
   key forward. So this narrows, and the gate that guards the build side has no
   counterpart here. *)
let a_one_ended_range_narrows_the_probe () =
  with_db (fun db ->
    seed db;
    let seek = q (Printf.sprintf "sw = 1 AND sub >= %d" (n_sub - 1)) in
    let foil = q (Printf.sprintf "sw = 1 AND sub + 0 >= %d" (n_sub - 1)) in
    same_rows db ~label:"a lower bound agrees with its foil" ~seek ~foil;
    Alcotest.(check int)
      "and starts the walk at the bound rather than at the prefix"
      (n_line + (n_line * 2))
      (examined db seek);
    Alcotest.(check int)
      "the foil walks the whole prefix under each key"
      (n_line + (n_line * n_sub))
      (examined db foil))
;;

(* An empty window returns nothing and reads nothing, on both spellings. The
   seek's start key is past its own upper bound, so the very first entry the
   cursor lands on is out of range. *)
let an_empty_window_reads_nothing () =
  with_db (fun db ->
    seed db;
    let seek = q "sw = 1 AND sub BETWEEN 40 AND 30" in
    let foil = q "sw = 1 AND sub + 0 BETWEEN 40 AND 30" in
    Alcotest.(check (list (list string)))
      "both spellings answer empty"
      (rows_of db foil)
      (rows_of db seek);
    Alcotest.(check int)
      "and the narrowed one pulls no right rows at all"
      n_line
      (examined db seek))
;;

(* The boundary #570's own text names: with a TWO-column key the probe pins
   every column, the residual span is 1, and a range on nothing-after-the-key
   contributes nothing. Asserting it keeps the win above from being read as
   "ranges always help a probe". *)
let a_full_key_probe_has_nothing_left_to_narrow () =
  with_db (fun db ->
    exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
    exec db "CREATE TABLE two (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
    exec db "BEGIN";
    for o = 1 to n_line do
      exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o ((o mod n_si) + 1))
    done;
    for si = 1 to n_si do
      exec db (Printf.sprintf "INSERT INTO two VALUES (1, %d, %d)" si (si * 100))
    done;
    exec db "COMMIT";
    let g pred =
      Printf.sprintf
        "SELECT qty FROM line INNER JOIN two ON si = i_id WHERE w = 1 AND %s"
        pred
    in
    let seek = g "sw = 1 AND si BETWEEN 2 AND 40" in
    let foil = g "sw = 1 AND si + 0 BETWEEN 2 AND 40" in
    same_rows db ~label:"the full-key probe agrees with its foil" ~seek ~foil;
    Alcotest.(check int)
      "one right row per driving row either way"
      (examined db foil)
      (examined db seek))
;;

(* A LEFT JOIN narrows identically, for the reason #516 settled on: a narrowed
   probe null-extends left rows a wider one would have matched, those rows carry
   NULL in the very column the narrowing conjunct tests, [sub BETWEEN …] is
   never true of NULL, and the post-join filter drops them exactly as it dropped
   the wider rows they replaced. *)
let left_join_agrees_with_its_foil () =
  with_db (fun db ->
    seed db;
    let ljq pred =
      Printf.sprintf
        "SELECT o, qty FROM line LEFT JOIN stock ON si = i_id WHERE w = 1 AND %s"
        pred
    in
    let seek = ljq (Printf.sprintf "sw = 1 AND sub BETWEEN %d AND %d" lo hi) in
    let foil = ljq (Printf.sprintf "sw = 1 AND sub + 0 BETWEEN %d AND %d" lo hi) in
    same_rows db ~label:"LEFT JOIN: a narrowed probe agrees with its foil" ~seek ~foil;
    Alcotest.(check bool)
      (Printf.sprintf "and reads less (%d < %d)" (examined db seek) (examined db foil))
      true
      (examined db seek < examined db foil))
;;

(* A range on a column that is NOT the one after the probe key must contribute
   nothing — narrowing on the wrong column would drop rows, which is the failure
   mode the re-basing exists to avoid. [qty] is no index column at all. *)
let a_range_off_the_index_narrows_nothing () =
  with_db (fun db ->
    seed db;
    let seek = q "sw = 1 AND qty BETWEEN 200 AND 400" in
    let foil = q "sw = 1 AND qty + 0 BETWEEN 200 AND 400" in
    same_rows db ~label:"a non-index range agrees with its foil" ~seek ~foil;
    Alcotest.(check int) "and narrows nothing" (examined db foil) (examined db seek))
;;

(* The re-basing has to be wrong in the visible direction if it is wrong at all.
   [line] has 3 columns and [stock] 4, so the right table owns combined ordinals
   3..6; [line.i_id] is combined ordinal 2, which is [stock.sub]'s right-table
   ordinal. A range on the LEFT table's column must not be mistaken for one on
   the right's. *)
let a_left_table_range_narrows_nothing () =
  with_db (fun db ->
    seed db;
    let seek = q (Printf.sprintf "sw = 1 AND i_id BETWEEN %d AND %d" lo hi) in
    let foil = q (Printf.sprintf "sw = 1 AND i_id + 0 BETWEEN %d AND %d" lo hi) in
    same_rows db ~label:"a left-table range agrees with its foil" ~seek ~foil;
    Alcotest.(check int)
      "and bounds nothing on the right"
      (examined db foil)
      (examined db seek))
;;

(* ------------------------------------------------------------------ *)
(* Run-time bounds: parameters, NaN, NULL                               *)
(* ------------------------------------------------------------------ *)

(* A PARAMETERISED probe range. On the build side #575/#606 decline this,
   because [range_rows_estimate] cannot size a window it cannot read and the gate
   refuses to seek on a constant it made up. The probe has no such gate and needs
   none — a probe is a seek either way, so a bound it cannot size still only
   moves the start key forward.

   That makes the parameterised spelling the one that reaches [range_seek_bounds]
   at RUN time on this path, and until now nothing exercised it. The assertion is
   that it narrows identically to the literal, and answers identically to the
   foil. *)
let a_parameterised_probe_range_narrows () =
  with_db (fun db ->
    seed db;
    let bound = q "sw = 1 AND sub BETWEEN ? AND ?" in
    let literal = q (Printf.sprintf "sw = 1 AND sub BETWEEN %d AND %d" lo hi) in
    let foil = q (Printf.sprintf "sw = 1 AND sub + 0 BETWEEN %d AND %d" lo hi) in
    let params = [ Db.V_int (Int64.of_int lo); Db.V_int (Int64.of_int hi) ] in
    Alcotest.(check (list (list string)))
      "the bound spelling agrees with the foil"
      (rows_of db foil)
      (rows_with db bound params);
    Alcotest.(check bool)
      "and not between two empty lists"
      true
      (rows_with db bound params <> []);
    Alcotest.(check int)
      "and narrows exactly as the literal does"
      (examined db literal)
      (examined_with db bound params);
    Alcotest.(check int)
      "which is the window, not the whole prefix"
      (n_line + (n_line * n_window))
      (examined_with db bound params))
;;

(* NaN as a probe bound, both ends.

   CLAUDE.md's #536 decision: NaN is a value here and sorts BELOW every number,
   in the comparator ([Exec.cmp_result] via [Float.compare]) and in the key
   encoding (the single 0x00 byte) alike. So [sub >= NaN] admits every row and
   [sub <= NaN] admits none — a divergence from SQLite in both directions, and
   sound only because the seek and its residual predicate agree about where NaN
   sits relative to a number.

   [nan_and_infinite_bounds_are_sound] in [test_range_bound_517.ml] pins that for
   [stream_index_lookup]. It does NOT pin it here: #570 made [nlj_probe_left] a
   second consumer of [range_seek_bounds], and a second consumer is a second
   place the agreement can be broken. The foil is the load-bearing half — if the
   seek and the predicate ever disagreed, the two columns would part company. *)
let nan_probe_bounds_agree_with_the_predicate () =
  with_db (fun db ->
    seed db;
    let nan = [ Db.V_real Float.nan ] in
    let lower = q "sw = 1 AND sub >= ?"
    and lower_foil = q "sw = 1 AND sub + 0 >= ?"
    and upper = q "sw = 1 AND sub <= ?"
    and upper_foil = q "sw = 1 AND sub + 0 <= ?" in
    Alcotest.(check (list (list string)))
      "NaN lower bound: agrees with the unoptimizable foil"
      (rows_with db lower_foil nan)
      (rows_with db lower nan);
    Alcotest.(check int)
      "NaN lower bound: NaN sorts below every number, so every row qualifies"
      (n_line * n_sub)
      (List.length (rows_with db lower nan));
    Alcotest.(check (list (list string)))
      "NaN upper bound: agrees with the unoptimizable foil"
      (rows_with db upper_foil nan)
      (rows_with db upper nan);
    Alcotest.(check int)
      "NaN upper bound: nothing sorts below NaN, so no row does"
      0
      (List.length (rows_with db upper nan)))
;;

(* NULL in the BOUNDED column. [sub] is a primary-key member above and so NOT
   NULL (#530); this needs a secondary index over a nullable column, which is
   also the shape where a probe is reached through something other than the PK.

   {b The WHERE clause must pin [sw].} [nul_ix] is [(sw, si, sub)] and
   [probe_key_for_index] walks the index columns in order, stopping at the first
   one pinned by neither the left row nor a WHERE equality — so without [sw = 1]
   there is no probe key at all, the planner takes a hash join over a sequential
   scan, and both spellings become the SAME PLAN. An earlier version of this case
   omitted it and therefore asserted that two identical plans return identical
   rows: green, and worth nothing. It is recorded here because the omission is
   invisible in the query text and the assertion that caught it is the row count,
   not the row values.

   With [sw] pinned, each probe covers one [si] and walks that key's 21 entries —
   20 real subs plus one NULL. NULL encodes to the same [0x00] byte NaN gets
   (#536), so the NULL entries sort BELOW every integer key:

     - a LOWER bound at the first real key starts the walk past them, so the
       seek reads 20 entries per probe where the foil reads 21. That difference
       — 4,200 against 4,400 — is exactly the observable a seek that mistook a
       NULL for a key would move, and it is the reason this case asserts on
       counters rather than only on rows.
     - an UPPER bound at the first real key does NOT skip them: they are inside
       the range by the encoding, so the seek walks them, fetches the rows, and
       the post-join filter drops them on the predicate. That is the direction
       that proves the NARROWING is not what removes NULLs — the WHERE clause is.

   Both must still answer identically to the foil, since a NULL never satisfies
   an inequality under three-valued logic. *)
let n_null_per_si = 1
let n_nul_entries = n_sub + n_null_per_si

let null_in_the_bounded_column_is_dropped_by_both () =
  with_db (fun db ->
    exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
    exec db "CREATE TABLE nul (sw INTEGER, si INTEGER, sub INTEGER, qty INTEGER)";
    exec db "CREATE INDEX nul_ix ON nul (sw, si, sub)";
    exec db "BEGIN";
    for o = 1 to n_line do
      exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o ((o mod n_si) + 1))
    done;
    for si = 1 to n_si do
      (* One NULL-subbed row per si, alongside the real ones. *)
      exec db (Printf.sprintf "INSERT INTO nul VALUES (1, %d, NULL, %d)" si (si * 100));
      for sub = 1 to n_sub do
        exec
          db
          (Printf.sprintf
             "INSERT INTO nul VALUES (1, %d, %d, %d)"
             si
             sub
             ((si * 100) + sub))
      done
    done;
    exec db "COMMIT";
    (* [sub] is projected so a leaked NULL is observable in the ROWS as well as
       in the counters; the earlier version projected [qty], which is never NULL
       in this population, so its "nothing leaked" assertion could not fail. *)
    let g pred =
      Printf.sprintf
        "SELECT sub, qty FROM line INNER JOIN nul ON si = i_id WHERE w = 1 AND sw = 1 \
         AND %s"
        pred
    in
    (* The foil pins [sw] too, so it takes the same nested-loop probe and differs
       from the seek ONLY in whether the range narrows it. *)
    let foil_of pred =
      Printf.sprintf
        "SELECT sub, qty FROM line INNER JOIN nul ON si = i_id WHERE w = 1 AND sw = 1 \
         AND %s"
        pred
    in
    let unnarrowed = n_line + (n_line * n_nul_entries) in
    List.iter
      (fun (pred, foil_pred, walked, label) ->
         let seek = g pred
         and foil = foil_of foil_pred in
         Alcotest.(check (list (list string)))
           (label ^ ": the narrowed probe agrees with its foil")
           (rows_of db foil)
           (rows_of db seek);
         Alcotest.(check bool)
           (label ^ ": and not between two empty lists")
           true
           (rows_of db seek <> []);
         Alcotest.(check bool)
           (label ^ ": no NULL sub survived the join")
           true
           (not (List.exists (List.mem "NULL") (rows_of db seek)));
         Alcotest.(check int)
           (label ^ ": the probe really ran and read what the bound says")
           (n_line + (n_line * walked))
           (examined db seek);
         Alcotest.(check int)
           (label ^ ": index entries agree with the rows fetched")
           (n_line + (n_line * walked))
           (entries db seek);
         Alcotest.(check int)
           (label ^ ": the foil walks every entry under the key, NULLs included")
           unnarrowed
           (examined db foil))
      [ ( Printf.sprintf "sub BETWEEN %d AND %d" lo hi
        , Printf.sprintf "sub + 0 BETWEEN %d AND %d" lo hi
        , n_window
        , "two-ended window" )
        (* 20 of 21: the NULL entry is below the bound and is never walked. *)
      ; "sub >= 1", "sub + 0 >= 1", n_sub, "lower bound at the first real key"
        (* 2 of 21: the NULL entry IS walked — it sorts inside the range — and
           the predicate is what drops its row. *)
      ; "sub <= 1", "sub + 0 <= 1", n_null_per_si + 1, "upper bound at the first real key"
      ])
;;

(* ------------------------------------------------------------------ *)
(* Property: no window changes an answer                                *)
(* ------------------------------------------------------------------ *)

(* The soundness invariant over arbitrary windows — empty ones, ones off both
   ends, ones covering the column. The narrowed probe must answer exactly what
   the foil answers and never read more. *)
let prop_probe_window_narrows_but_does_not_change =
  QCheck.Test.make
    ~count:30
    ~name:"#570: every probe window agrees with its unnarrowed foil and reads no more"
    QCheck.(pair (int_range (-3) 25) (int_range (-3) 25))
    (fun (a, b) ->
       let lo = min a b
       and hi = max a b in
       with_db (fun db ->
         seed db;
         let seek = q (Printf.sprintf "sw = 1 AND sub BETWEEN %d AND %d" lo hi) in
         let foil = q (Printf.sprintf "sw = 1 AND sub + 0 BETWEEN %d AND %d" lo hi) in
         rows_of db seek = rows_of db foil && examined db seek <= examined db foil))
;;

let () =
  Alcotest.run
    "nested-loop probe range bound (#570)"
    [ ( "narrowing"
      , [ Alcotest.test_case
            "a range after the probe key narrows the probe"
            `Quick
            a_range_after_the_probe_key_narrows_the_probe
        ; Alcotest.test_case
            "inequalities narrow the probe"
            `Quick
            inequalities_narrow_the_probe
        ; Alcotest.test_case
            "a one-ended range narrows the probe"
            `Quick
            a_one_ended_range_narrows_the_probe
        ; Alcotest.test_case
            "an empty window reads nothing"
            `Quick
            an_empty_window_reads_nothing
        ; Alcotest.test_case
            "a full-key probe has nothing left to narrow"
            `Quick
            a_full_key_probe_has_nothing_left_to_narrow
        ] )
    ; ( "re-basing"
      , [ Alcotest.test_case
            "a range off the index narrows nothing"
            `Quick
            a_range_off_the_index_narrows_nothing
        ; Alcotest.test_case
            "a left-table range narrows nothing"
            `Quick
            a_left_table_range_narrows_nothing
        ] )
    ; ( "join shapes"
      , [ Alcotest.test_case
            "LEFT JOIN agrees with its foil"
            `Quick
            left_join_agrees_with_its_foil
        ] )
    ; ( "run-time bounds"
      , [ Alcotest.test_case
            "a parameterised probe range narrows"
            `Quick
            a_parameterised_probe_range_narrows
        ; Alcotest.test_case
            "NaN probe bounds agree with the predicate (#536)"
            `Quick
            nan_probe_bounds_agree_with_the_predicate
        ; Alcotest.test_case
            "NULL in the bounded column is dropped by both"
            `Quick
            null_in_the_bounded_column_is_dropped_by_both
        ] )
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_probe_window_narrows_but_does_not_change ] )
    ]
;;
