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
      ... JOIN stock ON si = i_id WHERE sw = 1 AND si BETWEEN 20 AND 40
    v}

    seeks [sw = 1] and then reads every row of warehouse 1 rather than stopping
    at [si = 40].

    [right_table_ranges] is the missing half. It shares [rebase_right_col] with
    [right_table_eqs] — the two must agree about which slots the right table owns
    — and re-emits each recognised end as a canonical [col >= v] / [col <= v]
    conjunct over the re-based ordinal, which round-trips through
    [recognise_range_col_lit] to the same ends.

    [rows_examined] is the load-bearing assertion, for the reason
    [test_hash_join_build_seek_528.ml] gives: in-memory databases emit no page
    events, and a bounded build side is told from an unbounded one by counting
    the base rows the executor pulled.

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

(* The window the range selects: [si] in 20..40 inclusive. *)
let lo = 20
let hi = 40
let n_window = hi - lo + 1

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
   [si BETWEEN 20 AND 40] bounds the next index column (#532).

   Three plans, three build-side costs:
     - unpinned foil       [sw + 0 = 1]  reads all 2,000 stock rows
     - #528, prefix only   [sw = 1]      reads warehouse 1's 500
     - #532, prefix+range  ... BETWEEN   reads the 21-row window *)
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
    Alcotest.(check int)
      "prefix only (#528): the driving side plus warehouse 1"
      (n_line + n_per_w)
      (examined db foil);
    Alcotest.(check int)
      "prefix + range (#532): the driving side plus the 21-row window"
      (n_line + n_window)
      (examined db seek))
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

(* One end only. The lower bound moves the start key forward, the upper bound
   stops the walk early; neither needs the other. *)
let one_ended_ranges_bound_the_build_side () =
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
      "upper bound: reads si in 1..40"
      (n_line + hi)
      (examined db upper);
    Alcotest.(check int)
      "lower bound: reads si in 20..500"
      (n_line + (n_per_w - lo + 1))
      (examined db lower))
;;

(* #523: several conjuncts may constrain the same end, and the seek must take
   the extremum rather than the first one it meets. The re-emitted conjuncts go
   through the same fold, so this holds on the build side too. *)
let the_tighter_of_two_bounds_wins () =
  with_db (fun db ->
    seed db;
    let seek = q (Printf.sprintf "sw = 1 AND si <= 400 AND si <= %d" hi) in
    same_rows
      db
      ~label:"redundant upper bounds agree with the unbounded foil"
      ~seek
      ~foil:(q (Printf.sprintf "sw = 1 AND si + 0 <= 400 AND si + 0 <= %d" hi));
    Alcotest.(check int)
      "the tighter of the two bounds is the one that stops the walk"
      (n_line + hi)
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
    Alcotest.(check int)
      "the build side still walks the whole pinned prefix"
      (n_line + n_per_w)
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
    Alcotest.(check int)
      "the build side still walks the whole pinned prefix"
      (n_line + n_per_w)
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
            "one-ended ranges bound the build side"
            `Quick
            one_ended_ranges_bound_the_build_side
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
    ; ( "properties"
      , List.map QCheck_alcotest.to_alcotest [ prop_window_narrows_but_does_not_change ] )
    ]
;;
