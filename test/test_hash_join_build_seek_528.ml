(** #528: the build side of a hash join must be allowed to seek.

    [plan_join] built its hash-join fallback as
    [right = make_scan bj.right_meta] — unconditionally a full table scan, even
    when the WHERE clause pinned a leading prefix of the right table's primary
    key. The driving side got its access path in #513 and the nested-loop probe
    learned composite keys in #516; the build side got neither. In TPC-C
    StockLevel that is a scan of all 100,000 rows of [stock] across every
    warehouse, when [s_w_id = ?] makes one warehouse's stock directly seekable.

    [right_table_eqs] already re-bases the WHERE equalities that pin right-table
    columns to right-table ordinals — that is what {!best_probe} consumes. The
    same list now feeds the shared access-path chooser, so the build side is an
    [Op_index_lookup] (or [Op_rowid_lookup]) whenever one is available.

    Soundness is the invariant #513 and #516 already rest on: [chain_joins]
    applies the whole WHERE clause to the joined row, so restricting what the
    build side reads cannot change which joined rows survive. The equalities come
    only from the top-level AND spine — [conjuncts] never splits an [OR].

    LEFT JOIN gets the treatment #516 settled on rather than an exclusion. A
    narrowed build side null-extends left rows a full scan would have matched;
    those rows have NULL in the very column the narrowing conjunct tests, and
    [col = value] is never true of NULL, so the post-join filter drops them —
    exactly as it dropped the wider rows they replaced.

    [rows_examined] is the load-bearing assertion. In-memory databases emit no
    page events, and a seeked build side is told from a scanned one by counting
    the base rows the executor pulled.

    {2 #575: half of the above no longer holds}

    #561 measured this optimisation on disk and found the premise wrong for the
    shape it was written for. At 100,000 build-side rows a seek whose pinned
    prefix selects the whole table costs 393,980 pager reads against the scan's
    93,067 and 2.6-3.5x the wall time, breaking even only near 1% selectivity —
    and the 100% and 1% cases are the SAME PLAN over the same-sized table, with
    the same [table_rows_estimate], differing only in a value distribution the
    catalog does not record (#576 is the statistic that would). [rows_examined]
    reports 130,000 for both, so the number this file leans on cannot see the
    cost at all.

    #575 decided option B: take the build-side seek only where it reaches at most
    one row — a rowid alias, or a UNIQUE index with every key column pinned — or
    where a #532 range bound has BOTH ends as integer literals and the estimator
    says the window is smaller than the table. Every bare prefix pin is declined
    and scans. That gives up the measured 6% and 1% wins to avoid the measured
    2.6-3.5x regression at 100%.

    It does NOT give up TPC-C StockLevel, contrary to what the first version of
    this header said. That query plans as [NestedLoopJoin(stock)] over
    [IndexLookup(order_line)] and never reaches [build_side] at all — its driving
    seek estimates 100 rows, which is below [nlj_min_driving_rows] = 1000, so
    [probe_is_worth_it] short-circuits and the probe always wins. Checked by
    EXPLAIN on the real schema, identical before and after #575. The 1,580 ms to
    19.8 ms StockLevel win belongs to #516's probe (#513), not to #528.

    So the cases below split in two. The CORRECTNESS cases are untouched in
    substance — the soundness argument above still has to hold, both because the
    surviving seeks rely on it and because #576 is expected to bring the declined
    band back. The COST cases now assert that the seek and its foil are the same
    plan, and say which measurement retired them. *)

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
(* The population                                                       *)
(* ------------------------------------------------------------------ *)

(* [n_line] sits above the planner's 1000-row absolute floor (#520), so the
   strategy choice is the ratio test and this join really is a hash join — the
   only shape this issue is about. Below the floor a probe is taken
   unconditionally and the build side never runs. *)
let n_line = 1200

(* Four warehouses, so [sw = 1] discards three quarters of [stock]. Kept small
   enough that 1,200 driving rows lose the ratio test against it (1200 > 2000/8)
   and the planner picks the hash join. *)
let n_w = 4
let n_per_w = 500
let n_stock = n_w * n_per_w

(* [line] is the driving side, [stock] the build side keyed on [(sw, si)] — the
   TPC-C StockLevel shape, where the join is on [si] and [sw] comes from the
   WHERE clause. *)
let seed db ?(n_w = n_w) ?(n_per_w = n_per_w) ?(n_line = n_line) ?(i_mod = n_per_w) () =
  exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
  exec db "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
  exec db "BEGIN";
  for o = 1 to n_line do
    exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o ((o mod i_mod) + 1))
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

(* [sw] is pinned by a top-level equality, so the build side can seek stock's
   primary-key prefix. *)
let seek_sql = "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 1"

(* [sw + 0] is not a recognised equality, so this foil's build side can only be a
   full scan. It is the reference for what the unnarrowed plan costs and
   returns. *)
let foil_sql =
  "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw + 0 = 1"
;;

(* ------------------------------------------------------------------ *)
(* The narrowing                                                        *)
(* ------------------------------------------------------------------ *)

(* #575 (option B): the headline case, INVERTED.

   #528 narrowed this build side to warehouse 1's [n_per_w] rows, and that was
   the number this case asserted. #561 then measured the same shape on disk at
   100,000 rows and found the narrowing costs 2.6-3.5x the scan it replaces when
   the pinned prefix selects the whole table, breaking even only near 1%
   selectivity — and nothing in the catalog distinguishes those two, since they
   are the same plan over the same-sized table with the same
   [table_rows_estimate].

   #575 chose to decline the whole band rather than keep a known 2.6-3.5x
   pessimisation on a query anyone can write ([WHERE tenant_id = 1] on a
   single-tenant-heavy table). So a bare prefix pin scans, and this case now
   pins THAT: the seek and its foil are the same plan.

   What survives is the pair of cases below — a rowid alias and a full unique-key
   pin — which reach at most one row and cannot lose to a scan whatever the
   distribution. #576 is the statistic that would let the middle band be judged
   rather than declined. *)
let bare_prefix_pin_is_declined_575 () =
  with_db (fun db ->
    seed db ();
    same_rows
      db
      ~label:"declined and scanned build sides agree"
      ~seek:seek_sql
      ~foil:foil_sql;
    Alcotest.(check int)
      "one joined row per driving row"
      n_line
      (List.length (rows_of db seek_sql));
    Alcotest.(check int)
      "the unnarrowed foil reads the whole of stock"
      (n_line + n_stock)
      (examined db foil_sql);
    Alcotest.(check int)
      "and so does the pinned prefix, because #575 declines it"
      (n_line + n_stock)
      (examined db seek_sql))
;;

(* The half of #528 #575 kept, on the composite-key shape: pin the WHOLE of
   [stock]'s unique primary key and the build side reads exactly one row. It is a
   point, so it is right whatever the data looks like.

   [si = 5] pins the join column too, which is what makes it a full-key pin. A
   join whose right table has its whole key pinned is a narrow shape — but it is
   the one that is unambiguously right, and it is what option B keeps. *)
let full_unique_key_pin_still_seeks () =
  with_db (fun db ->
    seed db ();
    let seek =
      "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 1 AND si \
       = 5"
    in
    let foil =
      "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw + 0 = 1 AND \
       si = 5"
    in
    same_rows db ~label:"point seek and scan agree" ~seek ~foil;
    Alcotest.(check bool)
      "and the agreement is not between two empty lists"
      true
      (rows_of db seek <> []);
    Alcotest.(check int)
      "the build side reads exactly the one addressed stock row"
      (n_line + 1)
      (examined db seek))
;;

(* #593: the same point seek on a UNIQUE index over NULLABLE columns.

   [CREATE UNIQUE INDEX ix ON t (a, b)] with no NOT NULL is an ordinary schema,
   and a full pin of it still reaches at most one row — so it must seek, exactly
   as the PRIMARY KEY spelling above does.

   This case exists because folding this gate into [seek_is_unique_point]
   silently imported that function's [all_not_null] test and turned this seek
   into a full scan — a regression against `main`, invisible to every other case
   in this file because they all pin [stock]'s PRIMARY KEY, whose columns are
   implicitly NOT NULL since #530.

   [all_not_null] is calibrated for a different question. Its own doc says it is
   there to stop the CARDINALITY ESTIMATE answering 1 anti-conservatively, and
   explains in the same breath why reachability does not need it: a key is only
   ever pinned by [col = value], which is never true of NULL, so a NULL-keyed
   seek returns no rows at all. NULLs cannot make a seek reach MORE than one
   row, which is the only thing this gate asks. *)
let nullable_unique_key_pin_still_seeks () =
  with_db (fun db ->
    exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
    (* Deliberately no NOT NULL, and deliberately not a PRIMARY KEY. *)
    exec db "CREATE TABLE un (uw INTEGER, us INTEGER, qty INTEGER)";
    exec db "CREATE UNIQUE INDEX un_ix ON un (uw, us)";
    exec db "BEGIN";
    for o = 1 to n_line do
      exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o ((o mod 500) + 1))
    done;
    for w = 1 to n_w do
      for us = 1 to n_per_w do
        exec
          db
          (Printf.sprintf "INSERT INTO un VALUES (%d, %d, %d)" w us ((w * 1000) + us))
      done
    done;
    exec db "COMMIT";
    let seek =
      "SELECT qty FROM line INNER JOIN un ON us = i_id WHERE w = 1 AND uw = 1 AND us = 5"
    and foil =
      "SELECT qty FROM line INNER JOIN un ON us = i_id WHERE w = 1 AND uw + 0 = 1 AND us \
       = 5"
    in
    same_rows db ~label:"nullable unique point seek agrees with its scan" ~seek ~foil;
    Alcotest.(check bool)
      "and the agreement is not between two empty lists"
      true
      (rows_of db seek <> []);
    Alcotest.(check int)
      "a UNIQUE index on nullable columns still seeks its full-key pin"
      (n_line + 1)
      (examined db seek))
;;

(* A NON-unique index gets no such exemption even when its every column is
   pinned: the pin reaches a run of duplicates of unbounded length, which is the
   same unknown-selectivity problem in a different spelling. *)
let full_non_unique_key_pin_is_declined_575 () =
  with_db (fun db ->
    exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
    exec db "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER)";
    exec db "CREATE INDEX stock_sw ON stock (sw)";
    exec db "BEGIN";
    for o = 1 to n_line do
      exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o ((o mod 500) + 1))
    done;
    for w = 1 to n_w do
      for si = 1 to n_per_w do
        exec
          db
          (Printf.sprintf "INSERT INTO stock VALUES (%d, %d, %d)" w si ((w * 1000) + si))
      done
    done;
    exec db "COMMIT";
    let seek = "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 1"
    and foil =
      "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw + 0 = 1"
    in
    same_rows db ~label:"non-unique full pin agrees with its scan" ~seek ~foil;
    Alcotest.(check int)
      "a full pin of a non-unique index still scans"
      (examined db foil)
      (examined db seek))
;;

(* The headline number this issue is named after, given a home in the repo.

   PR #531 measured #520's shape — 30,000 driving rows against 100,000 stock
   rows spread over four warehouses — at 130,000 examined before and 55,000
   after. That figure lived only in the PR body: the largest committed case was
   1,200 x 2,000, and [test_tpcc_txn.ml] makes no [rows_examined] assertion on
   StockLevel, so nothing would have caught a regression of it.

   This is that shape at one tenth scale, which preserves the arithmetic exactly
   (3,000 + 10,000 = 13,000 examined before, 3,000 + 2,500 = 5,500 after) while
   keeping the seeding inside a unit test's budget. The ratio still sends it to
   the hash join: 3,000 driving rows against a right table of 10,000 fails
   [3000 <= 10000/8].

   #575 GAVE THIS BACK. The 5,500 was 55% of the rows for 2,500 extra tree
   descents, and #561's on-disk measurement of exactly this shape at full scale
   put it at 2.6-3.5x SLOWER than the 13,000-row plan it replaced — the
   [rows_examined] halving was never the cost. So both spellings read 13,000
   again, and this case is kept, renamed, as the record of a headline number that
   measured the wrong thing. *)
let headline_520_shape_is_declined_575 () =
  with_db (fun db ->
    seed db ~n_line:3_000 ~n_w:4 ~n_per_w:2_500 ();
    Alcotest.(check int)
      "the unrecognisable foil: the driving side plus every warehouse's stock"
      13_000
      (examined
         db
         "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw + 0 = 1");
    Alcotest.(check int)
      "the pinned prefix: the same, because #575 declines the seek"
      13_000
      (examined
         db
         "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 1"))
;;

(* A WHERE equality on the right table's rowid alias is a single table seek, not
   an index seek — the #243 path, reached through the same chooser. *)
let build_side_seeks_a_rowid_alias () =
  with_db (fun db ->
    exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
    exec db "CREATE TABLE item (it_id INTEGER PRIMARY KEY, qty INTEGER)";
    exec db "BEGIN";
    for o = 1 to n_line do
      exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o ((o mod 500) + 1))
    done;
    for i = 1 to 500 do
      exec db (Printf.sprintf "INSERT INTO item VALUES (%d, %d)" i (i * 10))
    done;
    exec db "COMMIT";
    let seek =
      "SELECT qty FROM line INNER JOIN item ON it_id = i_id WHERE w = 1 AND it_id = 7"
    in
    let foil =
      "SELECT qty FROM line INNER JOIN item ON it_id = i_id WHERE w = 1 AND it_id + 0 = 7"
    in
    same_rows db ~label:"rowid seek and scan agree" ~seek ~foil;
    Alcotest.(check int)
      "the build side reads exactly the one addressed item row"
      (n_line + 1)
      (examined db seek))
;;

(* Nothing pins a leading index column, so the build side must stay a full scan
   rather than seek a wrong prefix: [qty] is not indexed at all. *)
let unpinnable_build_side_still_scans () =
  with_db (fun db ->
    seed db ();
    let sql =
      "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND qty > 0"
    in
    Alcotest.(check int) "a full scan of stock" (n_line + n_stock) (examined db sql))
;;

(* An equality on a non-leading index column cannot pin the prefix either. Here
   [si] is pinned but stock is keyed [(sw, si)] and [sw] is free. *)
let non_leading_equality_still_scans () =
  with_db (fun db ->
    seed db ();
    let sql =
      "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND si = 5"
    in
    Alcotest.(check int) "a full scan of stock" (n_line + n_stock) (examined db sql))
;;

(* The general-ON path builds a cartesian hash join wrapped in a filter, and its
   build side reaches the same chooser. #575 declines the bare prefix pin here
   too — the ON predicate's shape was never what the decision turned on — so the
   surviving assertion is that the two plans agree on rows, which is what makes
   a later re-narrowing safe to attempt. *)
let general_on_predicate_build_side_is_declined_575 () =
  with_db (fun db ->
    seed db ~n_line:20 ~n_per_w:20 ();
    let seek = "SELECT qty FROM line INNER JOIN stock ON si > i_id WHERE sw = 1" in
    let foil = "SELECT qty FROM line INNER JOIN stock ON si > i_id WHERE sw + 0 = 1" in
    same_rows db ~label:"cartesian build side agrees with its scan" ~seek ~foil;
    Alcotest.(check bool)
      "and the agreement is not between two empty lists"
      true
      (rows_of db seek <> []);
    (* Bound once: these are cartesian products, and the Alcotest label must not
       cost a second execution of each. *)
    let n_seek = examined db seek
    and n_foil = examined db foil in
    Alcotest.(check int)
      (Printf.sprintf "examined %d equals the declined foil's %d" n_seek n_foil)
      n_foil
      n_seek)
;;

(* The general-ON path crossed with LEFT — the combination this PR newly widened,
   and the one place the argument needs two filters rather than one.

   #552 moved the ON predicate INSIDE the join for an outer join, so an
   unmatched left row is now genuinely null-extended rather than dropped. An ON
   predicate true of a null-extended row — [si IS NULL] — would therefore emit
   rows the narrowing could make strictly more numerous. What stops that is
   [chain_joins], which wraps the whole chain in the WHERE clause: [sw] is NULL
   on a null-extended row, and [sw = 1] is never true of NULL. The seek and its
   foil null-extend DIFFERENT left rows and both sets are then dropped, which is
   exactly the argument #516 settled on.

   That is also why this test cannot observe null extension itself: under a
   WHERE equality on the right table it is unobservable by construction. The
   [si IS NULL] case is therefore [] = [], and deliberately so — its
   [examined seek < foil] assertion is what bites here. The null extension it
   used to swallow is pinned separately, without a WHERE clause, by
   [is_null_on_null_extends_every_left_row] in [test_join_general_on_552.ml];
   the assertion below checks the same query here so that a regression of #552
   shows up in this file too rather than making the case silently vacuous again.

   The [o = 20] driving row matches no stock row under either predicate, so it
   is the one that must survive the WHERE-less LEFT JOIN null-extended. *)
let general_on_predicate_left_join_agrees () =
  with_db (fun db ->
    exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
    exec
      db
      "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
    exec db "BEGIN";
    for o = 1 to 20 do
      (* Row 20 matches nothing under either ON predicate, so the ON filter has
         a driving row whose every cartesian pairing it must reject. *)
      let i_id = if o = 20 then 90_000 else o mod 10 in
      exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o i_id)
    done;
    for w = 1 to 4 do
      for si = 1 to 20 do
        exec
          db
          (Printf.sprintf "INSERT INTO stock VALUES (%d, %d, %d)" w si ((w * 1000) + si))
      done
    done;
    exec db "COMMIT";
    let q on pred =
      Printf.sprintf "SELECT o, qty FROM line LEFT JOIN stock ON %s WHERE %s" on pred
    in
    List.iter
      (fun (on, nonempty) ->
         let seek = q on "sw = 1"
         and foil = q on "sw + 0 = 1" in
         same_rows
           db
           ~label:(Printf.sprintf "LEFT + general ON %S agrees with its scan" on)
           ~seek
           ~foil;
         (* Non-vacuity, stated per case: [si IS NULL] yields nothing because
            the WHERE clause drops every null-extended row, not because the
            narrowing swallowed rows and not because of #552 — which is
            re-checked just below, WHERE-less, where it IS observable. *)
         Alcotest.(check bool)
           (Printf.sprintf "ON %S: rows are%s empty" on (if nonempty then " not" else ""))
           nonempty
           (rows_of db seek <> []);
         (* #575: the narrowing is declined for a bare prefix pin, so the two
            plans cost the same. The agreement above is the assertion that
            matters here — the LEFT-plus-general-ON crossing is about which rows
            survive, not about how many are read. *)
         let n_seek = examined db seek
         and n_foil = examined db foil in
         Alcotest.(check int)
           (Printf.sprintf "ON %S: examined %d equals foil %d" on n_seek n_foil)
           n_foil
           n_seek)
      (* [si > i_id] is the ordinary general ON; [si IS NULL] is the one that is
         true of a null-extended row, so it is the case an ON filter placed
         above the join cannot handle at all (#552). *)
      [ "si > i_id", true; "si IS NULL", false ];
    (* #552, without the WHERE clause that makes null extension unobservable:
       no stock row has [si IS NULL], so every one of the 20 driving rows is
       null-extended. Before #552 this returned nothing. *)
    let null_extended =
      rows_of db "SELECT o, qty FROM line LEFT JOIN stock ON si IS NULL"
    in
    Alcotest.(check int)
      "WHERE-less: one null-extended row per driving row"
      20
      (List.length null_extended);
    Alcotest.(check bool)
      "and every one of them carries NULL from stock"
      true
      (List.for_all
         (function
           | [ _; "NULL" ] -> true
           | _ -> false)
         null_extended))
;;

(* ------------------------------------------------------------------ *)
(* The [tree_id >= 0] guard, which is load-bearing and not defensive     *)
(* ------------------------------------------------------------------ *)

(* [access_path_for_eqs] reaches the catalog BY NAME
   ([Cat.indexes_for_table cat ~table:right_meta.name]). A CTE gets a
   synthesized [table_meta] carrying the CTE's own name and [tree_id = -1], so a
   CTE that shadows a real table would pick up that table's indexes and plan an
   [Op_index_lookup] against a B-tree the CTE has nothing to do with.

   That is a WRONG ANSWER, not a missed optimisation: deleting the guard (making
   the [Cat.Row] arm [true]) turns this query's 240 rows into 0 while the foil
   still returns 240. Verified by hand on this branch; this case is what stops
   it rotting. *)
let cte_shadowing_a_real_table_still_scans () =
  with_db (fun db ->
    (* [i_mod:5] puts [i_id] in 1..5, so exactly a fifth of the driving rows
       carry the CTE's [si = 3]. *)
    seed db ~i_mod:5 ();
    let cte = "WITH stock AS (SELECT 1 AS sw, 3 AS si, 999 AS qty) " in
    let seek =
      cte ^ "SELECT qty FROM line JOIN stock ON si = i_id WHERE w = 1 AND sw = 1"
    in
    let foil =
      cte ^ "SELECT qty FROM line JOIN stock ON si = i_id WHERE w = 1 AND sw + 0 = 1"
    in
    let expected = List.init (n_line / 5) (fun _ -> [ "999" ]) in
    Alcotest.(check (list (list string)))
      "the CTE's own single row, once per matching driving row"
      expected
      (rows_of db seek);
    same_rows db ~label:"shadowing CTE agrees with its unoptimizable foil" ~seek ~foil)
;;

(* The [Cat.Columnar] arm of the same guard. Unlike the CTE arm this one is
   genuinely defensive — a columnar table has no rowid alias and no indexes, so
   [access_path_for_eqs] would answer [None] and fall back to [make_scan]
   anyway. It is still the only thing keeping [Cat.row_storage], which raises on
   a columnar [table_meta], off the path if the chooser ever grows an arm that
   reaches it. *)
let columnar_right_table_still_scans () =
  with_db (fun db ->
    exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
    exec db "CREATE TABLE cs (sw INTEGER, si INTEGER, qty INTEGER) USING COLUMNSTORE";
    exec db "BEGIN";
    for o = 1 to n_line do
      exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o ((o mod 8) + 1))
    done;
    for w = 1 to 2 do
      for si = 1 to 8 do
        exec
          db
          (Printf.sprintf "INSERT INTO cs VALUES (%d, %d, %d)" w si ((w * 100) + si))
      done
    done;
    exec db "COMMIT";
    let seek = "SELECT qty FROM line INNER JOIN cs ON si = i_id WHERE w = 1 AND sw = 1" in
    let foil =
      "SELECT qty FROM line INNER JOIN cs ON si = i_id WHERE w = 1 AND sw + 0 = 1"
    in
    same_rows db ~label:"columnar right table agrees with its foil" ~seek ~foil;
    Alcotest.(check int)
      "and every driving row found its warehouse-1 match"
      n_line
      (List.length (rows_of db seek)))
;;

(* Every other narrowing case joins one table, so [right_col_offset] is the
   driving table's width. Here [stock] is the SECOND join, and [right_table_eqs]
   must re-base [sw]'s combined-row ordinal across both preceding tables. A
   wrong offset would seek the wrong column and the foil would disagree.

   #575: the pin has to be the WHOLE of stock's key for the seek to be taken at
   all now, so the query pins [si] as well as [sw]. That still exercises the
   re-basing — both ordinals are re-based across the same two preceding tables,
   and a wrong offset would seek the wrong column and disagree with the foil. *)
let build_side_seeks_as_the_second_join_in_a_chain () =
  with_db (fun db ->
    seed db ~n_per_w:20 ~i_mod:20 ();
    exec db "CREATE TABLE ord (ow INTEGER PRIMARY KEY, tag INTEGER)";
    exec db "INSERT INTO ord VALUES (1, 7)";
    let q pred =
      Printf.sprintf
        "SELECT qty, tag FROM line INNER JOIN ord ON ow = w INNER JOIN stock ON si = \
         i_id WHERE w = 1 AND %s"
        pred
    in
    let seek = q "sw = 1 AND si = 5"
    and foil = q "sw + 0 = 1 AND si = 5" in
    same_rows db ~label:"chained build side agrees with its scan" ~seek ~foil;
    Alcotest.(check bool)
      "and the agreement is not between two empty lists"
      true
      (rows_of db seek <> []);
    Alcotest.(check bool)
      "and the narrowing fired across the offset"
      true
      (examined db seek < examined db foil))
;;

(* ------------------------------------------------------------------ *)
(* #526: the cost model must follow the narrowed build side             *)
(* ------------------------------------------------------------------ *)

(* #526 compares D seeks against D + R reads, with R the right table's size. Now
   that the build side can seek, R must not be costed at a table the plan never
   reads — but the estimate only moves where the seek is PROVABLY A POINT.
   [estimate_rows] answers [unbounded_rows] for an [Op_index_lookup] unless the
   seek is a unique point or carries a range, and the build side passes no range
   (#532), so every partial prefix pin still costs the whole table. There is no
   selectivity estimation here.

   A full primary-key pin is that point case. Here the WHERE clause pins stock's
   whole primary key, so the build side reads exactly one row and the estimate
   says 1. Costing R at the table's 20,000-row high-water mark makes
   1,200 driving rows look cheap enough to probe (1200 <= 20000/8) — 1,200 B-tree
   seeks in place of one seek and 1,200 hash lookups. Costing it at the seek's
   actual reach picks the hash join. *)
let full_key_pin_makes_the_hash_join_win () =
  with_db (fun db ->
    seed db ~n_w:1 ~n_per_w:20_000 ~i_mod:500 ();
    let sql =
      "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 1 AND si \
       = 5"
    in
    Alcotest.(check int)
      "the driving rows plus the single stock row the build side reads"
      (n_line + 1)
      (examined db sql);
    Alcotest.(check (list (list string)))
      "and the answer is unchanged"
      (rows_of
         db
         "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw + 0 = 1 \
          AND si = 5")
      (rows_of db sql))
;;

(* The converse: a partial prefix pin does not bound the seek to a known number
   of rows, so R stays the table's size and #520's ratio test must still send
   1,200 driving rows against a much larger right table to the PROBE. This is
   the case PR #526 exists for, and the narrowed build side must not steal it. *)
let partial_pin_still_probes_a_much_larger_right_table () =
  with_db (fun db ->
    seed db ~n_w:1 ~n_per_w:20_000 ~i_mod:500 ();
    Alcotest.(check int)
      "one probed stock row per driving row, not a scan of 20,000"
      (n_line * 2)
      (examined
         db
         "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 1"))
;;

(* ------------------------------------------------------------------ *)
(* Correctness the narrowing must not disturb                           *)
(* ------------------------------------------------------------------ *)

(* The LEFT JOIN case the header argues about. [i_id] 777 exists only in
   warehouse 2, so the narrowed build side null-extends those driving rows where
   a full scan would have matched them. The post-join [sw = 1] is NULL on the
   null-extended row and false on the row it replaced, so both are dropped and
   the answers agree. *)
let left_join_with_a_narrowing_constant () =
  with_db (fun db ->
    exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
    exec
      db
      "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
    exec db "BEGIN";
    for o = 1 to n_line do
      (* Three classes of driving row: matched in warehouse 1, matched only in
         warehouse 2, and matched nowhere at all. *)
      let i_id =
        match o mod 3 with
        | 0 -> (o mod 400) + 1
        | 1 -> 777
        | _ -> 90_000 + o
      in
      exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o i_id)
    done;
    for w = 1 to 2 do
      for si = 1 to 400 do
        exec
          db
          (Printf.sprintf "INSERT INTO stock VALUES (%d, %d, %d)" w si ((w * 1000) + si))
      done
    done;
    exec db "INSERT INTO stock VALUES (2, 777, 2777)";
    exec db "COMMIT";
    let seek = "SELECT o, qty FROM line LEFT JOIN stock ON si = i_id WHERE sw = 1" in
    let foil = "SELECT o, qty FROM line LEFT JOIN stock ON si = i_id WHERE sw + 0 = 1" in
    same_rows db ~label:"narrowed LEFT JOIN build side agrees with its scan" ~seek ~foil;
    (* Bound once each: the label must not cost a second execution. *)
    let n_seek = examined db seek
    and n_foil = examined db foil in
    (* #575 declines this bare prefix pin, so the two plans cost the same. The
       soundness argument the case exists for is unaffected — it is about which
       rows survive a narrowed build side, and it has to keep holding for the
       day #576 lets the narrowing come back. *)
    Alcotest.(check int)
      (Printf.sprintf "examined %d equals the declined foil's %d" n_seek n_foil)
      n_foil
      n_seek;
    (* And warehouse 2's 777 rows are still gone — dropped by the post-join
       [sw = 1] rather than by the narrowing. *)
    Alcotest.(check bool)
      "no warehouse-2 quantity survives"
      false
      (List.exists (fun r -> List.nth r 1 = "2777") (rows_of db seek)))
;;

(* A LEFT JOIN whose driving row has a NULL join key still null-extends.

   Note what this does and does not cover. With no WHERE clause [right_eqs] is
   [[]] and the build side is [make_scan] — so the first half is a regression
   guard on the UNNARROWED path, and passes identically on main. The second half
   adds the narrowing conjunct, which is what makes it coverage of #528: NULL
   join keys and a seeked build side at the same time.

   A narrowed LEFT JOIN can never emit a null-extended output row, and that is
   structural rather than an accident of this population — the narrowing
   conjunct is by construction in the post-join filter, and it is NULL on a
   null-extended row. So the second half asserts agreement with the foil (both
   drop them) rather than the presence of NULLs. *)
let left_join_null_key_null_extends () =
  with_db (fun db ->
    exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
    exec
      db
      "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
    exec db "BEGIN";
    for o = 1 to n_line do
      (* A third of the driving rows have a NULL join key; the rest match in
         warehouse 1, warehouse 2 only, or nowhere. *)
      let v =
        match o mod 4 with
        | 0 -> "NULL"
        | 1 -> string_of_int ((o mod 8) + 1)
        | 2 -> "777"
        | _ -> string_of_int (90_000 + o)
      in
      exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %s)" o v)
    done;
    for w = 1 to 2 do
      for si = 1 to 8 do
        exec
          db
          (Printf.sprintf "INSERT INTO stock VALUES (%d, %d, %d)" w si ((w * 100) + si))
      done
    done;
    exec db "INSERT INTO stock VALUES (2, 777, 2777)";
    exec db "COMMIT";
    (* Unnarrowed: no WHERE clause, so the build side is a plain scan. *)
    let rows = rows_of db "SELECT o, qty FROM line LEFT JOIN stock ON si = i_id" in
    let null_extended = List.filter (fun r -> List.nth r 1 = "NULL") rows in
    (* The NULL-keyed quarter plus the matches-nothing quarter. *)
    Alcotest.(check int)
      "every unmatchable driving row survives, null-extended"
      (n_line / 2)
      (List.length null_extended);
    (* Narrowed: the same NULL keys, now against a seeked build side. *)
    let seek = "SELECT o, qty FROM line LEFT JOIN stock ON si = i_id WHERE sw = 1" in
    let foil = "SELECT o, qty FROM line LEFT JOIN stock ON si = i_id WHERE sw + 0 = 1" in
    same_rows db ~label:"NULL keys against a seeked build side" ~seek ~foil;
    (* #575: declined, so equal rather than smaller — see
       [left_join_with_a_narrowing_constant]. *)
    let n_seek = examined db seek
    and n_foil = examined db foil in
    Alcotest.(check int)
      (Printf.sprintf "examined %d equals the declined foil's %d" n_seek n_foil)
      n_foil
      n_seek)
;;

(* A pin that matches nothing must yield no rows, not the index's NULL entries
   and not a wrongly-truncated prefix.

   #575: spelled as a FULL key pin, because that is the shape that still seeks —
   a bare [sw = 9] now scans and would prove nothing about the seek. The
   empty-result half is asserted for both spellings; only the taken seek can
   assert what the build side read. *)
let constant_matching_nothing_yields_nothing () =
  with_db (fun db ->
    seed db ();
    let bare =
      "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 9"
    in
    let full =
      "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 9 AND si \
       = 5"
    in
    Alcotest.(check (list (list string))) "no warehouse 9" [] (rows_of db bare);
    Alcotest.(check (list (list string)))
      "and none with si pinned too"
      []
      (rows_of db full);
    Alcotest.(check int)
      "the taken point seek read nothing beyond the driving rows"
      n_line
      (examined db full))
;;

(* Two equalities on the same right-table column: only the one the seek uses is
   consumed by the prefix, and the post-join filter still enforces both — so a
   contradiction is still empty. *)
let contradictory_equalities_stay_empty () =
  with_db (fun db ->
    seed db ();
    Alcotest.(check (list (list string)))
      "sw cannot be both 1 and 2"
      []
      (rows_of
         db
         "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 1 AND \
          sw = 2"))
;;

(* An [OR] must not reach the build side: [conjuncts] never splits one, so this
   query's build side is a full scan and both warehouses show up. *)
let or_does_not_narrow_the_build_side () =
  with_db (fun db ->
    seed db ~n_per_w:20 ();
    let sql =
      "SELECT sw, qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND (sw = 1 OR \
       sw = 2)"
    in
    let seen = List.sort_uniq compare (List.map (fun r -> List.hd r) (rows_of db sql)) in
    Alcotest.(check (list string)) "both warehouses" [ "1"; "2" ] seen)
;;

(* ------------------------------------------------------------------ *)
(* Property                                                             *)
(* ------------------------------------------------------------------ *)

(* Whatever the pinned warehouse and whatever the driving-side range, the
   narrowed build side returns exactly the row set of the unoptimizable foil.
   The driving side is deliberately kept above #520's 1000-row floor, because
   below it the planner takes a probe and the build side never runs at all.

   NON-VACUITY. The generators used to draw [sw] from 0..3 and [i_hi] from 0..12
   against a [stock] holding only [sw] in {1,2} and [si] in 1..8, so a draw of
   [sw = 0] or [i_hi = 0] compared two empty row sets and proved nothing. This
   repo has shipped silently-vacuous property tests before (#485). The ranges
   are now inside the population — [i_hi >= 2] matters, because [i_id] is
   [o mod 10] and [i_id < 1] selects only the [i_id = 0] rows, which join
   nothing — and each case asserts a non-empty result as well as agreement.

   The [left] flag makes the soundness argument executable rather than adding
   LEFT-JOIN output coverage: because the narrowing conjunct is always in the
   WHERE clause, a narrowed LEFT JOIN can never emit a null-extended row. That
   is structural, not an accident of these generators — see
   [left_join_null_key_null_extends]. *)
let n_line_prop = 1050

let prop_build_seek_matches_foil =
  QCheck.Test.make
    ~count:20
    ~name:"hash-join build-side seek agrees with unoptimizable foil"
    QCheck.(quad (int_range 1 2) (int_range 2 12) (int_range 1 8) bool)
    (fun (sw, i_hi, si_pin, left) ->
       with_db (fun db ->
         exec
           db
           "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
         exec
           db
           "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, \
            si))";
         exec db "BEGIN";
         for o = 1 to n_line_prop do
           exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o (o mod 10))
         done;
         for w = 1 to 2 do
           for si = 1 to 8 do
             exec
               db
               (Printf.sprintf
                  "INSERT INTO stock VALUES (%d, %d, %d)"
                  w
                  si
                  ((w * 100) + si))
           done
         done;
         exec db "COMMIT";
         let kind = if left then "LEFT" else "INNER" in
         let sql pred =
           Printf.sprintf
             "SELECT o, qty FROM line %s JOIN stock ON si = i_id WHERE %s"
             kind
             pred
         in
         (* #575 splits this into two regimes, and the property covers both.

            BARE — a prefix pin of stock's key, which the decision declines: the
            rows must still agree with the foil (the invariant that lets the
            narrowing come back under #576) and the two plans must now cost the
            same, which is the decision itself.

            FULL — the whole of the unique key pinned, which is still taken: the
            rows agree AND the seek really reads less. *)
         let bare = sql (Printf.sprintf "sw = %d AND i_id < %d" sw i_hi)
         and bare_foil = sql (Printf.sprintf "sw + 0 = %d AND i_id < %d" sw i_hi) in
         let full = sql (Printf.sprintf "sw = %d AND si = %d" sw si_pin)
         and full_foil = sql (Printf.sprintf "sw + 0 = %d AND si = %d" sw si_pin) in
         let bare_rows = rows_of db bare
         and full_rows = rows_of db full in
         (* Non-vacuity, checked per case rather than argued from the ranges. *)
         bare_rows <> []
         && full_rows <> []
         && bare_rows = rows_of db bare_foil
         && full_rows = rows_of db full_foil
         && examined db bare = examined db bare_foil
         && examined db full < examined db full_foil))
;;

let () =
  Alcotest.run
    "hash_join_build_seek_528"
    [ ( "narrowing"
      , [ Alcotest.test_case
            "bare prefix pin is declined (#575)"
            `Quick
            bare_prefix_pin_is_declined_575
        ; Alcotest.test_case
            "full unique-key pin still seeks (#575)"
            `Quick
            full_unique_key_pin_still_seeks
        ; Alcotest.test_case
            "nullable unique-key pin still seeks (#593)"
            `Quick
            nullable_unique_key_pin_still_seeks
        ; Alcotest.test_case
            "full non-unique-key pin is declined (#575)"
            `Quick
            full_non_unique_key_pin_is_declined_575
        ; Alcotest.test_case
            "build side seeks a rowid alias"
            `Quick
            build_side_seeks_a_rowid_alias
        ; Alcotest.test_case
            "unpinnable build side still scans"
            `Quick
            unpinnable_build_side_still_scans
        ; Alcotest.test_case
            "non-leading equality still scans"
            `Quick
            non_leading_equality_still_scans
        ; Alcotest.test_case
            "general ON predicate build side is declined (#575)"
            `Quick
            general_on_predicate_build_side_is_declined_575
        ; Alcotest.test_case
            "general ON predicate LEFT JOIN agrees"
            `Quick
            general_on_predicate_left_join_agrees
        ; Alcotest.test_case
            "#520's headline shape is declined (#575)"
            `Quick
            headline_520_shape_is_declined_575
        ; Alcotest.test_case
            "build side seeks as the second join in a chain"
            `Quick
            build_side_seeks_as_the_second_join_in_a_chain
        ] )
    ; ( "unseekable right tables"
      , [ Alcotest.test_case
            "CTE shadowing a real table still scans"
            `Quick
            cte_shadowing_a_real_table_still_scans
        ; Alcotest.test_case
            "columnar right table still scans"
            `Quick
            columnar_right_table_still_scans
        ] )
    ; ( "cost model"
      , [ Alcotest.test_case
            "full-key pin makes the hash join win"
            `Quick
            full_key_pin_makes_the_hash_join_win
        ; Alcotest.test_case
            "partial pin still probes a much larger right table"
            `Quick
            partial_pin_still_probes_a_much_larger_right_table
        ] )
    ; ( "correctness"
      , [ Alcotest.test_case
            "LEFT JOIN with a narrowing constant"
            `Quick
            left_join_with_a_narrowing_constant
        ; Alcotest.test_case
            "LEFT JOIN NULL key null-extends"
            `Quick
            left_join_null_key_null_extends
        ; Alcotest.test_case
            "constant matching nothing yields nothing"
            `Quick
            constant_matching_nothing_yields_nothing
        ; Alcotest.test_case
            "contradictory equalities stay empty"
            `Quick
            contradictory_equalities_stay_empty
        ; Alcotest.test_case
            "OR does not narrow the build side"
            `Quick
            or_does_not_narrow_the_build_side
        ] )
    ; "property", List.map QCheck_alcotest.to_alcotest [ prop_build_seek_matches_foil ]
    ]
;;
