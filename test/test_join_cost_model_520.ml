(** #520: the nested-loop-vs-hash join choice needs a cost model.

    [plan_join] took a nested-loop join whenever the right table had a usable
    probe, and never compared it against the hash-join alternative. That gamble
    is a huge win on a small driving side and a loss on a large one:

    {v
      driving side   nested-loop probe        hash join
      30,240 rows    60,480 examined  2757ms   130,240 examined  1240ms
          12 rows        24 examined   0.5ms   100,012 examined   945ms
    v}

    Rows examined FALL while wall-clock RISES — a seek costs far more than a
    hash-table lookup, so ~30,000 probes lose to a single 100,000-row scan.
    #516 widened the blast radius: any index whose leading columns are covered
    by WHERE constants plus the join column now qualifies as a probe, so many
    more queries took the gamble.

    The fix is a crude STATIC estimate of the driving side's cardinality — no
    stored statistics exist in this engine — compared against the right table's
    size. The comparison is a RATIO, because both halves of the trade-off scale:
    D probes against D + R reads. An absolute cut on D alone is calibrated for
    exactly one R, which review of PR #526 measured directly — 1,200 driving
    rows against TPC-C's 100,000-row stock was 36x slower as a hash join than as
    a probe. [large_driving_side_uses_hash_join] and
    [large_driving_side_still_probes_a_much_larger_right_table] hold D fixed at
    1,200 and flip only R, so between them they pin that the rule is a ratio.

    The rest pin the estimator's classes and, more importantly, that the two
    strategies return the same rows: this is purely a strategy choice and can
    never change an answer.

    [rows_examined] is the load-bearing assertion. A nested-loop join reads one
    right row per driving row; a hash join reads the right table exactly once,
    so the two are told apart by counting. *)

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

(* The choice is a RATIO, not an absolute cut: a probe costs one seek per
   driving row, a hash join costs one read per right-table row, so the same
   1,200 driving rows are the wrong choice against a 200-row [stock] and the
   right one against a 20,000-row [stock]. Both regimes appear below.

   [n_big] sits above the planner's absolute floor of 1000 driving rows and
   [n_small] far below it. *)
let n_big = 1200
let n_small = 5
let n_stock = 200

(* Big enough that [n_big] probes beat one scan of it. Kept well under the
   100,000 rows of TPC-C's stock so seeding stays a couple of seconds; the shape
   is the same, only the margin is narrower. *)
let n_stock_big = 20_000

(* [line] is the driving side, [stock] the probed side keyed on [(sw, si)] — the
   TPC-C StockLevel shape, where the join is on [si] and [sw] comes from the
   WHERE clause. [n_w] warehouses of [n_line] rows each. *)
let seed db ?(n_w = 1) ~n_line ~n_stock () =
  exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
  exec db "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
  exec db "BEGIN";
  for w = 1 to n_w do
    for o = 1 to n_line do
      exec
        db
        (Printf.sprintf "INSERT INTO line VALUES (%d, %d, %d)" w o ((o mod n_stock) + 1))
    done
  done;
  for si = 1 to n_stock do
    exec db (Printf.sprintf "INSERT INTO stock VALUES (1, %d, %d)" si (si * 10))
  done;
  exec db "COMMIT"
;;

let probe_sql =
  "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 1"
;;

(* [sw + 0] is unseekable, so this foil can only be a hash join — it is the
   reference for what the hash strategy costs and returns. *)
let foil_sql =
  "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw + 0 = 1"
;;

(* ------------------------------------------------------------------ *)
(* The threshold                                                        *)
(* ------------------------------------------------------------------ *)

(* Above the threshold the probe must be abandoned: [n_big] probes cost more
   than one scan of [stock], and the plan must read [stock] once. *)
let large_driving_side_uses_hash_join () =
  with_db (fun db ->
    seed db ~n_line:n_big ~n_stock ();
    Alcotest.(check int)
      "driving rows + one scan of stock, not one probe per driving row"
      (n_big + n_stock)
      (examined db probe_sql))
;;

(* Below the threshold the #516 probe must survive untouched — one right row
   per driving row, nowhere near a scan of [stock]. *)
let small_driving_side_keeps_the_probe () =
  with_db (fun db ->
    seed db ~n_line:n_small ~n_stock ();
    Alcotest.(check int)
      "one probed stock row per driving row"
      (n_small * 2)
      (examined db probe_sql))
;;

(* A rowid-alias lookup yields at most one driving row, so the probe is right
   however large the driving table is. This is the estimator's exact-1 class. *)
let rowid_lookup_driving_side_keeps_the_probe () =
  with_db (fun db ->
    exec db "CREATE TABLE big (id INTEGER PRIMARY KEY, i_id INTEGER)";
    exec
      db
      "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
    exec db "BEGIN";
    for id = 1 to n_big do
      exec db (Printf.sprintf "INSERT INTO big VALUES (%d, %d)" id ((id mod n_stock) + 1))
    done;
    for si = 1 to n_stock do
      exec db (Printf.sprintf "INSERT INTO stock VALUES (1, %d, %d)" si (si * 10))
    done;
    exec db "COMMIT";
    Alcotest.(check int)
      "one driving row and one probed stock row"
      2
      (examined
         db
         "SELECT qty FROM big INNER JOIN stock ON si = i_id WHERE id = 7 AND sw = 1"))
;;

(* An unbounded scan of a large driving table is the clearest loss of all: the
   estimator reads the table's own rowid high-water mark and refuses the probe. *)
let large_seq_scan_driving_side_uses_hash_join () =
  with_db (fun db ->
    seed db ~n_line:n_big ~n_stock ();
    Alcotest.(check int)
      "driving rows + one scan of stock"
      (n_big + n_stock)
      (examined db "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE sw = 1"))
;;

(* The StockLevel case #517/#519 rescued: the driving table is large but the
   seek carries a range bound, which narrows it to a window the probe handles
   well. This must NOT regress to a hash join. *)
let range_bounded_seek_keeps_the_probe () =
  with_db (fun db ->
    seed db ~n_line:n_big ~n_stock ();
    let sql =
      "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND o >= 100 AND o \
       < 120 AND sw = 1"
    in
    (* 20 rows in the window plus the strict endpoint (scanned, then rejected by
       the predicate — the #517 inclusive-bound design), each probing one stock
       row. *)
    Alcotest.(check int) "a bounded window, probed row by row" 42 (examined db sql))
;;

(* The same 1,200 driving rows against a right table 100x larger. A probe costs
   [n_big] seeks; a hash join costs [n_big] + [n_stock_big] reads, so here the
   probe is far the cheaper plan and the driving side alone cannot say which is
   which.

   Review of PR #526 measured the absolute-threshold version of this against
   TPC-C's 100,000-row stock, in memory: 101,200 rows examined / 153.7 ms for the
   hash join against 2,400 / 4.3 ms for the probe — a 36x regression on a plan
   that was fast before #520. [n_stock_big] is the same shape with a narrower
   margin so the test stays fast. *)
let large_driving_side_still_probes_a_much_larger_right_table () =
  with_db (fun db ->
    seed db ~n_line:n_big ~n_stock:n_stock_big ();
    Alcotest.(check int)
      "one probed stock row per driving row, not a scan of 20,000"
      (n_big * 2)
      (examined db probe_sql))
;;

(* A full-key seek on a composite PRIMARY KEY reaches exactly one row, and must
   keep the probe however large the driving table is — this is the #508/#516
   shape the whole series is about.

   It needs its own case because [Sema.mark_table_pk] only marks a table-level
   PRIMARY KEY that names ONE column, so both columns of [PRIMARY KEY (w, o)]
   carry [primary_key = false] and [not_null = false]. Review of PR #526 caught
   the estimator's NOT NULL guard silently failing here: 201 rows examined (a
   hash join, one scan of stock) to serve one row, where the probe reads 2. The
   second spelling declares the columns NOT NULL explicitly; both must agree,
   which is what says the estimate follows the seek's shape and not an
   incidental column flag. *)
let composite_pk_point_lookup_keeps_the_probe () =
  let check ~label ~cols =
    with_db (fun db ->
      exec db (Printf.sprintf "CREATE TABLE line (%s, PRIMARY KEY (w, o))" cols);
      exec
        db
        "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
      exec db "BEGIN";
      for o = 1 to 2000 do
        exec
          db
          (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o ((o mod n_stock) + 1))
      done;
      for si = 1 to n_stock do
        exec db (Printf.sprintf "INSERT INTO stock VALUES (1, %d, %d)" si (si * 10))
      done;
      exec db "COMMIT";
      let sql =
        "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND o = 5 AND sw \
         = 1"
      in
      Alcotest.(check int) (label ^ ": one joined row") 1 (List.length (rows_of db sql));
      Alcotest.(check int)
        (label ^ ": one driving row and one probed stock row")
        2
        (examined db sql))
  in
  check ~label:"nullable" ~cols:"w INTEGER, o INTEGER, i_id INTEGER";
  check ~label:"NOT NULL" ~cols:"w INTEGER NOT NULL, o INTEGER NOT NULL, i_id INTEGER"
;;

(* ------------------------------------------------------------------ *)
(* Correctness the choice must not disturb                              *)
(* ------------------------------------------------------------------ *)

let strategies_agree_on_a_large_driving_side () =
  with_db (fun db ->
    seed db ~n_line:n_big ~n_stock ();
    Alcotest.(check (list (list string)))
      "hash join and the unoptimizable foil agree"
      (rows_of db foil_sql)
      (rows_of db probe_sql))
;;

let strategies_agree_on_a_small_driving_side () =
  with_db (fun db ->
    seed db ~n_line:n_small ~n_stock ();
    Alcotest.(check (list (list string)))
      "probe and the unoptimizable foil agree"
      (rows_of db foil_sql)
      (rows_of db probe_sql))
;;

let left_join_agrees_across_the_threshold () =
  with_db (fun db ->
    seed db ~n_line:n_big ~n_stock ();
    let sql =
      "SELECT o, qty FROM line LEFT JOIN stock ON si = i_id WHERE w = 1 AND o < 50"
    in
    let foil =
      "SELECT o, qty FROM line LEFT JOIN stock ON si = i_id WHERE w + 0 = 1 AND o < 50"
    in
    Alcotest.(check (list (list string))) "same rows" (rows_of db foil) (rows_of db sql))
;;

(* ------------------------------------------------------------------ *)
(* Property                                                             *)
(* ------------------------------------------------------------------ *)

(* Whichever side of the choice a query lands on, the row set is the same as the
   unoptimizable foil's.

   Two things the first version of this property got wrong, and that the vacuity
   trap of #485/#511 has now caught three times in this repo. [w] is generated
   over 1..3 and the seed now really inserts three warehouses, so every draw
   selects rows rather than comparing [] to []; the result is asserted non-empty
   so it cannot go vacuous again. And [range] toggles the trailing [o < hi],
   which is what gives the driving seek a #517 range bound: with it the estimate
   is [range_seek_rows] and the probe is always chosen, so the earlier version
   never exercised the hash side at all. Without it the driving estimate is the
   whole table and the plan goes to the hash join. *)
let prop_choice_never_changes_the_rows =
  QCheck.Test.make
    ~count:20
    ~name:"join strategy choice agrees with the unoptimizable foil"
    QCheck.(triple (int_range 1 3) (int_range 2 60) bool)
    (fun (w, hi, range) ->
       with_db (fun db ->
         seed db ~n_w:3 ~n_line:1400 ~n_stock ();
         let window = if range then Printf.sprintf " AND o < %d" hi else "" in
         let q ~seekable =
           rows_of
             db
             (Printf.sprintf
                "SELECT o, qty FROM line INNER JOIN stock ON si = i_id WHERE %s%s AND %s"
                (if seekable
                 then Printf.sprintf "w = %d" w
                 else Printf.sprintf "w + 0 = %d" w)
                window
                (if seekable then "sw = 1" else "sw + 0 = 1"))
         in
         let chosen = q ~seekable:true in
         chosen <> [] && chosen = q ~seekable:false))
;;

let () =
  Alcotest.run
    "join_cost_model_520"
    [ ( "threshold"
      , [ Alcotest.test_case
            "large driving side uses hash join"
            `Quick
            large_driving_side_uses_hash_join
        ; Alcotest.test_case
            "small driving side keeps the probe"
            `Quick
            small_driving_side_keeps_the_probe
        ; Alcotest.test_case
            "rowid lookup driving side keeps the probe"
            `Quick
            rowid_lookup_driving_side_keeps_the_probe
        ; Alcotest.test_case
            "large seq scan driving side uses hash join"
            `Quick
            large_seq_scan_driving_side_uses_hash_join
        ; Alcotest.test_case
            "range bounded seek keeps the probe"
            `Quick
            range_bounded_seek_keeps_the_probe
        ; Alcotest.test_case
            "large driving side still probes a much larger right table"
            `Quick
            large_driving_side_still_probes_a_much_larger_right_table
        ; Alcotest.test_case
            "composite PK point lookup keeps the probe"
            `Quick
            composite_pk_point_lookup_keeps_the_probe
        ] )
    ; ( "correctness"
      , [ Alcotest.test_case
            "strategies agree on a large driving side"
            `Quick
            strategies_agree_on_a_large_driving_side
        ; Alcotest.test_case
            "strategies agree on a small driving side"
            `Quick
            strategies_agree_on_a_small_driving_side
        ; Alcotest.test_case
            "LEFT JOIN agrees across the threshold"
            `Quick
            left_join_agrees_across_the_threshold
        ] )
    ; ( "property"
      , List.map QCheck_alcotest.to_alcotest [ prop_choice_never_changes_the_rows ] )
    ]
;;
