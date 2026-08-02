(** #517: an access path must bound the index column after its equality prefix.

    #508 gave the planner an [equality*] access path; a trailing inequality was
    left entirely to the residual filter, so [w = ? AND o >= ? AND o < ?] over a
    [(w, o)] index seeked to the start of [w]'s range and then read the whole of
    it. In TPC-C StockLevel that read a district's 30,240 order lines to return
    230.

    The bound is a narrowing and nothing more: the conjuncts are not marked
    consumed, so the predicate is still evaluated on every row the seek yields.
    That is what lets both ends be treated as inclusive regardless of whether
    the SQL said [>] or [>=] — an inclusive reading of a strict bound scans at
    most one extra key and can never drop a row. The tests below pin both
    halves: [rows_examined] for the narrowing, and agreement with an
    unoptimizable foil for the semantics.

    Only [Integer] and [Real] columns are bounded, because their index-key
    encoding is a fixed width the stop test can compare at a known offset. Text
    falls back to an unbounded prefix scan, which the [text_range] case pins as
    correct-but-unnarrowed rather than silently wrong. *)

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

(* [t(w, o, v)] keyed by (w, o): [n_w] groups of [n_o] rows. A query pinning [w]
   and bounding [o] should read only the bounded span, not the whole group. *)
let n_w = 3
let n_o = 300

let seed db =
  exec db "CREATE TABLE t (w INTEGER, o INTEGER, v INTEGER, PRIMARY KEY (w, o))";
  exec db "BEGIN";
  for w = 1 to n_w do
    for o = 1 to n_o do
      exec db (Printf.sprintf "INSERT INTO t VALUES (%d, %d, %d)" w o ((w * 1000) + o))
    done
  done;
  exec db "COMMIT"
;;

(* Both spellings must return the same rows; [bounded] must read fewer.

   [expect_examined] is exact, and where a bound is strict it is deliberately
   one MORE than the number of rows returned: the strict endpoint's key is
   scanned and then rejected by the predicate.  That is the inclusive-bound
   design stated in the header, pinned as a number so a future change to it
   cannot pass unnoticed. *)
let check_narrows db ~bounded ~foil ~expect_examined =
  Alcotest.(check (list (list string)))
    "bounded and unoptimizable foil agree"
    (rows_of db foil)
    (rows_of db bounded);
  Alcotest.(check int) "rows examined" expect_examined (examined db bounded)
;;

(* ------------------------------------------------------------------ *)
(* Narrowing                                                            *)
(* ------------------------------------------------------------------ *)

(* The StockLevel shape: an equality prefix plus a half-open window. *)
let two_sided_range_narrows () =
  with_db (fun db ->
    seed db;
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o >= 100 AND o < 120"
      ~foil:"SELECT v FROM t WHERE w = 2 AND o + 0 >= 100 AND o + 0 < 120"
        (* 20 rows returned + the strict endpoint 120, scanned then rejected. *)
      ~expect_examined:21)
;;

(* #519: [BETWEEN] is its own AST node, not a pair of comparisons, so it has to
   be recognised in its own right or the whole equality group is scanned. Both
   its ends are inclusive, so — unlike the strict cases above — the examined
   count is exactly the number of rows returned. *)
let between_narrows () =
  with_db (fun db ->
    seed db;
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o BETWEEN 100 AND 119"
      ~foil:"SELECT v FROM t WHERE w = 2 AND o + 0 BETWEEN 100 AND 119"
      ~expect_examined:20)
;;

(* One end a literal, the other an expression the planner cannot evaluate at
   plan time: the recognisable end must still bound its side rather than the
   pair being rejected wholesale. *)
let half_recognisable_between_narrows () =
  with_db (fun db ->
    seed db;
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o BETWEEN 281 AND v"
      ~foil:"SELECT v FROM t WHERE w = 2 AND o + 0 BETWEEN 281 AND v"
      ~expect_examined:20)
;;

(* An unrecognisable end does not block a separate conjunct from supplying it:
   the [BETWEEN] gives the lower end, the recogniser falls through on its upper
   one, and the inequality completes the pair. *)
let between_and_inequality_compose () =
  with_db (fun db ->
    seed db;
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o BETWEEN 100 AND v AND o <= 119"
      ~foil:"SELECT v FROM t WHERE w = 2 AND o + 0 BETWEEN 100 AND v AND o + 0 <= 119"
      ~expect_examined:20)
;;

(* #513's lesson: the joined access path plans from [base_only_conjuncts], and a
   [BETWEEN] end may name a joined column there.  Only the recognised end
   becomes a bound; the post-join filter still evaluates the whole predicate, so
   the rows must match the unoptimizable foil while the read narrows. *)
let between_under_a_join_narrows () =
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE j (k INTEGER, lim INTEGER)";
    exec db "INSERT INTO j VALUES (1, 119)";
    let bounded =
      "SELECT t.v FROM t JOIN j ON j.k = 1 WHERE t.w = 2 AND t.o BETWEEN 100 AND j.lim"
    in
    let foil =
      "SELECT t.v FROM t JOIN j ON j.k = 1 WHERE t.w = 2 AND t.o + 0 BETWEEN 100 AND \
       j.lim"
    in
    Alcotest.(check (list (list string)))
      "bounded and unoptimizable foil agree"
      (rows_of db foil)
      (rows_of db bounded);
    Alcotest.(check int) "20 rows" 20 (List.length (rows_of db bounded));
    Alcotest.(check bool)
      (Printf.sprintf
         "the joined seek narrowed (%d examined vs the foil's %d)"
         (examined db bounded)
         (examined db foil))
      true
      (examined db bounded < examined db foil))
;;

(* A lower bound alone still moves the start key; the walk then runs to the end
   of the equality prefix's span. *)
let lower_bound_only_narrows () =
  with_db (fun db ->
    seed db;
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o >= 281"
      ~foil:"SELECT v FROM t WHERE w = 2 AND o + 0 >= 281"
      ~expect_examined:20)
;;

(* An upper bound alone stops the walk early. *)
let upper_bound_only_narrows () =
  with_db (fun db ->
    seed db;
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o <= 20"
      ~foil:"SELECT v FROM t WHERE w = 2 AND o + 0 <= 20"
      ~expect_examined:20)
;;

(* [100 <= o] constrains the same end as [o >= 100]; getting the direction
   backwards would seek to the wrong place and silently lose rows. *)
let reversed_operand_order_narrows () =
  with_db (fun db ->
    seed db;
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND 100 <= o AND 120 > o"
      ~foil:"SELECT v FROM t WHERE w = 2 AND 100 <= o + 0 AND 120 > o + 0"
      ~expect_examined:21)
;;

(* The bound is treated as inclusive at both ends whatever the SQL said, so a
   strict bound must still exclude its endpoint — the filter, not the seek, is
   what enforces that. *)
let strict_bounds_still_exclude_their_endpoints () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list (list string)))
      "o > 100 AND o < 103 is exactly 101 and 102"
      [ [ "2101" ]; [ "2102" ] ]
      (rows_of db "SELECT v FROM t WHERE w = 2 AND o > 100 AND o < 103");
    Alcotest.(check (list (list string)))
      "o >= 100 AND o <= 102 includes both endpoints"
      [ [ "2100" ]; [ "2101" ]; [ "2102" ] ]
      (rows_of db "SELECT v FROM t WHERE w = 2 AND o >= 100 AND o <= 102"))
;;

(* An empty window returns nothing and reads (almost) nothing. *)
let empty_window_reads_nothing () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list (list string)))
      "no rows"
      []
      (rows_of db "SELECT v FROM t WHERE w = 2 AND o >= 200 AND o < 200");
    Alcotest.(check bool)
      "and it did not scan the group to discover that"
      true
      (examined db "SELECT v FROM t WHERE w = 2 AND o >= 200 AND o < 200" < n_o))
;;

(* The DML seek shares the access path, so an UPDATE narrows the same way.
   Writes have no [rows_examined], so the narrowing is measured in page reads
   via the event callback, against an unoptimizable foil that also serves as the
   non-vacuity guard: if a warm cache flattened both counts to nothing, the
   foil's own count would fail the guard below. *)
let reads_during db sql =
  let n = ref 0 in
  Db.set_event_callback
    db
    (Some
       (function
         | Db.Event.Page_read _ | Db.Event.Wal_read _ -> incr n
         | _ -> ()));
  exec db sql;
  Db.set_event_callback db None;
  !n
;;

(* This case needs a FILE-backed database: an in-memory one has no pages to
   read, so [Page_read] never fires and both counts are zero — which the
   non-vacuity guard below catches rather than reporting a false pass. *)
let with_file_db f =
  let path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "granary-517-%d.db" (Unix.getpid ()))
  in
  List.iter
    (fun p ->
       try Unix.unlink p with
       | _ -> ())
    [ path; path ^ "-wal" ];
  let db = unwrap (run (Granary_unix.open_file_wal ~clock:Unix.gettimeofday ~path ())) in
  Fun.protect
    ~finally:(fun () ->
      (try run (Db.close db) with
       | _ -> ());
      List.iter
        (fun p ->
           try Unix.unlink p with
           | _ -> ())
        [ path; path ^ "-wal" ])
    (fun () -> f db)
;;

(* A much larger fixture too: at [seed]'s 900 rows the table stays entirely in
   the page cache. *)
let seed_large db =
  exec db "CREATE TABLE t (w INTEGER, o INTEGER, v INTEGER, PRIMARY KEY (w, o))";
  exec db "BEGIN";
  for w = 1 to 2 do
    for o = 1 to 10_000 do
      exec db (Printf.sprintf "INSERT INTO t VALUES (%d, %d, %d)" w o ((w * 100_000) + o))
    done
  done;
  exec db "COMMIT"
;;

let dml_range_narrows () =
  with_file_db (fun db ->
    seed_large db;
    let foil_reads =
      reads_during db "UPDATE t SET v = -2 WHERE w = 2 AND o + 0 >= 5000 AND o + 0 < 5020"
    in
    let bounded_reads =
      reads_during db "UPDATE t SET v = -1 WHERE w = 2 AND o >= 100 AND o < 120"
    in
    Alcotest.(check bool)
      (Printf.sprintf "the foil really does read the group (%d pages)" foil_reads)
      true
      (foil_reads > 20);
    Alcotest.(check bool)
      (Printf.sprintf
         "bounded UPDATE reads fewer pages (%d) than the foil (%d)"
         bounded_reads
         foil_reads)
      true
      (bounded_reads < foil_reads);
    (* Both statements must still have hit exactly their own 20 rows. *)
    Alcotest.(check int)
      "exactly the bounded window was updated"
      20
      (List.length (rows_of db "SELECT o FROM t WHERE v = -1"));
    Alcotest.(check (list (list string)))
      "and nothing outside it"
      []
      (rows_of db "SELECT o FROM t WHERE v = -1 AND (w <> 2 OR o < 100 OR o >= 120)"))
;;

(* ------------------------------------------------------------------ *)
(* Correctness where the bound cannot apply                             *)
(* ------------------------------------------------------------------ *)

(* A text column has a variable-width key encoding, so no bound is built. The
   result must still be right — this pins "unnarrowed", not "wrong". *)
let text_range_is_correct_without_a_bound () =
  with_db (fun db ->
    exec db "CREATE TABLE s (w INTEGER, name TEXT, v INTEGER, PRIMARY KEY (w, name))";
    exec db "BEGIN";
    List.iter
      (fun n -> exec db (Printf.sprintf "INSERT INTO s VALUES (1, '%s', %d)" n 1))
      [ "aa"; "ab"; "b"; "bb"; "c" ];
    exec db "COMMIT";
    Alcotest.(check (list (list string)))
      "range over a text key column"
      [ [ "ab" ]; [ "b" ] ]
      (rows_of db "SELECT name FROM s WHERE w = 1 AND name > 'aa' AND name <= 'b'"))
;;

(* NULLs IN the ranged column — the entries for which the stop test's comparison
   window runs past the column boundary, because a NULL encodes to one byte
   rather than the bounded types' nine.  What keeps that sound is the ordering
   of the encoding's type tags: [0x00] for NULL sorts below [0x01] for integer,
   so a misaligned window always compares low, the walk is never cut short, and
   the NULL entries sort to the front of the group anyway.

   This case exists because that is the input the justification turns on.  It
   pins both halves — the rows still match the unoptimizable foil, AND the
   narrowing is still live rather than silently disabled. *)
let nulls_in_the_ranged_column () =
  with_db (fun db ->
    exec db "CREATE TABLE n (w INTEGER, o INTEGER, v INTEGER)";
    exec db "CREATE INDEX idx_n ON n (w, o)";
    exec db "BEGIN";
    for w = 1 to 3 do
      for i = 1 to 5 do
        exec db (Printf.sprintf "INSERT INTO n VALUES (%d, NULL, %d)" w (-i))
      done;
      for o = 1 to 20 do
        exec db (Printf.sprintf "INSERT INTO n VALUES (%d, %d, %d)" w o ((w * 100) + o))
      done
    done;
    exec db "COMMIT";
    let check ~bounded ~foil ~expect_examined =
      Alcotest.(check (list (list string)))
        (bounded ^ " : rows")
        (rows_of db foil)
        (rows_of db bounded);
      Alcotest.(check int) (bounded ^ " : examined") expect_examined (examined db bounded)
    in
    (* Upper bound only: the five NULL entries sort first and are walked before
       the bound engages, so they are examined and then rejected. *)
    check
      ~bounded:"SELECT v FROM n WHERE w = 2 AND o <= 5"
      ~foil:"SELECT v FROM n WHERE w = 2 AND o + 0 <= 5"
      ~expect_examined:10;
    (* Lower bound only: the start key skips past the NULLs entirely. *)
    check
      ~bounded:"SELECT v FROM n WHERE w = 2 AND o >= 15"
      ~foil:"SELECT v FROM n WHERE w = 2 AND o + 0 >= 15"
      ~expect_examined:6;
    check
      ~bounded:"SELECT v FROM n WHERE w = 2 AND o >= 5 AND o <= 9"
      ~foil:"SELECT v FROM n WHERE w = 2 AND o + 0 >= 5 AND o + 0 <= 9"
      ~expect_examined:5;
    (* And the whole group, had nothing narrowed: 5 NULLs + 20 rows. *)
    Alcotest.(check int)
      "the foil really does read the whole group"
      25
      (examined db "SELECT v FROM n WHERE w = 2 AND o + 0 <= 5"))
;;

(* A NULL in a trailing index column, after the ranged one: [start] appends the
   minimum rowid straight after the lower bound rather than the remaining
   columns, so a key must never sort before it.  Every trailing byte is at least
   [0x00], so none can. *)
let nulls_in_a_column_after_the_ranged_one () =
  with_db (fun db ->
    exec db "CREATE TABLE m (w INTEGER, o INTEGER, k INTEGER, v INTEGER)";
    exec db "CREATE INDEX idx_m ON m (w, o, k)";
    exec db "BEGIN";
    for o = 1 to 20 do
      exec db (Printf.sprintf "INSERT INTO m VALUES (1, %d, NULL, %d)" o o);
      exec db (Printf.sprintf "INSERT INTO m VALUES (1, %d, %d, %d)" o o (100 + o))
    done;
    exec db "COMMIT";
    let bounded = "SELECT v FROM m WHERE w = 1 AND o >= 5 AND o <= 7" in
    let foil = "SELECT v FROM m WHERE w = 1 AND o + 0 >= 5 AND o + 0 <= 7" in
    Alcotest.(check (list (list string)))
      "rows agree with the unoptimizable foil"
      (rows_of db foil)
      (rows_of db bounded);
    (* Without this the case would be vacuous: a pure narrowing cannot change
       the rows, so the check above passes whether or not a bound was built —
       and with no bound there is no lower bound in the start key, which is the
       one code path this case exists to cover. *)
    Alcotest.(check int) "the foil reads the whole group" 40 (examined db foil);
    Alcotest.(check int) "the start key skips to the bound" 6 (examined db bounded))
;;

(* An inequality on a column that is NOT the one after the equality prefix must
   not be mistaken for a bound on it. *)
let inequality_on_another_column_is_not_a_bound () =
  with_db (fun db ->
    seed db;
    let sql = "SELECT o FROM t WHERE w = 2 AND v < 2010" in
    Alcotest.(check (list (list string)))
      "same as the unoptimizable foil"
      (rows_of db "SELECT o FROM t WHERE w = 2 AND v + 0 < 2010")
      (rows_of db sql))
;;

(* A NULL bound leaves that end unbounded rather than seeking the index's NULL
   entries; the predicate then rejects every row, as three-valued logic says. *)
let null_bound_matches_nothing () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list (list string)))
      "comparison with NULL is never true"
      []
      (rows_of db "SELECT v FROM t WHERE w = 2 AND o >= NULL"))
;;

(* #519: [NOT BETWEEN] parses as a negated [BETWEEN], so the conjunct is a
   [NOT] node and must not be read as a bound on the column — doing so would
   seek to the span the query excludes. *)
let not_between_is_not_a_bound () =
  with_db (fun db ->
    seed db;
    let sql = "SELECT v FROM t WHERE w = 2 AND o NOT BETWEEN 100 AND 119" in
    Alcotest.(check (list (list string)))
      "same as the unoptimizable foil"
      (rows_of db "SELECT v FROM t WHERE w = 2 AND o + 0 NOT BETWEEN 100 AND 119")
      (rows_of db sql);
    Alcotest.(check int) "280 of the group's 300 rows" 280 (List.length (rows_of db sql)))
;;

(* A NULL end of a [BETWEEN] leaves that end unbounded, exactly as a NULL
   inequality operand does; the predicate then rejects every row. *)
let null_between_bound_matches_nothing () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list (list string)))
      "NULL lower end"
      []
      (rows_of db "SELECT v FROM t WHERE w = 2 AND o BETWEEN NULL AND 119");
    Alcotest.(check (list (list string)))
      "NULL upper end"
      []
      (rows_of db "SELECT v FROM t WHERE w = 2 AND o BETWEEN 100 AND NULL"))
;;

(* #522: a [BETWEEN] end need not share the column's type. Evaluated through
   [compare_values] alone — which answers 0 for any cross-type pair — such an
   end compared equal in BOTH directions, so the predicate was true for every
   row while the equivalent inequalities were right all along. Each case pins
   the [BETWEEN] against both other spellings of the same test: the inequality
   pair it must equal, and the unoptimizable foil that takes no seek path, so
   neither the evaluator nor the seek can drift on its own. *)
let cross_type_between_agrees_with_inequalities () =
  with_db (fun db ->
    seed db;
    let check name ~between ~pair ~expect ~expect_examined =
      Alcotest.(check (list (list string)))
        (name ^ " : BETWEEN vs the inequality pair")
        (rows_of db pair)
        (rows_of db between);
      Alcotest.(check int)
        (name ^ " : rows returned")
        expect
        (List.length (rows_of db between));
      (* #527: how much of the 300-row group the seek had to read. *)
      Alcotest.(check int) (name ^ " : examined") expect_examined (examined db between)
    in
    (* Text ends against an integer column: no row compares in range, and there
       is nothing to promote a text bound into, so the whole group is read. *)
    check
      "text ends"
      ~between:"SELECT v FROM t WHERE w = 2 AND o BETWEEN '100' AND '119'"
      ~pair:"SELECT v FROM t WHERE w = 2 AND o >= '100' AND o <= '119'"
      ~expect:0
      ~expect_examined:300;
    (* Real ends against an integer column: the two promote, 101..119 match —
       and since #527 the seek reads exactly those 19 keys. *)
    check
      "real ends"
      ~between:"SELECT v FROM t WHERE w = 2 AND o BETWEEN 100.5 AND 119.5"
      ~pair:"SELECT v FROM t WHERE w = 2 AND o >= 100.5 AND o <= 119.5"
      ~expect:19
      ~expect_examined:19;
    (* One end the column's type, the other not: the mismatched end alone must
       still be able to reject a row.  Only the integer end bounds the seek, so
       the walk runs to the end of the group. *)
    check
      "one text end"
      ~between:"SELECT v FROM t WHERE w = 2 AND o BETWEEN 100 AND '119'"
      ~pair:"SELECT v FROM t WHERE w = 2 AND o >= 100 AND o <= '119'"
      ~expect:0
      ~expect_examined:201;
    check
      "one real end"
      ~between:"SELECT v FROM t WHERE w = 2 AND o BETWEEN 100.5 AND 119"
      ~pair:"SELECT v FROM t WHERE w = 2 AND o >= 100.5 AND o <= 119"
      ~expect:19
      ~expect_examined:19;
    (* The foil takes no seek path at all, so agreeing with it says the answer
       comes from the predicate rather than from a lucky bound. *)
    Alcotest.(check (list (list string)))
      "the unoptimizable foil agrees too"
      (rows_of db "SELECT v FROM t WHERE w = 2 AND o + 0 BETWEEN 100.5 AND 119.5")
      (rows_of db "SELECT v FROM t WHERE w = 2 AND o BETWEEN 100.5 AND 119.5"))
;;

(* #522, the other half of desugaring to a conjunction: a NULL end makes only
   ITS side unknown, not the whole predicate. [o BETWEEN 500 AND NULL] is false
   — not unknown — for every row here, because the lower end already answered
   false and [false AND unknown] is false. Returning unknown instead was
   invisible under a bare [WHERE] (both reject the row) but observable under
   [NOT], where unknown still rejects and false admits. *)
let null_end_of_between_is_three_valued () =
  with_db (fun db ->
    seed db;
    Alcotest.(check int)
      "NOT (o BETWEEN 500 AND NULL): the lower end is decisively false, so all 300 rows \
       come back"
      300
      (List.length
         (rows_of db "SELECT v FROM t WHERE w = 2 AND NOT (o BETWEEN 500 AND NULL)"));
    Alcotest.(check (list (list string)))
      "and it agrees with the inequality pair it desugars to"
      (rows_of db "SELECT v FROM t WHERE w = 2 AND NOT (o >= 500 AND o <= NULL)")
      (rows_of db "SELECT v FROM t WHERE w = 2 AND NOT (o BETWEEN 500 AND NULL)");
    (* Where neither end is decisive the result really is unknown, and [NOT] of
       unknown is unknown — so this one returns nothing. *)
    Alcotest.(check (list (list string)))
      "NOT (o BETWEEN 1 AND NULL) is unknown, which rejects"
      (rows_of db "SELECT v FROM t WHERE w = 2 AND NOT (o >= 1 AND o <= NULL)")
      (rows_of db "SELECT v FROM t WHERE w = 2 AND NOT (o BETWEEN 1 AND NULL)"))
;;

(* Reals are bounded too — the other fixed-width encoding. *)
let real_range_narrows () =
  with_db (fun db ->
    exec db "CREATE TABLE r (w INTEGER, x REAL, v INTEGER, PRIMARY KEY (w, x))";
    exec db "BEGIN";
    for i = 1 to 200 do
      exec db (Printf.sprintf "INSERT INTO r VALUES (1, %f, %d)" (float_of_int i /. 4.) i)
    done;
    exec db "COMMIT";
    check_narrows
      db
      ~bounded:"SELECT v FROM r WHERE w = 1 AND x >= 2.5 AND x < 5.0"
      ~foil:"SELECT v FROM r WHERE w = 1 AND x + 0 >= 2.5 AND x + 0 < 5.0"
        (* 10 rows in [2.5, 5.0) + the strict endpoint 5.0. *)
      ~expect_examined:11)
;;

(* ------------------------------------------------------------------ *)
(* #523: the tightest same-end conjunct wins                            *)
(* ------------------------------------------------------------------ *)

(* Two conjuncts constrain the SAME end. Taking the first one leaves the
   narrowing the second one offers on the floor: [o >= 150] must beat the
   [BETWEEN]'s [100]. *)
let tightest_lower_bound_wins () =
  with_db (fun db ->
    seed db;
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o BETWEEN 100 AND 200 AND o >= 150"
      ~foil:"SELECT v FROM t WHERE w = 2 AND o + 0 BETWEEN 100 AND 200 AND o + 0 >= 150"
        (* 150..200 inclusive. *)
      ~expect_examined:51)
;;

(* The mirror: the tighter of two upper bounds. *)
let tightest_upper_bound_wins () =
  with_db (fun db ->
    seed db;
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o BETWEEN 100 AND 200 AND o <= 150"
      ~foil:"SELECT v FROM t WHERE w = 2 AND o + 0 BETWEEN 100 AND 200 AND o + 0 <= 150"
      ~expect_examined:51)
;;

(* Pre-existing to #519: a plain pair of same-end inequalities has the same
   defect, in either order — the fold must not depend on the tighter one coming
   last. *)
let repeated_inequalities_fold () =
  with_db (fun db ->
    seed db;
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o >= 100 AND o >= 150 AND o <= 200"
      ~foil:
        "SELECT v FROM t WHERE w = 2 AND o + 0 >= 100 AND o + 0 >= 150 AND o + 0 <= 200"
      ~expect_examined:51;
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o >= 150 AND o >= 100 AND o <= 200"
      ~foil:
        "SELECT v FROM t WHERE w = 2 AND o + 0 >= 150 AND o + 0 >= 100 AND o + 0 <= 200"
      ~expect_examined:51;
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o >= 150 AND o <= 250 AND o <= 200"
      ~foil:
        "SELECT v FROM t WHERE w = 2 AND o + 0 >= 150 AND o + 0 <= 250 AND o + 0 <= 200"
      ~expect_examined:51)
;;

(* Folded bounds that contradict each other: the window is empty, and the seek
   must discover that without walking the group. *)
let contradictory_folded_bounds_read_nothing () =
  with_db (fun db ->
    seed db;
    let sql = "SELECT v FROM t WHERE w = 2 AND o BETWEEN 100 AND 200 AND o >= 250" in
    Alcotest.(check (list (list string))) "no rows" [] (rows_of db sql);
    let n = examined db sql in
    Alcotest.(check bool)
      (Printf.sprintf "and did not scan to find out (%d examined)" n)
      true
      (n <= 1))
;;

(* Same shape as {!stats_of}, for a prepared statement with bound parameters. *)
let stats_of_params db sql params =
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

let rows_of_params db sql params =
  let rows, _ = stats_of_params db sql params in
  List.sort compare (List.map (fun r -> Array.to_list (Array.map render r)) rows)
;;

let examined_params db sql params =
  let _, st = stats_of_params db sql params in
  st.Granary.Db.rows_examined
;;

(* A bound parameter's value is unknown at plan time, so it cannot take part in
   the fold. It must still be used when it is the only candidate for its end —
   the fold must not quietly prefer "no bound" to "a bound I cannot order". *)
let parameter_bound_alone_still_narrows () =
  with_db (fun db ->
    seed db;
    let sql = "SELECT v FROM t WHERE w = 2 AND o >= ?" in
    Alcotest.(check int) "rows" 20 (List.length (rows_of_params db sql [ Db.V_int 281L ]));
    Alcotest.(check int) "examined" 20 (examined_params db sql [ Db.V_int 281L ]))
;;

(* A mix of one orderable literal and one parameter on the same end: whichever
   the planner keeps, the rows must be right, because the predicate still runs
   on every row the seek yields. *)
let mixed_literal_and_parameter_bound_is_sound () =
  with_db (fun db ->
    seed db;
    let bounded = "SELECT v FROM t WHERE w = 2 AND o >= ? AND o >= 150 AND o <= 200" in
    let foil =
      "SELECT v FROM t WHERE w = 2 AND o + 0 >= ? AND o + 0 >= 150 AND o + 0 <= 200"
    in
    List.iter
      (fun p ->
         Alcotest.(check (list (list string)))
           (Printf.sprintf "param %Ld: agrees with the unoptimizable foil" p)
           (rows_of_params db foil [ Db.V_int p ])
           (rows_of_params db bounded [ Db.V_int p ]))
      [ 100L; 180L; 250L ])
;;

(* A same-end candidate whose literal type differs from the column's cannot be
   ordered against one that matches, and at run time it would not bound the
   seek at all. Keeping the orderable one is a narrowing; either way the rows
   must not move. (Cross-type comparison semantics are #522's business — the
   foil is compared against, not a hand-written expectation.) *)
let cross_type_same_end_bound_is_sound () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list (list string)))
      "real literal alongside an integer one, on an integer column"
      (rows_of
         db
         "SELECT v FROM t WHERE w = 2 AND o + 0 BETWEEN 100 AND 200 AND o + 0 >= 150.5")
      (rows_of db "SELECT v FROM t WHERE w = 2 AND o BETWEEN 100 AND 200 AND o >= 150.5");
    Alcotest.(check (list (list string)))
      "text literal alongside an integer one"
      (rows_of
         db
         "SELECT v FROM t WHERE w = 2 AND o + 0 BETWEEN 100 AND 200 AND o + 0 >= 'x'")
      (rows_of db "SELECT v FROM t WHERE w = 2 AND o BETWEEN 100 AND 200 AND o >= 'x'"))
;;

(* A text literal can never bound the seek on an integer column — there is
   nothing sound for {!Granary_sql.Exec}'s [range_bound_key] to turn it into, so
   it leaves that end unbounded. It must not shut out a candidate that CAN bound
   the seek just by being written first: both spellings must read the same.

   A cross-type NUMERIC literal is the other case, and since #527 it is not
   useless at all: the fold orders it against the integer candidate and the
   tighter one wins, in either written order.

   This is what {!cross_type_same_end_bound_is_sound} cannot catch — a pure
   narrowing never moves the rows, so pinning rows alone passes in either
   order. *)
let cross_type_bound_does_not_block_a_usable_one () =
  with_db (fun db ->
    seed db;
    let both_orders ~first ~second ~expect_examined =
      List.iter
        (fun sql ->
           let n = examined db sql in
           Alcotest.(check int) (Printf.sprintf "%s : examined" sql) expect_examined n)
        [ Printf.sprintf "SELECT v FROM t WHERE w = 2 AND %s AND %s" first second
        ; Printf.sprintf "SELECT v FROM t WHERE w = 2 AND %s AND %s" second first
        ]
    in
    (* 100..200 inclusive, whichever side of the useless conjunct it is on. *)
    both_orders ~first:"o >= 'x'" ~second:"o BETWEEN 100 AND 200" ~expect_examined:101;
    (* #527: [150.5] is the tighter lower bound and now wins outright — ceil to
       151, so 151..200. Order must not change that. *)
    both_orders ~first:"o >= 150.5" ~second:"o BETWEEN 100 AND 200" ~expect_examined:50)
;;

(* The fold applies to reals too, whose ordering is the other one it has to get
   right. *)
let tightest_real_bound_wins () =
  with_db (fun db ->
    exec db "CREATE TABLE r (w INTEGER, x REAL, v INTEGER, PRIMARY KEY (w, x))";
    exec db "BEGIN";
    for i = 1 to 200 do
      exec db (Printf.sprintf "INSERT INTO r VALUES (1, %f, %d)" (float_of_int i /. 4.) i)
    done;
    exec db "COMMIT";
    check_narrows
      db
      ~bounded:"SELECT v FROM r WHERE w = 1 AND x BETWEEN 2.5 AND 10.0 AND x >= 5.0"
      ~foil:"SELECT v FROM r WHERE w = 1 AND x + 0 BETWEEN 2.5 AND 10.0 AND x + 0 >= 5.0"
        (* 5.0 .. 10.0 in steps of 0.25, both ends inclusive. *)
      ~expect_examined:21)
;;

(* ------------------------------------------------------------------ *)
(* #527: a cross-type numeric bound is promoted by rounding OUTWARD      *)
(* ------------------------------------------------------------------ *)

(* A real bound on an INTEGER column used to be declined outright, so the whole
   equality prefix was scanned.  Promoting it to the enclosing integer is exact
   for an integer column — [o >= 280.5] is [o >= 281] and [o <= 20.5] is
   [o <= 20] — so the [expect_examined] numbers below are the tight ones.

   The direction is the whole point and is invisible in the rows: rounding a
   lower bound DOWN (or an upper bound UP) is still sound and returns the same
   rows, just one key wider, so only the exact examined counts pin it.  Rounding
   the other way — inward — would drop rows, which the foil comparison catches. *)
let real_bound_narrows_an_integer_column () =
  with_db (fun db ->
    seed db;
    (* ceil 280.5 = 281: 281..300. *)
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o >= 280.5"
      ~foil:"SELECT v FROM t WHERE w = 2 AND o + 0 >= 280.5"
      ~expect_examined:20;
    (* floor 20.5 = 20: 1..20. *)
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o <= 20.5"
      ~foil:"SELECT v FROM t WHERE w = 2 AND o + 0 <= 20.5"
      ~expect_examined:20;
    (* Both ends at once — the issue's repro shape. 101..119. *)
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o BETWEEN 100.5 AND 119.5"
      ~foil:"SELECT v FROM t WHERE w = 2 AND o + 0 BETWEEN 100.5 AND 119.5"
      ~expect_examined:19;
    (* A real that is already an integer must NOT be rounded away: [o >= 280.0]
       is [o >= 280], not [o >= 281], and its row 280 must come back. *)
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o >= 280.0"
      ~foil:"SELECT v FROM t WHERE w = 2 AND o + 0 >= 280.0"
      ~expect_examined:21;
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o <= 20.0"
      ~foil:"SELECT v FROM t WHERE w = 2 AND o + 0 <= 20.0"
      ~expect_examined:20;
    (* A strict real bound is still read inclusively, so an exactly-integral one
       costs the endpoint key that the predicate then rejects. *)
    check_narrows
      db
      ~bounded:"SELECT v FROM t WHERE w = 2 AND o > 280.0"
      ~foil:"SELECT v FROM t WHERE w = 2 AND o + 0 > 280.0"
      ~expect_examined:21)
;;

(* The regime the 1..300 cases above cannot reach: integer keys above 2^53,
   where a float ULP exceeds 1 and [Int64.to_float] stops being injective.

   The residual predicate does NOT compare an integer column against a real
   bound exactly — [compare_values] promotes the integer with [Int64.to_float]
   — so it admits every key whose ROUNDED value satisfies the bound, including
   keys strictly on the wrong side of it.  A seek built with plain
   [ceil]/[floor] sorts past exactly those keys and drops their rows, which is
   a regression against the declined-bound scan.  The promotion therefore
   widens by one float step first, and these cases are what pin that: each has
   at least one row the naive rounding excludes and the foil returns.

   The grid values, all chosen so that the stored key and its float image
   straddle the bound:
     9007199254740992 = 2^53           -> itself
     9007199254740993 = 2^53 + 1       -> 2^53          (ties to even, DOWN)
     9007199254740995 = 2^53 + 3       -> 2^53 + 4      (ties to even, UP)
     4611686018427387804 = 2^62 - 100  -> 2^62          (ULP 512, UP)
     4611686018427388004 = 2^62 + 100  -> 2^62          (ULP 1024, DOWN) *)
let real_bound_on_a_huge_integer_column () =
  with_db (fun db ->
    exec db "CREATE TABLE g (w INTEGER, o INTEGER, v INTEGER, PRIMARY KEY (w, o))";
    exec db "BEGIN";
    List.iteri
      (fun i o -> exec db (Printf.sprintf "INSERT INTO g VALUES (1, %s, %d)" o i))
      [ "9007199254740992"
      ; "9007199254740993"
      ; "9007199254740995"
      ; "4611686018427387804"
      ; "4611686018427388004"
      ];
    exec db "COMMIT";
    let check ~bounded ~foil ~expect_examined =
      Alcotest.(check (list (list string)))
        (bounded ^ " : agrees with the unoptimizable foil")
        (rows_of db foil)
        (rows_of db bounded);
      Alcotest.(check int) (bounded ^ " : examined") expect_examined (examined db bounded)
    in
    (* Lower bound at 2^62.  2^62 - 100 rounds UP onto the bound, so the
       predicate admits it; [ceil 2^62] would seek past it. *)
    check
      ~bounded:"SELECT v FROM g WHERE w = 1 AND o >= 4611686018427387904.0"
      ~foil:"SELECT v FROM g WHERE w = 1 AND o + 0 >= 4611686018427387904.0"
      ~expect_examined:2;
    (* Upper bound at 2^62.  2^62 + 100 rounds DOWN onto it; [floor 2^62] would
       stop the walk one key early. *)
    check
      ~bounded:"SELECT v FROM g WHERE w = 1 AND o <= 4611686018427387904.0"
      ~foil:"SELECT v FROM g WHERE w = 1 AND o + 0 <= 4611686018427387904.0"
      ~expect_examined:5;
    (* Upper bound at 2^53: 2^53 + 1 is above it but rounds down onto it. *)
    check
      ~bounded:"SELECT v FROM g WHERE w = 1 AND o <= 9007199254740992.0"
      ~foil:"SELECT v FROM g WHERE w = 1 AND o + 0 <= 9007199254740992.0"
      ~expect_examined:2;
    (* Lower bound at 2^53 + 4: 2^53 + 3 is below it but rounds up onto it. *)
    check
      ~bounded:"SELECT v FROM g WHERE w = 1 AND o >= 9007199254740996.0"
      ~foil:"SELECT v FROM g WHERE w = 1 AND o + 0 >= 9007199254740996.0"
      ~expect_examined:3;
    (* Both ends at once, spanning the whole grid. *)
    check
      ~bounded:
        "SELECT v FROM g WHERE w = 1 AND o BETWEEN 9007199254740992.0 AND \
         4611686018427387904.0"
      ~foil:
        "SELECT v FROM g WHERE w = 1 AND o + 0 BETWEEN 9007199254740992.0 AND \
         4611686018427387904.0"
      ~expect_examined:5)
;;

(* The mirror direction: an integer bound on a REAL column. Every integer here
   is exactly representable, so both ends land on the value itself. *)
let integer_bound_narrows_a_real_column () =
  with_db (fun db ->
    exec db "CREATE TABLE r (w INTEGER, x REAL, v INTEGER, PRIMARY KEY (w, x))";
    exec db "BEGIN";
    for i = 1 to 200 do
      exec db (Printf.sprintf "INSERT INTO r VALUES (1, %f, %d)" (float_of_int i /. 4.) i)
    done;
    exec db "COMMIT";
    (* 49.0 .. 50.0 in steps of 0.25. *)
    check_narrows
      db
      ~bounded:"SELECT v FROM r WHERE w = 1 AND x >= 49"
      ~foil:"SELECT v FROM r WHERE w = 1 AND x + 0 >= 49"
      ~expect_examined:5;
    (* 0.25 .. 1.0. *)
    check_narrows
      db
      ~bounded:"SELECT v FROM r WHERE w = 1 AND x <= 1"
      ~foil:"SELECT v FROM r WHERE w = 1 AND x + 0 <= 1"
      ~expect_examined:4;
    check_narrows
      db
      ~bounded:"SELECT v FROM r WHERE w = 1 AND x BETWEEN 49 AND 50"
      ~foil:"SELECT v FROM r WHERE w = 1 AND x + 0 BETWEEN 49 AND 50"
      ~expect_examined:5;
    (* The int64 extremes, where [Int64.to_float] is no longer exact and the
       promotion has to step the float back outward. Neither may produce a bound
       that excludes a row it should admit: [x <= max_int] admits all 200. *)
    check_narrows
      db
      ~bounded:"SELECT v FROM r WHERE w = 1 AND x <= 9223372036854775807"
      ~foil:"SELECT v FROM r WHERE w = 1 AND x + 0 <= 9223372036854775807"
      ~expect_examined:200;
    check_narrows
      db
      ~bounded:"SELECT v FROM r WHERE w = 1 AND x >= -9223372036854775808"
      ~foil:"SELECT v FROM r WHERE w = 1 AND x + 0 >= -9223372036854775808"
      ~expect_examined:200;
    (* And the empty directions: no row is anywhere near either extreme. *)
    check_narrows
      db
      ~bounded:"SELECT v FROM r WHERE w = 1 AND x >= 9223372036854775807"
      ~foil:"SELECT v FROM r WHERE w = 1 AND x + 0 >= 9223372036854775807"
      ~expect_examined:0;
    check_narrows
      db
      ~bounded:"SELECT v FROM r WHERE w = 1 AND x <= -9223372036854775808"
      ~foil:"SELECT v FROM r WHERE w = 1 AND x + 0 <= -9223372036854775808"
      ~expect_examined:0)
;;

(* NaN and the infinities cannot be spelled as literals, so they arrive as bound
   parameters. None of them may produce a wrong key.

   NaN gets exactly the treatment a same-type NaN bound already gets: it encodes
   to the single [0x00] NULL/NaN byte, which sorts below every other key. That
   agrees with the residual predicate, whose [Float.compare] also orders NaN
   below every number — so a NaN LOWER bound admits every row and a NaN UPPER
   bound stops the walk on the first key. The expectations below are the foil's,
   not SQLite's: SQLite has no NaN at all (it stores one as NULL, which would
   make both ends match nothing), and that divergence is the comparison stack's
   business, not the seek's. It is tracked as #536 — these cases pin the
   current behaviour, not a decision that it is the right one. *)
let nan_and_infinite_bounds_are_sound () =
  with_db (fun db ->
    seed db;
    let check name ~sql ~foil ~params ~expect_rows ~expect_examined =
      Alcotest.(check (list (list string)))
        (name ^ " : agrees with the unoptimizable foil")
        (rows_of_params db foil params)
        (rows_of_params db sql params);
      Alcotest.(check int)
        (name ^ " : rows returned")
        expect_rows
        (List.length (rows_of_params db sql params));
      Alcotest.(check int)
        (name ^ " : examined")
        expect_examined
        (examined_params db sql params)
    in
    let lo = "SELECT v FROM t WHERE w = 2 AND o >= ?"
    and lo_foil = "SELECT v FROM t WHERE w = 2 AND o + 0 >= ?"
    and hi = "SELECT v FROM t WHERE w = 2 AND o <= ?"
    and hi_foil = "SELECT v FROM t WHERE w = 2 AND o + 0 <= ?" in
    check
      "NaN lower bound"
      ~sql:lo
      ~foil:lo_foil
      ~params:[ Db.V_real Float.nan ]
      ~expect_rows:300
      ~expect_examined:300;
    check
      "NaN upper bound"
      ~sql:hi
      ~foil:hi_foil
      ~params:[ Db.V_real Float.nan ]
      ~expect_rows:0
      ~expect_examined:0;
    (* An infinity has no integer to round to, so the end is left unbounded —
       never overflowed into a key that would drop rows. *)
    check
      "+inf upper bound admits every row"
      ~sql:hi
      ~foil:hi_foil
      ~params:[ Db.V_real Float.infinity ]
      ~expect_rows:300
      ~expect_examined:300;
    check
      "-inf lower bound admits every row"
      ~sql:lo
      ~foil:lo_foil
      ~params:[ Db.V_real Float.neg_infinity ]
      ~expect_rows:300
      ~expect_examined:300;
    check
      "+inf lower bound admits none"
      ~sql:lo
      ~foil:lo_foil
      ~params:[ Db.V_real Float.infinity ]
      ~expect_rows:0
      ~expect_examined:300;
    (* Out of int64 range: declining is the only safe answer.  The two
       admit-everything cases are the load-bearing ones — an overflowed key
       would silently drop all 300 rows. *)
    check
      "huge upper bound admits every row"
      ~sql:hi
      ~foil:hi_foil
      ~params:[ Db.V_real 1e30 ]
      ~expect_rows:300
      ~expect_examined:300;
    check
      "huge negative lower bound admits every row"
      ~sql:lo
      ~foil:lo_foil
      ~params:[ Db.V_real (-1e30) ]
      ~expect_rows:300
      ~expect_examined:300)
;;

(* ------------------------------------------------------------------ *)
(* Property                                                             *)
(* ------------------------------------------------------------------ *)

(* #523: four same-end constraints at once — whatever the fold keeps, the rows
   must equal the unoptimizable foil's.  Both ends get two INDEPENDENT
   candidates: the lower end [a] (from the [BETWEEN]) against [b], the upper end
   [c] against [d].  With the same value on both upper candidates the two would
   always compare equal and a [>=]/[<=] slip in the fold's [`Hi] arm would go
   unseen under random input. *)
let prop_folded_bounds_match_foil =
  QCheck.Test.make
    ~count:200
    ~name:"folded same-end bounds agree with unoptimizable foil"
    QCheck.(
      quad
        (int_range 1 3)
        (int_range (-2) 14)
        (int_range (-2) 14)
        (pair (int_range (-2) 14) (int_range (-2) 14)))
    (fun (w, a, b, (c, d)) ->
       with_db (fun db ->
         exec db "CREATE TABLE t (w INTEGER, o INTEGER, v INTEGER, PRIMARY KEY (w, o))";
         exec db "BEGIN";
         for wi = 1 to 3 do
           for o = 1 to 12 do
             exec
               db
               (Printf.sprintf "INSERT INTO t VALUES (%d, %d, %d)" wi o ((wi * 100) + o))
           done
         done;
         exec db "COMMIT";
         let q col =
           rows_of
             db
             (Printf.sprintf
                "SELECT v FROM t WHERE w = %d AND %s BETWEEN %d AND %d AND %s >= %d AND \
                 %s <= %d"
                w
                col
                a
                c
                col
                b
                col
                d)
         in
         q "o" = q "o + 0"))
;;

let prop_range_matches_foil =
  QCheck.Test.make
    ~count:200
    ~name:"bounded seek agrees with unoptimizable foil"
    QCheck.(triple (int_range 1 3) (int_range (-2) 14) (int_range (-2) 14))
    (fun (w, lo, hi) ->
       with_db (fun db ->
         exec db "CREATE TABLE t (w INTEGER, o INTEGER, v INTEGER, PRIMARY KEY (w, o))";
         exec db "BEGIN";
         for wi = 1 to 3 do
           for o = 1 to 12 do
             exec
               db
               (Printf.sprintf "INSERT INTO t VALUES (%d, %d, %d)" wi o ((wi * 100) + o))
           done
         done;
         exec db "COMMIT";
         let q col =
           rows_of
             db
             (Printf.sprintf
                "SELECT v FROM t WHERE w = %d AND %s > %d AND %s <= %d"
                w
                col
                lo
                col
                hi)
         in
         q "o" = q "o + 0"))
;;

let prop_between_matches_foil =
  QCheck.Test.make
    ~count:200
    ~name:"bounded BETWEEN seek agrees with unoptimizable foil"
    QCheck.(triple (int_range 1 3) (int_range (-2) 14) (int_range (-2) 14))
    (fun (w, lo, hi) ->
       with_db (fun db ->
         exec db "CREATE TABLE t (w INTEGER, o INTEGER, v INTEGER, PRIMARY KEY (w, o))";
         exec db "BEGIN";
         for wi = 1 to 3 do
           for o = 1 to 12 do
             exec
               db
               (Printf.sprintf "INSERT INTO t VALUES (%d, %d, %d)" wi o ((wi * 100) + o))
           done
         done;
         exec db "COMMIT";
         let q col =
           rows_of
             db
             (Printf.sprintf
                "SELECT v FROM t WHERE w = %d AND %s BETWEEN %d AND %d"
                w
                col
                lo
                hi)
         in
         q "o" = q "o + 0"))
;;

(* #522: whatever the ends' types, [x BETWEEN a AND b] must be exactly
   [x >= a AND x <= b]. The generator spells each end as an integer, a real or
   a text literal independently, so most draws are cross-type. *)
let spell_literal which n =
  match which with
  | 0 -> string_of_int n
  | 1 -> Printf.sprintf "%d.5" n
  | _ -> Printf.sprintf "'%d'" n
;;

let prop_cross_type_between_matches_inequalities =
  QCheck.Test.make
    ~count:200
    ~name:"cross-type BETWEEN agrees with the inequality pair"
    QCheck.(quad (int_range 0 2) (int_range 0 2) (int_range (-2) 14) (int_range (-2) 14))
    (fun (lo_kind, hi_kind, lo_n, hi_n) ->
       let lo = spell_literal lo_kind lo_n
       and hi = spell_literal hi_kind hi_n in
       with_db (fun db ->
         exec db "CREATE TABLE t (w INTEGER, o INTEGER, v INTEGER, PRIMARY KEY (w, o))";
         exec db "BEGIN";
         for o = 1 to 12 do
           exec db (Printf.sprintf "INSERT INTO t VALUES (1, %d, %d)" o (100 + o))
         done;
         exec db "COMMIT";
         let between col =
           rows_of
             db
             (Printf.sprintf
                "SELECT v FROM t WHERE w = 1 AND %s BETWEEN %s AND %s"
                col
                lo
                hi)
         in
         let pair col =
           rows_of
             db
             (Printf.sprintf
                "SELECT v FROM t WHERE w = 1 AND %s >= %s AND %s <= %s"
                col
                lo
                col
                hi)
         in
         between "o" = pair "o" && between "o" = between "o + 0"))
;;

(* #527: a promoted cross-type bound must return exactly what the unoptimizable
   [o + 0] foil does.  Ends are drawn in halves so both the rounding case (a
   [.5] end, where the direction matters) and the exact case (a [.0] end, which
   must not be rounded away) come up, and each spelling — the inequality pair
   and the [BETWEEN] — is checked against its own foil. *)
let prop_cross_type_range_bound_matches_foil =
  QCheck.Test.make
    ~count:200
    ~name:"promoted cross-type range bound agrees with unoptimizable foil"
    QCheck.(triple (int_range 1 3) (int_range (-4) 28) (int_range (-4) 28))
    (fun (w, lo2, hi2) ->
       let lo = Printf.sprintf "%.1f" (float_of_int lo2 /. 2.)
       and hi = Printf.sprintf "%.1f" (float_of_int hi2 /. 2.) in
       with_db (fun db ->
         exec db "CREATE TABLE t (w INTEGER, o INTEGER, v INTEGER, PRIMARY KEY (w, o))";
         exec db "BEGIN";
         for wi = 1 to 3 do
           for o = 1 to 12 do
             exec
               db
               (Printf.sprintf "INSERT INTO t VALUES (%d, %d, %d)" wi o ((wi * 100) + o))
           done
         done;
         exec db "COMMIT";
         let pair col =
           rows_of
             db
             (Printf.sprintf
                "SELECT v FROM t WHERE w = %d AND %s > %s AND %s <= %s"
                w
                col
                lo
                col
                hi)
         in
         let between col =
           rows_of
             db
             (Printf.sprintf
                "SELECT v FROM t WHERE w = %d AND %s BETWEEN %s AND %s"
                w
                col
                lo
                hi)
         in
         pair "o" = pair "o + 0" && between "o" = between "o + 0"))
;;

let () =
  Alcotest.run
    "range_bound_517"
    [ ( "narrowing"
      , [ Alcotest.test_case "two-sided range narrows" `Quick two_sided_range_narrows
        ; Alcotest.test_case "lower bound only narrows" `Quick lower_bound_only_narrows
        ; Alcotest.test_case "upper bound only narrows" `Quick upper_bound_only_narrows
        ; Alcotest.test_case
            "reversed operand order narrows"
            `Quick
            reversed_operand_order_narrows
        ; Alcotest.test_case
            "empty window reads nothing"
            `Quick
            empty_window_reads_nothing
        ; Alcotest.test_case "BETWEEN narrows" `Quick between_narrows
        ; Alcotest.test_case
            "half-recognisable BETWEEN narrows"
            `Quick
            half_recognisable_between_narrows
        ; Alcotest.test_case
            "BETWEEN and inequality compose"
            `Quick
            between_and_inequality_compose
        ; Alcotest.test_case
            "BETWEEN under a join narrows"
            `Quick
            between_under_a_join_narrows
        ; Alcotest.test_case "DML range narrows" `Quick dml_range_narrows
        ; Alcotest.test_case "real range narrows" `Quick real_range_narrows
        ; Alcotest.test_case "tightest lower bound wins" `Quick tightest_lower_bound_wins
        ; Alcotest.test_case "tightest upper bound wins" `Quick tightest_upper_bound_wins
        ; Alcotest.test_case
            "repeated inequalities fold"
            `Quick
            repeated_inequalities_fold
        ; Alcotest.test_case "tightest real bound wins" `Quick tightest_real_bound_wins
        ; Alcotest.test_case
            "parameter bound alone still narrows"
            `Quick
            parameter_bound_alone_still_narrows
        ; Alcotest.test_case
            "real bound narrows an integer column"
            `Quick
            real_bound_narrows_an_integer_column
        ; Alcotest.test_case
            "integer bound narrows a real column"
            `Quick
            integer_bound_narrows_a_real_column
        ; Alcotest.test_case
            "real bound on a huge integer column"
            `Quick
            real_bound_on_a_huge_integer_column
        ] )
    ; ( "correctness"
      , [ Alcotest.test_case
            "strict bounds still exclude their endpoints"
            `Quick
            strict_bounds_still_exclude_their_endpoints
        ; Alcotest.test_case
            "text range is correct without a bound"
            `Quick
            text_range_is_correct_without_a_bound
        ; Alcotest.test_case
            "inequality on another column is not a bound"
            `Quick
            inequality_on_another_column_is_not_a_bound
        ; Alcotest.test_case
            "NULL bound matches nothing"
            `Quick
            null_bound_matches_nothing
        ; Alcotest.test_case
            "NOT BETWEEN is not a bound"
            `Quick
            not_between_is_not_a_bound
        ; Alcotest.test_case
            "NULL BETWEEN bound matches nothing"
            `Quick
            null_between_bound_matches_nothing
        ; Alcotest.test_case
            "cross-type BETWEEN agrees with inequalities"
            `Quick
            cross_type_between_agrees_with_inequalities
        ; Alcotest.test_case
            "NULL end of BETWEEN is three-valued"
            `Quick
            null_end_of_between_is_three_valued
        ; Alcotest.test_case
            "NULLs in the ranged column"
            `Quick
            nulls_in_the_ranged_column
        ; Alcotest.test_case
            "NULLs in a column after the ranged one"
            `Quick
            nulls_in_a_column_after_the_ranged_one
        ; Alcotest.test_case
            "contradictory folded bounds read nothing"
            `Quick
            contradictory_folded_bounds_read_nothing
        ; Alcotest.test_case
            "mixed literal and parameter bound is sound"
            `Quick
            mixed_literal_and_parameter_bound_is_sound
        ; Alcotest.test_case
            "cross-type same-end bound is sound"
            `Quick
            cross_type_same_end_bound_is_sound
        ; Alcotest.test_case
            "cross-type bound does not block a usable one"
            `Quick
            cross_type_bound_does_not_block_a_usable_one
        ; Alcotest.test_case
            "NaN and infinite bounds are sound"
            `Quick
            nan_and_infinite_bounds_are_sound
        ] )
    ; ( "property"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_range_matches_foil
          ; prop_between_matches_foil
          ; prop_cross_type_between_matches_inequalities
          ; prop_folded_bounds_match_foil
          ; prop_cross_type_range_bound_matches_foil
          ] )
    ]
;;
