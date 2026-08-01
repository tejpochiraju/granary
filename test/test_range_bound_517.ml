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
(* Property                                                             *)
(* ------------------------------------------------------------------ *)

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
            "NULLs in the ranged column"
            `Quick
            nulls_in_the_ranged_column
        ; Alcotest.test_case
            "NULLs in a column after the ranged one"
            `Quick
            nulls_in_a_column_after_the_ranged_one
        ] )
    ; ( "property"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_range_matches_foil; prop_between_matches_foil ] )
    ]
;;
