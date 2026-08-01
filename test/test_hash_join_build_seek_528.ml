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
    the base rows the executor pulled. *)

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

(* The headline case. The foil reads all [n_stock] rows of stock; the seek reads
   only warehouse 1's [n_per_w]. Both read the same [n_line] driving rows. *)
let build_side_seeks_the_pinned_prefix () =
  with_db (fun db ->
    seed db ();
    same_rows
      db
      ~label:"seeked and scanned build sides agree"
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
      "the seek reads only warehouse 1's stock"
      (n_line + n_per_w)
      (examined db seek_sql))
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
   build side was scanned for the same reason. The soundness argument does not
   depend on the ON predicate's shape, so it narrows too. *)
let general_on_predicate_build_side_seeks () =
  with_db (fun db ->
    seed db ~n_line:20 ~n_per_w:20 ();
    let seek = "SELECT qty FROM line INNER JOIN stock ON si > i_id WHERE sw = 1" in
    let foil = "SELECT qty FROM line INNER JOIN stock ON si > i_id WHERE sw + 0 = 1" in
    same_rows db ~label:"cartesian build side agrees with its scan" ~seek ~foil;
    Alcotest.(check bool)
      (Printf.sprintf
         "examined %d is below a full scan of stock (%d)"
         (examined db seek)
         (examined db foil))
      true
      (examined db seek < examined db foil))
;;

(* ------------------------------------------------------------------ *)
(* #526: the cost model must follow the narrowed build side             *)
(* ------------------------------------------------------------------ *)

(* #526 compares D seeks against D + R reads, with R the right table's size. Now
   that the build side can seek, R is the size of the SEEKED SUBSET, and the
   estimate has to say so or the comparison is against a table the plan never
   reads.

   Here the WHERE clause pins stock's whole primary key, so the build side reads
   exactly one row. Costing R at the table's 20,000-row high-water mark makes
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
    (* Not vacuous: the narrowing really fired. *)
    Alcotest.(check bool)
      (Printf.sprintf "examined %d < foil %d" (examined db seek) (examined db foil))
      true
      (examined db seek < examined db foil);
    (* And it really did suppress matches: warehouse 2's 777 rows are gone. *)
    Alcotest.(check bool)
      "no warehouse-2 quantity survives"
      false
      (List.exists (fun r -> List.nth r 1 = "2777") (rows_of db seek)))
;;

(* A LEFT JOIN whose driving row has a NULL join key still null-extends, whatever
   the build side reads. *)
let left_join_null_key_null_extends () =
  with_db (fun db ->
    exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
    exec
      db
      "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
    exec db "BEGIN";
    for o = 1 to n_line do
      exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, NULL)" o)
    done;
    exec db "INSERT INTO stock VALUES (1, 8, 80)";
    exec db "COMMIT";
    let rows = rows_of db "SELECT o, qty FROM line LEFT JOIN stock ON si = i_id" in
    Alcotest.(check int) "every NULL-keyed row survives" n_line (List.length rows);
    Alcotest.(check bool)
      "null-extended"
      true
      (List.for_all (fun r -> List.nth r 1 = "NULL") rows))
;;

(* A pinned prefix that matches nothing must yield no rows, not the index's NULL
   entries and not a wrongly-truncated prefix. *)
let constant_matching_nothing_yields_nothing () =
  with_db (fun db ->
    seed db ();
    Alcotest.(check (list (list string)))
      "no warehouse 9"
      []
      (rows_of
         db
         "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 9");
    Alcotest.(check int)
      "and the build side read nothing beyond the driving rows"
      n_line
      (examined
         db
         "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 9"))
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
   below it the planner takes a probe and the build side never runs at all. *)
let n_line_prop = 1050

let prop_build_seek_matches_foil =
  QCheck.Test.make
    ~count:20
    ~name:"hash-join build-side seek agrees with unoptimizable foil"
    QCheck.(triple (int_range 0 3) (int_range 0 12) bool)
    (fun (sw, i_hi, left) ->
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
         let q pred =
           rows_of
             db
             (Printf.sprintf
                "SELECT o, qty FROM line %s JOIN stock ON si = i_id WHERE %s"
                kind
                pred)
         in
         q (Printf.sprintf "sw = %d AND i_id < %d" sw i_hi)
         = q (Printf.sprintf "sw + 0 = %d AND i_id < %d" sw i_hi)))
;;

let () =
  Alcotest.run
    "hash_join_build_seek_528"
    [ ( "narrowing"
      , [ Alcotest.test_case
            "build side seeks the pinned prefix"
            `Quick
            build_side_seeks_the_pinned_prefix
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
            "general ON predicate build side seeks"
            `Quick
            general_on_predicate_build_side_seeks
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
