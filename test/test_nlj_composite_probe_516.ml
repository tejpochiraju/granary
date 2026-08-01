(** #516: a nested-loop join must be able to probe a multi-column index whose
    leading columns come from the WHERE clause and whose join column comes from
    the ON predicate.

    [find_index_on_col] accepted an index only if it had exactly one column, so
    a right table keyed on [(a, b)] and joined on [b] had no usable probe and
    fell back to [Op_hash_join] — which reads every row of the right table. In
    TPC-C StockLevel that was a full scan of [stock] per query, 100,000 rows,
    and the dominant remaining cost after #515.

    The full key IS available; it just arrives from two places. [a] is pinned by
    a WHERE equality and [b] by the join, so the probe key is a mix of constants
    and left-row values in index-column order.

    Safety rests on the same invariant as #515: the constant parts come only
    from the top-level AND spine of the WHERE clause, and [chain_joins] still
    applies that whole clause to the joined row. So a constant can only narrow
    what each probe reads. This matters most for LEFT JOIN, where a narrowed
    probe can null-extend a row that previously matched — that row's right
    columns are then NULL, the WHERE conjunct that narrowed the probe is NULL on
    it, and the post-join filter drops it. The row it replaced would have failed
    the same conjunct. An [OR] cannot reach the probe, because [conjuncts] never
    splits one.

    [rows_examined] is the load-bearing assertion, measured as the difference
    against an unoptimizable foil so the left side's own cost cancels out. *)

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

(* [rows_examined] counts base rows pulled on both sides of the join. Measuring
   a query against its unoptimizable foil cancels the left side, leaving exactly
   what the probe saved on the right. *)
let right_rows_saved db ~probe ~foil = examined db foil - examined db probe

(* Assert that [probe] and [foil] are the same query semantically, and report
   how much the probe saved. *)
let same_rows db ~probe ~foil =
  Alcotest.(check (list (list string)))
    "probe and unoptimizable foil agree"
    (rows_of db foil)
    (rows_of db probe)
;;

(* [line] is the driving side, [stock] the probed side keyed on [(sw, si)] —
   the TPC-C shape, where the join is on [si] alone and [sw] comes from the
   WHERE clause. [n_stock] is made much larger than the number of driving rows
   so a full scan of [stock] is unmistakable. *)
let n_line = 5
let n_stock = 400

let seed db =
  exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
  exec db "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
  exec db "BEGIN";
  for o = 1 to n_line do
    exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o o)
  done;
  (* Two warehouses' worth of stock, so the WHERE constant is doing real work. *)
  for si = 1 to n_stock / 2 do
    exec db (Printf.sprintf "INSERT INTO stock VALUES (1, %d, %d)" si (si * 10));
    exec db (Printf.sprintf "INSERT INTO stock VALUES (2, %d, %d)" si (si * 20))
  done;
  exec db "COMMIT"
;;

(* ------------------------------------------------------------------ *)
(* The probe                                                            *)
(* ------------------------------------------------------------------ *)

(* The headline case: [sw] from the WHERE clause, [si] from the ON predicate,
   together covering stock's whole primary key. *)
let where_constant_completes_the_probe_key () =
  with_db (fun db ->
    seed db;
    let probe =
      "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw = 1"
    in
    let foil =
      "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND sw + 0 = 1"
    in
    same_rows db ~probe ~foil;
    Alcotest.(check int)
      "one joined row per driving row"
      n_line
      (List.length (rows_of db probe));
    (* The foil reads all of [stock]; the probe reads one row per driving row. *)
    Alcotest.(check int)
      "probe reads one stock row per driving row instead of the whole table"
      (n_stock - n_line)
      (right_rows_saved db ~probe ~foil))
;;

(* A single-column index must keep working exactly as before: this is the path
   that already existed, and the probe key for it is a one-element list.
   [code] carries an explicit single-column index — an INTEGER PRIMARY KEY would
   not, being a rowid alias with no index tree of its own, and that case joins
   by hash scan today (see the header of the follow-up issue). *)
let single_column_index_still_probes () =
  with_db (fun db ->
    exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
    exec db "CREATE TABLE item (it_id INTEGER, code INTEGER, qty INTEGER)";
    exec db "CREATE INDEX idx_item_code ON item (code)";
    exec db "BEGIN";
    for o = 1 to n_line do
      exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o o)
    done;
    for i = 1 to n_stock do
      exec db (Printf.sprintf "INSERT INTO item VALUES (%d, %d, %d)" i i (i * 10))
    done;
    exec db "COMMIT";
    let sql = "SELECT qty FROM line INNER JOIN item ON code = i_id WHERE w = 1" in
    Alcotest.(check int)
      "one joined row per driving row"
      n_line
      (List.length (rows_of db sql));
    (* [n_line] driving rows + [n_line] probed item rows, nowhere near [n_stock]. *)
    Alcotest.(check bool)
      (Printf.sprintf
         "examined %d stays proportional to the driving side"
         (examined db sql))
      true
      (examined db sql < n_stock))
;;

(* The join column must be part of what the probe pins. An index the WHERE
   clause alone covers is not a join probe — every left row would read the same
   range — and must not be mistaken for one. *)
let where_constants_alone_do_not_form_a_probe () =
  with_db (fun db ->
    seed db;
    (* [qty] is not indexed, so nothing about the ON predicate is seekable. *)
    let probe =
      "SELECT qty FROM line INNER JOIN stock ON qty = i_id WHERE w = 1 AND sw = 1"
    in
    let foil =
      "SELECT qty FROM line INNER JOIN stock ON qty = i_id WHERE w = 1 AND sw + 0 = 1"
    in
    same_rows db ~probe ~foil)
;;

(* A WHERE equality on a column that is not the index's leading column cannot
   complete the key, and the probe must fall back rather than seek a wrong
   prefix. Here the constant pins [si] while the join is on [si] too — the
   leading [sw] is unpinned. *)
let unpinned_leading_column_falls_back () =
  with_db (fun db ->
    seed db;
    let probe =
      "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND qty = 30"
    in
    let foil =
      "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE w = 1 AND qty + 0 = 30"
    in
    same_rows db ~probe ~foil)
;;

(* ------------------------------------------------------------------ *)
(* Correctness the probe must not disturb                               *)
(* ------------------------------------------------------------------ *)

(* The LEFT JOIN case the header argues about: a narrowed probe null-extends
   rows that a wider probe would have matched, and the post-join filter must
   drop them. *)
let left_join_with_a_narrowing_constant () =
  with_db (fun db ->
    exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
    exec
      db
      "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
    exec db "BEGIN";
    exec db "INSERT INTO line VALUES (1, 1, 7)";
    exec db "INSERT INTO line VALUES (1, 2, 8)";
    (* i_id 7 exists only in warehouse 2, so the [sw = 1] constant suppresses a
       match the ON predicate alone would have found. *)
    exec db "INSERT INTO stock VALUES (2, 7, 70)";
    exec db "INSERT INTO stock VALUES (1, 8, 80)";
    exec db "COMMIT";
    let probe = "SELECT o, qty FROM line LEFT JOIN stock ON si = i_id WHERE sw = 1" in
    let foil = "SELECT o, qty FROM line LEFT JOIN stock ON si = i_id WHERE sw + 0 = 1" in
    same_rows db ~probe ~foil;
    Alcotest.(check (list (list string)))
      "only the row whose match survives the WHERE conjunct"
      [ [ "2"; "80" ] ]
      (rows_of db probe))
;;

(* A LEFT JOIN whose driving row has a NULL join key still null-extends. *)
let left_join_null_key_null_extends () =
  with_db (fun db ->
    exec db "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
    exec
      db
      "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, si))";
    exec db "BEGIN";
    exec db "INSERT INTO line VALUES (1, 1, NULL)";
    exec db "INSERT INTO stock VALUES (1, 8, 80)";
    exec db "COMMIT";
    Alcotest.(check (list (list string)))
      "the NULL-keyed row survives, null-extended"
      [ [ "1"; "NULL" ] ]
      (rows_of db "SELECT o, qty FROM line LEFT JOIN stock ON si = i_id"))
;;

(* A constant that matches nothing must yield no rows, not the index's NULL
   entries or a wrongly-truncated prefix. *)
let constant_matching_nothing_yields_nothing () =
  with_db (fun db ->
    seed db;
    Alcotest.(check (list (list string)))
      "no warehouse 9"
      []
      (rows_of db "SELECT qty FROM line INNER JOIN stock ON si = i_id WHERE sw = 9"))
;;

(* ------------------------------------------------------------------ *)
(* Property                                                             *)
(* ------------------------------------------------------------------ *)

let prop_probe_matches_foil =
  QCheck.Test.make
    ~count:100
    ~name:"composite join probe agrees with unoptimizable foil"
    QCheck.(pair (int_range 0 3) (int_range 0 12))
    (fun (sw, i_hi) ->
       with_db (fun db ->
         exec
           db
           "CREATE TABLE line (w INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, o))";
         exec
           db
           "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER, PRIMARY KEY (sw, \
            si))";
         exec db "BEGIN";
         for o = 1 to 10 do
           exec db (Printf.sprintf "INSERT INTO line VALUES (1, %d, %d)" o (o mod 6))
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
         let q pred =
           rows_of
             db
             (Printf.sprintf
                "SELECT o, qty FROM line LEFT JOIN stock ON si = i_id WHERE %s"
                pred)
         in
         q (Printf.sprintf "sw = %d AND i_id < %d" sw i_hi)
         = q (Printf.sprintf "sw + 0 = %d AND i_id < %d" sw i_hi)))
;;

let () =
  Alcotest.run
    "nlj_composite_probe_516"
    [ ( "probe"
      , [ Alcotest.test_case
            "WHERE constant completes the probe key"
            `Quick
            where_constant_completes_the_probe_key
        ; Alcotest.test_case
            "single-column index still probes"
            `Quick
            single_column_index_still_probes
        ; Alcotest.test_case
            "WHERE constants alone do not form a probe"
            `Quick
            where_constants_alone_do_not_form_a_probe
        ; Alcotest.test_case
            "unpinned leading column falls back"
            `Quick
            unpinned_leading_column_falls_back
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
        ] )
    ; "property", List.map QCheck_alcotest.to_alcotest [ prop_probe_matches_foil ]
    ]
;;
