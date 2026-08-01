(** #513: a join must not throw away the driving table's access path.

    Before this fix [plan_base] answered [make_scan] unconditionally whenever the
    query had any join, and the whole WHERE clause became a single post-join
    filter.  The effect was that the #508 seek — and the older rowid-alias seek
    with it — silently stopped existing the moment a second table appeared: a
    fully pinned primary key still scanned both tables end to end.  This was the
    dominant cost of TPC-C StockLevel (order_line JOIN stock), where a query that
    returns single-digit rows examined 400k.

    The fix pushes the base table's own equality conjuncts down into its access
    path.  It is safe for the same reason the #508 DML seek is: [chain_joins]
    still applies the entire WHERE to the joined row, so the pushed-down seek can
    only change how many base rows are read, never which joined rows survive.

    [rows_examined] is the load-bearing assertion — it counts rows pulled from
    base scans, so it sees the difference directly. *)

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

(* Rows are compared as rendered strings: what these tests care about is that
   two spellings of the same predicate agree, not the value representation. *)
let render = function
  | Db.V_int n -> Int64.to_string n
  | Db.V_text s -> s
  | Db.V_real f -> Printf.sprintf "%h" f
  | Db.V_blob b -> Bytes.to_string b
  | Db.V_null -> "NULL"
;;

let rows_of db sql =
  List.map
    (fun r -> Array.to_list (Array.map render r))
    (unwrap
       (run
          (let open Lwt.Syntax in
           let* r = Db.query db sql in
           match r with
           | Error e -> Lwt.return (Error e)
           | Ok stream ->
             let* rows = Lwt_stream.to_list stream in
             Lwt.return (Ok rows))))
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

let returned db sql =
  let rows, _ = stats_of db sql in
  List.length rows
;;

(* [rows_examined] counts every base-scan row the query pulls, on BOTH sides of
   a join.  What #513 is about is the driving side, and the joined side's cost
   is untouched by it — so the assertions below measure the difference between a
   query and its unoptimizable foil, which isolates the driving side exactly. *)
let base_rows_saved db ~seek ~foil = examined db foil - examined db seek

(* A miniature of the StockLevel shape: [line] is keyed by the composite
   PRIMARY KEY (w, d, o) and joins [item] on its item id.  [n_o] orders per
   district gives [n_w * n_d * n_o] line rows against [n_i] item rows. *)
let seed db ~n_w ~n_d ~n_o ~n_i =
  exec
    db
    "CREATE TABLE line (w INTEGER, d INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, \
     d, o))";
  exec db "CREATE TABLE item (it_id INTEGER PRIMARY KEY, qty INTEGER)";
  exec db "BEGIN";
  for i = 1 to n_i do
    exec db (Printf.sprintf "INSERT INTO item VALUES (%d, %d)" i (i mod 50))
  done;
  for w = 1 to n_w do
    for d = 1 to n_d do
      for o = 1 to n_o do
        exec
          db
          (Printf.sprintf
             "INSERT INTO line VALUES (%d, %d, %d, %d)"
             w
             d
             o
             ((o * 7 mod n_i) + 1))
      done
    done
  done;
  exec db "COMMIT"
;;

(* Sizes chosen so that a full scan of both tables is unmistakably larger than
   any pushed-down access path: 3*4*40 = 480 line rows + 60 item rows. *)
let n_w, n_d, n_o, n_i = 3, 4, 40, 60
let line_rows = n_w * n_d * n_o

(* ------------------------------------------------------------------ *)
(* Pushdown                                                             *)
(* ------------------------------------------------------------------ *)

(* The headline case. Every column of [line]'s composite PK is pinned, so the
   base side should reach exactly one row; before the fix the join forced a full
   scan of both tables. *)
let full_key_equality_pushes_down () =
  with_db (fun db ->
    seed db ~n_w ~n_d ~n_o ~n_i;
    let sql =
      "SELECT qty FROM line INNER JOIN item ON it_id = i_id WHERE w = 2 AND d = 3 AND o \
       = 7"
    in
    let foil =
      "SELECT qty FROM line INNER JOIN item ON it_id = i_id WHERE w + 0 = 2 AND d + 0 = \
       3 AND o + 0 = 7"
    in
    Alcotest.(check int) "one joined row" 1 (returned db sql);
    Alcotest.(check int) "same rows as the foil" (returned db foil) (returned db sql);
    (* The seek reaches exactly one [line] row where the scan read all of them. *)
    Alcotest.(check int)
      "driving side reduced to a single row"
      (line_rows - 1)
      (base_rows_saved db ~seek:sql ~foil))
;;

(* A leading prefix, not the whole key: the seek narrows to one district's
   [n_o] rows rather than all [line_rows]. *)
let leading_prefix_pushes_down () =
  with_db (fun db ->
    seed db ~n_w ~n_d ~n_o ~n_i;
    let sql =
      "SELECT qty FROM line INNER JOIN item ON it_id = i_id WHERE w = 2 AND d = 3"
    in
    let foil =
      "SELECT qty FROM line INNER JOIN item ON it_id = i_id WHERE w + 0 = 2 AND d + 0 = 3"
    in
    Alcotest.(check int)
      "one joined row per line row in the district"
      n_o
      (returned db sql);
    Alcotest.(check int) "same rows as the foil" (returned db foil) (returned db sql);
    (* The seek reaches one district; the scan read every district. *)
    Alcotest.(check int)
      "driving side reduced to one district"
      (line_rows - n_o)
      (base_rows_saved db ~seek:sql ~foil))
;;

(* The rowid-alias seek was lost under a join too, not just the #508 composite
   one — [item] is the driving table here and its INTEGER PRIMARY KEY is an
   alias, so this is the single-column path. *)
let rowid_alias_pushes_down () =
  with_db (fun db ->
    seed db ~n_w ~n_d ~n_o ~n_i;
    let sql = "SELECT qty FROM item INNER JOIN line ON i_id = it_id WHERE it_id = 5" in
    let foil =
      "SELECT qty FROM item INNER JOIN line ON i_id = it_id WHERE it_id + 0 = 5"
    in
    Alcotest.(check int) "same rows as the foil" (returned db foil) (returned db sql);
    Alcotest.(check int)
      "driving side reduced to a single row"
      (n_i - 1)
      (base_rows_saved db ~seek:sql ~foil))
;;

(* A conjunct that mentions the joined table cannot pin the base table's index,
   and must not be mistaken for one. *)
let joined_table_conjunct_does_not_seek_base () =
  with_db (fun db ->
    seed db ~n_w ~n_d ~n_o ~n_i;
    let sql = "SELECT w FROM line INNER JOIN item ON it_id = i_id WHERE qty = 3" in
    let foil = "SELECT w FROM line INNER JOIN item ON it_id = i_id WHERE qty + 0 = 3" in
    Alcotest.(check (list (list string)))
      "same rows as the unoptimizable foil"
      (List.sort compare (rows_of db foil))
      (List.sort compare (rows_of db sql)))
;;

(* ------------------------------------------------------------------ *)
(* Correctness the pushdown must not disturb                            *)
(* ------------------------------------------------------------------ *)

(* A LEFT JOIN keeps unmatched base rows.  Restricting the base input is only
   safe because the whole WHERE is still applied to the joined row; if the
   pushdown ever became a substitute for that filter this would change. *)
let left_join_keeps_unmatched_rows () =
  with_db (fun db ->
    exec
      db
      "CREATE TABLE line (w INTEGER, d INTEGER, o INTEGER, i_id INTEGER, PRIMARY KEY (w, \
       d, o))";
    exec db "CREATE TABLE item (it_id INTEGER PRIMARY KEY, qty INTEGER)";
    exec db "BEGIN";
    exec db "INSERT INTO item VALUES (1, 10)";
    exec db "INSERT INTO line VALUES (1, 1, 1, 1)";
    exec db "INSERT INTO line VALUES (1, 1, 2, 99)" (* no matching item *);
    exec db "INSERT INTO line VALUES (2, 1, 1, 1)" (* wrong warehouse *);
    exec db "COMMIT";
    Alcotest.(check int)
      "both rows of w = 1, including the unmatched one"
      2
      (returned db "SELECT o FROM line LEFT JOIN item ON it_id = i_id WHERE w = 1"))
;;

(* A pushed-down seek yields rows in index order, not table order.  With no
   ORDER BY the engine is free to pick either, but the row SET must not change —
   compare against the unoptimizable foil sorted. *)
let pushdown_preserves_the_row_set () =
  with_db (fun db ->
    seed db ~n_w ~n_d ~n_o ~n_i;
    let seek =
      rows_of
        db
        "SELECT o, qty FROM line INNER JOIN item ON it_id = i_id WHERE w = 2 AND d = 3"
    in
    let foil =
      rows_of
        db
        "SELECT o, qty FROM line INNER JOIN item ON it_id = i_id WHERE w + 0 = 2 AND d + \
         0 = 3"
    in
    Alcotest.(check (list (list string)))
      "seek and foil agree as sets"
      (List.sort compare foil)
      (List.sort compare seek))
;;

(* ------------------------------------------------------------------ *)
(* Property                                                             *)
(* ------------------------------------------------------------------ *)

let prop_pushdown_matches_foil =
  QCheck.Test.make
    ~count:100
    ~name:"join pushdown agrees with unoptimizable foil"
    QCheck.(pair (int_range 0 4) (int_range 0 5))
    (fun (w, d) ->
       with_db (fun db ->
         seed db ~n_w:3 ~n_d:4 ~n_o:6 ~n_i:20;
         let q pred =
           List.sort
             compare
             (rows_of
                db
                (Printf.sprintf
                   "SELECT o, qty FROM line INNER JOIN item ON it_id = i_id WHERE %s"
                   pred))
         in
         q (Printf.sprintf "w = %d AND d = %d" w d)
         = q (Printf.sprintf "w + 0 = %d AND d + 0 = %d" w d)))
;;

let () =
  Alcotest.run
    "join_pushdown_513"
    [ ( "pushdown"
      , [ Alcotest.test_case
            "full key equality pushes down"
            `Quick
            full_key_equality_pushes_down
        ; Alcotest.test_case
            "leading prefix pushes down"
            `Quick
            leading_prefix_pushes_down
        ; Alcotest.test_case "rowid alias pushes down" `Quick rowid_alias_pushes_down
        ; Alcotest.test_case
            "joined-table conjunct does not seek base"
            `Quick
            joined_table_conjunct_does_not_seek_base
        ] )
    ; ( "correctness"
      , [ Alcotest.test_case
            "LEFT JOIN keeps unmatched rows"
            `Quick
            left_join_keeps_unmatched_rows
        ; Alcotest.test_case
            "pushdown preserves the row set"
            `Quick
            pushdown_preserves_the_row_set
        ] )
    ; "property", List.map QCheck_alcotest.to_alcotest [ prop_pushdown_matches_foil ]
    ]
;;
