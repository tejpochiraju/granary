(** #600: [PRAGMA not_null_check] counts without retaining what it counts, and
    [PRAGMA not_null_repair]'s victim buffer is bounded to one table.

    The report emits [(table, column, count)] and nothing else, but PR #581
    built it on the scan the repair needs, which retains every violating row so
    it can delete it. The shape that hurts is exactly the one the feature
    exists for: a legacy file with a wholly-NULL declared-NOT NULL column, on
    which the report allocated O(table) resident memory. And this is the half
    #563 called the important one — the command an operator runs FIRST, on a
    database whose scope of damage is unknown. Being OOM-killed while surveying
    the damage is the worst possible moment for it.

    The columnstore arm already counted only; #600 is the row-store arm
    catching up, plus scanning and repairing one table at a time rather than
    scanning every table before deleting anything (peak was the SUM over the
    database instead of its largest table).

    {b What is NOT changed, deliberately.} The repair's per-table buffer stays
    collected-and-sorted before the rows are fetched. That is #541's finding:
    fetching in index-key order costs up to a page read per row once the table
    outgrows the pager cache. Streaming that half would trade a bounded buffer
    for unbounded I/O. The fix is the bound, not the streaming.

    The memory claim is measured rather than asserted — [peak_live_words] below
    samples the major heap through a [Gc] alarm while the pragma runs. Before
    the fix the row-store report's peak tracked the table size; after it, it
    does not. *)

open Lwt.Syntax
module Db = Granary.Db
module Cat = Granary_catalog.Catalog
module Row = Granary_encoding.Row

let run = Lwt_main.run

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let query db sql =
  run
    (let* r = Db.query db sql in
     match r with
     | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
     | Ok stream -> Lwt_stream.to_list stream)
;;

let show_value = function
  | Row.V_text s -> s
  | Row.V_null -> "<null>"
  | Row.V_int n -> Int64.to_string n
  | Row.V_real f -> string_of_float f
  | Row.V_blob _ -> "<blob>"
;;

let texts db sql =
  List.map
    (fun (r : Row.t) -> Array.to_list r |> List.map show_value |> String.concat "|")
    (query db sql)
;;

let open_db path =
  match run (Granary_unix.open_file ~path ()) with
  | Ok db -> db
  | Error e -> Alcotest.failf "open %s: %a" path Db.pp_error e
;;

let close_db db =
  try run (Db.close db) with
  | _ -> ()
;;

(* The #563/#548 fixture recipe: rows go in while the column is genuinely
   nullable, then an implicit-PK index is REGISTERED in the catalog, which is
   what [Catalog.open_] re-derives NOT NULL from on the next open (#533).  No
   index entries are written — this fixture is only ever read by the report,
   and populating a large index would swamp the measurement with unrelated
   allocation.  ([test_not_null_567.ml] covers the populated-index case, which
   is what the repair's delete path needs.) *)
let register_implicit_pk_index path ~table ~cols =
  run
    (let* store =
       let* r = Granary_unix.Store.open_file ~path () in
       match r with
       | Ok s -> Lwt.return s
       | Error _ -> Alcotest.failf "cannot reopen store %s" path
     in
     let* cat = Cat.open_ store in
     let* r =
       Cat.create_index
         cat
         ~name:(Printf.sprintf "__pk_%s_%s_0" table (String.concat "_" cols))
         ~table
         ~columns:cols
         ~unique:true
         ~expr_flags:(List.map (fun _ -> false) cols)
         ~where_sql:None
         ~origin:`Implicit_pk
     in
     match r with
     | Error m -> Alcotest.failf "create_index: %s" m
     | Ok _ -> Granary_store.Store.close store)
;;

let with_temp_path f =
  let path = Filename.temp_file "granary_600_" ".db" in
  Sys.remove path;
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () -> f path)
;;

(* [n] rows whose [v] is wholly NULL, with [v] declared NOT NULL on reopen. *)
let with_wholly_null_column_db ~n f =
  with_temp_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE big (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "BEGIN";
    for i = 1 to n do
      exec db (Printf.sprintf "INSERT INTO big VALUES (%d, NULL)" i)
    done;
    exec db "COMMIT";
    close_db db;
    register_implicit_pk_index path ~table:"big" ~cols:[ "v" ];
    let db = open_db path in
    Fun.protect ~finally:(fun () -> close_db db) (fun () -> f db))
;;

(* Peak major-heap occupancy while [f] runs, in words.  A [Gc] alarm fires at
   the end of every major cycle, and the scan allocates enough to force many of
   them, so a structure retained across the whole call is seen.  Reported as a
   DELTA over the settled baseline, so the pager cache and the catalog — live
   either way — do not enter the number. *)
let peak_live_words f =
  Gc.full_major ();
  let base = (Gc.quick_stat ()).live_words in
  let peak = ref base in
  let alarm =
    Gc.create_alarm (fun () ->
      let l = (Gc.quick_stat ()).live_words in
      if l > !peak then peak := l)
  in
  let r = Fun.protect ~finally:(fun () -> Gc.delete_alarm alarm) f in
  r, !peak - base
;;

(* ------------------------------------------------------------------ *)
(* The measurement                                                      *)
(* ------------------------------------------------------------------ *)

(* A SCALING gate, not an absolute ceiling.  An absolute one would have to be
   calibrated against whatever else is live during the scan — pager cache, Lwt
   promise chain — and would drift; what #600 is about is the SLOPE.  So the
   same report is measured over [n] and [2n] wholly-NULL rows and the marginal
   cost of the extra [n] rows is what is bounded.

   Retained, a violating row costs a [(rowid, row)] pair plus the decoded row
   and its boxed values, on top of the drained cursor's raw key/value bytes:
   measured at 23.0 words/row before the fix — peak +342508 words at 20 000
   rows, +803414 at 40 000, three runs agreeing to four significant figures —
   so the marginal there was the whole extra table.  Counting through a
   streaming seek retains none of it and measures 1.6-1.9 words/row (+76551
   and +108531 for the same two sizes), the residue being the pager's cached
   pages rather than anything this code holds.

   The gate is 4 words/row: a ~6x margin under the retaining slope and ~2x
   over the counting one, which is the right way round for a gate meant to
   catch a return to O(table) rather than to police allocator noise.

   It is ARMED everywhere, including on the CI jobs that disarm every
   `GRANARY_BENCH_*` timing gate, and that is deliberate: this measures
   allocation, not wall clock, so a loaded runner does not move it (±0.02%
   across runs).  What a loaded runner cannot do, an unfamiliar allocator or
   word size can, and arm64 in `cross-arch.yml` has never run it — hence the
   escape hatch [GRANARY_MEM_MAX_WORDS_PER_ROW], which raises the ceiling
   without disabling the correctness assertions inside [peak_for_check].  It is
   the last resort, not the first: a failure here means the report started
   retaining again until something proves otherwise.

   The counts are checked in the same call so the scan cannot pass by doing
   nothing. *)
let peak_for_check ~n =
  with_wholly_null_column_db ~n (fun db ->
    let rows, peak = peak_live_words (fun () -> texts db "PRAGMA not_null_check") in
    Alcotest.(check (list string))
      (Printf.sprintf "all %d rows reported as violations" n)
      [ Printf.sprintf "big|v|%d" n ]
      rows;
    peak)
;;

let max_words_per_row =
  match Sys.getenv_opt "GRANARY_MEM_MAX_WORDS_PER_ROW" with
  | Some s ->
    (try int_of_string s with
     | _ -> 4)
  | None -> 4
;;

let check_does_not_retain_the_rows_it_counts () =
  let n = 20_000 in
  let peak1 = peak_for_check ~n in
  let peak2 = peak_for_check ~n:(2 * n) in
  let marginal = peak2 - peak1 in
  Printf.printf
    "\n\
    \  [#600] not_null_check peak live heap: %d rows -> +%d words, %d rows -> +%d words\n\
    \         marginal %d words over %d extra rows = %.2f words/row\n\
     %!"
    n
    peak1
    (2 * n)
    peak2
    marginal
    n
    (float_of_int marginal /. float_of_int n);
  Alcotest.(check bool)
    (Printf.sprintf
       "doubling the table adds %d words (%.2f words/row, ceiling %d), not a copy of it"
       marginal
       (float_of_int marginal /. float_of_int n)
       max_words_per_row)
    true
    (marginal < max_words_per_row * n)
;;

(* The other half of the same claim: the count is not a function of how many
   tables the database has, and the report still reads its own transaction's
   writes.  Two tables, both wholly NULL — before #600 [not_null_repair]
   scanned every table into a list before deleting from any of them, so the
   peak was the sum. *)
let repair_is_bounded_to_one_table () =
  with_temp_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE a (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "CREATE TABLE b (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "BEGIN";
    for i = 1 to 2000 do
      exec db (Printf.sprintf "INSERT INTO a VALUES (%d, NULL)" i);
      exec db (Printf.sprintf "INSERT INTO b VALUES (%d, NULL)" i)
    done;
    exec db "COMMIT";
    close_db db;
    register_implicit_pk_index path ~table:"a" ~cols:[ "v" ];
    register_implicit_pk_index path ~table:"b" ~cols:[ "v" ];
    let db = open_db path in
    Fun.protect
      ~finally:(fun () -> close_db db)
      (fun () ->
         (* Sorted: the report follows [Cat.list_tables] order, which is not
            alphabetical and is not part of the contract. *)
         Alcotest.(check (list string))
           "both tables are reported"
           [ "a|v|2000"; "b|v|2000" ]
           (List.sort compare (texts db "PRAGMA not_null_check"));
         Alcotest.(check (list string))
           "and both are repaired in one statement"
           [ "a|v|2000"; "b|v|2000" ]
           (List.sort compare (texts db "PRAGMA not_null_repair"));
         Alcotest.(check (list string))
           "nothing is left behind"
           []
           (texts db "PRAGMA not_null_check");
         Alcotest.(check (list string)) "table a is empty" [] (texts db "SELECT * FROM a");
         Alcotest.(check (list string)) "table b is empty" [] (texts db "SELECT * FROM b");
         Alcotest.(check (list string))
           "and the file is still consistent"
           [ "ok" ]
           (texts db "PRAGMA integrity_check")))
;;

(* ------------------------------------------------------------------ *)
(* What must not change                                                 *)
(* ------------------------------------------------------------------ *)

(* The split introduced a SECOND scan implementation, so the two have to agree
   about which columns violate and by how much: partial violations, several
   NOT NULL columns on one table, rows violating two columns at once (counted
   under both), and clean tables staying silent. *)
let counts_match_what_the_repair_deletes () =
  with_temp_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, a INTEGER, b INTEGER)";
    exec db "INSERT INTO t VALUES (1, 1, 1)";
    exec db "INSERT INTO t VALUES (2, NULL, 2)";
    exec db "INSERT INTO t VALUES (3, NULL, NULL)";
    exec db "INSERT INTO t VALUES (4, 4, NULL)";
    exec db "CREATE TABLE clean (k INTEGER PRIMARY KEY, v INTEGER NOT NULL)";
    exec db "INSERT INTO clean VALUES (1, 1)";
    close_db db;
    register_implicit_pk_index path ~table:"t" ~cols:[ "a"; "b" ];
    let db = open_db path in
    Fun.protect
      ~finally:(fun () -> close_db db)
      (fun () ->
         Alcotest.(check (list string))
           "per-column counts, clean table silent"
           [ "t|a|2"; "t|b|2" ]
           (texts db "PRAGMA not_null_check");
         Alcotest.(check (list string))
           "repair reports the same counts"
           [ "t|a|2"; "t|b|2" ]
           (texts db "PRAGMA not_null_repair");
         (* Three distinct rows deleted, though the counts sum to four: row 3
            violates both columns and is deleted once. *)
         Alcotest.(check (list string))
           "only the conforming row survives"
           [ "1|1|1" ]
           (texts db "SELECT * FROM t");
         Alcotest.(check (list string))
           "report is silent afterwards"
           []
           (texts db "PRAGMA not_null_check")))
;;

(* The report's [mode] handling is what makes [BEGIN; repair; check] agree with
   a plain SELECT in the same transaction (read-your-own-writes, #262).  The
   count-only path is a new function and had to keep it. *)
let check_reads_its_own_transactions_writes () =
  with_temp_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, NULL)";
    exec db "INSERT INTO t VALUES (2, NULL)";
    close_db db;
    register_implicit_pk_index path ~table:"t" ~cols:[ "v" ];
    let db = open_db path in
    Fun.protect
      ~finally:(fun () -> close_db db)
      (fun () ->
         exec db "BEGIN";
         Alcotest.(check (list string))
           "the violation is visible before the repair"
           [ "t|v|2" ]
           (texts db "PRAGMA not_null_check");
         ignore (texts db "PRAGMA not_null_repair" : string list);
         Alcotest.(check (list string))
           "and gone after it, inside the same transaction"
           []
           (texts db "PRAGMA not_null_check");
         Alcotest.(check (list string))
           "agreeing with a plain SELECT"
           []
           (texts db "SELECT * FROM t");
         exec db "ROLLBACK";
         Alcotest.(check (list string))
           "the rollback puts the violation back"
           [ "t|v|2" ]
           (texts db "PRAGMA not_null_check")))
;;

(* A columnstore is scanned by the count-only path too — it was already the
   count-only arm, and the refactor moved it.  A repair still cannot delete its
   rows and still says so with a 0-count row. *)
let columnstore_still_counted_and_still_unrepairable () =
  with_temp_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE cs (k INTEGER, v INTEGER) USING COLUMNSTORE";
    exec db "INSERT INTO cs VALUES (1, 10)";
    exec db "INSERT INTO cs VALUES (2, NULL)";
    exec db "INSERT INTO cs VALUES (3, NULL)";
    close_db db;
    register_implicit_pk_index path ~table:"cs" ~cols:[ "v" ];
    let db = open_db path in
    Fun.protect
      ~finally:(fun () -> close_db db)
      (fun () ->
         Alcotest.(check (list string))
           "counted"
           [ "cs|v|2" ]
           (texts db "PRAGMA not_null_check");
         Alcotest.(check (list string))
           "repair reports 0 deleted"
           [ "cs|v|0" ]
           (texts db "PRAGMA not_null_repair");
         Alcotest.(check (list string))
           "rows untouched"
           [ "1|10"; "2|<null>"; "3|<null>" ]
           (texts db "SELECT * FROM cs")))
;;

let () =
  Alcotest.run
    "not_null_600"
    [ ( "memory"
      , [ Alcotest.test_case
            "not_null_check does not retain the rows it counts"
            `Slow
            check_does_not_retain_the_rows_it_counts
        ; Alcotest.test_case
            "repair is bounded to one table at a time"
            `Slow
            repair_is_bounded_to_one_table
        ] )
    ; ( "unchanged"
      , [ Alcotest.test_case
            "counts match what the repair deletes"
            `Quick
            counts_match_what_the_repair_deletes
        ; Alcotest.test_case
            "the report reads its own transaction's writes"
            `Quick
            check_reads_its_own_transactions_writes
        ; Alcotest.test_case
            "a columnstore is counted and reported unrepairable"
            `Quick
            columnstore_still_counted_and_still_unrepairable
        ] )
    ]
;;
