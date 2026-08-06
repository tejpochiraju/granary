(** #630: [PRAGMA not_null_repair]'s SCAN streams; its VICTIM BUFFER still
    retains.

    #600 gave the report path ([PRAGMA not_null_check]) a streaming
    [seek_ge]/[seek_next] scan and bounded the repair's victim buffer to one
    table at a time. It left the repair's own scan on [S.cursor_open], which
    drains the whole tree into a list before a single violation is examined —
    the #228/#229 defect. So a repair of a large but mostly CLEAN table paid
    O(table) resident memory to find a handful of violators, in the one command
    an operator runs on a database whose scope of damage is unknown. Being
    OOM-killed while REPAIRING is worse than while surveying, because the repair
    may be partially applied.

    {b Two different bounds, and only one of them changed.}

    - The SCAN is now O(1) in the table: one decoded row at a time. That is what
      this file's memory test measures, with the violation count held FIXED at
      five while the table doubles — so anything the peak tracks is the scan,
      not the victims.
    - The VICTIM BUFFER stays O(violations), collected and sorted before the
      rows are fetched. That is #541's finding: fetching in index-key order
      costs up to a page read per row once the table outgrows the pager cache
      (957 reads sorted versus 21 685 unsorted). Streaming that half would trade
      a bounded buffer for unbounded I/O. It is deliberately NOT changed here.

    {b Two fixtures, and the difference is load-bearing.} The memory
    measurement and the row-level assertions use the #548 recipe, which
    REGISTERS an implicit-PK index without writing its entries — a file that is
    inconsistent before anything here runs, which is exactly the "index entry
    absent" case the delete path has to survive. Anything asserting that the
    file is still CONSISTENT must instead pass [?entries] and start from a
    consistent one, or the assertion is unsatisfiable regardless of how the
    repair behaves. That is what [PRAGMA integrity_check] is comparing: index
    entries against rows.

    The gate and the knob are the ones [test_not_null_600.ml] established —
    marginal peak live major-heap words per added row, ceiling 4, escape hatch
    [GRANARY_MEM_MAX_WORDS_PER_ROW]. It measures allocation rather than wall
    clock, so it is armed everywhere; see CLAUDE.md. *)

open Lwt.Syntax
module Db = Granary.Db
module Cat = Granary_catalog.Catalog
module Row = Granary_encoding.Row
module Index_key = Granary_encoding.Index_key

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

(* The #563/#548/#600 fixture recipe: rows go in while the column is genuinely
   nullable, then an implicit-PK index is REGISTERED in the catalog, which is
   what [Catalog.open_] re-derives NOT NULL from on the next open (#533).

   [Cat.create_index] only REGISTERS the index — population lives in
   [Exec.execute_create_index], which is not exported — so without [?entries]
   the index TREE is left empty while the table holds rows.  That is a
   deliberately INCONSISTENT file: [PRAGMA integrity_check] compares entries
   against rows and reports the mismatch, before anything here touches the
   table.  It is fine for the memory measurement and for the pure row-level
   assertions, but a test that wants to say anything about the file staying
   consistent must pass [?entries] and start from a consistent one — see
   [test_not_null_567.ml], whose fixture writes them the same way. *)
let register_implicit_pk_index ?(entries = []) path ~table ~cols =
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
     | Ok (idx : Cat.index_info) ->
       let* () =
         if entries = []
         then Lwt.return_unit
         else
           let* tx = Granary_store.Store.rw_begin store in
           let* () =
             Lwt_list.iter_s
               (fun (rowid, key_vals) ->
                  Granary_store.Store.put
                    tx
                    idx.Cat.idx_tree_id
                    (Index_key.encode key_vals ~rowid)
                    Bytes.empty)
               entries
           in
           Granary_store.Store.commit tx
       in
       Granary_store.Store.close store)
;;

let with_temp_path f =
  let path = Filename.temp_file "granary_630_" ".db" in
  Sys.remove path;
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () -> f path)
;;

(* Peak major-heap occupancy while [f] runs, in words.  A [Gc] alarm fires at
   the end of every major cycle and the scan allocates enough to force many of
   them, so a structure retained across the whole call is seen.  Reported as a
   DELTA over the settled baseline, so the pager cache and the catalog — live
   either way — do not enter the number.  (Same instrument as
   [test_not_null_600.ml]; duplicated rather than shared because these two
   files are each other's independent check.) *)
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

(* The rowids whose [v] is NULL.  FIXED, and identical at both table sizes:
   the victim buffer is therefore the same size in both runs, so it cancels out
   of the marginal and what is left is the scan alone.  That is the whole point
   of this file — #600's test doubles the violations along with the table and
   so cannot separate the two. *)
let violator_rowids = [ 3; 7; 11; 13; 17 ]

(* [n] rows, of which exactly [violator_rowids] hold NULL in [v]; [v] is
   declared NOT NULL on reopen. *)
let with_mostly_clean_db ~n f =
  with_temp_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE big (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "BEGIN";
    for i = 1 to n do
      if List.mem i violator_rowids
      then exec db (Printf.sprintf "INSERT INTO big VALUES (%d, NULL)" i)
      else exec db (Printf.sprintf "INSERT INTO big VALUES (%d, %d)" i i)
    done;
    exec db "COMMIT";
    close_db db;
    register_implicit_pk_index path ~table:"big" ~cols:[ "v" ];
    let db = open_db path in
    Fun.protect ~finally:(fun () -> close_db db) (fun () -> f db))
;;

(* The counts are asserted in the same call, so the scan cannot pass the memory
   gate by doing nothing — and the surviving row count is asserted too, so it
   cannot pass by deleting the wrong rows either. *)
let peak_for_repair ~n =
  with_mostly_clean_db ~n (fun db ->
    let rows, peak = peak_live_words (fun () -> texts db "PRAGMA not_null_repair") in
    Alcotest.(check (list string))
      (Printf.sprintf
         "%d violators repaired out of %d rows"
         (List.length violator_rowids)
         n)
      [ Printf.sprintf "big|v|%d" (List.length violator_rowids) ]
      rows;
    Alcotest.(check (list string))
      "and the clean rows are all still there"
      [ string_of_int (n - List.length violator_rowids) ]
      (texts db "SELECT COUNT(*) FROM big");
    peak)
;;

let max_words_per_row =
  match Sys.getenv_opt "GRANARY_MEM_MAX_WORDS_PER_ROW" with
  | Some s ->
    (try int_of_string s with
     | _ -> 4)
  | None -> 4
;;

(* A SCALING gate, not an absolute ceiling — the same argument as #600's.  An
   absolute one would have to be calibrated against whatever else is live
   during the repair (pager cache, WAL frame cache, Lwt promise chain) and
   would drift; what #630 is about is the SLOPE.

   Before the fix, [S.cursor_open] materialised every [(key, value)] pair in
   the tree, so the marginal cost of doubling the table was a copy of the extra
   half regardless of how few rows violated anything.  After it, the scan holds
   one decoded row and the residue is the pager's own cached pages. *)
let repair_scan_does_not_materialise_the_table () =
  let n = 20_000 in
  let peak1 = peak_for_repair ~n in
  let peak2 = peak_for_repair ~n:(2 * n) in
  let marginal = peak2 - peak1 in
  Printf.printf
    "\n\
    \  [#630] not_null_repair peak live heap (%d violators throughout): %d rows -> +%d \
     words, %d rows -> +%d words\n\
    \         marginal %d words over %d extra rows = %.2f words/row\n\
     %!"
    (List.length violator_rowids)
    n
    peak1
    (2 * n)
    peak2
    marginal
    n
    (float_of_int marginal /. float_of_int n);
  Alcotest.(check bool)
    (Printf.sprintf
       "doubling a mostly-clean table adds %d words (%.2f words/row, ceiling %d), not a \
        copy of it"
       marginal
       (float_of_int marginal /. float_of_int n)
       max_words_per_row)
    true
    (marginal < max_words_per_row * n)
;;

(* ------------------------------------------------------------------ *)
(* What the repair must still do                                        *)
(* ------------------------------------------------------------------ *)

(* The scan was rewritten and its result type changed (per-column buckets of
   retained rows became counts + one deduplicated victim list), so every claim
   the old shape carried is re-pinned here rather than assumed. *)

(* Exactly the violators go, and every other row survives with its values
   unchanged — not merely its count.

   The index is POPULATED here ([?entries]), which is what lets the last two
   assertions mean anything: the file starts consistent, so a mismatch
   afterwards is the repair's doing.  With the unpopulated fixture the index
   tree holds 0 entries against 5 rows before the repair runs, and
   [integrity_check] rightly says so — an assertion that the file is still
   consistent is then unsatisfiable no matter how the repair behaves. *)
let repair_deletes_exactly_the_violators () =
  with_temp_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER, w TEXT)";
    exec db "INSERT INTO t VALUES (1, 10, 'a')";
    exec db "INSERT INTO t VALUES (2, NULL, 'b')";
    exec db "INSERT INTO t VALUES (3, 30, 'c')";
    exec db "INSERT INTO t VALUES (4, NULL, 'd')";
    exec db "INSERT INTO t VALUES (5, 50, 'e')";
    close_db db;
    register_implicit_pk_index
      path
      ~table:"t"
      ~cols:[ "v" ]
      ~entries:
        [ 1L, [ Index_key.IK_int 10L ]
        ; 2L, [ Index_key.IK_null ]
        ; 3L, [ Index_key.IK_int 30L ]
        ; 4L, [ Index_key.IK_null ]
        ; 5L, [ Index_key.IK_int 50L ]
        ];
    let db = open_db path in
    Fun.protect
      ~finally:(fun () -> close_db db)
      (fun () ->
         Alcotest.(check (list string))
           "the fixture starts consistent — one index entry per row"
           [ "ok" ]
           (texts db "PRAGMA integrity_check");
         Alcotest.(check (list string))
           "the report sees both"
           [ "t|v|2" ]
           (texts db "PRAGMA not_null_check");
         (* #600's count-only path is strictly read-only: it must not have
            disturbed the entries it just walked past. *)
         Alcotest.(check (list string))
           "and reporting changed nothing"
           [ "ok" ]
           (texts db "PRAGMA integrity_check");
         Alcotest.(check (list string))
           "and the repair deletes both"
           [ "t|v|2" ]
           (texts db "PRAGMA not_null_repair");
         Alcotest.(check (list string))
           "survivors keep their rowids and every column value"
           [ "1|10|a"; "3|30|c"; "5|50|e" ]
           (texts db "SELECT * FROM t ORDER BY k");
         Alcotest.(check (list string))
           "nothing is left to report"
           []
           (texts db "PRAGMA not_null_check");
         Alcotest.(check (list string))
           "and the file is still consistent"
           [ "ok" ]
           (texts db "PRAGMA integrity_check");
         (* Entries-versus-rows is a count; that the SURVIVORS' entries are the
            ones left is not.  A seek through the key index proves it. *)
         Alcotest.(check (list string))
           "and a surviving key still seeks to its row"
           [ "3|30|c" ]
           (texts db "SELECT * FROM t WHERE v = 30")))
;;

(* A SECONDARY index — an ordinary UNIQUE one the engine itself built and
   populated — must lose exactly the deleted rows' entries too.  The victim
   list is deduplicated since #630, so a row is handed to [apply_delete_row]
   once; index maintenance is per (row, index), not per (row, violated
   column), and this pins that the two are independent. *)
let secondary_unique_index_entries_go_with_the_rows () =
  with_temp_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER, w TEXT)";
    exec db "INSERT INTO t VALUES (1, 10, 'a')";
    exec db "INSERT INTO t VALUES (2, NULL, 'b')";
    exec db "INSERT INTO t VALUES (3, 30, 'c')";
    exec db "INSERT INTO t VALUES (4, NULL, 'd')";
    exec db "INSERT INTO t VALUES (5, 50, 'e')";
    (* Built by the engine while the rows are already there, so its entries
       are real ones rather than hand-written. *)
    exec db "CREATE UNIQUE INDEX ix_t_w ON t (w)";
    close_db db;
    register_implicit_pk_index
      path
      ~table:"t"
      ~cols:[ "v" ]
      ~entries:
        [ 1L, [ Index_key.IK_int 10L ]
        ; 2L, [ Index_key.IK_null ]
        ; 3L, [ Index_key.IK_int 30L ]
        ; 4L, [ Index_key.IK_null ]
        ; 5L, [ Index_key.IK_int 50L ]
        ];
    let db = open_db path in
    Fun.protect
      ~finally:(fun () -> close_db db)
      (fun () ->
         Alcotest.(check (list string))
           "both indexes start consistent"
           [ "ok" ]
           (texts db "PRAGMA integrity_check");
         Alcotest.(check (list string))
           "the repair deletes both violators"
           [ "t|v|2" ]
           (texts db "PRAGMA not_null_repair");
         Alcotest.(check (list string))
           "and both indexes are still consistent afterwards"
           [ "ok" ]
           (texts db "PRAGMA integrity_check");
         Alcotest.(check (list string))
           "the deleted row's secondary key no longer resolves"
           []
           (texts db "SELECT * FROM t WHERE w = 'b'");
         Alcotest.(check (list string))
           "a survivor's does"
           [ "3|30|c" ]
           (texts db "SELECT * FROM t WHERE w = 'c'");
         (* And the freed key is genuinely free — a stale UNIQUE entry would
            reject this insert. *)
         exec db "INSERT INTO t VALUES (6, 60, 'b')";
         Alcotest.(check (list string))
           "and the freed unique key can be reused"
           [ "6|60|b" ]
           (texts db "SELECT * FROM t WHERE w = 'b'");
         Alcotest.(check (list string))
           "with the file still consistent"
           [ "ok" ]
           (texts db "PRAGMA integrity_check")))
;;

(* The victim list is now built once per ROW rather than once per violated
   COLUMN, so the "counted twice, deleted once" property is a property of the
   scan itself and no longer of a downstream [sort_uniq].  Both must hold. *)
let a_row_violating_two_columns_is_counted_twice_and_deleted_once () =
  with_temp_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, a INTEGER, b INTEGER)";
    exec db "INSERT INTO t VALUES (1, 1, 1)";
    exec db "INSERT INTO t VALUES (2, NULL, 2)";
    exec db "INSERT INTO t VALUES (3, NULL, NULL)";
    exec db "INSERT INTO t VALUES (4, 4, NULL)";
    close_db db;
    register_implicit_pk_index path ~table:"t" ~cols:[ "a"; "b" ];
    let db = open_db path in
    Fun.protect
      ~finally:(fun () -> close_db db)
      (fun () ->
         Alcotest.(check (list string))
           "row 3 is counted under both columns"
           [ "t|a|2"; "t|b|2" ]
           (texts db "PRAGMA not_null_check");
         Alcotest.(check (list string))
           "the repair reports the same per-column counts"
           [ "t|a|2"; "t|b|2" ]
           (texts db "PRAGMA not_null_repair");
         (* Counts sum to four; three rows were deleted. *)
         Alcotest.(check (list string))
           "and only the conforming row survives"
           [ "1|1|1" ]
           (texts db "SELECT * FROM t ORDER BY k")))
;;

(* A [seek_ge] from the empty key must position before the FIRST entry however
   sparse the keyspace is — a gap at the start of the tree is where an
   off-by-one in the new positioning would show up, and a violator in the first
   or last slot is where a dropped boundary entry would. *)
let sparse_and_boundary_rowids_are_all_seen () =
  with_temp_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER)";
    List.iter
      (fun (k, v) ->
         match v with
         | None -> exec db (Printf.sprintf "INSERT INTO t VALUES (%d, NULL)" k)
         | Some v -> exec db (Printf.sprintf "INSERT INTO t VALUES (%d, %d)" k v))
      [ 100, None; 200, Some 2; 5000, None; 5001, Some 3; 999999, None ];
    close_db db;
    register_implicit_pk_index path ~table:"t" ~cols:[ "v" ];
    let db = open_db path in
    Fun.protect
      ~finally:(fun () -> close_db db)
      (fun () ->
         Alcotest.(check (list string))
           "first, middle and last rowids all counted"
           [ "t|v|3" ]
           (texts db "PRAGMA not_null_check");
         Alcotest.(check (list string))
           "and all three deleted"
           [ "t|v|3" ]
           (texts db "PRAGMA not_null_repair");
         Alcotest.(check (list string))
           "leaving exactly the two conforming rows"
           [ "200|2"; "5001|3" ]
           (texts db "SELECT * FROM t ORDER BY k")))
;;

(* A clean table produces NO report row at all, which is what makes a 0-count
   row an unambiguous "cannot fix" signal (see [repair_not_null_table]'s
   comment).  With the scan streaming, a clean table also retains nothing. *)
let clean_table_is_silent_and_untouched () =
  with_temp_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER NOT NULL)";
    exec db "INSERT INTO t VALUES (1, 10)";
    exec db "INSERT INTO t VALUES (2, 20)";
    Fun.protect
      ~finally:(fun () -> close_db db)
      (fun () ->
         Alcotest.(check (list string))
           "nothing to report"
           []
           (texts db "PRAGMA not_null_check");
         Alcotest.(check (list string))
           "and nothing to repair — no row at all, not a 0-count row"
           []
           (texts db "PRAGMA not_null_repair");
         Alcotest.(check (list string))
           "rows untouched"
           [ "1|10"; "2|20" ]
           (texts db "SELECT * FROM t ORDER BY k")))
;;

(* The columnstore arm returns its counts with an EMPTY victim list under the
   new shape; it must still report 0 deleted rather than raising, and must
   still leave its rows in place. *)
let columnstore_still_unrepairable () =
  with_temp_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE cs (k INTEGER, v INTEGER) USING COLUMNSTORE";
    exec db "INSERT INTO cs VALUES (1, 10)";
    exec db "INSERT INTO cs VALUES (2, NULL)";
    close_db db;
    register_implicit_pk_index path ~table:"cs" ~cols:[ "v" ];
    let db = open_db path in
    Fun.protect
      ~finally:(fun () -> close_db db)
      (fun () ->
         Alcotest.(check (list string))
           "counted"
           [ "cs|v|1" ]
           (texts db "PRAGMA not_null_check");
         Alcotest.(check (list string))
           "repair reports 0 deleted"
           [ "cs|v|0" ]
           (texts db "PRAGMA not_null_repair");
         Alcotest.(check (list string))
           "rows untouched"
           [ "1|10"; "2|<null>" ]
           (texts db "SELECT * FROM cs")))
;;

(* The repair runs inside the caller's explicit transaction when there is one,
   and a ROLLBACK must put every deleted row back — the streaming scan reads
   through the same write transaction it deletes through (#262). *)
let repair_rolls_back_with_its_transaction () =
  with_temp_path (fun path ->
    let db = open_db path in
    exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER)";
    exec db "INSERT INTO t VALUES (1, 10)";
    exec db "INSERT INTO t VALUES (2, NULL)";
    exec db "INSERT INTO t VALUES (3, 30)";
    close_db db;
    register_implicit_pk_index path ~table:"t" ~cols:[ "v" ];
    let db = open_db path in
    Fun.protect
      ~finally:(fun () -> close_db db)
      (fun () ->
         exec db "BEGIN";
         Alcotest.(check (list string))
           "repaired inside the transaction"
           [ "t|v|1" ]
           (texts db "PRAGMA not_null_repair");
         Alcotest.(check (list string))
           "and the deletion is visible to the same transaction"
           [ "1|10"; "3|30" ]
           (texts db "SELECT * FROM t ORDER BY k");
         exec db "ROLLBACK";
         Alcotest.(check (list string))
           "the rollback puts the violating row back"
           [ "1|10"; "2|<null>"; "3|30" ]
           (texts db "SELECT * FROM t ORDER BY k");
         Alcotest.(check (list string))
           "so the report sees it again"
           [ "t|v|1" ]
           (texts db "PRAGMA not_null_check")))
;;

let () =
  Alcotest.run
    "not_null_repair_630"
    [ ( "memory"
      , [ Alcotest.test_case
            "the repair's scan does not materialise the table"
            `Slow
            repair_scan_does_not_materialise_the_table
        ] )
    ; ( "correctness"
      , [ Alcotest.test_case
            "exactly the violating rows are deleted"
            `Quick
            repair_deletes_exactly_the_violators
        ; Alcotest.test_case
            "a secondary unique index loses exactly the deleted rows' entries"
            `Quick
            secondary_unique_index_entries_go_with_the_rows
        ; Alcotest.test_case
            "a row violating two columns is counted twice, deleted once"
            `Quick
            a_row_violating_two_columns_is_counted_twice_and_deleted_once
        ; Alcotest.test_case
            "sparse and boundary rowids are all seen"
            `Quick
            sparse_and_boundary_rowids_are_all_seen
        ; Alcotest.test_case
            "a clean table is silent and untouched"
            `Quick
            clean_table_is_silent_and_untouched
        ; Alcotest.test_case
            "a columnstore is still reported unrepairable"
            `Quick
            columnstore_still_unrepairable
        ; Alcotest.test_case
            "the repair rolls back with its transaction"
            `Quick
            repair_rolls_back_with_its_transaction
        ] )
    ]
;;
