(** #689: an FTS [rank] projection used to pay one point [S.get] per match, plus
    an O(matches x postings) association-list probe per match inside the score
    fold itself.

    Both are in {!Granary_sql.Exec.fts_score_matches}, which runs over the FULL
    match set before the sort that LIMIT/OFFSET slices — so neither could be
    truncated to the returned window (#687 fixed the content fetch, which could
    be).  What changed:

    - the score fold's per-term posting list became a hashtable, so a term's
      per-document frequency is a lookup instead of a linear probe.  This was the
      dominant cost by a wide margin and is a pure algorithmic fix: measured on a
      4 000-document single-term rank query, 288.8 ms -> 10.4 ms in memory
      (27.8x) and 442.2 ms -> 17.1 ms on disk (25.9x), with the per-query
      allocation going from quadratic-shaped to exactly linear in the match count
      (641 825 / 1 283 014 / 2 555 495 minor words at 1 000 / 2 000 / 4 000
      matches on disk — 2.00x, 1.99x per doubling);
    - the per-match [S.get] became ONE cursor walk across the doc-length key
      region when the match set is dense enough for that to pay
      ({!Granary_sql.Exec.fts_doclen_scan_ratio}).  On disk that took the same
      4 000-match query from 4 370 993 to 2 555 495 minor words.  It is gated
      because an ungated walk is much WORSE when the match set is selective:
      3 matches among 4 000 documents cost 642 663 minor words walking against
      19 325 point-fetching, 33x.

    The gate means there are now two strategies producing the doc lengths that
    feed BM25, so the load-bearing property is that they are INTERCHANGEABLE.
    Every test here runs the same statement over the same data twice — once with
    [set_fts_doclen_scan_ratio 0] (never walk) and once with it set high enough to
    always walk — and requires byte-identical output: same rows, same order, same
    rank floats.  A wall-clock gate would prove less and flake on a loaded box;
    these assertions are load-insensitive by construction.

    The one measurement this file makes is printed, never asserted, unless
    [GRANARY_FTS_DOCLEN_MAX_WORDS_PER_MATCH] is set.  It is armed in no workflow:
    the numbers above come from one box and one backend, which is not enough to
    set a ceiling on every PR (the convention [test_scan_borrow_481] establishes).
*)

open Lwt.Syntax
module D = Granary.Db
module Exec = Granary_sql.Exec

let run f = Lwt_main.run (f ())

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "db error: %a" D.pp_error e
;;

let exec db sql =
  let* r = D.execute db sql in
  Lwt.return (unwrap r)
;;

let rows db sql =
  let* r = D.query db sql in
  Lwt_stream.to_list (unwrap r)
;;

(* Render a row so two runs can be compared exactly, floats included. *)
let show_row (row : Granary.Db.row) =
  String.concat
    "|"
    (List.map
       (fun (v : Granary_encoding.Row.value) ->
          match v with
          | Granary_encoding.Row.V_null -> "NULL"
          | Granary_encoding.Row.V_int i -> Int64.to_string i
          | Granary_encoding.Row.V_real f -> Printf.sprintf "%.17g" f
          | Granary_encoding.Row.V_text s -> "'" ^ s ^ "'"
          | Granary_encoding.Row.V_blob b -> "x'" ^ Bytes.to_string b ^ "'")
       (Array.to_list row))
;;

(* Documents of deliberately DIFFERENT lengths, so [doc_length] actually moves
   the BM25 score: if the two strategies disagreed about a length the rank floats
   would differ, not just the row order. *)
let seed db n =
  let* () = exec db "CREATE VIRTUAL TABLE doc USING FTS5(body)" in
  let rec go i =
    if i > n
    then Lwt.return_unit
    else (
      let pad =
        String.concat " " (List.init (i mod 7) (fun j -> Printf.sprintf "p%d" j))
      in
      let rare = if i mod 500 = 1 then " zebra" else "" in
      let sql =
        Printf.sprintf "INSERT INTO doc (body) VALUES ('alpha term%d %s%s')" i pad rare
      in
      let* () = exec db sql in
      go (i + 1))
  in
  go 1
;;

(* Run [sql] under each doc-length strategy and require identical output. *)
let both_agree db label sql =
  Exec.set_fts_doclen_scan_ratio 0;
  let* by_get = rows db sql in
  Exec.set_fts_doclen_scan_ratio 1_000_000;
  let* by_scan = rows db sql in
  Exec.set_fts_doclen_scan_ratio 5;
  let a = List.map show_row by_get in
  let b = List.map show_row by_scan in
  Alcotest.(check (list string)) (label ^ ": point-get vs cursor-walk") a b;
  Lwt.return a
;;

let strategies_agree_on_every_match_shape () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* () = seed db 600 in
    (* dense: every document matches *)
    let* dense =
      both_agree db "dense" "SELECT body, rank FROM doc WHERE doc MATCH 'alpha'"
    in
    Alcotest.(check int) "dense row count" 600 (List.length dense);
    (* dense + a window: the slice happens after the sort, so the doc lengths
       behind the ordering are still every match's *)
    let* win =
      both_agree
        db
        "dense+limit"
        "SELECT body, rank FROM doc WHERE doc MATCH 'alpha' LIMIT 3 OFFSET 2"
    in
    Alcotest.(check int) "window row count" 3 (List.length win);
    (* selective, and spread across the whole rowid range *)
    let* sparse =
      both_agree db "sparse" "SELECT body, rank FROM doc WHERE doc MATCH 'zebra'"
    in
    Alcotest.(check int) "sparse row count" 2 (List.length sparse);
    (* exactly one match, so lo = hi and the walk sees a single entry *)
    let* one =
      both_agree db "single" "SELECT body, rank FROM doc WHERE doc MATCH 'term42'"
    in
    Alcotest.(check int) "single row count" 1 (List.length one);
    (* no matches at all: neither strategy may be entered *)
    let* none =
      both_agree db "empty" "SELECT body, rank FROM doc WHERE doc MATCH 'nosuchterm'"
    in
    Alcotest.(check int) "empty row count" 0 (List.length none);
    (* multi-term, so more than one posting list feeds the score fold *)
    let* multi =
      both_agree db "multi-term" "SELECT body, rank FROM doc WHERE doc MATCH 'alpha p0'"
    in
    Alcotest.(check bool) "multi-term returned rows" true (List.length multi > 0);
    (* prefix query, which reaches the score fold through a different posting
       builder than an exact term *)
    let* pre =
      both_agree db "prefix" "SELECT body, rank FROM doc WHERE doc MATCH 'term1*'"
    in
    Alcotest.(check bool) "prefix returned rows" true (List.length pre > 0);
    D.close db)
;;

(* A deleted document's doc-length entry is removed, so the walk crosses a HOLE
   in the region while the point-fetch path simply never asks for it.  They must
   still agree — and the surviving rows must still score as before. *)
let holes_in_the_doclen_region_do_not_diverge () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* () = seed db 200 in
    let* before =
      both_agree db "pre-delete" "SELECT body, rank FROM doc WHERE doc MATCH 'alpha'"
    in
    Alcotest.(check int) "pre-delete count" 200 (List.length before);
    let* () = exec db "DELETE FROM doc WHERE body LIKE '%term1 %'" in
    let* () = exec db "DELETE FROM doc WHERE body LIKE '%term100 %'" in
    let* after =
      both_agree db "post-delete" "SELECT body, rank FROM doc WHERE doc MATCH 'alpha'"
    in
    Alcotest.(check bool) "some rows were deleted" true (List.length after < 200);
    D.close db)
;;

(* The gate is chosen on cost, never on correctness, so the DEFAULT ratio must
   land on the same answers as either forced strategy. *)
let default_ratio_agrees_with_both () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* () = seed db 300 in
    let sql = "SELECT body, rank FROM doc WHERE doc MATCH 'alpha' LIMIT 10" in
    Exec.set_fts_doclen_scan_ratio 5;
    let* def = rows db sql in
    let* forced = both_agree db "default" sql in
    Alcotest.(check (list string)) "default vs forced" forced (List.map show_row def);
    D.close db)
;;

(* Rank must still be a descending sort by BM25 score, and doc length must still
   be an input to it — a shorter document holding the same term scores higher.
   This is what makes the doc-length fetch worth doing correctly at all. *)
let rank_still_orders_by_score_and_uses_doc_length () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* () = exec db "CREATE VIRTUAL TABLE d USING FTS5(body)" in
    let* () = exec db "INSERT INTO d (body) VALUES ('needle')" in
    let* () =
      exec
        db
        "INSERT INTO d (body) VALUES ('needle a b c d e f g h i j k l m n o p q r s t')"
    in
    let check_one label =
      let* rs = rows db "SELECT body, rank FROM d WHERE d MATCH 'needle'" in
      Alcotest.(check int) (label ^ ": rows") 2 (List.length rs);
      match rs with
      | [ [| Granary_encoding.Row.V_text b0; Granary_encoding.Row.V_real s0 |]
        ; [| _; Granary_encoding.Row.V_real s1 |]
        ] ->
        Alcotest.(check bool) (label ^ ": descending") true (s0 >= s1);
        Alcotest.(check bool)
          (label ^ ": short doc first")
          true
          (String.equal b0 "needle");
        Alcotest.(check bool) (label ^ ": lengths differ the score") true (s0 > s1);
        Lwt.return_unit
      | _ -> Alcotest.failf "%s: unexpected row shape" label
    in
    Exec.set_fts_doclen_scan_ratio 0;
    let* () = check_one "point-get" in
    Exec.set_fts_doclen_scan_ratio 1_000_000;
    let* () = check_one "cursor-walk" in
    Exec.set_fts_doclen_scan_ratio 5;
    D.close db)
;;

(* Measurement, not a gate.  Prints the per-match allocation of a dense rank
   query under each strategy; asserts only when the knob is set, which no
   workflow does.  Run it with [--verbose] to see the numbers — alcotest
   captures stdout otherwise.

   It must be FILE-BACKED.  The [Mem] backend answers [S.get] from a
   [Bytes_map], so a point fetch there costs about what a cursor step does and
   the two strategies measure the same (324.9 vs 323.4 words/match, observed) —
   an in-memory version of this would report that the change does nothing.  The
   cost #689 is about is a B-tree root-to-leaf descent per match. *)
let measure_words_per_match () =
  run (fun () ->
    let path = Filename.temp_file "granary689" ".db" in
    Sys.remove path;
    let* db_r = Granary_unix.open_file ~path () in
    let db = unwrap db_r in
    let n = 800 in
    let* () = seed db n in
    let sql = "SELECT body, rank FROM doc WHERE doc MATCH 'alpha' LIMIT 3" in
    let one label =
      Gc.compact ();
      let w0 = Gc.minor_words () in
      let* rs = rows db sql in
      let w1 = Gc.minor_words () in
      Alcotest.(check int) (label ^ ": window") 3 (List.length rs);
      let per = (w1 -. w0) /. float_of_int n in
      Printf.printf "  #689 %-12s %8.1f minor words per match\n%!" label per;
      Lwt.return per
    in
    Exec.set_fts_doclen_scan_ratio 0;
    let* by_get = one "point-get" in
    Exec.set_fts_doclen_scan_ratio 1_000_000;
    let* by_scan = one "cursor-walk" in
    Exec.set_fts_doclen_scan_ratio 5;
    (match Sys.getenv_opt "GRANARY_FTS_DOCLEN_MAX_WORDS_PER_MATCH" with
     | None -> ()
     | Some v ->
       let ceiling = float_of_string v in
       Alcotest.(check bool)
         (Printf.sprintf
            "cursor-walk %.1f words/match under ceiling %.1f"
            by_scan
            ceiling)
         true
         (by_scan <= ceiling));
    ignore by_get;
    let* () = D.close db in
    (try Sys.remove path with
     | _ -> ());
    Lwt.return_unit)
;;

let () =
  Alcotest.run
    "fts_doclen_689"
    [ ( "strategy equivalence"
      , [ Alcotest.test_case
            "strategies_agree_on_every_match_shape"
            `Quick
            strategies_agree_on_every_match_shape
        ; Alcotest.test_case
            "holes_in_the_doclen_region_do_not_diverge"
            `Quick
            holes_in_the_doclen_region_do_not_diverge
        ; Alcotest.test_case
            "default_ratio_agrees_with_both"
            `Quick
            default_ratio_agrees_with_both
        ] )
    ; ( "ranking"
      , [ Alcotest.test_case
            "rank_still_orders_by_score_and_uses_doc_length"
            `Quick
            rank_still_orders_by_score_and_uses_doc_length
        ] )
    ; ( "measurement"
      , [ Alcotest.test_case "measure_words_per_match" `Quick measure_words_per_match ] )
    ]
;;
