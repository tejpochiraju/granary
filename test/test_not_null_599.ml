(** #599: a conflict-resolution modifier means the same thing for NOT NULL as
    it does for UNIQUE.

    PR #581 (#567) added the runtime NOT NULL check but wired it in below the
    conflict resolution, so [enforce_not_null] never saw [on_conflict]. The
    result was measurable and inconsistent: [INSERT OR IGNORE] skipped a UNIQUE
    conflict and hard-errored on a NOT NULL one, in the same statement form. A
    caller batching inserts with [OR IGNORE] asked for a skip and got an
    exception.

    The decision (issue #599, option 1): [OR IGNORE] skips the row for NOT NULL
    too. That is what the modifier means, it is what the UNIQUE path already
    did, and it is what SQLite does — though "match SQLite" is not the reason
    here (granary is inspired by SQLite, not a port; #530 chose against it
    deliberately). The fix lives at the INSERT call sites, NOT inside
    [enforce_not_null]: [write_row_rekeyed] serves UPDATE, UPSERT DO UPDATE and
    ON UPDATE CASCADE, none of which have an [OR IGNORE] form to consult, so
    softening the shared function would have relaxed all four sites at once.

    Two enforcement levels have to cooperate for that, and the first revision
    of this fix changed only one of them. [Sema.bind_insert_row] rejects a
    LITERAL NULL in a VALUES list before the runtime check ever runs, and does
    so for the whole STATEMENT — so `INSERT OR IGNORE INTO t VALUES
    (1,10),(2,NULL),(3,30)` lost all three rows while the parameter spelling of
    the same statement skipped one. The binder now suspends that check under
    [CA_ignore] and lets the row through to be skipped at write time, so
    exactly ONE place decides what [OR IGNORE] means. Under every other
    resolution the static error stands, earlier and better located.

    Note what the boundary really was before that: not "a literal NULL is a
    bind error" but "a literal NULL *in a VALUES list*" — an artefact of where
    [Sema] happens to look, since `INSERT OR IGNORE ... SELECT k, NULL FROM s`
    always skipped silently. The spellings are pinned together below so the two
    levels cannot drift apart again.

    The other resolutions are pinned here too, because "whichever is chosen,
    the two constraint kinds should agree" cuts both ways — a later change that
    makes one of them skip must fail these tests. [OR REPLACE] is the
    deliberate divergence, recorded in CLAUDE.md's Testing standards: SQLite
    substitutes the column DEFAULT for the NULL and aborts only when there is
    none; granary raises. *)

open Lwt.Syntax
module Db = Granary.Db
module Row = Granary_encoding.Row

let run = Lwt_main.run

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

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let prepare db sql =
  run
    (let* r = Db.prepare db sql in
     match r with
     | Ok st -> Lwt.return st
     | Error e -> Alcotest.failf "prepare %S: %a" sql Db.pp_error e)
;;

(* Most cases below use a bound parameter, which reaches the runtime check
   without [Sema] having an opinion — the narrower thing to test.  The literal
   spelling exercises both levels at once and is covered separately, by
   [or_ignore_skips_every_spelling_of_null] and
   [literal_null_is_still_a_bind_error_without_or_ignore]. *)
let run_stmt db sql params =
  let st = prepare db sql in
  run (Db.run st ~params)
;;

let expect_skipped db sql params =
  match run_stmt db sql params with
  | Ok n ->
    Alcotest.(check int) (Printf.sprintf "%S reports 0 rows written" sql) 0 n;
    ()
  | Error e -> Alcotest.failf "%S was expected to skip, got: %a" sql Db.pp_error e
;;

let expect_not_null_error db ~table ~col sql params =
  let want = Printf.sprintf "NOT NULL constraint failed: %s.%s" table col in
  match run_stmt db sql params with
  | Ok n -> Alcotest.failf "%S was expected to fail with %S, wrote %d rows" sql want n
  | Error e ->
    let msg = Format.asprintf "%a" Db.pp_error e in
    Alcotest.(check bool)
      (Printf.sprintf "%S rejected as a NOT NULL violation (got %S)" sql msg)
      true
      (contains ~needle:want msg)
;;

let seed db =
  exec db "CREATE TABLE u (k INTEGER PRIMARY KEY, v INTEGER NOT NULL)";
  exec db "INSERT INTO u VALUES (1, 5)"
;;

(* ------------------------------------------------------------------ *)
(* The asymmetry the issue measured                                     *)
(* ------------------------------------------------------------------ *)

(* Both halves in one test, because the defect was not "NOT NULL errors" — it
   was the two constraints disagreeing inside a single modifier. *)
let or_ignore_skips_both_constraint_kinds () =
  with_db (fun db ->
    seed db;
    (* UNIQUE: was already a skip, and must stay one. *)
    exec db "INSERT OR IGNORE INTO u VALUES (1, 6)";
    (* NOT NULL: used to raise here. *)
    expect_skipped db "INSERT OR IGNORE INTO u VALUES (2, ?)" [ Db.V_null ];
    Alcotest.(check (list string))
      "neither row landed and the original is untouched"
      [ "1|5" ]
      (texts db "SELECT * FROM u"))
;;

(* A skip is a skip, not a statement abort: the rows around the offender in a
   multi-row VALUES list still land.  This is the batching case the issue names
   as the practical cost of the old behaviour. *)
let or_ignore_skips_only_the_offending_row () =
  with_db (fun db ->
    seed db;
    let st = prepare db "INSERT OR IGNORE INTO u VALUES (2, ?), (3, ?), (4, ?)" in
    (match run (Db.run st ~params:[ Db.V_int 20L; Db.V_null; Db.V_int 40L ]) with
     | Ok n -> Alcotest.(check int) "two of three rows written" 2 n
     | Error e -> Alcotest.failf "batched OR IGNORE raised: %a" Db.pp_error e);
    Alcotest.(check (list string))
      "the good rows landed, the NULL one did not"
      [ "1|5"; "2|20"; "4|40" ]
      (texts db "SELECT * FROM u"))
;;

(* INSERT ... SELECT is the other row-store entry point and shares
   [execute_insert_write], so it gets the same treatment. *)
let or_ignore_insert_select_skips () =
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO src VALUES (7, NULL)";
    exec db "INSERT INTO src VALUES (8, 80)";
    (* The reported count must fall with the skip, not report the source
       cardinality — a skip that still claims the row is the #567 defect
       wearing a new name at the API boundary. *)
    (match run_stmt db "INSERT OR IGNORE INTO u SELECT k, v FROM src" [] with
     | Ok n -> Alcotest.(check int) "one of two source rows written" 1 n
     | Error e -> Alcotest.failf "INSERT ... SELECT OR IGNORE raised: %a" Db.pp_error e);
    Alcotest.(check (list string))
      "only the non-violating source row was copied"
      [ "1|5"; "8|80" ]
      (texts db "SELECT * FROM u"))
;;

(* ------------------------------------------------------------------ *)
(* The other conflict resolutions                                       *)
(* ------------------------------------------------------------------ *)

(* Every non-IGNORE resolution raises, and so does the bare INSERT.  That is
   the same answer [check_insert_unique] gives them for a UNIQUE violation
   (only CA_ignore and CA_replace have a branch there; everything else falls
   through to [unique_constraint_failed_msg]), so the two constraint kinds
   agree across the whole modifier set rather than only at OR IGNORE. *)
let other_resolutions_still_raise () =
  List.iter
    (fun modifier ->
       with_db (fun db ->
         seed db;
         expect_not_null_error
           db
           ~table:"u"
           ~col:"v"
           (Printf.sprintf "INSERT %sINTO u VALUES (2, ?)" modifier)
           [ Db.V_null ];
         Alcotest.(check (list string))
           (Printf.sprintf "%S wrote nothing" modifier)
           [ "1|5" ]
           (texts db "SELECT * FROM u")))
    [ ""; "OR ABORT "; "OR FAIL "; "OR ROLLBACK "; "OR REPLACE " ]
;;

(* [OR REPLACE] singled out: this is a deliberate divergence from SQLite, which
   substitutes the column's DEFAULT for the NULL and only aborts when the
   column has none.  Granary raises even when a DEFAULT exists — REPLACE here
   means "delete the row this one conflicts with", and a NULL conflicts with
   nothing; quietly storing a value the caller did not supply is a larger
   surprise than the error.  Pinned so the divergence is a decision on the
   record rather than an omission. *)
let or_replace_does_not_substitute_the_default () =
  with_db (fun db ->
    exec db "CREATE TABLE d (k INTEGER PRIMARY KEY, v INTEGER NOT NULL DEFAULT 42)";
    exec db "INSERT INTO d VALUES (1, 5)";
    expect_not_null_error
      db
      ~table:"d"
      ~col:"v"
      "INSERT OR REPLACE INTO d VALUES (2, ?)"
      [ Db.V_null ];
    Alcotest.(check (list string))
      "no DEFAULT was substituted"
      [ "1|5" ]
      (texts db "SELECT * FROM d"))
;;

(* UPDATE has no [OR IGNORE] form to consult, so the shared
   [write_row_rekeyed] site keeps hard-erroring — the reason the fix is at the
   INSERT call sites and not inside [enforce_not_null].  UPSERT DO UPDATE
   funnels through the same site, and an [INSERT OR IGNORE ... ON CONFLICT DO
   UPDATE] is therefore NOT softened: the modifier governs the insert half. *)
let update_and_upsert_do_update_still_raise () =
  with_db (fun db ->
    seed db;
    expect_not_null_error db ~table:"u" ~col:"v" "UPDATE u SET v = ?" [ Db.V_null ];
    expect_not_null_error
      db
      ~table:"u"
      ~col:"v"
      "INSERT INTO u VALUES (1, 9) ON CONFLICT(k) DO UPDATE SET v = ?"
      [ Db.V_null ];
    Alcotest.(check (list string)) "row untouched" [ "1|5" ] (texts db "SELECT * FROM u"))
;;

(* Every spelling of the NULL skips, not just the bound parameter.  The first
   revision of this fix touched the runtime check only, and [Sema] (at
   [bind_insert_row]) still rejected a LITERAL NULL in a VALUES list before the
   runtime check could skip it — and rejected it for the whole STATEMENT, so a
   three-row batch with one offender lost all three rows.  That is worse than
   the defect #599 opens with, and it is the most obvious spelling.

   The list below is not decoration: the three spellings reach the NULL by
   three different routes ([Sema]'s literal check, the runtime check via a
   parameter, and the runtime check via a projection that [Sema] never
   inspects), and it was precisely the gap between the first and the other two
   that shipped broken.  A rule stated as "a literal NULL is a bind error" was
   in fact "a literal NULL in a VALUES list", which is where [Sema] happens to
   look — an artefact, and it must not be re-pinned as a contract. *)
let or_ignore_skips_every_spelling_of_null () =
  let stored db = texts db "SELECT * FROM u" in
  (* literal, in a multi-row VALUES list: one row skipped, not three lost *)
  with_db (fun db ->
    seed db;
    exec db "INSERT OR IGNORE INTO u VALUES (2, 20), (3, NULL), (4, 40)";
    Alcotest.(check (list string))
      "literal NULL skips its own row and keeps the batch"
      [ "1|5"; "2|20"; "4|40" ]
      (stored db));
  (* literal, alone *)
  with_db (fun db ->
    seed db;
    exec db "INSERT OR IGNORE INTO u VALUES (2, NULL)";
    Alcotest.(check (list string)) "single literal NULL skips" [ "1|5" ] (stored db));
  (* A NULL-valued EXPRESSION has no spelling here to test: an INSERT VALUES
     list takes literals and parameters only ("unsupported: complex expression
     in INSERT VALUES"), which is pre-existing and unrelated to #599.  The
     expression route into a NOT NULL column is UPDATE's, and that one still
     raises — see [update_binders_unchanged]. *)
  (* a projection [Sema] never inspects *)
  with_db (fun db ->
    seed db;
    exec db "CREATE TABLE src (k INTEGER)";
    exec db "INSERT INTO src VALUES (2)";
    exec db "INSERT INTO src VALUES (3)";
    exec db "INSERT OR IGNORE INTO u SELECT k, NULL FROM src";
    Alcotest.(check (list string)) "SELECT-projected NULL skips" [ "1|5" ] (stored db));
  (* the bound parameter the issue measured *)
  with_db (fun db ->
    seed db;
    expect_skipped db "INSERT OR IGNORE INTO u VALUES (2, ?)" [ Db.V_null ];
    Alcotest.(check (list string)) "parameter NULL skips" [ "1|5" ] (stored db))
;;

(* The static check is only SUSPENDED by [OR IGNORE], not removed.  Under every
   other resolution the literal is still refused by [Sema] — earlier, and with
   the better-located message — rather than being demoted to the runtime check.
   Pinned separately from [other_resolutions_still_raise], which uses the
   parameter spelling and therefore exercises the runtime check. *)
let literal_null_is_still_a_bind_error_without_or_ignore () =
  List.iter
    (fun modifier ->
       with_db (fun db ->
         seed db;
         let sql = Printf.sprintf "INSERT %sINTO u VALUES (2, NULL)" modifier in
         match run (Db.execute db sql) with
         | Ok () -> Alcotest.failf "%S accepted a literal NULL" sql
         | Error e ->
           let msg = Format.asprintf "%a" Db.pp_error e in
           Alcotest.(check bool)
             (Printf.sprintf
                "%S gives the binder message, not the runtime one (%S)"
                sql
                msg)
             true
             (contains ~needle:"NOT NULL violation: v" msg)))
    [ ""; "OR ABORT "; "OR FAIL "; "OR ROLLBACK "; "OR REPLACE " ]
;;

(* UPDATE has no [OR IGNORE] form, so its binder is untouched by all of this.
   Pinned because the change above is one `on_conflict` check away from the
   UPDATE / UPSERT DO UPDATE binders, which must keep rejecting the literal. *)
let update_binders_unchanged () =
  with_db (fun db ->
    seed db;
    List.iter
      (fun sql ->
         match run (Db.execute db sql) with
         | Ok () -> Alcotest.failf "%S accepted a literal NULL" sql
         | Error e ->
           let msg = Format.asprintf "%a" Db.pp_error e in
           Alcotest.(check bool)
             (Printf.sprintf "%S refused by the binder (%S)" sql msg)
             true
             (contains ~needle:"NOT NULL violation: v" msg))
      [ "UPDATE u SET v = NULL"
      ; "INSERT OR IGNORE INTO u VALUES (1, 9) ON CONFLICT(k) DO UPDATE SET v = NULL"
      ];
    Alcotest.(check (list string)) "row untouched" [ "1|5" ] (texts db "SELECT * FROM u"))
;;

(* ------------------------------------------------------------------ *)
(* Columnstore                                                          *)
(* ------------------------------------------------------------------ *)

(* The columnstore arms hand their row array straight to
   [Col_store.insert_rows] and never encode, which is how the first revision of
   #567 sailed past them.  The same trap applies here, so both arms are pinned:
   VALUES and INSERT ... SELECT.  Note the row count has to fall to match — the
   VALUES arm used to report [List.length values] unconditionally, which would
   have claimed a skipped row as written. *)
let columnstore_or_ignore_skips () =
  with_db (fun db ->
    exec db "CREATE TABLE cs (k INTEGER, v INTEGER NOT NULL) USING COLUMNSTORE";
    let st = prepare db "INSERT OR IGNORE INTO cs VALUES (1, ?), (2, ?)" in
    (match run (Db.run st ~params:[ Db.V_int 10L; Db.V_null ]) with
     | Ok n -> Alcotest.(check int) "one of two rows written" 1 n
     | Error e -> Alcotest.failf "columnstore OR IGNORE raised: %a" Db.pp_error e);
    exec db "CREATE TABLE src (k INTEGER, v INTEGER)";
    exec db "INSERT INTO src VALUES (3, NULL)";
    exec db "INSERT INTO src VALUES (4, 40)";
    exec db "INSERT OR IGNORE INTO cs SELECT k, v FROM src";
    (* The literal spelling reaches the columnstore through the same suspended
       binder check, so it must skip here as well. *)
    exec db "INSERT OR IGNORE INTO cs VALUES (5, NULL), (6, 60)";
    Alcotest.(check (list string))
      "only the non-violating rows are stored"
      [ "1|10"; "4|40"; "6|60" ]
      (texts db "SELECT * FROM cs");
    (* And the storage really is clean afterwards — a skip that stored a NULL
       would be the #567 defect wearing a new name. *)
    Alcotest.(check (list string))
      "nothing was stored that the schema forbids"
      []
      (texts db "PRAGMA not_null_check"))
;;

let columnstore_other_resolutions_still_raise () =
  List.iter
    (fun modifier ->
       with_db (fun db ->
         exec db "CREATE TABLE cs (k INTEGER, v INTEGER NOT NULL) USING COLUMNSTORE";
         exec db "INSERT INTO cs VALUES (1, 10)";
         expect_not_null_error
           db
           ~table:"cs"
           ~col:"v"
           (Printf.sprintf "INSERT %sINTO cs VALUES (2, ?)" modifier)
           [ Db.V_null ];
         Alcotest.(check (list string))
           (Printf.sprintf "%S wrote nothing" modifier)
           [ "1|10" ]
           (texts db "SELECT * FROM cs")))
    [ ""; "OR ABORT "; "OR FAIL "; "OR ROLLBACK "; "OR REPLACE " ]
;;

(* ------------------------------------------------------------------ *)
(* Property                                                             *)
(* ------------------------------------------------------------------ *)

(* Over a random mix of good and NULL values: [OR IGNORE] never raises, the
   reported row count equals the number of non-NULL values, and every row that
   landed is one of the good ones.  The last clause is what stops a "fix" that
   skips by writing nothing but reports success for everything. *)
let prop_or_ignore_keeps_exactly_the_good_rows =
  QCheck2.Test.make
    ~name:"OR IGNORE writes exactly the non-NULL rows"
    ~count:100
    QCheck2.Gen.(list_size (int_range 1 12) (option (int_range 0 1000)))
    (fun vals ->
       with_db (fun db ->
         exec db "CREATE TABLE t (k INTEGER PRIMARY KEY, v INTEGER NOT NULL)";
         let params =
           List.concat
             (List.mapi
                (fun i v ->
                   [ Db.V_int (Int64.of_int (i + 1))
                   ; (match v with
                      | None -> Db.V_null
                      | Some n -> Db.V_int (Int64.of_int n))
                   ])
                vals)
         in
         let tuples = String.concat ", " (List.map (fun _ -> "(?, ?)") vals) in
         let st =
           prepare db (Printf.sprintf "INSERT OR IGNORE INTO t VALUES %s" tuples)
         in
         let expected = List.length (List.filter Option.is_some vals) in
         match run (Db.run st ~params) with
         | Error _ -> false
         | Ok n ->
           let stored = texts db "SELECT v FROM t" in
           let want =
             List.filter_map (Option.map string_of_int) vals |> List.sort compare
           in
           n = expected && List.sort compare stored = want))
;;

let () =
  Alcotest.run
    "not_null_599"
    [ ( "row-store"
      , [ Alcotest.test_case
            "OR IGNORE skips NOT NULL as well as UNIQUE"
            `Quick
            or_ignore_skips_both_constraint_kinds
        ; Alcotest.test_case
            "only the offending row is skipped"
            `Quick
            or_ignore_skips_only_the_offending_row
        ; Alcotest.test_case
            "INSERT ... SELECT skips too"
            `Quick
            or_ignore_insert_select_skips
        ; Alcotest.test_case
            "every spelling of the NULL skips, not just the parameter"
            `Quick
            or_ignore_skips_every_spelling_of_null
        ] )
    ; ( "other-resolutions"
      , [ Alcotest.test_case
            "ABORT / FAIL / ROLLBACK / REPLACE / bare all raise"
            `Quick
            other_resolutions_still_raise
        ; Alcotest.test_case
            "OR REPLACE does not substitute the DEFAULT (SQLite divergence)"
            `Quick
            or_replace_does_not_substitute_the_default
        ; Alcotest.test_case
            "UPDATE and UPSERT DO UPDATE keep raising"
            `Quick
            update_and_upsert_do_update_still_raise
        ; Alcotest.test_case
            "a literal NULL is still a bind error without OR IGNORE"
            `Quick
            literal_null_is_still_a_bind_error_without_or_ignore
        ; Alcotest.test_case
            "the UPDATE binders are unchanged"
            `Quick
            update_binders_unchanged
        ] )
    ; ( "columnstore"
      , [ Alcotest.test_case
            "OR IGNORE skips on both columnstore arms"
            `Quick
            columnstore_or_ignore_skips
        ; Alcotest.test_case
            "other resolutions raise on a columnstore"
            `Quick
            columnstore_other_resolutions_still_raise
        ] )
    ; ( "property"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_or_ignore_keeps_exactly_the_good_rows ] )
    ]
;;
