# #677 (item 2): `Op_limit` early-stop Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `Op_limit` stop pulling its child stream once it has `offset + limit` rows, instead of draining the whole child via `Lwt_stream.to_list`, without leaking the reader handles/cursors the 3 lazy base scanners hold open across pulls.

**Architecture:** Add an Lwt-sequence-storage registry (`stream_cleanup_key`), the same pattern this file already uses for `query_stats_key`/`txn_mode_key`. The 3 lazy scanners (`stream_seq_scan`, `stream_index_lookup`, `stream_fts_seq_scan`) push their existing idempotent `finish` closure into it when a scope is active. `Op_limit` opens a scope around its child's construction, pulls only `offset + limit` rows, then unconditionally flushes the registry (a no-op for anything that already finished naturally).

**Tech Stack:** OCaml, Lwt, Alcotest. Dev commands run inside the `granary-dev` podman image — never call `dune` on the host.

## Global Constraints

- Never call `dune` directly on the host — every build/test command below is wrapped in `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev …`, run from the worktree root (`.worktrees/677-limit-early-stop`).
- This plan implements ONLY item (2) of #677 (Op_limit early-stop). Items (1) covering-index and (3) sort elision are out of scope and stay open on #677.
- Full design and rationale: `docs/superpowers/specs/2026-08-08-677-limit-early-stop-design.md` (already committed on this branch).
- Branch: `fix/677-limit-early-stop`, worktree: `.worktrees/677-limit-early-stop`. All commits happen there.
- Follow `sh scripts/check-fmt.sh` before the final commit of each task that touches `.ml` files; read its `✓`/`◐` summary line, not just the exit code.

---

## Task 1: Regression tests for LIMIT/OFFSET correctness and rows-examined bound

**Files:**
- Create: `test/test_limit_early_stop_677.ml`
- Modify: `test/dune` (add a `(test …)` stanza for the new file, alongside the other single-file `#NNN` test stanzas — see the `test_correlated_exists_493` stanza around line 1181 for the pattern)

**Interfaces:**
- Consumes: `Db.open_in_memory`, `Db.of_store ~file_path`, `Db.execute`, `Db.query`, `Db.query_with_stats` (all in `lib/db/db.mli`), `Granary_unix.Store.open_file`, `Store.active_reader_count` / `Store.live_read_locks` / `Store.pinned_page_count` (`lib/store/store.mli`, module `Granary_store.Store`).
- Produces: nothing consumed by later tasks — this is a leaf test file. It pins the contract Task 2's implementation must satisfy: correctness of `LIMIT`/`OFFSET` results, an upper bound on `rows_examined`/`index_entries` from `Db.query_with_stats`, and zero net reader/pin/lock growth after a `LIMIT`-bounded query on disk.

This test file is written against **today's** `Op_limit` (full drain). The correctness assertions will pass immediately. The `rows_examined` bound assertions are expected to **fail** until Task 2 lands — that is the point: they are the regression test that proves the fix does something.

- [ ] **Step 1: Write the test file**

```ocaml
(** #677 (item 2 of 3): [Op_limit] must stop pulling its child once it has
    [offset + limit] rows, not drain the child to exhaustion and slice
    afterward. This file pins three things:

    - Correctness: the same rows, in the same order, before and after the
      fix — [LIMIT]/[OFFSET] slicing must not change.
    - The actual perf claim: {!Db.query_with_stats}'s [rows_examined] /
      [index_entries] for a [LIMIT n] query must be bounded near
      [offset + n], not the size of the table or index range it draws from.
      Asserted for all 3 lazy scanners #677 names: a plain table scan
      ({!seq_scan_stops_early}), an index lookup
      ({!index_lookup_stops_early}), and an FTS sequential scan
      ({!fts_seq_scan_stops_early}).
    - No leak: after a [LIMIT]-bounded query returns, on disk,
      {!Store.active_reader_count} / {!Store.live_read_locks} /
      {!Store.pinned_page_count} must be back at their pre-query baseline —
      the #164/#493/#546 discipline this change must not violate.
      [Mem] answers 0 for all three unconditionally, so this needs an
      on-disk store, same as {!Store.active_reader_count} in
      [test_correlated_exists_493.ml]. *)

open Lwt.Syntax
module Db = Granary.Db
module Store = Granary_store.Store

let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/granary_test_limit_early_stop_677_%04d.db" n
;;

let cleanup path =
  List.iter
    (fun p ->
       try Unix.unlink p with
       | _ -> ())
    [ path; path ^ "-wal"; path ^ ".aslog" ]
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

let rows_of db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    List.map (fun r -> Array.to_list (Array.map render r)) (run (Lwt_stream.to_list stream))
;;

let stats_of db sql =
  match
    run
      (let* r = Db.query_with_stats db sql in
       match r with
       | Error e -> Lwt.return (Error e)
       | Ok (stream, stats) ->
         let* rows = Lwt_stream.to_list stream in
         Lwt.return (Ok (List.length rows, stats)))
  with
  | Ok v -> v
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
;;

let with_mem_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

(* On-disk, with the raw [Store.t] kept in hand for the leak assertions —
   the [Mem] backend answers 0 for all three counters no matter what leaks. *)
let with_disk_db f =
  let path = fresh_path () in
  cleanup path;
  let store =
    match run (Granary_unix.Store.open_file ~path ()) with
    | Ok s -> s
    | Error e -> Alcotest.failf "open_file: %a" Store.pp_error e
  in
  let db = run (Db.of_store ~file_path:path store) in
  Fun.protect
    ~finally:(fun () ->
      (try run (Db.close db) with
       | _ -> ());
      cleanup path)
    (fun () -> f db store)
;;

let probes store =
  Store.active_reader_count store, Store.live_read_locks store, Store.pinned_page_count store
;;

let check_released ~label store baseline =
  let br, bl, bp = baseline in
  let ar, al, ap = probes store in
  Alcotest.(check int) (label ^ ": live readers") br ar;
  Alcotest.(check int) (label ^ ": read locks") bl al;
  Alcotest.(check int) (label ^ ": pinned pages") bp ap
;;

(* ------------------------------------------------------------------ *)
(* Correctness: LIMIT/OFFSET slicing must not change                    *)
(* ------------------------------------------------------------------ *)

let n_rows = 200

let seed_plain db =
  exec db "CREATE TABLE wide (id INTEGER PRIMARY KEY, v INTEGER)";
  exec db "BEGIN";
  for id = 1 to n_rows do
    exec db (Printf.sprintf "INSERT INTO wide VALUES (%d, %d)" id (id * 10))
  done;
  exec db "COMMIT"
;;

let limit_returns_first_n () =
  with_mem_db
  @@ fun db ->
  seed_plain db;
  let got = rows_of db "SELECT v FROM wide LIMIT 5" in
  let expected = List.init 5 (fun i -> [ string_of_int ((i + 1) * 10) ]) in
  Alcotest.(check (list (list string))) "first 5" expected got
;;

let limit_offset_slices_middle () =
  with_mem_db
  @@ fun db ->
  seed_plain db;
  let got = rows_of db "SELECT v FROM wide LIMIT 3 OFFSET 2" in
  let expected = List.init 3 (fun i -> [ string_of_int ((i + 3) * 10) ]) in
  Alcotest.(check (list (list string))) "offset 2, limit 3" expected got
;;

let limit_past_end_returns_remainder () =
  with_mem_db
  @@ fun db ->
  seed_plain db;
  let got = rows_of db (Printf.sprintf "SELECT v FROM wide LIMIT 10 OFFSET %d" (n_rows - 3)) in
  let expected = List.init 3 (fun i -> [ string_of_int ((n_rows - 3 + i + 1) * 10) ]) in
  Alcotest.(check (list (list string))) "offset near end" expected got
;;

let limit_zero_returns_nothing () =
  with_mem_db
  @@ fun db ->
  seed_plain db;
  let got = rows_of db "SELECT v FROM wide LIMIT 0" in
  Alcotest.(check (list (list string))) "limit 0" [] got
;;

(* ------------------------------------------------------------------ *)
(* rows_examined / index_entries bound — the perf claim, expected RED   *)
(* until Task 2's Op_limit change lands.                                *)
(* ------------------------------------------------------------------ *)

let seq_scan_stops_early () =
  with_mem_db
  @@ fun db ->
  seed_plain db;
  let n, stats = stats_of db "SELECT v FROM wide LIMIT 5" in
  Alcotest.(check int) "rows returned" 5 n;
  Alcotest.(check bool)
    "rows_examined bounded near the limit, not the table size"
    true
    (stats.Db.rows_examined <= 10)
;;

let seed_indexed db =
  exec db "CREATE TABLE t2 (id INTEGER PRIMARY KEY, k INTEGER, v INTEGER)";
  exec db "CREATE INDEX idx_t2_k ON t2 (k)";
  exec db "BEGIN";
  for id = 1 to n_rows do
    exec db (Printf.sprintf "INSERT INTO t2 VALUES (%d, 1, %d)" id id)
  done;
  exec db "COMMIT"
;;

let index_lookup_stops_early () =
  with_mem_db
  @@ fun db ->
  seed_indexed db;
  let n, stats = stats_of db "SELECT v FROM t2 WHERE k = 1 LIMIT 3 OFFSET 2" in
  let expected = List.init 3 (fun i -> string_of_int (i + 3)) in
  Alcotest.(check int) "rows returned" 3 n;
  Alcotest.(check bool)
    "index_entries bounded near offset+limit, not the matching-key count"
    true
    (stats.Db.index_entries <= 10);
  ignore expected
;;

let seed_fts db =
  exec db "CREATE VIRTUAL TABLE doc USING fts5(body)";
  exec db "BEGIN";
  for id = 1 to n_rows do
    exec db (Printf.sprintf "INSERT INTO doc VALUES ('widget %d')" id)
  done;
  exec db "COMMIT"
;;

let fts_seq_scan_stops_early () =
  with_mem_db
  @@ fun db ->
  seed_fts db;
  let n, stats = stats_of db "SELECT body FROM doc LIMIT 4" in
  Alcotest.(check int) "rows returned" 4 n;
  Alcotest.(check bool)
    "rows_examined bounded near the limit, not the content table size"
    true
    (stats.Db.rows_examined <= 10)
;;

(* ------------------------------------------------------------------ *)
(* No leak: readers/pins/locks return to baseline after a LIMIT query   *)
(* ------------------------------------------------------------------ *)

let limit_query_leaves_no_reader_behind () =
  with_disk_db
  @@ fun db store ->
  seed_plain db;
  let baseline = probes store in
  let n = List.length (rows_of db "SELECT v FROM wide LIMIT 5") in
  Alcotest.(check int) "rows returned" 5 n;
  check_released ~label:"seq scan LIMIT" store baseline
;;

let index_lookup_limit_leaves_no_reader_behind () =
  with_disk_db
  @@ fun db store ->
  seed_indexed db;
  let baseline = probes store in
  let n = List.length (rows_of db "SELECT v FROM t2 WHERE k = 1 LIMIT 3 OFFSET 2") in
  Alcotest.(check int) "rows returned" 3 n;
  check_released ~label:"index lookup LIMIT" store baseline
;;

let () =
  Alcotest.run
    "limit_early_stop_677"
    [ ( "correctness"
      , [ Alcotest.test_case "limit returns first n" `Quick limit_returns_first_n
        ; Alcotest.test_case "limit+offset slices middle" `Quick limit_offset_slices_middle
        ; Alcotest.test_case
            "limit past end returns remainder"
            `Quick
            limit_past_end_returns_remainder
        ; Alcotest.test_case "limit 0 returns nothing" `Quick limit_zero_returns_nothing
        ] )
    ; ( "rows_examined_bound"
      , [ Alcotest.test_case "seq scan stops early" `Quick seq_scan_stops_early
        ; Alcotest.test_case "index lookup stops early" `Quick index_lookup_stops_early
        ; Alcotest.test_case "fts seq scan stops early" `Quick fts_seq_scan_stops_early
        ] )
    ; ( "no_leak"
      , [ Alcotest.test_case
            "limit query leaves no reader behind"
            `Quick
            limit_query_leaves_no_reader_behind
        ; Alcotest.test_case
            "index lookup limit leaves no reader behind"
            `Quick
            index_lookup_limit_leaves_no_reader_behind
        ] )
    ]
;;
```

- [ ] **Step 2: Add the dune stanza**

In `test/dune`, add this stanza near the other single-file `#NNN` test stanzas (e.g. right after the `test_or_ignore_upsert_639` stanza — search for `(name test_or_ignore_upsert_639)` to find the spot):

```dune
; #677 (item 2 of 3): Op_limit must stop pulling its child at offset+limit
; rows instead of draining it fully via Lwt_stream.to_list.

(test
 (name test_limit_early_stop_677)
 (modules test_limit_early_stop_677)
 (libraries
  granary
  granary.unix
  granary.encoding
  granary.store
  alcotest
  lwt.unix
  unix))
```

- [ ] **Step 3: Run the new test and confirm the expected RED**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev \
  dune test test/test_limit_early_stop_677.exe
```

Expected: the `correctness` and `no_leak` groups PASS (today's `Op_limit` already returns correct slices and, because it fully drains, already releases every reader — draining to exhaustion is what triggers each scanner's own `finish`). The `rows_examined_bound` group FAILS on all three cases, because today's `Op_limit` drains the whole table/index range before slicing, so `rows_examined`/`index_entries` equals `n_rows` (200), not `<= 10`.

If `correctness` or `no_leak` fail instead, stop and investigate before proceeding — that means an assumption in this plan about current behavior is wrong, and Task 2 must not be started on a false premise.

- [ ] **Step 4: Commit**

```sh
git add test/test_limit_early_stop_677.ml test/dune
git commit -m "$(cat <<'EOF'
test(#677): pin LIMIT/OFFSET correctness and add the rows-examined bound

The rows_examined/index_entries assertions are expected to fail against
today's Op_limit, which drains its child stream fully before slicing.
They pin the perf claim the early-stop change (next commit) must satisfy.
EOF
)"
```

---

## Task 2: Implement `Op_limit` early-stop via a scoped cleanup registry

**Files:**
- Modify: `lib/sql/exec.ml`
  - Add `stream_cleanup_key` near `query_stats_key` (`lib/sql/exec.ml:6930`)
  - Register `finish` in `stream_seq_scan` (`lib/sql/exec.ml:10244`), `stream_index_lookup` (`lib/sql/exec.ml:10560`), `stream_fts_seq_scan` (`lib/sql/exec.ml:11725`)
  - Rewrite the `Op_limit` case of `to_stream` (`lib/sql/exec.ml:12703`)

**Interfaces:**
- Consumes: `query_stats_key`'s declaration site and comment style (`lib/sql/exec.ml:6923-6930`) as the pattern to follow. `Lwt_stream.get`, `Lwt.with_value`, `Lwt.get` (stdlib Lwt).
- Produces: `stream_cleanup_key : (unit -> unit Lwt.t) list ref Lwt.key`, usable by any future scanner that needs the same early-stop cleanup discipline (none currently do, per the design doc's survey — `stream_rowid_lookup`, the join operators, and `stream_aggregate`'s fast path are all already eager).

- [ ] **Step 1: Add the cleanup registry key**

In `lib/sql/exec.ml`, immediately after the `query_stats_key` declaration (`lib/sql/exec.ml:6930`, before the `txn_mode_key` comment block), add:

```ocaml
(* #677: a registry of cleanup thunks for the lazy base scanners
   (stream_seq_scan / stream_index_lookup / stream_fts_seq_scan) that hold a
   live Store reader handle and cursor across pulls, releasing it only via
   their own idempotent [finish] closure when the stream is drained to
   exhaustion or raises.  [Op_limit] needs to stop pulling before
   exhaustion — that's the whole point of early-stop — without leaking that
   handle, which is exactly the #164/#493/#546 class of bug.

   [Op_limit] opens a scope with [Lwt.with_value] around its child's
   construction; the 3 lazy scanners push their [finish] into this key if a
   scope is active, and [Op_limit] flushes every registered thunk once it
   has enough rows.  Flushing is safe even when nothing is left to do:
   every [finish] is idempotent (guarded by its own [ended] ref), so a
   thunk that already ran because the child was fully drained naturally is
   just a no-op the second time.

   Nesting is handled by [Lwt.with_value]'s ordinary dynamic scoping: a
   nested [Op_limit] (e.g. inside a correlated subquery) opens its own inner
   scope for its own child construction, so its scanners register into its
   own registry, not this one's. *)
let stream_cleanup_key : (unit -> unit Lwt.t) list ref Lwt.key = Lwt.new_key ()

let register_stream_cleanup (finish : unit -> unit Lwt.t) : unit =
  match Lwt.get stream_cleanup_key with
  | Some reg -> reg := finish :: !reg
  | None -> ()
;;
```

- [ ] **Step 2: Register `finish` in `stream_seq_scan`**

In `lib/sql/exec.ml` at line 10244 (inside `stream_seq_scan`), the closure is currently:

```ocaml
  let ended = ref false in
  let finish () =
    if !ended
    then Lwt.return_unit
    else (
      ended := true;
      S.seek_close cur;
      rh_finish rh)
  in
  let stream =
    Lwt_stream.from (fun () ->
```

Change it to register `finish` right after defining it:

```ocaml
  let ended = ref false in
  let finish () =
    if !ended
    then Lwt.return_unit
    else (
      ended := true;
      S.seek_close cur;
      rh_finish rh)
  in
  register_stream_cleanup finish;
  let stream =
    Lwt_stream.from (fun () ->
```

- [ ] **Step 3: Register `finish` in `stream_index_lookup`**

At line 10560 (inside `stream_index_lookup`), the same shape:

```ocaml
    let exhausted = ref false in
    let ended = ref false in
    let finish () =
      if !ended
      then Lwt.return_unit
      else (
        ended := true;
        S.seek_close cur;
        rh_finish rh)
    in
```

becomes:

```ocaml
    let exhausted = ref false in
    let ended = ref false in
    let finish () =
      if !ended
      then Lwt.return_unit
      else (
        ended := true;
        S.seek_close cur;
        rh_finish rh)
    in
    register_stream_cleanup finish;
```

- [ ] **Step 4: Register `finish` in `stream_fts_seq_scan`**

At line 11725 (inside `stream_fts_seq_scan`):

```ocaml
  let exhausted = ref false in
  let ended = ref false in
  let finish () =
    if !ended
    then Lwt.return_unit
    else (
      ended := true;
      S.cursor_close cur;
      rh_finish rh)
  in
```

becomes:

```ocaml
  let exhausted = ref false in
  let ended = ref false in
  let finish () =
    if !ended
    then Lwt.return_unit
    else (
      ended := true;
      S.cursor_close cur;
      rh_finish rh)
  in
  register_stream_cleanup finish;
```

- [ ] **Step 5: Rewrite the `Op_limit` case**

At `lib/sql/exec.ml:12703`, replace:

```ocaml
  | Plan.Op_limit { limit; offset; child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    let* rows = Lwt_stream.to_list inner in
    let rows' = List.filteri (fun i _ -> i >= offset && i < offset + limit) rows in
    Lwt.return (Lwt_stream.of_list rows')
```

with:

```ocaml
  | Plan.Op_limit { limit; offset; child } ->
    (* #677 (item 2 of 3): pull only [offset + limit] rows from the child
       instead of draining it fully — [Sema.validate_limit_offset] already
       rejects a negative [limit]/[offset] before an [Op_limit] node can
       exist, so [want] is always >= 0 and [pull] terminates.  The cleanup
       registry (see [stream_cleanup_key]) is what makes stopping early safe:
       any of the 3 lazy scanners constructed while building [inner] register
       their [finish] into [cleanups], and flushing it here after the pull —
       whether or not the child was actually exhausted — releases whatever
       reader handle/cursor they still hold. *)
    let cleanups = ref [] in
    let* inner =
      Lwt.with_value stream_cleanup_key (Some cleanups) (fun () ->
        to_stream clock params store ~mode ~cat child)
    in
    let want = offset + limit in
    let rec pull n acc =
      if n <= 0
      then Lwt.return (List.rev acc)
      else
        let* v = Lwt_stream.get inner in
        match v with
        | None -> Lwt.return (List.rev acc)
        | Some row -> pull (n - 1) (row :: acc)
    in
    let* rows = pull want [] in
    let* () = Lwt_list.iter_s (fun f -> f ()) !cleanups in
    let rows' = List.filteri (fun i _ -> i >= offset) rows in
    Lwt.return (Lwt_stream.of_list rows')
```

- [ ] **Step 6: Build**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build
```

Expected: builds clean. If `Lwt_list` is not already opened/aliased in `exec.ml`, use its fully qualified name (`Lwt_list.iter_s`) — check the top of `lib/sql/exec.ml` for existing `Lwt_list` usage first (`grep -n "Lwt_list\." lib/sql/exec.ml`) and match whatever qualification the file already uses.

- [ ] **Step 7: Run the Task 1 test file and confirm all groups pass**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev \
  dune test test/test_limit_early_stop_677.exe
```

Expected: PASS, including `rows_examined_bound` this time.

- [ ] **Step 8: Run the full test suite**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test
```

Expected: everything green. This is the real regression gate — `Op_limit` is used by every `LIMIT` query in the engine, so any subtle change in pull semantics (e.g. an off-by-one in `pull`, or a query relying on the old full-materialization-then-filter order) will show up somewhere in the existing suite, most likely in `test_e2e.ml`, `test_conformance.ml`, or `test_sqlite_compare.ml`.

- [ ] **Step 9: Format check**

```sh
sh scripts/check-fmt.sh
```

Fix any reported diffs with `sh scripts/check-fmt.sh --fix`, then re-run to confirm clean. Read the final summary line (`✓` vs `◐`) as documented in `CLAUDE.md` — a `◐` here just means dune-file formatting is unverified in a worktree, which is expected and checked separately before opening the PR.

- [ ] **Step 10: Commit**

```sh
git add lib/sql/exec.ml
git commit -m "$(cat <<'EOF'
fix(#677): Op_limit stops pulling its child at offset+limit rows

Op_limit used to drain its child stream fully via Lwt_stream.to_list
before slicing to [offset, offset+limit) — every LIMIT query in the
engine paid for every row an unbounded scan would have produced.

Stopping the pull early without leaking is the actual work: the 3 lazy
base scanners (stream_seq_scan, stream_index_lookup, stream_fts_seq_scan)
hold a live Store reader handle/cursor across pulls, released only by
their own finish closure on natural exhaustion or an exception. A new
stream_cleanup_key registry (same Lwt-sequence-storage pattern as
query_stats_key/#239) lets Op_limit open a scope, let those scanners
register their already-idempotent finish into it, pull only
offset+limit rows, then flush the registry regardless of whether the
child was actually exhausted.

Everything else (joins, sort, aggregate) already materializes its child
fully by construction, so nothing else needed to change — see the design
doc for the full survey.

Item 2 of 3 from #677; items 1 (covering-index) and 3 (sort elision)
stay open on that issue.
EOF
)"
```

---

## Task 3: merlint, dune-file formatting, and open the PR

**Files:** none new — verification only, plus the PR itself.

**Interfaces:** none.

- [ ] **Step 1: Run merlint**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
```

Expected: 0 issues introduced by this change. `stream_cleanup_key` and `register_stream_cleanup` are `let`-bound in `exec.ml`, not in a `.mli`-guarded module boundary, so merlint's "every public `val` needs a doc comment" rule does not apply to them — confirm this by checking whether `lib/sql/exec.mli` exists and, if it does, whether it exports anything from this area (`grep -n "stream_cleanup\|query_stats_key" lib/sql/exec.mli` if the file exists).

- [ ] **Step 2: Verify dune-file formatting in the main checkout**

Since `test/dune` was touched in Task 1, and `check-fmt.sh` cannot verify dune-file formatting from inside a worktree (see `CLAUDE.md`'s "Before pushing" section), verify it from the main checkout:

```sh
cd /home/tej/projects/sqlite_ocaml_port
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build @fmt
```

If it reports a diff for `test/dune`, apply the same formatting to the worktree's copy of the file (copy the corrected file back, or hand-apply the diff), then re-commit in the worktree:

```sh
cd .worktrees/677-limit-early-stop
git add test/dune
git commit -m "fmt: dune-file formatting for test/dune"
```

- [ ] **Step 3: Push and open the PR**

```sh
cd .worktrees/677-limit-early-stop
git push origin fix/677-limit-early-stop
~/.local/bin/forgejo pr create IoTReadyNext/granary \
  --title="fix(#677): Op_limit stops pulling its child at offset+limit rows" \
  --head=fix/677-limit-early-stop \
  --base=main \
  --body="$(cat <<'EOF'
## Summary
- Op_limit now pulls only `offset + limit` rows from its child instead of
  draining it fully via `Lwt_stream.to_list`, fixing every `LIMIT` query in
  the engine, not just the #677/#674 TPC-C shapes.
- A new `stream_cleanup_key` registry (same Lwt-sequence-storage pattern as
  `query_stats_key`/#239) lets the 3 lazy base scanners
  (`stream_seq_scan`, `stream_index_lookup`, `stream_fts_seq_scan`) register
  their existing idempotent `finish` closure, so stopping early does not
  leak the #164/#493/#546 class of reader-handle/cursor bug. Everything
  else (joins, sort, aggregate) already materializes its child eagerly, so
  nothing else changed.
- Item 2 of 3 from #677; items 1 (covering-index shortcut) and 3 (sort
  elision) stay open on that issue.
- Design doc: `docs/superpowers/specs/2026-08-08-677-limit-early-stop-design.md`

## Test plan
- [ ] `dune test` passes (full suite)
- [ ] New `test/test_limit_early_stop_677.ml`: LIMIT/OFFSET correctness,
      rows_examined/index_entries bound (the actual perf claim), and
      no-reader-leak assertions on disk, covering all 3 lazy scanner shapes

Refs #677, #674
EOF
)"
```

- [ ] **Step 4: Report the PR URL**

Paste the URL returned by `forgejo pr create` back to the user. Do not merge it — merging is a separate, explicit step.

---

## Self-Review Notes (for whoever executes this plan)

- **Spec coverage:** Task 1 covers the design doc's "Testing" section in full (correctness, reader-leak, rows-examined). Task 2 covers the "Mechanism" section's exact code (registry key, 3 registration sites, `Op_limit` rewrite). Task 3 covers the repo's standard PR checklist (merlint, dune-file fmt, PR body referencing the issue).
- **Type consistency:** `stream_cleanup_key : (unit -> unit Lwt.t) list ref Lwt.key` is defined once in Task 2 Step 1 and consumed identically (`register_stream_cleanup`, `Lwt.with_value stream_cleanup_key (Some cleanups)`) in every later step — no signature drift.
- **Known risk to watch during Task 2 Step 8 (full suite run):** any existing test that asserts on `Db.query_with_stats` counters for a `LIMIT` query written against the *old* full-drain behavior will need its expected numbers updated — that is a legitimate consequence of this fix, not a regression, and should be fixed in the same commit rather than worked around.
