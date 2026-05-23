# Phase 41 — Concurrency Test Hardening (#161 + #162)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the two test-infrastructure gaps surfaced by #158. (a) Eliminate the latent nested-`Lwt_main.run` footgun in `test/test_e2e.ml`'s `query_ok` family so future async-backend changes don't trip on it. (b) Add a regression bench that genuinely exercises the `Unix_file.pread`-yield path independent of Pager cache geometry, so a revert of #158 actually fails CI.

**Architecture:** (a) Introduce parallel monadic helpers (`query_ok_lwt`, `exec_lwt`, `fresh_db_lwt`, `exec_err_lwt`) alongside the existing sync wrappers. Migrate every call site that lives inside an outer `Lwt_main.run` / `run (...)` block. Top-level callers keep the sync helpers. (b) New bench `test/bench_slow_read_yield.ml` opens a WAL store with an injected `Lwt_unix.sleep` (5 ms default) in front of the BLOCK `read_page` callback, opens a fresh DB per iteration so the Pager cache starts cold, runs a reader fiber alongside a writer with fast `wal_sync`, and asserts the writer's wall time stays bounded close to its own work (proxy for "the reader yielded during slow I/O").

**Tech Stack:** OCaml + Lwt; modules touched: `test/test_e2e.ml`, `test/bench_slow_read_yield.ml` (new), `test/dune`. Tests via Alcotest + Lwt under podman dune.

---

## File Structure

**Modified:**
- `test/test_e2e.ml` — add `*_lwt` helper family at top; migrate at-risk call sites in nested-`Lwt_main.run` regions to the `*_lwt` variants.
- `test/dune` — register new bench.

**Created:**
- `test/bench_slow_read_yield.ml` — slow-read I/O yield bench (#162 regression detector).

**Files that must change together:** none — each task is self-contained. The migration in Task 3 is mechanical and can be split per region without breaking the suite.

---

## Build / test conventions (read me first)

- **All `dune` commands run inside podman**, not on host: `podman run --rm -v "$(pwd):/workspace:Z" -w /workspace sqlocaml-dev dune build`, `podman run --rm -v "$(pwd):/workspace:Z" -w /workspace sqlocaml-dev dune runtest --force`. `sqlocaml-dev` is the image name; never `podman exec` against an image.
- **Stage commits with `git add <files>`**, never `-A` (the `_build/` tree is owned by root because of podman).
- **Forgejo CLI**: `~/.local/bin/forgejo issue …` and the repo is `tej/sqlite_ocaml_port`. To close: `forgejo issue edit tej/sqlite_ocaml_port <num> --state=closed`.
- **No emojis** in code or commit messages.

---

## Task 1 — Add monadic helper family to `test_e2e.ml` (#161)

**Why:** `query_ok` and the `run` alias (lines 9, 33) wrap `Lwt_main.run` internally. They are safe at top level, but every call site inside an outer `Lwt_main.run` block is a latent bug — the moment the inner `Db.query` actually needs the scheduler (any time real I/O cache-misses through `Lwt_unix.pread`), Lwt aborts with "Nested calls to Lwt_main.run are not allowed". We need a monadic variant so at-risk sites can be migrated without rewriting them from scratch.

**Files:**
- Modify: `test/test_e2e.ml` (top, near existing helpers at lines 9-40)

### Step 1.1 — Write a failing test that exercises the new helper

Pick any existing top-level test that uses `query_ok` and clone it to use `query_ok_lwt`, wrapped in a single outer `Lwt_main.run`. The point is to assert the helper exists, type-checks, and returns the same rows as `query_ok`.

- [ ] **Append at the end of `test/test_e2e.ml`** (above the runner registration) — replace `<runner_block>` with the existing `let () = Alcotest.run ...` block; we add an entry next to an existing group:

```ocaml
(* ------------------------------------------------------------------ *)
(* Helper tests (#161): the *_lwt family must exist and behave        *)
(* identically to their sync counterparts at the outermost layer.     *)
(* ------------------------------------------------------------------ *)

let helper_query_ok_lwt_smoke () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* r1 = Db.execute db "CREATE TABLE t (x INTEGER)" in
    (match r1 with
     | Ok () -> ()
     | Error e -> Alcotest.failf "create: %s" (fmt_err e));
    let* r2 = Db.execute db "INSERT INTO t (x) VALUES (1), (2), (3)" in
    (match r2 with
     | Ok () -> ()
     | Error e -> Alcotest.failf "insert: %s" (fmt_err e));
    let* rows = query_ok_lwt db "SELECT x FROM t ORDER BY x" in
    Alcotest.(check int) "row count" 3 (List.length rows);
    let* () = Db.close db in
    Lwt.return_unit
  )
```

- [ ] **In the existing `Alcotest.run` invocation** (search for `Alcotest.run "E2E"`), add a group entry — find a suitable location near other helpers:

```ocaml
    "helpers (#161)", [
      Alcotest.test_case "query_ok_lwt smoke" `Quick helper_query_ok_lwt_smoke;
    ];
```

- [ ] **Run it to verify it fails** — `query_ok_lwt` is not yet defined:

```bash
podman run --rm -v "$(pwd):/workspace:Z" -w /workspace sqlocaml-dev dune build test/test_e2e.exe 2>&1 | head -20
```

Expected: `Error: Unbound value query_ok_lwt`.

### Step 1.2 — Add the helper family

- [ ] **Edit `test/test_e2e.ml`** — replace the existing sync helpers block (around lines 9-40) with both sync and `_lwt` variants. Insert the `_lwt` family directly after the existing helpers:

```ocaml
(** Lwt-monadic variant of [fresh_db].  Use inside an outer [Lwt_main.run]
    to avoid nesting it (which Lwt forbids and which fails the moment the
    inner [Db.query] actually needs the scheduler — see #161). *)
let fresh_db_lwt () = Db.open_in_memory ()

(** Lwt-monadic variant of [exec]. *)
let exec_lwt db sql =
  let* result = Db.execute db sql in
  match result with
  | Ok () -> Lwt.return_unit
  | Error _ -> Alcotest.failf "exec_lwt: unexpected error for: %s" sql

(** Lwt-monadic variant of [query_ok]. *)
let query_ok_lwt db sql =
  let* result = Db.query db sql in
  match result with
  | Error _ -> Alcotest.failf "query_ok_lwt: unexpected error for: %s" sql
  | Ok stream -> Lwt_stream.to_list stream

(** Lwt-monadic variant of [exec_err] (defined later in this file). *)
let exec_err_lwt db sql =
  let* result = Db.execute db sql in
  match result with
  | Ok ()    -> Alcotest.failf "exec_err_lwt: expected error for: %s" sql
  | Error e  -> Lwt.return (fmt_err e)
```

- [ ] **Run the smoke test** — expect it to pass:

```bash
podman run --rm -v "$(pwd):/workspace:Z" -w /workspace sqlocaml-dev \
  bash -c "dune build test/test_e2e.exe && ./_build/default/test/test_e2e.exe test 'helpers (#161)'"
```

Expected: `[OK] helpers (#161) ...query_ok_lwt smoke`.

### Step 1.3 — Commit

- [ ] **Commit:**

```bash
git add test/test_e2e.ml
git commit -m "$(cat <<'EOF'
test(#161): add Lwt-monadic helper family (query_ok_lwt, exec_lwt, fresh_db_lwt, exec_err_lwt)

Parallel non-nesting helpers so test sites that live inside an outer
Lwt_main.run can be migrated off the sync wrappers without rewriting.
Sync helpers (query_ok, exec, run) keep working for top-level callers.

Refs #161.

Co-Authored-By: Claude Opus 4.7 <noreply@anthropic.com>
EOF
)"
```

---

## Task 2 — Audit at-risk call sites in `test_e2e.ml` (#161)

**Why:** 391 `query_ok` calls — only a subset are at risk. The risk surface is calls that live *inside* a function whose body is already wrapped in `Lwt_main.run` (or `run (...)`). We need a concrete list to migrate; this task produces that list.

**Files:**
- Read: `test/test_e2e.ml`
- Create: `docs/superpowers/plans/2026-05-24-phase-41-audit.txt` (transient artefact, not committed)

### Step 2.1 — Generate the audit

- [ ] **Run the audit script:**

```bash
awk '
  /^let .*=/ { fn=NR; depth=0; nested=0; printed=0 }
  /Lwt_main\.run|^[[:space:]]*run \(/ { depth++; if (depth >= 2) nested=1 }
  /^\)/ { depth=0 }
  /query_ok |exec |fresh_db |exec_err / {
    if (nested && !printed) {
      print NR": "$0
      printed=1
    }
  }
' test/test_e2e.ml > /tmp/audit_161.txt
wc -l /tmp/audit_161.txt
head -40 /tmp/audit_161.txt
```

The awk heuristic above is approximate. The reliable manual check is:

- [ ] **Grep for every function that wraps `Lwt_main.run`:**

```bash
grep -n "Lwt_main\.run\|^let .* =$\|^let .* () =$" test/test_e2e.ml \
  | grep -B1 "Lwt_main\.run\|^[[:space:]]*run (" \
  | head -60
```

- [ ] **For each such function, read its body and note whether it calls `query_ok`, `exec`, `fresh_db`, or `exec_err` *inside* the `Lwt_main.run` lambda.** Record file:line pairs in a scratch file:

```text
# /tmp/audit_161.txt format: <line>: <enclosing function name> :: <helper>
657-674: test_trigger_persists_across_reopen :: (already fixed in ea979c6, sanity check)
8520-8560: test_phase35_deferred_btree_backend :: (already fixed in ea979c6, sanity check)
<NEW>: <enclosing function> :: query_ok
<NEW>: <enclosing function> :: exec
...
```

- [ ] **Sanity check the two already-fixed regions** — they should *not* contain new `query_ok` / `exec` calls; if they do, the fix was incomplete.

- [ ] **Output expected:** zero to thirty at-risk sites. (The two fixed in ea979c6 are the only ones known to fail today; the rest pass because their reads happen to hit the Pager cache.) Record the count.

No commit for this task — output is a scratch list for Task 3.

---

## Task 3 — Migrate at-risk sites to `*_lwt` helpers (#161)

**Why:** Each migrated site eliminates one latent failure. Mechanical edit but high count tolerance — each region is independent.

**Files:**
- Modify: `test/test_e2e.ml` (sites from Task 2 audit)

### Step 3.1 — Migrate one region as the template

Pick the first region from `/tmp/audit_161.txt`. The transformation pattern is:

**Before (at-risk: nested `Lwt_main.run` inside outer `run`):**

```ocaml
let test_foo () =
  Lwt_main.run (
    let* db = Db.open_file "/tmp/foo.db" in
    ...
    let rows = query_ok db "SELECT * FROM t" in   (* nested run! *)
    Alcotest.(check int) "n rows" 3 (List.length rows);
    ...
    Lwt.return_unit
  )
```

**After (single `Lwt_main.run`, monadic body):**

```ocaml
let test_foo () =
  Lwt_main.run (
    let* db = Db.open_file "/tmp/foo.db" in
    ...
    let* rows = query_ok_lwt db "SELECT * FROM t" in
    Alcotest.(check int) "n rows" 3 (List.length rows);
    ...
    Lwt.return_unit
  )
```

Rules:
- `query_ok db sql` → `let* rows = query_ok_lwt db sql in`
- `exec db sql` → `let* () = exec_lwt db sql in`
- `fresh_db ()` → `let* db = fresh_db_lwt () in`
- `exec_err db sql` → `let* err = exec_err_lwt db sql in`

- [ ] **Edit one region.** Build and run just that test:

```bash
podman run --rm -v "$(pwd):/workspace:Z" -w /workspace sqlocaml-dev \
  bash -c "dune build test/test_e2e.exe && ./_build/default/test/test_e2e.exe"
```

Expected: full test_e2e passes, with no `Nested calls to Lwt_main.run` failures.

### Step 3.2 — Migrate the remaining regions

- [ ] **Repeat the pattern for every region in `/tmp/audit_161.txt`.** Commit in batches of ~5 regions to keep diffs reviewable.

- [ ] **After every batch, run the full suite:**

```bash
podman run --rm -v "$(pwd):/workspace:Z" -w /workspace sqlocaml-dev dune runtest --force 2>&1 | tail -30
```

Expected: zero failures.

### Step 3.3 — Final grep verification

- [ ] **Confirm no nested `Lwt_main.run` patterns remain by greppable signature.** The reliable signal is `query_ok\|exec\|fresh_db\|exec_err` appearing on the same line or one line below `Lwt_main.run` or `run (`:

```bash
awk '
  /Lwt_main\.run|^[[:space:]]*run \(/ { in_run=1; brace=0 }
  in_run && /\(/ { brace++ }
  in_run && /\)/ { brace--; if (brace<=0) in_run=0 }
  in_run && /(query_ok|exec|fresh_db|exec_err) [a-z]/ { print NR": "$0 }
' test/test_e2e.ml | head
```

Expected: empty output (the only matches should be `*_lwt` variants, which are fine).

### Step 3.4 — Commit

- [ ] **Final commit on the migration batch:**

```bash
git add test/test_e2e.ml
git commit -m "$(cat <<'EOF'
test(#161): migrate at-risk query_ok/exec sites to monadic *_lwt variants

Every call that lived inside an outer Lwt_main.run / run (...) block is
now monadic, so the test no longer relies on Pager cache geometry to
avoid the "Nested calls to Lwt_main.run" abort.

Closes #161.

Co-Authored-By: Claude Opus 4.7 <noreply@anthropic.com>
EOF
)"
```

---

## Task 4 — Write the slow-read I/O yield bench (#162)

**Why:** `bench_wal_fsync_overlap` exercises writer-fsync overlap but its 200-row tree fits in the Pager cache after the first walk — subsequent walks are pure cache hits and never enter `Unix_file`. Reverting #158 to synchronous `Unix.read` would not fail that bench. This task adds the missing regression detector: a bench that opens a fresh DB per iteration (cold cache), injects per-read latency at the BLOCK device callback, and asserts the writer makes forward progress concurrent with the reader's slow I/O.

**Files:**
- Create: `test/bench_slow_read_yield.ml`
- Modify: `test/dune` (register new bench)

### Step 4.1 — Add the dune stanza

- [ ] **Edit `test/dune`** — append (after the `bench_wal_fsync_overlap` stanza at line 145):

```lisp
(test
 (name bench_slow_read_yield)
 (libraries sqlocaml.store sqlocaml.block alcotest lwt.unix unix))
```

### Step 4.2 — Write the bench

- [ ] **Create `test/bench_slow_read_yield.ml`:**

```ocaml
(** Phase 41 / #162 — slow-read I/O yield bench.

    Regression detector for #158: a bench that fails if [Unix_file.read_page]
    ever regresses to synchronous behaviour, *independent of Pager cache
    geometry*.

    [bench_wal_fsync_overlap] cannot do this — its 200-row working set fits
    entirely in the 64-page Pager cache after the first walk, so subsequent
    walks are pure cache hits and the [Unix_file] layer is never re-entered.
    Reverting #158 to a synchronous backend would not fail that bench.

    This bench fixes the gap with three changes:

    1. Open a fresh DB per iteration so the Pager cache starts cold and
       every page reference forces a real BLOCK [read_page] callback.
    2. Inject a per-read [Lwt_unix.sleep] (default 5 ms) in front of the
       BLOCK [read_page] callback to simulate slow / remote storage.  This
       is where the [Lwt_unix.pread] yield delivered by #158 has to land.
    3. Run a reader fiber alongside a writer fiber with a *fast* [wal_sync]
       (1 ms) so the reader's slow I/O is the only bottleneck.  Assert the
       writer's wall time stays bounded close to its own work.

    If the reader stops yielding (#158 regression), the writer is starved
    during the reader's run and the assertion trips.

    Env vars (all optional):
      SQLOCAML_BENCH_READ_DELAY_MS  per-read injected sleep   (default 5)
      SQLOCAML_BENCH_WAL_DELAY_MS   per-fsync injected sleep  (default 1)
      SQLOCAML_BENCH_N_COMMITS      writer commits            (default 30)
      SQLOCAML_BENCH_N_READS        reader cursor walks       (default 50)
      SQLOCAML_BENCH_SEED_ROWS      initial tree size         (default 200)
      SQLOCAML_BENCH_MAX_WRITER_S   pass/fail upper bound (s) (default 0.5)
*)

open Lwt.Syntax

module S  = Sqlocaml_store.Store
module UF = Sqlocaml_block.Unix_file

let run = Lwt_main.run

let getenv_int   k d = try int_of_string   (Sys.getenv k) with _ -> d
let getenv_float k d = try float_of_string (Sys.getenv k) with _ -> d

let unix_read_at fd ~offset (out : Cstruct.t) =
  let len = Cstruct.length out in
  try
    let _ = Unix.lseek fd (Int64.to_int offset) Unix.SEEK_SET in
    let tmp = Bytes.create len in
    let rec loop o r =
      if r = 0 then ()
      else
        let n = Unix.read fd tmp o r in
        if n = 0 then Bytes.fill tmp o r '\x00'
        else loop (o + n) (r - n)
    in
    loop 0 len;
    Cstruct.blit_from_bytes tmp 0 out 0 len;
    Lwt.return (Ok ())
  with Unix.Unix_error (e, _, _) ->
    Lwt.return (Error (Unix.error_message e))

let unix_write_at fd ~offset (src : Cstruct.t) =
  let len = Cstruct.length src in
  try
    let _ = Unix.lseek fd (Int64.to_int offset) Unix.SEEK_SET in
    let tmp = Bytes.create len in
    Cstruct.blit_to_bytes src 0 tmp 0 len;
    let rec loop o r =
      if r = 0 then ()
      else
        let n = Unix.write fd tmp o r in
        if n = 0 then failwith "short write"
        else loop (o + n) (r - n)
    in
    loop 0 len;
    Lwt.return (Ok ())
  with Unix.Unix_error (e, _, _) ->
    Lwt.return (Error (Unix.error_message e))

(* Open a WAL store whose BLOCK [read_page] callback sleeps [read_delay]
   seconds before delegating to the real [UF.read_page].  This is the
   moment of truth for #158: the [Lwt_unix.sleep] yields the scheduler,
   so a properly-cooperating Pager + Btree + cursor stack will let the
   writer fiber make progress during that stall. *)
let open_slow_read_store ~path ~read_delay ~wal_delay =
  let* fr = UF.open_ ~path in
  let file = match fr with
    | Ok f -> f
    | Error e -> Alcotest.failf "UF.open_: %a" UF.pp_error e
  in
  let wal_path = path ^ "-wal" in
  let wal_fd = Unix.openfile wal_path [Unix.O_RDWR; Unix.O_CREAT] 0o644 in
  let wal_size_bytes = Int64.of_int (Unix.lseek wal_fd 0 Unix.SEEK_END) in
  let read_page ~page_id buf =
    let* () = Lwt_unix.sleep read_delay in
    UF.read_page file ~page_id buf
    |> Lwt.map (function
      | Ok () -> Ok ()
      | Error e -> Error (Format.asprintf "%a" UF.pp_error e))
  in
  let write_page ~page_id buf =
    UF.write_page file ~page_id buf
    |> Lwt.map (function
      | Ok () -> Ok ()
      | Error e -> Error (Format.asprintf "%a" UF.pp_error e))
  in
  let sync () =
    UF.sync file
    |> Lwt.map (function
      | Ok () -> Ok ()
      | Error e -> Error (Format.asprintf "%a" UF.pp_error e))
  in
  let resize ~n_pages =
    UF.resize file ~n_pages
    |> Lwt.map (function
      | Ok () -> Ok ()
      | Error e -> Error (Format.asprintf "%a" UF.pp_error e))
  in
  let wal_read_at  = unix_read_at  wal_fd in
  let wal_write_at = unix_write_at wal_fd in
  let wal_sync () =
    let* () = Lwt_unix.sleep wal_delay in
    Lwt.return (Ok ())
  in
  let close () = let* _ = UF.close file in Lwt.return_unit in
  let wal_close () = Unix.close wal_fd; Lwt.return_unit in
  let n_pages = UF.n_pages file in
  S.open_block_wal
    ~read_page ~write_page ~sync ~resize ~n_pages
    ~wal_read_at ~wal_write_at ~wal_sync ~wal_size_bytes
    ~close ~wal_close

let fresh_path () =
  let f = Filename.temp_file "bench_slow_read_yield" ".db" in
  (try Unix.unlink f with _ -> ());
  (try Unix.unlink (f ^ "-wal") with _ -> ());
  f

let seed_store ~path ~rows =
  let* sr = open_slow_read_store ~path ~read_delay:0.0 ~wal_delay:0.0 in
  let st = match sr with
    | Ok s -> s
    | Error e -> Alcotest.failf "open: %a" S.pp_error e
  in
  let* (S.Rw _ as tx) = S.rw_begin st in
  let rec ins i =
    if i >= rows then Lwt.return_unit
    else
      let k = Bytes.of_string (Printf.sprintf "k%08d" i) in
      let v = Bytes.of_string (Printf.sprintf "v%08d" i) in
      let* _ = S.put tx ~tree_id:1 ~key:k ~value:v in
      ins (i + 1)
  in
  let* () = ins 0 in
  let* () = S.commit tx in
  let* () = S.close st in
  Lwt.return_unit

let main () =
  let read_delay   = getenv_float "SQLOCAML_BENCH_READ_DELAY_MS"  5.0 /. 1000.0 in
  let wal_delay    = getenv_float "SQLOCAML_BENCH_WAL_DELAY_MS"   1.0 /. 1000.0 in
  let n_commits    = getenv_int   "SQLOCAML_BENCH_N_COMMITS"      30 in
  let n_reads      = getenv_int   "SQLOCAML_BENCH_N_READS"        50 in
  let seed_rows    = getenv_int   "SQLOCAML_BENCH_SEED_ROWS"      200 in
  let max_writer_s = getenv_float "SQLOCAML_BENCH_MAX_WRITER_S"   0.5 in

  let path = fresh_path () in
  let* () = seed_store ~path ~rows:seed_rows in

  let* sr = open_slow_read_store ~path ~read_delay ~wal_delay in
  let st = match sr with
    | Ok s -> s
    | Error e -> Alcotest.failf "open: %a" S.pp_error e
  in

  let writer_start = ref 0.0 in
  let writer_done  = ref 0.0 in
  let reader_done  = ref 0.0 in
  let t0 = Unix.gettimeofday () in

  let writer =
    writer_start := Unix.gettimeofday () -. t0;
    let rec loop i =
      if i >= n_commits then Lwt.return_unit
      else
        let* (S.Rw _ as tx) = S.rw_begin st in
        let k = Bytes.of_string (Printf.sprintf "w%08d" i) in
        let v = Bytes.of_string (Printf.sprintf "vv%08d" i) in
        let* _ = S.put tx ~tree_id:2 ~key:k ~value:v in
        let* () = S.commit tx in
        loop (i + 1)
    in
    let* () = loop 0 in
    writer_done := Unix.gettimeofday () -. t0;
    Lwt.return_unit
  in

  let reader =
    let* (S.Ro _ as tx) = S.ro_begin st in
    let rec loop i =
      if i >= n_reads then Lwt.return_unit
      else
        let* cur = S.cursor_open tx ~tree_id:1 in
        let rec walk () =
          let* r = S.cursor_next cur in
          match r with
          | None -> Lwt.return_unit
          | Some _ -> walk ()
        in
        let* () = walk () in
        loop (i + 1)
    in
    let* () = loop 0 in
    let* () = S.ro_end tx in
    reader_done := Unix.gettimeofday () -. t0;
    Lwt.return_unit
  in

  let* () = Lwt.join [writer; reader] in
  let* () = S.close st in
  (try Unix.unlink path with _ -> ());
  (try Unix.unlink (path ^ "-wal") with _ -> ());

  let writer_wall = !writer_done -. !writer_start in
  let reader_wall = !reader_done in
  Printf.printf
    "slow-read yield bench: read_delay=%.0fms wal_delay=%.0fms \
     commits=%d reads=%d seed_rows=%d\n%!"
    (read_delay *. 1000.0) (wal_delay *. 1000.0) n_commits n_reads seed_rows;
  Printf.printf "  writer wall: %.3fs (bound: %.3fs)\n" writer_wall max_writer_s;
  Printf.printf "  reader wall: %.3fs\n%!" reader_wall;

  (* Acceptance: writer wall is bounded — if the reader had monopolised
     the scheduler, the writer would have been parked behind the reader's
     [n_reads * seed_rows * read_delay] of slow I/O, dwarfing its own
     [n_commits * wal_delay] of work.  With #158's yield, the writer
     runs concurrently and finishes within [max_writer_s]. *)
  Alcotest.(check bool)
    (Printf.sprintf
       "writer wall (%.3fs) bounded by %.3fs (reader yields during slow I/O)"
       writer_wall max_writer_s)
    true (writer_wall <= max_writer_s);
  Lwt.return_unit

let () =
  Alcotest.run "bench_slow_read_yield" [
    "slow-read", [
      Alcotest.test_case "writer makes progress during reader I/O" `Slow
        (fun () -> run (main ()));
    ];
  ]
```

### Step 4.3 — Build and run

- [ ] **Build:**

```bash
podman run --rm -v "$(pwd):/workspace:Z" -w /workspace sqlocaml-dev dune build test/bench_slow_read_yield.exe 2>&1 | tail -10
```

Expected: clean build. If `S.put`, `S.cursor_open`, etc. names don't match, consult `lib/store/store.mli` and adjust — the bench is the only consumer here.

- [ ] **Run with defaults:**

```bash
podman run --rm -v "$(pwd):/workspace:Z" -w /workspace sqlocaml-dev \
  ./_build/default/test/bench_slow_read_yield.exe 2>&1 | tail -10
```

Expected: prints writer/reader wall times, asserts `writer wall <= 0.5s`, exits 0.

### Step 4.4 — Negative-control verification

This is the part that gives the bench teeth. Temporarily revert `lib/block/unix_file.ml` to use `Lwt.return (Unix.read ...)` (pre-#158 sync wrapping) and confirm the bench fails. Then revert the revert.

- [ ] **Stash the current `lib/block/unix_file.ml`:**

```bash
cp lib/block/unix_file.ml /tmp/unix_file.ml.bak
```

- [ ] **Manually edit `lib/block/unix_file.ml`** — replace one `let* n = Lwt_unix.pread ...` call with the pre-#158 pattern, e.g.:

```ocaml
let n = Unix.read (Lwt_unix.unix_file_descr fd) tmp o r in
Lwt.return n
```

(One regression site is enough to demonstrate; if the bench still passes with one such revert, the bench's `max_writer_s` is too generous.)

- [ ] **Rebuild and rerun the bench:**

```bash
podman run --rm -v "$(pwd):/workspace:Z" -w /workspace sqlocaml-dev \
  bash -c "dune build && ./_build/default/test/bench_slow_read_yield.exe" 2>&1 | tail -15
```

Expected: bench FAILS with `writer wall (X.XXs) bounded by 0.500s` — X exceeds 0.5.

- [ ] **Restore the file:**

```bash
cp /tmp/unix_file.ml.bak lib/block/unix_file.ml
rm /tmp/unix_file.ml.bak
```

- [ ] **Rebuild and confirm the bench passes again:**

```bash
podman run --rm -v "$(pwd):/workspace:Z" -w /workspace sqlocaml-dev \
  bash -c "dune build && ./_build/default/test/bench_slow_read_yield.exe" 2>&1 | tail -5
```

Expected: pass.

If the negative control did not trigger a failure, tighten `SQLOCAML_BENCH_MAX_WRITER_S` (default `0.5`) until it does, then commit the tightened default.

### Step 4.5 — Commit

- [ ] **Commit:**

```bash
git add test/bench_slow_read_yield.ml test/dune
git commit -m "$(cat <<'EOF'
test(#162): slow-read I/O yield bench (#158 regression detector)

Opens a fresh WAL store per run with a per-read Lwt_unix.sleep injected
in front of the BLOCK read_page callback, then runs a writer + reader
concurrently and asserts the writer's wall time stays bounded.

Unlike bench_wal_fsync_overlap, this bench's working set is forced
through real BLOCK I/O on every iteration (cold Pager cache), so a
revert of #158 to synchronous Unix.read in Unix_file would starve the
writer behind the reader's stall and fail the assertion.  Verified via
negative-control revert.

Closes #162.

Co-Authored-By: Claude Opus 4.7 <noreply@anthropic.com>
EOF
)"
```

---

## Task 5 — Full regression sweep + close issues

**Why:** Belt-and-braces.

### Step 5.1 — Full runtest

- [ ] **Run the full suite:**

```bash
podman run --rm -v "$(pwd):/workspace:Z" -w /workspace sqlocaml-dev dune runtest --force 2>&1 | tail -50
```

Expected: zero failures across all executables.

### Step 5.2 — Push and close

- [ ] **Push:**

```bash
git push origin main
```

- [ ] **Close issues:**

```bash
~/.local/bin/forgejo issue edit tej/sqlite_ocaml_port 161 --state=closed
~/.local/bin/forgejo issue edit tej/sqlite_ocaml_port 162 --state=closed
```

- [ ] **Add a comment summarising the deliverables on each closed issue:**

```bash
~/.local/bin/forgejo issue comment tej/sqlite_ocaml_port 161 --body "Closed in phase 41. Added \`*_lwt\` helper family in \`test/test_e2e.ml\` and migrated every site that lived inside an outer \`Lwt_main.run\`. Verified by full runtest."

~/.local/bin/forgejo issue comment tej/sqlite_ocaml_port 162 --body "Closed in phase 41. Added \`test/bench_slow_read_yield.ml\` — injects per-read latency at the BLOCK callback, fresh cold-cache DB per iteration. Verified the bench *fails* under a synchronous revert of \`lib/block/unix_file.ml\` (#158 regression simulation) and passes under main."
```

---

## Out of scope (for this phase)

- Migrating `test_store.ml` / `test_wal.ml` etc. away from any analogous nested-`run` pattern. If a future phase exposes the same footgun elsewhere, file a follow-up.
- Eio-or-similar runtime swap.
- Fixing #159 (Pager cache eviction is snapshot-unaware). Phase 42 handles it.
- Tuning the Lwt worker-thread pool.

## Acceptance summary

- [ ] `test/test_e2e.ml` has no nested `Lwt_main.run` in functions that call `Db.*` (greppable signature passes).
- [ ] `*_lwt` helpers exist and are used at every at-risk site.
- [ ] `test/bench_slow_read_yield.exe` builds, passes under main, and fails under a synchronous `Unix_file` revert.
- [ ] Full `dune runtest` passes.
- [ ] #161 and #162 closed with completion comments.
