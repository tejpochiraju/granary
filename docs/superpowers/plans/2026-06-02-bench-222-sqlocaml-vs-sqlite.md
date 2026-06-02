# Benchmark sqlocaml vs SQLite (#222) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build an in-process cross-engine benchmark (sqlocaml vs reference C SQLite), run it on the HDD host (`here`) and the NVMe host (`otp-prod-1`), and publish a results table + CPU-vs-I/O verdict that gates the multicore epic #156.

**Architecture:** One synchronous timing harness (`test/bench_compare.ml`) drives two engines behind a common `ENGINE` module type — `Sqlocaml` (via `Db`/`Sqlocaml_unix`) and `Ref_sqlite` (via the `sqlite3` opam bindings, in-process, prepared statements). Each workload is timed with wall-clock (`Unix.gettimeofday`) and CPU time (`Unix.times`: utime+stime); the CPU/wall ratio is the CPU-bound/I/O-bound signal. A bench-only podman image (`sqlocaml-bench`) layers `libsqlite3-dev` + `opam install sqlite3` on the existing `sqlocaml-dev` image, pinning one SQLite version across both hosts. A runner script bind-mounts a real host directory so file I/O hits the actual disk (HDD vs NVMe), not the container overlay.

**Tech Stack:** OCaml 5.4.1, Lwt, Alcotest (smoke harness), `sqlite3` opam package (≥5.x), podman, bash. Build/run inside `sqlocaml-bench` (never call `dune` on the host).

---

## Key facts the implementer needs

- **Never call `dune` directly on the host.** All build/run/test goes through podman, e.g.
  `podman run --rm -v "$PWD":/workspace:Z -w /workspace sqlocaml-bench dune build test/bench_compare.exe`.
  The worktree root must be world-writable for the container: `chmod 777 .` if you hit permission errors.
- **sqlocaml open APIs** (`lib/unix/sqlocaml_unix.mli`, `lib/db/db.mli`):
  - Plaintext WAL → `Db.t`: `Sqlocaml_unix.open_file_wal ~path () : (Db.t, Db.error) result Lwt.t`
  - Encrypted WAL → only the low-level store takes a key:
    `Sqlocaml_unix.Store.open_file_wal ~key ~path () : (Store.t, _) result Lwt.t`,
    then wrap: `Db.of_store ~file_path:path store : Db.t Lwt.t`.
  - SQL: `Db.execute db sql`, `Db.query db sql : (row Lwt_stream.t, _) result Lwt.t`,
    `Db.prepare`, `Db.run stmt ~params`, `Db.iter stmt ~params`, `Db.close db`.
  - `row = value array`; `value = V_int of int64 | V_text of string | V_null | V_real of float | V_blob of bytes`.
  - `Lwt_main.run` drives an Lwt promise to completion.
- **Page-cache parity knob:** sqlocaml reads `SQLOCAML_PAGE_CACHE` (pages; default `1024`, see `lib/storage/pager.ml:17`); page size default 4096B. For SQLite set `PRAGMA page_size=4096; PRAGMA cache_size=<same N>;` (positive = pages).
- **`sqlite3` opam package** confirmed installable in the dev image (versions to 5.4.1). Module is `Sqlite3`. The binding names used below target ≥5.x; the Task 3 build validates them.
- **Hosts:** `here` = 8 cores, HDD (HGST 7200rpm). `otp-prod-1` = 12 cores, NVMe (Samsung); reachable via `ssh otp-prod-1` (verified). Neither has opam/ocaml natively; both have podman + `sqlite3` CLI.

## File structure

- **Create** `containers/bench.Containerfile` — bench image (sqlocaml-dev + libsqlite3-dev + sqlite3 opam pkg).
- **Create** `test/bench_compare.ml` — the cross-engine bench (engines, harness, CSV, smoke self-check).
- **Modify** `test/dune` — add the `bench_compare` executable target (NOT a `(test ...)` stanza, so it never runs in the CI gate; it's an `(executable ...)` we invoke explicitly).
- **Create** `scripts/bench222.sh` — build-image-if-absent + podman-run-with-bind-mount + metadata capture + CSV out.
- **Create** `bench/results/here.csv`, `bench/results/otp-prod-1.csv` (+ `.meta` sidecars) — produced by runs.
- **Create** `docs/benchmarks/2026-06-02-bench-222-results.md` — tables + CPU-vs-I/O verdict.
- **Modify** `README.md` — add a `## Benchmarks` section.

---

## Task 1: Bench image (sqlocaml-dev + sqlite3 bindings)

**Files:**
- Create: `containers/bench.Containerfile`

- [ ] **Step 1: Write the Containerfile**

```dockerfile
# Bench image for #222: adds in-process SQLite bindings to the dev toolchain.
# Layering on sqlocaml-dev pins ONE libsqlite3 version across both hosts (fair).
FROM localhost/sqlocaml-dev:latest

USER root
RUN apt-get update \
 && apt-get install -y --no-install-recommends libsqlite3-dev \
 && rm -rf /var/lib/apt/lists/*

# opam switch is set up in the base image; install the sqlite3 bindings into it.
# OPAMYES makes this non-interactive; pin nothing — take what the base repo offers.
RUN opam install -y sqlite3 \
 && opam clean -y || true
```

- [ ] **Step 2: Build the image**

Run:
```bash
cd /home/tej/projects/sqlite_ocaml_port/.claude/worktrees/feat+214-215-encryption-copy-rekey
podman build -t sqlocaml-bench -f containers/bench.Containerfile .
```
Expected: build completes; final line `Successfully tagged localhost/sqlocaml-bench:latest` (or the image id). If `opam install` needs the base image's opam user, the build may need `USER opam` before the opam step — check the base image's default user with `podman run --rm localhost/sqlocaml-dev whoami` and adjust the Containerfile's `USER` lines accordingly.

- [ ] **Step 3: Verify the bindings are linkable in-image**

Run:
```bash
podman run --rm localhost/sqlocaml-bench bash -lc 'ocamlfind list 2>/dev/null | grep -i sqlite3; sqlite3 --version'
```
Expected: a line like `sqlite3 (version: 5.x)` from ocamlfind, and a `3.45.x` CLI version. Record the CLI version — it is the reference SQLite version for the published results.

- [ ] **Step 4: Commit**

```bash
git add containers/bench.Containerfile
git commit -m "build(#222): bench image — sqlocaml-dev + in-process sqlite3 bindings"
```

---

## Task 2: Bench harness + Sqlocaml engine + CSV

**Files:**
- Create: `test/bench_compare.ml`
- Modify: `test/dune` (add executable target)

- [ ] **Step 1: Write `test/bench_compare.ml` (harness + Sqlocaml engine + main, no SQLite yet)**

```ocaml
(** #222 — cross-engine benchmark: sqlocaml vs reference C SQLite.

    Synchronous timing harness over a common {!ENGINE} interface.  Each workload
    is timed with wall-clock ([Unix.gettimeofday]) and CPU time ([Unix.times]:
    utime+stime); [cpu/wall] is the CPU-bound vs I/O-bound signal for #156.

    NOT a CI pass/fail gate — it is a measurement tool.  Correctness is guarded
    by [SQLOCAML_BENCH_SMOKE=1] (cross-engine result equality on a tiny dataset).

    Output: CSV to stdout; human summary to stderr.  Tunables via env:
      SQLOCAML_BENCH_ROWS    dataset rows         (default 10000)
      SQLOCAML_BENCH_OPS     point-lookup ops     (default 5000)
      SQLOCAML_BENCH_SCANS   scan/agg repeats     (default 50)
      SQLOCAML_BENCH_COMMITS single-row txns      (default 200)
      SQLOCAML_BENCH_REPEATS timed repeats        (default 5; median+best reported)
      SQLOCAML_BENCH_PAGE_CACHE  page-cache pages (default 1024; mirrored to both engines)
      SQLOCAML_BENCH_HOST    host label for CSV   (default from Unix.gethostname) *)

let run = Lwt_main.run

let env_int key default =
  match Sys.getenv_opt key with
  | Some s -> (try int_of_string s with _ -> default)
  | None -> default

let rows_n   = env_int "SQLOCAML_BENCH_ROWS" 10000
let ops_n    = env_int "SQLOCAML_BENCH_OPS" 5000
let scans_n  = env_int "SQLOCAML_BENCH_SCANS" 50
let commits_n = env_int "SQLOCAML_BENCH_COMMITS" 200
let repeats_n = env_int "SQLOCAML_BENCH_REPEATS" 5
let page_cache = env_int "SQLOCAML_BENCH_PAGE_CACHE" 1024
let host_label =
  match Sys.getenv_opt "SQLOCAML_BENCH_HOST" with
  | Some h when h <> "" -> h
  | _ -> (try Unix.gethostname () with _ -> "unknown")

(* Deterministic pk sequence for point lookups — no wall-clock/random seed so
   both engines hit the SAME keys in the SAME order. Simple LCG mod rows. *)
let lookup_keys ~rows ~n =
  let a = Array.make n 0 in
  let x = ref 1234567 in
  for i = 0 to n - 1 do
    x := ((!x * 1103515245) + 12345) land 0x3FFFFFFF;
    a.(i) <- !x mod rows
  done;
  a

(* ── Common engine interface (synchronous from the harness's view) ────────── *)
module type ENGINE = sig
  type t
  val name : string
  (* [key=Some k] opens encrypted (32 bytes); [None] plaintext. May raise if the
     engine does not support encryption (the reference engine does not). *)
  val open_db : dir:string -> key:string option -> t
  val seed : t -> rows:int -> unit
  (* Each workload runs its full loop internally and returns ops performed. *)
  val w_point_lookup : t -> keys:int array -> int
  val w_scan_agg : t -> repeats:int -> int           (* returns rows summed-over *)
  val w_insert_one : t -> n:int -> base:int -> int    (* n autocommit inserts *)
  val w_insert_batch : t -> rows:int -> base:int -> int (* one txn of [rows] inserts *)
  val w_commit_n : t -> n:int -> base:int -> int      (* n single-row txns (fsync each) *)
  val close : t -> unit
end

(* ── sqlocaml engine ──────────────────────────────────────────────────────── *)
module Sqlocaml : ENGINE = struct
  open Sqlocaml
  type t = { db : Db.t }

  let name = "sqlocaml"

  let unwrap = function Ok v -> v | Error e -> Alcotest.failf "sqlocaml: %a" Db.pp_error e

  let open_db ~dir ~key =
    let path = Filename.concat dir "bench.db" in
    (try Unix.unlink path with _ -> ());
    (try Unix.unlink (path ^ "-wal") with _ -> ());
    let db =
      match key with
      | None -> unwrap (run (Sqlocaml_unix.open_file_wal ~path ()))
      | Some k ->
        (match run (Sqlocaml_unix.Store.open_file_wal ~key:k ~path ()) with
         | Error _ -> Alcotest.fail "sqlocaml: encrypted open failed"
         | Ok store -> run (Db.of_store ~file_path:path store))
    in
    { db }

  let exec t sql = ignore (unwrap (run (Db.execute t.db sql)))

  let seed t ~rows =
    exec t "CREATE TABLE t (id INTEGER PRIMARY KEY, k INTEGER, payload TEXT)";
    exec t "BEGIN";
    for i = 0 to rows - 1 do
      exec t (Printf.sprintf
        "INSERT INTO t (id, k, payload) VALUES (%d, %d, 'payload-row-%d')"
        i (i * 7 mod rows) i)
    done;
    exec t "COMMIT"

  let w_point_lookup t ~keys =
    run begin
      let open Lwt.Syntax in
      let* stmt = Lwt.map unwrap (Db.prepare t.db "SELECT payload FROM t WHERE id = ?") in
      let* () =
        Lwt_list.iter_s
          (fun pk ->
            let* stream = Lwt.map unwrap (Db.iter stmt ~params:[ Db.V_int (Int64.of_int pk) ]) in
            let* _ = Lwt_stream.to_list stream in
            Lwt.return_unit)
          (Array.to_list keys)
      in
      let* () = Db.finalize stmt in
      Lwt.return (Array.length keys)
    end

  let w_scan_agg t ~repeats =
    let n = ref 0 in
    for _ = 1 to repeats do
      run begin
        let open Lwt.Syntax in
        let* stream = Lwt.map unwrap (Db.query t.db "SELECT COUNT(*), SUM(k) FROM t") in
        let* rows = Lwt_stream.to_list stream in
        n := !n + List.length rows;
        Lwt.return_unit
      end
    done;
    !n

  let w_insert_one t ~n ~base =
    for i = 0 to n - 1 do
      let id = base + i in
      exec t (Printf.sprintf
        "INSERT INTO t (id, k, payload) VALUES (%d, %d, 'ins-%d')" id id id)
    done;
    n

  let w_insert_batch t ~rows ~base =
    exec t "BEGIN";
    for i = 0 to rows - 1 do
      let id = base + i in
      exec t (Printf.sprintf
        "INSERT INTO t (id, k, payload) VALUES (%d, %d, 'batch-%d')" id id id)
    done;
    exec t "COMMIT";
    rows

  let w_commit_n t ~n ~base =
    for i = 0 to n - 1 do
      let id = base + i in
      exec t "BEGIN";
      exec t (Printf.sprintf
        "INSERT INTO t (id, k, payload) VALUES (%d, %d, 'commit-%d')" id id id);
      exec t "COMMIT"
    done;
    n

  let close t = run (Db.close t.db)
end

(* ── timing harness ───────────────────────────────────────────────────────── *)
type sample = { ops : int; wall : float; cpu : float }

let cpu_now () =
  let tm = Unix.times () in
  tm.Unix.tms_utime +. tm.Unix.tms_stime

let measure ~repeats (f : unit -> int) : sample =
  ignore (f ());                                  (* warmup, discarded *)
  let best = ref None in
  for _ = 1 to repeats do
    let c0 = cpu_now () and t0 = Unix.gettimeofday () in
    let ops = f () in
    let wall = Unix.gettimeofday () -. t0 in
    let cpu = cpu_now () -. c0 in
    let s = { ops; wall; cpu } in
    (match !best with
     | Some b when b.wall <= wall -> ()
     | _ -> best := Some s)
  done;
  match !best with Some s -> s | None -> { ops = 0; wall = 0.; cpu = 0. }

(* ── CSV emission ─────────────────────────────────────────────────────────── *)
let csv_header =
  "host,engine,workload,variant,rows,ops,wall_s,cpu_s,cpu_wall_ratio,ops_per_s"

let emit_row ~engine ~workload ~variant ~rows (s : sample) =
  let ratio = if s.wall > 0. then s.cpu /. s.wall else 0. in
  let ops_s = if s.wall > 0. then float_of_int s.ops /. s.wall else 0. in
  Printf.printf "%s,%s,%s,%s,%d,%d,%.6f,%.6f,%.3f,%.1f\n%!"
    host_label engine workload variant rows s.ops s.wall s.cpu ratio ops_s

(* ── workload driver for one engine ───────────────────────────────────────── *)
let run_engine (module E : ENGINE) ~variant ~key =
  let dir = Filename.temp_file "bench222-" "" in
  Sys.remove dir; Unix.mkdir dir 0o755;
  let cleanup () =
    Array.iter (fun f -> try Sys.remove (Filename.concat dir f) with _ -> ())
      (try Sys.readdir dir with _ -> [||]);
    (try Unix.rmdir dir with _ -> ())
  in
  Fun.protect ~finally:cleanup (fun () ->
    let t = E.open_db ~dir ~key in
    E.seed t ~rows:rows_n;
    let keys = lookup_keys ~rows:rows_n ~n:ops_n in
    emit_row ~engine:E.name ~workload:"point_lookup" ~variant ~rows:rows_n
      (measure ~repeats:repeats_n (fun () -> E.w_point_lookup t ~keys));
    emit_row ~engine:E.name ~workload:"scan_agg" ~variant ~rows:rows_n
      (measure ~repeats:repeats_n (fun () -> E.w_scan_agg t ~repeats:scans_n));
    (* write workloads use disjoint id ranges per repeat to avoid PK clashes;
       measured single-shot (repeats=1) since they mutate state. *)
    emit_row ~engine:E.name ~workload:"insert_one" ~variant ~rows:ops_n
      (measure ~repeats:1 (fun () -> E.w_insert_one t ~n:ops_n ~base:(rows_n)));
    emit_row ~engine:E.name ~workload:"insert_batch" ~variant ~rows:rows_n
      (measure ~repeats:1 (fun () -> E.w_insert_batch t ~rows:rows_n ~base:(rows_n + ops_n)));
    emit_row ~engine:E.name ~workload:"commit_n" ~variant ~rows:commits_n
      (measure ~repeats:1 (fun () -> E.w_commit_n t ~n:commits_n ~base:(2 * rows_n + ops_n)));
    E.close t)

(* ── main ─────────────────────────────────────────────────────────────────── *)
let () =
  (* pin sqlocaml page cache for parity (SQLite mirrored in Ref_sqlite, Task 3) *)
  Unix.putenv "SQLOCAML_PAGE_CACHE" (string_of_int page_cache);
  print_string (csv_header ^ "\n");
  run_engine (module Sqlocaml) ~variant:"plaintext" ~key:None;
  run_engine (module Sqlocaml) ~variant:"encrypted" ~key:(Some (String.make 32 'K'))
```

- [ ] **Step 2: Add the executable target to `test/dune`**

Append this stanza to `test/dune` (it is an `(executable ...)`, NOT `(test ...)`, so `dune runtest` / the CI gate never invokes it):

```
(executable
 (name bench_compare)
 (modules bench_compare)
 (libraries sqlocaml sqlocaml.store sqlocaml.unix alcotest lwt.unix unix))
```

Note: if `test/dune` uses a single shared `(modules ...)` set or a wildcard, you must instead add `bench_compare` to the existing module exclusion lists so it compiles only under this executable. Inspect `test/dune` first; most stanzas there are per-`(name ...)` so a standalone `(executable ...)` with an explicit `(modules bench_compare)` is the clean addition. If dune complains that `bench_compare` is claimed by multiple stanzas, add `(modules (:standard \ bench_compare))` to the offending stanza.

- [ ] **Step 3: Build it in the bench image**

Run:
```bash
chmod 777 .
podman run --rm -v "$PWD":/workspace:Z -w /workspace localhost/sqlocaml-bench \
  dune build test/bench_compare.exe 2>&1 | tail -20
```
Expected: clean build, no errors. Fix any API mismatches (e.g. `Lwt.Syntax`/`Lwt_list` need `lwt` in libs — already pulled via `lwt.unix`).

- [ ] **Step 4: Smoke-run sqlocaml-only with a tiny dataset**

Run:
```bash
podman run --rm -e SQLOCAML_BENCH_ROWS=200 -e SQLOCAML_BENCH_OPS=100 \
  -e SQLOCAML_BENCH_COMMITS=20 -e SQLOCAML_BENCH_REPEATS=2 \
  -v "$PWD":/workspace:Z -w /workspace localhost/sqlocaml-bench \
  dune exec test/bench_compare.exe 2>/dev/null
```
Expected: CSV header + 10 rows (5 workloads × {plaintext, encrypted}), e.g.
`<host>,sqlocaml,point_lookup,plaintext,200,100,0.00...,0.00...,0.9xx,...`.
The `encrypted` rows for write workloads should also appear (we only restrict the cross-engine *comparison* to reads; sqlocaml still runs all workloads encrypted — that's fine and free extra data).

- [ ] **Step 5: Commit**

```bash
git add test/bench_compare.ml test/dune
git commit -m "feat(#222): bench_compare harness + sqlocaml engine (CSV, cpu-vs-wall)"
```

---

## Task 3: Reference SQLite engine (in-process bindings)

**Files:**
- Modify: `test/bench_compare.ml` (add `Ref_sqlite`, wire into main)

- [ ] **Step 1: Add the `Ref_sqlite` module** (insert after the `Sqlocaml` module, before the timing harness)

```ocaml
(* ── reference C SQLite engine (in-process bindings) ──────────────────────── *)
module Ref_sqlite : ENGINE = struct
  type t = { db : Sqlite3.db }
  let name = "sqlite"

  let ok rc =
    match rc with
    | Sqlite3.Rc.OK | Sqlite3.Rc.DONE | Sqlite3.Rc.ROW -> ()
    | r -> failwith ("sqlite3: " ^ Sqlite3.Rc.to_string r)

  let exec t sql = ok (Sqlite3.exec t.db sql)

  let open_db ~dir ~key =
    (match key with
     | Some _ -> failwith "Ref_sqlite: encrypted reference not supported (no sqlcipher)"
     | None -> ());
    let path = Filename.concat dir "ref.db" in
    (try Unix.unlink path with _ -> ());
    let db = Sqlite3.db_open path in
    let t = { db } in
    (* parity: same page size + cache page count as sqlocaml; WAL like sqlocaml. *)
    exec t "PRAGMA page_size=4096";
    exec t (Printf.sprintf "PRAGMA cache_size=%d" page_cache);
    exec t "PRAGMA journal_mode=WAL";
    exec t "PRAGMA synchronous=FULL";  (* match sqlocaml's fsync-per-commit durability *)
    t

  let seed t ~rows =
    exec t "CREATE TABLE t (id INTEGER PRIMARY KEY, k INTEGER, payload TEXT)";
    exec t "BEGIN";
    let stmt = Sqlite3.prepare t.db "INSERT INTO t (id,k,payload) VALUES (?,?,?)" in
    for i = 0 to rows - 1 do
      ok (Sqlite3.reset stmt);
      ok (Sqlite3.bind_int64 stmt 1 (Int64.of_int i));
      ok (Sqlite3.bind_int64 stmt 2 (Int64.of_int (i * 7 mod rows)));
      ok (Sqlite3.bind_text stmt 3 (Printf.sprintf "payload-row-%d" i));
      ok (Sqlite3.step stmt)
    done;
    ok (Sqlite3.finalize stmt);
    exec t "COMMIT"

  let drain stmt =
    let n = ref 0 in
    let rec loop () =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> incr n; loop ()
      | Sqlite3.Rc.DONE -> ()
      | r -> failwith ("sqlite3 step: " ^ Sqlite3.Rc.to_string r)
    in
    loop (); !n

  let w_point_lookup t ~keys =
    let stmt = Sqlite3.prepare t.db "SELECT payload FROM t WHERE id = ?" in
    Array.iter (fun pk ->
      ok (Sqlite3.reset stmt);
      ok (Sqlite3.bind_int64 stmt 1 (Int64.of_int pk));
      ignore (drain stmt)) keys;
    ok (Sqlite3.finalize stmt);
    Array.length keys

  let w_scan_agg t ~repeats =
    let n = ref 0 in
    for _ = 1 to repeats do
      let stmt = Sqlite3.prepare t.db "SELECT COUNT(*), SUM(k) FROM t" in
      n := !n + drain stmt;
      ok (Sqlite3.finalize stmt)
    done;
    !n

  let w_insert_one t ~n ~base =
    let stmt = Sqlite3.prepare t.db "INSERT INTO t (id,k,payload) VALUES (?,?,?)" in
    for i = 0 to n - 1 do
      let id = base + i in
      ok (Sqlite3.reset stmt);
      ok (Sqlite3.bind_int64 stmt 1 (Int64.of_int id));
      ok (Sqlite3.bind_int64 stmt 2 (Int64.of_int id));
      ok (Sqlite3.bind_text stmt 3 (Printf.sprintf "ins-%d" id));
      ok (Sqlite3.step stmt)
    done;
    ok (Sqlite3.finalize stmt);
    n

  let w_insert_batch t ~rows ~base =
    exec t "BEGIN";
    let stmt = Sqlite3.prepare t.db "INSERT INTO t (id,k,payload) VALUES (?,?,?)" in
    for i = 0 to rows - 1 do
      let id = base + i in
      ok (Sqlite3.reset stmt);
      ok (Sqlite3.bind_int64 stmt 1 (Int64.of_int id));
      ok (Sqlite3.bind_int64 stmt 2 (Int64.of_int id));
      ok (Sqlite3.bind_text stmt 3 (Printf.sprintf "batch-%d" id));
      ok (Sqlite3.step stmt)
    done;
    ok (Sqlite3.finalize stmt);
    exec t "COMMIT";
    rows

  let w_commit_n t ~n ~base =
    let stmt = Sqlite3.prepare t.db "INSERT INTO t (id,k,payload) VALUES (?,?,?)" in
    for i = 0 to n - 1 do
      let id = base + i in
      exec t "BEGIN";
      ok (Sqlite3.reset stmt);
      ok (Sqlite3.bind_int64 stmt 1 (Int64.of_int id));
      ok (Sqlite3.bind_int64 stmt 2 (Int64.of_int id));
      ok (Sqlite3.bind_text stmt 3 (Printf.sprintf "commit-%d" id));
      ok (Sqlite3.step stmt);
      exec t "COMMIT"
    done;
    ok (Sqlite3.finalize stmt);
    n

  let close t = ignore (Sqlite3.db_close t.db)
end
```

- [ ] **Step 2: Wire SQLite into `main` and add `sqlite3` to the dune libs**

In `test/bench_compare.ml` `main`, add the reference run as the FIRST engine (so its CSV rows come first):

```ocaml
  print_string (csv_header ^ "\n");
  run_engine (module Ref_sqlite) ~variant:"plaintext" ~key:None;
  run_engine (module Sqlocaml) ~variant:"plaintext" ~key:None;
  run_engine (module Sqlocaml) ~variant:"encrypted" ~key:(Some (String.make 32 'K'))
```

In `test/dune`, add `sqlite3` to the `bench_compare` executable libraries:
```
 (libraries sqlocaml sqlocaml.store sqlocaml.unix sqlite3 alcotest lwt.unix unix))
```

- [ ] **Step 3: Build (this validates the `Sqlite3` binding names)**

Run:
```bash
podman run --rm -v "$PWD":/workspace:Z -w /workspace localhost/sqlocaml-bench \
  dune build test/bench_compare.exe 2>&1 | tail -30
```
Expected: clean build. If a binding name differs in the installed version (e.g. `bind_int64`, `column_int64`, `Rc.to_string`), check the API with
`podman run --rm localhost/sqlocaml-bench bash -lc 'ocamlfind list | grep sqlite3'`
then open the package's `.mli`:
`podman run --rm localhost/sqlocaml-bench bash -lc 'cat $(ocamlfind query sqlite3)/sqlite3.mli' | grep -nE "val (bind|column|step|prepare|reset|finalize|exec|db_open|db_close)"`
and adjust the calls to match. Fix and rebuild until green.

- [ ] **Step 4: Smoke-run both engines, tiny dataset**

Run:
```bash
podman run --rm -e SQLOCAML_BENCH_ROWS=200 -e SQLOCAML_BENCH_OPS=100 \
  -e SQLOCAML_BENCH_COMMITS=20 -e SQLOCAML_BENCH_REPEATS=2 \
  -v "$PWD":/workspace:Z -w /workspace localhost/sqlocaml-bench \
  dune exec test/bench_compare.exe 2>/dev/null
```
Expected: header + 15 rows (sqlite plaintext ×5, sqlocaml plaintext ×5, sqlocaml encrypted ×5). All `ops_per_s` > 0, no failwith crash.

- [ ] **Step 5: Commit**

```bash
git add test/bench_compare.ml test/dune
git commit -m "feat(#222): in-process reference SQLite engine for bench_compare"
```

---

## Task 4: Cross-engine correctness smoke check

**Files:**
- Modify: `test/bench_compare.ml` (add smoke mode honoring `SQLOCAML_BENCH_SMOKE=1`)

- [ ] **Step 1: Add a results-equality query to both engines**

Add to the `ENGINE` signature (and both impls) a verification reader that returns a canonical fingerprint of the table — count, sum(k), and the payload of three fixed ids — as a string:

```ocaml
  val fingerprint : t -> string   (* "count=.. sum=.. p0=.. pmid=.. plast=.." *)
```

`Sqlocaml.fingerprint`:
```ocaml
  let scalar_str t sql =
    run begin
      let open Lwt.Syntax in
      let* s = Lwt.map unwrap (Db.query t.db sql) in
      let* rows = Lwt_stream.to_list s in
      match rows with
      | r :: _ when Array.length r > 0 ->
        Lwt.return (match r.(0) with
          | Db.V_int i -> Int64.to_string i
          | Db.V_text s -> s
          | Db.V_null -> "NULL"
          | Db.V_real f -> Printf.sprintf "%.0f" f
          | Db.V_blob _ -> "<blob>")
      | _ -> Lwt.return "NULL"
    end

  let fingerprint t =
    Printf.sprintf "count=%s sum=%s p0=%s"
      (scalar_str t "SELECT COUNT(*) FROM t")
      (scalar_str t "SELECT SUM(k) FROM t")
      (scalar_str t "SELECT payload FROM t WHERE id = 0")
```

`Ref_sqlite.fingerprint`:
```ocaml
  let scalar_str t sql =
    let stmt = Sqlite3.prepare t.db sql in
    let v = match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> (match Sqlite3.column stmt 0 with
          | Sqlite3.Data.NULL -> "NULL"
          | d -> Sqlite3.Data.to_string_coerce d)
      | _ -> "NULL" in
    ok (Sqlite3.finalize stmt); v

  let fingerprint t =
    Printf.sprintf "count=%s sum=%s p0=%s"
      (scalar_str t "SELECT COUNT(*) FROM t")
      (scalar_str t "SELECT SUM(k) FROM t")
      (scalar_str t "SELECT payload FROM t WHERE id = 0")
```

- [ ] **Step 2: Add the smoke entry point in `main`**

Replace the body of `let () =` so it branches on the env var:

```ocaml
let smoke () =
  let mk (module E : ENGINE) =
    let dir = Filename.temp_file "bench222smoke-" "" in
    Sys.remove dir; Unix.mkdir dir 0o755;
    let t = E.open_db ~dir ~key:None in
    E.seed t ~rows:100;
    let fp = E.fingerprint t in
    E.close t; (E.name, fp)
  in
  let (_, a) = mk (module Sqlocaml) in
  let (_, b) = mk (module Ref_sqlite) in
  if a <> b then (Printf.eprintf "SMOKE FAIL: sqlocaml=[%s] sqlite=[%s]\n%!" a b; exit 1);
  (* CSV header well-formed: exactly 10 comma-separated columns *)
  let cols = String.split_on_char ',' csv_header in
  if List.length cols <> 10 then (Printf.eprintf "SMOKE FAIL: csv header has %d cols\n%!" (List.length cols); exit 1);
  Printf.eprintf "SMOKE OK: engines agree [%s]; csv header %d cols\n%!" a (List.length cols)

let () =
  Unix.putenv "SQLOCAML_PAGE_CACHE" (string_of_int page_cache);
  match Sys.getenv_opt "SQLOCAML_BENCH_SMOKE" with
  | Some ("1" | "true") -> smoke ()
  | _ ->
    print_string (csv_header ^ "\n");
    run_engine (module Ref_sqlite) ~variant:"plaintext" ~key:None;
    run_engine (module Sqlocaml) ~variant:"plaintext" ~key:None;
    run_engine (module Sqlocaml) ~variant:"encrypted" ~key:(Some (String.make 32 'K'))
```

- [ ] **Step 3: Build, then run the smoke check**

Run:
```bash
podman run --rm -e SQLOCAML_BENCH_SMOKE=1 \
  -v "$PWD":/workspace:Z -w /workspace localhost/sqlocaml-bench \
  dune exec test/bench_compare.exe 2>&1 | tail -5
```
Expected: `SMOKE OK: engines agree [count=100 sum=... p0=payload-row-0]; csv header 10 cols`.
If `SMOKE FAIL` with differing fingerprints, the two engines disagree on results — STOP and debug (likely a seed or SUM type mismatch) before trusting any timing numbers. Use systematic-debugging.

- [ ] **Step 4: Commit**

```bash
git add test/bench_compare.ml
git commit -m "test(#222): cross-engine correctness smoke check for bench_compare"
```

---

## Task 5: Runner script (build image, bind-mount real disk, capture metadata)

**Files:**
- Create: `scripts/bench222.sh`

- [ ] **Step 1: Write `scripts/bench222.sh`**

```bash
#!/usr/bin/env bash
# #222 — run the cross-engine benchmark and emit a per-host CSV + metadata.
#
# File I/O is bind-mounted to a REAL host directory ($BENCH_DATA_DIR) so it hits
# the actual disk (HDD on 'here', NVMe on 'otp-prod-1'), not the podman overlay.
#
# Usage:  scripts/bench222.sh [host-label]
# Env:    SQLOCAML_BENCH_ROWS / _OPS / _SCANS / _COMMITS / _REPEATS / _PAGE_CACHE
set -euo pipefail

REPO="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO"

HOST_LABEL="${1:-$(hostname)}"
IMAGE="localhost/sqlocaml-bench"
RESULTS_DIR="$REPO/bench/results"
DATA_DIR="${BENCH_DATA_DIR:-$REPO/bench/data}"   # on the host's real fs
mkdir -p "$RESULTS_DIR" "$DATA_DIR"
chmod 777 "$REPO" "$DATA_DIR" || true

# Build the bench image if absent.
if ! podman image exists "$IMAGE"; then
  echo "building $IMAGE ..." >&2
  podman build -t sqlocaml-bench -f containers/bench.Containerfile .
fi

# Tunables (defaults chosen so HDD host exercises fsync/seek meaningfully).
ROWS="${SQLOCAML_BENCH_ROWS:-100000}"
OPS="${SQLOCAML_BENCH_OPS:-20000}"
SCANS="${SQLOCAML_BENCH_SCANS:-50}"
COMMITS="${SQLOCAML_BENCH_COMMITS:-500}"
REPEATS="${SQLOCAML_BENCH_REPEATS:-5}"
PAGE_CACHE="${SQLOCAML_BENCH_PAGE_CACHE:-1024}"

GIT_SHA="$(git rev-parse --short HEAD)"
SQLITE_VER="$(podman run --rm "$IMAGE" sqlite3 --version | awk '{print $1}')"

# Metadata sidecar.
{
  echo "host=$HOST_LABEL"
  echo "date=$(date -u +%FT%TZ)"
  echo "nproc=$(nproc)"
  echo "disk=$(lsblk -d -o NAME,ROTA,MODEL 2>/dev/null | awk 'NR>1 && $1!~"loop"{print $0}' | tr '\n' ';')"
  echo "sqlite_version=$SQLITE_VER"
  echo "sqlocaml_sha=$GIT_SHA"
  echo "rows=$ROWS ops=$OPS scans=$SCANS commits=$COMMITS repeats=$REPEATS page_cache=$PAGE_CACHE"
} > "$RESULTS_DIR/$HOST_LABEL.meta"

echo "running bench on $HOST_LABEL (sqlite=$SQLITE_VER sha=$GIT_SHA) ..." >&2

# Build first (separate from run so build noise stays off the CSV).
podman run --rm -v "$REPO":/workspace:Z -w /workspace "$IMAGE" \
  dune build test/bench_compare.exe

# Run: bind-mount the host data dir at /benchdata; point the bench's temp dir there.
podman run --rm \
  -e SQLOCAML_BENCH_HOST="$HOST_LABEL" \
  -e SQLOCAML_BENCH_ROWS="$ROWS" -e SQLOCAML_BENCH_OPS="$OPS" \
  -e SQLOCAML_BENCH_SCANS="$SCANS" -e SQLOCAML_BENCH_COMMITS="$COMMITS" \
  -e SQLOCAML_BENCH_REPEATS="$REPEATS" -e SQLOCAML_BENCH_PAGE_CACHE="$PAGE_CACHE" \
  -e TMPDIR=/benchdata \
  -v "$REPO":/workspace:Z -w /workspace \
  -v "$DATA_DIR":/benchdata:Z \
  "$IMAGE" \
  dune exec test/bench_compare.exe 2>/dev/null \
  > "$RESULTS_DIR/$HOST_LABEL.csv"

echo "wrote $RESULTS_DIR/$HOST_LABEL.csv" >&2
cat "$RESULTS_DIR/$HOST_LABEL.csv"
```

Note the `-e TMPDIR=/benchdata`: `Filename.temp_file` honors `TMPDIR`, so the bench's DB files land in the bind-mounted host dir (real disk), not the container overlay. This is the linchpin of valid I/O measurement.

- [ ] **Step 2: Make executable and smoke-run on `here` (small dataset)**

Run:
```bash
chmod +x scripts/bench222.sh
SQLOCAML_BENCH_ROWS=2000 SQLOCAML_BENCH_OPS=500 SQLOCAML_BENCH_COMMITS=50 \
  SQLOCAML_BENCH_REPEATS=2 scripts/bench222.sh here-smoke
```
Expected: a CSV with header + 15 rows printed; `bench/results/here-smoke.csv` and `here-smoke.meta` created. The `.meta` `disk=` line should mention `sda ... HGST` (HDD) on `here`.

- [ ] **Step 3: Add `bench/data/` to `.gitignore`** (DB files are transient; only CSV/meta are committed)

Append to `.gitignore` (create if absent):
```
bench/data/
```

- [ ] **Step 4: Commit**

```bash
git add scripts/bench222.sh .gitignore
git commit -m "build(#222): bench222.sh runner — real-disk bind mount + metadata"
```

---

## Task 6: Run the benchmark on both hosts

This is a measurement task (no code). Capture real numbers.

- [ ] **Step 1: Full run on `here` (HDD)**

Run:
```bash
scripts/bench222.sh here
```
Expected: `bench/results/here.csv` (15 rows) + `here.meta` (disk shows HGST/ROTA=1). Sanity-check: `cpu_wall_ratio` for write workloads (`insert_one`, `commit_n`) should be well below 1.0 on the HDD host (fsync wait); read workloads (`point_lookup`, `scan_agg`) closer to 1.0.

- [ ] **Step 2: Provision the bench image on `otp-prod-1`**

The image must exist on otp-prod-1. Build it there from the same base (the base `sqlocaml-dev` must be present; if not, note it and build/transfer per the project's usual image flow). Run:
```bash
ssh otp-prod-1 'cd <repo-on-otp-prod-1> && git fetch && git checkout '"$(git rev-parse HEAD)"' && podman image exists localhost/sqlocaml-bench || podman build -t sqlocaml-bench -f containers/bench.Containerfile .'
```
If the worktree/repo path on otp-prod-1 differs, adjust. If `sqlocaml-dev` is missing on otp-prod-1, STOP and ask the user how images are distributed there (the memory notes CI builds in `sqlocaml-dev` on otp-prod-1, so it likely exists under the runner user — you may need `sudo -u <runner>` or to build under your own user).

- [ ] **Step 3: Verify the SQLite version matches across hosts**

Run:
```bash
echo "here : $(podman run --rm localhost/sqlocaml-bench sqlite3 --version | awk '{print $1}')"
echo "otp  : $(ssh otp-prod-1 'podman run --rm localhost/sqlocaml-bench sqlite3 --version | awk "{print \$1}"')"
```
Expected: identical versions (both inherit the same base image). If they differ, record the discrepancy in the results doc as a caveat.

- [ ] **Step 4: Full run on `otp-prod-1` (NVMe)**

Run:
```bash
ssh otp-prod-1 'cd <repo-on-otp-prod-1> && scripts/bench222.sh otp-prod-1'
scp otp-prod-1:<repo-on-otp-prod-1>/bench/results/otp-prod-1.csv bench/results/
scp otp-prod-1:<repo-on-otp-prod-1>/bench/results/otp-prod-1.meta bench/results/
```
Expected: `bench/results/otp-prod-1.csv` (15 rows) + `.meta` (disk shows nvme/ROTA=0). On NVMe, write-workload `cpu_wall_ratio` should be notably HIGHER than on the HDD host (less fsync wait) — that delta is the I/O term.

- [ ] **Step 5: Commit the raw results**

```bash
git add bench/results/here.csv bench/results/here.meta \
        bench/results/otp-prod-1.csv bench/results/otp-prod-1.meta
git commit -m "bench(#222): raw cross-host results (here HDD, otp-prod-1 NVMe)"
```

---

## Task 7: Results doc + README section

**Files:**
- Create: `docs/benchmarks/2026-06-02-bench-222-results.md`
- Modify: `README.md`

- [ ] **Step 1: Tabulate both CSVs into the results doc**

Read `bench/results/here.csv` and `bench/results/otp-prod-1.csv`. Build the doc with these sections (fill EVERY number from the CSVs — no placeholders):

1. **Setup** — host table (from `.meta`): cores, disk, SQLite version, sqlocaml SHA, dataset params.
2. **Head-to-head (plaintext): SQLite vs sqlocaml** — one table per host, rows = the 5 workloads, columns = `ops/s sqlite | ops/s sqlocaml | ratio (sqlocaml/sqlite) | cpu/wall sqlite | cpu/wall sqlocaml`.
3. **Encryption tax (sqlocaml)** — read workloads: `plaintext ops/s | encrypted ops/s | overhead %`, per host.
4. **CPU-vs-I/O analysis** — for the read workloads, compare the sqlocaml↔SQLite gap on HDD vs NVMe. State the rule explicitly: gap constant across hosts ⇒ CPU-bound; gap widens on HDD ⇒ I/O-bound. Cite the `cpu_wall_ratio` columns.
5. **Concurrency (sqlocaml-only)** — run the existing benches and quote their output:
   ```bash
   podman run --rm -e SQLOCAML_BENCH_PARALLEL_MAX=1000000 \
     -v "$PWD":/workspace:Z -w /workspace localhost/sqlocaml-bench \
     dune exec test/bench_wal_reader_scaling.exe 2>&1 | grep serial
   ```
   on both hosts; tabulate serial/parallel/ratio.
6. **Verdict** — the one-paragraph CPU-bound-or-I/O-bound conclusion and the explicit **proceed / keep-deferred** recommendation for #156.

Template header for the doc:
```markdown
# Benchmark Results — sqlocaml vs SQLite (#222)

**Date:** 2026-06-02 · **sqlocaml:** <sha> · **SQLite:** <ver> · **Method:** [design doc](../superpowers/specs/2026-06-02-bench-222-sqlocaml-vs-sqlite-design.md)

In-process comparison (prepared statements both sides), WAL mode, fsync-per-commit
durability on both engines, identical dataset and page-cache size. Numbers are the
best of N repeats (warmup discarded). cpu/wall ≈ 1 ⇒ CPU-bound; cpu/wall ≪ 1 ⇒ I/O-wait-bound.

## Hosts
| host | cores | disk | SQLite | sqlocaml |
|------|-------|------|--------|----------|
| here | 8 | HDD HGST (7200rpm) | <ver> | <sha> |
| otp-prod-1 | 12 | NVMe Samsung | <ver> | <sha> |
...
```

- [ ] **Step 2: Sanity-review the verdict against the data**

Before writing the verdict prose, confirm it follows from the numbers: if `point_lookup`/`scan_agg` `cpu_wall_ratio` ≈ 1 on BOTH hosts and the sqlocaml/sqlite ratio is similar on both → CPU-bound → #156 (read-side multicore) is justified. If the HDD host shows much lower read ratios / a widening gap → I/O-bound → recommend keeping #156 deferred. Do not assert a verdict the numbers don't support.

- [ ] **Step 3: Add the README `## Benchmarks` section**

Append to `README.md` (after the existing sections), filling the headline numbers from the doc:

```markdown
## Benchmarks

In-process benchmarks vs reference C SQLite (same dataset, WAL, fsync-per-commit,
matched page cache) on two hosts — an HDD box and an NVMe box — to separate the
CPU term from the I/O term. Full method and tables:
[docs/benchmarks/2026-06-02-bench-222-results.md](docs/benchmarks/2026-06-02-bench-222-results.md).

**Headline (NVMe host, plaintext, best of N):**

| workload | sqlocaml vs SQLite |
|----------|--------------------|
| point lookup | <x>× |
| range scan / aggregate | <x>× |
| batched insert | <x>× |
| commit throughput | <x>× |

AES-256-GCM encryption-at-rest adds **<x>%** to the read path. **Verdict:** <CPU-/I-O-bound, one line>.

Reproduce: `scripts/bench222.sh` (builds the bench image and runs the suite; see the results doc for cross-host steps).
```

- [ ] **Step 4: Commit**

```bash
git add docs/benchmarks/2026-06-02-bench-222-results.md README.md
git commit -m "docs(#222): benchmark results table, CPU-vs-I/O verdict, README section"
```

---

## Task 8: Publish to the Forgejo wiki

**Files:** none in-repo (the wiki is a separate git repo: `<repo>.wiki.git`).

- [ ] **Step 1: Discover the wiki mechanism**

Run:
```bash
~/.local/bin/forgejo --help 2>&1 | grep -i wiki || true
git remote -v
```
The Forgejo wiki is a git repo at the same host as the code repo, suffixed `.wiki.git`. Derive its URL from the `origin` remote (replace `<repo>.git` → `<repo>.wiki.git`). If the `forgejo` CLI exposes a wiki subcommand, prefer it.

- [ ] **Step 2: Clone the wiki, add the page, push**

Run (substitute the derived URL):
```bash
WIKI_URL="$(git remote get-url origin | sed 's/\.git$/.wiki.git/')"
tmp="$(mktemp -d)"
git clone "$WIKI_URL" "$tmp/wiki" || { echo "wiki not initialized — create the first page via the Forgejo web UI, then retry"; exit 1; }
cp docs/benchmarks/2026-06-02-bench-222-results.md "$tmp/wiki/Benchmarks.md"
( cd "$tmp/wiki" && git add Benchmarks.md && git commit -m "Benchmarks (#222): sqlocaml vs SQLite, CPU-vs-I/O verdict" && git push )
rm -rf "$tmp"
```
Expected: push succeeds; the `Benchmarks` page is live on the wiki. If the wiki repo doesn't exist yet, create the first page through the web UI (which initializes `.wiki.git`), then rerun. Confirm with the user before pushing if the wiki URL is ambiguous.

- [ ] **Step 3: Cross-link** — add the wiki link to the README Benchmarks section (one line) and commit.

```bash
git add README.md
git commit -m "docs(#222): link wiki Benchmarks page from README"
```

---

## Task 9: Close out #222

- [ ] **Step 1: Post results summary to the issue**

Run (paste the verdict paragraph + headline table):
```bash
~/.local/bin/forgejo issue comment tej/sqlite_ocaml_port 222 --body "$(cat <<'EOF'
Benchmark complete. Results: docs/benchmarks/2026-06-02-bench-222-results.md + wiki Benchmarks page.

<headline table + one-paragraph CPU-vs-I/O verdict + #156 proceed/defer recommendation>
EOF
)"
```

- [ ] **Step 2: Update the #156 recommendation** — comment on #156 with the verdict (proceed or keep deferred) and a link to the results. Then close #222 if the user agrees the deliverable is met.

---

## Self-review notes (for the implementer)

- **Spec coverage:** workloads (point/scan/insert/batch/commit) → Tasks 2–3; encrypted read variant → Task 2 (`encrypted` rows) + Task 7 §3; in-process SQLite → Task 3; CPU-vs-wall → harness in Task 2; two hosts → Task 6; fairness (page cache, same dataset, WAL, fsync) → Tasks 2–3; deliverable table + verdict → Task 7; README + wiki → Tasks 7–8. Concurrency (sqlocaml-only) → Task 7 §5.
- **Out of scope (per spec):** sqlcipher reference (Task 3 raises on key), cross-engine concurrency, perf/flamegraph, tuning sqlocaml.
- **Risk — binding names:** the `Sqlite3` API names (`bind_int64`, `column`, `Data.to_string_coerce`, `Rc.to_string`) are validated at the Task 3 build; the step includes how to inspect the installed `.mli` if a name differs.
- **Risk — `Unix.times` resolution:** coarse (clock ticks). Workloads run for ≥ tens of ms aggregated, so the ratio is meaningful; if any workload's `wall` is < ~50ms, raise its op count (`SQLOCAML_BENCH_OPS`/`_SCANS`) so CPU time is measurable.
- **Risk — otp-prod-1 image provisioning:** Task 6 §2 stops and asks if `sqlocaml-dev` isn't available there.
```
