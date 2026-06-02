# Benchmark sqlocaml vs SQLite (#222) — Design

**Date:** 2026-06-02
**Issue:** #222 — Benchmark sqlocaml vs SQLite across hosts; CPU-vs-I/O verdict (Forgejo `tej/sqlite_ocaml_port`)
**Gates:** #156 (read-side multicore epic) — this benchmark decides proceed/defer.
**Base commit:** `92449ce`

## Goal

Measure where the pure-OCaml engine ("sqlocaml") stands against reference C SQLite, and
produce the empirical **CPU-bound vs I/O-bound verdict** that gates the multicore epic #156.

Two hosts with deliberately different I/O profiles isolate the I/O term from the CPU term:

| Host | Cores | Storage | Role |
|------|-------|---------|------|
| **here** | 8 | HDD (HGST HUS724020AL, 7200rpm) | fsync-limited |
| **otp-prod-1** | 12 | NVMe (Samsung MZVL2512) | fast I/O |

If sqlocaml's gap to SQLite is roughly constant across both hosts → **CPU-bound** (planner /
predicate eval / B+-tree compare / per-page AES-GCM decrypt) → #156 read-side multicore could
help. If the gap widens on the HDD host → **I/O/fsync-bound** → #156 stays deferred.

## Decisions (locked during brainstorming)

1. **Reference SQLite = in-process C bindings** (`sqlite3` opam package, prepared statements),
   not the CLI subprocess. The existing CLI baseline in `test_perf_bench.ml` is unfair for
   point-lookup latency (process spawn + text parse dominate). In-process is apples-to-apples.
2. **otp-prod-1 run = direct SSH** from `here` (verified: `ssh otp-prod-1` works).
3. **CPU-vs-I/O measurement = `getrusage` CPU-vs-wall**, not perf/flamegraph. Per workload,
   capture CPU time (utime+stime) alongside wall-clock. `cpu/wall ≈ 1` → CPU-bound;
   `wall ≫ cpu` → I/O-wait-bound. Portable, no privileges, runs identically in podman on both
   hosts. The cross-host (HDD vs NVMe) gap corroborates it.

## Comparison matrix

| Column | Engine / mode | Workloads covered |
|--------|---------------|-------------------|
| **plaintext SQLite** | reference C, in-process bindings | point lookup, range scan/agg, single insert, batch insert, commit |
| **plaintext sqlocaml** | head-to-head | same five (the "where we stand" number) |
| **encrypted sqlocaml** | sqlocaml + `~key` 32B AES-256-GCM | read paths only (point lookup, scan/agg) — isolates per-page decrypt overhead |
| **concurrent sqlocaml** | reader-scaling + fsync-overlap (Lwt) | **separate section**, sqlocaml-only |

Notes on the two judgment calls:
- **No sqlcipher** on either host, so the encrypted column is a *sqlocaml self-overhead*
  measurement (plaintext-vs-encrypted), which is exactly what #222 asks ("measure the decrypt
  overhead explicitly"). The reference engine stays plaintext.
- **Concurrency is sqlocaml-only.** Comparing Lwt-cooperative scheduling against C SQLite's
  threading model is apples-to-oranges; we report sqlocaml reader-scaling separately rather
  than manufacture a misleading cross-engine concurrency number.

## Architecture

### 1. Bench driver — `test/bench_compare.ml`

A single timing harness driving two engines behind one interface:

```ocaml
module type ENGINE = sig
  type t
  val name : string
  val open_db   : dir:string -> key:string option -> t Lwt.t
  val seed      : t -> rows:int -> unit Lwt.t
  val point_lookup : t -> pk:int -> unit Lwt.t      (* SELECT ... WHERE pk = ? *)
  val scan_agg     : t -> unit Lwt.t                (* SELECT COUNT/SUM ... ORDER BY *)
  val insert_one   : t -> row:int -> unit Lwt.t
  val insert_batch : t -> rows:int -> unit Lwt.t    (* one txn of N inserts *)
  val commit_n     : t -> n:int -> unit Lwt.t       (* N one-row txns, fsync each *)
  val close     : t -> unit Lwt.t
end
```

- `module Sqlocaml : ENGINE` — via `Db` high-level API + `Sqlocaml_unix.Store.open_file_wal`
  (with/without `~key`).
- `module Ref_sqlite : ENGINE` — via `sqlite3` opam bindings; prepared statements; `key`
  ignored (always plaintext) or raises if a key is requested.

**Fairness controls:**
- Identical deterministic dataset (fixed seed; no `Random`/`Date.now`) and query mix for both.
- Pinned page cache: sqlocaml cache pages ≈ SQLite `PRAGMA cache_size`; same `page_size`.
- Warmup pass, then K repeats; report **median and best**.
- Same schema (one indexed integer pk + a payload column).

**Measurement:** wrap each timed workload with wall-clock + `Unix.getrusage SELF` deltas
(utime+stime). Emit per workload: `ops`, `wall_s`, `cpu_s`, `cpu_wall_ratio`, `ops_per_s`.

**Output:** CSV to stdout, one row per (engine, workload, variant):
```
host,engine,workload,variant,rows,ops,wall_s,cpu_s,cpu_wall_ratio,ops_per_s
```
Plus a human-readable summary table to stderr. CSV is the source of truth for cross-host
tabulation.

**Self-check (smoke mode, `SQLOCAML_BENCH_SMOKE=1`):** small-N run that (a) asserts both engines
return **identical** query results on the same dataset (guards that we compare *correct*
engines) and (b) validates the CSV header/column count. Not a timing gate.

### 2. Image + runner

- **`containers/bench.Containerfile`**: `FROM localhost/sqlocaml-dev` + `apt-get install
  libsqlite3-dev` + `opam install sqlite3`. Tag `sqlocaml-bench`. Layering on the existing dev
  image pins **one** libsqlite3 version across both hosts (fairness) and reuses the toolchain.
- **`scripts/bench222.sh`**:
  1. Build `sqlocaml-bench` if absent.
  2. `podman run` the bench with a **bind-mounted host directory** for DB files
     (`-v $HOSTDIR:/data:Z`) so file I/O hits the real disk (HDD/NVMe), not the overlay fs.
  3. Env-pin params (rows, repeats, page cache); deterministic seed.
  4. Capture host metadata: hostname, nproc, disk model + rotational flag, `sqlite3 --version`
     (containerized), sqlocaml git SHA.
  5. Write `bench/results/<host>.csv` + a `<host>.meta` sidecar.

### 3. Cross-host execution

- Run `scripts/bench222.sh` on **here**.
- Provision `sqlocaml-bench` on **otp-prod-1** (build there from the same base image; verify
  `sqlite3 --version` matches here — if it diverges, pin via the amalgamation or mount). Run the
  same script over SSH; `scp` the CSV back.

### 4. Deliverable

- **`docs/benchmarks/2026-06-02-bench-222-results.md`**: side-by-side tables
  (workload × host × {plaintext SQLite, plaintext sqlocaml, encrypted sqlocaml}), CPU/wall
  ratios, the concurrent-sqlocaml section, and the **one-paragraph CPU-vs-I/O verdict +
  proceed/defer recommendation for #156**. Raw CSVs committed alongside.
- **`README.md`**: new `## Benchmarks` section — headline numbers + method caveats + link to the
  full results doc.
- **Forgejo wiki**: publish the results page (clone the repo `.wiki.git`, add page, push;
  confirm mechanism at publish time).

## Data flow

```
seed dataset (deterministic)
  └─ for each (engine, workload, variant):
       warmup → K timed repeats {wall + getrusage delta}
         └─ aggregate (median, best) → CSV row
              └─ tabulate across hosts → verdict
```

## Error handling

- Missing `sqlite3` bindings → **fail loud** (required now, not optional).
- Host missing the image → build step provisions it.
- Deterministic seeds only (no `Math.random`/`Date.now`); temp DB dirs created under the
  bind-mounted host dir and removed per run.
- Requesting a key on `Ref_sqlite` → raises (encrypted reference is out of scope).

## Testing

The bench is a **measurement tool**, not a pass/fail gate (CI timing gates remain neutralized
per #222). Correctness is guarded by the smoke-mode equality check between engines. No new
assertions are wired into the failing-test CI gate.

## Out of scope

- sqlcipher / encrypted reference SQLite (not installed).
- Cross-engine concurrency comparison.
- perf/flamegraph CPU attribution (coarse `getrusage` only).
- Tuning sqlocaml for the benchmark — we measure the engine as shipped.
