# Benchmark Results — sqlocaml vs SQLite (#222)

**Date:** 2026-06-02 · **sqlocaml:** `f5f6861` · **SQLite:** 3.45.1 (in-process bindings)
**Method/design:** [design doc](../superpowers/specs/2026-06-02-bench-222-sqlocaml-vs-sqlite-design.md) · **Harness:** `test/bench_compare.ml` · **Runner:** `scripts/bench222.sh` · **Raw CSVs:** [`bench/results/`](../../bench/results/)

In-process comparison — both engines driven from the same OCaml harness with prepared
statements, WAL mode, fsync-per-commit durability (SQLite `synchronous=FULL`), an identical
deterministic dataset, and a matched page cache (1024 pages × 4 KiB). Each workload is the
**best of N repeats** (warmup discarded). The discriminator for the CPU-vs-I/O question is
`cpu/wall` per run (`Unix.times` utime+stime ÷ wall-clock): **`cpu/wall ≈ 1` ⇒ CPU-bound;
`cpu/wall ≪ 1` ⇒ I/O-wait-bound.** Because it is measured per-run, it is independent of the
two hosts' different CPU clocks.

## Hosts

| host | cores | storage | role |
|------|-------|---------|------|
| **here** | 8 | HDD — HGST HUS724020AL (7200 rpm, RAID) | fsync-limited |
| **otp-prod-1** | 12 | NVMe — Samsung MZVL2512 | fast I/O |

**Parameters (identical on both):** 5000 rows, 500 point-lookup / autocommit-insert ops,
20 scan repeats, 50 single-row commits, 2 timed repeats, 1024-page cache.

> **Scale caveat.** Op counts are small because sqlocaml's single-row path is ~20–50 ms/op
> (see below), so a larger dataset is impractical to seed today. At 5000 rows the working set
> is cache-resident, so the **read** workloads measure pure read-CPU cost (no disk I/O); the
> **write** workloads still exercise real fsync. This is sufficient for the CPU-vs-I/O verdict
> (the read path is CPU-bound *even when fully cached*), but the absolute read numbers are not a
> large-dataset result.

## 1. Head-to-head, plaintext (ops/sec; higher is better)

### here (HDD)

| workload | SQLite | sqlocaml | SQLite advantage | cpu/wall (sqlite → sqlocaml) |
|----------|-------:|---------:|-----------------:|:----------------------------:|
| point lookup `WHERE pk=?` | 359,471 | 21.2 | ~16,900× | 1.00 → 1.00 |
| range scan / aggregate    | 4,581  | 21.7 | ~211× | 1.00 → 1.00 |
| insert (autocommit)       | 10.9   | 4.8  | ~2.3× | 0.002 → 0.47 |
| insert (batch, 1 txn)     | 74,675 | 24.6 | ~3,000× | 0.07 → 0.99 |
| commit throughput         | 11.2   | 3.7  | ~3.0× | 0.002 → 0.41 |

### otp-prod-1 (NVMe)

| workload | SQLite | sqlocaml | SQLite advantage | cpu/wall (sqlite → sqlocaml) |
|----------|-------:|---------:|-----------------:|:----------------------------:|
| point lookup `WHERE pk=?` | 234,765 | 36.9 | ~6,400× | 1.00 → 1.00 |
| range scan / aggregate    | 7,364  | 36.1 | ~204× | 1.00 → 1.00 |
| insert (autocommit)       | 112.6  | 13.8 | ~8.2× | 0.03 → 0.86 |
| insert (batch, 1 txn)     | 378,958 | 42.0 | ~9,000× | 0.39 → 0.99 |
| commit throughput         | 111.6  | 13.5 | ~8.3× | 0.04 → 0.85 |

**Read the two tables together — this is the whole experiment:**

- **Reads have `cpu/wall = 1.00` everywhere.** Cache-resident reads do zero I/O wait on either
  disk; the time is entirely CPU. sqlocaml's read path is **~200× slower on scans and
  ~6,400–16,900× slower on point lookups**, all of it CPU.
- **Writes expose the disk.** SQLite autocommit/commit throughput is **~10× higher on NVMe than
  on HDD** (112 vs 11 ops/s) — pure fsync latency (~100 ms/fsync HDD vs ~9 ms NVMe), confirmed
  by `cpu/wall ≈ 0.002` on HDD.
- **On the fsync-limited HDD the sqlocaml↔SQLite write gap collapses to ~2–3×** (both engines
  wait on the same slow fsync — the disk is the equalizer), but **on NVMe it widens to ~8×**
  (fsync is cheap, so sqlocaml's heavier per-insert CPU is exposed). A gap that widens off the
  fsync-limited host is the signature of an I/O-bound workload.

## 2. Encryption tax (sqlocaml, AES-256-GCM at rest)

ops/sec, plaintext → encrypted (read paths; `cpu/wall` stays 1.00, so the cost is pure
per-page decrypt CPU):

| workload | here plain → enc | otp plain → enc | overhead |
|----------|:----------------:|:---------------:|:--------:|
| point lookup | 21.2 → 10.3 | 36.9 → 16.8 | **~2.1×** (≈ +105%) |
| range scan / aggregate | 21.7 → 10.8 | 36.1 → 16.4 | **~2.1×** (≈ +105%) |

Writes are barely affected (e.g. otp batch 42.0 → 42.5; here 24.6 → 23.7) — the write cost is
dominated by B-tree/insert CPU, and encryption happens only at page flush. **Per-page
AES-256-GCM roughly doubles read cost**, and it is entirely CPU — the single strongest case for
read-side multicore among the read paths.

## 3. Concurrency (sqlocaml-only)

The reader-scaling bench (`test/bench_wal_reader_scaling.ml`, 8 readers) passes its assertion —
**"parallel readers do not regress vs serial"** — on both hosts: concurrent readers complete in
about the same wall-clock as serial ones (ratio ≈ 1). But that is *cooperative interleaving*
under a single OS thread, not parallelism: there is **no multi-core speedup today**. A
cross-engine concurrency number is intentionally omitted — comparing Lwt cooperative scheduling
to SQLite's threading model would be apples-to-oranges.

## 4. Verdict — CPU-bound or I/O-bound?

**The read path is unambiguously CPU-bound; the write path is fsync/I-O-bound.** Reads show
`cpu/wall = 1.00` on both engines and both disks (no I/O wait), while autocommit/commit writes
show `cpu/wall` from 0.002 (SQLite on HDD) to ~0.86 (sqlocaml on NVMe), and SQLite's write
throughput scales ~10× with the disk. sqlocaml's gap to SQLite is overwhelmingly a CPU gap on
reads and a mixed CPU+fsync gap on writes.

### Recommendation for #156 (read-side multicore): **keep deferred — fix the single-threaded read path first.**

The CPU-bound finding means multicore is the *right eventual direction* (more cores → more
concurrent CPU-bound readers → higher aggregate read throughput). But it is **not the
highest-leverage next step**, for one decisive reason:

> **A point lookup costs the same as a full table scan.** At every data point, `point_lookup`
> time ≈ `scan_agg` time (here: 47.2 ms vs 46.1 ms; otp: 27.1 ms vs 27.7 ms; encrypted here:
> 97 ms vs 93 ms). A primary-key equality lookup that scans the whole table means the
> `WHERE pk = ?` predicate is **not being lowered to a B-tree seek** — the read path is O(n) per
> lookup where it should be O(log n).

Fixing that single-threaded regression alone would yield roughly **three orders of magnitude** on
point lookups — far more than the ≤ N-cores× ceiling that #156 could ever deliver, and it
benefits every single-threaded query immediately. Multicore multiplies throughput, but
multiplying an O(n)-per-lookup path is the wrong order of operations.

**Therefore:** (1) treat the missing PK-seek as the priority read-path fix (filed as a
follow-up); (2) re-run this benchmark after that fix; (3) reconsider #156 then — the CPU-bound
nature will still hold, and at that point multicore becomes the natural next multiplier,
especially for the encrypted read path (~2× decrypt CPU). Until the per-core read cost is
reasonable, #156 stays gated.

## Reproduce

```bash
scripts/bench222.sh                 # builds the bench image, runs locally, writes bench/results/<host>.csv
# cross-host: transfer the image (podman save | ssh host podman load), rsync the source,
# then run the same harness on the second host (see the design doc).
```

Numbers are a snapshot at `f5f6861`; sqlocaml is a young engine and these are expected to move
as the read path is optimized.
