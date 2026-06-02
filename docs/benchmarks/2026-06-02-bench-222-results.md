# Benchmark Results — sqlocaml vs SQLite (#222)

**Date:** 2026-06-02 · **sqlocaml:** `358b2b9` · **SQLite:** 3.45.1 (in-process bindings)
**Method/design:** [design doc](../superpowers/specs/2026-06-02-bench-222-sqlocaml-vs-sqlite-design.md) · **Harness:** `test/bench_compare.ml` · **Runner:** `scripts/bench222.sh` · **Raw CSVs:** [`bench/results/`](../../bench/results/)

In-process comparison — both engines driven from the same OCaml harness with prepared
statements (reads *and* writes), WAL mode, fsync-per-commit durability (SQLite
`synchronous=FULL`), an identical deterministic dataset, and a matched page cache (1024 pages ×
4 KiB). Each workload is the **best of N repeats** (warmup discarded). The discriminator for the
CPU-vs-I/O question is `cpu/wall` per run (`Unix.times` utime+stime ÷ wall-clock): **`cpu/wall ≈
1` ⇒ CPU-bound; `cpu/wall ≪ 1` ⇒ I/O-wait-bound.** Because it is measured per-run, it is
independent of the two hosts' different CPU clocks.

## Hosts

| host | cores | storage | role |
|------|-------|---------|------|
| **here** | 8 | HDD — HGST HUS724020AL (7200 rpm, RAID) | fsync-limited |
| **otp-prod-1** | 12 | NVMe — Samsung MZVL2512 | fast I/O |

**Parameters (identical on both):** 5000 rows, 500 point-lookup / autocommit-insert ops,
20 scan repeats, 50 single-row commits, 2 timed repeats, 1024-page cache.

> **Scale caveat.** Op counts are small because sqlocaml's single-row path is ~25–50 ms/op
> (see the verdict), so a larger dataset is impractical to seed today. At 5000 rows the working
> set is cache-resident, so the **read** workloads measure pure read-CPU cost (no disk I/O); the
> **write** workloads still exercise real fsync. This is sufficient for the CPU-vs-I/O verdict
> (the read path is CPU-bound *even when fully cached*), but the absolute read numbers are not a
> large-dataset result. HDD write throughput is fsync-latency-dominated and varies a few percent
> run-to-run.

## 1. Head-to-head, plaintext (ops/sec; higher is better)

### here (HDD)

| workload | SQLite | sqlocaml | SQLite advantage | cpu/wall (sqlite → sqlocaml) |
|----------|-------:|---------:|-----------------:|:----------------------------:|
| point lookup `WHERE pk=?` | 377,933 | 19.5 | ~19,000× | 1.00 → 1.00 |
| range scan / aggregate    | 4,395  | 19.1 | ~230× | 1.00 → 1.00 |
| insert (autocommit)       | 9.0    | 4.3  | ~2.1× | 0.002 → 0.50 |
| insert (batch, 1 txn)     | 74,697 | 20.6 | ~3,600× | 0.12 → 0.99 |
| commit throughput         | 10.7   | 4.0  | ~2.7× | 0.002 → 0.51 |

### otp-prod-1 (NVMe)

| workload | SQLite | sqlocaml | SQLite advantage | cpu/wall (sqlite → sqlocaml) |
|----------|-------:|---------:|-----------------:|:----------------------------:|
| point lookup `WHERE pk=?` | 286,340 | 37.4 | ~7,700× | 1.00 → 1.00 |
| range scan / aggregate    | 7,294  | 36.5 | ~200× | 1.00 → 1.00 |
| insert (autocommit)       | 117.1  | 13.8 | ~8.5× | 0.03 → 0.87 |
| insert (batch, 1 txn)     | 223,775 | 41.9 | ~5,300× | 0.64 → 0.99 |
| commit throughput         | 108.0  | 13.4 | ~8.1× | 0.03 → 0.85 |

**Read the two tables together — this is the whole experiment:**

- **Reads have `cpu/wall = 1.00` everywhere.** Cache-resident reads do zero I/O wait on either
  disk; the time is entirely CPU. sqlocaml's read path is **~200–230× slower on scans and
  ~7,700–19,000× slower on point lookups**, all of it CPU.
- **Writes expose the disk.** SQLite autocommit/commit throughput is **~11× higher on NVMe than
  on HDD** (117 vs 9 ops/s) — pure fsync latency (~110 ms/fsync HDD vs ~9 ms NVMe), confirmed by
  `cpu/wall ≈ 0.002` on HDD.
- **On the fsync-limited HDD the sqlocaml↔SQLite write gap collapses to ~2–3×** (both engines
  wait on the same slow fsync — the disk is the equalizer), but **on NVMe it widens to ~8×**
  (fsync is cheap, so sqlocaml's heavier per-insert CPU is exposed). A gap that widens off the
  fsync-limited host is the signature of an I/O-bound workload.
- **The write cost is genuine engine work, not SQL parsing.** Switching sqlocaml's write
  workloads from interpolated SQL to prepared statements (matching the SQLite reference) changed
  its write throughput by <1% — so sqlocaml's per-insert cost is B-tree/page work plus fsync,
  not re-parse overhead.

## 2. Encryption tax (sqlocaml, AES-256-GCM at rest)

ops/sec, plaintext → encrypted (read paths; `cpu/wall` stays 1.00, so the cost is pure
per-page decrypt CPU):

| workload | here plain → enc | otp plain → enc | overhead |
|----------|:----------------:|:---------------:|:--------:|
| point lookup | 19.5 → 9.9 | 37.4 → 16.6 | **~2.0–2.2×** (≈ +100%) |
| range scan / aggregate | 19.1 → 10.3 | 36.5 → 16.9 | **~1.9–2.2×** (≈ +100%) |

Writes are barely affected (e.g. otp batch 41.9 → 42.2; here 20.6 → 19.4) — the write cost is
dominated by B-tree/insert CPU and fsync, and encryption happens only at page flush. **Per-page
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
show `cpu/wall` from 0.002 (SQLite on HDD) to ~0.87 (sqlocaml on NVMe), and SQLite's write
throughput scales ~11× with the disk. sqlocaml's gap to SQLite is overwhelmingly a CPU gap on
reads and a mixed CPU+fsync gap on writes.

### Recommendation for #156 (read-side multicore): **keep deferred — fix the single-threaded read path first.**

The CPU-bound finding means multicore is the *right eventual direction* (more cores → more
concurrent CPU-bound readers → higher aggregate read throughput). But it is **not the
highest-leverage next step**, for one decisive reason:

> **A point lookup costs the same as a full table scan.** At every data point, `point_lookup`
> time ≈ `scan_agg` time (here: 51.3 ms vs 52.4 ms; otp: 26.7 ms vs 27.4 ms; encrypted here:
> 101 ms vs 97 ms). A primary-key equality lookup that scans the whole table means the
> `WHERE pk = ?` predicate is **not being lowered to a B-tree seek** — the read path is O(n) per
> lookup where it should be O(log n). (Filed as **#228**.)

How much would the seek fix buy? SQLite's *own* seek-vs-scan margin on this dataset is **~40–90×**
(here 86×, otp 39×) — that is the order of the immediate single-threaded win at 5000 rows, and it
**grows with row count** (the scan is O(n), the seek O(log n)), so on realistic datasets it
reaches several orders of magnitude. Either way it dwarfs the ≤ N-cores× ceiling that #156 could
deliver, and it benefits every single-threaded query immediately. Multicore multiplies
throughput, but multiplying an O(n)-per-lookup path is the wrong order of operations.

**Therefore:** (1) treat the missing PK-seek as the priority read-path fix (filed as **#228**);
(2) re-run this benchmark after that fix; (3) reconsider #156 then — the CPU-bound nature will
still hold, and at that point multicore becomes the natural next multiplier, especially for the
encrypted read path (~2× decrypt CPU). Until the per-core read cost is reasonable, #156 stays
gated.

## Reproduce

```bash
# the published run used these parameters (the script's bare defaults are larger):
SQLOCAML_BENCH_ROWS=5000 SQLOCAML_BENCH_OPS=500 SQLOCAML_BENCH_SCANS=20 \
  SQLOCAML_BENCH_COMMITS=50 SQLOCAML_BENCH_REPEATS=2 scripts/bench222.sh
# → builds the bench image, runs locally, writes bench/results/<host>.csv
```

Cross-host: transfer the image (`podman save | ssh host podman load`), rsync the source, then run
the same harness on the second host (see the design doc). Numbers are a snapshot at `358b2b9`;
sqlocaml is a young engine and these are expected to move as the read path is optimized.
