# Benchmark Results — sqlocaml vs SQLite (#222), NVMe baseline

**Date:** 2026-06-07 · **sqlocaml:** `1a73da0` · **SQLite:** 3.45.1 (in-process bindings)
**Method/design:** [design doc](../superpowers/specs/2026-06-02-bench-222-sqlocaml-vs-sqlite-design.md) · **Harness:** `test/bench_compare.ml` · **Runner:** `scripts/bench222.sh` · **Raw CSV:** [`bench/results/otp-prod-1.csv`](../../bench/results/otp-prod-1.csv)

This is a refreshed **NVMe-only** baseline taken after the major read- and write-path
performance work landed: **#228** (PK `WHERE pk=?` lowered to an O(log n) B-tree seek), **#229**
(bulk insert reduced from O(n²) to O(n)), and the read-path optimizations **T4 frame-cache** and
**T5 aggregate fast-path**. It supersedes the [2026-06-02 pre-fix snapshot](2026-06-02-bench-222-results.md)
(`358b2b9`), which remains for historical reference. The HDD host was intentionally **not**
re-run this round; its last numbers are in the 2026-06-02 doc.

In-process comparison — both engines driven from the same OCaml harness with prepared statements
(reads *and* writes), WAL mode, fsync-per-commit durability (SQLite `synchronous=FULL`), an
identical deterministic dataset, and a matched page cache (1024 pages × 4 KiB). Each workload is
the **best of N repeats** (warmup discarded). The CPU-vs-I/O discriminator is `cpu/wall` per run
(`Unix.times` utime+stime ÷ wall-clock): **`cpu/wall ≈ 1` ⇒ CPU-bound; `cpu/wall ≪ 1` ⇒
I/O-wait-bound.**

## Host

| host | cores | storage | role |
|------|-------|---------|------|
| **otp-prod-1** | 12 | NVMe — Samsung MZVL2512 (md-RAID) | fast I/O |

**Parameters:** 5000 rows, 500 point-lookup / autocommit-insert ops, 20 scan repeats, 50
single-row commits, 2 timed repeats, 1024-page cache (identical to the 2026-06-02 run for
apples-to-apples comparison).

> **Caveats.** Run on a **production** host under live load (load average ~5–7 during the run).
> At 5000 rows the working set is cache-resident, so the **read** workloads measure pure read-CPU
> cost (no disk I/O) and the **write** workloads still exercise real fsync. The read and
> batch-insert measurements are sub-100 ms in absolute terms, so they carry run-to-run noise and
> ~10 ms CPU-tick quantization — single-digit `×` figures should be read as "low single digits,"
> not precise. The multi-second fsync workloads (insert_one, commit_n) are stable. One artifact of
> noise: encrypted point-lookup reads *faster* than plaintext below — both are ~1–3 ms totals, so
> that ordering is measurement noise, not a real result.

## 1. Head-to-head, plaintext, NVMe (ops/sec; higher is better)

| workload | SQLite | sqlocaml | SQLite advantage | cpu/wall (sqlite → sqlocaml) |
|----------|-------:|---------:|-----------------:|:----------------------------:|
| point lookup `WHERE pk=?` | 689,626 | 187,967 | **~3.7×** | 1.02 → 1.00 |
| range scan / aggregate    | 7,233   | 868      | **~8.3×** | 1.00 → 1.07 |
| insert (autocommit)       | 113.4   | 56.9     | **~2.0×** | 0.03 → 0.50 |
| insert (batch, 1 txn)     | 345,017 | 7,752    | **~44.5×** | 0.46 → 0.83 |
| commit throughput         | 117.4   | 40.8     | **~2.9×** | 0.04 → 0.58 |

- **Reads are still CPU-bound** (`cpu/wall ≈ 1.0`) but the gap is now small: point lookup ~3.7×
  (was ~7,700×) and scan/aggregate ~8.3× (was ~200×). The point-lookup path is a real B-tree seek
  now — it no longer tracks scan time.
- **Single-row writes are fsync-bound** (`cpu/wall` 0.50–0.58): ~2–3× SQLite on NVMe, the residual
  being sqlocaml's heavier per-insert CPU on top of the same fsync.
- **Batch insert (~44×)** is the lone large remaining gap — O(n) now, but with a high constant
  (copy-on-write write-amplification per row). This is the next perf target (#230 / #231).

## 2. Encryption tax (sqlocaml, AES-256-GCM at rest)

ops/sec, plaintext → encrypted:

| workload | plain → enc | overhead |
|----------|:-----------:|:--------:|
| point lookup `WHERE pk=?` | 187,967 → 343,177 | within noise (sub-ms totals) |
| range scan / aggregate    | 868 → 718 | **~1.2×** (≈ +20%) |
| insert (batch, 1 txn)     | 7,752 → 7,004 | ~1.1× |
| commit throughput         | 40.8 → 35.0 | ~1.2× |

The frame-cache (T4) caches **decrypted** pages, so per-page AES-256-GCM is now amortized across
cache-resident reads — encryption read overhead has dropped from the ~2× (≈ +100%) seen in the
pre-fix run to roughly noise / ~20%. Writes are barely affected, as before.

## 3. Before / after — NVMe, plaintext (× slower vs SQLite)

| workload | 2026-06-02 (`358b2b9`) | 2026-06-07 (`1a73da0`) | sqlocaml ops/s improvement |
|----------|----------------------:|----------------------:|---------------------------:|
| point lookup `WHERE pk=?` | ~7,700× | **~3.7×** | ~5,000× faster (37.4 → 187,967) |
| range scan / aggregate    | ~200×   | **~8.3×** | ~24× faster (36.5 → 868) |
| insert (autocommit)       | ~8.5×   | **~2.0×** | ~4× faster (13.8 → 56.9) |
| insert (batch, 1 txn)     | ~5,300× | **~44.5×** | ~185× faster (41.9 → 7,752) |
| commit throughput         | ~8.1×   | **~2.9×** | ~3× faster (13.4 → 40.8) |

## 4. Status vs the #231 "within 10× of SQLite" goal

Plaintext, NVMe: **4 of 5 workloads are now within 10×** (point lookup 3.7×, scan 8.3×,
insert_one 2.0×, commit 2.9×). **`insert_batch` (~44×) is the remaining gap** — the constant-factor
copy-on-write write-amplification, tracked by #230 / #231.

## Reproduce

```bash
# matched the 2026-06-02 parameters for comparability (the script's bare defaults are larger):
SQLOCAML_BENCH_ROWS=5000 SQLOCAML_BENCH_OPS=500 SQLOCAML_BENCH_SCANS=20 \
  SQLOCAML_BENCH_COMMITS=50 SQLOCAML_BENCH_REPEATS=2 scripts/bench222.sh otp-prod-1
# → builds the bench image, runs locally, writes bench/results/<host>.csv
```

Cross-host: transfer the image (`podman save | ssh host podman load`), clone the source at the
target SHA, then run the same harness on the NVMe host. Numbers are a snapshot at `1a73da0`;
sqlocaml is a young engine and these are expected to keep moving as the write path is optimized.
</content>
