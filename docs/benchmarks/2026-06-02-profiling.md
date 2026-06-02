# Hotpath Profiling — sqlocaml (#222 follow-up)

**Date:** 2026-06-02 · **sqlocaml:** `f5f6861` · **Host:** otp-infra-1 (HDD, 8 cores) · **Charts:** [wiki Profiling page](https://git.iotready.com/tej/sqlite_ocaml_port/wiki/Profiling)

Follow-up to the #222 benchmark: *where* does the slowness come from, toward the #231 "within 100× of SQLite" goal. Two methods — a **scaling measurement** (complexity) and a **`perf` CPU flamegraph** (hotspot).

## Method
- Scaling: `test/bench_compare.exe` at 1k/2k/4k rows; per-op cost = wall ÷ ops (raw in `bench/profiling/scaling.txt`, chart `scaling.gp`/`scaling.dat`).
- Flamegraph: `perf record -F 299 --call-graph dwarf` on a native build (host `perf_event_paranoid=1`; binary built in-container, run on host), folded with Brendan Gregg's FlameGraph. 75,021 samples.

## Result 1 — complexity (the decisive finding)

Per-operation cost vs table size (sqlocaml, plaintext), rows doubling each step:

| workload | 1k | 2k | 4k | per-doubling | complexity |
|----------|---:|---:|---:|:---:|---|
| point lookup `WHERE pk=?` | 9.8 ms | 22.4 ms | 49.2 ms | ~2.2× | **O(n) per lookup** ← bug (#228) |
| full scan / aggregate     | 10.2 ms | 26.2 ms | 45.5 ms | ~2.1× | O(n) — correct for a scan; constant is high (#230) |
| insert (per row)          | 9.7 ms | 19.3 ms | 41.6 ms | ~2.1× | **O(n) per insert ⇒ bulk insert O(n²)** ← bug (#229) |

Point-lookup cost ≈ full-scan cost at every size **and** grows linearly with row count → the PK equality is doing a full table scan, not a seek. Per-insert cost grows linearly with table size → inserting n rows is O(n²) (this is why a 20k-row seed never finished).

## Result 2 — the hot function

`perf` self-time, leaf hotspot:

| self-time | function |
|----------:|----------|
| **16.6%** | `Sqlocaml_storage.Page.leaf_entry_at` |
| (driver)  | `Sqlocaml_storage.Btree` leaf loop → `Sqlocaml_store.Store` → `Sqlocaml_sql.Exec` |

The dominant cost is decoding leaf entries one-by-one. In source this is `Btree.decode_leaf_entries` (`lib/storage/btree.ml:293`), a loop that decodes **every** entry of a leaf via `Page.leaf_entry_at` into a list. The insert path reaches it through `Exec → Store → Btree`; the read paths reach the same loop.

## Root cause (single shared bug)

Every key-locate **linearly walks O(n) leaf entries** instead of an O(log n) descent + in-page binary search. Combined with the O(n)-per-op scaling, the working hypothesis is that **leaf pages are not staying bounded / not splitting** (so one leaf holds ~all rows) and/or **in-leaf lookup is a linear scan over a fully-decoded entry list** rather than a binary search on page offsets. The fix author should confirm which, but the evidence (O(n) scaling + `decode_leaf_entries`/`leaf_entry_at` dominating) points squarely here.

This one hot path explains all three gaps:
- **#228** point lookup = O(n) leaf walk (no seek).
- **#229** insert positioning = O(n) leaf walk per insert → O(n²) bulk load.
- **#230** scans pay the same per-entry decode cost as a high constant (~11 µs/row).

## Suggested fixes (in #228/#229/#230)
1. Ensure the B-tree splits so leaves are bounded (fan-out), making descent O(log n).
2. Binary-search within a page using `leaf_entry_at` offsets instead of decoding the whole leaf into a list per op.
3. Avoid the per-op list allocation in `decode_leaf_entries` on the hot lookup/insert path (helps #230's constant + GC pressure).

Re-run scaling + `bench222.sh` after the fix; the per-op lines should flatten (point lookup → ~flat, insert per-row → ~flat).

## Reproduce
```bash
# scaling
for n in 1000 2000 4000; do SQLOCAML_BENCH_ROWS=$n SQLOCAML_BENCH_OPS=100 SQLOCAML_BENCH_SCANS=10 \
  SQLOCAML_BENCH_COMMITS=10 SQLOCAML_BENCH_REPEATS=1 ./_build/default/test/bench_compare.exe 2>/dev/null \
  | grep -E "sqlocaml,(point_lookup|scan_agg|insert_batch),plaintext"; done
# flamegraph (host perf_event_paranoid=1)
perf record -F 299 --call-graph dwarf -o perf.data -- ./_build/default/test/bench_compare.exe
perf script -i perf.data | ~/projects/FlameGraph/stackcollapse-perf.pl | ~/projects/FlameGraph/flamegraph.pl > flame.svg
```
