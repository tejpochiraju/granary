# #481 — where the `scan_agg` gap vs C SQLite actually goes

Companion to the #222 cross-engine run on `macbook15`. `scan_agg`
(`SELECT COUNT(*), SUM(k) FROM t`) is our **worst workload relative to SQLite —
12.2×**, well behind `point_lookup` (3.9×) and `insert_batch` (3.3×). This
documents the attribution so the next person does not have to re-derive it.

## Host

2015 MacBook Pro — i7-4870HQ (4c/8t Haswell @2.5 GHz), 15 GB RAM, APPLE SSD
SM0256F (SATA, non-rotational), btrfs, Fedora Atomic. Benchmark run through
rootless podman. granary `89c52d9`, SQLite 3.45.1. Both engines pinned to
`page_size=4096`, `cache_size=1024` pages, WAL, `synchronous=FULL`. Data dir
bind-mounted to real disk (raw CSV: `macbook15.csv` / `.meta`).

| engine | wall (50 scans) | ns/row |
|---|---:|---:|
| sqlite | 0.2407 s | **48** |
| granary | 2.9307 s | **586** |

586 ns/row is ~1465 cycles to count a row and add one integer.

## How to reproduce

`test/bench_scan_probe.exe` (added by this PR). The attribution works because
**`SELECT COUNT(*)` sets `need_decode = false`** in the #247 aggregate fast
path, so it drives the storage cursor with *zero* row decoding — no profiler
required.

```sh
podman run --rm -e TMPDIR=/benchdata -e PROBE_ROWS=100000 -e PROBE_REPS=20 \
  -v "$PWD":/workspace:Z -w /workspace -v "$PWD/bench/data":/benchdata:Z \
  localhost/granary-bench dune exec test/bench_scan_probe.exe
```

## Finding 1 — the #247 fast path is working; it is not the problem

`GRANARY_AGG_FASTPATH=0` → `1` at 100k rows: **1180 → 577 ns/row**. The fast
path is engaged and already buys 2.05×. The gap lives *below* the exec layer.
(577 ns/row here corroborates the 586 ns/row measured by `bench_compare`.)

## Finding 2 — 69% is the storage cursor, 31% is row decode

100k rows, default page cache:

| query | decode work | ns/row | words/row |
|---|---|---:|---:|
| `SELECT COUNT(*)` | none | **400** | 169.1 |
| `SELECT SUM(id)` | 1 column | 536 | 219.0 |
| `SELECT COUNT(*), SUM(k)` | 2 columns | 577 | 236.0 |

A query that decodes **nothing** already costs 400 ns/row — 8.3× SQLite's
entire 48 ns/row scan — and allocates **169 words (~1.35 KB) per row**.
Optimising `Row.decode_prefix` alone therefore cannot close this gap.

## Finding 3 — a page-cache cliff sits exactly where #222 runs

`COUNT(*)`, 100k rows, sweeping `GRANARY_PAGE_CACHE`. Note `words/row` is
**constant at 169.4** across the sweep — this is cache misses, not allocation:

| cache | ns/row |
|---|---:|
| 512 (2 MB) | 390 |
| **1024 (4 MB — the #222 setting)** | **401** |
| 2048 (8 MB) | 236 |
| 4096 (16 MB) | 222 |
| 16384 (64 MB) | 218 |

Row-count sweep at the default 1024 pages shows the same knee: 25k → 176
ns/row, 50k → 179, **100k → 409**, 200k → 448.

A 100k-row table's leaves exceed 1024 pages, so #222 measures granary in a
thrashing regime; ~180 ns/row (≈31% of `scan_agg`) is miss cost. **SQLite is
given the same 1024-page cache and does not collapse** — its miss path is a
`pread` from a warm OS page cache, ours is a WAL-index lookup plus a full 4 KB
`cstruct_dup`. This is a defect in our miss path, not an unfair setting.

## Finding 4 — scans pay for column bytes they never read

`COUNT(*)` (reads no columns), 20k rows, **64 MB cache so Finding 3 is excluded**:

| payload width | ns/row | words/row |
|---:|---:|---:|
| 12 B | 177 | 164.9 |
| 40 B | 204 | 177.2 |
| 55 B | 226 | 183.9 |
| 70 B | 243 | 191.2 |
| 85 B | **846** | 206.8 |
| 100 B | 927 | 214.8 |
| 200 B | 1421 | 266.1 |
| 400 B | 2331 | 364.2 |

Counting rows gets **13× slower** as a column it never touches grows.
Allocation rises ~4× faster than the payload; after subtracting the per-leaf
4 KB page dup that is **≈3 full copies of every value, per row, for a query
that reads no values.**

There is also a sharp, reproducible knee between 70 B and 85 B (243 → 846
ns/row) where minor collections roughly double (14 → 29) while `minor_words`
rises only 8%. Blocks over 256 words bypass the minor heap, so that signature
points at major-heap traffic (consistent with the page dups) — but the
mechanism is **unconfirmed**. It is *not* the overflow threshold
(`inline_value_threshold = 800`).

## Root causes

1. **Per-row key *and* value copy** — `Page.leaf_entry_at`
   (`lib/storage/page.ml`) `Bytes.create` + `blit_to_bytes` for both, on every
   entry. The aggregate fast path then discards the key (`Some (_key, vbytes)`
   in `lib/sql/exec.ml`): the key copy is pure waste on every scan.
2. **The value is copied again to strip one tag byte** —
   `Btree.decode_leaf_value` (`lib/storage/btree.ml`).
3. **A full 4 KB page copy per leaf advance** — `read_cur_leaf` uses
   `Pager.read`, which returns `cstruct_dup buf` *even on a cache hit*
   (`lib/storage/pager.ml`). `Pager.read_borrow` exists and is used elsewhere.
   #238 removed the *per-row* dup; the *per-leaf* one remains and grows as
   rows-per-leaf falls.
4. **The leaf header is re-parsed per row** — `cursor_next` calls
   `Page.read_common` for every entry, re-reading a leaf-invariant `n_keys`.
5. **A per-row Lwt bind chain over in-memory data** — `exec` →
   `Store.seek_next` → `Btree.cursor_next` → `read_cur_leaf` →
   `decode_leaf_value`, ~4–6 promise allocations per row even when every page
   is resident. Same conclusion #416 reached on the point-lookup path.
6. **O(n) work per leaf advance** — `frame_child` uses `List.length` then
   `List.nth`; `decode_branch_entries` decodes the whole branch page per advance.
7. **Row decode** (the smaller 31%) — `Null_bitmap.unpack_bits_to_bools`
   allocates a bool array per row, plus `Array.make n V_null`, a tuple per
   varint, and a boxed `Int64` per integer column (`lib/encoding/row.ml`).

## Candidate fixes (ranked)

- **A. Borrowed/zero-copy cursor entries** — a `cursor_fold` /
  `cursor_next_borrow` handing out a `Cstruct` window into the cached
  `leaf_buf`. Kills causes 1+2 and makes scan cost independent of unread column
  width. Safe under a fixed snapshot, where the leaf is immutable.
- **B. Don't materialise the key when the consumer discards it** — a
  `~want_key:false` / value-only cursor. Cheapest possible win, no
  borrow-lifetime questions. Good first PR.
- **C. Drop the tag-strip copy** — return `(bytes, offset)` or a sub-view.
- **D. Use `Pager.read_borrow` in `read_cur_leaf`** — removes a 4 KB
  major-heap copy per leaf advance, and would confirm or kill the Finding-4
  knee hypothesis.
- **E. Hoist `Page.read_common` out of the per-row path** — cache `n_keys` on
  the cursor. Small and contained.
- **F. Synchronous fast path for cache-resident scans** — no promise
  allocation when the leaf is already in `leaf_buf`, keeping the existing
  `seek_pause_interval` trampoline for stack safety. The scan path is an easier
  place to prototype this than #416's, since the fold already owns its loop.
- **G. Fix the miss path / revisit `default_cache_capacity = 1024`.**
- **H. Cheap decode wins** — test null-bitmap bits directly, drop the varint
  tuple, unboxed accumulators for COUNT/SUM.
- **I. Explain the 70→85 B knee.**

A+B+C+D target the ~400 ns/row cursor cost; H targets the ~177 ns/row decode.

Refs: #222 (benchmark), #247 (fast path — confirmed working), #230 / #238
(earlier cursor fixes), #416 (same Lwt-chain conclusion, point-lookup path),
#414 / #156 (allocation caps multicore read scaling).
