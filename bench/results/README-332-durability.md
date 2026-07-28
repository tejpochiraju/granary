# #332 — per-mode commit throughput on the HDD host profile

Follow-up to #298 (durability knob). Quantifies the hosting-density win of the
`full` / `batched` / `off` durability modes on the **fsync-bound small HDD host**
(the profile that caps how many sqlocaml apps a box can hold — see the #222
verdict: writes are fsync-bound, and the HDD fsync ceiling is the binding
constraint).

## How it was run

Host `otp-infra-1` (8 cores), data dir bind-mounted to a host directory on
`/dev/md3` — an `md` RAID over two **HGST HUS724020AL 7200rpm HDDs** — so commit
fsyncs hit a real rotating platter rather than the podman overlay (in-container /
NVMe numbers hide the fsync-ceiling story).

```sh
DATA_DIR="$PWD/bench/data"; mkdir -p "$DATA_DIR"; chmod 777 "$PWD" "$DATA_DIR"
podman run --rm \
  -e GRANARY_BENCH_DURABILITY=sweep \
  -e GRANARY_BENCH_HOST=otp-infra-1-hdd \
  -e GRANARY_BENCH_ROWS=2000 -e GRANARY_BENCH_OPS=1000 \
  -e GRANARY_BENCH_SCANS=10 -e GRANARY_BENCH_COMMITS=500 \
  -e GRANARY_BENCH_REPEATS=3 -e GRANARY_BENCH_PAGE_CACHE=1024 \
  -e TMPDIR=/benchdata \
  -v "$PWD":/workspace:Z -w /workspace \
  -v "$DATA_DIR":/benchdata:Z \
  localhost/sqlocaml-bench \
  dune exec test/bench_compare.exe > bench/results/otp-infra-1-hdd-durability-sweep.csv
```

`GRANARY_BENCH_DURABILITY` (added for #332) accepts `full` | `batched` | `off` |
`sweep`. `sweep` runs the sqlocaml engine once per mode in a single CSV;
`batched` honours `GRANARY_BENCH_BATCH_N` (256) and `GRANARY_BENCH_BATCH_T_MS`
(100). The SQLite reference always runs `synchronous=FULL`. With no env var set,
the harness behaves exactly as the original #222 run.

## Results (raw CSV: `otp-infra-1-hdd-durability-sweep.csv`)

The two fsync-bound write workloads — `insert_one` (N autocommit single-row
inserts, one fsync each under `full`) and `commit_n` (N single-row txns) — are
where durability mode dominates:

| workload   | mode    | wall (s) | ops/s | speedup vs full |
|------------|---------|---------:|------:|----------------:|
| insert_one | full    |    89.45 |  11.2 |          1.0×   |
| insert_one | batched |     9.83 | 101.7 |          9.1×   |
| insert_one | off     |     8.18 | 122.2 |         10.9×   |
| commit_n   | full    |    42.78 |  11.7 |          1.0×   |
| commit_n   | batched |     6.20 |  80.6 |          6.9×   |
| commit_n   | off     |     5.02 |  99.6 |          8.5×   |

(SQLite reference, `synchronous=FULL`: insert_one 14.1 ops/s, commit_n 36.3 ops/s.)

Read workloads (`point_lookup`, `scan_agg`) are unaffected by the durability
knob, as expected — the small variations across modes are noise.

## Takeaway

On the HDD host, relaxing durability from `full` lifts per-connection commit
throughput by **~7× (batched) to ~9–11× (off)**. Because the small host's
capacity is fsync-bound, that throughput headroom translates roughly into the
same multiple of additional co-hosted sqlocaml apps before the disk's fsync
ceiling is the bottleneck — the hosting-density win #298 was built for.

`batched` lands close to `off` here because with N=256 and only 500–1000 commits
the run incurs just a handful of fsyncs; under `batched`/`off` the workload is
CPU-bound (cpu/wall ≈ 0.3–0.4) rather than fsync-bound (cpu/wall ≈ 0.05 under
`full`). A deployment that needs a bounded crash-loss window picks `batched`;
one that treats the store as reconstructible picks `off`.
