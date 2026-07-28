# #177 full-suite run scripts

Reproducible orchestration for the Jepsen #177 matrix, including the
encrypted-WAL backend (`--backend enc-wal`, #84/#214/#215).

| Script | Container | Purpose |
|--------|-----------|---------|
| `gen_177.sh` | `granary-dev` | Build the harness and generate every workload + crash/pause-nemesis history into `jepsen/run/histories/`, plus the 5 negative controls. |
| `run_nemeses_fuse.sh` | `granary-jepsen` (host-side) | Run the privileged nemeses that `gen_177.sh` can't: **lazyfs** (un-fsynced write loss; needs `--device /dev/fuse --cap-add SYS_ADMIN`) on WAL + enc-WAL, and **clock-skew** (needs `libfaketime` via `LD_PRELOAD`). |
| `check_177.sh` | `granary-jepsen-elle2` | Check every history (Clojure temporal + Elle SI), reporting VALID/INVALID and catching the negative controls. |
| `detail_177.sh` | `granary-jepsen-elle2` | Dump full anomaly detail for the four counter/list-append histories that flag INVALID. |
| `ci_full.sh` | `granary-jepsen` (one container, +FUSE) | Self-contained gen + nemeses + check with a pass/fail **gate** (expected verdict per history). Generates into `/tmp` (never touches the committed run). Driven by the nightly workflow `.forgejo/workflows/jepsen-nightly.yml`. |

The `granary-jepsen` image (built from the root `Containerfile.jepsen`) carries
the OCaml harness, lazyfs (+ its `libpcache`), and `libfaketime`.

## Usage

```bash
# from the repo root
podman run --rm -v "$PWD":/workspace:z -w /workspace granary-dev \
  bash jepsen/run/gen_177.sh

podman run --rm -v "$PWD":/workspace:z granary-jepsen-elle2 \
  bash jepsen/run/check_177.sh
```

## Notes

- The runtime container user can't write the mounted tree or its own
  `~/.clojure`, so `check_177.sh` copies the Clojure project into `/tmp` and
  uses `HOME=/tmp/clj-home` for the dep cache (`clojure -P` once, then check).
- `counter_mem` / `counter_wal` / `counter_encwal` are all VALID since the WAL
  concurrent-`UPDATE` lost-update (**#223**) was fixed (PR #225): final value
  equals the acked increment count on every backend.
- `set_wal_lazyfs` / `set_encwal_lazyfs` are VALID: acked elements survive
  un-fsynced write loss (durability holds, including encrypted). The lazyfs
  console lines `BEHAVE AS` / `terminate called…` are its benign SIGTERM
  shutdown chatter, not a harness failure.
- `list_append_clockskew` is VALID under a +5d skewed clock and carries one
  recorded `clock-skew` nemesis marker.
- `list_append_wal` / `list_append_encwal` show INVALID under the pure-Clojure
  *temporal* checker (benign `:ok`-completion-timestamp inversion); **Elle SI is
  authoritative and reports VALID** for both. Elle's `:duplicate-elements` on
  `list_append_mem` is the known non-unique-append workload artifact (#180).
- `histories/` holds a committed reference run (post-#223 `main`). Re-running
  `gen_177.sh` (+ `run_nemeses_fuse.sh`) overwrites them.
