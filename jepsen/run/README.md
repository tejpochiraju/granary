# #177 full-suite run scripts

Reproducible orchestration for the Jepsen #177 matrix, including the
encrypted-WAL backend (`--backend enc-wal`, #84/#214/#215).

| Script | Container | Purpose |
|--------|-----------|---------|
| `gen_177.sh` | `sqlocaml-dev` | Build the harness and generate every workload/nemesis history into `jepsen/run/histories/`, plus the 5 negative controls. |
| `check_177.sh` | `sqlocaml-jepsen-elle2` | Check every history (Clojure temporal + Elle SI), reporting VALID/INVALID and catching the negative controls. |
| `detail_177.sh` | `sqlocaml-jepsen-elle2` | Dump full anomaly detail for the four counter/list-append histories that flag INVALID. |

## Usage

```bash
# from the repo root
podman run --rm -v "$PWD":/workspace:z -w /workspace sqlocaml-dev \
  bash jepsen/run/gen_177.sh

podman run --rm -v "$PWD":/workspace:z sqlocaml-jepsen-elle2 \
  bash jepsen/run/check_177.sh
```

## Notes

- The runtime container user can't write the mounted tree or its own
  `~/.clojure`, so `check_177.sh` copies the Clojure project into `/tmp` and
  uses `HOME=/tmp/clj-home` for the dep cache (`clojure -P` once, then check).
- The pure-Clojure **temporal** checker flags benign `:dirty-read`s on WAL
  list-append (`:ok`-timestamp inversion under concurrent fibers); **Elle SI**
  is authoritative and reports VALID. Elle's `:duplicate-elements` on
  `list_append_mem` is the known non-unique-append workload artifact (#180).
- `counter_wal` / `counter_encwal` are expected INVALID until **#223** (WAL
  concurrent-`UPDATE` lost-update) is fixed; `counter_mem` is correct.
- `histories/` holds a committed reference run (main @ `e1fa6eb`, 2026-06-01).
  Re-running `gen_177.sh` overwrites them.
