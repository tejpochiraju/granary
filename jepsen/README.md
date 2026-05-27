# Jepsen-style testing for sqlocaml

This directory contains a **Jepsen-style** concurrent workload and fault-injection
test suite for sqlocaml, following the approach outlined in issue #177.

## Architecture (Option 2)

Rather than running the full Jepsen distributed control plane, we use:

1. **OCaml harness** (`ocaml/`) — runs N concurrent Lwt worker fibers against
   a single in-memory or file-backed sqlocaml database, recording every
   operation (`:invoke` / `:ok` / `:fail`) into a Jepsen-format EDN history file.

2. **Clojure checker** (`clojure/`) — reads the EDN history offline and dispatches
   it to Elle's `list-append/check` (for isolation anomalies) and other Jepsen
   workload checkers (set, bank, counter).

3. **Nemeses** (planned) — fault injection via process pause, crash+restart,
   lazyfs (un-fsynced write loss), and clock skew.

## Quick Start

### Prerequisites

- Podman
- The `sqlocaml-dev` dev container (built from the project root Containerfile)
- Or build the Jepsen full container: `podman build -t sqlocaml-jepsen -f Containerfile.jepsen ..`

### Build the OCaml harness

```bash
cd /workspace
dune build jepsen/ocaml/
```

### Run the list-append workload

```bash
# In-memory backend (4 workers, 20 txns each, 5 keys)
opam exec -- dune exec jepsen/ocaml/harness.exe -- \
  --backend mem --workers 4 --ops 20 --keys 5 --history /tmp/history.edn

# File-backed
opam exec -- dune exec jepsen/ocaml/harness.exe -- \
  --backend file --path /tmp/test.db --workers 4 --ops 20 --keys 5 --history /tmp/history.edn

# WAL mode
opam exec -- dune exec jepsen/ocaml/harness.exe -- \
  --backend wal --path /tmp/test.db --workers 4 --ops 20 --keys 5 --history /tmp/history.edn
```

### Run the Clojure checker

```bash
cd /workspace/jepsen/clojure
clojure -M -m jepsen.check /tmp/history.edn -w list-append
```

### Negative controls

Prove the checker catches violations:

```bash
# Generate deliberately-broken histories
opam exec -- dune exec jepsen/ocaml/negative_control.exe --

# Check that Elle detects them (should exit 1)
clojure -M -m jepsen.check /tmp/sqlocaml_negative_dirty_read.edn -w list-append
clojure -M -m jepsen.check /tmp/sqlocaml_negative_lost_update.edn -w list-append
```

## CLI options (harness)

| Flag | Default | Description |
|------|---------|-------------|
| `--backend` | `mem` | Backend: `mem` \| `file` \| `wal` |
| `--path` | `/tmp/sqlocaml_jepsen.db` | DB file path (for file/wal) |
| `--workers` | `4` | Number of concurrent worker fibers |
| `--ops` | `100` | Operations per worker |
| `--keys` | `10` | Number of distinct keys |
| `--history` | `/tmp/sqlocaml_jepsen_history.edn` | Output EDN history path |

## File layout

```
jepsen/
├── README.md
├── Makefile                     # Convenience targets
├── Containerfile.jepsen         # Full Jepsen container (Java + Clojure + OCaml)
├── ocaml/
│   ├── dune
│   ├── edn_history.ml           # EDN format serialisation
│   ├── workload_list_append.ml  # List-append workload driver
│   ├── harness.ml              # Main entry point
│   └── negative_control.ml     # Deliberately-broken history generators
└── clojure/
    ├── deps.edn
    └── src/jepsen/
        └── check.clj           # Offline checker runner
```

## Phased rollout (per #177)

- [x] **Core (no faults):** OCaml harness + EDN recorder + Clojure Elle checker
- [ ] **Negative controls:** prove each checker catches its violation
- [ ] **More workloads:** bank, set, counter
- [ ] **Nemeses:** pause, crash, lazyfs, clock skew
- [ ] **CI wiring:** per-workload+nemesis gates
