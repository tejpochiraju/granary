# Jepsen test harness build & run targets
#
# Usage:
#   make build       — Build OCaml harness inside dev container
#   make build-full  — Build Jepsen container (Java + Clojure + OCaml)
#   make run         — Run list-append workload (mem backend)
#   make run-file    — Run list-append workload (file backend)
#   make run-wal     — Run list-append workload (WAL backend)
#   make negative    — Generate negative-control histories
#   make check       — Run Clojure checker on generated history
#   make test        — Full: generate history + run checker

OCAML_HARNESS ?= sqlocaml_jepsen
HISTORY      ?= /tmp/sqlocaml_jepsen_history.edn
IMG_DEV      ?= sqlocaml-dev
IMG_JEPSEN   ?= sqlocaml-jepsen

# ---------- OCaml build targets ----------

.PHONY: build
build:
	@echo "=== Building OCaml Jepsen harness ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) dune build jepsen/ocaml/

.PHONY: run
run: build
	@echo "=== Running list-append workload (mem) ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--backend mem --workers 4 --ops 100 --keys 10 --history $(HISTORY)

.PHONY: run-file
run-file: build
	@echo "=== Running list-append workload (file) ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--backend file --path /tmp/sqlocaml_jepsen.db --workers 4 --ops 100 --keys 10 \
		--history $(HISTORY)

.PHONY: run-wal
run-wal: build
	@echo "=== Running list-append workload (WAL) ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--backend wal --path /tmp/sqlocaml_jepsen.db --workers 4 --ops 100 --keys 10 \
		--history $(HISTORY)

.PHONY: negative
negative: build
	@echo "=== Generating negative-control histories ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/negative_control.exe --

# ---------- Full Jepsen container build ----------

.PHONY: build-full
build-full:
	@echo "=== Building Jepsen container (Java + Clojure + OCaml) ==="
	podman build -t $(IMG_JEPSEN) -f Containerfile.jepsen $(PWD)/..

.PHONY: check
check:
	@echo "=== Running Clojure checker ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace/jepsen/clojure \
		$(IMG_JEPSEN) clojure -M -m jepsen.check $(HISTORY) -w list-append

# ---------- Full test ----------

.PHONY: test
test: build run check

.PHONY: test-negative
test-negative: negative
	@echo "=== Running Clojure checker on negative controls ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace/jepsen/clojure \
		$(IMG_JEPSEN) clojure -M -m jepsen.check /tmp/sqlocaml_negative_dirty_read.edn -w list-append; \
	echo "Exit code (should be 1 — anomaly detected): $$?"

.PHONY: test-negative-lost
test-negative-lost: negative
	@echo "=== Running Clojure checker on lost-update negative control ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace/jepsen/clojure \
		$(IMG_JEPSEN) clojure -M -m jepsen.check /tmp/sqlocaml_negative_lost_update.edn -w list-append; \
	echo "Exit code (should be 1 — anomaly detected): $$?"

.PHONY: clean
clean:
	rm -f /tmp/sqlocaml_jepsen*.edn /tmp/sqlocaml_jepsen*.db /tmp/sqlocaml_jepsen*.db-wal
