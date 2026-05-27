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

# ---------- New workloads ----------

.PHONY: run-bank
run-bank: build
	@echo "=== Running bank workload ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--workload bank --backend mem --workers 4 --ops 100 --keys 5 \
		--history /tmp/sqlocaml_jepsen_bank.edn

.PHONY: run-set
run-set: build
	@echo "=== Running set workload ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--workload set --backend file --path /tmp/sqlocaml_jepsen_set.db \
		--workers 4 --ops 50 --history /tmp/sqlocaml_jepsen_set.edn

.PHONY: run-counter
run-counter: build
	@echo "=== Running counter workload ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--workload counter --backend mem --workers 4 --ops 200 --keys 5 \
		--history /tmp/sqlocaml_jepsen_counter.edn

# ---------- Nemesis runs ----------

.PHONY: run-crash
run-crash: build
	@echo "=== Running set workload with crash-restart ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--workload set --backend wal --path /tmp/sqlocaml_jepsen_crash.db \
		--nemesis crash-restart --crash-after 30 --workers 2 --ops 60 \
		--history /tmp/sqlocaml_jepsen_crash.edn

.PHONY: run-pause
run-pause: build
	@echo "=== Running list-append with process-pause ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--workload list-append --backend mem \
		--nemesis pause --pause-after 40 --pause-dur 3.0 --workers 4 --ops 100 \
		--history /tmp/sqlocaml_jepsen_pause.edn

.PHONY: run-lazyfs
run-lazyfs: build
	@echo "=== Running set workload with lazyfs (un-fsynced write loss) ==="
	@echo "Requires: --privileged or --device /dev/fuse for FUSE"
	podman run --rm --device /dev/fuse -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--workload set --backend wal --path /tmp/lazyfs_mount/test.db \
		--nemesis lazyfs --workers 2 --ops 40 \
		--history /tmp/sqlocaml_jepsen_lazyfs.edn

.PHONY: run-clock-skew
run-clock-skew: build
	@echo "=== Running list-append with clock-skew (faketime +5d) ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		-e FAKETIME="+5d" \
		-e LD_PRELOAD=/usr/lib/x86_64-linux-gnu/faketime/libfaketime.so.1 \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--workload list-append --backend mem \
		--nemesis clock-skew --workers 4 --ops 50 \
		--history /tmp/sqlocaml_jepsen_clockskew.edn

.PHONY: negative
negative: build
	@echo "=== Generating negative-control histories ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/negative_control.exe --

# ---------- Full Jepsen container build ----------

.PHONY: build-full
build-full:
	@echo "=== Building Jepsen container (Java + Clojure + OCaml) ==="
	podman build -t $(IMG_JEPSEN) -f jepsen/Containerfile.jepsen $(PWD)/..

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
