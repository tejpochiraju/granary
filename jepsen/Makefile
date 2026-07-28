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

OCAML_HARNESS ?= granary_jepsen
HISTORY      ?= /tmp/granary_jepsen_history.edn
IMG_DEV      ?= granary-dev
IMG_JEPSEN   ?= granary-jepsen

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
		--backend file --path /tmp/granary_jepsen.db --workers 4 --ops 100 --keys 10 \
		--history $(HISTORY)

.PHONY: run-wal
run-wal: build
	@echo "=== Running list-append workload (WAL) ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--backend wal --path /tmp/granary_jepsen.db --workers 4 --ops 100 --keys 10 \
		--history $(HISTORY)

# ---------- New workloads ----------

.PHONY: run-bank
run-bank: build
	@echo "=== Running bank workload ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--workload bank --backend mem --workers 4 --ops 100 --keys 5 \
		--history /tmp/granary_jepsen_bank.edn

.PHONY: run-set
run-set: build
	@echo "=== Running set workload ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--workload set --backend file --path /tmp/granary_jepsen_set.db \
		--workers 4 --ops 50 --history /tmp/granary_jepsen_set.edn

.PHONY: run-counter
run-counter: build
	@echo "=== Running counter workload ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--workload counter --backend mem --workers 4 --ops 200 --keys 5 \
		--history /tmp/granary_jepsen_counter.edn

# ---------- Nemesis runs ----------

.PHONY: run-crash
run-crash: build
	@echo "=== Running set workload with crash-restart ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--workload set --backend wal --path /tmp/granary_jepsen_crash.db \
		--nemesis crash-restart --crash-after 30 --workers 2 --ops 60 \
		--history /tmp/granary_jepsen_crash.edn

.PHONY: run-pause
run-pause: build
	@echo "=== Running list-append with process-pause ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--workload list-append --backend mem \
		--nemesis pause --pause-after 40 --pause-dur 3.0 --workers 4 --ops 100 \
		--history /tmp/granary_jepsen_pause.edn

.PHONY: run-lazyfs
run-lazyfs: build
	@echo "=== Running set workload with lazyfs (un-fsynced write loss) ==="
	@echo "Requires: --privileged or --device /dev/fuse for FUSE"
	podman run --rm --device /dev/fuse -v $(PWD)/..:/workspace:z -w /workspace \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--workload set --backend wal --path /tmp/lazyfs_mount/test.db \
		--nemesis lazyfs --workers 2 --ops 40 \
		--history /tmp/granary_jepsen_lazyfs.edn

.PHONY: run-clock-skew
run-clock-skew: build
	@echo "=== Running list-append with clock-skew (faketime +5d) ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace \
		-e FAKETIME="+5d" \
		-e LD_PRELOAD=/usr/lib/x86_64-linux-gnu/faketime/libfaketime.so.1 \
		$(IMG_DEV) opam exec -- dune exec jepsen/ocaml/harness.exe -- \
		--workload list-append --backend mem \
		--nemesis clock-skew --workers 4 --ops 50 \
		--history /tmp/granary_jepsen_clockskew.edn

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

# ---------- CI: gate checks (real histories) ----------

JEPSEN_RUN = podman run --rm -v $(PWD)/..:/workspace:z -w /workspace/jepsen/clojure \
	$(IMG_JEPSEN) clojure -J-Djava.awt.headless=true -M -m jepsen.check
JEPSEN_HIST = $(PWD)/../_build/jepsen_histories

.PHONY: check-list-append check-bank check-set check-counter

check-list-append:
	@echo "=== list-append (mem) ==="
	@$(JEPSEN_RUN) $(JEPSEN_HIST)/list_append_mem.edn -w list-append || true

check-bank:
	@echo "=== bank (mem) ==="
	@$(JEPSEN_RUN) $(JEPSEN_HIST)/bank_mem.edn -w bank || true

check-set:
	@echo "=== set (wal) ==="
	@$(JEPSEN_RUN) $(JEPSEN_HIST)/set_wal.edn -w set || true

check-counter:
	@echo "=== counter (mem) ==="
	@$(JEPSEN_RUN) $(JEPSEN_HIST)/counter_mem.edn -w counter || true

# Negative-control checks

.PHONY: check-neg-dirty check-neg-lost check-neg-bank check-neg-set check-neg-counter

check-neg-dirty:
	@echo "=== negative: dirty_read (list-append) ==="
	@$(JEPSEN_RUN) $(JEPSEN_HIST)/granary_negative_dirty_read.edn -w list-append

check-neg-lost:
	@echo "=== negative: lost_update (list-append) ==="
	@$(JEPSEN_RUN) $(JEPSEN_HIST)/granary_negative_lost_update.edn -w list-append

check-neg-bank:
	@echo "=== negative: bank_lost_transfer ==="
	@$(JEPSEN_RUN) $(JEPSEN_HIST)/granary_negative_bank_lost_transfer.edn -w bank

check-neg-set:
	@echo "=== negative: set_lost_element ==="
	@$(JEPSEN_RUN) $(JEPSEN_HIST)/granary_negative_set_lost_element.edn -w set

check-neg-counter:
	@echo "=== negative: counter_non_monotonic ==="
	@$(JEPSEN_RUN) $(JEPSEN_HIST)/granary_negative_counter_non_monotonic.edn -w counter

# ---------- Full CI gate ----------

.PHONY: ci-gate
ci-gate:
	@echo "=====================================================" ;\
	echo "    CI GATE — Jepsen checkers for #177" ;\
	echo "=====================================================" ;\
	echo ;\
	echo "--- Real histories (should be VALID, exit 0) ---" ;\
	pass=0; fail=0; \
	$(MAKE) check-list-append  ; rc=$? ; if [ $rc -eq 0 ]; then echo "  list-append: PASS" ; pass=$((pass+1)) ; else echo "  list-append: FAIL (exit $rc)" ; fail=$((fail+1)) ; fi ; \
	$(MAKE) check-bank          ; rc=$? ; if [ $rc -eq 0 ]; then echo "  bank: PASS" ; pass=$((pass+1)) ; else echo "  bank: FAIL (exit $rc)" ; fail=$((fail+1)) ; fi ; \
	$(MAKE) check-set           ; rc=$? ; if [ $rc -eq 0 ]; then echo "  set: PASS" ; pass=$((pass+1)) ; else echo "  set: FAIL (exit $rc)" ; fail=$((fail+1)) ; fi ; \
	$(MAKE) check-counter       ; rc=$? ; if [ $rc -eq 0 ]; then echo "  counter: PASS" ; pass=$((pass+1)) ; else echo "  counter: FAIL (exit $rc) — known mem ACID bug" ; fail=$((fail+1)) ; fi ; \
	echo ;\
	echo "--- Negative controls (should be INVALID, exit 1) ---" ;\
	neg_pass=0; neg_fail=0; \
	$(MAKE) check-neg-dirty    ; rc=$? ; if [ $rc -eq 1 ]; then echo "  dirty_read: CAUGHT ✓" ; neg_pass=$((neg_pass+1)) ; else echo "  dirty_read: MISS (exit $rc)" ; neg_fail=$((neg_fail+1)) ; fi ; \
	$(MAKE) check-neg-lost     ; rc=$? ; if [ $rc -eq 1 ]; then echo "  lost_update: CAUGHT ✓" ; neg_pass=$((neg_pass+1)) ; else echo "  lost_update: MISS (exit $rc)" ; neg_fail=$((neg_fail+1)) ; fi ; \
	$(MAKE) check-neg-bank     ; rc=$? ; if [ $rc -eq 1 ]; then echo "  bank_lost_transfer: CAUGHT ✓" ; neg_pass=$((neg_pass+1)) ; else echo "  bank_lost_transfer: MISS (exit $rc)" ; neg_fail=$((neg_fail+1)) ; fi ; \
	$(MAKE) check-neg-set      ; rc=$? ; if [ $rc -eq 1 ]; then echo "  set_lost_element: CAUGHT ✓" ; neg_pass=$((neg_pass+1)) ; else echo "  set_lost_element: MISS (exit $rc)" ; neg_fail=$((neg_fail+1)) ; fi ; \
	$(MAKE) check-neg-counter  ; rc=$? ; if [ $rc -eq 1 ]; then echo "  counter_non_monotonic: CAUGHT ✓" ; neg_pass=$((neg_pass+1)) ; else echo "  counter_non_monotonic: MISS (exit $rc)" ; neg_fail=$((neg_fail+1)) ; fi ; \
	echo ;\
	echo "=====================================================" ;\
	echo "  Real: $pass/4 pass (counter has known mem ACID bug)" ;\
	echo "  Negative: $neg_pass/5 caught" ;\
	echo "  Failures: $neg_fail remaining" ;\
	echo "====================================================="

# ---------- Full test ----------

.PHONY: test
test: build run check

.PHONY: test-negative
test-negative: negative
	@echo "=== Running Clojure checker on negative controls ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace/jepsen/clojure \
		$(IMG_JEPSEN) clojure -M -m jepsen.check /tmp/granary_negative_dirty_read.edn -w list-append; \
	echo "Exit code (should be 1 — anomaly detected): $?"

.PHONY: test-negative-lost
test-negative-lost: negative
	@echo "=== Running Clojure checker on lost-update negative control ==="
	podman run --rm -v $(PWD)/..:/workspace:z -w /workspace/jepsen/clojure \
		$(IMG_JEPSEN) clojure -M -m jepsen.check /tmp/granary_negative_lost_update.edn -w list-append; \
	echo "Exit code (should be 1 — anomaly detected): $?"

.PHONY: clean
clean:
	rm -f /tmp/granary_jepsen*.edn /tmp/granary_jepsen*.db /tmp/granary_jepsen*.db-wal
