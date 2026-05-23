# Phase 44 — Cross-Platform Build: arm64 (#157)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Verified working build and green test run on `linux/arm64` in addition to `linux/amd64`, with both arches exercised by CI. Expectation: zero source-code changes — the codebase is pure OCaml with explicit endianness on disk. Deliverable is verification, CI plumbing, and a short docs note.

**Architecture:** No engine changes. The `ocaml/opam:ubuntu-24.04-ocaml-5.1` base image is already published multi-arch on Docker Hub, so `podman build --platform=linux/arm64` will pull the matching manifest. Local exercise on an x86 host uses `qemu-user-static`. CI gets an `arch: [amd64, arm64]` matrix, with the arm64 leg running under emulation initially.

**Tech Stack:** podman buildx, qemu-user-static, Forgejo Actions, dune, bisect_ppx. Modules touched: none under `lib/`; only `Containerfile`, `.forgejo/workflows/*`, `README.md`, possibly `CONTRIBUTING.md`.

---

## File Structure

**Modified:**
- `Containerfile` — header comment documenting multi-arch build commands.
- `.forgejo/workflows/ci.yml` — `arch: [amd64, arm64]` matrix; QEMU setup for arm64 leg.
- `.forgejo/workflows/coverage.yml` — verify it still works (arm64 leg may run a coverage parity check).
- `README.md` — multi-arch build note.

**Created (only if surface area justifies):**
- `CONTRIBUTING.md` — note about `_build/` arch sensitivity (don't share across arches). Or, if `CONTRIBUTING.md` already exists, append; if not, fold into `README.md`.

**Files that must change together:** Containerfile and CI workflow are independent and can be split.

---

## Build / test conventions (read me first)

- **All `dune` commands run inside podman**: `podman run --rm -v "$(pwd):/workspace:Z" -w /workspace sqlocaml-dev dune build`. For arm64, the image tag changes (`sqlocaml-dev:arm64`) but the command shape is identical.
- **Stage commits with `git add <files>`**, never `-A` (`_build/` is owned by root).
- **Forgejo CLI**: `~/.local/bin/forgejo issue …`, repo `tej/sqlite_ocaml_port`. Close via `forgejo issue edit tej/sqlite_ocaml_port 157 --state=closed`.
- **No emojis** in code or commits.

---

## Task 1 — Verify multi-arch container build locally

**Why:** Before changing CI, prove that `podman build --platform=linux/arm64` works against the existing Containerfile on a developer machine. If the base image or any apt package is unavailable on arm64, we want to know now.

**Files:** none modified in this task — output is a confirmation note.

### Step 1.1 — Install qemu-user-static (host)

- [ ] **On the host** (Ubuntu / Debian):

```bash
sudo apt-get update
sudo apt-get install -y qemu-user-static binfmt-support
```

Verify cross-arch emulation is registered:

```bash
ls /proc/sys/fs/binfmt_misc/ | grep -i aarch64
```

Expected: an `aarch64` (or `qemu-aarch64`) entry.

### Step 1.2 — Build the arm64 image

- [ ] **Build:**

```bash
podman build --platform=linux/arm64 -t sqlocaml-dev:arm64 -f Containerfile .
```

Expected: success. The build pulls the arm64 manifest of `ocaml/opam:ubuntu-24.04-ocaml-5.1` and installs `libgmp-dev` (apt is multi-arch).

If `opam install -y lwt cstruct menhir ...` fails on arm64, log which package and treat as a blocker — the issue's "no source changes expected" assumption only holds if every dep ships arm64 artifacts. None of the listed deps have known arm64 issues.

### Step 1.3 — Smoke `dune build` inside the arm64 image

- [ ] **Build inside the image:**

```bash
podman run --rm --platform=linux/arm64 -v "$(pwd):/workspace:Z" -w /workspace \
  sqlocaml-dev:arm64 dune build 2>&1 | tail -20
```

Expected: clean build. The first run under emulation is slow (10-30x x86 speed). Subsequent runs reuse `_build/default/.lock`.

### Step 1.4 — Record findings

- [ ] **Append to `README.md`** under the "Build" section, a "multi-arch" subsection:

```markdown
### Multi-arch (amd64 + arm64)

The base image `ocaml/opam:ubuntu-24.04-ocaml-5.1` is published
multi-arch.  Build for arm64 on an x86 host using qemu emulation:

```bash
sudo apt-get install -y qemu-user-static binfmt-support  # host
podman build --platform=linux/arm64 -t sqlocaml-dev:arm64 -f Containerfile .
podman run --rm --platform=linux/arm64 -v "$(pwd):/workspace:Z" \
  -w /workspace sqlocaml-dev:arm64 dune build
```

`_build/` artefacts are arch-specific.  Wipe `_build/` when switching
arches on the same checkout.
```

### Step 1.5 — Commit

- [ ] **Commit:**

```bash
git add README.md
git commit -m "$(cat <<'EOF'
docs(#157): document multi-arch build (amd64 + arm64)

podman + qemu commands for building the sqlocaml-dev image on either
arch, plus a note about _build/ arch-sensitivity.

Refs #157.

Co-Authored-By: Claude Opus 4.7 <noreply@anthropic.com>
EOF
)"
```

---

## Task 2 — Run the full test suite on arm64

**Why:** Build success doesn't prove tests pass. Encoding tests and QCheck fuzzers are the canaries for endianness or word-size regressions. The codebase declares endianness explicitly on disk (BE page headers, LE float64 in `row.ml`), so we expect no failures — but the test result is the evidence.

**Files:** none modified — output is a log captured in the issue.

### Step 2.1 — Run full runtest under arm64

- [ ] **Run** (this is slow under emulation — budget 10-30 minutes):

```bash
podman run --rm --platform=linux/arm64 -v "$(pwd):/workspace:Z" \
  -w /workspace sqlocaml-dev:arm64 dune runtest --force 2>&1 \
  | tee /tmp/arm64_runtest.log | tail -30
```

Expected: zero failures. If any test fails, **stop and triage** — endianness or word-size bugs are real, not a CI plumbing issue.

### Step 2.2 — Run the QCheck-heavy modules explicitly

- [ ] **QCheck modules are the highest-value canaries.** Re-run them with a higher count for confidence:

```bash
podman run --rm --platform=linux/arm64 -v "$(pwd):/workspace:Z" \
  -w /workspace sqlocaml-dev:arm64 \
  bash -c "QCHECK_MSG_INTERVAL=0 _build/default/test/test_pager.exe --verbose"
```

(Substitute any other QCheck-bearing exes — `test_btree.exe`, `test_index_key.exe`, etc.)

Expected: passes; printed counts > 100 per property.

### Step 2.3 — Comment findings on #157

- [ ] **Post a comment** to #157 with the runtest summary (test count + duration):

```bash
~/.local/bin/forgejo issue comment tej/sqlite_ocaml_port 157 --body "Phase 44 progress: arm64 full \`dune runtest --force\` PASS under qemu emulation.

Duration: <X> minutes.  All N tests passed, no endianness regressions.  QCheck
canaries (test_pager, test_btree, test_index_key) re-run individually at the
default count, all green."
```

No commit for this task — the deliverable is the comment.

---

## Task 3 — Coverage parity check on arm64

**Why:** The coverage workflow uses the manual binary loop pattern (per `[[feedback-coverage-generation]]`). Verify it still produces a valid `_cov_*.xml` on arm64.

**Files:** none modified.

### Step 3.1 — Run coverage on arm64

- [ ] **Run:**

```bash
podman run --rm --platform=linux/arm64 -v "$(pwd):/workspace:Z" \
  -w /workspace sqlocaml-dev:arm64 \
  bash -c "dune build --instrument-with bisect_ppx --force; \
           for exe in _build/default/test/test_*.exe; do \$exe || true; done; \
           bisect-ppx-report cobertura -o _cov_cobertura_arm64.xml"
ls -la _cov_cobertura_arm64.xml
head -5 _cov_cobertura_arm64.xml
```

Expected: XML produced, line counts non-zero, comparable to amd64.

- [ ] **Compare** against the latest amd64 coverage report:

```bash
grep -oE 'line-rate="[0-9.]+"' _cov_cobertura.xml _cov_cobertura_arm64.xml
```

The two `line-rate` values should match to ≥ 0.001. Differences mean either non-determinism in the suite (look for time-of-day or randomness) or a real arch divergence (investigate).

### Step 3.2 — Comment on #157

- [ ] **Comment** with the diff. No commit — `_cov_*.xml` is gitignored.

---

## Task 4 — CI matrix: `arch: [amd64, arm64]`

**Why:** The whole point of #157. Local verification is great but only CI keeps it green long-term.

**Files:**
- Modify: `.forgejo/workflows/ci.yml`

### Step 4.1 — Add the matrix

- [ ] **Edit `.forgejo/workflows/ci.yml`** — restructure `build-and-test` to use a matrix:

```yaml
jobs:
  build-and-test:
    runs-on: ubuntu-latest
    strategy:
      fail-fast: false
      matrix:
        arch: [amd64, arm64]
    steps:
      - uses: actions/checkout@v4

      - name: Set up QEMU (arm64 leg only)
        if: matrix.arch == 'arm64'
        uses: docker/setup-qemu-action@v3
        with:
          platforms: arm64

      - name: Install system deps
        run: |
          sudo apt-get update -q
          sudo apt-get install -y -q pkg-config libgmp-dev

      - name: Cache opam packages
        uses: actions/cache@v4
        with:
          path: ~/.opam
          key: opam-5.1-${{ matrix.arch }}-${{ hashFiles('sqlocaml.opam') }}
          restore-keys: opam-5.1-${{ matrix.arch }}-

      - name: Build sqlocaml-dev image
        run: |
          podman build --platform=linux/${{ matrix.arch }} \
            -t sqlocaml-dev:${{ matrix.arch }} -f Containerfile .

      - name: Build
        run: |
          podman run --rm --platform=linux/${{ matrix.arch }} \
            -v "${{ github.workspace }}:/workspace:Z" -w /workspace \
            sqlocaml-dev:${{ matrix.arch }} dune build

      - name: Test
        run: |
          podman run --rm --platform=linux/${{ matrix.arch }} \
            -v "${{ github.workspace }}:/workspace:Z" -w /workspace \
            sqlocaml-dev:${{ matrix.arch }} dune runtest

      - name: Report test count
        run: |
          for exe in _build/default/test/test_*.exe; do
            name=$(basename "$exe" .exe)
            result=$("$exe" 2>&1 | tail -1)
            echo "[${{ matrix.arch }}] $name: $result"
          done
```

Notes:
- If `docker/setup-qemu-action` isn't available in Forgejo Actions, install qemu via apt directly:
  ```yaml
  - name: Install QEMU
    if: matrix.arch == 'arm64'
    run: sudo apt-get install -y -q qemu-user-static binfmt-support
  ```
- The current workflow uses the bare `ocaml/opam` container as the job container. We're switching to building our own image inside the job so the matrix can target a platform. If the Forgejo runner's storage is tight, fall back to a host opam install but keep the platform discipline.

### Step 4.2 — Push to a feature branch and watch the matrix

- [ ] **Push:**

```bash
git checkout -b phase-44-ci-matrix
git add .forgejo/workflows/ci.yml
git commit -m "$(cat <<'EOF'
ci(#157): arch matrix [amd64, arm64]

amd64 leg runs natively, arm64 under QEMU emulation.  Builds the
sqlocaml-dev image per-arch and runs dune build + dune runtest inside
the container.

Refs #157.

Co-Authored-By: Claude Opus 4.7 <noreply@anthropic.com>
EOF
)"
git push -u origin phase-44-ci-matrix
```

- [ ] **Watch the run:**

```bash
~/.local/bin/forgejo actions list tej/sqlite_ocaml_port
~/.local/bin/forgejo actions view tej/sqlite_ocaml_port <run_id>
```

Expected: both legs green. arm64 leg will take 10-30× longer than amd64.

### Step 4.3 — Record arm64 leg runtime in the issue

- [ ] **Comment** on #157 with runtime numbers:

```bash
~/.local/bin/forgejo issue comment tej/sqlite_ocaml_port 157 --body "CI matrix online.  amd64 leg: <Xm>; arm64 leg (QEMU emulation): <Ym>.  Per-arch runtimes are recorded so we can later decide whether to invest in a native arm64 runner."
```

### Step 4.4 — Merge

- [ ] **Open PR, get green CI, merge:**

```bash
~/.local/bin/forgejo pr create tej/sqlite_ocaml_port \
  --title "ci: arm64 matrix (#157)" \
  --head phase-44-ci-matrix \
  --base main \
  --body "Closes #157. amd64 native, arm64 under QEMU."
```

(Single-author repo, so feel free to merge directly via main if PR workflow isn't required.)

---

## Task 5 — Optional MirageOS-on-arm64 audit

**Why:** Issue acceptance lists `mirage configure -t hvt` on aarch64 as a stretch goal. The `unix` target is the primary deliverable. If we have time, prove the hvt-arm64 path also works; if not, file a follow-up.

**Files:** none unless follow-up is filed.

### Step 5.1 — Attempt mirage configure on arm64

- [ ] **From inside the arm64 container:**

```bash
podman run --rm --platform=linux/arm64 -v "$(pwd):/workspace:Z" \
  -w /workspace sqlocaml-dev:arm64 \
  bash -c "opam install -y mirage && cd lib/mirage_block && mirage configure -t hvt" 2>&1 | tail -30
```

Likely outcomes:
- Works → smoke complete, comment on #157.
- Fails on `mirage` opam install → arm64 Mirage stack has issues; file follow-up.
- Fails at configure with hvt-arm64-specific error → file follow-up with a snippet.

### Step 5.2 — File follow-up if needed

- [ ] **If it fails:**

```bash
~/.local/bin/forgejo issue create tej/sqlite_ocaml_port \
  --title "mirage: hvt-arm64 build path needs investigation (followup #157)" \
  --body "Phase 44 verified amd64 + arm64 for the unix target.  hvt-arm64 \
  failed at <step>: <excerpt>.  Reproduce with: <command>."
```

No commit for this task either way.

---

## Task 6 — Close issue

- [ ] **Push the lint/CI branch to main if not done.**

- [ ] **Close #157:**

```bash
~/.local/bin/forgejo issue edit tej/sqlite_ocaml_port 157 --state=closed
~/.local/bin/forgejo issue comment tej/sqlite_ocaml_port 157 --body "Closed in phase 44:
- Containerfile multi-arch build verified locally (amd64 + arm64).
- Full runtest + QCheck canaries pass on arm64 under QEMU; coverage parity confirmed.
- CI matrix \`arch: [amd64, arm64]\` is live in .forgejo/workflows/ci.yml.
- README documents the arm64 build command.
- MirageOS hvt-arm64 audit: <status>.  Follow-up filed as #<NN> (if applicable)."
```

---

## Out of scope (for this phase)

- 32-bit ARM (`armv7`).
- Windows / Alpine / musl portability.
- Native (non-emulated) arm64 runner. Decision deferred until the arm64 leg's emulation runtime becomes painful in practice.
- Performance tuning on arm64.

## Acceptance summary

- [ ] `podman build --platform=linux/arm64` succeeds against the existing Containerfile.
- [ ] Full `dune runtest` passes on arm64 under emulation.
- [ ] Coverage report generated on arm64 has line-rate parity with amd64.
- [ ] CI matrix `arch: [amd64, arm64]` is green on main.
- [ ] README documents multi-arch build.
- [ ] #157 closed with arm64 leg runtime recorded.
