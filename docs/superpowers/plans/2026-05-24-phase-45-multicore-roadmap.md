# Phase 45 — Multicore Roadmap (#156)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to execute this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Decompose the `Domain.spawn`-based physical-multicore epic (#156) into a sequence of self-contained, shippable sub-issues so future phases can pick them off one at a time without each having to re-litigate the epic-level decisions. This phase produces **no implementation code** — its deliverables are filed sub-issues, an architectural decision record, and a roadmap doc.

**Architecture:** #156 calls for moving from Lwt's cooperative single-domain interleaving (delivered in Phase 38 via #149) to true multi-domain parallelism. The work splits along five axes: (1) a domain-safety audit of every shared mutable structure; (2) a decision between pinned-pager-on-one-domain vs sharded-shared-pager; (3) a Lwt-vs-Eio choice for the runtime; (4) reader-worker pool plumbing; (5) a bench that actually measures multi-domain throughput. Each axis is one or two sub-issues. We file them all here so the epic stops being a black box.

**Tech Stack:** None implementation-wise. Documentation + Forgejo issues. Reference reading: OCaml 5.x `Domain` module, `Domainslib.Chan`, `Saturn` lock-free structures, `Lwt_domain.detach`, `Eio` (post-OCaml-5 effects-based runtime).

---

## File Structure

**Created:**
- `docs/MULTICORE_ROADMAP.md` — top-level roadmap, decision points, sub-issue map.
- `docs/adr/0001-multicore-runtime-choice.md` — ADR template; final answer recorded later, in the phase that picks the runtime.

**Modified:**
- None.

**Forgejo issues created:** 5-6 sub-issues, all referencing #156 and labelled `epic-156-multicore` (label created in Task 1).

---

## Build / test conventions (read me first)

- **No code in this phase**, so no `dune` commands.
- **Forgejo CLI**: `~/.local/bin/forgejo issue …`, repo `tej/sqlite_ocaml_port`. To create labels:
  ```bash
  ~/.local/bin/forgejo issue label tej/sqlite_ocaml_port create \
    --name "epic-156-multicore" --color "5319e7" \
    --desc "Sub-issues of #156 (Domain.spawn multicore)" --scope=repo
  ```
- **No emojis** in docs or issue bodies.

---

## Task 1 — Create epic label + scaffolding

**Why:** Group all sub-issues under one label so they're filterable later.

**Files:** none — just Forgejo CLI work.

### Step 1.1 — Create the label

- [ ] **Run:**

```bash
~/.local/bin/forgejo issue label tej/sqlite_ocaml_port create \
  --name "epic-156-multicore" --color "5319e7" \
  --desc "Sub-issues of #156 (Domain.spawn multicore)" --scope=repo
```

If the label already exists, skip (Forgejo returns 409; ignore).

### Step 1.2 — Tag the umbrella issue

- [ ] **Tag #156** with the new label:

```bash
~/.local/bin/forgejo issue label tej/sqlite_ocaml_port add 156 \
  --labels "epic-156-multicore"
```

---

## Task 2 — Write the roadmap doc

**Why:** A single doc is much faster to skim than re-reading the epic + 5 sub-issues every time we plan a phase. The doc also captures decision points that don't belong on any one sub-issue.

**Files:**
- Create: `docs/MULTICORE_ROADMAP.md`

### Step 2.1 — Draft

- [ ] **Create `docs/MULTICORE_ROADMAP.md`:**

```markdown
# Multicore Roadmap (#156)

This document tracks the path from sqlocaml's current single-domain
cooperative-Lwt concurrency (Phase 38, #149) to true multi-domain
physical parallelism (the umbrella epic #156).

It is **not** a phase plan — each numbered sub-issue below becomes its
own phase when scheduled. This doc captures cross-cutting decisions
that span those phases.

## Why now (or later)

Phase 38 (#149) gave us shared-read / exclusive-write concurrency on
Lwt's cooperative scheduler. Reader fibers and writer fsync now
overlap, which is the right answer for I/O-bound workloads. But there
is no physical CPU parallelism: a workload that bottlenecks on
query-planning, predicate evaluation, expression interpretation, or
B+-tree key compare cannot scale past one domain.

For typical MirageOS unikernel deployments (one Mirage tenant per
core or fractional core) this is fine. For the small-host hosting
plan (`[[project-hosting-infra]]` — ~10 sqlocaml apps per ₹2K box)
single-domain is the natural fit.

So #156 is **not urgent**. It is filed so the option exists on the
radar when someone profiles a CPU-bound workload and finds the Lwt
scheduler is the bottleneck. This roadmap exists so that when the
time comes, we're not starting from a blank page.

## Decision points (these gate everything else)

### D1. Lwt-on-multicore vs Eio rewrite

| Option | Pros | Cons |
|--------|------|------|
| Lwt + `Lwt_domain.detach` | Minimal disruption; every existing fiber keeps working. Lwt 5.7+ has the affordance. | Lwt's single-event-loop assumption leaks; cross-domain bind/promise plumbing is awkward. |
| Eio | Aligned with OCaml 5 effects-based concurrency; cleaner multi-domain model. | Major rewrite — touches every `Lwt.t`-typed function in `lib/`. |

**Default assumption (to be re-evaluated in the picking phase):**
start with Lwt + `Lwt_domain.detach` for the first measurable scaling
win; revisit Eio only if the Lwt path hits a structural wall we can't
work around.

### D2. Pager architecture: pinned-to-one-domain vs sharded-shared

| Option | Pros | Cons |
|--------|------|------|
| Pinned (pager-on-one-domain) | Simple. One domain owns the cache, freelist, WAL. All mutation lives behind a request queue. | One pager domain is a serial bottleneck for cache hits. Cross-domain latency added to every page read. |
| Sharded shared | Per-shard `Mutex.t` allows parallel hits. Better worst-case throughput. | Many more invariants to maintain; eviction across shards becomes its own coordination problem. |

**Default assumption:** start pinned. It's the smallest behavioural
change. If profiling shows the pager domain becoming the bottleneck,
revisit sharding.

### D3. Snapshot capture: main-domain vs worker

Snapshots are cheap (just a frame index + meta-root int). Capturing
on the main domain before dispatching to the worker keeps `Wal` and
`Pager` mutation single-domain — preferred.

## Sub-issue map

Numbers below are filed by Task 3 of this phase. Status as of phase 45
creation: all **draft** (filed open, not yet scheduled into a phase).

| # | Title | Depends on |
|---|-------|------------|
| 45-A | Domain-safety audit: catalog every shared mutable structure | — |
| 45-B | Choose runtime: Lwt+Lwt_domain or Eio (D1) | 45-A |
| 45-C | Pager threading model (D2) — implementation phase 1 | 45-A, 45-B |
| 45-D | Reader-worker pool via Lwt_domain.detach | 45-B, 45-C |
| 45-E | Bench: `bench_multi_domain_reader_scaling` | 45-D |
| 45-F | Cross-domain WAL append + checkpoint coordination | 45-C |

## Acceptance criteria for the epic as a whole

- A reader-heavy workload (e.g. `bench_wal_reader_scaling`-style with
  N domains × M reads each) scales close to `min(N, host_cores)`.
- Every existing concurrency property test passes under the multicore
  runtime.
- No regression in single-domain workloads (the existing `bench_wal_fsync_overlap`
  speedup floor still clears).
- Documentation: this roadmap is updated to reference the phases that
  delivered each sub-issue.

## Out of scope (for the epic)

- Multi-process concurrency (shm + mmapped wal-index). Different
  problem; covered by #149's out-of-scope note.
- GPU-offloaded query execution. Not relevant.
- Rewriting any module purely for "domain-cleanliness" if no real
  scaling benefit follows.

## References

- #149 (delivered cooperative-only concurrency)
- #155 (fsync-overlap bench; single-domain measurement)
- OCaml `Domain` module: <https://v2.ocaml.org/manual/parallelism.html>
- Eio: <https://github.com/ocaml-multicore/eio>
- Domainslib: <https://github.com/ocaml-multicore/domainslib>
- Saturn (lock-free structures): <https://github.com/ocaml-multicore/saturn>
```

### Step 2.2 — Add an ADR placeholder

- [ ] **Create `docs/adr/0001-multicore-runtime-choice.md`:**

```markdown
# ADR 0001 — Multicore runtime choice (Lwt vs Eio)

Status: **proposed** (deferred). To be filled in by the phase that
implements sub-issue 45-B.

## Context

(See `docs/MULTICORE_ROADMAP.md` decision D1.)

## Decision

(unset)

## Consequences

(unset)
```

### Step 2.3 — Commit

- [ ] **Commit:**

```bash
git add docs/MULTICORE_ROADMAP.md docs/adr/0001-multicore-runtime-choice.md
git commit -m "$(cat <<'EOF'
docs(#156): multicore roadmap + ADR placeholder

Captures the decision points (Lwt vs Eio, pinned vs sharded pager,
snapshot capture domain) so future phases can pick sub-issues off
without re-litigating the epic.

Refs #156.

Co-Authored-By: Claude Opus 4.7 <noreply@anthropic.com>
EOF
)"
```

---

## Task 3 — File the sub-issues

**Why:** With the roadmap doc in place, each sub-issue is a small, scoped task whose body can link back to the doc. Filing them up-front means the work is enumerable when someone scans open issues.

**Files:** none — Forgejo CLI work.

### Step 3.1 — File sub-issue A: domain-safety audit

- [ ] **Create:**

```bash
~/.local/bin/forgejo issue create tej/sqlite_ocaml_port \
  --title "epic 156 / A: domain-safety audit of shared mutable structures" \
  --body "Part of #156 (see docs/MULTICORE_ROADMAP.md).

## Goal

Enumerate every shared mutable structure on the Pager / Store / Btree
path and classify each as:

- **Pin-to-one-domain**: cache, freelist, WAL index, dirty set.  Other
  domains access via message queue.
- **Lock-free / immutable post-construction**: per-snapshot trees
  (\`rs_snap_trees\`), the snapshot record itself.
- **Replace with thread-safe equivalent**: \`Domainslib.Chan\` or
  \`Saturn.Queue\` candidates.

## Files to audit

- \`lib/storage/pager.ml\` — every \`Hashtbl\`, \`Queue\`, \`mutable\` field.
- \`lib/storage/wal.ml\` — index, committed_frames counter, etc.
- \`lib/storage/freelist.ml\` — the free list itself.
- \`lib/store/store.ml\` — active_readers, active_reader_frames,
  reader_done_cond, rs_pin, mem_savepoints, txn_freelist_snapshot.
- \`lib/storage/btree.ml\` — cursor state, in-memory tree handles.

## Deliverable

A markdown table inside docs/MULTICORE_ROADMAP.md under a new
'Audit' section, one row per structure, with classification +
brief rationale.

No code changes in this issue — just the audit.

## Acceptance

- [ ] Every shared mutable field in the above modules has a row.
- [ ] Each row picks a classification (pin / lock-free / replace).
- [ ] Followups for non-trivial 'replace' classifications are filed
      as separate issues.

Refs #156." \
  --labels "epic-156-multicore"
```

### Step 3.2 — Sub-issue B: runtime choice

- [ ] **Create:**

```bash
~/.local/bin/forgejo issue create tej/sqlite_ocaml_port \
  --title "epic 156 / B: runtime choice — Lwt+Lwt_domain vs Eio (ADR-0001)" \
  --body "Part of #156 (see docs/MULTICORE_ROADMAP.md and docs/adr/0001-multicore-runtime-choice.md).

## Goal

Pick the multi-domain runtime model and write the decision into the
ADR.

## Reading

- docs/MULTICORE_ROADMAP.md decision D1.
- Lwt_domain API: \`Lwt_domain.detach\`, \`Lwt_domain.bind\`.
- Eio rationale: 'Why Eio?' essay.

## Method

1. Prototype: write a 200-line spike implementing a reader-worker
   pool for a single SELECT against an open snapshot, once with
   Lwt+Lwt_domain, once with Eio.  Throw both away — the point is to
   surface friction.
2. Compare on three axes:
   - Lines of code touched in \`lib/\`.
   - Pleasantness of the cross-domain bind/promise plumbing.
   - Bench throughput on the prototype (smoke only, not the real bench).
3. Write up findings in the ADR.

## Acceptance

- [ ] Spike code (kept on a throwaway branch, not merged).
- [ ] ADR 0001 status changed from 'proposed' to 'accepted'.
- [ ] Roadmap doc updated to reference the chosen runtime.

Refs #156.

Depends-on: A (audit informs runtime choice)." \
  --labels "epic-156-multicore"
```

### Step 3.3 — Sub-issue C: pager threading

- [ ] **Create:**

```bash
~/.local/bin/forgejo issue create tej/sqlite_ocaml_port \
  --title "epic 156 / C: pager threading model (pinned-to-one-domain)" \
  --body "Part of #156 (see docs/MULTICORE_ROADMAP.md decision D2).

## Goal

Move the Pager (cache, freelist, WAL index, dirty set) onto a single
dedicated 'pager domain'.  Reader and writer requests cross the
domain boundary via a request queue.

## Scope

- New module \`lib/storage/pager_dispatcher.ml\`: owns the pager,
  exposes a thread-safe request API.
- Per-request types: \`Read of {page_id; snapshot_frames; pin; reply}\`,
  \`Write of {page_id; buf; reply}\`, etc.
- Cross-domain replies via \`Lwt_domain.detach\` (or Eio promises,
  pending B).
- All existing callers (\`Store\`, \`Btree\`) updated to go through the
  dispatcher.

## Acceptance

- [ ] Pager state is owned by exactly one domain at runtime.
- [ ] All existing tests pass (no behavioural change).
- [ ] A new test 'two domains hitting the dispatcher simultaneously'
      stresses the queue under contention.
- [ ] Single-domain read latency does not regress more than 15%
      (measured against the cross-domain hop overhead).

## Non-goal

Sharded shared pager (decision D2 picks pinned first; sharded is a
later sub-issue if we hit a wall).

Refs #156.  Depends-on: A, B." \
  --labels "epic-156-multicore"
```

### Step 3.4 — Sub-issue D: reader-worker pool

- [ ] **Create:**

```bash
~/.local/bin/forgejo issue create tej/sqlite_ocaml_port \
  --title "epic 156 / D: reader-worker domain pool" \
  --body "Part of #156.

## Goal

\`Db.query\` enqueues SELECTs onto a fixed-size pool of worker
domains.  Each worker holds its own Lwt event loop; the query
executes against a snapshot captured on the main domain.

## Scope

- New module \`lib/db/worker_pool.ml\`.
- \`Db.query\` becomes: capture snapshot on main, dispatch to worker,
  return an \`Lwt_stream\` populated by the worker.
- Worker count default: \`Domain.recommended_domain_count () - 1\`,
  capped at a configurable max.
- Snapshot release happens on the main domain (matches roadmap D3).

## Acceptance

- [ ] Pool sized correctly on machines with 1, 2, 4, 8+ cores.
- [ ] Existing single-domain workloads have no measurable regression
      (workers idle).
- [ ] A multi-fiber test exercises the cross-domain stream backpressure.

Refs #156.  Depends-on: B, C." \
  --labels "epic-156-multicore"
```

### Step 3.5 — Sub-issue E: scaling bench

- [ ] **Create:**

```bash
~/.local/bin/forgejo issue create tej/sqlite_ocaml_port \
  --title "epic 156 / E: bench_multi_domain_reader_scaling" \
  --body "Part of #156.

## Goal

Extend \`bench_wal_reader_scaling\` so the parallel run dispatches to
the worker pool from sub-issue D.  Target: ratio approaches
\`min(N, host_cores)\` for read-heavy workloads.

## Scope

- \`test/bench_multi_domain_reader_scaling.ml\`.
- Env var \`SQLOCAML_BENCH_DOMAINS\` selects how many workers.
- Compares sequential, multi-fiber-single-domain (the current
  baseline), and multi-domain throughput.

## Acceptance

- [ ] On a 4-core machine, ratio of multi-domain / sequential is
      between 2.5x and 4x.
- [ ] On a 1-core machine, multi-domain is no worse than 1.0x
      (no regression from the cross-domain hops when only one core
      is available).

Refs #156.  Depends-on: D." \
  --labels "epic-156-multicore"
```

### Step 3.6 — Sub-issue F: cross-domain WAL coordination

- [ ] **Create:**

```bash
~/.local/bin/forgejo issue create tej/sqlite_ocaml_port \
  --title "epic 156 / F: cross-domain WAL append + checkpoint coordination" \
  --body "Part of #156.

## Goal

The writer's WAL append and the background checkpointer (Phase 38)
both touch shared state.  When workers can also read across domains,
the snapshot frame bound, the active_reader_frames table, and the
checkpoint waiter need to be cross-domain safe.

## Scope

- Decide whether checkpoint stays on the pager domain (likely) or
  moves to its own domain.
- Add a domain-safe primitive (probably Mutex.t-protected) for
  \`active_reader_frames\` if the audit (sub-issue A) flags it.
- Ensure \`Lwt_condition.broadcast\` semantics are preserved across
  domains, or replace with \`Domainslib.Chan\`-based signalling.

## Acceptance

- [ ] Existing \`test_crash_property\` and \`test_multifiber_stress\`
      pass under the multicore runtime.
- [ ] New test 'checkpoint while N domains hold snapshots' exercises
      the cross-domain barrier.

Refs #156.  Depends-on: C." \
  --labels "epic-156-multicore"
```

### Step 3.7 — Record the sub-issue numbers in the roadmap

After Forgejo assigns numbers (likely 163-168 given last issue was 162), update the table in `docs/MULTICORE_ROADMAP.md`:

- [ ] **Edit `docs/MULTICORE_ROADMAP.md`** — replace the placeholder labels (`45-A`, `45-B`, …) in the sub-issue map with the real issue numbers Forgejo assigned:

```markdown
| # | Title | Depends on |
|---|-------|------------|
| 163 | epic 156 / A: domain-safety audit | — |
| 164 | epic 156 / B: runtime choice (Lwt vs Eio) | 163 |
| 165 | epic 156 / C: pager threading model | 163, 164 |
| 166 | epic 156 / D: reader-worker pool | 164, 165 |
| 167 | epic 156 / E: scaling bench | 166 |
| 168 | epic 156 / F: WAL cross-domain coordination | 165 |
```

(Substitute the actual numbers Forgejo returned.)

### Step 3.8 — Commit roadmap update

- [ ] **Commit:**

```bash
git add docs/MULTICORE_ROADMAP.md
git commit -m "$(cat <<'EOF'
docs(#156): record real sub-issue numbers in roadmap

Roadmap previously used placeholder labels A-F; replaced with the
issue numbers Forgejo assigned when the sub-issues were filed.

Refs #156.

Co-Authored-By: Claude Opus 4.7 <noreply@anthropic.com>
EOF
)"
```

---

## Task 4 — Cross-link the umbrella issue

**Why:** Anyone landing on #156 should immediately see the filed sub-issues without having to grep labels.

**Files:** none — Forgejo comment.

### Step 4.1 — Comment on #156

- [ ] **Post:**

```bash
~/.local/bin/forgejo issue comment tej/sqlite_ocaml_port 156 --body "Phase 45 decomposed this epic into sub-issues; see \`docs/MULTICORE_ROADMAP.md\`:

- #163 — domain-safety audit
- #164 — runtime choice (ADR-0001)
- #165 — pager threading model
- #166 — reader-worker pool
- #167 — bench_multi_domain_reader_scaling
- #168 — cross-domain WAL coordination

All filed with label \`epic-156-multicore\`.  Schedule individual sub-issues into future phases as motivated by profiling data."
```

(Substitute the real numbers from Task 3.7.)

---

## Task 5 — Push, no close (#156 stays open)

- [ ] **Push:**

```bash
git push origin main
```

- [ ] **Do NOT close #156.** It remains the umbrella epic; closing it would lose context for the still-open sub-issues.

---

## Out of scope

- Any implementation work for any sub-issue. Phase 45 is documentation + filing.
- Re-deciding D1 / D2 / D3 in this phase. The doc records defaults; the picking phases (sub-issues B and C) can override.
- Scheduling sub-issues into specific phase numbers. That happens when each one becomes the next thing to ship.

## Acceptance summary

- [ ] Label `epic-156-multicore` exists.
- [ ] `docs/MULTICORE_ROADMAP.md` exists with decision points and a sub-issue map.
- [ ] `docs/adr/0001-multicore-runtime-choice.md` exists as a placeholder.
- [ ] Six sub-issues filed (A-F) and labelled.
- [ ] #156 has a comment linking all sub-issues.
- [ ] Sub-issue numbers are recorded back into the roadmap doc.
