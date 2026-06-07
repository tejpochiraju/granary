# Per-deployment durability knob (#298)

**Date:** 2026-06-07
**Issue:** #298 — feat: per-deployment durability knob (synchronous=off/batched/full analogue)
**Status:** Design approved

## Problem

Writes are fsync-bound (#222 bench verdict). On the small HDD host the fsync ceiling
is what caps the number of hosted sqlocaml apps (~10 today). The dial that moves the
needle is **fsync policy**, not disabling CoW (CoW is load-bearing for crash atomicity,
snapshot isolation, and rollback — see #298 context).

We add a per-deployment durability setting, analogous to SQLite `synchronous`, that
gates **only** the group-commit drainer's fsync call. CoW, snapshot isolation, rollback,
and WAL recovery logic are left completely untouched.

## Durability model

A database-wide setting on the WAL-backed `Btree` backend (`bt_state`). It is a no-op on
the in-memory `Mem` backend (which has no WAL/fsync), mirroring how `wal_autocheckpoint`
behaves today. Three modes:

| Mode | Commit-time fsync | App crash (process death) | OS/power crash |
|------|-------------------|---------------------------|----------------|
| `full` (default) | every group-commit, before ack — **unchanged** | no loss | no loss |
| `batched` | deferred; sync when **≥N un-synced commits** OR **≥T ms elapsed** (whichever first), checked at commit time | no loss (page cache survives process death) | up to last synced commit frame; **prefix only** |
| `off` | never on commit | no loss | back to last checkpoint |

**Defaults:** mode `full`; batched window `N = 256` commits, `T = 100 ms`.

### Why this is safe

- **App-process crash is always safe in every mode.** Unsynced WAL frames live in the
  kernel page cache, which survives process death; recovery replays them.
- **OS/power crash** is the only exposure for `batched`/`off`. There is also **no write
  ordering guarantee** in those modes (same caveat LMDB documents for `NOSYNC`). But WAL
  recovery already trusts only checksum-valid, commit-marked frames and stops at the first
  bad frame (`wal.ml:280-322`), so a torn tail is discarded cleanly — recovery converges
  to a **prefix** of acked commits. `batched` bounds the loss window by N or T; `off` is
  for rebuildable data.

### Durability anchors (always full-sync, regardless of mode)

- **Checkpoint** (`store.ml:1172-1206`): reads WAL frames from cache → writes them to the
  main DB → `Pager.flush_sync_main` (fsync main) → `Wal.reset`. The existing order
  (fsync main *before* WAL reset) is already crash-safe and is unchanged.
- **`close`** (`store.ml:567`): in `batched`/`off`, if there are pending un-synced commits,
  issue one final `Pager.wal_sync` before invoking `wal_close`.

So `off`/`batched` data is always made durable at checkpoint and at clean shutdown.

## Components & changes

### 1. Core — `lib/store/store.ml` / `store.mli`

New public type in `store.mli`:

```ocaml
type durability =
  | Full
  | Batched of { commits : int; interval_ms : int }
  | Off
```

New `bt_state` fields:

```ocaml
; mutable durability : durability          (* default Full *)
; mutable unsynced_commits : int           (* committed-but-unsynced frame batches *)
; mutable last_sync_time : float           (* clock() at last commit fsync; for T *)
```

`unsynced_commits` and `last_sync_time` are initialised to `0` / `0.` at open.

**Gate point:** `commit_wal` (`store.ml:1423-1467`). Today it unconditionally runs
`group_commit_sync (fun () -> Pager.wal_sync …)` then `maybe_autockpt_after_commit`.
New logic:

- `Full` → unchanged path.
- `Batched { commits = n; interval_ms = t }`:
  - Increment `unsynced_commits` **under the write lock, before `unlock_once ()`**, so the
    counter is race-free across concurrent writers.
  - Compute `should_sync = unsynced_commits >= n || (clock () -. last_sync_time) *. 1000. >= t`.
  - If `should_sync`: run the existing `group_commit_sync` + `Pager.wal_sync`; on success
    reset `unsynced_commits <- 0` and `last_sync_time <- clock ()`.
  - Else: resolve the commit waiter immediately with `Ok ()`, no fsync.
- `Off` → resolve the waiter immediately, never fsync on commit.
- `maybe_autockpt_after_commit` runs in **all** modes (keeps the WAL bounded; checkpoint is
  the durability anchor).

The clock is the store's existing time source (the `unit -> float` already threaded for
timestamps). No new injected callback, no background task — `T` is opportunistic (checked
when a commit arrives or at checkpoint), per the portability decision below.

**`close` (`store.ml:567`):** for `Batched`/`Off`, if `unsynced_commits > 0`, perform one
final `Pager.wal_sync` before `wal_close`.

New store API (`store.mli`):

```ocaml
val durability : t -> durability
val set_durability : t -> durability -> unit
```

`set_durability`/`durability` are no-ops / return `Full` on the `Mem` backend.

### 2. Surface — open-option

Thread `?durability:durability` through:

- `Db.open_block` / `Db.of_store` (`lib/db/db.ml` / `db.mli`)
- `Store.open_block` (`lib/store/store.ml` / `store.mli`)
- into the `bt_state` constructor (default `Full`).

### 3. Surface — PRAGMA (reuses the `wal_autocheckpoint` pattern end-to-end)

Three PRAGMAs, parsed → planned → executed exactly like `wal_autocheckpoint`
(`parser.mly:325-384`, `planner.ml:822-833`, `exec.ml:6464/9498`):

| PRAGMA | Setter | Getter returns |
|--------|--------|----------------|
| `PRAGMA synchronous = full \| batched \| off` | set mode (keeping current N/T) | canonical `full`/`batched`/`off` |
| `PRAGMA wal_batch_commits = N` | set batched N | current N |
| `PRAGMA wal_batch_interval_ms = T` | set batched T | current T |

Vocabulary is our own (`full`/`batched`/`off`) — unambiguous 1:1 with the modes, not
SQLite's `OFF/NORMAL/FULL/EXTRA`. Unknown values error like other typed PRAGMAs.

AST additions (`ast.mli`): `Pragma_synchronous` / `Pragma_synchronous_set of string`,
plus `Pragma_wal_batch_commits[_set]` and `Pragma_wal_batch_interval_ms[_set]`.
Plan ops mirror these. Setting N or T while in `full`/`off` updates the stored batched
parameters but only takes effect once the mode is `batched`.

**Scoping (documented honestly):** the commit queue is **one per database** (`bt_state`),
so a PRAGMA on any connection changes the mode for *all* connections — last writer wins.
This differs from SQLite, where `synchronous` is per-connection. Documented in `store.mli`
and the PRAGMA docs.

### 4. Docs

A prominent caveat block in README/wiki and the `store.mli` doc-comment:

- `batched`/`off` give **no write-ordering guarantee** under OS/power loss.
- App-process crash is always safe in every mode.
- `batched` bounds the loss window (N commits or T ms); `off` is for rebuildable data.
- Setting is **database-wide** (shared commit queue), not per-connection.

## Data flow

```
PRAGMA synchronous=batched  ──parser──> AST.Pragma_synchronous_set "batched"
                            ──planner──> Plan.Op_pragma_set_synchronous
                            ──exec────> Store.set_durability store (Batched {256;100})
                                         └─ writes bt_state.durability

COMMIT (batched) ──> commit_wal
   prepare btree (no inline fsync)
   unsynced_commits++            (under write lock)
   unlock_once
   should_sync?                  (N reached OR T elapsed via clock)
     yes ─> group_commit_sync (Pager.wal_sync); reset counter + last_sync_time
     no  ─> resolve waiter Ok () immediately      <-- the throughput win
   maybe_autockpt_after_commit   (all modes)

CHECKPOINT / CLOSE ──> always full-sync (durability anchor)
```

## Error handling

- A failed `Pager.wal_sync` during a batched/triggered sync propagates to the committing
  waiter exactly as in `full` mode today (`group_commit_sync` failure path,
  `store.ml:1302`); `unsynced_commits`/`last_sync_time` are **not** reset on failure, so
  the next commit retries the sync.
- Invalid PRAGMA values (e.g. `synchronous = wat`, negative N/T) return a typed error,
  consistent with other typed PRAGMAs. N/T are clamped to `>= 0` like `set_wal_autocheckpoint`.
- `Mem` backend: setters are no-ops, getters return defaults — no error.

## Testing (TDD)

- **Unit — surface:** mode set/get via both open-option and each PRAGMA; getters return
  canonical values; N/T persist independent of mode.
- **Unit — `full` regression:** `wal_sync_count` per commit identical to today (byte-for-byte
  behaviour unchanged).
- **Unit — `batched` triggers:** counter trigger fires exactly at N commits; injected-clock
  time trigger fires at T ms; whichever-first honoured.
- **Unit — `off`:** `wal_sync_count` delta == 0 across commits; > 0 after checkpoint and
  after `close`.
- **Recovery suite:** crash (drop unsynced frames) in the batched/off window → recovers to
  the last synced commit frame, checksum chain stops cleanly; `full` recovers everything.
- **QCheck property:** for random sequences of commits + mode changes + simulated crashes,
  the recovered state is always a **prefix** of acked commits.
- **Jepsen (#177 harness):** `full` unchanged; `batched`/`off` validated for
  *prefix-consistent* loss only (no torn/interleaved state after `kill -9`). Power-loss
  ordering caveat is documented, **not** tested.

## Portability decision (recorded)

The "T ms elapsed" trigger is **opportunistic** — evaluated using the existing `unit -> float`
clock when a commit arrives or at checkpoint. We deliberately do **not** introduce a
background sleep/timer into the core store, because the core (`lib/store/`) is kept
MirageOS-portable via injected callbacks and currently imports no `Lwt_unix`/sleep
primitive. Consequence: a fully idle database does not flush pending writes until the next
commit / checkpoint / close. The N-commit trigger is unaffected. (App-crash safety is
total; only OS/power crash during idle is exposed, already covered by the documented caveat.)

## Out of scope (follow-up issues to file)

- **HDD-host throughput benchmark per mode** (#222 harness): run `batched`/`off` vs `full`
  commit-throughput on the small HDD host profile to quantify the hosting-density win.
  Deferred so this PR stays correctness-focused and because meaningful numbers need the
  real HDD host (in-container/NVMe numbers hide the fsync-ceiling story).
- **True wall-clock idle flushing** via an injected sleep primitive (background flusher),
  if opportunistic-T proves insufficient in practice.
