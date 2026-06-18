# Whole-DB `as-of` time travel — Tier-1 MVP (#266)

Status: design approved 2026-06-18. Scope: whole-DB point-in-time **reads**
against retained copy-on-write roots. Default **off**; enabled by an open-time
flag. Per-table retention, sparse retention, and the mark-sweep GC stay in #266
for later.

## 1. Goal & scope

Query the database as it existed at any past commit/timestamp as a first-class,
non-destructive read — instant and concurrent with live writes (not a restore).

In scope (this PR):

- An injected, append-only commit log `txn_id -> (timestamp, root_page)`.
- A retention floor that pins the existing reclamation watermark.
- A read API `ro_begin_as_of` resolving a txn/timestamp to a historical root.
- An open-time enable flag, default off.

Explicitly out of scope (remain in #266):

- Per-table retention scope.
- Sparse retention (retain every Nth root) and the tree-aware **mark-sweep GC**.
- Tier-2 per-entity history (`since`, datom diffing).
- Durable history on path-less / block-only backends (in-memory log only there).
- Encrypted-DB history (the timestamp must not leak into encrypted frames; this
  feature targets unencrypted DBs for now).

## 2. Key architectural findings (verified against `main`, 2026-06-18)

1. Each commit produces a new immutable root via the two-header ping-pong
   (`header.ml:201-208` `stage_next_header`; the old slot is overwritten on the
   next-next commit, so committed header state must be archived before then).
2. The header record carries **no timestamp** — `txn_id` is today the only
   monotonic coordinate (`header.ml` record fields). An 8-byte timestamp is new.
3. Reclamation is MVCC-gated on
   `min_safe_txn_id = min(current_rw_txn, min_active_reader)`
   (`store.ml:1252-1256`); the freelist reuses a page only when
   `freed_at_txn_id < min_safe_txn_id` (`freelist.ml:37-39`).
4. `ro_begin` already captures `rs_snap_txn_id` + `rs_snap_meta_root` and the
   read path resolves trees against that root (`store.ml:1159-1194`) — opening a
   snapshot against an *arbitrary* root is therefore already supported.
5. `Store` is **path-agnostic**: it is opened with I/O callbacks
   (`open_block` / `open_block_wal`, `store.mli:85-123`), not a filesystem path.
   The commit log must therefore be an **injected sink**, not a path baked into
   `Store`.

### Why no mark-sweep GC is needed for this MVP

For *dense* whole-DB retention (retain everything from a floor to head), every
superseded page is already on the freelist tagged with its `freed_at_txn_id`.
Pinning `min_safe_txn_id` merely stops `Freelist.pop` from returning those pages;
*raising* the floor lets the existing freelist reclaim them lazily on the next
allocations. The mark-sweep in #266 work-item 4 is only required for *sparse*
retention (keeping root T1 and T3 while reclaiming pages freed between them,
which `freed_at_txn_id` alone cannot classify). Sparse retention is deferred, so
this PR adds **zero** new GC machinery.

### Checkpoint safety

CoW never mutates a page in place: changing page P allocates a new page P' and
frees P. A retained historical root references the original page-ids, whose bytes
stay intact in main as long as they are not *reused*. Checkpoint writes each
live page-id's latest content to main; a freed-but-unreused page-id is never
overwritten, so its historical bytes remain at its offset in main. Reuse — gated
by `min_safe_txn_id` — is the only thing that destroys history. Therefore the
floor pin is sufficient and checkpoint-safe; this is asserted by a test.

## 3. Components

### 3a. `History` sink (injected, mirrors the block-device abstraction)

```ocaml
type record = { txn_id : int64; timestamp : int64; root_page : int64 }

type sink =
  { append : record -> unit Lwt.t   (* append one committed root, best-effort *)
  ; load   : unit -> record list Lwt.t  (* read the log in ascending txn order *)
  }
```

- Threaded into `create` / `open_block` / `open_block_wal` as `?history:sink`.
- Record wire format: `txn_id(8) ++ timestamp(8) ++ root_page(8) ++ crc32(4)`,
  fixed 28 bytes, big-endian, CRC over the first 24 bytes. A torn or
  CRC-mismatched tail record is dropped on `load` (warned, non-fatal): the DB's
  own header is the source of truth for the live head.
- **Unix layer** provides a file-backed `sink` over an append-only `db.aslog`
  sidecar (path derived from the main DB path at the Unix layer, never inside
  `Store`).
- `create ()` / Mirage / no sink supplied → feature inert (see 3e). An in-memory
  list sink may be supplied for tests / non-durable use.

### 3b. Clock injection

- `?now : unit -> int64` (milliseconds since epoch) threaded alongside `?history`.
- Unix layer default: wall-clock. Mirage: inject the platform clock.
- Single writer ⇒ commit order equals timestamp order; the resolver relies on
  monotonic non-decreasing timestamps.

### 3c. Commit hook

In `commit_prepare_btree`, immediately after `st.current_header` is updated on a
successful header commit (`store.ml:1719-1724`), if history is enabled, append
`{ txn_id; now (); root_page }` to the sink. Best-effort and off the durability
critical path: a lost tail record only makes that one commit non-addressable as
history (it is the live head anyway, reachable via the header). The append must
not fail the commit.

### 3d. Retention floor

- New `mutable history_floor : int64 option` in the store state.
- Folded into the watermark at `store.ml:1252`:
  `min_safe = min (current_rw, min_active_reader, floor + 1)` when `floor` is set
  (retain snapshot `floor` ⇒ keep pages freed at `>= floor + 1`).
- Setting the floor pins retention; clearing/raising it releases pages through
  the normal freelist path. No GC, no mark-sweep.

### 3e. As-of read path

- Factor the snapshot-building tail of `ro_begin` into
  `ro_begin_at ~snap_txn_id ~snap_meta_root`; the existing `ro_begin` calls it
  with the live header (no behaviour change).
- `ro_begin_as_of t target` (`target = `Txn of int64 | `Ts of int64`):
  1. `load` the log; resolve the record with the largest `txn_id` (or
     `timestamp`) `<= target`.
  2. If the resolved txn `< history_floor` → `Error History_pruned`.
  3. Register an `active_readers` entry at the resolved txn (so an open
     historical reader pins reclamation even with no floor set), then call
     `ro_begin_at` with the historical `(txn_id, root_page)`. `ro_end` releases
     it via the existing path.
- The read path is otherwise unchanged: snapshot isolation already supports
  concurrent old/new readers alongside live writes.

## 4. Enable flag (default off)

- Open-time boolean `?as_of_history:bool` (default `false`) on `create`,
  `open_block`, `open_block_wal`. **Open-time parameter only** — no PRAGMA. This
  is honest about the fact that durable history needs the sink + clock wired at
  open and cannot be conjured at runtime on a path-less store.
- When `false` (default): `history_floor` is fixed `None`, the commit hook is
  skipped, and the as-of API returns `History_unavailable`. Zero overhead on the
  commit path.
- When `true`: the caller must also supply `?history` (and normally `?now`);
  the commit hook, floor, and as-of API are live. If `as_of_history:true` is
  passed without a `?history` sink, open fails fast with a clear error rather
  than silently degrading.

## 5. API surface

```ocaml
(* Store *)
val history_pin     : t -> txn_id:int64 -> unit
val history_floor   : t -> int64 option
val history_release : t -> unit
val history_log     : t -> History.record list Lwt.t
val ro_begin_as_of  : t -> [ `Txn of int64 | `Ts of int64 ] -> ro txn Lwt.t

(* Db: thin wrapper that opens the historical RO snapshot via
   Store.ro_begin_as_of and runs the existing read-only query entry point
   against it, ending the snapshot afterward. Exact signature mirrors the
   current Db RO query function (to be matched during implementation); it
   takes the same SQL/statement input plus the as-of target. *)
val as_of : t -> [ `Txn of int64 | `Ts of int64 ] -> string -> (* rows *) _ Lwt.t
```

New errors: `History_pruned` (target older than the floor),
`History_unavailable` (as-of called on a store opened without the feature),
`History_misconfigured` (`as_of_history:true` but no sink). Each gets a
`pp_error` arm.

## 6. Error handling

- Torn / CRC-failed tail record in the log → dropped on `load`, warned, non-fatal.
- Commit-hook append failure → logged, never fails the commit.
- As-of target newer than head → resolves to head (largest `<=` target).
- As-of target with an empty log → `History_pruned`.

## 7. Testing (100% line coverage on new code, per repo standard)

- **Unit:** record encode/decode round-trip; CRC rejects corruption; torn-tail
  truncation on load; resolver picks largest `<=` target for both `Txn` and `Ts`;
  floor folds into `min_safe`; pruned-target and unavailable/misconfigured errors.
- **Integration:** write N commits, `as_of` each returns the exact historical
  contents; concurrent live writes do not disturb an open historical reader;
  raising the floor reclaims (observable freelist/file-size drop).
- **WAL + checkpoint:** a historical root is still readable after a checkpoint
  (verifies the checkpoint-safety claim of §2).
- **QCheck:** random commit/read interleavings — `as_of(T)` always equals a
  snapshot taken live at T; an open historical reader never observes its pages
  reused.
- **Default-off:** with the flag unset, the commit path is unchanged and the
  as-of API returns `History_unavailable`; existing suites stay green.

## 8. Follow-ups (keep #266 open)

- Per-table retention scope (requires the tree-aware mark-sweep GC).
- Sparse retention + horizon-advance mark-sweep.
- Durable history on block-only / Mirage backends.
- Encrypted-DB history via a separate signed manifest.
- PITR as `as_of` + `copy_to` to a fresh file.
- Tier-2 per-entity history.
