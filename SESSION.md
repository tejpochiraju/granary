# Session notes

## 2026-06-12 — PR #358 merged

**Merged:** `feat(#263): bounded snapshot reads on following replica`

What was done:
- Added `follower_ack_position` to `bt_state` (local `Wal.committed_frames` count)
- `ro_begin` caps `snap_frames` to `min(committed_frames, ack)` when following
- Standby calls `Store.set_follower_ack_position ~frames` after each apply
- `set_follower false` clears the ack position
- Btree-store test `test_follower_ro_ack_position` added

## 2026-06-12 — Item 2 of #263 — Reader-safe epoch-transition checkpoint

**Done:** `feat(#263): reader-safe epoch-transition checkpoint`

What was done:
- Added `Store.wait_for_readers_past : t -> target:int -> unit Lwt.t` to expose
  the reader-pin gating (used by the inline checkpoint) to external consumers.
- Added `~reader_gate:(target:int -> unit Lwt.t)` parameter to
  `Replication.checkpoint_wal_to_main` and `apply_frames_epoch_aware`.
- `checkpoint_wal_to_main` calls `~reader_gate` with `Wal.committed_frames wal`
  as target **before** `Wal.reset`, preventing frame-index recycling while any
  RO snapshot holds those indices.
- `Standby.t` stores a `reader_gate` (initialized from the store at `create`).
  `promote`, `start_following`, and `rebase` all thread it through.
- Added `test_reader_safe_epoch_transition_concurrent`: holds an RO snapshot
  across a concurrent epoch-transition apply; verifies the reader sees
  consistent data and the checkpoint correctly migrates pages to main.
- Added `prop_checkpoint_correctness` (QCheck, 100 iterations): random page/value
  sets, sequential epoch-0 then epoch-1 apply; verifies every epoch-0 page
  lands in main after the epoch transition.

Refs: #263, #207 (reader-aware replication floor — prerequisite, closed).

## 2026-06-12 — Session handover: item 3 of #263

### State of #263

| Item | Description | Status |
|------|-------------|--------|
| 1 | Bounded snapshot reads on following replica | Merged (PR #358) |
| 2 | Reader-safe epoch-transition checkpoint | Merged (PR #359) |
| 3 | Reader-floor bump after standby profile load | **Design drafted** |

### What item 3 is

Item 1 capped `snap_frames` to `follower_ack_position` on a following replica, preventing RO readers from seeing un-applied frames. Item 2 made `checkpoint_wal_to_main` gate on RO readers before `Wal.reset`.

Item 3 ("Reader-floor bump after standby profile load") would advance the *replication floor* (a checkpoint gate — see `replication_floor_below` in `store.ml:503`, `update_replication_position` in `store.mli:460`) when a standby finishes applying a batch. This prevents the standby from blocking the *master's* checkpoint: without a floor bump, the master's `wait_for_readers_past` will not advance past the standby's acked position, so the master may stall on checkpoint waiting for the standby to ship frames.

The prerequisite infrastructure already exists:
- `update_replication_position` (`store.mli:460`) — register the shipped frame position
- `replication_shipped_frames` (`store.ml:149`) — the floor `checkpoint_unlocked` waits on
- `replication_gate_max_yields` (`store.ml:156`) — bounded-yield timeout for the floor wait

Item 3 likely involves calling `Store.update_replication_position` from the standby driver after each successful apply, so the master's floor advances correctly. This interacts with the `on_committed_frames` callback (#337) and the sink-ship path.

### Design

See [docs/specs/2026-06-12-standby-replication-floor-bump-design.md](./docs/specs/2026-06-12-standby-replication-floor-bump-design.md).

Recommended approach: add an `?on_standby_ack:(int -> unit)` callback to
`Standby.create`. The application wires it to the master's
`update_replication_position`. The standby calls it after each successful
apply batch, with the local `Wal.committed_frames` value. This keeps the
standby driver agnostic of master-store topology while allowing the
master's replication floor to track the standby's true applied position.

### Related issues

- #207 (reader-aware replication floor — prerequisite, closed)
- #337 (sink-ship drain before Wal.reset)
- #360 (shared-WAL-handle end-to-end test) — deferred from PR #359 review

### Refs
- #263, #207, #337, #360
