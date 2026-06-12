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

## Next

- item 3 of #263 — Reader-floor bump after standby profile load
