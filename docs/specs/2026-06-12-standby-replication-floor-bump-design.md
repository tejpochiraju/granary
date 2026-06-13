# Standby replication-floor bump (#263 item 3) — design

Issue: #263 · Date: 2026-06-12 · Status: draft

## Summary

After a standby applies a commit batch, advance the master's
`replication_shipped_frames` floor so the master's checkpoint gate reflects
the standby's true ack'd position (not just the ship-notification position).
Without this, the master's floor can lag behind what the standby has actually
materialized, and in a shared-WAL-handle deployment the checkpoint may stall
waiting for a floor that was already satisfied.

## Background

Three position-tracking fields exist in `bt_state`:

| Field | Set by | Purpose |
|---|---|---|
| `replication_shipped_frames` | `Store.update_replication_position` (master side, after shipping) | Gates master checkpoint; floor must reach `committed_frames` before `Wal.reset` |
| `follower_ack_position` | `Store.set_follower_ack_position` (standby side, after apply) | Caps `snap_frames` on standby's RO readers (#263 item 1) |
| `sink_shipped_frames` | internal, per-epoch | Tracks how many frames were dispatched to `on_committed_frames` for the sink-ship drain (#337) |

Item 1 (#358) added `set_follower_ack_position` / `follower_ack_position`.
Item 2 (#359) added `wait_for_readers_past` gating for epoch transitions.

Item 3 addresses the gap: the standby never advances the master's
`replication_shipped_frames`.  In a deployment where the standby is in
the same address space as the master (the shared-WAL-handle design,
#360), this means the master's checkpoint gate sees a floor stuck at
whatever the ship path last reported — not what the standby has actually
applied.  The checkpoint may park waiting on `replication_floor_below`
even though the standby has already consumed those frames.

## Design: option A (recommended) — position-advance callback

Add an optional `?on_standby_ack:(int -> unit)` parameter to
`Standby.create`.  The application passes a callback that advances the
master's replication floor.  The standby calls it after every successful
apply batch, with the local `Wal.committed_frames` value.

### Changes

**`Standby.t`** gains an optional `on_standby_ack` field:

```ocaml
type t =
  { store : Store.t
  ; pager : Pager.t
  ; wal : Wal.t
  ; mutable mode : follower_mode
  ; mutable last_epoch : int64
  ; mutable last_frame_idx : int
  ; apply_mutex : Lwt_mutex.t
  ; reader_gate : target:int -> unit Lwt.t
  ; on_standby_ack : (int -> unit) option
    (* Called after each successful apply batch with
       [Wal.committed_frames wal].  Typically advanced by the
       application calling [Store.update_replication_position] on the
       master's store. *)
  }
```

**`Standby.create`** gains `?on_standby_ack:(int -> unit)`:

```ocaml
val create
  :  store:Store.t
  -> pager:Pager.t
  -> wal:Wal.t
  -> ?on_standby_ack:(int -> unit)
  -> t
```

**`start_following`** and **`rebase`** invoke `on_standby_ack` after each
apply batch (after `set_follower_ack_position`):

```ocaml
(* in start_following, after apply success: *)
Store.set_follower_ack_position t.store ~frames:(Wal.committed_frames t.wal);
(match t.on_standby_ack with
 | Some cb -> cb (Wal.committed_frames t.wal);
 | None -> ());
```

Same pattern in `rebase`.

**Application wiring** (shared-WAL-handle scenario, #360):

```ocaml
let master_store : Store.t = (* master's store *)
let standby_store : Store.t = (* standby's store, possibly same handle *)
let on_standby_ack frames =
  Store.update_replication_position master_store ~shipped:frames
in
let st = Standby.create ~store:standby_store ~pager ~wal ~on_standby_ack () in
Standby.start_following st stream
```

### Why a callback and not a direct store reference

- Keeps the standby driver agnostic of whether the master and standby
  share a store handle (they may use separate handles with different
  WAL/Pager instances in the test suite).
- Makes the position-advancement policy the application's responsibility,
  which already owns the transport and the master/standby lifecycle.

## Design: option B (simpler) — direct store reference

Add `~master_store:Store.t` to `Standby.create`.  The standby calls
`Store.update_replication_position master_store ~shipped:frames` after
each apply.

Rejected because it couples the standby to a specific master-store
handle and makes testing harder (each test would need a fake master
store or wrap in-noop logic).

## Interaction with the sink-ship path

Currently the ship path (the application's `on_committed_frames` implementation)
calls `Store.update_replication_position` on the master after dispatching
frames.  With item 3, the flow becomes:

```
Master commit
  → on_committed_frames fires (Lwt.async)
  → application reads frames and ships to standby
  → application calls update_replication_position ~shipped:X
      (X = committed_frames at ship time)
  → standby receives and applies batch
  → standby calls on_standby_ack (frames:=Wal.committed_frames wal)
  → application calls update_replication_position ~shipped:Y
      (Y = standby's committed_frames, which may be > X)
```

In a shared-WAL-handle deployment the second call advances the floor
past where the ship path left it.  In a dual-store deployment the second
call is a no-op on the master — the application simply doesn't wire
`on_standby_ack` or passes a no-op.

The floor may already be at or past the standby's position (the ship
path set it).  The standby's call then sets `replication_shipped_frames`
to a value at or below the current floor — harmless: a lower value only
makes the gate wait longer; it never recycles un-shipped frames.

## What about the sink-ship drain (#337)?

Not affected.  The sink-ship drain in `checkpoint_unlocked` waits for
`sink_ships_in_flight = 0` (lazy frame readers), which is a per-epoch
counter of in-flight async frame reads.  The `replication_shipped_frames`
floor is independent of this drain.

## What about the epoch-bump re-pin?

After `checkpoint_unlocked` resets the WAL, `replication_shipped_frames`
is re-pinned to `Wal.committed_frames wal` (which is 0 after reset) when
`on_committed_frames` is registered.  The standby's `on_standby_ack` will
then advance it past 0 on the next apply, which is correct.

## What about cascading replication (#208)?

Deferred.  A cascading standby that is both a consumer and a source
would need both: (a) `on_standby_ack` to report its own applied position
upstream, and (b) a registered `on_committed_frames` to ship frames
downstream.  The callback mechanism extends naturally: the standby's
`on_standby_ack` calls `update_replication_position` on its parent
master's store, while the standby itself has a commit callback for its
own downstream consumers.

## Testing

1. **Unit test (standalone):** Create a store, a standby, and a
   `position_ref` callback that records each call.  Apply frames via
   `start_following` and verify the callback fires with the correct
   `Wal.committed_frames` value after each batch.

2. **Unit test (master floor advances):** Create a master store, commit
   data, ship frames (simulated), then create a standby store with
   `~on_standby_ack:(fun frames -> Store.update_replication_position master_store ~shipped:frames)`.
   Apply frames on the standby and verify `Store.replication_state
   master_store`'s floor advances to the applied position.

3. **Integration test (#360):** Deferred to the shared-WAL-handle test
   that exercises the full master-standby lifecycle.

## Edge cases

- **No callback provided:** `on_standby_ack = None` — existing behavior
  unchanged.  Default no-op.
- **Apply during epoch transition:** `checkpoint_wal_to_main` is called
  internally by `apply_frames_epoch_aware`, which resets the WAL.  The
  `on_standby_ack` fires AFTER the epoch-aware apply returns, with the
  post-transition `Wal.committed_frames` value.  This is correct: the
  standby has switched epochs and the floor should advance to the new
  epoch's position.
- **Promotion:** `promote` does NOT fire `on_standby_ack`.  The promotion
  drains frames to main and resets the WAL; the standby is no longer
  following.  The floor was already advanced by the last `start_following`
  apply batch.
- **Re-base:** `rebase` fires `on_standby_ack` after each segment apply,
  same as `start_following`.  The application typically starts a fresh
  follower loop after the re-base's returned acked_position, so the
  `on_standby_ack` from the re-base segments correctly advances the floor
  through the replayed history.