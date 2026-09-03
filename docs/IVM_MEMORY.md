# IVM memory: the net-zero SUM-group retention ceiling (#423)

**Short answer for someone watching a unikernel's heap grow under a
delta-maintained `SUM` view:** the incremental aggregate retains at most one
entry per *distinct* `GROUP BY` key it has ever been shown, at **9 major-heap
words plus the key** each — **16 words (128 bytes) measured** for a view grouped
on an `INTEGER` column. It never retains per *update*. If your heap is growing
with the update count, the aggregate is not where it is going.

The rest of this file says why, what was measured, and what to do about it.

## The behaviour

`Granary_ivm.Aggregate.Make` (`lib/ivm/aggregate.ml`) keeps one accumulator per
group:

```ocaml
type acc = { mult : int; aggv : int }   (* Σ weight, Σ measure * weight *)
```

A group's output row exists iff `mult > 0`. The entry is removed from the map
only when **both** `mult = 0` **and** `aggv = 0`.

A group whose weights cancel to `mult = 0` while `aggv <> 0` is therefore
**deliberately retained**. Dropping it would discard `aggv`, which is not
recoverable from the delta feed — the operator has no access to the base rows —
so a later delta reviving the group would compute the wrong total silently.
That retention is correct and #423 decided to keep it: **accept the retention,
state the ceiling**. Compaction and an LRU/age cap with re-derivation on revival
were both considered and rejected; re-derivation needs exactly the base access
the operator does not have.

## The ceiling

### 1. It is bounded by distinct groups, never by updates

The state is `GMap.t`, a `Map` keyed by the group. One entry per group, forever.
So:

```
retained entries  ≤  distinct GROUP BY keys the operator has ever been shown
```

and **not** a function of how many deltas it has processed. A view churning a
fixed key set forever retains at most that key set. The hazard is unbounded
*key cardinality* — a `GROUP BY` over a session id, a request id, a timestamp —
not a high update rate over a stable key set. That distinction is the whole
point of the bound.

Measured: 50 000 net-zero rounds over one key cost 27 words; 200 000 rounds over
the same key cost 27 words. Quadrupling the churn added nothing.

### 2. It costs 9 words per retained group, plus the key

Structurally: one `Map.Make` node (a five-field record — left, key, data, right,
height — so 6 words with its header) plus the two-field `acc` record (3 words) =
**9 words**, or 72 bytes on a 64-bit target. The group key is the caller's and
costs whatever it costs; an immediate (`int`) key costs nothing extra.

Measured, by the peak-live-major-heap method the repo's other allocation gates
use (`Gc` alarm plus a settled `full_major` with the operator still reachable),
over 20 000 → 40 000 retained groups:

| instantiation | group key | measured |
|---|---|---|
| `Aggregate.Make` with an `int` group | immediate | **9.00 words/group** |
| `Reactive_view.Agg_engine` (what a delta-maintained SQL view runs on) | `[\| V_int n \|]` | **16.00 words/group** |

The second is the first plus its key: a 2-word `Row.value array` holding a
2-word `V_int` block around a 3-word boxed `int64` = 7 words on top of 9. A
`TEXT` group key adds the string instead. In operator terms, one million
retained integer-keyed groups is **≈128 MB** — the number to size a unikernel
against.

Both figures reproduce exactly (`9.00`, `16.00`) rather than approximately,
because they are block layouts rather than allocator behaviour.

### 3. It cannot arise at all without a negative weight

If every element of a group carries a non-negative cumulative weight, then
`mult = Σ w = 0` forces every `w = 0`, hence `aggv = Σ measure * w = 0`, and the
entry is pruned. **A delta stream that only ever retracts what it has inserted
never retains anything.**

This is the part of the issue's framing that turned out to be too pessimistic.
"On a high-churn SUM view such net-zero groups accumulate unboundedly" is true
only where signed weights reach the operator:

- a composed Z-set pipeline, where an upstream operator legitimately emits
  negative weights (the raw `granary.ivm` API is public and supports this);
- a retraction with no matching insertion — the operator being told a row left
  that it never saw arrive.

For the `Db` reactive-view driver the second is the reachable one. `Db`'s
change feed produces `Inserted` / `Deleted` / `Updated` mirroring real DML, and
under a faithful feed the aggregate never retains: emptying a group prunes it.
A feed that is *not* faithful can retain — see "How a live view reaches this
state" below.

### 4. It cannot arise unless the measure varies within one group

`aggv = Σ measure * weight`. If `measure` is a constant `c` across the group's
elements, that is `c * Σ weight = c * mult`, which is zero exactly when `mult`
is. So:

- **`COUNT` never retains** (`measure = fun _ -> 1`). The issue asserts this and
  it holds *unconditionally* — including for arbitrary signed weights, which is
  the only regime in which `SUM` retains. Verified by a QCheck property over
  signed weights rather than taken on trust.
- Neither does a `SUM` over a column that happens to be constant within each
  group. The rule is about the measure being constant, not about it being 1.

## How a live view reaches this state

For a delta-maintained view (`Reactive_view.Agg_engine`), the group key is the
`GROUP BY` column's value and the measure is the summed column's value, so
retention needs a group with two different summed values whose row weights
cancel — i.e. the feed retracted a `(key, value)` pair the engine never held.
The known route is a stale delta: `record_change` deltas are not reverted by
every rollback path (see the `#666` / `#737` sections of `CLAUDE.md`), so a
retracted-then-really-deleted row, or an `Updated` whose `old_row` the engine
never saw, can leave a group at `mult = 0` with `aggv <> 0`.

**The retention is not permanent for a live view.** `Db`'s `rv_rebuild_engine`
constructs a *fresh* `Agg_engine` on a resync — which `ROLLBACK TO` schedules
(#427) and which a failed statement can schedule (#737) — so a rebuilt view
starts from an empty map. Dropping and recreating the reactive view does the
same thing deliberately.

## Observing it

`Granary_ivm.Aggregate.Make(S).retained_groups : t -> int` and
`Granary.Reactive_view.Agg_engine.retained_groups : state -> int` report the
live entry count, including groups contributing no output row. Comparing it
against `List.length (Agg_engine.snapshot st)` gives the net-zero count
directly, so an embedding can watch its own ceiling rather than infer it.

## The pin

`test/test_agg_retention_423.ml` measures the per-group cost and asserts the
ceiling, alongside the four claims above. The gate is **armed by default** at
16 words/group, with `GRANARY_MEM_MAX_WORDS_PER_GROUP` as the escape hatch —
see `CLAUDE.md`'s non-wall-clock gate table for where it runs and what to do
when it fails.
