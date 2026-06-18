# #412 — as-of time-travel against ATTACHed databases (Tier-1, focused increment)

Follow-up from #266 / PR #411 (whole-DB as-of, Tier-1). Today `Db.query_as_of`
opens the historical RO snapshot on the **top** store and *rejects* any query
that routes to an ATTACHed sub-handle. This spec lifts that restriction with a
**focused increment**: per-store as-of resolution, one store per query (which is
how routing already works), with per-schema retention control.

Explicitly **out of scope**: coordinated cross-schema joins at a single
wall-clock target. Main and each attached DB have independent clocks and
commit-orders, so a single `` `Ts `` resolves to a different `txn_id` in each
store. Routing already picks exactly one store per compiled op, so this
increment never needs to coordinate two stores; the cross-store join case is
documented as a known limitation, not implemented.

## Decisions (locked during brainstorming)

1. **Scope:** focused increment — as-of resolves on the routed handle's store;
   one store per query. Per-store resolution *is* the semantics.
2. **Enablement:** attached DBs inherit the top handle's `~as_of_history`
   setting. If top has history on, attached stores opened thereafter get their
   own `<path>.aslog` sink; if top is off, attached stays off. No new SQL
   surface.
3. **Retention API:** `history_pin` / `history_floor` / `history_release` /
   `history_log` gain `?schema:string` (default `"main"`). Unknown schema raises
   `Invalid_argument`.

## Components

### 1. `query_as_of` — snapshot on the routed store (`lib/db/db.ml:1742`)

The execution call already runs against the routed handle (`t.store`,
`t.catalog`, `t.clock` at `db.ml:1782-1789`). Only the `ro` snapshot is wrongly
taken from `top.store`. Fix:

- Reorder: call `compile_routed top sql` **first** to obtain the routed handle
  `t` and the op promise.
- `Error e` from compile → return it directly (no `ro` opened, nothing to clean
  up — simpler than today's nested cleanup).
- `Ok op` → open `ro = S.ro_begin_as_of t.store target` (was `top.store`), then
  execute `In_ro_txn ro` exactly as today, with the same idempotent `end_ro`
  stream-drain / exception cleanup.
- Delete the guard block (`db.ml:1770-1781`) and its `Runtime "... main database
  only"` error.
- The outer `Lwt.catch` mapping `History_unavailable` / `History_pruned` is
  unchanged: `ro_begin_as_of` on the attached store raises the same exceptions.

When routing lands on main, `t.store == top.store` and behaviour is
byte-identical to today.

Update the `query_as_of` doc comment (`db.mli`): replace the "main database
only" note with "resolves against whichever database the statement routes to
(main or the active attached schema); a single query cannot span two databases
at one timestamp because their commit-orders are independent."

### 2. History enablement on attach (inherit)

- **`file_provider.open_store`** (`db.ml:90`): add `?as_of_history:bool`
  (default `false`). Existing call sites (VACUUM at `db.ml:326`, `db.ml:339`)
  keep the default — VACUUM history preservation is out of scope.
- **Unix provider** (`sqlocaml_unix.ml:15`): thread `?as_of_history` into
  `Store.open_file ~as_of_history`, which already wires the `<path>.aslog` sink
  (`lib/unix/store.ml:189`).
- **New store accessor** `S.history_enabled : t -> bool` (`store.mli` +
  `store.ml`): reports whether the store was opened with a history sink. New
  public `val` with a `(** … *)` doc comment (merlint requirement).
- **ATTACH** (`db.ml:1379`): pass
  `~as_of_history:(S.history_enabled top.store)`.

### 3. Per-schema retention API (`db.ml` + `db.mli`)

Add `?schema:string` (default `"main"`) to `history_pin`, `history_floor`,
`history_release`, `history_log`. A private helper resolves a schema name to a
store:

```
let store_for_schema top schema =
  if String.equal schema "main" then top.store
  else match Hashtbl.find_opt top.attached schema with
    | Some sub -> sub.store
    | None -> invalid_arg (Printf.sprintf "unknown schema: %s" schema)
```

Each public function delegates to its `S.*` counterpart on the resolved store.
`?schema` is always read against `top.attached`, so these are called on the top
handle (sub-handles never hold attachments). Document the `Invalid_argument` on
unknown schema in each `.mli` doc comment.

## Data flow

```
query_as_of top target sql
  └─ compile_routed top sql ─► (op_promise, t)        # t = main or attached sub-handle
       └─ ro = S.ro_begin_as_of t.store target        # routed store's own commit log + floor
            └─ Sql.Exec.query In_ro_txn ro  t.store t.catalog t.clock op
```

```
ATTACH 'p' AS aux   (top opened with as_of_history)
  └─ prov.open_store ~path:p ~as_of_history:true ()   # inherits top's setting
       └─ Store.open_file … ~as_of_history:true       # creates p.aslog sink
```

## Error handling

- Attached store with history off (top was off) → `History_unavailable`.
- Attached store with no floor pinned → `History_pruned` (floor is not
  retroactive — pin before the writes you want to keep, same as main).
- `history_*` with an unknown / detached schema → `Invalid_argument`.

## Testing (`test/test_attach_as_of.ml`, new)

Follows `test_db_as_of.ml` + `test_attach.ml` patterns; gated like the rest of
the suite (no real SQLite needed). Target 100% line coverage on the changed
paths.

- Top history on, attach `aux`, write rows to `aux`, capture a `txn_id`, write
  more, `history_pin ~schema:"aux"`, switch active schema (or qualified select),
  `query_as_of` sees only the historical rows.
- `aux` with no floor → `History_pruned`.
- Top opened **without** `~as_of_history` → attached inherits off →
  `History_unavailable`.
- `history_pin`/`history_floor`/`history_release`/`history_log` with
  `~schema:"aux"` round-trip; unknown schema → `Invalid_argument`.
- Detach `aux`, then `history_pin ~schema:"aux"` / `query_as_of` routed to it →
  `Invalid_argument` (pin) / routing falls back to top per `resolve_target_ast`.
- QCheck: pin `main` and `aux` at different `txn_id`s; assert each store
  resolves its own snapshot independently (floor independence).

## Known limitations (documented, not implemented)

- A single statement joining `main.t` with `aux.t` at one `` `Ts `` is not
  supported: routing executes one op against one store, so only that store's
  history is consulted. Cross-store coordinated snapshots are a future tier.
- VACUUM does not preserve / re-enable history on the rebuilt file.

Closes #412 (Tier-1 attached as-of). Cross-store joins-at-a-timestamp remain
tracked under #266 / a future follow-up.
