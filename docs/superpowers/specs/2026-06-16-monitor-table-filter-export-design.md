# #385 — Internals monitor: table-name filtering & event-log export

**Issue:** tej/sqlite_ocaml_port#385 (split from #382)
**Branch:** `feat/385-monitor-filter-export`
**Date:** 2026-06-16

## Goal

Extend the v1 internals monitor (#382, which shipped txn-id filtering and an
in-memory ring buffer) with the two deferred strace-UI features:

1. **Table-name filtering** — filter the live event stream to the page I/O of a
   single table.
2. **Event-log export** — a "dump current log to file" action for capturing
   repros.

## Background (current state)

- `Store_event.t` (`lib/store/store_event.mli`) — 14 variants. The four page
  variants (`Page_read`/`Page_write`/`Page_alloc`/`Page_free`) carry `txn_id`
  and `page`, but **no table identity**. Accessors `label`, `txn_id`, `pp`.
- `Pager_event.t` (`lib/storage/pager_event.mli`) — page-only variants in the
  storage layer; `translate_pager_event` (store.ml) maps them to `Store_event`,
  stamping `txn_id` read from the pager.
- `bt_state` (store.ml) holds `on_event` and `tree_tags` (an int32 schema
  fingerprint per tree — *not* a usable table id).
- REPL monitor (`bin/repl/`): `Event_log` ring buffer with `filter : int64
  option`; `Monitor_view` renders + maps keys; `repl_command.ml` parses
  prompt/dot input; `sqlocaml_repl.ml` wires it together. Keys today:
  `space`=pause, `/`=txn filter prompt, `c`=clear filter, `x`=clear log.

## Design

### Part A — table identity on page events (store layer)

**A1. `Store_event.t` page variants gain a `tree : int` field:**

```ocaml
| Page_read  of { txn_id : int64; tree : int; page : int64 }
| Page_write of { txn_id : int64; tree : int; page : int64 }
| Page_alloc of { txn_id : int64; tree : int; page : int64; reused : bool }
| Page_free  of { txn_id : int64; tree : int; page : int64 }
```

Add accessor `val tree_id_of : t -> int option` (the `tree` for page variants,
`None` for all others). Update `pp` to include `tree=N` and keep `label`
unchanged. `Pager_event.t` is **unchanged** — it stays page-only in the storage
layer, so no `storage → store` dependency is introduced.

**A2. Active-tree context in `bt_state`:**

Add `mutable current_tree : tree_id option` (default `None`). Set it at the top
of `bt_get_tree` and `bt_get_tree_ro` — the two functions that *every*
read/write/cursor op funnels through (`get`, `put`, `put_x`, `del`,
`cursor_open`, `seek_ge`). `translate_pager_event` stamps `current_tree` onto
the page event.

`tree` is a plain `int` (= `tree_id`). When `current_tree = None` (a page event
fired with no tree context — e.g. very early startup or background I/O before
any op), stamp `tree = -1` as a sentinel meaning "unknown"
(`Option.value (Pager-stamped) ~default:(-1)`); `tree_id_of` still returns
`Some (-1)`, which simply never matches a real table filter since catalog tree
ids are non-negative.

**A3. Accuracy — best-effort, documented (same spirit as `txn_id` stamping):**

- `Page_read`, `Page_alloc`, `Page_free` fire *inside* the originating tree op,
  while `current_tree` is set → **accurate**.
- `Page_write` fires at WAL-flush time (commit / checkpoint), after the tree ops
  have returned, so it carries the *most recently active* tree, not necessarily
  the true owner of each flushed page → **approximate for multi-table
  transactions and checkpoints**. This is the accepted cost of tracking the tree
  in the store rather than threading per-page ownership through the pager. It
  will be documented on the `tree` field and in `translate_pager_event`.

### Part B — name → tree_id resolution (db layer)

**B1. `Db.tree_of_table : t -> string -> int option`** — resolves a table name
to its storage tree id via `Cat.find_table_cached t.catalog ~name` +
`Cat.tid_of_storage` (handles both `Row` and `Columnar` storage). Returns `None`
for unknown tables. This keeps the store catalog-free: the REPL resolves a name
to a tree id once, at filter-set time, and matches subsequent events by id.

### Part C — monitor UI (`bin/repl/`)

**C1. `Event_log` filter becomes a variant** (replacing `filter : int64
option`):

```ocaml
type filter =
  | No_filter
  | By_txn   of int64
  | By_table of { name : string; tree : int }
```

`visible` matches:
- `No_filter` → all events
- `By_txn id` → `Event.txn_id ev = Some id` (unchanged behaviour)
- `By_table { tree; _ }` → `Event.tree_id_of ev = Some tree` (strict: page
  events for that table only; global txn/checkpoint/WAL events are hidden)

API: `set_filter t filter`, `filter t`, plus the existing
`clear`/`pause`/etc. The `name` is carried only for header display.

**C2. Keys & prompts (`Monitor_view` + `sqlocaml_repl`):**
- `/` — txn-id prompt (unchanged).
- `t` — **new** table-name prompt. Submitted text is resolved with
  `Db.tree_of_table`:
  - found → `set_filter (By_table { name; tree })`, status `filter: tbl=<name>`
  - unknown → status `filter: no such table '<name>'`, filter unchanged
- `c` — clear filter (`No_filter`).
- `x` — clear log (unchanged). `space` — pause (unchanged).
- Header shows the active filter: `all` / `txn=N` / `tbl=<name>`.

The prompt mechanism reuses the existing `filter_mode` flow, generalised to a
small variant so the submit handler knows whether the entered text is a txn id
or a table name:

```ocaml
(* sqlocaml_repl.ml *)
type prompt = No_prompt | Txn_prompt | Table_prompt
```

`repl_command.classify` is extended to take the active `prompt` and emit a
`Filter` action carrying either a parsed txn id or the raw table-name string;
the app resolves the table name (it owns `db_ref`).

**C3. Export — `.dump [path]` dot command:**
- Default path `sqlocaml-events.log` when omitted.
- Writes the **visible** (filter-respecting) events, oldest-first, one per line
  via `Store_event.pp`, to the file. Truncates/overwrites.
- Status line: `dumped N event(s) to <path>` on success; on `Sys_error`,
  `dump failed: <msg>`.
- Implemented as `Event_log.dump : t -> string -> (int, string) result` (pure
  file write over `visible`), called from a new `Dump of string option` dot
  command in `repl_command.ml` and dispatched in `sqlocaml_repl.ml`.

## Testing

Follow repo standard (100% line coverage target, QCheck where non-trivial).

- **`store_event`** — unit: `tree_id_of` returns the tree for each page variant
  and `None` for the rest; `pp` renders `tree=`. QCheck: `tree_id_of` total over
  all variants.
- **store active-tree stamping** — integration in `test/` (file-backed store):
  open two trees, do reads/writes on each, capture events via
  `set_event_callback`, assert `Page_read`/`Page_alloc` carry the expected
  `tree`. Document/accept `Page_write` approximation (assert it carries *a*
  valid tree, not a specific one, for a multi-table txn).
- **`Db.tree_of_table`** — known table → `Some tid` matching the catalog;
  unknown → `None`.
- **`Event_log`** — `visible` under each filter variant (incl. `By_table`
  matching only the right tree); `dump` writes the right line count and respects
  the active filter; round-trip line count == returned count.
- **`repl_command`** — `classify` under `Txn_prompt` vs `Table_prompt`; `.dump`
  / `.dump path` parse to `Dump`.
- **`monitor_view` / `sqlocaml_repl`** — extend existing smoke tests for the new
  `t` key and `.dump` dispatch (no real TTY; exercise handlers directly).

## Out of scope (YAGNI for v1)

- Continuous on-disk tracing / log rotation (only on-demand dump).
- JSON export (text only; revisit if tooling needs it).
- Per-row table-name rendering in the monitor (header shows the filtered table;
  raw `tree=N` is shown on page rows). Reverse tree→name lookup is not needed.
- Per-page write ownership in the pager (would remove the `Page_write`
  approximation but pushes tracking into the storage layer — explicitly declined).

## Files touched

- `lib/store/store_event.ml` / `.mli` — `tree` field, `tree_id_of`, `pp`.
- `lib/store/store.ml` — `current_tree` in `bt_state`; set in `bt_get_tree`,
  `bt_get_tree_ro`; stamp in `translate_pager_event`.
- `lib/db/db.ml` / `.mli` — `tree_of_table`.
- `bin/repl/event_log.ml` / `.mli` — filter variant, `dump`.
- `bin/repl/monitor_view.ml` — `t` key, header.
- `bin/repl/repl_command.ml` — `prompt`-aware `classify`, `Dump` dot command.
- `bin/repl/sqlocaml_repl.ml` — `prompt` state, table-filter resolution, `.dump`
  dispatch.
- `test/` — new + extended tests as above.
