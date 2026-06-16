# Internals Monitor — `on_event` seam + nottui REPL shell + monitor pane

**Issue:** #382 (split from #310). **Follow-ups filed:** #384 (page/COW/freelist events), #385 (table-name filtering + log export).

## Background

#310's PR (`fea4c0c`, titled "nottui TUI REPL with grid results, schema pane,
status line") landed only the **dependency scaffold** — `nottui`/`nottui-lwt`/`lwd`
in `bin/repl/dune`, the `sqlocaml_repl` opam package, the CI wiring — and *moved*
`bin/sqlocaml_repl.ml` to `bin/repl/` unchanged. No `Nottui`/`Lwd` code exists
anywhere in `bin/` or `lib/`. The REPL today is still the legacy line-oriented
blocking shell with nested `Lwt_main.run` per query.

So this work delivers both the nottui REPL shell #310 never wrote **and** the
internals monitor of #382, as one effort. #310's status is corrected in the PR
description for this work.

## Goals

- A library-side, Mirage-pure, zero-overhead-when-unused event hook on the store.
- A single-root-loop nottui REPL that preserves all existing REPL behaviour.
- A toggleable "internals monitor" pane that renders engine events live on the
  same Lwt loop, with pause/resume, scroll, and txn-id filtering.

## Non-goals (v1) → follow-ups

- Page read/write, COW, freelist events (need `Pager`/`Freelist` plumbing) → **#384**.
- Table-name filtering; event-log export/persistence → **#385**.
- Full interactive TUI test automation (component unit tests + render-smoke only).

## Architecture / layering

```
lib/store/store_event.ml   — event type (pure; no Store dependency)
lib/store/store.ml         — bt_state.on_event seam; emits at the store/WAL seams
lib/db/db.ml               — set_event_callback passthrough to t.store
bin/repl/  (rewritten as a small multi-module app)
  ├ repl_engine.ml         — reused pure logic (split_stmts, is_query_stmt,
  │                          has_terminator, value_to_string, dot-command SQL,
  │                          open_db) returning data instead of printing
  ├ event_log.ml           — bounded ring buffer + filter/pause state (Lwd vars)
  ├ monitor_view.ml        — nottui rendering of the event log + keybindings
  ├ shell_view.ml          — query input + results grid + status line
  └ sqlocaml_repl.ml       — composes panes, single root Nottui_lwt loop, wiring
```

The library only ever sees an optional `(Store_event.t -> unit)` callback. It does
**not** depend on any streaming/TUI abstraction. Backpressure is solved at the
consumer (the bounded ring buffer in `event_log.ml`), not in `lib/`.

## Component 1 — `lib/store/store_event.ml` (the event type)

A standalone module that `Store` depends on (never the reverse — no cycle). Uses
only `int64`/`int`/`string`.

```ocaml
type t =
  | Txn_begin          of { txn_id : int64 }
  | Txn_commit         of { txn_id : int64; frames : int }
  | Txn_rollback       of { txn_id : int64 }
  | Savepoint_begin    of { txn_id : int64; name : string }
  | Savepoint_release  of { txn_id : int64; name : string }
  | Savepoint_rollback of { txn_id : int64; name : string }
  | Wal_append         of { txn_id : int64; base_idx : int; count : int }
  | Wal_reset          of { epoch : int64 }
  | Checkpoint_begin   of { target_frames : int }
  | Checkpoint_end     of { pages_migrated : int }

val pp     : Format.formatter -> t -> unit
val label  : t -> string          (* short tag for the pane, e.g. "COMMIT" *)
val txn_id : t -> int64 option    (* None for Wal_reset / Checkpoint_* *)
```

**txn-id consistency:** every event of a single rw txn carries the same
*prospective committed id* — `Int64.add header.txn_id 1L` captured at begin and
threaded through commit/rollback/savepoint, so a filter on one id groups the whole
txn lifecycle. (Only one rw writer at a time under `rw_mutex`, so this is stable.)

## Component 2 — `lib/store/store.ml` seam

- Add `mutable on_event : (Store_event.t -> unit) option` to `bt_state`,
  default `None`.
- Helper:
  ```ocaml
  let emit st ev =
    match st.on_event with
    | None -> ()
    | Some f -> (try f ev with _ -> ())   (* a buggy monitor must never break a commit *)
  ```
- Emit sites (all in `store.ml`; **`wal.ml` is left untouched**):
  - rw-begin (near line 1208) → `Txn_begin`
  - `commit` / `commit_wal` (1848 / 1736) → `Txn_commit` (frames = count shipped);
    `Wal_append` emitted where `base_idx`/`count` are known (the
    `on_committed_frames` region ~1803–1833)
  - `rollback` (1894) → `Txn_rollback`
  - `savepoint_begin` / `_release` / `_rollback` (2141 / 2170 / 2192)
  - `checkpoint_unlocked` (1429) → `Checkpoint_begin` (target =
    `Wal.committed_frames`) at start; after `Wal.reset`, `Wal_reset { epoch }`
    and `Checkpoint_end { pages_migrated }` (count from the flush loop)
- `store.mli`: `val set_event_callback : t -> (Store_event.t -> unit) option -> unit`
  (mirrors existing `set_commit_callback`), plus re-export the event type as
  `module Event = Store_event`.
- **Mem backend** has no `bt_state`, so `set_event_callback` is a no-op there and
  emits nothing. Documented; the monitor is meaningful only against a file/WAL db.

## Component 3 — `lib/db/db.ml` passthrough

`Db.t = { mutable store : S.t; ... }`. Add, mirroring `wal_sync_count`:

```ocaml
module Event = S.Event
let set_event_callback t cb = S.set_event_callback t.store cb
```

The `store` field is swapped on `.open`/VACUUM, so the REPL **re-registers** the
callback after any swap.

## Component 4 — nottui REPL shell (`bin/repl/`)

- **Single root loop:** `Lwt_main.run (Nottui_lwt.run ui)` replaces every nested
  `Lwt_main.run`. Query submission becomes async on that loop (await `Db.query`,
  push rows into a results `Lwd.var`).
- **`repl_engine.ml`:** extract the existing pure logic verbatim where possible —
  `value_to_string`, `is_query_stmt`, `has_terminator`, `split_stmts`,
  `open_db`, and the dot-command SQL strings — refactored to *return* data (rows,
  schema text) rather than `print_*`. All current dot-commands preserved:
  `.help .quit/.exit .tables .schema[ name] .open <path> .databases`.
- **`shell_view.ml`:** input line (single-line editor), results grid (column
  headers where available, width-aligned — the grid #310 described), status line
  (db path, backend, row counts, durability mode).
- **Layout:** vertical split, shell on top, monitor below (toggle-hidden, e.g.
  `F2`). `Tab` switches focus.
- **Wiring:** on initial open and after each `.open` swap,
  `Db.set_event_callback db (Some (Event_log.push log))`.

## Component 5 — monitor (`event_log.ml` + `monitor_view.ml`)

- **Ring buffer:** fixed capacity (≈5000), drop-oldest. `push` is O(1) and
  non-blocking — the engine fiber never waits on the terminal.
- **State (Lwd vars):** the event ring, `paused : bool`, `filter : int64 option`,
  scroll offset.
- **Pause semantics:** pause freezes the *view* (and auto-scroll); events keep
  accumulating into the bounded ring.
- **Filtering:** when `filter = Some id`, render only events whose
  `Store_event.txn_id = Some id`; the non-txn events (`Wal_reset`,
  `Checkpoint_*`) are hidden while a txn filter is active (strace-style focus).
- **Keys:** `space` pause/resume, `/` enter a txn-id to filter, `c` clear filter,
  `x` clear log, arrows / `g` / `G` scroll.
- **Empty-state hint:** against a `:memory:` db (Mem backend emits nothing) the
  pane shows "in-memory db: storage events unavailable; open a file db".

## Error handling

- Library `emit` swallows callback exceptions (`try … with _ -> ()`); a faulty
  monitor can never corrupt or abort a transaction.
- REPL query errors update an error line in the shell view; the loop continues
  (preserving today's per-statement error recovery).

## Testing

- **`test/test_store_event.ml`** (file + WAL store): assert exact event
  sequence/fields for commit, rollback, nested savepoints, and checkpoint.
  QCheck: random begin/commit/rollback sequences yield correctly txn-id-tagged
  events; `emit` never raises even when the callback raises (defensive);
  `pp` / `txn_id` properties.
- **Db passthrough test:** `set_event_callback` reaches the store; callback is
  re-registered and fires after a `.open` swap.
- **`event_log` unit tests:** capacity bound (drop-oldest), filter selection,
  pause semantics.
- **Render-smoke tests:** render each view to a fixed-size image and assert
  expected text is present (matching #310's smoke-test approach). Full
  interactive TUI automation is out of scope.
- Coverage target 100% on the new `lib/` modules per repo standard.

## Risks

- **`Nottui_lwt` entry point/signature** confirmed against the installed lib at
  implementation time; loop wiring adapted if it differs from `Nottui_lwt.run`.
- **Hot commit path:** `emit` is O(1), guarded by the `None` check plus `try`;
  negligible when unused. A bench sanity check on the commit path guards against
  regression.
- **txn-id semantics** (begin id vs post-commit header bump): standardize on the
  prospective committed id captured at begin, reused for all of a txn's events.
