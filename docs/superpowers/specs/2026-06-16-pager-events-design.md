# Internals monitor — page/COW/freelist events (#384) + frames fix (#386)

**Date:** 2026-06-16
**Branch:** `feat/384-pager-events`
**Closes:** #384, #386
**Depends on:** #382 (merged in PR #388 — defines `Store_event.t`, the `on_event` seam, and the monitor pane)

## Goal

Extend the internals-monitor event seam (#382) with the finer-grained storage
events the strace-style monitor wants — per-page reads/writes, page
allocation/free (freelist pop/push), and enough information to infer
copy-on-write (COW) activity. Fold in #386: make `Txn_commit` carry the real
WAL frame count instead of the hardwired `0` placeholder.

Out of scope (separate issues): table-name identity / `tree_id` on events and
event-log export (#385); explicit per-COW events threaded into the btree.

## Decisions (settled during brainstorming)

1. **Page I/O granularity = physical I/O only.** `Page_read` fires on backend
   reads (cache *miss*) only; cache hits emit nothing. `Page_write` fires once
   per dirty page as it is handed to the WAL batch or written to main during a
   flush. This is the true "strace" view with the lowest volume and no firing
   on hot cache hits.
2. **COW = inferred from alloc/free.** No btree changes. The `Page_alloc` event
   carries a `reused` flag; the monitor infers COW as alloc-reused + free pairs
   within a transaction. Keeps the seam confined to `Pager`.
3. **#386 = thread the real count.** Pass the WAL-appended frame count from
   `commit_wal` into the `Txn_commit { frames }` emit. Keep the field; keep
   `Wal_append` as the authoritative batch event. The two now agree.
4. **`Page_write` emits once per dirty page** even in the WAL-batch flush path
   (not one event per batch).
5. **`txn_id` = best-effort, from `Pager.get_txn_id`.** Write-path events
   (alloc / write / free) always fire inside an active RW txn, so they carry the
   exact txn id. `Page_read` can fire outside a write txn; there `get_txn_id`
   returns `0L` before the first txn and the most-recent txn id between txns.
   This best-effort semantics is documented where the translator lives; the
   monitor already tolerates reads without a meaningful txn.

## Architecture & layering

`Pager` and `Freelist` live in `lib/storage`. `Store_event` lives in
`lib/store`, which already depends on `lib/storage`. If `Pager` constructed
`Store_event.t` directly we would create a dependency cycle. So we introduce a
storage-local signal type and translate it one layer up:

```
btree ──calls──▶ Pager (lib/storage)
                   │  mutable on_page_event : (Pager_event.t -> unit) option   [NEW]
                   ▼
              Pager_event.t  (lib/storage)   [NEW — page-level signal]
                   │  translated by Store.set_event_callback
                   ▼
              Store_event.t  (lib/store)     [extended with page variants]
                   ▼
              monitor observer (unchanged consumer)
```

`Pager` never references `Store_event`. `Store.set_event_callback` installs a
translator on the pager that maps `Pager_event.t` → the new `Store_event.t`
variants, stamps `txn_id`, and wraps the user observer in `try/with` — the same
"a faulty observer can never break a transaction" guarantee as the existing
`emit_event`.

## New types

### `lib/storage/pager_event.ml` (+ `.mli`)

```ocaml
type t =
  | Page_read  of { page_id : int64 }                 (* backend read — cache MISS only *)
  | Page_write of { page_id : int64 }                 (* page handed to WAL/main on flush *)
  | Page_alloc of { page_id : int64; reused : bool }  (* reused=true => freelist pop; false => file extend *)
  | Page_free  of { page_id : int64 }                 (* freelist push *)
```

A small, page-level signal. No `txn_id` (stamped by the translator), no
`tree_id` (out of scope, #385).

### `Store_event.t` — four new variants

```ocaml
  | Page_read  of { txn_id : int64; page : int64 }
  | Page_write of { txn_id : int64; page : int64 }
  | Page_alloc of { txn_id : int64; page : int64; reused : bool }
  | Page_free  of { txn_id : int64; page : int64 }
```

Plus matching arms in `pp` and `label` (and `txn_id : t -> int64 option`
returns `Some` for all four). `reused` lets the monitor infer COW.

## Emit sites (Pager) & zero-overhead

Every emit site uses **guard-before-construct** so nothing is allocated when the
monitor is off — stricter than the existing store-level pattern (which builds
the record then checks `None`), because these paths are far hotter:

```ocaml
match t.on_page_event with
| None -> ()
| Some f -> f (Pager_event.Page_read { page_id })
```

- **Page_read** — in `Pager.read`, on the cache-miss branch where `t.read_page`
  is invoked. Cache hits emit nothing.
- **Page_write** — in the flush loop (`flush` / `flush_no_sync` /
  `flush_one_to_main`), once per dirty page as it is handed to the WAL batch or
  written to main.
- **Page_alloc** — in `Pager.alloc`: `reused = true` when it pops the freelist,
  `reused = false` when it extends the file.
- **Page_free** — in `Pager.free`, when the page is pushed to the freelist.

New pager surface:

```ocaml
val set_page_event_callback : t -> (Pager_event.t -> unit) option -> unit
```

backed by a `mutable on_page_event : (Pager_event.t -> unit) option` field,
defaulting to `None`.

## Store wiring

`Store.set_event_callback` (Btree backend) additionally installs/clears the
pager hook:

```ocaml
| Btree st ->
  st.on_event <- cb;
  (match cb with
   | None -> Pager.set_page_event_callback st.pager None
   | Some f ->
     Pager.set_page_event_callback st.pager
       (Some (fun pev ->
          try f (translate_pager_event st pev) with _ -> ())))
```

`translate_pager_event st : Pager_event.t -> Store_event.t` maps each variant
and stamps `txn_id` from `Pager.get_txn_id st.pager` (see decision 5 for its
best-effort semantics on reads outside a txn).

## #386 — real frame count

`commit_wal` already computes `appended = frames_after - frames_before` (and
emits it via `Wal_append`). Thread that value out of `commit_wal` into the
`Txn_commit { txn_id; frames }` emit in `commit`, replacing the hardwired `0`.
Non-WAL / empty commits emit the actual count (`0` only when genuinely nothing
was appended).

## Testing

Repo standard: 100% line coverage on new modules/arms; QCheck on non-trivial
functions.

- **`pager_event.ml`:** pp/label arms; pp never raises (QCheck over generated
  variants).
- **Pager unit tests** (recording callback):
  - cache-miss read emits `Page_read`; a cache *hit* emits nothing;
  - flush emits one `Page_write` per dirty page (including the WAL-batch path);
  - `alloc` emits `Page_alloc{reused=false}` on file-extend and
    `Page_alloc{reused=true}` on freelist reuse;
  - `free` emits `Page_free`;
  - **zero-overhead:** with the callback `None`, no events and no observable
    behavior change.
- **Store-level** (observer over a real txn): an insert txn emits
  `Page_alloc` / `Page_write` / `Page_free` `Store_event`s with the correct
  `txn_id`; a checkpoint/read scenario produces `Page_read`.
- **#386:** `Txn_commit.frames` equals the matching `Wal_append.count` and is
  non-zero for a real write.
- **QCheck invariant:** every `Page_alloc{reused=true}` page-id was previously
  freed in the same session (freelist-reuse invariant).

## Risks

- **Hot-path regression.** Mitigated by guard-before-construct (no allocation
  when off) and a dedicated zero-overhead test. Confirm no measurable change in
  the insert benchmark with the monitor disabled.
- **txn_id sentinel ambiguity.** `0L` for "no active txn" is documented; the
  monitor already tolerates events without a meaningful txn for RO paths.
