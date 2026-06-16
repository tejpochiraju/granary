# Internals Monitor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a Mirage-pure `on_event` hook on the store, build the nottui REPL shell #310 never wrote, and add a toggleable internals-monitor pane that renders engine events live with txn-id filtering.

**Architecture:** A pure `Store_event` sum type lives below `Store`; `Store` emits events at the commit/rollback/savepoint/WAL/checkpoint seams through an optional, `try`-guarded callback (zero overhead when unset). `Db` passes the registration through to its store. The REPL is rewritten as a small multi-module nottui app on a single `Nottui_lwt.run` loop, with a bounded ring buffer decoupling the engine fiber from the terminal.

**Tech Stack:** OCaml 5.4, Lwt, `nottui`/`nottui-lwt`/`lwd`, Alcotest + QCheck, dune. Spec: `docs/superpowers/specs/2026-06-16-382-internals-monitor-design.md`. Follow-ups: #384 (page/COW/freelist events), #385 (table filtering + export).

## Conventions for every build/test step

All OCaml commands run inside the dev container, from the worktree root (`.worktrees/382-monitor`). The prefix below is referred to as `$C`:

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev
```

- Build: `$C dune build`
- Test one suite: `$C dune test test/test_store_event.exe`
- Format-fix a file (write back to host):
  ```sh
  tmp=$(mktemp) && $C ocamlformat lib/store/store_event.ml > "$tmp" && mv "$tmp" lib/store/store_event.ml && chmod 644 lib/store/store_event.ml
  ```

Commit after every task (the pre-commit hook runs format checks on staged `.ml`/`.mli`). Never push to `main`; this work lands via PR from branch `feat/382-internals-monitor`.

## File structure

| File | Responsibility |
|------|----------------|
| `lib/store/store_event.ml` / `.mli` | **Create.** Pure event sum type + `pp`/`label`/`txn_id`. No `Store` dep. |
| `lib/store/store.ml` | **Modify.** `bt_state.on_event` field, `emit` helper, emit at seams, `set_event_callback`, `module Event`. |
| `lib/store/store.mli` | **Modify.** Export `module Event = Store_event`, `val set_event_callback`. |
| `lib/db/db.ml` / `.mli` | **Modify.** `module Event = S.Event`, `set_event_callback` passthrough. |
| `bin/repl/repl_engine.ml` / `.mli` | **Create.** Pure REPL logic returning data (extracted from current `sqlocaml_repl.ml`). |
| `bin/repl/event_log.ml` / `.mli` | **Create.** Bounded ring buffer + filter/pause state as `Lwd` vars. |
| `bin/repl/monitor_view.ml` | **Create.** nottui rendering of the event log + keybindings. |
| `bin/repl/shell_view.ml` | **Create.** nottui query input + results grid + status line. |
| `bin/repl/sqlocaml_repl.ml` | **Rewrite.** Compose panes, single `Nottui_lwt.run` loop, wire callback. |
| `bin/repl/dune` | **Modify.** Add the new modules as a `(library)` + `(executable)` or `(executable (modules ...))`. |
| `test/test_store_event.ml` | **Create.** Event-sequence + QCheck + defensive tests. |
| `test/test_db_event_385.ml` | **Create.** Db passthrough + re-register-after-`.open`. |
| `test/test_repl_components.ml` | **Create.** `repl_engine` + `event_log` unit tests + render-smoke. |
| `test/dune` | **Modify.** Register the three new test executables. |

---

## Task 1: `Store_event` pure module

**Files:**
- Create: `lib/store/store_event.ml`, `lib/store/store_event.mli`
- Test: `test/test_store_event.ml` (created here, extended in Tasks 2–4)
- Modify: `test/dune`

- [ ] **Step 1: Write the `.mli`**

Create `lib/store/store_event.mli`:

```ocaml
(** Engine-internal events surfaced to an optional observer (the internals
    monitor; #382).  Pure — depends on nothing in {!Sqlocaml_store.Store}, so
    {!Store} can depend on it without a cycle.  Events are fire-and-forget; the
    library never blocks on an observer. *)

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

(** Short uppercase tag for display, e.g. ["COMMIT"], ["WAL_APPEND"]. *)
val label : t -> string

(** The transaction id an event belongs to, or [None] for events with no
    single owning txn ([Wal_reset], [Checkpoint_*]).  Used by the monitor's
    txn-id filter. *)
val txn_id : t -> int64 option

val pp : Format.formatter -> t -> unit
```

- [ ] **Step 2: Write the failing test**

Create `test/test_store_event.ml`:

```ocaml
(** Tests for #382 — Store_event type + the Store on_event seam. *)

module Ev = Sqlocaml_store.Store_event

let test_label_and_txn_id () =
  let c = Ev.Txn_commit { txn_id = 7L; frames = 3 } in
  Alcotest.(check string) "label" "COMMIT" (Ev.label c);
  Alcotest.(check (option int64)) "txn_id" (Some 7L) (Ev.txn_id c);
  Alcotest.(check (option int64))
    "wal_reset has no txn"
    None
    (Ev.txn_id (Ev.Wal_reset { epoch = 2L }))
;;

let test_pp_roundtrip_nonempty () =
  let s = Format.asprintf "%a" Ev.pp (Ev.Txn_begin { txn_id = 1L }) in
  Alcotest.(check bool) "pp non-empty" true (String.length s > 0)
;;

let () =
  Alcotest.run
    "store_event"
    [ ( "type"
      , [ Alcotest.test_case "label + txn_id" `Quick test_label_and_txn_id
        ; Alcotest.test_case "pp non-empty" `Quick test_pp_roundtrip_nonempty
        ] )
    ]
;;
```

Add `test_store_event` to the `(names ...)` stanza in `test/dune`.

- [ ] **Step 3: Run the test, verify it fails to build**

Run: `$C dune test test/test_store_event.exe`
Expected: FAIL — `Unbound module Sqlocaml_store.Store_event`.

- [ ] **Step 4: Implement `store_event.ml`**

Create `lib/store/store_event.ml`:

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

let label = function
  | Txn_begin _ -> "BEGIN"
  | Txn_commit _ -> "COMMIT"
  | Txn_rollback _ -> "ROLLBACK"
  | Savepoint_begin _ -> "SP_BEGIN"
  | Savepoint_release _ -> "SP_RELEASE"
  | Savepoint_rollback _ -> "SP_ROLLBACK"
  | Wal_append _ -> "WAL_APPEND"
  | Wal_reset _ -> "WAL_RESET"
  | Checkpoint_begin _ -> "CKPT_BEGIN"
  | Checkpoint_end _ -> "CKPT_END"
;;

let txn_id = function
  | Txn_begin { txn_id } | Txn_commit { txn_id; _ } | Txn_rollback { txn_id }
  | Savepoint_begin { txn_id; _ } | Savepoint_release { txn_id; _ }
  | Savepoint_rollback { txn_id; _ } | Wal_append { txn_id; _ } -> Some txn_id
  | Wal_reset _ | Checkpoint_begin _ | Checkpoint_end _ -> None
;;

let pp fmt ev =
  let tag = label ev in
  match ev with
  | Txn_begin { txn_id } | Txn_rollback { txn_id } ->
    Format.fprintf fmt "%s txn=%Ld" tag txn_id
  | Txn_commit { txn_id; frames } ->
    Format.fprintf fmt "%s txn=%Ld frames=%d" tag txn_id frames
  | Savepoint_begin { txn_id; name }
  | Savepoint_release { txn_id; name }
  | Savepoint_rollback { txn_id; name } ->
    Format.fprintf fmt "%s txn=%Ld name=%s" tag txn_id name
  | Wal_append { txn_id; base_idx; count } ->
    Format.fprintf fmt "%s txn=%Ld base=%d count=%d" tag txn_id base_idx count
  | Wal_reset { epoch } -> Format.fprintf fmt "%s epoch=%Ld" tag epoch
  | Checkpoint_begin { target_frames } ->
    Format.fprintf fmt "%s target=%d" tag target_frames
  | Checkpoint_end { pages_migrated } ->
    Format.fprintf fmt "%s migrated=%d" tag pages_migrated
;;
```

- [ ] **Step 5: Run the test, verify it passes**

Run: `$C dune test test/test_store_event.exe`
Expected: PASS (2 cases).

- [ ] **Step 6: Format + commit**

```sh
for f in lib/store/store_event.ml lib/store/store_event.mli test/test_store_event.ml; do
  tmp=$(mktemp) && $C ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"; done
git add lib/store/store_event.ml lib/store/store_event.mli test/test_store_event.ml test/dune
git commit -m "feat(#382): pure Store_event type (label/txn_id/pp)"
```

---

## Task 2: `on_event` seam — field, emit, set_event_callback, txn lifecycle

**Files:**
- Modify: `lib/store/store.ml` (bt_state ~line 186; `rw_begin` ~1174/1208; `commit_wal` ~1736; `commit` ~1848; `rollback` ~1894; add `set_event_callback`, `module Event` near `set_commit_callback` ~3122)
- Modify: `lib/store/store.mli` (add `module Event`, `val set_event_callback`)
- Modify: `test/test_store_event.ml`

- [ ] **Step 1: Write the failing test (append to `test/test_store_event.ml`)**

Add this helper block and suite. It opens a real file+WAL store and records event labels.

```ocaml
module S = struct
  include Sqlocaml_store.Store

  let open_file_wal = Sqlocaml_unix.Store.open_file_wal
end

let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_event_%04d.db" n
;;

let cleanup path =
  (try Unix.unlink path with _ -> ());
  (try Unix.unlink (path ^ "-wal") with _ -> ())
;;

(* Collect event labels into a ref while [f] runs against a fresh store. *)
let with_recorder ~f =
  let path = fresh_path () in
  cleanup path;
  let seen = ref [] in
  Lwt.finalize
    (fun () ->
       let open Lwt.Syntax in
       let* st = S.open_file_wal ~path () in
       let st = Result.get_ok st in
       S.set_event_callback st (Some (fun ev -> seen := Ev.label ev :: !seen));
       let* () = f st in
       let* () = S.close st in
       Lwt.return (List.rev !seen))
    (fun () -> cleanup path; Lwt.return_unit)
  |> run
;;

let bs = Bytes.of_string

let test_commit_emits_begin_and_commit () =
  let labels =
    with_recorder ~f:(fun st ->
      let open Lwt.Syntax in
      let* txn = S.rw_begin st in
      let* () = S.put txn S.main_tree (bs "k") (bs "v") in
      S.commit txn)
  in
  Alcotest.(check bool) "has BEGIN" true (List.mem "BEGIN" labels);
  Alcotest.(check bool) "has COMMIT" true (List.mem "COMMIT" labels)
;;

let test_rollback_emits_rollback () =
  let labels =
    with_recorder ~f:(fun st ->
      let open Lwt.Syntax in
      let* txn = S.rw_begin st in
      let* () = S.put txn S.main_tree (bs "k") (bs "v") in
      S.rollback txn)
  in
  Alcotest.(check bool) "has ROLLBACK" true (List.mem "ROLLBACK" labels);
  Alcotest.(check bool) "no COMMIT" false (List.mem "COMMIT" labels)
;;
```

> **Note for the implementer:** confirm the exact names for "put a key" and the main tree id by grepping `store.mli` (`grep -nE "val put|main_tree|val rw_begin" lib/store/store.mli`). Substitute the real names for `S.put`/`S.main_tree` if they differ; the assertions stay the same.

Register the new cases in the existing `Alcotest.run` call by adding a suite:

```ocaml
    ; ( "seam"
      , [ Alcotest.test_case "commit emits begin+commit" `Quick
            test_commit_emits_begin_and_commit
        ; Alcotest.test_case "rollback emits rollback" `Quick
            test_rollback_emits_rollback
        ] )
```

- [ ] **Step 2: Run the test, verify it fails**

Run: `$C dune test test/test_store_event.exe`
Expected: FAIL — `Unbound value Sqlocaml_store.Store.set_event_callback`.

- [ ] **Step 3: Add the `on_event` field to `bt_state`**

In `lib/store/store.ml`, immediately after the `on_committed_frames` field (~line 193, before `mutable follower`):

```ocaml
  ; mutable on_event : (Store_event.t -> unit) option
    (* #382: optional, synchronous, fire-and-forget observer for internal
       events (the internals monitor).  [None] = zero overhead.  Invoked via
       [emit_event], which swallows any exception so a faulty observer can
       never break a transaction.  Btree backend only — Mem has no bt_state. *)
```

Find the record-construction site(s) for `bt_state` (grep `on_committed_frames = None`) and add `; on_event = None` alongside each.

- [ ] **Step 4: Add the `emit_event` helper**

Near the top of the store implementation, after `bt_state` is in scope (e.g. just before `rw_begin`), add:

```ocaml
let emit_event (st : bt_state) (ev : Store_event.t) =
  match st.on_event with
  | None -> ()
  | Some f -> ( try f ev with _ -> () )
;;

(* The id the currently-active rw txn will commit as.  The header is not bumped
   until commit, so every event of one txn shares this id (one writer at a time
   under the write lock). *)
let active_txn_id (st : bt_state) = Int64.add st.current_header.txn_id 1L
```

- [ ] **Step 5: Emit at the three seams**

In `rw_begin`, inside the `| Btree st ->` branch, right after `let current_rw_txn_id = Int64.add st.current_header.txn_id 1L in` (~line 1208):

```ocaml
       emit_event st (Store_event.Txn_begin { txn_id = current_rw_txn_id });
```

In `commit` (`let commit (Rw t : rw txn) ...`, ~1848): capture the id before any header bump and emit after the commit succeeds. Add at the start of the `Btree`-backed path (where `st` is bound — follow the existing structure of `commit`/`commit_wal`):

```ocaml
       let committed_id = active_txn_id st in
```

and immediately before the function returns `Lwt.return_unit` for the Btree path, emit:

```ocaml
       emit_event st (Store_event.Txn_commit { txn_id = committed_id; frames = !frames_shipped });
```

where `frames_shipped` is the count of WAL frames in this commit. If `commit_wal` does not already have that count in scope, set `frames = 0` for now (Task 4 wires the precise count via the `Wal_append` event). Keep it simple: `frames = 0` here is acceptable; `Wal_append` carries the authoritative count.

In `rollback` (`let rollback (Rw t : rw txn) ...`, ~1894), inside the `Btree st` branch:

```ocaml
       emit_event st (Store_event.Txn_rollback { txn_id = active_txn_id st });
```

- [ ] **Step 6: Add `set_event_callback` + `module Event`**

In `lib/store/store.ml`, near `set_commit_callback` (~3122):

```ocaml
let set_event_callback (t : t) (cb : (Store_event.t -> unit) option) =
  match t.backend with
  | Mem _ -> ()  (* Mem backend has no bt_state; emits no events (#382). *)
  | Btree st -> st.on_event <- cb
;;

module Event = Store_event
```

In `lib/store/store.mli`, add near `set_commit_callback` (~600):

```ocaml
(** Re-export of the internal-events type (#382). *)
module Event = Store_event

(** Register (or clear with [None]) a synchronous, fire-and-forget observer for
    internal engine events — the internals monitor.  No-op on the in-memory
    backend (it has no storage seams).  The callback must not raise; any
    exception it throws is swallowed so it cannot break a transaction. *)
val set_event_callback : t -> (Event.t -> unit) option -> unit
```

- [ ] **Step 7: Run the test, verify it passes**

Run: `$C dune test test/test_store_event.exe`
Expected: PASS (all cases). If `S.put`/`S.main_tree` names were wrong, fix per the Step-1 note and re-run.

- [ ] **Step 8: Full build (the seam touches a hot path — make sure nothing else broke)**

Run: `$C dune build`
Expected: clean build.

- [ ] **Step 9: Format + commit**

```sh
for f in lib/store/store.ml lib/store/store.mli test/test_store_event.ml; do
  tmp=$(mktemp) && $C ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"; done
git add lib/store/store.ml lib/store/store.mli test/test_store_event.ml
git commit -m "feat(#382): on_event seam + set_event_callback; emit txn begin/commit/rollback"
```

---

## Task 3: Emit savepoint events

**Files:**
- Modify: `lib/store/store.ml` (`savepoint_begin` ~2141, `savepoint_release` ~2170, `savepoint_rollback` ~2192)
- Modify: `test/test_store_event.ml`

- [ ] **Step 1: Write the failing test (append)**

```ocaml
let test_savepoint_events () =
  let labels =
    with_recorder ~f:(fun st ->
      let open Lwt.Syntax in
      let* txn = S.rw_begin st in
      let* () = S.savepoint_begin txn "sp1" in
      let* () = S.put txn S.main_tree (bs "k") (bs "v") in
      let* () = S.savepoint_rollback txn "sp1" in
      let* () = S.savepoint_release txn "sp1" in
      S.commit txn)
  in
  Alcotest.(check bool) "SP_BEGIN" true (List.mem "SP_BEGIN" labels);
  Alcotest.(check bool) "SP_ROLLBACK" true (List.mem "SP_ROLLBACK" labels);
  Alcotest.(check bool) "SP_RELEASE" true (List.mem "SP_RELEASE" labels)
;;
```

Register it in the `"seam"` suite list.

> **Note:** confirm the savepoint API names/order in `store.mli` (~170–180). If `savepoint_release` of a rolled-back savepoint is illegal, drop that line and the `SP_RELEASE` assertion, then add a separate begin→release case.

- [ ] **Step 2: Run, verify fail**

Run: `$C dune test test/test_store_event.exe`
Expected: FAIL — `SP_BEGIN` not found (no emit yet).

- [ ] **Step 3: Emit in each savepoint function**

In `savepoint_begin (Rw t : rw txn) name`, in the `Btree st` branch:

```ocaml
       emit_event st (Store_event.Savepoint_begin { txn_id = active_txn_id st; name });
```

In `savepoint_release ... name`:

```ocaml
       emit_event st (Store_event.Savepoint_release { txn_id = active_txn_id st; name });
```

In `savepoint_rollback ... name`:

```ocaml
       emit_event st (Store_event.Savepoint_rollback { txn_id = active_txn_id st; name });
```

- [ ] **Step 4: Run, verify pass**

Run: `$C dune test test/test_store_event.exe`
Expected: PASS.

- [ ] **Step 5: Format + commit**

```sh
tmp=$(mktemp) && $C ocamlformat lib/store/store.ml > "$tmp" && mv "$tmp" lib/store/store.ml && chmod 644 lib/store/store.ml
tmp=$(mktemp) && $C ocamlformat test/test_store_event.ml > "$tmp" && mv "$tmp" test/test_store_event.ml && chmod 644 test/test_store_event.ml
git add lib/store/store.ml test/test_store_event.ml
git commit -m "feat(#382): emit savepoint begin/release/rollback events"
```

---

## Task 4: Emit WAL append + checkpoint + reset events; defensive + QCheck props

**Files:**
- Modify: `lib/store/store.ml` (`commit_wal` ~1736 / the `on_committed_frames` region ~1803–1833; `checkpoint_unlocked` ~1429)
- Modify: `test/test_store_event.ml`

- [ ] **Step 1: Write the failing tests (append)**

```ocaml
let test_wal_append_and_checkpoint () =
  let labels =
    with_recorder ~f:(fun st ->
      let open Lwt.Syntax in
      let* txn = S.rw_begin st in
      let* () = S.put txn S.main_tree (bs "k") (bs "v") in
      let* () = S.commit txn in
      S.checkpoint st)
  in
  Alcotest.(check bool) "WAL_APPEND" true (List.mem "WAL_APPEND" labels);
  Alcotest.(check bool) "CKPT_BEGIN" true (List.mem "CKPT_BEGIN" labels);
  Alcotest.(check bool) "CKPT_END" true (List.mem "CKPT_END" labels);
  Alcotest.(check bool) "WAL_RESET" true (List.mem "WAL_RESET" labels)
;;

(* Defensive: a raising callback must not break the commit. *)
let test_raising_callback_is_swallowed () =
  let path = fresh_path () in
  cleanup path;
  let ok =
    Lwt.finalize
      (fun () ->
         let open Lwt.Syntax in
         let* st = S.open_file_wal ~path () in
         let st = Result.get_ok st in
         S.set_event_callback st (Some (fun _ -> failwith "boom"));
         let* txn = S.rw_begin st in
         let* () = S.put txn S.main_tree (bs "k") (bs "v") in
         let* () = S.commit txn in
         let* () = S.close st in
         Lwt.return true)
      (fun () -> cleanup path; Lwt.return_unit)
    |> run
  in
  Alcotest.(check bool) "commit survived a raising observer" true ok
;;
```

Register both in the `"seam"` suite.

> **Note:** confirm `S.checkpoint : t -> unit Lwt.t` exists (store.mli:274). If checkpoint needs the WAL to exceed a threshold, the single committed frame above still forces a manual `checkpoint` to run the migrate+reset path.

- [ ] **Step 2: Run, verify fail**

Run: `$C dune test test/test_store_event.exe`
Expected: FAIL — `WAL_APPEND` not found.

- [ ] **Step 3: Emit `Wal_append` in `commit_wal`**

In the region where `base_idx`/`count` for the committed batch are known (the same place `on_committed_frames` is fired, ~1803–1833), add — using whatever local names hold the batch base index and frame count (grep for `base_idx`/`count` in `commit_wal`):

```ocaml
       emit_event st
         (Store_event.Wal_append
            { txn_id = committed_id; base_idx; count });
```

If `count > 0`, this is also the authoritative frame count for the `Txn_commit` event from Task 2 — update that emit to use `count` instead of `0` if it is in scope; otherwise leave `Txn_commit.frames = 0` (the monitor reads frame counts off `WAL_APPEND`).

- [ ] **Step 4: Emit checkpoint + reset in `checkpoint_unlocked`**

At the start of `checkpoint_unlocked (st : bt_state) (wal : Wal.t)` (~1429), after the target frame count is read:

```ocaml
  emit_event st (Store_event.Checkpoint_begin { target_frames = Wal.committed_frames wal });
```

Track migrated pages with a local counter incremented in the page-flush loop (where `Pager.flush_one_to_main` is called), then immediately after `Wal.reset wal` (~1483):

```ocaml
  emit_event st (Store_event.Wal_reset { epoch = Wal.epoch wal });
  emit_event st (Store_event.Checkpoint_end { pages_migrated = !migrated });
```

> Use the existing migrate-loop variable if one already counts pages; otherwise add `let migrated = ref 0 in` near the loop and `incr migrated` per flushed page.

- [ ] **Step 5: Run, verify pass**

Run: `$C dune test test/test_store_event.exe`
Expected: PASS (all seam cases + defensive).

- [ ] **Step 6: Add a QCheck property (random txn outcomes are correctly tagged)**

Append to `test/test_store_event.ml`:

```ocaml
(* Property: for a random sequence of committed/rolled-back txns, every emitted
   txn-lifecycle event carries a txn_id, and the count of COMMIT events equals
   the number of committed txns. *)
let prop_commit_count =
  QCheck.Test.make
    ~count:50
    ~name:"commit count matches"
    QCheck.(list bool) (* true = commit, false = rollback *)
    (fun outcomes ->
       let path = fresh_path () in
       cleanup path;
       let commits = ref 0 in
       Lwt.finalize
         (fun () ->
            let open Lwt.Syntax in
            let* st = S.open_file_wal ~path () in
            let st = Result.get_ok st in
            S.set_event_callback st
              (Some (fun ev -> match ev with
                 | Ev.Txn_commit _ -> incr commits
                 | _ -> ()));
            let* () =
              Lwt_list.iter_s
                (fun commit ->
                   let open Lwt.Syntax in
                   let* txn = S.rw_begin st in
                   let* () = S.put txn S.main_tree (bs "k") (bs "v") in
                   if commit then S.commit txn else S.rollback txn)
                outcomes
            in
            let* () = S.close st in
            Lwt.return_unit)
         (fun () -> cleanup path; Lwt.return_unit)
       |> run;
       !commits = List.length (List.filter Fun.id outcomes))
;;
```

Register with `QCheck_alcotest.to_alcotest prop_commit_count` in a new `"props"` suite:

```ocaml
    ; ("props", [ QCheck_alcotest.to_alcotest prop_commit_count ])
```

Ensure `test/dune` lists `qcheck-alcotest` for this executable (grep an existing QCheck test's dune entry to copy the `(libraries ...)` additions — likely a per-test `(library ...)` is not used; the repo uses a shared `(tests (libraries ... qcheck-core qcheck-alcotest alcotest ...))`).

- [ ] **Step 7: Run full event suite + full build**

Run: `$C dune test test/test_store_event.exe && $C dune build`
Expected: PASS + clean build.

- [ ] **Step 8: Format + commit**

```sh
for f in lib/store/store.ml test/test_store_event.ml; do
  tmp=$(mktemp) && $C ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"; done
git add lib/store/store.ml test/test_store_event.ml test/dune
git commit -m "feat(#382): emit WAL append/reset + checkpoint events; defensive + QCheck props"
```

---

## Task 5: `Db` passthrough + re-register-after-`.open` test

**Files:**
- Modify: `lib/db/db.ml` (after `wal_sync_count`, ~237), `lib/db/db.mli` (after `wal_sync_count`, ~114)
- Create: `test/test_db_event_385.ml`
- Modify: `test/dune`

- [ ] **Step 1: Write the failing test**

Create `test/test_db_event_385.ml`:

```ocaml
(** Db.set_event_callback passthrough (#382). *)

module D = struct
  include Sqlocaml.Db

  let open_file_wal = Sqlocaml_unix.open_file_wal
end

let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_dbevent_%04d.db" n
;;

let cleanup path =
  (try Unix.unlink path with _ -> ());
  (try Unix.unlink (path ^ "-wal") with _ -> ())
;;

let test_passthrough_fires () =
  let path = fresh_path () in
  cleanup path;
  let seen = ref 0 in
  let n =
    Lwt.finalize
      (fun () ->
         let open Lwt.Syntax in
         let* db = D.open_file_wal ~path () in
         let db = Result.get_ok db in
         D.set_event_callback db (Some (fun _ -> incr seen));
         let* _ = D.execute db "CREATE TABLE t(x)" in
         let* _ = D.execute db "INSERT INTO t VALUES (1)" in
         let* () = D.close db in
         Lwt.return !seen)
      (fun () -> cleanup path; Lwt.return_unit)
    |> run
  in
  Alcotest.(check bool) "events fired through Db" true (n > 0)
;;

let () =
  Sqlocaml_unix.install ();
  Alcotest.run "db_event"
    [ ("passthrough", [ Alcotest.test_case "fires" `Quick test_passthrough_fires ]) ]
;;
```

Add `test_db_event_385` to `test/dune` `(names ...)`.

- [ ] **Step 2: Run, verify fail**

Run: `$C dune test test/test_db_event_385.exe`
Expected: FAIL — `Unbound value Sqlocaml.Db.set_event_callback`.

- [ ] **Step 3: Implement passthrough**

In `lib/db/db.ml` after `let wal_sync_count t = S.wal_sync_count t.store` (~237):

```ocaml
module Event = S.Event
let set_event_callback t cb = S.set_event_callback t.store cb
```

In `lib/db/db.mli` after `val wal_sync_count : t -> int` (~114):

```ocaml
(** Re-export of the internal-events type (#382). *)
module Event = Sqlocaml_store.Store.Event

(** Register/clear the internals-monitor observer on this database's store.
    No-op for in-memory databases.  See {!Sqlocaml_store.Store.set_event_callback}. *)
val set_event_callback : t -> (Event.t -> unit) option -> unit
```

> Confirm the module path `Sqlocaml_store.Store.Event` resolves from `db.mli`'s scope (db.ml uses `module S = Sqlocaml_store.Store`). If the `.mli` cannot see `S`, write `module Event = Sqlocaml_store.Store.Event` (fully qualified, as above).

- [ ] **Step 4: Run, verify pass**

Run: `$C dune test test/test_db_event_385.exe`
Expected: PASS.

- [ ] **Step 5: Format + commit**

```sh
for f in lib/db/db.ml lib/db/db.mli test/test_db_event_385.ml; do
  tmp=$(mktemp) && $C ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"; done
git add lib/db/db.ml lib/db/db.mli test/test_db_event_385.ml test/dune
git commit -m "feat(#382): Db.set_event_callback passthrough to store"
```

---

## Task 6: `repl_engine` — extract pure REPL logic returning data

**Files:**
- Create: `bin/repl/repl_engine.ml`, `bin/repl/repl_engine.mli`
- Create: `test/test_repl_components.ml`
- Modify: `bin/repl/dune`, `test/dune`

The current `bin/repl/sqlocaml_repl.ml` mixes pure logic with `print_*`. Move the pure parts here unchanged in behaviour, returning values.

- [ ] **Step 1: Write the `.mli`**

Create `bin/repl/repl_engine.mli`:

```ocaml
(** Pure REPL helpers, independent of any UI.  Extracted from the original
    blocking shell so both the terminal logic and the nottui views can share
    them (#382). *)

module Db = Sqlocaml.Db

(** Render one engine value as a display string (NULL/int/real/text/blob). *)
val value_to_string : Db.value -> string

(** True iff the statement is a row-returning query (SELECT/WITH/EXPLAIN/
    VALUES/PRAGMA). *)
val is_query_stmt : string -> bool

(** True iff [buf] contains a [;] terminator outside any quoted string. *)
val has_terminator : Buffer.t -> bool

(** Split a multi-statement string into trimmed statements, respecting quotes. *)
val split_stmts : string -> string list

(** Open a db at [path] ([":memory:"] for in-memory). *)
val open_db : path:string -> (Db.t, Db.error) result Lwt.t

(** Column widths for a pipe-aligned table render of [rows]. *)
val column_widths : Db.row list -> int array
```

- [ ] **Step 2: Write the failing test**

Create `test/test_repl_components.ml`:

```ocaml
module E = Repl_engine

let test_is_query_stmt () =
  Alcotest.(check bool) "select" true (E.is_query_stmt "SELECT 1");
  Alcotest.(check bool) "with" true (E.is_query_stmt "  with x as (..) select ..");
  Alcotest.(check bool) "insert" false (E.is_query_stmt "INSERT INTO t VALUES (1)");
  Alcotest.(check bool) "empty" false (E.is_query_stmt "   ")
;;

let test_split_stmts_respects_quotes () =
  Alcotest.(check (list string)) "two stmts"
    [ "SELECT 1"; "SELECT 2" ]
    (E.split_stmts "SELECT 1; SELECT 2;");
  Alcotest.(check (list string)) "semicolon in string is not a split"
    [ "INSERT INTO t VALUES ('a;b')" ]
    (E.split_stmts "INSERT INTO t VALUES ('a;b');")
;;

let test_has_terminator () =
  let b = Buffer.create 16 in
  Buffer.add_string b "SELECT 1";
  Alcotest.(check bool) "no term" false (E.has_terminator b);
  Buffer.add_char b ';';
  Alcotest.(check bool) "term" true (E.has_terminator b)
;;

let () =
  Alcotest.run "repl_components"
    [ ( "repl_engine"
      , [ Alcotest.test_case "is_query_stmt" `Quick test_is_query_stmt
        ; Alcotest.test_case "split_stmts" `Quick test_split_stmts_respects_quotes
        ; Alcotest.test_case "has_terminator" `Quick test_has_terminator
        ] )
    ]
;;
```

For `test/dune` and the bin `dune`, see Step 4 (the bin modules must be exposed to the test as a library — restructure `bin/repl/dune` so the shared modules live in a `(library (name repl_lib))` and the executable depends on it).

- [ ] **Step 3: Restructure `bin/repl/dune`**

Replace `bin/repl/dune` with:

```lisp
(library
 (name repl_lib)
 (modules repl_engine event_log monitor_view shell_view)
 (libraries sqlocaml sqlocaml.unix lwt lwt.unix lwd nottui nottui-lwt notty)
 (preprocess (pps lwt_ppx)))

(executable
 (name sqlocaml_repl)
 (public_name sqlocaml_repl)
 (modules sqlocaml_repl)
 (libraries repl_lib sqlocaml sqlocaml.unix lwt lwt.unix lwd nottui nottui-lwt)
 (preprocess (pps lwt_ppx)))
```

> `event_log`/`monitor_view`/`shell_view` are created in later tasks; dune accepts the `(modules ...)` list only once the files exist. To keep each task building, add module names to the `(library)` stanza **as you create them** — for Task 6 the library `(modules repl_engine)` only; widen it in Tasks 7–9.

So for Task 6, the library stanza is:

```lisp
(library
 (name repl_lib)
 (modules repl_engine)
 (libraries sqlocaml sqlocaml.unix lwt lwt.unix)
 (preprocess (pps lwt_ppx)))
```

Add `test_repl_components` to `test/dune` `(names ...)`, and add `repl_lib` to the test executable's libraries (the shared `(tests (libraries ...))` stanza).

- [ ] **Step 4: Run, verify fail**

Run: `$C dune test test/test_repl_components.exe`
Expected: FAIL — `Unbound module Repl_engine`.

- [ ] **Step 5: Implement `repl_engine.ml`**

Create `bin/repl/repl_engine.ml` by lifting the existing pure functions from the original `sqlocaml_repl.ml` (value_to_string:24, is_query_stmt:68, has_terminator:94, split_stmts:249, open_db:117) verbatim, plus `column_widths` factored out of `print_rows`:

```ocaml
open Lwt.Syntax
module Db = Sqlocaml.Db

let value_to_string = function
  | Db.V_null -> "NULL"
  | Db.V_int n -> Int64.to_string n
  | Db.V_real f -> Printf.sprintf "%.17g" f
  | Db.V_text s -> s
  | Db.V_blob b -> Printf.sprintf "<blob:%d>" (Bytes.length b)
;;

let is_query_stmt sql =
  let s = String.trim sql in
  if s = "" then false
  else (
    let upper = String.uppercase_ascii s in
    let stop c = c = ' ' || c = '\n' || c = '\t' in
    let len = String.length upper in
    let rec end_of_word i = if i >= len || stop upper.[i] then i else end_of_word (i + 1) in
    let i = end_of_word 0 in
    match String.sub upper 0 i with
    | "SELECT" | "WITH" | "EXPLAIN" | "VALUES" | "PRAGMA" -> true
    | _ -> false)
;;

let has_terminator buf =
  let s = Buffer.contents buf in
  let n = String.length s in
  let rec loop i in_sq in_dq =
    if i >= n then false
    else (
      let c = s.[i] in
      if c = '\'' && not in_dq then loop (i + 1) (not in_sq) in_dq
      else if c = '"' && not in_sq then loop (i + 1) in_sq (not in_dq)
      else if c = ';' && (not in_sq) && not in_dq then true
      else loop (i + 1) in_sq in_dq)
  in
  loop 0 false false
;;

let split_stmts text =
  let n = String.length text in
  let rec scan i acc cur in_sq in_dq =
    if i >= n then (
      let last = String.trim cur in
      if last = "" then List.rev acc else List.rev (last :: acc))
    else (
      let c = text.[i] in
      if c = '\'' && not in_dq then scan (i + 1) acc (cur ^ String.make 1 c) (not in_sq) in_dq
      else if c = '"' && not in_sq then scan (i + 1) acc (cur ^ String.make 1 c) in_sq (not in_dq)
      else if c = ';' && (not in_sq) && not in_dq then (
        let s = String.trim cur in
        let acc' = if s = "" then acc else s :: acc in
        scan (i + 1) acc' "" in_sq in_dq)
      else scan (i + 1) acc (cur ^ String.make 1 c) in_sq in_dq)
  in
  scan 0 [] "" false false
;;

let open_db ~path =
  if path = ":memory:" then
    let* d = Db.open_in_memory () in
    Lwt.return (Ok d)
  else Sqlocaml_unix.open_file ~path ()
;;

let column_widths rows =
  match rows with
  | [] -> [||]
  | first :: _ ->
    let n_cols = Array.length first in
    let widths = Array.make n_cols 0 in
    List.iter
      (fun row ->
         Array.iteri
           (fun i v ->
              let len = String.length (value_to_string v) in
              if len > widths.(i) then widths.(i) <- len)
           row)
      rows;
    widths
;;
```

- [ ] **Step 6: Run, verify pass + build**

Run: `$C dune test test/test_repl_components.exe && $C dune build`
Expected: PASS + clean (the original `sqlocaml_repl.ml` still compiles — it keeps its own copies until Task 10 rewrites it; the dune `(modules sqlocaml_repl)` split prevents a clash).

- [ ] **Step 7: Format + commit**

```sh
for f in bin/repl/repl_engine.ml bin/repl/repl_engine.mli test/test_repl_components.ml; do
  tmp=$(mktemp) && $C ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"; done
git add bin/repl/repl_engine.ml bin/repl/repl_engine.mli bin/repl/dune test/test_repl_components.ml test/dune
git commit -m "feat(#382): extract pure repl_engine (shared by terminal + nottui views)"
```

---

## Task 7: `event_log` — bounded ring buffer + filter/pause state

**Files:**
- Create: `bin/repl/event_log.ml`, `bin/repl/event_log.mli`
- Modify: `bin/repl/dune` (widen library `(modules repl_engine event_log)`)
- Modify: `test/test_repl_components.ml`

- [ ] **Step 1: Write the `.mli`**

Create `bin/repl/event_log.mli`:

```ocaml
(** Bounded, drop-oldest ring buffer of engine events for the monitor pane,
    plus pause/filter UI state held as [Lwd] vars so the view re-renders
    reactively (#382). *)

module Event = Sqlocaml.Db.Event

type t

(** [create ~capacity] makes an empty log holding at most [capacity] events
    (oldest dropped on overflow). *)
val create : capacity:int -> t

(** Append an event.  O(1), non-blocking — safe to call from the engine fiber.
    No-op for the rendered view while paused?  No: push always records; pause
    only freezes the *view* (see {!paused}). *)
val push : t -> Event.t -> unit

(** Current events, oldest-first, after applying the active txn-id filter. *)
val visible : t -> Event.t list

(** Total events currently retained (ignoring the filter). *)
val length : t -> int

val set_filter : t -> int64 option -> unit
val filter : t -> int64 option
val toggle_pause : t -> unit
val paused : t -> bool
val clear : t -> unit

(** The [Lwd] root that the monitor view observes; changes whenever the buffer,
    filter, or pause flag changes. *)
val state_var : t -> unit Lwd.var
```

- [ ] **Step 2: Write the failing test (append to `test/test_repl_components.ml`)**

```ocaml
module L = Event_log
module Ev = Sqlocaml.Db.Event

let mk_commit id = Ev.Txn_commit { txn_id = id; frames = 0 }

let test_ring_capacity () =
  let l = L.create ~capacity:3 in
  List.iter (fun i -> L.push l (mk_commit (Int64.of_int i))) [ 1; 2; 3; 4; 5 ];
  Alcotest.(check int) "capped at 3" 3 (L.length l);
  let ids = List.filter_map Ev.txn_id (L.visible l) in
  Alcotest.(check (list int64)) "oldest dropped" [ 3L; 4L; 5L ] ids
;;

let test_filter () =
  let l = L.create ~capacity:10 in
  List.iter (fun i -> L.push l (mk_commit (Int64.of_int i))) [ 1; 2; 3 ];
  L.set_filter l (Some 2L);
  Alcotest.(check (list int64)) "only txn 2"
    [ 2L ] (List.filter_map Ev.txn_id (L.visible l));
  L.set_filter l None;
  Alcotest.(check int) "filter cleared" 3 (List.length (L.visible l))
;;

let test_pause_toggle () =
  let l = L.create ~capacity:10 in
  Alcotest.(check bool) "starts unpaused" false (L.paused l);
  L.toggle_pause l;
  Alcotest.(check bool) "paused" true (L.paused l)
;;
```

Register a new suite `"event_log"` with these three cases in the `Alcotest.run` list.

- [ ] **Step 3: Run, verify fail**

Run: `$C dune test test/test_repl_components.exe`
Expected: FAIL — `Unbound module Event_log`.

- [ ] **Step 4: Implement `event_log.ml`**

Create `bin/repl/event_log.ml`:

```ocaml
module Event = Sqlocaml.Db.Event

type t =
  { capacity : int
  ; q : Event.t Queue.t
  ; mutable filter : int64 option
  ; mutable paused : bool
  ; state : unit Lwd.var
  }

let create ~capacity =
  { capacity; q = Queue.create (); filter = None; paused = false; state = Lwd.var () }
;;

let bump t = Lwd.set t.state ()

let push t ev =
  Queue.push ev t.q;
  while Queue.length t.q > t.capacity do ignore (Queue.pop t.q) done;
  bump t
;;

let length t = Queue.length t.q

let visible t =
  let all = List.of_seq (Queue.to_seq t.q) in
  match t.filter with
  | None -> all
  | Some id -> List.filter (fun ev -> Event.txn_id ev = Some id) all
;;

let set_filter t f = t.filter <- f; bump t
let filter t = t.filter
let toggle_pause t = t.paused <- not t.paused; bump t
let paused t = t.paused
let clear t = Queue.clear t.q; bump t
let state_var t = t.state
```

- [ ] **Step 5: Run, verify pass**

Run: `$C dune test test/test_repl_components.exe`
Expected: PASS.

- [ ] **Step 6: Widen the library modules + format + commit**

Edit `bin/repl/dune` library stanza to `(modules repl_engine event_log)` and add `lwd` to its `(libraries ...)`.

```sh
for f in bin/repl/event_log.ml bin/repl/event_log.mli test/test_repl_components.ml; do
  tmp=$(mktemp) && $C ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"; done
$C dune build
git add bin/repl/event_log.ml bin/repl/event_log.mli bin/repl/dune test/test_repl_components.ml
git commit -m "feat(#382): event_log ring buffer + filter/pause state"
```

---

## Task 8: `monitor_view` — nottui rendering + keybindings

**Files:**
- Create: `bin/repl/monitor_view.ml`
- Modify: `bin/repl/dune` (`(modules repl_engine event_log monitor_view)`)
- Modify: `test/test_repl_components.ml` (render-smoke)

API facts (confirmed against the installed libs):
- `Nottui_widgets.string : ?attr:Notty.attr -> string -> Nottui.ui`
- `Nottui_widgets.vbox : Nottui.ui Lwd.t list -> Nottui.ui Lwd.t`
- `Nottui_widgets.scroll_area : ... -> Nottui.ui Lwd.t -> Nottui.ui Lwd.t`
- `Nottui.Ui.keyboard_area : ?focus:Focus.status -> (Ui.key -> Ui.may_handle) -> Ui.t -> Ui.t`
- `Ui.key = [ Unescape.special | `Uchar of Uchar.t | `ASCII of char | semantic_key ] * Unescape.mods`
- `Ui.may_handle = [ `Unhandled | `Handled ]`

- [ ] **Step 1: Write the render-smoke test first (append)**

```ocaml
let test_monitor_renders () =
  let l = Event_log.create ~capacity:10 in
  Event_log.push l (mk_commit 1L);
  let ui_lwd = Monitor_view.render l in
  let root = Lwd.observe ui_lwd in
  let ui = Lwd.quick_sample root in
  (* Renders some non-empty UI without raising. *)
  Alcotest.(check bool) "renders" true (Nottui.Ui.layout_height ui >= 0)
;;
```

Add to the `"event_log"` suite (or a new `"monitor_view"` suite).

- [ ] **Step 2: Run, verify fail**

Run: `$C dune test test/test_repl_components.exe`
Expected: FAIL — `Unbound module Monitor_view`.

- [ ] **Step 3: Implement `monitor_view.ml`**

Create `bin/repl/monitor_view.ml`:

```ocaml
module W = Nottui_widgets
module Ui = Nottui.Ui
module Ev = Sqlocaml.Db.Event

let attr_for ev =
  let open Notty.A in
  match Ev.label ev with
  | "COMMIT" -> fg green
  | "ROLLBACK" -> fg red
  | "CKPT_BEGIN" | "CKPT_END" | "WAL_RESET" -> fg yellow
  | _ -> empty
;;

let header log =
  let filt =
    match Event_log.filter log with
    | None -> "all txns"
    | Some id -> Printf.sprintf "txn=%Ld" id
  in
  let pause = if Event_log.paused log then "PAUSED" else "live" in
  W.string ~attr:Notty.A.(st bold)
    (Printf.sprintf "-- internals monitor [%s] [%s]  (space=pause / =filter c=clear x=clear-log) --"
       pause filt)
;;

let render log =
  Lwd.bind (Lwd.get (Event_log.state_var log)) ~f:(fun () ->
    let evs = Event_log.visible log in
    let rows =
      if evs = [] then
        [ Lwd.return (W.string "  (no events — open a file db; :memory: has no storage seams)") ]
      else
        List.map
          (fun ev -> Lwd.return (W.string ~attr:(attr_for ev) (Format.asprintf "  %a" Ev.pp ev)))
          evs
    in
    W.vbox (Lwd.return (header log) :: rows))
;;

(* Translate a key into a monitor action.  [set_filter_prompt] is supplied by
   the app so '/' can open an input line in the shell pane. *)
let handle_key log ~set_filter_prompt (key : Ui.key) : Ui.may_handle =
  match key with
  | `ASCII ' ', _ -> Event_log.toggle_pause log; `Handled
  | `ASCII 'c', _ -> Event_log.set_filter log None; `Handled
  | `ASCII 'x', _ -> Event_log.clear log; `Handled
  | `ASCII '/', _ -> set_filter_prompt (); `Handled
  | _ -> `Unhandled
;;
```

> If `Lwd.quick_sample`/`Lwd.observe` names differ in the installed `lwd`, grep `lwd.mli` in the container (`$C bash -c 'cat /home/opam/.opam/5.4/lib/lwd/lwd.mli | grep -nE "observe|sample|quick"'`) and adjust the test's sampling calls. The view code itself only uses `Lwd.bind`/`Lwd.get`/`Lwd.return`.

- [ ] **Step 4: Run, verify pass + build**

Run: `$C dune test test/test_repl_components.exe && $C dune build`
Expected: PASS + clean.

- [ ] **Step 5: Format + commit**

```sh
tmp=$(mktemp) && $C ocamlformat bin/repl/monitor_view.ml > "$tmp" && mv "$tmp" bin/repl/monitor_view.ml && chmod 644 bin/repl/monitor_view.ml
tmp=$(mktemp) && $C ocamlformat test/test_repl_components.ml > "$tmp" && mv "$tmp" test/test_repl_components.ml && chmod 644 test/test_repl_components.ml
git add bin/repl/monitor_view.ml bin/repl/dune test/test_repl_components.ml
git commit -m "feat(#382): monitor_view nottui rendering + key actions"
```

---

## Task 9: `shell_view` — input + results grid + status line

**Files:**
- Create: `bin/repl/shell_view.ml`
- Modify: `bin/repl/dune` (`(modules repl_engine event_log monitor_view shell_view)`)
- Modify: `test/test_repl_components.ml` (render-smoke)

`shell_view` holds the query input string, the last result (headers + rows) and an error/status line as `Lwd` vars, and renders them. It does **not** run queries — the app (`sqlocaml_repl.ml`) owns the `Db.t` and updates these vars.

- [ ] **Step 1: Write the render-smoke test (append)**

```ocaml
let test_shell_renders () =
  let v = Shell_view.create () in
  Shell_view.set_status v "Open: :memory:";
  Shell_view.set_result v ~headers:[ "x" ] ~rows:[ [| Sqlocaml.Db.V_int 1L |] ];
  let root = Lwd.observe (Shell_view.render v) in
  let ui = Lwd.quick_sample root in
  Alcotest.(check bool) "renders" true (Nottui.Ui.layout_height ui >= 0)
;;
```

- [ ] **Step 2: Run, verify fail**

Run: `$C dune test test/test_repl_components.exe`
Expected: FAIL — `Unbound module Shell_view`.

- [ ] **Step 3: Implement `shell_view.ml`**

Create `bin/repl/shell_view.ml`:

```ocaml
module W = Nottui_widgets
module Ui = Nottui.Ui

type t =
  { input : string Lwd.var
  ; status : string Lwd.var
  ; headers : string list Lwd.var
  ; rows : Sqlocaml.Db.row list Lwd.var
  }

let create () =
  { input = Lwd.var ""
  ; status = Lwd.var ""
  ; headers = Lwd.var []
  ; rows = Lwd.var []
  }
;;

let input_var t = t.input
let set_status t s = Lwd.set t.status s
let set_result t ~headers ~rows = Lwd.set t.headers headers; Lwd.set t.rows rows

let render_rows headers rows =
  let widths = Repl_engine.column_widths rows in
  let render_row vals =
    let cells =
      Array.to_list
        (Array.mapi
           (fun i v ->
              let s = Repl_engine.value_to_string v in
              let pad = (if i < Array.length widths then widths.(i) else 0) - String.length s in
              s ^ if pad > 0 then String.make pad ' ' else "")
           vals)
    in
    W.string (String.concat " | " cells)
  in
  let header_row =
    if headers = [] then [] else [ Lwd.return (W.string ~attr:Notty.A.(st bold) (String.concat " | " headers)) ]
  in
  header_row @ List.map (fun r -> Lwd.return (render_row r)) rows
;;

let render t =
  Lwd.bind (Lwd.get t.input) ~f:(fun input ->
    Lwd.bind (Lwd.get t.status) ~f:(fun status ->
      Lwd.bind (Lwd.get t.headers) ~f:(fun headers ->
        Lwd.bind (Lwd.get t.rows) ~f:(fun rows ->
          W.vbox
            ([ Lwd.return (W.string ~attr:Notty.A.(fg cyan) ("sqlocaml> " ^ input)) ]
             @ render_rows headers rows
             @ [ Lwd.return (W.string ~attr:Notty.A.(fg lightblack) status) ])))))
;;
```

> The live editable input is driven by `Nottui_widgets.edit_field` in Task 10 where keystrokes are wired; here the input is rendered as plain text so the view is independently testable.

- [ ] **Step 4: Run, verify pass + build**

Run: `$C dune test test/test_repl_components.exe && $C dune build`
Expected: PASS + clean.

- [ ] **Step 5: Format + commit**

```sh
tmp=$(mktemp) && $C ocamlformat bin/repl/shell_view.ml > "$tmp" && mv "$tmp" bin/repl/shell_view.ml && chmod 644 bin/repl/shell_view.ml
tmp=$(mktemp) && $C ocamlformat test/test_repl_components.ml > "$tmp" && mv "$tmp" test/test_repl_components.ml && chmod 644 test/test_repl_components.ml
git add bin/repl/shell_view.ml bin/repl/dune test/test_repl_components.ml
git commit -m "feat(#382): shell_view input + results grid + status line"
```

---

## Task 10: `sqlocaml_repl.ml` — compose panes, root loop, wire callback

**Files:**
- Rewrite: `bin/repl/sqlocaml_repl.ml`
- Modify: `test/test_repl.ml` (keep as smoke; ensure it still builds/passes)

This is the integration task. The executable owns the `Db.t`, an `Event_log.t`, a `Shell_view.t`; composes the two panes with `Nottui_widgets.v_pane`; runs `Nottui_lwt.run`; and registers the event callback on open and after `.open`.

- [ ] **Step 1: Rewrite `sqlocaml_repl.ml`**

Create `bin/repl/sqlocaml_repl.ml`:

```ocaml
open Lwt.Syntax
module Db = Sqlocaml.Db
module W = Nottui_widgets
module Ui = Nottui.Ui

let log = Event_log.create ~capacity:5000
let shell = Shell_view.create ()
let db_ref = ref None
let quit_t, quit_u = Lwt.wait ()
(* When set, the next Enter in the shell treats input as a txn-id filter. *)
let filter_mode = ref false

let wire_callback db =
  Db.set_event_callback db (Some (fun ev -> Event_log.push log ev))
;;

let set_db db =
  db_ref := Some db;
  wire_callback db
;;

let run_sql sql =
  match !db_ref with
  | None -> Lwt.return_unit
  | Some db ->
    if Repl_engine.is_query_stmt sql then (
      let* r = Db.query db sql in
      match r with
      | Error e -> Shell_view.set_status shell (Format.asprintf "Error: %a" Db.pp_error e); Lwt.return_unit
      | Ok stream ->
        let* rows = Lwt_stream.to_list stream in
        Shell_view.set_result shell ~headers:[] ~rows;
        Shell_view.set_status shell (Printf.sprintf "%d row(s)" (List.length rows));
        Lwt.return_unit)
    else (
      let* r = Db.execute_change_count db sql in
      match r with
      | Error e -> Shell_view.set_status shell (Format.asprintf "Error: %a" Db.pp_error e); Lwt.return_unit
      | Ok n -> Shell_view.set_status shell (Printf.sprintf "%d row(s) affected" n); Lwt.return_unit)
;;

let submit input =
  if !filter_mode then (
    filter_mode := false;
    (match Int64.of_string_opt (String.trim input) with
     | Some id -> Event_log.set_filter log (Some id)
     | None -> Shell_view.set_status shell "filter: not a txn id");
    Lwt.return_unit)
  else (
    let stmts = Repl_engine.split_stmts input in
    Lwt_list.iter_s run_sql stmts)
;;

(* Editable input line wired to keystrokes; Enter submits. *)
let input_ui =
  Lwd.bind (Lwd.get (Shell_view.input_var shell)) ~f:(fun cur ->
    let edit = W.string ~attr:Notty.A.(fg cyan) ("sqlocaml> " ^ cur) in
    Lwd.return
      (Ui.keyboard_area
         (fun key ->
            match key with
            | `Enter, _ ->
              let input = Lwd.peek (Shell_view.input_var shell) in
              Lwd.set (Shell_view.input_var shell) "";
              Lwt.async (fun () -> submit input);
              `Handled
            | `Backspace, _ ->
              let s = Lwd.peek (Shell_view.input_var shell) in
              if String.length s > 0 then Lwd.set (Shell_view.input_var shell) (String.sub s 0 (String.length s - 1));
              `Handled
            | `ASCII c, _ ->
              Lwd.set (Shell_view.input_var shell) (cur ^ String.make 1 c);
              `Handled
            | `Escape, _ -> Lwt.wakeup_later quit_u (); `Handled
            | _ -> `Unhandled)
         edit))
;;

let monitor_ui =
  let set_filter_prompt () =
    filter_mode := true;
    Shell_view.set_status shell "enter txn id, Enter to filter"
  in
  Lwd.map (Monitor_view.render log) ~f:(fun ui ->
    Ui.keyboard_area (Monitor_view.handle_key log ~set_filter_prompt) ui)
;;

let root =
  W.v_pane
    (W.vbox [ input_ui; Shell_view.render shell ])
    monitor_ui
;;

let main () =
  Sqlocaml_unix.install ();
  let path = match Array.to_list Sys.argv |> List.tl with [] -> ":memory:" | p :: _ -> p in
  Lwt_main.run
    (let* db = Repl_engine.open_db ~path in
     match db with
     | Error e -> Format.eprintf "Cannot open '%s': %a\n%!" path Db.pp_error e; exit 1
     | Ok db ->
       set_db db;
       Shell_view.set_status shell (Printf.sprintf "Open: %s" path);
       Nottui_lwt.run ~quit:quit_t root)
;;

let () = main ()
```

> **Implementer notes (adapt to the installed API, do not invent):**
> - Confirm `Lwd.peek` exists (grep `lwd.mli`); if not, read via `Lwd.observe`/`Lwd.quick_sample` of the var, or keep a plain `ref` for the input string alongside the `Lwd.var`.
> - Confirm the special-key constructor names (`` `Enter ``, `` `Backspace ``, `` `Escape ``) against `Notty.Unescape.special` (grep `notty.mli` in the container). Substitute the real polymorphic-variant tags.
> - Preserve dot-commands: before `split_stmts`, if `String.trim input` starts with `.`, dispatch `.tables/.schema/.open/.quit/.help/.databases` using `Repl_engine` queries and `Shell_view.set_result`/`set_status`. `.open` must call `set_db` (re-wires the callback) and close the previous db. `.quit` calls `Lwt.wakeup_later quit_u ()`. Add these in a `dispatch_dot` function mirroring the original file's `dispatch_dot` (lines 191–223) but writing to the views instead of `print_*`.

- [ ] **Step 2: Update `test/test_repl.ml` to a build/smoke test**

The original test was already reduced to smoke in #310. Ensure it does not reference removed printing internals. If it calls into the old `sqlocaml_repl` internals, replace those calls with `Repl_engine` equivalents (e.g. test `Repl_engine.split_stmts`). Keep at least one trivial `Alcotest` case so the executable builds and runs.

- [ ] **Step 3: Build the executable**

Run: `$C dune build bin/repl/sqlocaml_repl.exe`
Expected: clean build. Fix any API-name mismatches per the implementer notes (this is the task most likely to need 1–2 iteration cycles on nottui/lwd names).

- [ ] **Step 4: Run the full test + repl smoke**

Run: `$C dune test`
Expected: all suites PASS.

- [ ] **Step 5: Manual smoke (non-blocking sanity)**

Run a scripted check that the binary starts and exits cleanly against a file db (it needs a TTY for full interaction, so just verify it builds & the `--help`/immediate-EOF path doesn't crash):

```sh
$C bash -c 'echo "" | ./_build/default/bin/repl/sqlocaml_repl.exe /tmp/smoke.db || true'
```

Expected: no OCaml exception/backtrace on startup. (Full interactive verification is done by the user in a terminal.)

- [ ] **Step 6: Format + commit**

```sh
tmp=$(mktemp) && $C ocamlformat bin/repl/sqlocaml_repl.ml > "$tmp" && mv "$tmp" bin/repl/sqlocaml_repl.ml && chmod 644 bin/repl/sqlocaml_repl.ml
git add bin/repl/sqlocaml_repl.ml test/test_repl.ml
git commit -m "feat(#382): nottui REPL shell with internals monitor pane (single root loop)"
```

---

## Task 11: Coverage top-up + final verification

**Files:** as needed (`test/test_store_event.ml`, `test/test_repl_components.ml`)

- [ ] **Step 1: Generate coverage for the new lib module**

```sh
$C dune test
$C bisect-ppx-report html --output _coverage_report/ _build/default/test/*.coverage || true
$C bisect-ppx-report summary _build/default/test/*.coverage | grep -i store_event
```

Expected: `store_event.ml` at/near 100%. Add cases for any uncovered `pp`/`label` arms (e.g. a `Wal_append`/`Checkpoint_*` `pp` assertion) until covered.

- [ ] **Step 2: Full build + full test (final gate)**

Run: `$C dune build && $C dune test`
Expected: clean build, all suites PASS.

- [ ] **Step 3: Confirm no direct-to-main + open PR**

```sh
git push origin feat/382-internals-monitor
~/.local/bin/forgejo pr create tej/sqlite_ocaml_port \
  --title="feat(#382): internals monitor — on_event seam + nottui REPL + monitor pane" \
  --head=feat/382-internals-monitor --base=main \
  --body="$(cat <<'EOF'
## Summary
- Pure `Store_event` type + zero-overhead `on_event` seam on the store (commit/rollback/savepoint/WAL append/reset/checkpoint).
- `Db.set_event_callback` passthrough.
- Rewrote `bin/repl` as a single-root-loop nottui app: query shell + toggleable internals-monitor pane with txn-id filtering, pause/resume, bounded ring buffer.

Note: #310's PR shipped only the nottui deps/scaffold, not the REPL shell it described — this PR builds that shell as well.

Deferred (filed): #384 (page/COW/freelist events), #385 (table-name filtering + log export).

## Test plan
- [ ] dune test passes
- [ ] test_store_event (seam sequence + QCheck + defensive raising-callback)
- [ ] test_db_event_385 (passthrough)
- [ ] test_repl_components (repl_engine + event_log + render-smoke)
- [ ] store_event.ml at 100% coverage

Closes #382
EOF
)"
```

---

## Self-review notes (author)

- **Spec coverage:** event type (T1), seam+lifecycle (T2), savepoints (T3), WAL/checkpoint+defensive+QCheck (T4), Db passthrough (T5), repl_engine (T6), event_log ring/filter/pause (T7), monitor_view+keys (T8), shell_view grid/status (T9), root loop+wiring+dot-commands (T10), Mem-empty-state hint (T8 render), coverage (T11). All spec sections map to a task.
- **Type consistency:** `Store_event.t` constructors and `label` strings (`"COMMIT"`, `"WAL_APPEND"`, …) are used identically in tests and `monitor_view`. `set_event_callback : t -> (Event.t -> unit) option -> unit` is consistent across store/db. `Event_log` API (`push`/`visible`/`set_filter`/`toggle_pause`/`paused`/`clear`/`state_var`) matches between `.mli`, tests, and `monitor_view`/`sqlocaml_repl`.
- **Known adaptation points (flagged inline, not placeholders):** exact `Lwd` sampling/`peek` names and Notty special-key tags are confirmed at execution time against the container's `.mli` files — the plan says exactly how to confirm and what to substitute. These are integration realities of an unfamiliar TUI lib, not unspecified design.
