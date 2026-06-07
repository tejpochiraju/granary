# Per-deployment durability knob (#298) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a database-wide durability setting (`full` / `batched` / `off`, analogous to SQLite `synchronous`) that gates only the WAL group-commit fsync, surfaced via a `Db` open-option and `PRAGMA`, leaving CoW / snapshot-isolation / rollback / recovery untouched.

**Architecture:** A mode tag + batched params (`commits`, `interval_ms`) + an injected `unit -> float` clock live on `bt_state` in `lib/store/store.ml`. `commit_wal` reads them to decide whether to fsync now (`full`), defer until N commits or T ms elapse (`batched`), or never on commit (`off`). Checkpoint and `close` remain full-sync anchors. The setting is plumbed parser→planner→exec exactly like the existing `wal_autocheckpoint` PRAGMA, and threaded as a `Db.open_block` option via `Db.of_store`.

**Tech Stack:** OCaml 5.1, Lwt, Alcotest, dune (run **inside podman**, never directly), menhir parser (`parser.mly`).

**Spec:** `docs/superpowers/specs/2026-06-07-durability-knob-298-design.md`

---

## Conventions for every task

- **Build/test command (NEVER call `dune` on the host):**
  ```bash
  PODMAN="podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev"
  $PODMAN dune build 2>&1 | tail -30
  $PODMAN dune exec test/test_durability_298.exe 2>&1 | tail -40
  ```
- **Formatting:** before committing, format changed files with ocamlformat via the container, writing back to the host (per project convention), e.g.:
  ```bash
  $PODMAN ocamlformat --enable-outside-detected-project lib/store/store.ml > /tmp/fmt.ml && mv /tmp/fmt.ml lib/store/store.ml
  ```
  Then re-run `dune build` to confirm.
- **TDD:** write the failing test, run it to see it fail, implement minimally, run to green, commit.
- All Plan-variant `match` expressions in `exec.ml` / `planner.ml` are **exhaustive (no wildcard)**. After adding new `Plan.op` constructors the compiler WILL flag every match needing a new arm — follow the compiler errors; the arms you must add are all given below.

---

## File Structure

| File | Responsibility | Change |
|------|----------------|--------|
| `lib/store/store.mli` | public `durability` type + accessors | Modify |
| `lib/store/store.ml` | `bt_state` fields, accessors, `commit_wal` gating, `close` final-sync | Modify |
| `lib/sql/ast.mli` | 6 new `Pragma_*` AST variants | Modify |
| `lib/sql/parser.mly` | parse the 3 setters + 3 getters | Modify |
| `lib/sql/plan.mli` | 6 new `Op_pragma_*` ops | Modify |
| `lib/sql/planner.ml` | AST→Plan mapping + `plan_pragma_rows` assert list | Modify |
| `lib/sql/exec.ml` | op-label, execute (setters), is_write list, to_stream (getters) | Modify |
| `lib/db/db.ml` | `?durability` / `?clock` threading through `of_store` / `open_block` / `open_in_memory` | Modify |
| `lib/db/db.mli` | signatures for the above | Modify |
| `test/test_durability_298.ml` | all unit + property tests | Create |
| `test/dune` | register the new test exe | Modify |
| `README.md` (durability section) + `lib/store/store.mli` doc-comment | caveat docs | Modify |

---

## Task 1: Store-level durability state + accessors

**Files:**
- Modify: `lib/store/store.mli` (add type + accessors near the `wal_autocheckpoint` block, ~line 256)
- Modify: `lib/store/store.ml` (`bt_state` fields ~line 121; `make_btree_store` ~line 549; new accessors near `set_wal_autocheckpoint` ~line 1560)
- Test: `test/test_durability_298.ml`

- [ ] **Step 1: Register the new test executable**

In `test/dune`, after the `test_group_commit` stanza (~line 296), add:
```
(test
 (name test_durability_298)
 (libraries sqlocaml sqlocaml.store sqlocaml.storage sqlocaml.unix alcotest lwt.unix unix))
```

- [ ] **Step 2: Write the failing test (accessor round-trip)**

Create `test/test_durability_298.ml`:
```ocaml
(** Tests for #298 — per-deployment durability knob (full/batched/off). *)

open Lwt.Syntax

module S = struct
  include Sqlocaml_store.Store

  let open_file_wal = Sqlocaml_unix.Store.open_file_wal
end

module D = struct
  include Sqlocaml.Db

  let open_file_wal = Sqlocaml_unix.open_file_wal
end

let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_dura_298_%04d.db" n
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

let with_fresh ~f =
  let path = fresh_path () in
  cleanup path;
  Lwt.finalize (fun () -> f path) (fun () -> cleanup path; Lwt.return_unit)
;;

let bs = Bytes.of_string

let open_st path =
  let* sr = S.open_file_wal ~path () in
  match sr with
  | Ok t -> Lwt.return t
  | Error e -> Alcotest.failf "open_file_wal: %a" S.pp_error e
;;

(* --- accessors --- *)

let test_default_is_full () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    Alcotest.(check bool) "default Full"
      true (match S.durability st with S.Full -> true | _ -> false);
    Alcotest.(check int) "default batch commits" 256 (S.sync_batch_commits st);
    Alcotest.(check int) "default batch interval" 100 (S.sync_batch_interval_ms st);
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_set_get_round_trip () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    S.set_durability st (S.Batched { commits = 7; interval_ms = 33 });
    Alcotest.(check bool) "now Batched"
      true (match S.durability st with S.Batched _ -> true | _ -> false);
    Alcotest.(check int) "commits stored" 7 (S.sync_batch_commits st);
    Alcotest.(check int) "interval stored" 33 (S.sync_batch_interval_ms st);
    (* switching to Full keeps the batched params *)
    S.set_durability st S.Full;
    Alcotest.(check int) "commits survive mode switch" 7 (S.sync_batch_commits st);
    (* granular setter changes N without touching mode *)
    S.set_durability st (S.Batched { commits = 7; interval_ms = 33 });
    S.set_sync_batch_commits st 99;
    Alcotest.(check int) "granular N" 99 (S.sync_batch_commits st);
    Alcotest.(check bool) "still Batched"
      true (match S.durability st with S.Batched _ -> true | _ -> false);
    let* () = S.close st in
    Lwt.return_unit)
;;

let () =
  Alcotest.run "durability_298"
    [ "accessors",
      [ Alcotest.test_case "default is full" `Quick test_default_is_full
      ; Alcotest.test_case "set/get round-trip" `Quick test_set_get_round_trip
      ]
    ]
;;
```

- [ ] **Step 3: Run the test, expect a COMPILE failure**

Run: `$PODMAN dune exec test/test_durability_298.exe 2>&1 | tail -30`
Expected: build error — `Unbound constructor S.Full` / `Unbound value S.durability`.

- [ ] **Step 4: Add the public type + signatures to `store.mli`**

In `lib/store/store.mli`, immediately before the `wal_autocheckpoint` doc-comment (~line 253), add:
```ocaml
(** #298: per-deployment durability mode (analogue of SQLite [synchronous]).
    [Full] fsyncs the WAL on every group-commit before acking (the default,
    unchanged behaviour).  [Batched] acks immediately and defers the fsync
    until [commits] un-synced commits accumulate OR [interval_ms] have elapsed
    since the last sync (whichever first; the time bound needs a clock — see
    {!set_clock} — otherwise only the commit count triggers).  [Off] never
    fsyncs on commit.  Checkpoint and {!close} are always full-sync anchors,
    so [Batched]/[Off] data is made durable there.  The setting is
    DATABASE-WIDE (the commit queue is shared across connections), not
    per-connection.  No-op on the in-memory backend. *)
type durability =
  | Full
  | Batched of
      { commits : int
      ; interval_ms : int
      }
  | Off

(** Current durability mode. Returns [Full] on the in-memory backend. *)
val durability : t -> durability

(** Set the durability mode. [Batched] params are remembered across switches
    to [Full]/[Off] (so a later [PRAGMA synchronous=batched] restores them).
    No-op on the in-memory backend. *)
val set_durability : t -> durability -> unit

(** Batched commit-count threshold N (default 256). Independent of the active
    mode; only takes effect while the mode is [Batched]. *)
val sync_batch_commits : t -> int

(** Set the batched commit-count threshold N (clamped to >= 0). No-op on the
    in-memory backend. *)
val set_sync_batch_commits : t -> int -> unit

(** Batched time threshold T in milliseconds (default 100). *)
val sync_batch_interval_ms : t -> int

(** Set the batched time threshold T in milliseconds (clamped to >= 0). No-op
    on the in-memory backend. *)
val set_sync_batch_interval_ms : t -> int -> unit

(** Install the wall-clock source ([unit -> float], Unix-epoch seconds) used by
    [Batched] mode's time threshold. Without one, the default [fun () -> 0.]
    disables the time trigger (only the commit count fires). No-op on the
    in-memory backend. *)
val set_clock : t -> (unit -> float) -> unit
```

- [ ] **Step 5: Add the `bt_state` fields**

In `lib/store/store.ml`, in the `type bt_state = { ... }` record, immediately after the `mutable follower : bool` field (~line 170, before the closing `}` at ~line 175), add:
```ocaml
  ; mutable sync_mode : [ `Full | `Batched | `Off ]
    (* #298: durability mode. [`Full] = fsync every group-commit (default).
       [`Batched] = defer fsync until [batch_commits] or [batch_interval_ms].
       [`Off] = never fsync on commit. Only consulted in WAL mode. *)
  ; mutable batch_commits : int (* #298: batched N threshold (default 256) *)
  ; mutable batch_interval_ms : int (* #298: batched T threshold ms (default 100) *)
  ; mutable unsynced_commits : int (* #298: committed-but-unsynced batches since last fsync *)
  ; mutable last_sync_time : float (* #298: clock () at last commit fsync; for the T trigger *)
  ; mutable clock : unit -> float (* #298: wall-clock source; default returns 0. *)
```

- [ ] **Step 6: Initialise the fields in `make_btree_store`**

In `lib/store/store.ml`, in the `make_btree_store` record literal (~line 557, after `; follower = false`), add:
```ocaml
    ; sync_mode = `Full
    ; batch_commits = 256
    ; batch_interval_ms = 100
    ; unsynced_commits = 0
    ; last_sync_time = 0.
    ; clock = (fun () -> 0.)
```

- [ ] **Step 7: Add the accessors**

In `lib/store/store.ml`, immediately after `set_wal_autocheckpoint` (~line 1570), add:
```ocaml
let durability (t : t) : durability =
  match t.backend with
  | Mem _ -> Full
  | Btree st ->
    (match st.sync_mode with
     | `Full -> Full
     | `Off -> Off
     | `Batched ->
       Batched { commits = st.batch_commits; interval_ms = st.batch_interval_ms })
;;

let set_durability (t : t) (d : durability) : unit =
  match t.backend with
  | Mem _ -> ()
  | Btree st ->
    (match d with
     | Full -> st.sync_mode <- `Full
     | Off -> st.sync_mode <- `Off
     | Batched { commits; interval_ms } ->
       st.sync_mode <- `Batched;
       st.batch_commits <- max 0 commits;
       st.batch_interval_ms <- max 0 interval_ms)
;;

let sync_batch_commits (t : t) : int =
  match t.backend with
  | Mem _ -> 256
  | Btree st -> st.batch_commits
;;

let set_sync_batch_commits (t : t) (n : int) : unit =
  match t.backend with
  | Mem _ -> ()
  | Btree st -> st.batch_commits <- max 0 n
;;

let sync_batch_interval_ms (t : t) : int =
  match t.backend with
  | Mem _ -> 100
  | Btree st -> st.batch_interval_ms <- st.batch_interval_ms; st.batch_interval_ms
;;

let set_sync_batch_interval_ms (t : t) (n : int) : unit =
  match t.backend with
  | Mem _ -> ()
  | Btree st -> st.batch_interval_ms <- max 0 n
;;

let set_clock (t : t) (c : unit -> float) : unit =
  match t.backend with
  | Mem _ -> ()
  | Btree st -> st.clock <- c
;;
```
> Note: simplify `sync_batch_interval_ms` to just `| Btree st -> st.batch_interval_ms` (the self-assign above is a copy-paste guard; remove it).

- [ ] **Step 8: Build and run the test, expect PASS**

Run: `$PODMAN dune build 2>&1 | tail -30 && $PODMAN dune exec test/test_durability_298.exe 2>&1 | tail -20`
Expected: `accessors` suite passes (2 cases).

- [ ] **Step 9: Format changed files and commit**

```bash
git add lib/store/store.ml lib/store/store.mli test/test_durability_298.ml test/dune
git commit -m "feat(#298): store-level durability mode state + accessors

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: Gate the commit fsync in `commit_wal`

**Files:**
- Modify: `lib/store/store.ml` (`commit_wal`, ~line 1423-1467)
- Test: `test/test_durability_298.ml`

- [ ] **Step 1: Write the failing test (fsync accounting per mode)**

Add to `test/test_durability_298.ml` (before the `let ()` runner), then add the cases + a new group to the runner:
```ocaml
(* --- fsync accounting --- *)

let commit_kv st i =
  let* tx = S.rw_begin st in
  let* () =
    S.put tx 16 (bs (Printf.sprintf "k%04d" i)) (bs (Printf.sprintf "v%04d" i))
  in
  S.commit tx
;;

let do_commits st n =
  let rec loop i = if i = n then Lwt.return_unit else
    let* () = commit_kv st i in loop (i + 1)
  in loop 0
;;

let test_full_syncs_each_commit () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    S.set_durability st S.Full;
    S.set_wal_autocheckpoint st 0;  (* isolate commit fsyncs from checkpoint *)
    let s0 = S.wal_sync_count st in
    let* () = do_commits st 20 in
    let delta = S.wal_sync_count st - s0 in
    (* Solo-writer full mode: ~1 fsync per commit (group-commit may coalesce
       under concurrency, but these are sequential). *)
    Alcotest.(check bool)
      (Printf.sprintf "full: ~1 fsync/commit (got %d for 20)" delta)
      true (delta >= 20);
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_off_never_syncs_on_commit () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    S.set_durability st S.Off;
    S.set_wal_autocheckpoint st 0;  (* no checkpoint either *)
    let s0 = S.wal_sync_count st in
    let* () = do_commits st 50 in
    let delta = S.wal_sync_count st - s0 in
    Alcotest.(check int) "off: zero commit fsyncs" 0 delta;
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_batched_syncs_every_n () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    (* No clock => time trigger disabled; only N fires. *)
    S.set_durability st (S.Batched { commits = 10; interval_ms = 1_000_000 });
    S.set_wal_autocheckpoint st 0;
    let s0 = S.wal_sync_count st in
    let* () = do_commits st 30 in
    let delta = S.wal_sync_count st - s0 in
    (* 30 commits, N=10 => ~3 fsyncs (boundary commits 10,20,30). *)
    Alcotest.(check bool)
      (Printf.sprintf "batched N=10: ~3 fsyncs (got %d), far below 30" delta)
      true (delta >= 1 && delta <= 5);
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_batched_syncs_on_time () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    let now = ref 0. in
    S.set_clock st (fun () -> !now);
    (* High N so the count trigger never fires; T=100ms. *)
    S.set_durability st (S.Batched { commits = 1_000_000; interval_ms = 100 });
    S.set_wal_autocheckpoint st 0;
    let s0 = S.wal_sync_count st in
    let* () = do_commits st 5 in
    Alcotest.(check int) "no sync before T elapses" 0 (S.wal_sync_count st - s0);
    now := 0.5;  (* advance 500ms > T *)
    let* () = commit_kv st 999 in
    Alcotest.(check bool)
      "sync after T elapses" true (S.wal_sync_count st - s0 >= 1);
    let* () = S.close st in
    Lwt.return_unit)
;;
```
Add to the `Alcotest.run` list a new group:
```ocaml
    ; "fsync-accounting",
      [ Alcotest.test_case "full syncs each commit" `Quick test_full_syncs_each_commit
      ; Alcotest.test_case "off never syncs on commit" `Quick test_off_never_syncs_on_commit
      ; Alcotest.test_case "batched syncs every N" `Quick test_batched_syncs_every_n
      ; Alcotest.test_case "batched syncs on time" `Quick test_batched_syncs_on_time
      ]
```

- [ ] **Step 2: Run the test, expect FAIL**

Run: `$PODMAN dune exec test/test_durability_298.exe 2>&1 | tail -40`
Expected: `off never syncs on commit` and `batched ...` FAIL (current code always syncs).

- [ ] **Step 3: Implement the gating in `commit_wal`**

In `lib/store/store.ml`, replace the body of `commit_wal` from the `let* () = commit_prepare_btree ...` line through the final `match role with ...` (lines ~1440-1463) with:
```ocaml
       let* () = commit_prepare_btree ~header_commit:Header.commit_no_sync st in
       (* #298: decide the sync policy under the write lock so the counter is
          race-free across concurrent writers, then release the lock. *)
       let do_sync =
         match st.sync_mode with
         | `Full -> true
         | `Off ->
           st.unsynced_commits <- st.unsynced_commits + 1;
           false
         | `Batched ->
           st.unsynced_commits <- st.unsynced_commits + 1;
           let elapsed_ms = (st.clock () -. st.last_sync_time) *. 1000. in
           st.unsynced_commits >= st.batch_commits
           || elapsed_ms >= float_of_int st.batch_interval_ms
       in
       unlock_once ();
       let* () =
         if do_sync
         then (
           let* role =
             group_commit_sync st.commit_queue (fun () ->
               let* r = Pager.wal_sync st.pager in
               match r with
               | Ok () -> Lwt.return_unit
               | Error e ->
                 Lwt.fail_with
                   (Format.asprintf "Store.commit: wal_sync: %a" Pager.pp_error e))
           in
           (* fsync succeeded (failure raises above): reset the loss window. *)
           st.unsynced_commits <- 0;
           st.last_sync_time <- st.clock ();
           match role with
           | `Joiner -> Lwt.return_unit
           | `Drainer -> maybe_autockpt_after_commit t st)
         else
           (* No fsync this commit: still bound the WAL via autocheckpoint
              (checkpoint is a full-sync durability anchor). *)
           maybe_autockpt_after_commit t st
       in
       (* Fire the frame-sink callback asynchronously so the commit path
          is never blocked by replication I/O. *)
       (match st.on_committed_frames with
        | None -> ()
        | Some cb ->
          let new_frames = Wal.committed_frames wal in
          if new_frames > prev_frames
          then (
            let epoch = Wal.epoch wal in
            let count = new_frames - prev_frames in
            Lwt.async (fun () -> cb ~epoch ~base_idx:prev_frames ~count)));
       Lwt.return_unit)
```
> The surrounding `Lwt.catch (fun () -> ... ) (fun exn -> unlock_once (); Lwt.fail exn)` wrapper and the `let wal = ...` / `prev_frames` bindings above it stay exactly as-is. Only the inner body changes. `unlock_once` is now called after the policy decision in all paths; the `catch` handler's `unlock_once ()` remains the safety net for the prepare-phase failure.

- [ ] **Step 4: Build and run, expect PASS**

Run: `$PODMAN dune build 2>&1 | tail -30 && $PODMAN dune exec test/test_durability_298.exe 2>&1 | tail -30`
Expected: all `fsync-accounting` cases pass.

- [ ] **Step 5: Regression — run the existing group-commit + autockpt suites**

Run:
```bash
$PODMAN dune exec test/test_group_commit.exe 2>&1 | tail -15
$PODMAN dune exec test/test_wal_autocheckpoint.exe 2>&1 | tail -15
```
Expected: both green (full mode unchanged).

- [ ] **Step 6: Format and commit**

```bash
git add lib/store/store.ml test/test_durability_298.ml
git commit -m "feat(#298): gate WAL commit fsync by durability mode

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: `close` final-sync anchor for batched/off

**Files:**
- Modify: `lib/store/store.ml` (`close`, ~line 567)
- Test: `test/test_durability_298.ml`

- [ ] **Step 1: Write the failing test (data survives clean close in off mode)**

Add to `test/test_durability_298.ml` and register in a new `"durability-anchors"` group:
```ocaml
let test_off_durable_after_close () =
  let path = fresh_path () in
  cleanup path;
  (* Write in off mode, then close (which must flush), then REOPEN and read. *)
  run
    (let* st = open_st path in
     S.set_durability st S.Off;
     S.set_wal_autocheckpoint st 0;
     let* () = do_commits st 25 in
     S.close st);
  run
    (let* st = open_st path in
     let* tx = S.ro_begin st in
     let* v = S.get tx 16 (bs "k0010") in
     let* () = S.ro_end tx in
     Alcotest.(check (option string))
       "off-mode data durable after clean close"
       (Some "v0010") (Option.map Bytes.to_string v);
     S.close st);
  cleanup path
;;
```
Runner group:
```ocaml
    ; "durability-anchors",
      [ Alcotest.test_case "off durable after close" `Quick test_off_durable_after_close ]
```

- [ ] **Step 2: Run, expect PASS or FAIL**

Run: `$PODMAN dune exec test/test_durability_298.exe 2>&1 | tail -20`
Expected: This MAY already pass because the page cache survives within one process and `wal_close`/recovery replays unsynced-but-cached frames. It establishes the contract. If it passes, the explicit final-sync in Step 3 is still required for the **cross-process / crash** guarantee — proceed to add it (it makes the durability real, not cache-dependent).

- [ ] **Step 3: Add the final sync to `close`**

In `lib/store/store.ml`, replace `close` (~lines 567-577) with:
```ocaml
let close (t : t) : unit Lwt.t =
  match t.backend with
  | Mem _ -> Lwt.return_unit
  | Btree st ->
    (* #298: in batched/off mode a final fsync makes the last acked commits
       durable across process exit, since they may never have been synced. *)
    let* () =
      if st.unsynced_commits > 0
      then (
        match Pager.wal_sync st.pager with
        | exception _ -> Lwt.return_unit
        | p ->
          let* r = p in
          (match r with
           | Ok () -> st.unsynced_commits <- 0
           | Error _ -> ());
          Lwt.return_unit)
      else Lwt.return_unit
    in
    let* () =
      match st.wal_close with
      | None -> Lwt.return_unit
      | Some f -> f ()
    in
    st.close_fn ()
;;
```
> `Pager.wal_sync` returns a `(unit, error) result Lwt.t`; the `exception` guard is defensive only. If `Pager.wal_sync` is not in scope at this point in the file (it is used later in `commit_wal`), no forward-reference problem exists because both are `let`-bound at module top level and `wal_sync` lives in `Pager`. Build will confirm.

- [ ] **Step 4: Build and run, expect PASS**

Run: `$PODMAN dune build 2>&1 | tail -20 && $PODMAN dune exec test/test_durability_298.exe 2>&1 | tail -20`
Expected: `off durable after close` passes.

- [ ] **Step 5: Format and commit**

```bash
git add lib/store/store.ml test/test_durability_298.ml
git commit -m "feat(#298): final fsync on close for batched/off durability

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 4: PRAGMA plumbing (parser → AST → plan → planner → exec)

**Files:**
- Modify: `lib/sql/ast.mli` (~line 452, after `Pragma_wal_autocheckpoint_set`)
- Modify: `lib/sql/parser.mly` (setter block ~line 341; getter block ~line 381)
- Modify: `lib/sql/plan.mli` (~line 273, after `Op_pragma_set_wal_autocheckpoint`)
- Modify: `lib/sql/planner.ml` (`plan_pragma` ~line 833; `plan_pragma_rows` assert list ~line 813)
- Modify: `lib/sql/exec.ml` (op-label ~5416; execute setters ~6466; is_write list ~9337; to_stream getters ~9500)
- Test: `test/test_durability_298.ml`

- [ ] **Step 1: Write the failing test (PRAGMA round-trip via SQL)**

Add to `test/test_durability_298.ml`:
```ocaml
let open_db path =
  let* db = D.open_file_wal ~path () in
  match db with
  | Ok d -> Lwt.return d
  | Error e -> Alcotest.failf "Db.open_file_wal: %a" D.pp_error e
;;

let query1_text db sql =
  let* s = D.query db sql in
  let* s = match s with Ok s -> Lwt.return s | Error e -> Alcotest.failf "query: %a" D.pp_error e in
  let* rows = Lwt_stream.to_list s in
  match rows with
  | [ [| D.V_text t |] ] -> Lwt.return t
  | _ -> Alcotest.failf "expected one text row for %s" sql
;;

let query1_int db sql =
  let* s = D.query db sql in
  let* s = match s with Ok s -> Lwt.return s | Error e -> Alcotest.failf "query: %a" D.pp_error e in
  let* rows = Lwt_stream.to_list s in
  match rows with
  | [ [| D.V_int n |] ] -> Lwt.return (Int64.to_int n)
  | _ -> Alcotest.failf "expected one int row for %s" sql
;;

let exec_ok db sql =
  let* r = D.execute db sql in
  match r with Ok () -> Lwt.return_unit | Error e -> Alcotest.failf "execute %s: %a" sql D.pp_error e
;;

let test_pragma_round_trip () =
  run
  @@ with_fresh ~f:(fun path ->
    let* db = open_db path in
    let* v = query1_text db "PRAGMA synchronous" in
    Alcotest.(check string) "default full" "full" v;
    let* () = exec_ok db "PRAGMA synchronous = batched" in
    let* v = query1_text db "PRAGMA synchronous" in
    Alcotest.(check string) "set batched" "batched" v;
    let* () = exec_ok db "PRAGMA wal_batch_commits = 42" in
    let* n = query1_int db "PRAGMA wal_batch_commits" in
    Alcotest.(check int) "N round-trip" 42 n;
    let* () = exec_ok db "PRAGMA wal_batch_interval_ms = 250" in
    let* n = query1_int db "PRAGMA wal_batch_interval_ms" in
    Alcotest.(check int) "T round-trip" 250 n;
    let* () = exec_ok db "PRAGMA synchronous = off" in
    let* v = query1_text db "PRAGMA synchronous" in
    Alcotest.(check string) "set off" "off" v;
    let* () = exec_ok db "PRAGMA synchronous = full" in
    let* v = query1_text db "PRAGMA synchronous" in
    Alcotest.(check string) "back to full" "full" v;
    let* () = D.close db in
    Lwt.return_unit)
;;

let test_pragma_invalid_value () =
  run
  @@ with_fresh ~f:(fun path ->
    let* db = open_db path in
    let* r = D.execute db "PRAGMA synchronous = wat" in
    Alcotest.(check bool) "invalid mode rejected"
      true (match r with Error _ -> true | Ok () -> false);
    let* () = D.close db in
    Lwt.return_unit)
;;
```
Runner group:
```ocaml
    ; "pragma",
      [ Alcotest.test_case "synchronous/N/T round-trip" `Quick test_pragma_round_trip
      ; Alcotest.test_case "invalid mode rejected" `Quick test_pragma_invalid_value
      ]
```

- [ ] **Step 2: Run, expect FAIL (parser rejects unknown pragma → no-op, query returns no rows)**

Run: `$PODMAN dune exec test/test_durability_298.exe 2>&1 | tail -30`
Expected: `pragma` cases FAIL.

- [ ] **Step 3: Add AST variants**

In `lib/sql/ast.mli`, after line 453 (`Pragma_wal_autocheckpoint_set of int64`), add:
```ocaml
  | Pragma_synchronous (* PRAGMA synchronous — read mode (#298) *)
  | Pragma_synchronous_set of string (* PRAGMA synchronous = full|batched|off *)
  | Pragma_wal_batch_commits (* PRAGMA wal_batch_commits — read N (#298) *)
  | Pragma_wal_batch_commits_set of int64 (* PRAGMA wal_batch_commits = N *)
  | Pragma_wal_batch_interval_ms (* PRAGMA wal_batch_interval_ms — read T (#298) *)
  | Pragma_wal_batch_interval_ms_set of int64 (* PRAGMA wal_batch_interval_ms = T *)
```

- [ ] **Step 4: Parse the setters**

In `lib/sql/parser.mly`, in the setter `match` (after the `"wal_autocheckpoint"` arm, ~line 344), add:
```
      | "synchronous" ->
        (match String.lowercase_ascii value with
         | "full" | "batched" | "off" ->
           Ast.S_pragma (Ast.Pragma_synchronous_set (String.lowercase_ascii value))
         | _ ->
           failwith (Printf.sprintf
             "PRAGMA synchronous = %s: expected full|batched|off" value))
      | "wal_batch_commits" ->
        (try Ast.S_pragma (Ast.Pragma_wal_batch_commits_set (Int64.of_string value))
         with Failure _ ->
           failwith (Printf.sprintf "PRAGMA wal_batch_commits: expected integer, got %s" value))
      | "wal_batch_interval_ms" ->
        (try Ast.S_pragma (Ast.Pragma_wal_batch_interval_ms_set (Int64.of_string value))
         with Failure _ ->
           failwith (Printf.sprintf "PRAGMA wal_batch_interval_ms: expected integer, got %s" value))
```

- [ ] **Step 5: Parse the getters**

In `lib/sql/parser.mly`, in the bare-getter `match` (after the `"wal_autocheckpoint"` arm, ~line 381), add:
```
      | "synchronous"           -> Ast.S_pragma Ast.Pragma_synchronous
      | "wal_batch_commits"     -> Ast.S_pragma Ast.Pragma_wal_batch_commits
      | "wal_batch_interval_ms" -> Ast.S_pragma Ast.Pragma_wal_batch_interval_ms
```

- [ ] **Step 6: Add Plan ops**

In `lib/sql/plan.mli`, after `Op_pragma_set_wal_autocheckpoint of { n : int64 }` (~line 273), add:
```ocaml
  | Op_pragma_get_synchronous (** #298 read durability mode *)
  | Op_pragma_set_synchronous of { mode : string }
  | Op_pragma_get_wal_batch_commits
  | Op_pragma_set_wal_batch_commits of { n : int64 }
  | Op_pragma_get_wal_batch_interval_ms
  | Op_pragma_set_wal_batch_interval_ms of { n : int64 }
```

- [ ] **Step 7: Map AST → Plan in `plan_pragma`**

In `lib/sql/planner.ml`, in `plan_pragma` after the `Pragma_wal_autocheckpoint_set` arm (~line 833), add:
```ocaml
  | Ast.Pragma_synchronous -> Plan.Op_pragma_get_synchronous
  | Ast.Pragma_synchronous_set mode -> Plan.Op_pragma_set_synchronous { mode }
  | Ast.Pragma_wal_batch_commits -> Plan.Op_pragma_get_wal_batch_commits
  | Ast.Pragma_wal_batch_commits_set n -> Plan.Op_pragma_set_wal_batch_commits { n }
  | Ast.Pragma_wal_batch_interval_ms -> Plan.Op_pragma_get_wal_batch_interval_ms
  | Ast.Pragma_wal_batch_interval_ms_set n -> Plan.Op_pragma_set_wal_batch_interval_ms { n }
```
Also add the same six AST constructors to the `assert false` list in `plan_pragma_rows` (~line 813, the big OR-pattern that ends `-> assert false`):
```ocaml
  | Ast.Pragma_synchronous
  | Ast.Pragma_synchronous_set _
  | Ast.Pragma_wal_batch_commits
  | Ast.Pragma_wal_batch_commits_set _
  | Ast.Pragma_wal_batch_interval_ms
  | Ast.Pragma_wal_batch_interval_ms_set _
```

- [ ] **Step 8: exec — op labels (keep the explain match exhaustive)**

In `lib/sql/exec.ml`, after the `Op_pragma_set_wal_autocheckpoint` label arm (~line 5416), add:
```ocaml
  | Plan.Op_pragma_get_synchronous -> "Pragma(get_synchronous)"
  | Plan.Op_pragma_set_synchronous { mode } -> Printf.sprintf "Pragma(set_synchronous=%s)" mode
  | Plan.Op_pragma_get_wal_batch_commits -> "Pragma(get_wal_batch_commits)"
  | Plan.Op_pragma_set_wal_batch_commits { n } -> Printf.sprintf "Pragma(set_wal_batch_commits=%Ld)" n
  | Plan.Op_pragma_get_wal_batch_interval_ms -> "Pragma(get_wal_batch_interval_ms)"
  | Plan.Op_pragma_set_wal_batch_interval_ms { n } -> Printf.sprintf "Pragma(set_wal_batch_interval_ms=%Ld)" n
```

- [ ] **Step 9: exec — execute the setters**

In `lib/sql/exec.ml`, after the `Op_pragma_set_wal_autocheckpoint` execute arm (~line 6466), add:
```ocaml
  | Plan.Op_pragma_set_synchronous { mode } ->
    let d =
      match mode with
      | "full" -> S.Full
      | "off" -> S.Off
      | "batched" ->
        S.Batched
          { commits = S.sync_batch_commits store
          ; interval_ms = S.sync_batch_interval_ms store
          }
      | _ -> failwith (Printf.sprintf "PRAGMA synchronous: unknown mode %s" mode)
    in
    S.set_durability store d;
    Lwt.return 0
  | Plan.Op_pragma_set_wal_batch_commits { n } ->
    S.set_sync_batch_commits store (Int64.to_int n);
    Lwt.return 0
  | Plan.Op_pragma_set_wal_batch_interval_ms { n } ->
    S.set_sync_batch_interval_ms store (Int64.to_int n);
    Lwt.return 0
```
> `S` here is the store module alias used by `exec.ml` (the same one as `S.set_wal_autocheckpoint`). Confirm the alias name at the top of `exec.ml`; if it is `Store`, use that prefix instead.

- [ ] **Step 10: exec — classify the setters as writes**

In `lib/sql/exec.ml`, in the `is_write` OR-pattern (~line 9337, after `Op_pragma_set_wal_autocheckpoint _`), add:
```ocaml
        | Plan.Op_pragma_set_synchronous _
        | Plan.Op_pragma_set_wal_batch_commits _
        | Plan.Op_pragma_set_wal_batch_interval_ms _
```

- [ ] **Step 11: exec — stream the getters**

In `lib/sql/exec.ml`, after the `Op_pragma_get_wal_autocheckpoint` to_stream arm (~line 9500), add:
```ocaml
  | Plan.Op_pragma_get_synchronous ->
    let s =
      match S.durability store with
      | S.Full -> "full"
      | S.Batched _ -> "batched"
      | S.Off -> "off"
    in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_text s |] ])
  | Plan.Op_pragma_get_wal_batch_commits ->
    let n = S.sync_batch_commits store in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int (Int64.of_int n) |] ])
  | Plan.Op_pragma_get_wal_batch_interval_ms ->
    let n = S.sync_batch_interval_ms store in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int (Int64.of_int n) |] ])
```

- [ ] **Step 12: Build, fix any remaining non-exhaustive-match errors, run tests**

Run: `$PODMAN dune build 2>&1 | tail -40`
The compiler may flag additional exhaustive matches over `Plan.op` (e.g. a cost/labeling helper). Add the obvious mirror arms (getters → read-like, setters → write-like) following the pattern of the nearest `Op_pragma_*` neighbor. Then:
Run: `$PODMAN dune exec test/test_durability_298.exe 2>&1 | tail -30`
Expected: `pragma` group passes.

- [ ] **Step 13: Regression — parser + planner + exec suites**

Run:
```bash
$PODMAN dune exec test/test_parser.exe 2>&1 | tail -10
$PODMAN dune exec test/test_planner.exe 2>&1 | tail -10
$PODMAN dune exec test/test_exec.exe 2>&1 | tail -10
```
Expected: all green.

- [ ] **Step 14: Format and commit**

```bash
git add lib/sql/ast.mli lib/sql/parser.mly lib/sql/plan.mli lib/sql/planner.ml lib/sql/exec.ml test/test_durability_298.ml
git commit -m "feat(#298): PRAGMA synchronous/wal_batch_commits/wal_batch_interval_ms

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 5: Db open-option + clock threading

**Files:**
- Modify: `lib/db/db.ml` (`of_store` ~line 155, `open_block` ~line 180, `open_in_memory` ~line 92)
- Modify: `lib/db/db.mli` (`of_store` ~line 66, `open_block` ~line 50, `open_in_memory` ~line 39)
- Test: `test/test_durability_298.ml`

- [ ] **Step 1: Write the failing test (open-option sets the mode)**

Add to `test/test_durability_298.ml`. This needs a block-backed open with a durability option; use the lower-level `Sqlocaml_unix` open of a store, then `Db.of_store ~durability`:
```ocaml
let test_of_store_durability_option () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    let* db =
      D.of_store ~durability:(Sqlocaml_store.Store.Batched { commits = 8; interval_ms = 20 }) st
    in
    let* v = query1_text db "PRAGMA synchronous" in
    Alcotest.(check string) "of_store option applied" "batched" v;
    let* n = query1_int db "PRAGMA wal_batch_commits" in
    Alcotest.(check int) "of_store N applied" 8 n;
    let* () = D.close db in
    Lwt.return_unit)
;;
```
Runner group:
```ocaml
    ; "open-option",
      [ Alcotest.test_case "of_store ?durability" `Quick test_of_store_durability_option ]
```

- [ ] **Step 2: Run, expect COMPILE failure (`of_store` has no `?durability`)**

Run: `$PODMAN dune exec test/test_durability_298.exe 2>&1 | tail -20`
Expected: build error — unknown labelled arg `~durability`.

- [ ] **Step 3: Thread the option through `of_store`**

In `lib/db/db.ml`, change `of_store` (~line 155) to set clock + durability on the store before loading the catalog:
```ocaml
let of_store ?clock ?durability ?file_path store =
  (match clock with
   | Some c -> S.set_clock store c
   | None -> ());
  (match durability with
   | Some d -> S.set_durability store d
   | None -> ());
  let* catalog = Cat.open_ store in
  ...
```
(The rest of `of_store` body is unchanged.)

- [ ] **Step 4: Thread through `open_block` and `open_in_memory`**

In `lib/db/db.ml`, change `open_block` (~line 180) to accept and forward the options:
```ocaml
let open_block ?geom ?clock ?durability ~read_page ~write_page ~sync ~resize ~n_pages ~close ()
  : (t, error) result Lwt.t
  =
  let* result =
    S.open_block ?geom ~init_if_corrupt:true ~read_page ~write_page ~sync ~resize ~n_pages ~close ()
  in
  match result with
  | Error e -> Lwt.return (Error (Runtime (Format.asprintf "%a" S.pp_error e)))
  | Ok store ->
    let* db = of_store ?clock ?durability store in
    Lwt.return (Ok db)
;;
```
In `open_in_memory` (~line 92), set the clock on the (Mem) store for API symmetry — it is a no-op but keeps the path uniform. Change the first line to capture and apply:
```ocaml
let open_in_memory ?clock () =
  let store = S.create () in
  (match clock with Some c -> S.set_clock store c | None -> ());
  let* catalog = Cat.open_ store in
  ...
```

- [ ] **Step 5: Update `db.mli` signatures**

In `lib/db/db.mli`:
- `of_store` (~line 66): insert `-> ?durability:Sqlocaml_store.Store.durability` after the `?clock` line.
- `open_block` (~line 50): insert `-> ?clock:(unit -> float)` and `-> ?durability:Sqlocaml_store.Store.durability` after the `?geom` line.
- Update the `of_store` / `open_block` doc-comments to mention the durability option (database-wide; see Store docs).

Example for `open_block`:
```ocaml
val open_block
  :  ?geom:Sqlocaml_storage.Geometry.t
  -> ?clock:(unit -> float)
  -> ?durability:Sqlocaml_store.Store.durability
  -> read_page:(page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> ...
```

- [ ] **Step 6: Build and run, expect PASS**

Run: `$PODMAN dune build 2>&1 | tail -30 && $PODMAN dune exec test/test_durability_298.exe 2>&1 | tail -20`
Expected: `open-option` group passes.

- [ ] **Step 7: Regression — e2e + all-backends**

Run:
```bash
$PODMAN dune exec test/test_e2e.exe 2>&1 | tail -10
$PODMAN dune exec test/test_all_backends.exe 2>&1 | tail -10
```
Expected: green.

- [ ] **Step 8: Format and commit**

```bash
git add lib/db/db.ml lib/db/db.mli test/test_durability_298.ml
git commit -m "feat(#298): Db open-option (?durability/?clock) threaded via of_store

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 6: Crash-recovery prefix property

**Files:**
- Test: `test/test_durability_298.ml`
- Reference (read first): `test/test_crash_recovery.ml`, `test/test_crash_property.ml` for the project's crash-simulation idiom (how it truncates/drops the WAL tail and reopens).

- [ ] **Step 1: Read the existing crash harness**

Run: `sed -n '1,80p' test/test_crash_recovery.ml` and skim `test/test_crash_property.ml` to learn how the suite simulates a crash (typically: commit in `off`/`batched`, do NOT clean-close, truncate the `-wal` file at a frame boundary, reopen, assert recovered state is a commit-prefix).

- [ ] **Step 2: Write the property test (recovered state is a prefix of acked commits)**

Add to `test/test_durability_298.ml` a test that, in `off` mode: opens a fresh WAL db, performs K commits of keys `k0000..k{K-1}` WITHOUT clean close (drop the store handle without `S.close` — or copy the harness's crash primitive), reopens, and asserts: the set of recovered keys is a **prefix** `k0000..k{j-1}` for some `0 <= j <= K` (no holes, no torn values). Model it on `test_off_durable_after_close` but skipping `S.close` and using the crash primitive from Step 1. Example skeleton (adapt the crash primitive to match the existing harness):
```ocaml
let test_off_recovers_prefix () =
  let path = fresh_path () in
  cleanup path;
  (* Phase 1: write in off mode, simulate crash (no clean close). *)
  run
    (let* st = open_st path in
     S.set_durability st S.Off;
     S.set_wal_autocheckpoint st 0;
     do_commits st 40);   (* NOTE: intentionally no S.close — simulated crash *)
  (* Phase 2: reopen and verify prefix-consistency. *)
  run
    (let* st = open_st path in
     let* tx = S.ro_begin st in
     let rec scan i seen_gap =
       if i = 40 then Lwt.return_unit
       else
         let* v = S.get tx 16 (bs (Printf.sprintf "k%04d" i)) in
         match v, seen_gap with
         | Some _, true ->
           Alcotest.failf "hole then key at %d — not a prefix" i
         | None, _ -> scan (i + 1) true
         | Some _, false -> scan (i + 1) false
     in
     let* () = scan 0 false in
     let* () = S.ro_end tx in
     S.close st);
  cleanup path
;;
```
Runner group:
```ocaml
    ; "recovery",
      [ Alcotest.test_case "off recovers a commit-prefix" `Quick test_off_recovers_prefix ]
```

- [ ] **Step 3: Run, expect PASS**

Run: `$PODMAN dune exec test/test_durability_298.exe 2>&1 | tail -20`
Expected: prefix property holds (recovery already trusts only checksum-valid commit frames). If the no-clean-close path actually flushes everything (single-process page cache), the test still passes trivially as a prefix; to make it a real crash, use the truncation primitive from the existing harness.

- [ ] **Step 4: Run the existing crash suites as regression**

Run:
```bash
$PODMAN dune exec test/test_crash_recovery.exe 2>&1 | tail -10
$PODMAN dune exec test/test_crash_property.exe 2>&1 | tail -10
```
Expected: green (we did not touch recovery).

- [ ] **Step 5: Format and commit**

```bash
git add test/test_durability_298.ml
git commit -m "test(#298): off-mode recovery yields a commit-prefix

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 7: Documentation + caveat

**Files:**
- Modify: `README.md` (add a "Durability" subsection; locate the existing PRAGMA / WAL docs first with `grep -n "wal_autocheckpoint\|PRAGMA\|synchronous" README.md`)
- The `store.mli` `durability` doc-comment was already added in Task 1 — verify it states the caveats.

- [ ] **Step 1: Add the README durability section**

Add a subsection documenting:
- The three modes table (mode / commit fsync / app-crash safety / OS-power-crash exposure) — copy from the spec.
- `PRAGMA synchronous = full | batched | off`, `PRAGMA wal_batch_commits = N` (default 256), `PRAGMA wal_batch_interval_ms = T` (default 100), and the `Db.open_block ?durability` option.
- **Caveat block (prominent):** `batched`/`off` give no write-ordering guarantee under OS/power loss (LMDB-`NOSYNC` analogue); app-process crash is always safe; `batched` bounds the loss window by N commits or T ms; `off` is for rebuildable data. The time bound (T) is opportunistic (checked at commit/checkpoint), and needs a clock supplied via `?clock`; a fully idle database is not flushed until the next commit / checkpoint / close.
- **Scoping:** the setting is database-wide (shared commit queue), not per-connection like SQLite.

- [ ] **Step 2: Build the docs sanity (no code change) and commit**

```bash
git add README.md
git commit -m "docs(#298): document durability modes, PRAGMAs, and OS-crash caveat

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 8: Full suite + wrap-up

- [ ] **Step 1: Run the whole test suite inside the container**

Run: `$PODMAN dune runtest 2>&1 | tail -40`
Expected: all tests green. Investigate and fix any regression before proceeding.

- [ ] **Step 2: Format check across the tree**

Run: `bash scripts/check-fmt.sh 2>&1 | tail -20` (or the project's documented format-check). Fix any diffs.

- [ ] **Step 3: File the deferred follow-up issue**

File on Forgejo (`tej/sqlite_ocaml_port`): "bench(#298): per-mode commit-throughput on the HDD host profile" — run `batched`/`off` vs `full` via the #222 harness on the small HDD host to quantify the hosting-density win. Reference this PR.

- [ ] **Step 4: Open the PR**

```bash
git push -u origin feat-298-durability-knob
~/.local/bin/forgejo pr create tej/sqlite_ocaml_port \
  --title="feat(#298): per-deployment durability knob (synchronous full/batched/off)" \
  --head=feat-298-durability-knob --base=main \
  --body="Implements #298 ... (summary + caveat + link to spec). Closes #298."
```

---

## Self-Review (completed by plan author)

- **Spec coverage:** modes (T1-T3), surface PRAGMA+open-option (T4-T5), database-wide scoping (T4/T5/T7 docs), checkpoint/close anchors (T2-T3), recovery prefix (T6), Jepsen — *noted as manual validation in T6/T8 regression of existing crash suites; the dedicated Jepsen run is a manual harness step recorded in the PR, not a code task*, docs + caveat (T7), bench deferred (T8 follow-up issue). ✓
- **Placeholder scan:** none — every code step has concrete code; the only adaptive step is T6's crash primitive, which explicitly defers to the existing harness idiom (read in T6 Step 1). ✓
- **Type consistency:** `durability = Full | Batched of {commits;interval_ms} | Off` and accessors `durability` / `set_durability` / `sync_batch_commits` / `set_sync_batch_commits` / `sync_batch_interval_ms` / `set_sync_batch_interval_ms` / `set_clock` are used identically in T1 (def), T2-T3 (commit/close use raw `bt_state` fields), T4 (exec), T5 (Db). Plan ops `Op_pragma_{get,set}_{synchronous,wal_batch_commits,wal_batch_interval_ms}` consistent across plan.mli/planner/exec. ✓
