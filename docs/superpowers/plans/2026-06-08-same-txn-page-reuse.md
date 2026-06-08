# Same-Txn Page Reuse Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Relax the freelist guard at `rw_begin` so pages freed in the current transaction can be reused by subsequent `alloc` calls within the same transaction, eliminating CoW write-amplification file growth.

**Architecture:** A 1-line change in `store.ml` sets `alloc_min_safe` to `current_txn_id + 1` (instead of `current_txn_id`) when no reader is active. Pages freed at `current_txn_id` now satisfy `freed_at_txn_id < alloc_min_safe`, allowing `Freelist.pop` to return them. The reader-gating path (`Some m` branch) is unchanged.

**Tech Stack:** OCaml, Lwt, Alcotest

**Spec:** `docs/superpowers/specs/2026-06-08-same-txn-page-reuse-design.md`

---

### Task 1: Relax freelist guard at rw_begin

**Files:**
- Modify: `lib/store/store.ml:1152`

- [ ] **Step 1: Apply the 1-line change**

  ```ocaml
  (* Before: alloc_min_safe = current_txn_id — same-txn freed pages not reusable *)
  | None -> current_rw_txn_id

  (* After: alloc_min_safe = current_txn_id + 1 — same-txn freed pages allowed *)
  | None -> Int64.succ current_rw_txn_id
  ```

  Change in context (`lib/store/store.ml` lines 1148-1156):

  ```ocaml
  let current_rw_txn_id = Int64.add st.current_header.txn_id 1L in
  Pager.set_txn_id st.pager current_rw_txn_id;
  let min_safe =
    match min_active_reader_txn st with
    | None -> Int64.succ current_rw_txn_id  (* ← the change *)
    | Some m -> Int64.min current_rw_txn_id m
  in
  Pager.set_alloc_min_safe st.pager min_safe;
  ```

- [ ] **Step 2: Build to verify the change compiles**

  Run: `dune build lib/store/` — Expected: exit 0

- [ ] **Step 3: Commit**

  ```bash
  git add lib/store/store.ml
  git commit -m "perf(#297): relax freelist guard for same-txn page reuse

  Set alloc_min_safe to current_rw_txn_id + 1 at rw_begin when no reader
  is active. Pages freed at current_rw_txn_id become reusable within the
  same transaction, eliminating CoW write-amplification file growth.

  Previously: alloc_min_safe == current_txn_id, so freed_at == current_txn_id
  failed the guard freed_at < min_safe. Now: freed_at < current_txn_id + 1
  succeeds, and Freelist.pop returns the just-freed page.

  Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
  ```

---

### Task 2: Update existing test to expect same-txn reuse

**Files:**
- Modify: `test/test_pager.ml:183-196`

The existing test `test_free_then_alloc_same_txn` asserts that a page freed at `freed_at_txn_id=2` with `alloc_min_safe=2` is NOT reused. With our change, pages freed at `current_txn_id` ARE reused when `alloc_min_safe > current_txn_id`. We need to update this test to assert the new correct behavior.

- [ ] **Step 1: Update the test to expect page reuse**

  Change `test/test_pager.ml` from:

  ```ocaml
  let test_free_then_alloc_same_txn () =
    let p, _ = make_pager ~n_pages:5L () in
    Pager.free p ~page_id:3L ~freed_at_txn_id:2L;
    (* alloc_min_safe=2: freed_at(2) < 2 is false — page 3 not yet reusable *)
    Pager.set_alloc_min_safe p 2L;
    match run (Pager.alloc p) with
    | Error e -> Alcotest.failf "alloc failed: %a" Pager.pp_error e
    | Ok pid ->
      Alcotest.(check bool)
        "new page allocated, not freed one"
        true
        (Int64.compare pid 3L <> 0);
      Alcotest.(check int64) "new page = n_pages before (5)" 5L pid
  ;;
  ```

  To:

  ```ocaml
  let test_free_then_alloc_same_txn () =
    let p, _ = make_pager ~n_pages:5L () in
    Pager.free p ~page_id:3L ~freed_at_txn_id:2L;
    (* alloc_min_safe=3: freed_at(2) < 3 is true — page 3 IS reusable *)
    Pager.set_alloc_min_safe p 3L;
    match run (Pager.alloc p) with
    | Error e -> Alcotest.failf "alloc failed: %a" Pager.pp_error e
    | Ok pid -> Alcotest.(check int64) "reuses freed page 3" 3L pid
  ;;
  ```

  The change:
  1. `alloc_min_safe` changes from `2L` to `3L` (simulating `succ current_txn_id`)
  2. The assertion changes from "NOT page 3, but page 5" to "page 3 is reused"
  3. Drops the `Int64.compare` boolean check and replaces with a direct `check int64` against `3L`

- [ ] **Step 2: Run the test to verify it passes**

  Run: `dune exec test/test_pager.exe -- --test "same" 2>&1`

  Expected: `Test Same-txn free+alloc reuses page: OK`

- [ ] **Step 3: Commit**

  ```bash
  git add test/test_pager.ml
  git commit -m "test(#297): update test_free_then_alloc_same_txn for reuse

  With alloc_min_safe = current_txn_id + 1, pages freed at current_txn_id
  ARE reusable. Update the test to assert the page is returned from the
  freelist instead of allocated from file extension.

  Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
  ```

---

### Task 3: Add bounded n_pages test to store_btree

**Files:**
- Modify: `test/test_store_btree.ml`

Add a test that verifies `n_pages` growth is bounded by tree depth when inserting many rows in a single transaction.

- [ ] **Step 1: Add the test before the final `let () =` block**

  Add after the last test function (line 1645 `test_savepoint_then_commit_persists`):

  ```ocaml
  (* Same-txn page reuse (#297): inserting N rows in one transaction should
     not grow n_pages by N×depth. The freelist guard relaxation allows freed
     pages to be reused within the same transaction. *)
  let test_same_txn_page_reuse_bounded () =
    let path = Filename.temp_file "sqlocaml_reuse_" ".db" in
    Fun.protect
      ~finally:(fun () ->
        try Sys.remove path with
        | _ -> ())
      (fun () ->
        let store = Result.get_ok (run (S.open_file ~path ())) in
        (* Insert some rows in separate txns to build tree structure *)
        let n_setup = 10 in
        for i = 1 to n_setup do
          let tx = run (S.rw_begin store) in
          let key = Bytes.of_string (Printf.sprintf "%04d" i) in
          run (S.put tx 16 key (Bytes.of_string "v"));
          run (S.commit tx)
        done;
        let n_pages_before = S.n_pages store in
        (* Insert many more rows in a SINGLE transaction *)
        let n_insert = 200 in
        let tx = run (S.rw_begin store) in
        for i = n_setup + 1 to n_setup + n_insert do
          let key = Bytes.of_string (Printf.sprintf "%04d" i) in
          run (S.put tx 16 key (Bytes.of_string "v"))
        done;
        run (S.commit tx);
        let n_pages_after = S.n_pages store in
        let growth = Int64.(sub n_pages_after n_pages_before) in
        (* Growth should be bounded by tree depth × split pages, not by
           N_insert × depth. A 210-row single-tree insert should grow by
           well under 100 pages (each insert reuses freed pages). *)
        Alcotest.(check bool)
          (Printf.sprintf "n_pages growth %Ld < 100" growth)
          true
          (Int64.compare growth 100L < 0);
        (* Verify all data is correct *)
        let ro = run (S.ro_begin store) in
        for i = 1 to n_setup + n_insert do
          let key = Bytes.of_string (Printf.sprintf "%04d" i) in
          match run (S.get ro 16 key) with
          | Some v -> Alcotest.(check string) "correct value" "v" (Bytes.to_string v)
          | None -> Alcotest.failf "key %d not found" i
        done;
        run (S.ro_end ro);
        run (S.close store))
  ;;
  ```

  Then register it in the suite list (find the `let () =` block at the bottom). Look for the existing test list and add the new test:

  ```ocaml
  ; test_same_txn_page_reuse_bounded
  ```

  Find the `"store-btree"` test registration and add the new test name to the list. It should be placed alongside the other freelist-related tests (after `test_active_reader_gates_freelist`).

- [ ] **Step 2: Run the full test suite to verify nothing breaks**

  Run: `dune exec test/test_store_btree.exe 2>&1 | tail -20`

  Expected: All tests pass, including the new one.

- [ ] **Step 3: Commit**

  ```bash
  git add test/test_store_btree.ml
  git commit -m "test(#297): verify bounded n_pages growth within single txn

  Insert 200 rows in one transaction and assert n_pages growth stays under
  100 pages (bounded by tree depth × splits, not N×depth). Also verify all
  data reads back correctly via RO snapshot.

  Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>"
  ```

---

### Task 4: Run the full test suite

**Files:**
- Run the complete test suite

- [ ] **Step 1: Run all tests**

  Run: `dune runtest 2>&1 | tail -30`

  Expected: All tests pass.

- [ ] **Step 2: Run bench222 to verify batch-insert improvement**

  Run: `scripts/bench222.sh 2>&1 | head -60`

  Expected: Batch insert workload shows meaningful improvement (the current gap is ~44× slower than SQLite). Note: this is a long-running benchmark on a production host — only run if the test host is available and the user has confirmed.

- [ ] **Step 3: Commit if any fixups needed**

  ```bash
  git add -A
  git commit -m "fixup(#297): address test failures"
  ```
