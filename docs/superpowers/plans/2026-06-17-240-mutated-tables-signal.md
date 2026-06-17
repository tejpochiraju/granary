# #240 — Mutated-Tables Write-Path Signal — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Expose the set of user tables a write statement actually mutated — including tables touched indirectly by FK cascades and triggers — so an external read cache (camel, tej/camel#56) can invalidate exactly those tables.

**Architecture:** Mirror #239's `query_stats` mechanism. An opaque accumulator (a `string` hash-set) rides Lwt sequence-associated storage on a `dirty_tables_key`, installed around a statement by `Exec.with_dirty`. Every physical-mutation site calls `mark_dirty name`, a no-op when no accumulator is installed (so plain `execute`/`run` pay nothing). Three public `_with_dirty` wrappers in `Db` install an accumulator around the existing `execute`/`execute_change_count`/`run`, then drain it to a sorted, deduplicated, user-tables-only `string list`.

**Tech Stack:** OCaml 5.x, Lwt, Alcotest + QCheck. Build/test run inside the `sqlocaml-dev` podman container.

**Spec:** `docs/superpowers/specs/2026-06-17-240-mutated-tables-signal-design.md`

---

## Conventions for every build/test/format command

This repo never runs `dune` on the host. From the worktree root
(`.worktrees/240-mutated-tables`), substitute `"$(pwd)"` for the workspace mount.

```sh
# build
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune build
# run one test executable
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_dirty_tables_240.exe
# whole suite
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test
# format one .ml/.mli (write back to host)
tmp=$(mktemp) && podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
  ocamlformat lib/sql/exec.ml > "$tmp" && mv "$tmp" lib/sql/exec.ml && chmod 644 lib/sql/exec.ml
# format a dune file
tmp=$(mktemp) && podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
  dune format-dune-file test/dune > "$tmp" && mv "$tmp" test/dune && chmod 644 test/dune
# merlint (expect 0 issues for our files)
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev merlint
```

A build run prints a pre-existing `sqlite3 not found` warning — that is expected
and unrelated; merlint ignores it.

---

## File Structure

- **`lib/sql/exec.ml`** — add the accumulator machinery (type, constructor, Lwt
  key, `mark_dirty`, `with_dirty`, `dirty_elements`, `is_internal_table_name`)
  next to the existing `query_stats` machinery; add 5 one-line `mark_dirty`
  calls at the physical-mutation sites.
- **`lib/sql/exec.mli`** — export the opaque `dirty_tables_acc`, `make_dirty_acc`,
  `with_dirty`, `dirty_elements` (each with a `(** … *)` doc comment).
- **`lib/db/db.ml`** — add `type dirty_tables = string list` and the three
  `_with_dirty` wrappers.
- **`lib/db/db.mli`** — export `dirty_tables` and the three wrappers (doc comments).
- **`test/test_dirty_tables_240.ml`** — new Alcotest+QCheck suite.
- **`test/dune`** — register the new test in both `(names …)` lists.

### Why `with_dirty` wraps the *public* `execute`/`run` (not a threaded `?dirty`)

The accumulator is ambient (Lwt storage), so installing it once around the whole
statement covers every nested write — main DML, FK cascades, FOR-EACH-ROW
triggers (which re-enter `execute_insert`/`execute_update`/`execute_delete`),
and INSTEAD-OF view triggers — with no signature changes to `execute_with_count`
or the recursive cascade helpers. This is the minimal, lowest-risk surface.

### The five `mark_dirty` sites (all have the table name in scope)

| # | Function (`lib/sql/exec.ml`) | Condition | Covers |
|---|---|---|---|
| 1 | `execute_insert` (~3494) | row was inserted (`inserted = true`) | main + trigger-body INSERT |
| 2 | `execute_update` (~5106) | rows changed (`n > 0`) | main + trigger-body UPDATE |
| 3 | `execute_delete` (~5428) | rows changed (`n > 0`) | main + trigger-body DELETE |
| 4 | `delete_row_in_tx` (~3873) | unconditional (always deletes) | FK `ON DELETE CASCADE` child rows |
| 5 | `update_col_in_tx` (~3899) | unconditional (always writes) | FK `SET NULL` / `SET DEFAULT` / `ON UPDATE CASCADE` child rows |

Triggers re-enter sites 1–3; cascades hit sites 4–5. `delete_row_in_tx` /
`update_col_in_tx` are used *only* by the cascade helpers (verified: their sole
callers are `cascade_delete_row_in_tx` / `cascade_update_col_in_tx`), so an
unconditional mark there is correct and never fires on a non-mutating path.

---

## Task 1: Accumulator machinery + `execute_with_dirty` + INSERT mark

**Files:**
- Modify: `lib/sql/exec.ml` (add machinery near line ~5717; mark in `execute_insert`)
- Modify: `lib/sql/exec.mli` (exports after the `make_query_stats` block ~line 131)
- Modify: `lib/db/db.ml` (`type dirty_tables`; `execute_with_dirty`)
- Modify: `lib/db/db.mli` (exports)
- Create: `test/test_dirty_tables_240.ml`
- Modify: `test/dune`

- [ ] **Step 1: Write the failing test**

Create `test/test_dirty_tables_240.ml`:

```ocaml
(** #240: the set of user tables a write statement actually mutated, exposed via
    {!Db.execute_with_dirty} / {!Db.execute_change_count_with_dirty} /
    {!Db.run_with_dirty}.  The load-bearing property for an external read cache
    is completeness under FK cascades and triggers: a write to one table that
    silently mutates another (inside the engine) reports BOTH. *)

module Db = Sqlocaml.Db

let run = Lwt_main.run

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "db error: %a" Db.pp_error e
;;

let with_db f =
  let db = run (Db.open_in_memory ()) in
  Fun.protect
    ~finally:(fun () ->
      try run (Db.close db) with
      | _ -> ())
    (fun () -> f db)
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

(* Run [sql] via [execute_with_dirty] and return the dirtied user tables. *)
let dirty db sql : string list = unwrap (run (Db.execute_with_dirty db sql))

let check_dirty msg expected sql_result =
  Alcotest.(check (list string)) msg expected sql_result
;;

let test_insert_marks_table () =
  with_db (fun db ->
    exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, n TEXT)";
    check_dirty "plain insert" [ "users" ] (dirty db "INSERT INTO users VALUES (1, 'a')"))
;;

let test_insert_or_ignore_noop_is_empty () =
  with_db (fun db ->
    exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, n TEXT)";
    exec db "INSERT INTO users VALUES (1, 'a')";
    (* PK conflict + OR IGNORE inserts nothing → no table dirtied. *)
    check_dirty
      "insert-or-ignore no-op"
      []
      (dirty db "INSERT OR IGNORE INTO users VALUES (1, 'b')"))
;;

let test_ddl_is_empty () =
  with_db (fun db ->
    check_dirty "create table dirties nothing" [] (dirty db "CREATE TABLE t (x INTEGER)"))
;;

let () =
  Alcotest.run
    "dirty_tables_240"
    [ ( "core"
      , [ Alcotest.test_case "insert marks table" `Quick test_insert_marks_table
        ; Alcotest.test_case
            "insert-or-ignore no-op empty"
            `Quick
            test_insert_or_ignore_noop_is_empty
        ; Alcotest.test_case "ddl empty" `Quick test_ddl_is_empty
        ] )
    ]
;;
```

- [ ] **Step 2: Register the test in `test/dune`**

Add `test_dirty_tables_240` to BOTH `(names …)` lists (there are two — one in the
`(tests …)` stanza near line 7, one near line 80). Insert it right after
`test_query_stats_257` in each list. Then format the dune file:

```sh
tmp=$(mktemp) && podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
  dune format-dune-file test/dune > "$tmp" && mv "$tmp" test/dune && chmod 644 test/dune
```

- [ ] **Step 3: Run the test to verify it fails (compile error — symbol absent)**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_dirty_tables_240.exe
```
Expected: FAIL — `Unbound value Db.execute_with_dirty`.

- [ ] **Step 4: Add the accumulator machinery to `lib/sql/exec.ml`**

Insert immediately after the `query_stats_key` definition (~line 5717):

```ocaml
(* #240: the set of user tables a write statement actually mutated, accumulated
   for an external read cache.  Like [query_stats] it rides Lwt
   sequence-associated storage so it need not be threaded through the DML and
   recursive cascade/trigger paths: [with_dirty] installs a fresh accumulator
   for the statement; every physical-mutation site calls [mark_dirty], a no-op
   when no accumulator is installed (plain [execute]/[run] callers pay nothing —
   a single predicted branch off the hot path, never per-row in the common case). *)
type dirty_tables_acc = (string, unit) Hashtbl.t

let make_dirty_acc () : dirty_tables_acc = Hashtbl.create 8
let dirty_tables_key : dirty_tables_acc Lwt.key = Lwt.new_key ()

(* Reserved-prefix internal tables ([sqlite_…] master/sequence, [sys_…] catalog
   trees) are never reported: an external cache only invalidates user tables. *)
let is_internal_table_name (name : string) =
  String.starts_with ~prefix:"sqlite_" name || String.starts_with ~prefix:"sys_" name
;;

(* Record that [name]'s rows changed in the current statement. *)
let mark_dirty (name : string) =
  match Lwt.get dirty_tables_key with
  | None -> ()
  | Some h -> Hashtbl.replace h name ()
;;

(* Install [acc] as the active write-path mutation sink for [f]'s dynamic extent
   (propagated across binds, so nested cascade/trigger writes record into it). *)
let with_dirty (acc : dirty_tables_acc) (f : unit -> 'a Lwt.t) : 'a Lwt.t =
  Lwt.with_value dirty_tables_key (Some acc) f
;;

(* Drain to the public shape: user tables only, deduplicated, sorted. *)
let dirty_elements (h : dirty_tables_acc) : string list =
  Hashtbl.fold (fun k () acc -> if is_internal_table_name k then acc else k :: acc) h []
  |> List.sort_uniq String.compare
;;
```

- [ ] **Step 5: Add the INSERT mark in `execute_insert`**

In `execute_insert` (~line 3494) the success path computes a `bool` named
`inserted` and ends with `Lwt.return inserted` (the row-was-written result; an
`OR IGNORE`/`OR REPLACE`-displaced conflict yields `false`). Immediately before
that `Lwt.return inserted`, add the guarded mark:

```ocaml
      if inserted then mark_dirty table_meta.Cat.name;
      Lwt.return inserted
```

(`table_meta` is the labelled argument already in scope.)

- [ ] **Step 6: Export the machinery in `lib/sql/exec.mli`**

Insert after the `make_query_stats` doc/val block (~line 131):

```ocaml
(** #240: opaque accumulator for the set of user tables a write statement
    mutated.  Install it around a statement with {!with_dirty} and read the
    result with {!dirty_elements}. *)
type dirty_tables_acc

(** A fresh, empty {!dirty_tables_acc}. *)
val make_dirty_acc : unit -> dirty_tables_acc

(** [with_dirty acc f] runs [f] with [acc] installed as the active write-path
    mutation sink, so every table mutated by [f] — directly, or indirectly via
    FK cascades and triggers — is recorded in [acc].  Nestable and independent
    of the {!query} stats context. *)
val with_dirty : dirty_tables_acc -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** The user tables recorded in [acc]: sorted, deduplicated, with
    internal/system tables ([sqlite_…]/[sys_…]) excluded. *)
val dirty_elements : dirty_tables_acc -> string list
```

- [ ] **Step 7: Add `type dirty_tables` and `execute_with_dirty` in `lib/db/db.ml`**

Add the type next to the re-exported `query_stats` (~line 68):

```ocaml
(* #240: user tables whose rows a write statement actually mutated, including
   tables touched indirectly by triggers and FK cascades.  Sorted, deduplicated;
   internal/system tables excluded. *)
type dirty_tables = string list
```

Add the wrapper immediately after `execute_change_count` (~line 1630, before the
`query_impl` comment):

```ocaml
let execute_with_dirty top sql =
  let acc = Sql.Exec.make_dirty_acc () in
  let* r = Sql.Exec.with_dirty acc (fun () -> execute top sql) in
  match r with
  | Error e -> Lwt.return (Error e)
  | Ok () -> Lwt.return (Ok (Sql.Exec.dirty_elements acc))
;;
```

- [ ] **Step 8: Export in `lib/db/db.mli`**

After the `query_with_stats` val (~line 165) add the type and the first wrapper:

```ocaml
(** #240: the set of user tables whose rows a write statement actually mutated,
    including tables touched indirectly by triggers and FK cascades.  Sorted and
    deduplicated; internal/system tables are excluded.  Enables an external read
    cache to invalidate exactly the tables that changed. *)
type dirty_tables = string list

(** Like {!execute}, but also returns the {!dirty_tables} the statement mutated.
    For a pure-DDL or no-op write (e.g. [INSERT OR IGNORE] that inserts nothing)
    the list is empty. *)
val execute_with_dirty : t -> string -> (dirty_tables, error) result Lwt.t
```

- [ ] **Step 9: Build, format, run the test to verify it passes**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune build
# format every file touched
for f in lib/sql/exec.ml lib/sql/exec.mli lib/db/db.ml lib/db/db.mli test/test_dirty_tables_240.ml; do
  tmp=$(mktemp) && podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
    ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"; done
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_dirty_tables_240.exe
```
Expected: PASS (3 cases).

- [ ] **Step 10: Commit**

```sh
git add lib/sql/exec.ml lib/sql/exec.mli lib/db/db.ml lib/db/db.mli \
  test/test_dirty_tables_240.ml test/dune
git commit -m "feat(#240): dirty-tables accumulator + execute_with_dirty (INSERT)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: UPDATE / DELETE marks + `execute_change_count_with_dirty`

**Files:**
- Modify: `lib/sql/exec.ml` (marks in `execute_update`, `execute_delete`)
- Modify: `lib/db/db.ml`, `lib/db/db.mli` (`execute_change_count_with_dirty`)
- Modify: `test/test_dirty_tables_240.ml`

- [ ] **Step 1: Add failing tests**

Add these functions before the `let ()` runner in `test/test_dirty_tables_240.ml`:

```ocaml
let test_update_hit_and_miss () =
  with_db (fun db ->
    exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, n TEXT)";
    exec db "INSERT INTO users VALUES (1, 'a')";
    check_dirty "update hit" [ "users" ] (dirty db "UPDATE users SET n = 'b' WHERE id = 1");
    check_dirty "update miss" [] (dirty db "UPDATE users SET n = 'c' WHERE id = 999"))
;;

let test_delete_hit_and_miss () =
  with_db (fun db ->
    exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, n TEXT)";
    exec db "INSERT INTO users VALUES (1, 'a')";
    check_dirty "delete miss" [] (dirty db "DELETE FROM users WHERE id = 999");
    check_dirty "delete hit" [ "users" ] (dirty db "DELETE FROM users WHERE id = 1"))
;;

(* execute_change_count_with_dirty returns BOTH the rows-affected count and the set. *)
let test_change_count_with_dirty () =
  with_db (fun db ->
    exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, n TEXT)";
    exec db "INSERT INTO users VALUES (1, 'a')";
    exec db "INSERT INTO users VALUES (2, 'a')";
    let n, tables =
      unwrap (run (Db.execute_change_count_with_dirty db "UPDATE users SET n = 'z'"))
    in
    Alcotest.(check int) "rows changed" 2 n;
    Alcotest.(check (list string)) "dirtied once (deduped)" [ "users" ] tables)
;;
```

Add to the `"core"` case list:

```ocaml
        ; Alcotest.test_case "update hit/miss" `Quick test_update_hit_and_miss
        ; Alcotest.test_case "delete hit/miss" `Quick test_delete_hit_and_miss
        ; Alcotest.test_case "change_count_with_dirty" `Quick test_change_count_with_dirty
```

- [ ] **Step 2: Run to verify failure**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_dirty_tables_240.exe
```
Expected: FAIL — `Unbound value Db.execute_change_count_with_dirty` (and update/delete
marks not yet placed).

- [ ] **Step 3: Add the UPDATE mark in `execute_update`**

In `execute_update` (~line 5106) the success path returns the changed-row count
`n` (an `int`) via `Lwt.return n` inside the `Lwt.catch` success branch.
Immediately before that `Lwt.return n`, add:

```ocaml
       if n > 0 then mark_dirty table_meta.Cat.name;
       Lwt.return n
```

- [ ] **Step 4: Add the DELETE mark in `execute_delete`**

In `execute_delete` (~line 5428) the success path likewise returns its
deleted-row count via `Lwt.return n`. Immediately before it, add:

```ocaml
       if n > 0 then mark_dirty table_meta.Cat.name;
       Lwt.return n
```

(If the count is bound under a different local name in either function, use that
name; the test pins the behaviour either way.)

- [ ] **Step 5: Add `execute_change_count_with_dirty` in `lib/db/db.ml`**

Immediately after `execute_with_dirty`:

```ocaml
let execute_change_count_with_dirty top sql =
  let acc = Sql.Exec.make_dirty_acc () in
  let* r = Sql.Exec.with_dirty acc (fun () -> execute_change_count top sql) in
  match r with
  | Error e -> Lwt.return (Error e)
  | Ok n -> Lwt.return (Ok (n, Sql.Exec.dirty_elements acc))
;;
```

- [ ] **Step 6: Export in `lib/db/db.mli`** — after `execute_with_dirty`:

```ocaml
(** Like {!execute_change_count}, but also returns the {!dirty_tables} the
    statement mutated alongside the rows-affected count. *)
val execute_change_count_with_dirty
  :  t
  -> string
  -> (int * dirty_tables, error) result Lwt.t
```

- [ ] **Step 7: Build, format, test**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune build
for f in lib/sql/exec.ml lib/db/db.ml lib/db/db.mli test/test_dirty_tables_240.ml; do
  tmp=$(mktemp) && podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
    ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"; done
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_dirty_tables_240.exe
```
Expected: PASS (6 cases).

- [ ] **Step 8: Commit**

```sh
git add lib/sql/exec.ml lib/db/db.ml lib/db/db.mli test/test_dirty_tables_240.ml
git commit -m "feat(#240): UPDATE/DELETE marks + execute_change_count_with_dirty

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: `run_with_dirty` (prepared statements)

**Files:**
- Modify: `lib/db/db.ml`, `lib/db/db.mli`
- Modify: `test/test_dirty_tables_240.ml`

- [ ] **Step 1: Add the failing test**

Add before the runner:

```ocaml
let test_run_with_dirty () =
  with_db (fun db ->
    exec db "CREATE TABLE users (id INTEGER PRIMARY KEY, n TEXT)";
    let st = unwrap (run (Db.prepare db "INSERT INTO users VALUES (?, ?)")) in
    let n, tables =
      unwrap
        (run (Db.run_with_dirty st ~params:[ Sqlocaml_encoding.Row.V_int 1L;
                                             Sqlocaml_encoding.Row.V_text "a" ]))
    in
    Alcotest.(check int) "one row" 1 n;
    Alcotest.(check (list string)) "run dirtied users" [ "users" ] tables)
;;
```

Add to the `"core"` list:

```ocaml
        ; Alcotest.test_case "run_with_dirty" `Quick test_run_with_dirty
```

Note: `Db.run` takes `params:value list` where `value = Sqlocaml_encoding.Row.value`
(confirm the exact constructor names by checking `lib/db/db.mli`'s `value` alias
and `Row.value` — use `V_int`/`V_text` as defined there).

- [ ] **Step 2: Run to verify failure**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_dirty_tables_240.exe
```
Expected: FAIL — `Unbound value Db.run_with_dirty`.

- [ ] **Step 3: Add `run_with_dirty` in `lib/db/db.ml`** — immediately after `run`:

```ocaml
let run_with_dirty st ~params =
  let acc = Sql.Exec.make_dirty_acc () in
  let* r = Sql.Exec.with_dirty acc (fun () -> run st ~params) in
  match r with
  | Error e -> Lwt.return (Error e)
  | Ok n -> Lwt.return (Ok (n, Sql.Exec.dirty_elements acc))
;;
```

- [ ] **Step 4: Export in `lib/db/db.mli`** — after the `run` val (~line 255):

```ocaml
(** Like {!run}, but also returns the {!dirty_tables} the prepared write
    mutated alongside the rows-affected count. *)
val run_with_dirty
  :  stmt
  -> params:value list
  -> (int * dirty_tables, error) result Lwt.t
```

- [ ] **Step 5: Build, format, test**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune build
for f in lib/db/db.ml lib/db/db.mli test/test_dirty_tables_240.ml; do
  tmp=$(mktemp) && podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
    ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"; done
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_dirty_tables_240.exe
```
Expected: PASS (7 cases).

- [ ] **Step 6: Commit**

```sh
git add lib/db/db.ml lib/db/db.mli test/test_dirty_tables_240.ml
git commit -m "feat(#240): run_with_dirty for prepared writes

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 4: FK-cascade marks (the reason #240 exists)

**Files:**
- Modify: `lib/sql/exec.ml` (marks in `delete_row_in_tx`, `update_col_in_tx`)
- Modify: `test/test_dirty_tables_240.ml`

- [ ] **Step 1: Add failing tests**

Add before the runner:

```ocaml
(* ON DELETE CASCADE: deleting the parent silently deletes child rows inside the
   engine — the set must include BOTH tables. *)
let test_on_delete_cascade () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE dept (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE emp (id INTEGER PRIMARY KEY, d INTEGER REFERENCES dept(id) ON \
       DELETE CASCADE)";
    exec db "INSERT INTO dept VALUES (1)";
    exec db "INSERT INTO emp VALUES (10, 1)";
    (* sorted, deduplicated: dept before emp *)
    check_dirty
      "delete cascade marks parent+child"
      [ "dept"; "emp" ]
      (dirty db "DELETE FROM dept WHERE id = 1"))
;;

(* ON DELETE SET NULL: the child row is UPDATEd (FK col set NULL), not deleted. *)
let test_on_delete_set_null () =
  with_db (fun db ->
    exec db "PRAGMA foreign_keys = ON";
    exec db "CREATE TABLE dept (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TABLE emp (id INTEGER PRIMARY KEY, d INTEGER REFERENCES dept(id) ON \
       DELETE SET NULL)";
    exec db "INSERT INTO dept VALUES (1)";
    exec db "INSERT INTO emp VALUES (10, 1)";
    check_dirty
      "set-null marks parent+child"
      [ "dept"; "emp" ]
      (dirty db "DELETE FROM dept WHERE id = 1"))
;;
```

Add a new `"cascades"` group to the runner's list (alongside `"core"`):

```ocaml
    ; ( "cascades"
      , [ Alcotest.test_case "on delete cascade" `Quick test_on_delete_cascade
        ; Alcotest.test_case "on delete set null" `Quick test_on_delete_set_null
        ] )
```

- [ ] **Step 2: Run to verify failure**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_dirty_tables_240.exe
```
Expected: FAIL — cascade child table missing from the set (e.g. got `["dept"]`,
expected `["dept"; "emp"]`).

- [ ] **Step 3: Add the cascade-delete mark in `delete_row_in_tx`**

`delete_row_in_tx` (~line 3873) begins:

```ocaml
let delete_row_in_tx tx (cat : Cat.t) (meta : Cat.table_meta) ~rowid ~(row : Row.t) =
```

Add `mark_dirty meta.Cat.name;` as the first expression of its body (it is an
unconditional physical delete; `meta` is the child table being mutated).

- [ ] **Step 4: Add the cascade-update mark in `update_col_in_tx`**

`update_col_in_tx` (~line 3899) takes a `meta : Cat.table_meta` argument and
unconditionally writes one column. Add `mark_dirty meta.Cat.name;` as the first
expression of its body.

- [ ] **Step 5: Build, format, test**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune build
for f in lib/sql/exec.ml test/test_dirty_tables_240.ml; do
  tmp=$(mktemp) && podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
    ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"; done
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_dirty_tables_240.exe
```
Expected: PASS (9 cases).

- [ ] **Step 6: Commit**

```sh
git add lib/sql/exec.ml test/test_dirty_tables_240.ml
git commit -m "feat(#240): mark FK-cascade child tables (delete/update primitives)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 5: Trigger coverage + internal-table filter (verification, no new lib code expected)

These behaviours should already hold from Tasks 1–4 (triggers re-enter
`execute_insert/update/delete`; `sqlite_sequence` is a view + filtered). The
tests PIN them. If a trigger test fails, the gap is real — debug with
`superpowers:systematic-debugging` before adding code.

**Files:**
- Modify: `test/test_dirty_tables_240.ml`
- Possibly modify: `lib/sql/exec.ml` (only if a gap is found)

- [ ] **Step 1: Add the tests**

```ocaml
(* AFTER INSERT trigger whose body writes a DIFFERENT table: both must appear. *)
let test_trigger_marks_both_tables () =
  with_db (fun db ->
    exec db "CREATE TABLE a (id INTEGER PRIMARY KEY)";
    exec db "CREATE TABLE b (id INTEGER PRIMARY KEY)";
    exec
      db
      "CREATE TRIGGER a_ai AFTER INSERT ON a BEGIN INSERT INTO b VALUES (NEW.id); END";
    check_dirty
      "trigger fans out to b"
      [ "a"; "b" ]
      (dirty db "INSERT INTO a VALUES (1)"))
;;

(* AUTOINCREMENT bumps the internal rowid counter (sqlite_sequence is a view);
   the internal name must NOT leak into the set. *)
let test_autoincrement_excludes_internal () =
  with_db (fun db ->
    exec db "CREATE TABLE s (id INTEGER PRIMARY KEY AUTOINCREMENT, n TEXT)";
    check_dirty
      "autoincrement insert: user table only"
      [ "s" ]
      (dirty db "INSERT INTO s (n) VALUES ('x')"))
;;
```

Add a `"semantics"` group to the runner:

```ocaml
    ; ( "semantics"
      , [ Alcotest.test_case "trigger marks both" `Quick test_trigger_marks_both_tables
        ; Alcotest.test_case
            "autoincrement excludes internal"
            `Quick
            test_autoincrement_excludes_internal
        ] )
```

- [ ] **Step 2: Build and run**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune build
tmp=$(mktemp) && podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
  ocamlformat test/test_dirty_tables_240.ml > "$tmp" && mv "$tmp" test/test_dirty_tables_240.ml && chmod 644 test/test_dirty_tables_240.ml
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_dirty_tables_240.exe
```
Expected: PASS (11 cases). If `trigger marks both` fails, the trigger body did
not re-enter the marked `execute_*` path — STOP and debug before proceeding.

- [ ] **Step 3: Commit**

```sh
git add test/test_dirty_tables_240.ml
git commit -m "test(#240): pin trigger fan-out + internal-table exclusion

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 6: QCheck property (dedup/sort under trigger fan-out) + final gates

**Files:**
- Modify: `test/test_dirty_tables_240.ml`
- Modify: `test/dune` only if `qcheck`/`qcheck-alcotest` is not already a dep of
  the test stanza (check first; the existing QCheck tests imply it is).

- [ ] **Step 1: Add the property test**

A chain `t0 → t1 → … → t(n-1)` of `AFTER INSERT` triggers means one insert into
`t0` mutates every table in the chain. The reported set must be exactly those
tables, sorted and duplicate-free, for any chain length.

```ocaml
let trigger_chain_property =
  QCheck.Test.make
    ~count:50
    ~name:"trigger chain: dirty set is the sorted unique chain"
    QCheck.(int_range 1 6)
    (fun n ->
      with_db (fun db ->
        let names = List.init n (fun i -> Printf.sprintf "t%d" i) in
        List.iter
          (fun nm -> exec db (Printf.sprintf "CREATE TABLE %s (id INTEGER PRIMARY KEY)" nm))
          names;
        (* chain: inserting into t(i) inserts into t(i+1) *)
        for i = 0 to n - 2 do
          exec
            db
            (Printf.sprintf
               "CREATE TRIGGER tr%d AFTER INSERT ON t%d BEGIN INSERT INTO t%d VALUES \
                (NEW.id); END"
               i
               i
               (i + 1))
        done;
        let got = dirty db "INSERT INTO t0 VALUES (1)" in
        let expected = List.sort_uniq String.compare names in
        (* sorted + unique + complete *)
        got = expected))
;;
```

Register it (QCheck via `QCheck_alcotest.to_alcotest`) in a `"property"` group:

```ocaml
    ; "property", [ QCheck_alcotest.to_alcotest trigger_chain_property ]
```

If the file does not yet open the QCheck-Alcotest bridge, reference it fully
qualified as `QCheck_alcotest.to_alcotest` (no `open` needed). Confirm the test
stanza in `test/dune` lists `qcheck-alcotest` among its libraries (the existing
property tests already pull it in; if `test_dirty_tables_240` is in the same
`(tests …)` stanza it inherits the libraries).

- [ ] **Step 2: Build, format, run the full new suite**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune build
tmp=$(mktemp) && podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
  ocamlformat test/test_dirty_tables_240.ml > "$tmp" && mv "$tmp" test/test_dirty_tables_240.ml && chmod 644 test/test_dirty_tables_240.ml
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_dirty_tables_240.exe
```
Expected: PASS (12 cases incl. property).

- [ ] **Step 3: Full suite + lint gates (must all pass before PR)**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test
# dune-file format check (expect no diff)
diff <(podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
  dune format-dune-file test/dune) test/dune
# merlint — expect 0 issues for lib/sql/exec.{ml,mli}, lib/db/db.{ml,mli}
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev merlint
```
Expected: whole suite green; no merlint findings for the four edited lib files.
Fix any merlint finding (missing doc comment, nesting, etc.) before committing.

- [ ] **Step 4: Coverage check for the new code (target 100% on new lines)**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
  bisect-ppx-report html --output _coverage_report/ _build/default/test/*.coverage || true
```
Inspect the `exec.ml`/`db.ml` additions; add a focused test for any uncovered
branch (e.g. the `Error` arm of a wrapper via a statement that fails to parse).

- [ ] **Step 5: Commit**

```sh
git add test/test_dirty_tables_240.ml test/dune
git commit -m "test(#240): QCheck trigger-chain property; dedup+sort invariant

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 7: Open the PR

- [ ] **Step 1: Push and open the PR**

```sh
git push origin feat/240-mutated-tables
~/.local/bin/forgejo pr create tej/sqlite_ocaml_port \
  --title="feat(#240): expose set of tables mutated by a write statement" \
  --head=feat/240-mutated-tables \
  --base=main \
  --body="$(cat <<'EOF'
## Summary
- Adds `Db.execute_with_dirty` / `execute_change_count_with_dirty` / `run_with_dirty`,
  returning the set of **user** tables a write statement actually mutated —
  including tables touched indirectly by FK cascades and triggers — so an
  external read cache (camel, tej/camel#56) can invalidate exactly those tables.
- Mechanism mirrors #239's `query_stats`: an ambient `Lwt.key` accumulator
  installed by `Exec.with_dirty`; ~5 one-line `mark_dirty` sites (3 main DML
  functions, which also cover trigger-body DML via re-entry; 2 FK-cascade
  primitives). Plain `execute`/`run` install no accumulator and pay nothing.
- Internal/system tables (`sqlite_…`/`sys_…`) are excluded; result is sorted and
  deduplicated.

## Test plan
- [ ] `dune test` passes (full suite)
- [ ] new `test/test_dirty_tables_240.ml`: INSERT/UPDATE/DELETE (+ no-op),
      change-count + prepared-stmt variants, ON DELETE CASCADE, ON DELETE SET
      NULL, trigger fan-out, AUTOINCREMENT internal-exclusion, QCheck
      trigger-chain dedup/sort property
- [ ] merlint clean; dune-file + ocamlformat clean

Closes #240
EOF
)"
```

---

## Self-Review notes (author)

- **Spec coverage:** API shape (3 `_with_dirty` variants, `string list`,
  user-tables-only) → Tasks 1–4; cascades → Task 4; triggers + internal filter →
  Task 5; dedup/sort → Tasks 2 (dedup) & 6 (property); every spec test scenario
  maps to a case.
- **Mechanism refinement vs spec:** the spec sketched a `?dirty` param on
  `execute_with_count`; this plan uses the cleaner ambient `Exec.with_dirty`
  wrapping the public `execute`/`run` (no hot-path signature churn, and it also
  covers INSTEAD-OF view triggers for free). Behaviour and contract are
  unchanged.
- **Type consistency:** `dirty_tables_acc` (exec, opaque) vs `dirty_tables =
  string list` (db, public); `make_dirty_acc` / `with_dirty` / `dirty_elements`
  / `mark_dirty` used identically across tasks.
- **Mark-site placement caveat:** exact local names (`inserted`, `n`) are stated
  with a fallback instruction; the failing tests are the guard. If `execute_insert`
  has multiple `Lwt.return inserted` sites, the guarded mark before the
  success/commit return is the correct one — the no-op test (`INSERT OR IGNORE`)
  pins that `false` must not mark.
```
