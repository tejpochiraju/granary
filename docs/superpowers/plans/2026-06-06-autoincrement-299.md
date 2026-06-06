# AUTOINCREMENT Support (#299) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Parse, validate, persist, dump-round-trip, and honor the semantics of `INTEGER PRIMARY KEY AUTOINCREMENT` in the pure-OCaml SQLite port.

**Architecture:** A per-column `autoincrement` bit is parsed onto the AST `column_def` (the parser's `Col_primary_key` constraint variant carries a bool), validated in sema (only a single-column ascending `INTEGER PRIMARY KEY` on a rowid table), then folded to a **table-level** `autoincrement : bool` threaded through `BS_create_table → Op_create_table → Cat.create_table → table_meta`, persisted as a trailing backward-compatible field of `encode_table_value`. Allocation reuses the existing `next_rowid` counter unchanged. The single behavioral difference is on **ROLLBACK**: a plain rowid table recomputes `max(rowid)+1` from data (reuse), whereas an AUTOINCREMENT table restores the **committed** counter from the `_sys_tables` store row (sticky across committed DELETEs, but — matching real SQLite — still reverts a rolled-back allocation to the last-committed high-water).

**Tech Stack:** OCaml 5.1, Menhir (`parser.mly`), ocamllex (`lexer.mll`), Lwt, dune-in-podman build, alcotest + QCheck tests, `bisect_ppx`.

**Verified SQLite 3.45 oracle facts (host `sqlite3`):**
- `TEXT PRIMARY KEY AUTOINCREMENT` → error `AUTOINCREMENT is only allowed on an INTEGER PRIMARY KEY`.
- `INTEGER PRIMARY KEY DESC AUTOINCREMENT` → same error (DESC disqualifies the alias).
- `INTEGER PRIMARY KEY ASC AUTOINCREMENT` → **accepted**.
- `... AUTOINCREMENT) WITHOUT ROWID` → error `AUTOINCREMENT not allowed on WITHOUT ROWID tables`.
- composite `PRIMARY KEY(a,b) AUTOINCREMENT` → syntax error (our grammar rejects this naturally — table-level PK production has no AUTOINCREMENT).
- After a committed `DELETE` of the top row, the next id does **not** reuse it (sticky high-water).
- After a `ROLLBACK` of an in-txn insert, `sqlite_sequence` **reverts** to the last-committed value; the rolled-back id can be re-used. (The issue text's "sticky even after ROLLBACK" is wrong; this plan matches real SQLite.)

**Scope decision:** The persisted sticky-counter *semantics* are implemented in full (tier 2). A *queryable* `sqlite_sequence` table (visible in `SELECT`/`sqlite_master`/dump) is **out of scope** and deferred to a follow-up issue — the counter lives in `table_meta.next_rowid`, which already persists. `ASC`/`DESC` on a column PK are accepted; `DESC` is treated as a no-op for the rowid alias (a documented minor deviation), except `DESC + AUTOINCREMENT` is rejected to match SQLite.

---

## File Structure

- `lib/sql/lexer.mll` — add `AUTOINCREMENT` keyword.
- `lib/sql/parser.mly` — `%token AUTOINCREMENT`; `Col_primary_key of bool`; optional `ASC`/`DESC`/`AUTOINCREMENT` after column `PRIMARY KEY`; set `column_def.autoincrement`.
- `lib/sql/ast.mli` — add `autoincrement : bool` to `column_def`.
- `lib/sql/sema.ml` — validate placement; add `autoincrement` to `BS_create_table`; fold per-column flag to table-level.
- `lib/sql/plan.ml` / `lib/sql/plan.mli` — add `autoincrement : bool` to `Op_create_table`.
- `lib/sql/planner.ml` — pass `autoincrement` through.
- `lib/sql/exec.ml` — `execute_create_table_op ~autoincrement`; pass to `Cat.create_table`; emit `AUTOINCREMENT` in `ddl_of_table`.
- `lib/catalog/catalog.mli` / `lib/catalog/catalog.ml` — `table_meta.autoincrement`; `create_table ~autoincrement`; `put_table_rows ~autoincrement`; persist in `encode_table_value`/`decode_table_value`; AUTOINCREMENT branch in `recompute_rowid_counters_after_rollback`.
- Tests: `test/test_parser.ml`, `test/test_sema.ml`, `test/test_exec.ml` (or `test/test_db.ml` / `test/test_txn.ml`), `test/test_planner.ml` (compile-fix for new column_def field).

**Build/test commands** (dune runs inside podman — never call `dune` on the host). Use the project's container wrapper; from the worktree root the canonical forms are:
- Build: `scripts/in-container dune build 2>&1 | tail -40` (or the project's existing container-exec pattern — confirm the exact wrapper in Task 0).
- Test a suite: `scripts/in-container dune exec test/test_parser.exe 2>&1 | tail -40`.
- Format check: `ocamlformat --check <file>` (the `@fmt` git errors mask results in a worktree — see project memory `feedback_worktree_build_perms`).

---

## Task 0: Confirm the container build/test harness in the worktree

**Files:** none (environment only)

- [ ] **Step 1:** Identify the exact in-container build command used by this repo. Run:

```bash
ls scripts/ ; sed -n '1,40p' scripts/check-fmt.sh 2>/dev/null ; grep -rn "podman\|dune build\|in-container\|sqlocaml-dev" scripts/ Makefile dune-project 2>/dev/null | head
```

Expected: find the wrapper that runs `dune` inside the `sqlocaml-dev` podman image. Record the exact incantation; substitute it for `scripts/in-container dune ...` everywhere below.

- [ ] **Step 2:** Establish a green baseline. Build the whole project and run the parser + exec + catalog test suites unchanged.

Run (container form from Step 1):
```bash
<container> dune build 2>&1 | tail -20
```
Expected: builds clean (main is known-green).

- [ ] **Step 3:** Ensure worktree perms for the container (project memory `feedback_worktree_build_perms`): `chmod 777 .` at the worktree root if the container user needs write access to `_build`.

---

## Task 1: Lexer + parser accept `AUTOINCREMENT` / `ASC` / `DESC`; AST carries the bit

**Files:**
- Modify: `lib/sql/lexer.mll` (keyword table, ~line 102 near `ASC`/`DESC`)
- Modify: `lib/sql/parser.mly` (`%token` list ~line 55; `col_constraint` type ~line 6; `column_def` action ~line 563; `column_constraint` rule ~line 585)
- Modify: `lib/sql/ast.mli` (`column_def` record ~line 454)
- Test: `test/test_parser.ml`

- [ ] **Step 1: Write failing parser tests.** Add to `test/test_parser.ml` (place beside existing `CREATE TABLE` parse tests; adapt the assertion helper to the file's existing style — most tests parse a string and pattern-match the resulting `Ast.stmt`).

```ocaml
let test_autoincrement_parsed () =
  match Sqlocaml_sql.Parser_driver.parse_stmt
          "CREATE TABLE t (a INTEGER PRIMARY KEY AUTOINCREMENT)" with
  | Ast.S_create_table { columns = [ c ]; _ } ->
    Alcotest.(check bool) "primary_key" true c.Ast.primary_key;
    Alcotest.(check bool) "autoincrement" true c.Ast.autoincrement
  | _ -> Alcotest.fail "expected S_create_table with one column"

let test_pk_asc_accepted () =
  match Sqlocaml_sql.Parser_driver.parse_stmt
          "CREATE TABLE t (a INTEGER PRIMARY KEY ASC AUTOINCREMENT)" with
  | Ast.S_create_table { columns = [ c ]; _ } ->
    Alcotest.(check bool) "autoincrement" true c.Ast.autoincrement
  | _ -> Alcotest.fail "expected S_create_table"

let test_pk_desc_accepted_no_autoinc () =
  match Sqlocaml_sql.Parser_driver.parse_stmt
          "CREATE TABLE t (a INTEGER PRIMARY KEY DESC)" with
  | Ast.S_create_table { columns = [ c ]; _ } ->
    Alcotest.(check bool) "primary_key" true c.Ast.primary_key;
    Alcotest.(check bool) "autoincrement" false c.Ast.autoincrement
  | _ -> Alcotest.fail "expected S_create_table"

let test_desc_autoinc_rejected () =
  Alcotest.check_raises "DESC+AUTOINCREMENT rejected"
    (Failure "AUTOINCREMENT is only allowed on an INTEGER PRIMARY KEY")
    (fun () -> ignore (Sqlocaml_sql.Parser_driver.parse_stmt
      "CREATE TABLE t (a INTEGER PRIMARY KEY DESC AUTOINCREMENT)"))
```

> Confirm the actual parse entry point name (`Parser_driver.parse_stmt` vs the helper the existing tests use) when wiring these in; reuse whatever the file already calls. Register the four cases in the suite's test list.

- [ ] **Step 2: Run; verify failure.**

Run: `<container> dune exec test/test_parser.exe 2>&1 | tail -30`
Expected: compile error — `Ast.column_def` has no field `autoincrement` (and AUTOINCREMENT is an unknown keyword/token).

- [ ] **Step 3: Add the AST field.** In `lib/sql/ast.mli`, `column_def` record, add the field (keep alphabetic/positional grouping near `primary_key`):

```ocaml
and column_def =
  { name : string
  ; ty : ty
  ; not_null : bool
  ; primary_key : bool
  ; autoincrement : bool
    (** [AUTOINCREMENT] on a column [PRIMARY KEY] (#299).  Only ever [true]
        for a single-column ascending INTEGER PRIMARY KEY on a rowid table;
        validated in {!Sqlocaml_sql.Sema}. *)
  ; default : literal option
  ; check : expr option
  ; fk_ref : (string * string * fk_action * fk_action * bool) option
  ; generated_as : (expr * [ `Stored | `Virtual ]) option
  }
```

- [ ] **Step 4: Lexer keyword.** In `lib/sql/lexer.mll`, beside `ASC`/`DESC` (~line 102):

```ocaml
      | "AUTOINCREMENT" -> AUTOINCREMENT
```

- [ ] **Step 5: Parser token + constraint variant + productions.**

In `lib/sql/parser.mly`:

(a) Add the token to the `%token` declarations (group with `PRIMARY KEY`):
```ocaml
%token AUTOINCREMENT
```

(b) Change the header `col_constraint` variant (~line 6) so `Col_primary_key` carries the autoincrement bool:
```ocaml
    | Col_primary_key of bool   (* bool = AUTOINCREMENT present *)
```

(c) Update the `column_def` action (~lines 564-565 and the record at 577) — replace the `not_null`/`primary_key` derivations and add `autoincrement`:
```ocaml
  | name = any_ident ty = col_ty cs = column_constraint*
    { let not_null    = List.mem Col_not_null cs in
      let primary_key =
        List.exists (function Col_primary_key _ -> true | _ -> false) cs in
      let autoincrement =
        List.exists (function Col_primary_key true -> true | _ -> false) cs in
      let default     = List.fold_left (fun acc c ->
          match c with Col_default l -> Some l | _ -> acc) None cs in
      let check       = List.fold_left (fun acc c ->
          match c with Col_check e -> Some e | _ -> acc) None cs in
      let fk_ref      = List.fold_left (fun acc c ->
          match c with
          | Col_fk_ref (t, col_opt, od, ou, def) ->
            Some (t, Option.value ~default:"" col_opt, od, ou, def)
          | _ -> acc) None cs in
      let generated_as = List.fold_left (fun acc c ->
          match c with Col_generated (e, s) -> Some (e, s) | _ -> acc) None cs in
      { name; ty; not_null; primary_key; autoincrement;
        default; check; fk_ref; generated_as } }
```

(d) Replace the `PRIMARY KEY` arm of `column_constraint` (~line 587) and add the two helper rules:
```ocaml
column_constraint:
  | NOT NULL              { Col_not_null }
  | PRIMARY KEY ord = pk_order_opt ai = autoincrement_opt
      { (match ord, ai with
         | `Desc, true ->
           failwith "AUTOINCREMENT is only allowed on an INTEGER PRIMARY KEY"
         | _ -> ());
        Col_primary_key ai }
  | DEFAULT l = def_value { Col_default l }
  (* ... unchanged remaining arms ... *)

pk_order_opt:
  | ASC  { `Asc }
  | DESC { `Desc }
  |      { `None }

autoincrement_opt:
  | AUTOINCREMENT { true }
  |               { false }
```

> Note: the table-level `PRIMARY KEY (cols)` production (`table_item`, ~line 516) is left untouched, so composite-PK `AUTOINCREMENT` is a syntax error — matching SQLite.

- [ ] **Step 6: Fix the existing column_def construction sites** so the project compiles. Add `autoincrement = false;` to each `column_def` literal in `test/test_sema.ml` and `test/test_planner.ml` (the ~10 sites that set `fk_ref =`). Search:

```bash
grep -rn "fk_ref =" test/test_sema.ml test/test_planner.ml
```
For each, insert `; autoincrement = false` (placed consistently after `primary_key`).

- [ ] **Step 7: Run; verify parser tests pass.**

Run: `<container> dune exec test/test_parser.exe 2>&1 | tail -30`
Expected: the four new tests PASS; no regressions.

- [ ] **Step 8: Format + commit.**

```bash
ocamlformat --check lib/sql/parser.mly lib/sql/lexer.mll lib/sql/ast.mli test/test_parser.ml || \
  for f in lib/sql/ast.mli test/test_parser.ml; do ocamlformat "$f" > "$f.tmp" && mv "$f.tmp" "$f"; done
git add lib/sql/lexer.mll lib/sql/parser.mly lib/sql/ast.mli test/test_parser.ml test/test_sema.ml test/test_planner.ml
git commit -m "feat(#299): parse AUTOINCREMENT/ASC/DESC on column PRIMARY KEY

Col_primary_key now carries an autoincrement bool; AST column_def gains an
autoincrement field. DESC+AUTOINCREMENT rejected at parse to match SQLite.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: Validate placement in sema; thread the flag to the catalog and persist it

**Files:**
- Modify: `lib/sql/sema.ml` (`BS_create_table` record ~line 141; `bind_create` ~line 1279; reuse `validate_without_rowid` pattern ~line 1252)
- Modify: `lib/sql/plan.ml` + `lib/sql/plan.mli` (`Op_create_table` ~line 73 / 77)
- Modify: `lib/sql/planner.ml` (~line 838)
- Modify: `lib/sql/exec.ml` (`execute_create_table_op` ~line 5517; call site ~line 6197)
- Modify: `lib/catalog/catalog.mli` (`table_meta` ~line 59; `create_table` ~line 143)
- Modify: `lib/catalog/catalog.ml` (`table_meta` ~line 81; `encode_table_value` ~line 488; `decode_table_value` ~line 498; `put_table_rows` ~line 1626; `create_table` ~line 1652; the `decode_table_value` consumers ~line 987)
- Test: `test/test_exec.ml` (or `test/test_db.ml` — wherever end-to-end `CREATE TABLE` + reopen tests live)

- [ ] **Step 1: Write a failing end-to-end test.** Add to the suite that exercises a real `Db.t` (model on existing `CREATE TABLE` tests in that file). It must (a) create an AUTOINCREMENT table, (b) insert + verify ids, (c) reopen the db and confirm the flag persisted by checking dump (Task 3 wires dump, so here assert via a catalog accessor or via successful sticky behavior). Minimal version asserting acceptance + counter:

```ocaml
let test_autoincrement_create_and_insert () =
  Lwt_main.run begin
    let* db = open_fresh_db () in            (* use the file's existing helper *)
    let* () = exec_ok db "CREATE TABLE t (a INTEGER PRIMARY KEY AUTOINCREMENT, b TEXT)" in
    let* () = exec_ok db "INSERT INTO t(b) VALUES ('x')" in
    let* () = exec_ok db "INSERT INTO t(b) VALUES ('y')" in
    let* rows = query db "SELECT a FROM t ORDER BY a" in
    Alcotest.(check (list int64)) "ids" [ 1L; 2L ] (ints_of_rows rows);
    Lwt.return_unit
  end
```

> Use the file's actual helpers (`open_fresh_db`, `exec_ok`, `query`, row extraction). If none exist, copy the harness from the nearest existing test in the same file.

- [ ] **Step 2: Run; verify failure.**

Run: `<container> dune exec test/test_exec.exe 2>&1 | tail -30`
Expected: failure — `Cat.create_table` has no `~autoincrement`; or the statement is accepted but the new field is unthreaded (compile error first).

- [ ] **Step 3: `table_meta` field + persistence (catalog.ml + .mli).**

In `lib/catalog/catalog.mli` `table_meta` (~line 59) and `lib/catalog/catalog.ml` `table_meta` (~line 81), add after `without_rowid`:
```ocaml
  ; autoincrement : bool
    (** #299: [INTEGER PRIMARY KEY AUTOINCREMENT].  When [true] the rowid
        counter is a sticky high-water mark: on ROLLBACK it reverts to the
        last-committed value (read back from [_sys_tables]) instead of being
        recomputed as [max(rowid)+1] from the data tree. *)
```

In `encode_table_value` (~line 488) append a trailing flag after `without_rowid`:
```ocaml
  Varint.encode_uint64 buf (if m.without_rowid then 1L else 0L);
  (* #299: trailing autoincrement flag.  Older encodings lack it; the decoder
     treats absence as [false]. *)
  Varint.encode_uint64 buf (if m.autoincrement then 1L else 0L);
  Buffer.to_bytes buf
```

In `decode_table_value` (~line 498) make it return the flag and parse the new trailing byte. Change the return tuple to a 4-tuple `(tid, next, without_rowid, autoincrement)`:
```ocaml
let decode_table_value bytes =
  let tid, off = Varint.decode_uint64 bytes 0 in
  let next, off' = Varint.decode_int64 bytes off in
  let without_rowid, off'' =
    if off' >= Bytes.length bytes then false, off'
    else (let v, o = Varint.decode_uint64 bytes off' in Int64.to_int v <> 0, o)
  in
  let autoincrement =
    if off'' >= Bytes.length bytes then false
    else (let v, _ = Varint.decode_uint64 bytes off'' in Int64.to_int v <> 0)
  in
  Int64.to_int tid, next, without_rowid, autoincrement
;;
```

- [ ] **Step 4: Fix `decode_table_value` consumers.** The open-time loader (~line 987) destructures the old 3-tuple. Update it:
```bash
grep -n "decode_table_value" lib/catalog/catalog.ml
```
At ~line 987 change `let tid, next_rowid, without_rowid = decode_table_value v in` to add `, autoincrement` and set `; autoincrement` in the `table_meta` it builds (~line 995). Any other consumer (mirror decode etc.) gets `; autoincrement = false` if it builds from a source that doesn't carry it.

- [ ] **Step 5: `put_table_rows` + `create_table` take `~autoincrement`.**

`put_table_rows` (~line 1626):
```ocaml
let put_table_rows tx ~name ~columns ~without_rowid ~autoincrement ~tid =
  let m =
    { name; tree_id = tid; columns; next_rowid = empty_next_rowid
    ; fk_constraints = []; without_rowid; autoincrement }
  in
  ...
```
`create_table` (~line 1652) — add `~autoincrement` param and forward to both `put_table_rows` calls:
```ocaml
let create_table ?txn t ~name ~columns ~without_rowid ~autoincrement =
  ...
    let%lwt m = put_table_rows tx ~name ~columns ~without_rowid ~autoincrement ~tid in
  ...
    let%lwt m = put_table_rows tx ~name ~columns ~without_rowid ~autoincrement ~tid in
```
And in `catalog.mli` (~line 143):
```ocaml
val create_table
  :  ?txn:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  -> t
  -> name:string
  -> columns:Sqlocaml_encoding.Row.column list
  -> without_rowid:bool
  -> autoincrement:bool
  -> Sqlocaml_store.Store.tree_id Lwt.t
```

> Check for any other `table_meta` literal in catalog.ml (there are ~11 sites, e.g. the mirror decoder ~line 1319 and `S_create_table` empty meta) and add `; autoincrement = false` to those that build non-AUTOINCREMENT metas. Build to find them all.

- [ ] **Step 6: Sema — validate + carry the flag.**

In `lib/sql/sema.ml`, add `autoincrement : bool` to `BS_create_table` (~line 141, after `without_rowid`).

In `bind_create` (~line 1279), after `row_cols` are built and `validate_without_rowid` runs, compute and validate the flag. Place a helper near `validate_without_rowid`:
```ocaml
(* #299: AUTOINCREMENT is legal only on a single-column ascending INTEGER
   PRIMARY KEY of a rowid table.  [defs] are the AST column_defs (carrying the
   parsed [autoincrement] bit); [row_cols] are the resolved Row.columns. *)
let validate_autoincrement ~without_rowid (defs : Ast.column_def list)
      (row_cols : Row.column list) : bool =
  let any_ai = List.exists (fun (c : Ast.column_def) -> c.autoincrement) defs in
  if not any_ai then false
  else begin
    if without_rowid then
      failwith "AUTOINCREMENT not allowed on WITHOUT ROWID tables";
    (match Cat.compute_rowid_alias_col row_cols ~without_rowid with
     | Some i ->
       let ai_def = List.nth defs i in
       if not ai_def.Ast.autoincrement then
         failwith "AUTOINCREMENT is only allowed on an INTEGER PRIMARY KEY"
     | None ->
       failwith "AUTOINCREMENT is only allowed on an INTEGER PRIMARY KEY");
    true
  end
```
Wire it into the `BS_create_table` construction (~line 1286): compute `let autoincrement = validate_autoincrement ~without_rowid columns row_cols in` (use whatever the local AST column list + resolved row column list are named) and add `; autoincrement` to the record.

> `compute_rowid_alias_col` already requires a *single* INTEGER PK and returns `None` for composite/non-integer — so the alias-index check rejects every invalid placement that survived parsing. Confirm the `defs`↔`row_cols` index alignment (both are in column order; generated/virtual columns do not reorder them — verify in `column_of_def` usage).

- [ ] **Step 7: Plan + planner + exec wiring.**

`lib/sql/plan.ml` and `plan.mli` `Op_create_table` (~line 73/77): add `; autoincrement : bool`.

`lib/sql/planner.ml` (~line 837): destructure `autoincrement` from `BS_create_table` and pass it into `Plan.Op_create_table { ...; autoincrement }`.

`lib/sql/exec.ml`:
- `execute_create_table_op` (~line 5517): add `~autoincrement` param; pass to `Cat.create_table ~txn:tx cat ~name ~columns ~without_rowid ~autoincrement` (~line 5540).
- call site (~line 6197): destructure `autoincrement` from the `Op_create_table` record and forward `~autoincrement`.

- [ ] **Step 8: Run; verify the e2e test passes.**

Run: `<container> dune build 2>&1 | tail -20 && <container> dune exec test/test_exec.exe 2>&1 | tail -30`
Expected: build clean; `test_autoincrement_create_and_insert` PASS.

- [ ] **Step 9: Format + commit.**

```bash
git add lib/sql/sema.ml lib/sql/plan.ml lib/sql/plan.mli lib/sql/planner.ml lib/sql/exec.ml lib/catalog/catalog.ml lib/catalog/catalog.mli test/test_exec.ml
git commit -m "feat(#299): validate + persist AUTOINCREMENT through to table_meta

Thread a table-level autoincrement flag BS_create_table -> Op_create_table ->
Cat.create_table -> table_meta, persisted as a trailing field of the table
value encoding. Reject the keyword where SQLite does (non-INTEGER PK, composite,
WITHOUT ROWID).

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: Dump round-trip — `ddl_of_table` emits `AUTOINCREMENT`

**Files:**
- Modify: `lib/sql/exec.ml` `ddl_of_table` (~line 179)
- Test: `test/test_db.ml` (or wherever `Db.dump` round-trip tests live — see #264 tests)

- [ ] **Step 1: Write a failing dump round-trip test.**
```ocaml
let test_autoincrement_dump_roundtrip () =
  Lwt_main.run begin
    let* db = open_fresh_db () in
    let* () = exec_ok db "CREATE TABLE t (a INTEGER PRIMARY KEY AUTOINCREMENT, b TEXT)" in
    let* dump = Db.dump db in
    Alcotest.(check bool) "dump contains AUTOINCREMENT"
      true
      (contains_substring dump "PRIMARY KEY AUTOINCREMENT");
    Lwt.return_unit
  end
```
> Use the dump entry point used by #264 tests (`Db.dump`). Reuse a substring helper or inline `Astring`/`String.split_on_char` check.

- [ ] **Step 2: Run; verify failure.**

Run: `<container> dune exec test/test_db.exe 2>&1 | tail -20`
Expected: FAIL — dump emits `PRIMARY KEY` without `AUTOINCREMENT`.

- [ ] **Step 3: Emit the keyword.** In `ddl_of_table` (~line 188), the per-column closure has the column index available via `List.mapi` — convert the `List.map` to `List.mapi` and compute the rowid-alias index once:
```ocaml
let ddl_of_table (meta : Cat.table_meta) =
  let alias_idx =
    if meta.Cat.autoincrement
    then Cat.compute_rowid_alias_col meta.Cat.columns ~without_rowid:meta.Cat.without_rowid
    else None
  in
  let col_parts =
    List.mapi
      (fun i (col : Row.column) ->
         let buf = Buffer.create 64 in
         ... (* unchanged up to the PRIMARY KEY line *)
         if col.Row.primary_key then begin
           Buffer.add_string buf " PRIMARY KEY";
           if Some i = alias_idx then Buffer.add_string buf " AUTOINCREMENT"
         end;
         ... )
      meta.Cat.columns
  in
  ...
```

- [ ] **Step 4: Run; verify pass.**

Run: `<container> dune exec test/test_db.exe 2>&1 | tail -20`
Expected: PASS.

- [ ] **Step 5: Verify the dump *re-parses* (full round-trip).** Extend the test (or add a second) to feed `dump` back into a fresh db and confirm `CREATE TABLE` succeeds and a re-dump is identical:
```ocaml
  let* db2 = open_fresh_db () in
  let* () = exec_each db2 (split_statements dump) in
  let* dump2 = Db.dump db2 in
  Alcotest.(check string) "stable round-trip" dump dump2;
```

- [ ] **Step 6: Format + commit.**
```bash
git add lib/sql/exec.ml test/test_db.ml
git commit -m "feat(#299): emit AUTOINCREMENT in Db.dump for round-trip fidelity

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 4: Rejection tests for invalid placements (tier-1 guardrails)

**Files:**
- Test: `test/test_sema.ml` (or `test/test_exec.ml` for end-to-end statement execution)

- [ ] **Step 1: Write rejection tests** (expected errors match the SQLite oracle messages exactly):
```ocaml
let reject sql msg =
  Alcotest.check_raises ("reject: " ^ sql) (Failure msg)
    (fun () -> ignore (run_ddl sql))  (* helper that parses+binds the stmt *)

let test_autoincrement_rejections () =
  reject "CREATE TABLE t (a TEXT PRIMARY KEY AUTOINCREMENT)"
    "AUTOINCREMENT is only allowed on an INTEGER PRIMARY KEY";
  reject "CREATE TABLE t (a INTEGER PRIMARY KEY AUTOINCREMENT) WITHOUT ROWID"
    "AUTOINCREMENT not allowed on WITHOUT ROWID tables";
  reject "CREATE TABLE t (a INTEGER PRIMARY KEY DESC AUTOINCREMENT)"
    "AUTOINCREMENT is only allowed on an INTEGER PRIMARY KEY"
```
> The DESC case is caught in the parser (Task 1); the other two in sema (Task 2). If `run_ddl` only parses, split the DESC case out (it already has a parser test) and keep the two sema cases here. Confirm exact exception types — the codebase uses `failwith`/`Failure`; if sema wraps errors in a custom exception, match that instead.

- [ ] **Step 2: Run; verify pass** (no implementation needed — validation already exists from Tasks 1-2).

Run: `<container> dune exec test/test_sema.exe 2>&1 | tail -20`
Expected: PASS. If any case is *not* rejected, fix the validation in Task 2's `validate_autoincrement` before proceeding.

- [ ] **Step 3: Commit.**
```bash
git add test/test_sema.ml
git commit -m "test(#299): AUTOINCREMENT placement rejections match SQLite messages

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 5 (Tier 2): Sticky high-water semantics on ROLLBACK

**Files:**
- Modify: `lib/catalog/catalog.ml` `recompute_rowid_counters_after_rollback` (~line 1580); add a `read_committed_next_rowid` helper near `recover_next_rowid` (~line 1365); update the comment at ~line 1577.
- Test: `test/test_txn.ml`

**Behavioral contract (verified against SQLite 3.45):**
- Committed `DELETE` of the top row → next id does **not** reuse it (already true: commit never lowers `next_rowid`). No code change needed; add a regression test.
- `ROLLBACK` of an in-txn insert → counter reverts to the **last-committed** value. For an AUTOINCREMENT table this committed value is the sticky high-water (it survived earlier committed deletes), so we must restore it from `_sys_tables` rather than recompute `max(rowid)+1` from data.
- Distinguishing case (AUTOINCREMENT vs plain): insert 1,2 (commit) → committed `next=3`; delete 2 (commit) → `next` stays 3; `BEGIN; insert→3; ROLLBACK` → plain recomputes to `max(data={1})+1 = 2` (reuse 2), AUTOINCREMENT restores committed `3` (next id is 3).

- [ ] **Step 1: Write failing tier-2 tests** in `test/test_txn.ml`. Pin expected values with the host oracle first:
```bash
cd /tmp && rm -f o.db && sqlite3 o.db <<'EOF'
CREATE TABLE t(a INTEGER PRIMARY KEY AUTOINCREMENT, b TEXT);
INSERT INTO t(b) VALUES('x'); INSERT INTO t(b) VALUES('y');   -- 1,2
DELETE FROM t WHERE a=2;                                       -- committed delete
BEGIN; INSERT INTO t(b) VALUES('z'); ROLLBACK;                 -- a=3 rolled back
INSERT INTO t(b) VALUES('w');                                  -- expected a=?
SELECT group_concat(a) FROM t;                                 -- oracle answer
EOF
```
Expected oracle: rows `1,3` (the post-rollback insert gets `a=3`, sticky past the committed delete of 2). Encode that:
```ocaml
let test_autoincrement_sticky_rollback () =
  Lwt_main.run begin
    let* db = open_fresh_db () in
    let* () = exec_ok db "CREATE TABLE t (a INTEGER PRIMARY KEY AUTOINCREMENT, b TEXT)" in
    let* () = exec_ok db "INSERT INTO t(b) VALUES ('x')" in   (* 1 *)
    let* () = exec_ok db "INSERT INTO t(b) VALUES ('y')" in   (* 2 *)
    let* () = exec_ok db "DELETE FROM t WHERE a = 2" in       (* committed *)
    let* () = exec_ok db "BEGIN" in
    let* () = exec_ok db "INSERT INTO t(b) VALUES ('z')" in   (* 3, to be rolled back *)
    let* () = exec_ok db "ROLLBACK" in
    let* () = exec_ok db "INSERT INTO t(b) VALUES ('w')" in   (* must be 3, not 2 *)
    let* rows = query db "SELECT a FROM t ORDER BY a" in
    Alcotest.(check (list int64)) "sticky ids" [ 1L; 3L ] (ints_of_rows rows);
    Lwt.return_unit
  end

(* Contrast: plain rowid reuses the rolled-back id (#293 behavior). *)
let test_plain_rowid_reuses_after_rollback () =
  Lwt_main.run begin
    let* db = open_fresh_db () in
    let* () = exec_ok db "CREATE TABLE t (a INTEGER PRIMARY KEY, b TEXT)" in
    let* () = exec_ok db "INSERT INTO t(b) VALUES ('x')" in   (* 1 *)
    let* () = exec_ok db "BEGIN" in
    let* () = exec_ok db "INSERT INTO t(b) VALUES ('y')" in   (* 2 rolled back *)
    let* () = exec_ok db "ROLLBACK" in
    let* () = exec_ok db "INSERT INTO t(b) VALUES ('z')" in   (* reuses 2 *)
    let* rows = query db "SELECT a FROM t ORDER BY a" in
    Alcotest.(check (list int64)) "reuse" [ 1L; 2L ] (ints_of_rows rows);
    Lwt.return_unit
  end
```
> Verify the plain-rowid contrast oracle too: SQLite gives `1,2` (reuse). The plain test should already pass on `main`; include it to lock the contrast and guard against regressions.

- [ ] **Step 2: Run; verify the sticky test fails** (and the plain test passes).

Run: `<container> dune exec test/test_txn.exe 2>&1 | tail -30`
Expected: `test_autoincrement_sticky_rollback` FAILS (current `recompute` lowers the counter to `max(data)+1 = 2`, so the final insert wrongly gets `2`, yielding rows `1,2`). `test_plain_rowid_reuses_after_rollback` PASSES.

- [ ] **Step 3: Add the committed-counter reader** near `recover_next_rowid` (~line 1389):
```ocaml
(* #299: read a table's LAST-COMMITTED [next_rowid] straight from its
   [_sys_tables] row.  Used by the AUTOINCREMENT rollback path: after
   [S.rollback] the store row has reverted to the committed value (the sticky
   high-water), so restoring the cached counter from it — rather than
   recomputing max(rowid)+1 from data — preserves stickiness across committed
   DELETEs while still reverting a rolled-back allocation to the committed mark.
   Returns [empty_next_rowid] if the row is absent (table created in the
   rolled-back txn — caller skips uncached names anyway). *)
let read_committed_next_rowid store ~name : int64 Lwt.t =
  S.with_ro store
  @@ fun tx ->
  let%lwt v = S.get tx sys_tables_tid (Bytes.of_string name) in
  match v with
  | None -> Lwt.return empty_next_rowid
  | Some bytes ->
    let _tid, next, _wr, _ai = decode_table_value bytes in
    Lwt.return next
;;
```
> Confirm `S.get`'s exact signature/return (`bytes option Lwt.t` vs `string option`) from the earlier usages at catalog.ml:41/902 and adapt.

- [ ] **Step 4: Branch the rollback recompute** (~line 1580). For AUTOINCREMENT tables, restore the committed counter instead of recomputing:
```ocaml
let recompute_rowid_counters_after_rollback t =
  let names = Schema_cache.take_rowid_bumped t.sc in
  Lwt_list.iter_s
    (fun name ->
       match Schema_cache.find_table t.sc name with
       | None -> Lwt.return_unit
       | Some m when m.without_rowid -> Lwt.return_unit
       | Some m when m.autoincrement ->
         (* #299: sticky high-water — revert to the committed counter, not
            max(rowid)+1, so a committed DELETE of the top row is not undone. *)
         let%lwt committed = read_committed_next_rowid t.store ~name in
         Schema_cache.set_rowid_durable t.sc ~name { m with next_rowid = committed };
         Lwt.return_unit
       | Some m ->
         let%lwt recovered = recover_next_rowid t.store m in
         Schema_cache.set_rowid_durable t.sc ~name recovered;
         Lwt.return_unit)
    names
;;
```
> `set_rowid_durable t ~name meta` replaces the whole cached meta (it is `Hashtbl.replace`); pass the meta with the corrected `next_rowid`. Mirror exactly how the existing plain arm calls it — check whether `recover_next_rowid` returns a full meta (it returns `{ m with next_rowid }`) and `set_rowid_durable` is given that meta; match that shape for the AUTOINCREMENT arm.

- [ ] **Step 5: Update the stale comment** at ~line 1577 (it currently claims AUTOINCREMENT is unimplemented). Replace with a note that AUTOINCREMENT tables take the committed-counter branch above.

- [ ] **Step 6: Run; verify both tests pass + no regressions.**

Run: `<container> dune exec test/test_txn.exe 2>&1 | tail -30`
Expected: both new tests PASS; existing #293/#301/#303 rowid tests still PASS.

- [ ] **Step 7: Add a SAVEPOINT regression test** (the #303 snapshot/restore path already restores the cached counter to the savepoint value, which is correct for AUTOINCREMENT — this test confirms it). Pin with the oracle first:
```bash
cd /tmp && rm -f s.db && sqlite3 s.db <<'EOF'
CREATE TABLE t(a INTEGER PRIMARY KEY AUTOINCREMENT, b TEXT);
INSERT INTO t(b) VALUES('x');                 -- 1
SAVEPOINT sp; INSERT INTO t(b) VALUES('y');   -- 2
ROLLBACK TO sp; INSERT INTO t(b) VALUES('z'); -- expected a=?
SELECT group_concat(a) FROM t;
EOF
```
Encode the oracle result as the expected list. Run the suite; if it fails, the savepoint path needs the same committed/snapshot treatment — investigate before claiming done.

- [ ] **Step 8: Format + commit.**
```bash
git add lib/catalog/catalog.ml test/test_txn.ml
git commit -m "feat(#299): AUTOINCREMENT sticky high-water on ROLLBACK

On rollback, AUTOINCREMENT tables restore the committed next_rowid from
_sys_tables instead of recomputing max(rowid)+1 from data, so a committed
DELETE of the top row is never reused — while a rolled-back allocation still
reverts to the committed mark, matching SQLite 3.45.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 6: Fuzz/property test + documentation + follow-up issue

**Files:**
- Test: add a QCheck property to the relevant test module (project convention: QCheck on every module — see project memory `feedback_testing_approach`).
- Modify: the `recompute` comment (done in Task 5); add a short note in any AUTOINCREMENT-relevant doc/CHANGELOG if the repo keeps one.

- [ ] **Step 1: QCheck property — counter monotonicity under interleaved insert/delete/commit.** Generate a random sequence of `INSERT`/`DELETE`/`COMMIT` (no rollback) ops on an AUTOINCREMENT table and assert the allocated id is strictly greater than every previously allocated id (never reused). Model on an existing stateful QCheck test in the suite.

- [ ] **Step 2: Run the property** (`<container> dune exec test/<module>.exe`) — Expected: PASS.

- [ ] **Step 3: Run the full test suite** to confirm no regressions across all modules.

Run: `<container> dune build @runtest 2>&1 | tail -40`
Expected: all suites green.

- [ ] **Step 4: File a follow-up issue** for the deferred queryable `sqlite_sequence` table (Forgejo, repo `tej/sqlite_ocaml_port`):
```bash
~/.local/bin/forgejo issue create tej/sqlite_ocaml_port \
  --title "Queryable sqlite_sequence table for AUTOINCREMENT (follow-up to #299)" \
  --body "#299 implemented AUTOINCREMENT semantics via the persisted table_meta.next_rowid high-water. SQLite also exposes a queryable sqlite_sequence(name,seq) table (visible in SELECT, sqlite_master, and dumps). That surface was deferred from #299. Scope: materialize sqlite_sequence as a readable system table; reflect the per-table counter; decide write visibility (SQLite allows UPDATE of seq). Also: ASC/DESC on a column PK are currently accepted but treated as no-ops for the rowid alias (SQLite treats INTEGER PRIMARY KEY DESC as a non-alias) — decide whether to honor or keep documenting the deviation. Refs #299."
```

- [ ] **Step 5: Commit any test/doc additions.**
```bash
git add -A
git commit -m "test(#299): QCheck AUTOINCREMENT monotonicity; doc deviations

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Self-Review checklist (run before opening the PR)

- [ ] Spec coverage: parse-accept (T1) ✓, carry flag + persist (T2) ✓, dump round-trip (T3) ✓, reject-where-SQLite-does (T4) ✓, sticky high-water semantics (T5) ✓, follow-up for deferred sqlite_sequence visibility (T6) ✓.
- [ ] No placeholders: every code step shows real code.
- [ ] Type consistency: `autoincrement : bool` field name is identical across `Ast.column_def`, `Sema.BS_create_table`, `Plan.Op_create_table`, `Cat.table_meta`, and the `~autoincrement` labels on `create_table`/`put_table_rows`/`execute_create_table_op`. `decode_table_value` returns a 4-tuple everywhere it is consumed.
- [ ] `recompute_rowid_counters_after_rollback` AUTOINCREMENT arm uses `read_committed_next_rowid` (store), the plain arm uses `recover_next_rowid` (data).
- [ ] Run the full suite + `ocamlformat --check` on every touched file (worktree `@fmt` gate caveat) before requesting review.
