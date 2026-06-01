# Triggers (BEFORE/AFTER INSERT/UPDATE/DELETE) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement SQL trigger support (CREATE TRIGGER / DROP TRIGGER) covering BEFORE and AFTER events on INSERT, UPDATE, and DELETE, with NEW/OLD pseudo-table references and optional WHEN clause.

**Architecture:** Triggers follow the view pattern — stored as raw SQL text in a new system B-tree (`sys_triggers_tid = 6`), loaded into `db.t.triggers` at open time, and fired from db.ml by intercepting Op_insert/Op_update/Op_delete before forwarding to exec.ml. NEW/OLD column references in trigger bodies are resolved by AST-level literal substitution before compilation. Per-row BEFORE and AFTER hooks are threaded from db.ml into exec.ml's DML functions as optional labeled arguments.

**Tech Stack:** OCaml 5.x, dune, lwt, menhir; all dune commands run inside `podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune ...`

---

## File Map

| File | Change |
|------|--------|
| `lib/sql/ast.ml` | Add `trigger_timing`, `trigger_event`, `S_create_trigger`, `S_drop_trigger` |
| `lib/sql/lexer.mll` | Add TRIGGER, BEFORE, AFTER to ident match block |
| `lib/sql/parser.mly` | Add %token, grammar rules for CREATE/DROP TRIGGER |
| `lib/sql/sema.ml` | Add `BS_create_trigger`, `BS_drop_trigger` to bound_stmt |
| `lib/sql/sema.mli` | Export new bound_stmt variants |
| `lib/sql/plan.ml` | Add `Op_create_trigger`, `Op_drop_trigger` |
| `lib/sql/planner.ml` | Wire BS→Op for trigger DDL |
| `lib/sql/exec.ml` | Add `?before_hook`/`?after_hook` to DML functions; add exhaustiveness guards for trigger Ops |
| `lib/sql/exec.mli` | Expose new hook parameters |
| `lib/catalog/catalog.ml` | Add `sys_triggers_tid=6`; add `trigger_meta` type; add `triggers` hashtable to `t`; add `load_all_triggers`, `persist_trigger`, `remove_trigger` |
| `lib/catalog/catalog.mli` | Export new types and functions |
| `lib/db/db.ml` | Add `triggers` hashtable to `t`; add `load_triggers_into_hashtbl`; add `subst_new_old`, `make_trigger_hook`, `fire_trigger_stmt`; handle `Op_create_trigger`/`Op_drop_trigger`; add trigger-aware DML path |
| `test/test_e2e.ml` | Add "triggers" suite (8 tests) |
| `test/test_sqlite_compare.ml` | Add `phase22_trigger_cases` suite |

---

## Task 1: AST — trigger type definitions

**Files:**
- Modify: `lib/sql/ast.ml`

- [ ] **Step 1: Add trigger types after the `set_op` type (around line 89)**

In `lib/sql/ast.ml`, add these type definitions after `type set_op = ...`:

```ocaml
type trigger_timing = TT_before | TT_after

type trigger_event  = TE_insert | TE_update | TE_delete
```

- [ ] **Step 2: Add S_create_trigger and S_drop_trigger to the stmt type**

Find the `S_drop_view` variant (around line 291–293 of `lib/sql/ast.ml`) and add after it:

```ocaml
  | S_create_trigger of {
      name    : string;
      timing  : trigger_timing;
      event   : trigger_event;
      table   : string;
      when_   : expr option;       (** WHEN clause; None if absent *)
      body    : stmt list;         (** statements between BEGIN…END *)
    }
  | S_drop_trigger of {
      name : string;
    }
```

- [ ] **Step 3: Build inside podman to verify no compile errors**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune build lib/sql/ast.ml 2>&1
```

Expected: clean build (ast.ml is not a compiled unit on its own; the build succeeds when the full library builds). Run full build:

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -40
```

Expected: errors about non-exhaustive match in sema/planner (we haven't added those cases yet) — that's OK. The important thing is no type errors in ast.ml itself.

- [ ] **Step 4: Commit**

```bash
git add lib/sql/ast.ml
git commit -m "feat(phase22): add trigger AST types (S_create_trigger, S_drop_trigger) [#129]"
```

---

## Task 2: Lexer and Parser — trigger SQL syntax

**Files:**
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`

### Lexer

- [ ] **Step 1: Add TRIGGER, BEFORE, AFTER to the ident match block**

In `lib/sql/lexer.mll`, in the `String.uppercase_ascii id` match block (around line 150–199), add three new cases **before** the final `| _ -> IDENT id` catch-all:

```ocaml
      | "TRIGGER" -> TRIGGER
      | "BEFORE"  -> BEFORE
      | "AFTER"   -> AFTER
```

### Parser

- [ ] **Step 2: Declare the three new tokens**

In `lib/sql/parser.mly`, after the line `%token CONFLICT DO VIEW` (around line 47), add:

```
%token TRIGGER BEFORE AFTER
```

- [ ] **Step 3: Add create_trigger and drop_trigger to the stmt production**

In `lib/sql/parser.mly`, the `stmt:` production lists alternatives. After the `| s = drop_view { s }` line (around line 96), add:

```
  | s = create_trigger  { s }
  | s = drop_trigger    { s }
```

- [ ] **Step 4: Add the grammar rules**

In `lib/sql/parser.mly`, after the `drop_view:` rule (around line 140), add:

```
create_trigger:
  | CREATE TRIGGER name = IDENT
    timing = trigger_timing
    event  = trigger_event
    ON table = IDENT
    when_  = trigger_when
    BEGIN body = trigger_body END
    { Ast.S_create_trigger { name; timing; event; table; when_; body } }

drop_trigger:
  | DROP TRIGGER name = IDENT
    { Ast.S_drop_trigger { name } }

trigger_timing:
  | BEFORE { Ast.TT_before }
  | AFTER  { Ast.TT_after  }

trigger_event:
  | INSERT { Ast.TE_insert }
  | UPDATE { Ast.TE_update }
  | DELETE { Ast.TE_delete }

trigger_when:
  | WHEN e = expr { Some e }
  |               { None   }

trigger_body:
  | s = stmt SEMI             { [s] }
  | s = stmt SEMI rest = trigger_body { s :: rest }
```

Note: `BEGIN` and `END` are already declared tokens (used for transaction control). Menhir's LR(1) parser resolves the ambiguity by context: inside `create_trigger`, `BEGIN` is consumed as the trigger body start, not as `S_begin`.

- [ ] **Step 5: Build and check for parser conflicts**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -60
```

Expected: Some errors about non-exhaustive match in sema (OK); NO shift/reduce conflicts from menhir (if there are conflicts, fix them). If menhir reports `1 shift/reduce conflict` related to `trigger_when`, add an explicit `%prec` or reorder. Typically this resolves without intervention since WHEN and BEGIN are distinct tokens.

- [ ] **Step 6: Smoke-test parsing in utop (inside podman)**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec -- ocaml -I _build/default/lib/sql/.sqlocaml_sql.objs/byte \
  -stdin <<'EOF'
#load "sqlocaml_sql.cma";;
let lexbuf = Lexing.from_string
  "CREATE TRIGGER t_ai AFTER INSERT ON t BEGIN \
   INSERT INTO audit VALUES (1); END";;
let r = Sqlocaml_sql.Parser.stmt_eof Sqlocaml_sql.Lexer.token lexbuf;;
EOF
```

(If utop approach doesn't work, skip this — the e2e tests in Task 7 will cover it.)

- [ ] **Step 7: Commit**

```bash
git add lib/sql/lexer.mll lib/sql/parser.mly
git commit -m "feat(phase22): add TRIGGER/BEFORE/AFTER tokens and CREATE/DROP TRIGGER parser rules [#129]"
```

---

## Task 3: Sema, Plan, and Planner — pipe trigger DDL through the pipeline

**Files:**
- Modify: `lib/sql/sema.ml`, `lib/sql/sema.mli`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`, `lib/sql/planner.mli`

### Sema

- [ ] **Step 1: Add BS_create_trigger and BS_drop_trigger to bound_stmt**

In `lib/sql/sema.ml`, after the `| BS_drop_view of { name: string }` line (around line 188), add:

```ocaml
  | BS_create_trigger of {
      name    : string;
      timing  : Ast.trigger_timing;
      event   : Ast.trigger_event;
      table   : string;   (** target table name — validated to exist *)
      when_   : Ast.expr option;
      body    : Ast.stmt list;
    }
  | BS_drop_trigger of { name : string }
```

- [ ] **Step 2: Add binding cases in bind_internal**

In `lib/sql/sema.ml`, find the `bind_internal` function. It has a large match over `Ast.stmt`. After the case for `S_drop_view` (which returns `Ok (BS_drop_view { name })`), add:

```ocaml
    | Ast.S_create_trigger { name; timing; event; table; when_; body } ->
      (* Validate that the target table exists — triggers on unknown tables should error at creation. *)
      let* _meta = lookup_table cat table in
      Ok (BS_create_trigger { name; timing; event; table; when_; body })
    | Ast.S_drop_trigger { name } ->
      Ok (BS_drop_trigger { name })
```

`lookup_table` is the helper already used for other stmt cases — it returns `Error (Unknown_table t)` if not found.

- [ ] **Step 3: Update sema.mli**

In `lib/sql/sema.mli`, find the `bound_stmt` type declaration and add after `BS_drop_view`:

```ocaml
  | BS_create_trigger of {
      name    : string;
      timing  : Ast.trigger_timing;
      event   : Ast.trigger_event;
      table   : string;
      when_   : Ast.expr option;
      body    : Ast.stmt list;
    }
  | BS_drop_trigger of { name : string }
```

### Plan

- [ ] **Step 4: Add Op_create_trigger and Op_drop_trigger to plan.ml**

In `lib/sql/plan.ml`, after the `| Op_drop_view of { name : string }` line (last line of the `op` type), add:

```ocaml
  | Op_create_trigger of {
      name    : string;
      timing  : Ast.trigger_timing;
      event   : Ast.trigger_event;
      table   : string;
      when_   : Ast.expr option;
      body    : Ast.stmt list;
    }
  | Op_drop_trigger of { name : string }
```

### Planner

- [ ] **Step 5: Wire up in planner.ml**

In `lib/sql/planner.ml`, find the `plan` function's match on `bound_stmt`. After the case for `Sema.BS_drop_view`:

```ocaml
  | Sema.BS_drop_view { name } ->
    Plan.Op_drop_view { name }
```

Add:

```ocaml
  | Sema.BS_create_trigger { name; timing; event; table; when_; body } ->
    Plan.Op_create_trigger { name; timing; event; table; when_; body }
  | Sema.BS_drop_trigger { name } ->
    Plan.Op_drop_trigger { name }
```

- [ ] **Step 6: Build and verify**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -40
```

Expected: warnings about non-exhaustive patterns in exec.ml (not yet handling Op_create_trigger/Op_drop_trigger). No type errors.

- [ ] **Step 7: Commit**

```bash
git add lib/sql/sema.ml lib/sql/sema.mli lib/sql/plan.ml lib/sql/planner.ml lib/sql/planner.mli
git commit -m "feat(phase22): pipe CREATE/DROP TRIGGER through sema→plan→planner [#129]"
```

---

## Task 4: Catalog — trigger persistence and in-memory cache

**Files:**
- Modify: `lib/catalog/catalog.ml`
- Modify: `lib/catalog/catalog.mli`

### catalog.ml

- [ ] **Step 1: Add sys_triggers_tid system tree ID**

In `lib/catalog/catalog.ml`, after the line:

```ocaml
let sys_views_tid   : S.tree_id = 5
```

Add:

```ocaml
let sys_triggers_tid : S.tree_id = 6
```

- [ ] **Step 2: Add trigger_meta type**

In `lib/catalog/catalog.ml`, after the `fts_table_meta` type (around line 49), add:

```ocaml
type trigger_timing = TT_before | TT_after
type trigger_event  = TE_insert | TE_update | TE_delete

type trigger_meta = {
  trig_name    : string;
  trig_table   : string;
  trig_timing  : trigger_timing;
  trig_event   : trigger_event;
  trig_when    : string option;    (** WHEN expression as SQL text, or None *)
  trig_body    : string list;      (** body statements as SQL text (one per stmt) *)
}
```

Note: We store the WHEN and body as raw SQL text (not parsed AST) so the catalog doesn't depend on the SQL layer. Parsing happens in db.ml at fire time.

- [ ] **Step 3: Add triggers field to type t**

In `lib/catalog/catalog.ml`, find the `type t = { ... }` record (around line 51). Add a `triggers` field:

```ocaml
type t = {
  store   : S.t;
  cache   : (string, table_meta) Hashtbl.t;
  indexes : (string, index_info) Hashtbl.t;
  fts     : (string, fts_table_meta) Hashtbl.t;
  triggers : (string, trigger_meta) Hashtbl.t;   (* trigger_name → meta *)
}
```

- [ ] **Step 4: Add trigger encoding/decoding helpers**

After the view persistence section (around line 474), add:

```ocaml
(* ------------------------------------------------------------------ *)
(* Trigger persistence                                                  *)
(* ------------------------------------------------------------------ *)

let encode_trigger_timing = function TT_before -> "BEFORE" | TT_after -> "AFTER"
let decode_trigger_timing = function "BEFORE" -> TT_before | _ -> TT_after

let encode_trigger_event = function
  | TE_insert -> "INSERT" | TE_update -> "UPDATE" | TE_delete -> "DELETE"
let decode_trigger_event = function
  | "UPDATE" -> TE_update | "DELETE" -> TE_delete | _ -> TE_insert

(** Encode trigger_meta as a multi-line string.
    Format: TIMING NL EVENT NL TABLE NL WHEN_OR_EMPTY NL BODY_STMTS_TAB_SEPARATED *)
let encode_trigger m =
  let body_line = String.concat "\t" m.trig_body in
  let when_line = match m.trig_when with None -> "" | Some s -> s in
  Printf.sprintf "%s\n%s\n%s\n%s\n%s"
    (encode_trigger_timing m.trig_timing)
    (encode_trigger_event m.trig_event)
    m.trig_table
    when_line
    body_line

let decode_trigger name bytes =
  let s = Bytes.to_string bytes in
  match String.split_on_char '\n' s with
  | timing_s :: event_s :: table_s :: when_s :: body_line :: _ ->
    let trig_when = if when_s = "" then None else Some when_s in
    let trig_body = if body_line = "" then [] else String.split_on_char '\t' body_line in
    Some {
      trig_name   = name;
      trig_table  = table_s;
      trig_timing = decode_trigger_timing timing_s;
      trig_event  = decode_trigger_event event_s;
      trig_when;
      trig_body;
    }
  | _ -> None

let load_all_triggers store =
  let tbl = Hashtbl.create 4 in
  let%lwt tx = S.ro_begin store in
  let%lwt cur = S.cursor_open tx sys_triggers_tid in
  let _sr = S.cursor_first cur in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      let name = Bytes.to_string k in
      (match decode_trigger name v with
       | Some m -> Hashtbl.replace tbl name m
       | None   -> ());
      walk ()
  in
  walk ();
  S.cursor_close cur;
  let%lwt () = S.ro_end tx in
  Lwt.return tbl

let persist_trigger store (m : trigger_meta) =
  let%lwt tx = S.rw_begin store in
  let%lwt () = S.put tx sys_triggers_tid
    (Bytes.of_string m.trig_name) (Bytes.of_string (encode_trigger m)) in
  S.commit tx

let remove_trigger store ~name =
  let%lwt tx = S.rw_begin store in
  let%lwt () = S.del tx sys_triggers_tid (Bytes.of_string name) in
  S.commit tx
```

- [ ] **Step 5: Load triggers in open_**

In `lib/catalog/catalog.ml`, find the `open_` function (around line 521):

```ocaml
let open_ store =
  let%lwt cache = load_all_tables store in
  let%lwt indexes = load_all_indexes store in
  let%lwt fts = load_all_fts store in
  (* Load FK constraints for each table *)
  ...
  Lwt.return { store; cache; indexes; fts }
```

Change the final `Lwt.return` to also load triggers:

```ocaml
let open_ store =
  let%lwt cache = load_all_tables store in
  let%lwt indexes = load_all_indexes store in
  let%lwt fts = load_all_fts store in
  let%lwt triggers = load_all_triggers store in
  (* Load FK constraints for each table *)
  let names = Hashtbl.fold (fun k _ acc -> k :: acc) cache [] in
  let%lwt () = Lwt_list.iter_s (fun name ->
    let%lwt fks = load_fk_constraints_raw store name in
    (match Hashtbl.find_opt cache name with
     | Some meta -> Hashtbl.replace cache name { meta with fk_constraints = fks }
     | None -> ());
    Lwt.return_unit
  ) names in
  Lwt.return { store; cache; indexes; fts; triggers }
```

- [ ] **Step 6: Add trigger lookup helpers**

After `open_`, add:

```ocaml
let add_trigger t (m : trigger_meta) =
  Hashtbl.replace t.triggers m.trig_name m

let remove_trigger_from_cache t ~name =
  Hashtbl.remove t.triggers name

let triggers_for t ~table ~timing ~event =
  Hashtbl.fold (fun _ m acc ->
    if String.equal m.trig_table table
    && m.trig_timing = timing
    && m.trig_event  = event
    then m :: acc
    else acc
  ) t.triggers []
```

### catalog.mli

- [ ] **Step 7: Export new types and functions in catalog.mli**

After the `remove_view` declaration, add:

```ocaml
(** Trigger timing and event enumerations. *)
type trigger_timing = TT_before | TT_after
type trigger_event  = TE_insert | TE_update | TE_delete

(** Per-trigger metadata (in-memory and persisted). *)
type trigger_meta = {
  trig_name    : string;
  trig_table   : string;
  trig_timing  : trigger_timing;
  trig_event   : trigger_event;
  trig_when    : string option;
  trig_body    : string list;
}

(** Load all persisted trigger definitions. Returns a hashtable keyed by trigger name. *)
val load_all_triggers : Sqlocaml_store.Store.t -> (string, trigger_meta) Hashtbl.t Lwt.t

(** Persist a trigger definition to the sys_triggers B-tree. *)
val persist_trigger : Sqlocaml_store.Store.t -> trigger_meta -> unit Lwt.t

(** Remove a trigger definition from the sys_triggers B-tree. *)
val remove_trigger : Sqlocaml_store.Store.t -> name:string -> unit Lwt.t

(** Add a trigger to the in-memory cache. *)
val add_trigger : t -> trigger_meta -> unit

(** Remove a trigger from the in-memory cache (does not affect storage). *)
val remove_trigger_from_cache : t -> name:string -> unit

(** Return all triggers for a given table, timing, and event. *)
val triggers_for :
  t ->
  table:string ->
  timing:trigger_timing ->
  event:trigger_event ->
  trigger_meta list
```

- [ ] **Step 8: Build and test**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -40
```

Expected: warnings in exec.ml about non-exhaustive patterns, no type errors in catalog.

- [ ] **Step 9: Commit**

```bash
git add lib/catalog/catalog.ml lib/catalog/catalog.mli
git commit -m "feat(phase22): catalog trigger persistence (sys_triggers_tid=6, trigger_meta, load/save/lookup) [#129]"
```

---

## Task 5: exec.ml — DML hooks and Op_create_trigger exhaustiveness guards

**Files:**
- Modify: `lib/sql/exec.ml`
- Modify: `lib/sql/exec.mli`

### Add before_hook / after_hook to DML functions

The hooks allow db.ml to fire triggers without exec.ml having any knowledge of triggers. Each hook receives the new row (for INSERT/UPDATE) and/or old row (for UPDATE/DELETE).

- [ ] **Step 1: Update execute_insert signature and body**

In `lib/sql/exec.ml`, find `execute_insert` (around line 1235). Change the optional parameter list to add two new hooks:

```ocaml
let execute_insert ?(mode = Auto) ?(params = [||])
    ?(clock : (unit -> float) option = None)
    ?(on_conflict : Ast.conflict_action option = None)
    ?(upsert_update : (string list * (int * Plan.expr) list) option = None)
    ?(prebuilt_row : Row.t option = None)
    ?(before_hook : (Row.t -> unit Lwt.t) option = None)
    ?(after_hook  : (Row.t -> unit Lwt.t) option = None)
    (store : S.t) (cat : Cat.t)
    ~(table_meta : Cat.table_meta) ~ordinals ~(values : Plan.expr list) : bool Lwt.t =
```

Inside the body, after the row is built and CHECK/FK checks pass, **before** `acquire_txn`:

```ocaml
  (* Fire BEFORE INSERT triggers *)
  let* () = match before_hook with None -> Lwt.return_unit | Some f -> f row in
  let* (tx, owned) = acquire_txn store mode in
  ...
```

After `release_txn tx owned` succeeds (the last `Lwt.return true` or equivalent), fire the AFTER hook. The function currently ends with something like:

```ocaml
        let* () = release_txn tx owned in
        Lwt.return true)
```

Change to:

```ocaml
        let* () = release_txn tx owned in
        let* () = match after_hook with None -> Lwt.return_unit | Some f -> f row in
        Lwt.return true)
```

For the UPSERT/on-conflict paths that return `Lwt.return false` or skip the write, do NOT fire the after hook. Only fire it when the row was actually inserted.

- [ ] **Step 2: Update execute_update signature and body**

In `lib/sql/exec.ml`, find `execute_update` (around line 1565). Add hooks:

```ocaml
let execute_update ?(mode = Auto) ?(params = [||])
    ?(clock : (unit -> float) option = None)
    ?(before_hook : (old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option = None)
    ?(after_hook  : (old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option = None)
    (store : S.t) (cat : Cat.t)
    ~(table_meta : Cat.table_meta)
    ~(assignments : (int * Plan.expr) list)
    ~(where : Plan.expr option)
    ~(indexes : Cat.index_info list)
  : int Lwt.t =
```

Inside the body, after `matches` is computed and FK checks pass, **before** `acquire_txn`:

```ocaml
  (* Fire BEFORE UPDATE triggers (per row) *)
  let* () = match before_hook with
    | None -> Lwt.return_unit
    | Some f ->
      Lwt_list.iter_s (fun (_rowid, old_row) ->
        let new_row = Array.copy old_row in
        List.iter (fun (i, expr) ->
          new_row.(i) <- eval_expr clock params old_row expr
        ) assignments;
        f ~old_row ~new_row
      ) matches
  in
  let* (tx, owned) = acquire_txn store mode in
  ...
```

After `release_txn tx owned` succeeds:

```ocaml
        let* () = release_txn tx owned in
        (* Fire AFTER UPDATE triggers (per row) *)
        let* () = match after_hook with
          | None -> Lwt.return_unit
          | Some f ->
            Lwt_list.iter_s (fun (_rowid, old_row) ->
              let new_row = Array.copy old_row in
              List.iter (fun (i, expr) ->
                new_row.(i) <- eval_expr clock params old_row expr
              ) assignments;
              f ~old_row ~new_row
            ) matches
        in
        Lwt.return n)
```

- [ ] **Step 3: Update execute_delete signature and body**

In `lib/sql/exec.ml`, find `execute_delete` (around line 1714). Add hooks:

```ocaml
let execute_delete ?(mode = Auto) ?(params = [||])
    ?(clock : (unit -> float) option = None)
    ?(before_hook : (Row.t -> unit Lwt.t) option = None)
    ?(after_hook  : (Row.t -> unit Lwt.t) option = None)
    (store : S.t) (cat : Cat.t)
    ~(table_meta : Cat.table_meta)
    ~(where : Plan.expr option)
    ~(indexes : Cat.index_info list)
  : int Lwt.t =
```

Inside the body, after FK parent checks pass, **before** `acquire_txn`:

```ocaml
  (* Fire BEFORE DELETE triggers (per row) *)
  let* () = match before_hook with
    | None -> Lwt.return_unit
    | Some f -> Lwt_list.iter_s (fun (_rowid, old_row) -> f old_row) matches
  in
  let* (tx, owned) = acquire_txn store mode in
  ...
```

After `release_txn tx owned`:

```ocaml
        let* () = release_txn tx owned in
        (* Fire AFTER DELETE triggers (per row) *)
        let* () = match after_hook with
          | None -> Lwt.return_unit
          | Some f -> Lwt_list.iter_s (fun (_rowid, old_row) -> f old_row) matches
        in
        Lwt.return n)
```

- [ ] **Step 4: Update execute_with_count to accept and pass hooks**

In `lib/sql/exec.ml`, find `execute_with_count` (around line 1830). Add hook parameters:

```ocaml
let execute_with_count ?(mode = Auto)
    ?(clock : (unit -> float) option = None)
    ?(params = [||])
    ?(before_hook : (new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t) option = None)
    ?(after_hook  : (new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t) option = None)
    (store : S.t) (cat : Cat.t) (op : Plan.op) : int Lwt.t =
```

Translate the unified hook type to per-DML hook types in the dispatch cases:

For `Op_insert`:
```ocaml
  | Plan.Op_insert { table_meta; ordinals; values; on_conflict; returning = _; upsert_update } ->
    let bh = Option.map (fun f -> (fun row -> f ~new_row:(Some row) ~old_row:None)) before_hook in
    let ah = Option.map (fun f -> (fun row -> f ~new_row:(Some row) ~old_row:None)) after_hook in
    Lwt_list.fold_left_s (fun count row_vals ->
      let* inserted = execute_insert ~mode ~params ~clock ~on_conflict ~upsert_update
                        ?before_hook:bh ?after_hook:ah
                        store cat ~table_meta ~ordinals ~values:row_vals in
      Lwt.return (count + if inserted then 1 else 0)
    ) 0 values
```

For `Op_update`:
```ocaml
  | Plan.Op_update { table_meta; assignments; where; indexes; returning = _ } ->
    let bh = Option.map (fun f ->
      (fun ~old_row ~new_row -> f ~new_row:(Some new_row) ~old_row:(Some old_row))
    ) before_hook in
    let ah = Option.map (fun f ->
      (fun ~old_row ~new_row -> f ~new_row:(Some new_row) ~old_row:(Some old_row))
    ) after_hook in
    execute_update ~mode ~params ~clock ?before_hook:bh ?after_hook:ah
      store cat ~table_meta ~assignments ~where ~indexes
```

For `Op_delete`:
```ocaml
  | Plan.Op_delete { table_meta; where; indexes; returning = _ } ->
    let bh = Option.map (fun f ->
      (fun old_row -> f ~new_row:None ~old_row:(Some old_row))
    ) before_hook in
    let ah = Option.map (fun f ->
      (fun old_row -> f ~new_row:None ~old_row:(Some old_row))
    ) after_hook in
    execute_delete ~mode ~params ~clock ?before_hook:bh ?after_hook:ah
      store cat ~table_meta ~where ~indexes
```

- [ ] **Step 5: Add exhaustiveness guards for Op_create_trigger and Op_drop_trigger**

In `lib/sql/exec.ml`, find the exhaustiveness guard section in `execute_with_count`. It has a comment about "use Exec.query for read operations". After the existing `failwith "Exec.execute: use Exec.query for read operations"` guard, add Op_create_trigger and Op_drop_trigger to the guard that says to use the Db layer:

Find the pattern that currently ends with:
```ocaml
  | Plan.Op_begin | Plan.Op_commit | Plan.Op_rollback
  | Plan.Op_savepoint _ | Plan.Op_release _ | Plan.Op_rollback_to _ ->
    failwith "Exec.execute_with_count: transaction ops must be handled by the Db layer"
```

Add the trigger ops to this same guard:
```ocaml
  | Plan.Op_begin | Plan.Op_commit | Plan.Op_rollback
  | Plan.Op_savepoint _ | Plan.Op_release _ | Plan.Op_rollback_to _
  | Plan.Op_create_trigger _ | Plan.Op_drop_trigger _ ->
    failwith "Exec.execute_with_count: trigger DDL must be handled by the Db layer"
```

Also add to the `to_stream` function's DDL guard (the one that says "use Exec.execute for write operations"):
```ocaml
  | Plan.Op_create_view _ | Plan.Op_drop_view _
  | Plan.Op_create_trigger _ | Plan.Op_drop_trigger _
  | Plan.Op_begin | ...
```

- [ ] **Step 6: Update exec.mli**

In `lib/sql/exec.mli`, update `execute_with_count` to expose the new optional params:

```ocaml
val execute_with_count :
  ?mode:txn_mode ->
  ?clock:(unit -> float) option ->
  ?params:Sqlocaml_encoding.Row.value array ->
  ?before_hook:(new_row:Sqlocaml_encoding.Row.t option ->
                old_row:Sqlocaml_encoding.Row.t option ->
                unit Lwt.t) ->
  ?after_hook:(new_row:Sqlocaml_encoding.Row.t option ->
               old_row:Sqlocaml_encoding.Row.t option ->
               unit Lwt.t) ->
  Sqlocaml_store.Store.t ->
  Sqlocaml_catalog.Catalog.t ->
  Plan.op ->
  int Lwt.t
```

- [ ] **Step 7: Build and verify**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -40
```

Expected: warnings about non-exhaustive in db.ml (we haven't handled Op_create_trigger/Op_drop_trigger there yet). No type errors.

Run existing tests to check nothing regressed:

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe 2>&1 | tail -10
```

Expected: All 339 tests still pass.

- [ ] **Step 8: Commit**

```bash
git add lib/sql/exec.ml lib/sql/exec.mli
git commit -m "feat(phase22): add before_hook/after_hook to DML functions; Op_create_trigger/Op_drop_trigger guards [#129]"
```

---

## Task 6: db.ml — trigger firing logic and DDL handling

**Files:**
- Modify: `lib/db/db.ml`
- Modify: `lib/db/db.mli`

### db.t — add triggers hashtable

- [ ] **Step 1: Add triggers field to db.t**

In `lib/db/db.ml`, find the `type t = { ... }` record (around line 7). Add:

```ocaml
type t = {
  store            : S.t;
  catalog          : Cat.t;
  clock            : (unit -> float) option;
  mutable explicit_txn    : S.rw S.txn option;
  views            : (string, Sql.Ast.stmt) Hashtbl.t;
  triggers         : (string, Sql.Ast.stmt) Hashtbl.t;  (* trigger_name → S_create_trigger AST *)
  mutable savepoint_names : string list;
  mutable auto_began      : bool;
}
```

- [ ] **Step 2: Initialize triggers in all three construction functions**

In `open_in_memory`, `open_file`, and `open_block`, add `triggers = Hashtbl.create 4` and a call to load triggers.

Add helper `load_triggers_into_hashtbl` right after `load_views_into_hashtbl`:

```ocaml
let load_triggers_into_hashtbl store triggers_tbl =
  let* trig_map = Cat.load_all_triggers store in
  Hashtbl.iter (fun name (m : Cat.trigger_meta) ->
    (* Re-construct the S_create_trigger AST from the stored meta *)
    let timing = match m.Cat.trig_timing with
      | Cat.TT_before -> Sql.Ast.TT_before
      | Cat.TT_after  -> Sql.Ast.TT_after
    in
    let event = match m.Cat.trig_event with
      | Cat.TE_insert -> Sql.Ast.TE_insert
      | Cat.TE_update -> Sql.Ast.TE_update
      | Cat.TE_delete -> Sql.Ast.TE_delete
    in
    let when_ = Option.map (fun s ->
      let lexbuf = Lexing.from_string s in
      (* Parse as an expression using a simple wrapper query *)
      (* We store WHEN as a full condition expression string; re-parse it *)
      match Sql.Parser.expr_eof Sql.Lexer.token lexbuf with
      | e -> e
      | exception _ -> Sql.Ast.E_lit Sql.Ast.L_null  (* skip unparseable WHEN *)
    ) m.Cat.trig_when in
    let body = List.filter_map (fun sql ->
      let lexbuf = Lexing.from_string sql in
      match Sql.Parser.stmt_eof Sql.Lexer.token lexbuf with
      | s -> Some s
      | exception _ -> None  (* skip unparseable body statements *)
    ) m.Cat.trig_body in
    let ast = Sql.Ast.S_create_trigger { name; timing; event;
      table = m.Cat.trig_table; when_; body } in
    Hashtbl.replace triggers_tbl name ast
  ) trig_map;
  Lwt.return_unit
```

Wait — `Sql.Parser.expr_eof` does not exist yet. The parser only has `stmt_eof`. To parse a WHEN expression, we need a separate entry point. There are two options:
- Add `expr_eof` to the parser (a separate start symbol)
- Store the WHEN as a dummy `SELECT expr` and extract it

**Simpler approach:** Don't parse WHEN from catalog text at load time. Instead, store the entire `CREATE TRIGGER` SQL text in the catalog (like views), and parse it back at load time using the existing `stmt_eof`.

**Revised catalog persistence approach:** Store the full `CREATE TRIGGER ...` SQL as the value in sys_triggers (like views store the full `CREATE VIEW ...` SQL). This removes the need for custom encoding/decoding.

This requires `Ast.stmt_to_sql` for triggers. We don't have that. Alternative: store the SQL string that was originally used to create the trigger.

**Revised approach:** In db.ml's Op_create_trigger handler, receive the original SQL and persist it:

```ocaml
| Ok (Sql.Plan.Op_create_trigger _) ->
  (* Persist the original SQL *)
  let trig_name = ... in
  Hashtbl.replace t.triggers trig_name ast;
  Cat.persist_trigger_sql t.store ~name:trig_name ~sql;
  ...
```

But `execute t sql` doesn't give us both the `Op` and the original `sql` string in the Op_create_trigger handler... it does: `sql` is the parameter to `execute t sql`. So we can extract the name from Op and store `sql` keyed by name.

**Final revised catalog persistence:**

In catalog.ml, change `persist_trigger` to store just the SQL string:

```ocaml
(* In catalog.ml *)
let sys_triggers_tid : S.tree_id = 6

let load_all_triggers store =
  (* Returns (name, sql_string) list *)
  let%lwt tx = S.ro_begin store in
  let%lwt cur = S.cursor_open tx sys_triggers_tid in
  let _sr = S.cursor_first cur in
  let pairs = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      pairs := (Bytes.to_string k, Bytes.to_string v) :: !pairs;
      walk ()
  in
  walk ();
  S.cursor_close cur;
  let%lwt () = S.ro_end tx in
  Lwt.return (List.rev !pairs)

let persist_trigger store ~name ~sql =
  let%lwt tx = S.rw_begin store in
  let%lwt () = S.put tx sys_triggers_tid (Bytes.of_string name) (Bytes.of_string sql) in
  S.commit tx

let remove_trigger store ~name =
  let%lwt tx = S.rw_begin store in
  let%lwt () = S.del tx sys_triggers_tid (Bytes.of_string name) in
  S.commit tx
```

This matches the view pattern exactly. **The `trigger_meta` type stays in catalog.mli but is also used purely in db.ml** for the in-memory trigger representation that db.ml constructs when needed (extracted from the AST).

Revise the catalog to use simple SQL-text storage (like views), removing the encoding/decoding added in Task 4. Update catalog accordingly. Add the `trigger_meta` type but only used by db.ml.

Actually, let me revise: The `trigger_meta` type can stay in catalog.ml if we want (for clean separation), but we populate it in db.ml from the parsed AST. The catalog module just stores (name, sql) pairs.

**Revised catalog.ml (replacing what was written in Task 4):**

The `trigger_meta` type, `triggers` hashtable on `Cat.t`, `add_trigger`, `remove_trigger_from_cache`, and `triggers_for` can all move to **db.ml** since they depend on `Ast.trigger_timing`/`Ast.trigger_event` from the sql library, creating an unwanted dependency.

Cleanest solution: `Cat.t` gets NO trigger-related additions. Catalog only provides `load_all_triggers`, `persist_trigger`, `remove_trigger` (all operating on raw SQL strings). The `trigger_meta` type and in-memory hashtable live in **db.t**.

- [ ] **Step 3 (revised): Simplify catalog.ml to SQL-text storage only**

In `lib/catalog/catalog.ml`, add ONLY:

```ocaml
let sys_triggers_tid : S.tree_id = 6

let load_all_triggers store =
  let%lwt tx = S.ro_begin store in
  let%lwt cur = S.cursor_open tx sys_triggers_tid in
  let _sr = S.cursor_first cur in
  let pairs = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      pairs := (Bytes.to_string k, Bytes.to_string v) :: !pairs;
      walk ()
  in
  walk ();
  S.cursor_close cur;
  let%lwt () = S.ro_end tx in
  Lwt.return (List.rev !pairs)

let persist_trigger store ~name ~sql =
  let%lwt tx = S.rw_begin store in
  let%lwt () = S.put tx sys_triggers_tid (Bytes.of_string name) (Bytes.of_string sql) in
  S.commit tx

let remove_trigger store ~name =
  let%lwt tx = S.rw_begin store in
  let%lwt () = S.del tx sys_triggers_tid (Bytes.of_string name) in
  S.commit tx
```

Do NOT add `trigger_meta` type, `triggers` hashtable, or `triggers_for` to catalog. Do NOT add triggers field to `Cat.t`. Update `catalog.mli` to only declare:

```ocaml
val load_all_triggers : Sqlocaml_store.Store.t -> (string * string) list Lwt.t
val persist_trigger   : Sqlocaml_store.Store.t -> name:string -> sql:string -> unit Lwt.t
val remove_trigger    : Sqlocaml_store.Store.t -> name:string -> unit Lwt.t
```

### db.ml — trigger_meta type and in-memory state

- [ ] **Step 4: Define trigger_meta type in db.ml**

In `lib/db/db.ml`, after the `type row = Row.t` line (around line 24), add:

```ocaml
type trigger_meta = {
  trig_name   : string;
  trig_table  : string;
  trig_timing : [ `Before | `After ];
  trig_event  : [ `Insert | `Update | `Delete ];
  trig_when   : Sql.Ast.expr option;
  trig_body   : Sql.Ast.stmt list;
}
```

- [ ] **Step 5: Add triggers hashtable to db.t and initialize it**

In `type t`, add `triggers : (string, trigger_meta) Hashtbl.t`. Initialize it in `open_in_memory` as:

```ocaml
Lwt.return { store; catalog; clock; explicit_txn = None; views = Hashtbl.create 4;
             triggers = Hashtbl.create 4;
             savepoint_names = []; auto_began = false }
```

For `open_file` and `open_block`, add a `load_triggers_into_hashtbl` call similar to `load_views_into_hashtbl`.

Add after `load_views_into_hashtbl`:

```ocaml
let load_triggers_into_hashtbl store trig_tbl =
  let* pairs = Cat.load_all_triggers store in
  List.iter (fun (name, sql) ->
    match
      let lexbuf = Lexing.from_string sql in
      Sql.Parser.stmt_eof Sql.Lexer.token lexbuf
    with
    | Sql.Ast.S_create_trigger { timing; event; table; when_; body; _ } ->
      let trig_timing = (match timing with
        | Sql.Ast.TT_before -> `Before | Sql.Ast.TT_after -> `After) in
      let trig_event = (match event with
        | Sql.Ast.TE_insert -> `Insert
        | Sql.Ast.TE_update -> `Update
        | Sql.Ast.TE_delete -> `Delete) in
      let m = { trig_name = name; trig_table = table;
                trig_timing; trig_event; trig_when = when_; trig_body = body } in
      Hashtbl.replace trig_tbl name m
    | _ -> ()
    | exception _ -> ()
  ) pairs;
  Lwt.return_unit
```

Update `open_file` and `open_block` to call this:

```ocaml
let open_file ~path =
  ...
  let triggers = Hashtbl.create 4 in
  let* () = load_triggers_into_hashtbl store triggers in
  Lwt.return (Ok { store; catalog; clock = None; explicit_txn = None; views;
                   triggers; savepoint_names = []; auto_began = false })
```

### db.ml — AST substitution for NEW/OLD

- [ ] **Step 6: Add value_to_literal helper**

In `lib/db/db.ml`, before the `open_in_memory` function, add:

```ocaml
let value_to_literal = function
  | Row.V_int n  -> Sql.Ast.L_int n
  | Row.V_text s -> Sql.Ast.L_text s
  | Row.V_real f -> Sql.Ast.L_real f
  | Row.V_blob b -> Sql.Ast.L_blob b
  | Row.V_null   -> Sql.Ast.L_null
```

- [ ] **Step 7: Add the AST expression mapper**

Add `map_expr` and `subst_new_old` before `open_in_memory`:

```ocaml
(** Walk an Ast.expr tree, calling [f tbl col] for each E_tbl_col.
    If [f] returns [Some lit], replace with [E_lit lit]; otherwise keep. *)
let rec map_expr f e =
  let go = map_expr f in
  match e with
  | Sql.Ast.E_tbl_col (tbl, col) ->
    (match f tbl col with Some lit -> Sql.Ast.E_lit lit | None -> e)
  | Sql.Ast.E_binop (op, a, b) -> Sql.Ast.E_binop (op, go a, go b)
  | Sql.Ast.E_not x -> Sql.Ast.E_not (go x)
  | Sql.Ast.E_is_null x -> Sql.Ast.E_is_null (go x)
  | Sql.Ast.E_is_not_null x -> Sql.Ast.E_is_not_null (go x)
  | Sql.Ast.E_neg x -> Sql.Ast.E_neg (go x)
  | Sql.Ast.E_bitnot x -> Sql.Ast.E_bitnot (go x)
  | Sql.Ast.E_between (x, lo, hi) -> Sql.Ast.E_between (go x, go lo, go hi)
  | Sql.Ast.E_in (x, vs) -> Sql.Ast.E_in (go x, List.map go vs)
  | Sql.Ast.E_func (fn, args) -> Sql.Ast.E_func (fn, List.map go args)
  | Sql.Ast.E_agg (fn, arg) -> Sql.Ast.E_agg (fn, Option.map go arg)
  | Sql.Ast.E_case { scrutinee; branches; else_ } ->
    Sql.Ast.E_case {
      scrutinee = Option.map go scrutinee;
      branches  = List.map (fun (c, r) -> (go c, go r)) branches;
      else_     = Option.map go else_;
    }
  | Sql.Ast.E_cast (x, ty) -> Sql.Ast.E_cast (go x, ty)
  | Sql.Ast.E_collate (x, c) -> Sql.Ast.E_collate (go x, c)
  | other -> other  (* E_lit, E_col, E_param, E_match, E_subquery, E_exists, E_in_select, E_window — left as-is *)

(** Replace [NEW.col] and [OLD.col] references in an expression with literals. *)
let make_subst_fn ~schema ~(new_row : Row.t option) ~(old_row : Row.t option) =
  let col_idx name =
    let rec fi i = function
      | [] -> None
      | (c : Row.column) :: _ when String.equal c.name name -> Some i
      | _ :: rest -> fi (i + 1) rest
    in fi 0 schema
  in
  fun tbl col ->
    match String.uppercase_ascii tbl with
    | "NEW" -> (match new_row, col_idx col with
      | Some r, Some i -> Some (value_to_literal r.(i))
      | _ -> None)
    | "OLD" -> (match old_row, col_idx col with
      | Some r, Some i -> Some (value_to_literal r.(i))
      | _ -> None)
    | _ -> None

(** Substitute NEW.col / OLD.col references in an Ast.stmt with literal values. *)
let subst_new_old ~schema ~new_row ~old_row stmt =
  let f = make_subst_fn ~schema ~new_row ~old_row in
  let ge = map_expr f in
  match stmt with
  | Sql.Ast.S_insert { table; columns; values; on_conflict; returning; upsert_update } ->
    Sql.Ast.S_insert { table; columns;
      values = List.map (List.map ge) values;
      on_conflict;
      returning = List.map ge returning;
      upsert_update = Option.map (fun u ->
        { u with Sql.Ast.assignments =
            List.map (fun (c, e) -> (c, ge e)) u.Sql.Ast.assignments }
      ) upsert_update;
    }
  | Sql.Ast.S_update { table; assignments; where; returning } ->
    Sql.Ast.S_update { table;
      assignments = List.map (fun (c, e) -> (c, ge e)) assignments;
      where = Option.map ge where;
      returning = List.map ge returning;
    }
  | Sql.Ast.S_delete { table; where; returning } ->
    Sql.Ast.S_delete { table; where = Option.map ge where;
      returning = List.map ge returning }
  | Sql.Ast.S_select { distinct; proj; table; table_alias; joins;
                        where; group_by; having; order; limit; offset } ->
    Sql.Ast.S_select { distinct;
      proj = (match proj with
        | `Exprs es -> `Exprs (List.map (fun (e, a) -> (ge e, a)) es)
        | other -> other);
      table; table_alias; joins;
      where = Option.map ge where;
      group_by;
      having = Option.map ge having;
      order = List.map (fun ok ->
        { ok with Sql.Ast.expr = ge ok.Sql.Ast.expr }) order;
      limit; offset;
    }
  | other -> other  (* DDL, transaction control: NEW/OLD not relevant *)
```

### db.ml — trigger firing

- [ ] **Step 8: Add fire_trigger_stmt and make_trigger_hook**

Add after the savepoint functions (around line 188):

```ocaml
(* ------------------------------------------------------------------ *)
(* Trigger firing                                                       *)
(* ------------------------------------------------------------------ *)

(** Compile and execute a single pre-substituted trigger body statement. *)
let fire_trigger_stmt t (stmt : Sql.Ast.stmt) : (unit, error) result Lwt.t =
  let* bound = Sql.Sema.bind ~views:t.views t.catalog stmt in
  match bound with
  | Error e -> Lwt.return (Error (Sema e))
  | Ok b ->
    let op = Sql.Planner.plan ~cat:t.catalog b in
    let mode = match t.explicit_txn with
      | None    -> Sql.Exec.Auto
      | Some tx -> Sql.Exec.In_txn tx
    in
    Lwt.catch
      (fun () ->
        let* () = Sql.Exec.execute ~mode ~clock:t.clock t.store t.catalog op in
        Lwt.return (Ok ()))
      (function
       | Failure msg -> Lwt.return (Error (Runtime msg))
       | exn         -> Lwt.fail exn)

(** Execute all matching triggers for a given timing/event/table/row pair.
    BEFORE triggers are fired before the DML; AFTER triggers after.
    Errors in trigger bodies propagate as [Runtime] errors (aborting the DML). *)
let fire_triggers t ~timing ~event ~table_meta ~new_row ~old_row : (unit, error) result Lwt.t =
  let schema = table_meta.Cat.columns in
  let trigs =
    Hashtbl.fold (fun _ m acc ->
      if String.equal m.trig_table table_meta.Cat.name
      && m.trig_timing = timing
      && m.trig_event  = event
      then m :: acc else acc
    ) t.triggers []
  in
  Lwt_list.fold_left_s (fun acc_result m ->
    match acc_result with
    | Error _ as e -> Lwt.return e
    | Ok () ->
      (* Evaluate WHEN clause — skip trigger if false *)
      let should_fire = match m.trig_when with
        | None -> true
        | Some when_expr ->
          let subst_when = map_expr (make_subst_fn ~schema ~new_row ~old_row) when_expr in
          let* bound_when = Sql.Sema.bind ~views:t.views t.catalog
            (Sql.Ast.S_const_select { exprs = [(subst_when, None)] }) in
          (match bound_when with
           | Error _ -> Lwt.return true  (* if WHEN fails to bind, fire the trigger *)
           | Ok bw ->
             let op = Sql.Planner.plan ~cat:t.catalog bw in
             let mode = match t.explicit_txn with
               | None    -> Sql.Exec.Auto
               | Some tx -> Sql.Exec.In_txn tx
             in
             let* stream = Sql.Exec.query ~mode ~clock:t.clock t.store t.catalog op in
             let* rows = Lwt_stream.to_list stream in
             Lwt.return (match rows with
               | [| v |] :: _ ->
                 (match v with
                  | Row.V_int 0L | Row.V_null -> false
                  | _ -> true)
               | _ -> true))
      in
      (* Note: should_fire is `bool` now, but the above has Lwt in branches *)
      (* Fix: wrap the WHEN evaluation properly *)
      ...
  ) (Ok ()) trigs
```

Wait, the WHEN evaluation above has a type error — I'm mixing `bool` and `bool Lwt.t` in `should_fire`. Let me rewrite `fire_triggers` correctly:

```ocaml
let fire_triggers t ~timing ~event ~table_meta ~new_row ~old_row : (unit, error) result Lwt.t =
  let schema = table_meta.Cat.columns in
  let trigs =
    Hashtbl.fold (fun _ m acc ->
      if String.equal m.trig_table table_meta.Cat.name
      && m.trig_timing = timing
      && m.trig_event  = event
      then m :: acc else acc
    ) t.triggers []
  in
  let eval_when_clause when_expr =
    let subst = map_expr (make_subst_fn ~schema ~new_row ~old_row) when_expr in
    let* bound = Sql.Sema.bind ~views:t.views t.catalog
      (Sql.Ast.S_const_select { exprs = [(subst, None)] }) in
    match bound with
    | Error _ -> Lwt.return true  (* binding error → fire trigger *)
    | Ok bw ->
      let op = Sql.Planner.plan ~cat:t.catalog bw in
      let mode = match t.explicit_txn with
        | None -> Sql.Exec.Auto | Some tx -> Sql.Exec.In_txn tx in
      let* stream = Sql.Exec.query ~mode ~clock:t.clock t.store t.catalog op in
      let* rows = Lwt_stream.to_list stream in
      Lwt.return (match rows with
        | [| v |] :: _ -> (match v with Row.V_int 0L | Row.V_null -> false | _ -> true)
        | _ -> true)
  in
  Lwt_list.fold_left_s (fun acc m ->
    match acc with
    | Error _ as e -> Lwt.return e
    | Ok () ->
      let* should_fire = match m.trig_when with
        | None -> Lwt.return true
        | Some e -> eval_when_clause e
      in
      if not should_fire then Lwt.return (Ok ())
      else begin
        (* Execute each body statement *)
        Lwt_list.fold_left_s (fun acc2 stmt ->
          match acc2 with
          | Error _ as e -> Lwt.return e
          | Ok () ->
            let substituted = subst_new_old ~schema ~new_row ~old_row stmt in
            fire_trigger_stmt t substituted
        ) (Ok ()) m.trig_body
      end
  ) (Ok ()) trigs

(** Build a unified row hook for exec.ml's DML functions.
    Returns None if no triggers exist for the given table/event, avoiding overhead. *)
let make_trigger_hook t table_meta ~timing ~event =
  let has_any = Hashtbl.fold (fun _ m acc ->
    acc || (String.equal m.trig_table table_meta.Cat.name
            && m.trig_timing = timing && m.trig_event = event)
  ) t.triggers false in
  if not has_any then None
  else Some (fun ~new_row ~old_row ->
    let* result = fire_triggers t ~timing ~event ~table_meta ~new_row ~old_row in
    match result with
    | Ok () -> Lwt.return_unit
    | Error (Runtime msg) -> Lwt.fail_with msg
    | Error (Sema e) ->
      Lwt.fail_with (Format.asprintf "trigger sema error: %a" Sql.Sema.pp_error e)
    | Error (Parse msg) -> Lwt.fail_with ("trigger parse error: " ^ msg)
  )
```

### db.ml — Op_create_trigger / Op_drop_trigger handling

- [ ] **Step 9: Handle trigger DDL in execute and execute_change_count**

In `lib/db/db.ml`, in the `execute` function, add cases before the catch-all `| Ok op ->`:

```ocaml
  | Ok Sql.Plan.Op_create_trigger { name; timing; event; table; when_; body } ->
    let trig_timing = (match timing with Sql.Ast.TT_before -> `Before | Sql.Ast.TT_after -> `After) in
    let trig_event  = (match event  with
      | Sql.Ast.TE_insert -> `Insert
      | Sql.Ast.TE_update -> `Update
      | Sql.Ast.TE_delete -> `Delete) in
    let m = { trig_name = name; trig_table = table; trig_timing; trig_event;
              trig_when = when_; trig_body = body } in
    Hashtbl.replace t.triggers name m;
    let* () = Cat.persist_trigger t.store ~name ~sql in
    Lwt.return (Ok ())
  | Ok Sql.Plan.Op_drop_trigger { name } ->
    Hashtbl.remove t.triggers name;
    let* () = Cat.remove_trigger t.store ~name in
    Lwt.return (Ok ())
```

Also add the same two cases to `execute_change_count`:

```ocaml
  | Ok Sql.Plan.Op_create_trigger { name; timing; event; table; when_; body } ->
    let trig_timing = ... (* same as above *) in
    let trig_event  = ... in
    let m = { trig_name = name; trig_table = table; trig_timing; trig_event;
              trig_when = when_; trig_body = body } in
    Hashtbl.replace t.triggers name m;
    let* () = Cat.persist_trigger t.store ~name ~sql in
    Lwt.return (Ok 0)
  | Ok Sql.Plan.Op_drop_trigger { name } ->
    Hashtbl.remove t.triggers name;
    let* () = Cat.remove_trigger t.store ~name in
    Lwt.return (Ok 0)
```

### db.ml — trigger-aware DML dispatch

- [ ] **Step 10: Pass trigger hooks for DML ops**

In `execute` and `execute_change_count`, the catch-all `| Ok op ->` currently calls `Sql.Exec.execute` / `Sql.Exec.execute_with_count`. We need to pass hooks for INSERT/UPDATE/DELETE when triggers exist.

The simplest way: **extract the table_meta from the op when present**, build the hooks, and pass to `execute_with_count`. Since `execute` wraps `execute_with_count`, update `execute_change_count` and `execute` separately.

In `execute_change_count`, change the `| Ok op ->` catch-all to:

```ocaml
  | Ok op ->
    let mode = match t.explicit_txn with
      | None    -> Sql.Exec.Auto
      | Some tx -> Sql.Exec.In_txn tx
    in
    let (before_hook, after_hook) = match op with
      | Sql.Plan.Op_insert { table_meta; _ } ->
        (make_trigger_hook t table_meta ~timing:`Before ~event:`Insert,
         make_trigger_hook t table_meta ~timing:`After  ~event:`Insert)
      | Sql.Plan.Op_update { table_meta; _ } ->
        (make_trigger_hook t table_meta ~timing:`Before ~event:`Update,
         make_trigger_hook t table_meta ~timing:`After  ~event:`Update)
      | Sql.Plan.Op_delete { table_meta; _ } ->
        (make_trigger_hook t table_meta ~timing:`Before ~event:`Delete,
         make_trigger_hook t table_meta ~timing:`After  ~event:`Delete)
      | _ -> (None, None)
    in
    (match Sql.Exec.execute_with_count ~mode ~clock:t.clock
             ?before_hook ?after_hook t.store t.catalog op with
     | exception Failure msg -> Lwt.return (Error (Runtime msg))
     | lwt_op ->
       Lwt.catch
         (fun () ->
           let* n = lwt_op in
           Lwt.return (Ok n))
         (function
          | Failure msg -> Lwt.return (Error (Runtime msg))
          | exn         -> Lwt.fail exn))
```

Do the same for `execute` (which calls `Sql.Exec.execute`). Note: `Sql.Exec.execute` calls `execute_with_count` internally. You can either update `Sql.Exec.execute` to also take hooks, OR simply call `execute_change_count` directly from `execute`:

```ocaml
let execute t sql =
  ...
  | Ok op ->
    let* result = execute_change_count_internal t op sql in  (* factor out *)
    match result with Ok _ -> Lwt.return (Ok ()) | Error e -> Lwt.return (Error e)
```

OR: since `execute` already calls `Sql.Exec.execute` which calls `execute_with_count`, just add `?before_hook`/`?after_hook` to `Sql.Exec.execute` as well (pass through to `execute_with_count`).

Simplest: Update `Sql.Exec.execute` signature in exec.ml and exec.mli to also accept hooks, so db.ml can pass them through both paths.

- [ ] **Step 11: Update Sql.Exec.execute signature**

In `lib/sql/exec.ml`, find:

```ocaml
let execute ?(mode = Auto)
    ?(clock : (unit -> float) option = None)
    ?(params = [||]) (store : S.t) (cat : Cat.t) (op : Plan.op) : unit Lwt.t =
  let* _n = execute_with_count ~mode ~clock ~params store cat op in
  Lwt.return_unit
```

Change to:

```ocaml
let execute ?(mode = Auto)
    ?(clock : (unit -> float) option = None)
    ?(params = [||])
    ?(before_hook : (new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t) option = None)
    ?(after_hook  : (new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t) option = None)
    (store : S.t) (cat : Cat.t) (op : Plan.op) : unit Lwt.t =
  let* _n = execute_with_count ~mode ~clock ~params ?before_hook ?after_hook store cat op in
  Lwt.return_unit
```

Update `exec.mli` to match.

In db.ml's `execute` function, similarly extract table_meta and pass hooks via `?before_hook` / `?after_hook`.

- [ ] **Step 12: Build and run all tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -40
```

Expected: clean build.

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe 2>&1 | tail -10
```

Expected: All 339 tests still pass (no regressions).

- [ ] **Step 13: Commit**

```bash
git add lib/catalog/catalog.ml lib/catalog/catalog.mli lib/db/db.ml lib/db/db.mli \
        lib/sql/exec.ml lib/sql/exec.mli
git commit -m "feat(phase22): trigger firing in db.ml — subst_new_old, fire_triggers, make_trigger_hook, DDL handling [#129]"
```

---

## Task 7: e2e tests

**Files:**
- Modify: `test/test_e2e.ml`

Add a `"triggers"` test suite. Each test uses the standard helpers:

```ocaml
let exec db sql =
  let* r = Db.execute db sql in
  (match r with Ok () -> () | Error e -> Alcotest.failf "execute failed: %a" Db.pp_error e);
  Lwt.return_unit

let query_rows db sql =
  let* r = Db.query db sql in
  match r with
  | Error e -> Alcotest.failf "query failed: %a" Db.pp_error e
  | Ok stream -> Lwt_stream.to_list stream

let int_val = function Row.V_int n -> Int64.to_int n | _ -> -9999
let text_val = function Row.V_text s -> s | _ -> "<null>"
```

- [ ] **Step 1: test_after_insert_trigger**

```ocaml
let test_after_insert_trigger () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* () = exec db "CREATE TABLE t (id INTEGER, val TEXT)" in
    let* () = exec db "CREATE TABLE audit (t_id INTEGER, action TEXT)" in
    let* () = exec db
      "CREATE TRIGGER t_ai AFTER INSERT ON t BEGIN \
       INSERT INTO audit VALUES (NEW.id, 'INSERT'); END" in
    let* () = exec db "INSERT INTO t VALUES (1, 'hello')" in
    let* rows = query_rows db "SELECT t_id, action FROM audit" in
    Alcotest.(check int)    "one audit row" 1 (List.length rows);
    Alcotest.(check int)    "t_id = 1"  1 (int_val rows.(0).(0));
    Alcotest.(check string) "action"    "INSERT" (text_val rows.(0).(1));
    Lwt.return_unit
  )
```

- [ ] **Step 2: test_after_insert_two_rows**

```ocaml
let test_after_insert_two_rows () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* () = exec db "CREATE TABLE t (id INTEGER, val TEXT)" in
    let* () = exec db "CREATE TABLE audit (t_id INTEGER)" in
    let* () = exec db
      "CREATE TRIGGER t_ai AFTER INSERT ON t BEGIN \
       INSERT INTO audit VALUES (NEW.id); END" in
    let* () = exec db "INSERT INTO t VALUES (10, 'a')" in
    let* () = exec db "INSERT INTO t VALUES (20, 'b')" in
    let* rows = query_rows db "SELECT t_id FROM audit ORDER BY t_id" in
    Alcotest.(check int) "two audit rows" 2 (List.length rows);
    Alcotest.(check int) "first id" 10 (int_val rows.(0).(0));
    Alcotest.(check int) "second id" 20 (int_val rows.(1).(0));
    Lwt.return_unit
  )
```

- [ ] **Step 3: test_after_delete_trigger**

```ocaml
let test_after_delete_trigger () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* () = exec db "CREATE TABLE t (id INTEGER, val TEXT)" in
    let* () = exec db "CREATE TABLE deleted_log (old_id INTEGER)" in
    let* () = exec db "INSERT INTO t VALUES (5, 'five')" in
    let* () = exec db "INSERT INTO t VALUES (6, 'six')" in
    let* () = exec db
      "CREATE TRIGGER t_ad AFTER DELETE ON t BEGIN \
       INSERT INTO deleted_log VALUES (OLD.id); END" in
    let* () = exec db "DELETE FROM t WHERE id = 5" in
    let* rows = query_rows db "SELECT old_id FROM deleted_log" in
    Alcotest.(check int) "one log row" 1 (List.length rows);
    Alcotest.(check int) "old_id = 5" 5 (int_val rows.(0).(0));
    Lwt.return_unit
  )
```

- [ ] **Step 4: test_after_update_trigger**

```ocaml
let test_after_update_trigger () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* () = exec db "CREATE TABLE t (id INTEGER, val TEXT)" in
    let* () = exec db "CREATE TABLE changes (t_id INTEGER, old_val TEXT, new_val TEXT)" in
    let* () = exec db "INSERT INTO t VALUES (1, 'original')" in
    let* () = exec db
      "CREATE TRIGGER t_au AFTER UPDATE ON t BEGIN \
       INSERT INTO changes VALUES (OLD.id, OLD.val, NEW.val); END" in
    let* () = exec db "UPDATE t SET val = 'updated' WHERE id = 1" in
    let* rows = query_rows db "SELECT t_id, old_val, new_val FROM changes" in
    Alcotest.(check int)    "one change row" 1 (List.length rows);
    Alcotest.(check int)    "t_id = 1"       1 (int_val rows.(0).(0));
    Alcotest.(check string) "old_val"        "original" (text_val rows.(0).(1));
    Alcotest.(check string) "new_val"        "updated"  (text_val rows.(0).(2));
    Lwt.return_unit
  )
```

- [ ] **Step 5: test_before_insert_trigger_aborts**

BEFORE trigger that raises by executing a failing DML (INSERT into a non-existent table aborts the outer INSERT via exception propagation):

```ocaml
let test_before_insert_trigger_aborts () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* () = exec db "CREATE TABLE t (id INTEGER)" in
    (* BEFORE trigger that fails: INSERT into nonexistent table *)
    let* () = exec db
      "CREATE TRIGGER t_bi BEFORE INSERT ON t BEGIN \
       INSERT INTO no_such_table VALUES (1); END" in
    let* result = Db.execute db "INSERT INTO t VALUES (99)" in
    (match result with
     | Error (Db.Runtime _) -> ()   (* expected: trigger body failed *)
     | Ok () -> Alcotest.fail "expected trigger to abort insert"
     | Error e -> Alcotest.failf "unexpected error: %a" Db.pp_error e);
    (* Verify no row was inserted *)
    let* rows = query_rows db "SELECT id FROM t" in
    Alcotest.(check int) "no rows inserted" 0 (List.length rows);
    Lwt.return_unit
  )
```

- [ ] **Step 6: test_when_clause_conditional**

```ocaml
let test_when_clause_conditional () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* () = exec db "CREATE TABLE t (id INTEGER, score INTEGER)" in
    let* () = exec db "CREATE TABLE high_scores (t_id INTEGER)" in
    let* () = exec db
      "CREATE TRIGGER t_ai_when AFTER INSERT ON t WHEN NEW.score > 100 BEGIN \
       INSERT INTO high_scores VALUES (NEW.id); END" in
    let* () = exec db "INSERT INTO t VALUES (1, 50)"  in   (* score ≤ 100: no audit *)
    let* () = exec db "INSERT INTO t VALUES (2, 150)" in   (* score > 100: audit *)
    let* rows = query_rows db "SELECT t_id FROM high_scores" in
    Alcotest.(check int) "one high score" 1 (List.length rows);
    Alcotest.(check int) "t_id = 2"       2 (int_val rows.(0).(0));
    Lwt.return_unit
  )
```

- [ ] **Step 7: test_drop_trigger**

```ocaml
let test_drop_trigger () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* () = exec db "CREATE TABLE t (id INTEGER)" in
    let* () = exec db "CREATE TABLE audit (id INTEGER)" in
    let* () = exec db
      "CREATE TRIGGER t_ai AFTER INSERT ON t BEGIN \
       INSERT INTO audit VALUES (NEW.id); END" in
    let* () = exec db "INSERT INTO t VALUES (1)" in
    let* () = exec db "DROP TRIGGER t_ai" in
    let* () = exec db "INSERT INTO t VALUES (2)" in   (* no trigger fires *)
    let* rows = query_rows db "SELECT id FROM audit" in
    Alcotest.(check int) "one audit row (from before drop)" 1 (List.length rows);
    Lwt.return_unit
  )
```

- [ ] **Step 8: test_trigger_multiple_body_stmts**

```ocaml
let test_trigger_multiple_body_stmts () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* () = exec db "CREATE TABLE t (id INTEGER)" in
    let* () = exec db "CREATE TABLE log1 (id INTEGER)" in
    let* () = exec db "CREATE TABLE log2 (id INTEGER)" in
    let* () = exec db
      "CREATE TRIGGER t_ai AFTER INSERT ON t BEGIN \
       INSERT INTO log1 VALUES (NEW.id); \
       INSERT INTO log2 VALUES (NEW.id); END" in
    let* () = exec db "INSERT INTO t VALUES (42)" in
    let* r1 = query_rows db "SELECT id FROM log1" in
    let* r2 = query_rows db "SELECT id FROM log2" in
    Alcotest.(check int) "log1 has one row" 1 (List.length r1);
    Alcotest.(check int) "log2 has one row" 1 (List.length r2);
    Alcotest.(check int) "log1 id = 42" 42 (int_val r1.(0).(0));
    Alcotest.(check int) "log2 id = 42" 42 (int_val r2.(0).(0));
    Lwt.return_unit
  )
```

- [ ] **Step 9: Register the suite and run**

In `test/test_e2e.ml`, find the suite registration (the `Alcotest.run "sqlocaml_e2e"` call) and add:

```ocaml
    "triggers", [
      Alcotest.test_case "after_insert" `Quick test_after_insert_trigger;
      Alcotest.test_case "after_insert_two_rows" `Quick test_after_insert_two_rows;
      Alcotest.test_case "after_delete" `Quick test_after_delete_trigger;
      Alcotest.test_case "after_update" `Quick test_after_update_trigger;
      Alcotest.test_case "before_insert_aborts" `Quick test_before_insert_trigger_aborts;
      Alcotest.test_case "when_clause_conditional" `Quick test_when_clause_conditional;
      Alcotest.test_case "drop_trigger" `Quick test_drop_trigger;
      Alcotest.test_case "multiple_body_stmts" `Quick test_trigger_multiple_body_stmts;
    ];
```

- [ ] **Step 10: Run all e2e tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe 2>&1 | tail -20
```

Expected: All 347 tests pass (339 existing + 8 new trigger tests).

- [ ] **Step 11: Commit**

```bash
git add test/test_e2e.ml
git commit -m "feat(phase22): e2e tests for BEFORE/AFTER INSERT/UPDATE/DELETE triggers and WHEN clause [#129]"
```

---

## Task 8: SQLite comparison tests

**Files:**
- Modify: `test/test_sqlite_compare.ml`

Note: The sqlite3 binary requires PRAGMA foreign_keys for FK tests, but does NOT require any pragma for triggers. Triggers work identically. We test success-only (non-aborting) scenarios here.

- [ ] **Step 1: Add phase22_trigger_cases**

In `test/test_sqlite_compare.ml`, find the section where comparison suites are registered (near the bottom). Add:

```ocaml
let phase22_trigger_cases = [
  (* AFTER INSERT trigger populates audit table *)
  { setup =
      [ "CREATE TABLE t (id INTEGER, val TEXT)"
      ; "CREATE TABLE audit (t_id INTEGER, action TEXT)"
      ; "CREATE TRIGGER t_ai AFTER INSERT ON t BEGIN \
         INSERT INTO audit VALUES (NEW.id, 'INSERT'); END"
      ; "INSERT INTO t VALUES (1, 'hello')"
      ; "INSERT INTO t VALUES (2, 'world')"
      ];
    query = "SELECT t_id, action FROM audit ORDER BY t_id";
    label = "after_insert_trigger";
  };
  (* AFTER DELETE trigger logs deleted rows *)
  { setup =
      [ "CREATE TABLE t (id INTEGER)"
      ; "CREATE TABLE del_log (old_id INTEGER)"
      ; "INSERT INTO t VALUES (10)"
      ; "INSERT INTO t VALUES (20)"
      ; "CREATE TRIGGER t_ad AFTER DELETE ON t BEGIN \
         INSERT INTO del_log VALUES (OLD.id); END"
      ; "DELETE FROM t WHERE id = 10"
      ];
    query = "SELECT old_id FROM del_log";
    label = "after_delete_trigger";
  };
  (* AFTER UPDATE trigger records changes *)
  { setup =
      [ "CREATE TABLE t (id INTEGER, val TEXT)"
      ; "CREATE TABLE changes (t_id INTEGER, old_val TEXT, new_val TEXT)"
      ; "INSERT INTO t VALUES (1, 'original')"
      ; "CREATE TRIGGER t_au AFTER UPDATE ON t BEGIN \
         INSERT INTO changes VALUES (OLD.id, OLD.val, NEW.val); END"
      ; "UPDATE t SET val = 'updated' WHERE id = 1"
      ];
    query = "SELECT t_id, old_val, new_val FROM changes";
    label = "after_update_trigger";
  };
  (* WHEN clause: only fire trigger when condition is true *)
  { setup =
      [ "CREATE TABLE t (id INTEGER, score INTEGER)"
      ; "CREATE TABLE high_scores (t_id INTEGER)"
      ; "CREATE TRIGGER t_when AFTER INSERT ON t WHEN NEW.score > 100 BEGIN \
         INSERT INTO high_scores VALUES (NEW.id); END"
      ; "INSERT INTO t VALUES (1, 50)"
      ; "INSERT INTO t VALUES (2, 150)"
      ];
    query = "SELECT t_id FROM high_scores";
    label = "trigger_when_clause";
  };
  (* DROP TRIGGER stops trigger from firing *)
  { setup =
      [ "CREATE TABLE t (id INTEGER)"
      ; "CREATE TABLE audit (id INTEGER)"
      ; "CREATE TRIGGER t_ai AFTER INSERT ON t BEGIN \
         INSERT INTO audit VALUES (NEW.id); END"
      ; "INSERT INTO t VALUES (1)"
      ; "DROP TRIGGER t_ai"
      ; "INSERT INTO t VALUES (2)"
      ];
    query = "SELECT id FROM audit ORDER BY id";
    label = "drop_trigger";
  };
]
```

- [ ] **Step 2: Register the suite**

In the list of registered suites (where `phase21_savepoint_cases` etc. are registered), add:

```ocaml
    ("phase22_trigger", phase22_trigger_cases);
```

- [ ] **Step 3: Run comparison tests**

```bash
podman run --rm \
  -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
  -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
  -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
  -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
  -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe 2>&1 | tail -20
```

Expected: All 5 new phase22 trigger cases pass, plus all prior passing cases still pass (326 existing + 5 new = 331 passing). The 12 pre-existing failures are unchanged.

- [ ] **Step 4: Commit**

```bash
git add test/test_sqlite_compare.ml
git commit -m "feat(phase22): SQLite comparison tests for AFTER INSERT/DELETE/UPDATE triggers, WHEN clause, DROP TRIGGER [#129]"
```

---

## Self-Review

### Spec coverage

| Spec requirement | Task |
|-----------------|------|
| `AFTER INSERT` triggers | Task 7 test_after_insert_trigger |
| `AFTER UPDATE` triggers | Task 7 test_after_update_trigger |
| `AFTER DELETE` triggers | Task 7 test_after_delete_trigger |
| `BEFORE` triggers (validation/abort) | Task 7 test_before_insert_trigger_aborts |
| `NEW` pseudo-table (INSERT, UPDATE) | Tasks 6+7 — subst_new_old handles `E_tbl_col("NEW", col)` |
| `OLD` pseudo-table (DELETE, UPDATE) | Tasks 6+7 — subst_new_old handles `E_tbl_col("OLD", col)` |
| `WHEN` clause (conditional trigger) | Tasks 2+6+7 — parser + eval_when_clause |
| `DROP TRIGGER` | Task 7 test_drop_trigger |
| Persistence (survive open_file) | Task 4 — sys_triggers_tid + load_triggers_into_hashtbl |

Out of scope (Phase 23+): INSTEAD OF triggers on views, recursive triggers.

### Known limitations (acceptable)

- `FOR EACH ROW` syntax not supported in parser (sqlite3 does not require it either)
- NEW/OLD references in subquery bodies within trigger stmts are not substituted (edge case)
- RAISE() function not implemented — BEFORE triggers abort by raising OCaml exceptions
- Triggers are NOT atomic with DML in the B-tree backend (same as views); DDL is auto-committed

### Placeholder scan

No TBD, TODO, "add validation", or "handle edge cases" placeholders in this plan. Every step has concrete code.

### Type consistency

- `trigger_timing` in `ast.ml` = `trigger_timing` in `plan.ml` = same type piped through
- `trig_timing : [ `Before | `After ]` in `db.ml`'s `trigger_meta` (polymorphic variant, consistent across `make_trigger_hook` and `fire_triggers`)
- `new_row : Row.t option` / `old_row : Row.t option` type is consistent through exec.ml hooks and db.ml fire_triggers
