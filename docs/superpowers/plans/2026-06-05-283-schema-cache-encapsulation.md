# #283 Schema_cache Encapsulation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make "mutate the catalog cache without registering its reversal" structurally impossible by sealing the three cache hashtables and the undo log inside a signature-ascribed `Schema_cache` module nested in `catalog.ml`.

**Architecture:** A nested `module Schema_cache : sig … end = struct … end` privately owns `tables`/`indexes`/`fts` plus the undo log, savepoint stack, poison flag, and rowid-dirty set. Its only mutators capture the prior binding and synthesize their own inverse (undo-tracked variants) or are explicitly durable (autocommit/ephemeral). `Catalog.t` embeds one `sc : Schema_cache.t` field; every catalog function routes cache access through it. Pure refactor — zero observable behavior change.

**Tech Stack:** OCaml 5.x, Lwt, dune (built inside podman — never call dune on the host), bisect_ppx for coverage.

**Build/test note:** This repo's dune runs inside a podman container, not on the host. Use the project's established container build/test invocation (see CI config / prior session memory). In a git worktree, `chmod 777` the worktree root so the container can write build artifacts. Format via `ocamlformat` stdout redirected to the host file; verify with `ocamlformat --check` (do NOT rely on `@fmt`/`dune fmt` exit codes — a recurring trap, #213/#277).

**Spec:** `docs/superpowers/specs/2026-06-05-283-schema-cache-encapsulation-design.md`

---

## File Structure

- **Modify** `lib/catalog/catalog.ml` — add the `Schema_cache` nested module; migrate every cache access (`Hashtbl.* t.cache/t.indexes/t.fts`) and every undo/savepoint/poison/rowid operation to route through `t.sc`; delete the now-internal `register_schema_undo`, `restore_table_cache`, `restore_index_cache`.
- **Modify** `lib/catalog/catalog.mli` — delete the public `register_schema_undo`, `restore_table_cache`, `restore_index_cache` declarations; keep the rest (the lifecycle functions stay, now delegating).
- **Modify** `lib/sql/exec.ml` — delete the external `register_schema_undo` blocks in `execute_drop_table` (~`5301`) and `execute_drop_index` (~`5320`); `drop_table`/`drop_index` now self-register.
- **Create** `test/test_schema_cache_283.ml` — public-API rollback round-trip guards.
- **Modify** `test/dune` (or the relevant test stanza) — register the new test executable, following the existing pattern used by `test_ddl_txn_269`.

---

## Task 1: Add the sealed `Schema_cache` module to `catalog.ml`

**Files:**
- Modify: `lib/catalog/catalog.ml` (insert after the `fts_table_meta` type decl at `catalog.ml:125`, before `type t` at `catalog.ml:127`)

This task only ADDS the module. It is unused after this task, so the build stays green and the suite is unaffected. Reviewable in isolation.

- [ ] **Step 1: Insert the module**

Insert the following immediately before `type t = {` (currently `catalog.ml:127`). It references `table_meta`, `index_info`, `fts_table_meta` (defined just above) and nothing defined later.

```ocaml
(* #283: the in-memory schema cache and its rollback ledger, sealed behind a
   signature so the ONLY way to mutate the three catalog hashtables is through a
   mutator that registers its own reversal.  "Mutate the cache without recording
   how to undo it" is therefore unrepresentable outside this module.

   Two reversal strategies, both explicit (no raw, unprotected write exists):
   - undo-tracked mutators ([put_*]/[remove_*]) capture the prior binding and push
     the synthesized inverse onto [undo]; replayed by [rollback]/[savepoint_rollback],
     discarded by [commit].  Used for DDL run THROUGH [Exec.with_ddl_txn] (the undo is
     discarded on COMMIT in either Auto or explicit mode, replayed on ROLLBACK / a
     mid-statement failure — see exec.ml).
   - durable mutators ([*_durable]) apply with NO undo, for a catalog function's own
     autocommit path that self-commits its own writer txn (so the write is already
     durable and a [?txn=None] branch can never be inside an ambient writer txn — a
     nested rw_begin would deadlock).  Also used for ephemeral CTE sentinels.

   The rowid counter keeps the #293 recompute-on-rollback strategy: [bump_rowid]
   records the table in the dirty set instead of pushing a closure, and the db layer
   re-derives max(rowid)+1 from the rolled-back tree for exactly those tables. *)
module Schema_cache : sig
  type t

  (** [stamp] re-stamps the #174 tree-tag for a [table_meta]; wired to
      [register_tag store].  Every [table_meta] entering the cache is stamped so the
      page-stamp stays consistent automatically, and an undo re-stamps the prior. *)
  val create : stamp:(table_meta -> unit) -> t

  (* reads — never touch the undo log *)
  val find_table : t -> string -> table_meta option
  val mem_table : t -> string -> bool
  val find_index : t -> string -> index_info option
  val mem_index : t -> string -> bool
  val find_fts : t -> string -> fts_table_meta option
  val fold_tables : (string -> table_meta -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_indexes : (string -> index_info -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_fts : (string -> fts_table_meta -> 'a -> 'a) -> t -> 'a -> 'a
  val count_tables : t -> int
  val count_indexes : t -> int
  val count_fts : t -> int

  (* undo-tracked mutators (DDL under with_ddl_txn) *)
  val put_table : t -> name:string -> table_meta -> unit
  val remove_table : t -> name:string -> unit
  val put_index : t -> name:string -> index_info -> unit
  val remove_index : t -> name:string -> unit
  val put_fts : t -> name:string -> fts_table_meta -> unit
  val remove_fts : t -> name:string -> unit

  (* durable mutators (catalog-internal autocommit / ephemeral — no undo) *)
  val put_table_durable : t -> name:string -> table_meta -> unit
  val remove_table_durable : t -> name:string -> unit
  val put_index_durable : t -> name:string -> index_info -> unit
  val put_fts_durable : t -> name:string -> fts_table_meta -> unit

  (* rowid counter: in-txn bump (dirty-set tracked) and post-rollback/autocommit
     durable set; [take_rowid_bumped] returns the dirty names and clears the set. *)
  val bump_rowid : t -> name:string -> table_meta -> unit
  val set_rowid_durable : t -> name:string -> table_meta -> unit
  val take_rowid_bumped : t -> string list

  (* lifecycle — drive by the db layer at txn / savepoint boundaries *)
  val commit : t -> unit
  val rollback : t -> unit
  val savepoint_begin : t -> string -> unit
  val savepoint_rollback : t -> string -> unit
  val savepoint_release : t -> string -> unit
  val mark_poisoned : t -> unit
  val is_poisoned : t -> bool
end = struct
  type t =
    { tables : (string, table_meta) Hashtbl.t
    ; indexes : (string, index_info) Hashtbl.t
    ; fts : (string, fts_table_meta) Hashtbl.t
    ; stamp : table_meta -> unit
    ; mutable undo : (unit -> unit) list
    ; mutable savepoints : (string * (unit -> unit) list * bool) list
    ; mutable poisoned : bool
    ; rowid_bumped : (string, unit) Hashtbl.t
    }

  let create ~stamp =
    { tables = Hashtbl.create 16
    ; indexes = Hashtbl.create 16
    ; fts = Hashtbl.create 8
    ; stamp
    ; undo = []
    ; savepoints = []
    ; poisoned = false
    ; rowid_bumped = Hashtbl.create 8
    }
  ;;

  (* The undo log only ever grows by prepending, so a saved suffix stays
     physically identical (==) — the invariant [savepoint_rollback] relies on. *)
  let push_undo t f = t.undo <- f :: t.undo

  let find_table t name = Hashtbl.find_opt t.tables name
  let mem_table t name = Hashtbl.mem t.tables name
  let find_index t name = Hashtbl.find_opt t.indexes name
  let mem_index t name = Hashtbl.mem t.indexes name
  let find_fts t name = Hashtbl.find_opt t.fts name
  let fold_tables f t acc = Hashtbl.fold f t.tables acc
  let fold_indexes f t acc = Hashtbl.fold f t.indexes acc
  let fold_fts f t acc = Hashtbl.fold f t.fts acc
  let count_tables t = Hashtbl.length t.tables
  let count_indexes t = Hashtbl.length t.indexes
  let count_fts t = Hashtbl.length t.fts

  let put_table t ~name meta =
    let prior = Hashtbl.find_opt t.tables name in
    Hashtbl.replace t.tables name meta;
    t.stamp meta;
    push_undo t (fun () ->
      match prior with
      | Some m ->
        Hashtbl.replace t.tables name m;
        t.stamp m
      | None -> Hashtbl.remove t.tables name)
  ;;

  let remove_table t ~name =
    let prior = Hashtbl.find_opt t.tables name in
    Hashtbl.remove t.tables name;
    push_undo t (fun () ->
      match prior with
      | Some m ->
        Hashtbl.replace t.tables name m;
        t.stamp m
      | None -> ())
  ;;

  let put_index t ~name info =
    let prior = Hashtbl.find_opt t.indexes name in
    Hashtbl.replace t.indexes name info;
    push_undo t (fun () ->
      match prior with
      | Some i -> Hashtbl.replace t.indexes name i
      | None -> Hashtbl.remove t.indexes name)
  ;;

  let remove_index t ~name =
    let prior = Hashtbl.find_opt t.indexes name in
    Hashtbl.remove t.indexes name;
    push_undo t (fun () ->
      match prior with
      | Some i -> Hashtbl.replace t.indexes name i
      | None -> ())
  ;;

  let put_fts t ~name meta =
    let prior = Hashtbl.find_opt t.fts name in
    Hashtbl.replace t.fts name meta;
    push_undo t (fun () ->
      match prior with
      | Some m -> Hashtbl.replace t.fts name m
      | None -> Hashtbl.remove t.fts name)
  ;;

  let remove_fts t ~name =
    let prior = Hashtbl.find_opt t.fts name in
    Hashtbl.remove t.fts name;
    push_undo t (fun () ->
      match prior with
      | Some m -> Hashtbl.replace t.fts name m
      | None -> ())
  ;;

  let put_table_durable t ~name meta =
    Hashtbl.replace t.tables name meta;
    t.stamp meta
  ;;

  let remove_table_durable t ~name = Hashtbl.remove t.tables name
  let put_index_durable t ~name info = Hashtbl.replace t.indexes name info
  let put_fts_durable t ~name meta = Hashtbl.replace t.fts name meta

  let bump_rowid t ~name meta =
    Hashtbl.replace t.tables name meta;
    Hashtbl.replace t.rowid_bumped name ()
  ;;

  let set_rowid_durable t ~name meta = Hashtbl.replace t.tables name meta

  let take_rowid_bumped t =
    let names = Hashtbl.fold (fun k _ acc -> k :: acc) t.rowid_bumped [] in
    Hashtbl.reset t.rowid_bumped;
    names
  ;;

  let commit t =
    t.undo <- [];
    t.savepoints <- [];
    t.poisoned <- false;
    (* #293: COMMIT keeps the bumped next_rowid counter but clears the dirty set so
       a later unrelated ROLLBACK won't recompute a table not bumped in that txn. *)
    Hashtbl.reset t.rowid_bumped
  ;;

  let rollback t =
    List.iter (fun f -> f ()) t.undo;
    t.undo <- [];
    t.savepoints <- [];
    t.poisoned <- false
    (* #293: [rowid_bumped] is intentionally NOT cleared here — the db layer calls
       the recompute step right after, which reads it via [take_rowid_bumped]. *)
  ;;

  let savepoint_begin t name = t.savepoints <- (name, t.undo, t.poisoned) :: t.savepoints

  let savepoint_rollback t name =
    let rec find = function
      | [] -> None
      | (n, snap, poison) :: older when String.equal n name -> Some (snap, poison, older)
      | _ :: rest -> find rest
    in
    match find t.savepoints with
    | None -> ()
    | Some (snap, poison, older) ->
      let rec run lst =
        if lst == snap
        then ()
        else (
          match lst with
          | [] -> ()
          | f :: tl ->
            f ();
            run tl)
      in
      run t.undo;
      t.undo <- snap;
      t.poisoned <- poison;
      t.savepoints <- (name, snap, poison) :: older
  ;;

  let savepoint_release t name =
    let rec drop = function
      | [] -> []
      | (n, _, _) :: older when String.equal n name -> older
      | _ :: rest -> drop rest
    in
    t.savepoints <- drop t.savepoints
  ;;

  let mark_poisoned t = t.poisoned <- true
  let is_poisoned t = t.poisoned
end
```

- [ ] **Step 2: Build to verify the module compiles (unused is fine)**

Run the project's container build. Expected: builds clean (a top-level unused module binding does not warn). If an "unused" warning surfaces, it is resolved in Task 2 when the module is wired in — do not silence it.

- [ ] **Step 3: Commit**

```bash
git add lib/catalog/catalog.ml
git commit -m "feat(#283): add sealed Schema_cache module (unwired)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: Wire `Catalog.t` to `Schema_cache` and migrate every cache access

**Files:**
- Modify: `lib/catalog/catalog.ml` (the `type t`, the catalog `create`/init function, and every function listed below)

This is the core atomic change: catalog.ml compiles only once every reference is migrated. Work top-to-bottom, then build.

- [ ] **Step 1: Replace the four moved fields in `type t` with a single `sc`**

In `type t` (`catalog.ml:127-188`) delete the fields `cache`, `indexes`, `fts`, `schema_undo`, `schema_savepoints`, `schema_txn_poisoned`, `rowid_bumped_in_txn` and their doc comments. Add in their place:

```ocaml
  ; sc : Schema_cache.t
    (** #283: the sealed in-memory schema cache (tables/indexes/fts) and its
        rollback ledger.  The only path to a cache mutation, so a write that does
        not record its reversal is unrepresentable. *)
```

Keep `store`, `fk_enforcement`, `recursive_triggers`, `defer_fks_pragma`, `pending_fk_checks`, `last_inserted_rowid` unchanged.

- [ ] **Step 2: Build the `sc` field in the catalog constructor**

Find the function that constructs `Catalog.t` (the `create`/`open`/`make` that currently writes `cache = Hashtbl.create …; indexes = …; fts = …; schema_undo = []; …`). `register_tag` is defined at `catalog.ml:990`, before this constructor, so the closure resolves. Replace those field initializers with:

```ocaml
  ; sc = Schema_cache.create ~stamp:(fun m -> register_tag store m)
```

(Use the in-scope name for the store binding — it is `store` in the constructor; confirm and match it.)

- [ ] **Step 3: Migrate `pp`** (`catalog.ml:190`)

```ocaml
let pp fmt t =
  Format.fprintf
    fmt
    "@[<hv>Catalog.t { tables = %d;@ indexes = %d;@ fts = %d;@ fk_enforcement = %b }@]"
    (Schema_cache.count_tables t.sc)
    (Schema_cache.count_indexes t.sc)
    (Schema_cache.count_fts t.sc)
    t.fk_enforcement
;;
```

- [ ] **Step 4: Delete the now-internal undo primitive and delegate the lifecycle functions**

Delete `register_schema_undo` (`catalog.ml:1263`). Replace the bodies of the lifecycle functions:

```ocaml
let mark_schema_txn_poisoned t = Schema_cache.mark_poisoned t.sc
let schema_txn_poisoned t = Schema_cache.is_poisoned t.sc
let commit_schema_changes t = Schema_cache.commit t.sc
let rollback_schema_changes t = Schema_cache.rollback t.sc
let savepoint_begin_schema t name = Schema_cache.savepoint_begin t.sc name
let savepoint_rollback_schema t name = Schema_cache.savepoint_rollback t.sc name
let savepoint_release_schema t name = Schema_cache.savepoint_release t.sc name
```

(Preserve the existing doc comments above each — they still describe the observable semantics.)

- [ ] **Step 5: Migrate `recompute_rowid_counters_after_rollback`** (`catalog.ml:1324`)

```ocaml
let recompute_rowid_counters_after_rollback t =
  let names = Schema_cache.take_rowid_bumped t.sc in
  Lwt_list.iter_s
    (fun name ->
       match Schema_cache.find_table t.sc name with
       | None -> Lwt.return_unit
       | Some m when m.without_rowid -> Lwt.return_unit
       | Some m ->
         let%lwt recovered = recover_next_rowid t.store m in
         Schema_cache.set_rowid_durable t.sc ~name recovered;
         Lwt.return_unit)
    names
;;
```

- [ ] **Step 6: Migrate `create_table`** (`catalog.ml:1431`)

```ocaml
let create_table ?txn t ~name ~columns ~without_rowid =
  if Schema_cache.mem_table t.sc name
  then failwith (Printf.sprintf "table '%s' already exists" name);
  match txn with
  | Some tx ->
    let%lwt tid = next_user_tid_tx tx in
    let%lwt m = put_table_rows tx ~name ~columns ~without_rowid ~tid in
    Schema_cache.put_table t.sc ~name m;
    Lwt.return tid
  | None ->
    let%lwt tid = next_user_tid t in
    let%lwt tx = S.rw_begin t.store in
    let%lwt m = put_table_rows tx ~name ~columns ~without_rowid ~tid in
    let%lwt () = S.commit tx in
    Schema_cache.put_table_durable t.sc ~name m;
    Lwt.return tid
;;
```

(`Schema_cache.put_table`/`put_table_durable` re-stamp the tree-tag, replacing the old `register_tag` + manual undo. The #174 reasoning at the old `catalog.ml:1439-1445` still holds: a rolled-back create leaves an unreferenced tag, which cannot mis-stamp any page.)

- [ ] **Step 7: Migrate the table reads near `create_table`**

```ocaml
let find_table t ~name = Lwt.return (Schema_cache.find_table t.sc name)
let find_table_cached t ~name = Schema_cache.find_table t.sc name

let table_fingerprint t ~name =
  Option.map fingerprint_of_meta (Schema_cache.find_table t.sc name)
;;

let fingerprints_by_tree_id t =
  Schema_cache.fold_tables
    (fun _ (m : table_meta) acc ->
       if m.tree_id >= 0 then (m.tree_id, fingerprint_of_meta m) :: acc else acc)
    t.sc
    []
;;
```

- [ ] **Step 8: Migrate `register_ephemeral` / `unregister_ephemeral` / `list_tables`** (`catalog.ml:1530-1532`)

```ocaml
let register_ephemeral t (meta : table_meta) =
  Schema_cache.put_table_durable t.sc ~name:meta.name meta
;;

let unregister_ephemeral t ~name = Schema_cache.remove_table_durable t.sc ~name
let list_tables t = Lwt.return (Schema_cache.fold_tables (fun _ v acc -> v :: acc) t.sc [])
```

- [ ] **Step 9: Migrate the rowid functions** (`catalog.ml:1547-1602`)

`next_rowid` (autocommit, self-commits — durable):

```ocaml
let next_rowid t ~name =
  match Schema_cache.find_table t.sc name with
  | None -> failwith (Printf.sprintf "no table '%s'" name)
  | Some m ->
    let id, next = alloc_rowid m in
    let m' = { m with next_rowid = next } in
    let%lwt tx = S.rw_begin t.store in
    let%lwt () = S.put tx sys_tables_tid (Bytes.of_string name) (encode_table_value m') in
    let%lwt () = S.commit tx in
    Schema_cache.set_rowid_durable t.sc ~name m';
    Lwt.return id
;;
```

`next_rowid_in_txn` (in-txn — dirty-set tracked):

```ocaml
let next_rowid_in_txn t ~name (tx : S.rw S.txn) =
  match Schema_cache.find_table t.sc name with
  | None -> failwith (Printf.sprintf "no table '%s'" name)
  | Some m ->
    let id, next = alloc_rowid m in
    let m' = { m with next_rowid = next } in
    Schema_cache.bump_rowid t.sc ~name m';
    let%lwt () = S.put tx sys_tables_tid (Bytes.of_string name) (encode_table_value m') in
    Lwt.return id
;;
```

`bump_next_rowid_in_txn` (in-txn — dirty-set tracked):

```ocaml
let bump_next_rowid_in_txn t ~name ~at_least (tx : S.rw S.txn) =
  match Schema_cache.find_table t.sc name with
  | None -> failwith (Printf.sprintf "no table '%s'" name)
  | Some m ->
    let unseeded = Int64.equal m.next_rowid empty_next_rowid in
    if (not unseeded) && Int64.compare at_least m.next_rowid <= 0
    then Lwt.return_unit
    else (
      let m' = { m with next_rowid = at_least } in
      Schema_cache.bump_rowid t.sc ~name m';
      S.put tx sys_tables_tid (Bytes.of_string name) (encode_table_value m'))
;;
```

- [ ] **Step 10: Migrate `create_index`** (`catalog.ml:1607`)

Replace the duplicate check, the table lookup, and the two cache writes:

- `if Hashtbl.mem t.indexes name` → `if Schema_cache.mem_index t.sc name`
- `match Hashtbl.find_opt t.cache table with` → `match Schema_cache.find_table t.sc table with`
- In the `Some tx` branch, replace `Hashtbl.replace t.indexes name info; register_schema_undo t (fun () -> Hashtbl.remove t.indexes name);` with `Schema_cache.put_index t.sc ~name info;`
- In the `None` branch, replace `Hashtbl.replace t.indexes name info;` with `Schema_cache.put_index_durable t.sc ~name info;`

- [ ] **Step 11: Migrate `add_column`** (`catalog.ml:1670`)

- `match Hashtbl.find_opt t.cache table_name with` → `match Schema_cache.find_table t.sc table_name with`
- Replace the post-write block (`catalog.ml:1690-1697`):
  ```ocaml
        Hashtbl.replace t.cache table_name new_meta;
        register_tag t.store new_meta;
        (match txn with
         | Some _ -> register_schema_undo t (fun () -> Hashtbl.replace t.cache table_name meta; register_tag t.store meta)
         | None -> ());
  ```
  with:
  ```ocaml
        (match txn with
         | Some _ -> Schema_cache.put_table t.sc ~name:table_name new_meta
         | None -> Schema_cache.put_table_durable t.sc ~name:table_name new_meta);
  ```
  (`put_table` re-stamps the tag forward and on undo restores+re-stamps `meta` — exactly the old hand-written closure.)

- [ ] **Step 12: Migrate `indexes_for_table` / `find_index` / `find_index_covering_cols` / `table_exists` / `index_exists`** (`catalog.ml:1701-1767`)

- `indexes_for_table`: `Hashtbl.fold (… ) t.indexes []` → `Schema_cache.fold_indexes (…) t.sc []`
- `find_index t ~name`: `Hashtbl.find_opt t.indexes name` → `Schema_cache.find_index t.sc name`
- `find_index_covering_cols`: `match Hashtbl.find_opt t.cache table_name with` → `match Schema_cache.find_table t.sc table_name with`
- `table_exists`: `Hashtbl.mem t.cache name` → `Schema_cache.mem_table t.sc name`
- `index_exists`: `Hashtbl.mem t.indexes name` → `Schema_cache.mem_index t.sc name`

- [ ] **Step 13: Migrate `drop_index` and delete `restore_index_cache` / `restore_table_cache`** (`catalog.ml:1796-1823`)

`drop_index` always runs under `with_ddl_txn`, so use the undo-tracked mutator:

```ocaml
let drop_index t tx ~name =
  let%lwt key_opt = find_index_key_in_txn tx name in
  let%lwt () =
    match key_opt with
    | None -> Lwt.return_unit
    | Some key -> S.del tx sys_indexes_tid key
  in
  Schema_cache.remove_index t.sc ~name;
  Lwt.return_unit
;;
```

Delete `restore_index_cache` (`catalog.ml:1809-1815`) and `restore_table_cache` (`catalog.ml:1817-1823`) entirely — `remove_index`/`remove_table` now self-register the restore.

- [ ] **Step 14: Migrate `drop_table`** (`catalog.ml:1825`)

Replace the cache lookups and the final cache removal. The mirror/store deletes are unchanged; only the cache accesses change:

- The two `match Hashtbl.find_opt t.cache name with` (for the mirror tree_id and the column count) → `match Schema_cache.find_table t.sc name with`
- The final `Hashtbl.remove t.cache name;` (`catalog.ml:1853`) → `Schema_cache.remove_table t.sc ~name;`

(`indexes_for_table` it calls is already migrated; the per-index `drop_index` it loops over now self-registers each index restore.)

- [ ] **Step 15: Migrate `finish_rename` and `rename_table`** (`catalog.ml:1889-1978`)

In `finish_rename`, the in-memory cache section (`catalog.ml:1921-1942`) currently removes old, replaces new, updates index back-refs, and registers a combined undo only on the `Some` path. Replace with per-op routing that self-registers:

```ocaml
  (* Update in-memory cache + index back-references. *)
  let to_update =
    Schema_cache.fold_indexes
      (fun k v acc -> if String.equal v.idx_table old_name then (k, v) :: acc else acc)
      t.sc
      []
  in
  (match txn with
   | Some _ ->
     Schema_cache.remove_table t.sc ~name:old_name;
     Schema_cache.put_table t.sc ~name:new_name { meta with name = new_name };
     List.iter
       (fun (k, v) -> Schema_cache.put_index t.sc ~name:k { v with idx_table = new_name })
       to_update
   | None ->
     Schema_cache.remove_table_durable t.sc ~name:old_name;
     Schema_cache.put_table_durable t.sc ~name:new_name { meta with name = new_name };
     List.iter
       (fun (k, v) -> Schema_cache.put_index_durable t.sc ~name:k { v with idx_table = new_name })
       to_update);
```

Delete the trailing `register_schema_undo` block (old `catalog.ml:1936-1942`). In `rename_table`, the duplicate/existence checks change: `Hashtbl.find_opt t.cache old_name` → `Schema_cache.find_table t.sc old_name`, and `Hashtbl.mem t.cache new_name` → `Schema_cache.mem_table t.sc new_name`.

(Per-op undo replays most-recent-first: index back-refs restored, then new table removed, then old table restored — reversing the rename exactly. `put_table` re-stamps but a rename leaves the fingerprint unchanged, so the stamp is a no-op.)

- [ ] **Step 16: Migrate `rename_column`** (`catalog.ml:1985`)

- `match Hashtbl.find_opt t.cache table_name with` → `match Schema_cache.find_table t.sc table_name with`
- Replace the `finalize` body (`catalog.ml:2010-2019`):
  ```ocaml
       let finalize new_meta =
         (match txn with
          | Some _ -> Schema_cache.put_table t.sc ~name:table_name new_meta
          | None -> Schema_cache.put_table_durable t.sc ~name:table_name new_meta)
       in
  ```

- [ ] **Step 17: Migrate `drop_column`** (`catalog.ml:2044`)

- `match Hashtbl.find_opt t.cache table_name with` → `match Schema_cache.find_table t.sc table_name with`
- Replace the post-write block (`catalog.ml:2083-2090`):
  ```ocaml
       (match txn with
        | Some _ -> Schema_cache.put_table t.sc ~name:table_name new_meta
        | None -> Schema_cache.put_table_durable t.sc ~name:table_name new_meta);
  ```

- [ ] **Step 18: Migrate `find_fts` / `list_fts_tables` / `create_fts_table`** (`catalog.ml:2098-2145`)

- `find_fts`: `Hashtbl.find_opt t.fts name` → `Schema_cache.find_fts t.sc name`
- `list_fts_tables`: `Hashtbl.fold (…) t.fts []` → `Schema_cache.fold_fts (…) t.sc []`
- `create_fts_table` `Some tx` branch: replace `Hashtbl.replace t.fts name meta; register_schema_undo t (fun () -> Hashtbl.remove t.fts name);` with `Schema_cache.put_fts t.sc ~name meta;`
- `create_fts_table` `None` branch: replace `Hashtbl.replace t.fts name meta;` with `Schema_cache.put_fts_durable t.sc ~name meta;`

- [ ] **Step 19: Sweep for any remaining direct field access**

Run a search for stragglers and fix any hit the same way (read → `find/fold/mem`, in-txn write → `put/remove`, autocommit write → `*_durable`):

Run: `grep -n "t\.cache\|t\.indexes\|t\.fts\|t\.schema_undo\|t\.schema_savepoints\|t\.schema_txn_poisoned\|t\.rowid_bumped_in_txn\|register_schema_undo\|restore_table_cache\|restore_index_cache" lib/catalog/catalog.ml`
Expected: no matches (other than inside the `Schema_cache` module, which uses its own field names `t.tables`/`t.undo`/etc., not these).

- [ ] **Step 20: Build**

Run the container build. Expected: compiles clean, no unused-value/field warnings. Fix any type errors by matching the migrated signatures above.

- [ ] **Step 21: Run the full test suite**

Run the project's container test invocation for the whole suite. Expected: all pass — in particular `test_ddl_txn_269`, the #279 DROP-undo cases, the #280/#295 savepoint cases, and the #293 rowid-rollback cases. No assertion changes are permitted; a failure here means the migration changed behavior — debug before proceeding (use superpowers:systematic-debugging).

- [ ] **Step 22: Commit**

```bash
git add lib/catalog/catalog.ml
git commit -m "refactor(#283): route all catalog cache mutations through sealed Schema_cache

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: Remove dead public API and the external undo blocks

**Files:**
- Modify: `lib/catalog/catalog.mli`
- Modify: `lib/sql/exec.ml`

- [ ] **Step 1: Delete dead declarations from `catalog.mli`**

Delete the `val register_schema_undo : …` (`catalog.mli:415`), `val restore_index_cache : …` (`catalog.mli:296`), and `val restore_table_cache : …` (`catalog.mli:302`) declarations together with their doc comments. Keep `commit_schema_changes`, `rollback_schema_changes`, `savepoint_*_schema`, `mark_schema_txn_poisoned`, `schema_txn_poisoned`, `recompute_rowid_counters_after_rollback`, `register_ephemeral`, `unregister_ephemeral` — they remain part of the interface.

- [ ] **Step 2: Delete the external undo block in `execute_drop_table`** (`exec.ml:5288-5304`)

Replace the body so it no longer snapshots `restore_meta`/`dropped_idxs` or calls `register_schema_undo` — `Cat.drop_table` self-registers now:

```ocaml
  with_ddl_txn store cat mode (fun tx ->
    let name = table_meta.Cat.name in
    Cat.drop_table cat tx ~name)
```

(`table_meta` and `_indexes` parameters stay in the signature; `_indexes` is already unused-prefixed, and `table_meta` is still used for `.name`.)

- [ ] **Step 3: Delete the external undo block in `execute_drop_index`** (`exec.ml:5319-5321`)

```ocaml
  with_ddl_txn store cat mode (fun tx ->
    Cat.drop_index cat tx ~name:idx_info.Cat.idx_name)
```

- [ ] **Step 4: Build**

Run the container build. Expected: compiles clean. If `Cat.restore_table_cache`/`restore_index_cache`/`register_schema_undo` are referenced anywhere else, the build will name the site — migrate it the same way (it should not be, given the sweep, but confirm).

- [ ] **Step 5: Run the full test suite**

Expected: all pass (same set as Task 2 Step 21).

- [ ] **Step 6: Commit**

```bash
git add lib/catalog/catalog.mli lib/sql/exec.ml
git commit -m "refactor(#283): drop dead cache-undo API; drop_table/index self-register

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 4: Add public-API rollback round-trip guards

**Files:**
- Create: `test/test_schema_cache_283.ml`
- Modify: `test/dune` (register the executable, mirroring `test_ddl_txn_269`)

These exercise cache⇄store agreement after `ROLLBACK` / `ROLLBACK TO SAVEPOINT` purely through the SQL/`Db` API. They pass on the pre-refactor code too (behavior is unchanged), so they are regression guards rather than red-green drivers.

- [ ] **Step 1: Read the existing DDL-txn test to match harness conventions**

Read `test/test_ddl_txn_269.ml` to copy its exact open/setup helpers (how it opens an in-memory/temp `Db`, runs SQL, asserts, and its Lwt driver + Alcotest/registration style). Use the SAME helpers and registration pattern — do not invent a new harness.

- [ ] **Step 2: Write the test cases**

Using that harness, implement these cases (adapt the helper names to the ones found in Step 1; the assertions below are the contract):

1. `create_table_rollback`: `BEGIN; CREATE TABLE t(a); ROLLBACK;` → `SELECT` from `t` errors "no such table" AND a fresh `CREATE TABLE t(a)` succeeds (name free in cache).
2. `drop_table_with_index_rollback`: setup `CREATE TABLE t(a); CREATE INDEX i ON t(a);` committed; then `BEGIN; DROP TABLE t; ROLLBACK;` → `t` and `i` are both back: `INSERT INTO t …` works and the index is listed by `indexes_for_table` semantics (assert via a query that would use it / via `PRAGMA`/catalog introspection the harness exposes; if none, assert `SELECT` works and a duplicate `CREATE INDEX i` fails because it still exists).
3. `create_drop_index_rollback`: `BEGIN; CREATE INDEX i ON t(a); ROLLBACK;` → `i` absent (re-create succeeds); and committed `i` then `BEGIN; DROP INDEX i; ROLLBACK;` → `i` present (re-create fails as duplicate).
4. `alter_add_drop_rename_column_rollback`: for each of `ADD COLUMN`, `DROP COLUMN`, `RENAME COLUMN` inside `BEGIN … ROLLBACK`, assert the original column set round-trips (a `SELECT` naming the original columns succeeds and one naming the altered column fails, post-rollback).
5. `savepoint_partial_rollback`: `BEGIN; CREATE TABLE a(x); SAVEPOINT s; CREATE TABLE b(y); ROLLBACK TO s;` then `COMMIT;` → `a` exists, `b` does not.
6. `rowid_reuse_after_rollback`: `BEGIN; CREATE TABLE t(a); INSERT INTO t DEFAULT VALUES (or INSERT a row); ROLLBACK;` is covered by #293 already — instead assert the in-txn variant on a committed table: `CREATE TABLE t(a)` committed; `BEGIN; INSERT INTO t(a) VALUES(1); ROLLBACK;` then `INSERT INTO t(a) VALUES(2)` and assert the new row's `rowid` is `1` (reused), matching SQLite.

- [ ] **Step 3: Register in `test/dune`**

Add a test stanza for `test_schema_cache_283` mirroring the `test_ddl_txn_269` stanza (same libraries, same `(test …)`/`(executable …)` form the file uses).

- [ ] **Step 4: Build and run the new test**

Run the container build, then run only `test_schema_cache_283`. Expected: all 6 cases pass.

- [ ] **Step 5: Run the full suite once more**

Expected: everything green, new test included.

- [ ] **Step 6: Commit**

```bash
git add test/test_schema_cache_283.ml test/dune
git commit -m "test(#283): public-API rollback round-trip guards for Schema_cache

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 5: Format, final verification, PR

- [ ] **Step 1: Format the changed files**

For each changed `.ml`/`.mli`, run `ocamlformat` and redirect stdout to the host file, then verify with `ocamlformat --check` (do not trust `dune fmt` exit codes). Files: `lib/catalog/catalog.ml`, `lib/catalog/catalog.mli`, `lib/sql/exec.ml`, `test/test_schema_cache_283.ml`.

- [ ] **Step 2: Final full build + full suite**

Run the complete container build and the entire test suite. Expected: clean build, all tests pass. Capture the pass count for the PR body.

- [ ] **Step 3: Verify the structural guarantee by inspection**

Run: `grep -n "Hashtbl\.\(replace\|remove\|add\|find\|mem\|fold\)" lib/catalog/catalog.ml | grep -i "cache\|index\|fts\|tables"`
Expected: every hit is INSIDE the `Schema_cache` module (operating on its private `tables`/`indexes`/`fts`). No catalog function outside the module touches a cache hashtable directly — this is the deliverable.

- [ ] **Step 4: Commit any format changes**

```bash
git add -A && git commit -m "style(#283): ocamlformat

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

- [ ] **Step 5: Push and open the PR**

Push the branch and open a PR via the Forgejo CLI (`~/.local/bin/forgejo pr create tej/sqlite_ocaml_port …`) titled `refactor(#283): seal catalog cache mutations behind Schema_cache`, body summarizing: the sealed module, auto-inverse + tree-tag folding, deleted dead API, the structural guarantee (Step 3 grep), and the green suite count. Reference that it subsumes the cache-undo design half of #279/#280/#282. Do NOT close #283 until the PR merges.

---

## Self-Review Notes

- **Spec coverage:** §1 sealed module → Task 1. §2 auto-inverse + tree-tag fold → Schema_cache `put_table` + Steps 6/11/16/17. §3 durable + rowid → durable variants + `bump_rowid` (Steps 6/9/10/15/18). §4 reads → `find/fold/mem` (Steps 3/7/8/12/18). §5 lifecycle delegation → Step 4/5. §6 call-site deletions → Tasks 2-3. §7 gaps closed → drop_table/index symmetry (Steps 13-14, Task 3) + tag fold. §8 testing → Task 4. Non-goals respected (store-txn idioms untouched; ordering left as convention).
- **Placeholder scan:** the only deliberately harness-relative items are the container build/test command and the Alcotest registration form, both resolved by reading existing files (Task 4 Step 1) — no `TODO`/`TBD` left.
- **Type consistency:** mutator names (`put_table`/`remove_table`/`put_index`/`remove_index`/`put_fts`/`remove_fts`/`*_durable`/`bump_rowid`/`set_rowid_durable`/`take_rowid_bumped`/lifecycle) are used identically in Task 1's signature and every call site in Tasks 2-3.
