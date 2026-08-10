# #576 tier 1: per-index leading-column cardinality statistics — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the planner a real, measured selectivity number for a non-unique index's leading column — computed once at `CREATE INDEX` time — so `estimate_rows` and `build_side_seek_is_unambiguous` stop treating every non-unique equality-prefix seek as `unbounded_rows`/"always decline".

**Architecture:** A new optional `idx_stats` field on `Cat.index_info`, persisted as a version-4 extension of the existing index-metadata encoding. Populated inside `execute_create_index`'s existing full-table walk (no new scan) via a new `Cat.set_index_stats` catalog call. Consumed additively in two `lib/sql/planner.ml` functions, both of which fall back to today's exact behavior whenever `idx_stats = None`.

**Tech Stack:** OCaml, Lwt, the existing B-tree store/catalog/planner/executor stack. No new dependencies.

## Global Constraints

- Repo: `/home/tej/projects/granary`, worktree `.worktrees/576-index-cardinality`, branch `perf/576-index-cardinality-stats`.
- Build/test only via the dev container: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build` / `dune test` / `dune test test/test_foo.exe`. Never call `dune` directly on the host.
- Design doc: `docs/superpowers/specs/2026-08-10-576-tier1-index-cardinality-design.md` — every task below implements one piece of it. Do not widen scope to tier 2 (histograms) or to incremental stats maintenance; both are explicitly out of scope.
- `sh scripts/check-fmt.sh` (and `--fix` to auto-format) must be clean before each commit that touches `.ml`/`.mli` files.
- Commit after each task (bite-sized, working-tree-clean commits), per repo convention: `perf(#576): <what>`.
- `idx_stats = None` must be the exact, unchanged behavior for every code path that doesn't get a stat — this is the invariant that makes the whole change regression-proof. Every task's tests must include a `None`-path assertion, not just the happy path.

---

## File Map

| File | Responsibility |
|---|---|
| `lib/catalog/catalog.mli` / `.ml` | `index_stats` type, `idx_stats` field on `index_info`, version-4 encode/decode, `Cat.set_index_stats` |
| `lib/sql/exec.ml` | Population: accumulate the leading-column distinct-value count inside `execute_create_index`'s walk, call `Cat.set_index_stats` |
| `lib/sql/planner.ml` | Consumption: `index_leading_distinct_count`, `estimate_rows_from_stats` helpers; wire into `estimate_rows` and `build_side_seek_is_unambiguous` |
| `test/test_catalog.ml` | Round-trip encode/decode test (Task 1), `set_index_stats` persistence test (Task 2) |
| `test/test_index_cardinality_576.ml` (new) | Population tests (Task 3), consumption tests (Tasks 4 and 5) |
| `test/dune` | New `(test (name test_index_cardinality_576) ...)` stanza |

---

### Task 1: `index_stats` data model + version-4 encode/decode

**Files:**
- Modify: `lib/catalog/catalog.mli:113-134` (add `index_stats` type, `idx_stats` field)
- Modify: `lib/catalog/catalog.ml:128-142` (same type/field, mirrored)
- Modify: `lib/catalog/catalog.ml:1123-1152` (`encode_index_value`)
- Modify: `lib/catalog/catalog.ml:1154-1254` (`decode_index_ext_fields`, `decode_index_value`)
- Modify: `lib/catalog/catalog.ml:2886-2935` (`create_index`'s `mk_info`, add `idx_stats = None`)
- Test: `test/test_catalog.ml` (new test function, appended near the existing index round-trip tests)

**Interfaces:**
- Produces: `type index_stats = { distinct_count : int; rows_at_analysis : int }`, and `index_info.idx_stats : index_stats option`. Every later task reads/writes this field by name.

- [ ] **Step 1: Add the type and field in `catalog.mli`**

Find the existing block (currently lines ~115-137):

```ocaml
type idx_origin =
  [ `Implicit_pk
  | `Implicit_unique
  | `User
  ]

type index_info =
  { idx_name : string
  ; idx_table : string
  ; idx_columns : string list (* col names for plain; expr SQL for expression indexes *)
  ; idx_unique : bool
  ; idx_tree_id : Granary_store.Store.tree_id
  ; idx_expr_flags : bool list (* true = expression index column, false = plain column *)
  ; idx_where_sql : string option
  ; idx_origin : idx_origin
  }
```

Replace with:

```ocaml
type idx_origin =
  [ `Implicit_pk
  | `Implicit_unique
  | `User
  ]

(** #576 tier 1: the leading indexed column's distinct-value count, as
    measured the one time the index was populated ([CREATE INDEX]).  Never
    incrementally maintained — see the design doc's "Population" section for
    why staleness is accepted rather than tracked. *)
type index_stats =
  { distinct_count : int
  ; rows_at_analysis : int
  }

type index_info =
  { idx_name : string
  ; idx_table : string
  ; idx_columns : string list (* col names for plain; expr SQL for expression indexes *)
  ; idx_unique : bool
  ; idx_tree_id : Granary_store.Store.tree_id
  ; idx_expr_flags : bool list (* true = expression index column, false = plain column *)
  ; idx_where_sql : string option
  ; idx_origin : idx_origin
  ; idx_stats : index_stats option
    (** #576 tier 1: [None] for every index created before this shipped,
        every UNIQUE index (cardinality is definitionally 1 per key), and
        every WITHOUT ROWID / columnar / expression-column index — see
        [Exec.execute_create_index]. *)
  }
```

- [ ] **Step 2: Mirror the same change in `catalog.ml`**

Find (currently lines 128-142):

```ocaml
type idx_origin =
  [ `Implicit_pk
  | `Implicit_unique
  | `User
  ]

type index_info =
  { idx_name : string
  ; idx_table : string
  ; idx_columns : string list (* col names for plain; expr SQL for expression indexes *)
  ; idx_unique : bool
  ; idx_tree_id : S.tree_id
  ; idx_expr_flags : bool list (* true = expression index column, false = plain column *)
  ; idx_where_sql : string option
  ; idx_origin : idx_origin
  }
```

Replace with:

```ocaml
type idx_origin =
  [ `Implicit_pk
  | `Implicit_unique
  | `User
  ]

type index_stats =
  { distinct_count : int
  ; rows_at_analysis : int
  }

type index_info =
  { idx_name : string
  ; idx_table : string
  ; idx_columns : string list (* col names for plain; expr SQL for expression indexes *)
  ; idx_unique : bool
  ; idx_tree_id : S.tree_id
  ; idx_expr_flags : bool list (* true = expression index column, false = plain column *)
  ; idx_where_sql : string option
  ; idx_origin : idx_origin
  ; idx_stats : index_stats option
  }
```

- [ ] **Step 3: Fix the one existing construction site (`create_index`'s `mk_info`)**

In `catalog.ml`, `create_index` (~line 2886), find:

```ocaml
         let mk_info tid =
           { idx_name = name
           ; idx_table = table
           ; idx_columns = columns
           ; idx_unique = unique
           ; idx_tree_id = tid
           ; idx_expr_flags = expr_flags
           ; idx_where_sql = where_sql
           ; idx_origin = origin
           }
         in
```

Replace with:

```ocaml
         let mk_info tid =
           { idx_name = name
           ; idx_table = table
           ; idx_columns = columns
           ; idx_unique = unique
           ; idx_tree_id = tid
           ; idx_expr_flags = expr_flags
           ; idx_where_sql = where_sql
           ; idx_origin = origin
           ; idx_stats = None
           }
         in
```

(This is the ONLY other place in `lib/` or `test/` that constructs a full `index_info` record literal — confirmed by `grep -rn "idx_origin[ ]*=" lib/ test/`. `decode_index_value` is the other one; fixed in Step 5.)

- [ ] **Step 4: Bump `encode_index_value` to version 4**

Find (currently lines 1123-1152):

```ocaml
let encode_index_value (idx : index_info) =
  let buf = Buffer.create 32 in
  Varint.encode_uint64 buf (Int64.of_int (String.length idx.idx_name));
  Buffer.add_string buf idx.idx_name;
  Varint.encode_uint64 buf (Int64.of_int (String.length idx.idx_table));
  Buffer.add_string buf idx.idx_table;
  Varint.encode_uint64 buf (Int64.of_int (List.length idx.idx_columns));
  List.iter
    (fun col ->
       Varint.encode_uint64 buf (Int64.of_int (String.length col));
       Buffer.add_string buf col)
    idx.idx_columns;
  Buffer.add_char buf (if idx.idx_unique then '\x01' else '\x00');
  Varint.encode_uint64 buf (Int64.of_int idx.idx_tree_id);
  (* Extended fields version 3: origin byte + expr flags + optional WHERE *)
  Varint.encode_uint64 buf 3L;
  Buffer.add_char buf (byte_of_idx_origin idx.idx_origin);
  (* One varint per column: 0 = plain column, 1 = expression column *)
  List.iter
    (fun is_expr -> Varint.encode_uint64 buf (if is_expr then 1L else 0L))
    idx.idx_expr_flags;
  (* WHERE clause SQL *)
  (match idx.idx_where_sql with
   | None -> Varint.encode_uint64 buf 0L
   | Some sql ->
     Varint.encode_uint64 buf 1L;
     Varint.encode_uint64 buf (Int64.of_int (String.length sql));
     Buffer.add_string buf sql);
  Buffer.to_bytes buf
;;
```

Replace with:

```ocaml
let encode_index_value (idx : index_info) =
  let buf = Buffer.create 32 in
  Varint.encode_uint64 buf (Int64.of_int (String.length idx.idx_name));
  Buffer.add_string buf idx.idx_name;
  Varint.encode_uint64 buf (Int64.of_int (String.length idx.idx_table));
  Buffer.add_string buf idx.idx_table;
  Varint.encode_uint64 buf (Int64.of_int (List.length idx.idx_columns));
  List.iter
    (fun col ->
       Varint.encode_uint64 buf (Int64.of_int (String.length col));
       Buffer.add_string buf col)
    idx.idx_columns;
  Buffer.add_char buf (if idx.idx_unique then '\x01' else '\x00');
  Varint.encode_uint64 buf (Int64.of_int idx.idx_tree_id);
  (* Extended fields version 4 (#576 tier 1): origin byte + expr flags +
     optional WHERE + optional idx_stats. Versions 1-3 (pre-existing data)
     decode with [idx_stats = None] — see [decode_index_ext_fields]. *)
  Varint.encode_uint64 buf 4L;
  Buffer.add_char buf (byte_of_idx_origin idx.idx_origin);
  (* One varint per column: 0 = plain column, 1 = expression column *)
  List.iter
    (fun is_expr -> Varint.encode_uint64 buf (if is_expr then 1L else 0L))
    idx.idx_expr_flags;
  (* WHERE clause SQL *)
  (match idx.idx_where_sql with
   | None -> Varint.encode_uint64 buf 0L
   | Some sql ->
     Varint.encode_uint64 buf 1L;
     Varint.encode_uint64 buf (Int64.of_int (String.length sql));
     Buffer.add_string buf sql);
  (* #576 tier 1: leading-column cardinality, added in version 4 *)
  (match idx.idx_stats with
   | None -> Buffer.add_char buf '\x00'
   | Some { distinct_count; rows_at_analysis } ->
     Buffer.add_char buf '\x01';
     Varint.encode_uint64 buf (Int64.of_int distinct_count);
     Varint.encode_uint64 buf (Int64.of_int rows_at_analysis));
  Buffer.to_bytes buf
;;
```

- [ ] **Step 5: Extend `decode_index_ext_fields` with a version-4 branch, and thread `idx_stats` through `decode_index_value`**

Find (currently lines 1154-1254):

```ocaml
(* Returns [(expr_flags, where_sql, origin)].  The current encoder always writes
   version 3 (with an explicit origin); the pre-v3 branches default [origin] to
   [`User] — the dump-safe "emit it" choice — since the format is pre-release and
   no v<3 data exists.  Decode of expr flags + WHERE is shared by versions 2/3. *)
let decode_index_ext_fields bytes off2 cols =
  let decode_flags_and_where off_start =
    let off_ref = ref off_start in
    let expr_flags =
      List.map
        (fun _ ->
           let flag, next = Varint.decode_uint64 bytes !off_ref in
           off_ref := next;
           Int64.to_int flag = 1)
        cols
    in
    let has_where, off4 = Varint.decode_uint64 bytes !off_ref in
    let where_sql =
      if Int64.to_int has_where = 0
      then None
      else (
        let sql_len, off5 = Varint.decode_uint64 bytes off4 in
        Some (Bytes.sub_string bytes off5 (Int64.to_int sql_len)))
    in
    expr_flags, where_sql
  in
  if off2 >= Bytes.length bytes
  then List.map (fun _ -> false) cols, None, `User (* old format: no extended fields *)
  else (
    let version, off3 = Varint.decode_uint64 bytes off2 in
    match Int64.to_int version with
    | 1 ->
      (* Version 1 (Task 1): only WHERE clause, no expr flags *)
      let has_where, off4 = Varint.decode_uint64 bytes off3 in
      let where_sql =
        if Int64.to_int has_where = 0
        then None
        else (
          let sql_len, off5 = Varint.decode_uint64 bytes off4 in
          Some (Bytes.sub_string bytes off5 (Int64.to_int sql_len)))
      in
      List.map (fun _ -> false) cols, where_sql, `User
    | 2 ->
      (* Version 2 (Task 2): n_cols expr flags, then WHERE clause *)
      let expr_flags, where_sql = decode_flags_and_where off3 in
      expr_flags, where_sql, `User
    | 3 ->
      (* Version 3 (#273): origin byte, then expr flags, then WHERE clause *)
      let origin = idx_origin_of_byte (Bytes.get_uint8 bytes off3) in
      let expr_flags, where_sql = decode_flags_and_where (off3 + 1) in
      expr_flags, where_sql, origin
    | _ -> List.map (fun _ -> false) cols, None, `User)
;;

let decode_index_value bytes =
  let name_len, off = Varint.decode_uint64 bytes 0 in
  let name_len = Int64.to_int name_len in
  let name = Bytes.sub_string bytes off name_len in
  let off = off + name_len in
  let tbl_len, off = Varint.decode_uint64 bytes off in
  let tbl_len = Int64.to_int tbl_len in
  let tbl = Bytes.sub_string bytes off tbl_len in
  let off = off + tbl_len in
  let n_cols, off = Varint.decode_uint64 bytes off in
  let n_cols = Int64.to_int n_cols in
  if n_cols < 0 then invalid_arg "decode_index_value: negative column count";
  let off = ref off in
  (* #484: explicit recursion rather than [List.init].  Each element consumes
     bytes from [off], so the decoded list's contents depend on the order the
     elements are built in.  [List.init] is documented "evaluated left to
     right" on the toolchain this project pins, but stating the order here
     keeps the decode structural rather than inherited from a stdlib
     guarantee.  The negative-count guard above restores the
     [Invalid_argument] that [List.init] used to raise on a corrupt count;
     [decode_index_value] has no caller that catches a decode failure, so
     losing it would turn a corrupt blob into a silently-wrong [index_info]
     instead of a loud failure at [open_]. *)
  let rec decode_cols remaining acc =
    if remaining <= 0
    then List.rev acc
    else (
      let col_len, next_off = Varint.decode_uint64 bytes !off in
      let col = Bytes.sub_string bytes next_off (Int64.to_int col_len) in
      off := next_off + Int64.to_int col_len;
      decode_cols (remaining - 1) (col :: acc))
  in
  let cols = decode_cols n_cols [] in
  let unique_byte = Bytes.get_uint8 bytes !off in
  let tree_id, off2 = Varint.decode_uint64 bytes (!off + 1) in
  let idx_expr_flags, idx_where_sql, idx_origin =
    decode_index_ext_fields bytes off2 cols
  in
  { idx_name = name
  ; idx_table = tbl
  ; idx_columns = cols
  ; idx_unique = unique_byte <> 0
  ; idx_tree_id = Int64.to_int tree_id
  ; idx_expr_flags
  ; idx_where_sql
  ; idx_origin
  }
;;
```

Replace with:

```ocaml
(* Returns [(expr_flags, where_sql, origin, idx_stats)].  The current encoder
   always writes version 4 (with idx_stats); the pre-v4 branches default
   [idx_stats] to [None] — every index on disk before #576 tier 1 shipped is,
   correctly, "never analyzed". Decode of expr flags + WHERE is shared by
   versions 2/3/4; [decode_flags_and_where] now also returns the offset just
   past the WHERE clause, so version 4 knows where its stats bytes start. *)
let decode_index_ext_fields bytes off2 cols =
  let decode_flags_and_where off_start =
    let off_ref = ref off_start in
    let expr_flags =
      List.map
        (fun _ ->
           let flag, next = Varint.decode_uint64 bytes !off_ref in
           off_ref := next;
           Int64.to_int flag = 1)
        cols
    in
    let has_where, off4 = Varint.decode_uint64 bytes !off_ref in
    let where_sql, off_final =
      if Int64.to_int has_where = 0
      then None, off4
      else (
        let sql_len, off5 = Varint.decode_uint64 bytes off4 in
        let len = Int64.to_int sql_len in
        Some (Bytes.sub_string bytes off5 len), off5 + len)
    in
    expr_flags, where_sql, off_final
  in
  if off2 >= Bytes.length bytes
  then List.map (fun _ -> false) cols, None, `User, None (* old format: no extended fields *)
  else (
    let version, off3 = Varint.decode_uint64 bytes off2 in
    match Int64.to_int version with
    | 1 ->
      (* Version 1 (Task 1): only WHERE clause, no expr flags *)
      let has_where, off4 = Varint.decode_uint64 bytes off3 in
      let where_sql =
        if Int64.to_int has_where = 0
        then None
        else (
          let sql_len, off5 = Varint.decode_uint64 bytes off4 in
          Some (Bytes.sub_string bytes off5 (Int64.to_int sql_len)))
      in
      List.map (fun _ -> false) cols, where_sql, `User, None
    | 2 ->
      (* Version 2 (Task 2): n_cols expr flags, then WHERE clause *)
      let expr_flags, where_sql, _ = decode_flags_and_where off3 in
      expr_flags, where_sql, `User, None
    | 3 ->
      (* Version 3 (#273): origin byte, then expr flags, then WHERE clause *)
      let origin = idx_origin_of_byte (Bytes.get_uint8 bytes off3) in
      let expr_flags, where_sql, _ = decode_flags_and_where (off3 + 1) in
      expr_flags, where_sql, origin, None
    | 4 ->
      (* Version 4 (#576 tier 1): version-3 fields, then optional idx_stats *)
      let origin = idx_origin_of_byte (Bytes.get_uint8 bytes off3) in
      let expr_flags, where_sql, off_after_where = decode_flags_and_where (off3 + 1) in
      let has_stats = Bytes.get_uint8 bytes off_after_where in
      let idx_stats =
        if has_stats = 0
        then None
        else (
          let dc, off_a = Varint.decode_uint64 bytes (off_after_where + 1) in
          let ra, _ = Varint.decode_uint64 bytes off_a in
          Some { distinct_count = Int64.to_int dc; rows_at_analysis = Int64.to_int ra })
      in
      expr_flags, where_sql, origin, idx_stats
    | _ -> List.map (fun _ -> false) cols, None, `User, None)
;;

let decode_index_value bytes =
  let name_len, off = Varint.decode_uint64 bytes 0 in
  let name_len = Int64.to_int name_len in
  let name = Bytes.sub_string bytes off name_len in
  let off = off + name_len in
  let tbl_len, off = Varint.decode_uint64 bytes off in
  let tbl_len = Int64.to_int tbl_len in
  let tbl = Bytes.sub_string bytes off tbl_len in
  let off = off + tbl_len in
  let n_cols, off = Varint.decode_uint64 bytes off in
  let n_cols = Int64.to_int n_cols in
  if n_cols < 0 then invalid_arg "decode_index_value: negative column count";
  let off = ref off in
  (* #484: explicit recursion rather than [List.init].  Each element consumes
     bytes from [off], so the decoded list's contents depend on the order the
     elements are built in.  [List.init] is documented "evaluated left to
     right" on the toolchain this project pins, but stating the order here
     keeps the decode structural rather than inherited from a stdlib
     guarantee.  The negative-count guard above restores the
     [Invalid_argument] that [List.init] used to raise on a corrupt count;
     [decode_index_value] has no caller that catches a decode failure, so
     losing it would turn a corrupt blob into a silently-wrong [index_info]
     instead of a loud failure at [open_]. *)
  let rec decode_cols remaining acc =
    if remaining <= 0
    then List.rev acc
    else (
      let col_len, next_off = Varint.decode_uint64 bytes !off in
      let col = Bytes.sub_string bytes next_off (Int64.to_int col_len) in
      off := next_off + Int64.to_int col_len;
      decode_cols (remaining - 1) (col :: acc))
  in
  let cols = decode_cols n_cols [] in
  let unique_byte = Bytes.get_uint8 bytes !off in
  let tree_id, off2 = Varint.decode_uint64 bytes (!off + 1) in
  let idx_expr_flags, idx_where_sql, idx_origin, idx_stats =
    decode_index_ext_fields bytes off2 cols
  in
  { idx_name = name
  ; idx_table = tbl
  ; idx_columns = cols
  ; idx_unique = unique_byte <> 0
  ; idx_tree_id = Int64.to_int tree_id
  ; idx_expr_flags
  ; idx_where_sql
  ; idx_origin
  ; idx_stats
  }
;;
```

- [ ] **Step 6: Write the round-trip test in `test/test_catalog.ml`**

Add near the existing `test_mirror_roundtrips_columns_fks_and_index` test (search for that name to find the right neighborhood):

```ocaml
(* #576 tier 1: idx_stats round-trips through encode -> disk -> decode
   (version 4), AND an index created before this shipped (no idx_stats bytes
   at all, i.e. whatever `create_index` + no stats-setting call produces)
   still decodes with idx_stats = None. Forcing mirror reconstruction (as
   test_mirror_roundtrips_columns_fks_and_index does) exercises the real
   decode_index_value path rather than the in-memory Schema_cache copy. *)
let test_index_stats_roundtrip_576 () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let* _tid =
       C.create_table
         cat1
         ~name:"t"
         ~columns:[ int_col "a"; int_col "b" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* r =
       C.create_index
         cat1
         ~name:"idx_t_b"
         ~table:"t"
         ~columns:[ "b" ]
         ~unique:false
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     (match r with
      | Ok _ -> ()
      | Error e -> Alcotest.failf "create_index: %s" e);
     (* Not yet analyzed: idx_stats must be None immediately after CREATE INDEX. *)
     (match C.find_index cat1 ~name:"idx_t_b" with
      | None -> Alcotest.fail "index not found"
      | Some i -> Alcotest.(check bool) "unanalyzed index has no stats" true (i.C.idx_stats = None));
     let* () = C.set_index_stats cat1 (run (S.rw_begin store) |> fun x -> x) ~name:"idx_t_b" ~distinct_count:7 ~rows_at_analysis:42 in
     (* Force mirror reconstruction, as test_mirror_roundtrips_columns_fks_and_index does. *)
     let* tx = S.rw_begin store in
     let* () = S.del tx 0 (Bytes.of_string "t") in
     let* () = S.commit tx in
     let* cat2 = C.open_ store in
     (match C.find_index cat2 ~name:"idx_t_b" with
      | None -> Alcotest.fail "index not reconstructed from the mirror"
      | Some i ->
        (match i.C.idx_stats with
         | None -> Alcotest.fail "idx_stats lost across mirror reconstruction"
         | Some s ->
           Alcotest.(check int) "distinct_count round-trips" 7 s.C.distinct_count;
           Alcotest.(check int) "rows_at_analysis round-trips" 42 s.C.rows_at_analysis));
     Lwt.return_unit)
;;
```

Note: this step's `C.set_index_stats` call is a placeholder for Task 2's API and will not compile until Task 2 lands — that is fine, since Task 2 is next and this test is registered (Step 7 below) but not required to pass until then. If your workflow requires every task to leave `dune build` green, instead **stop after Step 5** for this task (data model + encode/decode only, no test yet), and write this test as part of Task 2's Step 5 instead, where `C.set_index_stats` actually exists. Either ordering is fine; the second is safer for strict TDD and is what Task 2 assumes below.

- [ ] **Step 7 (only if you wrote the test in Step 6): Register the test**

Find `test_catalog`'s test list in `test/test_catalog.ml` (search for `test_mirror_roundtrips_columns_fks_and_index` in the `Alcotest.run`/test-list block near the bottom of the file) and add:

```ocaml
"index stats round-trip (#576)", `Quick, test_index_stats_roundtrip_576;
```

- [ ] **Step 8: Build**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build`
Expected: builds clean (if you deferred the test to Task 2, `catalog.ml`/`.mli` alone must build clean here).

- [ ] **Step 9: Format**

Run: `sh scripts/check-fmt.sh --fix`

- [ ] **Step 10: Commit**

```bash
git add lib/catalog/catalog.ml lib/catalog/catalog.mli
git commit -m "perf(#576): add index_stats data model and version-4 encoding

Extends index_info with an optional idx_stats field (distinct_count,
rows_at_analysis) and bumps the persisted encoding to version 4.
Pre-v4 data decodes with idx_stats = None -- no migration needed.
Tier 1 of #576; population and consumption land in follow-up commits.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>"
```

---

### Task 2: `Cat.set_index_stats` — persist stats on an existing index

**Files:**
- Modify: `lib/catalog/catalog.mli` (add `val set_index_stats` near `val create_index`)
- Modify: `lib/catalog/catalog.ml` (add `let set_index_stats` near `create_index`, ~line 2937, right after it)
- Test: `test/test_catalog.ml` (the round-trip test from Task 1 Step 6, now made to compile and pass; plus a standalone `set_index_stats` test)

**Interfaces:**
- Consumes: `index_info`, `index_stats` from Task 1; `indexes_of_table_tx`, `Schema_cache.put_index`, `Schema_cache.find_index`, `sys_indexes_tid`, `encode_index_value` (all pre-existing, private to `catalog.ml`).
- Produces: `Cat.set_index_stats : t -> Granary_store.Store.rw Granary_store.Store.txn -> name:string -> distinct_count:int -> rows_at_analysis:int -> unit Lwt.t`. Task 3's population code is the only caller.

- [ ] **Step 1: Add the `.mli` declaration**

In `lib/catalog/catalog.mli`, immediately after the `val create_index` block (~line 388, right after its closing `-> (index_info, string) result Lwt.t`), add:

```ocaml
(** #576 tier 1: persist [idx_stats] on the named index's catalog row —
    the leading-column distinct-value count and the row count observed while
    computing it.  Must run inside [tx]: the sole caller,
    [Exec.execute_create_index], always holds one from populating the index,
    so stats land in the same DDL transaction as the index itself (and roll
    back with it).  A no-op if [name] does not name a live index (defensive;
    unreachable from the sole call site, which just created it). *)
val set_index_stats
  :  t
  -> Granary_store.Store.rw Granary_store.Store.txn
  -> name:string
  -> distinct_count:int
  -> rows_at_analysis:int
  -> unit Lwt.t
```

- [ ] **Step 2: Implement it in `catalog.ml`**

Immediately after `create_index`'s closing `;;` (~line 2936, right before the comment block for the next function), add:

```ocaml
(* #576 tier 1: [name]'s catalog row already exists (the sole caller,
   [Exec.execute_create_index], just created it in the same [tx]) — this
   re-locates its storage key via [indexes_of_table_tx] (read-your-own-writes
   through [tx]) rather than threading the numeric id back from
   [create_index], which returns only [index_info]. *)
let set_index_stats t tx ~name ~distinct_count ~rows_at_analysis =
  match Schema_cache.find_index t.sc name with
  | None -> Lwt.return_unit
  | Some info ->
    let%lwt idxs = indexes_of_table_tx tx ~table:info.idx_table in
    (match List.find_opt (fun (_, (i : index_info)) -> String.equal i.idx_name name) idxs with
     | None -> Lwt.return_unit
     | Some (k, _) ->
       let new_info = { info with idx_stats = Some { distinct_count; rows_at_analysis } } in
       let%lwt () = S.put tx sys_indexes_tid k (encode_index_value new_info) in
       Schema_cache.put_index t.sc ~name new_info;
       Lwt.return_unit)
;;
```

This must be placed AFTER `indexes_of_table_tx` is defined (~line 3143) OR you must move `indexes_of_table_tx` earlier — check with a build. If `indexes_of_table_tx` is defined later in the file than `create_index`, place `set_index_stats` immediately after `indexes_of_table_tx` instead (search for its definition and add `set_index_stats` right after it), not immediately after `create_index`. Either location is fine; what matters is that OCaml's single-pass compilation sees `indexes_of_table_tx` before `set_index_stats` uses it.

- [ ] **Step 3: Build**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build`
Expected: clean build. If `indexes_of_table_tx` is not yet in scope, move `set_index_stats` down per Step 2's note and rebuild.

- [ ] **Step 4: Finish (or write) the Task 1 round-trip test**

If you deferred Task 1 Step 6, write `test_index_stats_roundtrip_576` now (verbatim as given in Task 1 Step 6 — it compiles now that `C.set_index_stats` exists). If you already wrote it in Task 1, fix its `S.rw_begin` call — the inline `(run (S.rw_begin store) |> fun x -> x)` in that draft is intentionally awkward placeholder syntax; replace the whole statement with:

```ocaml
     let* tx0 = S.rw_begin store in
     let* () = C.set_index_stats cat1 tx0 ~name:"idx_t_b" ~distinct_count:7 ~rows_at_analysis:42 in
     let* () = S.commit tx0 in
```

(placed where the awkward line was, still before the "force mirror reconstruction" block). Register it in the test list (Task 1 Step 7) if not already done.

- [ ] **Step 5: Add a standalone `set_index_stats` behavior test**

Add alongside the round-trip test in `test/test_catalog.ml`:

```ocaml
(* #576 tier 1: set_index_stats only touches the named index -- a sibling
   index on the same table keeps idx_stats = None. *)
let test_set_index_stats_is_index_scoped_576 () =
  run
    (let store = S.create () in
     let* cat = C.open_ store in
     let* _tid =
       C.create_table
         cat
         ~name:"t"
         ~columns:[ int_col "a"; int_col "b" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let create name col =
       C.create_index
         cat
         ~name
         ~table:"t"
         ~columns:[ col ]
         ~unique:false
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     let* _ = create "idx_a" "a" in
     let* _ = create "idx_b" "b" in
     let* tx = S.rw_begin store in
     let* () = C.set_index_stats cat tx ~name:"idx_a" ~distinct_count:3 ~rows_at_analysis:9 in
     let* () = S.commit tx in
     (match C.find_index cat ~name:"idx_a" with
      | Some { C.idx_stats = Some s; _ } ->
        Alcotest.(check int) "idx_a distinct_count" 3 s.C.distinct_count
      | _ -> Alcotest.fail "idx_a should have stats");
     (match C.find_index cat ~name:"idx_b" with
      | Some { C.idx_stats = None; _ } -> ()
      | _ -> Alcotest.fail "idx_b should be untouched");
     Lwt.return_unit)
;;
```

Register it too: `"set_index_stats is index-scoped (#576)", \`Quick, test_set_index_stats_is_index_scoped_576;`

- [ ] **Step 6: Run the tests**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_catalog.exe`
Expected: both new tests PASS, no existing test regresses.

- [ ] **Step 7: Format**

Run: `sh scripts/check-fmt.sh --fix`

- [ ] **Step 8: Commit**

```bash
git add lib/catalog/catalog.ml lib/catalog/catalog.mli test/test_catalog.ml
git commit -m "perf(#576): add Cat.set_index_stats and pin its round-trip

set_index_stats persists a computed (distinct_count, rows_at_analysis)
onto an already-created index's catalog row, through the caller's
transaction, and updates the in-memory Schema_cache entry so a
subsequent Cat.find_index in the same process sees it immediately.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>"
```

---

### Task 3: Population — compute stats inside `execute_create_index`'s walk

**Files:**
- Modify: `lib/sql/exec.ml:4797-4890` (`execute_create_index`)
- Test: Create `test/test_index_cardinality_576.ml`
- Modify: `test/dune` (register the new test executable)

**Interfaces:**
- Consumes: `Cat.set_index_stats` (Task 2), `Index_key.encode_value : Index_key.value -> bytes` (pre-existing, already used in this same function via `Index_key.encode`), `Db.catalog : Db.t -> Cat.t` (pre-existing).
- Produces: nothing new callable — this task's only externally visible effect is that `idx_stats` is now populated after `CREATE INDEX` on a non-unique index. Tasks 4/5 do not call anything from this task directly; they rely on the catalog state it produces.

- [ ] **Step 1: Write the failing tests first**

Create `test/test_index_cardinality_576.ml`:

```ocaml
(** #576 tier 1: CREATE INDEX on a non-unique column computes and persists
    the leading column's distinct-value count, piggybacked on the index's
    existing full-table population walk (no new scan). A UNIQUE index, and
    any index on a WITHOUT ROWID table, is exempt -- see the design doc's
    "Data model" section for why. *)

module Db = Granary.Db
module Cat = Granary_catalog.Catalog

let run = Lwt_main.run

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

let idx_stats db name =
  match Cat.find_index (Db.catalog db) ~name with
  | None -> Alcotest.failf "index %S not found" name
  | Some i -> i.Cat.idx_stats
;;

(* 100,000 rows, 50 distinct tenant_id values -> 2,000 rows/value. *)
let n_rows = 100_000
let n_distinct = 50

let seed_skewed db =
  exec db "CREATE TABLE t (tenant_id INTEGER, v INTEGER)";
  exec db "BEGIN";
  for i = 1 to n_rows do
    exec db (Printf.sprintf "INSERT INTO t VALUES (%d, %d)" (i mod n_distinct) i)
  done;
  exec db "COMMIT"
;;

let non_unique_index_gets_analyzed () =
  with_db (fun db ->
    seed_skewed db;
    exec db "CREATE INDEX idx_t_tenant ON t(tenant_id)";
    match idx_stats db "idx_t_tenant" with
    | None -> Alcotest.fail "expected idx_stats to be populated"
    | Some s ->
      Alcotest.(check int) "distinct_count" n_distinct s.Cat.distinct_count;
      Alcotest.(check int) "rows_at_analysis" n_rows s.Cat.rows_at_analysis)
;;

let unique_index_is_exempt () =
  with_db (fun db ->
    exec db "CREATE TABLE u (k INTEGER, v INTEGER)";
    exec db "INSERT INTO u VALUES (1, 10), (2, 20), (3, 30)";
    exec db "CREATE UNIQUE INDEX idx_u_k ON u(k)";
    match idx_stats db "idx_u_k" with
    | None -> ()
    | Some _ -> Alcotest.fail "UNIQUE index must not carry idx_stats")
;;

let without_rowid_index_is_exempt () =
  with_db (fun db ->
    exec db "CREATE TABLE w (k INTEGER PRIMARY KEY, v INTEGER) WITHOUT ROWID";
    exec db "INSERT INTO w VALUES (1, 10), (1, 10)"; (* deliberately would-be dup, see below *)
    ignore (run (Db.execute db "INSERT INTO w VALUES (2, 20)"));
    exec db "CREATE INDEX idx_w_v ON w(v)";
    match idx_stats db "idx_w_v" with
    | None -> ()
    | Some _ -> Alcotest.fail "WITHOUT ROWID table's index must not carry idx_stats")
;;

let a_row_excluded_by_a_partial_index_where_is_not_counted () =
  with_db (fun db ->
    exec db "CREATE TABLE p (tenant_id INTEGER, active INTEGER)";
    exec db "BEGIN";
    for i = 1 to 100 do
      exec db (Printf.sprintf "INSERT INTO p VALUES (%d, %d)" (i mod 10) (if i <= 50 then 1 else 0))
    done;
    exec db "COMMIT";
    exec db "CREATE INDEX idx_p_tenant ON p(tenant_id) WHERE active = 1";
    match idx_stats db "idx_p_tenant" with
    | None -> Alcotest.fail "expected idx_stats to be populated"
    | Some s -> Alcotest.(check int) "rows_at_analysis excludes filtered-out rows" 50 s.Cat.rows_at_analysis)
;;

let () =
  Alcotest.run
    "index_cardinality_576"
    [ ( "population"
      , [ "non-unique index is analyzed", `Quick, non_unique_index_gets_analyzed
        ; "UNIQUE index is exempt", `Quick, unique_index_is_exempt
        ; "WITHOUT ROWID index is exempt", `Quick, without_rowid_index_is_exempt
        ; "partial index respects WHERE", `Quick, a_row_excluded_by_a_partial_index_where_is_not_counted
        ] )
    ]
;;
```

Note: the WITHOUT ROWID test's second `INSERT` line has a stray always-succeeding duplicate insert attempt written unclearly — replace it with a plain single insert. Corrected body for `without_rowid_index_is_exempt`:

```ocaml
let without_rowid_index_is_exempt () =
  with_db (fun db ->
    exec db "CREATE TABLE w (k INTEGER PRIMARY KEY, v INTEGER) WITHOUT ROWID";
    exec db "INSERT INTO w VALUES (1, 10)";
    exec db "INSERT INTO w VALUES (2, 20)";
    exec db "CREATE INDEX idx_w_v ON w(v)";
    match idx_stats db "idx_w_v" with
    | None -> ()
    | Some _ -> Alcotest.fail "WITHOUT ROWID table's index must not carry idx_stats")
;;
```

- [ ] **Step 2: Register the test executable in `test/dune`**

Find the `test_index_cardinality_576` neighborhood is new; add a stanza modeled on `test_join_cost_model_576`'s (search for `(name test_join_cost_model_576)` in `test/dune` and add a new stanza right after its closing `)`):

```
(test
 (name test_index_cardinality_576)
 (modules test_index_cardinality_576)
 (libraries granary granary.unix granary.catalog alcotest lwt.unix unix))
```

Check `granary.catalog` is the correct library name for `Granary_catalog.Catalog` by grepping `test/dune` for another stanza that already uses `Granary_catalog` (e.g. `test_rename_column_553`'s stanza) and copy its exact `libraries` list instead if it differs.

- [ ] **Step 3: Run the tests to see them fail**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build`
Expected: FAILS to build — `Db.catalog` may already exist, but `execute_create_index` does not yet call `Cat.set_index_stats`, so `non_unique_index_gets_analyzed` and `a_row_excluded_by_a_partial_index_where_is_not_counted` will build fine but FAIL at runtime (idx_stats stays `None`). Confirm the build succeeds and then run:

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_index_cardinality_576.exe`
Expected: `non-unique index is analyzed` and `partial index respects WHERE` FAIL (idx_stats is None); `UNIQUE index is exempt` and `WITHOUT ROWID index is exempt` PASS vacuously (they already expect `None`).

- [ ] **Step 4: Implement population in `execute_create_index`**

In `lib/sql/exec.ml`, find (~lines 4830-4886):

```ocaml
    | Ok info ->
      let* cur = S.cursor_open tx tree_id in
      let _sr = S.cursor_first cur in
      let rec walk () =
        match S.cursor_next cur with
        | None -> Lwt.return_unit
        | Some (kbytes, vbytes) ->
          let rowid = Rowid.decode kbytes in
          let row = decode_with_virtual_cols None [||] ~table_name:table columns vbytes in
          let skip =
            match where_expr with
            | None -> false
            | Some we -> not (value_truthy (eval_expr None [||] row we))
          in
          if skip
          then walk ()
          else (
            let key_vals = get_index_key_values None [||] info columns row in
            let iks = List.map row_value_to_index_value key_vals in
            let ikey = Index_key.encode iks ~rowid in
            (* ... uniqueness probe comment ... *)
            let* () =
              if (not unique) || any_null_val key_vals
              then Lwt.return_unit
              else (
                let prefix, plen = encode_index_key_prefix iks in
                let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
                let* probe = S.seek_ge tx info.idx_tree_id seek_key in
                let* first = S.seek_next probe in
                S.seek_close probe;
                match first with
                | Some (existing, _)
                  when Bytes.length existing >= plen
                       && Bytes.equal (Bytes.sub existing 0 plen) prefix ->
                  Lwt.fail_with
                    (unique_constraint_failed_msg ~table ~columns:info.idx_columns)
                | _ -> Lwt.return_unit)
            in
            let* () = S.put tx info.idx_tree_id ikey Bytes.empty in
            walk ())
      in
      let* () = walk () in
      S.cursor_close cur;
      Lwt.return_unit)
```

Replace with (new lines marked `(* NEW *)`):

```ocaml
    | Ok info ->
      let* cur = S.cursor_open tx tree_id in
      let _sr = S.cursor_first cur in
      (* #576 tier 1: piggyback the leading-column distinct-value count on
         this walk -- it already decodes every candidate row and computes its
         index key, so this adds no I/O. [None] for a UNIQUE index: its
         cardinality is definitionally 1 per key, so no stat is useful. *)
      let seen = if unique then None else Some (Hashtbl.create 64) in (* NEW *)
      let rows_indexed = ref 0 in (* NEW *)
      let rec walk () =
        match S.cursor_next cur with
        | None -> Lwt.return_unit
        | Some (kbytes, vbytes) ->
          let rowid = Rowid.decode kbytes in
          let row = decode_with_virtual_cols None [||] ~table_name:table columns vbytes in
          let skip =
            match where_expr with
            | None -> false
            | Some we -> not (value_truthy (eval_expr None [||] row we))
          in
          if skip
          then walk ()
          else (
            let key_vals = get_index_key_values None [||] info columns row in
            let iks = List.map row_value_to_index_value key_vals in
            let ikey = Index_key.encode iks ~rowid in
            (match seen with (* NEW *)
             | None -> () (* NEW *)
             | Some tbl -> (* NEW *)
               Hashtbl.replace tbl (Index_key.encode_value (List.hd iks)) () (* NEW *)
            ); (* NEW *)
            incr rows_indexed; (* NEW *)
            (* #288: for a UNIQUE index, the build must detect pre-existing
               duplicate values.  The encoded key includes the rowid suffix, so
               two rows sharing the indexed value produce DISTINCT keys and never
               collide in the tree — uniqueness would otherwise only be enforced
               at INSERT time, letting pre-existing duplicates slip through.
               Probe the partially-built index for an entry already carrying this
               value prefix (rowid excluded) using the SAME mechanism as
               [check_insert_unique], so build-time and insert-time uniqueness
               agree (including multi-column, partial-WHERE and NULL handling).
               #290: a key with ANY NULL column is exempt from the conflict
               probe (NULLs are distinct in SQLite) — but is STILL inserted into
               the index tree below, exactly as [check_insert_unique] does.
               A raise here unwinds through [with_ddl_txn]: an owned txn rolls
               back (no partial entries), a borrowed one is poisoned (#286).
               The raise skips the outer [S.cursor_close cur] below, but
               [cursor_close] is a no-op (no OS handle) and the txn unwind
               reclaims all store state — so no leak. *)
            let* () =
              if (not unique) || any_null_val key_vals
              then Lwt.return_unit
              else (
                let prefix, plen = encode_index_key_prefix iks in
                let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
                let* probe = S.seek_ge tx info.idx_tree_id seek_key in
                let* first = S.seek_next probe in
                S.seek_close probe;
                match first with
                | Some (existing, _)
                  when Bytes.length existing >= plen
                       && Bytes.equal (Bytes.sub existing 0 plen) prefix ->
                  Lwt.fail_with
                    (unique_constraint_failed_msg ~table ~columns:info.idx_columns)
                | _ -> Lwt.return_unit)
            in
            let* () = S.put tx info.idx_tree_id ikey Bytes.empty in
            walk ())
      in
      let* () = walk () in
      S.cursor_close cur;
      (* #576 tier 1: persist the stat in the same DDL transaction as the
         index itself, so it rolls back with it. *)
      (match seen with (* NEW *)
       | None -> Lwt.return_unit (* NEW *)
       | Some tbl -> (* NEW *)
         Cat.set_index_stats (* NEW *)
           cat (* NEW *)
           tx (* NEW *)
           ~name (* NEW *)
           ~distinct_count:(Hashtbl.length tbl) (* NEW *)
           ~rows_at_analysis:!rows_indexed) (* NEW *)
;;
```

Check the exact match on `Hashtbl.replace tbl (Index_key.encode_value (List.hd iks)) ()`: `iks : Index_key.value list` (from `List.map row_value_to_index_value key_vals`, already computed a few lines above and reused as-is — no new evaluation is added), and `Hashtbl.create`/`Hashtbl.replace`/`Hashtbl.length` are the plain `Stdlib.Hashtbl` (already available; `exec.ml` does not shadow it — verify with a build).

- [ ] **Step 5: Run the tests again**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_index_cardinality_576.exe`
Expected: all 4 tests PASS.

- [ ] **Step 6: Run the full suite to check for regressions**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test`
Expected: no regressions. Pay particular attention to `test_unique_build_288.ml` (the function you edited is the one it pins) and any test exercising `CREATE INDEX` at all.

- [ ] **Step 7: Format**

Run: `sh scripts/check-fmt.sh --fix`

- [ ] **Step 8: Commit**

```bash
git add lib/sql/exec.ml test/test_index_cardinality_576.ml test/dune
git commit -m "perf(#576): populate index_stats during CREATE INDEX's walk

execute_create_index now accumulates the leading indexed column's
distinct-value count in a Hashtbl while it walks the table to build
the index (no new scan), and persists it via Cat.set_index_stats in
the same DDL transaction. UNIQUE indexes are exempt (cardinality is
definitionally 1 per key); a partial index's WHERE-excluded rows are
correctly not counted, since the walk already skips them.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>"
```

---

### Task 4: Consumption — `estimate_rows` uses `idx_stats`

**Files:**
- Modify: `lib/sql/planner.ml:1087-1133` (add helpers after `index_by_tree`)
- Modify: `lib/sql/planner.ml:1289-1304` (`estimate_rows`)
- Test: `test/test_index_cardinality_576.ml` (append a new test group)

**Interfaces:**
- Consumes: `Cat.index_info.idx_stats`, `index_by_tree` (pre-existing planner helper, ~line 1120), `table_rows_estimate` (pre-existing, ~line 964).
- Produces: `index_leading_distinct_count : Cat.t -> Cat.table_meta -> idx_tree:int -> int option` and `estimate_rows_from_stats : Cat.t -> Cat.table_meta -> idx_tree:int -> int option`, both consumed again by Task 5.

- [ ] **Step 1: Write the failing test first**

Append to `test/test_index_cardinality_576.ml` (add `module Cat` is already imported; no new opens needed beyond what Task 3 added):

```ocaml
(* #576 tier 1: estimate_rows' new selectivity estimate for a non-unique
   equality-prefix DRIVING-side seek should let a nested-loop probe win where
   the old unbounded_rows estimate could never justify one.

   d: 100,000 rows, 500 distinct k values (200 rows/value) -- WHERE d.k = 7
   seeks a selective, non-unique prefix. r: 300 rows, no index -- always a
   plain SeqScan build side. Before this task, estimate_rows answers
   unbounded_rows for d's seek, so probe_is_worth_it's
   "driving_rows < unbounded_rows" guard is false and the join is ALWAYS a
   HashJoin. After this task, driving_rows ~= 100_000/500 = 200, which is
   <= nlj_min_driving_rows (1000), so the join becomes a NestedLoopJoin(r) --
   independent of build_side_seek_is_unambiguous (Task 5), which is not
   consulted for a build side that has no matching index at all. *)
let n_d_rows = 100_000
let n_d_distinct = 500
let n_r_rows = 300

let seed_driving_seek db =
  exec db "CREATE TABLE d (k INTEGER, v INTEGER)";
  exec db "CREATE INDEX idx_d_k ON d(k)";
  exec db "CREATE TABLE r (x INTEGER)";
  exec db "BEGIN";
  for i = 1 to n_d_rows do
    exec db (Printf.sprintf "INSERT INTO d VALUES (%d, %d)" (i mod n_d_distinct) i)
  done;
  for i = 1 to n_r_rows do
    exec db (Printf.sprintf "INSERT INTO r VALUES (%d)" i)
  done;
  exec db "COMMIT"
;;

let plan_text db sql =
  match run (Db.query db sql) with
  | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
  | Ok stream ->
    let rows = run (Lwt_stream.to_list stream) in
    String.concat
      "\n"
      (List.map
         (fun r ->
            String.concat
              " "
              (Array.to_list
                 (Array.map
                    (function
                      | Db.V_text s -> s
                      | Db.V_int n -> Int64.to_string n
                      | Db.V_real f -> Printf.sprintf "%h" f
                      | Db.V_blob b -> Bytes.to_string b
                      | Db.V_null -> "NULL")
                    r)))
         rows)
;;

let contains ~needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  let rec go i = i + nlen <= hlen && (String.sub haystack i nlen = needle || go (i + 1)) in
  go 0
;;

let selective_driving_seek_wins_the_probe () =
  with_db (fun db ->
    seed_driving_seek db;
    let plan = plan_text db "EXPLAIN SELECT * FROM d JOIN r ON d.v = r.x WHERE d.k = 7" in
    Alcotest.(check bool)
      ("selective driving seek should choose NestedLoopJoin, got:\n" ^ plan)
      true
      (contains ~needle:"NestedLoopJoin" plan))
;;

let () =
  Alcotest.run
    "index_cardinality_576"
    [ ( "population"
      , [ "non-unique index is analyzed", `Quick, non_unique_index_gets_analyzed
        ; "UNIQUE index is exempt", `Quick, unique_index_is_exempt
        ; "WITHOUT ROWID index is exempt", `Quick, without_rowid_index_is_exempt
        ; "partial index respects WHERE", `Quick, a_row_excluded_by_a_partial_index_where_is_not_counted
        ] )
    ; "estimate_rows", [ "selective driving seek wins the probe", `Quick, selective_driving_seek_wins_the_probe ]
    ]
;;
```

Delete the OLD trailing `let () = Alcotest.run ...` block from Task 3 (there must be only one `Alcotest.run` call in the file — this step's version replaces it, adding the new `"estimate_rows"` group).

- [ ] **Step 2: Run the test to see it fail**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_index_cardinality_576.exe`
Expected: `selective driving seek wins the probe` FAILS (plan is a `HashJoin`, not a `NestedLoopJoin`).

- [ ] **Step 3: Add the helper functions in `planner.ml`**

Find `index_by_tree` (~lines 1113-1124):

```ocaml
let index_by_tree cat (meta : Cat.table_meta) ~idx_tree =
  Cat.indexes_for_table cat ~table:meta.Cat.name
  |> List.find_opt (fun (i : Cat.index_info) -> i.Cat.idx_tree_id = idx_tree)
;;
```

Immediately after it (still before `index_is_unique`), add:

```ocaml
(** #576 tier 1: [idx_tree]'s leading-column distinct-value count, if the
    index was analyzed at [CREATE INDEX] time and the count is positive.
    [None] covers "never analyzed" (every index created before this shipped,
    every UNIQUE index, WITHOUT ROWID/columnar/expression indexes — see
    [Exec.execute_create_index]) uniformly with "analyzed but somehow zero" —
    the latter cannot happen for a table with at least one row, but a zero
    denominator must never reach the division in {!estimate_rows_from_stats}. *)
let index_leading_distinct_count cat (meta : Cat.table_meta) ~idx_tree =
  match index_by_tree cat meta ~idx_tree with
  | None -> None
  | Some i ->
    (match i.Cat.idx_stats with
     | Some stats when stats.Cat.distinct_count > 0 -> Some stats.Cat.distinct_count
     | _ -> None)
;;

(** #576 tier 1: estimate a non-unique equality-prefix seek's row count from
    [idx_tree]'s analyzed leading-column cardinality, or [None] when no usable
    stat exists (today's exact behavior applies unchanged in that case).
    [table_rows_estimate meta] uses the CURRENT row count; [distinct_count] is
    from analysis time — see the design doc's "Consumption" section for why
    mixing the two is the right call. Shared by {!estimate_rows} and
    {!build_side_seek_is_unambiguous} (Task 5) so the two questions — "how
    many rows" and "is that seek worth taking" — never answer from different
    numbers. *)
let estimate_rows_from_stats cat (meta : Cat.table_meta) ~idx_tree =
  match index_leading_distinct_count cat meta ~idx_tree with
  | None -> None
  | Some distinct_count ->
    let total = table_rows_estimate meta in
    Some (min (total / distinct_count) total)
;;
```

- [ ] **Step 4: Wire it into `estimate_rows`**

Find (~lines 1289-1304):

```ocaml
let estimate_rows cat (op : Plan.op) =
  match op with
  | Plan.Op_rowid_lookup _ -> 1
  | Plan.Op_index_lookup { idx_tree; keys; range; table_meta; _ } ->
    let seek =
      if seek_is_unique_point cat table_meta ~idx_tree ~keys
      then 1
      else (
        match range with
        | Some r -> range_rows_estimate r
        | None -> unbounded_rows)
    in
    min seek (table_rows_estimate table_meta)
  | Plan.Op_seq_scan { table_meta; _ } -> table_rows_estimate table_meta
  | _ -> unbounded_rows
;;
```

Replace the `Op_index_lookup` arm's `None -> unbounded_rows` line:

```ocaml
let estimate_rows cat (op : Plan.op) =
  match op with
  | Plan.Op_rowid_lookup _ -> 1
  | Plan.Op_index_lookup { idx_tree; keys; range; table_meta; _ } ->
    let seek =
      if seek_is_unique_point cat table_meta ~idx_tree ~keys
      then 1
      else (
        match range with
        | Some r -> range_rows_estimate r
        | None ->
          (* #576 tier 1: a non-unique equality prefix with no range used to
             be pure unbounded_rows; now it consults the leading column's
             analyzed distinct-value count when one exists. *)
          (match estimate_rows_from_stats cat table_meta ~idx_tree with
           | Some est -> est
           | None -> unbounded_rows))
    in
    min seek (table_rows_estimate table_meta)
  | Plan.Op_seq_scan { table_meta; _ } -> table_rows_estimate table_meta
  | _ -> unbounded_rows
;;
```

- [ ] **Step 5: Run the test again**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_index_cardinality_576.exe`
Expected: `selective driving seek wins the probe` PASSES.

- [ ] **Step 6: Run the full suite**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test`
Expected: no regressions. Pay particular attention to `test_planner.ml`, `test_join_cost_model_576.ml`, `test_build_side_range_532.ml`, and any TPC-C test (`test_tpcc_smoke.ml`, `bench_tpcc.ml`) — a changed `estimate_rows` answer can move a plan anywhere a non-unique index is seeked without a range. If a TPC-C table has a non-unique secondary index that now gets analyzed and moves a join strategy, that is expected — re-read the failing assertion before assuming it is wrong, since this is exactly the class of change #576 exists to allow.

- [ ] **Step 7: Format**

Run: `sh scripts/check-fmt.sh --fix`

- [ ] **Step 8: Commit**

```bash
git add lib/sql/planner.ml test/test_index_cardinality_576.ml
git commit -m "perf(#576): estimate_rows consults idx_stats for a non-unique prefix

A non-unique equality-prefix seek with no range used to answer
unbounded_rows unconditionally. It now reads the leading column's
analyzed distinct-value count when idx_stats is present, estimating
table_rows_estimate / distinct_count. Falls back to today's exact
behavior (unbounded_rows) whenever idx_stats is None.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>"
```

---

### Task 5: Consumption — `build_side_seek_is_unambiguous` admits a stats-backed seek

**Files:**
- Modify: `lib/sql/planner.ml:1520-1550` (`build_side_seek_is_unambiguous`)
- Test: `test/test_index_cardinality_576.ml` (append a new test group)

**Interfaces:**
- Consumes: `estimate_rows_from_stats`, `table_seek_budget` (both pre-existing/Task-4-added), `index_full_unique_pin` (pre-existing).
- Produces: nothing new callable — this is the last consumption site named in the design doc.

- [ ] **Step 1: Write the failing tests first**

Append to `test/test_index_cardinality_576.ml`, replacing the final `let () = Alcotest.run ...` block again to add a new group:

```ocaml
(* #576 tier 1: build_side_seek_is_unambiguous should ADMIT a non-unique
   equality-prefix seek when idx_stats says it is selective enough, and still
   DECLINE it when the stat says it is not.

   t: 100,000 rows. table_seek_budget = table_rows_estimate /
   build_side_seek_break_even_ratio(200) = 500 (private constant, stated here
   rather than referenced). driver: enough rows to sit comfortably above
   nlj_min_driving_rows so the strategy is decided by the cost comparison, not
   the floor -- mirroring test_join_cost_model_576.ml's own setup. The join
   key (driver.x = t.v) carries no selectivity information itself; only the
   WHERE-pinned prefix on t does. *)
let n_t_rows = 100_000
let n_driver_rows = 1_200

let seed_build_side ~n_distinct db =
  exec db "CREATE TABLE t (tenant_id INTEGER, v INTEGER)";
  exec db "CREATE INDEX idx_t_tenant ON t(tenant_id)";
  exec db "CREATE TABLE driver (x INTEGER)";
  exec db "BEGIN";
  for i = 1 to n_t_rows do
    exec db (Printf.sprintf "INSERT INTO t VALUES (%d, %d)" (i mod n_distinct) i)
  done;
  for i = 1 to n_driver_rows do
    exec db (Printf.sprintf "INSERT INTO driver VALUES (%d)" i)
  done;
  exec db "COMMIT"
;;

(* 500 distinct tenant_id -> estimate = 100_000/500 = 200 <= budget (500): ADMIT. *)
let selective_prefix_is_admitted () =
  with_db (fun db ->
    seed_build_side ~n_distinct:500 db;
    let plan =
      plan_text db "EXPLAIN SELECT * FROM driver JOIN t ON driver.x = t.v WHERE t.tenant_id = 5"
    in
    Alcotest.(check bool)
      ("selective prefix should seek t via IndexLookup, got:\n" ^ plan)
      true
      (contains ~needle:"IndexLookup(t)" plan))
;;

(* 2 distinct tenant_id -> estimate = 100_000/2 = 50_000 > budget (500): DECLINE. *)
let non_selective_prefix_still_declines () =
  with_db (fun db ->
    seed_build_side ~n_distinct:2 db;
    let plan =
      plan_text db "EXPLAIN SELECT * FROM driver JOIN t ON driver.x = t.v WHERE t.tenant_id = 1"
    in
    Alcotest.(check bool)
      ("non-selective prefix should still scan t via SeqScan, got:\n" ^ plan)
      true
      (contains ~needle:"SeqScan(t)" plan))
;;

let () =
  Alcotest.run
    "index_cardinality_576"
    [ ( "population"
      , [ "non-unique index is analyzed", `Quick, non_unique_index_gets_analyzed
        ; "UNIQUE index is exempt", `Quick, unique_index_is_exempt
        ; "WITHOUT ROWID index is exempt", `Quick, without_rowid_index_is_exempt
        ; "partial index respects WHERE", `Quick, a_row_excluded_by_a_partial_index_where_is_not_counted
        ] )
    ; "estimate_rows", [ "selective driving seek wins the probe", `Quick, selective_driving_seek_wins_the_probe ]
    ; ( "build_side_seek_is_unambiguous"
      , [ "selective prefix is admitted", `Quick, selective_prefix_is_admitted
        ; "non-selective prefix still declines", `Quick, non_selective_prefix_still_declines
        ] )
    ]
;;
```

- [ ] **Step 2: Run the tests to see the admission one fail**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_index_cardinality_576.exe`
Expected: `selective prefix is admitted` FAILS (plan still shows `SeqScan(t)`); `non-selective prefix still declines` already PASSES (today's behavior is "always decline", so the non-selective case is already correct — this is the "no regression when not selective enough" pin from the design doc, verified BEFORE the code change on purpose).

- [ ] **Step 3: Wire the admission into `build_side_seek_is_unambiguous`**

Find (~lines 1520-1523):

```ocaml
let build_side_seek_is_unambiguous cat (meta : Cat.table_meta) = function
  | Plan.Seek_rowid _ -> true
  | Plan.Seek_index { idx_tree; keys; range = None } ->
    index_full_unique_pin cat meta ~idx_tree ~keys
  | Plan.Seek_index { idx_tree; keys; range = Some r } ->
```

Replace the `range = None` arm:

```ocaml
let build_side_seek_is_unambiguous cat (meta : Cat.table_meta) = function
  | Plan.Seek_rowid _ -> true
  | Plan.Seek_index { idx_tree; keys; range = None } ->
    index_full_unique_pin cat meta ~idx_tree ~keys
    ||
    (* #576 tier 1: a non-unique equality prefix used to be declined
       unconditionally here. Admit it when the leading column's analyzed
       distinct-value count puts the estimated row count within
       table_seek_budget -- the same budget dml_seek_bail_out_at consults. *)
    (match estimate_rows_from_stats cat meta ~idx_tree with
     | None -> false
     | Some est ->
       (match table_seek_budget meta with
        | None -> false
        | Some budget -> est <= budget))
  | Plan.Seek_index { idx_tree; keys; range = Some r } ->
```

- [ ] **Step 4: Run the tests again**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_index_cardinality_576.exe`
Expected: all tests in the file PASS, including both new ones.

- [ ] **Step 5: Run the full suite**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test`
Expected: no regressions. Same caution as Task 4 Step 6 — re-read before assuming a moved plan is wrong; check especially `test_build_side_range_532.ml`, `test_build_side_seek_546.ml` (bench, not a gate, but worth a look), and any TPC-C consistency check per the CLAUDE.md #706 caveat (a multi-terminal TPC-C run's own consistency oracle, not this change, is what to trust there).

- [ ] **Step 6: Format**

Run: `sh scripts/check-fmt.sh --fix`

- [ ] **Step 7: merlint**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint`
Expected: 0 issues for the files touched in this plan (`catalog.ml`, `catalog.mli`, `exec.ml`, `planner.ml`, `test_catalog.ml`, `test_index_cardinality_576.ml`).

- [ ] **Step 8: Commit**

```bash
git add lib/sql/planner.ml test/test_index_cardinality_576.ml
git commit -m "perf(#576): build_side_seek_is_unambiguous admits a stats-backed prefix

A non-unique equality-prefix seek with no range was declined
unconditionally as a hash join's build side. It is now admitted when
idx_stats' distinct_count estimates a row count within
table_seek_budget -- the same runtime budget dml_seek_bail_out_at
already uses for the DML drain case. Declines exactly as before when
no stat exists or the estimate exceeds budget.

Closes tier 1 of #576.

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>"
```

- [ ] **Step 9: Open the PR**

```bash
git push origin perf/576-index-cardinality-stats
~/.local/bin/forgejo pr create IoTReadyNext/granary \
  --title="perf(#576): tier 1 - per-index leading-column cardinality statistics" \
  --head=perf/576-index-cardinality-stats \
  --base=main \
  --body="$(cat <<'EOF'
## Summary
- Adds idx_stats (distinct_count, rows_at_analysis) to Cat.index_info, persisted as encoding version 4 (pre-v4 data decodes with idx_stats = None -- no migration).
- Populates it inside CREATE INDEX's existing full-table walk (no new scan), for non-unique indexes only.
- estimate_rows and build_side_seek_is_unambiguous both consult it additively: a non-unique equality-prefix seek with no idx_stats behaves exactly as before.

Design: docs/superpowers/specs/2026-08-10-576-tier1-index-cardinality-design.md
Plan: docs/superpowers/plans/2026-08-10-576-tier1-index-cardinality.md

## Test plan
- [ ] dune test passes (full suite, in-container)
- [ ] New tests: test_index_stats_roundtrip_576, test_set_index_stats_is_index_scoped_576 (test_catalog.ml); test_index_cardinality_576.ml (population, estimate_rows, build_side_seek_is_unambiguous groups)
- [ ] sh scripts/check-fmt.sh clean
- [ ] merlint clean

Tier 1 of #576. Tiers 2 (per-column histograms) and any incremental-maintenance follow-up remain open on that umbrella issue.
EOF
)"
```

---

## Self-Review Notes

- **Spec coverage:** Data model (Task 1) ✓, encoding versioning (Task 1) ✓, population piggybacked on the existing walk (Task 3) ✓, static/no-incremental-maintenance (no task adds any — confirmed by omission) ✓, `estimate_rows` consumption (Task 4) ✓, `build_side_seek_is_unambiguous` consumption (Task 5) ✓, all four testing bullets from the spec (round-trip + backward-compat decode → Task 1/2; skewed-column pinning test → Task 4; admission test with both admit and decline cases → Task 5; UNIQUE/unanalyzed/WITHOUT-ROWID no-regression → Task 3) ✓.
- **Placeholder scan:** Task 1 Step 6 flags its own `S.rw_begin` line as an intentional placeholder and gives the exact fix in Task 2 Step 4 — this is a deliberate cross-task TDD sequencing note, not an unresolved TBD; every other step has literal code.
- **Type consistency:** `index_stats`, `idx_stats`, `set_index_stats`'s exact signature, `index_leading_distinct_count`, and `estimate_rows_from_stats` are named and typed identically everywhere they're introduced (Task 1) and consumed (Tasks 2-5).
- **Scope:** Single subsystem (planner cost model + catalog metadata), no decomposition needed.
