# #576 tier 2: per-index leading-column histograms — Implementation Plan

> **SUPERSEDED** by [2026-08-11-576-tier2-per-position-histograms-design.md](../specs/2026-08-11-576-tier2-per-position-histograms-design.md) — its Data model/Population/Consumption sections shipped a real bug (a histogram built for the wrong index column position); see that doc's "The bug" section.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give `Exec.execute_create_index`'s existing walk a per-index leading-column histogram, persist it in the catalog, and make `Planner.range_rows_estimate` consult it for a literal `Integer`/`Real` range — replacing the flat `range_seek_rows = 100` fallback with a real row-count estimate wherever a histogram is available.

**Architecture:** Extend `Cat.index_stats` with an optional `histogram` (an array of `Index_key`-encoded boundary bytes, equi-depth by row count). Populate it inside `execute_create_index`'s existing table walk by upgrading its distinct-value `Hashtbl` from presence to per-key row counts, sorting, and bucketing. Persist it via a new encoding version. Consume it in `range_rows_estimate`, which gains `cat`/`meta`/`~idx_tree` parameters so it can look up the seeked index's stats the same way `estimate_rows_from_stats` already does.

**Tech Stack:** OCaml, Lwt, the granary-dev podman container (`dune build`/`dune test`), Alcotest.

## Global Constraints

- Never call `dune` on the host — every build/test command runs inside `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev <cmd>`, from this worktree (`.worktrees/576-column-histograms`), substituting the worktree path for `$(pwd)`.
- `sh scripts/check-fmt.sh` (and `--fix` to auto-fix) must be clean before any commit that touches formatting-sensitive files; read its summary line, not just the exit code.
- `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint` must report 0 issues for touched files before the branch is considered done (run once, at the end — not required after every task).
- Every public `val` in an `.mli` needs a `(** ... *)` doc comment (merlint enforces this).
- Commit after each task, on branch `perf/576-column-histograms` (already created, worktree already `chmod 777`'d).
- Follow this repo's existing doc-comment density on anything non-obvious — see `catalog.mli`'s `index_stats`/`set_index_stats` comments and `planner.ml`'s `range_rows_estimate`/`estimate_rows_from_stats` comments for the house style. A future reader with no memory of this plan should be able to reconstruct *why*, not just *what*.
- Design doc: `docs/superpowers/specs/2026-08-10-576-tier2-column-histograms-design.md`. Read it before Task 1 — this plan assumes its data model and scope-boundary sections.

---

### Task 1: Data model + version-5 encode/decode + round-trip tests

**Files:**
- Modify: `lib/catalog/catalog.mli:132-156` (`index_stats` type, doc comments)
- Modify: `lib/catalog/catalog.ml:1129-1289` (`encode_index_value`, `decode_index_ext_fields`, `decode_index_value`)
- Test: `test/test_catalog.ml` (new tests, alongside `test_index_stats_roundtrip_576` at `:2516` and `test_decode_index_v3_backward_compat_576` at `:2657`)

**Interfaces:**
- Produces: `type histogram = { boundaries : string array }` and `index_stats` gains `histogram : histogram option`, both in `catalog.mli`/`catalog.ml`. Later tasks read `idx.Cat.idx_stats` → `.Cat.histogram` → `.Cat.boundaries`.
- Consumes: nothing new from other tasks — this is the base layer.

- [ ] **Step 1: Extend the type in `catalog.mli`**

Replace the `index_stats` block at `catalog.mli:128-135`:

```ocaml
(** #576 tier 1: the leading indexed column's distinct-value count, as
    measured the one time the index was populated ([CREATE INDEX]).  Never
    incrementally maintained — see the design doc's "Population" section for
    why staleness is accepted rather than tracked. *)
type index_stats =
  { distinct_count : int
  ; rows_at_analysis : int
  }
```

with:

```ocaml
(** #576 tier 2: an equi-depth (by row count) histogram of the leading
    indexed column's encoded values, computed at the same time as
    {!index_stats.distinct_count}.  [boundaries] holds
    [Index_key.encode_value]-encoded bytes, strictly ascending by
    [Bytes.compare]/[String.compare] (a [bytes] value round-tripped through
    [Bytes.to_string]/[Bytes.of_string] for storage in this array, since
    OCaml's structural comparison/serialization is simplest over immutable
    strings): [boundaries.(0)] is the smallest value seen at analysis time,
    [boundaries.(Array.length boundaries - 1)] the largest, and bucket [i]
    (for [0 <= i < Array.length boundaries - 1]) covers roughly
    [rows_at_analysis / (Array.length boundaries - 1)] rows.  The array length
    is NOT guaranteed to be [histogram_bucket_count + 1]: a single very
    skewed value can absorb several bucket-widths at once and only ever
    contributes one boundary, so a consumer must divide by
    [Array.length boundaries - 1], never by the population-time constant —
    see [Exec.execute_create_index]'s "Population" comment for how the array
    is built. *)
type histogram = { boundaries : string array }

(** #576 tier 1: the leading indexed column's distinct-value count, as
    measured the one time the index was populated ([CREATE INDEX]).  Never
    incrementally maintained — see the design doc's "Population" section for
    why staleness is accepted rather than tracked.

    #576 tier 2 adds [histogram], populated (or not) by exactly the same
    walk and under the same eligibility rules as [distinct_count] — see
    [Exec.execute_create_index]. [None] whenever [distinct_count] is [None],
    and additionally when the leading column has fewer distinct values than
    [Exec.histogram_bucket_count]: with fewer distinct values than buckets, a
    histogram would degenerate to one boundary per value, which
    [distinct_count] alone already answers as well. *)
type index_stats =
  { distinct_count : int
  ; rows_at_analysis : int
  ; histogram : histogram option
  }
```

Update the `index_info.idx_stats` field's doc comment at `catalog.mli:146-155` to add one sentence: after "...expression-column index — see `Exec.execute_create_index`...", add: `histogram` follows the same eligibility as `distinct_count`, with one further exemption (see `histogram`'s own doc comment above) for a leading column with fewer distinct values than the bucket-count floor.

- [ ] **Step 2: Bump the encoder to version 5, in `catalog.ml`**

In `encode_index_value` (`catalog.ml:1129-1166`), the version write and the `idx_stats` block currently read:

```ocaml
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

Replace with (bump to version 5, extend the `idx_stats` write with an appended histogram):

```ocaml
  (* Extended fields version 5 (#576 tier 2): origin byte + expr flags +
     optional WHERE + optional idx_stats (now including an optional
     histogram). Versions 1-4 (pre-existing data) decode with
     [idx_stats = None] / [histogram = None] respectively — see
     [decode_index_ext_fields]. *)
  Varint.encode_uint64 buf 5L;
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
  (* #576 tier 1: leading-column cardinality, added in version 4. #576 tier 2
     appends an optional histogram after it, added in version 5. *)
  (match idx.idx_stats with
   | None -> Buffer.add_char buf '\x00'
   | Some { distinct_count; rows_at_analysis; histogram } ->
     Buffer.add_char buf '\x01';
     Varint.encode_uint64 buf (Int64.of_int distinct_count);
     Varint.encode_uint64 buf (Int64.of_int rows_at_analysis);
     (match histogram with
      | None -> Buffer.add_char buf '\x00'
      | Some { boundaries } ->
        Buffer.add_char buf '\x01';
        Varint.encode_uint64 buf (Int64.of_int (Array.length boundaries));
        Array.iter
          (fun b ->
             Varint.encode_uint64 buf (Int64.of_int (String.length b));
             Buffer.add_string buf b)
          boundaries));
  Buffer.to_bytes buf
;;
```

- [ ] **Step 3: Add the version-5 decode branch and thread `histogram` through**

In `decode_index_ext_fields` (`catalog.ml:1175-1237`), the version-4 branch currently reads:

```ocaml
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
    | _ -> List.map (fun _ -> false) cols, None, `User, None
```

Change the version-4 arm's record construction to add `histogram = None` (it now has one more field), and add a version-5 arm right after it:

```ocaml
    | 4 ->
      (* Version 4 (#576 tier 1): version-3 fields, then optional idx_stats.
         No histogram in this version — #576 tier 2 added that in version 5,
         so data written as version 4 always decodes with [histogram = None]. *)
      let origin = idx_origin_of_byte (Bytes.get_uint8 bytes off3) in
      let expr_flags, where_sql, off_after_where = decode_flags_and_where (off3 + 1) in
      let has_stats = Bytes.get_uint8 bytes off_after_where in
      let idx_stats =
        if has_stats = 0
        then None
        else (
          let dc, off_a = Varint.decode_uint64 bytes (off_after_where + 1) in
          let ra, _ = Varint.decode_uint64 bytes off_a in
          Some
            { distinct_count = Int64.to_int dc
            ; rows_at_analysis = Int64.to_int ra
            ; histogram = None
            })
      in
      expr_flags, where_sql, origin, idx_stats
    | 5 ->
      (* Version 5 (#576 tier 2): version-4 fields, then idx_stats also
         carries an optional histogram (a boundary count, then that many
         length-prefixed byte strings). *)
      let origin = idx_origin_of_byte (Bytes.get_uint8 bytes off3) in
      let expr_flags, where_sql, off_after_where = decode_flags_and_where (off3 + 1) in
      let has_stats = Bytes.get_uint8 bytes off_after_where in
      let idx_stats =
        if has_stats = 0
        then None
        else (
          let dc, off_a = Varint.decode_uint64 bytes (off_after_where + 1) in
          let ra, off_b = Varint.decode_uint64 bytes off_a in
          let has_hist = Bytes.get_uint8 bytes off_b in
          let histogram =
            if has_hist = 0
            then None
            else (
              let n_boundaries, off_c = Varint.decode_uint64 bytes (off_b + 1) in
              let n = Int64.to_int n_boundaries in
              let off_ref = ref off_c in
              let boundaries =
                Array.init n (fun _ ->
                  let blen, off_next = Varint.decode_uint64 bytes !off_ref in
                  let s = Bytes.sub_string bytes off_next (Int64.to_int blen) in
                  off_ref := off_next + Int64.to_int blen;
                  s)
              in
              Some { boundaries })
          in
          Some
            { distinct_count = Int64.to_int dc
            ; rows_at_analysis = Int64.to_int ra
            ; histogram
            })
      in
      expr_flags, where_sql, origin, idx_stats
    | _ -> List.map (fun _ -> false) cols, None, `User, None
```

Versions 1-3 already default `idx_stats` to `None` outright (no `histogram` field to touch — `None` needs no record). No other changes needed in `decode_index_value` — it already just forwards whatever `decode_index_ext_fields` returns.

- [ ] **Step 4: Fix every other constructor of `index_stats`**

Search for every place that builds an `index_stats` or `{ distinct_count; rows_at_analysis }` record and add `histogram`. Run:

```sh
grep -rn "distinct_count" lib/ test/
```

At minimum this will hit `Cat.set_index_stats` in `catalog.ml` (~`:3200-3216`, `{ info with idx_stats = Some { distinct_count; rows_at_analysis } }`) — **do not fix this yet**, Task 2 changes its signature deliberately. For this task, only fix build failures in `catalog.ml`/`catalog.mli` itself; leave `exec.ml`'s call site and any compile errors it causes for Task 2/3 (this task's own build will fail there, which is expected — see Step 5).

- [ ] **Step 5: Build and expect exactly the failures Task 2/3 will fix**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build`

Expected: FAIL, with errors localized to `Cat.set_index_stats`'s call in `catalog.ml` (missing `histogram` field) and `Exec.execute_create_index`'s call to it in `exec.ml` (arity mismatch). No errors anywhere else — if there are, something else constructs `index_stats` and Step 4 missed it; fix those too before moving on.

- [ ] **Step 6: Write the round-trip test**

Add to `test/test_catalog.ml`, near `test_index_stats_roundtrip_576` (`:2516`):

```ocaml
(* #576 tier 2: histogram round-trips through encode/decode alongside
   distinct_count/rows_at_analysis, both present and absent. *)
let test_index_histogram_roundtrip_576 () =
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
     let boundaries = [| "\x02\x00"; "\x02\x05"; "\x02\x0a"; "\x02\x0f" |] in
     let* tx0 = S.rw_begin store in
     let* () =
       C.set_index_stats
         cat1
         tx0
         ~name:"idx_t_b"
         ~distinct_count:7
         ~rows_at_analysis:42
         ~histogram:(Some { C.boundaries })
     in
     let* () = S.commit tx0 in
     let* tx = S.rw_begin store in
     let* () = S.del tx 0 (Bytes.of_string "t") in
     let* () = S.commit tx in
     let* cat2 = C.open_ store in
     let idxs = C.indexes_for_table cat2 ~table:"t" in
     let idx =
       match List.find_opt (fun (i : C.index_info) -> i.C.idx_name = "idx_t_b") idxs with
       | Some i -> i
       | None -> Alcotest.fail "user index idx_t_b missing after reopen"
     in
     (match idx.C.idx_stats with
      | Some { C.histogram = Some { C.boundaries = got }; _ } ->
        Alcotest.(check (array string)) "boundaries preserved" boundaries got
      | Some { C.histogram = None; _ } -> Alcotest.fail "histogram not preserved"
      | None -> Alcotest.fail "idx_stats not preserved");
     Lwt.return_unit)
;;

(* #576 tier 2: a stats record with [histogram = None] round-trips too --
   the presence byte, not just the boundary bytes, must survive. *)
let test_index_stats_without_histogram_roundtrip_576 () =
  run
    (let store = S.create () in
     let* cat1 = C.open_ store in
     let* _tid =
       C.create_table
         cat1
         ~name:"t2"
         ~columns:[ int_col "a" ]
         ~without_rowid:false
         ~autoincrement:false
     in
     let* r =
       C.create_index
         cat1
         ~name:"idx_t2_a"
         ~table:"t2"
         ~columns:[ "a" ]
         ~unique:false
         ~expr_flags:[ false ]
         ~where_sql:None
         ~origin:`User
     in
     (match r with
      | Ok _ -> ()
      | Error e -> Alcotest.failf "create_index: %s" e);
     let* tx0 = S.rw_begin store in
     let* () =
       C.set_index_stats
         cat1
         tx0
         ~name:"idx_t2_a"
         ~distinct_count:3
         ~rows_at_analysis:9
         ~histogram:None
     in
     let* () = S.commit tx0 in
     let* tx = S.rw_begin store in
     let* () = S.del tx 0 (Bytes.of_string "t2") in
     let* () = S.commit tx in
     let* cat2 = C.open_ store in
     (match C.find_index cat2 ~name:"idx_t2_a" with
      | Some { C.idx_stats = Some { C.histogram = None; distinct_count = 3; _ }; _ } -> ()
      | _ -> Alcotest.fail "expected histogram = None to round-trip");
     Lwt.return_unit)
;;
```

Add a version-4 backward-compat test next to `test_decode_index_v3_backward_compat_576` (`:2657`), copying `make_v3_index_bytes` into a `make_v4_index_bytes` that also appends the version-4 `idx_stats` bytes (has_stats=1, distinct_count, rows_at_analysis varints, no histogram bytes at all — version 4 never wrote them):

```ocaml
let make_v4_index_bytes
      ~name
      ~table
      ~columns
      ~unique
      ~tree_id
      ~origin_byte
      ~expr_flags
      ~where_sql
      ~distinct_count
      ~rows_at_analysis
  =
  let buf = Buffer.create 32 in
  let v = Varint.encode_uint64 in
  v buf (Int64.of_int (String.length name));
  Buffer.add_string buf name;
  v buf (Int64.of_int (String.length table));
  Buffer.add_string buf table;
  v buf (Int64.of_int (List.length columns));
  List.iter
    (fun col ->
       v buf (Int64.of_int (String.length col));
       Buffer.add_string buf col)
    columns;
  Buffer.add_char buf (if unique then '\x01' else '\x00');
  v buf (Int64.of_int tree_id);
  v buf 4L;
  Buffer.add_char buf (Char.chr origin_byte);
  List.iter (fun is_expr -> v buf (if is_expr then 1L else 0L)) expr_flags;
  (match where_sql with
   | None -> v buf 0L
   | Some sql ->
     v buf 1L;
     v buf (Int64.of_int (String.length sql));
     Buffer.add_string buf sql);
  Buffer.add_char buf '\x01';
  v buf (Int64.of_int distinct_count);
  v buf (Int64.of_int rows_at_analysis);
  Buffer.to_bytes buf
;;

let test_decode_index_v4_backward_compat_576 () =
  run
    (let store = S.create () in
     let v4_bytes =
       make_v4_index_bytes
         ~name:"idx_v4_legacy"
         ~table:"legacy_t2"
         ~columns:[ "a" ]
         ~unique:false
         ~tree_id:11
         ~origin_byte:2
         ~expr_flags:[ false ]
         ~where_sql:None
         ~distinct_count:5
         ~rows_at_analysis:50
     in
     let* tx = S.rw_begin store in
     let* () = S.put tx sys_indexes_tid (Bytes.of_string "v4_key") v4_bytes in
     let* () = S.commit tx in
     let* cat = C.open_ store in
     (match C.find_index cat ~name:"idx_v4_legacy" with
      | Some { C.idx_stats = Some { C.distinct_count = 5; rows_at_analysis = 50; histogram = None }; _ } ->
        ()
      | Some { C.idx_stats = None; _ } ->
        Alcotest.fail "expected idx_stats to decode from v4 bytes"
      | Some { C.idx_stats = Some { C.histogram = Some _; _ }; _ } ->
        Alcotest.fail "v4 bytes must decode with histogram = None"
      | None -> Alcotest.fail "idx_v4_legacy not found");
     Lwt.return_unit)
;;
```

Register all four new tests in the same `Alcotest.run`/test-list block that already registers `test_index_stats_roundtrip_576` etc. (`:2933`-ish) — add each as its own `Alcotest.test_case "..." `Quick <fn>` entry, following the existing entries' exact style in that list.

- [ ] **Step 7: Run the new tests, expect FAIL (nothing calls `set_index_stats` with the new signature yet — this is fine, it's Task 2's job; if the build itself fails here rather than these specific tests, stop and fix Task 1 first)**

This step is really "confirm Task 1 alone doesn't build" (already shown in Step 5) — do not attempt to make `dune test` pass yet. Commit Task 1 as-is; Task 2 makes it compile.

- [ ] **Step 8: Commit**

```sh
git add lib/catalog/catalog.mli lib/catalog/catalog.ml test/test_catalog.ml
git commit -m "perf(#576): add histogram to index_stats, v5 encode/decode, round-trip tests"
```

---

### Task 2: Extend `Cat.set_index_stats` to carry the histogram

**Files:**
- Modify: `lib/catalog/catalog.mli:409-424` (`set_index_stats` signature + doc comment)
- Modify: `lib/catalog/catalog.ml:3200-3216` (`set_index_stats` implementation)

**Interfaces:**
- Consumes: `histogram` type from Task 1 (`catalog.mli`/`catalog.ml`).
- Produces: `Cat.set_index_stats : t -> _ txn -> name:string -> distinct_count:int -> rows_at_analysis:int -> histogram:histogram option -> unit Lwt.t`. Task 3 (`Exec.execute_create_index`) is the only production caller and will pass `~histogram` computed there.

- [ ] **Step 1: Update the `.mli` signature**

Replace `catalog.mli:409-424`:

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

with:

```ocaml
(** #576 tier 1/2: persist [idx_stats] on the named index's catalog row —
    the leading-column distinct-value count, the row count observed while
    computing it, and (#576 tier 2) an optional equi-depth histogram of the
    same column's encoded values.  Must run inside [tx]: the sole caller,
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
  -> histogram:histogram option
  -> unit Lwt.t
```

- [ ] **Step 2: Update the implementation**

Replace `catalog.ml:3200-3216`:

```ocaml
let set_index_stats t tx ~name ~distinct_count ~rows_at_analysis =
  match Schema_cache.find_index t.sc name with
  | None -> Lwt.return_unit
  | Some info ->
    let%lwt idxs = indexes_of_table_tx tx ~table:info.idx_table in
    (match
       List.find_opt (fun (_, (i : index_info)) -> String.equal i.idx_name name) idxs
     with
     | None -> Lwt.return_unit
     | Some (k, _) ->
       let new_info =
         { info with idx_stats = Some { distinct_count; rows_at_analysis } }
       in
       let%lwt () = S.put tx sys_indexes_tid k (encode_index_value new_info) in
       Schema_cache.put_index t.sc ~name new_info;
       Lwt.return_unit)
;;
```

with:

```ocaml
let set_index_stats t tx ~name ~distinct_count ~rows_at_analysis ~histogram =
  match Schema_cache.find_index t.sc name with
  | None -> Lwt.return_unit
  | Some info ->
    let%lwt idxs = indexes_of_table_tx tx ~table:info.idx_table in
    (match
       List.find_opt (fun (_, (i : index_info)) -> String.equal i.idx_name name) idxs
     with
     | None -> Lwt.return_unit
     | Some (k, _) ->
       let new_info =
         { info with idx_stats = Some { distinct_count; rows_at_analysis; histogram } }
       in
       let%lwt () = S.put tx sys_indexes_tid k (encode_index_value new_info) in
       Schema_cache.put_index t.sc ~name new_info;
       Lwt.return_unit)
;;
```

- [ ] **Step 3: Fix Task 1's two remaining test calls if they still use the old arity**

Task 1's Step 6 tests already pass `~histogram`, so this should be a no-op — just re-run the build to confirm.

- [ ] **Step 4: Build (still expect one failure: `exec.ml`'s call site, Task 3's job)**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build`

Expected: FAIL, isolated to `Exec.execute_create_index`'s call to `Cat.set_index_stats` in `exec.ml` (`:4968-4973`) missing the new `~histogram` argument.

- [ ] **Step 5: Run `test_catalog.ml`'s new tests from Task 1**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_catalog.exe`

Expected: this specific test binary FAILS to build too, for the same reason as Step 4 if `test_catalog.ml` itself doesn't compile standalone from the whole-project build failure — in this monorepo-style dune setup a single broken file typically blocks the whole `dune build`, so this step may just reconfirm Step 4's failure. That is fine; Task 3 clears it.

- [ ] **Step 6: Commit**

```sh
git add lib/catalog/catalog.mli lib/catalog/catalog.ml
git commit -m "perf(#576): thread ~histogram through Cat.set_index_stats"
```

---

### Task 3: Populate the histogram in `execute_create_index`'s walk

**Files:**
- Modify: `lib/sql/exec.ml:4797-4973` (`index_stats_cardinality_cap`, `execute_create_index`)
- Test: `test/test_index_cardinality_576.ml` (new tests, following the existing `non_unique_index_gets_analyzed`/`unique_index_is_exempt` pattern)

**Interfaces:**
- Consumes: `Cat.set_index_stats`'s new `~histogram` parameter (Task 2); `histogram`/`index_stats` types (Task 1).
- Produces: `histogram_bucket_count : int` (a new named constant in `exec.ml`, next to `index_stats_cardinality_cap`) — Task 4/5 reference it only in doc comments and tests, not in code (consumption divides by the *actual* array length, per Task 1's `histogram` doc comment, not this constant).

- [ ] **Step 1: Add the bucket-count constant**

In `exec.ml`, right after `let index_stats_cardinality_cap = 100_000` (`:4800`):

```ocaml
(** #576 tier 2: how many equi-depth (by row count) buckets
    [execute_create_index]'s walk targets when building a leading-column
    histogram. 20 buckets is ~5% CDF resolution — enough to separate "this
    range covers a small slice of the table" from "this range covers most of
    it" (#561's row-4 residual class of mis-estimate) without a
    variable-resolution scheme to justify a different number. A single
    skewed value can still make the actual persisted histogram shorter than
    [histogram_bucket_count + 1] entries — see [histogram]'s doc comment in
    catalog.mli — so nothing downstream may assume the array has exactly
    this many buckets; they must read [Array.length boundaries - 1]. *)
let histogram_bucket_count = 20
```

- [ ] **Step 2: Change the walk's `seen` table from presence to counts**

In `execute_create_index` (`exec.ml:4868-4872`), the current code is:

```ocaml
      let seen =
        if unique || without_rowid_table || leading_col_is_expr
        then None
        else Some (Hashtbl.create 64)
      in
```

This type doesn't need to change its declaration (still `Hashtbl.create 64`), but its *usage* at `:4907-4915` does. Replace:

```ocaml
            (match seen with
             | None -> ()
             | Some tbl ->
               if not !capped
               then (
                 let ek = Index_key.encode_value (List.hd iks) in
                 if Hashtbl.mem tbl ek || Hashtbl.length tbl < index_stats_cardinality_cap
                 then Hashtbl.replace tbl ek ()
                 else capped := true));
```

with (encode key bytes to a `string` via `Bytes.to_string`, since `Hashtbl`'s default polymorphic hash/equality prefers immutable keys and Task 1's `histogram.boundaries : string array` already commits to strings — keep the table's key type consistent with what gets persisted):

```ocaml
            (match seen with
             | None -> ()
             | Some tbl ->
               if not !capped
               then (
                 let ek = Bytes.to_string (Index_key.encode_value (List.hd iks)) in
                 match Hashtbl.find_opt tbl ek with
                 | Some count -> Hashtbl.replace tbl ek (count + 1)
                 | None ->
                   if Hashtbl.length tbl < index_stats_cardinality_cap
                   then Hashtbl.replace tbl ek 1
                   else capped := true));
```

- [ ] **Step 3: Write the histogram-building function**

Add a new top-level function just above `execute_create_index` (after `index_stats_cardinality_cap`/`histogram_bucket_count`):

```ocaml
(** #576 tier 2: build an equi-depth (by row count) histogram from
    [entries] — the [(encoded_key, row_count)] pairs
    [execute_create_index]'s walk collected, unsorted, for a leading indexed
    column. [total_rows] is the sum of every entry's count (equivalently,
    [rows_at_analysis]). [None] when [entries] has fewer than
    [histogram_bucket_count] distinct keys — see [histogram]'s doc comment
    in catalog.mli for why that floor exists.

    The boundary array is built by sorting [entries] by key (byte order —
    the same order the index itself sorts by) and walking the sorted list
    while accumulating a running row total; each time the running total
    crosses a multiple of [total_rows / histogram_bucket_count], the current
    key is emitted as an interior boundary, up to [histogram_bucket_count - 1]
    of them.  The first and last keys are always prepended/appended, so the
    result spans the full observed range even when a single skewed key's
    count overshoots several bucket-widths in one step (it still only
    contributes ONE boundary — this is the standard equi-depth degenerate
    case, not a bug: see [histogram]'s doc comment on why a consumer must
    read the actual array length rather than assume
    [histogram_bucket_count + 1]). *)
let build_histogram (entries : (string * int) list) ~total_rows : Cat.histogram option =
  if List.length entries < histogram_bucket_count
  then None
  else (
    let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) entries in
    let step = total_rows / histogram_bucket_count in
    let running = ref 0 in
    let next_threshold = ref step in
    let interior = ref [] in
    List.iter
      (fun (key, count) ->
         running := !running + count;
         if !running >= !next_threshold && List.length !interior < histogram_bucket_count - 1
         then (
           interior := key :: !interior;
           next_threshold := !next_threshold + step))
      sorted;
    let first_key = fst (List.hd sorted) in
    let last_key = fst (List.nth sorted (List.length sorted - 1)) in
    let mids = List.rev !interior in
    Some { Cat.boundaries = Array.of_list ((first_key :: mids) @ [ last_key ]) })
;;
```

- [ ] **Step 4: Call it from `execute_create_index` and pass `~histogram` to `set_index_stats`**

Replace the tail of `execute_create_index` (`exec.ml:4956-4973`):

```ocaml
      (* #576 tier 1: persist the stat in the same DDL transaction as the
         index itself, so it rolls back with it. *)
      (match seen with
       | None -> Lwt.return_unit
       | Some _ when !capped ->
         (* #576 final review: capped mid-walk -- the count in [tbl] is a
            partial, under-reported [distinct_count], and persisting it would
            estimate too FEW rows for the seek it gates (the unsafe,
            admitting direction). Fall back to no stat, same as an
            unanalyzed index. *)
         Lwt.return_unit
       | Some tbl ->
         Cat.set_index_stats
           cat
           tx
           ~name
           ~distinct_count:(Hashtbl.length tbl)
           ~rows_at_analysis:!rows_indexed))
;;
```

with:

```ocaml
      (* #576 tier 1/2: persist the stats in the same DDL transaction as the
         index itself, so they roll back with it. *)
      (match seen with
       | None -> Lwt.return_unit
       | Some _ when !capped ->
         (* #576 final review: capped mid-walk -- the count in [tbl] is a
            partial, under-reported [distinct_count] (and the entries it does
            hold are an arbitrary, non-representative subset for a
            histogram), and persisting either would bias the estimate toward
            the unsafe ADMITTING direction. Fall back to no stat at all,
            same as an unanalyzed index. *)
         Lwt.return_unit
       | Some tbl ->
         let entries = Hashtbl.fold (fun k c acc -> (k, c) :: acc) tbl [] in
         let histogram = build_histogram entries ~total_rows:!rows_indexed in
         Cat.set_index_stats
           cat
           tx
           ~name
           ~distinct_count:(Hashtbl.length tbl)
           ~rows_at_analysis:!rows_indexed
           ~histogram))
;;
```

- [ ] **Step 5: Build**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build`

Expected: PASS (this was the last remaining call site missing `~histogram`).

- [ ] **Step 6: Run the existing tier-1 suite to confirm no regression**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_catalog.exe test/test_index_cardinality_576.exe test/test_join_cost_model_576.exe`

Expected: PASS — Task 1's new tests pass now that `set_index_stats` has a real `~histogram` argument; tier 1's existing tests are unaffected because `distinct_count`/`rows_at_analysis` computation didn't change, only `seen`'s value type (presence → count) and the addition of a histogram alongside it.

- [ ] **Step 7: Write population tests in `test_index_cardinality_576.ml`**

Add near `non_unique_index_gets_analyzed` (`:46`):

```ocaml
(* #576 tier 2: same skewed seed as [non_unique_index_gets_analyzed] --
   n_distinct = 50 >= histogram_bucket_count = 20, so a histogram is built.
   Assert its shape rather than exact boundary values: spans the full
   min/max tenant_id range and has at most 21 entries. *)
let non_unique_index_gets_a_histogram () =
  with_db (fun db ->
    seed_skewed db;
    exec db "CREATE INDEX idx_t_tenant ON t(tenant_id)";
    match idx_stats db "idx_t_tenant" with
    | None -> Alcotest.fail "expected idx_stats to be populated"
    | Some { Cat.histogram = None; _ } -> Alcotest.fail "expected a histogram"
    | Some { Cat.histogram = Some { Cat.boundaries }; _ } ->
      Alcotest.(check bool) "at least 2 boundaries" true (Array.length boundaries >= 2);
      Alcotest.(check bool)
        "at most bucket_count + 1 boundaries"
        true
        (Array.length boundaries <= 21))
;;

(* #576 tier 2: a column with fewer distinct values than
   [Exec.histogram_bucket_count] (=20) gets no histogram, even though it
   still gets [distinct_count] -- see [histogram]'s doc comment in
   catalog.mli for why the two floors differ. *)
let few_distinct_values_gets_no_histogram () =
  with_db (fun db ->
    exec db "CREATE TABLE few (k INTEGER, v INTEGER)";
    exec db "BEGIN";
    for i = 1 to 1000 do
      exec db (Printf.sprintf "INSERT INTO few VALUES (%d, %d)" (i mod 5) i)
    done;
    exec db "COMMIT";
    exec db "CREATE INDEX idx_few_k ON few(k)";
    match idx_stats db "idx_few_k" with
    | None -> Alcotest.fail "expected idx_stats (distinct_count) to be populated"
    | Some { Cat.distinct_count = 5; histogram = None; _ } -> ()
    | Some { Cat.histogram = Some _; _ } ->
      Alcotest.fail "5 distinct values must not produce a histogram"
    | Some { Cat.distinct_count; _ } ->
      Alcotest.failf "expected distinct_count = 5, got %d" distinct_count)
;;

(* #576 tier 2: same over-cap seed as [cardinality_above_the_cap_falls_back_to_no_stat]
   -- above the cap, idx_stats is None entirely, so obviously no histogram
   either. This pins that the histogram code path doesn't somehow run on the
   partial, capped table. *)
let cardinality_above_the_cap_has_no_histogram_either () =
  with_db (fun db ->
    seed_over_cap db;
    exec db "CREATE INDEX idx_big_v2 ON big(v)";
    match idx_stats db "idx_big_v2" with
    | None -> ()
    | Some _ -> Alcotest.fail "expected idx_stats = None (and hence no histogram) above the cap")
;;
```

Register the three new tests alongside the existing `Alcotest.test_case` list at the bottom of `test_index_cardinality_576.ml` (find the `let () = Alcotest.run ...` or test-list block and follow its exact existing style/naming).

- [ ] **Step 8: Run the new tests**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_index_cardinality_576.exe`

Expected: PASS.

- [ ] **Step 9: `sh scripts/check-fmt.sh` and fix any diffs**

Run: `sh scripts/check-fmt.sh` (from the worktree root). If it reports deviations, run `sh scripts/check-fmt.sh --fix` and re-check the diff looks sane before committing.

- [ ] **Step 10: Commit**

```sh
git add lib/sql/exec.ml test/test_index_cardinality_576.ml
git commit -m "perf(#576): populate leading-column histogram in CREATE INDEX's walk"
```

---

### Task 4: Generalize `range_rows_estimate` to consult the histogram

**Files:**
- Modify: `lib/sql/planner.ml:894-917` (`range_int_literal_span`, `range_rows_estimate`) and its one call site at `:1336`
- Modify: `lib/sql/planner.ml:1` (module aliases, if `Index_key` isn't already aliased)

**Interfaces:**
- Consumes: `index_by_tree` (`planner.ml:1128`, already exists — same lookup `estimate_rows_from_stats` uses), `Cat.index_stats.histogram`/`Cat.histogram.boundaries` (Task 1), `Index_key.encode_value` (already used elsewhere in the codebase, e.g. `exec.ml:212`).
- Produces: `range_rows_estimate : Cat.t -> Cat.table_meta -> idx_tree:int -> Plan.range -> int` (signature change — was `Plan.range -> int`). `estimate_rows`'s `Op_index_lookup` branch (planner.ml:1336) is the only caller and is updated in this task, so no other task depends on the old 1-argument signature.

- [ ] **Step 1: Add the `Index_key` module alias**

Check whether `planner.ml` already has one:

```sh
grep -n "^module Index_key" lib/sql/planner.ml
```

If absent, add near the top (`planner.ml:1-2`, alongside `module Cat = ...` / `module Row = ...`):

```ocaml
module Index_key = Granary_encoding.Index_key
```

- [ ] **Step 2: Write the boundary-lookup helper**

Add a new function just before `range_rows_estimate` (after `range_int_literal_span`, `planner.ml:894-902`):

```ocaml
(** #576 tier 2: the smallest index [i] into [boundaries] such that
    [boundaries.(i) >= key] (byte order) — [Array.length boundaries] if
    [key] is greater than every boundary. A standard lower-bound binary
    search; [boundaries] is assumed sorted ascending, which
    [Exec.build_histogram] guarantees. *)
let histogram_lower_bound (boundaries : string array) (key : string) =
  let n = Array.length boundaries in
  let rec go lo hi =
    if lo >= hi
    then lo
    else (
      let mid = lo + ((hi - lo) / 2) in
      if String.compare boundaries.(mid) key < 0 then go (mid + 1) hi else go lo mid)
  in
  go 0 n
;;

(** #576 tier 2: estimate a literal [Integer]/[Real] range's row count from
    [idx_tree]'s leading-column histogram, or [None] when no histogram is
    available or the range has no literal bound to look up (a bound
    parameter, or a bound of any type other than [L_int]/[L_real] --
    {!Plan.range} only exists for [Integer]/[Real] columns in the first
    place, see the design doc's scope-boundary section, so this never sees
    text/blob). [None] on EITHER end falls back whole to
    {!range_int_literal_span}: locating a literal bound against the
    histogram tells you nothing about where an unbound parameter's runtime
    value will fall, so a range with one literal end and one parameter end
    gets today's flat estimate exactly as it did before this function
    existed.

    Deliberately BUCKET-GRANULARITY, not linear interpolation within a
    straddled bucket: [boundaries] holds raw encoded bytes, and interpolating
    "how far into this bucket" a value falls would mean decoding those bytes
    back into a comparable number, which needs a type-specific branch this
    function has no other reason to add. The estimate divides by
    [Array.length boundaries - 1] (the ACTUAL bucket count), never by
    {!Exec.histogram_bucket_count} — see [histogram]'s doc comment in
    catalog.mli for why a persisted histogram's bucket count can be smaller
    than the constant used at population time. *)
let range_histogram_estimate cat (meta : Cat.table_meta) ~idx_tree (r : Plan.range) =
  match index_by_tree cat meta ~idx_tree with
  | None -> None
  | Some { Cat.idx_stats = Some { Cat.histogram = Some { Cat.boundaries }; rows_at_analysis; _ }; _ }
    when Array.length boundaries >= 2 ->
    let lit_key = function
      | Some (Plan.P_lit (Ast.L_int n)) -> Some (Bytes.to_string (Index_key.encode_value (Index_key.IK_int n)))
      | Some (Plan.P_lit (Ast.L_real f)) ->
        Some (Bytes.to_string (Index_key.encode_value (Index_key.IK_real f)))
      | None -> None (* unbounded end: use the histogram's own extreme, handled below *)
      | Some _ -> None (* a parameter, or any other expr shape: no literal to look up *)
    in
    let n_buckets = Array.length boundaries - 1 in
    let lo_i =
      match r.Plan.r_lo with
      | None -> Some 0
      | Some _ as e -> (match lit_key e with Some k -> Some (histogram_lower_bound boundaries k) | None -> None)
    in
    let hi_i =
      match r.Plan.r_hi with
      | None -> Some n_buckets
      | Some _ as e -> (match lit_key e with Some k -> Some (histogram_lower_bound boundaries k) | None -> None)
    in
    (match lo_i, hi_i with
     | Some lo_i, Some hi_i when r.Plan.r_lo <> None || r.Plan.r_hi <> None ->
       let span_buckets = max 0 (hi_i - lo_i) in
       Some (max range_seek_rows (rows_at_analysis * span_buckets / n_buckets))
     | _ -> None)
  | _ -> None
;;
```

- [ ] **Step 3: Change `range_rows_estimate`'s signature to consult it first**

Replace `planner.ml:904-917`:

```ocaml
let range_rows_estimate (r : Plan.range) =
  match range_int_literal_span r with
  | Some (lo, hi) ->
    let span = Int64.sub hi lo in
    (* [hi < lo] is an empty window; a [span] that came out negative for the
       other reason — [hi - lo] overflowing int64 — is as unbounded as a range
       gets.  Both are handled by the sign test, in the direction each wants. *)
    if Int64.compare span 0L < 0
    then if Int64.compare hi lo < 0 then range_seek_rows else unbounded_rows
    else if Int64.compare span (Int64.of_int unbounded_rows) >= 0
    then unbounded_rows
    else max range_seek_rows (Int64.to_int span + 1)
  | None -> range_seek_rows
;;
```

with:

```ocaml
(** #576 tier 2: [cat]/[meta]/[idx_tree] identify the seeked index, exactly
    as {!estimate_rows_from_stats} already does — added so this can consult
    {!range_histogram_estimate} before falling back to the pre-existing
    integer-literal-span logic. See that function's doc comment for the
    fallback rules (parameter bounds, missing histograms, etc. all keep
    today's answer unchanged). *)
let range_rows_estimate cat (meta : Cat.table_meta) ~idx_tree (r : Plan.range) =
  match range_histogram_estimate cat meta ~idx_tree r with
  | Some est -> est
  | None ->
    (match range_int_literal_span r with
     | Some (lo, hi) ->
       let span = Int64.sub hi lo in
       (* [hi < lo] is an empty window; a [span] that came out negative for the
          other reason — [hi - lo] overflowing int64 — is as unbounded as a
          range gets.  Both are handled by the sign test, in the direction each
          wants. *)
       if Int64.compare span 0L < 0
       then if Int64.compare hi lo < 0 then range_seek_rows else unbounded_rows
       else if Int64.compare span (Int64.of_int unbounded_rows) >= 0
       then unbounded_rows
       else max range_seek_rows (Int64.to_int span + 1)
     | None -> range_seek_rows)
;;
```

- [ ] **Step 4: Update the one call site**

At `planner.ml:1336` (inside `estimate_rows`'s `Op_index_lookup` branch), change:

```ocaml
        | Some r -> range_rows_estimate r
```

to:

```ocaml
        | Some r -> range_rows_estimate cat table_meta ~idx_tree r
```

(`cat`, `table_meta`, `idx_tree` are already destructured in scope from the surrounding `match op with | Plan.Op_index_lookup { idx_tree; keys; range; table_meta; _ } ->` at `planner.ml:1326`.)

- [ ] **Step 5: Build**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build`

Expected: PASS. If there's a second call site to `range_rows_estimate` this plan missed, fix it the same way (thread `cat`/`meta`/`idx_tree` from its surrounding scope) — but per the design doc's "Consumption" section, `build_side_seek_is_unambiguous` deliberately does NOT call `range_rows_estimate`, so there should be exactly one.

- [ ] **Step 6: Run the full existing planner/join-cost/build-side-range suites to confirm no regression**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_planner.exe test/test_join.exe test/test_join_cost_model_576.exe test/test_build_side_range_532.exe test/test_range_bound_517.exe`

Expected: PASS — every existing test either has a `P_param` bound, no literal-int/real range at all, or an unanalyzed index (no histogram), all of which fall through to the unchanged `range_int_literal_span` path.

- [ ] **Step 7: `sh scripts/check-fmt.sh` and fix any diffs**

- [ ] **Step 8: Commit**

```sh
git add lib/sql/planner.ml
git commit -m "perf(#576): consult per-index histogram in range_rows_estimate"
```

---

### Task 5: Consumption tests — plan-shape assertions

**Files:**
- Create: `test/test_range_histogram_576.ml`
- Modify: `test/dune` (register the new test executable, following whatever pattern the existing `test_index_cardinality_576`/`test_join_cost_model_576` entries use)

**Interfaces:**
- Consumes: `Planner.plan`, `Sema.bind` (both already public — see `test_planner.ml`'s header for the exact module aliases/`bind` helper), `Db` (to seed real data and run real `CREATE INDEX` so the histogram is genuinely populated by Task 3's code, not hand-constructed).

- [ ] **Step 1: Check how `test_planner.ml` gets a bound statement + plan from raw SQL text, to reuse rather than reinvent**

```sh
grep -n "let parse\|Sema.bind\|Planner.plan " test/test_planner.ml | head -20
```

Follow whatever `parse`/`bind`/`plan` pipeline it already has (the plan assumes `Ast.parse`, `Sema.bind cat`, `Planner.plan ~cat` — same three calls `Db.prepare` makes at `db.ml:2712-2720` — but confirm the exact helper names/signatures in `test_planner.ml` before writing Step 3, since this repo's test helpers sometimes wrap these with a `bind cat stmt` one-liner as shown in the file's header excerpt gathered during planning.

- [ ] **Step 2: Write the file header and harness**

```ocaml
(** #576 tier 2: [Planner.range_rows_estimate] consulting a per-index
    histogram must be visible in the PLAN SHAPE it produces, since the
    function itself isn't exposed via [Planner.mli] (mirrors tier 1's own
    [test_index_cardinality_576.ml], which reads [idx_stats] off the catalog
    rather than calling a private planner function directly).

    This seeds real data through [Db], runs a real [CREATE INDEX] (so
    [Exec.execute_create_index]'s walk populates the histogram exactly as
    production does — nothing here hand-constructs a [Cat.histogram]), then
    reaches into [Db.catalog] to call [Sema.bind] + [Planner.plan] directly
    on a JOIN whose build-side-seek admission
    ([Planner.build_side_seek_is_unambiguous], unchanged by this tier but
    fed by [estimate_rows] -> [range_rows_estimate]) flips between "declined"
    (today's flat [range_seek_rows] = 100 estimate exceeds the seek budget)
    and "admitted" (a histogram-corrected estimate that reflects a small,
    real fraction of the table falls under it) depending on whether a
    histogram is available for the seeked index. *)

module Db = Granary.Db
module Cat = Granary_catalog.Catalog
module Sema = Granary_sql.Sema
module Ast = Granary_sql.Ast
module Plan = Granary_sql.Plan
module Planner = Granary_sql.Planner

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

let bind_and_plan cat sql =
  let ast =
    match Granary_sql.Parser_driver.parse sql with
    | Ok a -> a
    | Error e -> Alcotest.failf "parse %S: %s" sql e
  in
  let bound =
    match run (Sema.bind cat ast) with
    | Ok b -> b
    | Error e -> Alcotest.failf "bind %S: failed" sql
  in
  Planner.plan ~cat bound
;;
```

(Replace `Granary_sql.Parser_driver.parse`/error-shape assumptions in `bind_and_plan` with whatever `test_planner.ml`'s own `parse`/`bind` helpers actually are, confirmed in Step 1 — this plan's best guess based on `db.ml:2712-2720`'s pipeline, but the exact module/function names must come from the real test file, not this plan.)

- [ ] **Step 3: Write the histogram-vs-flat-estimate flip test**

The shape: a `stock`-like target table with a `REAL` leading indexed column, skewed so a small literal range covers a small fraction of the table but #561's machinery (integer-literal-only) could never have known that even if the column were an integer — and a driving table whose row count clears `nlj_min_driving_rows` (1000) but whose build-side-seek budget (`table_rows_estimate / build_side_seek_break_even_ratio`, 200) the flat `range_seek_rows = 100` estimate straddles right at the edge of admitting/declining, so a real, histogram-corrected estimate changes the outcome:

```ocaml
let n_target = 20_000

(* A REAL column: 20,000 rows, values 0.0 .. 19999.0, one row per value
   (dense, not skewed -- skew isn't needed to make the point: #561's own
   logic literally cannot run on a REAL column at all, so ANY real
   histogram-based estimate here is strictly new information, not just a
   more accurate one). *)
let seed_target db =
  exec db "CREATE TABLE tgt (v REAL, payload INTEGER)";
  exec db "BEGIN";
  for i = 0 to n_target - 1 do
    exec db (Printf.sprintf "INSERT INTO tgt VALUES (%d.0, %d)" i i)
  done;
  exec db "COMMIT"
;;

let n_driving = 1_200

let seed_driving db =
  exec db "CREATE TABLE drv (v REAL)";
  exec db "BEGIN";
  for i = 0 to n_driving - 1 do
    exec db (Printf.sprintf "INSERT INTO drv VALUES (%d.0)" i)
  done;
  exec db "COMMIT"
;;

(* [table_seek_budget] for a 20,000-row table is 20,000 / 200 = 100. A
   literal real-range window of exactly 90 keys (99.0 .. 8.0 -- i.e. 90.0
   down to 0.0? use an explicit ascending literal pair) sits comfortably
   under that budget once the histogram gives the TRUE 90-row estimate. The
   pre-tier-2 flat estimate for a REAL range is [range_seek_rows] = 100,
   which is ALSO under 100 -- so this specific window can't distinguish the
   two paths by admission alone; the assertion instead checks that
   [Op_hash_join]'s build side is [Op_index_lookup] with the seek engaged in
   BOTH the histogrammed and unanalyzed case, and separately pins the
   histogram exists after CREATE INDEX (Task 3's own tests already cover
   that) -- so this test's real job is the WIDER window below, which the
   flat 100 estimate cannot admit but a correct ~2000-row-window histogram
   estimate also correctly DECLINES, proving the histogram path is live and
   answering something other than the constant. *)
let wide_window_is_declined_via_histogram_not_luck () =
  with_db (fun db ->
    seed_target db;
    seed_driving db;
    exec db "CREATE INDEX idx_tgt_v ON tgt(v)";
    let cat = Db.catalog db in
    (* window v BETWEEN 0.0 AND 1999.0 = 2000 keys, well over the 100-row
       seek budget for a 20,000-row table -- the histogram must say so (the
       flat range_seek_rows = 100 constant would ALSO decline this, so this
       alone doesn't prove the histogram ran; paired with
       histogram_is_populated_after_create_index below, which confirms a
       histogram exists, and estimate_matches_the_seeded_distribution, which
       confirms decode->consult round-trips a real number, together they
       pin that this specific decision routes through the histogram path
       rather than an accidental identical answer). *)
    let op =
      bind_and_plan
        cat
        "SELECT drv.v FROM drv JOIN tgt ON drv.v = tgt.payload WHERE tgt.v BETWEEN 0.0 AND \
         1999.0"
    in
    match op with
    | Plan.Op_project { child = Plan.Op_hash_join { right; _ }; _ } ->
      (match right with
       | Plan.Op_seq_scan _ -> ()
       | Plan.Op_index_lookup _ ->
         Alcotest.fail "expected the wide window's seek to be declined (fall back to a scan)"
       | _ -> Alcotest.fail "unexpected build-side (right) op shape")
    | _ -> Alcotest.fail "expected Op_project(Op_hash_join _)")
;;
```

`Op_hash_join`'s build side is its `right : op` field — confirmed at `plan.mli:328-345` (`build_side`, `planner.ml:1790`, constructs the hash join with the seeked/scanned table as `right`). `Op_seq_scan`/`Op_index_lookup` are `plan.mli:194`/`:247`.

**Sanity-check this test is not vacuous** before trusting it: temporarily short-circuit `range_histogram_estimate` to always return `None`, confirm `wide_window_is_declined_via_histogram_not_luck` and `histogram_is_populated_after_create_index` still pass (they should — a `None` histogram estimate just falls back to the pre-existing flat/int-span logic, which also declines this window), then also temporarily seed a case that only a histogram can decline correctly (a window the flat `range_seek_rows = 100` constant would UNDER-estimate as admittable but the true row count exceeds the budget) to confirm at least one assertion in this file fails without the histogram path — then revert both temporary changes. If no such case exists among the tests as planned, add one: e.g. a `REAL` window of ~150 keys (`v BETWEEN 0.0 AND 149.0` on the 20,000-row `tgt` table) sits right at the `table_seek_budget` = 100 boundary and only the histogram (not the pre-existing flat 100, which is itself right at the same boundary and would need an off-by-one to distinguish) can tell it apart reliably — if this specific window doesn't cleanly separate the two paths when actually run, adjust the window size empirically until one does, and keep whichever value the real run shows separates them.

- [ ] **Step 4: Write a direct histogram-population sanity test (no plan-shape needed)**

```ocaml
let idx_stats db name =
  match Cat.find_index (Db.catalog db) ~name with
  | None -> Alcotest.failf "index %S not found" name
  | Some i -> i.Cat.idx_stats
;;

let histogram_is_populated_after_create_index () =
  with_db (fun db ->
    seed_target db;
    exec db "CREATE INDEX idx_tgt_v2 ON tgt(v)";
    match idx_stats db "idx_tgt_v2" with
    | None -> Alcotest.fail "expected idx_stats"
    | Some { Cat.histogram = None; _ } -> Alcotest.fail "expected a histogram for a REAL column"
    | Some { Cat.histogram = Some { Cat.boundaries }; _ } ->
      Alcotest.(check bool) "spans a real range" true (Array.length boundaries >= 2))
;;
```

- [ ] **Step 5: Write the parameter-bound regression test**

```ocaml
(* #576 tier 2 scope boundary: a range with a bound parameter must be
   UNCHANGED from main -- the histogram cannot know a parameter's runtime
   value at plan time. Pin this by checking the SAME wide window declines
   the seek whether the upper bound is a literal or a parameter -- if a
   future change accidentally made range_histogram_estimate "peek" at a
   parameter (which it structurally cannot -- Plan.P_param carries only an
   index, no value -- but this guards the intent even if the code changes),
   this would catch the resulting mismatch by asserting they still agree. *)
let parameter_bound_range_is_unaffected () =
  with_db (fun db ->
    seed_target db;
    seed_driving db;
    exec db "CREATE INDEX idx_tgt_v3 ON tgt(v)";
    let cat = Db.catalog db in
    let op =
      bind_and_plan
        cat
        "SELECT drv.v FROM drv JOIN tgt ON drv.v = tgt.payload WHERE tgt.v BETWEEN 0.0 AND ?"
    in
    match op with
    | Plan.Op_project { child = Plan.Op_hash_join { right = Plan.Op_seq_scan _; _ }; _ } -> ()
    | Plan.Op_project { child = Plan.Op_hash_join { right = Plan.Op_index_lookup _; _ }; _ } ->
      Alcotest.fail
        "a parameterized upper bound must not be admitted via the histogram -- the value is \
         unknown at plan time"
    | _ -> Alcotest.fail "expected Op_project(Op_hash_join _)")
;;
```

- [ ] **Step 6: Register the test executable**

```sh
grep -n "test_index_cardinality_576\|test_join_cost_model_576" test/dune
```

Add a matching `(test (name test_range_histogram_576) (libraries ...))` (or equivalent stanza per this repo's `test/dune` conventions) block, copying the libraries list from `test_index_cardinality_576`'s or `test_join_cost_model_576`'s existing stanza (both need `Db`, `Cat`, `Sema`/`Ast`/`Plan`/`Planner`) and register the five test functions above in an `Alcotest.run` call at the bottom of the file, following that same nearby test file's exact list-registration style.

- [ ] **Step 7: Run the new suite**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_range_histogram_576.exe`

Expected: PASS. If `Op_hash_join`'s build-side field name (or any other assumed constructor) doesn't match, fix per Step 3's note before proceeding — do not weaken an assertion to make it pass without understanding why it was wrong.

- [ ] **Step 8: Run the FULL suite once, to catch anything this plan's per-task runs missed**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test`

Expected: PASS, all binaries.

- [ ] **Step 9: `sh scripts/check-fmt.sh` and merlint**

```sh
sh scripts/check-fmt.sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
```

Both must be clean (or `check-fmt.sh --fix`'d, then re-verified) before the branch is considered done. If `check-fmt.sh` reports dune files UNVERIFIED because `test/dune` was touched in a worktree, follow CLAUDE.md's "Before pushing" section: re-run `dune build @fmt` from the main checkout (not this worktree) to get real parity.

- [ ] **Step 10: Commit**

```sh
git add test/test_range_histogram_576.ml test/dune
git commit -m "perf(#576): plan-shape tests for histogram-consulted range estimates"
```

---

## After all five tasks

Push and open the PR per CLAUDE.md's "Opening a PR" section:

```sh
git push origin perf/576-column-histograms
~/.local/bin/forgejo pr create IoTReadyNext/granary \
  --title="perf(#576): tier 2 - per-index leading-column histograms" \
  --head=perf/576-column-histograms \
  --base=main \
  --body="$(cat <<'EOF'
## Summary
- Adds an optional equi-depth histogram to `Cat.index_stats`, encoded as version 5 (backward-compatible with v1-4, which decode `histogram = None`).
- Populated inside `CREATE INDEX`'s existing walk (Task 3) -- no new scan, same eligibility as tier 1's `distinct_count`, plus a floor of `histogram_bucket_count` (20) distinct values.
- `Planner.range_rows_estimate` consults it for a literal `Integer`/`Real` range (Task 4), replacing the flat `range_seek_rows = 100` fallback wherever a histogram is available. A parameter bound anywhere in the range keeps today's flat fallback unchanged -- `Planner.plan` runs once at prepare time, before any parameter is bound, so no statistic can size a window whose endpoints don't exist yet. See the design doc's scope-boundary section.

Design: `docs/superpowers/specs/2026-08-10-576-tier2-column-histograms-design.md`
Plan: `docs/superpowers/plans/2026-08-10-576-tier2-column-histograms.md`

Tier 2 of #576. Tier 1 (#711) and tier 3 (#705) already shipped.

## Test plan
- [ ] \`dune test\` passes (full suite, in-container)
- [ ] New tests: histogram round-trip + v4 backward-compat (\`test_catalog.ml\`), population tests (\`test_index_cardinality_576.ml\`), plan-shape consumption tests (\`test_range_histogram_576.ml\`)
- [ ] \`sh scripts/check-fmt.sh\` clean
- [ ] \`merlint\` clean (0 issues)
EOF
)"
```

Then run the standard code-review pass (per this repo's practice on every #576 tier so far: each task reviewed individually as it's built, plus one whole-branch review before merge) before merging.
