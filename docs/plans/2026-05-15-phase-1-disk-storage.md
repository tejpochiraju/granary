# Phase 1 — Disk Storage Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace Phase 0's in-memory `BytesMap` store with a real CoW B+-tree persisted through the `BLOCK` abstraction. Add a `Unix_file` block backend. Add `ORDER BY`, `LIMIT`, single-column `CREATE INDEX`, and the REAL + BLOB column types. Deliver multi-backend conformance: the full test suite passes against both `Mem` and `Unix_file`.

**Architecture ref:** `docs/specs/2026-05-10-design.md` — sections 4 (BLOCK), 5 (storage layer), 6.3–6.4 (encoding), 7.3–7.4 (planner/exec).

**Phase 0 outcome:** `BytesMap`-backed store behind `store.mli`, full SQL pipeline (CREATE TABLE / INSERT / SELECT WHERE), 351 tests passing, 97.57% handwritten coverage.

**Phase 1 scope — what changes:**
- `lib/block/unix_file.ml` — new BLOCK backend
- `lib/storage/` — new sub-library (page format, pager, B+-tree, header, txn)
- `lib/store/` — rewired to delegate to the B+-tree instead of `BytesMap`
- `lib/encoding/index_key.ml` — new; order-preserving key codec for indexes
- `lib/encoding/row.ml` — extended for REAL + BLOB types
- `lib/sql/ast.ml`, `lexer.mll`, `parser.mly` — extended with ORDER BY, LIMIT, CREATE INDEX, REAL, BLOB
- `lib/sql/sema.ml` — extended for new types, CREATE INDEX, ORDER BY / LIMIT semantics
- `lib/sql/plan.ml` / `planner.ml` — new ops: Sort, Limit, IndexLookup, IndexRangeScan
- `lib/sql/exec.ml` — implement Sort, Limit, IndexLookup; CREATE INDEX builds index
- `lib/catalog/catalog.ml` — extend for `_sys_indexes`, `_sys_meta`, REAL/BLOB type tags
- `test/` — new test files; multi-backend conformance runner

**Tech stack (unchanged):** OCaml 5.x · dune 3.x · alcotest · qcheck-alcotest · menhir · cstruct · lwt · Podman

**Testing discipline:** 100% behavioral coverage goal. Every module gets Alcotest unit tests + QCheck property tests. Slow and thorough — completeness over speed.

---

## Build environment (unchanged from Phase 0)

All builds run in the `sqlocaml-dev` Podman image. Containerfile is at the project root. Shell wrapper:

```bash
dune() { podman run --rm -v "$(pwd)":/workspace:Z -w /workspace sqlocaml-dev dune "$@"; }
export -f dune
```

The image already includes `bisect_ppx`. Rebuild only if `opam` dependencies change.

---

## Phase 1 file structure

New files to create (existing files are modified in-place):

```
lib/
├── block/
│   └── unix_file.ml          (new — BLOCK backend over a POSIX file)
│   └── unix_file.mli         (new)
├── storage/                  (new sub-library: sqlocaml.storage)
│   ├── dune
│   ├── page.ml               (page layout + codec — Cstruct-based)
│   ├── page.mli
│   ├── freelist.ml           (free page tracking with txn-id gating)
│   ├── freelist.mli
│   ├── pager.ml              (page cache LRU + allocator)
│   ├── pager.mli
│   ├── btree.ml              (CoW B+-tree: get/put/del/cursor)
│   ├── btree.mli
│   ├── header.ml             (alternating header pages + CRC32 commit)
│   └── header.mli
├── encoding/
│   └── index_key.ml          (new — order-preserving tuple codec)
│   └── index_key.mli         (new)
test/
├── test_unix_file.ml         (new)
├── test_page.ml              (new)
├── test_freelist.ml          (new)
├── test_pager.ml             (new)
├── test_btree.ml             (new — most thorough test file in the project)
├── test_header.ml            (new)
├── test_index_key.ml         (new)
├── test_store_btree.ml       (new — store.mli contract tests on btree backend)
├── test_conformance.ml       (new — same SQL suite on Mem + Unix_file)
```

Existing test files (`test_store.ml` etc.) continue running against the `Mem` backend.

---

## Tasks

---

### Task 1: `Unix_file` block backend

**Context:** The `BLOCK` module type is in `lib/block/block.mli`. `Mem` is the existing in-memory backend. `Unix_file` is a single-file POSIX backend that will be used for the B+-tree disk backend.

**Files to create/modify:**
- Create: `lib/block/unix_file.mli`
- Create: `lib/block/unix_file.ml`
- Modify: `lib/block/dune` — add `unix` to libraries

**Spec (`lib/block/unix_file.mli`):**

```ocaml
(** Unix file-backed BLOCK implementation.
    Opens or creates a file. Takes an OS-level flock(LOCK_EX) on open
    to prevent concurrent access from two processes. *)

type t
type error = Io of string | Out_of_bounds of { page_id: int64; n_pages: int64 }

val pp_error : Format.formatter -> error -> unit
val page_size : int  (* 4096 *)

val open_  : path:string -> (t, error) result Lwt.t
val close  : t -> (unit, error) result Lwt.t

val n_pages    : t -> int64
val read_page  : t -> page_id:int64 -> Cstruct.t -> (unit, error) result Lwt.t
val write_page : t -> page_id:int64 -> Cstruct.t -> (unit, error) result Lwt.t
val sync       : t -> (unit, error) result Lwt.t
val resize     : t -> n_pages:int64 -> (unit, error) result Lwt.t
```

**Implementation notes:**
- Use `Unix.openfile` with `[O_RDWR; O_CREAT]` and `0o644`.
- Track `n_pages` as `(file_size / page_size)` computed once at open, updated on `resize`.
- `read_page`: `lseek` to `page_id * 4096`, `read` exactly 4096 bytes into the Cstruct buffer.
- `write_page`: `lseek`, `write` exactly 4096 bytes from Cstruct buffer. Grow file first if `page_id >= n_pages`.
- `sync`: `Unix.fsync` on the file descriptor.
- `resize`: `ftruncate` to `n_pages * 4096`. Update internal `n_pages`.
- `flock(LOCK_EX | LOCK_NB)`: take on open, release in close. Return `Io "already locked"` if unavailable.
- All I/O errors caught via `try/with Unix.Unix_error` → `Error (Io msg)`.
- Wrap synchronous Unix calls in `Lwt_unix` equivalents (or `Lwt.return` wrapping `try/with`) — must remain Lwt-typed for interface compatibility.

- [ ] **Step 1:** Write `lib/block/unix_file.mli` (signature above).
- [ ] **Step 2:** Write `lib/block/unix_file.ml` (implementation per notes above).
- [ ] **Step 3:** Update `lib/block/dune` — add `unix lwt.unix` to libraries, add bisect_ppx instrumentation.
- [ ] **Step 4:** Write `test/test_unix_file.ml`:
  - Unit: `open_` creates file; `n_pages` = 0 initially.
  - Unit: `resize` then `write_page` then `read_page` round-trips exactly.
  - Unit: `read_page` out of bounds returns `Error Out_of_bounds`.
  - Unit: `sync` succeeds.
  - Unit: second `open_` on same path returns `Error (Io _)` (flock conflict).
  - QCheck: random sequence of `resize`→`write_page`→`read_page` round-trips; data matches for all valid page IDs.
  - QCheck: page contents are independent (write to page N does not corrupt page M).
  - Cleanup: delete test files after each test (`Sys.remove`).
- [ ] **Step 5:** Add `test_unix_file` to `test/dune`.
- [ ] **Step 6:** `dune runtest` — all tests pass (including existing 351).
- [ ] **Step 7:** Commit: `feat(block): add Unix_file block backend with flock + resize`.

---

### Task 2: Page format (`lib/storage/page.ml`)

**Context:** All B+-tree on-disk state lives in 4096-byte pages. This module defines the page layout as Cstruct accessors and the types for each page kind. No I/O happens here — this is pure codec.

**Files to create:**
- Create: `lib/storage/` directory
- Create: `lib/storage/dune`
- Create: `lib/storage/page.mli`
- Create: `lib/storage/page.ml`

**Page kinds:**

```
kind byte: 0=Header, 1=Branch, 2=Leaf, 3=Freelist
```

**Common page header (bytes 0–15):**
```
+0  [1] kind
+1  [1] flags (reserved, set to 0)
+2  [2] n_keys (uint16, number of keys in this page)
+4  [4] right_page (uint32, branch pages: rightmost child; leaf: next-leaf page-id for range scans; 0 = none)
+8  [4] crc32 (uint32, CRC32 of bytes 0..4095 with bytes 8..11 zeroed during computation)
+12 [4] reserved (set to 0)
```
Total header: 16 bytes. Data area: bytes 16..4095 (4080 bytes).

**Header page layout (kind=0, pages 0 and 1 only):**
```
+16  [8] txn_id (uint64 big-endian; higher = more recent)
+24  [8] root_page (uint64; page-id of the B+-tree root — tree_id 0 is master catalog)
+32  [8] freelist_page (uint64; page-id of first freelist page, 0 if none)
+40  [8] n_pages_total (uint64; total pages in file including headers)
+48  [8] schema_version (uint64; bumped on each DDL)
+56  [4] page_size (uint32; must be 4096 for v1)
+60  [4] format_version (uint32; 1 for v1)
+64  [4080-48] reserved zeros
```

**Branch page layout (kind=1):**
```
Data area contains a packed array of (key_len uint16, key bytes, child_page uint32) entries.
Entries are sorted by key ascending.
The `right_page` common header field holds the rightmost child (for keys > last key).
n_keys: number of (key, left_child) pairs. Children = n_keys+1 total.
```

**Leaf page layout (kind=2):**
```
Data area contains packed (key_len uint16, key bytes, val_len uint16, val bytes) entries.
Entries sorted by key ascending.
right_page: page-id of next leaf (0 = end of chain) — enables forward range scans.
```

**Freelist page layout (kind=3):**
```
Data area: array of (page_id uint32, freed_at_txn_id uint64) entries.
right_page: next freelist page (0 = end).
n_keys: number of entries on this page.
```

**`page.mli` API:**
```ocaml
type kind = Header | Branch | Leaf | Freelist

type common = {
  kind    : kind;
  flags   : int;
  n_keys  : int;
  right_page : int32;
  crc32   : int32;
}

(** Read/write common header fields *)
val read_common  : Cstruct.t -> common
val write_common : Cstruct.t -> common -> unit

(** CRC32 of page contents (bytes 0..4095 with bytes 8..11 zeroed during computation) *)
val compute_crc : Cstruct.t -> int32
val verify_crc  : Cstruct.t -> bool

(** Header page fields (page must have kind=Header) *)
type header_fields = {
  txn_id         : int64;
  root_page      : int64;
  freelist_page  : int64;
  n_pages_total  : int64;
  schema_version : int64;
  page_size      : int32;
  format_version : int32;
}
val read_header_fields  : Cstruct.t -> header_fields
val write_header_fields : Cstruct.t -> header_fields -> unit

(** Entry accessors for Branch pages (data area offset iteration) *)
val branch_entry_at : Cstruct.t -> offset:int ->
  [ `Entry of { key: bytes; left_child: int32; next_offset: int } | `End ]

val branch_append_entry : Cstruct.t -> offset:int ->
  key:bytes -> left_child:int32 -> int  (* returns new offset *)

(** Entry accessors for Leaf pages *)
val leaf_entry_at : Cstruct.t -> offset:int ->
  [ `Entry of { key: bytes; value: bytes; next_offset: int } | `End ]

val leaf_append_entry : Cstruct.t -> offset:int ->
  key:bytes -> value:bytes -> int  (* returns new offset *)

(** Entry accessors for Freelist pages *)
val freelist_entry_at : Cstruct.t -> index:int ->
  { page_id: int32; freed_at_txn_id: int64 }

val freelist_set_entry : Cstruct.t -> index:int ->
  page_id:int32 -> freed_at_txn_id:int64 -> unit

val max_freelist_entries_per_page : int
val max_data_bytes : int  (* = 4080 *)
```

**Implementation notes:**
- Use `Cstruct.LE`/`BE` accessors. Big-endian for txn_id and page IDs (preserves sort order in debugging; not used as B+-tree keys directly).
- CRC32: implement a pure-OCaml CRC32 table lookup (IEEE polynomial). No external deps. Initialize the 256-entry lookup table once at module load.
- `branch_entry_at`/`leaf_entry_at`: linear scan from `offset`. Return `\`End` when remaining bytes < minimum entry size.
- All writes zero-initialize unused bytes in the data area on page creation.

- [ ] **Step 1:** Create `lib/storage/dune`:
  ```
  (library
   (name sqlocaml_storage)
   (public_name sqlocaml.storage)
   (libraries cstruct lwt sqlocaml.block)
   (preprocess (pps lwt_ppx))
   (instrumentation (backend bisect_ppx)))
  ```
- [ ] **Step 2:** Write `lib/storage/page.mli`.
- [ ] **Step 3:** Write `lib/storage/page.ml` — CRC32 table, common header R/W, header fields R/W, branch/leaf/freelist entry accessors.
- [ ] **Step 4:** Write `test/test_page.ml`:
  - Unit: `write_common` then `read_common` round-trips for all kinds.
  - Unit: `compute_crc` is deterministic; `verify_crc` passes on fresh page, fails on corrupted byte.
  - Unit: `write_header_fields` then `read_header_fields` round-trips.
  - Unit: `branch_append_entry` then `branch_entry_at` returns correct key and child.
  - Unit: `leaf_append_entry` then `leaf_entry_at` returns correct key and value.
  - Unit: `leaf_entry_at` returns `\`End` past the last entry.
  - Unit: freelist entry set/get round-trips.
  - QCheck: random key/value byte sequences round-trip through leaf append/read.
  - QCheck: CRC32 detects any single-byte corruption (flip random byte, check `verify_crc` returns false).
- [ ] **Step 5:** Add `test_page` to `test/dune`.
- [ ] **Step 6:** `dune runtest` — all pass.
- [ ] **Step 7:** Commit: `feat(storage): page format codec + CRC32`.

---

### Task 3: Freelist (`lib/storage/freelist.ml`)

**Context:** The freelist tracks which pages have been freed and at which transaction ID. A page can only be reused once no active reader holds a snapshot with `txn_id <= freed_at_txn_id`. In Phase 1 we don't implement full MVCC (that's Phase 3), so the freelist is simpler: track freed pages + freed_at_txn_id in memory during a transaction; persist to freelist pages on commit.

**Phase 1 simplification:** No active-readers table yet. A freed page becomes reusable on the *next* `rw_txn` (i.e., freed_at_txn_id < current txn_id). This is correct for single-reader usage and maintains the invariant for Phase 3 to extend.

**Files:**
- Create: `lib/storage/freelist.mli`
- Create: `lib/storage/freelist.ml`

**`freelist.mli` API:**
```ocaml
(** In-memory freelist state. Persisted to Freelist pages on commit. *)
type t

val empty : t

(** Add a page to the free set. freed_at_txn_id: the txn that freed it. *)
val add : t -> page_id:int32 -> freed_at_txn_id:int64 -> t

(** Pop a reusable page. A page is reusable if freed_at_txn_id < current_txn_id.
    Returns (page_id, updated_t) or None if no page is available. *)
val pop : t -> current_txn_id:int64 -> (int32 * t) option

(** All entries — for serialization to freelist pages. *)
val to_list : t -> (int32 * int64) list   (* (page_id, freed_at_txn_id) *)

(** Reconstruct from a list (deserialized from freelist pages). *)
val of_list : (int32 * int64) list -> t

val size : t -> int
```

**Implementation:** Use a simple list (or two queues: reusable vs. held). Keep it pure — no I/O.

- [ ] **Step 1:** Write `lib/storage/freelist.mli`.
- [ ] **Step 2:** Write `lib/storage/freelist.ml`.
- [ ] **Step 3:** Write `test/test_freelist.ml`:
  - Unit: `empty` has size 0; `pop` returns None.
  - Unit: `add` then `pop` with `current_txn_id > freed_at_txn_id` returns the page.
  - Unit: `pop` with `current_txn_id <= freed_at_txn_id` returns None (still held).
  - Unit: multiple adds; `pop` returns one at a time.
  - Unit: `to_list` / `of_list` round-trip preserves all entries.
  - QCheck: after adding N pages freed at txn_ids T1..TN, `pop` with various current_txn_id values returns exactly the reusable subset.
  - QCheck: `of_list (to_list t)` has same size and same pop behavior as `t`.
- [ ] **Step 4:** Add `test_freelist` to `test/dune`.
- [ ] **Step 5:** `dune runtest` — all pass.
- [ ] **Step 6:** Commit: `feat(storage): freelist with txn-id gating`.

---

### Task 4: Pager (`lib/storage/pager.ml`)

**Context:** The pager sits between the B+-tree and the BLOCK layer. It provides:
1. **Page cache** — bounded LRU cache of hot pages, avoiding redundant reads.
2. **Page allocator** — assigns new page IDs either by reusing freelist entries or extending the file.
3. **Dirty tracking** — modified pages are buffered and flushed on commit.

The pager is parameterized by a BLOCK instance (via first-class module).

**Files:**
- Create: `lib/storage/pager.mli`
- Create: `lib/storage/pager.ml`

**`pager.mli` API:**
```ocaml
type t
type error = Block_error of string | Corruption of string

val create : (module Sqlocaml_block.Block.S with type t = 'b) -> 'b ->
             n_pages:int64 -> freelist:Freelist.t -> t

(** Read a page. Returns cached copy if available; otherwise reads from BLOCK. *)
val read  : t -> int64 -> (Cstruct.t, error) result Lwt.t

(** Mark a page as dirty with new contents. Buffered until flush. *)
val write : t -> int64 -> Cstruct.t -> unit

(** Allocate a new page ID. Reuses freelist if possible; else extends. *)
val alloc : t -> current_txn_id:int64 -> (int64, error) result Lwt.t

(** Free a page (add to freelist with current txn_id). *)
val free  : t -> page_id:int64 -> freed_at_txn_id:int64 -> unit

(** Flush all dirty pages to BLOCK (write_page + sync). *)
val flush : t -> (unit, error) result Lwt.t

(** Current total page count (after any allocations). *)
val n_pages : t -> int64

(** Current freelist state (for serialization into header). *)
val freelist : t -> Freelist.t
```

**Implementation notes:**
- LRU cache: use a `Hashtbl` (page_id → Cstruct.t) with a bounded size (default: 64 pages). Eviction: simple FIFO is acceptable for Phase 1 (true LRU is a Phase 2+ refinement).
- Dirty set: `Hashtbl` (page_id → Cstruct.t) of pages that need writing.
- `alloc`: call `Freelist.pop` with `current_txn_id` first; if None, increment `n_pages` and call `BLOCK.resize` lazily on flush.
- `flush`: write all dirty pages via `BLOCK.write_page`, then `BLOCK.sync`, then clear dirty set.
- Cache size limit: 64 pages (256 KB — trivial for Phase 1; tune in Phase 2).

- [ ] **Step 1:** Write `lib/storage/pager.mli`.
- [ ] **Step 2:** Write `lib/storage/pager.ml` using a first-class module for BLOCK.
- [ ] **Step 3:** Write `test/test_pager.ml` (use `Mem` backend):
  - Unit: `read` after `write` returns the written content.
  - Unit: `alloc` returns 0 initially; subsequent allocs increment.
  - Unit: `free` then `alloc` (with higher txn_id) returns the freed page.
  - Unit: `flush` causes `read` on fresh pager (same BLOCK) to return the written data.
  - Unit: dirty pages are not visible to a second independent pager on the same block until after `flush`.
  - QCheck: random write/read sequence — last written value always readable.
  - QCheck: alloc/free/alloc cycle — page IDs are valid and non-overlapping.
- [ ] **Step 4:** Add `test_pager` to `test/dune`.
- [ ] **Step 5:** `dune runtest` — all pass.
- [ ] **Step 6:** Commit: `feat(storage): pager with LRU cache, dirty tracking, freelist allocator`.

---

### Task 5: Header management (`lib/storage/header.ml`)

**Context:** The alternating header commit protocol. Pages 0 and 1 are the two header pages. On each commit, the "inactive" header is overwritten with the new state, then fsynced. The header with the higher `txn_id` (and valid CRC) is "live".

**Files:**
- Create: `lib/storage/header.mli`
- Create: `lib/storage/header.ml`

**`header.mli` API:**
```ocaml
type t = {
  txn_id         : int64;
  root_page      : int64;
  freelist_page  : int64;
  n_pages_total  : int64;
  schema_version : int64;
}

type error = Io of string | Both_headers_corrupt

(** Read both headers from the pager. Pick the live one (higher valid txn_id).
    Returns Error Both_headers_corrupt if neither has a valid CRC. *)
val read_live : Pager.t -> (t, error) result Lwt.t

(** Commit a new header state. Writes to the INACTIVE header page (0 or 1),
    sets CRC, syncs. The live header alternates each commit. *)
val commit : Pager.t -> prev_header:t -> t -> (unit, error) result Lwt.t

(** Initialize a brand-new file: write both headers as txn_id=0 with zeroed root.
    Called once when creating a new database. *)
val init : Pager.t -> (unit, error) result Lwt.t
```

**Implementation notes:**
- `read_live`: read pages 0 and 1 via `Pager.read`, call `Page.verify_crc` on each, pick valid one with higher `txn_id`. If one is corrupt and the other valid, use the valid one.
- `commit`: determine inactive header page (0 if current live is 1, else 1). Write `Page.write_header_fields` with `crc32 = Page.compute_crc buf`. Call `Pager.flush` (which calls `BLOCK.sync`).
- After `commit`, the new header page has `txn_id = prev.txn_id + 1`.

- [ ] **Step 1:** Write `lib/storage/header.mli`.
- [ ] **Step 2:** Write `lib/storage/header.ml`.
- [ ] **Step 3:** Write `test/test_header.ml` (use `Mem` backend + pager):
  - Unit: `init` succeeds on empty pager; `read_live` returns txn_id=0.
  - Unit: `commit` increments txn_id; subsequent `read_live` returns updated state.
  - Unit: simulate corrupt page 0 (flip CRC byte) — `read_live` still succeeds using page 1.
  - Unit: simulate corrupt page 1 — `read_live` still succeeds using page 0.
  - Unit: both headers corrupt → `Error Both_headers_corrupt`.
  - Unit: after N commits, txn_id = N; alternation is correct (page 0 and 1 take turns).
  - QCheck: random sequence of commits; `read_live` always returns the latest committed state.
- [ ] **Step 4:** Add `test_header` to `test/dune`.
- [ ] **Step 5:** `dune runtest` — all pass.
- [ ] **Step 6:** Commit: `feat(storage): alternating header commit protocol`.

---

### Task 6: CoW B+-tree (`lib/storage/btree.ml`)

**Context:** The core data structure. This is the most complex module in the project. A copy-on-write B+-tree where modifications never overwrite existing pages — they allocate new pages and update parent pointers up to the root. The tree operates through the `Pager` layer.

**Design decisions:**
- **Order**: variable key sizes. Maximum key size: 512 bytes (keys larger than this are an error). Maximum value size for leaf entries: 1024 bytes. (Overflow pages deferred.)
- **Split policy**: split when inserting would overflow the 4080-byte data area. On split, the median key is promoted to the parent.
- **Merge policy**: merge (or redistribute) when a node is less than 25% full after delete. Phase 1: lazy deletion is acceptable — only compact obviously under-full nodes on delete.
- **Cursor**: forward-only. `cursor_first` positions at the leftmost leaf. `cursor_seek` does a tree search. `cursor_next` advances within leaf, then follows `right_page` pointers across leaves.
- **CoW**: any modification to a page allocates a new page via `Pager.alloc`, writes the new version, frees the old page via `Pager.free`, and propagates the new page-id to the parent. The root page-id changes on every mutation.

**Files:**
- Create: `lib/storage/btree.mli`
- Create: `lib/storage/btree.ml`

**`btree.mli` API:**
```ocaml
type t

type error =
  | Pager_error of Pager.error
  | Key_too_large of int          (* key size in bytes *)
  | Value_too_large of int        (* value size in bytes *)
  | Tree_corrupt of string

type cursor

(** Create a tree rooted at [root_page]. Use 0 for an empty tree (no root yet). *)
val create : Pager.t -> root_page:int64 -> t

(** Current root page id (changes after each mutation). *)
val root_page : t -> int64

(** Point lookup. *)
val get : t -> bytes -> (bytes option, error) result Lwt.t

(** Insert or replace a key-value pair. Returns the new root page id. *)
val put : t -> bytes -> bytes -> (int64, error) result Lwt.t

(** Delete a key. No-op if key does not exist. Returns the new root page id. *)
val del : t -> bytes -> (int64, error) result Lwt.t

(** Open a forward cursor over the tree. *)
val cursor_open  : t -> (cursor, error) result Lwt.t

(** Seek to the first entry >= key.
    Returns: `Found if exact match, `Not_found_after k if positioned after key. *)
val cursor_seek  : cursor -> bytes ->
  ([`Found | `Not_found_after of bytes], error) result Lwt.t

(** Move to next entry. Returns None at end of tree. *)
val cursor_next  : cursor -> ((bytes * bytes) option, error) result Lwt.t

(** Current position of cursor (key * value), or None if not positioned. *)
val cursor_current : cursor -> (bytes * bytes) option

val cursor_close : cursor -> unit
```

**Implementation notes:**

*Tree structure:*
- Empty tree: `root_page = 0`. First `put` creates a single leaf page.
- Leaf pages hold `(key, value)` pairs sorted by `Bytes.compare`.
- Branch pages hold `(key, left_child_page)` pairs + `right_page` (rightmost child).

*`get`:*
- Traverse from root to leaf, at each branch page binary-search (or linear-scan) the key array, follow the appropriate child pointer. At leaf: linear scan for exact match.

*`put`:*
- Traverse from root to leaf, remembering the path (list of branch page-ids + offsets for backtracking).
- At the leaf: if key exists, replace value. If not, insert in sorted position.
- If the leaf overflows (insert would exceed 4080 bytes): split. Allocate two new pages (or reuse one and allocate one). Promote median key to parent. Propagate upward, splitting parents as needed. If root splits, allocate new root.
- CoW on every modified page: `Pager.alloc` → build new page → `Pager.write` → `Pager.free` old page.
- Return updated `root_page`.

*`del`:*
- Find and remove the key from the leaf.
- Phase 1: lazy — if the node becomes empty, remove the entry from the parent branch. If this leaves the branch empty and it's not the root, propagate up. Don't rebalance non-empty nodes in Phase 1.
- CoW all modified pages.

*Cursor:*
- State: current leaf page-id + offset within leaf + `ready` flag (same semantics as Phase 0 BytesMap cursor).
- `cursor_open`: traverse to leftmost leaf (keep following left-child from root).
- `cursor_seek`: binary-search downward to the right leaf, position at first entry >= key.
- `cursor_next`: advance offset; when past end of leaf, follow `right_page` to next leaf.

- [ ] **Step 1:** Write `lib/storage/btree.mli`.
- [ ] **Step 2:** Write `lib/storage/btree.ml`. Implement in order: `get` → `put` (no split) → cursor → split logic → `del`.
- [ ] **Step 3:** Write `test/test_btree.ml` — this is the most important test file:
  - Unit: empty tree `get` returns None.
  - Unit: `put` then `get` returns value.
  - Unit: `put` replace: second `put` with same key returns new value on `get`.
  - Unit: `del` on missing key is no-op.
  - Unit: `put` then `del` then `get` returns None.
  - Unit: insert 100 keys in random order; all retrievable; `cursor_open`+`cursor_next` visits all in sorted order.
  - Unit: insert enough keys to trigger a leaf split (fill >4080 bytes); verify all keys still accessible.
  - Unit: insert enough keys to trigger a branch split (multi-level tree); verify all keys accessible.
  - Unit: `cursor_seek` to existing key returns `\`Found`; cursor_current returns correct pair.
  - Unit: `cursor_seek` to non-existing key positions after; `cursor_next` returns next key.
  - Unit: `cursor_seek` past all keys; `cursor_next` returns None.
  - Unit: `cursor_open` on empty tree; `cursor_next` returns None.
  - Unit: root page changes after each `put`.
  - Unit: after `put`+`flush`+reconstruct pager from same block, all data survives.
  - QCheck: `put` N random key/value pairs; all retrievable via `get`. (use `Bytes.of_string`)
  - QCheck: `put` then `del`; `get` returns None; tree size is consistent.
  - QCheck: cursor visits all keys in lexicographic order for any insertion sequence.
  - QCheck: split invariant — after any sequence of `put`s, no leaf or branch page exceeds 4080 data bytes.
  - QCheck: CoW invariant — each `put` returns a fresh root page-id different from previous (when tree is non-empty and key is new).
- [ ] **Step 4:** Add `test_btree` to `test/dune`.
- [ ] **Step 5:** `dune runtest` — all pass.
- [ ] **Step 6:** Commit: `feat(storage): CoW B+-tree — get/put/del + forward cursor`.

---

### Task 7: Wire up `lib/store/` to the B+-tree backend

**Context:** Phase 0's `store.ml` uses a `BytesMap` in-memory map. Phase 1 replaces the implementation while keeping `store.mli` identical. The `Store.t` now holds a `Pager.t` + one `Btree.t` per `tree_id` (tracked in a `Hashtbl`). The `rw_mutex` stays.

The `Store` in Phase 1 does not yet implement multi-tree root tracking across commits (all trees' roots must be stored in the header or a meta-tree). Use `tree_id=0` as a master meta-tree that maps `tree_id → root_page_id`. On commit, serialize the meta-tree root into the header.

**Files to modify:**
- `lib/store/store.ml` — full rewrite of implementation; mli is unchanged
- `lib/store/dune` — add `sqlocaml.storage` to libraries

**Implementation outline:**

```ocaml
(* Store.t *)
type t = {
  pager    : Pager.t;
  meta     : Btree.t;          (* tree_id=0: maps varint(tree_id) → varint(root_page) *)
  trees    : (int, Btree.t) Hashtbl.t;
  header   : Header.t ref;     (* last committed header *)
  rw_mutex : Lwt_mutex.t;
}
```

`Store.get/put/del`: look up the `Btree.t` for the given `tree_id` in `trees`; call the btree operation; update `trees` with new root if changed.

`Store.commit` (rw_txn):
1. For each dirty tree in `trees`, serialize `(tree_id, root_page)` into `meta` btree.
2. Write freelist pages using `Pager.alloc` + `Page.freelist_*`.
3. Call `Pager.flush` (writes all dirty data pages + sync).
4. Call `Header.commit` with new header (updated `root_page` = meta root, `n_pages_total`, `schema_version`).

`Store.open_`:
1. Open `Pager` on the given BLOCK.
2. `Header.read_live` — if both corrupt, error. If file is empty (n_pages=0), call `Header.init`.
3. Deserialize the meta-tree from `header.root_page`.
4. Deserialize the freelist from `header.freelist_page`.
5. Return `t`.

Cursor operations: delegate to `Btree.cursor_*` with the appropriate tree's `Btree.t`.

- [ ] **Step 1:** Update `lib/store/dune` — add `sqlocaml.storage` to libraries.
- [ ] **Step 2:** Rewrite `lib/store/store.ml` against the btree backend per the outline above.
- [ ] **Step 3:** Verify existing `test/test_store.ml` still passes against the `Mem` BLOCK (the test uses `Store.open_` — update if the signature changed).
- [ ] **Step 4:** Write `test/test_store_btree.ml` — same contract tests as `test_store.ml` but using the `Unix_file` backend:
  - All the same tests, but `Store.open_` uses `Unix_file` instead of `Mem`.
  - Add: data survives close + reopen (persistence).
  - Add: concurrent ro_txn sees pre-commit state while rw_txn is in progress.
- [ ] **Step 5:** Add `test_store_btree` to `test/dune`.
- [ ] **Step 6:** `dune runtest` — all 351+ tests pass.
- [ ] **Step 7:** Commit: `feat(store): rewire store to CoW B+-tree backend`.

---

### Task 8: Index key encoding (`lib/encoding/index_key.ml`)

**Context:** Index keys are tuples `(col1, col2, …, rowid)` that must sort correctly under `Bytes.compare`. Each type uses an order-preserving encoding per `docs/specs/2026-05-10-design.md` §6.4. This is a pure codec module — no I/O.

**Files:**
- Create: `lib/encoding/index_key.mli`
- Create: `lib/encoding/index_key.ml`
- Modify: `lib/encoding/dune` — no new deps needed (pure)

**`index_key.mli` API:**
```ocaml
(** Order-preserving encoding of SQL values for use as B+-tree index keys.
    Encoded bytes compare correctly under Bytes.compare for all supported types. *)

type value =
  | IK_int  of int64
  | IK_real of float
  | IK_text of string
  | IK_blob of bytes
  | IK_null   (* sorts before all non-null values *)

(** Encode a single value. Result bytes are order-preserving. *)
val encode_value : value -> bytes

(** Encode a key tuple (col values ++ rowid). Each component is length-prefixed
    to allow unambiguous multi-column keys. *)
val encode : value list -> rowid:int64 -> bytes

(** Decode a key tuple. Returns the column values and the rowid. *)
val decode : bytes -> (value list * int64, string) result
```

**Per-type order-preserving encodings:**

- **NULL**: tag byte `0x00`.
- **INTEGER (`IK_int`)**: tag byte `0x01` + 8-byte big-endian with sign bit flipped (`Rowid.encode` semantics): `Int64.logxor n 0x8000_0000_0000_0000L`, then write big-endian. Negatives sort before positives.
- **REAL (`IK_real`)**: tag byte `0x02` + 8 bytes. IEEE 754 big-endian with adjusted sign+exponent so float ordering matches bytewise ordering. For non-negative floats: just big-endian bits work. For negatives: flip all bits. Zero: all-zeros after tag. NaN: treated as NULL (tag `0x00`).
- **TEXT (`IK_text`)**: tag byte `0x03` + UTF-8 bytes + `0x01` terminator. Any `0x00` byte in the input is escaped as `0x00 0xFF`. The `0x01` terminator is chosen so that a prefix of one text value sorts before a longer text value.
- **BLOB (`IK_blob`)**: tag byte `0x04` + same escaping as TEXT + `0x01` terminator.

For multi-column keys: each component is encoded independently and concatenated. Since each encoding is self-delimiting (NULL is 1 byte; INT is 9 bytes; TEXT/BLOB have the `0x01` terminator), concatenation is unambiguous.

Rowid suffix: append `Rowid.encode rowid` (8 bytes, same sign-flip big-endian as before).

- [ ] **Step 1:** Write `lib/encoding/index_key.mli`.
- [ ] **Step 2:** Write `lib/encoding/index_key.ml`.
- [ ] **Step 3:** Write `test/test_index_key.ml`:
  - Unit: NULL < any non-null value (verify byte comparison).
  - Unit: INTEGER ordering: `encode_value (IK_int min_int) < encode_value (IK_int 0L) < encode_value (IK_int max_int)`.
  - Unit: REAL ordering: `-1.0 < -0.0 < 0.0 < 1.0`.
  - Unit: TEXT ordering: `"a" < "aa" < "b"`.
  - Unit: NULL sorts before INTEGER, INTEGER before REAL, REAL before TEXT, TEXT before BLOB.
  - Unit: multi-column key `[IK_int 1L; IK_text "a"]` < `[IK_int 1L; IK_text "b"]`.
  - Unit: `encode`/`decode` round-trip for single-column keys.
  - Unit: `encode`/`decode` round-trip for two-column keys.
  - Unit: text with embedded `0x00` bytes encodes and decodes correctly.
  - QCheck: random int64 pairs — ordering of `encode_value (IK_int a)` matches `Int64.compare a b`.
  - QCheck: random string pairs — ordering of `encode_value (IK_text a)` matches `String.compare a b`.
  - QCheck: `encode`/`decode` round-trip for random 1–3 column keys with random types and rowids.
- [ ] **Step 4:** Add `test_index_key` to `test/dune`.
- [ ] **Step 5:** `dune runtest` — all pass.
- [ ] **Step 6:** Commit: `feat(encoding): order-preserving index key codec`.

---

### Task 9: Extend types — REAL + BLOB

**Context:** Phase 0 supports INTEGER and TEXT only. Phase 1 adds REAL (float64) and BLOB (bytes) throughout the stack: AST, lexer, parser, sema, row codec, catalog.

**Files to modify:**
- `lib/sql/ast.ml` — add `Ty_real`, `Ty_blob` to `type ty`; `L_real of float`, `L_blob of bytes` to `type literal`
- `lib/sql/lexer.mll` — add `REAL`, `BLOB` keywords; float literal token (`[0-9]+ '.' [0-9]*`)
- `lib/sql/parser.mly` — add `REAL`, `BLOB` type tokens; float literal production
- `lib/sql/sema.ml` — extend type checking for new types
- `lib/encoding/row.ml` — add REAL (8-byte LE float64) and BLOB encoding/decoding
- `lib/catalog/catalog.ml` — add type tags: `3=Real`, `4=Blob`; `type_of_tag` handles them

**Changes to `row.ml`:**
- `type ty` adds `Ty_real` and `Ty_blob`.
- `type value` adds `V_real of float` and `V_blob of bytes`.
- `encode`: REAL → 8-byte little-endian float64 (`Bytes.set_int64_le (Int64.bits_of_float f)`). BLOB → varint length + raw bytes.
- `decode`: symmetric.

**Changes to `ast.ml`:**
```ocaml
type ty = Ty_int | Ty_text | Ty_real | Ty_blob
type literal = L_int of int64 | L_text of string | L_null | L_real of float | L_blob of bytes
```

- [ ] **Step 1:** Extend `lib/sql/ast.ml` with new ty/literal variants.
- [ ] **Step 2:** Extend `lib/sql/lexer.mll` — add `REAL`, `BLOB` keyword tokens; add `FLOAT_LIT` token for `[0-9]+.[0-9]*`.
- [ ] **Step 3:** Extend `lib/sql/parser.mly` — add productions for `REAL`/`BLOB` type names and `FLOAT_LIT` literal.
- [ ] **Step 4:** Extend `lib/sql/sema.ml` — `ty_equal`, `lit_ty`, type check branches for new types.
- [ ] **Step 5:** Extend `lib/encoding/row.ml` — REAL and BLOB encode/decode.
- [ ] **Step 6:** Extend `lib/catalog/catalog.ml` — `type_of_tag` handles tags 3 and 4; `tag_of_type` emits them.
- [ ] **Step 7:** Extend `test/test_row.ml` — round-trip tests for REAL and BLOB values.
- [ ] **Step 8:** Extend `test/test_lexer.ml`, `test/test_parser.ml` — new keyword and literal coverage.
- [ ] **Step 9:** Extend `test/test_e2e.ml` — `CREATE TABLE` with REAL and BLOB columns; `INSERT` and `SELECT` with REAL and BLOB values.
- [ ] **Step 10:** `dune runtest` — all pass.
- [ ] **Step 11:** Commit: `feat(types): add REAL and BLOB column types end-to-end`.

---

### Task 10: ORDER BY and LIMIT

**Context:** Add `ORDER BY col [ASC|DESC]` and `LIMIT n [OFFSET m]` to the SQL pipeline. Phase 1: single-column ORDER BY only; multi-column is Phase 2. Sort is done in-memory by materializing the result stream (acceptable for Phase 1; an external sort operator is Phase 2).

**Files to modify:**
- `lib/sql/ast.ml` — extend `S_select` with `order_by` and `limit` fields
- `lib/sql/lexer.mll` — add `ORDER`, `BY`, `ASC`, `DESC`, `LIMIT`, `OFFSET` keywords
- `lib/sql/parser.mly` — add `ORDER BY` and `LIMIT`/`OFFSET` productions
- `lib/sql/sema.ml` — validate ORDER BY column exists in schema; LIMIT must be non-negative integer literal
- `lib/sql/plan.ml` — add `Op_sort` and `Op_limit` plan ops
- `lib/sql/planner.ml` — emit `Op_sort` around `Op_seq_scan` when ORDER BY present; wrap in `Op_limit` when LIMIT present
- `lib/sql/exec.ml` — implement `Op_sort` (materialize + sort) and `Op_limit`

**AST changes:**
```ocaml
type order_dir = Asc | Desc
type order_key = { col: string; dir: order_dir }

type stmt = ...
  | S_select of {
      proj   : [`All | `Cols of string list];
      table  : string;
      where  : expr option;
      order  : order_key list;   (* empty = no ORDER BY *)
      limit  : int option;
      offset : int option;
    }
```

**Plan changes:**
```ocaml
type op = ...
  | Op_sort   of { col: int; dir: [`Asc | `Desc]; child: op }
  | Op_limit  of { limit: int; offset: int; child: op }
```

**Exec `Op_sort`:** Collect all rows from child stream into a list; sort by the indexed column; return as a stream. Use `List.sort` with a comparator that handles `V_int`, `V_real`, `V_text`, `V_blob`, `V_null` (NULLs sort last by default in Phase 1; document this).

**Exec `Op_limit`:** Wrap the child stream with `Lwt_stream.drop` (for offset) + `Lwt_stream.take` (for limit).

- [ ] **Step 1:** Extend `lib/sql/ast.ml`.
- [ ] **Step 2:** Extend lexer with `ORDER`, `BY`, `ASC`, `DESC`, `LIMIT`, `OFFSET`.
- [ ] **Step 3:** Extend parser with ORDER BY and LIMIT/OFFSET productions.
- [ ] **Step 4:** Extend sema — validate ORDER BY column name, LIMIT/OFFSET are non-negative literals.
- [ ] **Step 5:** Extend plan.ml and planner.ml.
- [ ] **Step 6:** Implement `Op_sort` and `Op_limit` in exec.ml.
- [ ] **Step 7:** Write tests in `test/test_planner.ml`, `test/test_exec.ml`, `test/test_e2e.ml`:
  - `SELECT * FROM t ORDER BY n ASC` returns rows in ascending order.
  - `SELECT * FROM t ORDER BY n DESC` returns rows in descending order.
  - `SELECT * FROM t LIMIT 3` returns first 3 rows.
  - `SELECT * FROM t LIMIT 3 OFFSET 2` returns rows 2–4.
  - `SELECT * FROM t ORDER BY n ASC LIMIT 2` — combine.
  - QCheck: random insert sequence; ORDER BY ASC produces sorted output.
- [ ] **Step 8:** `dune runtest` — all pass.
- [ ] **Step 9:** Commit: `feat(sql): ORDER BY and LIMIT/OFFSET`.

---

### Task 11: CREATE INDEX + IndexLookup / IndexRangeScan

**Context:** Single-column indexes on user tables. An index is its own B+-tree in the store (with a unique `tree_id`). The index key is `index_key.encode [col_value] ~rowid`. Index metadata lives in `_sys_indexes` (tree_id=2 per the spec).

**Files to modify:**
- `lib/sql/ast.ml` — add `S_create_index`
- `lib/sql/lexer.mll` — add `INDEX`, `ON`, `UNIQUE` keywords
- `lib/sql/parser.mly` — `CREATE [UNIQUE] INDEX name ON table (col)`
- `lib/sql/sema.ml` — validate table/column exist; detect duplicate index names
- `lib/sql/plan.ml` — add `Op_create_index`, `Op_index_lookup`, `Op_index_range_scan`
- `lib/sql/planner.ml` — rule: equality predicate on indexed column → `Op_index_lookup`
- `lib/sql/exec.ml` — `Op_create_index` (scan table, build index); `Op_index_lookup` (seek + fetch rowid + store.get)
- `lib/catalog/catalog.ml` — extend `open_` and `create_index` to read/write `_sys_indexes`

**`S_create_index` AST:**
```ocaml
| S_create_index of { name: string; table: string; column: string; unique: bool }
```

**Planner rule for index lookup:**
- If WHERE clause is `E_eq(E_col col, E_lit lit)` and `col` has an index → emit `Op_index_lookup { tree_id; col; value; table_tree_id }` instead of `Op_seq_scan`.
- Otherwise: `Op_seq_scan` as before.

**`Op_create_index` execution:**
1. `Catalog.create_index` — allocate new `tree_id`, write to `_sys_indexes`.
2. Sequential scan over table tree; for each row: encode index key; `Store.put` into index tree.
3. Commit.

**`Op_index_lookup` execution:**
1. `Store.cursor_seek` on index tree with `IndexKey.encode [lookup_value] ~rowid:Int64.min_int` (prefix seek — finds first entry for this value).
2. Advance cursor while key prefix matches; collect rowids.
3. For each rowid: `Store.get` on table tree; decode row.
4. Emit rows as stream.

- [ ] **Step 1:** Extend ast/lexer/parser for `CREATE INDEX`.
- [ ] **Step 2:** Extend sema — validate table+column, check no duplicate index name.
- [ ] **Step 3:** Extend catalog — `_sys_indexes` read/write; `Catalog.create_index`; `Catalog.indexes_for_table`.
- [ ] **Step 4:** Extend plan.ml + planner.ml — `Op_create_index`, `Op_index_lookup`, planner rule.
- [ ] **Step 5:** Implement `Op_create_index` and `Op_index_lookup` in exec.ml.
- [ ] **Step 6:** Write tests:
  - Unit `test_catalog.ml`: `create_index` persists; `indexes_for_table` returns it after reopen.
  - Unit `test_e2e.ml`: `CREATE INDEX idx ON t(col)` then `SELECT WHERE col = val` uses index (verify via planner output).
  - Unit `test_e2e.ml`: index lookup returns same rows as seq scan.
  - Unit `test_e2e.ml`: `CREATE UNIQUE INDEX` (Phase 1: enforce uniqueness on INSERT into the indexed column — return error on duplicate).
  - QCheck `test_e2e.ml`: random inserts + index; for any lookup value, index result matches seq scan result.
- [ ] **Step 7:** `dune runtest` — all pass.
- [ ] **Step 8:** Commit: `feat(sql): CREATE INDEX + index lookup planner rule`.

---

### Task 12: Multi-backend conformance test runner

**Context:** The design spec requires the full SQL test suite to pass on `Mem` + `Unix_file` backends and produce identical results. Write a conformance runner that runs a parameterized SQL test suite on both backends.

**Files to create:**
- `test/test_conformance.ml`

**Structure:**
```ocaml
(* A single test case: SQL + expected rows *)
type sql_test = {
  name     : string;
  setup    : string list;   (* SQL stmts to run first *)
  query    : string;        (* the SELECT to run *)
  expected : Db.row list;   (* expected result rows *)
}

(* Run all tests on a given Db.t *)
let run_suite db tests = ...

(* Test entry point: runs suite twice — once on Mem, once on Unix_file *)
let () =
  let tests = [...] in  (* comprehensive SQL test cases *)
  Alcotest.run "conformance" [
    "mem",       run_suite (open_mem ()) tests;
    "unix_file", run_suite (open_unix_file "/tmp/sqlocaml_test.db") tests;
  ]
```

**Test cases to include (minimum):**
- CREATE TABLE + INSERT + SELECT * — basic round-trip on both backends.
- SELECT WHERE col = lit — equality filter.
- SELECT ORDER BY col ASC / DESC.
- SELECT LIMIT N / LIMIT N OFFSET M.
- SELECT after close + reopen (persistence — Unix_file only).
- INSERT REAL and BLOB values; SELECT retrieves them correctly.
- CREATE INDEX; SELECT with indexed column in WHERE.
- NULL handling: INSERT NULL; SELECT WHERE col = NULL returns no rows; SELECT WHERE col IS NULL not yet supported (document).
- Error cases: unknown table, unknown column, type mismatch.

- [ ] **Step 1:** Write `test/test_conformance.ml` with the parameterized runner and all test cases above.
- [ ] **Step 2:** Add `test_conformance` to `test/dune`.
- [ ] **Step 3:** `dune runtest` — all pass on both backends.
- [ ] **Step 4:** Commit: `test: multi-backend conformance runner (Mem + Unix_file)`.

---

### Task 13: Coverage and cleanup

**Context:** Reach 100% behavioral coverage on all new handwritten code. Run coverage script, identify gaps, write targeted tests.

- [ ] **Step 1:** Run `./scripts/coverage.sh html` — review `_coverage/index.html`.
- [ ] **Step 2:** For each file with < 100% coverage: identify uncovered lines. Write targeted tests or, for `let%lwt` desugaring artifacts, add `[@coverage off]` annotation.
- [ ] **Step 3:** Update `ROADMAP.md` — check off Phase 1 items.
- [ ] **Step 4:** Close Forgejo issue for Phase 1 (update with progress).
- [ ] **Step 5:** `dune runtest` — all pass.
- [ ] **Step 6:** Final commit: `chore: Phase 1 complete — coverage, ROADMAP update`.

---

## Invariants to maintain throughout Phase 1

1. **`store.mli` is unchanged.** The B+-tree backend is an implementation detail.
2. **All Phase 0 tests continue passing** after every task. Never break existing tests.
3. **TDD:** write tests before or alongside implementation, not after.
4. **QCheck on every module.** Minimum 10k trials per property.
5. **No `bisect_ppx` regressions.** Coverage must not drop below 95% handwritten after each task.
6. **Podman only.** All `dune build` and `dune runtest` in container.
7. **Commit after each task.** One commit per task minimum.
8. **CoW invariant:** after every `Btree.put` or `Btree.del`, the old root page must be in the freelist and the new root page must differ from the old one (unless the tree was empty).

---

## Open questions to resolve during Phase 1

- **NULL ordering in ORDER BY**: spec says "document the decision." Phase 1 default: NULLs last. Make this explicit in sema output and tested.
- **UNIQUE index enforcement**: on INSERT, check if the index already has an entry for the value (ignore the rowid suffix for the uniqueness check). Return `Constraint (Unique, col_name)` error.
- **Key size limit**: 512 bytes for index keys. Document error behavior when exceeded.
- **Page cache size**: 64 pages is the Phase 1 default. Track if any test workload exceeds it (all tests are small; this is fine).
- **Lazy delete policy**: non-empty B+-tree nodes are not rebalanced on delete in Phase 1. Document this as a known space waste; fix in Phase 2.
