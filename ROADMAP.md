# OCaml SQL Engine — Project Roadmap

A pure-OCaml, MirageOS-friendly SQL engine. This roadmap captures every
decision made during brainstorming and every item explicitly deferred or
omitted, as a checklist to revisit.

**Started:** 2026-05-10
**v1 target scope:** tables, indexes, CRUD + JOINs + transactions on a
copy-on-write B+-tree with single-writer/multi-reader MVCC. FTS deferred.

**Design spec:** [`docs/specs/2026-05-10-design.md`](docs/specs/2026-05-10-design.md)
— full architecture, module structure, public APIs, data flow, error model,
testing strategy, phasing.

---

## Decisions made (v1 baseline)

These are the choices that frame v1. Revisit each as the project evolves;
some may be reconsidered in v2+.

- [x] **Pure OCaml, no C stubs.** MirageOS deployment requires it.
- [x] **Fresh file format** — not SQLite-compatible. Maximum design freedom;
  loses interop with `sqlite3` CLI.
- [x] **Concept port, not source port.** SQLite C source treated as a
  reference for SQL semantics, not as something to translate.
- [x] **Approach: reference-driven hybrid + walking skeleton** — borrow
  proven designs (LMDB storage, Volcano execution, Menhir parser); start
  with a thin end-to-end vertical slice on the in-memory backend.
- [x] **Storage: LMDB-shaped CoW B+-tree.** Single file, two alternating
  header pages, MVCC via free-list and txn IDs. No journal, no WAL.
- [x] **Concurrency: single-writer / multi-reader with snapshot isolation.**
  Enforced by a single Lwt mutex on `rw_begin`.
- [x] **Three storage backends from day 1:** `Mem`, `Unix_file`, `Mirage_block`.
- [x] **Async runtime: Lwt for v1.** Aligns with `Mirage_block.S`. Storage
  internals are algorithm-pure (operate on in-memory `Cstruct.t` page
  buffers); async only at the page-fetch boundary.
- [x] **Pluggable backend via `BLOCK` module type** functor.
- [x] **Test strategy: mine SQLite `.test` corpus** into native OCaml tests
  (Alcotest + QCheck). No TCL bridge.
- [x] **WAL deferred** to v3+, but txn manager is the single boundary
  between SQL/storage and the page writer so WAL slots in cleanly.
- [x] **Schema lives in storage as system tables** (no separate metadata
  file). DDL is just an rw_txn that updates system trees.
- [x] **Page checksum (CRC32) mandatory** on header pages.
- [x] **Corruption is one-way:** detected corruption poisons the `Db.t`;
  user must close+reopen. No silent auto-repair.
- [x] **Streaming results via `row Lwt_stream.t`** — no materialized lists
  for SELECTs.
- [x] **Page size: 4096 fixed** at file creation. (Configurable later.)
- [x] **Two fsyncs per commit** (data-pages + header). Acceptable for v1.

---

## v1 SQL feature scope

### In v1
- [x] `CREATE TABLE` with INTEGER / REAL / TEXT / BLOB / NULL types
- [x] Column constraints: NOT NULL, PRIMARY KEY (single + composite),
      UNIQUE (single col), DEFAULT (literal values)
- [x] `DROP TABLE`, `DROP INDEX`
- [x] `CREATE INDEX` (single col)
- [x] `INSERT` with `WHERE`
- [x] `UPDATE`, `DELETE` with `WHERE`
- [x] `SELECT` with `WHERE`, `ORDER BY`, `LIMIT`, `OFFSET`
- [x] `GROUP BY`, `HAVING`
- [x] Aggregates: `COUNT`, `SUM`, `AVG`, `MIN`, `MAX`
- [x] Joins: `INNER JOIN`, `LEFT JOIN` (nested-loop + hash join physical
      operators)
- [ ] `BEGIN`, `COMMIT`, `ROLLBACK`
- [ ] Prepared statements with `?` parameters
- [ ] Scalar functions: `LENGTH`, `LOWER`, `UPPER`, `COALESCE`, `IFNULL`,
      `ABS`, basic arithmetic & comparison

### Deferred — SQL features (v2+)
- [ ] Subqueries (scalar, IN, EXISTS, derived tables)
- [ ] Common Table Expressions (`WITH` clause)
- [ ] Recursive CTEs
- [ ] Views
- [ ] Triggers (`BEFORE` / `AFTER` / `INSTEAD OF`)
- [ ] `ALTER TABLE ADD COLUMN`
- [ ] `ALTER TABLE DROP COLUMN`
- [ ] `ALTER TABLE RENAME` (consider for late v1)
- [ ] FOREIGN KEY enforcement (parse but don't enforce in v1)
- [ ] `CHECK` constraints
- [ ] Window functions (`OVER`, `PARTITION BY`)
- [ ] `DISTINCT` (consider for late v1)
- [ ] `UNION`, `INTERSECT`, `EXCEPT`
- [ ] Multi-column `UNIQUE` constraints (single-col only in v1)
- [ ] Partial indexes (`CREATE INDEX … WHERE …`)
- [ ] Expression indexes (`CREATE INDEX … (lower(name))`)
- [ ] Generated columns (`GENERATED ALWAYS AS …`)
- [ ] Default *expressions* vs literals
- [ ] Collation customization (`COLLATE NOCASE` etc.)
- [ ] `RETURNING` clause on mutations
- [ ] `UPSERT` / `ON CONFLICT`
- [ ] Savepoints (nested transactions)
- [ ] `ATTACH DATABASE` (multi-file)

### Deferred — Full-text search (v2)
- [ ] FTS index type (separate from regular indexes)
- [ ] Tokenizer (Unicode-aware, configurable)
- [ ] `MATCH` operator + query syntax (AND/OR/NOT, phrases, prefixes)
- [ ] BM25 ranking
- [ ] Snippets / highlights
- [ ] Decision: aim for SQLite FTS5 syntax compat or our own surface

---

## Storage / runtime

### v1
- [x] CoW B+-tree implementation (page format, splits, merges, rebalancing)
- [ ] Free-list with per-page freed-at-txn metadata
- [x] RW txn manager (single-writer mutex)
- [ ] RO txn manager (active-readers table; freelist gating)
- [x] Page cache (bounded LRU)
- [x] Two-header alternating commit protocol
- [x] Checksums on header pages
- [x] `BLOCK` signature + Mem/Unix_file/Mirage_block backends
- [x] Crash recovery on `Db.open_` (pick valid header by checksum + txn_id)

### Deferred — runtime / storage (v2+)
- [ ] WAL (write-ahead log) layer between txn manager and page writer
- [ ] Commit coalescing (group commit for multi-fiber writers)
- [ ] Cost-based query planner (v1 is rule-based)
- [ ] Statistics collection (table sizes, index selectivity)
- [ ] Eio support (alternative to Lwt) — revisit when Mirage canonical
- [ ] Encryption at rest
- [ ] Compression (page-level or column-level)
- [ ] Multi-process access (currently single-process by design)
- [ ] Network access (server mode) — non-goal for MirageOS embedded use
- [ ] Replication
- [ ] Online backup / hot copy
- [ ] Page-size configurability (currently 4096 fixed)
- [ ] Variable-size pages or large-page optimization

---

## Tooling

### v1
- [ ] OCaml library API (`Db.open_`, `execute`, `query`, prepared stmts)
- [ ] Test runner harness (Alcotest + QCheck)
- [ ] Crash-recovery test harness

### Deferred — tooling (v2+)
- [ ] Interactive REPL / shell (`ocaml-sql` analog of `sqlite3`)
- [ ] CLI for opening + running SQL files
- [ ] Schema dump / `.schema` equivalent
- [ ] `EXPLAIN` / query plan visualization
- [ ] `EXPLAIN ANALYZE` with timing
- [ ] Migration tooling
- [ ] Backup / restore CLI
- [ ] Database integrity check (`PRAGMA integrity_check` analog)
- [ ] Repair / recovery tooling for corrupted files
- [ ] Schema diff between two DBs
- [ ] Import / export CSV / JSON
- [ ] (Stretch) Read-only adapter that imports from `.sqlite` files

---

## Open architectural questions to revisit

- [ ] **Manifest typing vs strict typing.** SQLite famously has manifest
      typing (any value any column unless STRICT). v1 plans strict typing
      per column. Confirm before SQL surface ships.
- [ ] **Type affinity rules.** SQLite has type affinity for INTEGER PRIMARY
      KEY etc. Decide our rules and document them.
- [ ] **NULL ordering in `ORDER BY`.** NULLS FIRST or NULLS LAST default?
      Make explicit, match SQL standard or SQLite — pick one.
- [ ] **String collation default.** Byte-wise vs Unicode-aware (NOCASE).
- [ ] **Numeric precision.** REAL = float64? What about decimal types
      (deferred — not in v1 scope but document the decision).
- [ ] **Date/time types.** SQLite has none (uses TEXT/INTEGER/REAL).
      Ours? Decide before applications standardize on a convention.
- [ ] **Page size beyond 4096.** Leave room in header to bump.
- [ ] **File format version field.** Reserve space + a check on open.
- [ ] **Maximum row size.** Today: ~page size minus overhead. Overflow
      pages for large blobs are deferred — confirm before TEXT/BLOB
      lands on real workloads.
- [ ] **Overflow pages for large values.** Probably v1.5 — flag if
      needed sooner.
- [ ] **Cursor lifetime vs Lwt fibers.** Single fiber per cursor for now.
      Document; revisit if multi-fiber readers are wanted.
- [ ] **Public error API stability.** Variants are easy to add but hard
      to remove later. Audit before tagging v1.0.

---

## Testing — what we said we'd do

### v1
- [ ] **Unit tests (Alcotest) per module:** encoding round-trips, B+-tree
      invariants, planner output for canonical queries.
- [ ] **Property tests (QCheck):** B+-tree-stays-sorted under random
      mutations; snapshot isolation invariants; commit-then-crash recovery
      preserves data.
- [ ] **Mined SQLite SQL corpus tests:** parse `.test` files, run
      `sqlite3` to capture expected outputs, run our engine, diff.
      Auto-skip unimplemented features. Track conformance %.
- [ ] **Crash tests:** simulated crash mid-write at every byte position
      in the commit window; recovery preserves invariant.
- [ ] **Multi-backend conformance:** same SQL test suite runs on
      Mem + Unix_file + Mirage_block (under Mirage's sim where possible).
      Identical results required.

### Deferred — testing (v2+)
- [ ] Fuzzing the SQL parser (afl / honggfuzz)
- [ ] Differential testing against `sqlite3` (compare query results)
- [ ] Performance benchmarks (vs in-memory baselines, vs LMDB+SQLite)
- [ ] Long-running soak tests (random workloads for hours)
- [ ] Multi-fiber stress tests (many concurrent readers + 1 writer)
- [ ] Coverage measurement
- [ ] Power-loss / disk-fault injection (beyond simple crash sim)

---

## Phasing target (rough orders of magnitude)

These are solo-developer-feel estimates. Team size, depth, and rigor will
stretch them.

- [x] **Phase 0 — walking skeleton (~4-6 wks):** Mem backend; CREATE TABLE,
      INSERT, simple SELECT. End-to-end demo.
- [x] **Phase 1 — disk storage (~6-8 wks):** Unix_file backend (CoW B+-tree);
      ORDER BY, LIMIT, single-col indexes.
- [x] **Phase 2 — query depth (~8-10 wks):** UPDATE, DELETE, joins,
      GROUP BY, aggregates.
- [ ] **Phase 3 — transactions (~6 wks):** RO snapshots, RW txn,
      BEGIN/COMMIT/ROLLBACK, MVCC correctness.
- [ ] **Phase 4 — Mirage_block (~4 wks):** unikernel deployment.
- [ ] **Phase 5 (later) — FTS:** v2.

---

## Notes on intentionally NOT doing certain things

- **Not building** an `ATTACH`-style multi-database system. Single-file v1.
- **Not** porting SQLite's VDBE bytecode model. Volcano operators instead.
  VDBE has real benefits (compact plans, easy serialization, JIT-able
  later) but is heavy to implement and harder to reason about. Revisit
  if/when bytecode advantages matter.
- **Not** matching SQLite's quirks (manifest typing, type affinity,
  silent type coercion). v1 is strict-typed. We document deviations.
- **Not** trying for SQL standard conformance (SQL:1999/2003/2011). We
  follow SQLite's pragmatic subset shape where it's clear; document
  divergences.
- **Not** building a server / network protocol. Library only.
- **Not** building cross-process locking. Unikernel = single process.
