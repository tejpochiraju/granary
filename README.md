# sqlocaml

A pure-OCaml SQL engine — a concept port of SQLite targeting [MirageOS](https://mirage.io/)
unikernels. No C stubs, a fresh on-disk file format, single-writer / multi-reader MVCC with
snapshot isolation, and strict typing (not SQLite's manifest typing).

The engine covers a large slice of the SQL surface: CRUD, JOINs, aggregates, subqueries and
correlated subqueries, CTEs and recursive CTEs, window functions, views, triggers (BEFORE /
AFTER / INSTEAD OF), foreign keys with CASCADE / SET NULL / SET DEFAULT and DEFERRABLE checks,
UPSERT, FTS5 with BM25 and `snippet()`, generated columns, partial / expression indexes,
SAVEPOINTs, a WAL with crash recovery, VACUUM, overflow pages, and WITHOUT ROWID tables.

> **Status:** pre-release (`0.0.1`). APIs and the on-disk format are not yet stable.

## Encryption at rest

Opt-in, page-level **AES-256-GCM** encryption (#84). Pass a 32-byte raw key to
`open_file` / `open_file_wal` (or the lower-level `Store.open_block*`) and the
database is created — or reopened — encrypted; omit the key and the database is
plaintext, exactly as before (encryption is **off by default**).

```ocaml
let key = (* 32 raw bytes from your secrets manager / boot config *) in
Sqlocaml_unix.Store.open_file ~key ~path:"app.db" ()
```

- **Fixed at creation.** Whether a database is encrypted is a per-file property
  chosen when it is first created and cannot be toggled later. Reopening an
  encrypted database without the key fails cleanly (`Encryption_key_required`);
  a wrong key is rejected by a header canary (`Encryption_key_mismatch`); a key
  supplied for a plaintext database is refused (`Not_encrypted`).
- **What is protected:** every data page of the main DB and every page payload
  in the WAL, both on disk and (for the WAL) on the replication wire. Each page
  carries its own random nonce + GCM authentication tag (a 32-byte reserved
  tail), so tampering is detected cryptographically.
- **What leaks (accepted):** the plaintext header pages expose structural
  metadata (page geometry, root/txn ids, schema version); WAL frame headers
  expose page-ids and the commit pattern. Page *contents* are never exposed.
- **App owns the key and entropy.** The library takes raw key material, not a
  passphrase (no in-engine KDF, no salt in the file). The application must seed
  `mirage-crypto-rng` at boot (the Unix backend uses
  `Mirage_crypto_rng_unix.use_default ()`); the core library never seeds, to
  stay Mirage-clean.

## Durability modes (`PRAGMA synchronous`)

sqlocaml supports a per-deployment durability setting analogous to SQLite's `synchronous`, gating
only the WAL group-commit fsync. CoW shadow-paging, snapshot isolation, rollback, and crash
recovery are unaffected — only *when* commits are fsynced changes.

| Mode | Commit-time fsync | App-process crash | OS / power crash |
|------|-------------------|-------------------|------------------|
| `full` (default) | fsync on every group-commit before the commit is acked | no loss | no loss |
| `batched` | deferred: fsync once `wal_batch_commits` commits accumulate **or** `wal_batch_interval_ms` ms elapse since the last sync (whichever first) | no loss | up to the last synced commit frame (prefix only) |
| `off` | never on commit | no loss | back to the last checkpoint |

**Configuration** (database-wide):

- `PRAGMA synchronous = full | batched | off`
- `PRAGMA wal_batch_commits = N` (default 256) — the batched commit-count threshold
- `PRAGMA wal_batch_interval_ms = T` (default 100) — the batched time threshold (milliseconds)
- Open-option: `Db.open_block ?durability:(Sqlocaml_store.Store.Batched { commits; interval_ms })` (also `Full` / `Off`)
- The getters (`PRAGMA synchronous`, `PRAGMA wal_batch_commits`, `PRAGMA wal_batch_interval_ms`) read the current values back.

> **Caveat — `batched` and `off` trade safety for speed.**
> - **App-process crash is always safe** in every mode: unsynced WAL frames live in the OS page
>   cache, which survives process death, and recovery replays them.
> - **`batched`/`off` give no write-ordering guarantee under an OS or power crash** (the same
>   caveat LMDB documents for `NOSYNC`). Recovery still converges to a *prefix* of acked commits —
>   never torn or interleaved state, because WAL recovery trusts only checksum-valid,
>   commit-marked frames — but acked commits in the loss window can be gone.
> - `batched` bounds the loss window by `wal_batch_commits` (N) or `wal_batch_interval_ms` (T).
>   `off` is for ephemeral / rebuildable data (bulk load, caches).
> - Checkpoint and database `close` are always full-sync anchors, so `batched`/`off` data is made
>   durable there.
> - The time bound (T) is **opportunistic**: it is checked when a commit arrives or at checkpoint,
>   and requires a clock supplied via the `?clock` open-option. A fully idle database is not
>   flushed until the next commit / checkpoint / close.
> - **Replication:** a registered replication commit-sink requires `synchronous=full`. `batched`/`off`
>   are rejected while replication is active (the checkpoint replica gate assumes every committed frame
>   is shipped, which only holds under `full`).

> **Scope:** unlike SQLite, where `synchronous` is per-connection, this setting is
> **database-wide** — the WAL commit queue is shared across all connections to a store, so a
> `PRAGMA synchronous` on any connection changes the mode for all of them (last writer wins).

## Benchmarks

In-process benchmarks against reference C **SQLite 3.45.1** (same dataset, prepared statements
both sides, WAL, fsync-per-commit, matched page cache), separating the CPU term from the I/O term
via `cpu/wall` per run. Full method and tables:
[docs/benchmarks/2026-06-07-bench-222-results.md](docs/benchmarks/2026-06-07-bench-222-results.md)
(also on the [wiki](https://git.iotready.com/tej/sqlite_ocaml_port/wiki/Benchmarks)).

**Current NVMe baseline** (`1a73da0`, after #228 PK B-tree seek, #229 O(n) bulk insert, and the
T4/T5 read-path work) — sqlocaml is now within single-digit multiples of C SQLite on most
workloads:

| workload (NVMe, plaintext) | sqlocaml vs SQLite | bound |
|----------------------------|--------------------|-------|
| point lookup `WHERE pk=?`  | ~3.7× slower | CPU |
| range scan / aggregate     | ~8.3× slower | CPU |
| insert (autocommit)        | ~2.0× slower | fsync |
| commit throughput          | ~2.9× slower | fsync |
| insert (batch, 1 txn)      | ~44× slower | mixed |

That is a large improvement over the [2026-06-02 pre-fix baseline](docs/benchmarks/2026-06-02-bench-222-results.md)
(`358b2b9`), where the same NVMe workloads were ~7,700× (point lookup, an O(n) full scan), ~200×
(scan), and ~5,300× (batch insert, an O(n²) path) slower. The point-lookup and bulk-insert
*complexity* bugs are gone; the only large remaining gap is **batch insert (~44×)**, a
constant-factor copy-on-write write-amplification cost (#230 / #231).

AES-256-GCM encryption-at-rest now adds only **~20% (or within noise)** to cache-resident reads —
the frame-cache (T4) caches decrypted pages, down from the ~2× (≈ +100%) of the pre-fix run.
Writes are barely affected.

**Verdict:** reads are **CPU-bound** (`cpu/wall ≈ 1.0`), single-row writes are **fsync/I-O-bound**
(`cpu/wall` 0.5–0.6). With the O(n) read path and O(n²) insert path fixed, **4 of 5 plaintext
workloads are within the #231 "10× of SQLite" goal**; the read-side multicore epic (#156) remains
gated on closing the batch-insert constant factor first.

> **Honest, early numbers.** sqlocaml is a young pure-OCaml engine; these are a snapshot on a
> production NVMe host under live load, at a small (cache-resident) dataset, and are expected to
> keep moving as the write path is optimized.

Reproduce: `scripts/bench222.sh` (builds the bench image and runs the suite; cross-host steps in
the results doc).

## AI authorship

**This codebase is entirely AI-written.** Per the
[avsm/ocaml-ai-disclosure](https://github.com/avsm/ocaml-ai-disclosure) proposal — which aligns
its vocabulary with the [W3C AI Content Disclosure](https://github.com/w3c-cg/ai-content-disclosure/)
levels (`none` / `ai-assisted` / `ai-generated` / `autonomous`) — sqlocaml's disclosure level is:

```
ai-generated
```

> *AI-generated with human prompting and/or review.*

**Authorship model.** A human (the repository owner) sets the scope, picks which issues to work
on, decides architectural trade-offs, and signs off on the result. An AI agent writes all of the
code, tests, documentation, and commit messages. The primary model is **Claude Opus**
(Anthropic), with **Claude Sonnet** occasionally used for cheaper mechanical work.

A handful of commits — multi-phase autonomous-loop work — drift toward `autonomous`, but
`autonomous` would overstate how hands-off the human is at the design and scoping layer, so
`ai-generated` is the honest level for the project as a whole.

The same disclosure is published in the package's opam metadata:

```
x-ai-disclosure: "ai-generated"
x-ai-model:      "claude-opus-4-7"
x-ai-provider:   "Anthropic"
```
