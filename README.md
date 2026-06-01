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
