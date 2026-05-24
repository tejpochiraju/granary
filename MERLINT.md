# merlint linting (#153)

[merlint](https://github.com/samoht/merlint) is the project's static-analysis
linter. It catches patterns the test suite cannot — catch-all exception
handlers, silenced warnings, `Obj.magic`, deprecated modules, complexity and
documentation gaps. Because this codebase is entirely AI-authored (#152), a
second mechanical reviewer is high value.

## Running it

merlint is baked into the `sqlocaml-dev` container image (see `Containerfile`).
It is git-only with no opam release, so the image pins a known-good commit.

```sh
# inside the dev image (dune runs in podman, never on the host):
podman run --rm -v "$(pwd)":/workspace:Z -w /workspace sqlocaml-dev merlint --color=never
```

Configuration lives in `.merlint` at the repo root. Command-line `--rules`
overrides the file (e.g. `merlint -r E105` to look at one rule in isolation).

## CI posture: warn-first

The `lint` job in `.forgejo/workflows/ci.yml` runs merlint on every push/PR but
is marked `continue-on-error: true` — it **reports** findings without failing
the build. The intent is to ratchet the curated rule set down to zero via the
follow-ups below, then flip `continue-on-error` to `false` to enforce. After the
#153 and #173 work, the only remaining finding is E500 (missing `.ocamlformat`),
deferred to #171; once that lands the gate can flip to enforcing.

## Disabled rules and why

These are excluded in `.merlint`. They conflict with deliberate project
conventions, not defects:

| Rule | Name | Why disabled |
|------|------|--------------|
| E205 | Consider Using Fmt | We use `Printf`/`Format` throughout; not adopting `Fmt` project-wide. |
| E320 | Long Identifier Names | Long descriptive names (e.g. `fk_child_has_ref_multi_in_tx`) are intentional. |
| E410 | Bad Documentation Style | Our doc-comment shape differs from merlint's preferred odoc style. |
| E600 | Test Module Convention | Test suite is behavior-oriented, not a 1:1 module mirror. |
| E605 | Missing Test File | One test file exercises many library modules by design. |
| E606 | Test File in Wrong Directory | All tests live flat under `test/` intentionally. |
| E610 | Test Without Library | Tests don't map 1:1 to a library module of the same name. |
| E618 | Avoid X__Y Module Access | Tooling artifact (#153): every finding is the `lwt_ppx`-generated `__ppx_lwt_0` binding from `let%lwt`/`let*`, mapped back to the source line. Not hand-written module access; zero genuine `Module__Sub` uses exist. |
| E331 | Redundant Function Prefixes | Naming opinion (#153): wants `create_table`→`table`, `find_index`→`index`. Our `create_*`/`find_*`/`get_*` names mirror the SQL DDL they implement (same rationale as E320). |
| E325 | Function Naming Convention | Naming opinion (#153): wants `find_table`→`get_table`, directly contradicting E331 (`find_table`→`table`) on the same definitions. Our `find_*` (returns option) convention is deliberate. |
| E332 | Prefer 'v' Constructor | Naming opinion (#153): wants `Pager.create`→`Pager.v` etc. We use `create` consistently (same stance as the E205 `Fmt` opinion). |
| E330 | Redundant Module Name | Naming opinion (#153). The genuine cases were fixed in code (`Fts_query.fts_query`→`Fts_query.t`; `Store.store_of`→`txn_store`); what remains is intentional domain naming — `page_size` (part of the BLOCK device interface across all backends; mirrors SQLite's term; ~140 cross-module refs) and `Wal.wal_magic`. |

Excluding these takes the raw report from ~4077 to ~16 findings.

### Scoped exclusion: E105 in `test/` only (#167)

`E105` (catch-all exception handler) stays **enabled for `lib/` and `bin/`** but is
excluded for `test/*.ml*`. In tests, catch-all handlers are idiomatic best-effort
cleanup — temp-file teardown (`(try Unix.unlink p with _ -> ())`), `Fun.protect`
finalizers, env-var parse fallbacks — where a swallowed error merely skips cleanup.
The 12 production sites were instead **narrowed** to the precise exception they
intend to absorb (`Unix.Unix_error`, `Failure`, `Parser.Error`, `Invalid_argument`),
so unexpected errors now propagate. With production narrowed and `test/` excluded,
E105 reaches a documented zero and is ready to enforce. This drops the curated
report to ~378 findings.

### Scoped exclusion: E005 in `test/` only (#168)

`E005` (long functions) stays **enabled for `lib/` and `bin/`** but is excluded
for `test/*.ml*`. merlint auto-skips functions in *nested* test directories, but
our suite lives flat under `test/` (its dirname has no `/`), so long bench
harnesses (`open_slow_store`, `main`, `open_slow_wal`) are not auto-skipped;
long test/bench setup is acceptable. The `#168` refactor drove E005 to **zero
across all of `lib/`** (every flagged function decomposed into named helpers,
behavior-preserving, full suite green). Module by module: `exec.ml` (19 fns,
incl. the 1058-line `to_stream`), `sema.ml` (11, incl. the 669-line
`bind_select`), `planner.ml` (`plan` 407 + `plan_select` 174), `db.ml`
(`execute_instead_of`/`execute`/`execute_change_count` + `fire_trigger_stmt`),
`store.ml` (6 open/commit/freelist fns), `btree.ml` (`put`/`del`/`cursor_seek`),
`catalog.ml` (3 decode/rename fns), and the singletons
(`fault_inject`/`index_key`/`row`/`json`/`pager`). E005 is now enforceable for
`lib/` + `bin/`.

### Scoped exclusion: E110 in `test/` only (#173)

`E110` (silenced warning) stays **enabled for `lib/` and `bin/`** but is excluded
for `test/*.ml*`. In tests the suppressions are deliberate scaffolding:
`[@warning "-69"]` on mock-pager record fields that are written but never read
(`test_btree`), and `[@@warning "-32"]` on `*_lwt` helper variants retained
during the monadic-test migration (#161). The 5 `lib/` sites were resolved
directly: three were stale suppressions on bindings that are actually used
(`Catalog.read_user_version_tx` / `write_user_version_tx`,
`Exec.find_col_idx_by_name_opt`) so the attribute was dropped; two were
genuinely-dead private functions (`Exec.fk_child_has_ref` single-col variant,
`Exec.scan_child_rows_tx`) and were deleted. E110 is now enforceable for
`lib/` + `bin/`.

## Enabled rules: current findings (1)

All other rules stay on. Status after the #153 and #173 work:

| Rule | Name | Count | Disposition |
|------|------|-------|-------------|
| E105 | Catch-all Exception Handler | 0 | Resolved (#167) — 12 production sites narrowed; `test/` scaffolding excluded. Enforceable. |
| E005 | Long Functions | 0 | Resolved (#168) — all `lib/` + `bin/` functions decomposed into named helpers; `test/` excluded. Enforceable. |
| E300 | Variant Naming | 0 | Resolved (#153) — `BytesMap`→`Bytes_map`. |
| E310 | Value Naming | 0 | Resolved (#153) — `test_parse_datetime_T`→`_t`. |
| E335 | Used Underscore-Prefixed Binding | 0 | Resolved (#153) — dropped a dead param + the used `_stream`. |
| E400 | Missing MLI Documentation | 0 | Resolved (#153) — module headers added to json/parallel/row/mem. |
| E405 | Missing Value Documentation | 0 | Resolved (#153) — all public `.mli` values documented. |
| E415 | Missing Pretty Printer | 0 | Resolved (#153) — `pp` added to all opaque `t` types. |
| E330 | Redundant Module Name | 0 | Resolved (#153) — genuine cases fixed (`fts_query`→`t`, `store_of`→`txn_store`); intentional domain constants excluded. |
| E010 | Deep Nesting | 0 | Resolved (#153) — helpers extracted in `row.decode`, `exec` snippet/update, test `run_case`. |
| E001 | High Cyclomatic Complexity | 0 | Resolved (#153) — `parse_file` split into top-level helpers. |
| E110 | Silenced Warning | 0 | Resolved (#173) — 5 `lib/` sites fixed (3 stale suppressions dropped, 2 dead fns deleted); `test/` scaffolding excluded. Enforceable. |
| E505 | Missing MLI File | 0 | Resolved (#173) — authored `lib/sql/ast.mli` (full AST surface) and `lib/sql/plan.mli` (query-plan IR). |
| E500 | Missing `.ocamlformat` | 1 | Open — deferred to #171 (full ocamlformat adoption: profile=janestreet, version=0.29.0, tree-wide reformat + CI fmt check, for parity with `camel`). Sole remaining blocker to flipping the CI `lint` job to enforcing. |

No `Obj.magic` exists anywhere in the codebase — the linter's other headline
concern is already clean.

### Caveat

merlint's internal project build currently reports a failure ("Function type
analysis may not work properly"), so type-aware rules are degraded. The
syntactic findings above (complexity, length, catch-all handlers, naming, docs)
are unaffected and reliable.
