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
follow-ups below, then flip `continue-on-error` to `false` to enforce.

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

Excluding these takes the raw report from ~4077 to ~265 findings.

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

## Enabled rules: current findings (~87)

All other rules stay on. The notable remaining findings, none yet enforced:

| Rule | Name | Count | Disposition |
|------|------|-------|-------------|
| E105 | Catch-all Exception Handler | 0 | Resolved (#167) — 12 production sites narrowed; `test/` scaffolding excluded. Enforceable. |
| E005 | Long Functions | 0 | Resolved (#168) — all `lib/` + `bin/` functions decomposed into named helpers; `test/` excluded. Enforceable. |
| E405 | Missing Value Documentation | 41 | Open — being driven to zero under #153. |
| E110 | Silenced Warning | 13 | Open — `[@@warning "-32"]`/`-69` on API/test bindings. |
| E415 | Missing Pretty Printer | 9 | Open — adding `pp` to opaque `t` types. |
| E330 | Redundant Module Name | 6 | Open — `fts_query`/`store_of` fixable; `page_size`/`wal_magic` under review. |
| E010 | Deep Nesting | 4 | Open. |
| E335 | Used Underscore-Prefixed Binding | 4 | Open. |
| E400 | Missing MLI Documentation | 4 | Open. |
| E505 | Missing MLI File | 2 | Open — `lib/sql/ast.ml`, `lib/sql/plan.ml`; large interface task, deferred to a follow-up. |
| E001 | High Cyclomatic Complexity | 1 | Open — `test/test_sqlite_corpus.ml:parse_file`. |
| E500 | Missing `.ocamlformat` | 1 | Open. |
| E300 / E310 | Variant / Value naming | 1 each | Open. |

No `Obj.magic` exists anywhere in the codebase — the linter's other headline
concern is already clean.

### Caveat

merlint's internal project build currently reports a failure ("Function type
analysis may not work properly"), so type-aware rules are degraded. The
syntactic findings above (complexity, length, catch-all handlers, naming, docs)
are unaffected and reliable.
