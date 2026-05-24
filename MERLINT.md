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

Excluding these takes the raw report from ~4077 to ~504 findings.

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
long test/bench setup is acceptable. The `#168` refactor drives E005 down in
`lib/`: `lib/sql/exec.ml` (the largest module) is now **E005-clean** — its 19
long functions, including the 1058-line `to_stream`, the 399-line
`execute_with_count`, and the `execute_insert`/`update`/`delete` drivers, were
decomposed into named helpers (behavior-preserving; full suite green). The
remaining E005 findings live in other modules (sema/store/db/catalog/btree/
planner/encoding) and are tracked for the same per-module treatment.

## Enabled rules: current findings (~357)

All other rules stay on. The notable remaining findings, none yet enforced:

| Rule | Name | Count | Disposition |
|------|------|-------|-------------|
| E105 | Catch-all Exception Handler | 0 | Resolved (#167) — 12 production sites narrowed; `test/` scaffolding excluded. Enforceable. |
| E331 | Redundant Function Prefixes | 89 | Open. |
| E005 | Long Functions | 34 | #168 in progress — `lib/sql/exec.ml` fully decomposed (E005-clean); remaining in sema/store/db/catalog/btree/planner/encoding. `test/` excluded. |
| E405 | Missing Value Documentation | 41 | Open. |
| E110 | Silenced Warning | 13 | Open — `[@@warning "-32"]`/`-69` on API/test bindings. |
| E415 | Missing Pretty Printer | 9 | Open. |
| E325 / E330 | Function / Module naming | 6 each | Open. |
| E335 | Used Underscore-Prefixed Binding | 4 | Open. |
| E010 | Deep Nesting | 4 | Open. |
| E400 | Missing MLI Documentation | 4 | Open. |
| E001 | High Cyclomatic Complexity | 3 | Open. |
| E505 | Missing MLI File | 2 | Open — `lib/sql/ast.ml`, `lib/sql/plan.ml`. |
| E500 | Missing `.ocamlformat` | 1 | Open. |
| E300 / E310 | Variant / Value naming | 1 each | Open. |

No `Obj.magic` exists anywhere in the codebase — the linter's other headline
concern is already clean.

### Caveat

merlint's internal project build currently reports a failure ("Function type
analysis may not work properly"), so type-aware rules are degraded. The
syntactic findings above (complexity, length, catch-all handlers, naming, docs)
are unaffected and reliable.
