# Dead-code analysis (#160)

[`dead_code_analyzer`](https://github.com/LexiFi/dead_code_analyzer) (LexiFi)
reports exported values, methods, constructors and record fields that nothing
references. It reads `.cmt`/`.cmti` files, so it sees across module boundaries —
unlike the compiler's warning 32, which is per-file.

It is **advisory**. Unlike merlint and ocamlformat it does **not** gate CI; see
"Why it is not a gate" below.

## Running it

The tool is baked into the `granary-dev` image (see `Containerfile`).

```sh
# 1. Build with @check — a plain `dune build` emits only ONE test .cmt, which
#    makes the entire public API look unreferenced (185 findings instead of 56).
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build @check

# 2. Analyse lib/, counting test/ and bin/ as references.
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev \
  dead_code_analyzer \
    --references _build/default/test \
    --references _build/default/bin \
    -E all -M all \
    _build/default/lib
```

Step 1 is the whole trick. `dune build` compiles only what the default alias
needs; the test executables are not in it, so every value that only the test
suite calls — `Db.query_as_of`, `Mem.create`, most of `Db` — is reported as
dead. `@check` builds `.cmt`s for everything without linking or running.

`@check` currently fails on `test/bench_compare.ml` (needs the `sqlite3` opam
library, absent from the dev image by design). The stanza is `(optional)` —
`test/dune:673` — so an ordinary build silently skips it; `@check` is not an
ordinary build, and asks for a `.cmt` from every stanza whether or not its
libraries resolve, which is why only this target sees the failure. It is
expected and harmless: dune still emits the other 119 test `.cmt`s before it
stops.

## Baseline (2026-07-31, `ca67f20` + our 5.4 port)

56 findings: 49 unused exported values, 7 unused constructors/record fields,
0 unused methods.

**~19 of the 49 are `pp`.** These are not actionable — see the rule collision
below. Netting those out leaves ~30 real candidates, of which the ones worth
triaging first are:

| Symbol | Note |
|---|---|
| `Db.vacuum` | shipped in #413; exercised via SQL text, not the OCaml entry point |
| `Db.create_worker_handle`, `Db.iter_with_stats`, `Db.param_slot` | |
| `Recovery.scan`, `Recovery.pp_scanned_page` | #85 recovery surface |
| `Replication.cold_restore` | |
| `Crypto.canary_adata`, `Crypto.tag_len` | |
| `Store.backup_state`, `Store.backup_gate_max_yields`, `Store.set_backup_gate_max_yields` | |
| `Catalog.pending_fk_check.{pfk_kind,pfk_table,pfk_rowid}` | whole record unread |
| `Exec.query_stats.used_index` | populated, never consumed |
| `Wal.frame.frame_idx` | |
| `Ast.binop_to_sql`, `Ast.func_to_sql` | |

## Never prune: exports whose only consumer is a guard

Some exported values exist so that a *test* can hold onto something the engine
itself never calls. `dead_code_analyzer` cannot see that, and reports them like
any other unused export — but deleting them does not fail anything, which is
exactly what makes them dangerous to prune: the guard stops guarding and
nothing says so.

| Symbol | Why it must stay |
|---|---|
| `Granary_encoding.Sql_ident.sql_keywords`, `.is_sql_keyword`, `.ident_needs_quoting` | The handle the `lexer.mll` drift guard in `test/test_ident_quoting_572.ml` holds. That guard re-derives the keyword table from `lexer.mll` and fails when it and `sql_keywords` have drifted apart — the only thing binding the two now that they live in different libraries. Delete them and the next reserved word added to the lexer corrupts stored SQL again (#577, #572, #619). |

Before triaging a finding on this list, read the `(** ... *)` note at the top
of the owning `.mli`. Anything added here owes itself the same note next to the
values, not only a row in this table — the table is the index, the `.mli` is
where the reader actually is (#619).

Note that the 2026-07-31 baseline above was taken before #619 removed the three
`Granary_sql.Ast` aliases (`sql_keywords`, `is_sql_keyword`,
`ident_needs_quoting`) that used to forward to these, so a rerun should report
up to three fewer unused exports.

## Why it is not a gate

**It collides with merlint.** merlint's `pp`-for-abstract-`type t` rule
*requires* a `pp` on every module exposing an abstract `t`; dead_code_analyzer
then reports each of those `pp`s as unused, because nothing in the codebase
calls them (they exist for debugging and for merlint). Gating both would make
the two linters unsatisfiable at the same time. Any future gate must exclude
`pp` first.

**Upstream has known false-positive classes.** Uses are not tracked through
type equations ([#79](https://github.com/LexiFi/dead_code_analyzer/issues/79),
[#80](https://github.com/LexiFi/dead_code_analyzer/issues/80)), module aliases
([#81](https://github.com/LexiFi/dead_code_analyzer/issues/81)), `include`
([#82](https://github.com/LexiFi/dead_code_analyzer/issues/82)), or first-class
modules ([#83](https://github.com/LexiFi/dead_code_analyzer/issues/83)).
Functor-heavy code — `lib/block/`, `lib/sample/` — is the worst affected: the
first run flagged all of `Mirage_backend.Make`.

So: run it before a release or when pruning, triage by hand, delete what is
genuinely dead. Do not wire it into CI until the `pp` collision is resolved.

## The OCaml 5.4 fork

Upstream declares `ocaml (>= 5.3) (< 5.4)` and, as of 2026-07-31, has no 5.4
branch and no 5.4 issue — [issue
#89](https://github.com/LexiFi/dead_code_analyzer/issues/89) jumps straight to
"OCaml 5.5 support" ("No rush, just a TODO"). The `Containerfile` therefore
pins
[`tejpochiraju/dead_code_analyzer@ae94d2f`](https://github.com/tejpochiraju/dead_code_analyzer/commit/ae94d2f83e2ba9dcedcc96bf0bff568021b41aca),
a 21-line port over upstream `master` (`ca67f20`) fixing four mechanical
compiler-libs breakages:

1. `Ttuple` carries `(string option * type_expr) list` (labeled tuples).
2. `Tpat_alias` gained a fifth argument.
3. `Types.label_description` moved to the new `Data_types` module.
4. `Texp_apply` arguments are `apply_arg = (expression, unit) arg_or_omitted`
   rather than `expression option`. Normalised at the `DeadArg.register_uses`
   and `DeadObj.arg` boundaries (`Arg e` ↔ `Some e`, `Omitted ()` ↔ `None`), so
   the analysis proper is untouched.

Same playbook as the `bisect_ppx` pin in `coverage.yml` (#166). Drop the fork
and pin upstream once it supports 5.4.

## History

#160 was first investigated in phase 43 (2026-05) and deferred: the spike
concluded that "LexiFi 1.2.0 and fantazio 4.14 both predate OCaml 5.x". That
was wrong — 1.1.1 targeted 5.2 and 1.2.0 targeted 5.3, and upstream has been
actively maintained through July 2026. The real blocker was only ever the
one-minor-version lag behind our compiler.

Rejected alternatives:

- **`reanalyze`** — constrained to `ocaml < 5.3`, further behind than LexiFi's
  tool and ReScript-oriented.
- **`ocaml-index`** — natively supports 5.4, but it is an *index* of value
  usages, not an analyzer; using it would mean writing the dead-code pass
  ourselves. Kept as the fallback if the fork becomes unmaintainable.
