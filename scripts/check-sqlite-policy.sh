#!/bin/sh
# Policy — no real-SQLite outside gated comparison files (#370)
#
# Granary is inspired by SQLite, not a port of it, and must not acquire a
# dependency on real SQLite.  Two guards enforce that, and they deliberately
# have DIFFERENT allowlists:
#
#   1. The `"sqlite3 ..."` error-string guard catches anything that names the
#      sqlite3 CLI binary.  The #91 `.import` REPL command intentionally shells
#      out to that CLI (optional, Unix-only, graceful when absent) and its gated
#      integration test drives it, so `bin/repl/granary_repl.ml` and
#      `test/test_sqlite_import_91.ml` are allowed here.
#
#   2. The `Sqlite3.` module guard catches use of the OCaml `sqlite3` library.
#      It is STRICTER on purpose: the two REPL/import files above are NOT
#      allowlisted for it, because shelling out to a CLI takes on no build
#      dependency while linking the module does.
#
# MERGING THE TWO LISTS WOULD SILENTLY WIDEN THE STRICT GUARD.  Do not do it.
#
# Adding a new comparison file means adding it to the relevant list(s) below —
# one place now, not four.  A designated comparison file is one that is
# `(optional)` in test/dune (so a checkout without sqlite3 does not build it)
# and exists to cross-check granary against reference C SQLite: bench_compare.ml
# and its TPC-derived siblings bench_tpch.ml (#482) and bench_tpcc.ml (#500).
#
# History: bench_tpch.ml was added by #501 without being allowlisted, leaving
# this job red on main (#505); bench_tpcc.ml repeated it verbatim in #500.  Both
# times the policy was not actually violated — the author simply did not know
# the allowlist existed.  See #574; test/dune now points here.
#
# THIS SCRIPT SELF-TESTS ON EVERY RUN (see [self_test]).  A policy gate that has
# never been observed to FAIL is a gate nobody knows works, and nothing else
# covers this one — there is no shellcheck step in CI either (#591).  Pass
# `--self-test` to run only the self-test, verbosely.

set -eu

# --- the two allowlists ----------------------------------------------------

# Guard 1: files permitted to name the sqlite3 CLI binary.
SQLITE_CLI_ALLOWLIST='
test/bench_compare.ml
test/test_sqlite_compare.ml
test/bench_perf_compare_sqlite3.ml
test/bench_tpch.ml
test/bench_tpcc.ml
bin/repl/granary_repl.ml
test/test_sqlite_import_91.ml
'

# Guard 2: files permitted to link the OCaml Sqlite3 module.  Strictly smaller
# than the list above, on purpose — see the header.
SQLITE3_MODULE_ALLOWLIST='
test/bench_compare.ml
test/bench_tpch.ml
test/bench_tpcc.ml
'

# --- matching --------------------------------------------------------------

# Roots scanned by both guards.  (bench/ is deliberately absent — see #604.)
SCAN_ROOTS='lib bin containers test'

# The allowlist is applied to the FILE LIST, never to grep's output.
#
# `find` selects the `*.ml` files under the roots and excludes the allowlisted
# paths with `! -path`; grep then runs only on what survives, so ANY hit is a
# violation and its output is reported verbatim, never parsed.  That is the
# whole point: four separate fail-opens in this gate were all caused by
# *interpreting* something instead of enforcing it.  For the record, in the
# order they were found (#574):
#
#   1. EMPTY ALLOWLIST DISABLED ITS GUARD (a regression introduced by this PR's
#      first draft).  `printf '%s\n' $EMPTY` emits one BLANK line, and
#      `grep -vFf` with a blank pattern matches every line, so emptying a list
#      suppressed every violation and the script printed "Policy OK".  That is
#      reachable: the module guard's three entries are all comparison
#      benchmarks and retiring them is a goal state, and the freshness check
#      below cannot see it because nothing is stale.  Here an empty list
#      contributes no `! -path`, so it excludes nothing — fail CLOSED.
#
#   2. UNANCHORED SUBSTRING MATCH (pre-existing on main).  Matching the
#      allowlist against `path:lineno:content` let a violation launder itself
#      by naming an allowlisted path on the same line:
#          let x = Sqlite3.db_open "x" (* mirrors test/bench_compare.ml *)
#      There is no content to confuse a path with any more.
#
#   3. `--include` IS NOT PORTABLE (pre-existing on main).  Under `busybox sh`,
#      `grep` resolves to the BusyBox applet, which rejects `--include`; the
#      `2>/dev/null` swallowed the error and BOTH guards matched nothing.  The
#      suffix is now `find -name`.
#
#   4. A COLON IN A PATH VANISHED FROM BOTH GUARDS (a regression introduced by
#      the fix for 2 and 3).  `awk -F:` split `grep -rn` output on every colon,
#      so `lib/zz:colon.ml` presented as field 1 `lib/zz`, failed the `.ml`
#      test and was dropped — fail-open, where main's `--include` matched it
#      and reported it.  The previous comment here NAMED that assumption ("no
#      source path contains a colon") without enforcing it; a documented
#      assumption in an enforcement gate is a fail-open with a note attached.
#      Nothing is field-split now, so the assumption is gone rather than
#      documented.
#
# Tool surface: `find` with `-type f -name -path ! -exec ... +` and `grep -n -E
# -e`.  All POSIX.  (The previous note here claimed "nothing outside POSIX
# grep" while using `grep -r`, which POSIX does not define — that is exactly
# the false confidence that produced 3, so: `-r`/`-R` and `--include` are both
# gone.)

# Print `path:lineno:content` for every hit of a pattern in a *.ml file under
# SCAN_ROOTS that is NOT in the given allowlist.
#
# $1 = allowlist (newline-separated), $2 = grep flag (-E or -F), $3 = pattern.
scan_outside_allowlist() {
  sc_allow=$1
  sc_flag=$2
  sc_pattern=$3
  # Build the `! -path X` exclusions positionally so that find's argv is exact
  # (an entry containing a space reaches find as ONE argument).  Note this does
  # not make such an entry work end-to-end: the source loop still splits on
  # whitespace, so `a b.ml` arrives as two entries, neither of which exists —
  # fail-closed and loudly, via the freshness check.
  set --
  # `set -f` here is DEFENCE IN DEPTH and is deliberately not independently
  # covered by the self-test: [reject_glob_entries] runs first and refuses any
  # entry that could be expanded, so removing this line changes no observable
  # behaviour today.  It stays because the day the rejection is bypassed or
  # reordered, this is what keeps `find` from receiving a widened exclusion set.
  # The load-bearing copy — the one the self-test does pin — is in
  # [reject_glob_entries].
  set -f
  for sc_p in $sc_allow; do
    set -- "$@" '!' -path "$sc_p"
  done
  set +f
  # /dev/null in the grep argument list forces the filename prefix even when
  # `-exec ... +` hands grep a single file.
  # shellcheck disable=SC2086
  find $SCAN_ROOTS -type f -name '*.ml' "$@" \
    -exec grep -n "$sc_flag" -e "$sc_pattern" /dev/null {} + || true
}

# `set -f` HERE is the load-bearing one, and the self-test pins it.  Unquoted
# expansion does field splitting AND THEN pathname expansion, so without it an
# entry like `test/*` is resolved against the CWD into real filenames before
# this check can inspect it — the rejection becomes dead code and `find` gets a
# silently widened exclusion set.  That was the fifth fail-open in this file
# (#574 review round 3), and it shipped in the very commit that added the
# rejection meant to prevent it.
#
# `find ! -path` takes a GLOB, so an entry carrying a metacharacter excludes
# more than the one file it names — `test/*` exempts every .ml under the largest
# scan root and prints "Policy OK".  A backslash is in the same category:
# `find -path` reads `\c` as a literal `c`, so `test/bench_\compare.ml` would
# additionally exclude `test/bench_compare.ml`.  Reject the lot; entries are
# meant to be literal paths, and the freshness check requires each to exist.
#
# The `*X*` forms matter: an earlier draft wrote `*'*'`, which only matches a
# metacharacter at the END of the entry, so `test/*.ml` sailed through.
reject_glob_entries() {
  rge_label=$1
  rge_bad=0
  set -f
  for rge_p in $2; do
    case $rge_p in
      *'*'* | *'?'* | *'['* | *']'* | *'\'*)
        echo "FAIL: the $rge_label allowlist entry '$rge_p' contains a glob"
        echo "      metacharacter or a backslash.  Entries are matched with"
        echo "      'find ! -path', so it would exclude more files than it names."
        rge_bad=1
        ;;
    esac
  done
  set +f
  return $rge_bad
}

# Both guards, against the tree in the CURRENT directory.  Parameterised on the
# two allowlists so [self_test] can drive it with planted violations and with a
# deliberately emptied list.  Returns 0 iff both pass, and reports BOTH rather
# than exiting at the first.
run_guards() {
  cli_allow=$1
  mod_allow=$2
  rc=0

  # `rg_` prefix: the loop variable must not collide with [self_test]'s $root,
  # which holds the temp tree that gets `rm -rf`'d.  Today [expect] calls this
  # in a subshell so the collision is harmless; that is not a property worth
  # depending on.
  for rg_root in $SCAN_ROOTS; do
    if [ ! -d "$rg_root" ]; then
      echo "FAIL: scan root '$rg_root' does not exist; this guard would silently"
      echo "      stop covering it.  Run from the repository root, or update"
      echo "      SCAN_ROOTS in scripts/check-sqlite-policy.sh."
      rc=1
    fi
  done

  # Rejected HERE, where the pattern is consumed, rather than only alongside the
  # freshness check — so that every caller of [run_guards] is covered, including
  # the self-test, which is what makes the rejection testable at all.
  reject_glob_entries 'sqlite3-CLI' "$cli_allow" || rc=1
  reject_glob_entries 'Sqlite3-module' "$mod_allow" || rc=1

  [ "$rc" -eq 0 ] || return 1

  cli_violations=$(scan_outside_allowlist "$cli_allow" -E '"sqlite3[ "]')
  if [ -n "$cli_violations" ]; then
    echo "FAIL: sqlite3 reference outside designated comparison files:"
    echo "$cli_violations"
    echo
    echo "If this is a designated comparison file, add it to"
    echo "SQLITE_CLI_ALLOWLIST in scripts/check-sqlite-policy.sh."
    rc=1
  fi

  mod_violations=$(scan_outside_allowlist "$mod_allow" -F 'Sqlite3.')
  if [ -n "$mod_violations" ]; then
    echo "FAIL: Sqlite3 module used outside the gated comparison benchmarks:"
    echo "$mod_violations"
    echo
    echo "If this is a designated comparison benchmark, add it to"
    echo "SQLITE3_MODULE_ALLOWLIST in scripts/check-sqlite-policy.sh.  Shelling"
    echo "out to the CLI does NOT qualify — that belongs in"
    echo "SQLITE_CLI_ALLOWLIST only."
    rc=1
  fi

  return $rc
}

# --- a stale allowlist must fail loudly ------------------------------------
#
# A renamed or deleted comparison file would otherwise leave a dead entry
# behind that quietly widens the guard for whatever takes that name next.

check_allowlist_fresh() {
  label=$1
  list=$2
  stale=0
  # `set -f` for the same reason as in [scan_outside_allowlist]: without it an
  # entry containing a glob is expanded against the CWD into files that DO
  # exist, so this check silently passes it.
  set -f
  for path in $list; do
    if [ ! -f "$path" ]; then
      echo "FAIL: the $label allowlist names '$path', which does not exist."
      echo "      A renamed or deleted comparison file leaves a dead entry that"
      echo "      silently widens this guard.  Fix the list in"
      echo "      scripts/check-sqlite-policy.sh."
      stale=1
    fi
  done
  set +f
  return $stale
}

# --- self-test -------------------------------------------------------------
#
# Builds a throwaway tree, plants violations in it, and asserts that each guard
# FAILS where it must and PASSES where it must.  Runs on every invocation,
# because otherwise these two guards are only ever observed passing — and the
# two defects above were both "reviewable-only": no test, and no shellcheck
# step in CI (#591), would have surfaced either.

self_test() {
  verbose=$1
  # The call site is `if ! self_test`, and POSIX suspends `set -e` for the whole
  # of a function invoked in a condition — so an unchecked `mktemp -d` failure
  # would carry on with root='' and `mkdir -p "$root/lib"` would create /lib,
  # /test, /containers and /bin/repl, IN THE CI CONTAINER, after which the
  # guards grep the image's own /lib and /bin (#574 review, reproduced under
  # --user 0 with a bad TMPDIR).  Check it.
  root=$(mktemp -d) || return 1
  case ${root:-} in
    /*/*) ;;
    *) return 1 ;;
  esac
  st_fail=0

  mkdir -p "$root/lib" "$root/bin/repl" "$root/containers" "$root/test"
  # The three module-guard entries, each legitimately using BOTH forms — this
  # is what the real tree looks like.
  for f in test/bench_compare.ml test/bench_tpch.ml test/bench_tpcc.ml; do
    printf 'let _ = Sqlite3.db_open "x"\nlet _ = failwith "sqlite3 not found"\n' \
      >"$root/$f"
  done
  # CLI-only entries: they name the binary but must never link the module.
  printf 'let _ = failwith "sqlite3 not found"\n' >"$root/bin/repl/granary_repl.ml"
  printf 'let _ = failwith "sqlite3 not found"\n' >"$root/test/test_sqlite_import_91.ml"

  st_cli='
test/bench_compare.ml
test/bench_tpch.ml
test/bench_tpcc.ml
bin/repl/granary_repl.ml
test/test_sqlite_import_91.ml
'
  st_mod='
test/bench_compare.ml
test/bench_tpch.ml
test/bench_tpcc.ml
'

  report() {
    if [ "$2" = "$1" ]; then
      [ "$verbose" = 0 ] || echo "  ok   (expected $1) $3"
    else
      echo "  BROKEN: expected $1, got $2 — $3"
      st_fail=1
    fi
  }

  expect() {
    if (
      cd "$root"
      run_guards "$3" "$4"
    ) >/dev/null 2>&1; then
      report "$1" pass "$2"
    else
      report "$1" fail "$2"
    fi
  }

  # [check_allowlist_fresh] is a separate entry point from [run_guards] and has
  # its own `set -f`; without a case here, removing that `set -f` is a mutation
  # nothing notices.
  expect_fresh() {
    if (
      cd "$root"
      check_allowlist_fresh 'self-test' "$3"
    ) >/dev/null 2>&1; then
      report "$1" pass "$2"
    else
      report "$1" fail "$2"
    fi
  }

  expect pass "clean tree: allowlisted files use both forms" "$st_cli" "$st_mod"
  expect_fresh pass "freshness: every allowlisted path exists" "$st_cli"
  expect_fresh fail "freshness: a missing allowlisted path is reported" \
    "$st_cli
test/gone.ml"
  expect_fresh fail "freshness: a glob entry is not expanded into existing files" \
    "$st_cli
test/*"

  printf 'let _ = failwith "sqlite3 not found"\n' >"$root/lib/zz_probe.ml"
  expect fail "CLI guard catches a planted error string in lib/" "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_probe.ml"

  printf 'let _ = Sqlite3.db_open "x"\n' >"$root/lib/zz_probe.ml"
  expect fail "module guard catches a planted Sqlite3. use in lib/" "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_probe.ml"

  # The #574-review regression: an EMPTIED allowlist must allow nothing, not
  # everything.  The tree still contains the three comparison benchmarks, so
  # with an empty module list they are violations.
  expect fail "module guard still fires when ITS allowlist is EMPTY" "$st_cli" ""
  expect fail "CLI guard still fires when ITS allowlist is EMPTY" "" "$st_mod"

  # Naming an allowlisted path on the offending line must not launder it.
  printf 'let _ = Sqlite3.db_open "x" (* mirrors test/bench_compare.ml *)\n' \
    >"$root/lib/zz_probe.ml"
  expect fail "an allowlisted path on the same line does not launder a violation" \
    "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_probe.ml"

  # A COLON in the path must not make a file invisible to either guard.  The
  # awk -F: draft dropped it silently; main's --include reported it.
  printf 'let _ = Sqlite3.db_open "x"\nlet _ = failwith "sqlite3 not found"\n' \
    >"$root/lib/zz:colon.ml"
  expect fail "a colon in the path does not hide a violation" "$st_cli" "$st_mod"
  rm -f "$root/lib/zz:colon.ml"

  mkdir -p "$root/lib/zz:dir"
  printf 'let _ = Sqlite3.db_open "x"\n' >"$root/lib/zz:dir/probe.ml"
  expect fail "a colon in a DIRECTORY does not hide a violation" "$st_cli" "$st_mod"
  rm -rf "$root/lib/zz:dir"

  # Suffix selection must be real: a non-.ml file carrying the pattern is NOT a
  # violation.  Without this, deleting the `-name '*.ml'` filter passes clean,
  # since every other fixture here is already .ml.
  printf 'doc: the engine raises failwith "sqlite3 not found" here,\nand Sqlite3.db_open is never called.\n' \
    >"$root/test/notes.md"
  expect pass "a non-.ml file carrying BOTH patterns is not a violation" \
    "$st_cli" "$st_mod"
  rm -f "$root/test/notes.md"

  # A missing scan root must be reported, not silently skipped.
  mv "$root/containers" "$root/containers-moved"
  expect fail "a missing scan root is reported" "$st_cli" "$st_mod"
  mv "$root/containers-moved" "$root/containers"

  # An allowlist entry containing a glob must be REJECTED, not honoured.  Left
  # untested, the rejection rots straight back out: it shipped once as dead code
  # because the loop expanded `test/*` into real filenames before the check saw
  # it (#574 review round 3).  The planted violation makes the mutation visible
  # — honouring `test/*` would exempt it and this case would pass.
  printf 'let _ = failwith "sqlite3 not found"\n' >"$root/test/zz_violation.ml"
  expect fail "control: a planted violation under test/ is caught" "$st_cli" "$st_mod"
  expect fail "a glob entry ('test/*') is rejected, not honoured" \
    "$st_cli
test/*" "$st_mod"
  # Each glob fixture must MATCH the planted violation, or it cannot tell a
  # working rejection from a broken one: a pattern that exempts nothing leaves
  # the violation caught either way.  `test/bench_*.ml` was such a fixture and
  # survived the "only reject a TRAILING metacharacter" mutation.
  expect fail "a mid-entry glob ('test/zz_*.ml') is rejected, not honoured" \
    "$st_cli
test/zz_*.ml" "$st_mod"
  # find's -path reads \v as a literal v, so this entry would exclude
  # test/zz_violation.ml if the backslash arm were dropped.
  expect fail "a backslash entry is rejected, not honoured" \
    "$st_cli
test/zz_\\violation.ml" "$st_mod"
  rm -f "$root/test/zz_violation.ml"

  # The exclusion must be ANCHORED at the path, not a suffix match.  Loosening
  # `-path "$sc_p"` to `-path "*$sc_p"` would exempt anything ENDING in an
  # allowlisted path; this fixture is what notices.
  mkdir -p "$root/lib/nest/test"
  printf 'let _ = Sqlite3.db_open "x"\nlet _ = failwith "sqlite3 not found"\n' \
    >"$root/lib/nest/test/bench_compare.ml"
  expect fail "a path merely ENDING in an allowlisted path is not exempt" \
    "$st_cli" "$st_mod"
  rm -rf "$root/lib/nest"

  # The asymmetry itself: a CLI-allowlisted file may not link the module.
  printf 'let _ = Sqlite3.db_open "x"\n' >>"$root/bin/repl/granary_repl.ml"
  expect fail "a CLI-allowlisted file may not link the Sqlite3 module" \
    "$st_cli" "$st_mod"

  rm -rf "$root"
  return $st_fail
}

# --- main ------------------------------------------------------------------

if [ "${1:-}" = "--self-test" ]; then
  self_test 1 || exit 1
  echo "Self-test OK: both guards fail where they must and pass where they must"
  exit 0
fi

if ! self_test 0; then
  echo "FAIL: this policy script's own self-test is broken, so its verdict"
  echo "      below cannot be trusted.  Fix scripts/check-sqlite-policy.sh."
  exit 1
fi

cd "$(dirname "$0")/.."

fresh=0
check_allowlist_fresh 'sqlite3-CLI' "$SQLITE_CLI_ALLOWLIST" || fresh=1
check_allowlist_fresh 'Sqlite3-module' "$SQLITE3_MODULE_ALLOWLIST" || fresh=1
[ "$fresh" -eq 0 ] || exit 1

run_guards "$SQLITE_CLI_ALLOWLIST" "$SQLITE3_MODULE_ALLOWLIST" || exit 1

echo "Policy OK: all sqlite3 references confined to gated comparison files"
