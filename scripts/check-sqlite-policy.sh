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
#   2. The `Sqlite3` module guard catches use of the OCaml `sqlite3` library.
#      It is STRICTER on purpose: the two REPL/import files above are NOT
#      allowlisted for it, because shelling out to a CLI takes on no build
#      dependency while linking the module does.  Its allowlist is itself in two
#      parts — files that LINK the module, and files that only NAME it (#605) —
#      because only the first set is a comparison benchmark and only the first
#      set is what the #571 keep-alive lint must cover (#601).
#
# MERGING THE TWO GUARDS' LISTS WOULD SILENTLY WIDEN THE STRICT GUARD.  Do not
# do it.  (Splitting guard 2's own list, by contrast, NARROWS what each half
# means, which is why it is safe.)
#
# What is scanned: `*.ml` and `*.mli` under lib/ bin/ containers/ test/ bench/,
# excluding `_build`, with symlinks REFUSED rather than skipped.  All three of
# those were blind spots until #604 — see [SCAN_ROOTS] and [check_no_symlinks].
# Since #621, `dune` files under those roots and the root `dune-project` are
# scanned too, by guard 4.
#
# #621, and why this section had become a LIE.  Guard 2's pattern was the fixed
# string `Sqlite3.` — the qualifier WITH ITS DOT — so every ordinary way of
# using an OCaml module walked straight past it:
#
#     open Sqlite3            let _ = db_open "x"
#     let f () = let open Sqlite3 in db_open "x"
#     module S = Sqlite3      let _ = S.db_open "x"
#     let _ = Sqlite3 . db_open "x"          (* a space before the dot *)
#
# Each of those was verified to print "Policy OK", rc 0.  These are not
# evasions; `open Sqlite3` at the top of an engine file is the idiomatic
# spelling, and it got a green gate.  Worse, #574 and #607 had by then made this
# header read as a COMPLETENESS CLAIM — which is #549's disease, a marker that
# trains people to trust something untrue.  The pattern is now the whole WORD.
#
# The second half of the same hole: no `dune` file was ever scanned, and
# `(libraries sqlite3)` is what actually creates the build dependency the whole
# policy exists to prevent.  Guard 4 closes that, with the limit stated at its
# allowlist.
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
# never been observed to FAIL is a gate nobody knows works.  Since #591 CI does
# run shellcheck over scripts/, but that is a syntactic linter: it cannot tell a
# working guard from one that greps for the wrong thing, so the self-test is
# still the only thing covering what this script MEANS.  Pass `--self-test` to
# run only the self-test, verbosely.

set -eu

# Overwritten by [run_guards] with a real temp file; /dev/null keeps
# [scan_outside_allowlist] usable on its own without tripping `set -u`.
SCAN_ERR_FILE=/dev/null

# --- the allowlists --------------------------------------------------------

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

# Guard 2 is in TWO parts, and the split is load-bearing rather than cosmetic.
#
# 2a. The designated comparison benchmarks: they really do link the module.
#     This list is also the source of truth for the #571 keep-alive lint — see
#     [check_lint_coverage] — so a file added here and nowhere else is still
#     linted, which is what #601 was about.
SQLITE3_COMPARISON_ALLOWLIST='
test/bench_compare.ml
test/bench_tpch.ml
test/bench_tpcc.ml
'

# 2b. Files that NAME the module without linking it.  The keep-alive lint scans
#     the comparison benchmarks' source text for unguarded calls, so it has to
#     spell the qualifier it is looking for; a lint whose whole job is to police
#     that module cannot avoid naming it.
#
#     Until #605 that file dodged this guard by writing `"Sqlite" ^ "3"`.  That
#     worked silently, recorded no reason where the policy could see one, and —
#     worst — established "split the literal" as an accepted technique in-tree,
#     which is the path a REAL violation takes next and which this guard cannot
#     distinguish from the honest case.  An allowlist entry is the same
#     exemption made reviewable.
#
#     Because "names it" and "links it" are indistinguishable to a grep, every
#     entry here must ALSO carry the marker below at the site, so the exemption
#     is visible in the file and not only in this script.  See
#     [check_names_only_entries].
#
#     test_sqlite_keepalive_571.ml joined this list in #621, not because it
#     changed but because the guard did: it pins the lint's own binding line as
#     SOURCE TEXT (see [test_binding_module_is_unsplit_in_the_source]), which
#     means quoting the bare module name, which the widened whole-word pattern
#     now sees.  Widening a guard is supposed to surface exactly this kind of
#     honest naming; the entry plus its marker is the record of it.
SQLITE3_NAMES_ONLY_ALLOWLIST='
test/tpc/tpc_keepalive_lint.ml
test/tpc/tpc_keepalive_lint.mli
test/test_sqlite_keepalive_571.ml
'

NAMES_ONLY_MARKER='sqlite3-policy: names-only'

# Guard 3: nobody assembles the module name out of pieces, ANYWHERE in the tree.
#
# This is the guard #605 actually needed and the first draft of this PR did not
# have.  That draft argued the exemption would go dead if the literal were
# re-split — but the exempted file also NAMES the module in its prose, so the
# exemption stayed alive and `sed -i 's/"Sqlite3"/"Sqlite" ^ "3"/'` passed green,
# with the allowlist now providing the cover the bare split never had.  The
# self-test case that was supposed to catch it used a fixture containing nothing
# but the split literal — a file unlike the real one in exactly the way that
# mattered.  Fixtures that do not resemble their subject test the fixture.
#
# It is deliberately tree-wide and has NO allowlist: an allowlisted file has no
# reason to split the name either, and the failure mode this addresses is a
# BRAND-NEW file splitting it to dodge guard 2 entirely, which a state-only pin
# on the current file cannot reach.
#
# What it matches: a `let`/`and` binding — optionally `rec`, optionally with a
# type annotation — whose right-hand side concatenates a string literal that is a
# proper prefix of `Sqlite3` with anything, or concatenates anything with a
# literal that is a proper suffix of it.  Interior splits (`"Sqli" ^ "te3"`) fall
# out of that pair for free.  Anchoring at the binding is what keeps it off the
# two places in the tree that discuss the evasion in PROSE (test_sqlite_keepalive_
# 571.ml, and a comment in bench_wal_fsync_overlap.ml); dropping the anchor would
# hit exactly those two lines and the guard would have to be disarmed instead.
#
# WHAT IT DOES NOT CATCH.  Measured, not guessed — a 20-case battery was run
# against it, and this is the honest half of the result:
#
#   - ANY LINE BREAK between the binding and the concatenation.  `let m =\n
#     "Sqlite" ^ "3"` walks straight past, and so does the same text with a decoy
#     comment above it.  **This repo is ocamlformat-enforced, so a binding that
#     ever grew past the margin would be WRAPPED BY THE FORMATTER and the guard
#     would disarm itself.**  That is not an exotic evasion; it is the same code,
#     reformatted.  A line-oriented grep cannot close it at all — closing it means
#     parsing, or at least joining logical lines, which is a different tool.
#   - Bindings this anchor still does not name: a tuple pattern, `let () = ignore
#     (…)`, a `function` argument, a record field on its own line.
#   - Assembly that is not `^` at all: `Printf.sprintf "Sqlite%d" 3`,
#     `String.concat`, `String.sub`, `Buffer.add_string`, and the decimal escape
#     `"\083qlite3"`, which contains no matchable fragment whatsoever.
#   - A split spread across two separate bindings.
#
# A grep cannot read intent, and this one is not trying to.  Its job is to make
# the KNOWN workaround loud so it stops being the path of least resistance, and
# for the one file that has a reason to name the module at all, the exact source
# line is additionally pinned from OCaml in test_sqlite_keepalive_571.ml.  Anyone
# reaching for `sprintf` to smuggle the name past this is no longer taking a
# shortcut they could mistake for normal practice.
SPLIT_LITERAL_PATTERN='^[[:space:]]*(let|and)([[:space:]]+rec)?[[:space:]]+[A-Za-z_][A-Za-z0-9_'"'"']*([[:space:]]*:[^=]*)?[[:space:]]*=.*("(S|Sq|Sql|Sqli|Sqlit|Sqlite|Sqlite3)"[[:space:]]*\^|\^[[:space:]]*"(qlite3|lite3|ite3|te3|e3|3)")'

# What guard 2 matches.  Held once so that [check_names_only_entries] can ask
# the converse question — does an exempted file still NEED its exemption? — with
# the same pattern the guard uses, rather than a second copy of it.
#
# The WHOLE WORD since #621, where the fixed string `Sqlite3.` missed `open
# Sqlite3`, `module S = Sqlite3` and a space before the dot — see the header.
# It is an ERE, so both consumers pass `-E`; a leftover `-F` would search for
# these parentheses literally and match nothing, which is a fail-open, so the
# self-test's planted-violation cases are what keep the two flags honest.
#
# The boundaries are spelled as character classes rather than `\b` on purpose:
# `\b` is a GNU extension that POSIX does not define and the BusyBox grep applet
# does not accept, and fail-open #3 in this file was exactly a non-portable grep
# flag whose error went unnoticed.
#
# What the boundaries deliberately do NOT match: a LONGER identifier that merely
# contains the name, `Sqlite3_ext.open_` or `MySqlite3`.  Those name a different
# module, and anything that really wraps the bindings has to write `Sqlite3`
# somewhere to do it.  The self-test pins that too, so a future "just make it a
# substring match" mutation shows up as a red case rather than as noise in
# everyone's PR.
SQLITE3_MODULE_PATTERN='(^|[^A-Za-z0-9_])Sqlite3([^A-Za-z0-9_]|$)'

# What guard 2 actually consumes.  Keep this an assembly of the two lists above,
# never a third hand-written copy.
SQLITE3_MODULE_ALLOWLIST="$SQLITE3_COMPARISON_ALLOWLIST
$SQLITE3_NAMES_ONLY_ALLOWLIST"

# Guard 4: no BUILD file may declare a dependency on the `sqlite3` library
# outside the one directory whose dune file declares the comparison benchmarks
# (#621).
#
# This is the half the policy never had.  Guards 1-3 read source text; the thing
# that actually creates the dependency is `(libraries ... sqlite3 ...)`, and no
# dune file was scanned at all.  A new `lib/foo/dune` could take the dependency
# on the whole engine and print "Policy OK".
#
# STATED LIMIT, because an overclaim here is what #621 was about: `test/dune` is
# allowlisted WHOLE, and it is where the comparison benchmarks' `(optional)`
# stanzas live, so this guard says nothing about a fourth stanza added there.
# `test/tpc/dune` is allowlisted because its comments discuss the dependency it
# deliberately does NOT take, and a grep cannot tell prose from a stanza.  What
# the guard does close is a build dependency appearing anywhere ELSE — lib/,
# bin/, containers/, bench/, and the root `dune-project`, i.e. every place the
# engine itself is built.  Narrowing `test/dune` to the stanza level needs an
# s-expression reader, not a grep; #591's shellcheck gate is a separate matter
# and is not being built here either.
SQLITE3_DUNE_ALLOWLIST='
test/dune
test/tpc/dune
'

# Same whole-word shape and the same portability argument as
# SQLITE3_MODULE_PATTERN, lowercased: a dune library name is `sqlite3`, and
# `bench_perf_compare_sqlite3` (an executable NAME containing it) must not
# match, which the leading boundary handles.
SQLITE3_DUNE_PATTERN='(^|[^A-Za-z0-9_])sqlite3([^A-Za-z0-9_]|$)'

# The root `dune-project` is not under any scan root, so `find` never reaches
# it, and it is scanned by name with NO allowlist: a `(depends sqlite3)` there
# would put the dependency on the granary package itself, which no comparison
# benchmark needs and nothing in the tree has ever wanted.
DUNE_PROJECT_FILE='dune-project'

# The #571 keep-alive lint, whose own list of comparison sources must agree with
# SQLITE3_COMPARISON_ALLOWLIST (#601).
KEEPALIVE_LINT_SOURCE='test/tpc/tpc_keepalive_lint.ml'

# --- matching --------------------------------------------------------------

# Roots scanned by both guards.  `bench/` is here since #604: it holds
# microbenchmarks and is exactly the kind of place a real-SQLite comparison gets
# written.  `_build` is pruned inside every root — a build tree is copied
# sources plus dune's own symlinks, not source.
SCAN_ROOTS='lib bin containers test bench'

# The scanned suffixes are `.ml` and `.mli`, spelled inline in
# [scan_outside_allowlist].  `.mli` is scanned since #604: an interface exposing
# the binding's types (`val x : Sqlite3.db`) forces the dependency on every
# consumer, which is the same consequence as an implementation using it.
#
# They are NOT held in a variable: an unquoted `$SCAN_SUFFIXES` holding
# `-name *.ml` is pathname-expanded against the CWD before find ever sees it,
# which is fail-open #5 in this very file wearing a different hat.  And two
# `-name` arms, not `*.ml*` — the wildcard form would also swallow `.mll`/`.mly`,
# and the self-test pins that it does not.

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

# Print `path:lineno:content` for every hit of a pattern in a scanned file under
# SCAN_ROOTS that is NOT in the given allowlist.
#
# $1 = allowlist (newline-separated), $2 = grep flag (-E or -F), $3 = pattern,
# $4 = file kind: `ml` (the default, `*.ml` and `*.mli`) or `dune` (`dune` and
# `dune-project`, added by #621).  The kind is a parameter rather than a second
# copy of this function because everything else here — the allowlist applied to
# the FILE LIST, the `_build` prune, the symlink refusal, the captured stderr —
# is exactly the set of fail-opens a second copy would reacquire one at a time.
scan_outside_allowlist() {
  sc_allow=$1
  sc_flag=$2
  sc_pattern=$3
  sc_kind=${4:-ml}
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
  # The `_build` prune, built positionally so it is EXACT.  `-name _build` at any
  # depth was too coarse twice over: `lib/nested/_build/evil.ml` was skipped even
  # though nothing puts a build tree there, and `ln -s /anywhere lib/_build`
  # slipped past [check_no_symlinks] as well.  Matching the full path pins it to
  # one directory per root, and `-type d` means a SYMLINK wearing that name is not
  # pruned — it falls through to the refusal instead.
  for sc_r in $SCAN_ROOTS; do
    set -- "$@" -path "$sc_r/_build" -type d -prune -o
  done
  case $sc_kind in
    dune) set -- "$@" -type f '(' -name 'dune' -o -name 'dune-project' ')' ;;
    *) set -- "$@" -type f '(' -name '*.ml' -o -name '*.mli' ')' ;;
  esac
  for sc_p in $sc_allow; do
    set -- "$@" '!' -path "$sc_p"
  done
  set +f
  # /dev/null in the grep argument list forces the filename prefix even when
  # `-exec ... +` hands grep a single file.
  #
  # A blanket `|| true` here would swallow grep's status, so an UNREADABLE file
  # containing a violation printed `Permission denied` on stderr and the run
  # ended "Policy OK" — an error mistaken for a clean result, which is the exact
  # thesis [check_no_symlinks] states 20 lines below and this function did not
  # apply to itself.  `-exec … +` cannot distinguish grep's 1 (no match, normal)
  # from its 2 (error) through find's status, so the errors are captured instead
  # and the caller fails on any of them.  An unreadable DIRECTORY was already
  # fail-closed, because find's own status is checked.
  # shellcheck disable=SC2086
  find $SCAN_ROOTS "$@" \
    -exec grep -n "$sc_flag" -e "$sc_pattern" /dev/null {} + 2>>"$SCAN_ERR_FILE" \
    || true
}

# `find -type f` never yields a symlink and `-exec grep` is therefore never
# handed one, so `ln -s ../test/bench_compare.ml lib/zz_link.ml` was invisible to
# BOTH guards, and `ln -s ../test lib/zz` hid a whole directory the same way
# (#604).  Refuse them rather than follow them:
#
#   - `find -L` would make the scanned set depend on link targets that may sit
#     outside the tree entirely, and reintroduces the traversal-loop question
#     that `-type f` currently sidesteps;
#   - nothing in the repository is a symlink — no tracked entry has mode 120000 —
#     so refusing costs nothing today and fails CLOSED tomorrow.
#
# If a legitimate source symlink is ever wanted, this is the place that has to be
# taught to follow it, which is the point: it becomes a decision instead of a
# hole.  `_build` is pruned first because dune leaves symlinked test directories
# there on any checkout that has run the suite.
check_no_symlinks() {
  # `set -e` is suspended inside a function invoked in a condition, so a failing
  # `find` would otherwise leave cns_found empty and report "no symlinks" — an
  # error mistaken for a clean result, which is fail-open #1's shape.  Check it.
  set --
  set -f
  for cns_r in $SCAN_ROOTS; do
    set -- "$@" -path "$cns_r/_build" -type d -prune -o
  done
  set +f
  # shellcheck disable=SC2086
  if ! cns_found=$(find $SCAN_ROOTS "$@" -type l -print); then
    echo "FAIL: the symlink scan itself failed; its 'no symlinks' answer cannot"
    echo "      be trusted, so this is a failure rather than a pass."
    return 1
  fi
  [ -n "$cns_found" ] || return 0
  echo "FAIL: symlink(s) under the scanned roots:"
  echo "$cns_found"
  echo
  echo "      This scan does not follow symlinks, so a symlinked source would"
  echo "      evade BOTH guards.  Remove the link, or teach"
  echo "      scripts/check-sqlite-policy.sh to follow it deliberately."
  return 1
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
  # Guard 4's allowlist (#621).  Optional so that the self-test cases which
  # predate it keep their two-argument form; unset means the EMPTY list, which
  # excludes nothing and therefore fails CLOSED — the same shape as fail-open 1
  # above, resolved the same way.
  dune_allow=${3:-}
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
  reject_glob_entries 'sqlite3-dune' "$dune_allow" || rc=1

  [ "$rc" -eq 0 ] || return 1

  # After the root-existence gate, so `find` is never pointed at a missing root.
  check_no_symlinks || rc=1

  # Where the scanners' stderr lands.  [scan_outside_allowlist] runs inside a
  # command substitution, so it cannot report an error back through a variable;
  # a file is what crosses that subshell boundary.
  SCAN_ERR_FILE=$(mktemp) || return 1

  cli_violations=$(scan_outside_allowlist "$cli_allow" -E '"sqlite3[ "]')
  if [ -n "$cli_violations" ]; then
    echo "FAIL: sqlite3 reference outside designated comparison files:"
    echo "$cli_violations"
    echo
    echo "If this is a designated comparison file, add it to"
    echo "SQLITE_CLI_ALLOWLIST in scripts/check-sqlite-policy.sh."
    rc=1
  fi

  mod_violations=$(scan_outside_allowlist "$mod_allow" -E "$SQLITE3_MODULE_PATTERN")
  if [ -n "$mod_violations" ]; then
    echo "FAIL: Sqlite3 module used outside the gated comparison benchmarks:"
    echo "$mod_violations"
    echo
    echo "This matches the whole word since #621, so 'open Sqlite3',"
    echo "'let open Sqlite3 in' and 'module S = Sqlite3' are caught as well as"
    echo "the qualified 'Sqlite3.' form.  If this is a designated comparison"
    echo "benchmark, add it to SQLITE3_MODULE_ALLOWLIST in"
    echo "scripts/check-sqlite-policy.sh; if the file only NAMES the module"
    echo "without linking it, that is SQLITE3_NAMES_ONLY_ALLOWLIST plus the"
    echo "marker at the site.  Shelling out to the CLI does NOT qualify — that"
    echo "belongs in SQLITE_CLI_ALLOWLIST only."
    rc=1
  fi

  # Guard 4: the build dependency itself (#621).
  dune_violations=$(
    scan_outside_allowlist "$dune_allow" -E "$SQLITE3_DUNE_PATTERN" dune
    # The root dune-project is outside every scan root; see DUNE_PROJECT_FILE.
    # `|| true` for grep's normal 1 (no match) only — its stderr is captured
    # into SCAN_ERR_FILE like the scanner's, so a read error still fails the run
    # rather than passing as "no match".
    if [ -f "$DUNE_PROJECT_FILE" ]; then
      grep -n -E -e "$SQLITE3_DUNE_PATTERN" /dev/null "$DUNE_PROJECT_FILE" \
        2>>"$SCAN_ERR_FILE" || true
    fi
  )
  if [ -n "$dune_violations" ]; then
    echo "FAIL: a build file declares a dependency on the sqlite3 library:"
    echo "$dune_violations"
    echo
    echo "'(libraries ... sqlite3 ...)' is what actually makes granary depend on"
    echo "real SQLite, which is the thing #370 exists to prevent.  The"
    echo "comparison benchmarks declare theirs in test/dune, which is"
    echo "allowlisted; nothing under lib/, bin/, containers/ or bench/, and not"
    echo "the root dune-project, may take that dependency.  See"
    echo "SQLITE3_DUNE_ALLOWLIST in scripts/check-sqlite-policy.sh."
    rc=1
  fi

  # Guard 3 has no allowlist: nobody assembles the name, anywhere.
  split_violations=$(scan_outside_allowlist "" -E "$SPLIT_LITERAL_PATTERN")
  if [ -n "$split_violations" ]; then
    echo "FAIL: the Sqlite3 module name is being assembled from pieces:"
    echo "$split_violations"
    echo
    echo "Splitting the literal dodges the guard above silently and records no"
    echo "reason anywhere the policy can see one.  If the file legitimately needs"
    echo "to NAME the module without linking it — a lint over the comparison"
    echo "benchmarks is the only current case — write the name whole and add the"
    echo "file to SQLITE3_NAMES_ONLY_ALLOWLIST with the marker at the site (#605)."
    rc=1
  fi

  if [ -s "$SCAN_ERR_FILE" ]; then
    echo "FAIL: the scanner itself reported errors, so its verdict on the files"
    echo "      it could not read is unknown — that is a failure, not a pass:"
    sed 's/^/      /' "$SCAN_ERR_FILE"
    rc=1
  fi
  rm -f "$SCAN_ERR_FILE"

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

# --- a names-only exemption must be visible at the site (#605) --------------
#
# A bare allowlist entry is a decision recorded in one file that the exempted
# file never mentions.  Requiring the marker in the file too means a reader of
# the file sees the exemption, and means the entry cannot be quietly reused for
# a file that actually links the module: the marker is a claim its author has to
# write down.

# The converse question matters just as much, and is what actually pins #605: an
# entry whose file NO LONGER matches the guard's pattern is a dead exemption.
# Splitting the literal again — `"Sqlite" ^ "3"` — would leave the entry in place
# and passing, so the evasion would be back with the allowlist as cover.  Here it
# fails and says so.
check_names_only_entries() {
  cnm_list=$1
  cnm_marker=$2
  cnm_pattern=$3
  cnm_bad=0
  set -f
  for cnm_p in $cnm_list; do
    # Missing files are the freshness check's job; do not double-report.
    [ -f "$cnm_p" ] || continue
    if ! grep -F -e "$cnm_marker" "$cnm_p" >/dev/null; then
      echo "FAIL: '$cnm_p' is allowlisted as naming the Sqlite3 module without"
      echo "      linking it, but does not carry the marker '$cnm_marker'."
      echo "      Put that marker in a comment at the site, or drop the entry"
      echo "      from SQLITE3_NAMES_ONLY_ALLOWLIST."
      cnm_bad=1
    fi
    # `-E`, not `-F`: since #621 the pattern is an ERE.  Matching it literally
    # would find nothing, every exemption would read as DEAD, and this check
    # would fail on the honest files — loud rather than fail-open, but wrong.
    if ! grep -E -e "$cnm_pattern" "$cnm_p" >/dev/null; then
      echo "FAIL: '$cnm_p' is allowlisted for naming the Sqlite3 module but does"
      echo "      not contain '$cnm_pattern' at all, so the exemption is dead."
      echo "      Remove it from SQLITE3_NAMES_ONLY_ALLOWLIST — and if the name"
      echo "      is being assembled from pieces to dodge this guard, don't: the"
      echo "      allowlist entry IS the supported way to name it (#605)."
      cnm_bad=1
    fi
  done
  set +f
  return $cnm_bad
}

# --- the keep-alive lint must cover every comparison benchmark (#601) -------
#
# [Tpc_keepalive_lint.comparison_sources] was a third independent copy of the
# comparison-file set, after test/dune's `(optional)` stanzas and the allowlists
# here.  The failure mode is the #505/#574 one exactly: an author adds a
# comparison bench, allowlists it here — the discoverable step, which is what
# #574 fixed — and the file is SILENTLY not linted, so it carries the #571
# use-after-free with nothing to catch it and crashes as a bare SIGSEGV that
# reads like a hung benchmark.
#
# SQLITE3_COMPARISON_ALLOWLIST is the source of truth: its members are exactly
# the files that touch the OCaml Sqlite3 module, which is exactly the set that
# needs the keep-alive guard.  Divergence in EITHER direction fails.

# The quoted basenames of `let comparison_sources = [ ... ]`, one per line.
# Reads across line breaks so that ocamlformat wrapping the list does not
# silently empty this.  If the binding is renamed or restructured the output is
# empty, and the caller treats that as a failure — fail closed, not open.
lint_comparison_sources() {
  awk '
    /^let comparison_sources =/ { inlist = 1 }
    inlist {
      rest = $0
      while (match(rest, /"[^"]*"/)) {
        print substr(rest, RSTART + 1, RLENGTH - 2)
        rest = substr(rest, RSTART + RLENGTH)
      }
      if (index($0, "]") > 0) exit
    }
  ' "$1"
}

check_lint_coverage() {
  clc_allow=$1
  clc_lint=$2
  clc_rc=0

  if [ ! -f "$clc_lint" ]; then
    echo "FAIL: the keep-alive lint '$clc_lint' does not exist, so nothing"
    echo "      cross-checks the comparison allowlist against it (#601)."
    return 1
  fi

  clc_have=$(lint_comparison_sources "$clc_lint" | sort)
  if [ -z "$clc_have" ]; then
    echo "FAIL: could not read 'let comparison_sources = [ ... ]' out of"
    echo "      '$clc_lint'.  If it was renamed, update lint_comparison_sources"
    echo "      in scripts/check-sqlite-policy.sh — an unreadable list would"
    echo "      otherwise make this cross-check vacuously true."
    return 1
  fi

  # The lint holds basenames (a test stanza's cwd is its own directory); the
  # allowlist holds repo-relative paths.  Compare on basenames.
  clc_want=$(
    set -f
    for clc_p in $clc_allow; do
      basename "$clc_p"
    done | sort
  )

  set -f
  for clc_f in $clc_want; do
    if ! printf '%s\n' "$clc_have" | grep -Fxq -e "$clc_f"; then
      echo "FAIL: '$clc_f' is in SQLITE3_COMPARISON_ALLOWLIST but not in"
      echo "      Tpc_keepalive_lint.comparison_sources, so it links the sqlite3"
      echo "      bindings with nothing checking its keep-alives (#571, #601)."
      clc_rc=1
    fi
  done
  for clc_f in $clc_have; do
    if ! printf '%s\n' "$clc_want" | grep -Fxq -e "$clc_f"; then
      echo "FAIL: Tpc_keepalive_lint.comparison_sources names '$clc_f', which is"
      echo "      not in SQLITE3_COMPARISON_ALLOWLIST.  Either it no longer links"
      echo "      the bindings (drop it from the lint) or this allowlist is"
      echo "      missing it (#601)."
      clc_rc=1
    fi
  done
  set +f

  return $clc_rc
}

# --- self-test -------------------------------------------------------------
#
# Builds a throwaway tree, plants violations in it, and asserts that each guard
# FAILS where it must and PASSES where it must.  Runs on every invocation,
# because otherwise these two guards are only ever observed passing — and the
# two defects above were both "reviewable-only": no test would have surfaced
# either.  #591's shellcheck step would not have either; both defects are
# semantic (an empty pattern file means "match everything" to grep), which is
# exactly the class a syntactic linter cannot see.

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

  mkdir -p "$root/lib" "$root/bin/repl" "$root/containers" "$root/test" "$root/bench"
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
  st_cmp='
test/bench_compare.ml
test/bench_tpch.ml
test/bench_tpcc.ml
'
  # Guard 2 consumes the two sub-lists assembled, exactly as main does.
  st_mod="$st_cmp
test/tpc/tpc_keepalive_lint.ml
test/tpc/tpc_keepalive_lint.mli
test/test_sqlite_keepalive_571.ml
"
  # Guard 4's allowlist (#621).
  st_dune='
test/dune
test/tpc/dune
'

  report() {
    if [ "$2" = "$1" ]; then
      [ "$verbose" = 0 ] || echo "  ok   (expected $1) $3"
    else
      echo "  BROKEN: expected $1, got $2 — $3"
      st_fail=1
    fi
  }

  # $5 is guard 4's allowlist and is optional, so every case written before
  # #621 keeps its four-argument form and gets the empty (excludes-nothing)
  # list.
  expect() {
    if (
      cd "$root"
      run_guards "$3" "$4" "${5:-}"
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

  # A stand-in for the keep-alive lint and its .mli: names the module as data,
  # links nothing, carries the marker.
  mkdir -p "$root/test/tpc"
  st_lint="$root/test/tpc/tpc_keepalive_lint.ml"
  write_lint() {
    printf '(* %s: scans for Sqlite3.finalize as text *)\n' "$NAMES_ONLY_MARKER" \
      >"$st_lint"
    printf 'let comparison_sources = [ %s ]\n' "$1" >>"$st_lint"
  }
  write_lint '"bench_tpcc.ml"; "bench_tpch.ml"; "bench_compare.ml"'
  printf '(* %s *)\nval x : Sqlite3.db\n' "$NAMES_ONLY_MARKER" \
    >"$root/test/tpc/tpc_keepalive_lint.mli"
  # The lint's TEST names the module as a bare word — it pins the lint's own
  # binding line as source text — which only the widened #621 pattern sees.  A
  # stand-in for it, so the clean-tree case exercises a names-only file that has
  # NO dot after the name.
  printf '(* %s *)\nlet expected = "Sqlite3"\n' "$NAMES_ONLY_MARKER" \
    >"$root/test/test_sqlite_keepalive_571.ml"

  st_names_only='
test/tpc/tpc_keepalive_lint.ml
test/tpc/tpc_keepalive_lint.mli
test/test_sqlite_keepalive_571.ml
'

  expect_marker() {
    if (
      cd "$root"
      check_names_only_entries "$3" "$NAMES_ONLY_MARKER" "$SQLITE3_MODULE_PATTERN"
    ) >/dev/null 2>&1; then
      report "$1" pass "$2"
    else
      report "$1" fail "$2"
    fi
  }

  expect_lint_cover() {
    if (
      cd "$root"
      check_lint_coverage "$3" test/tpc/tpc_keepalive_lint.ml
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

  # --- #604: the three blind spots -------------------------------------
  #
  # Each of these passed clean before #604 and each must now FAIL.  They are
  # placed next to the suffix/scan-root cases above because that is the mutation
  # they guard: dropping `bench` from SCAN_ROOTS, or the `-name '*.mli'` arm, or
  # the symlink refusal, must each turn exactly one of these red.

  printf 'let _ = Sqlite3.db_open "x"\n' >"$root/bench/zz_probe.ml"
  expect fail "module guard reaches bench/ (#604)" "$st_cli" "$st_mod"
  rm -f "$root/bench/zz_probe.ml"

  printf 'let _ = failwith "sqlite3 not found"\n' >"$root/bench/zz_probe.ml"
  expect fail "CLI guard reaches bench/ (#604)" "$st_cli" "$st_mod"
  rm -f "$root/bench/zz_probe.ml"

  # An interface exposing the binding's types forces the dependency on every
  # consumer, so it is a violation with the same consequence as an
  # implementation using it.
  printf 'val x : Sqlite3.db\n' >"$root/lib/zz_iface.mli"
  expect fail "module guard reaches .mli files (#604)" "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_iface.mli"

  printf 'val x : string\n(* raises with "sqlite3 not found" *)\n' >"$root/lib/zz_iface.mli"
  expect fail "CLI guard reaches .mli files (#604)" "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_iface.mli"

  # Widening the suffix filter must not widen it to everything: `.mll`, `.mly`
  # and the .md fixture above are not scanned.  Without this, `-name '*.ml*'`
  # — the obvious way to add .mli — passes every case here.
  printf 'let _ = Sqlite3.db_open "x"\nlet _ = failwith "sqlite3 not found"\n' \
    >"$root/lib/zz_lexer.mll"
  expect pass "a .mll file carrying BOTH patterns is not a violation" \
    "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_lexer.mll"

  # `find -type f` never yields a symlink and `grep` is never handed one, so a
  # link into an allowlisted file was invisible to both guards.
  ln -s ../test/bench_compare.ml "$root/lib/zz_link.ml"
  expect fail "a symlinked source is refused, not skipped (#604)" "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_link.ml"

  ln -s ../test "$root/lib/zz_dirlink"
  expect fail "a symlinked DIRECTORY is refused, not skipped (#604)" "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_dirlink"

  # ...but a build tree is not source, and dune leaves symlinks in one.  Pruning
  # `_build` is what keeps the refusal from firing on any developer checkout
  # that has run the suite.
  mkdir -p "$root/test/_build/_tests"
  ln -s . "$root/test/_build/_tests/latest"
  printf 'let _ = Sqlite3.db_open "x"\n' >"$root/test/_build/zz_stale.ml"
  expect pass "_build is pruned: neither its symlinks nor its sources count" \
    "$st_cli" "$st_mod"
  rm -rf "$root/test/_build"

  # A build tree pruned by NAME at any depth was two holes: a source tree that
  # merely contains a `_build` component was skipped, and a symlink wearing the
  # name evaded the refusal.  The prune is by exact path AND `-type d`.
  mkdir -p "$root/lib/nested/_build"
  printf 'let _ = Sqlite3.db_open "x"\n' >"$root/lib/nested/_build/zz_evil.ml"
  expect fail "a _build BELOW a scan root is not pruned" "$st_cli" "$st_mod"
  rm -rf "$root/lib/nested"

  ln -s /tmp "$root/lib/_build"
  expect fail "a SYMLINK named _build is refused, not pruned" "$st_cli" "$st_mod"
  rm -f "$root/lib/_build"

  # An unreadable file is a file the scanner did not read.  Reporting "Policy OK"
  # for it is an error mistaken for a clean result — the very thing the symlink
  # refusal exists to prevent, applied to the scanner itself.
  #
  # UNDER ROOT THIS CASE CANNOT RUN, and that is a limit of the fixture, not of
  # the guard: CAP_DAC_OVERRIDE means a mode-000 file is still readable, and POSIX
  # offers no other cheap way to make a regular file unreadable to root.  The
  # guard itself is live in the real scan either way; only its pin is skipped, so
  # a mutation removing the guard would go unnoticed on a root runner (CI's Alpine
  # runner is one).  Closing that needs the self-test to drop privileges, which
  # needs a re-exec entry point this script does not have — dropping it silently
  # would be worse than skipping loudly, so it skips loudly.
  printf 'let _ = Sqlite3.db_open "x"\n' >"$root/lib/zz_unreadable.ml"
  chmod 000 "$root/lib/zz_unreadable.ml"
  if [ -r "$root/lib/zz_unreadable.ml" ]; then
    echo "  SKIP 'an unreadable file fails closed' — running as root, so mode 000 is"
    echo "       still readable.  The guard runs in the real scan; only its pin does not."
  else
    expect fail "an unreadable file fails closed, not open" "$st_cli" "$st_mod"
  fi
  chmod 644 "$root/lib/zz_unreadable.ml"
  rm -f "$root/lib/zz_unreadable.ml"

  # --- #621: the module guard matches the WHOLE WORD --------------------
  #
  # Every one of these printed "Policy OK" against the fixed-string `Sqlite3.`
  # pattern, and none of them is an evasion — they are the normal ways to use an
  # OCaml module.  Reverting the pattern to the old fixed string turns all four
  # green, which is the mutation these cases exist to catch.

  printf 'open Sqlite3\nlet _ = db_open "x"\n' >"$root/lib/zz_probe.ml"
  expect fail "module guard catches 'open Sqlite3' (#621)" "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_probe.ml"

  printf 'let f () = let open Sqlite3 in db_open "x"\n' >"$root/lib/zz_probe.ml"
  expect fail "module guard catches a local 'let open Sqlite3 in' (#621)" \
    "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_probe.ml"

  printf 'module S = Sqlite3\nlet _ = S.db_open "x"\n' >"$root/lib/zz_probe.ml"
  expect fail "module guard catches a 'module S = Sqlite3' alias (#621)" \
    "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_probe.ml"

  printf 'let _ = Sqlite3 . db_open "x"\n' >"$root/lib/zz_probe.ml"
  expect fail "module guard catches a space before the dot (#621)" "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_probe.ml"

  # An .mli aliasing the module is the same build dependency as an .ml doing it.
  printf 'module S = Sqlite3\n' >"$root/lib/zz_iface.mli"
  expect fail "module guard catches an alias in an .mli (#621)" "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_iface.mli"

  # ...and the other direction, which is what stops the widened pattern being
  # "just make it a substring".  A LONGER identifier merely containing the name
  # is a different module and is not a violation.
  printf 'let _ = Sqlite3_ext.open_ "x"\nlet _ = MySqlite3.db_open "x"\n' \
    >"$root/lib/zz_probe.ml"
  expect pass "a longer identifier containing the name is not a violation (#621)" \
    "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_probe.ml"

  # A names-only file whose only mention has NO dot after it — the shape that
  # brought test_sqlite_keepalive_571.ml into the allowlist.  Drop JUST that
  # entry and the widened guard fires on it, which is both why the entry exists
  # and proof that the widening reaches a bare mention at all.
  expect fail "a names-only file with no dot needs its entry under the wider pattern" \
    "$st_cli" "$st_cmp
test/tpc/tpc_keepalive_lint.ml
test/tpc/tpc_keepalive_lint.mli
"

  # --- #621: guard 4, the build dependency itself -----------------------
  #
  # No dune file was scanned at all before this, so each of these passed clean
  # while declaring exactly the dependency the policy exists to prevent.

  printf '(library\n (name zz)\n (libraries granary sqlite3))\n' >"$root/lib/dune"
  expect fail "dune guard catches '(libraries ... sqlite3)' in lib/ (#621)" \
    "$st_cli" "$st_mod" "$st_dune"
  rm -f "$root/lib/dune"

  printf '(executable\n (name zz)\n (libraries granary sqlite3))\n' >"$root/bench/dune"
  expect fail "dune guard reaches bench/ (#621)" "$st_cli" "$st_mod" "$st_dune"
  rm -f "$root/bench/dune"

  # test/dune is where the comparison benchmarks declare theirs; allowlisted.
  printf '(tests\n (names bench_compare)\n (libraries granary sqlite3))\n' \
    >"$root/test/dune"
  expect pass "the allowlisted test/dune may declare the dependency" \
    "$st_cli" "$st_mod" "$st_dune"
  # ...and an EMPTY guard-4 allowlist must allow nothing rather than everything,
  # the same fail-open 1 shape as the other two lists.
  expect fail "dune guard still fires when ITS allowlist is EMPTY" \
    "$st_cli" "$st_mod" ""
  rm -f "$root/test/dune"

  # The boundary again, on the lowercase pattern: an executable NAME containing
  # the library name is not a dependency on it.  test/dune really does have one
  # (bench_perf_compare_sqlite3), so a substring match would need that file
  # allowlisted for the wrong reason.
  printf '(executable\n (name zz_probe_sqlite3)\n (libraries granary))\n' \
    >"$root/lib/dune"
  expect pass "an executable name containing the library name is not a dependency" \
    "$st_cli" "$st_mod" "$st_dune"
  rm -f "$root/lib/dune"

  # The root dune-project sits outside every scan root, so `find` never reaches
  # it; it is scanned by name and has NO allowlist.  A dependency there is on
  # the granary package itself.
  printf '(lang dune 3.0)\n(package (name granary) (depends lwt sqlite3))\n' \
    >"$root/dune-project"
  expect fail "dune guard reaches the root dune-project (#621)" \
    "$st_cli" "$st_mod" "$st_dune"
  printf '(lang dune 3.0)\n(package (name granary) (depends lwt))\n' \
    >"$root/dune-project"
  expect pass "a clean root dune-project is not a violation" \
    "$st_cli" "$st_mod" "$st_dune"
  rm -f "$root/dune-project"

  # --- #605: the split literal itself, tree-wide ------------------------
  #
  # THIS is the case the first draft of this PR lacked, and its fixture is
  # deliberately shaped like the real file: prose that names the module — which
  # is what keeps the dead-exemption check satisfied — PLUS the split literal.
  # A fixture containing only the split literal passes whether or not the guard
  # exists, which is how the evasion shipped green.
  cp "$st_lint" "$root/zz_lint_backup"
  printf '(* %s: scans for Sqlite3.finalize as text *)\nlet binding_module = "Sqlite" ^ "3"\nlet comparison_sources = [ "bench_tpcc.ml"; "bench_tpch.ml"; "bench_compare.ml" ]\n' \
    "$NAMES_ONLY_MARKER" >"$st_lint"
  expect fail "an ALLOWLISTED file may not split the module name (#605)" \
    "$st_cli" "$st_mod"
  # ...and a brand-new file may not either — the residual a state-only pin on the
  # current file cannot reach.
  cp "$root/zz_lint_backup" "$st_lint"
  printf 'let m = "Sq" ^ "lite3"\n' >"$root/lib/zz_split.ml"
  expect fail "a NEW file may not split the module name either (#605)" \
    "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_split.ml"
  printf 'let y = prefix ^ "te3"\n' >"$root/lib/zz_split.ml"
  expect fail "a suffix-piece split is caught too" "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_split.ml"
  # The binding forms the first anchor missed.  A type annotation is ordinary
  # OCaml, not an evasion technique, so missing it was a real hole rather than an
  # accepted limit.
  printf 'let m : string = "Sqlite" ^ "3"\n' >"$root/lib/zz_split.ml"
  expect fail "an ANNOTATED binding is caught" "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_split.ml"
  printf 'let a = 1\nand m = "Sqlite" ^ "3"\n' >"$root/lib/zz_split.ml"
  expect fail "an 'and' binding is caught" "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_split.ml"
  # KNOWN GAP, pinned so the claim in the comment above is executable rather than
  # only asserted: a LINE BREAK between the binding and the concatenation defeats
  # a line-oriented grep, and ocamlformat would introduce one by itself if the
  # binding ever grew past the margin.  If a future change closes this, THIS CASE
  # GOES RED — at which point delete it and the caveat together.  It is `pass`
  # because the guard genuinely does not fire, not because the file is innocent.
  printf 'let m =\n  "Sqlite" ^ "3"\n' >"$root/lib/zz_split.ml"
  expect pass "KNOWN GAP: a line break defeats guard 3 (see SPLIT_LITERAL_PATTERN)" \
    "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_split.ml"
  # PROSE about the evasion is not the evasion.  Two files in the tree discuss
  # it, and a guard that cannot tell them apart would have to be disarmed.
  printf '(* the name used to be built as "Sqlite" ^ "3" — see #605 *)\nlet x = 1\n' \
    >"$root/lib/zz_prose.ml"
  expect pass "prose ABOUT the split is not a violation" "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_prose.ml"
  # Ordinary concatenation is not the evasion either.
  printf 'let greeting = "hello" ^ "world"\nlet parts = String.concat "" [ "a"; "b" ]\n' \
    >"$root/lib/zz_concat.ml"
  expect pass "an unrelated concatenation is not a violation" "$st_cli" "$st_mod"
  rm -f "$root/lib/zz_concat.ml"
  rm -f "$root/zz_lint_backup"

  # --- #605: a names-only exemption must be visible at the site ---------

  expect_marker pass "names-only: the marker is present in both files" \
    "$st_names_only"
  # Without the marker the entry is a bare exemption nobody reading the file can
  # see — the same invisibility the split literal had.
  printf 'let comparison_sources = [ "bench_tpcc.ml" ]\n(* Sqlite3.finalize *)\n' \
    >"$st_lint"
  expect_marker fail "names-only: a missing marker is reported" "$st_names_only"
  # Re-splitting the literal must not hide behind the exemption: the entry is
  # then dead and says so, instead of passing.
  printf '(* %s *)\nlet m = "Sqlite" ^ "3"\n' "$NAMES_ONLY_MARKER" >"$st_lint"
  expect_marker fail "names-only: a dead exemption (name no longer present) fails" \
    "$st_names_only"
  write_lint '"bench_tpcc.ml"; "bench_tpch.ml"; "bench_compare.ml"'

  # --- #601: the keep-alive lint must cover every comparison benchmark ---

  expect_lint_cover pass "lint coverage: the two lists agree" "$st_cmp"
  # The live failure: a bench allowlisted here and not linted there.
  expect_lint_cover fail "lint coverage: an allowlisted bench that is not linted" \
    "$st_cmp
test/bench_tpcds.ml"
  # ...and the reverse, which means the lint is reading a file that no longer
  # links the bindings, i.e. its subject moved without it.
  write_lint '"bench_tpcc.ml"; "bench_tpch.ml"; "bench_compare.ml"; "bench_gone.ml"'
  expect_lint_cover fail "lint coverage: a linted file that is not allowlisted" \
    "$st_cmp"
  # An EMPTY comparison allowlist must not make the cross-check vacuous, for the
  # same reason an empty guard allowlist must not disable its guard (#574's
  # fail-open 1): the three files the lint still names are then uncovered by the
  # policy, which is a divergence like any other.
  expect_lint_cover fail "lint coverage: an EMPTY comparison allowlist is not vacuous" \
    ""
  # A list ocamlformat has wrapped across lines must still be read, or the
  # cross-check silently empties the day the list grows.
  printf '(* %s *)\nlet comparison_sources =\n  [ "bench_tpcc.ml"\n  ; "bench_tpch.ml"\n  ; "bench_compare.ml"\n  ]\n' \
    "$NAMES_ONLY_MARKER" >"$st_lint"
  expect_lint_cover pass "lint coverage: a wrapped list is still read" "$st_cmp"
  # An unreadable list must FAIL, not pass vacuously: with no entries extracted,
  # every "is it linted?" question would answer yes by comparing nothing.
  printf '(* %s *)\nlet sources_for_the_lint = [ "bench_tpcc.ml" ]\n' \
    "$NAMES_ONLY_MARKER" >"$st_lint"
  expect_lint_cover fail "lint coverage: an unreadable list fails, not passes" \
    "$st_cmp"
  # Both halves empty is the ONE combination where every comparison above is
  # vacuously satisfied, so it is the only case that isolates the [-z] check.
  # It is reachable: retiring the comparison benchmarks empties the allowlist by
  # design, and a rename empties the extraction.
  expect_lint_cover fail "lint coverage: empty list AND empty allowlist still fails" \
    ""
  write_lint '"bench_tpcc.ml"; "bench_tpch.ml"; "bench_compare.ml"'

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
check_allowlist_fresh 'sqlite3-dune' "$SQLITE3_DUNE_ALLOWLIST" || fresh=1
[ "$fresh" -eq 0 ] || exit 1

check_names_only_entries \
  "$SQLITE3_NAMES_ONLY_ALLOWLIST" "$NAMES_ONLY_MARKER" "$SQLITE3_MODULE_PATTERN" || exit 1
check_lint_coverage "$SQLITE3_COMPARISON_ALLOWLIST" "$KEEPALIVE_LINT_SOURCE" || exit 1

run_guards "$SQLITE_CLI_ALLOWLIST" "$SQLITE3_MODULE_ALLOWLIST" \
  "$SQLITE3_DUNE_ALLOWLIST" || exit 1

echo "Policy OK: all sqlite3 references confined to gated comparison files"
