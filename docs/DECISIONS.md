# Decision log

This is the full rationale, mechanism, and pinning-test detail behind the
one-line entries in `CLAUDE.md`'s "Decisions and divergences" index. It was
split out of `CLAUDE.md` on 2026-09-05 purely for size (`CLAUDE.md` had grown
past 225 KB); nothing here is less authoritative than it was as part of that
file, and nothing was reworded — this is a straight relocation.

Read `CLAUDE.md` first for the index and the one-line summary of each item,
then come here for the "why" and the "how it's enforced" behind any specific
one. Entries are in the order they were decided, which is not necessarily a
useful reading order — search for the issue number instead.

- **NaN is a value here, and it sorts below every number (#536, decided 2026-08-02).**
  SQLite has no NaN at all: `sqlite3_bind_double(NaN)` binds NULL, and a NaN
  expression result is NULL, so `o >= ?` bound to NaN is *unknown* and returns
  no rows. Granary instead keeps NaN as a real value in a total order, and
  diverges from SQLite in **both** directions — `o >= NaN` returns every row,
  `o <= NaN` returns none. Two mechanisms have to agree for that to be sound:
  - `Exec.cmp_result`, the WHERE-predicate comparator, orders NaN below every
    number. It used to get that from `Float.compare`'s total order, which puts
    NaN below `neg_infinity`; since #733 it delegates to `Exec.compare_values`,
    whose `cmp_int_real` states the rule explicitly instead. Same answer, now by
    construction rather than by coincidence. Since #738 `=` and `<>` go through
    it too, so `1 <> NaN` is true (it was false, sharing the cross-numeric
    catch-all) and no comparison operator is outside the rule.
  - `Index_key.encode_value` gives NaN its own single-byte tag `0x01` — below
    INTEGER's `0x02` and REAL's `0x03`, above NULL's `0x00` — so it also sorts
    below every other number, and *distinctly from* NULL.

  **They agree that NaN sorts below every number, and that — not blanket
  agreement — is what keeps this from being a rows-lost bug.** They now also
  agree about NaN vs NULL: `0x01` was #578's fix, and before it NaN and NULL
  shared `0x00` and were byte-identical. A seek and its residual predicate can never disagree about where
  NaN sits relative to a number, so no future change may move one of the two
  without the other, or an index seek will start skipping rows the predicate
  would have kept. Option A (fold NaN to NULL at the value-ingress points,
  matching SQLite) remains the only variant worth the disruption; option B
  (NULL comparison, unchanged storage) was rejected precisely because it breaks
  that agreement.

  `nan_and_infinite_bounds_are_sound` in `test/test_range_bound_517.ml` pins it
  (300 rows for a NaN lower bound, 0 for an upper one).

  **The agreement is only about NaN vs *numbers*.** The rest of the engine was
  surveyed when #536 was decided, and NaN vs *NULL* is where the two levels part
  company — `Exec.compare_values` puts `NULL < NaN < every number`, while the
  index encoding makes NaN and NULL the same `0x00` byte, i.e. *equal*. The
  value-level answers are all coherent with the decided order and need no
  change:
  - **ORDER BY** (`compare_with_nulls`) places NULLs by an explicit flag and
    sends NaN to `compare_values`, so NaN sorts after NULLs and before every
    other real.
  - **DISTINCT** and hash joins dedup on `row_key`'s `%h` rendering, so all NaNs
    collapse to one and none collapses into NULL.
  - **GROUP BY** / window PARTITION BY group on `compare_values = 0`, so two
    NaNs group together and NaN never groups with NULL — consistent with
    DISTINCT.
  - **MIN/MAX** skip NULLs and use `compare_values`, so `MIN` over a REAL column
    containing NaN returns NaN and `MAX` only does when NaN is the sole
    non-NULL value.

  **Index-keyed uniqueness used to be the one that did NOT agree — #578, since
  FIXED (`d296baf`).** The UNIQUE NULL exemption (`any_null_val`) tests the
  *value*, so a NaN is correctly not exempted; the conflict probe then compares
  *encoded bytes*, and while NaN's key was byte-identical to NULL's,
  `INSERT NULL` then `INSERT NaN` raised a spurious `UNIQUE constraint failed`
  while the reverse order succeeded. The fix was the one this paragraph
  predicted would be the good one: NaN got its own tag byte (`0x01`,
  `lib/encoding/index_key.ml:117-126`). Note what was NOT done, and must not
  be: making the exemption byte-based would have silently exempted NaN from
  uniqueness altogether. **Option C is correspondingly stronger, not weaker** —
  the value level and the key level now agree about NaN's position exactly,
  rather than only about "both below every number".

  **#579 (fixed): `Exec.compare_values` is now a TOTAL order.** It used to end
  in `| _, _ -> 0  (* cross-type: shouldn't happen *)`, and it does happen:
  strict column typing keeps a *stored* column single-typed, but a *computed*
  one is unconstrained per row, so `CASE WHEN i = 0 THEN f ELSE i END` mixes
  INTEGERs and REALs freely. Every such pair compared **equal**, making the
  relation **non-transitive** (`1 = 2.5`, `2.5 = 3`, but `1 < 3`) — and
  `List.sort` on a non-transitive comparator has no defined result, so
  `ORDER BY` over such a column returned rows in scan order, unsorted, with no
  error. The same comparator is behind GROUP BY (which sorts and then groups
  adjacent runs, so *which* rows landed in *which* group was input-order-
  dependent), window PARTITION BY, and MIN/MAX (which became first-wins).

  Two rules: within the numeric class compare **exactly** via `cmp_int_real`;
  across classes order NULL < number < TEXT < BLOB via `value_class_rank`. That
  order is SQLite's documented storage-class order *and* the order of
  `Index_key.encode_value`'s tag bytes, so the value level and the index level
  cannot disagree about which **class** sorts first. They still disagree about
  INTEGER vs REAL *within* the numeric class — the encoding gives them separate
  tags (`0x02`, `0x03`) and so puts every integer before every real — but that
  is pre-existing.

  **The exactness is the part that is easy to get wrong, and the first
  revision of the fix did.** Promoting through `Int64.to_float` rounds, so
  above 2^53 two distinct int64s promote to the same float and the comparator
  is *still* non-transitive — `9007199254740993 ≡ 9007199254740992.0` and
  `9007199254740992.0 ≡ 9007199254740992` while `9007199254740993 >
  9007199254740992`. That is #579's own defect one magnitude up, and the
  issue's repro reproduces verbatim with large values. `cmp_int_real` compares
  without converting (sign and range first, then `Int64.compare` against the
  truncated float, then the fraction as tiebreak), which is also what SQLite
  does (`sqlite3IntFloatCompare`). **`compare_values` is a total order for
  every input, with no magnitude caveat** — but that claim is about that
  function, not about the engine.

  **`cmp_result`, the WHERE-predicate comparator, IS that function now, and
  since #738 it is the comparator behind ALL SIX comparison operators — #733,
  #734 and #738 (all fixed, 2026-09-03).** For one release `cmp_result` was a
  third comparator differing in two filed ways, and the fix was to delete both
  differences rather than to reconcile them:
  - it promoted int-vs-real through `Int64.to_float`, so above 2^53 a
    *predicate* answered equal for a pair the *ordering* separated
    (**#733**);
  - it ended in `| _ -> Row.V_int 0L`, so every cross-class predicate was
    false — it applied no class order at all. After #579, `ORDER BY` said
    `5 < 'abc'` while `WHERE` said that was false. sqlite3 answers `1`, so
    `cmp_result` was the wrong half (**#734**).

  `cmp_result` is now `compare_values` with three-valued logic layered on top:

  ```ocaml
  and cmp_result lv rv pred =
    match lv, rv with
    | Row.V_null, _ | _, Row.V_null -> Row.V_null
    | _, _ -> if pred (compare_values lv rv) then Row.V_int 1L else Row.V_int 0L
  ```

  **The NULL arm must stay above the delegation.** `compare_values` *orders*
  NULL below everything, because a total order has to answer something; a
  predicate over a NULL is UNKNOWN. Routing NULL through it would make
  `WHERE x < 5` true for a NULL `x`.

  **`=` and `<>` are routed through it too, and that took `eval_binop` plus
  THREE index sites moving in one commit (#738, fixed 2026-09-03).** They used
  to keep their own arms in `eval_binop`, each enumerating the four same-type
  pairs and falling to a catch-all, so a cross-NUMERIC pair answered false in
  *both* operators:
  `1 = 1.0` was `0` **and** `1 <> 1.0` was `0`, while `1 <= 1.0` and
  `1 >= 1.0` were both true. That is incoherent independently of sqlite3, which
  answers `1|0|1` for `1 = 1.0, 1 <> 1.0, 1 <> 2.0` (oracle-checked). Both arms
  are now one line each — `cmp_result lv rv (fun c -> c = 0)` and
  `(fun c -> c <> 0)` — so no comparator survives that can answer `=` one way
  and `<=`/`>=` another. Cross-class is unchanged (never equal, therefore
  always different, #734's answer), and the exactness comes from
  `cmp_int_real`, so `9007199254740993 = 9007199254740992.0` is still **false**
  — the fix must never be spelled as an `Int64.to_float` promotion, which would
  be #733 reintroduced inside `=`.

  **The three index sites had to move with it, and must never be moved back
  apart.** This is the reason #738 could not ride along with #733/#734. Unlike
  a range conjunct, an **equality** conjunct *is* consumed by the access path
  (`Planner.recognise_eq_col_lit` → `access_path_for_eqs`, whose `consumed`
  positions `residual_filter` removes), so **no residual re-checks the rows a
  seek yields** — the seek *is* the answer. Making `=` exact while those sites
  still declined a cross-numeric probe would have made `WHERE i = 1.0` on an
  indexed INTEGER column return nothing while the predicate says the row
  qualifies: rows lost silently, the exact failure mode the NaN section's "a
  seek and its residual may never disagree" rule exists to prevent. The three
  index sites, all of them — the issue text named only the first two:

  - `Exec.index_lookup_values` — a REAL probe on an INTEGER column becomes
    `IK_int` when the float names an integer exactly, and a `V_int` probe on a
    REAL column becomes `IK_real` only when `Int64.to_float` round-trips
    exactly. Otherwise `None`, which still means *matches nothing* and is
    *exactly right*: no integer equals `1.5`, and no double equals `2^53 + 1`.
    Both directions go through the two shared helpers `Exec.int64_of_exact_real`
    and `Exec.exact_real_of_int64`, the latter implemented by *asking*
    `cmp_int_real`, so the key and the predicate agree by construction rather
    than by two hand-written range checks.
  - `Exec.stream_rowid_lookup` — the rowid-alias read path, which returned an
    empty stream for anything but `V_int`.
  - `Exec.seek_candidates`'s `Seek_rowid` arm — **the one #738's own issue text
    does not list.** The DML seek is only a *restriction* (the
    write path re-evaluates the whole predicate on every candidate), but a
    declined probe drops the row before the predicate ever sees it, so
    `DELETE FROM p WHERE id = 2.0` was a silent no-op. Both rowid sites now go
    through the shared `Exec.rowid_lookup_key`.

  `Exec.range_bound_key` is untouched and that is not luck: it intercepts both
  numeric cross-type pairs in its own arms *above* the delegation to
  `index_lookup_values`, so the widened bound it computes is unaffected by the
  equality path learning them.

  **NaN never reaches a seek as an "integral" real**, and the naive
  integrality test would have let it: `Float.trunc nan` is `nan` and
  `Float.equal nan nan` is true, so `Int64.of_float` would have been called
  outside its specified domain. `int64_of_exact_real` declines NaN, every
  infinity and everything outside int64 range *first*. At the value level
  `NaN = NaN` is still true and `NaN <> NaN` still false (`Float.compare nan
  nan = 0`, which is what `Float.equal` already was); the one NaN answer that
  **moved** is `1 <> NaN`, from false to true — it was sharing the
  cross-numeric catch-all, i.e. the same incoherence as `1 <> 1.0`, fixed by
  the same change rather than by a NaN rule. #536's decided order is unchanged.

  **A JOIN KEY equality is consumed too, and #738 left both of its executors
  behind — #743, fixed 2026-09-03.** `Planner.recognise_eq_col_col` picks the
  key and nothing re-applies the ON predicate, so the key *is* the match test,
  exactly as a seek is. The keyed `Exec.stream_hash_join` arm hashed both sides
  on `Index_key.encode_value (row_value_to_index_value v)` and
  `Exec.nlj_probe_left` encoded its probe the same way, so
  `FROM l JOIN r ON l.a = r.b` (INTEGER vs REAL) answered no rows while
  `ON 1 = 1 WHERE l.a = r.b` answered the row; sqlite3 answers the row for
  **both** (oracle-checked). Both spellings answered *no rows* before #738, so
  no answer regressed — what was new is that they disagreed with each other.

  **The two executors had to move in one change**, because which one runs
  depends on whether the right table has an index the probe can use: fixing one
  alone would have made the answer depend on whether a `CREATE INDEX` exists,
  the #639/#589 failure mode. They moved by two *different* mechanisms, and the
  asymmetry is the thing to understand before editing either:

  - **The hash arm has no index and no column type to translate towards** —
    both sides are just row values — so it keys on `Exec.join_key_value`, a
    CANONICAL key: an integral REAL keys as the INTEGER it names, via the same
    `Exec.int64_of_exact_real` the index sites use. That makes canonical-key
    equality **exactly** `compare_values … = 0` on non-NULL values, which is
    provable rather than approximate: a non-integral REAL keeps REAL's tag
    `0x03` and can never collide with an INTEGER's `0x02`; above 2^53 the
    directions stay apart because the helper is exact and not an
    `Int64.to_float` promotion (that would be #733 inside a join key); NaN is
    declined by that helper *first*, so it stays `IK_real nan` whose encoding
    is the single byte `0x01` (#578) — all NaNs share one bucket, matching
    `Float.compare nan nan = 0`, and none collides with a number or with NULL's
    `0x00`. **No residual re-check is needed and none was added**, precisely
    because the key is exact; if a future change makes the key an
    over-approximation it owes a `compare_values` re-check on each candidate
    pair.
  - **The nested-loop arm seeks a typed index**, so `Plan.probe_part` now
    carries the declared type of the index column each part pins — the way
    `Op_index_lookup`'s `keys : (int * Row.ty * expr) list` always did — and
    `Exec.nlj_probe_values` translates through `Exec.index_lookup_values`, the
    very function the `WHERE col = lit` seek uses. So a join key and a filter
    cannot disagree about which stored keys an equality covers. The type is
    supplied by `Planner.probe_key_for_index` from the RIGHT table's column
    list; note `Probe_from_left (ord, ty)` pairs a LEFT-row ordinal with the
    RIGHT index column's type, which is the whole point.

  NULL is unchanged and must stay so: both hash sides drop a NULL key before
  keying and `index_lookup_values` answers `None` for one, so a NULL join key
  matches nothing and a LEFT JOIN null-extends it.

  **Strict column typing bounds the blast radius, and that claim was verified
  rather than assumed.** One column can never hold both `1` and `1.0`
  (`INSERT 1.0` into an INTEGER column is a sema error), so the UNIQUE
  conflict probe and the index put/del encodings are genuinely unaffected. The
  **FK child-reference probe was not** — a child column and its parent column
  may be declared with different numeric types — and was broken as of #743:
  `Exec.fk_child_has_ref` seeked the child index with raw bytes when one
  existed and fell back to a `compare_values` scan when one did not, so an
  indexed cross-numeric child reference was MISSED and `ON DELETE RESTRICT`
  orphaned the row. Deliberately not fixed in #743 itself (different site, and
  the cascade paths wanted auditing with it); fixed separately by **#755**,
  below.

  **The one residual #743 could not close was `-0.0`, and it was not #743's —
  fixed by #754, see below.** `Float.compare (-0.) 0.` is `0`, so
  `compare_values` and `=` call `0`, `0.0` and `-0.0` all equal;
  `Index_key.encode_value` used to deliberately order `-0.0` below `+0.0`.
  That split already existed between an indexed and an unindexed
  `WHERE b = 0.0` over a REAL column holding `-0.0` (measured on `main`
  pre-#754: 0 rows with the index, the row without it), and the join arms
  inherited exactly it and nothing wider — the canonical hash key matched, the
  index probe did not.

  Pinned by `test/test_join_key_743.ml`, whose `check_all_three` runs the same
  data as a hash join, as a nested-loop probe and as a WHERE filter and asserts
  `used_index` for each, so a planner change cannot make the agreement vacuous
  by choosing one arm for all three. Verified by mutation: reverting either arm
  alone fails 6 of its 10 cases.

  **TEXT vs numeric is a deliberate divergence and must stay one.** sqlite3
  applies column affinity to a comparison operand, so `ON tl.a = tr.b` across a
  TEXT and an INTEGER column joins the row (oracle-checked 2026-09-03). Granary
  applies no affinity anywhere — the same divergence recorded below for
  `o BETWEEN 100 AND '119'` — so a cross-CLASS pair is never equal, in the join
  key and in the filter alike. What #743 owes is that the three spellings agree
  with each other, and they do.

  Pinned by `test/test_cmp_eq_738.ml`. Its `*_seeks_*` cases are the point of
  the file: each runs the seeking spelling **and** an unoptimizable foil
  (`col + 0`, which no recogniser matches) and requires the two to agree, and
  asserts `used_index` so a later planner change cannot make the agreement
  vacuous by choosing a scan. Verified by mutation: reverting only the index
  sites while leaving `=` exact fails 6 of its 15 cases, all of them
  seek/residual disagreements. `test_cmp_result_733`'s
  `cross_numeric_equality_is_unchanged_pending_738` — which pinned the hole so
  #738 would have a test to invert — is now
  `cross_numeric_equality_is_exact_738` and asserts the opposite.
  `a_cross_numeric_join_key_still_matches_nothing_743`, which pinned #743's
  hole the same way, is now `a_cross_numeric_join_key_matches_now_743`.

  **Why the index path survived #733, which is the thing to check before
  touching this again.** `range_bound_key`'s `pred`/`succ` widening was written
  to compensate for the *inexact* residual predicate — below it,
  `ceil`/`floor` sought past keys that qualified under rounding. An exact
  predicate is *narrower* than the seek, which is safe only for as long as a
  residual actually runs over the seek's output. **It does, and the reason is
  load-bearing: `Planner.range_for_index` never marks a range conjunct
  consumed** (an *equality* conjunct is — see #738 above, which is the same
  distinction seen from the other side, and which had to move the seek itself
  precisely because it has no residual to fall back on). So the widening is now a deliberate
  over-approximation: kept, not removed, because a widened seek is sound under
  *both* the old and the new predicate semantics, and tightening it would make
  soundness depend on that planner property holding forever. Tightening it is a
  separable performance change worth at most one ULP of keys.

  What that cost the tests: `test_range_bound_517`'s
  `real_bound_on_a_huge_integer_column` compares a seeked query against an
  unoptimizable foil, and **both sides moved together**, so its row assertions
  no longer discriminate the widening — its `expect_examined` counts still do
  (2 → 1 at the 2^62 lower bound without it). Absolute, sqlite3-checked rows for
  that regime live in `test/test_cmp_result_733.ml` instead. One expectation in
  that file genuinely inverted: `cross_type_between_agrees_with_inequalities`'s
  "one text end" case (`o BETWEEN 100 AND '119'`) went from 0 rows to 201,
  because `o <= '119'` is now true for every integer instead of false. **Granary
  applies no column affinity to a comparison operand**, so a text literal stays
  text where sqlite3 would coerce `'119'` to `119` and answer 20; that
  divergence is pre-existing and unchanged in kind — only the granary-side
  number moved.

  **Unremarked improvement, recorded so nobody finds it by bisect.**
  `compare_values` is not only an ordering function: nine call sites read
  `compare_values a b = 0` as "equal"/"unchanged", and the old catch-all made
  every cross-class pair satisfy that. So `SELECT 1 IN ('abc')` answered `1`
  and `CASE 1 WHEN 'abc' …` matched. Two of the nine are FK correctness:
  `check_fk_parent_update_restrict`'s `unchanged` fast path and its deferred
  twin skipped the child probe entirely when a parent key moved between storage
  classes, and `fk_child_has_ref*`'s match tests counted a child row of a
  different class as a live reference. All now behave correctly.

  **DISTINCT is deliberately not routed through it** and still dedups on
  `row_key`'s string rendering, where `1` and `1.0` are different keys. So
  DISTINCT and GROUP BY still disagree about whether an int and a numerically
  equal real are one key. That is the residual, and #733/#734 narrowed it
  rather than closing it: `cmp_result` is no longer an independent comparator
  and, since #738, neither are `eval_binop`'s `=`/`<>` arms, so the count is
  down from four to **three** — `compare_values` (which all six WHERE
  comparisons, ORDER BY, GROUP BY, PARTITION BY and MIN/MAX now share),
  `row_key`'s string rendering, and `Reactive_view.value_compare`. The two
  remaining outliers were re-checked as part of #738 and deliberately left:
  `row_key` is a **dedup key**, not a predicate, so `1` and `1.0` are different
  keys there and DISTINCT still disagrees with GROUP BY about whether they are
  one; `Reactive_view.value_compare` is a row-identity ordering for the delta
  log (it ranks `V_int` before `V_real` outright, so it is not even a numeric
  comparator) and answers no user-visible question. Folding the three into one
  is what #579's own "Note" asks for and is still not done. Pinned by `test/test_compare_values_579.ml`. Two of its cases carry the
  weight: `the_falsifying_triple_is_ordered_exactly` pins the three >2^53
  comparisons directly, and the QCheck `compare_values is transitive over
  random triples` property fuzzes for the same class of defect. **That
  generator is weighted, not uniform, and the weights are load-bearing** — a
  uniform draw over its branches needs ~4x10^5 cases to reach a falsifying
  triple, and 20 000 uniform cases pass against the promoting comparator.
  Verified by mutation: with promotion restored, four cases in that file fail,
  including both properties. An assertion on a single fixed input order catches
  none of it.

- **A cross-numeric FK child reference is found by RESTRICT and every
  cascade action, indexed or not (#755, decided 2026-09-05).** The gap #743
  left open (above): `Exec.fk_child_has_ref_multi_in_tx` (the deferred-recheck
  path) and `Exec.scan_child_rows_multi_tx` (the shared "locate the child
  rows" primitive — used not only by the immediate RESTRICT check but by
  `FA_set_null`, `FA_set_default` and `FA_cascade` on both DELETE and UPDATE)
  each seeked the child FK index with raw `row_value_to_index_value` bytes, so
  a parent value of one numeric class and a child column of the other missed
  each other in the index even though `=` (via `compare_values`, exact since
  #579/#738) says they are equal. So a cross-numeric-indexed child row was
  silently skipped by SET NULL/SET DEFAULT/CASCADE too, not just RESTRICT —
  arguably worse, since nothing raises and a stale FK value is left behind.

  Both functions now resolve the child index's declared column types from
  `child_meta.Cat.columns` and route the seek through
  `Exec.index_lookup_values`, keyed by the CHILD column's declared type — the
  same rule #743 gave the nested-loop join probe. `None` from that translation
  means "no key of the child column's type can equal this parent value",
  which for the FK case is the honest "no child row can reference this, so
  the action is safe" answer, not a reason to fall back to the scan.
  `Cat.find_index_covering_cols` has exactly two callers in `lib/sql/exec.ml`
  and both are fixed; `update_col_in_tx` needed no change (it updates an
  already-located row by rowid, never seeks by value).

  **Two follow-on defects surfaced in PR #765's review, both fixed in the
  same change rather than filed separately:**

  - **A deferred recheck must not reuse a column ordinal a mid-transaction
    schema change has invalidated.** `enforce_insert_fk`'s deferred-recheck
    closure used to capture `child_col_idxs` (an ordinal) at INSERT time and
    reuse it unchanged when the recheck ran at COMMIT; an `ALTER TABLE ...
    DROP COLUMN` on the same table before COMMIT, in the same explicit
    transaction, can shift or invalidate that ordinal by the time the recheck
    actually runs — reachable as an uncaught `Invalid_argument`/`Failure
    "nth"`, or a silently wrong column comparison that lets a live reference
    through as "no match". `Exec.make_fk_recheck` already existed with the
    correct pattern (used by the UPDATE-side RESTRICT and the general cascade
    recheck): it re-resolves both column lists **by NAME**
    (`find_col_idx_by_name_opt`) against the schema fetched fresh AT RECHECK
    TIME, and if a name no longer resolves (dropped), reports "not violated"
    — the same "nothing left to enforce" answer a table drop already gives an
    FK. `enforce_insert_fk` now calls it instead of hand-rolling its own
    closure with the stale-ordinal bug, which also deleted the duplicate
    logic.
  - **A VIRTUAL generated FK-child column's declared type is not a reliable
    key for the seek.** `Row.encode_col_value` enforces that a column's
    runtime value tag matches its declared type for every ordinary and
    STORED-generated column — a mismatch raises there, so it can never reach
    storage — but a VIRTUAL generated column is always encoded as NULL and
    recomputed on read (`compute_virtual_generated_cols`) with **no** such
    check, so its expression's result can be a different storage class than
    the column declares (e.g. `x REAL GENERATED ALWAYS AS (y) VIRTUAL` where
    `y` is INTEGER — the index physically holds `IK_int`, never `IK_real`).
    Seeking by the declared type would then walk a prefix the index never
    holds, silently treating a real reference as absent — reopening the same
    failure class for a different reason. `Exec.child_index_key_types`
    excludes a VIRTUAL generated column from the indexed fast path entirely
    (returns `None`, meaning "do not trust the index for this seek"), falling
    back to the scan, which recomputes the row and compares by VALUE
    regardless of what storage class the index physically holds. No
    equivalent problem exists for a STORED generated FK-child column, by the
    `encode_col_value` argument above. This is a narrower instance of a wider,
    pre-existing and NOT fixed here gap — nothing anywhere casts a GENERATED
    column's evaluated value to its declared type, so an ordinary indexed
    `WHERE`/`JOIN` seek against a VIRTUAL generated column with a
    declared-vs-actual type mismatch has the analogous exposure. Not
    addressed in #755's scope; worth its own issue if it proves reachable in
    practice.

  `Exec.seek_index_matches` also factors the seek-and-prefix-walk mechanics
  the two fixed functions shared near-verbatim into one helper, parameterised
  by an `on_match` callback that returns whether to stop early (an existence
  check) or keep collecting (a locate-all scan) — a review cleanliness item,
  not a correctness one.

  **Round 1's own fix reopened the same failure class one level up — round 2,
  2026-09-05.** `make_fk_recheck`'s by-NAME re-resolution is exactly right for
  a `DROP COLUMN` of an unrelated column, but has two further gaps `ALTER
  TABLE` can open, both filed against the same PR (#765) rather than closed
  one at a time:

  - **`RENAME COLUMN` desyncs a by-name lookup from the catalog's own
    already-updated FK metadata.** `Cat.rename_column` rewrites
    `fk_local_cols`/`fk_parent_cols` in the catalog immediately, but a
    closure that captured the OLD column name at enqueue time still looks
    that name up at recheck time — finds nothing, its length check fails,
    and (round 1's code) reported "not violated": COMMIT silently succeeded
    despite the parent row never existing. Fixed by identifying the
    constraint by **ordinal** (`Exec.fk_ordinal`, 0-based position within
    `child_meta.Cat.fk_constraints`, found by physical equality) instead of
    by column name. The ordinal survives every schema mutation touching
    `fk_constraints` in this codebase: `rename_column_body` substitutes
    names 1:1 via `List.map` (order and length preserved); `drop_column`
    does not touch `fk_constraints` at all; `alter_add_column`'s inline FK
    is appended at the list's end. Nothing removes or reorders an existing
    entry. Re-fetching the constraint by ordinal at recheck time means its
    `fk_local_cols`/`fk_parent_cols` are already current — the rename is
    simply reflected in them.
  - **A composite FK, partially broken by `DROP COLUMN`, must fail loudly,
    not silently.** `Cat.drop_column` never touches `fk_constraints`, so
    dropping one column of a two-column FK leaves a genuinely DANGLING name
    behind — a "this constraint can no longer be evaluated" state, which
    ordinal identification does not and should not paper over (the ordinal
    finds the right, still-broken, constraint record). `make_fk_recheck`'s
    column-count mismatch now raises instead of returning "not violated",
    matching the immediate enforcement path's own established behaviour for
    the identical condition (`Exec.enforce_insert_fk`'s "some local columns
    not found in table"). Deferred and immediate now agree, which is what
    this whole PR has been chasing at every turn.

  **Round 2 chose the targeted fix over a structural one — and round 3
  showed why that choice does not scale, 2026-09-05.** Round 2's own
  write-up (preserved above for the record) declined refusing the mutation
  outright, reasoning that the pending-FK-check queue held an opaque
  closure with no table/column metadata a mutation site could inspect, and
  that the targeted recheck-side fix closed both round-2 findings with no
  new design surface. Round 3 found a THIRD mutation shape past the same
  recheck-side approach: `DROP COLUMN pid` followed by `ADD COLUMN pid TEXT
  DEFAULT 'x'` reintroduces the name `pid`, pointing at a semantically
  unrelated column — `make_fk_recheck`'s name/ordinal resolution succeeds
  (the name resolves, the column COUNT is unchanged), so it silently
  compares the wrong column's value and reports whichever verdict that
  produces. **The pattern across all three rounds is that a recheck-side
  fix can only ever close the mutation shape that prompted it, because the
  recheck runs AFTER the mutation and has no way to know what the schema
  used to look like.** Fixing it a fourth time on the recheck side would
  only buy time until a fourth shape.

  So round 3 does what round 2 considered and declined: `ALTER TABLE ...
  RENAME COLUMN` / `DROP COLUMN` now REFUSE outright when the column still
  has a deferred FK obligation pending in the CURRENT transaction, matching
  `ALTER TABLE ... RENAME`'s own existing refusal when a view or trigger
  depends on the table (#673/#645) — conservative by the same reasoning:
  refusing a mutation that might have been harmless is cheaper than
  attempting one that silently corrupts. What made this newly tractable
  where round 2 judged it too invasive: the queue does not need a redesign,
  only two more fields. `Cat.pending_fk_check` gained `pfk_child_table` and
  `pfk_fk_ordinal` — the exact identity `Exec.make_fk_recheck` already
  derives for its own resolution — carried ALONGSIDE the opaque
  `pfk_recheck` closure rather than replacing it, and a new
  `Cat.peek_pending_fk_checks` (non-destructive, unlike `drain_...`) lets a
  mutation site read the queue without disturbing what COMMIT still needs
  to run. `Exec.fk_obligation_conflict` walks that list, re-resolving each
  pending check's constraint FRESH by ordinal (the same call
  `make_fk_recheck` makes) to ask "does this constraint, AS IT STANDS RIGHT
  NOW, still name the column this ALTER is about to touch, as either its
  local or its parent side" — deliberately fresh rather than trusting
  anything captured at enqueue time, for the same reason `fk_ordinal`
  beat column names in round 2.

  **Deliberately narrow, and the boundary is worth stating precisely.**
  Only `RENAME COLUMN` and `DROP COLUMN` call the new check. `ADD COLUMN`
  needs no check of its own: the only way it could reintroduce a column a
  pending obligation cares about is if an earlier statement in the SAME
  transaction dropped that very column, and that drop is exactly what is
  now refused — there is no live conflict left for `ADD COLUMN` to walk
  into. Two residuals are named rather than silently left:
  - This closes the mid-transaction race, not `Cat.drop_column`'s standalone
    corruption (filed separately, #767): a `DROP COLUMN` with NO deferred
    check pending still succeeds and still leaves a dangling name in
    `fk_constraints` forever, exactly as before. That gap is wider than this
    PR's transaction-scoped remit and was correctly scoped out in round 2's
    own write-up; round 3 does not reopen that scoping decision.
  - `ALTER TABLE ... RENAME TO` (the whole TABLE) and `DROP TABLE` are OUT
    of round 3's stated scope (RENAME COLUMN / DROP COLUMN / ADD COLUMN) and
    can still desync a pending check by TABLE name the same way a column
    rename used to: `make_fk_recheck`'s `Cat.find_table_cached` returning
    `None` reports "not violated" — the identical silent-wrong-answer shape
    round 2's RENAME COLUMN finding had, one level up. Not fixed here; worth
    a future issue if it proves reachable, the same way #767 was filed
    rather than folded in.

  Item 2, fixed the same round regardless of which fix shape was chosen for
  item 1: `precheck_update_fk`/`precheck_delete_fk` (the IMMEDIATE
  RESTRICT precheck) were still reaching `Exec.find_col_idx_by_name`'s bare
  `Failure "column not found: <name>"` on a corrupted constraint — reachable
  via #767's standalone corruption, since nothing prevents that outside a
  pending obligation — while `Exec.make_fk_recheck` (the DEFERRED path)
  already raised a loud, FK-specific message for the identical condition
  since round 2. New `Exec.resolve_fk_col_idxs` gives both immediate
  functions the same graceful check `Exec.enforce_insert_fk` already had for
  its own INSERT-side lookup: deferred and immediate now agree on every FK
  column-resolution failure, not just the ones round 1 and round 2 happened
  to touch.

  Item 3, also independent of the structural-vs-targeted choice:
  `make_fk_recheck`'s `List.nth_opt child_now.Cat.fk_constraints fk_ordinal`
  raises `Invalid_argument "List.nth"` for a NEGATIVE index rather than
  answering `None` — it only degrades gracefully for an out-of-range
  POSITIVE one. Every caller falls back to the sentinel `-1` if
  `Exec.fk_ordinal` somehow returns `None` (which should never happen: every
  caller passes an `fk` literally drawn from the list `fk_ordinal` searches),
  so this was a crash trap for an unreachable-in-practice case, in code whose
  whole design intent is graceful, loud handling. `make_fk_recheck` now
  guards `fk_ordinal < 0` explicitly and raises a clear internal-error
  message instead of letting `List.nth` blow up if the sentinel is ever hit.

  Also cleaned up in round 2, still true after round 3: the three
  near-identical `List.for_all2 (fun ci pv -> compare_values row.(ci) pv =
  0)` closures (`seek_index_matches`'s per-candidate check and both
  functions' full-scan fallback) are one `Exec.fk_cols_match`; the NULL
  check in both functions runs BEFORE `Cat.find_index_covering_cols` and
  `child_index_key_types` (two `Array.of_list` builds) rather than after;
  and `child_index_key_types`'s comment no longer claims an out-of-range
  ordinal "degrades to the scan instead of crashing" — the fallback scan's
  own predicate indexes the same ordinal and would crash identically, one
  step later. What actually rules a stale ordinal out is
  `make_fk_recheck`'s fresh resolution, not that function's own (still
  worthwhile, still total) bounds check.

  Pinned by `test/test_fk_cross_numeric_755.ml`: the issue's own repro, the
  un-indexed regression guard, the reversed type order, a same-type
  no-false-refusal case, both cascade actions found in the audit (SET NULL,
  CASCADE), the VIRTUAL generated FK-child column repro, a WITHOUT ROWID
  child-table repro, a 200-case QCheck property tying RESTRICT's refusal to
  genuine cross-numeric reference existence (indexed or not), and — for
  round 3 — the ALTER TABLE refusal itself (RENAME and DROP of a
  pending-FK column; the DROP-then-ADD-same-name shape refused at its
  first statement), a NO-OVER-REFUSAL case (renaming an unrelated column on
  a table with an unrelated pending check still succeeds), and both
  immediate-path loud-failure cases (item 2). Round 1 and round 2's own
  DROP-COLUMN/RENAME-COLUMN mid-transaction tests were REWRITTEN rather than
  kept as-is once round 3 landed: the ALTER itself now fails before COMMIT
  is ever reached, so a test asserting "COMMIT still refuses/allows"
  no longer exercises anything — asserting the ALTER's own refusal is the
  version of that test that means something post-round-3.
  Verified by reverting to the pre-round-N code at each round (a WIP commit,
  checked out and restored — not `git stash`): round 1's DROP COLUMN test
  failed with the predicted `Invalid_argument("index out of bounds")` and
  the VIRTUAL generated-column test failed by wrongly allowing the DELETE;
  round 2's RENAME COLUMN and composite-drop tests failed against round 1's
  code exactly as predicted (a silent COMMIT success, and a silent "not
  violated" respectively); round 3's five new/rewritten cases failed against
  round 2's code as predicted (the ALTER succeeding when it should have been
  refused, and the immediate path's bare "column not found" instead of a
  loud FK message).

  **Round 4 found the same disagreement one layer further down — the
  CASCADE-application functions, not just the RESTRICT paths — and a
  reopened crash trap in round 3's own new function, 2026-09-05.** Every
  fix through round 3 converged the RESTRICT surface — `make_fk_recheck`
  (deferred), `precheck_update_fk`/`precheck_delete_fk` (immediate) — on
  one behaviour for a corrupted column: fail loudly with an FK-specific
  message. Round 4's review found four CASCADE-dispatch functions never
  got the same treatment, and disagreed with each other as well as with
  RESTRICT:
  - `cascade_delete_fk` (the NESTED cascade dispatch, reached when a
    CASCADE recursively deletes a further child's own children) used
    `find_col_idx_by_name_opt` and, on a miss, **silently `Lwt.return_unit`**
    — the cascade action (SET NULL/SET DEFAULT/CASCADE/RESTRICT) never
    runs, nothing raised. Worse than RESTRICT's own loud refusal for the
    identical condition, and the literal "nothing errors" failure mode
    this issue's own cascade-path audit (#755's original diff) called out
    for the cross-numeric bug — reopened here for the corrupted-column
    case instead.
  - `apply_update_cascade_fk` / `apply_delete_cascade_fk` (the TOP-LEVEL
    cascade dispatch for a direct child of the row being updated/deleted)
    and `cascade_update_fk` (the nested UPDATE-side sibling of
    `cascade_delete_fk`, plus its `cascade_update_set_null`/
    `cascade_update_set_default` call chain) all used the raw, crashing
    `find_col_idx_by_name` — a bare `Failure "column not found: <name>"`
    with no FK context, a third behaviour for the same condition.

  All four (and, as a non-blocking cleanup folded in the same round since
  it was cheap, `enforce_insert_fk`'s own inline resolution, which
  predated `resolve_fk_col_idxs` and duplicated its logic with a
  slightly different message) now go through `Exec.resolve_fk_col_idxs`
  — one behaviour for "an FK column can no longer be resolved," everywhere
  in the FK surface, deferred or immediate, RESTRICT or CASCADE.

  **A second, independent finding in the same round: `fk_obligation_conflict`
  (round 3's own new function) reopened round 3's `make_fk_recheck`
  crash trap in the function that was supposed to prevent needing it.**
  `List.nth_opt child_now.Cat.fk_constraints chk.Cat.pfk_fk_ordinal` raises
  `Invalid_argument` for a NEGATIVE index rather than answering `None` —
  the identical gap `make_fk_recheck` guards against explicitly — and
  every `pfk_fk_ordinal` can be the `-1` sentinel if `Exec.fk_ordinal`
  ever fails to resolve at enqueue time. Reachable (in principle) from
  ANY `RENAME COLUMN`/`DROP COLUMN` in a transaction that shares the
  queue with one corrupted pending check, crashing an otherwise-unrelated
  ALTER instead of refusing it. Guarded the same way: check
  `pfk_fk_ordinal < 0` first and treat it as "no conflict from this
  entry" (this pending check can never be resolved either way, so it
  cannot be the reason to refuse someone else's ALTER).

  Neither finding needed a structural-vs-targeted reassessment — both are
  "make an existing loud-failure convention apply somewhere it was missed"
  and "add the same defensive guard a sibling function already has,"
  not a new mutation shape past the round-3 refusal.

  Pinned by four new `test/test_fk_cross_numeric_755.ml` cases, one per
  CASCADE-dispatch function, each reached via #767's same standalone
  autocommit `DROP COLUMN` (no pending check, so round 3's refusal does
  not intervene): `apply_delete_cascade_fk_fails_loudly` and
  `apply_update_cascade_fk_fails_loudly` hit the two top-level dispatch
  functions directly; `cascade_delete_fk_fails_loudly_not_silently` and
  `cascade_update_fk_fails_loudly` reach the two NESTED dispatch functions
  by building a two-level cascade chain (`p -> m -> c`, corrupting `c`'s
  column) so the recursive call actually exercises them rather than the
  top-level pair. `fk_obligation_conflict`'s negative-ordinal guard is
  deliberately NOT given a dedicated SQL-level test, for the same reason
  `make_fk_recheck`'s round-3 sibling guard was not: both `fk_ordinal` and
  `fk_obligation_conflict` are private to `exec.ml` (no `val` in
  `exec.mli`), and forcing the `-1` sentinel from outside the module would
  need either a test-only seam into private state or a schema-corruption
  bug this PR's own `fk_ordinal` (physical-equality lookup against a list
  every caller draws its argument from) rules out by construction. All
  four new cases verified to fail against the pre-round-4 code exactly as
  predicted (a WIP commit, checked out and restored — not `git stash`):
  the silent-no-op case for `cascade_delete_fk` and the bare
  `Failure "column not found: <name>"` for the other three.

  **Round 5 found one more disagreement in the surviving raw NULL handling,
  and — separately — that round 1's blanket VIRTUAL-column disqualification
  was safe but overly conservative, 2026-09-06.**

  `cascade_update_fk` was the one FK call site in this file still missing
  the `any_null_val` guard every other one has before scanning for
  matching child rows. `compare_values` (which `fk_cols_match`'s
  full-scan fallback uses) treats `[V_null, V_null]` as equal — structural
  equality for ordering, correct for that purpose — but that is NOT the
  FK reference rule: a NULL component means the row never matched
  anything under three-valued logic, so scanning for it can wrongly
  cascade into a child row that merely holds NULL in the same column
  too, not one that ever actually referenced the parent. Reachable
  through a SECOND-LEVEL `ON UPDATE CASCADE` fan-out where the parent's
  own reference uses one column and its child's reference to it is a
  DIFFERENT, composite pair with a NULL member — `m`'s reference to `p`
  via `pa` alone, `c`'s reference to `m` via `(pa, qb)` where `qb` is
  NULL: when `p`'s cascade changes `m.pa`, `cascade_update_fk` computes
  `m`'s OLD `(pa, qb)` to find `c`'s matching rows, and without the
  guard a `c` row with `(ca, cb) = (pa_old, NULL)` gets swept in even
  though its own NULL `cb` meant it was never validly checked at INSERT
  time either. Fixed with the identical `any_null_val` guard
  `cascade_delete_fk`/`apply_update_cascade_fk`/`apply_delete_cascade_fk`
  already had — a small, consistent addition, not new logic.

  Separately: `child_index_key_types` disqualified the indexed fast path
  for the WHOLE composite key if ANY single column was VIRTUAL, even with
  a covering index and otherwise-ordinary columns — a composite FK with
  one generated helper column lost the index entirely, the exact
  performance cliff this PR exists to avoid. VIRTUAL columns are
  ordinarily indexable in this engine (nothing in the index-build path
  refuses one); round 1's disqualification was about trusting the
  DECLARED type as the physical index-key storage class, not about
  VIRTUAL columns being unindexable in general. The round-1 concern is
  real whenever the generated expression's result can differ in storage
  class from the column's declaration (`x REAL GENERATED ALWAYS AS (y)
  VIRTUAL` where `y` is INTEGER stores `IK_int`, not `IK_real`) — but one
  shape is PROVABLY safe regardless: a top-level `CAST(_ AS ty)` where
  `ty` matches the column's own declared type. `Exec.eval_cast` (the
  interpreter for `Plan.P_cast`) is a total, exhaustive match on the
  target type — every non-NULL branch for `Ast.Ty_real` returns
  `Row.V_real`, every branch for `Ast.Ty_int` returns `Row.V_int`, and so
  on — so a CAST to the column's own type GUARANTEES that storage class
  no matter what the inner expression would otherwise have produced.
  New `Exec.virtual_col_cast_matches_declared_type` checks for exactly
  that shape (parses the GENERATED expression via the existing
  `compile_generated_expr` cache, so repeated checks are cheap); a
  VIRTUAL column matching it is now trusted like an ordinary column,
  and only a VIRTUAL column WITHOUT that guarantee still disqualifies
  the seek — still for the WHOLE composite key, since a B-tree seek
  needs every prefix column translated together, not some of them.
  Proving anything past a bare CAST safe (arithmetic, a bare column
  reference, a function call) would need real static type inference
  over the expression grammar, which this engine does not have and does
  not try to approximate here — deliberately narrow, not a general
  VIRTUAL-column type-inference feature.

  `child_index_key_types` and its new helper were also changed to take
  `~table_name`/`~columns` rather than a full `Cat.table_meta` (the
  `storage`/`fk_constraints` fields were never used), and
  `child_index_key_types` is now exposed in `exec.mli` — purely so a test
  can ask the fast-path/full-scan question directly (`Some` vs `None`
  IS the answer to "was the index trusted"), rather than needing to
  infer it from timing or a new `GRANARY_BENCH_*` scaling gate.

  Pinned by five more `test/test_fk_cross_numeric_755.ml` cases:
  `cascade_update_fk_does_not_cascade_a_null_composite_match` builds the
  `p -> m -> c` second-level fan-out above and asserts `c`'s row is
  untouched while `m`'s own cascade still applies correctly;
  `child_index_key_types_trusts_cast_matched_virtual_column` and
  `child_index_key_types_still_declines_uncast_mismatched_virtual_column`
  call the newly-exposed function directly (`Some [...]` for a
  CAST-matched composite key, `None` for round 1's own uncast repro,
  pinning that the narrowing does not weaken it); and
  `indexed_cast_matched_virtual_composite_fk_restrict_refuses` confirms
  end-to-end that the narrowed rule does not break RESTRICT's
  correctness for the case it newly trusts. Verified against the
  pre-round-5 code (a WIP commit, checked out and restored — not `git
  stash`): the NULL-guard case failed exactly as predicted (`c`'s row
  wrongly cascaded); the two `child_index_key_types` unit tests could not
  even be built against the pre-round-5 `exec.mli` (the function did not
  exist to call), which is itself the strongest possible confirmation
  that the capability was genuinely new.

  **Round 6 re-confirmed round 5's fixes and closed with no new blocking
  code issue, but named a compatibility cost of round 4's own fix worth
  recording, 2026-09-06.** Round 4's fix of `cascade_delete_fk` (silent
  no-op → `Lwt.fail_with` on an unresolvable FK column, above) and the
  equivalent fix in `make_fk_recheck` (silent "not violated" →
  `Lwt.fail_with`, round 3's own write-up) both now raise on the
  pre-existing, out-of-scope corruption class `Cat.drop_column` can leave
  behind (#767: a standalone `DROP COLUMN` with no pending obligation
  still leaves a dangling `fk_constraints` entry, forever). Loud beats
  silently wrong, but it is not free: a table already corrupted by #767 —
  where an unrelated DELETE/UPDATE used to succeed with the cascade
  silently skipped — now has every DML statement that reaches that
  cascade dispatch fail outright, with no repair path short of recreating
  the table. Anyone upgrading a deployment with pre-existing
  #767-corrupted on-disk state should read this as the compatibility note
  it is, not a new defect it introduces. Round 6 also produced a concrete
  repro for the `RENAME TABLE`/`DROP TABLE` residual round 3 named above
  (#768) — no new action follows; it stays the same already-filed,
  already-scoped-out gap, just no longer hypothetical.

- **`OR IGNORE` skips a NOT NULL violation; `OR REPLACE` raises on one (#599, decided 2026-08-02).**
  A conflict-resolution modifier means the same thing for NOT NULL as it does
  for UNIQUE. `OR IGNORE` skips the offending row — consistent with the UNIQUE
  path, with the modifier's own meaning, and (incidentally, not decisively)
  with SQLite. Every other resolution raises: bare, `OR ABORT`, `OR FAIL`,
  `OR ROLLBACK` and `OR REPLACE`.

  **`OR REPLACE` is the divergence.** SQLite substitutes the column's DEFAULT
  for the NULL and aborts only when the column has none — oracle-checked:
  `INSERT OR REPLACE INTO d VALUES (2, NULL)` on `v INTEGER NOT NULL DEFAULT 42`
  stores `2|42`. Granary raises even when a DEFAULT exists. `REPLACE` here
  means "delete the row this one conflicts with", and a NULL conflicts with
  nothing; storing a value the caller never supplied is a larger surprise than
  the error. Anyone changing this is changing a decision, not fixing an
  oversight.

  **The skip is decided in exactly one place, and it is the runtime one.**
  `Exec.not_null_skip_or_fail` is called from the INSERT sites only —
  `execute_insert`, `execute_insert_write` and the two columnstore `Op_insert`
  / `Op_insert_select` arms. (`execute_insert`'s call is #639's: it is guarded
  by `CA_ignore` and runs *before* conflict resolution, so the skip no longer
  depends on which index the row collides with;
  `execute_insert_write`'s is guarded by `not skip` and so never
  double-evaluates it.) `Exec.enforce_not_null` is unchanged and still
  raises unconditionally. Since #620 it has exactly ONE call site left —
  `write_row_rekeyed` — but three write paths funnel through it: plain
  `UPDATE`, `UPSERT ... DO UPDATE`, and `ON UPDATE CASCADE`. None of the three
  has an `OR IGNORE` form to consult, so softening that function would relax
  all three at once, silently and with no syntax asking for it.
  `Sema.bind_insert_row`'s static literal-NULL check **suspends itself** under
  `CA_ignore` rather than deciding anything — it exists to give the better,
  earlier error for the other resolutions, and it must not pre-empt the
  runtime skip.

  That last point is the one that shipped wrong once. The static check fires
  per STATEMENT, so while it was unconditional
  `INSERT OR IGNORE INTO t VALUES (1,10),(2,NULL),(3,30)` lost **all three**
  rows, where the parameter spelling of the same statement skipped one — worse
  than the defect #599 was filed about. Note also that the boundary was never
  "a literal NULL is a bind error" but "a literal NULL *in a VALUES list*":
  `INSERT OR IGNORE ... SELECT k, NULL FROM s` was always skipped silently,
  because `Sema` does not inspect a projection. `or_ignore_skips_every_spelling_of_null`
  in `test/test_not_null_599.ml` pins all four spellings together for that
  reason.

- **An aggregated SELECT sorts by grouped columns, or it refuses (#663, decided
  2026-09-02).** This is the same rule the projection and HAVING already
  applied, arriving late at the third of the three clauses — and it is a
  **deliberate divergence from sqlite3**, which accepts a bare non-grouped
  column in `ORDER BY` (oracle-checked: it returns rows, picking an arbitrary
  row's value from each group). Granary already declines that permissiveness in
  the other two clauses, and ORDER BY was the one where the consequence of not
  having the rule was a *wrong answer* rather than an error.

  The mechanism is worth knowing because it is the same one twice. An
  aggregated SELECT sorts AFTER projection, and
  `Planner.plan_post_agg_sort_input_space`'s `remap_e` rewrites a key's column
  index from pre-aggregation space into the aggregated output row. It only
  rewrites an index it *finds in* `group_by`, and — until #663 — only at the
  ROOT of the key expression. So:

  - a **non-grouped** column bound to its input index, was not remapped, and the
    key read whatever OUTPUT column sat at that index.
    `SELECT nm, COUNT(*) FROM g3 GROUP BY nm ORDER BY pad` sorted by the count,
    silently; on a table wide enough that the input index exceeded the output
    arity it indexed past the end of the row and raised `Invalid_argument` from
    `Exec` mid-query instead of a `Db.error`.
  - a **grouped** column *inside an expression* was not remapped either, because
    the rewrite did not recurse. That one only ever looked right when the
    grouped column sat at input index 0, where the two spaces coincide — which
    is what every fixture in the tree happened to do.

  Both halves are needed and they lean on each other: `Sema.bind_select_order`
  now refuses a key naming any non-grouped column (via `expr_col_refs`, so a
  column buried in a `CASE` or a concatenation is caught too), which is
  precisely what makes it safe for `remap_e` to recurse — **every** `P_col`
  remaining under an aggregated ORDER BY key is a grouped column, so descending
  cannot mis-fire on one that should have been left alone.

  **Being grouped is necessary but NOT sufficient to be remappable, and that
  gap is a third thing the fix owes.** `remap_e`'s inner search needs the
  column to appear in `agg_proj` as an `AP_group_col` — to be *projected as a
  bare group column*. Group by a column and do not project it and the search
  fails, so the old `| None -> e` arm kept the pre-aggregation index and both
  of the symptoms above came straight back:
  `SELECT COUNT(*) FROM g GROUP BY nm ORDER BY nm` raised `Invalid_argument`
  out of the executor, and `SELECT UPPER(nm), COUNT(*) … ORDER BY nm` sorted by
  the count. Making `remap_e` recursive *widened* the reach of that arm rather
  than narrowing it. An unprojected grouped column now gets a **hidden output
  slot** — appended to `Op_aggregate`'s own projection where its ordinal means
  what it was bound to mean, sorted on by position, and trimmed by an
  `Op_project` around the sort — which is exactly the mechanism
  `plan_agg_order_hidden` already uses for #495's aggregate keys. One slot per
  grouped column, not per mention. `GROUP BY` a column you do not project and
  then `ORDER BY` it is idiomatic, not a corner.

  **`is_aggregated` is not the same question as "has a GROUP BY", and the check
  is gated on both.** It is also true for an aggregate in the projection with
  no GROUP BY at all, where `group_cols` is `[]` and therefore every column
  reference fails the membership test. Gating on `is_aggregated` alone refused
  `SELECT COUNT(*) FROM g ORDER BY nm` — which sqlite3 answers, which `main`
  answered, and whose error message named a grouping there is none of. Such a
  statement returns exactly one row, so its ORDER BY is a no-op whatever it
  names; the refusal is skipped and the planner drops the sort rather than
  indexing a one-column output row with a pre-aggregation ordinal.

  The two sites that produce the refusal message —
  `bind_select_order_agg`'s `grouped` and `bind_select`'s `grouped_check` —
  share one `non_grouped_order_msg` helper rather than two copy-pasted format
  strings.

  Three things stay legal and are pinned as such: an ordinal and an output alias
  (both address the output row directly and are not column references, #489), an
  expression over a *grouped* column, and every non-aggregated SELECT
  (`grouped_check` is `None` there — a bare column is bound and evaluated
  against the input row exactly as before). An ORDER BY that *mentions an
  aggregate* keeps taking #495's separate `bind_select_order_agg` path, which
  had this discipline from the start. Pinned by `test/test_agg_order_by_663.ml`,
  whose first two cases assert the projection and HAVING already had the rule —
  that is what makes this consistency rather than a new opinion.

- **`COLLATE` is a comparison attribute and never rewrites a value (#722, fixed
  2026-09-03).** `Exec.eval_expr`'s `P_collate` arm used to implement
  `COLLATE NOCASE` as `String.lowercase_ascii` **on the value**. For an equality
  test the two models are indistinguishable, which is why it stood; in a
  value-returning position they are not, and
  `SELECT x COLLATE NOCASE FROM t` returned `'hello'` where sqlite3 returns
  `'HELLO'`. The same leak reached `CAST`, `||`, a function argument, `MIN`/`MAX`'s
  result, and the rows `SELECT DISTINCT` emits.

  `P_collate` is now the **identity on the value**. `Exec.expr_collation` reads
  the collation off an operand *expression* and each comparison site keys both
  sides through `Exec.collate_key` before comparing. **`collate_key` is
  byte-identical to the old fold**, so at every site the fix touched, the
  comparison answer is unchanged and only the emitted value moves — which is
  what made a change this wide auditable. The corollary is the hazard: a
  comparison site the fix *missed* would silently drop from NOCASE to BINARY, so
  the enumeration below is the artifact, not the diff.

  The collation of an operand follows sqlite3's rule — an operand carries an
  explicit collating-function assignment if **any** subexpression uses postfix
  `COLLATE`, leftmost wins. Two consequences beyond the headline, both
  oracle-checked: it now propagates *out* of a subexpression
  (`(x COLLATE NOCASE) || '!' = 'HELLO!'` matches, where it used to match
  nothing), and it no longer folds through a **non-comparison** operator
  (`(x COLLATE NOCASE) || 'B'` is `'HELLOB'`, not `'hellob'`).
  `Exec.binop_takes_collation` is that boundary: the six comparisons take one,
  and `Like` (`like_match` already lower-cases both sides) and `Glob` (sqlite3's
  GLOB is case-sensitive regardless of collation, oracle-checked) deliberately
  do not.

  **The comparison sites, all of them.** `P_binop`; `P_between` (which reached
  `eval_binop` *directly*, bypassing the old propagation entirely, so
  `x COLLATE NOCASE BETWEEN a AND b` folded `x` and neither bound and answered
  **no rows** — a second bug the model fixes rather than preserves); `eval_in`
  and `eval_in_select`; `eval_case_expr`'s scrutinee; the four sort-key sites
  and the window partition/peer sites, all via `Exec.eval_sort_key`;
  `distinct_filter` and MIN/MAX in both `aggregate_over_values` and
  `make_agg_acc_over_values` (which stay byte-identical to each other), keyed
  off `agg_spec_collation`; and `Op_distinct` plus the three set operations.

  **The dedup sites are the awkward ones and the reason `Exec.output_collations`
  exists.** `Op_distinct` is literally `{ child : op }` — it compares the OUTPUT
  row and holds no expressions — so before #722 it deduped case-insensitively
  only as a *side effect* of the projection having lower-cased the value, and it
  emitted that lower-cased value. Making `P_collate` transparent without giving
  those nodes the collation would have turned a wrong VALUE into a wrong ROW
  COUNT, which is worse. `output_collations` reads the per-column collation back
  off the projection underneath (`Op_expr_project`, `Op_const_select`,
  `Op_aggregate`'s `proj_item`s, descending through `Op_project`'s ordinals and
  through sort/limit/filter), and answers `Collate_binary` for anything it
  cannot resolve. `collated_row_keyer` makes the all-binary test **once per
  operator**, returning `row_key` itself, so DISTINCT and UNION pay nothing.

  **`GROUP BY x COLLATE NOCASE` is a parse error and stayed one.** It is the one
  comparison site that could not be made collation-aware, and the reason is a
  grammar limitation rather than a decision: `Ast.group_by_item` is
  `string * string option`, a name and an optional qualifier, not an expression,
  so `Exec.aggregate_build_groups` (over `group_cols : int list`) has nothing to
  consult. sqlite3 accepts it. **Anything that widens `group_by_item` to an
  expression owes that comparator the same treatment the sort keys got** —
  pinned as a live assertion by `group_by_collate_is_still_a_parse_error`, which
  fails if the grammar is widened without it.

  **A collated comparison cannot reach an index seek, and declining is the
  chosen answer.** Not by a new rule: every seek decision is made on
  `Sema.bound_expr` *before* `plan_expr` runs, and both recognisers
  (`Planner.recognise_eq_col_lit`, `recognise_range_col_lit`, plus
  `recognise_eq_col_col` for join keys) pattern-match `BE_col` / `BE_lit` /
  `BE_param` **directly with no wrapper stripping**, so a `BE_collate` falls to
  their `| _ -> None` arm in either operand position. That has to stay true.
  `Index_key.encode_value` emits BINARY bytes, so a NOCASE seek over them would
  skip exactly the rows the collation exists to find — the rows-lost failure
  mode the NaN section above describes — and it would be **unrecoverable**,
  because `Planner.plan_base` *drops* the conjunct a seek consumed
  (`residual_filter ~consumed`), deleting the predicate that would otherwise
  re-check the collation. Teaching a recogniser to strip `BE_collate` therefore
  requires forcing the conjunct to stay unconsumed, or a collation-aware
  encoding, and neither is worth it for a feature with no DDL spelling.

  There is **no DDL collation in this engine** — `COLLATE` appears in the
  grammar only as a postfix expression operator, never in a column definition or
  a `CREATE INDEX` — so no column and no index carries an implicit non-binary
  collation, and a UNIQUE constraint is BINARY by construction. If DDL collation
  is ever added, the index-seek question above reopens *and* the UNIQUE conflict
  probe (byte-exact on `Index_key` encodings) becomes a second, independent
  divergence.

  Two accepted residuals, neither introduced by #722: which duplicate survives a
  NOCASE-equal group in `UNION`/`INTERSECT` differs from sqlite3 (it sorts the
  compound and reports a different representative; granary streams the left arm
  and keeps the first it saw — the row *count* agrees), and an aggregate result
  projected under DISTINCT gets its collation from `agg_spec_collation`, not
  from the aggregate's own output column. Pinned by `test/test_collate_722.ml`;
  the two cases in `test/test_collate_outer_ref_670.ml` that used to pin the
  folded values were rewritten to the sqlite3 answer, deliberately, as that
  file's own comment predicted they would have to be.

- **One rule resolves a correlated subquery's outer references (#635/#626/#615, 2026-08-06).**
  An input's **scope identifier** is its FROM item's alias where it has one and
  its table name otherwise — an alias *replaces* the name. A qualified outer
  reference names a scope identifier; an unqualified one names a column exactly
  one input carries; **anything else is an error, never a silent empty or NULL
  result.** That rule holds at any nesting depth and in every clause a
  correlated subquery can sit in — WHERE, an INNER or OUTER join's ON,
  a projection, and HAVING.

  It is implemented at **three** levels and they must not be allowed to drift
  apart again, because each pair that disagreed produced a different silent
  wrong answer:
  - `Sema.from_ident` for the binder's two qualified lookups (`bind_expr_join`,
    `select_qual_lookup`);
  - `Exec.inner_scope_of` for a subquery's *own* FROM (this one was always
    right);
  - `Exec.scan_ident` / `get_outer_scan_metas` for the outer inputs, which
    needs `alias` on `Plan.Op_seq_scan` / `Op_col_seq_scan` /
    `Op_index_lookup` / `Op_rowid_lookup` and `right_alias` on
    `Op_nested_loop_join`, carried from `Sema.BS_select.table_alias` and
    `Sema.bound_join.right_alias`.

  Consequences worth knowing before editing this area:
  - **`SELECT t.x FROM t s` is now an error**, matching sqlite3. It used to
    answer rows, and that is what made an alias-hidden name inside a subquery
    resolve *inward*: the subquery was never recognised as correlated, was
    folded to a constant, and rows were lost with no error (#635's comment).
  - The duplicate guard in `get_outer_scan_metas` is by **identifier**, not by
    table name. `FROM l AS x JOIN l AS y` therefore resolves; the unaliased
    `FROM l JOIN l` still cannot and is still refused. #592's
    `self_join_is_refused_not_emptied` was rewritten to the unaliased spelling
    for exactly this reason — a green suite on the aliased one would now mean
    the opposite of what it used to. `test/test_refusal_error_627.ml` was
    written in parallel and had to be rewritten for the same reason, in two of
    its four categories: it provoked #627's refusals with the *aliased*
    self-join and with a plain correlated ON in an outer join, and #635 and
    #615 respectively turned both into answers. They are now the unaliased
    self-join and `l.a` under `FROM l AS x` in an outer join's ON. **Anything
    that resolves those spellings too must replace them again, not delete the
    row** — #627 covers seven public surfaces, and a category that quietly
    stops firing takes all seven with it.
  - `substitute_outer_in_expr` descends into nested `E_subquery` / `E_exists` /
    `E_in_select` carrying the **union** of every enclosing subquery's scope.
    Carrying only the innermost scope is the obvious implementation and is
    wrong: it rewrites an *intermediate* subquery's own column from the outer
    row.
  - **That descent is necessary but was not sufficient, and the missing half is
    "what counts as correlated".** `Sema` treats `E_subquery` / `E_exists` /
    `E_in_select` as opaque leaves and never descends into them, so a statement
    whose correlation sits TWO levels down *binds cleanly*. Every caller read
    "it bound" as "it is uncorrelated" and evaluated it eagerly, before any
    outer row existed to substitute from; the reference was then met for the
    first time by the intermediate query's own `stream_filter`, whose inputs are
    the intermediate FROM, and refused there. So the two-level shape
    `FROM l AS x WHERE EXISTS (SELECT 1 FROM r WHERE EXISTS (… v < x.a))` was
    refused with the descent in place — the descent was correct but never ran.
    `Exec.stmt_has_free_column_ref` answers the second question and is consulted
    at the one chokepoint all four callers share, `plan_subquery_cached`, so
    `refuse_unresolved_correlation` and the three `eval_*_subquery` functions
    cannot disagree about which statements are correlated. It is implemented by
    *running* `substitute_outer_in_stmt` with a binding that resolves nothing and
    records that it was asked, so the detector and the substituter agree by
    construction about which references are free — the same discipline the two
    binders are held to, and the reason not to hand-write a second walker
    (`substitute_outer_in_plan_expr` is already the odd member of a four-walker
    set, #670). It can only move a statement from "evaluate eagerly" to "treat
    as correlated", and `inner_scope_of` answers "owned" for everything it
    cannot resolve, so an unresolvable FROM never manufactures a free reference.
    Pinned by `two_nesting_levels` in `test/test_alias_outer_ref_635.ml`.
  - **#566's refusal of a correlated ON subquery in an OUTER join is
    reopened (#615).** Its stated blocker — no correlation source over a join
    node — was removed by #592, so the `Left` arm now substitutes per (left,
    right) *pair* inside the join. It has to be per pair, not per surviving
    row: for an outer join the ON predicate **is** the match test, and a filter
    above the join rejects the null-extended row it must emit (#552). The pure
    pairing loop is kept as the arm taken when no subquery survives.
  - `Op_nested_loop_join` cannot carry a subquery in its probe through the
    planner, but it is a public constructor, so `stream_nested_loop_join`
    refuses one explicitly rather than encoding it as NULL.
  - **The two walkers are now exhaustive over the clauses and nodes an outer
    reference can sit in (#721 and #732, fixed 2026-09-03).** Both were the
    same shape as #670 in a new place — a walker not descending somewhere that
    carries expressions, turning a runnable query into a refusal — and both are
    error-to-answer changes, never answer-to-answer:
    - `substitute_outer_in_expr` descends into `E_agg`, `E_agg_distinct` and
      `E_window` (the latter's `args`, `partition_by` and `order_by`;
      `frame_spec` carries no `expr`). #670 had listed them with an explicit
      `-> e` to make the omission visible. `E_fts_snippet` is a true leaf and
      was never one of them. #488 is what made this reachable at all: an
      aggregate's argument became a general expression, so `SUM(i.v + o.n)` is
      a legal spelling.
    - `substitute_outer_in_stmt` rewrites a SELECT's `proj` and the FROM-less
      `S_const_select` that `(SELECT o.n * 10)` parses to, and its statement
      match is exhaustive rather than `| _ -> s`. `inner_scope_of` answers
      `no_inner_scope` for `S_const_select` — it owns no input, so it can
      shadow nothing — rather than the "owns everything" default every other
      non-SELECT gets.
    - `E_window`'s only legal home is a projection, so #721's window arms are
      unreachable without #732's clause walk; the two land together for that
      reason.
    - A projection in the parser's `Cols` shape (the polymorphic-variant
      `` `Cols ``) is a `string list` and cannot hold a substituted literal, so
      `substitute_outer_proj` **promotes it to `` `Exprs ``** — but only when a
      name actually resolves. That condition is what keeps the
      detector honest: `stmt_has_free_column_ref` runs this same function with
      a probe binding that resolves nothing, so it inspects without reshaping.
    - **`order` and `group_by` are deliberately NOT rewritten, and that is a
      decision.** `group_by` and `limit`/`offset` are structurally incapable of
      holding a substituted value (`Ast.group_by_item` is
      `string * string option`; the limits are `int option`). `order` could be,
      and is not, for two reasons: sqlite3 *also* refuses a correlated
      reference in a subquery's ORDER BY (`no such column: o.n`,
      oracle-checked on 3.45.1), so rewriting it would create a divergence
      rather than remove one; and an ORDER BY key may name an **output alias**
      (#489/#663), which `inner_scope_of`'s `has_col` knows nothing about — so
      a subquery whose alias collided with an outer column name would have its
      sort key rewritten to a constant and silently return unsorted rows. That
      is the "plausible wrong answer" class this area exists to prevent, and it
      is worse than the refusal it would replace. Pinned as refusals in
      `test/test_outer_ref_732.ml`.
    - Widening the substituter widened the detector by construction, which is
      the intended direction: a statement can only move from "evaluate eagerly"
      to "treat as correlated". The shape that shows it is a free reference
      appearing **only** in the inner projection — `EXISTS (SELECT o.n FROM i)`
      used to bind cleanly, be evaluated before any outer row existed, and be
      refused.
    - #670's own file kept a boundary case asserting the aggregate spelling was
      *refused*, with instructions to replace rather than delete it if a later
      change resolved it. #721 is that change and the case now asserts the
      answer. Full coverage lives in `test/test_outer_ref_721.ml` and
      `test/test_outer_ref_732.ml`.
- **A comma in FROM is an INNER join on the literal `1`; a derived table is a
  CTE (#486, decided 2026-09-03).** `SELECT * FROM a, b WHERE a.x = b.y` and
  `SELECT * FROM (SELECT x FROM a) AS t` did not parse at all, and between them
  they account for ~18 of the 20 TPC-H queries #482 could not run. Both are
  closed **in the parser**, with no new `Ast` node and no new kind of FROM item
  for the layers below to learn.

  **The comma.** An implicit join has no ON clause of its own — its restriction
  lives in WHERE — so at the join itself the pairing is unrestricted. That is
  spelled as the literal `1` (`Parser.cross_join_on`) rather than as a third
  `Ast.join_kind`, which keeps every existing consumer of `Ast.join_clause`
  correct by construction. `CROSS JOIN` is the same production with the keyword
  spelled out, and comes out free. A hand-written `JOIN b ON 1` is
  indistinguishable from the sugar and is treated identically — right, because
  both say the same thing.

  **`FROM a, b WHERE a.x = b.y` must not become a cartesian product, and that
  is a planner change, not a parser one.** `general_on_join` would have built
  the whole product and filtered it above — O(N*M) on exactly the queries this
  exists for. `Planner.on_is_trivially_true` recognises the literal and
  `Planner.where_join_key` hands the join **one WHERE equality that spans its
  two sides**, so the implicit spelling plans to the same keyed hash join (or
  nested-loop probe) the explicit one does. It cannot change the answer: the ON
  is true for every pair and `chain_joins` applies the whole WHERE above the
  join anyway, so joining on a conjunct of that same WHERE emits a subset of
  the same product and every pair removed is one the filter above would have
  removed — NULL keys included, since `a.x = b.y` is unknown, hence false, on
  them. The chosen conjunct **stays** in the WHERE clause and is evaluated
  twice; that is deliberate, and removes any need to reason about which
  conjuncts the join consumed.

  **It is INNER-only and must stay so.** For a `Left` join the ON predicate
  *is* the match test (#552), so narrowing it would suppress null-extended rows
  that must be emitted. `where_join_key` is never consulted there.

  With nothing to borrow, the join stays the cartesian product it genuinely is
  — and `general_on_join` no longer wraps it in an `Op_filter` evaluating the
  constant `1` once per row. `plan_join_on_literal` in `test/test_planner.ml`
  used to pin that filter and now pins its absence; `plan_join_general_on`
  beside it still pins the filter for an ON that is not trivially true, which
  is the arm the literal case used to stand in for.

  **The derived table.** `(SELECT …) AS t` desugars into a non-recursive CTE
  wrapped around the SELECT that names it — the shape `Sema.expand_views`
  already builds for a view named in FROM position (#496/#497), so nothing
  downstream is new. **The alias becomes the CTE's NAME and the FROM item
  carries no alias of its own**, which is the whole of the #635 scope story
  here: a derived table has no underlying name for an alias to replace, so all
  three levels (`Sema.from_ident`, `Exec.inner_scope_of`, `Exec.scan_ident` /
  `get_outer_scan_metas`) answer the alias with no special case. Do not give
  the FROM item both a name and an alias — that is the drift the #635 rule
  exists to prevent.

  An **unaliased** derived table is legal (sqlite3 accepts it) and is named
  `__derived_<byte offset in the statement>`: unique within a statement, stable
  across re-parses of the same text. It is nonetheless a plain identifier, so a
  table literally called `__derived_12` would be shadowed for that statement.

  Three things had to move with it, and each was a silent failure before:
  - `lift_compound_tail` recurses through `S_with_cte`. The wrapper sits
    OUTSIDE the `S_select`, so a compound's trailing ORDER BY is no longer at
    the root of its right arm; without the recursion
    `SELECT … UNION SELECT … FROM (…) t ORDER BY x` sorted only the right arm.
  - `Ast.rename_view_columns` grew an `S_with_cte` arm. Its absence was
    deliberate and correctly reasoned at the time — no view body could be an
    `S_with_cte` — and stopped being true here. The prerequisite its comment
    named, a matching arm in `Sema.col_names_of_ast_stmt`, has existed since
    #491. An explicit `WITH` view body is still a syntax error; only the
    desugaring reaches this.
  - **`CREATE REACTIVE VIEW` over a derived table is REFUSED**
    (`Sema.reject_reactive_derived_table`). `Reactive_view.base_tables_of`
    reads the base tables off the `S_select` at the root of the body and
    answers `[]` for anything else, so the view would be registered with
    nothing to invalidate it and would serve its first snapshot forever, with
    no error. A plain `CREATE VIEW` is unaffected — its body is re-bound on
    every use. Lifting the refusal means teaching `base_tables_of` to look
    through the wrapper **and** to collect the CTE definition's own tables;
    both halves, or the same silence returns.

  **Accepted limitation, and it is not new: a subquery correlated to a derived
  table is refused.** `Exec.get_outer_scan_metas` resolves an outer input from
  the *plan*, and a CTE scan is materialized into `Op_pragma_rows` before the
  filter runs — an op carrying no `Cat.table_meta` and so no scope identifier.
  A plain `WITH c AS (…) SELECT … WHERE EXISTS (… c.y …)` is refused
  identically on the tree *before* #486, and
  `a_subquery_correlated_to_a_derived_table_is_refused` in
  `test/test_from_list_derived_486.ml` asserts the two refusals are the *same
  message* so the pairing cannot drift. sqlite3 answers it. The refusal is loud
  rather than a wrong answer, which is what keeps it a limitation; if
  `Op_pragma_rows` ever learns its source, that test should start passing as an
  answer and must be rewritten, not deleted.

  **Two smaller divergences from sqlite3, both deliberate.** A column-alias
  list on a derived table (`AS t(c1, c2)`) is a syntax error — so it is in the
  sqlite3 in the dev image, oracle-checked, and TPC-H Q13's spec spelling needs
  the same rewrite there. Nested parentheses in FROM (`FROM ((SELECT 1))`) are
  a syntax error where sqlite3 accepts them; distinguishing `(a)` from
  `(SELECT …)` at an arbitrary paren depth is not worth a FROM-position
  conflict.

  **Grammar cost.** The FROM productions add **zero** menhir conflicts. The
  count moved 290 → 292 solely because `CROSS` joined `any_ident`, putting it
  in the token set of two pre-existing `CREATE TABLE … DEFAULT <keyword>`
  conflict states; the state count is unchanged at 35. Like sqlite3, `CROSS` is
  reserved in bare-alias position (`FROM a cross` is a syntax error in both)
  but usable as a table name and after `AS`.
- **A reactive view must project explicitly; `SELECT *` is refused
  unconditionally (#747, decided 2026-09-03).** This is a **behaviour break**:
  `CREATE REACTIVE VIEW v AS SELECT * FROM t` used to be *accepted* whenever `t`
  had at least one row, and anybody relying on that must now name the columns.

  The old guard lived at runtime in `Db.rv_create`: the view's arity was read
  off the first row of the initial result, and only an arity of **zero** — an
  empty result — was refused, with a message that said "empty result and
  SELECT *; use an explicit projection". So the guard *read* as "star is
  refused" while really being "star is refused when we cannot guess a shape",
  and the accepted case degraded badly: the view froze its column list at
  creation time, thereafter silently dropped any column a later `ALTER TABLE …
  ADD COLUMN` added, and fired on **every** write to the base table including
  writes that were idempotent for the columns it actually projects. A
  downstream consumer (camel's hook loader) documented the engine as refusing
  `SELECT *` and had a tripwire test that only exercised the empty case, so it
  believed it was protected against something it was not.

  The refusal is now **static** — `Sema.reject_reactive_star`, alongside
  `reject_reactive_derived_table` and running *after* it, so #486's more
  specific message keeps precedence for a derived-table body. Consulting the
  data was the defect, so nothing in the check does.

  **What it covers, and what it deliberately does not.** The walk
  (`Sema.reactive_projects_star`) follows the statement's **output**
  projection: an `S_select` whose `proj` is `` `All ``, either arm of an
  `S_compound`, and an `S_with_cte`'s body — the last two latent, since #750 and
  #486 refuse those roots first. It does **not** descend into a CTE
  *definition* or a subquery, because a star there does not determine the
  view's own arity — `SELECT a FROM (SELECT * FROM t) d` yields exactly one
  column whatever `t` grows, and `WHERE EXISTS (SELECT * FROM u)` is a row
  test. A **qualified** star (`t.*`) needs no arm: the grammar has no
  `DOT STAR` production at all, so it is a parse error in every statement,
  reactive or not (verified against the engine's own CLI; sqlite3 is not an
  oracle here — reactive views are granary-specific).

  **A plain `CREATE VIEW … AS SELECT *` is untouched, and that is the whole
  basis of the decision.** It is not maintained; its body is re-bound on every
  use, so it widens with the base table on the next read. Pinned by
  `a_plain_create_view_with_a_star_is_unaffected` in
  `test/test_reactive_view_star_747.ml`, which asserts the widening rather than
  assuming it.

  Two residuals, both deliberate:

  - **`Db.rv_load` is not gated.** It re-parses the persisted `CREATE REACTIVE
    VIEW` text directly and never goes through `Sema`, so a star view created
    before this fix still loads and behaves exactly as it did. Gating it would
    brick *opening* the database rather than fixing the view; `DROP REACTIVE
    VIEW` is the repair.
  - **The arity-zero guard in `Db.rv_create` stays**, but its message no longer
    mentions emptiness or `SELECT *`, because emptiness stopped being the
    criterion. With the star refused it is unreachable for an `S_select` root
    (`Reactive_view.out_cols_of_proj` answers `Some` for both non-star
    projections), and #750 closed the `S_compound` root. **Exactly one spelling
    still reaches it**, and it is worth knowing because it is not a compound: a
    FROM-less `SELECT *` parses to `S_const_select { exprs = [] }`, which is
    neither an `S_select` (so the star check never sees it) nor backed by a
    table (so nothing could widen it). Verified against the engine's CLI.
- **A compound-root reactive view (`UNION`/`UNION ALL`/`INTERSECT`/`EXCEPT`) is
  refused (#750, decided 2026-09-03).** Another behaviour break, and the one
  with the worst pre-fix symptom of the three: the view was **created without
  error, materialised correctly once, and then served that first snapshot
  forever**. No error at any point.

  `Reactive_view.base_tables_of` reads the base tables off the `S_select` at the
  root of the body and answers `[]` for everything else, so a compound view
  registered with **no base tables** and no write to either arm's table ever
  marked it dirty. Reproduced, not merely reasoned about:

  ```sql
  CREATE REACTIVE VIEW cv AS SELECT a FROM t UNION SELECT b FROM u;
  SELECT * FROM _rv_cv;   -- 1, 2
  INSERT INTO t VALUES (99);
  SELECT * FROM _rv_cv;   -- 1, 2      *** 99 silently missing ***
  ```

  **This was the third instance of one pattern, and naming the pattern is the
  point**: a `Reactive_view` helper matches `S_select` and answers a
  benign-looking default for every other constructor. #486 found it in
  `base_tables_of` (derived table), #747 in `proj_of` (`SELECT *`), and this is
  `base_tables_of` again. All three are closed the same way — refuse the shape
  at bind time, because a maintained view needs a statically determinable one.
  A **fourth** instance is the thing to look for if a new root constructor ever
  becomes reachable as a view body.

  **Supporting it properly is a separate decision, not an omission.** It needs
  `base_tables_of` to union both arms **and** a correct incremental rule per set
  operation, and neither `UNION` (distinct) nor `EXCEPT` is an additive merge
  over Z-sets — whether a row leaves the result when one arm loses it depends on
  the other arm's multiplicity. Doing the first half alone would replace a stale
  view with a **wrong** one, which is worse.

  **Precedence: body-shape refusals run before projection ones.** The chain in
  `Sema`'s `S_create_reactive_view` arm is `reject_reserved_name` →
  `reject_reactive_derived_table` (#486) → `reject_reactive_compound` (#750) →
  `reject_reactive_star` (#747). A caller who hits an outer rule cannot fix it
  by editing the inner one, so the outer message is the useful one:
  `SELECT * FROM t UNION SELECT b FROM u` reports #750, not #747, and a derived
  table inside a compound *arm* also reports #750 because the desugaring wraps
  the arm and leaves `S_compound` at the root. Both directions of each boundary
  are pinned, in `test/test_reactive_view_compound_750.ml` and in
  `test/test_reactive_view_star_747.ml` — the #747 file's compound case was
  **rewritten rather than deleted**, because it is the only thing pinning which
  of the two messages a reader sees.

  **`Db.rv_load` is still not gated, and that was verified rather than
  assumed** — a database containing a compound reactive view was built against a
  probe binary with the check disabled, then reopened against the fixed one. It
  opens; the view is still there; `DROP REACTIVE VIEW` removes it. One detail
  makes the pre-fix behaviour *harder* to diagnose than "always stale": `rv_load`
  ends with a `rv_refresh_one ~resync:true` pass, so the materialisation
  **self-heals at every open** and then goes stale again on the next write. A
  user who restarts the process sees correct data and concludes the problem went
  away.

  **A plain `CREATE VIEW … AS SELECT … UNION SELECT …` is untouched**, for the
  same reason as #747's plain-view carve-out: it is not maintained, its body is
  re-bound on every use. Asserted, not assumed.

  **What still reaches the `| _ -> []` / `| _ -> None` arm once compounds are
  refused**: exactly one constructor, `S_const_select` — a FROM-less `SELECT`.
  Both defaults are *correct* there rather than benign-looking, because such a
  query is backed by no table: `[]` base tables is the truth and there is
  nothing that can go stale. `CREATE REACTIVE VIEW k AS SELECT 1` is therefore
  accepted and pinned as such.
- **A view is resolved at EVERY FROM position, and each subquery carries its own
  expansion (#496/#497, fixed 2026-09-03).** A view reference is desugared into
  a CTE wrapped around the statement that names it. That rewrite used to be
  applied to a statement's *leading* FROM table and to nothing else, which is
  one root cause with two faces:

  - #497 — a view named by a `JOIN` was never expanded and failed as
    `unknown table`. Same query, operands swapped, two answers, so view
    usability depended on join order.
  - #496 — a view named inside a **subquery's** FROM was neither expanded nor
    reported. That is the dangerous one, and the mechanism is the point: a
    subquery survives binding as an `Ast.stmt` and is re-bound at execution
    time by `Exec.plan_subquery_cached`, which calls `Sema.bind` with **no view
    table at all** (`Db.t` holds `t.views`; `Exec` never sees it). The bind
    failed, the failure was memoized as "this subquery has no plan", and the
    statement answered zero rows or was refused as an unresolvable
    correlation — never as "unknown table".

  `Sema.expand_views` now rewrites the whole statement: every FROM position of
  every SELECT it contains, at any depth. Three properties carry the fix and
  none of them is optional:

  - **Each subquery carries its OWN `WITH` wrapper** rather than leaning on an
    enclosing one. That is what fixes #496, because the execution-time re-bind
    must see a *self-contained* statement. `WITH` inside a parenthesised
    subquery has **no grammar** — `(WITH x AS (…) SELECT …)` is a parse
    error — but the AST node does, and `bind_internal`, `Planner.plan`,
    `Exec.to_stream` and `Exec.substitute_outer_in_stmt` all already handled
    it. Wrapping only the outer statement was tried and cannot work: the
    subquery is re-bound without the enclosing CTE registered.
  - **A name is expanded only when nothing shadows it** — not bound by an
    enclosing CTE (tracked syntactically in `scope`) and not a real table
    (`Cat.find_table_cached`, which is also what makes a `CREATE TABLE` of the
    same name keep winning). That second test is what makes the rewrite
    **terminate**: `bind_with_cte` registers the CTE as an ephemeral meta
    before binding its query, so re-entering the pass on the wrapper's own
    query finds the name registered and declines to expand it again. The pass
    is therefore idempotent, which is why it can sit at the top of
    `bind_internal` and run on every recursive bind.
  - **A cycle is refused, not expanded forever.** `CREATE VIEW` validates its
    body, so the obvious cycles cannot be built — but one can: create `v1` over
    `v2`, `DROP VIEW v2`, `CREATE TABLE v2`, then `CREATE VIEW v2 AS SELECT …
    FROM v1` (which binds against the *table*), then `DROP TABLE v2`. Before
    the guard that expands until the stack goes. `expand_views_wrap` carries the
    chain of views currently being expanded and raises `Unsupported "view '…'
    is defined in terms of itself"`.

  Consequences worth knowing before editing this:

  - **The scope identifier of an expanded view is the view's name, or its alias
    when it has one** — the #635 rule, unchanged, because the expansion hands
    the name straight to a CTE and the alias rides on the FROM item. So
    `SELECT v.x FROM v JOIN t …` resolves, `FROM v AS a` resolves under `a`,
    and `SELECT v.x FROM v AS a` is still the #635 refusal.
  - A correlated subquery over a view works because `Exec.inner_scope_of`'s
    `_` arm answers "the subquery owns everything" for the `S_with_cte` the
    expansion produces, while `substitute_outer_in_stmt` still *descends*
    into its `query` (it has had an `S_with_cte` arm since #670's neighbourhood)
    and recomputes the scope from the inner SELECT there. If `inner_scope_of`
    ever grows a real `S_with_cte` arm it must add the CTE name to `has_table`
    and delegate columns to the inner query, or a qualified outer reference
    stops being substituted.
  - `bind_internal` is now a two-line wrapper that runs the pass and delegates
    to `bind_expanded`. Putting the pass there rather than in the `S_select`
    arm is what makes the UPDATE / DELETE / INSERT arms of
    `expand_views_stmt` reachable — those binders are dispatched directly, so
    a view named in one of their subqueries would otherwise never be seen.
  - `CREATE VIEW`'s stored body is deliberately **not** rewritten
    (`expand_views_stmt` passes `S_create_view` through untouched), so the
    in-memory definition in `t.views` and the `sys_views` SQL it is re-parsed
    from on open stay the same statement. The body is expanded on every *use*
    instead. CREATE-time validation still expands it, because the arm binds the
    SELECT through `bind_internal`.
  - `col_names_of_ast_stmt`'s `S_with_cte` arm — added by #491 with a comment
    saying no path reached it — is now live: a CTE whose `def` selects from a
    view is handed to `derive_cte_meta` as an `S_with_cte`.

  Pinned by `test/test_view_resolution_496.ml`, whose expected values were
  oracle-checked against sqlite3.

- **A GENERATED column's NOT NULL is enforced on its COMPUTED value, at both
  levels (#629).** This was the third instance of the same shape as #567 and
  #599 — a check running where it cannot see the truth — and the fix narrows
  *when* each check runs, never *whether*.

  The caller is forbidden from supplying a generated column, so it is always
  omitted, and `Sema.bind_insert_row` fills an omitted column with a literal-NULL
  placeholder. Judging that placeholder made a `NOT NULL GENERATED` column reject
  **every** INSERT: no spelling could succeed, so the table was uninsertable.
  `bind_insert_row` therefore exempts generated columns of **both** storage
  classes — at bind time neither has a value. It keeps its literal-NULL error for
  every other column.

  Two neighbouring binders were out of step with that and had to move too, or
  the headline symptom survived the fix. `bind_insert`'s **implicit column list**
  now omits generated columns, so bare `INSERT INTO g VALUES (…)` works — before,
  it was `Arity_mismatch` with one value per base column and "cannot INSERT into
  generated column" when padded out. That was not a new divergence decision: the
  `INSERT … SELECT` binder's `columns = []` arm already applied exactly that
  filter, and `Db.dump` already emits an explicit list that omits them; the
  VALUES binder was the odd one out. And `bind_upsert_assignments` now refuses
  `DO UPDATE SET <generated> = …` the way `bind_update_assignments` always did —
  it was the third assignment spelling and the only unguarded one, so the
  assignment bound fine and `compute_stored_generated_cols` overwrote the column
  immediately after, making the statement silently do nothing.

  The runtime half had the mirror-image gap. `Exec.not_null_exempt_col` exempted
  VIRTUAL generated columns outright (#567's reasoning: their stored cell is
  `V_null` by design), which meant a NOT NULL VIRTUAL column was *unenforceable*.
  `Exec.not_null_violation` now recomputes the virtuals into a copy of the row
  before judging it, and consults the exemption only when it cannot — a row that
  does not cover every column, where evaluating the generated expression would
  raise. **The exemption is the degraded mode; do not re-broaden it.**

  **STORED columns rest on an ordering claim, and the claim covers the
  ROW-STORE sites only** — do not restate it as "every enforcement site", which
  is how it shipped and is false. `compute_stored_generated_cols` runs before
  the check at `execute_insert`, `execute_upsert_update`, `update_col_in_tx` and
  `apply_update_row`. It is **not** called on either columnar arm: both build the
  row with `Array.make n_cols Row.V_null` and pass it straight to
  `not_null_skip_or_fail`. Nor does `stream_col_seq_scan` recompute VIRTUAL ones
  on the way out. So a generated column on a columnstore table read NULL forever
  in **both** classes, silently. `Sema.bind_create` now **refuses** a GENERATED
  column on a `USING COLUMNSTORE` table (#660) — which is what makes the ordering
  question not arise at those two sites, rather than a claim that it was already
  answered there. Wiring the computation into the write arms alone was rejected:
  it fixes STORED and leaves VIRTUAL reading NULL, deepening the asymmetry.
  #660 has since decided to **keep** that refusal — see the next entry — so if
  it is ever lifted, both halves are owed at once.

  **#661 (fixed): `ALTER TABLE ... ADD COLUMN ... NOT NULL GENERATED ... VIRTUAL`
  is no longer refused, and it is #629 that makes lifting the refusal safe.**
  `Sema.bind_add_column`'s NOT NULL gate exists because existing rows decode
  SHORT — they were written without the new column, so it reads back as a stored
  NULL, and a DEFAULT is the only thing that can supply a value for them. That
  reason holds for a plain column and for a **STORED** generated one, which has
  no read-side recompute for rows written before the ALTER; both keep the
  refusal. It does not hold for a **VIRTUAL** one, which has no stored cell at
  all, so the exemption relaxes *when* the constraint is checked and not
  *whether* — `not_null_violation` recomputes the virtuals before judging, so
  the added column is genuinely enforced at write time.

  **The ALTER deliberately does NOT validate the expression over EXISTING
  rows** — the judgement call #661 leaves open. `bind_add_column` is a binder
  with no store, so the check would have to move into the exec ALTER path and
  would turn an O(1) metadata-only DDL into a full table scan; and it matches
  how pre-existing constraint violations are treated generally, reported by
  `PRAGMA not_null_check` rather than rejected at DDL time. **The residual is
  real and has no repair surface of its own**: a row already on disk whose
  generated expression is NULL reads its NULL silently and raises only on the
  next write that rewrites it (`write_row_rekeyed` → `enforce_not_null`), and
  `not_null_scan_cols` deliberately does not cover generated columns per the
  paragraph above, so `PRAGMA not_null_check` will not report it either. Moving
  the base column is the only repair. Both halves are pinned by
  `test/test_alter_add_generated_661.ml`.

  Consequences worth knowing: a generated expression that genuinely evaluates to
  NULL is still rejected — on INSERT (skipped under `OR IGNORE`, per #599, since
  the row now reaches the runtime site that decides that), and on an UPDATE of
  the base column it reads (always raises; `write_row_rekeyed` has no `OR IGNORE`
  form). `not_null_scan_cols` (`PRAGMA not_null_check`/`repair`) deliberately did
  **not** follow — it reports on cells already on disk whose only repair is to
  rewrite them, which is meaningless for a column never read from disk. Pinned by
  `test/test_not_null_629.ml`.
- **A GENERATED column on a `USING COLUMNSTORE` table stays refused at DDL
  (#660, decided 2026-09-03).** This closes the question the #629 entry above
  leaves open: the refusal *is* the answer, not a placeholder for wiring the
  computation in. `Sema.bind_create`'s `using_columnstore` branch scans the
  column definitions for `generated_as <> None` and returns `Unsupported
  "GENERATED column '<c>' in a COLUMNSTORE table (the columnar path never
  computes one; it would read NULL forever)"`. It is the only entry point that
  needs a guard: `Sema.bind_alter_table` refuses **every** `ALTER TABLE` on a
  columnar table outright, so `ADD COLUMN ... GENERATED` cannot reach one by
  the back door.

  **Wiring it up was rejected because it is two changes, and either one alone
  is worse than the refusal.** Supporting generated columns here needs both of:
  - STORED — `compute_stored_generated_cols` called on both columnar write
    arms, `Op_insert` and `Op_insert_select` in `Exec.execute_with_count`, which
    build the row with `Array.make n_cols Row.V_null`, fill only the
    caller-supplied ordinals, and hand it straight to `Col_store.insert_rows`;
  - VIRTUAL — a recompute on the read side, since `Exec.stream_col_seq_scan`
    returns `Col_store.to_row_seq` rows verbatim.

  Doing the write half alone fixes STORED and leaves VIRTUAL reading NULL on
  every scan: the asymmetry deepened rather than removed, and still a silent
  wrong answer rather than an error. The refusal, by contrast, makes #629's
  enumerated NOT NULL invariant hold at the two columnar enforcement sites
  vacuously — with no generated column reachable there, "STORED is materialised
  before the check" is true because there is nothing to materialise.

  **What the refusal costs, against what the silence cost.** Before #629 nothing
  rejected the DDL, so such a column read NULL forever in *both* storage
  classes with no error ever surfaced, and a `NOT NULL` one made the table
  uninsertable. There is no migration concern for existing databases: any such
  table already held nothing but NULLs in those columns.

  **What a future implementer owes if the refusal is lifted.** Both halves in
  the same change — not the write arms first — plus any other columnar path
  that hands rows out. The columnar store has no per-row decode hook to hang a
  recompute on, so every read site has to opt in individually, which is the
  "enumerate the sites, do not state a rule" trap #567 documented and the reason
  a partial implementation is not an improvement on refusing. `RETURNING` is
  *not* one of those sites today: all three columnar `RETURNING` arms in
  `Exec.to_stream` fail with `"RETURNING is not supported on columnar tables"`,
  so it owes nothing while that stands — and joins the list the day it does not.
  Pinned by `columnstore_refuses_generated_columns` in
  `test/test_not_null_629.ml`.
- **`PRAGMA not_null_repair` is a write; `PRAGMA not_null_check` is a read
  (#588, fixed 2026-09-03).** The repair DELETEs rows but was dispatched as a
  query, so `Db.execute db "PRAGMA not_null_repair"` answered
  `Exec.execute: use Exec.query for read operations` and deleted **nothing** —
  the natural call for a mutating statement silently did nothing but error,
  while the read API was the one that actually destroyed data. Both entry
  points now perform it and they differ in what they hand back: `Db.execute`
  reports the **rows deleted** as the statement's change count (deduplicated —
  a row violating two NOT NULL columns is counted under both in the report and
  deleted once), `Db.query` streams the per-column `(table, column, count)`
  report. Removing the query path was rejected: it is the only way to *see* the
  report, which is #563's whole point. `not_null_check` stays on the query path
  alone, so the two PRAGMAs differ in call shape the way they differ in effect.

  The mechanics: `Exec.execute_with_count` gained an `Op_pragma_not_null_repair`
  arm reaching the shared core `not_null_repair_run` through
  `not_null_repair_run_ref`, the same forward-reference idiom `to_stream_ref`
  already uses for `Op_insert_select` (the core lives in the recursive block
  below `execute_with_count`). `repair_not_null_table` returns
  `(report rows, rows deleted)` rather than just the rows.

  **The read-only half of the issue.** A repair reached under `query_as_of` (or
  any `In_ro_txn`) used to fail with `write attempted under a read-only
  transaction (In_ro_txn)` — a storage-layer message about an internal mode,
  from a statement whose problem is that it is destructive. `not_null_repair_run`
  now refuses at the statement level and names `PRAGMA not_null_check`, which
  *does* work against a snapshot and is deliberately still served there.

  **What was NOT done, and why.** #588 asks, as a consequence, that
  `repair_not_null_table`'s columnstore arm "go back to being a raise" — a
  columnstore is append-only, so its violations cannot be deleted, and the arm
  reports a count-`0` row by convention instead. The raise is now *expressible*
  (#627 fixed `Db.query_impl`'s guard, and the write path always converted a
  `Failure`), but it is still not taken: the raise would happen partway through
  the per-table `Lwt_list.map_s` and `not_null_repair_run` rolls the whole
  repair back, so one unrepairable columnstore would make the entire database
  unrepairable — and there is no per-table spelling of this PRAGMA to fall back
  on. That trades a convention an operator can act on for a refusal they
  cannot. Anything reopening this owes a per-table repair first, or a pre-pass
  that refuses *before* deleting anything. Pinned by
  `test/test_not_null_repair_588.ml`.
- **`Db.dump`'s #548 NOT NULL refusal points at the PRAGMAs (#583, fixed
  2026-09-03).** The refusal is raised from inside the row stream, so it names
  the violation it *stopped on*, not the scope. Hand-writing the repairing
  `DELETE` for that one `(table, column)` made an operator repair table by
  table off successive dump failures — exactly what #563's report mode exists
  to prevent. The message now leads with `PRAGMA not_null_check` (the whole
  scope in one pass) and `PRAGMA not_null_repair`, and keeps the hand-written
  `UPDATE`/`DELETE` as the manual escape hatch: #548 refuses in order to stop
  information being destroyed silently, so the non-destructive `UPDATE` must
  stay visible. `~data_only:true` is still named as the way to get rows out of
  an unrepaired file. Pinned by `test/test_dump_not_null_check_583.ml`;
  `test/test_dump_null_pk_548.ml` still asserts the manual statements are
  present, so the demotion cannot become a deletion.
- **An aggregate's ARGUMENT is an expression, and both rules that follow from
  that are now settled (#665 and #664, 2026-09-03).** #488 made
  `SUM(price * (1 - disc))` legal; these two are the consequences it did not
  finish.

  **#665: the SUM/AVG numeric check reaches the expression spelling.** #568 had
  already unified the *bare* `SUM(text_col)` and the *wrapped*
  `SUM(text_col) + 0` on one bind-time check, precisely so that a pair of
  parentheses could not turn it off. #488's expression argument was a third
  spelling with no column ordinal, so `agg_numeric_check` — which keys off a
  stored column's declared type — never ran on it:
  `SUM(CASE WHEN … THEN 'a' ELSE 'b' END)` failed at RUNTIME mid-scan, and
  **succeeded outright on an empty table**, which is the exact defect #568 was
  filed about. The verdict now lives in one function,
  `Sema.agg_numeric_ty_check`, reached by both spellings.

  **`Sema.agg_arg_static_ty` is deliberately NOT `Sema.infer_type`, and the
  reason is a compatibility break avoided rather than a style preference.**
  `infer_type` indexes a `Row.column list` where the binder has only the
  resolver's `agg_arg_col_ty` ordinal lookup; more importantly it must not
  descend into `BE_case`, because its other caller is `bind_update_assignments`
  and Granary's typing there is strict (`ty_equal Integer Real = false`), so a
  CASE arm would newly reject
  `UPDATE t SET real_col = CASE WHEN c THEN 1 ELSE 2 END`. The CASE descent is
  the whole point for #665 — the issue's headline shape is a CASE — so it lives
  in the aggregate-only function. Every arm that cannot be sure answers `None`,
  and `None` is `Ok`, so the check can only move a failure EARLIER; it can never
  refuse a query that would have answered. The residual it does not close: an
  argument whose type is statically indeterminate (a scalar function, a bound
  parameter, a CASE whose arms disagree) still reaches the runtime accumulator,
  and on an empty table still succeeds. Pinned by
  `test/test_agg_numeric_665.ml`, whose
  `indeterminate_argument_still_fails_at_runtime` is the boundary marker.

  **#664: a subquery in the argument is evaluated, not refused — and it does
  NOT follow #558's rule.** This is the part to read before touching either
  path. #558 pre-evaluates subqueries in `having` and `proj`; those are
  evaluated per aggregate **OUTPUT** row, whose only input-derived slots are the
  grouped columns, hence #558's rule that a correlated subquery there may
  reference a GROUP BY column and **nothing else**. An **ARGUMENT** is evaluated
  per **INPUT** row, before any grouping, so its outer reference may name any
  column the child carries; it is resolved the way `stream_expr_project`
  resolves a correlated projection, against `get_outer_scan_metas child`, per
  row. Two rules for two different rows. They have separate refusal messages
  (`agg_subquery_refusal` vs `agg_arg_subquery_refusal`) for exactly that
  reason: reporting the GROUP BY one for an argument sends the reader to a rule
  that does not apply to it. **Unifying the two would be a silent wrong answer**,
  not a simplification — `correlated_under_a_group_by_on_another_column` in
  `test/test_agg_arg_subquery_664.ml` is a query #558's mechanism could not
  answer at all.

  Mechanically the resolved argument VALUE is parked in a hidden trailing slot
  appended to its input row and the spec rewritten to `P_col slot` — the same
  hidden-slot mechanism #495/#663 use. That is what keeps the subquery evaluated
  exactly once per row: #491's DISTINCT filter and `aggregate_one` both read the
  argument through `agg_arg_getter`, so a `P_subquery` left in place would have
  had to be resolved separately by each. Widening is safe because everything
  that indexes an input row there indexes a PREFIX of it, and the aggregate
  output row is `group_key @ agg_vals`, so no hidden slot escapes into a result.

  **The #247 fast path gives such a query up**, gated above its child dispatch
  so both `run_aggregate_fast_path` and #674's `run_index_cover_walk` are
  covered. Its loop is pure and `eval_expr` answers `V_null` for an unresolved
  `P_subquery`, so keeping the query would silently fold NULLs — the same reason
  #558 made it give up a subquery-bearing projection. Every scalar assertion in
  #664's test runs with the fast path forced ON and forced OFF and must agree,
  so a fast path that quietly kept such a query shows up as a disagreement
  rather than as a plausible number.

  `Sema.expr_has_subquery_ast` is gone with the refusal that was its only
  caller. Three tests that asserted the refusal now assert the answer
  (`test_agg_expr_495_488`, `test_agg_subquery_558`, `test_sema`); they were
  converted rather than deleted so the moved boundary is visible in the diff.

- **An explicit `ON CONFLICT` target beats the statement's conflict-resolution
  modifier, for the index it names (#639, decided 2026-08-06).** The modifier
  still governs every *other* index. Before this, `CA_ignore` matched above the
  upsert arm in both places that resolve a conflict — `check_insert_unique` for
  a secondary UNIQUE index and `execute_insert_write`'s `put_x` arm for the
  rowid-alias PK — so `INSERT OR IGNORE ... ON CONFLICT(k) DO UPDATE` silently
  skipped for *any* conflict: a caller who wrote both got "insert, or do
  nothing".

  **The target is resolved in its own pass, before the modifier sees anything,
  and that is what makes the answer well-defined rather than a micro-
  optimisation.** `check_insert_unique` used to fold over every unique index in
  one pass and let whichever conflicted first decide. With two unique indexes
  and a row conflicting on both, the outcome then depended on the order
  `Cat.indexes_for_table` returned them in — newest-first, i.e. on `CREATE
  UNIQUE INDEX` order: target first gave an upsert, the other first gave a skip
  under `OR IGNORE`. Same schema, same statement, two answers. When the target
  hits, the accumulator is reset to `(false, [], Some rid)`: `skip` is
  meaningless because the insert it was decided against is discarded, and
  `dels` **must** be empty because `execute_insert`'s upsert branch never calls
  `delete_replace_conflicts` — a non-empty `dels` there is a queued delete that
  never happens.

  **What this pass does NOT do is hand the check downstream — `write_row_rekeyed`
  itself performs no uniqueness probe.** Its index loop is still an unconditional
  `S.del` of the old key and `S.put` of the new one; see the `#667 (fixed)`
  paragraph immediately below for where that check now lives instead.
  Discarding the target pass's other-index verdicts here is sound regardless —
  they were computed against the row being INSERTED, which is discarded, and a
  `SET v = 42` does not touch the column they were about.

  **#667 (fixed): a DO UPDATE that writes a duplicate into another unique index
  now raises, via a check at the `execute_upsert_update` call site rather than
  inside `write_row_rekeyed`.** `write_row_rekeyed` is shared by three write
  paths — plain `UPDATE`, `UPSERT ... DO UPDATE`, and `ON UPDATE CASCADE` — so
  pushing the probe inside it would have changed the other two as well; that is
  a separate decision (see the `enforce_not_null` bullet above, which makes the
  same point). `execute_upsert_update` calls the new shared helper
  `check_indexes_unique_on_update` — one `Lwt_list.iter_s` over
  `Cat.indexes_for_table` calling `check_index_unique_on_update` per index —
  before calling `write_row_rekeyed`, passing it the already-computed
  `new_row_for_idx` again via `write_row_rekeyed`'s new `?new_row_for_idx` so
  the VIRTUAL-column evaluation isn't paid twice. `check_index_unique_on_update`
  already excludes the row being updated from its own conflict probe (by
  rowid) and exempts an unchanged key and a NULL-containing key (#290), so
  this includes the conflict-target index itself — a DO UPDATE that moves the
  very column named in `ON CONFLICT(...)` to a value a third row already
  holds is caught too, not just a DO UPDATE touching an unrelated index.
  `validate_update_unique`, the plain-UPDATE pre-pass, now calls the same
  `check_indexes_unique_on_update` helper instead of hand-rolling an identical
  loop — the two call sites cannot drift apart on a future change (a new
  exemption, an early exit, batching) the way the first revision of this fix
  would have let them.

  Two things about `check_index_unique_on_update` moved as part of this fix
  are correctness-bearing, not just relocation:

  - It (and `check_indexes_unique_on_update`) moved earlier in `exec.ml`, to
    just before `write_row_rekeyed`, so `execute_upsert_update` — defined
    before their original position — could call them.
  - `unique_violation_on_update`'s `unchanged` fast path used to compare only
    the indexed COLUMN values (`old_vs` vs `new_vs`), never whether `old_row`
    had matched the index's `idx_where_sql` at all. A row flipping into a
    PARTIAL unique index's domain without touching the indexed column — e.g.
    `active` going 0 → 1 under `UNIQUE INDEX ... WHERE active = 1` while `a`
    stays the same — read as "nothing moved" and skipped the probe, so
    `write_row_rekeyed` inserted a second live entry for a key another row
    already held under that index. `unchanged` now also requires `old_row` to
    have matched the WHERE clause. Pre-existing in `validate_update_unique`'s
    path too (same shared primitive) — fixed there for free by fixing the one
    function both now call.
  - `unique_violation_on_update` also used to recompute the row's index
    values independently, via `with_computed_virtuals_cols None [||]` —
    hardcoding a fresh clock and no bound params instead of reusing the ones
    its only caller, `check_index_unique_on_update`, had already evaluated
    `new_vs` with a few lines above. For a UNIQUE index over a
    clock-dependent VIRTUAL column that let the probe's seek key diverge from
    the value just validated. It now takes the already-computed `key_vals`
    directly and does no evaluation of its own, which removes the second
    computation entirely (a `#667` review finding on top of the `unchanged`
    one) rather than just aligning its inputs.

  Pinned by `test/test_upsert_unique_667.ml`, covering both conflict shapes
  (secondary UNIQUE index and rowid-alias PRIMARY KEY, since both funnel
  through `execute_upsert_update`), a plain-UPDATE regression check, and the
  partial-index WHERE-transition case for both the UPSERT and plain-UPDATE
  paths.

  **#693 (fixed): `ON UPDATE CASCADE` / `SET NULL` / `SET DEFAULT` — the third
  `write_row_rekeyed` caller named above — had no uniqueness probe at all, not
  even the imprecise one #667 fixed for upserts.** `update_col_in_tx` now
  calls the same shared `check_indexes_unique_on_update` before
  `write_row_rekeyed`, with `~clock:None ~params:[||]` (its existing constants
  — a cascade runs outside any statement's clock/params scope) and hands the
  already-computed `new_row_for_idx` through via `write_row_rekeyed`'s
  `?new_row_for_idx`, exactly as `execute_upsert_update` and
  `validate_update_unique` do. All three `write_row_rekeyed` callers now run
  the identical pre-write probe. `SET NULL` can never trip it — #290 exempts
  any NULL-containing key — but `SET DEFAULT` can, when the column's default
  collides with a value another live row already holds; pinned alongside the
  issue's own CASCADE repro in `test/test_cascade_unique_693.ml`.

  One consequence of the target pass is a change for the *raising* modifiers:
  bare / `OR ABORT` / `OR FAIL` / `OR ROLLBACK` used to report
  `UNIQUE constraint failed` for a second index the discarded insert row
  collided with, and now run the DO UPDATE. That was a false positive — the
  conflicting row is never written — and it is pinned by
  `raising_modifiers_no_longer_report_the_discarded_rows_conflict`.

  The **rowid-alias PK** is the other thing an ON CONFLICT clause can name, and
  it has no index, so it is invisible to that fold. When the clause names it,
  `execute_insert` probes the row key with one `S.get` *before*
  `check_insert_unique` — otherwise a secondary conflict would set `skip`
  (losing the DO UPDATE) or run `delete_replace_conflicts` (displacing rows for
  an insert that then never happens, because `put_x` discovers the alias
  conflict afterwards). The probe is only paid when an upsert clause names the
  alias column, so #350's plain-INSERT path is untouched. The arm in
  `execute_insert_write` is kept as a backstop, not the primary path.

  **Two things ride on that pre-probe, both decided rather than incidental**,
  because routing a conflicting row to the upsert branch skips
  `execute_insert_write` entirely and that function did more than one job:

  - **The insert row's NOT NULL check moved up, for upserts only.** It is now in
    `execute_insert`, entered when the statement is `OR IGNORE` *or* carries an
    upsert clause. Without that, `INSERT INTO t VALUES (1, ?) ON CONFLICT(k) DO
    UPDATE …` bound to NULL raised on `main` and silently became a successful DO
    UPDATE. The rule stands: an `ON CONFLICT` clause never intercepts a NOT NULL
    violation. The *secondary-index* shape is the one that changed to agree — it
    never raised here — and a statement with **no** upsert clause is untouched,
    including the precedence between a UNIQUE error and a NOT NULL one.
  - **`last_insert_rowid()` is no longer set by a DO UPDATE.** No row was
    inserted, so it should not move. Before #639 the alias-PK shape set it (it
    returned through the INSERT branch) and the secondary-index shape never did —
    the same "answer depends on which constraint you hit" split #639 is about.
    `test_upsert_on_pk_last_rowid` in `test/test_rowid_alias.ml` asserted the old
    answer **vacuously** (it seeded rowid 5 and upserted rowid 5, so the seed's
    own value satisfied it); it now seeds 7, upserts 5, and pins the new one. The
    comment it carried claimed SQLite parity for the opposite answer and was
    never oracle-checked; the reasoning runs the other way (SQLite sets the value
    at `OP_Insert` under `OPFLAG_LASTROWID`, and a DO UPDATE is generated as an
    UPDATE). If the oracle disagrees, set it in **both** shapes — do not restore
    the split.

  **An unremarked improvement, recorded so nobody finds it by bisect:** the
  pre-probe also removes a bogus #417 delta. The old alias-PK upsert path emitted
  *both* an `Updated` and a phantom `Inserted { rowid; row }` — with `row` being
  the *attempted insert* row, not the stored one — so any reactive view over the
  table saw a row that was never written. The upsert branch emits only `Updated`.

  **A conflict target that names no PRIMARY KEY or UNIQUE constraint is silently
  ignored, where SQLite rejects the statement** ("ON CONFLICT clause does not
  match any PRIMARY KEY or UNIQUE constraint"). Granary treats it as a plain
  INSERT and drops the `DO UPDATE`. Pre-existing, pinned by
  `a_non_unique_index_is_not_a_conflict_target`, and tracked as **#668** — it is
  #639's failure mode reached through a schema mistake instead of a modifier.
  `Exec.index_is_conflict_target` does require `idx_unique`, so a non-unique
  index can never be *promoted* into a target; what is missing is the rejection.

  **`OR REPLACE` defers to the target too, and that is a decision, not a side
  effect of the arm order (#639, decided 2026-08-06).** `INSERT OR REPLACE ...
  ON CONFLICT(k) DO UPDATE` now updates the conflicting row in place instead of
  deleting it and inserting the new one. The alternative — `CA_replace` keeping
  precedence over the named target — reproduces #639 exactly, for `REPLACE`
  instead of `IGNORE`: the caller writes an explicit `DO UPDATE` and the engine
  silently does something else with it. One rule for all six modifiers is the
  only reading under which writing both clauses means anything. This is
  **believed** to match SQLite but was **not oracle-checked**; if the oracle
  disagrees, the divergence is deliberate under "inspired by, not a port" and
  whoever changes it is re-deciding, not fixing an oversight.

  **NOT NULL is not a uniqueness conflict, so an `ON CONFLICT` clause never
  intercepts it** — the modifier does, per #599 above. The two directions
  differ and both are pinned in `test/test_or_ignore_upsert_639.ml`: a NULL in
  the row being *inserted* skips under `OR IGNORE`, while a NULL *assigned by
  the DO UPDATE* raises, because that write funnels through
  `write_row_rekeyed` → `enforce_not_null`. `Sema.bind_upsert_assignments`'s
  static literal-NULL check is therefore **not** suspended under `CA_ignore`
  (unlike `bind_insert_row`'s): the runtime answer below it is "raise", so the
  two levels agree rather than disagreeing. #639 noted that binder check was
  load-bearing while the runtime path was unreachable; the path is reachable
  now, and the check stays as the earlier, better-located error.

  **`INSERT ... SELECT ... ON CONFLICT DO UPDATE` is implemented (#653, 2026-09-03).**
  It used to have no grammar at all and was therefore a **parse error**, which
  was the right failure mode while the clause had nowhere to go — a
  silently-dropped DO UPDATE would have been #639 again in a new place. The
  requirement was always "implement it fully or keep refusing it"; it is
  implemented now, and the shape of the implementation is the part to preserve.

  **It is a threading change, not a second upsert.** `execute_insert_select_op`
  already drove the source stream one row at a time through the *same*
  `execute_insert` the VALUES form uses; the only thing missing below the
  grammar was that it did not take `~upsert_update` and so could not pass one.
  The clause is threaded grammar → `Ast.S_insert_select` → `Sema.BS_insert_select`
  → `Plan.Op_insert_select` → that one call site. So #639's target-resolution
  pass, the rowid-alias `S.get` pre-probe, #599/#639's NOT NULL ordering, #667's
  pre-write uniqueness probe and `last_insert_rowid()` not moving for a DO UPDATE
  all apply to the SELECT form **by construction rather than by two
  implementations agreeing**. Anyone tempted to specialise the SELECT path is
  re-creating exactly the divergence this entry is two pages about. Binding goes
  through one shared `Sema.bind_upsert_clause` for the same reason — the
  conflict-target validation and the DO UPDATE assignment rules (#547's NOT NULL
  check, #629's generated-column refusal) now live once instead of twice.

  Two divergences from sqlite3, both deliberate:

  - **No `WHERE` is needed between the SELECT and the `ON CONFLICT`.** sqlite3
    cannot parse `INSERT INTO t SELECT k, v FROM src ON CONFLICT(k) DO UPDATE …`
    — its manual prescribes a `WHERE true` to separate the upsert's `ON` from a
    join's. Granary's `join_clause` requires the `JOIN` keyword before its `ON`,
    so once the join list is complete no `ON` can be shifted into it and a
    trailing one can only begin the upsert. Adding `opt_upsert` to the three
    SELECT arms left menhir's conflict counts unchanged (35 states / 290
    conflicts, before and after). `no_where_is_needed_before_on_conflict` pins
    the permissive spelling *and* the join spelling together — the second is
    what makes the first safe rather than lucky.
  - **`ON CONFLICT ... DO UPDATE` on a `USING COLUMNSTORE` table is refused, in
    both spellings** — this *closes* a hole rather than opening one. The
    columnar write arms hand the row straight to `Col_store.insert_rows` and
    probe for no conflict at all, so before #653 the VALUES form accepted the
    clause, ignored it, and inserted a duplicate key: #639's exact failure mode
    reached through the storage engine instead of through a modifier. Same
    reasoning as #660's refusal of a GENERATED column on the same table kind.

  Unchanged and out of scope, all three true of the VALUES form too, so none is
  a SELECT-form regression: `DO NOTHING` and `DO UPDATE ... WHERE ...` have no
  grammar in this engine; a **nested** `excluded.<col>` (one buried inside a
  larger expression, e.g. `excluded.v + 1`) binds to the *target* row's column
  because `Sema.bind_expr`'s `E_tbl_col` arm drops the qualifier and only
  `bind_upsert_rhs_expr`'s top-level match recognises `excluded` — a silent
  wrong answer (sqlite3 says `1010`, granary says `1005`), tracked as **#741**;
  and a DO UPDATE burns the rowid `execute_insert` allocated before it resolved
  the conflict, so a mixed upsert/insert statement lands the inserted row at id
  3 where sqlite3 says 2 (**#742**, the accepted "too HIGH merely skips ids"
  direction #632 names). Pinned by `test/test_insert_select_upsert_653.ml`.

- **`excluded` is a SCOPE, not a shape (#741, fixed 2026-09-03).**
  `Sema.bind_upsert_rhs_expr` recognised `excluded.<col>` only when the
  `E_tbl_col ("EXCLUDED", c)` sat at the **root** of a DO UPDATE assignment
  RHS. Anything nested fell through to `bind_expr`, whose `E_tbl_col` arm
  ignores the qualifier entirely — the documented single-table fallback shared
  by INSERT/UPDATE/DELETE/CHECK/DEFAULT — so `SET v = excluded.v + 1000` read
  the **target** row's `v` and answered 1005 where sqlite3 3.45.1 answers 1010.
  A silent wrong answer, and it bit exactly the idioms the feature exists for
  (`SET n = n + excluded.n`, a `CASE` over `excluded`), because the bare
  `SET v = excluded.v` spelling was the one shape that worked.

  **The Exec side was already correct and needed no change.**
  `Plan.P_excluded_col` is a general expression leaf and
  `Exec.substitute_excluded` recurses through the whole plan expression before
  `eval_expr`. The gap was purely that nested occurrences never became
  `BE_excluded_col`. `bind_expr` now carries `?(excluded = false)`, threaded
  through **every** one of its own recursive calls (including the `~bind`
  callbacks it hands `bind_func` and `bind_case`), and `bind_upsert_rhs_expr`
  is just `bind_expr ~excluded:true`. Threading the flag rather than
  hand-writing a second recursive walker over `Ast.expr` is the point: the
  resolution is exhaustive **by construction**, where a parallel walker's
  missing arm would silently fall back to the very fallback that caused the
  bug. A walker is the shape #670 already warns about for the four
  outer-reference walkers.

  **The flag must not become a default.** `excluded.x` has to stay
  unresolvable outside a DO UPDATE — `SELECT excluded.v FROM t` is refused
  ("unknown table: excluded"), matching sqlite3, and widening `bind_expr`'s
  fallback would resolve it to the target's own column everywhere.

  **Accepted residual, pinned rather than fixed:** a plain
  `UPDATE t SET v = excluded.v` binds with the flag unset, hits that same
  single-table fallback, and resolves to the target's own `v`, where sqlite3
  errors. #741 deliberately did not touch the fallback — it is shared by five
  statement kinds and narrowing it is a separate decision with a much wider
  blast radius. `plain_update_still_takes_the_single_table_fallback` in
  `test/test_nested_excluded_741.ml` holds it as known behaviour, not as an
  endorsement.

  There is no DO UPDATE `WHERE` clause to extend this to: `opt_upsert` in
  `parser.mly` is `ON CONFLICT (cols) DO UPDATE SET assigns` and nothing else.
  Pinned by `test/test_nested_excluded_741.ml`, whose every expected value was
  oracle-checked against sqlite3 3.45.1; verified by mutation (restoring the
  root-only match fails 8 of its 14 cases).

- **A discarded INSERT row burns the rowid `execute_insert` allocated for it
  (#742, ACCEPTED, decided 2026-09-03).** `Exec.execute_insert` calls
  `insert_rowid` before it resolves the conflict, so a row that is then
  discarded — by a DO UPDATE, or by an `OR IGNORE` skip — leaves its
  allocation behind. In autocommit each row is its own committed transaction,
  so the bump is durable and the next row gets 3 where sqlite3 gives 2.

  **Deferring the allocation past conflict resolution was considered and
  rejected**, because two things documented above read the row *after*
  `insert_rowid` has written the auto-assigned `INTEGER PRIMARY KEY` value back
  into it: #639's NOT NULL check (which must run before conflict resolution,
  and whose column is NOT NULL since #530) and the `row_for_idx` extraction
  that feeds `check_insert_unique`. Deferring makes an ordinary auto-assigned
  PK read NULL at that check and raise on the commonest INSERT shape — a wrong
  answer traded for a cosmetic one. Handing the allocation back instead is the
  "too LOW" direction #632 and #706 forbid; a skipped id is the "too HIGH"
  residual those same writeups already accept.

  **The issue's framing is narrower than the behaviour, and that matters for
  anyone who reopens it.** The burn is not specific to the upsert branch:
  `INSERT OR IGNORE` on a secondary UNIQUE index burns identically, with no
  upsert clause in sight and predating #639 entirely. A fix scoped to the
  upsert branch would leave the same divergence reachable through the older
  spelling. Both spellings are pinned by
  `burnt_rowid_on_a_discarded_insert_row` in
  `test/test_nested_excluded_741.ml`.

- **`TRUE` and `FALSE` are aliases for 1 and 0, resolved as a NAME-RESOLUTION
  FALLBACK rather than as keywords (#744, decided 2026-09-03).** Neither word
  existed anywhere in the grammar, so both fell through to the identifier rule
  and bound as column references: `SELECT TRUE` answered
  `unknown column: __const__.TRUE`. The sharp consequence was that sqlite3 and
  granary accepted **disjoint** spellings of an `INSERT ... SELECT` upsert —
  sqlite3's grammar needs the disambiguating `WHERE true` (its bare form is
  ambiguous with a join's `ON` and is a parse error), granary took only the bare
  form (#653), and the intersection was empty. Both spellings now work here;
  granary's permissive one is a deliberate superset and is unchanged.

  Two decisions, and both are about what was NOT done:

  - **They are aliases for the integers, not a boolean storage class.**
    Oracle-checked: `SELECT TRUE + TRUE` is `2` and `typeof(TRUE)` is
    `integer`. So `Exec.value_class_rank` and the #579/#733/#738 comparators
    never hear about them — no comparator learns a new class, and none of that
    work is disturbed.
  - **Nothing was added to `lexer.mll`.** The rule lives in
    `Ast.bool_ident_lit` and is consulted only where a column lookup has already
    FAILED, which is exactly how SQLite resolves them
    (`sqlite3ExprIdToTrueFalse`, reached from `lookupName` when the match count
    is zero). Identifier context therefore wins by construction —
    oracle-checked, `SELECT true FROM t` where `t` has a column named `true`
    answers the column in both engines — and, because the keyword table did not
    grow, #619's lexer/`Sql_ident` drift guard is untouched and no stored SQL
    starts needing quoting it did not need before (#572). Adding a `TRUE` token
    was rejected for the first reason alone: no token can defer to a column.

  **`Ast.bool_ident_lit` is the single chokepoint and the reason a fallback this
  diffuse is auditable.** Every site that can meet a bare `true`/`false` consults
  it and none may grow a second opinion: the parser's `def_value` (a `DEFAULT` has no row in scope, so no
  column can be meant); `Sema`'s five unqualified-column resolution failures
  (`bind_expr`, `bind_expr_join`, the aggregate resolver, and the window and
  projection arms over `select_proj_lookup`); its two `bind_value_expr`s — a
  VALUES list has no row in scope either, so `INSERT INTO u VALUES (true, 2)`
  stores `1` even when `u` HAS a column named `true`, oracle-checked; and
  `Exec.ast_expr_to_plan_check`, the re-compiler for the SQL text the catalog
  stores about itself (a CHECK, a GENERATED expression, a partial index's
  WHERE), which is a different resolver from `Sema`'s and meets a bare `true` on
  every write once one is written down.

  Two of the sites are not obvious and would each have shipped a wrong answer:

  - **The projection needed a `` `Cols `` promotion.** The parser emits
    `` `Cols `` — a bare `string list`, which cannot hold a literal — whenever
    EVERY select item is a plain name, so `SELECT true FROM q` took a path no
    expression ever reaches and stayed an `unknown column` error. It is promoted
    to `` `Exprs `` when a name is one the fallback answers, keeping the name as
    the alias; the same promotion, and the same conditionality,
    `Exec.substitute_outer_proj` already makes for #732.
  - **`Exec.substitute_outer_in_expr` needed an arm, and this is the one that
    would come back first.** #635's correlation detector runs that walker
    against a binding that resolves NOTHING and records that it was asked, so
    every `WHERE true` inside a subquery read as a free outer reference and the
    subquery was misclassified as correlated: a scalar subquery counting rows
    under `WHERE true` answered NULL — silently wrong — and the `IN` spelling
    raised out of the executor. A bare `true`/`false` is never an outer column
    reference, and the scope test already answers "owned" when the subquery's
    own FROM has a column of that name, so the arm sits above it and is a
    no-op in that case.

  **`IS TRUE` / `IS NOT FALSE` are deliberately out of scope, and the important
  half is that the fallback could not turn them into a silent wrong answer.**
  They are truthiness, not equality — oracle-checked, `2 IS TRUE` is `1` while
  `2 IS 1` is `0`, and `'1' IS TRUE` is `1` while `'1' IS 1` is `0`. Granary has
  no general `x IS y` operator at all: `IS` appears only in the `IS NULL` /
  `IS NOT NULL` productions, so `1 IS TRUE` was a PARSE error before #744 and
  still is, pinned as such. Adding the productions is a separate gap.

  Two recorded divergences, both narrower than the fix:

  - **A QUOTED `true` is the literal here** — `"true"`, backticked or bracketed
    — where sqlite3 keeps quoting semantically significant (it answers the TEXT
    `'true'` for the double-quoted spelling through its double-quoted-string
    misfeature, and an error for the other two). Distinguishing them needs an
    `Ast.expr` constructor carrying quotedness, which granary has never had;
    it already treats all four spellings of a non-keyword name as one
    identifier everywhere else.
  - **`GROUP BY true` is still refused**, where sqlite3 groups by the constant.
    `Ast.group_by_item` is `string * string option`, a name and a qualifier, so
    a constant group key is not expressible at all (`GROUP BY 1` is a parse
    error too) — the same orthogonal gap #722's `GROUP BY x COLLATE NOCASE`
    entry names, not a boolean one. It fails loudly.

  The grammar is untouched apart from `def_value`'s semantic action, so menhir's
  counts are unchanged either side of the fix: **35 states with shift/reduce
  conflicts, 290 shift/reduce conflicts arbitrarily resolved**, and the same
  four pre-existing warnings. Pinned by `test/test_bool_literal_744.ml`, whose
  every expected value was read off sqlite3 3.45.1 rather than predicted.

- **A skipped `INSERT` leaves nothing behind in the STORE and the CATALOG,
  including its BEFORE INSERT trigger's nested DML, in an explicit transaction
  as well as in autocommit (#631, fixed 2026-08-06).** Read the scope literally:
  the two things it does *not* revert are the #240 dirty-table set and the #417
  row-level change feed. A spurious dirty mark is over-invalidation of an
  external cache and is safe; a stale `record_change` delta is a phantom row for
  a reactive view whose base table the trigger wrote to. **Neither is a
  regression** — autocommit's `S.rollback` never cleared them either — but the
  invariant above is about `Store` and `Schema_cache` state only. That was
  #666, and **the delta half is now fixed; the name half is deliberately
  not** — see the separate section below. The undo used to be
  `if owned then S.rollback`,
  keyed on *who owns the transaction* rather than on *what the statement
  decided*, so the same statement left a trace or not depending on whether the
  caller had opened a `BEGIN`. It is now a statement-level savepoint
  (`Store.savepoint_begin` plus #280's schema-undo marker, which also restores
  #303's rowid counters), rolled back when the statement wrote nothing — so it
  covers the long-standing UNIQUE skip and #599's NOT NULL skip alike.

  It is taken **only** when the transaction is borrowed, a BEFORE INSERT
  trigger exists on the table, and the resolution is `CA_ignore`; outside that
  intersection no savepoint is pushed, which is what keeps it off the TPC-C
  write path (a B-tree savepoint clones the pager dirty set, and
  `Schema_cache.savepoint_begin` also encodes every columnar store). It is
  opened and resolved within one statement and never touches `explicit_txn` or
  the #555 poison flag, so it is **not** a second exit from a poisoned handle
  and "ROLLBACK is the sole exit" stays true. On an *exception* the savepoint
  is released, not rolled back: a raising statement's partial effects already
  survive in a borrowed transaction, and statement atomicity on error is a
  different problem.

- **`ALTER TABLE ... RENAME` refuses three shapes sqlite3 handles, and one of
  the three is a genuine over-refusal (#673/#645, recorded 2026-09-03).** #673's
  option 1: the divergence is *recorded*, not removed. Oracle-checked against
  sqlite3 3.45.1 on 2026-08-07, after #645's refusal reached `main` via PR #671.

  | case | sqlite3 3.45.1 | granary |
  |---|---|---|
  | `RENAME COLUMN` with a dependent view | remaps the stored view SQL (`CREATE VIEW v AS SELECT z FROM t`) | refuses, naming the view |
  | `RENAME TO` with a trigger on the table | remaps the stored trigger SQL (`... ON "t2" ...`) | refuses, naming the trigger |
  | `RENAME COLUMN a` on `t` while an **unrelated** view says `FROM other AS t` | renames, and correctly leaves the view alone — that `a` is `other.a` | **refuses** |

  **Rows 1 and 2 are defensible conservatism; row 3 is the genuine
  over-refusal.** In the first two the dependency is real — something would
  break if the rename went through unremapped — so granary trades a feature for
  a loud error naming the object, with a documented way out: drop the view or
  trigger, rename, recreate it. In row 3 *nothing depends on anything*: the
  token `t` is an alias for a different table, and the rename is refused over a
  name collision.

  **Row 3 is also the argument for the refusal, which is why it is not to be
  "fixed" by loosening the detector.** SQLite's rewrite is *scoped* — it knows
  that `a` belongs to `other` — and that is exactly what granary cannot do here.
  Views, reactive views and triggers are persisted as raw `CREATE ...` SQL
  **text** keyed by name (`_sys_views`, `_sys_reactive_views`,
  `_sys_triggers`), and the catalog sits *below* the parser in the dependency
  graph (`granary.sql` depends on `granary.catalog`, not the reverse), so there
  is no AST to walk and re-render and there cannot be one. A lexical rewrite
  over that text would rewrite `other`'s column inside the view and turn a
  working view into a quietly wrong one — a silent wrong answer traded for a
  loud break, which is the failure mode #609 was filed about.

  **`Catalog.sql_mentions_ident` is deliberately position-blind — and
  case-insensitive and bracket-aware for the same reason.** It is a *detector*
  guarding a refusal, not a rewriter, and its failure modes are asymmetric: a
  false positive costs a rename and says exactly why, a false negative silently
  leaves a view or trigger naming a column that no longer exists.
  `rewrite_ident_in_sql`'s `is_column_ref_at` excludes a word followed by `.`,
  which is precisely where a *table* name stands (`v0.a`), so a detector
  inheriting that filter would miss every qualified reference — the common
  spelling inside a view body. Position-blind makes it **role-blind** too, and
  row 3 is that bill: an identifier-shaped token counts wherever it stands, so a
  table alias — or a `COUNT` call where a column is named `count` — is
  indistinguishable from a reference. String literals and `--` comments are
  skipped, so a name inside either is not a reference. (`lexer.mll` has no
  block-comment rule, so a slash-star sequence is not a comment in this dialect
  and is not treated as one.)

  **Closing it properly** is #673's option 2 and #645's option (a): a
  parser-side rewriter that resolves names against the schema, so a rewrite can
  be scoped the way SQLite's is. That closes all three rows, not just the third.
  More string surgery in the catalog cannot close any of them at any level of
  cleverness — the information needed is not in the text.

  Two further things a reader who hits a refusal should know:
  - The **column** gate closes over *reachability*, not over the table name
    alone: a definition blocks when it spells the column AND spells something
    reachable from the table — the table itself, or a view that (transitively)
    names it. That is what catches a chain through a `SELECT *` view, whose
    stored text names the table but never the column; requiring both names in
    the same text let that chain through and left the downstream view silently
    dead, #609's own symptom. The **table** gate needs no closure: anything
    reaching the table indirectly does so through a definition that names it
    directly, and that one blocks.
  - `rename_table`'s refusal is a **compatibility break**, in those words: a
    table with any trigger declared `ON` it, or named by any view, cannot be
    renamed at all until that object is dropped, where before the rename
    succeeded and left the object broken.

  Pinned by `test/test_rename_deps_609.ml`, whose `alias_collision_over_refuses`
  holds row 3 — in its qualified spelling, `CREATE VIEW v2 AS SELECT t.a FROM
  other AS t` — as known behaviour pinned rather than endorsed. Change it as a
  decision, not to make a fix pass.
- **A rolled-back statement's #417 row-level deltas are reverted with the store;
  its #240 dirty NAMES are not (#666, decided 2026-09-03).** One accumulator,
  two halves, two different answers — that asymmetry is the decision, not an
  oversight, and the two are pinned side by side in
  `test/test_rollback_deltas_666.ml`.

  The delta log is a statement of **fact about rows**. `Db.drive_reactive`
  installs a change accumulator around every statement, `rv_absorb_changes`
  consumes it, and the maintenance applies each delta to the materialisation
  `_rv_<name>`. So a delta describing a write the store had *undone* became a
  **phantom row in a materialised reactive view** — a row the view reports and
  the base table does not hold. That is a wrong answer, so it is reverted.

  The name set is an **invalidation hint**. A superfluous entry costs an
  external cache one miss; a missing one is a stale read. Reverting it would
  buy nothing and would newly depend on this revert accounting for every
  `mark_dirty` site — a much larger and more scattered set than the delta
  log's, and a site missed there fails in the *unsafe* direction. Note that
  `record_change` marks the name as well as recording the delta, so reverting
  the delta and keeping the name lands on exactly the safe side by
  construction. Anyone "completing" the fix by reverting the names too is
  trading a free safety margin for a new failure mode.

  **The extent is deliberately wider than #631's savepoint, and that is the
  part that is easy to get wrong.** That savepoint is taken only inside a
  narrow intersection (borrowed transaction + a BEFORE INSERT trigger +
  `CA_ignore`). The stale-delta hole is just as real in **autocommit**, where
  no savepoint is ever pushed and the skip arms of `execute_insert_write` /
  `execute_upsert_update` roll the whole per-row transaction back instead —
  and the accumulator rides Lwt sequence-associated storage straight across
  that rollback. So `execute_insert` takes an unconditional delta mark
  (`Exec.changes_mark`, `Cm_none` and one predicted branch when nobody is
  capturing) and `stmt_savepoint_finish` restores it when the row did not
  write **and** something actually reverted the store — `owned`, or a
  savepoint was taken. With neither, nothing was reverted and the deltas must
  stay: dropping them would lose a real trigger write, which is the same class
  of wrong answer with the sign flipped.

  Three properties of the mark are load-bearing:

  - It is per **ROW**, not per statement: a multi-row `VALUES` list is one
    `execute_insert` (and, in autocommit, one transaction) per row, so a
    statement-level snapshot would restore over its siblings' real writes.
  - It is **O(tables touched), never O(rows)**. The log is prepend-only per
    table and each table's `ref` is created once and never replaced, so the
    mark is each table's current list — which later prepends leave as the tail
    — and the restore is an assignment back to it plus the removal of tables
    absent at mark time. Anything proportional to the deltas already recorded
    would make a multi-row INSERT quadratic.
  - It is restored on the autocommit **exception** path too, for the same
    reason (`S.rollback` undoes the store), and **not** on the borrowed one,
    where #631 already decided the savepoint is released rather than rolled
    back because a raising statement's partial effects survive.

  `Db`'s `ROLLBACK` and `ROLLBACK TO` handlers needed nothing: #427 already
  clears `rv_pending` on the former and sets `rv_resync` on the latter.

  **The opposite direction — #737, fixed 2026-09-03.** `Db.drive_reactive`
  used to drop the whole accumulator on an `Error` result, which left a
  materialised view **missing rows** rather than inventing them. It now
  schedules a **resync** of the reactive views instead — `rv_resync <- true`,
  the same answer `ROLLBACK TO` has always given for the same reason (#427) —
  and still absorbs nothing on failure.

  **Two of #737's premises, as filed, were wrong, and correcting them is what
  chose the fix.** Both were established by running the repro, not by reading:

  - **Autocommit is NOT the safe half.** The issue (and this file) said
    dropping the accumulator there is right because the statement's
    transaction was rolled back. A statement is not one transaction: a
    multi-row `VALUES` list and `INSERT … SELECT` run one `execute_insert` —
    and one **COMMIT** — per row, so a failure on row k leaves rows 1..k−1
    durably committed with their deltas discarded. `INSERT INTO base VALUES
    (1,'g'),(2,'g'),(1,'g')` in autocommit left two rows in the base table and
    an empty `_rv_av`.
  - **Absorbing the accumulator on `Error` is not sufficient**, so the
    obvious mirror of #666 does not close it. `execute_insert` records its
    `Inserted` delta only *after* `execute_insert_write` returns, and the
    AFTER INSERT trigger fires **inside** it — so a raising AFTER trigger in a
    borrowed transaction leaves the row in the store with **no delta recorded
    anywhere**. Absorbing would still have left the view missing that row. The
    delta log is not a faithful description of what a raising statement left
    behind, in either transaction mode, and no amount of marking makes it one.

  So **#737 owes no new marks at all** — the sentence that used to stand here
  claiming it owed them to `execute_update_op`, `execute_delete_op`, the FTS
  arms and the two columnar `Op_insert` arms was written against the absorbing
  fix. Under a resync the accumulator is discarded on `Error` exactly as
  before, so a handler that leaves stale deltas behind stays harmless.
  `execute_insert`'s mark (#666's) is kept because it also serves the
  non-raising SKIP path.

  **The trigger is narrowed, and the narrowing is the only part with a cost
  argument.** `Db.rv_note_failed_statement` sets `rv_resync` when either:

  - the statement ran inside an **explicit transaction** — #631 keeps a raising
    statement's partial effects there deliberately, so *any* error may have
    left rows behind, including the no-delta shape above; or
  - the accumulator's #240 **name set** names a reactive base table — in
    autocommit that is what says the statement got as far as committing
    something. The over-approximate half of the accumulator is used on
    purpose: it errs towards a superfluous rebuild, never a missed one.

  With neither — the common `try INSERT, catch UNIQUE` in autocommit, and every
  parse or sema error — nothing is scheduled and the error path costs what it
  always did. An unconditional resync would have made every failed statement
  rebuild every view.

  `drive_reactive`'s flush gate moved above the `Error` early-return so a
  resync scheduled in autocommit is applied immediately rather than waiting for
  the next successful statement; the failing statement's **own** error is still
  what the caller gets back, never the flush's.

  **`ROLLBACK` deliberately does not clear `rv_resync`.** A rebuild from the
  base tables is correct whatever the transaction did, so a needless one after
  an aborted transaction is a bounded cost, and clearing it would be one more
  thing to get right.

  Pinned by `test/test_change_feed_error_737.ml`, whose controls need a seam:
  a resync of an already-correct view is invisible, so they write a bogus row
  straight into `_rv_av` first — a resync rebuilds from the base table and
  removes it, an incremental flush leaves it. Verified by mutation against all
  three candidate designs: the old drop fails 7 of its 12 cases, an
  unconditional resync fails the two narrowing controls, and absorbing instead
  of resyncing fails 4 — including the raising-AFTER-trigger case, which is the
  one that decides between the two fixes.

- **The #240 name set covers DDL that changes a table's observable contents
  (#405, decided 2026-09-03).** It used to be row-level DML only, so
  `execute_with_dirty` answered `[]` for `DROP TABLE` and for every `ALTER
  TABLE` form — including `DROP COLUMN`, which physically re-`put`s every row.
  A name-keyed external cache went on serving a dropped table's rows, and rows
  of the wrong arity after a column was added or dropped. The `db.mli` wording
  ("pure-DDL … the list is empty") was the second half of the defect: it
  implied `DROP COLUMN` was row-neutral, which it is not.

  Marked: `DROP TABLE` (the table's name), and all four `ALTER TABLE` forms —
  `DROP COLUMN`, `ADD COLUMN`, `RENAME COLUMN`, and `RENAME TABLE`, which marks
  **both** the old name (it stops answering) and the new one (it starts
  answering with rows it did not have before). `ADD COLUMN` writes no row byte
  and is marked anyway: every row a reader sees gains a cell, so a cached
  result has the wrong arity — the same failure mode as `DROP COLUMN`, reached
  through the catalog instead of the tree.

  **Not** marked, because no existing table's answers move: `CREATE TABLE` /
  `CREATE VIRTUAL TABLE` (the new table is empty); `CREATE INDEX` / `DROP
  INDEX` (a whole B-tree is written or discarded, but every query returns the
  same rows — only the plan differs); view and trigger DDL; `VACUUM` (a
  physical rebuild that preserves every row, and which anyway kills every
  sibling handle, #634); `ATTACH` / `DETACH` (the signal carries bare names
  with no schema qualification, so it could not express the change).

  Two properties this rests on. The marks are taken **after** the DDL succeeds,
  so a raising `ALTER` marks nothing — which agrees with `Db`'s `Error` arm
  discarding the accumulator. And they are **names only**: `mark_dirty` does
  not touch the #417 delta log, so `execute_with_changes` still reports no
  row-level deltas for DDL, and a reactive view's `rv_absorb_changes` (which
  reads `dirty_changes`, not `dirty_elements`) cannot pick up a phantom row
  from a `DROP TABLE`. A consumer of both must invalidate from the name set;
  "no deltas" does not mean "nothing changed". Pinned by the two new
  `ddl in scope (#405)` / `ddl out of scope (#405)` groups in
  `test/test_dirty_tables_240.ml`, which replace the two tests that asserted
  the opposite.

  One knock-on worth knowing: `DROP REACTIVE VIEW v` runs an internal
  `DROP TABLE _rv_v` inside the caller's accumulator, so the materialisation's
  own name now appears in the dirty list. That is not new noise — `CREATE
  REACTIVE VIEW` populates `_rv_v` through the ordinary insert path and has
  always reported it — and the two spellings now agreeing is pinned by
  `reactive-view ddl marks materialisation`. The `rv_flush` path is unaffected:
  it installs its own accumulator (`Db.rv_flush`), which shields the caller's.

- A column's `not_null` no longer records *why* it is set — declared or implied by a primary key — because #530 folded both into the one stored bit. Anything that removes a key therefore cannot restore the column's original nullability: `ALTER TABLE ... DROP COLUMN` on a composite-PK member clears `primary_key` on the survivors but deliberately leaves `not_null`, since the engine is still enforcing it. Two bits (or an origin tag) is the fix if this ever needs to be exact — not cleverness at the ALTER sites.

### A failing autocheckpoint is surfaced, never raised (#638)

Both auto paths used to run the checkpoint under
`Lwt.catch … (fun _ -> Lwt.return_unit)`, so every failure was discarded whole.
The background one (`maybe_autockpt_after_commit`) was the worse of the two —
nobody awaits that fiber — and a checkpoint that failed on every attempt was
completely invisible: the WAL grew without bound with no counter, no event and
no log, and the first symptom was a full disk or a very slow recovery. On a long
TPC-C run that reads as a performance cliff.

Since #638 a failure is **recorded and emitted, and still not raised to the
caller of the commit that triggered it**. That asymmetry is the decision, not an
oversight: the commit has already succeeded and its WAL frames are still valid
frames, so failing it would convert a deferrable maintenance problem into
spurious transaction failures. Three surfaces, all fed from the one
`Store.note_checkpoint_failure` chokepoint:

- `Store_event.Checkpoint_failed { target_frames; consecutive; message }` — the
  live signal, and the thing that finally balances the `Checkpoint_begin` an
  aborting checkpoint used to leave dangling.
- `Store.checkpoint_health` — sticky, so an operator can read it long after the
  failing commit returned. `consecutive_failures` (and `last_error`) are cleared
  by any checkpoint that completes and by `Store.clear_checkpoint_error`;
  `total_failures` is never cleared by success, because "this store has been
  unable to truncate its WAL at least once" is a different question from "is it
  failing right now".
- `PRAGMA checkpoint_status` — one row of
  `(total_failures, consecutive_failures, last_error)`; `last_error` is NULL
  when nothing has failed since the last completing checkpoint.

The **explicit** `Store.checkpoint` path already surfaced its failure by
raising, and still does — it merely feeds the same counters, so
`checkpoint_health` describes the store rather than only its automatic path. Do
not "fix" the remaining silence by making the auto path raise; the escalation
this issue asks for is visibility. `test/test_checkpoint_failure_638.ml` pins
both halves (observable, and the triggering commit still succeeds with its data
readable) by failing the main-file `write_page` — in WAL mode the main file is
written *only* by a checkpoint, so commits keep succeeding while every
checkpoint fails.

### A parse error carries its position and the offending token (#487)

Every syntax failure used to be the bare string `parse error: syntax error`.
It now reads, for example:

```
parse error: syntax error at line 2, column 8 (byte offset 21): unexpected token "FORM"
```

Three things are decided here rather than incidental:

- **No expected-token set.** The issue asked for one "ideally", and it is
  deliberately not provided. `lib/sql/parser.mly` resolves ~290 shift/reduce
  conflicts arbitrarily, so the automaton state at failure does not correspond
  to an honest "expected X" list — a synthesised one would be confidently
  wrong, which is worse than silence. Position and offending token come
  straight off the lexbuf and are exact.
- **The line is counted from the SOURCE TEXT, not read off the lexbuf.**
  `lib/sql/lexer.mll` skips whitespace with one rule and never calls
  `Lexing.new_line`, so `lexbuf.lex_start_p.pos_lnum` is 1 for every position
  in every statement. `Db.line_col_of_offset` counts newlines in the SQL string
  up to `Lexing.lexeme_start`, which stays correct whatever the lexer does with
  newlines. **A single-line-only test cannot tell the two apart** — that is why
  `test/test_parse_error_487.ml` carries three multi-line cases.
- **`Db.Parse` still carries a plain `string`.** The positioned detail is the
  payload and `pp_error` still prefixes `parse error: `, so no consumer needed
  rewriting. Making it structured would have stranded the four test files that
  bind the payload as a string for their own diagnostics, for no caller that
  wanted the parts separately.

A `Failure` out of the lexer (`unexpected char: '@'`) or out of a parser
semantic action already said *what*; it is now suffixed with the same
`at line L, column C (byte offset O)`.

The two `Error (Parse "syntax error")` arms in `execute_core` and
`execute_change_count_core` — the INSTEAD OF re-parse, unreachable in practice
because the same SQL already parsed for the bind that produced
`Unknown_table` — now propagate the real error instead of manufacturing a
fresh bare one. Error quality must not depend on which entry point the caller
used.

### A net-zero SUM group is retained, with a stated ceiling (#423)

`Aggregate.Make` drops a group from its `GMap` only when **both** `mult = 0` and
`aggv = 0`. A SUM group whose row weights cancel to `mult = 0` while `aggv <> 0`
is kept on purpose: `aggv` is not recoverable from the delta feed — the operator
never sees a base row — so dropping it would silently compute the wrong total if
the group later revived. #423 chose the third of its three options: **accept the
retention and document a ceiling.** No compaction pass, no LRU/age cap. Do not
add one without re-deciding; re-derivation on revival needs exactly the base
access the operator does not have. The long form, with the measurement, is
`docs/IVM_MEMORY.md`; the short form is in `lib/ivm/aggregate.mli` where an
implementer meets it.

The ceiling, in the terms an operator has:

- **Bounded by DISTINCT groups, never by updates.** One map entry per group,
  forever, so a view churning a fixed key set retains at most that key set.
  Unbounded *key cardinality* — grouping on a session id, a request id, a
  timestamp — is the hazard; a high update rate over a stable key set is not.
  Measured: 50 000 and 200 000 net-zero rounds over one key both cost 27 words.
- **9 words per retained group, plus the caller's key.** One `Map.Make` node
  (6 words) plus the `{ mult; aggv }` record (3). `Reactive_view.Agg_engine`'s
  key is a `Row.value array` of one element, so an `INTEGER`-grouped view costs
  **16 words = 128 bytes** per retained group — ≈128 MB per million, which is
  the figure to size a unikernel against. Both numbers measured exactly.
- **It cannot arise without a NEGATIVE weight.** If every element of a group has
  a non-negative cumulative weight, `Σ w = 0` forces every `w = 0` and hence
  `aggv = 0`, which prunes. So a delta stream that only retracts what it has
  inserted retains **nothing at all**, and the issue's "on a high-churn SUM view
  such net-zero groups accumulate unboundedly" overstates it: signed weights are
  the precondition. They reach the operator either from a composed Z-set
  pipeline (the `granary.ivm` API is public) or from a retraction with no
  matching insertion — which for the `Db` driver means a stale `record_change`
  delta, i.e. the #666/#737 neighbourhood.
- **It cannot arise unless the measure VARIES within one group.**
  `aggv = Σ measure * weight`, so a constant measure `c` makes it `c * mult`.
  **The issue's claim that COUNT never hits this is correct** — checked, not
  repeated: it holds unconditionally, for arbitrary signed weights, and is
  fuzzed as a QCheck property. The rule is about constancy, not about the
  measure being 1: a SUM over a column constant within its group is equally
  safe.

**A live view's retention is not permanent.** `Db.rv_rebuild_engine` builds a
*fresh* `Agg_engine` on a resync — scheduled by `ROLLBACK TO` (#427) and by a
failed statement (#737) — so a rebuilt view starts from an empty map. That is
the operator's escape hatch and it costs a full rebuild.

`Aggregate.Make(S).retained_groups` and `Reactive_view.Agg_engine.retained_groups`
report the live entry count including the invisible net-zero ones, so an
embedding can watch its own ceiling instead of inferring it; comparing against
`snapshot`'s length gives the net-zero count directly. Pinned by
`test/test_agg_retention_423.ml`, whose allocation gate is armed by default —
see the non-wall-clock gate table above.

### A reactive-view callback can be detached, and registering one is O(1) (#746)

`Db.register_view_callback` returns an opaque `Db.view_callback` handle, and
`Db.unregister_view_callback : t -> view_callback -> bool` detaches the one
callback it names. Two gaps, one shape — nothing ever removed a callback, and
the append was `e.rv_callbacks <- e.rv_callbacks @ [ cb ]`, which copies the
whole list per registration.

**Neither was a correctness bug, and the commit message should not claim
otherwise.** #469 had already given a *view* a removal path
(`DROP REACTIVE VIEW`, which discards every callback on it), and the downstream
consumer that filed this works around the missing per-callback one with a
generation-token trampoline that measures correct — 1 notification on 1 change,
every time. What it cost was **sustainability**: every re-wire leaked a dead
closure that was still invoked on every change and had to decide for itself
that it was stale, and *n* leaked closures are *n* invocations per change, so
the quadratic append compounded it directly. Hot-reload is the workload that
turns both into a ceiling on how often an app may edit its hooks.

**Callback order is contractual as of #746, and that is a decision the O(1)
rewrite forced.** Nothing had ever documented it, but registration order,
sequentially awaited, was the observable behaviour of `@` plus `Lwt_list.iter_s`.
The list is now held **newest-first** (prepend, O(1)) and reversed at the one
firing site, so the order survives; the reverse is O(n) on a path that is about
to invoke n callbacks, i.e. free. `callbacks_fire_in_registration_order` is what
catches a future change that drops the reverse.

**Mid-flush removal is a per-batch SNAPSHOT** — the question the issue flags,
because `rv_flush_inner` snapshots the *view* list but read `entry.rv_callbacks`
live. `rv_apply_and_notify` binds `cbs` once, where it computes `want_cb`, so
`want_cb` and the fired set cannot disagree; `rv_callbacks` is an immutable list
and both register and unregister *replace the field* rather than mutating cells,
so an in-flight iteration is unaffected by construction. Concretely: a callback
that unregisters itself always completes the invocation it is in and is silent
from the next batch; a callback removed by an **earlier** callback in the same
batch still runs for that batch; a registration made from inside a callback
starts firing from the next batch, so it cannot make the current batch loop.
A tombstone (skip immediately) and a refusal were both rejected — the snapshot
is the only one of the three that needs no new state and no new error, and it is
what the immutable list gives for free.

**A growable vector was rejected for the same reason.** It buys O(1) removal
where the list is O(n) filter, and costs the snapshot: iterating a mutable
vector while a callback mutates it is exactly the undefined behaviour #746
exists to remove. Removal is also the *rare* operation once it exists at all —
the whole point is that the registry no longer grows without bound.

**Handle identity.** Ids come from one process-global counter (`rv_cb_seq`), not
a per-entry one, and the handle carries the view name — so there is no
view-name argument to get wrong, and a handle presented to a different `Db.t`
over the same store matches nothing rather than removing an unrelated callback.
`unregister_view_callback` is idempotent and answers `false` for an already-
removed handle, a dropped view, or a foreign one.

**API break, deliberately.** `register_view_callback` returned
`(unit, [ `Unknown_view of string ]) result` and now returns
`(view_callback, …) result`. Adding a second registration function was
rejected: it would leave the un-removable one as the shorter, default name
forever. The migration is one line at each call site — `| Ok () ->` becomes
`| Ok _ ->`, or bind the handle. In-tree, `test_reactive_view_427` gained an
`attach` helper that maps the handle away so its `register_result` testable is
unchanged.

Measured in-repo before and after, by allocation rather than wall clock
(`Gc.minor_words` over a fixed code path is deterministic, so a loaded box
cannot move it): registering *n* callbacks cost 376 750 words at n=500 rising
to 96 028 000 at n=8000 — **4.00x per doubling, at every step** — and now costs
a flat 13 words per registration, i.e. exactly 2.00x. That is the gate in
`test_view_callback_746`, armed by default (see the non-wall-clock table above)
because the ceiling is backed by a measurement rather than predicted; verified
by mutation, where restoring the `@` append reports slope 7.98 and fails.

### The writer lock is measured, and every acquisition goes through one door (#718)

`Store.lock_stats` reports the writer lock's wait and hold time per acquisition
site (`Txn`, `Checkpoint`, `Autocheckpoint`, `Commit_sink`), plus a "who held it
when the wait began" matrix. It exists because service-time profiling cannot see
the critical section: `commit_wal` releases the lock *before* it fsyncs, and a
`BEGIN` that finds the lock held is waiting rather than working. #716's
"75.3% of NewOrder service time is transaction control" was sound arithmetic
over service time; "75.3% of the critical section" did not follow from it.

**Every `Rwlock.acquire_write t.lock` and `Rwlock.release_write t.lock` in
`store.ml` goes through `acquire_writer` / `release_writer`.** A site that
acquires directly is not merely unmeasured — it holds the lock while the
accumulator believes nobody does, so it corrupts the `blocked_by` attribution of
everyone who waits behind it. `report.unattributed_waits` and
`unbalanced_releases` are the detectors for exactly that, and they are bug
signals rather than measurements: a bypassed acquisition leaves the totals
looking entirely plausible. A new acquisition site owes itself a new `site`
constructor rather than borrowing one.

Two properties are worth knowing before editing this:

- **The contention COUNTS need no clock.** They come from
  `Rwlock.writer_active` sampled immediately before the acquire, which is exact
  (`acquire_write` blocks iff a writer holds the lock at that moment), so they
  stay valid on a pure-Mirage build where every duration is `0.`.
  `report.clock_installed` is what keeps a reader from mistaking that zero for
  "nothing waited" — `Store.set_clock` installs the clock, and its default
  returns `0.`.
- **A wait is attributed to the holder observed when the wait BEGAN.**
  `Rwlock.acquire_write` wakes every waiter on release and lets the scheduler
  pick, so there is no queue position to read; a wait spanning several holders
  lands wholly on the first. Documented approximation, deliberate.

What it measured, first time out (`bench/results/2026-09-02-tpcc-stmt-profile-granary.csv`):
at **one** terminal the writer lock is **99.8% occupied**, and the background
autocheckpoint holds it for **25.1%** of the interval while being the blocker
for **100%** of all writer-lock wait. That settles #716 item 2 — `BEGIN` is
waiting, and it waits for `maybe_autockpt_after_commit` — and it means no part
of #716's headline converts to throughput at `TERMINALS > 1` until something
leaves the critical section. Tracked as #719, **fixed** — see below.

**This instrument cannot see scheduler drain, so it narrows that hypothesis
rather than excluding it.** An uncontended `Rwlock.acquire_write` returns an
already-resolved promise, so an uncontended acquisition contains no yield by
construction; the sub-millisecond residual in the `wait_ms` column is the
instrument's own two-clock-read floor (~0.3 µs per acquisition, and the
zero-contention `autocheckpoint` row is the built-in control for it), not drain.
Do not quote that residual as a drain measurement.
`COMMIT`'s fsync is outside all of it by construction and is a separate
question.

Any test of this must be **file-backed and WAL-mode** for the site attribution:
the Mem backend has no WAL, so `Store.checkpoint` returns before it acquires
anything and no autocheckpoint is ever dispatched — an in-memory version passes
while measuring nothing. `test/test_lock_stats_718.ml` is.

### The checkpoint migrates outside the writer lock (#719)

**A checkpoint no longer holds the writer lock while it copies pages.** It still
takes it exactly once — so #718's `Lock_stats.site` constructors did not have to
grow, and `unattributed_waits`/`unbalanced_releases` are still the detectors —
but the hold now covers the *install* (a final catch-up pass, the gate, and
`Wal.reset`) rather than the migration. Option 1 from the issue (raise
`default_wal_autocheckpoint_threshold`) was explicitly **not** taken: it makes
the holds fewer and longer, which is the same 25% differently spelled.

The migration is safe outside the lock for three reasons, and all three have to
keep holding:

- a committed WAL frame's bytes are **immutable** for the life of the
  generation, and an append only ever lands at or above `committed_frames`;
- in WAL mode the main file is written by **nothing but a checkpoint** (a
  commit's `Pager.alloc` can `ftruncate` it *longer*, which moves no data);
- a half-migrated main file is **invisible**, because every read resolves
  through the WAL overlay first and a page we have migrated still has its frame.
  Only `Wal.reset` retires the overlay, and that step is still under the lock.

**Two things the lock was silently providing had to be provided explicitly, and
they are the whole risk surface of this change.**

**1. Exclusion between checkpoints — `st.ckpt_mutex`.** `autockpt_in_flight`
only ever coalesced the *auto* path against itself; the manual `Store.checkpoint`
was excluded by the writer lock. With the migration unlocked, a second
checkpoint's `Wal.reset` would recycle the frame indices the first is still
reading, and it would then copy a *new* generation's bytes into an *old*
generation's page — silent corruption, not a crash. The mutex covers both paths.
**Lock order is `ckpt_mutex` then `t.lock`, never the reverse**; nothing else
takes `ckpt_mutex`, and the one site that would have reached for it while
holding `t.lock` (the non-WAL commit arm's `maybe_autocheckpoint`) was a
provable no-op and is gone.

**2. Snapshot isolation — the reader gate moved, and it is per pass.** This is
the part that is easy to get wrong, and the first revision of this fix did. The
old code ran the whole gate *before* the migration; the obvious split runs it
just before `Wal.reset` instead, which is what `Replication.checkpoint_wal_to_main`
documents as benign. **It is not benign here, and that comment's reasoning is
incomplete.** A snapshot at frame `m` resolves a page through
`Wal.find_page_at ~max_frame:m` and falls through to the **main file** for any
page whose every frame is at or above `m` — a page written for the first time
since the reader began. Writing that page's newer content into the main file is
immediately visible to that reader, with no `Wal.reset` anywhere near it.

So the two halves of `wait_for_readers_past` guard different operations and are
now called from different places:

- the **replication (#263) and backup (#265) floors** guard frame *recycling*,
  so they run once, immediately before `Wal.reset`, under the lock — running
  them per pass would spend their bounded-yield budgets several times per
  checkpoint;
- the **RO-snapshot gate** guards the *migration*, so `wait_for_ro_readers_past`
  runs before **every** pass, including the unlocked ones.

That gate is what makes the pass's frame window **bounded at both ends**:
`migrate_ckpt_pass` migrates a page only when its newest frame index is in
`[since, head)`, where `head` is `committed_frames` read at the pass's start and
gated on. The upper bound is not an optimisation — without it a pass could
migrate a frame at or above `head` while a snapshot sits exactly *at* `head`,
which is the violation above. Coverage stays complete because the bounds
interlock: the next pass is called with `~since:head`, and every frame published
after `head` was read necessarily lands at or above it.

Two further properties worth knowing before editing this:

- **The catch-up pass under the lock is what makes `Wal.reset` legal.**
  `Wal.reset`'s own crash-safety note assumes "the caller has already migrated
  and fsynced exactly those pages"; the unlocked passes cannot promise that,
  because a writer keeps appending underneath them. The final pass runs where
  `committed_frames` cannot move, so it is by construction the last one needed.
  A commit that lands mid-migration is therefore migrated, not lost — that is
  `a_commit_during_the_migration_is_migrated_not_lost`, and it is the case a
  wrong split fails *silently*.
- **`ckpt_io_in_flight` now brackets two regions, not one**, and the gap between
  them is exactly the `acquire_writer` that separates them. Keeping the count
  out of that gap is what preserves #338: a checkpoint merely parked on the
  writer lock behind an abandoned write txn stays invisible to `close`, so it
  cannot wedge it.

**One pre-existing window is WIDENED, and it is still sound.** `commit_wal`
releases the writer lock before its fsync, so the frames a checkpoint migrates
may be published but not yet durable — true before #719 too, but now for a whole
migration's duration rather than an instant. Crash between the main-file fsync
and `Wal.reset`'s marker fsync and recovery reads the WAL's durable prefix `P`;
pages migrated from a frame above `P` then resolve to the main file's newer
bytes. They are unreachable rather than wrong: both header pages are rewritten
every commit and a checkpoint spans many, so the recovered root is the root as of
`P`, and under copy-on-write a page whose only frame is above `P` was freshly
allocated or taken from the freelist by a commit after `P`. The file is
consistent as of `P`, with some orphaned pages. The argument is in
`checkpoint`'s header comment in `store.ml`; anything that makes a commit write
a page **in place** would break it.

**The unlocked phase is ONE pass, and that is a measured number.** The design
this started from assumed the passes converge — each covers only what the
previous pass's commits appended, so they should shrink geometrically. **They do
not.** A migration pass and a committing writer proceed at comparable rates
(both are streams of small yielding I/Os), so pass k+1 is about as long as pass
k and every extra pass is just more time for the WAL to grow before `Wal.reset`
runs. Growth is *linear in the pass count*. Measured with
`test_wal_autocheckpoint`'s `low threshold keeps WAL bounded` — 500 sequential
commits at `wal_autocheckpoint = 10`, WAL high-water in frames:

| unlocked passes | WAL peak |
|---|---|
| pre-#719 (migration under the lock) | 5 |
| 1 | 20 |
| 2 | 80 |
| 4 | 195 |

The pre-#719 number is small **precisely because** the migration held the lock:
no commit could append during it. So a WAL high-water above the threshold is the
unavoidable price of #719 and the only question is the multiple — one pass costs
2x, which is the same order the threshold already overshoots by; four costs 20x,
which would turn `wal_autocheckpoint = 1000` into an ~80 MB WAL and a
correspondingly slow recovery, i.e. the unbounded-WAL failure mode #638 exists to
make visible. `max_unlocked_ckpt_passes` is 1. Raising it does **not** buy a
smaller locked catch-up — the catch-up is whatever the last unlocked pass let
through, and every pass lets through about the same amount.

**Accepted trade, recorded so it is not found by surprise:** the locked gate now
runs against the *caught-up* target rather than the announced one, so an RO
snapshot that registered during the migration, at a frame below that final
target, can block the install while the lock is held. The window is the commits
that landed during the last unlocked pass, and #382 already requires consumers
to tolerate an unbalanced `Checkpoint_begin`; it is now also true that the
`Checkpoint_end` which does arrive may have recycled **more** frames than the
begin announced.

**Two neighbouring defects were found by an independent review of this change
rather than by a test. Both were pre-existing and both are now FIXED — see the
two sections below for what was decided.**

- **#739** — a *follower*'s snapshot could register below a checkpoint's target.
- **#740** — `PRAGMA wal_checkpoint` inside an explicit transaction
  self-deadlocked.

Pinned by `test/test_autocheckpoint_lock_719.ml`, which is **file-backed and
WAL-mode** for the reason the #718 section gives. Its first two cases **hook the
main file's `write_page`** so the migration can be parked at a chosen page: the
property under test is "the lock is free *during* the migration", and a test
that merely sampled the accounting at a convenient moment would pass on the old
code whenever it sampled outside a checkpoint.

### A follower does not checkpoint its own WAL (#739)

`Store.ro_begin_at` registers a snapshot at `Wal.committed_frames` — except on a
**follower**, where it registers at `min(committed_frames, follower_ack_position)`
so a reader never observes a frame past the last commit the replication apply
loop has applied (#263). That cap is what made #719's RO gate unmaintainable
there: `Rwlock.acquire_read` never blocks, so a snapshot registers whenever the
application likes, and on a follower it can register *below* a target the gate
has already cleared. It then resolves any page whose every WAL frame is at or
above its horizon from the **main file** — which is exactly where the migration
has just written newer content.

**A bigger gate cannot fix this and neither can clamping the target**, and both
dead ends are worth recording because both are the obvious next idea:

- A gate excludes snapshots that *exist*. One that registers below the target
  afterwards defeats a bigger, an earlier and a repeated gate equally.
- Clamping the checkpoint's target to the ack floor would leave the frames above
  it un-migrated, and this engine's checkpoint truncates the **whole** WAL
  (`Wal.reset`) — so a clamped migration could not truncate and would not be a
  checkpoint. The damage also outlives the checkpoint: once the overlay is
  retired the migrated content is in the main file permanently, where the cap
  cannot exclude it at all. The cap and a local checkpoint are *mutually
  exclusive*, not merely racy.

So the fix is a refusal, in the same spirit as `rw_begin`'s refusal of writes in
follower mode, plus one backstop:

- **`checkpoint_body` refuses when `st.follower`.** It fires before
  `Checkpoint_begin` and outside the `Lwt.catch`, so nothing is recorded as a
  checkpoint failure — the checkpoint never started. This makes the auto path
  moot rather than needing its own guard: no commit is possible on a follower,
  so no autocheckpoint is ever dispatched, and `Store.checkpoint` (reached from
  `PRAGMA wal_checkpoint`) was already the only way in. `Standby` is unaffected —
  it migrates through `Replication.checkpoint_wal_to_main`, a different path.
- **`Store.ro_begin` / `ro_begin_as_of` refuse a snapshot below
  `st.ckpt_migrated_through`**, the highest pass boundary any checkpoint has
  migrated in the **current WAL generation**. Each pass publishes it *before*
  writing a page (so a snapshot registering mid-pass is measured against it) and
  `after_ckpt_reset` clears it (a new generation's frame indices mean something
  else). It is deliberately **not** cleared on the failure path: a checkpoint
  that failed before `Wal.reset` still left migrated pages in the main file.

**The second half exists only because the first leaves one way in**, and it is
a real one: follower mode switched ON while a checkpoint that started on a
non-follower is still migrating — a store handed to `Standby.follow` may carry
an autocheckpoint in flight from its last commit. Both refusals release the read
lock before failing; a snapshot that is not registered must not leave it behind,
or the checkpoint coordinator and `close` wait on a reader that will never
drain.

**The `ro_begin` refusal is inert for a non-follower, by construction and not by
luck.** Its horizon is `Wal.committed_frames`, which only grows and is what each
pass's `head` was read from, so it is at or above every published boundary. That
is what keeps #719's whole point — reads and commits proceeding during the
migration — intact, and
`a_non_follower_snapshot_during_a_migration_is_served` asserts it directly.

Reachability, precisely: pre-existing (the pre-#719 migration yielded too, and
`ro_begin` was equally lock-free); #719 widens the window rather than opening
it; unreachable from the auto path in both codebases; never on `Standby`'s own
path. Pinned by `test/test_follower_snapshot_739.ml`, **file-backed and
WAL-mode** with the migration parked through a hooked main-file `write_page`.
Verified by mutation: stubbing either half red-lights its own case and leaves
the other passing.

### `PRAGMA wal_checkpoint` is refused inside an explicit transaction (#740)

`BEGIN` → `Store.rw_begin` takes the store's writer lock and holds it for the
whole transaction; `Store.checkpoint` takes the same lock for its install phase,
and `Rwlock` is **not re-entrant** (`rwlock.mli` says so in as many words). The
fiber parked on itself and the connection was unusable from that point.

Pre-existing, not a #719 regression: before the phase split the checkpoint
parked on the lock it took first; since #719 it parks at `ckpt_finish`'s single
`acquire_writer`, **holding `ckpt_mutex`**, so every later checkpoint on the
store queues behind the wedged one instead of latching on `autockpt_in_flight`.
**Do not "fix" it by reversing the lock order** — `ckpt_mutex` → `t.lock` is
what #719's checkpoint-vs-checkpoint exclusion rests on. Making `Rwlock`
re-entrant is the other alternative and is a much larger change that interacts
with #555/#585's ownership work.

It is refused up front instead, the shape #473 (`DROP REACTIVE VIEW`) and #598
(routing statements) already use. **Not a divergence**: sqlite3 declines the
same statement (oracle-checked 2026-09-03 — `database table is locked (6)`), and
in both engines the caller's transaction survives the refusal intact and
commits.

Two things about where the guard lives:

- **Scoped to the ROUTED handle's own `explicit_txn`, not `any_explicit_txn`.**
  Under ATTACH each schema is its own `Db.t` over its own `Store.t` with its own
  writer lock, so a transaction open on `aux` cannot deadlock a checkpoint of
  `main`. A transaction held by a *different handle over the same store* (a
  `create_worker_handle` sibling) is not this bug either — that checkpoint blocks
  and then proceeds, which is ordinary mutual exclusion.
- **Both entry points are guarded.** `execute_control_op` covers the one-shot
  spelling; `run_core` hands `st.plan` straight to `Sql.Exec.execute_with_count`
  and never reaches `execute_control_op`, so a *prepared* `PRAGMA wal_checkpoint`
  would otherwise still deadlock. The gate sits below the #634 staleness and
  #555 poison gates: a poisoned or stale handle has a more urgent thing to say.

The refusal does **not** poison the handle (nothing is doomed) and a bare
`SAVEPOINT`'s auto-begin is caught by the same predicate. Pinned by
`test/test_wal_checkpoint_txn_740.ml`, file-backed and WAL-mode. **On the
unfixed code that test does not fail, it HANGS** — verified by mutation, and
worth knowing before editing it.

### A pre-#636 stale replay is reported, never refused (#637)

#636 stopped the stale-generation replay from the next successful checkpoint
onward; it does not heal a file already on disk in the bad state, so an
upgrading user gets **one further stale replay** — silently. Two outcomes were
measured on the unfixed code: a database that will not open at all, and — the
dangerous one — a database that opens, answers queries, and is short 365
committed rows.

The decision is **option 1 of the issue: a detector plus a PRAGMA to report**,
in the same spirit as #638's `PRAGMA checkpoint_status`. Refusing to open
(option 2) was rejected: it converts a database that may be perfectly fine into
a hard failure, and without a repair path that is a worse trade than telling the
operator what was seen.

**The signal is header-page `txn_id` monotonicity.** Every commit writes exactly
one header page (page 0 or 1, alternating) with `txn_id = previous + 1`, in the
same batch as its commit-flagged frame, so within one generation the header
frames recovery walks past carry strictly increasing `txn_id`s. A decrease means
the walk ran off the end of the newest generation into the physical remains of
an older one — present in **both** of #636's outcomes.
`Wal.recover_index` records it (no extra I/O: it already walks every frame),
`Wal.replay_check` / `Store.wal_replay_check` expose it, and
`PRAGMA wal_replay_check` renders one row of
`(status, frames_walked, header_frames, detail)`.

**`status` has three values, and collapsing them to two would be worse than
having no detector.** `no_evidence` means "walked at least two header frames and
they increased", never "verified clean" — damage a PREVIOUS open already
replayed into the main file leaves a structurally valid database that no
integrity check finds either, and a post-#636 checkpoint has by then physically
destroyed the evidence. A walk with fewer than two header frames answers
`not_examined` rather than pretending to `no_evidence`; so do the in-memory
backend and a non-WAL store. The `detail` column carries the caveat in words,
because a bare status is exactly the thing that gets over-read.

**Only a regression at or below the last commit frame is reported.** A frame
checksum covers `(salt, seed, page_id, flags, page)` and **not** the frame's
index, so leftovers from an earlier, longer write verify wherever they sit —
including the tail of a batch a crash tore in half before its commit frame was
written. Recovery walks such a tail and then discards it for want of a commit,
so the database is correct; flagging it would make the detector cry wolf on
ordinary crash recovery. The stale frames #637 is about were *applied*, and
therefore sit within the committed prefix. `an_unapplied_stale_tail_is_not_reported`
in `test/test_wal_replay_check_637.ml` pins that, and fails by mutation if the
restriction is removed.

Three things it still cannot see, recorded so nobody reads the PRAGMA as an
oracle: damage replayed at an earlier open (above); a stale remainder that is a
fragment of one old commit batch carrying no header-page frame; and a database
that will not open at all — loud by construction, but no PRAGMA runs on it.

`test/test_wal_replay_check_637.ml` is file-backed and WAL-mode for every
PRAGMA case (the Mem backend has no WAL, so an in-memory version passes while
measuring nothing). Its headline case,
`a_pre_636_file_is_detected_end_to_end`, rebuilds the pre-#636 physical state
out of ordinary SQL plus two byte-level writes to the `-wal` file: commit a long
generation, checkpoint it into the main file, restore that generation's 24-byte
header — its unrotated `(salt, seed)` marker — over the truncated WAL so the
next writer starts at frame 0 under the same marker, write a short successor
there, and splice the old generation's tail back on behind it. The fixed code
cannot be talked into producing that state, which is the point of #636; every
spliced frame still verifies, which is the point of #637.

### `Db` hands out a read-only schema projection, never the catalog (#433)

`Db.catalog : t -> Catalog.t` is **gone**, replaced by
`Db.schema : t -> Schema.t` (`lib/db/schema.mli`, module `Granary.Schema`).
`Catalog.t` is a read-**write** surface — `create_table`, `drop_table`,
`add_column`, `rename_table`, `set_index_stats`, `set_last_inserted_rowid`,
`set_fk_enforcement`, and `Catalog.store` (which reaches the raw `Store.t`) all
take a `t` — so handing it out let a consumer mutate the in-memory schema, or
write to the store, **out of band from SQL DDL and the WAL**. The constraint
used to live in the accessor's doc comment; it is now a type.

**The projection is a live VIEW, not a snapshot, and that is the load-bearing
part.** `Schema.t` *is* the handle's `Catalog.t` behind an abstract type
(`Schema.of_catalog` is the identity; there is deliberately no inverse), so DDL
run through SQL is visible through a projection taken before it. A copy would
have been a second piece of catalog state that goes stale — the shape #589 and
#633 are about — and this one would have gone stale silently, since nothing
invalidates a value the caller is holding.

**What it exposes**, and nothing else: `list_tables` / `find_table` (returning
`Schema.table` = name, `Row.column list`, `fk_constraints`, `without_rowid`,
`columnar`), `table_exists`, `indexes_for_table` / `find_index` (returning
`Catalog.index_info`, an immutable record carrying #576's statistics),
`index_exists`, plus `pp` / `pp_table`.

`Schema.table` is a **projection, not `Catalog.table_meta`**, and the reason is
not tidiness: `table_meta.storage`'s `Columnar` arm carries a
`Col_store.t`, which is mutable, so re-exporting `table_meta` would have left a
real out-of-band write path open under a type that claims to be read-only. Two
residual sharp edges are accepted and documented rather than deep-copied around
(a copy would be the snapshot this design rejects): `index_stats.range_histograms`
and `histogram.boundaries` are `array`s, so their elements are assignable in
place. They are planner statistics, not schema, and mutating one changes a cost
estimate rather than what the engine believes the schema is.

**`Db.plan : t -> string -> (Plan.op, error) result Lwt.t` is the other half of
the change.** Planning a statement against a handle's own schema was the one
legitimate use of the live catalog that a projection cannot serve — `Sema.bind`
and `Planner.plan` both take a `Catalog.t` — and the planner's own tests
(`test_range_histogram_576`) did exactly that. It parses, binds and plans
without executing, opens no transaction and writes nothing, and routes the way
`execute` does. **The alternative was worse**: a test could otherwise only reach
a `Catalog.t` by opening a *second* one over the same store, which is the
duplicated-catalog anti-pattern the #589/#633 sections exist to prevent.

No deprecated alias was kept. A compatibility shim re-exposing the mutable
handle under another name would defeat the whole change, and the only known
consumer outside this repo is camel's hook type environment (`tej/camel#67`),
which uses `list_tables` and per-table `columns` — both of which the projection
carries. `test/test_readonly_catalog_433.ml` pins every accessor, the live-view
property (including a rolled-back `CREATE`), and `Db.plan`; the removal itself
is not testable — it is a compile-time property, checked by every consumer that
builds.

### An FTS `rank` projection scores the whole match set (#689)

`Exec.fts_score_matches` runs over the FULL, deduplicated match set before the
sort that LIMIT/OFFSET slices, and it has to: BM25 needs every score before any
window can be chosen. That is the difference from #687, which could move the
content fetch *after* the slice because content is not an input to the score.
So the work here is made cheaper, never truncated.

**The issue's own premise did not survive measurement, and that is the main
thing to know before touching this again.** #689 was filed against the per-match
`S.get` for `doc_length`. That call is real and exactly one per match, but on a
4 000-match single-term rank query it accounted for ~0.9 ms of a 288.8 ms query.
The other 99% was `List.assoc_opt rowid term_pl` inside the score fold — a linear
probe of the term's posting list, once per match, i.e. O(matches x postings x
terms), quadratic in the match count and allocating nothing to show for it. It is
now a hashtable, built from the reversed list with `Hashtbl.replace` so the FIRST
entry for a rowid wins exactly as `List.assoc_opt` did.

Measured (`Gc.minor_words` around the query, plus wall time; allocation is the
load-insensitive half):

| dense rank query, 4 000 matches | before | after |
|---|---|---|
| in-memory, wall | 288.8 ms | 10.4 ms |
| file-backed, wall | 442.2 ms | 17.1 ms |
| file-backed, minor words | 4 370 993 | 2 555 495 |

Per-query allocation is now exactly linear in the match count (641 825 /
1 283 014 / 2 555 495 words at 1 000 / 2 000 / 4 000 file-backed matches — 2.00x
then 1.99x per doubling). The residual wall-clock superlinearity on disk is the
pager working set, not the algorithm.

**The `doc_length` fetch itself is now gated on selectivity, and the gate is
computed for free.** `Exec.fts_doc_lengths` picks between one point `S.get` per
match (`fts_doclen_by_get`) and ONE cursor walk across the doc-length key region
(`fts_doclen_by_scan`). The region is contiguous — `fts_doclen_prefix` is
`"\x00\x01"`, the stats key `"\x00\x00"` sorts below it and every posting key
`term ++ "\x00" ++ rowid` above it — and holds exactly one entry per indexed
document. Both strategies read the same keys with the same value decoder and the
same "absent means length 1" default, so they are interchangeable and the choice
is purely about cost.

`Exec.fts_doclen_scan_ratio` (default 5, overridden by
`Exec.set_fts_doclen_scan_ratio`) is the threshold, derived rather than guessed: a point `S.get` for one doc length costs ~573 minor words on the B-tree
backend against ~112 for a `seek_next` step, so the walk wins while it crosses
fewer than ~5.1 entries per match. Both sides of that trade are real and were
measured — 3 matches among 4 000 documents cost 642 663 minor words walking
against 19 325 point-fetching (33x worse), while 4 000 matches among 4 000 cost
2 555 506 walking against 4 399 003 (1.7x better). The estimate the gate uses is
`min(rowid span, total_docs)`, an UPPER bound on the entries a walk would cross,
and both halves are already in hand — `total_docs` from the `read_fts_stats` the
function already does, the bounds from a fold over an in-memory list — so the
walk is taken only when even its worst case is cheaper, at no I/O cost to decide.

**On the `Mem` backend the two strategies measure the same** (324.9 vs 323.4
words per match, observed), because `S.get` there is a `Bytes_map` lookup rather
than a root-to-leaf descent. Any measurement of this must be **file-backed** or
it reports that the change does nothing;
`test/test_fts_doclen_689.ml`'s `measure_words_per_match` is (963.9 -> 646.7
words per match at 800 documents). That measurement **prints and does not
assert** unless `GRANARY_FTS_DOCLEN_MAX_WORDS_PER_MATCH` is set, and it is armed
in **no** workflow — one box and one backend is not enough to put a ceiling on
every PR, which is the convention `test_scan_borrow_481` establishes. It is not
in the gate tables above because it is not a gate anywhere.

The rest of that file is load-insensitive by construction: every case runs the
same statement over the same data twice, once with `set_fts_doclen_scan_ratio 0`
(never walk) and once with it forced high, and requires byte-identical output —
same rows, same order, same rank floats. That setter exists for exactly that and
production never calls it. The deletion case matters: `fts_deindex_document`
removes a doc-length key, so the walk crosses a HOLE the point path simply never
asks about.

**Two things in the issue text are wrong and should not be carried forward.**
There is no `ORDER BY rank`: `Sema.bind_select` refuses **any** `ORDER BY` on an
FTS table ("FTS tables do not support this query form"), and a bare `MATCH` does
**not** sort by score — `include_rank` is false there and no scoring runs at all.
The only spelling that reaches this code is projecting the virtual `rank` column,
`SELECT body, rank FROM doc WHERE doc MATCH '...'`, which implicitly sorts by
score descending.

**What was NOT done, deliberately.** The issue's option 1 — storing `doc_length`
inline with every posting entry — is still an on-disk format change needing a
version tag on `fts_table_meta` (there is none) plus a migration, and the
measurement no longer justifies it: after the two fixes above, the doc-length
fetch is a minority of a query that is 25x faster than when the issue was
written. And `fts_execute_query`'s `FQ_and` intersection is still
`List.mem r ids` over rowid lists, so a MULTI-term `MATCH` is still quadratic
(0.974 s -> 0.185 s at 4 000 matches from the score-fold fix alone, but still
~4x per doubling). That is the same defect class in a different function and is
tracked separately.

### A tree's root is a function of the committed state, not of the snapshot (#416)

`Store.bt_state` memoizes `tree_id -> data-tree root page` **across** RO
snapshots, tagged with the committed generation it describes — the pair
`(header txn_id, meta root page)`. A generation mismatch resets the whole memo
before anything is served from it, so at most one committed state is ever
described, and the publish after the meta descent re-checks the generation
because that descent awaited.

**The soundness argument is the one `rs_snap_trees` already rested on, not a
new one.** A snapshot resolves each tree once and reuses that root for its
whole life however many commits happen meanwhile, so a tree's root is already
treated as a function of the committed state the snapshot reads. Two snapshots
at the *same* committed state therefore observe the same roots by definition;
all this change does is identify that state by `(txn_id, meta root)` rather
than by snapshot identity. `commit` advances `txn_id` monotonically and never
reuses a value, and `rollback` leaves both the id and the root page alone while
reverting the meta tree to that root (#382), so an aborted txn cannot strand an
entry. `ro_begin_as_of` resolves its snapshot from a *history record*, which is
why the meta root is checked alongside the txn id and why the memo has to reset
**backwards** as well as forwards — alternating an as-of read with a live one is
the case a single-generation memo has to keep re-deriving, and it is pinned.

Three consequences worth knowing:

- **A memo hit skips the meta descent, so the meta pages are no longer pinned
  into `rs_pinned` on that path.** That is fine and is not what pins are for
  here: the checkpoint's RO gate is `active_reader_frames` (registered at
  `ro_begin`), not the pin set, and a page nobody reads needs no pin. Anything
  that makes a pin load-bearing for *correctness* rather than for cache
  retention owes this path a second look.
- **Interleaving snapshots at different generations degrades to the old cost,
  never to a wrong answer** — each transition resets the memo. That is the
  accepted trade for a bounded, single-generation table.
- **The memo hangs off `Store.t`, so every catalog and worker handle over one
  store shares it** — deliberately, and for the same reason as #633's rowid
  counters: a tree id is an identity within one store. It is unsynchronised,
  which is sound under Lwt's cooperative single-domain scheduling because the
  generation check, the reset and the lookup that follows it sit in ONE
  synchronous block with no await between them, and the publish after the
  meta descent re-checks. Nothing in `lib/` spawns a domain (`Parallel` has no
  call sites there), and the per-store `active_readers` / `trees` / `tree_tags`
  hashtables already rest on the same assumption. **Anything that runs reads on
  a second domain owes this table a lock**, along with those three.

The measured effect, and the other half of #416's re-measurement, are recorded
in `test/test_point_lookup_alloc_416.ml`'s header: a warm
`SELECT payload FROM t WHERE id = ?` went 1419.9 -> 950.9 words/lookup, of which
~214 is this memo and ~231 is fusing `Op_project` into `Op_rowid_lookup`
(`Lwt_stream.map` builds a whole second stream over a one-row one, and its
source is the *async* `Lwt_stream.from`; `project_row` is a pure ordinal
selection, so applying it to the at-most-one row is observationally identical).
**#416's own 2026-06-19 attribution is stale on both points** — it priced the
meta descent as "≈0, the meta tree is tiny/hot" and never costed the
`Lwt_stream.map` layer at all. The terms it *did* identify as dominant are
still dominant and still unaddressed: ~420 words in the data-tree descent
(async multi-level Lwt bind chains, needing the synchronous cache-resident fast
path) and ~160 in `ro_begin`/`ro_end`. The issue's "well under 500
words/lookup" target is open.

One stale thing in #416 itself, so nobody hunts for it: its acceptance criteria
ask to re-run `test/bench_multicore_read_headroom.ml`. **That file does not
exist in the tree** (nor does any other `Domain.spawn` site outside
`lib/parallel`, which has no callers), so that criterion cannot be met as
written.

`test/test_ro_root_memo_416.ml` pins the invalidation (commit, DDL, rollback,
as-of/live alternation) and the fused projection; both halves were
mutation-checked — disabling the generation test fails five of its six cases,
dropping the ordinals fails five as well.

### One `Db.t`, one explicit transaction (#555)

A `Db.t` carries a single explicit-transaction slot and every statement resolves
its transaction from it. **Autocommit sharing across fibers is fine and stays
fine** — `test_concurrent_rmw_223.ml` and `test_multifiber_stress.ml` both share
one handle across fibers and are correct. Explicit transactions are the problem:
two fibers cannot hold two.

Since #555 a `BEGIN` that arrives while another explicit transaction is active
fails *and* **poisons the handle**. While poisoned, every statement — read,
write, DDL, `SAVEPOINT`, prepared `run`/`iter`, and `COMMIT` — is rejected with a
`Runtime` error. `ROLLBACK` is the sole exit: it aborts whatever transaction was
in flight and clears the poison, after which the handle is fully usable.
`Db.transaction_poisoned` exposes the flag.

**The poison narrows the hazard; it does not close it.** It covers the window
between the failed `BEGIN` and the first `ROLLBACK` — and `ROLLBACK` is the
prescribed recovery, so the window closes by design. Past it the original
contamination is reachable with the fibers exchanged: the recovering fiber
`BEGIN`s afresh, and the fiber whose transaction was aborted — never told —
writes into the new one and commits it. That is **#584**, pinned as a canary by
`residual_584` in `test_txn.ml`. The enabling change for a real fix is **#585**
(a scoped `Db.with_transaction`, giving the engine an extent to attach an owner
token to); **#555** option 1 (a session object) is what an OLTP throughput
number needs. Do not read the poison as permission to share a handle.

It also dooms the *winner's* transaction — the engine cannot tell the two fibers
apart, so it cannot let `COMMIT` through without letting the wrong fiber's
`COMMIT` through.

Under ATTACH, poisoning is **per-handle**: each attached schema is its own
`Db.t` with its own slot, `BEGIN` routes to the active schema, and a poisoned
`aux` does not stop statements routed to `main`. Three consequences are wired in
deliberately — `Db.transaction_poisoned` ORs over the attached sub-handles (or
it would answer `false` on a genuinely poisoned connection), and both routing
statements that could carry a caller *away* from a poisoned schema are refused:
`PRAGMA active_database = …` (or the recovering `ROLLBACK` would route to the
wrong schema and answer "no active transaction" while the poisoned one still
held its writer lock) and `DETACH DATABASE` (which would otherwise drop the
sub-handle and its transaction — a clean outcome, but a *second* exit from the
poisoned state, making "ROLLBACK is the sole exit" false).

**#555's "arriving at a poisoned schema stays legal" valve no longer exists.**
It was there so a schema poisoned from elsewhere could still be reached to be
rolled back. Since #598, arriving requires a switch, a poisoned schema by
construction holds a transaction, so the #598 gate refuses the arrival too. The
state is unreachable today — a poisoned schema is always the one the caller is
already on, because that is where their `BEGIN` went — so nothing is stranded.
But anything that makes a poisoned schema reachable from elsewhere (a session
object, #555 option 1) must re-open that path explicitly rather than assume the
valve is still there.

**The ATTACH story is contained, not closed (#598).** #555's poison only fires
on a *collision*, and under ATTACH one `Db.t` legitimately holds two
explicit-transaction slots — so nothing collided when a `PRAGMA
active_database` switch moved the routing out from under an open transaction.
The caller's next write then landed in the *other* database and was durably
autocommitted there, with `transaction_poisoned = false` throughout; the caller
found out at `COMMIT`, after the write was on disk.

Since #598 the two statements that can move a caller's routing are **refused
while any schema on the connection has an explicit transaction open**:
`PRAGMA active_database = …` (unless it names the schema already active, which
is a no-op and stays legal) and `DETACH DATABASE`. The error is a distinct
message from `poisoned_msg` — nothing is poisoned and the caller's transaction
is intact; the statement just cannot be honoured yet. The gate sits *below*
#555's two poison gates in `execute_control_op`, so on a poisoned connection
the caller is still told to `ROLLBACK` rather than told a transaction is open,
and "ROLLBACK is the sole exit" stays true. Pinned by
`test/test_attach_active_txn_598.ml`.

That is the issue's *interim containment*, and it is deliberately blunt: it
buys loudness by removing the ability to switch schemas mid-transaction. The
real fix is the #585 family — bind a statement to the transaction its caller
opened instead of resolving it through shared mutable routing state at
execution time.

**Two deliberate compatibility breaks, both wider than the bug:**

- `PRAGMA active_database` mid-transaction previously worked for a pure **read**
  of another schema, and that is now refused too. The gate cannot tell a read
  from a write ahead of time, and the read case is one statement away from the
  write case that loses data.
- `DETACH DATABASE` is refused while **any** schema has a transaction open, even
  when the detach target itself has none. Narrowing it to the target's own slot
  would be defensible; it is not what is implemented.

### `Db.with_transaction` — the scoped extent (#585)

`Db.with_transaction db (fun db -> …)` BEGINs, runs the body, COMMITs on
success, ROLLBACKs and **re-raises** on exception. An exception is never
converted into `Error`; `Error` is reserved for a failed BEGIN or COMMIT.

Its point is not convenience. It is the first place a transaction has a dynamic
*extent*, so an owner token can live in an Lwt key for its duration — the thing
#555 and #584 both name as the missing prerequisite. The token is minted at
BEGIN, stored on the handle (`txn_scope`) and published into the calling fiber's
Lwt storage (`txn_scope_key`); a fiber owns the transaction iff the two agree,
which is what `Db.in_transaction_scope` reports.

**Nesting is refused, and that is a decision, not an omission.** A nested
`with_transaction` on the same handle — directly, or from a fiber spawned inside
the body, which inherits the token — returns `Error` without opening anything,
without rolling anything back and **without poisoning**. A savepoint would make
the inner scope's "commit" a `RELEASE`, so returning from it would not mean
durable and the outer scope could still discard it. A no-op join would make the
inner scope's rollback-on-exception abort the *outer* transaction while
returning to code that believes only its own work was undone. Refusal is the
only answer that does not lie; `SAVEPOINT`/`RELEASE`/`ROLLBACK TO` inside the
body is the supported partial-undo point.

The clean refusal is also the token's only live decision today, and it is worth
seeing why it is sound: the token *proves* the caller is the fiber that opened
the outer transaction, so there is nothing to contain. The #555 poison exists
precisely for the case where the engine cannot tell the fibers apart, and that
case is untouched — a second *fiber* on a shared handle holds no token, so its
BEGIN collides and poisons exactly as a bare `BEGIN` does.

**The poison contract is preserved, deliberately.** `with_transaction` issues no
`ROLLBACK` when its BEGIN fails, and none when its COMMIT fails — otherwise it
would be a second exit from the poisoned state and "ROLLBACK is the sole exit"
would become false. It rolls back only its *own* transaction, on the
body-raised-an-exception path.

**#584 is narrowed at the scope boundary, not closed.** If another fiber's
`ROLLBACK` aborts this scope's transaction — whether it then refills the slot or
leaves it empty — the token no longer matches; the combinator detects that at
scope exit and issues **neither** COMMIT nor ROLLBACK, returning `Error`, since
either would act on state that is no longer the scope's. It does *not* cover
statements *inside* the body: those still resolve the transaction from the
handle's mutable slot, so a displaced scope's writes land in the other fiber's
transaction before the boundary check reports the loss. Binding statements to
their owner is #555 option 1's work. **This is not permission to share a handle
across fibers** — `create_worker_handle` still is.

**The token must be invalidated by every path that ends a transaction, not just
by `with_transaction`'s own exit.** This shipped wrong once and the failure was
the guard's own headline case: `txn_scope` was written only by the combinator,
so after `B: BEGIN` (collide) → `B: ROLLBACK` (A's transaction aborted) →
`B: BEGIN` (B's transaction now in the slot), A's stale `Some 1` still equalled
the handle's `Some 1`, and A's scope exit COMMITted **B's** transaction and
returned `Ok`. `stolen_txn_msg` only fired when the displacing fiber also used
`with_transaction` — i.e. never in the spelling #584 is written in. The clears
now sit next to every `explicit_txn` assignment: `begin_txn` and `savepoint_txn`'s
auto-begin clear it when they *fill* the slot; `force_rollback_txn`, `commit_txn`,
`rollback_txn` and `release_savepoint`'s auto-commit clear it when they *empty*
it. Keep them adjacent — a token that outlives its transaction turns the guard
into a false match, which is worse than no guard at all.

**The token lives on the handle the BEGIN routed to** (`active_handle`), not on
the top-level handle, because that sub-handle is the one whose slot those clears
maintain. #598 keeps the routing from moving under an open scope, so the handle
is stable for the extent.

One inherited wrinkle: the rollback-on-exception path issues `ROLLBACK`, which
clears the handle's *poison* flag unconditionally (#555 made it unconditional so
a handle can never be stranded). Poison is connection state, not transaction
state, so an unwinding scope can clear a poison another fiber was told to
recover from; that fiber's own `ROLLBACK` then answers "no active transaction".
Nothing is lost, and making the clear conditional would strand the handle.

The body owns the statements, not the transaction: a `BEGIN` inside it poisons,
and a `COMMIT`/`ROLLBACK` inside it empties the slot *and clears the token*, so
the scope exit takes the displacement branch and returns `Error` without issuing
a second COMMIT. The body's work lands as the body asked; the `Error` is the
caller's only signal that the scope did not end the way it looks like it did.
`SAVEPOINT`/`RELEASE`/`ROLLBACK TO` are fine and leave the transaction in place.
Pinned by `test/test_with_transaction_585.ml`.

### Running explicit transactions from more than one fiber

`Db.create_worker_handle` is the mechanism, and it is sound: it is `of_store`
over the *same* `Store.t`, so each handle gets its own `explicit_txn` while
sharing the store's single-writer `Rwlock`. A second fiber's `BEGIN` **blocks**
until the first commits rather than contaminating it, and neither handle is ever
poisoned. (This also corrects #555's premise that "a second `Db.t` over the same
path would be a second lock with no mutual exclusion" — true of a second
`open_file`, false here.)

**#589 is fixed — writes to every table shape are now safe.** Read the history
anyway, because the fix's invariant is what keeps it that way.

Each handle still gets a *fresh catalog* — that is what makes DDL invisible
across handles — but it no longer gets a fresh **rowid allocator**. The
allocator's live state was lifted out of the cached `table_meta` and into a
tree-id-keyed table that **`Store.t` owns** (`Store.rowid_counters`, #633), so
every catalog opened over one store shares one allocator by construction. Every
read of a cached `table_meta` is patched from that table on the way out and
every write publishes to it on the way in, which keeps `table_meta` the only
type the rest of the engine sees.

**#633 moved the ownership; do not move it back.** It was originally threaded by
hand as `Cat.open_ ?rowid_counters` / `Db.of_store ?rowid_counters`, with
`create_worker_handle` as the only caller that remembered — and the penalty for
the next caller forgetting was #589 verbatim (silent row loss plus durable index
corruption). A tree id is only an identity within one store, so the store is the
only correct home. This also makes ATTACH right by type rather than by accident:
an attached schema is a different `Store.t` and therefore, necessarily, a
different set of counters.

Two rules make the sharing correct and must survive any future edit:

- **Keyed by tree id, not by table name** — a tree id identifies the data tree
  the counter counts for and survives `ALTER TABLE … RENAME`; keying by name
  would let a `DROP`+`CREATE` inherit the dead table's counter. **But a tree id
  is not unique for all time.** It is never reused after a *committed* `DROP`
  (measured: drop tid 16, next `CREATE` gets 18) and **is** reused after a
  *rolled-back* `CREATE` — `next_user_tid_tx` writes the bumped counter inside
  the transaction, so `S.rollback` reverts it and the next `CREATE` gets the
  same id (measured: doomed 17, rolled back, next `CREATE` also 17). **The real
  invariant is therefore not "tree ids are unique" but "the entry is cleared or
  overwritten before the reused id is allocated from"** — and *two redundant
  mechanisms* do that, each sufficient alone (verified by mutation): (1)
  `put_table`'s undo runs `del_meta` → `unpublish`; (2) the replacement `CREATE`
  publishes `empty_next_rowid` under the same tree id, overwriting the stale
  entry. Removing either alone still passes the suite; removing both loses the
  row. `tid_reuse_after_rolled_back_create` and `tid_reuse_worker` guard **the
  pair** — so a green suite is not evidence that the mechanism you are editing
  is dead. Keeping DDL on the `set_meta`/`del_meta` chokepoints keeps both.
- Negative tree ids are skipped (`tree_id >= 0` guard) because they are
  ephemeral/sentinel metas that name no data tree and would all collide on one
  entry. There are four: `-1` for a CTE *and* for a decoded columnar table whose
  stored tid is 0, `-2` for `sqlite_master`, `-3` for `sqlite_sequence`.
- **Open-time seeding uses `seed_table`, not `put_table_durable`** — it will not
  overwrite a counter that is already live. A worker re-reads the catalog off
  disk, and disk is never fresher than the running allocator; clobbering would
  reintroduce the same collision with the roles exchanged, making the **parent**
  go stale. **The rule is absolute: an entry that exists is never overwritten,
  whatever its value.** PR #650 briefly carved out an exception for
  `empty_next_rowid` ("a sentinel has never allocated, so seeding over it can
  only move the counter up") and it was wrong twice over — the sentinel is *also*
  written deliberately by the sqlite_sequence reset paths
  (`reset_next_rowid_in_txn`, `reset_all_next_rowid_in_txn`) *inside* an open
  transaction, so a concurrent `open_` would clobber the reset with the committed
  pre-reset high-water; and the obvious guard (skip tables dirty in
  `rowid_bumped`) does not work, because `rowid_bumped` is **per-cache** — the
  resetting transaction's flag lives on its own cache and the seeding cache is a
  brand-new one whose set is empty. There is no cheap store-wide discriminator,
  so there is no exception.

  **A test that wants a genuine restart must close a file-backed store**, not
  re-open a catalog over a live one: since #633 the latter is a worker handle and
  correctly shares the counter. `test_mirror_recovers_next_rowid` and
  `test_mirror_recovers_negative_next_rowid` in `test_catalog.ml` were converted
  for exactly this reason.

  **Accepted residual:** with no exception, mirror recovery is invisible to a
  *second* catalog over a *live* store whose counter is still the sentinel — the
  recovered `max(rowid)+1` loses to the sentinel the original `CREATE`
  published. Reaching it needs rows in the data tree that the allocator never
  issued *and* a lost `_sys_tables` row, on a still-open store: corruption on a
  live store, not a restart. The rejected alternative silently reverses a
  sqlite_sequence reset and needs no corruption at all. It is the better trade,
  but it is a trade, and nothing in the suite covers it.
- **Every allocator allocates *and publishes* with the writer lock held (#632).**
  That — not the weaker "the allocator holds the lock" — is the property that
  makes the shared table safe, because it is what makes the interval between
  reading the counter and publishing the new one an interval in which nothing
  else can allocate. `next_rowid_in_txn` and `bump_next_rowid_in_txn` have it by
  construction (they are handed a txn). `Catalog.next_rowid`, the autocommit
  allocator, used to publish *before* `S.rw_begin`: another handle could take
  the lock, `ROLLBACK`, and *lower* the counter through #293's recompute,
  discarding an allocation already handed out. It now takes the lock first and
  publishes immediately after the allocation, with no `Lwt` yield in between.

  **`S.commit` does not count as "under the lock", and this is the trap to
  know.** It releases the writer lock *before* its promise resolves —
  `commit_wal` calls `unlock_once ()` (`store.ml:2071`) and only then awaits the
  fsync; the non-WAL arm releases from a `Lwt.finalize` handler
  (`store.ml:2184`). So publishing in a `let%lwt () = S.commit tx in …`
  continuation runs *after* other fibers can take the lock, leaving the shared
  counter too LOW for the whole fsync — the direction that collides. (Too HIGH
  merely skips ids, and is the residual this design accepts if a commit fails.)
  PR #650 shipped that ordering for one round before review caught it.
  **The in-memory backend cannot detect the difference** — its commit releases
  and returns an already-resolved promise (`store.ml:2164`), so the bind runs
  synchronously and both orderings pass. Any test for this class of bug must be
  **WAL-mode and on disk**; `wal_two_fiber_catalog_next_rowid` and
  `wal_two_fiber_inserts` in `test/test_rowid_counter_ownership_632.ml` are.

  The unknown-table `Failure` stays *synchronous* (raised before any `Lwt.t`
  exists) — `test_rowid_unknown_table` in `test_catalog.ml` pins the contract.

  **The ROLLBACK side of the same shared table was not brought under this
  discipline until #706 — same class of bug as #632, different function.**
  `Cat.recompute_rowid_counters_after_rollback` re-derives a rolled-back
  table's counter (`recover_next_rowid`'s RO tree scan, or
  `read_committed_next_rowid` for AUTOINCREMENT) and publishes it via
  `Schema_cache.set_rowid_durable`. The RO scan correctly runs *after*
  `S.rollback` releases the writer lock (its own RO txn would otherwise
  self-deadlock against it), but the **publish** used to run right after with
  no lock at all — an unlocked write into the one counters table every
  catalog over the store shares, exactly what #632 eliminated for the
  allocate path. A second worker handle could `rw_begin` in that window, read
  the still-stale (too-high, not-yet-recomputed) counter, allocate from it,
  and commit — and the recompute's blind publish would then clobber the
  counter back down past that commit's id, so the next allocation reissued it
  and silently overwrote the row. Reliably reproducible with
  `GRANARY_TPCC_TERMINALS >= 2` once `Tpcc_driver`'s terminals got genuine
  concurrency (#703): NewOrder's ~1% rollback rate raced a sibling terminal's
  concurrent allocation on `new_order`/`order_line` on nearly every run.

  Unlike #632, re-acquiring the lock around a **blind** overwrite is not
  enough, and would break the common (no-race) case: the value already
  sitting in the shared table when the recompute is ready to publish is
  usually this *same* rollback's own stale bump — the value the recompute
  exists to correct — so "the live value differs from what I'm about to
  write" cannot tell that apart from "a concurrent commit already moved
  it". The fix is a compare-and-swap, `Schema_cache.cas_rowid_durable`:
  capture `expected`, the live counter as of the *start* of the recompute
  (before either RO scan runs); after the scan, re-acquire the writer lock
  and publish the recomputed value only if the live value still equals
  `expected`. A match means nothing else touched it — safe to lower, same as
  before #706. A mismatch means a concurrent commit already advanced it past
  what this recompute saw, and the recompute leaves it alone; the "wasted"
  ids between the recomputed value and the live one are the same accepted
  "too HIGH" residual #632's own writeup names, never a reissued one. All of
  a rollback's bumped tables are scanned first (unlocked), then published
  together under a *single* `rw_begin`/`rollback` bracket (not `commit` —
  nothing is written to any tree, only the in-memory counters table is
  republished, and `rollback` is the cheaper release: no header bump, no WAL
  frame).

  `test/test_rollback_recompute_publish_race_706.ml` pins it via
  `Cat.rollback_recompute_publish_hook`, a test-only seam (production code
  never assigns it) awaited at exactly the point between the RO scans
  finishing and the lock being re-acquired to publish — deterministic
  interleaving of a second handle's competing allocation, rather than relying
  on real scheduling timing the way the TPC-C repro does. Must run **WAL-mode
  and on disk**, for the same reason as #632's own tests: the in-memory
  backend's RO scan does no real (yielding) I/O, so nothing would interleave
  with it even without the hook.

What it used to do, and what the tests now assert the opposite of — two counters
over one data tree, neither invalidating the other, so an `INSERT` with an
engine-assigned rowid reused a rowid the other handle had already committed and
silently overwrote it:

| shape | before #589 | now |
|---|---|---|
| `CREATE TABLE t (b TEXT)` — plain rowid, the commonest shape | 1 row where there should be 2 | 2 rows |
| `a INTEGER PRIMARY KEY`, engine-assigned | 1 row | 2 rows, ids 1 and 2 |
| `a INTEGER PRIMARY KEY`, **caller-supplied** values | correct | correct |
| `a INTEGER PRIMARY KEY AUTOINCREMENT` | 1 row; `sqlite_sequence` reads `1` on both handles | 2 rows; `sqlite_sequence` reads `2` on both |
| `k TEXT PRIMARY KEY`, caller-supplied keys | 1 row **plus index corruption** | 2 rows, seeks correct |
| `WITHOUT ROWID` | correct | correct |

`TEXT PRIMARY KEY` was the worst case because the consequence was **wrong query
answers**, not row loss: the index kept a phantom entry for the overwritten key
pointing at the reused rowid. It was also symmetric and unbounded (a worker
created *after* the parent's rows snapshotted correctly, then the parent went
stale), persisted to disk, and was unaffected by explicit transactions — never a
race, so the writer lock was irrelevant. The whole matrix plus three
handles/five inserts, a rollback, and a close/reopen is pinned by
`test/test_worker_handle_589.ml`; `worker_handle_text_pk_corruption` and
`worker_handle_stale_rowid_counter` in `test_txn.ml` keep the issue's own two
sequences.

**What is still unsafe about a worker handle** — the DDL/scoping items below do
not corrupt anything, but the first one, found while implementing #703, does:

- **#706 (open, found via #703): `ROLLBACK`'s rowid-counter recompute publishes
  outside the writer lock and can race a concurrent worker handle's allocation
  on the same tree, reproducing #589's exact symptom (a reused engine rowid,
  second `S.put` silently overwriting the first row).** `force_rollback_txn`
  calls `S.rollback` — which releases the writer lock as its last synchronous
  step — and only THEN calls `Cat.recompute_rowid_counters_after_rollback`,
  deliberately outside the lock (its RO scan would deadlock inside `rw_begin`
  otherwise). That recompute's publish
  (`Schema_cache.set_rowid_durable` → `Catalog.publish`) is an unconditional
  `Hashtbl.replace` on `Store.rowid_counters` — no compare-and-swap, no
  re-acquisition of the lock. If a sibling worker handle begins, allocates from
  the same tree, and commits in the window between the rollback's unlock and
  this recompute's publish, the publish clobbers that legitimate allocation
  back down to a stale, lower value, and the next `INSERT` reissues an
  already-used rowid. This is the *same* mechanism the "#632" bullet above
  documents as fixed for `Catalog.next_rowid` — "every allocator allocates
  *and* publishes with the writer lock held" — except the rollback-recompute
  path was never brought under that discipline, because nothing before #703
  drove genuinely concurrent worker handles through a multi-statement,
  sometimes-rolling-back transaction on the same table under load. TPC-C's
  NewOrder profile's spec-mandated ~1% invalid-item `ROLLBACK` (after already
  bumping `orders`/`new_order`/`order_line`'s counters earlier in the same
  transaction) is exactly the shape that exposes it, and it reproduces
  reliably — not a flake — at `GRANARY_TPCC_TERMINALS` 2, 4, 8 and 16 (never at
  1, where there is no second handle to race). See #706 for the full
  repro/analysis. **Until #706 is fixed, do not treat a multi-terminal
  `Tpcc_driver`/`bench_tpcc` run — or any other workload that rolls back a
  bumped rowid table concurrently with a sibling worker handle's writes — as
  producing a consistent database; always check its own consistency oracle
  before trusting output from such a run.**
- **DDL on one handle is invisible to the other's schema cache.** This one is
  inherent to the per-handle catalog and was *not* fixed: create a table on the
  parent and the worker cannot see it until reopened. Pinned by
  `ddl_still_invisible_across_handles`.
- Reactive views, ATTACHed schemas and the active schema are per-handle; a
  worker starts with none of the parent's.
- Write transactions *serialize* on the shared lock rather than overlapping, and
  a read-only transaction does not overlap a writer either. Genuine write
  concurrency still needs #555 option 1.
- Anything else that reaches `Db.of_store` over an **already-open** store owes
  it `~cohort` by hand; `create_worker_handle` is the only caller that does so
  today. It no longer owes `~rowid_counters` — see below.

`Db.of_store` over an already-open store no longer has a rowid-allocator
argument to forget, as of #633: the allocator hangs off `Store.t`, so naming the
same store *is* sharing it. `test/test_rowid_counter_ownership_632.ml` exercises
that path directly (bare `of_store`, no `create_worker_handle`) alongside the
#632 rollback cases and the four invariants above. **`~cohort` is the one
argument still owed**, and for a different reason: it is about handle lifetime,
not allocator identity.

**A `VACUUM` on any handle kills every other handle over the same store (#634).**
VACUUM closes the `Store.t`, rebuilds the file and swaps a freshly opened store
into the handle that ran it. Siblings cannot follow: they keep the closed store
*and* the pre-VACUUM `rowid_counters` table. Since #634 that is **loud** — the
issue's option 2, not option 3. Handles over one store share a
`Db.store_cohort`; VACUUM bumps it and re-stamps only the vacuuming handle, so
every sibling is stale and every statement on it is refused with an error naming
VACUUM. `Db.stale_after_vacuum` exposes it.

**`ROLLBACK` is *not* an exit, and that is the deliberate difference from #555's
poison.** A poisoned handle is recoverable because its store is still there; a
stale handle's store is closed, so there is nothing to roll back into. The
`Op_rollback` arm of `execute_control_op` is exempt from the poison gate and
**below** the staleness gate for exactly that reason. The only supported
operation on a stale handle is `Db.close`, which deliberately skips `S.close`
(VACUUM already closed that store; a second teardown touches closed fds).

Two consequences worth knowing before editing this:

- **VACUUM does not move tree ids.** `copy_all_trees` writes each tid to the
  same tid in the rebuilt file, so #589's "counters are keyed by tree id" rule is
  untouched: the vacuuming handle gets a *fresh* counter table (`Cat.open_
  new_store`, no `?rowid_counters`), re-seeded from the copied data — rescanned
  for a plain rowid table, read from the copied `_sys_tables` row for
  AUTOINCREMENT. The table is **replaced wholesale, not remapped**. Anything
  that makes VACUUM renumber trees must remap or clear that table with it.
- **A worker handle cannot itself run VACUUM**, because `create_worker_handle`
  passes no `~file_path` — the statement is refused as "not file-backed" long
  before the cohort is consulted. So the symmetric "worker vacuums, parent goes
  stale" case is unreachable today; forwarding `file_path` to workers would make
  it reachable, and the cohort already handles it.

**Since #703, `Tpcc_driver` no longer runs a one-deep pool.** `test/bench_tpcc.ml`
mints one worker handle per terminal (after the load phase, per the DDL
limitation above) instead of serializing every terminal through a single
`Db.t`. The terminal-count sweep is no longer flat-by-construction — see
`docs/benchmarks/BENCHMARKS-TPCC.md`'s `#703` section for the measured shape —
but see the #706 bullet above: a multi-terminal run currently trips its own
consistency oracle, because #703 is the first thing in the tree to drive real
concurrent worker-handle writers through a workload that rolls back a bumped
rowid table under load.


### The AUTOINCREMENT mirror write is measured, and it stays (#316, decided 2026-09-03)

#314 made an AUTOINCREMENT table's sticky rowid high-water survive mirror
reconstruction by having every counter bump rewrite that table's **mirror**
entry (`Catalog.put_table_counter_tx` -> `put_mirror_tx`) alongside the primary
`_sys_tables` row. The mirror entry re-encodes the FULL schema — name, tree id,
fingerprint, every column, the FK block — so #316 recorded the worry that a
write-heavy AUTOINCREMENT workload pays a serialize-and-`S.put` of the whole
schema blob per allocated rowid, growing with the table's width. It was filed
`deferred`, with "no action needed unless an AUTOINCREMENT-heavy write benchmark
regresses".

**Measured rather than optimised**, per that condition.
`test/test_autoinc_mirror_316.ml` reports three quantities per inserted row —
minor words, WAL bytes, and the mirror blob's own size — for a 3-column and a
30-column table, plain rowid vs AUTOINCREMENT, in autocommit and inside an
explicit transaction. Every figure is a count or an allocation, never a clock,
so a loaded box does not move it; the numbers below were byte-identical across
repeated runs (400 rows per point, WAL mode, on disk, autocheckpoint disabled
for the measured window so the WAL only grows).

| autocommit | minor words/row | WAL bytes/row | mirror blob |
|---|---|---|---|
| width 3, plain | 11 679.4 | 43 157.0 | 69 B |
| width 3, AUTOINC | 13 988.0 | 51 397.0 | 71 B |
| width 30, plain | 16 920.4 | 44 506.3 | 386 B |
| width 30, AUTOINC | 21 401.6 | 52 746.3 | 388 B |
| **explicit txn** (400 rows in one BEGIN/COMMIT) | | | |
| width 30, plain | 7 311.9 | 164.8 | 386 B |
| width 30, AUTOINC | 7 318.0 | 175.1 | 388 B |

Three findings, and the decision rests on all three:

- **The WAL delta is +8 240 bytes/row — exactly two 4 120-byte frames — and it
  is IDENTICAL at 3 and at 30 columns.** The durable half of the cost is the two
  pages the mirror `S.put` dirties, not the size of the blob it serialises. So
  the issue's optimisation 1 (encode the counter as a small standalone record
  keyed separately) would leave it **untouched**: a put of 8 bytes dirties the
  same pages as a put of 388. That is the finding that matters most, because
  option 1 was the "least semantically loaded" candidate and it turns out to
  attack the minority half.
- The allocation delta does grow with the width, 2 308.6 -> 4 481.2 words/row,
  but only **1.94x for a 10x wider schema and a 5.5x bigger blob**. The
  re-encode is a minority of it; the 4 KB page and WAL-frame machinery around
  the put is the rest.
- **Inside an explicit transaction the whole thing costs +6.1 words/row and
  +10.3 bytes/row** — one extra WAL frame for the entire 400-row transaction —
  because #347's `~defer_counter` already coalesces the counter write (primary
  row AND #314 mirror) to COMMIT. That is the issue's optimisation 2, **already
  in place wherever it can mean anything**; in autocommit "commit time" IS per
  row, so there is nothing left for it to coalesce.

Net: 19-27% over a plain rowid insert, confined to the autocommit single-row
path, which already spends ~43 KB of WAL per row on per-statement transaction
machinery before AUTOINCREMENT is mentioned. Only optimisation 3 (refresh the
mirror counter every N bumps) would remove the 8 240 bytes, and it buys ~16% of
an un-batched insert in exchange for a durability semantics change. **Closed as
measured-and-acceptable; batch the inserts.** Anyone reopening this owes a
workload where the cost is NOT dominated by autocommit's own per-statement
transaction, and should note that the width-scaling premise the issue was filed
on is the half that did not survive measurement.

The same test pins the invariant the per-row write exists to guarantee, so a
future optimisation cannot quietly trade it away: an AUTOINCREMENT high-water
survives losing its `_sys_tables` row and being reconstructed from the mirror.
That case must be **file-backed** — the Mem backend never exercises mirror
reconstruction meaningfully.


### `-0.0` and `+0.0` encode to the same index key (#754, decided 2026-09-05)

Split out of #743 as the one residual its cross-numeric JOIN KEY fix could
not close. `Float.compare (-0.) 0.` is `0`, so `Exec.compare_values` — and
therefore `=`, since #738 — has always said `0`, `0.0` and `-0.0` are all
equal. `Index_key.encode_value`'s `IK_real` arm disagreed: it deliberately
encoded `-0.0` strictly BELOW `+0.0`, so the index's byte order was IEEE's
total order applied literally to the bit pattern, rather than the coarser
equivalence `compare_values` uses.

An equality conjunct on an indexed column is CONSUMED by the access path —
`Planner.residual_filter` drops it once the seek is built, so the seek IS the
answer and nothing re-checks the rows it yields. `WHERE b = 0.0` against an
indexed REAL column therefore returned zero rows whenever the only matching
stored value was `-0.0`, while the identical unindexed query correctly
returned it (verified on `main`, 2026-09-03, before this fix). #743's
nested-loop join probe inherits the same seek and so inherited the same gap;
its hash-join arm keys on `Exec.join_key_value`, which already canonicalises
an integral REAL — `-0.0` included — to `IK_int`, so the hash arm already
agreed with `compare_values` and was not the side that needed fixing.

**Three options were on the table, and the issue asked that all three be
argued through rather than picked by default:**

1. **Make `Index_key.encode_value` encode `-0.0` as `+0.0`** — chosen.
2. Normalize `-0.0` to `+0.0` at value-ingress points (the parser's unary
   minus on a float literal, and `row_value_to_index_value`) — rejected.
3. Leave the encoding alone and make every equality-seek site probe both
   possible keys for a literal `0.0`/`-0.0`, unioning the results — rejected.

**Option 1 was chosen, and it is deliberately NOT the same move #578 made,
even though the issue text and #743's own note both describe it as "as #578
did for NaN."** #578 gave NaN a brand-new tag byte (`0x01`) that no other
value had ever used — a value gaining its own key, disagreeing with nothing
that existed before. This fix is a different shape: `-0.0` and `+0.0` already
had distinct keys, and the fix makes them collide on purpose, because at the
value level (`compare_values`) they were always the same value. That
distinction matters for what kind of change this is: #578 could not corrupt
any existing on-disk index, because no pre-#578 row had ever been encoded
with tag `0x01`. This fix CAN — a database written before it, holding a
`-0.0` in an indexed REAL column, has that row's index entry under the OLD
bytes (`-0.0`'s bit pattern sign-flipped to `0x7FFF...`), and a POST-fix seek
for `0.0` builds the NEW bytes (`0x8000...`) and will not find it. That is a
genuine on-disk index-format change, not merely an additive one.

**The mechanism is a bit-pattern normalization inside `encode_value`'s
`IK_real` arm** (`lib/encoding/index_key.ml`), not a value-ingress rewrite:
before the existing sign-magnitude transform runs, `Int64.bits_of_float f`
is compared against `Int64.min_int` — the exact IEEE bit pattern for `-0.0`
(sign bit set, every other bit zero, and nothing else has this pattern) — and
replaced with `0L` (`+0.0`'s bits) when it matches. The transform that
follows is otherwise untouched, so `-0.0` now takes the same path `+0.0`
always did and the two produce byte-identical keys. `decode` is unchanged:
it only ever needs to invert bytes this encoder now produces, and a stored
`+0.0` key decodes to `+0.0` — the sign bit a caller supplied for `-0.0` is
lost once it passes through an index key, exactly as NaN's specific bit
pattern was already lost through the single-byte `0x01` tag (#578). The row
STORE (`Row.encode`/`decode`) is untouched and still preserves `-0.0` bit-for
-bit; only the index-key projection of a REAL value collapses the sign of
zero, which is the same asymmetry #578 accepted for NaN's payload.

**Why option 1 over option 2 (value-ingress normalization).** Option 2 would
silently rewrite a value the caller supplied — a stored `-0.0` would become
`0.0` the moment it was written, not only when read back through an index.
That is observable through `SELECT` on an UNINDEXED column, a covering read,
or a re-`SELECT` of the literal row, none of which #754 is about — the bug is
specific to a *seek* silently missing a row, not to the row's stored value
being wrong. Option 2 would also have to hit every ingress point (the
parser's unary-minus-on-float-literal production, AND
`row_value_to_index_value`, AND any bound-parameter path), and miss one and
the collapse is inconsistent depending on how the value arrived — a worse
failure mode than the bug it fixes. Option 1 fixes the actual site of
disagreement (the index-key projection) and touches no value the engine ever
hands back to a caller unindexed.

**Why option 1 over option 3 (seek both keys).** Probing both `+0.0` and
`-0.0` keys for a literal `0.0` avoids any format change, but it has to be
threaded through every equality-seek site that can name a REAL literal
matching zero — `Exec.index_lookup_values`'s direct REAL arm, its two
cross-numeric arms (an INTEGER `0` probing a REAL column, and vice versa),
`check_insert_unique`'s conflict probe, `unique_violation_on_update`, and the
`CREATE UNIQUE INDEX` build-time duplicate scan — each returning a UNION of
two lookups instead of one. That is strictly more code, at more sites, for a
one-value special case, and it does not even close the gap fully: it fixes
equality but leaves a RANGE seek's boundary handling to reason about two
keys at one point on the number line, which `range_bound_key` does not do
today and would have to. Option 1 fixes the single root cause once and every
consumer of `Index_key.encode_value` inherits the fix for free — the WHERE
seek, both #743 join-probe arms, the UNIQUE build probe, and
`unique_violation_on_update` — with no site-by-site change and no risk of
missing one.

**No migration path exists and none is being added, matching the project's
young-project stance and #578's own precedent.** Granary has no shipped
databases with a compatibility contract to preserve; #578 changed the
on-disk meaning of a stored NaN's index key with no rebuild tooling, and
this follows the same call. A database that already holds an indexed `-0.0`
built before this change needs its index rebuilt (`DROP INDEX` /
`CREATE INDEX`, or a table rewrite) to have that row's key take the new
bytes; nothing detects or forces this automatically. If granary starts
shipping databases with an on-disk compatibility promise, this is one of the
changes an index-versioning or migration story would need to account for
retroactively.

**As a consequence, the UNIQUE-index probe gained a real correctness fix,
not just a symmetric one.** Before this change, a UNIQUE index over a REAL
column could hold BOTH `0.0` and `-0.0` — two different encoded keys — even
though `compare_values` (and every other type's UNIQUE enforcement, which
routes through the same encoding) has always treated them as one value. That
was a genuine constraint hole, not merely an omission; #754 closes it as a
side effect of fixing the seek, because `check_insert_unique`,
`unique_violation_on_update` and the `CREATE UNIQUE INDEX` build probe all
read `Index_key.encode_value` bytes as an equality test, exactly like the
WHERE seek does.

`test/test_negative_zero_754.ml` pins the issue's exact repro (indexed vs.
unindexed `WHERE b = 0.0` and `WHERE b = 0`, the "unoptimizable foil"
`WHERE b + 0 = 0.0` as a regression control, and an explicit
indexed-vs-unindexed agreement check), the #743 join-probe residual (hash
arm already correct, nested-loop arm now fixed), and the UNIQUE-index
constraint hole in both insert orders. `test/test_index_key.ml`'s
`-0.0 < +0.0` unit test is now `-0.0 encodes same as +0.0`, and its
`encode/decode roundtrip` QCheck property is widened the same way #578
widened it for NaN: a generated `-0.0` is allowed to decode back as `+0.0`,
rather than requiring identical bits. `test/test_join_key_743.ml`'s
`negative_zero_is_a_known_residual_of_the_index_encoding` — which pinned the
gap as an accepted limitation — is replaced by
`negative_zero_now_matches_across_all_three_execution_paths`, which asserts
the fixed behaviour through the same `check_all_three` harness every other
case in that file uses, rather than leaving a "known limitation" test
standing once the limitation is gone.

**Two gaps surfaced in PR review before merge, both on paths the original
diff did not reach, and both are now closed.**

1. **No format-version bump.** The original PR described this as "the same
   call #578 made" but never actually MADE that call: `#578` bumped
   `Header.current_format_version`/`min_supported_format_version` to v3
   specifically so a pre-#578 file is refused at open time rather than
   silently misdecoded, and this PR left the constant at `3l` with no
   detection at all. Concretely: a database written pre-#754 with an indexed
   `-0.0` has that row's key sitting under the OLD bytes; opening it with a
   post-#754 binary and seeking `WHERE b = 0.0` builds the NEW bytes and
   silently fails to find it — the very bug this issue exists to fix,
   reintroduced for pre-existing data via the one channel (opening an old
   file) the in-memory test suite never exercises. Fixed by bumping both
   constants to `4l` in `lib/storage/header.ml`, with a new `v4` entry in the
   file's own version-history comment explaining precisely why this
   incompatibility is narrower than v3's (a v3 file's bytes still DECODE
   correctly under v4 — only a `-0.0` row's SEEK reachability is affected —
   but there is no way to tell without a full index scan whether a given file
   has one, so the whole file is refused). Pinned by
   `test_v3_format_rejected_by_v4` in `test/test_header.ml`, added alongside
   the existing generic `test_obsolete_format_rejected` (which also now
   covers this automatically, since it computes its bad version as
   `min_supported_format_version - 1` rather than a literal) — the new test
   pins the exact historical version number `3l` by name so the assertion
   keeps meaning "a pre-#754 file" even through a future version bump.

2. **The covering-index MIN/MAX fast path returned the wrong SIGN.**
   `Exec.run_index_cover_walk` (#674's covering-index aggregate optimisation)
   decodes MIN/MAX's value straight off the index key — that is the whole
   point of the optimisation, to never fetch a table row — but #754's
   encoder now makes a stored `-0.0` decode back as `+0.0` unconditionally
   (the sign is genuinely gone from the key, the same loss #578 already
   accepted for NaN's payload). So `CREATE INDEX i ON t(b); INSERT INTO t
   VALUES (-0.0), (1.5); SELECT MIN(b) FROM t` returned `+0.0` through the
   covering fast path while the identical query without a usable index — or
   with the fast path forced off — correctly returned `-0.0`. This is
   exactly the value-corruption failure mode this document's "why option 1
   over option 2" section warned about ("a stored `-0.0` would become `0.0`
   the moment it was written... observable through a covering read"); it
   recurred anyway because that warning was about VALUE-ingress rewriting,
   and this bug is a READ-path decode, a channel the original analysis did
   not enumerate.

   **Fixed by excluding REAL-typed columns from MIN/MAX's covering-index
   eligibility gate** (`Exec.index_cover_minmax_ok`, in `lib/sql/exec.ml`),
   rather than by fetching the winning row (which would restore correctness
   but silently give up the `rows_examined = 0` guarantee `test_covering_
   index_674.ml`'s `rows_examined_bound` suite pins for every eligible
   shape) or by trying to recover the lost sign bit from the key (there is
   nothing left to recover — the whole point of #754 is that the two bit
   patterns are no longer distinguishable once encoded). A REAL MIN/MAX now
   falls back to the general aggregate path, which fetches the actual row and
   therefore the actual sign (`Row.encode`/`decode` still preserve it
   bit-for-bit; only the INDEX KEY projection of a REAL loses it). INTEGER
   and TEXT MIN/MAX, and every type's COUNT, are unaffected — COUNT never
   reads the value, and no other type has a #754-shaped collapse. Pinned by
   `agrees_with_negative_zero` (the exact repro, fast-path vs. forced-off
   agreement, plus an explicit "`MIN(b)` is `-0.0`, not `+0.0`" assertion) and
   `real_minmax_column_falls_back` (`rows_examined > 0` for a REAL MIN/MAX,
   confirming the new gate is a real exclusion and not vacuously always-true)
   in `test/test_covering_index_674.ml`.

3. **QCheck property-suite gap (non-blocking, addressed anyway).** Both #578
   and #754 are instances of the same class — an index-key encoding
   disagreeing with `compare_values`'s coarser equivalence — and neither was
   ever findable by `test/test_index_key.ml`'s existing QCheck properties,
   which draw from `QCheck.float`'s uniformly-random bit patterns and so have
   effectively zero probability of ever drawing an exact corner value like
   `-0.0`, `Float.nan`, or an infinity. Both bugs were found only by manual,
   issue-driven inspection. Added `prop_encode_equality_matches_compare_
   exactly`: an explicit corpus of corner floats (`0.0`, `-0.0`, `Float.nan`,
   two DIFFERENT NaN bit patterns, both infinities, `min_float`/`max_float`/
   `epsilon`) unioned with ordinary random floats via `QCheck.Gen.oneof`,
   checked against the full BICONDITIONAL `Float.compare a b = 0 ⟺
   Bytes.equal (encode_value (IK_real a)) (encode_value (IK_real b))` — not
   only the one-directional implication #754's shape needs, because the
   converse direction is #578's shape generalised (a coarser key than
   `compare_values` gives UNIQUE a false conflict between values the engine
   considers distinct). It passes today, confirming the biconditional holds
   exactly for `IK_real` after #754; it exists so a THIRD instance of this
   class fails a property test before it ever needs a human to notice the
   symptom.

4. **Stale doc line (non-blocking, fixed anyway).** `docs/plans/2026-05-15-
   phase-1-disk-storage.md`'s original implementation-plan checklist named
   the ordering unit test as `-1.0 < -0.0 < 0.0 < 1.0` — the exact ordering
   #754 removes. Annotated in place rather than silently rewritten, since the
   surrounding document is a historical plan, not living reference material.

### OCaml row-mutation hooks share one store-wide registry, every rollback-undo merges rather than replaces, and a hook's own reentrant nested write fails fast (#752, decided 2026-09-06)

`Db.register_row_hook`/`unregister_row_hook` add typed OCaml before/after
callbacks for row mutations — the seam a GADT-certified caller needs for a
`before:`/`after:` hook without going through a SQL `CREATE TRIGGER` body.
The registry (`Store.row_hooks`) lives on `Store.t`, not on `Db.t`, for the
same reason `rowid_counters`/`rv_generations` do (#633/#757): a hook
registered through one handle must be visible to, and purged/migrated by,
every sibling handle sharing the same store (`create_worker_handle`,
#589/#633).

**Every rollback-undo in this registry — `row_hook_unregister`,
`row_hooks_purge_table`, and `move_row_hook_key` — merges the entries it is
restoring into whatever currently occupies the destination key, rather than
replacing it outright. The same shape, fixed one function at a time across
two review rounds** (round 5 for the first two; round 6 for
`move_row_hook_key`, the last of the three to still do a blind replace). A
blind snapshot-and-replace undo can lose or misfile a hook a *different*,
concurrent handle registers on the same key in the window between the
original mutation and the `ROLLBACK` that undoes it — made possible by the
pre-existing, documented DDL-visibility leak that lets a sibling handle see
an in-flight DROP/RENAME's effect before the transaction performing it
commits (#589/#633). `move_row_hook_key`'s fix additionally restricts which
entries the reverse move even looks at: it threads the ids the forward move
actually moved through the closure, so a `ROLLBACK`'s reverse move only
ever touches those specific ids at the (now-source) key — never "whatever
is registered there now," which could include a sibling's brand-new,
unrelated registration landing on the rename's target name mid-transaction.

**A row hook's own nested DML, run with no explicit transaction already
open, cannot open a second write transaction on the same store — it fails
immediately with a descriptive error instead of deadlocking (round 6, item
2).** The firing statement's own transaction still holds `Store`'s
non-reentrant writer lock (#740) for the hook's whole invocation; a nested
autocommit call (or a fresh `BEGIN`) needs that same lock and cannot get it
until the outer transaction commits or rolls back — which cannot happen
until the nested call returns. Two fixes were on the table (documented in
`Db.register_row_hook`'s doc comment): detect and refuse immediately, or
make the writer lock re-entrant for this specific case. The refusal was
chosen — matching the project's `#740` precedent of refusing rather than
redesigning locking for a narrow deadlock shape. `Store.rw_begin` detects
it via a dynamic-extent marker (`Lwt.with_value`, tagging the causal call
chain a row hook fires within — the same mechanism `Db`'s `#585`
`txn_scope_key` and `Sql.Exec`'s per-query keys already use), not a
store-wide counter: two logically independent statements can be
interleaved by the Lwt scheduler on one store (sibling handles), and a bare
"is any hook firing anywhere on this store" flag cannot tell an unrelated
sibling statement ordinarily queued behind the writer lock from the one
case that is a genuine self-deadlock.

**`row_hook_unregister`'s rollback-undo used to restore a resurrected hook
at the front of its key's list instead of appending it, contradicting its
own two sibling functions (round 6, item 3) — fixed in round 7, closing
#769.** `row_hooks_purge_table`'s `current @ to_restore` and
`move_row_hook_key`'s `current_new @ to_add` both append the restored
entries onto whatever is currently at the destination key; `row_hook_unregister`
alone prepended (`(id, fn) :: now`). Because the registry stores entries
newest-first and `row_hook_fire_list` reverses that for firing order,
prepending put the *restored* (chronologically earlier) hook at the *back*
of the fire order — firing after a hook registered later, while it was
unregistered — where append correctly restores it to fire *before* that
later registration, matching the other two siblings' documented contract.
One-line fix (`now @ [ id, fn ]`); pinned by a Store-level test mirroring
the round-6 `move_row_hook_key` concurrent-registration test.

**Round 7 also closed a genuine VACUUM-race gap in `unregister_row_hook`
itself — the mirror image of the `register_row_hook` race round 5 fixed —
and a false-positive in the round-6 reentrancy guard.**

- **`unregister_row_hook`'s round-5 `is_closing` short-circuit made a hook
  *survive* detachment during the pre-carry-over VACUUM window, rather than
  merely failing to attach a new one the way it does on the register side.**
  `Db.vacuum` flips `is_closing` on the old store (`S.close t.store`, its
  very first line) several Lwt-yielding steps before it calls
  `Store.row_hooks_carry_over`. A sibling handle's `unregister_row_hook`
  landing in that window used to no-op (mirroring `register_row_hook`'s
  refusal), which looks symmetric but is not: the hook was still fully
  present in the old store's registry at that point, so the no-op left it
  there for `row_hooks_carry_over` to copy verbatim into the new store —
  the caller believed (the call returns `unit` unconditionally) it had
  detached the hook, including a `` `Before `` veto's power to block writes,
  but it kept firing regardless. Fixed by always calling
  `Store.row_hook_unregister` — a plain, synchronous `Hashtbl` mutation with
  no transaction and no yield of its own, so it is safe to call
  unconditionally in both states this store can be in: before carry-over it
  removes the entry the copy is about to pick up; after carry-over (a
  permanently abandoned store from a VACUUM that already completed) it is a
  genuine no-op on a table nobody reads again, identical in effect to the
  old short-circuit's intended case.
- **The round-6 reentrancy guard (`Store.in_row_hook_key`,
  `run_in_row_hook_scope`) spuriously refused the exact deferred-write
  pattern `register_row_hook`'s own doc comment recommends as the safe
  workaround.** `Lwt.with_value`'s snapshot is captured at bind-construction
  time and reinstated whenever the bound continuation actually runs,
  however much later — not re-evaluated against "is this dynamic extent
  still live." A hook body that defers unconditional write work via
  `Lwt.async (fun () -> let* () = <something that yields> in Db.execute db
  sql)` constructs that bind synchronously while still inside the hook's
  extent, so the deferred continuation's captured snapshot said "inside a
  row hook" even when it actually ran long after the firing transaction had
  committed and released the writer lock — `rw_begin` then refused a write
  that was in no danger of deadlocking. Fixed by making the tagged value a
  *mutable* record (`row_hook_scope`, holding the store and an `rhs_active`
  flag) rather than an immutable `t option`: the flag is flipped to `false`
  under `Lwt.finalize` once the hook invocation's own promise settles,
  and — because a bind captures the record by reference, not a copy of its
  fields — a deferred continuation holding the same reference observes that
  flip even though `Lwt.get` still hands back `Some scope`. Confirmed via
  the PR's established revert-and-confirm-failure methodology: a test
  constructing exactly the `Lwt.async`-after-`Lwt.pause` shape failed
  against the pre-fix guard and passes against the fix.

**Known, accepted residual: a row hook's own nested `BEGIN`/`SAVEPOINT`
bypasses `Db.execute`'s `Ok`/`Error` contract and raises instead (round 7,
item 2).** `begin_txn`/`savepoint_txn` call `S.rw_begin` directly with no
`Lwt.catch`, unlike `run_dml`, which wraps and converts any raised exception
(including the round-6 reentrancy guard's `Lwt.fail_with`) into
`Error (Runtime _)`. A hook whose nested DML happens to be an explicit
`BEGIN`/`SAVEPOINT` rather than ordinary `Db.execute` DML can therefore see
a raw uncaught exception instead of the documented result type. Deliberately
not fixed in round 7 (the user's instruction was to defer it). Tracked as
#770.

**Round 8 fixed two more instances of the same recurring shape — a
mechanism torn down or snapshotted at the wrong time — one level further out
than round 7 fixed it, in the depth guard and the schema-undo log rather
than the reentrancy guard and the unregister race.**

- **The autocommit hook-registry mutation was not rolled back on a
  same-statement failure.** `register_row_hook`/`unregister_row_hook` only
  called `Cat.register_schema_undo` when an explicit transaction was open
  (`t.explicit_txn = Some _`); in autocommit, the mutation was a plain,
  untracked `Hashtbl` write. This is invisible for the common case (a hook
  mutating the registry with nothing else in the statement failing), but a
  hook that itself calls `register_row_hook`/`unregister_row_hook` on its
  own handle (the documented nested-hook-mutation pattern) followed by a
  LATER hook in the SAME autocommit statement raising or vetoing left the
  registry mutation in place even though the row-store write it accompanied
  was rolled back via `S.rollback`. DDL's own `with_ddl_txn` already treats
  an `Auto`-mode statement as a schema-undo scope symmetrically (discarding
  the log via `Cat.commit_schema_changes` on success, replaying it via
  `Cat.rollback_schema_changes` on failure) — `Sql.Exec.release_txn` and
  `execute_insert`/`execute_update`/`execute_delete`'s exception handlers now
  do the same. The harder part was deciding WHEN `register_row_hook`/
  `unregister_row_hook` should push an undo at all: pushing unconditionally
  (the first draft) is a **different, worse bug** — a plain top-level
  application call to `register_row_hook`, made outside any hook and outside
  any explicit transaction, has no statement or transaction that will ever
  commit or roll back the pushed entry, so it lingers on `Catalog`'s shared
  undo log to be wrongly replayed by whatever UNRELATED rollback happens
  next (this was caught by the round-8 "confirm the test fails before the
  fix, and PASSES only after" methodology — the first-draft fix made the new
  test fail a *different* way, by unregistering hooks that had nothing to do
  with the failing statement). The correct condition is
  `Option.is_some t.explicit_txn || Store.in_row_hook_for t.store`: the
  first covers a mutation made from anywhere while an explicit transaction
  is open (unchanged from round 3-7), the second is the new case — a
  mutation made from INSIDE a row hook's own body currently firing, whether
  for `t` or a `create_worker_handle` sibling sharing the same store, which
  is always reversible by whatever wrote-then-rolled-back statement fired
  that hook. A plain top-level call with neither condition true remains
  genuinely unaffected, exactly as round 3-7 documented — that claim was
  merely incomplete, not wrong.
- **`max_row_hook_depth` did not bound a chain of hooks that each re-trigger
  the same hook via a deferred `Lwt.async` write.** The depth counter
  (`Store.row_hook_depth_incr`/`_decr`) was incremented before firing a hook
  and decremented under `Lwt.finalize` around only the hook's OWN
  synchronous promise — the same timing round 7's reentrancy-guard fix had
  to work around, but nobody had applied the analogous fix to this
  mechanism yet. A hook that fires, schedules its own recursive next step via
  `Lwt.async (fun () -> ...)`, and returns `Ok ()` immediately releases the
  counter back down before the scheduled step ever runs — so a chain of such
  hooks, each re-entering after the previous one already "finished," never
  appeared to nest no matter how many times it re-entered. Fixed by giving
  `Store.row_hook_scope` (round 7's mutable reentrancy-guard record) a
  second, immutable field, `rhs_depth`, capturing the depth THIS invocation
  is running at; a new `Store.row_hook_effective_depth` reads it off the
  ambient scope (via the same `Lwt.with_value` dynamic-extent propagation
  round 7 established, which survives past `rhs_active` flipping to `false`)
  when the currently-running continuation is a causal descendant of one for
  this store, and falls back to the plain store-wide counter only when there
  is no such ambient scope — which is exactly the case the round-4
  shared-across-sibling-handles test exercises, and which still needs the
  plain counter since two independent Lwt fibers share no `Lwt.with_value`
  storage. Confirmed via the same revert-and-confirm-failure methodology: a
  hook that unconditionally reschedules itself via `Lwt.async`, tested with a
  cutoff far past the 32-deep limit as a safety net (so a still-buggy guard
  fails the test cleanly instead of the test hanging), ran to the cutoff
  without tripping the limit pre-fix and hit the limit well before the
  cutoff post-fix.

**Deliberately not fixed in round 8: a statement-level `unregister_row_hook`
has no effect on the REST of that same statement.** `S.row_hook_fire_list`
is snapshotted once per statement in `make_combined_hook`, before any
per-row loop runs — mirroring pre-existing SQL-trigger snapshot behavior,
but undocumented for row hooks and easier to trip over, since row hooks are
explicitly designed to be mutated mid-flight by other hooks. A hook that
unregisters another hook mid-statement (e.g. H1 unregisters H2 while
handling row N of a multi-row statement) still has H2 fire for every
remaining row of that statement; the detach only takes effect starting with
the next statement. Tracked as #771.

**Round 9 found that round 8's schema-undo fix only closed the
EXCEPTION-RAISING half of what it targeted, in two shapes round 8's own test
did not cover — the same "torn down or snapshotted at the wrong time" family
again, one level further out each time.**

- **A silent SKIP left the same schema-undo entry stranded that round 8 fixed
  for a raise/veto.** `execute_insert_write`'s three silent-skip arms —
  `Iw_skip` (a secondary-index UNIQUE or plain NOT NULL `OR IGNORE`), the
  function's own internal NOT NULL `OR IGNORE` check, and the alias-PK
  `CA_ignore` arm — each called `S.rollback tx` directly and returned `false`
  without going through EITHER of the two places round 8 taught to resolve
  the log: `release_txn`'s `owned` success branch (never reached — a skip
  returns before it) and `execute_insert`'s exception handler (never reached
  either — a skip is a SUCCESS outcome, not a raised exception). A `` `Before
  `` hook that self-mutates the registry (round 8's own documented pattern)
  followed by an `OR IGNORE` skip on that same statement therefore left the
  mutation's undo entry permanently on `cat.sc.undo` — neither committed nor
  rolled back — to be wrongly replayed by whatever UNRELATED statement next
  rolled back on that same `Db.t`. The fix, `rollback_skip`, resolves via
  `Cat.commit_schema_changes` (not rollback) when `owned`: a skip means "this
  row's write didn't happen," not "this statement failed," so the hook
  registry mutation — a real, intentional side effect independent of whether
  this particular row made it in — must stick, exactly as it already does on
  `release_txn`'s write-succeeded path. Confirmed via the same
  revert-and-confirm-failure methodology, with a discriminating twist specific
  to this bug: the immediate effect of a stranded (vs. resolved) undo entry is
  IDENTICAL right after the skip, since the store-level `S.row_hook_unregister`
  mutation is unconditional either way — the bug is only observable via a
  SECOND, unrelated, later statement that actually raises and wrongly replays
  the stranded entry. Each new test therefore asserts against that second
  statement's outcome, not the first, and (having tripped over it once while
  writing the tests) deliberately inserts no successful write in between,
  since an intervening successful statement would itself discard the log via
  `release_txn`'s pre-existing round-8 fix and mask the bug being tested.
- **A row-hook mutation made across sibling `create_worker_handle` handles
  landed on the wrong handle's catalog.** Round 8's gate —
  `Option.is_some t.explicit_txn || Store.in_row_hook_for t.store` — is keyed
  on the SHARED `Store.t` (multiple `Db.t` handles can share one via
  `create_worker_handle`, #589/#633/#632), so it answers `true` for ANY
  sibling handle currently inside a hook callback anywhere on that store —
  but the undo was still pushed onto `t.catalog`, the CALLING handle's own,
  regardless of which handle's statement was actually firing. If a hook body
  running as part of handle H2's statement called
  `register_row_hook`/`unregister_row_hook` on a DIFFERENT handle H1 sharing
  the same store, the undo landed on `H1.catalog`, but only H2's own exception
  handler (`Cat.rollback_schema_changes` on `H2.catalog`) ever ran when H2's
  statement later failed — H1's stranded entry was never replayed by the
  failure it was meant to guard, and was silently dropped whenever H1 itself
  next committed (`Schema_cache.commit` unconditionally clears `t.undo`).
  Fixed by threading the FIRING statement's own undo target through the
  ambient row-hook scope itself: `Store.row_hook_scope` gained a new
  `rhs_register_undo` field — a plain `(unit -> unit) -> unit` closure rather
  than a `Cat.t` (`Store` sits below `Catalog` in the dependency graph, so
  embedding `Cat.t` would be circular; a closure captured by
  `Db.fire_ocaml_row_hook`, the one caller who already has both types in
  scope, avoids that with no new dependency) — and a new
  `Store.row_hook_ambient_undo_target` that `register_row_hook`/
  `unregister_row_hook` now consult INSTEAD OF reaching for their own handle's
  `Cat.t`, falling back to it only when the calling handle has its own
  explicit transaction open (a real, present, more specific scope that wins
  regardless of what else is happening on the shared store) or when there is
  no ambient scope at all (the ordinary top-level-call case, unaffected).
  Confirmed via a two-handle test exercising the exact cross-handle shape:
  H2's hook chain unregisters H0's hook via H1 (a `create_worker_handle`
  sibling of H2 sharing one store), H2's statement then vetoes, and H1 itself
  never opens or resolves any transaction of its own — the registration must
  still be restored, immediately, with no help from H1.

**Deliberately not fixed in round 9** (both filed as new issues rather than
fixed, per instruction): FK `ON DELETE`/`ON UPDATE` `CASCADE`/`SET NULL`/
`SET DEFAULT` bypass OCaml row hooks entirely, since
`cascade_delete_row_in_tx`/`cascade_update_col_in_tx` route through
`delete_row_in_tx`/`update_col_in_tx` without ever building the
`before_hook`/`after_hook` closures the direct DELETE/UPDATE paths construct —
tracked as #773. Separately, `row_hooks_carry_over` does not copy
`row_hook_depth` across a VACUUM store-swap, so `row_hook_effective_depth`'s
store-wide-counter fallback silently resets to 0 for an `Lwt.async`-deferred
continuation whose causal chain no longer matches the new store object,
widening (not eliminating) `max_row_hook_depth`'s recursion ceiling across a
VACUUM that happens to land mid-chain — tracked as #774.

