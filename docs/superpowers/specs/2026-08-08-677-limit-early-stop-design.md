# #677 (2 of 3): `Op_limit` early-stop

Split from #677, which lists three independent perf defects downstream of a
correct index seek. This spec covers only item (2), the issue's own
recommended first cut: `Op_limit` fully drains its child stream before
slicing to `[offset, offset+limit)`, which forces every `LIMIT` query in the
engine to pay for every row an unbounded scan would have produced. Items (1)
(covering-index shortcut for MIN/MAX/COUNT) and (3) (sort elision) stay open
under #677 and are deliberately out of scope here.

## Problem

```ocaml
| Plan.Op_limit { limit; offset; child } ->
  let* inner = to_stream clock params store ~mode ~cat child in
  let* rows = Lwt_stream.to_list inner in
  let rows' = List.filteri (fun i _ -> i >= offset && i < offset + limit) rows in
  Lwt.return (Lwt_stream.of_list rows')
```

`Lwt_stream.to_list` pulls the child stream to exhaustion regardless of
`limit`. Naively truncating the pull (e.g. `Lwt_stream.get` in a loop, stop at
`offset+limit`) would fix the wasted work but reintroduce the class of leak
#164/#493/#546 fixed: the base scanners (`stream_seq_scan`,
`stream_index_lookup`, `stream_fts_seq_scan`) hold a live `Store` reader
handle and cursor across pulls, released only by a `finish ()` closure that
`Lwt_stream.from`'s generator function calls itself on natural exhaustion or
an exception. If the consumer simply stops pulling, that generator function
is never called again and `finish ()` never runs — a pinned page / uncounted
RO reader leaks forever.

## Which operators actually need this

Surveyed every `to_stream` case that can appear as `Op_limit`'s (transitive)
child:

- **Lazy, cursor-holding across pulls — need cleanup on early stop:**
  `stream_seq_scan`, `stream_index_lookup`, `stream_fts_seq_scan`. Each
  already has an idempotent `finish ()` closure (guarded by an `ended` ref)
  wired into their `Lwt_stream.from` generator.
- **Already eager — nothing to do:** `stream_rowid_lookup` (single
  `rh_get`/`rh_finish`, no stream), `stream_nested_loop_join` /
  `stream_hash_join` (drain their non-probed side via `Lwt_stream.to_list`
  before the join runs), `stream_aggregate`'s fast COUNT/SUM path (drains its
  own cursor fully in a synchronous loop before returning a stream at all),
  `stream_fts_match_scan` (built via `S.with_ro`, which self-closes).
  `Op_sort`/`Op_distinct`/`Op_aggregate` all materialize their child fully by
  construction, so an `Op_limit` above one of them gets no benefit from early
  stop and needs none — the drain already happened one level down.

So the only cleanup obligation early-stop can create is at those 3 scanners,
and it is bounded to exactly the resources opened for `Op_limit`'s own child
subtree (joins/aggregates below it are already fully drained by the time
`Op_limit` starts pulling).

## Mechanism: a scoped cleanup registry, same shape as `query_stats_key`

Add, next to `query_stats_key` (`lib/sql/exec.ml:6930`):

```ocaml
let stream_cleanup_key : (unit -> unit Lwt.t) list ref Lwt.key = Lwt.new_key ()
```

Each of the 3 lazy scanners, immediately after defining its existing `finish`
closure, registers it if a scope is active:

```ocaml
(match Lwt.get stream_cleanup_key with
 | Some reg -> reg := finish :: !reg
 | None -> ());
```

`Op_limit` opens a fresh scope around its child's construction, pulls at most
`offset + limit` rows, then unconditionally flushes the registry:

```ocaml
| Plan.Op_limit { limit; offset; child } ->
  let cleanups = ref [] in
  let* inner =
    Lwt.with_value stream_cleanup_key (Some cleanups) (fun () ->
      to_stream clock params store ~mode ~cat child)
  in
  let want = offset + limit in
  let rec pull n acc =
    if n <= 0
    then Lwt.return (List.rev acc)
    else
      let* v = Lwt_stream.get inner in
      match v with
      | None -> Lwt.return (List.rev acc)
      | Some row -> pull (n - 1) (row :: acc)
  in
  let* rows = pull want [] in
  let* () = Lwt_list.iter_s (fun f -> f ()) !cleanups in
  let rows' = List.filteri (fun i _ -> i >= offset) rows in
  Lwt.return (Lwt_stream.of_list rows')
```

Why this is safe and sufficient:

- **Idempotent by construction.** Every registered `finish` is already
  guarded by its own `ended` ref. Flushing the registry after a natural full
  drain (child exhausted before `want` was reached) is a no-op for every
  entry — the generator already called `finish` itself on the `None` branch.
- **Correctly scoped, including nesting.** `Lwt.with_value` is dynamic-extent
  scoping over the promise chain created during `f ()`, the same mechanism
  `query_stats_key`/`txn_mode_key` already rely on (#239/#262). A scanner
  constructed directly under this `Op_limit`'s child call registers into
  `cleanups`. A *nested* `Op_limit` somewhere in that subtree (e.g. a LIMIT
  inside a correlated subquery) opens its own inner `Lwt.with_value`, which
  shadows the key for its own construction call — its scanners register into
  its own registry and get cleaned up by its own flush, never the outer
  one's. Separately, correlated-subquery evaluation inside
  `stream_filter`/`stream_expr_project` runs its own `to_stream` per row and
  is fully drained/closed synchronously within that row's evaluation
  (#257/#493 pattern) — done before `Op_limit` ever checks whether it has
  enough rows, so nothing from it is left in the registry to force-close.
- **No behavior change when there is no `Op_limit` above a scanner.**
  `Lwt.get stream_cleanup_key` is `None` outside any `Op_limit` scope, so
  registration is skipped and every existing scanner behaves exactly as
  before (`finish` still runs on natural exhaustion/exception, nothing new).
- **LIMIT/OFFSET are already validated non-negative** by
  `Sema.validate_limit_offset` before an `Op_limit` node can exist, so
  `offset + limit` cannot go negative and the `pull` recursion terminates.

## Out of scope

- Sort elision (#677 item 3) and the covering-index/aggregate shortcut (#677
  item 1) stay open, tracked on #677 itself.
- No change to `Op_sort`, `Op_aggregate`, `Op_distinct`, or the join
  operators — none of them can benefit from early stop without item (3)
  (sort elision) landing first, since `Op_sort` necessarily materializes its
  whole input.
- `Db.query_with_stats`'s `rows_examined`/`index_entries` counters are
  unaffected: they are incremented per row actually pulled through the
  scanner, and early stop simply means fewer rows get pulled — the counters
  already report "work actually done," not "work the unbounded plan would
  have done."

## Testing

- A `LIMIT`/`OFFSET` correctness suite (existing behavior must not change):
  values, ordering, boundary `offset+limit` at/over the row count, `LIMIT 0`.
- A reader-leak regression test in the shape of #164/#493/#546's existing
  ones: run a `LIMIT`-bounded query against a table with far more rows than
  the limit, on disk (not `Mem` — `Store.active_reader_count` reads 0
  unconditionally on the in-memory backend, same caveat as
  `test_correlated_exists_493`), and assert `Store.active_reader_count` /
  `Store.pinned_page_count` return to their pre-query baseline afterward, for
  each of the 3 scanner shapes (seq scan, index lookup, FTS seq scan).
- A rows-examined assertion via `Db.query_with_stats` showing the LIMIT case
  now examines close to `offset+limit` rows, not the whole matching set —
  the numbers from #677's own writeup are a natural target (900 → ~1 for the
  `new_order` MIN-shaped query, once combined with item 1; for item (2) alone
  the measurable win is on a plain `SELECT ... LIMIT n` over a large table).
