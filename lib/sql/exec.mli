(** Execute write operations against the store and catalog. *)

(** Transaction mode for DML operations.
    [Auto] wraps each DML operation in its own RW transaction (autocommit).
    [In_txn tx] reuses an externally managed transaction; the caller is
    responsible for committing or rolling back.
    [In_ro_txn tx] (#274) reads every scan through one externally managed RO
    snapshot so a multi-statement read sees a single point-in-time committed
    state; read-only — using it on a write path raises. *)
type txn_mode =
  | Auto
  | In_txn of Granary_store.Store.rw Granary_store.Store.txn
  | In_ro_txn of Granary_store.Store.ro Granary_store.Store.txn

(** Execute a write operation ([Plan.op]) against the store and catalog.
    Runs in its own autocommit RW transaction unless [?mode] supplies an
    enclosing one; the optional hooks observe row changes for triggers and
    REPLACE/UPSERT bookkeeping. *)
val execute
  :  ?mode:txn_mode
  -> ?clock:(unit -> float) option
  -> ?params:Granary_encoding.Row.value array
  -> ?before_hook:
       (tx:Granary_store.Store.rw Granary_store.Store.txn
        -> new_row:Granary_encoding.Row.t option
        -> old_row:Granary_encoding.Row.t option
        -> unit Lwt.t)
         option
  -> ?after_hook:
       (tx:Granary_store.Store.rw Granary_store.Store.txn
        -> new_row:Granary_encoding.Row.t option
        -> old_row:Granary_encoding.Row.t option
        -> unit Lwt.t)
         option
  -> ?on_replace_delete_before:
       (tx:Granary_store.Store.rw Granary_store.Store.txn
        -> old_row:Granary_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_replace_delete:
       (tx:Granary_store.Store.rw Granary_store.Store.txn
        -> old_row:Granary_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_upsert_update_before:
       (tx:Granary_store.Store.rw Granary_store.Store.txn
        -> old_row:Granary_encoding.Row.t
        -> new_row:Granary_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_upsert_update:
       (tx:Granary_store.Store.rw Granary_store.Store.txn
        -> old_row:Granary_encoding.Row.t
        -> new_row:Granary_encoding.Row.t
        -> unit Lwt.t)
         option
  -> Granary_store.Store.t
  -> Granary_catalog.Catalog.t
  -> Plan.op
  -> unit Lwt.t

(** Like [execute], but returns the rows-affected count.  For DDL ops
    (CREATE TABLE, CREATE INDEX) this is [0]; for INSERT it is [1]; for
    UPDATE it is the number of rows whose contents were modified.
    [Op_begin], [Op_commit], and [Op_rollback] are not handled here —
    they raise [Failure] if passed; the [Db] layer intercepts them. *)
val execute_with_count
  :  ?mode:txn_mode
  -> ?clock:(unit -> float) option
  -> ?params:Granary_encoding.Row.value array
  -> ?before_hook:
       (tx:Granary_store.Store.rw Granary_store.Store.txn
        -> new_row:Granary_encoding.Row.t option
        -> old_row:Granary_encoding.Row.t option
        -> unit Lwt.t)
         option
  -> ?after_hook:
       (tx:Granary_store.Store.rw Granary_store.Store.txn
        -> new_row:Granary_encoding.Row.t option
        -> old_row:Granary_encoding.Row.t option
        -> unit Lwt.t)
         option
  -> ?on_replace_delete_before:
       (tx:Granary_store.Store.rw Granary_store.Store.txn
        -> old_row:Granary_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_replace_delete:
       (tx:Granary_store.Store.rw Granary_store.Store.txn
        -> old_row:Granary_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_upsert_update_before:
       (tx:Granary_store.Store.rw Granary_store.Store.txn
        -> old_row:Granary_encoding.Row.t
        -> new_row:Granary_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_upsert_update:
       (tx:Granary_store.Store.rw Granary_store.Store.txn
        -> old_row:Granary_encoding.Row.t
        -> new_row:Granary_encoding.Row.t
        -> unit Lwt.t)
         option
  -> Granary_store.Store.t
  -> Granary_catalog.Catalog.t
  -> Plan.op
  -> int Lwt.t

(** #239: per-query cost/stats signal for an external cost-based cache.
    [rows_examined] is the number of rows pulled from a base table/index scan —
    the true work signal (a query that scans a million rows to return one reads
    [rows_examined = 1_000_000], [rows_returned = 1]); [rows_returned] is the
    result size once the stream is drained; [used_index] is the plan-time fact
    that the base access is an index/rowid/FTS seek rather than a full scan.

    Mutable counters updated as the stream is consumed: pass a fresh record to
    {!query} via [?stats], drain the stream, then read the fields.  Mirage-pure
    (no clock/Unix dependency).  [rows_examined] covers seq scans, index/rowid
    lookups (incl. nested-loop-join right-side probes), the count fast path,
    both inputs of a hash join, FTS scans (content rows in a plain scan, matched
    index rows in a MATCH query), and rows read inside scalar/correlated
    subqueries (#257).

    #546: [index_entries] is the number of INDEX entries an index lookup walked.
    [rows_examined] counts only the table rows fetched afterwards, one [rh_get]
    per entry, so it charges an index seek and the sequential scan it replaced
    the same amount for reading the same rows and hides the per-entry tree
    descent entirely.  Everything other than an index lookup leaves it at 0. *)
type query_stats =
  { mutable rows_examined : int
  ; mutable rows_returned : int
  ; mutable index_entries : int
  ; mutable used_index : bool
  }

(** A zeroed {!query_stats} ([used_index = false]). *)
val make_query_stats : unit -> query_stats

(** #493: how many times a subquery statement has been bound and planned, as
    opposed to served from the per-query correlated-subquery plan cache.
    Monotone and process-global; read it either side of a query and take the
    difference.

    Diagnostic/testing only, on the same footing as
    {!Granary_store.Store.active_reader_count} (#164).  It exists because "plan
    once, execute N times" was otherwise unobservable: a correlated subquery
    re-planned on every outer row and one planned once look identical in
    {!query_stats}.  Being an integer count rather than a wall clock, a test
    asserting on it needs no [GRANARY_BENCH_*] neutralizer.

    Exposed as a function rather than the underlying [ref] so the counter
    cannot be written from outside (merlint E351). *)
val subquery_plans_built : unit -> int

(** #417 Phase 0: one row-level mutation captured by a change-capturing
    {!dirty_tables_acc}.  [rowid] is the row's int64 rowid; rows are the full
    {!Granary_encoding.Row.t} as written/removed.  [Updated] carries both the
    pre-image ([old_row]) and post-image ([new_row]). *)
type row_change =
  | Inserted of
      { rowid : int64
      ; row : Granary_encoding.Row.t
      }
  | Deleted of
      { rowid : int64
      ; row : Granary_encoding.Row.t
      }
  | Updated of
      { rowid : int64
      ; old_row : Granary_encoding.Row.t
      ; new_row : Granary_encoding.Row.t
      }

(** #240: opaque accumulator for the set of user tables a write statement
    mutated.  Install it around a statement with {!with_dirty} and read the
    result with {!dirty_elements}.  A capturing accumulator ({!make_change_acc})
    additionally records the per-row {!row_change} deltas, read with
    {!dirty_changes} (#417). *)
type dirty_tables_acc

(** A fresh, empty {!dirty_tables_acc} that records mutated table {e names} only. *)
val make_dirty_acc : unit -> dirty_tables_acc

(** #417: a fresh accumulator that additionally captures the per-row
    {!row_change} deltas (read with {!dirty_changes}).  Row capture is opt-in:
    {!make_dirty_acc} callers pay nothing for it. *)
val make_change_acc : unit -> dirty_tables_acc

(** [with_dirty acc f] runs [f] with [acc] installed as the active write-path
    mutation sink, so every table whose {e rows} [f] mutates — directly, or
    indirectly via FK cascades and triggers — is recorded in [acc].  Nestable
    and independent of the {!query} stats context.  Schema-changing DDL
    ([ALTER]/[DROP]) is intentionally {b not} recorded, even when it rewrites
    rows (e.g. [ALTER TABLE … DROP COLUMN]): DDL invalidation is handled out of
    band (see {!Granary_db.Db.dirty_tables}). *)
val with_dirty : dirty_tables_acc -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** The accumulator installed by the nearest enclosing {!with_dirty}, if any.
    Lets a caller (the #427 reactive-view driver) reuse an ambient
    change-capturing accumulator instead of shadowing it with a fresh one. *)
val current_dirty_acc : unit -> dirty_tables_acc option

(** The user tables recorded in [acc]: sorted, deduplicated, with SQLite-reserved
    [sqlite_…] objects (sqlite_master / sqlite_sequence) excluded. *)
val dirty_elements : dirty_tables_acc -> string list

(** #417: the per-row {!row_change} deltas recorded in [acc], as [(table,
    changes)] pairs sorted by table name, each table's changes in application
    order.  SQLite-reserved [sqlite_…] objects are excluded, matching
    {!dirty_elements}.  Always [[]] for a non-capturing accumulator
    ({!make_dirty_acc}). *)
val dirty_changes : dirty_tables_acc -> (string * row_change list) list

(** #514: what the DML (UPDATE/DELETE) index-seek path did with its candidate
    rowids.  [dss_candidates] is how many index entries the seek accepted and
    [dss_fetched] how many of those rowids were looked up in the table tree;
    [dss_peak_buffered] is the high-water mark of the difference — candidates
    walked but not yet fetched.

    That backlog is a {e shape} assertion, not a memory bound.  It equals the
    match count by design: the drain sorts every candidate into rowid order
    before fetching any row, because fetching them in index-key order costs up
    to a page read per row on a table larger than the pager cache.  A drop to 1
    therefore signals a fetch-as-you-walk regression, not an improvement.  The
    statement's peak memory is dominated by the match list (a decoded row each),
    which is O(affected rows) by design and not what these counters measure.

    So in the current shape [dss_peak_buffered] is redundant: no fetch happens
    until the walk ends, the backlog is monotone, and the peak is always exactly
    [dss_candidates].  It is kept anyway, because that redundancy IS the
    assertion — it is the only counter that distinguishes "buffered everything,
    then fetched" from "buffered and fetched in step", and those two have the
    same [(dss_candidates, dss_fetched)] pair and a 20x page-read gap.  The day
    it stops equalling [dss_candidates] is the day the drain changed shape.

    All three are cumulative over the enclosing {!with_dml_seek_stats} scope,
    not per statement: an FK [CASCADE] delete or a trigger firing nested DML
    re-enters the drain and adds to the same record, so the counts are a sum
    across every seeked drain in the scope, over any number of tables. *)
type dml_seek_stats =
  { mutable dss_candidates : int
  ; mutable dss_fetched : int
  ; mutable dss_peak_buffered : int
  }

(** A zeroed {!dml_seek_stats}. *)
val make_dml_seek_stats : unit -> dml_seek_stats

(** [with_dml_seek_stats st f] runs [f] with [st] collecting the DML seek
    counters (see {!dml_seek_stats}).  Purely observational; installing no
    accumulator costs one key lookup per drain and nothing per row. *)
val with_dml_seek_stats : dml_seek_stats -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** #264: quote a SQL identifier with double-quotes when it is not a plain
    [[A-Za-z_][A-Za-z0-9_]*] word, or is a reserved word (embedded quotes
    doubled); returned verbatim otherwise. #572: an alias for
    {!Granary_sql.Ast.quote_ident} — the DDL renderer and [Ast.expr_to_sql]
    emit SQL text into the same catalog, so they share one rule. *)
val quote_ident : string -> string

(** #264: reconstruct a [CREATE TABLE] statement from catalog metadata (used by
    both [sqlite_master] and the logical dump). Emits column types, constraints
    (NOT NULL, column-level PRIMARY KEY, DEFAULT, CHECK, GENERATED), foreign
    keys, and the [WITHOUT ROWID] clause. UNIQUE constraints are carried by
    their backing indexes ({!ddl_of_index}), not inline.

    [indexes] must be the table's own indexes ([Catalog.indexes_for_table]). A
    composite [PRIMARY KEY] is rendered as a table-level constraint recovered
    from its implicit index, which is the only place the key's column ORDER is
    recorded — [Row.column] has no key ordinal (#533). Pass the real index list:
    with the wrong one a composite key renders as several single-column keys,
    which is a different table. *)
val ddl_of_table
  :  indexes:Granary_catalog.Catalog.index_info list
  -> Granary_catalog.Catalog.table_meta
  -> string

(** #533: is [idx] already implied by the [PRIMARY KEY] that
    {!ddl_of_table} renders for [meta], so a logical dump can skip emitting its
    [CREATE INDEX]? [indexes] must be the same list passed to {!ddl_of_table} —
    the two answers are derived from one predicate so they cannot drift apart
    and drop a uniqueness constraint from the dump. Always [false] for [`User]
    and [`Implicit_unique] indexes. *)
val ddl_implies_index
  :  Granary_catalog.Catalog.table_meta
  -> indexes:Granary_catalog.Catalog.index_info list
  -> Granary_catalog.Catalog.index_info
  -> bool

(** #264: reconstruct a [CREATE [UNIQUE] INDEX] statement from index metadata. *)
val ddl_of_index : Granary_catalog.Catalog.index_info -> string

(** #264: reconstruct a [CREATE VIRTUAL TABLE .. USING fts5(..)] statement. *)
val ddl_of_fts : Granary_catalog.Catalog.fts_table_meta -> string

(** #330: read an FTS5 table's stored content as [(rowid, column_texts)] pairs
    through [mode] (the dump's shared snapshot / explicit txn), so [Db.dump] can
    emit [INSERT INTO fts(rowid, ..)] statements that round-trip rowids exactly.
    Rowids are surfaced only here (out-of-band), not via any SQL projection. *)
val read_fts_content_rows
  :  Granary_store.Store.t
  -> txn_mode
  -> Granary_catalog.Catalog.fts_table_meta
  -> (int64 * string list) list Lwt.t

(** #264: render a {!Granary_encoding.Row.value} as a standalone SQL literal
    that re-reads to the identical value (text quoted, blob as [X'..'], float as
    the shortest round-tripping REAL literal, non-finite floats as
    [1e999]/[-1e999]/[NULL]). Used to serialize rows as [INSERT] statements. *)
val sql_literal_of_value : Granary_encoding.Row.value -> string

(** Execute read operations; returns a lazy stream of result rows.
    [params] are the positional parameter values for [?] placeholders.
    [clock] supplies the current Unix timestamp for SQL date/time
    functions invoked with the literal ['now'].
    [mode] selects the read's transaction context: [Auto] reads a fresh RO
    snapshot of the committed state, whereas [In_txn tx] reads through [tx] so
    the result reflects the transaction's own uncommitted writes
    (read-your-own-writes, #262) — across scans, index/rowid lookups, the
    aggregate fast path, indexed joins, FTS scans, and subqueries.
    [stats], when supplied, is populated as the returned stream is consumed
    (#239); omit it and the read path is entirely unaffected. *)
val query
  :  ?mode:txn_mode
  -> ?clock:(unit -> float) option
  -> ?params:Granary_encoding.Row.value array
  -> ?stats:query_stats
  -> Granary_store.Store.t
  -> Granary_catalog.Catalog.t
  -> Plan.op
  -> Granary_encoding.Row.t Lwt_stream.t Lwt.t

(** Evaluate a [Plan.expr] against a row.  Exposed primarily for testing
    the rich expression language directly without round-tripping through
    SQL.  Failures (e.g. division by zero, type-mismatched arithmetic)
    raise [Failure]. *)
val eval_expr
  :  (unit -> float) option
  -> Granary_encoding.Row.value array
  -> Granary_encoding.Row.t
  -> Plan.expr
  -> Granary_encoding.Row.value
