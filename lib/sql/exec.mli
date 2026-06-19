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
  | In_txn of Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
  | In_ro_txn of Sqlocaml_store.Store.ro Sqlocaml_store.Store.txn

(** Execute a write operation ([Plan.op]) against the store and catalog.
    Runs in its own autocommit RW transaction unless [?mode] supplies an
    enclosing one; the optional hooks observe row changes for triggers and
    REPLACE/UPSERT bookkeeping. *)
val execute
  :  ?mode:txn_mode
  -> ?clock:(unit -> float) option
  -> ?params:Sqlocaml_encoding.Row.value array
  -> ?before_hook:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> new_row:Sqlocaml_encoding.Row.t option
        -> old_row:Sqlocaml_encoding.Row.t option
        -> unit Lwt.t)
         option
  -> ?after_hook:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> new_row:Sqlocaml_encoding.Row.t option
        -> old_row:Sqlocaml_encoding.Row.t option
        -> unit Lwt.t)
         option
  -> ?on_replace_delete_before:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_replace_delete:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_upsert_update_before:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> new_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_upsert_update:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> new_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> Sqlocaml_store.Store.t
  -> Sqlocaml_catalog.Catalog.t
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
  -> ?params:Sqlocaml_encoding.Row.value array
  -> ?before_hook:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> new_row:Sqlocaml_encoding.Row.t option
        -> old_row:Sqlocaml_encoding.Row.t option
        -> unit Lwt.t)
         option
  -> ?after_hook:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> new_row:Sqlocaml_encoding.Row.t option
        -> old_row:Sqlocaml_encoding.Row.t option
        -> unit Lwt.t)
         option
  -> ?on_replace_delete_before:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_replace_delete:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_upsert_update_before:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> new_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_upsert_update:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> new_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> Sqlocaml_store.Store.t
  -> Sqlocaml_catalog.Catalog.t
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
    subqueries (#257). *)
type query_stats =
  { mutable rows_examined : int
  ; mutable rows_returned : int
  ; mutable used_index : bool
  }

(** A zeroed {!query_stats} ([used_index = false]). *)
val make_query_stats : unit -> query_stats

(** #417 Phase 0: one row-level mutation captured by a change-capturing
    {!dirty_tables_acc}.  [rowid] is the row's int64 rowid; rows are the full
    {!Sqlocaml_encoding.Row.t} as written/removed.  [Updated] carries both the
    pre-image ([old_row]) and post-image ([new_row]). *)
type row_change =
  | Inserted of
      { rowid : int64
      ; row : Sqlocaml_encoding.Row.t
      }
  | Deleted of
      { rowid : int64
      ; row : Sqlocaml_encoding.Row.t
      }
  | Updated of
      { rowid : int64
      ; old_row : Sqlocaml_encoding.Row.t
      ; new_row : Sqlocaml_encoding.Row.t
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
    band (see {!Sqlocaml_db.Db.dirty_tables}). *)
val with_dirty : dirty_tables_acc -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** The user tables recorded in [acc]: sorted, deduplicated, with SQLite-reserved
    [sqlite_…] objects (sqlite_master / sqlite_sequence) excluded. *)
val dirty_elements : dirty_tables_acc -> string list

(** #417: the per-row {!row_change} deltas recorded in [acc], as [(table,
    changes)] pairs sorted by table name, each table's changes in application
    order.  SQLite-reserved [sqlite_…] objects are excluded, matching
    {!dirty_elements}.  Always [[]] for a non-capturing accumulator
    ({!make_dirty_acc}). *)
val dirty_changes : dirty_tables_acc -> (string * row_change list) list

(** #264: quote a SQL identifier with double-quotes when it is not a plain
    [[A-Za-z_][A-Za-z0-9_]*] word (embedded quotes doubled); returned verbatim
    otherwise. *)
val quote_ident : string -> string

(** #264: reconstruct a [CREATE TABLE] statement from catalog metadata (used by
    both [sqlite_master] and the logical dump). Emits column types, constraints
    (NOT NULL, column-level PRIMARY KEY, DEFAULT, CHECK, GENERATED), foreign
    keys, and the [WITHOUT ROWID] clause. UNIQUE and composite/table-level
    PRIMARY KEY constraints are carried by their backing indexes
    ({!ddl_of_index}), not inline. *)
val ddl_of_table : Sqlocaml_catalog.Catalog.table_meta -> string

(** #264: reconstruct a [CREATE [UNIQUE] INDEX] statement from index metadata. *)
val ddl_of_index : Sqlocaml_catalog.Catalog.index_info -> string

(** #264: reconstruct a [CREATE VIRTUAL TABLE .. USING fts5(..)] statement. *)
val ddl_of_fts : Sqlocaml_catalog.Catalog.fts_table_meta -> string

(** #330: read an FTS5 table's stored content as [(rowid, column_texts)] pairs
    through [mode] (the dump's shared snapshot / explicit txn), so [Db.dump] can
    emit [INSERT INTO fts(rowid, ..)] statements that round-trip rowids exactly.
    Rowids are surfaced only here (out-of-band), not via any SQL projection. *)
val read_fts_content_rows
  :  Sqlocaml_store.Store.t
  -> txn_mode
  -> Sqlocaml_catalog.Catalog.fts_table_meta
  -> (int64 * string list) list Lwt.t

(** #264: render a {!Sqlocaml_encoding.Row.value} as a standalone SQL literal
    that re-reads to the identical value (text quoted, blob as [X'..'], float as
    the shortest round-tripping REAL literal, non-finite floats as
    [1e999]/[-1e999]/[NULL]). Used to serialize rows as [INSERT] statements. *)
val sql_literal_of_value : Sqlocaml_encoding.Row.value -> string

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
  -> ?params:Sqlocaml_encoding.Row.value array
  -> ?stats:query_stats
  -> Sqlocaml_store.Store.t
  -> Sqlocaml_catalog.Catalog.t
  -> Plan.op
  -> Sqlocaml_encoding.Row.t Lwt_stream.t Lwt.t

(** Evaluate a [Plan.expr] against a row.  Exposed primarily for testing
    the rich expression language directly without round-tripping through
    SQL.  Failures (e.g. division by zero, type-mismatched arithmetic)
    raise [Failure]. *)
val eval_expr
  :  (unit -> float) option
  -> Sqlocaml_encoding.Row.value array
  -> Sqlocaml_encoding.Row.t
  -> Plan.expr
  -> Sqlocaml_encoding.Row.value
