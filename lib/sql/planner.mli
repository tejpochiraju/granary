(** Translate a bound statement into a physical plan.

    When [~cat] is supplied, the planner may use catalog metadata
    (e.g. indexes) to choose better plan shapes — for example, an
    equality predicate on an indexed column produces an [Op_index_lookup]
    instead of an [Op_seq_scan + Op_filter] chain.

    When [~cat] is omitted, the planner falls back to plans that need
    no catalog (sequential scan with filtering).  This keeps unit tests
    that don't construct a full catalog compact. *)

val plan : ?cat:Granary_catalog.Catalog.t -> Sema.bound_stmt -> Plan.op

(** #550: how many entries a DML (UPDATE/DELETE) seek over the index living in
    tree [idx_tree] may walk before [Exec]'s drain abandons it for a full table
    scan, or [None] to walk unconditionally — [None] for a UNIQUE index, or a
    table too small or too large (WITHOUT ROWID / columnar) to size at all.

    {b Must be called fresh, once per execution, against a [meta] read from the
    catalog at that moment} — never a [table_meta] carried on a cached
    [Plan.op]. A [Plan.op] is computed once at [Db.prepare] and reused for a
    prepared statement's whole life, so a budget computed once and stamped onto
    the plan would go stale as the table's row count changed underneath it;
    see the implementation's comment for the staleness bug this replaced.
    [Exec.seek_index_candidates] is the sole caller and re-reads [meta] via
    [Granary_catalog.Catalog.find_table_cached] on every call for exactly this
    reason. *)
val dml_seek_bail_out_at
  :  Granary_catalog.Catalog.t
  -> Granary_catalog.Catalog.table_meta
  -> idx_tree:int
  -> int option

(** #517/#576: [true] iff a column of this type can ever produce a
    [Plan.range] bound -- only [Row.Integer] and [Row.Real] have a
    fixed-width index-key encoding a range seek can be built from; [Row.Text]
    and [Row.Blob] never can. {!range_histogram_estimate}
    only ever consults a histogram for a column this returns [true] for, so
    [Exec.execute_create_index] uses this same predicate to decide whether a
    non-leading index column is worth building a histogram for in the first
    place -- building one for a column this returns [false] on would be a
    number nothing ever reads. *)
val bounded_type : Granary_encoding.Row.ty -> bool
