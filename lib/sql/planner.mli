(** Translate a bound statement into a physical plan.

    When [~cat] is supplied, the planner may use catalog metadata
    (e.g. indexes) to choose better plan shapes — for example, an
    equality predicate on an indexed column produces an [Op_index_lookup]
    instead of an [Op_seq_scan + Op_filter] chain.

    When [~cat] is omitted, the planner falls back to plans that need
    no catalog (sequential scan with filtering).  This keeps unit tests
    that don't construct a full catalog compact. *)

val plan : ?cat:Sqlocaml_catalog.Catalog.t -> Sema.bound_stmt -> Plan.op
