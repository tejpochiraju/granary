(** Stable 64-bit fingerprint of a table's schema *shape*, for corruption
    recovery (#174 / #85), schema-drift detection on open, and replication
    schema-matching (#92 / #172).

    The fingerprint is a pure function of the column list (names, types, and
    every flag that affects how a stored row decodes) plus the [without_rowid]
    flag.  It deliberately excludes volatile / identity fields — the table
    name, [tree_id], and the rowid counter — so it is stable across table
    renames and captures only what is required to decode a row.

    It is computed with FNV-1a 64-bit over a versioned, length-delimited
    canonical serialization, so it is deterministic and stable across runs,
    machines, and library versions (unlike [Hashtbl.hash]). *)

(** [compute ~columns ~without_rowid] is the 64-bit schema fingerprint. *)
val compute : columns:Row.column list -> without_rowid:bool -> int64

(** Low 32 bits of a fingerprint — used to stamp the 4-byte reserved page
    header field (#174 page-stamp). *)
val low32 : int64 -> int32
