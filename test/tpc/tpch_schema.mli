(** TPC-H schema and bulk loader (#482).

    DDL uses only types granary and SQLite share: INTEGER, REAL, TEXT.  Money
    columns are REAL rather than DECIMAL, matching what SQLite does natively,
    so the cross-engine answer comparison is between like representations. *)

(** CREATE TABLE statements in load order — parents before children. *)
val ddl : string list

(** CREATE INDEX statements.  Applied after the data is loaded, because
    maintaining indexes during a bulk load is substantially slower on both
    engines. *)
val indexes : string list

(** [literal v] renders a generated value as a SQL literal, doubling embedded
    single quotes in text. [VReal] uses [%.17g], which round-trips a float
    exactly through text, with [".0"] appended when that yields no fractional
    part or exponent — granary types a literal by its text, so the INTEGER
    literal ["100"] would be rejected by a REAL column under strict column
    typing. It can in principle emit scientific notation for
    extreme magnitudes, but every TPC-H domain generated here (money, rates,
    quantities) is bounded well within plain-decimal range. [Tpch_gen] cannot
    produce infinity or NaN — every [VReal] comes from bounded
    [Tpc_rand.float_between] or arithmetic over bounded inputs — so [literal]
    never has to special-case them. *)
val literal : Tpch_gen.value -> string

(** Bulk loader, parameterized over the engine under benchmark. *)
module Load (E : Bench_report.ENGINE) : sig
  (** [run engine gen] creates the schema, loads every table inside one
      transaction per table, then creates the indexes.  Rows are batched into
      multi-row INSERT statements of [GRANARY_TPCH_BATCH] rows (default 500). *)
  val run : E.t -> Tpch_gen.t -> unit
end
