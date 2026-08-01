(** Schema for the TPC-C-derived benchmark (#500).

    Column names and order here MUST match {!Tpcc_gen.column_names} exactly —
    verified by [test/test_tpcc_load.ml]'s "columns match the generator" case,
    which parses each [CREATE TABLE] statement below and diffs it against
    {!Tpcc_gen.column_names}. *)

(** The nine [CREATE TABLE] statements, in load order: parents before
    children, so a loader can follow this list directly. *)
val ddl : string list

(** Secondary indexes, created after the load. Covers the two lookups the
    transaction profiles make that no primary key serves: customer by last
    name (Payment and OrderStatus, the spec's 60% by-name case) and orders by
    customer (OrderStatus's most-recent-order scan).

    There is deliberately no [new_order] index: Delivery's [MIN(no_o_id)] per
    district is a prefix of that table's composite primary key, so an index
    would only duplicate it. That granary full-scans composite-PK lookups
    instead of seeking the implicit index is {b #508} — an engine bug to fix
    in the planner, not something to paper over here with a redundant
    secondary index that would distort what the benchmark measures. *)
val indexes : string list

(** The nine table names, in the same order as {!ddl}. An alias for
    {!Tpcc_gen.tables}, the single source of truth for the table list. *)
val tables : string list

(** [Load (E)] loads a generated population into an engine: DDL, then one
    batched transaction per table in {!tables} order, then {!indexes}.  Batch
    size is [GRANARY_TPCC_BATCH] (default 500). *)
module Load (E : Bench_report.ENGINE) : sig
  (** [run engine gen] creates the schema and loads every table. *)
  val run : E.t -> Tpcc_gen.t -> unit
end
