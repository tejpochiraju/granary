(** A lint against the #485 trap: an unqualified outer-column reference inside
    a correlated subquery (#510).

    granary does not bind such a reference to the outer row — it silently
    evaluates to NULL instead of erroring — so the harness qualifies every
    outer reference. That discipline lived only in comments and had already
    been broken twice: TPC-H Q4 returned 0 rows instead of 5, and TPC-C's
    consistency conditions 1 and 2 became vacuous oracles that reported a
    clean database over any input, corrupted ones included.

    {b This is cheap insurance until #485 is fixed and should be deleted with
    it}, not grown into a general SQL analyzer. It is a deliberately crude
    heuristic: a bare column name is mapped to its table through the
    generators' own {!Tpch_gen.column_names} / {!Tpcc_gen.column_names}, which
    the TPC schemas make unambiguous because every column carries its table's
    prefix. Names the generators do not declare — projection aliases, view
    columns — are ignored rather than guessed at. See the implementation's
    header for the full statement of what it does and does not see. *)

(** One unqualified reference that resolves to a table of an enclosing query
    rather than to the subquery's own FROM. *)
type finding =
  { column : string (** the bare column name as written *)
  ; table : string (** the outer table it resolves to *)
  ; subquery_tables : string list
    (** the tables the offending subquery selects from, which identify which
        subquery it is *)
  }

(** [pp_finding fmt f] prints the finding as a one-line diagnostic naming the
    qualified form that should have been written. *)
val pp_finding : Format.formatter -> finding -> unit

(** One piece of harness SQL to lint, with the schema its column names are
    resolved against. *)
type source =
  { label : string (** where the SQL comes from, for failure messages *)
  ; columns : (string * string) list
    (** column name to owning table; use {!tpch_columns} or {!tpcc_columns} *)
  ; sql : string (** the statement to check *)
  }

(** The TPC-H column-to-table map, derived from {!Tpch_gen.column_names} so it
    cannot drift from the schema the generator populates. *)
val tpch_columns : (string * string) list

(** The TPC-C column-to-table map, derived from {!Tpcc_gen.column_names}. *)
val tpcc_columns : (string * string) list

(** [check source] is every unqualified outer reference the heuristic finds in
    [source.sql], in the order encountered. The empty list means the SQL is
    clean as far as this lint can tell. *)
val check : source -> finding list

(** Every piece of harness SQL under [test/tpc/] that this lint covers: each
    TPC-H query's statement and its setup views, and each TPC-C consistency
    condition's queries. Tpcc_txn's transaction statements are excluded
    because the spec's five profiles contain no subquery at all. *)
val harness : source list
