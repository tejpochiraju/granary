(** Reactive-view compilation for [CREATE REACTIVE VIEW] (#427).

    Two pure pieces the [Db] layer assembles into a maintained view:

    - {!classify} inspects a view's SELECT AST and decides whether it can be
      delta-maintained by the IVM engine (COUNT/SUM single-column GROUP BY) or
      must fall back to full re-computation, and records the base-table
      dependency set either way.
    - {!Agg_engine} is a generic, runtime-driven incremental aggregate over
      rows (values arrays), built on [Granary_ivm].  The [Db] driver feeds it
      row-level changes and applies the returned output delta to the
      materialization table. *)

module Row := Granary_encoding.Row

(** The aggregate a delta-maintained view computes per group. *)
type agg =
  | Count (** [COUNT( * )] — measure 1 per contributing row. *)
  | Sum_col of string (** [SUM(col)] — measure is the named column's integer value. *)

(** Maintenance strategy chosen for a view. *)
type kind =
  | Full (** re-run the whole SELECT on each relevant commit. *)
  | Delta of
      { group_col : string (** single GROUP BY column. *)
      ; agg : agg
      } (** incrementally maintained via {!Agg_engine}. *)

(** Result of classifying a view's SELECT. *)
type classified =
  { base_tables : string list (** distinct base tables the view reads. *)
  ; out_cols : string list option
    (** materialization column names when derivable from the projection;
        [None] when the SELECT projects [*] or otherwise defeats naming. *)
  ; kind : kind
  }

(** [classify select] inspects [select] and returns its maintenance
    classification.  Never fails: any shape the delta engine cannot handle is
    reported as {!Full}. *)
val classify : Granary_sql.Ast.stmt -> classified

(** [is_delta_maintainable select] is [true] when [classify] yields a {!Delta}
    kind — used to reject an explicit [REFRESH DELTA] on an unmaintainable
    shape at CREATE time. *)
val is_delta_maintainable : Granary_sql.Ast.stmt -> bool

(** Generic incremental aggregate over value-array rows, driven at runtime. *)
module Agg_engine : sig
  (** One contributing element: a group key and the integer measure the row
      adds to that group. *)
  type input =
    { key : Row.value array
    ; meas : int
    }

  (** A row-level change lifted to the aggregate's input space. *)
  type change =
    | Ins of input
    | Del of input
    | Upd of input * input

  (** Persistent operator state for one delta-maintained view. *)
  type state

  (** [create ()] is a fresh, empty aggregate. *)
  val create : unit -> state

  (** [step st changes] folds [changes] into [st] and returns the output delta
      as [(output_row, weight)] pairs: [weight > 0] rows entered the
      materialization, [weight < 0] rows left it.  Output rows are the group key
      extended with the aggregate value. *)
  val step : state -> change list -> (Row.value array * int) list

  (** [snapshot st] is the full current materialization (one row per live
      group). *)
  val snapshot : state -> Row.value array list
end

(** Total order over value-array rows; exposed for the driver's diffing. *)
val row_compare : Row.value array -> Row.value array -> int
