(** Generated column values and their SQL literal rendering (#500).

    Shared by both TPC-derived harnesses so the escaping and REAL-formatting
    rules have one implementation. *)

(** A generated column value. *)
type t =
  | VInt of int (** INTEGER column *)
  | VReal of float (** REAL column — money and rates *)
  | VText of string (** TEXT column — names, codes, dates, comments *)
  | VNull (** SQL NULL — an undelivered order's carrier and delivery date *)

(** [literal v] renders [v] as a SQL literal. Text is single-quoted with
    embedded quotes doubled. A whole-valued [VReal] keeps a fractional part:
    granary types a literal by its text, so a REAL column rejects ["100"]
    under strict column typing but accepts ["100.0"].

    Raises [Invalid_argument "Tpc_value.literal: <v> has no SQL literal"] on a
    non-finite [VReal] ([nan], [infinity], [neg_infinity]). SQL has no literal
    for those, and rendering them would silently produce a {e plausible} one:
    ["nan"] and ["inf"] contain no ['.'], so the whole-valued rule would emit
    [nan.0] / [inf.0]. Unreachable from the current generators; guarded
    because a broken value must raise rather than become a different, still
    parseable value. *)
val literal : t -> string

(** [pp fmt v] prints [v]'s constructor and payload, for debugging only. *)
val pp : Format.formatter -> t -> unit
