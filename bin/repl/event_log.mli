(** Bounded, drop-oldest ring buffer of engine events for the monitor pane,
    plus pause/filter UI state held as [Lwd] vars so the view re-renders
    reactively (#382). *)

module Event = Granary.Db.Event

type t

(** [create ~capacity] makes an empty log holding at most [capacity] events
    (oldest dropped on overflow). *)
val create : capacity:int -> t

(** Append an event.  O(1) amortised, non-blocking — safe to call from the
    engine fiber.  [push] always records into the bounded ring; the [paused]
    flag only freezes the rendered view, not ingestion. *)
val push : t -> Event.t -> unit

(** Current events, oldest-first, after applying the active filter. *)
val visible : t -> Event.t list

(** Total events currently retained (ignoring the filter). *)
val length : t -> int

(** Active filter over the event stream (#385). *)
type filter =
  | No_filter
  | By_txn of int64
  | By_table of
      { name : string (** display name; for the header only *)
      ; tree : int (** the table's storage tree id; matched against events *)
      }

(** [set_filter t f] sets the active filter ([No_filter] = show all). *)
val set_filter : t -> filter -> unit

(** The active filter. *)
val filter : t -> filter

(** [dump t path] writes the currently {!visible} events (respecting the active
    filter), oldest-first, one per line via the event pretty-printer, to [path]
    (truncating). Returns the number of events written, or an error message on
    an I/O failure. *)
val dump : t -> string -> (int, string) result

(** Toggle the paused flag (freezes the view, not ingestion). *)
val toggle_pause : t -> unit

(** Whether the view is paused. *)
val paused : t -> bool

(** Drop all retained events. *)
val clear : t -> unit

(** The [Lwd] root that a view observes; bumped whenever the buffer, filter, or
    pause flag changes. *)
val state_var : t -> unit Lwd.var

(** Minimal pretty-printer: retained-event count, pause state, active filter. *)
val pp : Format.formatter -> t -> unit
