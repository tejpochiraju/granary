(** Bounded, drop-oldest ring buffer of engine events for the monitor pane,
    plus pause/filter UI state held as [Lwd] vars so the view re-renders
    reactively (#382). *)

module Event = Sqlocaml.Db.Event

type t

(** [create ~capacity] makes an empty log holding at most [capacity] events
    (oldest dropped on overflow). *)
val create : capacity:int -> t

(** Append an event.  O(1) amortised, non-blocking — safe to call from the
    engine fiber.  [push] always records into the bounded ring; the [paused]
    flag only freezes the rendered view, not ingestion. *)
val push : t -> Event.t -> unit

(** Current events, oldest-first, after applying the active txn-id filter. *)
val visible : t -> Event.t list

(** Total events currently retained (ignoring the filter). *)
val length : t -> int

(** [set_filter t f] sets the active txn-id filter ([None] = show all). *)
val set_filter : t -> int64 option -> unit

(** The active txn-id filter, if any. *)
val filter : t -> int64 option

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
