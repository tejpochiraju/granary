(** Renders the internals-monitor pane and maps keys to monitor actions (#382). *)

(** [render log] is the reactive UI for the event log: a header line plus one
    coloured line per visible event (or an empty-state hint). *)
val render : Event_log.t -> Nottui.ui Lwd.t

(** [handle_key log ~set_filter_prompt key] maps a key to a monitor action
    (space=pause, c=clear filter, x=clear log, '/'=open the filter prompt via
    [set_filter_prompt]); returns [`Unhandled] for keys it does not consume. *)
val handle_key
  :  Event_log.t
  -> set_filter_prompt:(unit -> unit)
  -> Nottui.Ui.key
  -> Nottui.Ui.may_handle
