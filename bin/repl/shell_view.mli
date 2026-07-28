(** The query-shell pane: an editable input line, a results grid, and a status
    line, all backed by [Lwd] vars (#382). *)

type t

(** [create ()] is a fresh shell view with empty input/result/status. *)
val create : unit -> t

(** The [Lwd] var holding the current input-line text. *)
val input_var : t -> string Lwd.var

(** [set_status t s] replaces the status line. *)
val set_status : t -> string -> unit

(** [set_result t ~headers ~rows] replaces the displayed result grid. *)
val set_result : t -> headers:string list -> rows:Granary.Db.row list -> unit

(** [render t] is the reactive UI for the pane. *)
val render : t -> Nottui.ui Lwd.t

(** Per-column display widths = max(widest data value, header label length).
    Exposed for testing. *)
val column_widths_with_headers
  :  headers:string list
  -> rows:Granary.Db.row list
  -> int array

(** Minimal pretty-printer (input + status summary) for debugging/logging. *)
val pp : Format.formatter -> t -> unit
