(** Pure-OCaml date/time module for SQL date/time functions. *)

type dt = {
  year  : int;
  month : int;
  day   : int;
  hour  : int;
  min   : int;
  sec   : float;
}

val parse : ?now:(unit -> float) -> string -> (dt, string) result
(** Parse a time string. Accepted formats:
    - ['YYYY-MM-DD'] — date only, time = 00:00:00
    - ['YYYY-MM-DD HH:MM:SS'] — date and time (space or T separator)
    - ['HH:MM[:SS]'] — time only, date = 2000-01-01
    - ['now'] — requires [~now] clock; returns current UTC time
    - integer/float — unix epoch or Julian day number *)

val to_date      : dt -> string   (** "YYYY-MM-DD" *)
val to_time      : dt -> string   (** "HH:MM:SS" *)
val to_datetime  : dt -> string   (** "YYYY-MM-DD HH:MM:SS" *)
val to_julianday : dt -> float
val to_unixepoch : dt -> int64
val strftime     : string -> dt -> string
(** [strftime fmt dt] formats [dt] using [fmt]. Supported: %Y %m %d %H %M %S %f %j %s %% *)
