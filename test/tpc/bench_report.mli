(** Shared reporting layer for the TPC-derived benchmarks (#482).

    Holds the cross-engine signature, wall/CPU timing, CSV emission, and the
    float-comparison rule used by the answer cross-check.  Contains no
    [sqlite3] dependency — reference-engine implementations live in the
    benchmark executables. *)

(** A database engine under benchmark.  Both granary and the reference C
    SQLite satisfy this; the harness is written once against it. *)
module type ENGINE = sig
  type t

  (** Short engine label used in the [engine] CSV column. *)
  val name : string

  (** [open_db ~dir] creates a fresh database beneath [dir], removing any
      previous database files there. *)
  val open_db : dir:string -> t

  (** [exec t sql] runs a statement for effect.  Raises on error. *)
  val exec : t -> string -> unit

  (** [query_rows t sql] runs a query and returns every row, each value
      rendered by the engine's canonical text conversion.  Raises on error. *)
  val query_rows : t -> string -> string list list

  (** [close t] releases the engine's resources. *)
  val close : t -> unit
end

(** [time_it f] runs [f] and returns its result with elapsed wall-clock
    seconds and consumed CPU seconds (user + system). *)
val time_it : (unit -> 'a) -> 'a * float * float

(** [real_eq a b] is the benchmark's float equality: equal within a relative
    epsilon of [1e-9], or both within [1e-6] of zero. Identical infinities
    ([infinity]/[infinity] or [neg_infinity]/[neg_infinity]) compare equal;
    mismatched infinities and any [nan] compare unequal (including
    [nan]/[nan], per IEEE-754). *)
val real_eq : float -> float -> bool

(** [env_int key default] reads an integer environment variable, falling back
    to [default] when unset or unparseable. *)
val env_int : string -> int -> int

(** [env_float key default] reads a float environment variable, falling back
    to [default] when unset or unparseable. *)
val env_float : string -> float -> float

(** [env_str key default] reads a string environment variable, falling back to
    [default] when unset or empty. *)
val env_str : string -> string -> string

(** [host_label ()] is the CSV [host] column: [GRANARY_TPC_HOST] when set,
    otherwise the system hostname, otherwise ["unknown"]. *)
val host_label : unit -> string

(** RFC 4180 CSV emission. *)
module Csv : sig
  (** [row fields] renders one CSV line, quoting fields that contain a comma,
      a double quote, or a newline, and doubling embedded quotes.  No trailing
      newline. *)
  val row : string list -> string

  (** [header names] renders the header line.  Identical to {!row}; named
      separately so call sites read clearly. *)
  val header : string list -> string
end
