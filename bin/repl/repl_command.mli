(** Pure parsing/routing for the REPL, extracted from the executable so it is
    unit-testable (#382 review). *)

(** A dot-command parsed from a line beginning with '.'. *)
type dot =
  | Help
  | Quit
  | Tables
  | Schema of string option
  | Databases
  | Open of string
  | Unknown of string

(** What an input line resolves to, given whether the monitor filter prompt is
    active. *)
type action =
  | Empty
  | Filter of int64 option (** filter-mode submit: parsed txn id (None = clear/invalid) *)
  | Dot of dot
  | Sql of string list (** one or more statements to run *)

val parse_dot : string -> dot

(** [classify ~filter_mode input] routes a submitted input line. *)
val classify : filter_mode:bool -> string -> action

(* Pure SQL builders for the dot-commands (so their text — incl. the .schema
   single-quote escaping — is testable). *)
val tables_sql : string
val databases_sql : string
val schema_sql : string option -> string
