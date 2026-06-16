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
  | Dump of string option (** [.dump [path]]: export the event log (#385) *)
  | Unknown of string

(** Which filter prompt (if any) is currently active, so a submitted line is
    routed as filter input rather than SQL/dot (#385). *)
type prompt =
  | No_prompt
  | Txn_prompt
  | Table_prompt

(** Parsed filter-prompt input. *)
type filter_input =
  | Filter_txn of int64 option (** txn-id filter ([None] = clear/invalid) *)
  | Filter_table of string (** table-name filter (resolved to a tree elsewhere) *)

(** What an input line resolves to, given the active prompt. *)
type action =
  | Empty
  | Filter of filter_input
  | Dot of dot
  | Sql of string list (** one or more statements to run *)

(** [parse_dot line] parses a line beginning with '.' into a {!dot} command. *)
val parse_dot : string -> dot

(** [classify ~prompt input] routes a submitted input line according to the
    active {!prompt}. *)
val classify : prompt:prompt -> string -> action

(* Pure SQL builders for the dot-commands (so their text — incl. the .schema
   single-quote escaping — is testable). *)

(** SQL for [.tables]: list user tables. *)
val tables_sql : string

(** SQL for [.databases]. *)
val databases_sql : string

(** [schema_sql name] is the [.schema] query (all tables, or one; single-quotes
    in [name] are escaped). *)
val schema_sql : string option -> string
