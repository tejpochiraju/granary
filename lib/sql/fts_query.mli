(** FTS query string parser.

    Parses a MATCH query string into a structured query tree.

    Syntax:
    - Space-separated terms → implicit AND
    - [term OR term] → OR
    - [-term] or [NOT term] → NOT (negation — must be combined with a positive term)
    - ["term1 term2"] → phrase (terms must appear consecutively in same column)
    - [term*] → prefix search

    All terms are lowercased before matching.
*)

type fts_term =
  | FT_exact of string (** single exact term *)
  | FT_prefix of string (** prefix: "foo*" stored as "foo" *)
  | FT_phrase of string list (** phrase: consecutive terms *)

type t =
  | FQ_and of t list (** all must match (default for spaces) *)
  | FQ_or of t list (** any must match *)
  | FQ_not of t (** must NOT match; used inside FQ_and *)
  | FQ_term of fts_term

(** Parse a raw MATCH query string. Returns [Error msg] on malformed input. *)
val parse : string -> (t, string) result

(** Collect all positive (non-negated) terms from a query, for posting-list lookups. *)
val collect_terms : t -> fts_term list

(** Pretty-print a query tree in a parenthesised AND/OR/NOT form. *)
val pp : Format.formatter -> t -> unit
