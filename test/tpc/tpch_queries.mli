(** The 22 TPC-H-derived queries (#482).

    Substitution parameters are fixed at the spec's validation values rather than
    randomized, so successive runs are comparable and the SQLite cross-check has
    a stable target.

    Each query records how faithfully it could be expressed in granary's SQL
    dialect. Verdicts are established by running the queries, never by predicting
    from the grammar. *)

(** How faithfully a query could be expressed. *)
type verdict =
  | Native (** runs on granary as the spec writes it; the SQL is untouched *)
  | Rewritten of string (** runs after a transformation; the string explains it *)
  | Rewritten_pending of string
  (** the SQL has been transformed — it is no longer the spec text — but granary
          still does not produce the right answer for it: either it errors out,
          or it runs and disagrees with reference SQLite. The string names the
          transformation and the remaining blocker. Distinct from {!Native} so
          that a reader of the benchmark CSV cannot mistake it for untouched
          spec SQL, and from {!Rewritten} so that it is never counted as a
          success. *)
  | Skipped of string
  (** cannot be expressed; the string names the missing capability and cites
          the Forgejo issue tracking it *)

type query =
  { number : int (** 1–22 *)
  ; sql : string
    (** the query text; retained even when skipped, as the record of the
          attempted rewrite *)
  ; setup : string list
    (** statements a rewrite depends on, typically CREATE VIEW. Run before the
          timed section and never counted in a query's measured time. *)
  ; verdict : verdict
  }

(** All 22 queries, ordered by number. *)
val all : query list

(** [find n] is the query numbered [n], or [None] outside 1–22. *)
val find : int -> query option

(** [verdict_label v] is the CSV token: ["native"], ["rewritten"],
    ["rewritten-not-yet-running"], or ["skipped"]. The tokens are
    comma-free so they need no CSV quoting. *)
val verdict_label : verdict -> string

(** [view_name_of_setup stmt] is the name of the view created by [stmt] when
    [stmt] is a [CREATE VIEW name ...] statement (with or without an explicit
    column list), or [None] otherwise. Used to drop a query's setup views
    before and after it runs, since the harness does not discard the database
    between queries. *)
val view_name_of_setup : string -> string option
