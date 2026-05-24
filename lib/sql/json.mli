(** JSON value model and the SQLite [json1] scalar functions.

    Provides a parsed {!value} tree, text (de)serialisation, type
    introspection, and the path-based accessors/mutators ([path_get],
    [path_set], [path_insert], [path_replace], [path_remove]) used to
    implement the SQL JSON functions. *)

type value =
  | J_null
  | J_bool of bool
  | J_int  of int64
  | J_float of float
  | J_string of string
  | J_array  of value list
  | J_object of (string * value) list

(** Parse JSON text into a {!value}, or [Error msg] on malformed input. *)
val parse     : string -> (value, string) result

(** Serialise a {!value} back to compact JSON text. *)
val to_string : value -> string

(** SQL [json_type] name of a value ("null"/"true"/"integer"/"object"/...). *)
val type_name : value -> string

(** [path_get v p] returns the value at JSON path [p] (e.g. ["$.a[0]"]),
    or [None] if the path does not resolve. *)
val path_get    : value -> string -> value option

(** [path_set v p x] sets the value at path [p] to [x], creating or
    overwriting as needed (SQL [json_set]). *)
val path_set    : value -> string -> value -> value

(** [path_insert v p x] sets path [p] to [x] only if it does not already
    exist (SQL [json_insert]). *)
val path_insert : value -> string -> value -> value

(** [path_replace v p x] sets path [p] to [x] only if it already exists
    (SQL [json_replace]). *)
val path_replace: value -> string -> value -> value

(** [path_remove v p] removes the element at path [p] (SQL [json_remove]). *)
val path_remove : value -> string -> value
