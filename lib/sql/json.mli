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

val parse     : string -> (value, string) result
val to_string : value -> string
val type_name : value -> string

val path_get    : value -> string -> value option
val path_set    : value -> string -> value -> value
val path_insert : value -> string -> value -> value
val path_replace: value -> string -> value -> value
val path_remove : value -> string -> value
