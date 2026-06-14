(** Typed column arrays backed by Bigarray.

    Each column is stored as a contiguous typed array — {!Int64} and {!Real}
    columns use Bigarray for cache-friendly sequential scan; {!Text} columns
    use dictionary encoding; {!Blob} columns use OCaml [bytes] arrays. *)

module Row = Sqlocaml_encoding.Row

type t

(** [pp fmt col] pretty-prints the column variant and row count. *)
val pp : Format.formatter -> t -> unit

(** [create ty cap] creates an empty column with the given type and initial
    capacity. *)
val create : Row.ty -> int -> t

(** [length col] returns the number of rows in the column. *)
val length : t -> int

(** [append_value col v] returns a new column with [v] appended. *)
val append_value : t -> Row.value -> t

(** [get_value col idx] returns the value at position [idx]. *)
val get_value : t -> int -> Row.value

(** [append_batch col rows] returns a new column with all [rows] appended. *)
val append_batch : t -> Row.value array -> t

(** [of_values schema rows] transposes a row-major array into column-major
    typed arrays, one per schema column. *)
val of_values : Row.column list -> Row.t array -> t array
