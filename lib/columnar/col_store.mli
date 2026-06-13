(** In-memory columnar store for COLUMNSTORE tables.

    Rows are accumulated as {!Row.t} arrays; M2 will replace this with
    typed Bigarray columns for cache-friendly scan performance. *)

module Row = Sqlocaml_encoding.Row

type t

(** [create columns] returns an empty store with the given schema. *)
val create : Row.column list -> t

(** [nrows store] returns the number of rows currently held. *)
val nrows : t -> int

(** [columns store] returns the schema that was passed to {!create}. *)
val columns : t -> Row.column list

(** [insert_rows store batch] appends [batch] to the store in O(1). *)
val insert_rows : t -> Row.t array -> unit

(** [to_row_seq store] streams all rows in insertion order. *)
val to_row_seq : t -> Row.t Seq.t
