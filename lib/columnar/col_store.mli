(** In-memory columnar store for COLUMNSTORE tables.

    Rows are stored as per-column Bigarray arrays for cache-friendly scan
    performance. Text columns use dictionary encoding. *)

module Row = Sqlocaml_encoding.Row

type t

(** [create columns] returns an empty store with the given schema. *)
val create : Row.column list -> t

(** [nrows store] returns the number of rows currently held. *)
val nrows : t -> int

(** [columns store] returns the schema that was passed to {!create}. *)
val columns : t -> Row.column list

(** [insert_rows store batch] appends [batch] to the store, transposing rows
    into typed column arrays. *)
val insert_rows : t -> Row.t array -> unit

(** [to_row_seq store] streams all rows in insertion order by reading from the
    typed column arrays. *)
val to_row_seq : t -> Row.t Seq.t

(** [encode store] serializes the entire store (schema + data) to a byte string. *)
val encode : t -> bytes

(** [decode ?off schema buf] deserializes a store from [buf] starting at
    offset [off] (default 0) using the given schema.
    Raises [Failure] on corrupt or malformed data. *)
val decode : ?off:int -> Row.column list -> bytes -> t

(** [dirty store] returns [true] if the store has been mutated since the
    last call to {!mark_clean} (i.e. has unpersisted changes). *)
val dirty : t -> bool

(** [mark_clean store] resets the dirty flag. *)
val mark_clean : t -> unit

(** [mark_dirty store] sets the dirty flag. *)
val mark_dirty : t -> unit
