module Row = Sqlocaml_encoding.Row

type t

val create : Row.column list -> t
val nrows : t -> int
val columns : t -> Row.column list
val insert_rows : t -> Row.t array -> unit
val to_row_seq : t -> Row.t Seq.t
