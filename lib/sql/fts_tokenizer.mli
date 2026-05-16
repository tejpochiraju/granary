(** FTS word tokenizer.

    Splits text on non-alphanumeric boundaries, lowercases each token,
    and assigns monotonically-increasing position numbers per column.
    High-byte UTF-8 sequences (>127) are treated as word characters so
    that multi-byte Unicode letters are not split on byte boundaries.  *)

type token = {
  term : string;  (** lowercased word *)
  col  : int;     (** which input column (0-indexed) *)
  pos  : int;     (** word position within this column (0-indexed) *)
}

(** [tokenize col_texts] tokenizes a list of [(col_index, text)] pairs.
    Tokens for each column are numbered starting at 0. *)
val tokenize : (int * string) list -> token list
