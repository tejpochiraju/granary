(** FTS word tokenizer.

    Splits text on non-alphanumeric boundaries, lowercases each token,
    and assigns monotonically-increasing position numbers per column.
    High-byte UTF-8 sequences (>127) are treated as word characters so
    that multi-byte Unicode letters are not split on byte boundaries.  *)

type token = {
  term       : string;  (** lowercased word *)
  col        : int;     (** which input column (0-indexed) *)
  pos        : int;     (** word position within this column (0-indexed) *)
  start_byte : int;     (** byte offset of first byte of raw token in source string *)
  end_byte   : int;     (** byte offset one past last byte of raw token *)
}

(** [tokenize_string ~col text] tokenizes a single text string as column [col].
    Tokens are numbered starting at 0 within this column. *)
val tokenize_string : col:int -> string -> token list

(** [tokenize col_texts] tokenizes a list of [(col_index, text)] pairs.
    Tokens for each column are numbered starting at 0. *)
val tokenize : (int * string) list -> token list
