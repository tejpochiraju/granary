type token = {
  term       : string;
  col        : int;
  pos        : int;
  start_byte : int;
  end_byte   : int;
}

let is_word_char c =
  let n = Char.code c in
  (n >= 65 && n <= 90)    (* A-Z *)
  || (n >= 97 && n <= 122) (* a-z *)
  || (n >= 48 && n <= 57)  (* 0-9 *)
  || n > 127               (* high UTF-8 byte: treat as word char *)

let tokenize_string ~col text =
  let n = String.length text in
  let tokens = ref [] in
  let pos = ref 0 in
  let i = ref 0 in
  while !i < n do
    (* skip non-word chars *)
    while !i < n && not (is_word_char text.[!i]) do incr i done;
    let start = !i in
    (* consume word chars *)
    while !i < n && is_word_char text.[!i] do incr i done;
    if !i > start then begin
      let raw  = String.sub text start (!i - start) in
      let term = String.lowercase_ascii raw in
      tokens := { term; col; pos = !pos;
                  start_byte = start; end_byte = !i } :: !tokens;
      incr pos
    end
  done;
  List.rev !tokens

let tokenize col_texts =
  List.concat_map (fun (col, text) -> tokenize_string ~col text) col_texts
