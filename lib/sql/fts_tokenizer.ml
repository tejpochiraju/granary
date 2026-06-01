type token =
  { term : string
  ; col : int
  ; pos : int
  ; start_byte : int
  ; end_byte : int
  }

(* unicode61-like classification: word chars = letters, numbers, marks,
   connector-punct (e.g. underscore). Everything else is a separator. *)
let is_word_uchar u =
  match Uucp.Gc.general_category u with
  | `Lu | `Ll | `Lt | `Lm | `Lo | `Nd | `Nl | `No | `Mn | `Mc | `Me | `Pc -> true
  | _ -> false
;;

let utf8_byte_len u =
  let c = Uchar.to_int u in
  if c < 0x80 then 1 else if c < 0x800 then 2 else if c < 0x10000 then 3 else 4
;;

let buffer_add_uchar buf u =
  let b = Buffer.create 4 in
  Uutf.Buffer.add_utf_8 b u;
  Buffer.add_string buf (Buffer.contents b)
;;

let tokenize_string ~col text =
  let n = String.length text in
  let dec = Uutf.decoder ~encoding:`UTF_8 (`String text) in
  let tokens = ref [] in
  let pos = ref 0 in
  let buf = Buffer.create 16 in
  let run_start = ref (-1) in
  (* byte offset of first uchar of current run *)
  let run_end = ref (-1) in
  (* byte offset just past last uchar of run *)
  let emit () =
    if !run_start >= 0
    then (
      tokens
      := { term = Buffer.contents buf
         ; col
         ; pos = !pos
         ; start_byte = !run_start
         ; end_byte = !run_end
         }
         :: !tokens;
      incr pos;
      Buffer.clear buf;
      run_start := -1;
      run_end := -1)
  in
  let rec loop () =
    let i_before = Uutf.decoder_byte_count dec in
    match Uutf.decode dec with
    | `End | `Malformed _ -> emit ()
    | `Uchar u ->
      let i_after = Uutf.decoder_byte_count dec in
      let _ = i_after in
      if is_word_uchar u
      then (
        if !run_start < 0 then run_start := i_before;
        (match Uucp.Case.Fold.fold u with
         | `Self -> buffer_add_uchar buf u
         | `Uchars us -> List.iter (buffer_add_uchar buf) us);
        run_end := i_before + utf8_byte_len u)
      else emit ();
      loop ()
    | `Await -> emit ()
  in
  loop ();
  let _ = n in
  List.rev !tokens
;;

let tokenize col_texts =
  List.concat_map (fun (col, text) -> tokenize_string ~col text) col_texts
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
