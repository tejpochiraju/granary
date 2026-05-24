type fts_term =
  | FT_exact of string
  | FT_prefix of string
  | FT_phrase of string list

type t =
  | FQ_and of t list
  | FQ_or of t list
  | FQ_not of t
  | FQ_term of fts_term

(* Lex the query string into raw tokens *)
type raw_tok =
  | RT_word of string
  | RT_phrase of string
  | RT_or
  | RT_not_word of string

let lex query =
  let n = String.length query in
  let toks = ref [] in
  let i = ref 0 in
  let skip_ws () =
    while !i < n && query.[!i] = ' ' do
      incr i
    done
  in
  while !i < n do
    skip_ws ();
    if !i >= n
    then ()
    else if query.[!i] = '"'
    then (
      incr i;
      let start = !i in
      while !i < n && query.[!i] <> '"' do
        incr i
      done;
      toks := RT_phrase (String.sub query start (!i - start)) :: !toks;
      if !i < n then incr i)
    else if query.[!i] = '-' && !i + 1 < n && query.[!i + 1] <> ' '
    then (
      incr i;
      let start = !i in
      while !i < n && query.[!i] <> ' ' do
        incr i
      done;
      toks := RT_not_word (String.sub query start (!i - start)) :: !toks)
    else (
      let start = !i in
      while !i < n && query.[!i] <> ' ' do
        incr i
      done;
      let word = String.sub query start (!i - start) in
      toks
      := (if String.uppercase_ascii word = "OR" then RT_or else RT_word word) :: !toks)
  done;
  List.rev !toks
;;

(* Normalize a query term through the same Unicode pipeline used to
   tokenize documents. For multi-token inputs (e.g. an accent + space
   in a phrase) Fts_tokenizer may emit multiple tokens; for a single
   query word we take the concatenation of the folded forms, which
   matches what the document indexer stored. *)
let fold_term raw =
  let toks = Fts_tokenizer.tokenize_string ~col:0 raw in
  String.concat "" (List.map (fun t -> t.Fts_tokenizer.term) toks)
;;

let split_phrase p =
  List.filter
    (fun s -> s <> "")
    (List.map (fun t -> t.Fts_tokenizer.term) (Fts_tokenizer.tokenize_string ~col:0 p))
;;

let parse_raw_tok = function
  | RT_word w ->
    let q =
      if String.length w > 0 && w.[String.length w - 1] = '*'
      then FQ_term (FT_prefix (fold_term (String.sub w 0 (String.length w - 1))))
      else FQ_term (FT_exact (fold_term w))
    in
    Ok q
  | RT_phrase p ->
    let words = split_phrase p in
    Ok (FQ_term (FT_phrase words))
  | RT_not_word w -> Ok (FQ_not (FQ_term (FT_exact (fold_term w))))
  | RT_or -> Error "unexpected OR"
;;

let parse query =
  let raw = lex query in
  (* Split on OR to form OR-groups; within each group AND is implied *)
  let groups_rev =
    List.fold_left
      (fun (cur, groups) tok ->
         match tok with
         | RT_or -> [], cur :: groups
         | t -> t :: cur, groups)
      ([], [])
      raw
  in
  let last, rest = groups_rev in
  let all_groups = List.rev (last :: rest) in
  let parse_group toks =
    let results = List.map parse_raw_tok toks in
    let errors =
      List.filter_map
        (function
          | Error e -> Some e
          | Ok _ -> None)
        results
    in
    match errors with
    | e :: _ -> Error e
    | [] ->
      let qs =
        List.filter_map
          (function
            | Ok q -> Some q
            | Error _ -> None)
          results
      in
      (match qs with
       | [] -> Error "empty query group"
       | [ q ] -> Ok q
       | qs -> Ok (FQ_and qs))
  in
  let group_results = List.map parse_group all_groups in
  let errors =
    List.filter_map
      (function
        | Error e -> Some e
        | Ok _ -> None)
      group_results
  in
  match errors with
  | e :: _ -> Error e
  | [] ->
    let qs =
      List.filter_map
        (function
          | Ok q -> Some q
          | Error _ -> None)
        group_results
    in
    (match qs with
     | [] -> Error "empty query"
     | [ q ] -> Ok q
     | qs -> Ok (FQ_or qs))
;;

let rec collect_terms = function
  | FQ_term t -> [ t ]
  | FQ_and qs -> List.concat_map collect_terms qs
  | FQ_or qs -> List.concat_map collect_terms qs
  | FQ_not _ -> [] (* negated terms don't need posting lists *)
;;

let pp_term fmt = function
  | FT_exact s -> Format.pp_print_string fmt s
  | FT_prefix s -> Format.fprintf fmt "%s*" s
  | FT_phrase ws -> Format.fprintf fmt "\"%s\"" (String.concat " " ws)
;;

let rec pp fmt = function
  | FQ_term t -> pp_term fmt t
  | FQ_not q -> Format.fprintf fmt "(NOT %a)" pp q
  | FQ_and qs -> Format.fprintf fmt "(AND %a)" pp_list qs
  | FQ_or qs -> Format.fprintf fmt "(OR %a)" pp_list qs

and pp_list fmt qs =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ' ') pp fmt qs
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
