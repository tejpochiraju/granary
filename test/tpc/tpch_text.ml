let nouns =
  [| "foxes"
   ; "ideas"
   ; "theodolites"
   ; "pinto beans"
   ; "instructions"
   ; "dependencies"
   ; "excuses"
   ; "platelets"
   ; "asymptotes"
   ; "courts"
   ; "dolphins"
   ; "multipliers"
   ; "sauternes"
   ; "warthogs"
   ; "frets"
   ; "dinos"
   ; "attainments"
   ; "somas"
   ; "Tiresias"
   ; "patterns"
   ; "forges"
   ; "braids"
   ; "hockey players"
   ; "frays"
   ; "warhorses"
   ; "dugouts"
   ; "notornis"
   ; "epitaphs"
   ; "pearls"
   ; "tithes"
   ; "waters"
   ; "orbits"
   ; "gifts"
   ; "sheaves"
   ; "depths"
   ; "sentiments"
   ; "decoys"
   ; "realms"
   ; "pains"
   ; "grouches"
   ; "escapades"
   ; "packages"
   ; "requests"
   ; "accounts"
   ; "deposits"
  |]
;;

let verbs =
  [| "sleep"
   ; "wake"
   ; "are"
   ; "cajole"
   ; "haggle"
   ; "nag"
   ; "use"
   ; "boost"
   ; "affix"
   ; "detect"
   ; "integrate"
   ; "maintain"
   ; "nod"
   ; "was"
   ; "lose"
   ; "sublate"
   ; "solve"
   ; "thrash"
   ; "promise"
   ; "engage"
   ; "hinder"
   ; "print"
   ; "x-ray"
   ; "breach"
   ; "eat"
   ; "grow"
   ; "impress"
   ; "mold"
   ; "poach"
   ; "serve"
   ; "run"
   ; "dazzle"
   ; "snooze"
   ; "doze"
   ; "unwind"
   ; "kindle"
   ; "play"
   ; "hang"
   ; "believe"
   ; "doubt"
  |]
;;

let adjectives =
  [| "furious"
   ; "sly"
   ; "careful"
   ; "blithe"
   ; "quick"
   ; "fluffy"
   ; "slow"
   ; "quiet"
   ; "ruthless"
   ; "thin"
   ; "close"
   ; "dogged"
   ; "daring"
   ; "brave"
   ; "stealthy"
   ; "permanent"
   ; "enticing"
   ; "idle"
   ; "busy"
   ; "regular"
   ; "final"
   ; "ironic"
   ; "even"
   ; "bold"
   ; "silent"
   ; "special"
   ; "pending"
   ; "unusual"
   ; "express"
  |]
;;

let adverbs =
  [| "sometimes"
   ; "always"
   ; "never"
   ; "furiously"
   ; "slyly"
   ; "carefully"
   ; "blithely"
   ; "quickly"
   ; "fluffily"
   ; "slowly"
   ; "quietly"
   ; "ruthlessly"
   ; "thinly"
   ; "closely"
   ; "doggedly"
   ; "daringly"
   ; "bravely"
   ; "stealthily"
   ; "permanently"
   ; "enticingly"
   ; "idly"
   ; "busily"
   ; "regularly"
   ; "finally"
   ; "ironically"
   ; "evenly"
   ; "boldly"
   ; "silently"
   ; "specially"
   ; "pendingly"
   ; "unusually"
   ; "expressly"
  |]
;;

let prepositions =
  [| "about"
   ; "above"
   ; "according to"
   ; "across"
   ; "after"
   ; "against"
   ; "along"
   ; "alongside of"
   ; "among"
   ; "around"
   ; "at"
   ; "atop"
   ; "before"
   ; "behind"
   ; "beneath"
   ; "beside"
   ; "besides"
   ; "between"
   ; "beyond"
   ; "by"
   ; "despite"
   ; "during"
   ; "except"
   ; "for"
   ; "from"
   ; "in place of"
   ; "inside"
   ; "instead of"
   ; "into"
   ; "near"
   ; "of"
   ; "on"
   ; "outside"
   ; "over"
   ; "past"
   ; "since"
   ; "through"
   ; "throughout"
   ; "to"
   ; "toward"
   ; "under"
   ; "until"
   ; "up"
   ; "upon"
   ; "whithout"
   ; "with"
   ; "within"
  |]
;;

let terminators = [| "."; ";"; ":"; "?"; "!"; "--" |]

(* The spec composes a noun phrase, a verb phrase, and a preposition into a
   handful of sentence forms.  Kept to four forms and one level of nesting so
   the module stays within merlint's nesting limit. *)

let noun_phrase r =
  match Tpc_rand.int_between r ~lo:0 ~hi:3 with
  | 0 -> Tpc_rand.pick r nouns
  | 1 -> Tpc_rand.pick r adjectives ^ " " ^ Tpc_rand.pick r nouns
  | 2 ->
    Tpc_rand.pick r adjectives
    ^ ", "
    ^ Tpc_rand.pick r adjectives
    ^ " "
    ^ Tpc_rand.pick r nouns
  | _ ->
    Tpc_rand.pick r adverbs
    ^ " "
    ^ Tpc_rand.pick r adjectives
    ^ " "
    ^ Tpc_rand.pick r nouns
;;

let verb_phrase r =
  match Tpc_rand.int_between r ~lo:0 ~hi:3 with
  | 0 -> Tpc_rand.pick r verbs
  | 1 -> Tpc_rand.pick r adverbs ^ " " ^ Tpc_rand.pick r verbs
  | 2 -> Tpc_rand.pick r verbs ^ " " ^ Tpc_rand.pick r adverbs
  | _ ->
    Tpc_rand.pick r adverbs ^ " " ^ Tpc_rand.pick r verbs ^ " " ^ Tpc_rand.pick r adverbs
;;

let prepositional_phrase r = Tpc_rand.pick r prepositions ^ " the " ^ noun_phrase r

let sentence r =
  match Tpc_rand.int_between r ~lo:0 ~hi:4 with
  | 0 -> noun_phrase r ^ " " ^ verb_phrase r ^ " " ^ Tpc_rand.pick r terminators
  | 1 ->
    noun_phrase r
    ^ " "
    ^ verb_phrase r
    ^ " "
    ^ prepositional_phrase r
    ^ " "
    ^ Tpc_rand.pick r terminators
  | 2 ->
    noun_phrase r
    ^ " "
    ^ verb_phrase r
    ^ " "
    ^ noun_phrase r
    ^ " "
    ^ Tpc_rand.pick r terminators
  | 3 ->
    prepositional_phrase r
    ^ " "
    ^ noun_phrase r
    ^ " "
    ^ verb_phrase r
    ^ " "
    ^ Tpc_rand.pick r terminators
  | _ ->
    prepositional_phrase r
    ^ " "
    ^ noun_phrase r
    ^ " "
    ^ verb_phrase r
    ^ " "
    ^ prepositional_phrase r
    ^ " "
    ^ Tpc_rand.pick r terminators
;;

let pool r ~size =
  let buf = Buffer.create (size + 256) in
  while Buffer.length buf < size do
    Buffer.add_string buf (sentence r);
    Buffer.add_char buf ' '
  done;
  Buffer.contents buf
;;

let substring ~pool r ~lo ~hi =
  let n = Tpc_rand.int_between r ~lo ~hi in
  let max_start = String.length pool - n in
  if max_start <= 0
  then String.sub pool 0 (min n (String.length pool))
  else String.sub pool (Tpc_rand.int_between r ~lo:0 ~hi:max_start) n
;;
