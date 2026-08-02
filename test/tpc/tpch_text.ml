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

(* Every phrase below binds each draw to its own [let], in the order the words
   are meant to be produced, and only then concatenates.  Two draws written as
   operands of [^] — as these were until #509 — are evaluated in a
   toolchain-dependent order, and each draw advances the LCG, so the pool of
   comment text a seed produced varied by compiler and flambda setting.  Since
   every TPC-H *_comment column is a substring of this pool, that voided the
   reproducibility contract for a large part of the dataset.  [join] exists so
   the sequencing is expressed once instead of at each of the twelve sites. *)
let join parts = String.concat "" parts

let noun_phrase r =
  match Tpc_rand.int_between r ~lo:0 ~hi:3 with
  | 0 -> Tpc_rand.pick r nouns
  | 1 ->
    let adj = Tpc_rand.pick r adjectives in
    let noun = Tpc_rand.pick r nouns in
    join [ adj; " "; noun ]
  | 2 ->
    let adj1 = Tpc_rand.pick r adjectives in
    let adj2 = Tpc_rand.pick r adjectives in
    let noun = Tpc_rand.pick r nouns in
    join [ adj1; ", "; adj2; " "; noun ]
  | _ ->
    let adv = Tpc_rand.pick r adverbs in
    let adj = Tpc_rand.pick r adjectives in
    let noun = Tpc_rand.pick r nouns in
    join [ adv; " "; adj; " "; noun ]
;;

let verb_phrase r =
  match Tpc_rand.int_between r ~lo:0 ~hi:3 with
  | 0 -> Tpc_rand.pick r verbs
  | 1 ->
    let adv = Tpc_rand.pick r adverbs in
    let verb = Tpc_rand.pick r verbs in
    join [ adv; " "; verb ]
  | 2 ->
    let verb = Tpc_rand.pick r verbs in
    let adv = Tpc_rand.pick r adverbs in
    join [ verb; " "; adv ]
  | _ ->
    let adv1 = Tpc_rand.pick r adverbs in
    let verb = Tpc_rand.pick r verbs in
    let adv2 = Tpc_rand.pick r adverbs in
    join [ adv1; " "; verb; " "; adv2 ]
;;

let prepositional_phrase r =
  let prep = Tpc_rand.pick r prepositions in
  let noun = noun_phrase r in
  join [ prep; " the "; noun ]
;;

let sentence r =
  match Tpc_rand.int_between r ~lo:0 ~hi:4 with
  | 0 ->
    let np = noun_phrase r in
    let vp = verb_phrase r in
    let term = Tpc_rand.pick r terminators in
    join [ np; " "; vp; " "; term ]
  | 1 ->
    let np = noun_phrase r in
    let vp = verb_phrase r in
    let pp = prepositional_phrase r in
    let term = Tpc_rand.pick r terminators in
    join [ np; " "; vp; " "; pp; " "; term ]
  | 2 ->
    let np1 = noun_phrase r in
    let vp = verb_phrase r in
    let np2 = noun_phrase r in
    let term = Tpc_rand.pick r terminators in
    join [ np1; " "; vp; " "; np2; " "; term ]
  | 3 ->
    let pp = prepositional_phrase r in
    let np = noun_phrase r in
    let vp = verb_phrase r in
    let term = Tpc_rand.pick r terminators in
    join [ pp; " "; np; " "; vp; " "; term ]
  | _ ->
    let pp1 = prepositional_phrase r in
    let np = noun_phrase r in
    let vp = verb_phrase r in
    let pp2 = prepositional_phrase r in
    let term = Tpc_rand.pick r terminators in
    join [ pp1; " "; np; " "; vp; " "; pp2; " "; term ]
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
