(* Compile-time proof that the sqlocaml core can be consumed by a library that
   declares no [unix] / [lwt.unix] dependency (acceptance check for the
   platform-agnostic core, #170).  This unit references only the core's
   platform-agnostic entry points; its dune stanza lists [sqlocaml] (+ [lwt])
   and nothing Unix-specific, so it fails to build if the core ever regains a
   direct OS dependency that the core's own dune does not declare. *)

(* Reference the agnostic block entry point so the link is genuine. *)
let _open_block = Sqlocaml.Db.open_block
let probe () : Sqlocaml.Db.t Lwt.t = Sqlocaml.Db.open_in_memory ()

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
