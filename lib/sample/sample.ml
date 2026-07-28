module Db = Granary.Db

(* Result-aware bind over an [(_, _) result Lwt.t]: short-circuits on the first
   [Error] so the workload reads as a flat sequence (keeps merlint nesting low). *)
let ( let*? ) (m : ('a, 'e) result Lwt.t) (f : 'a -> ('b, 'e) result Lwt.t)
  : ('b, 'e) result Lwt.t
  =
  Lwt.bind m (function
    | Ok x -> f x
    | Error e -> Lwt.return_error e)
;;

let run_demo db =
  let exec sql = Db.execute db sql in
  let*? () = exec "CREATE TABLE kv (id INTEGER PRIMARY KEY, name TEXT NOT NULL)" in
  let*? () = exec "BEGIN" in
  let*? () = exec "INSERT INTO kv (id, name) VALUES (1, 'alpha')" in
  let*? () = exec "INSERT INTO kv (id, name) VALUES (2, 'beta')" in
  let*? () = exec "INSERT INTO kv (id, name) VALUES (3, 'gamma')" in
  let*? () = exec "COMMIT" in
  let*? stream = Db.query db "SELECT id, name FROM kv ORDER BY id" in
  Lwt.bind (Lwt_stream.to_list stream) (fun rows -> Lwt.return_ok (List.length rows))
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-8"]
[@@@ai_provider "Anthropic"]
