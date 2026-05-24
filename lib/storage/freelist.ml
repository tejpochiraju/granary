(** In-memory freelist.

    Internally stores a list of [(page_id, freed_at_txn_id)] pairs.
    No sorting is maintained on add; [pop] performs a linear scan to find the
    reusable entry with the lowest [freed_at_txn_id] (oldest freed first).
    All operations are pure — no mutation. *)

type t = (int32 * int64) list
(** [(page_id, freed_at_txn_id)] pairs, in insertion order. *)

let empty : t = []

let add (t : t) ~page_id ~freed_at_txn_id : t =
  (page_id, freed_at_txn_id) :: t

(** Find the entry with the lowest freed_at_txn_id among those where
    freed_at_txn_id < min_safe_txn_id.  Returns [(best_entry, rest)] or [None]. *)
let pop (t : t) ~min_safe_txn_id : (int32 * t) option =
  (* Scan for the reusable entry with the minimum freed_at_txn_id *)
  let best =
    List.fold_left
      (fun acc (pid, txn) ->
         if Int64.compare txn min_safe_txn_id < 0 then
           match acc with
           | None -> Some (pid, txn)
           | Some (_, best_txn) ->
             if Int64.compare txn best_txn < 0 then Some (pid, txn)
             else acc
         else
           acc)
      None
      t
  in
  match best with
  | None -> None
  | Some (pid, txn) ->
    (* Remove the first occurrence of this exact entry *)
    let rec remove_first = function
      | [] -> []
      | (p, tx) :: rest when p = pid && tx = txn -> rest
      | x :: rest -> x :: remove_first rest
    in
    Some (pid, remove_first t)

let to_list (t : t) : (int32 * int64) list = t

let of_list (l : (int32 * int64) list) : t = l

let size (t : t) : int = List.length t

let reusable_count (t : t) ~min_safe_txn_id : int =
  List.fold_left
    (fun acc (_, txn) ->
       if Int64.compare txn min_safe_txn_id < 0 then acc + 1
       else acc)
    0
    t

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
