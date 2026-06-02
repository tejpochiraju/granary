(** In-memory freelist.

    Maps each [freed_at_txn_id] to the page-ids freed at that txn. [pop]
    reuses the oldest-freed reusable page (lowest [freed_at_txn_id] among those
    with [freed_at_txn_id < min_safe_txn_id]).

    Keyed by txn lets both operations be O(log d) in the number of distinct
    freeing txns (small and bounded), instead of O(n) in the page count:
    - the non-reuse hot path (a long write txn whose own COW frees are never
      yet safe) short-circuits on [min_binding] without scanning;
    - the reuse path (allocating against a backlog of safely-freed pages, e.g.
      inserting into a table seeded by an earlier committed txn) pops in
      O(log d) instead of rescanning the whole growing list.

    A flat per-page list made every [alloc] O(freelist size); during bulk
    inserts that turned an n-row load into O(n^2) (#229). All operations are
    pure — no mutation. *)

module Txn_map = Map.Make (Int64)

type t =
  { by_txn : int32 list Txn_map.t (** freed_at_txn_id -> page_ids freed then *)
  ; count : int (** total pages held (sum of list lengths) *)
  }

let empty : t = { by_txn = Txn_map.empty; count = 0 }
let pp fmt t = Format.fprintf fmt "Freelist.t { entries = %d }" t.count

let add (t : t) ~page_id ~freed_at_txn_id : t =
  let cur = Option.value ~default:[] (Txn_map.find_opt freed_at_txn_id t.by_txn) in
  { by_txn = Txn_map.add freed_at_txn_id (page_id :: cur) t.by_txn; count = t.count + 1 }
;;

(** Pop the reusable entry with the lowest [freed_at_txn_id] (oldest freed
    first).  A page is reusable iff [freed_at_txn_id < min_safe_txn_id].
    O(log d) in the number of distinct freeing txns. *)
let pop (t : t) ~min_safe_txn_id : (int32 * t) option =
  match Txn_map.min_binding_opt t.by_txn with
  | Some (txn, pids) when Int64.compare txn min_safe_txn_id < 0 ->
    (match pids with
     | [] ->
       (* Empty list under a key never persists (see below); treat as absent. *)
       None
     | pid :: rest ->
       let by_txn =
         if rest = [] then Txn_map.remove txn t.by_txn else Txn_map.add txn rest t.by_txn
       in
       Some (pid, { by_txn; count = t.count - 1 }))
  | _ -> None (* empty, or the oldest free is not yet safe to reuse *)
;;

(* Flatten to (page_id, freed_at_txn_id) pairs.  Emits each txn's pages in
   reverse of their internal (newest-first) order so that [of_list] — which
   prepends via [add] — reconstructs the identical structure (round-trip
   stable: pop order is preserved across to_list/of_list). *)
let to_list (t : t) : (int32 * int64) list =
  Txn_map.fold
    (fun txn pids acc -> List.fold_left (fun a p -> (p, txn) :: a) acc pids)
    t.by_txn
    []
;;

let of_list (l : (int32 * int64) list) : t =
  List.fold_left
    (fun acc (page_id, freed_at_txn_id) -> add acc ~page_id ~freed_at_txn_id)
    empty
    l
;;

let size (t : t) : int = t.count

let reusable_count (t : t) ~min_safe_txn_id : int =
  Txn_map.fold
    (fun txn pids acc ->
       if Int64.compare txn min_safe_txn_id < 0 then acc + List.length pids else acc)
    t.by_txn
    0
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-8"]
[@@@ai_provider "Anthropic"]
