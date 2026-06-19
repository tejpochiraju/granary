module type SPEC = sig
  module In : Zset.S
  module Out : Zset.S

  type group

  val compare_group : group -> group -> int
  val group_of : In.elt -> group
  val measure : In.elt -> int
  val result : group -> int -> Out.elt
end

module Make (S : SPEC) = struct
  module GMap = Map.Make (struct
      type t = S.group

      let compare = S.compare_group
    end)

  (* Per-group running totals: [mult] is Σ weight (the group exists iff [mult >
     0]); [aggv] is Σ measure*weight (the value of its result row). *)
  type acc =
    { mult : int
    ; aggv : int
    }

  type t =
    { mutable groups : acc GMap.t
    ; mutable out : S.Out.t
    }

  let create () = { groups = GMap.empty; out = S.Out.zero }
  let zero_acc = { mult = 0; aggv = 0 }

  let get m g =
    match GMap.find_opt g m with
    | Some a -> a
    | None -> zero_acc
  ;;

  (* The per-group [(mult, aggv)] change contributed by one input delta. *)
  let group_deltas delta =
    S.In.fold
      (fun e w acc ->
         let g = S.group_of e in
         let cur = get acc g in
         GMap.add g { mult = cur.mult + w; aggv = cur.aggv + (S.measure e * w) } acc)
      delta
      GMap.empty
  ;;

  (* The output delta for one group transitioning [old_acc] -> [new_acc]: retract
     the old visible row (if it existed) and insert the new one (if it exists). *)
  let row_delta g old_acc new_acc =
    let d = S.Out.zero in
    let d =
      if old_acc.mult > 0
      then S.Out.add d (S.Out.singleton (S.result g old_acc.aggv) (-1))
      else d
    in
    if new_acc.mult > 0
    then S.Out.add d (S.Out.singleton (S.result g new_acc.aggv) 1)
    else d
  ;;

  let step t delta =
    let out_delta = ref S.Out.zero in
    GMap.iter
      (fun g d ->
         let old_acc = get t.groups g in
         let new_acc = { mult = old_acc.mult + d.mult; aggv = old_acc.aggv + d.aggv } in
         out_delta := S.Out.add !out_delta (row_delta g old_acc new_acc);
         t.groups
         <- (if new_acc.mult = 0 && new_acc.aggv = 0
             then GMap.remove g t.groups
             else GMap.add g new_acc t.groups))
      (group_deltas delta);
    t.out <- S.Out.add t.out !out_delta;
    !out_delta
  ;;

  let output t = t.out
end
