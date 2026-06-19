module type SPEC = sig
  module Left : Zset.S
  module Right : Zset.S
  module Out : Zset.S

  type key

  val compare_key : key -> key -> int
  val key_left : Left.elt -> key
  val key_right : Right.elt -> key
  val combine : Left.elt -> Right.elt -> Out.elt
end

module Make (S : SPEC) = struct
  module KMap = Map.Make (struct
      type t = S.key

      let compare = S.compare_key
    end)

  type t =
    { mutable il : S.Left.t KMap.t (* integrated left input, indexed by join key *)
    ; mutable ir : S.Right.t KMap.t (* integrated right input, indexed by join key *)
    ; mutable out : S.Out.t (* materialized join output *)
    }

  let create () = { il = KMap.empty; ir = KMap.empty; out = S.Out.zero }

  let lookup_l idx k =
    match KMap.find_opt k idx with
    | Some z -> z
    | None -> S.Left.zero
  ;;

  let lookup_r idx k =
    match KMap.find_opt k idx with
    | Some z -> z
    | None -> S.Right.zero
  ;;

  (* Bucket a left delta into per-key Z-sets. *)
  let index_left (d : S.Left.t) : S.Left.t KMap.t =
    S.Left.fold
      (fun e w acc ->
         let k = S.key_left e in
         KMap.add k (S.Left.add (lookup_l acc k) (S.Left.singleton e w)) acc)
      d
      KMap.empty
  ;;

  let index_right (d : S.Right.t) : S.Right.t KMap.t =
    S.Right.fold
      (fun e w acc ->
         let k = S.key_right e in
         KMap.add k (S.Right.add (lookup_r acc k) (S.Right.singleton e w)) acc)
      d
      KMap.empty
  ;;

  (* [dl] joined against the right side reachable through [right_of]: each matched
     pair contributes [combine l r] with the product of weights. *)
  let join_left_with (dl : S.Left.t) (right_of : S.key -> S.Right.t) : S.Out.t =
    S.Left.fold
      (fun l wl acc ->
         S.Right.fold
           (fun r wr acc2 -> S.Out.add acc2 (S.Out.singleton (S.combine l r) (wl * wr)))
           (right_of (S.key_left l))
           acc)
      dl
      S.Out.zero
  ;;

  let join_right_with (dr : S.Right.t) (left_of : S.key -> S.Left.t) : S.Out.t =
    S.Right.fold
      (fun r wr acc ->
         S.Left.fold
           (fun l wl acc2 -> S.Out.add acc2 (S.Out.singleton (S.combine l r) (wl * wr)))
           (left_of (S.key_right r))
           acc)
      dr
      S.Out.zero
  ;;

  (* Fold an indexed left delta into the integrated left index, per key, dropping
     keys whose bucket cancels to empty. *)
  let merge_left into delta =
    KMap.fold
      (fun k z acc ->
         let merged = S.Left.add (lookup_l acc k) z in
         if S.Left.is_zero merged then KMap.remove k acc else KMap.add k merged acc)
      delta
      into
  ;;

  let merge_right into delta =
    KMap.fold
      (fun k z acc ->
         let merged = S.Right.add (lookup_r acc k) z in
         if S.Right.is_zero merged then KMap.remove k acc else KMap.add k merged acc)
      delta
      into
  ;;

  let step t ~left ~right =
    let dr_idx = index_right right in
    (* Δ(L ⋈ R) = ΔL ⋈ IR + IL ⋈ ΔR + ΔL ⋈ ΔR, all against the OLD integrals. *)
    let term1 = join_left_with left (lookup_r t.ir) in
    let term2 = join_right_with right (lookup_l t.il) in
    let term3 = join_left_with left (lookup_r dr_idx) in
    let delta = S.Out.add (S.Out.add term1 term2) term3 in
    t.il <- merge_left t.il (index_left left);
    t.ir <- merge_right t.ir dr_idx;
    t.out <- S.Out.add t.out delta;
    delta
  ;;

  let output t = t.out
end
