module type ELEMENT = sig
  type t

  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type elt
  type t

  val zero : t
  val is_zero : t -> bool
  val singleton : elt -> int -> t
  val of_list : (elt * int) list -> t
  val to_list : t -> (elt * int) list
  val weight : t -> elt -> int
  val add : t -> t -> t
  val negate : t -> t
  val sub : t -> t -> t
  val scale : int -> t -> t
  val map : (elt -> elt) -> t -> t
  val filter : (elt -> bool) -> t -> t
  val distinct : t -> t
  val support : t -> elt list
  val cardinality : t -> int
  val total_weight : t -> int
  val iter : (elt -> int -> unit) -> t -> unit
  val fold : (elt -> int -> 'a -> 'a) -> t -> 'a -> 'a
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module Make (E : ELEMENT) = struct
  module M = Map.Make (E)

  type elt = E.t

  (* Invariant: no entry ever has weight 0 — all constructors below drop them, so
     [equal] is exact and [is_zero] is just emptiness. *)
  type t = int M.t

  let zero = M.empty
  let is_zero = M.is_empty

  (* Add [w] to [e]'s weight, removing the entry if it cancels to 0.  The single
     chokepoint that preserves the no-zero-entry invariant. *)
  let add_weight m e w =
    if w = 0
    then m
    else
      M.update
        e
        (function
          | None -> Some w
          | Some w0 ->
            let s = w0 + w in
            if s = 0 then None else Some s)
        m
  ;;

  let singleton e w = add_weight M.empty e w
  let of_list pairs = List.fold_left (fun m (e, w) -> add_weight m e w) M.empty pairs
  let to_list m = M.bindings m

  let weight m e =
    match M.find_opt e m with
    | Some w -> w
    | None -> 0
  ;;

  let add a b = M.fold (fun e w acc -> add_weight acc e w) b a
  let negate m = M.map (fun w -> -w) m
  let sub a b = add a (negate b)
  let scale n m = if n = 0 then M.empty else M.map (fun w -> n * w) m
  let map f m = M.fold (fun e w acc -> add_weight acc (f e) w) m M.empty
  let filter p m = M.filter (fun e _ -> p e) m
  let distinct m = M.fold (fun e w acc -> if w > 0 then M.add e 1 acc else acc) m M.empty
  let support m = List.map fst (M.bindings m)
  let cardinality m = M.cardinal m
  let total_weight m = M.fold (fun _ w acc -> acc + w) m 0
  let iter f m = M.iter f m
  let fold f m acc = M.fold f m acc
  let equal a b = M.equal Int.equal a b

  let pp ppf m =
    Format.fprintf ppf "{@[";
    let first = ref true in
    M.iter
      (fun e w ->
         if !first then first := false else Format.fprintf ppf ",@ ";
         Format.fprintf ppf "%a: %d" E.pp e w)
      m;
    Format.fprintf ppf "@]}"
  ;;
end
