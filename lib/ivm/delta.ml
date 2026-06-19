type 'elt event =
  | Insert of 'elt
  | Delete of 'elt
  | Update of 'elt * 'elt

module Make (Z : Zset.S) = struct
  let of_event = function
    | Insert e -> Z.singleton e 1
    | Delete e -> Z.singleton e (-1)
    | Update (old_e, new_e) -> Z.add (Z.singleton old_e (-1)) (Z.singleton new_e 1)
  ;;

  let of_events es = List.fold_left (fun acc e -> Z.add acc (of_event e)) Z.zero es
end
