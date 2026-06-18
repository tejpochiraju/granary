type record =
  { txn_id : int64
  ; timestamp : int64
  ; root_page : int64
  }

type target =
  [ `Txn of int64
  | `Ts of int64
  ]

type sink =
  { append : record -> unit Lwt.t
  ; load : unit -> record list Lwt.t
  }

let record_size = 28
let payload_size = 24

(* IEEE CRC32, computed over the whole given slice (nothing zeroed). Kept
   self-contained so the log codec owns its format end-to-end. *)
let crc32_table =
  Array.init 256 (fun i ->
    let crc = ref i in
    for _ = 0 to 7 do
      crc := if !crc land 1 = 1 then 0xEDB88320 lxor (!crc lsr 1) else !crc lsr 1
    done;
    !crc)
;;

let crc32 (buf : Cstruct.t) : int32 =
  let crc = ref 0xFFFFFFFF in
  for i = 0 to Cstruct.length buf - 1 do
    let byte = Char.code (Cstruct.get_char buf i) in
    crc := crc32_table.(!crc lxor byte land 0xFF) lxor (!crc lsr 8)
  done;
  Int32.of_int (!crc lxor 0xFFFFFFFF)
;;

let encode (r : record) : Cstruct.t =
  let buf = Cstruct.create record_size in
  Cstruct.BE.set_uint64 buf 0 r.txn_id;
  Cstruct.BE.set_uint64 buf 8 r.timestamp;
  Cstruct.BE.set_uint64 buf 16 r.root_page;
  Cstruct.BE.set_uint32 buf payload_size (crc32 (Cstruct.sub buf 0 payload_size));
  buf
;;

let decode (buf : Cstruct.t) : record option =
  if Cstruct.length buf < record_size
  then None
  else (
    let stored = Cstruct.BE.get_uint32 buf payload_size in
    if Int32.equal stored (crc32 (Cstruct.sub buf 0 payload_size))
    then
      Some
        { txn_id = Cstruct.BE.get_uint64 buf 0
        ; timestamp = Cstruct.BE.get_uint64 buf 8
        ; root_page = Cstruct.BE.get_uint64 buf 16
        }
    else None)
;;

let decode_all (buf : Cstruct.t) : record list =
  let n = Cstruct.length buf in
  let rec loop off acc =
    if off + record_size > n
    then List.rev acc
    else (
      match decode (Cstruct.sub buf off record_size) with
      | Some r -> loop (off + record_size) (r :: acc)
      | None -> List.rev acc)
  in
  loop 0 []
;;

let resolve (records : record list) (target : target) : record option =
  let key r =
    match target with
    | `Txn _ -> r.txn_id
    | `Ts _ -> r.timestamp
  in
  let bound =
    match target with
    | `Txn t | `Ts t -> t
  in
  (* #266 (review): return the record with the genuine MAXIMUM key ≤ bound, not
     the last one in list order. For [`Txn] (unique, sorted) this is identical to
     a left fold; for [`Ts] under a non-monotonic clock it is the fix — the
     largest-timestamp record need not be the last in txn order. Equal-max ties
     resolve to the later record in list order (the later txn). *)
  List.fold_left
    (fun acc r ->
       if Int64.compare (key r) bound <= 0
       then (
         match acc with
         | Some a when Int64.compare (key a) (key r) >= 0 -> acc
         | _ -> Some r)
       else acc)
    None
    records
;;
