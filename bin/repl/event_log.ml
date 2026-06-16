module Event = Sqlocaml.Db.Event

type filter =
  | No_filter
  | By_txn of int64
  | By_table of
      { name : string
      ; tree : int
      }

type t =
  { capacity : int
  ; q : Event.t Queue.t
  ; mutable filter : filter
  ; mutable paused : bool
  ; state : unit Lwd.var
  }

let create ~capacity =
  { capacity
  ; q = Queue.create ()
  ; filter = No_filter
  ; paused = false
  ; state = Lwd.var ()
  }
;;

let bump t = Lwd.set t.state ()

let push t ev =
  Queue.push ev t.q;
  while Queue.length t.q > t.capacity do
    ignore (Queue.pop t.q)
  done;
  bump t
;;

let length t = Queue.length t.q

let visible t =
  let all = List.of_seq (Queue.to_seq t.q) in
  match t.filter with
  | No_filter -> all
  | By_txn id -> List.filter (fun ev -> Event.txn_id ev = Some id) all
  | By_table { tree; _ } -> List.filter (fun ev -> Event.tree_id_of ev = Some tree) all
;;

let set_filter t f =
  t.filter <- f;
  bump t
;;

let filter t = t.filter

let toggle_pause t =
  t.paused <- not t.paused;
  bump t
;;

let paused t = t.paused

let clear t =
  Queue.clear t.q;
  bump t
;;

let dump t path =
  let evs = visible t in
  try
    let oc = open_out path in
    Fun.protect
      ~finally:(fun () -> close_out oc)
      (fun () ->
         List.iter
           (fun ev -> Printf.fprintf oc "%s\n" (Format.asprintf "%a" Event.pp ev))
           evs);
    Ok (List.length evs)
  with
  | Sys_error msg -> Error msg
;;

let state_var t = t.state

let pp fmt t =
  Format.fprintf
    fmt
    "Event_log{len=%d paused=%b filter=%s}"
    (Queue.length t.q)
    t.paused
    (match t.filter with
     | No_filter -> "-"
     | By_txn id -> Printf.sprintf "txn=%Ld" id
     | By_table { name; _ } -> Printf.sprintf "tbl=%s" name)
;;
