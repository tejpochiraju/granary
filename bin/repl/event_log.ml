module Event = Sqlocaml.Db.Event

type t =
  { capacity : int
  ; q : Event.t Queue.t
  ; mutable filter : int64 option
  ; mutable paused : bool
  ; state : unit Lwd.var
  }

let create ~capacity =
  { capacity; q = Queue.create (); filter = None; paused = false; state = Lwd.var () }
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
  | None -> all
  | Some id -> List.filter (fun ev -> Event.txn_id ev = Some id) all
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

let state_var t = t.state

let pp fmt t =
  Format.fprintf
    fmt
    "Event_log{len=%d paused=%b filter=%s}"
    (Queue.length t.q)
    t.paused
    (match t.filter with
     | None -> "-"
     | Some id -> Int64.to_string id)
;;
