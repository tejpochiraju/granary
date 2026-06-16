module W = Nottui_widgets
module Ui = Nottui.Ui
module Ev = Sqlocaml.Db.Event

let attr_for ev =
  let open Notty.A in
  match (ev : Ev.t) with
  | Ev.Txn_commit _ -> fg green
  | Ev.Txn_rollback _ -> fg red
  | Ev.Checkpoint_begin _ | Ev.Checkpoint_end _ | Ev.Wal_reset _ -> fg yellow
  | Ev.Txn_begin _
  | Ev.Savepoint_begin _
  | Ev.Savepoint_release _
  | Ev.Savepoint_rollback _
  | Ev.Wal_append _
  | Ev.Page_read _
  | Ev.Wal_read _
  | Ev.Page_write _
  | Ev.Page_alloc _
  | Ev.Page_free _ -> empty
;;

let header log =
  let filt =
    match Event_log.filter log with
    | Event_log.No_filter -> "all"
    | Event_log.By_txn id -> Printf.sprintf "txn=%Ld" id
    | Event_log.By_table { name; _ } -> Printf.sprintf "tbl=%s" name
  in
  let pause = if Event_log.paused log then "PAUSED" else "live" in
  W.string
    ~attr:Notty.A.(st bold)
    (Printf.sprintf
       "-- internals monitor [%s] [%s]  (space=pause /=txn t=table c=clear x=clear-log) \
        --"
       pause
       filt)
;;

let render log =
  Lwd.bind
    (Lwd.get (Event_log.state_var log))
    ~f:(fun () ->
      let evs = Event_log.visible log in
      let rows =
        if evs = []
        then
          [ Lwd.return
              (W.string "  (no events — open a file db; :memory: has no storage seams)")
          ]
        else
          List.map
            (fun ev ->
               Lwd.return (W.string ~attr:(attr_for ev) (Format.asprintf "  %a" Ev.pp ev)))
            evs
      in
      W.vbox (Lwd.return (header log) :: rows))
;;

(* Translate a key into a monitor action.  [set_filter_prompt] /
   [set_table_filter_prompt] are supplied by the app so '/' and 't' can open an
   input line elsewhere (the shell pane). *)
let handle_key log ~set_filter_prompt ~set_table_filter_prompt (key : Ui.key)
  : Ui.may_handle
  =
  match key with
  | `ASCII ' ', _ ->
    Event_log.toggle_pause log;
    `Handled
  | `ASCII 'c', _ ->
    Event_log.set_filter log Event_log.No_filter;
    `Handled
  | `ASCII 'x', _ ->
    Event_log.clear log;
    `Handled
  | `ASCII '/', _ ->
    set_filter_prompt ();
    `Handled
  | `ASCII 't', _ ->
    set_table_filter_prompt ();
    `Handled
  | _ -> `Unhandled
;;
