module W = Nottui_widgets
module Ui = Nottui.Ui
module Ev = Sqlocaml.Db.Event

let attr_for ev =
  let open Notty.A in
  match Ev.label ev with
  | "COMMIT" -> fg green
  | "ROLLBACK" -> fg red
  | "CKPT_BEGIN" | "CKPT_END" | "WAL_RESET" -> fg yellow
  | _ -> empty
;;

let header log =
  let filt =
    match Event_log.filter log with
    | None -> "all txns"
    | Some id -> Printf.sprintf "txn=%Ld" id
  in
  let pause = if Event_log.paused log then "PAUSED" else "live" in
  W.string
    ~attr:Notty.A.(st bold)
    (Printf.sprintf
       "-- internals monitor [%s] [%s]  (space=pause / =filter c=clear x=clear-log) --"
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

(* Translate a key into a monitor action.  [set_filter_prompt] is supplied by
   the app so '/' can open an input line elsewhere (the shell pane). *)
let handle_key log ~set_filter_prompt (key : Ui.key) : Ui.may_handle =
  match key with
  | `ASCII ' ', _ ->
    Event_log.toggle_pause log;
    `Handled
  | `ASCII 'c', _ ->
    Event_log.set_filter log None;
    `Handled
  | `ASCII 'x', _ ->
    Event_log.clear log;
    `Handled
  | `ASCII '/', _ ->
    set_filter_prompt ();
    `Handled
  | _ -> `Unhandled
;;
