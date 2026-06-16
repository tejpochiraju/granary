(** sqlocaml interactive TUI shell (#382).

    A single-root nottui application composing two panes:
    - SHELL (top): an editable input line, query results, and a status line
      (all rendered by {!Shell_view}).
    - MONITOR (bottom): a live, bounded view of engine internals events
      (rendered by {!Monitor_view}), with pause / filter / clear actions.

    The two panes each own a focus handle; Tab toggles focus between them.
    The shell handles character entry, Backspace, Enter (submit) and Escape
    (quit).  The monitor handles space (pause), '/' (filter prompt), 'c'
    (clear filter) and 'x' (clear log).

    Dot commands are preserved from the original blocking REPL: [.help],
    [.quit] / [.exit], [.tables], [.schema [name]], [.open <path>],
    [.databases], [.dump [path]].  They now write their output to the shell views instead of
    printing to stdout. *)

open Lwt.Syntax
module Db = Sqlocaml.Db
module W = Nottui_widgets
module Ui = Nottui.Ui
module Focus = Nottui.Focus

let log = Event_log.create ~capacity:5000
let shell = Shell_view.create ()
let db_ref = ref None
let quit_t, quit_u = Lwt.wait ()
let prompt = ref Repl_command.No_prompt
let busy = ref false
let input_var = Shell_view.input_var shell

(* ------------------------------------------------------------------ *)
(* Engine event wiring                                                  *)
(* ------------------------------------------------------------------ *)

let wire_callback db = Db.set_event_callback db (Some (fun ev -> Event_log.push log ev))

let set_db db =
  db_ref := Some db;
  wire_callback db
;;

let do_quit () = if Lwt.is_sleeping quit_t then Lwt.wakeup_later quit_u ()

(* ------------------------------------------------------------------ *)
(* SQL execution                                                        *)
(* ------------------------------------------------------------------ *)

let run_sql sql =
  let s = String.trim sql in
  if s = ""
  then Lwt.return_unit
  else (
    match !db_ref with
    | None ->
      Shell_view.set_status shell "Error: no database open";
      Lwt.return_unit
    | Some db ->
      if Repl_engine.is_query_stmt s
      then (
        let* r = Db.query db s in
        match r with
        | Error e ->
          Shell_view.set_status shell (Format.asprintf "Error: %a" Db.pp_error e);
          Lwt.return_unit
        | Ok stream ->
          let* rows = Lwt_stream.to_list stream in
          Shell_view.set_result shell ~headers:[] ~rows;
          Shell_view.set_status shell (Printf.sprintf "%d row(s)" (List.length rows));
          Lwt.return_unit)
      else
        let* r = Db.execute_change_count db s in
        (match r with
         | Error e ->
           Shell_view.set_status shell (Format.asprintf "Error: %a" Db.pp_error e);
           Lwt.return_unit
         | Ok n ->
           Shell_view.set_status shell (Printf.sprintf "%d row(s) affected" n);
           Lwt.return_unit))
;;

(* ------------------------------------------------------------------ *)
(* Dot commands (preserved from the original REPL, writing to views)    *)
(* ------------------------------------------------------------------ *)

let dot_help () =
  Shell_view.set_result shell ~headers:[] ~rows:[];
  let lines =
    [ ".help                       this list"
    ; ".quit | .exit               leave the shell"
    ; ".tables                     list tables in the active schema"
    ; ".schema [name]              show CREATE statements (optionally for one table)"
    ; ".open <path>                close current db and open the given path"
    ; ".dump [path]                write the visible event log to a file (default \
       sqlocaml-events.log)"
    ; ".databases                  list attached databases"
    ; "Tab switches panes; Esc quits."
    ]
  in
  let rows = List.map (fun l -> [| Db.V_text l |]) lines in
  Shell_view.set_result shell ~headers:[ "help" ] ~rows;
  Shell_view.set_status shell "see commands above"
;;

let dot_open path =
  let* r = Repl_engine.open_db ~path in
  match r with
  | Error e ->
    Shell_view.set_status
      shell
      (Format.asprintf "Error opening '%s': %a" path Db.pp_error e);
    Lwt.return_unit
  | Ok new_db ->
    let* () =
      match !db_ref with
      | Some old -> Db.close old
      | None -> Lwt.return_unit
    in
    set_db new_db;
    Shell_view.set_result shell ~headers:[] ~rows:[];
    Shell_view.set_status shell (Printf.sprintf "Opened %s" path);
    Lwt.return_unit
;;

let dispatch_dot (d : Repl_command.dot) =
  match d with
  | Help ->
    dot_help ();
    Lwt.return_unit
  | Quit ->
    do_quit ();
    Lwt.return_unit
  | Tables -> run_sql Repl_command.tables_sql
  | Schema o -> run_sql (Repl_command.schema_sql o)
  | Databases -> run_sql Repl_command.databases_sql
  | Open path -> dot_open path
  | Dump path_opt ->
    let path = Option.value path_opt ~default:"sqlocaml-events.log" in
    (match Event_log.dump log path with
     | Ok n ->
       Shell_view.set_status shell (Printf.sprintf "dumped %d event(s) to %s" n path)
     | Error e -> Shell_view.set_status shell (Printf.sprintf "dump failed: %s" e));
    Lwt.return_unit
  | Unknown s ->
    Shell_view.set_status shell (Printf.sprintf "Unknown dot command: %s (try .help)" s);
    Lwt.return_unit
;;

(* ------------------------------------------------------------------ *)
(* Submit                                                               *)
(* ------------------------------------------------------------------ *)

let submit input =
  match Repl_command.classify ~prompt:!prompt input with
  | Empty -> Lwt.return_unit
  | Filter (Repl_command.Filter_txn idopt) ->
    prompt := Repl_command.No_prompt;
    (match idopt with
     | Some id ->
       Event_log.set_filter log (Event_log.By_txn id);
       Shell_view.set_status shell (Printf.sprintf "filter: txn=%Ld" id)
     | None ->
       Event_log.set_filter log Event_log.No_filter;
       Shell_view.set_status shell "filter: not a txn id");
    Lwt.return_unit
  | Filter (Repl_command.Filter_table name) ->
    prompt := Repl_command.No_prompt;
    (match !db_ref with
     | None -> Shell_view.set_status shell "filter: no database open"
     | Some db ->
       (match Db.tree_of_table db name with
        | Some tree ->
          Event_log.set_filter log (Event_log.By_table { name; tree });
          Shell_view.set_status shell (Printf.sprintf "filter: tbl=%s" name)
        | None ->
          Shell_view.set_status shell (Printf.sprintf "filter: no such table '%s'" name)));
    Lwt.return_unit
  | Dot d -> dispatch_dot d
  | Sql stmts -> Lwt_list.iter_s run_sql stmts
;;

(* ------------------------------------------------------------------ *)
(* Key handling                                                         *)
(* ------------------------------------------------------------------ *)

let shell_focus = Focus.make ()
let monitor_focus = Focus.make ()

let set_filter_prompt () =
  prompt := Repl_command.Txn_prompt;
  Lwd.set input_var "";
  Focus.request shell_focus;
  Shell_view.set_status shell "filter: enter a txn id, then Enter (empty=clear)"
;;

let set_table_filter_prompt () =
  prompt := Repl_command.Table_prompt;
  Lwd.set input_var "";
  Focus.request shell_focus;
  Shell_view.set_status shell "filter: enter a table name, then Enter"
;;

(* Handle a key directed at the shell input.  Returns [`Handled] for keys it
   consumes so they do not bubble to other areas. *)
let shell_handle (key : Ui.key) : Ui.may_handle =
  match key with
  | `Escape, _ ->
    if !prompt <> Repl_command.No_prompt
    then (
      prompt := Repl_command.No_prompt;
      Shell_view.set_status shell "filter: cancelled")
    else do_quit ();
    `Handled
  | `Tab, _ ->
    prompt := Repl_command.No_prompt;
    Focus.request monitor_focus;
    `Handled
  | `Enter, _ ->
    if !busy
    then Shell_view.set_status shell "busy — a query is still running"
    else (
      let input = Lwd.peek input_var in
      Lwd.set input_var "";
      busy := true;
      Lwt.async (fun () ->
        Lwt.finalize
          (fun () ->
             Lwt.catch
               (fun () -> submit input)
               (fun exn ->
                  Shell_view.set_status shell ("Error: " ^ Printexc.to_string exn);
                  Lwt.return_unit))
          (fun () ->
             busy := false;
             Lwt.return_unit)));
    `Handled
  | `Backspace, _ ->
    let s = Lwd.peek input_var in
    let n = String.length s in
    if n > 0 then Lwd.set input_var (String.sub s 0 (n - 1));
    `Handled
  | `ASCII c, _ ->
    Lwd.set input_var (Lwd.peek input_var ^ String.make 1 c);
    `Handled
  | `Uchar u, _ ->
    let b = Buffer.create 4 in
    Buffer.add_utf_8_uchar b u;
    Lwd.set input_var (Lwd.peek input_var ^ Buffer.contents b);
    `Handled
  | _ -> `Unhandled
;;

(* Monitor keys; Tab returns focus to the shell, Escape quits, everything else
   is delegated to [Monitor_view.handle_key]. *)
let monitor_handle (key : Ui.key) : Ui.may_handle =
  match key with
  | `Escape, _ ->
    do_quit ();
    `Handled
  | `Tab, _ ->
    Focus.request shell_focus;
    `Handled
  | _ -> Monitor_view.handle_key log ~set_filter_prompt ~set_table_filter_prompt key
;;

(* ------------------------------------------------------------------ *)
(* Root UI                                                              *)
(* ------------------------------------------------------------------ *)

let root =
  let shell_ui =
    Lwd.map2 (Focus.status shell_focus) (Shell_view.render shell) ~f:(fun focus ui ->
      Ui.keyboard_area ~focus shell_handle ui)
  in
  let monitor_ui =
    Lwd.map2 (Focus.status monitor_focus) (Monitor_view.render log) ~f:(fun focus ui ->
      Ui.keyboard_area ~focus monitor_handle ui)
  in
  W.v_pane shell_ui monitor_ui
;;

(* ------------------------------------------------------------------ *)
(* Entry point                                                          *)
(* ------------------------------------------------------------------ *)

let main () =
  Sqlocaml_unix.install ();
  let path =
    match Array.to_list Sys.argv |> List.tl with
    | [] -> ":memory:"
    | path :: _ -> path
  in
  let* r = Repl_engine.open_db ~path in
  match r with
  | Error e ->
    Format.eprintf "Cannot open '%s': %a\n%!" path Db.pp_error e;
    exit 1
  | Ok db ->
    set_db db;
    Focus.request shell_focus;
    if path = ":memory:"
    then
      Shell_view.set_status
        shell
        "Open: :memory: — in-memory; .open a file db to see monitor events"
    else Shell_view.set_status shell (Printf.sprintf "Open: %s" path);
    let* () = Nottui_lwt.run ~quit:quit_t root in
    (match !db_ref with
     | Some db -> Db.close db
     | None -> Lwt.return_unit)
;;

let () = Lwt_main.run (main ())
