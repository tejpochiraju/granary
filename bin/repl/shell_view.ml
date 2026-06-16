module W = Nottui_widgets
module Ui = Nottui.Ui

type t =
  { input : string Lwd.var
  ; status : string Lwd.var
  ; headers : string list Lwd.var
  ; rows : Sqlocaml.Db.row list Lwd.var
  }

let create () =
  { input = Lwd.var ""; status = Lwd.var ""; headers = Lwd.var []; rows = Lwd.var [] }
;;

let input_var t = t.input
let set_status t s = Lwd.set t.status s

let set_result t ~headers ~rows =
  Lwd.set t.headers headers;
  Lwd.set t.rows rows
;;

let render_rows headers rows =
  let widths = Repl_engine.column_widths rows in
  let render_row vals =
    let cells =
      Array.to_list
        (Array.mapi
           (fun i v ->
              let s = Repl_engine.value_to_string v in
              let pad =
                (if i < Array.length widths then widths.(i) else 0) - String.length s
              in
              s ^ if pad > 0 then String.make pad ' ' else "")
           vals)
    in
    W.string (String.concat " | " cells)
  in
  let header_row =
    if headers = []
    then []
    else [ Lwd.return (W.string ~attr:Notty.A.(st bold) (String.concat " | " headers)) ]
  in
  header_row @ List.map (fun r -> Lwd.return (render_row r)) rows
;;

let render t =
  Lwd.bind (Lwd.get t.input) ~f:(fun input ->
    Lwd.bind (Lwd.get t.status) ~f:(fun status ->
      Lwd.bind (Lwd.get t.headers) ~f:(fun headers ->
        Lwd.bind (Lwd.get t.rows) ~f:(fun rows ->
          W.vbox
            ([ Lwd.return (W.string ~attr:Notty.A.(fg cyan) ("sqlocaml> " ^ input)) ]
             @ render_rows headers rows
             @ [ Lwd.return (W.string ~attr:Notty.A.(fg lightblack) status) ])))))
;;
