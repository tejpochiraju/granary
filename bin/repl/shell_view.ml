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

let pad s w =
  let p = w - String.length s in
  s ^ if p > 0 then String.make p ' ' else ""
;;

(* Per-column widths that account for BOTH the widest data value and the header
   label, so header and data cells in the same column share one width. Exposed
   (see .mli) for testing. *)
let column_widths_with_headers ~headers ~rows =
  let data_widths = Repl_engine.column_widths rows in
  let header_arr = Array.of_list headers in
  let n_cols = max (Array.length data_widths) (Array.length header_arr) in
  Array.init n_cols (fun i ->
    let dw = if i < Array.length data_widths then data_widths.(i) else 0 in
    let hw = if i < Array.length header_arr then String.length header_arr.(i) else 0 in
    max dw hw)
;;

let render_rows headers rows =
  let widths = column_widths_with_headers ~headers ~rows in
  let width_at i = if i < Array.length widths then widths.(i) else 0 in
  let render_row vals =
    let cells =
      Array.to_list
        (Array.mapi (fun i v -> pad (Repl_engine.value_to_string v) (width_at i)) vals)
    in
    W.string (String.concat " | " cells)
  in
  let header_row =
    if headers = []
    then []
    else (
      let cells = List.mapi (fun i h -> pad h (width_at i)) headers in
      [ Lwd.return (W.string ~attr:Notty.A.(st bold) (String.concat " | " cells)) ])
  in
  header_row @ List.map (fun r -> Lwd.return (render_row r)) rows
;;

let pp fmt t =
  Format.fprintf
    fmt
    "Shell_view{input=%S status=%S}"
    (Lwd.peek t.input)
    (Lwd.peek t.status)
;;

let render t =
  let pair a b = Lwd.map2 a b ~f:(fun x y -> x, y) in
  Lwd.bind
    (pair (Lwd.get t.input) (Lwd.get t.status))
    ~f:(fun (input, status) ->
      Lwd.bind
        (pair (Lwd.get t.headers) (Lwd.get t.rows))
        ~f:(fun (headers, rows) ->
          W.vbox
            ([ Lwd.return (W.string ~attr:Notty.A.(fg cyan) ("sqlocaml> " ^ input)) ]
             @ render_rows headers rows
             @ [ Lwd.return (W.string ~attr:Notty.A.(fg lightblack) status) ])))
;;
