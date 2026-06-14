module Row = Sqlocaml_encoding.Row

type t =
  { schema : Row.column list
  ; mutable columns : Col.t array
  ; mutable total : int
  }

let create schema = { schema; columns = [||]; total = 0 }
let nrows t = t.total
let columns t = t.schema

let insert_rows t batch =
  let n = Array.length batch in
  if n > 0
  then (
    let ncols = List.length t.schema in
    let new_total = t.total + n in
    let schema_arr = Array.of_list t.schema in
    let init_col i =
      let col_ty = schema_arr.(i).Row.ty in
      let col = Col.create col_ty (max n 16) in
      let col_rows = Array.init n (fun r -> batch.(r).(i)) in
      Col.append_batch col col_rows
    in
    let append_col i existing =
      let col_rows = Array.init n (fun r -> batch.(r).(i)) in
      Col.append_batch existing col_rows
    in
    let new_cols =
      if Array.length t.columns = 0
      then Array.init ncols init_col
      else Array.init ncols (fun i -> append_col i t.columns.(i))
    in
    t.columns <- new_cols;
    t.total <- new_total)
;;

let to_row_seq t =
  let n = t.total in
  if n = 0
  then Seq.empty
  else (
    let ncols = List.length t.schema in
    let ri = ref 0 in
    let rec next () =
      if !ri >= n
      then Seq.Nil
      else (
        let row = Array.init ncols (fun i -> Col.get_value t.columns.(i) !ri) in
        incr ri;
        Seq.Cons (row, next))
    in
    next)
;;
