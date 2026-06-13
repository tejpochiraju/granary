module Row = Sqlocaml_encoding.Row

type t =
  { schema : Row.column list
  ; mutable rows : Row.t array list
  ; mutable total : int
  }

let create schema = { schema; rows = []; total = 0 }
let nrows t = t.total
let columns t = t.schema

let insert_rows t batch =
  if Array.length batch > 0
  then (
    t.rows <- batch :: t.rows;
    t.total <- t.total + Array.length batch)
;;

let to_row_seq t =
  let all = List.rev t.rows in
  let batches = Array.of_list all in
  let bi = ref 0
  and ri = ref 0 in
  let rec next () =
    if !bi >= Array.length batches
    then Seq.Nil
    else (
      let batch = batches.(!bi) in
      if !ri >= Array.length batch
      then (
        incr bi;
        ri := 0;
        next ())
      else (
        let row = batch.(!ri) in
        incr ri;
        Seq.Cons (row, next)))
  in
  next
;;
