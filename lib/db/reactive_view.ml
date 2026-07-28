module Row = Granary_encoding.Row
module Ast = Granary_sql.Ast

type agg =
  | Count
  | Sum_col of string

type kind =
  | Full
  | Delta of
      { group_col : string
      ; agg : agg
      }

type classified =
  { base_tables : string list
  ; out_cols : string list option
  ; kind : kind
  }

(* ------------------------------------------------------------------ *)
(* Value / row ordering                                                 *)
(* ------------------------------------------------------------------ *)

let value_rank = function
  | Row.V_null -> 0
  | Row.V_int _ -> 1
  | Row.V_real _ -> 2
  | Row.V_text _ -> 3
  | Row.V_blob _ -> 4
;;

let value_compare (a : Row.value) (b : Row.value) =
  match a, b with
  | Row.V_null, Row.V_null -> 0
  | Row.V_int x, Row.V_int y -> Int64.compare x y
  | Row.V_real x, Row.V_real y -> Float.compare x y
  | Row.V_text x, Row.V_text y -> String.compare x y
  | Row.V_blob x, Row.V_blob y -> Bytes.compare x y
  | _ -> Int.compare (value_rank a) (value_rank b)
;;

let row_compare (a : Row.value array) (b : Row.value array) =
  let la = Array.length a
  and lb = Array.length b in
  let n = if la < lb then la else lb in
  let rec go i =
    if i = n
    then Int.compare la lb
    else (
      let c = value_compare a.(i) b.(i) in
      if c <> 0 then c else go (i + 1))
  in
  go 0
;;

(* ------------------------------------------------------------------ *)
(* Classification                                                       *)
(* ------------------------------------------------------------------ *)

(* Name a projected expression the way an output column would be named:
   an explicit alias wins, else a bare column keeps its name, else [None]. *)
let expr_out_name expr alias =
  match alias with
  | Some a -> Some a
  | None ->
    (match (expr : Ast.expr) with
     | Ast.E_col c -> Some c
     | Ast.E_tbl_col (_, c) -> Some c
     | Ast.E_agg (Ast.Agg_count, _) -> Some "count"
     | Ast.E_agg (Ast.Agg_sum, _) -> Some "sum"
     | _ -> None)
;;

let out_cols_of_proj proj =
  match
    (proj : [ `All | `Cols of string list | `Exprs of (Ast.expr * string option) list ])
  with
  | `All -> None
  | `Cols cols -> Some cols
  | `Exprs items ->
    (* An explicit projection always yields a fixed arity; name each column by
       its alias / bare-column name, falling back to a positional name. *)
    Some
      (List.mapi
         (fun i (e, a) ->
            match expr_out_name e a with
            | Some n -> n
            | None -> Printf.sprintf "col%d" (i + 1))
         items)
;;

(* A single-column GROUP BY with a projection of exactly the group column plus
   one COUNT( * )/SUM(col) aggregate, no WHERE/HAVING/JOIN/DISTINCT/ORDER/LIMIT,
   is delta-maintainable. *)
let classify_delta (sel : Ast.stmt) =
  match sel with
  | Ast.S_select
      { distinct = false
      ; proj = `Exprs [ (E_col g1, _); (E_agg (afn, aarg), _) ]
      ; joins = []
      ; where = None
      ; group_by = [ (g2, _) ]
      ; having = None
      ; order = []
      ; limit = None
      ; offset = None
      ; _
      }
    when String.equal g1 g2 ->
    (match afn, aarg with
     | Ast.Agg_count, None -> Some (Delta { group_col = g1; agg = Count })
     | Ast.Agg_count, Some (E_col _) ->
       (* COUNT(col) counts non-null col; that differs from COUNT of all rows,
          so fall back to full to stay exact. *)
       None
     | Ast.Agg_sum, Some (E_col c) -> Some (Delta { group_col = g1; agg = Sum_col c })
     | _ -> None)
  | _ -> None
;;

let base_tables_of (sel : Ast.stmt) =
  match sel with
  | Ast.S_select { table; joins; _ } ->
    let joined = List.filter_map (fun (j : Ast.join_clause) -> Some j.table) joins in
    List.sort_uniq String.compare (table :: joined)
  | _ -> []
;;

let proj_of (sel : Ast.stmt) =
  match sel with
  | Ast.S_select { proj; _ } -> Some proj
  | _ -> None
;;

let classify (sel : Ast.stmt) =
  let base_tables = base_tables_of sel in
  let out_cols =
    match proj_of sel with
    | Some proj -> out_cols_of_proj proj
    | None -> None
  in
  let kind =
    match classify_delta sel with
    | Some k -> k
    | None -> Full
  in
  { base_tables; out_cols; kind }
;;

let is_delta_maintainable sel =
  match classify_delta sel with
  | Some _ -> true
  | None -> false
;;

(* ------------------------------------------------------------------ *)
(* Generic incremental aggregate                                        *)
(* ------------------------------------------------------------------ *)

module Agg_engine = struct
  type input =
    { key : Row.value array
    ; meas : int
    }

  type change =
    | Ins of input
    | Del of input
    | Upd of input * input

  (* Input Z-set element: a group key plus the integer measure the row adds.
     Two rows with the same key and measure are the same element (their weights
     add) — correct for both COUNT (measure 1) and SUM. *)
  module Elt = struct
    type t = input

    let compare a b =
      match row_compare a.key b.key with
      | 0 -> Int.compare a.meas b.meas
      | c -> c
    ;;

    let pp fmt _ = Format.fprintf fmt "<rv_input>"
  end

  (* Output Z-set element: a materialized output row (group key ++ aggregate). *)
  module Out_elt = struct
    type t = Row.value array

    let compare = row_compare
    let pp fmt _ = Format.fprintf fmt "<rv_output>"
  end

  module ZIn = Granary_ivm.Zset.Make (Elt)
  module ZOut = Granary_ivm.Zset.Make (Out_elt)
  module DIn = Granary_ivm.Delta.Make (ZIn)

  module Agg = Granary_ivm.Aggregate.Make (struct
      module In = ZIn
      module Out = ZOut

      type group = Row.value array

      let compare_group = row_compare
      let group_of (e : input) = e.key
      let measure (e : input) = e.meas
      let result g total = Array.append g [| Row.V_int (Int64.of_int total) |]
    end)

  type state = Agg.t

  let create () = Agg.create ()

  let event_of_change = function
    | Ins i -> Granary_ivm.Delta.Insert i
    | Del i -> Granary_ivm.Delta.Delete i
    | Upd (o, n) -> Granary_ivm.Delta.Update (o, n)
  ;;

  let step st changes =
    let delta_in = DIn.of_events (List.map event_of_change changes) in
    let out_delta = Agg.step st delta_in in
    ZOut.to_list out_delta
  ;;

  let snapshot st = Agg.output st |> ZOut.to_list |> List.map fst
end
