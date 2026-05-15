open Lwt.Syntax
module S     = Sqlocaml_store.Store
module Cat   = Sqlocaml_catalog.Catalog
module Row   = Sqlocaml_encoding.Row
module Rowid = Sqlocaml_encoding.Rowid

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let lit_to_value : Ast.literal -> Row.value = function
  | Ast.L_int  n -> Row.V_int n
  | Ast.L_text s -> Row.V_text s
  | Ast.L_null   -> Row.V_null
  | Ast.L_real f -> Row.V_real f
  | Ast.L_blob b -> Row.V_blob b

let compare_values (a : Row.value) (b : Row.value) : int =
  match a, b with
  | Row.V_null, Row.V_null -> 0
  | Row.V_null, _          -> 1   (* NULLs sort last *)
  | _, Row.V_null          -> -1
  | Row.V_int  x, Row.V_int  y -> Int64.compare x y
  | Row.V_real x, Row.V_real y -> Float.compare x y
  | Row.V_text x, Row.V_text y -> String.compare x y
  | Row.V_blob x, Row.V_blob y -> Bytes.compare x y
  | _,            _            -> 0  (* cross-type: shouldn't happen *)

(* ------------------------------------------------------------------ *)
(* execute: write operations only                                       *)
(* ------------------------------------------------------------------ *)

let execute (store : S.t) (cat : Cat.t) (op : Plan.op) : unit Lwt.t =
  match op with
  | Plan.Op_create_table { name; columns } ->
    let* _tid = Cat.create_table cat ~name ~columns in
    Lwt.return_unit
  | Plan.Op_insert { table_meta; ordinals; values } ->
    let n   = List.length table_meta.columns in
    let row = Array.make n Row.V_null in
    List.iter2 (fun ord v -> row.(ord) <- lit_to_value v) ordinals values;
    let* rowid = Cat.next_rowid cat ~name:table_meta.name in
    let key    = Rowid.encode rowid in
    let bytes  = Row.encode table_meta.columns row in
    let* tx    = S.rw_begin store in
    let* ()    = S.put tx table_meta.tree_id key bytes in
    S.commit tx
  | Plan.Op_seq_scan _ | Plan.Op_filter _ | Plan.Op_project _
  | Plan.Op_sort _ | Plan.Op_limit _ ->
    failwith "Exec.execute: use Exec.query for read operations"

(* ------------------------------------------------------------------ *)
(* Expression evaluation                                                *)
(* ------------------------------------------------------------------ *)

let rec eval_expr (row : Row.t) (e : Plan.expr) : Row.value =
  match e with
  | Plan.P_lit l     -> lit_to_value l
  | Plan.P_col i     -> row.(i)
  | Plan.P_eq (a, b) ->
    let va = eval_expr row a
    and vb = eval_expr row b in
    (* NaN != NaN is intentional SQL semantics (IEEE 754): a WHERE clause
       comparing a REAL column to NaN must not match, matching SQLite and
       standard SQL behaviour.  Row.value_equal uses bit-equality instead,
       which is appropriate only for round-trip testing (encode/decode). *)
    let eq = match va, vb with
      | Row.V_int  x, Row.V_int  y -> Int64.equal x y
      | Row.V_text x, Row.V_text y -> String.equal x y
      | Row.V_real x, Row.V_real y -> Float.equal x y  (* NaN != NaN intentional *)
      | Row.V_blob x, Row.V_blob y -> Bytes.equal x y
      | Row.V_null,   _            -> false   (* NULL != anything *)
      | _,            Row.V_null   -> false
      | _                          -> false
    in
    if eq then Row.V_int 1L else Row.V_int 0L

let value_truthy : Row.value -> bool = function
  | Row.V_null | Row.V_int 0L -> false
  | _                          -> true

let project_row (ords : int list) (row : Row.t) : Row.t =
  Array.of_list (List.map (fun i -> row.(i)) ords)

(* ------------------------------------------------------------------ *)
(* to_stream: convert a read op tree into a Row stream                  *)
(* ------------------------------------------------------------------ *)

let rec to_stream (store : S.t) (op : Plan.op) : Row.t Lwt_stream.t Lwt.t =
  match op with
  | Plan.Op_seq_scan { table_meta } ->
    let* tx  = S.ro_begin store in
    let* cur = S.cursor_open tx table_meta.tree_id in
    (* cursor_first positions the cursor; cursor_next returns the first entry
       on the first call when ready=true (per store.mli contract). *)
    let _sr = S.cursor_first cur in
    let stream = Lwt_stream.from (fun () ->
      match S.cursor_next cur with
      | None ->
        S.cursor_close cur;
        let%lwt () = S.ro_end tx in
        Lwt.return_none
      | Some (_key, vbytes) ->
        let row = Row.decode table_meta.columns vbytes in
        Lwt.return_some row
    ) in
    Lwt.return stream
  | Plan.Op_filter { pred; child } ->
    let* inner = to_stream store child in
    Lwt.return (Lwt_stream.filter (fun row -> value_truthy (eval_expr row pred)) inner)
  | Plan.Op_project { ordinals; child } ->
    let* inner = to_stream store child in
    Lwt.return (Lwt_stream.map (project_row ordinals) inner)
  | Plan.Op_sort { col_idx; dir; child } ->
    let* inner = to_stream store child in
    let* rows = Lwt_stream.to_list inner in
    let cmp a b =
      let va = a.(col_idx) and vb = b.(col_idx) in
      let c = compare_values va vb in
      if dir = `Asc then c else -c
    in
    let sorted = List.sort cmp rows in
    Lwt.return (Lwt_stream.of_list sorted)
  | Plan.Op_limit { limit; offset; child } ->
    let* inner = to_stream store child in
    let* rows = Lwt_stream.to_list inner in
    let rows' = List.filteri (fun i _ -> i >= offset && i < offset + limit) rows in
    Lwt.return (Lwt_stream.of_list rows')
  | Plan.Op_create_table _ | Plan.Op_insert _ ->
    failwith "Exec.query: use Exec.execute for write operations"

(* ------------------------------------------------------------------ *)
(* Public query entry point                                             *)
(* ------------------------------------------------------------------ *)

let query (store : S.t) (_cat : Cat.t) (op : Plan.op) :
    Row.t Lwt_stream.t Lwt.t =
  to_stream store op
