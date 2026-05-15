open Lwt.Syntax
module S         = Sqlocaml_store.Store
module Cat       = Sqlocaml_catalog.Catalog
module Row       = Sqlocaml_encoding.Row
module Rowid     = Sqlocaml_encoding.Rowid
module Index_key = Sqlocaml_encoding.Index_key

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let lit_to_value : Ast.literal -> Row.value = function
  | Ast.L_int  n -> Row.V_int n
  | Ast.L_text s -> Row.V_text s
  | Ast.L_null   -> Row.V_null
  | Ast.L_real f -> Row.V_real f
  | Ast.L_blob b -> Row.V_blob b

let row_value_to_index_value : Row.value -> Index_key.value = function
  | Row.V_int  n -> Index_key.IK_int n
  | Row.V_text s -> Index_key.IK_text s
  | Row.V_null   -> Index_key.IK_null
  | Row.V_real f -> Index_key.IK_real f
  | Row.V_blob b -> Index_key.IK_blob b

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

(** Find a column ordinal by name within a [Row.column] list. *)
let find_col_idx_by_name (cols : Row.column list) (name : string) : int =
  let rec find i = function
    | [] -> failwith (Printf.sprintf "column not found: %s" name)
    | (c : Row.column) :: _ when String.equal c.Row.name name -> i
    | _ :: rest -> find (i + 1) rest
  in
  find 0 cols

(* ------------------------------------------------------------------ *)
(* Expression evaluation                                                *)
(* (Defined before [execute] so that [Op_update] can evaluate WHERE     *)
(*  predicates and right-hand-side expressions for SET assignments.)    *)
(* ------------------------------------------------------------------ *)

let value_truthy : Row.value -> bool = function
  | Row.V_null | Row.V_int 0L -> false
  | _                          -> true

let rec eval_expr (row : Row.t) (e : Plan.expr) : Row.value =
  match e with
  | Plan.P_lit l            -> lit_to_value l
  | Plan.P_col i            -> row.(i)
  | Plan.P_neg e ->
    (match eval_expr row e with
     | Row.V_int  n -> Row.V_int  (Int64.neg n)
     | Row.V_real f -> Row.V_real (-. f)
     | Row.V_null   -> Row.V_null
     | _            -> failwith "unary minus requires numeric operand")
  | Plan.P_is_null e ->
    (match eval_expr row e with
     | Row.V_null -> Row.V_int 1L
     | _          -> Row.V_int 0L)
  | Plan.P_is_not_null e ->
    (match eval_expr row e with
     | Row.V_null -> Row.V_int 0L
     | _          -> Row.V_int 1L)
  | Plan.P_not e ->
    if value_truthy (eval_expr row e) then Row.V_int 0L else Row.V_int 1L
  | Plan.P_binop (op, a, b) ->
    eval_binop op (eval_expr row a) (eval_expr row b)

and eval_binop (op : Plan.binop) (lv : Row.value) (rv : Row.value) : Row.value =
  match op with
  (* Phase 2 simplification: two-valued logic — NULL is falsy, not unknown (diverges from SQL 3VL). *)
  | Plan.And ->
    if value_truthy lv && value_truthy rv then Row.V_int 1L else Row.V_int 0L
  | Plan.Or ->
    if value_truthy lv || value_truthy rv then Row.V_int 1L else Row.V_int 0L
  (* Cross-type comparisons (e.g. V_int vs V_real) return false — no implicit coercion is performed. *)
  | Plan.Eq ->
    (* NaN != NaN is intentional SQL semantics (IEEE 754). *)
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_int 0L
     | Row.V_int  x, Row.V_int  y -> if Int64.equal x y then Row.V_int 1L else Row.V_int 0L
     | Row.V_text x, Row.V_text y -> if String.equal x y then Row.V_int 1L else Row.V_int 0L
     | Row.V_real x, Row.V_real y -> if Float.equal  x y then Row.V_int 1L else Row.V_int 0L
     | Row.V_blob x, Row.V_blob y -> if Bytes.equal  x y then Row.V_int 1L else Row.V_int 0L
     | _                          -> Row.V_int 0L)
  | Plan.Ne ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_int 0L
     | Row.V_int  x, Row.V_int  y -> if Int64.equal x y then Row.V_int 0L else Row.V_int 1L
     | Row.V_text x, Row.V_text y -> if String.equal x y then Row.V_int 0L else Row.V_int 1L
     | Row.V_real x, Row.V_real y -> if Float.equal  x y then Row.V_int 0L else Row.V_int 1L
     | Row.V_blob x, Row.V_blob y -> if Bytes.equal  x y then Row.V_int 0L else Row.V_int 1L
     | _                          -> Row.V_int 0L)
  | Plan.Lt -> cmp_result lv rv (fun c -> c <  0)
  | Plan.Le -> cmp_result lv rv (fun c -> c <= 0)
  | Plan.Gt -> cmp_result lv rv (fun c -> c >  0)
  | Plan.Ge -> cmp_result lv rv (fun c -> c >= 0)
  | Plan.Add -> arith_op lv rv Int64.add ( +. )
  | Plan.Sub -> arith_op lv rv Int64.sub ( -. )
  | Plan.Mul -> arith_op lv rv Int64.mul ( *. )
  | Plan.Div ->
    arith_op lv rv
      (fun a b -> if Int64.equal b 0L then failwith "division by zero" else Int64.div a b)
      ( /. )

and cmp_result lv rv pred =
  match lv, rv with
  | Row.V_null, _ | _, Row.V_null -> Row.V_int 0L
  | Row.V_int _,  Row.V_int _
  | Row.V_text _, Row.V_text _
  | Row.V_real _, Row.V_real _
  | Row.V_blob _, Row.V_blob _ ->
    if pred (compare_values lv rv) then Row.V_int 1L else Row.V_int 0L
  | _ -> Row.V_int 0L  (* cross-type comparisons are false *)

and arith_op lv rv int_f float_f =
  match lv, rv with
  | Row.V_null, _ | _, Row.V_null -> Row.V_null
  | Row.V_int  a, Row.V_int  b -> Row.V_int  (int_f a b)
  | Row.V_real a, Row.V_real b -> Row.V_real (float_f a b)
  | Row.V_int  a, Row.V_real b -> Row.V_real (float_f (Int64.to_float a) b)
  | Row.V_real a, Row.V_int  b -> Row.V_real (float_f a (Int64.to_float b))
  | _ -> failwith "arithmetic on non-numeric operands"

let project_row (ords : int list) (row : Row.t) : Row.t =
  Array.of_list (List.map (fun i -> row.(i)) ords)

(* ------------------------------------------------------------------ *)
(* execute: write operations only                                       *)
(* ------------------------------------------------------------------ *)

(** Run [Op_insert] against the store: write the new row to the table
    tree and, if any indexes are defined on the table, also write the
    corresponding index entries (checking UNIQUE constraints first). *)
let execute_insert (store : S.t) (cat : Cat.t)
    ~(table_meta : Cat.table_meta) ~ordinals ~values : unit Lwt.t =
  let n   = List.length table_meta.columns in
  let row = Array.make n Row.V_null in
  List.iter2 (fun ord v -> row.(ord) <- lit_to_value v) ordinals values;
  let* rowid = Cat.next_rowid cat ~name:table_meta.name in
  let key    = Rowid.encode rowid in
  let bytes  = Row.encode table_meta.columns row in
  let* tx    = S.rw_begin store in
  let* ()    = S.put tx table_meta.tree_id key bytes in
  let* ()    = S.commit tx in
  (* If any indexes exist on this table, write index entries too. *)
  let idxs = Cat.indexes_for_table cat ~table:table_meta.name in
  (match idxs with
   | [] -> Lwt.return_unit
   | _  ->
     let* tx = S.rw_begin store in
     (* Check UNIQUE constraints before writing any index entry.
        Open RO cursors on each unique index to test for duplicates. *)
     let* unique_ok =
       Lwt_list.fold_left_s (fun acc (idx : Cat.index_info) ->
         if not acc || not idx.idx_unique then Lwt.return acc
         else begin
           let col_idx =
             find_col_idx_by_name table_meta.columns idx.idx_column
           in
           let v = row.(col_idx) in
           let ik_value = row_value_to_index_value v in
           let prefix = Index_key.encode_value ik_value in
           let plen = Bytes.length prefix in
           (* Seek to the smallest key >= prefix ++ min_rowid. *)
           let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
           let* cur = S.cursor_open tx idx.idx_tree_id in
           let _sr = S.cursor_seek cur seek_key in
           let duplicate =
             match S.cursor_next cur with
             | None -> false
             | Some (ikey, _) ->
               Bytes.length ikey >= plen &&
               Bytes.equal (Bytes.sub ikey 0 plen) prefix
           in
           S.cursor_close cur;
           if duplicate then
             Lwt.fail_with (Printf.sprintf
               "UNIQUE constraint violated: duplicate value in column '%s'"
               idx.idx_column)
           else
             Lwt.return true
         end
       ) true idxs
     in
     ignore unique_ok;
     let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
       let col_idx =
         find_col_idx_by_name table_meta.columns idx.idx_column
       in
       let v = row.(col_idx) in
       let ikey = Index_key.encode [row_value_to_index_value v] ~rowid in
       S.put tx idx.idx_tree_id ikey Bytes.empty
     ) idxs in
     S.commit tx)

(** Run [Op_create_index]: register the index in the catalog, then scan
    the table tree and populate the index tree with one entry per row. *)
let execute_create_index (store : S.t) (cat : Cat.t)
    ~name ~table ~tree_id ~col_idx ~unique
    ~(columns : Row.column list) : unit Lwt.t =
  let* res = Cat.create_index cat ~name ~table
               ~column:(List.nth columns col_idx).Row.name ~unique in
  match res with
  | Error msg -> failwith msg
  | Ok info ->
    let* tx = S.rw_begin store in
    let* cur = S.cursor_open tx tree_id in
    let _sr = S.cursor_first cur in
    let rec walk () =
      match S.cursor_next cur with
      | None -> Lwt.return_unit
      | Some (kbytes, vbytes) ->
        let rowid = Rowid.decode kbytes in
        let row = Row.decode columns vbytes in
        let v = row.(col_idx) in
        let ikey = Index_key.encode [row_value_to_index_value v] ~rowid in
        let* () = S.put tx info.idx_tree_id ikey Bytes.empty in
        walk ()
    in
    let* () = walk () in
    S.cursor_close cur;
    S.commit tx

(** Check whether inserting a new index entry for [new_value] with
    [rowid] into [idx] would violate a UNIQUE constraint.  Returns
    [true] if a different row already has the same indexed value. *)
let unique_violation_on_update
    (tx : S.rw S.txn)
    (idx : Cat.index_info)
    (new_value : Row.value)
    ~(rowid : int64) : bool Lwt.t =
  let ik_value = row_value_to_index_value new_value in
  let prefix   = Index_key.encode_value ik_value in
  let plen     = Bytes.length prefix in
  let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
  let* cur = S.cursor_open tx idx.idx_tree_id in
  let _sr = S.cursor_seek cur seek_key in
  (* Scan entries while the value prefix matches.  A different rowid
     with the same value is a UNIQUE violation. *)
  let rec scan () =
    match S.cursor_next cur with
    | None -> Lwt.return false
    | Some (ikey, _) ->
      if Bytes.length ikey >= plen + 8 &&
         Bytes.equal (Bytes.sub ikey 0 plen) prefix
      then begin
        let rowid_bytes = Bytes.sub ikey (Bytes.length ikey - 8) 8 in
        let other = Rowid.decode rowid_bytes in
        if Int64.equal other rowid then scan ()
        else Lwt.return true
      end else
        Lwt.return false
  in
  let* result = scan () in
  S.cursor_close cur;
  Lwt.return result

(** Run [Op_update]: drain matching rows into a list (snapshot read),
    then for each (rowid, old_row) compute the new row, update index
    entries, and overwrite the row in the table tree.  Returns the
    number of rows whose contents were modified. *)
let execute_update (store : S.t)
    ~(table_meta : Cat.table_meta)
    ~(assignments : (int * Plan.expr) list)
    ~(where : Plan.expr option)
    ~(indexes : Cat.index_info list)
  : int Lwt.t =
  let schema = table_meta.Cat.columns in
  (* Drain matching rows into a list under an RO snapshot first to
     avoid cursor invalidation when we issue puts/dels below. *)
  let* tx_ro = S.ro_begin store in
  let* cur   = S.cursor_open tx_ro table_meta.tree_id in
  let _sr    = S.cursor_first cur in
  let buf    = ref [] in
  let rec drain () =
    match S.cursor_next cur with
    | None -> ()
    | Some (kbytes, vbytes) ->
      let rowid = Rowid.decode kbytes in
      let row   = Row.decode schema vbytes in
      let keep  = match where with
        | None      -> true
        | Some pred -> value_truthy (eval_expr row pred)
      in
      if keep then buf := (rowid, row) :: !buf;
      drain ()
  in
  drain ();
  S.cursor_close cur;
  let* () = S.ro_end tx_ro in
  let matches = List.rev !buf in
  let n = List.length matches in
  if n = 0 then Lwt.return 0
  else begin
    let* tx = S.rw_begin store in
    (* First pass: validate UNIQUE constraints for every target row,
       considering the FULL set of new values (each updated row may
       conflict with another updated row). *)
    let* () =
      Lwt_list.iter_s (fun (rowid, old_row) ->
        let new_row = Array.copy old_row in
        List.iter (fun (i, expr) ->
          new_row.(i) <- eval_expr old_row expr
        ) assignments;
        Lwt_list.iter_s (fun (idx : Cat.index_info) ->
          if not idx.idx_unique then Lwt.return_unit
          else begin
            let col_i = find_col_idx_by_name schema idx.idx_column in
            (* Only check if the value actually changed (otherwise the
               existing entry has the same rowid and won't conflict). *)
            let old_v = old_row.(col_i) in
            let new_v = new_row.(col_i) in
            let unchanged =
              match old_v, new_v with
              | Row.V_null, Row.V_null     -> true
              | Row.V_int  a, Row.V_int  b -> Int64.equal a b
              | Row.V_text a, Row.V_text b -> String.equal a b
              | Row.V_real a, Row.V_real b -> Float.equal a b
              | Row.V_blob a, Row.V_blob b -> Bytes.equal a b
              | _                           -> false
            in
            if unchanged then Lwt.return_unit
            else
              let* dup = unique_violation_on_update tx idx new_v ~rowid in
              if dup then
                Lwt.fail_with (Printf.sprintf
                  "UNIQUE constraint violated: duplicate value in column '%s'"
                  idx.idx_column)
              else Lwt.return_unit
          end
        ) indexes
      ) matches
    in
    (* Second pass: actually apply the updates. *)
    let* () =
      Lwt_list.iter_s (fun (rowid, old_row) ->
        let new_row = Array.copy old_row in
        List.iter (fun (i, expr) ->
          new_row.(i) <- eval_expr old_row expr
        ) assignments;
        let key = Rowid.encode rowid in
        (* Update index entries: delete old, insert new. *)
        let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
          let col_i = find_col_idx_by_name schema idx.idx_column in
          let old_v = old_row.(col_i) in
          let new_v = new_row.(col_i) in
          let old_ikey =
            Index_key.encode [row_value_to_index_value old_v] ~rowid
          in
          let new_ikey =
            Index_key.encode [row_value_to_index_value new_v] ~rowid
          in
          let* () = S.del tx idx.idx_tree_id old_ikey in
          S.put tx idx.idx_tree_id new_ikey Bytes.empty
        ) indexes in
        (* Update the row in the table tree.  We could just S.put on
           the same key (overwriting), but the task spec asks for an
           explicit del+put to mirror the index-update pattern. *)
        let new_bytes = Row.encode schema new_row in
        let* () = S.del tx table_meta.tree_id key in
        S.put tx table_meta.tree_id key new_bytes
      ) matches
    in
    let* () = S.commit tx in
    Lwt.return n
  end

(** Run [Op_delete]: drain matching rows into a list (snapshot read),
    then for each matching (rowid, row) remove index entries and the
    row itself from the table tree.  Returns the number of rows deleted. *)
let execute_delete (store : S.t)
    ~(table_meta : Cat.table_meta)
    ~(where : Plan.expr option)
    ~(indexes : Cat.index_info list)
  : int Lwt.t =
  let schema = table_meta.Cat.columns in
  (* Drain matching rows under an RO snapshot. *)
  let* tx_ro = S.ro_begin store in
  let* cur   = S.cursor_open tx_ro table_meta.tree_id in
  let _sr    = S.cursor_first cur in
  let buf    = ref [] in
  let rec drain () =
    match S.cursor_next cur with
    | None -> ()
    | Some (kbytes, vbytes) ->
      let rowid = Rowid.decode kbytes in
      let row   = Row.decode schema vbytes in
      let keep  = match where with
        | None      -> true
        | Some pred -> value_truthy (eval_expr row pred)
      in
      if keep then buf := (rowid, row) :: !buf;
      drain ()
  in
  drain ();
  S.cursor_close cur;
  let* () = S.ro_end tx_ro in
  let matches = List.rev !buf in
  let n = List.length matches in
  if n = 0 then Lwt.return 0
  else begin
    let* tx = S.rw_begin store in
    let* () =
      Lwt_list.iter_s (fun (rowid, row) ->
        let rowid_key = Rowid.encode rowid in
        (* Remove index entries for this row. *)
        let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
          let col_i = find_col_idx_by_name schema idx.idx_column in
          let v = row.(col_i) in
          let old_ikey =
            Index_key.encode [row_value_to_index_value v] ~rowid
          in
          S.del tx idx.idx_tree_id old_ikey
        ) indexes in
        (* Remove the row from the table tree. *)
        S.del tx table_meta.tree_id rowid_key
      ) matches
    in
    let* () = S.commit tx in
    Lwt.return n
  end

(** [execute_with_count] returns the rows-affected count.  For most
    write ops this is 1 (INSERT) or 0 (DDL); for UPDATE it is the
    number of rows whose contents were modified. *)
let execute_with_count (store : S.t) (cat : Cat.t) (op : Plan.op)
  : int Lwt.t =
  match op with
  | Plan.Op_create_table { name; columns } ->
    let* _tid = Cat.create_table cat ~name ~columns in
    Lwt.return 0
  | Plan.Op_insert { table_meta; ordinals; values } ->
    let* () = execute_insert store cat ~table_meta ~ordinals ~values in
    Lwt.return 1
  | Plan.Op_create_index { name; table; tree_id; col_idx; unique; columns } ->
    let* () = execute_create_index store cat ~name ~table ~tree_id
                ~col_idx ~unique ~columns in
    Lwt.return 0
  | Plan.Op_update { table_meta; assignments; where; indexes } ->
    execute_update store ~table_meta ~assignments ~where ~indexes
  | Plan.Op_delete { table_meta; where; indexes } ->
    execute_delete store ~table_meta ~where ~indexes
  | Plan.Op_seq_scan _ | Plan.Op_filter _ | Plan.Op_project _
  | Plan.Op_sort _ | Plan.Op_limit _ | Plan.Op_index_lookup _ ->
    failwith "Exec.execute: use Exec.query for read operations"

(** Compatibility entry point: discards the rows-affected count. *)
let execute (store : S.t) (cat : Cat.t) (op : Plan.op) : unit Lwt.t =
  let* _n = execute_with_count store cat op in
  Lwt.return_unit

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
  | Plan.Op_index_lookup { table_tree; idx_tree; col_idx = _;
                           col_type; lookup_val; table_meta } ->
    (* Encode the lookup value as an IndexKey.value matching the column type. *)
    let lookup_v =
      let v = eval_expr [||] lookup_val in
      match v, col_type with
      | Row.V_null, _ -> Index_key.IK_null
      | Row.V_int  n, Row.Integer -> Index_key.IK_int n
      | Row.V_text s, Row.Text    -> Index_key.IK_text s
      | Row.V_real f, Row.Real    -> Index_key.IK_real f
      | Row.V_blob b, Row.Blob    -> Index_key.IK_blob b
      | _, _ ->
        (* Type mismatch: no rows can match — use null to short-circuit
           (no row will be a value-prefix match). *)
        Index_key.IK_null
    in
    let prefix = Index_key.encode_value lookup_v in
    let plen = Bytes.length prefix in
    (* Position cursor at the first entry >= prefix ++ min_rowid. *)
    let seek_key =
      let rowid_bytes = Rowid.encode Int64.min_int in
      Bytes.cat prefix rowid_bytes
    in
    let* tx = S.ro_begin store in
    let* cur = S.cursor_open tx idx_tree in
    let _sr = S.cursor_seek cur seek_key in
    let exhausted = ref false in
    let stream = Lwt_stream.from (fun () ->
      if !exhausted then Lwt.return_none
      else begin
        let rec next () =
          match S.cursor_next cur with
          | None ->
            exhausted := true;
            S.cursor_close cur;
            let%lwt () = S.ro_end tx in
            Lwt.return_none
          | Some (ikey, _ival) ->
            (* Check value-prefix match. *)
            if Bytes.length ikey >= plen + 8 &&
               Bytes.equal (Bytes.sub ikey 0 plen) prefix
            then begin
              (* Extract rowid from the last 8 bytes. *)
              let rowid_bytes = Bytes.sub ikey (Bytes.length ikey - 8) 8 in
              let rowid = Rowid.decode rowid_bytes in
              let table_key = Rowid.encode rowid in
              let%lwt vrow = S.get tx table_tree table_key in
              match vrow with
              | None ->
                (* Skip orphan index entries gracefully. *)
                next ()
              | Some vbytes ->
                let row = Row.decode table_meta.Cat.columns vbytes in
                Lwt.return_some row
            end else begin
              exhausted := true;
              S.cursor_close cur;
              let%lwt () = S.ro_end tx in
              Lwt.return_none
            end
        in
        next ()
      end
    ) in
    Lwt.return stream
  | Plan.Op_create_table _ | Plan.Op_insert _ | Plan.Op_create_index _
  | Plan.Op_update _ | Plan.Op_delete _ ->
    failwith "Exec.query: use Exec.execute for write operations"

(* ------------------------------------------------------------------ *)
(* Public query entry point                                             *)
(* ------------------------------------------------------------------ *)

let query (store : S.t) (_cat : Cat.t) (op : Plan.op) :
    Row.t Lwt_stream.t Lwt.t =
  to_stream store op
