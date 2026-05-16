open Lwt.Syntax
module S         = Sqlocaml_store.Store
module Cat       = Sqlocaml_catalog.Catalog
module Row       = Sqlocaml_encoding.Row
module Rowid     = Sqlocaml_encoding.Rowid
module Index_key = Sqlocaml_encoding.Index_key
module Varint    = Sqlocaml_encoding.Varint

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

let rec eval_expr (params : Row.value array) (row : Row.t) (e : Plan.expr) : Row.value =
  match e with
  | Plan.P_lit l            -> lit_to_value l
  | Plan.P_col i            -> row.(i)
  | Plan.P_param i          ->
    if i < Array.length params then params.(i) else Row.V_null
  | Plan.P_neg e ->
    (match eval_expr params row e with
     | Row.V_int  n -> Row.V_int  (Int64.neg n)
     | Row.V_real f -> Row.V_real (-. f)
     | Row.V_null   -> Row.V_null
     | _            -> failwith "unary minus requires numeric operand")
  | Plan.P_is_null e ->
    (match eval_expr params row e with
     | Row.V_null -> Row.V_int 1L
     | _          -> Row.V_int 0L)
  | Plan.P_is_not_null e ->
    (match eval_expr params row e with
     | Row.V_null -> Row.V_int 0L
     | _          -> Row.V_int 1L)
  | Plan.P_not e ->
    if value_truthy (eval_expr params row e) then Row.V_int 0L else Row.V_int 1L
  | Plan.P_binop (op, a, b) ->
    eval_binop op (eval_expr params row a) (eval_expr params row b)
  | Plan.P_func (func, args) ->
    eval_func func (List.map (eval_expr params row) args)

and eval_func (func : Ast.scalar_func) (args : Row.value list) : Row.value =
  match func, args with
  | Ast.Fn_length, [Row.V_text s] -> Row.V_int (Int64.of_int (String.length s))
  | Ast.Fn_length, [Row.V_blob b] -> Row.V_int (Int64.of_int (Bytes.length b))
  | Ast.Fn_length, [Row.V_null]   -> Row.V_null
  | Ast.Fn_length, [_]            -> Row.V_null  (* non-text/blob: return null like SQLite *)
  | Ast.Fn_lower,  [Row.V_text s] -> Row.V_text (String.lowercase_ascii s)
  | Ast.Fn_lower,  [Row.V_null]   -> Row.V_null
  | Ast.Fn_lower,  [_]            -> Row.V_null
  | Ast.Fn_upper,  [Row.V_text s] -> Row.V_text (String.uppercase_ascii s)
  | Ast.Fn_upper,  [Row.V_null]   -> Row.V_null
  | Ast.Fn_upper,  [_]            -> Row.V_null
  | Ast.Fn_abs,    [Row.V_int  n] -> Row.V_int  (Int64.abs n)
  | Ast.Fn_abs,    [Row.V_real f] -> Row.V_real (Float.abs f)
  | Ast.Fn_abs,    [Row.V_null]   -> Row.V_null
  | Ast.Fn_abs,    [_]            -> Row.V_null
  | Ast.Fn_coalesce, vs           ->
    (match List.find_opt (fun v -> v <> Row.V_null) vs with
     | Some v -> v | None -> Row.V_null)
  | Ast.Fn_ifnull, [a; b]         -> (match a with Row.V_null -> b | v -> v)
  | _ ->
    failwith (Printf.sprintf "scalar_func: unexpected argument count (arity check should have caught this)")

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
(* FTS inverted-index helpers                                           *)
(* ------------------------------------------------------------------ *)

(** Key format: term_bytes ++ "\x00" ++ rowid_be8
    Rowid stored with sign bit flipped so unsigned byte order = signed int64 order. *)
let fts_term_key term rowid =
  let rb = Bytes.create 8 in
  let v  = Int64.logxor rowid Int64.min_int in
  for i = 0 to 7 do
    Bytes.set_uint8 rb i
      (Int64.to_int (Int64.logand (Int64.shift_right_logical v ((7-i)*8)) 0xFFL))
  done;
  Bytes.concat Bytes.empty [Bytes.of_string term; Bytes.of_string "\x00"; rb]

let fts_stats_key = Bytes.of_string "\x00\x00"

let fts_doclen_key rowid =
  let rb = Bytes.create 8 in
  let v  = Int64.logxor rowid Int64.min_int in
  for i = 0 to 7 do
    Bytes.set_uint8 rb i
      (Int64.to_int (Int64.logand (Int64.shift_right_logical v ((7-i)*8)) 0xFFL))
  done;
  Bytes.cat (Bytes.of_string "\x00\x01") rb

(** Value: varint pairs (col, pos)* — all positions for one (term, rowid). *)
let encode_positions positions =
  let buf = Buffer.create (List.length positions * 2) in
  List.iter (fun (col, pos) ->
    Varint.encode_uint64 buf (Int64.of_int col);
    Varint.encode_uint64 buf (Int64.of_int pos)) positions;
  Buffer.to_bytes buf

(** FTS content row: n_cols_varint ++ (col_len_varint ++ col_bytes)* *)
let fts_encode_content (texts : string list) : bytes =
  let buf = Buffer.create 64 in
  Varint.encode_uint64 buf (Int64.of_int (List.length texts));
  List.iter (fun s ->
    let b = Bytes.of_string s in
    Varint.encode_uint64 buf (Int64.of_int (Bytes.length b));
    Buffer.add_bytes buf b) texts;
  Buffer.to_bytes buf

let decode_positions value =
  let len = Bytes.length value in
  let pos = ref 0 in
  let result = ref [] in
  while !pos < len do
    let col, off1 = Varint.decode_uint64 value !pos in
    let p, off2   = Varint.decode_uint64 value off1 in
    result := (Int64.to_int col, Int64.to_int p) :: !result;
    pos := off2
  done;
  List.rev !result

let fts_decode_content bytes =
  let n, off0 = Varint.decode_uint64 bytes 0 in
  let nc = Int64.to_int n in
  let texts = ref [] in
  let pos = ref off0 in
  for _ = 1 to nc do
    let len, off = Varint.decode_uint64 bytes !pos in
    let s = Bytes.sub_string bytes off (Int64.to_int len) in
    texts := s :: !texts;
    pos := off + Int64.to_int len
  done;
  List.rev !texts

(** Read global FTS stats from index tree: (total_docs, total_tokens). *)
let read_fts_stats tx index_tree =
  let+ bytes_opt = S.get tx index_tree fts_stats_key in
  match bytes_opt with
  | None -> (0, 0)
  | Some b ->
    let docs, off = Varint.decode_uint64 b 0 in
    let toks, _   = Varint.decode_uint64 b off in
    (Int64.to_int docs, Int64.to_int toks)

let write_fts_stats tx index_tree docs tokens =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf (Int64.of_int docs);
  Varint.encode_uint64 buf (Int64.of_int tokens);
  S.put tx index_tree fts_stats_key (Buffer.to_bytes buf)

(** Write inverted index entries for a newly inserted document. *)
let fts_index_document tx ~(fts_meta : Cat.fts_table_meta) ~rowid ~col_texts =
  let tokens = Fts_tokenizer.tokenize col_texts in
  (* Group by term *)
  let by_term : (string, (int * int) list) Hashtbl.t = Hashtbl.create 8 in
  List.iter (fun (tok : Fts_tokenizer.token) ->
    let lst = Option.value ~default:[] (Hashtbl.find_opt by_term tok.term) in
    Hashtbl.replace by_term tok.term ((tok.col, tok.pos) :: lst)) tokens;
  (* Write one entry per unique term *)
  let* () = Hashtbl.fold (fun term positions acc ->
    let* () = acc in
    let key   = fts_term_key term rowid in
    let value = encode_positions (List.rev positions) in
    S.put tx fts_meta.Cat.fts_index_tree key value) by_term (Lwt.return_unit) in
  (* Write doc length *)
  let dlen = List.length tokens in
  let dlen_buf = Buffer.create 4 in
  Varint.encode_uint64 dlen_buf (Int64.of_int dlen);
  let* () = S.put tx fts_meta.Cat.fts_index_tree (fts_doclen_key rowid)
                  (Buffer.to_bytes dlen_buf) in
  (* Update global stats *)
  let* (docs, toks) = read_fts_stats tx fts_meta.Cat.fts_index_tree in
  write_fts_stats tx fts_meta.Cat.fts_index_tree (docs + 1) (toks + dlen)

(** Remove inverted index entries for a deleted document. *)
let fts_deindex_document tx ~(fts_meta : Cat.fts_table_meta) ~rowid ~col_texts =
  let tokens = Fts_tokenizer.tokenize col_texts in
  let terms = List.sort_uniq String.compare
    (List.map (fun (t : Fts_tokenizer.token) -> t.term) tokens) in
  let* () = Lwt_list.iter_s (fun term ->
    S.del tx fts_meta.Cat.fts_index_tree (fts_term_key term rowid)) terms in
  let dlen = List.length tokens in
  let* () = S.del tx fts_meta.Cat.fts_index_tree (fts_doclen_key rowid) in
  let* (docs, toks) = read_fts_stats tx fts_meta.Cat.fts_index_tree in
  write_fts_stats tx fts_meta.Cat.fts_index_tree
    (max 0 (docs - 1)) (max 0 (toks - dlen))

(* ------------------------------------------------------------------ *)
(* FTS query execution                                                  *)
(* ------------------------------------------------------------------ *)

(** Fetch the posting list for an exact term: [(rowid, positions)] *)
let fts_posting_list tx ~index_tree term =
  (* Scan keys from term\x00 onwards (sorted order) *)
  let prefix = Bytes.cat (Bytes.of_string term) (Bytes.of_string "\x00") in
  let* cur = S.cursor_open tx index_tree in
  let _sr = S.cursor_seek cur prefix in
  let entries = ref [] in
  let rec gather () =
    match S.cursor_next cur with
    | None -> ()
    | Some (key, value) ->
      if Bytes.length key >= Bytes.length prefix &&
         Bytes.equal (Bytes.sub key 0 (Bytes.length prefix)) prefix then begin
        (* Extract rowid from last 8 bytes (sign-bit-flipped) *)
        let rowid_off = Bytes.length key - 8 in
        let v = ref 0L in
        for i = 0 to 7 do
          v := Int64.logor (Int64.shift_left !v 8)
                 (Int64.of_int (Bytes.get_uint8 key (rowid_off + i)))
        done;
        let rowid = Int64.logxor !v Int64.min_int in
        let positions = decode_positions value in
        entries := (rowid, positions) :: !entries;
        gather ()
      end
  in
  gather ();
  S.cursor_close cur;
  Lwt.return (List.rev !entries)

(** Fetch posting lists for a prefix: merge all (rowid, positions) for terms matching prefix* *)
let fts_prefix_posting_list tx ~index_tree prefix_str =
  let prefix_bytes = Bytes.of_string prefix_str in
  let plen = Bytes.length prefix_bytes in
  let* cur = S.cursor_open tx index_tree in
  let _sr = S.cursor_seek cur prefix_bytes in
  let by_rowid : (int64, (int * int) list) Hashtbl.t = Hashtbl.create 16 in
  let rec gather () =
    match S.cursor_next cur with
    | None -> ()
    | Some (key, value) ->
      (* Find the null byte separating term from rowid *)
      let null_pos = ref (-1) in
      let klen = Bytes.length key in
      let i = ref 0 in
      while !i < klen - 8 && !null_pos = -1 do
        if Bytes.get_uint8 key !i = 0 then null_pos := !i;
        incr i
      done;
      if !null_pos > 0 then begin
        let term_len = !null_pos in
        (* Check term has our prefix *)
        if term_len >= plen &&
           Bytes.equal (Bytes.sub key 0 plen) prefix_bytes then begin
          let rowid_off = !null_pos + 1 in
          if rowid_off + 8 <= klen then begin
            let v = ref 0L in
            for j = 0 to 7 do
              v := Int64.logor (Int64.shift_left !v 8)
                     (Int64.of_int (Bytes.get_uint8 key (rowid_off + j)))
            done;
            let rowid = Int64.logxor !v Int64.min_int in
            let positions = decode_positions value in
            let existing = Option.value ~default:[] (Hashtbl.find_opt by_rowid rowid) in
            Hashtbl.replace by_rowid rowid (existing @ positions);
            gather ()
          end
        end
        (* if term no longer has the prefix, stop — keys are sorted *)
      end
  in
  gather ();
  S.cursor_close cur;
  Lwt.return (Hashtbl.fold (fun rowid positions acc -> (rowid, positions) :: acc) by_rowid [])

(** Execute an FTS query, returning [(rowid, positions)] for matching documents. *)
let rec fts_execute_query tx ~index_tree query =
  match query with
  | Fts_query.FQ_term (Fts_query.FT_exact term) ->
    fts_posting_list tx ~index_tree term
  | Fts_query.FQ_term (Fts_query.FT_prefix prefix) ->
    fts_prefix_posting_list tx ~index_tree prefix
  | Fts_query.FQ_term (Fts_query.FT_phrase words) ->
    (* Phrase: all words must appear consecutively in the same column.
       For each candidate document, check that there exists a starting position p
       and column c such that word[i] occurs at (col=c, pos=p+i) for all i. *)
    (match words with
     | [] -> Lwt.return []
     | first :: rest ->
       let* first_pl = fts_posting_list tx ~index_tree first in
       let* rest_pls = Lwt_list.map_s (fts_posting_list tx ~index_tree) rest in
       (* Keep only docs present in every posting list. *)
       let intersect_ids acc pl =
         let ids = List.map fst pl in
         List.filter (fun (r, _) -> List.mem r ids) acc
       in
       let candidates = List.fold_left intersect_ids first_pl rest_pls in
       (* Build an array of per-term posting lists for position checking. *)
       let all_pls = Array.of_list (first_pl :: rest_pls) in
       let n = Array.length all_pls in
       (* Check whether doc with [rowid] contains the phrase. *)
       let phrase_matches rowid =
         (* Collect positions for each word in this doc. *)
         let term_positions = Array.map (fun pl ->
           match List.assoc_opt rowid pl with
           | None -> []
           | Some pos -> pos) all_pls in
         (* For each (col, pos) of the first word, test adjacency of the rest. *)
         List.exists (fun (c0, p0) ->
           let rec check i =
             if i >= n then true
             else List.mem (c0, p0 + i) term_positions.(i) && check (i + 1)
           in check 1) term_positions.(0)
       in
       let matched = List.filter (fun (r, _) -> phrase_matches r) candidates in
       Lwt.return matched)
  | Fts_query.FQ_and qs ->
    let positive = List.filter (function Fts_query.FQ_not _ -> false | _ -> true) qs in
    let negated  = List.filter_map (function Fts_query.FQ_not q -> Some q | _ -> None) qs in
    let* pos_results = Lwt_list.map_s (fts_execute_query tx ~index_tree) positive in
    let* neg_results = Lwt_list.map_s (fts_execute_query tx ~index_tree) negated in
    let neg_ids = List.concat_map (List.map fst) neg_results in
    let intersected = match pos_results with
      | [] -> []
      | first :: rest ->
        List.fold_left (fun acc pl ->
          let ids = List.map fst pl in
          List.filter (fun (r, _) -> List.mem r ids) acc) first rest
    in
    Lwt.return (List.filter (fun (r, _) -> not (List.mem r neg_ids)) intersected)
  | Fts_query.FQ_or qs ->
    let* results = Lwt_list.map_s (fts_execute_query tx ~index_tree) qs in
    let seen : (int64, unit) Hashtbl.t = Hashtbl.create 16 in
    let union = List.concat_map (fun pl ->
      List.filter (fun (r, _) ->
        if Hashtbl.mem seen r then false
        else begin Hashtbl.replace seen r (); true end) pl) results in
    Lwt.return union
  | Fts_query.FQ_not _ ->
    (* Standalone NOT is meaningless; returns empty set.
       NOT inside AND is handled in the FQ_and case above. *)
    Lwt.return []

(** Helper: find the first index [i] such that [pred lst[i]] holds. *)
let list_find_index pred lst =
  let rec go i = function
    | [] -> None
    | x :: _ when pred x -> Some (i, x)
    | _ :: rest -> go (i+1) rest
  in go 0 lst

(* ------------------------------------------------------------------ *)
(* Transaction mode                                                     *)
(* ------------------------------------------------------------------ *)

type txn_mode =
  | Auto        (** Each DML op starts and commits its own RW txn. *)
  | In_txn of S.rw S.txn  (** Use this txn; skip auto begin/commit. *)

let acquire_txn store mode =
  match mode with
  | Auto -> let* tx = S.rw_begin store in Lwt.return (tx, true)
  | In_txn tx -> Lwt.return (tx, false)

let release_txn tx owned =
  if owned then S.commit tx else Lwt.return_unit

(* ------------------------------------------------------------------ *)
(* execute: write operations only                                       *)
(* ------------------------------------------------------------------ *)

(** Run [Op_insert] against the store: write the new row to the table
    tree and, if any indexes are defined on the table, also write the
    corresponding index entries (checking UNIQUE constraints first).
    Uses a SINGLE RW txn for both the row write and index writes. *)
let execute_insert ?(mode = Auto) ?(params = [||]) (store : S.t) (cat : Cat.t)
    ~(table_meta : Cat.table_meta) ~ordinals ~(values : Plan.expr list) : unit Lwt.t =
  let n   = List.length table_meta.columns in
  let row = Array.make n Row.V_null in
  List.iter2 (fun ord expr -> row.(ord) <- eval_expr params [||] expr) ordinals values;
  (* When an explicit transaction is already held, we must NOT call
     Cat.next_rowid (which opens its own RW txn and deadlocks on the
     mutex).  Instead acquire/reuse the txn first, then update the
     rowid counter within that same txn. *)
  let* (tx, owned) = acquire_txn store mode in
  Lwt.catch
    (fun () ->
      let* rowid = Cat.next_rowid_in_txn cat ~name:table_meta.name tx in
      let key    = Rowid.encode rowid in
      let bytes  = Row.encode table_meta.columns row in
      let* () = S.put tx table_meta.tree_id key bytes in
      let idxs = Cat.indexes_for_table cat ~table:table_meta.name in
      (* Check UNIQUE constraints and write index entries in the same txn. *)
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
      release_txn tx owned)
    (fun exn ->
      (* On any exception: rollback if we own the txn, then re-raise. *)
      let* () = if owned then S.rollback tx else Lwt.return_unit in
      Lwt.fail exn)

(** Run [Op_create_index]: register the index in the catalog, then scan
    the table tree and populate the index tree with one entry per row. *)
let execute_create_index ?(mode = Auto) (store : S.t) (cat : Cat.t)
    ~name ~table ~tree_id ~col_idx ~unique
    ~(columns : Row.column list) : unit Lwt.t =
  let* res = Cat.create_index cat ~name ~table
               ~column:(List.nth columns col_idx).Row.name ~unique in
  match res with
  | Error msg -> failwith msg
  | Ok info ->
    let* (tx, owned) = acquire_txn store mode in
    Lwt.catch
      (fun () ->
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
        release_txn tx owned)
      (fun exn ->
        (* On any exception: rollback if we own the txn, then re-raise. *)
        let* () = if owned then S.rollback tx else Lwt.return_unit in
        Lwt.fail exn)

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
let execute_update ?(mode = Auto) ?(params = [||]) (store : S.t)
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
        | Some pred -> value_truthy (eval_expr params row pred)
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
    let* (tx, owned) = acquire_txn store mode in
    Lwt.catch
      (fun () ->
        (* First pass: validate UNIQUE constraints for every target row,
           considering the FULL set of new values (each updated row may
           conflict with another updated row). *)
        let* () =
          Lwt_list.iter_s (fun (rowid, old_row) ->
            let new_row = Array.copy old_row in
            List.iter (fun (i, expr) ->
              new_row.(i) <- eval_expr params old_row expr
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
              new_row.(i) <- eval_expr params old_row expr
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
        let* () = release_txn tx owned in
        Lwt.return n)
      (fun exn ->
        (* On any exception: rollback if we own the txn, then re-raise. *)
        let* () = if owned then S.rollback tx else Lwt.return_unit in
        Lwt.fail exn)
  end

(** Run [Op_delete]: drain matching rows into a list (snapshot read),
    then for each matching (rowid, row) remove index entries and the
    row itself from the table tree.  Returns the number of rows deleted. *)
let execute_delete ?(mode = Auto) ?(params = [||]) (store : S.t)
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
        | Some pred -> value_truthy (eval_expr params row pred)
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
    let* (tx, owned) = acquire_txn store mode in
    Lwt.catch
      (fun () ->
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
        let* () = release_txn tx owned in
        Lwt.return n)
      (fun exn ->
        (* On any exception: rollback if we own the txn, then re-raise. *)
        let* () = if owned then S.rollback tx else Lwt.return_unit in
        Lwt.fail exn)
  end

(** Run [Op_drop_table]: remove catalog entries for the table and all
    its indexes.  The B+-tree pages are NOT reclaimed in Phase 2. *)
let execute_drop_table ?(mode = Auto) (store : S.t) (cat : Cat.t)
    ~(table_meta : Cat.table_meta)
    ~(_indexes : Cat.index_info list) : unit Lwt.t =
  let* (tx, owned) = acquire_txn store mode in
  Lwt.catch
    (fun () ->
      let* () = Cat.drop_table cat tx ~name:table_meta.Cat.name in
      release_txn tx owned)
    (fun exn ->
      (* On any exception: rollback if we own the txn, then re-raise. *)
      let* () = if owned then S.rollback tx else Lwt.return_unit in
      Lwt.fail exn)

(** Run [Op_drop_index]: remove catalog entry for the index.
    The B+-tree pages are NOT reclaimed in Phase 2. *)
let execute_drop_index ?(mode = Auto) (store : S.t) (cat : Cat.t)
    ~(idx_info : Cat.index_info) : unit Lwt.t =
  let* (tx, owned) = acquire_txn store mode in
  Lwt.catch
    (fun () ->
      let* () = Cat.drop_index cat tx ~name:idx_info.Cat.idx_name in
      release_txn tx owned)
    (fun exn ->
      (* On any exception: rollback if we own the txn, then re-raise. *)
      let* () = if owned then S.rollback tx else Lwt.return_unit in
      Lwt.fail exn)

(** [execute_with_count] returns the rows-affected count.  For most
    write ops this is 1 (INSERT) or 0 (DDL); for UPDATE it is the
    number of rows whose contents were modified. *)
let execute_with_count ?(mode = Auto) ?(params = [||]) (store : S.t) (cat : Cat.t) (op : Plan.op)
  : int Lwt.t =
  match op with
  | Plan.Op_create_table { name; columns } ->
    (* Note: create_table acquires its own RW txn internally via catalog.
       This means CREATE TABLE is NOT atomic within an explicit BEGIN/COMMIT block —
       it commits immediately regardless of mode. Phase 4 work to fix. *)
    let* _tid = Cat.create_table cat ~name ~columns in
    Lwt.return 0
  | Plan.Op_insert { table_meta; ordinals; values } ->
    let* () = execute_insert ~mode ~params store cat ~table_meta ~ordinals ~values in
    Lwt.return 1
  | Plan.Op_create_index { name; table; tree_id; col_idx; unique; columns } ->
    (* Note: create_index calls catalog functions that acquire their own RW txn.
       Like CREATE TABLE, CREATE INDEX is NOT atomic within an explicit BEGIN/COMMIT
       block — it commits immediately. Phase 4 work to fix. *)
    let* () = execute_create_index ~mode store cat ~name ~table ~tree_id
                ~col_idx ~unique ~columns in
    Lwt.return 0
  | Plan.Op_update { table_meta; assignments; where; indexes } ->
    execute_update ~mode ~params store ~table_meta ~assignments ~where ~indexes
  | Plan.Op_delete { table_meta; where; indexes } ->
    execute_delete ~mode ~params store ~table_meta ~where ~indexes
  | Plan.Op_drop_table { table_meta; indexes } ->
    let* () = execute_drop_table ~mode store cat ~table_meta ~_indexes:indexes in
    Lwt.return 0
  | Plan.Op_drop_index { idx_info } ->
    let* () = execute_drop_index ~mode store cat ~idx_info in
    Lwt.return 0
  | Plan.Op_create_fts_table { name; columns } ->
    let* _ = Cat.create_fts_table cat ~name ~columns in
    Lwt.return 0
  | Plan.Op_fts_insert { fts_meta; col_names; col_values } ->
    let* (tx, owned) = acquire_txn store mode in
    Lwt.catch
      (fun () ->
        let* rowid = Cat.next_fts_rowid_in_txn cat ~name:fts_meta.Cat.fts_name tx in
        let key = Rowid.encode rowid in
        (* Evaluate expressions to get text values *)
        let vals = List.map (fun e -> eval_expr params [||] e) col_values in
        (* Map to FTS column order *)
        let n_cols = List.length fts_meta.Cat.fts_columns in
        let texts = Array.make n_cols "" in
        List.iter2 (fun col_name v ->
          match list_find_index (String.equal col_name) fts_meta.Cat.fts_columns with
          | None -> ()
          | Some (i, _) ->
            texts.(i) <- (match v with Row.V_text s -> s | _ -> "")
        ) col_names vals;
        let text_list = Array.to_list texts in
        (* Store content row *)
        let* () = S.put tx fts_meta.Cat.fts_content_tree key
                    (fts_encode_content text_list) in
        (* Index *)
        let col_texts = List.mapi (fun i t -> (i, t)) text_list in
        let* () = fts_index_document tx ~fts_meta ~rowid ~col_texts in
        let* () = release_txn tx owned in
        Lwt.return 1)
      (fun exn ->
        let* () = if owned then S.rollback tx else Lwt.return_unit in
        Lwt.fail exn)
  | Plan.Op_fts_delete { fts_meta; where } ->
    (* Drain matching rows under an RO snapshot *)
    let* tx_ro = S.ro_begin store in
    let* cur = S.cursor_open tx_ro fts_meta.Cat.fts_content_tree in
    let _sr = S.cursor_first cur in
    let buf = ref [] in
    let rec drain () =
      match S.cursor_next cur with
      | None -> ()
      | Some (kbytes, vbytes) ->
        let rowid = Rowid.decode kbytes in
        let texts = fts_decode_content vbytes in
        let row = Array.of_list (List.map (fun s -> Row.V_text s) texts) in
        let keep = match where with
          | None      -> true
          | Some pred -> value_truthy (eval_expr params row pred)
        in
        if keep then buf := (rowid, kbytes, texts) :: !buf;
        drain ()
    in
    drain ();
    S.cursor_close cur;
    let* () = S.ro_end tx_ro in
    let matches = List.rev !buf in
    let n = List.length matches in
    if n = 0 then Lwt.return 0
    else begin
      let* (tx, owned) = acquire_txn store mode in
      Lwt.catch
        (fun () ->
          let* () =
            Lwt_list.iter_s (fun (rowid, key, texts) ->
              let col_texts = List.mapi (fun i t -> (i, t)) texts in
              let* () = S.del tx fts_meta.Cat.fts_content_tree key in
              fts_deindex_document tx ~fts_meta ~rowid ~col_texts
            ) matches
          in
          let* () = release_txn tx owned in
          Lwt.return n)
        (fun exn ->
          let* () = if owned then S.rollback tx else Lwt.return_unit in
          Lwt.fail exn)
    end
  | Plan.Op_begin | Plan.Op_commit | Plan.Op_rollback ->
    failwith "Exec.execute_with_count: BEGIN/COMMIT/ROLLBACK handled by Db layer"
  | Plan.Op_seq_scan _ | Plan.Op_filter _ | Plan.Op_project _
  | Plan.Op_expr_project _
  | Plan.Op_sort _ | Plan.Op_limit _ | Plan.Op_index_lookup _
  | Plan.Op_nested_loop_join _ | Plan.Op_hash_join _ | Plan.Op_aggregate _
  | Plan.Op_fts_seq_scan _ | Plan.Op_fts_match_scan _ ->
    failwith "Exec.execute: use Exec.query for read operations"

(** Compatibility entry point: discards the rows-affected count. *)
let execute ?(mode = Auto) ?(params = [||]) (store : S.t) (cat : Cat.t) (op : Plan.op) : unit Lwt.t =
  let* _n = execute_with_count ~mode ~params store cat op in
  Lwt.return_unit

(* ------------------------------------------------------------------ *)
(* BM25 scoring helpers                                                 *)
(* ------------------------------------------------------------------ *)

let bm25_score ~k1 ~b ~total_docs ~total_tokens
               ~n_docs_with_term ~term_freq ~doc_length =
  if total_docs = 0 || n_docs_with_term = 0 then 0.0
  else
    let n      = Float.of_int total_docs in
    let n_t    = Float.of_int n_docs_with_term in
    let tf     = Float.of_int term_freq in
    let dl     = Float.of_int doc_length in
    let avgdl  = Float.of_int total_tokens /. n in
    let idf    = Float.log ((n -. n_t +. 0.5) /. (n_t +. 0.5) +. 1.0) in
    idf *. (tf *. (k1 +. 1.0)) /. (tf +. k1 *. (1.0 -. b +. b *. dl /. avgdl))

(** Collect all positive (non-negated) terms from a query for BM25. *)
let fts_query_terms query =
  let rec collect = function
    | Fts_query.FQ_term (Fts_query.FT_exact t)   -> [t]
    | Fts_query.FQ_term (Fts_query.FT_prefix t)  -> [t]
    | Fts_query.FQ_term (Fts_query.FT_phrase ts) -> ts
    | Fts_query.FQ_and qs | Fts_query.FQ_or qs   -> List.concat_map collect qs
    | Fts_query.FQ_not _                          -> []
  in
  List.sort_uniq String.compare (collect query)

(* ------------------------------------------------------------------ *)
(* to_stream: convert a read op tree into a Row stream                  *)
(* ------------------------------------------------------------------ *)

let rec to_stream (params : Row.value array) (store : S.t) (op : Plan.op) : Row.t Lwt_stream.t Lwt.t =
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
    let* inner = to_stream params store child in
    Lwt.return (Lwt_stream.filter (fun row -> value_truthy (eval_expr params row pred)) inner)
  | Plan.Op_project { ordinals; child } ->
    let* inner = to_stream params store child in
    Lwt.return (Lwt_stream.map (project_row ordinals) inner)
  | Plan.Op_expr_project { exprs; child } ->
    let* inner = to_stream params store child in
    let eval_exprs row =
      Array.of_list (List.map (eval_expr params row) exprs)
    in
    Lwt.return (Lwt_stream.map eval_exprs inner)
  | Plan.Op_sort { col_idx; dir; child } ->
    let* inner = to_stream params store child in
    let* rows = Lwt_stream.to_list inner in
    let cmp a b =
      let va = a.(col_idx) and vb = b.(col_idx) in
      let c = compare_values va vb in
      if dir = `Asc then c else -c
    in
    let sorted = List.sort cmp rows in
    Lwt.return (Lwt_stream.of_list sorted)
  | Plan.Op_limit { limit; offset; child } ->
    let* inner = to_stream params store child in
    let* rows = Lwt_stream.to_list inner in
    let rows' = List.filteri (fun i _ -> i >= offset && i < offset + limit) rows in
    Lwt.return (Lwt_stream.of_list rows')
  | Plan.Op_index_lookup { table_tree; idx_tree; col_idx = _;
                           col_type; lookup_val; table_meta } ->
    (* Encode the lookup value as an IndexKey.value matching the column type. *)
    let lookup_v =
      let v = eval_expr params [||] lookup_val in
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
  | Plan.Op_nested_loop_join {
      left; right_meta; idx_tree;
      right_col_idx = _; left_col_idx; join_kind;
      right_col_offset = _; n_right_cols } ->
    (* Indexed nested-loop join: for each left row, seek the right
       index tree for the join key and collect matching right rows. *)
    let* left_stream = to_stream params store left in
    let* left_rows = Lwt_stream.to_list left_stream in
    let* tx = S.ro_begin store in
    let out = ref [] in
    let is_left_join = (join_kind = `Left) in
    let* () =
      Lwt_list.iter_s (fun lrow ->
        let lkey = lrow.(left_col_idx) in
        (* SQL NULL semantics: NULL = NULL is false, so a NULL join key
           never matches any right row.  Skip the index probe entirely. *)
        if lkey = Row.V_null then begin
          if is_left_join then begin
            let null_right = Array.make n_right_cols Row.V_null in
            out := Array.append lrow null_right :: !out
          end;
          Lwt.return_unit
        end else begin
          (* Encode the lookup value uniformly via row_value_to_index_value;
             cross-type entries simply will not match. *)
          let ik_value = row_value_to_index_value lkey in
          let prefix = Index_key.encode_value ik_value in
          let plen = Bytes.length prefix in
          let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
          let* cur = S.cursor_open tx idx_tree in
          let _sr = S.cursor_seek cur seek_key in
          let found = ref false in
          let rec scan () =
            match S.cursor_next cur with
            | None -> Lwt.return_unit
            | Some (ikey, _) ->
              if Bytes.length ikey >= plen + 8 &&
                 Bytes.equal (Bytes.sub ikey 0 plen) prefix
              then begin
                let rowid_bytes = Bytes.sub ikey (Bytes.length ikey - 8) 8 in
                let rowid = Rowid.decode rowid_bytes in
                let table_key = Rowid.encode rowid in
                let* vrow = S.get tx right_meta.Cat.tree_id table_key in
                (match vrow with
                 | None -> scan ()
                 | Some vbytes ->
                   let rrow = Row.decode right_meta.Cat.columns vbytes in
                   let combined = Array.append lrow rrow in
                   out := combined :: !out;
                   found := true;
                   scan ())
              end else Lwt.return_unit
          in
          let* () = scan () in
          S.cursor_close cur;
          (match join_kind with
           | `Left when not !found ->
             let null_right = Array.make n_right_cols Row.V_null in
             out := Array.append lrow null_right :: !out
           | _ -> ());
          Lwt.return_unit
        end
      ) left_rows
    in
    let* () = S.ro_end tx in
    Lwt.return (Lwt_stream.of_list (List.rev !out))
  | Plan.Op_hash_join {
      left; right; left_key; right_key; join_kind;
      right_col_offset = _; n_right_cols } ->
    let* left_stream  = to_stream params store left in
    let* right_stream = to_stream params store right in
    let* right_rows = Lwt_stream.to_list right_stream in
    if left_key < 0 || right_key < 0 then begin
      (* Cartesian product fallback (general ON predicate). *)
      let* left_rows = Lwt_stream.to_list left_stream in
      let out = ref [] in
      List.iter (fun lrow ->
        let any = ref false in
        List.iter (fun rrow ->
          out := Array.append lrow rrow :: !out;
          any := true
        ) right_rows;
        (match join_kind with
         | `Left when not !any ->
           let null_right = Array.make n_right_cols Row.V_null in
           out := Array.append lrow null_right :: !out
         | _ -> ())
      ) left_rows;
      Lwt.return (Lwt_stream.of_list (List.rev !out))
    end else begin
      (* Build phase: hash right rows by their join key.
         NULL-keyed right rows are excluded: they can never match any probe
         (probe phase already skips NULL left keys, so NULL = NULL never fires). *)
      let tbl : (bytes, Row.t list) Hashtbl.t = Hashtbl.create 64 in
      List.iter (fun rrow ->
        let key_v = rrow.(right_key) in
        match key_v with
        | Row.V_null -> ()   (* NULL join key: never matches, skip *)
        | _ ->
          let key_bytes =
            Index_key.encode_value (row_value_to_index_value key_v)
          in
          let prev = try Hashtbl.find tbl key_bytes with Not_found -> [] in
          Hashtbl.replace tbl key_bytes (rrow :: prev)
      ) right_rows;
      let* left_rows = Lwt_stream.to_list left_stream in
      let out = ref [] in
      List.iter (fun lrow ->
        let key_v = lrow.(left_key) in
        let any = ref false in
        (match key_v with
         | Row.V_null -> ()                (* NULL never joins in equi-join *)
         | _ ->
           let key_bytes =
             Index_key.encode_value (row_value_to_index_value key_v)
           in
           (match Hashtbl.find_opt tbl key_bytes with
            | None -> ()
            | Some rrows ->
              (* Preserve build-order: rrows is reversed-insertion. *)
              List.iter (fun rrow ->
                out := Array.append lrow rrow :: !out;
                any := true
              ) (List.rev rrows)));
        (match join_kind with
         | `Left when not !any ->
           let null_right = Array.make n_right_cols Row.V_null in
           out := Array.append lrow null_right :: !out
         | _ -> ())
      ) left_rows;
      Lwt.return (Lwt_stream.of_list (List.rev !out))
    end
  | Plan.Op_aggregate { child; group_col; aggs; having; proj } ->
    let* inner = to_stream params store child in
    let* rows = Lwt_stream.to_list inner in
    let groups : (Row.value * Row.t list) list =
      match group_col with
      | None ->
        [ (Row.V_null, rows) ]
      | Some gc ->
        (* Stable-sort by group column, then split runs of equal keys. *)
        let sorted =
          List.stable_sort (fun a b ->
            compare_values a.(gc) b.(gc)
          ) rows
        in
        let rec group_runs acc cur_key cur_rows = function
          | [] ->
            (match cur_rows with
             | [] -> List.rev acc
             | _  -> List.rev ((cur_key, List.rev cur_rows) :: acc))
          | r :: rest ->
            let k = r.(gc) in
            if compare_values k cur_key = 0 && cur_rows <> [] then
              group_runs acc cur_key (r :: cur_rows) rest
            else
              let acc' =
                if cur_rows = [] then acc
                else (cur_key, List.rev cur_rows) :: acc
              in
              group_runs acc' k [r] rest
        in
        group_runs [] Row.V_null [] sorted
    in
    let compute_agg (spec : Plan.agg_spec) (group_rows : Row.t list) : Row.value =
      match spec.func, spec.col_ord with
      | Ast.Agg_count, None ->
        Row.V_int (Int64.of_int (List.length group_rows))
      | Ast.Agg_count, Some i ->
        let n = List.fold_left (fun acc r ->
          match r.(i) with
          | Row.V_null -> acc
          | _ -> acc + 1
        ) 0 group_rows in
        Row.V_int (Int64.of_int n)
      | Ast.Agg_sum, Some i ->
        (* Sum non-null numeric values; preserve INT vs REAL like SQLite-lite. *)
        let any_real = List.exists (fun r ->
          match r.(i) with Row.V_real _ -> true | _ -> false
        ) group_rows in
        let any_non_null = List.exists (fun r ->
          match r.(i) with Row.V_null -> false | _ -> true
        ) group_rows in
        if not any_non_null then Row.V_null
        else if any_real then
          let s = List.fold_left (fun acc r ->
            match r.(i) with
            | Row.V_null -> acc
            | Row.V_int n -> acc +. Int64.to_float n
            | Row.V_real f -> acc +. f
            | _ -> failwith "SUM on non-numeric value"
          ) 0.0 group_rows in
          Row.V_real s
        else
          let s = List.fold_left (fun acc r ->
            match r.(i) with
            | Row.V_null -> acc
            | Row.V_int n -> Int64.add acc n
            | _ -> failwith "SUM on non-numeric value"
          ) 0L group_rows in
          Row.V_int s
      | Ast.Agg_avg, Some i ->
        let sum, n = List.fold_left (fun (s, n) r ->
          match r.(i) with
          | Row.V_null -> (s, n)
          | Row.V_int x -> (s +. Int64.to_float x, n + 1)
          | Row.V_real f -> (s +. f, n + 1)
          | _ -> failwith "AVG on non-numeric value"
        ) (0.0, 0) group_rows in
        if n = 0 then Row.V_null
        else Row.V_real (sum /. float_of_int n)
      | Ast.Agg_min, Some i ->
        List.fold_left (fun acc r ->
          match r.(i), acc with
          | Row.V_null, _ -> acc
          | v, Row.V_null -> v
          | v, cur ->
            if compare_values v cur < 0 then v else cur
        ) Row.V_null group_rows
      | Ast.Agg_max, Some i ->
        List.fold_left (fun acc r ->
          match r.(i), acc with
          | Row.V_null, _ -> acc
          | v, Row.V_null -> v
          | v, cur ->
            if compare_values v cur > 0 then v else cur
        ) Row.V_null group_rows
      | (Ast.Agg_sum | Ast.Agg_avg | Ast.Agg_min | Ast.Agg_max), None ->
        failwith "non-COUNT aggregate must have a column argument"
    in
    let agg_output_rows =
      List.map (fun (group_key, group_rows) ->
        let agg_vals = List.map (fun spec -> compute_agg spec group_rows) aggs in
        let out =
          match group_col with
          | None   -> Array.of_list agg_vals
          | Some _ -> Array.of_list (group_key :: agg_vals)
        in
        out
      ) groups
    in
    (* Apply HAVING on the aggregate output row. *)
    let after_having =
      match having with
      | None -> agg_output_rows
      | Some pred ->
        List.filter (fun r -> value_truthy (eval_expr params r pred)) agg_output_rows
    in
    (* Project to final output row. *)
    let final_rows =
      List.map (fun agg_row ->
        Array.of_list (List.map (function
          | Plan.PI_group_col ->
            (match group_col with
             | Some _ -> agg_row.(0)
             | None   -> failwith "PI_group_col without group_col")
          | Plan.PI_agg_slot k ->
            let off = match group_col with Some _ -> 1 | None -> 0 in
            agg_row.(off + k)
        ) proj)
      ) after_having
    in
    Lwt.return (Lwt_stream.of_list final_rows)
  | Plan.Op_fts_seq_scan { fts_meta; where } ->
    let* tx = S.ro_begin store in
    let* cur = S.cursor_open tx fts_meta.Cat.fts_content_tree in
    let _sr = S.cursor_first cur in
    let exhausted = ref false in
    let rec read_next () =
      if !exhausted then Lwt.return_none
      else
        match S.cursor_next cur with
        | None ->
          exhausted := true;
          S.cursor_close cur;
          let%lwt () = S.ro_end tx in
          Lwt.return_none
        | Some (_key, val_bytes) ->
          let texts = fts_decode_content val_bytes in
          let row = Array.of_list (List.map (fun s -> Row.V_text s) texts) in
          let emit = match where with
            | None      -> true
            | Some pred -> value_truthy (eval_expr params row pred)
          in
          if emit then Lwt.return_some row
          else read_next ()
    in
    Lwt.return (Lwt_stream.from read_next)
  | Plan.Op_fts_match_scan { fts_meta; query; proj; include_rank } ->
    let* tx = S.ro_begin store in
    let* matches = fts_execute_query tx ~index_tree:fts_meta.Cat.fts_index_tree query in
    (* Compute BM25 scores when rank is requested *)
    let* scored_matches =
      if not include_rank then
        Lwt.return (List.map (fun (rowid, positions) -> (rowid, positions, 0.0)) matches)
      else begin
        let* (total_docs, total_tokens) = read_fts_stats tx fts_meta.Cat.fts_index_tree in
        let query_terms = fts_query_terms query in
        let* term_doc_counts = Lwt_list.map_s (fun term ->
          let* pl = fts_posting_list tx ~index_tree:fts_meta.Cat.fts_index_tree term in
          Lwt.return (term, List.length pl)) query_terms in
        let* doc_lengths = Lwt_list.map_s (fun (rowid, positions) ->
          let dlen_key = fts_doclen_key rowid in
          let* v = S.get tx fts_meta.Cat.fts_index_tree dlen_key in
          let dl = match v with
            | None -> 1
            | Some b -> let (n, _) = Varint.decode_uint64 b 0 in Int64.to_int n
          in
          Lwt.return (rowid, positions, dl)) matches in
        let scored = List.map (fun (rowid, positions, dl) ->
          let tf = List.length positions in
          let score = List.fold_left (fun acc (_, n_docs) ->
            acc +. bm25_score ~k1:1.2 ~b:0.75 ~total_docs ~total_tokens
                               ~n_docs_with_term:n_docs ~term_freq:tf ~doc_length:dl)
            0.0 term_doc_counts in
          (rowid, positions, score)) doc_lengths in
        Lwt.return scored
      end
    in
    (* Sort by BM25 score descending when rank is included *)
    let sorted = if include_rank then
      List.sort (fun (_, _, s1) (_, _, s2) -> Float.compare s2 s1) scored_matches
    else scored_matches in
    let* rows = Lwt_list.filter_map_s (fun (rowid, _positions, score) ->
      let key = Rowid.encode rowid in
      let* val_opt = S.get tx fts_meta.Cat.fts_content_tree key in
      match val_opt with
      | None -> Lwt.return None
      | Some bytes ->
        let texts = fts_decode_content bytes in
        let full_row = Array.of_list (List.map (fun s -> Row.V_text s) texts) in
        let projected = if proj = [] then Array.to_list full_row
                        else List.map (fun i -> full_row.(i)) proj in
        let row_values = projected @ (if include_rank then [Row.V_real score] else []) in
        Lwt.return (Some (Array.of_list row_values))) sorted in
    let* () = S.ro_end tx in
    Lwt.return (Lwt_stream.of_list rows)
  | Plan.Op_create_table _ | Plan.Op_insert _ | Plan.Op_create_index _
  | Plan.Op_update _ | Plan.Op_delete _
  | Plan.Op_drop_table _ | Plan.Op_drop_index _
  | Plan.Op_create_fts_table _
  | Plan.Op_fts_insert _ | Plan.Op_fts_delete _
  | Plan.Op_begin | Plan.Op_commit | Plan.Op_rollback ->
    failwith "Exec.query: use Exec.execute for write operations"

(* ------------------------------------------------------------------ *)
(* Public query entry point                                             *)
(* ------------------------------------------------------------------ *)

let query ?(params = [||]) (store : S.t) (_cat : Cat.t) (op : Plan.op) :
    Row.t Lwt_stream.t Lwt.t =
  to_stream params store op
