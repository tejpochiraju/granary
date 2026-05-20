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
  | Ast.L_current_timestamp
  | Ast.L_current_date
  | Ast.L_current_time ->
    failwith "lit_to_value: CURRENT_* should not appear as a plan literal"

let value_to_literal : Row.value -> Ast.literal = function
  | Row.V_int n  -> Ast.L_int n
  | Row.V_text s -> Ast.L_text s
  | Row.V_real f -> Ast.L_real f
  | Row.V_blob b -> Ast.L_blob b
  | Row.V_null   -> Ast.L_null

let row_value_to_index_value : Row.value -> Index_key.value = function
  | Row.V_int  n -> Index_key.IK_int n
  | Row.V_text s -> Index_key.IK_text s
  | Row.V_null   -> Index_key.IK_null
  | Row.V_real f -> Index_key.IK_real f
  | Row.V_blob b -> Index_key.IK_blob b

let compare_values (a : Row.value) (b : Row.value) : int =
  match a, b with
  | Row.V_null, Row.V_null -> 0
  | Row.V_null, _          -> -1  (* NULLs sort first — less than any non-null value, matches SQLite *)
  | _, Row.V_null          -> 1
  | Row.V_int  x, Row.V_int  y -> Int64.compare x y
  | Row.V_real x, Row.V_real y -> Float.compare x y
  | Row.V_text x, Row.V_text y -> String.compare x y
  | Row.V_blob x, Row.V_blob y -> Bytes.compare x y
  | _,            _            -> 0  (* cross-type: shouldn't happen *)

let compare_with_nulls (dir : [`Asc | `Desc]) (nulls : [`Nulls_first | `Nulls_last])
    (va : Row.value) (vb : Row.value) : int =
  match va, vb with
  | Row.V_null, Row.V_null -> 0
  | Row.V_null, _ -> (match nulls with `Nulls_first -> -1 | `Nulls_last -> 1)
  | _, Row.V_null -> (match nulls with `Nulls_first -> 1 | `Nulls_last -> -1)
  | _, _ ->
    let c = compare_values va vb in
    (match dir with `Asc -> c | `Desc -> -c)

let list_drop n lst =
  let rec go k = function
    | [] -> []
    | (_ :: t) as l -> if k <= 0 then l else go (k - 1) t
  in go n lst

let list_take n lst =
  let rec go k = function
    | [] -> []
    | h :: t -> if k <= 0 then [] else h :: go (k - 1) t
  in go n lst

(** Find a column ordinal by name within a [Row.column] list. *)
let find_col_idx_by_name (cols : Row.column list) (name : string) : int =
  let rec find i = function
    | [] -> failwith (Printf.sprintf "column not found: %s" name)
    | (c : Row.column) :: _ when String.equal c.Row.name name -> i
    | _ :: rest -> find (i + 1) rest
  in
  find 0 cols

(* Module-level cache for compiled CHECK expressions.
   Key: (table_name, column_ordinal, check_sql) → compiled Plan.expr.
   Including check_sql avoids stale hits when different tables share the same
   name and column index across DB instances (e.g. test isolation). *)
let check_expr_cache : (string * int * string, Plan.expr) Hashtbl.t = Hashtbl.create 16

(* ── DDL reconstruction for Op_sqlite_master ─────────────────── *)

let sql_of_row_type = function
  | Row.Integer -> "INTEGER"
  | Row.Text    -> "TEXT"
  | Row.Real    -> "REAL"
  | Row.Blob    -> "BLOB"

let sql_of_default_value = function
  | Row.DV_int n  -> Int64.to_string n
  | Row.DV_text s ->
    let escaped = String.concat "''" (String.split_on_char '\'' s) in
    Printf.sprintf "'%s'" escaped
  | Row.DV_real f -> Printf.sprintf "%g" f
  | Row.DV_blob b ->
    let hex = Bytes.to_seq b
      |> Seq.map (fun c -> Printf.sprintf "%02X" (Char.code c))
      |> List.of_seq
      |> String.concat ""
    in
    Printf.sprintf "X'%s'" hex
  | Row.DV_null   -> "NULL"
  | Row.DV_current_timestamp -> "CURRENT_TIMESTAMP"
  | Row.DV_current_date      -> "CURRENT_DATE"
  | Row.DV_current_time      -> "CURRENT_TIME"

let sql_of_fk_action = function
  | Cat.FA_no_action   -> "NO ACTION"
  | Cat.FA_restrict    -> "RESTRICT"
  | Cat.FA_cascade     -> "CASCADE"
  | Cat.FA_set_null    -> "SET NULL"
  | Cat.FA_set_default -> "SET DEFAULT"

let ddl_of_table (meta : Cat.table_meta) =
  let col_parts = List.map (fun (col : Row.column) ->
    let buf = Buffer.create 64 in
    Buffer.add_string buf col.Row.name;
    Buffer.add_char   buf ' ';
    Buffer.add_string buf (sql_of_row_type col.Row.ty);
    if col.Row.not_null    then Buffer.add_string buf " NOT NULL";
    if col.Row.primary_key then Buffer.add_string buf " PRIMARY KEY";
    (match col.Row.default with
     | None    -> ()
     | Some dv ->
       Buffer.add_string buf " DEFAULT ";
       Buffer.add_string buf (sql_of_default_value dv));
    (match col.Row.check_sql with
     | None     -> ()
     | Some sql ->
       Buffer.add_string buf " CHECK(";
       Buffer.add_string buf sql;
       Buffer.add_char   buf ')');
    (match col.Row.generated_as with
     | None -> ()
     | Some (expr_sql, is_stored) ->
       Buffer.add_string buf " GENERATED ALWAYS AS (";
       Buffer.add_string buf expr_sql;
       Buffer.add_string buf ") ";
       Buffer.add_string buf (if is_stored then "STORED" else "VIRTUAL"));
    Buffer.contents buf
  ) meta.Cat.columns in
  let fk_parts = List.map (fun (fk : Cat.fk_constraint) ->
    Printf.sprintf "FOREIGN KEY (%s) REFERENCES %s(%s) ON DELETE %s ON UPDATE %s"
      fk.Cat.fk_local_col
      fk.Cat.fk_parent_table
      fk.Cat.fk_parent_col
      (sql_of_fk_action fk.Cat.fk_on_delete)
      (sql_of_fk_action fk.Cat.fk_on_update)
  ) meta.Cat.fk_constraints in
  Printf.sprintf "CREATE TABLE %s (%s)"
    meta.Cat.name
    (String.concat ", " (col_parts @ fk_parts))

(** Extract the ON <table> target from a CREATE TRIGGER statement.
    Falls back to the trigger name if the ON clause is not found. *)
let trigger_table_of_sql trigger_name sql =
  (* Look for " ON " followed by identifier, case-insensitive *)
  let upper = String.uppercase_ascii sql in
  match String.index_opt upper 'O' with
  | None -> trigger_name
  | _ ->
    let n = String.length upper in
    (* Search for " ON " pattern *)
    let rec search i =
      if i + 4 >= n then trigger_name
      else if upper.[i] = ' ' && upper.[i+1] = 'O' && upper.[i+2] = 'N' && upper.[i+3] = ' ' then
        (* Found " ON " — extract the identifier that follows *)
        let start = i + 4 in
        let j = ref start in
        while !j < n &&
              (let c = upper.[!j] in
               (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_') do
          incr j
        done;
        if !j > start then String.sub sql start (!j - start)
        else trigger_name
      else search (i + 1)
    in
    search 0

let ddl_of_index (idx : Cat.index_info) =
  let unique_kw = if idx.Cat.idx_unique then "UNIQUE " else "" in
  let col_strs = List.map2 (fun col_sql is_expr ->
    if is_expr then Printf.sprintf "(%s)" col_sql else col_sql
  ) idx.Cat.idx_columns idx.Cat.idx_expr_flags in
  let cols_str = String.concat ", " col_strs in
  let where_clause = match idx.Cat.idx_where_sql with
    | None     -> ""
    | Some sql -> Printf.sprintf " WHERE %s" sql
  in
  Printf.sprintf "CREATE %sINDEX %s ON %s (%s)%s"
    unique_kw idx.Cat.idx_name idx.Cat.idx_table cols_str where_clause


(* ------------------------------------------------------------------ *)
(* Expression evaluation                                                *)
(* (Defined before [execute] so that [Op_update] can evaluate WHERE     *)
(*  predicates and right-hand-side expressions for SET assignments.)    *)
(* ------------------------------------------------------------------ *)

let value_truthy : Row.value -> bool = function
  | Row.V_null | Row.V_int 0L -> false
  | _                          -> true

(* Pattern matching helpers for LIKE and GLOB.
   Uses naive recursive backtracking: worst case is O(2^k) for k '%'/'*'
   metacharacters against an adversarial string.  Acceptable for typical
   SQL workloads; replace with NFA/DP if adversarial patterns are a concern. *)
let rec like_match pat pi str si =
  let plen = String.length pat and slen = String.length str in
  if pi = plen then si = slen
  else match pat.[pi] with
  | '%' -> like_match pat (pi+1) str si ||
            (si < slen && like_match pat pi str (si+1))
  | '_' -> si < slen && like_match pat (pi+1) str (si+1)
  | c   -> si < slen && Char.lowercase_ascii c = Char.lowercase_ascii str.[si] &&
            like_match pat (pi+1) str (si+1)

let rec glob_match pat pi str si =
  let plen = String.length pat and slen = String.length str in
  if pi = plen then si = slen
  else match pat.[pi] with
  | '*' -> glob_match pat (pi+1) str si ||
            (si < slen && glob_match pat pi str (si+1))
  | '?' -> si < slen && glob_match pat (pi+1) str (si+1)
  | c   -> si < slen && c = str.[si] && glob_match pat (pi+1) str (si+1)

let str_trim_spaces s =
  let n = String.length s in
  let l = ref 0 and r = ref (n - 1) in
  while !l <= !r && (let c = s.[!l] in c = ' ' || c = '\t' || c = '\n' || c = '\r') do incr l done;
  while !r >= !l && (let c = s.[!r] in c = ' ' || c = '\t' || c = '\n' || c = '\r') do decr r done;
  if !l > !r then "" else String.sub s !l (!r - !l + 1)

let parse_int_prefix s =
  let s = String.trim s in
  match Int64.of_string_opt s with
  | Some n -> n
  | None ->
    match float_of_string_opt s with
    | Some f -> Int64.of_float f
    | None ->
      (* Scan leading numeric prefix: optional sign, digits, optional decimal *)
      let n = String.length s in
      let i = ref 0 in
      if !i < n && (s.[!i] = '-' || s.[!i] = '+') then incr i;
      let digit_start = !i in
      while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do incr i done;
      (* Include decimal part for float->int conversion *)
      let has_dot = !i < n && s.[!i] = '.' in
      if has_dot then begin
        incr i;
        while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do incr i done
      end;
      if !i > digit_start then
        (match float_of_string_opt (String.sub s 0 !i) with
         | Some f -> Int64.of_float f
         | None ->
           match Int64.of_string_opt (String.sub s 0 !i) with
           | Some v -> v
           | None -> 0L)
      else 0L

let parse_real_prefix s =
  let s = String.trim s in
  match float_of_string_opt s with
  | Some f -> f
  | None ->
    (* Try progressively shorter prefixes until one parses *)
    let n = String.length s in
    let result = ref 0.0 in
    let found = ref false in
    let i = ref n in
    while !i > 0 && not !found do
      (match float_of_string_opt (String.sub s 0 !i) with
       | Some f -> result := f; found := true
       | None   -> decr i)
    done;
    !result

let str_trim_chars s chars =
  let n = String.length s in
  let l = ref 0 and r = ref (n - 1) in
  while !l <= !r && String.contains chars s.[!l] do incr l done;
  while !r >= !l && String.contains chars s.[!r] do decr r done;
  if !l > !r then "" else String.sub s !l (!r - !l + 1)

let str_ltrim_spaces s =
  let n = String.length s in
  let l = ref 0 in
  while !l < n && (let c = s.[!l] in c = ' ' || c = '\t' || c = '\n' || c = '\r') do incr l done;
  String.sub s !l (n - !l)

let str_ltrim_chars s chars =
  let n = String.length s in
  let l = ref 0 in
  while !l < n && String.contains chars s.[!l] do incr l done;
  String.sub s !l (n - !l)

let str_rtrim_spaces s =
  let n = String.length s in
  let r = ref (n - 1) in
  while !r >= 0 && (let c = s.[!r] in c = ' ' || c = '\t' || c = '\n' || c = '\r') do decr r done;
  if !r < 0 then "" else String.sub s 0 (!r + 1)

let str_rtrim_chars s chars =
  let r = ref (String.length s - 1) in
  while !r >= 0 && String.contains chars s.[!r] do decr r done;
  if !r < 0 then "" else String.sub s 0 (!r + 1)

let str_replace s old rep =
  if String.length old = 0 then s
  else
    let buf = Buffer.create (String.length s) in
    let n = String.length s and m = String.length old in
    let i = ref 0 in
    while !i <= n - m do
      if String.sub s !i m = old then (Buffer.add_string buf rep; i := !i + m)
      else (Buffer.add_char buf s.[!i]; incr i)
    done;
    while !i < n do Buffer.add_char buf s.[!i]; incr i done;
    Buffer.contents buf

let str_instr s sub =
  let n = String.length s and m = String.length sub in
  if m = 0 then 1
  else
    let found = ref 0 in
    let i = ref 0 in
    while !found = 0 && !i <= n - m do
      if String.sub s !i m = sub then found := !i + 1  (* 1-indexed *)
      else incr i
    done;
    !found

let row_key (row : Row.t) : string =
  let buf = Buffer.create 64 in
  Array.iter (function
    | Row.V_null   -> Buffer.add_string buf "N|"
    | Row.V_int n  -> Buffer.add_char buf 'I';
                      Buffer.add_string buf (Int64.to_string n);
                      Buffer.add_char buf '|'
    | Row.V_real f -> Buffer.add_char buf 'R';
                      Buffer.add_string buf (Printf.sprintf "%h" f);
                      Buffer.add_char buf '|'
    | Row.V_text s -> Buffer.add_char buf 'T';
                      Buffer.add_string buf (string_of_int (String.length s));
                      Buffer.add_char buf ':';
                      Buffer.add_string buf s;
                      Buffer.add_char buf '|'
    | Row.V_blob b -> Buffer.add_char buf 'B';
                      Buffer.add_string buf (string_of_int (Bytes.length b));
                      Buffer.add_char buf ':';
                      Buffer.add_bytes buf b;
                      Buffer.add_char buf '|'
  ) row;
  Buffer.contents buf

let json_of_sql : Row.value -> Json.value = function
  | Row.V_null   -> Json.J_null
  | Row.V_int n  -> Json.J_int n
  | Row.V_real f -> Json.J_float f
  | Row.V_text s -> Json.J_string s
  | Row.V_blob b -> Json.J_string (Bytes.to_string b)

let sql_of_json : Json.value -> Row.value = function
  | Json.J_null     -> Row.V_null
  | Json.J_bool b   -> Row.V_int (if b then 1L else 0L)
  | Json.J_int n    -> Row.V_int n
  | Json.J_float f  -> Row.V_real f
  | Json.J_string s -> Row.V_text s
  | Json.J_array _  as v -> Row.V_text (Json.to_string v)
  | Json.J_object _ as v -> Row.V_text (Json.to_string v)

let rec eval_expr (clock : (unit -> float) option) (params : Row.value array) (row : Row.t) (e : Plan.expr) : Row.value =
  match e with
  | Plan.P_lit l            -> lit_to_value l
  | Plan.P_col i            -> row.(i)
  | Plan.P_param i          ->
    if i < Array.length params then params.(i) else Row.V_null
  | Plan.P_neg e ->
    (match eval_expr clock params row e with
     | Row.V_int  n -> Row.V_int  (Int64.neg n)
     | Row.V_real f -> Row.V_real (-. f)
     | Row.V_null   -> Row.V_null
     | _            -> failwith "unary minus requires numeric operand")
  | Plan.P_bitnot e ->
    (match eval_expr clock params row e with
     | Row.V_int n -> Row.V_int (Int64.lognot n)
     | Row.V_null  -> Row.V_null
     | _           -> Row.V_null)
  | Plan.P_between (x, lo, hi) ->
    let vx  = eval_expr clock params row x  in
    let vlo = eval_expr clock params row lo in
    let vhi = eval_expr clock params row hi in
    (match vx, vlo, vhi with
     | Row.V_null, _, _ | _, Row.V_null, _ | _, _, Row.V_null -> Row.V_null
     | _ ->
       let ge_lo = compare_values vx vlo >= 0 in
       let le_hi = compare_values vx vhi <= 0 in
       Row.V_int (if ge_lo && le_hi then 1L else 0L))
  | Plan.P_in (x, vals) ->
    let vx = eval_expr clock params row x in
    if vx = Row.V_null then Row.V_null
    else
      let result = List.fold_left (fun acc ve ->
        let v = eval_expr clock params row ve in
        match acc with
        | `Found -> `Found
        | _ when v = Row.V_null -> `Maybe
        | _ when compare_values vx v = 0 -> `Found
        | acc -> acc
      ) `Not_found vals in
      (match result with
       | `Found     -> Row.V_int 1L
       | `Maybe     -> Row.V_null
       | `Not_found -> Row.V_int 0L)
  | Plan.P_is_null e ->
    (match eval_expr clock params row e with
     | Row.V_null -> Row.V_int 1L
     | _          -> Row.V_int 0L)
  | Plan.P_is_not_null e ->
    (match eval_expr clock params row e with
     | Row.V_null -> Row.V_int 0L
     | _          -> Row.V_int 1L)
  | Plan.P_not e ->
    (match eval_expr clock params row e with
     | Row.V_null -> Row.V_null
     | v -> if value_truthy v then Row.V_int 0L else Row.V_int 1L)
  | Plan.P_binop (op, lhs_e, rhs_e) ->
    let lv = eval_expr clock params row lhs_e in
    let rv = eval_expr clock params row rhs_e in
    let is_nocase = function
      | Plan.P_collate (_, Ast.Collate_nocase) -> true
      | _ -> false
    in
    let nocase_text v = match v with
      | Row.V_text s -> Row.V_text (String.lowercase_ascii s)
      | o -> o
    in
    let (lv', rv') =
      if is_nocase lhs_e then (lv, nocase_text rv)
      else if is_nocase rhs_e then (nocase_text lv, rv)
      else (lv, rv)
    in
    eval_binop op lv' rv'
  | Plan.P_func (func, args) ->
    eval_func clock func (List.map (eval_expr clock params row) args)
  | Plan.P_case { scrutinee; branches; else_ } ->
    let scr_val = Option.map (eval_expr clock params row) scrutinee in
    let rec find_match = function
      | [] ->
        (match else_ with
         | None   -> Row.V_null
         | Some e -> eval_expr clock params row e)
      | (cond, result) :: rest ->
        let matched = match scr_val with
          | None ->
            value_truthy (eval_expr clock params row cond)
          | Some sv ->
            let cv = eval_expr clock params row cond in
            (match sv, cv with
             | Row.V_null, _ | _, Row.V_null -> false
             | _ -> compare_values sv cv = 0)
        in
        if matched then eval_expr clock params row result
        else find_match rest
    in
    find_match branches
  | Plan.P_cast (e, ty) ->
    let v = eval_expr clock params row e in
    (match v with
     | Row.V_null -> Row.V_null
     | _ ->
       (match ty with
        | Ast.Ty_int ->
          (match v with
           | Row.V_int n  -> Row.V_int n
           | Row.V_real f -> Row.V_int (Int64.of_float f)
           | Row.V_text s -> Row.V_int (parse_int_prefix s)
           | Row.V_blob _ -> Row.V_int 0L
           | Row.V_null   -> assert false)
        | Ast.Ty_real ->
          (match v with
           | Row.V_int n  -> Row.V_real (Int64.to_float n)
           | Row.V_real f -> Row.V_real f
           | Row.V_text s -> Row.V_real (parse_real_prefix s)
           | Row.V_blob _ -> Row.V_real 0.0
           | Row.V_null   -> assert false)
        | Ast.Ty_text ->
          (match v with
           | Row.V_int n  -> Row.V_text (Int64.to_string n)
           | Row.V_real f ->
             (* SQLite appends ".0" when the %.15g result has no decimal point
                or exponent, so that CAST(1.0 AS TEXT) → "1.0" not "1". *)
             let s = Printf.sprintf "%.15g" f in
             let needs_dot = not (String.contains s '.' || String.contains s 'e'
                                  || String.contains s 'E' || String.contains s 'n') in
             Row.V_text (if needs_dot then s ^ ".0" else s)
           | Row.V_text s -> Row.V_text s
           | Row.V_blob b -> Row.V_text (Bytes.to_string b)
           | Row.V_null   -> assert false)
        | Ast.Ty_blob ->
          (match v with
           | Row.V_blob b -> Row.V_blob b
           | Row.V_text s -> Row.V_blob (Bytes.of_string s)
           | Row.V_int n  -> Row.V_blob (Bytes.of_string (Int64.to_string n))
           | Row.V_real f -> Row.V_blob (Bytes.of_string (Printf.sprintf "%.15g" f))
           | Row.V_null   -> assert false)))
  | Plan.P_subquery _ | Plan.P_exists _ | Plan.P_in_select _ ->
    (* These are replaced by pre_eval_subquery before row evaluation. *)
    Row.V_null
  | Plan.P_excluded_col _ ->
    failwith "Exec: P_excluded_col in eval_expr — must be substituted before evaluation"
  | Plan.P_window_slot _ ->
    failwith "Exec: P_window_slot in eval_expr — must be substituted by planner before evaluation"
  | Plan.P_collate (e, Ast.Collate_nocase) ->
    let v = eval_expr clock params row e in
    (match v with Row.V_text s -> Row.V_text (String.lowercase_ascii s) | o -> o)
  | Plan.P_collate (e, _) ->
    eval_expr clock params row e  (* Collate_binary and Collate_rtrim are identity *)

and eval_func (clock : (unit -> float) option) (func : Ast.scalar_func) (args : Row.value list) : Row.value =
  let to_float_opt = function
    | Row.V_real f -> Some f
    | Row.V_int n  -> Some (Int64.to_float n)
    | _            -> None
  in
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
  | Ast.Fn_substr, (Row.V_text s :: rest) ->
    (match rest with
     | [Row.V_int start] ->
       let i = max 0 (Int64.to_int start - 1) in
       if i >= String.length s then Row.V_text ""
       else Row.V_text (String.sub s i (String.length s - i))
     | [Row.V_int start; Row.V_int len] ->
       let i = max 0 (Int64.to_int start - 1) in
       let l = Int64.to_int len in
       if i >= String.length s || l <= 0 then Row.V_text ""
       else Row.V_text (String.sub s i (min l (String.length s - i)))
     | _ -> Row.V_null)
  | Ast.Fn_substr, (Row.V_null :: _) -> Row.V_null
  | Ast.Fn_trim,  [Row.V_text s]                        -> Row.V_text (str_trim_spaces s)
  | Ast.Fn_trim,  [Row.V_text s; Row.V_text chars]      -> Row.V_text (str_trim_chars s chars)
  | Ast.Fn_trim,  [_; Row.V_null]                       -> Row.V_null
  | Ast.Fn_trim,  (Row.V_null :: _)                     -> Row.V_null
  | Ast.Fn_ltrim, [Row.V_text s]                        -> Row.V_text (str_ltrim_spaces s)
  | Ast.Fn_ltrim, [Row.V_text s; Row.V_text chars]      -> Row.V_text (str_ltrim_chars s chars)
  | Ast.Fn_ltrim, [_; Row.V_null]                       -> Row.V_null
  | Ast.Fn_ltrim, (Row.V_null :: _)                     -> Row.V_null
  | Ast.Fn_rtrim, [Row.V_text s]                        -> Row.V_text (str_rtrim_spaces s)
  | Ast.Fn_rtrim, [Row.V_text s; Row.V_text chars]      -> Row.V_text (str_rtrim_chars s chars)
  | Ast.Fn_rtrim, [_; Row.V_null]                       -> Row.V_null
  | Ast.Fn_rtrim, (Row.V_null :: _)                     -> Row.V_null
  | Ast.Fn_replace, [Row.V_text s; Row.V_text old; Row.V_text rep] ->
    Row.V_text (str_replace s old rep)
  | Ast.Fn_replace, [_; Row.V_null; _] -> Row.V_null
  | Ast.Fn_replace, [_; _; Row.V_null] -> Row.V_null
  | Ast.Fn_replace, (Row.V_null :: _) -> Row.V_null
  | Ast.Fn_instr, [Row.V_text s; Row.V_text sub] ->
    Row.V_int (Int64.of_int (str_instr s sub))
  | Ast.Fn_instr, (Row.V_null :: _) | Ast.Fn_instr, [_; Row.V_null] -> Row.V_null
  | Ast.Fn_round, [Row.V_real f] ->
    Row.V_real (Float.round f)
  | Ast.Fn_round, [Row.V_int n] ->
    Row.V_real (Int64.to_float n)
  | Ast.Fn_round, [Row.V_real f; Row.V_int d] ->
    let factor = 10. ** Int64.to_float d in
    Row.V_real (Float.round (f *. factor) /. factor)
  | Ast.Fn_round, [Row.V_int n; Row.V_int _] ->
    Row.V_real (Int64.to_float n)
  | Ast.Fn_round, [_; Row.V_null] -> Row.V_null
  | Ast.Fn_round, (Row.V_null :: _) -> Row.V_null
  | Ast.Fn_typeof, [v] ->
    Row.V_text (match v with
      | Row.V_int  _ -> "integer"
      | Row.V_real _ -> "real"
      | Row.V_text _ -> "text"
      | Row.V_blob _ -> "blob"
      | Row.V_null   -> "null")
  | Ast.Fn_date, args ->
    (match args with
     | [] | [Row.V_null] -> Row.V_null
     | Row.V_null :: _ -> Row.V_null
     | Row.V_text ts :: rest ->
       if rest <> [] then Row.V_null
       else (match Datetime.parse ?now:clock ts with
         | Error _ -> Row.V_null
         | Ok dt   -> Row.V_text (Datetime.to_date dt))
     | _ -> Row.V_null)
  | Ast.Fn_time, args ->
    (match args with
     | [] | [Row.V_null] -> Row.V_null
     | Row.V_null :: _ -> Row.V_null
     | Row.V_text ts :: rest ->
       if rest <> [] then Row.V_null
       else (match Datetime.parse ?now:clock ts with
         | Error _ -> Row.V_null
         | Ok dt   -> Row.V_text (Datetime.to_time dt))
     | _ -> Row.V_null)
  | Ast.Fn_datetime, args ->
    (match args with
     | [] | [Row.V_null] -> Row.V_null
     | Row.V_null :: _ -> Row.V_null
     | Row.V_text ts :: rest ->
       if rest <> [] then Row.V_null
       else (match Datetime.parse ?now:clock ts with
         | Error _ -> Row.V_null
         | Ok dt   -> Row.V_text (Datetime.to_datetime dt))
     | _ -> Row.V_null)
  | Ast.Fn_julianday, args ->
    (match args with
     | [] | [Row.V_null] -> Row.V_null
     | Row.V_null :: _ -> Row.V_null
     | Row.V_text ts :: rest ->
       if rest <> [] then Row.V_null
       else (match Datetime.parse ?now:clock ts with
         | Error _ -> Row.V_null
         | Ok dt   -> Row.V_real (Datetime.to_julianday dt))
     | _ -> Row.V_null)
  | Ast.Fn_unixepoch, args ->
    (match args with
     | [] | [Row.V_null] -> Row.V_null
     | Row.V_null :: _ -> Row.V_null
     | Row.V_text ts :: rest ->
       if rest <> [] then Row.V_null
       else (match Datetime.parse ?now:clock ts with
         | Error _ -> Row.V_null
         | Ok dt   -> Row.V_int (Datetime.to_unixepoch dt))
     | _ -> Row.V_null)
  | Ast.Fn_strftime, args ->
    (match args with
     | Row.V_text fmt :: Row.V_text ts :: rest ->
       if rest <> [] then Row.V_null
       else (match Datetime.parse ?now:clock ts with
         | Error _ -> Row.V_null
         | Ok dt   -> Row.V_text (Datetime.strftime fmt dt))
     | _ -> Row.V_null)
  | Ast.Fn_ceil, [v] ->
    (match to_float_opt v with Some f -> Row.V_real (Float.ceil f) | None -> Row.V_null)
  | Ast.Fn_floor, [v] ->
    (match to_float_opt v with Some f -> Row.V_real (Float.floor f) | None -> Row.V_null)
  | Ast.Fn_sqrt, [v] ->
    (match to_float_opt v with Some f -> Row.V_real (Float.sqrt f) | None -> Row.V_null)
  | Ast.Fn_pow, [b; e] ->
    (match to_float_opt b, to_float_opt e with
     | Some bf, Some ef -> Row.V_real (bf ** ef)
     | _ -> Row.V_null)
  | Ast.Fn_exp, [v] ->
    (match to_float_opt v with Some f -> Row.V_real (Float.exp f) | None -> Row.V_null)
  | Ast.Fn_ln, [v] ->
    (match to_float_opt v with Some f -> Row.V_real (Float.log f) | None -> Row.V_null)
  | Ast.Fn_log, [v] ->
    (match to_float_opt v with Some f -> Row.V_real (Float.log f) | None -> Row.V_null)
  | Ast.Fn_log, [b; x] ->
    (match to_float_opt b, to_float_opt x with
     | Some bf, Some xf -> Row.V_real (Float.log xf /. Float.log bf)
     | _ -> Row.V_null)
  | Ast.Fn_log2, [v] ->
    (match to_float_opt v with
     | Some f -> Row.V_real (Float.log f /. Float.log 2.0)
     | None -> Row.V_null)
  | Ast.Fn_log10, [v] ->
    (match to_float_opt v with Some f -> Row.V_real (Float.log10 f) | None -> Row.V_null)
  | Ast.Fn_sign, [v] ->
    (match to_float_opt v with
     | Some f -> Row.V_int (if f > 0.0 then 1L else if f < 0.0 then (-1L) else 0L)
     | None -> Row.V_null)
  | Ast.Fn_trunc, [v] ->
    (match to_float_opt v with
     | Some f -> Row.V_real (if f >= 0.0 then Float.floor f else Float.ceil f)
     | None -> Row.V_null)
  | Ast.Fn_trunc, [v; d] ->
    (match to_float_opt v, to_float_opt d with
     | Some f, Some df ->
       let factor = 10.0 ** (Float.round df) in
       let fx = f *. factor in
       Row.V_real ((if fx >= 0.0 then Float.floor fx else Float.ceil fx) /. factor)
     | _ -> Row.V_null)
  | Ast.Fn_pi, [] -> Row.V_real Float.pi
  | Ast.Fn_sin, [v] ->
    (match to_float_opt v with Some f -> Row.V_real (Float.sin f) | None -> Row.V_null)
  | Ast.Fn_cos, [v] ->
    (match to_float_opt v with Some f -> Row.V_real (Float.cos f) | None -> Row.V_null)
  | Ast.Fn_tan, [v] ->
    (match to_float_opt v with Some f -> Row.V_real (Float.tan f) | None -> Row.V_null)
  | Ast.Fn_asin, [v] ->
    (match to_float_opt v with Some f -> Row.V_real (Float.asin f) | None -> Row.V_null)
  | Ast.Fn_acos, [v] ->
    (match to_float_opt v with Some f -> Row.V_real (Float.acos f) | None -> Row.V_null)
  | Ast.Fn_atan, [v] ->
    (match to_float_opt v with Some f -> Row.V_real (Float.atan f) | None -> Row.V_null)
  | Ast.Fn_atan2, [y; x] ->
    (match to_float_opt y, to_float_opt x with
     | Some yf, Some xf -> Row.V_real (Float.atan2 yf xf)
     | _ -> Row.V_null)
  | Ast.Fn_degrees, [v] ->
    (match to_float_opt v with
     | Some f -> Row.V_real (f *. 180.0 /. Float.pi)
     | None -> Row.V_null)
  | Ast.Fn_radians, [v] ->
    (match to_float_opt v with
     | Some f -> Row.V_real (f *. Float.pi /. 180.0)
     | None -> Row.V_null)
  | Ast.Fn_json_extract, [json_v; path_v] ->
    let json_s = (match json_v with Row.V_text s -> s | _ -> "") in
    let path_s = (match path_v with Row.V_text s -> s | _ -> "") in
    (match Json.parse json_s with
     | Error _ -> Row.V_null
     | Ok jv   ->
       (match Json.path_get jv path_s with
        | None   -> Row.V_null
        | Some v -> sql_of_json v))
  | Ast.Fn_json_object, pairs ->
    if List.length pairs mod 2 <> 0 then Row.V_null
    else
      let rec make_pairs = function
        | []          -> []
        | k :: v :: rest ->
          let key = (match k with Row.V_text s -> s | _ -> "") in
          (key, json_of_sql v) :: make_pairs rest
        | [_]         -> assert false
      in
      Row.V_text (Json.to_string (Json.J_object (make_pairs pairs)))
  | Ast.Fn_json_array, elems ->
    Row.V_text (Json.to_string (Json.J_array (List.map json_of_sql elems)))
  | Ast.Fn_json_type, [json_v] ->
    (match json_v with
     | Row.V_text s ->
       (match Json.parse s with
        | Error _ -> Row.V_null
        | Ok jv   -> Row.V_text (Json.type_name jv))
     | _ -> Row.V_null)
  | Ast.Fn_json_type, [json_v; path_v] ->
    (match json_v, path_v with
     | Row.V_text s, Row.V_text path ->
       (match Json.parse s with
        | Error _ -> Row.V_null
        | Ok jv   ->
          (match Json.path_get jv path with
           | None    -> Row.V_null
           | Some sub -> Row.V_text (Json.type_name sub)))
     | _ -> Row.V_null)
  | Ast.Fn_json_valid, [json_v] ->
    (match json_v with
     | Row.V_null -> Row.V_null
     | Row.V_text s ->
       (match Json.parse s with Ok _ -> Row.V_int 1L | Error _ -> Row.V_int 0L)
     | _ -> Row.V_int 0L)
  | Ast.Fn_json_set, json_v :: rest ->
    let json_s = (match json_v with Row.V_text s -> s | _ -> "") in
    (match Json.parse json_s with
     | Error _ -> Row.V_null
     | Ok jv ->
       let rec apply jv = function
         | path_v :: val_v :: rest ->
           let path = (match path_v with Row.V_text s -> s | _ -> "") in
           apply (Json.path_set jv path (json_of_sql val_v)) rest
         | _ -> jv
       in
       Row.V_text (Json.to_string (apply jv rest)))
  | Ast.Fn_json_insert, json_v :: rest ->
    let json_s = (match json_v with Row.V_text s -> s | _ -> "") in
    (match Json.parse json_s with
     | Error _ -> Row.V_null
     | Ok jv ->
       let rec apply jv = function
         | path_v :: val_v :: rest ->
           let path = (match path_v with Row.V_text s -> s | _ -> "") in
           apply (Json.path_insert jv path (json_of_sql val_v)) rest
         | _ -> jv
       in
       Row.V_text (Json.to_string (apply jv rest)))
  | Ast.Fn_json_replace, json_v :: rest ->
    let json_s = (match json_v with Row.V_text s -> s | _ -> "") in
    (match Json.parse json_s with
     | Error _ -> Row.V_null
     | Ok jv ->
       let rec apply jv = function
         | path_v :: val_v :: rest ->
           let path = (match path_v with Row.V_text s -> s | _ -> "") in
           apply (Json.path_replace jv path (json_of_sql val_v)) rest
         | _ -> jv
       in
       Row.V_text (Json.to_string (apply jv rest)))
  | Ast.Fn_json_remove, json_v :: paths ->
    let json_s = (match json_v with Row.V_text s -> s | _ -> "") in
    (match Json.parse json_s with
     | Error _ -> Row.V_null
     | Ok jv ->
       let result = List.fold_left (fun acc path_v ->
         let path = (match path_v with Row.V_text s -> s | _ -> "") in
         Json.path_remove acc path
       ) jv paths in
       Row.V_text (Json.to_string result))
  (* ── HEX ──────────────────────────────────────────────────────── *)
  | Ast.Fn_hex, [Row.V_blob b] ->
    let buf = Buffer.create (Bytes.length b * 2) in
    Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02X" (Char.code c))) b;
    Row.V_text (Buffer.contents buf)
  | Ast.Fn_hex, [Row.V_text s] ->
    let buf = Buffer.create (String.length s * 2) in
    String.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02X" (Char.code c))) s;
    Row.V_text (Buffer.contents buf)
  | Ast.Fn_hex, [Row.V_int n] ->
    (* SQLite converts the integer to its decimal string representation, then hexes that *)
    let s = Int64.to_string n in
    let buf = Buffer.create (String.length s * 2) in
    String.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02X" (Char.code c))) s;
    Row.V_text (Buffer.contents buf)
  | Ast.Fn_hex, [Row.V_null] -> Row.V_text ""

  (* ── CHAR ──────────────────────────────────────────────────────── *)
  | Ast.Fn_char, args ->
    let buf = Buffer.create 16 in
    List.iter (fun v ->
      match v with
      | Row.V_int n when n >= 1L && n <= 0x10FFFFL ->
        let cp = Int64.to_int n in
        if cp < 0x80 then
          Buffer.add_char buf (Char.chr cp)
        else if cp < 0x800 then begin
          Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
          Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
        end else if cp < 0x10000 then begin
          Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
          Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
          Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
        end else begin
          Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
          Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
          Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
          Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
        end
      | _ -> ()
    ) args;
    Row.V_text (Buffer.contents buf)

  (* ── UNICODE ──────────────────────────────────────────────────── *)
  | Ast.Fn_unicode, [Row.V_text s] when String.length s > 0 ->
    let b0 = Char.code s.[0] in
    let cp =
      if b0 < 0x80 then b0
      else if b0 < 0xE0 && String.length s >= 2 then
        ((b0 land 0x1F) lsl 6) lor (Char.code s.[1] land 0x3F)
      else if b0 < 0xF0 && String.length s >= 3 then
        ((b0 land 0x0F) lsl 12)
        lor ((Char.code s.[1] land 0x3F) lsl 6)
        lor (Char.code s.[2] land 0x3F)
      else if b0 >= 0xF0 && String.length s >= 4 then
        ((b0 land 0x07) lsl 18)
        lor ((Char.code s.[1] land 0x3F) lsl 12)
        lor ((Char.code s.[2] land 0x3F) lsl 6)
        lor (Char.code s.[3] land 0x3F)
      else b0
    in
    Row.V_int (Int64.of_int cp)
  | Ast.Fn_unicode, [Row.V_text _] -> Row.V_null
  | Ast.Fn_unicode, [Row.V_null]   -> Row.V_null

  (* ── PRINTF / FORMAT ──────────────────────────────────────────── *)
  | Ast.Fn_printf, (Row.V_text fmt :: rest) ->
    let args_arr = Array.of_list rest in
    let arg_idx = ref 0 in
    let buf = Buffer.create 64 in
    let n = String.length fmt in
    let i = ref 0 in
    while !i < n do
      if fmt.[!i] = '%' then begin
        incr i;
        if !i < n then begin
          let get_arg () =
            let v = if !arg_idx < Array.length args_arr
                    then args_arr.(!arg_idx)
                    else Row.V_null in
            incr arg_idx; v
          in
          (match fmt.[!i] with
           | '%' -> Buffer.add_char buf '%'
           | 'd' | 'i' ->
             (match get_arg () with
              | Row.V_int  n2 -> Buffer.add_string buf (Int64.to_string n2)
              | Row.V_real f -> Buffer.add_string buf (string_of_int (int_of_float f))
              | Row.V_text s -> (try Buffer.add_string buf (string_of_int (int_of_string s))
                                 with _ -> ())
              | _ -> ())
           | 'f' ->
             (match get_arg () with
              | Row.V_real f -> Buffer.add_string buf (Printf.sprintf "%f" f)
              | Row.V_int  n2 -> Buffer.add_string buf (Printf.sprintf "%f" (Int64.to_float n2))
              | _ -> ())
           | 'e' ->
             (match get_arg () with
              | Row.V_real f -> Buffer.add_string buf (Printf.sprintf "%e" f)
              | Row.V_int  n2 -> Buffer.add_string buf (Printf.sprintf "%e" (Int64.to_float n2))
              | _ -> ())
           | 'g' ->
             (match get_arg () with
              | Row.V_real f -> Buffer.add_string buf (Printf.sprintf "%g" f)
              | Row.V_int  n2 -> Buffer.add_string buf (Printf.sprintf "%g" (Int64.to_float n2))
              | _ -> ())
           | 's' ->
             (match get_arg () with
              | Row.V_text s -> Buffer.add_string buf s
              | Row.V_int  n2 -> Buffer.add_string buf (Int64.to_string n2)
              | Row.V_real f -> Buffer.add_string buf (Printf.sprintf "%g" f)
              | Row.V_null   -> Buffer.add_string buf "NULL"
              | Row.V_blob _ -> Buffer.add_string buf "")
           | 'q' ->
             (match get_arg () with
              | Row.V_text s ->
                String.iter (fun c ->
                  if c = '\'' then Buffer.add_string buf "''"
                  else Buffer.add_char buf c) s
              | Row.V_int  n2 -> Buffer.add_string buf (Int64.to_string n2)
              | Row.V_real f -> Buffer.add_string buf (Printf.sprintf "%g" f)
              | Row.V_null   -> Buffer.add_string buf "NULL"
              | Row.V_blob _ -> ())
           | c ->
             Buffer.add_char buf '%';
             Buffer.add_char buf c);
          incr i
        end
      end else begin
        Buffer.add_char buf fmt.[!i];
        incr i
      end
    done;
    Row.V_text (Buffer.contents buf)
  | Ast.Fn_printf, _ -> Row.V_null

  (* ── ZEROBLOB ──────────────────────────────────────────────────── *)
  | Ast.Fn_zeroblob, [Row.V_int n] when n >= 0L ->
    Row.V_blob (Bytes.make (Int64.to_int n) '\000')
  | Ast.Fn_zeroblob, _ -> Row.V_null

  (* ── RANDOM ──────────────────────────────────────────────────── *)
  | Ast.Fn_random, [] ->
    let b0 = Int64.of_int (Random.bits ()) in
    let b1 = Int64.of_int (Random.bits ()) in
    let b2 = Int64.of_int (Random.bits ()) in
    let sign = if Random.bool () then Int64.min_int else 0L in
    let v =
      Int64.logor sign
        (Int64.logor
          (Int64.shift_left b2 60)
          (Int64.logor (Int64.shift_left b1 30) b0))
    in
    Row.V_int v
  | Ast.Fn_random, _ -> Row.V_null

  (* ── RANDOMBLOB ──────────────────────────────────────────────── *)
  (* SQLite always generates at least 1 byte, even for n <= 0.
     Clamp to [1, Sys.max_string_length] to avoid allocation errors. *)
  | Ast.Fn_randomblob, [Row.V_int n] ->
    let sz = max 1 (if n < 0L || n > Int64.of_int Sys.max_string_length
                    then 1 else Int64.to_int n) in
    Row.V_blob (Bytes.init sz (fun _ -> Char.chr (Random.int 256)))
  | Ast.Fn_randomblob, _ -> Row.V_null

  (* ── CHANGES / LAST_INSERT_ROWID fallback ─────────────────────── *)
  | Ast.Fn_changes, [] -> Row.V_int 0L
  | Ast.Fn_changes, _  -> Row.V_null
  | Ast.Fn_last_insert_rowid, [] -> Row.V_int 0L
  | Ast.Fn_last_insert_rowid, _  -> Row.V_null

  | _ ->
    failwith (Printf.sprintf "scalar_func: unexpected argument count (arity check should have caught this)")

and eval_binop (op : Plan.binop) (lv : Row.value) (rv : Row.value) : Row.value =
  match op with
  | Plan.And ->
    let lt = value_truthy lv and rt = value_truthy rv in
    let ln = lv = Row.V_null  and rn = rv = Row.V_null in
    if lt && rt then Row.V_int 1L
    else if (not ln && not lt) || (not rn && not rt) then Row.V_int 0L
    else Row.V_null
  | Plan.Or ->
    let lt = value_truthy lv and rt = value_truthy rv in
    let ln = lv = Row.V_null  and rn = rv = Row.V_null in
    if lt || rt then Row.V_int 1L
    else if not ln && not rn then Row.V_int 0L
    else Row.V_null
  (* NULL compared with anything yields NULL (3-valued logic). Cross-type → false. *)
  | Plan.Eq ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_int  x, Row.V_int  y -> if Int64.equal x y then Row.V_int 1L else Row.V_int 0L
     | Row.V_text x, Row.V_text y -> if String.equal x y then Row.V_int 1L else Row.V_int 0L
     | Row.V_real x, Row.V_real y -> if Float.equal  x y then Row.V_int 1L else Row.V_int 0L
     | Row.V_blob x, Row.V_blob y -> if Bytes.equal  x y then Row.V_int 1L else Row.V_int 0L
     | _                          -> Row.V_int 0L)
  | Plan.Ne ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
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
  | Plan.Concat ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_text a, Row.V_text b -> Row.V_text (a ^ b)
     | Row.V_text a, Row.V_int  n -> Row.V_text (a ^ Int64.to_string n)
     | Row.V_int  n, Row.V_text b -> Row.V_text (Int64.to_string n ^ b)
     | Row.V_int  a, Row.V_int  b -> Row.V_text (Int64.to_string a ^ Int64.to_string b)
     | _ -> Row.V_null)
  | Plan.Mod ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_int a, Row.V_int b ->
       if b = 0L then Row.V_null else Row.V_int (Int64.rem a b)
     | Row.V_real a, Row.V_real b ->
       if b = 0.0 then Row.V_null else Row.V_real (mod_float a b)
     | Row.V_int a, Row.V_real b ->
       if b = 0.0 then Row.V_null else Row.V_real (mod_float (Int64.to_float a) b)
     | Row.V_real a, Row.V_int b ->
       if b = 0L then Row.V_null else Row.V_real (mod_float a (Int64.to_float b))
     | _ -> Row.V_null)
  | Plan.Bit_and ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_int a, Row.V_int b -> Row.V_int (Int64.logand a b)
     | _ -> Row.V_null)
  | Plan.Bit_or ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_int a, Row.V_int b -> Row.V_int (Int64.logor a b)
     | _ -> Row.V_null)
  | Plan.Lshift ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_int a, Row.V_int b ->
       let n = Int64.to_int b in
       Row.V_int (if n < 0 || n >= 64 then 0L else Int64.shift_left a n)
     | _ -> Row.V_null)
  | Plan.Rshift ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_int a, Row.V_int b ->
       let n = Int64.to_int b in
       Row.V_int (if n < 0 || n >= 64 then 0L else Int64.shift_right a n)
     | _ -> Row.V_null)
  | Plan.Like ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_text str, Row.V_text pat ->
       Row.V_int (if like_match (String.lowercase_ascii pat) 0 (String.lowercase_ascii str) 0 then 1L else 0L)
     | _ -> Row.V_null)
  | Plan.Glob ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_text str, Row.V_text pat ->
       Row.V_int (if glob_match pat 0 str 0 then 1L else 0L)
     | _ -> Row.V_null)

and cmp_result lv rv pred =
  match lv, rv with
  | Row.V_null, _ | _, Row.V_null -> Row.V_null
  | Row.V_int _,  Row.V_int _
  | Row.V_text _, Row.V_text _
  | Row.V_real _, Row.V_real _
  | Row.V_blob _, Row.V_blob _ ->
    if pred (compare_values lv rv) then Row.V_int 1L else Row.V_int 0L
  (* Cross-type numeric comparisons: promote int to float *)
  | Row.V_real a, Row.V_int  b ->
    let c = Float.compare a (Int64.to_float b) in
    if pred c then Row.V_int 1L else Row.V_int 0L
  | Row.V_int  a, Row.V_real b ->
    let c = Float.compare (Int64.to_float a) b in
    if pred c then Row.V_int 1L else Row.V_int 0L
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
(* CHECK constraint evaluation                                          *)
(* ------------------------------------------------------------------ *)

let ast_binop_to_plan : Ast.binop -> Plan.binop = function
  | Ast.Eq -> Plan.Eq | Ast.Ne -> Plan.Ne | Ast.Lt -> Plan.Lt | Ast.Le -> Plan.Le
  | Ast.Gt -> Plan.Gt | Ast.Ge -> Plan.Ge
  | Ast.Add -> Plan.Add | Ast.Sub -> Plan.Sub
  | Ast.Mul -> Plan.Mul | Ast.Div -> Plan.Div
  | Ast.And -> Plan.And | Ast.Or  -> Plan.Or
  | Ast.Concat -> Plan.Concat | Ast.Mod -> Plan.Mod
  | Ast.Bit_and -> Plan.Bit_and | Ast.Bit_or -> Plan.Bit_or
  | Ast.Lshift  -> Plan.Lshift  | Ast.Rshift -> Plan.Rshift
  | Ast.Like -> Plan.Like | Ast.Glob -> Plan.Glob

let rec ast_expr_to_plan_check (columns : Row.column list) (e : Ast.expr) : Plan.expr =
  match e with
  | Ast.E_lit l       -> Plan.P_lit l
  | Ast.E_col name    -> Plan.P_col (find_col_idx_by_name columns name)
  | Ast.E_tbl_col (_, name) -> Plan.P_col (find_col_idx_by_name columns name)
  | Ast.E_binop (op, a, b) ->
    Plan.P_binop (ast_binop_to_plan op,
                  ast_expr_to_plan_check columns a,
                  ast_expr_to_plan_check columns b)
  | Ast.E_not e      -> Plan.P_not (ast_expr_to_plan_check columns e)
  | Ast.E_is_null e  -> Plan.P_is_null (ast_expr_to_plan_check columns e)
  | Ast.E_is_not_null e -> Plan.P_is_not_null (ast_expr_to_plan_check columns e)
  | Ast.E_neg e      -> Plan.P_neg (ast_expr_to_plan_check columns e)
  | Ast.E_bitnot e   -> Plan.P_bitnot (ast_expr_to_plan_check columns e)
  | Ast.E_between (x, lo, hi) ->
    Plan.P_between (ast_expr_to_plan_check columns x,
                    ast_expr_to_plan_check columns lo,
                    ast_expr_to_plan_check columns hi)
  | Ast.E_in (x, vals) ->
    Plan.P_in (ast_expr_to_plan_check columns x,
               List.map (ast_expr_to_plan_check columns) vals)
  | Ast.E_func (f, args) ->
    Plan.P_func (f, List.map (ast_expr_to_plan_check columns) args)
  | Ast.E_case { scrutinee; branches; else_ } ->
    let go = ast_expr_to_plan_check columns in
    Plan.P_case {
      scrutinee = Option.map go scrutinee;
      branches  = List.map (fun (c, r) -> (go c, go r)) branches;
      else_     = Option.map go else_;
    }
  | Ast.E_cast (e, ty) -> Plan.P_cast (ast_expr_to_plan_check columns e, ty)
  | Ast.E_collate (e, c) -> Plan.P_collate (ast_expr_to_plan_check columns e, c)
  | _ -> failwith "ast_expr_to_plan_check: unsupported expression in CHECK"

let compile_check_expr (table_name : string) (col_idx : int)
    (columns : Row.column list) (check_sql : string) : Plan.expr =
  let key = (table_name, col_idx, check_sql) in
  match Hashtbl.find_opt check_expr_cache key with
  | Some e -> e
  | None ->
    let lexbuf = Lexing.from_string check_sql in
    let ast_expr =
      try Parser.expr_only Lexer.token lexbuf
      with _ ->
        failwith (Printf.sprintf "CHECK constraint parse error for %s.col%d: %s"
          table_name col_idx check_sql)
    in
    let plan_expr = ast_expr_to_plan_check columns ast_expr in
    Hashtbl.add check_expr_cache key plan_expr;
    plan_expr

(* Cache for compiled generated-column expressions.
   Key: (table_name, col_idx, expr_sql) — same three-part pattern as check_expr_cache.
   Schema changes invalidate entries via clear on DROP TABLE / DROP COLUMN. *)
let generated_expr_cache : (string * int * string, Plan.expr) Hashtbl.t = Hashtbl.create 8

let compile_generated_expr (table_name : string) (col_idx : int)
    (columns : Row.column list) (expr_sql : string) : Plan.expr =
  let key = (table_name, col_idx, expr_sql) in
  match Hashtbl.find_opt generated_expr_cache key with
  | Some e -> e
  | None ->
    let lexbuf = Lexing.from_string expr_sql in
    let ast_expr =
      try Parser.expr_only Lexer.token lexbuf
      with _ -> failwith (Printf.sprintf
        "generated column expr parse error for %s.col%d: %s" table_name col_idx expr_sql)
    in
    let plan_expr = ast_expr_to_plan_check columns ast_expr in
    Hashtbl.add generated_expr_cache key plan_expr;
    plan_expr

(** Compute all generated columns in [row] in-place.
    Iterates columns in schema order; earlier generated columns are available
    to later generated column expressions (in-order dependency). *)
let compute_generated_cols
    (clock : (unit -> float) option)
    (params : Row.value array)
    (meta : Cat.table_meta)
    (row : Row.t) : unit =
  List.iteri (fun i (col : Row.column) ->
    match col.Row.generated_as with
    | None -> ()
    | Some (sql, _is_stored) ->
      let plan_e = compile_generated_expr meta.Cat.name i meta.Cat.columns sql in
      row.(i) <- eval_expr clock params row plan_e
  ) meta.Cat.columns

let index_where_cache : (string * string * string * string, Plan.expr) Hashtbl.t = Hashtbl.create 8

let compile_index_where (idx : Cat.index_info) (columns : Row.column list) : Plan.expr =
  match idx.idx_where_sql with
  | None -> failwith "compile_index_where: called on non-partial index"
  | Some sql ->
    let schema_sig = String.concat "," (List.map (fun c -> c.Row.name) columns) in
    let key = (idx.idx_name, idx.idx_table, sql, schema_sig) in
    match Hashtbl.find_opt index_where_cache key with
    | Some e -> e
    | None ->
      let lexbuf = Lexing.from_string sql in
      let ast_expr =
        try Parser.expr_only Lexer.token lexbuf
        with _ -> failwith (Printf.sprintf "index WHERE parse error for %s: %s" idx.idx_name sql)
      in
      let plan_expr = ast_expr_to_plan_check columns ast_expr in
      Hashtbl.add index_where_cache key plan_expr;
      plan_expr

let row_matches_index_where
    (clock : (unit -> float) option)
    (params : Row.value array)
    (idx : Cat.index_info)
    (schema : Row.column list)
    (row : Row.t) : bool =
  match idx.idx_where_sql with
  | None -> true
  | Some _ ->
    let plan_e = compile_index_where idx schema in
    value_truthy (eval_expr clock params row plan_e)

(* Cache for compiled index column expressions.
   Key: (idx_name, idx_table, expr_sql, schema_sig) — four parts to prevent collisions. *)
let index_expr_cache : (string * string * string * string, Plan.expr) Hashtbl.t =
  Hashtbl.create 8

let compile_index_col_expr (idx : Cat.index_info) (i : int) (columns : Row.column list) : Plan.expr =
  let expr_sql = List.nth idx.idx_columns i in
  let schema_sig = String.concat "," (List.map (fun c -> c.Row.name) columns) in
  let key = (idx.idx_name, idx.idx_table, expr_sql, schema_sig) in
  match Hashtbl.find_opt index_expr_cache key with
  | Some e -> e
  | None ->
    let lexbuf = Lexing.from_string expr_sql in
    let ast_expr =
      try Parser.expr_only Lexer.token lexbuf
      with _ -> failwith (Printf.sprintf
        "index expr parse error for %s[%d]: %s" idx.idx_name i expr_sql)
    in
    let plan_e = ast_expr_to_plan_check columns ast_expr in
    Hashtbl.add index_expr_cache key plan_e;
    plan_e

(** Evaluate all index key values for [row] against [idx].
    For expression-indexed columns, evaluates the compiled expression.
    For plain columns, fetches from the row by column ordinal. *)
let get_index_key_values
    (clock : (unit -> float) option)
    (params : Row.value array)
    (idx : Cat.index_info)
    (schema : Row.column list)
    (row : Row.t) : Row.value list =
  List.mapi (fun i col_sql ->
    let is_expr =
      if i < List.length idx.idx_expr_flags
      then List.nth idx.idx_expr_flags i
      else false
    in
    if is_expr then
      let plan_e = compile_index_col_expr idx i schema in
      eval_expr clock params row plan_e
    else
      let col_idx = find_col_idx_by_name schema col_sql in
      row.(col_idx)
  ) idx.idx_columns

let eval_check_constraints
    (clock : (unit -> float) option)
    (params : Row.value array)
    (table_meta : Cat.table_meta)
    (row : Row.t) : unit =
  List.iteri (fun i (col : Row.column) ->
    match col.check_sql with
    | None -> ()
    | Some check_sql ->
      let check_plan = compile_check_expr table_meta.name i table_meta.columns check_sql in
      let result = eval_expr clock params row check_plan in
      (* SQLite: NULL result -> passes (not a violation) *)
      if result <> Row.V_null && not (value_truthy result) then
        failwith (Printf.sprintf "CHECK constraint failed: %s.%s" table_meta.name col.name)
  ) table_meta.columns

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

(** Replace every [P_excluded_col i] with [P_lit (value_to_literal excluded_row.(i))].
    Used to materialise UPSERT excluded-row refs before [eval_expr]. *)
let rec substitute_excluded (excluded_row : Row.t) (e : Plan.expr) : Plan.expr =
  match e with
  | Plan.P_excluded_col i -> Plan.P_lit (value_to_literal excluded_row.(i))
  | Plan.P_binop (op, a, b) ->
    Plan.P_binop (op, substitute_excluded excluded_row a, substitute_excluded excluded_row b)
  | Plan.P_not e      -> Plan.P_not (substitute_excluded excluded_row e)
  | Plan.P_is_null e  -> Plan.P_is_null (substitute_excluded excluded_row e)
  | Plan.P_is_not_null e -> Plan.P_is_not_null (substitute_excluded excluded_row e)
  | Plan.P_neg e      -> Plan.P_neg (substitute_excluded excluded_row e)
  | Plan.P_bitnot e   -> Plan.P_bitnot (substitute_excluded excluded_row e)
  | Plan.P_between (x, lo, hi) ->
    Plan.P_between (substitute_excluded excluded_row x,
                    substitute_excluded excluded_row lo,
                    substitute_excluded excluded_row hi)
  | Plan.P_in (x, vals) ->
    Plan.P_in (substitute_excluded excluded_row x,
               List.map (substitute_excluded excluded_row) vals)
  | Plan.P_func (f, args) ->
    Plan.P_func (f, List.map (substitute_excluded excluded_row) args)
  | Plan.P_case { scrutinee; branches; else_ } ->
    let go = substitute_excluded excluded_row in
    Plan.P_case {
      scrutinee = Option.map go scrutinee;
      branches  = List.map (fun (c, r) -> (go c, go r)) branches;
      else_     = Option.map go else_;
    }
  | Plan.P_cast (e, ty) -> Plan.P_cast (substitute_excluded excluded_row e, ty)
  | Plan.P_collate (e, c) -> Plan.P_collate (substitute_excluded excluded_row e, c)
  | other -> other

(** Run [Op_insert] against the store: write the new row to the table
    tree and, if any indexes are defined on the table, also write the
    corresponding index entries (checking UNIQUE constraints first).
    Uses a SINGLE RW txn for both the row write and index writes. *)
let execute_insert ?(mode = Auto) ?(params = [||])
    ?(clock : (unit -> float) option = None)
    ?(on_conflict : Ast.conflict_action option = None)
    ?(upsert_update : (string list * (int * Plan.expr) list) option = None)
    ?(prebuilt_row : Row.t option = None)
    ?(before_hook : (new_row:Row.t -> unit Lwt.t) option = None)
    ?(after_hook  : (new_row:Row.t -> unit Lwt.t) option = None)
    (store : S.t) (cat : Cat.t)
    ~(table_meta : Cat.table_meta) ~ordinals ~(values : Plan.expr list) : bool Lwt.t =
  let n   = List.length table_meta.columns in
  let row = match prebuilt_row with
    | Some r -> r
    | None ->
      let r = Array.make n Row.V_null in
      List.iter2 (fun ord expr -> r.(ord) <- eval_expr clock params [||] expr) ordinals values;
      r
  in
  compute_generated_cols clock params table_meta row;
  (* Evaluate CHECK constraints before any writes. *)
  eval_check_constraints clock params table_meta row;
  (* Evaluate FK constraints before any writes. *)
  let* () =
    let fks = table_meta.Cat.fk_constraints in
    if fks = [] || not (Cat.get_fk_enforcement cat) then Lwt.return_unit
    else
      let find_col_idx cols col_name =
        let rec fi i = function
          | [] -> None
          | (c : Row.column) :: _ when String.equal c.name col_name -> Some i
          | _ :: rest -> fi (i + 1) rest
        in fi 0 cols
      in
      Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
        match find_col_idx table_meta.Cat.columns fk.fk_local_col with
        | None -> Lwt.return_unit
        | Some local_idx ->
          let v = row.(local_idx) in
          (match v with
           | Row.V_null -> Lwt.return_unit  (* NULL FK is always valid *)
           | fk_val ->
             (match Cat.find_table_cached cat ~name:fk.fk_parent_table with
              | None ->
                Lwt.fail_with (Printf.sprintf "FOREIGN KEY: parent table '%s' not found"
                                 fk.fk_parent_table)
              | Some parent_meta ->
                let* parent_col_idx =
                  match find_col_idx parent_meta.Cat.columns fk.fk_parent_col with
                  | Some i -> Lwt.return i
                  | None ->
                    Lwt.fail_with (Printf.sprintf
                      "FOREIGN KEY: column '%s' not found in parent table '%s'"
                      fk.Cat.fk_parent_col fk.Cat.fk_parent_table)
                in
                let* ro_tx = S.ro_begin store in
                let* cur = S.cursor_open ro_tx parent_meta.Cat.tree_id in
                let _sr = S.cursor_first cur in
                let found = ref false in
                let rec scan () =
                  if !found then ()
                  else match S.cursor_next cur with
                  | None -> ()
                  | Some (_k, vbytes) ->
                    let parent_row = Row.decode parent_meta.Cat.columns vbytes in
                    if compare_values parent_row.(parent_col_idx) fk_val = 0 then
                      found := true
                    else scan ()
                in
                scan ();
                S.cursor_close cur;
                let* () = S.ro_end ro_tx in
                if !found then Lwt.return_unit
                else Lwt.fail_with (Printf.sprintf
                       "FOREIGN KEY constraint failed: no row in '%s' where %s matches"
                       fk.fk_parent_table fk.fk_parent_col)))
      ) fks
  in
  (* Fire BEFORE INSERT triggers *)
  let* () = match before_hook with None -> Lwt.return_unit | Some f -> f ~new_row:(Array.copy row) in
  (* When an explicit transaction is already held, we must NOT call
     Cat.next_rowid (which opens its own RW txn and deadlocks on the
     mutex).  Instead acquire/reuse the txn first, then update the
     rowid counter within that same txn. *)
  let* (tx, owned) = acquire_txn store mode in
  Lwt.catch
    (fun () ->
      let* rowid = Cat.next_rowid_in_txn cat ~name:table_meta.name tx in
      let idxs   = Cat.indexes_for_table cat ~table:table_meta.name in
      (* Phase 1: check UNIQUE constraints BEFORE writing the row.
         Collect skip flag, list of conflicting rowids to delete, and
         the rowid to update in-place for UPSERT. *)
      let* (skip, to_delete, upsert_rowid) =
        Lwt_list.fold_left_s (fun (skip, dels, upsert_rid) (idx : Cat.index_info) ->
          if skip || not idx.idx_unique then Lwt.return (skip, dels, upsert_rid)
          else if not (row_matches_index_where clock params idx table_meta.columns row)
          then Lwt.return (skip, dels, upsert_rid)
          else begin
            let iks    = List.map row_value_to_index_value
                           (get_index_key_values clock params idx table_meta.columns row) in
            let prefix =
              let buf = Buffer.create 32 in
              List.iter (fun ikv -> Buffer.add_bytes buf (Index_key.encode_value ikv)) iks;
              Buffer.to_bytes buf
            in
            let plen     = Bytes.length prefix in
            let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
            let* cur     = S.cursor_open tx idx.idx_tree_id in
            let _        = S.cursor_seek cur seek_key in
            let conflict_rowid_opt =
              match S.cursor_next cur with
              | None -> None
              | Some (ikey, _) ->
                if Bytes.length ikey >= plen &&
                   Bytes.equal (Bytes.sub ikey 0 plen) prefix
                then
                  let rid_bytes = Bytes.sub ikey plen (Bytes.length ikey - plen) in
                  Some (Rowid.decode rid_bytes)
                else None
            in
            S.cursor_close cur;
            match conflict_rowid_opt with
            | None -> Lwt.return (false, dels, upsert_rid)
            | Some old_rowid ->
              (match on_conflict, upsert_update with
               | Some Ast.CA_ignore, _ ->
                 Lwt.return (true, dels, upsert_rid)  (* skip=true, stop checking *)
               | Some Ast.CA_replace, _ ->
                 Lwt.return (false, old_rowid :: dels, upsert_rid)
               | _, Some (conflict_cols, _) when
                   List.sort String.compare idx.idx_columns =
                   List.sort String.compare conflict_cols ->
                 Lwt.return (false, dels, Some old_rowid)
               | _ ->
                 Lwt.fail_with (Printf.sprintf
                   "UNIQUE constraint violated: duplicate value in columns (%s)"
                   (String.concat ", " idx.idx_columns)))
          end
        ) (false, [], None) idxs
      in
      match upsert_update, upsert_rowid with
      | Some (_, assigns), Some old_rowid ->
        let old_key = Rowid.encode old_rowid in
        let* old_bytes_opt = S.get tx table_meta.tree_id old_key in
        (match old_bytes_opt with
         | None ->
           let* () = if owned then S.rollback tx else Lwt.return_unit in
           Lwt.return false
         | Some old_bytes ->
           let old_row = Row.decode table_meta.columns old_bytes in
           let new_row = Array.copy old_row in
           List.iter (fun (col_ord, expr) ->
             let e' = substitute_excluded row expr in
             new_row.(col_ord) <- eval_expr clock params old_row e'
           ) assigns;
           compute_generated_cols clock params table_meta new_row;
           eval_check_constraints clock params table_meta new_row;
           let idxs2 = Cat.indexes_for_table cat ~table:table_meta.name in
           let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
             let old_matches = row_matches_index_where clock params idx table_meta.columns old_row in
             let new_matches = row_matches_index_where clock params idx table_meta.columns new_row in
             let old_iks = List.map row_value_to_index_value
                             (get_index_key_values clock params idx table_meta.columns old_row) in
             let new_iks = List.map row_value_to_index_value
                             (get_index_key_values clock params idx table_meta.columns new_row) in
             let old_ikey = Index_key.encode old_iks ~rowid:old_rowid in
             let new_ikey = Index_key.encode new_iks ~rowid:old_rowid in
             let* () = if old_matches then S.del tx idx.idx_tree_id old_ikey else Lwt.return_unit in
             if new_matches then S.put tx idx.idx_tree_id new_ikey Bytes.empty
             else Lwt.return_unit
           ) idxs2 in
           let new_bytes = Row.encode table_meta.columns new_row in
           let* () = S.del tx table_meta.tree_id old_key in
           let* () = S.put tx table_meta.tree_id old_key new_bytes in
           let* () = release_txn tx owned in
           let* () = match after_hook with None -> Lwt.return_unit | Some f -> f ~new_row in
           Lwt.return true)
      | _ ->
        (* Normal path: skip, replace, or plain insert *)
        if skip then begin
          (* IGNORE: rollback if we own the txn (undo rowid allocation), return false *)
          let* () = if owned then S.rollback tx else Lwt.return_unit in
          Lwt.return false
        end else begin
          (* REPLACE: delete all conflicting rows first *)
          let* () = Lwt_list.iter_s (fun old_rowid ->
            let old_key = Rowid.encode old_rowid in
            let* old_bytes_opt = S.get tx table_meta.tree_id old_key in
            match old_bytes_opt with
            | None -> Lwt.return_unit
            | Some old_bytes ->
              let old_row = Row.decode table_meta.columns old_bytes in
              let* () = S.del tx table_meta.tree_id old_key in
              Lwt_list.iter_s (fun (idx2 : Cat.index_info) ->
                if not (row_matches_index_where clock params idx2 table_meta.columns old_row)
                then Lwt.return_unit
                else begin
                  let iks2    = List.map row_value_to_index_value
                                  (get_index_key_values clock params idx2 table_meta.columns old_row) in
                  let old_ikey = Index_key.encode iks2 ~rowid:old_rowid in
                  S.del tx idx2.idx_tree_id old_ikey
                end
              ) idxs
          ) (List.sort_uniq compare to_delete) in
          (* Phase 2: write new row and index entries *)
          let key   = Rowid.encode rowid in
          let bytes = Row.encode table_meta.columns row in
          let* () = S.put tx table_meta.tree_id key bytes in
          let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
            if not (row_matches_index_where clock params idx table_meta.columns row)
            then Lwt.return_unit
            else begin
              let iks    = List.map row_value_to_index_value
                             (get_index_key_values clock params idx table_meta.columns row) in
              let ikey   = Index_key.encode iks ~rowid in
              S.put tx idx.idx_tree_id ikey Bytes.empty
            end
          ) idxs in
          let* () = release_txn tx owned in
          let* () = match after_hook with None -> Lwt.return_unit | Some f -> f ~new_row:row in
          Lwt.return true
        end)
    (fun exn ->
      (* On any exception: rollback if we own the txn, then re-raise. *)
      let* () = if owned then S.rollback tx else Lwt.return_unit in
      Lwt.fail exn)

(** Run [Op_create_index]: register the index in the catalog, then scan
    the table tree and populate the index tree with one entry per row. *)
let execute_create_index ?(mode = Auto) (store : S.t) (cat : Cat.t)
    ~name ~table ~tree_id
    ~col_sqls
    ~col_expr_flags
    ~(where_expr : Plan.expr option)
    ~where_sql
    ~unique
    ~(columns : Row.column list) : unit Lwt.t =
  let* res = Cat.create_index cat ~name ~table ~columns:col_sqls
      ~unique ~expr_flags:col_expr_flags ~where_sql in
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
            let skip = match where_expr with
              | None -> false
              | Some we -> not (value_truthy (eval_expr None [||] row we))
            in
            if skip then walk ()
            else begin
              let iks = List.map row_value_to_index_value
                          (get_index_key_values None [||] info columns row) in
              let ikey = Index_key.encode iks ~rowid in
              let* () = S.put tx info.idx_tree_id ikey Bytes.empty in
              walk ()
            end
        in
        let* () = walk () in
        S.cursor_close cur;
        release_txn tx owned)
      (fun exn ->
        (* On any exception: rollback if we own the txn, then re-raise. *)
        let* () = if owned then S.rollback tx else Lwt.return_unit in
        Lwt.fail exn)

(** Check whether inserting a new index entry for [new_row] with
    [rowid] into [idx] would violate a UNIQUE constraint.  Returns
    [true] if a different row already has the same indexed value. *)
let unique_violation_on_update
    (tx : S.rw S.txn)
    (idx : Cat.index_info)
    (_new_values : Row.value list)    (* kept for call-site compat but unused for expr indexes *)
    ~(rowid : int64)
    ~(new_row : Row.t)
    ~(schema : Row.column list) : bool Lwt.t =
  (* For UNIQUE check we use the first value as the seek prefix.
     This is a conservative approach: we seek to the first key with the
     matching first-column value, then compare the entire encoded key. *)
  let ik_values = List.map row_value_to_index_value
                    (get_index_key_values None [||] idx schema new_row) in
  let full_key_no_rowid =
    (* Encode all values without rowid to use as a prefix for exact match *)
    let buf = Buffer.create 32 in
    List.iter (fun ikv ->
      Buffer.add_bytes buf (Index_key.encode_value ikv)
    ) ik_values;
    Buffer.to_bytes buf
  in
  let prefix = match ik_values with
    | [] -> Bytes.empty
    | ik :: _ -> Index_key.encode_value ik
  in
  let plen     = Bytes.length prefix in
  let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
  let* cur = S.cursor_open tx idx.idx_tree_id in
  let _sr = S.cursor_seek cur seek_key in
  (* Scan entries while the value prefix matches.  A different rowid
     with the same full value sequence is a UNIQUE violation. *)
  let full_klen = Bytes.length full_key_no_rowid in
  let rec scan () =
    match S.cursor_next cur with
    | None -> Lwt.return false
    | Some (ikey, _) ->
      if Bytes.length ikey >= plen + 8 &&
         Bytes.equal (Bytes.sub ikey 0 plen) prefix
      then begin
        (* Check that the full value prefix (all columns) also matches *)
        if Bytes.length ikey >= full_klen + 8 &&
           Bytes.equal (Bytes.sub ikey 0 full_klen) full_key_no_rowid
        then begin
          let rowid_bytes = Bytes.sub ikey (Bytes.length ikey - 8) 8 in
          let other = Rowid.decode rowid_bytes in
          if Int64.equal other rowid then scan ()
          else Lwt.return true
        end else
          scan ()
      end else
        Lwt.return false
  in
  let* result = scan () in
  S.cursor_close cur;
  Lwt.return result

(** Build the list of (child_table_meta, relevant_fk_constraints) pairs
    for tables that have FK constraints pointing to [parent_table_name]. *)
let build_child_refs cat ~parent_table_name =
  let* all_tables = Cat.list_tables cat in
  Lwt.return (List.filter_map (fun (child_meta : Cat.table_meta) ->
    let fks = List.filter (fun (fk : Cat.fk_constraint) ->
      String.equal fk.fk_parent_table parent_table_name
    ) child_meta.Cat.fk_constraints in
    if fks = [] then None else Some (child_meta, fks)
  ) all_tables)

(** Scan [child_meta] for any row where [child_col_idx] equals [parent_val].
    Opens and closes its own RO snapshot. *)
let fk_child_has_ref store (child_meta : Cat.table_meta) ~child_col_idx ~(parent_val : Row.value) =
  let schema = child_meta.Cat.columns in
  let* ro_tx = S.ro_begin store in
  let* cur   = S.cursor_open ro_tx child_meta.Cat.tree_id in
  let _sr    = S.cursor_first cur in
  let found  = ref false in
  let rec scan () =
    if !found then ()
    else match S.cursor_next cur with
    | None -> ()
    | Some (_k, vbytes) ->
      let row = Row.decode schema vbytes in
      if compare_values row.(child_col_idx) parent_val = 0 then
        found := true
      else scan ()
  in
  scan ();
  S.cursor_close cur;
  let* () = S.ro_end ro_tx in
  Lwt.return !found

(** Scan [child_meta] using an existing RW transaction for rows where
    [child_col_idx] equals [parent_val]. Returns (rowid, row) list. *)
let scan_child_rows_tx tx (child_meta : Cat.table_meta) ~child_col_idx ~(parent_val : Row.value) =
  let schema = child_meta.Cat.columns in
  let* cur   = S.cursor_open tx child_meta.Cat.tree_id in
  let _sr    = S.cursor_first cur in
  let buf    = ref [] in
  let rec scan () =
    match S.cursor_next cur with
    | None -> ()
    | Some (kbytes, vbytes) ->
      let rowid = Rowid.decode kbytes in
      let row   = Row.decode schema vbytes in
      if compare_values row.(child_col_idx) parent_val = 0 then
        buf := (rowid, row) :: !buf;
      scan ()
  in
  scan ();
  S.cursor_close cur;
  Lwt.return (List.rev !buf)

(** Delete a single row and its index entries within an existing RW transaction. *)
let delete_row_in_tx tx (cat : Cat.t) (meta : Cat.table_meta) ~rowid ~(row : Row.t) =
  let rowid_key  = Rowid.encode rowid in
  let child_idxs = Cat.indexes_for_table cat ~table:meta.Cat.name in
  let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
    if not (row_matches_index_where None [||] idx meta.Cat.columns row)
    then Lwt.return_unit
    else begin
      let iks      = List.map row_value_to_index_value
                       (get_index_key_values None [||] idx meta.Cat.columns row) in
      let old_ikey = Index_key.encode iks ~rowid in
      S.del tx idx.idx_tree_id old_ikey
    end
  ) child_idxs in
  S.del tx meta.Cat.tree_id rowid_key

(** Update one column to [new_val] in a row within an existing RW transaction.
    Also updates index entries for any index that covers [col_idx]. *)
let update_col_in_tx tx (cat : Cat.t) (meta : Cat.table_meta) ~rowid ~(row : Row.t) ~col_idx ~new_val =
  let schema     = meta.Cat.columns in
  let rowid_key  = Rowid.encode rowid in
  let new_row    = Array.copy row in
  new_row.(col_idx) <- new_val;
  compute_generated_cols None [||] meta new_row;
  let child_idxs = Cat.indexes_for_table cat ~table:meta.Cat.name in
  let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
    let has_where = idx.idx_where_sql <> None in
    (* For expression indexes, we always update (can't cheaply determine dependency).
       For plain indexes, only skip if col is not indexed AND no WHERE clause. *)
    let has_expr_col = List.exists Fun.id idx.idx_expr_flags in
    let col_is_plain = List.filter_map Fun.id (List.mapi (fun i col_sql ->
      let is_expr = if i < List.length idx.idx_expr_flags
                    then List.nth idx.idx_expr_flags i else false in
      if is_expr then None
      else Some (find_col_idx_by_name schema col_sql)
    ) idx.idx_columns) in
    (* Only skip if: no expr cols, col is not in plain indexed cols, no WHERE *)
    if not has_expr_col && not (List.mem col_idx col_is_plain) && not has_where
    then Lwt.return_unit
    else begin
      let old_matches = row_matches_index_where None [||] idx schema row in
      let new_matches = row_matches_index_where None [||] idx schema new_row in
      let old_iks  = List.map row_value_to_index_value
                       (get_index_key_values None [||] idx schema row) in
      let new_iks  = List.map row_value_to_index_value
                       (get_index_key_values None [||] idx schema new_row) in
      let old_ikey = Index_key.encode old_iks ~rowid in
      let new_ikey = Index_key.encode new_iks ~rowid in
      let* () = if old_matches then S.del tx idx.idx_tree_id old_ikey else Lwt.return_unit in
      if new_matches then S.put tx idx.idx_tree_id new_ikey Bytes.empty
      else Lwt.return_unit
    end
  ) child_idxs in
  let new_bytes = Row.encode schema new_row in
  S.put tx meta.Cat.tree_id rowid_key new_bytes

(** Recursively delete a row and cascade FK actions to child tables.
    Only runs cascade logic when FK enforcement is enabled in [cat]. *)
let rec cascade_delete_row_in_tx tx (cat : Cat.t)
    (clock : (unit -> float) option) (params : Row.value array)
    (meta : Cat.table_meta) ~rowid ~(row : Row.t) =
  let* child_refs =
    if Cat.get_fk_enforcement cat then
      build_child_refs cat ~parent_table_name:meta.Cat.name
    else Lwt.return []
  in
  let* () =
    Lwt_list.iter_s (fun (child_meta, fks) ->
      Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
        let parent_col_idx =
          find_col_idx_by_name meta.Cat.columns fk.Cat.fk_parent_col
        in
        let parent_val = row.(parent_col_idx) in
        match parent_val with
        | Row.V_null -> Lwt.return_unit
        | _ ->
          let child_col_idx =
            find_col_idx_by_name child_meta.Cat.columns fk.Cat.fk_local_col
          in
          (match fk.Cat.fk_on_delete with
           | Cat.FA_restrict | Cat.FA_no_action ->
             let* child_rows =
               scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val
             in
             if child_rows <> [] then
               Lwt.fail_with (Printf.sprintf
                 "FOREIGN KEY constraint failed: '%s.%s' is still \
                  referenced by '%s.%s'"
                 meta.Cat.name fk.Cat.fk_parent_col
                 child_meta.Cat.name fk.Cat.fk_local_col)
             else Lwt.return_unit
           | Cat.FA_cascade ->
             let* child_rows =
               scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val
             in
             Lwt_list.iter_s (fun (crid, crow) ->
               cascade_delete_row_in_tx tx cat clock params
                 child_meta ~rowid:crid ~row:crow
             ) child_rows
           | Cat.FA_set_null ->
             let* child_rows =
               scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val
             in
             if child_rows = [] then Lwt.return_unit
             else begin
               let col = List.nth child_meta.Cat.columns child_col_idx in
               if col.Row.not_null then
                 Lwt.fail_with (Printf.sprintf
                   "FOREIGN KEY constraint failed: ON DELETE SET NULL on NOT NULL column '%s.%s'"
                   child_meta.Cat.name fk.Cat.fk_local_col)
               else
                 Lwt_list.iter_s (fun (crid, crow) ->
                   update_col_in_tx tx cat child_meta ~rowid:crid ~row:crow
                     ~col_idx:child_col_idx ~new_val:Row.V_null
                 ) child_rows
             end
           | Cat.FA_set_default ->
             let* child_rows =
               scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val
             in
             if child_rows = [] then Lwt.return_unit
             else begin
               let col = List.nth child_meta.Cat.columns child_col_idx in
               let default_val = match col.Row.default with
                 | None               -> Row.V_null
                 | Some Row.DV_int  n -> Row.V_int  n
                 | Some Row.DV_text s -> Row.V_text s
                 | Some Row.DV_real f -> Row.V_real f
                 | Some Row.DV_blob b -> Row.V_blob b
                 | Some Row.DV_null   -> Row.V_null
                 | Some Row.DV_current_timestamp ->
                   eval_expr clock params [||]
                     (Plan.P_func (Ast.Fn_datetime,
                        [Plan.P_lit (Ast.L_text "now")]))
                 | Some Row.DV_current_date ->
                   eval_expr clock params [||]
                     (Plan.P_func (Ast.Fn_date,
                        [Plan.P_lit (Ast.L_text "now")]))
                 | Some Row.DV_current_time ->
                   eval_expr clock params [||]
                     (Plan.P_func (Ast.Fn_time,
                        [Plan.P_lit (Ast.L_text "now")]))
               in
               if col.Row.not_null && default_val = Row.V_null then
                 Lwt.fail_with (Printf.sprintf
                   "FOREIGN KEY constraint failed: ON DELETE SET DEFAULT on NOT NULL column '%s.%s' with no default"
                   child_meta.Cat.name fk.Cat.fk_local_col)
               else
                 Lwt_list.iter_s (fun (crid, crow) ->
                   update_col_in_tx tx cat child_meta ~rowid:crid ~row:crow
                     ~col_idx:child_col_idx ~new_val:default_val
                 ) child_rows
             end)
      ) fks
    ) child_refs
  in
  delete_row_in_tx tx cat meta ~rowid ~row

(** Recursively update a column and cascade FK UPDATE actions to child tables
    that reference this column. *)
and cascade_update_col_in_tx tx (cat : Cat.t)
    (clock : (unit -> float) option) (params : Row.value array)
    (meta : Cat.table_meta) ~rowid ~(row : Row.t) ~col_idx ~new_val =
  let old_val = row.(col_idx) in
  let* () =
    update_col_in_tx tx cat meta ~rowid ~row ~col_idx ~new_val
  in
  if not (Cat.get_fk_enforcement cat) then Lwt.return_unit
  else begin
    let parent_col_name = (List.nth meta.Cat.columns col_idx).Row.name in
    let* all_child_refs =
      build_child_refs cat ~parent_table_name:meta.Cat.name
    in
    let col_child_refs =
      List.filter_map (fun (child_meta, fks) ->
        let matching_fks =
          List.filter (fun (fk : Cat.fk_constraint) ->
            String.equal fk.Cat.fk_parent_col parent_col_name
          ) fks
        in
        if matching_fks = [] then None
        else Some (child_meta, matching_fks)
      ) all_child_refs
    in
    Lwt_list.iter_s (fun (child_meta, fks) ->
      Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
        let child_col_idx =
          find_col_idx_by_name child_meta.Cat.columns fk.Cat.fk_local_col
        in
        match fk.Cat.fk_on_update with
        | Cat.FA_restrict | Cat.FA_no_action -> Lwt.return_unit
        | Cat.FA_cascade ->
          let* child_rows =
            scan_child_rows_tx tx child_meta ~child_col_idx
              ~parent_val:old_val
          in
          Lwt_list.iter_s (fun (crid, crow) ->
            cascade_update_col_in_tx tx cat clock params child_meta
              ~rowid:crid ~row:crow ~col_idx:child_col_idx ~new_val
          ) child_rows
        | Cat.FA_set_null ->
          let col = List.nth child_meta.Cat.columns child_col_idx in
          if col.Row.not_null then
            Lwt.fail_with (Printf.sprintf
              "FOREIGN KEY constraint failed: ON UPDATE SET NULL on \
               NOT NULL column '%s.%s'"
              child_meta.Cat.name fk.Cat.fk_local_col)
          else begin
            let* child_rows =
              scan_child_rows_tx tx child_meta ~child_col_idx
                ~parent_val:old_val
            in
            Lwt_list.iter_s (fun (crid, crow) ->
              update_col_in_tx tx cat child_meta ~rowid:crid ~row:crow
                ~col_idx:child_col_idx ~new_val:Row.V_null
            ) child_rows
          end
        | Cat.FA_set_default ->
          let* child_rows =
            scan_child_rows_tx tx child_meta ~child_col_idx
              ~parent_val:old_val
          in
          if child_rows = [] then Lwt.return_unit
          else begin
            let col = List.nth child_meta.Cat.columns child_col_idx in
            let default_val = match col.Row.default with
              | None               -> Row.V_null
              | Some Row.DV_int  n -> Row.V_int  n
              | Some Row.DV_text s -> Row.V_text s
              | Some Row.DV_real f -> Row.V_real f
              | Some Row.DV_blob b -> Row.V_blob b
              | Some Row.DV_null   -> Row.V_null
              | Some Row.DV_current_timestamp ->
                eval_expr clock params [||]
                  (Plan.P_func (Ast.Fn_datetime,
                     [Plan.P_lit (Ast.L_text "now")]))
              | Some Row.DV_current_date ->
                eval_expr clock params [||]
                  (Plan.P_func (Ast.Fn_date,
                     [Plan.P_lit (Ast.L_text "now")]))
              | Some Row.DV_current_time ->
                eval_expr clock params [||]
                  (Plan.P_func (Ast.Fn_time,
                     [Plan.P_lit (Ast.L_text "now")]))
            in
            if col.Row.not_null && default_val = Row.V_null then
              Lwt.fail_with (Printf.sprintf
                "FOREIGN KEY constraint failed: ON UPDATE SET DEFAULT on NOT NULL column '%s.%s' with no default"
                child_meta.Cat.name fk.Cat.fk_local_col)
            else
              Lwt_list.iter_s (fun (crid, crow) ->
                update_col_in_tx tx cat child_meta ~rowid:crid ~row:crow
                  ~col_idx:child_col_idx ~new_val:default_val
              ) child_rows
          end
      ) fks
    ) col_child_refs
  end

(** Run [Op_update]: drain matching rows into a list (snapshot read),
    then for each (rowid, old_row) compute the new row, update index
    entries, and overwrite the row in the table tree.  Returns the
    number of rows whose contents were modified. *)
let execute_update ?(mode = Auto) ?(params = [||])
    ?(clock : (unit -> float) option = None)
    ?(before_hook : (old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option = None)
    ?(after_hook  : (old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option = None)
    (store : S.t)
    (cat : Cat.t)
    ~(table_meta : Cat.table_meta)
    ~(assignments : (int * Plan.expr) list)
    ~(where : Plan.expr option)
    ~(order : (Plan.expr * [`Asc | `Desc] * [`Nulls_first | `Nulls_last]) list)
    ~(limit : int option)
    ~(offset : int option)
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
        | Some pred -> value_truthy (eval_expr clock params row pred)
      in
      if keep then buf := (rowid, row) :: !buf;
      drain ()
  in
  drain ();
  S.cursor_close cur;
  let* () = S.ro_end tx_ro in
  let matches = List.rev !buf in
  (* Apply ORDER BY sort, then OFFSET, then LIMIT *)
  let matches =
    let sorted =
      if order = [] then matches
      else
        List.sort (fun (_, ra) (_, rb) ->
          let rec cmp = function
            | [] -> 0
            | (e, dir, nulls) :: rest ->
              let va = eval_expr clock params ra e in
              let vb = eval_expr clock params rb e in
              let c = compare_with_nulls dir nulls va vb in
              if c <> 0 then c else cmp rest
          in cmp order
        ) matches
    in
    let after_offset = match offset with
      | None | Some 0 -> sorted
      | Some n -> list_drop n sorted
    in
    match limit with
    | None -> after_offset
    | Some n -> list_take n after_offset
  in
  let n = List.length matches in
  if n = 0 then Lwt.return 0
  else begin
    (* FK pre-check: fail for RESTRICT/NO_ACTION when referenced key changes.
       CASCADE/SET_NULL/SET_DEFAULT applied inside the RW transaction below. *)
    let* child_refs =
      if Cat.get_fk_enforcement cat then
        build_child_refs cat ~parent_table_name:table_meta.Cat.name
      else Lwt.return []
    in
    let* () =
      if child_refs = [] then Lwt.return_unit
      else
        Lwt_list.iter_s (fun (_rowid, old_row) ->
          let new_row = Array.copy old_row in
          List.iter (fun (i, expr) ->
            new_row.(i) <- eval_expr clock params old_row expr
          ) assignments;
          Lwt_list.iter_s (fun (child_meta, fks) ->
            Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
              match fk.fk_on_update with
              | Cat.FA_cascade | Cat.FA_set_null | Cat.FA_set_default -> Lwt.return_unit
              | Cat.FA_restrict | Cat.FA_no_action ->
                let parent_col_idx = find_col_idx_by_name table_meta.Cat.columns fk.fk_parent_col in
                let old_val = old_row.(parent_col_idx) in
                let new_val = new_row.(parent_col_idx) in
                if compare_values old_val new_val = 0 then Lwt.return_unit
                else
                  (match old_val with
                   | Row.V_null -> Lwt.return_unit
                   | _ ->
                     let child_col_idx = find_col_idx_by_name child_meta.Cat.columns fk.fk_local_col in
                     let* has_ref = fk_child_has_ref store child_meta ~child_col_idx ~parent_val:old_val in
                     if has_ref then
                       Lwt.fail_with (Printf.sprintf
                         "FOREIGN KEY constraint failed: update to '%s.%s' is referenced by '%s.%s'"
                         table_meta.Cat.name fk.fk_parent_col
                         child_meta.Cat.name fk.fk_local_col)
                     else Lwt.return_unit)
            ) fks
          ) child_refs
        ) matches
    in
    (* Fire BEFORE UPDATE triggers (per row) *)
    let* () = match before_hook with
      | None -> Lwt.return_unit
      | Some f ->
        Lwt_list.iter_s (fun (_rowid, old_row) ->
          let new_row = Array.copy old_row in
          List.iter (fun (i, expr) ->
            new_row.(i) <- eval_expr clock params old_row expr
          ) assignments;
          f ~old_row ~new_row
        ) matches
    in
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
              new_row.(i) <- eval_expr clock params old_row expr
            ) assignments;
            compute_generated_cols clock params table_meta new_row;
            (* Evaluate CHECK constraints on the new row before writes. *)
            eval_check_constraints clock params table_meta new_row;
            Lwt_list.iter_s (fun (idx : Cat.index_info) ->
              if not idx.idx_unique then Lwt.return_unit
              else if not (row_matches_index_where clock params idx schema new_row)
              then Lwt.return_unit
              else begin
                (* Only check if any of the indexed values actually changed *)
                let old_vs = get_index_key_values clock params idx schema old_row in
                let new_vs = get_index_key_values clock params idx schema new_row in
                let values_equal a b = match a, b with
                  | Row.V_null, Row.V_null     -> true
                  | Row.V_int  x, Row.V_int  y -> Int64.equal x y
                  | Row.V_text x, Row.V_text y -> String.equal x y
                  | Row.V_real x, Row.V_real y -> Float.equal x y
                  | Row.V_blob x, Row.V_blob y -> Bytes.equal x y
                  | _                           -> false
                in
                let unchanged =
                  List.for_all2 values_equal old_vs new_vs
                in
                if unchanged then Lwt.return_unit
                else
                  let* dup = unique_violation_on_update tx idx new_vs ~rowid
                               ~new_row ~schema in
                  if dup then
                    Lwt.fail_with (Printf.sprintf
                      "UNIQUE constraint violated: duplicate value in columns (%s)"
                      (String.concat ", " idx.idx_columns))
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
              new_row.(i) <- eval_expr clock params old_row expr
            ) assignments;
            compute_generated_cols clock params table_meta new_row;
            (* Apply FK cascade UPDATE actions (CASCADE / SET NULL / SET DEFAULT). *)
            let* () =
              if child_refs = [] then Lwt.return_unit
              else
                Lwt_list.iter_s (fun (child_meta, fks) ->
                  Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
                    let parent_col_idx = find_col_idx_by_name table_meta.Cat.columns fk.fk_parent_col in
                    let old_val = old_row.(parent_col_idx) in
                    let new_val = new_row.(parent_col_idx) in
                    if compare_values old_val new_val = 0 then Lwt.return_unit
                    else
                      (match old_val with
                       | Row.V_null -> Lwt.return_unit
                       | _ ->
                         let child_col_idx = find_col_idx_by_name child_meta.Cat.columns fk.fk_local_col in
                         (match fk.fk_on_update with
                          | Cat.FA_restrict | Cat.FA_no_action -> Lwt.return_unit
                          | Cat.FA_cascade ->
                            let* child_rows = scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val:old_val in
                            Lwt_list.iter_s (fun (crid, crow) ->
                              cascade_update_col_in_tx tx cat clock params child_meta
                                ~rowid:crid ~row:crow ~col_idx:child_col_idx ~new_val
                            ) child_rows
                          | Cat.FA_set_null ->
                            let* child_rows = scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val:old_val in
                            if child_rows = [] then Lwt.return_unit
                            else begin
                              let col = List.nth child_meta.Cat.columns child_col_idx in
                              if col.Row.not_null then
                                Lwt.fail_with (Printf.sprintf
                                  "FOREIGN KEY constraint failed: ON UPDATE SET NULL on NOT NULL column '%s.%s'"
                                  child_meta.Cat.name fk.fk_local_col)
                              else
                                Lwt_list.iter_s (fun (crid, crow) ->
                                  update_col_in_tx tx cat child_meta ~rowid:crid ~row:crow
                                    ~col_idx:child_col_idx ~new_val:Row.V_null
                                ) child_rows
                            end
                          | Cat.FA_set_default ->
                            let* child_rows = scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val:old_val in
                            if child_rows = [] then Lwt.return_unit
                            else begin
                              let col = List.nth child_meta.Cat.columns child_col_idx in
                              let default_val = match col.Row.default with
                                | None               -> Row.V_null
                                | Some Row.DV_int  n -> Row.V_int  n
                                | Some Row.DV_text s -> Row.V_text s
                                | Some Row.DV_real f -> Row.V_real f
                                | Some Row.DV_blob b -> Row.V_blob b
                                | Some Row.DV_null   -> Row.V_null
                                | Some Row.DV_current_timestamp ->
                                  eval_expr clock params [||]
                                    (Plan.P_func (Ast.Fn_datetime, [Plan.P_lit (Ast.L_text "now")]))
                                | Some Row.DV_current_date ->
                                  eval_expr clock params [||]
                                    (Plan.P_func (Ast.Fn_date, [Plan.P_lit (Ast.L_text "now")]))
                                | Some Row.DV_current_time ->
                                  eval_expr clock params [||]
                                    (Plan.P_func (Ast.Fn_time, [Plan.P_lit (Ast.L_text "now")]))
                              in
                              if col.Row.not_null && default_val = Row.V_null then
                                Lwt.fail_with (Printf.sprintf
                                  "FOREIGN KEY constraint failed: ON UPDATE SET DEFAULT on NOT NULL column '%s.%s' with no default"
                                  child_meta.Cat.name fk.fk_local_col)
                              else
                                Lwt_list.iter_s (fun (crid, crow) ->
                                  update_col_in_tx tx cat child_meta ~rowid:crid ~row:crow
                                    ~col_idx:child_col_idx ~new_val:default_val
                                ) child_rows
                            end))
                  ) fks
                ) child_refs
            in
            let key = Rowid.encode rowid in
            (* Update index entries: delete old, insert new. *)
            let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
              let old_matches = row_matches_index_where clock params idx schema old_row in
              let new_matches = row_matches_index_where clock params idx schema new_row in
              let old_iks = List.map row_value_to_index_value
                              (get_index_key_values clock params idx schema old_row) in
              let new_iks = List.map row_value_to_index_value
                              (get_index_key_values clock params idx schema new_row) in
              let old_ikey = Index_key.encode old_iks ~rowid in
              let new_ikey = Index_key.encode new_iks ~rowid in
              let* () = if old_matches then S.del tx idx.idx_tree_id old_ikey else Lwt.return_unit in
              if new_matches then S.put tx idx.idx_tree_id new_ikey Bytes.empty
              else Lwt.return_unit
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
        (* After-hook new_row is recomputed from the pre-write snapshot; for
           non-deterministic expressions (e.g. random(), now()) the value seen
           by the trigger may differ from the committed row. *)
        let* () = match after_hook with
          | None -> Lwt.return_unit
          | Some f ->
            Lwt_list.iter_s (fun (_rowid, old_row) ->
              let new_row = Array.copy old_row in
              List.iter (fun (i, expr) ->
                new_row.(i) <- eval_expr clock params old_row expr
              ) assignments;
              f ~old_row ~new_row
            ) matches
        in
        Lwt.return n)
      (fun exn ->
        (* On any exception: rollback if we own the txn, then re-raise. *)
        let* () = if owned then S.rollback tx else Lwt.return_unit in
        Lwt.fail exn)
  end

(** Run [Op_delete]: drain matching rows into a list (snapshot read),
    then for each matching (rowid, row) remove index entries and the
    row itself from the table tree.  Returns the number of rows deleted. *)
let execute_delete ?(mode = Auto) ?(params = [||])
    ?(clock : (unit -> float) option = None)
    ?(before_hook : (old_row:Row.t -> unit Lwt.t) option = None)
    ?(after_hook  : (old_row:Row.t -> unit Lwt.t) option = None)
    (store : S.t)
    (cat : Cat.t)
    ~(table_meta : Cat.table_meta)
    ~(where : Plan.expr option)
    ~(order : (Plan.expr * [`Asc | `Desc] * [`Nulls_first | `Nulls_last]) list)
    ~(limit : int option)
    ~(offset : int option)
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
        | Some pred -> value_truthy (eval_expr clock params row pred)
      in
      if keep then buf := (rowid, row) :: !buf;
      drain ()
  in
  drain ();
  S.cursor_close cur;
  let* () = S.ro_end tx_ro in
  let matches = List.rev !buf in
  (* Apply ORDER BY sort, then OFFSET, then LIMIT *)
  let matches =
    let sorted =
      if order = [] then matches
      else
        List.sort (fun (_, ra) (_, rb) ->
          let rec cmp = function
            | [] -> 0
            | (e, dir, nulls) :: rest ->
              let va = eval_expr clock params ra e in
              let vb = eval_expr clock params rb e in
              let c = compare_with_nulls dir nulls va vb in
              if c <> 0 then c else cmp rest
          in cmp order
        ) matches
    in
    let after_offset = match offset with
      | None | Some 0 -> sorted
      | Some n -> list_drop n sorted
    in
    match limit with
    | None -> after_offset
    | Some n -> list_take n after_offset
  in
  let n = List.length matches in
  if n = 0 then Lwt.return 0
  else begin
    (* FK pre-check: fail immediately for RESTRICT/NO_ACTION.
       CASCADE/SET_NULL/SET_DEFAULT are applied inside the RW transaction below. *)
    let* child_refs =
      if Cat.get_fk_enforcement cat then
        build_child_refs cat ~parent_table_name:table_meta.Cat.name
      else Lwt.return []
    in
    let* () =
      if child_refs = [] then Lwt.return_unit
      else
        Lwt_list.iter_s (fun (_rowid, row) ->
          Lwt_list.iter_s (fun (child_meta, fks) ->
            Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
              match fk.fk_on_delete with
              | Cat.FA_cascade | Cat.FA_set_null | Cat.FA_set_default -> Lwt.return_unit
              | Cat.FA_restrict | Cat.FA_no_action ->
                let parent_col_idx = find_col_idx_by_name table_meta.Cat.columns fk.fk_parent_col in
                let parent_val = row.(parent_col_idx) in
                (match parent_val with
                 | Row.V_null -> Lwt.return_unit
                 | _ ->
                   let child_col_idx = find_col_idx_by_name child_meta.Cat.columns fk.fk_local_col in
                   let* has_ref = fk_child_has_ref store child_meta ~child_col_idx ~parent_val in
                   if has_ref then
                     Lwt.fail_with (Printf.sprintf
                       "FOREIGN KEY constraint failed: '%s.%s' is still referenced by '%s.%s'"
                       table_meta.Cat.name fk.fk_parent_col
                       child_meta.Cat.name fk.fk_local_col)
                   else Lwt.return_unit)
            ) fks
          ) child_refs
        ) matches
    in
    (* Fire BEFORE DELETE triggers (per row) *)
    let* () = match before_hook with
      | None -> Lwt.return_unit
      | Some f -> Lwt_list.iter_s (fun (_rowid, old_row) -> f ~old_row) matches
    in
    let* (tx, owned) = acquire_txn store mode in
    Lwt.catch
      (fun () ->
        let* () =
          Lwt_list.iter_s (fun (rowid, row) ->
            (* Apply FK cascade actions (CASCADE / SET NULL / SET DEFAULT) within same tx. *)
            let* () =
              if child_refs = [] then Lwt.return_unit
              else
                Lwt_list.iter_s (fun (child_meta, fks) ->
                  Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
                    let parent_col_idx = find_col_idx_by_name table_meta.Cat.columns fk.fk_parent_col in
                    let parent_val = row.(parent_col_idx) in
                    (match parent_val with
                     | Row.V_null -> Lwt.return_unit
                     | _ ->
                       let child_col_idx = find_col_idx_by_name child_meta.Cat.columns fk.fk_local_col in
                       (match fk.fk_on_delete with
                        | Cat.FA_restrict | Cat.FA_no_action -> Lwt.return_unit
                        | Cat.FA_cascade ->
                          let* child_rows = scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val in
                          Lwt_list.iter_s (fun (crid, crow) ->
                            cascade_delete_row_in_tx tx cat clock params child_meta ~rowid:crid ~row:crow
                          ) child_rows
                        | Cat.FA_set_null ->
                          let* child_rows = scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val in
                          if child_rows = [] then Lwt.return_unit
                          else begin
                            let col = List.nth child_meta.Cat.columns child_col_idx in
                            if col.Row.not_null then
                              Lwt.fail_with (Printf.sprintf
                                "FOREIGN KEY constraint failed: ON DELETE SET NULL on NOT NULL column '%s.%s'"
                                child_meta.Cat.name fk.fk_local_col)
                            else
                              Lwt_list.iter_s (fun (crid, crow) ->
                                update_col_in_tx tx cat child_meta ~rowid:crid ~row:crow
                                  ~col_idx:child_col_idx ~new_val:Row.V_null
                              ) child_rows
                          end
                        | Cat.FA_set_default ->
                          let* child_rows = scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val in
                          if child_rows = [] then Lwt.return_unit
                          else begin
                            let col = List.nth child_meta.Cat.columns child_col_idx in
                            let default_val = match col.Row.default with
                              | None               -> Row.V_null
                              | Some Row.DV_int  n -> Row.V_int  n
                              | Some Row.DV_text s -> Row.V_text s
                              | Some Row.DV_real f -> Row.V_real f
                              | Some Row.DV_blob b -> Row.V_blob b
                              | Some Row.DV_null   -> Row.V_null
                              | Some Row.DV_current_timestamp ->
                                eval_expr clock params [||]
                                  (Plan.P_func (Ast.Fn_datetime, [Plan.P_lit (Ast.L_text "now")]))
                              | Some Row.DV_current_date ->
                                eval_expr clock params [||]
                                  (Plan.P_func (Ast.Fn_date, [Plan.P_lit (Ast.L_text "now")]))
                              | Some Row.DV_current_time ->
                                eval_expr clock params [||]
                                  (Plan.P_func (Ast.Fn_time, [Plan.P_lit (Ast.L_text "now")]))
                            in
                            if col.Row.not_null && default_val = Row.V_null then
                              Lwt.fail_with (Printf.sprintf
                                "FOREIGN KEY constraint failed: ON DELETE SET DEFAULT on NOT NULL column '%s.%s' with no default"
                                child_meta.Cat.name fk.fk_local_col)
                            else
                              Lwt_list.iter_s (fun (crid, crow) ->
                                update_col_in_tx tx cat child_meta ~rowid:crid ~row:crow
                                  ~col_idx:child_col_idx ~new_val:default_val
                              ) child_rows
                          end))
                  ) fks
                ) child_refs
            in
            let rowid_key = Rowid.encode rowid in
            (* Remove index entries for this row. *)
            let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
              if not (row_matches_index_where clock params idx schema row)
              then Lwt.return_unit
              else begin
                let iks = List.map row_value_to_index_value
                            (get_index_key_values clock params idx schema row) in
                let old_ikey = Index_key.encode iks ~rowid in
                S.del tx idx.idx_tree_id old_ikey
              end
            ) indexes in
            (* Remove the row from the table tree. *)
            S.del tx table_meta.tree_id rowid_key
          ) matches
        in
        let* () = release_txn tx owned in
        (* Fire AFTER DELETE triggers (per row) *)
        let* () = match after_hook with
          | None -> Lwt.return_unit
          | Some f -> Lwt_list.iter_s (fun (_rowid, old_row) -> f ~old_row) matches
        in
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

(* ------------------------------------------------------------------ *)
(* EXPLAIN plan-tree pretty-printer                                     *)
(* ------------------------------------------------------------------ *)

let op_name = function
  | Plan.Op_seq_scan { table_meta } -> "SeqScan(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_filter _                -> "Filter"
  | Plan.Op_project _               -> "Project"
  | Plan.Op_expr_project _          -> "ExprProject"
  | Plan.Op_sort _                  -> "Sort"
  | Plan.Op_limit { limit; offset; _ } ->
    Printf.sprintf "Limit(%d offset %d)" limit offset
  | Plan.Op_aggregate _             -> "Aggregate"
  | Plan.Op_hash_join { join_kind; _ } ->
    (match join_kind with `Inner -> "HashJoin" | `Left -> "LeftHashJoin")
  | Plan.Op_nested_loop_join { join_kind; right_meta; _ } ->
    (match join_kind with
     | `Inner -> "NestedLoopJoin(" ^ right_meta.Cat.name ^ ")"
     | `Left  -> "LeftNestedLoopJoin(" ^ right_meta.Cat.name ^ ")")
  | Plan.Op_index_lookup { table_meta; _ } ->
    "IndexLookup(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_union { all; _ }        -> if all then "UnionAll" else "Union"
  | Plan.Op_intersect _             -> "Intersect"
  | Plan.Op_except _                -> "Except"
  | Plan.Op_distinct _              -> "Distinct"
  | Plan.Op_const_select _          -> "ConstSelect"
  | Plan.Op_window _                -> "Window"
  | Plan.Op_with_cte { cte_name; _ } -> "WithCte(" ^ cte_name ^ ")"
  | Plan.Op_cte_scan { cte_name; _ } -> "CteScan(" ^ cte_name ^ ")"
  | Plan.Op_insert { table_meta; _ } -> "Insert(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_insert_select { table_meta; _ } ->
    "InsertSelect(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_update { table_meta; _ } -> "Update(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_delete { table_meta; _ } -> "Delete(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_create_table { name; _ } -> "CreateTable(" ^ name ^ ")"
  | Plan.Op_create_index { name; table; _ } ->
    "CreateIndex(" ^ name ^ " on " ^ table ^ ")"
  | Plan.Op_drop_table { table_meta; _ } ->
    "DropTable(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_drop_index { idx_info } ->
    "DropIndex(" ^ idx_info.Cat.idx_name ^ ")"
  | Plan.Op_alter_table { table_meta; _ } ->
    "AlterTable(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_begin               -> "Begin"
  | Plan.Op_commit              -> "Commit"
  | Plan.Op_rollback            -> "Rollback"
  | Plan.Op_savepoint name      -> "Savepoint(" ^ name ^ ")"
  | Plan.Op_release name        -> "Release(" ^ name ^ ")"
  | Plan.Op_rollback_to name    -> "RollbackTo(" ^ name ^ ")"
  | Plan.Op_create_view { name; _ }    -> "CreateView(" ^ name ^ ")"
  | Plan.Op_drop_view { name }         -> "DropView(" ^ name ^ ")"
  | Plan.Op_create_trigger { name; _ } -> "CreateTrigger(" ^ name ^ ")"
  | Plan.Op_drop_trigger { name }      -> "DropTrigger(" ^ name ^ ")"
  | Plan.Op_pragma_rows _              -> "Pragma"
  | Plan.Op_pragma_get_user_version    -> "Pragma(get_user_version)"
  | Plan.Op_pragma_set_user_version { version } ->
    Printf.sprintf "Pragma(set_user_version=%Ld)" version
  | Plan.Op_pragma_integrity_check     -> "Pragma(integrity_check)"
  | Plan.Op_pragma_get_fk              -> "Pragma(get_foreign_keys)"
  | Plan.Op_pragma_set_fk { on }       -> Printf.sprintf "Pragma(set_foreign_keys=%b)" on
  | Plan.Op_no_op                      -> "NoOp"
  | Plan.Op_changes                    -> "Changes"
  | Plan.Op_last_insert_rowid          -> "LastInsertRowid"
  | Plan.Op_explain { analyze; _ }     ->
    if analyze then "ExplainAnalyze" else "Explain"
  | Plan.Op_create_fts_table { name; _ } -> "CreateFtsTable(" ^ name ^ ")"
  | Plan.Op_fts_insert { fts_meta; _ }   -> "FtsInsert(" ^ fts_meta.Cat.fts_name ^ ")"
  | Plan.Op_fts_delete { fts_meta; _ }   -> "FtsDelete(" ^ fts_meta.Cat.fts_name ^ ")"
  | Plan.Op_fts_seq_scan { fts_meta; _ } -> "FtsSeqScan(" ^ fts_meta.Cat.fts_name ^ ")"
  | Plan.Op_fts_match_scan { fts_meta; _ } ->
    "FtsMatchScan(" ^ fts_meta.Cat.fts_name ^ ")"
  | Plan.Op_sqlite_master -> "SqliteMaster"

let op_children = function
  | Plan.Op_filter { child; _ }      -> [child]
  | Plan.Op_project { child; _ }     -> [child]
  | Plan.Op_expr_project { child; _ }-> [child]
  | Plan.Op_sort { child; _ }        -> [child]
  | Plan.Op_limit { child; _ }       -> [child]
  | Plan.Op_distinct { child }       -> [child]
  | Plan.Op_aggregate { child; _ }   -> [child]
  | Plan.Op_window { child; _ }      -> [child]
  | Plan.Op_hash_join { left; right; _ } -> [left; right]
  | Plan.Op_nested_loop_join { left; _ } -> [left]
  | Plan.Op_union { left; right; _ } -> [left; right]
  | Plan.Op_intersect { left; right } -> [left; right]
  | Plan.Op_except { left; right }   -> [left; right]
  | Plan.Op_with_cte { def; query; _ } -> [def; query]
  | Plan.Op_explain { inner; _ }     -> [inner]
  | Plan.Op_insert_select { source; _ } -> [source]
  | _                                -> []

let explain_plan op =
  let counter = ref 0 in
  let rec walk parent op =
    let id = !counter in
    incr counter;
    let my_row = [| Row.V_int (Int64.of_int id);
                    Row.V_int (Int64.of_int parent);
                    Row.V_text (op_name op) |] in
    my_row :: List.concat_map (walk id) (op_children op)
  in
  walk (-1) op

(** Forward reference to [to_stream], which is defined in the mutually-recursive
    block starting at [pre_eval_subquery].  [execute_with_count] needs this to
    implement [Op_insert_select] (read source, then write rows). *)
let to_stream_ref : ((unit -> float) option -> Row.value array -> S.t -> ?mode:txn_mode -> ?cat:Cat.t option -> Plan.op -> Row.t Lwt_stream.t Lwt.t) ref =
  ref (fun _clock _params _store ?mode:_ ?cat:_ _op ->
    failwith "to_stream_ref not yet initialised")

(** [execute_with_count] returns the rows-affected count.  For most
    write ops this is 1 (INSERT) or 0 (DDL); for UPDATE it is the
    number of rows whose contents were modified. *)
let execute_with_count ?(mode = Auto)
    ?(clock : (unit -> float) option = None)
    ?(params = [||])
    ?(before_hook : (new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t) option = None)
    ?(after_hook  : (new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t) option = None)
    (store : S.t) (cat : Cat.t) (op : Plan.op)
  : int Lwt.t =
  match op with
  | Plan.Op_create_table { name; columns; uniq_idxs; if_not_exists; fk_constraints } ->
    (* Note: create_table acquires its own RW txn internally via catalog.
       This means CREATE TABLE is NOT atomic within an explicit BEGIN/COMMIT block —
       it commits immediately regardless of mode. Phase 4 work to fix. *)
    if if_not_exists && Cat.table_exists cat ~name then
      Lwt.return 0
    else begin
      let* _tid = Cat.create_table cat ~name ~columns in
      let* () = Lwt_list.iter_s (fun (idx_name, col_names) ->
        let* result = Cat.create_index cat ~name:idx_name ~table:name
            ~columns:col_names ~unique:true
            ~expr_flags:(List.map (fun _ -> false) col_names)
            ~where_sql:None in
        match result with
        | Error msg -> Lwt.fail_with msg
        | Ok _      -> Lwt.return_unit
      ) uniq_idxs in
      (* Persist FK constraints if any *)
      let* () =
        if fk_constraints = [] then Lwt.return_unit
        else begin
          let fk_list = List.map (fun (lc, pt, pc, od, ou) ->
            Cat.{ fk_local_col = lc; fk_parent_table = pt; fk_parent_col = pc;
                  fk_on_delete = od; fk_on_update = ou }
          ) fk_constraints in
          let* () = Cat.save_fk_constraints cat ~table_name:name ~fks:fk_list in
          Cat.set_fk_constraints cat ~table_name:name ~fks:fk_list;
          Lwt.return_unit
        end
      in
      Lwt.return 0
    end
  | Plan.Op_insert { table_meta; ordinals; values; on_conflict; returning = _; upsert_update } ->
    let bh = Option.map (fun f ~new_row -> f ~new_row:(Some new_row) ~old_row:None) before_hook in
    let ah = Option.map (fun f ~new_row -> f ~new_row:(Some new_row) ~old_row:None) after_hook in
    Lwt_list.fold_left_s (fun count row_vals ->
      let* inserted = execute_insert ~mode ~params ~clock ~on_conflict ~upsert_update
                        ~before_hook:bh ~after_hook:ah
                        store cat ~table_meta ~ordinals ~values:row_vals in
      Lwt.return (count + if inserted then 1 else 0)
    ) 0 values
  | Plan.Op_insert_select { table_meta; ordinals; source; on_conflict } ->
    let n_cols = List.length table_meta.Cat.columns in
    let bh = Option.map (fun f ~new_row -> f ~new_row:(Some new_row) ~old_row:None) before_hook in
    let ah = Option.map (fun f ~new_row -> f ~new_row:(Some new_row) ~old_row:None) after_hook in
    let* stream = !to_stream_ref clock params store ~mode ~cat:(Some cat) source in
    let* src_rows = Lwt_stream.to_list stream in
    Lwt_list.fold_left_s (fun count src_row ->
      let row_arr = Array.make n_cols Row.V_null in
      List.iteri (fun i ord ->
        if i < Array.length src_row then
          row_arr.(ord) <- src_row.(i)
      ) ordinals;
      let* inserted = execute_insert ~mode ~params ~clock ~on_conflict
                        ~before_hook:bh ~after_hook:ah
                        store cat ~table_meta ~ordinals ~values:[]
                        ~prebuilt_row:(Some row_arr) in
      Lwt.return (count + if inserted then 1 else 0)
    ) 0 src_rows
  | Plan.Op_create_index { name; table; tree_id; col_sqls; col_expr_flags;
                           where_expr; where_sql; unique; columns; if_not_exists } ->
    (* Note: create_index calls catalog functions that acquire their own RW txn.
       Like CREATE TABLE, CREATE INDEX is NOT atomic within an explicit BEGIN/COMMIT
       block — it commits immediately. Phase 4 work to fix. *)
    if if_not_exists && Cat.index_exists cat ~name then
      Lwt.return 0
    else begin
      let* () = execute_create_index ~mode store cat ~name ~table ~tree_id
                  ~col_sqls ~col_expr_flags
                  ~where_expr ~where_sql ~unique ~columns in
      Lwt.return 0
    end
  | Plan.Op_update { table_meta; assignments; where; order; limit; offset; indexes; returning = _ } ->
    let bh = Option.map (fun f ~old_row ~new_row ->
      f ~new_row:(Some new_row) ~old_row:(Some old_row)
    ) before_hook in
    let ah = Option.map (fun f ~old_row ~new_row ->
      f ~new_row:(Some new_row) ~old_row:(Some old_row)
    ) after_hook in
    execute_update ~mode ~params ~clock ~before_hook:bh ~after_hook:ah
      store cat ~table_meta ~assignments ~where ~order ~limit ~offset ~indexes
  | Plan.Op_delete { table_meta; where; order; limit; offset; indexes; returning = _ } ->
    let bh = Option.map (fun f ~old_row ->
      f ~new_row:None ~old_row:(Some old_row)
    ) before_hook in
    let ah = Option.map (fun f ~old_row ->
      f ~new_row:None ~old_row:(Some old_row)
    ) after_hook in
    execute_delete ~mode ~params ~clock ~before_hook:bh ~after_hook:ah
      store cat ~table_meta ~where ~order ~limit ~offset ~indexes
  | Plan.Op_drop_table { table_meta; indexes } ->
    let* () = execute_drop_table ~mode store cat ~table_meta ~_indexes:indexes in
    (* Invalidate cached CHECK expressions for the dropped table *)
    Hashtbl.filter_map_inplace (fun (tbl, _, _) v ->
      if String.equal tbl table_meta.name then None else Some v
    ) check_expr_cache;
    (* Invalidate cached generated-column expressions for the dropped table *)
    Hashtbl.filter_map_inplace (fun (tbl, _, _) v ->
      if String.equal tbl table_meta.name then None else Some v
    ) generated_expr_cache;
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
        let vals = List.map (fun e -> eval_expr clock params [||] e) col_values in
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
          | Some pred -> value_truthy (eval_expr clock params row pred)
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
  | Plan.Op_alter_table { table_meta; action } ->
    (match action with
     | Ast.AA_add_column col_def ->
       let col : Row.column = {
         Row.name        = col_def.Ast.name;
         Row.ty          = (match col_def.Ast.ty with
                            | Ast.Ty_int  -> Row.Integer
                            | Ast.Ty_text -> Row.Text
                            | Ast.Ty_real -> Row.Real
                            | Ast.Ty_blob -> Row.Blob);
         Row.not_null    = col_def.Ast.not_null;
         Row.primary_key = col_def.Ast.primary_key;
         Row.default     = (match col_def.Ast.default with
                            | None              -> None
                            | Some Ast.L_null   -> Some Row.DV_null
                            | Some (Ast.L_int  n) -> Some (Row.DV_int  n)
                            | Some (Ast.L_text s) -> Some (Row.DV_text s)
                            | Some (Ast.L_real f) -> Some (Row.DV_real f)
                            | Some (Ast.L_blob b) -> Some (Row.DV_blob b)
                            | Some Ast.L_current_timestamp -> Some Row.DV_current_timestamp
                            | Some Ast.L_current_date      -> Some Row.DV_current_date
                            | Some Ast.L_current_time      -> Some Row.DV_current_time);
         Row.check_sql    = Option.map Ast.expr_to_sql col_def.Ast.check;
         Row.generated_as = Option.map (fun (e, s) ->
           (Ast.expr_to_sql e, s = `Stored)) col_def.Ast.generated_as;
       } in
       let* result = Cat.add_column cat ~table_name:table_meta.Cat.name ~column:col in
       (match result with
        | Error msg -> Lwt.fail_with msg
        | Ok () ->
          (match col_def.Ast.fk_ref with
           | None -> Lwt.return 0
           | Some (parent_table, parent_col, ast_od, ast_ou) ->
             let inferred_parent_col =
               if parent_col = "" then
                 (match Cat.find_table_cached cat ~name:parent_table with
                  | None -> parent_col
                  | Some pm ->
                    (match List.find_opt (fun (c : Row.column) -> c.primary_key) pm.Cat.columns with
                     | None -> parent_col
                     | Some pk -> pk.Row.name))
               else parent_col
             in
             let new_fk : Cat.fk_constraint = {
               Cat.fk_local_col    = col_def.Ast.name;
               Cat.fk_parent_table = parent_table;
               Cat.fk_parent_col   = inferred_parent_col;
               Cat.fk_on_delete    = ast_od;
               Cat.fk_on_update    = ast_ou;
             } in
             let existing_fks =
               match Cat.find_table_cached cat ~name:table_meta.Cat.name with
               | None -> []
               | Some m -> m.Cat.fk_constraints
             in
             let new_fks = existing_fks @ [new_fk] in
             let* () = Cat.save_fk_constraints cat ~table_name:table_meta.Cat.name ~fks:new_fks in
             Cat.set_fk_constraints cat ~table_name:table_meta.Cat.name ~fks:new_fks;
             Lwt.return 0))
     | Ast.AA_rename_table new_name ->
       let* result = Cat.rename_table cat
           ~old_name:table_meta.Cat.name ~new_name in
       (match result with
        | Error msg -> Lwt.fail_with msg
        | Ok ()     ->
          (* Remap cached CHECK entries from old_name to new_name *)
          let to_add = Hashtbl.fold (fun (tbl, idx, sql) v acc ->
            if String.equal tbl table_meta.Cat.name then (new_name, idx, sql, v) :: acc
            else acc) check_expr_cache [] in
          List.iter (fun (_, idx, sql, _) ->
            Hashtbl.remove check_expr_cache (table_meta.Cat.name, idx, sql)) to_add;
          List.iter (fun (new_t, idx, sql, v) ->
            Hashtbl.add check_expr_cache (new_t, idx, sql) v) to_add;
          (* Remap cached generated-column entries from old_name to new_name *)
          let to_add_gen = Hashtbl.fold (fun (tbl, idx, sql) v acc ->
            if String.equal tbl table_meta.Cat.name then (new_name, idx, sql, v) :: acc
            else acc) generated_expr_cache [] in
          List.iter (fun (_, idx, sql, _) ->
            Hashtbl.remove generated_expr_cache (table_meta.Cat.name, idx, sql)) to_add_gen;
          List.iter (fun (new_t, idx, sql, v) ->
            Hashtbl.add generated_expr_cache (new_t, idx, sql) v) to_add_gen;
          Lwt.return 0)
     | Ast.AA_rename_column (old_col, new_col) ->
       let* result = Cat.rename_column cat
           ~table_name:table_meta.Cat.name ~old_col ~new_col in
       (match result with
        | Error msg -> Lwt.fail_with msg
        | Ok ()     -> Lwt.return 0)
     | Ast.AA_drop_column col_name ->
       let table_name = table_meta.Cat.name in
       let col_idx = find_col_idx_by_name table_meta.Cat.columns col_name in
       let new_columns = List.filteri (fun i _ -> i <> col_idx) table_meta.Cat.columns in
       (* Drop indexes referencing the dropped column *)
       let idxs_on_col = List.filter (fun (idx : Cat.index_info) ->
         List.mem col_name idx.Cat.idx_columns)
         (Cat.indexes_for_table cat ~table:table_name) in
       let* () = if idxs_on_col = [] then Lwt.return_unit
         else begin
           let* tx_idx = S.rw_begin store in
           let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
             Cat.drop_index cat tx_idx ~name:idx.idx_name
           ) idxs_on_col in
           S.commit tx_idx
         end
       in
       (* Migrate data rows: scan → decode → re-encode without col_idx *)
       let* tx_ro = S.ro_begin store in
       let* cur = S.cursor_open tx_ro table_meta.Cat.tree_id in
       let _sr = S.cursor_first cur in
       let rows = ref [] in
       let rec drain () =
         match S.cursor_next cur with
         | None -> ()
         | Some (k, v) ->
           let old_row = Row.decode table_meta.Cat.columns v in
           let new_row = Array.of_list
             (List.filteri (fun i _ -> i <> col_idx) (Array.to_list old_row)) in
           rows := (Bytes.copy k, new_row) :: !rows;
           drain ()
       in
       drain ();
       S.cursor_close cur;
       let* () = S.ro_end tx_ro in
       let* tx = S.rw_begin store in
       let* () = Lwt_list.iter_s (fun (k, new_row) ->
         let new_bytes = Row.encode new_columns new_row in
         S.put tx table_meta.Cat.tree_id k new_bytes
       ) !rows in
       let* () = S.commit tx in
       let* result = Cat.drop_column cat ~table_name ~col_name in
       (match result with
        | Error msg -> Lwt.fail_with msg
        | Ok ()     ->
          (* Invalidate cached CHECK and generated-column expressions for this table *)
          let to_clear_chk = Hashtbl.fold (fun (tn, idx, sql) _ acc ->
            if String.equal tn table_name then (tn, idx, sql) :: acc else acc
          ) check_expr_cache [] in
          List.iter (Hashtbl.remove check_expr_cache) to_clear_chk;
          let to_clear_gen = Hashtbl.fold (fun (tn, idx, sql) _ acc ->
            if String.equal tn table_name then (tn, idx, sql) :: acc else acc
          ) generated_expr_cache [] in
          List.iter (Hashtbl.remove generated_expr_cache) to_clear_gen;
          Lwt.return 0))
  | Plan.Op_begin | Plan.Op_commit | Plan.Op_rollback
  | Plan.Op_savepoint _ | Plan.Op_release _ | Plan.Op_rollback_to _ ->
    failwith "Exec.execute_with_count: BEGIN/COMMIT/ROLLBACK/SAVEPOINT handled by Db layer"
  | Plan.Op_pragma_rows _ -> Lwt.return 0
  | Plan.Op_pragma_set_user_version { version } ->
    let* tx = S.rw_begin store in
    let* () = Cat.write_user_version_tx tx version in
    let* () = S.commit tx in
    Lwt.return 0
  | Plan.Op_pragma_set_fk { on } ->
    Cat.set_fk_enforcement cat on;
    Lwt.return 0
  | Plan.Op_create_view _ | Plan.Op_drop_view _
  | Plan.Op_create_trigger _ | Plan.Op_drop_trigger _
  | Plan.Op_no_op -> Lwt.return 0
  | Plan.Op_explain _ -> Lwt.return 0
  | Plan.Op_union _ | Plan.Op_intersect _ | Plan.Op_except _
  | Plan.Op_const_select _ | Plan.Op_with_cte _ | Plan.Op_cte_scan _
  | Plan.Op_window _
  | Plan.Op_pragma_get_user_version | Plan.Op_pragma_integrity_check
  | Plan.Op_pragma_get_fk
  | Plan.Op_changes | Plan.Op_last_insert_rowid ->
    failwith "Exec.execute: use Exec.query for read operations"
  | Plan.Op_seq_scan _ | Plan.Op_filter _ | Plan.Op_project _
  | Plan.Op_expr_project _
  | Plan.Op_sort _ | Plan.Op_limit _ | Plan.Op_index_lookup _
  | Plan.Op_nested_loop_join _ | Plan.Op_hash_join _ | Plan.Op_aggregate _
  | Plan.Op_fts_seq_scan _ | Plan.Op_fts_match_scan _
  | Plan.Op_distinct _ | Plan.Op_sqlite_master ->
    failwith "Exec.execute: use Exec.query for read operations"

(** Compatibility entry point: discards the rows-affected count. *)
let execute ?(mode = Auto)
    ?(clock : (unit -> float) option = None)
    ?(params = [||])
    ?(before_hook : (new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t) option = None)
    ?(after_hook  : (new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t) option = None)
    (store : S.t) (cat : Cat.t) (op : Plan.op) : unit Lwt.t =
  let* _n = execute_with_count ~mode ~clock ~params ~before_hook ~after_hook store cat op in
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

(** Build a highlighted excerpt of [col_text] for the given snippet [spec].
    Finds the first token matching a query term, centres a window, reconstructs
    original-cased text with matching terms wrapped in [start_tag]/[end_tag]. *)
let compute_snippet ~col_text ~query_terms ~(spec : Plan.snippet_spec) =
  let tokens = Fts_tokenizer.tokenize_string ~col:0 col_text in
  let n_toks = List.length tokens in
  let first_match =
    List.find_opt (fun tok ->
      List.mem tok.Fts_tokenizer.term query_terms
    ) tokens
  in
  match first_match with
  | None ->
    let max_len = min (String.length col_text) 50 in
    let text = String.sub col_text 0 max_len in
    if String.length col_text > 50 then text ^ spec.Plan.ellipsis else text
  | Some matched ->
    let center   = matched.Fts_tokenizer.pos in
    let win_half = spec.Plan.n_tokens in
    let win_start = max 0 (center - win_half) in
    let win_end   = min (n_toks - 1) (center + win_half) in
    let arr = Array.of_list tokens in
    let window = Array.to_list (Array.sub arr win_start (win_end - win_start + 1)) in
    let prefix = if win_start > 0 then spec.Plan.ellipsis else "" in
    let suffix = if win_end < n_toks - 1 then spec.Plan.ellipsis else "" in
    let buf = Buffer.create 128 in
    Buffer.add_string buf prefix;
    let first_tok_start =
      match window with [] -> 0 | t :: _ -> t.Fts_tokenizer.start_byte
    in
    let prev_end = ref first_tok_start in
    List.iter (fun tok ->
      if tok.Fts_tokenizer.start_byte > !prev_end then
        Buffer.add_string buf
          (String.sub col_text !prev_end
             (tok.Fts_tokenizer.start_byte - !prev_end));
      let raw =
        String.sub col_text tok.Fts_tokenizer.start_byte
          (tok.Fts_tokenizer.end_byte - tok.Fts_tokenizer.start_byte)
      in
      if List.mem tok.Fts_tokenizer.term query_terms then begin
        Buffer.add_string buf spec.Plan.start_tag;
        Buffer.add_string buf raw;
        Buffer.add_string buf spec.Plan.end_tag
      end else
        Buffer.add_string buf raw;
      prev_end := tok.Fts_tokenizer.end_byte
    ) window;
    Buffer.add_string buf suffix;
    Buffer.contents buf

(* ------------------------------------------------------------------ *)
(* substitute_cte: replace Op_cte_scan nodes with Op_pragma_rows       *)
(* to_stream: convert a read op tree into a Row stream                  *)
(* pre_eval_subquery: resolve subquery Plan.expr nodes before row scan  *)
(* ------------------------------------------------------------------ *)

(** Check whether any unresolved subquery nodes remain in a Plan.expr. *)
let rec plan_expr_has_subquery : Plan.expr -> bool = function
  | Plan.P_subquery _ | Plan.P_exists _ | Plan.P_in_select _ -> true
  | Plan.P_binop (_, a, b)        -> plan_expr_has_subquery a || plan_expr_has_subquery b
  | Plan.P_not e | Plan.P_is_null e | Plan.P_is_not_null e
  | Plan.P_neg e | Plan.P_bitnot e -> plan_expr_has_subquery e
  | Plan.P_between (x, lo, hi)    ->
    plan_expr_has_subquery x || plan_expr_has_subquery lo || plan_expr_has_subquery hi
  | Plan.P_in (x, vs)             -> plan_expr_has_subquery x || List.exists plan_expr_has_subquery vs
  | Plan.P_func (_, args)         -> List.exists plan_expr_has_subquery args
  | Plan.P_case { scrutinee; branches; else_ } ->
    Option.fold ~none:false ~some:plan_expr_has_subquery scrutinee
    || List.exists (fun (c, r) -> plan_expr_has_subquery c || plan_expr_has_subquery r) branches
    || Option.fold ~none:false ~some:plan_expr_has_subquery else_
  | Plan.P_cast (e, _)            -> plan_expr_has_subquery e
  | Plan.P_collate (e, _)        -> plan_expr_has_subquery e
  | _                             -> false

(** Extract table_meta from the leftmost seq scan in a plan op. *)
let rec get_outer_scan_meta : Plan.op -> Cat.table_meta option = function
  | Plan.Op_seq_scan { table_meta } -> Some table_meta
  | Plan.Op_filter  { child; _ }    -> get_outer_scan_meta child
  | Plan.Op_sort    { child; _ }    -> get_outer_scan_meta child
  | Plan.Op_limit   { child; _ }    -> get_outer_scan_meta child
  | Plan.Op_index_lookup { table_meta; _ } -> Some table_meta
  | _                               -> None

(** Substitute outer column refs (table.col) with literal values from the outer row. *)
let rec substitute_outer_in_expr (meta : Cat.table_meta) (row : Row.t) (e : Ast.expr) : Ast.expr =
  let go = substitute_outer_in_expr meta row in
  match e with
  | Ast.E_tbl_col (tbl, col) when String.equal tbl meta.Cat.name ->
    (try
       let i = find_col_idx_by_name meta.Cat.columns col in
       Ast.E_lit (value_to_literal row.(i))
     with _ -> e)
  | Ast.E_binop (op, a, b)         -> Ast.E_binop (op, go a, go b)
  | Ast.E_not a                    -> Ast.E_not (go a)
  | Ast.E_is_null a                -> Ast.E_is_null (go a)
  | Ast.E_is_not_null a            -> Ast.E_is_not_null (go a)
  | Ast.E_neg a                    -> Ast.E_neg (go a)
  | Ast.E_bitnot a                 -> Ast.E_bitnot (go a)
  | Ast.E_between (x, lo, hi)      -> Ast.E_between (go x, go lo, go hi)
  | Ast.E_in (x, vals)             -> Ast.E_in (go x, List.map go vals)
  | Ast.E_func (f, args)           -> Ast.E_func (f, List.map go args)
  | Ast.E_cast (x, ty)             -> Ast.E_cast (go x, ty)
  | Ast.E_case { scrutinee; branches; else_ } ->
    Ast.E_case {
      scrutinee = Option.map go scrutinee;
      branches  = List.map (fun (c, r) -> (go c, go r)) branches;
      else_     = Option.map go else_;
    }
  | _ -> e

(** Apply substitute_outer_in_expr to WHERE/HAVING/JOIN ON clauses in an AST stmt. *)
let rec substitute_outer_in_stmt (meta : Cat.table_meta) (row : Row.t) (s : Ast.stmt) : Ast.stmt =
  let go_e = substitute_outer_in_expr meta row in
  let go_s = substitute_outer_in_stmt meta row in
  match s with
  | Ast.S_select r ->
    Ast.S_select { r with
      where  = Option.map go_e r.where;
      having = Option.map go_e r.having;
      joins  = List.map (fun j -> { j with Ast.on = go_e j.Ast.on }) r.joins;
    }
  | Ast.S_compound { op; left; right } ->
    Ast.S_compound { op; left = go_s left; right = go_s right }
  | Ast.S_with_cte { name; def; query; recursive } ->
    Ast.S_with_cte { name; def = go_s def; query = go_s query; recursive }
  | _ -> s

let rec substitute_cte ~(cte_name : string) ~(rows : Row.t list) (op : Plan.op) : Plan.op =
  let go = substitute_cte ~cte_name ~rows in
  match op with
  | Plan.Op_cte_scan { cte_name = n; _ } when String.equal n cte_name ->
    Plan.Op_pragma_rows { rows }
  | Plan.Op_filter r          -> Plan.Op_filter { r with child = go r.child }
  | Plan.Op_project r         -> Plan.Op_project { r with child = go r.child }
  | Plan.Op_expr_project r    -> Plan.Op_expr_project { r with child = go r.child }
  | Plan.Op_sort r            -> Plan.Op_sort { r with child = go r.child }
  | Plan.Op_limit r           -> Plan.Op_limit { r with child = go r.child }
  | Plan.Op_distinct r        -> Plan.Op_distinct { child = go r.child }
  | Plan.Op_aggregate r       -> Plan.Op_aggregate { r with child = go r.child }
  | Plan.Op_nested_loop_join r -> Plan.Op_nested_loop_join { r with left = go r.left }
  | Plan.Op_hash_join r       -> Plan.Op_hash_join { r with left = go r.left; right = go r.right }
  | Plan.Op_union r           -> Plan.Op_union { r with left = go r.left; right = go r.right }
  | Plan.Op_intersect r       -> Plan.Op_intersect { left = go r.left; right = go r.right }
  | Plan.Op_except r          -> Plan.Op_except { left = go r.left; right = go r.right }
  | Plan.Op_with_cte r when not (String.equal r.cte_name cte_name) ->
    Plan.Op_with_cte { r with query = go r.query }
  | Plan.Op_window r -> Plan.Op_window { r with child = go r.child }
  | Plan.Op_insert_select ({ source; _ } as r) ->
    Plan.Op_insert_select { r with source = go source }
  | _ -> op

let rec pre_eval_subquery
    (clock : (unit -> float) option)
    (store : S.t)
    (params : Row.value array)
    (cat_opt : Cat.t option)
    (e : Plan.expr) : Plan.expr Lwt.t =
  match e with
  | Plan.P_subquery inner_ast ->
    (match cat_opt with
     | None -> Lwt.return (Plan.P_lit Ast.L_null)
     | Some cat ->
       let* bound_r = Sema.bind cat inner_ast in
       (match bound_r with
        | Error _ -> Lwt.return e
        | Ok bound ->
          let op = Planner.plan ~cat bound in
          let* stream = to_stream clock params store ~mode:Auto ~cat:(Some cat) op in
          let* rows = Lwt_stream.to_list stream in
          let v = match rows with
            | [] -> Ast.L_null
            | row :: _ when Array.length row >= 1 -> value_to_literal row.(0)
            | _ -> Ast.L_null
          in
          Lwt.return (Plan.P_lit v)))
  | Plan.P_exists inner_ast ->
    (match cat_opt with
     | None -> Lwt.return (Plan.P_lit (Ast.L_int 0L))
     | Some cat ->
       let* bound_r = Sema.bind cat inner_ast in
       (match bound_r with
        | Error _ -> Lwt.return e
        | Ok bound ->
          let op = Planner.plan ~cat bound in
          let* stream = to_stream clock params store ~mode:Auto ~cat:(Some cat) op in
          let* first = Lwt_stream.get stream in
          Lwt.return (Plan.P_lit (Ast.L_int (if first = None then 0L else 1L)))))
  | Plan.P_in_select (x, inner_ast) ->
    (match cat_opt with
     | None -> Lwt.return (Plan.P_in (x, []))
     | Some cat ->
       let* bound_r = Sema.bind cat inner_ast in
       (match bound_r with
        | Error _ -> Lwt.return e
        | Ok bound ->
          let op = Planner.plan ~cat bound in
          let* stream = to_stream clock params store ~mode:Auto ~cat:(Some cat) op in
          let* rows = Lwt_stream.to_list stream in
          let vals = List.filter_map (fun row ->
            if Array.length row >= 1 then Some (Plan.P_lit (value_to_literal row.(0)))
            else None) rows in
          let* x' = pre_eval_subquery clock store params cat_opt x in
          Lwt.return (Plan.P_in (x', vals))))
  | Plan.P_binop (op, a, b) ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    let* b' = pre_eval_subquery clock store params cat_opt b in
    Lwt.return (Plan.P_binop (op, a', b'))
  | Plan.P_not a ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    Lwt.return (Plan.P_not a')
  | Plan.P_is_null a ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    Lwt.return (Plan.P_is_null a')
  | Plan.P_is_not_null a ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    Lwt.return (Plan.P_is_not_null a')
  | Plan.P_neg a ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    Lwt.return (Plan.P_neg a')
  | Plan.P_bitnot a ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    Lwt.return (Plan.P_bitnot a')
  | Plan.P_between (x, lo, hi) ->
    let* x'  = pre_eval_subquery clock store params cat_opt x in
    let* lo' = pre_eval_subquery clock store params cat_opt lo in
    let* hi' = pre_eval_subquery clock store params cat_opt hi in
    Lwt.return (Plan.P_between (x', lo', hi'))
  | Plan.P_in (x, vals) ->
    let* x'    = pre_eval_subquery clock store params cat_opt x in
    let* vals' = Lwt_list.map_s (pre_eval_subquery clock store params cat_opt) vals in
    Lwt.return (Plan.P_in (x', vals'))
  | Plan.P_func (f, args) ->
    let* args' = Lwt_list.map_s (pre_eval_subquery clock store params cat_opt) args in
    Lwt.return (Plan.P_func (f, args'))
  | Plan.P_case { scrutinee; branches; else_ } ->
    let* scrutinee' =
      match scrutinee with
      | None   -> Lwt.return None
      | Some e ->
        let+ e' = pre_eval_subquery clock store params cat_opt e in
        Some e'
    in
    let* branches' = Lwt_list.map_s (fun (cond, res) ->
      let* cond' = pre_eval_subquery clock store params cat_opt cond in
      let+ res'  = pre_eval_subquery clock store params cat_opt res  in
      (cond', res')
    ) branches in
    let+ else_' =
      match else_ with
      | None   -> Lwt.return None
      | Some e ->
        let+ e' = pre_eval_subquery clock store params cat_opt e in
        Some e'
    in
    Plan.P_case { scrutinee = scrutinee'; branches = branches'; else_ = else_' }
  | Plan.P_cast (e, ty) ->
    let* e' = pre_eval_subquery clock store params cat_opt e in
    Lwt.return (Plan.P_cast (e', ty))
  | Plan.P_collate (e, c) ->
    let* e' = pre_eval_subquery clock store params cat_opt e in
    Lwt.return (Plan.P_collate (e', c))
  | _ -> Lwt.return e

(* ------------------------------------------------------------------ *)
(* Window function helpers                                              *)
(* ------------------------------------------------------------------ *)

and eval_partition_key clock params (row : Row.t) (partition_by : Plan.expr list) : Row.value list =
  List.map (eval_expr clock params row) partition_by

and partition_keys_equal (a : Row.value list) (b : Row.value list) : bool =
  List.length a = List.length b &&
  List.for_all2 (fun x y -> compare_values x y = 0) a b

and group_by_partition clock params (partition_by : Plan.expr list)
    (indexed_rows : (int * Row.t) list)
    : (Row.value list * (int * Row.t) list) list =
  List.fold_left (fun acc (idx, row) ->
    let key = eval_partition_key clock params row partition_by in
    match List.find_opt (fun (k, _) -> partition_keys_equal k key) acc with
    | Some _ ->
      List.map (fun (k, pairs) ->
        if partition_keys_equal k key then (k, pairs @ [(idx, row)]) else (k, pairs)
      ) acc
    | None -> acc @ [(key, [(idx, row)])]
  ) [] indexed_rows

and sort_partition_by clock params
    (order_by : (Plan.expr * [`Asc | `Desc] * [`Nulls_first | `Nulls_last]) list)
    (indexed_rows : (int * Row.t) list) : (int * Row.t) list =
  if order_by = [] then indexed_rows
  else
    List.sort (fun (_, ra) (_, rb) ->
      let rec cmp = function
        | [] -> 0
        | (e, dir, nulls) :: rest ->
          let va = eval_expr clock params ra e in
          let vb = eval_expr clock params rb e in
          let c = compare_with_nulls dir nulls va vb in
          if c <> 0 then c else cmp rest
      in cmp order_by
    ) indexed_rows

and compute_window_for_partition clock params (wplan : Plan.window_plan_item)
    (sorted_indexed : (int * Row.t) list) (n_total : int) : Row.value array =
  let results = Array.make n_total Row.V_null in
  let sorted_rows = Array.of_list (List.map snd sorted_indexed) in
  let sorted_orig_idxs = Array.of_list (List.map fst sorted_indexed) in
  let n = Array.length sorted_rows in
  (match wplan.Plan.func with
   | Ast.WF_row_number ->
     for pos = 0 to n - 1 do
       results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int (pos + 1))
     done

   | Ast.WF_rank ->
     let cur_rank = ref 1 in
     for pos = 0 to n - 1 do
       if pos > 0 then begin
         let order_changed = List.exists (fun (e, dir, nulls) ->
           compare_with_nulls dir nulls
             (eval_expr clock params sorted_rows.(pos)   e)
             (eval_expr clock params sorted_rows.(pos-1) e) <> 0
         ) wplan.Plan.order_by in
         if order_changed then cur_rank := pos + 1
       end;
       results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int !cur_rank)
     done

   | Ast.WF_dense_rank ->
     let cur_rank = ref 1 in
     for pos = 0 to n - 1 do
       if pos > 0 then begin
         let order_changed = List.exists (fun (e, dir, nulls) ->
           compare_with_nulls dir nulls
             (eval_expr clock params sorted_rows.(pos)   e)
             (eval_expr clock params sorted_rows.(pos-1) e) <> 0
         ) wplan.Plan.order_by in
         if order_changed then incr cur_rank
       end;
       results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int !cur_rank)
     done

   | Ast.WF_ntile ->
     let n_buckets =
       match wplan.Plan.args with
       | [e] -> (match eval_expr clock params [||] e with
                 | Row.V_int k -> Int64.to_int k
                 | _ -> 1)
       | _ -> 1
     in
     let n_buckets = max 1 n_buckets in
     for pos = 0 to n - 1 do
       let bucket = (pos * n_buckets / n) + 1 in
       results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int bucket)
     done

   | Ast.WF_lag | Ast.WF_lead ->
     let is_lag = (wplan.Plan.func = Ast.WF_lag) in
     let offset =
       match wplan.Plan.args with
       | _ :: e :: _ -> (match eval_expr clock params [||] e with
                         | Row.V_int k -> Int64.to_int k
                         | _ -> 1)
       | _ -> 1
     in
     let default_expr =
       match wplan.Plan.args with _ :: _ :: e :: _ -> Some e | _ -> None
     in
     for pos = 0 to n - 1 do
       let src_pos = if is_lag then pos - offset else pos + offset in
       let v =
         if src_pos >= 0 && src_pos < n then
           (match wplan.Plan.args with
            | e :: _ -> eval_expr clock params sorted_rows.(src_pos) e
            | []     -> Row.V_null)
         else
           (match default_expr with
            | Some e -> eval_expr clock params sorted_rows.(pos) e
            | None   -> Row.V_null)
       in
       results.(sorted_orig_idxs.(pos)) <- v
     done

   | Ast.WF_first_value ->
     let arg_expr =
       match wplan.Plan.args with
       | e :: _ -> e
       | [] -> failwith "FIRST_VALUE requires one argument"
     in
     let first_val =
       if n > 0 then eval_expr clock params sorted_rows.(0) arg_expr
       else Row.V_null
     in
     for pos = 0 to n - 1 do
       results.(sorted_orig_idxs.(pos)) <- first_val
     done

   | Ast.WF_last_value ->
     let arg_expr =
       match wplan.Plan.args with
       | e :: _ -> e
       | [] -> failwith "LAST_VALUE requires one argument"
     in
     for pos = 0 to n - 1 do
       results.(sorted_orig_idxs.(pos)) <-
         eval_expr clock params sorted_rows.(pos) arg_expr
     done

   | Ast.WF_nth_value ->
     let arg_expr =
       match wplan.Plan.args with
       | e :: _ -> e
       | [] -> failwith "NTH_VALUE requires at least one argument"
     in
     let n_arg =
       match wplan.Plan.args with
       | _ :: e :: _ -> (match eval_expr clock params [||] e with
                         | Row.V_int k -> Int64.to_int k
                         | _ -> 1)
       | _ -> 1
     in
     for pos = 0 to n - 1 do
       let v =
         if n_arg >= 1 && n_arg <= pos + 1 then
           eval_expr clock params sorted_rows.(n_arg - 1) arg_expr
         else
           Row.V_null
       in
       results.(sorted_orig_idxs.(pos)) <- v
     done

   | Ast.WF_percent_rank ->
     (* PERCENT_RANK = peer_group_start / (n - 1).
        Use positional adjacency in the already-direction-sorted array so that
        DESC order works correctly without needing to know the sort direction. *)
     if n = 0 then ()
     else begin
       let peer_start = ref 0 in
       for pos = 0 to n - 1 do
         if pos > 0 then begin
           let order_changed = List.exists (fun (e, dir, nulls) ->
             compare_with_nulls dir nulls
               (eval_expr clock params sorted_rows.(pos)   e)
               (eval_expr clock params sorted_rows.(pos-1) e) <> 0
           ) wplan.Plan.order_by in
           if order_changed then peer_start := pos
         end;
         let pct = if n <= 1 then 0.0
                   else Float.of_int !peer_start /. Float.of_int (n - 1) in
         results.(sorted_orig_idxs.(pos)) <- Row.V_real pct
       done
     end

   | Ast.WF_cume_dist ->
     (* CUME_DIST = (last position in peer group + 1) / n.
        Use positional adjacency in the already-direction-sorted array so that
        DESC order works correctly without needing to know the sort direction. *)
     if n = 0 then ()
     else begin
       let pos = ref 0 in
       while !pos < n do
         (* Find the end of the current peer group *)
         let peer_end = ref !pos in
         while !peer_end + 1 < n &&
               List.for_all (fun (e, dir, nulls) ->
                 compare_with_nulls dir nulls
                   (eval_expr clock params sorted_rows.(!peer_end + 1) e)
                   (eval_expr clock params sorted_rows.(!peer_end)     e) = 0
               ) wplan.Plan.order_by
         do
           incr peer_end
         done;
         let cd = Float.of_int (!peer_end + 1) /. Float.of_int n in
         for i = !pos to !peer_end do
           results.(sorted_orig_idxs.(i)) <- Row.V_real cd
         done;
         pos := !peer_end + 1
       done
     end

   | Ast.WF_agg agg_func ->
     let has_order = wplan.Plan.order_by <> [] in
     let arg_expr = match wplan.Plan.args with e :: _ -> Some e | [] -> None in
     let arg_vals = Array.init n (fun pos ->
       match arg_expr with
       | Some e -> eval_expr clock params sorted_rows.(pos) e
       | None   -> Row.V_null
     ) in
     let resolve_bound bound pos =
       match bound with
       | Ast.FB_unbounded_preceding -> 0
       | Ast.FB_preceding k         -> max 0 (pos - k)
       | Ast.FB_current_row         -> pos
       | Ast.FB_following k         -> min (n - 1) (pos + k)
       | Ast.FB_unbounded_following -> n - 1
     in
     for pos = 0 to n - 1 do
       let (frame_start, frame_end) = match wplan.Plan.frame with
         | None ->
           (* Default: RANGE BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW when ORDER BY
              present, RANGE BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING otherwise *)
           let fe = if has_order then pos else n - 1 in
           (0, fe)
         | Some spec ->
           (* RANGE with numeric bounds approximated as ROWS — full value-based RANGE
              semantics not implemented *)
           (resolve_bound spec.Ast.start pos, resolve_bound spec.Ast.end_ pos)
       in
       let frame_start = max 0 frame_start in
       let frame_end   = min (n - 1) frame_end in
       let indices = if frame_start > frame_end then []
                     else List.init (frame_end - frame_start + 1) (fun i -> frame_start + i) in
       let result = match agg_func with
         | Ast.Agg_count ->
           let cnt =
             if arg_expr = None then List.length indices
             else List.length (List.filter (fun i ->
               not (arg_vals.(i) = Row.V_null)) indices)
           in
           Row.V_int (Int64.of_int cnt)
         | Ast.Agg_sum ->
           List.fold_left (fun acc i ->
             match acc, arg_vals.(i) with
             | _, Row.V_null                       -> acc
             | Row.V_null, v                       -> v
             | Row.V_int  a, Row.V_int  b          -> Row.V_int  (Int64.add a b)
             | Row.V_real a, Row.V_real b          -> Row.V_real (a +. b)
             | Row.V_int  a, Row.V_real b          -> Row.V_real (Int64.to_float a +. b)
             | Row.V_real a, Row.V_int  b          -> Row.V_real (a +. Int64.to_float b)
             | _, _                                -> acc
           ) Row.V_null indices
         | Ast.Agg_avg ->
           let vals = List.filter_map (fun i ->
             match arg_vals.(i) with
             | Row.V_int  n -> Some (Int64.to_float n)
             | Row.V_real f -> Some f
             | _            -> None
           ) indices in
           if vals = [] then Row.V_null
           else Row.V_real (List.fold_left ( +. ) 0.0 vals /. float_of_int (List.length vals))
         | Ast.Agg_min ->
           List.fold_left (fun acc i ->
             match arg_vals.(i) with
             | Row.V_null -> acc
             | v -> (match acc with
               | Row.V_null -> v
               | acc_v -> if compare_values v acc_v < 0 then v else acc_v)
           ) Row.V_null indices
         | Ast.Agg_max ->
           List.fold_left (fun acc i ->
             match arg_vals.(i) with
             | Row.V_null -> acc
             | v -> (match acc with
               | Row.V_null -> v
               | acc_v -> if compare_values v acc_v > 0 then v else acc_v)
           ) Row.V_null indices
         | Ast.Agg_group_concat sep ->
           let separator = Option.value sep ~default:"," in
           let parts = List.filter_map (fun i ->
             match arg_vals.(i) with
             | Row.V_null -> None
             | Row.V_int  n -> Some (Int64.to_string n)
             | Row.V_real f -> Some (Printf.sprintf "%.17g" f)
             | Row.V_text s -> Some s
             | Row.V_blob _ -> Some ""
           ) indices in
           if parts = [] then Row.V_null
           else Row.V_text (String.concat separator parts)
       in
       results.(sorted_orig_idxs.(pos)) <- result
     done
  );
  results

and to_stream (clock : (unit -> float) option) (params : Row.value array) (store : S.t) ?(mode : txn_mode = Auto) ?(cat : Cat.t option = None) (op : Plan.op) : Row.t Lwt_stream.t Lwt.t =
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
    let* child_stream = to_stream clock params store ~mode ~cat child in
    let* pred' = pre_eval_subquery clock store params cat pred in
    if not (plan_expr_has_subquery pred') then
      (* Fast path: all subqueries resolved — filter synchronously *)
      Lwt.return (Lwt_stream.filter (fun row ->
        value_truthy (eval_expr clock params row pred')
      ) child_stream)
    else begin
      (* Slow path: correlated subqueries remain — evaluate async per row *)
      let outer_meta = get_outer_scan_meta child in
      match outer_meta with
      | None ->
        (* No outer table meta: correlated substitution impossible.
           Return false for all rows — matches pre-existing behavior for
           unresolvable subqueries, but O(1) instead of O(n) bind calls. *)
        Lwt.return (Lwt_stream.filter (fun _row -> false) child_stream)
      | Some meta ->
        Lwt.return (Lwt_stream.filter_s (fun row ->
        (* 1. Substitute outer column refs in embedded Ast.stmt nodes *)
        let subst_pred =
            let rec subst_plan e =
              match e with
              | Plan.P_exists inner ->
                Plan.P_exists (substitute_outer_in_stmt meta row inner)
              | Plan.P_in_select (x, inner) ->
                Plan.P_in_select (x, substitute_outer_in_stmt meta row inner)
              | Plan.P_subquery inner ->
                Plan.P_subquery (substitute_outer_in_stmt meta row inner)
              | Plan.P_binop (op, a, b)   -> Plan.P_binop (op, subst_plan a, subst_plan b)
              | Plan.P_not a              -> Plan.P_not (subst_plan a)
              | Plan.P_is_null a          -> Plan.P_is_null (subst_plan a)
              | Plan.P_is_not_null a      -> Plan.P_is_not_null (subst_plan a)
              | Plan.P_neg a              -> Plan.P_neg (subst_plan a)
              | Plan.P_bitnot a           -> Plan.P_bitnot (subst_plan a)
              | Plan.P_between (x, lo, hi) ->
                Plan.P_between (subst_plan x, subst_plan lo, subst_plan hi)
              | Plan.P_in (x, vs)         -> Plan.P_in (subst_plan x, List.map subst_plan vs)
              | Plan.P_func (f, args)     -> Plan.P_func (f, List.map subst_plan args)
              | Plan.P_case { scrutinee; branches; else_ } ->
                Plan.P_case {
                  scrutinee = Option.map subst_plan scrutinee;
                  branches  = List.map (fun (c, r) -> (subst_plan c, subst_plan r)) branches;
                  else_     = Option.map subst_plan else_;
                }
              | Plan.P_cast (e, ty)       -> Plan.P_cast (subst_plan e, ty)
              | _                         -> e
            in
            subst_plan pred'
        in
        (* 2. Re-evaluate subqueries with outer values now substituted as literals *)
        let* resolved = pre_eval_subquery clock store params cat subst_pred in
        Lwt.return (value_truthy (eval_expr clock params row resolved))
      ) child_stream)
    end
  | Plan.Op_project { ordinals; child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    Lwt.return (Lwt_stream.map (project_row ordinals) inner)
  | Plan.Op_expr_project { exprs; child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    let* exprs' = Lwt_list.map_s
      (fun (e, _alias) -> pre_eval_subquery clock store params cat e)
      exprs
    in
    let eval_exprs row =
      Array.of_list (List.map (eval_expr clock params row) exprs')
    in
    Lwt.return (Lwt_stream.map eval_exprs inner)
  | Plan.Op_sort { keys; child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    let* rows = Lwt_stream.to_list inner in
    let* keys' = Lwt_list.map_s (fun (e, dir, nulls) ->
        let* e' = pre_eval_subquery clock store params cat e in
        Lwt.return (e', dir, nulls)) keys in
    let cmp a b =
      List.fold_left (fun acc (key, dir, nulls) ->
        if acc <> 0 then acc
        else
          let va = eval_expr clock params a key
          and vb = eval_expr clock params b key in
          compare_with_nulls dir nulls va vb
      ) 0 keys'
    in
    let sorted = List.sort cmp rows in
    Lwt.return (Lwt_stream.of_list sorted)
  | Plan.Op_limit { limit; offset; child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    let* rows = Lwt_stream.to_list inner in
    let rows' = List.filteri (fun i _ -> i >= offset && i < offset + limit) rows in
    Lwt.return (Lwt_stream.of_list rows')
  | Plan.Op_distinct { child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    let seen = Hashtbl.create 64 in
    Lwt.return (Lwt_stream.filter (fun row ->
      let k = row_key row in
      if Hashtbl.mem seen k then false
      else (Hashtbl.replace seen k (); true)
    ) inner)
  | Plan.Op_index_lookup { table_tree; idx_tree; col_idx = _;
                           col_type; lookup_val; table_meta } ->
    (* Encode the lookup value as an IndexKey.value matching the column type. *)
    let lookup_v =
      let v = eval_expr clock params [||] lookup_val in
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
    let* left_stream = to_stream clock params store ~mode ~cat left in
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
    let* left_stream  = to_stream clock params store ~mode ~cat left in
    let* right_stream = to_stream clock params store ~mode ~cat right in
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
  | Plan.Op_aggregate { child; group_cols; aggs; having; proj; windows = agg_windows } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    let* rows = Lwt_stream.to_list inner in
    let n_group_cols = List.length group_cols in
    let group_keys_of_row row = List.map (fun i -> row.(i)) group_cols in
    let compare_group_keys ka kb =
      List.fold_left2 (fun acc a b ->
        if acc <> 0 then acc else compare_values a b
      ) 0 ka kb
    in
    let groups : (Row.value list * Row.t list) list =
      if group_cols = [] then
        [ ([], rows) ]
      else begin
        (* Stable-sort by group key tuple, then split runs of equal keys. *)
        let sorted =
          List.stable_sort (fun a b ->
            compare_group_keys (group_keys_of_row a) (group_keys_of_row b)
          ) rows
        in
        let rec group_runs acc cur_key cur_rows = function
          | [] ->
            (match cur_rows with
             | [] -> List.rev acc
             | _  -> List.rev ((cur_key, List.rev cur_rows) :: acc))
          | r :: rest ->
            let k = group_keys_of_row r in
            if cur_rows <> [] && compare_group_keys k cur_key = 0 then
              group_runs acc cur_key (r :: cur_rows) rest
            else
              let acc' =
                if cur_rows = [] then acc
                else (cur_key, List.rev cur_rows) :: acc
              in
              group_runs acc' k [r] rest
        in
        group_runs [] [] [] sorted
      end
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
      | Ast.Agg_group_concat sep, Some i ->
        let separator = Option.value sep ~default:"," in
        let parts = List.filter_map (fun r ->
          match r.(i) with
          | Row.V_null -> None
          | Row.V_int  n -> Some (Int64.to_string n)
          | Row.V_real f -> Some (Printf.sprintf "%.17g" f)
          | Row.V_text s -> Some s
          | Row.V_blob _ -> Some ""
        ) group_rows in
        if parts = [] then Row.V_null
        else Row.V_text (String.concat separator parts)
      | Ast.Agg_group_concat _, None ->
        failwith "GROUP_CONCAT requires a column argument"
      | (Ast.Agg_sum | Ast.Agg_avg | Ast.Agg_min | Ast.Agg_max), None ->
        failwith "non-COUNT aggregate must have a column argument"
    in
    let agg_output_rows =
      List.map (fun (group_key, group_rows) ->
        let agg_vals = List.map (fun spec -> compute_agg spec group_rows) aggs in
        (* Output row: [key0; key1; ...; agg0; agg1; ...] *)
        Array.of_list (group_key @ agg_vals)
      ) groups
    in
    (* Apply HAVING on the aggregate output row. *)
    let after_having =
      match having with
      | None -> agg_output_rows
      | Some pred ->
        List.filter (fun r -> value_truthy (eval_expr clock params r pred)) agg_output_rows
    in
    (* Compute post-aggregate window functions if any. *)
    let n_agg_cols = n_group_cols + List.length aggs in
    let with_windows =
      if agg_windows = [] then after_having
      else begin
        let n_total = List.length after_having in
        let indexed = List.mapi (fun i r -> (i, r)) after_having in
        let window_arrays = List.map (fun (wplan : Plan.window_plan_item) ->
          let partitions = group_by_partition clock params wplan.Plan.partition_by indexed in
          let combined = Array.make n_total Row.V_null in
          List.iter (fun (_, partition_indexed) ->
            let sorted = sort_partition_by clock params wplan.Plan.order_by partition_indexed in
            let part_results = compute_window_for_partition clock params wplan sorted n_total in
            List.iter (fun (orig_idx, _) ->
              combined.(orig_idx) <- part_results.(orig_idx)
            ) sorted
          ) partitions;
          combined
        ) agg_windows in
        List.mapi (fun i row ->
          let extras = List.map (fun arr -> arr.(i)) window_arrays in
          Array.append row (Array.of_list extras)
        ) after_having
      end
    in
    (* Project to final output row. *)
    let final_rows =
      List.map (fun agg_row ->
        Array.of_list (List.map (function
          | Plan.PI_group_col i   -> agg_row.(i)
          | Plan.PI_agg_slot k    -> agg_row.(n_group_cols + k)
          | Plan.PI_window_slot j -> agg_row.(n_agg_cols + j)
        ) proj)
      ) with_windows
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
            | Some pred -> value_truthy (eval_expr clock params row pred)
          in
          if emit then Lwt.return_some row
          else read_next ()
    in
    Lwt.return (Lwt_stream.from read_next)
  | Plan.Op_fts_match_scan { fts_meta; query; proj; include_rank; snippets } ->
    let* tx = S.ro_begin store in
    let* matches = fts_execute_query tx ~index_tree:fts_meta.Cat.fts_index_tree query in
    (* Compute BM25 scores when rank is requested *)
    let* scored_matches =
      if not include_rank then
        Lwt.return (List.map (fun (rowid, positions) -> (rowid, positions, 0.0)) matches)
      else begin
        let* (total_docs, total_tokens) = read_fts_stats tx fts_meta.Cat.fts_index_tree in
        let query_terms = fts_query_terms query in
        (* Fetch per-term posting lists: (n_docs_with_term, posting_list).
           Keeping the posting list lets us look up each term's tf per document. *)
        let* term_data = Lwt_list.map_s (fun term ->
          let* pl = fts_posting_list tx ~index_tree:fts_meta.Cat.fts_index_tree term in
          Lwt.return (List.length pl, pl)) query_terms in
        let* doc_lengths = Lwt_list.map_s (fun (rowid, positions) ->
          let dlen_key = fts_doclen_key rowid in
          let* v = S.get tx fts_meta.Cat.fts_index_tree dlen_key in
          let dl = match v with
            | None -> 1
            | Some b -> let (n, _) = Varint.decode_uint64 b 0 in Int64.to_int n
          in
          Lwt.return (rowid, positions, dl)) matches in
        let scored = List.map (fun (rowid, positions, dl) ->
          (* Use each term's own tf (occurrences in this doc) rather than
             a single shared tf from the combined query result. *)
          let score = List.fold_left (fun acc (n_docs, term_pl) ->
            let tf = match List.assoc_opt rowid term_pl with
              | None -> 0
              | Some pos -> List.length pos
            in
            acc +. bm25_score ~k1:1.2 ~b:0.75 ~total_docs ~total_tokens
                               ~n_docs_with_term:n_docs ~term_freq:tf ~doc_length:dl)
            0.0 term_data in
          (rowid, positions, score)) doc_lengths in
        Lwt.return scored
      end
    in
    (* Sort by BM25 score descending when rank is included *)
    let sorted = if include_rank then
      List.sort (fun (_, _, s1) (_, _, s2) -> Float.compare s2 s1) scored_matches
    else scored_matches in
    let query_terms = fts_query_terms query in
    let* rows = Lwt_list.filter_map_s (fun (rowid, _positions, score) ->
      let key = Rowid.encode rowid in
      let* val_opt = S.get tx fts_meta.Cat.fts_content_tree key in
      match val_opt with
      | None -> Lwt.return None
      | Some bytes ->
        let texts = fts_decode_content bytes in
        let full_row = Array.of_list (List.map (fun s -> Row.V_text s) texts) in
        (* If proj=[] and there are snippets, it means only snippets were selected
           (no regular columns). If proj=[] with no snippets, it means SELECT *. *)
        let projected =
          if proj = [] && snippets = [] then Array.to_list full_row
          else List.map (fun i -> full_row.(i)) proj
        in
        let snippet_vals =
          List.map (fun (spec : Plan.snippet_spec) ->
            let col_text =
              let idx =
                if spec.Plan.col_idx < 0 then 0
                else min spec.Plan.col_idx (max 0 (List.length texts - 1))
              in
              if texts = [] then "" else List.nth texts idx
            in
            Row.V_text (compute_snippet ~col_text ~query_terms ~spec)
          ) snippets
        in
        let row_values =
          projected
          @ (if include_rank then [Row.V_real score] else [])
          @ snippet_vals
        in
        Lwt.return (Some (Array.of_list row_values))) sorted in
    let* () = S.ro_end tx in
    Lwt.return (Lwt_stream.of_list rows)
  | Plan.Op_pragma_rows { rows } ->
    Lwt.return (Lwt_stream.of_list rows)
  | Plan.Op_pragma_get_user_version ->
    let* tx = S.ro_begin store in
    let* v  = Cat.read_user_version_tx tx in
    let* () = S.ro_end tx in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int v |] ])
  | Plan.Op_pragma_get_fk ->
    let v = match cat with
      | None     -> false
      | Some cat -> Cat.get_fk_enforcement cat
    in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int (if v then 1L else 0L) |] ])
  | Plan.Op_pragma_integrity_check ->
    let cat_val = match cat with
      | None -> failwith "Exec.to_stream: Op_pragma_integrity_check requires catalog"
      | Some c -> c
    in
    let* tables = Cat.list_tables cat_val in
    let errors  = ref [] in
    let add_err msg = errors := msg :: !errors in
    let count_entries tx tid =
      let count = ref 0 in
      let* cur  = S.cursor_open tx tid in
      let _sr   = S.cursor_first cur in
      let rec go () =
        match S.cursor_next cur with
        | None   -> Lwt.return_unit
        | Some _ -> incr count; go ()
      in
      let* () = go () in
      S.cursor_close cur;
      Lwt.return !count
    in
    let* tx = S.ro_begin store in
    let* () =
      Lwt_list.iter_s (fun (meta : Cat.table_meta) ->
        let* row_count = count_entries tx meta.tree_id in
        let idxs = Cat.indexes_for_table cat_val ~table:meta.name in
        Lwt_list.iter_s (fun (idx : Cat.index_info) ->
          let is_partial = idx.idx_where_sql <> None in
          let* idx_count = count_entries tx idx.idx_tree_id in
          (if (not is_partial) && idx_count <> row_count then
            add_err (Printf.sprintf
              "index %s on %s: %d entries != %d rows"
              idx.idx_name meta.name idx_count row_count));
          Lwt.return_unit
        ) idxs
      ) tables
    in
    let* () = S.ro_end tx in
    let result = List.rev !errors in
    let rows =
      if result = [] then [ [| Row.V_text "ok" |] ]
      else List.map (fun msg -> [| Row.V_text msg |]) result
    in
    Lwt.return (Lwt_stream.of_list rows)
  | Plan.Op_sqlite_master ->
    let cat_val = match cat with
      | None   -> failwith "Exec.to_stream: Op_sqlite_master requires catalog"
      | Some c -> c
    in
    let* tables = Cat.list_tables cat_val in
    let table_rows = List.map (fun (meta : Cat.table_meta) ->
      [| Row.V_text "table";
         Row.V_text meta.Cat.name;
         Row.V_text meta.Cat.name;
         Row.V_int  (Int64.of_int meta.Cat.tree_id);
         Row.V_text (ddl_of_table meta) |]
    ) tables in
    let index_rows =
      List.concat_map (fun (meta : Cat.table_meta) ->
        List.map (fun (idx : Cat.index_info) ->
          [| Row.V_text "index";
             Row.V_text idx.Cat.idx_name;
             Row.V_text idx.Cat.idx_table;
             Row.V_int  (Int64.of_int idx.Cat.idx_tree_id);
             Row.V_text (ddl_of_index idx) |]
        ) (Cat.indexes_for_table cat_val ~table:meta.Cat.name)
      ) tables
    in
    let* views = Cat.load_all_views store in
    let view_rows = List.map (fun (name, sql) ->
      [| Row.V_text "view";
         Row.V_text name;
         Row.V_text name;
         Row.V_int  0L;
         Row.V_text sql |]
    ) views in
    let* triggers = Cat.load_all_triggers store in
    let trigger_rows = List.map (fun (name, sql) ->
      let tbl_name = trigger_table_of_sql name sql in
      [| Row.V_text "trigger";
         Row.V_text name;
         Row.V_text tbl_name;
         Row.V_int  0L;
         Row.V_text sql |]
    ) triggers in
    let all_rows = table_rows @ index_rows @ view_rows @ trigger_rows in
    Lwt.return (Lwt_stream.of_list all_rows)
  | Plan.Op_union { all; left; right } ->
    let* ls = to_stream clock params store ~mode ~cat left  in
    let* rs = to_stream clock params store ~mode ~cat right in
    let combined = Lwt_stream.append ls rs in
    if all then Lwt.return combined
    else begin
      let* rows = Lwt_stream.to_list combined in
      let seen = Hashtbl.create 64 in
      let deduped = List.filter (fun row ->
        let k = row_key row in
        if Hashtbl.mem seen k then false
        else (Hashtbl.replace seen k (); true)
      ) rows in
      Lwt.return (Lwt_stream.of_list deduped)
    end
  | Plan.Op_intersect { left; right } ->
    let* ls = to_stream clock params store ~mode ~cat left  in
    let* rs = to_stream clock params store ~mode ~cat right in
    let* right_list = Lwt_stream.to_list rs in
    let right_set = Hashtbl.create (max 1 (List.length right_list)) in
    List.iter (fun r -> Hashtbl.replace right_set (row_key r) ()) right_list;
    let* left_list = Lwt_stream.to_list ls in
    (* INTERSECT deduplicates: each distinct left row that also appears in right *)
    let seen = Hashtbl.create 64 in
    let result = List.filter (fun row ->
      let k = row_key row in
      if (not (Hashtbl.mem right_set k)) || Hashtbl.mem seen k then false
      else (Hashtbl.replace seen k (); true)
    ) left_list in
    Lwt.return (Lwt_stream.of_list result)
  | Plan.Op_except { left; right } ->
    let* ls = to_stream clock params store ~mode ~cat left  in
    let* rs = to_stream clock params store ~mode ~cat right in
    let* right_list = Lwt_stream.to_list rs in
    let right_set = Hashtbl.create (max 1 (List.length right_list)) in
    List.iter (fun r -> Hashtbl.replace right_set (row_key r) ()) right_list;
    let* left_list = Lwt_stream.to_list ls in
    (* EXCEPT deduplicates: each distinct left row not in right *)
    let seen = Hashtbl.create 64 in
    let result = List.filter (fun row ->
      let k = row_key row in
      if Hashtbl.mem right_set k || Hashtbl.mem seen k then false
      else (Hashtbl.replace seen k (); true)
    ) left_list in
    Lwt.return (Lwt_stream.of_list result)
  | Plan.Op_insert { table_meta; ordinals; values; on_conflict; returning; upsert_update }
    when returning <> [] ->
    (match cat with
     | None -> failwith "Exec.query: RETURNING requires catalog context"
     | Some c ->
       let* result_lists = Lwt_list.map_s (fun row_vals ->
         let n = List.length table_meta.columns in
         let inserted_row = Array.make n Row.V_null in
         List.iter2 (fun ord e ->
           inserted_row.(ord) <- eval_expr clock params [||] e
         ) ordinals row_vals;
         let* inserted =
           execute_insert ~mode ~clock ~on_conflict ~upsert_update ~prebuilt_row:(Some inserted_row)
             store c ~table_meta ~ordinals ~values:row_vals
         in
         if not inserted then Lwt.return []
         else
           let result = Array.of_list (List.map (eval_expr clock params inserted_row) returning) in
           Lwt.return [result]
       ) values in
       Lwt.return (Lwt_stream.of_list (List.concat result_lists)))
  | Plan.Op_update { table_meta; assignments; where; order; limit; offset; indexes; returning }
    when returning <> [] ->
    let schema = table_meta.Cat.columns in
    (* Snapshot matching rows BEFORE update to compute RETURNING values. *)
    let* tx_ro = S.ro_begin store in
    let* cur   = S.cursor_open tx_ro table_meta.tree_id in
    let _sr    = S.cursor_first cur in
    let buf    = ref [] in
    let rec drain () =
      match S.cursor_next cur with
      | None -> ()
      | Some (kbytes, vbytes) ->
        let rowid = Rowid.decode kbytes in
        let row = Row.decode schema vbytes in
        let keep = match where with
          | None      -> true
          | Some pred -> value_truthy (eval_expr clock params row pred)
        in
        if keep then buf := (rowid, row) :: !buf;
        drain ()
    in
    drain ();
    S.cursor_close cur;
    let* () = S.ro_end tx_ro in
    let matched = List.rev !buf in
    (* Apply ORDER BY, OFFSET, LIMIT *)
    let matched =
      let sorted =
        if order = [] then matched
        else
          List.sort (fun (_, ra) (_, rb) ->
            let rec cmp = function
              | [] -> 0
              | (e, dir, nulls) :: rest ->
                let va = eval_expr clock params ra e in
                let vb = eval_expr clock params rb e in
                let c = compare_with_nulls dir nulls va vb in
                if c <> 0 then c else cmp rest
            in cmp order
          ) matched
      in
      let after_offset = match offset with
        | None | Some 0 -> sorted
        | Some n -> list_drop n sorted
      in
      match limit with
      | None -> after_offset
      | Some n -> list_take n after_offset
    in
    (* Compute new values for each matched row, project RETURNING from new row. *)
    let result_rows = List.map (fun (_, old_row) ->
      let new_row = Array.copy old_row in
      List.iter (fun (i, expr) ->
        new_row.(i) <- eval_expr clock params old_row expr
      ) assignments;
      compute_generated_cols clock params table_meta new_row;
      Array.of_list (List.map (eval_expr clock params new_row) returning)
    ) matched in
    (* NOTE: ORDER BY expressions must be deterministic — the RETURNING snapshot
       and the actual write use separate table scans that both apply the same
       order/limit/offset. Non-deterministic expressions (e.g. random()) could
       return RETURNING values for rows different from those actually modified. *)
    let c = match cat with Some c -> c | None -> failwith "Exec.to_stream: UPDATE RETURNING requires catalog context" in
    let* _ = execute_update ~mode ~params ~clock store c ~table_meta ~assignments ~where ~order ~limit ~offset ~indexes in
    Lwt.return (Lwt_stream.of_list result_rows)
  | Plan.Op_delete { table_meta; where; order; limit; offset; indexes; returning }
    when returning <> [] ->
    let schema = table_meta.Cat.columns in
    (* Snapshot matching rows BEFORE delete to compute RETURNING values. *)
    let* tx_ro = S.ro_begin store in
    let* cur   = S.cursor_open tx_ro table_meta.tree_id in
    let _sr    = S.cursor_first cur in
    let buf    = ref [] in
    let rec drain () =
      match S.cursor_next cur with
      | None -> ()
      | Some (_kbytes, vbytes) ->
        let row = Row.decode schema vbytes in
        let keep = match where with
          | None      -> true
          | Some pred -> value_truthy (eval_expr clock params row pred)
        in
        if keep then buf := row :: !buf;
        drain ()
    in
    drain ();
    S.cursor_close cur;
    let* () = S.ro_end tx_ro in
    let matched = List.rev !buf in
    (* Apply ORDER BY, OFFSET, LIMIT *)
    let matched =
      let sorted =
        if order = [] then matched
        else
          List.sort (fun ra rb ->
            let rec cmp = function
              | [] -> 0
              | (e, dir, nulls) :: rest ->
                let va = eval_expr clock params ra e in
                let vb = eval_expr clock params rb e in
                let c = compare_with_nulls dir nulls va vb in
                if c <> 0 then c else cmp rest
            in cmp order
          ) matched
      in
      let after_offset = match offset with
        | None | Some 0 -> sorted
        | Some n -> list_drop n sorted
      in
      match limit with
      | None -> after_offset
      | Some n -> list_take n after_offset
    in
    let result_rows = List.map (fun old_row ->
      Array.of_list (List.map (eval_expr clock params old_row) returning)
    ) matched in
    (* NOTE: ORDER BY expressions must be deterministic — the RETURNING snapshot
       and the actual write use separate table scans that both apply the same
       order/limit/offset. Non-deterministic expressions (e.g. random()) could
       return RETURNING values for rows different from those actually modified. *)
    let c = match cat with Some c -> c | None -> failwith "Exec.to_stream: DELETE RETURNING requires catalog context" in
    let* _ = execute_delete ~mode ~params ~clock store c ~table_meta ~where ~order ~limit ~offset ~indexes in
    Lwt.return (Lwt_stream.of_list result_rows)
  | Plan.Op_changes ->
    failwith "Exec.to_stream: Op_changes must be intercepted in db.ml query"
  | Plan.Op_last_insert_rowid ->
    failwith "Exec.to_stream: Op_last_insert_rowid must be intercepted in db.ml query"
  | Plan.Op_const_select { exprs } ->
    (* FROM-less SELECT: evaluate each expression with an empty row and
       return a single result row. Aliases are stored in the plan for
       column-name purposes but are not needed during execution. *)
    let raw_exprs = List.map fst exprs in
    let* exprs' = Lwt_list.map_s (pre_eval_subquery clock store params cat) raw_exprs in
    let row = Array.of_list (List.map (eval_expr clock params [||]) exprs') in
    Lwt.return (Lwt_stream.of_list [row])
  | Plan.Op_with_cte { cte_name; def; query; recursive = false } ->
    let* def_stream = to_stream clock params store ~mode ~cat def in
    let* cte_rows = Lwt_stream.to_list def_stream in
    let patched = substitute_cte ~cte_name ~rows:cte_rows query in
    to_stream clock params store ~mode ~cat patched

  | Plan.Op_with_cte { cte_name; def; query; recursive = true } ->
    (* Recursive CTE: def must be Op_union { all=true; left=base_case; right=recursive_arm }.
       Execute base case once, then iteratively execute recursive_arm substituting
       the CTE scan with current working rows, until no new rows are produced. *)
    let (base_op, recursive_arm) = match def with
      | Plan.Op_union { all = true; left; right } -> (left, right)
      | _ ->
        failwith "Exec: recursive CTE def must be UNION ALL — non-UNION-ALL recursive CTEs are not supported"
    in
    let* base_stream = to_stream clock params store ~mode ~cat base_op in
    let* seed_rows = Lwt_stream.to_list base_stream in
    let max_iterations = 1000 in
    let rec iterate depth acc working =
      if working = [] then Lwt.return acc
      else if depth >= max_iterations then
        failwith (Printf.sprintf
          "Exec: recursive CTE '%s' exceeded maximum iteration depth of %d"
          cte_name max_iterations)
      else
        let patched_arm = substitute_cte ~cte_name ~rows:working recursive_arm in
        let* new_stream = to_stream clock params store ~mode ~cat patched_arm in
        let* new_rows = Lwt_stream.to_list new_stream in
        iterate (depth + 1) (acc @ new_rows) new_rows
    in
    let* all_rows = iterate 0 seed_rows seed_rows in
    let patched_query = substitute_cte ~cte_name ~rows:all_rows query in
    to_stream clock params store ~mode ~cat patched_query
  | Plan.Op_cte_scan { cte_name; _ } ->
    failwith (Printf.sprintf "Exec: unsubstituted Op_cte_scan '%s' — internal planner error" cte_name)
  | Plan.Op_window { child; windows; n_input_cols = _ } ->
    let* child_stream = to_stream clock params store ~mode ~cat child in
    let* all_rows = Lwt_stream.to_list child_stream in
    let n_rows = List.length all_rows in
    if n_rows = 0 then Lwt.return (Lwt_stream.of_list [])
    else begin
      let all_rows_arr = Array.of_list all_rows in
      let n_windows = List.length windows in
      let window_results : Row.value array array =
        Array.init n_windows (fun wi ->
          let wplan = List.nth windows wi in
          let indexed_rows = List.mapi (fun i row -> (i, row)) all_rows in
          let partitions = group_by_partition clock params wplan.Plan.partition_by indexed_rows in
          let combined = Array.make n_rows Row.V_null in
          List.iter (fun (_, partition_idx_rows) ->
            let sorted = sort_partition_by clock params wplan.Plan.order_by partition_idx_rows in
            let part_results = compute_window_for_partition clock params wplan sorted n_rows in
            List.iter (fun (orig_idx, _) ->
              combined.(orig_idx) <- part_results.(orig_idx)
            ) sorted
          ) partitions;
          combined
        )
      in
      let augmented = Array.to_list (Array.mapi (fun i row ->
        let extras = Array.init n_windows (fun wi -> window_results.(wi).(i)) in
        Array.append row extras
      ) all_rows_arr) in
      Lwt.return (Lwt_stream.of_list augmented)
    end
  | Plan.Op_no_op -> Lwt.return (Lwt_stream.of_list [])
  | Plan.Op_explain { analyze; inner } ->
    let plan_rows = explain_plan inner in
    let nullify row = Array.append row [| Row.V_null; Row.V_null |] in
    if not analyze then
      Lwt.return (Lwt_stream.of_list (List.map nullify plan_rows))
    else begin
      let cat_v = match cat with Some c -> c
        | None -> failwith "EXPLAIN ANALYZE requires a catalog" in
      let t0 = match clock with Some c -> c () | None -> 0.0 in
      let* n =
        let is_write = match inner with
          | Plan.Op_insert _ | Plan.Op_insert_select _ | Plan.Op_update _ | Plan.Op_delete _
          | Plan.Op_create_table _ | Plan.Op_create_index _
          | Plan.Op_drop_table _ | Plan.Op_drop_index _
          | Plan.Op_alter_table _ | Plan.Op_begin | Plan.Op_commit | Plan.Op_rollback
          | Plan.Op_savepoint _ | Plan.Op_release _ | Plan.Op_rollback_to _
          | Plan.Op_create_view _ | Plan.Op_drop_view _
          | Plan.Op_create_trigger _ | Plan.Op_drop_trigger _
          | Plan.Op_pragma_set_user_version _
          | Plan.Op_pragma_set_fk _
          | Plan.Op_fts_insert _ | Plan.Op_fts_delete _
          | Plan.Op_create_fts_table _ -> true
          | _ -> false
        in
        if is_write then
          execute_with_count ~mode ~clock ~params store cat_v inner
        else begin
          let* s = to_stream clock params store ~mode ~cat inner in
          let* rows = Lwt_stream.to_list s in
          Lwt.return (List.length rows)
        end
      in
      let elapsed_ms =
        match clock with Some c -> (c () -. t0) *. 1000.0 | None -> 0.0
      in
      let rows = List.mapi (fun i row ->
        if i = 0 then
          Array.append row
            [| Row.V_int (Int64.of_int n); Row.V_real elapsed_ms |]
        else
          nullify row
      ) plan_rows in
      Lwt.return (Lwt_stream.of_list rows)
    end
  | Plan.Op_create_table _ | Plan.Op_create_index _
  | Plan.Op_drop_table _ | Plan.Op_drop_index _
  | Plan.Op_create_fts_table _
  | Plan.Op_fts_insert _ | Plan.Op_fts_delete _
  | Plan.Op_alter_table _
  | Plan.Op_create_view _ | Plan.Op_drop_view _
  | Plan.Op_create_trigger _ | Plan.Op_drop_trigger _
  | Plan.Op_begin | Plan.Op_commit | Plan.Op_rollback
  | Plan.Op_savepoint _ | Plan.Op_release _ | Plan.Op_rollback_to _
  | Plan.Op_pragma_set_user_version _
  | Plan.Op_pragma_set_fk _ ->
    failwith "Exec.query: use Exec.execute for write operations"
  | Plan.Op_insert _ | Plan.Op_insert_select _ | Plan.Op_update _ | Plan.Op_delete _ ->
    failwith "Exec.query: use Exec.execute for write operations"

(* Wire the forward reference so execute_with_count can call to_stream for
   Op_insert_select.  This runs once at module initialization time, after both
   functions are fully defined in the let-rec block above. *)
let () = to_stream_ref := to_stream

(* ------------------------------------------------------------------ *)
(* Public query entry point                                             *)
(* ------------------------------------------------------------------ *)

let query ?(mode = Auto) ?(clock : (unit -> float) option = None) ?(params = [||]) (store : S.t) (cat : Cat.t) (op : Plan.op) :
    Row.t Lwt_stream.t Lwt.t =
  to_stream clock params store ~mode ~cat:(Some cat) op
