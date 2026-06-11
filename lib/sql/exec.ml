open Lwt.Syntax
module S = Sqlocaml_store.Store
module Cat = Sqlocaml_catalog.Catalog
module Row = Sqlocaml_encoding.Row
module Rowid = Sqlocaml_encoding.Rowid
module Index_key = Sqlocaml_encoding.Index_key
module Varint = Sqlocaml_encoding.Varint

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

(* #247: kill-switch for the cursor-level aggregate fast path.  Defaults on;
   [SQLOCAML_AGG_FASTPATH=0] forces the general [stream_aggregate] path (a safety
   valve, and the foil the Gc-gate regression test compares against).  Read
   per-call — an aggregate runs it once per query, never in a tight loop. *)
let agg_fastpath_enabled () =
  match Sys.getenv_opt "SQLOCAML_AGG_FASTPATH" with
  | Some ("0" | "false" | "off") -> false
  | _ -> true
;;

let lit_to_value : Ast.literal -> Row.value = function
  | Ast.L_int n -> Row.V_int n
  | Ast.L_text s -> Row.V_text s
  | Ast.L_null -> Row.V_null
  | Ast.L_real f -> Row.V_real f
  | Ast.L_blob b -> Row.V_blob b
  | Ast.L_current_timestamp | Ast.L_current_date | Ast.L_current_time ->
    failwith "lit_to_value: CURRENT_* should not appear as a plan literal"
;;

let value_to_literal : Row.value -> Ast.literal = function
  | Row.V_int n -> Ast.L_int n
  | Row.V_text s -> Ast.L_text s
  | Row.V_real f -> Ast.L_real f
  | Row.V_blob b -> Ast.L_blob b
  | Row.V_null -> Ast.L_null
;;

let row_value_to_index_value : Row.value -> Index_key.value = function
  | Row.V_int n -> Index_key.IK_int n
  | Row.V_text s -> Index_key.IK_text s
  | Row.V_null -> Index_key.IK_null
  | Row.V_real f -> Index_key.IK_real f
  | Row.V_blob b -> Index_key.IK_blob b
;;

let compare_values (a : Row.value) (b : Row.value) : int =
  match a, b with
  | Row.V_null, Row.V_null -> 0
  | Row.V_null, _ ->
    -1 (* NULLs sort first — less than any non-null value, matches SQLite *)
  | _, Row.V_null -> 1
  | Row.V_int x, Row.V_int y -> Int64.compare x y
  | Row.V_real x, Row.V_real y -> Float.compare x y
  | Row.V_text x, Row.V_text y -> String.compare x y
  | Row.V_blob x, Row.V_blob y -> Bytes.compare x y
  | _, _ -> 0 (* cross-type: shouldn't happen *)
;;

let compare_with_nulls
      (dir : [ `Asc | `Desc ])
      (nulls : [ `Nulls_first | `Nulls_last ])
      (va : Row.value)
      (vb : Row.value)
  : int
  =
  match va, vb with
  | Row.V_null, Row.V_null -> 0
  | Row.V_null, _ ->
    (match nulls with
     | `Nulls_first -> -1
     | `Nulls_last -> 1)
  | _, Row.V_null ->
    (match nulls with
     | `Nulls_first -> 1
     | `Nulls_last -> -1)
  | _, _ ->
    let c = compare_values va vb in
    (match dir with
     | `Asc -> c
     | `Desc -> -c)
;;

let list_drop n lst =
  let rec go k = function
    | [] -> []
    | _ :: t as l -> if k <= 0 then l else go (k - 1) t
  in
  go n lst
;;

let list_take n lst =
  let rec go k = function
    | [] -> []
    | h :: t -> if k <= 0 then [] else h :: go (k - 1) t
  in
  go n lst
;;

(** Find a column ordinal by name within a [Row.column] list. *)
let find_col_idx_by_name (cols : Row.column list) (name : string) : int =
  let rec find i = function
    | [] -> failwith (Printf.sprintf "column not found: %s" name)
    | (c : Row.column) :: _ when String.equal c.Row.name name -> i
    | _ :: rest -> find (i + 1) rest
  in
  find 0 cols
;;

(* Module-level cache for compiled CHECK expressions.
   Key: (table_name, column_ordinal, check_sql) → compiled Plan.expr.
   Including check_sql avoids stale hits when different tables share the same
   name and column index across DB instances (e.g. test isolation). *)
let check_expr_cache : (string * int * string, Plan.expr) Hashtbl.t = Hashtbl.create 16

(* ── DDL reconstruction for Op_sqlite_master ─────────────────── *)

let sql_of_row_type = function
  | Row.Integer -> "INTEGER"
  | Row.Text -> "TEXT"
  | Row.Real -> "REAL"
  | Row.Blob -> "BLOB"
;;

(* A SQL single-quoted string literal with embedded quotes doubled. *)
let quote_text_literal s = "'" ^ String.concat "''" (String.split_on_char '\'' s) ^ "'"

let sql_of_default_value = function
  | Row.DV_int n -> Int64.to_string n
  | Row.DV_text s -> quote_text_literal s
  | Row.DV_real f -> Printf.sprintf "%g" f
  | Row.DV_blob b ->
    let hex =
      Bytes.to_seq b
      |> Seq.map (fun c -> Printf.sprintf "%02X" (Char.code c))
      |> List.of_seq
      |> String.concat ""
    in
    Printf.sprintf "X'%s'" hex
  | Row.DV_null -> "NULL"
  | Row.DV_current_timestamp -> "CURRENT_TIMESTAMP"
  | Row.DV_current_date -> "CURRENT_DATE"
  | Row.DV_current_time -> "CURRENT_TIME"
;;

let sql_of_fk_action = function
  | Cat.FA_no_action -> "NO ACTION"
  | Cat.FA_restrict -> "RESTRICT"
  | Cat.FA_cascade -> "CASCADE"
  | Cat.FA_set_null -> "SET NULL"
  | Cat.FA_set_default -> "SET DEFAULT"
;;

(* Phase 35 task 3b: quote DDL identifiers that contain non-alphanumeric
   characters, start with a digit, or are empty.  Embedded double-quotes are
   doubled per SQL identifier syntax. *)
let needs_quoting s =
  String.length s = 0
  || (let c = s.[0] in
      not ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_'))
  || String.exists
       (fun c ->
          not
            ((c >= 'A' && c <= 'Z')
             || (c >= 'a' && c <= 'z')
             || (c >= '0' && c <= '9')
             || c = '_'))
       s
;;

let quote_ident s =
  if needs_quoting s
  then "\"" ^ String.concat "\"\"" (String.split_on_char '"' s) ^ "\""
  else s
;;

let ddl_of_table (meta : Cat.table_meta) =
  (* #299: the rowid-alias column carries the AUTOINCREMENT keyword in the dump. *)
  let autoinc_idx =
    if meta.Cat.autoincrement
    then
      Cat.compute_rowid_alias_col meta.Cat.columns ~without_rowid:meta.Cat.without_rowid
    else None
  in
  let col_parts =
    List.mapi
      (fun i (col : Row.column) ->
         let buf = Buffer.create 64 in
         Buffer.add_string buf (quote_ident col.Row.name);
         Buffer.add_char buf ' ';
         Buffer.add_string buf (sql_of_row_type col.Row.ty);
         if col.Row.not_null then Buffer.add_string buf " NOT NULL";
         if col.Row.primary_key
         then (
           Buffer.add_string buf " PRIMARY KEY";
           (* #312: a DESC PK is a non-alias; re-emit DESC so reopen reproduces
              the non-alias shape (hidden rowid + __pk index). *)
           if col.Row.pk_desc then Buffer.add_string buf " DESC";
           if Some i = autoinc_idx then Buffer.add_string buf " AUTOINCREMENT");
         (match col.Row.default with
          | None -> ()
          | Some dv ->
            Buffer.add_string buf " DEFAULT ";
            Buffer.add_string buf (sql_of_default_value dv));
         (match col.Row.check_sql with
          | None -> ()
          | Some sql ->
            Buffer.add_string buf " CHECK(";
            Buffer.add_string buf sql;
            Buffer.add_char buf ')');
         (match col.Row.generated_as with
          | None -> ()
          | Some (expr_sql, is_stored) ->
            Buffer.add_string buf " GENERATED ALWAYS AS (";
            Buffer.add_string buf expr_sql;
            Buffer.add_string buf ") ";
            Buffer.add_string buf (if is_stored then "STORED" else "VIRTUAL"));
         Buffer.contents buf)
      meta.Cat.columns
  in
  let fk_parts =
    List.map
      (fun (fk : Cat.fk_constraint) ->
         Printf.sprintf
           "FOREIGN KEY (%s) REFERENCES %s(%s) ON DELETE %s ON UPDATE %s"
           (String.concat ", " (List.map quote_ident fk.Cat.fk_local_cols))
           (quote_ident fk.Cat.fk_parent_table)
           (String.concat ", " (List.map quote_ident fk.Cat.fk_parent_cols))
           (sql_of_fk_action fk.Cat.fk_on_delete)
           (sql_of_fk_action fk.Cat.fk_on_update))
      meta.Cat.fk_constraints
  in
  Printf.sprintf
    "CREATE TABLE %s (%s)%s"
    (quote_ident meta.Cat.name)
    (String.concat ", " (col_parts @ fk_parts))
    (if meta.Cat.without_rowid then " WITHOUT ROWID" else "")
;;

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
      if i + 4 >= n
      then trigger_name
      else if
        upper.[i] = ' '
        && upper.[i + 1] = 'O'
        && upper.[i + 2] = 'N'
        && upper.[i + 3] = ' '
      then (
        (* Found " ON " — extract the identifier that follows *)
        let start = i + 4 in
        let j = ref start in
        while
          !j < n
          &&
          let c = upper.[!j] in
          (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'
        do
          incr j
        done;
        if !j > start then String.sub sql start (!j - start) else trigger_name)
      else search (i + 1)
    in
    search 0
;;

let ddl_of_index (idx : Cat.index_info) =
  let unique_kw = if idx.Cat.idx_unique then "UNIQUE " else "" in
  let col_strs =
    List.map2
      (fun col_sql is_expr ->
         if is_expr then Printf.sprintf "(%s)" col_sql else quote_ident col_sql)
      idx.Cat.idx_columns
      idx.Cat.idx_expr_flags
  in
  let cols_str = String.concat ", " col_strs in
  let where_clause =
    match idx.Cat.idx_where_sql with
    | None -> ""
    | Some sql -> Printf.sprintf " WHERE %s" sql
  in
  Printf.sprintf
    "CREATE %sINDEX %s ON %s (%s)%s"
    unique_kw
    (quote_ident idx.Cat.idx_name)
    (quote_ident idx.Cat.idx_table)
    cols_str
    where_clause
;;

let ddl_of_fts (m : Cat.fts_table_meta) =
  Printf.sprintf
    "CREATE VIRTUAL TABLE %s USING fts5(%s)"
    (quote_ident m.Cat.fts_name)
    (String.concat ", " (List.map quote_ident m.Cat.fts_columns))
;;

(* ------------------------------------------------------------------ *)
(* Expression evaluation                                                *)
(* (Defined before [execute] so that [Op_update] can evaluate WHERE     *)
(*  predicates and right-hand-side expressions for SET assignments.)    *)
(* ------------------------------------------------------------------ *)

let value_truthy : Row.value -> bool = function
  | Row.V_null | Row.V_int 0L -> false
  | _ -> true
;;

(* Pattern matching helpers for LIKE and GLOB.
   Uses naive recursive backtracking: worst case is O(2^k) for k '%'/'*'
   metacharacters against an adversarial string.  Acceptable for typical
   SQL workloads; replace with NFA/DP if adversarial patterns are a concern. *)
let rec like_match pat pi str si =
  let plen = String.length pat
  and slen = String.length str in
  if pi = plen
  then si = slen
  else (
    match pat.[pi] with
    | '%' ->
      like_match pat (pi + 1) str si || (si < slen && like_match pat pi str (si + 1))
    | '_' -> si < slen && like_match pat (pi + 1) str (si + 1)
    | c ->
      si < slen
      && Char.lowercase_ascii c = Char.lowercase_ascii str.[si]
      && like_match pat (pi + 1) str (si + 1))
;;

let rec glob_match pat pi str si =
  let plen = String.length pat
  and slen = String.length str in
  if pi = plen
  then si = slen
  else (
    match pat.[pi] with
    | '*' ->
      glob_match pat (pi + 1) str si || (si < slen && glob_match pat pi str (si + 1))
    | '?' -> si < slen && glob_match pat (pi + 1) str (si + 1)
    | c -> si < slen && c = str.[si] && glob_match pat (pi + 1) str (si + 1))
;;

let str_trim_spaces s =
  let n = String.length s in
  let l = ref 0
  and r = ref (n - 1) in
  while
    !l <= !r
    &&
    let c = s.[!l] in
    c = ' ' || c = '\t' || c = '\n' || c = '\r'
  do
    incr l
  done;
  while
    !r >= !l
    &&
    let c = s.[!r] in
    c = ' ' || c = '\t' || c = '\n' || c = '\r'
  do
    decr r
  done;
  if !l > !r then "" else String.sub s !l (!r - !l + 1)
;;

let parse_int_prefix s =
  let s = String.trim s in
  match Int64.of_string_opt s with
  | Some n -> n
  | None ->
    (match float_of_string_opt s with
     | Some f -> Int64.of_float f
     | None ->
       (* Scan leading numeric prefix: optional sign, digits, optional decimal *)
       let n = String.length s in
       let i = ref 0 in
       if !i < n && (s.[!i] = '-' || s.[!i] = '+') then incr i;
       let digit_start = !i in
       while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do
         incr i
       done;
       (* Include decimal part for float->int conversion *)
       let has_dot = !i < n && s.[!i] = '.' in
       if has_dot
       then (
         incr i;
         while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do
           incr i
         done);
       if !i > digit_start
       then (
         match float_of_string_opt (String.sub s 0 !i) with
         | Some f -> Int64.of_float f
         | None ->
           (match Int64.of_string_opt (String.sub s 0 !i) with
            | Some v -> v
            | None -> 0L))
       else 0L)
;;

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
      match float_of_string_opt (String.sub s 0 !i) with
      | Some f ->
        result := f;
        found := true
      | None -> decr i
    done;
    !result
;;

let str_trim_chars s chars =
  let n = String.length s in
  let l = ref 0
  and r = ref (n - 1) in
  while !l <= !r && String.contains chars s.[!l] do
    incr l
  done;
  while !r >= !l && String.contains chars s.[!r] do
    decr r
  done;
  if !l > !r then "" else String.sub s !l (!r - !l + 1)
;;

let str_ltrim_spaces s =
  let n = String.length s in
  let l = ref 0 in
  while
    !l < n
    &&
    let c = s.[!l] in
    c = ' ' || c = '\t' || c = '\n' || c = '\r'
  do
    incr l
  done;
  String.sub s !l (n - !l)
;;

let str_ltrim_chars s chars =
  let n = String.length s in
  let l = ref 0 in
  while !l < n && String.contains chars s.[!l] do
    incr l
  done;
  String.sub s !l (n - !l)
;;

let str_rtrim_spaces s =
  let n = String.length s in
  let r = ref (n - 1) in
  while
    !r >= 0
    &&
    let c = s.[!r] in
    c = ' ' || c = '\t' || c = '\n' || c = '\r'
  do
    decr r
  done;
  if !r < 0 then "" else String.sub s 0 (!r + 1)
;;

let str_rtrim_chars s chars =
  let r = ref (String.length s - 1) in
  while !r >= 0 && String.contains chars s.[!r] do
    decr r
  done;
  if !r < 0 then "" else String.sub s 0 (!r + 1)
;;

let str_replace s old rep =
  if String.length old = 0
  then s
  else (
    let buf = Buffer.create (String.length s) in
    let n = String.length s
    and m = String.length old in
    let i = ref 0 in
    while !i <= n - m do
      if String.sub s !i m = old
      then (
        Buffer.add_string buf rep;
        i := !i + m)
      else (
        Buffer.add_char buf s.[!i];
        incr i)
    done;
    while !i < n do
      Buffer.add_char buf s.[!i];
      incr i
    done;
    Buffer.contents buf)
;;

let str_instr s sub =
  let n = String.length s
  and m = String.length sub in
  if m = 0
  then 1
  else (
    let found = ref 0 in
    let i = ref 0 in
    while !found = 0 && !i <= n - m do
      if String.sub s !i m = sub then found := !i + 1 (* 1-indexed *) else incr i
    done;
    !found)
;;

let row_key (row : Row.t) : string =
  let buf = Buffer.create 64 in
  Array.iter
    (function
      | Row.V_null -> Buffer.add_string buf "N|"
      | Row.V_int n ->
        Buffer.add_char buf 'I';
        Buffer.add_string buf (Int64.to_string n);
        Buffer.add_char buf '|'
      | Row.V_real f ->
        Buffer.add_char buf 'R';
        Buffer.add_string buf (Printf.sprintf "%h" f);
        Buffer.add_char buf '|'
      | Row.V_text s ->
        Buffer.add_char buf 'T';
        Buffer.add_string buf (string_of_int (String.length s));
        Buffer.add_char buf ':';
        Buffer.add_string buf s;
        Buffer.add_char buf '|'
      | Row.V_blob b ->
        Buffer.add_char buf 'B';
        Buffer.add_string buf (string_of_int (Bytes.length b));
        Buffer.add_char buf ':';
        Buffer.add_bytes buf b;
        Buffer.add_char buf '|')
    row;
  Buffer.contents buf
;;

let json_of_sql : Row.value -> Json.value = function
  | Row.V_null -> Json.J_null
  | Row.V_int n -> Json.J_int n
  | Row.V_real f -> Json.J_float f
  | Row.V_text s -> Json.J_string s
  | Row.V_blob b -> Json.J_string (Bytes.to_string b)
;;

let sql_of_json : Json.value -> Row.value = function
  | Json.J_null -> Row.V_null
  | Json.J_bool b -> Row.V_int (if b then 1L else 0L)
  | Json.J_int n -> Row.V_int n
  | Json.J_float f -> Row.V_real f
  | Json.J_string s -> Row.V_text s
  | Json.J_array _ as v -> Row.V_text (Json.to_string v)
  | Json.J_object _ as v -> Row.V_text (Json.to_string v)
;;

(* ── Scalar-function evaluation, split by category (#168) ──────────
   [eval_func] dispatches to the [eval_*_func] helpers below; each returns
   [Some v] for the functions it owns and [None] otherwise, so [eval_func]
   can chain them and fall through to the arity-error case.  Verbose
   per-function bodies are themselves factored into small named helpers. *)

let hex_encode_str s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02X" (Char.code c))) s;
  Buffer.contents buf
;;

(* #264: render a value as a standalone SQL literal for a logical dump.
   The output must parse back to the identical value through our own executor:
   - text is single-quoted with embedded quotes doubled;
   - blobs use the [X'..'] hex syntax;
   - a finite float always carries a '.' or exponent so it re-reads as REAL
     (not INTEGER), and uses the shortest decimal that round-trips bit-for-bit;
   - non-finite floats map to [1e999]/[-1e999] (overflow to ±inf, as SQLite's
     own .dump emits) and NaN to NULL (SQLite cannot store a NaN). *)
let sql_literal_of_value : Row.value -> string = function
  | Row.V_null -> "NULL"
  | Row.V_int n -> Int64.to_string n
  | Row.V_text s -> quote_text_literal s
  | Row.V_blob b -> "X'" ^ hex_encode_str (Bytes.to_string b) ^ "'"
  | Row.V_real f ->
    if Float.is_nan f
    then "NULL"
    else if f = Float.infinity
    then "1e999"
    else if f = Float.neg_infinity
    then "-1e999"
    else (
      let rec shortest p =
        if p >= 17
        then Printf.sprintf "%.17g" f
        else (
          let s = Printf.sprintf "%.*g" p f in
          if float_of_string s = f then s else shortest (p + 1))
      in
      let s = shortest 1 in
      if String.contains s '.' || String.contains s 'e' || String.contains s 'E'
      then s
      else s ^ ".0")
;;

(* UTF-8 encode each in-range integer codepoint, mirroring SQLite's char(). *)
let char_encode args =
  let buf = Buffer.create 16 in
  List.iter
    (fun v ->
       match v with
       | Row.V_int n when n >= 1L && n <= 0x10FFFFL ->
         let cp = Int64.to_int n in
         if cp < 0x80
         then Buffer.add_char buf (Char.chr cp)
         else if cp < 0x800
         then (
           Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
           Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
         else if cp < 0x10000
         then (
           Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
           Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
           Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
         else (
           Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
           Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
           Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
           Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
       | _ -> ())
    args;
  Buffer.contents buf
;;

(* Decode the codepoint of the first UTF-8 character of [s] (s non-empty). *)
let unicode_codepoint s =
  let b0 = Char.code s.[0] in
  if b0 < 0x80
  then b0
  else if b0 < 0xE0 && String.length s >= 2
  then ((b0 land 0x1F) lsl 6) lor (Char.code s.[1] land 0x3F)
  else if b0 < 0xF0 && String.length s >= 3
  then
    ((b0 land 0x0F) lsl 12)
    lor ((Char.code s.[1] land 0x3F) lsl 6)
    lor (Char.code s.[2] land 0x3F)
  else if b0 >= 0xF0 && String.length s >= 4
  then
    ((b0 land 0x07) lsl 18)
    lor ((Char.code s.[1] land 0x3F) lsl 12)
    lor ((Char.code s.[2] land 0x3F) lsl 6)
    lor (Char.code s.[3] land 0x3F)
  else b0
;;

(* Emit one printf conversion [spec] (the char after '%') to [buf], pulling
   the next argument via [get_arg]. *)
let printf_emit buf spec (get_arg : unit -> Row.value) =
  match spec with
  | '%' -> Buffer.add_char buf '%'
  | 'd' | 'i' ->
    (match get_arg () with
     | Row.V_int n2 -> Buffer.add_string buf (Int64.to_string n2)
     | Row.V_real f -> Buffer.add_string buf (string_of_int (int_of_float f))
     | Row.V_text s ->
       (try Buffer.add_string buf (string_of_int (int_of_string s)) with
        | Failure _ -> ())
     | _ -> ())
  | 'f' ->
    (match get_arg () with
     | Row.V_real f -> Buffer.add_string buf (Printf.sprintf "%f" f)
     | Row.V_int n2 -> Buffer.add_string buf (Printf.sprintf "%f" (Int64.to_float n2))
     | _ -> ())
  | 'e' ->
    (match get_arg () with
     | Row.V_real f -> Buffer.add_string buf (Printf.sprintf "%e" f)
     | Row.V_int n2 -> Buffer.add_string buf (Printf.sprintf "%e" (Int64.to_float n2))
     | _ -> ())
  | 'g' ->
    (match get_arg () with
     | Row.V_real f -> Buffer.add_string buf (Printf.sprintf "%g" f)
     | Row.V_int n2 -> Buffer.add_string buf (Printf.sprintf "%g" (Int64.to_float n2))
     | _ -> ())
  | 's' ->
    (match get_arg () with
     | Row.V_text s -> Buffer.add_string buf s
     | Row.V_int n2 -> Buffer.add_string buf (Int64.to_string n2)
     | Row.V_real f -> Buffer.add_string buf (Printf.sprintf "%g" f)
     | Row.V_null -> Buffer.add_string buf "NULL"
     | Row.V_blob _ -> Buffer.add_string buf "")
  | 'q' ->
    (match get_arg () with
     | Row.V_text s ->
       String.iter
         (fun c -> if c = '\'' then Buffer.add_string buf "''" else Buffer.add_char buf c)
         s
     | Row.V_int n2 -> Buffer.add_string buf (Int64.to_string n2)
     | Row.V_real f -> Buffer.add_string buf (Printf.sprintf "%g" f)
     | Row.V_null -> Buffer.add_string buf "NULL"
     | Row.V_blob _ -> ())
  | c ->
    Buffer.add_char buf '%';
    Buffer.add_char buf c
;;

(* SQLite printf()/format(): a small subset of C printf conversions. *)
let printf_format fmt rest =
  let args_arr = Array.of_list rest in
  let arg_idx = ref 0 in
  let get_arg () =
    let v =
      if !arg_idx < Array.length args_arr then args_arr.(!arg_idx) else Row.V_null
    in
    incr arg_idx;
    v
  in
  let buf = Buffer.create 64 in
  let n = String.length fmt in
  let i = ref 0 in
  while !i < n do
    if fmt.[!i] = '%'
    then (
      incr i;
      if !i < n
      then (
        printf_emit buf fmt.[!i] get_arg;
        incr i))
    else (
      Buffer.add_char buf fmt.[!i];
      incr i)
  done;
  Buffer.contents buf
;;

let eval_str_func (func : Ast.scalar_func) (args : Row.value list) : Row.value option =
  match func, args with
  | Ast.Fn_length, [ Row.V_text s ] -> Some (Row.V_int (Int64.of_int (String.length s)))
  | Ast.Fn_length, [ Row.V_blob b ] -> Some (Row.V_int (Int64.of_int (Bytes.length b)))
  | Ast.Fn_length, [ Row.V_null ] -> Some Row.V_null
  | Ast.Fn_length, [ _ ] -> Some Row.V_null (* non-text/blob: return null like SQLite *)
  | Ast.Fn_lower, [ Row.V_text s ] -> Some (Row.V_text (String.lowercase_ascii s))
  | Ast.Fn_lower, [ Row.V_null ] -> Some Row.V_null
  | Ast.Fn_lower, [ _ ] -> Some Row.V_null
  | Ast.Fn_upper, [ Row.V_text s ] -> Some (Row.V_text (String.uppercase_ascii s))
  | Ast.Fn_upper, [ Row.V_null ] -> Some Row.V_null
  | Ast.Fn_upper, [ _ ] -> Some Row.V_null
  | Ast.Fn_substr, Row.V_text s :: rest ->
    Some
      (match rest with
       | [ Row.V_int start ] ->
         let i = max 0 (Int64.to_int start - 1) in
         if i >= String.length s
         then Row.V_text ""
         else Row.V_text (String.sub s i (String.length s - i))
       | [ Row.V_int start; Row.V_int len ] ->
         let i = max 0 (Int64.to_int start - 1) in
         let l = Int64.to_int len in
         if i >= String.length s || l <= 0
         then Row.V_text ""
         else Row.V_text (String.sub s i (min l (String.length s - i)))
       | _ -> Row.V_null)
  | Ast.Fn_substr, Row.V_null :: _ -> Some Row.V_null
  | Ast.Fn_trim, [ Row.V_text s ] -> Some (Row.V_text (str_trim_spaces s))
  | Ast.Fn_trim, [ Row.V_text s; Row.V_text chars ] ->
    Some (Row.V_text (str_trim_chars s chars))
  | Ast.Fn_trim, [ _; Row.V_null ] -> Some Row.V_null
  | Ast.Fn_trim, Row.V_null :: _ -> Some Row.V_null
  | Ast.Fn_ltrim, [ Row.V_text s ] -> Some (Row.V_text (str_ltrim_spaces s))
  | Ast.Fn_ltrim, [ Row.V_text s; Row.V_text chars ] ->
    Some (Row.V_text (str_ltrim_chars s chars))
  | Ast.Fn_ltrim, [ _; Row.V_null ] -> Some Row.V_null
  | Ast.Fn_ltrim, Row.V_null :: _ -> Some Row.V_null
  | Ast.Fn_rtrim, [ Row.V_text s ] -> Some (Row.V_text (str_rtrim_spaces s))
  | Ast.Fn_rtrim, [ Row.V_text s; Row.V_text chars ] ->
    Some (Row.V_text (str_rtrim_chars s chars))
  | Ast.Fn_rtrim, [ _; Row.V_null ] -> Some Row.V_null
  | Ast.Fn_rtrim, Row.V_null :: _ -> Some Row.V_null
  | Ast.Fn_replace, [ Row.V_text s; Row.V_text old; Row.V_text rep ] ->
    Some (Row.V_text (str_replace s old rep))
  | Ast.Fn_replace, [ _; Row.V_null; _ ] -> Some Row.V_null
  | Ast.Fn_replace, [ _; _; Row.V_null ] -> Some Row.V_null
  | Ast.Fn_replace, Row.V_null :: _ -> Some Row.V_null
  | Ast.Fn_instr, [ Row.V_text s; Row.V_text sub ] ->
    Some (Row.V_int (Int64.of_int (str_instr s sub)))
  | Ast.Fn_instr, Row.V_null :: _ | Ast.Fn_instr, [ _; Row.V_null ] -> Some Row.V_null
  | Ast.Fn_hex, [ Row.V_blob b ] -> Some (Row.V_text (hex_encode_str (Bytes.to_string b)))
  | Ast.Fn_hex, [ Row.V_text s ] -> Some (Row.V_text (hex_encode_str s))
  | Ast.Fn_hex, [ Row.V_int n ] -> Some (Row.V_text (hex_encode_str (Int64.to_string n)))
  | Ast.Fn_hex, [ Row.V_null ] -> Some (Row.V_text "")
  | Ast.Fn_char, args -> Some (Row.V_text (char_encode args))
  | Ast.Fn_unicode, [ Row.V_text s ] when String.length s > 0 ->
    Some (Row.V_int (Int64.of_int (unicode_codepoint s)))
  | Ast.Fn_unicode, [ Row.V_text _ ] -> Some Row.V_null
  | Ast.Fn_unicode, [ Row.V_null ] -> Some Row.V_null
  | Ast.Fn_printf, Row.V_text fmt :: rest -> Some (Row.V_text (printf_format fmt rest))
  | Ast.Fn_printf, _ -> Some Row.V_null
  | _ -> None
;;

let eval_math_func (func : Ast.scalar_func) (args : Row.value list) : Row.value option =
  let to_float_opt = function
    | Row.V_real f -> Some f
    | Row.V_int n -> Some (Int64.to_float n)
    | _ -> None
  in
  match func, args with
  | Ast.Fn_abs, [ Row.V_int n ] -> Some (Row.V_int (Int64.abs n))
  | Ast.Fn_abs, [ Row.V_real f ] -> Some (Row.V_real (Float.abs f))
  | Ast.Fn_abs, [ Row.V_null ] -> Some Row.V_null
  | Ast.Fn_abs, [ _ ] -> Some Row.V_null
  | Ast.Fn_round, [ Row.V_real f ] -> Some (Row.V_real (Float.round f))
  | Ast.Fn_round, [ Row.V_int n ] -> Some (Row.V_real (Int64.to_float n))
  | Ast.Fn_round, [ Row.V_real f; Row.V_int d ] ->
    let factor = 10. ** Int64.to_float d in
    Some (Row.V_real (Float.round (f *. factor) /. factor))
  | Ast.Fn_round, [ Row.V_int n; Row.V_int _ ] -> Some (Row.V_real (Int64.to_float n))
  | Ast.Fn_round, [ _; Row.V_null ] -> Some Row.V_null
  | Ast.Fn_round, Row.V_null :: _ -> Some Row.V_null
  | Ast.Fn_ceil, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.ceil f)
       | None -> Row.V_null)
  | Ast.Fn_floor, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.floor f)
       | None -> Row.V_null)
  | Ast.Fn_sqrt, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.sqrt f)
       | None -> Row.V_null)
  | Ast.Fn_pow, [ b; e ] ->
    Some
      (match to_float_opt b, to_float_opt e with
       | Some bf, Some ef -> Row.V_real (bf ** ef)
       | _ -> Row.V_null)
  | Ast.Fn_exp, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.exp f)
       | None -> Row.V_null)
  | Ast.Fn_ln, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.log f)
       | None -> Row.V_null)
  | Ast.Fn_log, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.log f)
       | None -> Row.V_null)
  | Ast.Fn_log, [ b; x ] ->
    Some
      (match to_float_opt b, to_float_opt x with
       | Some bf, Some xf -> Row.V_real (Float.log xf /. Float.log bf)
       | _ -> Row.V_null)
  | Ast.Fn_log2, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.log f /. Float.log 2.0)
       | None -> Row.V_null)
  | Ast.Fn_log10, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.log10 f)
       | None -> Row.V_null)
  | Ast.Fn_sign, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_int (if f > 0.0 then 1L else if f < 0.0 then -1L else 0L)
       | None -> Row.V_null)
  | Ast.Fn_trunc, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (if f >= 0.0 then Float.floor f else Float.ceil f)
       | None -> Row.V_null)
  | Ast.Fn_trunc, [ v; d ] ->
    Some
      (match to_float_opt v, to_float_opt d with
       | Some f, Some df ->
         let factor = 10.0 ** Float.round df in
         let fx = f *. factor in
         Row.V_real ((if fx >= 0.0 then Float.floor fx else Float.ceil fx) /. factor)
       | _ -> Row.V_null)
  | Ast.Fn_pi, [] -> Some (Row.V_real Float.pi)
  | Ast.Fn_sin, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.sin f)
       | None -> Row.V_null)
  | Ast.Fn_cos, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.cos f)
       | None -> Row.V_null)
  | Ast.Fn_tan, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.tan f)
       | None -> Row.V_null)
  | Ast.Fn_asin, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.asin f)
       | None -> Row.V_null)
  | Ast.Fn_acos, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.acos f)
       | None -> Row.V_null)
  | Ast.Fn_atan, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.atan f)
       | None -> Row.V_null)
  | Ast.Fn_atan2, [ y; x ] ->
    Some
      (match to_float_opt y, to_float_opt x with
       | Some yf, Some xf -> Row.V_real (Float.atan2 yf xf)
       | _ -> Row.V_null)
  | Ast.Fn_degrees, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (f *. 180.0 /. Float.pi)
       | None -> Row.V_null)
  | Ast.Fn_radians, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (f *. Float.pi /. 180.0)
       | None -> Row.V_null)
  | _ -> None
;;

(* date/time/datetime/julianday/unixepoch share arg-shape handling; only the
   final conversion differs. *)
let eval_datetime_unary clock args (conv : Datetime.dt -> Row.value) : Row.value =
  match args with
  | [] | [ Row.V_null ] -> Row.V_null
  | Row.V_null :: _ -> Row.V_null
  | Row.V_text ts :: rest ->
    if rest <> []
    then Row.V_null
    else (
      match Datetime.parse ?now:clock ts with
      | Error _ -> Row.V_null
      | Ok dt -> conv dt)
  | _ -> Row.V_null
;;

let eval_datetime_func clock (func : Ast.scalar_func) (args : Row.value list)
  : Row.value option
  =
  match func with
  | Ast.Fn_date ->
    Some (eval_datetime_unary clock args (fun dt -> Row.V_text (Datetime.to_date dt)))
  | Ast.Fn_time ->
    Some (eval_datetime_unary clock args (fun dt -> Row.V_text (Datetime.to_time dt)))
  | Ast.Fn_datetime ->
    Some (eval_datetime_unary clock args (fun dt -> Row.V_text (Datetime.to_datetime dt)))
  | Ast.Fn_julianday ->
    Some
      (eval_datetime_unary clock args (fun dt -> Row.V_real (Datetime.to_julianday dt)))
  | Ast.Fn_unixepoch ->
    Some (eval_datetime_unary clock args (fun dt -> Row.V_int (Datetime.to_unixepoch dt)))
  | Ast.Fn_strftime ->
    Some
      (match args with
       | Row.V_text fmt :: Row.V_text ts :: rest ->
         if rest <> []
         then Row.V_null
         else (
           match Datetime.parse ?now:clock ts with
           | Error _ -> Row.V_null
           | Ok dt -> Row.V_text (Datetime.strftime fmt dt))
       | _ -> Row.V_null)
  | _ -> None
;;

(* json_set/insert/replace differ only in the per-path Json.path_* operation. *)
let json_modify (path_op : Json.value -> string -> Json.value -> Json.value) json_v rest
  : Row.value
  =
  let json_s =
    match json_v with
    | Row.V_text s -> s
    | _ -> ""
  in
  match Json.parse json_s with
  | Error _ -> Row.V_null
  | Ok jv ->
    let rec apply jv = function
      | path_v :: val_v :: rest ->
        let path =
          match path_v with
          | Row.V_text s -> s
          | _ -> ""
        in
        apply (path_op jv path (json_of_sql val_v)) rest
      | _ -> jv
    in
    Row.V_text (Json.to_string (apply jv rest))
;;

let eval_json_func (func : Ast.scalar_func) (args : Row.value list) : Row.value option =
  match func, args with
  | Ast.Fn_json_extract, [ json_v; path_v ] ->
    let json_s =
      match json_v with
      | Row.V_text s -> s
      | _ -> ""
    in
    let path_s =
      match path_v with
      | Row.V_text s -> s
      | _ -> ""
    in
    Some
      (match Json.parse json_s with
       | Error _ -> Row.V_null
       | Ok jv ->
         (match Json.path_get jv path_s with
          | None -> Row.V_null
          | Some v -> sql_of_json v))
  | Ast.Fn_json_object, pairs ->
    if List.length pairs mod 2 <> 0
    then Some Row.V_null
    else (
      let rec make_pairs = function
        | [] -> []
        | k :: v :: rest ->
          let key =
            match k with
            | Row.V_text s -> s
            | _ -> ""
          in
          (key, json_of_sql v) :: make_pairs rest
        | [ _ ] -> assert false
      in
      Some (Row.V_text (Json.to_string (Json.J_object (make_pairs pairs)))))
  | Ast.Fn_json_array, elems ->
    Some (Row.V_text (Json.to_string (Json.J_array (List.map json_of_sql elems))))
  | Ast.Fn_json_type, [ json_v ] ->
    Some
      (match json_v with
       | Row.V_text s ->
         (match Json.parse s with
          | Error _ -> Row.V_null
          | Ok jv -> Row.V_text (Json.type_name jv))
       | _ -> Row.V_null)
  | Ast.Fn_json_type, [ json_v; path_v ] ->
    Some
      (match json_v, path_v with
       | Row.V_text s, Row.V_text path ->
         (match Json.parse s with
          | Error _ -> Row.V_null
          | Ok jv ->
            (match Json.path_get jv path with
             | None -> Row.V_null
             | Some sub -> Row.V_text (Json.type_name sub)))
       | _ -> Row.V_null)
  | Ast.Fn_json_valid, [ json_v ] ->
    Some
      (match json_v with
       | Row.V_null -> Row.V_null
       | Row.V_text s ->
         (match Json.parse s with
          | Ok _ -> Row.V_int 1L
          | Error _ -> Row.V_int 0L)
       | _ -> Row.V_int 0L)
  | Ast.Fn_json_set, json_v :: rest -> Some (json_modify Json.path_set json_v rest)
  | Ast.Fn_json_insert, json_v :: rest -> Some (json_modify Json.path_insert json_v rest)
  | Ast.Fn_json_replace, json_v :: rest ->
    Some (json_modify Json.path_replace json_v rest)
  | Ast.Fn_json_remove, json_v :: paths ->
    let json_s =
      match json_v with
      | Row.V_text s -> s
      | _ -> ""
    in
    Some
      (match Json.parse json_s with
       | Error _ -> Row.V_null
       | Ok jv ->
         let result =
           List.fold_left
             (fun acc path_v ->
                let path =
                  match path_v with
                  | Row.V_text s -> s
                  | _ -> ""
                in
                Json.path_remove acc path)
             jv
             paths
         in
         Row.V_text (Json.to_string result))
  | _ -> None
;;

let eval_misc_func (func : Ast.scalar_func) (args : Row.value list) : Row.value option =
  match func, args with
  | Ast.Fn_coalesce, vs ->
    Some
      (match List.find_opt (fun v -> v <> Row.V_null) vs with
       | Some v -> v
       | None -> Row.V_null)
  | Ast.Fn_ifnull, [ a; b ] ->
    Some
      (match a with
       | Row.V_null -> b
       | v -> v)
  | Ast.Fn_typeof, [ v ] ->
    Some
      (Row.V_text
         (match v with
          | Row.V_int _ -> "integer"
          | Row.V_real _ -> "real"
          | Row.V_text _ -> "text"
          | Row.V_blob _ -> "blob"
          | Row.V_null -> "null"))
  | Ast.Fn_zeroblob, [ Row.V_int n ] when n >= 0L ->
    Some (Row.V_blob (Bytes.make (Int64.to_int n) '\000'))
  | Ast.Fn_zeroblob, _ -> Some Row.V_null
  | Ast.Fn_random, [] ->
    let b0 = Int64.of_int (Random.bits ()) in
    let b1 = Int64.of_int (Random.bits ()) in
    let b2 = Int64.of_int (Random.bits ()) in
    let sign = if Random.bool () then Int64.min_int else 0L in
    let v =
      Int64.logor
        sign
        (Int64.logor (Int64.shift_left b2 60) (Int64.logor (Int64.shift_left b1 30) b0))
    in
    Some (Row.V_int v)
  | Ast.Fn_random, _ -> Some Row.V_null
  (* SQLite always generates at least 1 byte, even for n <= 0.
     Clamp to [1, Sys.max_string_length] to avoid allocation errors. *)
  | Ast.Fn_randomblob, [ Row.V_int n ] ->
    let sz =
      max
        1
        (if n < 0L || n > Int64.of_int Sys.max_string_length then 1 else Int64.to_int n)
    in
    Some (Row.V_blob (Bytes.init sz (fun _ -> Char.chr (Random.int 256))))
  | Ast.Fn_randomblob, _ -> Some Row.V_null
  | Ast.Fn_changes, [] -> Some (Row.V_int 0L)
  | Ast.Fn_changes, _ -> Some Row.V_null
  | Ast.Fn_last_insert_rowid, [] -> Some (Row.V_int 0L)
  | Ast.Fn_last_insert_rowid, _ -> Some Row.V_null
  | Ast.Fn_total_changes, [] -> Some (Row.V_int 0L)
  | Ast.Fn_total_changes, _ -> Some Row.V_null
  | Ast.Fn_sqlite_version, [] -> Some (Row.V_text "3.45.0-sqlocaml")
  | Ast.Fn_sqlite_version, _ -> Some Row.V_null
  | _ -> None
;;

(* CAST evaluation; [v] is the already-evaluated operand. NULL casts to NULL. *)
let eval_cast (v : Row.value) (ty : Ast.ty) : Row.value =
  match v with
  | Row.V_null -> Row.V_null
  | _ ->
    (match ty with
     | Ast.Ty_int ->
       (match v with
        | Row.V_int n -> Row.V_int n
        | Row.V_real f -> Row.V_int (Int64.of_float f)
        | Row.V_text s -> Row.V_int (parse_int_prefix s)
        | Row.V_blob _ -> Row.V_int 0L
        | Row.V_null -> assert false)
     | Ast.Ty_real ->
       (match v with
        | Row.V_int n -> Row.V_real (Int64.to_float n)
        | Row.V_real f -> Row.V_real f
        | Row.V_text s -> Row.V_real (parse_real_prefix s)
        | Row.V_blob _ -> Row.V_real 0.0
        | Row.V_null -> assert false)
     | Ast.Ty_text ->
       (match v with
        | Row.V_int n -> Row.V_text (Int64.to_string n)
        | Row.V_real f ->
          (* SQLite appends ".0" when the %.15g result has no decimal point
             or exponent, so that CAST(1.0 AS TEXT) → "1.0" not "1". *)
          let s = Printf.sprintf "%.15g" f in
          let needs_dot =
            not
              (String.contains s '.'
               || String.contains s 'e'
               || String.contains s 'E'
               || String.contains s 'n')
          in
          Row.V_text (if needs_dot then s ^ ".0" else s)
        | Row.V_text s -> Row.V_text s
        | Row.V_blob b -> Row.V_text (Bytes.to_string b)
        | Row.V_null -> assert false)
     | Ast.Ty_blob ->
       (match v with
        | Row.V_blob b -> Row.V_blob b
        | Row.V_text s -> Row.V_blob (Bytes.of_string s)
        | Row.V_int n -> Row.V_blob (Bytes.of_string (Int64.to_string n))
        | Row.V_real f -> Row.V_blob (Bytes.of_string (Printf.sprintf "%.15g" f))
        | Row.V_null -> assert false))
;;

(* Bitwise binops: result is NULL unless both operands are integers. *)
let int_bitop lv rv f =
  match lv, rv with
  | Row.V_int a, Row.V_int b -> Row.V_int (f a b)
  | _ -> Row.V_null
;;

let rec eval_expr
          (clock : (unit -> float) option)
          (params : Row.value array)
          (row : Row.t)
          (e : Plan.expr)
  : Row.value
  =
  match e with
  | Plan.P_lit l -> lit_to_value l
  | Plan.P_col i -> row.(i)
  | Plan.P_param i -> if i < Array.length params then params.(i) else Row.V_null
  | Plan.P_neg e ->
    (match eval_expr clock params row e with
     | Row.V_int n -> Row.V_int (Int64.neg n)
     | Row.V_real f -> Row.V_real (-.f)
     | Row.V_null -> Row.V_null
     | _ -> failwith "unary minus requires numeric operand")
  | Plan.P_bitnot e ->
    (match eval_expr clock params row e with
     | Row.V_int n -> Row.V_int (Int64.lognot n)
     | Row.V_null -> Row.V_null
     | _ -> Row.V_null)
  | Plan.P_between (x, lo, hi) ->
    let vx = eval_expr clock params row x in
    let vlo = eval_expr clock params row lo in
    let vhi = eval_expr clock params row hi in
    (match vx, vlo, vhi with
     | Row.V_null, _, _ | _, Row.V_null, _ | _, _, Row.V_null -> Row.V_null
     | _ ->
       let ge_lo = compare_values vx vlo >= 0 in
       let le_hi = compare_values vx vhi <= 0 in
       Row.V_int (if ge_lo && le_hi then 1L else 0L))
  | Plan.P_in (x, vals) -> eval_in clock params row x vals
  | Plan.P_is_null e ->
    (match eval_expr clock params row e with
     | Row.V_null -> Row.V_int 1L
     | _ -> Row.V_int 0L)
  | Plan.P_is_not_null e ->
    (match eval_expr clock params row e with
     | Row.V_null -> Row.V_int 0L
     | _ -> Row.V_int 1L)
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
    let nocase_text v =
      match v with
      | Row.V_text s -> Row.V_text (String.lowercase_ascii s)
      | o -> o
    in
    let lv', rv' =
      if is_nocase lhs_e
      then lv, nocase_text rv
      else if is_nocase rhs_e
      then nocase_text lv, rv
      else lv, rv
    in
    eval_binop op lv' rv'
  | Plan.P_func (func, args) ->
    eval_func clock func (List.map (eval_expr clock params row) args)
  | Plan.P_case { scrutinee; branches; else_ } ->
    eval_case_expr clock params row scrutinee branches else_
  | Plan.P_cast (e, ty) -> eval_cast (eval_expr clock params row e) ty
  | Plan.P_subquery _ | Plan.P_exists _ | Plan.P_in_select _ ->
    (* These are replaced by pre_eval_subquery before row evaluation. *)
    Row.V_null
  | Plan.P_excluded_col _ ->
    failwith "Exec: P_excluded_col in eval_expr — must be substituted before evaluation"
  | Plan.P_window_slot _ ->
    failwith
      "Exec: P_window_slot in eval_expr — must be substituted by planner before \
       evaluation"
  | Plan.P_collate (e, Ast.Collate_nocase) ->
    let v = eval_expr clock params row e in
    (match v with
     | Row.V_text s -> Row.V_text (String.lowercase_ascii s)
     | o -> o)
  | Plan.P_collate (e, _) ->
    eval_expr clock params row e (* Collate_binary and Collate_rtrim are identity *)

and eval_in clock params row x vals =
  let vx = eval_expr clock params row x in
  if vx = Row.V_null
  then Row.V_null
  else (
    let result =
      List.fold_left
        (fun acc ve ->
           let v = eval_expr clock params row ve in
           match acc with
           | `Found -> `Found
           | _ when v = Row.V_null -> `Maybe
           | _ when compare_values vx v = 0 -> `Found
           | acc -> acc)
        `Not_found
        vals
    in
    match result with
    | `Found -> Row.V_int 1L
    | `Maybe -> Row.V_null
    | `Not_found -> Row.V_int 0L)

and eval_case_expr clock params row scrutinee branches else_ =
  let scr_val = Option.map (eval_expr clock params row) scrutinee in
  let rec find_match = function
    | [] ->
      (match else_ with
       | None -> Row.V_null
       | Some e -> eval_expr clock params row e)
    | (cond, result) :: rest ->
      let matched =
        match scr_val with
        | None -> value_truthy (eval_expr clock params row cond)
        | Some sv ->
          let cv = eval_expr clock params row cond in
          (match sv, cv with
           | Row.V_null, _ | _, Row.V_null -> false
           | _ -> compare_values sv cv = 0)
      in
      if matched then eval_expr clock params row result else find_match rest
  in
  find_match branches

and eval_func
      (clock : (unit -> float) option)
      (func : Ast.scalar_func)
      (args : Row.value list)
  : Row.value
  =
  match eval_str_func func args with
  | Some v -> v
  | None ->
    (match eval_math_func func args with
     | Some v -> v
     | None ->
       (match eval_datetime_func clock func args with
        | Some v -> v
        | None ->
          (match eval_json_func func args with
           | Some v -> v
           | None ->
             (match eval_misc_func func args with
              | Some v -> v
              | None ->
                failwith
                  "scalar_func: unexpected argument count (arity check should have \
                   caught this)"))))

and eval_binop (op : Plan.binop) (lv : Row.value) (rv : Row.value) : Row.value =
  match op with
  | Plan.And ->
    let lt = value_truthy lv
    and rt = value_truthy rv in
    let ln = lv = Row.V_null
    and rn = rv = Row.V_null in
    if lt && rt
    then Row.V_int 1L
    else if ((not ln) && not lt) || ((not rn) && not rt)
    then Row.V_int 0L
    else Row.V_null
  | Plan.Or ->
    let lt = value_truthy lv
    and rt = value_truthy rv in
    let ln = lv = Row.V_null
    and rn = rv = Row.V_null in
    if lt || rt
    then Row.V_int 1L
    else if (not ln) && not rn
    then Row.V_int 0L
    else Row.V_null
  (* NULL compared with anything yields NULL (3-valued logic). Cross-type → false. *)
  | Plan.Eq ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_int x, Row.V_int y -> if Int64.equal x y then Row.V_int 1L else Row.V_int 0L
     | Row.V_text x, Row.V_text y ->
       if String.equal x y then Row.V_int 1L else Row.V_int 0L
     | Row.V_real x, Row.V_real y ->
       if Float.equal x y then Row.V_int 1L else Row.V_int 0L
     | Row.V_blob x, Row.V_blob y ->
       if Bytes.equal x y then Row.V_int 1L else Row.V_int 0L
     | _ -> Row.V_int 0L)
  | Plan.Ne ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_int x, Row.V_int y -> if Int64.equal x y then Row.V_int 0L else Row.V_int 1L
     | Row.V_text x, Row.V_text y ->
       if String.equal x y then Row.V_int 0L else Row.V_int 1L
     | Row.V_real x, Row.V_real y ->
       if Float.equal x y then Row.V_int 0L else Row.V_int 1L
     | Row.V_blob x, Row.V_blob y ->
       if Bytes.equal x y then Row.V_int 0L else Row.V_int 1L
     | _ -> Row.V_int 0L)
  | Plan.Lt -> cmp_result lv rv (fun c -> c < 0)
  | Plan.Le -> cmp_result lv rv (fun c -> c <= 0)
  | Plan.Gt -> cmp_result lv rv (fun c -> c > 0)
  | Plan.Ge -> cmp_result lv rv (fun c -> c >= 0)
  | Plan.Add -> arith_op lv rv Int64.add ( +. )
  | Plan.Sub -> arith_op lv rv Int64.sub ( -. )
  | Plan.Mul -> arith_op lv rv Int64.mul ( *. )
  | Plan.Div ->
    arith_op
      lv
      rv
      (fun a b -> if Int64.equal b 0L then failwith "division by zero" else Int64.div a b)
      ( /. )
  | Plan.Concat ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_text a, Row.V_text b -> Row.V_text (a ^ b)
     | Row.V_text a, Row.V_int n -> Row.V_text (a ^ Int64.to_string n)
     | Row.V_int n, Row.V_text b -> Row.V_text (Int64.to_string n ^ b)
     | Row.V_int a, Row.V_int b -> Row.V_text (Int64.to_string a ^ Int64.to_string b)
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
  | Plan.Bit_and -> int_bitop lv rv Int64.logand
  | Plan.Bit_or -> int_bitop lv rv Int64.logor
  | Plan.Lshift ->
    int_bitop lv rv (fun a b ->
      let n = Int64.to_int b in
      if n < 0 || n >= 64 then 0L else Int64.shift_left a n)
  | Plan.Rshift ->
    int_bitop lv rv (fun a b ->
      let n = Int64.to_int b in
      if n < 0 || n >= 64 then 0L else Int64.shift_right a n)
  | Plan.Like ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_text str, Row.V_text pat ->
       Row.V_int
         (if like_match (String.lowercase_ascii pat) 0 (String.lowercase_ascii str) 0
          then 1L
          else 0L)
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
  | Row.V_int _, Row.V_int _
  | Row.V_text _, Row.V_text _
  | Row.V_real _, Row.V_real _
  | Row.V_blob _, Row.V_blob _ ->
    if pred (compare_values lv rv) then Row.V_int 1L else Row.V_int 0L
  (* Cross-type numeric comparisons: promote int to float *)
  | Row.V_real a, Row.V_int b ->
    let c = Float.compare a (Int64.to_float b) in
    if pred c then Row.V_int 1L else Row.V_int 0L
  | Row.V_int a, Row.V_real b ->
    let c = Float.compare (Int64.to_float a) b in
    if pred c then Row.V_int 1L else Row.V_int 0L
  | _ -> Row.V_int 0L (* cross-type comparisons are false *)

and arith_op lv rv int_f float_f =
  match lv, rv with
  | Row.V_null, _ | _, Row.V_null -> Row.V_null
  | Row.V_int a, Row.V_int b -> Row.V_int (int_f a b)
  | Row.V_real a, Row.V_real b -> Row.V_real (float_f a b)
  | Row.V_int a, Row.V_real b -> Row.V_real (float_f (Int64.to_float a) b)
  | Row.V_real a, Row.V_int b -> Row.V_real (float_f a (Int64.to_float b))
  | _ -> failwith "arithmetic on non-numeric operands"
;;

let project_row (ords : int list) (row : Row.t) : Row.t =
  Array.of_list (List.map (fun i -> row.(i)) ords)
;;

(* ------------------------------------------------------------------ *)
(* CHECK constraint evaluation                                          *)
(* ------------------------------------------------------------------ *)

let ast_binop_to_plan : Ast.binop -> Plan.binop = function
  | Ast.Eq -> Plan.Eq
  | Ast.Ne -> Plan.Ne
  | Ast.Lt -> Plan.Lt
  | Ast.Le -> Plan.Le
  | Ast.Gt -> Plan.Gt
  | Ast.Ge -> Plan.Ge
  | Ast.Add -> Plan.Add
  | Ast.Sub -> Plan.Sub
  | Ast.Mul -> Plan.Mul
  | Ast.Div -> Plan.Div
  | Ast.And -> Plan.And
  | Ast.Or -> Plan.Or
  | Ast.Concat -> Plan.Concat
  | Ast.Mod -> Plan.Mod
  | Ast.Bit_and -> Plan.Bit_and
  | Ast.Bit_or -> Plan.Bit_or
  | Ast.Lshift -> Plan.Lshift
  | Ast.Rshift -> Plan.Rshift
  | Ast.Like -> Plan.Like
  | Ast.Glob -> Plan.Glob
;;

let rec ast_expr_to_plan_check (columns : Row.column list) (e : Ast.expr) : Plan.expr =
  match e with
  | Ast.E_lit l -> Plan.P_lit l
  | Ast.E_col name -> Plan.P_col (find_col_idx_by_name columns name)
  | Ast.E_tbl_col (_, name) -> Plan.P_col (find_col_idx_by_name columns name)
  | Ast.E_binop (op, a, b) ->
    Plan.P_binop
      ( ast_binop_to_plan op
      , ast_expr_to_plan_check columns a
      , ast_expr_to_plan_check columns b )
  | Ast.E_not e -> Plan.P_not (ast_expr_to_plan_check columns e)
  | Ast.E_is_null e -> Plan.P_is_null (ast_expr_to_plan_check columns e)
  | Ast.E_is_not_null e -> Plan.P_is_not_null (ast_expr_to_plan_check columns e)
  | Ast.E_neg e -> Plan.P_neg (ast_expr_to_plan_check columns e)
  | Ast.E_bitnot e -> Plan.P_bitnot (ast_expr_to_plan_check columns e)
  | Ast.E_between (x, lo, hi) ->
    Plan.P_between
      ( ast_expr_to_plan_check columns x
      , ast_expr_to_plan_check columns lo
      , ast_expr_to_plan_check columns hi )
  | Ast.E_in (x, vals) ->
    Plan.P_in
      (ast_expr_to_plan_check columns x, List.map (ast_expr_to_plan_check columns) vals)
  | Ast.E_func (f, args) -> Plan.P_func (f, List.map (ast_expr_to_plan_check columns) args)
  | Ast.E_case { scrutinee; branches; else_ } ->
    let go = ast_expr_to_plan_check columns in
    Plan.P_case
      { scrutinee = Option.map go scrutinee
      ; branches = List.map (fun (c, r) -> go c, go r) branches
      ; else_ = Option.map go else_
      }
  | Ast.E_cast (e, ty) -> Plan.P_cast (ast_expr_to_plan_check columns e, ty)
  | Ast.E_collate (e, c) -> Plan.P_collate (ast_expr_to_plan_check columns e, c)
  | _ -> failwith "ast_expr_to_plan_check: unsupported expression in CHECK"
;;

let compile_check_expr
      (table_name : string)
      (col_idx : int)
      (columns : Row.column list)
      (check_sql : string)
  : Plan.expr
  =
  let key = table_name, col_idx, check_sql in
  match Hashtbl.find_opt check_expr_cache key with
  | Some e -> e
  | None ->
    let lexbuf = Lexing.from_string check_sql in
    let ast_expr =
      try Parser.expr_only Lexer.token lexbuf with
      | Parser.Error | Failure _ ->
        failwith
          (Printf.sprintf
             "CHECK constraint parse error for %s.col%d: %s"
             table_name
             col_idx
             check_sql)
    in
    let plan_expr = ast_expr_to_plan_check columns ast_expr in
    Hashtbl.add check_expr_cache key plan_expr;
    plan_expr
;;

(* Cache for compiled generated-column expressions.
   Key: (table_name, col_idx, expr_sql) — same three-part pattern as check_expr_cache.
   Schema changes invalidate entries via clear on DROP TABLE / DROP COLUMN. *)
let generated_expr_cache : (string * int * string, Plan.expr) Hashtbl.t = Hashtbl.create 8

let compile_generated_expr
      (table_name : string)
      (col_idx : int)
      (columns : Row.column list)
      (expr_sql : string)
  : Plan.expr
  =
  let key = table_name, col_idx, expr_sql in
  match Hashtbl.find_opt generated_expr_cache key with
  | Some e -> e
  | None ->
    let lexbuf = Lexing.from_string expr_sql in
    let ast_expr =
      try Parser.expr_only Lexer.token lexbuf with
      | Parser.Error | Failure _ ->
        failwith
          (Printf.sprintf
             "generated column expr parse error for %s.col%d: %s"
             table_name
             col_idx
             expr_sql)
    in
    let plan_expr = ast_expr_to_plan_check columns ast_expr in
    Hashtbl.add generated_expr_cache key plan_expr;
    plan_expr
;;

(** Compute STORED generated columns on the write path, in-place in [row].
    Iterates columns in schema order; earlier generated columns are available
    to later generated column expressions (in-order dependency). VIRTUAL
    generated columns are set to [V_null] in memory and on disk; they are
    recomputed on read via [compute_virtual_generated_cols]. *)
let compute_stored_generated_cols
      (clock : (unit -> float) option)
      (params : Row.value array)
      (meta : Cat.table_meta)
      (row : Row.t)
  : unit
  =
  (* #347: skip when no column is a STORED generated column — the common case.
     VIRTUAL columns stay at V_null (set by [build_insert_row]'s Array.make). *)
  if
    List.exists
      (fun (c : Row.column) ->
         match c.Row.generated_as with
         | Some (_, true) -> true
         | _ -> false)
      meta.Cat.columns
  then
    List.iteri
      (fun i (col : Row.column) ->
         match col.Row.generated_as with
         | None -> ()
         | Some (sql, true) ->
           let plan_e = compile_generated_expr meta.Cat.name i meta.Cat.columns sql in
           row.(i) <- eval_expr clock params row plan_e
         | Some (_, false) ->
           (* VIRTUAL: write NULL placeholder; recomputed on read. *)
           row.(i) <- Row.V_null)
      meta.Cat.columns
;;

(** Recompute VIRTUAL generated columns from the underlying row values.
    Invoked after [Row.decode] for table-row reads in [exec.ml]. *)
let compute_virtual_generated_cols
      (clock : (unit -> float) option)
      (params : Row.value array)
      (meta : Cat.table_meta)
      (row : Row.t)
  : unit
  =
  List.iteri
    (fun i (col : Row.column) ->
       match col.Row.generated_as with
       | Some (sql, false) ->
         let plan_e = compile_generated_expr meta.Cat.name i meta.Cat.columns sql in
         row.(i) <- eval_expr clock params row plan_e
       | _ -> ())
    meta.Cat.columns
;;

(** Like [compute_virtual_generated_cols] but driven by [(name, columns)]
    rather than a full [Cat.table_meta]. Used by call sites that only have
    a column list in scope (e.g., [execute_create_index]). *)
let compute_virtual_generated_cols_cols
      (clock : (unit -> float) option)
      (params : Row.value array)
      ~(table_name : string)
      (columns : Row.column list)
      (row : Row.t)
  : unit
  =
  List.iteri
    (fun i (col : Row.column) ->
       match col.Row.generated_as with
       | Some (sql, false) ->
         let plan_e = compile_generated_expr table_name i columns sql in
         row.(i) <- eval_expr clock params row plan_e
       | _ -> ())
    columns
;;

let has_virtual_cols (columns : Row.column list) : bool =
  List.exists
    (fun (c : Row.column) ->
       match c.Row.generated_as with
       | Some (_, false) -> true
       | _ -> false)
    columns
;;

(** [with_computed_virtuals]: return a copy of [row] with any VIRTUAL
    generated columns recomputed.  Used by the index-key extraction and
    CHECK-evaluation write paths so that VIRTUAL cells contribute the
    up-to-date value instead of [V_null].  Returns [row] unchanged when
    the table has no virtual columns (the common case). *)
let with_computed_virtuals
      (clock : (unit -> float) option)
      (params : Row.value array)
      (meta : Cat.table_meta)
      (row : Row.t)
  : Row.t
  =
  if not (has_virtual_cols meta.Cat.columns)
  then row
  else (
    let row' = Array.copy row in
    compute_virtual_generated_cols clock params meta row';
    row')
;;

(** Like [with_computed_virtuals] but takes a [(table_name, columns)] pair. *)
let with_computed_virtuals_cols
      (clock : (unit -> float) option)
      (params : Row.value array)
      ~(table_name : string)
      (columns : Row.column list)
      (row : Row.t)
  : Row.t
  =
  if not (has_virtual_cols columns)
  then row
  else (
    let row' = Array.copy row in
    compute_virtual_generated_cols_cols clock params ~table_name columns row';
    row')
;;

(** [decode_with_virtual]: like [Row.decode], but also recomputes any VIRTUAL
    generated columns in the schema. Skips the recompute when the table has
    no virtual cols (the common case). *)
let decode_with_virtual
      (clock : (unit -> float) option)
      (params : Row.value array)
      (meta : Cat.table_meta)
      (bytes : bytes)
  : Row.t
  =
  let row = Row.decode meta.Cat.columns bytes in
  if has_virtual_cols meta.Cat.columns
  then compute_virtual_generated_cols clock params meta row;
  row
;;

(** Variant that takes a [(table_name, columns)] pair instead of a full meta. *)
let decode_with_virtual_cols
      (clock : (unit -> float) option)
      (params : Row.value array)
      ~(table_name : string)
      (columns : Row.column list)
      (bytes : bytes)
  : Row.t
  =
  let row = Row.decode columns bytes in
  if has_virtual_cols columns
  then compute_virtual_generated_cols_cols clock params ~table_name columns row;
  row
;;

let index_where_cache : (string * string * string * string, Plan.expr) Hashtbl.t =
  Hashtbl.create 8
;;

let compile_index_where (idx : Cat.index_info) (columns : Row.column list) : Plan.expr =
  match idx.idx_where_sql with
  | None -> failwith "compile_index_where: called on non-partial index"
  | Some sql ->
    let schema_sig = String.concat "," (List.map (fun c -> c.Row.name) columns) in
    let key = idx.idx_name, idx.idx_table, sql, schema_sig in
    (match Hashtbl.find_opt index_where_cache key with
     | Some e -> e
     | None ->
       let lexbuf = Lexing.from_string sql in
       let ast_expr =
         try Parser.expr_only Lexer.token lexbuf with
         | Parser.Error | Failure _ ->
           failwith (Printf.sprintf "index WHERE parse error for %s: %s" idx.idx_name sql)
       in
       let plan_expr = ast_expr_to_plan_check columns ast_expr in
       Hashtbl.add index_where_cache key plan_expr;
       plan_expr)
;;

let row_matches_index_where
      (clock : (unit -> float) option)
      (params : Row.value array)
      (idx : Cat.index_info)
      (schema : Row.column list)
      (row : Row.t)
  : bool
  =
  match idx.idx_where_sql with
  | None -> true
  | Some _ ->
    let plan_e = compile_index_where idx schema in
    value_truthy (eval_expr clock params row plan_e)
;;

(* Cache for compiled index column expressions.
   Key: (idx_name, idx_table, expr_sql, schema_sig) — four parts to prevent collisions. *)
let index_expr_cache : (string * string * string * string, Plan.expr) Hashtbl.t =
  Hashtbl.create 8
;;

let compile_index_col_expr (idx : Cat.index_info) (i : int) (columns : Row.column list)
  : Plan.expr
  =
  let expr_sql = List.nth idx.idx_columns i in
  let schema_sig = String.concat "," (List.map (fun c -> c.Row.name) columns) in
  let key = idx.idx_name, idx.idx_table, expr_sql, schema_sig in
  match Hashtbl.find_opt index_expr_cache key with
  | Some e -> e
  | None ->
    let lexbuf = Lexing.from_string expr_sql in
    let ast_expr =
      try Parser.expr_only Lexer.token lexbuf with
      | Parser.Error | Failure _ ->
        failwith
          (Printf.sprintf "index expr parse error for %s[%d]: %s" idx.idx_name i expr_sql)
    in
    let plan_e = ast_expr_to_plan_check columns ast_expr in
    Hashtbl.add index_expr_cache key plan_e;
    plan_e
;;

(** Evaluate all index key values for [row] against [idx].
    For expression-indexed columns, evaluates the compiled expression.
    For plain columns, fetches from the row by column ordinal. *)
let get_index_key_values
      (clock : (unit -> float) option)
      (params : Row.value array)
      (idx : Cat.index_info)
      (schema : Row.column list)
      (row : Row.t)
  : Row.value list
  =
  List.mapi
    (fun i col_sql ->
       let is_expr =
         if i < List.length idx.idx_expr_flags
         then List.nth idx.idx_expr_flags i
         else false
       in
       if is_expr
       then (
         let plan_e = compile_index_col_expr idx i schema in
         eval_expr clock params row plan_e)
       else (
         let col_idx = find_col_idx_by_name schema col_sql in
         row.(col_idx)))
    idx.idx_columns
;;

let eval_check_constraints
      (clock : (unit -> float) option)
      (params : Row.value array)
      (table_meta : Cat.table_meta)
      (row : Row.t)
  : unit
  =
  (* #347: skip entirely when no column carries a CHECK — the common case. *)
  if
    List.exists
      (fun (c : Row.column) -> Option.is_some c.check_sql)
      table_meta.Cat.columns
  then (
    (* Phase 35 Task 2: populate VIRTUAL generated columns into a scratch row
       before evaluating CHECKs, so checks that reference a VIRTUAL column see
       the up-to-date value instead of [V_null]. *)
    let row_for_check = with_computed_virtuals clock params table_meta row in
    List.iteri
      (fun i (col : Row.column) ->
         match col.check_sql with
         | None -> ()
         | Some check_sql ->
           let check_plan =
             compile_check_expr table_meta.name i table_meta.columns check_sql
           in
           let result = eval_expr clock params row_for_check check_plan in
           (* SQLite: NULL result -> passes (not a violation) *)
           if result <> Row.V_null && not (value_truthy result)
           then
             failwith
               (Printf.sprintf "CHECK constraint failed: %s.%s" table_meta.name col.name))
      table_meta.columns)
;;

(* ------------------------------------------------------------------ *)
(* FTS inverted-index helpers                                           *)
(* ------------------------------------------------------------------ *)

(** Key format: term_bytes ++ "\x00" ++ rowid_be8
    Rowid stored with sign bit flipped so unsigned byte order = signed int64 order. *)
let fts_term_key term rowid =
  let rb = Bytes.create 8 in
  let v = Int64.logxor rowid Int64.min_int in
  for i = 0 to 7 do
    Bytes.set_uint8
      rb
      i
      (Int64.to_int (Int64.logand (Int64.shift_right_logical v ((7 - i) * 8)) 0xFFL))
  done;
  Bytes.concat Bytes.empty [ Bytes.of_string term; Bytes.of_string "\x00"; rb ]
;;

let fts_stats_key = Bytes.of_string "\x00\x00"

let fts_doclen_key rowid =
  let rb = Bytes.create 8 in
  let v = Int64.logxor rowid Int64.min_int in
  for i = 0 to 7 do
    Bytes.set_uint8
      rb
      i
      (Int64.to_int (Int64.logand (Int64.shift_right_logical v ((7 - i) * 8)) 0xFFL))
  done;
  Bytes.cat (Bytes.of_string "\x00\x01") rb
;;

(** Value: varint pairs (col, pos)* — all positions for one (term, rowid). *)
let encode_positions positions =
  let buf = Buffer.create (List.length positions * 2) in
  List.iter
    (fun (col, pos) ->
       Varint.encode_uint64 buf (Int64.of_int col);
       Varint.encode_uint64 buf (Int64.of_int pos))
    positions;
  Buffer.to_bytes buf
;;

(** FTS content row: n_cols_varint ++ (col_len_varint ++ col_bytes)* *)
let fts_encode_content (texts : string list) : bytes =
  let buf = Buffer.create 64 in
  Varint.encode_uint64 buf (Int64.of_int (List.length texts));
  List.iter
    (fun s ->
       let b = Bytes.of_string s in
       Varint.encode_uint64 buf (Int64.of_int (Bytes.length b));
       Buffer.add_bytes buf b)
    texts;
  Buffer.to_bytes buf
;;

let decode_positions value =
  let len = Bytes.length value in
  let pos = ref 0 in
  let result = ref [] in
  while !pos < len do
    let col, off1 = Varint.decode_uint64 value !pos in
    let p, off2 = Varint.decode_uint64 value off1 in
    result := (Int64.to_int col, Int64.to_int p) :: !result;
    pos := off2
  done;
  List.rev !result
;;

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
;;

(** Read global FTS stats from index tree: (total_docs, total_tokens). *)
let read_fts_stats tx index_tree =
  let+ bytes_opt = S.get tx index_tree fts_stats_key in
  match bytes_opt with
  | None -> 0, 0
  | Some b ->
    let docs, off = Varint.decode_uint64 b 0 in
    let toks, _ = Varint.decode_uint64 b off in
    Int64.to_int docs, Int64.to_int toks
;;

let write_fts_stats tx index_tree docs tokens =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf (Int64.of_int docs);
  Varint.encode_uint64 buf (Int64.of_int tokens);
  S.put tx index_tree fts_stats_key (Buffer.to_bytes buf)
;;

(** Write inverted index entries for a newly inserted document. *)
let fts_index_document tx ~(fts_meta : Cat.fts_table_meta) ~rowid ~col_texts =
  let tokens = Fts_tokenizer.tokenize col_texts in
  (* Group by term *)
  let by_term : (string, (int * int) list) Hashtbl.t = Hashtbl.create 8 in
  List.iter
    (fun (tok : Fts_tokenizer.token) ->
       let lst = Option.value ~default:[] (Hashtbl.find_opt by_term tok.term) in
       Hashtbl.replace by_term tok.term ((tok.col, tok.pos) :: lst))
    tokens;
  (* Write one entry per unique term *)
  let* () =
    Hashtbl.fold
      (fun term positions acc ->
         let* () = acc in
         let key = fts_term_key term rowid in
         let value = encode_positions (List.rev positions) in
         S.put tx fts_meta.Cat.fts_index_tree key value)
      by_term
      Lwt.return_unit
  in
  (* Write doc length *)
  let dlen = List.length tokens in
  let dlen_buf = Buffer.create 4 in
  Varint.encode_uint64 dlen_buf (Int64.of_int dlen);
  let* () =
    S.put tx fts_meta.Cat.fts_index_tree (fts_doclen_key rowid) (Buffer.to_bytes dlen_buf)
  in
  (* Update global stats *)
  let* docs, toks = read_fts_stats tx fts_meta.Cat.fts_index_tree in
  write_fts_stats tx fts_meta.Cat.fts_index_tree (docs + 1) (toks + dlen)
;;

(** Remove inverted index entries for a deleted document. *)
let fts_deindex_document tx ~(fts_meta : Cat.fts_table_meta) ~rowid ~col_texts =
  let tokens = Fts_tokenizer.tokenize col_texts in
  let terms =
    List.sort_uniq
      String.compare
      (List.map (fun (t : Fts_tokenizer.token) -> t.term) tokens)
  in
  let* () =
    Lwt_list.iter_s
      (fun term -> S.del tx fts_meta.Cat.fts_index_tree (fts_term_key term rowid))
      terms
  in
  let dlen = List.length tokens in
  let* () = S.del tx fts_meta.Cat.fts_index_tree (fts_doclen_key rowid) in
  let* docs, toks = read_fts_stats tx fts_meta.Cat.fts_index_tree in
  write_fts_stats tx fts_meta.Cat.fts_index_tree (max 0 (docs - 1)) (max 0 (toks - dlen))
;;

(* ------------------------------------------------------------------ *)
(* FTS query execution                                                  *)
(* ------------------------------------------------------------------ *)

(** Fetch the posting list for an exact term: [(rowid, positions)] *)
let fts_posting_list tx ~index_tree term =
  (* Scan keys from term\x00 onwards (sorted order).  Native O(log n) seek +
     lazy streaming of the matching prefix range, instead of draining the whole
     FTS index tree per query (#233; same fix class as #228/#229). *)
  let prefix = Bytes.cat (Bytes.of_string term) (Bytes.of_string "\x00") in
  let plen = Bytes.length prefix in
  let* cur = S.seek_ge tx index_tree prefix in
  let entries = ref [] in
  let rec gather () =
    match%lwt S.seek_next cur with
    | None -> Lwt.return_unit
    | Some (key, value) ->
      if Bytes.length key >= plen && Bytes.equal (Bytes.sub key 0 plen) prefix
      then (
        (* Extract rowid from last 8 bytes (sign-bit-flipped) *)
        let rowid_off = Bytes.length key - 8 in
        let v = ref 0L in
        for i = 0 to 7 do
          v
          := Int64.logor
               (Int64.shift_left !v 8)
               (Int64.of_int (Bytes.get_uint8 key (rowid_off + i)))
        done;
        let rowid = Int64.logxor !v Int64.min_int in
        let positions = decode_positions value in
        entries := (rowid, positions) :: !entries;
        gather ())
      else Lwt.return_unit (* keys are sorted: first non-match ends the range *)
  in
  let* () = gather () in
  S.seek_close cur;
  Lwt.return (List.rev !entries)
;;

(** Fetch posting lists for a prefix: merge all (rowid, positions) for terms matching prefix* *)
let fts_prefix_posting_list tx ~index_tree prefix_str =
  let prefix_bytes = Bytes.of_string prefix_str in
  let plen = Bytes.length prefix_bytes in
  (* Native O(log n) seek + lazy streaming of the matching prefix range,
     instead of draining the whole FTS index tree per query (#233). *)
  let* cur = S.seek_ge tx index_tree prefix_bytes in
  let by_rowid : (int64, (int * int) list) Hashtbl.t = Hashtbl.create 16 in
  let rec gather () =
    match%lwt S.seek_next cur with
    | None -> Lwt.return_unit
    | Some (key, value) ->
      (* Find the null byte separating term from rowid *)
      let null_pos = ref (-1) in
      let klen = Bytes.length key in
      let i = ref 0 in
      while !i < klen - 8 && !null_pos = -1 do
        if Bytes.get_uint8 key !i = 0 then null_pos := !i;
        incr i
      done;
      if !null_pos > 0
      then (
        let term_len = !null_pos in
        (* Check term has our prefix *)
        if term_len >= plen && Bytes.equal (Bytes.sub key 0 plen) prefix_bytes
        then (
          let rowid_off = !null_pos + 1 in
          if rowid_off + 8 <= klen
          then (
            let v = ref 0L in
            for j = 0 to 7 do
              v
              := Int64.logor
                   (Int64.shift_left !v 8)
                   (Int64.of_int (Bytes.get_uint8 key (rowid_off + j)))
            done;
            let rowid = Int64.logxor !v Int64.min_int in
            let positions = decode_positions value in
            let existing = Option.value ~default:[] (Hashtbl.find_opt by_rowid rowid) in
            Hashtbl.replace by_rowid rowid (existing @ positions);
            gather ())
          else Lwt.return_unit)
        else Lwt.return_unit (* term no longer has the prefix, stop — keys are sorted *))
      else Lwt.return_unit
  in
  let* () = gather () in
  S.seek_close cur;
  Lwt.return
    (Hashtbl.fold (fun rowid positions acc -> (rowid, positions) :: acc) by_rowid [])
;;

(* FTS phrase match: all [words] must appear consecutively in the same column.
   For each candidate doc, check there is a start position p and column c with
   word[i] at (col=c, pos=p+i) for all i. *)
(** Execute an FTS query, returning [(rowid, positions)] for matching documents. *)
let fts_phrase_match tx ~index_tree words =
  match words with
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
      let term_positions =
        Array.map
          (fun pl ->
             match List.assoc_opt rowid pl with
             | None -> []
             | Some pos -> pos)
          all_pls
      in
      List.exists
        (fun (c0, p0) ->
           let rec check i =
             if i >= n
             then true
             else List.mem (c0, p0 + i) term_positions.(i) && check (i + 1)
           in
           check 1)
        term_positions.(0)
    in
    let matched = List.filter (fun (r, _) -> phrase_matches r) candidates in
    Lwt.return matched
;;

let rec fts_execute_query tx ~index_tree query =
  match query with
  | Fts_query.FQ_term (Fts_query.FT_exact term) -> fts_posting_list tx ~index_tree term
  | Fts_query.FQ_term (Fts_query.FT_prefix prefix) ->
    fts_prefix_posting_list tx ~index_tree prefix
  | Fts_query.FQ_term (Fts_query.FT_phrase words) -> fts_phrase_match tx ~index_tree words
  | Fts_query.FQ_and qs ->
    let positive =
      List.filter
        (function
          | Fts_query.FQ_not _ -> false
          | _ -> true)
        qs
    in
    let negated =
      List.filter_map
        (function
          | Fts_query.FQ_not q -> Some q
          | _ -> None)
        qs
    in
    let* pos_results = Lwt_list.map_s (fts_execute_query tx ~index_tree) positive in
    let* neg_results = Lwt_list.map_s (fts_execute_query tx ~index_tree) negated in
    let neg_ids = List.concat_map (List.map fst) neg_results in
    let intersected =
      match pos_results with
      | [] -> []
      | first :: rest ->
        List.fold_left
          (fun acc pl ->
             let ids = List.map fst pl in
             List.filter (fun (r, _) -> List.mem r ids) acc)
          first
          rest
    in
    Lwt.return (List.filter (fun (r, _) -> not (List.mem r neg_ids)) intersected)
  | Fts_query.FQ_or qs ->
    let* results = Lwt_list.map_s (fts_execute_query tx ~index_tree) qs in
    let seen : (int64, unit) Hashtbl.t = Hashtbl.create 16 in
    let union =
      List.concat_map
        (fun pl ->
           List.filter
             (fun (r, _) ->
                if Hashtbl.mem seen r
                then false
                else (
                  Hashtbl.replace seen r ();
                  true))
             pl)
        results
    in
    Lwt.return union
  | Fts_query.FQ_not _ ->
    (* Standalone NOT is meaningless; returns empty set.
       NOT inside AND is handled in the FQ_and case above. *)
    Lwt.return []
;;

(** Helper: find the first index [i] such that [pred lst[i]] holds. *)
let list_find_index pred lst =
  let rec go i = function
    | [] -> None
    | x :: _ when pred x -> Some (i, x)
    | _ :: rest -> go (i + 1) rest
  in
  go 0 lst
;;

(* ------------------------------------------------------------------ *)
(* Transaction mode                                                     *)
(* ------------------------------------------------------------------ *)

type txn_mode =
  | Auto (** Each DML op starts and commits its own RW txn. *)
  | In_txn of S.rw S.txn (** Use this txn; skip auto begin/commit. *)
  | In_ro_txn of S.ro S.txn
  (** #274: read every scan through this one RO snapshot so a multi-statement
        read (e.g. [Db.dump] sweeping every table) observes a single
        point-in-time committed state.  Read-only: never reaches a write path;
        its lifecycle is owned by the caller, not ended by a scanner. *)

let acquire_txn store mode =
  match mode with
  | Auto ->
    let* tx = S.rw_begin store in
    Lwt.return (tx, true)
  | In_txn tx -> Lwt.return (tx, false)
  | In_ro_txn _ ->
    (* A write was attempted under a read-only ambient snapshot — a caller bug,
       not a runtime condition: [In_ro_txn] is only ever set on read paths. *)
    Lwt.fail (Failure "write attempted under a read-only transaction (In_ro_txn)")
;;

(* Commit [tx] if [owned], otherwise no-op.  When [cat] is provided and
   [owned], flush deferred rowid counters first (#347) so that counters
   dirtied by nested In_txn DML (e.g. trigger inserts) are persisted. *)
let release_txn ?cat tx owned =
  if owned
  then
    let* () =
      match cat with
      | None -> Lwt.return_unit
      | Some c -> Cat.flush_dirty_counters_tx c tx
    in
    S.commit tx
  else Lwt.return_unit
;;

(* #269: run a DDL body [f tx] under a transaction chosen by [mode], threading
   the writer txn into the catalog so DDL participates in any ambient explicit
   transaction instead of opening its own (which would self-deadlock against the
   single-writer lock the explicit txn already holds).

   Ownership decides who finalizes:
   - [Auto] (we opened the txn): commit on success / rollback on failure, and
     correspondingly clear or run the catalog's schema-cache undo log — the DDL
     mutated the in-memory cache before this commit, so a rollback must revert it.
   - [In_txn] (borrowed): leave commit/rollback AND schema-undo finalization to
     the db layer's COMMIT/ROLLBACK; an error here propagates with the ambient
     transaction left open.  A failed in-txn DDL statement may have already
     applied partial on-disk effects (e.g. [alter_drop_column] dropping a
     dependent index, then rewriting rows, before [Cat.drop_column] raises), and
     we have no statement-level savepoint to undo just this statement (#280/#283).
     So we poison the catalog (#286): the db layer forces a later COMMIT to roll
     the whole transaction back instead of persisting the half-applied DDL — the
     transaction is uncommittable, matching SQLite.  A ROLLBACK still unwinds
     cleanly via the whole-txn schema-undo log + store rollback. *)
let with_ddl_txn store (cat : Cat.t) mode f =
  let* tx, owned = acquire_txn store mode in
  Lwt.catch
    (fun () ->
       let* r = f tx in
       let* () =
         if owned
         then (
           let* () = S.commit tx in
           Cat.commit_schema_changes cat;
           Lwt.return_unit)
         else Lwt.return_unit
       in
       Lwt.return r)
    (fun exn ->
       let* () =
         if owned
         then (
           let* () = S.rollback tx in
           Cat.rollback_schema_changes cat;
           Lwt.return_unit)
         else (
           (* #286: borrowed txn — partial effects remain; mark uncommittable. *)
           Cat.mark_schema_txn_poisoned cat;
           Lwt.return_unit)
       in
       Lwt.fail exn)
;;

(* #262: a read handle for a base scanner.  Inside an explicit transaction
   ([In_txn tx]) reads must go THROUGH [tx] so they observe the transaction's
   own uncommitted writes (read-your-own-writes).  A scanner that instead opens
   a fresh RO snapshot ([S.ro_begin]) is, by snapshot-isolation design (#178),
   blind to the active writer's in-flight mutations — so a [SELECT] after
   [BEGIN; INSERT] would see the pre-[BEGIN] committed state.  In [Auto] mode
   (no explicit txn) the scanner owns a fresh RO snapshot and ends it when the
   read finishes; the borrowed txn is never ended here — Db owns its lifecycle. *)
type read_handle =
  | RH_borrowed of S.rw S.txn (* active explicit txn; lifecycle owned by Db *)
  | RH_borrowed_ro of S.ro S.txn (* #274: shared RO snapshot; lifecycle owned by caller *)
  | RH_owned of S.ro S.txn (* scanner-owned RO snapshot; ended on finish *)

let rh_begin store = function
  | In_txn tx -> Lwt.return (RH_borrowed tx)
  | In_ro_txn tx -> Lwt.return (RH_borrowed_ro tx)
  | Auto ->
    let* tx = S.ro_begin store in
    Lwt.return (RH_owned tx)
;;

let rh_finish = function
  | RH_borrowed _ | RH_borrowed_ro _ -> Lwt.return_unit
  | RH_owned tx -> S.ro_end tx
;;

let rh_get = function
  | RH_borrowed tx -> S.get tx
  | RH_borrowed_ro tx -> S.get tx
  | RH_owned tx -> S.get tx
;;

let rh_seek_ge = function
  | RH_borrowed tx -> S.seek_ge tx
  | RH_borrowed_ro tx -> S.seek_ge tx
  | RH_owned tx -> S.seek_ge tx
;;

let rh_cursor_open = function
  | RH_borrowed tx -> S.cursor_open tx
  | RH_borrowed_ro tx -> S.cursor_open tx
  | RH_owned tx -> S.cursor_open tx
;;

(* [with_read store mode f] runs [f] over a read handle, ending it afterwards
   only when the scanner owns it (Auto).  The txn-aware analogue of [S.with_ro];
   like it, the handle is released even if [f] raises (#164). *)
let with_read store mode f =
  let* rh = rh_begin store mode in
  Lwt.finalize (fun () -> f rh) (fun () -> rh_finish rh)
;;

(* ------------------------------------------------------------------ *)
(* execute: write operations only                                       *)
(* ------------------------------------------------------------------ *)

(** Replace every [P_excluded_col i] with [P_lit (value_to_literal excluded_row.(i))].
    Used to materialise UPSERT excluded-row refs before [eval_expr]. *)
let rec substitute_excluded (excluded_row : Row.t) (e : Plan.expr) : Plan.expr =
  match e with
  | Plan.P_excluded_col i -> Plan.P_lit (value_to_literal excluded_row.(i))
  | Plan.P_binop (op, a, b) ->
    Plan.P_binop
      (op, substitute_excluded excluded_row a, substitute_excluded excluded_row b)
  | Plan.P_not e -> Plan.P_not (substitute_excluded excluded_row e)
  | Plan.P_is_null e -> Plan.P_is_null (substitute_excluded excluded_row e)
  | Plan.P_is_not_null e -> Plan.P_is_not_null (substitute_excluded excluded_row e)
  | Plan.P_neg e -> Plan.P_neg (substitute_excluded excluded_row e)
  | Plan.P_bitnot e -> Plan.P_bitnot (substitute_excluded excluded_row e)
  | Plan.P_between (x, lo, hi) ->
    Plan.P_between
      ( substitute_excluded excluded_row x
      , substitute_excluded excluded_row lo
      , substitute_excluded excluded_row hi )
  | Plan.P_in (x, vals) ->
    Plan.P_in
      ( substitute_excluded excluded_row x
      , List.map (substitute_excluded excluded_row) vals )
  | Plan.P_func (f, args) ->
    Plan.P_func (f, List.map (substitute_excluded excluded_row) args)
  | Plan.P_case { scrutinee; branches; else_ } ->
    let go = substitute_excluded excluded_row in
    Plan.P_case
      { scrutinee = Option.map go scrutinee
      ; branches = List.map (fun (c, r) -> go c, go r) branches
      ; else_ = Option.map go else_
      }
  | Plan.P_cast (e, ty) -> Plan.P_cast (substitute_excluded excluded_row e, ty)
  | Plan.P_collate (e, c) -> Plan.P_collate (substitute_excluded excluded_row e, c)
  | other -> other
;;

(** True if any value in the list is NULL. *)
let any_null_val = List.exists (fun v -> v = Row.V_null)

(** Find column indices for a list of column names in [schema].
    Returns [None] for any name not found. *)
let find_col_idxs schema col_names =
  List.map
    (fun name ->
       let rec fi i = function
         | [] -> None
         | (c : Row.column) :: _ when String.equal c.name name -> Some i
         | _ :: rest -> fi (i + 1) rest
       in
       fi 0 schema)
    col_names
;;

(** Non-raising variant of find_col_idx_by_name: returns [None] if not found. *)
let find_col_idx_by_name_opt schema col_name =
  let rec fi i = function
    | [] -> None
    | (c : Row.column) :: _ when String.equal c.name col_name -> Some i
    | _ :: rest -> fi (i + 1) rest
  in
  fi 0 schema
;;

(** Encode a multi-column index-key prefix (no rowid).  Used by FK enforcement
    to seek to the first entry whose leading key columns match a target value
    list.  Returns the prefix bytes and their length. *)
let encode_index_key_prefix (ivs : Index_key.value list) : bytes * int =
  let parts = List.map Index_key.encode_value ivs in
  let total = List.fold_left (fun acc b -> acc + Bytes.length b) 0 parts in
  let buf = Bytes.create total in
  let off = ref 0 in
  List.iter
    (fun b ->
       let len = Bytes.length b in
       Bytes.blit b 0 buf !off len;
       off := !off + len)
    parts;
  buf, total
;;

(** Decode the rowid from the trailing 8 bytes of an index key. *)
let decode_index_key_rowid (ikey : bytes) : int64 =
  let n = Bytes.length ikey in
  let v = ref 0L in
  for i = 0 to 7 do
    v
    := Int64.logor
         (Int64.shift_left !v 8)
         (Int64.of_int (Bytes.get_uint8 ikey (n - 8 + i)))
  done;
  Int64.logxor !v Int64.min_int
;;

(* Full-table-scan fallbacks shared by the FK lookup/scan helpers below
   (used when no index covers the child columns). [full_scan_exists] stops at
   the first matching row; [full_scan_collect] gathers all (rowid,row) matches. *)
let full_scan_exists tx (meta : Cat.table_meta) (pred : Row.t -> bool) : bool Lwt.t =
  let* cur = S.cursor_open tx meta.Cat.tree_id in
  let _sr = S.cursor_first cur in
  let found = ref false in
  let rec scan () =
    if !found
    then ()
    else (
      match S.cursor_next cur with
      | None -> ()
      | Some (_k, vbytes) ->
        let row = decode_with_virtual None [||] meta vbytes in
        if pred row then found := true else scan ())
  in
  scan ();
  S.cursor_close cur;
  Lwt.return !found
;;

let full_scan_collect tx (meta : Cat.table_meta) (pred : Row.t -> bool)
  : (int64 * Row.t) list Lwt.t
  =
  let* cur = S.cursor_open tx meta.Cat.tree_id in
  let _sr = S.cursor_first cur in
  let buf = ref [] in
  let rec scan () =
    match S.cursor_next cur with
    | None -> ()
    | Some (kbytes, vbytes) ->
      let rowid = Rowid.decode kbytes in
      let row = decode_with_virtual None [||] meta vbytes in
      if pred row then buf := (rowid, row) :: !buf;
      scan ()
  in
  scan ();
  S.cursor_close cur;
  Lwt.return (List.rev !buf)
;;

(** Internal: scan [child_meta] within an already-open transaction (RO or RW)
    for any row whose [child_col_idxs] match [parent_vals].  Used by both the
    public store-opening variant below and the deferred FK recheck path
    (which must see writes performed in the active RW txn — opening a fresh
    [ro_begin] on the B+-tree backend would snapshot the pre-txn state and
    miss the about-to-commit rows). *)
let fk_child_has_ref_multi_in_tx
      (cat : Cat.t)
      tx
      (child_meta : Cat.table_meta)
      ~(child_col_idxs : int list)
      ~(parent_vals : Row.value list)
  =
  match
    Cat.find_index_covering_cols
      cat
      ~table_name:child_meta.Cat.name
      ~col_idxs:child_col_idxs
  with
  | Some idx when not (List.exists (fun v -> v = Row.V_null) parent_vals) ->
    let ivs = List.map row_value_to_index_value parent_vals in
    let prefix, plen = encode_index_key_prefix ivs in
    let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
    (* O(log n) native seek; stop at the first non-matching prefix (#228/#229). *)
    let* cur = S.seek_ge tx idx.Cat.idx_tree_id seek_key in
    let found = ref false in
    let exhausted = ref false in
    let rec walk () =
      if !found || !exhausted
      then Lwt.return_unit
      else (
        match%lwt S.seek_next cur with
        | None ->
          exhausted := true;
          Lwt.return_unit
        | Some (ikey, _ival) ->
          if Bytes.length ikey >= plen + 8 && Bytes.equal (Bytes.sub ikey 0 plen) prefix
          then (
            let rowid = decode_index_key_rowid ikey in
            let* row_opt = S.get tx child_meta.Cat.tree_id (Rowid.encode rowid) in
            match row_opt with
            | None -> walk ()
            | Some vbytes ->
              let row = decode_with_virtual None [||] child_meta vbytes in
              let ok =
                List.for_all2
                  (fun ci pv -> compare_values row.(ci) pv = 0)
                  child_col_idxs
                  parent_vals
              in
              if ok
              then (
                found := true;
                Lwt.return_unit)
              else walk ())
          else (
            exhausted := true;
            Lwt.return_unit))
    in
    let* () = walk () in
    S.seek_close cur;
    Lwt.return !found
  | _ ->
    full_scan_exists tx child_meta (fun row ->
      List.for_all2
        (fun ci pv -> compare_values row.(ci) pv = 0)
        child_col_idxs
        parent_vals)
;;

(** Scan [child_meta] for any row where all [child_col_idxs] match [parent_vals]
    simultaneously.  When an index covers [child_col_idxs] as a leading prefix,
    use it; otherwise fall back to a full table scan.
    Opens and closes its own RO snapshot. *)
let fk_child_has_ref_multi
      (cat : Cat.t)
      store
      (child_meta : Cat.table_meta)
      ~(child_col_idxs : int list)
      ~(parent_vals : Row.value list)
  =
  S.with_ro store
  @@ fun ro_tx ->
  fk_child_has_ref_multi_in_tx cat ro_tx child_meta ~child_col_idxs ~parent_vals
;;

(** Internal: scan [parent_meta] within an already-open transaction (RO or
    RW) for a row matching [parent_vals] on [parent_idxs].  Used by the
    deferred FK recheck path to observe uncommitted writes in the active
    write txn. *)
let fk_parent_has_row_in_tx
      tx
      (parent_meta : Cat.table_meta)
      ~(parent_idxs : int list)
      ~(parent_vals : Row.value list)
  : bool Lwt.t
  =
  let* cur = S.cursor_open tx parent_meta.Cat.tree_id in
  let _sr = S.cursor_first cur in
  let found = ref false in
  let rec scan () =
    if !found
    then ()
    else (
      match S.cursor_next cur with
      | None -> ()
      | Some (_k, vbytes) ->
        let row = decode_with_virtual None [||] parent_meta vbytes in
        let ok =
          List.for_all2
            (fun pi pv -> compare_values row.(pi) pv = 0)
            parent_idxs
            parent_vals
        in
        if ok then found := true else scan ())
  in
  scan ();
  S.cursor_close cur;
  Lwt.return !found
;;

(** Scan [parent_meta] for a row matching [parent_vals] on [parent_idxs].
    Returns true iff such a row exists.  Used at INSERT/UPDATE time
    (immediate FK enforcement); opens and closes its own RO snapshot. *)
let fk_parent_has_row
      store
      (parent_meta : Cat.table_meta)
      ~(parent_idxs : int list)
      ~(parent_vals : Row.value list)
  : bool Lwt.t
  =
  S.with_ro store
  @@ fun ro_tx ->
  let* found = fk_parent_has_row_in_tx ro_tx parent_meta ~parent_idxs ~parent_vals in
  Lwt.return found
;;

(** Helper for FK enforcement: routes a violation either to the pending
    queue (deferred) or raises immediately (immediate).  [recheck] is the
    closure invoked at commit time; it must return true iff the violation
    is still present. *)
let fk_violation
      ~deferred
      (cat : Cat.t)
      ~kind
      ~table
      ~rowid
      ~msg
      ~(recheck : Cat.pending_fk_recheck)
  =
  if deferred
  then (
    Cat.queue_pending_fk_check
      cat
      { Cat.pfk_kind = kind
      ; Cat.pfk_table = table
      ; Cat.pfk_rowid = rowid
      ; Cat.pfk_message = msg
      ; Cat.pfk_recheck = recheck
      };
    Lwt.return_unit)
  else Lwt.fail_with msg
;;

(* Immediate/deferred FK existence check for one [fk] of an INSERT row. *)
let enforce_insert_fk
      store
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      (row : Row.t)
      (fk : Cat.fk_constraint)
  : unit Lwt.t
  =
  let is_deferred = fk.fk_deferrable || Cat.get_defer_fks_pragma cat in
  let local_idxs_opt = find_col_idxs table_meta.Cat.columns fk.fk_local_cols in
  if List.exists Option.is_none local_idxs_opt
  then
    Lwt.fail_with
      (Printf.sprintf
         "FOREIGN KEY: some local columns not found in table '%s'"
         table_meta.Cat.name)
  else (
    let local_idxs = List.filter_map Fun.id local_idxs_opt in
    let local_vals = List.map (fun i -> row.(i)) local_idxs in
    (* NULL in any FK column => skip enforcement *)
    if any_null_val local_vals
    then Lwt.return_unit
    else (
      match Cat.find_table_cached cat ~name:fk.fk_parent_table with
      | None ->
        Lwt.fail_with
          (Printf.sprintf "FOREIGN KEY: parent table '%s' not found" fk.fk_parent_table)
      | Some parent_meta ->
        let parent_idxs_opt = find_col_idxs parent_meta.Cat.columns fk.fk_parent_cols in
        let parent_idxs = List.filter_map Fun.id parent_idxs_opt in
        if List.length parent_idxs <> List.length fk.fk_parent_cols
        then
          Lwt.fail_with
            (Printf.sprintf
               "FOREIGN KEY: column not found in parent table '%s'"
               fk.fk_parent_table)
        else (
          let child_col_idxs = local_idxs in
          let table_name = table_meta.Cat.name in
          let parent_meta_name = parent_meta.Cat.name in
          let msg =
            Printf.sprintf
              "FOREIGN KEY constraint failed: no row in '%s' where %s matches"
              fk.fk_parent_table
              (String.concat ", " fk.fk_parent_cols)
          in
          let* found =
            fk_parent_has_row store parent_meta ~parent_idxs ~parent_vals:local_vals
          in
          if found
          then Lwt.return_unit
          else (
            (* Deferred recheck threads the active write txn so it observes
              uncommitted writes (a fresh ro_begin would miss them). *)
            let recheck =
              { Cat.recheck =
                  (fun (type m) (recheck_tx : m S.txn) ->
                    match
                      ( Cat.find_table_cached cat ~name:table_name
                      , Cat.find_table_cached cat ~name:parent_meta_name )
                    with
                    | None, _ | _, None -> Lwt.return false
                    | Some child_now, Some parent_now ->
                      let* has_child =
                        fk_child_has_ref_multi_in_tx
                          cat
                          recheck_tx
                          child_now
                          ~child_col_idxs
                          ~parent_vals:local_vals
                      in
                      if not has_child
                      then Lwt.return false
                      else
                        let* has_parent =
                          fk_parent_has_row_in_tx
                            recheck_tx
                            parent_now
                            ~parent_idxs
                            ~parent_vals:local_vals
                        in
                        Lwt.return (not has_parent))
              }
            in
            fk_violation
              ~deferred:is_deferred
              cat
              ~kind:`Insert
              ~table:table_name
              ~rowid:0L
              ~msg
              ~recheck))))
;;

(* Evaluate all FK constraints for an INSERT of [row] before any writes. *)
let enforce_insert_fks store (cat : Cat.t) (table_meta : Cat.table_meta) (row : Row.t)
  : unit Lwt.t
  =
  let fks = table_meta.Cat.fk_constraints in
  if fks = [] || not (Cat.get_fk_enforcement cat)
  then Lwt.return_unit
  else Lwt_list.iter_s (enforce_insert_fk store cat table_meta row) fks
;;

(* Resolve the rowid for an INSERT: the INTEGER PRIMARY KEY for WITHOUT ROWID
   tables (must be present, non-NULL, integer), else a freshly allocated one. *)
let insert_rowid
      ?(defer_counter = false)
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      (row : Row.t)
  : int64 Lwt.t
  =
  if table_meta.Cat.without_rowid
  then (
    match
      List.find_index (fun (c : Row.column) -> c.primary_key) table_meta.Cat.columns
    with
    | None ->
      Lwt.fail_with
        (Printf.sprintf
           "WITHOUT ROWID table '%s' has no PRIMARY KEY column"
           table_meta.Cat.name)
    | Some pk_idx ->
      (match row.(pk_idx) with
       | Row.V_int n -> Lwt.return n
       | Row.V_null ->
         Lwt.fail_with
           (Printf.sprintf
              "WITHOUT ROWID table '%s': PRIMARY KEY column must not be NULL"
              table_meta.Cat.name)
       | _ ->
         Lwt.fail_with
           (Printf.sprintf
              "WITHOUT ROWID table '%s': PRIMARY KEY column must be INTEGER"
              table_meta.Cat.name)))
  else (
    match Cat.rowid_alias_col table_meta with
    | Some pk_idx ->
      (* #243 (T1): INTEGER PRIMARY KEY IS the rowid.  Use the supplied integer
         as the table key; on NULL/omitted, auto-allocate and write it back so
         [SELECT id] / RETURNING observe the assigned value.  An explicit value
         advances the autoincrement counter past it (SQLite parity: a later NULL
         insert gets max(existing)+1). *)
      (match row.(pk_idx) with
       | Row.V_int n ->
         let* () =
           if Int64.compare n Int64.max_int < 0
           then
             Cat.bump_next_rowid_in_txn
               ~defer_counter
               cat
               ~name:table_meta.name
               ~at_least:(Int64.add n 1L)
               tx
           else if table_meta.Cat.autoincrement
           then
             (* #312: pin the AUTOINCREMENT counter at max_int so the next
                auto-allocation detects exhaustion and raises SQLITE_FULL. *)
             Cat.bump_next_rowid_in_txn
               ~defer_counter
               cat
               ~name:table_meta.name
               ~at_least:Int64.max_int
               tx
           else Lwt.return_unit
         in
         Lwt.return n
       | Row.V_null ->
         let* id = Cat.next_rowid_in_txn ~defer_counter cat ~name:table_meta.name tx in
         row.(pk_idx) <- Row.V_int id;
         Lwt.return id
       | _ ->
         Lwt.fail_with
           (Printf.sprintf
              "datatype mismatch: INTEGER PRIMARY KEY column '%s' requires an integer"
              (List.nth table_meta.columns pk_idx).Row.name))
    | None -> Cat.next_rowid_in_txn ~defer_counter cat ~name:table_meta.name tx)
;;

(* SQLite-faithful UNIQUE violation message: "UNIQUE constraint failed: t.a"
   (each column listed as "<table>.<col>", comma-separated for composite keys).
   Shared by every secondary-index uniqueness check — INSERT-time
   ([check_insert_unique]), UPDATE-time ([check_index_unique_on_update]) and the
   CREATE UNIQUE INDEX build (#288) — so all three report identically, and match
   the rowid-alias/PRIMARY KEY paths that already use this exact wording. *)
let unique_constraint_failed_msg ~(table : string) ~(columns : string list) : string =
  Printf.sprintf
    "UNIQUE constraint failed: %s"
    (String.concat ", " (List.map (fun c -> table ^ "." ^ c) columns))
;;

(* UNIQUE pre-check for INSERT: fold over [idxs] returning (skip, rowids to
   delete for REPLACE, optional rowid to update for UPSERT). Raises on a plain
   UNIQUE violation. *)
let check_insert_unique
      tx
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~(row_for_idx : Row.t)
      ~(on_conflict : Ast.conflict_action option)
      ~(upsert_update : (string list * (int * Plan.expr) list) option)
      (idxs : Cat.index_info list)
  : (bool * int64 list * int64 option) Lwt.t
  =
  Lwt_list.fold_left_s
    (fun (skip, dels, upsert_rid) (idx : Cat.index_info) ->
       if skip || not idx.idx_unique
       then Lwt.return (skip, dels, upsert_rid)
       else if
         not (row_matches_index_where clock params idx table_meta.columns row_for_idx)
       then Lwt.return (skip, dels, upsert_rid)
       else (
         let key_vals =
           get_index_key_values clock params idx table_meta.columns row_for_idx
         in
         (* #290: SQLite treats every NULL as distinct in a UNIQUE index — a row
            whose key has ANY NULL column is exempt from the uniqueness probe (it
            is still inserted into the index tree, it just never conflicts). *)
         if any_null_val key_vals
         then Lwt.return (false, dels, upsert_rid)
         else (
           let iks = List.map row_value_to_index_value key_vals in
           let prefix, plen = encode_index_key_prefix iks in
           let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
           (* O(log n) native probe: only the first entry >= seek_key is needed
            to detect a duplicate prefix — never drain the whole index (#229). *)
           let* cur = S.seek_ge tx idx.idx_tree_id seek_key in
           let* first = S.seek_next cur in
           let conflict_rowid_opt =
             match first with
             | None -> None
             | Some (ikey, _) ->
               if Bytes.length ikey >= plen && Bytes.equal (Bytes.sub ikey 0 plen) prefix
               then (
                 let rid_bytes = Bytes.sub ikey plen (Bytes.length ikey - plen) in
                 Some (Rowid.decode rid_bytes))
               else None
           in
           S.seek_close cur;
           match conflict_rowid_opt with
           | None -> Lwt.return (false, dels, upsert_rid)
           | Some old_rowid ->
             (match on_conflict, upsert_update with
              | Some Ast.CA_ignore, _ ->
                Lwt.return (true, dels, upsert_rid) (* skip=true, stop checking *)
              | Some Ast.CA_replace, _ -> Lwt.return (false, old_rowid :: dels, upsert_rid)
              | _, Some (conflict_cols, _)
                when List.sort String.compare idx.idx_columns
                     = List.sort String.compare conflict_cols ->
                Lwt.return (false, dels, Some old_rowid)
              | _ ->
                Lwt.fail_with
                  (unique_constraint_failed_msg
                     ~table:table_meta.Cat.name
                     ~columns:idx.idx_columns)))))
    (false, [], None)
    idxs
;;

(* Write [row]'s index entries (honoring each index's WHERE predicate). *)
let insert_row_indexes
      tx
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~(row_for_idx : Row.t)
      ~rowid
      (idxs : Cat.index_info list)
  : unit Lwt.t
  =
  Lwt_list.iter_s
    (fun (idx : Cat.index_info) ->
       if not (row_matches_index_where clock params idx table_meta.columns row_for_idx)
       then Lwt.return_unit
       else (
         let iks =
           List.map
             row_value_to_index_value
             (get_index_key_values clock params idx table_meta.columns row_for_idx)
         in
         let ikey = Index_key.encode iks ~rowid in
         S.put tx idx.idx_tree_id ikey Bytes.empty))
    idxs
;;

(* [row_for_idx] must already have VIRTUAL generated columns applied (e.g.
   via [decode_with_virtual] or [with_computed_virtuals]).  Callers that
   obtain the row from [decode_with_virtual] can pass it directly — virtual
   cols are computed in-place there, so no extra [Array.copy] is needed. *)
let delete_row_indexes
      tx
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~(row_for_idx : Row.t)
      ~rowid
      indexes
  : unit Lwt.t
  =
  let schema = table_meta.Cat.columns in
  Lwt_list.iter_s
    (fun (idx : Cat.index_info) ->
       if not (row_matches_index_where clock params idx schema row_for_idx)
       then Lwt.return_unit
       else (
         let iks =
           List.map
             row_value_to_index_value
             (get_index_key_values clock params idx schema row_for_idx)
         in
         let old_ikey = Index_key.encode iks ~rowid in
         S.del tx idx.idx_tree_id old_ikey))
    indexes
;;

(* REPLACE conflict resolution: delete each [to_delete] row and its index
   entries (firing BEFORE DELETE); returns the displaced rows in original order. *)
let delete_replace_conflicts
      tx
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~(idxs : Cat.index_info list)
      ~on_replace_delete_before
      to_delete
  : Row.t list Lwt.t
  =
  let displaced_rows : Row.t list ref = ref [] in
  let* () =
    Lwt_list.iter_s
      (fun old_rowid ->
         let old_key = Rowid.encode old_rowid in
         let* old_bytes_opt = S.get tx table_meta.tree_id old_key in
         match old_bytes_opt with
         | None -> Lwt.return_unit
         | Some old_bytes ->
           let old_row = decode_with_virtual clock params table_meta old_bytes in
           displaced_rows := old_row :: !displaced_rows;
           let* () =
             match on_replace_delete_before with
             | None -> Lwt.return_unit
             | Some f -> f ~tx ~old_row
           in
           let* () = S.del tx table_meta.tree_id old_key in
           (* old_row from decode_with_virtual already has VIRTUAL cols applied *)
           delete_row_indexes
             tx
             table_meta
             ~clock
             ~params
             ~row_for_idx:old_row
             ~rowid:old_rowid
             idxs)
      (List.sort_uniq compare to_delete)
  in
  Lwt.return (List.rev !displaced_rows)
;;

(* #243/#249: write [new_row] for the row currently stored at [old_rowid],
   MOVING it to a new table-tree key when the INTEGER PRIMARY KEY alias column
   changed — with a uniqueness probe on the new key — and re-keying its
   secondary-index entries (the rowid is their key suffix) old->new.  For a
   non-alias table or an unchanged alias this is an in-place rewrite.  Returns
   the (possibly new) rowid.

   Every single-row UPDATE path (UPDATE, UPSERT DO UPDATE, ON UPDATE CASCADE)
   funnels through this so none can independently re-introduce the divergence
   between the stored key and the id column (#249). *)
let write_row_rekeyed
      tx
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~(old_row : Row.t)
      ~(new_row : Row.t)
      ~old_rowid
      ~indexes
  : int64 Lwt.t
  =
  let alias_col = Cat.rowid_alias_col table_meta in
  let* new_rowid =
    match alias_col with
    | None -> Lwt.return old_rowid
    | Some i ->
      (match new_row.(i) with
       | Row.V_int n -> Lwt.return n
       | _ ->
         Lwt.fail_with
           (Printf.sprintf
              "datatype mismatch: INTEGER PRIMARY KEY column '%s' requires an integer"
              (List.nth table_meta.Cat.columns i).Row.name))
  in
  let* () =
    if Int64.equal new_rowid old_rowid
    then Lwt.return_unit
    else
      let* existing = S.get tx table_meta.Cat.tree_id (Rowid.encode new_rowid) in
      match existing with
      | None -> Lwt.return_unit
      | Some _ ->
        let col_name =
          match alias_col with
          | Some i -> (List.nth table_meta.Cat.columns i).Row.name
          | None -> "rowid"
        in
        Lwt.fail_with
          (Printf.sprintf "UNIQUE constraint failed: %s.%s" table_meta.Cat.name col_name)
  in
  let schema = table_meta.Cat.columns in
  let old_row_for_idx = with_computed_virtuals clock params table_meta old_row in
  let new_row_for_idx = with_computed_virtuals clock params table_meta new_row in
  let* () =
    Lwt_list.iter_s
      (fun (idx : Cat.index_info) ->
         let old_matches =
           row_matches_index_where clock params idx schema old_row_for_idx
         in
         let new_matches =
           row_matches_index_where clock params idx schema new_row_for_idx
         in
         let old_iks =
           List.map
             row_value_to_index_value
             (get_index_key_values clock params idx schema old_row_for_idx)
         in
         let new_iks =
           List.map
             row_value_to_index_value
             (get_index_key_values clock params idx schema new_row_for_idx)
         in
         let old_ikey = Index_key.encode old_iks ~rowid:old_rowid in
         let new_ikey = Index_key.encode new_iks ~rowid:new_rowid in
         let* () =
           if old_matches then S.del tx idx.idx_tree_id old_ikey else Lwt.return_unit
         in
         if new_matches
         then S.put tx idx.idx_tree_id new_ikey Bytes.empty
         else Lwt.return_unit)
      indexes
  in
  let new_bytes = Row.encode schema new_row in
  let* () = S.del tx table_meta.Cat.tree_id (Rowid.encode old_rowid) in
  let* () = S.put tx table_meta.Cat.tree_id (Rowid.encode new_rowid) new_bytes in
  Lwt.return new_rowid
;;

(* UPSERT DO UPDATE: apply [assigns] to conflicting row [old_rowid], refresh
   indexes, fire BEFORE/AFTER UPDATE hooks, commit if we own the txn. *)
let execute_upsert_update
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~owned
      ~(row : Row.t)
      ~(assigns : (int * Plan.expr) list)
      ~old_rowid
      ~on_upsert_update_before
      ~on_upsert_update
  : bool Lwt.t
  =
  let old_key = Rowid.encode old_rowid in
  let* old_bytes_opt = S.get tx table_meta.tree_id old_key in
  match old_bytes_opt with
  | None ->
    let* () = if owned then S.rollback tx else Lwt.return_unit in
    Lwt.return false
  | Some old_bytes ->
    let old_row = decode_with_virtual clock params table_meta old_bytes in
    let new_row = Array.copy old_row in
    List.iter
      (fun (col_ord, expr) ->
         let e' = substitute_excluded row expr in
         new_row.(col_ord) <- eval_expr clock params old_row e')
      assigns;
    compute_stored_generated_cols clock params table_meta new_row;
    eval_check_constraints clock params table_meta new_row;
    let* () =
      match on_upsert_update_before with
      | None -> Lwt.return_unit
      | Some f -> f ~tx ~old_row ~new_row
    in
    (* #249: SET id = N in a DO UPDATE must move the row (and check uniqueness),
       same as a plain UPDATE — funnel through the shared re-key helper. *)
    let* (_ : int64) =
      write_row_rekeyed
        tx
        table_meta
        ~clock
        ~params
        ~old_row
        ~new_row
        ~old_rowid
        ~indexes:(Cat.indexes_for_table cat ~table:table_meta.name)
    in
    let* () =
      match on_upsert_update with
      | None -> Lwt.return_unit
      | Some f -> f ~tx ~old_row ~new_row
    in
    let* () = release_txn ~cat tx owned in
    Lwt.return true
;;

(* Plain INSERT path (no UPSERT match from secondary indexes): honor IGNORE
   (skip), delete REPLACE conflicts, write the new row + index entries using
   [S.put_x] (combined check+write) when [alias_explicit=true] to avoid a
   separate pre-read for the alias PK uniqueness check (#350). *)
let execute_insert_write
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~owned
      ~(row : Row.t)
      ~(row_for_idx : Row.t)
      ~rowid
      ~(idxs : Cat.index_info list)
      ~skip
      ~to_delete
      ~alias_explicit
      ~alias_col_name
      ~(on_conflict : Ast.conflict_action option)
      ~(upsert_update : (string list * (int * Plan.expr) list) option)
      ~on_replace_delete_before
      ~on_replace_delete
      ~on_upsert_update_before
      ~on_upsert_update
      ~after_hook
  : bool Lwt.t
  =
  if skip
  then
    (* IGNORE from secondary-index pre-check: rollback if owned.
       [on_conflict = CA_ignore] means [check_insert_unique] set [skip=true]
       and never populated [to_delete], so the else-branch (including
       [delete_replace_conflicts]) is unreachable — no hooks have fired and no
       B-tree deletes have been made, so [S.rollback] is safe. *)
    let* () = if owned then S.rollback tx else Lwt.return_unit in
    Lwt.return false
  else
    let* displaced_rows =
      delete_replace_conflicts
        tx
        table_meta
        ~clock
        ~params
        ~idxs
        ~on_replace_delete_before
        to_delete
    in
    let key = Rowid.encode rowid in
    let bytes = Row.encode table_meta.columns row in
    (* #350: use put_x for explicit alias PK rows to combine the uniqueness
       check with the write in a single B-tree descent (1 descent on the
       no-conflict path; 2 for CA_replace since put_x does not write on
       conflict and a follow-up S.put is needed).  For all other cases (no
       alias col, auto-allocated rowid) just S.put — no alias PK conflict
       is possible. *)
    let* conflict_opt =
      if alias_explicit
      then S.put_x tx table_meta.tree_id key bytes
      else
        let* () = S.put tx table_meta.tree_id key bytes in
        Lwt.return None
    in
    match conflict_opt with
    | None ->
      (* Common path: key was absent, write succeeded. *)
      let* () =
        insert_row_indexes tx table_meta ~clock ~params ~row_for_idx ~rowid idxs
      in
      let* () =
        match on_replace_delete with
        | None -> Lwt.return_unit
        | Some f -> Lwt_list.iter_s (fun old_row -> f ~tx ~old_row) displaced_rows
      in
      let* () =
        match after_hook with
        | None -> Lwt.return_unit
        | Some f -> f ~tx ~new_row:row
      in
      let* () = release_txn ~cat tx owned in
      Lwt.return true
    | Some _ ->
      (* Alias PK conflict detected by put_x (key present, NOT overwritten).
         put_x returns a sentinel [Some Bytes.empty] — callers that need the
         old row bytes (CA_replace) fetch them via S.get below. *)
      let col_name = Option.value alias_col_name ~default:"rowid" in
      (match on_conflict, upsert_update with
       | Some Ast.CA_ignore, _ ->
         let* () = if owned then S.rollback tx else Lwt.return_unit in
         Lwt.return false
       | Some Ast.CA_replace, _ ->
         let* old_bytes_opt = S.get tx table_meta.tree_id key in
         let old_row =
           match old_bytes_opt with
           | Some b -> decode_with_virtual clock params table_meta b
           | None -> failwith "put_x conflict but row gone before CA_replace fetch"
         in
         (* Fire BEFORE DELETE for the alias-PK displaced row.  Note: when
            there are simultaneous secondary-index REPLACE conflicts, those
            hooks fired first (inside delete_replace_conflicts above), so the
            alias-PK BEFORE DELETE fires last. *)
         let* () =
           match on_replace_delete_before with
           | None -> Lwt.return_unit
           | Some f -> f ~tx ~old_row
         in
         (* Delete the alias-PK row's index entries.
            old_rowid = rowid by alias-PK invariant: the conflict is on this
            same key, so the displaced row's rowid equals the inserted rowid.
            old_row from decode_with_virtual already has VIRTUAL cols applied. *)
         let* () =
           delete_row_indexes
             tx
             table_meta
             ~clock
             ~params
             ~row_for_idx:old_row
             ~rowid
             idxs
         in
         let* () = S.put tx table_meta.tree_id key bytes in
         let* () =
           insert_row_indexes tx table_meta ~clock ~params ~row_for_idx ~rowid idxs
         in
         (* Fire AFTER DELETE for all displaced rows.  Use [displaced_rows @
            [old_row]] so alias-PK row fires last — matching the BEFORE DELETE
            order (secondary conflicts first, alias-PK last). *)
         let all_displaced = displaced_rows @ [ old_row ] in
         let* () =
           match on_replace_delete with
           | None -> Lwt.return_unit
           | Some f -> Lwt_list.iter_s (fun r -> f ~tx ~old_row:r) all_displaced
         in
         let* () =
           match after_hook with
           | None -> Lwt.return_unit
           | Some f -> f ~tx ~new_row:row
         in
         let* () = release_txn ~cat tx owned in
         Lwt.return true
       | _, Some (conflict_cols, assigns) when conflict_cols = [ col_name ] ->
         (* Alias PK is always a single column, so single-element equality
            suffices — no sort needed.  Fire AFTER DELETE for any secondary
            REPLACE displaced rows before handing off to the upsert path. *)
         let* () =
           match on_replace_delete with
           | None -> Lwt.return_unit
           | Some f -> Lwt_list.iter_s (fun r -> f ~tx ~old_row:r) displaced_rows
         in
         execute_upsert_update
           tx
           cat
           table_meta
           ~clock
           ~params
           ~owned
           ~row
           ~assigns
           ~old_rowid:rowid
           ~on_upsert_update_before
           ~on_upsert_update
       | _ ->
         Lwt.fail_with
           (Printf.sprintf "UNIQUE constraint failed: %s.%s" table_meta.Cat.name col_name))
;;

(* Build the row to insert: use [prebuilt_row] if given, else evaluate each
   (ordinal, expr) into a fresh NULL-filled row of the table's width. *)
let build_insert_row
      ~clock
      ~params
      ~prebuilt_row
      ~ordinals
      ~values
      (table_meta : Cat.table_meta)
  : Row.t
  =
  match prebuilt_row with
  | Some r -> r
  | None ->
    let n = List.length table_meta.columns in
    let r = Array.make n Row.V_null in
    List.iter2
      (fun ord expr -> r.(ord) <- eval_expr clock params [||] expr)
      ordinals
      values;
    r
;;

(** Run [Op_insert] against the store: write the new row to the table
    tree and, if any indexes are defined on the table, also write the
    corresponding index entries (checking UNIQUE constraints first).
    Uses a SINGLE RW txn for both the row write and index writes. *)
let execute_insert
      ?(mode = Auto)
      ?(params = [||])
      ?(clock : (unit -> float) option = None)
      ?(on_conflict : Ast.conflict_action option = None)
      ?(upsert_update : (string list * (int * Plan.expr) list) option = None)
      ?(prebuilt_row : Row.t option = None)
      ?(before_hook : (tx:S.rw S.txn -> new_row:Row.t -> unit Lwt.t) option = None)
      ?(after_hook : (tx:S.rw S.txn -> new_row:Row.t -> unit Lwt.t) option = None)
      ?(on_replace_delete_before : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option =
        None)
      ?(on_replace_delete : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option = None)
      ?(on_upsert_update_before :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      ?(on_upsert_update :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      (store : S.t)
      (cat : Cat.t)
      ~(table_meta : Cat.table_meta)
      ~ordinals
      ~(values : Plan.expr list)
  : bool Lwt.t
  =
  let row = build_insert_row ~clock ~params ~prebuilt_row ~ordinals ~values table_meta in
  compute_stored_generated_cols clock params table_meta row;
  (* Evaluate CHECK and FK constraints before any writes. *)
  eval_check_constraints clock params table_meta row;
  let* () = enforce_insert_fks store cat table_meta row in
  (* When an explicit transaction is already held, we must NOT call
     Cat.next_rowid (which opens its own RW txn and deadlocks on the mutex):
     acquire/reuse the txn first, then allocate the rowid within it.  BEFORE
     INSERT fires inside the parent txn so its nested DML shares the tx and
     its writes roll back atomically with the parent on failure. *)
  let* tx, owned = acquire_txn store mode in
  Lwt.catch
    (fun () ->
       let* () =
         match before_hook with
         | None -> Lwt.return_unit
         | Some f -> f ~tx ~new_row:(Array.copy row)
       in
       (* #243 (T1): capture whether an EXPLICIT integer id was supplied for the
          rowid-alias column BEFORE [insert_rowid] writes back an auto value. *)
       let alias_idx = Cat.rowid_alias_col table_meta in
       let alias_explicit =
         match alias_idx with
         | Some i ->
           (match row.(i) with
            | Row.V_int _ -> true
            | _ -> false)
         | None -> false
       in
       (* #347: defer the per-row counter B-tree write when inside an explicit txn;
          [flush_dirty_counters_tx] writes it once at COMMIT instead. *)
       let* rowid = insert_rowid ~defer_counter:(not owned) tx cat table_meta row in
       let idxs = Cat.indexes_for_table cat ~table:table_meta.name in
       (* Phase 35 Task 2: compute VIRTUAL generated columns into a scratch row
         before extracting index keys so VIRTUAL cells contribute their value. *)
       let row_for_idx = with_computed_virtuals clock params table_meta row in
       let* skip, to_delete, upsert_rowid =
         check_insert_unique
           tx
           table_meta
           ~clock
           ~params
           ~row_for_idx
           ~on_conflict
           ~upsert_update
           idxs
       in
       (* #243 (T1): alias PK conflict detection is now folded into put_x
          inside execute_insert_write (#350) — no pre-read needed. *)
       let alias_col_name =
         match alias_idx with
         | Some i when alias_explicit -> Some (List.nth table_meta.columns i).Row.name
         | _ -> None
       in
       match upsert_update, upsert_rowid with
       | Some (_, assigns), Some old_rowid ->
         (* Secondary-index upsert conflict: update the conflicting row. *)
         execute_upsert_update
           tx
           cat
           table_meta
           ~clock
           ~params
           ~owned
           ~row
           ~assigns
           ~old_rowid
           ~on_upsert_update_before
           ~on_upsert_update
       | _ ->
         let* inserted =
           execute_insert_write
             tx
             cat
             table_meta
             ~clock
             ~params
             ~owned
             ~row
             ~row_for_idx
             ~rowid
             ~idxs
             ~skip
             ~to_delete
             ~alias_explicit
             ~alias_col_name
             ~on_conflict
             ~upsert_update
             ~on_replace_delete_before
             ~on_replace_delete
             ~on_upsert_update_before
             ~on_upsert_update
             ~after_hook
         in
         (* #243 (T1): record the rowid actually written so [last_insert_rowid()]
            is correct even when an explicit id differs from [next_rowid - 1].
            Skipped inserts (ON CONFLICT IGNORE ⇒ [inserted=false]) leave it. *)
         if inserted then Cat.set_last_inserted_rowid cat rowid;
         Lwt.return inserted)
    (fun exn ->
       (* On any exception: rollback if we own the txn, then re-raise. *)
       let* () = if owned then S.rollback tx else Lwt.return_unit in
       Lwt.fail exn)
;;

(** Run [Op_create_index]: register the index in the catalog, then scan
    the table tree and populate the index tree with one entry per row. *)
let execute_create_index
      ?(mode = Auto)
      (store : S.t)
      (cat : Cat.t)
      ~name
      ~table
      ~tree_id
      ~col_sqls
      ~col_expr_flags
      ~(where_expr : Plan.expr option)
      ~where_sql
      ~unique
      ~(columns : Row.column list)
  : unit Lwt.t
  =
  (* #269: register the index in the catalog AND populate the index tree through
     ONE writer txn.  Previously [Cat.create_index] opened (and committed) its
     own txn before this scan acquired another — which self-deadlocks when an
     explicit transaction already holds the writer lock.  [with_ddl_txn] threads
     a single txn through both so the whole CREATE INDEX participates in (and
     rolls back with) any ambient explicit transaction. *)
  with_ddl_txn store cat mode (fun tx ->
    let* res =
      Cat.create_index
        ~txn:tx
        cat
        ~name
        ~table
        ~columns:col_sqls
        ~unique
        ~expr_flags:col_expr_flags
        ~where_sql
        ~origin:`User
    in
    match res with
    | Error msg -> failwith msg
    | Ok info ->
      let* cur = S.cursor_open tx tree_id in
      let _sr = S.cursor_first cur in
      let rec walk () =
        match S.cursor_next cur with
        | None -> Lwt.return_unit
        | Some (kbytes, vbytes) ->
          let rowid = Rowid.decode kbytes in
          let row = decode_with_virtual_cols None [||] ~table_name:table columns vbytes in
          let skip =
            match where_expr with
            | None -> false
            | Some we -> not (value_truthy (eval_expr None [||] row we))
          in
          if skip
          then walk ()
          else (
            let key_vals = get_index_key_values None [||] info columns row in
            let iks = List.map row_value_to_index_value key_vals in
            let ikey = Index_key.encode iks ~rowid in
            (* #288: for a UNIQUE index, the build must detect pre-existing
               duplicate values.  The encoded key includes the rowid suffix, so
               two rows sharing the indexed value produce DISTINCT keys and never
               collide in the tree — uniqueness would otherwise only be enforced
               at INSERT time, letting pre-existing duplicates slip through.
               Probe the partially-built index for an entry already carrying this
               value prefix (rowid excluded) using the SAME mechanism as
               [check_insert_unique], so build-time and insert-time uniqueness
               agree (including multi-column, partial-WHERE and NULL handling).
               #290: a key with ANY NULL column is exempt from the conflict
               probe (NULLs are distinct in SQLite) — but is STILL inserted into
               the index tree below, exactly as [check_insert_unique] does.
               A raise here unwinds through [with_ddl_txn]: an owned txn rolls
               back (no partial entries), a borrowed one is poisoned (#286).
               The raise skips the outer [S.cursor_close cur] below, but
               [cursor_close] is a no-op (no OS handle) and the txn unwind
               reclaims all store state — so no leak. *)
            let* () =
              if (not unique) || any_null_val key_vals
              then Lwt.return_unit
              else (
                let prefix, plen = encode_index_key_prefix iks in
                let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
                let* probe = S.seek_ge tx info.idx_tree_id seek_key in
                let* first = S.seek_next probe in
                S.seek_close probe;
                match first with
                | Some (existing, _)
                  when Bytes.length existing >= plen
                       && Bytes.equal (Bytes.sub existing 0 plen) prefix ->
                  Lwt.fail_with
                    (unique_constraint_failed_msg ~table ~columns:info.idx_columns)
                | _ -> Lwt.return_unit)
            in
            let* () = S.put tx info.idx_tree_id ikey Bytes.empty in
            walk ())
      in
      let* () = walk () in
      S.cursor_close cur;
      Lwt.return_unit)
;;

(** Check whether inserting a new index entry for [new_row] with
    [rowid] into [idx] would violate a UNIQUE constraint.  Returns
    [true] if a different row already has the same indexed value. *)
let unique_violation_on_update
      (tx : S.rw S.txn)
      (idx : Cat.index_info)
      (_new_values : Row.value list)
        (* kept for call-site compat but unused for expr indexes *)
      ~(rowid : int64)
      ~(new_row : Row.t)
      ~(schema : Row.column list)
  : bool Lwt.t
  =
  (* For UNIQUE check we use the first value as the seek prefix.
     This is a conservative approach: we seek to the first key with the
     matching first-column value, then compare the entire encoded key.
     Phase 35 Task 2: populate VIRTUAL gen cols on the new row so the
     UNIQUE comparison keys reflect their computed value. *)
  let new_row_for_idx =
    with_computed_virtuals_cols None [||] ~table_name:idx.Cat.idx_table schema new_row
  in
  let key_vals = get_index_key_values None [||] idx schema new_row_for_idx in
  (* #290: a key with ANY NULL column is exempt — NULLs are distinct in a SQLite
     UNIQUE index, so it can never collide.  Short-circuit before probing. *)
  if any_null_val key_vals
  then Lwt.return false
  else (
    let ik_values = List.map row_value_to_index_value key_vals in
    (* Encode all values (no rowid) as the exact-match key; [encode_index_key_prefix]
     concatenates each value's encoding in order, same as the index key body. *)
    let full_key_no_rowid, full_klen = encode_index_key_prefix ik_values in
    let prefix =
      match ik_values with
      | [] -> Bytes.empty
      | ik :: _ -> Index_key.encode_value ik
    in
    let plen = Bytes.length prefix in
    let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
    (* O(log n) native seek; scan only the matching prefix range (#229). *)
    let* cur = S.seek_ge tx idx.idx_tree_id seek_key in
    (* Scan entries while the value prefix matches.  A different rowid
     with the same full value sequence is a UNIQUE violation. *)
    let rec scan () =
      match%lwt S.seek_next cur with
      | None -> Lwt.return false
      | Some (ikey, _) ->
        if Bytes.length ikey >= plen + 8 && Bytes.equal (Bytes.sub ikey 0 plen) prefix
        then
          (* Check that the full value prefix (all columns) also matches *)
          if
            Bytes.length ikey >= full_klen + 8
            && Bytes.equal (Bytes.sub ikey 0 full_klen) full_key_no_rowid
          then (
            let rowid_bytes = Bytes.sub ikey (Bytes.length ikey - 8) 8 in
            let other = Rowid.decode rowid_bytes in
            if Int64.equal other rowid then scan () else Lwt.return true)
          else scan ()
        else Lwt.return false
    in
    let* result = scan () in
    S.seek_close cur;
    Lwt.return result)
;;

(** Build the list of (child_table_meta, relevant_fk_constraints) pairs
    for tables that have FK constraints pointing to [parent_table_name]. *)
let build_child_refs cat ~parent_table_name =
  let* all_tables = Cat.list_tables cat in
  Lwt.return
    (List.filter_map
       (fun (child_meta : Cat.table_meta) ->
          let fks =
            List.filter
              (fun (fk : Cat.fk_constraint) ->
                 String.equal fk.fk_parent_table parent_table_name)
              child_meta.Cat.fk_constraints
          in
          if fks = [] then None else Some (child_meta, fks))
       all_tables)
;;

(** Scan [child_meta] using an existing RW transaction for rows where all
    [child_col_idxs] match [parent_vals] simultaneously.  When an index covers
    [child_col_idxs] as a leading prefix, the scan is driven by the index;
    otherwise it falls back to a full table scan.
    Returns (rowid, row) list. *)
let scan_child_rows_multi_tx
      (cat : Cat.t)
      tx
      (child_meta : Cat.table_meta)
      ~(child_col_idxs : int list)
      ~(parent_vals : Row.value list)
  =
  match
    Cat.find_index_covering_cols
      cat
      ~table_name:child_meta.Cat.name
      ~col_idxs:child_col_idxs
  with
  | Some idx when not (List.exists (fun v -> v = Row.V_null) parent_vals) ->
    let ivs = List.map row_value_to_index_value parent_vals in
    let prefix, plen = encode_index_key_prefix ivs in
    let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
    (* O(log n) native seek; scan only the matching prefix range (#228/#229). *)
    let* cur = S.seek_ge tx idx.Cat.idx_tree_id seek_key in
    let buf = ref [] in
    let exhausted = ref false in
    let rec walk () =
      if !exhausted
      then Lwt.return_unit
      else (
        match%lwt S.seek_next cur with
        | None ->
          exhausted := true;
          Lwt.return_unit
        | Some (ikey, _ival) ->
          if Bytes.length ikey >= plen + 8 && Bytes.equal (Bytes.sub ikey 0 plen) prefix
          then (
            let rowid = decode_index_key_rowid ikey in
            let* row_opt = S.get tx child_meta.Cat.tree_id (Rowid.encode rowid) in
            match row_opt with
            | None -> walk ()
            | Some vbytes ->
              let row = decode_with_virtual None [||] child_meta vbytes in
              let all_match =
                List.for_all2
                  (fun ci pv -> compare_values row.(ci) pv = 0)
                  child_col_idxs
                  parent_vals
              in
              if all_match then buf := (rowid, row) :: !buf;
              walk ())
          else (
            exhausted := true;
            Lwt.return_unit))
    in
    let* () = walk () in
    S.seek_close cur;
    Lwt.return (List.rev !buf)
  | _ ->
    full_scan_collect tx child_meta (fun row ->
      List.for_all2
        (fun ci pv -> compare_values row.(ci) pv = 0)
        child_col_idxs
        parent_vals)
;;

(** Delete a single row and its index entries within an existing RW transaction. *)
let delete_row_in_tx tx (cat : Cat.t) (meta : Cat.table_meta) ~rowid ~(row : Row.t) =
  let rowid_key = Rowid.encode rowid in
  let child_idxs = Cat.indexes_for_table cat ~table:meta.Cat.name in
  (* Phase 35 Task 2: ensure VIRTUAL gen cols are populated before key extraction. *)
  let row_for_idx = with_computed_virtuals None [||] meta row in
  let* () =
    Lwt_list.iter_s
      (fun (idx : Cat.index_info) ->
         if not (row_matches_index_where None [||] idx meta.Cat.columns row_for_idx)
         then Lwt.return_unit
         else (
           let iks =
             List.map
               row_value_to_index_value
               (get_index_key_values None [||] idx meta.Cat.columns row_for_idx)
           in
           let old_ikey = Index_key.encode iks ~rowid in
           S.del tx idx.idx_tree_id old_ikey))
      child_idxs
  in
  S.del tx meta.Cat.tree_id rowid_key
;;

(** Update one column to [new_val] in a row within an existing RW transaction.
    Also updates index entries for any index that covers [col_idx]. *)
let update_col_in_tx
      tx
      (cat : Cat.t)
      (meta : Cat.table_meta)
      ~rowid
      ~(row : Row.t)
      ~col_idx
      ~new_val
  =
  let new_row = Array.copy row in
  new_row.(col_idx) <- new_val;
  compute_stored_generated_cols None [||] meta new_row;
  (* #249: a cascade that lands on the child's own INTEGER PRIMARY KEY column
     must MOVE the child row (re-key + reindex), like any other alias-column
     UPDATE — funnel through the shared helper. *)
  let* (_ : int64) =
    write_row_rekeyed
      tx
      meta
      ~clock:None
      ~params:[||]
      ~old_row:row
      ~new_row
      ~old_rowid:rowid
      ~indexes:(Cat.indexes_for_table cat ~table:meta.Cat.name)
  in
  Lwt.return_unit
;;

(* Build the commit-time recheck for a deferred FK violation: it still stands
   iff a child row references [parent_vals] AND no parent row has them.
   Re-resolves column indices against the current schema. *)
let make_fk_recheck
      (cat : Cat.t)
      ~child_name
      ~parent_name
      ~child_cols
      ~parent_cols
      ~parent_vals
  : Cat.pending_fk_recheck
  =
  { Cat.recheck =
      (fun (type m) (recheck_tx : m S.txn) ->
        match
          ( Cat.find_table_cached cat ~name:child_name
          , Cat.find_table_cached cat ~name:parent_name )
        with
        | None, _ | _, None -> Lwt.return false
        | Some child_now, Some parent_now ->
          let cci =
            List.filter_map (find_col_idx_by_name_opt child_now.Cat.columns) child_cols
          in
          let pci =
            List.filter_map (find_col_idx_by_name_opt parent_now.Cat.columns) parent_cols
          in
          if
            List.length cci <> List.length child_cols
            || List.length pci <> List.length parent_cols
          then Lwt.return false
          else
            let* has_child =
              fk_child_has_ref_multi_in_tx
                cat
                recheck_tx
                child_now
                ~child_col_idxs:cci
                ~parent_vals
            in
            if not has_child
            then Lwt.return false
            else
              let* has_parent =
                fk_parent_has_row_in_tx
                  recheck_tx
                  parent_now
                  ~parent_idxs:pci
                  ~parent_vals
              in
              Lwt.return (not has_parent))
  }
;;

(* The DEFAULT value for [col] as a Row.value, resolving CURRENT_* sentinels
   via the clock.  Shared by the ON DELETE / ON UPDATE SET DEFAULT cascades. *)
let fk_default_value clock params (col : Row.column) : Row.value =
  match col.Row.default with
  | None -> Row.V_null
  | Some (Row.DV_int n) -> Row.V_int n
  | Some (Row.DV_text s) -> Row.V_text s
  | Some (Row.DV_real f) -> Row.V_real f
  | Some (Row.DV_blob b) -> Row.V_blob b
  | Some Row.DV_null -> Row.V_null
  | Some Row.DV_current_timestamp ->
    eval_expr
      clock
      params
      [||]
      (Plan.P_func (Ast.Fn_datetime, [ Plan.P_lit (Ast.L_text "now") ]))
  | Some Row.DV_current_date ->
    eval_expr
      clock
      params
      [||]
      (Plan.P_func (Ast.Fn_date, [ Plan.P_lit (Ast.L_text "now") ]))
  | Some Row.DV_current_time ->
    eval_expr
      clock
      params
      [||]
      (Plan.P_func (Ast.Fn_time, [ Plan.P_lit (Ast.L_text "now") ]))
;;

(** Recursively delete a row and cascade FK actions to child tables.
    Only runs cascade logic when FK enforcement is enabled in [cat]. *)
let rec cascade_delete_row_in_tx
          tx
          (cat : Cat.t)
          ?(visited : (string * int64, unit) Hashtbl.t = Hashtbl.create 16)
          (clock : (unit -> float) option)
          (params : Row.value array)
          (meta : Cat.table_meta)
          ~rowid
          ~(row : Row.t)
  =
  let visited_key = meta.Cat.name, rowid in
  if Hashtbl.mem visited visited_key
  then Lwt.return_unit
  else (
    Hashtbl.add visited visited_key ();
    let* child_refs =
      if Cat.get_fk_enforcement cat
      then build_child_refs cat ~parent_table_name:meta.Cat.name
      else Lwt.return []
    in
    let* () =
      Lwt_list.iter_s
        (fun (child_meta, fks) ->
           Lwt_list.iter_s
             (fun (fk : Cat.fk_constraint) ->
                cascade_delete_fk
                  tx
                  cat
                  visited
                  clock
                  params
                  meta
                  ~rowid
                  ~row
                  child_meta
                  fk)
             fks)
        child_refs
    in
    delete_row_in_tx tx cat meta ~rowid ~row)

(* Apply the ON DELETE action of one [fk] (child_meta references meta) while
   deleting [row] of [meta] at [rowid]. *)
and cascade_delete_fk
      tx
      cat
      visited
      clock
      params
      (meta : Cat.table_meta)
      ~rowid
      ~(row : Row.t)
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
  =
  let parent_col_idxs_opt =
    List.map (find_col_idx_by_name_opt meta.Cat.columns) fk.Cat.fk_parent_cols
  in
  if List.exists Option.is_none parent_col_idxs_opt
  then Lwt.return_unit
  else (
    let parent_col_idxs = List.filter_map Fun.id parent_col_idxs_opt in
    let parent_vals = List.map (fun i -> row.(i)) parent_col_idxs in
    if any_null_val parent_vals
    then Lwt.return_unit
    else (
      let child_col_idxs_opt =
        List.map (find_col_idx_by_name_opt child_meta.Cat.columns) fk.Cat.fk_local_cols
      in
      if List.exists Option.is_none child_col_idxs_opt
      then Lwt.return_unit
      else (
        let child_col_idxs = List.filter_map Fun.id child_col_idxs_opt in
        match fk.Cat.fk_on_delete with
        | Cat.FA_restrict | Cat.FA_no_action ->
          cascade_delete_restrict
            cat
            tx
            meta
            child_meta
            fk
            ~parent_vals
            ~child_col_idxs
            ~rowid
        | Cat.FA_cascade ->
          let* child_rows =
            scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals
          in
          Lwt_list.iter_s
            (fun (crid, crow) ->
               cascade_delete_row_in_tx
                 tx
                 cat
                 ~visited
                 clock
                 params
                 child_meta
                 ~rowid:crid
                 ~row:crow)
            child_rows
        | Cat.FA_set_null ->
          cascade_delete_set_null
            tx
            cat
            visited
            clock
            params
            child_meta
            ~child_col_idxs
            ~parent_vals
        | Cat.FA_set_default ->
          cascade_delete_set_default
            tx
            cat
            visited
            clock
            params
            child_meta
            ~child_col_idxs
            ~parent_vals)))

(* ON DELETE RESTRICT/NO ACTION: if any child row still references the parent,
   queue a deferred recheck or raise immediately. *)
and cascade_delete_restrict
      cat
      tx
      (meta : Cat.table_meta)
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
      ~(parent_vals : Row.value list)
      ~child_col_idxs
      ~rowid
  =
  let is_deferred = fk.Cat.fk_deferrable || Cat.get_defer_fks_pragma cat in
  let* child_rows =
    scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals
  in
  if child_rows <> []
  then (
    let msg =
      Printf.sprintf
        "FOREIGN KEY constraint failed: '%s.%s' is still referenced by '%s.%s'"
        meta.Cat.name
        (String.concat "," fk.Cat.fk_parent_cols)
        child_meta.Cat.name
        (String.concat "," fk.Cat.fk_local_cols)
    in
    let parent_meta_name = meta.Cat.name in
    let child_meta_name = child_meta.Cat.name in
    let parent_cols_copy = fk.Cat.fk_parent_cols in
    let child_cols_copy = fk.Cat.fk_local_cols in
    let recheck =
      { Cat.recheck =
          (fun (type m) (recheck_tx : m S.txn) ->
            match
              ( Cat.find_table_cached cat ~name:child_meta_name
              , Cat.find_table_cached cat ~name:parent_meta_name )
            with
            | None, _ | _, None -> Lwt.return false
            | Some child_now, Some parent_now ->
              let cci =
                List.filter_map
                  (find_col_idx_by_name_opt child_now.Cat.columns)
                  child_cols_copy
              in
              let pci =
                List.filter_map
                  (find_col_idx_by_name_opt parent_now.Cat.columns)
                  parent_cols_copy
              in
              if
                List.length cci <> List.length child_cols_copy
                || List.length pci <> List.length parent_cols_copy
              then Lwt.return false
              else
                let* has_child =
                  fk_child_has_ref_multi_in_tx
                    cat
                    recheck_tx
                    child_now
                    ~child_col_idxs:cci
                    ~parent_vals
                in
                if not has_child
                then Lwt.return false
                else
                  let* has_parent =
                    fk_parent_has_row_in_tx
                      recheck_tx
                      parent_now
                      ~parent_idxs:pci
                      ~parent_vals
                  in
                  Lwt.return (not has_parent))
      }
    in
    fk_violation
      ~deferred:is_deferred
      cat
      ~kind:`Delete
      ~table:parent_meta_name
      ~rowid
      ~msg
      ~recheck)
  else Lwt.return_unit

(* ON DELETE SET NULL: set each child FK column to NULL (rejecting NOT NULL),
   routing through cascade_update_col_in_tx so further ON UPDATE chains run. *)
and cascade_delete_set_null
      tx
      cat
      visited
      clock
      params
      (child_meta : Cat.table_meta)
      ~child_col_idxs
      ~(parent_vals : Row.value list)
  =
  let* child_rows =
    scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals
  in
  if child_rows = []
  then Lwt.return_unit
  else
    (* For single-col FKs (common case), apply to the one child col.
       For multi-col, apply SET NULL to each child col independently. *)
    Lwt_list.iter_s
      (fun child_col_idx ->
         let col = List.nth child_meta.Cat.columns child_col_idx in
         if col.Row.not_null
         then
           Lwt.fail_with
             (Printf.sprintf
                "FOREIGN KEY constraint failed: ON DELETE SET NULL on NOT NULL column \
                 '%s.%s'"
                child_meta.Cat.name
                col.Row.name)
         else
           Lwt_list.iter_s
             (fun (crid, crow) ->
                cascade_update_col_in_tx
                  tx
                  cat
                  ~visited
                  clock
                  params
                  child_meta
                  ~rowid:crid
                  ~row:crow
                  ~col_idx:child_col_idx
                  ~new_val:Row.V_null)
             child_rows)
      child_col_idxs

(* ON DELETE SET DEFAULT: like SET NULL but with each column's DEFAULT value. *)
and cascade_delete_set_default
      tx
      cat
      visited
      clock
      params
      (child_meta : Cat.table_meta)
      ~child_col_idxs
      ~(parent_vals : Row.value list)
  =
  let* child_rows =
    scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals
  in
  if child_rows = []
  then Lwt.return_unit
  else
    Lwt_list.iter_s
      (fun child_col_idx ->
         let col = List.nth child_meta.Cat.columns child_col_idx in
         let default_val = fk_default_value clock params col in
         if col.Row.not_null && default_val = Row.V_null
         then
           Lwt.fail_with
             (Printf.sprintf
                "FOREIGN KEY constraint failed: ON DELETE SET DEFAULT on NOT NULL column \
                 '%s.%s' with no default"
                child_meta.Cat.name
                col.Row.name)
         else
           Lwt_list.iter_s
             (fun (crid, crow) ->
                cascade_update_col_in_tx
                  tx
                  cat
                  ~visited
                  clock
                  params
                  child_meta
                  ~rowid:crid
                  ~row:crow
                  ~col_idx:child_col_idx
                  ~new_val:default_val)
             child_rows)
      child_col_idxs

(** Recursively update a column and cascade FK UPDATE actions to child tables
    that reference this column. *)
and cascade_update_col_in_tx
      tx
      (cat : Cat.t)
      ?(visited : (string * int64, unit) Hashtbl.t = Hashtbl.create 16)
      (clock : (unit -> float) option)
      (params : Row.value array)
      (meta : Cat.table_meta)
      ~rowid
      ~(row : Row.t)
      ~col_idx
      ~new_val
  =
  let visited_key = meta.Cat.name, rowid in
  if Hashtbl.mem visited visited_key
  then Lwt.return_unit
  else (
    Hashtbl.add visited visited_key ();
    let* () = update_col_in_tx tx cat meta ~rowid ~row ~col_idx ~new_val in
    if not (Cat.get_fk_enforcement cat)
    then Lwt.return_unit
    else (
      let parent_col_name = (List.nth meta.Cat.columns col_idx).Row.name in
      let* all_child_refs = build_child_refs cat ~parent_table_name:meta.Cat.name in
      let col_child_refs =
        List.filter_map
          (fun (child_meta, fks) ->
             let matching_fks =
               List.filter
                 (fun (fk : Cat.fk_constraint) ->
                    List.mem parent_col_name fk.Cat.fk_parent_cols)
                 fks
             in
             if matching_fks = [] then None else Some (child_meta, matching_fks))
          all_child_refs
      in
      Lwt_list.iter_s
        (fun (child_meta, fks) ->
           Lwt_list.iter_s
             (fun (fk : Cat.fk_constraint) ->
                cascade_update_fk
                  tx
                  cat
                  visited
                  clock
                  params
                  meta
                  ~row
                  ~new_val
                  ~parent_col_name
                  child_meta
                  fk)
             fks)
        col_child_refs))

(* Apply the ON UPDATE action of one [fk] when [parent_col_name] of [meta]
   changes to [new_val]. *)
and cascade_update_fk
      tx
      cat
      visited
      clock
      params
      (meta : Cat.table_meta)
      ~(row : Row.t)
      ~new_val
      ~parent_col_name
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
  =
  (* Find the position of parent_col_name in fk_parent_cols to get the
     corresponding fk_local_cols entry for single-update cascade. *)
  let fk_pos =
    let rec find_pos i = function
      | [] -> 0
      | col :: _ when String.equal col parent_col_name -> i
      | _ :: rest -> find_pos (i + 1) rest
    in
    find_pos 0 fk.Cat.fk_parent_cols
  in
  let child_col_name = List.nth fk.Cat.fk_local_cols fk_pos in
  let child_col_idx = find_col_idx_by_name child_meta.Cat.columns child_col_name in
  (* For multi-col FKs, we need all parent_vals to scan child rows *)
  let all_parent_col_idxs =
    List.map (fun c -> find_col_idx_by_name meta.Cat.columns c) fk.Cat.fk_parent_cols
  in
  let all_parent_vals_old = List.map (fun i -> row.(i)) all_parent_col_idxs in
  match fk.Cat.fk_on_update with
  | Cat.FA_restrict | Cat.FA_no_action -> Lwt.return_unit
  | Cat.FA_cascade ->
    let all_child_col_idxs =
      List.map
        (fun c -> find_col_idx_by_name child_meta.Cat.columns c)
        fk.Cat.fk_local_cols
    in
    let* child_rows =
      scan_child_rows_multi_tx
        cat
        tx
        child_meta
        ~child_col_idxs:all_child_col_idxs
        ~parent_vals:all_parent_vals_old
    in
    Lwt_list.iter_s
      (fun (crid, crow) ->
         cascade_update_col_in_tx
           tx
           cat
           ~visited
           clock
           params
           child_meta
           ~rowid:crid
           ~row:crow
           ~col_idx:child_col_idx
           ~new_val)
      child_rows
  | Cat.FA_set_null ->
    cascade_update_set_null
      tx
      cat
      visited
      clock
      params
      child_meta
      fk
      ~child_col_idx
      ~child_col_name
      ~parent_vals_old:all_parent_vals_old
  | Cat.FA_set_default ->
    cascade_update_set_default
      tx
      cat
      visited
      clock
      params
      child_meta
      fk
      ~child_col_idx
      ~child_col_name
      ~parent_vals_old:all_parent_vals_old

(* ON UPDATE SET NULL for one fk's child column. *)
and cascade_update_set_null
      tx
      cat
      visited
      clock
      params
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
      ~child_col_idx
      ~child_col_name
      ~parent_vals_old
  =
  let col = List.nth child_meta.Cat.columns child_col_idx in
  if col.Row.not_null
  then
    Lwt.fail_with
      (Printf.sprintf
         "FOREIGN KEY constraint failed: ON UPDATE SET NULL on NOT NULL column '%s.%s'"
         child_meta.Cat.name
         child_col_name)
  else (
    let all_child_col_idxs =
      List.map
        (fun c -> find_col_idx_by_name child_meta.Cat.columns c)
        fk.Cat.fk_local_cols
    in
    let* child_rows =
      scan_child_rows_multi_tx
        cat
        tx
        child_meta
        ~child_col_idxs:all_child_col_idxs
        ~parent_vals:parent_vals_old
    in
    Lwt_list.iter_s
      (fun (crid, crow) ->
         cascade_update_col_in_tx
           tx
           cat
           ~visited
           clock
           params
           child_meta
           ~rowid:crid
           ~row:crow
           ~col_idx:child_col_idx
           ~new_val:Row.V_null)
      child_rows)

(* ON UPDATE SET DEFAULT for one fk's child column. *)
and cascade_update_set_default
      tx
      cat
      visited
      clock
      params
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
      ~child_col_idx
      ~child_col_name
      ~parent_vals_old
  =
  let all_child_col_idxs =
    List.map (fun c -> find_col_idx_by_name child_meta.Cat.columns c) fk.Cat.fk_local_cols
  in
  let* child_rows =
    scan_child_rows_multi_tx
      cat
      tx
      child_meta
      ~child_col_idxs:all_child_col_idxs
      ~parent_vals:parent_vals_old
  in
  if child_rows = []
  then Lwt.return_unit
  else (
    let col = List.nth child_meta.Cat.columns child_col_idx in
    let default_val = fk_default_value clock params col in
    if col.Row.not_null && default_val = Row.V_null
    then
      Lwt.fail_with
        (Printf.sprintf
           "FOREIGN KEY constraint failed: ON UPDATE SET DEFAULT on NOT NULL column \
            '%s.%s' with no default"
           child_meta.Cat.name
           child_col_name)
    else
      Lwt_list.iter_s
        (fun (crid, crow) ->
           cascade_update_col_in_tx
             tx
             cat
             ~visited
             clock
             params
             child_meta
             ~rowid:crid
             ~row:crow
             ~col_idx:child_col_idx
             ~new_val:default_val)
        child_rows)
;;

(* Apply SET NULL to each [child_col_idxs] of every row in [child_rows],
   rejecting NOT NULL columns; routes through cascade_update_col_in_tx so the
   write propagates further ON UPDATE chains.  [op_label] is "ON UPDATE" /
   "ON DELETE" for the error message. *)
let cascade_apply_set_null
      tx
      (cat : Cat.t)
      ~clock
      ~params
      ~visited
      ~op_label
      (child_meta : Cat.table_meta)
      ~child_col_idxs
      child_rows
  : unit Lwt.t
  =
  if child_rows = []
  then Lwt.return_unit
  else
    Lwt_list.iter_s
      (fun child_col_idx ->
         let col = List.nth child_meta.Cat.columns child_col_idx in
         if col.Row.not_null
         then
           Lwt.fail_with
             (Printf.sprintf
                "FOREIGN KEY constraint failed: %s SET NULL on NOT NULL column '%s.%s'"
                op_label
                child_meta.Cat.name
                col.Row.name)
         else
           Lwt_list.iter_s
             (fun (crid, crow) ->
                cascade_update_col_in_tx
                  tx
                  cat
                  ~visited
                  clock
                  params
                  child_meta
                  ~rowid:crid
                  ~row:crow
                  ~col_idx:child_col_idx
                  ~new_val:Row.V_null)
             child_rows)
      child_col_idxs
;;

(* Apply SET DEFAULT to each [child_col_idxs] of every row in [child_rows]. *)
let cascade_apply_set_default
      tx
      (cat : Cat.t)
      ~clock
      ~params
      ~visited
      ~op_label
      (child_meta : Cat.table_meta)
      ~child_col_idxs
      child_rows
  : unit Lwt.t
  =
  if child_rows = []
  then Lwt.return_unit
  else
    Lwt_list.iter_s
      (fun child_col_idx ->
         let col = List.nth child_meta.Cat.columns child_col_idx in
         let default_val = fk_default_value clock params col in
         if col.Row.not_null && default_val = Row.V_null
         then
           Lwt.fail_with
             (Printf.sprintf
                "FOREIGN KEY constraint failed: %s SET DEFAULT on NOT NULL column \
                 '%s.%s' with no default"
                op_label
                child_meta.Cat.name
                col.Row.name)
         else
           Lwt_list.iter_s
             (fun (crid, crow) ->
                cascade_update_col_in_tx
                  tx
                  cat
                  ~visited
                  clock
                  params
                  child_meta
                  ~rowid:crid
                  ~row:crow
                  ~col_idx:child_col_idx
                  ~new_val:default_val)
             child_rows)
      child_col_idxs
;;

(* Drain all rows of [table_meta] satisfying [where] into a (rowid,row) list
   under an RO snapshot, so subsequent writes don't invalidate the cursor. *)
(* Drain matching rows from the given txn (RO or RW). *)
let drain_matching_rows_in_tx
      tx
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~(where : Plan.expr option)
  : (int64 * Row.t) list Lwt.t
  =
  let* cur = S.cursor_open tx table_meta.tree_id in
  let _sr = S.cursor_first cur in
  let buf = ref [] in
  let rec drain () =
    match S.cursor_next cur with
    | None -> ()
    | Some (kbytes, vbytes) ->
      let rowid = Rowid.decode kbytes in
      let row = decode_with_virtual clock params table_meta vbytes in
      let keep =
        match where with
        | None -> true
        | Some pred -> value_truthy (eval_expr clock params row pred)
      in
      if keep then buf := (rowid, row) :: !buf;
      drain ()
  in
  drain ();
  S.cursor_close cur;
  Lwt.return (List.rev !buf)
;;

(* Apply ORDER BY, then OFFSET, then LIMIT to a drained (rowid,row) list. *)
let apply_order_offset_limit ~clock ~params ~order ~offset ~limit matches =
  let sorted =
    if order = []
    then matches
    else
      List.sort
        (fun (_, ra) (_, rb) ->
           let rec cmp = function
             | [] -> 0
             | (e, dir, nulls) :: rest ->
               let va = eval_expr clock params ra e in
               let vb = eval_expr clock params rb e in
               let c = compare_with_nulls dir nulls va vb in
               if c <> 0 then c else cmp rest
           in
           cmp order)
        matches
  in
  let after_offset =
    match offset with
    | None | Some 0 -> sorted
    | Some n -> list_drop n sorted
  in
  match limit with
  | None -> after_offset
  | Some n -> list_take n after_offset
;;

(* Build the post-UPDATE row: copy [old_row] and apply each (i, expr) in
   [assignments], evaluating expr against the OLD row. *)
let apply_assignments ~clock ~params assignments (old_row : Row.t) : Row.t =
  let new_row = Array.copy old_row in
  List.iter
    (fun (i, expr) -> new_row.(i) <- eval_expr clock params old_row expr)
    assignments;
  new_row
;;

(* Pre-write RESTRICT/NO ACTION FK check for one UPDATE row's [fk]: if the
   parent key changes and is still referenced, raise (or queue deferred). *)
let precheck_update_fk
      store
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~rowid_outer
      ~(old_row : Row.t)
      ~(new_row : Row.t)
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
  : unit Lwt.t
  =
  match fk.fk_on_update with
  | Cat.FA_cascade | Cat.FA_set_null | Cat.FA_set_default -> Lwt.return_unit
  | Cat.FA_restrict | Cat.FA_no_action ->
    let is_deferred = fk.fk_deferrable || Cat.get_defer_fks_pragma cat in
    let parent_col_idxs =
      List.map (fun c -> find_col_idx_by_name table_meta.Cat.columns c) fk.fk_parent_cols
    in
    let old_vals = List.map (fun i -> old_row.(i)) parent_col_idxs in
    let new_vals = List.map (fun i -> new_row.(i)) parent_col_idxs in
    let unchanged =
      List.for_all2 (fun ov nv -> compare_values ov nv = 0) old_vals new_vals
    in
    if unchanged
    then Lwt.return_unit
    else if any_null_val old_vals
    then Lwt.return_unit
    else (
      let child_col_idxs =
        List.map (fun c -> find_col_idx_by_name child_meta.Cat.columns c) fk.fk_local_cols
      in
      let* has_ref =
        fk_child_has_ref_multi cat store child_meta ~child_col_idxs ~parent_vals:old_vals
      in
      if has_ref
      then (
        let msg =
          Printf.sprintf
            "FOREIGN KEY constraint failed: update to '%s.%s' is referenced by '%s.%s'"
            table_meta.Cat.name
            (String.concat "," fk.fk_parent_cols)
            child_meta.Cat.name
            (String.concat "," fk.fk_local_cols)
        in
        let recheck =
          make_fk_recheck
            cat
            ~child_name:child_meta.Cat.name
            ~parent_name:table_meta.Cat.name
            ~child_cols:fk.fk_local_cols
            ~parent_cols:fk.fk_parent_cols
            ~parent_vals:old_vals
        in
        fk_violation
          ~deferred:is_deferred
          cat
          ~kind:`Update
          ~table:table_meta.Cat.name
          ~rowid:rowid_outer
          ~msg
          ~recheck)
      else Lwt.return_unit)
;;

(* Pre-write FK RESTRICT check across all matched UPDATE rows. *)
let precheck_update_fk_restrict
      store
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~assignments
      ~child_refs
      matches
  : unit Lwt.t
  =
  if child_refs = []
  then Lwt.return_unit
  else
    Lwt_list.iter_s
      (fun (rowid_outer, old_row) ->
         let new_row = apply_assignments ~clock ~params assignments old_row in
         Lwt_list.iter_s
           (fun (child_meta, fks) ->
              Lwt_list.iter_s
                (precheck_update_fk
                   store
                   cat
                   table_meta
                   ~rowid_outer
                   ~old_row
                   ~new_row
                   child_meta)
                fks)
           child_refs)
      matches
;;

(* First UPDATE pass: validate UNIQUE for every target row against the full
   set of new values (an updated row may collide with another updated row). *)
(* Check one unique index for an UPDATE that turns [old_row] into [new_row]
   (with virtuals computed in [new_row_for_idx]); fails the Lwt thread on a
   duplicate. *)
let check_index_unique_on_update
      tx
      (idx : Cat.index_info)
      ~clock
      ~params
      ~schema
      ~old_row
      ~new_row
      ~new_row_for_idx
      ~rowid
  : unit Lwt.t
  =
  if not idx.idx_unique
  then Lwt.return_unit
  else if not (row_matches_index_where clock params idx schema new_row_for_idx)
  then Lwt.return_unit
  else (
    let old_vs = get_index_key_values clock params idx schema old_row in
    let new_vs = get_index_key_values clock params idx schema new_row_for_idx in
    (* #290: a new key with ANY NULL column is exempt — NULLs are distinct in a
       SQLite UNIQUE index, so the updated row can never conflict.  (The index
       entry itself is still maintained by the regular update path.) *)
    if any_null_val new_vs
    then Lwt.return_unit
    else (
      let values_equal a b =
        match a, b with
        | Row.V_null, Row.V_null -> true
        | Row.V_int x, Row.V_int y -> Int64.equal x y
        | Row.V_text x, Row.V_text y -> String.equal x y
        | Row.V_real x, Row.V_real y -> Float.equal x y
        | Row.V_blob x, Row.V_blob y -> Bytes.equal x y
        | _ -> false
      in
      let unchanged = List.for_all2 values_equal old_vs new_vs in
      if unchanged
      then Lwt.return_unit
      else
        let* dup = unique_violation_on_update tx idx new_vs ~rowid ~new_row ~schema in
        if dup
        then
          Lwt.fail_with
            (unique_constraint_failed_msg
               ~table:idx.Cat.idx_table
               ~columns:idx.idx_columns)
        else Lwt.return_unit))
;;

let validate_update_unique
      tx
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~indexes
      ~assignments
      matches
  : unit Lwt.t
  =
  let schema = table_meta.Cat.columns in
  Lwt_list.iter_s
    (fun (rowid, old_row) ->
       let new_row = apply_assignments ~clock ~params assignments old_row in
       compute_stored_generated_cols clock params table_meta new_row;
       eval_check_constraints clock params table_meta new_row;
       let new_row_for_idx = with_computed_virtuals clock params table_meta new_row in
       Lwt_list.iter_s
         (fun (idx : Cat.index_info) ->
            check_index_unique_on_update
              tx
              idx
              ~clock
              ~params
              ~schema
              ~old_row
              ~new_row
              ~new_row_for_idx
              ~rowid)
         indexes)
    matches
;;

(* Apply the ON UPDATE cascade of one [fk] for a parent row changing
   [old_row] -> [new_row], within the RW txn (RESTRICT handled in precheck). *)
let apply_update_cascade_fk
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~visited
      ~(old_row : Row.t)
      ~(new_row : Row.t)
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
  : unit Lwt.t
  =
  let parent_col_idxs =
    List.map (fun c -> find_col_idx_by_name table_meta.Cat.columns c) fk.fk_parent_cols
  in
  let old_vals = List.map (fun i -> old_row.(i)) parent_col_idxs in
  let new_vals = List.map (fun i -> new_row.(i)) parent_col_idxs in
  let unchanged =
    List.for_all2 (fun ov nv -> compare_values ov nv = 0) old_vals new_vals
  in
  if unchanged
  then Lwt.return_unit
  else if any_null_val old_vals
  then Lwt.return_unit
  else (
    let child_col_idxs =
      List.map (fun c -> find_col_idx_by_name child_meta.Cat.columns c) fk.fk_local_cols
    in
    match fk.fk_on_update with
    | Cat.FA_restrict | Cat.FA_no_action -> Lwt.return_unit
    | Cat.FA_cascade ->
      let* child_rows =
        scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals:old_vals
      in
      (* For cascade, use the first child col (single-col FK compat) *)
      let child_col_idx = List.hd child_col_idxs in
      let new_val_single = List.hd new_vals in
      Lwt_list.iter_s
        (fun (crid, crow) ->
           cascade_update_col_in_tx
             tx
             cat
             ~visited
             clock
             params
             child_meta
             ~rowid:crid
             ~row:crow
             ~col_idx:child_col_idx
             ~new_val:new_val_single)
        child_rows
    | Cat.FA_set_null ->
      let* child_rows =
        scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals:old_vals
      in
      cascade_apply_set_null
        tx
        cat
        ~clock
        ~params
        ~visited
        ~op_label:"ON UPDATE"
        child_meta
        ~child_col_idxs
        child_rows
    | Cat.FA_set_default ->
      let* child_rows =
        scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals:old_vals
      in
      cascade_apply_set_default
        tx
        cat
        ~clock
        ~params
        ~visited
        ~op_label:"ON UPDATE"
        child_meta
        ~child_col_idxs
        child_rows)
;;

(* Apply all ON UPDATE cascades for a parent row changing old_row -> new_row. *)
let apply_update_cascades
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~visited
      ~child_refs
      ~(old_row : Row.t)
      ~(new_row : Row.t)
  : unit Lwt.t
  =
  if child_refs = []
  then Lwt.return_unit
  else
    Lwt_list.iter_s
      (fun (child_meta, fks) ->
         Lwt_list.iter_s
           (apply_update_cascade_fk
              tx
              cat
              table_meta
              ~clock
              ~params
              ~visited
              ~old_row
              ~new_row
              child_meta)
           fks)
      child_refs
;;

(* Apply one matched UPDATE row: compute new row, run ON UPDATE cascades,
   reindex, and overwrite the row in the table tree. *)
let apply_update_row
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~child_refs
      ~indexes
      ~assignments
      (rowid, old_row)
  : Row.t Lwt.t
  =
  let new_row = apply_assignments ~clock ~params assignments old_row in
  compute_stored_generated_cols clock params table_meta new_row;
  (* Phase 35 task 3a: per-row visited set seeded with parent rowid, so
     cyclic ON UPDATE cascades terminate. *)
  let visited = Hashtbl.create 16 in
  Hashtbl.add visited (table_meta.Cat.name, rowid) ();
  let* () =
    apply_update_cascades
      tx
      cat
      table_meta
      ~clock
      ~params
      ~visited
      ~child_refs
      ~old_row
      ~new_row
  in
  (* #243/#249: re-keys the row when the UPDATE changed the INTEGER PRIMARY KEY
     alias column (uniqueness probe + del-old/put-new + reindex), else rewrites
     in place.  Shared with the UPSERT and ON UPDATE CASCADE paths. *)
  let* (_ : int64) =
    write_row_rekeyed
      tx
      table_meta
      ~clock
      ~params
      ~old_row
      ~new_row
      ~old_rowid:rowid
      ~indexes
  in
  (* Return the row as actually stored (generated columns included) so callers
     such as UPDATE ... RETURNING can project committed values, not a pre-lock
     snapshot (#226). *)
  Lwt.return new_row
;;

(* Fire an UPDATE row-hook (BEFORE/AFTER) for each matched row, recomputing
   the post-UPDATE row from the pre-write snapshot.  For non-deterministic
   expressions (random(), now()) the value the trigger sees may differ from
   the committed row. *)
let run_update_hook ~clock ~params ~assignments ~tx hook matches : unit Lwt.t =
  match hook with
  | None -> Lwt.return_unit
  | Some f ->
    Lwt_list.iter_s
      (fun (_rowid, old_row) ->
         let new_row = apply_assignments ~clock ~params assignments old_row in
         f ~tx ~old_row ~new_row)
      matches
;;

(** Run [Op_update]: drain matching rows into a list (snapshot read),
    then for each (rowid, old_row) compute the new row, update index
    entries, and overwrite the row in the table tree.  Returns the
    number of rows whose contents were modified. *)
let execute_update
      ?(mode = Auto)
      ?(params = [||])
      ?(clock : (unit -> float) option = None)
      ?(before_hook :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      ?(after_hook :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      ?(collect : (Row.t -> unit) option = None)
      (store : S.t)
      (cat : Cat.t)
      ~(table_meta : Cat.table_meta)
      ~(assignments : (int * Plan.expr) list)
      ~(where : Plan.expr option)
      ~(order : (Plan.expr * [ `Asc | `Desc ] * [ `Nulls_first | `Nulls_last ]) list)
      ~(limit : int option)
      ~(offset : int option)
      ~(indexes : Cat.index_info list)
  : int Lwt.t
  =
  (* Acquire the write lock BEFORE draining so the read-modify-write is atomic.
     Draining via a separate RO snapshot first (the old [Auto] path) let a
     concurrent commit land between the row read and the lock acquisition, so
     the new row was computed from a stale value and clobbered that commit —
     a lost update on backends whose commit yields, e.g. WAL/file (#223).
     [In_txn] already drained under the caller's lock; this makes [Auto] match. *)
  let* tx, owned = acquire_txn store mode in
  Lwt.catch
    (fun () ->
       let* matches = drain_matching_rows_in_tx tx table_meta ~clock ~params ~where in
       let matches =
         apply_order_offset_limit ~clock ~params ~order ~offset ~limit matches
       in
       let n = List.length matches in
       if n = 0
       then
         let* () = if owned then S.rollback tx else Lwt.return_unit in
         Lwt.return 0
       else
         let* child_refs =
           if Cat.get_fk_enforcement cat
           then build_child_refs cat ~parent_table_name:table_meta.Cat.name
           else Lwt.return []
         in
         (* FK pre-check: fail for RESTRICT/NO_ACTION when a referenced key
            changes.  CASCADE/SET_NULL/SET_DEFAULT are applied in the txn below. *)
         let* () =
           precheck_update_fk_restrict
             store
             cat
             table_meta
             ~clock
             ~params
             ~assignments
             ~child_refs
             matches
         in
         (* Phase 38: BEFORE/AFTER UPDATE fire inside the parent txn so nested
            DML shares it (atomic rollback on failure; no nested-trigger
            deadlock). *)
         let* () = run_update_hook ~clock ~params ~assignments ~tx before_hook matches in
         let* () =
           validate_update_unique
             tx
             table_meta
             ~clock
             ~params
             ~indexes
             ~assignments
             matches
         in
         let* () =
           Lwt_list.iter_s
             (fun m ->
                let* new_row =
                  apply_update_row
                    tx
                    cat
                    table_meta
                    ~clock
                    ~params
                    ~child_refs
                    ~indexes
                    ~assignments
                    m
                in
                (* RETURNING / row collection sees the committed row (#226). *)
                (match collect with
                 | Some f -> f new_row
                 | None -> ());
                Lwt.return_unit)
             matches
         in
         let* () = run_update_hook ~clock ~params ~assignments ~tx after_hook matches in
         let* () = release_txn ~cat tx owned in
         Lwt.return n)
    (fun exn ->
       let* () = if owned then S.rollback tx else Lwt.return_unit in
       Lwt.fail exn)
;;

(* Pre-write RESTRICT/NO ACTION FK check for one DELETE row's [fk]: if a
   child still references the row being deleted, raise (or queue deferred). *)
let precheck_delete_fk
      store
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~rowid_outer
      ~(row : Row.t)
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
  : unit Lwt.t
  =
  match fk.fk_on_delete with
  | Cat.FA_cascade | Cat.FA_set_null | Cat.FA_set_default -> Lwt.return_unit
  | Cat.FA_restrict | Cat.FA_no_action ->
    let is_deferred = fk.fk_deferrable || Cat.get_defer_fks_pragma cat in
    let parent_col_idxs =
      List.map (fun c -> find_col_idx_by_name table_meta.Cat.columns c) fk.fk_parent_cols
    in
    let parent_vals = List.map (fun i -> row.(i)) parent_col_idxs in
    if any_null_val parent_vals
    then Lwt.return_unit
    else (
      let child_col_idxs =
        List.map (fun c -> find_col_idx_by_name child_meta.Cat.columns c) fk.fk_local_cols
      in
      let* has_ref =
        fk_child_has_ref_multi cat store child_meta ~child_col_idxs ~parent_vals
      in
      if has_ref
      then (
        let msg =
          Printf.sprintf
            "FOREIGN KEY constraint failed: '%s.%s' is still referenced by '%s.%s'"
            table_meta.Cat.name
            (String.concat "," fk.fk_parent_cols)
            child_meta.Cat.name
            (String.concat "," fk.fk_local_cols)
        in
        let recheck =
          make_fk_recheck
            cat
            ~child_name:child_meta.Cat.name
            ~parent_name:table_meta.Cat.name
            ~child_cols:fk.fk_local_cols
            ~parent_cols:fk.fk_parent_cols
            ~parent_vals
        in
        fk_violation
          ~deferred:is_deferred
          cat
          ~kind:`Delete
          ~table:table_meta.Cat.name
          ~rowid:rowid_outer
          ~msg
          ~recheck)
      else Lwt.return_unit)
;;

(* Pre-write FK RESTRICT check across all matched DELETE rows. *)
let precheck_delete_fk_restrict
      store
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~child_refs
      matches
  : unit Lwt.t
  =
  if child_refs = []
  then Lwt.return_unit
  else
    Lwt_list.iter_s
      (fun (rowid_outer, row) ->
         Lwt_list.iter_s
           (fun (child_meta, fks) ->
              Lwt_list.iter_s
                (precheck_delete_fk store cat table_meta ~rowid_outer ~row child_meta)
                fks)
           child_refs)
      matches
;;

(* Apply the ON DELETE cascade of one [fk] for parent [row] being deleted,
   within the RW txn (RESTRICT handled in precheck). *)
let apply_delete_cascade_fk
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~visited
      ~(row : Row.t)
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
  : unit Lwt.t
  =
  let parent_col_idxs =
    List.map (fun c -> find_col_idx_by_name table_meta.Cat.columns c) fk.fk_parent_cols
  in
  let parent_vals = List.map (fun i -> row.(i)) parent_col_idxs in
  if any_null_val parent_vals
  then Lwt.return_unit
  else (
    let child_col_idxs =
      List.map (fun c -> find_col_idx_by_name child_meta.Cat.columns c) fk.fk_local_cols
    in
    match fk.fk_on_delete with
    | Cat.FA_restrict | Cat.FA_no_action -> Lwt.return_unit
    | Cat.FA_cascade ->
      let* child_rows =
        scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals
      in
      Lwt_list.iter_s
        (fun (crid, crow) ->
           cascade_delete_row_in_tx
             tx
             cat
             ~visited
             clock
             params
             child_meta
             ~rowid:crid
             ~row:crow)
        child_rows
    | Cat.FA_set_null ->
      let* child_rows =
        scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals
      in
      cascade_apply_set_null
        tx
        cat
        ~clock
        ~params
        ~visited
        ~op_label:"ON DELETE"
        child_meta
        ~child_col_idxs
        child_rows
    | Cat.FA_set_default ->
      let* child_rows =
        scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals
      in
      cascade_apply_set_default
        tx
        cat
        ~clock
        ~params
        ~visited
        ~op_label:"ON DELETE"
        child_meta
        ~child_col_idxs
        child_rows)
;;

(* Apply all ON DELETE cascades for parent [row] being deleted. *)
let apply_delete_cascades
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~visited
      ~child_refs
      ~(row : Row.t)
  : unit Lwt.t
  =
  if child_refs = []
  then Lwt.return_unit
  else
    Lwt_list.iter_s
      (fun (child_meta, fks) ->
         Lwt_list.iter_s
           (apply_delete_cascade_fk
              tx
              cat
              table_meta
              ~clock
              ~params
              ~visited
              ~row
              child_meta)
           fks)
      child_refs
;;

(* Delete one matched row: run ON DELETE cascades, remove index entries, then
   remove the row.  Visited set seeded with this row so cyclic cascades stop. *)
let apply_delete_row
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~child_refs
      ~indexes
      (rowid, row)
  : unit Lwt.t
  =
  let visited = Hashtbl.create 16 in
  Hashtbl.add visited (table_meta.Cat.name, rowid) ();
  let* () =
    apply_delete_cascades tx cat table_meta ~clock ~params ~visited ~child_refs ~row
  in
  let rowid_key = Rowid.encode rowid in
  (* row from drain_matching_rows_in_tx → decode_with_virtual: VIRTUAL cols
     already applied in-place, so pass directly as row_for_idx. *)
  let* () =
    delete_row_indexes tx table_meta ~clock ~params ~row_for_idx:row ~rowid indexes
  in
  S.del tx table_meta.tree_id rowid_key
;;

(** Run [Op_delete]: drain matching rows into a list (snapshot read),
    then for each matching (rowid, row) remove index entries and the
    row itself from the table tree.  Returns the number of rows deleted. *)
let execute_delete
      ?(mode = Auto)
      ?(params = [||])
      ?(clock : (unit -> float) option = None)
      ?(before_hook : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option = None)
      ?(after_hook : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option = None)
      ?(collect : (Row.t -> unit) option = None)
      (store : S.t)
      (cat : Cat.t)
      ~(table_meta : Cat.table_meta)
      ~(where : Plan.expr option)
      ~(order : (Plan.expr * [ `Asc | `Desc ] * [ `Nulls_first | `Nulls_last ]) list)
      ~(limit : int option)
      ~(offset : int option)
      ~(indexes : Cat.index_info list)
  : int Lwt.t
  =
  (* Acquire the write lock BEFORE draining so the match set can't go stale
     between the read and the delete (same TOCTOU as the UPDATE path, #223):
     otherwise a row matched on a separate RO snapshot could be concurrently
     modified to no longer match — yet still be deleted.  [In_txn] already
     drained under the caller's lock; this makes [Auto] match. *)
  let* tx, owned = acquire_txn store mode in
  Lwt.catch
    (fun () ->
       let* matches = drain_matching_rows_in_tx tx table_meta ~clock ~params ~where in
       let matches =
         apply_order_offset_limit ~clock ~params ~order ~offset ~limit matches
       in
       let n = List.length matches in
       if n = 0
       then
         let* () = if owned then S.rollback tx else Lwt.return_unit in
         Lwt.return 0
       else
         (* FK pre-check: fail for RESTRICT/NO_ACTION; CASCADE/SET_NULL/SET_DEFAULT
            are applied inside the RW transaction below. *)
         let* child_refs =
           if Cat.get_fk_enforcement cat
           then build_child_refs cat ~parent_table_name:table_meta.Cat.name
           else Lwt.return []
         in
         let* () = precheck_delete_fk_restrict store cat table_meta ~child_refs matches in
         (* Phase 38: BEFORE/AFTER DELETE fire inside the parent txn so nested
            DML shares it and trigger failures roll back the DELETE. *)
         let* () =
           match before_hook with
           | None -> Lwt.return_unit
           | Some f -> Lwt_list.iter_s (fun (_rowid, old_row) -> f ~tx ~old_row) matches
         in
         let* () =
           Lwt_list.iter_s
             (fun ((_rowid, old_row) as m) ->
                let* () =
                  apply_delete_row tx cat table_meta ~clock ~params ~child_refs ~indexes m
                in
                (* RETURNING / row collection sees the row as deleted under the
                   write lock, not a pre-lock snapshot (#226). *)
                (match collect with
                 | Some f -> f old_row
                 | None -> ());
                Lwt.return_unit)
             matches
         in
         let* () =
           match after_hook with
           | None -> Lwt.return_unit
           | Some f -> Lwt_list.iter_s (fun (_rowid, old_row) -> f ~tx ~old_row) matches
         in
         let* () = release_txn ~cat tx owned in
         Lwt.return n)
    (fun exn ->
       let* () = if owned then S.rollback tx else Lwt.return_unit in
       Lwt.fail exn)
;;

(** Run [Op_drop_table]: remove catalog entries for the table and all
    its indexes.  The B+-tree pages are NOT reclaimed in Phase 2.

    #279: runs through [with_ddl_txn] so it participates in any ambient explicit
    transaction (borrowed [In_txn]) or owns its own auto-committed txn ([Auto]),
    inheriting the same poison-on-failure / no-partial-effect-COMMIT behaviour as
    CREATE/ALTER (#286).  [drop_table] removes the table AND its dependent
    indexes from the in-memory cache before the caller commits, so a schema-cache
    undo is registered to restore both on a [ROLLBACK] (the store reverts the
    _sys_* row deletes; this re-syncs the cache).  The undo restores EXACTLY the
    entries [drop_table] removes; if an index was itself created earlier in the
    same transaction, this DROP undo restores it but the earlier CREATE INDEX's
    undo — running later in LIFO order — removes it again, netting the correct
    "absent after ROLLBACK" outcome. *)
let execute_drop_table
      ?(mode = Auto)
      (store : S.t)
      (cat : Cat.t)
      ~(table_meta : Cat.table_meta)
      ~(_indexes : Cat.index_info list)
  : unit Lwt.t
  =
  with_ddl_txn store cat mode (fun tx ->
    let name = table_meta.Cat.name in
    (* #283: [Cat.drop_table] self-registers the cache undo for the table and each
       dependent index (via [Schema_cache.remove_table]/[remove_index]), so no
       external snapshot+undo is needed here. *)
    Cat.drop_table cat tx ~name)
;;

(** Run [Op_drop_index]: remove catalog entry for the index.
    The B+-tree pages are NOT reclaimed in Phase 2.

    #279: as for [execute_drop_table] — runs through [with_ddl_txn] and registers
    a schema-cache undo so a [ROLLBACK] restores the dropped index entry. *)
let execute_drop_index
      ?(mode = Auto)
      (store : S.t)
      (cat : Cat.t)
      ~(idx_info : Cat.index_info)
  : unit Lwt.t
  =
  with_ddl_txn store cat mode (fun tx ->
    (* #283: [Cat.drop_index] self-registers the cache undo. *)
    Cat.drop_index cat tx ~name:idx_info.Cat.idx_name)
;;

(* ------------------------------------------------------------------ *)
(* EXPLAIN plan-tree pretty-printer                                     *)
(* ------------------------------------------------------------------ *)

let op_name = function
  | Plan.Op_seq_scan { table_meta } -> "SeqScan(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_filter _ -> "Filter"
  | Plan.Op_project _ -> "Project"
  | Plan.Op_expr_project _ -> "ExprProject"
  | Plan.Op_sort _ -> "Sort"
  | Plan.Op_limit { limit; offset; _ } ->
    Printf.sprintf "Limit(%d offset %d)" limit offset
  | Plan.Op_aggregate _ -> "Aggregate"
  | Plan.Op_hash_join { join_kind; _ } ->
    (match join_kind with
     | `Inner -> "HashJoin"
     | `Left -> "LeftHashJoin")
  | Plan.Op_nested_loop_join { join_kind; right_meta; _ } ->
    (match join_kind with
     | `Inner -> "NestedLoopJoin(" ^ right_meta.Cat.name ^ ")"
     | `Left -> "LeftNestedLoopJoin(" ^ right_meta.Cat.name ^ ")")
  | Plan.Op_index_lookup { table_meta; _ } -> "IndexLookup(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_rowid_lookup { table_meta; _ } -> "RowidLookup(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_union { all; _ } -> if all then "UnionAll" else "Union"
  | Plan.Op_intersect _ -> "Intersect"
  | Plan.Op_except _ -> "Except"
  | Plan.Op_distinct _ -> "Distinct"
  | Plan.Op_const_select _ -> "ConstSelect"
  | Plan.Op_window _ -> "Window"
  | Plan.Op_with_cte { cte_name; _ } -> "WithCte(" ^ cte_name ^ ")"
  | Plan.Op_cte_scan { cte_name; _ } -> "CteScan(" ^ cte_name ^ ")"
  | Plan.Op_insert { table_meta; _ } -> "Insert(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_insert_select { table_meta; _ } -> "InsertSelect(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_update { table_meta; _ } -> "Update(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_delete { table_meta; _ } -> "Delete(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_create_table { name; _ } -> "CreateTable(" ^ name ^ ")"
  | Plan.Op_create_index { name; table; _ } ->
    "CreateIndex(" ^ name ^ " on " ^ table ^ ")"
  | Plan.Op_drop_table { table_meta; _ } -> "DropTable(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_drop_index { idx_info } -> "DropIndex(" ^ idx_info.Cat.idx_name ^ ")"
  | Plan.Op_alter_table { table_meta; _ } -> "AlterTable(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_begin -> "Begin"
  | Plan.Op_commit -> "Commit"
  | Plan.Op_rollback -> "Rollback"
  | Plan.Op_savepoint name -> "Savepoint(" ^ name ^ ")"
  | Plan.Op_release name -> "Release(" ^ name ^ ")"
  | Plan.Op_rollback_to name -> "RollbackTo(" ^ name ^ ")"
  | Plan.Op_create_view { name; _ } -> "CreateView(" ^ name ^ ")"
  | Plan.Op_drop_view { name } -> "DropView(" ^ name ^ ")"
  | Plan.Op_create_trigger { name; _ } -> "CreateTrigger(" ^ name ^ ")"
  | Plan.Op_drop_trigger { name } -> "DropTrigger(" ^ name ^ ")"
  | Plan.Op_pragma_rows _ -> "Pragma"
  | Plan.Op_pragma_get_user_version -> "Pragma(get_user_version)"
  | Plan.Op_pragma_set_user_version { version } ->
    Printf.sprintf "Pragma(set_user_version=%Ld)" version
  | Plan.Op_pragma_integrity_check -> "Pragma(integrity_check)"
  | Plan.Op_pragma_get_fk -> "Pragma(get_foreign_keys)"
  | Plan.Op_pragma_set_fk { on } -> Printf.sprintf "Pragma(set_foreign_keys=%b)" on
  | Plan.Op_pragma_get_recursive_triggers -> "Pragma(get_recursive_triggers)"
  | Plan.Op_pragma_set_recursive_triggers { on } ->
    Printf.sprintf "Pragma(set_recursive_triggers=%b)" on
  | Plan.Op_pragma_get_defer_fk -> "Pragma(get_defer_foreign_keys)"
  | Plan.Op_pragma_set_defer_fk { on } ->
    Printf.sprintf "Pragma(set_defer_foreign_keys=%b)" on
  | Plan.Op_pragma_wal_checkpoint -> "Pragma(wal_checkpoint)"
  | Plan.Op_pragma_get_wal_autocheckpoint -> "Pragma(get_wal_autocheckpoint)"
  | Plan.Op_pragma_set_wal_autocheckpoint { n } ->
    Printf.sprintf "Pragma(set_wal_autocheckpoint=%Ld)" n
  | Plan.Op_pragma_get_synchronous -> "Pragma(get_synchronous)"
  | Plan.Op_pragma_set_synchronous { mode } ->
    Printf.sprintf "Pragma(set_synchronous=%s)" mode
  | Plan.Op_pragma_get_wal_batch_commits -> "Pragma(get_wal_batch_commits)"
  | Plan.Op_pragma_set_wal_batch_commits { n } ->
    Printf.sprintf "Pragma(set_wal_batch_commits=%Ld)" n
  | Plan.Op_pragma_get_wal_batch_interval_ms -> "Pragma(get_wal_batch_interval_ms)"
  | Plan.Op_pragma_set_wal_batch_interval_ms { n } ->
    Printf.sprintf "Pragma(set_wal_batch_interval_ms=%Ld)" n
  | Plan.Op_vacuum -> "Vacuum"
  | Plan.Op_attach { schema; _ } -> Printf.sprintf "Attach(%s)" schema
  | Plan.Op_detach { schema } -> Printf.sprintf "Detach(%s)" schema
  | Plan.Op_database_list -> "Pragma(database_list)"
  | Plan.Op_active_database_get -> "Pragma(active_database)"
  | Plan.Op_active_database_set { schema } ->
    Printf.sprintf "Pragma(active_database=%s)" schema
  | Plan.Op_no_op -> "NoOp"
  | Plan.Op_changes -> "Changes"
  | Plan.Op_last_insert_rowid -> "LastInsertRowid"
  | Plan.Op_total_changes -> "TotalChanges"
  | Plan.Op_explain { analyze; _ } -> if analyze then "ExplainAnalyze" else "Explain"
  | Plan.Op_create_fts_table { name; _ } -> "CreateFtsTable(" ^ name ^ ")"
  | Plan.Op_fts_insert { fts_meta; _ } -> "FtsInsert(" ^ fts_meta.Cat.fts_name ^ ")"
  | Plan.Op_fts_delete { fts_meta; _ } -> "FtsDelete(" ^ fts_meta.Cat.fts_name ^ ")"
  | Plan.Op_fts_seq_scan { fts_meta; _ } -> "FtsSeqScan(" ^ fts_meta.Cat.fts_name ^ ")"
  | Plan.Op_fts_match_scan { fts_meta; _ } ->
    "FtsMatchScan(" ^ fts_meta.Cat.fts_name ^ ")"
  | Plan.Op_sqlite_master -> "SqliteMaster"
  | Plan.Op_sqlite_sequence -> "SqliteSequence"
  | Plan.Op_seq_set { table; _ } -> "SeqSet(" ^ table ^ ")"
  | Plan.Op_seq_reset { table } ->
    "SeqReset("
    ^ (match table with
       | Some t -> t
       | None -> "*")
    ^ ")"
;;

let op_children = function
  | Plan.Op_filter { child; _ } -> [ child ]
  | Plan.Op_project { child; _ } -> [ child ]
  | Plan.Op_expr_project { child; _ } -> [ child ]
  | Plan.Op_sort { child; _ } -> [ child ]
  | Plan.Op_limit { child; _ } -> [ child ]
  | Plan.Op_distinct { child } -> [ child ]
  | Plan.Op_aggregate { child; _ } -> [ child ]
  | Plan.Op_window { child; _ } -> [ child ]
  | Plan.Op_hash_join { left; right; _ } -> [ left; right ]
  | Plan.Op_nested_loop_join { left; _ } -> [ left ]
  | Plan.Op_union { left; right; _ } -> [ left; right ]
  | Plan.Op_intersect { left; right } -> [ left; right ]
  | Plan.Op_except { left; right } -> [ left; right ]
  | Plan.Op_with_cte { def; query; _ } -> [ def; query ]
  | Plan.Op_explain { inner; _ } -> [ inner ]
  | Plan.Op_insert_select { source; _ } -> [ source ]
  | _ -> []
;;

let explain_plan op =
  let counter = ref 0 in
  let rec walk parent op =
    let id = !counter in
    incr counter;
    let my_row =
      [| Row.V_int (Int64.of_int id)
       ; Row.V_int (Int64.of_int parent)
       ; Row.V_text (op_name op)
      |]
    in
    my_row :: List.concat_map (walk id) (op_children op)
  in
  walk (-1) op
;;

(* #239: per-query cost/stats signal for an external cost-based cache.
   [rows_examined] counts rows the executor pulled from a base table/index scan
   (the true work signal: a query that scans a million rows to return one reads
   examined=1_000_000, returned=1); [rows_returned] is the size of the result
   stream once drained; [used_index] is the plan-time fact that the base access
   is an index/rowid seek rather than a full scan.  Mirage-pure — plain counters
   the executor already has, no clock/Unix dependency. *)
type query_stats =
  { mutable rows_examined : int
  ; mutable rows_returned : int
  ; mutable used_index : bool
  }

let make_query_stats () = { rows_examined = 0; rows_returned = 0; used_index = false }

(* The active query's stats record, propagated to the base scanners via Lwt
   sequence-associated storage rather than threaded through the ~50 mutually
   recursive [to_stream] helpers.  A leaf scanner reads it ONCE at stream
   construction (which runs inside [query]'s [with_value] scope, carried across
   binds), captures the result in its row-producing closure, and increments per
   row pulled — correct regardless of when the lazy stream is later drained, and
   safe across interleaved fibres because each query has its own record. *)
let query_stats_key : query_stats Lwt.key = Lwt.new_key ()

(* #262: the active transaction mode for the query currently executing, carried
   in Lwt sequence-associated storage.  Subquery evaluation ([pre_eval_subquery]
   and the correlated re-eval at pull time) reads it to run inner reads under the
   same txn, so [SELECT … WHERE x IN (SELECT …)] inside an open transaction sees
   the transaction's own uncommitted writes — not just the top-level scan.  The
   base scanners take [mode] as an explicit argument and do not consult this. *)
let txn_mode_key : txn_mode Lwt.key = Lwt.new_key ()

let current_txn_mode () =
  match Lwt.get txn_mode_key with
  | Some mode -> mode
  | None -> Auto
;;

(* #262: re-establish BOTH per-query Lwt-storage contexts (the stats record and
   the txn mode) for work that runs at pull time — outside [query]'s
   construction-time [with_value] scope — currently the correlated-subquery
   re-eval in [stream_filter] / [stream_expr_project].  Bundling the pair here
   keeps them in lock-step: a future pull-time site cannot restore one and
   silently drop the other (the exact omission #262 corrected for the mode). *)
let with_pull_context ~stats ~mode f =
  Lwt.with_value query_stats_key stats
  @@ fun () -> Lwt.with_value txn_mode_key (Some mode) f
;;

(* Increment via the closure-captured option; never calls [Lwt.get] at pull time
   (the consumer drains outside the [with_value] scope).  [None] for the common
   no-stats query is a single predicted branch with no per-row cost. *)
let incr_examined (s_opt : query_stats option) =
  match s_opt with
  | Some s -> s.rows_examined <- s.rows_examined + 1
  | None -> ()
;;

(** Forward reference to [to_stream], which is defined in the mutually-recursive
    block starting at [pre_eval_subquery].  [execute_with_count] needs this to
    implement [Op_insert_select] (read source, then write rows). *)
let to_stream_ref
  : ((unit -> float) option
     -> Row.value array
     -> S.t
     -> ?mode:txn_mode
     -> ?cat:Cat.t option
     -> Plan.op
     -> Row.t Lwt_stream.t Lwt.t)
      ref
  =
  ref (fun _clock _params _store ?mode:_ ?cat:_ _op ->
    failwith "to_stream_ref not yet initialised")
;;

(* Op_create_table: register the table, its UNIQUE indexes, and FK constraints. *)
(** [execute_with_count] returns the rows-affected count.  For most
    write ops this is 1 (INSERT) or 0 (DDL); for UPDATE it is the
    number of rows whose contents were modified. *)
let execute_create_table_op
      (store : S.t)
      (cat : Cat.t)
      ~mode
      ~name
      ~columns
      ~uniq_idxs
      ~if_not_exists
      ~fk_constraints
      ~without_rowid
      ~autoincrement
  : int Lwt.t
  =
  if Cat.table_exists cat ~name
  then
    if if_not_exists
    then Lwt.return 0
    else (* Raise synchronously (before acquiring any txn), as callers expect. *)
      failwith (Printf.sprintf "table '%s' already exists" name)
  else
    (* #269: the table, its implicit UNIQUE indexes, and its FK rows all go
       through one writer txn (the ambient explicit one if any), so the whole
       CREATE TABLE is atomic and never self-deadlocks. *)
    with_ddl_txn store cat mode (fun tx ->
      let* _tid =
        Cat.create_table ~txn:tx cat ~name ~columns ~without_rowid ~autoincrement
      in
      let* () =
        Lwt_list.iter_s
          (fun (idx_name, col_names, origin) ->
             let* result =
               Cat.create_index
                 ~txn:tx
                 cat
                 ~name:idx_name
                 ~table:name
                 ~columns:col_names
                 ~unique:true
                 ~expr_flags:(List.map (fun _ -> false) col_names)
                 ~where_sql:None
                 ~origin
             in
             match result with
             | Error msg -> Lwt.fail_with msg
             | Ok _ -> Lwt.return_unit)
          uniq_idxs
      in
      let* () =
        if fk_constraints = []
        then Lwt.return_unit
        else (
          let fk_list =
            List.map
              (fun (lcs, pt, pcs, od, ou, def) ->
                 Cat.
                   { fk_local_cols = lcs
                   ; fk_parent_table = pt
                   ; fk_parent_cols = pcs
                   ; fk_on_delete = od
                   ; fk_on_update = ou
                   ; fk_deferrable = def
                   })
              fk_constraints
          in
          let* () = Cat.save_fk_constraints ~txn:tx cat ~table_name:name ~fks:fk_list in
          (* Raw, undo-free cache update by design — reverted on ROLLBACK by
             [create_table]'s schema-cache undo, which removes the whole table
             entry.  See [Cat.set_fk_constraints]. *)
          Cat.set_fk_constraints cat ~table_name:name ~fks:fk_list;
          Lwt.return_unit)
      in
      Lwt.return 0)
;;

(* Op_insert: insert each VALUES row, counting successful inserts. *)
let execute_insert_values
      store
      (cat : Cat.t)
      ~mode
      ~params
      ~clock
      ~before_hook
      ~after_hook
      ~on_replace_delete_before
      ~on_replace_delete
      ~on_upsert_update_before
      ~on_upsert_update
      ~table_meta
      ~ordinals
      ~values
      ~on_conflict
      ~upsert_update
  : int Lwt.t
  =
  let bh =
    Option.map
      (fun f ~tx ~new_row -> f ~tx ~new_row:(Some new_row) ~old_row:None)
      before_hook
  in
  let ah =
    Option.map
      (fun f ~tx ~new_row -> f ~tx ~new_row:(Some new_row) ~old_row:None)
      after_hook
  in
  Lwt_list.fold_left_s
    (fun count row_vals ->
       let* inserted =
         execute_insert
           ~mode
           ~params
           ~clock
           ~on_conflict
           ~upsert_update
           ~before_hook:bh
           ~after_hook:ah
           ~on_replace_delete_before
           ~on_replace_delete
           ~on_upsert_update_before
           ~on_upsert_update
           store
           cat
           ~table_meta
           ~ordinals
           ~values:row_vals
       in
       Lwt.return (count + if inserted then 1 else 0))
    0
    values
;;

(* Op_insert_select: insert one row per source-stream row. *)
let execute_insert_select_op
      store
      (cat : Cat.t)
      ~mode
      ~params
      ~clock
      ~before_hook
      ~after_hook
      ~on_replace_delete_before
      ~on_replace_delete
      ~on_upsert_update_before
      ~on_upsert_update
      ~(table_meta : Cat.table_meta)
      ~ordinals
      ~source
      ~on_conflict
  : int Lwt.t
  =
  let n_cols = List.length table_meta.Cat.columns in
  let bh =
    Option.map
      (fun f ~tx ~new_row -> f ~tx ~new_row:(Some new_row) ~old_row:None)
      before_hook
  in
  let ah =
    Option.map
      (fun f ~tx ~new_row -> f ~tx ~new_row:(Some new_row) ~old_row:None)
      after_hook
  in
  let* stream = !to_stream_ref clock params store ~mode ~cat:(Some cat) source in
  let* src_rows = Lwt_stream.to_list stream in
  Lwt_list.fold_left_s
    (fun count src_row ->
       let row_arr = Array.make n_cols Row.V_null in
       List.iteri
         (fun i ord -> if i < Array.length src_row then row_arr.(ord) <- src_row.(i))
         ordinals;
       let* inserted =
         execute_insert
           ~mode
           ~params
           ~clock
           ~on_conflict
           ~before_hook:bh
           ~after_hook:ah
           ~on_replace_delete_before
           ~on_replace_delete
           ~on_upsert_update_before
           ~on_upsert_update
           store
           cat
           ~table_meta
           ~ordinals
           ~values:[]
           ~prebuilt_row:(Some row_arr)
       in
       Lwt.return (count + if inserted then 1 else 0))
    0
    src_rows
;;

(* Op_update dispatch: adapt the new/old-row hooks and delegate to execute_update. *)
let execute_update_op
      store
      cat
      ~mode
      ~params
      ~clock
      ~before_hook
      ~after_hook
      ~table_meta
      ~assignments
      ~where
      ~order
      ~limit
      ~offset
      ~indexes
  : int Lwt.t
  =
  let bh =
    Option.map
      (fun f ~tx ~old_row ~new_row ->
         f ~tx ~new_row:(Some new_row) ~old_row:(Some old_row))
      before_hook
  in
  let ah =
    Option.map
      (fun f ~tx ~old_row ~new_row ->
         f ~tx ~new_row:(Some new_row) ~old_row:(Some old_row))
      after_hook
  in
  execute_update
    ~mode
    ~params
    ~clock
    ~before_hook:bh
    ~after_hook:ah
    store
    cat
    ~table_meta
    ~assignments
    ~where
    ~order
    ~limit
    ~offset
    ~indexes
;;

(* Op_delete dispatch: adapt the old-row hooks and delegate to execute_delete. *)
let execute_delete_op
      store
      cat
      ~mode
      ~params
      ~clock
      ~before_hook
      ~after_hook
      ~table_meta
      ~where
      ~order
      ~limit
      ~offset
      ~indexes
  : int Lwt.t
  =
  let bh =
    Option.map
      (fun f ~tx ~old_row -> f ~tx ~new_row:None ~old_row:(Some old_row))
      before_hook
  in
  let ah =
    Option.map
      (fun f ~tx ~old_row -> f ~tx ~new_row:None ~old_row:(Some old_row))
      after_hook
  in
  execute_delete
    ~mode
    ~params
    ~clock
    ~before_hook:bh
    ~after_hook:ah
    store
    cat
    ~table_meta
    ~where
    ~order
    ~limit
    ~offset
    ~indexes
;;

(* Op_drop_table: drop the table and invalidate its cached CHECK / generated
   expressions. *)
let execute_drop_table_op
      store
      (cat : Cat.t)
      ~mode
      ~(table_meta : Cat.table_meta)
      ~indexes
  : int Lwt.t
  =
  let* () = execute_drop_table ~mode store cat ~table_meta ~_indexes:indexes in
  Hashtbl.filter_map_inplace
    (fun (tbl, _, _) v -> if String.equal tbl table_meta.name then None else Some v)
    check_expr_cache;
  Hashtbl.filter_map_inplace
    (fun (tbl, _, _) v -> if String.equal tbl table_meta.name then None else Some v)
    generated_expr_cache;
  Lwt.return 0
;;

(* Op_fts_insert: allocate a rowid, store the content row, and index it. *)
let execute_fts_insert
      store
      (cat : Cat.t)
      ~mode
      ~clock
      ~params
      (fts_meta : Cat.fts_table_meta)
      ~col_names
      ~col_values
      ~rowid_value
  : int Lwt.t
  =
  let* tx, owned = acquire_txn store mode in
  Lwt.catch
    (fun () ->
       (* #330: an explicit [rowid] is used verbatim (and the high-water advanced
          past it so a later auto-insert never reuses it); otherwise allocate the
          next rowid as before. *)
       let* rowid =
         match rowid_value with
         | None -> Cat.next_fts_rowid_in_txn cat ~name:fts_meta.Cat.fts_name tx
         | Some e ->
           let rowid =
             match eval_expr clock params [||] e with
             | Row.V_int n -> n
             | Row.V_real f -> Int64.of_float f
             | _ -> raise (Failure "FTS rowid must be an integer")
           in
           let* () =
             Cat.ensure_fts_rowid_above_in_txn cat ~name:fts_meta.Cat.fts_name tx rowid
           in
           Lwt.return rowid
       in
       let key = Rowid.encode rowid in
       (* #330: if a row already exists at this rowid (explicit-rowid collision),
          de-index it first so its index entries are not left stale. *)
       let* () =
         match rowid_value with
         | None -> Lwt.return_unit
         | Some _ ->
           let* existing = S.get tx fts_meta.Cat.fts_content_tree key in
           (match existing with
            | None -> Lwt.return_unit
            | Some old_bytes ->
              let old_texts = fts_decode_content old_bytes in
              let old_col_texts = List.mapi (fun i t -> i, t) old_texts in
              fts_deindex_document tx ~fts_meta ~rowid ~col_texts:old_col_texts)
       in
       let vals = List.map (fun e -> eval_expr clock params [||] e) col_values in
       let n_cols = List.length fts_meta.Cat.fts_columns in
       let texts = Array.make n_cols "" in
       List.iter2
         (fun col_name v ->
            match list_find_index (String.equal col_name) fts_meta.Cat.fts_columns with
            | None -> ()
            | Some (i, _) ->
              texts.(i)
              <- (match v with
                  | Row.V_text s -> s
                  | _ -> ""))
         col_names
         vals;
       let text_list = Array.to_list texts in
       let* () =
         S.put tx fts_meta.Cat.fts_content_tree key (fts_encode_content text_list)
       in
       let col_texts = List.mapi (fun i t -> i, t) text_list in
       let* () = fts_index_document tx ~fts_meta ~rowid ~col_texts in
       let* () = release_txn ~cat tx owned in
       Lwt.return 1)
    (fun exn ->
       let* () = if owned then S.rollback tx else Lwt.return_unit in
       Lwt.fail exn)
;;

(* Op_fts_delete: drain matching content rows, then delete + de-index them. *)
let execute_fts_delete
      store
      (cat : Cat.t)
      ~mode
      ~clock
      ~params
      (fts_meta : Cat.fts_table_meta)
      ~where
  : int Lwt.t
  =
  ignore cat;
  let* matches =
    S.with_ro store
    @@ fun tx_ro ->
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
        let keep =
          match where with
          | None -> true
          | Some pred -> value_truthy (eval_expr clock params row pred)
        in
        if keep then buf := (rowid, kbytes, texts) :: !buf;
        drain ()
    in
    drain ();
    S.cursor_close cur;
    Lwt.return (List.rev !buf)
  in
  let n = List.length matches in
  if n = 0
  then Lwt.return 0
  else
    let* tx, owned = acquire_txn store mode in
    Lwt.catch
      (fun () ->
         let* () =
           Lwt_list.iter_s
             (fun (rowid, key, texts) ->
                let col_texts = List.mapi (fun i t -> i, t) texts in
                let* () = S.del tx fts_meta.Cat.fts_content_tree key in
                fts_deindex_document tx ~fts_meta ~rowid ~col_texts)
             matches
         in
         let* () = release_txn ~cat tx owned in
         Lwt.return n)
      (fun exn ->
         let* () = if owned then S.rollback tx else Lwt.return_unit in
         Lwt.fail exn)
;;

(* Drop cached CHECK and generated-column expressions for [table_name]
   (used after DROP COLUMN, which can invalidate them). *)
let clear_table_expr_caches table_name =
  let clear cache =
    let to_clear =
      Hashtbl.fold
        (fun (tn, idx, sql) _ acc ->
           if String.equal tn table_name then (tn, idx, sql) :: acc else acc)
        cache
        []
    in
    List.iter (Hashtbl.remove cache) to_clear
  in
  clear check_expr_cache;
  clear generated_expr_cache
;;

(* Convert an AST column definition into a catalog [Row.column]. *)
let column_of_col_def col_def : Row.column =
  { Row.name = col_def.Ast.name
  ; Row.ty =
      (match col_def.Ast.ty with
       | Ast.Ty_int -> Row.Integer
       | Ast.Ty_text -> Row.Text
       | Ast.Ty_real -> Row.Real
       | Ast.Ty_blob -> Row.Blob)
  ; Row.not_null = col_def.Ast.not_null
  ; Row.primary_key = col_def.Ast.primary_key
  ; Row.pk_desc = col_def.Ast.pk_desc
  ; Row.default =
      (match col_def.Ast.default with
       | None -> None
       | Some Ast.L_null -> Some Row.DV_null
       | Some (Ast.L_int n) -> Some (Row.DV_int n)
       | Some (Ast.L_text s) -> Some (Row.DV_text s)
       | Some (Ast.L_real f) -> Some (Row.DV_real f)
       | Some (Ast.L_blob b) -> Some (Row.DV_blob b)
       | Some Ast.L_current_timestamp -> Some Row.DV_current_timestamp
       | Some Ast.L_current_date -> Some Row.DV_current_date
       | Some Ast.L_current_time -> Some Row.DV_current_time)
  ; Row.check_sql = Option.map Ast.expr_to_sql col_def.Ast.check
  ; Row.generated_as =
      Option.map (fun (e, s) -> Ast.expr_to_sql e, s = `Stored) col_def.Ast.generated_as
  }
;;

(* ALTER TABLE ADD COLUMN: add [col_def] to the catalog and persist any inline
   FK reference it declares. *)
let alter_add_column ?txn (cat : Cat.t) ~(table_meta : Cat.table_meta) col_def : int Lwt.t
  =
  let col = column_of_col_def col_def in
  let* result = Cat.add_column ?txn cat ~table_name:table_meta.Cat.name ~column:col in
  match result with
  | Error msg -> Lwt.fail_with msg
  | Ok () ->
    (match col_def.Ast.fk_ref with
     | None -> Lwt.return 0
     | Some (parent_table, parent_col, ast_od, ast_ou, ast_def) ->
       let inferred_parent_col =
         if parent_col = ""
         then (
           match Cat.find_table_cached cat ~name:parent_table with
           | None -> parent_col
           | Some pm ->
             (match
                List.find_opt (fun (c : Row.column) -> c.primary_key) pm.Cat.columns
              with
              | None -> parent_col
              | Some pk -> pk.Row.name))
         else parent_col
       in
       let new_fk : Cat.fk_constraint =
         { Cat.fk_local_cols = [ col_def.Ast.name ]
         ; Cat.fk_parent_table = parent_table
         ; Cat.fk_parent_cols = [ inferred_parent_col ]
         ; Cat.fk_on_delete = ast_od
         ; Cat.fk_on_update = ast_ou
         ; Cat.fk_deferrable = ast_def
         }
       in
       let existing_fks =
         match Cat.find_table_cached cat ~name:table_meta.Cat.name with
         | None -> []
         | Some m -> m.Cat.fk_constraints
       in
       let new_fks = existing_fks @ [ new_fk ] in
       let* () =
         Cat.save_fk_constraints ?txn cat ~table_name:table_meta.Cat.name ~fks:new_fks
       in
       (* The in-memory FK mutation is reverted on ROLLBACK by [add_column]'s
          schema-cache undo, which restores the whole prior [table_meta]. *)
       Cat.set_fk_constraints cat ~table_name:table_meta.Cat.name ~fks:new_fks;
       Lwt.return 0)
;;

(* ALTER TABLE DROP COLUMN: drop dependent indexes, migrate rows to the new
   shape, drop the catalog column, and invalidate cached expressions.

   #282: everything runs through the single writer transaction [tx] supplied by
   [with_ddl_txn] (borrowed from the ambient explicit transaction, or owned in
   autocommit) — no longer three separate [rw_begin]/[with_ro] phases (which
   would self-deadlock inside an explicit transaction).  The row scan reads
   THROUGH [tx] so rows inserted earlier in the same transaction are migrated
   (read-your-own-writes).  Dropped dependent indexes register a schema-cache
   undo so a [ROLLBACK] restores them. *)
let alter_drop_column tx (cat : Cat.t) ~(table_meta : Cat.table_meta) col_name : int Lwt.t
  =
  let table_name = table_meta.Cat.name in
  let col_idx = find_col_idx_by_name table_meta.Cat.columns col_name in
  let new_columns = List.filteri (fun i _ -> i <> col_idx) table_meta.Cat.columns in
  let idxs_on_col =
    List.filter
      (fun (idx : Cat.index_info) -> List.mem col_name idx.Cat.idx_columns)
      (Cat.indexes_for_table cat ~table:table_name)
  in
  let* () =
    Lwt_list.iter_s
      (fun (idx : Cat.index_info) -> Cat.drop_index cat tx ~name:idx.idx_name)
      idxs_on_col
  in
  (* #283: each [Cat.drop_index] above self-registers its own cache undo, so the
     dropped dependent indexes are restored on ROLLBACK without an external block. *)
  (* Drain every row through [tx] (read-your-own-writes) before rewriting, so the
     cursor is closed before we put back the reshaped rows into the same tree. *)
  let* cur = S.cursor_open tx table_meta.Cat.tree_id in
  let _sr = S.cursor_first cur in
  let rows = ref [] in
  let rec drain () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      let old_row = decode_with_virtual None [||] table_meta v in
      let new_row =
        Array.of_list (List.filteri (fun i _ -> i <> col_idx) (Array.to_list old_row))
      in
      rows := (Bytes.copy k, new_row) :: !rows;
      drain ()
  in
  drain ();
  S.cursor_close cur;
  let* () =
    Lwt_list.iter_s
      (fun (k, new_row) ->
         let new_bytes = Row.encode new_columns new_row in
         S.put tx table_meta.Cat.tree_id k new_bytes)
      !rows
  in
  let* result = Cat.drop_column ~txn:tx cat ~table_name ~col_name in
  match result with
  | Error msg -> Lwt.fail_with msg
  | Ok () ->
    clear_table_expr_caches table_name;
    Lwt.return 0
;;

(* ALTER TABLE RENAME TABLE: rename in the catalog and remap cached CHECK /
   generated-column entries from the old name to the new one. *)
let alter_rename_table ?txn (cat : Cat.t) ~(table_meta : Cat.table_meta) new_name
  : int Lwt.t
  =
  let* result = Cat.rename_table ?txn cat ~old_name:table_meta.Cat.name ~new_name in
  match result with
  | Error msg -> Lwt.fail_with msg
  | Ok () ->
    let remap tbl_cache =
      let to_add =
        Hashtbl.fold
          (fun (tbl, idx, sql) v acc ->
             if String.equal tbl table_meta.Cat.name
             then (new_name, idx, sql, v) :: acc
             else acc)
          tbl_cache
          []
      in
      List.iter
        (fun (_, idx, sql, _) -> Hashtbl.remove tbl_cache (table_meta.Cat.name, idx, sql))
        to_add;
      List.iter
        (fun (new_t, idx, sql, v) -> Hashtbl.add tbl_cache (new_t, idx, sql) v)
        to_add
    in
    remap check_expr_cache;
    remap generated_expr_cache;
    Lwt.return 0
;;

(* Op_alter_table: dispatch on the ALTER action.

   #282: the ALTER mutators run through [with_ddl_txn], which supplies a single
   writer transaction — borrowed from the ambient explicit transaction
   ([In_txn]) or owned and auto-committed ([Auto]).  Each catalog mutator threads
   that txn (no nested [rw_begin], which previously self-deadlocked inside
   [BEGIN…COMMIT]) and registers a schema-cache undo so a [ROLLBACK] reverts the
   in-memory catalog along with the store. *)
let execute_alter_table store (cat : Cat.t) ~mode ~(table_meta : Cat.table_meta) action
  : int Lwt.t
  =
  with_ddl_txn store cat mode (fun tx ->
    match action with
    | Ast.AA_add_column col_def -> alter_add_column ~txn:tx cat ~table_meta col_def
    | Ast.AA_rename_table new_name -> alter_rename_table ~txn:tx cat ~table_meta new_name
    | Ast.AA_rename_column (old_col, new_col) ->
      let* result =
        Cat.rename_column ~txn:tx cat ~table_name:table_meta.Cat.name ~old_col ~new_col
      in
      (match result with
       | Error msg -> Lwt.fail_with msg
       | Ok () -> Lwt.return 0)
    | Ast.AA_drop_column col_name -> alter_drop_column tx cat ~table_meta col_name)
;;

(* Op_create_index: create the index unless IF NOT EXISTS finds it present. *)
let execute_create_index_op
      store
      (cat : Cat.t)
      ~mode
      ~name
      ~table
      ~tree_id
      ~col_sqls
      ~col_expr_flags
      ~where_expr
      ~where_sql
      ~unique
      ~columns
      ~if_not_exists
  : int Lwt.t
  =
  if if_not_exists && Cat.index_exists cat ~name
  then Lwt.return 0
  else
    let* () =
      execute_create_index
        ~mode
        store
        cat
        ~name
        ~table
        ~tree_id
        ~col_sqls
        ~col_expr_flags
        ~where_expr
        ~where_sql
        ~unique
        ~columns
    in
    Lwt.return 0
;;

(** [execute_with_count] returns the rows-affected count.  For most
    write ops this is 1 (INSERT) or 0 (DDL); for UPDATE it is the
    number of rows whose contents were modified. *)
let execute_with_count
      ?(mode = Auto)
      ?(clock : (unit -> float) option = None)
      ?(params = [||])
      ?(before_hook :
          (tx:S.rw S.txn -> new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t)
            option =
        None)
      ?(after_hook :
          (tx:S.rw S.txn -> new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t)
            option =
        None)
      ?(on_replace_delete_before : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option =
        None)
      ?(on_replace_delete : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option = None)
      ?(on_upsert_update_before :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      ?(on_upsert_update :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      (store : S.t)
      (cat : Cat.t)
      (op : Plan.op)
  : int Lwt.t
  =
  match op with
  | Plan.Op_create_table
      { name
      ; columns
      ; uniq_idxs
      ; if_not_exists
      ; fk_constraints
      ; without_rowid
      ; autoincrement
      } ->
    execute_create_table_op
      store
      cat
      ~mode
      ~name
      ~columns
      ~uniq_idxs
      ~if_not_exists
      ~fk_constraints
      ~without_rowid
      ~autoincrement
  | Plan.Op_insert
      { table_meta; ordinals; values; on_conflict; returning = _; upsert_update } ->
    execute_insert_values
      store
      cat
      ~mode
      ~params
      ~clock
      ~before_hook
      ~after_hook
      ~on_replace_delete_before
      ~on_replace_delete
      ~on_upsert_update_before
      ~on_upsert_update
      ~table_meta
      ~ordinals
      ~values
      ~on_conflict
      ~upsert_update
  | Plan.Op_insert_select { table_meta; ordinals; source; on_conflict } ->
    execute_insert_select_op
      store
      cat
      ~mode
      ~params
      ~clock
      ~before_hook
      ~after_hook
      ~on_replace_delete_before
      ~on_replace_delete
      ~on_upsert_update_before
      ~on_upsert_update
      ~table_meta
      ~ordinals
      ~source
      ~on_conflict
  | Plan.Op_create_index
      { name
      ; table
      ; tree_id
      ; col_sqls
      ; col_expr_flags
      ; where_expr
      ; where_sql
      ; unique
      ; columns
      ; if_not_exists
      } ->
    execute_create_index_op
      store
      cat
      ~mode
      ~name
      ~table
      ~tree_id
      ~col_sqls
      ~col_expr_flags
      ~where_expr
      ~where_sql
      ~unique
      ~columns
      ~if_not_exists
  | Plan.Op_update
      { table_meta; assignments; where; order; limit; offset; indexes; returning = _ } ->
    execute_update_op
      store
      cat
      ~mode
      ~params
      ~clock
      ~before_hook
      ~after_hook
      ~table_meta
      ~assignments
      ~where
      ~order
      ~limit
      ~offset
      ~indexes
  | Plan.Op_delete { table_meta; where; order; limit; offset; indexes; returning = _ } ->
    execute_delete_op
      store
      cat
      ~mode
      ~params
      ~clock
      ~before_hook
      ~after_hook
      ~table_meta
      ~where
      ~order
      ~limit
      ~offset
      ~indexes
  | Plan.Op_seq_set { table; seq } ->
    (* #312.1: writable sqlite_sequence SET/INSERT.  Runs through [with_ddl_txn]
       so it participates in any ambient explicit transaction (borrowed [In_txn]
       — reverts on ROLLBACK) or owns its own auto-committed txn ([Auto]). *)
    with_ddl_txn store cat mode (fun tx ->
      let* () = Cat.set_next_rowid_in_txn cat ~name:table ~requested:seq tx in
      (* #317.3: the UPDATE/INSERT form touches exactly one sqlite_sequence row,
         so [changes()] reports 1 — SQLite parity.  (The DELETE/reset form below
         still reports 0: we do not materialise per-table rows to count.) *)
      Lwt.return 1)
  | Plan.Op_seq_reset { table } ->
    (* #312.1: writable sqlite_sequence DELETE.  [None] (bare DELETE, no WHERE)
       resets every AUTOINCREMENT counter — SQLite parity, and what a real
       [sqlite3 .dump] emits before re-INSERTing. *)
    with_ddl_txn store cat mode (fun tx ->
      let* () =
        match table with
        | Some name -> Cat.reset_next_rowid_in_txn cat ~name tx
        | None -> Cat.reset_all_next_rowid_in_txn cat tx
      in
      Lwt.return 0)
  | Plan.Op_drop_table { table_meta; indexes } ->
    execute_drop_table_op store cat ~mode ~table_meta ~indexes
  | Plan.Op_drop_index { idx_info } ->
    let* () = execute_drop_index ~mode store cat ~idx_info in
    Lwt.return 0
  | Plan.Op_create_fts_table { name; columns } ->
    (* #269: thread any ambient explicit txn so CREATE VIRTUAL TABLE … USING fts5
       does not self-deadlock and rolls back atomically. *)
    with_ddl_txn store cat mode (fun tx ->
      let* _ = Cat.create_fts_table ~txn:tx cat ~name ~columns in
      Lwt.return 0)
  | Plan.Op_fts_insert { fts_meta; col_names; col_values; rowid_value } ->
    execute_fts_insert
      store
      cat
      ~mode
      ~clock
      ~params
      fts_meta
      ~col_names
      ~col_values
      ~rowid_value
  | Plan.Op_fts_delete { fts_meta; where } ->
    execute_fts_delete store cat ~mode ~clock ~params fts_meta ~where
  | Plan.Op_alter_table { table_meta; action } ->
    execute_alter_table store cat ~mode ~table_meta action
  | Plan.Op_begin
  | Plan.Op_commit
  | Plan.Op_rollback
  | Plan.Op_savepoint _
  | Plan.Op_release _
  | Plan.Op_rollback_to _ ->
    failwith
      "Exec.execute_with_count: BEGIN/COMMIT/ROLLBACK/SAVEPOINT handled by Db layer"
  | Plan.Op_pragma_rows _ -> Lwt.return 0
  | Plan.Op_pragma_set_user_version { version } ->
    let* tx = S.rw_begin store in
    let* () = Cat.write_user_version_tx tx version in
    let* () = S.commit tx in
    Lwt.return 0
  | Plan.Op_pragma_set_fk { on } ->
    Cat.set_fk_enforcement cat on;
    Lwt.return 0
  | Plan.Op_pragma_set_recursive_triggers { on } ->
    Cat.set_recursive_triggers cat on;
    Lwt.return 0
  | Plan.Op_pragma_set_defer_fk { on } ->
    Cat.set_defer_fks_pragma cat on;
    Lwt.return 0
  | Plan.Op_pragma_wal_checkpoint ->
    let* () = S.checkpoint store in
    Lwt.return 0
  | Plan.Op_pragma_set_wal_autocheckpoint { n } ->
    S.set_wal_autocheckpoint store (Int64.to_int n);
    Lwt.return 0
  | Plan.Op_pragma_set_synchronous { mode } ->
    if mode <> "full" && S.commit_callback_active store
    then
      failwith
        "PRAGMA synchronous: durability cannot be relaxed while a replication \
         commit-sink is active (replication requires synchronous=full)"
    else (
      let d =
        match mode with
        | "full" -> S.Full
        | "off" -> S.Off
        | "batched" ->
          S.Batched
            { commits = S.sync_batch_commits store
            ; interval_ms = S.sync_batch_interval_ms store
            }
        | _ -> failwith (Printf.sprintf "PRAGMA synchronous: unknown mode %s" mode)
      in
      let* () = if mode = "full" then S.flush_unsynced store else Lwt.return_unit in
      S.set_durability store d;
      Lwt.return 0)
  | Plan.Op_pragma_set_wal_batch_commits { n } ->
    S.set_sync_batch_commits store (Int64.to_int n);
    Lwt.return 0
  | Plan.Op_pragma_set_wal_batch_interval_ms { n } ->
    S.set_sync_batch_interval_ms store (Int64.to_int n);
    Lwt.return 0
  | Plan.Op_vacuum ->
    Lwt.fail_with "VACUUM must be executed via Db.execute / Db.vacuum (no Db handle)"
  | Plan.Op_attach _
  | Plan.Op_detach _
  | Plan.Op_database_list
  | Plan.Op_active_database_get
  | Plan.Op_active_database_set _ ->
    Lwt.fail_with
      "ATTACH/DETACH/database_list/active_database must be executed via Db.execute"
  | Plan.Op_create_view _
  | Plan.Op_drop_view _
  | Plan.Op_create_trigger _
  | Plan.Op_drop_trigger _
  | Plan.Op_no_op -> Lwt.return 0
  | Plan.Op_explain _ -> Lwt.return 0
  | Plan.Op_union _
  | Plan.Op_intersect _
  | Plan.Op_except _
  | Plan.Op_const_select _
  | Plan.Op_with_cte _
  | Plan.Op_cte_scan _
  | Plan.Op_window _
  | Plan.Op_pragma_get_user_version
  | Plan.Op_pragma_integrity_check
  | Plan.Op_pragma_get_fk
  | Plan.Op_pragma_get_recursive_triggers
  | Plan.Op_pragma_get_defer_fk
  | Plan.Op_pragma_get_wal_autocheckpoint
  | Plan.Op_changes
  | Plan.Op_last_insert_rowid
  | Plan.Op_total_changes -> failwith "Exec.execute: use Exec.query for read operations"
  | Plan.Op_seq_scan _
  | Plan.Op_filter _
  | Plan.Op_project _
  | Plan.Op_expr_project _
  | Plan.Op_sort _
  | Plan.Op_limit _
  | Plan.Op_index_lookup _
  | Plan.Op_rowid_lookup _
  | Plan.Op_nested_loop_join _
  | Plan.Op_hash_join _
  | Plan.Op_aggregate _
  | Plan.Op_fts_seq_scan _
  | Plan.Op_fts_match_scan _
  | Plan.Op_distinct _
  | Plan.Op_sqlite_master
  | Plan.Op_sqlite_sequence
  | Plan.Op_pragma_get_synchronous
  | Plan.Op_pragma_get_wal_batch_commits
  | Plan.Op_pragma_get_wal_batch_interval_ms ->
    failwith "Exec.execute: use Exec.query for read operations"
;;

(** Compatibility entry point: discards the rows-affected count. *)
let execute
      ?(mode = Auto)
      ?(clock : (unit -> float) option = None)
      ?(params = [||])
      ?(before_hook :
          (tx:S.rw S.txn -> new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t)
            option =
        None)
      ?(after_hook :
          (tx:S.rw S.txn -> new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t)
            option =
        None)
      ?(on_replace_delete_before : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option =
        None)
      ?(on_replace_delete : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option = None)
      ?(on_upsert_update_before :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      ?(on_upsert_update :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      (store : S.t)
      (cat : Cat.t)
      (op : Plan.op)
  : unit Lwt.t
  =
  let* _n =
    execute_with_count
      ~mode
      ~clock
      ~params
      ~before_hook
      ~after_hook
      ~on_replace_delete_before
      ~on_replace_delete
      ~on_upsert_update_before
      ~on_upsert_update
      store
      cat
      op
  in
  Lwt.return_unit
;;

(* ------------------------------------------------------------------ *)
(* BM25 scoring helpers                                                 *)
(* ------------------------------------------------------------------ *)

let bm25_score ~k1 ~b ~total_docs ~total_tokens ~n_docs_with_term ~term_freq ~doc_length =
  if total_docs = 0 || n_docs_with_term = 0
  then 0.0
  else (
    let n = Float.of_int total_docs in
    let n_t = Float.of_int n_docs_with_term in
    let tf = Float.of_int term_freq in
    let dl = Float.of_int doc_length in
    let avgdl = Float.of_int total_tokens /. n in
    let idf = Float.log (((n -. n_t +. 0.5) /. (n_t +. 0.5)) +. 1.0) in
    idf *. (tf *. (k1 +. 1.0)) /. (tf +. (k1 *. (1.0 -. b +. (b *. dl /. avgdl)))))
;;

(** Collect all positive (non-negated) terms from a query for BM25. *)
let fts_query_terms query =
  let rec collect = function
    | Fts_query.FQ_term (Fts_query.FT_exact t) -> [ t ]
    | Fts_query.FQ_term (Fts_query.FT_prefix t) -> [ t ]
    | Fts_query.FQ_term (Fts_query.FT_phrase ts) -> ts
    | Fts_query.FQ_and qs | Fts_query.FQ_or qs -> List.concat_map collect qs
    | Fts_query.FQ_not _ -> []
  in
  List.sort_uniq String.compare (collect query)
;;

(** A snippet phrase is the unit SQLite FTS5 reports via xPhraseSize:
    either a single token (exact or prefix) or a multi-token exact
    phrase. The multi-token form requires consecutive token matches
    and is scored ONCE per occurrence (not once per constituent
    token) to mirror SQLite's centering and bm25 behaviour. *)
type snippet_phrase =
  | SP_term of string * [ `Exact | `Prefix ]
  | SP_phrase of string list (* length >= 2; all matched exactly *)

(** Collect snippet phrases from a query in left-to-right order. *)
let fts_query_terms_with_kind query : snippet_phrase list =
  let rec collect = function
    | Fts_query.FQ_term (Fts_query.FT_exact t) -> [ SP_term (t, `Exact) ]
    | Fts_query.FQ_term (Fts_query.FT_prefix t) -> [ SP_term (t, `Prefix) ]
    | Fts_query.FQ_term (Fts_query.FT_phrase ts) ->
      (match ts with
       | [] -> []
       | [ t ] -> [ SP_term (t, `Exact) ]
       | _ -> [ SP_phrase ts ])
    | Fts_query.FQ_and qs | Fts_query.FQ_or qs -> List.concat_map collect qs
    | Fts_query.FQ_not _ -> []
  in
  (* De-duplicate identical phrases (so a query like `foo AND foo` does
     not over-credit token highlights). Preserve first-occurrence order. *)
  let seen = Hashtbl.create 8 in
  let key = function
    | SP_term (t, `Exact) -> "e:" ^ t
    | SP_term (t, `Prefix) -> "p:" ^ t
    | SP_phrase ts -> "P:" ^ String.concat "\x00" ts
  in
  List.filter
    (fun p ->
       let k = key p in
       if Hashtbl.mem seen k
       then false
       else (
         Hashtbl.add seen k ();
         true))
    (collect query)
;;

(** Test whether the phrase at index [pi] matches the token sequence
    starting at [tokens.(i)]. Returns the phrase length on hit (so the
    caller can compute the end position), else [None]. *)
let phrase_match_at
      ~(phrases : snippet_phrase array)
      ~(tokens : Fts_tokenizer.token array)
      (pi : int)
      (i : int)
  : int option
  =
  let n_toks = Array.length tokens in
  let token_at j = tokens.(j).Fts_tokenizer.term in
  match phrases.(pi) with
  | SP_term (t, `Exact) ->
    if i < n_toks && String.equal (token_at i) t then Some 1 else None
  | SP_term (t, `Prefix) ->
    if i < n_toks
    then (
      let tk = token_at i in
      if
        String.length tk >= String.length t
        && String.equal (String.sub tk 0 (String.length t)) t
      then Some 1
      else None)
    else None
  | SP_phrase ts ->
    let len = List.length ts in
    if i + len > n_toks
    then None
    else (
      let rec walk j = function
        | [] -> true
        | t :: rest ->
          if String.equal (token_at (i + j)) t then walk (j + 1) rest else false
      in
      if walk 0 ts then Some len else None)
;;

(** Find the first phrase that matches at token position [i].
    Returns [(phrase_idx, length)] if any. *)
let token_phrase_match
      ~(phrases : snippet_phrase array)
      ~(tokens : Fts_tokenizer.token array)
      (i : int)
  : (int * int) option
  =
  let n = Array.length phrases in
  let rec loop pi =
    if pi >= n
    then None
    else (
      match phrase_match_at ~phrases ~tokens pi i with
      | Some len -> Some (pi, len)
      | None -> loop (pi + 1))
  in
  loop 0
;;

(** Identify FTS5 "sentence start" token positions in a column.
    Position 0 is always a sentence start. Any token preceded (after any
    intervening whitespace) by '.' or ':' also starts a sentence. *)
let fts_sentence_starts ~col_text ~(tokens : Fts_tokenizer.token array) : int array =
  let n = Array.length tokens in
  let buf = Buffer.create 8 in
  for i = 0 to n - 1 do
    let tok = tokens.(i) in
    if i = 0
    then Buffer.add_string buf (string_of_int 0)
    else (
      let start = tok.Fts_tokenizer.start_byte in
      (* Walk backwards skipping ' ', '\t', '\n', '\r'. *)
      let j = ref (start - 1) in
      while
        !j >= 0
        &&
        let c = col_text.[!j] in
        c = ' ' || c = '\t' || c = '\n' || c = '\r'
      do
        decr j
      done;
      if !j >= 0
      then (
        let c = col_text.[!j] in
        if c = '.' || c = ':'
        then (
          if Buffer.length buf > 0 then Buffer.add_char buf ',';
          Buffer.add_string buf (string_of_int i))))
  done;
  if Buffer.length buf = 0
  then [| 0 |]
  else
    Array.of_list
      (List.map int_of_string (String.split_on_char ',' (Buffer.contents buf)))
;;

(** Score a candidate window [i_pos, i_pos + n_token).
    Returns [(score, i_adj)] where:
      - score = 1000 for each new phrase instance seen + 1 for repeats.
      - i_adj = the actual starting position after centering adjustment,
                clamped to [0, n_docsize - n_token] (or 0 if window > doc).
    [a_seen] is reset by the caller before each call.
    [instances] is a sorted list of [(phrase_idx, position, length)] —
    a multi-token phrase counts as a single contiguous instance whose
    extent spans [position, position + length). *)
let fts_snippet_score
      ~(instances : (int * int * int) list)
      ~(a_seen : bool array)
      ~(i_pos : int)
      ~(n_token : int)
      ~(n_docsize : int)
  : int * int
  =
  let i_end = i_pos + n_token in
  let score = ref 0 in
  let i_first = ref (-1) in
  let i_last = ref 0 in
  List.iter
    (fun (ip, io, len) ->
       (* Phrase fully inside the window. SQLite requires the entire
       phrase span to fit; partial overlaps don't count. *)
       if io >= i_pos && io + len <= i_end
       then (
         score := !score + if a_seen.(ip) then 1 else 1000;
         a_seen.(ip) <- true;
         if !i_first < 0 then i_first := io;
         i_last := io + len))
    instances;
  let i_adj =
    if !i_first < 0 then i_pos else !i_first - ((n_token - (!i_last - !i_first)) / 2)
  in
  let i_adj = if i_adj + n_token > n_docsize then n_docsize - n_token else i_adj in
  let i_adj = if i_adj < 0 then 0 else i_adj in
  !score, i_adj
;;

(* Greedy scan for snippet phrase matches: at each token take the first phrase
   that matches, skipping past its length. Returns [(phrase_idx,pos,len)] list. *)
(** Build a highlighted excerpt of [col_text] for the given snippet [spec].
    Replicates SQLite FTS5's snippet() algorithm:
      - For each phrase instance, score the window anchored at its position
        (with centering adjustment), and also the window anchored at the
        latest preceding sentence start (with a +100 or +120 bonus).
      - Pick the (strictly) highest-scoring window; tie → earliest considered.
      - Reconstruct text from byte offsets, wrapping matched tokens (whole
        token for prefix matches) with [start_tag]/[end_tag].
      - Prepend [ellipsis] unless window starts at token 0.
      - Append [ellipsis] unless window covers through the last token.
    [query_terms] is a list of snippet phrases. *)
let snippet_build_instances ~phrases ~tokens ~n_toks =
  let acc = ref [] in
  let i = ref 0 in
  while !i < n_toks do
    match token_phrase_match ~phrases ~tokens !i with
    | None -> incr i
    | Some (ip, len) ->
      acc := (ip, tokens.(!i).Fts_tokenizer.pos, len) :: !acc;
      i := !i + len
  done;
  List.rev !acc
;;

(* No-match snippet: SQLite anchors at sentence start 0 and emits the first
   n_token tokens (no leading ellipsis; trailing ellipsis if doc is longer). *)
let snippet_no_match ~col_text ~tokens ~n_toks ~n_token ~spec =
  if n_toks = 0
  then ""
  else (
    let win_end_excl = min n_toks n_token in
    let last_tok = tokens.(win_end_excl - 1) in
    let prefix_text = String.sub col_text 0 last_tok.Fts_tokenizer.end_byte in
    if win_end_excl >= n_toks then prefix_text else prefix_text ^ spec.Plan.ellipsis)
;;

(* Choose the best snippet window start: score each instance position and each
   preceding sentence start (with a sentence-alignment bonus). *)
(* Score the candidate windows anchored at instance offset [io] — both the
   centered window and (when the column is longer than one window) the latest
   sentence start before [io], with a sentence-alignment bonus — feeding each
   to [consider]. *)
let snippet_score_instance
      ~consider
      ~instances
      ~a_seen
      ~sentence_starts
      ~n_phrases
      ~n_token
      ~n_toks
      io
  =
  (* Non-sentence-aligned: window anchored at this instance, centered. *)
  Array.fill a_seen 0 n_phrases false;
  let score, i_adj =
    fts_snippet_score ~instances ~a_seen ~i_pos:io ~n_token ~n_docsize:n_toks
  in
  consider score i_adj;
  (* Sentence-aligned: latest sentence start strictly before io. *)
  if n_toks > n_token
  then (
    let n_sent = Array.length sentence_starts in
    let jj = ref 0 in
    while !jj < n_sent - 1 && sentence_starts.(!jj + 1) <= io do
      incr jj
    done;
    let s_start = sentence_starts.(!jj) in
    if s_start < io
    then (
      Array.fill a_seen 0 n_phrases false;
      let score, _ =
        fts_snippet_score ~instances ~a_seen ~i_pos:s_start ~n_token ~n_docsize:n_toks
      in
      let bonus = if s_start = 0 then 120 else 100 in
      consider (score + bonus) s_start))
;;

let snippet_best_window ~instances ~tokens ~col_text ~n_phrases ~n_token ~n_toks =
  let a_seen = Array.make (max 1 n_phrases) false in
  let sentence_starts = fts_sentence_starts ~col_text ~tokens in
  let best_score = ref 0 in
  let best_start = ref 0 in
  let consider score start_pos =
    if score > !best_score
    then (
      best_score := score;
      best_start := start_pos)
  in
  List.iter
    (fun (_ip, io, _len) ->
       snippet_score_instance
         ~consider
         ~instances
         ~a_seen
         ~sentence_starts
         ~n_phrases
         ~n_token
         ~n_toks
         io)
    instances;
  !best_start
;;

(* Reconstruct the snippet text for the chosen window, wrapping matched phrase
   instances in start/end tags and emitting leading/trailing ellipses. *)
let snippet_render
      ~col_text
      ~tokens
      ~token_instance_at
      ~i_best_start
      ~n_token
      ~n_toks
      ~spec
  =
  let i_range_end = i_best_start + n_token - 1 in
  let buf = Buffer.create 128 in
  if i_best_start > 0 then Buffer.add_string buf spec.Plan.ellipsis;
  if n_toks > 0
  then (
    let first_in_range = i_best_start in
    let last_in_range = min (n_toks - 1) i_range_end in
    let prev_end = ref tokens.(first_in_range).Fts_tokenizer.start_byte in
    let prev_inst = ref (-1) in
    for i = first_in_range to last_in_range do
      let tok = tokens.(i) in
      let inst = token_instance_at.(i) in
      let gap_len = tok.Fts_tokenizer.start_byte - !prev_end in
      let gap = if gap_len > 0 then String.sub col_text !prev_end gap_len else "" in
      if !prev_inst <> inst
      then (
        (* Close the previous wrap, emit gap outside, open a new wrap if
           entering a phrase instance. *)
        if !prev_inst >= 0 then Buffer.add_string buf spec.Plan.end_tag;
        Buffer.add_string buf gap;
        if inst >= 0 then Buffer.add_string buf spec.Plan.start_tag)
      else
        (* Same wrap state — gap belongs to it (e.g. space inside <b>..</b>). *)
        Buffer.add_string buf gap;
      Buffer.add_string
        buf
        (String.sub
           col_text
           tok.Fts_tokenizer.start_byte
           (tok.Fts_tokenizer.end_byte - tok.Fts_tokenizer.start_byte));
      prev_end := tok.Fts_tokenizer.end_byte;
      prev_inst := inst
    done;
    if !prev_inst >= 0 then Buffer.add_string buf spec.Plan.end_tag;
    (* Trailing: append the rest of the source if the window reaches the last
       token, else a trailing ellipsis. *)
    if i_range_end >= n_toks - 1
    then (
      let last_end = tokens.(last_in_range).Fts_tokenizer.end_byte in
      if last_end < String.length col_text
      then
        Buffer.add_string
          buf
          (String.sub col_text last_end (String.length col_text - last_end)))
    else Buffer.add_string buf spec.Plan.ellipsis);
  Buffer.contents buf
;;

let compute_snippet
      ~col_text
      ~(query_terms : snippet_phrase list)
      ~(spec : Plan.snippet_spec)
  =
  let tokens = Array.of_list (Fts_tokenizer.tokenize_string ~col:0 col_text) in
  let n_toks = Array.length tokens in
  let phrases = Array.of_list query_terms in
  let n_phrases = Array.length phrases in
  let n_token = max 1 spec.Plan.n_tokens in
  let instances = snippet_build_instances ~phrases ~tokens ~n_toks in
  (* Mark each token position with its covering instance index (-1 = none),
     so adjacent occurrences of the same phrase emit separate wraps. *)
  let token_instance_at = Array.make (max 1 n_toks) (-1) in
  List.iteri
    (fun inst_idx (_ip, io, len) ->
       for k = 0 to len - 1 do
         if io + k < n_toks then token_instance_at.(io + k) <- inst_idx
       done)
    instances;
  if instances = [] || n_phrases = 0
  then snippet_no_match ~col_text ~tokens ~n_toks ~n_token ~spec
  else (
    let i_best_start =
      snippet_best_window ~instances ~tokens ~col_text ~n_phrases ~n_token ~n_toks
    in
    snippet_render
      ~col_text
      ~tokens
      ~token_instance_at
      ~i_best_start
      ~n_token
      ~n_toks
      ~spec)
;;

(* ------------------------------------------------------------------ *)
(* substitute_cte: replace Op_cte_scan nodes with Op_pragma_rows       *)
(* to_stream: convert a read op tree into a Row stream                  *)
(* pre_eval_subquery: resolve subquery Plan.expr nodes before row scan  *)
(* ------------------------------------------------------------------ *)

(** Check whether any unresolved subquery nodes remain in a Plan.expr. *)
let rec plan_expr_has_subquery : Plan.expr -> bool = function
  | Plan.P_subquery _ | Plan.P_exists _ | Plan.P_in_select _ -> true
  | Plan.P_binop (_, a, b) -> plan_expr_has_subquery a || plan_expr_has_subquery b
  | Plan.P_not e
  | Plan.P_is_null e
  | Plan.P_is_not_null e
  | Plan.P_neg e
  | Plan.P_bitnot e -> plan_expr_has_subquery e
  | Plan.P_between (x, lo, hi) ->
    plan_expr_has_subquery x || plan_expr_has_subquery lo || plan_expr_has_subquery hi
  | Plan.P_in (x, vs) -> plan_expr_has_subquery x || List.exists plan_expr_has_subquery vs
  | Plan.P_func (_, args) -> List.exists plan_expr_has_subquery args
  | Plan.P_case { scrutinee; branches; else_ } ->
    Option.fold ~none:false ~some:plan_expr_has_subquery scrutinee
    || List.exists
         (fun (c, r) -> plan_expr_has_subquery c || plan_expr_has_subquery r)
         branches
    || Option.fold ~none:false ~some:plan_expr_has_subquery else_
  | Plan.P_cast (e, _) -> plan_expr_has_subquery e
  | Plan.P_collate (e, _) -> plan_expr_has_subquery e
  | _ -> false
;;

(** Extract table_meta from the leftmost seq scan in a plan op. *)
let rec get_outer_scan_meta : Plan.op -> Cat.table_meta option = function
  | Plan.Op_seq_scan { table_meta } -> Some table_meta
  | Plan.Op_filter { child; _ } -> get_outer_scan_meta child
  | Plan.Op_sort { child; _ } -> get_outer_scan_meta child
  | Plan.Op_limit { child; _ } -> get_outer_scan_meta child
  | Plan.Op_index_lookup { table_meta; _ } -> Some table_meta
  | Plan.Op_rowid_lookup { table_meta; _ } -> Some table_meta
  | _ -> None
;;

(** Substitute outer column refs (table.col) with literal values from the outer row. *)
let rec substitute_outer_in_expr (meta : Cat.table_meta) (row : Row.t) (e : Ast.expr)
  : Ast.expr
  =
  let go = substitute_outer_in_expr meta row in
  match e with
  | Ast.E_tbl_col (tbl, col) when String.equal tbl meta.Cat.name ->
    (try
       let i = find_col_idx_by_name meta.Cat.columns col in
       Ast.E_lit (value_to_literal row.(i))
     with
     | Failure _ -> e)
  | Ast.E_binop (op, a, b) -> Ast.E_binop (op, go a, go b)
  | Ast.E_not a -> Ast.E_not (go a)
  | Ast.E_is_null a -> Ast.E_is_null (go a)
  | Ast.E_is_not_null a -> Ast.E_is_not_null (go a)
  | Ast.E_neg a -> Ast.E_neg (go a)
  | Ast.E_bitnot a -> Ast.E_bitnot (go a)
  | Ast.E_between (x, lo, hi) -> Ast.E_between (go x, go lo, go hi)
  | Ast.E_in (x, vals) -> Ast.E_in (go x, List.map go vals)
  | Ast.E_func (f, args) -> Ast.E_func (f, List.map go args)
  | Ast.E_cast (x, ty) -> Ast.E_cast (go x, ty)
  | Ast.E_case { scrutinee; branches; else_ } ->
    Ast.E_case
      { scrutinee = Option.map go scrutinee
      ; branches = List.map (fun (c, r) -> go c, go r) branches
      ; else_ = Option.map go else_
      }
  | _ -> e
;;

(** Substitute outer column refs in any embedded Ast.stmt nodes inside a
    Plan.expr (correlated subqueries / EXISTS / IN). *)
let rec substitute_outer_in_plan_expr
          (meta : Cat.table_meta)
          (row : Row.t)
          (e : Plan.expr)
  : Plan.expr
  =
  let go = substitute_outer_in_plan_expr meta row in
  match e with
  | Plan.P_exists inner -> Plan.P_exists (substitute_outer_in_stmt meta row inner)
  | Plan.P_in_select (x, inner) ->
    Plan.P_in_select (go x, substitute_outer_in_stmt meta row inner)
  | Plan.P_subquery inner -> Plan.P_subquery (substitute_outer_in_stmt meta row inner)
  | Plan.P_binop (op, a, b) -> Plan.P_binop (op, go a, go b)
  | Plan.P_not a -> Plan.P_not (go a)
  | Plan.P_is_null a -> Plan.P_is_null (go a)
  | Plan.P_is_not_null a -> Plan.P_is_not_null (go a)
  | Plan.P_neg a -> Plan.P_neg (go a)
  | Plan.P_bitnot a -> Plan.P_bitnot (go a)
  | Plan.P_between (x, lo, hi) -> Plan.P_between (go x, go lo, go hi)
  | Plan.P_in (x, vs) -> Plan.P_in (go x, List.map go vs)
  | Plan.P_func (f, args) -> Plan.P_func (f, List.map go args)
  | Plan.P_case { scrutinee; branches; else_ } ->
    Plan.P_case
      { scrutinee = Option.map go scrutinee
      ; branches = List.map (fun (c, r) -> go c, go r) branches
      ; else_ = Option.map go else_
      }
  | Plan.P_cast (e, ty) -> Plan.P_cast (go e, ty)
  | _ -> e

(** Apply substitute_outer_in_expr to WHERE/HAVING/JOIN ON clauses in an AST stmt. *)
and substitute_outer_in_stmt (meta : Cat.table_meta) (row : Row.t) (s : Ast.stmt)
  : Ast.stmt
  =
  let go_e = substitute_outer_in_expr meta row in
  let go_s = substitute_outer_in_stmt meta row in
  match s with
  | Ast.S_select r ->
    Ast.S_select
      { r with
        where = Option.map go_e r.where
      ; having = Option.map go_e r.having
      ; joins = List.map (fun j -> { j with Ast.on = go_e j.Ast.on }) r.joins
      }
  | Ast.S_compound { op; left; right; order; limit; offset } ->
    Ast.S_compound { op; left = go_s left; right = go_s right; order; limit; offset }
  | Ast.S_with_cte { name; def; query; recursive } ->
    Ast.S_with_cte { name; def = go_s def; query = go_s query; recursive }
  | _ -> s
;;

let rec substitute_cte ~(cte_name : string) ~(rows : Row.t list) (op : Plan.op) : Plan.op =
  let go = substitute_cte ~cte_name ~rows in
  match op with
  | Plan.Op_cte_scan { cte_name = n; _ } when String.equal n cte_name ->
    Plan.Op_pragma_rows { rows }
  | Plan.Op_filter r -> Plan.Op_filter { r with child = go r.child }
  | Plan.Op_project r -> Plan.Op_project { r with child = go r.child }
  | Plan.Op_expr_project r -> Plan.Op_expr_project { r with child = go r.child }
  | Plan.Op_sort r -> Plan.Op_sort { r with child = go r.child }
  | Plan.Op_limit r -> Plan.Op_limit { r with child = go r.child }
  | Plan.Op_distinct r -> Plan.Op_distinct { child = go r.child }
  | Plan.Op_aggregate r -> Plan.Op_aggregate { r with child = go r.child }
  | Plan.Op_nested_loop_join r -> Plan.Op_nested_loop_join { r with left = go r.left }
  | Plan.Op_hash_join r ->
    Plan.Op_hash_join { r with left = go r.left; right = go r.right }
  | Plan.Op_union r -> Plan.Op_union { r with left = go r.left; right = go r.right }
  | Plan.Op_intersect r -> Plan.Op_intersect { left = go r.left; right = go r.right }
  | Plan.Op_except r -> Plan.Op_except { left = go r.left; right = go r.right }
  | Plan.Op_with_cte r when not (String.equal r.cte_name cte_name) ->
    Plan.Op_with_cte { r with query = go r.query }
  | Plan.Op_window r -> Plan.Op_window { r with child = go r.child }
  | Plan.Op_insert_select ({ source; _ } as r) ->
    Plan.Op_insert_select { r with source = go source }
  | _ -> op
;;

(* BM25-score FTS [matches] against [query] when rank is requested; otherwise
   tag each with score 0.0.  Each term's own per-doc term-frequency is used.
   Standalone (not in the [to_stream] rec group) so it stays polymorphic in the
   txn kind — #262 calls it with either a borrowed RW txn or a fresh RO snap. *)
let fts_score_matches tx (fts_meta : Cat.fts_table_meta) query matches include_rank =
  if not include_rank
  then Lwt.return (List.map (fun (rowid, positions) -> rowid, positions, 0.0) matches)
  else
    let* total_docs, total_tokens = read_fts_stats tx fts_meta.Cat.fts_index_tree in
    let query_terms = fts_query_terms query in
    let* term_data =
      Lwt_list.map_s
        (fun term ->
           let* pl = fts_posting_list tx ~index_tree:fts_meta.Cat.fts_index_tree term in
           Lwt.return (List.length pl, pl))
        query_terms
    in
    let* doc_lengths =
      Lwt_list.map_s
        (fun (rowid, positions) ->
           let dlen_key = fts_doclen_key rowid in
           let* v = S.get tx fts_meta.Cat.fts_index_tree dlen_key in
           let dl =
             match v with
             | None -> 1
             | Some b ->
               let n, _ = Varint.decode_uint64 b 0 in
               Int64.to_int n
           in
           Lwt.return (rowid, positions, dl))
        matches
    in
    let scored =
      List.map
        (fun (rowid, positions, dl) ->
           let score =
             List.fold_left
               (fun acc (n_docs, term_pl) ->
                  let tf =
                    match List.assoc_opt rowid term_pl with
                    | None -> 0
                    | Some pos -> List.length pos
                  in
                  acc
                  +. bm25_score
                       ~k1:1.2
                       ~b:0.75
                       ~total_docs
                       ~total_tokens
                       ~n_docs_with_term:n_docs
                       ~term_freq:tf
                       ~doc_length:dl)
               0.0
               term_data
           in
           rowid, positions, score)
        doc_lengths
    in
    Lwt.return scored
;;

let rec pre_eval_subquery
          (clock : (unit -> float) option)
          (store : S.t)
          (params : Row.value array)
          (cat_opt : Cat.t option)
          (e : Plan.expr)
  : Plan.expr Lwt.t
  =
  match e with
  | Plan.P_subquery inner_ast ->
    eval_scalar_subquery clock store params cat_opt e inner_ast
  | Plan.P_exists inner_ast -> eval_exists_subquery clock store params cat_opt e inner_ast
  | Plan.P_in_select (x, inner_ast) ->
    eval_in_select clock store params cat_opt e x inner_ast
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
    let* x' = pre_eval_subquery clock store params cat_opt x in
    let* lo' = pre_eval_subquery clock store params cat_opt lo in
    let* hi' = pre_eval_subquery clock store params cat_opt hi in
    Lwt.return (Plan.P_between (x', lo', hi'))
  | Plan.P_in (x, vals) ->
    let* x' = pre_eval_subquery clock store params cat_opt x in
    let* vals' = Lwt_list.map_s (pre_eval_subquery clock store params cat_opt) vals in
    Lwt.return (Plan.P_in (x', vals'))
  | Plan.P_func (f, args) ->
    let* args' = Lwt_list.map_s (pre_eval_subquery clock store params cat_opt) args in
    Lwt.return (Plan.P_func (f, args'))
  | Plan.P_case { scrutinee; branches; else_ } ->
    let* scrutinee' =
      match scrutinee with
      | None -> Lwt.return None
      | Some e ->
        let+ e' = pre_eval_subquery clock store params cat_opt e in
        Some e'
    in
    let* branches' =
      Lwt_list.map_s
        (fun (cond, res) ->
           let* cond' = pre_eval_subquery clock store params cat_opt cond in
           let+ res' = pre_eval_subquery clock store params cat_opt res in
           cond', res')
        branches
    in
    let+ else_' =
      match else_ with
      | None -> Lwt.return None
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

(* Scalar subquery: run [inner_ast], yield its first column's first value as a
   literal (NULL if empty); returns [e] unchanged if it fails to bind. *)
and eval_scalar_subquery clock store params cat_opt (e : Plan.expr) inner_ast
  : Plan.expr Lwt.t
  =
  match cat_opt with
  | None -> Lwt.return (Plan.P_lit Ast.L_null)
  | Some cat ->
    let* bound_r = Sema.bind cat inner_ast in
    (match bound_r with
     | Error _ -> Lwt.return e
     | Ok bound ->
       let op = Planner.plan ~cat bound in
       (* #262: run the subquery under the active txn (read-your-own-writes). *)
       let* stream =
         to_stream clock params store ~mode:(current_txn_mode ()) ~cat:(Some cat) op
       in
       let* rows = Lwt_stream.to_list stream in
       let v =
         match rows with
         | [] -> Ast.L_null
         | row :: _ when Array.length row >= 1 -> value_to_literal row.(0)
         | _ -> Ast.L_null
       in
       Lwt.return (Plan.P_lit v))

(* EXISTS subquery: 1 if [inner_ast] yields any row, else 0. *)
and eval_exists_subquery clock store params cat_opt (e : Plan.expr) inner_ast
  : Plan.expr Lwt.t
  =
  match cat_opt with
  | None -> Lwt.return (Plan.P_lit (Ast.L_int 0L))
  | Some cat ->
    let* bound_r = Sema.bind cat inner_ast in
    (match bound_r with
     | Error _ -> Lwt.return e
     | Ok bound ->
       let op = Planner.plan ~cat bound in
       (* #262: run the subquery under the active txn (read-your-own-writes). *)
       let* stream =
         to_stream clock params store ~mode:(current_txn_mode ()) ~cat:(Some cat) op
       in
       let* first = Lwt_stream.get stream in
       Lwt.return (Plan.P_lit (Ast.L_int (if first = None then 0L else 1L))))

(* IN (subquery): materialize [inner_ast]'s first column into the IN value list. *)
and eval_in_select clock store params cat_opt (e : Plan.expr) x inner_ast
  : Plan.expr Lwt.t
  =
  match cat_opt with
  | None -> Lwt.return (Plan.P_in (x, []))
  | Some cat ->
    let* bound_r = Sema.bind cat inner_ast in
    (match bound_r with
     | Error _ -> Lwt.return e
     | Ok bound ->
       let op = Planner.plan ~cat bound in
       (* #262: run the subquery under the active txn (read-your-own-writes). *)
       let* stream =
         to_stream clock params store ~mode:(current_txn_mode ()) ~cat:(Some cat) op
       in
       let* rows = Lwt_stream.to_list stream in
       let vals =
         List.filter_map
           (fun row ->
              if Array.length row >= 1
              then Some (Plan.P_lit (value_to_literal row.(0)))
              else None)
           rows
       in
       let* x' = pre_eval_subquery clock store params cat_opt x in
       Lwt.return (Plan.P_in (x', vals)))

(* ------------------------------------------------------------------ *)
(* Window function helpers                                              *)
(* ------------------------------------------------------------------ *)

and eval_partition_key clock params (row : Row.t) (partition_by : Plan.expr list)
  : Row.value list
  =
  List.map (eval_expr clock params row) partition_by

and partition_keys_equal (a : Row.value list) (b : Row.value list) : bool =
  List.length a = List.length b && List.for_all2 (fun x y -> compare_values x y = 0) a b

and group_by_partition
      clock
      params
      (partition_by : Plan.expr list)
      (indexed_rows : (int * Row.t) list)
  : (Row.value list * (int * Row.t) list) list
  =
  List.fold_left
    (fun acc (idx, row) ->
       let key = eval_partition_key clock params row partition_by in
       match List.find_opt (fun (k, _) -> partition_keys_equal k key) acc with
       | Some _ ->
         List.map
           (fun (k, pairs) ->
              if partition_keys_equal k key then k, pairs @ [ idx, row ] else k, pairs)
           acc
       | None -> acc @ [ key, [ idx, row ] ])
    []
    indexed_rows

and sort_partition_by
      clock
      params
      (order_by : (Plan.expr * [ `Asc | `Desc ] * [ `Nulls_first | `Nulls_last ]) list)
      (indexed_rows : (int * Row.t) list)
  : (int * Row.t) list
  =
  if order_by = []
  then indexed_rows
  else
    List.sort
      (fun (_, ra) (_, rb) ->
         let rec cmp = function
           | [] -> 0
           | (e, dir, nulls) :: rest ->
             let va = eval_expr clock params ra e in
             let vb = eval_expr clock params rb e in
             let c = compare_with_nulls dir nulls va vb in
             if c <> 0 then c else cmp rest
         in
         cmp order_by)
      indexed_rows

and win_rank
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  let cur_rank = ref 1 in
  for pos = 0 to n - 1 do
    if pos > 0
    then (
      let order_changed =
        List.exists
          (fun (e, dir, nulls) ->
             compare_with_nulls
               dir
               nulls
               (eval_expr clock params sorted_rows.(pos) e)
               (eval_expr clock params sorted_rows.(pos - 1) e)
             <> 0)
          wplan.Plan.order_by
      in
      if order_changed then cur_rank := pos + 1);
    results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int !cur_rank)
  done

and win_dense_rank
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  let cur_rank = ref 1 in
  for pos = 0 to n - 1 do
    if pos > 0
    then (
      let order_changed =
        List.exists
          (fun (e, dir, nulls) ->
             compare_with_nulls
               dir
               nulls
               (eval_expr clock params sorted_rows.(pos) e)
               (eval_expr clock params sorted_rows.(pos - 1) e)
             <> 0)
          wplan.Plan.order_by
      in
      if order_changed then incr cur_rank);
    results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int !cur_rank)
  done

and win_ntile
      clock
      params
      (wplan : Plan.window_plan_item)
      _sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  let n_buckets =
    match wplan.Plan.args with
    | [ e ] ->
      (match eval_expr clock params [||] e with
       | Row.V_int k -> Int64.to_int k
       | _ -> 1)
    | _ -> 1
  in
  let n_buckets = max 1 n_buckets in
  for pos = 0 to n - 1 do
    let bucket = (pos * n_buckets / n) + 1 in
    results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int bucket)
  done

and win_lag_lead
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  let is_lag = wplan.Plan.func = Ast.WF_lag in
  let offset =
    match wplan.Plan.args with
    | _ :: e :: _ ->
      (match eval_expr clock params [||] e with
       | Row.V_int k -> Int64.to_int k
       | _ -> 1)
    | _ -> 1
  in
  let default_expr =
    match wplan.Plan.args with
    | _ :: _ :: e :: _ -> Some e
    | _ -> None
  in
  for pos = 0 to n - 1 do
    let src_pos = if is_lag then pos - offset else pos + offset in
    let v =
      if src_pos >= 0 && src_pos < n
      then (
        match wplan.Plan.args with
        | e :: _ -> eval_expr clock params sorted_rows.(src_pos) e
        | [] -> Row.V_null)
      else (
        match default_expr with
        | Some e -> eval_expr clock params sorted_rows.(pos) e
        | None -> Row.V_null)
    in
    results.(sorted_orig_idxs.(pos)) <- v
  done

and win_first_value
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  let arg_expr =
    match wplan.Plan.args with
    | e :: _ -> e
    | [] -> failwith "FIRST_VALUE requires one argument"
  in
  let first_val =
    if n > 0 then eval_expr clock params sorted_rows.(0) arg_expr else Row.V_null
  in
  for pos = 0 to n - 1 do
    results.(sorted_orig_idxs.(pos)) <- first_val
  done

and win_nth_value
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  let arg_expr =
    match wplan.Plan.args with
    | e :: _ -> e
    | [] -> failwith "NTH_VALUE requires at least one argument"
  in
  let n_arg =
    match wplan.Plan.args with
    | _ :: e :: _ ->
      (match eval_expr clock params [||] e with
       | Row.V_int k -> Int64.to_int k
       | _ -> 1)
    | _ -> 1
  in
  for pos = 0 to n - 1 do
    let v =
      if n_arg >= 1 && n_arg <= pos + 1
      then eval_expr clock params sorted_rows.(n_arg - 1) arg_expr
      else Row.V_null
    in
    results.(sorted_orig_idxs.(pos)) <- v
  done

and win_percent_rank
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  (* PERCENT_RANK = peer_group_start / (n - 1).  Positional adjacency in the
     already-direction-sorted array, so DESC works without knowing direction. *)
  if n = 0
  then ()
  else (
    let peer_start = ref 0 in
    for pos = 0 to n - 1 do
      if pos > 0
      then (
        let order_changed =
          List.exists
            (fun (e, dir, nulls) ->
               compare_with_nulls
                 dir
                 nulls
                 (eval_expr clock params sorted_rows.(pos) e)
                 (eval_expr clock params sorted_rows.(pos - 1) e)
               <> 0)
            wplan.Plan.order_by
        in
        if order_changed then peer_start := pos);
      let pct =
        if n <= 1 then 0.0 else Float.of_int !peer_start /. Float.of_int (n - 1)
      in
      results.(sorted_orig_idxs.(pos)) <- Row.V_real pct
    done)

and win_cume_dist
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  (* CUME_DIST = (last position in peer group + 1) / n.  Positional adjacency
     in the already-direction-sorted array, so DESC works correctly. *)
  if n = 0
  then ()
  else (
    let pos = ref 0 in
    while !pos < n do
      let peer_end = ref !pos in
      while
        !peer_end + 1 < n
        && List.for_all
             (fun (e, dir, nulls) ->
                compare_with_nulls
                  dir
                  nulls
                  (eval_expr clock params sorted_rows.(!peer_end + 1) e)
                  (eval_expr clock params sorted_rows.(!peer_end) e)
                = 0)
             wplan.Plan.order_by
      do
        incr peer_end
      done;
      let cd = Float.of_int (!peer_end + 1) /. Float.of_int n in
      for i = !pos to !peer_end do
        results.(sorted_orig_idxs.(i)) <- Row.V_real cd
      done;
      pos := !peer_end + 1
    done)

(* Compute one aggregate-window value over the rows in [indices]. *)
and win_agg_over_frame
      agg_func
      (arg_expr : Plan.expr option)
      (arg_vals : Row.value array)
      indices
  : Row.value
  =
  match agg_func with
  | Ast.Agg_count ->
    let cnt =
      if arg_expr = None
      then List.length indices
      else List.length (List.filter (fun i -> not (arg_vals.(i) = Row.V_null)) indices)
    in
    Row.V_int (Int64.of_int cnt)
  | Ast.Agg_sum ->
    List.fold_left
      (fun acc i ->
         match acc, arg_vals.(i) with
         | _, Row.V_null -> acc
         | Row.V_null, v -> v
         | Row.V_int a, Row.V_int b -> Row.V_int (Int64.add a b)
         | Row.V_real a, Row.V_real b -> Row.V_real (a +. b)
         | Row.V_int a, Row.V_real b -> Row.V_real (Int64.to_float a +. b)
         | Row.V_real a, Row.V_int b -> Row.V_real (a +. Int64.to_float b)
         | _, _ -> acc)
      Row.V_null
      indices
  | Ast.Agg_avg ->
    let vals =
      List.filter_map
        (fun i ->
           match arg_vals.(i) with
           | Row.V_int n -> Some (Int64.to_float n)
           | Row.V_real f -> Some f
           | _ -> None)
        indices
    in
    if vals = []
    then Row.V_null
    else Row.V_real (List.fold_left ( +. ) 0.0 vals /. float_of_int (List.length vals))
  | Ast.Agg_min ->
    List.fold_left
      (fun acc i ->
         match arg_vals.(i) with
         | Row.V_null -> acc
         | v ->
           (match acc with
            | Row.V_null -> v
            | acc_v -> if compare_values v acc_v < 0 then v else acc_v))
      Row.V_null
      indices
  | Ast.Agg_max ->
    List.fold_left
      (fun acc i ->
         match arg_vals.(i) with
         | Row.V_null -> acc
         | v ->
           (match acc with
            | Row.V_null -> v
            | acc_v -> if compare_values v acc_v > 0 then v else acc_v))
      Row.V_null
      indices
  | Ast.Agg_group_concat sep ->
    let separator = Option.value sep ~default:"," in
    let parts =
      List.filter_map
        (fun i ->
           match arg_vals.(i) with
           | Row.V_null -> None
           | Row.V_int n -> Some (Int64.to_string n)
           | Row.V_real f -> Some (Printf.sprintf "%.17g" f)
           | Row.V_text s -> Some s
           | Row.V_blob _ -> Some "")
        indices
    in
    if parts = [] then Row.V_null else Row.V_text (String.concat separator parts)

and win_aggregate
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
      agg_func
  =
  let has_order = wplan.Plan.order_by <> [] in
  let arg_expr =
    match wplan.Plan.args with
    | e :: _ -> Some e
    | [] -> None
  in
  let arg_vals =
    Array.init n (fun pos ->
      match arg_expr with
      | Some e -> eval_expr clock params sorted_rows.(pos) e
      | None -> Row.V_null)
  in
  let resolve_bound bound pos =
    match bound with
    | Ast.FB_unbounded_preceding -> 0
    | Ast.FB_preceding k -> max 0 (pos - k)
    | Ast.FB_current_row -> pos
    | Ast.FB_following k -> min (n - 1) (pos + k)
    | Ast.FB_unbounded_following -> n - 1
  in
  for pos = 0 to n - 1 do
    let frame_start, frame_end =
      match wplan.Plan.frame with
      | None ->
        (* Default: UNBOUNDED PRECEDING AND CURRENT ROW with ORDER BY, else
           UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING. *)
        let fe = if has_order then pos else n - 1 in
        0, fe
      | Some spec ->
        (* RANGE numeric bounds approximated as ROWS — full value-based RANGE
           semantics not implemented. *)
        resolve_bound spec.Ast.start pos, resolve_bound spec.Ast.end_ pos
    in
    let frame_start = max 0 frame_start in
    let frame_end = min (n - 1) frame_end in
    let indices =
      if frame_start > frame_end
      then []
      else List.init (frame_end - frame_start + 1) (fun i -> frame_start + i)
    in
    results.(sorted_orig_idxs.(pos))
    <- win_agg_over_frame agg_func arg_expr arg_vals indices
  done

and compute_window_for_partition
      clock
      params
      (wplan : Plan.window_plan_item)
      (sorted_indexed : (int * Row.t) list)
      (n_total : int)
  : Row.value array
  =
  let results = Array.make n_total Row.V_null in
  let sorted_rows = Array.of_list (List.map snd sorted_indexed) in
  let sorted_orig_idxs = Array.of_list (List.map fst sorted_indexed) in
  let n = Array.length sorted_rows in
  (match wplan.Plan.func with
   | Ast.WF_row_number ->
     for pos = 0 to n - 1 do
       results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int (pos + 1))
     done
   | Ast.WF_rank -> win_rank clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_dense_rank ->
     win_dense_rank clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_ntile -> win_ntile clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_lag | Ast.WF_lead ->
     win_lag_lead clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_first_value ->
     win_first_value clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_last_value ->
     let arg_expr =
       match wplan.Plan.args with
       | e :: _ -> e
       | [] -> failwith "LAST_VALUE requires one argument"
     in
     for pos = 0 to n - 1 do
       results.(sorted_orig_idxs.(pos))
       <- eval_expr clock params sorted_rows.(pos) arg_expr
     done
   | Ast.WF_nth_value ->
     win_nth_value clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_percent_rank ->
     win_percent_rank clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_cume_dist ->
     win_cume_dist clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_agg agg_func ->
     win_aggregate clock params wplan sorted_rows sorted_orig_idxs results n agg_func);
  results

and stream_seq_scan clock params store mode (table_meta : Cat.table_meta) =
  (* #239: captured at construction (inside [query]'s [with_value] scope). *)
  let s_opt = Lwt.get query_stats_key in
  (* #262: read through the active txn when one is open, so the scan observes
     the transaction's own uncommitted writes. *)
  let* rh = rh_begin store mode in
  (* #238: stream the leaves natively via [seek_ge ""] instead of
     [cursor_open], which drains the WHOLE tree into an OCaml list at open time
     (every (key,value) pair held simultaneously) before the first row is read.
     That eager drain — not value boxing — was the bulk of the scan pipeline's
     per-row allocation (~9.2 KB/row, vs ~1.5 KB at the streaming storage floor
     and ~0.6 KB for the row decode itself).  [seek_ge ""] descends in O(log n)
     and materialises only the entries actually pulled.  [Bytes.empty] is the
     minimum key, so the first [seek_next] returns the first row — matching the
     old [cursor_first]+[cursor_next] semantics. *)
  let* cur = rh_seek_ge rh table_meta.tree_id Bytes.empty in
  (* Snapshot lifetime tied to the stream: end on exhaustion OR a mid-scan read
     error so a corrupt page can't leak locks/refcounts/pins (#164). Idempotent. *)
  let ended = ref false in
  let finish () =
    if !ended
    then Lwt.return_unit
    else (
      ended := true;
      S.seek_close cur;
      rh_finish rh)
  in
  let stream =
    Lwt_stream.from (fun () ->
      Lwt.catch
        (fun () ->
           let* kv = S.seek_next cur in
           match kv with
           | None ->
             let%lwt () = finish () in
             Lwt.return_none
           | Some (_key, vbytes) ->
             incr_examined s_opt;
             let row = decode_with_virtual clock params table_meta vbytes in
             Lwt.return_some row)
        (fun exn ->
           let%lwt () = finish () in
           Lwt.fail exn))
  in
  Lwt.return stream

and stream_filter clock params store mode cat pred child =
  (* #257: a correlated subquery in the predicate is re-evaluated per row at
     pull time — outside [query]'s [with_value] scope — so capture the active
     stats here and re-establish the scope around the inner evaluation, letting
     the subquery's leaf scanners attribute their reads to this query. *)
  let s_opt = Lwt.get query_stats_key in
  let* child_stream = to_stream clock params store ~mode ~cat child in
  let* pred' = pre_eval_subquery clock store params cat pred in
  if not (plan_expr_has_subquery pred')
  then
    Lwt.return
      (Lwt_stream.filter
         (fun row -> value_truthy (eval_expr clock params row pred'))
         child_stream)
  else (
    let outer_meta = get_outer_scan_meta child in
    match outer_meta with
    | None -> Lwt.return (Lwt_stream.filter (fun _row -> false) child_stream)
    | Some meta ->
      Lwt.return
        (Lwt_stream.filter_s
           (fun row ->
              let subst_pred = substitute_outer_in_plan_expr meta row pred' in
              let* resolved =
                with_pull_context ~stats:s_opt ~mode (fun () ->
                  pre_eval_subquery clock store params cat subst_pred)
              in
              Lwt.return (value_truthy (eval_expr clock params row resolved)))
           child_stream))

and stream_expr_project clock params store mode cat exprs child =
  (* #257: as in [stream_filter], re-establish the stats scope around per-row
     correlated-subquery evaluation in a projected expression. *)
  let s_opt = Lwt.get query_stats_key in
  let* inner = to_stream clock params store ~mode ~cat child in
  let* exprs' =
    Lwt_list.map_s (fun (e, _alias) -> pre_eval_subquery clock store params cat e) exprs
  in
  let has_corr = List.exists plan_expr_has_subquery exprs' in
  if not has_corr
  then (
    let eval_exprs row = Array.of_list (List.map (eval_expr clock params row) exprs') in
    Lwt.return (Lwt_stream.map eval_exprs inner))
  else (
    let outer_meta = get_outer_scan_meta child in
    match outer_meta with
    | None ->
      let eval_exprs row = Array.of_list (List.map (eval_expr clock params row) exprs') in
      Lwt.return (Lwt_stream.map eval_exprs inner)
    | Some meta ->
      Lwt.return
        (Lwt_stream.map_s
           (fun row ->
              let* vals =
                Lwt_list.map_s
                  (fun e ->
                     let e_subst = substitute_outer_in_plan_expr meta row e in
                     let* resolved =
                       with_pull_context ~stats:s_opt ~mode (fun () ->
                         pre_eval_subquery clock store params cat e_subst)
                     in
                     Lwt.return (eval_expr clock params row resolved))
                  exprs'
              in
              Lwt.return (Array.of_list vals))
           inner))

and stream_sort clock params store mode cat keys child =
  let* inner = to_stream clock params store ~mode ~cat child in
  let* rows = Lwt_stream.to_list inner in
  let* keys' =
    Lwt_list.map_s
      (fun (e, dir, nulls) ->
         let* e' = pre_eval_subquery clock store params cat e in
         Lwt.return (e', dir, nulls))
      keys
  in
  let cmp a b =
    List.fold_left
      (fun acc (key, dir, nulls) ->
         if acc <> 0
         then acc
         else (
           let va = eval_expr clock params a key
           and vb = eval_expr clock params b key in
           compare_with_nulls dir nulls va vb))
      0
      keys'
  in
  Lwt.return (Lwt_stream.of_list (List.sort cmp rows))

and stream_index_lookup
      clock
      params
      store
      mode
      table_tree
      idx_tree
      col_type
      lookup_val
      (table_meta : Cat.table_meta)
  =
  let s_opt = Lwt.get query_stats_key in
  let v = eval_expr clock params [||] lookup_val in
  (* [WHERE col = NULL] never matches (SQL three-valued logic).  A bound
     parameter may be NULL at run time (#228: [col = ?] is now index-eligible);
     return no rows rather than seeking the index's NULL entries. *)
  match v with
  | Row.V_null -> Lwt.return (Lwt_stream.of_list [])
  | _ ->
    let lookup_v =
      match v, col_type with
      | Row.V_null, _ -> Index_key.IK_null
      | Row.V_int n, Row.Integer -> Index_key.IK_int n
      | Row.V_text s, Row.Text -> Index_key.IK_text s
      | Row.V_real f, Row.Real -> Index_key.IK_real f
      | Row.V_blob b, Row.Blob -> Index_key.IK_blob b
      | _, _ -> Index_key.IK_null (* type mismatch: nothing matches *)
    in
    let prefix = Index_key.encode_value lookup_v in
    let plen = Bytes.length prefix in
    let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
    (* #262: read through the active txn so an index lookup sees rows the open
       transaction has inserted/updated but not yet committed. *)
    let* rh = rh_begin store mode in
    (* O(log n) native seek + lazy streaming of just the matching prefix range,
     instead of draining the entire index tree per lookup (#228). *)
    let* cur = rh_seek_ge rh idx_tree seek_key in
    let exhausted = ref false in
    let ended = ref false in
    let finish () =
      if !ended
      then Lwt.return_unit
      else (
        ended := true;
        S.seek_close cur;
        rh_finish rh)
    in
    let stream =
      Lwt_stream.from (fun () ->
        if !exhausted
        then Lwt.return_none
        else
          Lwt.catch
            (fun () ->
               let rec next () =
                 match%lwt S.seek_next cur with
                 | None ->
                   exhausted := true;
                   let%lwt () = finish () in
                   Lwt.return_none
                 | Some (ikey, _ival) ->
                   if
                     Bytes.length ikey >= plen + 8
                     && Bytes.equal (Bytes.sub ikey 0 plen) prefix
                   then (
                     let rowid_bytes = Bytes.sub ikey (Bytes.length ikey - 8) 8 in
                     let rowid = Rowid.decode rowid_bytes in
                     let table_key = Rowid.encode rowid in
                     let%lwt vrow = rh_get rh table_tree table_key in
                     match vrow with
                     | None -> next ()
                     | Some vbytes ->
                       incr_examined s_opt;
                       let row = decode_with_virtual clock params table_meta vbytes in
                       Lwt.return_some row)
                   else (
                     exhausted := true;
                     let%lwt () = finish () in
                     Lwt.return_none)
               in
               next ())
            (fun exn ->
               exhausted := true;
               let%lwt () = finish () in
               Lwt.fail exn))
    in
    Lwt.return stream

(* #243 (T1): point lookup on an INTEGER PRIMARY KEY rowid alias — the column IS
   the table key, so this is a single O(log n) table-tree seek, no index and no
   second fetch.  A NULL or non-integer probe matches nothing, mirroring the
   old __pk Op_index_lookup path (which mapped a type-mismatched value to
   IK_null ⇒ empty), so behavior is unchanged. *)
and stream_rowid_lookup clock params store mode lookup_val (table_meta : Cat.table_meta) =
  let s_opt = Lwt.get query_stats_key in
  let v = eval_expr clock params [||] lookup_val in
  match v with
  | Row.V_int n ->
    (* #262: read through the active txn so a primary-key point lookup sees the
       row when it was written earlier in the same open transaction. *)
    let* rh = rh_begin store mode in
    let* vrow = rh_get rh table_meta.Cat.tree_id (Rowid.encode n) in
    let* () = rh_finish rh in
    (match vrow with
     | None -> Lwt.return (Lwt_stream.of_list [])
     | Some vbytes ->
       incr_examined s_opt;
       let row = decode_with_virtual clock params table_meta vbytes in
       Lwt.return (Lwt_stream.of_list [ row ]))
  | _ -> Lwt.return (Lwt_stream.of_list [])

(* Probe the right index for one left row [lrow], appending matched (or a
   null-padded row for LEFT JOIN) combinations to [out]. *)
and nlj_probe_left
      clock
      params
      s_opt
      rh
      (right_meta : Cat.table_meta)
      idx_tree
      left_col_idx
      join_kind
      n_right_cols
      out
      lrow
  : unit Lwt.t
  =
  let lkey = lrow.(left_col_idx) in
  if lkey = Row.V_null
  then (
    if join_kind = `Left
    then out := Array.append lrow (Array.make n_right_cols Row.V_null) :: !out;
    Lwt.return_unit)
  else (
    let ik_value = row_value_to_index_value lkey in
    let prefix = Index_key.encode_value ik_value in
    let plen = Bytes.length prefix in
    let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
    (* O(log n) native seek per probe — avoids draining the whole index per
       left row, which made indexed nested-loop joins O(n^2) (#228/#229). *)
    let* cur = rh_seek_ge rh idx_tree seek_key in
    let found = ref false in
    let rec scan () =
      match%lwt S.seek_next cur with
      | None -> Lwt.return_unit
      | Some (ikey, _) ->
        if Bytes.length ikey >= plen + 8 && Bytes.equal (Bytes.sub ikey 0 plen) prefix
        then (
          let rowid_bytes = Bytes.sub ikey (Bytes.length ikey - 8) 8 in
          let rowid = Rowid.decode rowid_bytes in
          let table_key = Rowid.encode rowid in
          let* vrow = rh_get rh right_meta.Cat.tree_id table_key in
          match vrow with
          | None -> scan ()
          | Some vbytes ->
            incr_examined s_opt;
            let rrow = decode_with_virtual clock params right_meta vbytes in
            out := Array.append lrow rrow :: !out;
            found := true;
            scan ())
        else Lwt.return_unit
    in
    let* () = scan () in
    S.seek_close cur;
    (match join_kind with
     | `Left when not !found ->
       out := Array.append lrow (Array.make n_right_cols Row.V_null) :: !out
     | _ -> ());
    Lwt.return_unit)

and stream_nested_loop_join
      clock
      params
      store
      mode
      cat
      left
      (right_meta : Cat.table_meta)
      idx_tree
      left_col_idx
      join_kind
      n_right_cols
  =
  (* #239: captured under [query]'s [with_value] scope; counts right-side index
     probes (the left input's base scan is counted via [to_stream] below). *)
  let s_opt = Lwt.get query_stats_key in
  let* left_stream = to_stream clock params store ~mode ~cat left in
  let* left_rows = Lwt_stream.to_list left_stream in
  (* #262: probe the inner index through the active txn so the join sees inner
     rows written earlier in the same open transaction. *)
  with_read store mode
  @@ fun rh ->
  let out = ref [] in
  let* () =
    Lwt_list.iter_s
      (nlj_probe_left
         clock
         params
         s_opt
         rh
         right_meta
         idx_tree
         left_col_idx
         join_kind
         n_right_cols
         out)
      left_rows
  in
  Lwt.return (Lwt_stream.of_list (List.rev !out))

(* Build a hash table mapping each right row's join key to its rows; NULL keys
   are excluded (they never match an equi-join probe). *)
and hash_build right_rows right_key : (bytes, Row.t list) Hashtbl.t =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun rrow ->
       match rrow.(right_key) with
       | Row.V_null -> ()
       | key_v ->
         let key_bytes = Index_key.encode_value (row_value_to_index_value key_v) in
         let prev =
           try Hashtbl.find tbl key_bytes with
           | Not_found -> []
         in
         Hashtbl.replace tbl key_bytes (rrow :: prev))
    right_rows;
  tbl

and stream_hash_join
      clock
      params
      store
      mode
      cat
      left
      right
      left_key
      right_key
      join_kind
      n_right_cols
  =
  let* left_stream = to_stream clock params store ~mode ~cat left in
  let* right_stream = to_stream clock params store ~mode ~cat right in
  let* right_rows = Lwt_stream.to_list right_stream in
  if left_key < 0 || right_key < 0
  then (
    (* Cartesian product fallback (general ON predicate). *)
    let* left_rows = Lwt_stream.to_list left_stream in
    let out = ref [] in
    List.iter
      (fun lrow ->
         let any = ref false in
         List.iter
           (fun rrow ->
              out := Array.append lrow rrow :: !out;
              any := true)
           right_rows;
         match join_kind with
         | `Left when not !any ->
           let null_right = Array.make n_right_cols Row.V_null in
           out := Array.append lrow null_right :: !out
         | _ -> ())
      left_rows;
    Lwt.return (Lwt_stream.of_list (List.rev !out)))
  else (
    let tbl = hash_build right_rows right_key in
    let* left_rows = Lwt_stream.to_list left_stream in
    let out = ref [] in
    List.iter
      (fun lrow ->
         let key_v = lrow.(left_key) in
         let any = ref false in
         (match key_v with
          | Row.V_null -> ()
          | _ ->
            let key_bytes = Index_key.encode_value (row_value_to_index_value key_v) in
            (match Hashtbl.find_opt tbl key_bytes with
             | None -> ()
             | Some rrows ->
               List.iter
                 (fun rrow ->
                    out := Array.append lrow rrow :: !out;
                    any := true)
                 (List.rev rrows)));
         match join_kind with
         | `Left when not !any ->
           let null_right = Array.make n_right_cols Row.V_null in
           out := Array.append lrow null_right :: !out
         | _ -> ())
      left_rows;
    Lwt.return (Lwt_stream.of_list (List.rev !out)))

(* SUM over a group's column [i]: preserve INT vs REAL like SQLite-lite. *)
and agg_sum group_rows i : Row.value =
  let any_real =
    List.exists
      (fun r ->
         match r.(i) with
         | Row.V_real _ -> true
         | _ -> false)
      group_rows
  in
  let any_non_null =
    List.exists
      (fun r ->
         match r.(i) with
         | Row.V_null -> false
         | _ -> true)
      group_rows
  in
  if not any_non_null
  then Row.V_null
  else if any_real
  then (
    let s =
      List.fold_left
        (fun acc r ->
           match r.(i) with
           | Row.V_null -> acc
           | Row.V_int n -> acc +. Int64.to_float n
           | Row.V_real f -> acc +. f
           | _ -> failwith "SUM on non-numeric value")
        0.0
        group_rows
    in
    Row.V_real s)
  else (
    let s =
      List.fold_left
        (fun acc r ->
           match r.(i) with
           | Row.V_null -> acc
           | Row.V_int n -> Int64.add acc n
           | _ -> failwith "SUM on non-numeric value")
        0L
        group_rows
    in
    Row.V_int s)

(* Evaluate one aggregate [spec] over the rows of a group. *)
and aggregate_one (spec : Plan.agg_spec) (group_rows : Row.t list) : Row.value =
  match spec.func, spec.col_ord with
  | Ast.Agg_count, None -> Row.V_int (Int64.of_int (List.length group_rows))
  | Ast.Agg_count, Some i ->
    let n =
      List.fold_left
        (fun acc r ->
           match r.(i) with
           | Row.V_null -> acc
           | _ -> acc + 1)
        0
        group_rows
    in
    Row.V_int (Int64.of_int n)
  | Ast.Agg_sum, Some i -> agg_sum group_rows i
  | Ast.Agg_avg, Some i ->
    let sum, n =
      List.fold_left
        (fun (s, n) r ->
           match r.(i) with
           | Row.V_null -> s, n
           | Row.V_int x -> s +. Int64.to_float x, n + 1
           | Row.V_real f -> s +. f, n + 1
           | _ -> failwith "AVG on non-numeric value")
        (0.0, 0)
        group_rows
    in
    if n = 0 then Row.V_null else Row.V_real (sum /. float_of_int n)
  | Ast.Agg_min, Some i ->
    List.fold_left
      (fun acc r ->
         match r.(i), acc with
         | Row.V_null, _ -> acc
         | v, Row.V_null -> v
         | v, cur -> if compare_values v cur < 0 then v else cur)
      Row.V_null
      group_rows
  | Ast.Agg_max, Some i ->
    List.fold_left
      (fun acc r ->
         match r.(i), acc with
         | Row.V_null, _ -> acc
         | v, Row.V_null -> v
         | v, cur -> if compare_values v cur > 0 then v else cur)
      Row.V_null
      group_rows
  | Ast.Agg_group_concat sep, Some i ->
    let separator = Option.value sep ~default:"," in
    let parts =
      List.filter_map
        (fun r ->
           match r.(i) with
           | Row.V_null -> None
           | Row.V_int n -> Some (Int64.to_string n)
           | Row.V_real f -> Some (Printf.sprintf "%.17g" f)
           | Row.V_text s -> Some s
           | Row.V_blob _ -> Some "")
        group_rows
    in
    if parts = [] then Row.V_null else Row.V_text (String.concat separator parts)
  | Ast.Agg_group_concat _, None -> failwith "GROUP_CONCAT requires a column argument"
  | (Ast.Agg_sum | Ast.Agg_avg | Ast.Agg_min | Ast.Agg_max), None ->
    failwith "non-COUNT aggregate must have a column argument"

(* Partition [rows] into (group_key, group_rows) by [group_cols] (stable). *)
and aggregate_build_groups group_cols rows : (Row.value list * Row.t list) list =
  let group_keys_of_row row = List.map (fun i -> row.(i)) group_cols in
  let compare_group_keys ka kb =
    List.fold_left2 (fun acc a b -> if acc <> 0 then acc else compare_values a b) 0 ka kb
  in
  if group_cols = []
  then [ [], rows ]
  else (
    let sorted =
      List.stable_sort
        (fun a b -> compare_group_keys (group_keys_of_row a) (group_keys_of_row b))
        rows
    in
    let rec group_runs acc cur_key cur_rows = function
      | [] ->
        (match cur_rows with
         | [] -> List.rev acc
         | _ -> List.rev ((cur_key, List.rev cur_rows) :: acc))
      | r :: rest ->
        let k = group_keys_of_row r in
        if cur_rows <> [] && compare_group_keys k cur_key = 0
        then group_runs acc cur_key (r :: cur_rows) rest
        else (
          let acc' = if cur_rows = [] then acc else (cur_key, List.rev cur_rows) :: acc in
          group_runs acc' k [ r ] rest)
    in
    group_runs [] [] [] sorted)

(* Append post-aggregate window-function columns to [after_having] rows. *)
and aggregate_apply_windows clock params agg_windows after_having =
  if agg_windows = []
  then after_having
  else (
    let n_total = List.length after_having in
    let indexed = List.mapi (fun i r -> i, r) after_having in
    let window_arrays =
      List.map
        (fun (wplan : Plan.window_plan_item) ->
           let partitions =
             group_by_partition clock params wplan.Plan.partition_by indexed
           in
           let combined = Array.make n_total Row.V_null in
           List.iter
             (fun (_, partition_indexed) ->
                let sorted =
                  sort_partition_by clock params wplan.Plan.order_by partition_indexed
                in
                let part_results =
                  compute_window_for_partition clock params wplan sorted n_total
                in
                List.iter
                  (fun (orig_idx, _) -> combined.(orig_idx) <- part_results.(orig_idx))
                  sorted)
             partitions;
           combined)
        agg_windows
    in
    List.mapi
      (fun i row ->
         let extras = List.map (fun arr -> arr.(i)) window_arrays in
         Array.append row (Array.of_list extras))
      after_having)

(* #247: build an incremental accumulator for one aggregate [spec]: an
   [(update, finalize)] pair folded over scanned rows.  Returns [None] for any
   spec the fast-path doesn't handle (e.g. a non-COUNT aggregate with no column),
   which makes the caller fall back to the general [stream_aggregate] path.  The
   per-type logic here MUST stay byte-identical to [aggregate_one]/[agg_sum]. *)
and make_agg_acc (spec : Plan.agg_spec) : ((Row.t -> unit) * (unit -> Row.value)) option =
  match spec.Plan.func, spec.Plan.col_ord with
  | Ast.Agg_count, None ->
    let c = ref 0 in
    Some ((fun _ -> incr c), fun () -> Row.V_int (Int64.of_int !c))
  | Ast.Agg_count, Some i ->
    let c = ref 0 in
    Some
      ( (fun row ->
          match row.(i) with
          | Row.V_null -> ()
          | _ -> incr c)
      , fun () -> Row.V_int (Int64.of_int !c) )
  | Ast.Agg_sum, Some i ->
    (* INT vs REAL preserved exactly like [agg_sum]: REAL iff any real seen;
       NULL iff no non-null seen. *)
    let si = ref 0L
    and sf = ref 0.0
    and any_real = ref false
    and any_nn = ref false in
    Some
      ( (fun row ->
          match row.(i) with
          | Row.V_null -> ()
          | Row.V_int n ->
            any_nn := true;
            si := Int64.add !si n;
            sf := !sf +. Int64.to_float n
          | Row.V_real f ->
            any_nn := true;
            any_real := true;
            sf := !sf +. f
          | _ -> failwith "SUM on non-numeric value")
      , fun () ->
          if not !any_nn
          then Row.V_null
          else if !any_real
          then Row.V_real !sf
          else Row.V_int !si )
  | Ast.Agg_avg, Some i ->
    let sf = ref 0.0
    and n = ref 0 in
    Some
      ( (fun row ->
          match row.(i) with
          | Row.V_null -> ()
          | Row.V_int x ->
            sf := !sf +. Int64.to_float x;
            incr n
          | Row.V_real f ->
            sf := !sf +. f;
            incr n
          | _ -> failwith "AVG on non-numeric value")
      , fun () -> if !n = 0 then Row.V_null else Row.V_real (!sf /. float_of_int !n) )
  | Ast.Agg_min, Some i ->
    let best = ref Row.V_null in
    Some
      ( (fun row ->
          match row.(i), !best with
          | Row.V_null, _ -> ()
          | v, Row.V_null -> best := v
          | v, cur -> if compare_values v cur < 0 then best := v)
      , fun () -> !best )
  | Ast.Agg_max, Some i ->
    let best = ref Row.V_null in
    Some
      ( (fun row ->
          match row.(i), !best with
          | Row.V_null, _ -> ()
          | v, Row.V_null -> best := v
          | v, cur -> if compare_values v cur > 0 then best := v)
      , fun () -> !best )
  | Ast.Agg_group_concat sep, Some i ->
    let separator = Option.value sep ~default:"," in
    let parts = ref [] in
    (* newest-first; reversed at finalize to preserve scan order *)
    Some
      ( (fun row ->
          match row.(i) with
          | Row.V_null -> ()
          | Row.V_int n -> parts := Int64.to_string n :: !parts
          | Row.V_real f -> parts := Printf.sprintf "%.17g" f :: !parts
          | Row.V_text s -> parts := s :: !parts
          | Row.V_blob _ -> parts := "" :: !parts)
      , fun () ->
          match !parts with
          | [] -> Row.V_null
          | l -> Row.V_text (String.concat separator (List.rev l)) )
  | (Ast.Agg_sum | Ast.Agg_avg | Ast.Agg_min | Ast.Agg_max | Ast.Agg_group_concat _), None
    -> None

(* #247: cursor-level fast path for a no-GROUP-BY aggregate directly over a
   (optionally filtered) sequential scan.  Folds the accumulators over the scan
   cursor in a single pass, bypassing the child's per-row [Lwt_stream] layers and
   the [Lwt_stream.to_list] full-table materialisation the general path pays.
   Returns [Some stream] (always exactly one output row, matching
   [aggregate_build_groups]'s single implicit group) when applicable, else [None]
   to fall back.  Preserves the streaming/stack-bound property: the fold holds
   only the accumulators, never the rows. *)
and aggregate_fast_path
      clock
      params
      store
      mode
      cat
      child
      group_cols
      aggs
      having
      proj
      agg_windows
  : Row.t Lwt_stream.t option Lwt.t
  =
  if not (agg_fastpath_enabled ())
  then Lwt.return None
  else if group_cols <> [] || having <> None || agg_windows <> []
  then Lwt.return None
  else if
    not
      (List.for_all
         (function
           | Plan.PI_agg_slot _ -> true
           | _ -> false)
         proj)
  then Lwt.return None
  else (
    match child with
    | Plan.Op_seq_scan { table_meta } ->
      run_aggregate_fast_path clock params store mode cat table_meta None aggs proj
    | Plan.Op_filter { pred; child = Plan.Op_seq_scan { table_meta } }
      when not (plan_expr_has_subquery pred) ->
      run_aggregate_fast_path clock params store mode cat table_meta (Some pred) aggs proj
    | _ -> Lwt.return None)

and run_aggregate_fast_path clock params store mode cat table_meta pred_opt aggs proj =
  match
    let accs = List.map make_agg_acc aggs in
    if List.exists Option.is_none accs
    then None
    else Some (Array.of_list (List.map Option.get accs))
  with
  | None -> Lwt.return None
  | Some accs ->
    let* pred' =
      match pred_opt with
      | None -> Lwt.return None
      | Some p ->
        let* p' = pre_eval_subquery clock store params cat p in
        Lwt.return (Some p')
    in
    (* No decode needed when nothing reads a column and there is no filter: a
       pure COUNT-star loop runs at storage-cursor speed. *)
    let max_col =
      List.fold_left
        (fun m (s : Plan.agg_spec) ->
           match s.Plan.col_ord with
           | Some i when i > m -> i
           | _ -> m)
        (-1)
        aggs
    in
    let need_decode = pred_opt <> None || max_col >= 0 in
    (* #247: when no filter reads other columns and there are no virtual columns
       to recompute, decode only the [0, max_col] prefix — skipping trailing
       columns (e.g. a TEXT payload) the aggregate never touches. *)
    let can_prune = pred_opt = None && not (has_virtual_cols table_meta.Cat.columns) in
    let decode_row vbytes =
      if can_prune
      then Row.decode_prefix table_meta.Cat.columns vbytes ~upto:max_col
      else decode_with_virtual clock params table_meta vbytes
    in
    (* #262: fold over the active txn when one is open, so a COUNT/SUM reflects
       rows written earlier in the same uncommitted transaction. *)
    let* rh = rh_begin store mode in
    let* cur = rh_seek_ge rh table_meta.Cat.tree_id Bytes.empty in
    let ended = ref false in
    let finish () =
      if !ended
      then Lwt.return_unit
      else (
        ended := true;
        S.seek_close cur;
        rh_finish rh)
    in
    let dummy = [||] in
    let s_opt = Lwt.get query_stats_key in
    Lwt.catch
      (fun () ->
         let rec loop () =
           let* kv = S.seek_next cur in
           match kv with
           | None ->
             let* () = finish () in
             let agg_vals = Array.map (fun (_, fin) -> fin ()) accs in
             let out =
               Array.of_list
                 (List.map
                    (function
                      | Plan.PI_agg_slot k -> agg_vals.(k)
                      | Plan.PI_group_col _ | Plan.PI_window_slot _ ->
                        assert false (* excluded above *))
                    proj)
             in
             Lwt.return (Some (Lwt_stream.of_list [ out ]))
           | Some (_key, vbytes) ->
             incr_examined s_opt;
             if need_decode
             then (
               let row = decode_row vbytes in
               let keep =
                 match pred' with
                 | None -> true
                 | Some p -> value_truthy (eval_expr clock params row p)
               in
               if keep then Array.iter (fun (upd, _) -> upd row) accs)
             else Array.iter (fun (upd, _) -> upd dummy) accs;
             loop ()
         in
         loop ())
      (fun exn ->
         let* () = finish () in
         Lwt.fail exn)

and stream_aggregate
      clock
      params
      store
      mode
      cat
      child
      group_cols
      aggs
      having
      proj
      agg_windows
  =
  let* fast =
    aggregate_fast_path
      clock
      params
      store
      mode
      cat
      child
      group_cols
      aggs
      having
      proj
      agg_windows
  in
  match fast with
  | Some stream -> Lwt.return stream
  | None ->
    let* inner = to_stream clock params store ~mode ~cat child in
    let* rows = Lwt_stream.to_list inner in
    let n_group_cols = List.length group_cols in
    let groups = aggregate_build_groups group_cols rows in
    let agg_output_rows =
      List.map
        (fun (group_key, group_rows) ->
           let agg_vals = List.map (fun spec -> aggregate_one spec group_rows) aggs in
           Array.of_list (group_key @ agg_vals))
        groups
    in
    let after_having =
      match having with
      | None -> agg_output_rows
      | Some pred ->
        List.filter
          (fun r -> value_truthy (eval_expr clock params r pred))
          agg_output_rows
    in
    let n_agg_cols = n_group_cols + List.length aggs in
    let with_windows = aggregate_apply_windows clock params agg_windows after_having in
    let final_rows =
      List.map
        (fun agg_row ->
           Array.of_list
             (List.map
                (function
                  | Plan.PI_group_col i -> agg_row.(i)
                  | Plan.PI_agg_slot k -> agg_row.(n_group_cols + k)
                  | Plan.PI_window_slot j -> agg_row.(n_agg_cols + j))
                proj))
        with_windows
    in
    Lwt.return (Lwt_stream.of_list final_rows)

and read_fts_content_rows store mode (fts_meta : Cat.fts_table_meta)
  : (int64 * string list) list Lwt.t
  =
  (* #330: read the FTS content tree as (rowid, column-texts) pairs through [mode]
     (the same shared snapshot / explicit txn the dump uses for table rows), so
     [Db.dump] can emit INSERTs that carry the original rowids and round-trip the
     index exactly.  The content tree is keyed by rowid, so this is the only place
     FTS rowids are surfaced — deliberately out-of-band, not via a SQL projection
     (see #330). *)
  with_read store mode (fun rh ->
    let* cur = rh_cursor_open rh fts_meta.Cat.fts_content_tree in
    let _sr = S.cursor_first cur in
    let acc = ref [] in
    let rec walk () =
      match S.cursor_next cur with
      | None -> ()
      | Some (k, v) ->
        acc := (Rowid.decode k, fts_decode_content v) :: !acc;
        walk ()
    in
    walk ();
    S.cursor_close cur;
    Lwt.return (List.rev !acc))

and stream_fts_seq_scan clock params store mode (fts_meta : Cat.fts_table_meta) where =
  (* #257: captured at construction (inside [query]'s [with_value] scope), same
     pattern as the table scanners; every content row scanned counts as examined
     regardless of the WHERE filter. *)
  let s_opt = Lwt.get query_stats_key in
  (* #262: scan the content tree through the active txn so an in-transaction
     write to the FTS table is visible to the scan. *)
  let* rh = rh_begin store mode in
  let* cur = rh_cursor_open rh fts_meta.Cat.fts_content_tree in
  let _sr = S.cursor_first cur in
  let exhausted = ref false in
  let ended = ref false in
  let finish () =
    if !ended
    then Lwt.return_unit
    else (
      ended := true;
      S.cursor_close cur;
      rh_finish rh)
  in
  let rec read_next () =
    if !exhausted
    then Lwt.return_none
    else (
      match S.cursor_next cur with
      | None ->
        exhausted := true;
        let%lwt () = finish () in
        Lwt.return_none
      | Some (_key, val_bytes) ->
        incr_examined s_opt;
        let texts = fts_decode_content val_bytes in
        let row = Array.of_list (List.map (fun s -> Row.V_text s) texts) in
        let emit =
          match where with
          | None -> true
          | Some pred -> value_truthy (eval_expr clock params row pred)
        in
        if emit then Lwt.return_some row else read_next ())
  in
  Lwt.return
    (Lwt_stream.from (fun () ->
       Lwt.catch read_next (fun exn ->
         exhausted := true;
         let%lwt () = finish () in
         Lwt.fail exn)))

and stream_fts_match_scan
      _clock
      _params
      store
      mode
      (fts_meta : Cat.fts_table_meta)
      query
      proj
      include_rank
      snippets
  =
  (* #257: each matched FTS index row counts as one examined row — the index
     seek (and the content fetch it drives) is the work this scan does.  The
     increment sits on the match, ahead of the content [S.get], so a match whose
     content row is absent still counts: the seek happened regardless. *)
  let s_opt = Lwt.get query_stats_key in
  (* #262: run the index query and content fetches through the active txn (when
     one is open) so an in-transaction write to the FTS table is matched and
     returned.  The body delegates the raw txn to the FTS helpers, so it is made
     polymorphic over the txn kind rather than using a [read_handle]. *)
  let body : type a. a S.txn -> Row.t Lwt_stream.t Lwt.t =
    fun tx ->
    let* matches = fts_execute_query tx ~index_tree:fts_meta.Cat.fts_index_tree query in
    let* scored_matches = fts_score_matches tx fts_meta query matches include_rank in
    let sorted =
      if include_rank
      then List.sort (fun (_, _, s1) (_, _, s2) -> Float.compare s2 s1) scored_matches
      else scored_matches
    in
    let snippet_terms = fts_query_terms_with_kind query in
    let* rows =
      Lwt_list.filter_map_s
        (fun (rowid, _positions, score) ->
           incr_examined s_opt;
           let key = Rowid.encode rowid in
           let* val_opt = S.get tx fts_meta.Cat.fts_content_tree key in
           match val_opt with
           | None -> Lwt.return None
           | Some bytes ->
             let texts = fts_decode_content bytes in
             let full_row = Array.of_list (List.map (fun s -> Row.V_text s) texts) in
             let projected =
               if proj = [] && snippets = []
               then Array.to_list full_row
               else List.map (fun i -> full_row.(i)) proj
             in
             let snippet_vals =
               List.map
                 (fun (spec : Plan.snippet_spec) ->
                    let col_text =
                      let idx =
                        if spec.Plan.col_idx < 0
                        then 0
                        else min spec.Plan.col_idx (max 0 (List.length texts - 1))
                      in
                      if texts = [] then "" else List.nth texts idx
                    in
                    Row.V_text
                      (compute_snippet ~col_text ~query_terms:snippet_terms ~spec))
                 snippets
             in
             let row_values =
               projected
               @ (if include_rank then [ Row.V_real score ] else [])
               @ snippet_vals
             in
             Lwt.return (Some (Array.of_list row_values)))
        sorted
    in
    Lwt.return (Lwt_stream.of_list rows)
  in
  match mode with
  | In_txn tx -> body tx
  | In_ro_txn tx -> body tx
  | Auto -> S.with_ro store body

and stream_pragma_integrity_check store cat =
  let cat_val =
    match cat with
    | None -> failwith "Exec.to_stream: Op_pragma_integrity_check requires catalog"
    | Some c -> c
  in
  let* tables = Cat.list_tables cat_val in
  let errors = ref [] in
  let add_err msg = errors := msg :: !errors in
  let count_entries tx tid =
    let count = ref 0 in
    let* cur = S.cursor_open tx tid in
    let _sr = S.cursor_first cur in
    let rec go () =
      match S.cursor_next cur with
      | None -> Lwt.return_unit
      | Some _ ->
        incr count;
        go ()
    in
    let* () = go () in
    S.cursor_close cur;
    Lwt.return !count
  in
  S.with_ro store
  @@ fun tx ->
  let* () =
    Lwt_list.iter_s
      (fun (meta : Cat.table_meta) ->
         let* row_count = count_entries tx meta.tree_id in
         let idxs = Cat.indexes_for_table cat_val ~table:meta.name in
         Lwt_list.iter_s
           (fun (idx : Cat.index_info) ->
              let is_partial = idx.idx_where_sql <> None in
              let* idx_count = count_entries tx idx.idx_tree_id in
              if (not is_partial) && idx_count <> row_count
              then
                add_err
                  (Printf.sprintf
                     "index %s on %s: %d entries != %d rows"
                     idx.idx_name
                     meta.name
                     idx_count
                     row_count);
              Lwt.return_unit)
           idxs)
      tables
  in
  let result = List.rev !errors in
  let rows =
    if result = []
    then [ [| Row.V_text "ok" |] ]
    else List.map (fun msg -> [| Row.V_text msg |]) result
  in
  Lwt.return (Lwt_stream.of_list rows)

and stream_sqlite_master store cat =
  let cat_val =
    match cat with
    | None -> failwith "Exec.to_stream: Op_sqlite_master requires catalog"
    | Some c -> c
  in
  let* tables = Cat.list_tables cat_val in
  let table_rows =
    List.map
      (fun (meta : Cat.table_meta) ->
         [| Row.V_text "table"
          ; Row.V_text meta.Cat.name
          ; Row.V_text meta.Cat.name
          ; Row.V_int (Int64.of_int meta.Cat.tree_id)
          ; Row.V_text (ddl_of_table meta)
         |])
      tables
  in
  let index_rows =
    List.concat_map
      (fun (meta : Cat.table_meta) ->
         List.map
           (fun (idx : Cat.index_info) ->
              [| Row.V_text "index"
               ; Row.V_text idx.Cat.idx_name
               ; Row.V_text idx.Cat.idx_table
               ; Row.V_int (Int64.of_int idx.Cat.idx_tree_id)
               ; Row.V_text (ddl_of_index idx)
              |])
           (Cat.indexes_for_table cat_val ~table:meta.Cat.name))
      tables
  in
  let* views = Cat.load_all_views store in
  let view_rows =
    List.map
      (fun (name, sql) ->
         [| Row.V_text "view"
          ; Row.V_text name
          ; Row.V_text name
          ; Row.V_int 0L
          ; Row.V_text sql
         |])
      views
  in
  let* triggers = Cat.load_all_triggers store in
  let trigger_rows =
    List.map
      (fun (name, sql) ->
         let tbl_name = trigger_table_of_sql name sql in
         [| Row.V_text "trigger"
          ; Row.V_text name
          ; Row.V_text tbl_name
          ; Row.V_int 0L
          ; Row.V_text sql
         |])
      triggers
  in
  let fts_rows =
    List.map
      (fun (m : Cat.fts_table_meta) ->
         [| Row.V_text "table"
          ; Row.V_text m.Cat.fts_name
          ; Row.V_text m.Cat.fts_name
          ; Row.V_int (Int64.of_int m.Cat.fts_content_tree)
          ; Row.V_text (ddl_of_fts m)
         |])
      (Cat.list_fts_tables cat_val)
  in
  (* #312: sqlite_sequence appears in sqlite_master once any AUTOINCREMENT
     table exists (matching SQLite — independent of whether a row has been
     inserted yet). *)
  let seq_rows =
    if List.exists (fun (m : Cat.table_meta) -> m.Cat.autoincrement) tables
    then
      [ [| Row.V_text "table"
         ; Row.V_text "sqlite_sequence"
         ; Row.V_text "sqlite_sequence"
         ; Row.V_int 0L
         ; Row.V_text "CREATE TABLE sqlite_sequence(name,seq)"
        |]
      ]
    else []
  in
  Lwt.return
    (Lwt_stream.of_list
       (table_rows @ seq_rows @ index_rows @ view_rows @ trigger_rows @ fts_rows))

and stream_sqlite_sequence cat =
  let cat_val =
    match cat with
    | None -> failwith "Exec.to_stream: Op_sqlite_sequence requires catalog"
    | Some c -> c
  in
  let* tables = Cat.list_tables cat_val in
  let rows =
    List.filter_map
      (fun (m : Cat.table_meta) ->
         if m.Cat.autoincrement && not (Int64.equal m.Cat.next_rowid Cat.empty_next_rowid)
         then
           (* [next_rowid] is the next id to allocate, so the high-water mark
              (last id handed out — SQLite's [sqlite_sequence.seq]) is
              [next_rowid - 1]. *)
           Some [| Row.V_text m.Cat.name; Row.V_int (Int64.sub m.Cat.next_rowid 1L) |]
         else None)
      tables
  in
  Lwt.return (Lwt_stream.of_list rows)

and stream_union clock params store mode cat all left right =
  let* ls = to_stream clock params store ~mode ~cat left in
  let* rs = to_stream clock params store ~mode ~cat right in
  let combined = Lwt_stream.append ls rs in
  if all
  then Lwt.return combined
  else
    let* rows = Lwt_stream.to_list combined in
    let seen = Hashtbl.create 64 in
    let deduped =
      List.filter
        (fun row ->
           let k = row_key row in
           if Hashtbl.mem seen k
           then false
           else (
             Hashtbl.replace seen k ();
             true))
        rows
    in
    Lwt.return (Lwt_stream.of_list deduped)

and stream_intersect clock params store mode cat left right =
  let* ls = to_stream clock params store ~mode ~cat left in
  let* rs = to_stream clock params store ~mode ~cat right in
  let* right_list = Lwt_stream.to_list rs in
  let right_set = Hashtbl.create (max 1 (List.length right_list)) in
  List.iter (fun r -> Hashtbl.replace right_set (row_key r) ()) right_list;
  let* left_list = Lwt_stream.to_list ls in
  let seen = Hashtbl.create 64 in
  let result =
    List.filter
      (fun row ->
         let k = row_key row in
         if (not (Hashtbl.mem right_set k)) || Hashtbl.mem seen k
         then false
         else (
           Hashtbl.replace seen k ();
           true))
      left_list
  in
  Lwt.return (Lwt_stream.of_list result)

and stream_except clock params store mode cat left right =
  let* ls = to_stream clock params store ~mode ~cat left in
  let* rs = to_stream clock params store ~mode ~cat right in
  let* right_list = Lwt_stream.to_list rs in
  let right_set = Hashtbl.create (max 1 (List.length right_list)) in
  List.iter (fun r -> Hashtbl.replace right_set (row_key r) ()) right_list;
  let* left_list = Lwt_stream.to_list ls in
  let seen = Hashtbl.create 64 in
  let result =
    List.filter
      (fun row ->
         let k = row_key row in
         if Hashtbl.mem right_set k || Hashtbl.mem seen k
         then false
         else (
           Hashtbl.replace seen k ();
           true))
      left_list
  in
  Lwt.return (Lwt_stream.of_list result)

and stream_insert_returning
      clock
      params
      store
      mode
      cat
      (table_meta : Cat.table_meta)
      ordinals
      values
      on_conflict
      returning
      upsert_update
  =
  match cat with
  | None -> failwith "Exec.query: RETURNING requires catalog context"
  | Some c ->
    let* result_lists =
      Lwt_list.map_s
        (fun row_vals ->
           let n = List.length table_meta.columns in
           let inserted_row = Array.make n Row.V_null in
           List.iter2
             (fun ord e -> inserted_row.(ord) <- eval_expr clock params [||] e)
             ordinals
             row_vals;
           let* inserted =
             execute_insert
               ~mode
               ~clock
               ~on_conflict
               ~upsert_update
               ~prebuilt_row:(Some inserted_row)
               store
               c
               ~table_meta
               ~ordinals
               ~values:row_vals
           in
           if not inserted
           then Lwt.return []
           else (
             let result =
               Array.of_list (List.map (eval_expr clock params inserted_row) returning)
             in
             Lwt.return [ result ]))
        values
    in
    Lwt.return (Lwt_stream.of_list (List.concat result_lists))

and stream_update_returning
      clock
      params
      store
      mode
      cat
      (table_meta : Cat.table_meta)
      assignments
      where
      order
      limit
      offset
      indexes
      returning
  =
  (* Project RETURNING from the rows actually written INSIDE the update's write
     txn (via [collect]), not a separate pre-lock RO snapshot — so concurrent
     `UPDATE ... RETURNING` callers see read-from-the-write values, never stale
     or duplicated ones (#226).  Rows arrive in update order (post
     order/offset/limit), which is also the RETURNING order. *)
  let c =
    match cat with
    | Some c -> c
    | None -> failwith "Exec.to_stream: UPDATE RETURNING requires catalog context"
  in
  let acc = ref [] in
  let collect new_row =
    acc := Array.of_list (List.map (eval_expr clock params new_row) returning) :: !acc
  in
  let* _ =
    execute_update
      ~mode
      ~params
      ~clock
      ~collect:(Some collect)
      store
      c
      ~table_meta
      ~assignments
      ~where
      ~order
      ~limit
      ~offset
      ~indexes
  in
  Lwt.return (Lwt_stream.of_list (List.rev !acc))

and stream_delete_returning
      clock
      params
      store
      mode
      cat
      (table_meta : Cat.table_meta)
      where
      order
      limit
      offset
      indexes
      returning
  =
  (* Project RETURNING from the rows actually deleted INSIDE the delete's write
     txn (via [collect]), not a separate pre-lock RO snapshot (#226). *)
  let c =
    match cat with
    | Some c -> c
    | None -> failwith "Exec.to_stream: DELETE RETURNING requires catalog context"
  in
  let acc = ref [] in
  let collect old_row =
    acc := Array.of_list (List.map (eval_expr clock params old_row) returning) :: !acc
  in
  let* _ =
    execute_delete
      ~mode
      ~params
      ~clock
      ~collect:(Some collect)
      store
      c
      ~table_meta
      ~where
      ~order
      ~limit
      ~offset
      ~indexes
  in
  Lwt.return (Lwt_stream.of_list (List.rev !acc))

and stream_const_select clock params store cat exprs =
  let raw_exprs = List.map fst exprs in
  let* exprs' = Lwt_list.map_s (pre_eval_subquery clock store params cat) raw_exprs in
  let row = Array.of_list (List.map (eval_expr clock params [||]) exprs') in
  Lwt.return (Lwt_stream.of_list [ row ])

and stream_with_cte_recursive clock params store mode cat cte_name def query =
  let base_op, recursive_arm =
    match def with
    | Plan.Op_union { all = true; left; right } -> left, right
    | _ ->
      failwith
        "Exec: recursive CTE def must be UNION ALL — non-UNION-ALL recursive CTEs are \
         not supported"
  in
  let* base_stream = to_stream clock params store ~mode ~cat base_op in
  let* seed_rows = Lwt_stream.to_list base_stream in
  let max_iterations = 1000 in
  let rec iterate depth acc working =
    if working = []
    then Lwt.return acc
    else if depth >= max_iterations
    then
      failwith
        (Printf.sprintf
           "Exec: recursive CTE '%s' exceeded maximum iteration depth of %d"
           cte_name
           max_iterations)
    else (
      let patched_arm = substitute_cte ~cte_name ~rows:working recursive_arm in
      let* new_stream = to_stream clock params store ~mode ~cat patched_arm in
      let* new_rows = Lwt_stream.to_list new_stream in
      iterate (depth + 1) (acc @ new_rows) new_rows)
  in
  let* all_rows = iterate 0 seed_rows seed_rows in
  let patched_query = substitute_cte ~cte_name ~rows:all_rows query in
  to_stream clock params store ~mode ~cat patched_query

and stream_window clock params store mode cat child windows =
  let* child_stream = to_stream clock params store ~mode ~cat child in
  let* all_rows = Lwt_stream.to_list child_stream in
  let n_rows = List.length all_rows in
  if n_rows = 0
  then Lwt.return (Lwt_stream.of_list [])
  else (
    let all_rows_arr = Array.of_list all_rows in
    let n_windows = List.length windows in
    let window_results : Row.value array array =
      Array.init n_windows (fun wi ->
        let wplan = List.nth windows wi in
        let indexed_rows = List.mapi (fun i row -> i, row) all_rows in
        let partitions =
          group_by_partition clock params wplan.Plan.partition_by indexed_rows
        in
        let combined = Array.make n_rows Row.V_null in
        List.iter
          (fun (_, partition_idx_rows) ->
             let sorted =
               sort_partition_by clock params wplan.Plan.order_by partition_idx_rows
             in
             let part_results =
               compute_window_for_partition clock params wplan sorted n_rows
             in
             List.iter
               (fun (orig_idx, _) -> combined.(orig_idx) <- part_results.(orig_idx))
               sorted)
          partitions;
        combined)
    in
    let augmented =
      Array.to_list
        (Array.mapi
           (fun i row ->
              let extras = Array.init n_windows (fun wi -> window_results.(wi).(i)) in
              Array.append row extras)
           all_rows_arr)
    in
    Lwt.return (Lwt_stream.of_list augmented))

and stream_explain clock params store mode cat analyze inner =
  let plan_rows = explain_plan inner in
  let nullify row = Array.append row [| Row.V_null; Row.V_null |] in
  if not analyze
  then Lwt.return (Lwt_stream.of_list (List.map nullify plan_rows))
  else (
    let cat_v =
      match cat with
      | Some c -> c
      | None -> failwith "EXPLAIN ANALYZE requires a catalog"
    in
    let t0 =
      match clock with
      | Some c -> c ()
      | None -> 0.0
    in
    let* n =
      let is_write =
        match inner with
        | Plan.Op_insert _
        | Plan.Op_insert_select _
        | Plan.Op_update _
        | Plan.Op_delete _
        | Plan.Op_create_table _
        | Plan.Op_create_index _
        | Plan.Op_drop_table _
        | Plan.Op_drop_index _
        | Plan.Op_alter_table _
        | Plan.Op_begin
        | Plan.Op_commit
        | Plan.Op_rollback
        | Plan.Op_savepoint _
        | Plan.Op_release _
        | Plan.Op_rollback_to _
        | Plan.Op_create_view _
        | Plan.Op_drop_view _
        | Plan.Op_create_trigger _
        | Plan.Op_drop_trigger _
        | Plan.Op_pragma_set_user_version _
        | Plan.Op_pragma_set_fk _
        | Plan.Op_pragma_set_recursive_triggers _
        | Plan.Op_pragma_set_defer_fk _
        | Plan.Op_pragma_set_wal_autocheckpoint _
        | Plan.Op_pragma_set_synchronous _
        | Plan.Op_pragma_set_wal_batch_commits _
        | Plan.Op_pragma_set_wal_batch_interval_ms _
        | Plan.Op_fts_insert _
        | Plan.Op_fts_delete _
        | Plan.Op_create_fts_table _ -> true
        | _ -> false
      in
      if is_write
      then execute_with_count ~mode ~clock ~params store cat_v inner
      else
        let* s = to_stream clock params store ~mode ~cat inner in
        let* rows = Lwt_stream.to_list s in
        Lwt.return (List.length rows)
    in
    let elapsed_ms =
      match clock with
      | Some c -> (c () -. t0) *. 1000.0
      | None -> 0.0
    in
    let rows =
      List.mapi
        (fun i row ->
           if i = 0
           then Array.append row [| Row.V_int (Int64.of_int n); Row.V_real elapsed_ms |]
           else nullify row)
        plan_rows
    in
    Lwt.return (Lwt_stream.of_list rows))

and to_stream
      (clock : (unit -> float) option)
      (params : Row.value array)
      (store : S.t)
      ?(mode : txn_mode = Auto)
      ?(cat : Cat.t option = None)
      (op : Plan.op)
  : Row.t Lwt_stream.t Lwt.t
  =
  match op with
  | Plan.Op_seq_scan { table_meta } -> stream_seq_scan clock params store mode table_meta
  | Plan.Op_filter { pred; child } -> stream_filter clock params store mode cat pred child
  | Plan.Op_project { ordinals; child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    Lwt.return (Lwt_stream.map (project_row ordinals) inner)
  | Plan.Op_expr_project { exprs; child } ->
    stream_expr_project clock params store mode cat exprs child
  | Plan.Op_sort { keys; child } -> stream_sort clock params store mode cat keys child
  | Plan.Op_limit { limit; offset; child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    let* rows = Lwt_stream.to_list inner in
    let rows' = List.filteri (fun i _ -> i >= offset && i < offset + limit) rows in
    Lwt.return (Lwt_stream.of_list rows')
  | Plan.Op_distinct { child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    let seen = Hashtbl.create 64 in
    Lwt.return
      (Lwt_stream.filter
         (fun row ->
            let k = row_key row in
            if Hashtbl.mem seen k
            then false
            else (
              Hashtbl.replace seen k ();
              true))
         inner)
  | Plan.Op_index_lookup
      { table_tree; idx_tree; col_idx = _; col_type; lookup_val; table_meta } ->
    stream_index_lookup
      clock
      params
      store
      mode
      table_tree
      idx_tree
      col_type
      lookup_val
      table_meta
  | Plan.Op_rowid_lookup { table_meta; lookup_val } ->
    stream_rowid_lookup clock params store mode lookup_val table_meta
  | Plan.Op_nested_loop_join
      { left
      ; right_meta
      ; idx_tree
      ; right_col_idx = _
      ; left_col_idx
      ; join_kind
      ; right_col_offset = _
      ; n_right_cols
      } ->
    stream_nested_loop_join
      clock
      params
      store
      mode
      cat
      left
      right_meta
      idx_tree
      left_col_idx
      join_kind
      n_right_cols
  | Plan.Op_hash_join
      { left; right; left_key; right_key; join_kind; right_col_offset = _; n_right_cols }
    ->
    stream_hash_join
      clock
      params
      store
      mode
      cat
      left
      right
      left_key
      right_key
      join_kind
      n_right_cols
  | Plan.Op_aggregate { child; group_cols; aggs; having; proj; windows = agg_windows } ->
    stream_aggregate
      clock
      params
      store
      mode
      cat
      child
      group_cols
      aggs
      having
      proj
      agg_windows
  | Plan.Op_fts_seq_scan { fts_meta; where } ->
    stream_fts_seq_scan clock params store mode fts_meta where
  | Plan.Op_fts_match_scan { fts_meta; query; proj; include_rank; snippets } ->
    stream_fts_match_scan
      clock
      params
      store
      mode
      fts_meta
      query
      proj
      include_rank
      snippets
  | Plan.Op_pragma_rows { rows } -> Lwt.return (Lwt_stream.of_list rows)
  | Plan.Op_pragma_get_user_version ->
    S.with_ro store
    @@ fun tx ->
    let* v = Cat.read_user_version_tx tx in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int v |] ])
  | Plan.Op_pragma_get_fk ->
    let v =
      match cat with
      | None -> false
      | Some cat -> Cat.get_fk_enforcement cat
    in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int (if v then 1L else 0L) |] ])
  | Plan.Op_pragma_get_recursive_triggers ->
    let v =
      match cat with
      | None -> true
      | Some cat -> Cat.get_recursive_triggers cat
    in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int (if v then 1L else 0L) |] ])
  | Plan.Op_pragma_get_wal_autocheckpoint ->
    let n = S.wal_autocheckpoint store in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int (Int64.of_int n) |] ])
  | Plan.Op_pragma_get_synchronous ->
    let s = S.string_of_durability (S.durability store) in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_text s |] ])
  | Plan.Op_pragma_get_wal_batch_commits ->
    let n = S.sync_batch_commits store in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int (Int64.of_int n) |] ])
  | Plan.Op_pragma_get_wal_batch_interval_ms ->
    let n = S.sync_batch_interval_ms store in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int (Int64.of_int n) |] ])
  | Plan.Op_pragma_get_defer_fk ->
    let v =
      match cat with
      | None -> false
      | Some cat -> Cat.get_defer_fks_pragma cat
    in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int (if v then 1L else 0L) |] ])
  | Plan.Op_pragma_integrity_check -> stream_pragma_integrity_check store cat
  | Plan.Op_sqlite_master -> stream_sqlite_master store cat
  | Plan.Op_sqlite_sequence -> stream_sqlite_sequence cat
  | Plan.Op_union { all; left; right } ->
    stream_union clock params store mode cat all left right
  | Plan.Op_intersect { left; right } ->
    stream_intersect clock params store mode cat left right
  | Plan.Op_except { left; right } -> stream_except clock params store mode cat left right
  | Plan.Op_insert { table_meta; ordinals; values; on_conflict; returning; upsert_update }
    when returning <> [] ->
    stream_insert_returning
      clock
      params
      store
      mode
      cat
      table_meta
      ordinals
      values
      on_conflict
      returning
      upsert_update
  | Plan.Op_update
      { table_meta; assignments; where; order; limit; offset; indexes; returning }
    when returning <> [] ->
    stream_update_returning
      clock
      params
      store
      mode
      cat
      table_meta
      assignments
      where
      order
      limit
      offset
      indexes
      returning
  | Plan.Op_delete { table_meta; where; order; limit; offset; indexes; returning }
    when returning <> [] ->
    stream_delete_returning
      clock
      params
      store
      mode
      cat
      table_meta
      where
      order
      limit
      offset
      indexes
      returning
  | Plan.Op_changes ->
    failwith "Exec.to_stream: Op_changes must be intercepted in db.ml query"
  | Plan.Op_last_insert_rowid ->
    failwith "Exec.to_stream: Op_last_insert_rowid must be intercepted in db.ml query"
  | Plan.Op_total_changes ->
    failwith "Exec.to_stream: Op_total_changes must be intercepted in db.ml query"
  | Plan.Op_const_select { exprs } -> stream_const_select clock params store cat exprs
  | Plan.Op_with_cte { cte_name; def; query; recursive = false } ->
    let* def_stream = to_stream clock params store ~mode ~cat def in
    let* cte_rows = Lwt_stream.to_list def_stream in
    let patched = substitute_cte ~cte_name ~rows:cte_rows query in
    to_stream clock params store ~mode ~cat patched
  | Plan.Op_with_cte { cte_name; def; query; recursive = true } ->
    stream_with_cte_recursive clock params store mode cat cte_name def query
  | Plan.Op_cte_scan { cte_name; _ } ->
    failwith
      (Printf.sprintf
         "Exec: unsubstituted Op_cte_scan '%s' — internal planner error"
         cte_name)
  | Plan.Op_window { child; windows; n_input_cols = _ } ->
    stream_window clock params store mode cat child windows
  | Plan.Op_no_op -> Lwt.return (Lwt_stream.of_list [])
  | Plan.Op_explain { analyze; inner } ->
    stream_explain clock params store mode cat analyze inner
  | Plan.Op_create_table _
  | Plan.Op_create_index _
  | Plan.Op_drop_table _
  | Plan.Op_drop_index _
  | Plan.Op_create_fts_table _
  | Plan.Op_fts_insert _
  | Plan.Op_fts_delete _
  | Plan.Op_alter_table _
  | Plan.Op_create_view _
  | Plan.Op_drop_view _
  | Plan.Op_create_trigger _
  | Plan.Op_drop_trigger _
  | Plan.Op_begin
  | Plan.Op_commit
  | Plan.Op_rollback
  | Plan.Op_savepoint _
  | Plan.Op_release _
  | Plan.Op_rollback_to _
  | Plan.Op_pragma_set_user_version _
  | Plan.Op_pragma_set_fk _
  | Plan.Op_pragma_set_recursive_triggers _
  | Plan.Op_pragma_set_defer_fk _
  | Plan.Op_pragma_set_wal_autocheckpoint _
  | Plan.Op_pragma_wal_checkpoint
  | Plan.Op_vacuum
  | Plan.Op_attach _
  | Plan.Op_detach _
  | Plan.Op_active_database_set _ ->
    failwith "Exec.query: use Exec.execute for write operations"
  | Plan.Op_database_list | Plan.Op_active_database_get ->
    failwith "Exec.query: routed via Db.query (no Db handle)"
  | Plan.Op_insert _ | Plan.Op_insert_select _ | Plan.Op_update _ | Plan.Op_delete _ ->
    failwith "Exec.query: use Exec.execute for write operations"
  | Plan.Op_seq_set _ | Plan.Op_seq_reset _ ->
    failwith "Exec.query: use Exec.execute for write operations"
  | Plan.Op_pragma_set_synchronous _
  | Plan.Op_pragma_set_wal_batch_commits _
  | Plan.Op_pragma_set_wal_batch_interval_ms _ ->
    failwith "Exec.query: use Exec.execute for write operations"
;;

(* Wire the forward reference so execute_with_count can call to_stream for
   Op_insert_select.  This runs once at module initialization time, after both
   functions are fully defined in the let-rec block above. *)
let () = to_stream_ref := to_stream

(* ------------------------------------------------------------------ *)
(* Public query entry point                                             *)
(* ------------------------------------------------------------------ *)

(* #239: [used_index] is a plan-time fact — does the query's base access reach
   the data through an index/rowid/FTS seek, or a full table scan?  Descends
   through the row-shaping wrappers to the base; [true] if ANY base uses a seek.
   A nested-loop join always probes its right table by index, so it counts. *)
let rec op_uses_index (op : Plan.op) : bool =
  match op with
  | Plan.Op_index_lookup _ | Plan.Op_rowid_lookup _ | Plan.Op_fts_match_scan _ -> true
  | Plan.Op_seq_scan _ | Plan.Op_fts_seq_scan _ -> false
  | Plan.Op_filter { child; _ }
  | Plan.Op_project { child; _ }
  | Plan.Op_expr_project { child; _ }
  | Plan.Op_sort { child; _ }
  | Plan.Op_limit { child; _ }
  | Plan.Op_distinct { child }
  | Plan.Op_aggregate { child; _ }
  | Plan.Op_window { child; _ } -> op_uses_index child
  | Plan.Op_nested_loop_join _ -> true
  | Plan.Op_hash_join { left; right; _ } -> op_uses_index left || op_uses_index right
  | Plan.Op_union { left; right; _ }
  | Plan.Op_intersect { left; right }
  | Plan.Op_except { left; right } -> op_uses_index left || op_uses_index right
  | Plan.Op_with_cte { query; _ } -> op_uses_index query
  (* Conservative: anything not recognised as a seek-bearing base access (or a
     wrapper over one) reports [false].  KEEP IN SYNC: a NEW index/seek-bearing
     plan op added here would silently report [used_index = false] until a case
     is added above. *)
  | _ -> false
;;

let query
      ?(mode = Auto)
      ?(clock : (unit -> float) option = None)
      ?(params = [||])
      ?(stats : query_stats option)
      (store : S.t)
      (cat : Cat.t)
      (op : Plan.op)
  : Row.t Lwt_stream.t Lwt.t
  =
  let body () =
    match stats with
    | None -> to_stream clock params store ~mode ~cat:(Some cat) op
    | Some s ->
      s.used_index <- op_uses_index op;
      (* Run the whole stream construction under the stats record so the base
         scanners capture it (Lwt sequence-associated storage); wrap the result
         to count rows actually delivered once the caller drains it. *)
      Lwt.with_value query_stats_key (Some s)
      @@ fun () ->
      let* stream = to_stream clock params store ~mode ~cat:(Some cat) op in
      Lwt.return
        (Lwt_stream.map
           (fun row ->
              s.rows_returned <- s.rows_returned + 1;
              row)
           stream)
  in
  (* #262: publish the txn mode to subquery evaluation only when inside an
     explicit transaction.  In [Auto] mode [current_txn_mode] already defaults to
     [Auto], so the common read path pays no [with_value] — preserving the
     zero-overhead scan path (#259). *)
  match mode with
  | Auto -> body ()
  | In_txn _ | In_ro_txn _ -> Lwt.with_value txn_mode_key (Some mode) body
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
