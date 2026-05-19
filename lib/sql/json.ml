(* lib/sql/json.ml *)
type value =
  | J_null
  | J_bool  of bool
  | J_int   of int64
  | J_float of float
  | J_string of string
  | J_array  of value list
  | J_object of (string * value) list

(* ── serialiser ──────────────────────────────────────────────── *)

let to_string v =
  let buf = Buffer.create 64 in
  let rec go = function
    | J_null     -> Buffer.add_string buf "null"
    | J_bool b   -> Buffer.add_string buf (if b then "true" else "false")
    | J_int n    -> Buffer.add_string buf (Int64.to_string n)
    | J_float f  ->
      let s = Printf.sprintf "%.15g" f in
      if String.contains s '.' || String.contains s 'e' || String.contains s 'E'
      then Buffer.add_string buf s
      else (Buffer.add_string buf s; Buffer.add_string buf ".0")
    | J_string s ->
      Buffer.add_char buf '"';
      String.iter (function
        | '"'  -> Buffer.add_string buf "\\\""
        | '\\' -> Buffer.add_string buf "\\\\"
        | '\n' -> Buffer.add_string buf "\\n"
        | '\r' -> Buffer.add_string buf "\\r"
        | '\t' -> Buffer.add_string buf "\\t"
        | c    -> Buffer.add_char buf c) s;
      Buffer.add_char buf '"'
    | J_array vs ->
      Buffer.add_char buf '[';
      List.iteri (fun i v -> if i > 0 then Buffer.add_char buf ','; go v) vs;
      Buffer.add_char buf ']'
    | J_object kvs ->
      Buffer.add_char buf '{';
      List.iteri (fun i (k, v) ->
        if i > 0 then Buffer.add_char buf ',';
        go (J_string k); Buffer.add_char buf ':'; go v) kvs;
      Buffer.add_char buf '}'
  in
  go v; Buffer.contents buf

(* ── type name ───────────────────────────────────────────────── *)

let type_name = function
  | J_null     -> "null"
  | J_bool b   -> if b then "true" else "false"
  | J_int _    -> "integer"
  | J_float _  -> "real"
  | J_string _ -> "text"
  | J_array _  -> "array"
  | J_object _ -> "object"

(* ── parser ──────────────────────────────────────────────────── *)

type state = { s : string; mutable pos : int }

let peek st =
  if st.pos < String.length st.s then Some st.s.[st.pos] else None

let advance st = st.pos <- st.pos + 1

let skip_ws st =
  while (match peek st with Some (' '|'\t'|'\n'|'\r') -> true | _ -> false)
  do advance st done

let expect_char st c =
  skip_ws st;
  match peek st with
  | Some x when x = c -> advance st; Ok ()
  | Some x -> Error (Printf.sprintf "expected '%c' got '%c' at pos %d" c x st.pos)
  | None   -> Error (Printf.sprintf "expected '%c' but got EOF" c)

let parse_string_body st =
  let buf = Buffer.create 16 in
  let rec loop () =
    match peek st with
    | None    -> Error "unterminated string"
    | Some '"' -> advance st; Ok (Buffer.contents buf)
    | Some '\\' ->
      advance st;
      (match peek st with
       | None -> Error "unterminated escape"
       | Some c ->
         advance st;
         (match c with
          | '"'  -> Buffer.add_char buf '"';  loop ()
          | '\\' -> Buffer.add_char buf '\\'; loop ()
          | '/'  -> Buffer.add_char buf '/';  loop ()
          | 'n'  -> Buffer.add_char buf '\n'; loop ()
          | 'r'  -> Buffer.add_char buf '\r'; loop ()
          | 't'  -> Buffer.add_char buf '\t'; loop ()
          | 'b'  -> Buffer.add_char buf '\b'; loop ()
          | 'f'  -> Buffer.add_char buf '\012'; loop ()
          | 'u'  ->
            for _ = 1 to 4 do
              (match peek st with Some _ -> advance st | None -> ())
            done;
            Buffer.add_char buf '?'; loop ()
          | _    -> Buffer.add_char buf c; loop ()))
    | Some c -> advance st; Buffer.add_char buf c; loop ()
  in
  loop ()

let rec parse_value st =
  skip_ws st;
  match peek st with
  | None -> Error "unexpected EOF"
  | Some '"' ->
    advance st;
    (match parse_string_body st with
     | Ok s -> Ok (J_string s) | Error e -> Error e)
  | Some '[' -> advance st; parse_array st
  | Some '{' -> advance st; parse_object st
  | Some 't' ->
    if String.length st.s - st.pos >= 4
       && String.sub st.s st.pos 4 = "true"
    then (st.pos <- st.pos + 4; Ok (J_bool true))
    else Error (Printf.sprintf "unexpected token at %d" st.pos)
  | Some 'f' ->
    if String.length st.s - st.pos >= 5
       && String.sub st.s st.pos 5 = "false"
    then (st.pos <- st.pos + 5; Ok (J_bool false))
    else Error (Printf.sprintf "unexpected token at %d" st.pos)
  | Some 'n' ->
    if String.length st.s - st.pos >= 4
       && String.sub st.s st.pos 4 = "null"
    then (st.pos <- st.pos + 4; Ok J_null)
    else Error (Printf.sprintf "unexpected token at %d" st.pos)
  | Some ('-' | '0'..'9') -> parse_number st
  | Some c -> Error (Printf.sprintf "unexpected char '%c' at pos %d" c st.pos)

and parse_number st =
  let start = st.pos in
  (match peek st with Some '-' -> advance st | _ -> ());
  while (match peek st with Some ('0'..'9') -> true | _ -> false) do advance st done;
  let is_float = ref false in
  (match peek st with
   | Some '.' ->
     is_float := true; advance st;
     while (match peek st with Some ('0'..'9') -> true | _ -> false) do advance st done
   | _ -> ());
  (match peek st with
   | Some ('e' | 'E') ->
     is_float := true; advance st;
     (match peek st with Some ('+' | '-') -> advance st | _ -> ());
     while (match peek st with Some ('0'..'9') -> true | _ -> false) do advance st done
   | _ -> ());
  let s = String.sub st.s start (st.pos - start) in
  if !is_float
  then (match float_of_string_opt s with
        | Some f -> Ok (J_float f)
        | None   -> Error ("invalid float: " ^ s))
  else (match Int64.of_string_opt s with
        | Some n -> Ok (J_int n)
        | None   -> match float_of_string_opt s with
          | Some f -> Ok (J_float f)
          | None   -> Error ("invalid number: " ^ s))

and parse_array st =
  skip_ws st;
  match peek st with
  | Some ']' -> advance st; Ok (J_array [])
  | _ ->
    let rec loop acc =
      match parse_value st with
      | Error e -> Error e
      | Ok v ->
        skip_ws st;
        (match peek st with
         | Some ',' -> advance st; loop (v :: acc)
         | Some ']' -> advance st; Ok (J_array (List.rev (v :: acc)))
         | Some c -> Error (Printf.sprintf "expected ',' or ']', got '%c'" c)
         | None   -> Error "unexpected EOF in array")
    in
    loop []

and parse_object st =
  skip_ws st;
  match peek st with
  | Some '}' -> advance st; Ok (J_object [])
  | _ ->
    let rec loop acc =
      skip_ws st;
      match peek st with
      | Some '"' ->
        advance st;
        (match parse_string_body st with
         | Error e -> Error e
         | Ok key ->
           (match expect_char st ':' with
            | Error e -> Error e
            | Ok () ->
              (match parse_value st with
               | Error e -> Error e
               | Ok value ->
                 skip_ws st;
                 (match peek st with
                  | Some ',' -> advance st; loop ((key, value) :: acc)
                  | Some '}' -> advance st; Ok (J_object (List.rev ((key, value) :: acc)))
                  | Some c -> Error (Printf.sprintf "expected ',' or '}', got '%c'" c)
                  | None   -> Error "unexpected EOF in object"))))
      | Some c -> Error (Printf.sprintf "expected '\"' for key, got '%c'" c)
      | None   -> Error "unexpected EOF in object"
    in
    loop []

let parse s =
  let st = { s; pos = 0 } in
  match parse_value st with
  | Error e -> Error e
  | Ok v ->
    skip_ws st;
    if st.pos = String.length st.s then Ok v
    else Error (Printf.sprintf "trailing content at pos %d" st.pos)

(* ── JSONPath ─────────────────────────────────────────────────── *)

type path_step = Key of string | Idx of int

let parse_path path =
  let n = String.length path in
  if n = 0 || path.[0] <> '$'
  then Error ("path must start with '$': " ^ path)
  else
    let steps = ref [] in
    let pos   = ref 1 in
    let err   = ref false in
    while not !err && !pos < n do
      match path.[!pos] with
      | '.' ->
        incr pos;
        let start = !pos in
        while !pos < n && path.[!pos] <> '.' && path.[!pos] <> '[' do incr pos done;
        if !pos = start then err := true
        else steps := Key (String.sub path start (!pos - start)) :: !steps
      | '[' ->
        incr pos;
        let start = !pos in
        while !pos < n && path.[!pos] <> ']' do incr pos done;
        if !pos >= n then err := true
        else
          (match int_of_string_opt (String.sub path start (!pos - start)) with
           | Some i -> steps := Idx i :: !steps; incr pos
           | None   -> err := true)
      | _ -> err := true
    done;
    if !err then Error ("invalid JSON path: " ^ path)
    else Ok (List.rev !steps)

let path_get v path =
  match parse_path path with
  | Error _ -> None
  | Ok steps ->
    let rec go v = function
      | [] -> Some v
      | Key k :: rest ->
        (match v with
         | J_object kvs ->
           (match List.assoc_opt k kvs with Some sub -> go sub rest | None -> None)
         | _ -> None)
      | Idx i :: rest ->
        (match v with
         | J_array elems ->
           let n = List.length elems in
           let i = if i < 0 then n + i else i in
           if i < 0 || i >= n then None else go (List.nth elems i) rest
         | _ -> None)
    in
    go v steps

(* ── path mutation ───────────────────────────────────────────── *)

type set_mode = Set | Insert | Replace

let path_modify mode v path new_val =
  match parse_path path with
  | Error _ -> v
  | Ok [] ->
    (match mode with Set | Replace -> new_val | Insert -> v)
  | Ok steps ->
    let rec go v steps =
      match steps with
      | [] -> assert false
      | [Key k] ->
        (match v with
         | J_object kvs ->
           let exists = List.mem_assoc k kvs in
           (match mode with
            | Set ->
              if exists
              then J_object (List.map (fun (k2,v2) -> if k2=k then (k2,new_val) else (k2,v2)) kvs)
              else J_object (kvs @ [(k, new_val)])
            | Insert ->
              if exists then v else J_object (kvs @ [(k, new_val)])
            | Replace ->
              if not exists then v
              else J_object (List.map (fun (k2,v2) -> if k2=k then (k2,new_val) else (k2,v2)) kvs))
         | _ -> v)
      | [Idx i] ->
        (match v with
         | J_array elems ->
           let n = List.length elems in
           let i = if i < 0 then n + i else i in
           (match mode with
            | Set ->
              if i < 0 || i > n then v
              else if i = n then J_array (elems @ [new_val])
              else J_array (List.mapi (fun j e -> if j = i then new_val else e) elems)
            | Replace ->
              if i < 0 || i >= n then v
              else J_array (List.mapi (fun j e -> if j = i then new_val else e) elems)
            | Insert ->
              if i < 0 || i > n then v
              else
                let arr    = Array.of_list elems in
                let result = Array.make (n + 1) J_null in
                Array.blit arr 0 result 0 i;
                result.(i) <- new_val;
                Array.blit arr i result (i + 1) (n - i);
                J_array (Array.to_list result))
         | _ -> v)
      | Key k :: rest ->
        (match v with
         | J_object kvs ->
           (match List.assoc_opt k kvs with
            | Some sub ->
              let new_sub = go sub rest in
              J_object (List.map (fun (k2,v2) -> if k2=k then (k2,new_sub) else (k2,v2)) kvs)
            | None -> v)
         | _ -> v)
      | Idx i :: rest ->
        (match v with
         | J_array elems ->
           let n = List.length elems in
           let i = if i < 0 then n + i else i in
           if i < 0 || i >= n then v
           else J_array (List.mapi (fun j e -> if j = i then go e rest else e) elems)
         | _ -> v)
    in
    go v steps

let path_set     v path new_val = path_modify Set     v path new_val
let path_insert  v path new_val = path_modify Insert  v path new_val
let path_replace v path new_val = path_modify Replace v path new_val

let path_remove v path =
  match parse_path path with
  | Error _ -> v
  | Ok [] -> v
  | Ok steps ->
    let rec go v steps =
      match steps with
      | [] -> assert false
      | [Key k] ->
        (match v with
         | J_object kvs -> J_object (List.filter (fun (k2,_) -> k2 <> k) kvs)
         | _ -> v)
      | [Idx i] ->
        (match v with
         | J_array elems ->
           let n = List.length elems in
           let i = if i < 0 then n + i else i in
           if i < 0 || i >= n then v
           else J_array (List.filteri (fun j _ -> j <> i) elems)
         | _ -> v)
      | Key k :: rest ->
        (match v with
         | J_object kvs ->
           (match List.assoc_opt k kvs with
            | Some sub ->
              let new_sub = go sub rest in
              J_object (List.map (fun (k2,v2) -> if k2=k then (k2,new_sub) else (k2,v2)) kvs)
            | None -> v)
         | _ -> v)
      | Idx i :: rest ->
        (match v with
         | J_array elems ->
           let n = List.length elems in
           let i = if i < 0 then n + i else i in
           if i < 0 || i >= n then v
           else J_array (List.mapi (fun j e -> if j = i then go e rest else e) elems)
         | _ -> v)
    in
    go v steps
