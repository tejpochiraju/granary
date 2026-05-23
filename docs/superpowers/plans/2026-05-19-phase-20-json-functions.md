# Phase 20: JSON Scalar Functions Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add 9 JSON scalar functions (json_extract, json_object, json_array, json_type, json_valid, json_set, json_insert, json_replace, json_remove) backed by a pure-OCaml JSON parser/serializer with JSONPath support.

**Architecture:** New `lib/sql/json.ml` module owns parsing, serialization, type naming, and path operations. Exec.ml adds `json_of_sql`/`sql_of_json` helpers and 9 `eval_func` cases. AST, lexer, and parser each gain 9 new entries following the exact same pattern as the Phase 19 math functions.

**Tech Stack:** OCaml 5.x, dune 3.x (podman build), menhir parser, alcotest tests; no external JSON library.

---

## Codebase orientation

The pipeline is AST → Sema → Plan → Exec. Scalar functions are a closed ADT:
- `lib/sql/ast.ml` — `type scalar_func` with all `Fn_*` variants; `func_to_sql` match at bottom of file
- `lib/sql/lexer.mll` — keywords are matched first as string literals; "soft" keywords (including all math functions) are matched in the trailing `ident as id { match String.uppercase_ascii id with ... | _ -> IDENT id }` block
- `lib/sql/parser.mly` — `%token` declarations at top; `scalar_expr` rule (starts ~line 370) lists one rule per function form
- `lib/sql/exec.ml` — `eval_func` (starts ~line 382) matches `(func, args)` pairs; `to_float_opt` helper defined once before the `match`
- `test/test_e2e.ml` — standalone test functions registered at the bottom in `Alcotest.run`
- `test/test_sqlite_compare.ml` — `{name; setup; query; unordered}` record lists registered at the bottom

**Build (all dune commands must run inside podman):**
```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
```
**Run e2e tests:**
```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1
```
**Run SQLite comparison tests:**
```bash
podman run --rm \
  -v $(pwd):/workspace:Z \
  -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
  -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
  -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
  -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
  -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe 2>&1
```

## File map

| File | Action | What changes |
|------|--------|--------------|
| `lib/sql/json.ml` | **Create** | JSON value type, parser, serializer, type_name, path_get, path_set/insert/replace/remove |
| `lib/sql/json.mli` | **Create** | Public interface for json.ml |
| `lib/sql/ast.ml` | Modify | 9 new `Fn_json_*` variants; 9 new `func_to_sql` cases |
| `lib/sql/lexer.mll` | Modify | 9 new ident-match cases in the `match String.uppercase_ascii id with` block |
| `lib/sql/parser.mly` | Modify | 9 new `%token` declarations; 12 new `scalar_expr` rules (some functions have 1-arg and 2-arg forms) |
| `lib/sql/exec.ml` | Modify | `json_of_sql`/`sql_of_json` helpers; 9 `eval_func` cases |
| `test/test_e2e.ml` | Modify | 8 new test functions; registered in `"json"` suite |
| `test/test_sqlite_compare.ml` | Modify | `phase20_json_cases` + `phase20_json_mutation_cases` lists; registered in runner |

`lib/sql/dune` needs **no changes** — all `.ml` files in `lib/sql/` are compiled automatically; only menhir and ocamllex modules need explicit listing.

---

## Task 1: JSON module + core read-only functions

**Implements:** `json_extract`, `json_object`, `json_array`, `json_type`, `json_valid` (Issue #126 core)

**Files:**
- Create: `lib/sql/json.mli`
- Create: `lib/sql/json.ml`
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`
- Modify: `test/test_sqlite_compare.ml`

---

- [ ] **Step 1: Write failing e2e tests for core JSON functions**

Add these test functions to `test/test_e2e.ml`, just before the final `let () = Alcotest.run` call:

```ocaml
let test_json_extract () =
  let db = fresh_db () in
  let r1 = query_ok db {|SELECT json_extract('{"a":1,"b":"hi"}', '$.a')|} in
  Alcotest.(check row_testable) "extract int" [| Db.V_int 1L |] (List.nth r1 0);
  let r2 = query_ok db {|SELECT json_extract('{"a":1,"b":"hi"}', '$.b')|} in
  Alcotest.(check row_testable) "extract text" [| Db.V_text "hi" |] (List.nth r2 0);
  let r3 = query_ok db {|SELECT json_extract('[10,20,30]', '$[1]')|} in
  Alcotest.(check row_testable) "extract array index" [| Db.V_int 20L |] (List.nth r3 0);
  let r4 = query_ok db {|SELECT json_extract('{"a":{"b":99}}', '$.a.b')|} in
  Alcotest.(check row_testable) "extract nested" [| Db.V_int 99L |] (List.nth r4 0);
  let r5 = query_ok db {|SELECT json_extract('{"a":1}', '$.missing')|} in
  Alcotest.(check row_testable) "extract missing = null" [| Db.V_null |] (List.nth r5 0);
  let r6 = query_ok db {|SELECT json_extract('{"a":1.5}', '$.a')|} in
  Alcotest.(check row_testable) "extract real" [| Db.V_real 1.5 |] (List.nth r6 0)

let test_json_object () =
  let db = fresh_db () in
  let r1 = query_ok db {|SELECT json_object('a', 1, 'b', 'hi')|} in
  Alcotest.(check row_testable) "json_object basic"
    [| Db.V_text {|{"a":1,"b":"hi"}|} |] (List.nth r1 0);
  let r2 = query_ok db {|SELECT json_object()|} in
  Alcotest.(check row_testable) "json_object empty"
    [| Db.V_text "{}" |] (List.nth r2 0)

let test_json_array () =
  let db = fresh_db () in
  let r1 = query_ok db {|SELECT json_array(1, 2, 3)|} in
  Alcotest.(check row_testable) "json_array ints"
    [| Db.V_text "[1,2,3]" |] (List.nth r1 0);
  let r2 = query_ok db {|SELECT json_array()|} in
  Alcotest.(check row_testable) "json_array empty"
    [| Db.V_text "[]" |] (List.nth r2 0)

let test_json_type () =
  let db = fresh_db () in
  let check q expected =
    let r = query_ok db q in
    Alcotest.(check row_testable) (q ^ "=" ^ expected) [| Db.V_text expected |] (List.nth r 0)
  in
  check {|SELECT json_type('{"a":1}')|} "object";
  check {|SELECT json_type('[1,2]')|} "array";
  check {|SELECT json_type('"hello"')|} "text";
  check {|SELECT json_type('42')|} "integer";
  check {|SELECT json_type('3.14')|} "real";
  check {|SELECT json_type('null')|} "null";
  check {|SELECT json_type('true')|} "true";
  check {|SELECT json_type('false')|} "false";
  (* 2-arg form with path *)
  let r2 = query_ok db {|SELECT json_type('{"a":1}', '$.a')|} in
  Alcotest.(check row_testable) "json_type with path" [| Db.V_text "integer" |] (List.nth r2 0)

let test_json_valid () =
  let db = fresh_db () in
  let r1 = query_ok db {|SELECT json_valid('{"a":1}')|} in
  Alcotest.(check row_testable) "valid json = 1" [| Db.V_int 1L |] (List.nth r1 0);
  let r2 = query_ok db {|SELECT json_valid('not json')|} in
  Alcotest.(check row_testable) "invalid json = 0" [| Db.V_int 0L |] (List.nth r2 0);
  let r3 = query_ok db {|SELECT json_valid('[]')|} in
  Alcotest.(check row_testable) "array is valid" [| Db.V_int 1L |] (List.nth r3 0)
```

Also add registration in the `Alcotest.run` call at the bottom of the file, after the `"math"` section:
```ocaml
    "json", [
      Alcotest.test_case "json_extract" `Quick test_json_extract;
      Alcotest.test_case "json_object"  `Quick test_json_object;
      Alcotest.test_case "json_array"   `Quick test_json_array;
      Alcotest.test_case "json_type"    `Quick test_json_type;
      Alcotest.test_case "json_valid"   `Quick test_json_valid;
    ];
```

- [ ] **Step 2: Verify tests fail to build (function not found)**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -20
```

Expected: build error about unknown token/function `json_extract`. If it somehow passes, something is wrong.

- [ ] **Step 3: Create `lib/sql/json.mli`**

```ocaml
(* lib/sql/json.mli *)
type value =
  | J_null
  | J_bool of bool
  | J_int  of int64
  | J_float of float
  | J_string of string
  | J_array  of value list
  | J_object of (string * value) list

val parse     : string -> (value, string) result
val to_string : value -> string
val type_name : value -> string

val path_get    : value -> string -> value option
val path_set    : value -> string -> value -> value
val path_insert : value -> string -> value -> value
val path_replace: value -> string -> value -> value
val path_remove : value -> string -> value
```

- [ ] **Step 4: Create `lib/sql/json.ml`**

```ocaml
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
      (* ensure it looks like a float so round-trips stay real *)
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
    (match mode with Set -> new_val | Insert | Replace -> v)
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
            | Set | Replace ->
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
```

- [ ] **Step 5: Add 5 new `Fn_json_*` variants to `lib/sql/ast.ml`**

After the last math variant (`| Fn_radians ...`) in the `type scalar_func` definition (around line 78), add:

```ocaml
  | Fn_json_extract   (** json_extract(json, path) *)
  | Fn_json_object    (** json_object(k,v,...) *)
  | Fn_json_array     (** json_array(v,...) *)
  | Fn_json_type      (** json_type(json[,path]) *)
  | Fn_json_valid     (** json_valid(json) → 0|1 *)
```

In `func_to_sql` (around line 326, after `| Fn_radians -> "RADIANS"`), add:

```ocaml
  | Fn_json_extract -> "JSON_EXTRACT" | Fn_json_object -> "JSON_OBJECT"
  | Fn_json_array -> "JSON_ARRAY"     | Fn_json_type   -> "JSON_TYPE"
  | Fn_json_valid -> "JSON_VALID"
```

- [ ] **Step 6: Add tokens and ident matches to `lib/sql/lexer.mll`**

In the trailing `ident as id { match String.uppercase_ascii id with ... }` block (after `| "NULLS" -> NULLS`), add:

```ocaml
      | "JSON_EXTRACT" -> JSON_EXTRACT
      | "JSON_OBJECT"  -> JSON_OBJECT_FN
      | "JSON_ARRAY"   -> JSON_ARRAY_FN
      | "JSON_TYPE"    -> JSON_TYPE
      | "JSON_VALID"   -> JSON_VALID
```

- [ ] **Step 7: Add `%token` declarations and `scalar_expr` rules to `lib/sql/parser.mly`**

After the math token line `%token SIN COS TAN ASIN ACOS ATAN ATAN2 DEGREES RADIANS` (around line 53), add:

```ocaml
%token JSON_EXTRACT JSON_OBJECT_FN JSON_ARRAY_FN JSON_TYPE JSON_VALID
```

In the `scalar_expr` rule, after the last `| RADIANS ...` line (around line 447), add:

```ocaml
  | JSON_EXTRACT LPAREN j = expr COMMA p = expr RPAREN
    { E_func (Fn_json_extract, [j; p]) }
  | JSON_OBJECT_FN LPAREN args = separated_list(COMMA, expr) RPAREN
    { E_func (Fn_json_object, args) }
  | JSON_ARRAY_FN LPAREN args = separated_list(COMMA, expr) RPAREN
    { E_func (Fn_json_array, args) }
  | JSON_TYPE LPAREN e = expr RPAREN
    { E_func (Fn_json_type, [e]) }
  | JSON_TYPE LPAREN e = expr COMMA p = expr RPAREN
    { E_func (Fn_json_type, [e; p]) }
  | JSON_VALID LPAREN e = expr RPAREN
    { E_func (Fn_json_valid, [e]) }
```

- [ ] **Step 8: Add `json_of_sql`/`sql_of_json` helpers and 5 `eval_func` cases to `lib/sql/exec.ml`**

Find `and eval_func` (around line 382). Just before this function definition (or at its beginning, before the `to_float_opt` local binding), add two top-level helper functions. Since `eval_func` is in a `let rec ... and ...` group, add them as separate `let` definitions outside that group, near the top of the relevant section. The cleanest place is immediately before the `let rec eval_expr ...` group:

```ocaml
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
```

Then in `eval_func`, after the last math function case (after `| Ast.Fn_radians, ...`), add:

```ocaml
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
        | [_]         -> []
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
     | Row.V_text s ->
       (match Json.parse s with Ok _ -> Row.V_int 1L | Error _ -> Row.V_int 0L)
     | _ -> Row.V_int 0L)
```

- [ ] **Step 9: Add comparison test cases to `test/test_sqlite_compare.ml`**

Before the final `let () = Alcotest.run` call, add:

```ocaml
let phase20_json_cases = [
  { name = "extract_int";
    setup = []; unordered = false;
    query = {|SELECT json_extract('{"a":1,"b":2}', '$.a')|} };
  { name = "extract_text";
    setup = []; unordered = false;
    query = {|SELECT json_extract('{"a":"hi"}', '$.a')|} };
  { name = "extract_array_idx";
    setup = []; unordered = false;
    query = {|SELECT json_extract('[10,20,30]', '$[1]')|} };
  { name = "extract_nested";
    setup = []; unordered = false;
    query = {|SELECT json_extract('{"a":{"b":99}}', '$.a.b')|} };
  { name = "extract_missing";
    setup = []; unordered = false;
    query = {|SELECT json_extract('{"a":1}', '$.z')|} };
  { name = "json_object_int";
    setup = []; unordered = false;
    query = {|SELECT json_object('a', 1, 'b', 2)|} };
  { name = "json_object_text";
    setup = []; unordered = false;
    query = {|SELECT json_object('k', 'hello')|} };
  { name = "json_array_ints";
    setup = []; unordered = false;
    query = {|SELECT json_array(1, 2, 3)|} };
  { name = "json_array_empty";
    setup = []; unordered = false;
    query = {|SELECT json_array()|} };
  { name = "json_type_object";
    setup = []; unordered = false;
    query = {|SELECT json_type('{"a":1}')|} };
  { name = "json_type_array";
    setup = []; unordered = false;
    query = {|SELECT json_type('[1,2]')|} };
  { name = "json_type_text";
    setup = []; unordered = false;
    query = {|SELECT json_type('"hello"')|} };
  { name = "json_type_integer";
    setup = []; unordered = false;
    query = {|SELECT json_type('42')|} };
  { name = "json_type_null";
    setup = []; unordered = false;
    query = {|SELECT json_type('null')|} };
  { name = "json_type_with_path";
    setup = []; unordered = false;
    query = {|SELECT json_type('{"a":1}', '$.a')|} };
  { name = "json_valid_true";
    setup = []; unordered = false;
    query = {|SELECT json_valid('{"a":1}')|} };
  { name = "json_valid_false";
    setup = []; unordered = false;
    query = {|SELECT json_valid('not json')|} };
]
```

In the `Alcotest.run` call, add after `"phase19_window_agg"`:
```ocaml
    "phase20_json",            List.map make_test phase20_json_cases;
```

- [ ] **Step 10: Build and run all tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
```

Expected: clean build.

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -5
```

Expected: all tests pass including the 5 new `json` tests.

```bash
podman run --rm \
  -v $(pwd):/workspace:Z \
  -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
  -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
  -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
  -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
  -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe 2>&1 | tail -10
```

Expected: 17 new `phase20_json` cases pass; no regressions.

- [ ] **Step 11: Commit Task 1**

```bash
git add lib/sql/json.ml lib/sql/json.mli lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly lib/sql/exec.ml test/test_e2e.ml test/test_sqlite_compare.ml
git commit -m "feat(phase20): json_extract, json_object, json_array, json_type, json_valid [#126]"
```

---

## Task 2: JSON mutation functions

**Implements:** `json_set`, `json_insert`, `json_replace`, `json_remove` (Issue #126 mutation subset)

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`
- Modify: `test/test_sqlite_compare.ml`

Note: `lib/sql/json.ml` already has `path_set`, `path_insert`, `path_replace`, `path_remove` from Task 1. No changes needed there.

---

- [ ] **Step 1: Write failing e2e tests for mutation functions**

Add to `test/test_e2e.ml`, after `test_json_valid`:

```ocaml
let test_json_set () =
  let db = fresh_db () in
  let r1 = query_ok db {|SELECT json_set('{"a":1}', '$.a', 99)|} in
  Alcotest.(check row_testable) "set existing key"
    [| Db.V_text {|{"a":99}|} |] (List.nth r1 0);
  let r2 = query_ok db {|SELECT json_set('{"a":1}', '$.b', 2)|} in
  Alcotest.(check row_testable) "set new key"
    [| Db.V_text {|{"a":1,"b":2}|} |] (List.nth r2 0);
  let r3 = query_ok db {|SELECT json_set('[1,2,3]', '$[1]', 99)|} in
  Alcotest.(check row_testable) "set array index"
    [| Db.V_text "[1,99,3]" |] (List.nth r3 0)

let test_json_insert () =
  let db = fresh_db () in
  let r1 = query_ok db {|SELECT json_insert('{"a":1}', '$.a', 99)|} in
  Alcotest.(check row_testable) "insert existing no-op"
    [| Db.V_text {|{"a":1}|} |] (List.nth r1 0);
  let r2 = query_ok db {|SELECT json_insert('{"a":1}', '$.b', 2)|} in
  Alcotest.(check row_testable) "insert new key"
    [| Db.V_text {|{"a":1,"b":2}|} |] (List.nth r2 0)

let test_json_replace () =
  let db = fresh_db () in
  let r1 = query_ok db {|SELECT json_replace('{"a":1}', '$.a', 99)|} in
  Alcotest.(check row_testable) "replace existing"
    [| Db.V_text {|{"a":99}|} |] (List.nth r1 0);
  let r2 = query_ok db {|SELECT json_replace('{"a":1}', '$.b', 2)|} in
  Alcotest.(check row_testable) "replace non-existing no-op"
    [| Db.V_text {|{"a":1}|} |] (List.nth r2 0)

let test_json_remove () =
  let db = fresh_db () in
  let r1 = query_ok db {|SELECT json_remove('{"a":1,"b":2}', '$.a')|} in
  Alcotest.(check row_testable) "remove object key"
    [| Db.V_text {|{"b":2}|} |] (List.nth r1 0);
  let r2 = query_ok db {|SELECT json_remove('[1,2,3]', '$[1]')|} in
  Alcotest.(check row_testable) "remove array element"
    [| Db.V_text "[1,3]" |] (List.nth r2 0)
```

Add to the `"json"` suite registration:
```ocaml
      Alcotest.test_case "json_set"     `Quick test_json_set;
      Alcotest.test_case "json_insert"  `Quick test_json_insert;
      Alcotest.test_case "json_replace" `Quick test_json_replace;
      Alcotest.test_case "json_remove"  `Quick test_json_remove;
```

- [ ] **Step 2: Add 4 new `Fn_json_*` variants to `lib/sql/ast.ml`**

After `| Fn_json_valid ...` in `type scalar_func`, add:

```ocaml
  | Fn_json_set     (** json_set(json, path, val[, path, val ...]) *)
  | Fn_json_insert  (** json_insert — insert only if absent *)
  | Fn_json_replace (** json_replace — update only if present *)
  | Fn_json_remove  (** json_remove(json, path[, path ...]) *)
```

In `func_to_sql`, after `| Fn_json_valid -> "JSON_VALID"`, add:

```ocaml
  | Fn_json_set -> "JSON_SET"       | Fn_json_insert  -> "JSON_INSERT"
  | Fn_json_replace -> "JSON_REPLACE" | Fn_json_remove -> "JSON_REMOVE"
```

- [ ] **Step 3: Add ident matches to `lib/sql/lexer.mll`**

After `| "JSON_VALID" -> JSON_VALID` in the ident match block, add:

```ocaml
      | "JSON_SET"     -> JSON_SET
      | "JSON_INSERT"  -> JSON_INSERT_FN
      | "JSON_REPLACE" -> JSON_REPLACE_FN
      | "JSON_REMOVE"  -> JSON_REMOVE
```

Note: token names `JSON_INSERT_FN` and `JSON_REPLACE_FN` are used to avoid any reader confusion with the existing `INSERT` and `REPLACE` tokens, even though there's no technical conflict.

- [ ] **Step 4: Add `%token` declarations and `scalar_expr` rules to `lib/sql/parser.mly`**

After `%token JSON_EXTRACT JSON_OBJECT_FN JSON_ARRAY_FN JSON_TYPE JSON_VALID`, add:

```ocaml
%token JSON_SET JSON_INSERT_FN JSON_REPLACE_FN JSON_REMOVE
```

In `scalar_expr`, after `| JSON_VALID ...`, add:

```ocaml
  | JSON_SET LPAREN args = separated_nonempty_list(COMMA, expr) RPAREN
    { E_func (Fn_json_set, args) }
  | JSON_INSERT_FN LPAREN args = separated_nonempty_list(COMMA, expr) RPAREN
    { E_func (Fn_json_insert, args) }
  | JSON_REPLACE_FN LPAREN args = separated_nonempty_list(COMMA, expr) RPAREN
    { E_func (Fn_json_replace, args) }
  | JSON_REMOVE LPAREN args = separated_nonempty_list(COMMA, expr) RPAREN
    { E_func (Fn_json_remove, args) }
```

- [ ] **Step 5: Add 4 `eval_func` cases to `lib/sql/exec.ml`**

After the `| Ast.Fn_json_valid, ...` case, add:

```ocaml
  | Ast.Fn_json_set, json_v :: rest ->
    let json_s = (match json_v with Row.V_text s -> s | _ -> "{}") in
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
    let json_s = (match json_v with Row.V_text s -> s | _ -> "{}") in
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
    let json_s = (match json_v with Row.V_text s -> s | _ -> "{}") in
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
    let json_s = (match json_v with Row.V_text s -> s | _ -> "{}") in
    (match Json.parse json_s with
     | Error _ -> Row.V_null
     | Ok jv ->
       let result = List.fold_left (fun acc path_v ->
         let path = (match path_v with Row.V_text s -> s | _ -> "") in
         Json.path_remove acc path
       ) jv paths in
       Row.V_text (Json.to_string result))
```

- [ ] **Step 6: Add mutation comparison test cases to `test/test_sqlite_compare.ml`**

After `phase20_json_cases`, add:

```ocaml
let phase20_json_mutation_cases = [
  { name = "json_set_existing";
    setup = []; unordered = false;
    query = {|SELECT json_set('{"a":1}', '$.a', 99)|} };
  { name = "json_set_new";
    setup = []; unordered = false;
    query = {|SELECT json_set('{"a":1}', '$.b', 2)|} };
  { name = "json_insert_existing_noop";
    setup = []; unordered = false;
    query = {|SELECT json_insert('{"a":1}', '$.a', 99)|} };
  { name = "json_insert_new";
    setup = []; unordered = false;
    query = {|SELECT json_insert('{"a":1}', '$.b', 2)|} };
  { name = "json_replace_existing";
    setup = []; unordered = false;
    query = {|SELECT json_replace('{"a":1}', '$.a', 99)|} };
  { name = "json_replace_missing_noop";
    setup = []; unordered = false;
    query = {|SELECT json_replace('{"a":1}', '$.b', 2)|} };
  { name = "json_remove_key";
    setup = []; unordered = false;
    query = {|SELECT json_remove('{"a":1,"b":2}', '$.a')|} };
  { name = "json_remove_array_elem";
    setup = []; unordered = false;
    query = {|SELECT json_remove('[1,2,3]', '$[1]')|} };
]
```

Add to the `Alcotest.run` call after `"phase20_json"`:
```ocaml
    "phase20_json_mut",        List.map make_test phase20_json_mutation_cases;
```

- [ ] **Step 7: Build and run all tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
```

Expected: clean build.

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -5
```

Expected: all tests pass including the 4 new json mutation tests.

```bash
podman run --rm \
  -v $(pwd):/workspace:Z \
  -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
  -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
  -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
  -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
  -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe 2>&1 | tail -10
```

Expected: 8 new `phase20_json_mut` cases pass; no regressions.

- [ ] **Step 8: Commit Task 2**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly lib/sql/exec.ml test/test_e2e.ml test/test_sqlite_compare.ml
git commit -m "feat(phase20): json_set, json_insert, json_replace, json_remove [#126]"
```
