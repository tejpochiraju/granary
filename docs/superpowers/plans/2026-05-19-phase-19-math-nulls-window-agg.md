# Phase 19: Math Functions, NULLS FIRST/LAST, Window Functions in Aggregated Queries

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add math scalar functions (Issue #123), NULLS FIRST/LAST in ORDER BY (Issue #124), and window functions in aggregated queries (Issue #125).

**Architecture:** Three sequential tasks. Tasks 1 and 2 are additive (new functions + one type field). Task 3 extends Op_aggregate to run window functions inline on its output rows, adds `AP_window_slot`/`PI_window_slot` variants, and adds `agg_windows : window_sema list` to BS_select. All tasks touch the AST→Sema→Plan→Exec pipeline.

**Tech Stack:** OCaml 5.x, dune 3.x, menhir, alcotest, lwt, podman container build.

**Build commands (ALWAYS use podman):**
```bash
# Run e2e tests:
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe

# Run sqlite comparison tests (requires sqlite3 on host):
podman run --rm \
  -v $(pwd):/workspace:Z \
  -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
  -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
  -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
  -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
  -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe

# Build only (check compilation):
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build
```

**Baseline:** 308 e2e tests + 263 sqlite comparison tests = 571 total, all passing.

---

## Task 1: Math scalar functions

Adds 21 new scalar functions: CEIL, FLOOR, SQRT, POW, EXP, LN, LOG, LOG2, LOG10, SIGN, TRUNC, PI, SIN, COS, TAN, ASIN, ACOS, ATAN, ATAN2, DEGREES, RADIANS.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`
- Modify: `test/test_sqlite_compare.ml`

- [ ] **Step 1: Write failing tests in `test/test_e2e.ml`**

Add these tests before the final `let () =` runner (insert after the last `test_*` definition). Each test uses `Db.open_block (fun db -> Db.execute db sql >>= ...)` where `Db` is `Sqlocaml_db.Db`.

```ocaml
let test_math_ceil () =
  Db.open_block (fun db ->
    let* rows = Db.execute db "SELECT CEIL(2.3)" in
    Alcotest.check (Alcotest.list row_testable) "ceil(2.3)=3.0"
      [[| Db.V_real 3.0 |]] rows;
    let* rows2 = Db.execute db "SELECT CEIL(-2.3)" in
    Alcotest.check (Alcotest.list row_testable) "ceil(-2.3)=-2.0"
      [[| Db.V_real (-2.0) |]] rows2;
    Lwt.return_unit)

let test_math_floor () =
  Db.open_block (fun db ->
    let* rows = Db.execute db "SELECT FLOOR(2.7)" in
    Alcotest.check (Alcotest.list row_testable) "floor(2.7)=2.0"
      [[| Db.V_real 2.0 |]] rows;
    let* rows2 = Db.execute db "SELECT FLOOR(-2.7)" in
    Alcotest.check (Alcotest.list row_testable) "floor(-2.7)=-3.0"
      [[| Db.V_real (-3.0) |]] rows2;
    Lwt.return_unit)

let test_math_sqrt () =
  Db.open_block (fun db ->
    let* rows = Db.execute db "SELECT SQRT(4.0)" in
    Alcotest.check (Alcotest.list row_testable) "sqrt(4.0)=2.0"
      [[| Db.V_real 2.0 |]] rows;
    Lwt.return_unit)

let test_math_pow () =
  Db.open_block (fun db ->
    let* rows = Db.execute db "SELECT POW(2.0, 10.0)" in
    Alcotest.check (Alcotest.list row_testable) "pow(2,10)=1024"
      [[| Db.V_real 1024.0 |]] rows;
    Lwt.return_unit)

let test_math_sign () =
  Db.open_block (fun db ->
    let* r1 = Db.execute db "SELECT SIGN(-5)" in
    Alcotest.check (Alcotest.list row_testable) "sign(-5)=-1" [[| Db.V_int (-1L) |]] r1;
    let* r2 = Db.execute db "SELECT SIGN(0)" in
    Alcotest.check (Alcotest.list row_testable) "sign(0)=0" [[| Db.V_int 0L |]] r2;
    let* r3 = Db.execute db "SELECT SIGN(5)" in
    Alcotest.check (Alcotest.list row_testable) "sign(5)=1" [[| Db.V_int 1L |]] r3;
    Lwt.return_unit)

let test_math_trunc () =
  Db.open_block (fun db ->
    let* r1 = Db.execute db "SELECT TRUNC(3.7)" in
    Alcotest.check (Alcotest.list row_testable) "trunc(3.7)=3.0" [[| Db.V_real 3.0 |]] r1;
    let* r2 = Db.execute db "SELECT TRUNC(-3.7)" in
    Alcotest.check (Alcotest.list row_testable) "trunc(-3.7)=-3.0" [[| Db.V_real (-3.0) |]] r2;
    Lwt.return_unit)

let test_math_pi () =
  Db.open_block (fun db ->
    let* rows = Db.execute db "SELECT PI()" in
    (match rows with
     | [[| Db.V_real v |]] ->
       let diff = abs_float (v -. Float.pi) in
       Alcotest.(check bool) "pi close to Float.pi" true (diff < 1e-10)
     | _ -> Alcotest.fail "expected single real row");
    Lwt.return_unit)

let test_math_trig () =
  Db.open_block (fun db ->
    let* r1 = Db.execute db "SELECT SIN(0.0)" in
    Alcotest.check (Alcotest.list row_testable) "sin(0)=0" [[| Db.V_real 0.0 |]] r1;
    let* r2 = Db.execute db "SELECT COS(0.0)" in
    Alcotest.check (Alcotest.list row_testable) "cos(0)=1" [[| Db.V_real 1.0 |]] r2;
    let* r3 = Db.execute db "SELECT TAN(0.0)" in
    Alcotest.check (Alcotest.list row_testable) "tan(0)=0" [[| Db.V_real 0.0 |]] r3;
    Lwt.return_unit)

let test_math_log () =
  Db.open_block (fun db ->
    let* r1 = Db.execute db "SELECT LOG2(8.0)" in
    Alcotest.check (Alcotest.list row_testable) "log2(8)=3" [[| Db.V_real 3.0 |]] r1;
    let* r2 = Db.execute db "SELECT LOG10(1000.0)" in
    Alcotest.check (Alcotest.list row_testable) "log10(1000)=3" [[| Db.V_real 3.0 |]] r2;
    let* r3 = Db.execute db "SELECT LOG(10.0, 100.0)" in
    (match r3 with
     | [[| Db.V_real v |]] -> Alcotest.(check bool) "log(10,100)≈2" true (abs_float (v -. 2.0) < 1e-10)
     | _ -> Alcotest.fail "expected real");
    Lwt.return_unit)
```

Register them in the `let () =` runner block (find the `Alcotest.run` call and add):

```ocaml
      Alcotest.test_case "math_ceil"  `Quick (fun () -> Lwt_main.run (test_math_ceil ()));
      Alcotest.test_case "math_floor" `Quick (fun () -> Lwt_main.run (test_math_floor ()));
      Alcotest.test_case "math_sqrt"  `Quick (fun () -> Lwt_main.run (test_math_sqrt ()));
      Alcotest.test_case "math_pow"   `Quick (fun () -> Lwt_main.run (test_math_pow ()));
      Alcotest.test_case "math_sign"  `Quick (fun () -> Lwt_main.run (test_math_sign ()));
      Alcotest.test_case "math_trunc" `Quick (fun () -> Lwt_main.run (test_math_trunc ()));
      Alcotest.test_case "math_pi"    `Quick (fun () -> Lwt_main.run (test_math_pi ()));
      Alcotest.test_case "math_trig"  `Quick (fun () -> Lwt_main.run (test_math_trig ()));
      Alcotest.test_case "math_log"   `Quick (fun () -> Lwt_main.run (test_math_log ()));
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | grep -E "FAIL|Error|math"
```

Expected: build failure because `Fn_ceil` etc. don't exist yet.

- [ ] **Step 3: Add 21 new `scalar_func` variants to `lib/sql/ast.ml`**

In `lib/sql/ast.ml`, locate the `scalar_func` type (currently ends with `| Fn_unixepoch`). Add after `Fn_unixepoch`:

```ocaml
  | Fn_ceil          (** CEIL(x) / CEILING(x) — round up *)
  | Fn_floor         (** FLOOR(x) — round down *)
  | Fn_sqrt          (** SQRT(x) — square root *)
  | Fn_pow           (** POW(x,y) / POWER(x,y) — x^y *)
  | Fn_exp           (** EXP(x) — e^x *)
  | Fn_ln            (** LN(x) — natural log *)
  | Fn_log           (** LOG(x) → ln x; LOG(B,x) → log base B of x *)
  | Fn_log2          (** LOG2(x) — log base 2 *)
  | Fn_log10         (** LOG10(x) — log base 10 *)
  | Fn_sign          (** SIGN(x) → -1 | 0 | 1 as integer *)
  | Fn_trunc         (** TRUNC(x[,d]) — truncate toward zero *)
  | Fn_pi            (** PI() — constant π, zero args *)
  | Fn_sin           (** SIN(x) *)
  | Fn_cos           (** COS(x) *)
  | Fn_tan           (** TAN(x) *)
  | Fn_asin          (** ASIN(x) *)
  | Fn_acos          (** ACOS(x) *)
  | Fn_atan          (** ATAN(x) *)
  | Fn_atan2         (** ATAN2(y,x) — two-argument arctangent *)
  | Fn_degrees       (** DEGREES(x) — radians to degrees *)
  | Fn_radians       (** RADIANS(x) — degrees to radians *)
```

Also update `func_to_sql` (used by CHECK constraint serializer) by appending at the end of the match:

```ocaml
  | Fn_ceil -> "CEIL" | Fn_floor -> "FLOOR" | Fn_sqrt -> "SQRT"
  | Fn_pow -> "POW" | Fn_exp -> "EXP" | Fn_ln -> "LN"
  | Fn_log -> "LOG" | Fn_log2 -> "LOG2" | Fn_log10 -> "LOG10"
  | Fn_sign -> "SIGN" | Fn_trunc -> "TRUNC" | Fn_pi -> "PI"
  | Fn_sin -> "SIN" | Fn_cos -> "COS" | Fn_tan -> "TAN"
  | Fn_asin -> "ASIN" | Fn_acos -> "ACOS" | Fn_atan -> "ATAN"
  | Fn_atan2 -> "ATAN2" | Fn_degrees -> "DEGREES" | Fn_radians -> "RADIANS"
```

- [ ] **Step 4: Add token declarations and lex rules to `lib/sql/lexer.mll`**

Add to the `%token` declarations section in `lib/sql/parser.mly` (see Step 5). In `lib/sql/lexer.mll`, extend the ident match block (currently ends with `| "FOLLOWING" -> FOLLOWING`). Add before `| _ -> IDENT id`:

```ocaml
      | "CEIL" | "CEILING" -> CEIL
      | "FLOOR" -> FLOOR
      | "SQRT" -> SQRT
      | "POW" | "POWER" -> POW
      | "EXP" -> EXP
      | "LN" -> LN
      | "LOG" -> LOG
      | "LOG2" -> LOG2
      | "LOG10" -> LOG10
      | "SIGN" -> SIGN
      | "TRUNC" | "TRUNCATE" -> TRUNC
      | "PI" -> PI
      | "SIN" -> SIN
      | "COS" -> COS
      | "TAN" -> TAN
      | "ASIN" -> ASIN
      | "ACOS" -> ACOS
      | "ATAN" -> ATAN
      | "ATAN2" -> ATAN2
      | "DEGREES" -> DEGREES
      | "RADIANS" -> RADIANS
```

These go in the `match String.uppercase_ascii id with` block. Case-insensitive since we uppercase before matching.

- [ ] **Step 5: Add `%token` declarations and `scalar_expr` rules to `lib/sql/parser.mly`**

Add to the `%token` section (near existing scalar function tokens):

```
%token CEIL FLOOR SQRT POW EXP LN LOG LOG2 LOG10 SIGN TRUNC PI
%token SIN COS TAN ASIN ACOS ATAN ATAN2 DEGREES RADIANS
```

In the `scalar_expr` rule, append these alternatives (the order doesn't matter; LOG must come before LOG2/LOG10 for correct disambiguation by menhir, but since they differ in the token itself it's fine):

```
  | CEIL    LPAREN e = expr RPAREN { E_func (Fn_ceil,    [e]) }
  | FLOOR   LPAREN e = expr RPAREN { E_func (Fn_floor,   [e]) }
  | SQRT    LPAREN e = expr RPAREN { E_func (Fn_sqrt,    [e]) }
  | POW     LPAREN b = expr COMMA e = expr RPAREN { E_func (Fn_pow,   [b; e]) }
  | EXP     LPAREN e = expr RPAREN { E_func (Fn_exp,     [e]) }
  | LN      LPAREN e = expr RPAREN { E_func (Fn_ln,      [e]) }
  | LOG     LPAREN e = expr RPAREN { E_func (Fn_log,     [e]) }
  | LOG     LPAREN b = expr COMMA x = expr RPAREN { E_func (Fn_log,  [b; x]) }
  | LOG2    LPAREN e = expr RPAREN { E_func (Fn_log2,    [e]) }
  | LOG10   LPAREN e = expr RPAREN { E_func (Fn_log10,   [e]) }
  | SIGN    LPAREN e = expr RPAREN { E_func (Fn_sign,    [e]) }
  | TRUNC   LPAREN e = expr RPAREN { E_func (Fn_trunc,   [e]) }
  | TRUNC   LPAREN e = expr COMMA d = expr RPAREN { E_func (Fn_trunc, [e; d]) }
  | PI      LPAREN RPAREN         { E_func (Fn_pi,       []) }
  | SIN     LPAREN e = expr RPAREN { E_func (Fn_sin,     [e]) }
  | COS     LPAREN e = expr RPAREN { E_func (Fn_cos,     [e]) }
  | TAN     LPAREN e = expr RPAREN { E_func (Fn_tan,     [e]) }
  | ASIN    LPAREN e = expr RPAREN { E_func (Fn_asin,    [e]) }
  | ACOS    LPAREN e = expr RPAREN { E_func (Fn_acos,    [e]) }
  | ATAN    LPAREN e = expr RPAREN { E_func (Fn_atan,    [e]) }
  | ATAN2   LPAREN y = expr COMMA x = expr RPAREN { E_func (Fn_atan2, [y; x]) }
  | DEGREES LPAREN e = expr RPAREN { E_func (Fn_degrees, [e]) }
  | RADIANS LPAREN e = expr RPAREN { E_func (Fn_radians, [e]) }
```

- [ ] **Step 6: Add eval_func cases to `lib/sql/exec.ml`**

In `lib/sql/exec.ml`, find `eval_func`. Add a helper `let to_float_opt` at the top of the function body (before the `match func, args with`):

```ocaml
and eval_func (clock : (unit -> float) option) (func : Ast.scalar_func) (args : Row.value list) : Row.value =
  let to_float_opt = function
    | Row.V_real f -> Some f
    | Row.V_int n  -> Some (Int64.to_float n)
    | _            -> None
  in
  match func, args with
```

Then add these cases just before the final catch-all `| _ -> failwith ...` (line ~501):

```ocaml
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
```

- [ ] **Step 7: Add sqlite comparison test cases in `test/test_sqlite_compare.ml`**

Find the pattern where test case lists are defined (e.g. `phase18_*_cases`). Add:

```ocaml
let phase19_math_cases = [
  ("ceil pos",    "SELECT CEIL(2.3)");
  ("ceil neg",    "SELECT CEIL(-2.3)");
  ("floor pos",   "SELECT FLOOR(2.7)");
  ("floor neg",   "SELECT FLOOR(-2.7)");
  ("sqrt exact",  "SELECT SQRT(4.0)");
  ("pow exact",   "SELECT POW(2.0, 10.0)");
  ("sign neg",    "SELECT SIGN(-5)");
  ("sign zero",   "SELECT SIGN(0)");
  ("sign pos",    "SELECT SIGN(5)");
  ("trunc pos",   "SELECT TRUNC(3.7)");
  ("trunc neg",   "SELECT TRUNC(-3.7)");
  ("log2 exact",  "SELECT LOG2(8.0)");
  ("log10 exact", "SELECT LOG10(1000.0)");
  ("sin zero",    "SELECT SIN(0.0)");
  ("cos zero",    "SELECT COS(0.0)");
  ("tan zero",    "SELECT TAN(0.0)");
  ("exp zero",    "SELECT EXP(0.0)");
  ("ln one",      "SELECT LN(1.0)");
  ("ceil alias",  "SELECT CEILING(2.3)");
  ("pow alias",   "SELECT POWER(2.0, 3.0)");
  ("trunc alias", "SELECT TRUNCATE(3.9)");
]
```

Register them (find the pattern `run_cases "phase18..." phase18_*_cases`):

```ocaml
  run_cases "phase19_math" phase19_math_cases;
```

- [ ] **Step 8: Run all tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -5
```

Expected: 317+ tests passing (9 new e2e math tests).

```bash
podman run --rm \
  -v $(pwd):/workspace:Z \
  -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
  -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
  -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
  -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
  -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe 2>&1 | tail -5
```

Expected: 21 new comparison tests passing.

- [ ] **Step 9: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly lib/sql/exec.ml \
        test/test_e2e.ml test/test_sqlite_compare.ml
git commit -m "feat(phase19): add 21 math scalar functions (CEIL, FLOOR, SQRT, POW, trig, etc.) [#123]"
```

---

## Task 2: NULLS FIRST/LAST in ORDER BY

Adds `nulls : [`Nulls_first | `Nulls_last] option` to `Ast.order_key`, `NULLS FIRST` / `NULLS LAST` syntax, and sort comparison that respects the explicit directive.

**Default behavior (unchanged):** ASC → NULLs first (smallest), DESC → NULLs last (same as SQLite). The explicit `NULLS FIRST` / `NULLS LAST` overrides the default.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml` and `lib/sql/sema.mli`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`
- Modify: `test/test_sqlite_compare.ml`

- [ ] **Step 1: Write failing tests in `test/test_e2e.ml`**

```ocaml
let test_nulls_first_last () =
  Db.open_block (fun db ->
    let* () = Db.execute_change_count db
      "CREATE TABLE tnull (x INTEGER)" >>= fun _ -> Lwt.return_unit in
    let* () = Db.execute_change_count db "INSERT INTO tnull VALUES (1)" >>= fun _ -> Lwt.return_unit in
    let* () = Db.execute_change_count db "INSERT INTO tnull VALUES (NULL)" >>= fun _ -> Lwt.return_unit in
    let* () = Db.execute_change_count db "INSERT INTO tnull VALUES (3)" >>= fun _ -> Lwt.return_unit in
    (* ASC NULLS LAST: 1, 3, NULL *)
    let* r1 = Db.execute db "SELECT x FROM tnull ORDER BY x ASC NULLS LAST" in
    Alcotest.check (Alcotest.list row_testable) "asc nulls last"
      [[| Db.V_int 1L |]; [| Db.V_int 3L |]; [| Db.V_null |]] r1;
    (* ASC NULLS FIRST (default, explicit): NULL, 1, 3 *)
    let* r2 = Db.execute db "SELECT x FROM tnull ORDER BY x ASC NULLS FIRST" in
    Alcotest.check (Alcotest.list row_testable) "asc nulls first"
      [[| Db.V_null |]; [| Db.V_int 1L |]; [| Db.V_int 3L |]] r2;
    (* DESC NULLS FIRST: NULL, 3, 1 *)
    let* r3 = Db.execute db "SELECT x FROM tnull ORDER BY x DESC NULLS FIRST" in
    Alcotest.check (Alcotest.list row_testable) "desc nulls first"
      [[| Db.V_null |]; [| Db.V_int 3L |]; [| Db.V_int 1L |]] r3;
    (* DESC NULLS LAST (default, explicit): 3, 1, NULL *)
    let* r4 = Db.execute db "SELECT x FROM tnull ORDER BY x DESC NULLS LAST" in
    Alcotest.check (Alcotest.list row_testable) "desc nulls last"
      [[| Db.V_int 3L |]; [| Db.V_int 1L |]; [| Db.V_null |]] r4;
    Lwt.return_unit)
```

Register:
```ocaml
      Alcotest.test_case "nulls_first_last" `Quick (fun () -> Lwt_main.run (test_nulls_first_last ()));
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -20
```

Expected: build error (NULLS, FIRST, LAST are unknown).

- [ ] **Step 3: Add `nulls` field to `Ast.order_key` in `lib/sql/ast.ml`**

Find the `order_key` type definition:
```ocaml
and order_key = {
  expr : expr;
  dir  : order_dir;
}
```

Change to:
```ocaml
and order_key = {
  expr  : expr;
  dir   : order_dir;
  nulls : [`Nulls_first | `Nulls_last] option;
}
```

- [ ] **Step 4: Add NULLS/FIRST/LAST tokens to `lib/sql/lexer.mll`**

In the ident match block (the `match String.uppercase_ascii id with` section), add before `| _ -> IDENT id`:

```ocaml
      | "NULLS" -> NULLS
      | "FIRST" -> FIRST
      | "LAST"  -> LAST
```

- [ ] **Step 5: Add tokens and update `order_key` rule in `lib/sql/parser.mly`**

Add to `%token` section:
```
%token NULLS FIRST LAST
```

Find the `order_key` rule:
```
order_key:
  | e = expr      { { expr = e; dir = Asc } }
  | e = expr ASC  { { expr = e; dir = Asc } }
  | e = expr DESC { { expr = e; dir = Desc } }
```

Replace with:
```
order_key:
  | e = expr                             { { expr = e; dir = Asc;  nulls = None } }
  | e = expr ASC                         { { expr = e; dir = Asc;  nulls = None } }
  | e = expr DESC                        { { expr = e; dir = Desc; nulls = None } }
  | e = expr NULLS FIRST                 { { expr = e; dir = Asc;  nulls = Some `Nulls_first } }
  | e = expr NULLS LAST                  { { expr = e; dir = Asc;  nulls = Some `Nulls_last  } }
  | e = expr ASC  NULLS FIRST            { { expr = e; dir = Asc;  nulls = Some `Nulls_first } }
  | e = expr ASC  NULLS LAST             { { expr = e; dir = Asc;  nulls = Some `Nulls_last  } }
  | e = expr DESC NULLS FIRST            { { expr = e; dir = Desc; nulls = Some `Nulls_first } }
  | e = expr DESC NULLS LAST             { { expr = e; dir = Desc; nulls = Some `Nulls_last  } }
```

- [ ] **Step 6: Add `nulls` to `bound_order_key` in `lib/sql/sema.mli` and `lib/sql/sema.ml`**

In `lib/sql/sema.mli`, find:
```ocaml
type bound_order_key = {
  key : bound_expr;
  dir : Ast.order_dir;
}
```
Change to:
```ocaml
type bound_order_key = {
  key   : bound_expr;
  dir   : Ast.order_dir;
  nulls : [`Nulls_first | `Nulls_last] option;
}
```

In `lib/sql/sema.ml`, find the same type definition (near line 40) and apply the same change.

Now update the two places in `sema.ml` that construct `bound_order_key` records:

1. In `bind_ww` (around line 1360), find:
```ocaml
   Ok (acc @ [{ key = be; dir = ok.Ast.dir }])
```
Change to:
```ocaml
   Ok (acc @ [{ key = be; dir = ok.Ast.dir; nulls = ok.Ast.nulls }])
```

2. In `bind_select` (around line 1725), find:
```ocaml
   | Ok key  -> Ok (keys @ [{ key; dir = ok.Ast.dir }]))
```
Change to:
```ocaml
   | Ok key  -> Ok (keys @ [{ key; dir = ok.Ast.dir; nulls = ok.Ast.nulls }]))
```

- [ ] **Step 7: Update plan sort key types in `lib/sql/plan.ml`**

In `lib/sql/plan.ml`, find the two places that use `(expr * [`Asc | `Desc]) list`:

1. In `window_plan_item`:
```ocaml
  order_by     : (expr * [`Asc | `Desc]) list;
```
Change to:
```ocaml
  order_by     : (expr * [`Asc | `Desc] * [`Nulls_first | `Nulls_last]) list;
```

2. In `Op_sort`:
```ocaml
  | Op_sort of {
      keys  : (expr * [`Asc | `Desc]) list;
```
Change to:
```ocaml
  | Op_sort of {
      keys  : (expr * [`Asc | `Desc] * [`Nulls_first | `Nulls_last]) list;
```

- [ ] **Step 8: Update `lib/sql/planner.ml` to propagate nulls**

In `lib/sql/planner.ml`, find `plan_window_item`:
```ocaml
    order_by     = List.map (fun (bk : Sema.bound_order_key) ->
      let dir = match bk.Sema.dir with Ast.Asc -> `Asc | Ast.Desc -> `Desc in
      (plan_expr bk.Sema.key, dir)
    ) ws.Sema.order_by;
```
Change to:
```ocaml
    order_by     = List.map (fun (bk : Sema.bound_order_key) ->
      let dir = match bk.Sema.dir with Ast.Asc -> `Asc | Ast.Desc -> `Desc in
      let nulls = match bk.Sema.nulls with
        | Some `Nulls_first -> `Nulls_first
        | Some `Nulls_last  -> `Nulls_last
        | None -> (match dir with `Asc -> `Nulls_first | `Desc -> `Nulls_last)
      in
      (plan_expr bk.Sema.key, dir, nulls)
    ) ws.Sema.order_by;
```

Find `make_sort_keys`:
```ocaml
  let make_sort_keys () =
    List.map (fun (bkey : Sema.bound_order_key) ->
      let dir = match bkey.dir with Ast.Asc -> `Asc | Ast.Desc -> `Desc in
      let e = plan_expr bkey.key in
      let e' = if windows = [] then e
               else substitute_window_slots ~n_input_cols e in
      (e', dir)
    ) order
```
Change to:
```ocaml
  let make_sort_keys () =
    List.map (fun (bkey : Sema.bound_order_key) ->
      let dir = match bkey.dir with Ast.Asc -> `Asc | Ast.Desc -> `Desc in
      let nulls = match bkey.nulls with
        | Some `Nulls_first -> `Nulls_first
        | Some `Nulls_last  -> `Nulls_last
        | None -> (match dir with `Asc -> `Nulls_first | `Desc -> `Nulls_last)
      in
      let e = plan_expr bkey.key in
      let e' = if windows = [] then e
               else substitute_window_slots ~n_input_cols e in
      (e', dir, nulls)
    ) order
```

- [ ] **Step 9: Update `lib/sql/exec.ml` sort comparison**

In `lib/sql/exec.ml`, add a new helper function after `compare_values` (around line 43):

```ocaml
let compare_with_nulls (dir : [`Asc | `Desc]) (nulls : [`Nulls_first | `Nulls_last])
    (va : Row.value) (vb : Row.value) : int =
  match va, vb with
  | Row.V_null, Row.V_null -> 0
  | Row.V_null, _ -> (match nulls with `Nulls_first -> -1 | `Nulls_last -> 1)
  | _, Row.V_null -> (match nulls with `Nulls_first -> 1 | `Nulls_last -> -1)
  | _, _ ->
    let c = compare_values va vb in
    (match dir with `Asc -> c | `Desc -> -c)
```

Update `Op_sort` handler (around line 2428):
```ocaml
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
```

Update `sort_partition_by` (around line 2065):
```ocaml
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
```

Update window function peer detection (all places that use `wplan.Plan.order_by` for peer comparison). Find lines like:
```ocaml
let order_changed = List.exists (fun (e, _) ->
  compare_values
    (eval_expr clock params sorted_rows.(pos)   e)
    (eval_expr clock params sorted_rows.(pos-1) e) <> 0
) wplan.Plan.order_by in
```

There are multiple such occurrences (for WF_rank, WF_dense_rank, WF_percent_rank, WF_cume_dist). Change each to:
```ocaml
let order_changed = List.exists (fun (e, dir, nulls) ->
  compare_with_nulls dir nulls
    (eval_expr clock params sorted_rows.(pos)   e)
    (eval_expr clock params sorted_rows.(pos-1) e) <> 0
) wplan.Plan.order_by in
```

Also update the WF_agg frame bound evaluation (around line 2247):
```ocaml
) wplan.Plan.order_by
```
Any tuple-destructure of order_by items with `(e, dir)` patterns need to become `(e, dir, _nulls)`.

- [ ] **Step 10: Add sqlite comparison test cases in `test/test_sqlite_compare.ml`**

```ocaml
let phase19_nulls_cases =
  let setup = "CREATE TABLE tn (x INTEGER); INSERT INTO tn VALUES (1); INSERT INTO tn VALUES (NULL); INSERT INTO tn VALUES (3); " in
  [
    ("nulls last asc",   setup ^ "SELECT x FROM tn ORDER BY x ASC NULLS LAST");
    ("nulls first asc",  setup ^ "SELECT x FROM tn ORDER BY x ASC NULLS FIRST");
    ("nulls first desc", setup ^ "SELECT x FROM tn ORDER BY x DESC NULLS FIRST");
    ("nulls last desc",  setup ^ "SELECT x FROM tn ORDER BY x DESC NULLS LAST");
  ]
```

Register:
```ocaml
  run_cases "phase19_nulls" phase19_nulls_cases;
```

- [ ] **Step 11: Run all tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -5
```

Expected: all prior tests still pass plus the new nulls test.

- [ ] **Step 12: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/sema.ml lib/sql/sema.mli lib/sql/plan.ml lib/sql/planner.ml lib/sql/exec.ml \
        test/test_e2e.ml test/test_sqlite_compare.ml
git commit -m "feat(phase19): NULLS FIRST/LAST in ORDER BY [#124]"
```

---

## Task 3: Window functions in aggregated queries

Allows window functions in the SELECT clause of a GROUP BY query. Example:
```sql
SELECT dept, SUM(sal), RANK() OVER (ORDER BY SUM(sal) DESC)
FROM emp GROUP BY dept;
```

**Architecture:** Add `AP_window_slot of int` to `agg_proj_item` and `PI_window_slot of int` to `proj_item`. Add `agg_windows : window_sema list` to `BS_select`. Add `windows : window_plan_item list` to `Op_aggregate`. In exec.ml, Op_aggregate computes inline window functions on its post-HAVING output before projecting.

Post-agg context column layout: `[group_col0, ..., group_colN-1, agg0, ..., aggM-1]`. Window functions in this context reference columns by that layout (group cols by name → pos in [0,N), agg results by E_agg expression → pos N+k).

**Files:**
- Modify: `lib/sql/sema.ml` and `lib/sql/sema.mli`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`
- Modify: `test/test_sqlite_compare.ml`

- [ ] **Step 1: Write failing tests in `test/test_e2e.ml`**

```ocaml
let test_window_in_agg () =
  Db.open_block (fun db ->
    let* () = Db.execute_change_count db
      "CREATE TABLE wagg (dept TEXT, sal INTEGER)" >>= fun _ -> Lwt.return_unit in
    let* () = Db.execute_change_count db
      "INSERT INTO wagg VALUES ('eng', 100), ('eng', 200), ('mkt', 150), ('mkt', 50)"
      >>= fun _ -> Lwt.return_unit in
    (* RANK() OVER (ORDER BY SUM(sal) DESC) — eng=300 rank 1, mkt=200 rank 2 *)
    let* rows = Db.execute db
      "SELECT dept, SUM(sal), RANK() OVER (ORDER BY SUM(sal) DESC) FROM wagg GROUP BY dept ORDER BY dept" in
    Alcotest.check (Alcotest.list row_testable) "rank over sum"
      [ [| Db.V_text "eng"; Db.V_int 300L; Db.V_int 1L |];
        [| Db.V_text "mkt"; Db.V_int 200L; Db.V_int 2L |] ]
      rows;
    Lwt.return_unit)

let test_window_in_agg_count () =
  Db.open_block (fun db ->
    let* () = Db.execute_change_count db
      "CREATE TABLE wagg2 (dept TEXT, sal INTEGER)" >>= fun _ -> Lwt.return_unit in
    let* () = Db.execute_change_count db
      "INSERT INTO wagg2 VALUES ('eng', 100), ('eng', 200), ('mkt', 150)"
      >>= fun _ -> Lwt.return_unit in
    let* rows = Db.execute db
      "SELECT dept, COUNT(*), DENSE_RANK() OVER (ORDER BY COUNT(*) DESC) FROM wagg2 GROUP BY dept ORDER BY dept" in
    Alcotest.check (Alcotest.list row_testable) "dense_rank over count"
      [ [| Db.V_text "eng"; Db.V_int 2L; Db.V_int 1L |];
        [| Db.V_text "mkt"; Db.V_int 1L; Db.V_int 2L |] ]
      rows;
    Lwt.return_unit)
```

Register:
```ocaml
      Alcotest.test_case "window_in_agg"       `Quick (fun () -> Lwt_main.run (test_window_in_agg ()));
      Alcotest.test_case "window_in_agg_count" `Quick (fun () -> Lwt_main.run (test_window_in_agg_count ()));
```

- [ ] **Step 2: Run to verify failure**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | grep -E "window_in_agg|FAIL"
```

Expected: test failures (window functions return Unsupported in aggregate context).

- [ ] **Step 3: Add `AP_window_slot` to `agg_proj_item` in `lib/sql/sema.mli` and `lib/sql/sema.ml`**

In `lib/sql/sema.mli`, find:
```ocaml
type agg_proj_item =
  | AP_group_col of int       (** project the i-th GROUP BY column (index into group_cols list) *)
  | AP_agg_slot of int        (** project the [i]-th aggregate result from the aggregate output *)
```
Add:
```ocaml
type agg_proj_item =
  | AP_group_col of int       (** project the i-th GROUP BY column (index into group_cols list) *)
  | AP_agg_slot of int        (** project the [i]-th aggregate result from the aggregate output *)
  | AP_window_slot of int     (** project the [i]-th post-aggregate window result *)
```

Also add `agg_windows : window_sema list` to `BS_select` in `sema.mli`:
```ocaml
  | BS_select of {
      ...
      windows    : window_sema list;
      agg_windows : window_sema list;
        (** Window functions computed AFTER aggregation, over aggregated rows. *)
    }
```

Apply the same changes in `lib/sql/sema.ml`:
- Same `agg_proj_item` type update (around line 58)
- Same `BS_select` record extension (add `agg_windows : window_sema list` field)

- [ ] **Step 4: Update `agg_proj_item` in `lib/sql/sema.ml` and add `bind_post_agg_expr`**

In `lib/sql/sema.ml`, inside the `bind_select` function, in the **aggregated branch** (the `begin ... end` block starting around line 1497), make these changes:

**a)** Add `agg_windows_queue` alongside `acc_aggs`:
```ocaml
let acc_aggs = ref [] in
let agg_windows_queue : window_sema Queue.t = Queue.create () in
```

**b)** In `project_one`, add a case for `Ast.E_window` before the catch-all `| _ ->` case:
```ocaml
            | Ast.E_window { func; args; window } ->
              (* Bind args in post-agg context where:
                 - E_col name resolves to its position in the group-by output (0..N-1)
                 - E_agg resolves to position N+k in the aggregated output row *)
              let bind_post_agg e =
                let rec go = function
                  | Ast.E_col name ->
                    (match proj_lookup name with
                     | Error e -> Error e
                     | Ok i ->
                       (match find_pos group_cols i with
                        | Some pos -> Ok (BE_col pos)
                        | None -> Error (Unsupported (Printf.sprintf
                            "column '%s' must appear in GROUP BY to be used in a window function here" name))))
                  | Ast.E_tbl_col (t, c) ->
                    (match qual_lookup t c with
                     | Error e -> Error e
                     | Ok i ->
                       (match find_pos group_cols i with
                        | Some pos -> Ok (BE_col pos)
                        | None -> Error (Unsupported (Printf.sprintf
                            "column '%s.%s' must appear in GROUP BY to be used in a window function here" t c))))
                  | Ast.E_agg (func, arg_opt) ->
                    let col_ord_result : (int option, error) result =
                      match arg_opt with
                      | None -> (match func with Ast.Agg_count -> Ok None
                                 | _ -> Error (Unsupported "non-COUNT aggregate requires an argument"))
                      | Some (Ast.E_col name) ->
                        (match proj_lookup name with
                         | Error e -> Error e | Ok i -> Ok (Some i))
                      | Some (Ast.E_tbl_col (t, c)) ->
                        (match qual_lookup t c with
                         | Error e -> Error e | Ok i -> Ok (Some i))
                      | Some _ ->
                        Error (Unsupported "aggregate argument in window function must be a column reference")
                    in
                    (match col_ord_result with
                     | Error e -> Error e
                     | Ok co ->
                       let spec = { func; col_ord = co } in
                       let rec find_slot i = function
                         | [] ->
                           acc_aggs := !acc_aggs @ [spec];
                           List.length !acc_aggs - 1
                         | s :: _ when s.func = spec.func && s.col_ord = spec.col_ord -> i
                         | _ :: rest -> find_slot (i + 1) rest
                       in
                       let slot = find_slot 0 !acc_aggs in
                       Ok (BE_col (offset_for_aggs + slot)))
                  | Ast.E_lit l -> Ok (BE_lit l)
                  | Ast.E_neg e -> (match go e with Ok be -> Ok (BE_neg be) | Error e -> Error e)
                  | Ast.E_not e -> (match go e with Ok be -> Ok (BE_not be) | Error e -> Error e)
                  | Ast.E_binop (op, a, b) ->
                    (match go a, go b with
                     | Ok ba, Ok bb -> Ok (BE_binop (ast_binop_to_sema op, ba, bb))
                     | Error e, _ | _, Error e -> Error e)
                  | Ast.E_window _ -> Error (Unsupported "nested window functions not supported")
                  | _ -> Error (Unsupported "unsupported expression in window function context (only GROUP BY columns and aggregates)")
                in go e
              in
              let bind_list es =
                List.fold_left (fun acc_r ex ->
                  match acc_r with
                  | Error _ as err -> err
                  | Ok acc ->
                    match bind_post_agg ex with
                    | Error er -> Error er
                    | Ok be    -> Ok (acc @ [be])
                ) (Ok []) es
              in
              let bind_ok_list (oks : Ast.order_key list) =
                List.fold_left (fun acc_r ok ->
                  match acc_r with
                  | Error _ as err -> err
                  | Ok acc ->
                    match bind_post_agg ok.Ast.expr with
                    | Error er -> Error er
                    | Ok be    ->
                      let nulls = ok.Ast.nulls in
                      Ok (acc @ [{ key = be; dir = ok.Ast.dir; nulls }])
                ) (Ok []) oks
              in
              (match bind_list args with
               | Error er -> Error er
               | Ok bound_args ->
                 match bind_list window.Ast.partition_by with
                 | Error er -> Error er
                 | Ok bound_pb ->
                   match bind_ok_list window.Ast.order_by with
                   | Error er -> Error er
                   | Ok bound_ob ->
                     let slot = Queue.length agg_windows_queue in
                     Queue.push
                       { func; args = bound_args;
                         partition_by = bound_pb;
                         order_by = bound_ob;
                         frame = window.Ast.frame }
                       agg_windows_queue;
                     Ok (AP_window_slot slot))
```

**c)** After building `agg_proj_result`, collect `agg_windows` from the queue and include it in `BS_select`:

Find the `Lwt.return (Ok ([], items, !acc_aggs, [], []))` line (around line 1599). Change to:
```ocaml
             Lwt.return (match agg_proj_result with
              | Error e -> Error e
              | Ok items -> Ok ([], items, !acc_aggs, [], [], Queue.fold (fun acc w -> acc @ [w]) [] agg_windows_queue))
```

Note: the outer tuple in `proj_result` is now 6 elements: `(proj_ords, agg_proj_items, proj_aggs, proj_exprs, windows, agg_wins)`. Update the non-aggregated path too. In the non-aggregated branch (around line 1495), return 6-element tuple:
```ocaml
Lwt.return (Ok (o, [], [], [], windows_list, []))   (* agg_wins = [] *)
Lwt.return (Ok ([], [], [], bes, windows_list, []))  (* agg_wins = [] *)
```

Update the destructuring of `proj_result` (around line 1604):
```ocaml
| Ok (proj_ords, agg_proj_items, proj_aggs, proj_exprs, proj_windows, agg_wins) ->
```

And in the final `BS_select` construction (around line 1749), add `agg_windows = agg_wins`:
```ocaml
Lwt.return (Ok (BS_select {
  ...
  windows    = proj_windows;
  agg_windows = agg_wins;
}))
```

- [ ] **Step 5: Add `PI_window_slot` and `windows` to `lib/sql/plan.ml`**

In `lib/sql/plan.ml`, find:
```ocaml
and proj_item =
  | PI_group_col of int     (** project the i-th GROUP BY column (index into group_cols) *)
  | PI_agg_slot of int      (** project the k-th aggregate result *)
```
Add:
```ocaml
and proj_item =
  | PI_group_col of int     (** project the i-th GROUP BY column (index into group_cols) *)
  | PI_agg_slot of int      (** project the k-th aggregate result *)
  | PI_window_slot of int   (** project the j-th post-aggregate window result *)
```

Find `Op_aggregate`:
```ocaml
  | Op_aggregate of {
      child      : op;
      group_cols : int list;
      aggs       : agg_spec list;
      having     : expr option;
      proj       : proj_item list;
    }
```
Add `windows`:
```ocaml
  | Op_aggregate of {
      child      : op;
      group_cols : int list;
      aggs       : agg_spec list;
      having     : expr option;
      proj       : proj_item list;
      windows    : window_plan_item list;
        (** Post-aggregate window functions; empty for plain GROUP BY. *)
    }
```

- [ ] **Step 6: Update `lib/sql/planner.ml`**

Update `sema_agg_proj_to_plan`:
```ocaml
let sema_agg_proj_to_plan : Sema.agg_proj_item -> Plan.proj_item = function
  | Sema.AP_group_col i  -> Plan.PI_group_col i
  | Sema.AP_agg_slot i   -> Plan.PI_agg_slot i
  | Sema.AP_window_slot i -> Plan.PI_window_slot i
```

Update `plan_select` signature and body. Add `~agg_windows` parameter:

In `plan_select cat ~table_meta ~proj ~expr_proj ~where ~order ~limit ~offset ~joins ~group_by ~aggs ~having ~agg_proj ~distinct ~windows`, add `~agg_windows`:

```ocaml
let plan_select cat
    ~table_meta ~proj ~expr_proj ~where ~order ~limit ~offset ~joins
    ~group_by ~aggs ~having ~agg_proj ~distinct ~windows ~agg_windows =
```

In the `projected` construction (around line 278):
```ocaml
  let projected =
    if is_aggregated then
      Plan.Op_aggregate {
        child = after_sort;
        group_cols = group_by;
        aggs = List.map sema_agg_to_plan aggs;
        having = Option.map plan_expr having;
        proj = List.map sema_agg_proj_to_plan agg_proj;
        windows = List.map plan_window_item agg_windows;
      }
```

Update the call site in `plan`:
```ocaml
  | Sema.BS_select { distinct; table_meta; proj; expr_proj; where; order; limit; offset;
                     joins; group_by; aggs; having; agg_proj; windows; agg_windows } ->
    (match cat with
     | Some cat ->
       plan_select cat ~table_meta ~proj ~expr_proj ~where ~order ~limit ~offset
         ~joins ~group_by ~aggs ~having ~agg_proj ~distinct ~windows ~agg_windows
```

Also update the backwards-compatible path (no-catalog path) — find `Sema.BS_select` destructuring in the `| None ->` branch and add `agg_windows = _` (or `agg_windows`) to the pattern. The no-catalog path doesn't use `agg_windows` (it doesn't build Op_aggregate at all — it's legacy).

- [ ] **Step 7: Update `lib/sql/exec.ml` Op_aggregate handler**

Find `Plan.Op_aggregate { child; group_cols; aggs; having; proj }` in exec.ml (around line 2656). Update to:

```ocaml
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
      (* ... existing grouping code unchanged ... *)
    in
    let compute_agg (* ... existing unchanged ... *) in
    let agg_output_rows =
      List.map (fun (group_key, group_rows) ->
        let agg_vals = List.map (fun spec -> compute_agg spec group_rows) aggs in
        Array.of_list (group_key @ agg_vals)
      ) groups
    in
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
          let results = Array.make n_total Row.V_null in
          List.iter (fun (_, partition_indexed) ->
            let sorted = sort_partition_by clock params wplan.Plan.order_by partition_indexed in
            let win_res = compute_window_for_partition clock params wplan sorted n_total in
            Array.blit win_res 0 results 0 n_total
          ) partitions;
          results
        ) agg_windows in
        List.mapi (fun i row ->
          let extras = List.map (fun arr -> arr.(i)) window_arrays in
          Array.append row (Array.of_list extras)
        ) after_having
      end
    in
    let final_rows =
      List.map (fun agg_row ->
        Array.of_list (List.map (function
          | Plan.PI_group_col i  -> agg_row.(i)
          | Plan.PI_agg_slot k   -> agg_row.(n_group_cols + k)
          | Plan.PI_window_slot j -> agg_row.(n_agg_cols + j)
        ) proj)
      ) with_windows
    in
    Lwt.return (Lwt_stream.of_list final_rows)
```

**Important:** Keep all existing code inside `compute_agg` and the grouping logic unchanged. Only replace the final `after_having → final_rows` block with the window-aware version above.

Also update `substitute_cte` in exec.ml (around line 1918) which pattern-matches on Op_aggregate:
```ocaml
  | Plan.Op_aggregate r -> Plan.Op_aggregate { r with child = go r.child }
```
This still works since we added `windows` as a record field (it'll be carried through `r`).

The `pre_eval_subquery` function also matches Op_aggregate — update its pattern if it has an explicit destructuring. Search for `Op_aggregate` in exec.ml and update any exhaustive patterns.

- [ ] **Step 8: Remove the now-incorrect `Unsupported` in `bind_expr_agg`**

In `lib/sql/sema.ml`, find (around line 737):
```ocaml
    | Ast.E_window _ ->
      Error (Unsupported "window functions not yet supported in aggregate context")
```

This is in `bind_expr_agg` which handles expressions inside aggregate arguments (e.g., `SUM(window_func())`). This block should remain — nested window functions inside aggregate args are still unsupported. Do NOT remove it.

The change is only in `project_one` inside the aggregated branch of `bind_select`. The existing `bind_expr_agg` Unsupported for E_window is correct and should stay.

- [ ] **Step 9: Add sqlite comparison tests in `test/test_sqlite_compare.ml`**

```ocaml
let phase19_window_agg_cases =
  let setup = "CREATE TABLE wagg (dept TEXT, sal INTEGER); \
               INSERT INTO wagg VALUES ('eng', 100); \
               INSERT INTO wagg VALUES ('eng', 200); \
               INSERT INTO wagg VALUES ('mkt', 150); \
               INSERT INTO wagg VALUES ('mkt', 50); " in
  [
    ("rank over sum",
     setup ^ "SELECT dept, SUM(sal), RANK() OVER (ORDER BY SUM(sal) DESC) FROM wagg GROUP BY dept ORDER BY dept");
    ("dense rank over count",
     setup ^ "SELECT dept, COUNT(*), DENSE_RANK() OVER (ORDER BY COUNT(*) DESC) FROM wagg GROUP BY dept ORDER BY dept");
  ]
```

Register:
```ocaml
  run_cases "phase19_window_agg" phase19_window_agg_cases;
```

- [ ] **Step 10: Run all tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -5
```

Expected: all tests passing (330+ e2e tests).

```bash
podman run --rm \
  -v $(pwd):/workspace:Z \
  -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
  -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
  -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
  -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
  -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe 2>&1 | tail -5
```

Expected: all comparison tests passing.

- [ ] **Step 11: Commit**

```bash
git add lib/sql/sema.ml lib/sql/sema.mli lib/sql/plan.ml lib/sql/planner.ml lib/sql/exec.ml \
        test/test_e2e.ml test/test_sqlite_compare.ml
git commit -m "feat(phase19): window functions in aggregated queries (GROUP BY + OVER) [#125]"
```

---

## Self-Review Checklist

**Spec coverage:**
- [x] #123 math functions: CEIL, FLOOR, SQRT, POW, EXP, LN, LOG, LOG2, LOG10, SIGN, TRUNC, PI, SIN, COS, TAN, ASIN, ACOS, ATAN, ATAN2, DEGREES, RADIANS — covered in Task 1
- [x] #124 NULLS FIRST/LAST: syntax, sema propagation, sort comparison — covered in Task 2
- [x] #125 window functions in aggregated queries: AP_window_slot, PI_window_slot, agg_windows, inline window computation in Op_aggregate — covered in Task 3

**Placeholder scan:** None.

**Type consistency:**
- `AP_window_slot of int` in sema ↔ `PI_window_slot of int` in plan ↔ `agg_row.(n_agg_cols + j)` in exec
- `agg_windows : window_sema list` in sema ↔ `windows : window_plan_item list` in Op_aggregate
- Sort key tuples: `(expr * [`Asc|`Desc] * [`Nulls_first|`Nulls_last])` consistent across plan.ml, planner.ml, exec.ml

**Edge cases documented:**
- LOG(b, x) vs LOG(x): two separate parser alternatives differing by arity; the COMMA distinguishes them
- Window in agg context: E_agg in window args resolved via `find_slot` on `acc_aggs` (may add new agg slot)
- NULLS FIRST/LAST default: resolved at planner layer (None → Nulls_first for ASC, Nulls_last for DESC)
- `Array.blit` for window results: `n_total` must equal len(after_having), which it does since indexed is `List.mapi after_having`
