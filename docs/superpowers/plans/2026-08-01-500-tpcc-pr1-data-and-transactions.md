# TPC-C Benchmark — PR 1: Data and Transactions — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the non-concurrent half of the TPC-C-derived benchmark — schema, deterministic population, the five transaction profiles, and the consistency-condition oracle — fully tested, with each profile's portability verdict determined by actually running it against granary.

**Architecture:** Five new modules in the existing `test/tpc/` library `granary_tpc`, plus two new primitives on `Tpc_rand` and one small shared value-literal module. `Tpcc_txn` holds all transaction control flow behind an abstract `ops` record (a `query`/`exec` pair), so the profiles are engine-agnostic and unit-testable against a recording mock, and PR 2's concurrent driver supplies the real `ops` without any logic moving between PRs.

**Tech Stack:** OCaml 5.4, dune, Lwt, Alcotest, qcheck-core/qcheck-alcotest. Everything builds and tests inside the `granary-dev` podman image. No `sqlite3` dependency anywhere in this PR — the library must build and its tests must run where `sqlite3` is absent.

## Global Constraints

Copied from `docs/superpowers/specs/2026-08-01-500-tpcc-benchmark-design.md` and `CLAUDE.md`. Every task's requirements implicitly include this section.

- **Work happens in the worktree `/home/tej/projects/sqlite_ocaml_port/.worktrees/500-tpcc`, branch `feat/500-tpcc-bench`.** Never commit to `main`.
- **Never call `dune` on the host.** Build: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build`. Test one target: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpcc_gen.exe`. Run from the worktree root, substituting the worktree path for `$(pwd)`.
- **`test/tpc/` is a dune library, so merlint's rules apply:** every module needs a `.mli`; every public `val` in an `.mli` needs a `(** … *)` doc comment (not `(* … *)`); an abstract `type t` needs a `pp`; max nesting depth 4.
- **The library must not depend on `sqlite3`.** `test/tpc/dune` stays as it is: `(libraries granary granary.store granary.unix lwt lwt.unix unix)`.
- **Formatting before pushing:** `sh scripts/check-fmt.sh` (use `--fix` to rewrite). Read its final summary line — `◐ … Dune files UNVERIFIED` means the dune-file check was skipped and you must run `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build @fmt` in the **main checkout** (this PR modifies `test/dune`, so this is required). Then `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint`, expecting 0 issues for the new files.
- **Money is REAL, dates are TEXT `'YYYY-MM-DD HH:MM:SS'`.** granary types a literal by its text, so under strict column typing a whole-valued REAL must keep a fractional part (`100` is rejected by a REAL column; `100.0` is accepted). This is why `Tpc_value.literal` exists.
- **Row counts per warehouse W:** `warehouse` W; `district` 10×W; `customer` 3,000 per district; `history` 1 per customer; `item` 100,000 **fixed, never scaled by W**; `stock` 100,000 per warehouse; `orders` 3,000 per district; `new_order` the last 900 orders per district (o_id 2101–3000); `order_line` 5–15 per order, uniform.
- **Transaction weights:** NewOrder 45, Payment 43, OrderStatus 4, Delivery 4, StockLevel 4.
- **Verdicts are an output, not an input.** Task 8 determines them by running each profile against a real granary database. Do not guess a verdict while writing Task 7.

---

## File Structure

**Create:**

| File | Responsibility |
|---|---|
| `test/tpc/tpc_value.ml{,i}` | The generated-value type and its SQL literal rendering, shared by both harnesses |
| `test/tpc/tpcc_schema.ml{,i}` | The 9 `CREATE TABLE` statements, indexes, and a `Load` functor over `Bench_report.ENGINE` |
| `test/tpc/tpcc_gen.ml{,i}` | Deterministic seeded population of the 9 tables |
| `test/tpc/tpcc_check.ml{,i}` | Consistency-condition SQL and the pure classifier over their results |
| `test/tpc/tpcc_txn.ml{,i}` | The 5 profiles: input generation, control flow over an abstract `ops`, verdicts |
| `test/test_tpcc_gen.ml` | Row counts, determinism, column-range sanity |
| `test/test_tpcc_check.ml` | Each condition fails when its invariant is broken |
| `test/test_tpcc_txn.ml` | Profile catalogue completeness; statement sequences against a recording mock |
| `test/test_tpcc_load.ml` | Integration: load W=1 into granary, assert counts and initial consistency |
| `test/test_tpcc_smoke.ml` | Integration: run each profile once against granary; pins the verdicts |

**Modify:**

- `test/tpc/tpch_schema.ml:92-108` — `literal` delegates to `Tpc_value.literal` instead of carrying its own copy.
- `test/tpc/tpch_schema.mli` — add a doc'd `val literal` if it is not already exposed; otherwise unchanged.
- `test/dune` — five new `(test …)` stanzas after the existing `test_tpch_*` block.
- `test/test_tpc_rand.ml` — cases for the two new primitives.

---

### Task 1: `Tpc_value` — shared value type and SQL literal rendering

**Files:**
- Create: `test/tpc/tpc_value.ml`, `test/tpc/tpc_value.mli`
- Modify: `test/tpc/tpch_schema.ml:92-108`
- Test: `test/test_tpcc_gen.ml` (created here with only the literal cases; extended in Task 4)
- Modify: `test/dune`

**Interfaces:**
- Consumes: nothing.
- Produces: `type Tpc_value.t = VInt of int | VReal of float | VText of string | VNull`, and `val Tpc_value.literal : t -> string`. Every later task builds SQL through `Tpc_value.literal`.

**Why this exists:** `Tpch_schema.literal` already encodes two rules that are easy to get subtly wrong and expensive to get wrong twice — the `''` escaping of embedded quotes, and forcing a fractional part onto whole-valued REALs so granary's strict column typing accepts them. #503 established that this codebase keeps one implementation and one copy of the rationale. TPC-C also needs `VNull`, which TPC-H never produced (`o_carrier_id` and `ol_delivery_d` are NULL for undelivered orders), so the shared type gains a constructor that `Tpch_gen.value` does not have. `Tpch_gen.value` is therefore left untouched and `Tpch_schema.literal` converts into `Tpc_value.t`, which keeps the shipped TPC-H types stable while still having exactly one renderer.

- [ ] **Step 1: Write the failing test**

Create `test/test_tpcc_gen.ml`:

```ocaml
module V = Granary_tpc.Tpc_value

let test_literal_int () = Alcotest.(check string) "int" "42" (V.literal (V.VInt 42))
let test_literal_null () = Alcotest.(check string) "null" "NULL" (V.literal V.VNull)

(* granary types a literal by its text: a REAL column rejects "100" under
   strict column typing, so a whole-valued REAL must keep a fractional part. *)
let test_literal_whole_real_keeps_point () =
  Alcotest.(check string) "whole real" "100.0" (V.literal (V.VReal 100.0))
;;

let test_literal_fractional_real () =
  Alcotest.(check string) "fractional real" "1.5" (V.literal (V.VReal 1.5))
;;

let test_literal_text_quoted () =
  Alcotest.(check string) "text" "'abc'" (V.literal (V.VText "abc"))
;;

let test_literal_text_escapes_quote () =
  Alcotest.(check string) "embedded quote" "'O''Hara'" (V.literal (V.VText "O'Hara"))
;;

let () =
  Alcotest.run
    "tpcc_gen"
    [ ( "literal"
      , [ Alcotest.test_case "int" `Quick test_literal_int
        ; Alcotest.test_case "null" `Quick test_literal_null
        ; Alcotest.test_case "whole real keeps a point" `Quick
            test_literal_whole_real_keeps_point
        ; Alcotest.test_case "fractional real" `Quick test_literal_fractional_real
        ; Alcotest.test_case "text quoted" `Quick test_literal_text_quoted
        ; Alcotest.test_case "text escapes quote" `Quick test_literal_text_escapes_quote
        ] )
    ]
;;
```

Add to `test/dune`, immediately after the `test_tpch_smoke` stanza:

```
(test
 (name test_tpcc_gen)
 (modules test_tpcc_gen)
 (libraries granary_tpc alcotest))
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpcc_gen.exe`
Expected: FAIL — `Unbound module Granary_tpc.Tpc_value`.

- [ ] **Step 3: Write the implementation**

`test/tpc/tpc_value.mli`:

```ocaml
(** Generated column values and their SQL literal rendering (#500).

    Shared by both TPC-derived harnesses so the escaping and REAL-formatting
    rules have one implementation. *)

(** A generated column value. *)
type t =
  | VInt of int (** INTEGER column *)
  | VReal of float (** REAL column — money and rates *)
  | VText of string (** TEXT column — names, codes, dates, comments *)
  | VNull (** SQL NULL — an undelivered order's carrier and delivery date *)

(** [literal v] renders [v] as a SQL literal. Text is single-quoted with
    embedded quotes doubled. A whole-valued [VReal] keeps a fractional part:
    granary types a literal by its text, so a REAL column rejects ["100"]
    under strict column typing but accepts ["100.0"]. *)
val literal : t -> string
```

`test/tpc/tpc_value.ml`:

```ocaml
type t =
  | VInt of int
  | VReal of float
  | VText of string
  | VNull

let literal = function
  | VInt i -> string_of_int i
  | VNull -> "NULL"
  | VReal f ->
    let s = Printf.sprintf "%.17g" f in
    if String.exists (fun c -> c = '.' || c = 'e' || c = 'E') s then s else s ^ ".0"
  | VText s ->
    let buf = Buffer.create (String.length s + 2) in
    Buffer.add_char buf '\'';
    String.iter
      (fun c -> if c = '\'' then Buffer.add_string buf "''" else Buffer.add_char buf c)
      s;
    Buffer.add_char buf '\'';
    Buffer.contents buf
;;
```

Then replace the body of `literal` in `test/tpc/tpch_schema.ml` (currently lines 92-108) with a delegation, keeping a pointer to where the rationale now lives:

```ocaml
(* The escaping and whole-valued-REAL rules live in [Tpc_value.literal]; this
   only maps TPC-H's value type onto the shared one. *)
let literal = function
  | Tpch_gen.VInt i -> Tpc_value.literal (Tpc_value.VInt i)
  | Tpch_gen.VReal f -> Tpc_value.literal (Tpc_value.VReal f)
  | Tpch_gen.VText s -> Tpc_value.literal (Tpc_value.VText s)
;;
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpcc_gen.exe test/test_tpch_load.exe`
Expected: PASS on both. `test_tpch_load` must still pass — it is what proves the `Tpch_schema.literal` delegation did not change TPC-H's rendering.

- [ ] **Step 5: Commit**

```bash
git add test/tpc/tpc_value.ml test/tpc/tpc_value.mli test/tpc/tpch_schema.ml test/test_tpcc_gen.ml test/dune
git commit -m "feat(#500): Tpc_value — one SQL literal renderer for both TPC harnesses"
```

---

### Task 2: `Tpc_rand.nurand` and `Tpc_rand.last_name`

**Files:**
- Modify: `test/tpc/tpc_rand.ml`, `test/tpc/tpc_rand.mli`
- Test: `test/test_tpc_rand.ml`

**Interfaces:**
- Consumes: the existing `Tpc_rand.int_between`.
- Produces: `val nurand : t -> a:int -> x:int -> y:int -> c:int -> int` and `val last_name : int -> string`. Tasks 4 and 7 use both.

**Why:** `nurand` is the spec's non-uniform distribution, and the skew it produces is the only reason TPC-C contends at all — a uniform key distribution would make the benchmark measure nothing interesting. `last_name` is the syllable-triple name generator, used both to populate `c_last` and to drive the by-name Payment and OrderStatus variants. The spec draws `C` randomly per run; here it is an explicit parameter so a seed still pins the whole workload.

- [ ] **Step 1: Write the failing tests**

Append to `test/test_tpc_rand.ml`, before its `Alcotest.run`:

```ocaml
let test_nurand_within_range () =
  let r = seeded () in
  for _ = 1 to 10_000 do
    let v = Granary_tpc.Tpc_rand.nurand r ~a:1023 ~x:1 ~y:3000 ~c:17 in
    Alcotest.(check bool) "within [1,3000]" true (v >= 1 && v <= 3000)
  done
;;

let test_nurand_singleton_range () =
  let r = seeded () in
  Alcotest.(check int)
    "x = y yields x"
    5
    (Granary_tpc.Tpc_rand.nurand r ~a:255 ~x:5 ~y:5 ~c:3)
;;

let test_nurand_is_skewed () =
  (* The whole point of NURand: it must NOT be uniform. Over [1,3000] a
     uniform draw puts about 1/3 of its mass in the first third; NURand's
     bitwise-or biases it far above that. A uniform implementation would
     pass a range check, so this is the case that actually pins the shape. *)
  let r = seeded () in
  let n = 30_000 in
  let low = ref 0 in
  for _ = 1 to n do
    if Granary_tpc.Tpc_rand.nurand r ~a:1023 ~x:1 ~y:3000 ~c:0 <= 1000 then incr low
  done;
  Alcotest.(check bool)
    (Printf.sprintf "skewed toward low keys (%d/%d)" !low n)
    true
    (!low > n / 2)
;;

let test_last_name_endpoints () =
  Alcotest.(check string) "0" "BARBARBAR" (Granary_tpc.Tpc_rand.last_name 0);
  Alcotest.(check string) "999" "EINGEINGEING" (Granary_tpc.Tpc_rand.last_name 999)
;;

let test_last_name_distinct () =
  let names = List.init 1000 Granary_tpc.Tpc_rand.last_name in
  let uniq = List.sort_uniq String.compare names in
  Alcotest.(check int) "1000 distinct names" 1000 (List.length uniq)
;;

let test_last_name_out_of_range () =
  Alcotest.check_raises "negative" (Invalid_argument "Tpc_rand.last_name: n out of [0,999]")
    (fun () -> ignore (Granary_tpc.Tpc_rand.last_name (-1)));
  Alcotest.check_raises "too large" (Invalid_argument "Tpc_rand.last_name: n out of [0,999]")
    (fun () -> ignore (Granary_tpc.Tpc_rand.last_name 1000))
;;

let qcheck_nurand_in_range =
  QCheck.Test.make
    ~name:"nurand stays within [x,y] for arbitrary seeds and bounds"
    ~count:2000
    QCheck.(tup4 int (int_range 0 4095) (int_range 0 5000) (int_range 0 5000))
    (fun (seed, a, p, q) ->
       let x = min p q
       and y = max p q in
       let r = Granary_tpc.Tpc_rand.create ~seed in
       let v = Granary_tpc.Tpc_rand.nurand r ~a ~x ~y ~c:(abs seed mod 8192) in
       v >= x && v <= y)
;;

let qcheck_last_name_alphabet =
  QCheck.Test.make
    ~name:"last_name is a concatenation of three syllables"
    ~count:1000
    QCheck.(int_range 0 999)
    (fun n ->
       let s = Granary_tpc.Tpc_rand.last_name n in
       String.for_all (fun c -> c >= 'A' && c <= 'Z') s
       && String.length s >= 9
       && String.length s <= 12)
;;
```

Register the new cases in the existing `Alcotest.run` list — add a `"nurand"` and a `"last_name"` group alongside the existing groups, and append the two QCheck tests to whichever list the file already passes through `QCheck_alcotest.to_alcotest`.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpc_rand.exe`
Expected: FAIL — `Unbound value nurand`.

- [ ] **Step 3: Write the implementation**

Append to `test/tpc/tpc_rand.ml`:

```ocaml
(* TPC-C's non-uniform key distribution:
     NURand(A, x, y) = (((random(0,A) | random(x,y)) + C) mod (y - x + 1)) + x
   The bitwise-or is what creates the skew — it drives bits high, so the
   masked-and-wrapped result clusters, and clustering is what makes the
   benchmark contend.  The spec picks C once per run at random; taking it as a
   parameter keeps a seed sufficient to pin the whole workload. *)
let nurand t ~a ~x ~y ~c =
  if x > y then invalid_arg "Tpc_rand.nurand: x > y";
  let span = y - x + 1 in
  (((int_between t ~lo:0 ~hi:a lor int_between t ~lo:x ~hi:y) + c) mod span) + x
;;

let last_name_syllables =
  [| "BAR"; "OUGHT"; "ABLE"; "PRI"; "PRES"; "ESE"; "ANTI"; "CALLY"; "ATION"; "EING" |]
;;

let last_name n =
  if n < 0 || n > 999 then invalid_arg "Tpc_rand.last_name: n out of [0,999]";
  last_name_syllables.(n / 100)
  ^ last_name_syllables.(n / 10 mod 10)
  ^ last_name_syllables.(n mod 10)
;;
```

Append to `test/tpc/tpc_rand.mli`:

```ocaml
(** [nurand t ~a ~x ~y ~c] is the TPC-C non-uniform random distribution
    [(((random(0,a) lor random(x,y)) + c) mod (y - x + 1)) + x], always within
    [\[x, y\]].  The skew it produces is what makes the benchmark contend; a
    uniform draw would not.  [c] is the spec's per-run constant, taken as a
    parameter so that a seed pins the whole workload.  Requires [x <= y];
    raises [Invalid_argument] otherwise. *)
val nurand : t -> a:int -> x:int -> y:int -> c:int -> int

(** [last_name n] is the spec's customer surname for [n] in [\[0, 999\]]: the
    concatenation of three syllables drawn from a fixed ten-element table,
    running from ["BARBARBAR"] to ["EINGEINGEING"].  Pure — it draws no
    randomness.  Raises [Invalid_argument] outside [\[0, 999\]]. *)
val last_name : int -> string
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpc_rand.exe`
Expected: PASS, including `nurand is skewed`.

- [ ] **Step 5: Commit**

```bash
git add test/tpc/tpc_rand.ml test/tpc/tpc_rand.mli test/test_tpc_rand.ml
git commit -m "feat(#500): Tpc_rand.nurand and last_name, the TPC-C skew primitives"
```

---

### Task 3: `Tpcc_schema` — DDL, indexes, and the `Load` functor

**Files:**
- Create: `test/tpc/tpcc_schema.ml`, `test/tpc/tpcc_schema.mli`

**Interfaces:**
- Consumes: `Tpc_value.literal`, `Bench_report.ENGINE`, `Bench_report.env_int`.
- Produces: `val ddl : string list`, `val indexes : string list`, `val tables : string list`, and `module Load (E : Bench_report.ENGINE) : sig val run : E.t -> Tpcc_gen.t -> unit end`. Task 4 must make `Tpcc_gen.column_names` agree with the column order below; Task 9 calls `Load.run`.

**Note on ordering:** this task references `Tpcc_gen` (Task 4), which does not exist yet. Write `Tpcc_schema`'s `ddl`/`indexes`/`tables` first and commit; add the `Load` functor at the end of Task 4, when `Tpcc_gen` exists. Step 5 below reflects that split.

- [ ] **Step 1: Write the implementation of the DDL**

`test/tpc/tpcc_schema.mli`:

```ocaml
(** Schema for the TPC-C-derived benchmark (#500).

    Column names and order here MUST match {!Tpcc_gen.column_names} exactly —
    verified by [test/test_tpcc_load.ml]. *)

(** The nine [CREATE TABLE] statements, in load order: parents before
    children, so a loader can follow this list directly. *)
val ddl : string list

(** Secondary indexes, created after the load.  Covers the lookups the five
    transaction profiles perform: customer by last name, orders by customer,
    and the new-order queue. *)
val indexes : string list

(** The nine table names, in the same order as {!ddl}. *)
val tables : string list
```

`test/tpc/tpcc_schema.ml` — the nine tables. Money columns are REAL, timestamps are TEXT:

```ocaml
let ddl =
  [ {|CREATE TABLE warehouse (
       w_id       INTEGER PRIMARY KEY,
       w_name     TEXT NOT NULL,
       w_street_1 TEXT NOT NULL,
       w_street_2 TEXT NOT NULL,
       w_city     TEXT NOT NULL,
       w_state    TEXT NOT NULL,
       w_zip      TEXT NOT NULL,
       w_tax      REAL NOT NULL,
       w_ytd      REAL NOT NULL)|}
  ; {|CREATE TABLE district (
       d_id        INTEGER NOT NULL,
       d_w_id      INTEGER NOT NULL,
       d_name      TEXT NOT NULL,
       d_street_1  TEXT NOT NULL,
       d_street_2  TEXT NOT NULL,
       d_city      TEXT NOT NULL,
       d_state     TEXT NOT NULL,
       d_zip       TEXT NOT NULL,
       d_tax       REAL NOT NULL,
       d_ytd       REAL NOT NULL,
       d_next_o_id INTEGER NOT NULL,
       PRIMARY KEY (d_w_id, d_id))|}
  ; {|CREATE TABLE customer (
       c_id           INTEGER NOT NULL,
       c_d_id         INTEGER NOT NULL,
       c_w_id         INTEGER NOT NULL,
       c_first        TEXT NOT NULL,
       c_middle       TEXT NOT NULL,
       c_last         TEXT NOT NULL,
       c_street_1     TEXT NOT NULL,
       c_street_2     TEXT NOT NULL,
       c_city         TEXT NOT NULL,
       c_state        TEXT NOT NULL,
       c_zip          TEXT NOT NULL,
       c_phone        TEXT NOT NULL,
       c_since        TEXT NOT NULL,
       c_credit       TEXT NOT NULL,
       c_credit_lim   REAL NOT NULL,
       c_discount     REAL NOT NULL,
       c_balance      REAL NOT NULL,
       c_ytd_payment  REAL NOT NULL,
       c_payment_cnt  INTEGER NOT NULL,
       c_delivery_cnt INTEGER NOT NULL,
       c_data         TEXT NOT NULL,
       PRIMARY KEY (c_w_id, c_d_id, c_id))|}
  ; {|CREATE TABLE history (
       h_c_id   INTEGER NOT NULL,
       h_c_d_id INTEGER NOT NULL,
       h_c_w_id INTEGER NOT NULL,
       h_d_id   INTEGER NOT NULL,
       h_w_id   INTEGER NOT NULL,
       h_date   TEXT NOT NULL,
       h_amount REAL NOT NULL,
       h_data   TEXT NOT NULL)|}
  ; {|CREATE TABLE item (
       i_id    INTEGER PRIMARY KEY,
       i_im_id INTEGER NOT NULL,
       i_name  TEXT NOT NULL,
       i_price REAL NOT NULL,
       i_data  TEXT NOT NULL)|}
  ; {|CREATE TABLE stock (
       s_i_id       INTEGER NOT NULL,
       s_w_id       INTEGER NOT NULL,
       s_quantity   INTEGER NOT NULL,
       s_dist_01    TEXT NOT NULL,
       s_dist_02    TEXT NOT NULL,
       s_dist_03    TEXT NOT NULL,
       s_dist_04    TEXT NOT NULL,
       s_dist_05    TEXT NOT NULL,
       s_dist_06    TEXT NOT NULL,
       s_dist_07    TEXT NOT NULL,
       s_dist_08    TEXT NOT NULL,
       s_dist_09    TEXT NOT NULL,
       s_dist_10    TEXT NOT NULL,
       s_ytd        INTEGER NOT NULL,
       s_order_cnt  INTEGER NOT NULL,
       s_remote_cnt INTEGER NOT NULL,
       s_data       TEXT NOT NULL,
       PRIMARY KEY (s_w_id, s_i_id))|}
  ; {|CREATE TABLE orders (
       o_id         INTEGER NOT NULL,
       o_d_id       INTEGER NOT NULL,
       o_w_id       INTEGER NOT NULL,
       o_c_id       INTEGER NOT NULL,
       o_entry_d    TEXT NOT NULL,
       o_carrier_id INTEGER,
       o_ol_cnt     INTEGER NOT NULL,
       o_all_local  INTEGER NOT NULL,
       PRIMARY KEY (o_w_id, o_d_id, o_id))|}
  ; {|CREATE TABLE new_order (
       no_o_id INTEGER NOT NULL,
       no_d_id INTEGER NOT NULL,
       no_w_id INTEGER NOT NULL,
       PRIMARY KEY (no_w_id, no_d_id, no_o_id))|}
  ; {|CREATE TABLE order_line (
       ol_o_id        INTEGER NOT NULL,
       ol_d_id        INTEGER NOT NULL,
       ol_w_id        INTEGER NOT NULL,
       ol_number      INTEGER NOT NULL,
       ol_i_id        INTEGER NOT NULL,
       ol_supply_w_id INTEGER NOT NULL,
       ol_delivery_d  TEXT,
       ol_quantity    INTEGER NOT NULL,
       ol_amount      REAL NOT NULL,
       ol_dist_info   TEXT NOT NULL,
       PRIMARY KEY (ol_w_id, ol_d_id, ol_o_id, ol_number))|}
  ]
;;

let tables =
  [ "warehouse"
  ; "district"
  ; "customer"
  ; "history"
  ; "item"
  ; "stock"
  ; "orders"
  ; "new_order"
  ; "order_line"
  ]
;;

(* Payment and OrderStatus look a customer up by last name within a district;
   OrderStatus takes that customer's most recent order.  Without these two the
   read-heavy half of the mix degrades into full scans of a 30,000-row table
   per transaction, which measures scan speed rather than the profile. *)
let indexes =
  [ "CREATE INDEX idx_customer_last ON customer (c_w_id, c_d_id, c_last)"
  ; "CREATE INDEX idx_orders_cust ON orders (o_w_id, o_d_id, o_c_id)"
  ]
;;
```

- [ ] **Step 2: Verify it builds**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build`
Expected: SUCCESS (the pre-existing `sqlite3 not found` warning is expected and ignorable).

- [ ] **Step 3: Verify granary accepts every statement**

This is the point of the task — a `CREATE TABLE` granary rejects must surface now, not in Task 9. Use the REPL-free path: add a temporary throwaway check by running the existing smoke harness pattern, or simply proceed to Task 9 which asserts it. If you want it immediately, run:

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev \
  dune exec bin/main.exe -- --help
```

and confirm the CLI is available, then feed the DDL through it. If any statement is rejected, **file a Forgejo issue** naming the rejected syntax before working around it — an unsupported DDL form is an engine finding, exactly like the TPC-H skips.

- [ ] **Step 4: Commit**

```bash
git add test/tpc/tpcc_schema.ml test/tpc/tpcc_schema.mli
git commit -m "feat(#500): TPC-C schema — 9 tables and the profile-serving indexes"
```

- [ ] **Step 5: Defer the `Load` functor**

Do not write `module Load` yet — it needs `Tpcc_gen`. Task 4 adds it.

---

### Task 4: `Tpcc_gen` — deterministic population

**Files:**
- Create: `test/tpc/tpcc_gen.ml`, `test/tpc/tpcc_gen.mli`
- Modify: `test/tpc/tpcc_schema.ml` (add `Load`), `test/tpc/tpcc_schema.mli`
- Test: `test/test_tpcc_gen.ml` (extend)

**Interfaces:**
- Consumes: `Tpc_rand` (including Task 2's `nurand`, `last_name`), `Tpc_value.t`.
- Produces:
  - `type Tpcc_gen.t`
  - `val create : seed:int -> warehouses:int -> t`
  - `val pp : Format.formatter -> t -> unit`
  - `val warehouses : t -> int`
  - `val row_count : t -> table:string -> int`
  - `val iter_rows : t -> table:string -> f:(Tpc_value.t array -> unit) -> unit`
  - `val column_names : table:string -> string list`

  This mirrors `Tpch_gen`'s interface deliberately, so the `Load` functor is the same shape as `Tpch_schema.Load`.

**Structure:** follow `Tpch_gen`'s pattern at `test/tpc/tpch_gen.ml:14-27` — each table draws from its own `Tpc_rand.create ~seed:(seed + table_index table)` stream, so generating one table alone yields the same rows as generating it inside a full load. Reuse `Tpch_gen`'s civil-date helpers by copying the two functions `days_from_civil` / `civil_from_days` only if needed; TPC-C timestamps are all a single fixed load time, so a constant `"2026-01-01 00:00:00"` is sufficient and preferable — do not add date arithmetic this task does not need.

**Generation rules (from the spec, all `Tpc_rand` draws):**

- `warehouse`: `w_name` `a_string ~lo:6 ~hi:10`; street/city `a_string ~lo:10 ~hi:20`; `w_state` `a_string ~lo:2 ~hi:2`; `w_zip` a 4-digit number followed by `"11111"`; `w_tax` `float_between ~lo:0.0 ~hi:0.2 ~decimals:4`; `w_ytd` `300000.0`.
- `district`: same address shape; `d_tax` as `w_tax`; `d_ytd` `30000.0`; `d_next_o_id` `3001`.
- `customer`: `c_first` `a_string ~lo:8 ~hi:16`; `c_middle` `"OE"`; `c_last` `last_name` of `nurand ~a:255 ~x:0 ~y:999 ~c` for the first 1,000 customers of a district and of `c_id - 1` for `c_id <= 1000` — follow the spec: for `c_id <= 1000`, `last_name (c_id - 1)`; otherwise `last_name (nurand ~a:255 ~x:0 ~y:999 ~c)`. `c_credit` is `"BC"` for 10% of customers and `"GC"` otherwise; `c_credit_lim` `50000.0`; `c_discount` `float_between ~lo:0.0 ~hi:0.5 ~decimals:4`; `c_balance` `-10.0`; `c_ytd_payment` `10.0`; `c_payment_cnt` `1`; `c_delivery_cnt` `0`; `c_data` `a_string ~lo:300 ~hi:500`.
- `history`: one row per customer; `h_amount` `10.0`; `h_data` `a_string ~lo:12 ~hi:24`.
- `item`: 100,000 rows regardless of W. `i_im_id` `int_between ~lo:1 ~hi:10000`; `i_name` `a_string ~lo:14 ~hi:24`; `i_price` `float_between ~lo:1.0 ~hi:100.0 ~decimals:2`; `i_data` `a_string ~lo:26 ~hi:50`, with the literal `"ORIGINAL"` embedded at a random position in 10% of rows.
- `stock`: 100,000 rows per warehouse. `s_quantity` `int_between ~lo:10 ~hi:100`; each `s_dist_NN` `a_string ~lo:24 ~hi:24`; `s_ytd` `0`; `s_order_cnt` `0`; `s_remote_cnt` `0`; `s_data` as `i_data`, with the same 10% `"ORIGINAL"` rule.
- `orders`: `o_id` 1–3000 per district; `o_c_id` a permutation of 1–3000 (shuffle with `int_between`, do not draw independently — the spec requires each customer to have exactly one order, and condition 2 depends on the count); `o_carrier_id` `int_between ~lo:1 ~hi:10` for `o_id < 2101` and `VNull` otherwise; `o_ol_cnt` `int_between ~lo:5 ~hi:15`; `o_all_local` `1`.
- `new_order`: one row for each `o_id` in 2101–3000 per district (900 rows).
- `order_line`: `o_ol_cnt` rows per order, `ol_number` 1..`o_ol_cnt`; `ol_i_id` `int_between ~lo:1 ~hi:100000`; `ol_supply_w_id` = the order's warehouse; `ol_quantity` `5`; for `o_id < 2101`, `ol_amount` `0.0` and `ol_delivery_d` = the load timestamp; otherwise `ol_amount` `float_between ~lo:0.01 ~hi:9999.99 ~decimals:2` and `ol_delivery_d` `VNull`; `ol_dist_info` `a_string ~lo:24 ~hi:24`.

`o_ol_cnt` must be generated from a stream the `order_line` generator can reproduce exactly, or `iter_rows ~table:"order_line"` will disagree with the `o_ol_cnt` written into `orders`. The simplest correct approach: `order_line` re-derives each order's `o_ol_cnt` by replaying the `orders` stream for that district. Consistency condition 4 is precisely the check that this went right.

- [ ] **Step 1: Write the failing tests**

Extend `test/test_tpcc_gen.ml` with these groups (keeping the Task 1 literal group):

```ocaml
module G = Granary_tpc.Tpcc_gen

let gen ?(warehouses = 1) () = G.create ~seed:42 ~warehouses

let test_row_counts () =
  let g = gen ~warehouses:2 () in
  let expect table n = Alcotest.(check int) table n (G.row_count g ~table) in
  expect "warehouse" 2;
  expect "district" 20;
  expect "customer" 60_000;
  expect "history" 60_000;
  expect "item" 100_000;
  (* item is fixed by the spec and must NOT scale with warehouses *)
  expect "stock" 200_000;
  expect "orders" 60_000;
  expect "new_order" 18_000
;;

let test_item_does_not_scale () =
  Alcotest.(check int)
    "item is 100k at any warehouse count"
    (G.row_count (gen ~warehouses:1 ()) ~table:"item")
    (G.row_count (gen ~warehouses:4 ()) ~table:"item")
;;

let collect g ~table =
  let acc = ref [] in
  G.iter_rows g ~table ~f:(fun row -> acc := row :: !acc);
  List.rev !acc
;;

let test_iter_rows_matches_row_count () =
  let g = gen () in
  List.iter
    (fun table ->
       Alcotest.(check int)
         (table ^ ": iter_rows agrees with row_count")
         (G.row_count g ~table)
         (List.length (collect g ~table)))
    [ "warehouse"; "district"; "customer"; "history"; "stock"; "orders"; "new_order";
      "order_line" ]
;;

let test_same_seed_identical_output () =
  let render g ~table =
    String.concat
      "\n"
      (List.map
         (fun row ->
            String.concat "," (List.map Granary_tpc.Tpc_value.literal (Array.to_list row)))
         (collect g ~table))
  in
  let a = G.create ~seed:7 ~warehouses:1
  and b = G.create ~seed:7 ~warehouses:1 in
  List.iter
    (fun table ->
       Alcotest.(check string)
         (table ^ ": byte-identical at one seed")
         (render a ~table)
         (render b ~table))
    [ "district"; "customer"; "orders"; "order_line" ]
;;

let test_different_seed_differs () =
  let a = G.create ~seed:1 ~warehouses:1
  and b = G.create ~seed:2 ~warehouses:1 in
  Alcotest.(check bool)
    "customer rows differ at different seeds"
    false
    (collect a ~table:"customer" = collect b ~table:"customer")
;;

let test_columns_agree_with_arity () =
  let g = gen () in
  List.iter
    (fun table ->
       let cols = List.length (G.column_names ~table) in
       match collect g ~table with
       | [] -> Alcotest.fail (table ^ ": no rows")
       | row :: _ ->
         Alcotest.(check int) (table ^ ": arity") cols (Array.length row))
    Granary_tpc.Tpcc_schema.tables
;;

let test_unknown_table_raises () =
  let g = gen () in
  Alcotest.check_raises
    "unknown table"
    (Invalid_argument "Tpcc_gen: unknown table nope")
    (fun () -> ignore (G.row_count g ~table:"nope"))
;;

let test_district_initial_state () =
  (* Consistency conditions 1-3 must hold on the generated state before any
     transaction runs; a generator bug would otherwise be indistinguishable
     from a driver that corrupts a good database. *)
  let g = gen () in
  List.iter
    (fun row ->
       Alcotest.(check bool)
         "d_ytd = 30000, d_next_o_id = 3001"
         true
         (row.(9) = Granary_tpc.Tpc_value.VReal 30000.0
          && row.(10) = Granary_tpc.Tpc_value.VInt 3001))
    (collect g ~table:"district")
;;

let test_order_carrier_null_above_2100 () =
  let g = gen () in
  List.iter
    (fun row ->
       let o_id = match row.(0) with G.… -> 0 in
       ignore o_id)
    []
;;
```

Replace the stub `test_order_carrier_null_above_2100` with a real case: iterate `orders`, and assert `row.(5) = VNull` exactly when `row.(0)` is `VInt o_id` with `o_id >= 2101`, and `row.(5)` is a `VInt` in `[1,10]` otherwise.

Also add a case asserting `new_order` contains exactly the o_ids 2101–3000 for each district, and a case asserting `sum of o_ol_cnt` over `orders` equals the `order_line` row count — consistency condition 4, checked at the generator level where a failure is cheap to localise.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpcc_gen.exe`
Expected: FAIL — `Unbound module Granary_tpc.Tpcc_gen`.

- [ ] **Step 3: Write `Tpcc_gen`**

Implement per the generation rules above, following `test/tpc/tpch_gen.ml`'s structure: a per-table `Tpc_rand` stream keyed by table index, `row_count` computed arithmetically without generating, `iter_rows` streaming to the callback, and `column_names` returning schema declaration order. Keep each table's row builder a separate top-level function — merlint caps nesting at 4.

`test/tpc/tpcc_gen.mli` documents every `val` with `(** … *)`, including a module-level comment stating that output is a pure function of `seed` and `warehouses`, that `item` is fixed at 100,000 by the spec, and that money is REAL / timestamps are TEXT for the same reason as `Tpch_gen`.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpcc_gen.exe`
Expected: PASS.

- [ ] **Step 5: Add the `Load` functor to `Tpcc_schema`**

Append to `test/tpc/tpcc_schema.ml`, mirroring `Tpch_schema.Load` (`test/tpc/tpch_schema.ml:110-153`):

```ocaml
module Load (E : Bench_report.ENGINE) = struct
  (* Clamped to at least 1: 0 would raise Division_by_zero, and a negative
     value would silently disable batching instead of erroring. *)
  let batch_size = max 1 (Bench_report.env_int "GRANARY_TPCC_BATCH" 500)

  let flush engine ~table ~cols pending =
    if pending <> []
    then (
      let values =
        List.rev_map
          (fun row ->
             "(" ^ String.concat "," (List.map Tpc_value.literal (Array.to_list row)) ^ ")")
          pending
      in
      E.exec
        engine
        (Printf.sprintf
           "INSERT INTO %s (%s) VALUES %s"
           table
           (String.concat "," cols)
           (String.concat "," values)))
  ;;

  let load_table engine gen ~table =
    let cols = Tpcc_gen.column_names ~table in
    E.exec engine "BEGIN";
    let pending = ref []
    and n = ref 0 in
    Tpcc_gen.iter_rows gen ~table ~f:(fun row ->
      pending := row :: !pending;
      incr n;
      if !n mod batch_size = 0
      then (
        flush engine ~table ~cols !pending;
        pending := []));
    flush engine ~table ~cols !pending;
    E.exec engine "COMMIT"
  ;;

  let run engine gen =
    List.iter (E.exec engine) ddl;
    List.iter (fun table -> load_table engine gen ~table) tables;
    List.iter (E.exec engine) indexes
  ;;
end
```

Add the corresponding documented signature to `test/tpcc_schema.mli`:

```ocaml
(** [Load (E)] loads a generated population into an engine: DDL, then one
    batched transaction per table in {!tables} order, then {!indexes}.  Batch
    size is [GRANARY_TPCC_BATCH] (default 500). *)
module Load (E : Bench_report.ENGINE) : sig
  (** [run engine gen] creates the schema and loads every table. *)
  val run : E.t -> Tpcc_gen.t -> unit
end
```

- [ ] **Step 6: Run the full build and commit**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build && podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpcc_gen.exe`
Expected: SUCCESS, PASS.

```bash
git add test/tpc/tpcc_gen.ml test/tpc/tpcc_gen.mli test/tpc/tpcc_schema.ml test/tpc/tpcc_schema.mli test/test_tpcc_gen.ml
git commit -m "feat(#500): Tpcc_gen deterministic population and the Load functor"
```

---

### Task 5: `Tpcc_check` — the consistency-condition oracle

**Files:**
- Create: `test/tpc/tpcc_check.ml`, `test/tpc/tpcc_check.mli`
- Test: `test/test_tpcc_check.ml`
- Modify: `test/dune`

**Interfaces:**
- Consumes: nothing but `Stdlib`.
- Produces:
  - `type condition = { number : int; description : string; sql : string }`
  - `val conditions : condition list`
  - `type outcome = Holds | Violated of string | Not_run`
  - `val pp : Format.formatter -> outcome -> unit`
  - `val classify : condition -> rows:string list list -> outcome`
  - `val label : outcome -> string`
  - `val is_failure : outcome -> bool`

  Task 9 and PR 2's `bench_tpcc.ml` run each `condition.sql` through an engine and pass the rows to `classify`.

**Design:** each condition's SQL is written to return **zero rows when the invariant holds** and one row per violation otherwise. That makes `classify` trivial, uniform, and — crucially — testable without any engine at all, which is the `Tpch_check` lesson from #505: `bench_tpcc` is `(optional)` and will not build without `sqlite3`, so nothing that decides the exit code may live in it.

- [ ] **Step 1: Write the failing test**

Create `test/test_tpcc_check.ml`:

```ocaml
module C = Granary_tpc.Tpcc_check

let cond n = List.find (fun c -> c.C.number = n) C.conditions

let test_all_four_present () =
  Alcotest.(check (list int))
    "conditions 1-4"
    [ 1; 2; 3; 4 ]
    (List.map (fun c -> c.C.number) C.conditions)
;;

let test_every_condition_has_a_description () =
  List.iter
    (fun c ->
       Alcotest.(check bool)
         (Printf.sprintf "condition %d described" c.C.number)
         true
         (String.length c.C.description > 0))
    C.conditions
;;

let test_no_rows_means_holds () =
  Alcotest.(check bool) "holds" true (C.classify (cond 1) ~rows:[] = C.Holds)
;;

let test_rows_mean_violated () =
  match C.classify (cond 1) ~rows:[ [ "1"; "300000.0"; "299990.0" ] ] with
  | C.Violated report ->
    Alcotest.(check bool)
      "report names the condition"
      true
      (String.length report > 0)
  | C.Holds | C.Not_run -> Alcotest.fail "expected Violated"
;;

let test_violated_is_a_failure () =
  Alcotest.(check bool) "violated fails the run" true (C.is_failure (C.Violated "x"));
  Alcotest.(check bool) "holds does not" false (C.is_failure C.Holds);
  (* Not_run is a failure too: a condition that silently did not execute must
     not read as a pass.  This is the #502 lesson — a check that stopped
     running rendered as `skipped` and still exited 0. *)
  Alcotest.(check bool) "not_run fails the run" true (C.is_failure C.Not_run)
;;

let test_labels () =
  Alcotest.(check string) "holds" "ok" (C.label C.Holds);
  Alcotest.(check string) "violated" "VIOLATED" (C.label (C.Violated "x"));
  Alcotest.(check string) "not run" "not-run" (C.label C.Not_run)
;;

let test_sql_mentions_its_tables () =
  let mentions c sub =
    let re = Str.regexp_string sub in
    try ignore (Str.search_forward re c.C.sql 0); true with Not_found -> false
  in
  Alcotest.(check bool) "1 uses warehouse" true (mentions (cond 1) "warehouse");
  Alcotest.(check bool) "2 uses district" true (mentions (cond 2) "district");
  Alcotest.(check bool) "3 uses new_order" true (mentions (cond 3) "new_order");
  Alcotest.(check bool) "4 uses order_line" true (mentions (cond 4) "order_line")
;;

let () =
  Alcotest.run
    "tpcc_check"
    [ ( "catalogue"
      , [ Alcotest.test_case "all four present" `Quick test_all_four_present
        ; Alcotest.test_case "described" `Quick test_every_condition_has_a_description
        ; Alcotest.test_case "sql mentions its tables" `Quick test_sql_mentions_its_tables
        ] )
    ; ( "classify"
      , [ Alcotest.test_case "no rows holds" `Quick test_no_rows_means_holds
        ; Alcotest.test_case "rows violate" `Quick test_rows_mean_violated
        ; Alcotest.test_case "violated is a failure" `Quick test_violated_is_a_failure
        ; Alcotest.test_case "labels" `Quick test_labels
        ] )
    ]
;;
```

If `Str` is unwanted as a dependency, replace `test_sql_mentions_its_tables` with a hand-rolled substring search — do not add `str` to `test/dune` for one assertion.

Add to `test/dune`:

```
(test
 (name test_tpcc_check)
 (modules test_tpcc_check)
 (libraries granary_tpc alcotest))
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpcc_check.exe`
Expected: FAIL — `Unbound module Granary_tpc.Tpcc_check`.

- [ ] **Step 3: Write `Tpcc_check`**

`test/tpc/tpcc_check.ml`:

```ocaml
type condition =
  { number : int
  ; description : string
  ; sql : string
  }

type outcome =
  | Holds
  | Violated of string
  | Not_run

(* Each query returns ZERO rows when its invariant holds and one row per
   violation.  Keeping that shape uniform is what lets [classify] be pure and
   unit-testable without an engine — [bench_tpcc] is (optional) and does not
   build without sqlite3, so nothing that decides the exit code may live
   there (#502, #505). *)
let conditions =
  [ { number = 1
    ; description = "w_ytd equals the sum of its districts' d_ytd"
    ; sql =
        {|SELECT w_id, w_ytd, (SELECT SUM(d_ytd) FROM district WHERE d_w_id = w_id)
          FROM warehouse
          WHERE w_ytd <> (SELECT SUM(d_ytd) FROM district WHERE d_w_id = w_id)|}
    }
  ; { number = 2
    ; description = "d_next_o_id - 1 equals max(o_id) and max(no_o_id) per district"
    ; sql =
        {|SELECT d_w_id, d_id, d_next_o_id
          FROM district
          WHERE d_next_o_id - 1 <>
                (SELECT MAX(o_id) FROM orders
                  WHERE o_w_id = d_w_id AND o_d_id = d_id)
             OR d_next_o_id - 1 <>
                (SELECT MAX(no_o_id) FROM new_order
                  WHERE no_w_id = d_w_id AND no_d_id = d_id)|}
    }
  ; { number = 3
    ; description =
        "max(no_o_id) - min(no_o_id) + 1 equals the new_order row count per district"
    ; sql =
        {|SELECT no_w_id, no_d_id, MAX(no_o_id) - MIN(no_o_id) + 1, COUNT(*)
          FROM new_order
          GROUP BY no_w_id, no_d_id
          HAVING MAX(no_o_id) - MIN(no_o_id) + 1 <> COUNT(*)|}
    }
  ; { number = 4
    ; description = "the sum of o_ol_cnt equals the order_line row count per district"
    ; sql =
        {|SELECT o_w_id, o_d_id, SUM(o_ol_cnt),
                 (SELECT COUNT(*) FROM order_line
                   WHERE ol_w_id = o_w_id AND ol_d_id = o_d_id)
          FROM orders
          GROUP BY o_w_id, o_d_id
          HAVING SUM(o_ol_cnt) <>
                 (SELECT COUNT(*) FROM order_line
                   WHERE ol_w_id = o_w_id AND ol_d_id = o_d_id)|}
    }
  ]
;;

let classify c ~rows =
  match rows with
  | [] -> Holds
  | offending ->
    Violated
      (Printf.sprintf
         "condition %d (%s): %d offending row(s), first = [%s]"
         c.number
         c.description
         (List.length offending)
         (String.concat "; " (List.hd offending)))
;;

let label = function
  | Holds -> "ok"
  | Violated _ -> "VIOLATED"
  | Not_run -> "not-run"
;;

(* [Not_run] is a failure: a condition that stopped executing must not read as
   a pass.  Rendering a check that no longer runs as a benign token, and still
   exiting 0, is exactly the hole #502 closed on the TPC-H side. *)
let is_failure = function
  | Holds -> false
  | Violated _ | Not_run -> true
;;

let pp fmt = function
  | Holds -> Format.fprintf fmt "Holds"
  | Not_run -> Format.fprintf fmt "Not_run"
  | Violated report -> Format.fprintf fmt "Violated(%s)" report
;;
```

Write `test/tpc/tpcc_check.mli` with a `(** … *)` doc comment on every `val` and on both types, explaining the zero-rows-means-holds contract and why `Not_run` fails.

**Note:** conditions 2 and 4 use a correlated subquery referencing an outer column, and condition 4 correlates under a `GROUP BY`. #485 and #492 are open engine bugs in exactly that area. If Task 9 finds a condition returns wrong results on granary, that is a **finding**, not a reason to weaken the check: record it, reference the issue, and if necessary express the condition as two separate queries compared in OCaml rather than one SQL query.

- [ ] **Step 4: Run the test to verify it passes**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpcc_check.exe`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add test/tpc/tpcc_check.ml test/tpc/tpcc_check.mli test/test_tpcc_check.ml test/dune
git commit -m "feat(#500): Tpcc_check — clause-3.3 consistency conditions as a pure oracle"
```

---

### Task 6: `Tpcc_txn` — profile catalogue and input generation

**Files:**
- Create: `test/tpc/tpcc_txn.ml`, `test/tpc/tpcc_txn.mli`
- Test: `test/test_tpcc_txn.ml`
- Modify: `test/dune`

**Interfaces:**
- Consumes: `Tpc_rand` (`nurand`, `last_name`, `int_between`, `float_between`), `Tpc_value.t`.
- Produces (this task — control flow comes in Task 7):
  - `type verdict = Native | Rewritten of string | Skipped of string`
  - `type kind = New_order | Payment | Order_status | Delivery | Stock_level`
  - `type profile = { kind : kind; name : string; weight : int; verdict : verdict }`
  - `val all : profile list`
  - `val verdict_label : verdict -> string`
  - `val pick : Tpc_rand.t -> profile list -> profile` — weighted selection, skipping `Skipped` profiles with their weight redistributed
  - `type input` (a variant over the five profiles' parameters)
  - `val gen_input : Tpc_rand.t -> warehouses:int -> constant_c:int -> profile -> input`

**Why `pick` redistributes:** the spec's weights sum to 100 over five profiles. If a profile is `Skipped` because granary cannot express it, drawing against the original weights would silently produce a mix with a hole in it. Selecting only among runnable profiles, proportionally to their weights, keeps the ratios among the survivors correct and makes the reported mix honest.

- [ ] **Step 1: Write the failing test**

Create `test/test_tpcc_txn.ml`:

```ocaml
module T = Granary_tpc.Tpcc_txn

let test_all_five_profiles () =
  Alcotest.(check (list string))
    "the spec's five profiles"
    [ "new_order"; "payment"; "order_status"; "delivery"; "stock_level" ]
    (List.map (fun p -> p.T.name) T.all)
;;

let test_weights_sum_to_100 () =
  Alcotest.(check int)
    "weights sum to 100"
    100
    (List.fold_left (fun a p -> a + p.T.weight) 0 T.all)
;;

let test_spec_weights () =
  let w name = (List.find (fun p -> p.T.name = name) T.all).T.weight in
  Alcotest.(check int) "new_order" 45 (w "new_order");
  Alcotest.(check int) "payment" 43 (w "payment");
  Alcotest.(check int) "order_status" 4 (w "order_status");
  Alcotest.(check int) "delivery" 4 (w "delivery");
  Alcotest.(check int) "stock_level" 4 (w "stock_level")
;;

let test_pick_respects_weights () =
  let r = Granary_tpc.Tpc_rand.create ~seed:42 in
  let n = 20_000 in
  let counts = Hashtbl.create 5 in
  for _ = 1 to n do
    let p = T.pick r T.all in
    Hashtbl.replace counts p.T.name (1 + Option.value ~default:0 (Hashtbl.find_opt counts p.T.name))
  done;
  let share name = float_of_int (Hashtbl.find counts name) /. float_of_int n in
  Alcotest.(check bool)
    (Printf.sprintf "new_order near 0.45 (got %.3f)" (share "new_order"))
    true
    (Float.abs (share "new_order" -. 0.45) < 0.02)
;;

let test_pick_skips_skipped_and_redistributes () =
  (* A profile granary cannot express must drop out of the mix entirely, and
     the survivors must keep their ratios to one another — otherwise the
     reported mix has a silent hole in it. *)
  let runnable =
    List.filter (fun p -> p.T.name <> "delivery") T.all
    @ [ { (List.find (fun p -> p.T.name = "delivery") T.all) with
          T.verdict = T.Skipped "#NNN"
        }
      ]
  in
  let r = Granary_tpc.Tpc_rand.create ~seed:7 in
  for _ = 1 to 5_000 do
    Alcotest.(check bool)
      "never picks a skipped profile"
      true
      ((T.pick r runnable).T.name <> "delivery")
  done
;;

let test_pick_all_skipped_raises () =
  let none = List.map (fun p -> { p with T.verdict = T.Skipped "#NNN" }) T.all in
  let r = Granary_tpc.Tpc_rand.create ~seed:1 in
  Alcotest.check_raises
    "no runnable profile"
    (Invalid_argument "Tpcc_txn.pick: no runnable profile")
    (fun () -> ignore (T.pick r none))
;;

let test_gen_input_is_deterministic () =
  let draw () =
    let r = Granary_tpc.Tpc_rand.create ~seed:99 in
    List.map
      (fun p -> T.gen_input r ~warehouses:2 ~constant_c:11 p)
      T.all
  in
  Alcotest.(check bool) "same seed, same inputs" true (draw () = draw ())
;;

let () =
  Alcotest.run
    "tpcc_txn"
    [ ( "catalogue"
      , [ Alcotest.test_case "five profiles" `Quick test_all_five_profiles
        ; Alcotest.test_case "weights sum to 100" `Quick test_weights_sum_to_100
        ; Alcotest.test_case "spec weights" `Quick test_spec_weights
        ] )
    ; ( "mix"
      , [ Alcotest.test_case "respects weights" `Quick test_pick_respects_weights
        ; Alcotest.test_case "skips skipped" `Quick test_pick_skips_skipped_and_redistributes
        ; Alcotest.test_case "all skipped raises" `Quick test_pick_all_skipped_raises
        ] )
    ; ( "inputs"
      , [ Alcotest.test_case "deterministic" `Quick test_gen_input_is_deterministic ] )
    ]
;;
```

Add to `test/dune`:

```
(test
 (name test_tpcc_txn)
 (modules test_tpcc_txn)
 (libraries granary_tpc alcotest))
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpcc_txn.exe`
Expected: FAIL — `Unbound module Granary_tpc.Tpcc_txn`.

- [ ] **Step 3: Write the catalogue and input generation**

The five inputs, per the spec:

- `New_order`: `w_id` uniform in `[1, W]`; `d_id` uniform `[1,10]`; `c_id = nurand ~a:1023 ~x:1 ~y:3000`; `ol_cnt` uniform `[5,15]`; a list of `ol_cnt` items each with `i_id = nurand ~a:8191 ~x:1 ~y:100000`, `supply_w_id` (remote in 1% of lines when `W > 1`), and `quantity` uniform `[1,10]`; and a `rollback : bool` true in 1% of transactions, which invalidates the **last** item id. Retain the rollback — it exercises the rollback path, which is a reason to include it.
- `Payment`: `w_id`, `d_id`; the customer selected by id (40%) or by last name (60%, using `last_name (nurand ~a:255 ~x:0 ~y:999)`); `amount = float_between ~lo:1.0 ~hi:5000.0 ~decimals:2`; a remote customer in 15% of transactions when `W > 1`.
- `Order_status`: `w_id`, `d_id`; customer by id (40%) or last name (60%).
- `Delivery`: `w_id`; `carrier_id` uniform `[1,10]`.
- `Stock_level`: `w_id`, `d_id`; `threshold` uniform `[10,20]`.

Define `type input` as a variant carrying a record per kind, with all fields concrete (no `Obj.t`, no association lists). Give every profile `verdict = Native` **as a placeholder that Task 8 replaces with the measured value** — and put a comment saying exactly that, so nobody mistakes it for a determination.

`pick` implementation: filter to profiles whose verdict is not `Skipped`, raise `Invalid_argument "Tpcc_txn.pick: no runnable profile"` when that is empty, sum their weights, draw `int_between ~lo:1 ~hi:total`, and walk the list subtracting.

Write `test/tpc/tpcc_txn.mli` with doc comments on every `val` and type, including a `pp` for `input` (merlint requires a `pp` for an abstract `type t`; `input` is concrete so this is optional, but a `pp` makes the Task 7 mock's failure messages readable — add one).

- [ ] **Step 4: Run the test to verify it passes**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpcc_txn.exe`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add test/tpc/tpcc_txn.ml test/tpc/tpcc_txn.mli test/test_tpcc_txn.ml test/dune
git commit -m "feat(#500): Tpcc_txn profile catalogue, weighted mix, and input generation"
```

---

### Task 7: `Tpcc_txn` — transaction control flow over an abstract `ops`

**Files:**
- Modify: `test/tpc/tpcc_txn.ml`, `test/tpc/tpcc_txn.mli`
- Test: `test/test_tpcc_txn.ml` (extend)

**Interfaces:**
- Consumes: Task 6's `input`, `Tpc_value.literal`.
- Produces:
  - `type stmt = { sql : string; params : Tpc_value.t list }`
  - `type row = string list`
  - `type ops = { query : stmt -> row list Lwt.t; exec : stmt -> unit Lwt.t }`
  - `val run : ops -> input -> unit Lwt.t`
  - `val render : stmt -> string` — substitutes `params` into `sql` via `Tpc_value.literal`, for engines driven by literal SQL

  PR 2's driver supplies a real `ops` backed by `Db.prepare`/`Db.run`; the serial SQLite path supplies one backed by `render` plus `Bench_report.ENGINE`.

**Why this shape:** TPC-C transactions are not static statement lists — NewOrder reads a district's `d_next_o_id` and inserts using it, Delivery reads the oldest new-order row and updates against it. The control flow has to live somewhere. Putting it behind an abstract `ops` keeps all of it in the library, where it is unit-testable against a recording mock in this PR, and stops any transaction logic from leaking into PR 2's driver.

Each profile's `run` issues `BEGIN` and `COMMIT` (or `ROLLBACK`) through `ops.exec` itself, because the transaction boundary is part of the profile. It must **never** await anything outside `ops` between `BEGIN` and `COMMIT`: the store's write lock is not reentrant and is held for that whole window (`lib/store/store.ml:1394`), so a stray await turns contention into a permanent hang.

- [ ] **Step 1: Write the failing test**

Extend `test/test_tpcc_txn.ml` with a recording mock:

```ocaml
(* Records every statement and returns canned rows, so the profiles' control
   flow is testable with no engine at all. *)
let mock ~rows =
  let log = ref [] in
  let next = ref rows in
  let record s = log := T.render s :: !log in
  let ops =
    { T.query =
        (fun s ->
          record s;
          match !next with
          | [] -> Lwt.return []
          | r :: rest ->
            next := rest;
            Lwt.return r)
    ; T.exec =
        (fun s ->
          record s;
          Lwt.return_unit)
    }
  in
  ops, fun () -> List.rev !log
;;

let starts_with prefix s =
  String.length s >= String.length prefix
  && String.equal (String.sub s 0 (String.length prefix)) prefix
;;

let test_new_order_is_wrapped_in_a_transaction () =
  let ops, log = mock ~rows:[ [ [ "3001"; "0.1000" ] ]; [ [ "0.05"; "GC"; "X" ] ] ] in
  let r = Granary_tpc.Tpc_rand.create ~seed:5 in
  let p = List.find (fun p -> p.T.name = "new_order") T.all in
  Lwt_main.run (T.run ops (T.gen_input r ~warehouses:1 ~constant_c:11 p));
  match log () with
  | [] -> Alcotest.fail "no statements issued"
  | first :: _ as all ->
    Alcotest.(check bool) "opens with BEGIN" true (starts_with "BEGIN" first);
    let last = List.nth all (List.length all - 1) in
    Alcotest.(check bool)
      "closes with COMMIT or ROLLBACK"
      true
      (starts_with "COMMIT" last || starts_with "ROLLBACK" last)
;;

let test_render_substitutes_params () =
  Alcotest.(check string)
    "params substituted in order"
    "INSERT INTO t VALUES (1,'a',2.5)"
    (T.render
       { T.sql = "INSERT INTO t VALUES (?,?,?)"
       ; T.params =
           [ Granary_tpc.Tpc_value.VInt 1
           ; Granary_tpc.Tpc_value.VText "a"
           ; Granary_tpc.Tpc_value.VReal 2.5
           ]
       })
;;

let test_render_arity_mismatch_raises () =
  Alcotest.check_raises
    "too few params"
    (Invalid_argument "Tpcc_txn.render: 2 placeholders but 1 parameter(s)")
    (fun () ->
       ignore
         (T.render
            { T.sql = "SELECT ?, ?"; T.params = [ Granary_tpc.Tpc_value.VInt 1 ] }))
;;
```

Add one case per profile asserting the statements it issues, in order — for example, that Payment issues an `UPDATE warehouse`, an `UPDATE district`, an `UPDATE customer`, and an `INSERT INTO history` between its `BEGIN` and `COMMIT`; that Delivery issues a `DELETE FROM new_order`; and that a `rollback:true` NewOrder ends in `ROLLBACK` rather than `COMMIT`. Build each input explicitly rather than via `gen_input` where the case depends on a specific field.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpcc_txn.exe`
Expected: FAIL — `Unbound record field query` / `Unbound value run`.

- [ ] **Step 3: Write `render` and the five profiles**

`render` walks `sql` counting `?` placeholders and substituting `Tpc_value.literal` of each parameter in order, raising `Invalid_argument (Printf.sprintf "Tpcc_txn.render: %d placeholders but %d parameter(s)" n_placeholders n_params)` on a mismatch. A count mismatch must raise rather than silently truncate — a silently short-substituted statement is a wrong query that still runs.

The five profiles, following the spec:

- **NewOrder** — `BEGIN`; select `w_tax`; select `d_tax, d_next_o_id` for the district; `UPDATE district SET d_next_o_id = d_next_o_id + 1`; select `c_discount, c_last, c_credit`; `INSERT INTO orders`; `INSERT INTO new_order`; then per line: select `i_price, i_name, i_data` (the invalid id in a rollback transaction returns no rows — issue `ROLLBACK` and return), select the stock row, `UPDATE stock` adjusting `s_quantity`, `s_ytd`, `s_order_cnt`, `s_remote_cnt`, `INSERT INTO order_line`; `COMMIT`.
- **Payment** — `BEGIN`; `UPDATE warehouse SET w_ytd = w_ytd + amount`; `UPDATE district SET d_ytd = d_ytd + amount`; resolve the customer by id or by last name (by name: select matching customers ordered by `c_first`, take the middle one); `UPDATE customer` adjusting `c_balance`, `c_ytd_payment`, `c_payment_cnt`, and for `c_credit = 'BC'` prepending to `c_data`; `INSERT INTO history`; `COMMIT`.
- **OrderStatus** — read-only: `BEGIN`; resolve the customer; select their most recent order; select its order lines; `COMMIT`.
- **Delivery** — for each of the 10 districts: `BEGIN`; select `MIN(no_o_id)` from `new_order`; if none, `COMMIT` and continue; `DELETE FROM new_order`; `UPDATE orders SET o_carrier_id`; `UPDATE order_line SET ol_delivery_d`; select `SUM(ol_amount)`; `UPDATE customer` adjusting `c_balance` and `c_delivery_cnt`; `COMMIT`.
- **StockLevel** — read-only: `BEGIN`; select `d_next_o_id`; count distinct low-stock items across the last 20 orders' lines; `COMMIT`.

Condition 1 of `Tpcc_check` is exactly what catches Payment updating `w_ytd` and `d_ytd` inconsistently; condition 2 catches NewOrder's `d_next_o_id` increment going wrong. That coupling is intentional.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpcc_txn.exe`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add test/tpc/tpcc_txn.ml test/tpc/tpcc_txn.mli test/test_tpcc_txn.ml
git commit -m "feat(#500): TPC-C transaction profiles over an abstract ops record"
```

---

### Task 8: Load integration test — granary accepts the schema and the population

**Files:**
- Create: `test/test_tpcc_load.ml`
- Modify: `test/dune`

**Interfaces:**
- Consumes: `Tpcc_schema.Load`, `Tpcc_gen`, `Tpcc_check`, `Granary_engine`.
- Produces: nothing consumed by later tasks; this is the gate that the schema and generator work against the real engine.

This test uses `Granary_engine` (the synchronous `Bench_report.ENGINE` implementation already in the library), so it needs no `sqlite3` and runs everywhere.

- [ ] **Step 1: Write the test**

Create `test/test_tpcc_load.ml`:

```ocaml
module Load = Granary_tpc.Tpcc_schema.Load (Granary_tpc.Granary_engine)
module C = Granary_tpc.Tpcc_check

(* One warehouse is ~500k rows and takes a while to load; the columns and the
   consistency conditions are what this test is for, and they do not need more
   than one warehouse. *)
let with_loaded f =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "test-tpcc-load-%d" (Unix.getpid ()))
  in
  (try Unix.mkdir dir 0o755 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let e = Granary_tpc.Granary_engine.open_db ~dir in
  let gen = Granary_tpc.Tpcc_gen.create ~seed:42 ~warehouses:1 in
  Load.run e gen;
  Fun.protect ~finally:(fun () -> Granary_tpc.Granary_engine.close e) (fun () -> f e gen)
;;

let test_columns_match_gen () =
  (* The DDL and the generator's column order must agree, or the load writes
     values into the wrong columns and every downstream number is nonsense. *)
  List.iter
    (fun table ->
       let from_gen = Granary_tpc.Tpcc_gen.column_names ~table in
       let ddl =
         List.find
           (fun s ->
              let needle = "CREATE TABLE " ^ table ^ " " in
              String.length s > String.length needle
              && String.equal (String.sub s 0 (String.length needle)) needle)
           Granary_tpc.Tpcc_schema.ddl
       in
       (* Parse the column names out of the CREATE TABLE body: the first token
          of each comma-separated clause, skipping PRIMARY KEY clauses. *)
       ignore ddl;
       Alcotest.(check bool)
         (table ^ ": has columns")
         true
         (List.length from_gen > 0))
    Granary_tpc.Tpcc_schema.tables
;;

let test_row_counts_landed () =
  with_loaded (fun e gen ->
    List.iter
      (fun table ->
         let expected = Granary_tpc.Tpcc_gen.row_count gen ~table in
         let actual =
           match
             Granary_tpc.Granary_engine.query_rows
               e
               (Printf.sprintf "SELECT COUNT(*) FROM %s" table)
           with
           | [ [ n ] ] -> int_of_string n
           | _ -> Alcotest.fail (table ^ ": unexpected COUNT shape")
         in
         Alcotest.(check int) (table ^ ": rows loaded") expected actual)
      Granary_tpc.Tpcc_schema.tables)
;;

let test_initial_state_is_consistent () =
  with_loaded (fun e _gen ->
    List.iter
      (fun c ->
         let rows = Granary_tpc.Granary_engine.query_rows e c.C.sql in
         match C.classify c ~rows with
         | C.Holds -> ()
         | C.Violated report -> Alcotest.fail report
         | C.Not_run -> Alcotest.fail "condition did not run")
      C.conditions)
;;

let () =
  Alcotest.run
    "tpcc_load"
    [ ( "schema"
      , [ Alcotest.test_case "columns match the generator" `Quick test_columns_match_gen ] )
    ; ( "load"
      , [ Alcotest.test_case "row counts landed" `Slow test_row_counts_landed
        ; Alcotest.test_case "initial state is consistent" `Slow
            test_initial_state_is_consistent
        ] )
    ]
;;
```

Complete `test_columns_match_gen` properly: parse each `CREATE TABLE` body, split on commas at depth zero, take the first token of each clause, skip clauses beginning `PRIMARY KEY`, and `Alcotest.(check (list string))` the result against `Tpcc_gen.column_names ~table`. `test/test_tpch_load.ml` already does exactly this for TPC-H — read it and follow it rather than inventing a second parser.

Add to `test/dune`:

```
(test
 (name test_tpcc_load)
 (modules test_tpcc_load)
 (libraries granary_tpc alcotest unix))
```

- [ ] **Step 2: Run the test**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpcc_load.exe`
Expected: PASS. If a `CREATE TABLE` is rejected, or a consistency condition fails on the freshly generated state, **stop and diagnose** — a condition failing here is either a generator bug or one of the known correlated-subquery bugs (#485, #492). Determine which before changing anything, and file a Forgejo issue if it is the engine.

- [ ] **Step 3: Commit**

```bash
git add test/test_tpcc_load.ml test/dune
git commit -m "test(#500): TPC-C load integration — schema, row counts, initial consistency"
```

---

### Task 9: Smoke test — run every profile against granary and pin the verdicts

**Files:**
- Create: `test/test_tpcc_smoke.ml`
- Modify: `test/tpc/tpcc_txn.ml` (set the real verdicts), `test/dune`

**Interfaces:**
- Consumes: everything above.
- Produces: the measured `verdict` for each of the five profiles — the deliverable the spec calls "an output of this work, not an input".

This is where verdicts are determined. Build an `ops` backed by `Granary_engine` (via `Tpcc_txn.render`), run each profile a few hundred times against a loaded W=1 database, and see what happens.

- [ ] **Step 1: Write the test**

Create `test/test_tpcc_smoke.ml`. It loads W=1 as in Task 8, builds

```ocaml
let engine_ops e =
  { Granary_tpc.Tpcc_txn.query =
      (fun s -> Lwt.return (Granary_tpc.Granary_engine.query_rows e (Granary_tpc.Tpcc_txn.render s)))
  ; Granary_tpc.Tpcc_txn.exec =
      (fun s -> Granary_tpc.Granary_engine.exec e (Granary_tpc.Tpcc_txn.render s); Lwt.return_unit)
  }
;;
```

and then, for each profile in `Tpcc_txn.all`, runs 200 transactions with inputs from a seeded `Tpc_rand`, asserting:

1. **Every profile whose verdict is not `Skipped` completes without raising.** A profile that raises must be investigated and either fixed (if the harness is wrong) or marked `Skipped "#NNN"` with an issue filed (if the engine cannot express it).
2. **All four consistency conditions still hold afterwards.** This is the real assertion of the whole PR: 1,000 transactions across five profiles must leave the database consistent.
3. **The mix ran what it claims:** count the transactions actually executed per profile and assert it matches what was requested.

Note that `Granary_engine` is synchronous and this test is single-threaded — that is fine and intended. Concurrency is PR 2's problem; this test isolates "do the profiles work at all" from "do they work under contention".

Add to `test/dune`:

```
(test
 (name test_tpcc_smoke)
 (modules test_tpcc_smoke)
 (libraries granary_tpc alcotest lwt lwt.unix unix))
```

- [ ] **Step 2: Run it and record what actually happens**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpcc_smoke.exe`

For each profile that fails, capture the exact error, reduce it to the smallest SQL that reproduces it, and **file a Forgejo issue**:

```sh
~/.local/bin/forgejo issue create IoTReadyNext/granary \
  --title="TPC-C <profile>: <the missing capability>" \
  --body="Found by the #500 TPC-C harness. Minimal repro: …"
```

- [ ] **Step 3: Set the real verdicts**

Replace the placeholder `Native` verdicts in `test/tpc/tpcc_txn.ml` with what Step 2 measured: `Native` for a profile that runs as the spec writes it, `Rewritten "<what changed and why>"` for one that needed a documented transformation, `Skipped "#NNN"` for one granary cannot express. Every `Rewritten` and `Skipped` string must be specific enough that a reader can tell what the engine could not do.

- [ ] **Step 4: Re-run the full test suite**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test`
Expected: PASS across the whole suite, including the pre-existing TPC-H tests.

- [ ] **Step 5: Commit**

```bash
git add test/test_tpcc_smoke.ml test/tpc/tpcc_txn.ml test/dune
git commit -m "test(#500): TPC-C profile smoke run; verdicts set from measurement"
```

---

### Task 10: Formatting, lint, and the PR

**Files:**
- Modify: whatever `check-fmt.sh --fix` rewrites.

- [ ] **Step 1: Format**

```bash
sh scripts/check-fmt.sh --fix
sh scripts/check-fmt.sh
```

Read the **final summary line**, not the exit code. `◐ … Dune files UNVERIFIED` means the dune-file check was skipped.

- [ ] **Step 2: Verify the dune files in the main checkout**

This PR modifies `test/dune`, so the `◐` case applies and this step is mandatory:

```bash
cd /home/tej/projects/sqlite_ocaml_port
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build @fmt
```

- [ ] **Step 3: merlint**

```bash
cd /home/tej/projects/sqlite_ocaml_port/.worktrees/500-tpcc
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
```

Expected: 0 issues for the new files. The pre-existing `sqlite3 not found` warning is expected.

- [ ] **Step 4: Full test run**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test
```

Expected: PASS.

- [ ] **Step 5: Commit any formatting changes and open the PR**

```bash
git add -A && git commit -m "chore(#500): formatting"
git push origin feat/500-tpcc-bench
~/.local/bin/forgejo pr create IoTReadyNext/granary \
  --title="feat(#500): TPC-C-derived benchmark, part 1 — data and transactions" \
  --head=feat/500-tpcc-bench \
  --base=main \
  --body="$(cat <<'EOF'
## Summary
- Schema, deterministic population, the five transaction profiles, and the
  clause-3.3 consistency-condition oracle for the TPC-C-derived OLTP benchmark.
- No concurrency and no `sqlite3` dependency: the driver and the reference
  engine are PR 2.
- Design: `docs/superpowers/specs/2026-08-01-500-tpcc-benchmark-design.md`

## Verdicts measured
<one line per profile: Native / Rewritten / Skipped, with issue numbers>

## Test plan
- [ ] `dune test` passes
- [ ] `sh scripts/check-fmt.sh` reports parity with CI's @fmt gate
- [ ] `dune build @fmt` in the main checkout (this PR touches test/dune)
- [ ] merlint reports 0 issues for the new files
- [ ] 1,000 transactions across five profiles leave all four consistency
      conditions holding

Refs #500, #482
EOF
)"
```

Note this PR adds **no** `sqlite3` reference, so the `#370` no-real-SQLite policy allowlist is **not** needed here. It becomes mandatory in PR 2, when `test/bench_tpcc.ml` arrives.

---

## Self-Review

**Spec coverage.** Every section of the design spec that belongs to PR 1 has a task: shared literal rendering (Task 1); the two `Tpc_rand` primitives (Task 2); the 9-table schema and indexes (Task 3); population with the spec's row counts and the item-not-scaled rule (Task 4); the `Load` functor (Task 4 step 5); the four consistency conditions with `Not_run` failing (Task 5); the profile catalogue, weights, redistribution on `Skipped`, and the 1% rollback (Task 6); the five profiles' control flow and transaction boundaries (Task 7); initial-state consistency checked before any transaction (Task 8); verdicts determined by measurement, not prediction, with issues filed for gaps (Task 9). The spec's driver, `bench_tpcc.ml`, CSV output, `lock_wait_ms`, warm-up, disk guard, CI smoke tier, `#370` allowlist, and `docs/benchmarks/BENCHMARKS-TPCC.md` are all PR 2 and deliberately absent here.

**Placeholders.** Task 4's test contains a deliberately incomplete `test_order_carrier_null_above_2100`, and Task 8's `test_columns_match_gen` is a stub — both are called out in prose immediately below the code with what to write instead, and Task 8 points at `test/test_tpch_load.ml` as the working reference. Task 9's verdict strings are intentionally unwritten: writing them now would be predicting a measurement, which the spec forbids.

**Type consistency.** `Tpc_value.t` is the value type everywhere; `Tpcc_gen.iter_rows` produces `Tpc_value.t array` and `Tpcc_schema.Load` renders it through `Tpc_value.literal`. `Tpcc_txn.stmt` carries `Tpc_value.t list` params and `render` goes through the same function. `Tpcc_check.classify` takes `~rows:string list list`, which is what both `Granary_engine.query_rows` and `Bench_report.ENGINE.query_rows` return. `Tpcc_schema.tables` and `Tpcc_gen.column_names` are the single source of table and column order and are cross-checked in Task 8.
