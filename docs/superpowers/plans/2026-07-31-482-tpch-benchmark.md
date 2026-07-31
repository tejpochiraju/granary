# TPC-H-Derived Benchmark Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a TPC-H-derived analytic benchmark for granary — a pure-OCaml deterministic data generator, the 22 queries expressed in granary's SQL dialect, and a runner that times them and cross-checks every answer against reference C SQLite.

**Architecture:** A new non-public dune library `test/tpc/` holds the generator, schema, queries, and a shared reporting layer. A separate `(optional)` executable `test/bench_tpch.ml` drives it and owns the `sqlite3` dependency, so the library itself stays sqlite3-free and unit-testable everywhere. Output is CSV to stdout in the style of the existing `test/bench_compare.ml`.

**Tech Stack:** OCaml 5.4, Lwt, Alcotest, QCheck, `sqlite3` opam library (optional), dune, podman dev container.

**Spec:** `docs/superpowers/specs/2026-07-31-482-tpc-benchmarks-design.md`
**Issue:** #482
**Branch/worktree:** `feat/482-tpc-benchmarks` in `.worktrees/482-tpc-bench` (already created)

## Global Constraints

- **All work happens in the worktree** `.worktrees/482-tpc-bench`. Never commit to `main`. Push protection enforces this.
- **Never call `dune` on the host.** Every build/test command runs in the dev container:
  `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/<name>.exe`
  run from the worktree root, so `$(pwd)` is the worktree path.
- **Formatting:** `sh scripts/check-fmt.sh` before every commit. Read its final summary line: `✓` means full parity with CI; `◐` means dune files were NOT verified. If you touched any `dune` file, additionally run `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build @fmt` **from the main checkout** at `/home/tej/projects/sqlite_ocaml_port`.
- **merlint:** every module in a dune `library` needs an `.mli`, and every public `val` in an `.mli` needs a `(** … *)` doc comment (not `(* … *)`). Max nesting depth 4. Run `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint` before pushing; expect 0 issues for new files.
- **Naming:** the benchmark is **TPC-H-derived**, never presented as an audited TPC result. Env vars are `GRANARY_TPCH_*` and `GRANARY_TPC_*`.
- **Money is REAL** in both engines. Cross-check compares REAL columns with a relative epsilon of `1e-9` and an absolute floor of `1e-6`.
- **Dates are TEXT** in `'YYYY-MM-DD'` form.
- **Not a timing gate.** No test asserts a latency or throughput bound. Correctness assertions are fine and expected.
- Commit messages end with:
  `Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>`

---

## File Structure

| Path | Responsibility |
|---|---|
| `test/tpc/dune` | Library stanza for `granary_tpc` |
| `test/tpc/tpc_rand.ml{,i}` | Seeded PRNG and the TPC-H spec's random primitives |
| `test/tpc/tpch_text.ml{,i}` | The spec's §4.2.2.1 text-pool grammar |
| `test/tpc/tpch_gen.ml{,i}` | Row generators for the 8 TPC-H tables |
| `test/tpc/tpch_schema.ml{,i}` | DDL, indexes, and the loader |
| `test/tpc/tpch_queries.ml{,i}` | Q1–Q22 SQL text plus per-query verdict |
| `test/tpc/bench_report.ml{,i}` | Engine signature, timing, CSV emitter |
| `test/bench_tpch.ml` | Executable: runs the benchmark, owns the sqlite3 cross-check |
| `test/test_tpch_gen.ml` | Alcotest + QCheck tests for the generator |
| `test/test_tpch_smoke.ml` | Integration test: SF 0.001, all runnable queries, cross-check |
| `test/dune` | New stanzas for the two test executables and `bench_tpch` |
| `docs/BENCHMARKS-TPCH.md` | How to run it, what the numbers mean, what "derived" means |

---

### Task 1: Shared reporting layer

Establishes the engine signature and CSV emitter that the runner and the future TPC-C plan both consume. Deliberately sqlite3-free so the library builds and tests without it.

**Files:**
- Create: `test/tpc/dune`
- Create: `test/tpc/bench_report.ml`, `test/tpc/bench_report.mli`
- Create: `test/test_bench_report.ml`
- Modify: `test/dune` (add the `test_bench_report` stanza)

**Interfaces:**
- Consumes: nothing (first task).
- Produces:
  - `module type Bench_report.ENGINE` with `type t`, `val name : string`, `val open_db : dir:string -> t`, `val exec : t -> string -> unit`, `val query_rows : t -> string -> string list list`, `val close : t -> unit`
  - `Bench_report.time_it : (unit -> 'a) -> 'a * float * float` — returns result, wall seconds, cpu seconds
  - `Bench_report.Csv.header : string list -> string`
  - `Bench_report.Csv.row : string list -> string`
  - `Bench_report.real_eq : float -> float -> bool`
  - `Bench_report.env_int : string -> int -> int`, `Bench_report.env_float : string -> float -> float`, `Bench_report.env_str : string -> string -> string`

- [ ] **Step 1: Write the failing test**

Create `test/test_bench_report.ml`:

```ocaml
let test_csv_row_quotes_commas () =
  Alcotest.(check string)
    "comma-bearing field is quoted"
    {|a,"b,c",d|}
    (Granary_tpc.Bench_report.Csv.row [ "a"; "b,c"; "d" ])
;;

let test_csv_row_escapes_quotes () =
  Alcotest.(check string)
    "embedded quote is doubled"
    {|a,"say ""hi""",c|}
    (Granary_tpc.Bench_report.Csv.row [ "a"; {|say "hi"|}; "c" ])
;;

let test_real_eq_relative () =
  Alcotest.(check bool)
    "1e9 vs 1e9+1e-3 is equal within relative epsilon"
    true
    (Granary_tpc.Bench_report.real_eq 1e9 (1e9 +. 1e-3));
  Alcotest.(check bool)
    "1.0 vs 1.1 is not equal"
    false
    (Granary_tpc.Bench_report.real_eq 1.0 1.1)
;;

let test_real_eq_near_zero () =
  Alcotest.(check bool)
    "values under the absolute floor compare equal"
    true
    (Granary_tpc.Bench_report.real_eq 0.0 1e-9)
;;

let test_time_it_returns_result () =
  let v, wall, cpu = Granary_tpc.Bench_report.time_it (fun () -> 6 * 7) in
  Alcotest.(check int) "result passes through" 42 v;
  Alcotest.(check bool) "wall is non-negative" true (wall >= 0.0);
  Alcotest.(check bool) "cpu is non-negative" true (cpu >= 0.0)
;;

let test_env_int_default () =
  Alcotest.(check int)
    "absent var falls back to default"
    99
    (Granary_tpc.Bench_report.env_int "GRANARY_TPC_DEFINITELY_UNSET" 99)
;;

let () =
  Alcotest.run
    "bench_report"
    [ ( "csv"
      , [ Alcotest.test_case "quotes commas" `Quick test_csv_row_quotes_commas
        ; Alcotest.test_case "escapes quotes" `Quick test_csv_row_escapes_quotes
        ] )
    ; ( "compare"
      , [ Alcotest.test_case "relative epsilon" `Quick test_real_eq_relative
        ; Alcotest.test_case "absolute floor" `Quick test_real_eq_near_zero
        ] )
    ; ("timing", [ Alcotest.test_case "passes result through" `Quick test_time_it_returns_result ])
    ; ("env", [ Alcotest.test_case "int default" `Quick test_env_int_default ])
    ]
;;
```

- [ ] **Step 2: Create the dune stanzas**

Create `test/tpc/dune`:

```
; TPC-derived benchmark support library (#482).  Deliberately free of any
; `sqlite3` dependency: the cross-check engine lives in the executable, so this
; library builds and its tests run even where `sqlite3` is absent.

(library
 (name granary_tpc)
 (libraries granary granary.store granary.unix lwt lwt.unix unix))
```

Append to `test/dune`:

```
(test
 (name test_bench_report)
 (modules test_bench_report)
 (libraries granary_tpc alcotest))
```

- [ ] **Step 3: Run the test to verify it fails**

Run from the worktree root:
```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_bench_report.exe
```
Expected: FAIL — `Unbound module Granary_tpc` (or unbound `Bench_report` members).

- [ ] **Step 4: Write the interface**

Create `test/tpc/bench_report.mli`:

```ocaml
(** Shared reporting layer for the TPC-derived benchmarks (#482).

    Holds the cross-engine signature, wall/CPU timing, CSV emission, and the
    float-comparison rule used by the answer cross-check.  Contains no
    [sqlite3] dependency — reference-engine implementations live in the
    benchmark executables. *)

(** A database engine under benchmark.  Both granary and the reference C
    SQLite satisfy this; the harness is written once against it. *)
module type ENGINE = sig
  type t

  (** Short engine label used in the [engine] CSV column. *)
  val name : string

  (** [open_db ~dir] creates a fresh database beneath [dir], removing any
      previous database files there. *)
  val open_db : dir:string -> t

  (** [exec t sql] runs a statement for effect.  Raises on error. *)
  val exec : t -> string -> unit

  (** [query_rows t sql] runs a query and returns every row, each value
      rendered by the engine's canonical text conversion.  Raises on error. *)
  val query_rows : t -> string -> string list list

  (** [close t] releases the engine's resources. *)
  val close : t -> unit
end

(** [time_it f] runs [f] and returns its result with elapsed wall-clock
    seconds and consumed CPU seconds (user + system). *)
val time_it : (unit -> 'a) -> 'a * float * float

(** [real_eq a b] is the benchmark's float equality: equal within a relative
    epsilon of [1e-9], or both within [1e-6] of zero. *)
val real_eq : float -> float -> bool

(** [env_int key default] reads an integer environment variable, falling back
    to [default] when unset or unparseable. *)
val env_int : string -> int -> int

(** [env_float key default] reads a float environment variable, falling back
    to [default] when unset or unparseable. *)
val env_float : string -> float -> float

(** [env_str key default] reads a string environment variable, falling back to
    [default] when unset or empty. *)
val env_str : string -> string -> string

(** [host_label ()] is the CSV [host] column: [GRANARY_TPC_HOST] when set,
    otherwise the system hostname, otherwise ["unknown"]. *)
val host_label : unit -> string

(** RFC 4180 CSV emission. *)
module Csv : sig
  (** [row fields] renders one CSV line, quoting fields that contain a comma,
      a double quote, or a newline, and doubling embedded quotes.  No trailing
      newline. *)
  val row : string list -> string

  (** [header names] renders the header line.  Identical to {!row}; named
      separately so call sites read clearly. *)
  val header : string list -> string
end
```

- [ ] **Step 5: Write the implementation**

Create `test/tpc/bench_report.ml`:

```ocaml
module type ENGINE = sig
  type t

  val name : string
  val open_db : dir:string -> t
  val exec : t -> string -> unit
  val query_rows : t -> string -> string list list
  val close : t -> unit
end

let cpu_now () =
  let t = Unix.times () in
  t.Unix.tms_utime +. t.Unix.tms_stime
;;

let time_it f =
  let w0 = Unix.gettimeofday () in
  let c0 = cpu_now () in
  let v = f () in
  let wall = Unix.gettimeofday () -. w0 in
  let cpu = cpu_now () -. c0 in
  v, wall, cpu
;;

let rel_epsilon = 1e-9
let abs_floor = 1e-6

let real_eq a b =
  let d = Float.abs (a -. b) in
  if d <= abs_floor
  then true
  else (
    let scale = Float.max (Float.abs a) (Float.abs b) in
    d <= rel_epsilon *. scale)
;;

let env_int key default =
  match Sys.getenv_opt key with
  | Some s ->
    (try int_of_string s with
     | _ -> default)
  | None -> default
;;

let env_float key default =
  match Sys.getenv_opt key with
  | Some s ->
    (try float_of_string s with
     | _ -> default)
  | None -> default
;;

let env_str key default =
  match Sys.getenv_opt key with
  | Some s when s <> "" -> s
  | _ -> default
;;

let host_label () =
  env_str
    "GRANARY_TPC_HOST"
    (try Unix.gethostname () with
     | _ -> "unknown")
;;

module Csv = struct
  let needs_quoting s =
    String.exists
      (function
        | ',' | '"' | '\n' | '\r' -> true
        | _ -> false)
      s
  ;;

  let quote s =
    let buf = Buffer.create (String.length s + 2) in
    Buffer.add_char buf '"';
    String.iter
      (fun c ->
         if c = '"' then Buffer.add_string buf "\"\"" else Buffer.add_char buf c)
      s;
    Buffer.add_char buf '"';
    Buffer.contents buf
  ;;

  let field s = if needs_quoting s then quote s else s
  let row fields = String.concat "," (List.map field fields)
  let header names = row names
end
```

- [ ] **Step 6: Run the test to verify it passes**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_bench_report.exe
```
Expected: PASS, all 6 test cases.

- [ ] **Step 7: Format, lint, commit**

```bash
sh scripts/check-fmt.sh --fix
sh scripts/check-fmt.sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
cd /home/tej/projects/sqlite_ocaml_port && \
  podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build @fmt
```
The last command verifies the new `test/tpc/dune` and modified `test/dune` — required, because `check-fmt.sh` reports `◐ Dune files UNVERIFIED` inside a worktree.

```bash
git add test/tpc/dune test/tpc/bench_report.ml test/tpc/bench_report.mli \
        test/test_bench_report.ml test/dune
git commit -m "feat(#482): shared engine signature, timing, and CSV for TPC benchmarks

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 2: TPC-H random primitives

The spec's data distributions rest on four primitives. Getting them right is what makes the dataset comparable to published results, so they are built and tested before any table generator.

**Files:**
- Create: `test/tpc/tpc_rand.ml`, `test/tpc/tpc_rand.mli`
- Create: `test/test_tpc_rand.ml`
- Modify: `test/dune`

**Interfaces:**
- Consumes: nothing from Task 1.
- Produces:
  - `type Tpc_rand.t`
  - `Tpc_rand.create : seed:int -> t`
  - `Tpc_rand.int_between : t -> lo:int -> hi:int -> int` (inclusive both ends)
  - `Tpc_rand.float_between : t -> lo:float -> hi:float -> decimals:int -> float`
  - `Tpc_rand.a_string : t -> lo:int -> hi:int -> string`
  - `Tpc_rand.pick : t -> string array -> string`
  - `Tpc_rand.phone : t -> nation:int -> string`

- [ ] **Step 1: Write the failing test**

Create `test/test_tpc_rand.ml`:

```ocaml
let seeded () = Granary_tpc.Tpc_rand.create ~seed:42

let test_int_between_in_range () =
  let r = seeded () in
  for _ = 1 to 10_000 do
    let v = Granary_tpc.Tpc_rand.int_between r ~lo:3 ~hi:7 in
    Alcotest.(check bool) "within [3,7]" true (v >= 3 && v <= 7)
  done
;;

let test_int_between_hits_both_ends () =
  let r = seeded () in
  let saw_lo = ref false
  and saw_hi = ref false in
  for _ = 1 to 10_000 do
    match Granary_tpc.Tpc_rand.int_between r ~lo:3 ~hi:7 with
    | 3 -> saw_lo := true
    | 7 -> saw_hi := true
    | _ -> ()
  done;
  Alcotest.(check bool) "lo is reachable" true !saw_lo;
  Alcotest.(check bool) "hi is reachable" true !saw_hi
;;

let test_int_between_singleton () =
  let r = seeded () in
  Alcotest.(check int) "lo = hi yields lo" 5 (Granary_tpc.Tpc_rand.int_between r ~lo:5 ~hi:5)
;;

let test_same_seed_same_sequence () =
  let a = Granary_tpc.Tpc_rand.create ~seed:7 in
  let b = Granary_tpc.Tpc_rand.create ~seed:7 in
  for _ = 1 to 1000 do
    Alcotest.(check int)
      "streams agree"
      (Granary_tpc.Tpc_rand.int_between a ~lo:0 ~hi:1_000_000)
      (Granary_tpc.Tpc_rand.int_between b ~lo:0 ~hi:1_000_000)
  done
;;

let test_different_seed_differs () =
  let a = Granary_tpc.Tpc_rand.create ~seed:1 in
  let b = Granary_tpc.Tpc_rand.create ~seed:2 in
  let draw r = List.init 50 (fun _ -> Granary_tpc.Tpc_rand.int_between r ~lo:0 ~hi:1_000_000) in
  Alcotest.(check bool) "streams differ" false (draw a = draw b)
;;

let test_a_string_length_and_alphabet () =
  let r = seeded () in
  for _ = 1 to 2000 do
    let s = Granary_tpc.Tpc_rand.a_string r ~lo:10 ~hi:20 in
    let n = String.length s in
    Alcotest.(check bool) "length within bounds" true (n >= 10 && n <= 20);
    String.iter
      (fun c ->
         let ok =
           (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = ','
           || c = ' '
         in
         Alcotest.(check bool) "char is in the spec alphabet" true ok)
      s
  done
;;

let test_float_between_decimals () =
  let r = seeded () in
  for _ = 1 to 2000 do
    let v = Granary_tpc.Tpc_rand.float_between r ~lo:1.0 ~hi:2.0 ~decimals:2 in
    Alcotest.(check bool) "within bounds" true (v >= 1.0 && v <= 2.0);
    let scaled = v *. 100.0 in
    Alcotest.(check bool)
      "quantized to 2 decimals"
      true
      (Float.abs (scaled -. Float.round scaled) < 1e-6)
  done
;;

let test_phone_shape () =
  let r = seeded () in
  let s = Granary_tpc.Tpc_rand.phone r ~nation:12 in
  Alcotest.(check int) "phone is 15 chars" 15 (String.length s);
  Alcotest.(check string) "country code is nation + 10" "22" (String.sub s 0 2);
  Alcotest.(check char) "first separator" '-' s.[2];
  Alcotest.(check char) "second separator" '-' s.[6];
  Alcotest.(check char) "third separator" '-' s.[10]
;;

let () =
  Alcotest.run
    "tpc_rand"
    [ ( "int_between"
      , [ Alcotest.test_case "in range" `Quick test_int_between_in_range
        ; Alcotest.test_case "reaches both ends" `Quick test_int_between_hits_both_ends
        ; Alcotest.test_case "singleton range" `Quick test_int_between_singleton
        ] )
    ; ( "determinism"
      , [ Alcotest.test_case "same seed" `Quick test_same_seed_same_sequence
        ; Alcotest.test_case "different seed" `Quick test_different_seed_differs
        ] )
    ; ("a_string", [ Alcotest.test_case "length and alphabet" `Quick test_a_string_length_and_alphabet ])
    ; ("float", [ Alcotest.test_case "decimal quantization" `Quick test_float_between_decimals ])
    ; ("phone", [ Alcotest.test_case "shape" `Quick test_phone_shape ])
    ]
;;
```

Append to `test/dune`:

```
(test
 (name test_tpc_rand)
 (modules test_tpc_rand)
 (libraries granary_tpc alcotest))
```

- [ ] **Step 2: Run the test to verify it fails**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpc_rand.exe
```
Expected: FAIL — `Unbound module Granary_tpc.Tpc_rand`.

- [ ] **Step 3: Write the interface**

Create `test/tpc/tpc_rand.mli`:

```ocaml
(** Seeded pseudo-random primitives for TPC-derived data generation (#482).

    A self-contained linear congruential generator, not [Stdlib.Random], so a
    given seed produces the same dataset on every platform and OCaml version.
    Generation must be reproducible for benchmark numbers to be comparable
    across runs and machines. *)

type t

(** [create ~seed] starts a generator stream.  Distinct seeds give distinct
    streams; equal seeds give byte-identical output. *)
val create : seed:int -> t

(** [int_between r ~lo ~hi] is a uniform integer in the closed interval
    [\[lo, hi\]].  Requires [lo <= hi]; raises [Invalid_argument] otherwise. *)
val int_between : t -> lo:int -> hi:int -> int

(** [float_between r ~lo ~hi ~decimals] is a uniform value in [\[lo, hi\]]
    quantized to [decimals] places — the spec's fixed-point money and rate
    columns. *)
val float_between : t -> lo:float -> hi:float -> decimals:int -> float

(** [a_string r ~lo ~hi] is the spec's random alphanumeric string: a length
    uniform in [\[lo, hi\]], each character drawn uniformly from the 64-symbol
    alphabet (letters, digits, comma, space). *)
val a_string : t -> lo:int -> hi:int -> string

(** [pick r choices] selects one element uniformly.  Raises
    [Invalid_argument] on an empty array. *)
val pick : t -> string array -> string

(** [phone r ~nation] builds the spec's 15-character phone number
    ["CC-AAA-BBB-CCCC"], where the country code is [nation + 10]. *)
val phone : t -> nation:int -> string
```

- [ ] **Step 4: Write the implementation**

Create `test/tpc/tpc_rand.ml`:

```ocaml
(* A 48-bit linear congruential generator with the parameters used by
   POSIX drand48.  Chosen over Stdlib.Random so that a seed pins the dataset
   independently of the stdlib's PRNG implementation, which is not stable
   across OCaml releases. *)

type t = { mutable state : int }

let modulus = 1 lsl 48
let multiplier = 0x5DEECE66D
let increment = 0xB

let create ~seed = { state = (seed lxor multiplier) land (modulus - 1) }

let next_bits t bits =
  t.state <- ((t.state * multiplier) + increment) land (modulus - 1);
  t.state lsr (48 - bits)
;;

(* 31 random bits, i.e. a non-negative int under 2^31. *)
let next_int t = next_bits t 31

let int_between t ~lo ~hi =
  if lo > hi then invalid_arg "Tpc_rand.int_between: lo > hi";
  let span = hi - lo + 1 in
  lo + (next_int t mod span)
;;

let float_between t ~lo ~hi ~decimals =
  let scale = int_of_float (10.0 ** float_of_int decimals) in
  let lo_i = int_of_float (Float.round (lo *. float_of_int scale)) in
  let hi_i = int_of_float (Float.round (hi *. float_of_int scale)) in
  float_of_int (int_between t ~lo:lo_i ~hi:hi_i) /. float_of_int scale
;;

let alphabet =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789, "
;;

let a_string t ~lo ~hi =
  let n = int_between t ~lo ~hi in
  String.init n (fun _ -> alphabet.[int_between t ~lo:0 ~hi:(String.length alphabet - 1)])
;;

let pick t choices =
  let n = Array.length choices in
  if n = 0 then invalid_arg "Tpc_rand.pick: empty choices";
  choices.(int_between t ~lo:0 ~hi:(n - 1))
;;

let phone t ~nation =
  Printf.sprintf
    "%02d-%03d-%03d-%04d"
    (nation + 10)
    (int_between t ~lo:100 ~hi:999)
    (int_between t ~lo:100 ~hi:999)
    (int_between t ~lo:1000 ~hi:9999)
;;
```

Note the alphabet is 64 characters: 26 lowercase + 26 uppercase + 10 digits + comma + space.

- [ ] **Step 5: Run the test to verify it passes**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpc_rand.exe
```
Expected: PASS, all 8 test cases.

- [ ] **Step 6: Add QCheck properties**

Append to `test/test_tpc_rand.ml`, before the `Alcotest.run` call, and add the property group to the run list:

```ocaml
let prop_int_between_respects_bounds =
  QCheck.Test.make
    ~count:2000
    ~name:"int_between stays inside its closed interval for any seed and range"
    QCheck.(triple small_int small_nat small_nat)
    (fun (seed, a, b) ->
       let lo = min a b
       and hi = max a b in
       let r = Granary_tpc.Tpc_rand.create ~seed in
       let v = Granary_tpc.Tpc_rand.int_between r ~lo ~hi in
       v >= lo && v <= hi)
;;

let prop_a_string_length =
  QCheck.Test.make
    ~count:2000
    ~name:"a_string length lands inside the requested bounds for any seed"
    QCheck.(triple small_int (int_range 0 40) (int_range 0 40))
    (fun (seed, a, b) ->
       let lo = min a b
       and hi = max a b in
       let r = Granary_tpc.Tpc_rand.create ~seed in
       let s = Granary_tpc.Tpc_rand.a_string r ~lo ~hi in
       String.length s >= lo && String.length s <= hi)
;;

let prop_determinism =
  QCheck.Test.make
    ~count:500
    ~name:"two generators on the same seed emit identical streams"
    QCheck.small_int
    (fun seed ->
       let a = Granary_tpc.Tpc_rand.create ~seed in
       let b = Granary_tpc.Tpc_rand.create ~seed in
       let draw r = List.init 20 (fun _ -> Granary_tpc.Tpc_rand.int_between r ~lo:0 ~hi:99999) in
       draw a = draw b)
;;
```

Add to the `Alcotest.run` list:

```ocaml
    ; ( "properties"
      , List.map
          QCheck_alcotest.to_alcotest
          [ prop_int_between_respects_bounds; prop_a_string_length; prop_determinism ] )
```

Update the `test/dune` stanza to add the QCheck libraries:

```
(test
 (name test_tpc_rand)
 (modules test_tpc_rand)
 (libraries granary_tpc alcotest qcheck qcheck-alcotest))
```

- [ ] **Step 7: Run to verify the properties pass**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpc_rand.exe
```
Expected: PASS, including the three property tests.

- [ ] **Step 8: Format, lint, commit**

```bash
sh scripts/check-fmt.sh --fix && sh scripts/check-fmt.sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
cd /home/tej/projects/sqlite_ocaml_port && \
  podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build @fmt
```

```bash
git add test/tpc/tpc_rand.ml test/tpc/tpc_rand.mli test/test_tpc_rand.ml test/dune
git commit -m "feat(#482): seeded TPC random primitives with QCheck properties

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 3: TPC-H text pool

The spec's comment columns are substrings of a grammar-generated text pool. Content matters: Q13 filters on `l_comment`-style text with `NOT LIKE '%special%requests%'` and Q16 on `p_comment LIKE '%Customer%Complaints%'`. A generator that emits arbitrary noise makes those two queries return degenerate results, so the grammar is implemented rather than approximated.

**Files:**
- Create: `test/tpc/tpch_text.ml`, `test/tpc/tpch_text.mli`
- Create: `test/test_tpch_text.ml`
- Modify: `test/dune`

**Interfaces:**
- Consumes: `Tpc_rand.t`, `Tpc_rand.int_between`, `Tpc_rand.pick`.
- Produces:
  - `Tpch_text.pool : Tpc_rand.t -> size:int -> string` — builds the pool once
  - `Tpch_text.substring : pool:string -> Tpc_rand.t -> lo:int -> hi:int -> string`

- [ ] **Step 1: Write the failing test**

Create `test/test_tpch_text.ml`:

```ocaml
let build () =
  let r = Granary_tpc.Tpc_rand.create ~seed:1 in
  Granary_tpc.Tpch_text.pool r ~size:200_000
;;

let test_pool_reaches_requested_size () =
  let p = build () in
  Alcotest.(check bool) "pool is at least the requested size" true (String.length p >= 200_000)
;;

let test_pool_is_deterministic () =
  let a = build () in
  let b = build () in
  Alcotest.(check string) "same seed gives the same pool" a b
;;

let test_pool_contains_grammar_words () =
  let p = build () in
  let contains needle =
    let nl = String.length needle in
    let rec go i = i + nl <= String.length p && (String.sub p i nl = needle || go (i + 1)) in
    go 0
  in
  Alcotest.(check bool) "grammar noun appears" true (contains "packages");
  Alcotest.(check bool) "grammar verb appears" true (contains "sleep")
;;

let test_substring_respects_bounds () =
  let p = build () in
  let r = Granary_tpc.Tpc_rand.create ~seed:9 in
  for _ = 1 to 2000 do
    let s = Granary_tpc.Tpch_text.substring ~pool:p r ~lo:10 ~hi:40 in
    let n = String.length s in
    Alcotest.(check bool) "length within bounds" true (n >= 10 && n <= 40)
  done
;;

let test_special_requests_appears () =
  (* Q13 depends on 'special ... requests' occurring in order-comment text at a
     non-trivial rate; the grammar's adverb/noun phrases produce it. *)
  let p = build () in
  let count_occurrences needle =
    let nl = String.length needle in
    let n = ref 0 in
    for i = 0 to String.length p - nl do
      if String.sub p i nl = needle then incr n
    done;
    !n
  in
  Alcotest.(check bool)
    "the token 'special' occurs in a 200KB pool"
    true
    (count_occurrences "special" > 0);
  Alcotest.(check bool)
    "the token 'requests' occurs in a 200KB pool"
    true
    (count_occurrences "requests" > 0)
;;

let () =
  Alcotest.run
    "tpch_text"
    [ ( "pool"
      , [ Alcotest.test_case "size" `Quick test_pool_reaches_requested_size
        ; Alcotest.test_case "deterministic" `Quick test_pool_is_deterministic
        ; Alcotest.test_case "grammar words" `Quick test_pool_contains_grammar_words
        ; Alcotest.test_case "Q13/Q16 tokens" `Quick test_special_requests_appears
        ] )
    ; ("substring", [ Alcotest.test_case "bounds" `Quick test_substring_respects_bounds ])
    ]
;;
```

Append to `test/dune`:

```
(test
 (name test_tpch_text)
 (modules test_tpch_text)
 (libraries granary_tpc alcotest))
```

- [ ] **Step 2: Run to verify it fails**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpch_text.exe
```
Expected: FAIL — `Unbound module Granary_tpc.Tpch_text`.

- [ ] **Step 3: Write the interface**

Create `test/tpc/tpch_text.mli`:

```ocaml
(** The TPC-H §4.2.2.1 text pool (#482).

    Comment columns in the TPC-H schema are random substrings of a large pool
    of grammar-generated English-like text.  The grammar matters rather than
    being decoration: Q13 and Q16 filter on tokens the grammar produces
    ([special], [requests], [Customer], [Complaints]), so a pool of arbitrary
    noise would make both queries degenerate. *)

(** [pool r ~size] generates at least [size] bytes of grammar text.  The
    result is deterministic in the generator's seed.  Build it once and share
    it across all comment columns — regenerating per row is both slow and
    contrary to the spec. *)
val pool : Tpc_rand.t -> size:int -> string

(** [substring ~pool r ~lo ~hi] takes a random substring of [pool] whose
    length is uniform in [\[lo, hi\]], as the spec's comment columns require. *)
val substring : pool:string -> Tpc_rand.t -> lo:int -> hi:int -> string
```

- [ ] **Step 4: Write the implementation**

Create `test/tpc/tpch_text.ml`. The word lists and sentence forms follow the spec's grammar:

```ocaml
let nouns =
  [| "foxes"; "ideas"; "theodolites"; "pinto beans"; "instructions"; "dependencies"
   ; "excuses"; "platelets"; "asymptotes"; "courts"; "dolphins"; "multipliers"
   ; "sauternes"; "warthogs"; "frets"; "dinos"; "attainments"; "somas"; "Tiresias"
   ; "patterns"; "forges"; "braids"; "hockey players"; "frays"; "warhorses"
   ; "dugouts"; "notornis"; "epitaphs"; "pearls"; "tithes"; "waters"; "orbits"
   ; "gifts"; "sheaves"; "depths"; "sentiments"; "decoys"; "realms"; "pains"
   ; "grouches"; "escapades"; "packages"; "requests"; "accounts"; "deposits"
  |]
;;

let verbs =
  [| "sleep"; "wake"; "are"; "cajole"; "haggle"; "nag"; "use"; "boost"; "affix"
   ; "detect"; "integrate"; "maintain"; "nod"; "was"; "lose"; "sublate"; "solve"
   ; "thrash"; "promise"; "engage"; "hinder"; "print"; "x-ray"; "breach"; "eat"
   ; "grow"; "impress"; "mold"; "poach"; "serve"; "run"; "dazzle"; "snooze"
   ; "doze"; "unwind"; "kindle"; "play"; "hang"; "believe"; "doubt"
  |]
;;

let adjectives =
  [| "furious"; "sly"; "careful"; "blithe"; "quick"; "fluffy"; "slow"; "quiet"
   ; "ruthless"; "thin"; "close"; "dogged"; "daring"; "brave"; "stealthy"
   ; "permanent"; "enticing"; "idle"; "busy"; "regular"; "final"; "ironic"
   ; "even"; "bold"; "silent"; "special"; "pending"; "unusual"; "express"
  |]
;;

let adverbs =
  [| "sometimes"; "always"; "never"; "furiously"; "slyly"; "carefully"; "blithely"
   ; "quickly"; "fluffily"; "slowly"; "quietly"; "ruthlessly"; "thinly"; "closely"
   ; "doggedly"; "daringly"; "bravely"; "stealthily"; "permanently"; "enticingly"
   ; "idly"; "busily"; "regularly"; "finally"; "ironically"; "evenly"; "boldly"
   ; "silently"; "specially"; "pendingly"; "unusually"; "expressly"
  |]
;;

let prepositions =
  [| "about"; "above"; "according to"; "across"; "after"; "against"; "along"
   ; "alongside of"; "among"; "around"; "at"; "atop"; "before"; "behind"; "beneath"
   ; "beside"; "besides"; "between"; "beyond"; "by"; "despite"; "during"; "except"
   ; "for"; "from"; "in place of"; "inside"; "instead of"; "into"; "near"; "of"
   ; "on"; "outside"; "over"; "past"; "since"; "through"; "throughout"; "to"
   ; "toward"; "under"; "until"; "up"; "upon"; "whithout"; "with"; "within"
  |]
;;

let terminators = [| "."; ";"; ":"; "?"; "!"; "--" |]

(* The spec composes a noun phrase, a verb phrase, and a preposition into a
   handful of sentence forms.  Kept to four forms and one level of nesting so
   the module stays within merlint's nesting limit. *)

let noun_phrase r =
  match Tpc_rand.int_between r ~lo:0 ~hi:3 with
  | 0 -> Tpc_rand.pick r nouns
  | 1 -> Tpc_rand.pick r adjectives ^ " " ^ Tpc_rand.pick r nouns
  | 2 -> Tpc_rand.pick r adjectives ^ ", " ^ Tpc_rand.pick r adjectives ^ " " ^ Tpc_rand.pick r nouns
  | _ -> Tpc_rand.pick r adverbs ^ " " ^ Tpc_rand.pick r adjectives ^ " " ^ Tpc_rand.pick r nouns
;;

let verb_phrase r =
  match Tpc_rand.int_between r ~lo:0 ~hi:3 with
  | 0 -> Tpc_rand.pick r verbs
  | 1 -> Tpc_rand.pick r adverbs ^ " " ^ Tpc_rand.pick r verbs
  | 2 -> Tpc_rand.pick r verbs ^ " " ^ Tpc_rand.pick r adverbs
  | _ -> Tpc_rand.pick r adverbs ^ " " ^ Tpc_rand.pick r verbs ^ " " ^ Tpc_rand.pick r adverbs
;;

let prepositional_phrase r = Tpc_rand.pick r prepositions ^ " the " ^ noun_phrase r

let sentence r =
  match Tpc_rand.int_between r ~lo:0 ~hi:4 with
  | 0 -> noun_phrase r ^ " " ^ verb_phrase r ^ " " ^ Tpc_rand.pick r terminators
  | 1 ->
    noun_phrase r ^ " " ^ verb_phrase r ^ " " ^ prepositional_phrase r ^ " "
    ^ Tpc_rand.pick r terminators
  | 2 ->
    noun_phrase r ^ " " ^ verb_phrase r ^ " " ^ noun_phrase r ^ " "
    ^ Tpc_rand.pick r terminators
  | 3 ->
    prepositional_phrase r ^ " " ^ noun_phrase r ^ " " ^ verb_phrase r ^ " "
    ^ Tpc_rand.pick r terminators
  | _ ->
    prepositional_phrase r ^ " " ^ noun_phrase r ^ " " ^ verb_phrase r ^ " "
    ^ prepositional_phrase r ^ " " ^ Tpc_rand.pick r terminators
;;

let pool r ~size =
  let buf = Buffer.create (size + 256) in
  while Buffer.length buf < size do
    Buffer.add_string buf (sentence r);
    Buffer.add_char buf ' '
  done;
  Buffer.contents buf
;;

let substring ~pool r ~lo ~hi =
  let n = Tpc_rand.int_between r ~lo ~hi in
  let max_start = String.length pool - n in
  if max_start <= 0
  then String.sub pool 0 (min n (String.length pool))
  else String.sub pool (Tpc_rand.int_between r ~lo:0 ~hi:max_start) n
;;
```

Note: `"Customer"` and `"Complaints"` for Q16 are not grammar tokens — Q16's predicate applies to `p_comment` in the spec, and the spec seeds those words into supplier comments specifically. Task 4 handles that by injecting the required marker strings into the fixed fraction of `s_comment` values the spec prescribes, rather than relying on the pool.

- [ ] **Step 5: Run to verify it passes**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpch_text.exe
```
Expected: PASS, all 5 test cases. If `test_special_requests_appears` fails, the tokens `special` and `requests` are missing from the word lists above — both are present in `adjectives` and `nouns` respectively, so a failure means a typo in transcription.

- [ ] **Step 6: Format, lint, commit**

```bash
sh scripts/check-fmt.sh --fix && sh scripts/check-fmt.sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
```

```bash
git add test/tpc/tpch_text.ml test/tpc/tpch_text.mli test/test_tpch_text.ml test/dune
git commit -m "feat(#482): TPC-H grammar text pool for comment columns

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 4: TPC-H table generators

**Files:**
- Create: `test/tpc/tpch_gen.ml`, `test/tpc/tpch_gen.mli`
- Create: `test/test_tpch_gen.ml`
- Modify: `test/dune`

**Interfaces:**
- Consumes: `Tpc_rand`, `Tpch_text`.
- Produces:
  - `type Tpch_gen.value = VInt of int | VReal of float | VText of string`
  - `type Tpch_gen.t` — a generator context carrying the seed and shared text pool
  - `Tpch_gen.create : seed:int -> sf:float -> t`
  - `Tpch_gen.row_count : t -> table:string -> int` — expected cardinality
  - `Tpch_gen.iter_rows : t -> table:string -> f:(value array -> unit) -> unit` — streams rows without materializing the table
  - `Tpch_gen.tables : string list` — the 8 table names in load order (parents first)

Cardinalities at scale factor SF, from the spec:

| Table | Rows |
|---|---|
| `region` | 5 (fixed) |
| `nation` | 25 (fixed) |
| `supplier` | 10 000 × SF |
| `part` | 200 000 × SF |
| `partsupp` | 4 per part = 800 000 × SF |
| `customer` | 150 000 × SF |
| `orders` | 1 500 000 × SF |
| `lineitem` | 1–7 per order, averaging 4 ⇒ ≈ 6 000 000 × SF |

`lineitem` is the only table whose count is not a fixed multiple; its test asserts a range, not an equality.

- [ ] **Step 1: Write the failing test**

Create `test/test_tpch_gen.ml`:

```ocaml
module G = Granary_tpc.Tpch_gen

let ctx ?(seed = 42) ?(sf = 0.01) () = G.create ~seed ~sf

let count g ~table =
  let n = ref 0 in
  G.iter_rows g ~table ~f:(fun _ -> incr n);
  !n
;;

let test_fixed_cardinalities () =
  let g = ctx () in
  Alcotest.(check int) "region has 5 rows" 5 (count g ~table:"region");
  Alcotest.(check int) "nation has 25 rows" 25 (count g ~table:"nation")
;;

let test_scaled_cardinalities () =
  let g = ctx ~sf:0.01 () in
  Alcotest.(check int) "supplier" 100 (count g ~table:"supplier");
  Alcotest.(check int) "part" 2000 (count g ~table:"part");
  Alcotest.(check int) "partsupp" 8000 (count g ~table:"partsupp");
  Alcotest.(check int) "customer" 1500 (count g ~table:"customer");
  Alcotest.(check int) "orders" 15000 (count g ~table:"orders")
;;

let test_lineitem_in_spec_range () =
  let g = ctx ~sf:0.01 () in
  let orders = count g ~table:"orders" in
  let li = count g ~table:"lineitem" in
  Alcotest.(check bool) "at least one line per order" true (li >= orders);
  Alcotest.(check bool) "at most seven lines per order" true (li <= orders * 7);
  let avg = float_of_int li /. float_of_int orders in
  Alcotest.(check bool)
    (Printf.sprintf "average lines per order near 4 (got %.2f)" avg)
    true
    (avg >= 3.5 && avg <= 4.5)
;;

let test_row_count_matches_iteration () =
  let g = ctx () in
  List.iter
    (fun table ->
       Alcotest.(check int)
         (table ^ ": row_count agrees with iter_rows")
         (count g ~table)
         (G.row_count g ~table))
    [ "region"; "nation"; "supplier"; "part"; "partsupp"; "customer"; "orders" ]
;;

let digest g ~table =
  let buf = Buffer.create 4096 in
  G.iter_rows g ~table ~f:(fun row ->
    Array.iter
      (fun v ->
         Buffer.add_string
           buf
           (match v with
            | G.VInt i -> string_of_int i
            | G.VReal f -> Printf.sprintf "%.4f" f
            | G.VText s -> s);
         Buffer.add_char buf '\031')
      row;
    Buffer.add_char buf '\030');
  Digest.to_hex (Digest.string (Buffer.contents buf))
;;

let test_determinism_same_seed () =
  let a = ctx ~seed:7 () in
  let b = ctx ~seed:7 () in
  List.iter
    (fun table ->
       Alcotest.(check string)
         (table ^ ": identical for identical seed")
         (digest a ~table)
         (digest b ~table))
    [ "part"; "customer"; "orders"; "lineitem" ]
;;

let test_different_seed_differs () =
  let a = ctx ~seed:1 () in
  let b = ctx ~seed:2 () in
  Alcotest.(check bool)
    "different seeds give different data"
    false
    (digest a ~table:"customer" = digest b ~table:"customer")
;;

let test_nation_references_region () =
  let g = ctx () in
  let bad = ref 0 in
  G.iter_rows g ~table:"nation" ~f:(fun row ->
    match row.(2) with
    | G.VInt rk when rk >= 0 && rk < 5 -> ()
    | _ -> incr bad);
  Alcotest.(check int) "every n_regionkey is a valid region" 0 !bad
;;

let test_lineitem_dates_are_ordered () =
  let g = ctx ~sf:0.01 () in
  (* l_shipdate (col 10), l_commitdate (11), l_receiptdate (12) are TEXT dates;
     the spec requires ship <= receipt. *)
  let bad = ref 0 in
  G.iter_rows g ~table:"lineitem" ~f:(fun row ->
    match row.(10), row.(12) with
    | G.VText ship, G.VText receipt -> if String.compare ship receipt > 0 then incr bad
    | _ -> incr bad);
  Alcotest.(check int) "shipdate never after receiptdate" 0 !bad
;;

let test_date_format () =
  let g = ctx ~sf:0.01 () in
  let re_ok s =
    String.length s = 10
    && s.[4] = '-'
    && s.[7] = '-'
    && String.for_all (fun c -> (c >= '0' && c <= '9') || c = '-') s
  in
  let bad = ref 0 in
  G.iter_rows g ~table:"orders" ~f:(fun row ->
    match row.(4) with
    | G.VText d -> if not (re_ok d) then incr bad
    | _ -> incr bad);
  Alcotest.(check int) "o_orderdate is YYYY-MM-DD" 0 !bad
;;

let () =
  Alcotest.run
    "tpch_gen"
    [ ( "cardinality"
      , [ Alcotest.test_case "fixed tables" `Quick test_fixed_cardinalities
        ; Alcotest.test_case "scaled tables" `Quick test_scaled_cardinalities
        ; Alcotest.test_case "lineitem range" `Quick test_lineitem_in_spec_range
        ; Alcotest.test_case "row_count agrees" `Quick test_row_count_matches_iteration
        ] )
    ; ( "determinism"
      , [ Alcotest.test_case "same seed" `Quick test_determinism_same_seed
        ; Alcotest.test_case "different seed" `Quick test_different_seed_differs
        ] )
    ; ( "integrity"
      , [ Alcotest.test_case "nation -> region" `Quick test_nation_references_region
        ; Alcotest.test_case "lineitem date order" `Quick test_lineitem_dates_are_ordered
        ; Alcotest.test_case "date format" `Quick test_date_format
        ] )
    ]
;;
```

Append to `test/dune`:

```
(test
 (name test_tpch_gen)
 (modules test_tpch_gen)
 (libraries granary_tpc alcotest))
```

- [ ] **Step 2: Run to verify it fails**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpch_gen.exe
```
Expected: FAIL — `Unbound module Granary_tpc.Tpch_gen`.

- [ ] **Step 3: Write the interface**

Create `test/tpc/tpch_gen.mli`:

```ocaml
(** Deterministic TPC-H-derived data generator (#482).

    Emits the eight TPC-H tables at a given scale factor, streaming rows to a
    callback so a large dataset never has to be held in memory.  Output is a
    pure function of [seed] and [sf], which is what makes benchmark numbers
    comparable across runs and machines.

    Money columns are [VReal]: neither granary nor SQLite has DECIMAL, and
    using scaled integers on one side only would make the cross-engine answer
    comparison misleading.  Dates are [VText] in ['YYYY-MM-DD'] form, which
    orders correctly under lexicographic comparison. *)

(** A generated column value. *)
type value =
  | VInt of int (** INTEGER column *)
  | VReal of float (** REAL column — money and rates *)
  | VText of string (** TEXT column — names, codes, dates, comments *)

(** Generator context: seed, scale factor, and the shared text pool. *)
type t

(** [create ~seed ~sf] builds a context at scale factor [sf].  Raises
    [Invalid_argument] if [sf <= 0.0]. *)
val create : seed:int -> sf:float -> t

(** The eight table names in load order — parents before children, so a
    foreign-key-respecting loader can follow this list directly. *)
val tables : string list

(** [row_count t ~table] is the number of rows [iter_rows] will emit.  For
    ["lineitem"] this requires generating the order stream, so it is not
    free.  Raises [Invalid_argument] on an unknown table name. *)
val row_count : t -> table:string -> int

(** [iter_rows t ~table ~f] calls [f] once per generated row, with the row's
    columns in schema declaration order.  Raises [Invalid_argument] on an
    unknown table name. *)
val iter_rows : t -> table:string -> f:(value array -> unit) -> unit

(** [column_names ~table] is the schema declaration order for [table],
    matching the array layout [iter_rows] produces. *)
val column_names : table:string -> string list
```

- [ ] **Step 4: Write the implementation**

Create `test/tpc/tpch_gen.ml`. The scaffolding, date handling, and two worked generators are given below; the remaining six follow the same shape, with per-column value rules taken from the TPC-H specification's §4.2.3 column definitions.

Required structural decisions, all load-bearing for later tasks:

1. **Keys are dense and 0-based** so they can serve as rowid aliases: `r_regionkey` 0–4, `n_nationkey` 0–24, `p_partkey` 0..(200000×SF − 1), and so on.
2. **The text pool is built once** in `create`, sized `max 200_000 (int_of_float (2_000_000.0 *. sf))` bytes, and shared by every comment column.
3. **Each table's generator uses its own `Tpc_rand.t`**, seeded as `seed + table_index`, so generating `orders` alone gives the same rows as generating it as part of a full load. Table index is the position in `tables`.
4. **`lineitem` is generated by walking the same order stream `orders` uses**, with the same per-table seed offset for orders, so the two tables agree on order keys and dates.
5. **`s_comment` marker injection**: the spec requires that 5 out of every 10 000 suppliers carry a `Customer...Complaints` marker and 5 more a `Customer...Recommends` marker inside `s_comment`. Implement this — Q16 counts suppliers by the absence of the Complaints marker, and skipping it makes Q16 meaningless. At SF 0.01 (100 suppliers) this rounds to zero markers, which is expected and fine; the smoke tier does not validate Q16's magnitude, only its answer against SQLite.
6. **Date range** is 1992-01-01 through 1998-12-31 as the spec defines. Implement a small day-number ↔ `YYYY-MM-DD` conversion pair inside the module; do not reach for `Unix.mktime`, whose local-timezone behavior would make output machine-dependent and break determinism.
7. **`l_shipdate <= l_receiptdate`** always, per the spec's offsets from `o_orderdate`.

`column_names` returns, for each table, the standard TPC-H column names in order — `["r_regionkey"; "r_name"; "r_comment"]` for region, and so on through lineitem's 16 columns. These strings are consumed by the loader in Task 5 to build INSERT statements, so they must match `Tpch_schema`'s DDL exactly.

The scaffolding, verbatim:

```ocaml
type value =
  | VInt of int
  | VReal of float
  | VText of string

type t =
  { seed : int
  ; sf : float
  ; pool : string
  }

let tables =
  [ "region"; "nation"; "supplier"; "part"; "partsupp"; "customer"; "orders"; "lineitem" ]
;;

let table_index table =
  match List.find_index (String.equal table) tables with
  | Some i -> i
  | None -> invalid_arg ("Tpch_gen: unknown table " ^ table)
;;

(* Each table draws from its own stream so that generating one table alone
   yields the same rows as generating it inside a full load. *)
let rand_for t ~table = Tpc_rand.create ~seed:(t.seed + table_index table)

let create ~seed ~sf =
  if sf <= 0.0 then invalid_arg "Tpch_gen.create: sf must be positive";
  let pool_size = Stdlib.max 200_000 (int_of_float (2_000_000.0 *. sf)) in
  { seed; sf; pool = Tpch_text.pool (Tpc_rand.create ~seed) ~size:pool_size }
;;

let scaled t n = int_of_float (Float.round (float_of_int n *. t.sf))
```

Date handling — a self-contained civil-date conversion, because `Unix.mktime`
applies the local timezone and would make output machine-dependent:

```ocaml
(* Days since 1970-01-01, by Howard Hinnant's civil_from_days algorithm. *)
let days_from_civil ~y ~m ~d =
  let y = if m <= 2 then y - 1 else y in
  let era = (if y >= 0 then y else y - 399) / 400 in
  let yoe = y - (era * 400) in
  let mp = (m + 9) mod 12 in
  let doy = (((153 * mp) + 2) / 5) + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468
;;

let civil_from_days z =
  let z = z + 719468 in
  let era = (if z >= 0 then z else z - 146096) / 146097 in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let y = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  (if m <= 2 then y + 1 else y), m, d
;;

let date_of_day n =
  let y, m, d = civil_from_days n in
  Printf.sprintf "%04d-%02d-%02d" y m d
;;

(* The spec's date window: 1992-01-01 through 1998-12-31. *)
let date_lo = days_from_civil ~y:1992 ~m:1 ~d:1
let date_hi = days_from_civil ~y:1998 ~m:12 ~d:31
```

Two worked generators showing the fixed-table and scaled-table shapes:

```ocaml
let region_names = [| "AFRICA"; "AMERICA"; "ASIA"; "EUROPE"; "MIDDLE EAST" |]

let gen_region t ~f =
  let r = rand_for t ~table:"region" in
  Array.iteri
    (fun i name ->
       f [| VInt i; VText name; VText (Tpch_text.substring ~pool:t.pool r ~lo:31 ~hi:115) |])
    region_names
;;

let gen_customer t ~f =
  let r = rand_for t ~table:"customer" in
  let n = scaled t 150_000 in
  let segments = [| "AUTOMOBILE"; "BUILDING"; "FURNITURE"; "HOUSEHOLD"; "MACHINERY" |] in
  for i = 0 to n - 1 do
    let nation = Tpc_rand.int_between r ~lo:0 ~hi:24 in
    f
      [| VInt i
       ; VText (Printf.sprintf "Customer#%09d" i)
       ; VText (Tpc_rand.a_string r ~lo:10 ~hi:40)
       ; VInt nation
       ; VText (Tpc_rand.phone r ~nation)
       ; VReal (Tpc_rand.float_between r ~lo:(-999.99) ~hi:9999.99 ~decimals:2)
       ; VText (Tpc_rand.pick r segments)
       ; VText (Tpch_text.substring ~pool:t.pool r ~lo:29 ~hi:116)
      |]
  done
;;
```

`orders` and `lineitem` share a stream: `gen_lineitem` re-derives each order's
key and `o_orderdate` from a generator seeded exactly as `gen_orders`, then
emits 1–7 lines per order with `l_shipdate` in `[o_orderdate + 1,
o_orderdate + 121]`, `l_commitdate` in `[o_orderdate + 30, o_orderdate + 90]`,
and `l_receiptdate` in `[l_shipdate + 1, l_shipdate + 30]` — which is what
guarantees `l_shipdate <= l_receiptdate` for the Task 4 integrity test.

Dispatch and the two remaining public functions:

```ocaml
let iter_rows t ~table ~f =
  match table with
  | "region" -> gen_region t ~f
  | "nation" -> gen_nation t ~f
  | "supplier" -> gen_supplier t ~f
  | "part" -> gen_part t ~f
  | "partsupp" -> gen_partsupp t ~f
  | "customer" -> gen_customer t ~f
  | "orders" -> gen_orders t ~f
  | "lineitem" -> gen_lineitem t ~f
  | other -> invalid_arg ("Tpch_gen.iter_rows: unknown table " ^ other)
;;

let row_count t ~table =
  match table with
  | "region" -> 5
  | "nation" -> 25
  | "supplier" -> scaled t 10_000
  | "part" -> scaled t 200_000
  | "partsupp" -> scaled t 200_000 * 4
  | "customer" -> scaled t 150_000
  | "orders" -> scaled t 1_500_000
  | "lineitem" ->
    (* Not a fixed multiple — count by generating the stream. *)
    let n = ref 0 in
    iter_rows t ~table ~f:(fun _ -> incr n);
    !n
  | other -> invalid_arg ("Tpch_gen.row_count: unknown table " ^ other)
;;
```

- [ ] **Step 5: Run to verify it passes**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpch_gen.exe
```
Expected: PASS, all 9 test cases.

- [ ] **Step 6: Format, lint, commit**

```bash
sh scripts/check-fmt.sh --fix && sh scripts/check-fmt.sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
```

```bash
git add test/tpc/tpch_gen.ml test/tpc/tpch_gen.mli test/test_tpch_gen.ml test/dune
git commit -m "feat(#482): deterministic TPC-H table generators for all 8 tables

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 5: Schema and loader

**Files:**
- Create: `test/tpc/tpch_schema.ml`, `test/tpc/tpch_schema.mli`
- Create: `test/test_tpch_load.ml`
- Modify: `test/dune`

**Interfaces:**
- Consumes: `Tpch_gen`, `Bench_report.ENGINE`.
- Produces:
  - `Tpch_schema.ddl : string list` — CREATE TABLE statements in load order
  - `Tpch_schema.indexes : string list` — CREATE INDEX statements, applied after load
  - `Tpch_schema.Load : functor (E : Bench_report.ENGINE) -> sig val run : E.t -> Tpch_gen.t -> unit end`
  - `Tpch_schema.literal : Tpch_gen.value -> string` — SQL literal rendering, with quote escaping

Load strategy: one transaction per table, rows batched into multi-row `INSERT ... VALUES (...),(...)` statements of `GRANARY_TPCH_BATCH` rows (default 500). Indexes are created **after** the data loads — building them incrementally during a bulk load is materially slower on both engines.

- [ ] **Step 1: Write the failing test**

Create `test/test_tpch_load.ml`:

```ocaml
module Engine = struct
  type t = { mutable stmts : string list }

  let name = "recording"
  let open_db ~dir = ignore dir; { stmts = [] }
  let exec t sql = t.stmts <- sql :: t.stmts
  let query_rows _ _ = []
  let close _ = ()
end

module L = Granary_tpc.Tpch_schema.Load (Engine)

let test_literal_escapes_quotes () =
  Alcotest.(check string)
    "single quotes are doubled"
    "'O''Brien'"
    (Granary_tpc.Tpch_schema.literal (Granary_tpc.Tpch_gen.VText "O'Brien"))
;;

let test_literal_int_and_real () =
  Alcotest.(check string) "int" "42" (Granary_tpc.Tpch_schema.literal (Granary_tpc.Tpch_gen.VInt 42));
  Alcotest.(check bool)
    "real round-trips"
    true
    (Float.abs
       (float_of_string
          (Granary_tpc.Tpch_schema.literal (Granary_tpc.Tpch_gen.VReal 1234.56))
        -. 1234.56)
     < 1e-9)
;;

let test_ddl_covers_every_table () =
  let ddl = String.concat " " Granary_tpc.Tpch_schema.ddl in
  List.iter
    (fun table ->
       let needle = "CREATE TABLE " ^ table in
       let found =
         let nl = String.length needle in
         let rec go i = i + nl <= String.length ddl && (String.sub ddl i nl = needle || go (i + 1)) in
         go 0
       in
       Alcotest.(check bool) (table ^ " has DDL") true found)
    Granary_tpc.Tpch_gen.tables
;;

let test_load_wraps_each_table_in_a_transaction () =
  let g = Granary_tpc.Tpch_gen.create ~seed:1 ~sf:0.001 in
  let e = Engine.open_db ~dir:"/tmp" in
  L.run e g;
  let stmts = List.rev e.Engine.stmts in
  let begins = List.length (List.filter (fun s -> s = "BEGIN") stmts) in
  let commits = List.length (List.filter (fun s -> s = "COMMIT") stmts) in
  Alcotest.(check int) "one BEGIN per table" (List.length Granary_tpc.Tpch_gen.tables) begins;
  Alcotest.(check int) "BEGIN and COMMIT are balanced" begins commits
;;

let test_indexes_are_created_after_inserts () =
  let g = Granary_tpc.Tpch_gen.create ~seed:1 ~sf:0.001 in
  let e = Engine.open_db ~dir:"/tmp" in
  L.run e g;
  let stmts = Array.of_list (List.rev e.Engine.stmts) in
  let starts_with p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p in
  let last_insert = ref (-1)
  and first_index = ref max_int in
  Array.iteri
    (fun i s ->
       if starts_with "INSERT" s then last_insert := i;
       if starts_with "CREATE INDEX" s && i < !first_index then first_index := i)
    stmts;
  Alcotest.(check bool)
    "every index is created after the last insert"
    true
    (!first_index > !last_insert)
;;

let () =
  Alcotest.run
    "tpch_load"
    [ ( "literal"
      , [ Alcotest.test_case "escapes quotes" `Quick test_literal_escapes_quotes
        ; Alcotest.test_case "int and real" `Quick test_literal_int_and_real
        ] )
    ; ("ddl", [ Alcotest.test_case "covers every table" `Quick test_ddl_covers_every_table ])
    ; ( "load"
      , [ Alcotest.test_case "transaction per table" `Quick test_load_wraps_each_table_in_a_transaction
        ; Alcotest.test_case "indexes last" `Quick test_indexes_are_created_after_inserts
        ] )
    ]
;;
```

This test uses a recording engine rather than a real database, so it runs fast and asserts the *loader's* behavior rather than granary's. Task 7's smoke test exercises the real path.

Append to `test/dune`:

```
(test
 (name test_tpch_load)
 (modules test_tpch_load)
 (libraries granary_tpc alcotest))
```

- [ ] **Step 2: Run to verify it fails**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpch_load.exe
```
Expected: FAIL — `Unbound module Granary_tpc.Tpch_schema`.

- [ ] **Step 3: Write the interface**

Create `test/tpc/tpch_schema.mli`:

```ocaml
(** TPC-H schema and bulk loader (#482).

    DDL uses only types granary and SQLite share: INTEGER, REAL, TEXT.  Money
    columns are REAL rather than DECIMAL, matching what SQLite does natively,
    so the cross-engine answer comparison is between like representations. *)

(** CREATE TABLE statements in load order — parents before children. *)
val ddl : string list

(** CREATE INDEX statements.  Applied after the data is loaded, because
    maintaining indexes during a bulk load is substantially slower on both
    engines. *)
val indexes : string list

(** [literal v] renders a generated value as a SQL literal, doubling embedded
    single quotes in text. *)
val literal : Tpch_gen.value -> string

(** Bulk loader, parameterized over the engine under benchmark. *)
module Load (E : Bench_report.ENGINE) : sig
  (** [run engine gen] creates the schema, loads every table inside one
      transaction per table, then creates the indexes.  Rows are batched into
      multi-row INSERT statements of [GRANARY_TPCH_BATCH] rows (default 500). *)
  val run : E.t -> Tpch_gen.t -> unit
end
```

- [ ] **Step 4: Write the implementation**

Create `test/tpc/tpch_schema.ml`. The DDL follows the TPC-H spec's column list with the type mapping above. For example, region and lineitem:

```ocaml
let ddl =
  [ {|CREATE TABLE region (
       r_regionkey INTEGER PRIMARY KEY,
       r_name      TEXT NOT NULL,
       r_comment   TEXT)|}
  ; {|CREATE TABLE nation (
       n_nationkey INTEGER PRIMARY KEY,
       n_name      TEXT NOT NULL,
       n_regionkey INTEGER NOT NULL,
       n_comment   TEXT)|}
    (* ... supplier, part, partsupp, customer, orders ... *)
  ; {|CREATE TABLE lineitem (
       l_orderkey      INTEGER NOT NULL,
       l_partkey       INTEGER NOT NULL,
       l_suppkey       INTEGER NOT NULL,
       l_linenumber    INTEGER NOT NULL,
       l_quantity      REAL    NOT NULL,
       l_extendedprice REAL    NOT NULL,
       l_discount      REAL    NOT NULL,
       l_tax           REAL    NOT NULL,
       l_returnflag    TEXT    NOT NULL,
       l_linestatus    TEXT    NOT NULL,
       l_shipdate      TEXT    NOT NULL,
       l_commitdate    TEXT    NOT NULL,
       l_receiptdate   TEXT    NOT NULL,
       l_shipinstruct  TEXT    NOT NULL,
       l_shipmode      TEXT    NOT NULL,
       l_comment       TEXT)|}
  ]
;;
```

The full DDL covers all eight tables; column names must match `Tpch_gen.column_names` exactly, and the column order must match the arrays `Tpch_gen.iter_rows` emits.

Indexes, following what the workload actually needs:

```ocaml
let indexes =
  [ "CREATE INDEX idx_lineitem_orderkey ON lineitem (l_orderkey)"
  ; "CREATE INDEX idx_lineitem_partkey ON lineitem (l_partkey)"
  ; "CREATE INDEX idx_lineitem_suppkey ON lineitem (l_suppkey)"
  ; "CREATE INDEX idx_lineitem_shipdate ON lineitem (l_shipdate)"
  ; "CREATE INDEX idx_orders_custkey ON orders (o_custkey)"
  ; "CREATE INDEX idx_orders_orderdate ON orders (o_orderdate)"
  ; "CREATE INDEX idx_partsupp_partkey ON partsupp (ps_partkey)"
  ; "CREATE INDEX idx_partsupp_suppkey ON partsupp (ps_suppkey)"
  ; "CREATE INDEX idx_customer_nationkey ON customer (c_nationkey)"
  ; "CREATE INDEX idx_supplier_nationkey ON supplier (s_nationkey)"
  ]
;;

let literal = function
  | Tpch_gen.VInt i -> string_of_int i
  | Tpch_gen.VReal f -> Printf.sprintf "%.17g" f
  | Tpch_gen.VText s ->
    let buf = Buffer.create (String.length s + 2) in
    Buffer.add_char buf '\'';
    String.iter
      (fun c -> if c = '\'' then Buffer.add_string buf "''" else Buffer.add_char buf c)
      s;
    Buffer.add_char buf '\'';
    Buffer.contents buf
;;

module Load (E : Bench_report.ENGINE) = struct
  let batch_size = Bench_report.env_int "GRANARY_TPCH_BATCH" 500

  let flush engine ~table ~cols pending =
    if pending <> []
    then (
      let values =
        List.rev_map (fun row -> "(" ^ String.concat "," (List.map literal (Array.to_list row)) ^ ")") pending
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
    let cols = Tpch_gen.column_names ~table in
    E.exec engine "BEGIN";
    let pending = ref []
    and n = ref 0 in
    Tpch_gen.iter_rows gen ~table ~f:(fun row ->
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
    List.iter (fun table -> load_table engine gen ~table) Tpch_gen.tables;
    List.iter (E.exec engine) indexes
  ;;
end
```

`%.17g` for REAL is deliberate: it round-trips a float exactly through text, so the loaded value is bit-identical to the generated one on both engines.

- [ ] **Step 5: Run to verify it passes**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpch_load.exe
```
Expected: PASS, all 5 test cases.

- [ ] **Step 6: Format, lint, commit**

```bash
sh scripts/check-fmt.sh --fix && sh scripts/check-fmt.sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
```

```bash
git add test/tpc/tpch_schema.ml test/tpc/tpch_schema.mli test/test_tpch_load.ml test/dune
git commit -m "feat(#482): TPC-H schema, indexes, and batched bulk loader

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 6: Query catalogue

Holds all 22 queries with their portability verdicts. The verdicts start as `Native` for every query and are corrected to `Rewritten` or `Skipped` in Task 8, once they have actually been run. Do not guess them here.

**Files:**
- Create: `test/tpc/tpch_queries.ml`, `test/tpc/tpch_queries.mli`
- Create: `test/test_tpch_queries.ml`
- Modify: `test/dune`

**Interfaces:**
- Consumes: nothing.
- Produces:
  - `type Tpch_queries.verdict = Native | Rewritten of string | Skipped of string`
  - `type Tpch_queries.query = { number : int; sql : string; setup : string list; verdict : verdict }`
  - `Tpch_queries.all : query list` — exactly 22 entries, ordered by number
  - `Tpch_queries.find : int -> query option`
  - `Tpch_queries.verdict_label : verdict -> string` — `"native"`, `"rewritten"`, `"skipped"`

`setup` holds any `CREATE VIEW` statements a rewrite needs. Setup runs before the timed section and is never included in a query's measured time.

- [ ] **Step 1: Write the failing test**

Create `test/test_tpch_queries.ml`:

```ocaml
module Q = Granary_tpc.Tpch_queries

let test_exactly_22_queries () =
  Alcotest.(check int) "22 queries present" 22 (List.length Q.all)
;;

let test_numbered_one_to_22_in_order () =
  Alcotest.(check (list int))
    "numbers are 1..22 in order"
    (List.init 22 (fun i -> i + 1))
    (List.map (fun q -> q.Q.number) Q.all)
;;

let test_every_query_findable () =
  for i = 1 to 22 do
    match Q.find i with
    | Some q -> Alcotest.(check int) "find returns the right query" i q.Q.number
    | None -> Alcotest.failf "query %d not found" i
  done;
  Alcotest.(check bool) "out-of-range returns None" true (Q.find 23 = None)
;;

let test_runnable_queries_have_sql () =
  List.iter
    (fun q ->
       match q.Q.verdict with
       | Q.Skipped _ -> ()
       | Q.Native | Q.Rewritten _ ->
         Alcotest.(check bool)
           (Printf.sprintf "Q%d has non-empty SQL" q.Q.number)
           true
           (String.trim q.Q.sql <> ""))
    Q.all
;;

let test_skipped_queries_cite_an_issue () =
  List.iter
    (fun q ->
       match q.Q.verdict with
       | Q.Skipped reason ->
         Alcotest.(check bool)
           (Printf.sprintf "Q%d's skip reason cites a Forgejo issue" q.Q.number)
           true
           (String.contains reason '#')
       | _ -> ())
    Q.all
;;

let test_rewritten_queries_explain_themselves () =
  List.iter
    (fun q ->
       match q.Q.verdict with
       | Q.Rewritten why ->
         Alcotest.(check bool)
           (Printf.sprintf "Q%d's rewrite has a rationale" q.Q.number)
           true
           (String.length (String.trim why) > 10)
       | _ -> ())
    Q.all
;;

let () =
  Alcotest.run
    "tpch_queries"
    [ ( "catalogue"
      , [ Alcotest.test_case "22 queries" `Quick test_exactly_22_queries
        ; Alcotest.test_case "numbered in order" `Quick test_numbered_one_to_22_in_order
        ; Alcotest.test_case "findable" `Quick test_every_query_findable
        ] )
    ; ( "verdicts"
      , [ Alcotest.test_case "runnable have SQL" `Quick test_runnable_queries_have_sql
        ; Alcotest.test_case "skipped cite an issue" `Quick test_skipped_queries_cite_an_issue
        ; Alcotest.test_case "rewritten explain themselves" `Quick test_rewritten_queries_explain_themselves
        ] )
    ]
;;
```

Append to `test/dune`:

```
(test
 (name test_tpch_queries)
 (modules test_tpch_queries)
 (libraries granary_tpc alcotest))
```

- [ ] **Step 2: Run to verify it fails**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpch_queries.exe
```
Expected: FAIL — `Unbound module Granary_tpc.Tpch_queries`.

- [ ] **Step 3: Write the interface**

Create `test/tpc/tpch_queries.mli`:

```ocaml
(** The 22 TPC-H-derived queries (#482).

    Substitution parameters are fixed at the spec's validation values rather
    than randomized, so successive runs are comparable and the SQLite
    cross-check has a stable target.

    Each query records how faithfully it could be expressed in granary's SQL
    dialect.  Verdicts are established by running the queries, never by
    predicting from the grammar. *)

(** How faithfully a query could be expressed. *)
type verdict =
  | Native (** runs as the spec writes it *)
  | Rewritten of string (** runs after a transformation; the string explains it *)
  | Skipped of string
      (** cannot be expressed; the string names the missing capability and
          cites the Forgejo issue tracking it *)

type query =
  { number : int (** 1–22 *)
  ; sql : string (** the query text, empty when skipped *)
  ; setup : string list
      (** statements a rewrite depends on, typically CREATE VIEW.  Run before
          the timed section and never counted in a query's measured time. *)
  ; verdict : verdict
  }

(** All 22 queries, ordered by number. *)
val all : query list

(** [find n] is the query numbered [n], or [None] outside 1–22. *)
val find : int -> query option

(** [verdict_label v] is the CSV token: ["native"], ["rewritten"], or
    ["skipped"]. *)
val verdict_label : verdict -> string
```

- [ ] **Step 4: Write the implementation**

Create `test/tpc/tpch_queries.ml` with all 22 queries transcribed from the TPC-H specification, using the spec's validation-parameter values. Every entry starts as `verdict = Native` and `setup = []`; Task 8 revises them based on what actually runs.

Q1 as the pattern to follow — note `EXTRACT`/`INTERVAL` replaced by a literal date, which is the one rewrite that is safe to make up front because it is a pure constant-folding of the spec's own arithmetic:

```ocaml
let q1 =
  { number = 1
  ; setup = []
  ; verdict = Native
  ; sql =
      {|SELECT l_returnflag, l_linestatus,
               SUM(l_quantity) AS sum_qty,
               SUM(l_extendedprice) AS sum_base_price,
               SUM(l_extendedprice * (1 - l_discount)) AS sum_disc_price,
               SUM(l_extendedprice * (1 - l_discount) * (1 + l_tax)) AS sum_charge,
               AVG(l_quantity) AS avg_qty,
               AVG(l_extendedprice) AS avg_price,
               AVG(l_discount) AS avg_disc,
               COUNT(*) AS count_order
        FROM lineitem
        WHERE l_shipdate <= '1998-09-02'
        GROUP BY l_returnflag, l_linestatus
        ORDER BY l_returnflag, l_linestatus|}
  }
;;
```

Then `let all = [ q1; q2; ...; q22 ]` and:

```ocaml
let find n = List.find_opt (fun q -> q.number = n) all

let verdict_label = function
  | Native -> "native"
  | Rewritten _ -> "rewritten"
  | Skipped _ -> "skipped"
;;
```

- [ ] **Step 5: Run to verify it passes**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpch_queries.exe
```
Expected: PASS, all 6 test cases.

- [ ] **Step 6: Format, lint, commit**

```bash
sh scripts/check-fmt.sh --fix && sh scripts/check-fmt.sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
```

```bash
git add test/tpc/tpch_queries.ml test/tpc/tpch_queries.mli test/test_tpch_queries.ml test/dune
git commit -m "feat(#482): TPC-H query catalogue with portability verdicts

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 7: Benchmark runner with SQLite cross-check

**Files:**
- Create: `test/bench_tpch.ml`
- Modify: `test/dune`
- Create: `docs/BENCHMARKS-TPCH.md`

**Interfaces:**
- Consumes: everything from Tasks 1–6.
- Produces: the `bench_tpch` executable. Nothing depends on it.

This is the first task where a real granary database is opened, and it owns the `sqlite3` dependency. The executable is `(optional)`, matching `bench_compare`.

Behavior:
1. Read env config (`GRANARY_TPCH_SF`, `GRANARY_TPCH_REPEATS`, `GRANARY_TPCH_QUERIES`, `GRANARY_TPC_SEED`, `GRANARY_TPC_HOST`).
2. Build a generator; create a temp directory.
3. For each engine (granary always; sqlite when the library is linked): load the schema and data, run each selected query's `setup`, then time it `REPEATS` times, keeping the best wall time.
4. Compare granary's rows against SQLite's, using `Bench_report.real_eq` for fields that parse as floats and string equality otherwise.
5. Emit one CSV row per (engine, query).

- [ ] **Step 1: Write the failing test**

Create `test/test_tpch_smoke.ml`, the integration test that also serves as the CI smoke tier:

```ocaml
(* #482 — smoke tier: SF 0.001 through the real granary engine.  Asserts
   correctness and completion; never a latency bound. *)

module BR = Granary_tpc.Bench_report
module Q = Granary_tpc.Tpch_queries

module Granary_engine : BR.ENGINE = struct
  open Granary

  type t = { db : Db.t }

  let name = "granary"
  let run = Lwt_main.run

  let unwrap = function
    | Ok v -> v
    | Error e -> Alcotest.failf "granary: %a" Db.pp_error e
  ;;

  let open_db ~dir =
    let path = Filename.concat dir "tpch.db" in
    List.iter
      (fun p ->
         try Unix.unlink p with
         | _ -> ())
      [ path; path ^ "-wal" ];
    { db = unwrap (run (Granary_unix.open_file_wal ~clock:Unix.gettimeofday ~path ())) }
  ;;

  let exec t sql = ignore (unwrap (run (Db.execute t.db sql)))

  let render = function
    | Db.V_int i -> Int64.to_string i
    | Db.V_text s -> s
    | Db.V_null -> "NULL"
    | Db.V_real f -> Printf.sprintf "%.17g" f
    | Db.V_blob _ -> "<blob>"
  ;;

  let query_rows t sql =
    run
      (let open Lwt.Syntax in
       let* stream = Lwt.map unwrap (Db.query t.db sql) in
       let* rows = Lwt_stream.to_list stream in
       Lwt.return (List.map (fun r -> Array.to_list (Array.map render r)) rows))
  ;;

  let close t = run (Db.close t.db)
end

module Load = Granary_tpc.Tpch_schema.Load (Granary_engine)

let with_tmp_dir f =
  let dir = Filename.temp_file "tpch-smoke" "" in
  Unix.unlink dir;
  Unix.mkdir dir 0o755;
  Fun.protect ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)))) (fun () -> f dir)
;;

let test_load_row_counts () =
  with_tmp_dir (fun dir ->
    let gen = Granary_tpc.Tpch_gen.create ~seed:42 ~sf:0.001 in
    let e = Granary_engine.open_db ~dir in
    Load.run e gen;
    List.iter
      (fun table ->
         let expected = Granary_tpc.Tpch_gen.row_count gen ~table in
         match Granary_engine.query_rows e (Printf.sprintf "SELECT COUNT(*) FROM %s" table) with
         | [ [ n ] ] ->
           Alcotest.(check int)
             (table ^ ": loaded row count matches generated")
             expected
             (int_of_string n)
         | _ -> Alcotest.failf "%s: COUNT(*) returned an unexpected shape" table)
      Granary_tpc.Tpch_gen.tables;
    Granary_engine.close e)
;;

let check_well_formed q rows =
  match rows with
  | [] -> () (* an empty result is legitimate for several queries at SF 0.001 *)
  | first :: _ ->
    let arity = List.length first in
    Alcotest.(check bool)
      (Printf.sprintf "Q%d projects at least one column" q.Q.number)
      true
      (arity > 0);
    List.iteri
      (fun i row ->
         Alcotest.(check int)
           (Printf.sprintf "Q%d row %d has the same arity as row 0" q.Q.number i)
           arity
           (List.length row))
      rows
;;

(* Enabled unconditionally in Task 8, once Tpch_queries carries real verdicts.
   Until then every query claims Native, so this would fail on the ones that
   need a rewrite — gated rather than left red, so Task 7's suite is honest. *)
let verdicts_are_real () = Sys.getenv_opt "GRANARY_TPCH_VERDICTS" = Some "1"

let test_every_runnable_query_executes () =
  if not (verdicts_are_real ())
  then
    Printf.eprintf
      "skipped: set GRANARY_TPCH_VERDICTS=1 to run (verdicts are provisional until Task 8)\n"
  else
    with_tmp_dir (fun dir ->
      let gen = Granary_tpc.Tpch_gen.create ~seed:42 ~sf:0.001 in
      let e = Granary_engine.open_db ~dir in
      Load.run e gen;
      List.iter
        (fun q ->
           match q.Q.verdict with
           | Q.Skipped _ -> ()
           | Q.Native | Q.Rewritten _ ->
             List.iter (Granary_engine.exec e) q.Q.setup;
             check_well_formed q (Granary_engine.query_rows e q.Q.sql))
        Q.all;
      Granary_engine.close e)
;;

let () =
  Alcotest.run
    "tpch_smoke"
    [ ( "load"
      , [ Alcotest.test_case "row counts match generator" `Slow test_load_row_counts ] )
    ; ( "queries"
      , [ Alcotest.test_case "every runnable query executes" `Slow test_every_runnable_query_executes ] )
    ]
;;
```

Append to `test/dune`:

```
(test
 (name test_tpch_smoke)
 (modules test_tpch_smoke)
 (libraries granary granary_tpc granary.store granary.unix alcotest lwt.unix unix))
```

- [ ] **Step 2: Run to verify it fails**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_tpch_smoke.exe
```
Expected: FAIL. Queries that granary cannot parse or plan raise here — **that failure list is the input to Task 8.** Capture the full output:

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev \
  dune test test/test_tpch_smoke.exe 2>&1 | tee /tmp/claude-1001/tpch-first-run.log
```

- [ ] **Step 3: Make the load test pass**

`test_load_row_counts` must pass before moving on — it validates the generator, schema, and loader together. If it fails, the bug is in Task 4 or Task 5, not here. Fix it there and re-run.

`test_every_runnable_query_executes` stays gated behind `GRANARY_TPCH_VERDICTS=1` for this task, because every query still claims `Native` and the ones needing a rewrite would fail. Task 7's suite must be **green** without the gate. Run it *with* the gate to collect the failure list for Task 8:

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace -e GRANARY_TPCH_VERDICTS=1 \
  granary-dev dune test test/test_tpch_smoke.exe 2>&1 | tee /tmp/claude-1001/tpch-first-run.log
```

Task 8 removes the gate and the `verdicts_are_real` helper entirely.

- [ ] **Step 4: Write the runner**

Create `test/bench_tpch.ml`. It reuses the same `Granary_engine` shape as the smoke test, adds a `Ref_sqlite` implementation of `BR.ENGINE` modeled on `Ref_sqlite` at `test/bench_compare.ml:318` (PRAGMA parity: `page_size=4096`, `journal_mode=WAL`, `synchronous=FULL`), and drives:

```ocaml
let compare_rows granary_rows sqlite_rows =
  let field_eq a b =
    if a = b
    then true
    else (
      match float_of_string_opt a, float_of_string_opt b with
      | Some fa, Some fb -> BR.real_eq fa fb
      | _ -> false)
  in
  List.length granary_rows = List.length sqlite_rows
  && List.for_all2
       (fun ra rb -> List.length ra = List.length rb && List.for_all2 field_eq ra rb)
       granary_rows
       sqlite_rows
;;
```

Before generating anything, refuse a scale factor that cannot fit. TPC-H's raw
dataset is roughly 1 GB per unit of scale factor, and loading it into two
engines with indexes needs about three times that:

```ocaml
let check_disk_space ~dir ~sf =
  let needed_bytes = Int64.of_float (3.0 *. 1.1e9 *. sf) in
  let available =
    (* `df -k` is portable across the dev container and the CI runner; the
       Alpine CI image has no python3 or bc, so keep this to one shell call. *)
    let ic = Unix.open_process_in (Printf.sprintf "df -k %s | tail -1" (Filename.quote dir)) in
    let line = try input_line ic with End_of_file -> "" in
    ignore (Unix.close_process_in ic);
    match String.split_on_char ' ' line |> List.filter (fun s -> s <> "") with
    | _dev :: _size :: _used :: avail :: _ ->
      (try Int64.mul (Int64.of_string avail) 1024L with
       | _ -> Int64.max_int)
    | _ -> Int64.max_int
  in
  if Int64.compare available needed_bytes < 0
  then
    failwith
      (Printf.sprintf
         "GRANARY_TPCH_SF=%g needs about %Ld MB under %s but only %Ld MB is free"
         sf
         (Int64.div needed_bytes 1_048_576L)
         dir
         (Int64.div available 1_048_576L))
;;
```

Failing to parse `df` output yields `Int64.max_int`, so an unreadable filesystem
lets the run proceed rather than blocking it on a guard that is only advisory.

CSV columns, emitted in this order:

```
host,engine,sf,query,verdict,wall_s,cpu_s,cpu_wall_ratio,rows_out,cross_check
```

`cross_check` is `ok`, `MISMATCH`, or `skipped` (the last when the sqlite engine is unavailable or the query is skipped). A `MISMATCH` is also printed to stderr with the first differing row, and sets a non-zero exit code — a wrong answer is a real failure even though timing is not gated.

Add to `test/dune`:

```
; TPC-H-derived benchmark (#482).  `(optional)` for the same reason as
; bench_compare: builds where `sqlite3` is absent skip it rather than fail.

(executable
 (name bench_tpch)
 (optional)
 (modules bench_tpch)
 (libraries granary granary_tpc granary.store granary.unix sqlite3 alcotest lwt.unix unix))
```

- [ ] **Step 5: Run the benchmark end to end**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace \
  -e GRANARY_TPCH_SF=0.001 -e GRANARY_TPCH_REPEATS=1 \
  granary-dev dune exec test/bench_tpch.exe
```
Expected: a CSV header plus one row per query per engine. Queries that fail appear with their error in the `verdict` column and do not abort the run.

- [ ] **Step 6: Write the documentation**

Create `docs/BENCHMARKS-TPCH.md` covering: what "derived" means and why these are not audited TPC results; every env knob with its default; the scale tiers; how to read the CSV columns; and the standing caveat that rewritten queries are marked in the `verdict` column and their rationale lives in `Tpch_queries`.

- [ ] **Step 7: Format, lint, commit**

```bash
sh scripts/check-fmt.sh --fix && sh scripts/check-fmt.sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
cd /home/tej/projects/sqlite_ocaml_port && \
  podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build @fmt
```

```bash
git add test/bench_tpch.ml test/test_tpch_smoke.ml test/dune docs/BENCHMARKS-TPCH.md
git commit -m "feat(#482): TPC-H benchmark runner with SQLite answer cross-check

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 8: Gap triage — establish the real verdicts

The empirical task. Everything before this assumed `Native`; here each query is run and its verdict established from what the engine actually does.

**Files:**
- Modify: `test/tpc/tpch_queries.ml` (verdicts, rewritten SQL, setup views)
- Modify: `docs/BENCHMARKS-TPCH.md` (verdict summary table)

**Interfaces:**
- Consumes: the runner from Task 7.
- Produces: a `Tpch_queries.all` whose verdicts are true, and Forgejo issues for every skip.

- [ ] **Step 1: Collect the failures**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace \
  -e GRANARY_TPCH_SF=0.001 -e GRANARY_TPCH_REPEATS=1 \
  granary-dev dune exec test/bench_tpch.exe 2> /tmp/claude-1001/tpch-verdicts.log
```

Read the log. Group failures by root cause, not by query number — several queries usually fail for one shared reason.

- [ ] **Step 2: Apply the rewrite levers, one query at a time**

For each failing query, in order, try these in sequence and stop at the first that works:

1. **Subquery in FROM** → hoist it into a `CREATE VIEW` in the query's `setup` list and reference the view by name in FROM. `Sema.bind_internal` rewrites a view named in FROM position into a CTE, so this is the supported path.
2. **`GROUP BY <expr>`** → move the expression into a view's projection under an alias, then `GROUP BY` that alias as a plain column.
3. **`EXTRACT(year FROM d)`** → `CAST(strftime('%Y', d) AS INTEGER)`.
4. **A second CTE** → the engine binds one CTE per statement; convert the extra CTEs to views in `setup`.

Set `verdict = Rewritten "<what changed and why>"`. The rationale must name the transformation, e.g. `"derived table hoisted to a view: FROM-subqueries are unsupported"`.

- [ ] **Step 3: Verify each rewrite preserves the answer**

After each rewrite, re-run and confirm the `cross_check` column reads `ok` for that query:

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace \
  -e GRANARY_TPCH_SF=0.001 -e GRANARY_TPCH_QUERIES=<n> \
  granary-dev dune exec test/bench_tpch.exe
```

A `MISMATCH` means the rewrite changed the query's meaning. Fix the rewrite; do not adjust the comparison to accommodate it.

- [ ] **Step 4: File an issue for each genuine gap**

For every query no lever rescues, file the missing capability — one issue per capability, not per query:

```bash
~/.local/bin/forgejo issue create IoTReadyNext/granary \
  --title="sql: <missing capability>" \
  --body="Blocks TPC-H <queries> in the #482 benchmark.

<what the SQL needs, the smallest failing example, and the error granary returns>"
```

Then set `verdict = Skipped "<capability> — #NNN"` on each affected query. The test from Task 6 enforces that a skip reason contains a `#`.

- [ ] **Step 5: Remove the Task 7 gate**

Delete `verdicts_are_real` from `test/test_tpch_smoke.ml` and the `if not (verdicts_are_real ()) then ... else` wrapper around `test_every_runnable_query_executes`, so the test runs unconditionally. Verdicts are now real, so it must pass on its own.

- [ ] **Step 6: Run the full suite**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test
```
Expected: PASS throughout, including `test_tpch_smoke.exe`'s `test_every_runnable_query_executes` — every query is now either running or explicitly skipped.

- [ ] **Step 7: Record the verdict summary**

Add a table to `docs/BENCHMARKS-TPCH.md`: one row per query, with its verdict and, for rewrites and skips, the one-line reason. This is the honest account of how much of TPC-H granary runs today.

- [ ] **Step 8: Format, lint, commit**

```bash
sh scripts/check-fmt.sh --fix && sh scripts/check-fmt.sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
```

```bash
git add test/tpc/tpch_queries.ml test/test_tpch_smoke.ml docs/BENCHMARKS-TPCH.md
git commit -m "feat(#482): establish TPC-H query verdicts; rewrite what fits, skip what does not

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

### Task 9: CI smoke tier and pull request

**Files:**
- Modify: `.forgejo/workflows/bench.yml` (or the equivalent workflow file — check what exists)
- Modify: `.github/workflows/bench.yml` (the mirrored GitHub Actions workflow)

**Interfaces:**
- Consumes: `test_tpch_smoke.exe`.
- Produces: CI coverage. Nothing depends on it.

- [ ] **Step 1: Find the existing workflow shape**

```bash
ls .forgejo/workflows/ .github/workflows/
grep -n "bench" .forgejo/workflows/*.yml
```

Match the established conventions exactly: `runs-on: self-hosted`, `docker_host: "-"`, build inside the dev image. Do not invent a new workflow shape.

- [ ] **Step 2: Add the smoke step**

The smoke test is an ordinary `dune test` target, so it already runs in the standard test job. Confirm that it does:

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test 2>&1 | grep tpch
```

If the standard job already covers it, no workflow change is needed — say so in the PR rather than adding a redundant step. Only add an explicit step if the smoke test is excluded from the default `dune test` target.

- [ ] **Step 3: Full verification before pushing**

```bash
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test
sh scripts/check-fmt.sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
cd /home/tej/projects/sqlite_ocaml_port && \
  podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build @fmt
```

Every one must pass. Read `check-fmt.sh`'s summary line: `✓` not `◐`.

- [ ] **Step 4: File the TPC-E follow-up**

```bash
~/.local/bin/forgejo issue create IoTReadyNext/granary \
  --title="bench: TPC-E-derived OLTP workload" \
  --body="Deferred from #482, which shipped TPC-H and scoped TPC-C.

TPC-E is 33 tables and 12 transaction profiles against a market-feed driver — roughly 5-10x TPC-C's implementation cost for an embedded engine, and harder to compare against anything. Worth revisiting once the TPC-C harness has proven its shape."
```

- [ ] **Step 5: Commit and open the pull request**

```bash
git push origin feat/482-tpc-benchmarks
~/.local/bin/forgejo pr create IoTReadyNext/granary \
  --title="feat(#482): TPC-H-derived OLAP benchmark" \
  --head=feat/482-tpc-benchmarks \
  --base=main \
  --body="$(cat <<'EOF'
## Summary
- Pure-OCaml deterministic TPC-H data generator: the spec's random primitives, the §4.2.2.1 text-pool grammar, and all 8 tables at any scale factor
- TPC-H schema, indexes, and a batched bulk loader parameterized over the engine
- All 22 queries with honest per-query verdicts — native, rewritten (with rationale), or skipped (with a Forgejo issue)
- `bench_tpch` runner emitting CSV, cross-checking every answer against reference C SQLite
- Smoke tier at SF 0.001 running in CI

Not audited TPC results: queries are rewritten to fit granary's SQL dialect and there is no audit or pricing disclosure. Measurement tool, not a timing gate.

## Test plan
- [ ] `dune test` passes
- [ ] Generator determinism and cardinality tests pass
- [ ] QCheck properties on the random primitives pass
- [ ] Every runnable query cross-checks `ok` against SQLite at SF 0.001
- [ ] `check-fmt.sh` reports full parity, `merlint` reports 0 issues

Part of #482. TPC-C follows in a separate plan.
EOF
)"
```

---

## Follow-on work, not in this plan

- **TPC-C-derived OLTP benchmark** — its own plan once this lands, consuming `Bench_report` unchanged.
- **Retrofit `bench_compare.ml` onto `Bench_report.ENGINE`** — worth doing, deliberately excluded here so an unrelated refactor stays out of this diff.
- **TPC-E** — filed as a deferred issue in Task 9.
