# TPC-C per-statement attribution profiler (#714) — Implementation Plan

> **Superseded in part.** The branch's own measurement retracted three claims
> this plan makes: the `NewOrder/sec = 1 / (time a NewOrder holds the writer
> lock)` identity; that every recorded millisecond is in-lock *work* because at
> one terminal there is exactly one writer; and the "~35 statements (5 header +
> 10 items × 3)" estimate, actually 45.6 (6 header + 4 per item). The first two
> are wrong because `commit_wal` releases the writer lock at
> `lib/store/store.ml:2166` *before* it fsyncs, and `maybe_autockpt_after_commit`
> (`:2078`/`:2083`) can take the writer lock as an unawaited second writer even
> at one terminal. See `docs/benchmarks/BENCHMARKS-TPCC.md`'s "What this
> measures, and what it does not" subsection for the current position. The
> PR-body template near the end of this file asserts the retracted claims and
> must not be pasted as-is. The body below is kept as the design record and is
> not edited.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Attribute the ~17.6 ms a TPC-C NewOrder holds granary's single writer lock across the ~35 statements it issues, so the fix that shrinks it can be designed from data rather than guessed at.

**Architecture:** A tiny stateful accumulator module (`Tpcc_stmt_profile`) keyed by `(profile, sql)`, fed from a wrapper installed inside `Tpcc_txn.run` — the one place that knows both the `ops` record and which profile is executing. The accumulator is unconditional and pure arithmetic so it is unit-testable; the on/off gate lives at the single call site in `run`. `bench_tpcc` prints a ranked table and optionally writes a CSV.

**Tech Stack:** OCaml 5.4, Lwt, Alcotest, dune. All build/test commands run inside the `granary-dev` podman image.

## Global Constraints

- **All work goes through a PR. Never commit to `main`.** Work happens in the worktree `.worktrees/714-stmt-profile` on branch `perf/714-tpcc-stmt-profile`.
- **Never call `dune` on the host.** Every build/test command is `podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev <cmd>`, run from the worktree root.
- **No engine change.** Nothing under `lib/` is modified by this plan. Every file touched is under `test/`.
- **No new `sqlite3` dependency.** The new test links `granary_tpc` and `alcotest` only, so `scripts/check-sqlite-policy.sh` needs no edit.
- **merlint gates:** every library module needs a `.mli`; every public `val` in an `.mli` needs a `(** … *)` doc comment (not `(* … *)`); max nesting depth 4.
- **Formatting:** `sh scripts/check-fmt.sh --fix` before every commit, and read its final summary line. This plan touches no `dune-project`, but it does touch `test/dune`, so expect `◐ … Dune files UNVERIFIED` in the worktree — verify dune formatting from the main checkout before pushing.
- **Profile names are `new_order`, `payment`, `order_status`, `delivery`, `stock_level`** — exactly `Tpcc_txn.all`'s `name` fields, because the coverage summary joins against `Tpcc_driver.profile_stats.name`.

---

### Task 1: The `Tpcc_stmt_profile` accumulator

The only stateful piece. Deliberately separate from `Tpcc_conn`/`bench_tpcc` so its arithmetic is unit-testable — `bench_tpcc` is `(optional)` and does not build without `sqlite3`, so anything living there is untestable. Same reason `Tpch_check` was extracted in #505.

**Files:**
- Create: `test/tpc/tpcc_stmt_profile.ml`
- Create: `test/tpc/tpcc_stmt_profile.mli`
- Create: `test/test_tpcc_stmt_profile.ml`
- Modify: `test/dune` (add a `(test …)` stanza near the other `granary_tpc` tests, e.g. after the `test_tpch_check` stanza around line 856)

**Interfaces:**
- Consumes: `Granary_tpc.Bench_report.env_str`, `Granary_tpc.Bench_report.Csv.{header,row}` (both already exist in `test/tpc/bench_report.ml`).
- Produces, for Tasks 2–4:
  - `val enabled : bool`
  - `val record : profile:string -> sql:string -> rows:int -> secs:float -> unit`
  - `val reset : unit -> unit`
  - `type entry = { profile : string; sql : string; calls : int; rows : int; total_ms : float; mean_ms : float; pct_of_profile : float }`
  - `val ranked : unit -> entry list`
  - `type summary = { profile : string; statements_total_ms : float; driver_service_ms : float; attributed_pct : float }`
  - `val summaries : service_ms:(string * float) list -> summary list`
  - `val report : service_ms:(string * float) list -> string`
  - `val to_csv : path:string -> service_ms:(string * float) list -> unit`

- [ ] **Step 1: Write the failing tests**

Create `test/test_tpcc_stmt_profile.ml`:

```ocaml
open Granary_tpc
module P = Tpcc_stmt_profile

let approx name expected actual =
  Alcotest.(check bool)
    (Printf.sprintf "%s: expected ~%g, got %g" name expected actual)
    true
    (Float.abs (expected -. actual) <= 1e-6)
;;

(* Every test starts from a clean table: the accumulator is global, and
   Alcotest runs cases in one process. *)
let fresh () = P.reset ()

let test_aggregates_repeated_calls () =
  fresh ();
  P.record ~profile:"new_order" ~sql:"SELECT 1" ~rows:2 ~secs:0.001;
  P.record ~profile:"new_order" ~sql:"SELECT 1" ~rows:3 ~secs:0.003;
  match P.ranked () with
  | [ e ] ->
    Alcotest.(check int) "calls" 2 e.P.calls;
    Alcotest.(check int) "rows" 5 e.P.rows;
    approx "total_ms" 4.0 e.P.total_ms;
    approx "mean_ms" 2.0 e.P.mean_ms
  | other -> Alcotest.failf "expected 1 entry, got %d" (List.length other)
;;

let test_same_sql_under_two_profiles_stays_distinct () =
  fresh ();
  P.record ~profile:"new_order" ~sql:"COMMIT" ~rows:0 ~secs:0.005;
  P.record ~profile:"payment" ~sql:"COMMIT" ~rows:0 ~secs:0.001;
  let entries = P.ranked () in
  Alcotest.(check int) "two entries" 2 (List.length entries);
  (* new_order is the costlier profile, so it ranks first. *)
  Alcotest.(check string) "first profile" "new_order" (List.hd entries).P.profile
;;

let test_ranking_within_a_profile_is_by_total_desc () =
  fresh ();
  P.record ~profile:"delivery" ~sql:"cheap" ~rows:0 ~secs:0.001;
  P.record ~profile:"delivery" ~sql:"dear" ~rows:0 ~secs:0.009;
  let sqls = List.map (fun e -> e.P.sql) (P.ranked ()) in
  Alcotest.(check (list string)) "dear first" [ "dear"; "cheap" ] sqls
;;

let test_equal_totals_break_ties_on_sql_for_stability () =
  fresh ();
  P.record ~profile:"delivery" ~sql:"bbb" ~rows:0 ~secs:0.002;
  P.record ~profile:"delivery" ~sql:"aaa" ~rows:0 ~secs:0.002;
  let sqls = List.map (fun e -> e.P.sql) (P.ranked ()) in
  Alcotest.(check (list string)) "alphabetical on a tie" [ "aaa"; "bbb" ] sqls
;;

let test_pct_of_profile_sums_to_100_per_profile () =
  fresh ();
  P.record ~profile:"payment" ~sql:"a" ~rows:0 ~secs:0.003;
  P.record ~profile:"payment" ~sql:"b" ~rows:0 ~secs:0.001;
  P.record ~profile:"stock_level" ~sql:"c" ~rows:0 ~secs:0.005;
  let sum p =
    List.fold_left
      (fun acc e -> if String.equal e.P.profile p then acc +. e.P.pct_of_profile else acc)
      0.0
      (P.ranked ())
  in
  approx "payment sums to 100" 100.0 (sum "payment");
  approx "stock_level sums to 100" 100.0 (sum "stock_level")
;;

let test_attributed_pct_against_driver_service_ms () =
  fresh ();
  P.record ~profile:"new_order" ~sql:"a" ~rows:0 ~secs:0.006;
  match P.summaries ~service_ms:[ "new_order", 10.0 ] with
  | [ s ] ->
    approx "statements_total_ms" 6.0 s.P.statements_total_ms;
    approx "driver_service_ms" 10.0 s.P.driver_service_ms;
    approx "attributed_pct" 60.0 s.P.attributed_pct
  | other -> Alcotest.failf "expected 1 summary, got %d" (List.length other)
;;

(* Statement time exceeding the driver's own service time is a real signal —
   clock skew, or a driver window that does not enclose every statement — and
   must render rather than be clamped or treated as impossible. *)
let test_attributed_pct_over_100_renders () =
  fresh ();
  P.record ~profile:"new_order" ~sql:"a" ~rows:0 ~secs:0.020;
  match P.summaries ~service_ms:[ "new_order", 10.0 ] with
  | [ s ] -> approx "attributed_pct" 200.0 s.P.attributed_pct
  | other -> Alcotest.failf "expected 1 summary, got %d" (List.length other)
;;

let test_missing_driver_service_ms_is_zero_not_a_crash () =
  fresh ();
  P.record ~profile:"delivery" ~sql:"a" ~rows:0 ~secs:0.004;
  match P.summaries ~service_ms:[] with
  | [ s ] ->
    approx "driver_service_ms" 0.0 s.P.driver_service_ms;
    approx "attributed_pct" 0.0 s.P.attributed_pct
  | other -> Alcotest.failf "expected 1 summary, got %d" (List.length other)
;;

let test_empty_table_renders () =
  fresh ();
  Alcotest.(check (list string)) "no entries" [] (List.map (fun e -> e.P.sql) (P.ranked ()));
  Alcotest.(check bool)
    "report says so"
    true
    (String.length (P.report ~service_ms:[]) > 0)
;;

let test_reset_clears () =
  fresh ();
  P.record ~profile:"payment" ~sql:"a" ~rows:1 ~secs:0.001;
  P.reset ();
  Alcotest.(check int) "cleared" 0 (List.length (P.ranked ()))
;;

(* The gate is off unless the environment asks for it; a benchmark run must
   never be perturbed by instrumentation that shipped enabled by accident. *)
let test_disabled_by_default () =
  Alcotest.(check bool) "GRANARY_TPCC_STMT_PROFILE unset" false P.enabled
;;

let () =
  Alcotest.run
    "tpcc_stmt_profile"
    [ ( "accumulator"
      , [ Alcotest.test_case "aggregates repeated calls" `Quick test_aggregates_repeated_calls
        ; Alcotest.test_case
            "same sql under two profiles stays distinct"
            `Quick
            test_same_sql_under_two_profiles_stays_distinct
        ; Alcotest.test_case
            "ranking within a profile is by total desc"
            `Quick
            test_ranking_within_a_profile_is_by_total_desc
        ; Alcotest.test_case
            "equal totals break ties on sql"
            `Quick
            test_equal_totals_break_ties_on_sql_for_stability
        ; Alcotest.test_case
            "pct_of_profile sums to 100"
            `Quick
            test_pct_of_profile_sums_to_100_per_profile
        ; Alcotest.test_case "reset clears" `Quick test_reset_clears
        ; Alcotest.test_case "empty table renders" `Quick test_empty_table_renders
        ] )
    ; ( "coverage"
      , [ Alcotest.test_case
            "attributed_pct against driver service_ms"
            `Quick
            test_attributed_pct_against_driver_service_ms
        ; Alcotest.test_case
            "attributed_pct over 100 renders"
            `Quick
            test_attributed_pct_over_100_renders
        ; Alcotest.test_case
            "missing driver service_ms is zero"
            `Quick
            test_missing_driver_service_ms_is_zero_not_a_crash
        ] )
    ; ("gate", [ Alcotest.test_case "disabled by default" `Quick test_disabled_by_default ])
    ]
;;
```

Add to `test/dune`, immediately after the `test_tpch_check` stanza:

```
(test
 (name test_tpcc_stmt_profile)
 (modules test_tpcc_stmt_profile)
 (libraries granary_tpc alcotest))
```

- [ ] **Step 2: Run the test to verify it fails**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev \
  dune test test/test_tpcc_stmt_profile.exe
```

Expected: FAIL — `Unbound module Tpcc_stmt_profile`.

- [ ] **Step 3: Write the interface**

Create `test/tpc/tpcc_stmt_profile.mli`:

```ocaml
(** Per-statement attribution for the TPC-C harness (#714).

    granary serialises writers: [Rwlock] lets many readers run concurrently and
    never blocks them, and only writers exclude each other.  A TPC-C
    transaction is explicit and multi-statement, and {!Granary.Db.begin_txn}
    takes the writer lock unconditionally at [BEGIN], so a whole transaction
    runs inside the critical section and

    {[ NewOrder/sec = 1 / (time a NewOrder holds the writer lock) ]}

    This module attributes that time across the statement shapes the profiles
    issue, so the work of shrinking it can start from a measurement.

    {b Read the numbers only from a run at [GRANARY_TPCC_TERMINALS=1].}  At one
    terminal there is exactly one writer, so [rw_begin] never blocks and every
    millisecond recorded is in-lock {e work}.  With more terminals the same
    figures silently absorb lock {e wait}, which the terminal sweep in
    [docs/benchmarks/BENCHMARKS-TPCC.md] already reports and which this module
    cannot separate.

    The accumulator itself is unconditional — {!record} always records.  The
    on/off gate is {!enabled}, consulted at the single instrumentation site in
    {!Tpcc_txn.run}, which keeps the arithmetic here directly testable. *)

(** [true] when [GRANARY_TPCC_STMT_PROFILE] is set to a non-empty string.

    Read {e once}, at module initialisation.  Once rather than per call so the
    disabled path costs one bool test, and so a mid-run environment change
    cannot produce a half-profiled run. *)
val enabled : bool

(** [record ~profile ~sql ~rows ~secs] adds one statement observation.

    [profile] must be one of {!Tpcc_txn.all}'s [name] fields, because
    {!summaries} joins on it against [Tpcc_driver.profile_stats.name].  [sql]
    is the {e raw, unrendered} shape string — the same key
    {!Tpcc_conn}'s statement cache uses (#697) — so that one shape accumulates
    one entry rather than one per distinct parameter value.  [rows] is the
    number of rows a query returned, and [0] for a non-query. *)
val record : profile:string -> sql:string -> rows:int -> secs:float -> unit

(** [reset ()] discards every observation.  Called between the driver's
    warm-up and measured windows so cold-cache first touches and the one-off
    [Db.prepare] per shape do not contaminate steady-state means. *)
val reset : unit -> unit

(** One accumulated statement shape within one profile.  [total_ms] and
    [mean_ms] are milliseconds; [pct_of_profile] is [total_ms] as a percentage
    of every statement recorded under the same [profile]. *)
type entry =
  { profile : string
  ; sql : string
  ; calls : int
  ; rows : int
  ; total_ms : float
  ; mean_ms : float
  ; pct_of_profile : float
  }

(** [ranked ()] returns every entry, costliest profile first and costliest
    statement first within each profile.  Ties break on the SQL text, so the
    order is stable across runs rather than dependent on hash-table iteration
    order. *)
val ranked : unit -> entry list

(** Per-profile coverage: how much of the driver's own measured service time
    the recorded statements account for.  [attributed_pct] is
    [statements_total_ms] as a percentage of [driver_service_ms], and is
    deliberately {e not} clamped — a figure above 100 is a real signal, and a
    figure well below it says the cost is driver or scheduling overhead rather
    than engine work, which is the difference between a table worth acting on
    and one worth discarding. *)
type summary =
  { profile : string
  ; statements_total_ms : float
  ; driver_service_ms : float
  ; attributed_pct : float
  }

(** [summaries ~service_ms] pairs each profile's recorded statement time with
    that profile's total driver service time, looked up by name from
    [service_ms].  A profile absent from [service_ms] gets [0.0], and an
    [attributed_pct] of [0.0] rather than a division by zero.  Ordered by
    profile name. *)
val summaries : service_ms:(string * float) list -> summary list

(** [report ~service_ms] renders {!ranked} and {!summaries} as a human-readable
    block for stderr.  Returns a one-line notice when nothing was recorded. *)
val report : service_ms:(string * float) list -> string

(** [to_csv ~path ~service_ms] writes two CSV tables to [path], separated by a
    blank line: {!ranked} first, then {!summaries}.  Two tables in one file
    because they are one measurement and separating them invites reading the
    per-statement table without its own coverage figure. *)
val to_csv : path:string -> service_ms:(string * float) list -> unit
```

- [ ] **Step 4: Write the implementation**

Create `test/tpc/tpcc_stmt_profile.ml`:

```ocaml
let enabled = Bench_report.env_str "GRANARY_TPCC_STMT_PROFILE" "" <> ""

type acc =
  { mutable calls : int
  ; mutable rows : int
  ; mutable total_secs : float
  }

(* Keyed by (profile, sql).  Not by sql alone: BEGIN, COMMIT and ROLLBACK
   recur across all five profiles, and lumping them would hide which profile's
   commit is expensive — the single most likely thing this table needs to
   distinguish.

   A global table needs no locking: the harness is single-domain, Lwt is
   cooperative, and [record] performs no await, so no other fiber can
   interleave inside it. *)
let table : (string * string, acc) Hashtbl.t = Hashtbl.create 64

let record ~profile ~sql ~rows ~secs =
  match Hashtbl.find_opt table (profile, sql) with
  | Some a ->
    a.calls <- a.calls + 1;
    a.rows <- a.rows + rows;
    a.total_secs <- a.total_secs +. secs
  | None -> Hashtbl.add table (profile, sql) { calls = 1; rows; total_secs = secs }
;;

let reset () = Hashtbl.reset table

type entry =
  { profile : string
  ; sql : string
  ; calls : int
  ; rows : int
  ; total_ms : float
  ; mean_ms : float
  ; pct_of_profile : float
  }

type summary =
  { profile : string
  ; statements_total_ms : float
  ; driver_service_ms : float
  ; attributed_pct : float
  }

(* Total seconds per profile — the denominator for [pct_of_profile] and the
   numerator for [summaries]. *)
let profile_totals () =
  let h = Hashtbl.create 8 in
  Hashtbl.iter
    (fun (profile, _) a ->
       let prev = Option.value (Hashtbl.find_opt h profile) ~default:0.0 in
       Hashtbl.replace h profile (prev +. a.total_secs))
    table;
  h
;;

let ranked () =
  let totals = profile_totals () in
  let cost_of p = Option.value (Hashtbl.find_opt totals p) ~default:0.0 in
  let entries =
    Hashtbl.fold
      (fun (profile, sql) a acc ->
         let total_ms = 1000.0 *. a.total_secs in
         let denom = cost_of profile in
         { profile
         ; sql
         ; calls = a.calls
         ; rows = a.rows
         ; total_ms
         ; mean_ms = total_ms /. float_of_int (max 1 a.calls)
         ; pct_of_profile = (if denom > 0.0 then 100.0 *. a.total_secs /. denom else 0.0)
         }
         :: acc)
      table
      []
  in
  (* Costliest profile first, costliest statement first within it, SQL text
     breaking ties — a total order, so the output does not move between runs
     with the hash-table's iteration order. *)
  let key e = -.cost_of e.profile, e.profile, -.e.total_ms, e.sql in
  List.sort (fun a b -> compare (key a) (key b)) entries
;;

let summaries ~service_ms =
  let totals = profile_totals () in
  Hashtbl.fold (fun profile secs acc -> (profile, secs) :: acc) totals []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  |> List.map (fun (profile, secs) ->
    let statements_total_ms = 1000.0 *. secs in
    let driver_service_ms = Option.value (List.assoc_opt profile service_ms) ~default:0.0 in
    { profile
    ; statements_total_ms
    ; driver_service_ms
    ; attributed_pct =
        (if driver_service_ms > 0.0
         then 100.0 *. statements_total_ms /. driver_service_ms
         else 0.0)
    })
;;

(* The stderr table is for reading; the CSV is the artifact.  A 200-character
   statement wraps and destroys the column alignment that makes the table
   worth printing at all, so elide there and never in the CSV. *)
let elide ~max s =
  if String.length s <= max then s else String.sub s 0 (max - 3) ^ "..."
;;

let report ~service_ms =
  match ranked () with
  | [] -> "tpcc: statement profile empty (no statements recorded)\n"
  | entries ->
    let buf = Buffer.create 4096 in
    Buffer.add_string
      buf
      "\ntpcc per-statement profile — in-lock work; read only at TERMINALS=1\n";
    List.iter
      (fun e ->
         Buffer.add_string
           buf
           (Printf.sprintf
              "  %-12s %9.1f ms %5.1f%% %6d calls %9d rows %8.3f ms/call  %s\n"
              e.profile
              e.total_ms
              e.pct_of_profile
              e.calls
              e.rows
              e.mean_ms
              (elide ~max:76 e.sql)))
      entries;
    Buffer.add_string buf "\ncoverage — summed statement time vs driver service time\n";
    List.iter
      (fun s ->
         Buffer.add_string
           buf
           (Printf.sprintf
              "  %-12s %9.1f ms of %9.1f ms  %6.1f%% attributed\n"
              s.profile
              s.statements_total_ms
              s.driver_service_ms
              s.attributed_pct))
      (summaries ~service_ms);
    Buffer.contents buf
;;

let to_csv ~path ~service_ms =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
       let line s = output_string oc (s ^ "\n") in
       line
         (Bench_report.Csv.header
            [ "profile"; "sql"; "calls"; "rows"; "total_ms"; "mean_ms"; "pct_of_profile" ]);
       List.iter
         (fun e ->
            line
              (Bench_report.Csv.row
                 [ e.profile
                 ; e.sql
                 ; string_of_int e.calls
                 ; string_of_int e.rows
                 ; Printf.sprintf "%.3f" e.total_ms
                 ; Printf.sprintf "%.4f" e.mean_ms
                 ; Printf.sprintf "%.2f" e.pct_of_profile
                 ]))
         (ranked ());
       line "";
       line
         (Bench_report.Csv.header
            [ "profile"; "statements_total_ms"; "driver_service_ms"; "attributed_pct" ]);
       List.iter
         (fun s ->
            line
              (Bench_report.Csv.row
                 [ s.profile
                 ; Printf.sprintf "%.3f" s.statements_total_ms
                 ; Printf.sprintf "%.3f" s.driver_service_ms
                 ; Printf.sprintf "%.2f" s.attributed_pct
                 ]))
         (summaries ~service_ms))
;;
```

- [ ] **Step 5: Run the tests to verify they pass**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev \
  dune test test/test_tpcc_stmt_profile.exe
```

Expected: PASS, all 11 cases.

- [ ] **Step 6: Format, lint, commit**

```sh
sh scripts/check-fmt.sh --fix
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
git add test/tpc/tpcc_stmt_profile.ml test/tpc/tpcc_stmt_profile.mli \
        test/test_tpcc_stmt_profile.ml test/dune
git commit -m "perf(#714): Tpcc_stmt_profile, a (profile, sql)-keyed statement accumulator

Keyed by (profile, sql) rather than sql alone because BEGIN/COMMIT/ROLLBACK
recur across all five profiles. Unconditional — the enabled gate lives at the
instrumentation call site — so the arithmetic is directly unit-testable.

Reports summed statement time against the driver's own service_ms, unclamped,
so unattributed driver overhead is visible rather than silently chased as an
engine cost.

Refs #714, #555"
```

Expect 0 merlint issues for the new files.

---

### Task 2: Instrument `Tpcc_txn.run`

`Tpcc_txn.run` is the single dispatch point that knows both the `ops` record and which profile is executing. Instrumenting there — rather than in `Tpcc_conn.ops` — needs no change to the `ops` type, no change to any caller, and covers `BEGIN`/`COMMIT`/`ROLLBACK` as well as DML. That matters: `COMMIT` is where group commit and its fsync wait land, and it is a plausible candidate for a large share of the transaction.

**Files:**
- Modify: `test/tpc/tpcc_txn.ml` (add helpers before `let run` at `:1095`; wrap `ops` inside `run`)
- Modify: `test/test_tpcc_stmt_profile.ml` (add the drift and inertness cases)

**Interfaces:**
- Consumes: `Tpcc_stmt_profile.{enabled,record}` from Task 1; `Tpcc_txn.{all,ops,stmt,input}` which already exist.
- Produces: nothing new in the `.mli` — `profile_name`, `timed_query`, `timed_exec` and `instrument` are all internal to `tpcc_txn.ml`. `Tpcc_txn.run`'s existing signature `ops -> input -> unit Lwt.t` is unchanged.

- [ ] **Step 1: Write the failing tests**

Append to `test/test_tpcc_stmt_profile.ml`, before the `let () = Alcotest.run …` block:

```ocaml
(* A mock [ops] that answers every statement with no rows and records nothing
   of its own — enough to drive [Tpcc_txn.run] to completion. *)
let mock_ops () =
  { Tpcc_txn.query = (fun _ -> Lwt.return [])
  ; Tpcc_txn.exec = (fun _ -> Lwt.return_unit)
  }
;;

let a_stock_level_input =
  Tpcc_txn.Stock_level_input { w_id = 1; d_id = 1; threshold = 15 }
;;

(* The gate is at [Tpcc_txn.run]'s single call site, so with the environment
   variable unset a normal benchmark run records nothing at all. *)
let test_run_is_inert_when_disabled () =
  fresh ();
  Lwt_main.run (Tpcc_txn.run (mock_ops ()) a_stock_level_input);
  Alcotest.(check int) "nothing recorded" 0 (List.length (P.ranked ()))
;;
```

And add to the `"gate"` group in `Alcotest.run`:

```ocaml
      ; Alcotest.test_case "run is inert when disabled" `Quick test_run_is_inert_when_disabled
```

The test's `(test …)` stanza in `test/dune` needs `lwt` added to its libraries:

```
(test
 (name test_tpcc_stmt_profile)
 (modules test_tpcc_stmt_profile)
 (libraries granary_tpc alcotest lwt lwt.unix))
```

- [ ] **Step 2: Run the test to verify it fails**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev \
  dune test test/test_tpcc_stmt_profile.exe
```

Expected: FAIL — the `Stock_level_input` profile raises or the mock is rejected before the inertness assertion is reached. If it unexpectedly PASSES, that is still a real signal (the gate is already inert), but the test must exist before Step 3 adds the instrumentation it guards.

- [ ] **Step 3: Write the implementation**

In `test/tpc/tpcc_txn.ml`, insert immediately before `let run ops input =` (currently `:1095`):

```ocaml
(* --- #714 statement profiling ---------------------------------------- *)

let kind_of_input = function
  | New_order_input _ -> New_order
  | Payment_input _ -> Payment
  | Order_status_input _ -> Order_status
  | Delivery_input _ -> Delivery
  | Stock_level_input _ -> Stock_level
;;

(* Looked up from [all] rather than spelled out a second time.  The profiler's
   coverage summary joins on this name against [Tpcc_driver.profile_stats.name],
   which is also taken from [all], so a hand-written table here could drift and
   silently produce a profile whose statements attribute to nothing. *)
let profile_name input =
  let k = kind_of_input input in
  match List.find_opt (fun p -> p.kind = k) all with
  | Some p -> p.name
  | None -> invalid_arg "Tpcc_txn.profile_name: input's kind is not in [all]"
;;

(* Timing brackets the promise's RESOLUTION, not the call that creates it, or
   every statement would read as free.

   Both outcomes are recorded.  Recording only success would silently drop the
   statement that ends NewOrder's spec-mandated ~1% invalid-item rollback —
   the one statement on that path whose cost the table exists to show. *)
let timed_query ~profile f s =
  let t0 = Unix.gettimeofday () in
  let stop rows =
    Tpcc_stmt_profile.record
      ~profile
      ~sql:s.sql
      ~rows
      ~secs:(Unix.gettimeofday () -. t0)
  in
  Lwt.try_bind
    (fun () -> f s)
    (fun rows ->
       stop (List.length rows);
       Lwt.return rows)
    (fun exn ->
       stop 0;
       Lwt.fail exn)
;;

let timed_exec ~profile f s =
  let t0 = Unix.gettimeofday () in
  let stop () =
    Tpcc_stmt_profile.record ~profile ~sql:s.sql ~rows:0 ~secs:(Unix.gettimeofday () -. t0)
  in
  Lwt.try_bind
    (fun () -> f s)
    (fun () ->
       stop ();
       Lwt.return_unit)
    (fun exn ->
       stop ();
       Lwt.fail exn)
;;

let instrument ~profile ops =
  { query = timed_query ~profile ops.query; exec = timed_exec ~profile ops.exec }
;;
```

Then change `run`'s head from

```ocaml
let run ops input =
  match input with
```

to

```ocaml
let run ops input =
  let ops =
    if Tpcc_stmt_profile.enabled then instrument ~profile:(profile_name input) ops else ops
  in
  match input with
```

If `Lwt.fail` draws a deprecation warning in this tree, use `Lwt.reraise exn` instead — match whichever form `test/tpc/tpcc_txn.ml` already uses in `with_rollback`.

- [ ] **Step 4: Run the tests to verify they pass**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev \
  dune test test/test_tpcc_stmt_profile.exe
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev \
  dune test test/test_tpcc_txn.exe
```

Expected: both PASS. `test_tpcc_txn` must be unchanged — `run`'s behaviour with the gate off is identical.

- [ ] **Step 5: Format, lint, commit**

```sh
sh scripts/check-fmt.sh --fix
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
git add test/tpc/tpcc_txn.ml test/test_tpcc_stmt_profile.ml test/dune
git commit -m "perf(#714): instrument Tpcc_txn.run with the statement profiler

run is the single dispatch point that knows both the ops record and which
profile is executing, so instrumenting there needs no change to the ops type,
no change to any caller, and covers BEGIN/COMMIT/ROLLBACK as well as DML —
COMMIT is where group commit and its fsync wait land.

profile_name is looked up from [all] rather than spelled out again, so it
cannot drift from the name the coverage summary joins on. Both the success and
the failure path record, or NewOrder's ~1% invalid-item rollback would drop the
one statement on that path.

Refs #714"
```

---

### Task 3: Reset between the warm-up and measured windows

**Files:**
- Modify: `test/tpc/tpcc_driver.ml` (in `run`, around `:361-364`)

**Interfaces:**
- Consumes: `Tpcc_stmt_profile.reset` from Task 1.
- Produces: nothing new. `Tpcc_driver.run`'s signature is unchanged.

- [ ] **Step 1: Write the implementation**

In `test/tpc/tpcc_driver.ml`, `run` currently reads:

```ocaml
  let warm = make_recorder ~record:false profiles in
  let* _ = run_interval pool warm ~config ~profiles ~duration:config.warmup_seconds in
  let rec_ = make_recorder ~record:true profiles in
```

Insert the reset between the warm-up and the measured recorder:

```ocaml
  let warm = make_recorder ~record:false profiles in
  let* _ = run_interval pool warm ~config ~profiles ~duration:config.warmup_seconds in
  (* #714: discard the warm-up window's statement observations, so cold-cache
     first touches and the one-off [Db.prepare] per shape do not contaminate
     the steady-state means.  Unconditional: on a non-profiling run this is
     [Hashtbl.reset] on an empty table. *)
  Tpcc_stmt_profile.reset ();
  let rec_ = make_recorder ~record:true profiles in
```

- [ ] **Step 2: Run the driver tests**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev \
  dune test test/test_tpcc_driver.exe
```

Expected: PASS, unchanged.

- [ ] **Step 3: Format, lint, commit**

```sh
sh scripts/check-fmt.sh --fix
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
git add test/tpc/tpcc_driver.ml
git commit -m "perf(#714): reset the statement profile between warm-up and measurement

Cold-cache first touches and the one-off Db.prepare per shape would otherwise
dominate the steady-state means the table exists to report.

Refs #714"
```

---

### Task 4: Report from `bench_tpcc`

Both engines drive `Tpcc_txn.run`, so reference-SQLite statements would land under the same `(profile, sql)` keys as granary's. Emitting the report immediately after each engine's `D.run` — which itself resets after its warm-up — keeps the two engines' figures separate with no extra bookkeeping.

**Files:**
- Modify: `test/bench_tpcc.ml` (in `run_engine`, around `:241-252`)
- Modify: `docs/benchmarks/BENCHMARKS-TPCC.md` (the "Environment knobs" table, `:116-129`)

**Interfaces:**
- Consumes: `Tpcc_stmt_profile.{enabled,report,to_csv}` from Task 1; `Tpcc_driver.profile_stats.{name,service_ms,committed}` which already exist.
- Produces: nothing consumed by a later task.

- [ ] **Step 1: Write the implementation**

`run_engine` in `test/bench_tpcc.ml` currently reads:

```ocaml
  let result = Lwt_main.run (D.run config ~workers:(List.map T.run workers)) in
```

followed by the check and `prerr_string (D.summary result)`. After the `D.run` line and before `prerr_string (D.summary result)`, insert:

```ocaml
  (* #714: [D.run] resets the profile after its own warm-up window, so what is
     here is this engine's measured interval alone — both engines drive
     [Tpcc_txn.run] and would otherwise share the (profile, sql) keys.
     [service_ms] is a mean over committed transactions, so the profile's total
     is that mean times the committed count. *)
  if Granary_tpc.Tpcc_stmt_profile.enabled
  then (
    let service_ms =
      List.map
        (fun (p : D.profile_stats) ->
           p.D.name, p.D.service_ms *. float_of_int p.D.committed)
        result.D.per_profile
    in
    Printf.eprintf "[%s]%s%!" engine (Granary_tpc.Tpcc_stmt_profile.report ~service_ms);
    match BR.env_str "GRANARY_TPCC_STMT_PROFILE_CSV" "" with
    | "" -> ()
    | dir ->
      let path = Filename.concat dir (Printf.sprintf "tpcc-stmt-profile-%s.csv" engine) in
      Granary_tpc.Tpcc_stmt_profile.to_csv ~path ~service_ms;
      Printf.eprintf "[%s] statement profile CSV: %s\n%!" engine path);
```

`GRANARY_TPCC_STMT_PROFILE_CSV` names a **directory**, and the engine name is appended, so a two-engine run cannot have one engine silently overwrite the other's file. The directory must already exist; `bench/results/` does.

Note the module path: `bench_tpcc.ml` already aliases `module BR = Granary_tpc.Bench_report` and `module D = Granary_tpc.Tpcc_driver`. Add `module SP = Granary_tpc.Tpcc_stmt_profile` alongside them and use `SP.` rather than the fully-qualified name, to match the file's existing style.

- [ ] **Step 2: Document the two knobs**

In `docs/benchmarks/BENCHMARKS-TPCC.md`, add two rows to the "Environment knobs" table, after the `GRANARY_TPCC_ORACLE_SELFTEST` row:

```
| `GRANARY_TPCC_STMT_PROFILE` | unset | per-statement attribution (#714); read only at `TERMINALS=1` |
| `GRANARY_TPCC_STMT_PROFILE_CSV` | unset | directory for the profile CSV; engine name is appended |
```

- [ ] **Step 3: Verify it builds**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev \
  dune build test/bench_tpcc.exe
```

Expected: builds cleanly. (`bench_tpcc` is `(optional)`; if the image lacks the `sqlite3` bindings the target will not exist — in that case run `dune build @check` and confirm no error mentions `bench_tpcc.ml`.)

- [ ] **Step 4: Format, lint, commit**

```sh
sh scripts/check-fmt.sh --fix
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
git add test/bench_tpcc.ml docs/benchmarks/BENCHMARKS-TPCC.md
git commit -m "perf(#714): report the statement profile from bench_tpcc

Emitted immediately after each engine's D.run, which resets after its own
warm-up, so the two engines' figures stay separate with no extra bookkeeping.
The CSV knob names a directory and the engine name is appended, so neither
engine can overwrite the other's file.

Refs #714"
```

---

### Task 5: Run the profile and publish the attribution

The only task that produces numbers. It changes no code.

**Files:**
- Create: `bench/results/2026-08-11-tpcc-stmt-profile-granary.csv` (whatever the run writes)
- Modify: `docs/benchmarks/BENCHMARKS-TPCC.md` (a new section after the post-#706 sweep)

**Interfaces:**
- Consumes: everything from Tasks 1–4.
- Produces: the table the follow-up fix will be designed against.

- [ ] **Step 1: Run the profile at one terminal**

From the worktree root:

```sh
mkdir -p bench/results
podman run --rm --user 0 -v "$(pwd):/workspace:z" -w /workspace \
  -e GRANARY_TPCC_WAREHOUSES=1 \
  -e GRANARY_TPCC_TERMINALS=1 \
  -e GRANARY_TPCC_SECONDS=10 \
  -e GRANARY_TPCC_WARMUP_SECONDS=2 \
  -e GRANARY_TPCC_ENGINES=granary \
  -e GRANARY_TPCC_STMT_PROFILE=1 \
  -e GRANARY_TPCC_STMT_PROFILE_CSV=bench/results \
  granary-dev dune exec test/bench_tpcc.exe 2>&1 | tee /tmp/714-profile.log
```

`TERMINALS=1` is not optional — at one terminal there is exactly one writer, so every millisecond recorded is in-lock work rather than lock wait. `ENGINES=granary` skips the reference SQLite load, which costs ~45 s and measures nothing this run is about.

Rename the CSV the run wrote to `bench/results/2026-08-11-tpcc-stmt-profile-granary.csv`.

- [ ] **Step 2: Read the coverage line first**

Before reading the per-statement table, read the `coverage` block. If `attributed_pct` for `new_order` is well below 100, the missing fraction is driver or scheduling overhead and **the fix does not live in the engine** — say so in the write-up rather than proceeding to rank engine statements. If it is near or above 100, the per-statement table is the real attribution.

- [ ] **Step 3: Write the section**

Append a section to `docs/benchmarks/BENCHMARKS-TPCC.md`, after "Terminal sweep, W=1, granary, worker-handle driver, post-#706":

```markdown
### Per-statement attribution of the in-lock critical section (#714)
```

It must state, in this order:

1. The methodology commitment — `TERMINALS=1`, and why (one writer, so the figures are in-lock work rather than lock wait).
2. The **coverage** figures per profile, before any per-statement table. A table that accounts for only part of the service time must say so where nobody can miss it.
3. The ranked per-statement table for `new_order`, at minimum: `sql`, `calls`, `rows`, `total_ms`, `mean_ms`, `%`.
4. What the table implies for the fix, and what it rules out — stated as a reading of the numbers, not a plan.
5. A pointer to the CSV under `bench/results/`.

Do not propose or scope the fix here; that is a separate issue, filed against this table.

- [ ] **Step 4: Commit**

```sh
git add bench/results/2026-08-11-tpcc-stmt-profile-granary.csv \
        docs/benchmarks/BENCHMARKS-TPCC.md
git commit -m "docs(#714): per-statement attribution of the TPC-C in-lock critical section

Measured at W=1, TERMINALS=1 — one writer, so every millisecond is in-lock work
rather than lock wait. Coverage against the driver's own service_ms is reported
first, so a table that accounts for only part of the transaction says so before
anyone acts on its ranking.

Refs #714, #555"
```

---

### Task 6: Full suite, dune-file check, and the PR

**Files:** none modified.

- [ ] **Step 1: Run the full suite**

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test
```

Expected: PASS. If a `GRANARY_BENCH_*` gate fails, check `uptime` first — sibling agents running suites in parallel are the usual cause, and the gates print their per-trial numbers.

- [ ] **Step 2: Verify dune-file formatting from the main checkout**

`check-fmt.sh` reports `◐ … Dune files UNVERIFIED` inside a worktree, and this branch modifies `test/dune`. Run the real gate where it works:

```sh
cd /home/tej/projects/granary
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build @fmt
```

- [ ] **Step 3: Push and open the PR**

```sh
cd /home/tej/projects/granary/.worktrees/714-stmt-profile
git push origin perf/714-tpcc-stmt-profile
~/.local/bin/forgejo pr create IoTReadyNext/granary \
  --title="perf(#714): TPC-C per-statement attribution profiler for the in-lock critical section" \
  --head=perf/714-tpcc-stmt-profile \
  --base=main \
  --body="$(cat <<'EOF'
## Summary

granary serialises writers, so `NewOrder/sec = 1 / (time a NewOrder holds the
writer lock)`. The post-#706 sweep measures that at 17.6 ms per NewOrder with
zero contention, and throughput flat at 28-33 NewOrder/sec from 1 to 16
terminals. Nothing in the tree attributed those 17.6 ms across the ~35
statements a NewOrder issues.

This adds an env-gated per-statement profiler to the TPC-C harness and
publishes the measured attribution. **No engine change** — the fix is designed
from the table, in a separate issue.

- `Tpcc_stmt_profile`, keyed by `(profile, sql)` because `BEGIN`/`COMMIT`
  recur across all five profiles
- instrumented at `Tpcc_txn.run`, the one place that knows both the `ops`
  record and the executing profile, so `COMMIT` (where group commit and its
  fsync wait land) is covered too
- reset between the driver's warm-up and measured windows
- reports summed statement time **against the driver's own `service_ms`**,
  unclamped, so unattributed driver overhead is visible rather than silently
  chased as an engine cost
- read only at `GRANARY_TPCC_TERMINALS=1`: at one terminal there is exactly one
  writer, so the figures are in-lock *work* rather than lock *wait*

## Test plan

- [ ] `dune test` passes
- [ ] `test_tpcc_stmt_profile` covers aggregation, per-profile key separation,
      ranking and tie-breaking, the `%` denominators, over-100% coverage,
      missing `service_ms`, empty rendering, `reset`, and inertness when the
      env var is unset
- [ ] `dune build @fmt` verified from the main checkout (this branch touches
      `test/dune`)
- [ ] profile run at W=1, `TERMINALS=1` committed under `bench/results/`

Closes #714
EOF
)"
```

---

## Self-review

**Spec coverage.** Every section of `2026-08-11-714-tpcc-stmt-profile-design.md` maps to a task: the accumulator and its unit tests → Task 1; profile identity and the instrumentation point → Task 2; warm-up exclusion → Task 3; reporting, CSV and the coverage line → Task 4; the measurement and the published section → Task 5.

**Two deliberate deviations from the spec, both improvements found while reading the code:**

1. **The instrumentation lives in `Tpcc_txn.run`, not in `Tpcc_conn.ops`, and the `ops` record gains no `profile` field.** `run` already dispatches on the input variant, so it knows the profile without any signature change, and it covers `BEGIN`/`COMMIT`/`ROLLBACK` — which `Tpcc_conn` routes down a separate `is_control_stmt` path that would have needed instrumenting twice. Strictly less code and strictly more coverage.
2. **The CSV path is an env-named directory rather than a fixed `bench/results/` path, and the engine name is appended.** `bench_tpcc` runs two engines through the same `Tpcc_txn.run`; a fixed path would let the second engine silently overwrite the first's file.

**Placeholder scan.** No TBD/TODO; every code step carries the actual code; Task 5's write-up step enumerates the five things the section must state rather than saying "document the results".

**Type consistency.** `entry` and `summary` field names are identical in the `.mli` (Task 1 Step 3), the `.ml` (Step 4), the tests (Step 1) and `bench_tpcc`'s consumption (Task 4). `record`'s four labelled arguments match at all three call sites. `summaries ~service_ms` takes `(string * float) list` in the interface, the tests, and `bench_tpcc`'s construction from `per_profile`.

**One risk worth flagging to the implementer.** `Tpcc_driver.profile_stats.service_ms` is a *mean over committed transactions*, so Task 4 reconstructs the total as `service_ms *. float_of_int committed`. If that reconstruction is wrong, every `attributed_pct` is wrong by the same factor — and it would look plausible. Confirm against `stats_of_acc` (`test/tpc/tpcc_driver.ml:172-186`) before trusting Task 5's numbers.
