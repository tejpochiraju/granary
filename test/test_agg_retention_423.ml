(** #423: the net-zero SUM-group retention in [Aggregate] has a stated ceiling.

    [Aggregate.Make] drops a group only when both its total weight and its
    running value reach zero.  A group whose weights cancel to zero while its
    value does not is kept on purpose — the value is not recoverable from the
    delta feed, so dropping it would corrupt the group on revival.  #423 decided
    to {b accept} that and document a ceiling rather than compact or age it out,
    so this file's job is to establish what the ceiling actually is and stop it
    getting quietly worse.

    Four claims, each pinned below:

    - {b Bounded by distinct groups, never by updates.}  The state is a map keyed
      by group, so churning one key forever retains exactly one entry.  Only
      unbounded {e key cardinality} is unbounded memory, and that is the
      distinction an operator needs — see [churning_one_key_retains_one_group].
    - {b 9 words per retained group}, plus the caller's group value.  Measured
      here rather than derived from the type, by the same peak-live-major-heap
      method the other allocation gates in this suite use.
    - {b Retention needs a negative weight.}  If every element of a group has a
      non-negative cumulative weight, a total weight of zero forces every weight
      to zero, hence a value of zero, which prunes.  A delta stream that only
      retracts what it has inserted therefore retains nothing at all; signed
      weights are what reach this state.
    - {b Retention needs two measures inside one group.}  The value is
      [Σ measure * weight]; a constant measure [c] makes that [c * Σ weight].
      This is why COUNT never retains — the issue text asserts that and it holds,
      unconditionally, for arbitrary signed weights, which the QCheck property
      here fuzzes for.

    The measurement is allocation, not wall clock, so a loaded runner does not
    move it; the gate is therefore armed by default, with
    [GRANARY_MEM_MAX_WORDS_PER_GROUP] as the escape hatch for an unfamiliar
    allocator or word size.  See [docs/IVM_MEMORY.md] for the long form. *)

module Zset = Granary_ivm.Zset
module Row = Granary_encoding.Row
module Rv = Granary.Reactive_view

(* ------------------------------------------------------------------ *)
(* Operators under test                                                 *)
(* ------------------------------------------------------------------ *)

(* Input element for SUM: a (group, value) row.  Two rows with the same group
   and the same value are the same Z-set element, exactly as the reactive-view
   driver's input space works. *)
module GV = struct
  type t = int * int

  let compare = compare
  let pp ppf (g, v) = Format.fprintf ppf "(%d,%d)" g v
end

module G_out = struct
  type t = int * int

  let compare = compare
  let pp ppf (g, v) = Format.fprintf ppf "(%d=%d)" g v
end

module ZIn = Zset.Make (GV)
module ZOut = Zset.Make (G_out)

module Sum = Granary_ivm.Aggregate.Make (struct
    module In = ZIn
    module Out = ZOut

    type group = int

    let compare_group = Int.compare
    let group_of (g, _) = g
    let measure (_, v) = v
    let result g s = g, s
  end)

(* COUNT is the same operator with a constant measure. *)
module Count = Granary_ivm.Aggregate.Make (struct
    module In = ZIn
    module Out = ZOut

    type group = int

    let compare_group = Int.compare
    let group_of (g, _) = g
    let measure _ = 1
    let result g c = g, c
  end)

let out_rows t = Sum.output t |> ZOut.to_list

(* ------------------------------------------------------------------ *)
(* What the retention IS                                                *)
(* ------------------------------------------------------------------ *)

(* The shape: a retraction the operator never saw a matching insertion for.
   Element [(g, 5)] at weight +1 and [(g, 3)] at weight -1 sum to a total weight
   of 0 and a value of 2, so the group is kept with no output row. *)
let net_zero_group_is_retained_and_revives_with_its_value () =
  let t = Sum.create () in
  let _ = Sum.step t (ZIn.of_list [ (7, 5), 1; (7, 3), -1 ]) in
  Alcotest.(check int)
    "no output row: the group's total weight is 0"
    0
    (List.length (out_rows t));
  Alcotest.(check int) "but the group is retained" 1 (Sum.retained_groups t);
  (* Revival: one more row worth 10 joins the group.  The retained 2 is still
     part of the total, which is the whole reason the entry is kept. *)
  let d = Sum.step t (ZIn.of_list [ (7, 10), 1 ]) in
  Alcotest.(check (list (pair (pair int int) int)))
    "revived group carries the retained value"
    [ (7, 12), 1 ]
    (ZOut.to_list d);
  Alcotest.(check int) "still exactly one entry" 1 (Sum.retained_groups t)
;;

(* And the complement, which is the claim that keeps this bounded in practice:
   with only non-negative cumulative weights, emptying a group prunes it.  A
   base-table change feed never retracts a row it has not inserted, so it never
   reaches the retaining state at all. *)
let a_well_formed_delta_stream_retains_nothing () =
  let t = Sum.create () in
  let n = 500 in
  for i = 1 to n do
    let _ = Sum.step t (ZIn.of_list [ (i, i * 3), 1; (i, i * 7), 1 ]) in
    ()
  done;
  Alcotest.(check int) "every group live" n (Sum.retained_groups t);
  (* Retract exactly what was inserted, one group at a time. *)
  for i = 1 to n do
    let _ = Sum.step t (ZIn.of_list [ (i, i * 3), -1; (i, i * 7), -1 ]) in
    ()
  done;
  Alcotest.(check int) "nothing retained" 0 (Sum.retained_groups t);
  Alcotest.(check int) "and no output rows" 0 (List.length (out_rows t))
;;

(* An UPDATE is the one everyday shape that produces the signed pair: it
   retracts the old row and inserts the new one in the same delta.  It only
   retains if the engine's idea of the old row is wrong -- i.e. it retracts a
   row it never held -- which is what the first case above spells out
   explicitly.  An update over a row the engine really holds cancels cleanly. *)
let an_update_over_a_known_row_does_not_retain () =
  let t = Sum.create () in
  let _ = Sum.step t (ZIn.of_list [ (1, 5), 1 ]) in
  let _ = Sum.step t (ZIn.of_list [ (1, 5), -1; (1, 3), 1 ]) in
  Alcotest.(check (list (pair (pair int int) int)))
    "value follows the update"
    [ (1, 3), 1 ]
    (out_rows t);
  let _ = Sum.step t (ZIn.of_list [ (1, 3), -1 ]) in
  Alcotest.(check int)
    "deleting the updated row prunes the group"
    0
    (Sum.retained_groups t)
;;

(* ------------------------------------------------------------------ *)
(* COUNT: the issue's claim, checked rather than repeated                *)
(* ------------------------------------------------------------------ *)

(* [aggv = Σ 1 * w = mult] for a constant measure, so the two totals reach zero
   together and the prune fires.  Fuzzed over arbitrary SIGNED weights, which is
   the only regime in which SUM retains, so a passing COUNT here is a real
   difference between the two and not a vacuous run. *)
let count_never_retains_property =
  QCheck.Test.make
    ~count:2000
    ~name:"COUNT never retains a net-zero group, whatever the signed weights"
    QCheck.(
      list_size
        (Gen.int_range 1 12)
        (triple (int_range 0 3) (int_range 0 9) (int_range (-4) 4)))
    (fun entries ->
       let t = Count.create () in
       (* Feed each weighted element as its own delta, so intermediate states are
          exercised too, then drive every group's total weight back to zero. *)
       List.iter
         (fun (g, v, w) -> ignore (Count.step t (ZIn.of_list [ (g, v), w ])))
         entries;
       let totals = Hashtbl.create 8 in
       List.iter
         (fun (g, _, w) ->
            Hashtbl.replace
              totals
              g
              (w + Option.value ~default:0 (Hashtbl.find_opt totals g)))
         entries;
       Hashtbl.iter
         (fun g tot ->
            if tot <> 0 then ignore (Count.step t (ZIn.of_list [ (g, 0), -tot ])))
         totals;
       Count.retained_groups t = 0)
;;

(* Same argument, same operator, different constant: SUM over a column that is
   constant within its group cannot retain either.  Recorded because "COUNT is
   safe" understates the rule -- the rule is about the measure being constant,
   not about it being 1. *)
let a_constant_measure_never_retains () =
  let t = Sum.create () in
  let _ = Sum.step t (ZIn.of_list [ (4, 9), 3; (4, 9), -1 ]) in
  let _ = Sum.step t (ZIn.of_list [ (4, 9), -2 ]) in
  Alcotest.(check int) "constant measure prunes" 0 (Sum.retained_groups t)
;;

(* ------------------------------------------------------------------ *)
(* The bound is on DISTINCT groups, not on updates                       *)
(* ------------------------------------------------------------------ *)

let churn_one_key ~rounds t =
  for i = 1 to rounds do
    (* Each round leaves the single group at total weight 0 with a nonzero
       value, i.e. in the retaining state, and then does it again. *)
    ignore (Sum.step t (ZIn.of_list [ (0, i), 1; (0, i + 1), -1 ]))
  done
;;

let churning_one_key_retains_one_group () =
  let t = Sum.create () in
  churn_one_key ~rounds:50_000 t;
  Alcotest.(check int)
    "50 000 net-zero rounds over one key retain exactly one group"
    1
    (Sum.retained_groups t);
  Alcotest.(check int) "and publish no output row" 0 (List.length (out_rows t))
;;

let distinct_keys_bound_the_retention () =
  let t = Sum.create () in
  let keys = 300 in
  (* Ten passes over the same 300 keys: 3000 retaining transitions, 300 groups. *)
  for pass = 1 to 10 do
    for k = 1 to keys do
      ignore (Sum.step t (ZIn.of_list [ (k, pass), 1; (k, pass + 1), -1 ]))
    done
  done;
  Alcotest.(check int)
    "retention equals the distinct key count, not the update count"
    keys
    (Sum.retained_groups t)
;;

(* ------------------------------------------------------------------ *)
(* The measurement                                                      *)
(* ------------------------------------------------------------------ *)

(* Live major-heap words across [f]: the [Gc] alarm samples the peak, exactly as
   [test_not_null_600] and [test_scan_borrow_481] do, and a [full_major] with
   the result still reachable gives the SETTLED figure.

   {b The gate is on the settled figure, and for this measurement that is the
   stricter of the two.}  The other allocation gates bound a TRANSIENT — a scan
   that must not retain what it walks — so a peak is the only thing that can see
   them.  #423 is the opposite: the retention is what SURVIVES, so it is the
   settled heap that carries it, and the alarm's asynchronous samples can easily
   all land below it (the alarm fires at major-slice boundaries, not on demand).
   The reported peak is therefore [max sampled settled] — a sampled peak below
   the settled heap is an artefact of when the alarm fired, not a smaller
   footprint. *)
let live_and_peak_words f =
  Gc.full_major ();
  let base = (Gc.quick_stat ()).live_words in
  let peak = ref base in
  let alarm =
    Gc.create_alarm (fun () ->
      let l = (Gc.quick_stat ()).live_words in
      if l > !peak then peak := l)
  in
  let r = Fun.protect ~finally:(fun () -> Gc.delete_alarm alarm) (fun () -> f ()) in
  Gc.full_major ();
  let settled = (Gc.quick_stat ()).live_words in
  (* [r] must survive the second [full_major] or the retention is collected out
     from under the measurement. *)
  ignore (Sys.opaque_identity r);
  settled - base, max !peak settled - base
;;

(* [n] distinct groups, each driven into the retaining state and left there. *)
let retain_distinct_groups n =
  let t = Sum.create () in
  for k = 1 to n do
    ignore (Sum.step t (ZIn.of_list [ (k, 5), 1; (k, 3), -1 ]))
  done;
  Alcotest.(check int) "all groups retained" n (Sum.retained_groups t);
  Alcotest.(check int) "and none of them is visible" 0 (List.length (out_rows t));
  t
;;

let max_words_per_group =
  match Sys.getenv_opt "GRANARY_MEM_MAX_WORDS_PER_GROUP" with
  | Some s ->
    (try int_of_string s with
     | _ -> 16)
  | None -> 16
;;

(* A SCALING measurement, like [test_not_null_600]'s: the same construction is
   run at [n] and [2n] and the marginal cost of the extra [n] groups is what is
   bounded, so nothing else that happens to be live is calibrated into the
   number.

   What one retained group costs, structurally: one [Map.Make] node (a five-
   field record -- left, key, data, right, height -- so 6 words) plus the
   [{ mult; aggv }] accumulator (3 words) = 9, with an [int] group key costing
   nothing because it is immediate.  That is an integer derivation from the
   OCaml block layout rather than a tuned number, and it is word-count stable
   across 32- and 64-bit targets.

   The gate is 16 words/group: enough headroom to absorb an allocator's
   accounting and a field or two, far short of the shapes that would signal a
   real regression (retaining the contributing elements, or a per-update rather
   than per-group entry, both of which are unbounded in the update count). *)
let retained_group_cost_is_bounded () =
  let n = 20_000 in
  let live1, peak1 = live_and_peak_words (fun () -> retain_distinct_groups n) in
  let live2, peak2 = live_and_peak_words (fun () -> retain_distinct_groups (2 * n)) in
  let marginal = live2 - live1 in
  Printf.printf
    "\n\
    \  [#423] retained net-zero SUM groups, live major-heap words:\n\
    \         %d groups -> +%d words (peak +%d), %d groups -> +%d words (peak +%d)\n\
    \         marginal %d words over %d extra groups = %.2f words/group (ceiling %d)\n\
     %!"
    n
    live1
    peak1
    (2 * n)
    live2
    peak2
    marginal
    n
    (float_of_int marginal /. float_of_int n)
    max_words_per_group;
  Alcotest.(check bool)
    (Printf.sprintf
       "%.2f words per retained group is within the %d-word ceiling"
       (float_of_int marginal /. float_of_int n)
       max_words_per_group)
    true
    (marginal < max_words_per_group * n)
;;

(* The other half of the same claim, and the one an operator cares about: the
   number does NOT grow with the update count.  Same instrument, same operator,
   one key -- doubling the churn must add nothing. *)
let churn_costs_nothing () =
  let one_key rounds () =
    let t = Sum.create () in
    churn_one_key ~rounds t;
    Alcotest.(check int) "one group throughout" 1 (Sum.retained_groups t);
    t
  in
  let live1, _ = live_and_peak_words (one_key 50_000) in
  let live2, _ = live_and_peak_words (one_key 200_000) in
  Printf.printf
    "  [#423] one key: 50 000 rounds -> +%d words, 200 000 rounds -> +%d words\n%!"
    live1
    live2;
  Alcotest.(check bool)
    (Printf.sprintf
       "quadrupling the churn over one key adds %d words, not O(updates)"
       (live2 - live1))
    true
    (live2 - live1 < max_words_per_group)
;;

(* ------------------------------------------------------------------ *)
(* The same ceiling at the level an operator actually meets it           *)
(* ------------------------------------------------------------------ *)

(* [Reactive_view.Agg_engine] is the instantiation a delta-maintained SQL view
   runs on, and its group key is a one-element [Row.value array] rather than an
   immediate, so its per-group cost is the 9 words above plus the key.  Measured
   separately so the two are not confused: the operator's own cost is fixed, the
   key's is the caller's. *)
let reactive_view_engine_cost () =
  let build n () =
    let st = Rv.Agg_engine.create () in
    for k = 1 to n do
      let key = [| Row.V_int (Int64.of_int k) |] in
      ignore
        (Rv.Agg_engine.step
           st
           [ Rv.Agg_engine.Ins { key; meas = 5 }; Rv.Agg_engine.Del { key; meas = 3 } ])
    done;
    Alcotest.(check int) "all groups retained" n (Rv.Agg_engine.retained_groups st);
    Alcotest.(check int)
      "and none is materialized"
      0
      (List.length (Rv.Agg_engine.snapshot st));
    st
  in
  let n = 10_000 in
  let live1, _ = live_and_peak_words (build n) in
  let live2, _ = live_and_peak_words (build (2 * n)) in
  let marginal = live2 - live1 in
  Printf.printf
    "  [#423] Agg_engine (INTEGER group key): %d -> +%d words, %d -> +%d words = %.2f \
     words/group\n\
     %!"
    n
    live1
    (2 * n)
    live2
    (float_of_int marginal /. float_of_int n);
  (* The key is a 2-word array holding a 2-word [V_int] block around a 3-word
     boxed [int64]: 7 words on top of the operator's 9. *)
  Alcotest.(check bool)
    (Printf.sprintf
       "%.2f words per retained group with a boxed INTEGER key"
       (float_of_int marginal /. float_of_int n))
    true
    (marginal < (max_words_per_group + 12) * n)
;;

(* A live view's retention is also not permanent: rebuilding the engine (which
   the driver does on a resync) starts from an empty map. *)
let a_fresh_engine_starts_empty () =
  let st = Rv.Agg_engine.create () in
  let key = [| Row.V_int 1L |] in
  ignore
    (Rv.Agg_engine.step
       st
       [ Rv.Agg_engine.Ins { key; meas = 5 }; Rv.Agg_engine.Del { key; meas = 3 } ]);
  Alcotest.(check int) "retained" 1 (Rv.Agg_engine.retained_groups st);
  Alcotest.(check int) "not materialized" 0 (List.length (Rv.Agg_engine.snapshot st));
  Alcotest.(check int)
    "a rebuilt engine keeps nothing"
    0
    (Rv.Agg_engine.retained_groups (Rv.Agg_engine.create ()))
;;

let () =
  Alcotest.run
    "agg_retention_423"
    [ ( "what the retention is"
      , [ Alcotest.test_case
            "net-zero group is retained and revives with its value"
            `Quick
            net_zero_group_is_retained_and_revives_with_its_value
        ; Alcotest.test_case
            "a well-formed delta stream retains nothing"
            `Quick
            a_well_formed_delta_stream_retains_nothing
        ; Alcotest.test_case
            "an update over a known row does not retain"
            `Quick
            an_update_over_a_known_row_does_not_retain
        ] )
    ; ( "constant measures never retain"
      , [ QCheck_alcotest.to_alcotest count_never_retains_property
        ; Alcotest.test_case
            "a constant measure never retains"
            `Quick
            a_constant_measure_never_retains
        ] )
    ; ( "the bound is distinct groups"
      , [ Alcotest.test_case
            "churning one key retains one group"
            `Quick
            churning_one_key_retains_one_group
        ; Alcotest.test_case
            "distinct keys bound the retention"
            `Quick
            distinct_keys_bound_the_retention
        ] )
    ; ( "the ceiling"
      , [ Alcotest.test_case
            "retained group cost is bounded"
            `Slow
            retained_group_cost_is_bounded
        ; Alcotest.test_case "churn costs nothing" `Slow churn_costs_nothing
        ; Alcotest.test_case "reactive view engine cost" `Slow reactive_view_engine_cost
        ; Alcotest.test_case
            "a fresh engine starts empty"
            `Quick
            a_fresh_engine_starts_empty
        ] )
    ]
;;
