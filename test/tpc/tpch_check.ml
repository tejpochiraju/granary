(* #482 — cross-checking one query's two answers.  Extracted from bench_tpch so
   that the classification deciding a run's exit code is unit-testable on a
   checkout without `sqlite3` (#502). *)

module BR = Bench_report

type t =
  | Agree
  | Agree_both_empty
  | Mismatch of string
  | Errored
  | Skipped

let label = function
  | Agree -> "ok"
  | Agree_both_empty -> "ok-both-empty"
  | Mismatch _ -> "MISMATCH"
  | Errored -> "error"
  | Skipped -> "skipped"
;;

let pp fmt = function
  | Mismatch report -> Format.fprintf fmt "MISMATCH (%s)" report
  | (Agree | Agree_both_empty | Errored | Skipped) as t ->
    Format.pp_print_string fmt (label t)
;;

let is_failure = function
  | Mismatch _ | Errored -> true
  | Agree | Agree_both_empty | Skipped -> false
;;

(* Written as a float by at least one engine, so the difference may be
   rendering noise rather than a disagreement.  Two *integer* literals that
   differ as text but parse to the same number ('07' vs '7', '+7' vs '7') can
   only come from a TEXT column the engines rendered differently — Q22 groups
   on cntrycode — and that is a real disagreement, not arithmetic noise (#504).
   Restricting the numeric fallback this way keeps it as narrow as its
   justification. *)
let float_form s =
  String.exists
    (function
      | '.' | 'e' | 'E' -> true
      | _ -> false)
    s
;;

let field_eq a b =
  if a = b
  then true
  else if not (float_form a || float_form b)
  then false
  else (
    match float_of_string_opt a, float_of_string_opt b with
    | Some fa, Some fb -> BR.real_eq fa fb
    | _ -> false)
;;

let row_eq ra rb = List.length ra = List.length rb && List.for_all2 field_eq ra rb

(* Q2, Q3, Q10, Q18 and Q21 truncate with LIMIT under an ORDER BY that is not a
   total order — Q10 ties on revenue alone, Q18 on (o_totalprice, o_orderdate).
   Two engines may legitimately order tied rows differently, so for these the
   comparison is on the multiset of rows rather than the sequence.  Every other
   query keeps sequence comparison, where a wrong row order IS a defect.

   Residual limit, stated rather than hidden: if the tie spans the LIMIT cut, the
   engines may return genuinely different rows and this still reports MISMATCH.
   That is the correct default — it is indistinguishable from a real disagreement
   without re-deriving the query's tie-break semantics. *)
let tie_prone = [ 2; 3; 10; 18; 21 ]
let unordered_compare number = List.mem number tie_prone

let rec take_matching ra acc = function
  | [] -> None
  | rb :: tl ->
    if row_eq ra rb
    then Some (List.rev_append acc tl)
    else take_matching ra (rb :: acc) tl
;;

(* Greedy multiset match; result is the rows of [a] with no partner in [b]. *)
let multiset_unmatched a b =
  let remaining = ref b in
  let step acc ra =
    match take_matching ra [] !remaining with
    | Some rest ->
      remaining := rest;
      acc
    | None -> ra :: acc
  in
  List.rev (List.fold_left step [] a)
;;

let rec first_seq_diff i a b =
  match a, b with
  | [], [] -> None
  | ra :: ta, rb :: tb ->
    if row_eq ra rb then first_seq_diff (i + 1) ta tb else Some (i, Some ra, Some rb)
  | ra :: _, [] -> Some (i, Some ra, None)
  | [], rb :: _ -> Some (i, None, Some rb)
;;

let compare_rows ~unordered granary_rows sqlite_rows =
  let show r =
    match r with
    | None -> "<missing>"
    | Some row -> String.concat " | " row
  in
  let counts =
    Printf.sprintf
      "granary %d rows, sqlite %d rows"
      (List.length granary_rows)
      (List.length sqlite_rows)
  in
  if unordered
  then (
    let only_g = multiset_unmatched granary_rows sqlite_rows in
    let only_s = multiset_unmatched sqlite_rows granary_rows in
    match only_g, only_s with
    | [], [] when List.length granary_rows = List.length sqlite_rows -> None
    | g, s ->
      Some
        (Printf.sprintf
           "%s; unmatched (multiset compare): granary-only %s / sqlite-only %s"
           counts
           (show (List.nth_opt g 0))
           (show (List.nth_opt s 0))))
  else (
    match first_seq_diff 0 granary_rows sqlite_rows with
    | None -> None
    | Some (i, g, s) ->
      Some
        (Printf.sprintf "%s; row %d: granary %s / sqlite %s" counts i (show g) (show s)))
;;

(* Two empty answers compare equal, and that agreement carries no information:
   at SF 0.001 Q2 and Q20 read "ok" for two rounds of review and were wrong at
   SF 0.01.  So an all-empty agreement gets its own token — the false-pass class
   is then visible in the artifact instead of depending on a reader noticing.

   No rows at all is either a deliberate skip or a failure, and which one is not
   a property of the rows: a query the catalogue asserts runs and that stops
   running is a regression, and rendering it as `skipped` hid it inside the 12
   queries that never ran (#502). *)
let classify ~number ~runnable ~granary ~sqlite =
  match granary, sqlite with
  | Some gr, Some sr ->
    (match compare_rows ~unordered:(unordered_compare number) gr sr with
     | None -> if gr = [] && sr = [] then Agree_both_empty else Agree
     | Some report -> Mismatch report)
  | _ -> if runnable then Errored else Skipped
;;
