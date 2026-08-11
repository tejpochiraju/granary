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
  let key (e : entry) = -.cost_of e.profile, e.profile, -.e.total_ms, e.sql in
  List.sort (fun a b -> compare (key a) (key b)) entries
;;

(* [record] keys on the raw SQL, which is right for a prepared shape but wrong
   for a *generated* one: {!Tpcc_txn.stock_select_sql} interpolates the district
   into a COLUMN NAME ([s_dist_%02d]), so one logical statement arrives as ten
   distinct keys and {!ranked} shows it as ten small rows instead of one large
   one.  In the published run that is the NewOrder stock read — really 326.7 ms
   over 3264 calls, 5.85% of the profile and 4th overall, rendered as ten rows
   of 0.39-0.79%.  Nothing about that is visible from the table, which is the
   hazard: a reader ranks by row and the shape is silently understated by its
   fan-out factor.

   [shape_of_sql] collapses every maximal run of digits to a single [#], and
   {!families} groups the entries that then coincide.  Grouping is only ever
   REPORTED, never folded back into {!ranked} or {!to_csv}: the per-key rows are
   the measurement and a rollup is an interpretation of it, and collapsing
   digits is a heuristic — two statements differing only in a numeric literal
   are genuinely distinct shapes to the planner even though this rule merges
   them.  A family of one is not reported at all, so the ordinary case where the
   heuristic merges nothing costs nothing. *)
let shape_of_sql sql =
  let b = Buffer.create (String.length sql) in
  let in_digits = ref false in
  let add c =
    let is_digit = c >= '0' && c <= '9' in
    if not is_digit
    then Buffer.add_char b c
    else if not !in_digits
    then Buffer.add_char b '#';
    in_digits := is_digit
  in
  String.iter add sql;
  Buffer.contents b
;;

type family =
  { profile : string
  ; shape : string
  ; members : int
  ; calls : int
  ; rows : int
  ; total_ms : float
  ; pct_of_profile : float
  }

let families () =
  let totals = profile_totals () in
  let cost_of p = Option.value (Hashtbl.find_opt totals p) ~default:0.0 in
  let h : (string * string, family) Hashtbl.t = Hashtbl.create 8 in
  let add (e : entry) =
    let key = e.profile, shape_of_sql e.sql in
    let prev =
      Option.value
        (Hashtbl.find_opt h key)
        ~default:
          { profile = e.profile
          ; shape = snd key
          ; members = 0
          ; calls = 0
          ; rows = 0
          ; total_ms = 0.0
          ; pct_of_profile = 0.0
          }
    in
    Hashtbl.replace
      h
      key
      { prev with
        members = prev.members + 1
      ; calls = prev.calls + e.calls
      ; rows = prev.rows + e.rows
      ; total_ms = prev.total_ms +. e.total_ms
      }
  in
  List.iter add (ranked ());
  Hashtbl.fold (fun _ f acc -> f :: acc) h []
  |> List.filter (fun f -> f.members > 1)
  |> List.map (fun f ->
    let denom = 1000.0 *. cost_of f.profile in
    { f with
      pct_of_profile = (if denom > 0.0 then 100.0 *. f.total_ms /. denom else 0.0)
    })
  |> List.sort (fun a b ->
    compare
      (-.cost_of a.profile, a.profile, -.a.total_ms, a.shape)
      (-.cost_of b.profile, b.profile, -.b.total_ms, b.shape))
;;

let summaries ~service_ms =
  let totals = profile_totals () in
  Hashtbl.fold (fun profile secs acc -> (profile, secs) :: acc) totals []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  |> List.map (fun (profile, secs) ->
    let statements_total_ms = 1000.0 *. secs in
    let driver_service_ms =
      Option.value (List.assoc_opt profile service_ms) ~default:0.0
    in
    { profile
    ; statements_total_ms
    ; driver_service_ms
    ; attributed_pct =
        (if driver_service_ms > 0.0
         then 100.0 *. statements_total_ms /. driver_service_ms
         else if statements_total_ms > 0.0
         then Float.infinity
         else 0.0)
    })
;;

(* [elide] used to live here, keeping a head and a tail of the SQL around a
   fixed "..." marker so a long statement did not destroy the stderr table's
   column alignment. It went through two revisions and neither was correct:
   a front-truncating elide collapsed statements sharing a long prefix into
   one row, and the head/tail revision fixed that case but not the one that
   matters most — payment's two customer-update shapes
   (test/tpc/tpcc_txn.ml:748,753) share both a long prefix AND a 44-character
   suffix, and differ only in a clause sitting in the middle, which a
   fixed-offset window discards regardless of where the head/tail boundary
   falls. No fixed-offset elide can fix this, because the discriminator's
   position is a property of the statements being rendered, not of the
   window — so [elide] is gone rather than re-tuned. The SQL is printed in
   full below: it is the table's LAST column, so a long statement wraps onto
   its own line and every numeric column above and below it stays aligned.
   The compactness elision bought was never worth a rendering that cannot
   tell two statements apart. *)

let report ~service_ms =
  match ranked () with
  | [] -> "tpcc: statement profile empty (no statements recorded)\n"
  | entries ->
    let buf = Buffer.create 4096 in
    Buffer.add_string
      buf
      "\n\
       tpcc per-statement profile — service time (in-lock share unmeasured); read only \
       at TERMINALS=1\n";
    List.iter
      (fun (e : entry) ->
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
              e.sql))
      entries;
    (match families () with
     | [] -> ()
     | fams ->
       Buffer.add_string
         buf
         "\n\
          generated-SQL families — one logical statement split across several keys by an \
          embedded number; each row is the rollup, and its members are already listed \
          above\n";
       List.iter
         (fun (f : family) ->
            Buffer.add_string
              buf
              (Printf.sprintf
                 "  %-12s %9.1f ms %5.1f%% %6d calls %9d rows %6d keys  %s\n"
                 f.profile
                 f.total_ms
                 f.pct_of_profile
                 f.calls
                 f.rows
                 f.members
                 f.shape))
         fams);
    Buffer.add_string buf "\ncoverage — summed statement time vs driver service time\n";
    List.iter
      (fun (s : summary) ->
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

(* Deliberately not [Fun.protect ~finally:(fun () -> close_out oc)]: buffered
   [output_string] means the realistic failure (a full filesystem or a
   tripped quota) surfaces from [close_out]'s flush, and [Fun.protect] wraps
   any exception its finaliser raises in [Fun.Finally_raised], which does not
   match a caller's [with Sys_error _] and escapes it whole (#715 review).
   Closing explicitly on both the success and failure path keeps every
   exception this function can raise a plain, un-wrapped [Sys_error]. *)
let to_csv ~path ~service_ms =
  let oc = open_out path in
  let write () =
    let line s = output_string oc (s ^ "\n") in
    line
      (Bench_report.Csv.header
         [ "profile"; "sql"; "calls"; "rows"; "total_ms"; "mean_ms"; "pct_of_profile" ]);
    List.iter
      (fun (e : entry) ->
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
      (fun (s : summary) ->
         line
           (Bench_report.Csv.row
              [ s.profile
              ; Printf.sprintf "%.3f" s.statements_total_ms
              ; Printf.sprintf "%.3f" s.driver_service_ms
              ; Printf.sprintf "%.2f" s.attributed_pct
              ]))
      (summaries ~service_ms)
  in
  match write () with
  | () -> close_out oc
  | exception exn ->
    close_out_noerr oc;
    raise exn
;;
