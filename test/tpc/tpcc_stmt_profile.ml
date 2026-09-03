module Lock_stats = Granary_store.Lock_stats

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

(* #718: the writer-lock accounting, when the caller has one to show.  It is
   the same measured interval as everything above, and it is what says how much
   of that interval was spent HOLDING the engine's one global critical section
   rather than merely inside a statement — the split service time cannot see.
   Rendered by [Lock_stats.pp_report] rather than re-laid-out here, so the
   stderr block and any other consumer of a report agree. *)
let lock_block = function
  | None -> ""
  | Some r -> "\n" ^ Format.asprintf "%a" Lock_stats.pp_report r
;;

let report ?lock ~service_ms () =
  match ranked () with
  | [] -> lock_block lock ^ "tpcc: statement profile empty (no statements recorded)\n"
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
    Buffer.add_string buf (lock_block lock);
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
(* #718: three further CSV tables, appended after the two above.

   [clock_installed] is repeated on every site row rather than hoisted into a
   table of its own, because it is what tells a reader whether a column of
   0.000 means "nothing waited" or "nothing was measured" — and a row of this
   file will be read on its own, cut out of context by grep or awk, far more
   often than the file will be read whole.

   The integrity table is separate and is NOT a measurement: both its counters
   are zero on a healthy run and non-zero only when some acquisition of the
   writer lock bypassed the accounting, which would make the two tables above
   quietly incomplete rather than visibly wrong.

   Two caveats a consumer of these rows needs and cannot see in them:

   - [acquisitions] can be 0 while [hold_ms] is not.  The measured window opens
     with [Lock_stats.reset], which keeps an outstanding hold but re-stamps its
     start; if a warm-up [Lwt.async] autocheckpoint was still holding at that
     instant, its acquisition was counted before the window and its hold inside
     it.  Do not compute a mean as [hold_ms / acquisitions] without guarding.
   - [held_at_snapshot] is read after the run returns, at which point the last
     commit's [Lwt.async] autocheckpoint may still be in flight.  When that
     column is non-empty, the named site's hold is TRUNCATED — it never released
     before the snapshot — so its [hold_ms] is a lower bound. *)
let lock_csv_tables line (r : Lock_stats.report) =
  let bool_s b = if b then "true" else "false" in
  line "";
  line
    (Bench_report.Csv.header
       [ "site"
       ; "clock_installed"
       ; "acquisitions"
       ; "contended"
       ; "wait_ms"
       ; "wait_max_ms"
       ; "hold_ms"
       ; "hold_max_ms"
       ]);
  List.iter
    (fun (site, (st : Lock_stats.site_stat)) ->
       line
         (Bench_report.Csv.row
            [ Lock_stats.site_name site
            ; bool_s r.Lock_stats.clock_installed
            ; string_of_int st.Lock_stats.acquisitions
            ; string_of_int st.Lock_stats.contended
            ; Printf.sprintf "%.3f" (1000. *. st.Lock_stats.wait_s)
            ; Printf.sprintf "%.3f" (1000. *. st.Lock_stats.wait_max_s)
            ; Printf.sprintf "%.3f" (1000. *. st.Lock_stats.hold_s)
            ; Printf.sprintf "%.3f" (1000. *. st.Lock_stats.hold_max_s)
            ]))
    r.Lock_stats.sites;
  line "";
  line (Bench_report.Csv.header [ "waiter"; "blocked_by_holder"; "waits"; "wait_ms" ]);
  List.iter
    (fun (b : Lock_stats.blocked_by) ->
       line
         (Bench_report.Csv.row
            [ Lock_stats.site_name b.Lock_stats.waiter
            ; Lock_stats.site_name b.Lock_stats.holder
            ; string_of_int b.Lock_stats.count
            ; Printf.sprintf "%.3f" (1000. *. b.Lock_stats.wait_s)
            ]))
    r.Lock_stats.blocked_by;
  line "";
  line
    (Bench_report.Csv.header
       [ "unattributed_waits"; "unbalanced_releases"; "held_at_snapshot" ]);
  line
    (Bench_report.Csv.row
       [ string_of_int r.Lock_stats.unattributed_waits
       ; string_of_int r.Lock_stats.unbalanced_releases
       ; (match r.Lock_stats.held with
          | None -> ""
          | Some s -> Lock_stats.site_name s)
       ])
;;

let to_csv ?lock ~path ~service_ms () =
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
      (summaries ~service_ms);
    match lock with
    | None -> ()
    | Some r -> lock_csv_tables line r
  in
  match write () with
  | () -> close_out oc
  | exception exn ->
    close_out_noerr oc;
    raise exn
;;
