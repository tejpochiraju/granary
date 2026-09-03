(* #718: writer-lock wait/hold accounting, attributed by acquisition site.

   See lock_stats.mli for what this is for and what it can and cannot say.  The
   two properties worth restating next to the code:

   - The contention counters ([contended], the [blocked_by] matrix) are derived
     from [Rwlock.writer_active], not from elapsed time, so they are exact and
     survive a build with no clock installed.
   - A wait is attributed to the holder observed when the wait BEGAN.  Rwlock
     wakes every waiter on release and lets the scheduler pick, so there is no
     queue position to read; a wait spanning several holders lands wholly on the
     first.  Documented approximation, deliberate.

   No Lwt here on purpose: every entry point is synchronous, so an acquisition
   and its accounting cannot be separated by a scheduler yield. *)

type site =
  | Txn
  | Checkpoint
  | Autocheckpoint
  | Commit_sink

let all_sites = [ Txn; Checkpoint; Autocheckpoint; Commit_sink ]

let site_name = function
  | Txn -> "txn"
  | Checkpoint -> "checkpoint"
  | Autocheckpoint -> "autocheckpoint"
  | Commit_sink -> "commit_sink"
;;

(* Dense index into [accs] and [matrix].  [n_sites] must equal
   [List.length all_sites]; [site_index_is_total] in the tests pins that, since
   a new constructor added without widening the arrays would raise
   [Index_out_of_bounds] only on the first acquisition from the new site. *)
let site_index = function
  | Txn -> 0
  | Checkpoint -> 1
  | Autocheckpoint -> 2
  | Commit_sink -> 3
;;

let n_sites = 4

type site_stat =
  { acquisitions : int
  ; contended : int
  ; wait_s : float
  ; wait_max_s : float
  ; hold_s : float
  ; hold_max_s : float
  }

type blocked_by =
  { waiter : site
  ; holder : site
  ; count : int
  ; wait_s : float
  }

type report =
  { clock_installed : bool
  ; sites : (site * site_stat) list
  ; blocked_by : blocked_by list
  ; held : site option
  ; unattributed_waits : int
  ; unbalanced_releases : int
  }

(* Mutable mirrors of [site_stat] / [blocked_by]: the accumulator is written on
   every lock operation on the write path, so it updates fields in place rather
   than rebuilding a record.  [report] converts to the immutable shapes. *)
type acc =
  { mutable a_acquisitions : int
  ; mutable a_contended : int
  ; mutable a_wait_s : float
  ; mutable a_wait_max_s : float
  ; mutable a_hold_s : float
  ; mutable a_hold_max_s : float
  }

type cell =
  { mutable c_count : int
  ; mutable c_wait_s : float
  }

type t =
  { mutable clock : (unit -> float) option
  ; accs : acc array
  ; matrix : cell array (* [waiter * n_sites + holder] *)
  ; mutable current : (site * float) option (* holder, and when it acquired *)
  ; mutable unattributed_waits : int
  ; mutable unbalanced_releases : int
  }

let fresh_acc () =
  { a_acquisitions = 0
  ; a_contended = 0
  ; a_wait_s = 0.
  ; a_wait_max_s = 0.
  ; a_hold_s = 0.
  ; a_hold_max_s = 0.
  }
;;

let create () =
  { clock = None
  ; accs = Array.init n_sites (fun _ -> fresh_acc ())
  ; matrix = Array.init (n_sites * n_sites) (fun _ -> { c_count = 0; c_wait_s = 0. })
  ; current = None
  ; unattributed_waits = 0
  ; unbalanced_releases = 0
  }
;;

let set_clock t c = t.clock <- Some c

let now t =
  match t.clock with
  | None -> 0.
  | Some c -> c ()
;;

let held t =
  match t.current with
  | None -> None
  | Some (s, _) -> Some s
;;

let note_acquired t site ~waited ~contended ~blocked_by =
  let a = t.accs.(site_index site) in
  a.a_acquisitions <- a.a_acquisitions + 1;
  a.a_wait_s <- a.a_wait_s +. waited;
  if waited > a.a_wait_max_s then a.a_wait_max_s <- waited;
  if contended
  then (
    a.a_contended <- a.a_contended + 1;
    match blocked_by with
    (* Contended with no recorded holder: some [Rwlock.acquire_write] did not go
       through this accumulator.  Counted, never guessed — an invented holder
       would be indistinguishable from a measured one in the report. *)
    | None -> t.unattributed_waits <- t.unattributed_waits + 1
    | Some holder ->
      let c = t.matrix.((site_index site * n_sites) + site_index holder) in
      c.c_count <- c.c_count + 1;
      c.c_wait_s <- c.c_wait_s +. waited);
  t.current <- Some (site, now t)
;;

let note_released t ~at =
  match t.current with
  | None -> t.unbalanced_releases <- t.unbalanced_releases + 1
  | Some (site, acquired_at) ->
    t.current <- None;
    let a = t.accs.(site_index site) in
    (* [at -. acquired_at] is non-negative for any sane clock; a clock stepped
       backwards (NTP) would otherwise subtract from the total and could drive
       it negative, which reads as a measurement bug rather than a clock one. *)
    let holdt = Float.max 0. (at -. acquired_at) in
    a.a_hold_s <- a.a_hold_s +. holdt;
    if holdt > a.a_hold_max_s then a.a_hold_max_s <- holdt
;;

let reset t =
  Array.iteri (fun i _ -> t.accs.(i) <- fresh_acc ()) t.accs;
  Array.iter
    (fun c ->
       c.c_count <- 0;
       c.c_wait_s <- 0.)
    t.matrix;
  t.unattributed_waits <- 0;
  t.unbalanced_releases <- 0;
  (* Keep the outstanding hold but re-stamp its start: see the .mli.  Dropping
     it would make the next release unbalanced; keeping the old start would
     charge the measured window for pre-reset time. *)
  match t.current with
  | None -> ()
  | Some (site, _) -> t.current <- Some (site, now t)
;;

let report t =
  let stat_of (a : acc) =
    { acquisitions = a.a_acquisitions
    ; contended = a.a_contended
    ; wait_s = a.a_wait_s
    ; wait_max_s = a.a_wait_max_s
    ; hold_s = a.a_hold_s
    ; hold_max_s = a.a_hold_max_s
    }
  in
  let cells =
    List.concat_map
      (fun waiter ->
         List.filter_map
           (fun holder ->
              let c = t.matrix.((site_index waiter * n_sites) + site_index holder) in
              if c.c_count = 0
              then None
              else Some { waiter; holder; count = c.c_count; wait_s = c.c_wait_s })
           all_sites)
      all_sites
  in
  (* Costliest wait first; count, then the site names, break ties, so the order
     is total and a CSV does not move between runs that measured the same
     thing. *)
  let key b = -.b.wait_s, -b.count, site_name b.waiter, site_name b.holder in
  let cells = List.sort (fun a b -> compare (key a) (key b)) cells in
  { clock_installed = t.clock <> None
  ; sites = List.map (fun s -> s, stat_of t.accs.(site_index s)) all_sites
  ; blocked_by = cells
  ; held = held t
  ; unattributed_waits = t.unattributed_waits
  ; unbalanced_releases = t.unbalanced_releases
  }
;;

let pp fmt t =
  let r = report t in
  Format.fprintf
    fmt
    "Lock_stats.t { clock = %s; held = %s; acquisitions = %d }"
    (if r.clock_installed then "installed" else "none")
    (match r.held with
     | None -> "-"
     | Some s -> site_name s)
    (List.fold_left (fun n (_, s) -> n + s.acquisitions) 0 r.sites)
;;

let pp_report fmt (r : report) =
  Format.fprintf fmt "@[<v>writer-lock accounting (#718)@,";
  if not r.clock_installed
  then
    Format.fprintf
      fmt
      "  NO CLOCK INSTALLED — every duration below is 0.000 because nothing measured \
       them,@,\
      \  not because nothing waited.  The counts are still exact.@,";
  Format.fprintf
    fmt
    "  %-15s %8s %8s %10s %10s %10s %10s@,"
    "site"
    "acquires"
    "contend"
    "wait_ms"
    "wait_max"
    "hold_ms"
    "hold_max";
  List.iter
    (fun (s, st) ->
       Format.fprintf
         fmt
         "  %-15s %8d %8d %10.3f %10.3f %10.3f %10.3f@,"
         (site_name s)
         st.acquisitions
         st.contended
         (1000. *. st.wait_s)
         (1000. *. st.wait_max_s)
         (1000. *. st.hold_s)
         (1000. *. st.hold_max_s))
    r.sites;
  (match r.blocked_by with
   | [] -> Format.fprintf fmt "  no contended acquisition recorded@,"
   | cells ->
     Format.fprintf fmt "  waits, by who held the lock when the wait began:@,";
     List.iter
       (fun b ->
          Format.fprintf
            fmt
            "    %-15s blocked by %-15s %6d x  %10.3f ms@,"
            (site_name b.waiter)
            (site_name b.holder)
            b.count
            (1000. *. b.wait_s))
       cells);
  if r.unattributed_waits > 0
  then
    Format.fprintf
      fmt
      "  BUG: %d contended wait(s) had no recorded holder — an acquire_write bypassed \
       this accumulator@,"
      r.unattributed_waits;
  if r.unbalanced_releases > 0
  then
    Format.fprintf
      fmt
      "  BUG: %d release(s) with no matching acquisition@,"
      r.unbalanced_releases;
  Format.fprintf fmt "@]@."
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-5"]
[@@@ai_provider "Anthropic"]
