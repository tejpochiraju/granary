open Lwt.Syntax
module S = Granary_store.Store
module Cat = Granary_catalog.Catalog
module Row = Granary_encoding.Row
module Rowid = Granary_encoding.Rowid
module Index_key = Granary_encoding.Index_key
module Varint = Granary_encoding.Varint

(* #240: the set of user tables a write statement actually mutated, accumulated
   for an external read cache.  Like [query_stats] it rides Lwt
   sequence-associated storage so it need not be threaded through the DML and
   recursive cascade/trigger paths: [with_dirty] installs a fresh accumulator
   for the statement; every physical-mutation site calls [mark_dirty], a no-op
   when no accumulator is installed (plain [execute]/[run] callers pay nothing —
   a single predicted branch off the hot path, never per-row in the common case).
   Defined here, ahead of the DML functions, so every [mark_dirty] call site can
   reach it (the mutation sites span [execute_insert] onward).

   #417 Phase 0: an accumulator may ALSO capture the row-level delta — the rowid
   and old/new {!Row.t} of every mutated row — for an incremental view-maintenance
   consumer (DBSP-style).  This is opt-in: [make_change_acc] enables capture,
   [make_dirty_acc] does not.  The name-set path ([mark_dirty]) is byte-for-byte
   unchanged whether or not capture is on, so plain #240 callers pay nothing; the
   row-level [record_change] is a single predicted branch (returns immediately
   when [changes = None] or no accumulator is installed). *)
type row_change =
  | Inserted of
      { rowid : int64
      ; row : Row.t
      }
  | Deleted of
      { rowid : int64
      ; row : Row.t
      }
  | Updated of
      { rowid : int64
      ; old_row : Row.t
      ; new_row : Row.t
      }

type dirty_tables_acc =
  { names : (string, unit) Hashtbl.t
  ; changes : (string, row_change list ref) Hashtbl.t option
    (* [Some] => row-level capture is on; per-table lists held in reverse
       (newest-first) and reversed on drain by [dirty_changes]. *)
  }

let make_dirty_acc () : dirty_tables_acc = { names = Hashtbl.create 8; changes = None }

let make_change_acc () : dirty_tables_acc =
  { names = Hashtbl.create 8; changes = Some (Hashtbl.create 8) }
;;

let dirty_tables_key : dirty_tables_acc Lwt.key = Lwt.new_key ()

(* #514: instrumentation for the DML seek path.  [dss_candidates]/[dss_fetched]
   make the seek's shape observable without timing it — a full-key seek that
   walks one entry and reads one row has not degraded into a scan, whatever the
   clock says (#512).  [dss_peak_buffered] is the candidate BACKLOG: rowids the
   index walk had produced but the row fetch had not yet consumed, at its worst
   moment.  It equals the match count, because the drain deliberately sorts all
   candidates before fetching any (see [rowid_buf]); it is a shape assertion
   against a fetch-as-you-walk regression, which would drive it to one and cost
   a page read per row on disk.  Rides Lwt
   sequence-associated storage like [query_stats]/[dirty_tables_acc], so a caller
   that installs no accumulator pays one predicted branch per candidate and the
   DML path needs no extra parameter. *)
type dml_seek_stats =
  { mutable dss_candidates : int
  ; mutable dss_fetched : int
  ; mutable dss_peak_buffered : int
  }

let make_dml_seek_stats () =
  { dss_candidates = 0; dss_fetched = 0; dss_peak_buffered = 0 }
;;

let dml_seek_stats_key : dml_seek_stats Lwt.key = Lwt.new_key ()
let with_dml_seek_stats st f = Lwt.with_value dml_seek_stats_key (Some st) f

(* Record that the index walk produced one more candidate rowid, updating the
   high-water mark of walked-but-not-yet-fetched candidates.  Takes the
   already-resolved accumulator rather than reading the Lwt key itself: the DML
   drain resolves it once per statement, exactly as the read path does with
   [query_stats] (see [incr_examined]), so the seek #512 made hot pays no
   per-row key lookup. *)
let note_seek_candidate (st_opt : dml_seek_stats option) =
  match st_opt with
  | None -> ()
  | Some st ->
    st.dss_candidates <- st.dss_candidates + 1;
    let backlog = st.dss_candidates - st.dss_fetched in
    if backlog > st.dss_peak_buffered then st.dss_peak_buffered <- backlog
;;

(* Record that one candidate's row was looked up in the table tree. *)
let note_seek_fetched (st_opt : dml_seek_stats option) =
  match st_opt with
  | None -> ()
  | Some st -> st.dss_fetched <- st.dss_fetched + 1
;;

(* Reserved-prefix internal tables — the synthesized [sqlite_…] objects
   (sqlite_master / sqlite_sequence) — are never reported: an external cache only
   invalidates user tables.  [sqlite_] is the SOLE prefix [sema] forbids to user
   objects ([reject_reserved_name], sema.ml), so filtering exactly it is
   false-positive-free.  The engine's own [_sys_…] catalog trees are NOT matched:
   they are reached by fixed tree-id, never by name through a mark site, so they
   cannot appear here — whereas [_sys_]/[sys_] ARE legal user-table names (sema
   permits them), and a user table named e.g. [sys_audit] must still be reported. *)
let is_internal_table_name (name : string) = String.starts_with ~prefix:"sqlite_" name

(* Record that [name]'s rows changed in the current statement. *)
let mark_dirty (name : string) =
  match Lwt.get dirty_tables_key with
  | None -> ()
  | Some { names; _ } -> Hashtbl.replace names name ()
;;

(* #417: record one row-level [change] against [table].  A no-op (one branch)
   unless an accumulator with capture enabled is installed.  Also marks [table]
   in the name set so a captured change can never reference a table missing from
   {!dirty_elements} (callers at the per-row loop sites need not also call
   [mark_dirty]). *)
let record_change (table : string) (change : row_change) =
  match Lwt.get dirty_tables_key with
  | None | Some { changes = None; _ } -> ()
  | Some { names; changes = Some log } ->
    Hashtbl.replace names table ();
    (match Hashtbl.find_opt log table with
     | Some r -> r := change :: !r
     | None -> Hashtbl.add log table (ref [ change ]))
;;

(* #417: record an UPDATE whose row may have been re-keyed.  When the rowid is
   unchanged it is one [Updated]; when an UPDATE of the INTEGER-PK alias MOVED the
   row to a new rowid (#243/#249), the identity changed, so it is modelled as a
   [Deleted] of the old rowid followed by an [Inserted] at the new one — the same
   shape REPLACE-displacement emits, and what a Z-set/IVM consumer needs to track
   the move (it keys deltas by rowid). *)
let record_update (table : string) ~old_rowid ~new_rowid ~old_row ~new_row =
  if Int64.equal old_rowid new_rowid
  then record_change table (Updated { rowid = old_rowid; old_row; new_row })
  else (
    record_change table (Deleted { rowid = old_rowid; row = old_row });
    record_change table (Inserted { rowid = new_rowid; row = new_row }))
;;

(* Install [acc] as the active write-path mutation sink for [f]'s dynamic extent
   (propagated across binds, so nested cascade/trigger writes record into it). *)
let with_dirty (acc : dirty_tables_acc) (f : unit -> 'a Lwt.t) : 'a Lwt.t =
  Lwt.with_value dirty_tables_key (Some acc) f
;;

let current_dirty_acc () : dirty_tables_acc option = Lwt.get dirty_tables_key

(* Drain to the public shape: user tables only, deduplicated, sorted. *)
let dirty_elements ({ names; _ } : dirty_tables_acc) : string list =
  Hashtbl.fold
    (fun k () acc -> if is_internal_table_name k then acc else k :: acc)
    names
    []
  |> List.sort_uniq String.compare
;;

(* #417: drain the per-table row-level deltas: user tables only, sorted by name,
   each table's changes in the order they were applied.  Empty for a
   non-capturing accumulator. *)
let dirty_changes ({ changes; _ } : dirty_tables_acc) : (string * row_change list) list =
  match changes with
  | None -> []
  | Some log ->
    Hashtbl.fold
      (fun k r acc -> if is_internal_table_name k then acc else (k, List.rev !r) :: acc)
      log
      []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
;;

(* #666: a mark/restore point for the #417 row-level delta log, so a write the
   store UNDOES cannot leave a delta describing it behind.

   Rollback used to revert two of the three pieces of per-statement state — the
   [Store] trees and the [Schema_cache] — and not the ambient accumulator here.
   The two halves of the accumulator are not equally dangerous and are treated
   differently on purpose:

   - the #240 NAME set is an invalidation HINT.  A stale entry costs an external
     cache one miss, and is deliberately NOT reverted: over-invalidation is
     free, whereas under-invalidation (a mark site this restore failed to
     account for) is a stale-cache wrong answer.  Note [record_change] marks the
     name as well as recording the delta, so restoring the delta and keeping the
     name lands on exactly that safe side.
   - the #417 delta log is a statement of FACT about rows.  A stale [Inserted]
     is a PHANTOM ROW in a materialised reactive view ([Db.drive_reactive]
     absorbs it and the maintenance applies it), which is a wrong answer, not a
     cost.  So it is reverted.

   The log is prepend-only per table and its per-table [ref] is created once and
   never replaced, so a mark is exactly each table's current list — which later
   prepends leave as the tail — and a restore is an assignment back to it plus
   the removal of tables that did not exist at mark time.  That makes both
   operations O(tables touched), never O(rows): [execute_insert] takes one mark
   per ROW, so anything proportional to the deltas already recorded would make a
   multi-row INSERT quadratic. *)
type changes_mark =
  | Cm_none
  | Cm_marked of
      { log : (string, row_change list ref) Hashtbl.t
      ; saved : (string * row_change list) list
      }

(* Capture the delta log's current tails.  [Cm_none] when no accumulator is
   installed or capture is off — the common case, and one predicted branch. *)
let changes_mark () : changes_mark =
  match Lwt.get dirty_tables_key with
  | None | Some { changes = None; _ } -> Cm_none
  | Some { changes = Some log; _ } ->
    Cm_marked { log; saved = Hashtbl.fold (fun k r acc -> (k, !r) :: acc) log [] }
;;

(* Discard every delta recorded since [m] was taken.  Call it wherever the STORE
   is reverted (an autocommit [S.rollback] of a skipped row, or #631's
   statement savepoint) and NOT where effects survive — a raising statement in a
   borrowed transaction keeps its partial writes, so it must keep their deltas.

   Two passes, each O(tables): drop the tables that did not exist at mark time,
   then put every table that did back to its marked tail.  The second pass
   re-adds a missing entry rather than assuming one is there.  That case is
   believed unreachable — only a restore removes an entry, and it removes only
   tables absent at ITS OWN mark, which (marks nest LIFO) can never include a
   table present at an enclosing mark — but the invariant is subtle enough that
   depending on it silently would be the wrong trade for one [Hashtbl.replace]. *)
let changes_restore (m : changes_mark) : unit =
  match m with
  | Cm_none -> ()
  | Cm_marked { log; saved } ->
    Hashtbl.fold (fun k _ acc -> k :: acc) log []
    |> List.iter (fun k -> if not (List.mem_assoc k saved) then Hashtbl.remove log k);
    List.iter
      (fun (k, tail) ->
         match Hashtbl.find_opt log k with
         | Some r -> r := tail
         | None -> Hashtbl.replace log k (ref tail))
      saved
;;

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

(* #247: kill-switch for the cursor-level aggregate fast path.  Defaults on;
   [GRANARY_AGG_FASTPATH=0] forces the general [stream_aggregate] path (a safety
   valve, and the foil the Gc-gate regression test compares against).  Read
   per-call — an aggregate runs it once per query, never in a tight loop. *)
let agg_fastpath_enabled () =
  match Sys.getenv_opt "GRANARY_AGG_FASTPATH" with
  | Some ("0" | "false" | "off") -> false
  | _ -> true
;;

let lit_to_value : Ast.literal -> Row.value = function
  | Ast.L_int n -> Row.V_int n
  | Ast.L_text s -> Row.V_text s
  | Ast.L_null -> Row.V_null
  | Ast.L_real f -> Row.V_real f
  | Ast.L_blob b -> Row.V_blob b
  | Ast.L_current_timestamp | Ast.L_current_date | Ast.L_current_time ->
    failwith "lit_to_value: CURRENT_* should not appear as a plan literal"
;;

let value_to_literal : Row.value -> Ast.literal = function
  | Row.V_int n -> Ast.L_int n
  | Row.V_text s -> Ast.L_text s
  | Row.V_real f -> Ast.L_real f
  | Row.V_blob b -> Ast.L_blob b
  | Row.V_null -> Ast.L_null
;;

let row_value_to_index_value : Row.value -> Index_key.value = function
  | Row.V_int n -> Index_key.IK_int n
  | Row.V_text s -> Index_key.IK_text s
  | Row.V_null -> Index_key.IK_null
  | Row.V_real f -> Index_key.IK_real f
  | Row.V_blob b -> Index_key.IK_blob b
;;

(* #579: the STORAGE CLASS a value belongs to, for cross-class ordering.
   INTEGER and REAL share a class because they are compared numerically, not by
   representation — see [compare_values].

   The ranks match [Index_key.encode_value]'s tag bytes (NULL [0x00], NaN
   [0x01], INTEGER [0x02] / REAL [0x03], TEXT [0x04], BLOB [0x05]) and
   SQLite's documented storage-class order, so a value-level comparison and an
   index-key comparison cannot disagree about which CLASS sorts first.  They
   still disagree about INTEGER vs REAL *within* the numeric class, because the
   encoding gives them separate tags and therefore puts every integer before
   every real — but that disagreement is pre-existing.

   #733/#734: [cmp_result], the WHERE-predicate comparator, now delegates to
   [compare_values] outright, so this rank order is the one the ordering
   operators apply too.  It briefly was not — #579 gave [compare_values] a
   class order while [cmp_result] still answered false for every cross-class
   predicate — and that gap is what those two issues closed. *)
let value_class_rank (v : Row.value) : int =
  match v with
  | Row.V_null -> 0
  | Row.V_int _ | Row.V_real _ -> 1
  | Row.V_text _ -> 2
  | Row.V_blob _ -> 3
;;

(* 2^63 as a float.  Every float [>= this] is out of int64 range; [-.this] is
   exactly [Int64.min_int] and therefore IS in range.  Duplicated from
   [two_pow_63] below, which is defined much later in this file for
   [range_bound_key]. *)
let two_pow_63_cmp = 9.2233720368547758e18

(* #579: compare an int64 with a float EXACTLY, without promoting the int64 to
   a float.

   [Int64.to_float] rounds to nearest, so above 2^53 two distinct int64s can
   promote to the same float — and a comparator that answers 0 for both pairs
   is non-transitive:

     9007199254740993L  vs 9007199254740992.0  ->  0   (promoted: equal)
     9007199254740992.0 vs 9007199254740992L   ->  0   (promoted: equal)
     9007199254740993L  vs 9007199254740992L   ->  1   (exact)

   which is the same defect #579 is about, one magnitude up: [List.sort] on a
   non-transitive comparator has no defined result.  Comparing exactly is also
   what SQLite does ([sqlite3IntFloatCompare]).

   NaN is ordered by #536's decided rule — below every number — rather than by
   [Float.compare]'s accident, so the two agree by construction.  Infinities
   and out-of-range floats are decided by sign before any conversion. *)
let cmp_int_real (x : int64) (y : float) : int =
  if Float.is_nan y
  then 1 (* #536: NaN sorts below every number, so [x] is greater. *)
  else if y >= two_pow_63_cmp
  then -1 (* includes [infinity] *)
  else if y < -.two_pow_63_cmp
  then 1 (* includes [neg_infinity] *)
  else (
    (* [y] is finite and within int64 range, so [Float.trunc] is exact and
       [Int64.of_float] of it is lossless. *)
    let ty = Float.trunc y in
    let c = Int64.compare x (Int64.of_float ty) in
    if c <> 0
    then c
    else if y > ty
    then -1 (* [y] has a positive fraction: x < y *)
    else if y < ty
    then 1 (* [y] has a negative fraction: x > y *)
    else 0)
;;

(* #738: the int64 a REAL is exactly equal to, or [None] when no integer is.

   Declines a NaN, an infinity, anything outside int64 range (where
   [Int64.of_float] is unspecified) and any value with a fraction.  Every
   declined case is a genuine "no integer equals this", which is what lets an
   equality seek turn [None] into an empty result rather than a wider scan. *)
let int64_of_exact_real (f : float) : int64 option =
  if Float.is_nan f || f >= two_pow_63_cmp || f < -.two_pow_63_cmp
  then None
  else if Float.equal f (Float.trunc f)
  then Some (Int64.of_float f)
  else None
;;

(* #738: the float an int64 is exactly equal to, or [None] when no REAL is.

   [Int64.to_float] rounds to nearest, so above 2^53 the result names a
   DIFFERENT integer; {!cmp_int_real} is the exact test for whether it landed on
   [n] itself, and reusing it is what keeps this helper and the [=] predicate
   from ever disagreeing. *)
let exact_real_of_int64 (n : int64) : float option =
  let f = Int64.to_float n in
  if cmp_int_real n f = 0 then Some f else None
;;

(* #743: the CANONICAL equality key of a join-key value.

   A hash join has no index and no column type to translate towards — both
   sides are just row values — so it needs a key that is equal for exactly the
   values {!compare_values} calls equal.  Keying on the raw
   [Index_key.encode_value] bytes is not that: [1] and [1.0] are different
   bytes, so [FROM l JOIN r ON l.a = r.b] matched nothing across an INTEGER and
   a REAL column while the same equality written as a WHERE filter matched
   (#738 made [=] exact at the value level, and a join key is CONSUMED — the
   key IS the match test, nothing re-checks the pairs it yields).

   The rule: an integral REAL keys as the INTEGER it names, via the same
   {!int64_of_exact_real} the index sites use.  That makes canonical-key
   equality {b exactly} [compare_values … = 0] on non-NULL values, in every
   direction:

   - [1] and [1.0] canonicalise to the same [IK_int] and join;
   - a non-integral REAL stays [IK_real] (tag [0x03]), which no [IK_int] (tag
     [0x02]) can collide with, so no integer joins [1.5];
   - above 2^53 the two directions stay apart, because
     [int64_of_exact_real 9007199254740992.0] is [9007199254740992L] and not
     the [9007199254740993L] it must not equal — this must never be spelled as
     an [Int64.to_float] promotion, which would be #733 inside a join key;
   - a NaN is declined by {!int64_of_exact_real} FIRST, so it can never reach
     [Int64.of_float] as an "integral" real.  It stays [IK_real nan], whose
     encoding is the single byte [0x01] (#578) — so all NaNs share one bucket
     (matching [Float.compare nan nan = 0]) and none can collide with a number
     or with NULL's [0x00];
   - [-0.0] canonicalises to [IK_int 0], which is what makes it join [0.0] and
     [0] alike — [Float.compare (-0.) 0. = 0], so [compare_values] agrees.
     Before #754 the raw index encoding separated [-0.0] from [+0.0]
     ([-0.0 < +0.0]), which was why the raw bytes alone could not be used for
     this canonical key; [Index_key.encode_value] now normalizes [-0.0] to
     [+0.0]'s bits too, so the raw encoding and this canonical key agree on
     every REAL value, not just the ones this function special-cases;
   - a cross-CLASS pair keeps distinct tag bytes and never joins, which is
     [compare_values]' answer too.

   NULL is never handed here: both the build side and the probe side drop a
   NULL join key before keying, because [col = NULL] matches nothing under
   three-valued logic. *)
let join_key_value (v : Row.value) : Index_key.value =
  match v with
  | Row.V_real f ->
    (match int64_of_exact_real f with
     | Some n -> Index_key.IK_int n
     | None -> Index_key.IK_real f)
  | Row.V_int _ | Row.V_text _ | Row.V_blob _ | Row.V_null -> row_value_to_index_value v
;;

(* #743: the bytes a hash join buckets a non-NULL join key under. *)
let join_key_bytes (v : Row.value) : bytes = Index_key.encode_value (join_key_value v)

(* #579: a TOTAL order over values, which is what every caller needs and what
   this did not used to be.

   It used to end in [| _, _ -> 0  (* cross-type: shouldn't happen *)], and it
   does happen: strict column typing keeps a STORED column single-typed, but a
   COMPUTED one is unconstrained per row, so
   [SELECT CASE WHEN i = 0 THEN f ELSE i END AS v FROM n ORDER BY v] mixes
   INTEGERs and REALs in one column.  Every such pair compared EQUAL, which made
   the relation non-transitive ([1 = 2.5], [2.5 = 3], but [1 < 3]) — and
   [List.sort] on a non-transitive comparator has no defined result, so the rows
   came back in scan order, entirely unsorted, with no error.

   The blast radius is every ordering the engine does: ORDER BY (through
   [compare_with_nulls]), GROUP BY (which sorts and then groups adjacent runs,
   so WHICH rows land in WHICH group became input-order-dependent), window
   PARTITION BY, and MIN/MAX (which became first-wins).

   Two rules:

   - within the numeric class, compare EXACTLY through {!cmp_int_real}, which
     also puts NaN below every number (#536's decided order, where
     [NULL < NaN < every number] — NULL is a lower CLASS, so both halves hold);
   - across classes, order by {!value_class_rank}.

   #733/#734: [cmp_result] — the [<]/[<=]/[>]/[>=] half of a WHERE predicate —
   is now this function plus three-valued NULL handling, so an ORDER BY and a
   WHERE can no longer disagree about where a value sits.  For one release they
   did, in two separate ways: [cmp_result] promoted int-vs-real through
   [Int64.to_float] (inexact above 2^53, #733) and ended in
   [| _ -> Row.V_int 0L], applying no class order at all (#734).  Both are
   gone.  #738 routed [Eq]/[Ne] through [cmp_result] too, so all six comparison
   operators share this function; that took the three index equality sites with
   it (see {!index_lookup_values}), and #743 then took the two JOIN KEY sites
   (see {!join_key_value}), for the same reason in both cases — an equality is
   CONSUMED by the access path, so nothing re-checks the rows it yields.

   DISTINCT is deliberately NOT routed through this: it dedups on [row_key]'s
   string rendering, where [1] and [1.0] are different keys.  So DISTINCT and
   GROUP BY still disagree about whether an int and a numerically equal real are
   one key.  That is a real inconsistency and it is #579's "Note" — four
   comparators that should be one — not something this change decides. *)
let compare_values (a : Row.value) (b : Row.value) : int =
  match a, b with
  | Row.V_null, Row.V_null -> 0
  | Row.V_null, _ ->
    -1 (* NULLs sort first — less than any non-null value, matches SQLite *)
  | _, Row.V_null -> 1
  | Row.V_int x, Row.V_int y -> Int64.compare x y
  | Row.V_real x, Row.V_real y -> Float.compare x y
  | Row.V_text x, Row.V_text y -> String.compare x y
  | Row.V_blob x, Row.V_blob y -> Bytes.compare x y
  (* #579: numeric promotion, but EXACT — see {!cmp_int_real}.  #733 routed
     [cmp_result] through here, so the WHERE predicate is exact at these
     magnitudes too. *)
  | Row.V_int x, Row.V_real y -> cmp_int_real x y
  | Row.V_real x, Row.V_int y -> -cmp_int_real y x
  (* #579: everything left is a genuine cross-CLASS pair (number/text/blob in
     some order).  Ordered by class rather than compared equal.

     With the exact numeric arm above, this function IS a total order — every
     pair of values is related, antisymmetrically and transitively — for every
     input, with no magnitude caveat. *)
  | _, _ -> Int.compare (value_class_rank a) (value_class_rank b)
;;

let compare_with_nulls
      (dir : [ `Asc | `Desc ])
      (nulls : [ `Nulls_first | `Nulls_last ])
      (va : Row.value)
      (vb : Row.value)
  : int
  =
  match va, vb with
  | Row.V_null, Row.V_null -> 0
  | Row.V_null, _ ->
    (match nulls with
     | `Nulls_first -> -1
     | `Nulls_last -> 1)
  | _, Row.V_null ->
    (match nulls with
     | `Nulls_first -> 1
     | `Nulls_last -> -1)
  | _, _ ->
    let c = compare_values va vb in
    (match dir with
     | `Asc -> c
     | `Desc -> -c)
;;

let list_drop n lst =
  let rec go k = function
    | [] -> []
    | _ :: t as l -> if k <= 0 then l else go (k - 1) t
  in
  go n lst
;;

let list_take n lst =
  let rec go k = function
    | [] -> []
    | h :: t -> if k <= 0 then [] else h :: go (k - 1) t
  in
  go n lst
;;

(** Find a column ordinal by name within a [Row.column] list. *)
let find_col_idx_by_name (cols : Row.column list) (name : string) : int =
  let rec find i = function
    | [] -> failwith (Printf.sprintf "column not found: %s" name)
    | (c : Row.column) :: _ when String.equal c.Row.name name -> i
    | _ :: rest -> find (i + 1) rest
  in
  find 0 cols
;;

(* Module-level cache for compiled CHECK expressions.
   Key: (table_name, column_ordinal, check_sql) → compiled Plan.expr.
   Including check_sql avoids stale hits when different tables share the same
   name and column index across DB instances (e.g. test isolation). *)
let check_expr_cache : (string * int * string, Plan.expr) Hashtbl.t = Hashtbl.create 16

(* ── DDL reconstruction for Op_sqlite_master ─────────────────── *)

let sql_of_row_type = function
  | Row.Integer -> "INTEGER"
  | Row.Text -> "TEXT"
  | Row.Real -> "REAL"
  | Row.Blob -> "BLOB"
;;

(* A SQL single-quoted string literal with embedded quotes doubled. *)
let quote_text_literal s = "'" ^ String.concat "''" (String.split_on_char '\'' s) ^ "'"

let sql_of_default_value = function
  | Row.DV_int n -> Int64.to_string n
  | Row.DV_text s -> quote_text_literal s
  | Row.DV_real f -> Printf.sprintf "%g" f
  | Row.DV_blob b ->
    let hex =
      Bytes.to_seq b
      |> Seq.map (fun c -> Printf.sprintf "%02X" (Char.code c))
      |> List.of_seq
      |> String.concat ""
    in
    Printf.sprintf "X'%s'" hex
  | Row.DV_null -> "NULL"
  | Row.DV_current_timestamp -> "CURRENT_TIMESTAMP"
  | Row.DV_current_date -> "CURRENT_DATE"
  | Row.DV_current_time -> "CURRENT_TIME"
;;

let sql_of_fk_action = function
  | Cat.FA_no_action -> "NO ACTION"
  | Cat.FA_restrict -> "RESTRICT"
  | Cat.FA_cascade -> "CASCADE"
  | Cat.FA_set_null -> "SET NULL"
  | Cat.FA_set_default -> "SET DEFAULT"
;;

(* Phase 35 task 3b: quote DDL identifiers that contain non-alphanumeric
   characters, start with a digit, or are empty.  Embedded double-quotes are
   doubled per SQL identifier syntax.

   #572: the rule now lives in [Ast] and is shared with [Ast.expr_to_sql],
   which had no quoting at all and so poisoned the CHECK / GENERATED /
   partial-index text it built.  Two emitters of the same SQL text must agree
   on when a name is bare, so there is one implementation, not two — and the
   one implementation also covers the case this copy missed: a name that is a
   plain word but a reserved one ([CREATE TABLE t ("key" INTEGER)] rendered as
   [key INTEGER], which does not parse). *)
let quote_ident = Ast.quote_ident

let format_pk_suffix ~autoinc_idx i (col : Row.column) buf =
  Buffer.add_string buf " PRIMARY KEY";
  (* #312: a DESC PK is a non-alias; re-emit DESC so reopen reproduces
     the non-alias shape (hidden rowid + __pk index). *)
  if col.Row.pk_desc then Buffer.add_string buf " DESC";
  if Some i = autoinc_idx then Buffer.add_string buf " AUTOINCREMENT"
;;

(* #530: a composite PRIMARY KEY marks every one of its columns, so the inline
   [PRIMARY KEY] suffix cannot be emitted per column — that would render
   [PRIMARY KEY (k, j)] as two separate single-column keys, a different table.

   #533: the suppressed set is derived from the KEY ITSELF — the `Implicit_pk
   index, whose [idx_columns] names exactly the members of one table-level
   [PRIMARY KEY (...)], in key order.  Counting marked columns globally (the
   first cut of #530) is wrong, because this engine accepts more than one
   [PRIMARY KEY] declaration on a table (SQLite rejects it; we don't).  A count
   of 2 then dropped BOTH single-column keys out of the rendered DDL — and
   [ddl_implies_index] still called their indexes implied, so [Db.dump] emitted
   a table with no primary key and no unique index at all.

   That index is also what supplies the key ORDINAL that [Row.column] lacks, so
   the table-level form is now reconstructed faithfully instead of dropped: a
   composite PK survives [Db.dump] as a PRIMARY KEY rather than being downgraded
   to a bare UNIQUE index. *)

(* How the PRIMARY KEY of one table is rendered: which columns must NOT carry
   the inline suffix, and which table-level [PRIMARY KEY (...)] clauses to emit.
   One value, computed once, read by both the renderer and [ddl_implies_index],
   so the two cannot answer the question independently and drift. *)
type pk_layout =
  { pk_inline_suppressed : string list
  ; pk_table_level : string list list
  }

(* #533: a `Implicit_pk index can name a column the table no longer has —
   [ALTER TABLE ... RENAME COLUMN] renames the column but not [idx_columns], so
   [__pk_c_k_j_0 ON c (k, j)] outlives [j].  Trusting it then emits
   [PRIMARY KEY (k, j)] inside the CREATE TABLE, naming a column that does not
   exist, and [ddl_implies_index] suppresses the CREATE INDEX that would
   otherwise have carried (and failed on) the staleness — so the TABLE fails to
   restore and every row in it is lost, where before only the index was lost.

   When any implicit PK index of the table is stale, fall all the way back to
   the pre-#533 shape: render no key at all and let the [CREATE UNIQUE INDEX]
   be emitted.  The dump then loses a constraint, which is the degradation this
   whole area is allowed; it does not lose the table.  The stale index itself is
   a catalog bug (#553) — this only keeps its blast radius at the index. *)
let pk_layout (meta : Cat.table_meta) (indexes : Cat.index_info list) =
  let names = List.map (fun (c : Row.column) -> c.Row.name) meta.Cat.columns in
  let implicit =
    List.filter (fun (i : Cat.index_info) -> i.Cat.idx_origin = `Implicit_pk) indexes
  in
  let stale =
    List.exists
      (fun (i : Cat.index_info) ->
         List.exists (fun c -> not (List.mem c names)) i.Cat.idx_columns)
      implicit
  in
  if stale
  then { pk_inline_suppressed = names; pk_table_level = [] }
  else (
    let composite =
      List.filter_map
        (fun (i : Cat.index_info) ->
           match i.Cat.idx_columns with
           | _ :: _ :: _ as cols -> Some cols
           | _ -> None)
        implicit
    in
    { pk_inline_suppressed = List.concat composite; pk_table_level = composite })
;;

(* Does [ddl_of_table] emit the inline [PRIMARY KEY] suffix on [col]? *)
let inline_pk_emitted ~layout (col : Row.column) =
  col.Row.primary_key && not (List.mem col.Row.name layout.pk_inline_suppressed)
;;

let format_column ~autoinc_idx ~layout i (col : Row.column) =
  let buf = Buffer.create 64 in
  Buffer.add_string buf (quote_ident col.Row.name);
  Buffer.add_char buf ' ';
  Buffer.add_string buf (sql_of_row_type col.Row.ty);
  if col.Row.not_null then Buffer.add_string buf " NOT NULL";
  if inline_pk_emitted ~layout col then format_pk_suffix ~autoinc_idx i col buf;
  (match col.Row.default with
   | None -> ()
   | Some dv ->
     Buffer.add_string buf " DEFAULT ";
     Buffer.add_string buf (sql_of_default_value dv));
  (match col.Row.check_sql with
   | None -> ()
   | Some sql ->
     Buffer.add_string buf " CHECK(";
     Buffer.add_string buf sql;
     Buffer.add_char buf ')');
  (match col.Row.generated_as with
   | None -> ()
   | Some (expr_sql, is_stored) ->
     Buffer.add_string buf " GENERATED ALWAYS AS (";
     Buffer.add_string buf expr_sql;
     Buffer.add_string buf ") ";
     Buffer.add_string buf (if is_stored then "STORED" else "VIRTUAL"));
  Buffer.contents buf
;;

let ddl_of_table ~(indexes : Cat.index_info list) (meta : Cat.table_meta) =
  let without_rowid, autoincrement =
    match meta.Cat.storage with
    | Cat.Row { without_rowid; autoincrement; _ } -> without_rowid, autoincrement
    | Cat.Columnar _ -> false, false
  in
  let autoinc_idx =
    if autoincrement
    then Cat.compute_rowid_alias_col meta.Cat.columns ~without_rowid
    else None
  in
  let layout = pk_layout meta indexes in
  let col_parts = List.mapi (format_column ~autoinc_idx ~layout) meta.Cat.columns in
  let pk_parts =
    List.map
      (fun cols ->
         Printf.sprintf
           "PRIMARY KEY (%s)"
           (String.concat ", " (List.map quote_ident cols)))
      layout.pk_table_level
  in
  let fk_parts =
    List.map
      (fun (fk : Cat.fk_constraint) ->
         Printf.sprintf
           "FOREIGN KEY (%s) REFERENCES %s(%s) ON DELETE %s ON UPDATE %s"
           (String.concat ", " (List.map quote_ident fk.Cat.fk_local_cols))
           (quote_ident fk.Cat.fk_parent_table)
           (String.concat ", " (List.map quote_ident fk.Cat.fk_parent_cols))
           (sql_of_fk_action fk.Cat.fk_on_delete)
           (sql_of_fk_action fk.Cat.fk_on_update))
      meta.Cat.fk_constraints
  in
  Printf.sprintf
    "CREATE TABLE %s (%s)%s%s"
    (quote_ident meta.Cat.name)
    (String.concat ", " (col_parts @ pk_parts @ fk_parts))
    (if without_rowid then " WITHOUT ROWID" else "")
    (if Cat.is_columnar meta then " USING COLUMNSTORE" else "")
;;

(* #533: is [idx] already implied by the DDL [ddl_of_table] renders for its
   table, so [Db.dump] can skip its CREATE INDEX?  Answered from
   [inline_pk_emitted]/[pk_layout] — the SAME predicates the renderer used —
   because the two used to answer it independently, and that is exactly how a
   suppressed inline PRIMARY KEY ended up paired with a suppressed index and a
   dump that silently accepted duplicates on restore. *)
let ddl_implies_index (meta : Cat.table_meta) ~indexes (idx : Cat.index_info) =
  match idx.Cat.idx_origin with
  | `Implicit_unique | `User -> false
  | `Implicit_pk ->
    let layout = pk_layout meta indexes in
    (match idx.Cat.idx_columns with
     | [ col ] ->
       List.exists
         (fun (c : Row.column) ->
            String.equal c.Row.name col && inline_pk_emitted ~layout c)
         meta.Cat.columns
     | cols -> List.mem cols layout.pk_table_level)
;;

(** Extract the ON <table> target from a CREATE TRIGGER statement.
    Falls back to the trigger name if the ON clause is not found. *)
let trigger_table_of_sql trigger_name sql =
  (* Look for " ON " followed by identifier, case-insensitive *)
  let upper = String.uppercase_ascii sql in
  match String.index_opt upper 'O' with
  | None -> trigger_name
  | _ ->
    let n = String.length upper in
    (* Search for " ON " pattern *)
    let rec search i =
      if i + 4 >= n
      then trigger_name
      else if
        upper.[i] = ' '
        && upper.[i + 1] = 'O'
        && upper.[i + 2] = 'N'
        && upper.[i + 3] = ' '
      then (
        (* Found " ON " — extract the identifier that follows *)
        let start = i + 4 in
        let j = ref start in
        while
          !j < n
          &&
          let c = upper.[!j] in
          (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'
        do
          incr j
        done;
        if !j > start then String.sub sql start (!j - start) else trigger_name)
      else search (i + 1)
    in
    search 0
;;

let ddl_of_index (idx : Cat.index_info) =
  let unique_kw = if idx.Cat.idx_unique then "UNIQUE " else "" in
  let col_strs =
    List.map2
      (fun col_sql is_expr ->
         if is_expr then Printf.sprintf "(%s)" col_sql else quote_ident col_sql)
      idx.Cat.idx_columns
      idx.Cat.idx_expr_flags
  in
  let cols_str = String.concat ", " col_strs in
  let where_clause =
    match idx.Cat.idx_where_sql with
    | None -> ""
    | Some sql -> Printf.sprintf " WHERE %s" sql
  in
  Printf.sprintf
    "CREATE %sINDEX %s ON %s (%s)%s"
    unique_kw
    (quote_ident idx.Cat.idx_name)
    (quote_ident idx.Cat.idx_table)
    cols_str
    where_clause
;;

let ddl_of_fts (m : Cat.fts_table_meta) =
  Printf.sprintf
    "CREATE VIRTUAL TABLE %s USING fts5(%s)"
    (quote_ident m.Cat.fts_name)
    (String.concat ", " (List.map quote_ident m.Cat.fts_columns))
;;

(* ------------------------------------------------------------------ *)
(* Expression evaluation                                                *)
(* (Defined before [execute] so that [Op_update] can evaluate WHERE     *)
(*  predicates and right-hand-side expressions for SET assignments.)    *)
(* ------------------------------------------------------------------ *)

let value_truthy : Row.value -> bool = function
  | Row.V_null | Row.V_int 0L -> false
  | _ -> true
;;

(* Pattern matching helpers for LIKE and GLOB.
   Uses naive recursive backtracking: worst case is O(2^k) for k '%'/'*'
   metacharacters against an adversarial string.  Acceptable for typical
   SQL workloads; replace with NFA/DP if adversarial patterns are a concern. *)
let rec like_match pat pi str si =
  let plen = String.length pat
  and slen = String.length str in
  if pi = plen
  then si = slen
  else (
    match pat.[pi] with
    | '%' ->
      like_match pat (pi + 1) str si || (si < slen && like_match pat pi str (si + 1))
    | '_' -> si < slen && like_match pat (pi + 1) str (si + 1)
    | c ->
      si < slen
      && Char.lowercase_ascii c = Char.lowercase_ascii str.[si]
      && like_match pat (pi + 1) str (si + 1))
;;

let rec glob_match pat pi str si =
  let plen = String.length pat
  and slen = String.length str in
  if pi = plen
  then si = slen
  else (
    match pat.[pi] with
    | '*' ->
      glob_match pat (pi + 1) str si || (si < slen && glob_match pat pi str (si + 1))
    | '?' -> si < slen && glob_match pat (pi + 1) str (si + 1)
    | c -> si < slen && c = str.[si] && glob_match pat (pi + 1) str (si + 1))
;;

let str_trim_spaces s =
  let n = String.length s in
  let l = ref 0
  and r = ref (n - 1) in
  while
    !l <= !r
    &&
    let c = s.[!l] in
    c = ' ' || c = '\t' || c = '\n' || c = '\r'
  do
    incr l
  done;
  while
    !r >= !l
    &&
    let c = s.[!r] in
    c = ' ' || c = '\t' || c = '\n' || c = '\r'
  do
    decr r
  done;
  if !l > !r then "" else String.sub s !l (!r - !l + 1)
;;

let parse_int_prefix s =
  let s = String.trim s in
  match Int64.of_string_opt s with
  | Some n -> n
  | None ->
    (match float_of_string_opt s with
     | Some f -> Int64.of_float f
     | None ->
       (* Scan leading numeric prefix: optional sign, digits, optional decimal *)
       let n = String.length s in
       let i = ref 0 in
       if !i < n && (s.[!i] = '-' || s.[!i] = '+') then incr i;
       let digit_start = !i in
       while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do
         incr i
       done;
       (* Include decimal part for float->int conversion *)
       let has_dot = !i < n && s.[!i] = '.' in
       if has_dot
       then (
         incr i;
         while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do
           incr i
         done);
       if !i > digit_start
       then (
         match float_of_string_opt (String.sub s 0 !i) with
         | Some f -> Int64.of_float f
         | None ->
           (match Int64.of_string_opt (String.sub s 0 !i) with
            | Some v -> v
            | None -> 0L))
       else 0L)
;;

let parse_real_prefix s =
  let s = String.trim s in
  match float_of_string_opt s with
  | Some f -> f
  | None ->
    (* Try progressively shorter prefixes until one parses *)
    let n = String.length s in
    let result = ref 0.0 in
    let found = ref false in
    let i = ref n in
    while !i > 0 && not !found do
      match float_of_string_opt (String.sub s 0 !i) with
      | Some f ->
        result := f;
        found := true
      | None -> decr i
    done;
    !result
;;

let str_trim_chars s chars =
  let n = String.length s in
  let l = ref 0
  and r = ref (n - 1) in
  while !l <= !r && String.contains chars s.[!l] do
    incr l
  done;
  while !r >= !l && String.contains chars s.[!r] do
    decr r
  done;
  if !l > !r then "" else String.sub s !l (!r - !l + 1)
;;

let str_ltrim_spaces s =
  let n = String.length s in
  let l = ref 0 in
  while
    !l < n
    &&
    let c = s.[!l] in
    c = ' ' || c = '\t' || c = '\n' || c = '\r'
  do
    incr l
  done;
  String.sub s !l (n - !l)
;;

let str_ltrim_chars s chars =
  let n = String.length s in
  let l = ref 0 in
  while !l < n && String.contains chars s.[!l] do
    incr l
  done;
  String.sub s !l (n - !l)
;;

let str_rtrim_spaces s =
  let n = String.length s in
  let r = ref (n - 1) in
  while
    !r >= 0
    &&
    let c = s.[!r] in
    c = ' ' || c = '\t' || c = '\n' || c = '\r'
  do
    decr r
  done;
  if !r < 0 then "" else String.sub s 0 (!r + 1)
;;

let str_rtrim_chars s chars =
  let r = ref (String.length s - 1) in
  while !r >= 0 && String.contains chars s.[!r] do
    decr r
  done;
  if !r < 0 then "" else String.sub s 0 (!r + 1)
;;

let str_replace s old rep =
  if String.length old = 0
  then s
  else (
    let buf = Buffer.create (String.length s) in
    let n = String.length s
    and m = String.length old in
    let i = ref 0 in
    while !i <= n - m do
      if String.sub s !i m = old
      then (
        Buffer.add_string buf rep;
        i := !i + m)
      else (
        Buffer.add_char buf s.[!i];
        incr i)
    done;
    while !i < n do
      Buffer.add_char buf s.[!i];
      incr i
    done;
    Buffer.contents buf)
;;

let str_instr s sub =
  let n = String.length s
  and m = String.length sub in
  if m = 0
  then 1
  else (
    let found = ref 0 in
    let i = ref 0 in
    while !found = 0 && !i <= n - m do
      if String.sub s !i m = sub then found := !i + 1 (* 1-indexed *) else incr i
    done;
    !found)
;;

let row_key (row : Row.t) : string =
  let buf = Buffer.create 64 in
  Array.iter
    (function
      | Row.V_null -> Buffer.add_string buf "N|"
      | Row.V_int n ->
        Buffer.add_char buf 'I';
        Buffer.add_string buf (Int64.to_string n);
        Buffer.add_char buf '|'
      | Row.V_real f ->
        Buffer.add_char buf 'R';
        Buffer.add_string buf (Printf.sprintf "%h" f);
        Buffer.add_char buf '|'
      | Row.V_text s ->
        Buffer.add_char buf 'T';
        Buffer.add_string buf (string_of_int (String.length s));
        Buffer.add_char buf ':';
        Buffer.add_string buf s;
        Buffer.add_char buf '|'
      | Row.V_blob b ->
        Buffer.add_char buf 'B';
        Buffer.add_string buf (string_of_int (Bytes.length b));
        Buffer.add_char buf ':';
        Buffer.add_bytes buf b;
        Buffer.add_char buf '|')
    row;
  Buffer.contents buf
;;

let json_of_sql : Row.value -> Json.value = function
  | Row.V_null -> Json.J_null
  | Row.V_int n -> Json.J_int n
  | Row.V_real f -> Json.J_float f
  | Row.V_text s -> Json.J_string s
  | Row.V_blob b -> Json.J_string (Bytes.to_string b)
;;

let sql_of_json : Json.value -> Row.value = function
  | Json.J_null -> Row.V_null
  | Json.J_bool b -> Row.V_int (if b then 1L else 0L)
  | Json.J_int n -> Row.V_int n
  | Json.J_float f -> Row.V_real f
  | Json.J_string s -> Row.V_text s
  | Json.J_array _ as v -> Row.V_text (Json.to_string v)
  | Json.J_object _ as v -> Row.V_text (Json.to_string v)
;;

(* ── Scalar-function evaluation, split by category (#168) ──────────
   [eval_func] dispatches to the [eval_*_func] helpers below; each returns
   [Some v] for the functions it owns and [None] otherwise, so [eval_func]
   can chain them and fall through to the arity-error case.  Verbose
   per-function bodies are themselves factored into small named helpers. *)

let hex_encode_str s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02X" (Char.code c))) s;
  Buffer.contents buf
;;

(* #264: render a value as a standalone SQL literal for a logical dump.
   The output must parse back to the identical value through our own executor:
   - text is single-quoted with embedded quotes doubled;
   - blobs use the [X'..'] hex syntax;
   - a finite float always carries a '.' or exponent so it re-reads as REAL
     (not INTEGER), and uses the shortest decimal that round-trips bit-for-bit;
   - non-finite floats map to [1e999]/[-1e999] (overflow to ±inf, as SQLite's
     own .dump emits) and NaN to NULL (SQLite cannot store a NaN). *)
let sql_literal_of_value : Row.value -> string = function
  | Row.V_null -> "NULL"
  | Row.V_int n -> Int64.to_string n
  | Row.V_text s -> quote_text_literal s
  | Row.V_blob b -> "X'" ^ hex_encode_str (Bytes.to_string b) ^ "'"
  | Row.V_real f ->
    if Float.is_nan f
    then "NULL"
    else if f = Float.infinity
    then "1e999"
    else if f = Float.neg_infinity
    then "-1e999"
    else (
      let rec shortest p =
        if p >= 17
        then Printf.sprintf "%.17g" f
        else (
          let s = Printf.sprintf "%.*g" p f in
          if float_of_string s = f then s else shortest (p + 1))
      in
      let s = shortest 1 in
      if String.contains s '.' || String.contains s 'e' || String.contains s 'E'
      then s
      else s ^ ".0")
;;

(* UTF-8 encode each in-range integer codepoint, mirroring SQLite's char(). *)
let char_encode args =
  let buf = Buffer.create 16 in
  List.iter
    (fun v ->
       match v with
       | Row.V_int n when n >= 1L && n <= 0x10FFFFL ->
         let cp = Int64.to_int n in
         if cp < 0x80
         then Buffer.add_char buf (Char.chr cp)
         else if cp < 0x800
         then (
           Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
           Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
         else if cp < 0x10000
         then (
           Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
           Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
           Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
         else (
           Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
           Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
           Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
           Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F))))
       | _ -> ())
    args;
  Buffer.contents buf
;;

(* Decode the codepoint of the first UTF-8 character of [s] (s non-empty). *)
let unicode_codepoint s =
  let b0 = Char.code s.[0] in
  if b0 < 0x80
  then b0
  else if b0 < 0xE0 && String.length s >= 2
  then ((b0 land 0x1F) lsl 6) lor (Char.code s.[1] land 0x3F)
  else if b0 < 0xF0 && String.length s >= 3
  then
    ((b0 land 0x0F) lsl 12)
    lor ((Char.code s.[1] land 0x3F) lsl 6)
    lor (Char.code s.[2] land 0x3F)
  else if b0 >= 0xF0 && String.length s >= 4
  then
    ((b0 land 0x07) lsl 18)
    lor ((Char.code s.[1] land 0x3F) lsl 12)
    lor ((Char.code s.[2] land 0x3F) lsl 6)
    lor (Char.code s.[3] land 0x3F)
  else b0
;;

(* Emit one printf conversion [spec] (the char after '%') to [buf], pulling
   the next argument via [get_arg]. *)
let printf_emit buf spec (get_arg : unit -> Row.value) =
  match spec with
  | '%' -> Buffer.add_char buf '%'
  | 'd' | 'i' ->
    (match get_arg () with
     | Row.V_int n2 -> Buffer.add_string buf (Int64.to_string n2)
     | Row.V_real f -> Buffer.add_string buf (string_of_int (int_of_float f))
     | Row.V_text s ->
       (try Buffer.add_string buf (string_of_int (int_of_string s)) with
        | Failure _ -> ())
     | _ -> ())
  | 'f' ->
    (match get_arg () with
     | Row.V_real f -> Buffer.add_string buf (Printf.sprintf "%f" f)
     | Row.V_int n2 -> Buffer.add_string buf (Printf.sprintf "%f" (Int64.to_float n2))
     | _ -> ())
  | 'e' ->
    (match get_arg () with
     | Row.V_real f -> Buffer.add_string buf (Printf.sprintf "%e" f)
     | Row.V_int n2 -> Buffer.add_string buf (Printf.sprintf "%e" (Int64.to_float n2))
     | _ -> ())
  | 'g' ->
    (match get_arg () with
     | Row.V_real f -> Buffer.add_string buf (Printf.sprintf "%g" f)
     | Row.V_int n2 -> Buffer.add_string buf (Printf.sprintf "%g" (Int64.to_float n2))
     | _ -> ())
  | 's' ->
    (match get_arg () with
     | Row.V_text s -> Buffer.add_string buf s
     | Row.V_int n2 -> Buffer.add_string buf (Int64.to_string n2)
     | Row.V_real f -> Buffer.add_string buf (Printf.sprintf "%g" f)
     | Row.V_null -> Buffer.add_string buf "NULL"
     | Row.V_blob _ -> Buffer.add_string buf "")
  | 'q' ->
    (match get_arg () with
     | Row.V_text s ->
       String.iter
         (fun c -> if c = '\'' then Buffer.add_string buf "''" else Buffer.add_char buf c)
         s
     | Row.V_int n2 -> Buffer.add_string buf (Int64.to_string n2)
     | Row.V_real f -> Buffer.add_string buf (Printf.sprintf "%g" f)
     | Row.V_null -> Buffer.add_string buf "NULL"
     | Row.V_blob _ -> ())
  | c ->
    Buffer.add_char buf '%';
    Buffer.add_char buf c
;;

(* SQLite printf()/format(): a small subset of C printf conversions. *)
let printf_format fmt rest =
  let args_arr = Array.of_list rest in
  let arg_idx = ref 0 in
  let get_arg () =
    let v =
      if !arg_idx < Array.length args_arr then args_arr.(!arg_idx) else Row.V_null
    in
    incr arg_idx;
    v
  in
  let buf = Buffer.create 64 in
  let n = String.length fmt in
  let i = ref 0 in
  while !i < n do
    if fmt.[!i] = '%'
    then (
      incr i;
      if !i < n
      then (
        printf_emit buf fmt.[!i] get_arg;
        incr i))
    else (
      Buffer.add_char buf fmt.[!i];
      incr i)
  done;
  Buffer.contents buf
;;

let eval_str_func (func : Ast.scalar_func) (args : Row.value list) : Row.value option =
  match func, args with
  | Ast.Fn_length, [ Row.V_text s ] -> Some (Row.V_int (Int64.of_int (String.length s)))
  | Ast.Fn_length, [ Row.V_blob b ] -> Some (Row.V_int (Int64.of_int (Bytes.length b)))
  | Ast.Fn_length, [ Row.V_null ] -> Some Row.V_null
  | Ast.Fn_length, [ _ ] -> Some Row.V_null (* non-text/blob: return null like SQLite *)
  | Ast.Fn_lower, [ Row.V_text s ] -> Some (Row.V_text (String.lowercase_ascii s))
  | Ast.Fn_lower, [ Row.V_null ] -> Some Row.V_null
  | Ast.Fn_lower, [ _ ] -> Some Row.V_null
  | Ast.Fn_upper, [ Row.V_text s ] -> Some (Row.V_text (String.uppercase_ascii s))
  | Ast.Fn_upper, [ Row.V_null ] -> Some Row.V_null
  | Ast.Fn_upper, [ _ ] -> Some Row.V_null
  | Ast.Fn_substr, Row.V_text s :: rest ->
    Some
      (match rest with
       | [ Row.V_int start ] ->
         let i = max 0 (Int64.to_int start - 1) in
         if i >= String.length s
         then Row.V_text ""
         else Row.V_text (String.sub s i (String.length s - i))
       | [ Row.V_int start; Row.V_int len ] ->
         let i = max 0 (Int64.to_int start - 1) in
         let l = Int64.to_int len in
         if i >= String.length s || l <= 0
         then Row.V_text ""
         else Row.V_text (String.sub s i (min l (String.length s - i)))
       | _ -> Row.V_null)
  | Ast.Fn_substr, Row.V_null :: _ -> Some Row.V_null
  | Ast.Fn_trim, [ Row.V_text s ] -> Some (Row.V_text (str_trim_spaces s))
  | Ast.Fn_trim, [ Row.V_text s; Row.V_text chars ] ->
    Some (Row.V_text (str_trim_chars s chars))
  | Ast.Fn_trim, [ _; Row.V_null ] -> Some Row.V_null
  | Ast.Fn_trim, Row.V_null :: _ -> Some Row.V_null
  | Ast.Fn_ltrim, [ Row.V_text s ] -> Some (Row.V_text (str_ltrim_spaces s))
  | Ast.Fn_ltrim, [ Row.V_text s; Row.V_text chars ] ->
    Some (Row.V_text (str_ltrim_chars s chars))
  | Ast.Fn_ltrim, [ _; Row.V_null ] -> Some Row.V_null
  | Ast.Fn_ltrim, Row.V_null :: _ -> Some Row.V_null
  | Ast.Fn_rtrim, [ Row.V_text s ] -> Some (Row.V_text (str_rtrim_spaces s))
  | Ast.Fn_rtrim, [ Row.V_text s; Row.V_text chars ] ->
    Some (Row.V_text (str_rtrim_chars s chars))
  | Ast.Fn_rtrim, [ _; Row.V_null ] -> Some Row.V_null
  | Ast.Fn_rtrim, Row.V_null :: _ -> Some Row.V_null
  | Ast.Fn_replace, [ Row.V_text s; Row.V_text old; Row.V_text rep ] ->
    Some (Row.V_text (str_replace s old rep))
  | Ast.Fn_replace, [ _; Row.V_null; _ ] -> Some Row.V_null
  | Ast.Fn_replace, [ _; _; Row.V_null ] -> Some Row.V_null
  | Ast.Fn_replace, Row.V_null :: _ -> Some Row.V_null
  | Ast.Fn_instr, [ Row.V_text s; Row.V_text sub ] ->
    Some (Row.V_int (Int64.of_int (str_instr s sub)))
  | Ast.Fn_instr, Row.V_null :: _ | Ast.Fn_instr, [ _; Row.V_null ] -> Some Row.V_null
  | Ast.Fn_hex, [ Row.V_blob b ] -> Some (Row.V_text (hex_encode_str (Bytes.to_string b)))
  | Ast.Fn_hex, [ Row.V_text s ] -> Some (Row.V_text (hex_encode_str s))
  | Ast.Fn_hex, [ Row.V_int n ] -> Some (Row.V_text (hex_encode_str (Int64.to_string n)))
  | Ast.Fn_hex, [ Row.V_null ] -> Some (Row.V_text "")
  | Ast.Fn_char, args -> Some (Row.V_text (char_encode args))
  | Ast.Fn_unicode, [ Row.V_text s ] when String.length s > 0 ->
    Some (Row.V_int (Int64.of_int (unicode_codepoint s)))
  | Ast.Fn_unicode, [ Row.V_text _ ] -> Some Row.V_null
  | Ast.Fn_unicode, [ Row.V_null ] -> Some Row.V_null
  | Ast.Fn_printf, Row.V_text fmt :: rest -> Some (Row.V_text (printf_format fmt rest))
  | Ast.Fn_printf, _ -> Some Row.V_null
  | _ -> None
;;

let eval_math_func (func : Ast.scalar_func) (args : Row.value list) : Row.value option =
  let to_float_opt = function
    | Row.V_real f -> Some f
    | Row.V_int n -> Some (Int64.to_float n)
    | _ -> None
  in
  match func, args with
  | Ast.Fn_abs, [ Row.V_int n ] -> Some (Row.V_int (Int64.abs n))
  | Ast.Fn_abs, [ Row.V_real f ] -> Some (Row.V_real (Float.abs f))
  | Ast.Fn_abs, [ Row.V_null ] -> Some Row.V_null
  | Ast.Fn_abs, [ _ ] -> Some Row.V_null
  | Ast.Fn_round, [ Row.V_real f ] -> Some (Row.V_real (Float.round f))
  | Ast.Fn_round, [ Row.V_int n ] -> Some (Row.V_real (Int64.to_float n))
  | Ast.Fn_round, [ Row.V_real f; Row.V_int d ] ->
    let factor = 10. ** Int64.to_float d in
    Some (Row.V_real (Float.round (f *. factor) /. factor))
  | Ast.Fn_round, [ Row.V_int n; Row.V_int _ ] -> Some (Row.V_real (Int64.to_float n))
  | Ast.Fn_round, [ _; Row.V_null ] -> Some Row.V_null
  | Ast.Fn_round, Row.V_null :: _ -> Some Row.V_null
  | Ast.Fn_ceil, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.ceil f)
       | None -> Row.V_null)
  | Ast.Fn_floor, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.floor f)
       | None -> Row.V_null)
  | Ast.Fn_sqrt, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.sqrt f)
       | None -> Row.V_null)
  | Ast.Fn_pow, [ b; e ] ->
    Some
      (match to_float_opt b, to_float_opt e with
       | Some bf, Some ef -> Row.V_real (bf ** ef)
       | _ -> Row.V_null)
  | Ast.Fn_exp, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.exp f)
       | None -> Row.V_null)
  | Ast.Fn_ln, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.log f)
       | None -> Row.V_null)
  | Ast.Fn_log, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.log f)
       | None -> Row.V_null)
  | Ast.Fn_log, [ b; x ] ->
    Some
      (match to_float_opt b, to_float_opt x with
       | Some bf, Some xf -> Row.V_real (Float.log xf /. Float.log bf)
       | _ -> Row.V_null)
  | Ast.Fn_log2, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.log f /. Float.log 2.0)
       | None -> Row.V_null)
  | Ast.Fn_log10, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.log10 f)
       | None -> Row.V_null)
  | Ast.Fn_sign, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_int (if f > 0.0 then 1L else if f < 0.0 then -1L else 0L)
       | None -> Row.V_null)
  | Ast.Fn_trunc, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (if f >= 0.0 then Float.floor f else Float.ceil f)
       | None -> Row.V_null)
  | Ast.Fn_trunc, [ v; d ] ->
    Some
      (match to_float_opt v, to_float_opt d with
       | Some f, Some df ->
         let factor = 10.0 ** Float.round df in
         let fx = f *. factor in
         Row.V_real ((if fx >= 0.0 then Float.floor fx else Float.ceil fx) /. factor)
       | _ -> Row.V_null)
  | Ast.Fn_pi, [] -> Some (Row.V_real Float.pi)
  | Ast.Fn_sin, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.sin f)
       | None -> Row.V_null)
  | Ast.Fn_cos, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.cos f)
       | None -> Row.V_null)
  | Ast.Fn_tan, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.tan f)
       | None -> Row.V_null)
  | Ast.Fn_asin, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.asin f)
       | None -> Row.V_null)
  | Ast.Fn_acos, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.acos f)
       | None -> Row.V_null)
  | Ast.Fn_atan, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (Float.atan f)
       | None -> Row.V_null)
  | Ast.Fn_atan2, [ y; x ] ->
    Some
      (match to_float_opt y, to_float_opt x with
       | Some yf, Some xf -> Row.V_real (Float.atan2 yf xf)
       | _ -> Row.V_null)
  | Ast.Fn_degrees, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (f *. 180.0 /. Float.pi)
       | None -> Row.V_null)
  | Ast.Fn_radians, [ v ] ->
    Some
      (match to_float_opt v with
       | Some f -> Row.V_real (f *. Float.pi /. 180.0)
       | None -> Row.V_null)
  | _ -> None
;;

(* date/time/datetime/julianday/unixepoch share arg-shape handling; only the
   final conversion differs. *)
let eval_datetime_unary clock args (conv : Datetime.dt -> Row.value) : Row.value =
  match args with
  | [] | [ Row.V_null ] -> Row.V_null
  | Row.V_null :: _ -> Row.V_null
  | Row.V_text ts :: rest ->
    if rest <> []
    then Row.V_null
    else (
      match Datetime.parse ?now:clock ts with
      | Error _ -> Row.V_null
      | Ok dt -> conv dt)
  | _ -> Row.V_null
;;

let eval_datetime_func clock (func : Ast.scalar_func) (args : Row.value list)
  : Row.value option
  =
  match func with
  | Ast.Fn_date ->
    Some (eval_datetime_unary clock args (fun dt -> Row.V_text (Datetime.to_date dt)))
  | Ast.Fn_time ->
    Some (eval_datetime_unary clock args (fun dt -> Row.V_text (Datetime.to_time dt)))
  | Ast.Fn_datetime ->
    Some (eval_datetime_unary clock args (fun dt -> Row.V_text (Datetime.to_datetime dt)))
  | Ast.Fn_julianday ->
    Some
      (eval_datetime_unary clock args (fun dt -> Row.V_real (Datetime.to_julianday dt)))
  | Ast.Fn_unixepoch ->
    Some (eval_datetime_unary clock args (fun dt -> Row.V_int (Datetime.to_unixepoch dt)))
  | Ast.Fn_strftime ->
    Some
      (match args with
       | Row.V_text fmt :: Row.V_text ts :: rest ->
         if rest <> []
         then Row.V_null
         else (
           match Datetime.parse ?now:clock ts with
           | Error _ -> Row.V_null
           | Ok dt -> Row.V_text (Datetime.strftime fmt dt))
       | _ -> Row.V_null)
  | _ -> None
;;

(* json_set/insert/replace differ only in the per-path Json.path_* operation. *)
let json_modify (path_op : Json.value -> string -> Json.value -> Json.value) json_v rest
  : Row.value
  =
  let json_s =
    match json_v with
    | Row.V_text s -> s
    | _ -> ""
  in
  match Json.parse json_s with
  | Error _ -> Row.V_null
  | Ok jv ->
    let rec apply jv = function
      | path_v :: val_v :: rest ->
        let path =
          match path_v with
          | Row.V_text s -> s
          | _ -> ""
        in
        apply (path_op jv path (json_of_sql val_v)) rest
      | _ -> jv
    in
    Row.V_text (Json.to_string (apply jv rest))
;;

let eval_json_func (func : Ast.scalar_func) (args : Row.value list) : Row.value option =
  match func, args with
  | Ast.Fn_json_extract, [ json_v; path_v ] ->
    let json_s =
      match json_v with
      | Row.V_text s -> s
      | _ -> ""
    in
    let path_s =
      match path_v with
      | Row.V_text s -> s
      | _ -> ""
    in
    Some
      (match Json.parse json_s with
       | Error _ -> Row.V_null
       | Ok jv ->
         (match Json.path_get jv path_s with
          | None -> Row.V_null
          | Some v -> sql_of_json v))
  | Ast.Fn_json_object, pairs ->
    if List.length pairs mod 2 <> 0
    then Some Row.V_null
    else (
      let rec make_pairs = function
        | [] -> []
        | k :: v :: rest ->
          let key =
            match k with
            | Row.V_text s -> s
            | _ -> ""
          in
          (key, json_of_sql v) :: make_pairs rest
        | [ _ ] -> assert false
      in
      Some (Row.V_text (Json.to_string (Json.J_object (make_pairs pairs)))))
  | Ast.Fn_json_array, elems ->
    Some (Row.V_text (Json.to_string (Json.J_array (List.map json_of_sql elems))))
  | Ast.Fn_json_type, [ json_v ] ->
    Some
      (match json_v with
       | Row.V_text s ->
         (match Json.parse s with
          | Error _ -> Row.V_null
          | Ok jv -> Row.V_text (Json.type_name jv))
       | _ -> Row.V_null)
  | Ast.Fn_json_type, [ json_v; path_v ] ->
    Some
      (match json_v, path_v with
       | Row.V_text s, Row.V_text path ->
         (match Json.parse s with
          | Error _ -> Row.V_null
          | Ok jv ->
            (match Json.path_get jv path with
             | None -> Row.V_null
             | Some sub -> Row.V_text (Json.type_name sub)))
       | _ -> Row.V_null)
  | Ast.Fn_json_valid, [ json_v ] ->
    Some
      (match json_v with
       | Row.V_null -> Row.V_null
       | Row.V_text s ->
         (match Json.parse s with
          | Ok _ -> Row.V_int 1L
          | Error _ -> Row.V_int 0L)
       | _ -> Row.V_int 0L)
  | Ast.Fn_json_set, json_v :: rest -> Some (json_modify Json.path_set json_v rest)
  | Ast.Fn_json_insert, json_v :: rest -> Some (json_modify Json.path_insert json_v rest)
  | Ast.Fn_json_replace, json_v :: rest ->
    Some (json_modify Json.path_replace json_v rest)
  | Ast.Fn_json_remove, json_v :: paths ->
    let json_s =
      match json_v with
      | Row.V_text s -> s
      | _ -> ""
    in
    Some
      (match Json.parse json_s with
       | Error _ -> Row.V_null
       | Ok jv ->
         let result =
           List.fold_left
             (fun acc path_v ->
                let path =
                  match path_v with
                  | Row.V_text s -> s
                  | _ -> ""
                in
                Json.path_remove acc path)
             jv
             paths
         in
         Row.V_text (Json.to_string result))
  | _ -> None
;;

let eval_misc_func (func : Ast.scalar_func) (args : Row.value list) : Row.value option =
  match func, args with
  | Ast.Fn_coalesce, vs ->
    Some
      (match List.find_opt (fun v -> v <> Row.V_null) vs with
       | Some v -> v
       | None -> Row.V_null)
  | Ast.Fn_ifnull, [ a; b ] ->
    Some
      (match a with
       | Row.V_null -> b
       | v -> v)
  | Ast.Fn_typeof, [ v ] ->
    Some
      (Row.V_text
         (match v with
          | Row.V_int _ -> "integer"
          | Row.V_real _ -> "real"
          | Row.V_text _ -> "text"
          | Row.V_blob _ -> "blob"
          | Row.V_null -> "null"))
  | Ast.Fn_zeroblob, [ Row.V_int n ] when n >= 0L ->
    Some (Row.V_blob (Bytes.make (Int64.to_int n) '\000'))
  | Ast.Fn_zeroblob, _ -> Some Row.V_null
  | Ast.Fn_random, [] ->
    let b0 = Int64.of_int (Random.bits ()) in
    let b1 = Int64.of_int (Random.bits ()) in
    let b2 = Int64.of_int (Random.bits ()) in
    let sign = if Random.bool () then Int64.min_int else 0L in
    let v =
      Int64.logor
        sign
        (Int64.logor (Int64.shift_left b2 60) (Int64.logor (Int64.shift_left b1 30) b0))
    in
    Some (Row.V_int v)
  | Ast.Fn_random, _ -> Some Row.V_null
  (* SQLite always generates at least 1 byte, even for n <= 0.
     Clamp to [1, Sys.max_string_length] to avoid allocation errors. *)
  | Ast.Fn_randomblob, [ Row.V_int n ] ->
    let sz =
      max
        1
        (if n < 0L || n > Int64.of_int Sys.max_string_length then 1 else Int64.to_int n)
    in
    Some (Row.V_blob (Bytes.init sz (fun _ -> Char.chr (Random.int 256))))
  | Ast.Fn_randomblob, _ -> Some Row.V_null
  | Ast.Fn_changes, [] -> Some (Row.V_int 0L)
  | Ast.Fn_changes, _ -> Some Row.V_null
  | Ast.Fn_last_insert_rowid, [] -> Some (Row.V_int 0L)
  | Ast.Fn_last_insert_rowid, _ -> Some Row.V_null
  | Ast.Fn_total_changes, [] -> Some (Row.V_int 0L)
  | Ast.Fn_total_changes, _ -> Some Row.V_null
  | Ast.Fn_sqlite_version, [] -> Some (Row.V_text "3.45.0-granary")
  | Ast.Fn_sqlite_version, _ -> Some Row.V_null
  | _ -> None
;;

(* CAST evaluation; [v] is the already-evaluated operand. NULL casts to NULL. *)
let eval_cast (v : Row.value) (ty : Ast.ty) : Row.value =
  match v with
  | Row.V_null -> Row.V_null
  | _ ->
    (match ty with
     | Ast.Ty_int ->
       (match v with
        | Row.V_int n -> Row.V_int n
        | Row.V_real f -> Row.V_int (Int64.of_float f)
        | Row.V_text s -> Row.V_int (parse_int_prefix s)
        | Row.V_blob _ -> Row.V_int 0L
        | Row.V_null -> assert false)
     | Ast.Ty_real ->
       (match v with
        | Row.V_int n -> Row.V_real (Int64.to_float n)
        | Row.V_real f -> Row.V_real f
        | Row.V_text s -> Row.V_real (parse_real_prefix s)
        | Row.V_blob _ -> Row.V_real 0.0
        | Row.V_null -> assert false)
     | Ast.Ty_text ->
       (match v with
        | Row.V_int n -> Row.V_text (Int64.to_string n)
        | Row.V_real f ->
          (* SQLite appends ".0" when the %.15g result has no decimal point
             or exponent, so that CAST(1.0 AS TEXT) → "1.0" not "1". *)
          let s = Printf.sprintf "%.15g" f in
          let needs_dot =
            not
              (String.contains s '.'
               || String.contains s 'e'
               || String.contains s 'E'
               || String.contains s 'n')
          in
          Row.V_text (if needs_dot then s ^ ".0" else s)
        | Row.V_text s -> Row.V_text s
        | Row.V_blob b -> Row.V_text (Bytes.to_string b)
        | Row.V_null -> assert false)
     | Ast.Ty_blob ->
       (match v with
        | Row.V_blob b -> Row.V_blob b
        | Row.V_text s -> Row.V_blob (Bytes.of_string s)
        | Row.V_int n -> Row.V_blob (Bytes.of_string (Int64.to_string n))
        | Row.V_real f -> Row.V_blob (Bytes.of_string (Printf.sprintf "%.15g" f))
        | Row.V_null -> assert false))
;;

(* Bitwise binops: result is NULL unless both operands are integers. *)
let int_bitop lv rv f =
  match lv, rv with
  | Row.V_int a, Row.V_int b -> Row.V_int (f a b)
  | _ -> Row.V_null
;;

(* ------------------------------------------------------------------ *)
(* #722: COLLATE is a comparison attribute, not a value transform.      *)
(* ------------------------------------------------------------------ *)

(* The comparison KEY of [v] under [c].  A key is only ever compared, never
   emitted, so folding case here cannot change what a projection returns —
   which is the whole of #722.  [Collate_binary] and [Collate_rtrim] are the
   identity, exactly as they were before. *)
let collate_key (c : Ast.collation) (v : Row.value) : Row.value =
  match c, v with
  | Ast.Collate_nocase, Row.V_text s -> Row.V_text (String.lowercase_ascii s)
  | _, v -> v
;;

(* The collation governing a comparison whose operand is [e].

   SQLite's rule is that an operand has an explicit collating-function
   assignment if ANY subexpression of it uses the postfix COLLATE operator, and
   the leftmost such assignment wins.  So
   [(x COLLATE NOCASE) || '!' = 'HELLO!'] compares under NOCASE even though the
   COLLATE sits two nodes down — oracle-checked, sqlite3 answers both the
   'HELLO' and the 'Hello' row where granary answered neither.

   A subquery ([P_subquery] / [P_exists], and the statement half of
   [P_in_select]) is an opaque leaf here: its result column's collation is a
   property of the inner SELECT, not of this expression, and none of the four
   walkers over [Plan.expr] descends into an [Ast.stmt] either. *)
let rec expr_collation (e : Plan.expr) : Ast.collation =
  match e with
  | Plan.P_collate (_, c) -> c
  | Plan.P_lit _ | Plan.P_col _ | Plan.P_param _ -> Ast.Collate_binary
  | Plan.P_subquery _ | Plan.P_exists _ -> Ast.Collate_binary
  | Plan.P_excluded_col _ | Plan.P_window_slot _ -> Ast.Collate_binary
  | Plan.P_not e | Plan.P_is_null e | Plan.P_is_not_null e -> expr_collation e
  | Plan.P_neg e | Plan.P_bitnot e | Plan.P_cast (e, _) -> expr_collation e
  | Plan.P_in_select (x, _) -> expr_collation x
  | Plan.P_binop (_, l, r) -> collation2 l r
  | Plan.P_between (x, lo, hi) -> collation3 x lo hi
  | Plan.P_in (x, vals) -> collation_in x vals
  | Plan.P_func (_, args) -> collation_of_list args
  | Plan.P_case { scrutinee; branches; else_ } ->
    (match collation_opt scrutinee with
     | Ast.Collate_binary ->
       (match collation_of_branches branches with
        (* every subexpression *)
        | Ast.Collate_binary -> collation_opt else_
        | c -> c)
     | c -> c)

and collation_opt (e : Plan.expr option) : Ast.collation =
  match e with
  | None -> Ast.Collate_binary
  | Some e -> expr_collation e

(* Every subexpression of a CASE's branches — what {!expr_collation} needs when
   the whole CASE is an operand of an outer comparison. *)
and collation_of_branches (bs : (Plan.expr * Plan.expr) list) : Ast.collation =
  match bs with
  | [] -> Ast.Collate_binary
  | (cond, result) :: rest ->
    (match collation2 cond result with
     | Ast.Collate_binary -> collation_of_branches rest
     | c -> c)

(* Only the WHEN conditions — the comparison a [CASE x WHEN w] performs is
   [x = w], so a THEN result is not an operand of it.  Scanned in place rather
   than through [List.map fst] because this is reached per ROW. *)
and collation_of_conds (bs : (Plan.expr * Plan.expr) list) : Ast.collation =
  match bs with
  | [] -> Ast.Collate_binary
  | (cond, _) :: rest ->
    (match expr_collation cond with
     | Ast.Collate_binary -> collation_of_conds rest
     | c -> c)

(* Two operands, leftmost explicit assignment wins.  Spelled out rather than
   going through {!collation_of_list} because this runs per ROW for every
   comparison in every WHERE clause: a list and a thunk per evaluation would be
   a real allocation on the TPC-C path, where the answer is [Collate_binary]
   after two constructor matches. *)
and collation2 (a : Plan.expr) (b : Plan.expr) : Ast.collation =
  match expr_collation a with
  | Ast.Collate_binary -> expr_collation b
  | c -> c

and collation3 (a : Plan.expr) (b : Plan.expr) (c : Plan.expr) : Ast.collation =
  match collation2 a b with
  | Ast.Collate_binary -> expr_collation c
  | x -> x

and collation_in (x : Plan.expr) (vals : Plan.expr list) : Ast.collation =
  match expr_collation x with
  | Ast.Collate_binary -> collation_of_list vals
  | c -> c

and collation_of_list (es : Plan.expr list) : Ast.collation =
  match es with
  | [] -> Ast.Collate_binary
  | e :: rest ->
    (match expr_collation e with
     | Ast.Collate_binary -> collation_of_list rest
     | c -> c)
;;

(* [compare_values] under a collation: both sides are keyed, so the comparison
   is collation-aware while the values the caller keeps are untouched. *)
let compare_collated (c : Ast.collation) (a : Row.value) (b : Row.value) : int =
  compare_values (collate_key c a) (collate_key c b)
;;

(* An aggregate's comparison collation comes from its argument expression.  The
   bare-column forms carry [arg_expr = None] and are always BINARY, which is
   what they were before #722. *)
let agg_spec_collation (spec : Plan.agg_spec) : Ast.collation =
  match spec.Plan.arg_expr with
  | Some e -> expr_collation e
  | None -> Ast.Collate_binary
;;

(* Per-output-column collation of a row-producing plan op.

   #722: DISTINCT and the three set operations compare the OUTPUT row and hold
   no expressions of their own — [Op_distinct] is literally [{ child : op }] —
   so the collation has to be read back off the projection underneath them.
   A column this cannot resolve answers [Collate_binary], which is what every
   column answered before #722 unless the projection happened to lower-case it. *)
let rec output_collations (op : Plan.op) : Ast.collation list =
  match op with
  | Plan.Op_expr_project { exprs; _ } | Plan.Op_const_select { exprs } ->
    List.map (fun (e, _) -> expr_collation e) exprs
  | Plan.Op_project { ordinals; child } ->
    let inner = output_collations child in
    let at i = Option.value (List.nth_opt inner i) ~default:Ast.Collate_binary in
    List.map at ordinals
  | Plan.Op_aggregate { proj; aggs; _ } -> List.map (proj_item_collation aggs) proj
  | Plan.Op_sort { child; _ }
  | Plan.Op_limit { child; _ }
  | Plan.Op_filter { child; _ }
  | Plan.Op_distinct { child } -> output_collations child
  | Plan.Op_union { left; _ } | Plan.Op_intersect { left; _ } | Plan.Op_except { left; _ }
    -> output_collations left
  | _ -> []

and proj_item_collation (aggs : Plan.agg_spec list) (pi : Plan.proj_item) : Ast.collation =
  match pi with
  | Plan.PI_expr e -> expr_collation e
  | Plan.PI_agg_slot k ->
    (match List.nth_opt aggs k with
     | Some spec -> agg_spec_collation spec
     | None -> Ast.Collate_binary)
  | Plan.PI_group_col _ | Plan.PI_window_slot _ -> Ast.Collate_binary
;;

(* The dedup-key function for rows whose columns carry [cols].  When every
   column is BINARY — the overwhelmingly common case, and every case before
   #722 — this IS [row_key], so DISTINCT and the set operations pay nothing at
   all for the feature; the all-binary test is made once per operator rather
   than once per row. *)
let collated_row_keyer (cols : Ast.collation list) : Row.t -> string =
  if List.for_all (fun c -> c = Ast.Collate_binary) cols
  then row_key
  else (
    let arr = Array.of_list cols in
    let at i = if i < Array.length arr then arr.(i) else Ast.Collate_binary in
    fun row -> row_key (Array.mapi (fun i v -> collate_key (at i) v) row))
;;

(* Only a COMPARISON takes a collation from its operands; every other binop is
   a value computation and must not fold.  Before #722 the fold ran for EVERY
   operator, so [(x COLLATE NOCASE) || 'B'] answered ['hellob'] where sqlite3
   answers ['HELLOB'].

   [Like] is excluded because {!like_match} already lower-cases both sides, and
   [Glob] because sqlite3's GLOB is case-sensitive regardless of collation
   (oracle-checked: [x COLLATE NOCASE GLOB 'HELL*'] matches only ['HELLO']). *)
let binop_takes_collation (op : Plan.binop) : bool =
  match op with
  | Plan.Eq | Plan.Ne | Plan.Lt | Plan.Le | Plan.Gt | Plan.Ge -> true
  | Plan.Like | Plan.Glob -> false
  | Plan.Add | Plan.Sub | Plan.Mul | Plan.Div | Plan.Mod -> false
  | Plan.And | Plan.Or | Plan.Concat -> false
  | Plan.Bit_and | Plan.Bit_or | Plan.Lshift | Plan.Rshift -> false
;;

let rec eval_expr
          (clock : (unit -> float) option)
          (params : Row.value array)
          (row : Row.t)
          (e : Plan.expr)
  : Row.value
  =
  match e with
  | Plan.P_lit l -> lit_to_value l
  | Plan.P_col i -> row.(i)
  | Plan.P_param i -> if i < Array.length params then params.(i) else Row.V_null
  | Plan.P_neg e ->
    (match eval_expr clock params row e with
     | Row.V_int n -> Row.V_int (Int64.neg n)
     | Row.V_real f -> Row.V_real (-.f)
     | Row.V_null -> Row.V_null
     | _ -> failwith "unary minus requires numeric operand")
  | Plan.P_bitnot e ->
    (match eval_expr clock params row e with
     | Row.V_int n -> Row.V_int (Int64.lognot n)
     | Row.V_null -> Row.V_null
     | _ -> Row.V_null)
  (* #522: [x BETWEEN lo AND hi] is evaluated as literally [x >= lo AND x <= hi]
     — same operators, same three-valued AND — so the two spellings cannot
     disagree. Comparing through [compare_values] instead used to answer 0 for
     any cross-type pair, which made both ends true at once and the whole
     predicate true for every row. [x] is still evaluated once. *)
  | Plan.P_between (x, lo, hi) ->
    (* #722: BETWEEN reached [eval_binop] directly, bypassing the collation
       propagation the [P_binop] arm did, so [x COLLATE NOCASE BETWEEN a AND b]
       folded [x] but neither bound and answered NO rows where sqlite3 answers
       two.  Both ends are now keyed with the same collation as [x]. *)
    let c = collation3 x lo hi in
    let vx = collate_key c (eval_expr clock params row x) in
    let vlo = collate_key c (eval_expr clock params row lo) in
    let vhi = collate_key c (eval_expr clock params row hi) in
    eval_binop Plan.And (eval_binop Plan.Ge vx vlo) (eval_binop Plan.Le vx vhi)
  | Plan.P_in (x, vals) -> eval_in clock params row x vals
  | Plan.P_is_null e ->
    (match eval_expr clock params row e with
     | Row.V_null -> Row.V_int 1L
     | _ -> Row.V_int 0L)
  | Plan.P_is_not_null e ->
    (match eval_expr clock params row e with
     | Row.V_null -> Row.V_int 0L
     | _ -> Row.V_int 1L)
  | Plan.P_not e ->
    (match eval_expr clock params row e with
     | Row.V_null -> Row.V_null
     | v -> if value_truthy v then Row.V_int 0L else Row.V_int 1L)
  (* #722: the collation is read off the OPERAND EXPRESSIONS and applied to
     both comparison keys.  Before, it was applied by [P_collate] to its own
     value and to the other operand, which made a collated value observable
     wherever it was not compared. *)
  | Plan.P_binop (op, lhs_e, rhs_e) ->
    let lv = eval_expr clock params row lhs_e in
    let rv = eval_expr clock params row rhs_e in
    if not (binop_takes_collation op)
    then eval_binop op lv rv
    else (
      match collation2 lhs_e rhs_e with
      | Ast.Collate_binary -> eval_binop op lv rv
      | c -> eval_binop op (collate_key c lv) (collate_key c rv))
  | Plan.P_func (func, args) ->
    eval_func clock func (List.map (eval_expr clock params row) args)
  | Plan.P_case { scrutinee; branches; else_ } ->
    eval_case_expr clock params row scrutinee branches else_
  | Plan.P_cast (e, ty) -> eval_cast (eval_expr clock params row e) ty
  | Plan.P_subquery _ | Plan.P_exists _ | Plan.P_in_select _ ->
    (* These are replaced by pre_eval_subquery before row evaluation. *)
    Row.V_null
  | Plan.P_excluded_col _ ->
    failwith "Exec: P_excluded_col in eval_expr — must be substituted before evaluation"
  | Plan.P_window_slot _ ->
    failwith
      "Exec: P_window_slot in eval_expr — must be substituted by planner before \
       evaluation"
  (* #722: a COLLATE never changes the VALUE.  It is read by the comparison
     sites through {!expr_collation}; here it is the identity, so a projected
     or concatenated or CAST-wrapped collated column returns what is stored. *)
  | Plan.P_collate (e, _) -> eval_expr clock params row e

and eval_in clock params row x vals =
  let vx_raw = eval_expr clock params row x in
  if vx_raw = Row.V_null
  then Row.V_null
  else (
    (* #722: [IN] is a disjunction of equalities, so it takes a collation from
       its operands the way [=] does. *)
    let c = collation_in x vals in
    let vx = collate_key c vx_raw in
    let result =
      List.fold_left
        (fun acc ve ->
           let v = eval_expr clock params row ve in
           match acc with
           | `Found -> `Found
           | _ when v = Row.V_null -> `Maybe
           | _ when compare_values vx (collate_key c v) = 0 -> `Found
           | acc -> acc)
        `Not_found
        vals
    in
    match result with
    | `Found -> Row.V_int 1L
    | `Maybe -> Row.V_null
    | `Not_found -> Row.V_int 0L)

and eval_case_expr clock params row scrutinee branches else_ =
  (* #722: [CASE x WHEN v THEN ...] is an equality against [x], so it takes a
     collation from the scrutinee and the branch conditions. *)
  let c =
    match collation_opt scrutinee with
    | Ast.Collate_binary -> collation_of_conds branches
    | c -> c
  in
  let scr_val =
    Option.map (fun e -> collate_key c (eval_expr clock params row e)) scrutinee
  in
  let rec find_match = function
    | [] ->
      (match else_ with
       | None -> Row.V_null
       | Some e -> eval_expr clock params row e)
    | (cond, result) :: rest ->
      let matched =
        match scr_val with
        | None -> value_truthy (eval_expr clock params row cond)
        | Some sv ->
          let cv = collate_key c (eval_expr clock params row cond) in
          (match sv, cv with
           | Row.V_null, _ | _, Row.V_null -> false
           | _ -> compare_values sv cv = 0)
      in
      if matched then eval_expr clock params row result else find_match rest
  in
  find_match branches

(* #722: a sort / partition / peer-boundary key is a COMPARISON key — it is
   never emitted — so the key expression's collation is applied to it here.
   Before #722 the same effect fell out of [P_collate] rewriting the value,
   which is why ORDER BY ... COLLATE NOCASE worked while a projection of the
   same expression did not. *)
and eval_sort_key clock params row (e : Plan.expr) : Row.value =
  collate_key (expr_collation e) (eval_expr clock params row e)

and eval_func
      (clock : (unit -> float) option)
      (func : Ast.scalar_func)
      (args : Row.value list)
  : Row.value
  =
  match eval_str_func func args with
  | Some v -> v
  | None ->
    (match eval_math_func func args with
     | Some v -> v
     | None ->
       (match eval_datetime_func clock func args with
        | Some v -> v
        | None ->
          (match eval_json_func func args with
           | Some v -> v
           | None ->
             (match eval_misc_func func args with
              | Some v -> v
              | None ->
                failwith
                  "scalar_func: unexpected argument count (arity check should have \
                   caught this)"))))

and eval_binop (op : Plan.binop) (lv : Row.value) (rv : Row.value) : Row.value =
  match op with
  | Plan.And ->
    let lt = value_truthy lv
    and rt = value_truthy rv in
    let ln = lv = Row.V_null
    and rn = rv = Row.V_null in
    if lt && rt
    then Row.V_int 1L
    else if ((not ln) && not lt) || ((not rn) && not rt)
    then Row.V_int 0L
    else Row.V_null
  | Plan.Or ->
    let lt = value_truthy lv
    and rt = value_truthy rv in
    let ln = lv = Row.V_null
    and rn = rv = Row.V_null in
    if lt || rt
    then Row.V_int 1L
    else if (not ln) && not rn
    then Row.V_int 0L
    else Row.V_null
  (* NULL compared with anything yields NULL (3-valued logic), which
     {!cmp_result}'s own first arm supplies.

     #738: [Eq] and [Ne] go through {!cmp_result} like the four ordering
     operators, so ALL SIX comparisons are {!compare_values} and there is no
     longer a comparator that answers [=] one way and [<=]/[>=] another.  Before
     it they enumerated the four same-type pairs and fell to a catch-all, so
     [1 = 1.0] and [1 <> 1.0] were BOTH false while [1 <= 1.0] and [1 >= 1.0]
     were both true.

     {b The three sites this rests on must never move apart again.}  Unlike a
     range conjunct, an EQUALITY conjunct IS consumed by the access path
     ([Planner.recognise_eq_col_lit] -> [access_path_for_eqs], whose [consumed]
     positions [residual_filter] removes), so no residual re-checks a seek's
     output.  Making this arm exact therefore required
     {!index_lookup_values}, {!stream_rowid_lookup} and {!seek_candidates}'s
     [Seek_rowid] arm to learn the same cross-numeric equality in the same
     change — otherwise [WHERE i = 1.0] on an indexed INTEGER column returns
     nothing while the predicate says the row qualifies.  Rows lost silently is
     the failure mode; see #738. *)
  | Plan.Eq -> cmp_result lv rv (fun c -> c = 0)
  | Plan.Ne -> cmp_result lv rv (fun c -> c <> 0)
  | Plan.Lt -> cmp_result lv rv (fun c -> c < 0)
  | Plan.Le -> cmp_result lv rv (fun c -> c <= 0)
  | Plan.Gt -> cmp_result lv rv (fun c -> c > 0)
  | Plan.Ge -> cmp_result lv rv (fun c -> c >= 0)
  | Plan.Add -> arith_op lv rv Int64.add ( +. )
  | Plan.Sub -> arith_op lv rv Int64.sub ( -. )
  | Plan.Mul -> arith_op lv rv Int64.mul ( *. )
  | Plan.Div ->
    arith_op
      lv
      rv
      (fun a b -> if Int64.equal b 0L then failwith "division by zero" else Int64.div a b)
      ( /. )
  | Plan.Concat ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_text a, Row.V_text b -> Row.V_text (a ^ b)
     | Row.V_text a, Row.V_int n -> Row.V_text (a ^ Int64.to_string n)
     | Row.V_int n, Row.V_text b -> Row.V_text (Int64.to_string n ^ b)
     | Row.V_int a, Row.V_int b -> Row.V_text (Int64.to_string a ^ Int64.to_string b)
     | _ -> Row.V_null)
  | Plan.Mod ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_int a, Row.V_int b ->
       if b = 0L then Row.V_null else Row.V_int (Int64.rem a b)
     | Row.V_real a, Row.V_real b ->
       if b = 0.0 then Row.V_null else Row.V_real (mod_float a b)
     | Row.V_int a, Row.V_real b ->
       if b = 0.0 then Row.V_null else Row.V_real (mod_float (Int64.to_float a) b)
     | Row.V_real a, Row.V_int b ->
       if b = 0L then Row.V_null else Row.V_real (mod_float a (Int64.to_float b))
     | _ -> Row.V_null)
  | Plan.Bit_and -> int_bitop lv rv Int64.logand
  | Plan.Bit_or -> int_bitop lv rv Int64.logor
  | Plan.Lshift ->
    int_bitop lv rv (fun a b ->
      let n = Int64.to_int b in
      if n < 0 || n >= 64 then 0L else Int64.shift_left a n)
  | Plan.Rshift ->
    int_bitop lv rv (fun a b ->
      let n = Int64.to_int b in
      if n < 0 || n >= 64 then 0L else Int64.shift_right a n)
  | Plan.Like ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_text str, Row.V_text pat ->
       Row.V_int
         (if like_match (String.lowercase_ascii pat) 0 (String.lowercase_ascii str) 0
          then 1L
          else 0L)
     | _ -> Row.V_null)
  | Plan.Glob ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_text str, Row.V_text pat ->
       Row.V_int (if glob_match pat 0 str 0 then 1L else 0L)
     | _ -> Row.V_null)

(* #733/#734/#738: a WHERE predicate's comparison — all six of [=], [<>], [<],
   [<=], [>], [>=] — is {!compare_values} with three-valued logic layered on
   top, and nothing else.

   It used to be a third comparator with its own two disagreements:

   - it promoted int-vs-real through [Int64.to_float], so above 2^53 the
     PREDICATE answered equal for pairs the ORDERING separated (#733).
     sqlite3 answers 1 for [9007199254740993 > 9007199254740992.0]; this
     answered 0.
   - it ended in [| _ -> Row.V_int 0L], applying no cross-CLASS order at all,
     so [ORDER BY] said [5 < 'abc'] while [WHERE] said that was false
     (#734).  sqlite3 answers 1, and NUMBER < TEXT < BLOB is exactly
     {!value_class_rank}'s order and exactly
     {!Granary_encoding.Index_key.encode_value}'s tag-byte order — so
     [cmp_result] was the wrong half of the disagreement, not [compare_values].

   NULL stays a separate arm above the delegation and must: [compare_values]
   ORDERS NULL below everything (it is a total order, so it has to answer
   something), whereas a predicate over a NULL is UNKNOWN.  Routing NULL
   through it would make [WHERE x < 5] true for a NULL [x].

   {b The index path is unaffected and the reason is worth keeping.}
   {!range_bound_key}'s [pred]/[succ] widening was written to compensate for
   the inexact promotion this removes, so the seek is now WIDER than the
   predicate needs rather than exactly as wide.  That is the safe direction:
   {!Granary_sql.Planner.range_for_index} never marks a range conjunct
   consumed, so a residual filter runs over every row a seek yields.  See
   {!range_bound_key}'s own comment for the full argument.

   #738 routed [Eq] and [Ne] through here too.  They used to enumerate the four
   same-type pairs and fall to a catch-all, so [1 = 1.0] and [1 <> 1.0] were
   both false while [1 <= 1.0] and [1 >= 1.0] were both true.  That could not be
   fixed in {!eval_binop} alone: an equality conjunct IS consumed by the access
   path, so {!index_lookup_values}, {!stream_rowid_lookup} and
   {!seek_candidates}'s [Seek_rowid] arm learned the cross-numeric case in the
   same change.  Do not move one of the four without the other three. *)
and cmp_result lv rv pred =
  match lv, rv with
  | Row.V_null, _ | _, Row.V_null -> Row.V_null
  | _, _ -> if pred (compare_values lv rv) then Row.V_int 1L else Row.V_int 0L

and arith_op lv rv int_f float_f =
  match lv, rv with
  | Row.V_null, _ | _, Row.V_null -> Row.V_null
  | Row.V_int a, Row.V_int b -> Row.V_int (int_f a b)
  | Row.V_real a, Row.V_real b -> Row.V_real (float_f a b)
  | Row.V_int a, Row.V_real b -> Row.V_real (float_f (Int64.to_float a) b)
  | Row.V_real a, Row.V_int b -> Row.V_real (float_f a (Int64.to_float b))
  | _ -> failwith "arithmetic on non-numeric operands"
;;

let project_row (ords : int list) (row : Row.t) : Row.t =
  Array.of_list (List.map (fun i -> row.(i)) ords)
;;

(* ------------------------------------------------------------------ *)
(* CHECK constraint evaluation                                          *)
(* ------------------------------------------------------------------ *)

let ast_binop_to_plan : Ast.binop -> Plan.binop = function
  | Ast.Eq -> Plan.Eq
  | Ast.Ne -> Plan.Ne
  | Ast.Lt -> Plan.Lt
  | Ast.Le -> Plan.Le
  | Ast.Gt -> Plan.Gt
  | Ast.Ge -> Plan.Ge
  | Ast.Add -> Plan.Add
  | Ast.Sub -> Plan.Sub
  | Ast.Mul -> Plan.Mul
  | Ast.Div -> Plan.Div
  | Ast.And -> Plan.And
  | Ast.Or -> Plan.Or
  | Ast.Concat -> Plan.Concat
  | Ast.Mod -> Plan.Mod
  | Ast.Bit_and -> Plan.Bit_and
  | Ast.Bit_or -> Plan.Bit_or
  | Ast.Lshift -> Plan.Lshift
  | Ast.Rshift -> Plan.Rshift
  | Ast.Like -> Plan.Like
  | Ast.Glob -> Plan.Glob
;;

let rec ast_expr_to_plan_check (columns : Row.column list) (e : Ast.expr) : Plan.expr =
  match e with
  | Ast.E_lit l -> Plan.P_lit l
  (* #744: this is the re-compiler for the SQL text the catalog stores about
     itself — a CHECK, a GENERATED expression, a partial index's WHERE — so it
     meets a bare [true]/[false] on every write once one is written down.  The
     same rule as [Sema]'s: the columns are consulted first, and the literal is
     the fallback for a name none of them answers to. *)
  | (Ast.E_col name | Ast.E_tbl_col (_, name))
    when (not
            (List.exists (fun (c : Row.column) -> String.equal c.Row.name name) columns))
         && Option.is_some (Ast.bool_ident_lit name) ->
    Plan.P_lit (Option.get (Ast.bool_ident_lit name))
  | Ast.E_col name -> Plan.P_col (find_col_idx_by_name columns name)
  | Ast.E_tbl_col (_, name) -> Plan.P_col (find_col_idx_by_name columns name)
  | Ast.E_binop (op, a, b) ->
    Plan.P_binop
      ( ast_binop_to_plan op
      , ast_expr_to_plan_check columns a
      , ast_expr_to_plan_check columns b )
  | Ast.E_not e -> Plan.P_not (ast_expr_to_plan_check columns e)
  | Ast.E_is_null e -> Plan.P_is_null (ast_expr_to_plan_check columns e)
  | Ast.E_is_not_null e -> Plan.P_is_not_null (ast_expr_to_plan_check columns e)
  | Ast.E_neg e -> Plan.P_neg (ast_expr_to_plan_check columns e)
  | Ast.E_bitnot e -> Plan.P_bitnot (ast_expr_to_plan_check columns e)
  | Ast.E_between (x, lo, hi) ->
    Plan.P_between
      ( ast_expr_to_plan_check columns x
      , ast_expr_to_plan_check columns lo
      , ast_expr_to_plan_check columns hi )
  | Ast.E_in (x, vals) ->
    Plan.P_in
      (ast_expr_to_plan_check columns x, List.map (ast_expr_to_plan_check columns) vals)
  | Ast.E_func (f, args) -> Plan.P_func (f, List.map (ast_expr_to_plan_check columns) args)
  | Ast.E_case { scrutinee; branches; else_ } ->
    let go = ast_expr_to_plan_check columns in
    Plan.P_case
      { scrutinee = Option.map go scrutinee
      ; branches = List.map (fun (c, r) -> go c, go r) branches
      ; else_ = Option.map go else_
      }
  | Ast.E_cast (e, ty) -> Plan.P_cast (ast_expr_to_plan_check columns e, ty)
  | Ast.E_collate (e, c) -> Plan.P_collate (ast_expr_to_plan_check columns e, c)
  | _ -> failwith "ast_expr_to_plan_check: unsupported expression in CHECK"
;;

let compile_check_expr
      (table_name : string)
      (col_idx : int)
      (columns : Row.column list)
      (check_sql : string)
  : Plan.expr
  =
  let key = table_name, col_idx, check_sql in
  match Hashtbl.find_opt check_expr_cache key with
  | Some e -> e
  | None ->
    let lexbuf = Lexing.from_string check_sql in
    let ast_expr =
      try Parser.expr_only Lexer.token lexbuf with
      | Parser.Error | Failure _ ->
        failwith
          (Printf.sprintf
             "CHECK constraint parse error for %s.col%d: %s"
             table_name
             col_idx
             check_sql)
    in
    let plan_expr = ast_expr_to_plan_check columns ast_expr in
    Hashtbl.add check_expr_cache key plan_expr;
    plan_expr
;;

(* Cache for compiled generated-column expressions.
   Key: (table_name, col_idx, expr_sql) — same three-part pattern as check_expr_cache.
   Schema changes invalidate entries via clear on DROP TABLE / DROP COLUMN. *)
let generated_expr_cache : (string * int * string, Plan.expr) Hashtbl.t = Hashtbl.create 8

let compile_generated_expr
      (table_name : string)
      (col_idx : int)
      (columns : Row.column list)
      (expr_sql : string)
  : Plan.expr
  =
  let key = table_name, col_idx, expr_sql in
  match Hashtbl.find_opt generated_expr_cache key with
  | Some e -> e
  | None ->
    let lexbuf = Lexing.from_string expr_sql in
    let ast_expr =
      try Parser.expr_only Lexer.token lexbuf with
      | Parser.Error | Failure _ ->
        failwith
          (Printf.sprintf
             "generated column expr parse error for %s.col%d: %s"
             table_name
             col_idx
             expr_sql)
    in
    let plan_expr = ast_expr_to_plan_check columns ast_expr in
    Hashtbl.add generated_expr_cache key plan_expr;
    plan_expr
;;

(** Compute STORED generated columns on the write path, in-place in [row].
    Iterates columns in schema order; earlier generated columns are available
    to later generated column expressions (in-order dependency). VIRTUAL
    generated columns are set to [V_null] in memory and on disk; they are
    recomputed on read via [compute_virtual_generated_cols]. *)
let compute_stored_generated_cols
      (clock : (unit -> float) option)
      (params : Row.value array)
      (meta : Cat.table_meta)
      (row : Row.t)
  : unit
  =
  (* #347: skip when no column is a STORED generated column — the common case.
     VIRTUAL columns stay at V_null (set by [build_insert_row]'s Array.make). *)
  if
    List.exists
      (fun (c : Row.column) ->
         match c.Row.generated_as with
         | Some (_, true) -> true
         | _ -> false)
      meta.Cat.columns
  then
    List.iteri
      (fun i (col : Row.column) ->
         match col.Row.generated_as with
         | None -> ()
         | Some (sql, true) ->
           let plan_e = compile_generated_expr meta.Cat.name i meta.Cat.columns sql in
           row.(i) <- eval_expr clock params row plan_e
         | Some (_, false) ->
           (* VIRTUAL: write NULL placeholder; recomputed on read. *)
           row.(i) <- Row.V_null)
      meta.Cat.columns
;;

(** Recompute VIRTUAL generated columns from the underlying row values.
    Invoked after [Row.decode] for table-row reads in [exec.ml]. *)
let compute_virtual_generated_cols
      (clock : (unit -> float) option)
      (params : Row.value array)
      (meta : Cat.table_meta)
      (row : Row.t)
  : unit
  =
  List.iteri
    (fun i (col : Row.column) ->
       match col.Row.generated_as with
       | Some (sql, false) ->
         let plan_e = compile_generated_expr meta.Cat.name i meta.Cat.columns sql in
         row.(i) <- eval_expr clock params row plan_e
       | _ -> ())
    meta.Cat.columns
;;

(** Like [compute_virtual_generated_cols] but driven by [(name, columns)]
    rather than a full [Cat.table_meta]. Used by call sites that only have
    a column list in scope (e.g., [execute_create_index]). *)
let compute_virtual_generated_cols_cols
      (clock : (unit -> float) option)
      (params : Row.value array)
      ~(table_name : string)
      (columns : Row.column list)
      (row : Row.t)
  : unit
  =
  List.iteri
    (fun i (col : Row.column) ->
       match col.Row.generated_as with
       | Some (sql, false) ->
         let plan_e = compile_generated_expr table_name i columns sql in
         row.(i) <- eval_expr clock params row plan_e
       | _ -> ())
    columns
;;

let has_virtual_cols (columns : Row.column list) : bool =
  List.exists
    (fun (c : Row.column) ->
       match c.Row.generated_as with
       | Some (_, false) -> true
       | _ -> false)
    columns
;;

(** [with_computed_virtuals]: return a copy of [row] with any VIRTUAL
    generated columns recomputed.  Used by the index-key extraction and
    CHECK-evaluation write paths so that VIRTUAL cells contribute the
    up-to-date value instead of [V_null].  Returns [row] unchanged when
    the table has no virtual columns (the common case). *)
let with_computed_virtuals
      (clock : (unit -> float) option)
      (params : Row.value array)
      (meta : Cat.table_meta)
      (row : Row.t)
  : Row.t
  =
  if not (has_virtual_cols meta.Cat.columns)
  then row
  else (
    let row' = Array.copy row in
    compute_virtual_generated_cols clock params meta row';
    row')
;;

(** [decode_with_virtual]: like [Row.decode], but also recomputes any VIRTUAL
    generated columns in the schema. Skips the recompute when the table has
    no virtual cols (the common case). *)
let decode_with_virtual
      (clock : (unit -> float) option)
      (params : Row.value array)
      (meta : Cat.table_meta)
      (bytes : bytes)
  : Row.t
  =
  let row = Row.decode meta.Cat.columns bytes in
  if has_virtual_cols meta.Cat.columns
  then compute_virtual_generated_cols clock params meta row;
  row
;;

(** Variant that takes a [(table_name, columns)] pair instead of a full meta. *)
let decode_with_virtual_cols
      (clock : (unit -> float) option)
      (params : Row.value array)
      ~(table_name : string)
      (columns : Row.column list)
      (bytes : bytes)
  : Row.t
  =
  let row = Row.decode columns bytes in
  if has_virtual_cols columns
  then compute_virtual_generated_cols_cols clock params ~table_name columns row;
  row
;;

let index_where_cache : (string * string * string * string, Plan.expr) Hashtbl.t =
  Hashtbl.create 8
;;

let compile_index_where (idx : Cat.index_info) (columns : Row.column list) : Plan.expr =
  match idx.idx_where_sql with
  | None -> failwith "compile_index_where: called on non-partial index"
  | Some sql ->
    let schema_sig = String.concat "," (List.map (fun c -> c.Row.name) columns) in
    let key = idx.idx_name, idx.idx_table, sql, schema_sig in
    (match Hashtbl.find_opt index_where_cache key with
     | Some e -> e
     | None ->
       let lexbuf = Lexing.from_string sql in
       let ast_expr =
         try Parser.expr_only Lexer.token lexbuf with
         | Parser.Error | Failure _ ->
           failwith (Printf.sprintf "index WHERE parse error for %s: %s" idx.idx_name sql)
       in
       let plan_expr = ast_expr_to_plan_check columns ast_expr in
       Hashtbl.add index_where_cache key plan_expr;
       plan_expr)
;;

let row_matches_index_where
      (clock : (unit -> float) option)
      (params : Row.value array)
      (idx : Cat.index_info)
      (schema : Row.column list)
      (row : Row.t)
  : bool
  =
  match idx.idx_where_sql with
  | None -> true
  | Some _ ->
    let plan_e = compile_index_where idx schema in
    value_truthy (eval_expr clock params row plan_e)
;;

(* Cache for compiled index column expressions.
   Key: (idx_name, idx_table, expr_sql, schema_sig) — four parts to prevent collisions. *)
let index_expr_cache : (string * string * string * string, Plan.expr) Hashtbl.t =
  Hashtbl.create 8
;;

let compile_index_col_expr (idx : Cat.index_info) (i : int) (columns : Row.column list)
  : Plan.expr
  =
  let expr_sql = List.nth idx.idx_columns i in
  let schema_sig = String.concat "," (List.map (fun c -> c.Row.name) columns) in
  let key = idx.idx_name, idx.idx_table, expr_sql, schema_sig in
  match Hashtbl.find_opt index_expr_cache key with
  | Some e -> e
  | None ->
    let lexbuf = Lexing.from_string expr_sql in
    let ast_expr =
      try Parser.expr_only Lexer.token lexbuf with
      | Parser.Error | Failure _ ->
        failwith
          (Printf.sprintf "index expr parse error for %s[%d]: %s" idx.idx_name i expr_sql)
    in
    let plan_e = ast_expr_to_plan_check columns ast_expr in
    Hashtbl.add index_expr_cache key plan_e;
    plan_e
;;

(** Evaluate all index key values for [row] against [idx].
    For expression-indexed columns, evaluates the compiled expression.
    For plain columns, fetches from the row by column ordinal. *)
let get_index_key_values
      (clock : (unit -> float) option)
      (params : Row.value array)
      (idx : Cat.index_info)
      (schema : Row.column list)
      (row : Row.t)
  : Row.value list
  =
  List.mapi
    (fun i col_sql ->
       let is_expr =
         if i < List.length idx.idx_expr_flags
         then List.nth idx.idx_expr_flags i
         else false
       in
       if is_expr
       then (
         let plan_e = compile_index_col_expr idx i schema in
         eval_expr clock params row plan_e)
       else (
         let col_idx = find_col_idx_by_name schema col_sql in
         row.(col_idx)))
    idx.idx_columns
;;

let eval_check_constraints
      (clock : (unit -> float) option)
      (params : Row.value array)
      (table_meta : Cat.table_meta)
      (row : Row.t)
  : unit
  =
  (* #347: skip entirely when no column carries a CHECK — the common case. *)
  if
    List.exists
      (fun (c : Row.column) -> Option.is_some c.check_sql)
      table_meta.Cat.columns
  then (
    (* Phase 35 Task 2: populate VIRTUAL generated columns into a scratch row
       before evaluating CHECKs, so checks that reference a VIRTUAL column see
       the up-to-date value instead of [V_null]. *)
    let row_for_check = with_computed_virtuals clock params table_meta row in
    List.iteri
      (fun i (col : Row.column) ->
         match col.check_sql with
         | None -> ()
         | Some check_sql ->
           let check_plan =
             compile_check_expr table_meta.name i table_meta.columns check_sql
           in
           let result = eval_expr clock params row_for_check check_plan in
           (* SQLite: NULL result -> passes (not a violation) *)
           if result <> Row.V_null && not (value_truthy result)
           then
             failwith
               (Printf.sprintf "CHECK constraint failed: %s.%s" table_meta.name col.name))
      table_meta.columns)
;;

(* #567: a column whose stored cell is allowed to be [V_null] even though the
   schema says NOT NULL.  VIRTUAL generated columns are stored as NULL and
   recomputed on read ([decode_with_virtual]), so their stored cell says
   nothing about the value the schema declares.

   #629: this is a fallback, not the answer.  "The stored cell says nothing"
   is a reason to look at the COMPUTED value, not a reason to stop checking —
   exempting outright made a NOT NULL VIRTUAL generated column unenforceable.
   [not_null_violation] now recomputes the virtual cells first whenever it can
   and consults this only when it cannot (a short row, i.e. one that does not
   cover every column, where evaluating the generated expression would raise).
   Do not re-broaden it: the exemption is the degraded mode. *)
let not_null_exempt_col (col : Row.column) : bool =
  match col.Row.generated_as with
  | Some (_, false) -> true (* VIRTUAL: not materialised in the stored row *)
  | _ -> false
;;

(* #567: the runtime half of NOT NULL enforcement.  [Sema] rejects a *literal*
   NULL assigned to a NOT NULL column, but a bound parameter, a NULL-valued
   expression ([v = NULL + 1]), a DEFAULT, a subquery or an FK cascade all
   reach the encoder untouched, and there was no check there — so the write
   succeeded and left a row the table's own rendered DDL refuses to restore
   (#548 makes [Db.dump] refuse the whole script over it).  Since #530/#533
   every PRIMARY KEY column carries [not_null = true], so a PK is reachable
   the same way.

   This runs where the row values are finally known, at the four places a row
   assembled from user input is committed to storage.  Enumerated, because
   "before every [Row.encode]" is not the right rule and stating it that way
   sent this fix past the columnstore once already:

   - [execute_insert_write] — INSERT, after [insert_rowid] has written the
     rowid-alias value back into the row;
   - [write_row_rekeyed] — UPDATE, UPSERT DO UPDATE and ON UPDATE CASCADE all
     funnel through it;
   - the two columnstore [Op_insert] / [Op_insert_select] arms, which hand the
     row array straight to [Col_store.insert_rows] and never encode at all.

   The third [Row.encode] in this module, in the ALTER TABLE DROP COLUMN
   rewrite, is deliberately NOT a site: it re-encodes an already-stored row
   with one column removed, so it can only preserve or reduce the set of NULLs
   in the columns that survive.

   The static binder checks stay as the earlier, better-located error.

   The message matches SQLite's ("NOT NULL constraint failed: t.c") and the
   wording already used by [eval_check_constraints] / the UNIQUE paths; it
   surfaces to callers as [Db.Runtime]. *)
(* #599: the check itself, decoupled from the raise.  [INSERT OR IGNORE] has to
   ask "does this row violate NOT NULL?" and then *skip* rather than fail, so
   the predicate is separated from the policy.  Only the INSERT call site is
   allowed to soften it — see [execute_insert_write]; the UPDATE site keeps
   [enforce_not_null] because UPDATE has no [OR IGNORE] form to consult.

   Returns the message for the FIRST violating column, matching the column
   order the raising version reported.

   #629: [clock]/[params] are how a VIRTUAL generated column gets checked
   against the value it actually holds.  The write paths set a VIRTUAL cell to
   [V_null] on purpose ([compute_stored_generated_cols]) and recompute it on
   read, so the row handed in here says NULL for a column that is not NULL —
   which is why #567 exempted them wholesale.  Recomputing the virtuals into a
   copy first narrows WHEN the check runs (after the generated value exists)
   rather than WHETHER, so a genuinely NULL-valued generated expression is
   still caught.  Both are optional and default to the same values
   [compute_stored_generated_cols] is already called with elsewhere in this
   module ([None], [[||]]); a generated expression is DDL and cannot reference
   a parameter, so the default is only ever wrong for a clock-dependent one.

   The STORED half of #629 rests on an ordering claim, and the claim is about
   the ROW-STORE sites only — stating it as "every enforcement site" was wrong
   when this shipped, so here it is enumerated.  [compute_stored_generated_cols]
   runs before the check at [execute_insert] (feeding [execute_insert_write]),
   [execute_upsert_update], [update_col_in_tx] and [apply_update_row] (feeding
   [write_row_rekeyed]).  It is NOT called on either columnar arm: both build
   the row with [Array.make n_cols Row.V_null] and pass it straight to
   [not_null_skip_or_fail].  That is sound only because
   [Sema.bind_create] now refuses a GENERATED column on a COLUMNSTORE table
   outright (#660) — no generated column can reach those two sites, so the
   ordering question does not arise there.  If that refusal is ever lifted, the
   two arms need the call before the check, and the VIRTUAL read path needs a
   recompute, or a columnar generated column reads NULL forever. *)
let not_null_violation
      ?(clock : (unit -> float) option = None)
      ?(params : Row.value array = [||])
      (table_meta : Cat.table_meta)
      (row : Row.t)
  : string option
  =
  let n = Array.length row in
  (* Only safe when the row covers every column: [compute_virtual_generated_cols]
     writes into [row.(i)] for each virtual column and would raise on a short
     row.  When it is not safe the #567 exemption stands. *)
  let virtuals_computed =
    has_virtual_cols table_meta.Cat.columns && n = List.length table_meta.Cat.columns
  in
  let row =
    if virtuals_computed then with_computed_virtuals clock params table_meta row else row
  in
  let rec go i = function
    | [] -> None
    | (col : Row.column) :: rest ->
      let violated =
        col.Row.not_null
        && ((not (not_null_exempt_col col)) || virtuals_computed)
        && i < n
        && row.(i) = Row.V_null
      in
      if violated
      then
        Some
          (Printf.sprintf
             "NOT NULL constraint failed: %s.%s"
             table_meta.Cat.name
             col.Row.name)
      else go (i + 1) rest
  in
  go 0 table_meta.Cat.columns
;;

let enforce_not_null ?clock ?params (table_meta : Cat.table_meta) (row : Row.t) : unit =
  match not_null_violation ?clock ?params table_meta row with
  | None -> ()
  | Some msg -> failwith msg
;;

(* #599: NOT NULL under a conflict-resolution modifier.  [OR IGNORE] means
   "skip rows that violate a constraint" and NOT NULL is a constraint, so it
   skips here exactly as it already did for UNIQUE — returning [true] for
   "skip this row".  Every other resolution raises:

   - [OR ABORT] / [OR FAIL] / [OR ROLLBACK] and the bare INSERT all raise on a
     UNIQUE violation too ([check_insert_unique]'s catch-all), so raising here
     keeps the two constraint kinds in step.
   - [OR REPLACE] raises, and that is a deliberate divergence from SQLite,
     which substitutes the column's DEFAULT for the NULL and only aborts when
     there is none.  REPLACE here means "delete the row this one conflicts
     with"; there is no conflicting row for a NULL, and silently rewriting a
     caller's value is a bigger surprise than the error.  Pinned in
     [test_not_null_599.ml]. *)
let not_null_skip_or_fail
      ?clock
      ?params
      (table_meta : Cat.table_meta)
      (row : Row.t)
      ~(on_conflict : Ast.conflict_action option)
  : bool
  =
  match not_null_violation ?clock ?params table_meta row with
  | None -> false
  | Some msg -> if on_conflict = Some Ast.CA_ignore then true else failwith msg
;;

(* ------------------------------------------------------------------ *)
(* FTS inverted-index helpers                                           *)
(* ------------------------------------------------------------------ *)

(** Key format: term_bytes ++ "\x00" ++ rowid_be8
    Rowid stored with sign bit flipped so unsigned byte order = signed int64 order. *)
let fts_term_key term rowid =
  let rb = Bytes.create 8 in
  let v = Int64.logxor rowid Int64.min_int in
  for i = 0 to 7 do
    Bytes.set_uint8
      rb
      i
      (Int64.to_int (Int64.logand (Int64.shift_right_logical v ((7 - i) * 8)) 0xFFL))
  done;
  Bytes.concat Bytes.empty [ Bytes.of_string term; Bytes.of_string "\x00"; rb ]
;;

let fts_stats_key = Bytes.of_string "\x00\x00"

(* #689: doc-length keys share this two-byte tag, so they form ONE contiguous
   region of the FTS index tree holding exactly one entry per indexed document.
   [fts_stats_key] ("\x00\x00") sorts below it and every posting key
   (term ++ "\x00" ++ rowid, over a non-empty tokenizer term) sorts above it,
   which is what lets [fts_doclen_by_scan] walk the region with a single
   cursor. *)
let fts_doclen_prefix = "\x00\x01"

let fts_doclen_key rowid =
  let rb = Bytes.create 8 in
  let v = Int64.logxor rowid Int64.min_int in
  for i = 0 to 7 do
    Bytes.set_uint8
      rb
      i
      (Int64.to_int (Int64.logand (Int64.shift_right_logical v ((7 - i) * 8)) 0xFFL))
  done;
  Bytes.cat (Bytes.of_string fts_doclen_prefix) rb
;;

(* Inverse of [fts_doclen_key]: the rowid a doc-length key names, or [None] when
   [k] is not one.  The body of the key is exactly {!Rowid.encode}'s
   order-preserving biased big-endian int64, so {!Rowid.decode} inverts it. *)
let fts_doclen_key_rowid k =
  if Bytes.length k = 2 + 8 && String.equal (Bytes.sub_string k 0 2) fts_doclen_prefix
  then Some (Rowid.decode (Bytes.sub k 2 8))
  else None
;;

(* Value of a doc-length entry; a missing entry counts as length 1, which is
   what BM25's length normalization has always been given for an index whose
   doc-length row is absent. *)
let fts_decode_doclen = function
  | None -> 1
  | Some b ->
    let n, _ = Varint.decode_uint64 b 0 in
    Int64.to_int n
;;

(** Value: varint pairs (col, pos)* — all positions for one (term, rowid). *)
let encode_positions positions =
  let buf = Buffer.create (List.length positions * 2) in
  List.iter
    (fun (col, pos) ->
       Varint.encode_uint64 buf (Int64.of_int col);
       Varint.encode_uint64 buf (Int64.of_int pos))
    positions;
  Buffer.to_bytes buf
;;

(** FTS content row: n_cols_varint ++ (col_len_varint ++ col_bytes)* *)
let fts_encode_content (texts : string list) : bytes =
  let buf = Buffer.create 64 in
  Varint.encode_uint64 buf (Int64.of_int (List.length texts));
  List.iter
    (fun s ->
       let b = Bytes.of_string s in
       Varint.encode_uint64 buf (Int64.of_int (Bytes.length b));
       Buffer.add_bytes buf b)
    texts;
  Buffer.to_bytes buf
;;

let decode_positions value =
  let len = Bytes.length value in
  let pos = ref 0 in
  let result = ref [] in
  while !pos < len do
    let col, off1 = Varint.decode_uint64 value !pos in
    let p, off2 = Varint.decode_uint64 value off1 in
    result := (Int64.to_int col, Int64.to_int p) :: !result;
    pos := off2
  done;
  List.rev !result
;;

let fts_decode_content bytes =
  let n, off0 = Varint.decode_uint64 bytes 0 in
  let nc = Int64.to_int n in
  let texts = ref [] in
  let pos = ref off0 in
  for _ = 1 to nc do
    let len, off = Varint.decode_uint64 bytes !pos in
    let s = Bytes.sub_string bytes off (Int64.to_int len) in
    texts := s :: !texts;
    pos := off + Int64.to_int len
  done;
  List.rev !texts
;;

(** Read global FTS stats from index tree: (total_docs, total_tokens). *)
let read_fts_stats tx index_tree =
  let+ bytes_opt = S.get tx index_tree fts_stats_key in
  match bytes_opt with
  | None -> 0, 0
  | Some b ->
    let docs, off = Varint.decode_uint64 b 0 in
    let toks, _ = Varint.decode_uint64 b off in
    Int64.to_int docs, Int64.to_int toks
;;

let write_fts_stats tx index_tree docs tokens =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf (Int64.of_int docs);
  Varint.encode_uint64 buf (Int64.of_int tokens);
  S.put tx index_tree fts_stats_key (Buffer.to_bytes buf)
;;

(** Write inverted index entries for a newly inserted document. *)
let fts_index_document tx ~(fts_meta : Cat.fts_table_meta) ~rowid ~col_texts =
  let tokens = Fts_tokenizer.tokenize col_texts in
  (* Group by term *)
  let by_term : (string, (int * int) list) Hashtbl.t = Hashtbl.create 8 in
  List.iter
    (fun (tok : Fts_tokenizer.token) ->
       let lst = Option.value ~default:[] (Hashtbl.find_opt by_term tok.term) in
       Hashtbl.replace by_term tok.term ((tok.col, tok.pos) :: lst))
    tokens;
  (* Write one entry per unique term *)
  let* () =
    Hashtbl.fold
      (fun term positions acc ->
         let* () = acc in
         let key = fts_term_key term rowid in
         let value = encode_positions (List.rev positions) in
         S.put tx fts_meta.Cat.fts_index_tree key value)
      by_term
      Lwt.return_unit
  in
  (* Write doc length *)
  let dlen = List.length tokens in
  let dlen_buf = Buffer.create 4 in
  Varint.encode_uint64 dlen_buf (Int64.of_int dlen);
  let* () =
    S.put tx fts_meta.Cat.fts_index_tree (fts_doclen_key rowid) (Buffer.to_bytes dlen_buf)
  in
  (* Update global stats *)
  let* docs, toks = read_fts_stats tx fts_meta.Cat.fts_index_tree in
  write_fts_stats tx fts_meta.Cat.fts_index_tree (docs + 1) (toks + dlen)
;;

(** Remove inverted index entries for a deleted document. *)
let fts_deindex_document tx ~(fts_meta : Cat.fts_table_meta) ~rowid ~col_texts =
  let tokens = Fts_tokenizer.tokenize col_texts in
  let terms =
    List.sort_uniq
      String.compare
      (List.map (fun (t : Fts_tokenizer.token) -> t.term) tokens)
  in
  let* () =
    Lwt_list.iter_s
      (fun term -> S.del tx fts_meta.Cat.fts_index_tree (fts_term_key term rowid))
      terms
  in
  let dlen = List.length tokens in
  let* () = S.del tx fts_meta.Cat.fts_index_tree (fts_doclen_key rowid) in
  let* docs, toks = read_fts_stats tx fts_meta.Cat.fts_index_tree in
  write_fts_stats tx fts_meta.Cat.fts_index_tree (max 0 (docs - 1)) (max 0 (toks - dlen))
;;

(* ------------------------------------------------------------------ *)
(* FTS query execution                                                  *)
(* ------------------------------------------------------------------ *)

(** Fetch the posting list for an exact term: [(rowid, positions)] *)
let fts_posting_list tx ~index_tree term =
  (* Scan keys from term\x00 onwards (sorted order).  Native O(log n) seek +
     lazy streaming of the matching prefix range, instead of draining the whole
     FTS index tree per query (#233; same fix class as #228/#229). *)
  let prefix = Bytes.cat (Bytes.of_string term) (Bytes.of_string "\x00") in
  let plen = Bytes.length prefix in
  let* cur = S.seek_ge tx index_tree prefix in
  let entries = ref [] in
  let rec gather () =
    match%lwt S.seek_next cur with
    | None -> Lwt.return_unit
    | Some (key, value) ->
      if Bytes.length key >= plen && Bytes.equal (Bytes.sub key 0 plen) prefix
      then (
        (* Extract rowid from last 8 bytes (sign-bit-flipped) *)
        let rowid_off = Bytes.length key - 8 in
        let v = ref 0L in
        for i = 0 to 7 do
          v
          := Int64.logor
               (Int64.shift_left !v 8)
               (Int64.of_int (Bytes.get_uint8 key (rowid_off + i)))
        done;
        let rowid = Int64.logxor !v Int64.min_int in
        let positions = decode_positions value in
        entries := (rowid, positions) :: !entries;
        gather ())
      else Lwt.return_unit (* keys are sorted: first non-match ends the range *)
  in
  let* () = gather () in
  S.seek_close cur;
  Lwt.return (List.rev !entries)
;;

(** Fetch posting lists for a prefix: merge all (rowid, positions) for terms matching prefix* *)
let fts_prefix_posting_list tx ~index_tree prefix_str =
  let prefix_bytes = Bytes.of_string prefix_str in
  let plen = Bytes.length prefix_bytes in
  (* Native O(log n) seek + lazy streaming of the matching prefix range,
     instead of draining the whole FTS index tree per query (#233). *)
  let* cur = S.seek_ge tx index_tree prefix_bytes in
  let by_rowid : (int64, (int * int) list) Hashtbl.t = Hashtbl.create 16 in
  let rec gather () =
    match%lwt S.seek_next cur with
    | None -> Lwt.return_unit
    | Some (key, value) ->
      (* Find the null byte separating term from rowid *)
      let null_pos = ref (-1) in
      let klen = Bytes.length key in
      let i = ref 0 in
      while !i < klen - 8 && !null_pos = -1 do
        if Bytes.get_uint8 key !i = 0 then null_pos := !i;
        incr i
      done;
      if !null_pos > 0
      then (
        let term_len = !null_pos in
        (* Check term has our prefix *)
        if term_len >= plen && Bytes.equal (Bytes.sub key 0 plen) prefix_bytes
        then (
          let rowid_off = !null_pos + 1 in
          if rowid_off + 8 <= klen
          then (
            let v = ref 0L in
            for j = 0 to 7 do
              v
              := Int64.logor
                   (Int64.shift_left !v 8)
                   (Int64.of_int (Bytes.get_uint8 key (rowid_off + j)))
            done;
            let rowid = Int64.logxor !v Int64.min_int in
            let positions = decode_positions value in
            let existing = Option.value ~default:[] (Hashtbl.find_opt by_rowid rowid) in
            Hashtbl.replace by_rowid rowid (existing @ positions);
            gather ())
          else Lwt.return_unit)
        else Lwt.return_unit (* term no longer has the prefix, stop — keys are sorted *))
      else Lwt.return_unit
  in
  let* () = gather () in
  S.seek_close cur;
  Lwt.return
    (Hashtbl.fold (fun rowid positions acc -> (rowid, positions) :: acc) by_rowid [])
;;

(* FTS phrase match: all [words] must appear consecutively in the same column.
   For each candidate doc, check there is a start position p and column c with
   word[i] at (col=c, pos=p+i) for all i. *)
(** Execute an FTS query, returning [(rowid, positions)] for matching documents. *)
let fts_phrase_match tx ~index_tree words =
  match words with
  | [] -> Lwt.return []
  | first :: rest ->
    let* first_pl = fts_posting_list tx ~index_tree first in
    let* rest_pls = Lwt_list.map_s (fts_posting_list tx ~index_tree) rest in
    (* Keep only docs present in every posting list. *)
    let intersect_ids acc pl =
      let ids = List.map fst pl in
      List.filter (fun (r, _) -> List.mem r ids) acc
    in
    let candidates = List.fold_left intersect_ids first_pl rest_pls in
    (* Build an array of per-term posting lists for position checking. *)
    let all_pls = Array.of_list (first_pl :: rest_pls) in
    let n = Array.length all_pls in
    (* Check whether doc with [rowid] contains the phrase. *)
    let phrase_matches rowid =
      let term_positions =
        Array.map
          (fun pl ->
             match List.assoc_opt rowid pl with
             | None -> []
             | Some pos -> pos)
          all_pls
      in
      List.exists
        (fun (c0, p0) ->
           let rec check i =
             if i >= n
             then true
             else List.mem (c0, p0 + i) term_positions.(i) && check (i + 1)
           in
           check 1)
        term_positions.(0)
    in
    let matched = List.filter (fun (r, _) -> phrase_matches r) candidates in
    Lwt.return matched
;;

let rec fts_execute_query tx ~index_tree query =
  match query with
  | Fts_query.FQ_term (Fts_query.FT_exact term) -> fts_posting_list tx ~index_tree term
  | Fts_query.FQ_term (Fts_query.FT_prefix prefix) ->
    fts_prefix_posting_list tx ~index_tree prefix
  | Fts_query.FQ_term (Fts_query.FT_phrase words) -> fts_phrase_match tx ~index_tree words
  | Fts_query.FQ_and qs ->
    let positive =
      List.filter
        (function
          | Fts_query.FQ_not _ -> false
          | _ -> true)
        qs
    in
    let negated =
      List.filter_map
        (function
          | Fts_query.FQ_not q -> Some q
          | _ -> None)
        qs
    in
    let* pos_results = Lwt_list.map_s (fts_execute_query tx ~index_tree) positive in
    let* neg_results = Lwt_list.map_s (fts_execute_query tx ~index_tree) negated in
    let neg_ids = List.concat_map (List.map fst) neg_results in
    let intersected =
      match pos_results with
      | [] -> []
      | first :: rest ->
        List.fold_left
          (fun acc pl ->
             let ids = List.map fst pl in
             List.filter (fun (r, _) -> List.mem r ids) acc)
          first
          rest
    in
    Lwt.return (List.filter (fun (r, _) -> not (List.mem r neg_ids)) intersected)
  | Fts_query.FQ_or qs ->
    let* results = Lwt_list.map_s (fts_execute_query tx ~index_tree) qs in
    let seen : (int64, unit) Hashtbl.t = Hashtbl.create 16 in
    let union =
      List.concat_map
        (fun pl ->
           List.filter
             (fun (r, _) ->
                if Hashtbl.mem seen r
                then false
                else (
                  Hashtbl.replace seen r ();
                  true))
             pl)
        results
    in
    Lwt.return union
  | Fts_query.FQ_not _ ->
    (* Standalone NOT is meaningless; returns empty set.
       NOT inside AND is handled in the FQ_and case above. *)
    Lwt.return []
;;

(** Helper: find the first index [i] such that [pred lst[i]] holds. *)
let list_find_index pred lst =
  let rec go i = function
    | [] -> None
    | x :: _ when pred x -> Some (i, x)
    | _ :: rest -> go (i + 1) rest
  in
  go 0 lst
;;

(* ------------------------------------------------------------------ *)
(* Transaction mode                                                     *)
(* ------------------------------------------------------------------ *)

type txn_mode =
  | Auto (** Each DML op starts and commits its own RW txn. *)
  | In_txn of S.rw S.txn (** Use this txn; skip auto begin/commit. *)
  | In_ro_txn of S.ro S.txn
  (** #274: read every scan through this one RO snapshot so a multi-statement
        read (e.g. [Db.dump] sweeping every table) observes a single
        point-in-time committed state.  Read-only: never reaches a write path;
        its lifecycle is owned by the caller, not ended by a scanner. *)

let acquire_txn store mode =
  match mode with
  | Auto ->
    let* tx = S.rw_begin store in
    Lwt.return (tx, true)
  | In_txn tx -> Lwt.return (tx, false)
  | In_ro_txn _ ->
    (* A write was attempted under a read-only ambient snapshot — a caller bug,
       not a runtime condition: [In_ro_txn] is only ever set on read paths. *)
    Lwt.fail (Failure "write attempted under a read-only transaction (In_ro_txn)")
;;

(* Commit [tx] if [owned], otherwise no-op.  When [cat] is provided and
   [owned], flush deferred rowid counters first (#347) so that counters
   dirtied by nested In_txn DML (e.g. trigger inserts) are persisted, then
   persist any dirty columnar stores before committing.  After commit
   succeeds, clear the dirty flag on the stores that were persisted so the
   next cycle only flushes new mutations. *)
let release_txn ?cat tx owned =
  if owned
  then (
    let* saved =
      match cat with
      | None -> Lwt.return []
      | Some c ->
        let* () = Cat.flush_dirty_counters_tx c tx in
        Cat.persist_dirty_columnar_stores c tx
    in
    let* () = S.commit tx in
    List.iter Granary_columnar.Col_store.mark_clean saved;
    Lwt.return_unit)
  else Lwt.return_unit
;;

(** Extract the in-memory columnar store from a table_meta.  Asserts [Row]
    cannot happen at call sites guarded by [Cat.is_columnar]. *)
let col_store_of_meta (m : Cat.table_meta) : Granary_columnar.Col_store.t =
  match m.Cat.storage with
  | Cat.Columnar (cs, _) -> cs
  | Cat.Row _ -> assert false
;;

(* #269: run a DDL body [f tx] under a transaction chosen by [mode], threading
   the writer txn into the catalog so DDL participates in any ambient explicit
   transaction instead of opening its own (which would self-deadlock against the
   single-writer lock the explicit txn already holds).

   Ownership decides who finalizes:
   - [Auto] (we opened the txn): commit on success / rollback on failure, and
     correspondingly clear or run the catalog's schema-cache undo log — the DDL
     mutated the in-memory cache before this commit, so a rollback must revert it.
   - [In_txn] (borrowed): leave commit/rollback AND schema-undo finalization to
     the db layer's COMMIT/ROLLBACK; an error here propagates with the ambient
     transaction left open.  A failed in-txn DDL statement may have already
     applied partial on-disk effects (e.g. [alter_drop_column] dropping a
     dependent index, then rewriting rows, before [Cat.drop_column] raises), and
     we have no statement-level savepoint to undo just this statement (#280/#283).
     So we poison the catalog (#286): the db layer forces a later COMMIT to roll
     the whole transaction back instead of persisting the half-applied DDL — the
     transaction is uncommittable, matching SQLite.  A ROLLBACK still unwinds
     cleanly via the whole-txn schema-undo log + store rollback. *)
let with_ddl_txn store (cat : Cat.t) mode f =
  let* tx, owned = acquire_txn store mode in
  Lwt.catch
    (fun () ->
       let* r = f tx in
       let* () =
         if owned
         then (
           let* () = S.commit tx in
           Cat.commit_schema_changes cat;
           Lwt.return_unit)
         else Lwt.return_unit
       in
       Lwt.return r)
    (fun exn ->
       let* () =
         if owned
         then (
           let* () = S.rollback tx in
           Cat.rollback_schema_changes cat;
           Lwt.return_unit)
         else (
           (* #286: borrowed txn — partial effects remain; mark uncommittable. *)
           Cat.mark_schema_txn_poisoned cat;
           Lwt.return_unit)
       in
       Lwt.fail exn)
;;

(* #262: a read handle for a base scanner.  Inside an explicit transaction
   ([In_txn tx]) reads must go THROUGH [tx] so they observe the transaction's
   own uncommitted writes (read-your-own-writes).  A scanner that instead opens
   a fresh RO snapshot ([S.ro_begin]) is, by snapshot-isolation design (#178),
   blind to the active writer's in-flight mutations — so a [SELECT] after
   [BEGIN; INSERT] would see the pre-[BEGIN] committed state.  In [Auto] mode
   (no explicit txn) the scanner owns a fresh RO snapshot and ends it when the
   read finishes; the borrowed txn is never ended here — Db owns its lifecycle. *)
type read_handle =
  | RH_borrowed of S.rw S.txn (* active explicit txn; lifecycle owned by Db *)
  | RH_borrowed_ro of S.ro S.txn (* #274: shared RO snapshot; lifecycle owned by caller *)
  | RH_owned of S.ro S.txn (* scanner-owned RO snapshot; ended on finish *)

let rh_begin store = function
  | In_txn tx -> Lwt.return (RH_borrowed tx)
  | In_ro_txn tx -> Lwt.return (RH_borrowed_ro tx)
  | Auto ->
    let* tx = S.ro_begin store in
    Lwt.return (RH_owned tx)
;;

let rh_finish = function
  | RH_borrowed _ | RH_borrowed_ro _ -> Lwt.return_unit
  | RH_owned tx -> S.ro_end tx
;;

let rh_get = function
  | RH_borrowed tx -> S.get tx
  | RH_borrowed_ro tx -> S.get tx
  | RH_owned tx -> S.get tx
;;

let rh_seek_ge = function
  | RH_borrowed tx -> S.seek_ge tx
  | RH_borrowed_ro tx -> S.seek_ge tx
  | RH_owned tx -> S.seek_ge tx
;;

let rh_cursor_open = function
  | RH_borrowed tx -> S.cursor_open tx
  | RH_borrowed_ro tx -> S.cursor_open tx
  | RH_owned tx -> S.cursor_open tx
;;

(* [with_read store mode f] runs [f] over a read handle, ending it afterwards
   only when the scanner owns it (Auto).  The txn-aware analogue of [S.with_ro];
   like it, the handle is released even if [f] raises (#164). *)
let with_read store mode f =
  let* rh = rh_begin store mode in
  Lwt.finalize (fun () -> f rh) (fun () -> rh_finish rh)
;;

(* ------------------------------------------------------------------ *)
(* execute: write operations only                                       *)
(* ------------------------------------------------------------------ *)

(** Replace every [P_excluded_col i] with [P_lit (value_to_literal excluded_row.(i))].
    Used to materialise UPSERT excluded-row refs before [eval_expr]. *)
let rec substitute_excluded (excluded_row : Row.t) (e : Plan.expr) : Plan.expr =
  match e with
  | Plan.P_excluded_col i -> Plan.P_lit (value_to_literal excluded_row.(i))
  | Plan.P_binop (op, a, b) ->
    Plan.P_binop
      (op, substitute_excluded excluded_row a, substitute_excluded excluded_row b)
  | Plan.P_not e -> Plan.P_not (substitute_excluded excluded_row e)
  | Plan.P_is_null e -> Plan.P_is_null (substitute_excluded excluded_row e)
  | Plan.P_is_not_null e -> Plan.P_is_not_null (substitute_excluded excluded_row e)
  | Plan.P_neg e -> Plan.P_neg (substitute_excluded excluded_row e)
  | Plan.P_bitnot e -> Plan.P_bitnot (substitute_excluded excluded_row e)
  | Plan.P_between (x, lo, hi) ->
    Plan.P_between
      ( substitute_excluded excluded_row x
      , substitute_excluded excluded_row lo
      , substitute_excluded excluded_row hi )
  | Plan.P_in (x, vals) ->
    Plan.P_in
      ( substitute_excluded excluded_row x
      , List.map (substitute_excluded excluded_row) vals )
  | Plan.P_func (f, args) ->
    Plan.P_func (f, List.map (substitute_excluded excluded_row) args)
  | Plan.P_case { scrutinee; branches; else_ } ->
    let go = substitute_excluded excluded_row in
    Plan.P_case
      { scrutinee = Option.map go scrutinee
      ; branches = List.map (fun (c, r) -> go c, go r) branches
      ; else_ = Option.map go else_
      }
  | Plan.P_cast (e, ty) -> Plan.P_cast (substitute_excluded excluded_row e, ty)
  | Plan.P_collate (e, c) -> Plan.P_collate (substitute_excluded excluded_row e, c)
  (* Leaves, and the three subquery-bearing nodes whose inner [Ast.stmt] carries
     no [EXCLUDED] reference to substitute.  Exhaustive rather than [| other ->]
     for the reason #670 gives: a catch-all is what lets a newly added
     constructor fall through a walker silently. *)
  | Plan.P_lit _
  | Plan.P_col _
  | Plan.P_param _
  | Plan.P_window_slot _
  | Plan.P_subquery _
  | Plan.P_exists _
  | Plan.P_in_select _ -> e
;;

(** True if any value in the list is NULL. *)
let any_null_val = List.exists (fun v -> v = Row.V_null)

(** Find column indices for a list of column names in [schema].
    Returns [None] for any name not found. *)
let find_col_idxs schema col_names =
  List.map
    (fun name ->
       let rec fi i = function
         | [] -> None
         | (c : Row.column) :: _ when String.equal c.name name -> Some i
         | _ :: rest -> fi (i + 1) rest
       in
       fi 0 schema)
    col_names
;;

(** #765 review round 3, item 2: resolve an FK's own column-name list
    ([fk_local_cols] or [fk_parent_cols]) against [schema], or fail with a
    loud, FK-specific message naming [table_name] and the missing columns —
    the same shape {!enforce_insert_fk} already uses for an INSERT's
    parent-column lookup — rather than {!find_col_idx_by_name}'s bare,
    FK-context-free [Failure "column not found: <name>"]. [precheck_update_fk]
    and [precheck_delete_fk] (the immediate RESTRICT precheck, as opposed to
    {!make_fk_recheck}'s deferred recheck) did not get this treatment when
    rounds 1 and 2 gave it to the deferred path, so a [DROP COLUMN] of an
    FK-participating column (#767) surfaced there as an internal-looking
    crash instead of a deliberate refusal. *)
let resolve_fk_col_idxs ~table_name schema col_names : int list Lwt.t =
  let idxs_opt = find_col_idxs schema col_names in
  if List.for_all Option.is_some idxs_opt
  then Lwt.return (List.filter_map Fun.id idxs_opt)
  else
    Lwt.fail_with
      (Printf.sprintf
         "FOREIGN KEY: some columns not found in table '%s' (expected %s) -- the \
          constraint can no longer be evaluated"
         table_name
         (String.concat ", " col_names))
;;

(** Non-raising variant of find_col_idx_by_name: returns [None] if not found. *)
let find_col_idx_by_name_opt schema col_name =
  let rec fi i = function
    | [] -> None
    | (c : Row.column) :: _ when String.equal c.name col_name -> Some i
    | _ :: rest -> fi (i + 1) rest
  in
  fi 0 schema
;;

(** #765 review (round 2): the 0-based position of [fk] within
    [child_meta.Cat.fk_constraints], found by PHYSICAL equality. [fk] must be
    an element drawn from that exact list -- every call site gets it either
    directly from the unfiltered list, or via [List.filter] over it (e.g.
    [build_child_refs]), and [List.filter] never copies the elements it keeps
    -- not a freshly-constructed record.

    This ordinal, not the FK's column NAME strings, is what a deferred
    recheck must capture to identify the constraint again later: {!ALTER
    TABLE ... RENAME COLUMN} rewrites [fk_local_cols]/[fk_parent_cols] in
    place ([Cat.rename_column]), so a name captured before the rename is
    stale by the time a recheck queued before it runs. The ordinal survives
    every schema mutation touching [fk_constraints] found in this codebase:
    RENAME substitutes names 1:1 via [List.map] (order and length preserved);
    [Cat.drop_column] does not touch [fk_constraints] at all (a dropped
    column can leave a stale name behind, which {!make_fk_recheck} handles
    separately by failing loudly rather than mis-identifying a constraint);
    and [alter_add_column]'s inline FK is appended at the END of the list.
    Nothing removes or reorders an existing entry. *)
let fk_ordinal (child_meta : Cat.table_meta) (fk : Cat.fk_constraint) : int option =
  List.find_index (fun fk' -> fk' == fk) child_meta.Cat.fk_constraints
;;

(** Encode a multi-column index-key prefix (no rowid).  Used by FK enforcement
    to seek to the first entry whose leading key columns match a target value
    list.  Returns the prefix bytes and their length. *)
let encode_index_key_prefix (ivs : Index_key.value list) : bytes * int =
  let parts = List.map Index_key.encode_value ivs in
  let total = List.fold_left (fun acc b -> acc + Bytes.length b) 0 parts in
  let buf = Bytes.create total in
  let off = ref 0 in
  List.iter
    (fun b ->
       let len = Bytes.length b in
       Bytes.blit b 0 buf !off len;
       off := !off + len)
    parts;
  buf, total
;;

(** #508: map evaluated equality values to the index-key values that seek them.
    [None] means "matches nothing" and the caller must return no rows without
    seeking — which is the honest encoding of both cases that reach it:

    - a NULL value, because [WHERE col = NULL] never matches (three-valued
      logic); a bound parameter may only turn out NULL at run time (#228).
    - a value whose type does not match the column's, which no stored key can
      equal.  Note this must NOT become an [IK_null] prefix: that is a real seek
      key selecting the index's NULL entries, not an empty result.

    #738: a value of the OTHER numeric type is the exception, and it has to be,
    because the residual predicate no longer declines it.  [=] is
    {!compare_values} now, so [i = 1.0] is TRUE for the stored integer [1] —
    and an equality conjunct is CONSUMED by the access path
    ([Planner.residual_filter]), so nothing re-checks the rows a seek yields.
    Declining here would therefore lose them silently rather than merely
    widening the scan.  The translation is exact in both directions and
    [None] still means "matches nothing", never "seek wider":

    - a REAL probe on an INTEGER column becomes [IK_int] when the float names
      an integer exactly ({!int64_of_exact_real}); otherwise no integer equals
      it, so [None] is the honest answer — [i = 1.5] genuinely matches nothing.
      A NaN or an infinity is declined by the same helper, so neither can reach
      the key as an "integral" real.
    - an INTEGER probe on a REAL column becomes [IK_real] only when the
      round-trip is exact ({!exact_real_of_int64}); above 2^53 [Int64.to_float]
      names a different integer, and no stored double equals the one asked for.

    The read ([stream_index_lookup]) and write ([seek_index_candidates]) paths
    share this so they can never disagree about which rows a key matches. *)
let index_lookup_values (vs : (Row.value * Row.ty) list) : Index_key.value list option =
  let rec go acc = function
    | [] -> Some (List.rev acc)
    | (v, ty) :: rest ->
      (match v, ty with
       | Row.V_int n, Row.Integer -> go (Index_key.IK_int n :: acc) rest
       | Row.V_text s, Row.Text -> go (Index_key.IK_text s :: acc) rest
       | Row.V_real f, Row.Real -> go (Index_key.IK_real f :: acc) rest
       | Row.V_blob b, Row.Blob -> go (Index_key.IK_blob b :: acc) rest
       | Row.V_null, _ -> None (* [col = NULL] never matches *)
       (* #738: cross-numeric, exactly — see the note above. *)
       | Row.V_real f, Row.Integer ->
         (match int64_of_exact_real f with
          | Some n -> go (Index_key.IK_int n :: acc) rest
          | None -> None)
       | Row.V_int n, Row.Real ->
         (match exact_real_of_int64 n with
          | Some f -> go (Index_key.IK_real f :: acc) rest
          | None -> None)
       | _, _ -> None (* type mismatch: no stored key can equal this *))
  in
  go [] vs
;;

(** #738: the rowid an equality probe on the INTEGER PRIMARY KEY rowid alias
    addresses, or [None] when no rowid can equal it.

    The alias column IS the table key, so there is no index and no
    [Index_key.value] — but the question is the same one {!index_lookup_values}
    answers for an indexed column, and the answer must agree with it and with
    [=]'s residual, which since #738 is {!compare_values}.  Both the read path
    ({!stream_rowid_lookup}) and the DML path ({!seek_candidates}'s
    [Seek_rowid] arm) go through here so they cannot drift apart. *)
let rowid_lookup_key (v : Row.value) : int64 option =
  match v with
  | Row.V_int n -> Some n
  | Row.V_real f -> int64_of_exact_real f
  | Row.V_null | Row.V_text _ | Row.V_blob _ -> None
;;

(* [2^63] as a float — exactly representable, and one past [Int64.max_int].  A
   float strictly inside [[-2^63, 2^63)] converts to an int64 without
   overflowing; [Int64.of_float] is unspecified outside it.

   #527: [Granary_sql.Planner.classify_range_bound] spells this same literal
   inline, because this module depends on [Planner] and not the other way round,
   so sharing it would mean moving an [Int64.of_float]-domain constant into the
   planner's [.mli] for one use.  The two MUST stay equal: the documented
   disagreement between classification and {!range_bound_key} is then exactly
   the one ULP the [pred]/[succ] widening moves, and nothing more.  Change one
   and you must change the other. *)
let two_pow_63 = 9.2233720368547758e18

(** #527: the index-key value that bounds [which] end of a range at [v], for a
    column of type [ty].  Unlike {!index_lookup_values} — which answers an
    {i equality} question and so must decline every type mismatch — a bound of
    the other numeric type is a perfectly good bound even though it is not a
    key, and declining it costs the whole narrowing (the entire equality prefix
    is scanned instead).

    The promotion rounds {b OUTWARD}: a lower bound up, an upper bound down.
    Rounding the other way would silently drop the endpoint row.  Both ends are
    read inclusively anyway (see {!range_seek_bounds}) and the predicate still
    runs on every yielded row, so no strictness has to be tracked.

    On an integer column the rounding is {b widened by one float step first}
    ([ceil (pred f)], [floor (succ f)]).  {b Read the history before touching
    it: the widening is now a deliberate over-approximation, not a
    requirement.}

    It was written against the {i inexact} residual predicate that existed
    until #733.  [cmp_result] then compared [V_int a] against [V_real b] as
    [Float.compare (Int64.to_float a) b], so it admitted every int64 whose
    {i rounded} float value satisfied the bound.  Above 2^53, where a float ULP
    exceeds 1, many integers strictly below a lower bound [f] rounded up onto
    [f] and so qualified; [ceil f] would have sought past all of them and
    dropped those rows (256 of them at 2^62, ~1024 near 2^63).  One
    [pred]/[succ] step covers exactly that gap, moving the bound by one ULP —
    the same grid spacing that creates it — while below 2^53 it moves by less
    than 1 and so changes nothing after the [ceil]/[floor]:
    [ceil (pred 100.5) = 101], [ceil (pred 280.0) = 280],
    [floor (succ 119.5) = 119], [floor (succ 280.0) = 280].

    Since #733 the residual predicate compares exactly ({!cmp_int_real}), so
    plain [ceil]/[floor] {i would} now be the tight boundary and the widening
    buys at most one ULP of keys the predicate then rejects (512 at 2^62, one
    key below 2^53).  It is kept, for two reasons:

    - the seek stays a {b superset} of the qualifying set under {i both} the
      old and the new predicate semantics.  A widened seek plus an exact
      residual is safe (superset in, correct filter after); a tightened seek is
      only safe while the predicate stays exact, so keeping the widening means
      no future change to {!cmp_result} can silently turn this into a rows-lost
      bug.
    - the safety of tightening it rests entirely on
      {!Granary_sql.Planner.range_for_index} never marking a range conjunct
      consumed, i.e. on a residual filter always running over the seek's
      output.  That holds today and is asserted there, but it is a property of
      another module; over-approximating here does not depend on it.

    Tightening it is a separable performance change, not a correctness one.

    The result must be an [IK_int] on an integer column, not the real as given:
    {!Granary_encoding.Index_key.encode_value} emits a distinct leading type tag
    per type ([0x02] integer, [0x03] real), so an [IK_real] bound sorts into the
    reals' region and never meets the stored integer keys at all — encoding it
    as-is would be worse than declining.

    Declined ([None]) cases leave that end unbounded, which is always sound:

    - a value whose {i widened} rounding is out of int64 range, or an infinity,
      has no integer to round to.  Producing a key here would mean trusting
      [Int64.of_float] outside its specified domain, and a wrong key drops rows.
      Declining at the very edge costs nothing: a lower bound of -2^63 (whose
      widening steps below the range) admits every row anyway.
    - a non-numeric bound on a numeric column, and vice versa, exactly as
      {!index_lookup_values} decides.

    A NaN is deliberately {i not} declined: it goes through as [IK_real nan],
    which {!Granary_encoding.Index_key.encode_value} writes as its own
    single-byte [0x01] tag (#578), sorting below every INTEGER/REAL key but
    above NULL's [0x00].  That matches the residual predicate, which since
    #733 orders NaN below every number through {!cmp_int_real} (#536's decided
    rule) rather than through [Float.compare]'s accident — the same answer, now
    by construction — and is the existing behaviour for a same-type NaN bound —
    see [range_seek_bounds]. *)
let range_bound_key ~(which : [ `Lo | `Hi ]) (v : Row.value) (ty : Row.ty)
  : Index_key.value option
  =
  match v, ty with
  | Row.V_real f, Row.Integer ->
    if Float.is_nan f
    then Some (Index_key.IK_real f)
    else (
      let g =
        match which with
        | `Lo -> Float.ceil (Float.pred f)
        | `Hi -> Float.floor (Float.succ f)
      in
      if g >= -.two_pow_63 && g < two_pow_63
      then Some (Index_key.IK_int (Int64.of_float g))
      else None)
  | Row.V_int n, Row.Real ->
    (* [Int64.to_float] rounds to nearest, which for |n| > 2^53 can land on the
       wrong side of [n].  One step of [pred]/[succ] is enough to push it back
       outward, the error being at most half a ULP. *)
    let f = Int64.to_float n in
    let cmp =
      (* [f] is integral with |f| <= 2^63, so this comparison is exact. *)
      if f >= two_pow_63 then 1 else Int64.compare (Int64.of_float f) n
    in
    Some
      (Index_key.IK_real
         (match which with
          | `Lo -> if cmp > 0 then Float.pred f else f
          | `Hi -> if cmp < 0 then Float.succ f else f))
  | _, _ ->
    (* Everything that is not a numeric cross-type pair — a same-type bound, a
       NULL, text or a blob — is deliberately delegated to the equality path's
       strictness, which for these cases is also the right answer for a bound.
       The delegation is a shorthand for that agreement, NOT an assumption that
       the two questions always coincide: the numeric arms above exist precisely
       because they do not.  If [index_lookup_values] ever changes which of
       these it accepts, re-check that the new answer is still a sound bound
       rather than assuming it carries over.

       #738 did change it — it taught the equality path the two numeric
       cross-type pairs — and this function is untouched by that precisely
       because both arms above intercept them before the delegation. *)
    (match index_lookup_values [ v, ty ] with
     | Some [ iv ] -> Some iv
     | Some _ | None -> None)
;;

(** #517: the seek start key and stop test implied by an equality [prefix] and
    an optional [range] over the column right after it.

    Both are pure narrowings of the prefix scan.  The start key positions the
    cursor no later than the first qualifying entry, and the stop test ends the
    walk no earlier than the last one; every row the walk yields is still put
    through the statement's predicate, so an inclusive reading of a strict bound
    costs at most one extra key and can never drop a row.

    The stop test compares a fixed-width window at offset [plen], which needs
    care: {b the bounded column is NOT always that width}.  [Plan.range] admits
    only [Integer] and [Real], whose encodings are 9 bytes — but
    {!Granary_encoding.Index_key.encode_value} emits a {i single} byte for a
    NULL ([0x00]) and, since #578, a {i different} single byte for a NaN real
    ([0x01]).  On such an entry the window runs past the column boundary into
    the bytes that follow.

    That is still sound, and this is the load-bearing reason — not the width.
    Both the NULL tag [0x00] and the NaN tag [0x01] sort below the integer tag
    [0x02] and the real tag [0x03], so a misaligned window always compares
    {i low}: [past_end] never fires early on one, and those entries sort to
    the front of the prefix group anyway, ahead of anything the bound could
    exclude.  A future change to the tag ordering, not to the widths, is what
    would break this.

    The mirror case is a NaN {i bound}, which encodes to its own one byte and
    so makes [past_end] fire on the very first key whose tag is [0x02] or
    above — the seek returns nothing, which agrees with the residual
    predicate, which also orders NaN below every number (#536, via
    {!cmp_int_real} since #733).

    #527: an end whose type is not the column's is not simply dropped — a
    numeric one is promoted across the int/real boundary by {!range_bound_key},
    which rounds outward so the bound can only widen.  Its result is always the
    column type's own key, so the fixed-width reasoning above is unaffected.

    Byte order is column order throughout, because the encoding is
    order-preserving by construction. *)
let range_seek_bounds clock params ~prefix ~plen (range : Plan.range option) =
  match range with
  | None -> Bytes.cat prefix (Rowid.encode Int64.min_int), fun _ -> false
  | Some { Plan.r_ty; r_lo; r_hi } ->
    let encode_end which e =
      match range_bound_key ~which (eval_expr clock params [||] e) r_ty with
      | Some iv -> Some (Index_key.encode_value iv)
      | None -> None (* NULL, or nothing sound to round to: leave that end open *)
    in
    let start =
      match Option.bind r_lo (encode_end `Lo) with
      | None -> Bytes.cat prefix (Rowid.encode Int64.min_int)
      | Some lo -> Bytes.cat (Bytes.cat prefix lo) (Rowid.encode Int64.min_int)
    in
    let past_end =
      match Option.bind r_hi (encode_end `Hi) with
      | None -> fun _ -> false
      | Some hi ->
        let w = Bytes.length hi in
        fun ikey ->
          Bytes.length ikey >= plen + w && Bytes.compare (Bytes.sub ikey plen w) hi > 0
    in
    start, past_end
;;

(** Does [ikey] still belong to the range [range_seek_bounds] described?  It must
    carry a trailing rowid, still match the equality [prefix], and not have run
    past the range's high end.

    Shared by the read ([stream_index_lookup]) and write
    ([seek_index_candidates]) paths for the same reason {!index_lookup_values}
    is: the two must never disagree about which index entries a seek covers. *)
let index_key_in_range ~prefix ~plen ~past_end ikey =
  Bytes.length ikey >= plen + 8
  && Bytes.equal (Bytes.sub ikey 0 plen) prefix
  && not (past_end ikey)
;;

(** Decode the rowid from the trailing 8 bytes of an index key. *)
let decode_index_key_rowid (ikey : bytes) : int64 =
  let n = Bytes.length ikey in
  let v = ref 0L in
  for i = 0 to 7 do
    v
    := Int64.logor
         (Int64.shift_left !v 8)
         (Int64.of_int (Bytes.get_uint8 ikey (n - 8 + i)))
  done;
  Int64.logxor !v Int64.min_int
;;

(* Full-table-scan fallbacks shared by the FK lookup/scan helpers below
   (used when no index covers the child columns). [full_scan_exists] stops at
   the first matching row; [full_scan_collect] gathers all (rowid,row) matches. *)
let full_scan_exists tx (meta : Cat.table_meta) (pred : Row.t -> bool) : bool Lwt.t =
  let tree_id, _, _, _ = Cat.row_storage meta in
  let* cur = S.cursor_open tx tree_id in
  let _sr = S.cursor_first cur in
  let found = ref false in
  let rec scan () =
    if !found
    then ()
    else (
      match S.cursor_next cur with
      | None -> ()
      | Some (_k, vbytes) ->
        let row = decode_with_virtual None [||] meta vbytes in
        if pred row then found := true else scan ())
  in
  scan ();
  S.cursor_close cur;
  Lwt.return !found
;;

let full_scan_collect tx (meta : Cat.table_meta) (pred : Row.t -> bool)
  : (int64 * Row.t) list Lwt.t
  =
  let tree_id, _, _, _ = Cat.row_storage meta in
  let* cur = S.cursor_open tx tree_id in
  let _sr = S.cursor_first cur in
  let buf = ref [] in
  let rec scan () =
    match S.cursor_next cur with
    | None -> ()
    | Some (kbytes, vbytes) ->
      let rowid = Rowid.decode kbytes in
      let row = decode_with_virtual None [||] meta vbytes in
      if pred row then buf := (rowid, row) :: !buf;
      scan ()
  in
  scan ();
  S.cursor_close cur;
  Lwt.return (List.rev !buf)
;;

(** #755/#765 review item 2: the CHILD index column types [index_lookup_values]
    should translate [parent_vals] through for a seek on [child_col_idxs], or
    [None] if the DECLARED type cannot be trusted as the column's ACTUAL
    index-key storage class.

    [Row.encode]'s [encode_col_value] enforces that invariant for every
    ordinary and STORED-generated column — a value whose runtime tag disagrees
    with [col.ty] raises there, so it can never reach storage — but a VIRTUAL
    generated column is always encoded as NULL and recomputed on read
    ([compute_virtual_generated_cols]) with NO such check: its expression's
    result can be a different storage class than the column declares (e.g.
    [x REAL GENERATED ALWAYS AS (y) VIRTUAL] where [y] is INTEGER), and THAT
    value, not the declared type, is what [row_value_to_index_value] put in
    the index. Seeking by the declared type would then walk a prefix keyed to
    a variant the index never holds.

    [None] here means "the index cannot be trusted for this seek", which the
    caller must treat as "fall back to the full scan" — {b not} as
    [index_lookup_values]'s own [None], "no stored key can equal this value".
    The full scan recomputes the row and compares with {!compare_values},
    which is correct regardless of what storage class the index physically
    holds.

    Also declines (returning [None]) an out-of-range ordinal, which keeps
    THIS function total, but that is {b not} a general "stale
    [child_col_idxs] degrades to the scan instead of crashing" guarantee
    (#765 review item 3 caught an earlier version of this comment claiming
    exactly that): the full-scan fallback's own predicate indexes
    [row.(ci)] with the very same [ci], so an out-of-range ordinal that
    reaches this call at all would crash there identically, just one step
    later. What actually prevents a stale ordinal from reaching either path
    is {!make_fk_recheck} re-resolving [fk_ordinal] and every column ordinal
    fresh against the schema AT RECHECK TIME (#765 review, rounds 1 and 2) —
    this function's own out-of-range check is defence in depth for a case
    that should not arise, not the mechanism that rules it out. *)
let child_index_key_types (child_meta : Cat.table_meta) (child_col_idxs : int list)
  : Row.ty list option
  =
  let cols = Array.of_list child_meta.Cat.columns in
  let n = Array.length cols in
  let rec go acc = function
    | [] -> Some (List.rev acc)
    | ci :: rest ->
      if ci < 0 || ci >= n
      then None
      else (
        match cols.(ci).Row.generated_as with
        | Some (_, false) -> None (* VIRTUAL: declared type is not trustworthy *)
        | _ -> go (cols.(ci).Row.ty :: acc) rest)
  in
  go [] child_col_idxs
;;

(** #765 review item 5: the equality-over-columns check shared by
    {!seek_index_matches}'s per-candidate verification and by both
    {!fk_child_has_ref_multi_in_tx}'s and {!scan_child_rows_multi_tx}'s
    full-scan fallback predicate — one definition of "does this row match"
    instead of three copies of the same [List.for_all2]. *)
let fk_cols_match
      ~(child_col_idxs : int list)
      ~(parent_vals : Row.value list)
      (row : Row.t)
  : bool
  =
  List.for_all2 (fun ci pv -> compare_values row.(ci) pv = 0) child_col_idxs parent_vals
;;

(** #765 review item 4: the seek + prefix-walk shared by
    {!fk_child_has_ref_multi_in_tx} and {!scan_child_rows_multi_tx} — the two
    functions differed only in what they did with a matching row. [on_match]
    is called with each row whose [child_col_idxs] equal [parent_vals]
    (via {!compare_values}) among the rows [prefix] matches; returning [true]
    stops the walk early (an existence check), [false] continues to the next
    candidate (a collect). *)
let seek_index_matches
      tx
      (idx : Cat.index_info)
      (child_meta : Cat.table_meta)
      ~(prefix : bytes)
      ~(plen : int)
      ~(child_col_idxs : int list)
      ~(parent_vals : Row.value list)
      ~(on_match : int64 -> Row.t -> bool)
  : unit Lwt.t
  =
  let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
  (* O(log n) native seek; stop at the first non-matching prefix (#228/#229). *)
  let* cur = S.seek_ge tx idx.Cat.idx_tree_id seek_key in
  let stop = ref false in
  let exhausted = ref false in
  let rec walk () =
    if !stop || !exhausted
    then Lwt.return_unit
    else (
      match%lwt S.seek_next cur with
      | None ->
        exhausted := true;
        Lwt.return_unit
      | Some (ikey, _ival) ->
        if Bytes.length ikey >= plen + 8 && Bytes.equal (Bytes.sub ikey 0 plen) prefix
        then (
          let rowid = decode_index_key_rowid ikey in
          let child_tree_id, _, _, _ = Cat.row_storage child_meta in
          let* row_opt = S.get tx child_tree_id (Rowid.encode rowid) in
          match row_opt with
          | None -> walk ()
          | Some vbytes ->
            let row = decode_with_virtual None [||] child_meta vbytes in
            let ok = fk_cols_match ~child_col_idxs ~parent_vals row in
            if ok
            then (
              if on_match rowid row then stop := true;
              walk ())
            else walk ())
        else (
          exhausted := true;
          Lwt.return_unit))
  in
  let* () = walk () in
  S.seek_close cur;
  Lwt.return_unit
;;

(** Internal: scan [child_meta] within an already-open transaction (RO or RW)
    for any row whose [child_col_idxs] match [parent_vals].  Used by both the
    public store-opening variant below and the deferred FK recheck path
    (which must see writes performed in the active RW txn — opening a fresh
    [ro_begin] on the B+-tree backend would snapshot the pre-txn state and
    miss the about-to-commit rows). *)
let fk_child_has_ref_multi_in_tx
      (cat : Cat.t)
      tx
      (child_meta : Cat.table_meta)
      ~(child_col_idxs : int list)
      ~(parent_vals : Row.value list)
  =
  (* #765 review item 5: a NULL component always falls through to the scan
     below (three-valued equality never matches NULL), so that check runs
     FIRST -- otherwise both [Cat.find_index_covering_cols] and
     [child_index_key_types] (an [Array.of_list] apiece) run to completion
     only to be discarded on the [when] guard every time a NULL is present. *)
  if List.exists (fun v -> v = Row.V_null) parent_vals
  then full_scan_exists tx child_meta (fk_cols_match ~child_col_idxs ~parent_vals)
  else (
    match
      ( Cat.find_index_covering_cols
          cat
          ~table_name:child_meta.Cat.name
          ~col_idxs:child_col_idxs
      , child_index_key_types child_meta child_col_idxs )
    with
    | Some idx, Some child_tys ->
      (* #755: seek the index through {!index_lookup_values}'s exact
         cross-numeric translation, keyed by the CHILD column's declared type
         — the same rule {!nlj_probe_values} uses for a join probe (#743).
         [None] means no key of the child column's type can equal
         [parent_vals], which is the honest "no child row can reference this"
         answer, not a reason to widen into the full scan below. *)
      (match index_lookup_values (List.combine parent_vals child_tys) with
       | None -> Lwt.return false
       | Some iks ->
         let prefix, plen = encode_index_key_prefix iks in
         let found = ref false in
         let* () =
           seek_index_matches
             tx
             idx
             child_meta
             ~prefix
             ~plen
             ~child_col_idxs
             ~parent_vals
             ~on_match:(fun _rowid _row ->
               found := true;
               true (* stop at the first match: an existence check *))
         in
         Lwt.return !found)
    | _ -> full_scan_exists tx child_meta (fk_cols_match ~child_col_idxs ~parent_vals))
;;

(** Scan [child_meta] for any row where all [child_col_idxs] match [parent_vals]
    simultaneously.  When an index covers [child_col_idxs] as a leading prefix,
    use it; otherwise fall back to a full table scan.
    Opens and closes its own RO snapshot. *)
let fk_child_has_ref_multi
      (cat : Cat.t)
      store
      (child_meta : Cat.table_meta)
      ~(child_col_idxs : int list)
      ~(parent_vals : Row.value list)
  =
  S.with_ro store
  @@ fun ro_tx ->
  fk_child_has_ref_multi_in_tx cat ro_tx child_meta ~child_col_idxs ~parent_vals
;;

(** Internal: scan [parent_meta] within an already-open transaction (RO or
    RW) for a row matching [parent_vals] on [parent_idxs].  Used by the
    deferred FK recheck path to observe uncommitted writes in the active
    write txn. *)
let fk_parent_has_row_in_tx
      tx
      (parent_meta : Cat.table_meta)
      ~(parent_idxs : int list)
      ~(parent_vals : Row.value list)
  : bool Lwt.t
  =
  (* Columnar tables cannot be FK parents — no unique constraints, no indexes. *)
  if Cat.is_columnar parent_meta
  then Lwt.return false
  else (
    let parent_tree_id, _, _, _ = Cat.row_storage parent_meta in
    let* cur = S.cursor_open tx parent_tree_id in
    let _sr = S.cursor_first cur in
    let found = ref false in
    let rec scan () =
      if !found
      then ()
      else (
        match S.cursor_next cur with
        | None -> ()
        | Some (_k, vbytes) ->
          let row = decode_with_virtual None [||] parent_meta vbytes in
          let ok =
            List.for_all2
              (fun pi pv -> compare_values row.(pi) pv = 0)
              parent_idxs
              parent_vals
          in
          if ok then found := true else scan ())
    in
    scan ();
    S.cursor_close cur;
    Lwt.return !found)
;;

(** Scan [parent_meta] for a row matching [parent_vals] on [parent_idxs].
    Returns true iff such a row exists.  Used at INSERT/UPDATE time
    (immediate FK enforcement); opens and closes its own RO snapshot. *)
let fk_parent_has_row
      store
      (parent_meta : Cat.table_meta)
      ~(parent_idxs : int list)
      ~(parent_vals : Row.value list)
  : bool Lwt.t
  =
  S.with_ro store
  @@ fun ro_tx ->
  let* found = fk_parent_has_row_in_tx ro_tx parent_meta ~parent_idxs ~parent_vals in
  Lwt.return found
;;

(* Build the commit-time recheck for a deferred FK violation: it still stands
   iff a child row references [parent_vals] AND no parent row has them.

   #765 review, round 1: re-resolves column ordinals against the schema AT
   RECHECK TIME rather than reusing ones captured at enqueue time -- a
   [DROP COLUMN] on either table between the statement and COMMIT, in the
   same explicit transaction, can shift or invalidate a captured ordinal.

   #765 review, round 2: [~fk_ordinal] identifies the constraint ITSELF
   (see {!fk_ordinal}) instead of trusting captured column NAME strings --
   round 1's own fix, which re-resolved by name, turned out to have the
   analogous bug one level up: [ALTER TABLE ... RENAME COLUMN] rewrites
   [fk_local_cols]/[fk_parent_cols] in the catalog immediately
   ([Cat.rename_column]), so a name captured at enqueue time is stale by
   the time the recheck looks it up, and the constraint is wrongly reported
   as gone even though it is perfectly checkable under its new name. Finding
   the constraint by ordinal instead means [fk_now.Cat.fk_local_cols] /
   [fk_now.Cat.fk_parent_cols] below are ALREADY current -- the rename is
   simply reflected in them, nothing to chase.

   What ordinal lookup does NOT fix, and must not paper over: [DROP COLUMN]
   leaves a genuinely DANGLING name in [fk_local_cols]/[fk_parent_cols] (the
   catalog's [drop_column] does not touch [fk_constraints] at all), which is
   a real "this constraint can no longer be evaluated" state, not a "the
   constraint moved" one. The length check below reports that case as a LOUD
   failure -- consistent with the immediate enforcement path's own refusal
   for the same condition ([Exec.enforce_insert_fk]'s "some local columns not
   found in table") -- rather than silently returning "not violated", which
   would let a genuine violation through uncaught at COMMIT. *)
let make_fk_recheck (cat : Cat.t) ~child_name ~parent_name ~fk_ordinal ~parent_vals
  : Cat.pending_fk_recheck
  =
  { Cat.recheck =
      (fun (type m) (recheck_tx : m S.txn) ->
        match
          ( Cat.find_table_cached cat ~name:child_name
          , Cat.find_table_cached cat ~name:parent_name )
        with
        | None, _ | _, None -> Lwt.return false
        | Some child_now, Some parent_now ->
          (* #765 review round 3, item 3: [List.nth_opt] raises
             [Invalid_argument "List.nth"] for a NEGATIVE index rather than
             answering [None] -- it only degrades gracefully for an
             out-of-range POSITIVE one. [fk_ordinal] should never actually be
             negative (every caller resolves it via {!fk_ordinal} against the
             constraint it just built the recheck for), but every caller also
             falls back to the sentinel [-1] if that resolution somehow came
             back [None], so this guard turns an unreachable-in-practice
             invariant violation into a clear internal error instead of an
             [Invalid_argument] escaping a code path whose whole design intent
             is graceful, loud handling. *)
          if fk_ordinal < 0
          then
            Lwt.fail_with
              (Printf.sprintf
                 "internal error: no FK ordinal recorded for the pending check on '%s' \
                  referencing '%s'"
                 child_name
                 parent_name)
          else (
            match List.nth_opt child_now.Cat.fk_constraints fk_ordinal with
            | None -> Lwt.return false
            | Some fk_now ->
              let cci =
                List.filter_map
                  (find_col_idx_by_name_opt child_now.Cat.columns)
                  fk_now.Cat.fk_local_cols
              in
              let pci =
                List.filter_map
                  (find_col_idx_by_name_opt parent_now.Cat.columns)
                  fk_now.Cat.fk_parent_cols
              in
              if
                List.length cci <> List.length fk_now.Cat.fk_local_cols
                || List.length pci <> List.length fk_now.Cat.fk_parent_cols
              then
                Lwt.fail_with
                  (Printf.sprintf
                     "FOREIGN KEY constraint on '%s' (%s) referencing '%s' (%s) can no \
                      longer be evaluated: a participating column no longer exists"
                     child_name
                     (String.concat "," fk_now.Cat.fk_local_cols)
                     parent_name
                     (String.concat "," fk_now.Cat.fk_parent_cols))
              else
                let* has_child =
                  fk_child_has_ref_multi_in_tx
                    cat
                    recheck_tx
                    child_now
                    ~child_col_idxs:cci
                    ~parent_vals
                in
                if not has_child
                then Lwt.return false
                else
                  let* has_parent =
                    fk_parent_has_row_in_tx
                      recheck_tx
                      parent_now
                      ~parent_idxs:pci
                      ~parent_vals
                  in
                  Lwt.return (not has_parent)))
  }
;;

(** Helper for FK enforcement: routes a violation either to the pending
    queue (deferred) or raises immediately (immediate).  [recheck] is the
    closure invoked at commit time; it must return true iff the violation
    is still present.

    [child_table]/[fk_ordinal] identify the SAME constraint [recheck] closes
    over — every caller already computes both for {!make_fk_recheck} — and
    are carried alongside the opaque closure (#765 review round 3) so an
    ALTER TABLE mutation site can ask "does a pending obligation still need
    this column" without invoking [recheck] itself, which performs the
    actual re-check rather than answering that question. *)
let fk_violation
      ~deferred
      (cat : Cat.t)
      ~kind
      ~table
      ~rowid
      ~msg
      ~(recheck : Cat.pending_fk_recheck)
      ~child_table
      ~fk_ordinal
  =
  if deferred
  then (
    Cat.queue_pending_fk_check
      cat
      { Cat.pfk_kind = kind
      ; Cat.pfk_table = table
      ; Cat.pfk_rowid = rowid
      ; Cat.pfk_message = msg
      ; Cat.pfk_recheck = recheck
      ; Cat.pfk_child_table = child_table
      ; Cat.pfk_fk_ordinal = fk_ordinal
      };
    Lwt.return_unit)
  else Lwt.fail_with msg
;;

(* Immediate/deferred FK existence check for one [fk] of an INSERT row. *)
let enforce_insert_fk
      store
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      (row : Row.t)
      (fk : Cat.fk_constraint)
  : unit Lwt.t
  =
  let is_deferred = fk.fk_deferrable || Cat.get_defer_fks_pragma cat in
  (* #765 review round 4 (non-blocking duplication note): [resolve_fk_col_idxs]
     instead of hand-rolling the same [_opt]-then-check pattern this function
     originated, now that every other FK column-resolution site shares it. *)
  let* local_idxs =
    resolve_fk_col_idxs
      ~table_name:table_meta.Cat.name
      table_meta.Cat.columns
      fk.fk_local_cols
  in
  let local_vals = List.map (fun i -> row.(i)) local_idxs in
  (* NULL in any FK column => skip enforcement *)
  if any_null_val local_vals
  then Lwt.return_unit
  else (
    match Cat.find_table_cached cat ~name:fk.fk_parent_table with
    | None ->
      Lwt.fail_with
        (Printf.sprintf "FOREIGN KEY: parent table '%s' not found" fk.fk_parent_table)
    | Some parent_meta ->
      let* parent_idxs =
        resolve_fk_col_idxs
          ~table_name:parent_meta.Cat.name
          parent_meta.Cat.columns
          fk.fk_parent_cols
      in
      let table_name = table_meta.Cat.name in
      let parent_meta_name = parent_meta.Cat.name in
      let msg =
        Printf.sprintf
          "FOREIGN KEY constraint failed: no row in '%s' where %s matches"
          fk.fk_parent_table
          (String.concat ", " fk.fk_parent_cols)
      in
      let* found =
        fk_parent_has_row store parent_meta ~parent_idxs ~parent_vals:local_vals
      in
      if found
      then Lwt.return_unit
      else (
        (* #765 review: [make_fk_recheck] re-resolves the constraint
               (via [fk_ordinal], stable across a mid-transaction RENAME
               COLUMN or DROP COLUMN) and both column lists against the
               schema AT RECHECK TIME, rather than reusing anything captured
               here at INSERT time. *)
        let ord = Option.value (fk_ordinal table_meta fk) ~default:(-1) in
        let recheck =
          make_fk_recheck
            cat
            ~child_name:table_name
            ~parent_name:parent_meta_name
            ~fk_ordinal:ord
            ~parent_vals:local_vals
        in
        fk_violation
          ~deferred:is_deferred
          cat
          ~kind:`Insert
          ~table:table_name
          ~rowid:0L
          ~msg
          ~recheck
          ~child_table:table_name
          ~fk_ordinal:ord))
;;

(* Evaluate all FK constraints for an INSERT of [row] before any writes. *)
let enforce_insert_fks store (cat : Cat.t) (table_meta : Cat.table_meta) (row : Row.t)
  : unit Lwt.t
  =
  let fks = table_meta.Cat.fk_constraints in
  if fks = [] || not (Cat.get_fk_enforcement cat)
  then Lwt.return_unit
  else Lwt_list.iter_s (enforce_insert_fk store cat table_meta row) fks
;;

(* Resolve the rowid for an INSERT: the INTEGER PRIMARY KEY for WITHOUT ROWID
   tables (must be present, non-NULL, integer), else a freshly allocated one. *)
let insert_rowid
      ?(defer_counter = false)
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      (row : Row.t)
  : int64 Lwt.t
  =
  let _, _, without_rowid, autoincrement = Cat.row_storage table_meta in
  if without_rowid
  then (
    match
      List.find_index (fun (c : Row.column) -> c.primary_key) table_meta.Cat.columns
    with
    | None ->
      Lwt.fail_with
        (Printf.sprintf
           "WITHOUT ROWID table '%s' has no PRIMARY KEY column"
           table_meta.Cat.name)
    | Some pk_idx ->
      (match row.(pk_idx) with
       | Row.V_int n -> Lwt.return n
       | Row.V_null ->
         Lwt.fail_with
           (Printf.sprintf
              "WITHOUT ROWID table '%s': PRIMARY KEY column must not be NULL"
              table_meta.Cat.name)
       | _ ->
         Lwt.fail_with
           (Printf.sprintf
              "WITHOUT ROWID table '%s': PRIMARY KEY column must be INTEGER"
              table_meta.Cat.name)))
  else (
    match Cat.rowid_alias_col table_meta with
    | Some pk_idx ->
      (* #243 (T1): INTEGER PRIMARY KEY IS the rowid.  Use the supplied integer
         as the table key; on NULL/omitted, auto-allocate and write it back so
         [SELECT id] / RETURNING observe the assigned value.  An explicit value
         advances the autoincrement counter past it (SQLite parity: a later NULL
         insert gets max(existing)+1). *)
      (match row.(pk_idx) with
       | Row.V_int n ->
         let* () =
           if Int64.compare n Int64.max_int < 0
           then
             Cat.bump_next_rowid_in_txn
               ~defer_counter
               cat
               ~name:table_meta.name
               ~at_least:(Int64.add n 1L)
               tx
           else if autoincrement
           then
             (* #312: pin the AUTOINCREMENT counter at max_int so the next
                auto-allocation detects exhaustion and raises SQLITE_FULL. *)
             Cat.bump_next_rowid_in_txn
               ~defer_counter
               cat
               ~name:table_meta.name
               ~at_least:Int64.max_int
               tx
           else Lwt.return_unit
         in
         Lwt.return n
       | Row.V_null ->
         let* id = Cat.next_rowid_in_txn ~defer_counter cat ~name:table_meta.name tx in
         row.(pk_idx) <- Row.V_int id;
         Lwt.return id
       | _ ->
         Lwt.fail_with
           (Printf.sprintf
              "datatype mismatch: INTEGER PRIMARY KEY column '%s' requires an integer"
              (List.nth table_meta.columns pk_idx).Row.name))
    | None -> Cat.next_rowid_in_txn ~defer_counter cat ~name:table_meta.name tx)
;;

(* SQLite-faithful UNIQUE violation message: "UNIQUE constraint failed: t.a"
   (each column listed as "<table>.<col>", comma-separated for composite keys).
   Shared by every secondary-index uniqueness check — INSERT-time
   ([check_insert_unique]), UPDATE-time ([check_index_unique_on_update]) and the
   CREATE UNIQUE INDEX build (#288) — so all three report identically, and match
   the rowid-alias/PRIMARY KEY paths that already use this exact wording. *)
let unique_constraint_failed_msg ~(table : string) ~(columns : string list) : string =
  Printf.sprintf
    "UNIQUE constraint failed: %s"
    (String.concat ", " (List.map (fun c -> table ^ "." ^ c) columns))
;;

(* Probe ONE unique index for a conflict with [row_for_idx], returning the
   conflicting row's rowid.  [None] means "no conflict", which includes the two
   cases that exempt the row from the probe entirely: a partial index whose
   WHERE the row does not match, and #290's NULL exemption (SQLite treats every
   NULL as distinct in a UNIQUE index — such a row is still written to the index
   tree, it just never conflicts).  Callers must have checked [idx_unique]. *)
let probe_unique_conflict
      tx
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~(row_for_idx : Row.t)
      (idx : Cat.index_info)
  : int64 option Lwt.t
  =
  if not (row_matches_index_where clock params idx table_meta.columns row_for_idx)
  then Lwt.return_none
  else (
    let key_vals = get_index_key_values clock params idx table_meta.columns row_for_idx in
    if any_null_val key_vals
    then Lwt.return_none
    else (
      let iks = List.map row_value_to_index_value key_vals in
      let prefix, plen = encode_index_key_prefix iks in
      let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
      (* O(log n) native probe: only the first entry >= seek_key is needed to
         detect a duplicate prefix — never drain the whole index (#229). *)
      let* cur = S.seek_ge tx idx.idx_tree_id seek_key in
      let* first = S.seek_next cur in
      let conflict_rowid_opt =
        match first with
        | None -> None
        | Some (ikey, _) ->
          if Bytes.length ikey >= plen && Bytes.equal (Bytes.sub ikey 0 plen) prefix
          then (
            let rid_bytes = Bytes.sub ikey plen (Bytes.length ikey - plen) in
            Some (Rowid.decode rid_bytes))
          else None
      in
      S.seek_close cur;
      Lwt.return conflict_rowid_opt))
;;

(* #639: does this index carry the constraint an ON CONFLICT clause names?
   [idx_unique] is part of the test on purpose — a conflict target must name a
   uniqueness constraint, so a non-unique index with the same column list is not
   one, and falls through to the modifier like any other index. *)
let index_is_conflict_target
      (idx : Cat.index_info)
      ~(upsert_update : (string list * (int * Plan.expr) list) option)
  : bool
  =
  match upsert_update with
  | None -> false
  | Some (conflict_cols, _) ->
    idx.Cat.idx_unique
    && List.sort String.compare idx.Cat.idx_columns
       = List.sort String.compare conflict_cols
;;

(* #669: the result of an INSERT's UNIQUE pre-check. Only these four shapes
   are meaningful — [check_insert_unique] used to return
   [bool * int64 list * int64 option] ((skip, rowids to delete for REPLACE,
   rowid to update for UPSERT)), of which only three of the eight
   representable tuples ever occurred; the fourth combination the type
   allowed but the code never produced, [upsert_rid = Some _] alongside a live
   [skip] or a non-empty [dels], was exactly the shape #639's review had to
   pin with a comment (see [execute_insert]'s dispatch below) because the
   tuple could not say it was impossible. The variant makes it unrepresentable. *)
type insert_conflict =
  | Ic_plain (** no conflict: write the row normally. *)
  | Ic_skip (** [OR IGNORE] (or an ON CONFLICT target skip): do nothing. *)
  | Ic_replace of int64 list (** [OR REPLACE]: displace these rowids first. *)
  | Ic_upsert of int64 (** an ON CONFLICT ... DO UPDATE target: update this rowid. *)

(* #669 (review): the subset of [insert_conflict] that [execute_insert_write]
   can actually receive. [execute_insert]'s dispatch always routes [Ic_upsert]
   to [execute_upsert_update] before this function is called, so unlike the
   first cut of this refactor, that case is not just handled by a comment and
   a runtime [failwith] inside [execute_insert_write] — it is unrepresentable
   in the type that function's [~conflict] parameter actually takes. *)
type insert_write_conflict =
  | Iw_plain
  | Iw_skip
  | Iw_replace of int64 list

(* #669 (review): total over [insert_write_conflict], so the one place that
   needs the REPLACE rowid list out of a conflict does not have to re-match
   inside an arm already narrowed to [Iw_plain]/[Iw_replace] with a dead
   fallback for the [Iw_skip] that arm can never hold. *)
let insert_write_to_delete = function
  | Iw_replace dels -> dels
  | Iw_plain | Iw_skip -> []
;;

(* Converts at [execute_insert]'s dispatch: the [| _ ->] arm below is entered
   only when [conflict] is NOT [Ic_upsert] paired with a live upsert clause
   (that pair is matched and routed to [execute_upsert_update] first), so an
   [Ic_upsert] reaching here is unreachable by construction rather than by
   this function's own logic — see the comment on that dispatch match. *)
let to_insert_write_conflict = function
  | Ic_plain -> Iw_plain
  | Ic_skip -> Iw_skip
  | Ic_replace dels -> Iw_replace dels
  | Ic_upsert _ ->
    failwith
      "execute_insert: unreachable Ic_upsert reaching execute_insert_write (handled by \
       the dispatch match above)"
;;

(* UNIQUE pre-check for INSERT: returns the conflict [check_insert_unique]
   found. Raises on a plain UNIQUE violation.

   #639: the conflict TARGET is probed first, in its own pass, and supersedes
   everything else when it hits. That is not a micro-optimisation — it is what
   makes the answer well-defined. The single fold this replaced let a conflict
   on ANY index decide, so with two unique indexes and a row conflicting on
   both, the outcome depended on the order [Cat.indexes_for_table] happened to
   return them in (newest-first, i.e. on `CREATE UNIQUE INDEX` order): the
   target seen first gave an upsert, the other seen first gave a skip under
   [CA_ignore]. Same schema, same statement, two answers.

   When the target hits, the result is [Ic_upsert rid] — the accumulated
   [skip]/[dels] the fold below might otherwise have produced are discarded
   entirely, not carried alongside it:

   - the skip must not survive. The upsert supersedes the insert, and a skip
     decided against a row that is no longer being inserted is meaningless.
   - the queued deletes must not survive either. Those rowids were queued for
     deletion because they conflicted with the row being INSERTED — and that
     row is discarded in favour of updating [rid], so nothing should be
     displaced. Dropping them silently would be worse than either answer:
     [execute_insert]'s upsert branch never calls [delete_replace_conflicts],
     so a queued delete reaching that branch would be a delete that never
     happens.

   What is NOT true, and was claimed here in the first revision of #639: that
   the DO UPDATE's result is re-checked against the other unique indexes BY
   THIS PASS. [write_row_rekeyed] itself still does no uniqueness probe — its
   index loop is an unconditional [S.del] of the old key and [S.put] of the
   new one — but #667 added that check at [execute_upsert_update]'s call site,
   which every producer of [upsert_rowid] here (this function's target-hit
   reset above, and the alias-PK probe in [execute_insert]) eventually
   reaches. So discarding the other indexes' verdicts in THIS pass is sound
   without loss: they were computed against the row being INSERTED, which is
   discarded, the DO UPDATE may not touch those columns at all, and if it does
   write a duplicate into another unique index, #667's check downstream
   catches it. *)
let check_insert_unique
      tx
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~(row_for_idx : Row.t)
      ~(on_conflict : Ast.conflict_action option)
      ~(upsert_update : (string list * (int * Plan.expr) list) option)
      (idxs : Cat.index_info list)
  : insert_conflict Lwt.t
  =
  let targets, others =
    List.partition (fun idx -> index_is_conflict_target idx ~upsert_update) idxs
  in
  let* target_hit =
    Lwt_list.fold_left_s
      (fun acc idx ->
         match acc with
         | Some _ -> Lwt.return acc
         | None -> probe_unique_conflict tx table_meta ~clock ~params ~row_for_idx idx)
      None
      targets
  in
  match target_hit with
  | Some old_rowid -> Lwt.return (Ic_upsert old_rowid)
  | None ->
    (* No target conflict (or no target at all): the modifier governs, exactly
       as it did before #639. The fold can never produce [Ic_upsert] — every
       index that could have set one is in [targets]. *)
    Lwt_list.fold_left_s
      (fun acc (idx : Cat.index_info) ->
         match acc with
         | Ic_skip -> Lwt.return acc
         | _ when not idx.idx_unique -> Lwt.return acc
         | _ ->
           let* conflict =
             probe_unique_conflict tx table_meta ~clock ~params ~row_for_idx idx
           in
           (match conflict with
            | None -> Lwt.return acc
            | Some old_rowid ->
              (match on_conflict with
               | Some Ast.CA_ignore -> Lwt.return Ic_skip (* stop checking *)
               | Some Ast.CA_replace ->
                 let dels =
                   match acc with
                   | Ic_replace dels -> dels
                   | _ -> []
                 in
                 Lwt.return (Ic_replace (old_rowid :: dels))
               | _ ->
                 Lwt.fail_with
                   (unique_constraint_failed_msg
                      ~table:table_meta.Cat.name
                      ~columns:idx.idx_columns))))
      Ic_plain
      others
;;

(* Write [row]'s index entries (honoring each index's WHERE predicate). *)
let insert_row_indexes
      tx
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~(row_for_idx : Row.t)
      ~rowid
      (idxs : Cat.index_info list)
  : unit Lwt.t
  =
  Lwt_list.iter_s
    (fun (idx : Cat.index_info) ->
       if not (row_matches_index_where clock params idx table_meta.columns row_for_idx)
       then Lwt.return_unit
       else (
         let iks =
           List.map
             row_value_to_index_value
             (get_index_key_values clock params idx table_meta.columns row_for_idx)
         in
         let ikey = Index_key.encode iks ~rowid in
         S.put tx idx.idx_tree_id ikey Bytes.empty))
    idxs
;;

(* [row_for_idx] must already have VIRTUAL generated columns applied (e.g.
   via [decode_with_virtual] or [with_computed_virtuals]).  Callers that
   obtain the row from [decode_with_virtual] can pass it directly — virtual
   cols are computed in-place there, so no extra [Array.copy] is needed. *)
let delete_row_indexes
      tx
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~(row_for_idx : Row.t)
      ~rowid
      indexes
  : unit Lwt.t
  =
  let schema = table_meta.Cat.columns in
  Lwt_list.iter_s
    (fun (idx : Cat.index_info) ->
       if not (row_matches_index_where clock params idx schema row_for_idx)
       then Lwt.return_unit
       else (
         let iks =
           List.map
             row_value_to_index_value
             (get_index_key_values clock params idx schema row_for_idx)
         in
         let old_ikey = Index_key.encode iks ~rowid in
         S.del tx idx.idx_tree_id old_ikey))
    indexes
;;

(* REPLACE conflict resolution: delete each [to_delete] row and its index
   entries (firing BEFORE DELETE); returns the displaced rows in original order. *)
let delete_replace_conflicts
      tx
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~(idxs : Cat.index_info list)
      ~on_replace_delete_before
      to_delete
  : Row.t list Lwt.t
  =
  let displaced_rows : Row.t list ref = ref [] in
  let* () =
    Lwt_list.iter_s
      (fun old_rowid ->
         let old_key = Rowid.encode old_rowid in
         let* old_bytes_opt =
           S.get
             tx
             (let x, _, _, _ = Cat.row_storage table_meta in
              x)
             old_key
         in
         match old_bytes_opt with
         | None -> Lwt.return_unit
         | Some old_bytes ->
           let old_row = decode_with_virtual clock params table_meta old_bytes in
           displaced_rows := old_row :: !displaced_rows;
           (* #417: REPLACE/INSERT OR REPLACE displaces a conflicting row before
              re-inserting; record the removed row so the delta feed pairs this
              [Deleted] with the [Inserted] the insert path emits. *)
           record_change
             table_meta.Cat.name
             (Deleted { rowid = old_rowid; row = old_row });
           let* () =
             match on_replace_delete_before with
             | None -> Lwt.return_unit
             | Some f -> f ~tx ~old_row
           in
           let* () =
             S.del
               tx
               (let x, _, _, _ = Cat.row_storage table_meta in
                x)
               old_key
           in
           (* old_row from decode_with_virtual already has VIRTUAL cols applied *)
           delete_row_indexes
             tx
             table_meta
             ~clock
             ~params
             ~row_for_idx:old_row
             ~rowid:old_rowid
             idxs)
      (List.sort_uniq compare to_delete)
  in
  Lwt.return (List.rev !displaced_rows)
;;

(** Check whether inserting a new index entry with [key_vals] and [rowid]
    into [idx] would violate a UNIQUE constraint.  Returns [true] if a
    different row already has the same indexed value.

    [key_vals] must already be the caller's fully-evaluated index-key
    values (virtuals/expressions computed, in the ambient statement's real
    [~clock]/[~params]) — this function does no evaluation of its own.  It
    used to: it independently recomputed the row's index values via
    [with_computed_virtuals_cols None [||]], hardcoding a fresh clock and
    no bound params instead of reusing the ones its only caller,
    [check_index_unique_on_update], had already evaluated [new_vs] with a
    few lines above. For a UNIQUE index over a clock-dependent VIRTUAL
    column (e.g. one referencing CURRENT_TIMESTAMP), that let the probe's
    seek key diverge from the value just validated — a different instant
    than the one the caller reasoned about, which could miss a real
    duplicate or seek on a value nothing else holds. Taking the
    already-computed [key_vals] removes the second evaluation entirely
    rather than just aligning its inputs. *)
let unique_violation_on_update
      (tx : S.rw S.txn)
      (idx : Cat.index_info)
      (key_vals : Row.value list)
      ~(rowid : int64)
  : bool Lwt.t
  =
  (* #290: a key with ANY NULL column is exempt — NULLs are distinct in a SQLite
     UNIQUE index, so it can never collide.  Short-circuit before probing.
     (Already true of [key_vals] by the time [check_index_unique_on_update]
     calls this — kept here too since this is the one place the seek logic
     lives, in case a future caller doesn't pre-filter.) *)
  if any_null_val key_vals
  then Lwt.return false
  else (
    let ik_values = List.map row_value_to_index_value key_vals in
    (* Encode all values (no rowid) as the exact-match key; [encode_index_key_prefix]
     concatenates each value's encoding in order, same as the index key body. *)
    let full_key_no_rowid, full_klen = encode_index_key_prefix ik_values in
    let prefix =
      match ik_values with
      | [] -> Bytes.empty
      | ik :: _ -> Index_key.encode_value ik
    in
    let plen = Bytes.length prefix in
    let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
    (* O(log n) native seek; scan only the matching prefix range (#229). *)
    let* cur = S.seek_ge tx idx.idx_tree_id seek_key in
    (* Scan entries while the value prefix matches.  A different rowid
     with the same full value sequence is a UNIQUE violation. *)
    let rec scan () =
      match%lwt S.seek_next cur with
      | None -> Lwt.return false
      | Some (ikey, _) ->
        if Bytes.length ikey >= plen + 8 && Bytes.equal (Bytes.sub ikey 0 plen) prefix
        then
          (* Check that the full value prefix (all columns) also matches *)
          if
            Bytes.length ikey >= full_klen + 8
            && Bytes.equal (Bytes.sub ikey 0 full_klen) full_key_no_rowid
          then (
            let rowid_bytes = Bytes.sub ikey (Bytes.length ikey - 8) 8 in
            let other = Rowid.decode rowid_bytes in
            if Int64.equal other rowid then scan () else Lwt.return true)
          else scan ()
        else Lwt.return false
    in
    let* result = scan () in
    S.seek_close cur;
    Lwt.return result)
;;

(* Check one unique index for an UPDATE that turns [old_row] into [new_row]
   (with virtuals computed in [new_row_for_idx]); fails the Lwt thread on a
   duplicate. *)
let check_index_unique_on_update
      tx
      (idx : Cat.index_info)
      ~clock
      ~params
      ~schema
      ~old_row
      ~new_row_for_idx
      ~rowid
  : unit Lwt.t
  =
  if not idx.idx_unique
  then Lwt.return_unit
  else if not (row_matches_index_where clock params idx schema new_row_for_idx)
  then Lwt.return_unit
  else (
    let old_vs = get_index_key_values clock params idx schema old_row in
    let new_vs = get_index_key_values clock params idx schema new_row_for_idx in
    (* #290: a new key with ANY NULL column is exempt — NULLs are distinct in a
       SQLite UNIQUE index, so the updated row can never conflict.  (The index
       entry itself is still maintained by the regular update path.) *)
    if any_null_val new_vs
    then Lwt.return_unit
    else (
      let values_equal a b =
        match a, b with
        | Row.V_null, Row.V_null -> true
        | Row.V_int x, Row.V_int y -> Int64.equal x y
        | Row.V_text x, Row.V_text y -> String.equal x y
        | Row.V_real x, Row.V_real y -> Float.equal x y
        | Row.V_blob x, Row.V_blob y -> Bytes.equal x y
        | _ -> false
      in
      (* A partial index's WHERE membership can flip true without the indexed
         COLUMNS changing at all — [old_row] not matching [idx_where_sql] is
         itself a change, because the row was never a live entry in this index
         to compare against. Gating [unchanged] on that too (not just on
         [old_vs]/[new_vs]) is what makes a WHERE-only transition fall through
         to the real probe below instead of being waved through as "nothing
         moved". *)
      let old_matched = row_matches_index_where clock params idx schema old_row in
      let unchanged = old_matched && List.for_all2 values_equal old_vs new_vs in
      if unchanged
      then Lwt.return_unit
      else
        let* dup = unique_violation_on_update tx idx new_vs ~rowid in
        if dup
        then
          Lwt.fail_with
            (unique_constraint_failed_msg
               ~table:idx.Cat.idx_table
               ~columns:idx.idx_columns)
        else Lwt.return_unit))
;;

(* #667/#692 review: both call sites that need a pre-write uniqueness probe
   over every index — [validate_update_unique] for plain UPDATE,
   [execute_upsert_update] for UPSERT DO UPDATE — ran the identical loop over
   [check_index_unique_on_update] by hand. Sharing it here is what keeps a
   future change to the loop (a new exemption, an early exit, batching) from
   being applied at one call site and forgotten at the other. *)
let check_indexes_unique_on_update
      tx
      (indexes : Cat.index_info list)
      ~clock
      ~params
      ~schema
      ~old_row
      ~new_row_for_idx
      ~rowid
  : unit Lwt.t
  =
  Lwt_list.iter_s
    (fun (idx : Cat.index_info) ->
       check_index_unique_on_update
         tx
         idx
         ~clock
         ~params
         ~schema
         ~old_row
         ~new_row_for_idx
         ~rowid)
    indexes
;;

(* #243/#249: write [new_row] for the row currently stored at [old_rowid],
   MOVING it to a new table-tree key when the INTEGER PRIMARY KEY alias column
   changed — with a uniqueness probe on the new key — and re-keying its
   secondary-index entries (the rowid is their key suffix) old->new.  For a
   non-alias table or an unchanged alias this is an in-place rewrite.  Returns
   the (possibly new) rowid.

   Every single-row UPDATE path (UPDATE, UPSERT DO UPDATE, ON UPDATE CASCADE)
   funnels through this so none can independently re-introduce the divergence
   between the stored key and the id column (#249). *)
let write_row_rekeyed
      tx
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~(old_row : Row.t)
      ~(new_row : Row.t)
      ~old_rowid
      ~indexes
      ?(new_row_for_idx : Row.t option)
      ()
  : int64 Lwt.t
  =
  let alias_col = Cat.rowid_alias_col table_meta in
  let* new_rowid =
    match alias_col with
    | None -> Lwt.return old_rowid
    | Some i ->
      (match new_row.(i) with
       | Row.V_int n -> Lwt.return n
       | _ ->
         Lwt.fail_with
           (Printf.sprintf
              "datatype mismatch: INTEGER PRIMARY KEY column '%s' requires an integer"
              (List.nth table_meta.Cat.columns i).Row.name))
  in
  let* () =
    if Int64.equal new_rowid old_rowid
    then Lwt.return_unit
    else (
      let upd_tree_id, _, _, _ = Cat.row_storage table_meta in
      let* existing = S.get tx upd_tree_id (Rowid.encode new_rowid) in
      match existing with
      | None -> Lwt.return_unit
      | Some _ ->
        let col_name =
          match alias_col with
          | Some i -> (List.nth table_meta.Cat.columns i).Row.name
          | None -> "rowid"
        in
        Lwt.fail_with
          (Printf.sprintf "UNIQUE constraint failed: %s.%s" table_meta.Cat.name col_name))
  in
  let schema = table_meta.Cat.columns in
  (* #567: every single-row UPDATE path (UPDATE, UPSERT DO UPDATE, ON UPDATE
     CASCADE / SET NULL / SET DEFAULT) funnels through here, so this is the one
     place the new row's NULLs have to be checked.  Raises before any index or
     row write.  #629: [clock]/[params] let it see a VIRTUAL generated column's
     computed value — an UPDATE to a base column can make one NULL. *)
  enforce_not_null ~clock ~params table_meta new_row;
  let old_row_for_idx = with_computed_virtuals clock params table_meta old_row in
  (* Callers that have already computed [new_row]'s virtuals for their own
     uniqueness probe (#667: [execute_upsert_update]) pass it through here
     instead of paying for a second evaluation of every VIRTUAL generated
     column's expression. *)
  let new_row_for_idx =
    match new_row_for_idx with
    | Some v -> v
    | None -> with_computed_virtuals clock params table_meta new_row
  in
  let* () =
    Lwt_list.iter_s
      (fun (idx : Cat.index_info) ->
         let old_matches =
           row_matches_index_where clock params idx schema old_row_for_idx
         in
         let new_matches =
           row_matches_index_where clock params idx schema new_row_for_idx
         in
         let old_iks =
           List.map
             row_value_to_index_value
             (get_index_key_values clock params idx schema old_row_for_idx)
         in
         let new_iks =
           List.map
             row_value_to_index_value
             (get_index_key_values clock params idx schema new_row_for_idx)
         in
         let old_ikey = Index_key.encode old_iks ~rowid:old_rowid in
         let new_ikey = Index_key.encode new_iks ~rowid:new_rowid in
         let* () =
           if old_matches then S.del tx idx.idx_tree_id old_ikey else Lwt.return_unit
         in
         if new_matches
         then S.put tx idx.idx_tree_id new_ikey Bytes.empty
         else Lwt.return_unit)
      indexes
  in
  let upd_tree_id2, _, _, _ = Cat.row_storage table_meta in
  let new_bytes = Row.encode schema new_row in
  let* () = S.del tx upd_tree_id2 (Rowid.encode old_rowid) in
  let* () = S.put tx upd_tree_id2 (Rowid.encode new_rowid) new_bytes in
  Lwt.return new_rowid
;;

(* UPSERT DO UPDATE: apply [assigns] to conflicting row [old_rowid], refresh
   indexes, fire BEFORE/AFTER UPDATE hooks, commit if we own the txn. *)
let execute_upsert_update
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~owned
      ~(row : Row.t)
      ~(assigns : (int * Plan.expr) list)
      ~old_rowid
      ~on_upsert_update_before
      ~on_upsert_update
  : bool Lwt.t
  =
  let old_key = Rowid.encode old_rowid in
  let* old_bytes_opt =
    S.get
      tx
      (let x, _, _, _ = Cat.row_storage table_meta in
       x)
      old_key
  in
  match old_bytes_opt with
  | None ->
    let* () = if owned then S.rollback tx else Lwt.return_unit in
    Lwt.return false
  | Some old_bytes ->
    let old_row = decode_with_virtual clock params table_meta old_bytes in
    let new_row = Array.copy old_row in
    List.iter
      (fun (col_ord, expr) ->
         let e' = substitute_excluded row expr in
         new_row.(col_ord) <- eval_expr clock params old_row e')
      assigns;
    compute_stored_generated_cols clock params table_meta new_row;
    eval_check_constraints clock params table_meta new_row;
    let* () =
      match on_upsert_update_before with
      | None -> Lwt.return_unit
      | Some f -> f ~tx ~old_row ~new_row
    in
    let indexes = Cat.indexes_for_table cat ~table:table_meta.name in
    (* #667: [write_row_rekeyed]'s index loop is an unconditional del/put with
       no uniqueness probe of its own — it is shared with a plain UPDATE and
       ON UPDATE CASCADE, so it cannot gain one without changing those paths
       too (see the comment on [validate_update_unique]'s equivalent pass).
       Run the same per-index check here instead, before the row moves:
       [check_index_unique_on_update] excludes [old_rowid]'s own entry from
       the probe and exempts an unchanged key and a NULL-containing key
       (#290), so this fires only on a DO UPDATE that writes a value another
       row already holds in a UNIQUE index — including the index that named
       the conflict target, if the assignment moved that column too. *)
    let new_row_for_idx = with_computed_virtuals clock params table_meta new_row in
    let* () =
      check_indexes_unique_on_update
        tx
        indexes
        ~clock
        ~params
        ~schema:table_meta.Cat.columns
        ~old_row
        ~new_row_for_idx
        ~rowid:old_rowid
    in
    (* #249: SET id = N in a DO UPDATE must move the row (and check uniqueness),
       same as a plain UPDATE — funnel through the shared re-key helper.
       [new_row_for_idx] was already computed for the loop above; hand it
       through instead of paying for a second VIRTUAL-column evaluation. *)
    let* (_ : int64) =
      write_row_rekeyed
        tx
        table_meta
        ~clock
        ~params
        ~old_row
        ~new_row
        ~old_rowid
        ~indexes
        ~new_row_for_idx
        ()
    in
    let* () =
      match on_upsert_update with
      | None -> Lwt.return_unit
      | Some f -> f ~tx ~old_row ~new_row
    in
    (* #417: secondary-index UPSERT DO UPDATE writes via [write_row_rekeyed],
       bypassing the marked plain-insert/update loops; record the pre/post images
       here so the delta feed covers it. *)
    record_change table_meta.Cat.name (Updated { rowid = old_rowid; old_row; new_row });
    let* () = release_txn ~cat tx owned in
    Lwt.return true
;;

(* Plain INSERT path (no UPSERT match from secondary indexes): honor IGNORE
   (skip), delete REPLACE conflicts, write the new row + index entries using
   [S.put_x] (combined check+write) when [alias_explicit=true] to avoid a
   separate pre-read for the alias PK uniqueness check (#350). *)
let execute_insert_write
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~owned
      ~(row : Row.t)
      ~(row_for_idx : Row.t)
      ~rowid
      ~(idxs : Cat.index_info list)
      ~(conflict : insert_write_conflict)
      ~alias_explicit
      ~alias_col_name
      ~(on_conflict : Ast.conflict_action option)
      ~(upsert_update : (string list * (int * Plan.expr) list) option)
      ~on_replace_delete_before
      ~on_replace_delete
      ~on_upsert_update_before
      ~on_upsert_update
      ~after_hook
  : bool Lwt.t
  =
  (* #669 (review): [conflict]'s type no longer has an [Ic_upsert]-shaped case
     to exclude — [execute_insert]'s dispatch converts to
     [insert_write_conflict] only in the branch that already routed
     [Ic_upsert] to [execute_upsert_update] instead of here. *)
  match conflict with
  | Iw_skip ->
    (* IGNORE from secondary-index pre-check: rollback if owned.
       [on_conflict = CA_ignore] means [check_insert_unique] returned
       [Ic_skip] without ever producing [Ic_replace], so the write path below
       (including [delete_replace_conflicts]) is unreachable — no hooks have
       fired and no B-tree deletes have been made, so [S.rollback] is safe. *)
    let* () = if owned then S.rollback tx else Lwt.return_unit in
    Lwt.return false
  | Iw_plain | Iw_replace _ ->
    (* #567: the rowid-alias column has just been written back by
       [insert_rowid], so by now the row is exactly what will be encoded —
       check it before any REPLACE deletes or index writes happen.  #599:
       under [OR IGNORE] the violation skips the row instead of raising,
       which is what the same modifier already did for a UNIQUE conflict;
       every other resolution raises.  [Iw_skip] never reaches here (handled
       above), so unlike before #669 there is no separate guard needed to
       avoid double-evaluating this. *)
    (* #669 (review): [insert_write_to_delete] replaces the old inline
       re-match on [conflict] that needed a dead [Iw_skip] fallback — this
       one is total, so there is nothing to mark unreachable. *)
    let to_delete = insert_write_to_delete conflict in
    let null_skip = not_null_skip_or_fail ~clock ~params table_meta row ~on_conflict in
    if null_skip
    then
      (* Decided before any write, same as the [Iw_skip] arm above. *)
      let* () = if owned then S.rollback tx else Lwt.return_unit in
      Lwt.return false
    else
      let* displaced_rows =
        delete_replace_conflicts
          tx
          table_meta
          ~clock
          ~params
          ~idxs
          ~on_replace_delete_before
          to_delete
      in
      let key = Rowid.encode rowid in
      let bytes = Row.encode table_meta.columns row in
      (* #350: use put_x for explicit alias PK rows to combine the uniqueness
       check with the write in a single B-tree descent (1 descent on the
       no-conflict path; 2 for CA_replace since put_x does not write on
       conflict and a follow-up S.put is needed).  For all other cases (no
       alias col, auto-allocated rowid) just S.put — no alias PK conflict
       is possible. *)
      let* conflict_opt =
        if alias_explicit
        then
          S.put_x
            tx
            (let x, _, _, _ = Cat.row_storage table_meta in
             x)
            key
            bytes
        else
          let* () =
            S.put
              tx
              (let x, _, _, _ = Cat.row_storage table_meta in
               x)
              key
              bytes
          in
          Lwt.return None
      in
      (match conflict_opt with
       | None ->
         (* Common path: key was absent, write succeeded. *)
         let* () =
           insert_row_indexes tx table_meta ~clock ~params ~row_for_idx ~rowid idxs
         in
         let* () =
           match on_replace_delete with
           | None -> Lwt.return_unit
           | Some f -> Lwt_list.iter_s (fun old_row -> f ~tx ~old_row) displaced_rows
         in
         let* () =
           match after_hook with
           | None -> Lwt.return_unit
           | Some f -> f ~tx ~new_row:row
         in
         let* () = release_txn ~cat tx owned in
         Lwt.return true
       | Some _ ->
         (* Alias PK conflict detected by put_x (key present, NOT overwritten).
         put_x returns a sentinel [Some Bytes.empty] — callers that need the
         old row bytes (CA_replace) fetch them via S.get below. *)
         let col_name = Option.value alias_col_name ~default:"rowid" in
         (* #639: same reorder as [check_insert_unique] — an explicit ON CONFLICT
         clause naming this alias PK beats the statement's modifier, so
         `INSERT OR IGNORE ... ON CONFLICT(k) DO UPDATE` runs the DO UPDATE
         instead of silently skipping.  A modifier still governs a conflict the
         ON CONFLICT clause does not name (it falls through to the arms below).

         BACKSTOP, not the primary path: since the review of PR #652 the
         alias-PK target is probed in [execute_insert] BEFORE
         [check_insert_unique], because a secondary-index conflict resolved here
         would otherwise have already set [skip] or run
         [delete_replace_conflicts] by the time [put_x] discovers this one.  The
         arm is kept because it costs nothing and its absence would turn any
         hole in that probe into a bare "UNIQUE constraint failed" rather than
         the DO UPDATE the caller asked for. *)
         (match on_conflict, upsert_update with
          | _, Some (conflict_cols, assigns) when conflict_cols = [ col_name ] ->
            (* Alias PK is always a single column, so single-element equality
            suffices — no sort needed.  Fire AFTER DELETE for any secondary
            REPLACE displaced rows before handing off to the upsert path. *)
            let* () =
              match on_replace_delete with
              | None -> Lwt.return_unit
              | Some f -> Lwt_list.iter_s (fun r -> f ~tx ~old_row:r) displaced_rows
            in
            execute_upsert_update
              tx
              cat
              table_meta
              ~clock
              ~params
              ~owned
              ~row
              ~assigns
              ~old_rowid:rowid
              ~on_upsert_update_before
              ~on_upsert_update
          | Some Ast.CA_ignore, _ ->
            let* () = if owned then S.rollback tx else Lwt.return_unit in
            Lwt.return false
          | Some Ast.CA_replace, _ ->
            let* old_bytes_opt =
              S.get
                tx
                (let x, _, _, _ = Cat.row_storage table_meta in
                 x)
                key
            in
            let old_row =
              match old_bytes_opt with
              | Some b -> decode_with_virtual clock params table_meta b
              | None -> failwith "put_x conflict but row gone before CA_replace fetch"
            in
            (* Fire BEFORE DELETE for the alias-PK displaced row.  Note: when
            there are simultaneous secondary-index REPLACE conflicts, those
            hooks fired first (inside delete_replace_conflicts above), so the
            alias-PK BEFORE DELETE fires last. *)
            let* () =
              match on_replace_delete_before with
              | None -> Lwt.return_unit
              | Some f -> f ~tx ~old_row
            in
            (* Delete the alias-PK row's index entries.
            old_rowid = rowid by alias-PK invariant: the conflict is on this
            same key, so the displaced row's rowid equals the inserted rowid.
            old_row from decode_with_virtual already has VIRTUAL cols applied. *)
            let* () =
              delete_row_indexes
                tx
                table_meta
                ~clock
                ~params
                ~row_for_idx:old_row
                ~rowid
                idxs
            in
            let* () =
              S.put
                tx
                (let x, _, _, _ = Cat.row_storage table_meta in
                 x)
                key
                bytes
            in
            let* () =
              insert_row_indexes tx table_meta ~clock ~params ~row_for_idx ~rowid idxs
            in
            (* Fire AFTER DELETE for all displaced rows.  Use [displaced_rows @
            [old_row]] so alias-PK row fires last — matching the BEFORE DELETE
            order (secondary conflicts first, alias-PK last). *)
            let all_displaced = displaced_rows @ [ old_row ] in
            let* () =
              match on_replace_delete with
              | None -> Lwt.return_unit
              | Some f -> Lwt_list.iter_s (fun r -> f ~tx ~old_row:r) all_displaced
            in
            let* () =
              match after_hook with
              | None -> Lwt.return_unit
              | Some f -> f ~tx ~new_row:row
            in
            let* () = release_txn ~cat tx owned in
            Lwt.return true
          | _ ->
            Lwt.fail_with
              (Printf.sprintf
                 "UNIQUE constraint failed: %s.%s"
                 table_meta.Cat.name
                 col_name)))
;;

(* Build the row to insert: use [prebuilt_row] if given, else evaluate each
   (ordinal, expr) into a fresh NULL-filled row of the table's width. *)
let build_insert_row
      ~clock
      ~params
      ~prebuilt_row
      ~ordinals
      ~values
      (table_meta : Cat.table_meta)
  : Row.t
  =
  match prebuilt_row with
  | Some r -> r
  | None ->
    let n = List.length table_meta.columns in
    let r = Array.make n Row.V_null in
    List.iter2
      (fun ord expr -> r.(ord) <- eval_expr clock params [||] expr)
      ordinals
      values;
    r
;;

(* #631: a statement-level undo point for a row an [OR IGNORE] INSERT may skip.

   A skipped row must leave nothing behind.  The row-store path is otherwise
   scrupulous about that — the skip returns before [S.put], the index writes,
   the FTS/AFTER-trigger hook, the IVM change feed and the #240 dirty mark —
   but a BEFORE INSERT trigger has ALREADY run, inside the parent txn, and its
   nested DML is real.  In autocommit that vanished with [S.rollback tx]
   ([owned = true]); inside an explicit [BEGIN] it survived, because the undo
   was keyed on WHO OWNS the transaction rather than on WHAT THE STATEMENT
   DECIDED.  The same statement therefore left a trace or not depending on
   whether the caller happened to open a transaction.

   The fix is a savepoint taken around the row and rolled back when the row is
   skipped.  Three properties matter:

   - It is taken ONLY when [not owned] (autocommit already undoes everything),
     a BEFORE INSERT trigger actually exists ([Option.is_some before_hook] —
     [Db] returns [None] when no trigger matches table/timing/event), and the
     statement can skip at all ([CA_ignore]).  Outside that intersection not a
     single savepoint is pushed, so the TPC-C write path is untouched; a B-tree
     savepoint clones the pager's dirty set and is not free.
   - It never aborts the caller's transaction and never touches
     [Db.explicit_txn] or the #555 poison flag — it is opened and resolved
     within one statement, so it adds no second way out of a poisoned handle
     and nothing in #555/#584/#598 changes shape.
   - The name is unique per row, so nesting (a trigger body whose own INSERT
     takes one) stays LIFO over [Store]'s savepoint stack.

   On an exception the savepoint is RELEASED, not rolled back: a statement that
   raises mid-way still leaves its partial effects in an explicit transaction
   (see [with_ddl_txn]'s #286 note), and changing that is a different issue.
   Releasing keeps the stack from growing without changing what is kept. *)
let stmt_savepoint_seq = ref 0

let stmt_savepoint_begin ~(cat : Cat.t) tx ~(take : bool) : string option Lwt.t =
  if not take
  then Lwt.return_none
  else (
    incr stmt_savepoint_seq;
    let name = Printf.sprintf "__granary_stmt_%d" !stmt_savepoint_seq in
    let* () = S.savepoint_begin tx name in
    Cat.savepoint_begin_schema cat name;
    Lwt.return_some name)
;;

let stmt_savepoint_release ~(cat : Cat.t) tx (sp : string option) : unit Lwt.t =
  match sp with
  | None -> Lwt.return_unit
  | Some name ->
    let* () = S.savepoint_release tx name in
    Cat.savepoint_release_schema cat name;
    Lwt.return_unit
;;

(* #666: [mark] is the row's #417 delta-log mark and [owned] says whether the
   transaction is this statement's own.  A row that did not write must leave no
   delta behind, and the store is reverted for it by ONE of two mechanisms:
   in autocommit ([owned]) the skip arms of [execute_insert_write] /
   [execute_upsert_update] have already rolled the whole per-row transaction
   back; in a borrowed transaction it is the savepoint rolled back just below,
   which exists only inside #631's intersection.  Restore exactly then — with
   [not owned] and no savepoint nothing was reverted, and dropping the deltas
   would lose a real trigger write instead of a phantom one. *)
let stmt_savepoint_finish
      ~(cat : Cat.t)
      tx
      ~(wrote : bool)
      ~(owned : bool)
      ~(mark : changes_mark)
      (sp : string option)
  : unit Lwt.t
  =
  if (not wrote) && (owned || Option.is_some sp) then changes_restore mark;
  let* () =
    match sp with
    | Some name when not wrote ->
      let* () = S.savepoint_rollback tx name in
      Cat.savepoint_rollback_schema cat name;
      Lwt.return_unit
    | _ -> Lwt.return_unit
  in
  stmt_savepoint_release ~cat tx sp
;;

(** Run [Op_insert] against the store: write the new row to the table
    tree and, if any indexes are defined on the table, also write the
    corresponding index entries (checking UNIQUE constraints first).
    Uses a SINGLE RW txn for both the row write and index writes. *)
let execute_insert
      ?(mode = Auto)
      ?(params = [||])
      ?(clock : (unit -> float) option = None)
      ?(on_conflict : Ast.conflict_action option = None)
      ?(upsert_update : (string list * (int * Plan.expr) list) option = None)
      ?(prebuilt_row : Row.t option = None)
      ?(before_hook : (tx:S.rw S.txn -> new_row:Row.t -> unit Lwt.t) option = None)
      ?(after_hook : (tx:S.rw S.txn -> new_row:Row.t -> unit Lwt.t) option = None)
      ?(on_replace_delete_before : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option =
        None)
      ?(on_replace_delete : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option = None)
      ?(on_upsert_update_before :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      ?(on_upsert_update :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      (store : S.t)
      (cat : Cat.t)
      ~(table_meta : Cat.table_meta)
      ~ordinals
      ~(values : Plan.expr list)
  : bool Lwt.t
  =
  let row = build_insert_row ~clock ~params ~prebuilt_row ~ordinals ~values table_meta in
  compute_stored_generated_cols clock params table_meta row;
  (* Evaluate CHECK and FK constraints before any writes. *)
  eval_check_constraints clock params table_meta row;
  let* () = enforce_insert_fks store cat table_meta row in
  (* When an explicit transaction is already held, we must NOT call
     Cat.next_rowid (which opens its own RW txn and deadlocks on the mutex):
     acquire/reuse the txn first, then allocate the rowid within it.  BEFORE
     INSERT fires inside the parent txn so its nested DML shares the tx and
     its writes roll back atomically with the parent on failure. *)
  let* tx, owned = acquire_txn store mode in
  (* #631: undo point for a row this statement may skip.  Only when the
     transaction is the caller's ([not owned] — autocommit already undoes
     everything through [S.rollback]), a BEFORE INSERT trigger exists whose
     nested DML could outlive the skip, and the statement has a resolution that
     can skip at all.  [Option.is_some], not [<> None]: the payload is a
     closure and structural comparison would raise. *)
  let* sp =
    stmt_savepoint_begin
      ~cat
      tx
      ~take:((not owned) && Option.is_some before_hook && on_conflict = Some Ast.CA_ignore)
  in
  (* #666: the delta-log counterpart of that undo point, and deliberately WIDER
     than it.  The savepoint is only needed when the transaction is borrowed;
     the stale-delta hole is just as real in autocommit, where the skip arms
     roll the whole per-row transaction back and the accumulator — which rides
     Lwt storage across that rollback — kept the trigger's [Inserted] anyway.
     Marking is O(tables touched) and [Cm_none] when nobody is capturing, so
     the unconditional mark costs the plain write path one branch. *)
  let mark = changes_mark () in
  Lwt.catch
    (fun () ->
       let* () =
         match before_hook with
         | None -> Lwt.return_unit
         | Some f -> f ~tx ~new_row:(Array.copy row)
       in
       (* #243 (T1): capture whether an EXPLICIT integer id was supplied for the
          rowid-alias column BEFORE [insert_rowid] writes back an auto value. *)
       let alias_idx = Cat.rowid_alias_col table_meta in
       let alias_explicit =
         match alias_idx with
         | Some i ->
           (match row.(i) with
            | Row.V_int _ -> true
            | _ -> false)
         | None -> false
       in
       (* #347: defer the per-row counter B-tree write when inside an explicit txn;
          [flush_dirty_counters_tx] writes it once at COMMIT instead. *)
       let* rowid = insert_rowid ~defer_counter:(not owned) tx cat table_meta row in
       let idxs = Cat.indexes_for_table cat ~table:table_meta.name in
       (* Phase 35 Task 2: compute VIRTUAL generated columns into a scratch row
         before extracting index keys so VIRTUAL cells contribute their value. *)
       let row_for_idx = with_computed_virtuals clock params table_meta row in
       (* #639/#599: an ON CONFLICT clause only ever intercepts a UNIQUENESS
          conflict, never a NOT NULL one — the modifier governs the INSERT half.
          So the row being inserted is checked for NULLs BEFORE any conflict
          resolution is consulted, and the modifier decides what that means:
          [OR IGNORE] skips the row, every other resolution raises.

          Two entry conditions, and the second one is the review fix.
          [CA_ignore] is there so `INSERT OR IGNORE` skips identically whichever
          index the row collides with — the check used to live only in
          [execute_insert_write], which an alias-PK conflict reaches and a
          secondary-index conflict does not.  [Option.is_some upsert_update]
          extends the same reasoning to the raising resolutions, which have the
          same asymmetry for the same reason and which the alias pre-probe below
          would otherwise route past [execute_insert_write] entirely: on `main`,
          `INSERT INTO t VALUES (1, NULL_param) ON CONFLICT(k) DO UPDATE ...`
          with row 1 present raised NOT NULL, and the pre-probe silently turned
          that into a successful DO UPDATE.  It now raises again, and the
          secondary-index shape — which never raised — agrees with it.

          Deliberately NOT extended to plain INSERTs: with no upsert clause,
          [execute_insert_write] still owns the check, so a statement without an
          ON CONFLICT clause keeps `main`'s exact behaviour, including the
          precedence between a UNIQUE error and a NOT NULL one.
          [execute_insert_write] never double-evaluates — since #669 its
          [Ic_skip] arm returns before its NOT NULL check ever runs, and for a
          raising resolution this call has already raised. *)
       let null_skip =
         (on_conflict = Some Ast.CA_ignore || Option.is_some upsert_update)
         && not_null_skip_or_fail table_meta row ~on_conflict
       in
       (* #243 (T1): alias PK conflict detection is normally folded into put_x
          inside execute_insert_write (#350) — no pre-read needed. *)
       let alias_col_name =
         match alias_idx with
         | Some i when alias_explicit -> Some (List.nth table_meta.columns i).Row.name
         | _ -> None
       in
       (* #639: the rowid-alias PK is the OTHER thing an ON CONFLICT clause can
          name, and when it names that one the target has to be resolved before
          [check_insert_unique] gets a say — for exactly the reason spelled out
          there. The alias PK has no index, so it is invisible to that fold: a
          secondary conflict would set [skip] (under [OR IGNORE], losing the DO
          UPDATE the caller asked for) or queue deletes (under [OR REPLACE],
          displacing rows for an insert that then never happens, since
          [delete_replace_conflicts] runs before [put_x] discovers the alias
          conflict). Both are the same defect as the multi-index case, mirrored.

          The probe costs one [S.get] and is only paid when an ON CONFLICT
          clause actually names the alias column — never on the plain INSERT
          path #350 optimised, which has no upsert clause at all. When it hits,
          the result is the same shape the secondary-index target produces:
          [Ic_upsert rowid], with [old_rowid = rowid] by the alias-PK
          invariant (the conflict is on this very key). *)
       let alias_is_conflict_target =
         match upsert_update, alias_col_name with
         | Some (conflict_cols, _), Some name -> conflict_cols = [ name ]
         | _ -> false
       in
       let* alias_target_hit =
         if null_skip || not alias_is_conflict_target
         then Lwt.return false
         else
           let* existing =
             S.get
               tx
               (let x, _, _, _ = Cat.row_storage table_meta in
                x)
               (Rowid.encode rowid)
           in
           Lwt.return (Option.is_some existing)
       in
       let* conflict =
         if null_skip
         then Lwt.return Ic_skip
         else if alias_target_hit
         then Lwt.return (Ic_upsert rowid)
         else
           check_insert_unique
             tx
             table_meta
             ~clock
             ~params
             ~row_for_idx
             ~on_conflict
             ~upsert_update
             idxs
       in
       (* #669: [Ic_skip]/[Ic_replace] are unrepresentable alongside [Ic_upsert]
          now, so the branch below no longer needs a comment to say the
          discarded fields were meaningless — there are no fields to discard.
          #639 (review)'s original point stands: this is sound only because
          every producer of [Ic_upsert] here — [check_insert_unique]'s target
          pass and the alias probe above — never also queues a REPLACE
          delete for the row being discarded in its favour. *)
       match upsert_update, conflict with
       | Some (_, assigns), Ic_upsert old_rowid ->
         (* Secondary-index upsert conflict: update the conflicting row.
            [execute_upsert_update] writes via [write_row_rekeyed] (raw put/del),
            bypassing the marked normal-insert path, so mark here when it actually
            updated the row. *)
         let* updated =
           execute_upsert_update
             tx
             cat
             table_meta
             ~clock
             ~params
             ~owned
             ~row
             ~assigns
             ~old_rowid
             ~on_upsert_update_before
             ~on_upsert_update
         in
         if updated then mark_dirty table_meta.Cat.name;
         let* () = stmt_savepoint_finish ~cat tx ~wrote:updated ~owned ~mark sp in
         Lwt.return updated
       | _ ->
         let* inserted =
           execute_insert_write
             tx
             cat
             table_meta
             ~clock
             ~params
             ~owned
             ~row
             ~row_for_idx
             ~rowid
             ~idxs
             ~conflict:(to_insert_write_conflict conflict)
             ~alias_explicit
             ~alias_col_name
             ~on_conflict
             ~upsert_update
             ~on_replace_delete_before
             ~on_replace_delete
             ~on_upsert_update_before
             ~on_upsert_update
             ~after_hook
         in
         (* #243 (T1): record the rowid actually written so [last_insert_rowid()]
            is correct even when an explicit id differs from [next_rowid - 1].
            Skipped inserts (ON CONFLICT IGNORE ⇒ [inserted=false]) leave it. *)
         if inserted then Cat.set_last_inserted_rowid cat rowid;
         if inserted
         then (
           mark_dirty table_meta.Cat.name;
           record_change table_meta.Cat.name (Inserted { rowid; row }));
         let* () = stmt_savepoint_finish ~cat tx ~wrote:inserted ~owned ~mark sp in
         Lwt.return inserted)
    (fun exn ->
       (* On any exception: rollback if we own the txn, then re-raise.  #631:
          the statement savepoint is released, not rolled back — a raising
          statement's partial effects already survive in a borrowed
          transaction, and this fix is about a SKIP, not about statement
          atomicity on error.  Releasing only keeps the stack bounded.

          #666: the deltas follow the STORE, so they are dropped exactly when
          the store is — in autocommit, where [S.rollback] below undoes the
          whole per-row transaction.  In a borrowed transaction the partial
          effects survive, so their deltas must survive with them.

          #737 (fixed): the consumer that used to lose them —
          [Db.drive_reactive]'s [Error] arm — no longer absorbs the
          accumulator at all on failure; it schedules a resync of the affected
          reactive views instead.  So no OTHER exception handler in this module
          owes itself a mark: whatever a raising statement left in the delta
          log is discarded, and the views are rebuilt from the base tables.
          A mark here is still correct and is kept, because it also serves the
          non-raising SKIP path through [stmt_savepoint_finish]. *)
       let* () = stmt_savepoint_release ~cat tx sp in
       if owned then changes_restore mark;
       let* () = if owned then S.rollback tx else Lwt.return_unit in
       Lwt.fail exn)
;;

(** #576 final review: caps the [Hashtbl] {!execute_create_index} builds to
    compute a leading-column distinct-value count during its table walk, so
    that walk's peak retained memory cannot grow past this many entries
    regardless of table size -- see the comment at the [Hashtbl.create] site
    for why an unbounded version is a real regression, not a theoretical one. *)
let index_stats_cardinality_cap = 100_000

(** #576 tier 2: how many equi-depth (by row count) buckets
    [execute_create_index]'s walk targets when building a leading-column
    histogram. 20 buckets is ~5% CDF resolution -- enough to separate "this
    range covers a small slice of the table" from "this range covers most of
    it" (#561's row-4 residual class of mis-estimate) without a
    variable-resolution scheme to justify a different number. A single
    skewed value can still make the actual persisted histogram shorter than
    [histogram_bucket_count + 1] entries -- see [histogram]'s doc comment in
    catalog.mli -- so nothing downstream may assume the array has exactly
    this many buckets; they must read [Array.length boundaries - 1]. *)
let histogram_bucket_count = 20

(** #576 tier 2: build an equi-depth (by row count) histogram from
    [entries] -- the [(encoded_key, row_count)] pairs
    [execute_create_index]'s walk collected, unsorted, for one indexed
    column. [total_rows] is the sum of every entry's count (equivalently,
    [rows_at_analysis]). [None] when [entries] has fewer than
    [histogram_bucket_count] distinct keys -- see [histogram]'s doc comment
    in catalog.mli for why that floor exists.

    The boundary array is built by sorting [entries] by key (byte order --
    the same order the index itself sorts by) and walking the sorted list
    while accumulating a running row total; each time the running total
    crosses a multiple of [total_rows / histogram_bucket_count], the current
    key is emitted as an interior boundary, up to [histogram_bucket_count - 1]
    of them.  The first and last keys are always prepended/appended, so the
    result spans the full observed range even when a single skewed key's
    count overshoots several bucket-widths in one step (it still only
    contributes ONE boundary -- this is the standard equi-depth degenerate
    case, not a bug: see [histogram]'s doc comment on why a consumer must
    read the actual array length rather than assume
    [histogram_bucket_count + 1]).

    The first sorted key can never itself be emitted as an interior
    boundary -- it is always the array's own unconditional first element,
    so the loop below only walks the REMAINING (non-first) entries, with
    [running]/[next_threshold] pre-seeded/pre-advanced past the first
    key's own count before the loop starts. Folding the first key into
    the loop like every other entry would let a large first-key count
    cross the very first threshold on the first iteration and push that
    same key onto the interior list too, duplicating it and producing a
    zero-width phantom bucket at the start ([boundaries.(0) =
    boundaries.(1)]). *)
let build_histogram (entries : (string * int) list) ~total_rows : Cat.histogram option =
  if List.length entries < histogram_bucket_count
  then None
  else (
    let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) entries in
    let step = total_rows / histogram_bucket_count in
    let first_key, first_count = List.hd sorted in
    let rest = List.tl sorted in
    let last_key = fst (List.nth sorted (List.length sorted - 1)) in
    (* [middle] excludes both the first key (via [rest]) and the last key,
       so the loop below can never re-emit either as an interior boundary
       — the mirror image of the pre-advance done for the first key. *)
    let middle =
      match List.rev rest with
      | [] -> [] (* unreachable: [rest] always has >= 19 elements here *)
      | _last :: rev_middle -> List.rev rev_middle
    in
    let running = ref first_count in
    let next_threshold = ref step in
    (* Pre-advance past every threshold multiple the first key's own count
       already meets, so the loop below can never re-emit the first key
       as an interior boundary. *)
    while !next_threshold <= !running do
      next_threshold := !next_threshold + step
    done;
    let interior = ref [] in
    List.iter
      (fun (key, count) ->
         running := !running + count;
         if
           !running >= !next_threshold
           && List.length !interior < histogram_bucket_count - 1
         then (
           interior := key :: !interior;
           (* Advance past every threshold multiple [running] already
              exceeds, not just one [step] past wherever it happened to
              land. A single dominant key can jump [running] well past
              several thresholds in one stride; incrementing by a fixed
              [step] here would re-cross those already-passed thresholds
              on the very next (near-empty) keys, cramming a run of
              degenerate boundaries right after the skewed key instead of
              spreading them across the real value range. *)
           while !next_threshold <= !running do
             next_threshold := !next_threshold + step
           done))
      middle;
    let mids = List.rev !interior in
    Some { Cat.boundaries = Array.of_list ((first_key :: mids) @ [ last_key ]) })
;;

(** Run [Op_create_index]: register the index in the catalog, then scan
    the table tree and populate the index tree with one entry per row. *)
let execute_create_index
      ?(mode = Auto)
      (store : S.t)
      (cat : Cat.t)
      ~name
      ~table
      ~tree_id
      ~col_sqls
      ~col_expr_flags
      ~(where_expr : Plan.expr option)
      ~where_sql
      ~unique
      ~(columns : Row.column list)
  : unit Lwt.t
  =
  (* #269: register the index in the catalog AND populate the index tree through
     ONE writer txn.  Previously [Cat.create_index] opened (and committed) its
     own txn before this scan acquired another — which self-deadlocks when an
     explicit transaction already holds the writer lock.  [with_ddl_txn] threads
     a single txn through both so the whole CREATE INDEX participates in (and
     rolls back with) any ambient explicit transaction. *)
  with_ddl_txn store cat mode (fun tx ->
    let* res =
      Cat.create_index
        ~txn:tx
        cat
        ~name
        ~table
        ~columns:col_sqls
        ~unique
        ~expr_flags:col_expr_flags
        ~where_sql
        ~origin:`User
    in
    match res with
    | Error msg -> failwith msg
    | Ok info ->
      let* cur = S.cursor_open tx tree_id in
      let _sr = S.cursor_first cur in
      (* #576 tier 1: piggyback the leading-column distinct-value count on
         this walk -- it already decodes every candidate row and computes its
         index key, so this adds no I/O. [None] for a UNIQUE index: its
         cardinality is definitionally 1 per key, so no stat is useful. Also
         [None] for a WITHOUT ROWID table's index: its rows are keyed by the
         PRIMARY KEY, not a rowid, and the leading-column cardinality stat
         has no consumer there yet (see the [idx_stats] doc comment in
         catalog.mli). And [None] when the leading column is an expression
         column (the same doc comment) -- [Index_key.encode_value (List.hd
         iks)] would still run and produce a technically-correct count, but
         there is no plan-time consumer that resolves a stat back to the
         expression that produced it, so persisting one would be a number
         nothing ever reads. *)
      let without_rowid_table =
        match Cat.find_table_cached cat ~name:table with
        | Some tm ->
          let _, _, without_rowid, _ = Cat.row_storage tm in
          without_rowid
        | None -> false
      in
      let leading_col_is_expr =
        match col_expr_flags with
        | flag :: _ -> flag
        | [] -> false
      in
      let index_eligible = not (unique || without_rowid_table) in
      let seen =
        if index_eligible && not leading_col_is_expr
        then Some (Hashtbl.create 64)
        else None
      in
      (* #576 final review: [seen] retains one encoded leading-column key per
         DISTINCT value for the whole table walk, so on a non-unique-but-
         near-unique column (an email column, a timestamp, an order-line id)
         over a large table it is effectively O(table size) memory retained
         for the duration of this one DDL statement -- a regression from the
         O(1)-memory streaming walk every other CREATE INDEX path gets.
         [index_stats_cardinality_cap] bounds that peak: once the table has
         already produced this many distinct leading-column values, stop
         adding new ones to [tbl] (an already-seen value can still be
         re-probed at no cost) and fall back to persisting [idx_stats = None]
         below rather than a stat computed from a partial, capped count. A
         capped count would UNDER-report [distinct_count], which makes
         [estimate_rows_from_stats]'s resulting estimate too SMALL -- the
         ADMITTING direction, which is the unsafe one; [None] reproduces
         today's exact pre-#576 behavior for that index instead. *)
      (* #576 tier 2 (corrected): one per-position [Hashtbl] for every
         column OTHER than column 0 -- column 0's own [seen] above still
         only feeds [distinct_count], unaffected by this array. Slot 0 of
         [pos_tables] always stays [None]: it is never built, matching
         [range_histograms]'s own slot-0-always-[None] contract (see the
         [index_stats] doc comment in catalog.mli). A position whose
         column is an expression column also stays [None] -- see that same
         doc comment for why a histogram there is never consulted.

         Gated on [seen <> None], not just [index_eligible]: when the
         LEADING column is itself an expression column, [seen] is [None]
         (see its own construction above) and nothing from this walk is
         ever persisted -- [Some tbl -> ... | Some _ when !capped -> ...]
         below all key off [seen]. Building real [Hashtbl]s here in that
         case would do up to [(n_cols-1) * index_stats_cardinality_cap]
         wasted inserts per row, discarded unread at the end. Sized off
         [info.idx_columns], not [col_expr_flags]: [iks] (below) is built
         from [info.idx_columns] via [get_index_key_values], so sizing
         [pos_tables] from the same list makes the two agree by
         construction rather than by every current SQL code path
         coincidentally producing equal-length lists. *)
      (* #576 waste fix: a non-expression position also stays [None] when its
         declared column type can never carry a [Plan.range] bound --
         [Planner.bounded_type] is the single source of truth
         [range_histogram_estimate] itself consults on the read side, and a
         histogram built for a column it returns [false] on (TEXT/BLOB) would
         never be read: real per-row CPU/memory work, and a stat persisted to
         disk forever, for nothing. *)
      let pos_col_bounded i =
        match List.nth_opt info.idx_columns i with
        | None -> false
        | Some col_name ->
          (match List.find_opt (fun (c : Row.column) -> c.name = col_name) columns with
           | Some c -> Planner.bounded_type c.ty
           | None -> false)
      in
      let pos_tables =
        match seen with
        | None -> Array.make (List.length info.idx_columns) None
        | Some _ ->
          Array.of_list
            (List.mapi
               (fun i is_expr ->
                  if i = 0 || is_expr || not (pos_col_bounded i)
                  then None
                  else Some (Hashtbl.create 64))
               col_expr_flags)
      in
      let pos_capped = Array.make (Array.length pos_tables) false in
      let capped = ref false in
      let rows_indexed = ref 0 in
      let rec walk () =
        match S.cursor_next cur with
        | None -> Lwt.return_unit
        | Some (kbytes, vbytes) ->
          let rowid = Rowid.decode kbytes in
          let row = decode_with_virtual_cols None [||] ~table_name:table columns vbytes in
          let skip =
            match where_expr with
            | None -> false
            | Some we -> not (value_truthy (eval_expr None [||] row we))
          in
          if skip
          then walk ()
          else (
            let key_vals = get_index_key_values None [||] info columns row in
            let iks = List.map row_value_to_index_value key_vals in
            let ikey = Index_key.encode iks ~rowid in
            (match seen with
             | None -> ()
             | Some tbl ->
               if not !capped
               then (
                 let ek = Bytes.to_string (Index_key.encode_value (List.hd iks)) in
                 match Hashtbl.find_opt tbl ek with
                 | Some count -> Hashtbl.replace tbl ek (count + 1)
                 | None ->
                   if Hashtbl.length tbl < index_stats_cardinality_cap
                   then Hashtbl.replace tbl ek 1
                   else capped := true));
            List.iteri
              (fun i v ->
                 match pos_tables.(i) with
                 | None -> ()
                 | Some tbl ->
                   if not pos_capped.(i)
                   then (
                     let ek = Bytes.to_string (Index_key.encode_value v) in
                     match Hashtbl.find_opt tbl ek with
                     | Some count -> Hashtbl.replace tbl ek (count + 1)
                     | None ->
                       if Hashtbl.length tbl < index_stats_cardinality_cap
                       then Hashtbl.replace tbl ek 1
                       else pos_capped.(i) <- true))
              iks;
            incr rows_indexed;
            (* #288: for a UNIQUE index, the build must detect pre-existing
               duplicate values.  The encoded key includes the rowid suffix, so
               two rows sharing the indexed value produce DISTINCT keys and never
               collide in the tree — uniqueness would otherwise only be enforced
               at INSERT time, letting pre-existing duplicates slip through.
               Probe the partially-built index for an entry already carrying this
               value prefix (rowid excluded) using the SAME mechanism as
               [check_insert_unique], so build-time and insert-time uniqueness
               agree (including multi-column, partial-WHERE and NULL handling).
               #290: a key with ANY NULL column is exempt from the conflict
               probe (NULLs are distinct in SQLite) — but is STILL inserted into
               the index tree below, exactly as [check_insert_unique] does.
               A raise here unwinds through [with_ddl_txn]: an owned txn rolls
               back (no partial entries), a borrowed one is poisoned (#286).
               The raise skips the outer [S.cursor_close cur] below, but
               [cursor_close] is a no-op (no OS handle) and the txn unwind
               reclaims all store state — so no leak. *)
            let* () =
              if (not unique) || any_null_val key_vals
              then Lwt.return_unit
              else (
                let prefix, plen = encode_index_key_prefix iks in
                let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
                let* probe = S.seek_ge tx info.idx_tree_id seek_key in
                let* first = S.seek_next probe in
                S.seek_close probe;
                match first with
                | Some (existing, _)
                  when Bytes.length existing >= plen
                       && Bytes.equal (Bytes.sub existing 0 plen) prefix ->
                  Lwt.fail_with
                    (unique_constraint_failed_msg ~table ~columns:info.idx_columns)
                | _ -> Lwt.return_unit)
            in
            let* () = S.put tx info.idx_tree_id ikey Bytes.empty in
            walk ())
      in
      let* () = walk () in
      S.cursor_close cur;
      (* #576 tier 1/2 (corrected): persist the stats in the same DDL
         transaction as the index itself, so they roll back with it.
         [seen] (column 0) governs whether ANY stats are persisted at all,
         exactly as tier 1 always did -- if column 0's own walk was capped
         or the whole index is ineligible, nothing is persisted, including
         every per-position histogram, even one that individually never
         hit its own cap. This keeps [distinct_count]'s existing all-or-
         nothing contract; only per-position CAPPING (below) is new and
         granular. *)
      (match seen with
       | None -> Lwt.return_unit
       | Some _ when !capped -> Lwt.return_unit
       | Some tbl ->
         let range_histograms =
           Array.mapi
             (fun i pos_tbl ->
                match pos_tbl with
                | None -> None
                | Some _ when pos_capped.(i) ->
                  (* #576 tier 2 (corrected): a per-position cap hit is
                     LOCAL -- it degrades only this slot to [None], not the
                     whole index's stats (unlike column 0's [capped] flag
                     above, which is whole-index by design -- see the
                     design doc's "Population" section for why the two
                     scopes differ). *)
                  None
                | Some t ->
                  let entries = Hashtbl.fold (fun k c acc -> (k, c) :: acc) t [] in
                  build_histogram entries ~total_rows:!rows_indexed)
             pos_tables
         in
         Cat.set_index_stats
           cat
           tx
           ~name
           ~distinct_count:(Hashtbl.length tbl)
           ~rows_at_analysis:!rows_indexed
           ~range_histograms))
;;

(** Build the list of (child_table_meta, relevant_fk_constraints) pairs
    for tables that have FK constraints pointing to [parent_table_name]. *)
let build_child_refs cat ~parent_table_name =
  let* all_tables = Cat.list_tables cat in
  Lwt.return
    (List.filter_map
       (fun (child_meta : Cat.table_meta) ->
          let fks =
            List.filter
              (fun (fk : Cat.fk_constraint) ->
                 String.equal fk.fk_parent_table parent_table_name)
              child_meta.Cat.fk_constraints
          in
          if fks = [] then None else Some (child_meta, fks))
       all_tables)
;;

(** Scan [child_meta] using an existing RW transaction for rows where all
    [child_col_idxs] match [parent_vals] simultaneously.  When an index covers
    [child_col_idxs] as a leading prefix, the scan is driven by the index;
    otherwise it falls back to a full table scan.
    Returns (rowid, row) list. *)
let scan_child_rows_multi_tx
      (cat : Cat.t)
      tx
      (child_meta : Cat.table_meta)
      ~(child_col_idxs : int list)
      ~(parent_vals : Row.value list)
  =
  (* #765 review item 5: NULL check first — see the twin comment in
     {!fk_child_has_ref_multi_in_tx}. *)
  if List.exists (fun v -> v = Row.V_null) parent_vals
  then full_scan_collect tx child_meta (fk_cols_match ~child_col_idxs ~parent_vals)
  else (
    match
      ( Cat.find_index_covering_cols
          cat
          ~table_name:child_meta.Cat.name
          ~col_idxs:child_col_idxs
      , child_index_key_types child_meta child_col_idxs )
    with
    | Some idx, Some child_tys ->
      (* #755: same fix as {!fk_child_has_ref_multi_in_tx} — this scan backs
         the immediate RESTRICT check AND the CASCADE / SET NULL / SET DEFAULT
         actions, so a byte-exact seek here silently skipped cascading a
         cross-numeric-equal child row, not just RESTRICT's existence check. *)
      (match index_lookup_values (List.combine parent_vals child_tys) with
       | None -> Lwt.return []
       | Some iks ->
         let prefix, plen = encode_index_key_prefix iks in
         let buf = ref [] in
         let* () =
           seek_index_matches
             tx
             idx
             child_meta
             ~prefix
             ~plen
             ~child_col_idxs
             ~parent_vals
             ~on_match:(fun rowid row ->
               buf := (rowid, row) :: !buf;
               false (* keep walking: collect every match *))
         in
         Lwt.return (List.rev !buf))
    | _ -> full_scan_collect tx child_meta (fk_cols_match ~child_col_idxs ~parent_vals))
;;

(** Delete a single row and its index entries within an existing RW transaction. *)
let delete_row_in_tx tx (cat : Cat.t) (meta : Cat.table_meta) ~rowid ~(row : Row.t) =
  mark_dirty meta.Cat.name;
  (* #417: this primitive fires for FK-cascade child deletes (the top-level
     delete records via [apply_delete_row]'s loop), so the removed child row
     reaches the delta feed. *)
  record_change meta.Cat.name (Deleted { rowid; row });
  let rowid_key = Rowid.encode rowid in
  let child_idxs = Cat.indexes_for_table cat ~table:meta.Cat.name in
  (* Phase 35 Task 2: ensure VIRTUAL gen cols are populated before key extraction. *)
  let row_for_idx = with_computed_virtuals None [||] meta row in
  let* () =
    Lwt_list.iter_s
      (fun (idx : Cat.index_info) ->
         if not (row_matches_index_where None [||] idx meta.Cat.columns row_for_idx)
         then Lwt.return_unit
         else (
           let iks =
             List.map
               row_value_to_index_value
               (get_index_key_values None [||] idx meta.Cat.columns row_for_idx)
           in
           let old_ikey = Index_key.encode iks ~rowid in
           S.del tx idx.idx_tree_id old_ikey))
      child_idxs
  in
  let del_tree_id, _, _, _ = Cat.row_storage meta in
  let* () = S.del tx del_tree_id rowid_key in
  (* #409: reuse a plain rowid table's high-water after a committed delete. *)
  Cat.note_rowid_deleted cat ~name:meta.Cat.name ~rowid tx
;;

(** Update one column to [new_val] in a row within an existing RW transaction.
    Also updates index entries for any index that covers [col_idx]. *)
let update_col_in_tx
      tx
      (cat : Cat.t)
      (meta : Cat.table_meta)
      ~rowid
      ~(row : Row.t)
      ~col_idx
      ~new_val
  =
  mark_dirty meta.Cat.name;
  let new_row = Array.copy row in
  new_row.(col_idx) <- new_val;
  compute_stored_generated_cols None [||] meta new_row;
  let indexes = Cat.indexes_for_table cat ~table:meta.Cat.name in
  (* #693: this is the third and last [write_row_rekeyed] caller (ON UPDATE
     CASCADE / SET NULL / SET DEFAULT) — plain UPDATE has
     [validate_update_unique] and UPSERT DO UPDATE has [execute_upsert_update]'s
     own pass (#667), but this write path had no uniqueness probe at all, so a
     cascade could silently write a duplicate into a UNIQUE child column.  Run
     the same shared [check_indexes_unique_on_update] helper before the row
     moves, and hand its already-computed [new_row_for_idx] through to
     [write_row_rekeyed] so the VIRTUAL-column evaluation isn't paid twice. *)
  let new_row_for_idx = with_computed_virtuals None [||] meta new_row in
  let* () =
    check_indexes_unique_on_update
      tx
      indexes
      ~clock:None
      ~params:[||]
      ~schema:meta.Cat.columns
      ~old_row:row
      ~new_row_for_idx
      ~rowid
  in
  (* #249: a cascade that lands on the child's own INTEGER PRIMARY KEY column
     must MOVE the child row (re-key + reindex), like any other alias-column
     UPDATE — funnel through the shared helper. *)
  let* new_rowid =
    write_row_rekeyed
      tx
      meta
      ~clock:None
      ~params:[||]
      ~old_row:row
      ~new_row
      ~old_rowid:rowid
      ~indexes
      ~new_row_for_idx
      ()
  in
  (* #417: record the FK-cascade child column update (ON UPDATE CASCADE / SET
     NULL / SET DEFAULT); a cascade that re-keyed the child's own INTEGER PK is
     recorded as Deleted+Inserted, like any rowid-changing UPDATE. *)
  record_update meta.Cat.name ~old_rowid:rowid ~new_rowid ~old_row:row ~new_row;
  Lwt.return_unit
;;

(* The DEFAULT value for [col] as a Row.value, resolving CURRENT_* sentinels
   via the clock.  Shared by the ON DELETE / ON UPDATE SET DEFAULT cascades. *)
let fk_default_value clock params (col : Row.column) : Row.value =
  match col.Row.default with
  | None -> Row.V_null
  | Some (Row.DV_int n) -> Row.V_int n
  | Some (Row.DV_text s) -> Row.V_text s
  | Some (Row.DV_real f) -> Row.V_real f
  | Some (Row.DV_blob b) -> Row.V_blob b
  | Some Row.DV_null -> Row.V_null
  | Some Row.DV_current_timestamp ->
    eval_expr
      clock
      params
      [||]
      (Plan.P_func (Ast.Fn_datetime, [ Plan.P_lit (Ast.L_text "now") ]))
  | Some Row.DV_current_date ->
    eval_expr
      clock
      params
      [||]
      (Plan.P_func (Ast.Fn_date, [ Plan.P_lit (Ast.L_text "now") ]))
  | Some Row.DV_current_time ->
    eval_expr
      clock
      params
      [||]
      (Plan.P_func (Ast.Fn_time, [ Plan.P_lit (Ast.L_text "now") ]))
;;

(** Recursively delete a row and cascade FK actions to child tables.
    Only runs cascade logic when FK enforcement is enabled in [cat]. *)
let rec cascade_delete_row_in_tx
          tx
          (cat : Cat.t)
          ?(visited : (string * int64, unit) Hashtbl.t = Hashtbl.create 16)
          (clock : (unit -> float) option)
          (params : Row.value array)
          (meta : Cat.table_meta)
          ~rowid
          ~(row : Row.t)
  =
  let visited_key = meta.Cat.name, rowid in
  if Hashtbl.mem visited visited_key
  then Lwt.return_unit
  else (
    Hashtbl.add visited visited_key ();
    let* child_refs =
      if Cat.get_fk_enforcement cat
      then build_child_refs cat ~parent_table_name:meta.Cat.name
      else Lwt.return []
    in
    let* () =
      Lwt_list.iter_s
        (fun (child_meta, fks) ->
           Lwt_list.iter_s
             (fun (fk : Cat.fk_constraint) ->
                cascade_delete_fk
                  tx
                  cat
                  visited
                  clock
                  params
                  meta
                  ~rowid
                  ~row
                  child_meta
                  fk)
             fks)
        child_refs
    in
    delete_row_in_tx tx cat meta ~rowid ~row)

(* Apply the ON DELETE action of one [fk] (child_meta references meta) while
   deleting [row] of [meta] at [rowid]. *)
and cascade_delete_fk
      tx
      cat
      visited
      clock
      params
      (meta : Cat.table_meta)
      ~rowid
      ~(row : Row.t)
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
  =
  (* #765 review round 4, item 1: used to resolve both column lists with the
     [_opt] variant and silently [Lwt.return_unit] the WHOLE cascade action
     (SET NULL / SET DEFAULT / CASCADE / RESTRICT never runs, nothing
     raised) on a corrupted column — worse than RESTRICT's own loud
     refusal for the identical condition, and the exact "nothing errors"
     failure mode this PR's own cascade-path audit called out for the
     original cross-numeric bug. [resolve_fk_col_idxs] now fails loudly
     instead, agreeing with every other FK column-resolution site this PR
     has touched. *)
  let* parent_col_idxs =
    resolve_fk_col_idxs ~table_name:meta.Cat.name meta.Cat.columns fk.Cat.fk_parent_cols
  in
  let parent_vals = List.map (fun i -> row.(i)) parent_col_idxs in
  if any_null_val parent_vals
  then Lwt.return_unit
  else
    let* child_col_idxs =
      resolve_fk_col_idxs
        ~table_name:child_meta.Cat.name
        child_meta.Cat.columns
        fk.Cat.fk_local_cols
    in
    match fk.Cat.fk_on_delete with
    | Cat.FA_restrict | Cat.FA_no_action ->
      cascade_delete_restrict
        cat
        tx
        meta
        child_meta
        fk
        ~parent_vals
        ~child_col_idxs
        ~rowid
    | Cat.FA_cascade ->
      let* child_rows =
        scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals
      in
      Lwt_list.iter_s
        (fun (crid, crow) ->
           cascade_delete_row_in_tx
             tx
             cat
             ~visited
             clock
             params
             child_meta
             ~rowid:crid
             ~row:crow)
        child_rows
    | Cat.FA_set_null ->
      cascade_delete_set_null
        tx
        cat
        visited
        clock
        params
        child_meta
        ~child_col_idxs
        ~parent_vals
    | Cat.FA_set_default ->
      cascade_delete_set_default
        tx
        cat
        visited
        clock
        params
        child_meta
        ~child_col_idxs
        ~parent_vals

(* ON DELETE RESTRICT/NO ACTION: if any child row still references the parent,
   queue a deferred recheck or raise immediately. *)
and cascade_delete_restrict
      cat
      tx
      (meta : Cat.table_meta)
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
      ~(parent_vals : Row.value list)
      ~child_col_idxs
      ~rowid
  =
  let is_deferred = fk.Cat.fk_deferrable || Cat.get_defer_fks_pragma cat in
  let* child_rows =
    scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals
  in
  if child_rows <> []
  then (
    let msg =
      Printf.sprintf
        "FOREIGN KEY constraint failed: '%s.%s' is still referenced by '%s.%s'"
        meta.Cat.name
        (String.concat "," fk.Cat.fk_parent_cols)
        child_meta.Cat.name
        (String.concat "," fk.Cat.fk_local_cols)
    in
    let parent_meta_name = meta.Cat.name in
    let child_meta_name = child_meta.Cat.name in
    (* #765 review item 4: was a hand-rolled closure duplicating
       {!make_fk_recheck}'s by-name resolution verbatim, which also meant it
       carried round 1's RENAME COLUMN gap independently and a future fix to
       [make_fk_recheck] would not have propagated here. Calling it directly
       keeps this call site and every other deferred FK recheck agreeing by
       construction. *)
    let ord = Option.value (fk_ordinal child_meta fk) ~default:(-1) in
    let recheck =
      make_fk_recheck
        cat
        ~child_name:child_meta_name
        ~parent_name:parent_meta_name
        ~fk_ordinal:ord
        ~parent_vals
    in
    fk_violation
      ~deferred:is_deferred
      cat
      ~kind:`Delete
      ~table:parent_meta_name
      ~rowid
      ~msg
      ~recheck
      ~child_table:child_meta_name
      ~fk_ordinal:ord)
  else Lwt.return_unit

(* ON DELETE SET NULL: set each child FK column to NULL (rejecting NOT NULL),
   routing through cascade_update_col_in_tx so further ON UPDATE chains run. *)
and cascade_delete_set_null
      tx
      cat
      visited
      clock
      params
      (child_meta : Cat.table_meta)
      ~child_col_idxs
      ~(parent_vals : Row.value list)
  =
  let* child_rows =
    scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals
  in
  if child_rows = []
  then Lwt.return_unit
  else
    (* For single-col FKs (common case), apply to the one child col.
       For multi-col, apply SET NULL to each child col independently. *)
    Lwt_list.iter_s
      (fun child_col_idx ->
         let col = List.nth child_meta.Cat.columns child_col_idx in
         if col.Row.not_null
         then
           Lwt.fail_with
             (Printf.sprintf
                "FOREIGN KEY constraint failed: ON DELETE SET NULL on NOT NULL column \
                 '%s.%s'"
                child_meta.Cat.name
                col.Row.name)
         else
           Lwt_list.iter_s
             (fun (crid, crow) ->
                cascade_update_col_in_tx
                  tx
                  cat
                  ~visited
                  clock
                  params
                  child_meta
                  ~rowid:crid
                  ~row:crow
                  ~col_idx:child_col_idx
                  ~new_val:Row.V_null)
             child_rows)
      child_col_idxs

(* ON DELETE SET DEFAULT: like SET NULL but with each column's DEFAULT value. *)
and cascade_delete_set_default
      tx
      cat
      visited
      clock
      params
      (child_meta : Cat.table_meta)
      ~child_col_idxs
      ~(parent_vals : Row.value list)
  =
  let* child_rows =
    scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals
  in
  if child_rows = []
  then Lwt.return_unit
  else
    Lwt_list.iter_s
      (fun child_col_idx ->
         let col = List.nth child_meta.Cat.columns child_col_idx in
         let default_val = fk_default_value clock params col in
         if col.Row.not_null && default_val = Row.V_null
         then
           Lwt.fail_with
             (Printf.sprintf
                "FOREIGN KEY constraint failed: ON DELETE SET DEFAULT on NOT NULL column \
                 '%s.%s' with no default"
                child_meta.Cat.name
                col.Row.name)
         else
           Lwt_list.iter_s
             (fun (crid, crow) ->
                cascade_update_col_in_tx
                  tx
                  cat
                  ~visited
                  clock
                  params
                  child_meta
                  ~rowid:crid
                  ~row:crow
                  ~col_idx:child_col_idx
                  ~new_val:default_val)
             child_rows)
      child_col_idxs

(** Recursively update a column and cascade FK UPDATE actions to child tables
    that reference this column. *)
and cascade_update_col_in_tx
      tx
      (cat : Cat.t)
      ?(visited : (string * int64, unit) Hashtbl.t = Hashtbl.create 16)
      (clock : (unit -> float) option)
      (params : Row.value array)
      (meta : Cat.table_meta)
      ~rowid
      ~(row : Row.t)
      ~col_idx
      ~new_val
  =
  let visited_key = meta.Cat.name, rowid in
  if Hashtbl.mem visited visited_key
  then Lwt.return_unit
  else (
    Hashtbl.add visited visited_key ();
    let* () = update_col_in_tx tx cat meta ~rowid ~row ~col_idx ~new_val in
    if not (Cat.get_fk_enforcement cat)
    then Lwt.return_unit
    else (
      let parent_col_name = (List.nth meta.Cat.columns col_idx).Row.name in
      let* all_child_refs = build_child_refs cat ~parent_table_name:meta.Cat.name in
      let col_child_refs =
        List.filter_map
          (fun (child_meta, fks) ->
             let matching_fks =
               List.filter
                 (fun (fk : Cat.fk_constraint) ->
                    List.mem parent_col_name fk.Cat.fk_parent_cols)
                 fks
             in
             if matching_fks = [] then None else Some (child_meta, matching_fks))
          all_child_refs
      in
      Lwt_list.iter_s
        (fun (child_meta, fks) ->
           Lwt_list.iter_s
             (fun (fk : Cat.fk_constraint) ->
                cascade_update_fk
                  tx
                  cat
                  visited
                  clock
                  params
                  meta
                  ~row
                  ~new_val
                  ~parent_col_name
                  child_meta
                  fk)
             fks)
        col_child_refs))

(* Apply the ON UPDATE action of one [fk] when [parent_col_name] of [meta]
   changes to [new_val]. *)
and cascade_update_fk
      tx
      cat
      visited
      clock
      params
      (meta : Cat.table_meta)
      ~(row : Row.t)
      ~new_val
      ~parent_col_name
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
  =
  (* Find the position of parent_col_name in fk_parent_cols to get the
     corresponding fk_local_cols entry for single-update cascade. *)
  let fk_pos =
    let rec find_pos i = function
      | [] -> 0
      | col :: _ when String.equal col parent_col_name -> i
      | _ :: rest -> find_pos (i + 1) rest
    in
    find_pos 0 fk.Cat.fk_parent_cols
  in
  let child_col_name = List.nth fk.Cat.fk_local_cols fk_pos in
  (* #765 review round 4, item 1: [resolve_fk_col_idxs] instead of the raw,
     crashing [find_col_idx_by_name] -- same fix as {!cascade_delete_fk}. *)
  let* child_col_idx =
    let* idxs =
      resolve_fk_col_idxs
        ~table_name:child_meta.Cat.name
        child_meta.Cat.columns
        [ child_col_name ]
    in
    Lwt.return (List.hd idxs)
  in
  (* For multi-col FKs, we need all parent_vals to scan child rows *)
  let* all_parent_col_idxs =
    resolve_fk_col_idxs ~table_name:meta.Cat.name meta.Cat.columns fk.Cat.fk_parent_cols
  in
  let all_parent_vals_old = List.map (fun i -> row.(i)) all_parent_col_idxs in
  match fk.Cat.fk_on_update with
  | Cat.FA_restrict | Cat.FA_no_action -> Lwt.return_unit
  | Cat.FA_cascade ->
    let* all_child_col_idxs =
      resolve_fk_col_idxs
        ~table_name:child_meta.Cat.name
        child_meta.Cat.columns
        fk.Cat.fk_local_cols
    in
    let* child_rows =
      scan_child_rows_multi_tx
        cat
        tx
        child_meta
        ~child_col_idxs:all_child_col_idxs
        ~parent_vals:all_parent_vals_old
    in
    Lwt_list.iter_s
      (fun (crid, crow) ->
         cascade_update_col_in_tx
           tx
           cat
           ~visited
           clock
           params
           child_meta
           ~rowid:crid
           ~row:crow
           ~col_idx:child_col_idx
           ~new_val)
      child_rows
  | Cat.FA_set_null ->
    cascade_update_set_null
      tx
      cat
      visited
      clock
      params
      child_meta
      fk
      ~child_col_idx
      ~child_col_name
      ~parent_vals_old:all_parent_vals_old
  | Cat.FA_set_default ->
    cascade_update_set_default
      tx
      cat
      visited
      clock
      params
      child_meta
      fk
      ~child_col_idx
      ~child_col_name
      ~parent_vals_old:all_parent_vals_old

(* ON UPDATE SET NULL for one fk's child column. *)
and cascade_update_set_null
      tx
      cat
      visited
      clock
      params
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
      ~child_col_idx
      ~child_col_name
      ~parent_vals_old
  =
  let col = List.nth child_meta.Cat.columns child_col_idx in
  if col.Row.not_null
  then
    Lwt.fail_with
      (Printf.sprintf
         "FOREIGN KEY constraint failed: ON UPDATE SET NULL on NOT NULL column '%s.%s'"
         child_meta.Cat.name
         child_col_name)
  else
    (* #765 review round 4, item 1: [resolve_fk_col_idxs], same fix as
       {!cascade_delete_fk}/{!cascade_update_fk}. *)
    let* all_child_col_idxs =
      resolve_fk_col_idxs
        ~table_name:child_meta.Cat.name
        child_meta.Cat.columns
        fk.Cat.fk_local_cols
    in
    let* child_rows =
      scan_child_rows_multi_tx
        cat
        tx
        child_meta
        ~child_col_idxs:all_child_col_idxs
        ~parent_vals:parent_vals_old
    in
    Lwt_list.iter_s
      (fun (crid, crow) ->
         cascade_update_col_in_tx
           tx
           cat
           ~visited
           clock
           params
           child_meta
           ~rowid:crid
           ~row:crow
           ~col_idx:child_col_idx
           ~new_val:Row.V_null)
      child_rows

(* ON UPDATE SET DEFAULT for one fk's child column. *)
and cascade_update_set_default
      tx
      cat
      visited
      clock
      params
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
      ~child_col_idx
      ~child_col_name
      ~parent_vals_old
  =
  (* #765 review round 4, item 1: [resolve_fk_col_idxs], same fix as
     {!cascade_delete_fk}/{!cascade_update_fk}. *)
  let* all_child_col_idxs =
    resolve_fk_col_idxs
      ~table_name:child_meta.Cat.name
      child_meta.Cat.columns
      fk.Cat.fk_local_cols
  in
  let* child_rows =
    scan_child_rows_multi_tx
      cat
      tx
      child_meta
      ~child_col_idxs:all_child_col_idxs
      ~parent_vals:parent_vals_old
  in
  if child_rows = []
  then Lwt.return_unit
  else (
    let col = List.nth child_meta.Cat.columns child_col_idx in
    let default_val = fk_default_value clock params col in
    if col.Row.not_null && default_val = Row.V_null
    then
      Lwt.fail_with
        (Printf.sprintf
           "FOREIGN KEY constraint failed: ON UPDATE SET DEFAULT on NOT NULL column \
            '%s.%s' with no default"
           child_meta.Cat.name
           child_col_name)
    else
      Lwt_list.iter_s
        (fun (crid, crow) ->
           cascade_update_col_in_tx
             tx
             cat
             ~visited
             clock
             params
             child_meta
             ~rowid:crid
             ~row:crow
             ~col_idx:child_col_idx
             ~new_val:default_val)
        child_rows)
;;

(* Apply SET NULL to each [child_col_idxs] of every row in [child_rows],
   rejecting NOT NULL columns; routes through cascade_update_col_in_tx so the
   write propagates further ON UPDATE chains.  [op_label] is "ON UPDATE" /
   "ON DELETE" for the error message. *)
let cascade_apply_set_null
      tx
      (cat : Cat.t)
      ~clock
      ~params
      ~visited
      ~op_label
      (child_meta : Cat.table_meta)
      ~child_col_idxs
      child_rows
  : unit Lwt.t
  =
  if child_rows = []
  then Lwt.return_unit
  else
    Lwt_list.iter_s
      (fun child_col_idx ->
         let col = List.nth child_meta.Cat.columns child_col_idx in
         if col.Row.not_null
         then
           Lwt.fail_with
             (Printf.sprintf
                "FOREIGN KEY constraint failed: %s SET NULL on NOT NULL column '%s.%s'"
                op_label
                child_meta.Cat.name
                col.Row.name)
         else
           Lwt_list.iter_s
             (fun (crid, crow) ->
                cascade_update_col_in_tx
                  tx
                  cat
                  ~visited
                  clock
                  params
                  child_meta
                  ~rowid:crid
                  ~row:crow
                  ~col_idx:child_col_idx
                  ~new_val:Row.V_null)
             child_rows)
      child_col_idxs
;;

(* Apply SET DEFAULT to each [child_col_idxs] of every row in [child_rows]. *)
let cascade_apply_set_default
      tx
      (cat : Cat.t)
      ~clock
      ~params
      ~visited
      ~op_label
      (child_meta : Cat.table_meta)
      ~child_col_idxs
      child_rows
  : unit Lwt.t
  =
  if child_rows = []
  then Lwt.return_unit
  else
    Lwt_list.iter_s
      (fun child_col_idx ->
         let col = List.nth child_meta.Cat.columns child_col_idx in
         let default_val = fk_default_value clock params col in
         if col.Row.not_null && default_val = Row.V_null
         then
           Lwt.fail_with
             (Printf.sprintf
                "FOREIGN KEY constraint failed: %s SET DEFAULT on NOT NULL column \
                 '%s.%s' with no default"
                op_label
                child_meta.Cat.name
                col.Row.name)
         else
           Lwt_list.iter_s
             (fun (crid, crow) ->
                cascade_update_col_in_tx
                  tx
                  cat
                  ~visited
                  clock
                  params
                  child_meta
                  ~rowid:crid
                  ~row:crow
                  ~col_idx:child_col_idx
                  ~new_val:default_val)
             child_rows)
      child_col_idxs
;;

(* Hand one candidate rowid to [emit], counting it. *)
let emit_candidate ~stats ~(emit : int64 -> unit Lwt.t) rowid =
  note_seek_candidate stats;
  emit rowid
;;

(* Walk the index range an equality [keys] prefix (plus optional [range]) covers,
   handing each candidate rowid to [emit] as it is decoded.  Nothing is
   accumulated here; what the caller does with the rowids is its business.
   Returns [true] iff the walk bailed out before exhausting the range (see
   below) rather than running to completion.

   [emit] runs with this index cursor OPEN, so it must not mutate the tree being
   walked: deleting rows or moving their index keys mid-walk would revisit rows
   whose new key sorts later in the range and skip their neighbours.  The one
   caller ([drain_matching_rows_in_tx]) only appends to a buffer, and every
   physical mutation of the statement happens after the drain has returned and
   this cursor is closed — table reads included, since the candidates are sorted
   into rowid order before any row is fetched.

   #550: the non-unique-index bail-out budget is computed HERE, fresh, on every
   call, via [Planner.dml_seek_bail_out_at] against [cat] and a freshly
   re-read [table_meta] ([Cat.find_table_cached]) — never against a
   [table_meta] carried on a cached [Plan.op], which can be stale for the
   whole life of a prepared statement; see [Plan.seek]'s doc and
   [Planner.dml_seek_bail_out_at]'s for the staleness bug this replaced. [None]
   means walk unconditionally — always true for a UNIQUE index. When it is
   [Some k], the [(k+1)]-th entry the walk sees sets [bailed] instead of being
   emitted, and the walk stops: the caller has already seen more of a
   non-unique index's leading columns than the measured break-even for a seek,
   so continuing (and the one table-tree descent per candidate it feeds) costs
   more than the full scan it stands in for.

   [bailed] is a ref checked at the top of the recursive loop rather than an
   exception, matching this file's own idiom for aborting a walk early (see
   e.g. [validate_child_ref_exists]'s [found]/[exhausted]): the caller only
   needs a boolean verdict once the walk finishes, not an unwind through
   intermediate frames. *)
let seek_index_candidates
      tx
      clock
      params
      cat
      (table_meta : Cat.table_meta)
      ~idx_tree
      ~keys
      ~range
      ~(stats : dml_seek_stats option)
      ~(emit : int64 -> unit Lwt.t)
  : bool Lwt.t
  =
  let vs = List.map (fun (_, ty, e) -> eval_expr clock params [||] e, ty) keys in
  match index_lookup_values vs with
  | None -> Lwt.return_false (* NULL or type mismatch: matches nothing *)
  | Some ivs ->
    let prefix, plen = encode_index_key_prefix ivs in
    let start, past_end = range_seek_bounds clock params ~prefix ~plen range in
    let* cur = S.seek_ge tx idx_tree start in
    let live_meta =
      match Cat.find_table_cached cat ~name:table_meta.Cat.name with
      | Some m -> m
      | None -> table_meta
    in
    let bail_out_at = Planner.dml_seek_bail_out_at cat live_meta ~idx_tree in
    let walked = ref 0 in
    let bailed = ref false in
    let rec walk () =
      if !bailed
      then Lwt.return_unit
      else (
        match%lwt S.seek_next cur with
        | Some (ikey, _) when index_key_in_range ~prefix ~plen ~past_end ikey ->
          incr walked;
          (match bail_out_at with
           | Some k when !walked > k ->
             bailed := true;
             Lwt.return_unit
           | Some _ | None ->
             let* () = emit_candidate ~stats ~emit (decode_index_key_rowid ikey) in
             walk ())
        | _ -> Lwt.return_unit)
    in
    (* [S.seek_next] and [emit] can both raise; close the cursor on that path
       too.  [Store.seek_close] is a no-op for the B-tree cursor today, so this
       leaks nothing either way — it is here so that stops being true safely. *)
    let* () =
      Lwt.finalize walk (fun () ->
        S.seek_close cur;
        Lwt.return_unit)
    in
    Lwt.return !bailed
;;

(* #508: candidate rowids for a DML [seek], handed to [emit] as they are
   decoded.  The seek is only a restriction: the caller still evaluates the full
   WHERE predicate on every candidate, so a wrong-but-superset answer here can
   cost time but cannot change results.  Returns whether the walk bailed out —
   see [seek_index_candidates] — always [false] for [Seek_rowid], which never
   walks more than one entry.

   Candidates arrive in INDEX-KEY order, which for a prefix spanning several
   distinct full keys is not rowid order.  The caller must therefore sort its
   accumulated matches by rowid — the order a full table-tree scan drains in —
   or an [UPDATE/DELETE ... LIMIT n] without [ORDER BY] would silently hit a
   different n rows than the scan it replaced. *)
let seek_candidates
      tx
      clock
      params
      cat
      (table_meta : Cat.table_meta)
      (seek : Plan.seek)
      ~stats
      ~emit
  : bool Lwt.t
  =
  match seek with
  | Plan.Seek_rowid e ->
    (* #738: an integral REAL addresses the rowid it names, exactly as it does
       on the index path — the DML seek is a restriction, but the residual it
       restricts now says [id = 1.0] is true, so declining here would make
       [DELETE ... WHERE id = 1.0] a silent no-op. *)
    (match rowid_lookup_key (eval_expr clock params [||] e) with
     | Some n ->
       let* () = emit_candidate ~stats ~emit n in
       Lwt.return_false
     | None -> Lwt.return_false (* NULL, non-numeric or fractional: no rowid *))
  | Plan.Seek_index { idx_tree; keys; range } ->
    seek_index_candidates
      tx
      clock
      params
      cat
      table_meta
      ~idx_tree
      ~keys
      ~range
      ~stats
      ~emit
;;

(* #514: a growable, flat buffer of candidate rowids.  THE measurement table
   for this PR lives here; the mli and the tests point at it rather than
   restating it.

   The DML seek must hand the table tree its [get]s in ASCENDING ROWID order,
   which means collecting every candidate the index walk produces before
   fetching the first row.  That is not an oversight, it is the measured
   choice.  Fetching each rowid the instant the walk decodes it needs no buffer
   at all, but delivers the gets in INDEX-KEY order; for an index whose order is
   uncorrelated with rowid that is random access against the table, and since
   the pager cache is a bounded FIFO ([Pager.default_cache_capacity]) a table
   larger than the cache then re-reads a page per row.  Measured on a
   file-backed DB with [GRANARY_PAGE_CACHE=64] and index order deliberately
   scrambled against rowid order, one prefix DELETE:

     rows    sorted-then-fetch    fetch-as-you-walk
     20 000              957                21 685   (22.7x)

   [test_bounded_drain_514]'s [prefix_delete_reads_each_table_page_about_once]
   is that exact run, and its threshold (4 000) is the only copy of these
   numbers with teeth.  A 60 000-row run measured 2 365 against 67 648, but
   nothing asserts it, so it is recorded here and nowhere else.

   Chunking (buffer K, sort, fetch, repeat) does not escape that either: it
   holds locality only while K stays comparable to the match count, degrading
   back toward the right-hand column as the count grows past K (measured on the
   60 000-row case at K = 16 384: 4 375 reads, 1.85x; on the 20 000-row case at
   K = 1 024: 5 150, 5.4x).  A bound that costs a page read per row exactly when
   it starts to bind is not a bound worth having.

   So the buffer stays whole and its per-candidate cost is cut instead.  The
   representation is a [Bigarray.Array1] of [int64], grown by doubling, rather
   than the [int64 list] this replaced or the [int64 array] the first pass at it
   used.  Note that an [int64 array] is NOT flat: OCaml unboxes only [float
   array], so it stores a pointer per slot to a 24-byte custom block — 32 bytes
   a rowid, and a [caml_modify] write barrier on every push.  A [Bigarray]
   stores the 8 bytes themselves, off-heap, in one block the major GC never
   scans element-wise.  Resident set after buffering 1M candidates, each
   representation in a fresh process, [Gc.compact]ed:

     int64 list      49.3 MB     cons (3 words) + box (3 words)
     int64 array     34.2 MB     8-byte slot + 24-byte box
     Bigarray         8.3 MB     8-byte slot

   Peak matters more than steady state here, because doubling means up to [2N]
   slots are live when the buffer is largest.  The [int64 array] version also
   compacted with [Array.sub] before sorting, so [2N] slots, [N] boxes and an
   [N]-slot copy overlapped; the heapsort below is in place over the filled
   prefix, so nothing is copied and the peak is just the two buffers a doubling
   straddles.  For 1M candidates: ~48 MB against ~13 MB.

   The price is that [Array.sort] does not apply to a [Bigarray], so the sort is
   hand-rolled ([rowid_buf_sort]).  Heapsort, because it is in place, has no
   recursion depth to blow on an adversarial input, and needs no scratch — the
   three properties this buffer exists for.  It is pinned end to end by a
   QCheck property in [test_bounded_drain_514] that runs a seeked
   [DELETE .. LIMIT k] over a random rowid permutation against a scan foil:
   LIMIT takes a prefix of the drain, so it reads out the sort.

   All of this is a constant factor, not a bound: the match list the callers
   need in full is a decoded row per match and remains the larger term.  Capping
   THAT means streaming the mutations, which the callers' shape forbids (see
   [drain_matching_rows_in_tx]). *)
type rowid_buf =
  { mutable ids : (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t
  ; mutable len : int
  }

let rowid_buf_alloc n = Bigarray.Array1.create Bigarray.Int64 Bigarray.c_layout n
let rowid_buf_create () = { ids = rowid_buf_alloc 0; len = 0 }

let rowid_buf_push b rowid =
  let cap = Bigarray.Array1.dim b.ids in
  if b.len = cap
  then (
    let bigger = rowid_buf_alloc (if cap = 0 then 16 else 2 * cap) in
    Bigarray.Array1.blit b.ids (Bigarray.Array1.sub bigger 0 b.len);
    b.ids <- bigger);
  Bigarray.Array1.set b.ids b.len rowid;
  b.len <- b.len + 1
;;

let rowid_buf_swap a i j =
  let t = Bigarray.Array1.get a i in
  Bigarray.Array1.set a i (Bigarray.Array1.get a j);
  Bigarray.Array1.set a j t
;;

(* Sift [root] down a max-heap occupying [0, limit) of [a]. *)
let rec rowid_buf_sift a root limit =
  let l = (2 * root) + 1 in
  let r = l + 1 in
  if l < limit
  then (
    let child =
      if
        r < limit && Int64.compare (Bigarray.Array1.get a r) (Bigarray.Array1.get a l) > 0
      then r
      else l
    in
    if Int64.compare (Bigarray.Array1.get a child) (Bigarray.Array1.get a root) > 0
    then (
      rowid_buf_swap a root child;
      rowid_buf_sift a child limit))
;;

(* Ascending rowid, in place, over just the filled prefix — no scratch array, so
   the buffer's peak is its capacity and nothing more (see [rowid_buf]). *)
let rowid_buf_sort b =
  let a = b.ids in
  let n = b.len in
  for i = (n / 2) - 1 downto 0 do
    rowid_buf_sift a i n
  done;
  for last = n - 1 downto 1 do
    rowid_buf_swap a 0 last;
    rowid_buf_sift a 0 last
  done
;;

(* Sequential Lwt iteration over the filled prefix, without going through a
   list (which would re-spend the allocation the buffer exists to avoid). *)
let rowid_buf_iter_s f b =
  let rec go i =
    if i >= b.len
    then Lwt.return_unit
    else Lwt.bind (f (Bigarray.Array1.get b.ids i)) (fun () -> go (i + 1))
  in
  go 0
;;

(* Drain matching rows from the given txn (RO or RW).  With a [seek], only the
   candidate rows it names are read; [where] is applied either way.

   Every match is materialised BEFORE the caller mutates anything, and that is
   deliberate: the callers consume the list several times over (length, FK
   pre-check, triggers, then the write loop), and mutating rows while a cursor
   still walks the table or an index it lives in would revisit or skip rows.
   #514 shrinks the seek's candidate buffer from an [int64 list] to a packed
   [rowid_buf]; it does not — and must not — stream the mutations. *)
(* The no-seek drain: walk the whole table tree, keeping what [keep] accepts.
   Shared by [drain_matching_rows_in_tx]'s own no-seek case and by its #550
   fallback, when a non-selective index seek bails out mid-walk. *)
let drain_full_scan_in_tx tx tree_id (table_meta : Cat.table_meta) ~clock ~params ~keep
  : (int64 * Row.t) list Lwt.t
  =
  let* cur = S.cursor_open tx tree_id in
  let _sr = S.cursor_first cur in
  let buf = ref [] in
  let rec drain () =
    match S.cursor_next cur with
    | None -> ()
    | Some (kbytes, vbytes) ->
      let rowid = Rowid.decode kbytes in
      let row = decode_with_virtual clock params table_meta vbytes in
      if keep row then buf := (rowid, row) :: !buf;
      drain ()
  in
  drain ();
  S.cursor_close cur;
  Lwt.return (List.rev !buf)
;;

let drain_matching_rows_in_tx
      ~(seek : Plan.seek option)
      tx
      cat
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~(where : Plan.expr option)
  : (int64 * Row.t) list Lwt.t
  =
  let tree_id, _, _, _ = Cat.row_storage table_meta in
  let keep row =
    match where with
    | None -> true
    | Some pred -> value_truthy (eval_expr clock params row pred)
  in
  match seek with
  | Some s ->
    (* Candidates land in a [rowid_buf] and are fetched in ascending rowid
       order: the order a full table scan drains in, which keeps the table-tree
       reads sequential (see [rowid_buf] for the page-read measurement that
       forces this) and keeps an unordered [UPDATE/DELETE ... LIMIT n] hitting
       the same rows the scan it replaced would (#512 review).  Because the
       fetches already run in that order, [acc] only needs reversing. *)
    let stats = Lwt.get dml_seek_stats_key in
    let acc = ref [] in
    let fetch_one rowid =
      note_seek_fetched stats;
      let* v = S.get tx tree_id (Rowid.encode rowid) in
      match v with
      | None -> Lwt.return_unit
      | Some vbytes ->
        let row = decode_with_virtual clock params table_meta vbytes in
        if keep row then acc := (rowid, row) :: !acc;
        Lwt.return_unit
    in
    let cands = rowid_buf_create () in
    let* bailed =
      seek_candidates tx clock params cat table_meta s ~stats ~emit:(fun rowid ->
        rowid_buf_push cands rowid;
        Lwt.return_unit)
    in
    if bailed
    then
      (* #550: the seek's non-unique-index prefix walked past its budget.
         [acc] is still empty here — a bail-out can only happen during
         candidate collection, which runs entirely before the fetch phase
         below populates it — so there is nothing to undo before falling back
         to the scan the seek would otherwise have replaced. *)
      drain_full_scan_in_tx tx tree_id table_meta ~clock ~params ~keep
    else (
      rowid_buf_sort cands;
      let* () = rowid_buf_iter_s fetch_one cands in
      Lwt.return (List.rev !acc))
  | None -> drain_full_scan_in_tx tx tree_id table_meta ~clock ~params ~keep
;;

(* Apply ORDER BY, then OFFSET, then LIMIT to a drained (rowid,row) list. *)
let apply_order_offset_limit ~clock ~params ~order ~offset ~limit matches =
  let sorted =
    if order = []
    then matches
    else
      List.sort
        (fun (_, ra) (_, rb) ->
           let rec cmp = function
             | [] -> 0
             | (e, dir, nulls) :: rest ->
               let va = eval_sort_key clock params ra e in
               let vb = eval_sort_key clock params rb e in
               let c = compare_with_nulls dir nulls va vb in
               if c <> 0 then c else cmp rest
           in
           cmp order)
        matches
  in
  let after_offset =
    match offset with
    | None | Some 0 -> sorted
    | Some n -> list_drop n sorted
  in
  match limit with
  | None -> after_offset
  | Some n -> list_take n after_offset
;;

(* Build the post-UPDATE row: copy [old_row] and apply each (i, expr) in
   [assignments], evaluating expr against the OLD row. *)
let apply_assignments ~clock ~params assignments (old_row : Row.t) : Row.t =
  let new_row = Array.copy old_row in
  List.iter
    (fun (i, expr) -> new_row.(i) <- eval_expr clock params old_row expr)
    assignments;
  new_row
;;

(* Pre-write RESTRICT/NO ACTION FK check for one UPDATE row's [fk]: if the
   parent key changes and is still referenced, raise (or queue deferred). *)
let precheck_update_fk
      store
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~rowid_outer
      ~(old_row : Row.t)
      ~(new_row : Row.t)
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
  : unit Lwt.t
  =
  match fk.fk_on_update with
  | Cat.FA_cascade | Cat.FA_set_null | Cat.FA_set_default -> Lwt.return_unit
  | Cat.FA_restrict | Cat.FA_no_action ->
    let is_deferred = fk.fk_deferrable || Cat.get_defer_fks_pragma cat in
    (* #765 review round 3, item 2: [resolve_fk_col_idxs] fails loudly with an
       FK-specific message rather than [find_col_idx_by_name]'s bare
       [Failure "column not found: ..."], matching the deferred path's own
       established behaviour for the identical condition. *)
    let* parent_col_idxs =
      resolve_fk_col_idxs
        ~table_name:table_meta.Cat.name
        table_meta.Cat.columns
        fk.fk_parent_cols
    in
    let old_vals = List.map (fun i -> old_row.(i)) parent_col_idxs in
    let new_vals = List.map (fun i -> new_row.(i)) parent_col_idxs in
    let unchanged =
      List.for_all2 (fun ov nv -> compare_values ov nv = 0) old_vals new_vals
    in
    if unchanged
    then Lwt.return_unit
    else if any_null_val old_vals
    then Lwt.return_unit
    else
      let* child_col_idxs =
        resolve_fk_col_idxs
          ~table_name:child_meta.Cat.name
          child_meta.Cat.columns
          fk.fk_local_cols
      in
      let* has_ref =
        fk_child_has_ref_multi cat store child_meta ~child_col_idxs ~parent_vals:old_vals
      in
      if has_ref
      then (
        let msg =
          Printf.sprintf
            "FOREIGN KEY constraint failed: update to '%s.%s' is referenced by '%s.%s'"
            table_meta.Cat.name
            (String.concat "," fk.fk_parent_cols)
            child_meta.Cat.name
            (String.concat "," fk.fk_local_cols)
        in
        let ord = Option.value (fk_ordinal child_meta fk) ~default:(-1) in
        let recheck =
          make_fk_recheck
            cat
            ~child_name:child_meta.Cat.name
            ~parent_name:table_meta.Cat.name
            ~fk_ordinal:ord
            ~parent_vals:old_vals
        in
        fk_violation
          ~deferred:is_deferred
          cat
          ~kind:`Update
          ~table:table_meta.Cat.name
          ~rowid:rowid_outer
          ~msg
          ~recheck
          ~child_table:child_meta.Cat.name
          ~fk_ordinal:ord)
      else Lwt.return_unit
;;

(* Pre-write FK RESTRICT check across all matched UPDATE rows. *)
let precheck_update_fk_restrict
      store
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~assignments
      ~child_refs
      matches
  : unit Lwt.t
  =
  if child_refs = []
  then Lwt.return_unit
  else
    Lwt_list.iter_s
      (fun (rowid_outer, old_row) ->
         let new_row = apply_assignments ~clock ~params assignments old_row in
         Lwt_list.iter_s
           (fun (child_meta, fks) ->
              Lwt_list.iter_s
                (precheck_update_fk
                   store
                   cat
                   table_meta
                   ~rowid_outer
                   ~old_row
                   ~new_row
                   child_meta)
                fks)
           child_refs)
      matches
;;

(* First UPDATE pass: validate UNIQUE for every target row against the full
   set of new values (an updated row may collide with another updated row). *)
let validate_update_unique
      tx
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~indexes
      ~assignments
      matches
  : unit Lwt.t
  =
  let schema = table_meta.Cat.columns in
  Lwt_list.iter_s
    (fun (rowid, old_row) ->
       let new_row = apply_assignments ~clock ~params assignments old_row in
       compute_stored_generated_cols clock params table_meta new_row;
       eval_check_constraints clock params table_meta new_row;
       let new_row_for_idx = with_computed_virtuals clock params table_meta new_row in
       check_indexes_unique_on_update
         tx
         indexes
         ~clock
         ~params
         ~schema
         ~old_row
         ~new_row_for_idx
         ~rowid)
    matches
;;

(* Apply the ON UPDATE cascade of one [fk] for a parent row changing
   [old_row] -> [new_row], within the RW txn (RESTRICT handled in precheck). *)
let apply_update_cascade_fk
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~visited
      ~(old_row : Row.t)
      ~(new_row : Row.t)
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
  : unit Lwt.t
  =
  (* #765 review round 4, item 1: [resolve_fk_col_idxs] instead of the raw,
     crashing [find_col_idx_by_name] -- same fix as {!cascade_delete_fk}. *)
  let* parent_col_idxs =
    resolve_fk_col_idxs
      ~table_name:table_meta.Cat.name
      table_meta.Cat.columns
      fk.fk_parent_cols
  in
  let old_vals = List.map (fun i -> old_row.(i)) parent_col_idxs in
  let new_vals = List.map (fun i -> new_row.(i)) parent_col_idxs in
  let unchanged =
    List.for_all2 (fun ov nv -> compare_values ov nv = 0) old_vals new_vals
  in
  if unchanged
  then Lwt.return_unit
  else if any_null_val old_vals
  then Lwt.return_unit
  else
    let* child_col_idxs =
      resolve_fk_col_idxs
        ~table_name:child_meta.Cat.name
        child_meta.Cat.columns
        fk.fk_local_cols
    in
    match fk.fk_on_update with
    | Cat.FA_restrict | Cat.FA_no_action -> Lwt.return_unit
    | Cat.FA_cascade ->
      let* child_rows =
        scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals:old_vals
      in
      (* For cascade, use the first child col (single-col FK compat) *)
      let child_col_idx = List.hd child_col_idxs in
      let new_val_single = List.hd new_vals in
      Lwt_list.iter_s
        (fun (crid, crow) ->
           cascade_update_col_in_tx
             tx
             cat
             ~visited
             clock
             params
             child_meta
             ~rowid:crid
             ~row:crow
             ~col_idx:child_col_idx
             ~new_val:new_val_single)
        child_rows
    | Cat.FA_set_null ->
      let* child_rows =
        scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals:old_vals
      in
      cascade_apply_set_null
        tx
        cat
        ~clock
        ~params
        ~visited
        ~op_label:"ON UPDATE"
        child_meta
        ~child_col_idxs
        child_rows
    | Cat.FA_set_default ->
      let* child_rows =
        scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals:old_vals
      in
      cascade_apply_set_default
        tx
        cat
        ~clock
        ~params
        ~visited
        ~op_label:"ON UPDATE"
        child_meta
        ~child_col_idxs
        child_rows
;;

(* Apply all ON UPDATE cascades for a parent row changing old_row -> new_row. *)
let apply_update_cascades
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~visited
      ~child_refs
      ~(old_row : Row.t)
      ~(new_row : Row.t)
  : unit Lwt.t
  =
  if child_refs = []
  then Lwt.return_unit
  else
    Lwt_list.iter_s
      (fun (child_meta, fks) ->
         Lwt_list.iter_s
           (apply_update_cascade_fk
              tx
              cat
              table_meta
              ~clock
              ~params
              ~visited
              ~old_row
              ~new_row
              child_meta)
           fks)
      child_refs
;;

(* Apply one matched UPDATE row: compute new row, run ON UPDATE cascades,
   reindex, and overwrite the row in the table tree. *)
let apply_update_row
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~child_refs
      ~indexes
      ~assignments
      (rowid, old_row)
  : (int64 * Row.t) Lwt.t
  =
  let new_row = apply_assignments ~clock ~params assignments old_row in
  compute_stored_generated_cols clock params table_meta new_row;
  (* Phase 35 task 3a: per-row visited set seeded with parent rowid, so
     cyclic ON UPDATE cascades terminate. *)
  let visited = Hashtbl.create 16 in
  Hashtbl.add visited (table_meta.Cat.name, rowid) ();
  let* () =
    apply_update_cascades
      tx
      cat
      table_meta
      ~clock
      ~params
      ~visited
      ~child_refs
      ~old_row
      ~new_row
  in
  (* #243/#249: re-keys the row when the UPDATE changed the INTEGER PRIMARY KEY
     alias column (uniqueness probe + del-old/put-new + reindex), else rewrites
     in place.  Shared with the UPSERT and ON UPDATE CASCADE paths. *)
  let* new_rowid =
    write_row_rekeyed
      tx
      table_meta
      ~clock
      ~params
      ~old_row
      ~new_row
      ~old_rowid:rowid
      ~indexes
      ()
  in
  (* Return the row as actually stored (generated columns included) so callers
     such as UPDATE ... RETURNING can project committed values, not a pre-lock
     snapshot (#226); [new_rowid] (#417) differs from [rowid] only when the UPDATE
     moved the INTEGER-PK alias. *)
  Lwt.return (new_rowid, new_row)
;;

(* Fire an UPDATE row-hook (BEFORE/AFTER) for each matched row, recomputing
   the post-UPDATE row from the pre-write snapshot.  For non-deterministic
   expressions (random(), now()) the value the trigger sees may differ from
   the committed row. *)
let run_update_hook ~clock ~params ~assignments ~tx hook matches : unit Lwt.t =
  match hook with
  | None -> Lwt.return_unit
  | Some f ->
    Lwt_list.iter_s
      (fun (_rowid, old_row) ->
         let new_row = apply_assignments ~clock ~params assignments old_row in
         f ~tx ~old_row ~new_row)
      matches
;;

(** Run [Op_update]: drain matching rows into a list (snapshot read),
    then for each (rowid, old_row) compute the new row, update index
    entries, and overwrite the row in the table tree.  Returns the
    number of rows whose contents were modified. *)
let execute_update
      ?(mode = Auto)
      ?(params = [||])
      ?(clock : (unit -> float) option = None)
      ?(before_hook :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      ?(after_hook :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      ?(collect : (Row.t -> unit) option = None)
      (store : S.t)
      (cat : Cat.t)
      ~(table_meta : Cat.table_meta)
      ~(assignments : (int * Plan.expr) list)
      ~(where : Plan.expr option)
      ~(seek : Plan.seek option)
      ~(order : (Plan.expr * [ `Asc | `Desc ] * [ `Nulls_first | `Nulls_last ]) list)
      ~(limit : int option)
      ~(offset : int option)
      ~(indexes : Cat.index_info list)
  : int Lwt.t
  =
  (* Acquire the write lock BEFORE draining so the read-modify-write is atomic.
     Draining via a separate RO snapshot first (the old [Auto] path) let a
     concurrent commit land between the row read and the lock acquisition, so
     the new row was computed from a stale value and clobbered that commit —
     a lost update on backends whose commit yields, e.g. WAL/file (#223).
     [In_txn] already drained under the caller's lock; this makes [Auto] match. *)
  let* tx, owned = acquire_txn store mode in
  Lwt.catch
    (fun () ->
       let* matches =
         drain_matching_rows_in_tx ~seek tx cat table_meta ~clock ~params ~where
       in
       let matches =
         apply_order_offset_limit ~clock ~params ~order ~offset ~limit matches
       in
       let n = List.length matches in
       if n = 0
       then
         let* () = if owned then S.rollback tx else Lwt.return_unit in
         Lwt.return 0
       else
         let* child_refs =
           if Cat.get_fk_enforcement cat
           then build_child_refs cat ~parent_table_name:table_meta.Cat.name
           else Lwt.return []
         in
         (* FK pre-check: fail for RESTRICT/NO_ACTION when a referenced key
            changes.  CASCADE/SET_NULL/SET_DEFAULT are applied in the txn below. *)
         let* () =
           precheck_update_fk_restrict
             store
             cat
             table_meta
             ~clock
             ~params
             ~assignments
             ~child_refs
             matches
         in
         (* Phase 38: BEFORE/AFTER UPDATE fire inside the parent txn so nested
            DML shares it (atomic rollback on failure; no nested-trigger
            deadlock). *)
         let* () = run_update_hook ~clock ~params ~assignments ~tx before_hook matches in
         let* () =
           validate_update_unique
             tx
             table_meta
             ~clock
             ~params
             ~indexes
             ~assignments
             matches
         in
         let* () =
           Lwt_list.iter_s
             (fun ((rowid, old_row) as m) ->
                let* new_rowid, new_row =
                  apply_update_row
                    tx
                    cat
                    table_meta
                    ~clock
                    ~params
                    ~child_refs
                    ~indexes
                    ~assignments
                    m
                in
                (* #417: capture the per-row pre/post images for the delta feed
                   (no-op unless a change-capturing accumulator is installed); a
                   rowid-changing UPDATE is recorded as Deleted+Inserted. *)
                record_update
                  table_meta.Cat.name
                  ~old_rowid:rowid
                  ~new_rowid
                  ~old_row
                  ~new_row;
                (* RETURNING / row collection sees the committed row (#226). *)
                (match collect with
                 | Some f -> f new_row
                 | None -> ());
                Lwt.return_unit)
             matches
         in
         let* () = run_update_hook ~clock ~params ~assignments ~tx after_hook matches in
         let* () = release_txn ~cat tx owned in
         if n > 0 then mark_dirty table_meta.Cat.name;
         Lwt.return n)
    (fun exn ->
       let* () = if owned then S.rollback tx else Lwt.return_unit in
       Lwt.fail exn)
;;

(* Pre-write RESTRICT/NO ACTION FK check for one DELETE row's [fk]: if a
   child still references the row being deleted, raise (or queue deferred). *)
let precheck_delete_fk
      store
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~rowid_outer
      ~(row : Row.t)
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
  : unit Lwt.t
  =
  match fk.fk_on_delete with
  | Cat.FA_cascade | Cat.FA_set_null | Cat.FA_set_default -> Lwt.return_unit
  | Cat.FA_restrict | Cat.FA_no_action ->
    let is_deferred = fk.fk_deferrable || Cat.get_defer_fks_pragma cat in
    (* #765 review round 3, item 2: same fix as {!precheck_update_fk}. *)
    let* parent_col_idxs =
      resolve_fk_col_idxs
        ~table_name:table_meta.Cat.name
        table_meta.Cat.columns
        fk.fk_parent_cols
    in
    let parent_vals = List.map (fun i -> row.(i)) parent_col_idxs in
    if any_null_val parent_vals
    then Lwt.return_unit
    else
      let* child_col_idxs =
        resolve_fk_col_idxs
          ~table_name:child_meta.Cat.name
          child_meta.Cat.columns
          fk.fk_local_cols
      in
      let* has_ref =
        fk_child_has_ref_multi cat store child_meta ~child_col_idxs ~parent_vals
      in
      if has_ref
      then (
        let msg =
          Printf.sprintf
            "FOREIGN KEY constraint failed: '%s.%s' is still referenced by '%s.%s'"
            table_meta.Cat.name
            (String.concat "," fk.fk_parent_cols)
            child_meta.Cat.name
            (String.concat "," fk.fk_local_cols)
        in
        let ord = Option.value (fk_ordinal child_meta fk) ~default:(-1) in
        let recheck =
          make_fk_recheck
            cat
            ~child_name:child_meta.Cat.name
            ~parent_name:table_meta.Cat.name
            ~fk_ordinal:ord
            ~parent_vals
        in
        fk_violation
          ~deferred:is_deferred
          cat
          ~kind:`Delete
          ~table:table_meta.Cat.name
          ~rowid:rowid_outer
          ~msg
          ~recheck
          ~child_table:child_meta.Cat.name
          ~fk_ordinal:ord)
      else Lwt.return_unit
;;

(* Pre-write FK RESTRICT check across all matched DELETE rows. *)
let precheck_delete_fk_restrict
      store
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~child_refs
      matches
  : unit Lwt.t
  =
  if child_refs = []
  then Lwt.return_unit
  else
    Lwt_list.iter_s
      (fun (rowid_outer, row) ->
         Lwt_list.iter_s
           (fun (child_meta, fks) ->
              Lwt_list.iter_s
                (precheck_delete_fk store cat table_meta ~rowid_outer ~row child_meta)
                fks)
           child_refs)
      matches
;;

(* Apply the ON DELETE cascade of one [fk] for parent [row] being deleted,
   within the RW txn (RESTRICT handled in precheck). *)
let apply_delete_cascade_fk
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~visited
      ~(row : Row.t)
      (child_meta : Cat.table_meta)
      (fk : Cat.fk_constraint)
  : unit Lwt.t
  =
  (* #765 review round 4, item 1: [resolve_fk_col_idxs] instead of the raw,
     crashing [find_col_idx_by_name] -- same fix as {!cascade_delete_fk}. *)
  let* parent_col_idxs =
    resolve_fk_col_idxs
      ~table_name:table_meta.Cat.name
      table_meta.Cat.columns
      fk.fk_parent_cols
  in
  let parent_vals = List.map (fun i -> row.(i)) parent_col_idxs in
  if any_null_val parent_vals
  then Lwt.return_unit
  else
    let* child_col_idxs =
      resolve_fk_col_idxs
        ~table_name:child_meta.Cat.name
        child_meta.Cat.columns
        fk.fk_local_cols
    in
    match fk.fk_on_delete with
    | Cat.FA_restrict | Cat.FA_no_action -> Lwt.return_unit
    | Cat.FA_cascade ->
      let* child_rows =
        scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals
      in
      Lwt_list.iter_s
        (fun (crid, crow) ->
           cascade_delete_row_in_tx
             tx
             cat
             ~visited
             clock
             params
             child_meta
             ~rowid:crid
             ~row:crow)
        child_rows
    | Cat.FA_set_null ->
      let* child_rows =
        scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals
      in
      cascade_apply_set_null
        tx
        cat
        ~clock
        ~params
        ~visited
        ~op_label:"ON DELETE"
        child_meta
        ~child_col_idxs
        child_rows
    | Cat.FA_set_default ->
      let* child_rows =
        scan_child_rows_multi_tx cat tx child_meta ~child_col_idxs ~parent_vals
      in
      cascade_apply_set_default
        tx
        cat
        ~clock
        ~params
        ~visited
        ~op_label:"ON DELETE"
        child_meta
        ~child_col_idxs
        child_rows
;;

(* Apply all ON DELETE cascades for parent [row] being deleted. *)
let apply_delete_cascades
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~visited
      ~child_refs
      ~(row : Row.t)
  : unit Lwt.t
  =
  if child_refs = []
  then Lwt.return_unit
  else
    Lwt_list.iter_s
      (fun (child_meta, fks) ->
         Lwt_list.iter_s
           (apply_delete_cascade_fk
              tx
              cat
              table_meta
              ~clock
              ~params
              ~visited
              ~row
              child_meta)
           fks)
      child_refs
;;

(* Delete one matched row: run ON DELETE cascades, remove index entries, then
   remove the row.  Visited set seeded with this row so cyclic cascades stop. *)
let apply_delete_row
      tx
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      ~clock
      ~params
      ~child_refs
      ~indexes
      (rowid, row)
  : unit Lwt.t
  =
  let visited = Hashtbl.create 16 in
  Hashtbl.add visited (table_meta.Cat.name, rowid) ();
  let* () =
    apply_delete_cascades tx cat table_meta ~clock ~params ~visited ~child_refs ~row
  in
  let rowid_key = Rowid.encode rowid in
  (* row from drain_matching_rows_in_tx → decode_with_virtual: VIRTUAL cols
     already applied in-place, so pass directly as row_for_idx. *)
  let* () =
    delete_row_indexes tx table_meta ~clock ~params ~row_for_idx:row ~rowid indexes
  in
  let* () =
    S.del
      tx
      (let x, _, _, _ = Cat.row_storage table_meta in
       x)
      rowid_key
  in
  (* #409: reuse a plain rowid table's high-water after a committed delete. *)
  Cat.note_rowid_deleted cat ~name:table_meta.Cat.name ~rowid tx
;;

(** Run [Op_delete]: drain matching rows into a list (snapshot read),
    then for each matching (rowid, row) remove index entries and the
    row itself from the table tree.  Returns the number of rows deleted. *)
let execute_delete
      ?(mode = Auto)
      ?(params = [||])
      ?(clock : (unit -> float) option = None)
      ?(before_hook : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option = None)
      ?(after_hook : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option = None)
      ?(collect : (Row.t -> unit) option = None)
      (store : S.t)
      (cat : Cat.t)
      ~(table_meta : Cat.table_meta)
      ~(where : Plan.expr option)
      ~(seek : Plan.seek option)
      ~(order : (Plan.expr * [ `Asc | `Desc ] * [ `Nulls_first | `Nulls_last ]) list)
      ~(limit : int option)
      ~(offset : int option)
      ~(indexes : Cat.index_info list)
  : int Lwt.t
  =
  (* Acquire the write lock BEFORE draining so the match set can't go stale
     between the read and the delete (same TOCTOU as the UPDATE path, #223):
     otherwise a row matched on a separate RO snapshot could be concurrently
     modified to no longer match — yet still be deleted.  [In_txn] already
     drained under the caller's lock; this makes [Auto] match. *)
  let* tx, owned = acquire_txn store mode in
  Lwt.catch
    (fun () ->
       let* matches =
         drain_matching_rows_in_tx ~seek tx cat table_meta ~clock ~params ~where
       in
       let matches =
         apply_order_offset_limit ~clock ~params ~order ~offset ~limit matches
       in
       let n = List.length matches in
       if n = 0
       then
         let* () = if owned then S.rollback tx else Lwt.return_unit in
         Lwt.return 0
       else
         (* FK pre-check: fail for RESTRICT/NO_ACTION; CASCADE/SET_NULL/SET_DEFAULT
            are applied inside the RW transaction below. *)
         let* child_refs =
           if Cat.get_fk_enforcement cat
           then build_child_refs cat ~parent_table_name:table_meta.Cat.name
           else Lwt.return []
         in
         let* () = precheck_delete_fk_restrict store cat table_meta ~child_refs matches in
         (* Phase 38: BEFORE/AFTER DELETE fire inside the parent txn so nested
            DML shares it and trigger failures roll back the DELETE. *)
         let* () =
           match before_hook with
           | None -> Lwt.return_unit
           | Some f -> Lwt_list.iter_s (fun (_rowid, old_row) -> f ~tx ~old_row) matches
         in
         let* () =
           Lwt_list.iter_s
             (fun ((rowid, old_row) as m) ->
                let* () =
                  apply_delete_row tx cat table_meta ~clock ~params ~child_refs ~indexes m
                in
                (* #417: capture the removed row for the delta feed (no-op unless
                   a change-capturing accumulator is installed). *)
                record_change table_meta.Cat.name (Deleted { rowid; row = old_row });
                (* RETURNING / row collection sees the row as deleted under the
                   write lock, not a pre-lock snapshot (#226). *)
                (match collect with
                 | Some f -> f old_row
                 | None -> ());
                Lwt.return_unit)
             matches
         in
         let* () =
           match after_hook with
           | None -> Lwt.return_unit
           | Some f -> Lwt_list.iter_s (fun (_rowid, old_row) -> f ~tx ~old_row) matches
         in
         let* () = release_txn ~cat tx owned in
         if n > 0 then mark_dirty table_meta.Cat.name;
         Lwt.return n)
    (fun exn ->
       let* () = if owned then S.rollback tx else Lwt.return_unit in
       Lwt.fail exn)
;;

(** Run [Op_drop_table]: remove catalog entries for the table and all
    its indexes.  The B+-tree pages are NOT reclaimed in Phase 2.

    #279: runs through [with_ddl_txn] so it participates in any ambient explicit
    transaction (borrowed [In_txn]) or owns its own auto-committed txn ([Auto]),
    inheriting the same poison-on-failure / no-partial-effect-COMMIT behaviour as
    CREATE/ALTER (#286).  [drop_table] removes the table AND its dependent
    indexes from the in-memory cache before the caller commits, so a schema-cache
    undo is registered to restore both on a [ROLLBACK] (the store reverts the
    _sys_* row deletes; this re-syncs the cache).  The undo restores EXACTLY the
    entries [drop_table] removes; if an index was itself created earlier in the
    same transaction, this DROP undo restores it but the earlier CREATE INDEX's
    undo — running later in LIFO order — removes it again, netting the correct
    "absent after ROLLBACK" outcome. *)
let execute_drop_table
      ?(mode = Auto)
      (store : S.t)
      (cat : Cat.t)
      ~(table_meta : Cat.table_meta)
      ~(_indexes : Cat.index_info list)
  : unit Lwt.t
  =
  let name = table_meta.Cat.name in
  let* () =
    with_ddl_txn store cat mode (fun tx ->
      (* #283: [Cat.drop_table] self-registers the cache undo for the table and each
         dependent index (via [Schema_cache.remove_table]/[remove_index]), so no
         external snapshot+undo is needed here. *)
      Cat.drop_table cat tx ~name)
  in
  (* #405: the table's rows are gone, so every cached result over [name] is
     stale.  Marked AFTER the drop succeeds: a raising DROP marks nothing, which
     matches [Db]'s [Error] arm discarding the accumulator. *)
  mark_dirty name;
  Lwt.return_unit
;;

(** Run [Op_drop_index]: remove catalog entry for the index.
    The B+-tree pages are NOT reclaimed in Phase 2.

    #279: as for [execute_drop_table] — runs through [with_ddl_txn] and registers
    a schema-cache undo so a [ROLLBACK] restores the dropped index entry. *)
let execute_drop_index
      ?(mode = Auto)
      (store : S.t)
      (cat : Cat.t)
      ~(idx_info : Cat.index_info)
  : unit Lwt.t
  =
  with_ddl_txn store cat mode (fun tx ->
    (* #283: [Cat.drop_index] self-registers the cache undo. *)
    Cat.drop_index cat tx ~name:idx_info.Cat.idx_name)
;;

(* ------------------------------------------------------------------ *)
(* EXPLAIN plan-tree pretty-printer                                     *)
(* ------------------------------------------------------------------ *)

let op_name = function
  | Plan.Op_seq_scan { table_meta; _ } -> "SeqScan(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_col_seq_scan { table_meta; _ } -> "ColSeqScan(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_filter _ -> "Filter"
  | Plan.Op_project _ -> "Project"
  | Plan.Op_expr_project _ -> "ExprProject"
  | Plan.Op_sort _ -> "Sort"
  | Plan.Op_limit { limit; offset; _ } ->
    Printf.sprintf "Limit(%d offset %d)" limit offset
  | Plan.Op_aggregate _ -> "Aggregate"
  | Plan.Op_hash_join { join_kind; on_pred; _ } ->
    (* #552: a general ON predicate on an outer join lives inside the join
       rather than in an [Op_filter] above it, so without this suffix the
       predicate disappears from EXPLAIN entirely and the [`Inner] and [`Left]
       spellings of the same query explain differently for no visible reason. *)
    let on = if Option.is_some on_pred then "(ON)" else "" in
    (match join_kind with
     | `Inner -> "HashJoin" ^ on
     | `Left -> "LeftHashJoin" ^ on)
  | Plan.Op_nested_loop_join { join_kind; right_meta; _ } ->
    (match join_kind with
     | `Inner -> "NestedLoopJoin(" ^ right_meta.Cat.name ^ ")"
     | `Left -> "LeftNestedLoopJoin(" ^ right_meta.Cat.name ^ ")")
  | Plan.Op_index_lookup { table_meta; _ } -> "IndexLookup(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_rowid_lookup { table_meta; _ } -> "RowidLookup(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_union { all; _ } -> if all then "UnionAll" else "Union"
  | Plan.Op_intersect _ -> "Intersect"
  | Plan.Op_except _ -> "Except"
  | Plan.Op_distinct _ -> "Distinct"
  | Plan.Op_const_select _ -> "ConstSelect"
  | Plan.Op_window _ -> "Window"
  | Plan.Op_with_cte { cte_name; _ } -> "WithCte(" ^ cte_name ^ ")"
  | Plan.Op_cte_scan { cte_name; _ } -> "CteScan(" ^ cte_name ^ ")"
  | Plan.Op_insert { table_meta; _ } -> "Insert(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_insert_select { table_meta; _ } -> "InsertSelect(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_update { table_meta; _ } -> "Update(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_delete { table_meta; _ } -> "Delete(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_create_table { name; _ } -> "CreateTable(" ^ name ^ ")"
  | Plan.Op_col_create_table { name; _ } -> "ColCreateTable(" ^ name ^ ")"
  | Plan.Op_create_index { name; table; _ } ->
    "CreateIndex(" ^ name ^ " on " ^ table ^ ")"
  | Plan.Op_drop_table { table_meta; _ } -> "DropTable(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_drop_index { idx_info } -> "DropIndex(" ^ idx_info.Cat.idx_name ^ ")"
  | Plan.Op_alter_table { table_meta; _ } -> "AlterTable(" ^ table_meta.Cat.name ^ ")"
  | Plan.Op_begin -> "Begin"
  | Plan.Op_commit -> "Commit"
  | Plan.Op_rollback -> "Rollback"
  | Plan.Op_savepoint name -> "Savepoint(" ^ name ^ ")"
  | Plan.Op_release name -> "Release(" ^ name ^ ")"
  | Plan.Op_rollback_to name -> "RollbackTo(" ^ name ^ ")"
  | Plan.Op_create_view { name; _ } -> "CreateView(" ^ name ^ ")"
  | Plan.Op_create_reactive_view { name; _ } -> "CreateReactiveView(" ^ name ^ ")"
  | Plan.Op_drop_view { name } -> "DropView(" ^ name ^ ")"
  | Plan.Op_drop_reactive_view { name; _ } -> "DropReactiveView(" ^ name ^ ")"
  | Plan.Op_create_trigger { name; _ } -> "CreateTrigger(" ^ name ^ ")"
  | Plan.Op_drop_trigger { name } -> "DropTrigger(" ^ name ^ ")"
  | Plan.Op_pragma_rows _ -> "Pragma"
  | Plan.Op_pragma_get_user_version -> "Pragma(get_user_version)"
  | Plan.Op_pragma_set_user_version { version } ->
    Printf.sprintf "Pragma(set_user_version=%Ld)" version
  | Plan.Op_pragma_integrity_check -> "Pragma(integrity_check)"
  | Plan.Op_pragma_not_null_check -> "Pragma(not_null_check)"
  | Plan.Op_pragma_not_null_repair -> "Pragma(not_null_repair)"
  | Plan.Op_pragma_get_fk -> "Pragma(get_foreign_keys)"
  | Plan.Op_pragma_set_fk { on } -> Printf.sprintf "Pragma(set_foreign_keys=%b)" on
  | Plan.Op_pragma_get_recursive_triggers -> "Pragma(get_recursive_triggers)"
  | Plan.Op_pragma_set_recursive_triggers { on } ->
    Printf.sprintf "Pragma(set_recursive_triggers=%b)" on
  | Plan.Op_pragma_get_defer_fk -> "Pragma(get_defer_foreign_keys)"
  | Plan.Op_pragma_set_defer_fk { on } ->
    Printf.sprintf "Pragma(set_defer_foreign_keys=%b)" on
  | Plan.Op_pragma_wal_checkpoint -> "Pragma(wal_checkpoint)"
  | Plan.Op_pragma_checkpoint_status -> "Pragma(checkpoint_status)"
  | Plan.Op_pragma_wal_replay_check -> "Pragma(wal_replay_check)"
  | Plan.Op_pragma_get_wal_autocheckpoint -> "Pragma(get_wal_autocheckpoint)"
  | Plan.Op_pragma_set_wal_autocheckpoint { n } ->
    Printf.sprintf "Pragma(set_wal_autocheckpoint=%Ld)" n
  | Plan.Op_pragma_get_synchronous -> "Pragma(get_synchronous)"
  | Plan.Op_pragma_set_synchronous { mode } ->
    Printf.sprintf "Pragma(set_synchronous=%s)" mode
  | Plan.Op_pragma_get_wal_batch_commits -> "Pragma(get_wal_batch_commits)"
  | Plan.Op_pragma_set_wal_batch_commits { n } ->
    Printf.sprintf "Pragma(set_wal_batch_commits=%Ld)" n
  | Plan.Op_pragma_get_wal_batch_interval_ms -> "Pragma(get_wal_batch_interval_ms)"
  | Plan.Op_pragma_set_wal_batch_interval_ms { n } ->
    Printf.sprintf "Pragma(set_wal_batch_interval_ms=%Ld)" n
  | Plan.Op_vacuum -> "Vacuum"
  | Plan.Op_attach { schema; _ } -> Printf.sprintf "Attach(%s)" schema
  | Plan.Op_detach { schema } -> Printf.sprintf "Detach(%s)" schema
  | Plan.Op_database_list -> "Pragma(database_list)"
  | Plan.Op_active_database_get -> "Pragma(active_database)"
  | Plan.Op_active_database_set { schema } ->
    Printf.sprintf "Pragma(active_database=%s)" schema
  | Plan.Op_no_op -> "NoOp"
  | Plan.Op_changes -> "Changes"
  | Plan.Op_last_insert_rowid -> "LastInsertRowid"
  | Plan.Op_total_changes -> "TotalChanges"
  | Plan.Op_explain { analyze; _ } -> if analyze then "ExplainAnalyze" else "Explain"
  | Plan.Op_create_fts_table { name; _ } -> "CreateFtsTable(" ^ name ^ ")"
  | Plan.Op_fts_insert { fts_meta; _ } -> "FtsInsert(" ^ fts_meta.Cat.fts_name ^ ")"
  | Plan.Op_fts_delete { fts_meta; _ } -> "FtsDelete(" ^ fts_meta.Cat.fts_name ^ ")"
  | Plan.Op_fts_seq_scan { fts_meta; _ } -> "FtsSeqScan(" ^ fts_meta.Cat.fts_name ^ ")"
  | Plan.Op_fts_match_scan { fts_meta; limit; offset; _ } ->
    (* #687: limit/offset are fields on this op rather than a wrapping
       [Op_limit] (see [stream_fts_match_scan]), so without this suffix a
       MATCH query's LIMIT/OFFSET would vanish from EXPLAIN entirely — the
       sibling [Op_fts_seq_scan] path still gets a visible [Op_limit] node.
       Same rendering as [Op_limit] itself, so the two read the same way. *)
    let limit_suffix =
      match limit with
      | None -> ""
      | Some n -> Printf.sprintf " Limit(%d offset %d)" n (Option.value ~default:0 offset)
    in
    "FtsMatchScan(" ^ fts_meta.Cat.fts_name ^ ")" ^ limit_suffix
  | Plan.Op_sqlite_master -> "SqliteMaster"
  | Plan.Op_sqlite_sequence -> "SqliteSequence"
  | Plan.Op_seq_set { table; _ } -> "SeqSet(" ^ table ^ ")"
  | Plan.Op_seq_reset { table } ->
    "SeqReset("
    ^ (match table with
       | Some t -> t
       | None -> "*")
    ^ ")"
;;

let op_children = function
  | Plan.Op_filter { child; _ } -> [ child ]
  | Plan.Op_project { child; _ } -> [ child ]
  | Plan.Op_expr_project { child; _ } -> [ child ]
  | Plan.Op_sort { child; _ } -> [ child ]
  | Plan.Op_limit { child; _ } -> [ child ]
  | Plan.Op_distinct { child } -> [ child ]
  | Plan.Op_aggregate { child; _ } -> [ child ]
  | Plan.Op_window { child; _ } -> [ child ]
  | Plan.Op_hash_join { left; right; _ } -> [ left; right ]
  | Plan.Op_nested_loop_join { left; _ } -> [ left ]
  | Plan.Op_union { left; right; _ } -> [ left; right ]
  | Plan.Op_intersect { left; right } -> [ left; right ]
  | Plan.Op_except { left; right } -> [ left; right ]
  | Plan.Op_with_cte { def; query; _ } -> [ def; query ]
  | Plan.Op_explain { inner; _ } -> [ inner ]
  | Plan.Op_insert_select { source; _ } -> [ source ]
  | _ -> []
;;

let explain_plan op =
  let counter = ref 0 in
  let rec walk parent op =
    let id = !counter in
    incr counter;
    let my_row =
      [| Row.V_int (Int64.of_int id)
       ; Row.V_int (Int64.of_int parent)
       ; Row.V_text (op_name op)
      |]
    in
    my_row :: List.concat_map (walk id) (op_children op)
  in
  walk (-1) op
;;

(* #239: per-query cost/stats signal for an external cost-based cache.
   [rows_examined] counts rows the executor pulled from a base table/index scan
   (the true work signal: a query that scans a million rows to return one reads
   examined=1_000_000, returned=1); [rows_returned] is the size of the result
   stream once drained; [used_index] is the plan-time fact that the base access
   is an index/rowid seek rather than a full scan.  Mirage-pure — plain counters
   the executor already has, no clock/Unix dependency.

   #546: [index_entries] counts the INDEX entries an index lookup walked, which
   [rows_examined] does not — it counts only the table rows the walk then
   fetched.  The two differ by exactly the work an index seek adds over the
   ordered leaf walk it replaces: [stream_index_lookup] does one [rh_get] on the
   table tree per entry, so an unselective seek can pay N tree descents to
   examine the same N rows a scan would have read sequentially, and no counter
   could tell the two plans apart.  Entries skipped by a range's [past_end] stop
   are not counted: the walk ends at the first one. *)
type query_stats =
  { mutable rows_examined : int
  ; mutable rows_returned : int
  ; mutable index_entries : int
  ; mutable used_index : bool
  }

let make_query_stats () =
  { rows_examined = 0; rows_returned = 0; index_entries = 0; used_index = false }
;;

(* The active query's stats record, propagated to the base scanners via Lwt
   sequence-associated storage rather than threaded through the ~50 mutually
   recursive [to_stream] helpers.  A leaf scanner reads it ONCE at stream
   construction (which runs inside [query]'s [with_value] scope, carried across
   binds), captures the result in its row-producing closure, and increments per
   row pulled — correct regardless of when the lazy stream is later drained, and
   safe across interleaved fibres because each query has its own record. *)
let query_stats_key : query_stats Lwt.key = Lwt.new_key ()

(* #677: a registry of cleanup thunks for the lazy base scanners
   (stream_seq_scan / stream_index_lookup / stream_fts_seq_scan) that hold a
   live Store reader handle and cursor across pulls, releasing it only via
   their own idempotent [finish] closure when the stream is drained to
   exhaustion or raises.  [Op_limit] needs to stop pulling before
   exhaustion — that's the whole point of early-stop — without leaking that
   handle, which is exactly the #164/#493/#546 class of bug.

   [Op_limit] opens a scope with [Lwt.with_value] around its child's
   construction; the 3 lazy scanners push their [finish] into this key if a
   scope is active, and [Op_limit] flushes every registered thunk once it
   has enough rows.  Flushing is safe even when nothing is left to do:
   every [finish] is idempotent (guarded by its own [ended] ref), so a
   thunk that already ran because the child was fully drained naturally is
   just a no-op the second time.

   Nesting is handled by [Lwt.with_value]'s ordinary dynamic scoping: a
   nested [Op_limit] (e.g. inside a correlated subquery) opens its own inner
   scope for its own child construction, so its scanners register into its
   own registry, not this one's. *)
let stream_cleanup_key : (unit -> unit Lwt.t) list ref Lwt.key = Lwt.new_key ()

let register_stream_cleanup (finish : unit -> unit Lwt.t) : unit =
  match Lwt.get stream_cleanup_key with
  | Some reg -> reg := finish :: !reg
  | None -> ()
;;

(* #262: the active transaction mode for the query currently executing, carried
   in Lwt sequence-associated storage.  Subquery evaluation ([pre_eval_subquery]
   and the correlated re-eval at pull time) reads it to run inner reads under the
   same txn, so [SELECT … WHERE x IN (SELECT …)] inside an open transaction sees
   the transaction's own uncommitted writes — not just the top-level scan.  The
   base scanners take [mode] as an explicit argument and do not consult this. *)
let txn_mode_key : txn_mode Lwt.key = Lwt.new_key ()

let current_txn_mode () =
  match Lwt.get txn_mode_key with
  | Some mode -> mode
  | None -> Auto
;;

(* #493: the per-query plan cache for correlated subqueries, keyed by the inner
   [Ast.stmt] as it stands AFTER outer-reference substitution.

   It is only sound to install because #493 also changed that substitution to
   emit a positional PARAMETER for each outer reference rather than the row's
   literal value: with a literal the substituted statement differed on every
   outer row, so the table would have grown without bound and never hit.  With a
   parameter the substituted statement is structurally identical for every row,
   so one bind+plan serves the whole scan and the table holds one entry per
   subquery site.

   Anything that reinstates literal substitution at a cached site MUST pass
   [~cache:None] there, or the cache becomes a per-row memory leak.

   The value is an OPTION so that a statement which does not bind — the #592
   unresolvable correlation — is remembered as such rather than re-bound per
   row.

   KNOWN LIMIT (#493 review): the table is created inside [stream_filter] /
   [stream_expr_project], which run once per [to_stream].  For a NESTED
   correlated subquery — an EXISTS inside an EXISTS — [to_stream] on the cached
   outer plan is invoked once per outer row, so the inner [stream_filter]
   allocates a fresh table each time and the innermost subquery still pays a
   full bind+plan per outer row.  Correct, just not accelerated; it is the one
   shape where "plan once, execute N times" does not apply.  Lifting the table
   to the whole query (a [Lwt.with_value] at [query]) would fix it, and is
   deliberately out of scope here. *)
type subplan_cache = (Ast.stmt, Plan.op option) Hashtbl.t

let subplan_cache_key : subplan_cache Lwt.key = Lwt.new_key ()

(* #493 review: the count-shaped observable for "plan once, execute N times".

   That was the one claim in #493 with nothing in the tree able to see it: the
   leak has [Store.active_reader_count], the seek has [index_entries], the
   short-circuit has [rows_examined], but a subquery re-planned per outer row
   and one planned once are indistinguishable in every counter that existed.
   This is the missing one — a monotone count of the times a subquery statement
   was actually bound and planned, as opposed to served from the cache.

   Diagnostic/testing only, exactly like [Store.active_reader_count] and
   [Store.pinned_page_count] (#164): a process-global counter, not per-query, so
   a test reads it either side of one query and takes the difference. It is an
   integer count with no clock in it, so a gate built on it needs no
   [GRANARY_BENCH_*] neutralizer. *)
let subquery_plans_built_ref = ref 0
let subquery_plans_built () = !subquery_plans_built_ref

(* #262: re-establish the per-query Lwt-storage contexts (the stats record, the
   txn mode, and #493's subquery plan cache) for work that runs at pull time —
   outside [query]'s construction-time [with_value] scope — currently the
   correlated-subquery re-eval in [stream_filter] / [stream_expr_project].
   Bundling them here keeps them in lock-step: a future pull-time site cannot
   restore one and silently drop the other (the exact omission #262 corrected
   for the mode). *)
let with_pull_context ~stats ~mode ~(cache : subplan_cache option) f =
  Lwt.with_value query_stats_key stats
  @@ fun () ->
  Lwt.with_value txn_mode_key (Some mode)
  @@ fun () -> Lwt.with_value subplan_cache_key cache f
;;

(* Increment via the closure-captured option; never calls [Lwt.get] at pull time
   (the consumer drains outside the [with_value] scope).  [None] for the common
   no-stats query is a single predicted branch with no per-row cost. *)
let incr_examined (s_opt : query_stats option) =
  match s_opt with
  | Some s -> s.rows_examined <- s.rows_examined + 1
  | None -> ()
;;

(* #546: same discipline as [incr_examined], for the index entries an index
   lookup walks. *)
let incr_index_entries (s_opt : query_stats option) =
  match s_opt with
  | Some s -> s.index_entries <- s.index_entries + 1
  | None -> ()
;;

(** Forward reference to [to_stream], which is defined in the mutually-recursive
    block starting at [pre_eval_subquery].  [execute_with_count] needs this to
    implement [Op_insert_select] (read source, then write rows). *)
let to_stream_ref
  : ((unit -> float) option
     -> Row.value array
     -> S.t
     -> ?mode:txn_mode
     -> ?cat:Cat.t option
     -> Plan.op
     -> Row.t Lwt_stream.t Lwt.t)
      ref
  =
  ref (fun _clock _params _store ?mode:_ ?cat:_ _op ->
    failwith "to_stream_ref not yet initialised")
;;

(** #588: forward reference to [not_null_repair_run], the shared core of
    [PRAGMA not_null_repair].  It lives in the same mutually-recursive block as
    [to_stream], and [execute_with_count] — defined above it — needs it so the
    repair is reachable through the WRITE api ([Db.execute]) and not only
    through [Db.query].  Returns the report rows and the number of rows actually
    deleted; the write path reports the latter as the statement's change count,
    the query path streams the former. *)
let not_null_repair_run_ref : (S.t -> txn_mode -> Cat.t -> (Row.t list * int) Lwt.t) ref =
  ref (fun _store _mode _cat -> failwith "not_null_repair_run_ref not yet initialised")
;;

(* Op_create_table: register the table, its UNIQUE indexes, and FK constraints. *)
(** [execute_with_count] returns the rows-affected count.  For most
    write ops this is 1 (INSERT) or 0 (DDL); for UPDATE it is the
    number of rows whose contents were modified. *)
let execute_create_table_op
      (store : S.t)
      (cat : Cat.t)
      ~mode
      ~name
      ~columns
      ~uniq_idxs
      ~if_not_exists
      ~fk_constraints
      ~without_rowid
      ~autoincrement
  : int Lwt.t
  =
  if Cat.table_exists cat ~name
  then
    if if_not_exists
    then Lwt.return 0
    else (* Raise synchronously (before acquiring any txn), as callers expect. *)
      failwith (Printf.sprintf "table '%s' already exists" name)
  else
    (* #269: the table, its implicit UNIQUE indexes, and its FK rows all go
       through one writer txn (the ambient explicit one if any), so the whole
       CREATE TABLE is atomic and never self-deadlocks. *)
    with_ddl_txn store cat mode (fun tx ->
      let* _tid =
        Cat.create_table ~txn:tx cat ~name ~columns ~without_rowid ~autoincrement
      in
      let* () =
        Lwt_list.iter_s
          (fun (idx_name, col_names, origin) ->
             let* result =
               Cat.create_index
                 ~txn:tx
                 cat
                 ~name:idx_name
                 ~table:name
                 ~columns:col_names
                 ~unique:true
                 ~expr_flags:(List.map (fun _ -> false) col_names)
                 ~where_sql:None
                 ~origin
             in
             match result with
             | Error msg -> Lwt.fail_with msg
             | Ok _ -> Lwt.return_unit)
          uniq_idxs
      in
      let* () =
        if fk_constraints = []
        then Lwt.return_unit
        else (
          let fk_list =
            List.map
              (fun (lcs, pt, pcs, od, ou, def) ->
                 Cat.
                   { fk_local_cols = lcs
                   ; fk_parent_table = pt
                   ; fk_parent_cols = pcs
                   ; fk_on_delete = od
                   ; fk_on_update = ou
                   ; fk_deferrable = def
                   })
              fk_constraints
          in
          let* () = Cat.save_fk_constraints ~txn:tx cat ~table_name:name ~fks:fk_list in
          (* Raw, undo-free cache update by design — reverted on ROLLBACK by
             [create_table]'s schema-cache undo, which removes the whole table
             entry.  See [Cat.set_fk_constraints]. *)
          Cat.set_fk_constraints cat ~table_name:name ~fks:fk_list;
          Lwt.return_unit)
      in
      Lwt.return 0)
;;

(* Op_insert: insert each VALUES row, counting successful inserts. *)
let execute_insert_values
      store
      (cat : Cat.t)
      ~mode
      ~params
      ~clock
      ~before_hook
      ~after_hook
      ~on_replace_delete_before
      ~on_replace_delete
      ~on_upsert_update_before
      ~on_upsert_update
      ~table_meta
      ~ordinals
      ~values
      ~on_conflict
      ~upsert_update
  : int Lwt.t
  =
  let bh =
    Option.map
      (fun f ~tx ~new_row -> f ~tx ~new_row:(Some new_row) ~old_row:None)
      before_hook
  in
  let ah =
    Option.map
      (fun f ~tx ~new_row -> f ~tx ~new_row:(Some new_row) ~old_row:None)
      after_hook
  in
  Lwt_list.fold_left_s
    (fun count row_vals ->
       let* inserted =
         execute_insert
           ~mode
           ~params
           ~clock
           ~on_conflict
           ~upsert_update
           ~before_hook:bh
           ~after_hook:ah
           ~on_replace_delete_before
           ~on_replace_delete
           ~on_upsert_update_before
           ~on_upsert_update
           store
           cat
           ~table_meta
           ~ordinals
           ~values:row_vals
       in
       Lwt.return (count + if inserted then 1 else 0))
    0
    values
;;

(* Op_insert_select: insert one row per source-stream row. *)
let execute_insert_select_op
      store
      (cat : Cat.t)
      ~mode
      ~params
      ~clock
      ~before_hook
      ~after_hook
      ~on_replace_delete_before
      ~on_replace_delete
      ~on_upsert_update_before
      ~on_upsert_update
      ~(table_meta : Cat.table_meta)
      ~ordinals
      ~source
      ~on_conflict
      ~upsert_update
  : int Lwt.t
  =
  let n_cols = List.length table_meta.Cat.columns in
  let bh =
    Option.map
      (fun f ~tx ~new_row -> f ~tx ~new_row:(Some new_row) ~old_row:None)
      before_hook
  in
  let ah =
    Option.map
      (fun f ~tx ~new_row -> f ~tx ~new_row:(Some new_row) ~old_row:None)
      after_hook
  in
  let* stream = !to_stream_ref clock params store ~mode ~cat:(Some cat) source in
  let* src_rows = Lwt_stream.to_list stream in
  Lwt_list.fold_left_s
    (fun count src_row ->
       let row_arr = Array.make n_cols Row.V_null in
       List.iteri
         (fun i ord -> if i < Array.length src_row then row_arr.(ord) <- src_row.(i))
         ordinals;
       (* #653: the SELECT form is the VALUES form one row at a time — the
          upsert clause is handed to the SAME [execute_insert], so #639's target
          pass, the rowid-alias pre-probe, the NOT NULL ordering and #667's
          pre-write uniqueness check all apply here by construction rather than
          by a second implementation agreeing with the first. *)
       let* inserted =
         execute_insert
           ~mode
           ~params
           ~clock
           ~on_conflict
           ~upsert_update
           ~before_hook:bh
           ~after_hook:ah
           ~on_replace_delete_before
           ~on_replace_delete
           ~on_upsert_update_before
           ~on_upsert_update
           store
           cat
           ~table_meta
           ~ordinals
           ~values:[]
           ~prebuilt_row:(Some row_arr)
       in
       Lwt.return (count + if inserted then 1 else 0))
    0
    src_rows
;;

(* Op_update dispatch: adapt the new/old-row hooks and delegate to execute_update. *)
let execute_update_op
      store
      cat
      ~mode
      ~params
      ~clock
      ~before_hook
      ~after_hook
      ~table_meta
      ~assignments
      ~where
      ~seek
      ~order
      ~limit
      ~offset
      ~indexes
  : int Lwt.t
  =
  let bh =
    Option.map
      (fun f ~tx ~old_row ~new_row ->
         f ~tx ~new_row:(Some new_row) ~old_row:(Some old_row))
      before_hook
  in
  let ah =
    Option.map
      (fun f ~tx ~old_row ~new_row ->
         f ~tx ~new_row:(Some new_row) ~old_row:(Some old_row))
      after_hook
  in
  execute_update
    ~mode
    ~params
    ~clock
    ~before_hook:bh
    ~after_hook:ah
    store
    cat
    ~table_meta
    ~assignments
    ~where
    ~seek
    ~order
    ~limit
    ~offset
    ~indexes
;;

(* Op_delete dispatch: adapt the old-row hooks and delegate to execute_delete. *)
let execute_delete_op
      store
      cat
      ~mode
      ~params
      ~clock
      ~before_hook
      ~after_hook
      ~table_meta
      ~where
      ~seek
      ~order
      ~limit
      ~offset
      ~indexes
  : int Lwt.t
  =
  let bh =
    Option.map
      (fun f ~tx ~old_row -> f ~tx ~new_row:None ~old_row:(Some old_row))
      before_hook
  in
  let ah =
    Option.map
      (fun f ~tx ~old_row -> f ~tx ~new_row:None ~old_row:(Some old_row))
      after_hook
  in
  execute_delete
    ~mode
    ~params
    ~clock
    ~before_hook:bh
    ~after_hook:ah
    store
    cat
    ~table_meta
    ~where
    ~seek
    ~order
    ~limit
    ~offset
    ~indexes
;;

(* Op_drop_table: drop the table and invalidate its cached CHECK / generated
   expressions. *)
let execute_drop_table_op
      store
      (cat : Cat.t)
      ~mode
      ~(table_meta : Cat.table_meta)
      ~indexes
  : int Lwt.t
  =
  let* () = execute_drop_table ~mode store cat ~table_meta ~_indexes:indexes in
  Hashtbl.filter_map_inplace
    (fun (tbl, _, _) v -> if String.equal tbl table_meta.name then None else Some v)
    check_expr_cache;
  Hashtbl.filter_map_inplace
    (fun (tbl, _, _) v -> if String.equal tbl table_meta.name then None else Some v)
    generated_expr_cache;
  Lwt.return 0
;;

(* Op_fts_insert: allocate a rowid, store the content row, and index it. *)
let execute_fts_insert
      store
      (cat : Cat.t)
      ~mode
      ~clock
      ~params
      (fts_meta : Cat.fts_table_meta)
      ~col_names
      ~col_values
      ~rowid_value
  : int Lwt.t
  =
  let* tx, owned = acquire_txn store mode in
  Lwt.catch
    (fun () ->
       (* #330: an explicit [rowid] is used verbatim (and the high-water advanced
          past it so a later auto-insert never reuses it); otherwise allocate the
          next rowid as before. *)
       let* rowid =
         match rowid_value with
         | None -> Cat.next_fts_rowid_in_txn cat ~name:fts_meta.Cat.fts_name tx
         | Some e ->
           let rowid =
             match eval_expr clock params [||] e with
             | Row.V_int n -> n
             | Row.V_real f -> Int64.of_float f
             | _ -> raise (Failure "FTS rowid must be an integer")
           in
           let* () =
             Cat.ensure_fts_rowid_above_in_txn cat ~name:fts_meta.Cat.fts_name tx rowid
           in
           Lwt.return rowid
       in
       let key = Rowid.encode rowid in
       (* #330: if a row already exists at this rowid (explicit-rowid collision),
          de-index it first so its index entries are not left stale. *)
       let* () =
         match rowid_value with
         | None -> Lwt.return_unit
         | Some _ ->
           let* existing = S.get tx fts_meta.Cat.fts_content_tree key in
           (match existing with
            | None -> Lwt.return_unit
            | Some old_bytes ->
              let old_texts = fts_decode_content old_bytes in
              let old_col_texts = List.mapi (fun i t -> i, t) old_texts in
              fts_deindex_document tx ~fts_meta ~rowid ~col_texts:old_col_texts)
       in
       let vals = List.map (fun e -> eval_expr clock params [||] e) col_values in
       let n_cols = List.length fts_meta.Cat.fts_columns in
       let texts = Array.make n_cols "" in
       List.iter2
         (fun col_name v ->
            match list_find_index (String.equal col_name) fts_meta.Cat.fts_columns with
            | None -> ()
            | Some (i, _) ->
              texts.(i)
              <- (match v with
                  | Row.V_text s -> s
                  | _ -> ""))
         col_names
         vals;
       let text_list = Array.to_list texts in
       let* () =
         S.put tx fts_meta.Cat.fts_content_tree key (fts_encode_content text_list)
       in
       let col_texts = List.mapi (fun i t -> i, t) text_list in
       let* () = fts_index_document tx ~fts_meta ~rowid ~col_texts in
       let* () = release_txn ~cat tx owned in
       mark_dirty fts_meta.Cat.fts_name;
       Lwt.return 1)
    (fun exn ->
       let* () = if owned then S.rollback tx else Lwt.return_unit in
       Lwt.fail exn)
;;

(* Op_fts_delete: drain matching content rows, then delete + de-index them. *)
let execute_fts_delete
      store
      (cat : Cat.t)
      ~mode
      ~clock
      ~params
      (fts_meta : Cat.fts_table_meta)
      ~where
  : int Lwt.t
  =
  ignore cat;
  let* matches =
    S.with_ro store
    @@ fun tx_ro ->
    let* cur = S.cursor_open tx_ro fts_meta.Cat.fts_content_tree in
    let _sr = S.cursor_first cur in
    let buf = ref [] in
    let rec drain () =
      match S.cursor_next cur with
      | None -> ()
      | Some (kbytes, vbytes) ->
        let rowid = Rowid.decode kbytes in
        let texts = fts_decode_content vbytes in
        let row = Array.of_list (List.map (fun s -> Row.V_text s) texts) in
        let keep =
          match where with
          | None -> true
          | Some pred -> value_truthy (eval_expr clock params row pred)
        in
        if keep then buf := (rowid, kbytes, texts) :: !buf;
        drain ()
    in
    drain ();
    S.cursor_close cur;
    Lwt.return (List.rev !buf)
  in
  let n = List.length matches in
  if n = 0
  then Lwt.return 0
  else
    let* tx, owned = acquire_txn store mode in
    Lwt.catch
      (fun () ->
         let* () =
           Lwt_list.iter_s
             (fun (rowid, key, texts) ->
                let col_texts = List.mapi (fun i t -> i, t) texts in
                let* () = S.del tx fts_meta.Cat.fts_content_tree key in
                fts_deindex_document tx ~fts_meta ~rowid ~col_texts)
             matches
         in
         let* () = release_txn ~cat tx owned in
         (* [n > 0] here (the [n = 0] case returned early above). *)
         mark_dirty fts_meta.Cat.fts_name;
         Lwt.return n)
      (fun exn ->
         let* () = if owned then S.rollback tx else Lwt.return_unit in
         Lwt.fail exn)
;;

(* Drop cached CHECK and generated-column expressions for [table_name]
   (used after DROP COLUMN, which can invalidate them). *)
let clear_table_expr_caches table_name =
  let clear cache =
    let to_clear =
      Hashtbl.fold
        (fun (tn, idx, sql) _ acc ->
           if String.equal tn table_name then (tn, idx, sql) :: acc else acc)
        cache
        []
    in
    List.iter (Hashtbl.remove cache) to_clear
  in
  clear check_expr_cache;
  clear generated_expr_cache
;;

(* Convert an AST column definition into a catalog [Row.column].

   #530/#533: [not_null] is derived as [not_null || primary_key], the same
   derivation [Sema.column_of_def] makes on the CREATE TABLE path — the two are
   siblings and had drifted, so an ALTER-added PRIMARY KEY column came out
   nullable.  [Sema.bind_add_column] now refuses to add a PRIMARY KEY column at
   all, so this is belt-and-braces; it is kept so the two converters cannot
   disagree again if that guard ever moves. *)
let column_of_col_def col_def : Row.column =
  { Row.name = col_def.Ast.name
  ; Row.ty =
      (match col_def.Ast.ty with
       | Ast.Ty_int -> Row.Integer
       | Ast.Ty_text -> Row.Text
       | Ast.Ty_real -> Row.Real
       | Ast.Ty_blob -> Row.Blob)
  ; Row.not_null = col_def.Ast.not_null || col_def.Ast.primary_key
  ; Row.primary_key = col_def.Ast.primary_key
  ; Row.pk_desc = col_def.Ast.pk_desc
  ; Row.default =
      (match col_def.Ast.default with
       | None -> None
       | Some Ast.L_null -> Some Row.DV_null
       | Some (Ast.L_int n) -> Some (Row.DV_int n)
       | Some (Ast.L_text s) -> Some (Row.DV_text s)
       | Some (Ast.L_real f) -> Some (Row.DV_real f)
       | Some (Ast.L_blob b) -> Some (Row.DV_blob b)
       | Some Ast.L_current_timestamp -> Some Row.DV_current_timestamp
       | Some Ast.L_current_date -> Some Row.DV_current_date
       | Some Ast.L_current_time -> Some Row.DV_current_time)
  ; Row.check_sql = Option.map Ast.expr_to_sql col_def.Ast.check
  ; Row.generated_as =
      Option.map (fun (e, s) -> Ast.expr_to_sql e, s = `Stored) col_def.Ast.generated_as
  }
;;

(* ALTER TABLE ADD COLUMN: add [col_def] to the catalog and persist any inline
   FK reference it declares. *)
let alter_add_column ?txn (cat : Cat.t) ~(table_meta : Cat.table_meta) col_def : int Lwt.t
  =
  let col = column_of_col_def col_def in
  let* result = Cat.add_column ?txn cat ~table_name:table_meta.Cat.name ~column:col in
  match result with
  | Error msg -> Lwt.fail_with msg
  | Ok () ->
    (match col_def.Ast.fk_ref with
     | None -> Lwt.return 0
     | Some (parent_table, parent_col, ast_od, ast_ou, ast_def) ->
       let inferred_parent_col =
         if parent_col = ""
         then (
           match Cat.find_table_cached cat ~name:parent_table with
           | None -> parent_col
           | Some pm ->
             (* #533: the exactly-one question, like every other parent-PK
                inference site — [List.find_opt] would silently infer a
                single-column reference to the FIRST column of a composite key.
                Unreachable while [Sema.bind_add_column] refuses first, but the
                two must not be able to disagree. *)
             (match Sema.sole_pk_column pm.Cat.columns with
              | None -> parent_col
              | Some pk -> pk.Row.name))
         else parent_col
       in
       let new_fk : Cat.fk_constraint =
         { Cat.fk_local_cols = [ col_def.Ast.name ]
         ; Cat.fk_parent_table = parent_table
         ; Cat.fk_parent_cols = [ inferred_parent_col ]
         ; Cat.fk_on_delete = ast_od
         ; Cat.fk_on_update = ast_ou
         ; Cat.fk_deferrable = ast_def
         }
       in
       let existing_fks =
         match Cat.find_table_cached cat ~name:table_meta.Cat.name with
         | None -> []
         | Some m -> m.Cat.fk_constraints
       in
       let new_fks = existing_fks @ [ new_fk ] in
       let* () =
         Cat.save_fk_constraints ?txn cat ~table_name:table_meta.Cat.name ~fks:new_fks
       in
       (* The in-memory FK mutation is reverted on ROLLBACK by [add_column]'s
          schema-cache undo, which restores the whole prior [table_meta]. *)
       Cat.set_fk_constraints cat ~table_name:table_meta.Cat.name ~fks:new_fks;
       Lwt.return 0)
;;

(* ALTER TABLE DROP COLUMN: drop dependent indexes, migrate rows to the new
   shape, drop the catalog column, and invalidate cached expressions.

   #282: everything runs through the single writer transaction [tx] supplied by
   [with_ddl_txn] (borrowed from the ambient explicit transaction, or owned in
   autocommit) — no longer three separate [rw_begin]/[with_ro] phases (which
   would self-deadlock inside an explicit transaction).  The row scan reads
   THROUGH [tx] so rows inserted earlier in the same transaction are migrated
   (read-your-own-writes).  Dropped dependent indexes register a schema-cache
   undo so a [ROLLBACK] restores them. *)
let alter_drop_column tx (cat : Cat.t) ~(table_meta : Cat.table_meta) col_name : int Lwt.t
  =
  let table_name = table_meta.Cat.name in
  let col_idx = find_col_idx_by_name table_meta.Cat.columns col_name in
  let new_columns = List.filteri (fun i _ -> i <> col_idx) table_meta.Cat.columns in
  let idxs_on_col =
    List.filter
      (fun (idx : Cat.index_info) -> List.mem col_name idx.Cat.idx_columns)
      (Cat.indexes_for_table cat ~table:table_name)
  in
  let* () =
    Lwt_list.iter_s
      (fun (idx : Cat.index_info) -> Cat.drop_index cat tx ~name:idx.idx_name)
      idxs_on_col
  in
  (* #283: each [Cat.drop_index] above self-registers its own cache undo, so the
     dropped dependent indexes are restored on ROLLBACK without an external block. *)
  (* #533: dropping a member of a composite PRIMARY KEY drops the `Implicit_pk
     index with it, so the key stops being enforced — but the SURVIVING members
     kept their [primary_key] flag.  Since #530 that flag is what the DDL
     renderer reads, so [Db.dump] emitted e.g. [k TEXT NOT NULL PRIMARY KEY] for
     a table that accepts duplicate [k] and already holds them: the dump would
     not restore.  Clear the flag on the survivors before the column goes, the
     exact inverse of the [Catalog.open_] re-derivation.  Runs through [tx], so
     ROLLBACK reverts it with everything else. *)
  let* () =
    let surviving =
      List.filter_map
        (fun (idx : Cat.index_info) ->
           match idx.Cat.idx_origin with
           | `Implicit_pk ->
             Some
               (List.filter (fun c -> not (String.equal c col_name)) idx.Cat.idx_columns)
           | `Implicit_unique | `User -> None)
        idxs_on_col
      |> List.concat
    in
    if surviving = []
    then Lwt.return_unit
    else
      let* r = Cat.clear_pk_flags ~txn:tx cat ~table_name ~cols:surviving in
      match r with
      | Error msg -> Lwt.fail_with msg
      | Ok () -> Lwt.return_unit
  in
  (* [Cat.drop_column] below re-reads the table from the schema cache, so it
     sees the cleared flags. *)
  (* Drain every row through [tx] (read-your-own-writes) before rewriting, so the
     cursor is closed before we put back the reshaped rows into the same tree. *)
  let alt_tree_id, _, _, _ = Cat.row_storage table_meta in
  let* cur = S.cursor_open tx alt_tree_id in
  let _sr = S.cursor_first cur in
  let rows = ref [] in
  let rec drain () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      let old_row = decode_with_virtual None [||] table_meta v in
      let new_row =
        Array.of_list (List.filteri (fun i _ -> i <> col_idx) (Array.to_list old_row))
      in
      rows := (Bytes.copy k, new_row) :: !rows;
      drain ()
  in
  drain ();
  S.cursor_close cur;
  let* () =
    Lwt_list.iter_s
      (fun (k, new_row) ->
         let new_bytes = Row.encode new_columns new_row in
         S.put tx alt_tree_id k new_bytes)
      !rows
  in
  let* result = Cat.drop_column ~txn:tx cat ~table_name ~col_name in
  match result with
  | Error msg -> Lwt.fail_with msg
  | Ok () ->
    clear_table_expr_caches table_name;
    Lwt.return 0
;;

(* ALTER TABLE RENAME TABLE: rename in the catalog and remap cached CHECK /
   generated-column entries from the old name to the new one. *)
let alter_rename_table ?txn (cat : Cat.t) ~(table_meta : Cat.table_meta) new_name
  : int Lwt.t
  =
  let* result = Cat.rename_table ?txn cat ~old_name:table_meta.Cat.name ~new_name in
  match result with
  | Error msg -> Lwt.fail_with msg
  | Ok () ->
    let remap tbl_cache =
      let to_add =
        Hashtbl.fold
          (fun (tbl, idx, sql) v acc ->
             if String.equal tbl table_meta.Cat.name
             then (new_name, idx, sql, v) :: acc
             else acc)
          tbl_cache
          []
      in
      List.iter
        (fun (_, idx, sql, _) -> Hashtbl.remove tbl_cache (table_meta.Cat.name, idx, sql))
        to_add;
      List.iter
        (fun (new_t, idx, sql, v) -> Hashtbl.add tbl_cache (new_t, idx, sql) v)
        to_add
    in
    remap check_expr_cache;
    remap generated_expr_cache;
    Lwt.return 0
;;

(** #765 review round 3: refuse a schema mutation rather than chase it on the
    recheck side — closing the whole "FK constraint/column identity captured
    at enqueue time desyncs from the catalog by the time a deferred recheck
    runs" class at its source, matching this project's own conservative
    precedent for a structurally identical problem ([ALTER TABLE ... RENAME]
    refusing outright when a view or trigger depends on the table, rather
    than trying to keep the dependent artifact consistent through the
    rename — #673/#645).

    Rounds 1 and 2 fixed this on the RECHECK side, twice, for two different
    mutation shapes (a stale ordinal under [DROP COLUMN]; a stale NAME under
    [RENAME COLUMN]) — each fix closed exactly the shape that prompted it and
    no other, and round 3's [DROP COLUMN] + [ADD COLUMN] of the identical
    name is a third. Fixing the recheck side can never get ahead of a new
    mutation shape, because the recheck only ever sees the schema AFTER the
    mutation; refusing the mutation WHILE a pending obligation still needs
    the column removes the race instead of predicting its next shape.

    [table_name]/[col_name] are the ALTER's own target column. Returns
    [Some conflict_message] if some pending check's FK constraint — resolved
    FRESH right now via {!Cat.peek_pending_fk_checks} and its
    [pfk_fk_ordinal}, exactly as {!make_fk_recheck} resolves it at COMMIT —
    still names [col_name] as one of its LOCAL columns on [table_name] (the
    check's child side) or one of its PARENT columns on [table_name] (the
    check's parent side); [None] if the column is free to mutate.

    Deliberately narrow: only [RENAME COLUMN] and [DROP COLUMN] call this.
    [ADD COLUMN] needs no check of its own — the only way it could
    reintroduce a column a pending obligation cares about is if an earlier
    statement in the SAME transaction dropped that very column, and that
    drop is what this function refuses; there is no live conflict left for
    [ADD COLUMN] to walk into. A column dropped in an EARLIER, already
    committed transaction has no pending check left to conflict with either
    (the queue is drained/cleared at every COMMIT/ROLLBACK) — that scenario
    is #767's standalone, non-transactional [drop_column] corruption, a
    pre-existing and separately-filed gap this function does not attempt to
    close.

    Residual NOT closed by this check, named explicitly rather than left
    implicit: [ALTER TABLE ... RENAME TO] (renaming the WHOLE table) and
    [DROP TABLE] can still desync a pending check by table name the same
    way — [make_fk_recheck]'s [Cat.find_table_cached] returning [None]
    reports "not violated", the same silent-wrong-answer shape the review's
    RENAME COLUMN finding had for a column. Out of round 3's stated scope
    (RENAME COLUMN / DROP COLUMN / ADD COLUMN); worth a future issue if it
    proves reachable — filing it is simpler than a new mid-flight guard, and
    matches how #767 was itself scoped out of this same PR. *)
let fk_obligation_conflict (cat : Cat.t) ~table_name ~col_name : string option =
  let check_conflict (chk : Cat.pending_fk_check) =
    if chk.Cat.pfk_fk_ordinal < 0
    then
      (* #765 review round 4, item 2: [List.nth_opt] raises
         [Invalid_argument] for a NEGATIVE index rather than answering
         [None] — it only degrades gracefully for an out-of-range POSITIVE
         one (the same trap {!make_fk_recheck} guards against explicitly,
         reopened here in the new function round 3 added). The [-1]
         sentinel means [Exec.fk_ordinal] could not find the constraint at
         enqueue time — this pending check can never be resolved either, so
         treat it as "no conflict from this entry" rather than crashing an
         unrelated ALTER that merely shares a transaction with it. *)
      None
    else (
      match Cat.find_table_cached cat ~name:chk.Cat.pfk_child_table with
      | None -> None
      | Some child_now ->
        (match List.nth_opt child_now.Cat.fk_constraints chk.Cat.pfk_fk_ordinal with
         | None -> None
         | Some fk_now ->
           if
             String.equal chk.Cat.pfk_child_table table_name
             && List.mem col_name fk_now.Cat.fk_local_cols
           then
             Some
               (Printf.sprintf
                  "column '%s.%s' is the local side of a FOREIGN KEY referencing '%s' \
                   with a deferred check still pending in this transaction"
                  table_name
                  col_name
                  fk_now.Cat.fk_parent_table)
           else if
             String.equal fk_now.Cat.fk_parent_table table_name
             && List.mem col_name fk_now.Cat.fk_parent_cols
           then
             Some
               (Printf.sprintf
                  "column '%s.%s' is referenced by a FOREIGN KEY on '%s' with a deferred \
                   check still pending in this transaction"
                  table_name
                  col_name
                  chk.Cat.pfk_child_table)
           else None))
  in
  List.find_map check_conflict (Cat.peek_pending_fk_checks cat)
;;

(* #405: which user table names an ALTER makes stale for a name-keyed external
   read cache.  Every ALTER form this engine has changes the table's OBSERVABLE
   contents, so all four mark:

   - [DROP COLUMN] physically rewrites every stored row (see [alter_drop_column]);
   - [ADD COLUMN] leaves the row bytes alone, but widens every row a reader sees,
     so a cached [SELECT] result has the wrong arity - the same failure mode;
   - [RENAME COLUMN] changes the names a result set is keyed by;
   - [RENAME TABLE] invalidates the OLD name (queries against it now fail) AND
     the new one (which now answers with rows it did not have before), so both
     are marked.

   The marks are deliberately over-approximate, like the rest of this
   accumulator (#666): a superfluous invalidation costs one re-read, a missing
   one serves a wrong answer from cache. *)
let mark_alter_dirty ~(table_meta : Cat.table_meta) = function
  | Ast.AA_rename_table new_name ->
    mark_dirty table_meta.Cat.name;
    mark_dirty new_name
  | Ast.AA_add_column _ | Ast.AA_rename_column _ | Ast.AA_drop_column _ ->
    mark_dirty table_meta.Cat.name
;;

(* Op_alter_table: dispatch on the ALTER action.

   #282: the ALTER mutators run through [with_ddl_txn], which supplies a single
   writer transaction — borrowed from the ambient explicit transaction
   ([In_txn]) or owned and auto-committed ([Auto]).  Each catalog mutator threads
   that txn (no nested [rw_begin], which previously self-deadlocked inside
   [BEGIN…COMMIT]) and registers a schema-cache undo so a [ROLLBACK] reverts the
   in-memory catalog along with the store. *)
let execute_alter_table store (cat : Cat.t) ~mode ~(table_meta : Cat.table_meta) action
  : int Lwt.t
  =
  let* n =
    with_ddl_txn store cat mode (fun tx ->
      match action with
      | Ast.AA_add_column col_def -> alter_add_column ~txn:tx cat ~table_meta col_def
      | Ast.AA_rename_table new_name ->
        alter_rename_table ~txn:tx cat ~table_meta new_name
      | Ast.AA_rename_column (old_col, new_col) ->
        (match
           fk_obligation_conflict cat ~table_name:table_meta.Cat.name ~col_name:old_col
         with
         | Some conflict ->
           Lwt.fail_with
             (Printf.sprintf
                "cannot rename column %s.%s: %s"
                table_meta.Cat.name
                old_col
                conflict)
         | None ->
           let* result =
             Cat.rename_column
               ~txn:tx
               cat
               ~table_name:table_meta.Cat.name
               ~old_col
               ~new_col
           in
           (match result with
            | Error msg -> Lwt.fail_with msg
            | Ok () ->
              (* #553: the rename rewrites the CHECK / GENERATED expression SQL
                 of this table, and those caches are keyed by that SQL text — a
                 stale entry would keep a compiled expression resolved against
                 the old column list.  Same reasoning as [alter_drop_column]. *)
              clear_table_expr_caches table_meta.Cat.name;
              Lwt.return 0))
      | Ast.AA_drop_column col_name ->
        (match fk_obligation_conflict cat ~table_name:table_meta.Cat.name ~col_name with
         | Some conflict ->
           Lwt.fail_with
             (Printf.sprintf
                "cannot drop column %s.%s: %s"
                table_meta.Cat.name
                col_name
                conflict)
         | None -> alter_drop_column tx cat ~table_meta col_name))
  in
  mark_alter_dirty ~table_meta action;
  Lwt.return n
;;

(* Op_create_index: create the index unless IF NOT EXISTS finds it present. *)
let execute_create_index_op
      store
      (cat : Cat.t)
      ~mode
      ~name
      ~table
      ~tree_id
      ~col_sqls
      ~col_expr_flags
      ~where_expr
      ~where_sql
      ~unique
      ~columns
      ~if_not_exists
  : int Lwt.t
  =
  if if_not_exists && Cat.index_exists cat ~name
  then Lwt.return 0
  else
    let* () =
      execute_create_index
        ~mode
        store
        cat
        ~name
        ~table
        ~tree_id
        ~col_sqls
        ~col_expr_flags
        ~where_expr
        ~where_sql
        ~unique
        ~columns
    in
    Lwt.return 0
;;

(** [execute_with_count] returns the rows-affected count.  For most
    write ops this is 1 (INSERT) or 0 (DDL); for UPDATE it is the
    number of rows whose contents were modified. *)
let execute_with_count
      ?(mode = Auto)
      ?(clock : (unit -> float) option = None)
      ?(params = [||])
      ?(before_hook :
          (tx:S.rw S.txn -> new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t)
            option =
        None)
      ?(after_hook :
          (tx:S.rw S.txn -> new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t)
            option =
        None)
      ?(on_replace_delete_before : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option =
        None)
      ?(on_replace_delete : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option = None)
      ?(on_upsert_update_before :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      ?(on_upsert_update :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      (store : S.t)
      (cat : Cat.t)
      (op : Plan.op)
  : int Lwt.t
  =
  match op with
  | Plan.Op_create_table
      { name
      ; columns
      ; uniq_idxs
      ; if_not_exists
      ; fk_constraints
      ; without_rowid
      ; autoincrement
      } ->
    execute_create_table_op
      store
      cat
      ~mode
      ~name
      ~columns
      ~uniq_idxs
      ~if_not_exists
      ~fk_constraints
      ~without_rowid
      ~autoincrement
  | Plan.Op_col_create_table { name; columns; if_not_exists } ->
    if Cat.table_exists cat ~name
    then
      if if_not_exists
      then Lwt.return 0
      else failwith (Printf.sprintf "table '%s' already exists" name)
    else
      let* () =
        with_ddl_txn store cat mode (fun tx ->
          Cat.create_columnstore_table ~txn:tx cat ~name ~columns)
      in
      Lwt.return 0
  | Plan.Op_insert
      { table_meta; ordinals; values; on_conflict; returning = _; upsert_update = _ }
    when Cat.is_columnar table_meta ->
    let* tx, owned = acquire_txn store mode in
    Lwt.catch
      (fun () ->
         let col_store = col_store_of_meta table_meta in
         let n_cols = List.length table_meta.Cat.columns in
         let rows =
           List.filter_map
             (fun vals ->
                let row = Array.make n_cols Row.V_null in
                List.iter2
                  (fun ord expr -> row.(ord) <- eval_expr clock params [||] expr)
                  ordinals
                  vals;
                (* #567: a columnstore INSERT never reaches [Row.encode] — it
                   hands the row array straight to [Col_store.insert_rows] — so
                   it needs the same check the row-store path gets before its
                   encode.  Without it the bound-parameter spelling the issue
                   calls out as the one that matters in practice wrote NULL
                   into a NOT NULL column here.  #599: [OR IGNORE] drops the
                   offending row here too, so the modifier means the same thing
                   on both storage engines. *)
                if not_null_skip_or_fail ~clock ~params table_meta row ~on_conflict
                then None
                else Some row)
             values
         in
         Granary_columnar.Col_store.insert_rows col_store (Array.of_list rows);
         let* () = release_txn ~cat tx owned in
         if rows <> [] then mark_dirty table_meta.Cat.name;
         Lwt.return (List.length rows))
      (fun exn ->
         let* () = if owned then S.rollback tx else Lwt.return_unit in
         Lwt.fail exn)
  | Plan.Op_insert
      { table_meta; ordinals; values; on_conflict; returning = _; upsert_update } ->
    execute_insert_values
      store
      cat
      ~mode
      ~params
      ~clock
      ~before_hook
      ~after_hook
      ~on_replace_delete_before
      ~on_replace_delete
      ~on_upsert_update_before
      ~on_upsert_update
      ~table_meta
      ~ordinals
      ~values
      ~on_conflict
      ~upsert_update
  | Plan.Op_insert_select { table_meta; ordinals; source; on_conflict; upsert_update = _ }
    when Cat.is_columnar table_meta ->
    (* #653: [upsert_update] is [None] here by construction —
       [Sema.bind_upsert_clause] refuses an ON CONFLICT ... DO UPDATE on a
       COLUMNSTORE table, because the columnar write path probes for no conflict
       and the clause could only ever be dropped. *)
    let col_store = col_store_of_meta table_meta in
    let n_cols = List.length table_meta.Cat.columns in
    let* stream = !to_stream_ref clock params store ~mode ~cat:(Some cat) source in
    let* src_rows = Lwt_stream.to_list stream in
    let batch =
      Array.of_list
        (List.filter_map
           (fun src_row ->
              let dest = Array.make n_cols Row.V_null in
              List.iteri (fun i ord -> dest.(ord) <- src_row.(i)) ordinals;
              (* #567: same as the columnar VALUES path above — no encode, so
                 the check has to be here.  Raised before the txn is acquired,
                 so nothing has been written when it fires.  #599: under
                 [OR IGNORE] the row is dropped instead. *)
              if not_null_skip_or_fail ~clock ~params table_meta dest ~on_conflict
              then None
              else Some dest)
           src_rows)
    in
    let* tx, owned = acquire_txn store mode in
    Lwt.catch
      (fun () ->
         Granary_columnar.Col_store.insert_rows col_store batch;
         let* () = release_txn ~cat tx owned in
         if Array.length batch > 0 then mark_dirty table_meta.Cat.name;
         Lwt.return (Array.length batch))
      (fun exn ->
         let* () = if owned then S.rollback tx else Lwt.return_unit in
         Lwt.fail exn)
  | Plan.Op_insert_select { table_meta; ordinals; source; on_conflict; upsert_update } ->
    execute_insert_select_op
      store
      cat
      ~mode
      ~params
      ~clock
      ~before_hook
      ~after_hook
      ~on_replace_delete_before
      ~on_replace_delete
      ~on_upsert_update_before
      ~on_upsert_update
      ~table_meta
      ~ordinals
      ~source
      ~on_conflict
      ~upsert_update
  | Plan.Op_create_index
      { name
      ; table
      ; tree_id
      ; col_sqls
      ; col_expr_flags
      ; where_expr
      ; where_sql
      ; unique
      ; columns
      ; if_not_exists
      } ->
    execute_create_index_op
      store
      cat
      ~mode
      ~name
      ~table
      ~tree_id
      ~col_sqls
      ~col_expr_flags
      ~where_expr
      ~where_sql
      ~unique
      ~columns
      ~if_not_exists
  | Plan.Op_update
      { table_meta
      ; assignments
      ; where
      ; seek
      ; order
      ; limit
      ; offset
      ; indexes
      ; returning = _
      } ->
    if Cat.is_columnar table_meta
    then
      Lwt.fail_with
        (Printf.sprintf
           "UPDATE is not supported on columnar table '%s'"
           table_meta.Cat.name)
    else
      execute_update_op
        store
        cat
        ~mode
        ~params
        ~clock
        ~before_hook
        ~after_hook
        ~table_meta
        ~assignments
        ~where
        ~seek
        ~order
        ~limit
        ~offset
        ~indexes
  | Plan.Op_delete
      { table_meta; where; seek; order; limit; offset; indexes; returning = _ } ->
    if Cat.is_columnar table_meta
    then
      Lwt.fail_with
        (Printf.sprintf
           "DELETE is not supported on columnar table '%s'"
           table_meta.Cat.name)
    else
      execute_delete_op
        store
        cat
        ~mode
        ~params
        ~clock
        ~before_hook
        ~after_hook
        ~table_meta
        ~where
        ~seek
        ~order
        ~limit
        ~offset
        ~indexes
  | Plan.Op_seq_set { table; seq } ->
    (* #312.1: writable sqlite_sequence SET/INSERT.  Runs through [with_ddl_txn]
       so it participates in any ambient explicit transaction (borrowed [In_txn]
       — reverts on ROLLBACK) or owns its own auto-committed txn ([Auto]). *)
    with_ddl_txn store cat mode (fun tx ->
      let* () = Cat.set_next_rowid_in_txn cat ~name:table ~requested:seq tx in
      (* #317.3: the UPDATE/INSERT form touches exactly one sqlite_sequence row,
         so [changes()] reports 1 — SQLite parity.  (The DELETE/reset form below
         still reports 0: we do not materialise per-table rows to count.) *)
      Lwt.return 1)
  | Plan.Op_seq_reset { table } ->
    (* #312.1: writable sqlite_sequence DELETE.  [None] (bare DELETE, no WHERE)
       resets every AUTOINCREMENT counter — SQLite parity, and what a real
       [sqlite3 .dump] emits before re-INSERTing. *)
    with_ddl_txn store cat mode (fun tx ->
      let* () =
        match table with
        | Some name -> Cat.reset_next_rowid_in_txn cat ~name tx
        | None -> Cat.reset_all_next_rowid_in_txn cat tx
      in
      Lwt.return 0)
  | Plan.Op_drop_table { table_meta; indexes } ->
    execute_drop_table_op store cat ~mode ~table_meta ~indexes
  | Plan.Op_drop_index { idx_info } ->
    let* () = execute_drop_index ~mode store cat ~idx_info in
    Lwt.return 0
  | Plan.Op_create_fts_table { name; columns } ->
    (* #269: thread any ambient explicit txn so CREATE VIRTUAL TABLE … USING fts5
       does not self-deadlock and rolls back atomically. *)
    with_ddl_txn store cat mode (fun tx ->
      let* _ = Cat.create_fts_table ~txn:tx cat ~name ~columns in
      Lwt.return 0)
  | Plan.Op_fts_insert { fts_meta; col_names; col_values; rowid_value } ->
    execute_fts_insert
      store
      cat
      ~mode
      ~clock
      ~params
      fts_meta
      ~col_names
      ~col_values
      ~rowid_value
  | Plan.Op_fts_delete { fts_meta; where } ->
    execute_fts_delete store cat ~mode ~clock ~params fts_meta ~where
  | Plan.Op_alter_table { table_meta; action } ->
    execute_alter_table store cat ~mode ~table_meta action
  | Plan.Op_begin
  | Plan.Op_commit
  | Plan.Op_rollback
  | Plan.Op_savepoint _
  | Plan.Op_release _
  | Plan.Op_rollback_to _ ->
    failwith
      "Exec.execute_with_count: BEGIN/COMMIT/ROLLBACK/SAVEPOINT handled by Db layer"
  | Plan.Op_pragma_rows _ -> Lwt.return 0
  | Plan.Op_pragma_set_user_version { version } ->
    let* tx = S.rw_begin store in
    let* () = Cat.write_user_version_tx tx version in
    let* () = S.commit tx in
    Lwt.return 0
  | Plan.Op_pragma_set_fk { on } ->
    Cat.set_fk_enforcement cat on;
    Lwt.return 0
  | Plan.Op_pragma_set_recursive_triggers { on } ->
    Cat.set_recursive_triggers cat on;
    Lwt.return 0
  | Plan.Op_pragma_set_defer_fk { on } ->
    Cat.set_defer_fks_pragma cat on;
    Lwt.return 0
  | Plan.Op_pragma_wal_checkpoint ->
    let* () = S.checkpoint store in
    Lwt.return 0
  | Plan.Op_pragma_set_wal_autocheckpoint { n } ->
    S.set_wal_autocheckpoint store (Int64.to_int n);
    Lwt.return 0
  | Plan.Op_pragma_set_synchronous { mode } ->
    if mode <> "full" && S.commit_callback_active store
    then
      failwith
        "PRAGMA synchronous: durability cannot be relaxed while a replication \
         commit-sink is active (replication requires synchronous=full)"
    else (
      let d =
        match mode with
        | "full" -> S.Full
        | "off" -> S.Off
        | "batched" ->
          S.Batched
            { commits = S.sync_batch_commits store
            ; interval_ms = S.sync_batch_interval_ms store
            }
        | _ -> failwith (Printf.sprintf "PRAGMA synchronous: unknown mode %s" mode)
      in
      let* () = if mode = "full" then S.flush_unsynced store else Lwt.return_unit in
      S.set_durability store d;
      Lwt.return 0)
  | Plan.Op_pragma_set_wal_batch_commits { n } ->
    S.set_sync_batch_commits store (Int64.to_int n);
    Lwt.return 0
  | Plan.Op_pragma_set_wal_batch_interval_ms { n } ->
    S.set_sync_batch_interval_ms store (Int64.to_int n);
    Lwt.return 0
  | Plan.Op_vacuum ->
    Lwt.fail_with "VACUUM must be executed via Db.execute / Db.vacuum (no Db handle)"
  (* #588: [PRAGMA not_null_repair] DELETEs rows, so it belongs on the WRITE
     path.  It used to be reachable only through [Exec.query] / [Db.query]: the
     natural call for a statement that mutates and returns no interesting rows
     — [Db.execute] — answered "use Exec.query for read operations" and deleted
     nothing, so a caller had to route a destructive operation through the read
     api to make it happen.  Both entry points now perform the repair, and they
     differ only in what they hand back: [Db.execute] reports the number of rows
     deleted as the statement's change count, [Db.query] streams the per-column
     (table, column, count) report.  Its read-only half, [PRAGMA
     not_null_check], stays on the query path alone. *)
  | Plan.Op_pragma_not_null_repair ->
    let* _rows, deleted = !not_null_repair_run_ref store mode cat in
    Lwt.return deleted
  | Plan.Op_attach _
  | Plan.Op_detach _
  | Plan.Op_database_list
  | Plan.Op_active_database_get
  | Plan.Op_active_database_set _ ->
    Lwt.fail_with
      "ATTACH/DETACH/database_list/active_database must be executed via Db.execute"
  | Plan.Op_create_view _
  | Plan.Op_create_reactive_view _
  | Plan.Op_drop_view _
  | Plan.Op_drop_reactive_view _
  | Plan.Op_create_trigger _
  | Plan.Op_drop_trigger _
  | Plan.Op_no_op -> Lwt.return 0
  | Plan.Op_explain _ -> Lwt.return 0
  | Plan.Op_union _
  | Plan.Op_intersect _
  | Plan.Op_except _
  | Plan.Op_const_select _
  | Plan.Op_with_cte _
  | Plan.Op_cte_scan _
  | Plan.Op_window _
  | Plan.Op_pragma_get_user_version
  | Plan.Op_pragma_integrity_check
  | Plan.Op_pragma_not_null_check
  | Plan.Op_pragma_get_fk
  | Plan.Op_pragma_get_recursive_triggers
  | Plan.Op_pragma_get_defer_fk
  | Plan.Op_pragma_get_wal_autocheckpoint
  | Plan.Op_changes
  | Plan.Op_last_insert_rowid
  | Plan.Op_total_changes -> failwith "Exec.execute: use Exec.query for read operations"
  | Plan.Op_seq_scan _
  | Plan.Op_col_seq_scan _
  | Plan.Op_filter _
  | Plan.Op_project _
  | Plan.Op_expr_project _
  | Plan.Op_sort _
  | Plan.Op_limit _
  | Plan.Op_index_lookup _
  | Plan.Op_rowid_lookup _
  | Plan.Op_nested_loop_join _
  | Plan.Op_hash_join _
  | Plan.Op_aggregate _
  | Plan.Op_fts_seq_scan _
  | Plan.Op_fts_match_scan _
  | Plan.Op_distinct _
  | Plan.Op_sqlite_master
  | Plan.Op_sqlite_sequence
  | Plan.Op_pragma_get_synchronous
  | Plan.Op_pragma_checkpoint_status
  | Plan.Op_pragma_wal_replay_check
  | Plan.Op_pragma_get_wal_batch_commits
  | Plan.Op_pragma_get_wal_batch_interval_ms ->
    failwith "Exec.execute: use Exec.query for read operations"
;;

(** Compatibility entry point: discards the rows-affected count. *)
let execute
      ?(mode = Auto)
      ?(clock : (unit -> float) option = None)
      ?(params = [||])
      ?(before_hook :
          (tx:S.rw S.txn -> new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t)
            option =
        None)
      ?(after_hook :
          (tx:S.rw S.txn -> new_row:Row.t option -> old_row:Row.t option -> unit Lwt.t)
            option =
        None)
      ?(on_replace_delete_before : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option =
        None)
      ?(on_replace_delete : (tx:S.rw S.txn -> old_row:Row.t -> unit Lwt.t) option = None)
      ?(on_upsert_update_before :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      ?(on_upsert_update :
          (tx:S.rw S.txn -> old_row:Row.t -> new_row:Row.t -> unit Lwt.t) option =
        None)
      (store : S.t)
      (cat : Cat.t)
      (op : Plan.op)
  : unit Lwt.t
  =
  let* _n =
    execute_with_count
      ~mode
      ~clock
      ~params
      ~before_hook
      ~after_hook
      ~on_replace_delete_before
      ~on_replace_delete
      ~on_upsert_update_before
      ~on_upsert_update
      store
      cat
      op
  in
  Lwt.return_unit
;;

(* ------------------------------------------------------------------ *)
(* BM25 scoring helpers                                                 *)
(* ------------------------------------------------------------------ *)

let bm25_score ~k1 ~b ~total_docs ~total_tokens ~n_docs_with_term ~term_freq ~doc_length =
  if total_docs = 0 || n_docs_with_term = 0
  then 0.0
  else (
    let n = Float.of_int total_docs in
    let n_t = Float.of_int n_docs_with_term in
    let tf = Float.of_int term_freq in
    let dl = Float.of_int doc_length in
    let avgdl = Float.of_int total_tokens /. n in
    let idf = Float.log (((n -. n_t +. 0.5) /. (n_t +. 0.5)) +. 1.0) in
    idf *. (tf *. (k1 +. 1.0)) /. (tf +. (k1 *. (1.0 -. b +. (b *. dl /. avgdl)))))
;;

(** Collect all positive (non-negated) terms from a query for BM25. *)
let fts_query_terms query =
  let rec collect = function
    | Fts_query.FQ_term (Fts_query.FT_exact t) -> [ t ]
    | Fts_query.FQ_term (Fts_query.FT_prefix t) -> [ t ]
    | Fts_query.FQ_term (Fts_query.FT_phrase ts) -> ts
    | Fts_query.FQ_and qs | Fts_query.FQ_or qs -> List.concat_map collect qs
    | Fts_query.FQ_not _ -> []
  in
  List.sort_uniq String.compare (collect query)
;;

(** A snippet phrase is the unit SQLite FTS5 reports via xPhraseSize:
    either a single token (exact or prefix) or a multi-token exact
    phrase. The multi-token form requires consecutive token matches
    and is scored ONCE per occurrence (not once per constituent
    token) to mirror SQLite's centering and bm25 behaviour. *)
type snippet_phrase =
  | SP_term of string * [ `Exact | `Prefix ]
  | SP_phrase of string list (* length >= 2; all matched exactly *)

(** Collect snippet phrases from a query in left-to-right order. *)
let fts_query_terms_with_kind query : snippet_phrase list =
  let rec collect = function
    | Fts_query.FQ_term (Fts_query.FT_exact t) -> [ SP_term (t, `Exact) ]
    | Fts_query.FQ_term (Fts_query.FT_prefix t) -> [ SP_term (t, `Prefix) ]
    | Fts_query.FQ_term (Fts_query.FT_phrase ts) ->
      (match ts with
       | [] -> []
       | [ t ] -> [ SP_term (t, `Exact) ]
       | _ -> [ SP_phrase ts ])
    | Fts_query.FQ_and qs | Fts_query.FQ_or qs -> List.concat_map collect qs
    | Fts_query.FQ_not _ -> []
  in
  (* De-duplicate identical phrases (so a query like `foo AND foo` does
     not over-credit token highlights). Preserve first-occurrence order. *)
  let seen = Hashtbl.create 8 in
  let key = function
    | SP_term (t, `Exact) -> "e:" ^ t
    | SP_term (t, `Prefix) -> "p:" ^ t
    | SP_phrase ts -> "P:" ^ String.concat "\x00" ts
  in
  List.filter
    (fun p ->
       let k = key p in
       if Hashtbl.mem seen k
       then false
       else (
         Hashtbl.add seen k ();
         true))
    (collect query)
;;

(** Test whether the phrase at index [pi] matches the token sequence
    starting at [tokens.(i)]. Returns the phrase length on hit (so the
    caller can compute the end position), else [None]. *)
let phrase_match_at
      ~(phrases : snippet_phrase array)
      ~(tokens : Fts_tokenizer.token array)
      (pi : int)
      (i : int)
  : int option
  =
  let n_toks = Array.length tokens in
  let token_at j = tokens.(j).Fts_tokenizer.term in
  match phrases.(pi) with
  | SP_term (t, `Exact) ->
    if i < n_toks && String.equal (token_at i) t then Some 1 else None
  | SP_term (t, `Prefix) ->
    if i < n_toks
    then (
      let tk = token_at i in
      if
        String.length tk >= String.length t
        && String.equal (String.sub tk 0 (String.length t)) t
      then Some 1
      else None)
    else None
  | SP_phrase ts ->
    let len = List.length ts in
    if i + len > n_toks
    then None
    else (
      let rec walk j = function
        | [] -> true
        | t :: rest ->
          if String.equal (token_at (i + j)) t then walk (j + 1) rest else false
      in
      if walk 0 ts then Some len else None)
;;

(** Find the first phrase that matches at token position [i].
    Returns [(phrase_idx, length)] if any. *)
let token_phrase_match
      ~(phrases : snippet_phrase array)
      ~(tokens : Fts_tokenizer.token array)
      (i : int)
  : (int * int) option
  =
  let n = Array.length phrases in
  let rec loop pi =
    if pi >= n
    then None
    else (
      match phrase_match_at ~phrases ~tokens pi i with
      | Some len -> Some (pi, len)
      | None -> loop (pi + 1))
  in
  loop 0
;;

(** Identify FTS5 "sentence start" token positions in a column.
    Position 0 is always a sentence start. Any token preceded (after any
    intervening whitespace) by '.' or ':' also starts a sentence. *)
let fts_sentence_starts ~col_text ~(tokens : Fts_tokenizer.token array) : int array =
  let n = Array.length tokens in
  let buf = Buffer.create 8 in
  for i = 0 to n - 1 do
    let tok = tokens.(i) in
    if i = 0
    then Buffer.add_string buf (string_of_int 0)
    else (
      let start = tok.Fts_tokenizer.start_byte in
      (* Walk backwards skipping ' ', '\t', '\n', '\r'. *)
      let j = ref (start - 1) in
      while
        !j >= 0
        &&
        let c = col_text.[!j] in
        c = ' ' || c = '\t' || c = '\n' || c = '\r'
      do
        decr j
      done;
      if !j >= 0
      then (
        let c = col_text.[!j] in
        if c = '.' || c = ':'
        then (
          if Buffer.length buf > 0 then Buffer.add_char buf ',';
          Buffer.add_string buf (string_of_int i))))
  done;
  if Buffer.length buf = 0
  then [| 0 |]
  else
    Array.of_list
      (List.map int_of_string (String.split_on_char ',' (Buffer.contents buf)))
;;

(** Score a candidate window [i_pos, i_pos + n_token).
    Returns [(score, i_adj)] where:
      - score = 1000 for each new phrase instance seen + 1 for repeats.
      - i_adj = the actual starting position after centering adjustment,
                clamped to [0, n_docsize - n_token] (or 0 if window > doc).
    [a_seen] is reset by the caller before each call.
    [instances] is a sorted list of [(phrase_idx, position, length)] —
    a multi-token phrase counts as a single contiguous instance whose
    extent spans [position, position + length). *)
let fts_snippet_score
      ~(instances : (int * int * int) list)
      ~(a_seen : bool array)
      ~(i_pos : int)
      ~(n_token : int)
      ~(n_docsize : int)
  : int * int
  =
  let i_end = i_pos + n_token in
  let score = ref 0 in
  let i_first = ref (-1) in
  let i_last = ref 0 in
  List.iter
    (fun (ip, io, len) ->
       (* Phrase fully inside the window. SQLite requires the entire
       phrase span to fit; partial overlaps don't count. *)
       if io >= i_pos && io + len <= i_end
       then (
         score := !score + if a_seen.(ip) then 1 else 1000;
         a_seen.(ip) <- true;
         if !i_first < 0 then i_first := io;
         i_last := io + len))
    instances;
  let i_adj =
    if !i_first < 0 then i_pos else !i_first - ((n_token - (!i_last - !i_first)) / 2)
  in
  let i_adj = if i_adj + n_token > n_docsize then n_docsize - n_token else i_adj in
  let i_adj = if i_adj < 0 then 0 else i_adj in
  !score, i_adj
;;

(* Greedy scan for snippet phrase matches: at each token take the first phrase
   that matches, skipping past its length. Returns [(phrase_idx,pos,len)] list. *)
(** Build a highlighted excerpt of [col_text] for the given snippet [spec].
    Replicates SQLite FTS5's snippet() algorithm:
      - For each phrase instance, score the window anchored at its position
        (with centering adjustment), and also the window anchored at the
        latest preceding sentence start (with a +100 or +120 bonus).
      - Pick the (strictly) highest-scoring window; tie → earliest considered.
      - Reconstruct text from byte offsets, wrapping matched tokens (whole
        token for prefix matches) with [start_tag]/[end_tag].
      - Prepend [ellipsis] unless window starts at token 0.
      - Append [ellipsis] unless window covers through the last token.
    [query_terms] is a list of snippet phrases. *)
let snippet_build_instances ~phrases ~tokens ~n_toks =
  let acc = ref [] in
  let i = ref 0 in
  while !i < n_toks do
    match token_phrase_match ~phrases ~tokens !i with
    | None -> incr i
    | Some (ip, len) ->
      acc := (ip, tokens.(!i).Fts_tokenizer.pos, len) :: !acc;
      i := !i + len
  done;
  List.rev !acc
;;

(* No-match snippet: SQLite anchors at sentence start 0 and emits the first
   n_token tokens (no leading ellipsis; trailing ellipsis if doc is longer). *)
let snippet_no_match ~col_text ~tokens ~n_toks ~n_token ~spec =
  if n_toks = 0
  then ""
  else (
    let win_end_excl = min n_toks n_token in
    let last_tok = tokens.(win_end_excl - 1) in
    let prefix_text = String.sub col_text 0 last_tok.Fts_tokenizer.end_byte in
    if win_end_excl >= n_toks then prefix_text else prefix_text ^ spec.Plan.ellipsis)
;;

(* Choose the best snippet window start: score each instance position and each
   preceding sentence start (with a sentence-alignment bonus). *)
(* Score the candidate windows anchored at instance offset [io] — both the
   centered window and (when the column is longer than one window) the latest
   sentence start before [io], with a sentence-alignment bonus — feeding each
   to [consider]. *)
let snippet_score_instance
      ~consider
      ~instances
      ~a_seen
      ~sentence_starts
      ~n_phrases
      ~n_token
      ~n_toks
      io
  =
  (* Non-sentence-aligned: window anchored at this instance, centered. *)
  Array.fill a_seen 0 n_phrases false;
  let score, i_adj =
    fts_snippet_score ~instances ~a_seen ~i_pos:io ~n_token ~n_docsize:n_toks
  in
  consider score i_adj;
  (* Sentence-aligned: latest sentence start strictly before io. *)
  if n_toks > n_token
  then (
    let n_sent = Array.length sentence_starts in
    let jj = ref 0 in
    while !jj < n_sent - 1 && sentence_starts.(!jj + 1) <= io do
      incr jj
    done;
    let s_start = sentence_starts.(!jj) in
    if s_start < io
    then (
      Array.fill a_seen 0 n_phrases false;
      let score, _ =
        fts_snippet_score ~instances ~a_seen ~i_pos:s_start ~n_token ~n_docsize:n_toks
      in
      let bonus = if s_start = 0 then 120 else 100 in
      consider (score + bonus) s_start))
;;

let snippet_best_window ~instances ~tokens ~col_text ~n_phrases ~n_token ~n_toks =
  let a_seen = Array.make (max 1 n_phrases) false in
  let sentence_starts = fts_sentence_starts ~col_text ~tokens in
  let best_score = ref 0 in
  let best_start = ref 0 in
  let consider score start_pos =
    if score > !best_score
    then (
      best_score := score;
      best_start := start_pos)
  in
  List.iter
    (fun (_ip, io, _len) ->
       snippet_score_instance
         ~consider
         ~instances
         ~a_seen
         ~sentence_starts
         ~n_phrases
         ~n_token
         ~n_toks
         io)
    instances;
  !best_start
;;

(* Reconstruct the snippet text for the chosen window, wrapping matched phrase
   instances in start/end tags and emitting leading/trailing ellipses. *)
let snippet_render
      ~col_text
      ~tokens
      ~token_instance_at
      ~i_best_start
      ~n_token
      ~n_toks
      ~spec
  =
  let i_range_end = i_best_start + n_token - 1 in
  let buf = Buffer.create 128 in
  if i_best_start > 0 then Buffer.add_string buf spec.Plan.ellipsis;
  if n_toks > 0
  then (
    let first_in_range = i_best_start in
    let last_in_range = min (n_toks - 1) i_range_end in
    let prev_end = ref tokens.(first_in_range).Fts_tokenizer.start_byte in
    let prev_inst = ref (-1) in
    for i = first_in_range to last_in_range do
      let tok = tokens.(i) in
      let inst = token_instance_at.(i) in
      let gap_len = tok.Fts_tokenizer.start_byte - !prev_end in
      let gap = if gap_len > 0 then String.sub col_text !prev_end gap_len else "" in
      if !prev_inst <> inst
      then (
        (* Close the previous wrap, emit gap outside, open a new wrap if
           entering a phrase instance. *)
        if !prev_inst >= 0 then Buffer.add_string buf spec.Plan.end_tag;
        Buffer.add_string buf gap;
        if inst >= 0 then Buffer.add_string buf spec.Plan.start_tag)
      else
        (* Same wrap state — gap belongs to it (e.g. space inside <b>..</b>). *)
        Buffer.add_string buf gap;
      Buffer.add_string
        buf
        (String.sub
           col_text
           tok.Fts_tokenizer.start_byte
           (tok.Fts_tokenizer.end_byte - tok.Fts_tokenizer.start_byte));
      prev_end := tok.Fts_tokenizer.end_byte;
      prev_inst := inst
    done;
    if !prev_inst >= 0 then Buffer.add_string buf spec.Plan.end_tag;
    (* Trailing: append the rest of the source if the window reaches the last
       token, else a trailing ellipsis. *)
    if i_range_end >= n_toks - 1
    then (
      let last_end = tokens.(last_in_range).Fts_tokenizer.end_byte in
      if last_end < String.length col_text
      then
        Buffer.add_string
          buf
          (String.sub col_text last_end (String.length col_text - last_end)))
    else Buffer.add_string buf spec.Plan.ellipsis);
  Buffer.contents buf
;;

let compute_snippet
      ~col_text
      ~(query_terms : snippet_phrase list)
      ~(spec : Plan.snippet_spec)
  =
  let tokens = Array.of_list (Fts_tokenizer.tokenize_string ~col:0 col_text) in
  let n_toks = Array.length tokens in
  let phrases = Array.of_list query_terms in
  let n_phrases = Array.length phrases in
  let n_token = max 1 spec.Plan.n_tokens in
  let instances = snippet_build_instances ~phrases ~tokens ~n_toks in
  (* Mark each token position with its covering instance index (-1 = none),
     so adjacent occurrences of the same phrase emit separate wraps. *)
  let token_instance_at = Array.make (max 1 n_toks) (-1) in
  List.iteri
    (fun inst_idx (_ip, io, len) ->
       for k = 0 to len - 1 do
         if io + k < n_toks then token_instance_at.(io + k) <- inst_idx
       done)
    instances;
  if instances = [] || n_phrases = 0
  then snippet_no_match ~col_text ~tokens ~n_toks ~n_token ~spec
  else (
    let i_best_start =
      snippet_best_window ~instances ~tokens ~col_text ~n_phrases ~n_token ~n_toks
    in
    snippet_render
      ~col_text
      ~tokens
      ~token_instance_at
      ~i_best_start
      ~n_token
      ~n_toks
      ~spec)
;;

(* ------------------------------------------------------------------ *)
(* substitute_cte: replace Op_cte_scan nodes with Op_pragma_rows       *)
(* to_stream: convert a read op tree into a Row stream                  *)
(* pre_eval_subquery: resolve subquery Plan.expr nodes before row scan  *)
(* ------------------------------------------------------------------ *)

(** Check whether any unresolved subquery nodes remain in a Plan.expr. *)
let rec plan_expr_has_subquery : Plan.expr -> bool = function
  | Plan.P_subquery _ | Plan.P_exists _ | Plan.P_in_select _ -> true
  | Plan.P_binop (_, a, b) -> plan_expr_has_subquery a || plan_expr_has_subquery b
  | Plan.P_not e
  | Plan.P_is_null e
  | Plan.P_is_not_null e
  | Plan.P_neg e
  | Plan.P_bitnot e -> plan_expr_has_subquery e
  | Plan.P_between (x, lo, hi) ->
    plan_expr_has_subquery x || plan_expr_has_subquery lo || plan_expr_has_subquery hi
  | Plan.P_in (x, vs) -> plan_expr_has_subquery x || List.exists plan_expr_has_subquery vs
  | Plan.P_func (_, args) -> List.exists plan_expr_has_subquery args
  | Plan.P_case { scrutinee; branches; else_ } ->
    Option.fold ~none:false ~some:plan_expr_has_subquery scrutinee
    || List.exists
         (fun (c, r) -> plan_expr_has_subquery c || plan_expr_has_subquery r)
         branches
    || Option.fold ~none:false ~some:plan_expr_has_subquery else_
  | Plan.P_cast (e, _) -> plan_expr_has_subquery e
  | Plan.P_collate (e, _) -> plan_expr_has_subquery e
  (* #670: the leaves are listed rather than caught by [| _ ->].  This walker is
     one of four over [Plan.expr] that look for subquery-bearing nodes, and a
     catch-all in one of them is how they came to disagree: a [P_collate] arm
     missing from ONE of the four turned a correlated subquery under COLLATE
     into a refusal, with the other three insisting it was correlated.  An
     exhaustive match makes the next constructor a compile error in all four
     rather than a silent fall-through in whichever ones forgot it.

     Extended past the four the issue named, so the claim and the guard have
     the same extent: [substitute_excluded], [pre_eval_subquery] and
     [Planner.substitute_window_slots] are exhaustive too.  Nothing was broken
     in those three — all had their [P_collate] arm — but [pre_eval_subquery]
     sits directly on this same correlated-subquery path (it is what folds
     [P_subquery] / [P_exists] / [P_in_select]), so a catch-all there is the
     same latent hole in the same place.  [plan_expr_reads_only_cols] and
     [eval_expr] were already exhaustive.  No REWRITING walker over [Plan.expr]
     ends in a catch-all any more, in either file.  Two catch-alls over
     [Plan.expr] remain and are deliberate, because neither is a walker:
     [eval_expr]'s local [is_nocase] predicate (a two-way test, not a
     traversal) and [plan_and_conjuncts]'s [| e -> [ e ]] (its base case — any
     non-[And] node IS a leaf conjunct, and a new constructor is correctly one
     too). *)
  | Plan.P_lit _
  | Plan.P_col _
  | Plan.P_param _
  | Plan.P_excluded_col _
  | Plan.P_window_slot _ -> false
;;

(* #674 (item 1 of 3): does [e] read only columns [ok] accepts, and carry no
   subquery/excluded-row/window reference?  Used by the index-covering
   aggregate fast path to decide whether a residual predicate or an
   aggregate's argument expression can be evaluated straight off a decoded
   index key, without a [rh_get] of the table row.  [ok] is expected to gate
   on "is this column part of the index AND declared NOT NULL" — see the
   #536 NaN/NULL ambiguity discussion at the call site. *)
let rec plan_expr_reads_only_cols (ok : int -> bool) : Plan.expr -> bool = function
  | Plan.P_lit _ | Plan.P_param _ -> true
  | Plan.P_col i -> ok i
  | Plan.P_binop (_, a, b) ->
    plan_expr_reads_only_cols ok a && plan_expr_reads_only_cols ok b
  | Plan.P_not e
  | Plan.P_is_null e
  | Plan.P_is_not_null e
  | Plan.P_neg e
  | Plan.P_bitnot e -> plan_expr_reads_only_cols ok e
  | Plan.P_between (x, lo, hi) ->
    plan_expr_reads_only_cols ok x
    && plan_expr_reads_only_cols ok lo
    && plan_expr_reads_only_cols ok hi
  | Plan.P_in (x, vs) ->
    plan_expr_reads_only_cols ok x && List.for_all (plan_expr_reads_only_cols ok) vs
  | Plan.P_func (_, args) -> List.for_all (plan_expr_reads_only_cols ok) args
  | Plan.P_case { scrutinee; branches; else_ } ->
    Option.fold ~none:true ~some:(plan_expr_reads_only_cols ok) scrutinee
    && List.for_all
         (fun (c, r) -> plan_expr_reads_only_cols ok c && plan_expr_reads_only_cols ok r)
         branches
    && Option.fold ~none:true ~some:(plan_expr_reads_only_cols ok) else_
  | Plan.P_cast (e, _) -> plan_expr_reads_only_cols ok e
  | Plan.P_collate (e, _) -> plan_expr_reads_only_cols ok e
  | Plan.P_subquery _
  | Plan.P_exists _
  | Plan.P_in_select _
  | Plan.P_excluded_col _
  | Plan.P_window_slot _ -> false
;;

(** #592: the width of the row a plan subtree emits, or [None] when a node
    reshapes it (projection, aggregate, set operation, …). Only the
    layout-preserving spine is walked, because the width is used to place a
    join's right-hand columns and a projection between the two would move
    them. *)
let rec outer_row_width : Plan.op -> int option = function
  | Plan.Op_seq_scan { table_meta; _ }
  | Plan.Op_col_seq_scan { table_meta; _ }
  | Plan.Op_index_lookup { table_meta; _ }
  | Plan.Op_rowid_lookup { table_meta; _ } -> Some (List.length table_meta.Cat.columns)
  | Plan.Op_filter { child; _ } | Plan.Op_sort { child; _ } | Plan.Op_limit { child; _ }
    -> outer_row_width child
  | Plan.Op_hash_join { left; n_right_cols; _ } ->
    Option.map (fun w -> w + n_right_cols) (outer_row_width left)
  | Plan.Op_nested_loop_join { right_col_offset; n_right_cols; _ } ->
    Some (right_col_offset + n_right_cols)
  | _ -> None
;;

(** #592/#635: one base-table input of a plan subtree.

    [oi_ident] is the input's {b scope identifier} — the FROM item's alias where
    one was given, otherwise the table name. That is the single rule an outer
    reference resolves by, and it is the same rule [inner_scope_of] applies to a
    subquery's own FROM: an alias {i replaces} the table name rather than adding
    to it. *)
type outer_input =
  { oi_meta : Cat.table_meta
  ; oi_ident : string
  ; oi_offset : int (** ordinal of this input's first column in the emitted row *)
  }

(** #635: the scope identifier of a plan leaf — its alias where it has one. *)
let scan_ident (m : Cat.table_meta) (alias : string option) =
  Option.value alias ~default:m.Cat.name
;;

(** #592/#635: every base table feeding a plan subtree, paired with the ordinal
    of its first column in that subtree's output row and with the identifier an
    outer reference may name it by.

    This replaces the single-table [get_outer_scan_meta] that answered [None]
    over a join — the reason a correlated subquery in an INNER join's ON clause
    silently dropped every row. Both join operators concatenate [left] then
    [right] ([Array.append lrow rrow]), so the offsets are exactly the widths
    to the left of each input.

    Two inputs sharing an {b identifier} answers [None]: resolution is by
    identifier, so keeping both would silently pick one. The caller refuses
    instead. #635 narrowed that from "sharing a table name" —
    [FROM l AS x JOIN l AS y] carries two distinct identifiers and now resolves,
    while the unaliased [FROM l JOIN l] still cannot and is still refused. *)
let get_outer_scan_metas (op : Plan.op) : outer_input list option =
  let leaf table_meta alias base =
    Some
      [ { oi_meta = table_meta; oi_ident = scan_ident table_meta alias; oi_offset = base }
      ]
  in
  let rec go base : Plan.op -> outer_input list option = function
    | Plan.Op_seq_scan { table_meta; alias } | Plan.Op_col_seq_scan { table_meta; alias }
      -> leaf table_meta alias base
    | Plan.Op_index_lookup { table_meta; alias; _ }
    | Plan.Op_rowid_lookup { table_meta; alias; _ } -> leaf table_meta alias base
    | Plan.Op_filter { child; _ } | Plan.Op_sort { child; _ } | Plan.Op_limit { child; _ }
      -> go base child
    | Plan.Op_hash_join { left; right; _ } ->
      (match go base left, outer_row_width left with
       | Some ls, Some w -> Option.map (fun rs -> ls @ rs) (go (base + w) right)
       | _ -> None)
    | Plan.Op_nested_loop_join { left; right_meta; right_alias; right_col_offset; _ } ->
      Option.map
        (fun ls ->
           ls
           @ [ { oi_meta = right_meta
               ; oi_ident = scan_ident right_meta right_alias
               ; oi_offset = base + right_col_offset
               }
             ])
        (go base left)
    | _ -> None
  in
  match go 0 op with
  | None -> None
  | Some inputs ->
    let idents = List.map (fun i -> i.oi_ident) inputs in
    if List.length (List.sort_uniq String.compare idents) <> List.length idents
    then None
    else Some inputs
;;

(** #592: how a correlated subquery's outer column references are resolved
    against one row of the enclosing operator. Both lookups answer [None] when
    the name is not an outer reference, in which case the reference is left
    alone (it belongs to the subquery's own scope, or it is unresolvable and
    the caller refuses the query).

    #493: the two lookups answer an [Ast.expr] rather than a [Row.value],
    because there are now two ways to pin an outer reference — as the row's
    literal value ({!binding_of_metas}) or as a positional parameter addressing
    that row slot ({!param_binding_of_metas}). The substitution walk is shared
    and does not care which. *)
type outer_binding =
  { bind_qual : string -> string -> Ast.expr option (** [table] then [column] *)
  ; bind_unqual : string -> Ast.expr option
  }

(** Build an [outer_binding] over a row whose layout is described by
    [get_outer_scan_metas].

    #635: a qualified reference matches an input's {b scope identifier} — the
    alias where the FROM item has one, the table name otherwise — so
    [FROM l AS x] resolves [x.a] and leaves [l.a] unresolved, which the caller
    then refuses. An unqualified name that more than one input carries is
    ambiguous and is likewise left unresolved rather than guessed at.

    #493: the binding yields an {!Ast.expr}, not a {!Row.value}, so that
    {!param_binding_of_metas} can pin an outer value as a positional parameter
    instead of a literal and keep the inner statement identical across rows.
    This binder is the literal spelling of the same interface. *)
let binding_of_metas (inputs : outer_input list) (row : Row.t) : outer_binding =
  let at (i : outer_input) col =
    match find_col_idx_by_name i.oi_meta.Cat.columns col with
    | k when i.oi_offset + k < Array.length row ->
      Some (Ast.E_lit (value_to_literal row.(i.oi_offset + k)))
    | _ -> None
    | exception Failure _ -> None
  in
  { bind_qual =
      (fun tbl col ->
        match List.find_opt (fun i -> String.equal i.oi_ident tbl) inputs with
        | None -> None
        | Some i -> at i col)
  ; bind_unqual =
      (fun col ->
        match List.filter_map (fun i -> at i col) inputs with
        | [ v ] -> Some v
        | _ -> None)
  }
;;

(** #493: the same binding as {!binding_of_metas}, except each outer reference
    is pinned as a positional PARAMETER addressing the row slot it came from
    rather than as that slot's literal value.

    [base] is where the outer row is spliced into the parameter array — the
    caller runs the subquery with [Array.append params row], so outer row slot
    [k] is parameter [base + k] (0-based), spelled [Ast.Param_index] which
    {!Sema.resolve_param} reads 1-based.

    Why this exists: with literals the substituted subquery statement differs on
    every outer row, so it has to be re-bound and re-planned per row — the whole
    of #493's cost. With parameters it is structurally identical for every row,
    which is what makes the {!subplan_cache} hit, and the planner still gets an
    index seek out of it because {!Planner.recognise_eq_col_lit} accepts a bound
    parameter on the value side exactly as it accepts a literal (#228's prepared
    point lookup).

    [row_len] must be the width of the rows being scanned, so that the
    "is this actually an outer reference?" decision matches the literal
    binding's bound check exactly.

    #635: this resolves qualified references against the input's {b scope
    identifier} exactly as {!binding_of_metas} does — alias where the FROM item
    has one, table name otherwise. The two binders must agree on WHICH
    references resolve; they differ only in what they substitute (a parameter
    here, a literal there). If they diverge, the same query answers differently
    depending on whether its subquery happened to take the cached path. *)
let param_binding_of_metas ~(base : int) ~(row_len : int) (inputs : outer_input list)
  : outer_binding
  =
  let at (i : outer_input) col =
    match find_col_idx_by_name i.oi_meta.Cat.columns col with
    | k when i.oi_offset + k < row_len ->
      Some (Ast.E_param (Ast.Param_index (base + i.oi_offset + k + 1)))
    | _ -> None
    | exception Failure _ -> None
  in
  { bind_qual =
      (fun tbl col ->
        match List.find_opt (fun i -> String.equal i.oi_ident tbl) inputs with
        | None -> None
        | Some i -> at i col)
  ; bind_unqual =
      (fun col ->
        match List.filter_map (fun i -> at i col) inputs with
        | [ v ] -> Some v
        | _ -> None)
  }
;;

(** #493: does [e] mention a bound parameter anywhere, including inside a nested
    subquery?

    The parameter substitution above splices the outer row in at [base =
    Array.length params], which is safe only while the inner statement has no
    parameters of its own: {!Sema.resolve_param} numbers an inner statement's
    placeholders from 0 with a fresh counter, and an explicit [Param_index] also
    {i advances} that counter, so a subquery that mixes its own [?] with the
    injected ones could have a placeholder land on a row slot. Rather than
    reason about encounter order, the caller falls back to the pre-#493
    literal-substitution path whenever this answers [true]. *)
let rec ast_expr_uses_param : Ast.expr -> bool = function
  | Ast.E_param _ -> true
  | Ast.E_binop (_, a, b) -> ast_expr_uses_param a || ast_expr_uses_param b
  | Ast.E_not a
  | Ast.E_is_null a
  | Ast.E_is_not_null a
  | Ast.E_neg a
  | Ast.E_bitnot a
  | Ast.E_collate (a, _)
  | Ast.E_cast (a, _) -> ast_expr_uses_param a
  | Ast.E_between (x, lo, hi) ->
    ast_expr_uses_param x || ast_expr_uses_param lo || ast_expr_uses_param hi
  | Ast.E_in (x, vs) -> ast_expr_uses_param x || List.exists ast_expr_uses_param vs
  | Ast.E_agg (_, a) -> Option.fold ~none:false ~some:ast_expr_uses_param a
  (* #491: a DISTINCT aggregate's argument is walked like a plain one — #493's
     gate is about whether a placeholder could land on an injected row slot,
     and DISTINCT changes nothing about that. *)
  | Ast.E_agg_distinct (_, a) -> ast_expr_uses_param a
  | Ast.E_func (_, args) -> List.exists ast_expr_uses_param args
  | Ast.E_subquery s | Ast.E_exists s -> ast_stmt_uses_param s
  | Ast.E_in_select (x, s) -> ast_expr_uses_param x || ast_stmt_uses_param s
  | Ast.E_case { scrutinee; branches; else_ } ->
    Option.fold ~none:false ~some:ast_expr_uses_param scrutinee
    || List.exists (fun (c, r) -> ast_expr_uses_param c || ast_expr_uses_param r) branches
    || Option.fold ~none:false ~some:ast_expr_uses_param else_
  | Ast.E_window { args; window; _ } ->
    List.exists ast_expr_uses_param args
    || List.exists ast_expr_uses_param window.Ast.partition_by
    || List.exists
         (fun (k : Ast.order_key) -> ast_expr_uses_param k.Ast.expr)
         window.Ast.order_by
  | Ast.E_lit _ | Ast.E_col _ | Ast.E_tbl_col _ | Ast.E_match _ | Ast.E_fts_snippet _ ->
    false

(** #493: the statement-level half of {!ast_expr_uses_param}. Anything that is
    not a SELECT-shaped statement answers [true] — conservatively refusing the
    parameterized path rather than enumerating write-statement shapes that
    cannot appear in a subquery position anyway. *)
and ast_stmt_uses_param : Ast.stmt -> bool = function
  | Ast.S_select r ->
    (match r.proj with
     | `All | `Cols _ -> false
     | `Exprs es -> List.exists (fun (e, _) -> ast_expr_uses_param e) es)
    || Option.fold ~none:false ~some:ast_expr_uses_param r.where
    || Option.fold ~none:false ~some:ast_expr_uses_param r.having
    || List.exists (fun (j : Ast.join_clause) -> ast_expr_uses_param j.Ast.on) r.joins
    || List.exists (fun (k : Ast.order_key) -> ast_expr_uses_param k.Ast.expr) r.order
  | Ast.S_compound { left; right; order; _ } ->
    ast_stmt_uses_param left
    || ast_stmt_uses_param right
    || List.exists (fun (k : Ast.order_key) -> ast_expr_uses_param k.Ast.expr) order
  | Ast.S_with_cte { def; query; _ } ->
    ast_stmt_uses_param def || ast_stmt_uses_param query
  | _ -> true
;;

(** #493: does any subquery embedded in this plan expression use a parameter of
    its own?  Only the three subquery-carrying nodes are inspected; the plan
    expression's own [P_param] nodes are irrelevant, because they address the
    OUTER statement's parameter array, whose slots all sit below [base]. *)
let rec plan_expr_subqueries_use_param : Plan.expr -> bool = function
  | Plan.P_subquery s | Plan.P_exists s -> ast_stmt_uses_param s
  | Plan.P_in_select (x, s) -> plan_expr_subqueries_use_param x || ast_stmt_uses_param s
  | Plan.P_binop (_, a, b) ->
    plan_expr_subqueries_use_param a || plan_expr_subqueries_use_param b
  | Plan.P_not e
  | Plan.P_is_null e
  | Plan.P_is_not_null e
  | Plan.P_neg e
  | Plan.P_bitnot e
  | Plan.P_cast (e, _)
  | Plan.P_collate (e, _) -> plan_expr_subqueries_use_param e
  | Plan.P_between (x, lo, hi) ->
    plan_expr_subqueries_use_param x
    || plan_expr_subqueries_use_param lo
    || plan_expr_subqueries_use_param hi
  | Plan.P_in (x, vs) ->
    plan_expr_subqueries_use_param x || List.exists plan_expr_subqueries_use_param vs
  | Plan.P_func (_, args) -> List.exists plan_expr_subqueries_use_param args
  | Plan.P_case { scrutinee; branches; else_ } ->
    Option.fold ~none:false ~some:plan_expr_subqueries_use_param scrutinee
    || List.exists
         (fun (c, r) ->
            plan_expr_subqueries_use_param c || plan_expr_subqueries_use_param r)
         branches
    || Option.fold ~none:false ~some:plan_expr_subqueries_use_param else_
  (* #670: exhaustive, not [| _ ->] — see {!plan_expr_has_subquery}. *)
  | Plan.P_lit _
  | Plan.P_col _
  | Plan.P_param _
  | Plan.P_excluded_col _
  | Plan.P_window_slot _ -> false
;;

(** #493: every [Ast.stmt] a plan expression carries in a subquery position.

    Used by {!refuse_unresolved_correlation} to decide the #592 refusal from the
    PLAN rather than from a row. Only the top level is collected — a subquery
    nested inside one of these statements is resolved by that statement's own
    [stream_filter] when it runs, which is exactly where the pre-#493 per-row
    check placed it too. *)
let rec plan_expr_embedded_stmts : Plan.expr -> Ast.stmt list = function
  | Plan.P_subquery s | Plan.P_exists s -> [ s ]
  | Plan.P_in_select (x, s) -> s :: plan_expr_embedded_stmts x
  | Plan.P_binop (_, a, b) -> plan_expr_embedded_stmts a @ plan_expr_embedded_stmts b
  | Plan.P_not e
  | Plan.P_is_null e
  | Plan.P_is_not_null e
  | Plan.P_neg e
  | Plan.P_bitnot e
  | Plan.P_cast (e, _)
  | Plan.P_collate (e, _) -> plan_expr_embedded_stmts e
  | Plan.P_between (x, lo, hi) ->
    plan_expr_embedded_stmts x @ plan_expr_embedded_stmts lo @ plan_expr_embedded_stmts hi
  | Plan.P_in (x, vs) ->
    plan_expr_embedded_stmts x @ List.concat_map plan_expr_embedded_stmts vs
  | Plan.P_func (_, args) -> List.concat_map plan_expr_embedded_stmts args
  | Plan.P_case { scrutinee; branches; else_ } ->
    Option.fold ~none:[] ~some:plan_expr_embedded_stmts scrutinee
    @ List.concat_map
        (fun (c, r) -> plan_expr_embedded_stmts c @ plan_expr_embedded_stmts r)
        branches
    @ Option.fold ~none:[] ~some:plan_expr_embedded_stmts else_
  (* #670: exhaustive, not [| _ ->] — see {!plan_expr_has_subquery}. *)
  | Plan.P_lit _
  | Plan.P_col _
  | Plan.P_param _
  | Plan.P_excluded_col _
  | Plan.P_window_slot _ -> []
;;

(** #493: flatten a plan predicate's top-level [AND] spine.

    [stream_filter] evaluates the conjuncts left to right and stops at the first
    that is not truthy, so a correlated subquery written after a cheap
    restriction — TPC-H Q4's shape exactly — is never run for a row the
    restriction already rejected. Filtering is a two-valued decision (a row
    passes iff every conjunct is truthy), so per-conjunct truthiness agrees with
    [value_truthy] over the whole [AND] tree, NULL operands included. The
    conjuncts are NOT reordered: only short-circuited in the order written. *)
let rec plan_and_conjuncts : Plan.expr -> Plan.expr list = function
  | Plan.P_binop (Plan.And, a, b) -> plan_and_conjuncts a @ plan_and_conjuncts b
  | e -> [ e ]
;;

(** #558: the message used when a subquery beside an aggregate cannot be
    resolved.  Only a {i grouped} column can be an outer reference here — the
    aggregate output row holds nothing else from the input — so an ungrouped
    reference, or a child whose base tables cannot be located, is refused. *)
let agg_subquery_refusal () =
  "Exec: a correlated subquery in an aggregate expression can only reference a GROUP BY \
   column (#558). The aggregate output row carries the grouped columns and the aggregate \
   values, and nothing else of the input rows."
;;

(** #665: the message the SUM/AVG accumulators fail with when a value that is
    not a number reaches them.

    [Sema.agg_arg_static_ty] now refuses at BIND time every SUM/AVG argument
    whose type is statically known to be TEXT or BLOB, in all three spellings
    — a bare column, a wrapped aggregate, and #488's expression argument.  What
    still reaches here is an argument whose type could not be known until a row
    arrived: a scalar function, a bound parameter, a subquery, or a CASE whose
    arms disagree.  For those the offending value's storage class is the whole
    diagnostic the caller gets, so it is named rather than left to be guessed.

    Defined at the top level rather than inside the accumulator's [let rec]
    group on purpose: it returns ['a] and is instantiated at three different
    types (the [int64] and [float] SUM folds, and [unit] in
    [make_agg_acc_over_values]), which monomorphic recursion inside the group
    would not allow. *)
let agg_non_numeric_failure (what : string) (v : Row.value) =
  let cls =
    match v with
    | Row.V_null -> "NULL"
    | Row.V_int _ -> "INTEGER"
    | Row.V_real _ -> "REAL"
    | Row.V_text _ -> "TEXT"
    | Row.V_blob _ -> "BLOB"
  in
  failwith (Printf.sprintf "%s on non-numeric value (%s)" what cls)
;;

(** #664: the message used when a subquery inside an aggregate ARGUMENT cannot
    be resolved.

    Deliberately distinct from {!agg_subquery_refusal}, because the two are
    about different rows.  A subquery in [having] or [proj] is evaluated per
    aggregate OUTPUT row, which carries only the grouped columns and the
    aggregate values — so only a GROUP BY column can be its outer reference.
    An ARGUMENT is evaluated per INPUT row, before any grouping, so any column
    the child carries is fair game and this fires only when the child's base
    tables cannot be located at all, or the reference names none of their
    columns.  Reporting the group-by message for an argument would send the
    reader to a rule that does not apply to it. *)
let agg_arg_subquery_refusal () =
  "Exec: a correlated subquery in an aggregate argument cannot be resolved — its outer \
   column reference has no source in the rows being aggregated (#664). Rewrite it as an \
   uncorrelated subquery, or qualify the outer column with the table name or alias it is \
   in scope under (#635)."
;;

(** #558: bind outer column references appearing in an aggregate projection or
    HAVING against one aggregate {i output} row, whose leading
    [List.length group_cols] slots hold the grouped columns in order.  [metas]
    describes the {b child} row layout, so each group ordinal is mapped back to
    the table and column it came from. *)
let binding_of_group_cols
      (inputs : outer_input list)
      (group_cols : int list)
      (agg_row : Row.t)
  : outer_binding
  =
  let entry i child_ord =
    if i >= Array.length agg_row
    then None
    else (
      match
        List.find_opt
          (fun (oi : outer_input) ->
             child_ord >= oi.oi_offset
             && child_ord < oi.oi_offset + List.length oi.oi_meta.Cat.columns)
          inputs
      with
      | None -> None
      | Some oi ->
        let c : Row.column = List.nth oi.oi_meta.Cat.columns (child_ord - oi.oi_offset) in
        (* #635: the qualifier recorded here is the input's scope identifier, so
           an alias-qualified reference to a grouped column resolves too.
           #493: wrapped as an expr here rather than by the walker, so the
           parameterized binder can substitute a [Param_index] instead. *)
        Some (oi.oi_ident, c.Row.name, Ast.E_lit (value_to_literal agg_row.(i))))
  in
  let entries = List.filter_map Fun.id (List.mapi entry group_cols) in
  { bind_qual =
      (fun tbl col ->
        List.find_map
          (fun (t, c, v) ->
             if String.equal t tbl && String.equal c col then Some v else None)
          entries)
  ; bind_unqual =
      (fun col ->
        match
          List.filter_map
            (fun (_, c, v) -> if String.equal c col then Some v else None)
            entries
        with
        | [ v ] -> Some v
        | _ -> None)
  }
;;

(** #592: the message used wherever a correlated subquery cannot be resolved.
    It replaces the silent [fun _row -> false] that dropped every row. *)
let correlated_filter_refusal () =
  "Exec: a correlated subquery in this predicate cannot be resolved — its outer column \
   reference has no source in the rows being filtered (#592). Rewrite it as an \
   uncorrelated subquery, or qualify the outer column with the table name or alias it is \
   in scope under (#635)."
;;

(** #626: the projection's counterpart to {!correlated_filter_refusal}.

    [stream_expr_project] used to keep a non-raising fallback when the
    correlation source could not be located: every such subquery evaluated to
    [Row.V_null]. A NULL there is indistinguishable from a legitimately-NULL
    aggregate, so the caller could not tell "no matching rows" from "the engine
    could not resolve this correlation" — the #592 failure mode wearing a
    different hat, a column of plausible NULLs instead of zero rows. The same
    shape in a WHERE or an ON clause was already refused, so the two spellings
    disagreed about the same unresolvable reference. *)
let correlated_projection_refusal () =
  "Exec: a correlated subquery in this projection cannot be resolved — its outer column \
   reference has no source in the rows being projected (#626). Rewrite it as an \
   uncorrelated subquery, or qualify the outer column with the table name or alias it is \
   in scope under (#635)."
;;

(** #615: the outer join's counterpart.

    #566 refused every correlated subquery in an outer join's ON predicate,
    because the correlation source could not be located over a join node. #592
    built that source ([get_outer_scan_metas]), so the resolvable shapes are now
    evaluated and only the genuinely unresolvable ones reach this message —
    which is the same boundary the INNER spelling has. *)
let correlated_on_refusal () =
  "Exec: a correlated subquery in an outer join's ON predicate cannot be resolved — its \
   outer column reference has no source in the joined row (#615; #566 refused every \
   spelling of this before the correlation source existed). Rewrite it as an \
   uncorrelated subquery, or qualify the outer column with the table name or alias it is \
   in scope under (#635)."
;;

(** #592: what the {i inner} SELECT already has in scope, so a reference the
    subquery owns can be told apart from an outer one. SQL resolves
    innermost-first, and both halves of that matter:

    - [has_col] guards an {b unqualified} reference: [n] inside the subquery is
      the subquery's own [n] if its FROM provides one.
    - [has_table] guards a {b qualified} one: when a subquery's own FROM names
      the same table as an outer input, a [t.x] inside it is the {i inner}
      [t], not the outer one, and pinning it to the outer row's value answers a
      plausible wrong result rather than an empty one. That guard was missing
      when #592 first widened resolution to joins; on a single-table outer the
      two [t]s are indistinguishable, so the join is what made it reachable. *)
type inner_scope =
  { has_col : string -> bool
  ; has_table : string -> bool
  }

(** #635: the scope an outer reference is judged against when the substitution
    descends through {i more than one} level of subquery nesting.

    SQL resolves innermost-first, and "innermost" is cumulative: a name owned by
    an {i intermediate} subquery belongs to that subquery, not to the outer row,
    even when the innermost SELECT knows nothing about it.  Descending with only
    the innermost scope would rewrite such a name from the outer row — a
    plausible wrong answer, the failure class this whole area is about — so the
    scopes are unioned on the way down. *)
let scope_union (a : inner_scope) (b : inner_scope) : inner_scope =
  { has_col = (fun n -> a.has_col n || b.has_col n)
  ; has_table = (fun n -> a.has_table n || b.has_table n)
  }
;;

(** The scope enclosing the {i outermost} correlated subquery: the operator
    holding the outer row owns no identifier the subquery could be shadowed by,
    so nothing is hidden at that level. *)
let no_inner_scope : inner_scope =
  { has_col = (fun _ -> false); has_table = (fun _ -> false) }
;;

(** #592: build the [inner_scope] of a SELECT.

    The {b table identifiers} are pure syntax — the alias where one is given,
    otherwise the table name — so they are computed even when the catalog
    cannot resolve the tables. An alias {i replaces} the name rather than
    adding to it: in [FROM t q] the identifier in scope is [q], and [t.x]
    therefore resolves {i outward}, which is what sqlite3 does.

    The {b column names} do need the catalog. If any FROM table is
    unresolvable, [has_col] answers [true] for everything — "assume the
    subquery owns it" — which reduces to the pre-#592 behaviour of rewriting
    only qualified references. Anything else would rewrite on a guess.

    #732: a FROM-LESS SELECT ([S_const_select], the shape of
    [(SELECT o.n * 10)]) owns nothing — it has no inputs at all — so it is
    {!no_inner_scope}, and every column reference in it is by construction the
    enclosing query's. That is not the same as the default below: "owns
    everything" would shadow the outer reference the substitution exists to
    resolve, and this is now a statement form whose clauses {i are} rewritten.

    Every other non-SELECT has no clauses this module rewrites, so both halves
    default to "shadowed" there. *)
let inner_scope_of (cat_opt : Cat.t option) (s : Ast.stmt) : inner_scope =
  match s with
  | Ast.S_select r ->
    let idents =
      Option.value r.table_alias ~default:r.table
      :: List.map
           (fun (j : Ast.join_clause) -> Option.value j.Ast.alias ~default:j.Ast.table)
           r.joins
    in
    let has_table name = List.exists (String.equal name) idents in
    let tables = r.table :: List.map (fun (j : Ast.join_clause) -> j.Ast.table) r.joins in
    let resolved =
      match cat_opt with
      | None -> [ None ]
      | Some cat ->
        List.map
          (fun t ->
             Option.map
               (fun (m : Cat.table_meta) ->
                  List.map (fun (c : Row.column) -> c.Row.name) m.Cat.columns)
               (Cat.find_table_cached cat ~name:t))
          tables
    in
    let has_col =
      if List.exists Option.is_none resolved
      then fun _ -> true
      else (
        let cols = List.concat_map Option.get resolved in
        fun name -> List.exists (String.equal name) cols)
    in
    { has_col; has_table }
  | Ast.S_const_select _ -> no_inner_scope
  | _ -> { has_col = (fun _ -> true); has_table = (fun _ -> true) }
;;

(** Substitute outer column refs with literal values drawn from [bnd], leaving
    anything [scope] says the subquery owns alone.

    #635: the [E_subquery] / [E_exists] / [E_in_select] arms are what make this
    work at {i any} nesting depth.  They used to fall to the catch-all, so a
    reference from a doubly-nested subquery to the outermost query was never
    substituted — it survived, and the caller refused the query.  Descending
    carries the union of every enclosing subquery's scope (see {!scope_union}),
    so an intermediate level still shadows what it owns. *)
let rec substitute_outer_in_expr
          ~(cat : Cat.t option)
          ~(scope : inner_scope)
          (bnd : outer_binding)
          (e : Ast.expr)
  : Ast.expr
  =
  let go = substitute_outer_in_expr ~cat ~scope bnd in
  let go_s = substitute_outer_in_stmt ~cat ~enclosing:scope bnd in
  match e with
  (* #493: the binding already yields an expr — a literal from
     {!binding_of_metas}, a [Param_index] from {!param_binding_of_metas}.  Do
     NOT re-wrap here: that is what keeps the parameterized spelling, and with
     it the single cached plan, reachable. *)
  | Ast.E_tbl_col (tbl, col) when not (scope.has_table tbl) ->
    (match bnd.bind_qual tbl col with
     | Some pinned -> pinned
     | None -> e)
  (* #744: a bare [true]/[false] is never an outer column reference.  The scope
     test below already answers "owned" whenever the subquery's own FROM has a
     column of that name, so this arm only fires where nothing inside answers to
     it — which is exactly where [Sema] resolves it to the literal.  Without it,
     [stmt_has_free_column_ref] (which runs this walker against a binding that
     resolves nothing, #635) reads every [WHERE true] inside a subquery as a
     free outer reference: the subquery is then treated as correlated, and
     [SELECT (SELECT COUNT(x) FROM q WHERE true)] answered NULL while the [IN]
     spelling raised {!refuse_unresolved_correlation}. *)
  | Ast.E_col name when Option.is_some (Ast.bool_ident_lit name) -> e
  | Ast.E_col name when not (scope.has_col name) ->
    (match bnd.bind_unqual name with
     | Some pinned -> pinned
     | None -> e)
  | Ast.E_binop (op, a, b) -> Ast.E_binop (op, go a, go b)
  | Ast.E_not a -> Ast.E_not (go a)
  | Ast.E_is_null a -> Ast.E_is_null (go a)
  | Ast.E_is_not_null a -> Ast.E_is_not_null (go a)
  | Ast.E_neg a -> Ast.E_neg (go a)
  | Ast.E_bitnot a -> Ast.E_bitnot (go a)
  | Ast.E_between (x, lo, hi) -> Ast.E_between (go x, go lo, go hi)
  | Ast.E_in (x, vals) -> Ast.E_in (go x, List.map go vals)
  | Ast.E_func (f, args) -> Ast.E_func (f, List.map go args)
  | Ast.E_cast (x, ty) -> Ast.E_cast (go x, ty)
  | Ast.E_case { scrutinee; branches; else_ } ->
    Ast.E_case
      { scrutinee = Option.map go scrutinee
      ; branches = List.map (fun (c, r) -> go c, go r) branches
      ; else_ = Option.map go else_
      }
  | Ast.E_subquery inner -> Ast.E_subquery (go_s inner)
  | Ast.E_exists inner -> Ast.E_exists (go_s inner)
  | Ast.E_in_select (x, inner) -> Ast.E_in_select (go x, go_s inner)
  (* #670: the same missing COLLATE descent as in {!substitute_outer_in_plan_expr},
     one level up.  This walker handles a correlated reference inside the
     subquery's OWN clauses, so without this arm
     [EXISTS (SELECT 1 FROM i WHERE i.v = o.x COLLATE NOCASE)] left [o.x]
     unsubstituted and was refused — a second reachable spelling of the same
     defect, and one the plan-level fix does not cover. *)
  | Ast.E_collate (x, c) -> Ast.E_collate (go x, c)
  (* True leaves: nothing inside to substitute.  [E_fts_snippet] belongs here
     and not below — it is a record of a table name, a column index, three
     string tags and a token count ([Ast.E_fts_snippet], ast.ml:247), with no
     [expr] field anywhere, so there is no spelling in which an outer reference
     could appear inside it. *)
  | Ast.E_lit _ | Ast.E_param _ | Ast.E_match _ | Ast.E_fts_snippet _ -> e
  (* [E_col] / [E_tbl_col] the enclosing scope DOES own — the two guarded arms
     at the top of this match handle the ones it does not.  Listed rather than
     left to a catch-all so those guards cannot be edited into a silent hole. *)
  | Ast.E_col _ | Ast.E_tbl_col _ -> e
  (* #721: these three carry sub-expressions and are now descended into.  #670
     listed them explicitly with [-> e] to make the omission visible; each was
     a spelling in which a correlated reference sat somewhere this walker never
     looked, so the reference survived, [Sema.bind] failed on it, and the
     statement came back as a refusal.

     [E_agg] is the one that is reachable on its own, in a correlated
     subquery's HAVING —
     [EXISTS (SELECT 1 FROM i WHERE i.fk = o.k GROUP BY i.fk
              HAVING SUM(i.v + o.n) > 0)] — and #488, which made an aggregate's
     argument a general expression rather than a bare column, is what widened
     that surface.  [E_agg_distinct] is the same node with DISTINCT.
     [E_window] cannot appear in WHERE or HAVING at all, so its only spelling
     is a subquery's PROJECTION, which is #732's half of this fix; the two
     arrive together for that reason.

     [window_spec.frame] carries no [expr] — [frame_bound]'s offsets are [int]
     ([Ast.frame_bound], ast.ml:158) — so [partition_by] and [order_by] are the
     whole of it. *)
  | Ast.E_agg (f, a) -> Ast.E_agg (f, Option.map go a)
  | Ast.E_agg_distinct (f, a) -> Ast.E_agg_distinct (f, go a)
  | Ast.E_window { func; args; window } ->
    let go_key (k : Ast.order_key) = { k with Ast.expr = go k.Ast.expr } in
    Ast.E_window
      { func
      ; args = List.map go args
      ; window =
          { window with
            Ast.partition_by = List.map go window.Ast.partition_by
          ; Ast.order_by = List.map go_key window.Ast.order_by
          }
      }

(** Apply substitute_outer_in_expr to every clause of an AST stmt that can carry
    an outer reference.  [enclosing] is the union of the scopes of every
    subquery between this one and the row [bnd] describes; it is
    {!no_inner_scope} at the top.

    #732: this used to rewrite only WHERE / HAVING / [joins.*.on] and end in a
    [| _ -> s] catch-all, so an outer reference in the subquery's own
    PROJECTION was never substituted and the statement was refused with #626's
    message — for [SELECT k, (SELECT i.m + o.n FROM i WHERE i.fk = o.k) FROM o],
    which sqlite3 answers.  See {!substitute_outer_proj} for the projection and
    the arms below for the rest.

    Two clauses of [S_select] are still passed through, and neither is a hole:
    [limit] / [offset] are [int option] and [group_by] is a
    [(string * string option) list] ([Ast.group_by_item]) — neither can hold a
    substituted literal, so there is no spelling in which an outer reference
    reaches them.  (sqlite3 rejects [GROUP BY <outer col>] in a subquery with
    "no such column" in any case.)

    [order] is passed through {b deliberately}, and this is the one judgement
    call in #732:
    - sqlite3 refuses a correlated reference in a subquery's ORDER BY outright
      ([SELECT k, (SELECT i.v FROM i WHERE i.fk = o.k ORDER BY o.n LIMIT 1)
       FROM o] → "no such column: o.n", oracle-checked on 3.45.1), so granary's
      refusal already agrees with it and rewriting the clause would {i create} a
      divergence rather than remove one;
    - an ORDER BY key may name an OUTPUT ALIAS rather than an input column
      (#489/#663), and an alias is not in [inner_scope_of]'s [has_col].  So a
      subquery whose alias happens to share a name with an outer column would
      have that key rewritten to a literal — an ORDER BY over a constant, i.e.
      a silently unsorted result.  That is precisely the "plausible wrong
      answer" class this whole area exists to avoid, and it is a worse outcome
      than the refusal it would replace.
    [S_compound]'s [order] is passed through for the same two reasons. *)
and substitute_outer_in_stmt
      ~(cat : Cat.t option)
      ~(enclosing : inner_scope)
      (bnd : outer_binding)
      (s : Ast.stmt)
  : Ast.stmt
  =
  let scope = scope_union (inner_scope_of cat s) enclosing in
  let go_e = substitute_outer_in_expr ~cat ~scope bnd in
  let go_s = substitute_outer_in_stmt ~cat ~enclosing bnd in
  match s with
  | Ast.S_select r ->
    Ast.S_select
      { r with
        proj = substitute_outer_proj go_e r.proj
      ; where = Option.map go_e r.where
      ; having = Option.map go_e r.having
      ; joins = List.map (fun j -> { j with Ast.on = go_e j.Ast.on }) r.joins
      }
  | Ast.S_compound { op; left; right; order; limit; offset } ->
    Ast.S_compound { op; left = go_s left; right = go_s right; order; limit; offset }
  | Ast.S_with_cte { name; def; query; recursive } ->
    Ast.S_with_cte { name; def = go_s def; query = go_s query; recursive }
  (* #732: a FROM-less subquery — [SELECT k, (SELECT o.n * 10) FROM o], which
     sqlite3 answers — parses to [S_const_select] and so fell to the catch-all
     with everything else.  [inner_scope_of] gives it {!no_inner_scope} (it owns
     no input, so it can shadow nothing), which is what makes [scope] here
     exactly the union of the enclosing subqueries' scopes and every remaining
     column reference the outer row's. *)
  | Ast.S_const_select { exprs } ->
    Ast.S_const_select { exprs = List.map (fun (e, a) -> go_e e, a) exprs }
  (* #732: exhaustive, not [| _ -> s].  None of these can appear as the body of
     an [E_subquery] / [E_exists] / [E_in_select] or of the [Plan] equivalents —
     the grammar admits only a SELECT there — so listing them changes nothing
     today.  It is listed rather than caught so that a NEW statement form that
     CAN appear there is a compile error here instead of the silent refusal
     #670, #721 and this issue are all instances of. *)
  | Ast.S_create_table _
  | Ast.S_insert _
  | Ast.S_insert_select _
  | Ast.S_create_index _
  | Ast.S_update _
  | Ast.S_delete _
  | Ast.S_drop_table _
  | Ast.S_drop_index _
  | Ast.S_alter_table _
  | Ast.S_begin
  | Ast.S_commit
  | Ast.S_rollback
  | Ast.S_savepoint _
  | Ast.S_release _
  | Ast.S_rollback_to _
  | Ast.S_create_fts_table _
  | Ast.S_pragma _
  | Ast.S_create_view _
  | Ast.S_create_reactive_view _
  | Ast.S_drop_view _
  | Ast.S_drop_reactive_view _
  | Ast.S_create_trigger _
  | Ast.S_drop_trigger _
  | Ast.S_explain _
  | Ast.S_vacuum
  | Ast.S_attach _
  | Ast.S_detach _ -> s

(** #732: rewrite a SELECT's projection.

    [`Cols] is a [string list], so it cannot hold the literal a substitution
    produces — but it is exactly the shape an {b unqualified} outer reference
    parses to ([SELECT k, (SELECT n FROM i WHERE i.fk = o.k) FROM o] when [i]
    has no [n]; the parser emits [`Cols] only when every item is a bare
    [E_col], parser.mly:897).  So a name the binding resolves promotes the whole
    projection to [`Exprs].

    The promotion is conditional on a substitution actually happening, which
    keeps two things true: an unchanged projection keeps the AST shape the rest
    of the engine sees today, and the probe binding
    {!stmt_has_free_column_ref} runs with — which resolves nothing — never
    reshapes the statement it is only supposed to inspect. *)
and substitute_outer_proj
      (go_e : Ast.expr -> Ast.expr)
      (proj : [ `All | `Cols of string list | `Exprs of (Ast.expr * string option) list ])
  : [ `All | `Cols of string list | `Exprs of (Ast.expr * string option) list ]
  =
  match proj with
  | `All -> proj
  | `Exprs items -> `Exprs (List.map (fun (e, a) -> go_e e, a) items)
  | `Cols names ->
    let subst = List.map (fun n -> n, go_e (Ast.E_col n)) names in
    let substituted (n, e') =
      match e' with
      | Ast.E_col m -> not (String.equal m n)
      | _ -> true
    in
    if List.exists substituted subst
    then `Exprs (List.map (fun (n, e') -> e', Some n) subst)
    else proj

(** Substitute outer column refs in any embedded Ast.stmt nodes inside a
    Plan.expr (correlated subqueries / EXISTS / IN). *)
and substitute_outer_in_plan_expr
      ~(cat : Cat.t option)
      (bnd : outer_binding)
      (e : Plan.expr)
  : Plan.expr
  =
  let go = substitute_outer_in_plan_expr ~cat bnd in
  let go_s = substitute_outer_in_stmt ~cat ~enclosing:no_inner_scope bnd in
  match e with
  | Plan.P_exists inner -> Plan.P_exists (go_s inner)
  | Plan.P_in_select (x, inner) -> Plan.P_in_select (go x, go_s inner)
  | Plan.P_subquery inner -> Plan.P_subquery (go_s inner)
  | Plan.P_binop (op, a, b) -> Plan.P_binop (op, go a, go b)
  | Plan.P_not a -> Plan.P_not (go a)
  | Plan.P_is_null a -> Plan.P_is_null (go a)
  | Plan.P_is_not_null a -> Plan.P_is_not_null (go a)
  | Plan.P_neg a -> Plan.P_neg (go a)
  | Plan.P_bitnot a -> Plan.P_bitnot (go a)
  | Plan.P_between (x, lo, hi) -> Plan.P_between (go x, go lo, go hi)
  | Plan.P_in (x, vs) -> Plan.P_in (go x, List.map go vs)
  | Plan.P_func (f, args) -> Plan.P_func (f, List.map go args)
  | Plan.P_case { scrutinee; branches; else_ } ->
    Plan.P_case
      { scrutinee = Option.map go scrutinee
      ; branches = List.map (fun (c, r) -> go c, go r) branches
      ; else_ = Option.map go else_
      }
  | Plan.P_cast (e, ty) -> Plan.P_cast (go e, ty)
  (* #670: this arm is the fix, and its absence was the bug.  This is one of
     four walkers over [Plan.expr] that look for subquery-bearing nodes;
     {!plan_expr_has_subquery}, {!plan_expr_subqueries_use_param} and
     {!plan_expr_embedded_stmts} all descended into [P_collate] and this one did
     not.  So for [x = (SELECT …) COLLATE NOCASE] the other three agreed the
     subquery was correlated while the one that would have RESOLVED it left the
     outer reference in place — the statement then failed [Sema.bind] and was
     refused by [correlated_filter_refusal].  A refusal, not a wrong answer, but
     for a query the engine is perfectly able to run.

     #493 wrote the arm and then reverted it deliberately: it turns a refusal
     into rows, and that PR shipped in a batch that could not be built, so it
     could not carry the test such a change needs.  The test exists now
     ([test/test_collate_outer_ref_670.ml]) and pins the rows. *)
  | Plan.P_collate (e, c) -> Plan.P_collate (go e, c)
  (* #670: exhaustive, not [| _ ->].  The catch-all is what let the missing arm
     above be invisible for as long as it was, in the one walker of four where
     it changed an answer — see {!plan_expr_has_subquery}. *)
  | Plan.P_lit _
  | Plan.P_col _
  | Plan.P_param _
  | Plan.P_excluded_col _
  | Plan.P_window_slot _ -> e
;;

(** #635: does [s] carry a {b free} column reference — a name no scope inside
    [s] owns, which can therefore only come from an enclosing query?

    That is the definition of "this subquery is correlated", and it is the
    question {!Sema.bind} cannot answer. [Sema] treats [E_subquery] / [E_exists]
    / [E_in_select] as opaque leaves: it never descends into a nested subquery,
    so a reference buried TWO levels down is invisible to it and the enclosing
    statement binds cleanly. The callers below read "it bound" as "it is
    uncorrelated" and evaluate it eagerly — before any outer row exists to
    substitute from. The reference is then met for the first time by the
    intermediate query's own [stream_filter], whose inputs are the intermediate
    FROM, and refused there (#592) because the outermost input is nowhere in
    sight.

    That is what made
    [... FROM l AS x WHERE EXISTS (SELECT 1 FROM r WHERE EXISTS (SELECT 1 FROM k
    WHERE v < x.a))] a refusal even after #635 taught
    {!substitute_outer_in_expr} to descend through nesting: the descent was
    correct but never ran, because nothing had classified the middle subquery as
    correlated.

    It is deliberately implemented by RUNNING {!substitute_outer_in_stmt} with a
    binding that resolves nothing and records that it was asked. The detector
    and the substituter therefore agree by construction about which references
    are free — the same discipline the two binders are held to. A hand-written
    second walker would be one more member of a set that already disagrees
    (#670).

    It can only ever move a statement from "evaluate eagerly" to "treat as
    correlated": when it answers [false] the caller does exactly what it did
    before, and when a statement it flags turns out to have no resolvable outer
    source, the refusal it eventually raises is the one that was raised before.
    [inner_scope_of] answers "owned" for everything it cannot resolve, so an
    unresolvable FROM never manufactures a free reference.

    #721/#732 widened {!substitute_outer_in_stmt} — into aggregate and window
    arguments, into a SELECT's projection, and into a FROM-less
    [S_const_select] — and this detector therefore widened with it, by
    construction rather than by a matching edit. That is the intended
    consequence and it is bounded by the paragraph above: the statements it
    newly flags are exactly the ones whose outer reference the substituter can
    now resolve, and any it flags without resolving reach the same refusal as
    before.

    The probe is also why the [`Cols] promotion in {!substitute_outer_proj} is
    conditional: a probe binding resolves nothing, so no name changes, so the
    projection keeps its shape and this function inspects without rewriting. *)
let stmt_has_free_column_ref (cat : Cat.t option) (s : Ast.stmt) : bool =
  let seen = ref false in
  let probe : outer_binding =
    { bind_qual =
        (fun _ _ ->
          seen := true;
          None)
    ; bind_unqual =
        (fun _ ->
          seen := true;
          None)
    }
  in
  ignore (substitute_outer_in_stmt ~cat ~enclosing:no_inner_scope probe s : Ast.stmt);
  !seen
;;

let rec substitute_cte ~(cte_name : string) ~(rows : Row.t list) (op : Plan.op) : Plan.op =
  let go = substitute_cte ~cte_name ~rows in
  match op with
  | Plan.Op_cte_scan { cte_name = n; _ } when String.equal n cte_name ->
    Plan.Op_pragma_rows { rows }
  | Plan.Op_filter r -> Plan.Op_filter { r with child = go r.child }
  | Plan.Op_project r -> Plan.Op_project { r with child = go r.child }
  | Plan.Op_expr_project r -> Plan.Op_expr_project { r with child = go r.child }
  | Plan.Op_sort r -> Plan.Op_sort { r with child = go r.child }
  | Plan.Op_limit r -> Plan.Op_limit { r with child = go r.child }
  | Plan.Op_distinct r -> Plan.Op_distinct { child = go r.child }
  | Plan.Op_aggregate r -> Plan.Op_aggregate { r with child = go r.child }
  | Plan.Op_nested_loop_join r -> Plan.Op_nested_loop_join { r with left = go r.left }
  | Plan.Op_hash_join r ->
    Plan.Op_hash_join { r with left = go r.left; right = go r.right }
  | Plan.Op_union r -> Plan.Op_union { r with left = go r.left; right = go r.right }
  | Plan.Op_intersect r -> Plan.Op_intersect { left = go r.left; right = go r.right }
  | Plan.Op_except r -> Plan.Op_except { left = go r.left; right = go r.right }
  | Plan.Op_with_cte r when not (String.equal r.cte_name cte_name) ->
    Plan.Op_with_cte { r with query = go r.query }
  | Plan.Op_window r -> Plan.Op_window { r with child = go r.child }
  | Plan.Op_insert_select ({ source; _ } as r) ->
    Plan.Op_insert_select { r with source = go source }
  | _ -> op
;;

(* One [S.get] per match against the doc-length region.  #689's starting point,
   and still the right strategy for a SELECTIVE match set — see
   [fts_doclen_scan_ratio]. *)
let fts_doclen_by_get tx (fts_meta : Cat.fts_table_meta) matches =
  Lwt_list.map_s
    (fun (rowid, positions) ->
       let* v = S.get tx fts_meta.Cat.fts_index_tree (fts_doclen_key rowid) in
       Lwt.return (rowid, positions, fts_decode_doclen v))
    matches
;;

(* #689 option 2: ONE cursor walk across the doc-length region between the
   lowest and highest matched rowid, instead of one root-to-leaf [S.get] descent
   per match.  Produces exactly the same [(rowid, positions, doc_length)] triples
   as {!fts_doclen_by_get} — same keys, same value decoder, same "absent means
   1" default — so the two are interchangeable and the caller picks on cost
   alone.  The walk stops at the first key that is not a doc-length key (the
   region is contiguous, see [fts_doclen_prefix]) or that is past [hi]. *)
let fts_doclen_by_scan tx (fts_meta : Cat.fts_table_meta) ~lo ~hi matches =
  let n = List.length matches in
  let want : (int64, unit) Hashtbl.t = Hashtbl.create n in
  List.iter (fun (r, _) -> Hashtbl.replace want r ()) matches;
  let found : (int64, int) Hashtbl.t = Hashtbl.create n in
  let* cur = S.seek_ge tx fts_meta.Cat.fts_index_tree (fts_doclen_key lo) in
  let rec walk () =
    let* e = S.seek_next cur in
    match e with
    | None -> Lwt.return_unit
    | Some (k, v) -> step k v
  and step k v =
    match fts_doclen_key_rowid k with
    | Some r when Int64.compare r hi <= 0 ->
      if Hashtbl.mem want r then Hashtbl.replace found r (fts_decode_doclen (Some v));
      walk ()
    | _ -> Lwt.return_unit
  in
  let* () = walk () in
  S.seek_close cur;
  Lwt.return
    (List.map
       (fun (rowid, positions) ->
          rowid, positions, Option.value ~default:1 (Hashtbl.find_opt found rowid))
       matches)
;;

(** #689 selectivity threshold: take the cursor walk when the doc-length region
    it would cross holds at most this many entries per match.

    Measured on this repo's B-tree backend (4 000-document FTS table, one term
    matching every document, [Gc.minor_words] delta around the query): a point
    [S.get] for one doc length costs ~573 minor words, a [seek_next] step ~112 —
    so the walk wins while it crosses fewer than ~5.1 entries per match.  5 is
    the conservative integer below that.  Both sides of the trade are real: at
    3 matches out of 4 000 documents an ungated walk cost 642 663 minor words
    against 19 325 for the point gets (33x worse), and at 4 000 matches out of
    4 000 the walk cost 2 555 506 against 4 399 003 (1.7x better).

    {!set_fts_doclen_scan_ratio} exists so a test can force EITHER path over the
    SAME data and prove they agree; production never calls it.  [0] disables the
    walk entirely. *)
let fts_doclen_scan_ratio_ref = ref 5

let fts_doclen_scan_ratio () = !fts_doclen_scan_ratio_ref
let set_fts_doclen_scan_ratio n = fts_doclen_scan_ratio_ref := n

(* Fetch every match's doc length, picking the cheaper strategy.  The estimate
   costs no I/O: [total_docs] is already in hand from [read_fts_stats] and the
   rowid bounds are a fold over an in-memory list.  [span] is an UPPER bound on
   the entries a walk would cross — the region holds one entry per document, and
   every entry between [lo] and [hi] has a distinct rowid in that range — so the
   walk is chosen only when even its worst case is cheaper. *)
let fts_doc_lengths tx (fts_meta : Cat.fts_table_meta) ~total_docs matches =
  let n = List.length matches in
  if n = 0
  then Lwt.return []
  else (
    let lo, hi =
      List.fold_left
        (fun (lo, hi) (r, _) -> Int64.min lo r, Int64.max hi r)
        (Int64.max_int, Int64.min_int)
        matches
    in
    let by_rowid =
      let d = Int64.sub hi lo in
      if Int64.compare d 0L < 0 || Int64.compare d (Int64.of_int max_int) >= 0
      then max_int
      else Int64.to_int d + 1
    in
    let span = if total_docs > 0 then min by_rowid total_docs else by_rowid in
    let ratio = !fts_doclen_scan_ratio_ref in
    if ratio > 0 && span <= ratio * n
    then fts_doclen_by_scan tx fts_meta ~lo ~hi matches
    else fts_doclen_by_get tx fts_meta matches)
;;

(* BM25-score FTS [matches] against [query] when rank is requested; otherwise
   tag each with score 0.0.  Each term's own per-doc term-frequency is used.
   Standalone (not in the [to_stream] rec group) so it stays polymorphic in the
   txn kind — #262 calls it with either a borrowed RW txn or a fresh RO snap. *)
let fts_score_matches tx (fts_meta : Cat.fts_table_meta) query matches include_rank =
  if not include_rank
  then Lwt.return (List.map (fun (rowid, positions) -> rowid, positions, 0.0) matches)
  else
    let* total_docs, total_tokens = read_fts_stats tx fts_meta.Cat.fts_index_tree in
    let query_terms = fts_query_terms query in
    (* #689: the score fold below asks each term's posting list for ONE rowid's
       term frequency, once per match.  Over an association list that is a linear
       probe, making the fold O(matches x postings x terms) — quadratic in the
       match count, with no allocation to show for it, and the dominant cost of
       this whole function by a wide margin (measured: a 4 000-match single-term
       rank query went 218 ms -> 9.6 ms on the in-memory backend when this became
       a hashtable, a 22.7x drop, while the per-match [S.get] #689 was filed about
       accounted for ~0.9 ms of the 218).  The table is built from the reversed
       list with [Hashtbl.replace] so the FIRST entry for a rowid wins, exactly as
       [List.assoc_opt] did; posting lists carry one entry per rowid by
       construction, so this only matters if that ever stops being true. *)
    let* term_data =
      Lwt_list.map_s
        (fun term ->
           let* pl = fts_posting_list tx ~index_tree:fts_meta.Cat.fts_index_tree term in
           let by_rowid : (int64, (int * int) list) Hashtbl.t =
             Hashtbl.create (List.length pl)
           in
           List.iter (fun (r, ps) -> Hashtbl.replace by_rowid r ps) (List.rev pl);
           Lwt.return (List.length pl, by_rowid))
        query_terms
    in
    (* #687 review finding 1: every match needs its own [doc_length] — BM25 uses
       it to normalize term frequency — and this branch runs exactly when the
       caller wants results sorted by rank, which needs every score before any
       LIMIT/OFFSET window can be chosen.  So unlike #687's content fetch this
       cannot be truncated to [offset, offset+limit); what #689 removes instead
       is the per-match ROUND TRIP, by walking the doc-length region with one
       cursor when the match set is dense enough for that to pay.  Storing
       [doc_length] inline with each posting entry would remove the second key
       region altogether, but that is an on-disk format change with a migration
       story of its own and is NOT what this does. *)
    let* doc_lengths = fts_doc_lengths tx fts_meta ~total_docs matches in
    let scored =
      List.map
        (fun (rowid, positions, dl) ->
           let score =
             List.fold_left
               (fun acc (n_docs, term_pl) ->
                  let tf =
                    match Hashtbl.find_opt term_pl rowid with
                    | None -> 0
                    | Some pos -> List.length pos
                  in
                  acc
                  +. bm25_score
                       ~k1:1.2
                       ~b:0.75
                       ~total_docs
                       ~total_tokens
                       ~n_docs_with_term:n_docs
                       ~term_freq:tf
                       ~doc_length:dl)
               0.0
               term_data
           in
           rowid, positions, score)
        doc_lengths
    in
    Lwt.return scored
;;

(** #493: bind and plan a subquery's [Ast.stmt], reusing the plan when this
    query has already planned the same statement.

    This is the "plan once, execute N times" half of #493. Before it, a
    correlated subquery paid a full {!Sema.bind} + {!Planner.plan} — catalog
    resolution, type checking, index-candidate enumeration — on every outer row.
    The cache is installed by {!with_pull_context} only at the call sites that
    substitute outer references as PARAMETERS, because only there is the
    statement identical across rows; with no cache in scope this degrades to the
    previous per-call bind+plan and nothing else changes.

    [None] means the statement cannot stand on its own, which every caller
    reports by leaving its expression unresolved, exactly as before.

    #635: "cannot stand on its own" is two questions, and {!Sema.bind} answers
    only the first. It does not descend into a nested subquery, so a statement
    whose correlation sits TWO levels down binds cleanly and would be evaluated
    eagerly, before any outer row exists to substitute from — see
    {!stmt_has_free_column_ref}, which answers the second. Both are asked here,
    at the one chokepoint all four callers share, so
    {!refuse_unresolved_correlation} and the three [eval_*_subquery] functions
    cannot disagree about which statements are correlated.

    The FAILURE is memoized too (the cache holds [Plan.op option], not
    [Plan.op]). A statement that will not bind is the #592 unresolvable
    correlation, and re-running {!Sema.bind} on it once per outer row only to
    reach the same refusal is pure waste. {!refuse_unresolved_correlation}
    normally raises on the first row, but the negative entry keeps the cost
    bounded on any path that does not. *)
let plan_subquery_cached (cat : Cat.t) (inner_ast : Ast.stmt) : Plan.op option Lwt.t =
  let cache = Lwt.get subplan_cache_key in
  match Option.bind cache (fun tbl -> Hashtbl.find_opt tbl inner_ast) with
  | Some cached -> Lwt.return cached
  | None ->
    let* result =
      if stmt_has_free_column_ref (Some cat) inner_ast
      then Lwt.return None
      else (
        (* Counted here and nowhere else: this is the branch a cache hit skips,
           so the counter measures exactly "how many times did we pay
           bind+plan". A statement rejected above pays neither. *)
        incr subquery_plans_built_ref;
        let* bound_r = Sema.bind cat inner_ast in
        Lwt.return
          (match bound_r with
           | Error _ -> None
           | Ok bound -> Some (Planner.plan ~cat bound)))
    in
    (match cache with
     | Some tbl -> Hashtbl.replace tbl inner_ast result
     | None -> ());
    Lwt.return result
;;

(** #493 review: decide #592's "an unresolvable correlation is refused, not
    silently answered" ONCE, over EVERY correlated conjunct, before any row is
    filtered.

    The AND short-circuit introduced by #493 made that invariant
    data-dependent: [correlated_row_passes] returns [false] the moment a
    conjunct is not truthy, so a refusal sitting in a LATER conjunct was only
    reached for rows that passed every earlier one. On TPC-H Q4's own shape —
    a cheap date restriction written before the [EXISTS] —
    [WHERE a.x = -1 AND EXISTS (<unresolvable>)] with no row satisfying
    [a.x = -1] returned an empty result set and no error at all. That is
    precisely the "empty result that reads as a legitimate nothing-matched"
    #592 removed, reintroduced through the back door.

    Resolvability is a property of the PLAN, not of the row: the substitution
    decides what to pin from the column NAMES in [metas] and [inner_scope_of]
    plus the row's WIDTH, none of which vary across the rows of one scan — only
    the pinned VALUES do. So one probe, on the first row pulled, settles it for
    the whole stream, and the short-circuit can then never suppress a refusal.

    The probe binds and plans; it does not execute. It also warms the plan
    cache, so the row that triggers it pays nothing extra. An empty child
    stream probes nothing and raises nothing — as it did before #493, since
    [Lwt_stream.filter_s] over an empty stream never ran the check either. *)
let refuse_unresolved_correlation (cat : Cat.t option) (substituted : Plan.expr list)
  : unit Lwt.t
  =
  match cat with
  | None -> Lwt.return_unit
  | Some c ->
    Lwt_list.iter_s
      (fun s ->
         let* op = plan_subquery_cached c s in
         if Option.is_none op
         then Lwt.fail_with (correlated_filter_refusal ())
         else Lwt.return_unit)
      (List.concat_map plan_expr_embedded_stmts substituted)
;;

(** #493: run a subquery's stream under a read transaction the CALLER owns, and
    end it unconditionally.

    This is the fix for the super-linear term the issue observed, and it is not
    a cost optimisation so much as a leak repair. A leaf scanner in [Auto] mode
    opens its own RO snapshot ([rh_begin] → [S.ro_begin]) and releases it from
    the stream's [finish], which runs only when the stream is drained to
    exhaustion or raises. An [EXISTS] subquery stops at the first row and
    abandons the rest, so [finish] never ran: [S.ro_end] — and with it
    [Pager.unpin_all] over that snapshot's pinned pages, the [active_readers]
    decrement and [Rwlock.release_read] — was skipped once per outer row. The
    pins are what make the cost super-linear: every page the abandoned scan
    touched stays un-evictable for the rest of the query, so the pager's cache
    grows monotonically with the number of outer rows and stops being a cache.
    Blocked checkpoints and a read-lock count that never returns to zero are the
    same bug's other two faces.

    Opening the snapshot here instead makes the leaf scanners {i borrow} it
    ([RH_borrowed_ro], whose [rh_finish] is a no-op), so abandoning the stream
    releases nothing and [Lwt.finalize] releases everything.

    #262 is preserved: when an explicit transaction is already active the
    subquery keeps reading through it, so read-your-own-writes is unchanged and
    the caller's transaction is never ended here. *)
let with_subquery_txn (store : S.t) (f : txn_mode -> 'a Lwt.t) : 'a Lwt.t =
  match current_txn_mode () with
  | (In_txn _ | In_ro_txn _) as m -> f m
  | Auto ->
    let* tx = S.ro_begin store in
    Lwt.finalize (fun () -> f (In_ro_txn tx)) (fun () -> S.ro_end tx)
;;

let rec pre_eval_subquery
          (clock : (unit -> float) option)
          (store : S.t)
          (params : Row.value array)
          (cat_opt : Cat.t option)
          (e : Plan.expr)
  : Plan.expr Lwt.t
  =
  match e with
  | Plan.P_subquery inner_ast ->
    eval_scalar_subquery clock store params cat_opt e inner_ast
  | Plan.P_exists inner_ast -> eval_exists_subquery clock store params cat_opt e inner_ast
  | Plan.P_in_select (x, inner_ast) ->
    eval_in_select clock store params cat_opt e x inner_ast
  | Plan.P_binop (op, a, b) ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    let* b' = pre_eval_subquery clock store params cat_opt b in
    Lwt.return (Plan.P_binop (op, a', b'))
  | Plan.P_not a ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    Lwt.return (Plan.P_not a')
  | Plan.P_is_null a ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    Lwt.return (Plan.P_is_null a')
  | Plan.P_is_not_null a ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    Lwt.return (Plan.P_is_not_null a')
  | Plan.P_neg a ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    Lwt.return (Plan.P_neg a')
  | Plan.P_bitnot a ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    Lwt.return (Plan.P_bitnot a')
  | Plan.P_between (x, lo, hi) ->
    let* x' = pre_eval_subquery clock store params cat_opt x in
    let* lo' = pre_eval_subquery clock store params cat_opt lo in
    let* hi' = pre_eval_subquery clock store params cat_opt hi in
    Lwt.return (Plan.P_between (x', lo', hi'))
  | Plan.P_in (x, vals) ->
    let* x' = pre_eval_subquery clock store params cat_opt x in
    let* vals' = Lwt_list.map_s (pre_eval_subquery clock store params cat_opt) vals in
    Lwt.return (Plan.P_in (x', vals'))
  | Plan.P_func (f, args) ->
    let* args' = Lwt_list.map_s (pre_eval_subquery clock store params cat_opt) args in
    Lwt.return (Plan.P_func (f, args'))
  | Plan.P_case { scrutinee; branches; else_ } ->
    let* scrutinee' =
      match scrutinee with
      | None -> Lwt.return None
      | Some e ->
        let+ e' = pre_eval_subquery clock store params cat_opt e in
        Some e'
    in
    let* branches' =
      Lwt_list.map_s
        (fun (cond, res) ->
           let* cond' = pre_eval_subquery clock store params cat_opt cond in
           let+ res' = pre_eval_subquery clock store params cat_opt res in
           cond', res')
        branches
    in
    let+ else_' =
      match else_ with
      | None -> Lwt.return None
      | Some e ->
        let+ e' = pre_eval_subquery clock store params cat_opt e in
        Some e'
    in
    Plan.P_case { scrutinee = scrutinee'; branches = branches'; else_ = else_' }
  | Plan.P_cast (e, ty) ->
    let* e' = pre_eval_subquery clock store params cat_opt e in
    Lwt.return (Plan.P_cast (e', ty))
  | Plan.P_collate (e, c) ->
    let* e' = pre_eval_subquery clock store params cat_opt e in
    Lwt.return (Plan.P_collate (e', c))
  (* Leaves — nothing inside to fold.  Exhaustive rather than [| _ ->]: this
     walker sits directly on the correlated-subquery path (it is what folds
     [P_subquery] / [P_exists] / [P_in_select], handled above), so a newly added
     constructor falling through it silently is precisely the failure mode #670
     is about. *)
  | Plan.P_lit _
  | Plan.P_col _
  | Plan.P_param _
  | Plan.P_excluded_col _
  | Plan.P_window_slot _ -> Lwt.return e

(* Scalar subquery: run [inner_ast], yield its first column's first value as a
   literal (NULL if empty); returns [e] unchanged if it fails to bind. *)
and eval_scalar_subquery clock store params cat_opt (e : Plan.expr) inner_ast
  : Plan.expr Lwt.t
  =
  match cat_opt with
  | None -> Lwt.return (Plan.P_lit Ast.L_null)
  | Some cat ->
    let* op_r = plan_subquery_cached cat inner_ast in
    (match op_r with
     | None -> Lwt.return e
     | Some op ->
       (* #262: run the subquery under the active txn (read-your-own-writes);
          #493: or under one this call owns and ends. *)
       with_subquery_txn store (fun mode ->
         let* stream = to_stream clock params store ~mode ~cat:(Some cat) op in
         (* #493: a scalar subquery needs only its first row, and every stream
            it pulls from is lazy, so [get] stops the inner scan there. Safe to
            abandon the rest only because [with_subquery_txn] owns the snapshot
            the abandoned stream would otherwise have leaked. *)
         let* first = Lwt_stream.get stream in
         let v =
           match first with
           | Some row when Array.length row >= 1 -> value_to_literal row.(0)
           | _ -> Ast.L_null
         in
         Lwt.return (Plan.P_lit v)))

(* #674 (item 3 of 3): unwrap the layout-only nodes a trivial subquery like
   [SELECT 1 FROM t WHERE ...] plans through — projection, distinct, sort —
   none of which change whether the subquery yields at least one row. Stops
   at anything that could (a filter, join, aggregate, ...), so the caller
   only takes the covering-existence shortcut for the exact shape it knows
   how to answer without a table fetch.

   [Op_limit] is deliberately NOT unwrapped here (#684 review): "the index
   has a matching entry" is only equivalent to EXISTS's answer when the
   LIMIT is >= 1 rows and no OFFSET is skipping past the match — a LIMIT 0
   must always answer false, and a nonzero OFFSET can turn an existing match
   into a false answer too. Rather than special-case offset=0/limit>=1, we
   simply stop here: a LIMIT over the existence subquery falls through to
   the general (always-correct) streaming path below instead of taking the
   fast path. *)
and unwrap_for_existence : Plan.op -> Plan.op = function
  | Plan.Op_project { child; _ }
  | Plan.Op_expr_project { child; _ }
  | Plan.Op_distinct { child }
  | Plan.Op_sort { child; _ } -> unwrap_for_existence child
  | op -> op

(* #674 (item 3 of 3): does the equality-bound (+ optional #517 range) prefix
   [keys]/[range] match at least one entry of [idx_tree]?  No column is ever
   read — existence needs only "did the seek find a qualifying key", so unlike
   [run_index_cover_walk] this needs no NOT-NULL gate at all: NULL/NaN
   ambiguity only matters when a VALUE is read back, and none is here.  Opens
   and closes its own read handle synchronously (no stream, nothing to
   abandon), so this carries none of the #493 leak risk [eval_exists_subquery]
   already reasons about for the general path. *)
and index_lookup_exists clock params store mode idx_tree keys range =
  let vs = List.map (fun (_, ty, e) -> eval_expr clock params [||] e, ty) keys in
  match index_lookup_values vs with
  | None -> Lwt.return false
  | Some lookup_vs ->
    let prefix, plen = encode_index_key_prefix lookup_vs in
    let seek_key, past_end = range_seek_bounds clock params ~prefix ~plen range in
    let* rh = rh_begin store mode in
    let* cur = rh_seek_ge rh idx_tree seek_key in
    let s_opt = Lwt.get query_stats_key in
    let* kv = S.seek_next cur in
    S.seek_close cur;
    let* () = rh_finish rh in
    (match kv with
     | Some (ikey, _ivalue) when index_key_in_range ~prefix ~plen ~past_end ikey ->
       incr_index_entries s_opt;
       Lwt.return true
     | _ -> Lwt.return false)

(* EXISTS subquery: 1 if [inner_ast] yields any row, else 0. *)
and eval_exists_subquery clock store params cat_opt (e : Plan.expr) inner_ast
  : Plan.expr Lwt.t
  =
  match cat_opt with
  | None -> Lwt.return (Plan.P_lit (Ast.L_int 0L))
  | Some cat ->
    let* op_r = plan_subquery_cached cat inner_ast in
    (match op_r with
     | None -> Lwt.return e
     | Some op ->
       (match unwrap_for_existence op with
        | Plan.Op_index_lookup { idx_tree; keys; range; _ } ->
          (* #674 (item 3 of 3): the single remaining waste in the
             already-#493-safe EXISTS path was the one [rh_get] its one
             pulled row still paid.  Existence needs only the seek. *)
          with_subquery_txn store (fun mode ->
            let* found =
              index_lookup_exists clock params store mode idx_tree keys range
            in
            Lwt.return (Plan.P_lit (Ast.L_int (if found then 1L else 0L))))
        | _ ->
          (* #262 / #493, as in [eval_scalar_subquery]. EXISTS pulls exactly
             one row and abandons the stream; the owned snapshot is what
             makes that safe rather than a per-outer-row page-pin leak. *)
          with_subquery_txn store (fun mode ->
            let* stream = to_stream clock params store ~mode ~cat:(Some cat) op in
            let* first = Lwt_stream.get stream in
            Lwt.return (Plan.P_lit (Ast.L_int (if first = None then 0L else 1L))))))

(* IN (subquery): materialize [inner_ast]'s first column into the IN value list. *)
and eval_in_select clock store params cat_opt (e : Plan.expr) x inner_ast
  : Plan.expr Lwt.t
  =
  match cat_opt with
  | None -> Lwt.return (Plan.P_in (x, []))
  | Some cat ->
    let* op_r = plan_subquery_cached cat inner_ast in
    (match op_r with
     | None -> Lwt.return e
     | Some op ->
       (* #262 / #493. This one drains the stream, so it never leaked; it takes
          the owned snapshot for the same reason the others do — one place
          decides how a subquery gets its read transaction. *)
       let* rows =
         with_subquery_txn store (fun mode ->
           let* stream = to_stream clock params store ~mode ~cat:(Some cat) op in
           Lwt_stream.to_list stream)
       in
       let vals =
         List.filter_map
           (fun row ->
              if Array.length row >= 1
              then Some (Plan.P_lit (value_to_literal row.(0)))
              else None)
           rows
       in
       let* x' = pre_eval_subquery clock store params cat_opt x in
       Lwt.return (Plan.P_in (x', vals)))

(* ------------------------------------------------------------------ *)
(* Window function helpers                                              *)
(* ------------------------------------------------------------------ *)

and eval_partition_key clock params (row : Row.t) (partition_by : Plan.expr list)
  : Row.value list
  =
  (* #722: a partition key is compared, never emitted — see {!eval_sort_key}. *)
  List.map (eval_sort_key clock params row) partition_by

and partition_keys_equal (a : Row.value list) (b : Row.value list) : bool =
  List.length a = List.length b && List.for_all2 (fun x y -> compare_values x y = 0) a b

and group_by_partition
      clock
      params
      (partition_by : Plan.expr list)
      (indexed_rows : (int * Row.t) list)
  : (Row.value list * (int * Row.t) list) list
  =
  List.fold_left
    (fun acc (idx, row) ->
       let key = eval_partition_key clock params row partition_by in
       match List.find_opt (fun (k, _) -> partition_keys_equal k key) acc with
       | Some _ ->
         List.map
           (fun (k, pairs) ->
              if partition_keys_equal k key then k, pairs @ [ idx, row ] else k, pairs)
           acc
       | None -> acc @ [ key, [ idx, row ] ])
    []
    indexed_rows

and sort_partition_by
      clock
      params
      (order_by : (Plan.expr * [ `Asc | `Desc ] * [ `Nulls_first | `Nulls_last ]) list)
      (indexed_rows : (int * Row.t) list)
  : (int * Row.t) list
  =
  if order_by = []
  then indexed_rows
  else
    List.sort
      (fun (_, ra) (_, rb) ->
         let rec cmp = function
           | [] -> 0
           | (e, dir, nulls) :: rest ->
             let va = eval_sort_key clock params ra e in
             let vb = eval_sort_key clock params rb e in
             let c = compare_with_nulls dir nulls va vb in
             if c <> 0 then c else cmp rest
         in
         cmp order_by)
      indexed_rows

and win_rank
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  let cur_rank = ref 1 in
  for pos = 0 to n - 1 do
    if pos > 0
    then (
      let order_changed =
        List.exists
          (fun (e, dir, nulls) ->
             compare_with_nulls
               dir
               nulls
               (eval_sort_key clock params sorted_rows.(pos) e)
               (eval_sort_key clock params sorted_rows.(pos - 1) e)
             <> 0)
          wplan.Plan.order_by
      in
      if order_changed then cur_rank := pos + 1);
    results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int !cur_rank)
  done

and win_dense_rank
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  let cur_rank = ref 1 in
  for pos = 0 to n - 1 do
    if pos > 0
    then (
      let order_changed =
        List.exists
          (fun (e, dir, nulls) ->
             compare_with_nulls
               dir
               nulls
               (eval_sort_key clock params sorted_rows.(pos) e)
               (eval_sort_key clock params sorted_rows.(pos - 1) e)
             <> 0)
          wplan.Plan.order_by
      in
      if order_changed then incr cur_rank);
    results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int !cur_rank)
  done

and win_ntile
      clock
      params
      (wplan : Plan.window_plan_item)
      _sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  let n_buckets =
    match wplan.Plan.args with
    | [ e ] ->
      (match eval_expr clock params [||] e with
       | Row.V_int k -> Int64.to_int k
       | _ -> 1)
    | _ -> 1
  in
  let n_buckets = max 1 n_buckets in
  for pos = 0 to n - 1 do
    let bucket = (pos * n_buckets / n) + 1 in
    results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int bucket)
  done

and win_lag_lead
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  let is_lag = wplan.Plan.func = Ast.WF_lag in
  let offset =
    match wplan.Plan.args with
    | _ :: e :: _ ->
      (match eval_expr clock params [||] e with
       | Row.V_int k -> Int64.to_int k
       | _ -> 1)
    | _ -> 1
  in
  let default_expr =
    match wplan.Plan.args with
    | _ :: _ :: e :: _ -> Some e
    | _ -> None
  in
  for pos = 0 to n - 1 do
    let src_pos = if is_lag then pos - offset else pos + offset in
    let v =
      if src_pos >= 0 && src_pos < n
      then (
        match wplan.Plan.args with
        | e :: _ -> eval_expr clock params sorted_rows.(src_pos) e
        | [] -> Row.V_null)
      else (
        match default_expr with
        | Some e -> eval_expr clock params sorted_rows.(pos) e
        | None -> Row.V_null)
    in
    results.(sorted_orig_idxs.(pos)) <- v
  done

and win_first_value
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  let arg_expr =
    match wplan.Plan.args with
    | e :: _ -> e
    | [] -> failwith "FIRST_VALUE requires one argument"
  in
  let first_val =
    if n > 0 then eval_expr clock params sorted_rows.(0) arg_expr else Row.V_null
  in
  for pos = 0 to n - 1 do
    results.(sorted_orig_idxs.(pos)) <- first_val
  done

and win_nth_value
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  let arg_expr =
    match wplan.Plan.args with
    | e :: _ -> e
    | [] -> failwith "NTH_VALUE requires at least one argument"
  in
  let n_arg =
    match wplan.Plan.args with
    | _ :: e :: _ ->
      (match eval_expr clock params [||] e with
       | Row.V_int k -> Int64.to_int k
       | _ -> 1)
    | _ -> 1
  in
  for pos = 0 to n - 1 do
    let v =
      if n_arg >= 1 && n_arg <= pos + 1
      then eval_expr clock params sorted_rows.(n_arg - 1) arg_expr
      else Row.V_null
    in
    results.(sorted_orig_idxs.(pos)) <- v
  done

and win_percent_rank
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  (* PERCENT_RANK = peer_group_start / (n - 1).  Positional adjacency in the
     already-direction-sorted array, so DESC works without knowing direction. *)
  if n = 0
  then ()
  else (
    let peer_start = ref 0 in
    for pos = 0 to n - 1 do
      if pos > 0
      then (
        let order_changed =
          List.exists
            (fun (e, dir, nulls) ->
               compare_with_nulls
                 dir
                 nulls
                 (eval_sort_key clock params sorted_rows.(pos) e)
                 (eval_sort_key clock params sorted_rows.(pos - 1) e)
               <> 0)
            wplan.Plan.order_by
        in
        if order_changed then peer_start := pos);
      let pct =
        if n <= 1 then 0.0 else Float.of_int !peer_start /. Float.of_int (n - 1)
      in
      results.(sorted_orig_idxs.(pos)) <- Row.V_real pct
    done)

and win_cume_dist
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
  =
  (* CUME_DIST = (last position in peer group + 1) / n.  Positional adjacency
     in the already-direction-sorted array, so DESC works correctly. *)
  if n = 0
  then ()
  else (
    let pos = ref 0 in
    while !pos < n do
      let peer_end = ref !pos in
      while
        !peer_end + 1 < n
        && List.for_all
             (fun (e, dir, nulls) ->
                compare_with_nulls
                  dir
                  nulls
                  (eval_sort_key clock params sorted_rows.(!peer_end + 1) e)
                  (eval_sort_key clock params sorted_rows.(!peer_end) e)
                = 0)
             wplan.Plan.order_by
      do
        incr peer_end
      done;
      let cd = Float.of_int (!peer_end + 1) /. Float.of_int n in
      for i = !pos to !peer_end do
        results.(sorted_orig_idxs.(i)) <- Row.V_real cd
      done;
      pos := !peer_end + 1
    done)

(* Compute one aggregate-window value over the rows in [indices]. *)
and win_agg_over_frame
      agg_func
      (arg_expr : Plan.expr option)
      (arg_vals : Row.value array)
      indices
  : Row.value
  =
  match agg_func with
  | Ast.Agg_count ->
    let cnt =
      if arg_expr = None
      then List.length indices
      else List.length (List.filter (fun i -> not (arg_vals.(i) = Row.V_null)) indices)
    in
    Row.V_int (Int64.of_int cnt)
  | Ast.Agg_sum ->
    List.fold_left
      (fun acc i ->
         match acc, arg_vals.(i) with
         | _, Row.V_null -> acc
         | Row.V_null, v -> v
         | Row.V_int a, Row.V_int b -> Row.V_int (Int64.add a b)
         | Row.V_real a, Row.V_real b -> Row.V_real (a +. b)
         | Row.V_int a, Row.V_real b -> Row.V_real (Int64.to_float a +. b)
         | Row.V_real a, Row.V_int b -> Row.V_real (a +. Int64.to_float b)
         | _, _ -> acc)
      Row.V_null
      indices
  | Ast.Agg_avg ->
    let vals =
      List.filter_map
        (fun i ->
           match arg_vals.(i) with
           | Row.V_int n -> Some (Int64.to_float n)
           | Row.V_real f -> Some f
           | _ -> None)
        indices
    in
    if vals = []
    then Row.V_null
    else Row.V_real (List.fold_left ( +. ) 0.0 vals /. float_of_int (List.length vals))
  | Ast.Agg_min ->
    List.fold_left
      (fun acc i ->
         match arg_vals.(i) with
         | Row.V_null -> acc
         | v ->
           (match acc with
            | Row.V_null -> v
            | acc_v -> if compare_values v acc_v < 0 then v else acc_v))
      Row.V_null
      indices
  | Ast.Agg_max ->
    List.fold_left
      (fun acc i ->
         match arg_vals.(i) with
         | Row.V_null -> acc
         | v ->
           (match acc with
            | Row.V_null -> v
            | acc_v -> if compare_values v acc_v > 0 then v else acc_v))
      Row.V_null
      indices
  | Ast.Agg_group_concat sep ->
    let separator = Option.value sep ~default:"," in
    let parts =
      List.filter_map
        (fun i ->
           match arg_vals.(i) with
           | Row.V_null -> None
           | Row.V_int n -> Some (Int64.to_string n)
           | Row.V_real f -> Some (Printf.sprintf "%.17g" f)
           | Row.V_text s -> Some s
           | Row.V_blob _ -> Some "")
        indices
    in
    if parts = [] then Row.V_null else Row.V_text (String.concat separator parts)

and win_aggregate
      clock
      params
      (wplan : Plan.window_plan_item)
      sorted_rows
      sorted_orig_idxs
      (results : Row.value array)
      n
      agg_func
  =
  let has_order = wplan.Plan.order_by <> [] in
  let arg_expr =
    match wplan.Plan.args with
    | e :: _ -> Some e
    | [] -> None
  in
  let arg_vals =
    Array.init n (fun pos ->
      match arg_expr with
      | Some e -> eval_expr clock params sorted_rows.(pos) e
      | None -> Row.V_null)
  in
  let resolve_bound bound pos =
    match bound with
    | Ast.FB_unbounded_preceding -> 0
    | Ast.FB_preceding k -> max 0 (pos - k)
    | Ast.FB_current_row -> pos
    | Ast.FB_following k -> min (n - 1) (pos + k)
    | Ast.FB_unbounded_following -> n - 1
  in
  for pos = 0 to n - 1 do
    let frame_start, frame_end =
      match wplan.Plan.frame with
      | None ->
        (* Default: UNBOUNDED PRECEDING AND CURRENT ROW with ORDER BY, else
           UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING. *)
        let fe = if has_order then pos else n - 1 in
        0, fe
      | Some spec ->
        (* RANGE numeric bounds approximated as ROWS — full value-based RANGE
           semantics not implemented. *)
        resolve_bound spec.Ast.start pos, resolve_bound spec.Ast.end_ pos
    in
    let frame_start = max 0 frame_start in
    let frame_end = min (n - 1) frame_end in
    let indices =
      if frame_start > frame_end
      then []
      else List.init (frame_end - frame_start + 1) (fun i -> frame_start + i)
    in
    results.(sorted_orig_idxs.(pos))
    <- win_agg_over_frame agg_func arg_expr arg_vals indices
  done

and compute_window_for_partition
      clock
      params
      (wplan : Plan.window_plan_item)
      (sorted_indexed : (int * Row.t) list)
      (n_total : int)
  : Row.value array
  =
  let results = Array.make n_total Row.V_null in
  let sorted_rows = Array.of_list (List.map snd sorted_indexed) in
  let sorted_orig_idxs = Array.of_list (List.map fst sorted_indexed) in
  let n = Array.length sorted_rows in
  (match wplan.Plan.func with
   | Ast.WF_row_number ->
     for pos = 0 to n - 1 do
       results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int (pos + 1))
     done
   | Ast.WF_rank -> win_rank clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_dense_rank ->
     win_dense_rank clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_ntile -> win_ntile clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_lag | Ast.WF_lead ->
     win_lag_lead clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_first_value ->
     win_first_value clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_last_value ->
     let arg_expr =
       match wplan.Plan.args with
       | e :: _ -> e
       | [] -> failwith "LAST_VALUE requires one argument"
     in
     for pos = 0 to n - 1 do
       results.(sorted_orig_idxs.(pos))
       <- eval_expr clock params sorted_rows.(pos) arg_expr
     done
   | Ast.WF_nth_value ->
     win_nth_value clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_percent_rank ->
     win_percent_rank clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_cume_dist ->
     win_cume_dist clock params wplan sorted_rows sorted_orig_idxs results n
   | Ast.WF_agg agg_func ->
     win_aggregate clock params wplan sorted_rows sorted_orig_idxs results n agg_func);
  results

and stream_seq_scan clock params store mode (table_meta : Cat.table_meta) =
  (* #239: captured at construction (inside [query]'s [with_value] scope). *)
  let s_opt = Lwt.get query_stats_key in
  (* #262: read through the active txn when one is open, so the scan observes
     the transaction's own uncommitted writes. *)
  let* rh = rh_begin store mode in
  (* #238: stream the leaves natively via [seek_ge ""] instead of
     [cursor_open], which drains the WHOLE tree into an OCaml list at open time
     (every (key,value) pair held simultaneously) before the first row is read.
     That eager drain — not value boxing — was the bulk of the scan pipeline's
     per-row allocation (~9.2 KB/row, vs ~1.5 KB at the streaming storage floor
     and ~0.6 KB for the row decode itself).  [seek_ge ""] descends in O(log n)
     and materialises only the entries actually pulled.  [Bytes.empty] is the
     minimum key, so the first [seek_next] returns the first row — matching the
     old [cursor_first]+[cursor_next] semantics. *)
  let seq_tree_id, _, _, _ = Cat.row_storage table_meta in
  let* cur = rh_seek_ge rh seq_tree_id Bytes.empty in
  (* Snapshot lifetime tied to the stream: end on exhaustion OR a mid-scan read
     error so a corrupt page can't leak locks/refcounts/pins (#164). Idempotent. *)
  let ended = ref false in
  let finish () =
    if !ended
    then Lwt.return_unit
    else (
      ended := true;
      S.seek_close cur;
      rh_finish rh)
  in
  register_stream_cleanup finish;
  let stream =
    Lwt_stream.from (fun () ->
      Lwt.catch
        (fun () ->
           (* #481: [seek_next_value], not [seek_next] — this scan binds the key
              as [_key] and drops it, and materialising it cost a [Bytes.create]
              + blit out of the leaf page on EVERY row of EVERY table scan. *)
           let* kv = S.seek_next_value cur in
           match kv with
           | None ->
             let%lwt () = finish () in
             Lwt.return_none
           | Some vbytes ->
             incr_examined s_opt;
             let row = decode_with_virtual clock params table_meta vbytes in
             Lwt.return_some row)
        (fun exn ->
           let%lwt () = finish () in
           Lwt.fail exn))
  in
  Lwt.return stream

and stream_col_seq_scan _clock _params _store _mode (table_meta : Cat.table_meta) =
  let col_store = col_store_of_meta table_meta in
  let seq = Granary_columnar.Col_store.to_row_seq col_store in
  Lwt.return (Lwt_stream.of_list (List.of_seq seq))

and stream_filter clock params store mode cat pred child =
  (* #257: a correlated subquery in the predicate is re-evaluated per row at
     pull time — outside [query]'s [with_value] scope — so capture the active
     stats here and re-establish the scope around the inner evaluation, letting
     the subquery's leaf scanners attribute their reads to this query. *)
  let s_opt = Lwt.get query_stats_key in
  let* child_stream = to_stream clock params store ~mode ~cat child in
  let* pred' = pre_eval_subquery clock store params cat pred in
  if not (plan_expr_has_subquery pred')
  then
    Lwt.return
      (Lwt_stream.filter
         (fun row -> value_truthy (eval_expr clock params row pred'))
         child_stream)
  else (
    (* #592: a subquery that survives [pre_eval_subquery] is correlated, and
       [eval_expr] answers [Row.V_null] for it. Until this commit an
       unresolvable correlation source fell to [Lwt_stream.filter (fun _ ->
       false)] — every row dropped, silently, which is how a correlated
       subquery in an INNER join's ON clause returned an empty result set that
       reads as a legitimate "nothing matched".

       [get_outer_scan_metas] now resolves the correlation source over a join
       (that was the whole of #566's and #592's blocker), so the common shapes
       evaluate. What is still unresolvable is refused rather than answered —
       and since #493 added the AND short-circuit below, that refusal is decided
       by {!refuse_unresolved_correlation} over EVERY conjunct on the first row
       pulled, not by whichever conjunct a given row happened to reach. Deciding
       it per row would have made the invariant data-dependent: a refusal behind
       a cheap restriction that no row satisfies would never fire, and the
       silent empty result would be back. *)
    match get_outer_scan_metas child with
    | None -> Lwt.fail_with (correlated_filter_refusal ())
    | Some metas ->
      (* #493, three changes, all inside this branch:

         - the predicate is split into its top-level AND conjuncts and evaluated
           left to right, stopping at the first that is not truthy, so a
           correlated subquery written after a cheap restriction runs only for
           the rows that restriction kept (TPC-H Q4 evaluated its EXISTS for
           every row of [orders], date range or not);
         - outer references are pinned as PARAMETERS rather than literals, which
           makes the substituted inner statement identical for every row;
         - which in turn lets one bind+plan per subquery site serve the whole
           scan, through the cache [with_pull_context] installs.

         The parameterized path is taken only when no inner statement carries
         parameters of its own — see {!ast_stmt_uses_param}. Otherwise this
         falls back to per-row literal substitution with no cache, which is
         exactly the pre-#493 behaviour, and the short-circuit still applies. *)
      let cs = plan_and_conjuncts pred' in
      let parameterized = not (List.exists plan_expr_subqueries_use_param cs) in
      let cache = if parameterized then Some (Hashtbl.create 4) else None in
      let base = Array.length params in
      let correlated_cs = List.filter plan_expr_has_subquery cs in
      (* Decided once, over every correlated conjunct — see
         {!refuse_unresolved_correlation}. The first row pulled settles it,
         because resolvability depends on the substituted NAMES and the row
         WIDTH, neither of which varies across one scan.

         The flag is set AFTER the probe resolves, not before. [filter_s] pulls
         strictly sequentially, so [keep] is never re-entered while a probe is
         in flight and either order works today — but setting it first would
         make the invariant depend on that sequencing, and a row sailing past a
         refusal that has not finished being decided is the failure this whole
         pre-pass exists to prevent.

         Cost note: on the parameterized path the probe runs against the SAME
         cache the rows then use, so it warms rather than duplicates. With
         [cache = None] (an inner statement carrying its own parameters) its
         bind+plan is discarded, and it is paid even for a conjunct the
         short-circuit would have skipped for every row. That is bounded by the
         number of subquery SITES, not by rows, so it does not reintroduce the
         per-row cost #493 removed. *)
      let refusal_decided = ref false in
      let decide_refusal bnd =
        if !refusal_decided
        then Lwt.return_unit
        else
          let* () =
            with_pull_context ~stats:s_opt ~mode ~cache (fun () ->
              refuse_unresolved_correlation
                cat
                (List.map (substitute_outer_in_plan_expr ~cat bnd) correlated_cs))
          in
          refusal_decided := true;
          Lwt.return_unit
      in
      let keep row =
        let bnd =
          if parameterized
          then param_binding_of_metas ~base ~row_len:(Array.length row) metas
          else binding_of_metas metas row
        in
        let row_params = if parameterized then Array.append params row else params in
        let* () = decide_refusal bnd in
        correlated_row_passes
          clock
          params
          store
          mode
          cat
          ~s_opt
          ~cache
          ~bnd
          ~row_params
          row
          cs
      in
      Lwt.return (Lwt_stream.filter_s keep child_stream))

(* #493: does [row] satisfy every conjunct?  Evaluated left to right in the
   order written — NOT reordered — and stopped at the first conjunct that is not
   truthy, so the subquery in a later conjunct is never run for a row an earlier
   one already rejected.

   This agrees with the pre-#493 [value_truthy] over the whole AND tree: row
   filtering is a two-valued decision (the row passes iff every conjunct is
   truthy), so a NULL conjunct rejects the row either way. *)
and correlated_row_passes
      clock
      params
      store
      mode
      cat
      ~s_opt
      ~cache
      ~bnd
      ~row_params
      (row : Row.t)
      conjuncts
  : bool Lwt.t
  =
  match conjuncts with
  | [] -> Lwt.return true
  | c :: rest ->
    let* ok =
      if not (plan_expr_has_subquery c)
      then Lwt.return (value_truthy (eval_expr clock params row c))
      else (
        let subst = substitute_outer_in_plan_expr ~cat bnd c in
        let* resolved =
          with_pull_context ~stats:s_opt ~mode ~cache (fun () ->
            pre_eval_subquery clock store row_params cat subst)
        in
        (* A backstop, not the guarantee. {!refuse_unresolved_correlation} has
           already decided the refusal over EVERY conjunct on the first row, so
           an unresolved subquery cannot reach here — which is the point: with
           only this check, whether a refusal fired depended on how many
           conjuncts the row got past. Kept because answering [V_null] for an
           unresolved subquery is the silent-wrong-answer failure #592 was
           about, and it should never be reachable by any route. *)
        if plan_expr_has_subquery resolved
        then Lwt.fail_with (correlated_filter_refusal ())
        else Lwt.return (value_truthy (eval_expr clock params row resolved)))
    in
    if not ok
    then Lwt.return false
    else
      correlated_row_passes
        clock
        params
        store
        mode
        cat
        ~s_opt
        ~cache
        ~bnd
        ~row_params
        row
        rest

and stream_expr_project clock params store mode cat exprs child =
  (* #257: as in [stream_filter], re-establish the stats scope around per-row
     correlated-subquery evaluation in a projected expression. *)
  let s_opt = Lwt.get query_stats_key in
  let* inner = to_stream clock params store ~mode ~cat child in
  let* exprs' =
    Lwt_list.map_s (fun (e, _alias) -> pre_eval_subquery clock store params cat e) exprs
  in
  let has_corr = List.exists plan_expr_has_subquery exprs' in
  if not has_corr
  then (
    let eval_exprs row = Array.of_list (List.map (eval_expr clock params row) exprs') in
    Lwt.return (Lwt_stream.map eval_exprs inner))
  else (
    (* #626: what cannot be resolved is refused, exactly as [stream_filter]
       refuses it. The fallback this replaces answered [Row.V_null] for every
       such expression — silent, and indistinguishable from a legitimate NULL. *)
    match get_outer_scan_metas child with
    | None -> Lwt.fail_with (correlated_projection_refusal ())
    | Some metas ->
      (* #493: same parameterize-and-cache treatment as [stream_filter]; there
         is no AND spine to short-circuit in a projection. *)
      let parameterized = not (List.exists plan_expr_subqueries_use_param exprs') in
      let cache = if parameterized then Some (Hashtbl.create 4) else None in
      let base = Array.length params in
      Lwt.return
        (Lwt_stream.map_s
           (fun row ->
              let bnd =
                if parameterized
                then param_binding_of_metas ~base ~row_len:(Array.length row) metas
                else binding_of_metas metas row
              in
              let row_params =
                if parameterized then Array.append params row else params
              in
              let* vals =
                Lwt_list.map_s
                  (fun e ->
                     let e_subst = substitute_outer_in_plan_expr ~cat bnd e in
                     let* resolved =
                       with_pull_context ~stats:s_opt ~mode ~cache (fun () ->
                         pre_eval_subquery clock store row_params cat e_subst)
                     in
                     (* A subquery that survives the substitution named an outer
                        column no input carries, or an ambiguous one. Refuse
                        rather than let [eval_expr] answer NULL for it. *)
                     if plan_expr_has_subquery resolved
                     then Lwt.fail_with (correlated_projection_refusal ())
                     else Lwt.return (eval_expr clock params row resolved))
                  exprs'
              in
              Lwt.return (Array.of_list vals))
           inner))

and stream_sort clock params store mode cat keys child =
  let* inner = to_stream clock params store ~mode ~cat child in
  let* rows = Lwt_stream.to_list inner in
  let* keys' =
    Lwt_list.map_s
      (fun (e, dir, nulls) ->
         let* e' = pre_eval_subquery clock store params cat e in
         Lwt.return (e', dir, nulls))
      keys
  in
  let cmp a b =
    List.fold_left
      (fun acc (key, dir, nulls) ->
         if acc <> 0
         then acc
         else (
           let va = eval_sort_key clock params a key
           and vb = eval_sort_key clock params b key in
           compare_with_nulls dir nulls va vb))
      0
      keys'
  in
  Lwt.return (Lwt_stream.of_list (List.sort cmp rows))

and stream_index_lookup
      clock
      params
      store
      mode
      table_tree
      idx_tree
      (keys : (int * Row.ty * Plan.expr) list)
      (range : Plan.range option)
      (table_meta : Cat.table_meta)
  =
  let s_opt = Lwt.get query_stats_key in
  let vs = List.map (fun (_, ty, e) -> eval_expr clock params [||] e, ty) keys in
  (* A NULL or type-mismatched value anywhere in the key kills the whole
     conjunction; return no rows rather than seeking the index's NULL entries. *)
  match index_lookup_values vs with
  | None -> Lwt.return (Lwt_stream.of_list [])
  | Some lookup_vs ->
    let prefix, plen = encode_index_key_prefix lookup_vs in
    (* #517: the range (if any) moves the start key forward and stops the walk
       early; it never changes which rows qualify. *)
    let seek_key, past_end = range_seek_bounds clock params ~prefix ~plen range in
    (* #262: read through the active txn so an index lookup sees rows the open
       transaction has inserted/updated but not yet committed. *)
    let* rh = rh_begin store mode in
    (* O(log n) native seek + lazy streaming of just the matching prefix range,
     instead of draining the entire index tree per lookup (#228). *)
    let* cur = rh_seek_ge rh idx_tree seek_key in
    let exhausted = ref false in
    let ended = ref false in
    let finish () =
      if !ended
      then Lwt.return_unit
      else (
        ended := true;
        S.seek_close cur;
        rh_finish rh)
    in
    register_stream_cleanup finish;
    let stream =
      Lwt_stream.from (fun () ->
        if !exhausted
        then Lwt.return_none
        else
          Lwt.catch
            (fun () ->
               let rec next () =
                 match%lwt S.seek_next cur with
                 | None ->
                   exhausted := true;
                   let%lwt () = finish () in
                   Lwt.return_none
                 | Some (ikey, _ival) ->
                   if index_key_in_range ~prefix ~plen ~past_end ikey
                   then (
                     (* #546: counted here, before the [rh_get] — an entry whose
                        table row has since gone is still an entry walked, and
                        this is the count that says what the seek cost over the
                        scan it replaced. *)
                     incr_index_entries s_opt;
                     let rowid_bytes = Bytes.sub ikey (Bytes.length ikey - 8) 8 in
                     let rowid = Rowid.decode rowid_bytes in
                     let table_key = Rowid.encode rowid in
                     let%lwt vrow = rh_get rh table_tree table_key in
                     match vrow with
                     | None -> next ()
                     | Some vbytes ->
                       incr_examined s_opt;
                       let row = decode_with_virtual clock params table_meta vbytes in
                       Lwt.return_some row)
                   else (
                     exhausted := true;
                     let%lwt () = finish () in
                     Lwt.return_none)
               in
               next ())
            (fun exn ->
               exhausted := true;
               let%lwt () = finish () in
               Lwt.fail exn))
    in
    Lwt.return stream

(* #243 (T1): point lookup on an INTEGER PRIMARY KEY rowid alias — the column IS
   the table key, so this is a single O(log n) table-tree seek, no index and no
   second fetch.  A NULL, a non-numeric or a fractional probe matches nothing,
   and an integral REAL addresses the rowid it names — the same answer
   [index_lookup_values] gives for an indexed column, via the shared
   [rowid_lookup_key] (#738). *)
and stream_rowid_lookup
      ?project
      clock
      params
      store
      mode
      lookup_val
      (table_meta : Cat.table_meta)
  =
  let s_opt = Lwt.get query_stats_key in
  let v = eval_expr clock params [||] lookup_val in
  match rowid_lookup_key v with
  | Some n ->
    (* #262: read through the active txn so a primary-key point lookup sees the
       row when it was written earlier in the same open transaction. *)
    let* rh = rh_begin store mode in
    let rl_tree_id, _, _, _ = Cat.row_storage table_meta in
    let* vrow = rh_get rh rl_tree_id (Rowid.encode n) in
    let* () = rh_finish rh in
    (match vrow with
     | None -> Lwt.return (Lwt_stream.of_list [])
     | Some vbytes ->
       incr_examined s_opt;
       let row = decode_with_virtual clock params table_meta vbytes in
       (* #416: [project] is the ordinal list of an [Op_project] fused into
          this lookup by {!to_stream}.  A point lookup yields at most one row,
          so applying the projection here is exactly what wrapping the result
          in [Lwt_stream.map (project_row ords)] would have computed —
          [project_row] is a pure array-index selection — while saving the
          second [Lwt_stream] layer that wrapping costs. *)
       let row =
         match project with
         | None -> row
         | Some ords -> project_row ords row
       in
       Lwt.return (Lwt_stream.of_list [ row ]))
  | None -> Lwt.return (Lwt_stream.of_list [])

(* Probe the right index for one left row [lrow], appending matched (or a
   null-padded row for LEFT JOIN) combinations to [out]. *)
(* #516: evaluate a probe key against one left row.  [None] means the key
   matches nothing — a NULL component can never equal a stored key under
   three-valued logic — and the caller null-extends or drops the row without
   seeking, exactly as the single-column probe did for a NULL join key.

   #743: the probe seeks a TYPED index, so each part is translated towards its
   index column's declared type by {!index_lookup_values} — the very function
   the [WHERE col = lit] seek uses, so a join key and a filter can never
   disagree about which stored keys an equality covers.  That is what makes a
   cross-numeric ON equality find its rows: an integral REAL addresses the
   INTEGER key it names, and an INTEGER addresses the REAL key only when the
   round-trip is exact.  Every other [None] it returns still means "matches
   nothing" and never "seek wider" — a NULL, a fractional REAL against an
   INTEGER column, a value above 2^53 with no exact double, a TEXT against a
   numeric column. *)
and nlj_probe_values clock params lrow (probe : Plan.probe_part list)
  : Index_key.value list option
  =
  let rec go acc = function
    | [] -> index_lookup_values (List.rev acc)
    | Plan.Probe_from_left (i, ty) :: rest -> go ((lrow.(i), ty) :: acc) rest
    | Plan.Probe_const (e, ty) :: rest ->
      go ((eval_expr clock params [||] e, ty) :: acc) rest
  in
  go [] probe

and nlj_probe_left
      clock
      params
      s_opt
      rh
      (right_meta : Cat.table_meta)
      idx_tree
      (probe : Plan.probe_part list)
      (probe_range : Plan.range option)
      join_kind
      n_right_cols
      out
      lrow
  : unit Lwt.t
  =
  match nlj_probe_values clock params lrow probe with
  | None ->
    if join_kind = `Left
    then out := Array.append lrow (Array.make n_right_cols Row.V_null) :: !out;
    Lwt.return_unit
  | Some key_values ->
    let prefix, plen = encode_index_key_prefix key_values in
    (* #570: the probe key covers a leading prefix of the index, so a WHERE
       range on the column AFTER it narrows this walk exactly as it narrows
       [stream_index_lookup]'s — same two helpers, so the two paths cannot
       disagree about which entries a bound covers.  It is a pure narrowing of
       what is READ: [chain_joins] still applies the whole WHERE clause to the
       joined row. *)
    let seek_key, past_end = range_seek_bounds clock params ~prefix ~plen probe_range in
    (* O(log n) native seek per probe — avoids draining the whole index per
       left row, which made indexed nested-loop joins O(n^2) (#228/#229). *)
    let* cur = rh_seek_ge rh idx_tree seek_key in
    let found = ref false in
    let rec scan () =
      match%lwt S.seek_next cur with
      | None -> Lwt.return_unit
      | Some (ikey, _) ->
        if index_key_in_range ~prefix ~plen ~past_end ikey
        then (
          (* #546/#595: an index entry walked is the counter that tells a probe
             from a scan; [rows_examined] cannot. *)
          incr_index_entries s_opt;
          let rowid_bytes = Bytes.sub ikey (Bytes.length ikey - 8) 8 in
          let rowid = Rowid.decode rowid_bytes in
          let table_key = Rowid.encode rowid in
          let nlj_tree_id, _, _, _ = Cat.row_storage right_meta in
          let* vrow = rh_get rh nlj_tree_id table_key in
          match vrow with
          | None -> scan ()
          | Some vbytes ->
            incr_examined s_opt;
            let rrow = decode_with_virtual clock params right_meta vbytes in
            out := Array.append lrow rrow :: !out;
            found := true;
            scan ())
        else Lwt.return_unit
    in
    let* () = scan () in
    S.seek_close cur;
    (match join_kind with
     | `Left when not !found ->
       out := Array.append lrow (Array.make n_right_cols Row.V_null) :: !out
     | _ -> ());
    Lwt.return_unit

and stream_nested_loop_join
      clock
      params
      store
      mode
      cat
      left
      (right_meta : Cat.table_meta)
      idx_tree
      (probe : Plan.probe_part list)
      (probe_range : Plan.range option)
      join_kind
      n_right_cols
  =
  (* #239: captured under [query]'s [with_value] scope; counts right-side index
     probes (the left input's base scan is counted via [to_stream] below). *)
  let s_opt = Lwt.get query_stats_key in
  (* #615: this operator expresses its ON predicate as an index probe, and
     [plan_join] only builds it from a recognised [col = col] equality whose
     remaining key columns are pinned by literal equalities — so a subquery
     cannot reach [probe] or [probe_range] through the planner today.
     [Op_nested_loop_join] is a public constructor, and an unresolved
     [P_subquery] here would encode as NULL and silently drop every driving row,
     which is the exact failure class #566/#592/#615 are about.  Refuse it. *)
  let expr_corr = plan_expr_has_subquery in
  let range_corr (r : Plan.range) =
    Option.fold ~none:false ~some:expr_corr r.Plan.r_lo
    || Option.fold ~none:false ~some:expr_corr r.Plan.r_hi
  in
  if
    List.exists
      (function
        | Plan.Probe_const (e, _) -> expr_corr e
        | Plan.Probe_from_left _ -> false)
      probe
    || Option.fold ~none:false ~some:range_corr probe_range
  then failwith (correlated_on_refusal ());
  let* left_stream = to_stream clock params store ~mode ~cat left in
  let* left_rows = Lwt_stream.to_list left_stream in
  (* #262: probe the inner index through the active txn so the join sees inner
     rows written earlier in the same open transaction. *)
  with_read store mode
  @@ fun rh ->
  let out = ref [] in
  let* () =
    Lwt_list.iter_s
      (nlj_probe_left
         clock
         params
         s_opt
         rh
         right_meta
         idx_tree
         probe
         probe_range
         join_kind
         n_right_cols
         out)
      left_rows
  in
  Lwt.return (Lwt_stream.of_list (List.rev !out))

(* Build a hash table mapping each right row's join key to its rows; NULL keys
   are excluded (they never match an equi-join probe).

   #743: buckets are keyed by {!join_key_bytes}, the CANONICAL key, not by the
   raw encoding of the value — so an integral REAL lands in the same bucket as
   the INTEGER it names and a cross-numeric equi-join joins the rows [=] says
   are equal. *)
and hash_build right_rows right_key : (bytes, Row.t list) Hashtbl.t =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun rrow ->
       match rrow.(right_key) with
       | Row.V_null -> ()
       | key_v ->
         let key_bytes = join_key_bytes key_v in
         let prev =
           try Hashtbl.find tbl key_bytes with
           | Not_found -> []
         in
         Hashtbl.replace tbl key_bytes (rrow :: prev))
    right_rows;
  tbl

and stream_hash_join
      clock
      params
      store
      mode
      cat
      left
      right
      left_key
      right_key
      on_pred
      join_kind
      n_right_cols
  =
  (* [on_pred] is only ever consulted by the cartesian arm below, so a keyed
     join carrying one would drop its match test silently.  [Op_hash_join] is a
     public constructor — the planner honouring that invariant is not enough to
     enforce it, and the failure mode is a wrong answer, not a crash. *)
  if left_key >= 0 && right_key >= 0 && Option.is_some on_pred
  then
    invalid_arg
      "Exec.stream_hash_join: on_pred is only meaningful on the cartesian arm (left_key \
       < 0 || right_key < 0)";
  let* left_stream = to_stream clock params store ~mode ~cat left in
  let* right_stream = to_stream clock params store ~mode ~cat right in
  let* right_rows = Lwt_stream.to_list right_stream in
  if left_key < 0 || right_key < 0
  then (
    (* Cartesian product fallback (general ON predicate).

       #552: when the planner supplies [on_pred] it is the join's match test,
       applied here to each pair, and a left row that survives no pair is
       null-extended.  That is the only placement an outer join can use — a
       filter above the join would reject the very null-extended row it has to
       let through.  With [on_pred = None] every pair matches and the caller
       filters, which is what an INNER join still does. *)
    (* Uncorrelated subqueries in the ON predicate are resolved once, as
       [stream_filter] does.  What survives [pre_eval_subquery] is correlated.

       #566 refused that outright, because the correlation source could not be
       located over a join node: a surviving [P_subquery] evaluates to
       [Row.V_null], so [matches] is false for every pair, [any] is never set,
       and an outer join null-extends {i every} left row — a complete result set
       of the right cardinality with the ON predicate silently unevaluated.

       #615 reopens that decision, because #592 built the source: the joined row
       is [lrow @ rrow] and [get_outer_scan_metas] describes both inputs, so the
       correlation resolves here exactly as it does in the [Op_filter] above an
       INNER join.  The two spellings of the same query agreed on nothing before
       this; now they agree on both the answer and the refusal.

       The cost is that the pairing loop becomes Lwt and the substitution runs
       once per (left, right) {i pair} rather than once per surviving row — an
       outer join has no choice, since the ON predicate {b is} the match test and
       a filter above the join would reject the null-extended row it must emit
       (#552).  The pure loop below is kept for the arm where no subquery
       survives, which is every join that has no correlated ON. *)
    let s_opt = Lwt.get query_stats_key in
    let* pred =
      match on_pred with
      | None -> Lwt.return None
      | Some p ->
        let+ p = pre_eval_subquery clock store params cat p in
        Some p
    in
    let correlated =
      match pred with
      | Some p -> plan_expr_has_subquery p
      | None -> false
    in
    (* #615: both inputs of this join, re-based into the joined row.  The right
       input's offsets shift by the left input's width, and the identifiers must
       still be unique across the two — a self-join with no aliases is refused
       here for the same reason [get_outer_scan_metas] refuses one. *)
    let joined_inputs () =
      match get_outer_scan_metas left, outer_row_width left with
      | Some ls, Some w ->
        (match get_outer_scan_metas right with
         | None -> None
         | Some rs ->
           let all =
             ls @ List.map (fun oi -> { oi with oi_offset = oi.oi_offset + w }) rs
           in
           let idents = List.map (fun oi -> oi.oi_ident) all in
           if List.length (List.sort_uniq String.compare idents) <> List.length idents
           then None
           else Some all)
      | _, _ -> None
    in
    let* left_rows = Lwt_stream.to_list left_stream in
    let out = ref [] in
    let null_extend lrow any =
      match join_kind with
      | `Left when not any ->
        out := Array.append lrow (Array.make n_right_cols Row.V_null) :: !out
      | _ -> ()
    in
    if not correlated
    then (
      (* The pre-#615 loop, unchanged and still pure: every join whose ON
         predicate carries no surviving subquery takes this arm. *)
      let matches joined =
        match pred with
        | None -> true
        | Some p -> value_truthy (eval_expr clock params joined p)
      in
      List.iter
        (fun lrow ->
           let any = ref false in
           List.iter
             (fun rrow ->
                let joined = Array.append lrow rrow in
                if matches joined
                then (
                  out := joined :: !out;
                  any := true))
             right_rows;
           null_extend lrow !any)
        left_rows;
      Lwt.return (Lwt_stream.of_list (List.rev !out)))
    else (
      match joined_inputs () with
      | None -> Lwt.fail_with (correlated_on_refusal ())
      | Some inputs ->
        let p = Option.get pred in
        let matches_s joined =
          let bnd = binding_of_metas inputs joined in
          let* resolved =
            (* #493 + #615: [binding_of_metas] pins literals, so the substituted
               statement differs on every (left, right) pair and a cache would
               only grow — same reasoning as the aggregate site below.  The
               parameterized spelling is not available here: the ON predicate is
               the match test, so it is evaluated per pair rather than per
               surviving row (#552). *)
            with_pull_context ~stats:s_opt ~mode ~cache:None (fun () ->
              pre_eval_subquery
                clock
                store
                params
                cat
                (substitute_outer_in_plan_expr ~cat bnd p))
          in
          if plan_expr_has_subquery resolved
          then Lwt.fail_with (correlated_on_refusal ())
          else Lwt.return (value_truthy (eval_expr clock params joined resolved))
        in
        let* () =
          Lwt_list.iter_s
            (fun lrow ->
               let any = ref false in
               let* () =
                 Lwt_list.iter_s
                   (fun rrow ->
                      let joined = Array.append lrow rrow in
                      let+ keep = matches_s joined in
                      if keep
                      then (
                        out := joined :: !out;
                        any := true))
                   right_rows
               in
               null_extend lrow !any;
               Lwt.return_unit)
            left_rows
        in
        Lwt.return (Lwt_stream.of_list (List.rev !out))))
  else (
    let tbl = hash_build right_rows right_key in
    let* left_rows = Lwt_stream.to_list left_stream in
    let out = ref [] in
    List.iter
      (fun lrow ->
         let key_v = lrow.(left_key) in
         let any = ref false in
         (match key_v with
          | Row.V_null -> ()
          | _ ->
            (* #743: the same canonical key the build side used, so the two
               sides cannot disagree about which values are one key. *)
            let key_bytes = join_key_bytes key_v in
            (match Hashtbl.find_opt tbl key_bytes with
             | None -> ()
             | Some rrows ->
               List.iter
                 (fun rrow ->
                    out := Array.append lrow rrow :: !out;
                    any := true)
                 (List.rev rrows)));
         match join_kind with
         | `Left when not !any ->
           let null_right = Array.make n_right_cols Row.V_null in
           out := Array.append lrow null_right :: !out
         | _ -> ())
      left_rows;
    Lwt.return (Lwt_stream.of_list (List.rev !out)))

(* #488: how an aggregate reads its argument out of one INPUT row.  [None] is
   the COUNT-star case — no argument at all.  A bare column stays an array
   index (what the whole engine did before #488); anything else is an
   expression evaluated per row, which is why this needs [clock] and [params].
   Every consumer of a [Plan.agg_spec] must go through here: reading
   [col_ord] directly answers [None] for an expression argument and would
   silently turn [SUM(a * b)] into COUNT-star's "no argument" arm. *)
and agg_arg_getter clock params (spec : Plan.agg_spec) : (Row.t -> Row.value) option =
  match spec.Plan.arg_expr, spec.Plan.col_ord with
  | Some e, _ -> Some (fun row -> eval_expr clock params row e)
  | None, Some i -> Some (fun row -> row.(i))
  | None, None -> None

(* SUM over one group's already-extracted argument values: preserve INT vs REAL
   like SQLite-lite. *)
and agg_sum (vals : Row.value list) : Row.value =
  let any_real =
    List.exists
      (function
        | Row.V_real _ -> true
        | _ -> false)
      vals
  in
  let any_non_null =
    List.exists
      (function
        | Row.V_null -> false
        | _ -> true)
      vals
  in
  if not any_non_null
  then Row.V_null
  else if any_real
  then (
    let s =
      List.fold_left
        (fun acc v ->
           match v with
           | Row.V_null -> acc
           | Row.V_int n -> acc +. Int64.to_float n
           | Row.V_real f -> acc +. f
           | v -> agg_non_numeric_failure "SUM" v)
        0.0
        vals
    in
    Row.V_real s)
  else (
    let s =
      List.fold_left
        (fun acc v ->
           match v with
           | Row.V_null -> acc
           | Row.V_int n -> Int64.add acc n
           | v -> agg_non_numeric_failure "SUM" v)
        0L
        vals
    in
    Row.V_int s)

(* #491 x #488: the DISTINCT dedup key, defined ONCE and used by both the batch
   ([aggregate_one]) and incremental ([make_agg_acc]) paths, so the two cannot
   drift apart the way #488's comment warns about.

   The key is [row_key]'s rendering of the ARGUMENT VALUE — not of a column.
   That is the whole composition: #491 landed dedup on a column ordinal, which
   #488's expression arguments do not have ([col_ord] is [None] for
   [COUNT(DISTINCT a * b)]), so a column-keyed dedup had no correct answer for
   the shape only both features together can express.  Keying on the value the
   aggregate is about to consume works for both spellings and is the same key
   SELECT DISTINCT and the hash joins already use — so #536's decisions are
   inherited verbatim: all NaNs collapse to one, and none collapses into NULL.
   No fifth value comparator is introduced.

   Returns a stateful predicate: [true] the first time a value is seen, [false]
   afterwards.  A NULL is "seen" like any other value, so it survives the dedup
   as ONE entry and is then dropped by each aggregate's own NULL handling —
   which is why [COUNT(DISTINCT x)] skips NULLs exactly as [COUNT(x)] does. *)
and distinct_filter ?(collation = Ast.Collate_binary) () : Row.value -> bool =
  let seen = Hashtbl.create 64 in
  fun v ->
    (* #722: the dedup KEY carries the argument's collation; the value the
       aggregate then accumulates is the raw one. *)
    let k = row_key [| collate_key collation v |] in
    if Hashtbl.mem seen k
    then false
    else (
      Hashtbl.replace seen k ();
      true)

(* Evaluate one aggregate [spec] over the argument values of a group. *)
and aggregate_over_values
      ?(collation = Ast.Collate_binary)
      (func : Ast.agg_func)
      (vals : Row.value list)
  : Row.value
  =
  match func with
  | Ast.Agg_count ->
    let n =
      List.fold_left
        (fun acc v ->
           match v with
           | Row.V_null -> acc
           | _ -> acc + 1)
        0
        vals
    in
    Row.V_int (Int64.of_int n)
  | Ast.Agg_sum -> agg_sum vals
  | Ast.Agg_avg ->
    let sum, n =
      List.fold_left
        (fun (s, n) v ->
           match v with
           | Row.V_null -> s, n
           | Row.V_int x -> s +. Int64.to_float x, n + 1
           | Row.V_real f -> s +. f, n + 1
           | v -> agg_non_numeric_failure "AVG" v)
        (0.0, 0)
        vals
    in
    if n = 0 then Row.V_null else Row.V_real (sum /. float_of_int n)
  (* #722: MIN/MAX compare under the argument's collation but return the RAW
     winning value, so [MIN(x COLLATE NOCASE)] answers 'HELLO', not 'hello'. *)
  | Ast.Agg_min ->
    List.fold_left
      (fun acc v ->
         match v, acc with
         | Row.V_null, _ -> acc
         | v, Row.V_null -> v
         | v, cur -> if compare_collated collation v cur < 0 then v else cur)
      Row.V_null
      vals
  | Ast.Agg_max ->
    List.fold_left
      (fun acc v ->
         match v, acc with
         | Row.V_null, _ -> acc
         | v, Row.V_null -> v
         | v, cur -> if compare_collated collation v cur > 0 then v else cur)
      Row.V_null
      vals
  | Ast.Agg_group_concat sep ->
    let separator = Option.value sep ~default:"," in
    let parts =
      List.filter_map
        (function
          | Row.V_null -> None
          | Row.V_int n -> Some (Int64.to_string n)
          | Row.V_real f -> Some (Printf.sprintf "%.17g" f)
          | Row.V_text s -> Some s
          | Row.V_blob _ -> Some "")
        vals
    in
    if parts = [] then Row.V_null else Row.V_text (String.concat separator parts)

(* Evaluate one aggregate [spec] over the rows of a group. *)
and aggregate_one clock params (spec : Plan.agg_spec) (group_rows : Row.t list)
  : Row.value
  =
  match agg_arg_getter clock params spec with
  | None ->
    (match spec.Plan.func with
     (* #491: a COUNT-star cannot carry DISTINCT — the grammar has no
        [COUNT(DISTINCT * )] and [E_agg_distinct] holds a mandatory argument, so
        this is unreachable.  It raises rather than counting rows, because the
        one thing worse than refusing the shape is silently ignoring a modifier
        the caller wrote. *)
     | Ast.Agg_count when not spec.Plan.distinct ->
       Row.V_int (Int64.of_int (List.length group_rows))
     | Ast.Agg_count -> failwith "COUNT(DISTINCT ...) requires an argument"
     | Ast.Agg_group_concat _ -> failwith "GROUP_CONCAT requires a column argument"
     | _ -> failwith "non-COUNT aggregate must have a column argument")
  | Some get ->
    let collation = agg_spec_collation spec in
    let vals = List.map get group_rows in
    let vals =
      if spec.Plan.distinct
      then List.filter (distinct_filter ~collation ()) vals
      else vals
    in
    aggregate_over_values ~collation spec.Plan.func vals

(* Partition [rows] into (group_key, group_rows) by [group_cols] (stable). *)
and aggregate_build_groups group_cols rows : (Row.value list * Row.t list) list =
  let group_keys_of_row row = List.map (fun i -> row.(i)) group_cols in
  let compare_group_keys ka kb =
    List.fold_left2 (fun acc a b -> if acc <> 0 then acc else compare_values a b) 0 ka kb
  in
  if group_cols = []
  then [ [], rows ]
  else (
    let sorted =
      List.stable_sort
        (fun a b -> compare_group_keys (group_keys_of_row a) (group_keys_of_row b))
        rows
    in
    let rec group_runs acc cur_key cur_rows = function
      | [] ->
        (match cur_rows with
         | [] -> List.rev acc
         | _ -> List.rev ((cur_key, List.rev cur_rows) :: acc))
      | r :: rest ->
        let k = group_keys_of_row r in
        if cur_rows <> [] && compare_group_keys k cur_key = 0
        then group_runs acc cur_key (r :: cur_rows) rest
        else (
          let acc' = if cur_rows = [] then acc else (cur_key, List.rev cur_rows) :: acc in
          group_runs acc' k [ r ] rest)
    in
    group_runs [] [] [] sorted)

(* Append post-aggregate window-function columns to [after_having] rows. *)
and aggregate_apply_windows clock params agg_windows after_having =
  if agg_windows = []
  then after_having
  else (
    let n_total = List.length after_having in
    let indexed = List.mapi (fun i r -> i, r) after_having in
    let window_arrays =
      List.map
        (fun (wplan : Plan.window_plan_item) ->
           let partitions =
             group_by_partition clock params wplan.Plan.partition_by indexed
           in
           let combined = Array.make n_total Row.V_null in
           List.iter
             (fun (_, partition_indexed) ->
                let sorted =
                  sort_partition_by clock params wplan.Plan.order_by partition_indexed
                in
                let part_results =
                  compute_window_for_partition clock params wplan sorted n_total
                in
                List.iter
                  (fun (orig_idx, _) -> combined.(orig_idx) <- part_results.(orig_idx))
                  sorted)
             partitions;
           combined)
        agg_windows
    in
    List.mapi
      (fun i row ->
         let extras = List.map (fun arr -> arr.(i)) window_arrays in
         Array.append row (Array.of_list extras))
      after_having)

(* #247: build an incremental accumulator for one aggregate [spec]: an
   [(update, finalize)] pair folded over scanned rows.  Returns [None] for any
   spec the fast-path doesn't handle (e.g. a non-COUNT aggregate with no column),
   which makes the caller fall back to the general [stream_aggregate] path.  The
   per-type logic here MUST stay byte-identical to [aggregate_one]/[agg_sum]. *)
and make_agg_acc clock params (spec : Plan.agg_spec)
  : ((Row.t -> unit) * (unit -> Row.value)) option
  =
  match agg_arg_getter clock params spec with
  | None ->
    (* No argument at all: COUNT-star, which counts rows and cannot be
       DISTINCT (see [aggregate_one]).  Every other aggregate without an
       argument is refused here, which drops the query onto the general
       [stream_aggregate] path exactly as before. *)
    (match spec.Plan.func with
     | Ast.Agg_count when not spec.Plan.distinct ->
       let c = ref 0 in
       Some ((fun _ -> incr c), fun () -> Row.V_int (Int64.of_int !c))
     | _ -> None)
  | Some get ->
    let collation = agg_spec_collation spec in
    let update_v, finalize = make_agg_acc_over_values ~collation spec.Plan.func in
    (* #491: DISTINCT keeps the #247 fast path rather than falling back to the
       general path — a no-GROUP-BY distinct count is exactly TPC-C
       StockLevel's shape.  The filter wraps the GETTER's result, not the row,
       so the argument is evaluated ONCE per row: wrapping [update] instead
       would re-evaluate an [arg_expr] per row, and a clock-dependent argument
       could then differ between the dedup key and the accumulated value. *)
    let update =
      if spec.Plan.distinct
      then (
        let keep = distinct_filter ~collation () in
        fun (row : Row.t) ->
          let v = get row in
          if keep v then update_v v)
      else fun (row : Row.t) -> update_v (get row)
    in
    Some (update, finalize)

(* The per-type accumulator, over ARGUMENT VALUES rather than rows.  #488 made
   every consumer read its argument through [agg_arg_getter]; hoisting the
   getter out to the caller leaves this function a pure function of [func] and
   a value — structurally the same shape as [aggregate_over_values], which is
   what makes "these two MUST stay byte-identical" checkable by reading them
   side by side instead of by trusting a comment.  It is also what lets #491's
   DISTINCT filter sit between the getter and the accumulator. *)
and make_agg_acc_over_values ?(collation = Ast.Collate_binary) (func : Ast.agg_func)
  : (Row.value -> unit) * (unit -> Row.value)
  =
  match func with
  | Ast.Agg_count ->
    let c = ref 0 in
    ( (fun v ->
        match v with
        | Row.V_null -> ()
        | _ -> incr c)
    , fun () -> Row.V_int (Int64.of_int !c) )
  | Ast.Agg_sum ->
    (* INT vs REAL preserved exactly like [agg_sum]: REAL iff any real seen;
       NULL iff no non-null seen. *)
    let si = ref 0L
    and sf = ref 0.0
    and any_real = ref false
    and any_nn = ref false in
    ( (fun v ->
        match v with
        | Row.V_null -> ()
        | Row.V_int n ->
          any_nn := true;
          si := Int64.add !si n;
          sf := !sf +. Int64.to_float n
        | Row.V_real f ->
          any_nn := true;
          any_real := true;
          sf := !sf +. f
        | v -> agg_non_numeric_failure "SUM" v)
    , fun () ->
        if not !any_nn
        then Row.V_null
        else if !any_real
        then Row.V_real !sf
        else Row.V_int !si )
  | Ast.Agg_avg ->
    let sf = ref 0.0
    and n = ref 0 in
    ( (fun v ->
        match v with
        | Row.V_null -> ()
        | Row.V_int x ->
          sf := !sf +. Int64.to_float x;
          incr n
        | Row.V_real f ->
          sf := !sf +. f;
          incr n
        | v -> agg_non_numeric_failure "AVG" v)
    , fun () -> if !n = 0 then Row.V_null else Row.V_real (!sf /. float_of_int !n) )
  (* #722: same collation rule as [aggregate_over_values] — these two MUST
     stay byte-identical. *)
  | Ast.Agg_min ->
    let best = ref Row.V_null in
    ( (fun v ->
        match v, !best with
        | Row.V_null, _ -> ()
        | v, Row.V_null -> best := v
        | v, cur -> if compare_collated collation v cur < 0 then best := v)
    , fun () -> !best )
  | Ast.Agg_max ->
    let best = ref Row.V_null in
    ( (fun v ->
        match v, !best with
        | Row.V_null, _ -> ()
        | v, Row.V_null -> best := v
        | v, cur -> if compare_collated collation v cur > 0 then best := v)
    , fun () -> !best )
  | Ast.Agg_group_concat sep ->
    let separator = Option.value sep ~default:"," in
    let parts = ref [] in
    (* newest-first; reversed at finalize to preserve scan order *)
    ( (fun v ->
        match v with
        | Row.V_null -> ()
        | Row.V_int n -> parts := Int64.to_string n :: !parts
        | Row.V_real f -> parts := Printf.sprintf "%.17g" f :: !parts
        | Row.V_text s -> parts := s :: !parts
        | Row.V_blob _ -> parts := "" :: !parts)
    , fun () ->
        (match !parts with
         | [] -> Row.V_null
         | l -> Row.V_text (String.concat separator (List.rev l))) )

(* #247: cursor-level fast path for a no-GROUP-BY aggregate directly over a
   (optionally filtered) sequential scan.  Folds the accumulators over the scan
   cursor in a single pass, bypassing the child's per-row [Lwt_stream] layers and
   the [Lwt_stream.to_list] full-table materialisation the general path pays.
   Returns [Some stream] (always exactly one output row, matching
   [aggregate_build_groups]'s single implicit group) when applicable, else [None]
   to fall back.  Preserves the streaming/stack-bound property: the fold holds
   only the accumulators, never the rows. *)
and aggregate_fast_path
      clock
      params
      store
      mode
      cat
      child
      group_cols
      aggs
      having
      proj
      agg_windows
  : Row.t Lwt_stream.t option Lwt.t
  =
  if not (agg_fastpath_enabled ())
  then Lwt.return None
  else if group_cols <> [] || having <> None || agg_windows <> []
  then Lwt.return None
  else if
    (* #664: an aggregate ARGUMENT may now carry a subquery.  This loop is
       pure and [eval_expr] answers [Row.V_null] for an unresolved
       [P_subquery], so accumulating over one here would silently sum NULLs —
       the same reason the projection test below gives up.  [stream_aggregate]
       resolves it; give the query up to it. *)
    List.exists
      (fun (s : Plan.agg_spec) ->
         match s.Plan.arg_expr with
         | Some e -> plan_expr_has_subquery e
         | None -> false)
      aggs
  then Lwt.return None
  else if
    not
      (List.for_all
         (function
           | Plan.PI_agg_slot _ -> true
           (* #507: with no GROUP BY the aggregate output row IS the aggregate
              value array, so an expression over it evaluates directly against
              the accumulators — no need to give up the fast path for it.

              #558: unless it carries a subquery. This loop is pure, and
              [eval_expr] answers [Row.V_null] for an unresolved subquery, so a
              fast-path projection would silently drop it. [stream_aggregate]
              resolves it; give the query up to it. *)
           | Plan.PI_expr e -> not (plan_expr_has_subquery e)
           | _ -> false)
         proj)
  then Lwt.return None
  else (
    match child with
    | Plan.Op_seq_scan { table_meta; _ } ->
      run_aggregate_fast_path clock params store mode cat table_meta None aggs proj
    | Plan.Op_filter { pred; child = Plan.Op_seq_scan { table_meta; _ } }
      when not (plan_expr_has_subquery pred) ->
      run_aggregate_fast_path clock params store mode cat table_meta (Some pred) aggs proj
    (* #674 (item 1 of 3): COUNT-star/COUNT(col)/MIN/MAX directly over an
       [Op_index_lookup] (optionally [Op_filter]-wrapped), reading the
       aggregated value(s) straight off the decoded index key instead of
       paying an [rh_get] per matching entry.  See
       [index_cover_eligible]/[run_index_cover_walk] for the gate and the
       walk. *)
    | Plan.Op_index_lookup { idx_tree; keys; range; table_meta; _ } ->
      run_aggregate_fast_path_index
        clock
        params
        store
        mode
        cat
        table_meta
        idx_tree
        keys
        range
        None
        aggs
        proj
    | Plan.Op_filter
        { pred; child = Plan.Op_index_lookup { idx_tree; keys; range; table_meta; _ } }
      when not (plan_expr_has_subquery pred) ->
      run_aggregate_fast_path_index
        clock
        params
        store
        mode
        cat
        table_meta
        idx_tree
        keys
        range
        (Some pred)
        aggs
        proj
    | _ -> Lwt.return None)

(* #674 (item 1 of 3): decode one column read off an index entry into a
   [Row.value].  [col_ty] is the DECLARED type of the table column at that
   ordinal.  Before #578, [Index_key.decode]'s [IK_null] was ambiguous between
   "the value is NULL" and "the value is NaN" (#536: both encoded to the same
   [0x00] byte); every caller of this function has already gated the column to
   NOT NULL (see [index_cover_eligible]), so an actual NULL cannot occur here.
   Since #578 a NaN decodes to its own [Index_key.IK_real nan], never
   [IK_null], so the [Row.Real -> V_real nan] arm below is unreachable through
   any live caller today — it is kept as the defensive fallback rather than
   deleted: on any declared type a NOT NULL column can never legally encode
   [IK_null] at all, and [V_null] (or, for REAL, [V_real nan]) is returned
   rather than raising, matching the general engine's preference (per
   CLAUDE.md's #638 section) for surfacing rather than crashing on an
   invariant that "cannot" be violated. *)
and index_value_to_row_value (col_ty : Row.ty) (iv : Index_key.value) : Row.value =
  match iv with
  | Index_key.IK_null ->
    (match col_ty with
     | Row.Real -> Row.V_real Float.nan
     | _ -> Row.V_null)
  | Index_key.IK_int n -> Row.V_int n
  | Index_key.IK_real f -> Row.V_real f
  | Index_key.IK_text s -> Row.V_text s
  | Index_key.IK_blob b -> Row.V_blob b

(* #674 (item 1 of 3): is [table_meta]'s [idx_tree] index, [keys]/[range]
   equality+range shape, [pred_opt] residual predicate and [aggs] aggregate
   list eligible for the covering (no [rh_get]) walk?  Returns
   [Some (idx_ords, min_early_stop)] when it is:

   - [idx_ords]: the table column ordinal at each position of the index's own
     column list, i.e. what [Index_key.decode]'s i-th value corresponds to.
   - [min_early_stop]: whether the single-aggregate shape [MIN(col)] with no
     predicate, where [col] is exactly the index column right after the
     equality prefix and there is no #517 range, can stop after the FIRST
     matching entry (ascending index order already yields the minimum) — see
     the design doc's discussion of why MAX cannot do the same without a
     reverse B-tree walk primitive this design does not add.

   Gates, matching the design doc's stated scope exactly:
   - every column [pred_opt] or any [agg]'s argument reads must be part of
     the index AND declared NOT NULL (#536: at the time this was written,
     [IK_null] was ambiguous between NULL and NaN, so a nullable column could
     not be read back safely.  #578 gave NaN its own tag and resolved that
     specific ambiguity, but the NOT NULL requirement here predates and is
     not re-derived from it — relaxing this gate for nullable REAL columns is
     a separate, unverified optimisation and out of #578's scope);
   - only [Agg_count], [Agg_min], [Agg_max] are covered — SUM/AVG/GROUP_CONCAT
     are out of this item's scope;
   - MIN/MAX additionally require: no #517 [range], and the aggregated column
     is exactly the next unconstrained key column of the index (position
     [List.length keys]) — not merely SOME index column.  The index itself is
     already guaranteed non-expression and non-partial by
     [Planner.index_is_seekable], which is what [access_path_for_eqs] must
     pass to ever produce an [Op_index_lookup] in the first place — see
     CLAUDE.md's #674 design-doc section on GENERATED/partial-index scoping. *)
and index_cover_col_ordinal (table_meta : Cat.table_meta) name : int option =
  let rec go n = function
    | [] -> None
    | (c : Row.column) :: rest ->
      if String.equal c.Row.name name then Some n else go (n + 1) rest
  in
  go 0 table_meta.Cat.columns

(* The index's own column list, translated to table column ordinals — [None]
   if the catalog and the index somehow disagree on a column name (defensive;
   should not happen for a live index). *)
and index_cover_idx_ords (table_meta : Cat.table_meta) (idx_info : Cat.index_info)
  : int array option
  =
  let idx_ords_opt =
    List.map (index_cover_col_ordinal table_meta) idx_info.Cat.idx_columns
  in
  if List.exists Option.is_none idx_ords_opt
  then None
  else Some (Array.of_list (List.map Option.get idx_ords_opt))

(* #536: a column is safe to read off the index only if it is part of the
   index (so it is actually present in the encoded key) AND declared NOT
   NULL.  See the #578 note on [index_cover_eligible] above: the NULL/NaN
   ambiguity this originally guarded against was fixed at the encoding level
   by #578, so this NOT NULL requirement is now conservative rather than
   strictly necessary — kept as-is because relaxing it is a separate,
   unverified change. *)
and index_cover_ok_col (table_meta : Cat.table_meta) (idx_ords : int array) ord : bool =
  let is_idx_ord = Array.exists (fun o -> o = ord) idx_ords in
  let col_not_null =
    match List.nth_opt table_meta.Cat.columns ord with
    | Some (c : Row.column) -> c.Row.not_null
    | None -> false
  in
  is_idx_ord && col_not_null

and index_cover_agg_ok ok_col (s : Plan.agg_spec) : bool =
  match s.Plan.func, s.Plan.arg_expr, s.Plan.col_ord with
  | Ast.Agg_count, None, None -> true (* COUNT-star *)
  | (Ast.Agg_count | Ast.Agg_min | Ast.Agg_max), Some e, None ->
    plan_expr_reads_only_cols ok_col e
  | (Ast.Agg_count | Ast.Agg_min | Ast.Agg_max), None, Some i -> ok_col i
  | _ -> false

and index_cover_pred_ok ok_col (pred_opt : Plan.expr option) : bool =
  match pred_opt with
  | None -> true
  | Some p -> plan_expr_reads_only_cols ok_col p

(* MIN/MAX additionally require: no #517 [range], the aggregated column is
   exactly the next unconstrained key column of the index (position
   [n_eq]) — not merely SOME index column — and #754: the column is NOT a
   REAL.

   The REAL exclusion exists because [Index_key.encode_value] (#754)
   deliberately makes [-0.0] and [+0.0] encode to the SAME key bytes, which
   is exactly what makes an equality seek find a stored [-0.0] again — but it
   also means the sign of a decoded zero is gone from the index key itself:
   [Index_key.decode]'ing either one's key yields [+0.0], because that is the
   only bit pattern ever written now. [run_index_cover_walk] reads the
   MIN/MAX value straight off the decoded key (never touching the row, which
   is the entire point of the covering optimisation), so for a REAL column it
   would report [+0.0] as the answer even when the true extremal row, still
   sitting in the table with its sign bit intact ([Row.encode]/[decode]
   preserve it bit-for-bit), is [-0.0] — silently returning the wrong SIGN,
   not merely the wrong row. Excluding REAL here sends such a query to the
   general aggregate path instead, which fetches the actual row and therefore
   the actual sign. This costs the covering optimisation only for REAL-typed
   MIN/MAX, and only in exchange for correctness on a value class the index
   key can no longer round-trip losslessly. *)
and index_cover_minmax_ok
      (table_meta : Cat.table_meta)
      (idx_ords : int array)
      (n_eq : int)
      (range : Plan.range option)
      (s : Plan.agg_spec)
  : bool
  =
  match s.Plan.func with
  | Ast.Agg_min | Ast.Agg_max ->
    range = None
    && n_eq < Array.length idx_ords
    &&
      (match s.Plan.col_ord with
      | Some i ->
        idx_ords.(n_eq) = i
        &&
          (match List.nth_opt table_meta.Cat.columns i with
          | Some (c : Row.column) -> c.Row.ty <> Row.Real
          | None -> false)
      | None -> false)
  | _ -> true

and index_cover_min_early_stop (pred_opt : Plan.expr option) (aggs : Plan.agg_spec list)
  : bool
  =
  pred_opt = None
  &&
  match aggs with
  | [ { Plan.func = Ast.Agg_min; col_ord = Some _; distinct = false; arg_expr = None } ]
    -> true
  | _ -> false

and index_cover_eligible
      (cat : Cat.t)
      (table_meta : Cat.table_meta)
      (idx_tree : int)
      (keys : (int * Row.ty * Plan.expr) list)
      (range : Plan.range option)
      (pred_opt : Plan.expr option)
      (aggs : Plan.agg_spec list)
  : (int array * bool) option
  =
  let idx_infos = Cat.indexes_for_table cat ~table:table_meta.Cat.name in
  match
    List.find_opt (fun (i : Cat.index_info) -> i.Cat.idx_tree_id = idx_tree) idx_infos
  with
  | None -> None
  | Some idx_info ->
    (match index_cover_idx_ords table_meta idx_info with
     | None -> None
     | Some idx_ords ->
       let n_eq = List.length keys in
       let ok_col = index_cover_ok_col table_meta idx_ords in
       if not (List.for_all (index_cover_agg_ok ok_col) aggs)
       then None
       else if not (index_cover_pred_ok ok_col pred_opt)
       then None
       else if
         not (List.for_all (index_cover_minmax_ok table_meta idx_ords n_eq range) aggs)
       then None
       else Some (idx_ords, index_cover_min_early_stop pred_opt aggs))

(* #674 (item 1 of 3): entry point for the covering-index aggregate fast
   path — checks eligibility, then folds the accumulators over [idx_tree]
   directly, never touching the table tree. *)
and run_aggregate_fast_path_index
      clock
      params
      store
      mode
      (cat : Cat.t option)
      (table_meta : Cat.table_meta)
      (idx_tree : int)
      (keys : (int * Row.ty * Plan.expr) list)
      (range : Plan.range option)
      (pred_opt : Plan.expr option)
      (aggs : Plan.agg_spec list)
      proj
  : Row.t Lwt_stream.t option Lwt.t
  =
  match cat with
  | None -> Lwt.return None
  | Some c ->
    (match index_cover_eligible c table_meta idx_tree keys range pred_opt aggs with
     | None -> Lwt.return None
     | Some (idx_ords, min_early_stop) ->
       run_index_cover_walk
         clock
         params
         store
         mode
         cat
         table_meta
         idx_tree
         idx_ords
         keys
         range
         pred_opt
         aggs
         proj
         ~min_early_stop)

(* #674 (item 1 of 3): the covering walk itself.  Structurally the same shape
   as [run_aggregate_fast_path]'s loop — fold accumulators over a cursor, emit
   exactly one output row — but seeking [idx_tree] and decoding
   [Index_key.decode]'d columns instead of [rh_get]-ing and decoding a table
   row.  MUST stay byte-identical to [aggregate_one]/[make_agg_acc_over_values]
   for the values it does read, which is why it reuses [make_agg_acc]
   unchanged rather than re-deriving MIN/MAX/COUNT comparison logic. *)
and run_index_cover_walk
      clock
      params
      store
      mode
      (cat : Cat.t option)
      (table_meta : Cat.table_meta)
      (idx_tree : int)
      (idx_ords : int array)
      (keys : (int * Row.ty * Plan.expr) list)
      (range : Plan.range option)
      (pred_opt : Plan.expr option)
      (aggs : Plan.agg_spec list)
      proj
      ~min_early_stop
  : Row.t Lwt_stream.t option Lwt.t
  =
  match
    let accs = List.map (make_agg_acc clock params) aggs in
    if List.exists Option.is_none accs
    then None
    else Some (Array.of_list (List.map Option.get accs))
  with
  | None -> Lwt.return None
  | Some accs ->
    let* pred' =
      match pred_opt with
      | None -> Lwt.return None
      | Some p ->
        let* p' = pre_eval_subquery clock store params cat p in
        Lwt.return (Some p')
    in
    let finalize_stream () =
      let agg_vals = Array.map (fun (_, fin) -> fin ()) accs in
      let out =
        Array.of_list
          (List.map
             (function
               | Plan.PI_agg_slot k -> agg_vals.(k)
               | Plan.PI_expr e -> eval_expr clock params agg_vals e
               | Plan.PI_group_col _ | Plan.PI_window_slot _ ->
                 assert false (* excluded by [aggregate_fast_path] above *))
             proj)
      in
      Lwt.return (Some (Lwt_stream.of_list [ out ]))
    in
    let vs = List.map (fun (_, ty, e) -> eval_expr clock params [||] e, ty) keys in
    (* Same NULL/type-mismatch gate the general index path uses
       ([stream_index_lookup]/[index_lookup_values]): a NULL or type-mismatched
       equality-bound value matches no rows at all. *)
    (match index_lookup_values vs with
     | None -> finalize_stream ()
     | Some lookup_vs ->
       let prefix, plen = encode_index_key_prefix lookup_vs in
       let seek_key, past_end = range_seek_bounds clock params ~prefix ~plen range in
       let col_tys =
         Array.of_list
           (List.map (fun (c : Row.column) -> c.Row.ty) table_meta.Cat.columns)
       in
       let n_table_cols = Array.length col_tys in
       let decode_ikey ikey =
         match Index_key.decode ikey with
         | Error _ -> None
         | Ok (col_vals, _rowid) ->
           let row = Array.make n_table_cols Row.V_null in
           List.iteri
             (fun pos iv ->
                if pos < Array.length idx_ords
                then (
                  let ord = idx_ords.(pos) in
                  row.(ord) <- index_value_to_row_value col_tys.(ord) iv))
             col_vals;
           Some row
       in
       (* #262: read through the active txn so the covering walk sees rows
          written earlier in the same open transaction, exactly like
          [stream_index_lookup] and [run_aggregate_fast_path]. *)
       let* rh = rh_begin store mode in
       let* cur = rh_seek_ge rh idx_tree seek_key in
       let s_opt = Lwt.get query_stats_key in
       let ended = ref false in
       let finish () =
         if !ended
         then Lwt.return_unit
         else (
           ended := true;
           S.seek_close cur;
           rh_finish rh)
       in
       Lwt.catch
         (fun () ->
            let rec loop () =
              let* kv = S.seek_next cur in
              match kv with
              | None ->
                let* () = finish () in
                finalize_stream ()
              | Some (ikey, _ivalue) ->
                if index_key_in_range ~prefix ~plen ~past_end ikey
                then (
                  (* #546-style: counted before any decode, same discipline as
                     [stream_index_lookup] — an entry walked is an entry
                     walked whether or not it survives the residual predicate.
                     [rows_examined] (table fetches) is never incremented at
                     all on this path — that is the whole point of #674. *)
                  incr_index_entries s_opt;
                  match decode_ikey ikey with
                  | None -> loop () (* malformed entry: skip rather than crash *)
                  | Some row ->
                    let keep =
                      match pred' with
                      | None -> true
                      | Some p -> value_truthy (eval_expr clock params row p)
                    in
                    if keep then Array.iter (fun (upd, _) -> upd row) accs;
                    if keep && min_early_stop
                    then
                      let* () = finish () in
                      finalize_stream ()
                    else loop ())
                else
                  let* () = finish () in
                  finalize_stream ()
            in
            loop ())
         (fun exn ->
            let* () = finish () in
            Lwt.fail exn))

and run_aggregate_fast_path clock params store mode cat table_meta pred_opt aggs proj =
  match
    let accs = List.map (make_agg_acc clock params) aggs in
    if List.exists Option.is_none accs
    then None
    else Some (Array.of_list (List.map Option.get accs))
  with
  | None -> Lwt.return None
  | Some accs ->
    let* pred' =
      match pred_opt with
      | None -> Lwt.return None
      | Some p ->
        let* p' = pre_eval_subquery clock store params cat p in
        Lwt.return (Some p')
    in
    (* No decode needed when nothing reads a column and there is no filter: a
       pure COUNT-star loop runs at storage-cursor speed. *)
    let max_col =
      List.fold_left
        (fun m (s : Plan.agg_spec) ->
           match s.Plan.col_ord with
           | Some i when i > m -> i
           | _ -> m)
        (-1)
        aggs
    in
    (* #488: an aggregate over an EXPRESSION reads whichever columns the
       expression names, and [max_col] cannot see them — its [col_ord] is
       [None].  Both of the decode shortcuts below are keyed off [max_col], so
       both must be switched off for such a spec: skipping the decode entirely
       would accumulate over an empty row (a silent wrong answer, not a crash),
       and pruning to a [max_col] of -1 would decode no columns at all.  The
       test is on the spec list, not on the projection, because an expression
       aggregate combined with a COUNT-star in one select list must still
       decode.
       Widening the prefix to the columns an argument expression mentions is a
       later optimisation; correctness first. *)
    let any_arg_expr =
      List.exists (fun (s : Plan.agg_spec) -> s.Plan.arg_expr <> None) aggs
    in
    (* #491 x #488: DISTINCT deliberately adds NO term to either shortcut below,
       and that is a proof rather than an omission.  DISTINCT is a modifier on
       an argument that must EXIST — the grammar has no [COUNT(DISTINCT * )] and
       [Ast.E_agg_distinct] carries a mandatory expression — so every DISTINCT
       spec has either [col_ord = Some i] (which already lifts [max_col] to
       [>= i], forcing the decode AND keeping [i] inside the pruned prefix) or
       [arg_expr = Some _] (which [any_arg_expr] already catches).  The dedup
       reads exactly the argument value and nothing else, so it can never widen
       the set of columns that must be decoded beyond what the argument itself
       already forces.  If DISTINCT ever becomes legal without an argument,
       this reasoning dies with it — which is why [make_agg_acc] refuses that
       shape rather than letting it reach here. *)
    let need_decode = pred_opt <> None || max_col >= 0 || any_arg_expr in
    (* #247: when no filter reads other columns and there are no virtual columns
       to recompute, decode only the [0, max_col] prefix — skipping trailing
       columns (e.g. a TEXT payload) the aggregate never touches. *)
    let can_prune =
      pred_opt = None
      && (not any_arg_expr)
      && not (has_virtual_cols table_meta.Cat.columns)
    in
    let decode_row vbytes =
      if can_prune
      then Row.decode_prefix table_meta.Cat.columns vbytes ~upto:max_col
      else decode_with_virtual clock params table_meta vbytes
    in
    (* #262: fold over the active txn when one is open, so a COUNT/SUM reflects
       rows written earlier in the same uncommitted transaction. *)
    let* rh = rh_begin store mode in
    let agg_tree_id, _, _, _ = Cat.row_storage table_meta in
    let* cur = rh_seek_ge rh agg_tree_id Bytes.empty in
    let ended = ref false in
    let finish () =
      if !ended
      then Lwt.return_unit
      else (
        ended := true;
        S.seek_close cur;
        rh_finish rh)
    in
    let dummy = [||] in
    let s_opt = Lwt.get query_stats_key in
    Lwt.catch
      (fun () ->
         let rec loop () =
           let* kv = S.seek_next cur in
           match kv with
           | None ->
             let* () = finish () in
             let agg_vals = Array.map (fun (_, fin) -> fin ()) accs in
             let out =
               Array.of_list
                 (List.map
                    (function
                      | Plan.PI_agg_slot k -> agg_vals.(k)
                      | Plan.PI_expr e -> eval_expr clock params agg_vals e
                      | Plan.PI_group_col _ | Plan.PI_window_slot _ ->
                        assert false (* excluded above *))
                    proj)
             in
             Lwt.return (Some (Lwt_stream.of_list [ out ]))
           | Some (_key, vbytes) ->
             incr_examined s_opt;
             if need_decode
             then (
               let row = decode_row vbytes in
               let keep =
                 match pred' with
                 | None -> true
                 | Some p -> value_truthy (eval_expr clock params row p)
               in
               if keep then Array.iter (fun (upd, _) -> upd row) accs)
             else Array.iter (fun (upd, _) -> upd dummy) accs;
             loop ()
         in
         loop ())
      (fun exn ->
         let* () = finish () in
         Lwt.fail exn)

and stream_aggregate
      clock
      params
      store
      mode
      cat
      child
      group_cols
      aggs
      having
      proj
      agg_windows
  =
  let* fast =
    aggregate_fast_path
      clock
      params
      store
      mode
      cat
      child
      group_cols
      aggs
      having
      proj
      agg_windows
  in
  match fast with
  | Some stream -> Lwt.return stream
  | None ->
    (* #257: as in [stream_filter], the stats scope has to be re-established
       around any subquery evaluated here. *)
    let s_opt = Lwt.get query_stats_key in
    let* inner = to_stream clock params store ~mode ~cat child in
    let* rows = Lwt_stream.to_list inner in
    let n_group_cols = List.length group_cols in
    (* #558: an uncorrelated subquery beside an aggregate resolves once, here.
       Before #558 [Sema.bind_expr_agg] refused the whole shape, so nothing in
       this path had ever seen one. *)
    let* having =
      match having with
      | None -> Lwt.return None
      | Some p ->
        let+ p' = pre_eval_subquery clock store params cat p in
        Some p'
    in
    let* proj =
      Lwt_list.map_s
        (function
          | Plan.PI_expr e ->
            let+ e' = pre_eval_subquery clock store params cat e in
            Plan.PI_expr e'
          | other -> Lwt.return other)
        proj
    in
    (* #664: an aggregate's ARGUMENT is an expression too (#488), so it can
       carry a subquery, and until #664 [Sema.bind_agg_arg] refused the shape
       outright because nothing here resolved it — a surviving [P_subquery]
       reads [Row.V_null], so [SUM(qty * (SELECT 2))] would have answered NULL
       and [COUNT(price * (SELECT 1))] 0, with no error.  An uncorrelated one
       resolves once, here, exactly as [having] and [proj] do just above. *)
    let* aggs =
      Lwt_list.map_s
        (fun (spec : Plan.agg_spec) ->
           match spec.Plan.arg_expr with
           | None -> Lwt.return spec
           | Some e ->
             let+ e' = pre_eval_subquery clock store params cat e in
             { spec with Plan.arg_expr = Some e' })
        aggs
    in
    (* An ARGUMENT's subquery that survives that is correlated against the
       INPUT row — the argument is evaluated once per scanned row, before any
       grouping — so it resolves the way [stream_expr_project] resolves a
       correlated projection, and NOT the way [resolve] below resolves
       [having]/[proj] against the aggregate output row.  Two different rules
       for two different rows; see [agg_arg_subquery_refusal]. *)
    let* rows, aggs =
      resolve_correlated_agg_args clock params store mode cat ~s_opt child rows aggs
    in
    (* What survives is correlated. Its only possible source in the aggregate
       output row is a grouped column; [binding_of_group_cols] maps those back
       to their table and column, and anything else is refused rather than
       silently answered [NULL]. *)
    let correlated =
      (match having with
       | Some p -> plan_expr_has_subquery p
       | None -> false)
      || List.exists
           (function
             | Plan.PI_expr e -> plan_expr_has_subquery e
             | _ -> false)
           proj
    in
    let metas = if correlated then get_outer_scan_metas child else None in
    let resolve agg_row e =
      if not (plan_expr_has_subquery e)
      then Lwt.return e
      else (
        match metas with
        | None -> Lwt.fail_with (agg_subquery_refusal ())
        | Some metas ->
          let bnd = binding_of_group_cols metas group_cols agg_row in
          (* #493: no cache here — this site still substitutes LITERALS, so the
             substituted statement differs per aggregate output row and a cache
             would only grow. The number of rows is bounded by the group count
             rather than the input, so it was never the #493 hot path. *)
          let* r =
            with_pull_context ~stats:s_opt ~mode ~cache:None (fun () ->
              pre_eval_subquery
                clock
                store
                params
                cat
                (substitute_outer_in_plan_expr ~cat bnd e))
          in
          if plan_expr_has_subquery r
          then Lwt.fail_with (agg_subquery_refusal ())
          else Lwt.return r)
    in
    let groups = aggregate_build_groups group_cols rows in
    let agg_output_rows =
      List.map
        (fun (group_key, group_rows) ->
           let agg_vals =
             List.map (fun spec -> aggregate_one clock params spec group_rows) aggs
           in
           Array.of_list (group_key @ agg_vals))
        groups
    in
    let* after_having =
      match having with
      | None -> Lwt.return agg_output_rows
      (* The pure arm is the pre-#558 code, kept because every aggregate query
         that has no subquery at all goes through it. *)
      | Some pred when not correlated ->
        Lwt.return
          (List.filter
             (fun r -> value_truthy (eval_expr clock params r pred))
             agg_output_rows)
      | Some pred ->
        Lwt_list.filter_s
          (fun r ->
             let* pred' = resolve r pred in
             Lwt.return (value_truthy (eval_expr clock params r pred')))
          agg_output_rows
    in
    let n_agg_cols = n_group_cols + List.length aggs in
    let with_windows = aggregate_apply_windows clock params agg_windows after_having in
    let project_slot agg_row = function
      | Plan.PI_group_col i -> agg_row.(i)
      | Plan.PI_agg_slot k -> agg_row.(n_group_cols + k)
      | Plan.PI_window_slot j -> agg_row.(n_agg_cols + j)
      (* #507: window slots inside [e] were already rewritten to
         [n_agg_cols + j] by the planner. *)
      | Plan.PI_expr e -> eval_expr clock params agg_row e
    in
    let* final_rows =
      if not correlated
      then
        Lwt.return
          (List.map
             (fun agg_row -> Array.of_list (List.map (project_slot agg_row) proj))
             with_windows)
      else
        Lwt_list.map_s
          (fun agg_row ->
             let+ vals =
               Lwt_list.map_s
                 (function
                   | Plan.PI_expr e ->
                     let+ e' = resolve agg_row e in
                     eval_expr clock params agg_row e'
                   | other -> Lwt.return (project_slot agg_row other))
                 proj
             in
             Array.of_list vals)
          with_windows
    in
    Lwt.return (Lwt_stream.of_list final_rows)

(* #664: resolve the subqueries that survive [pre_eval_subquery] in an
   aggregate ARGUMENT.  The identity on both inputs unless one actually
   survived — which is the uncommon case, so the ordinary aggregate pays one
   [List.exists] over the spec list and nothing else.

   Such a subquery is correlated against the INPUT row, so its correlation
   source is the child's own scan metas and the resolution runs per input row:
   [stream_expr_project]'s treatment of a correlated projection, not
   [stream_aggregate]'s per-group one.

   The resolved ARGUMENT VALUE is parked in a hidden trailing slot appended to
   its row, and the spec's [arg_expr] is rewritten to read that slot.  Doing it
   once up front rather than inside the accumulator is what keeps the subquery
   evaluated exactly once per row: #491's DISTINCT filter and [aggregate_one]
   both read the argument through [agg_arg_getter], and a [P_subquery] left in
   place would have had to be resolved separately by each.

   Widening the row is safe because everything that indexes an input row here
   indexes a PREFIX of it — [group_cols] are child ordinals — and the aggregate
   OUTPUT row is built as [group_key @ agg_vals], so no hidden slot can escape
   into a result. *)
and resolve_correlated_agg_args
      clock
      params
      store
      mode
      (cat : Cat.t option)
      ~s_opt
      child
      (rows : Row.t list)
      (aggs : Plan.agg_spec list)
  : (Row.t list * Plan.agg_spec list) Lwt.t
  =
  if not (List.exists agg_arg_is_correlated aggs)
  then Lwt.return (rows, aggs)
  else (
    match get_outer_scan_metas child with
    (* Checked before the empty-[rows] shortcut below: an unresolvable
       correlation must be refused on an empty table too, or the refusal would
       depend on the data. *)
    | None -> Lwt.fail_with (agg_arg_subquery_refusal ())
    | Some metas ->
      widen_rows_for_agg_args clock params store mode cat ~s_opt ~metas rows aggs)

and agg_arg_is_correlated (s : Plan.agg_spec) : bool =
  match s.Plan.arg_expr with
  | Some e -> plan_expr_has_subquery e
  | None -> false

and widen_rows_for_agg_args
      clock
      params
      store
      mode
      (cat : Cat.t option)
      ~s_opt
      ~(metas : outer_input list)
      (rows : Row.t list)
      (aggs : Plan.agg_spec list)
  : (Row.t list * Plan.agg_spec list) Lwt.t
  =
  match rows with
  (* No row ever reaches [agg_arg_getter], so there is no argument to
     evaluate and nothing to widen.  The specs keep their [P_subquery], which
     is unreachable rather than wrong. *)
  | [] -> Lwt.return (rows, aggs)
  | first :: _ ->
    (* Rows from one child are uniform in width, as everywhere else here. *)
    let width = Array.length first in
    let corr =
      List.filter_map
        (fun (s : Plan.agg_spec) ->
           if agg_arg_is_correlated s then s.Plan.arg_expr else None)
        aggs
    in
    (* #493: the same parameterize-and-cache treatment [stream_expr_project]
       gets, and for the same reason — the cardinality here is the INPUT rows,
       which is that issue's hot path.  ([stream_aggregate]'s own [resolve]
       passes [cache:None] because it runs per GROUP, which is bounded much
       lower.) *)
    let parameterized = not (List.exists plan_expr_subqueries_use_param corr) in
    let cache = if parameterized then Some (Hashtbl.create 4) else None in
    let base = Array.length params in
    let* rows' =
      Lwt_list.map_s
        (fun (row : Row.t) ->
           let bnd =
             if parameterized
             then param_binding_of_metas ~base ~row_len:(Array.length row) metas
             else binding_of_metas metas row
           in
           let row_params = if parameterized then Array.append params row else params in
           let+ vals =
             Lwt_list.map_s
               (resolve_agg_arg_for_row
                  clock
                  params
                  store
                  mode
                  cat
                  ~s_opt
                  ~cache
                  ~bnd
                  ~row_params
                  row)
               corr
           in
           Array.append row (Array.of_list vals))
        rows
    in
    Lwt.return (rows', rewrite_agg_arg_slots width aggs)

(* One correlated argument, one input row: substitute the outer references from
   that row, resolve, and evaluate to the value the accumulator will consume.
   A subquery that survives the substitution named an outer column no input
   carries, or an ambiguous one; refuse rather than let [eval_expr] answer NULL
   for it, exactly as [stream_expr_project] does. *)
and resolve_agg_arg_for_row
      clock
      params
      store
      mode
      (cat : Cat.t option)
      ~s_opt
      ~cache
      ~bnd
      ~row_params
      (row : Row.t)
      (e : Plan.expr)
  : Row.value Lwt.t
  =
  let e_subst = substitute_outer_in_plan_expr ~cat bnd e in
  let* resolved =
    with_pull_context ~stats:s_opt ~mode ~cache (fun () ->
      pre_eval_subquery clock store row_params cat e_subst)
  in
  if plan_expr_has_subquery resolved
  then Lwt.fail_with (agg_arg_subquery_refusal ())
  else Lwt.return (eval_expr clock params row resolved)

(* The specs' half of [widen_rows_for_agg_args]: each correlated argument reads
   the hidden slot its value was appended to.  Iterates [aggs] in the same
   order, under the same predicate, as the [corr] list the values were computed
   from — that correspondence is what makes the slot numbers line up, so the
   two must not be given separate filters. *)
and rewrite_agg_arg_slots (width : int) (aggs : Plan.agg_spec list) : Plan.agg_spec list =
  let slot = ref (width - 1) in
  List.map
    (fun (s : Plan.agg_spec) ->
       if not (agg_arg_is_correlated s)
       then s
       else (
         incr slot;
         { s with Plan.arg_expr = Some (Plan.P_col !slot) }))
    aggs

and read_fts_content_rows store mode (fts_meta : Cat.fts_table_meta)
  : (int64 * string list) list Lwt.t
  =
  (* #330: read the FTS content tree as (rowid, column-texts) pairs through [mode]
     (the same shared snapshot / explicit txn the dump uses for table rows), so
     [Db.dump] can emit INSERTs that carry the original rowids and round-trip the
     index exactly.  The content tree is keyed by rowid, so this is the only place
     FTS rowids are surfaced — deliberately out-of-band, not via a SQL projection
     (see #330). *)
  with_read store mode (fun rh ->
    let* cur = rh_cursor_open rh fts_meta.Cat.fts_content_tree in
    let _sr = S.cursor_first cur in
    let acc = ref [] in
    let rec walk () =
      match S.cursor_next cur with
      | None -> ()
      | Some (k, v) ->
        acc := (Rowid.decode k, fts_decode_content v) :: !acc;
        walk ()
    in
    walk ();
    S.cursor_close cur;
    Lwt.return (List.rev !acc))

and stream_fts_seq_scan clock params store mode (fts_meta : Cat.fts_table_meta) where =
  (* #257: captured at construction (inside [query]'s [with_value] scope), same
     pattern as the table scanners; every content row scanned counts as examined
     regardless of the WHERE filter. *)
  let s_opt = Lwt.get query_stats_key in
  (* #262: scan the content tree through the active txn so an in-transaction
     write to the FTS table is visible to the scan. *)
  let* rh = rh_begin store mode in
  let* cur = rh_cursor_open rh fts_meta.Cat.fts_content_tree in
  let _sr = S.cursor_first cur in
  let exhausted = ref false in
  let ended = ref false in
  let finish () =
    if !ended
    then Lwt.return_unit
    else (
      ended := true;
      S.cursor_close cur;
      rh_finish rh)
  in
  register_stream_cleanup finish;
  let rec read_next () =
    if !exhausted
    then Lwt.return_none
    else (
      match S.cursor_next cur with
      | None ->
        exhausted := true;
        let%lwt () = finish () in
        Lwt.return_none
      | Some (_key, val_bytes) ->
        incr_examined s_opt;
        let texts = fts_decode_content val_bytes in
        let row = Array.of_list (List.map (fun s -> Row.V_text s) texts) in
        let emit =
          match where with
          | None -> true
          | Some pred -> value_truthy (eval_expr clock params row pred)
        in
        if emit then Lwt.return_some row else read_next ())
  in
  Lwt.return
    (Lwt_stream.from (fun () ->
       Lwt.catch read_next (fun exn ->
         exhausted := true;
         let%lwt () = finish () in
         Lwt.fail exn)))

and stream_fts_match_scan
      _clock
      _params
      store
      mode
      (fts_meta : Cat.fts_table_meta)
      query
      proj
      include_rank
      snippets
      limit
      offset
  =
  (* #257: each matched FTS index row counts as one examined row — the index
     seek (and the content fetch it drives) is the work this scan does.  The
     increment sits on the match, ahead of the content [S.get], so a match whose
     content row is absent still counts: the seek happened regardless. *)
  let s_opt = Lwt.get query_stats_key in
  (* #262: run the index query and content fetches through the active txn (when
     one is open) so an in-transaction write to the FTS table is matched and
     returned.  The body delegates the raw txn to the FTS helpers, so it is made
     polymorphic over the txn kind rather than using a [read_handle]. *)
  let body : type a. a S.txn -> Row.t Lwt_stream.t Lwt.t =
    fun tx ->
    let* matches = fts_execute_query tx ~index_tree:fts_meta.Cat.fts_index_tree query in
    let* scored_matches = fts_score_matches tx fts_meta query matches include_rank in
    let sorted =
      if include_rank
      then List.sort (fun (_, _, s1) (_, _, s2) -> Float.compare s2 s1) scored_matches
      else scored_matches
    in
    (* #687: slice to the [offset, offset+limit) window BEFORE the content
       fetch loop below — sorting needs every score, but the content fetch
       and snippet computation that follow are per-row work that only the
       returned rows need to pay for. Mirrors [finalize_select]'s semantics:
       an OFFSET with no LIMIT is a no-op (matches the plain-table / FTS seq
       scan path), and LIMIT/OFFSET never change which rows are picked, only
       how many of the sorted list are fetched. [list_drop]/[list_take]
       (review finding 3) touch only [offset + limit] cons cells rather than
       walking the full match list with a [List.filteri] predicate. *)
    let sorted =
      match limit with
      | None -> sorted
      | Some n ->
        let off = Option.value ~default:0 offset in
        list_take n (list_drop off sorted)
    in
    let snippet_terms = fts_query_terms_with_kind query in
    let* rows =
      Lwt_list.filter_map_s
        (fun (rowid, _positions, score) ->
           incr_examined s_opt;
           let key = Rowid.encode rowid in
           let* val_opt = S.get tx fts_meta.Cat.fts_content_tree key in
           match val_opt with
           | None -> Lwt.return None
           | Some bytes ->
             let texts = fts_decode_content bytes in
             let full_row = Array.of_list (List.map (fun s -> Row.V_text s) texts) in
             let projected =
               if proj = [] && snippets = []
               then Array.to_list full_row
               else List.map (fun i -> full_row.(i)) proj
             in
             let snippet_vals =
               List.map
                 (fun (spec : Plan.snippet_spec) ->
                    let col_text =
                      let idx =
                        if spec.Plan.col_idx < 0
                        then 0
                        else min spec.Plan.col_idx (max 0 (List.length texts - 1))
                      in
                      if texts = [] then "" else List.nth texts idx
                    in
                    Row.V_text
                      (compute_snippet ~col_text ~query_terms:snippet_terms ~spec))
                 snippets
             in
             let row_values =
               projected
               @ (if include_rank then [ Row.V_real score ] else [])
               @ snippet_vals
             in
             Lwt.return (Some (Array.of_list row_values)))
        sorted
    in
    Lwt.return (Lwt_stream.of_list rows)
  in
  match mode with
  | In_txn tx -> body tx
  | In_ro_txn tx -> body tx
  | Auto -> S.with_ro store body

and stream_pragma_integrity_check store cat =
  let cat_val =
    match cat with
    | None -> failwith "Exec.to_stream: Op_pragma_integrity_check requires catalog"
    | Some c -> c
  in
  let* tables = Cat.list_tables cat_val in
  let errors = ref [] in
  let add_err msg = errors := msg :: !errors in
  let count_entries tx tid =
    let count = ref 0 in
    let* cur = S.cursor_open tx tid in
    let _sr = S.cursor_first cur in
    let rec go () =
      match S.cursor_next cur with
      | None -> Lwt.return_unit
      | Some _ ->
        incr count;
        go ()
    in
    let* () = go () in
    S.cursor_close cur;
    Lwt.return !count
  in
  S.with_ro store
  @@ fun tx ->
  let* () =
    Lwt_list.iter_s
      (fun (meta : Cat.table_meta) ->
         match meta.Cat.storage with
         | Cat.Columnar _ -> Lwt.return_unit
         | Cat.Row { tree_id; _ } ->
           let* row_count = count_entries tx tree_id in
           let idxs = Cat.indexes_for_table cat_val ~table:meta.name in
           Lwt_list.iter_s
             (fun (idx : Cat.index_info) ->
                let is_partial = idx.idx_where_sql <> None in
                let* idx_count = count_entries tx idx.idx_tree_id in
                if (not is_partial) && idx_count <> row_count
                then
                  add_err
                    (Printf.sprintf
                       "index %s on %s: %d entries != %d rows"
                       idx.idx_name
                       meta.name
                       idx_count
                       row_count);
                Lwt.return_unit)
             idxs)
      tables
  in
  let result = List.rev !errors in
  let rows =
    if result = []
    then [ [| Row.V_text "ok" |] ]
    else List.map (fun msg -> [| Row.V_text msg |]) result
  in
  Lwt.return (Lwt_stream.of_list rows)

(* #563: the (ordinal, name) of every column whose loaded schema declares NOT
   NULL and whose stored cell therefore has to hold a value.  VIRTUAL
   generated columns are excluded because they are stored as NULL and
   recomputed on read.

   #629 deliberately does NOT follow the write path here.  [not_null_violation]
   stopped exempting VIRTUAL columns and recomputes them instead, because a
   write can be refused; this scan reports on cells that are already on disk and
   whose only repair action is to rewrite the cell — which is meaningless for a
   column that is never read from disk in the first place.  The exclusion is
   about what [repair] can act on, not about where the truth lives. *)
and not_null_scan_cols (meta : Cat.table_meta) : (int * string) list =
  meta.Cat.columns
  |> List.mapi (fun i (c : Row.column) -> i, c)
  |> List.filter_map (fun (i, (c : Row.column)) ->
    if c.Row.not_null && not (not_null_exempt_col c) then Some (i, c.Row.name) else None)

(* #563: a columnstore table's rows never enter a B-tree, so they are scanned
   through [Col_store.to_row_seq] instead of a cursor.  There are no rowids to
   hand back — the store exposes no delete — so the victim list is empty and
   only the count is reported.  [not_null_repair] refuses such a table rather
   than reporting a repair it did not perform. *)
and not_null_count_columnar (meta : Cat.table_meta) (cols : (int * string) list) =
  let cs = col_store_of_meta meta in
  let counts = List.map (fun (i, name) -> i, name, ref 0) cols in
  Seq.iter
    (fun (row : Row.t) ->
       List.iter
         (fun (i, _, n) -> if i < Array.length row && row.(i) = Row.V_null then incr n)
         counts)
    (Granary_columnar.Col_store.to_row_seq cs);
  List.filter_map (fun (_i, name, n) -> if !n = 0 then None else Some (name, !n)) counts

(* #600: the counting half of the scan, which is all [PRAGMA not_null_check]
   ever needs — it emits (table, column, count) and nothing else.

   [not_null_scan_table] below retains every violating row so the repair can
   delete it (and, before #630, drained the whole tree to find them), and the
   shape this command exists for is a legacy file whose
   declared-NOT NULL column is wholly NULL: retaining there costs O(table)
   resident memory in the one command an operator runs FIRST, on a database
   whose scope of damage is still unknown.  Being OOM-killed while surveying
   the damage is the worst possible moment for it, so the report path counts
   each row and drops it.  The columnstore arm always worked this way; this is
   the row-store arm catching up.

   It scans through [S.seek_ge]/[S.seek_next] rather than [S.cursor_open],
   which is the other half of the same problem: [cursor_open] drains the whole
   tree into a list up front (the #228/#229 finding), so retaining nothing
   downstream of it would still have left the report O(table).  [seek_ge] from
   the empty key positions before the first entry in O(log n) and yields one
   [(key, value)] at a time. *)
and not_null_count_table : type m. m S.txn -> Cat.table_meta -> (string * int) list Lwt.t =
  fun tx meta ->
  let cols = not_null_scan_cols meta in
  if cols = []
  then Lwt.return []
  else (
    match meta.Cat.storage with
    | Cat.Columnar _ -> Lwt.return (not_null_count_columnar meta cols)
    | Cat.Row { tree_id; _ } ->
      let counters = List.map (fun (i, name) -> i, name, ref 0) cols in
      let note row (i, _, n) =
        if i < Array.length row && row.(i) = Row.V_null then incr n
      in
      let* cur = S.seek_ge tx tree_id Bytes.empty in
      let rec go () =
        let* nxt = S.seek_next cur in
        match nxt with
        | None -> Lwt.return_unit
        | Some (_k, v) ->
          let row = Row.decode meta.Cat.columns v in
          List.iter (note row) counters;
          go ()
      in
      let* () = go () in
      S.seek_close cur;
      Lwt.return
        (List.filter_map
           (fun (_i, name, n) -> if !n = 0 then None else Some (name, !n))
           counters))

(* #563: scan one table for stored NULLs in a declared-NOT NULL column.
   Returns the per-column violation counts — one entry per VIOLATING column,
   clean columns dropped, so empty counts mean the table honours its own
   schema — paired with the violating rows themselves, which is what the
   repair deletes.  A row violating two NOT NULL columns is counted under both
   but appears ONCE in the victim list.

   #600: this is the REPAIR path only; the report uses [not_null_count_table],
   which retains nothing.

   #630: the SCAN streams and the VICTIM BUFFER retains — the two halves are
   separate and only the first was ever the defect.

   - The scan runs on [S.seek_ge]/[S.seek_next], not [S.cursor_open].
     [cursor_open] drains the entire tree into a list before the first
     violation is examined (the #228/#229 finding), so [PRAGMA
     not_null_repair] on a large but mostly CLEAN table paid O(table) resident
     memory to find a handful of violators.  [seek_ge] from the empty key
     positions before the first entry in O(log n) and yields one [(key, value)]
     at a time, so what the scan holds is now one decoded row, not the table.
     This is the same move #600 made for the report path.
   - The victim buffer stays collected-and-sorted before the rows are fetched,
     and that is deliberate: #541 found that fetching in index-key order costs
     up to a page read per row once the table outgrows the pager cache.  Its
     bound is one TABLE's violations (see [stream_pragma_not_null_repair]), not
     the whole database's, and it is O(violations) rather than O(table) — a
     clean table now costs nothing at all.

   Splitting the counts from the victims (they used to share one per-column
   bucket of retained rows) is what makes the second bound exact: a row
   violating [k] columns previously occupied [k] list cells and was deduped
   only later, in [repair_not_null_table]. *)
and not_null_scan_table
  : type m.
    m S.txn -> Cat.table_meta -> ((string * int) list * (int64 * Row.t) list) Lwt.t
  =
  fun tx meta ->
  let cols = not_null_scan_cols meta in
  if cols = []
  then Lwt.return ([], [])
  else (
    match meta.Cat.storage with
    | Cat.Columnar _ -> Lwt.return (not_null_count_columnar meta cols, [])
    | Cat.Row { tree_id; _ } ->
      let counters = List.map (fun (i, name) -> i, name, ref 0) cols in
      let victims = ref [] in
      (* One pass over the row: bump EVERY column it violates (the counts are
         per column) but retain the row at most ONCE.  Both helpers are lifted
         out of [go] so the scan loop stays flat. *)
      let bump row hit (i, _, n) =
        if i < Array.length row && row.(i) = Row.V_null
        then (
          incr n;
          hit := true)
      in
      let note k row =
        let hit = ref false in
        List.iter (bump row hit) counters;
        if !hit then victims := (Rowid.decode k, row) :: !victims
      in
      let* cur = S.seek_ge tx tree_id Bytes.empty in
      let rec go () =
        let* nxt = S.seek_next cur in
        match nxt with
        | None -> Lwt.return_unit
        | Some (k, v) ->
          note k (Row.decode meta.Cat.columns v);
          go ()
      in
      let* () = go () in
      S.seek_close cur;
      let counts =
        List.filter_map
          (fun (_i, name, n) -> if !n = 0 then None else Some (name, !n))
          counters
      in
      Lwt.return (counts, List.rev !victims))

(* #563: the report row shape shared by check and repair — (table, column, n).
   [n] is the number of offending rows for [not_null_check] and the number of
   offending rows actually DELETED for [not_null_repair]; on a row-store table
   the two coincide, and where they cannot (a columnstore, which has no delete)
   the difference is precisely what the operator has to see. *)
and not_null_report_rows (meta : Cat.table_meta) ~counted found =
  List.map
    (fun (name, n) ->
       [| Row.V_text meta.Cat.name
        ; Row.V_text name
        ; Row.V_int (Int64.of_int (counted n))
       |])
    found

(* #563 report mode: [PRAGMA not_null_check].  One row per offending (table,
   column) with the number of stored rows that hold NULL there, so an operator
   sees the whole scope before touching anything.  A clean database returns no
   rows.  Strictly read-only — the destructive half is [not_null_repair].

   [mode] is honoured rather than always taking a fresh RO snapshot: inside an
   explicit transaction the report must see that transaction's own writes, or
   [BEGIN; PRAGMA not_null_repair; PRAGMA not_null_check] would contradict a
   plain SELECT in the same transaction and tell the operator the repair
   failed (read-your-own-writes, #262). *)
and stream_pragma_not_null_check store mode cat =
  let cat_val =
    match cat with
    | None -> failwith "Exec.to_stream: Op_pragma_not_null_check requires catalog"
    | Some c -> c
  in
  let* tables = Cat.list_tables cat_val in
  let body : type m. m S.txn -> Row.t Lwt_stream.t Lwt.t =
    fun tx ->
    let* per_table =
      Lwt_list.map_s
        (fun (meta : Cat.table_meta) ->
           (* #600: counts only — the report never looks at a violating row, so
              it must not hold one. *)
           let* found = not_null_count_table tx meta in
           Lwt.return (not_null_report_rows meta ~counted:Fun.id found))
        tables
    in
    Lwt.return (Lwt_stream.of_list (List.concat per_table))
  in
  match mode with
  | In_txn tx -> body tx
  | In_ro_txn tx -> body tx
  | Auto -> S.with_ro store body

(* #563 repair mode: [PRAGMA not_null_repair] DELETEs the rows the report
   names, through the ordinary delete path so index entries and ON DELETE
   cascades are honoured.  Reports the same (table, column, count) shape as the
   check; a row violating two NOT NULL columns is counted under both but
   deleted once, so the counts are per-column violations, not a total.

   #588: this is the shared CORE, reached from both entry points —
   [stream_pragma_not_null_repair] below (the query path, which streams the
   report rows) and [execute_with_count]'s [Op_pragma_not_null_repair] arm (the
   write path, which reports the rows deleted as the statement's change count),
   the latter through [not_null_repair_run_ref].  Splitting it is what keeps the
   two from being able to disagree about what the repair does. *)
and not_null_repair_run store mode (cat_val : Cat.t) =
  (* #588: a read-only ambient snapshot ([Db.query_as_of], or any [In_ro_txn])
     used to reach [acquire_txn] and fail with "write attempted under a
     read-only transaction (In_ro_txn)" — a storage-layer message about an
     internal mode, from a statement whose problem is that it is destructive.
     Refuse at the statement level instead, and name the read-only half that
     DOES work against a snapshot. *)
  (match mode with
   | In_ro_txn _ ->
     failwith
       "PRAGMA not_null_repair deletes rows and cannot run against a read-only snapshot \
        or transaction; survey it with PRAGMA not_null_check, and repair it on a \
        writable connection"
   | Auto | In_txn _ -> ());
  let* tables = Cat.list_tables cat_val in
  (* Reuse the ambient write txn when there is one: opening our own would block
     on the write lock the caller already holds. *)
  let* tx, owned = acquire_txn store mode in
  Lwt.catch
    (fun () ->
       (* #600: scan and repair one table at a time.  Scanning every table
          first made the peak victim buffer the SUM over the whole database
          rather than the largest single table; nothing needed the earlier
          scans once their table was repaired.  Interleaving also makes the
          reported counts truer: a repair whose ON DELETE CASCADE removes rows
          from a later table no longer reports them as separately found. *)
       let* per_table =
         Lwt_list.map_s
           (fun (meta : Cat.table_meta) ->
              let* counts, victims = not_null_scan_table tx meta in
              repair_not_null_table tx cat_val meta ~counts ~victims)
           tables
       in
       let* () = release_txn ~cat:cat_val tx owned in
       (* #630: [per_table] is one entry per TABLE holding that table's report
          rows — O(tables), not O(violations) and not O(table).  The victim
          buffer it was derived from is already gone. *)
       let rows = List.concat_map fst per_table in
       let deleted = List.fold_left (fun acc (_, n) -> acc + n) 0 per_table in
       Lwt.return (rows, deleted))
    (fun exn ->
       let* () = if owned then S.rollback tx else Lwt.return_unit in
       Lwt.fail exn)

and stream_pragma_not_null_repair store mode cat =
  let cat_val =
    match cat with
    | None -> failwith "Exec.to_stream: Op_pragma_not_null_repair requires catalog"
    | Some c -> c
  in
  let* rows, _deleted = not_null_repair_run store mode cat_val in
  Lwt.return (Lwt_stream.of_list rows)

(* #563: delete one table's NOT NULL violators, returning its report rows.

   A columnstore is append-only — [Col_store] exposes no delete — so its rows
   cannot be repaired.  It reports [0] deleted rather than raising: the raise
   escapes [Db.query] uncaught (it happens inside the returned promise, past
   the [exception Failure] guard in [Db.query_impl]), which would make an
   unrepairable table crash the caller instead of informing it.

   {b The count-0 row is a standalone signal, not a cross-reference.}  A table
   with nothing to fix produces NO row at all (the [counts = []] branch below),
   so a row whose count is 0 is emitted in exactly one situation: findings that
   could not be deleted.  "Nothing to fix" and "cannot fix" are therefore
   distinguishable from the repair output alone — empty versus a 0-count row —
   and [PRAGMA not_null_check] then gives the size of what was left behind.
   Both cases are pinned in [test_not_null_567.ml] ("clean table works" and
   "report sees a columnstore violation").

   {b Why the 0-count row survived #588, which made the raise expressible.}
   #627 fixed [Db.query_impl]'s guard ([Lwt.catch] around the whole call, not
   [| exception Failure msg ->] on it), and #588 put the repair on the write
   path too, whose [Lwt.catch] always converted a [Failure] — so a raise here
   WOULD now surface as [Error (Runtime msg)] on both entry points.  It is
   still not taken, for a reason #588 did not weigh: the raise happens partway
   through [Lwt_list.map_s] over the tables, and [not_null_repair_run] rolls the
   whole repair back.  A database holding one unrepairable columnstore would
   therefore become unrepairable ENTIRELY — there is no per-table spelling of
   this PRAGMA to fall back on — which trades a convention an operator can act
   on for a refusal they cannot.  Anything that reopens this owes a per-table
   repair first, or a pre-pass that refuses before deleting anything.

   Returns the report rows paired with the number of rows actually DELETED
   (#588): the write path reports that as the statement's change count, and it
   is the deduplicated row count, not the sum of the per-column counts. *)
and repair_not_null_table tx (cat_val : Cat.t) (meta : Cat.table_meta) ~counts ~victims =
  if counts = []
  then Lwt.return ([], 0)
  else if Cat.is_columnar meta
  then Lwt.return (not_null_report_rows meta ~counted:(fun _ -> 0) counts, 0)
  else (
    let indexes = Cat.indexes_for_table cat_val ~table:meta.Cat.name in
    let* child_refs =
      if Cat.get_fk_enforcement cat_val
      then build_child_refs cat_val ~parent_table_name:meta.Cat.name
      else Lwt.return []
    in
    (* #541: the rows are fetched in ascending rowid order, never index-key
       order.  [not_null_scan_table] already yields them that way (its seek
       walks the data tree ascending) and already deduplicates; the sort is
       kept because THIS is where the ordering the delete depends on is
       required, and it must not become an accident of the scan. *)
    let victims = List.sort_uniq (fun (a, _) (b, _) -> Int64.compare a b) victims in
    let* () =
      Lwt_list.iter_s
        (fun ((rowid, row) as m) ->
           let* () =
             apply_delete_row
               tx
               cat_val
               meta
               ~clock:None
               ~params:[||]
               ~child_refs
               ~indexes
               m
           in
           record_change meta.Cat.name (Deleted { rowid; row });
           Lwt.return_unit)
        victims
    in
    mark_dirty meta.Cat.name;
    Lwt.return (not_null_report_rows meta ~counted:Fun.id counts, List.length victims))

and stream_sqlite_master store cat =
  let cat_val =
    match cat with
    | None -> failwith "Exec.to_stream: Op_sqlite_master requires catalog"
    | Some c -> c
  in
  let* tables = Cat.list_tables cat_val in
  let table_rows =
    List.map
      (fun (meta : Cat.table_meta) ->
         let sm_tree_id =
           match meta.Cat.storage with
           | Cat.Row { tree_id; _ } -> tree_id
           | Cat.Columnar _ -> 0
         in
         [| Row.V_text "table"
          ; Row.V_text meta.Cat.name
          ; Row.V_text meta.Cat.name
          ; Row.V_int (Int64.of_int sm_tree_id)
          ; Row.V_text
              (ddl_of_table
                 ~indexes:(Cat.indexes_for_table cat_val ~table:meta.Cat.name)
                 meta)
         |])
      tables
  in
  let index_rows =
    List.concat_map
      (fun (meta : Cat.table_meta) ->
         List.map
           (fun (idx : Cat.index_info) ->
              [| Row.V_text "index"
               ; Row.V_text idx.Cat.idx_name
               ; Row.V_text idx.Cat.idx_table
               ; Row.V_int (Int64.of_int idx.Cat.idx_tree_id)
               ; Row.V_text (ddl_of_index idx)
              |])
           (Cat.indexes_for_table cat_val ~table:meta.Cat.name))
      tables
  in
  let* views = Cat.load_all_views store in
  let view_rows =
    List.map
      (fun (name, sql) ->
         [| Row.V_text "view"
          ; Row.V_text name
          ; Row.V_text name
          ; Row.V_int 0L
          ; Row.V_text sql
         |])
      views
  in
  let* triggers = Cat.load_all_triggers store in
  let trigger_rows =
    List.map
      (fun (name, sql) ->
         let tbl_name = trigger_table_of_sql name sql in
         [| Row.V_text "trigger"
          ; Row.V_text name
          ; Row.V_text tbl_name
          ; Row.V_int 0L
          ; Row.V_text sql
         |])
      triggers
  in
  let fts_rows =
    List.map
      (fun (m : Cat.fts_table_meta) ->
         [| Row.V_text "table"
          ; Row.V_text m.Cat.fts_name
          ; Row.V_text m.Cat.fts_name
          ; Row.V_int (Int64.of_int m.Cat.fts_content_tree)
          ; Row.V_text (ddl_of_fts m)
         |])
      (Cat.list_fts_tables cat_val)
  in
  (* #312: sqlite_sequence appears in sqlite_master once any AUTOINCREMENT
     table exists (matching SQLite — independent of whether a row has been
     inserted yet). *)
  let seq_rows =
    if
      List.exists
        (fun (m : Cat.table_meta) ->
           match m.Cat.storage with
           | Cat.Row { autoincrement = true; _ } -> true
           | _ -> false)
        tables
    then
      [ [| Row.V_text "table"
         ; Row.V_text "sqlite_sequence"
         ; Row.V_text "sqlite_sequence"
         ; Row.V_int 0L
         ; Row.V_text "CREATE TABLE sqlite_sequence(name,seq)"
        |]
      ]
    else []
  in
  Lwt.return
    (Lwt_stream.of_list
       (table_rows @ seq_rows @ index_rows @ view_rows @ trigger_rows @ fts_rows))

and stream_sqlite_sequence cat =
  let cat_val =
    match cat with
    | None -> failwith "Exec.to_stream: Op_sqlite_sequence requires catalog"
    | Some c -> c
  in
  let* tables = Cat.list_tables cat_val in
  let rows =
    List.filter_map
      (fun (m : Cat.table_meta) ->
         match m.Cat.storage with
         | Cat.Row { autoincrement = true; next_rowid; _ }
           when not (Int64.equal next_rowid Cat.empty_next_rowid) ->
           Some [| Row.V_text m.Cat.name; Row.V_int (Int64.sub next_rowid 1L) |]
         | _ -> None)
      tables
  in
  Lwt.return (Lwt_stream.of_list rows)

and stream_union clock params store mode cat all left right =
  let* ls = to_stream clock params store ~mode ~cat left in
  let* rs = to_stream clock params store ~mode ~cat right in
  let combined = Lwt_stream.append ls rs in
  if all
  then Lwt.return combined
  else
    let* rows = Lwt_stream.to_list combined in
    let seen = Hashtbl.create 64 in
    (* #722: as for DISTINCT, the compound's column collations come from the
       LEFT arm's projection. *)
    let key_of = collated_row_keyer (output_collations left) in
    let deduped =
      List.filter
        (fun row ->
           let k = key_of row in
           if Hashtbl.mem seen k
           then false
           else (
             Hashtbl.replace seen k ();
             true))
        rows
    in
    Lwt.return (Lwt_stream.of_list deduped)

and stream_intersect clock params store mode cat left right =
  let* ls = to_stream clock params store ~mode ~cat left in
  let* rs = to_stream clock params store ~mode ~cat right in
  let* right_list = Lwt_stream.to_list rs in
  let key_of = collated_row_keyer (output_collations left) in
  let right_set = Hashtbl.create (max 1 (List.length right_list)) in
  List.iter (fun r -> Hashtbl.replace right_set (key_of r) ()) right_list;
  let* left_list = Lwt_stream.to_list ls in
  let seen = Hashtbl.create 64 in
  let result =
    List.filter
      (fun row ->
         let k = key_of row in
         if (not (Hashtbl.mem right_set k)) || Hashtbl.mem seen k
         then false
         else (
           Hashtbl.replace seen k ();
           true))
      left_list
  in
  Lwt.return (Lwt_stream.of_list result)

and stream_except clock params store mode cat left right =
  let* ls = to_stream clock params store ~mode ~cat left in
  let* rs = to_stream clock params store ~mode ~cat right in
  let* right_list = Lwt_stream.to_list rs in
  let key_of = collated_row_keyer (output_collations left) in
  let right_set = Hashtbl.create (max 1 (List.length right_list)) in
  List.iter (fun r -> Hashtbl.replace right_set (key_of r) ()) right_list;
  let* left_list = Lwt_stream.to_list ls in
  let seen = Hashtbl.create 64 in
  let result =
    List.filter
      (fun row ->
         let k = key_of row in
         if Hashtbl.mem right_set k || Hashtbl.mem seen k
         then false
         else (
           Hashtbl.replace seen k ();
           true))
      left_list
  in
  Lwt.return (Lwt_stream.of_list result)

and stream_insert_returning
      clock
      params
      store
      mode
      cat
      (table_meta : Cat.table_meta)
      ordinals
      values
      on_conflict
      returning
      upsert_update
  =
  match cat with
  | None -> failwith "Exec.query: RETURNING requires catalog context"
  | Some c ->
    let* result_lists =
      Lwt_list.map_s
        (fun row_vals ->
           let n = List.length table_meta.columns in
           let inserted_row = Array.make n Row.V_null in
           List.iter2
             (fun ord e -> inserted_row.(ord) <- eval_expr clock params [||] e)
             ordinals
             row_vals;
           let* inserted =
             execute_insert
               ~mode
               ~clock
               ~on_conflict
               ~upsert_update
               ~prebuilt_row:(Some inserted_row)
               store
               c
               ~table_meta
               ~ordinals
               ~values:row_vals
           in
           if not inserted
           then Lwt.return []
           else (
             let result =
               Array.of_list (List.map (eval_expr clock params inserted_row) returning)
             in
             Lwt.return [ result ]))
        values
    in
    Lwt.return (Lwt_stream.of_list (List.concat result_lists))

and stream_update_returning
      clock
      params
      store
      mode
      cat
      (table_meta : Cat.table_meta)
      assignments
      where
      seek
      order
      limit
      offset
      indexes
      returning
  =
  (* Project RETURNING from the rows actually written INSIDE the update's write
     txn (via [collect]), not a separate pre-lock RO snapshot — so concurrent
     `UPDATE ... RETURNING` callers see read-from-the-write values, never stale
     or duplicated ones (#226).  Rows arrive in update order (post
     order/offset/limit), which is also the RETURNING order. *)
  let c =
    match cat with
    | Some c -> c
    | None -> failwith "Exec.to_stream: UPDATE RETURNING requires catalog context"
  in
  let acc = ref [] in
  let collect new_row =
    acc := Array.of_list (List.map (eval_expr clock params new_row) returning) :: !acc
  in
  let* _ =
    execute_update
      ~mode
      ~params
      ~clock
      ~collect:(Some collect)
      store
      c
      ~table_meta
      ~assignments
      ~where
      ~seek
      ~order
      ~limit
      ~offset
      ~indexes
  in
  Lwt.return (Lwt_stream.of_list (List.rev !acc))

and stream_delete_returning
      clock
      params
      store
      mode
      cat
      (table_meta : Cat.table_meta)
      where
      seek
      order
      limit
      offset
      indexes
      returning
  =
  (* Project RETURNING from the rows actually deleted INSIDE the delete's write
     txn (via [collect]), not a separate pre-lock RO snapshot (#226). *)
  let c =
    match cat with
    | Some c -> c
    | None -> failwith "Exec.to_stream: DELETE RETURNING requires catalog context"
  in
  let acc = ref [] in
  let collect old_row =
    acc := Array.of_list (List.map (eval_expr clock params old_row) returning) :: !acc
  in
  let* _ =
    execute_delete
      ~mode
      ~params
      ~clock
      ~collect:(Some collect)
      store
      c
      ~table_meta
      ~where
      ~seek
      ~order
      ~limit
      ~offset
      ~indexes
  in
  Lwt.return (Lwt_stream.of_list (List.rev !acc))

and stream_const_select clock params store cat exprs =
  let raw_exprs = List.map fst exprs in
  let* exprs' = Lwt_list.map_s (pre_eval_subquery clock store params cat) raw_exprs in
  let row = Array.of_list (List.map (eval_expr clock params [||]) exprs') in
  Lwt.return (Lwt_stream.of_list [ row ])

and stream_with_cte_recursive clock params store mode cat cte_name def query =
  let base_op, recursive_arm =
    match def with
    | Plan.Op_union { all = true; left; right } -> left, right
    | _ ->
      failwith
        "Exec: recursive CTE def must be UNION ALL — non-UNION-ALL recursive CTEs are \
         not supported"
  in
  let* base_stream = to_stream clock params store ~mode ~cat base_op in
  let* seed_rows = Lwt_stream.to_list base_stream in
  let max_iterations = 1000 in
  let rec iterate depth acc working =
    if working = []
    then Lwt.return acc
    else if depth >= max_iterations
    then
      failwith
        (Printf.sprintf
           "Exec: recursive CTE '%s' exceeded maximum iteration depth of %d"
           cte_name
           max_iterations)
    else (
      let patched_arm = substitute_cte ~cte_name ~rows:working recursive_arm in
      let* new_stream = to_stream clock params store ~mode ~cat patched_arm in
      let* new_rows = Lwt_stream.to_list new_stream in
      iterate (depth + 1) (acc @ new_rows) new_rows)
  in
  let* all_rows = iterate 0 seed_rows seed_rows in
  let patched_query = substitute_cte ~cte_name ~rows:all_rows query in
  to_stream clock params store ~mode ~cat patched_query

and stream_window clock params store mode cat child windows =
  let* child_stream = to_stream clock params store ~mode ~cat child in
  let* all_rows = Lwt_stream.to_list child_stream in
  let n_rows = List.length all_rows in
  if n_rows = 0
  then Lwt.return (Lwt_stream.of_list [])
  else (
    let all_rows_arr = Array.of_list all_rows in
    let n_windows = List.length windows in
    let window_results : Row.value array array =
      Array.init n_windows (fun wi ->
        let wplan = List.nth windows wi in
        let indexed_rows = List.mapi (fun i row -> i, row) all_rows in
        let partitions =
          group_by_partition clock params wplan.Plan.partition_by indexed_rows
        in
        let combined = Array.make n_rows Row.V_null in
        List.iter
          (fun (_, partition_idx_rows) ->
             let sorted =
               sort_partition_by clock params wplan.Plan.order_by partition_idx_rows
             in
             let part_results =
               compute_window_for_partition clock params wplan sorted n_rows
             in
             List.iter
               (fun (orig_idx, _) -> combined.(orig_idx) <- part_results.(orig_idx))
               sorted)
          partitions;
        combined)
    in
    let augmented =
      Array.to_list
        (Array.mapi
           (fun i row ->
              let extras = Array.init n_windows (fun wi -> window_results.(wi).(i)) in
              Array.append row extras)
           all_rows_arr)
    in
    Lwt.return (Lwt_stream.of_list augmented))

and stream_explain clock params store mode cat analyze inner =
  let plan_rows = explain_plan inner in
  let nullify row = Array.append row [| Row.V_null; Row.V_null |] in
  if not analyze
  then Lwt.return (Lwt_stream.of_list (List.map nullify plan_rows))
  else (
    let cat_v =
      match cat with
      | Some c -> c
      | None -> failwith "EXPLAIN ANALYZE requires a catalog"
    in
    let t0 =
      match clock with
      | Some c -> c ()
      | None -> 0.0
    in
    let* n =
      let is_write =
        match inner with
        | Plan.Op_insert _
        | Plan.Op_insert_select _
        | Plan.Op_update _
        | Plan.Op_delete _
        | Plan.Op_create_table _
        | Plan.Op_create_index _
        | Plan.Op_drop_table _
        | Plan.Op_drop_index _
        | Plan.Op_alter_table _
        | Plan.Op_begin
        | Plan.Op_commit
        | Plan.Op_rollback
        | Plan.Op_savepoint _
        | Plan.Op_release _
        | Plan.Op_rollback_to _
        | Plan.Op_create_view _
        | Plan.Op_create_reactive_view _
        | Plan.Op_drop_view _
        | Plan.Op_drop_reactive_view _
        | Plan.Op_create_trigger _
        | Plan.Op_drop_trigger _
        | Plan.Op_pragma_set_user_version _
        | Plan.Op_pragma_set_fk _
        | Plan.Op_pragma_set_recursive_triggers _
        | Plan.Op_pragma_set_defer_fk _
        | Plan.Op_pragma_set_wal_autocheckpoint _
        | Plan.Op_pragma_set_synchronous _
        | Plan.Op_pragma_set_wal_batch_commits _
        | Plan.Op_pragma_set_wal_batch_interval_ms _
        | Plan.Op_fts_insert _
        | Plan.Op_fts_delete _
        | Plan.Op_create_fts_table _ -> true
        | _ -> false
      in
      if is_write
      then execute_with_count ~mode ~clock ~params store cat_v inner
      else
        let* s = to_stream clock params store ~mode ~cat inner in
        let* rows = Lwt_stream.to_list s in
        Lwt.return (List.length rows)
    in
    let elapsed_ms =
      match clock with
      | Some c -> (c () -. t0) *. 1000.0
      | None -> 0.0
    in
    let rows =
      List.mapi
        (fun i row ->
           if i = 0
           then Array.append row [| Row.V_int (Int64.of_int n); Row.V_real elapsed_ms |]
           else nullify row)
        plan_rows
    in
    Lwt.return (Lwt_stream.of_list rows))

and to_stream
      (clock : (unit -> float) option)
      (params : Row.value array)
      (store : S.t)
      ?(mode : txn_mode = Auto)
      ?(cat : Cat.t option = None)
      (op : Plan.op)
  : Row.t Lwt_stream.t Lwt.t
  =
  match op with
  | Plan.Op_seq_scan { table_meta; _ } ->
    stream_seq_scan clock params store mode table_meta
  | Plan.Op_col_seq_scan { table_meta; _ } ->
    stream_col_seq_scan clock params store mode table_meta
  | Plan.Op_filter { pred; child } -> stream_filter clock params store mode cat pred child
  | Plan.Op_project
      { ordinals; child = Plan.Op_rowid_lookup { table_meta; lookup_val; _ } } ->
    (* #416: fuse the projection into the point lookup.  [Lwt_stream.map] builds
       a whole second [Lwt_stream] over the one the lookup already returns, and
       its source is the ASYNC [Lwt_stream.from] one, so draining a single row
       through it costs a promise chain per element on top of the stream record
       itself — measured at ~230 words per warm point lookup, ~16% of the total.
       Applying the (pure) ordinal selection to the at-most-one row the lookup
       produces is observationally identical and pays none of it. *)
    stream_rowid_lookup ~project:ordinals clock params store mode lookup_val table_meta
  | Plan.Op_project { ordinals; child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    Lwt.return (Lwt_stream.map (project_row ordinals) inner)
  | Plan.Op_expr_project { exprs; child } ->
    stream_expr_project clock params store mode cat exprs child
  | Plan.Op_sort { keys; child } -> stream_sort clock params store mode cat keys child
  | Plan.Op_limit { limit; offset; child } ->
    (* #677 (item 2 of 3): pull only [offset + limit] rows from the child
       instead of draining it fully — [Sema.validate_limit_offset] already
       rejects a negative [limit]/[offset] before an [Op_limit] node can
       exist, so [want] is always >= 0 and [pull] terminates.  The cleanup
       registry (see [stream_cleanup_key]) is what makes stopping early safe:
       any of the 3 lazy scanners constructed while building [inner] register
       their [finish] into [cleanups], and flushing it here after the pull —
       whether or not the child was actually exhausted — releases whatever
       reader handle/cursor they still hold. *)
    let cleanups = ref [] in
    let* inner =
      Lwt.with_value stream_cleanup_key (Some cleanups) (fun () ->
        to_stream clock params store ~mode ~cat child)
    in
    let want = offset + limit in
    let rec pull n acc =
      if n <= 0
      then Lwt.return (List.rev acc)
      else
        let* v = Lwt_stream.get inner in
        match v with
        | None -> Lwt.return (List.rev acc)
        | Some row -> pull (n - 1) (row :: acc)
    in
    let* rows = pull want [] in
    let* () = Lwt_list.iter_s (fun f -> f ()) !cleanups in
    let rows' = List.filteri (fun i _ -> i >= offset) rows in
    Lwt.return (Lwt_stream.of_list rows')
  | Plan.Op_distinct { child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    let seen = Hashtbl.create 64 in
    (* #722: dedup under each output column's collation, so
       [SELECT DISTINCT x COLLATE NOCASE] still folds 'HELLO' and 'Hello' into
       one row — while emitting the stored value rather than a lower-cased one. *)
    let key_of = collated_row_keyer (output_collations child) in
    Lwt.return
      (Lwt_stream.filter
         (fun row ->
            let k = key_of row in
            if Hashtbl.mem seen k
            then false
            else (
              Hashtbl.replace seen k ();
              true))
         inner)
  | Plan.Op_index_lookup { table_tree; idx_tree; keys; range; table_meta; _ } ->
    stream_index_lookup clock params store mode table_tree idx_tree keys range table_meta
  | Plan.Op_rowid_lookup { table_meta; lookup_val; _ } ->
    stream_rowid_lookup clock params store mode lookup_val table_meta
  | Plan.Op_nested_loop_join
      { left
      ; right_meta
      ; right_alias = _
      ; idx_tree
      ; probe
      ; probe_range
      ; join_kind
      ; right_col_offset = _
      ; n_right_cols
      } ->
    stream_nested_loop_join
      clock
      params
      store
      mode
      cat
      left
      right_meta
      idx_tree
      probe
      probe_range
      join_kind
      n_right_cols
  | Plan.Op_hash_join
      { left
      ; right
      ; left_key
      ; right_key
      ; on_pred
      ; join_kind
      ; right_col_offset = _
      ; n_right_cols
      } ->
    stream_hash_join
      clock
      params
      store
      mode
      cat
      left
      right
      left_key
      right_key
      on_pred
      join_kind
      n_right_cols
  | Plan.Op_aggregate { child; group_cols; aggs; having; proj; windows = agg_windows } ->
    stream_aggregate
      clock
      params
      store
      mode
      cat
      child
      group_cols
      aggs
      having
      proj
      agg_windows
  | Plan.Op_fts_seq_scan { fts_meta; where } ->
    stream_fts_seq_scan clock params store mode fts_meta where
  | Plan.Op_fts_match_scan
      { fts_meta; query; proj; include_rank; snippets; limit; offset } ->
    stream_fts_match_scan
      clock
      params
      store
      mode
      fts_meta
      query
      proj
      include_rank
      snippets
      limit
      offset
  | Plan.Op_pragma_rows { rows } -> Lwt.return (Lwt_stream.of_list rows)
  | Plan.Op_pragma_get_user_version ->
    S.with_ro store
    @@ fun tx ->
    let* v = Cat.read_user_version_tx tx in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int v |] ])
  | Plan.Op_pragma_get_fk ->
    let v =
      match cat with
      | None -> false
      | Some cat -> Cat.get_fk_enforcement cat
    in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int (if v then 1L else 0L) |] ])
  | Plan.Op_pragma_get_recursive_triggers ->
    let v =
      match cat with
      | None -> true
      | Some cat -> Cat.get_recursive_triggers cat
    in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int (if v then 1L else 0L) |] ])
  | Plan.Op_pragma_get_wal_autocheckpoint ->
    let n = S.wal_autocheckpoint store in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int (Int64.of_int n) |] ])
  | Plan.Op_pragma_get_synchronous ->
    let s = S.string_of_durability (S.durability store) in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_text s |] ])
  (* #638: (total_failures, consecutive_failures, last_error).  [last_error] is
     NULL when no checkpoint has failed since the last one that completed. *)
  | Plan.Op_pragma_checkpoint_status ->
    let h = S.checkpoint_health store in
    let last =
      match h.S.last_error with
      | None -> Row.V_null
      | Some m -> Row.V_text m
    in
    Lwt.return
      (Lwt_stream.of_list
         [ [| Row.V_int (Int64.of_int h.S.total_failures)
            ; Row.V_int (Int64.of_int h.S.consecutive_failures)
            ; last
           |]
         ])
  (* #637: (status, frames_walked, header_frames, detail).  The detail column
     carries the caveat in words, because the status alone is easy to over-read:
     [no_evidence] is a statement about the frames THIS OPEN replayed, never a
     clean bill of health for the database. *)
  | Plan.Op_pragma_wal_replay_check ->
    let c = S.wal_replay_check store in
    let status, detail =
      match c.S.status with
      | S.Wal_replay_stale_generation { frame_idx; previous_txn_id; frame_txn_id } ->
        ( "stale_generation"
        , Printf.sprintf
            "WAL recovery walked into an older generation at frame %d: a header page \
             carries txn_id %Ld, not above the %Ld already seen.  This database was \
             written by a pre-#636 binary, whose checkpoint left the checkpointed \
             generation verifying on disk, so recovery has replayed stale pages over \
             newer ones.  Committed rows may be missing.  Restore from a backup taken \
             before the affected checkpoint, or dump what is readable (PRAGMA \
             integrity_check first) and reload."
            frame_idx
            frame_txn_id
            previous_txn_id )
      | S.Wal_replay_no_evidence ->
        ( "no_evidence"
        , "The frames WAL recovery replayed at this open showed no generation \
           regression.  This is not a clean bill of health: a pre-#636 stale replay from \
           an EARLIER open is already in the main file, leaves a structurally valid \
           database, and is not detectable here or by PRAGMA integrity_check." )
      | S.Wal_replay_not_examined ->
        ( "not_examined"
        , "Nothing to examine: no WAL, or fewer than two header-page frames were \
           recovered, so there was no txn_id sequence to compare.  This is not a \
           statement either way about the database." )
    in
    Lwt.return
      (Lwt_stream.of_list
         [ [| Row.V_text status
            ; Row.V_int (Int64.of_int c.S.frames_walked)
            ; Row.V_int (Int64.of_int c.S.header_frames)
            ; Row.V_text detail
           |]
         ])
  | Plan.Op_pragma_get_wal_batch_commits ->
    let n = S.sync_batch_commits store in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int (Int64.of_int n) |] ])
  | Plan.Op_pragma_get_wal_batch_interval_ms ->
    let n = S.sync_batch_interval_ms store in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int (Int64.of_int n) |] ])
  | Plan.Op_pragma_get_defer_fk ->
    let v =
      match cat with
      | None -> false
      | Some cat -> Cat.get_defer_fks_pragma cat
    in
    Lwt.return (Lwt_stream.of_list [ [| Row.V_int (if v then 1L else 0L) |] ])
  | Plan.Op_pragma_integrity_check -> stream_pragma_integrity_check store cat
  | Plan.Op_pragma_not_null_check -> stream_pragma_not_null_check store mode cat
  | Plan.Op_pragma_not_null_repair -> stream_pragma_not_null_repair store mode cat
  | Plan.Op_sqlite_master -> stream_sqlite_master store cat
  | Plan.Op_sqlite_sequence -> stream_sqlite_sequence cat
  | Plan.Op_union { all; left; right } ->
    stream_union clock params store mode cat all left right
  | Plan.Op_intersect { left; right } ->
    stream_intersect clock params store mode cat left right
  | Plan.Op_except { left; right } -> stream_except clock params store mode cat left right
  | Plan.Op_insert { table_meta; returning; _ }
    when returning <> [] && Cat.is_columnar table_meta ->
    Lwt.fail_with "RETURNING is not supported on columnar tables"
  | Plan.Op_insert { table_meta; ordinals; values; on_conflict; returning; upsert_update }
    when returning <> [] ->
    stream_insert_returning
      clock
      params
      store
      mode
      cat
      table_meta
      ordinals
      values
      on_conflict
      returning
      upsert_update
  | Plan.Op_update { table_meta; returning; _ }
    when returning <> [] && Cat.is_columnar table_meta ->
    Lwt.fail_with "RETURNING is not supported on columnar tables"
  | Plan.Op_update
      { table_meta; assignments; where; seek; order; limit; offset; indexes; returning }
    when returning <> [] ->
    stream_update_returning
      clock
      params
      store
      mode
      cat
      table_meta
      assignments
      where
      seek
      order
      limit
      offset
      indexes
      returning
  | Plan.Op_delete { table_meta; returning; _ }
    when returning <> [] && Cat.is_columnar table_meta ->
    Lwt.fail_with "RETURNING is not supported on columnar tables"
  | Plan.Op_delete { table_meta; where; seek; order; limit; offset; indexes; returning }
    when returning <> [] ->
    stream_delete_returning
      clock
      params
      store
      mode
      cat
      table_meta
      where
      seek
      order
      limit
      offset
      indexes
      returning
  | Plan.Op_changes ->
    failwith "Exec.to_stream: Op_changes must be intercepted in db.ml query"
  | Plan.Op_last_insert_rowid ->
    failwith "Exec.to_stream: Op_last_insert_rowid must be intercepted in db.ml query"
  | Plan.Op_total_changes ->
    failwith "Exec.to_stream: Op_total_changes must be intercepted in db.ml query"
  | Plan.Op_const_select { exprs } -> stream_const_select clock params store cat exprs
  | Plan.Op_with_cte { cte_name; def; query; recursive = false } ->
    let* def_stream = to_stream clock params store ~mode ~cat def in
    let* cte_rows = Lwt_stream.to_list def_stream in
    let patched = substitute_cte ~cte_name ~rows:cte_rows query in
    to_stream clock params store ~mode ~cat patched
  | Plan.Op_with_cte { cte_name; def; query; recursive = true } ->
    stream_with_cte_recursive clock params store mode cat cte_name def query
  | Plan.Op_cte_scan { cte_name; _ } ->
    failwith
      (Printf.sprintf
         "Exec: unsubstituted Op_cte_scan '%s' — internal planner error"
         cte_name)
  | Plan.Op_window { child; windows; n_input_cols = _ } ->
    stream_window clock params store mode cat child windows
  | Plan.Op_no_op -> Lwt.return (Lwt_stream.of_list [])
  | Plan.Op_explain { analyze; inner } ->
    stream_explain clock params store mode cat analyze inner
  | Plan.Op_create_table _
  | Plan.Op_col_create_table _
  | Plan.Op_create_index _
  | Plan.Op_drop_table _
  | Plan.Op_drop_index _
  | Plan.Op_create_fts_table _
  | Plan.Op_fts_insert _
  | Plan.Op_fts_delete _
  | Plan.Op_alter_table _
  | Plan.Op_create_view _
  | Plan.Op_create_reactive_view _
  | Plan.Op_drop_view _
  | Plan.Op_drop_reactive_view _
  | Plan.Op_create_trigger _
  | Plan.Op_drop_trigger _
  | Plan.Op_begin
  | Plan.Op_commit
  | Plan.Op_rollback
  | Plan.Op_savepoint _
  | Plan.Op_release _
  | Plan.Op_rollback_to _
  | Plan.Op_pragma_set_user_version _
  | Plan.Op_pragma_set_fk _
  | Plan.Op_pragma_set_recursive_triggers _
  | Plan.Op_pragma_set_defer_fk _
  | Plan.Op_pragma_set_wal_autocheckpoint _
  | Plan.Op_pragma_wal_checkpoint
  | Plan.Op_vacuum
  | Plan.Op_attach _
  | Plan.Op_detach _
  | Plan.Op_active_database_set _ ->
    failwith "Exec.query: use Exec.execute for write operations"
  | Plan.Op_database_list | Plan.Op_active_database_get ->
    failwith "Exec.query: routed via Db.query (no Db handle)"
  | Plan.Op_insert _ | Plan.Op_insert_select _ | Plan.Op_update _ | Plan.Op_delete _ ->
    failwith "Exec.query: use Exec.execute for write operations"
  | Plan.Op_seq_set _ | Plan.Op_seq_reset _ ->
    failwith "Exec.query: use Exec.execute for write operations"
  | Plan.Op_pragma_set_synchronous _
  | Plan.Op_pragma_set_wal_batch_commits _
  | Plan.Op_pragma_set_wal_batch_interval_ms _ ->
    failwith "Exec.query: use Exec.execute for write operations"
;;

(* Wire the forward reference so execute_with_count can call to_stream for
   Op_insert_select.  This runs once at module initialization time, after both
   functions are fully defined in the let-rec block above. *)
let () = to_stream_ref := to_stream

(* #588: see [not_null_repair_run_ref]. *)
let () = not_null_repair_run_ref := not_null_repair_run

(* ------------------------------------------------------------------ *)
(* Public query entry point                                             *)
(* ------------------------------------------------------------------ *)

(* #239: [used_index] is a plan-time fact — does the query's base access reach
   the data through an index/rowid/FTS seek, or a full table scan?  Descends
   through the row-shaping wrappers to the base; [true] if ANY base uses a seek.
   A nested-loop join always probes its right table by index, so it counts. *)
let rec op_uses_index (op : Plan.op) : bool =
  match op with
  | Plan.Op_index_lookup _ | Plan.Op_rowid_lookup _ | Plan.Op_fts_match_scan _ -> true
  | Plan.Op_seq_scan _ | Plan.Op_col_seq_scan _ | Plan.Op_fts_seq_scan _ -> false
  | Plan.Op_filter { child; _ }
  | Plan.Op_project { child; _ }
  | Plan.Op_expr_project { child; _ }
  | Plan.Op_sort { child; _ }
  | Plan.Op_limit { child; _ }
  | Plan.Op_distinct { child }
  | Plan.Op_aggregate { child; _ }
  | Plan.Op_window { child; _ } -> op_uses_index child
  | Plan.Op_nested_loop_join _ -> true
  | Plan.Op_hash_join { left; right; _ } -> op_uses_index left || op_uses_index right
  | Plan.Op_union { left; right; _ }
  | Plan.Op_intersect { left; right }
  | Plan.Op_except { left; right } -> op_uses_index left || op_uses_index right
  | Plan.Op_with_cte { query; _ } -> op_uses_index query
  (* Conservative: anything not recognised as a seek-bearing base access (or a
     wrapper over one) reports [false].  KEEP IN SYNC: a NEW index/seek-bearing
     plan op added here would silently report [used_index = false] until a case
     is added above. *)
  | _ -> false
;;

let query
      ?(mode = Auto)
      ?(clock : (unit -> float) option = None)
      ?(params = [||])
      ?(stats : query_stats option)
      (store : S.t)
      (cat : Cat.t)
      (op : Plan.op)
  : Row.t Lwt_stream.t Lwt.t
  =
  let body () =
    match stats with
    | None -> to_stream clock params store ~mode ~cat:(Some cat) op
    | Some s ->
      s.used_index <- op_uses_index op;
      (* Run the whole stream construction under the stats record so the base
         scanners capture it (Lwt sequence-associated storage); wrap the result
         to count rows actually delivered once the caller drains it. *)
      Lwt.with_value query_stats_key (Some s)
      @@ fun () ->
      let* stream = to_stream clock params store ~mode ~cat:(Some cat) op in
      Lwt.return
        (Lwt_stream.map
           (fun row ->
              s.rows_returned <- s.rows_returned + 1;
              row)
           stream)
  in
  (* #262: publish the txn mode to subquery evaluation only when inside an
     explicit transaction.  In [Auto] mode [current_txn_mode] already defaults to
     [Auto], so the common read path pays no [with_value] — preserving the
     zero-overhead scan path (#259). *)
  match mode with
  | Auto -> body ()
  | In_txn _ | In_ro_txn _ -> Lwt.with_value txn_mode_key (Some mode) body
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
