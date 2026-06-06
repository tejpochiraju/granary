module S = Sqlocaml_store.Store
module Row = Sqlocaml_encoding.Row
module Varint = Sqlocaml_encoding.Varint
module Schema_fingerprint = Sqlocaml_encoding.Schema_fingerprint
module Rowid = Sqlocaml_encoding.Rowid

type fk_action =
  | FA_no_action
  | FA_restrict
  | FA_cascade
  | FA_set_null
  | FA_set_default

(* System tree IDs *)
let sys_tables_tid : S.tree_id = 0
let sys_columns_tid : S.tree_id = 1
let sys_indexes_tid : S.tree_id = 2
let sys_meta_tid : S.tree_id = 3
let sys_fts_tid : S.tree_id = 4
let sys_views_tid : S.tree_id = 5
let sys_triggers_tid : S.tree_id = 6

(* #174: redundant catalog mirror.  A second, self-describing copy of every
   table's schema keyed by tree_id, so a single damaged primary-catalog page
   does not lose the schema for every table.  Also the canonical
   reference-fingerprint store used for drift detection on open. *)
let sys_mirror_tid : S.tree_id = 7

(* Rowid counter key suffix for FTS tables: name ++ "\x00rowid" *)
let sys_fts_rowid_suffix = Bytes.of_string "\x00rowid"
let next_user_tid_key = Bytes.of_string "next_user_tid"
let next_user_tid_init = 16

(* Counter for monotonically-increasing index IDs, stored in sys_meta. *)
let next_index_id_key = Bytes.of_string "next_index_id"
let user_version_key = Bytes.of_string "\x00user_version"

(** Read user_version from an already-open RW or RO transaction.
    Returns 0 if never set. *)
let read_user_version_tx tx : int64 Lwt.t =
  let%lwt v = S.get tx sys_meta_tid user_version_key in
  Lwt.return
    (match v with
     | None -> 0L
     | Some b -> Bytes.get_int64_be b 0)
;;

(** Write user_version inside an already-open RW transaction.
    Caller is responsible for commit. *)
let write_user_version_tx tx (v : int64) : unit Lwt.t =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 v;
  S.put tx sys_meta_tid user_version_key b
;;

type fk_constraint =
  { fk_local_cols : string list
  ; fk_parent_table : string
  ; fk_parent_cols : string list
  ; fk_on_delete : fk_action
  ; fk_on_update : fk_action
  ; fk_deferrable : bool (** false = IMMEDIATE (default), true = INITIALLY DEFERRED *)
  }

type pending_fk_kind =
  [ `Insert
  | `Update
  | `Delete
  ]

type pending_fk_recheck = { recheck : 'm. 'm S.txn -> bool Lwt.t }

type pending_fk_check =
  { pfk_kind : pending_fk_kind
  ; pfk_table : string
  ; pfk_rowid : int64
  ; pfk_message : string
  ; pfk_recheck : pending_fk_recheck
  }

type table_meta =
  { name : string
  ; tree_id : S.tree_id
  ; columns : Row.column list
  ; next_rowid : int64
    (** Next rowid to auto-allocate, maintained as [max existing rowid + 1].
        The sentinel [empty_next_rowid] marks a table that has never had a
        rowid seeded (conceptually [max = -inf]) so the FIRST row seeds the
        counter from its actual value — see #250. *)
  ; fk_constraints : fk_constraint list
  ; without_rowid : bool (** WITHOUT ROWID — phase 37 #122. *)
  ; autoincrement : bool
    (** #299: [INTEGER PRIMARY KEY AUTOINCREMENT].  Sticky rowid high-water —
        ROLLBACK reverts to the committed counter rather than recomputing
        [max(rowid)+1] from data, so committed DELETEs never get reused. *)
  }

(** #250: sentinel [next_rowid] for an alias table with no rowid seeded yet.  A
    fresh/empty INTEGER PRIMARY KEY table starts here so the first row — an
    explicit id (even <= 0) OR an auto NULL — seeds the counter from the real
    value (explicit: id+1; NULL: 1), matching SQLite ([max(existing)+1], empty
    -> 1) and [recover_next_rowid].  The live counter is always
    [max(rowid)+1 >= Int64.min_int + 1], so it can never collide with this
    sentinel (allocation guards the [max_int] overflow that would wrap to it). *)
let empty_next_rowid = Int64.min_int

type idx_origin =
  [ `Implicit_pk
  | `Implicit_unique
  | `User
  ]

type index_info =
  { idx_name : string
  ; idx_table : string
  ; idx_columns : string list (* col names for plain; expr SQL for expression indexes *)
  ; idx_unique : bool
  ; idx_tree_id : S.tree_id
  ; idx_expr_flags : bool list (* true = expression index column, false = plain column *)
  ; idx_where_sql : string option
  ; idx_origin : idx_origin
  }

type fts_table_meta =
  { fts_name : string
  ; fts_content_tree : S.tree_id
  ; fts_index_tree : S.tree_id
  ; fts_columns : string list
  }

(* #283: the in-memory schema cache and its rollback ledger, sealed behind a
   signature so the ONLY way to mutate the three catalog hashtables is through a
   mutator that registers its own reversal.  "Mutate the cache without recording
   how to undo it" is therefore unrepresentable outside this module.

   Two reversal strategies, both explicit (no raw, unprotected write exists):
   - undo-tracked mutators ([put_*]/[remove_*]) capture the prior binding and push
     the synthesized inverse onto [undo]; replayed by [rollback]/[savepoint_rollback],
     discarded by [commit].  Used for DDL run THROUGH [Exec.with_ddl_txn] (the undo is
     discarded on COMMIT in either Auto or explicit mode, replayed on ROLLBACK / a
     mid-statement failure — see exec.ml).
   - durable mutators ([*_durable]) apply with NO undo, for a catalog function's own
     autocommit path that self-commits its own writer txn (so the write is already
     durable and a [?txn=None] branch can never be inside an ambient writer txn — a
     nested rw_begin would deadlock).  Also used for ephemeral CTE sentinels.

   The rowid counter keeps the #293 recompute-on-rollback strategy: [bump_rowid]
   records the table in the dirty set instead of pushing a closure, and the db layer
   re-derives max(rowid)+1 from the rolled-back tree for exactly those tables. *)
module Schema_cache : sig
  type t

  (** [stamp] re-stamps the #174 tree-tag for a [table_meta]; wired to
      [register_tag store].  Every [table_meta] entering the cache is stamped so the
      page-stamp stays consistent automatically, and an undo re-stamps the prior. *)
  val create : stamp:(table_meta -> unit) -> t

  (* reads — never touch the undo log *)
  val find_table : t -> string -> table_meta option
  val mem_table : t -> string -> bool
  val find_index : t -> string -> index_info option
  val mem_index : t -> string -> bool
  val find_fts : t -> string -> fts_table_meta option
  val fold_tables : (string -> table_meta -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_indexes : (string -> index_info -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_fts : (string -> fts_table_meta -> 'a -> 'a) -> t -> 'a -> 'a
  val count_tables : t -> int
  val count_indexes : t -> int
  val count_fts : t -> int

  (* undo-tracked mutators (DDL under with_ddl_txn) *)
  val put_table : t -> name:string -> table_meta -> unit
  val remove_table : t -> name:string -> unit
  val put_index : t -> name:string -> index_info -> unit
  val remove_index : t -> name:string -> unit
  val put_fts : t -> name:string -> fts_table_meta -> unit

  (* durable mutators (catalog-internal autocommit / ephemeral — no undo) *)
  val put_table_durable : t -> name:string -> table_meta -> unit
  val remove_table_durable : t -> name:string -> unit
  val put_index_durable : t -> name:string -> index_info -> unit
  val put_fts_durable : t -> name:string -> fts_table_meta -> unit

  (* rowid counter: in-txn bump (dirty-set tracked) and post-rollback/autocommit
     durable set; [take_rowid_bumped] returns the dirty names and clears the set. *)
  val bump_rowid : t -> name:string -> table_meta -> unit
  val set_rowid_durable : t -> name:string -> table_meta -> unit
  val take_rowid_bumped : t -> string list

  (** Append an arbitrary reversal to the undo log.  The ONLY way an external
      owner (the db layer's view/trigger caches, #269) can enroll a rollback in
      this ledger so it replays in order with the catalog's own DDL undos at
      ROLLBACK / ROLLBACK TO SAVEPOINT.  Note this appends to the undo LOG only —
      it cannot reach the sealed cache hashtables, so the structural guarantee is
      preserved.  The closure MUST be idempotent (re-run as a no-op): #280's
      [savepoint_rollback] can leave an already-run closure queued for the outer
      ROLLBACK. *)
  val register_undo : t -> (unit -> unit) -> unit

  (* lifecycle — drive by the db layer at txn / savepoint boundaries *)
  val commit : t -> unit
  val rollback : t -> unit
  val savepoint_begin : t -> string -> unit
  val savepoint_rollback : t -> string -> unit
  val savepoint_release : t -> string -> unit
  val mark_poisoned : t -> unit
  val is_poisoned : t -> bool
end = struct
  (* #280/#293/#303: one frame per open SAVEPOINT.  [sp_undo] is the [undo] list
     as it stood when the savepoint opened (a physical suffix — see [push_undo]),
     so ROLLBACK TO can run+drop exactly the DDL undos registered since.
     [sp_poison] restores the #295 poison flag.  [sp_rowids] snapshots every
     table's cached [next_rowid] at SAVEPOINT so ROLLBACK TO can restore the
     in-memory counter (#303): the full-ROLLBACK recompute-from-tree path
     ([recompute_rowid_counters_after_rollback]) is unusable mid-transaction —
     the RW txn is still open, so a fresh RO snapshot reads the last-committed
     tree, not the savepoint state — so we snapshot/restore in memory instead. *)
  type savepoint =
    { sp_name : string
    ; sp_undo : (unit -> unit) list
    ; sp_poison : bool
    ; sp_rowids : (string * int64) list
    }

  type t =
    { tables : (string, table_meta) Hashtbl.t
    ; indexes : (string, index_info) Hashtbl.t
    ; fts : (string, fts_table_meta) Hashtbl.t
    ; stamp : table_meta -> unit
    ; mutable undo : (unit -> unit) list
    ; mutable savepoints : savepoint list
    ; mutable poisoned : bool
    ; rowid_bumped : (string, unit) Hashtbl.t
    }

  let create ~stamp =
    { tables = Hashtbl.create 16
    ; indexes = Hashtbl.create 16
    ; fts = Hashtbl.create 8
    ; stamp
    ; undo = []
    ; savepoints = []
    ; poisoned = false
    ; rowid_bumped = Hashtbl.create 8
    }
  ;;

  (* The undo log only ever grows by prepending, so a saved suffix stays
     physically identical (==) — the invariant [savepoint_rollback] relies on. *)
  let push_undo t f = t.undo <- f :: t.undo
  let register_undo = push_undo
  let find_table t name = Hashtbl.find_opt t.tables name
  let mem_table t name = Hashtbl.mem t.tables name
  let find_index t name = Hashtbl.find_opt t.indexes name
  let mem_index t name = Hashtbl.mem t.indexes name
  let find_fts t name = Hashtbl.find_opt t.fts name
  let fold_tables f t acc = Hashtbl.fold f t.tables acc
  let fold_indexes f t acc = Hashtbl.fold f t.indexes acc
  let fold_fts f t acc = Hashtbl.fold f t.fts acc
  let count_tables t = Hashtbl.length t.tables
  let count_indexes t = Hashtbl.length t.indexes
  let count_fts t = Hashtbl.length t.fts

  let put_table t ~name meta =
    let prior = Hashtbl.find_opt t.tables name in
    Hashtbl.replace t.tables name meta;
    t.stamp meta;
    push_undo t (fun () ->
      match prior with
      | Some m ->
        Hashtbl.replace t.tables name m;
        t.stamp m
      | None -> Hashtbl.remove t.tables name)
  ;;

  let remove_table t ~name =
    let prior = Hashtbl.find_opt t.tables name in
    Hashtbl.remove t.tables name;
    push_undo t (fun () ->
      match prior with
      | Some m ->
        Hashtbl.replace t.tables name m;
        t.stamp m
      | None -> ())
  ;;

  let put_index t ~name info =
    let prior = Hashtbl.find_opt t.indexes name in
    Hashtbl.replace t.indexes name info;
    push_undo t (fun () ->
      match prior with
      | Some i -> Hashtbl.replace t.indexes name i
      | None -> Hashtbl.remove t.indexes name)
  ;;

  let remove_index t ~name =
    let prior = Hashtbl.find_opt t.indexes name in
    Hashtbl.remove t.indexes name;
    push_undo t (fun () ->
      match prior with
      | Some i -> Hashtbl.replace t.indexes name i
      | None -> ())
  ;;

  let put_fts t ~name meta =
    let prior = Hashtbl.find_opt t.fts name in
    Hashtbl.replace t.fts name meta;
    push_undo t (fun () ->
      match prior with
      | Some m -> Hashtbl.replace t.fts name m
      | None -> Hashtbl.remove t.fts name)
  ;;

  let put_table_durable t ~name meta =
    Hashtbl.replace t.tables name meta;
    t.stamp meta
  ;;

  let remove_table_durable t ~name = Hashtbl.remove t.tables name
  let put_index_durable t ~name info = Hashtbl.replace t.indexes name info
  let put_fts_durable t ~name meta = Hashtbl.replace t.fts name meta

  let bump_rowid t ~name meta =
    Hashtbl.replace t.tables name meta;
    Hashtbl.replace t.rowid_bumped name ()
  ;;

  let set_rowid_durable t ~name meta = Hashtbl.replace t.tables name meta

  let take_rowid_bumped t =
    let names = Hashtbl.fold (fun k _ acc -> k :: acc) t.rowid_bumped [] in
    Hashtbl.reset t.rowid_bumped;
    names
  ;;

  let commit t =
    t.undo <- [];
    t.savepoints <- [];
    t.poisoned <- false;
    (* #293: COMMIT keeps the bumped next_rowid counter but clears the dirty set so
       a later unrelated ROLLBACK won't recompute a table not bumped in that txn. *)
    Hashtbl.reset t.rowid_bumped
  ;;

  let rollback t =
    List.iter (fun f -> f ()) t.undo;
    t.undo <- [];
    t.savepoints <- [];
    t.poisoned <- false
  ;;

  (* #293: [rowid_bumped] is intentionally NOT cleared here — the db layer calls
       the recompute step right after, which reads it via [take_rowid_bumped]. *)

  (* #303: snapshot every table's cached [next_rowid] so ROLLBACK TO can restore
     the in-memory counter to its value at SAVEPOINT.  Tables created after the
     savepoint are absent here and their CREATE is undone by [sp_undo]; tables
     dropped/altered after it are restored by [sp_undo] first, then their counter
     is corrected to this snapshot value. *)
  let snapshot_rowids t =
    Hashtbl.fold
      (fun name (m : table_meta) acc -> (name, m.next_rowid) :: acc)
      t.tables
      []
  ;;

  let savepoint_begin t name =
    t.savepoints
    <- { sp_name = name
       ; sp_undo = t.undo
       ; sp_poison = t.poisoned
       ; sp_rowids = snapshot_rowids t
       }
       :: t.savepoints
  ;;

  (* #303: restore each snapshotted counter onto the (already DDL-undone) cached
     meta.  Skip names no longer cached (their CREATE was rolled back). *)
  let restore_rowids t rowids =
    List.iter
      (fun (name, next_rowid) ->
         match Hashtbl.find_opt t.tables name with
         | Some m -> Hashtbl.replace t.tables name { m with next_rowid }
         | None -> ())
      rowids
  ;;

  let savepoint_rollback t name =
    let rec find = function
      | [] -> None
      | ({ sp_name; _ } as sp) :: older when String.equal sp_name name -> Some (sp, older)
      | _ :: rest -> find rest
    in
    match find t.savepoints with
    | None -> ()
    | Some (sp, older) ->
      let rec run lst =
        if lst == sp.sp_undo
        then ()
        else (
          match lst with
          | [] -> ()
          | f :: tl ->
            f ();
            run tl)
      in
      run t.undo;
      t.undo <- sp.sp_undo;
      t.poisoned <- sp.sp_poison;
      (* After the DDL undos above, correct the cached rowid counters to their
         savepoint values (#303). *)
      restore_rowids t sp.sp_rowids;
      t.savepoints <- sp :: older
  ;;

  let savepoint_release t name =
    let rec drop = function
      | [] -> []
      | { sp_name; _ } :: older when String.equal sp_name name -> older
      | _ :: rest -> drop rest
    in
    t.savepoints <- drop t.savepoints
  ;;

  let mark_poisoned t = t.poisoned <- true
  let is_poisoned t = t.poisoned
end

type t =
  { store : S.t
  ; sc : Schema_cache.t
    (** #283: the sealed in-memory schema cache (tables/indexes/fts) and its
        rollback ledger.  The only path to a cache mutation, so a write that does
        not record its reversal is unrepresentable. *)
  ; mutable fk_enforcement : bool
  ; mutable recursive_triggers : bool
  ; mutable defer_fks_pragma : bool
    (** PRAGMA defer_foreign_keys — when ON, every FK enforcement site treats
        the violation as deferred regardless of constraint definition.
        Reset to false at every txn boundary by the db layer. *)
  ; mutable pending_fk_checks : pending_fk_check list
    (** Queued deferred FK violations; drained at commit. The list is in
        reverse insertion order; drain reverses again before returning. *)
  ; mutable last_inserted_rowid : int64
    (** #243 (T1): rowid of the most recently INSERTed row, set by the executor.
        Read by the db layer for [last_insert_rowid()].  Required because with
        INTEGER PRIMARY KEY rowid aliases an explicit id need not equal
        [next_rowid - 1] (e.g. inserting id=5 after id=100). *)
  }

let pp fmt t =
  Format.fprintf
    fmt
    "@[<hv>Catalog.t { tables = %d;@ indexes = %d;@ fts = %d;@ fk_enforcement = %b }@]"
    (Schema_cache.count_tables t.sc)
    (Schema_cache.count_indexes t.sc)
    (Schema_cache.count_fts t.sc)
    t.fk_enforcement
;;

(* #243 (T1): SQLite's "INTEGER PRIMARY KEY is an alias for the rowid".  When a
   rowid table has exactly one PRIMARY KEY column whose type is INTEGER, that
   column IS the rowid: the table tree is keyed by its value, no separate __pk
   index exists, and uniqueness is enforced by the table tree itself.  Returns
   the column index of that alias column, or None for every other shape
   (WITHOUT ROWID, composite PK, non-INTEGER PK, no PK).

   Note: this port's AST collapses every integer type spelling (INT, INTEGER,
   BIGINT, ...) to a single [Row.Integer], so unlike SQLite — which aliases only
   the exact spelling "INTEGER" — any integer-typed single-column PK qualifies.
   The distinction is not representable here and was already absent. *)
let compute_rowid_alias_col (columns : Row.column list) ~without_rowid : int option =
  if without_rowid
  then None
  else (
    let indexed = List.mapi (fun i (c : Row.column) -> i, c) columns in
    let pks = List.filter (fun (_, (c : Row.column)) -> c.primary_key) indexed in
    match pks with
    (* #312: an INTEGER PRIMARY KEY DESC is NOT a rowid alias in SQLite — the
       column gets a hidden auto rowid plus a real unique index, exactly like a
       non-INTEGER PK.  So [pk_desc] disqualifies the alias. *)
    | [ (i, (c : Row.column)) ] when c.ty = Row.Integer && not c.pk_desc -> Some i
    | _ -> None)
;;

(* Convenience: the rowid-alias column of a loaded table, if any. *)
let rowid_alias_col (m : table_meta) : int option =
  compute_rowid_alias_col m.columns ~without_rowid:m.without_rowid
;;

(* ------------------------------------------------------------------ *)
(* Encoding helpers                                                     *)
(* ------------------------------------------------------------------ *)

let encode_table_value m =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf (Int64.of_int m.tree_id);
  Varint.encode_int64 buf m.next_rowid;
  (* Trailing without_rowid flag (phase 37).  Old encodings have no trailing
     bytes; the decoder treats their absence as [false]. *)
  Varint.encode_uint64 buf (if m.without_rowid then 1L else 0L);
  (* #299: trailing autoincrement flag.  Older encodings lack it; the decoder
     treats its absence as [false]. *)
  Varint.encode_uint64 buf (if m.autoincrement then 1L else 0L);
  Buffer.to_bytes buf
;;

let decode_table_value bytes =
  let tid, off = Varint.decode_uint64 bytes 0 in
  let next, off' = Varint.decode_int64 bytes off in
  let without_rowid, off'' =
    if off' >= Bytes.length bytes
    then false, off'
    else (
      let v, o = Varint.decode_uint64 bytes off' in
      Int64.to_int v <> 0, o)
  in
  let autoincrement =
    if off'' >= Bytes.length bytes
    then false
    else (
      let v, _ = Varint.decode_uint64 bytes off'' in
      Int64.to_int v <> 0)
  in
  Int64.to_int tid, next, without_rowid, autoincrement
;;

(* Column key: table_name ++ NUL ++ ordinal_be8 *)
let column_key table_name ordinal =
  let tn = Bytes.of_string table_name in
  let ord = Bytes.create 8 in
  for i = 0 to 7 do
    Bytes.set_uint8 ord i ((ordinal lsr ((7 - i) * 8)) land 0xFF)
  done;
  Bytes.cat (Bytes.cat tn (Bytes.of_string "\x00")) ord
;;

let column_prefix table_name =
  Bytes.cat (Bytes.of_string table_name) (Bytes.of_string "\x00")
;;

let type_tag = function
  | Row.Integer -> 1
  | Row.Text -> 2
  | Row.Real -> 3
  | Row.Blob -> 4
;;

let type_of_tag = function
  | 1 -> Row.Integer
  | 2 -> Row.Text
  | 3 -> Row.Real
  | 4 -> Row.Blob
  | n -> failwith (Printf.sprintf "unknown column type tag %d" n)
;;

let default_value_tag : Row.default_value -> int = function
  | Row.DV_null -> 0
  | Row.DV_int _ -> 1
  | Row.DV_real _ -> 2
  | Row.DV_text _ -> 3
  | Row.DV_blob _ -> 4
  | Row.DV_current_timestamp -> 5
  | Row.DV_current_date -> 6
  | Row.DV_current_time -> 7
;;

let encode_default_value buf (dv : Row.default_value) =
  Varint.encode_uint64 buf (Int64.of_int (default_value_tag dv));
  match dv with
  | Row.DV_null -> ()
  | Row.DV_int n ->
    (* 8-byte LE int64 *)
    let tmp = Bytes.create 8 in
    for k = 0 to 7 do
      Bytes.set_uint8
        tmp
        k
        (Int64.to_int (Int64.logand (Int64.shift_right_logical n (k * 8)) 0xFFL))
    done;
    Buffer.add_bytes buf tmp
  | Row.DV_real f ->
    let bits = Int64.bits_of_float f in
    let tmp = Bytes.create 8 in
    for k = 0 to 7 do
      Bytes.set_uint8
        tmp
        k
        (Int64.to_int (Int64.logand (Int64.shift_right_logical bits (k * 8)) 0xFFL))
    done;
    Buffer.add_bytes buf tmp
  | Row.DV_text s ->
    Varint.encode_uint64 buf (Int64.of_int (String.length s));
    Buffer.add_string buf s
  | Row.DV_blob b ->
    Varint.encode_uint64 buf (Int64.of_int (Bytes.length b));
    Buffer.add_bytes buf b
  | Row.DV_current_timestamp | Row.DV_current_date | Row.DV_current_time ->
    () (* tag alone is sufficient — no payload *)
;;

let decode_default_value bytes off =
  let tag, off = Varint.decode_uint64 bytes off in
  match Int64.to_int tag with
  | 0 -> Row.DV_null, off
  | 1 ->
    let n = ref Int64.zero in
    for k = 0 to 7 do
      let byte = Int64.of_int (Bytes.get_uint8 bytes (off + k)) in
      n := Int64.logor !n (Int64.shift_left byte (k * 8))
    done;
    Row.DV_int !n, off + 8
  | 2 ->
    let bits = ref Int64.zero in
    for k = 0 to 7 do
      let byte = Int64.of_int (Bytes.get_uint8 bytes (off + k)) in
      bits := Int64.logor !bits (Int64.shift_left byte (k * 8))
    done;
    Row.DV_real (Int64.float_of_bits !bits), off + 8
  | 3 ->
    let len, off = Varint.decode_uint64 bytes off in
    let len = Int64.to_int len in
    let s = Bytes.sub_string bytes off len in
    Row.DV_text s, off + len
  | 4 ->
    let len, off = Varint.decode_uint64 bytes off in
    let len = Int64.to_int len in
    let b = Bytes.sub bytes off len in
    Row.DV_blob b, off + len
  | 5 -> Row.DV_current_timestamp, off
  | 6 -> Row.DV_current_date, off
  | 7 -> Row.DV_current_time, off
  | n -> failwith (Printf.sprintf "unknown default value tag %d" n)
;;

let encode_column (col : Row.column) =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf (Int64.of_int (type_tag col.ty));
  Varint.encode_uint64 buf (Int64.of_int (String.length col.name));
  Buffer.add_string buf col.name;
  Varint.encode_uint64 buf (if col.not_null then 1L else 0L);
  Varint.encode_uint64 buf (if col.primary_key then 1L else 0L);
  (match col.default with
   | None -> Varint.encode_uint64 buf 0L
   | Some dv ->
     Varint.encode_uint64 buf 1L;
     encode_default_value buf dv);
  (* Phase 9: check_sql field — appended at end for backward compat *)
  (match col.check_sql with
   | None -> Varint.encode_uint64 buf 0L
   | Some sql ->
     Varint.encode_uint64 buf 1L;
     Varint.encode_uint64 buf (Int64.of_int (String.length sql));
     Buffer.add_string buf sql);
  (* Phase 25: generated_as field — appended for backward compat *)
  (match col.generated_as with
   | None -> Varint.encode_uint64 buf 0L
   | Some (sql, is_stored) ->
     Varint.encode_uint64 buf 1L;
     Varint.encode_uint64 buf (if is_stored then 1L else 0L);
     Varint.encode_uint64 buf (Int64.of_int (String.length sql));
     Buffer.add_string buf sql);
  (* #312: trailing pk_desc flag — appended for backward compat (absent ⇒ false). *)
  Varint.encode_uint64 buf (if col.pk_desc then 1L else 0L);
  Buffer.to_bytes buf
;;

let decode_check_sql bytes off =
  if Bytes.length bytes - off <= 0
  then None, off
  else (
    let has_check, off2 = Varint.decode_uint64 bytes off in
    if Int64.to_int has_check = 0
    then None, off2
    else (
      let sql_len, off3 = Varint.decode_uint64 bytes off2 in
      let sql = Bytes.sub_string bytes off3 (Int64.to_int sql_len) in
      Some sql, off3 + Int64.to_int sql_len))
;;

(* Returns the decoded [generated_as] AND the offset just past it, so the
   caller can continue decoding trailing fields (#312 pk_desc). *)
let decode_generated_as bytes off =
  if Bytes.length bytes - off <= 0
  then None, off
  else (
    let has_gen, off2 = Varint.decode_uint64 bytes off in
    if Int64.to_int has_gen = 0
    then None, off2
    else (
      let is_stored, off3 = Varint.decode_uint64 bytes off2 in
      let sql_len, off4 = Varint.decode_uint64 bytes off3 in
      let sql = Bytes.sub_string bytes off4 (Int64.to_int sql_len) in
      Some (sql, Int64.to_int is_stored = 1), off4 + Int64.to_int sql_len))
;;

(* #312: optional trailing pk_desc flag.  Absent (old encodings) ⇒ false. *)
let decode_pk_desc bytes off =
  if off >= Bytes.length bytes
  then false
  else (
    let flag, _ = Varint.decode_uint64 bytes off in
    Int64.to_int flag <> 0)
;;

let decode_column bytes =
  let tag, off = Varint.decode_uint64 bytes 0 in
  let len, off = Varint.decode_uint64 bytes off in
  let name = Bytes.sub_string bytes off (Int64.to_int len) in
  let off = off + Int64.to_int len in
  (* not_null and primary_key — present only in the new format.
     If there are no more bytes, default to false (backward compat). *)
  let bytes_left = Bytes.length bytes - off in
  if bytes_left = 0
  then
    Row.
      { name
      ; ty = type_of_tag (Int64.to_int tag)
      ; not_null = false
      ; primary_key = false
      ; pk_desc = false
      ; default = None
      ; check_sql = None
      ; generated_as = None
      }
  else (
    let nn, off = Varint.decode_uint64 bytes off in
    let pk, off = Varint.decode_uint64 bytes off in
    let has_def, off = Varint.decode_uint64 bytes off in
    let default, off =
      if Int64.to_int has_def = 0
      then None, off
      else (
        let dv, off' = decode_default_value bytes off in
        Some dv, off')
    in
    let check_sql, final_off = decode_check_sql bytes off in
    let generated_as, off = decode_generated_as bytes final_off in
    let pk_desc = decode_pk_desc bytes off in
    Row.
      { name
      ; ty = type_of_tag (Int64.to_int tag)
      ; not_null = Int64.to_int nn <> 0
      ; primary_key = Int64.to_int pk <> 0
      ; pk_desc
      ; default
      ; check_sql
      ; generated_as
      })
;;

let byte_of_idx_origin : idx_origin -> char = function
  | `Implicit_pk -> '\x00'
  | `Implicit_unique -> '\x01'
  | `User -> '\x02'
;;

let idx_origin_of_byte = function
  | 0 -> `Implicit_pk
  | 1 -> `Implicit_unique
  | _ -> `User
;;

(* Index value encoding:
   varint(name_len) ++ name ++ varint(table_len) ++ table
   ++ varint(n_cols) ++ (varint(col_len) ++ col)*n_cols
   ++ [unique: 1 byte] ++ varint(tree_id) *)
let encode_index_value (idx : index_info) =
  let buf = Buffer.create 32 in
  Varint.encode_uint64 buf (Int64.of_int (String.length idx.idx_name));
  Buffer.add_string buf idx.idx_name;
  Varint.encode_uint64 buf (Int64.of_int (String.length idx.idx_table));
  Buffer.add_string buf idx.idx_table;
  Varint.encode_uint64 buf (Int64.of_int (List.length idx.idx_columns));
  List.iter
    (fun col ->
       Varint.encode_uint64 buf (Int64.of_int (String.length col));
       Buffer.add_string buf col)
    idx.idx_columns;
  Buffer.add_char buf (if idx.idx_unique then '\x01' else '\x00');
  Varint.encode_uint64 buf (Int64.of_int idx.idx_tree_id);
  (* Extended fields version 3: origin byte + expr flags + optional WHERE *)
  Varint.encode_uint64 buf 3L;
  Buffer.add_char buf (byte_of_idx_origin idx.idx_origin);
  (* One varint per column: 0 = plain column, 1 = expression column *)
  List.iter
    (fun is_expr -> Varint.encode_uint64 buf (if is_expr then 1L else 0L))
    idx.idx_expr_flags;
  (* WHERE clause SQL *)
  (match idx.idx_where_sql with
   | None -> Varint.encode_uint64 buf 0L
   | Some sql ->
     Varint.encode_uint64 buf 1L;
     Varint.encode_uint64 buf (Int64.of_int (String.length sql));
     Buffer.add_string buf sql);
  Buffer.to_bytes buf
;;

(* Returns [(expr_flags, where_sql, origin)].  The current encoder always writes
   version 3 (with an explicit origin); the pre-v3 branches default [origin] to
   [`User] — the dump-safe "emit it" choice — since the format is pre-release and
   no v<3 data exists.  Decode of expr flags + WHERE is shared by versions 2/3. *)
let decode_index_ext_fields bytes off2 cols =
  let decode_flags_and_where off_start =
    let off_ref = ref off_start in
    let expr_flags =
      List.map
        (fun _ ->
           let flag, next = Varint.decode_uint64 bytes !off_ref in
           off_ref := next;
           Int64.to_int flag = 1)
        cols
    in
    let has_where, off4 = Varint.decode_uint64 bytes !off_ref in
    let where_sql =
      if Int64.to_int has_where = 0
      then None
      else (
        let sql_len, off5 = Varint.decode_uint64 bytes off4 in
        Some (Bytes.sub_string bytes off5 (Int64.to_int sql_len)))
    in
    expr_flags, where_sql
  in
  if off2 >= Bytes.length bytes
  then List.map (fun _ -> false) cols, None, `User (* old format: no extended fields *)
  else (
    let version, off3 = Varint.decode_uint64 bytes off2 in
    match Int64.to_int version with
    | 1 ->
      (* Version 1 (Task 1): only WHERE clause, no expr flags *)
      let has_where, off4 = Varint.decode_uint64 bytes off3 in
      let where_sql =
        if Int64.to_int has_where = 0
        then None
        else (
          let sql_len, off5 = Varint.decode_uint64 bytes off4 in
          Some (Bytes.sub_string bytes off5 (Int64.to_int sql_len)))
      in
      List.map (fun _ -> false) cols, where_sql, `User
    | 2 ->
      (* Version 2 (Task 2): n_cols expr flags, then WHERE clause *)
      let expr_flags, where_sql = decode_flags_and_where off3 in
      expr_flags, where_sql, `User
    | 3 ->
      (* Version 3 (#273): origin byte, then expr flags, then WHERE clause *)
      let origin = idx_origin_of_byte (Bytes.get_uint8 bytes off3) in
      let expr_flags, where_sql = decode_flags_and_where (off3 + 1) in
      expr_flags, where_sql, origin
    | _ -> List.map (fun _ -> false) cols, None, `User)
;;

let decode_index_value bytes =
  let name_len, off = Varint.decode_uint64 bytes 0 in
  let name_len = Int64.to_int name_len in
  let name = Bytes.sub_string bytes off name_len in
  let off = off + name_len in
  let tbl_len, off = Varint.decode_uint64 bytes off in
  let tbl_len = Int64.to_int tbl_len in
  let tbl = Bytes.sub_string bytes off tbl_len in
  let off = off + tbl_len in
  let n_cols, off = Varint.decode_uint64 bytes off in
  let n_cols = Int64.to_int n_cols in
  let off = ref off in
  let cols =
    List.init n_cols (fun _ ->
      let col_len, next_off = Varint.decode_uint64 bytes !off in
      let col = Bytes.sub_string bytes next_off (Int64.to_int col_len) in
      off := next_off + Int64.to_int col_len;
      col)
  in
  let unique_byte = Bytes.get_uint8 bytes !off in
  let tree_id, off2 = Varint.decode_uint64 bytes (!off + 1) in
  let idx_expr_flags, idx_where_sql, idx_origin =
    decode_index_ext_fields bytes off2 cols
  in
  { idx_name = name
  ; idx_table = tbl
  ; idx_columns = cols
  ; idx_unique = unique_byte <> 0
  ; idx_tree_id = Int64.to_int tree_id
  ; idx_expr_flags
  ; idx_where_sql
  ; idx_origin
  }
;;

(* FTS value encoding:
   varint(content_tree) ++ varint(index_tree) ++ varint(n_cols)
   ++ (varint(col_len) ++ col_bytes)* *)
let encode_fts_value (m : fts_table_meta) =
  let buf = Buffer.create 32 in
  Varint.encode_uint64 buf (Int64.of_int m.fts_content_tree);
  Varint.encode_uint64 buf (Int64.of_int m.fts_index_tree);
  Varint.encode_uint64 buf (Int64.of_int (List.length m.fts_columns));
  List.iter
    (fun col ->
       let b = Bytes.of_string col in
       Varint.encode_uint64 buf (Int64.of_int (Bytes.length b));
       Buffer.add_bytes buf b)
    m.fts_columns;
  Buffer.to_bytes buf
;;

let decode_fts_value fts_name bytes =
  let ct, off0 = Varint.decode_uint64 bytes 0 in
  let it, off1 = Varint.decode_uint64 bytes off0 in
  let nc, off2 = Varint.decode_uint64 bytes off1 in
  let n = Int64.to_int nc in
  let cols = ref [] in
  let pos = ref off2 in
  for _ = 1 to n do
    let len, off = Varint.decode_uint64 bytes !pos in
    let col = Bytes.sub_string bytes off (Int64.to_int len) in
    cols := col :: !cols;
    pos := off + Int64.to_int len
  done;
  { fts_name
  ; fts_content_tree = Int64.to_int ct
  ; fts_index_tree = Int64.to_int it
  ; fts_columns = List.rev !cols
  }
;;

(* ------------------------------------------------------------------ *)
(* next_user_tid / next_index_id management                              *)
(* ------------------------------------------------------------------ *)

(* #269: the tx-threaded forms are the primitives — they read/write the counter
   through a supplied [tx], so allocation is read-your-own-writes (two CREATEs in
   one transaction never alloc the SAME tree-ID) and rolls back with the txn.
   The store-level forms below wrap them in their own RO snapshot / RW txn for
   the autocommit path; those must NOT be used while an explicit writer txn is
   held (the nested [rw_begin] would self-deadlock — that was the #269 bug). *)
let read_uint64_key_tx tx key default =
  let%lwt v = S.get tx sys_meta_tid key in
  match v with
  | Some b ->
    let n, _ = Varint.decode_uint64 b 0 in
    Lwt.return (Int64.to_int n)
  | None -> Lwt.return default
;;

let write_uint64_key_tx tx key n =
  let buf = Buffer.create 8 in
  Varint.encode_uint64 buf (Int64.of_int n);
  S.put tx sys_meta_tid key (Buffer.to_bytes buf)
;;

let read_uint64_key store key default =
  S.with_ro store @@ fun tx -> read_uint64_key_tx tx key default
;;

let write_uint64_key store key n =
  let%lwt tx = S.rw_begin store in
  let%lwt () = write_uint64_key_tx tx key n in
  S.commit tx
;;

let read_next_user_tid store = read_uint64_key store next_user_tid_key next_user_tid_init
let write_next_user_tid store tid = write_uint64_key store next_user_tid_key tid
let read_next_index_id store = read_uint64_key store next_index_id_key 0
let write_next_index_id store id = write_uint64_key store next_index_id_key id
let read_next_user_tid_tx tx = read_uint64_key_tx tx next_user_tid_key next_user_tid_init
let write_next_user_tid_tx tx tid = write_uint64_key_tx tx next_user_tid_key tid
let read_next_index_id_tx tx = read_uint64_key_tx tx next_index_id_key 0
let write_next_index_id_tx tx id = write_uint64_key_tx tx next_index_id_key id

(* Allocate the next user tree-ID through [tx] (atomic with the caller's txn). *)
let next_user_tid_tx tx =
  let%lwt tid = read_next_user_tid_tx tx in
  let%lwt () = write_next_user_tid_tx tx (tid + 1) in
  Lwt.return tid
;;

(* Encode an int as a varint key for the _sys_indexes tree. *)
let index_key id =
  let buf = Buffer.create 8 in
  Varint.encode_uint64 buf (Int64.of_int id);
  Buffer.to_bytes buf
;;

(* ------------------------------------------------------------------ *)
(* Load all metadata from the store                                     *)
(* ------------------------------------------------------------------ *)

let load_columns tx table_name =
  let prefix = column_prefix table_name in
  let%lwt cur = S.cursor_open tx sys_columns_tid in
  let _sr = S.cursor_seek cur prefix in
  let cols = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (ck, cv) ->
      let plen = Bytes.length prefix in
      if Bytes.length ck >= plen && Bytes.equal (Bytes.sub ck 0 plen) prefix
      then (
        cols := decode_column cv :: !cols;
        walk ())
  in
  walk ();
  S.cursor_close cur;
  Lwt.return (List.rev !cols)
;;

let load_all_tables store =
  let tbl = Hashtbl.create 16 in
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_tables_tid in
  let _sr = S.cursor_first cur in
  let rec walk_tables () =
    match S.cursor_next cur with
    | None -> Lwt.return_unit
    | Some (k, v) ->
      let%lwt () =
        Lwt.catch
          (fun () ->
             let name = Bytes.to_string k in
             let tid, next_rowid, without_rowid, autoincrement = decode_table_value v in
             let%lwt cols = load_columns tx name in
             Hashtbl.replace
               tbl
               name
               { name
               ; tree_id = tid
               ; columns = cols
               ; next_rowid
               ; fk_constraints = []
               ; without_rowid
               ; autoincrement
               };
             Lwt.return_unit)
          (fun _exn ->
             (* Corrupt primary catalog row/columns (#174): skip it here; the
                table is reconstructed from the redundant mirror in [open_]. *)
             Lwt.return_unit)
      in
      walk_tables ()
  in
  let%lwt () = walk_tables () in
  S.cursor_close cur;
  Lwt.return tbl
;;

let load_all_indexes store =
  let tbl = Hashtbl.create 8 in
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_indexes_tid in
  let _sr = S.cursor_first cur in
  let rec walk () =
    match S.cursor_next cur with
    | None -> Lwt.return_unit
    | Some (_k, v) ->
      let info = decode_index_value v in
      Hashtbl.replace tbl info.idx_name info;
      walk ()
  in
  let%lwt () = walk () in
  S.cursor_close cur;
  Lwt.return tbl
;;

let is_fts_rowid_key k =
  let slen = Bytes.length sys_fts_rowid_suffix in
  Bytes.length k >= slen
  && Bytes.equal (Bytes.sub k (Bytes.length k - slen) slen) sys_fts_rowid_suffix
;;

let load_all_fts store =
  let tbl = Hashtbl.create 4 in
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_fts_tid in
  let _sr = S.cursor_first cur in
  let rec walk () =
    match S.cursor_next cur with
    | None -> Lwt.return_unit
    | Some (k, v) ->
      (* Skip rowid counter keys: they end with "\x00rowid" *)
      if is_fts_rowid_key k
      then walk ()
      else (
        (try
           let name = Bytes.to_string k in
           let meta = decode_fts_value name v in
           Hashtbl.replace tbl name meta
         with
         | Invalid_argument msg ->
           (* Corrupt FTS catalog entry for key; skip and continue.
             A corrupt entry will simply be absent from the cache;
             queries against that table will fail with "table not found". *)
           Printf.eprintf "warning: skipping corrupt FTS catalog entry (%s)\n%!" msg);
        walk ())
  in
  let%lwt () = walk () in
  S.cursor_close cur;
  Lwt.return tbl
;;

(* ------------------------------------------------------------------ *)
(* View persistence                                                     *)
(* ------------------------------------------------------------------ *)

let load_all_views store =
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_views_tid in
  let _sr = S.cursor_first cur in
  let pairs = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      pairs := (Bytes.to_string k, Bytes.to_string v) :: !pairs;
      walk ()
  in
  walk ();
  S.cursor_close cur;
  Lwt.return (List.rev !pairs)
;;

(* #269: run [f tx] through the ambient explicit transaction ([?txn = Some tx],
   left uncommitted — the db layer owns its lifecycle) or, in autocommit, a fresh
   writer txn committed here.  Shared by the view/trigger persistence and the FK
   save so the borrow-or-autocommit plumbing lives in one place. *)
let borrow_or_autocommit ?txn store f =
  match txn with
  | Some tx -> f tx
  | None ->
    let%lwt tx = S.rw_begin store in
    let%lwt () = f tx in
    S.commit tx
;;

(* [?txn] (#269): persist/remove through the ambient explicit transaction when
   one is active, else autocommit. *)
let persist_view ?txn store ~name ~sql =
  borrow_or_autocommit ?txn store (fun tx ->
    S.put tx sys_views_tid (Bytes.of_string name) (Bytes.of_string sql))
;;

let remove_view ?txn store ~name =
  borrow_or_autocommit ?txn store (fun tx ->
    S.del tx sys_views_tid (Bytes.of_string name))
;;

(* ------------------------------------------------------------------ *)
(* Trigger persistence                                                  *)
(* ------------------------------------------------------------------ *)

let load_all_triggers store =
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_triggers_tid in
  let _sr = S.cursor_first cur in
  let pairs = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      pairs := (Bytes.to_string k, Bytes.to_string v) :: !pairs;
      walk ()
  in
  walk ();
  S.cursor_close cur;
  Lwt.return (List.rev !pairs)
;;

(* [?txn] (#269): persist/remove through the ambient explicit transaction when
   one is active, else autocommit. *)
let persist_trigger ?txn store ~name ~sql =
  borrow_or_autocommit ?txn store (fun tx ->
    S.put tx sys_triggers_tid (Bytes.of_string name) (Bytes.of_string sql))
;;

let remove_trigger ?txn store ~name =
  borrow_or_autocommit ?txn store (fun tx ->
    S.del tx sys_triggers_tid (Bytes.of_string name))
;;

(* ------------------------------------------------------------------ *)
(* FK constraint persistence                                            *)
(* ------------------------------------------------------------------ *)

let fk_meta_key table_name = Bytes.of_string ("fk:" ^ table_name)

let fk_action_to_string = function
  | FA_no_action -> "no_action"
  | FA_restrict -> "restrict"
  | FA_cascade -> "cascade"
  | FA_set_null -> "set_null"
  | FA_set_default -> "set_default"
;;

let fk_action_of_string = function
  | "no_action" -> FA_no_action
  | "restrict" -> FA_restrict
  | "cascade" -> FA_cascade
  | "set_null" -> FA_set_null
  | "set_default" -> FA_set_default
  | s -> failwith ("catalog: unknown fk_action: " ^ s)
;;

let encode_fks fks =
  let lines =
    List.map
      (fun fk ->
         String.concat
           "\t"
           [ String.concat "," fk.fk_local_cols
           ; fk.fk_parent_table
           ; String.concat "," fk.fk_parent_cols
           ; fk_action_to_string fk.fk_on_delete
           ; fk_action_to_string fk.fk_on_update
           ; (if fk.fk_deferrable then "1" else "0")
           ])
      fks
  in
  Bytes.of_string (String.concat "\n" lines)
;;

let decode_fks bytes =
  let s = Bytes.to_string bytes in
  if s = ""
  then []
  else
    List.filter_map
      (fun line ->
         match String.split_on_char '\t' line with
         | [ lc; pt; pc ] ->
           (* Legacy 3-field form (very old). *)
           Some
             { fk_local_cols = String.split_on_char ',' lc
             ; fk_parent_table = pt
             ; fk_parent_cols = String.split_on_char ',' pc
             ; fk_on_delete = FA_restrict
             ; fk_on_update = FA_restrict
             ; fk_deferrable = false
             }
         | [ lc; pt; pc; od; ou ] ->
           (* Pre-phase-35 5-field form: deferrable defaults false. *)
           Some
             { fk_local_cols = String.split_on_char ',' lc
             ; fk_parent_table = pt
             ; fk_parent_cols = String.split_on_char ',' pc
             ; fk_on_delete = fk_action_of_string od
             ; fk_on_update = fk_action_of_string ou
             ; fk_deferrable = false
             }
         | [ lc; pt; pc; od; ou; def ] ->
           (* Phase 35 6-field form. *)
           Some
             { fk_local_cols = String.split_on_char ',' lc
             ; fk_parent_table = pt
             ; fk_parent_cols = String.split_on_char ',' pc
             ; fk_on_delete = fk_action_of_string od
             ; fk_on_update = fk_action_of_string ou
             ; fk_deferrable = def = "1"
             }
         | _ -> None)
      (String.split_on_char '\n' s)
;;

(* ------------------------------------------------------------------ *)
(* Schema fingerprint + redundant catalog mirror helpers (#174)         *)
(* Defined here, ahead of all DDL and [open_], which use them.          *)
(* ------------------------------------------------------------------ *)

(* Schema fingerprint: a stable hash of a table's shape (columns +
   without_rowid).  Computed from the in-memory cache, so it always reflects
   the current schema after any DDL. *)
let fingerprint_of_meta (m : table_meta) =
  Schema_fingerprint.compute ~columns:m.columns ~without_rowid:m.without_rowid
;;

(* #174: register the table's page-header stamp (low 32 bits of its
   fingerprint) with the store, so its B+-tree pages self-identify their
   schema.  Skips the ephemeral CTE sentinel (tree_id = -1). *)
let register_tag store (m : table_meta) =
  if m.tree_id >= 0
  then S.set_tree_tag store m.tree_id (Schema_fingerprint.low32 (fingerprint_of_meta m))
;;

(* The mirror is keyed by tree_id (fixed 8-byte BE) and stores a fully
   self-describing schema blob — name, tree_id, WITHOUT ROWID, fingerprint,
   every column (via the same [encode_column] used by the primary), and FK
   constraints.  It carries no volatile state (no [next_rowid]) so it only
   changes on DDL, not on every insert. *)
(* v2 (#299): appends a trailing autoincrement byte after the column/FK blocks.
   v1 entries lack it; the decoder treats their absence as [false]. *)
(* v3 (#314): for an AUTOINCREMENT table ONLY, appends a presence byte + int64
   carrying the volatile [next_rowid] high-water, so mirror reconstruction can
   restore the sticky counter instead of recomputing max(rowid)+1 (which would
   make a committed-DELETE high-water reusable).  Non-AUTOINCREMENT tables write
   a presence byte of 0 and incur no per-insert mirror write. *)
let mirror_version = 3

let mirror_key (tid : S.tree_id) =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 (Int64.of_int tid);
  b
;;

let encode_mirror_entry (m : table_meta) =
  let buf = Buffer.create 128 in
  Varint.encode_uint64 buf (Int64.of_int mirror_version);
  Varint.encode_uint64 buf (Int64.of_int (String.length m.name));
  Buffer.add_string buf m.name;
  Varint.encode_uint64 buf (Int64.of_int m.tree_id);
  Buffer.add_uint8 buf (if m.without_rowid then 1 else 0);
  let fpb = Bytes.create 8 in
  Bytes.set_int64_be fpb 0 (fingerprint_of_meta m);
  Buffer.add_bytes buf fpb;
  Varint.encode_uint64 buf (Int64.of_int (List.length m.columns));
  List.iter
    (fun col ->
       let cb = encode_column col in
       Varint.encode_uint64 buf (Int64.of_int (Bytes.length cb));
       Buffer.add_bytes buf cb)
    m.columns;
  let fkb = encode_fks m.fk_constraints in
  Varint.encode_uint64 buf (Int64.of_int (Bytes.length fkb));
  Buffer.add_bytes buf fkb;
  (* #299 (mirror v2): trailing autoincrement byte. *)
  Buffer.add_uint8 buf (if m.autoincrement then 1 else 0);
  (* #314 (mirror v3): for an AUTOINCREMENT table, persist the volatile rowid
     high-water so mirror reconstruction restores it instead of recomputing
     max(rowid)+1.  Non-AUTOINCREMENT tables omit it (no per-insert mirror
     write).  Encoded as a presence byte + int64. *)
  if m.autoincrement
  then (
    Buffer.add_uint8 buf 1;
    Varint.encode_int64 buf m.next_rowid)
  else Buffer.add_uint8 buf 0;
  Buffer.to_bytes buf
;;

(* Decode a mirror entry into a [table_meta] (with [next_rowid =
   empty_next_rowid]; the mirror does not persist the rowid counter — it is
   recovered at open-time by scanning the data tree, see [recover_next_rowid])
   and the stored fingerprint. *)
let decode_mirror_entry bytes : table_meta * int64 =
  let ver, off = Varint.decode_uint64 bytes 0 in
  let ver = Int64.to_int ver in
  let nlen, off = Varint.decode_uint64 bytes off in
  let nlen = Int64.to_int nlen in
  let name = Bytes.sub_string bytes off nlen in
  let off = off + nlen in
  let tid, off = Varint.decode_uint64 bytes off in
  let without_rowid = Bytes.get_uint8 bytes off <> 0 in
  let off = off + 1 in
  let fp = Bytes.get_int64_be bytes off in
  let off = ref (off + 8) in
  let ncols, o = Varint.decode_uint64 bytes !off in
  off := o;
  let columns =
    List.init (Int64.to_int ncols) (fun _ ->
      let clen, o = Varint.decode_uint64 bytes !off in
      let clen = Int64.to_int clen in
      let col = decode_column (Bytes.sub bytes o clen) in
      off := o + clen;
      col)
  in
  let fklen, o = Varint.decode_uint64 bytes !off in
  let fkb = Bytes.sub bytes o (Int64.to_int fklen) in
  let fk_constraints = decode_fks fkb in
  off := o + Int64.to_int fklen;
  (* #299 (mirror v2): trailing autoincrement byte; absent in v1. *)
  let ai_present = ver >= 2 && !off < Bytes.length bytes in
  let autoincrement = ai_present && Bytes.get_uint8 bytes !off <> 0 in
  if ai_present then incr off;
  (* #314 (mirror v3): for an AUTOINCREMENT table, a presence byte followed (when
     nonzero) by the persisted [next_rowid] high-water.  v1/v2 blobs lack this;
     a v3 non-AUTOINCREMENT blob has presence byte 0.  Either way [next_rowid]
     defaults to [empty_next_rowid], so [recover_next_rowid] still recomputes
     max(rowid)+1 for those. *)
  let next_rowid =
    if ver >= 3 && !off < Bytes.length bytes && Bytes.get_uint8 bytes !off <> 0
    then (
      incr off;
      let v, o = Varint.decode_int64 bytes !off in
      off := o;
      v)
    else empty_next_rowid
  in
  ( { name
    ; tree_id = Int64.to_int tid
    ; columns
    ; next_rowid
    ; fk_constraints
    ; without_rowid
    ; autoincrement
    }
  , fp )
;;

(* Write/replace a table's mirror entry inside an already-open RW txn. *)
let put_mirror_tx tx (m : table_meta) =
  S.put tx sys_mirror_tid (mirror_key m.tree_id) (encode_mirror_entry m)
;;

(* Persist [m]'s rowid counter to the primary [_sys_tables] row, and — for
   AUTOINCREMENT tables (#314) — keep the redundant mirror's high-water in step
   so a later mirror reconstruction restores the sticky counter.  Non-
   AUTOINCREMENT tables skip the mirror write (no per-insert mirror amplification).
   Single home for the "mirror tracks primary" invariant shared by every
   counter-mutation path. *)
let put_table_counter_tx tx (m : table_meta) =
  let%lwt () = S.put tx sys_tables_tid (Bytes.of_string m.name) (encode_table_value m) in
  if m.autoincrement then put_mirror_tx tx m else Lwt.return_unit
;;

(* Remove a table's mirror entry inside an already-open RW txn. *)
let del_mirror_tx tx (tid : S.tree_id) = S.del tx sys_mirror_tid (mirror_key tid)

(* Decode every mirror entry into a [table_meta]; skip corrupt entries. *)
let load_mirror_entries store =
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_mirror_tid in
  let _sr = S.cursor_first cur in
  let acc = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (_k, v) ->
      (try
         let m, _fp = decode_mirror_entry v in
         acc := m :: !acc
       with
       | Invalid_argument _ | Failure _ -> ());
      walk ()
  in
  walk ();
  S.cursor_close cur;
  Lwt.return (List.rev !acc)
;;

(* #175: recover next_rowid for tables reconstructed from the mirror.
   Scan the table's data tree for the maximum integer rowid key (the
   tree is keyed by [Rowid.encode], whose offset-binary encoding sorts
   negatives correctly, so the last key in byte-sorted order is the maximum
   rowid).  Return [next_rowid = max + 1], or [empty_next_rowid] for an empty or
   unreadable tree (#250: so a subsequent NULL insert seeds at 1 and an explicit
   below-counter id seeds from its own value, exactly as in-session).  WITHOUT
   ROWID tables are skipped — they don't use rowid keys. *)
let recover_next_rowid store (m : table_meta) : table_meta Lwt.t =
  if m.without_rowid
  then Lwt.return m
  else if m.autoincrement && not (Int64.equal m.next_rowid empty_next_rowid)
  then
    (* #314: the mirror (v3) carries the sticky high-water for AUTOINCREMENT
       tables; trust it instead of recomputing max(rowid)+1, which would make a
       committed-DELETE high-water reusable.  Only a v3 AUTOINCREMENT mirror
       entry decodes to a non-empty [next_rowid], so the rollback-recompute
       caller (which passes live-cache, non-AUTOINCREMENT metas) never trips
       this branch. *)
    Lwt.return m
  else
    S.with_ro store
    @@ fun tx ->
    let%lwt cur = S.cursor_open tx m.tree_id in
    let _sr = S.cursor_first cur in
    let max_key = ref None in
    let rec walk () =
      match S.cursor_next cur with
      | None -> ()
      | Some (k, _) ->
        max_key := Some k;
        walk ()
    in
    walk ();
    S.cursor_close cur;
    let recovered =
      match !max_key with
      | None -> empty_next_rowid
      | Some k -> Int64.add (Rowid.decode k) 1L
    in
    Lwt.return { m with next_rowid = recovered }
;;

(* #299: read a table's LAST-COMMITTED [next_rowid] straight from its
   [_sys_tables] row.  Used by the AUTOINCREMENT rollback path: after
   [S.rollback] the store row has reverted to the committed value (the sticky
   high-water that a committed DELETE never lowers), so restoring the cached
   counter from it — rather than recomputing [max(rowid)+1] from data — keeps
   the counter sticky across committed deletes while still reverting a
   rolled-back allocation to the committed mark.  Returns [empty_next_rowid] if
   the row is absent (e.g. the table was created in the rolled-back txn — the
   caller skips uncached names anyway). *)
let read_committed_next_rowid store ~name : int64 Lwt.t =
  S.with_ro store
  @@ fun tx ->
  let%lwt v = S.get tx sys_tables_tid (Bytes.of_string name) in
  match v with
  | None -> Lwt.return empty_next_rowid
  | Some bytes ->
    let _tid, next, _wr, _ai = decode_table_value bytes in
    Lwt.return next
;;

let load_fk_constraints_raw store table_name =
  let key = fk_meta_key table_name in
  S.with_ro store
  @@ fun tx ->
  let%lwt v = S.get tx sys_meta_tid key in
  Lwt.return
    (match v with
     | None -> []
     | Some b -> decode_fks b)
;;

(* [?txn]: when set (#269) the FK rows are written through the ambient explicit
   transaction; otherwise an autocommit writer txn is used. *)
let save_fk_constraints ?txn t ~table_name ~fks =
  let key = fk_meta_key table_name in
  borrow_or_autocommit ?txn t.store (fun tx ->
    let%lwt () =
      if fks = []
      then S.del tx sys_meta_tid key
      else S.put tx sys_meta_tid key (encode_fks fks)
    in
    (* Keep the mirror's FK list current so a mirror reconstruction restores
       constraints, not just columns. *)
    match Schema_cache.find_table t.sc table_name with
    | Some m -> put_mirror_tx tx { m with fk_constraints = fks }
    | None -> Lwt.return_unit)
;;

(* #269/#282: this is a RAW, undo-free cache update by design — the FK mutation's
   ROLLBACK is handled by the enclosing operation (the txn store-rollback, or
   [add_column]'s schema-cache undo which restores the whole prior [table_meta]),
   never by this call.  Hence [put_table_durable] (no undo), matching the original
   [Hashtbl.replace] semantics exactly. *)
let set_fk_constraints t ~table_name ~fks =
  match Schema_cache.find_table t.sc table_name with
  | None -> ()
  | Some meta ->
    Schema_cache.put_table_durable
      t.sc
      ~name:table_name
      { meta with fk_constraints = fks }
;;

(* ------------------------------------------------------------------ *)
(* Public API                                                           *)
(* ------------------------------------------------------------------ *)

let open_ store =
  let%lwt cache = load_all_tables store in
  let%lwt indexes = load_all_indexes store in
  let%lwt fts = load_all_fts store in
  (* Load FK constraints for each table *)
  let names = Hashtbl.fold (fun k _ acc -> k :: acc) cache [] in
  let%lwt () =
    Lwt_list.iter_s
      (fun name ->
         let%lwt fks = load_fk_constraints_raw store name in
         (match Hashtbl.find_opt cache name with
          | Some meta -> Hashtbl.replace cache name { meta with fk_constraints = fks }
          | None -> ());
         Lwt.return_unit)
      names
  in
  (* #174: reconstruct any table missing from the primary catalog (its
     _sys_tables row or column entries were lost or failed to decode) from the
     redundant mirror.  Tables loaded fine from the primary are left untouched
     here; the mirror only fills gaps.  Reconstructed entries carry the
     mirror's own columns and FK constraints (the primary FK load above ran
     only over primary tables). *)
  let%lwt mirror = load_mirror_entries store in
  let present_tids =
    Hashtbl.fold (fun _ (m : table_meta) acc -> m.tree_id :: acc) cache []
  in
  let reconstructed =
    List.filter (fun (m : table_meta) -> not (List.mem m.tree_id present_tids)) mirror
  in
  List.iter (fun (m : table_meta) -> Hashtbl.replace cache m.name m) reconstructed;
  (* #175: for tables reconstructed from the mirror, recover next_rowid by
     scanning the data tree for the maximum integer rowid key.  WITHOUT ROWID
     tables are skipped.  Best-effort: defaults to 1L for empty trees. *)
  let%lwt () =
    Lwt_list.iter_s
      (fun (m : table_meta) ->
         let%lwt recovered = recover_next_rowid store m in
         Hashtbl.replace cache recovered.name recovered;
         Lwt.return_unit)
      reconstructed
  in
  (* #174: schema-drift check on open.  For tables present in BOTH the primary
     and the mirror, a fingerprint mismatch means one copy is corrupt or drifted
     — warn (but stay openable so recovery tooling can still run).  Reconstructed
     tables match the mirror by construction, so they never trip this. *)
  List.iter
    (fun (m : table_meta) ->
       match Hashtbl.find_opt cache m.name with
       | Some primary
         when primary.tree_id = m.tree_id
              && not (Int64.equal (fingerprint_of_meta primary) (fingerprint_of_meta m))
         ->
         Printf.eprintf
           "warning: schema fingerprint mismatch for table %s — primary and redundant \
            catalog disagree (possible corruption, #174)\n\
            %!"
           m.name
       | _ -> ())
    mirror;
  (* #283: seed the sealed cache durably (no undo, this is open-time state).
     [put_table_durable] re-stamps each table's #174 page-header tag, replacing
     the old explicit [register_tag] iteration. *)
  let sc = Schema_cache.create ~stamp:(fun m -> register_tag store m) in
  Hashtbl.iter (fun name m -> Schema_cache.put_table_durable sc ~name m) cache;
  Hashtbl.iter (fun name i -> Schema_cache.put_index_durable sc ~name i) indexes;
  Hashtbl.iter (fun name m -> Schema_cache.put_fts_durable sc ~name m) fts;
  Lwt.return
    { store
    ; sc
    ; fk_enforcement = false
    ; recursive_triggers = true
    ; defer_fks_pragma = false
    ; pending_fk_checks = []
    ; last_inserted_rowid = 0L
    }
;;

(* #243 (T1): last-inserted rowid accessors for [last_insert_rowid()]. *)
let set_last_inserted_rowid t rowid = t.last_inserted_rowid <- rowid
let last_inserted_rowid t = t.last_inserted_rowid

(** Allocate and return the next available user tree ID, atomically incrementing the counter. *)
let next_user_tid t =
  let%lwt tid = read_next_user_tid t.store in
  let%lwt () = write_next_user_tid t.store (tid + 1) in
  Lwt.return tid
;;

(* #269: register an in-memory schema-cache reversal for a change run through an
   explicit transaction.  Delegates to the sealed [Schema_cache] undo log so the
   db layer's view/trigger-cache reversals replay in order with the catalog's own
   DDL undos.  (Catalog DDL self-registers; this entry point is for the db layer's
   own [t.views]/[t.triggers] caches, which live outside the catalog.) *)
let register_schema_undo t f = Schema_cache.register_undo t.sc f

(* #286: mark the ambient explicit transaction uncommittable because an in-txn
   DDL statement failed partway through (partial on-disk effects remain). *)
let mark_schema_txn_poisoned t = Schema_cache.mark_poisoned t.sc
let schema_txn_poisoned t = Schema_cache.is_poisoned t.sc

(* #293: COMMIT keeps the bumped next_rowid counter, so do NOT recompute — but
   clears the dirty set so a later unrelated ROLLBACK won't wrongly recompute a
   table that wasn't bumped in that later txn. *)
let commit_schema_changes t = Schema_cache.commit t.sc

(* #293: the rowid dirty set is intentionally NOT cleared on rollback here — the
   db layer calls [recompute_rowid_counters_after_rollback] right after, which
   reads the set then clears it. *)
let rollback_schema_changes t = Schema_cache.rollback t.sc

(* #293: re-derive the cached [next_rowid] from the (now rolled-back) data tree,
   using the same max(rowid)+1 logic as [recover_next_rowid], for ONLY the tables
   whose counter was bumped during the rolled-back transaction.  An INSERT run
   THROUGH an explicit transaction bumps the in-memory counter via
   [next_rowid_in_txn]/[bump_next_rowid_in_txn] (which also record the table in
   [rowid_bumped_in_txn] and write the bumped value to [_sys_tables] under the
   txn).  On ROLLBACK the store row reverts but the in-memory counter does not,
   so the next allocation would SKIP the rolled-back rowid instead of reusing it.
   SQLite, for a plain (non-AUTOINCREMENT) rowid table, recomputes max(rowid)+1
   from the data after a rollback and so reuses it; recomputing here matches that.

   Option (b) from the issue: rather than snapshot/restore each DML's counter
   delta, we drop straight to the authoritative source (the data tree).  This
   leans on the store having already been rolled back — the db layer calls this
   only AFTER [S.rollback], so the trees show the last-committed state and the
   RW lock is released (so [recover_next_rowid]'s own RO txn cannot deadlock).

   We recompute ONLY the [rowid_bumped_in_txn] set — usually a single table —
   rather than every cached rowid table.  [recover_next_rowid] is an O(n) tree
   walk to find max(rowid); scanning every table would make a rollback cost
   O(total rows across ALL tables), a regression on a perf-sensitive engine
   (cf. #228/#229 driving cursor_open O(n)->O(log n)).  Restricting to the
   bumped set keeps rollback ~O(1) in the common case.  A bumped name that is no
   longer cached (e.g. its CREATE TABLE rolled back in the same txn) or that is
   WITHOUT ROWID is skipped.  The set is CLEARED here so it never leaks into a
   later transaction; commit clears it too (via [commit_schema_changes]), which
   is why a COMMIT keeps the bumped counter yet a subsequent unrelated ROLLBACK
   does not wrongly recompute it.

   #299: AUTOINCREMENT tables take a DIFFERENT branch.  Their counter is a
   sticky high-water that a committed DELETE never lowers, so recomputing
   [max(rowid)+1] from data would wrongly reissue an id below the high-water.
   Instead we restore the cached counter to the LAST-COMMITTED value persisted
   in [_sys_tables] (which [S.rollback] has already reverted to), via
   [read_committed_next_rowid].  That still reverts a rolled-back allocation to
   the committed mark (matching SQLite, whose [sqlite_sequence] is itself
   transactional) while preserving stickiness across committed deletes. *)
let recompute_rowid_counters_after_rollback t =
  let names = Schema_cache.take_rowid_bumped t.sc in
  Lwt_list.iter_s
    (fun name ->
       match Schema_cache.find_table t.sc name with
       | None -> Lwt.return_unit
       | Some m when m.without_rowid -> Lwt.return_unit
       | Some m when m.autoincrement ->
         let%lwt committed = read_committed_next_rowid t.store ~name in
         Schema_cache.set_rowid_durable t.sc ~name { m with next_rowid = committed };
         Lwt.return_unit
       | Some m ->
         let%lwt recovered = recover_next_rowid t.store m in
         Schema_cache.set_rowid_durable t.sc ~name recovered;
         Lwt.return_unit)
    names
;;

(* #280/#295: open a savepoint over the schema-undo log.  Records the current
   [schema_undo] list so [ROLLBACK TO]/[RELEASE] of this savepoint can find the
   boundary between entries registered before and after it, and the current
   [schema_txn_poisoned] flag (#295) so [ROLLBACK TO] can restore the poison
   state as it was when this savepoint opened. *)
let savepoint_begin_schema t name = Schema_cache.savepoint_begin t.sc name

(* #280/#295: ROLLBACK TO a savepoint.  Run+drop the undo closures registered
   since the savepoint (the prefix of [schema_undo] down to its recorded
   snapshot, most-recent-first), reset [schema_undo] to that snapshot, restore
   [schema_txn_poisoned] to its snapshot (#295), drop newer savepoint markers,
   and keep this savepoint so it can be rolled back to again (mirroring
   [Store.savepoint_rollback]).  Unknown name: no-op.

   #295: the poison restore is what un-poisons a txn whose failed in-txn DDL
   lay AFTER this savepoint (its partial effects are in the unwound range).  A
   failure that PREDATES the savepoint left the poison flag already set when
   this savepoint opened, so the snapshot is [true] and the txn stays poisoned —
   exactly correct, since those partial effects are NOT unwound here. *)
let savepoint_rollback_schema t name = Schema_cache.savepoint_rollback t.sc name

(* #280/#295: RELEASE a savepoint.  The since-savepoint undo entries merge into
   the enclosing scope, so [schema_undo] is untouched — only the marker (and any
   newer markers, including their recorded poison snapshots) is dropped
   (mirroring [Store.savepoint_release]).  The poison flag itself is left as-is:
   a poison raised since the savepoint survives the RELEASE into the enclosing
   scope.  An outer ROLLBACK still unwinds the merged entries.  Unknown name:
   no-op. *)
let savepoint_release_schema t name = Schema_cache.savepoint_release t.sc name

(* Write a table's catalog rows (primary + columns + mirror) through [tx] and
   return its meta.  Shared by the autocommit and in-transaction paths. *)
let put_table_rows tx ~name ~columns ~without_rowid ~autoincrement ~tid =
  let m =
    { name
    ; tree_id = tid
    ; columns
    ; next_rowid = empty_next_rowid
    ; fk_constraints = []
    ; without_rowid
    ; autoincrement
    }
  in
  let%lwt () = S.put tx sys_tables_tid (Bytes.of_string name) (encode_table_value m) in
  let%lwt () =
    Lwt_list.iteri_s
      (fun i col -> S.put tx sys_columns_tid (column_key name i) (encode_column col))
      columns
  in
  let%lwt () = put_mirror_tx tx m in
  Lwt.return m
;;

(* [?txn]: when an explicit transaction is active the executor threads it here
   (#269) so the table's catalog rows AND its tree-ID allocation participate in
   that transaction — no nested [rw_begin] (which would self-deadlock), and a
   [ROLLBACK] discards both the rows and the cache entry.  When absent, the
   autocommit path opens and commits its own writer txn (counter bump first, as
   before). *)
let create_table ?txn t ~name ~columns ~without_rowid ~autoincrement =
  if Schema_cache.mem_table t.sc name
  then failwith (Printf.sprintf "table '%s' already exists" name);
  match txn with
  | Some tx ->
    let%lwt tid = next_user_tid_tx tx in
    let%lwt m = put_table_rows tx ~name ~columns ~without_rowid ~autoincrement ~tid in
    (* [put_table] also stamps the store-global in-memory [tree_tags] for [tid].
       On ROLLBACK we revert only the cache entry, not the tag — but that is
       safe: the txn also rolls back [tid]'s allocation (the user-tid counter is
       restored), leaving [tid] unallocated, so no table_meta references it and
       nothing writes pages to it.  The next CREATE reuses [tid] and overwrites
       the tag.  A lingering stamp for an unreferenced tree therefore cannot
       mis-stamp any page (#174). *)
    Schema_cache.put_table t.sc ~name m;
    Lwt.return tid
  | None ->
    let%lwt tid = next_user_tid t in
    let%lwt tx = S.rw_begin t.store in
    let%lwt m = put_table_rows tx ~name ~columns ~without_rowid ~autoincrement ~tid in
    let%lwt () = S.commit tx in
    Schema_cache.put_table_durable t.sc ~name m;
    Lwt.return tid
;;

let find_table t ~name = Lwt.return (Schema_cache.find_table t.sc name)
let find_table_cached t ~name = Schema_cache.find_table t.sc name

let table_fingerprint t ~name =
  Option.map fingerprint_of_meta (Schema_cache.find_table t.sc name)
;;

let fingerprints_by_tree_id t =
  Schema_cache.fold_tables
    (fun _ (m : table_meta) acc ->
       (* Skip the ephemeral CTE sentinel (tree_id = -1): no real on-disk tree. *)
       if m.tree_id >= 0 then (m.tree_id, fingerprint_of_meta m) :: acc else acc)
    t.sc
    []
;;

(* All [(tree_id, fingerprint)] pairs recorded in the mirror. *)
let mirror_fingerprints t =
  S.with_ro t.store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_mirror_tid in
  let _sr = S.cursor_first cur in
  let acc = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (_k, v) ->
      (try
         let m, fp = decode_mirror_entry v in
         acc := (m.tree_id, fp) :: !acc
       with
       | Invalid_argument _ | Failure _ -> ());
      walk ()
  in
  walk ();
  S.cursor_close cur;
  Lwt.return !acc
;;

type schema_discrepancy =
  | Fingerprint_mismatch of
      { tree_id : S.tree_id
      ; primary : int64
      ; mirror : int64
      }
  | Missing_in_mirror of S.tree_id
  | Missing_in_primary of S.tree_id

let verify_against_mirror t =
  let%lwt mirror = mirror_fingerprints t in
  let primary = fingerprints_by_tree_id t in
  let findings = ref [] in
  List.iter
    (fun (tid, pfp) ->
       match List.assoc_opt tid mirror with
       | None -> findings := Missing_in_mirror tid :: !findings
       | Some mfp ->
         if not (Int64.equal pfp mfp)
         then
           findings
           := Fingerprint_mismatch { tree_id = tid; primary = pfp; mirror = mfp }
              :: !findings)
    primary;
  List.iter
    (fun (tid, _) ->
       if not (List.mem_assoc tid primary)
       then findings := Missing_in_primary tid :: !findings)
    mirror;
  Lwt.return (List.rev !findings)
;;

let register_ephemeral t (meta : table_meta) =
  Schema_cache.put_table_durable t.sc ~name:meta.name meta
;;

let unregister_ephemeral t ~name = Schema_cache.remove_table_durable t.sc ~name

let list_tables t =
  Lwt.return (Schema_cache.fold_tables (fun _ v acc -> v :: acc) t.sc [])
;;

(* #250: pick the rowid to auto-allocate for a NULL/omitted id, and the new
   counter.  An unseeded table ([empty_next_rowid]) allocates 1 (SQLite: empty
   table -> rowid 1); a seeded one allocates the running [max+1] counter.  The
   new counter is [id+1], except at the [max_int] ceiling we hold at [max_int]
   rather than wrap to [Int64.min_int] (which is the empty sentinel) — a further
   NULL insert then re-tries [max_int] and collides, instead of silently
   resetting the table to "empty". *)
let alloc_rowid (m : table_meta) : int64 * int64 =
  let id = if Int64.equal m.next_rowid empty_next_rowid then 1L else m.next_rowid in
  let next = if Int64.equal id Int64.max_int then Int64.max_int else Int64.add id 1L in
  id, next
;;

let next_rowid t ~name =
  match Schema_cache.find_table t.sc name with
  | None -> failwith (Printf.sprintf "no table '%s'" name)
  | Some m ->
    let id, next = alloc_rowid m in
    let m' = { m with next_rowid = next } in
    (* Update the cache BEFORE [rw_begin] (which yields), matching the original
       order: find -> alloc -> cache-write stays atomic under Lwt so two
       concurrent autocommit callers cannot read the same stale counter. *)
    Schema_cache.set_rowid_durable t.sc ~name m';
    let%lwt tx = S.rw_begin t.store in
    let%lwt () = put_table_counter_tx tx m' in
    let%lwt () = S.commit tx in
    Lwt.return id
;;

(** Like [next_rowid] but uses an already-acquired RW transaction.
    The txn is NOT committed; the caller is responsible for the commit.
    Use this when an explicit transaction is already held to avoid
    deadlocking on the store's RW mutex. *)
let next_rowid_in_txn t ~name (tx : S.rw S.txn) =
  match Schema_cache.find_table t.sc name with
  | None -> failwith (Printf.sprintf "no table '%s'" name)
  | Some m ->
    (* #312: an AUTOINCREMENT table whose counter is already pinned at max_int
       (a max_int rowid exists) cannot allocate another id.  SQLite raises
       SQLITE_FULL here instead of probing for a free rowid; match its wording.
       Use [Lwt.fail_with] so it surfaces as a catchable SQL [Error] like the
       other user-facing insert failures.  Plain rowid tables keep the existing
       hold-at-max behavior.
       Known conflation (PR#315 review): [next_rowid = max_int] means both
       "max_int already handed out" and "max_int is next to hand out", so we
       raise one id early in the pure auto-increment path (an explicit insert of
       max_int-1 bumps next to max_int, then the next NULL insert raises instead
       of allocating max_int).  Unreachable in practice — it needs 2^63 rows. *)
    if m.autoincrement && Int64.equal m.next_rowid Int64.max_int
    then Lwt.fail_with "database or disk is full"
    else (
      let id, next = alloc_rowid m in
      let m' = { m with next_rowid = next } in
      (* #293: [bump_rowid] caches [m'] and marks this table's counter dirty so a
         ROLLBACK recomputes only it. *)
      Schema_cache.bump_rowid t.sc ~name m';
      let%lwt () = put_table_counter_tx tx m' in
      Lwt.return id)
;;

(** #243 (T1): after an INSERT supplies an explicit INTEGER PRIMARY KEY value,
    advance the autoincrement counter so a later NULL/omitted insert receives a
    fresh, non-colliding id (SQLite parity: rowid becomes max(existing)+1).
    [at_least] is the smallest value the next allocation must be (= id + 1).

    #250: an UNSEEDED table ([empty_next_rowid]) seeds directly from [at_least],
    even when that is <= 1 — the explicit id IS the table's max, so a below-1 id
    (e.g. -5) correctly makes the next NULL insert -4 instead of 1.  A seeded
    counter only ever rises and never lowers.  [at_least] is always [id+1] with
    [id < max_int] (the caller guards the ceiling), so it can never be the empty
    sentinel. *)
let bump_next_rowid_in_txn t ~name ~at_least (tx : S.rw S.txn) =
  match Schema_cache.find_table t.sc name with
  | None -> failwith (Printf.sprintf "no table '%s'" name)
  | Some m ->
    let unseeded = Int64.equal m.next_rowid empty_next_rowid in
    if (not unseeded) && Int64.compare at_least m.next_rowid <= 0
    then Lwt.return_unit
    else (
      let m' = { m with next_rowid = at_least } in
      (* #293: [bump_rowid] caches [m'] and marks dirty only when the counter
         actually moved (the early-return no-op above leaves the cached counter
         untouched, so nothing to recompute). *)
      Schema_cache.bump_rowid t.sc ~name m';
      (* #314: [put_table_counter_tx] mirrors the high-water for AUTOINCREMENT
         tables.  The no-op early-return path above never reaches here, so a
         non-moving bump leaves both primary and mirror untouched. *)
      put_table_counter_tx tx m')
;;

(* #312.1: largest stored rowid in a table's data tree, computed within an
   already-open txn.  Used only on the uncommon lower-clamp path of a writable
   [sqlite_sequence] SET/INSERT, to avoid lowering the counter below the live
   max(rowid).  The store has no [cursor_last]/[cursor_prev], so this reuses the
   forward walk from [recover_next_rowid]: [Rowid.encode]'s offset-binary
   encoding sorts integer rowids correctly, so the last key in byte order is the
   maximum.  Returns [None] for an empty tree. *)
let max_rowid_in_txn t ~name (tx : 'a S.txn) : int64 option Lwt.t =
  match Schema_cache.find_table t.sc name with
  (* Private helper; the sole caller ([set_next_rowid_in_txn]) has already
     confirmed the table is present, so this arm is unreachable in practice. *)
  | None -> failwith (Printf.sprintf "no table '%s'" name)
  | Some m ->
    let%lwt cur = S.cursor_open tx m.tree_id in
    let _sr = S.cursor_first cur in
    let max_key = ref None in
    let rec walk () =
      match S.cursor_next cur with
      | None -> ()
      | Some (k, _) ->
        max_key := Some k;
        walk ()
    in
    walk ();
    S.cursor_close cur;
    Lwt.return
      (match !max_key with
       | None -> None
       | Some k -> Some (Rowid.decode k))
;;

(* #312.1: writable [sqlite_sequence] SET/INSERT for table [name] with the
   requested seq value [requested].  Faithful to SQLite's effective rule
   [next = max(requested, max(rowid)) + 1]: the new counter is
   [max(requested + 1, max(rowid) + 1)].
   - RAISE path (the common case): when [requested + 1 >= next_rowid] and the
     counter is already seeded, [next_rowid] already equals [max(rowid) + 1], so
     just set [next_rowid := requested + 1] — no tree scan needed.
   - LOWER path: when [requested + 1] would drop below the counter (or the
     counter is unseeded), clamp to [max(rowid) + 1] so the next insert never
     collides with a live row.
   Mutates through [tx] via [put_table_counter_tx] (primary row + #314 mirror)
   and marks the counter dirty ([Schema_cache.bump_rowid]) so a ROLLBACK reverts
   it via [recompute_rowid_counters_after_rollback].  The table must exist and
   be AUTOINCREMENT. *)
let set_next_rowid_in_txn t ~name ~requested (tx : S.rw S.txn) =
  match Schema_cache.find_table t.sc name with
  | None -> failwith (Printf.sprintf "sqlite_sequence: no such table '%s'" name)
  | Some m ->
    if not m.autoincrement
    then
      failwith (Printf.sprintf "sqlite_sequence: '%s' is not an AUTOINCREMENT table" name)
    else (
      (* Saturating add: [requested = max_int] means "max_int was handed out", so
         the counter must pin at [max_int] (the next insert then raises
         SQLITE_FULL).  A plain [requested + 1] would overflow to [min_int] and
         fall into the lower-clamp path, silently resetting to [max(rowid)+1]. *)
      let want_next =
        if Int64.equal requested Int64.max_int
        then Int64.max_int
        else Int64.add requested 1L
      in
      let%lwt clamped =
        if
          (not (Int64.equal m.next_rowid empty_next_rowid))
          && Int64.compare want_next m.next_rowid >= 0
        then Lwt.return want_next
        else (
          let%lwt mx = max_rowid_in_txn t ~name tx in
          let floor =
            match mx with
            | Some k -> Int64.add k 1L
            | None -> 1L
          in
          Lwt.return (if Int64.compare want_next floor > 0 then want_next else floor))
      in
      let m' = { m with next_rowid = clamped } in
      Schema_cache.bump_rowid t.sc ~name m';
      put_table_counter_tx tx m')
;;

(* #312.1: writable [sqlite_sequence] DELETE for table [name].  Resets the
   counter to [empty_next_rowid] so the next insert recomputes from data —
   matching SQLite removing the [sqlite_sequence] row.  Mutates through [tx] and
   marks the counter dirty so a ROLLBACK reverts it.  The table must exist and be
   AUTOINCREMENT. *)
let reset_next_rowid_in_txn t ~name (tx : S.rw S.txn) =
  match Schema_cache.find_table t.sc name with
  | None -> failwith (Printf.sprintf "sqlite_sequence: no such table '%s'" name)
  | Some m ->
    if not m.autoincrement
    then
      failwith (Printf.sprintf "sqlite_sequence: '%s' is not an AUTOINCREMENT table" name)
    else (
      let m' = { m with next_rowid = empty_next_rowid } in
      Schema_cache.bump_rowid t.sc ~name m';
      put_table_counter_tx tx m')
;;

(* #312.1: [DELETE FROM sqlite_sequence] with no WHERE — reset EVERY seeded
   AUTOINCREMENT counter (SQLite parity, and what a real [sqlite3 .dump] emits
   before re-INSERTing).  Non-AUTOINCREMENT and already-unseeded tables are left
   untouched (no spurious mirror writes). *)
let reset_all_next_rowid_in_txn t (tx : S.rw S.txn) =
  let%lwt tables = list_tables t in
  Lwt_list.iter_s
    (fun (m : table_meta) ->
       if m.autoincrement && not (Int64.equal m.next_rowid empty_next_rowid)
       then (
         let m' = { m with next_rowid = empty_next_rowid } in
         Schema_cache.bump_rowid t.sc ~name:m.name m';
         put_table_counter_tx tx m')
       else Lwt.return_unit)
    tables
;;

(* [?txn]: as for [create_table] (#269), an active explicit transaction is
   threaded here so the index's catalog row, tree-ID and index-ID allocation,
   and cache entry all participate in it and roll back together. *)
let create_index ?txn t ~name ~table ~columns ~unique ~expr_flags ~where_sql ~origin =
  if Schema_cache.mem_index t.sc name
  then Lwt.return (Error (Printf.sprintf "index '%s' already exists" name))
  else (
    match Schema_cache.find_table t.sc table with
    | None -> Lwt.return (Error (Printf.sprintf "no table '%s'" table))
    | Some tm ->
      (* Validate: for plain columns, check they exist in the table; skip for expression columns *)
      let col_with_flags = List.combine columns expr_flags in
      let missing =
        List.find_opt
          (fun (col, is_expr) ->
             (not is_expr)
             && not (List.exists (fun (c : Row.column) -> c.name = col) tm.columns))
          col_with_flags
      in
      (match missing with
       | Some (col, _) ->
         Lwt.return (Error (Printf.sprintf "no column '%s' on table '%s'" col table))
       | None ->
         let mk_info tid =
           { idx_name = name
           ; idx_table = table
           ; idx_columns = columns
           ; idx_unique = unique
           ; idx_tree_id = tid
           ; idx_expr_flags = expr_flags
           ; idx_where_sql = where_sql
           ; idx_origin = origin
           }
         in
         (match txn with
          | Some tx ->
            let%lwt tid = next_user_tid_tx tx in
            let%lwt id = read_next_index_id_tx tx in
            let%lwt () = write_next_index_id_tx tx (id + 1) in
            let info = mk_info tid in
            let%lwt () =
              S.put tx sys_indexes_tid (index_key id) (encode_index_value info)
            in
            Schema_cache.put_index t.sc ~name info;
            Lwt.return (Ok info)
          | None ->
            let%lwt tid = next_user_tid t in
            let%lwt id = read_next_index_id t.store in
            let%lwt () = write_next_index_id t.store (id + 1) in
            let info = mk_info tid in
            let%lwt tx = S.rw_begin t.store in
            let%lwt () =
              S.put tx sys_indexes_tid (index_key id) (encode_index_value info)
            in
            let%lwt () = S.commit tx in
            Schema_cache.put_index_durable t.sc ~name info;
            Lwt.return (Ok info))))
;;

(* [?txn] (#282): when an explicit transaction is active the executor threads it
   here so the column's catalog row and mirror refresh participate in it (no
   nested [rw_begin], which would self-deadlock against the writer lock the txn
   already holds).  A schema-cache undo restores the prior [table_meta] AND its
   tree-tag fingerprint on [ROLLBACK] — unlike a fresh CREATE the tree_id stays
   allocated, so the #174 page-stamp must revert to the old schema too. *)
let add_column ?txn t ~table_name ~(column : Row.column) =
  match Schema_cache.find_table t.sc table_name with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" table_name))
  | Some meta ->
    let exists =
      List.exists (fun c -> String.equal c.Row.name column.Row.name) meta.columns
    in
    if exists
    then Lwt.return (Error (Printf.sprintf "column already exists: %s" column.Row.name))
    else (
      let new_cols = meta.columns @ [ column ] in
      let new_meta = { meta with columns = new_cols } in
      let ordinal = List.length meta.columns in
      let col_k = column_key table_name ordinal in
      let col_v = encode_column column in
      let%lwt () =
        borrow_or_autocommit ?txn t.store (fun tx ->
          let%lwt () = S.put tx sys_columns_tid col_k col_v in
          put_mirror_tx tx new_meta)
      in
      (match txn with
       | Some _ -> Schema_cache.put_table t.sc ~name:table_name new_meta
       | None -> Schema_cache.put_table_durable t.sc ~name:table_name new_meta);
      Lwt.return (Ok ()))
;;

let indexes_for_table t ~table =
  Schema_cache.fold_indexes
    (fun _ info acc -> if info.idx_table = table then info :: acc else acc)
    t.sc
    []
;;

let find_index t ~name = Schema_cache.find_index t.sc name

let find_index_covering_cols t ~table_name ~col_idxs =
  match Schema_cache.find_table t.sc table_name with
  | None -> None
  | Some meta ->
    let n_target = List.length col_idxs in
    if n_target = 0
    then None
    else (
      (* Resolve col_idxs to column names; bail out if any idx is out of range. *)
      let cols_arr = Array.of_list meta.columns in
      let n_cols = Array.length cols_arr in
      let target_names_opt =
        try
          Some
            (List.map
               (fun i ->
                  if i < 0 || i >= n_cols then raise Exit else cols_arr.(i).Row.name)
               col_idxs)
        with
        | Exit -> None
      in
      match target_names_opt with
      | None -> None
      | Some target_names ->
        let candidates = indexes_for_table t ~table:table_name in
        List.find_opt
          (fun (i : index_info) ->
             (* Skip partial indexes — a row absent from the index may still
             satisfy the FK predicate (the WHERE clause masks rows). *)
             if i.idx_where_sql <> None
             then false
             else (
               (* Skip indexes that contain any expression column in the leading
               prefix we'd be scanning — we cannot match a raw value list
               against an expression key. *)
               let n_idx = List.length i.idx_columns in
               if n_idx < n_target
               then false
               else (
                 let prefix_names =
                   List.filteri (fun k _ -> k < n_target) i.idx_columns
                 in
                 let prefix_flags =
                   let len_flags = List.length i.idx_expr_flags in
                   if len_flags = 0
                   then List.init n_target (fun _ -> false)
                   else List.filteri (fun k _ -> k < n_target) i.idx_expr_flags
                 in
                 let no_expr_in_prefix = not (List.exists Fun.id prefix_flags) in
                 no_expr_in_prefix
                 &&
                 try List.for_all2 String.equal prefix_names target_names with
                 | Invalid_argument _ -> false)))
          candidates)
;;

let table_exists t ~name = Schema_cache.mem_table t.sc name
let index_exists t ~name = Schema_cache.mem_index t.sc name

(** Scan _sys_indexes (using the given txn) to find the key for [name].
    Returns [None] if not found.

    [cursor_first] positions at the first entry (id=0 when it exists).
    We inspect that entry immediately via [cursor_next] — which on a
    pre-positioned cursor returns the current entry without advancing —
    so no entry is ever skipped, including the very first one. *)
let find_index_key_in_txn tx name =
  let%lwt cur = S.cursor_open tx sys_indexes_tid in
  (* Position at the first entry; returns Not_found `End if the tree is
     empty, in which case cursor_next will immediately return None. *)
  let _sr = S.cursor_first cur in
  let result = ref None in
  (* cursor_next after cursor_first returns the positioned (first) entry on
     its initial call, then advances on each subsequent call. *)
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      let info = decode_index_value v in
      if info.idx_name = name then result := Some k else walk ()
  in
  walk ();
  S.cursor_close cur;
  Lwt.return !result
;;

let drop_index t tx ~name =
  (* Remove from _sys_indexes on disk by scanning for the numeric key. *)
  let%lwt key_opt = find_index_key_in_txn tx name in
  let%lwt () =
    match key_opt with
    | None -> Lwt.return_unit
    | Some key -> S.del tx sys_indexes_tid key
  in
  (* Update in-memory cache.  [remove_index] self-registers a ROLLBACK restore. *)
  Schema_cache.remove_index t.sc ~name;
  Lwt.return_unit
;;

let drop_table t tx ~name =
  (* 0. Remove the mirror entry (keyed by tree_id), if we know the tree_id. *)
  let%lwt () =
    match Schema_cache.find_table t.sc name with
    | Some m -> del_mirror_tx tx m.tree_id
    | None -> Lwt.return_unit
  in
  (* 1. Remove table entry from _sys_tables. *)
  let%lwt () = S.del tx sys_tables_tid (Bytes.of_string name) in
  (* 2. Remove all column entries from _sys_columns. *)
  let n_cols =
    match Schema_cache.find_table t.sc name with
    | None -> 0
    | Some m -> List.length m.columns
  in
  let%lwt () =
    Lwt_list.iter_s
      (fun i -> S.del tx sys_columns_tid (column_key name i))
      (List.init n_cols (fun i -> i))
  in
  (* 3. Remove all associated indexes.  Each [drop_index] self-registers its own
     ROLLBACK restore. *)
  let idx_list = indexes_for_table t ~table:name in
  let%lwt () =
    Lwt_list.iter_s
      (fun (idx : index_info) -> drop_index t tx ~name:idx.idx_name)
      idx_list
  in
  (* 4. Update in-memory cache.  [remove_table] self-registers a ROLLBACK restore. *)
  Schema_cache.remove_table t.sc ~name;
  Lwt.return_unit
;;

(* #282: on a corrupt-catalog Error this no longer rolls [tx] back itself — the
   caller decides.  In autocommit the caller aborts its own writer txn; when the
   txn is borrowed from an ambient explicit transaction, aborting it here would
   tear down the user's whole transaction, so we surface the Error and let the
   db layer's [ROLLBACK] (or [with_ddl_txn] in [Auto]) handle teardown. *)
let rekey_table_columns tx ~old_name ~new_name ~n_cols =
  let rec loop i =
    if i >= n_cols
    then Lwt.return (Ok ())
    else (
      let old_k = column_key old_name i in
      let new_k = column_key new_name i in
      let%lwt bytes_opt = S.get tx sys_columns_tid old_k in
      match bytes_opt with
      | None ->
        Lwt.return
          (Error
             (Printf.sprintf "catalog corrupt: column %d missing for table %s" i old_name))
      | Some bytes ->
        let%lwt () = S.del tx sys_columns_tid old_k in
        let%lwt () = S.put tx sys_columns_tid new_k bytes in
        loop (i + 1))
  in
  loop 0
;;

(* [~txn] (#282): [Some] when the surrounding rename is borrowing an ambient
   explicit transaction; the entry is committed (and the cache undo registered)
   only on the borrowed path, otherwise the autocommit caller commits its own
   writer txn.  The sys_indexes scan runs THROUGH [tx] (read-your-own-writes) so
   an index created earlier in the same transaction is remapped too, not just
   pre-txn indexes. *)
let finish_rename t tx ~txn ~old_name ~new_name ~meta =
  (* Re-write sys_indexes entries that reference old_name, reading through the
     active txn so uncommitted in-txn index entries are also caught. *)
  let%lwt cur = S.cursor_open tx sys_indexes_tid in
  let _sr = S.cursor_first cur in
  let idx_updates = ref [] in
  let rec scan_idxs () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      let info = decode_index_value v in
      if String.equal info.idx_table old_name
      then idx_updates := (k, info) :: !idx_updates;
      scan_idxs ()
  in
  scan_idxs ();
  S.cursor_close cur;
  let%lwt () =
    Lwt_list.iter_s
      (fun (k, (info : index_info)) ->
         let new_info = { info with idx_table = new_name } in
         S.put tx sys_indexes_tid k (encode_index_value new_info))
      !idx_updates
  in
  (* Refresh the mirror entry (keyed by the unchanged tree_id) with the new
     name; the schema shape — hence the fingerprint — is unchanged. *)
  let%lwt () = put_mirror_tx tx { meta with name = new_name } in
  let%lwt () =
    match txn with
    | Some _ -> Lwt.return_unit
    | None -> S.commit tx
  in
  (* Update in-memory cache + index back-references.  Each mutator self-registers
     its own ROLLBACK restore (replayed most-recent-first: index back-refs, then
     the new table removed, then the old table restored — reversing the rename
     exactly).  The fingerprint is unchanged by a rename, so [put_table]'s re-stamp
     is a no-op. *)
  let to_update =
    Schema_cache.fold_indexes
      (fun k v acc -> if String.equal v.idx_table old_name then (k, v) :: acc else acc)
      t.sc
      []
  in
  (match txn with
   | Some _ ->
     Schema_cache.remove_table t.sc ~name:old_name;
     Schema_cache.put_table t.sc ~name:new_name { meta with name = new_name };
     List.iter
       (fun (k, v) -> Schema_cache.put_index t.sc ~name:k { v with idx_table = new_name })
       to_update
   | None ->
     Schema_cache.remove_table_durable t.sc ~name:old_name;
     Schema_cache.put_table_durable t.sc ~name:new_name { meta with name = new_name };
     List.iter
       (fun (k, v) ->
          Schema_cache.put_index_durable t.sc ~name:k { v with idx_table = new_name })
       to_update);
  Lwt.return (Ok ())
;;

let rename_table ?txn t ~old_name ~new_name =
  match Schema_cache.find_table t.sc old_name with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" old_name))
  | Some meta ->
    if Schema_cache.mem_table t.sc new_name
    then Lwt.return (Error (Printf.sprintf "table already exists: %s" new_name))
    else (
      let body tx =
        (* Remove old sys_tables entry *)
        let%lwt () = S.del tx sys_tables_tid (Bytes.of_string old_name) in
        (* Insert new sys_tables entry *)
        let%lwt () =
          S.put tx sys_tables_tid (Bytes.of_string new_name) (encode_table_value meta)
        in
        (* Re-key all column entries; return Error if any entry is missing *)
        let%lwt col_result =
          rekey_table_columns tx ~old_name ~new_name ~n_cols:(List.length meta.columns)
        in
        match col_result with
        | Error msg -> Lwt.return (Error msg)
        | Ok () -> finish_rename t tx ~txn ~old_name ~new_name ~meta
      in
      match txn with
      | Some tx -> body tx
      | None ->
        let%lwt tx = S.rw_begin t.store in
        let%lwt r = body tx in
        (match r with
         | Ok () -> Lwt.return (Ok ()) (* finish_rename committed on the None path *)
         | Error msg ->
           let%lwt () = S.rollback tx in
           Lwt.return (Error msg)))
;;

(* [?txn] (#282): mirrors [add_column].  Renaming a column changes the schema
   fingerprint (it is computed over column names), so the undo restores both the
   prior [table_meta] and its tree-tag stamp.  The corrupt-catalog error path no
   longer rolls a borrowed txn back — it surfaces an Error and leaves teardown to
   the caller. *)
let rename_column ?txn t ~table_name ~old_col ~new_col =
  match Schema_cache.find_table t.sc table_name with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" table_name))
  | Some meta ->
    (match List.find_index (fun c -> String.equal c.Row.name old_col) meta.columns with
     | None -> Lwt.return (Error (Printf.sprintf "column not found: %s" old_col))
     | Some i ->
       let col_k = column_key table_name i in
       let body tx =
         let%lwt bytes_opt = S.get tx sys_columns_tid col_k in
         match bytes_opt with
         | None -> Lwt.return (Error "column entry missing from catalog")
         | Some old_bytes ->
           let old_col_rec = decode_column old_bytes in
           let new_col_rec = { old_col_rec with Row.name = new_col } in
           let%lwt () = S.put tx sys_columns_tid col_k (encode_column new_col_rec) in
           let new_columns =
             List.mapi
               (fun j c -> if j = i then { c with Row.name = new_col } else c)
               meta.columns
           in
           let new_meta = { meta with columns = new_columns } in
           let%lwt () = put_mirror_tx tx new_meta in
           Lwt.return (Ok new_meta)
       in
       let finalize new_meta =
         match txn with
         | Some _ -> Schema_cache.put_table t.sc ~name:table_name new_meta
         | None -> Schema_cache.put_table_durable t.sc ~name:table_name new_meta
       in
       (match txn with
        | Some tx ->
          (match%lwt body tx with
           | Error msg -> Lwt.return (Error msg)
           | Ok new_meta ->
             finalize new_meta;
             Lwt.return (Ok ()))
        | None ->
          let%lwt tx = S.rw_begin t.store in
          (match%lwt body tx with
           | Error msg ->
             let%lwt () = S.rollback tx in
             Lwt.return (Error msg)
           | Ok new_meta ->
             let%lwt () = S.commit tx in
             finalize new_meta;
             Lwt.return (Ok ()))))
;;

(* [?txn] (#282): mirrors [add_column].  Dropping a column changes the schema
   fingerprint, so the undo restores the prior [table_meta] and re-stamps its
   tree-tag.  Only the catalog's _sys_columns re-keying happens here; the
   executor ([alter_drop_column]) is responsible for migrating the row data
   through the same txn. *)
let drop_column ?txn t ~table_name ~col_name =
  match Schema_cache.find_table t.sc table_name with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" table_name))
  | Some meta ->
    let rec find_idx i = function
      | [] -> None
      | (c : Row.column) :: _ when String.equal c.name col_name -> Some i
      | _ :: rest -> find_idx (i + 1) rest
    in
    (match find_idx 0 meta.columns with
     | None -> Lwt.return (Error (Printf.sprintf "column not found: %s" col_name))
     | Some drop_idx ->
       let n_cols = List.length meta.columns in
       let new_columns = List.filteri (fun i _ -> i <> drop_idx) meta.columns in
       let new_meta = { meta with columns = new_columns } in
       let%lwt () =
         borrow_or_autocommit ?txn t.store (fun tx ->
           (* Delete the dropped column's entry *)
           let%lwt () = S.del tx sys_columns_tid (column_key table_name drop_idx) in
           (* Re-key all columns after drop_idx: shift ordinal down by 1 *)
           let%lwt () =
             let rec shift i =
               if i >= n_cols
               then Lwt.return_unit
               else (
                 let old_k = column_key table_name i in
                 let new_k = column_key table_name (i - 1) in
                 let%lwt bytes_opt = S.get tx sys_columns_tid old_k in
                 match bytes_opt with
                 | None -> shift (i + 1)
                 | Some bytes ->
                   let%lwt () = S.del tx sys_columns_tid old_k in
                   let%lwt () = S.put tx sys_columns_tid new_k bytes in
                   shift (i + 1))
             in
             shift (drop_idx + 1)
           in
           put_mirror_tx tx new_meta)
       in
       (match txn with
        | Some _ -> Schema_cache.put_table t.sc ~name:table_name new_meta
        | None -> Schema_cache.put_table_durable t.sc ~name:table_name new_meta);
       Lwt.return (Ok ()))
;;

(* ------------------------------------------------------------------ *)
(* FTS public API                                                       *)
(* ------------------------------------------------------------------ *)

let find_fts (t : t) name = Schema_cache.find_fts t.sc name

let list_fts_tables (t : t) =
  Schema_cache.fold_fts (fun _name meta acc -> meta :: acc) t.sc []
;;

(* [?txn]: as for [create_table] (#269), an active explicit transaction is
   threaded here so the FTS metadata write and both tree-ID allocations
   participate in it and roll back together. *)
let create_fts_table ?txn (t : t) ~name ~columns : fts_table_meta Lwt.t =
  (* NOTE (autocommit path only): tree-ID allocation and metadata write span
     multiple transactions.  A crash between the two next_user_tid calls leaks a
     tree-ID slot (non-fatal; the next create will allocate the next available
     slot). A crash after both allocations but before the sys_fts_tid write leaves
     the name unregistered and the two tree IDs permanently unused. Same pattern
     as create_table.  The in-transaction path is atomic. *)
  match txn with
  | Some tx ->
    let%lwt content_tree = next_user_tid_tx tx in
    let%lwt index_tree = next_user_tid_tx tx in
    let meta =
      { fts_name = name
      ; fts_content_tree = content_tree
      ; fts_index_tree = index_tree
      ; fts_columns = columns
      }
    in
    let%lwt () = S.put tx sys_fts_tid (Bytes.of_string name) (encode_fts_value meta) in
    Schema_cache.put_fts t.sc ~name meta;
    Lwt.return meta
  | None ->
    (* Allocate two new tree IDs: one for content, one for the inverted index *)
    let%lwt content_tree = next_user_tid t in
    let%lwt index_tree = next_user_tid t in
    let meta =
      { fts_name = name
      ; fts_content_tree = content_tree
      ; fts_index_tree = index_tree
      ; fts_columns = columns
      }
    in
    (* Write to sys_fts_tid *)
    let%lwt tx = S.rw_begin t.store in
    let key = Bytes.of_string name in
    let value = encode_fts_value meta in
    let%lwt () = S.put tx sys_fts_tid key value in
    let%lwt () = S.commit tx in
    Schema_cache.put_fts_durable t.sc ~name meta;
    Lwt.return meta
;;

(** Rowid counter for FTS tables stored as a separate key in sys_fts_tid.
    Key format: name ++ "\x00rowid" (the \x00 prefix sorts before printable ASCII). *)
let next_fts_rowid_in_txn (_t : t) ~name (tx : S.rw S.txn) : int64 Lwt.t =
  let rowid_key = Bytes.cat (Bytes.of_string name) sys_fts_rowid_suffix in
  let%lwt cur_opt = S.get tx sys_fts_tid rowid_key in
  let cur =
    match cur_opt with
    | None -> 1L
    | Some b ->
      let n, _ = Varint.decode_int64 b 0 in
      n
  in
  let next = Int64.add cur 1L in
  let nbuf = Buffer.create 8 in
  Varint.encode_int64 nbuf next;
  let%lwt () = S.put tx sys_fts_tid rowid_key (Buffer.to_bytes nbuf) in
  Lwt.return cur
;;

let get_fk_enforcement t = t.fk_enforcement
let set_fk_enforcement t v = t.fk_enforcement <- v
let get_recursive_triggers t = t.recursive_triggers
let set_recursive_triggers t v = t.recursive_triggers <- v
let get_defer_fks_pragma t = t.defer_fks_pragma
let set_defer_fks_pragma t v = t.defer_fks_pragma <- v
let queue_pending_fk_check t check = t.pending_fk_checks <- check :: t.pending_fk_checks

let drain_pending_fk_checks t =
  let pending = List.rev t.pending_fk_checks in
  t.pending_fk_checks <- [];
  pending
;;

let clear_pending_fk_checks t = t.pending_fk_checks <- []
let pending_fk_check_count t = List.length t.pending_fk_checks
let store t = t.store

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
