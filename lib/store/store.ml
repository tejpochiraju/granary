(* Phase 1 store.  Two backends share the same interface:

   - [Mem] — pure in-memory [BytesMap]-per-tree (the Phase 0 backend).
     Used by [create ()].  No I/O, no size limits, no errors.

   - [Btree] — CoW B+-tree over a Pager over a BLOCK device (Unix file).
     Used by [open_file ~path].  Persists across reopen.  Inherits the
     B+-tree leaf-cell size limits (512-byte keys, 1024-byte values).

   The two are wrapped in a sum type so callers see one [Store.t]. *)

open Lwt.Syntax

module Btree    = Sqlocaml_storage.Btree
module Pager    = Sqlocaml_storage.Pager
module Header   = Sqlocaml_storage.Header
module Freelist = Sqlocaml_storage.Freelist
module Unix_file = Sqlocaml_block.Unix_file
module Varint   = Sqlocaml_encoding.Varint

module BytesMap = Map.Make (Bytes)

type ro
type rw

type tree_id = int

type error =
  | Block_error of string
  | Corruption of string
  | Key_too_large of int
  | Value_too_large of int
  | Header_error of string

let pp_error fmt = function
  | Block_error s     -> Format.fprintf fmt "Block_error(%s)" s
  | Corruption s      -> Format.fprintf fmt "Corruption(%s)" s
  | Key_too_large n   -> Format.fprintf fmt "Key_too_large(%d)" n
  | Value_too_large n -> Format.fprintf fmt "Value_too_large(%d)" n
  | Header_error s    -> Format.fprintf fmt "Header_error(%s)" s

(* ------------------------------------------------------------------ *)
(* Btree-backend internal state                                         *)
(* ------------------------------------------------------------------ *)

(* The meta-tree is stored separately from user trees (it doesn't live
   in the [trees] hashtable).  It tracks the root_page of every tree_id
   created via [get/put/del]; its OWN root_page is what we commit into
   the header. *)

type bt_state = {
  file                 : Unix_file.t;
  pager                : Pager.t;
  mutable meta         : Btree.t;
  trees                : (tree_id, Btree.t) Hashtbl.t;
  mutable current_header : Header.t;
  schema_version : int64;
}

type backend =
  | Mem  of (tree_id, Bytes.t BytesMap.t ref) Hashtbl.t
  | Btree of bt_state

type t = {
  backend  : backend;
  rw_mutex : Lwt_mutex.t;
}

type 'a txn =
  | Ro : t -> ro txn
  | Rw : t -> rw txn

type seek_result =
  | Found of bytes
  | Not_found of [`Greater of bytes | `End]

(* Cursor over either backend.
   For the in-memory backend, the cursor holds an immutable snapshot of
   the bindings as a list (matches Phase 0 semantics).

   For the B+-tree backend we similarly materialise a snapshot (list of
   (k,v) pairs) at cursor_open time.  This is acceptable for Phase 1 and
   makes seek/next semantics identical to the in-memory implementation;
   true streaming cursors come later.

   In both cases [ready] and [remaining] together implement the
   "pre-positioned" semantics from [store.mli]: the first [cursor_next]
   after positioning returns the positioned entry without advancing. *)
type cursor = {
  all : (bytes * bytes) list;
  mutable remaining : (bytes * bytes) list;
  mutable ready : bool;
}

(* ------------------------------------------------------------------ *)
(* Backend helpers — Mem                                                *)
(* ------------------------------------------------------------------ *)

let mem_tree trees tid =
  match Hashtbl.find_opt trees tid with
  | Some r -> r
  | None ->
    let r = ref BytesMap.empty in
    Hashtbl.add trees tid r;
    r

(* ------------------------------------------------------------------ *)
(* Backend helpers — Btree                                              *)
(* ------------------------------------------------------------------ *)

(* tree_id <-> bytes encoding via varint (zigzag, since negative ids are
   reserved for internal use; we don't actually persist negative ids but
   using signed encoding lets us round-trip safely). *)
let encode_tree_id (tid : tree_id) : bytes =
  let buf = Buffer.create 8 in
  Varint.encode_int64 buf (Int64.of_int tid);
  Buffer.to_bytes buf

let encode_root_page (pid : int64) : bytes =
  let buf = Buffer.create 8 in
  Varint.encode_uint64 buf pid;
  Buffer.to_bytes buf

let decode_root_page (b : bytes) : int64 =
  let v, _ = Varint.decode_uint64 b 0 in
  v

let map_btree_err : Btree.error -> error = function
  | Btree.Pager_error (Pager.Block_error s)  -> Block_error s
  | Btree.Pager_error (Pager.Corruption s)   -> Corruption s
  | Btree.Key_too_large n                    -> Key_too_large n
  | Btree.Value_too_large n                  -> Value_too_large n
  | Btree.Tree_corrupt s                     -> Corruption s

(* The B+-tree treats Bytes by [Bytes.compare]; cursor_seek consumes the
   raw bytes; everything is byte-clean. *)

(* Lookup-or-build the Btree handle for a tree_id.  Looks up the
   tree_id's root page in the meta-tree; if absent (new tree), creates a
   fresh empty Btree (root_page = 0L). *)
let bt_get_tree st (tid : tree_id) : (Btree.t, error) result Lwt.t =
  match Hashtbl.find_opt st.trees tid with
  | Some bt -> Lwt.return_ok bt
  | None ->
    let key = encode_tree_id tid in
    let* r = Btree.get st.meta key in
    match r with
    | Error e -> Lwt.return_error (map_btree_err e)
    | Ok None ->
      let bt = Btree.create st.pager ~root_page:0L in
      Hashtbl.replace st.trees tid bt;
      Lwt.return_ok bt
    | Ok (Some v) ->
      let root_page = decode_root_page v in
      let bt = Btree.create st.pager ~root_page in
      Hashtbl.replace st.trees tid bt;
      Lwt.return_ok bt

(* Convert a result with [error] payload to an Lwt-failing version.  The
   public [get/put/del/cursor_open] signatures don't return [result], so
   B+-tree errors are surfaced as Lwt exceptions. *)
let unwrap_error r =
  match r with
  | Ok v -> Lwt.return v
  | Error e -> Lwt.fail_with (Format.asprintf "Store: %a" pp_error e)

(* ------------------------------------------------------------------ *)
(* create / open_file / close                                           *)
(* ------------------------------------------------------------------ *)

let create () : t =
  { backend = Mem (Hashtbl.create 16); rw_mutex = Lwt_mutex.create () }

let map_unix_err (e : Unix_file.error) : error =
  match e with
  | Unix_file.Io s -> Block_error s
  | Unix_file.Out_of_bounds { page_id; n_pages } ->
    Block_error
      (Format.asprintf "out of bounds page_id=%Ld n_pages=%Ld" page_id n_pages)

let map_header_err (e : Header.error) : error =
  match e with
  | Header.Io s -> Header_error s
  | Header.Both_headers_corrupt ->
    Header_error "both header pages corrupt"

(* Build a Pager that delegates to a Unix_file. *)
let pager_of_unix_file (f : Unix_file.t) ~freelist : Pager.t =
  let read_page ~page_id buf =
    let%lwt r = Unix_file.read_page f ~page_id buf in
    match r with
    | Ok () -> Lwt.return_ok ()
    | Error e -> Lwt.return_error (Format.asprintf "%a" Unix_file.pp_error e)
  in
  let write_page ~page_id buf =
    let%lwt r = Unix_file.write_page f ~page_id buf in
    match r with
    | Ok () -> Lwt.return_ok ()
    | Error e -> Lwt.return_error (Format.asprintf "%a" Unix_file.pp_error e)
  in
  let sync () =
    let%lwt r = Unix_file.sync f in
    match r with
    | Ok () -> Lwt.return_ok ()
    | Error e -> Lwt.return_error (Format.asprintf "%a" Unix_file.pp_error e)
  in
  let resize ~n_pages =
    let%lwt r = Unix_file.resize f ~n_pages in
    match r with
    | Ok () -> Lwt.return_ok ()
    | Error e -> Lwt.return_error (Format.asprintf "%a" Unix_file.pp_error e)
  in
  let n_pages = Unix_file.n_pages f in
  Pager.create ~read_page ~write_page ~sync ~resize ~n_pages ~freelist

let open_file ~path : (t, error) result Lwt.t =
  let%lwt fr = Unix_file.open_ ~path in
  match fr with
  | Error e -> Lwt.return_error (map_unix_err e)
  | Ok file ->
    let n_pages = Unix_file.n_pages file in
    if Int64.compare n_pages 0L = 0 then begin
      (* Fresh file — pre-resize to 2 pages so Header.init can write the
         two alternating header pages, then initialise them.  After
         Header.init the live header has txn_id=0, root_page=0, etc.
         We MUST create the pager AFTER the resize so it knows n_pages=2;
         otherwise [Pager.alloc] would re-allocate page 0. *)
      let%lwt rr = Unix_file.resize file ~n_pages:2L in
      match rr with
      | Error e ->
        let%lwt _ = Unix_file.close file in
        Lwt.return_error (map_unix_err e)
      | Ok () ->
      let pager = pager_of_unix_file file ~freelist:Freelist.empty in
      let%lwt ir = Header.init pager in
      match ir with
      | Error e -> Lwt.return_error (map_header_err e)
      | Ok () ->
        let%lwt hr = Header.read_live pager in
        match hr with
        | Error e -> Lwt.return_error (map_header_err e)
        | Ok h ->
          let meta = Btree.create pager ~root_page:0L in
          let st =
            { file; pager; meta;
              trees = Hashtbl.create 16;
              current_header = h;
              schema_version = h.schema_version }
          in
          Lwt.return_ok
            { backend = Btree st; rw_mutex = Lwt_mutex.create () }
    end else begin
      let pager = pager_of_unix_file file ~freelist:Freelist.empty in
      let%lwt hr = Header.read_live pager in
      match hr with
      | Error e -> Lwt.return_error (map_header_err e)
      | Ok h ->
        let meta = Btree.create pager ~root_page:h.root_page in
        let st =
          { file; pager; meta;
            trees = Hashtbl.create 16;
            current_header = h;
            schema_version = h.schema_version }
        in
        Lwt.return_ok
          { backend = Btree st; rw_mutex = Lwt_mutex.create () }
    end

let close (t : t) : unit Lwt.t =
  match t.backend with
  | Mem _ -> Lwt.return_unit
  | Btree st ->
    let%lwt _ = Unix_file.close st.file in
    Lwt.return_unit

(* ------------------------------------------------------------------ *)
(* Transactions                                                         *)
(* ------------------------------------------------------------------ *)

let ro_begin t = Lwt.return (Ro t)

let rw_begin t =
  let* () = Lwt_mutex.lock t.rw_mutex in
  Lwt.return (Rw t)

let ro_end (Ro _ : ro txn) = Lwt.return_unit

(* commit:
   - Mem backend: no I/O, just release the writer lock.
   - Btree backend: flush all currently-open trees' root_pages into the
     meta-tree, then write a new header pointing at the new meta root.

   Note: the Btree.create/put/del API returns a NEW Btree.t after every
   mutation (root_page may have changed).  We update [st.trees] each
   time; here we additionally persist the latest root_page for each
   touched tree into the meta-tree (whose own root we then commit via
   the header alternating-pages protocol). *)
let commit (Rw t : rw txn) : unit Lwt.t =
  (match t.backend with
   | Mem _ -> Lwt.return_unit
   | Btree st ->
     (* Persist every cached tree's root_page into the meta-tree.  We
        iterate over a snapshot of the bindings to avoid mutation during
        iteration. *)
     let bindings =
       Hashtbl.fold (fun tid bt acc -> (tid, bt) :: acc) st.trees []
     in
     let* () =
       Lwt_list.iter_s (fun (tid, bt) ->
         let key = encode_tree_id tid in
         let v   = encode_root_page (Btree.root_page bt) in
         let* r = Btree.put st.meta key v in
         match r with
         | Ok meta' -> st.meta <- meta'; Lwt.return_unit
         | Error e -> Lwt.fail_with
           (Format.asprintf "Store.commit: %a" pp_error (map_btree_err e))
       ) bindings
     in
     let new_state : Header.t =
       { txn_id         = 0L;  (* overwritten by Header.commit *)
         root_page      = Btree.root_page st.meta;
         freelist_page  = 0L;  (* Phase 1: no freelist persistence *)
         n_pages_total  = Pager.n_pages st.pager;
         schema_version = st.schema_version }
     in
     let* r = Header.commit st.pager
                ~prev_header:st.current_header ~new_state
     in
     match r with
     | Ok () ->
       st.current_header <-
         { new_state with
           txn_id = Int64.add st.current_header.txn_id 1L };
       Lwt.return_unit
     | Error e ->
       Lwt.fail_with
         (Format.asprintf "Store.commit: %a" pp_error (map_header_err e)))
  |> fun work ->
  let* () = work in
  Lwt_mutex.unlock t.rw_mutex;
  Lwt.return_unit

(* rollback:
   - Phase 1 Mem: mutations are applied immediately; nothing to undo.
   - Phase 1 Btree: mutations are likewise applied immediately to the
     pager's dirty set.  True rollback would need a separate dirty-page
     stash; for Phase 1 we just drop the cached tree handles (forcing
     the next access to re-read from the meta-tree's committed state)
     and skip the header commit.  Any pages dirtied during this
     transaction remain in the pager cache but will be overwritten /
     ignored on the next commit's header swap.  This is a simplification
     — full MVCC arrives in Phase 3. *)
let rollback (Rw t : rw txn) : unit Lwt.t =
  (match t.backend with
   | Mem _ -> ()
   | Btree st ->
     (* Drop the per-tree cache so subsequent reads pick up the
        last-committed roots from the meta-tree.  Note: the meta-tree
        itself may have been mutated during this txn (uncommitted puts
        to it); we revert it to the last-committed root from the
        header. *)
     Hashtbl.clear st.trees;
     st.meta <- Btree.create st.pager ~root_page:st.current_header.root_page);
  Lwt_mutex.unlock t.rw_mutex;
  Lwt.return_unit

(* ------------------------------------------------------------------ *)
(* get / put / del                                                      *)
(* ------------------------------------------------------------------ *)

let store_of : type a. a txn -> t = function
  | Ro s -> s
  | Rw s -> s

let get : type a. a txn -> tree_id -> bytes -> bytes option Lwt.t =
  fun tx tid key ->
    let t = store_of tx in
    match t.backend with
    | Mem trees ->
      Lwt.return (BytesMap.find_opt key !(mem_tree trees tid))
    | Btree st ->
      let* r = bt_get_tree st tid in
      let* bt = unwrap_error r in
      let* g = Btree.get bt key in
      (match g with
       | Ok v -> Lwt.return v
       | Error e ->
         Lwt.fail_with
           (Format.asprintf "Store.get: %a" pp_error (map_btree_err e)))

let put (Rw t : rw txn) tid key value : unit Lwt.t =
  match t.backend with
  | Mem trees ->
    let r = mem_tree trees tid in
    r := BytesMap.add key value !r;
    Lwt.return_unit
  | Btree st ->
    let* r = bt_get_tree st tid in
    let* bt = unwrap_error r in
    let* p = Btree.put bt key value in
    (match p with
     | Ok bt' ->
       Hashtbl.replace st.trees tid bt';
       Lwt.return_unit
     | Error e ->
       Lwt.fail_with
         (Format.asprintf "Store.put: %a" pp_error (map_btree_err e)))

let del (Rw t : rw txn) tid key : unit Lwt.t =
  match t.backend with
  | Mem trees ->
    let r = mem_tree trees tid in
    r := BytesMap.remove key !r;
    Lwt.return_unit
  | Btree st ->
    let* r = bt_get_tree st tid in
    let* bt = unwrap_error r in
    let* d = Btree.del bt key in
    (match d with
     | Ok bt' ->
       Hashtbl.replace st.trees tid bt';
       Lwt.return_unit
     | Error e ->
       Lwt.fail_with
         (Format.asprintf "Store.del: %a" pp_error (map_btree_err e)))

(* ------------------------------------------------------------------ *)
(* Cursors                                                              *)
(* ------------------------------------------------------------------ *)

(* Drain a B+-tree cursor into an in-memory snapshot list.  Phase 1
   cursors are materialised; streaming cursors arrive later. *)
let drain_btree_cursor (c : Btree.cursor) : (bytes * bytes) list Lwt.t =
  let rec loop acc =
    let* r = Btree.cursor_next c in
    match r with
    | Error e ->
      Lwt.fail_with
        (Format.asprintf "Store.cursor: %a" pp_error (map_btree_err e))
    | Ok None -> Lwt.return (List.rev acc)
    | Ok (Some kv) -> loop (kv :: acc)
  in
  loop []

let cursor_open : type a. a txn -> tree_id -> cursor Lwt.t =
  fun tx tid ->
    let t = store_of tx in
    match t.backend with
    | Mem trees ->
      let entries = BytesMap.bindings !(mem_tree trees tid) in
      Lwt.return { all = entries; remaining = []; ready = false }
    | Btree st ->
      let* r = bt_get_tree st tid in
      let* bt = unwrap_error r in
      let* co = Btree.cursor_open bt in
      (match co with
       | Error e ->
         Lwt.fail_with
           (Format.asprintf "Store.cursor_open: %a"
              pp_error (map_btree_err e))
       | Ok c ->
         let* entries = drain_btree_cursor c in
         Btree.cursor_close c;
         Lwt.return { all = entries; remaining = []; ready = false })

let cursor_close _ = ()

let cursor_first c =
  c.remaining <- c.all;
  match c.all with
  | [] ->
    c.ready <- false;
    Not_found `End
  | (k, _) :: _ ->
    c.ready <- true;
    Found k

let cursor_seek c key =
  let rec find = function
    | [] ->
      c.remaining <- [];
      c.ready <- false;
      Not_found `End
    | ((k, _) :: _ as cur) ->
      let cmp = Bytes.compare k key in
      if cmp >= 0 then begin
        c.remaining <- cur;
        c.ready <- true;
        if cmp = 0 then Found k else Not_found (`Greater k)
      end else
        find (List.tl cur)
  in
  find c.all

let cursor_next c =
  match c.remaining with
  | [] -> None
  | entry :: rest ->
    if c.ready then begin
      c.ready <- false;
      Some entry
    end else begin
      c.remaining <- rest;
      match rest with
      | [] -> None
      | next :: _ -> Some next
    end

let cursor_value c =
  match c.remaining with
  | (_, v) :: _ when c.ready -> Some v
  | _ -> None
