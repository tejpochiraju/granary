(* Phase 0 in-memory store. See store.mli for the full contract.
   Phase 1 replaces this module with CoW B+-tree on BLOCK. *)

module BytesMap = Map.Make (Bytes)

type ro
type rw

type tree_id = int

type t = {
  trees : (tree_id, Bytes.t BytesMap.t ref) Hashtbl.t;
  rw_mutex : Lwt_mutex.t;
}

type 'a txn =
  | Ro : t -> ro txn
  | Rw : t -> rw txn

type seek_result =
  | Found of bytes
  | Not_found of [`Greater of bytes | `End]

(* Cursor semantics (per .mli):
   After cursor_first or cursor_seek, the cursor is "pre-positioned".
   The FIRST call to cursor_next returns the positioned entry (not the next one).
   Subsequent calls advance and return the next entry.

   Implementation:
   - [remaining]: the suffix of all bindings starting at the current position.
   - [ready]: when true, [remaining]'s head is the current position and
     cursor_next should return it without advancing first. *)
type cursor = {
  all : (bytes * bytes) list;
  mutable remaining : (bytes * bytes) list;
  mutable ready : bool;
}

let create () =
  { trees = Hashtbl.create 16; rw_mutex = Lwt_mutex.create () }

let close _t = Lwt.return_unit

let tree t tid =
  match Hashtbl.find_opt t.trees tid with
  | Some r -> r
  | None ->
    let r = ref BytesMap.empty in
    Hashtbl.add t.trees tid r;
    r

let ro_begin t = Lwt.return (Ro t)

let rw_begin t =
  let%lwt () = Lwt_mutex.lock t.rw_mutex in
  Lwt.return (Rw t)

let commit (Rw t : rw txn) =
  Lwt_mutex.unlock t.rw_mutex;
  Lwt.return_unit

let rollback (Rw t : rw txn) =
  Lwt_mutex.unlock t.rw_mutex;
  Lwt.return_unit

let ro_end (Ro _ : ro txn) = Lwt.return_unit

let store_of : type a. a txn -> t = function
  | Ro s -> s
  | Rw s -> s

let get : type a. a txn -> tree_id -> bytes -> bytes option Lwt.t =
  fun tx tid key ->
    let t = store_of tx in
    Lwt.return (BytesMap.find_opt key !(tree t tid))

let put (Rw t : rw txn) tid key value =
  let r = tree t tid in
  r := BytesMap.add key value !r;
  Lwt.return_unit

let del (Rw t : rw txn) tid key =
  let r = tree t tid in
  r := BytesMap.remove key !r;
  Lwt.return_unit

let cursor_open : type a. a txn -> tree_id -> cursor Lwt.t =
  fun tx tid ->
    let t = store_of tx in
    let entries = BytesMap.bindings !(tree t tid) in
    Lwt.return { all = entries; remaining = []; ready = false }

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
  (* Find the first entry with k >= key *)
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
      (* First call after positioning: return the current entry without advancing *)
      c.ready <- false;
      Some entry
    end else begin
      (* Subsequent calls: advance past the head, return the new head *)
      c.remaining <- rest;
      match rest with
      | [] -> None
      | next :: _ -> Some next
    end

let cursor_value c =
  (* Return the value at the current position without advancing.
     Only valid when ready=true (i.e. after cursor_first/seek, before cursor_next). *)
  match c.remaining with
  | (_, v) :: _ when c.ready -> Some v
  | _ -> None
