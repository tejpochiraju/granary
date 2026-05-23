(** Copy-on-write B+-tree over the {!Pager}.  See {!Btree} interface. *)

(* ------------------------------------------------------------------ *)
(* Constants                                                           *)
(* ------------------------------------------------------------------ *)

let max_key_size   = 512

(* Largest user-supplied value supported.  Values larger than
   [inline_value_threshold] spill to an overflow page chain; the leaf cell
   only stores a 17-byte marker (tag + head_pid + total_size). *)
let inline_value_threshold = 800

(* Hard ceiling on individual values.  The overflow chain itself can hold
   essentially arbitrary sizes — this bound is conservative and keeps a
   single value's chain bounded so allocation latency stays predictable. *)
let max_value_size = 1 lsl 30  (* 1 GiB *)

(* Leaf value tag bytes. *)
let tag_inline   = 0x00
let tag_overflow = 0x01
let overflow_marker_size = 1 + 8 + 8  (* tag + head_pid + total_size *)

(* Transaction ids are now managed by the Pager itself.  The B+-tree
   uses [Pager.get_txn_id] to stamp freed pages and [Pager.alloc] reads
   [alloc_min_safe] internally.  Higher-level commit logic (store.ml /
   header module) is responsible for sequencing via [Pager.set_txn_id]
   and [Pager.set_alloc_min_safe]. *)

(* ------------------------------------------------------------------ *)
(* Types                                                               *)
(* ------------------------------------------------------------------ *)

type t = {
  pager           : Pager.t;
  root_page       : int64;
  snapshot_frames : int option;
  (* When Some n, reads via this handle resolve against WAL frames
     strictly less than n.  When None, the handle is a writer's tree
     and reads consult the writer's dirty hashtable + latest WAL. *)
}

type error =
  | Pager_error of Pager.error
  | Key_too_large of int
  | Value_too_large of int
  | Tree_corrupt of string

let pp_error fmt = function
  | Pager_error e -> Format.fprintf fmt "Pager_error(%a)" Pager.pp_error e
  | Key_too_large n -> Format.fprintf fmt "Key_too_large(%d)" n
  | Value_too_large n -> Format.fprintf fmt "Value_too_large(%d)" n
  | Tree_corrupt s -> Format.fprintf fmt "Tree_corrupt(%s)" s

let create ?snapshot_frames pager ~root_page = { pager; root_page; snapshot_frames }
let root_page t = t.root_page

(* ------------------------------------------------------------------ *)
(* Lwt helpers                                                          *)
(* ------------------------------------------------------------------ *)

let ( let* ) = Lwt.bind
let return_ok x = Lwt.return (Ok x)
let return_error e = Lwt.return (Error e)

let bind_pager r f =
  match r with
  | Error e -> return_error (Pager_error e)
  | Ok v -> f v

(* ------------------------------------------------------------------ *)
(* Page-id <-> int32 conversion                                         *)
(* ------------------------------------------------------------------ *)

(* On-disk fields ([right_page], [left_child]) are uint32.  We assume page
   ids fit in 32 bits for Phase 1.  Conversion preserves bit pattern. *)
let int32_of_page_id (id : int64) : int32 = Int64.to_int32 id
let page_id_of_int32 (id : int32) : int64 = Int64.logand 0xFFFFFFFFL (Int64.of_int32 id)

(* ------------------------------------------------------------------ *)
(* Overflow page chains                                                 *)
(* ------------------------------------------------------------------ *)

(* Encode the leaf marker for an overflow chain.
   Layout: [tag=0x01][head_pid: u64 BE][total_size: u64 BE]  (17 bytes) *)
let encode_overflow_marker ~head_pid ~total_size : bytes =
  let b = Bytes.create overflow_marker_size in
  Bytes.set_uint8 b 0 tag_overflow;
  Bytes.set_int64_be b 1 head_pid;
  Bytes.set_int64_be b 9 (Int64.of_int total_size);
  b

(* Wrap an inline value with the inline tag byte. *)
let wrap_inline_value (v : bytes) : bytes =
  let n = Bytes.length v in
  let out = Bytes.create (n + 1) in
  Bytes.set_uint8 out 0 tag_inline;
  Bytes.blit v 0 out 1 n;
  out

(* Allocate an overflow chain that stores [value], returning the head page
   id and the total payload size.  Each chain page holds at most
   [Page.max_overflow_payload_bytes] payload bytes; the last page has
   next_pid = 0. *)
let write_overflow_chain pager (value : bytes) :
  (int64 * int, error) result Lwt.t =
  let total = Bytes.length value in
  let chunk = Page.max_overflow_payload_bytes in
  (* Number of pages needed (at least one even for empty values, though we
     never spill empties). *)
  let n_pages = max 1 ((total + chunk - 1) / chunk) in
  (* Allocate all page ids up front so we can chain them. *)
  let rec alloc_n n acc =
    if n = 0 then return_ok (List.rev acc)
    else
      let* r = Pager.alloc pager in
      bind_pager r (fun pid -> alloc_n (n - 1) (pid :: acc))
  in
  let* allocs = alloc_n n_pages [] in
  match allocs with
  | Error e -> return_error e
  | Ok pids ->
    (* Write each page, chained to the next. *)
    let rec write_chain idx pids' offset =
      match pids' with
      | [] -> return_ok ()
      | pid :: rest ->
        let next_pid = match rest with
          | [] -> 0l
          | p :: _ -> int32_of_page_id p
        in
        let remaining = total - offset in
        let payload_len = min chunk remaining in
        let buf = Cstruct.create Page.page_size in
        Page.write_overflow buf ~next_pid
          ~payload:value ~payload_off:offset ~payload_len;
        Page.seal buf;
        Pager.write pager pid buf;
        write_chain (idx + 1) rest (offset + payload_len)
    in
    let* w = write_chain 0 pids 0 in
    match w with
    | Error e -> return_error e
    | Ok () ->
      let head_pid = List.hd pids in
      return_ok (head_pid, total)

(* Read an overflow chain back into a single bytes buffer. *)
let read_overflow_chain ?snapshot_frames pager ~head_pid ~total_size :
  (bytes, error) result Lwt.t =
  let out = Bytes.create total_size in
  let rec loop pid offset =
    if Int64.equal pid 0L then
      if offset = total_size then return_ok out
      else return_error (Tree_corrupt
        (Printf.sprintf "overflow chain short: got %d of %d bytes"
           offset total_size))
    else
      let* r = Pager.read ?snapshot_frames pager pid in
      bind_pager r (fun buf ->
        let common = Page.read_common buf in
        if common.kind <> Page.Overflow then
          return_error (Tree_corrupt
            "overflow chain points to non-overflow page")
        else begin
          let payload_len = Page.overflow_payload_len buf in
          let remaining = total_size - offset in
          if payload_len > remaining then
            return_error (Tree_corrupt
              (Printf.sprintf "overflow chain page payload %d exceeds remaining %d"
                 payload_len remaining))
          else begin
            Cstruct.blit_to_bytes buf
              (Page.data_offset + 2) out offset payload_len;
            let next_pid = page_id_of_int32 common.right_page in
            loop next_pid (offset + payload_len)
          end
        end)
  in
  loop head_pid 0

(* Free every page in an overflow chain starting at [head_pid].
   Stamps each freed page with the current txn_id. *)
let free_overflow_chain pager ~head_pid : (unit, error) result Lwt.t =
  let rec loop pid =
    if Int64.equal pid 0L then return_ok ()
    else
      let* r = Pager.read pager pid in
      bind_pager r (fun buf ->
        let common = Page.read_common buf in
        if common.kind <> Page.Overflow then
          (* Defensive: don't free non-overflow pages. *)
          return_ok ()
        else begin
          let next_pid = page_id_of_int32 common.right_page in
          Pager.free pager ~page_id:pid
            ~freed_at_txn_id:(Pager.get_txn_id pager);
          loop next_pid
        end)
  in
  loop head_pid

(* Decode a stored leaf value: returns the user-visible value.
   Inline values strip the leading [0x00] tag; overflow markers follow
   the chain. *)
let decode_leaf_value ?snapshot_frames pager (stored : bytes) :
  (bytes, error) result Lwt.t =
  let n = Bytes.length stored in
  if n = 0 then return_ok stored
  else
    let tag = Bytes.get_uint8 stored 0 in
    if tag = tag_inline then begin
      let out = Bytes.create (n - 1) in
      Bytes.blit stored 1 out 0 (n - 1);
      return_ok out
    end
    else if tag = tag_overflow then begin
      if n <> overflow_marker_size then
        return_error (Tree_corrupt
          (Printf.sprintf "overflow marker size %d (expected %d)"
             n overflow_marker_size))
      else
        let head_pid = Bytes.get_int64_be stored 1 in
        let total_size = Int64.to_int (Bytes.get_int64_be stored 9) in
        read_overflow_chain ?snapshot_frames pager ~head_pid ~total_size
    end
    else
      return_error (Tree_corrupt
        (Printf.sprintf "unknown leaf-value tag 0x%02x" tag))

(* If [stored] is an overflow marker, free its chain.  Inline values are
   no-ops. *)
let maybe_free_overflow_of pager (stored : bytes) :
  (unit, error) result Lwt.t =
  let n = Bytes.length stored in
  if n = 0 then return_ok ()
  else
    let tag = Bytes.get_uint8 stored 0 in
    if tag <> tag_overflow then return_ok ()
    else if n <> overflow_marker_size then return_ok ()
    else
      let head_pid = Bytes.get_int64_be stored 1 in
      free_overflow_chain pager ~head_pid

(* ------------------------------------------------------------------ *)
(* Reading / decoding a page                                            *)
(* ------------------------------------------------------------------ *)

(* Decode all leaf entries of a leaf page.
   Returns (entries, end_offset) where end_offset is the byte offset just
   past the last entry — i.e. the current data size. *)
let decode_leaf_entries buf (common : Page.common) :
  (Page.leaf_entry list * int) =
  let rec loop offset i acc =
    if i >= common.n_keys then (List.rev acc, offset)
    else
      match Page.leaf_entry_at buf ~offset with
      | `End -> (List.rev acc, offset)
      | `Entry e -> loop e.next_offset (i + 1) (e :: acc)
  in
  loop Page.data_offset 0 []

(* Decode all branch entries of a branch page. *)
let decode_branch_entries buf (common : Page.common) :
  (Page.branch_entry list * int) =
  let rec loop offset i acc =
    if i >= common.n_keys then (List.rev acc, offset)
    else
      match Page.branch_entry_at buf ~offset with
      | `End -> (List.rev acc, offset)
      | `Entry e -> loop e.next_offset (i + 1) (e :: acc)
  in
  loop Page.data_offset 0 []

(* ------------------------------------------------------------------ *)
(* Encoding pages                                                       *)
(* ------------------------------------------------------------------ *)

(* Build a fresh leaf page from a list of (key, value) entries, with the
   given [right_page] (next-leaf pointer).
   Writes the page to the pager under [page_id] and returns unit (or error). *)
let build_and_write_leaf pager ~page_id ~entries ~right_page :
  (unit, error) result Lwt.t =
  let buf = Cstruct.create Page.page_size in
  Cstruct.memset buf 0;
  let _final_offset =
    List.fold_left
      (fun off (k, v) ->
         Page.leaf_append_entry buf ~offset:off ~key:k ~value:v)
      Page.data_offset entries
  in
  let common : Page.common = {
    kind = Page.Leaf;
    flags = 0;
    n_keys = List.length entries;
    right_page = int32_of_page_id right_page;
    crc32 = 0l;
  } in
  Page.write_common buf common;
  Page.seal buf;
  Pager.write pager page_id buf;
  return_ok ()

(* Build a fresh branch page from a list of (key, left_child) entries and a
   rightmost child page id.  Writes the page to the pager. *)
let build_and_write_branch pager ~page_id ~entries ~right_page :
  (unit, error) result Lwt.t =
  let buf = Cstruct.create Page.page_size in
  Cstruct.memset buf 0;
  let _final_offset =
    List.fold_left
      (fun off (k, lc) ->
         Page.branch_append_entry buf ~offset:off ~key:k
           ~left_child:(int32_of_page_id lc))
      Page.data_offset entries
  in
  let common : Page.common = {
    kind = Page.Branch;
    flags = 0;
    n_keys = List.length entries;
    right_page = int32_of_page_id right_page;
    crc32 = 0l;
  } in
  Page.write_common buf common;
  Page.seal buf;
  Pager.write pager page_id buf;
  return_ok ()

(* ------------------------------------------------------------------ *)
(* Size calculations                                                    *)
(* ------------------------------------------------------------------ *)

let leaf_entry_size key value = 2 + Bytes.length key + 2 + Bytes.length value
let branch_entry_size key = 2 + Bytes.length key + 4

let leaf_entries_total_size entries =
  List.fold_left (fun acc (k, v) -> acc + leaf_entry_size k v) 0 entries

let branch_entries_total_size entries =
  List.fold_left (fun acc (k, _) -> acc + branch_entry_size k) 0 entries

(* ------------------------------------------------------------------ *)
(* Tree traversal helpers                                               *)
(* ------------------------------------------------------------------ *)

(* Pick the child page-id of a branch page that should be followed for [key].
   Returns the page-id (int64).  Walks entries in order. *)
let pick_branch_child (branch_entries : Page.branch_entry list)
    (common : Page.common) (key : bytes) : int64 =
  let rec loop = function
    | [] -> page_id_of_int32 common.right_page
    | (e : Page.branch_entry) :: rest ->
      if Bytes.compare key e.key < 0 then
        page_id_of_int32 e.left_child
      else
        loop rest
  in
  loop branch_entries

(* ------------------------------------------------------------------ *)
(* GET                                                                  *)
(* ------------------------------------------------------------------ *)

let get t key : (bytes option, error) result Lwt.t =
  if Int64.compare t.root_page 0L = 0 then return_ok None
  else
    let rec descend page_id =
      let* r = Pager.read ?snapshot_frames:t.snapshot_frames t.pager page_id in
      bind_pager r (fun buf ->
          let common = Page.read_common buf in
          match common.kind with
          | Page.Leaf ->
            let (entries, _) = decode_leaf_entries buf common in
            let rec lookup = function
              | [] -> return_ok None
              | (e : Page.leaf_entry) :: rest ->
                let c = Bytes.compare key e.key in
                if c = 0 then
                  let* dv = decode_leaf_value ?snapshot_frames:t.snapshot_frames t.pager e.value in
                  (match dv with
                   | Ok v -> return_ok (Some v)
                   | Error e -> return_error e)
                else if c < 0 then return_ok None
                else lookup rest
            in
            lookup entries
          | Page.Branch ->
            let (entries, _) = decode_branch_entries buf common in
            let child = pick_branch_child entries common key in
            descend child
          | _ -> return_error (Tree_corrupt "non-tree page in tree"))
    in
    descend t.root_page

(* Return the raw stored bytes for [key] (still tagged) without decoding
   overflow chains.  Used by [put] / [del] to detect and free an existing
   overflow chain before overwriting it. *)
let get_raw t key : (bytes option, error) result Lwt.t =
  if Int64.compare t.root_page 0L = 0 then return_ok None
  else
    let rec descend page_id =
      let* r = Pager.read ?snapshot_frames:t.snapshot_frames t.pager page_id in
      bind_pager r (fun buf ->
          let common = Page.read_common buf in
          match common.kind with
          | Page.Leaf ->
            let (entries, _) = decode_leaf_entries buf common in
            let rec lookup = function
              | [] -> return_ok None
              | (e : Page.leaf_entry) :: rest ->
                let c = Bytes.compare key e.key in
                if c = 0 then return_ok (Some e.value)
                else if c < 0 then return_ok None
                else lookup rest
            in
            lookup entries
          | Page.Branch ->
            let (entries, _) = decode_branch_entries buf common in
            let child = pick_branch_child entries common key in
            descend child
          | _ -> return_error (Tree_corrupt "non-tree page in tree"))
    in
    descend t.root_page

(* ------------------------------------------------------------------ *)
(* Path-stack record for PUT/DEL                                        *)
(* ------------------------------------------------------------------ *)

(* When descending for a mutation we record each branch page visited and the
   list of decoded entries (so we don't have to re-read), plus the index of
   the child pointer we followed.

   [child_idx] semantics:
     - 0 .. n_keys - 1 → followed [left_child] of branch_entries.(child_idx)
     - n_keys          → followed [right_page]

   This lets us reconstruct the branch with one pointer changed. *)
type path_step = {
  page_id        : int64;
  branch_entries : Page.branch_entry list;
  right_page     : int64;
  child_idx      : int;
}

(* Compute child_idx and child page id for a branch and key. *)
let pick_branch_child_with_idx
    (branch_entries : Page.branch_entry list)
    (common : Page.common) (key : bytes) : int * int64 =
  let rec loop i = function
    | [] -> (i, page_id_of_int32 common.right_page)
    | (e : Page.branch_entry) :: rest ->
      if Bytes.compare key e.key < 0 then
        (i, page_id_of_int32 e.left_child)
      else
        loop (i + 1) rest
  in
  loop 0 branch_entries

(* Walk from root to the leaf containing [key], recording the branch path.
   Returns (path, leaf_page_id).  Path is ordered ROOT → ... → parent-of-leaf. *)
let find_leaf t key : (path_step list * int64, error) result Lwt.t =
  let rec loop path page_id =
    let* r = Pager.read ?snapshot_frames:t.snapshot_frames t.pager page_id in
    bind_pager r (fun buf ->
        let common = Page.read_common buf in
        match common.kind with
        | Page.Leaf -> return_ok (List.rev path, page_id)
        | Page.Branch ->
          let (entries, _) = decode_branch_entries buf common in
          let right_page = page_id_of_int32 common.right_page in
          let (idx, child) = pick_branch_child_with_idx entries common key in
          let step = {
            page_id;
            branch_entries = entries;
            right_page;
            child_idx = idx;
          } in
          loop (step :: path) child
        | _ -> return_error (Tree_corrupt "non-tree page in tree"))
  in
  loop [] t.root_page

(* ------------------------------------------------------------------ *)
(* PUT — leaf manipulation                                              *)
(* ------------------------------------------------------------------ *)

(* Insert or replace (key, value) in a sorted list of leaf-entry tuples.
   Returns the new list. *)
let leaf_insert_or_replace entries key value : (bytes * bytes) list =
  let rec loop acc = function
    | [] -> List.rev_append acc [(key, value)]
    | ((k, _) as hd) :: rest ->
      let c = Bytes.compare key k in
      if c = 0 then List.rev_append acc ((key, value) :: rest)
      else if c < 0 then List.rev_append acc ((key, value) :: hd :: rest)
      else loop (hd :: acc) rest
  in
  loop [] entries

(* Split a list at index [n] (n elements in the first part). *)
let split_at_idx n xs =
  let rec loop i acc = function
    | [] -> (List.rev acc, [])
    | xs when i = 0 -> (List.rev acc, xs)
    | x :: rest -> loop (i - 1) (x :: acc) rest
  in
  loop n [] xs

(* Choose the split point for a leaf so that left half has <= half the bytes.
   Returns the count of entries in the left half (at least 1). *)
let leaf_split_count (entries : (bytes * bytes) list) : int =
  let total = leaf_entries_total_size entries in
  let target = total / 2 in
  let rec loop i acc = function
    | [] -> max 1 i
    | (k, v) :: rest ->
      let sz = leaf_entry_size k v in
      if acc + sz > target && i >= 1 then i
      else loop (i + 1) (acc + sz) rest
  in
  let n = loop 0 0 entries in
  let total_count = List.length entries in
  (* Ensure both halves are non-empty. *)
  if n >= total_count then total_count - 1
  else if n < 1 then 1
  else n

let branch_split_count (entries : (bytes * 'a) list) : int =
  let total =
    List.fold_left (fun acc (k, _) -> acc + branch_entry_size k) 0 entries
  in
  let target = total / 2 in
  let rec loop i acc = function
    | [] -> max 1 i
    | (k, _) :: rest ->
      let sz = branch_entry_size k in
      if acc + sz > target && i >= 1 then i
      else loop (i + 1) (acc + sz) rest
  in
  let n = loop 0 0 entries in
  let total_count = List.length entries in
  if n >= total_count then total_count - 1
  else if n < 1 then 1
  else n

(* Replace the i-th child pointer of a branch with [new_child].
   [child_idx = length entries] means right_page.  Returns (entries, right_page). *)
let replace_branch_child
    (entries : Page.branch_entry list)
    (right_page : int64)
    (child_idx : int) (new_child : int64) :
  (bytes * int64) list * int64 =
  let n = List.length entries in
  let mapped =
    List.map (fun (e : Page.branch_entry) ->
        (e.key, page_id_of_int32 e.left_child))
      entries
  in
  if child_idx >= n then
    (mapped, new_child)
  else
    let new_entries =
      List.mapi (fun i (k, c) ->
          if i = child_idx then (k, new_child) else (k, c))
        mapped
    in
    (new_entries, right_page)

(* Replace the i-th child of a branch and ALSO promote a split key, expanding
   one child pointer into two pointers separated by [split_key].

   I.e. if originally we have entries=[(k0, c0); (k1, c1)] right_page=r and
   we split child at idx 1 (c1) into (left_new, right_new) with split key sk,
   the result is entries=[(k0, c0); (sk, left_new)] right_page' (case
   depending on idx). *)
let split_branch_child
    (entries : Page.branch_entry list)
    (right_page : int64)
    (child_idx : int)
    (left_new : int64) (split_key : bytes) (right_new : int64) :
  (bytes * int64) list * int64 =
  let n = List.length entries in
  let mapped =
    List.map (fun (e : Page.branch_entry) ->
        (e.key, page_id_of_int32 e.left_child))
      entries
  in
  if child_idx = n then begin
    (* Followed right_page.  Replace the implicit "right" with
       [..., (split_key, left_new)], right_page' = right_new. *)
    (mapped @ [(split_key, left_new)], right_new)
  end else begin
    (* Followed entries.(child_idx).  Replace that entry (k_i, c_i) with
       (split_key, left_new); insert (k_i, right_new) AFTER it.  Wait —
       order matters.

       Original: ..., (k_{i-1}, c_{i-1}), (k_i, c_i), (k_{i+1}, c_{i+1}), ...
                 right_page
       c_i was the page we descended into and it split into left_new (lower)
       and right_new (higher) with separator split_key.
       The new branch entries:
         ..., (k_{i-1}, c_{i-1}), (split_key, left_new), (k_i, right_new),
         (k_{i+1}, c_{i+1}), ...
       right_page unchanged. *)
    let before, after = split_at_idx child_idx mapped in
    (* after = (k_i, c_i) :: rest *)
    match after with
    | [] -> assert false
    | (k_i, _c_i) :: rest ->
      let new_entries =
        before @ [(split_key, left_new); (k_i, right_new)] @ rest
      in
      (new_entries, right_page)
  end

(* Result of writing a (possibly split) leaf/branch.  Tells the caller what
   to do with the parent. *)
type write_result =
  | One_page of int64
    (* Single replacement page id. *)
  | Split of int64 * bytes * int64
    (* Left page id, split key, right page id. *)

(* Write a list of leaf entries.  If it fits in one page, return One_page;
   else split into two and return Split.  [right_page] is the chain pointer
   for the rightmost resulting leaf page. *)
let write_leaf_maybe_split pager (entries : (bytes * bytes) list) ~right_page :
  (write_result, error) result Lwt.t =
  let total = leaf_entries_total_size entries in
  if total <= Page.max_data_bytes then begin
    let* alloc_r = Pager.alloc pager in
    bind_pager alloc_r (fun new_pid ->
        let* w = build_and_write_leaf pager ~page_id:new_pid ~entries ~right_page in
        match w with
        | Error e -> return_error e
        | Ok () -> return_ok (One_page new_pid))
  end else begin
    let n_left = leaf_split_count entries in
    let left_entries, right_entries = split_at_idx n_left entries in
    (* split_key = first key of right half *)
    match right_entries with
    | [] -> return_error (Tree_corrupt "leaf split with empty right half")
    | (split_key, _) :: _ ->
      let* alloc_r1 = Pager.alloc pager in
      bind_pager alloc_r1 (fun right_pid ->
          let* alloc_r2 = Pager.alloc pager in
          bind_pager alloc_r2 (fun left_pid ->
              (* Build right first (next-leaf = original right_page). *)
              let* w1 = build_and_write_leaf pager ~page_id:right_pid
                  ~entries:right_entries ~right_page in
              match w1 with
              | Error e -> return_error e
              | Ok () ->
                (* Build left (next-leaf = right_pid). *)
                let* w2 = build_and_write_leaf pager ~page_id:left_pid
                    ~entries:left_entries ~right_page:right_pid in
                match w2 with
                | Error e -> return_error e
                | Ok () -> return_ok (Split (left_pid, split_key, right_pid))))
  end

(* Same for branches.  [right_page] is the rightmost child page-id. *)
let write_branch_maybe_split pager
    (entries : (bytes * int64) list) ~right_page :
  (write_result, error) result Lwt.t =
  let total = branch_entries_total_size entries in
  (* Branches need at least an 8-byte head (right_page already in common) so
     [max_data_bytes] suffices. *)
  if total <= Page.max_data_bytes then begin
    let* alloc_r = Pager.alloc pager in
    bind_pager alloc_r (fun new_pid ->
        let* w = build_and_write_branch pager ~page_id:new_pid ~entries ~right_page in
        match w with
        | Error e -> return_error e
        | Ok () -> return_ok (One_page new_pid))
  end else begin
    let n_left = branch_split_count entries in
    (* Middle entry gets promoted; left half is before, right half is after. *)
    let left_entries, mid_and_right = split_at_idx n_left entries in
    match mid_and_right with
    | [] -> return_error (Tree_corrupt "branch split: empty right half")
    | (mid_key, mid_child) :: right_entries ->
      (* mid_child is the left_child of the middle entry.  After the split it
         becomes the rightmost child of the LEFT branch.  mid_key is promoted
         to the parent.  right_entries plus right_page form the right
         branch (with right_page = right_page). *)
      let* alloc_r1 = Pager.alloc pager in
      bind_pager alloc_r1 (fun right_pid ->
          let* alloc_r2 = Pager.alloc pager in
          bind_pager alloc_r2 (fun left_pid ->
              let* w1 = build_and_write_branch pager ~page_id:right_pid
                  ~entries:right_entries ~right_page in
              match w1 with
              | Error e -> return_error e
              | Ok () ->
                let* w2 = build_and_write_branch pager ~page_id:left_pid
                    ~entries:left_entries ~right_page:mid_child in
                match w2 with
                | Error e -> return_error e
                | Ok () -> return_ok (Split (left_pid, mid_key, right_pid))))
  end

(* Propagate a [write_result] for a child up through the recorded path,
   producing a final write_result for the root.  As we go, free each old
   branch page. *)
let rec propagate_up pager (path : path_step list)
    (child_result : write_result) :
  (write_result, error) result Lwt.t =
  match path with
  | [] -> return_ok child_result
  | step :: rest ->
    let new_entries, new_right_page =
      match child_result with
      | One_page new_child ->
        replace_branch_child step.branch_entries step.right_page
          step.child_idx new_child
      | Split (left_new, split_key, right_new) ->
        split_branch_child step.branch_entries step.right_page
          step.child_idx left_new split_key right_new
    in
    (* Free the old branch page.  Stamp it with the pager's current_txn_id.
       Pager reuses a freed page only when alloc_min_safe > freed_at, so a page
       freed within this transaction will not be immediately recycled — safe. *)
    Pager.free pager ~page_id:step.page_id ~freed_at_txn_id:(Pager.get_txn_id pager);
    let* w = write_branch_maybe_split pager new_entries
        ~right_page:new_right_page in
    match w with
    | Error e -> return_error e
    | Ok wr -> propagate_up pager rest wr

(* ------------------------------------------------------------------ *)
(* PUT                                                                  *)
(* ------------------------------------------------------------------ *)

(* Wrap [value] for storage in a leaf cell.  Small values are tag-prefixed
   inline; large values spill to an overflow page chain and the leaf cell
   stores a 17-byte marker. *)
let prepare_stored_value pager (value : bytes) :
  (bytes, error) result Lwt.t =
  if Bytes.length value <= inline_value_threshold then
    return_ok (wrap_inline_value value)
  else
    let* r = write_overflow_chain pager value in
    match r with
    | Error e -> return_error e
    | Ok (head_pid, total_size) ->
      return_ok (encode_overflow_marker ~head_pid ~total_size)

let put t key value : (t, error) result Lwt.t =
  let key_len = Bytes.length key in
  let val_len = Bytes.length value in
  if key_len > max_key_size then return_error (Key_too_large key_len)
  else if val_len > max_value_size then return_error (Value_too_large val_len)
  else
    (* Before installing the new value, free any existing overflow chain
       under [key].  Doing this BEFORE allocating the new chain keeps the
       freelist available for reuse where possible. *)
    let* existing_r =
      if Int64.compare t.root_page 0L = 0 then return_ok None
      else get_raw t key
    in
    match existing_r with
    | Error e -> return_error e
    | Ok existing_opt ->
      let* free_r = match existing_opt with
        | None -> return_ok ()
        | Some stored -> maybe_free_overflow_of t.pager stored
      in
      match free_r with
      | Error e -> return_error e
      | Ok () ->
        let* prep_r = prepare_stored_value t.pager value in
        match prep_r with
        | Error e -> return_error e
        | Ok stored_value ->
          if Int64.compare t.root_page 0L = 0 then begin
            (* Empty tree → create a single leaf page. *)
            let* alloc_r = Pager.alloc t.pager in
            bind_pager alloc_r (fun new_pid ->
                let* w = build_and_write_leaf t.pager ~page_id:new_pid
                    ~entries:[(key, stored_value)] ~right_page:0L in
                match w with
                | Error e -> return_error e
                | Ok () -> return_ok { t with root_page = new_pid })
          end else begin
            let* path_r = find_leaf t key in
            match path_r with
            | Error e -> return_error e
            | Ok (path, leaf_pid) ->
              let* leaf_r = Pager.read ?snapshot_frames:t.snapshot_frames t.pager leaf_pid in
              bind_pager leaf_r (fun leaf_buf ->
                  let leaf_common = Page.read_common leaf_buf in
                  let (entries, _) = decode_leaf_entries leaf_buf leaf_common in
                  let leaf_right = page_id_of_int32 leaf_common.right_page in
                  let plain_entries =
                    List.map (fun (e : Page.leaf_entry) -> (e.key, e.value)) entries
                  in
                  let new_entries =
                    leaf_insert_or_replace plain_entries key stored_value
                  in
                  Pager.free t.pager ~page_id:leaf_pid
                    ~freed_at_txn_id:(Pager.get_txn_id t.pager);
                  let* w = write_leaf_maybe_split t.pager new_entries
                      ~right_page:leaf_right in
                  match w with
                  | Error e -> return_error e
                  | Ok wr ->
                    let* up = propagate_up t.pager (List.rev path) wr in
                    match up with
                    | Error e -> return_error e
                    | Ok (One_page new_root) ->
                      return_ok { t with root_page = new_root }
                    | Ok (Split (left_pid, split_key, right_pid)) ->
                      let* alloc_r = Pager.alloc t.pager in
                      bind_pager alloc_r (fun new_root_pid ->
                          let* w2 = build_and_write_branch t.pager
                              ~page_id:new_root_pid
                              ~entries:[(split_key, left_pid)]
                              ~right_page:right_pid in
                          match w2 with
                          | Error e -> return_error e
                          | Ok () ->
                            return_ok { t with root_page = new_root_pid }))
          end

(* ------------------------------------------------------------------ *)
(* DEL                                                                  *)
(* ------------------------------------------------------------------ *)

(* Remove the first occurrence of [key] from a sorted leaf entry list.
   Returns (new_list, removed). *)
let leaf_remove key entries =
  let rec loop acc = function
    | [] -> (List.rev acc, false)
    | ((k, _) as hd) :: rest ->
      let c = Bytes.compare key k in
      if c = 0 then (List.rev_append acc rest, true)
      else if c < 0 then (List.rev_append acc (hd :: rest), false)
      else loop (hd :: acc) rest
  in
  loop [] entries

let del t key : (t, error) result Lwt.t =
  let key_len = Bytes.length key in
  if key_len > max_key_size then return_ok t
  else if Int64.compare t.root_page 0L = 0 then return_ok t
  else begin
    let* path_r = find_leaf t key in
    match path_r with
    | Error e -> return_error e
    | Ok (path, leaf_pid) ->
      let* leaf_r = Pager.read ?snapshot_frames:t.snapshot_frames t.pager leaf_pid in
      bind_pager leaf_r (fun leaf_buf ->
          let leaf_common = Page.read_common leaf_buf in
          let (entries, _) = decode_leaf_entries leaf_buf leaf_common in
          let leaf_right = page_id_of_int32 leaf_common.right_page in
          let plain_entries =
            List.map (fun (e : Page.leaf_entry) -> (e.key, e.value)) entries
          in
          (* If we're about to remove an entry whose stored value is an
             overflow marker, free its chain first. *)
          let stored_for_key =
            List.find_map
              (fun (k, v) -> if Bytes.equal k key then Some v else None)
              plain_entries
          in
          let* free_r = match stored_for_key with
            | None -> return_ok ()
            | Some v -> maybe_free_overflow_of t.pager v
          in
          match free_r with
          | Error e -> return_error e
          | Ok () ->
          let (new_entries, removed) = leaf_remove key plain_entries in
          if not removed then return_ok t
          else begin
            (* Special case: root is a single empty leaf → set root to 0L. *)
            if path = [] && new_entries = [] then begin
              Pager.free t.pager ~page_id:leaf_pid ~freed_at_txn_id:(Pager.get_txn_id t.pager);
              return_ok { t with root_page = 0L }
            end else begin
              Pager.free t.pager ~page_id:leaf_pid ~freed_at_txn_id:(Pager.get_txn_id t.pager);
              let* w = write_leaf_maybe_split t.pager new_entries
                  ~right_page:leaf_right in
              match w with
              | Error e -> return_error e
              | Ok wr ->
                let* up = propagate_up t.pager (List.rev path) wr in
                match up with
                | Error e -> return_error e
                | Ok (One_page new_root) ->
                  return_ok { t with root_page = new_root }
                | Ok (Split (left_pid, split_key, right_pid)) ->
                  (* Extremely unlikely: deletion caused a split (entries
                     decreased so no split — but defensive case). *)
                  let* alloc_r = Pager.alloc t.pager in
                  bind_pager alloc_r (fun new_root_pid ->
                      let* w2 = build_and_write_branch t.pager
                          ~page_id:new_root_pid
                          ~entries:[(split_key, left_pid)]
                          ~right_page:right_pid in
                      match w2 with
                      | Error e -> return_error e
                      | Ok () ->
                        return_ok { t with root_page = new_root_pid })
            end
          end)
  end

(* ------------------------------------------------------------------ *)
(* CURSOR                                                               *)
(* ------------------------------------------------------------------ *)

(* The cursor maintains an explicit path from current leaf → root so that we
   can advance to the next leaf without relying on a leaf-chain (which is
   very tricky to maintain under CoW without rewriting the left sibling on
   every split).

   Path is stored DEEPEST-FIRST: head = the frame whose child is the current
   leaf, last = the root frame. *)

(* A frame: a branch page and which child pointer we descended through.
   [child_idx = i] means we followed [branch_entries.(i).left_child].
   [child_idx = List.length branch_entries] means we followed [right_page]. *)
type cursor_frame = {
  cf_branch_entries : Page.branch_entry list;
  cf_right_page     : int64;
  mutable cf_child_idx : int;
}

type cursor = {
  c_pager           : Pager.t;
  c_root            : int64;
  c_snapshot_frames : int option;
  (* Path from current leaf back to root.  Empty when root is a leaf or
     tree is empty. *)
  mutable path      : cursor_frame list;
  mutable leaf_page : int64;
  mutable offset    : int;
  mutable finished  : bool;
}

(* Get the child page-id of frame at its current cf_child_idx. *)
let frame_child (f : cursor_frame) : int64 =
  let n = List.length f.cf_branch_entries in
  if f.cf_child_idx >= n then f.cf_right_page
  else
    let e = List.nth f.cf_branch_entries f.cf_child_idx in
    page_id_of_int32 e.left_child

(* Descend to the leftmost leaf starting from [page_id], returning the new
   frames in DEEPEST-FIRST order (i.e. the frame whose child is the leaf is
   at the head). *)
let leftmost_leaf_with_path ?snapshot_frames pager page_id :
  (cursor_frame list * int64, error) result Lwt.t =
  let rec loop pid acc =
    let* r = Pager.read ?snapshot_frames pager pid in
    bind_pager r (fun buf ->
        let common = Page.read_common buf in
        match common.kind with
        | Page.Leaf -> return_ok (acc, pid)
        | Page.Branch ->
          let (entries, _) = decode_branch_entries buf common in
          let right_page = page_id_of_int32 common.right_page in
          let child =
            match entries with
            | [] -> right_page
            | (e : Page.branch_entry) :: _ -> page_id_of_int32 e.left_child
          in
          let frame = {
            cf_branch_entries = entries;
            cf_right_page = right_page;
            cf_child_idx = 0;
          } in
          (* New frame goes on TOP of acc (acc is deepest-first; we're going
             deeper, so this new one becomes the new head). *)
          loop child (frame :: acc)
        | _ -> return_error (Tree_corrupt "non-tree page in tree"))
  in
  loop page_id []

let cursor_open t : (cursor, error) result Lwt.t =
  if Int64.compare t.root_page 0L = 0 then
    return_ok { c_pager = t.pager; c_root = 0L;
                c_snapshot_frames = t.snapshot_frames;
                path = [];
                leaf_page = 0L; offset = Page.data_offset; finished = true }
  else
    let* r = leftmost_leaf_with_path ?snapshot_frames:t.snapshot_frames t.pager t.root_page in
    match r with
    | Error e -> return_error e
    | Ok (path, leaf_pid) ->
      return_ok { c_pager = t.pager;
                  c_root = t.root_page;
                  c_snapshot_frames = t.snapshot_frames;
                  path;
                  leaf_page = leaf_pid;
                  offset = Page.data_offset;
                  finished = false }

(* Walk back up the path, finding the first frame whose child_idx can be
   advanced (i.e. has a sibling to its right).  Then descend from that
   sibling's leftmost leaf, pushing fresh frames. *)
let rec advance_to_next_leaf c : (bool, error) result Lwt.t =
  match c.path with
  | [] ->
    c.finished <- true;
    return_ok false
  | top :: rest ->
    let n = List.length top.cf_branch_entries in
    if top.cf_child_idx >= n then begin
      (* Already at right_page of this frame — pop and try parent. *)
      c.path <- rest;
      advance_to_next_leaf c
    end else begin
      top.cf_child_idx <- top.cf_child_idx + 1;
      let next_child = frame_child top in
      let* r = leftmost_leaf_with_path ?snapshot_frames:c.c_snapshot_frames c.c_pager next_child in
      match r with
      | Error e -> return_error e
      | Ok (sub_path, leaf_pid) ->
        (* sub_path is deepest-first relative to its subtree.  The deepest
           frame of the whole new path is the head of sub_path (or [top] if
           sub_path is empty, meaning next_child was already a leaf).
           Splice: new_path = sub_path @ [top; rest...] *)
        c.path <- sub_path @ c.path;
        c.leaf_page <- leaf_pid;
        c.offset <- Page.data_offset;
        return_ok true
    end

(* Read the entry at the cursor's current position.  If at end-of-leaf, use
   the path to walk to the next leaf.  Returns the (key, value) and advances
   the cursor past it.

   Also skips over empty leaves (which can result from lazy [del]). *)
let rec cursor_next c : ((bytes * bytes) option, error) result Lwt.t =
  if c.finished then return_ok None
  else begin
    let* r = Pager.read ?snapshot_frames:c.c_snapshot_frames c.c_pager c.leaf_page in
    bind_pager r (fun buf ->
        let common = Page.read_common buf in
        let (_entries, end_offset) = decode_leaf_entries buf common in
        if c.offset >= end_offset then begin
          (* Exhausted this leaf — advance via the path. *)
          let* a = advance_to_next_leaf c in
          match a with
          | Error e -> return_error e
          | Ok false -> return_ok None
          | Ok true -> cursor_next c
        end else begin
          match Page.leaf_entry_at buf ~offset:c.offset with
          | `End ->
            let* a = advance_to_next_leaf c in
            (match a with
             | Error e -> return_error e
             | Ok false -> return_ok None
             | Ok true -> cursor_next c)
          | `Entry e ->
            c.offset <- e.next_offset;
            let* dv = decode_leaf_value ?snapshot_frames:c.c_snapshot_frames c.c_pager e.value in
            (match dv with
             | Ok v -> return_ok (Some (e.key, v))
             | Error err -> return_error err)
        end)
  end

(* Descend from [page_id] toward [key], recording the path deepest-first.
   Returns (path, leaf_pid). *)
let descend_with_path_for_key ?snapshot_frames pager page_id key :
  (cursor_frame list * int64, error) result Lwt.t =
  let rec loop pid acc =
    let* r = Pager.read ?snapshot_frames pager pid in
    bind_pager r (fun buf ->
        let common = Page.read_common buf in
        match common.kind with
        | Page.Leaf -> return_ok (acc, pid)
        | Page.Branch ->
          let (entries, _) = decode_branch_entries buf common in
          let right_page = page_id_of_int32 common.right_page in
          let (idx, child) = pick_branch_child_with_idx entries common key in
          let frame = {
            cf_branch_entries = entries;
            cf_right_page = right_page;
            cf_child_idx = idx;
          } in
          loop child (frame :: acc)
        | _ -> return_error (Tree_corrupt "non-tree page in tree"))
  in
  loop page_id []

(* [cursor_seek]: find the leaf for [key], then within the leaf locate the
   first entry >= key.  If past the end of the leaf, advance to the next
   leaf via path-based traversal.  Cursor invariant after seek: cursor.offset
   points at the entry the first [cursor_next] should return. *)
let cursor_seek c key :
  ([ `Found | `Not_found_after of bytes ], error) result Lwt.t =
  if Int64.compare c.c_root 0L = 0 then begin
    c.finished <- true;
    return_ok (`Not_found_after key)
  end else begin
    let* r = descend_with_path_for_key ?snapshot_frames:c.c_snapshot_frames c.c_pager c.c_root key in
    match r with
    | Error e -> return_error e
    | Ok (path, leaf_pid) ->
      c.path <- path;
      c.leaf_page <- leaf_pid;
      c.offset <- Page.data_offset;
      c.finished <- false;
      let rec scan () =
        if c.finished then return_ok (`Not_found_after key)
        else
          let* rr = Pager.read ?snapshot_frames:c.c_snapshot_frames c.c_pager c.leaf_page in
          bind_pager rr (fun buf ->
              let common = Page.read_common buf in
              let (_entries, end_offset) = decode_leaf_entries buf common in
              if c.offset >= end_offset then begin
                let* a = advance_to_next_leaf c in
                match a with
                | Error e -> return_error e
                | Ok false -> return_ok (`Not_found_after key)
                | Ok true -> scan ()
              end else begin
                match Page.leaf_entry_at buf ~offset:c.offset with
                | `End ->
                  let* a = advance_to_next_leaf c in
                  (match a with
                   | Error e -> return_error e
                   | Ok false -> return_ok (`Not_found_after key)
                   | Ok true -> scan ())
                | `Entry e ->
                  let cmp = Bytes.compare e.key key in
                  if cmp = 0 then
                    return_ok `Found
                  else if cmp > 0 then
                    return_ok (`Not_found_after key)
                  else begin
                    c.offset <- e.next_offset;
                    scan ()
                  end
              end)
      in
      scan ()
  end

let cursor_close _ = ()
