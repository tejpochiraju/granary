(** In-memory freelist. Tracks freed pages and when they can be reused.
    A page freed at txn_id T becomes reusable when min_safe_txn_id > T
    (i.e. freed_at_txn_id < min_safe_txn_id).
    Pure — no I/O, no Lwt. *)

type t

(** Pretty-print the freelist's entry count. *)
val pp : Format.formatter -> t -> unit

(** The empty freelist (no freed pages). *)
val empty : t

(** Add a page to the free set.
    [freed_at_txn_id]: the txn_id of the transaction that freed the page. *)
val add : t -> page_id:int32 -> freed_at_txn_id:int64 -> t

(** Pop a reusable page. A page is reusable if freed_at_txn_id < min_safe_txn_id.
    Returns (page_id, updated_t) or None if no reusable page is available.
    Picks the page with the lowest freed_at_txn_id first (oldest freed first). *)
val pop : t -> min_safe_txn_id:int64 -> (int32 * t) option

(** All entries in the freelist — for serialisation to Freelist pages. *)
val to_list : t -> (int32 * int64) list   (* (page_id, freed_at_txn_id) *)

(** Reconstruct from a list (deserialised from Freelist pages). *)
val of_list : (int32 * int64) list -> t

(** Number of entries in the freelist. *)
val size : t -> int

(** Reusable count: number of entries with freed_at_txn_id < min_safe_txn_id. *)
val reusable_count : t -> min_safe_txn_id:int64 -> int
