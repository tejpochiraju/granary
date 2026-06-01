(** Mirage_block.S adapter.  Wraps any [Mirage_block.S] implementation
    so it can be used as a sqlocaml block backend via [Store.open_block].

    Usage:
      module MB = Mirage_backend.Make(Block)   (* Block = mirage-block-unix *)
      let* dev = Block.connect path in
      let* adapter = MB.connect dev in
      let* store = Store.open_block
        ~read_page:(MB.read_page adapter)
        ~write_page:(MB.write_page adapter)
        ~sync:(MB.sync adapter)
        ~resize:(MB.resize adapter)
        ~n_pages:(MB.n_pages adapter)
        ~close:(fun () -> MB.close adapter) in
      ...
*)

module Make (B : Mirage_block.S) : sig
  (** Adapter state: wraps [B.t] with page-granularity access. *)
  type t

  (** [connect ?page_size dev] reads [get_info] from [dev] to determine sector
      size and capacity, then creates an adapter with logical [n_pages = 0].
      [page_size] defaults to 4096 (#95).  Raises [Failure] if [page_size] is
      not a multiple of the device's [sector_size]. *)
  val connect : ?page_size:int -> B.t -> t Lwt.t

  val n_pages : t -> int64

  (** This adapter's page size in bytes (#95). *)
  val page_size : t -> int

  val read_page : t -> page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t
  val write_page : t -> page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t
  val sync : t -> unit -> (unit, string) result Lwt.t

  (** [resize t ~n_pages] validates [n_pages <= capacity] and updates the
      logical count. The physical device is not modified. *)
  val resize : t -> n_pages:int64 -> (unit, string) result Lwt.t

  val close : t -> unit Lwt.t
end
