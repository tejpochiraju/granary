(** Per-page AES-256-GCM codec for at-rest encryption (#84).

    Pure: no I/O.  The application supplies a 32-byte raw key; nonces are
    freshly generated per call via {!Mirage_crypto_rng} (the application must
    seed the RNG at boot — [lib/] never seeds, to stay Mirage-clean).

    On-disk encrypted page layout (length [n] = page_size):
    {[ [ ciphertext : n - overhead ] [ nonce : nonce_len ] [ tag : tag_len ] ]}
    The encrypted region is the logical page minus the reserved tail; the
    B+-tree already keeps its data within that region. *)

type t

(** Pretty-printer for {!t} (redacts key material). *)
val pp : Format.formatter -> t -> unit

(** 16 *)
val nonce_len : int

(** 16 *)
val tag_len : int

(** 32 — reserved bytes an encrypted page must carve off its tail *)
val overhead : int

(** Build a cipher from raw key material.  [Error `Bad_key_length] unless the
    key is exactly 32 bytes (AES-256). *)
val create : key:string -> (t, [ `Bad_key_length ]) result

(** Encrypt the page in place: encrypts bytes [0 .. len-overhead), then writes
    the freshly-generated nonce and the auth tag into the reserved tail.  AAD is
    the [page_id], binding the ciphertext to its slot.  [buf] length must be
    > [overhead]. *)
val encrypt_page : t -> page_id:int64 -> Cstruct.t -> unit

(** Total page decrypt invocations since process start.  Observability hook used
    by tests to assert the #246 decrypted-frame cache serves repeated reads with
    zero extra decrypts and that authentication still runs on every fresh
    disk/WAL fill.  Monotonic; never reset. *)
val decrypt_page_count : unit -> int

(** Total WAL-frame decrypt invocations since process start.  See
    {!decrypt_page_count}. *)
val decrypt_frame_count : unit -> int

(** Decrypt the page in place: verifies the tag over [0 .. len-overhead) using
    the tail nonce and [page_id] AAD, writes plaintext back, and zeroes the
    reserved tail (so the page's pre-seal CRC over a zeroed tail still
    verifies).  [Error `Tag_mismatch] on a wrong key or tampering. *)
val decrypt_page : t -> page_id:int64 -> Cstruct.t -> (unit, [ `Tag_mismatch ]) result

(** Encrypt a WAL frame payload.  Returns a fresh buffer of length
    [Cstruct.length plaintext + overhead]: [ ciphertext ][ nonce ][ tag ]. *)
val encrypt_frame : t -> page_id:int64 -> plaintext:Cstruct.t -> Cstruct.t

(** Decrypt a WAL frame payload produced by {!encrypt_frame}.  [payload] length
    must be [plaintext_len + overhead]; returns the [plaintext_len] plaintext or
    [Error `Tag_mismatch]. *)
val decrypt_frame
  :  t
  -> page_id:int64
  -> Cstruct.t
  -> (Cstruct.t, [ `Tag_mismatch ]) result

(** Fixed associated data for the key-check canary. *)
val canary_adata : string

(** [make_canary t ~nonce] returns the [tag_len]-byte GCM tag over an empty
    message with [canary_adata], under [t]'s key and [nonce]. *)
val make_canary : t -> nonce:string -> string

(** [check_canary t ~nonce ~tag] recomputes the canary and reports whether it
    matches [tag] (i.e. the key is correct). *)
val check_canary : t -> nonce:string -> tag:string -> bool
