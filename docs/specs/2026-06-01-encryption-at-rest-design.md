# Encryption at rest (AES-256-GCM, page-level) — design

Issue: #84 · Date: 2026-06-01 · Status: approved

## Summary

Opt-in, page-level **encryption at rest**. Every data page is encrypted before
it reaches the block device and decrypted on read, so the engine above the
storage edge (pager cache, B+-tree, SQL) only ever sees plaintext.

**Encryption is opt-in and OFF by default.** The `?key` argument is optional on
every open entry point; when it is absent the engine behaves exactly as today
(plaintext, no new overhead, no behavioural change). A database becomes
encrypted only when a key is supplied at *creation* time, and encryption is then
a per-database property fixed at creation that cannot be toggled on an existing
file.

This supersedes the issue's original AES-XTS decision: **mirage-crypto provides
no XTS** (verified across every published version, 0.8.0–2.1.0; no other
maintained opam package provides it either). We use **AES-256-GCM**, a native
mirage-crypto mode, which additionally upgrades tamper-detection from the
non-cryptographic page CRC32 to a real authentication tag and gives a clean
key-mismatch error for free.

## Crypto primitive and per-page overhead

- **AES-256-GCM** via `mirage-crypto`. Key = **32 raw bytes**, app-supplied
  (issue decision #2: the storage layer takes key material, not a passphrase; no
  in-engine KDF, no salt in file).
- Per encrypted page we store a **16-byte random nonce + 16-byte GCM tag = 32
  bytes** in the page's reserved tail. An encrypted DB is therefore created with
  `reserved_bytes_per_page = 32` — the existing #95 geometry facility, whose
  doc-comment already earmarks the reserved tail for "a per-page AEAD tag for
  at-rest encryption (#84)".
- The nonce is freshly generated per write via `Mirage_crypto_rng.generate 16`.
  A 128-bit random nonce is collision-safe well past any realistic database
  lifetime (~2^48 writes at ≤2^-32 collision probability), so we never reuse a
  nonce under one key (the one fatal GCM failure mode).
- **AAD = page identity**: `page_id` for the main DB; the frame's `page_id` for
  the WAL. This cryptographically binds each ciphertext to its location so an
  attacker cannot swap whole encrypted blocks between page slots.

### Why GCM + random-stored-nonce (decisions taken)

- **GCM over CTR**: we must store a nonce either way (neither mode is
  deterministic-safe across page rewrites). GCM's extra 16-byte tag buys
  cryptographic integrity and — combined with a plaintext "encrypted" flag in
  the header — turns a wrong/missing key into a clean error instead of garbage.
- **Random nonce stored per page** over a deterministic `(txn_id, page_id)`
  nonce: self-contained (the nonce travels with the ciphertext, no global
  counter to persist or corrupt, no commit-path plumbing), simplest to prove
  correct. Cost: a `mirage-crypto-rng` dependency that the application seeds at
  boot.

## Layering

The pager and everything above it **only ever see plaintext**. Encryption lives
at the two storage edges; the pager is oblivious.

### Main DB — callback wrap

Wrap the `read_page` / `write_page` callbacks at `Store.open_block` /
`open_block_wal`:

- **Pages 0 and 1 (the two header pages) pass through plaintext.** They are read
  by `Header.peek_geometry` *before* any CRC check or decryption, to bootstrap
  the page size; keeping them plaintext is what makes keyless geometry discovery
  and the key-required canary possible. They hold only structural metadata
  (txn_id, root/freelist page, geometry, schema_version), not user data.
- **Pages ≥ 2 are GCM-encrypted/decrypted.** On-disk layout of an encrypted
  page:

  ```
  [ ciphertext: bytes 0 .. page_size-32 ] [ nonce: 16 ] [ tag: 16 ]
  ```

  The encrypted region is exactly the logical page minus the reserved tail. The
  B+-tree already respects `reserved_bytes_per_page` and never writes into the
  tail. At seal time the tail is zero (covered by the page CRC); on decrypt we
  restore the tail to zero before `verify_crc`, so the existing CRC still
  validates the plaintext.

### WAL — inside the frame codec

The WAL cannot be a blind byte-stream wrap: frames are `24 + page_size` bytes at
unaligned offsets. Encryption lives **inside the frame codec** (`wal.ml`),
which is passed an optional cipher at `Wal.open_`:

- Encrypt **only the page payload** of each frame. The **24-byte frame meta
  header (page_id, flags, checksum) stays plaintext** so the keyless recovery
  forward-scan still locates commit batches.
- **Checksum (FNV-1a-64) is computed over the ciphertext**, so a torn/corrupt
  frame is rejected *before* any decrypt attempt.
- An encrypted frame grows by 32 bytes (nonce + tag) relative to a plaintext
  frame; `frame_size_bytes` becomes cipher-aware, and all
  `header_size + i * frame_size` offset math follows from it.
- `read_frame` verifies the checksum, decrypts, and returns the **plaintext**
  page — so the pager's WAL read path sees plaintext, exactly like the main-DB
  path. `append_commit*` receive plaintext pages and encrypt internally.

Each page is thus encrypted independently at each storage surface (main DB and
WAL) with its own random nonce. This matches the issue's "encryption below the
WAL/page layer" intent and keeps the pager free of crypto. (Rejected
alternative: encrypt once above the WAL and store WAL ciphertext verbatim — that
would force the pager to decrypt WAL frames, coupling it to crypto.)

## Header marker, canary, and open-time behavior

The two header pages (0 and 1) stay **plaintext** — they are read before any key
use to bootstrap geometry, and they carry the encryption marker + a key-check
canary. The header page's field area uses bytes 16–67; bytes 68+ are currently
zeroed reserved space, covered by the header CRC. We add (all big-endian /
fixed-width, within the plaintext header page):

```
byte 68  enc_magic : uint32   (0x53454E43 "SENC" when encrypted, else 0)
byte 72  canary_nonce : 16 bytes
byte 88  canary_tag   : 16 bytes
```

- The **canary** is a GCM authentication tag computed over an *empty* message
  with `adata = "sqlocaml-enc-v1"` under the database key and a per-database
  random `canary_nonce`, generated once at creation and preserved verbatim
  across every commit (like geometry/format_version). It stores no plaintext.
- At open, with a key supplied, the engine recomputes the tag from the supplied
  key + stored nonce + fixed adata and compares it to `canary_tag`. This gives a
  **deterministic key check that does not depend on any data page existing** (so
  a freshly-created, empty encrypted DB still detects a wrong key cleanly).
- Open-time matrix:

  | On-disk state | Key supplied? | Result |
  |---|---|---|
  | encrypted (`enc_magic` set) | no  | `Encryption_key_required` (clean error, not `Both_headers_corrupt`) |
  | encrypted | wrong | canary tag mismatch → `Encryption_key_mismatch` |
  | encrypted | right | encryption active |
  | plaintext | yes | `Not_encrypted` (refuse — no silent mis-encryption) |
  | fresh (new file) | yes | create with `reserved=32`, marker + canary written, encryption active |
  | fresh / plaintext | no | plaintext, exactly as today (opt-in: no key ⇒ no encryption) |

- Encryption is **fixed at creation, never toggleable** (issue decision #4),
  enforced by the immutable header marker + geometry.

## New module: `lib/storage/crypto.ml` (+ `.mli`)

Pure, no I/O, Mirage-compatible. Orchestrates GCM + nonce placement over the
library-provided AES core (we do not write our own cipher).

```
type t                                   (* holds the derived GCM key *)
val create : key:string -> (t, [`Bad_key_length]) result

(* main-DB page: encrypts bytes [0 .. len-32] in place, writes nonce+tag tail *)
val encrypt_page : t -> page_id:int64 -> Cstruct.t -> unit
val decrypt_page : t -> page_id:int64 -> Cstruct.t -> (unit, [`Tag_mismatch]) result

(* WAL frame payload variants, AAD = page_id *)
val encrypt_frame : t -> page_id:int64 -> plaintext:Cstruct.t -> Cstruct.t
val decrypt_frame : t -> page_id:int64 -> Cstruct.t -> (Cstruct.t, [`Tag_mismatch]) result

(* key-check canary: tag over an empty message, adata = canary_adata *)
val canary_adata : string                       (* "sqlocaml-enc-v1" *)
val make_canary  : t -> nonce:string -> string  (* returns the 16-byte tag *)
val check_canary : t -> nonce:string -> tag:string -> bool

val tag_len : int      (* 16 *)
val nonce_len : int    (* 16 *)
val overhead : int     (* 32 — reserved bytes an encrypted page needs *)
```

`lib/` depends on `mirage-crypto` + `mirage-crypto-rng`. **Seeding the RNG
happens only in `bin/` and test harnesses** (e.g. `Mirage_crypto_rng_lwt` /
`Mirage_crypto_rng_unix`), never in `lib/`, so `lib/` stays free of `Unix`/
`*-unix` deps per the Mirage-compatibility rule.

Dependency note: installing `mirage-crypto-rng` makes the solver select
`mirage-crypto.1.2.0` (rather than 2.1.0) in the current opam snapshot.
Implementation targets the GCM API of the co-installable version; the Containerfile
pins both. The architecture is independent of which 1.x/2.x lands.

## Testing (strict TDD, repo discipline)

**`crypto.ml` unit (alcotest):**
- round-trip `decrypt (encrypt p) = p`
- tamper any ciphertext/nonce/tag byte → `Tag_mismatch`
- wrong key → `Tag_mismatch`
- two encryptions of the same plaintext produce different ciphertext (nonce freshness)
- **NIST GCM known-answer vectors** to prove correct AES-GCM usage end to end
- `create` rejects a key whose length ≠ 32

**Property (qcheck-alcotest):**
- arbitrary page bytes: round-trip identity
- ciphertext ≠ plaintext for non-trivial pages
- flipping any single byte of `[ciphertext|nonce|tag]` ⇒ decrypt fails

**Integration (alcotest, file-backed via the unix driver in tests):**
- encrypted `open_block`: put → commit → reopen-with-key → get round-trips
- encrypted `open_block_wal`: same across WAL commit, checkpoint, and crash
  recovery (reopen)
- open-time error matrix above (key required / mismatch / not-encrypted)
- **plaintext-leak guard**: after writing known values, scan the raw on-disk
  bytes (and raw WAL bytes) and assert the plaintext values do **not** appear

## Scope notes (documented, not solved here)

- **Metadata leak (accepted):** header fields (root/txn/geometry/schema), and in
  the WAL the page-ids + commit pattern, remain visible. Page *contents* are
  protected; access pattern is not.
- **`copy_to` / hot-copy (#93):** in this model `copy_to` sinks *plaintext* pages
  (the pager sees plaintext); a destination becomes encrypted only if it is
  itself opened with a key. File a follow-up to make hot-copy encryption-aware
  rather than silently widening scope here.
- **Key rotation:** re-encrypting every page is an offline bulk operation; out of
  scope for v1. Note the cost.
- **Mirage deployment** depends on #170 (platform-agnostic block/store split),
  same as the replication family. The Unix file-backed path (and the tests)
  work today.

## Out-of-scope / non-goals

- No KDF, no passphrase handling, no salt in file (app owns key material).
- No on-the-fly enable/disable of encryption for an existing database.
- No re-keying / rotation tooling.
- No change to replication (#92/#172) in this PR; WAL frames being ciphertext is
  a property those features can later exploit, but no replication code is touched.
