# Encryption-aware hot-copy (#214) & offline key rotation (#215) — design

Issues: #214, #215 · Date: 2026-06-01 · Status: approved · Follows: #84

## Summary

Two follow-ups to the #84 encryption-at-rest feature, sharing one mechanism:

- **#214 — hot-copy is encryption-unaware.** `Store.copy_to` resolves pages
  through the pager, which sees **plaintext** (decryption happens in the read
  callback wrapper below it). A hot copy of an encrypted DB therefore emits
  plaintext pages to the sink, and the on-disk destination is plaintext unless
  it is independently re-opened with a key. Fix (approach **a**): make
  `copy_to` re-encrypt data pages under the source's own key before they reach
  the sink, so the destination is a faithful, self-contained encrypted DB and
  **no user-data plaintext ever transits the sink**.

- **#215 — key rotation.** Re-encrypting every page under a new key is an
  offline bulk operation. Add `Store.rekey_to` (read every page as plaintext via
  the old key, re-encrypt under a new cipher, rewrite the header canary under the
  new key) plus a crash-safe `Sqlocaml_unix.Store.rotate_key_file`.

Both build on the same page-iteration core: read the consistent plaintext page
image (WAL overlay + main DB under an RO snapshot), then apply a per-page output
transform. #214's transform encrypts under the source key; #215's encrypts under
a new key and rewrites the canary.

## Background: why plaintext, and why a merge copy can't ship ciphertext

The cipher lives only inside the read/write callback closures installed by
`wrap_callbacks` at open; `bt_state` does not retain it today. The pager and
everything above it only ever see plaintext.

A raw-ciphertext copy (ship encrypted bytes verbatim, never decrypt) is **not
viable** for this engine because the two storage surfaces use different on-disk
layouts:

- **Main-DB page** (`Crypto.encrypt_page`): encrypts `page_size − 32` bytes,
  storing `nonce(16)+tag(16)` *in* the page's reserved tail. On-disk size =
  `page_size`.
- **WAL frame** (`Crypto.encrypt_frame`): encrypts the *full* `page_size`-byte
  payload and *appends* `nonce(16)+tag(16)`. On-disk size = `page_size + 32`.

`copy_to` merges the WAL overlay with the main DB into one flat page image. A
page that lives in the WAL is a frame-layout ciphertext, not a page-layout
ciphertext, so it cannot be written verbatim into a main-DB page slot. The merge
inherently passes through plaintext in-process (the pager already holds
plaintext), then re-encrypts into main-DB page layout for the destination. This
is acceptable: in-process plaintext is unavoidable; the leak being fixed is the
plaintext **destination file** and plaintext **crossing the sink**.

## #214 — encryption-aware hot-copy

### Core (`lib/store/store.ml`)

1. Add `cipher : Crypto.t option` to `bt_state`, populated at `open_block` /
   `open_block_wal` from the cipher already built by `build_cipher`. No new
   secret retention beyond what the closures already capture.

2. In `copy_to`, wrap the sink so that when `cipher = Some c`:
   - pages `0` and `1` (plaintext headers carrying `enc_magic` + canary) pass
     through **verbatim** — they are structural metadata, never user data, and
     the spec already keeps them plaintext on disk;
   - pages `≥ 2` are encrypted with `c` (fresh per-page nonce) into a scratch
     buffer before being handed to the sink.

   When `cipher = None`, behaviour is exactly as today (plaintext passthrough).

The destination thus receives the same on-disk byte format as a freshly-created
encrypted DB: plaintext headers with the source's canary, encrypted data pages,
no WAL sidecar. Opening it later with the same key validates the canary and
decrypts cleanly; opening without a key yields `Encryption_key_required`; with a
wrong key, `Encryption_key_mismatch` — all already handled by the open path.

### Unix (`lib/unix/store.ml`)

`copy_to_file` needs **no signature change**. It writes the bytes the sink
produces verbatim to the destination file, sized at the source's `page_size`
(already `reserved = 32` for an encrypted source). Because `copy_to` now emits
ciphertext for an encrypted source, the destination file is encrypted with no
further work.

### No API surface change

`copy_to : t -> page_sink -> unit Lwt.t` and
`copy_to_file : Core.t -> dest:string -> (unit, error) result Lwt.t` keep their
signatures. The behavioural change (encrypted source ⇒ encrypted destination) is
documented in both doc-comments.

## #215 — offline key rotation

### Core: `rekey_to`

```
val rekey_to : t -> new_key:string -> page_sink -> (unit, error) result Lwt.t
```

Preconditions and behaviour:

- The store `t` must have been opened **with the old key** (so reads decrypt to
  plaintext). If `t` is not encrypted, return `Not_encrypted` — rotation only
  applies to an already-encrypted DB. (Plaintext→encrypted "encrypt an existing
  DB" is explicitly out of scope per #84 / spec non-goals.)
- `new_key` must be 32 bytes, else `Block_error "encryption key must be 32 bytes"`
  (reusing `build_cipher`'s message).
- Builds a new cipher `c'` from `new_key` and a fresh canary
  (`nonce = Mirage_crypto_rng.generate 16`, `tag = Crypto.make_canary c' ~nonce`).
- Iterates the same consistent plaintext page image as `copy_to`:
  - pages `0` and `1`: decode header fields with `Page.read_header_fields`,
    replace `canary_nonce`/`canary_tag` with the new-key canary (leaving
    `enc_magic` set and every other field — txn parity, geometry, root,
    freelist, schema — untouched), then `Page.write_header_fields` +
    `Page.seal` to recompute the CRC, and sink the rebuilt page. Doing this on
    *each* header page preserves the alternating-slot invariant `read_live`
    relies on.
  - pages `≥ 2`: encrypt the plaintext with `c'` (fresh nonce) and sink.

This requires `Page.read_header_fields` / `write_header_fields` / `seal`, all
already exported.

### Shared iteration

`copy_to` and `rekey_to` share one internal helper that, under an RO snapshot,
yields each plaintext page `(page_id, buf)` for `page_id ∈ [0, n)` (WAL overlay
within the snapshot horizon, falling back to the main DB), exactly as the
current `copy_to` loop does. The two callers differ only in the per-page output
transform. The `Mem` backend is a no-op for both.

### Unix: `rotate_key_file`

```
val rotate_key_file
  :  src_path:string
  -> old_key:string
  -> new_key:string
  -> dest:string
  -> (unit, Core.error) result Lwt.t
```

- Opens `src_path` with `~key:old_key`, **WAL-aware**: if a `src_path ^ "-wal"`
  sidecar exists it opens via `open_file_wal` so any committed-but-uncheckpointed
  frames are folded into the snapshot (`rekey_to` reads through the same WAL
  overlay `copy_to` uses); otherwise it opens via `open_file`. Either way the
  destination is a single self-contained file with **no WAL sidecar**.
- Mirrors `copy_to_file`'s crash-safety: write to `dest.tmp`, `rekey_to` into it
  via a raw write_page sink, `fsync`, atomic `rename` to `dest`, then directory
  `fsync`. On any error, close and unlink the tmp file.
- Sizes the tmp file at the source `page_size` and `n_pages`, as `copy_to_file`
  does.
- The destination opens standalone via `open_file ~key:new_key`.

`old_key = new_key` is permitted (a no-op re-key that simply rewrites nonces);
not special-cased.

## Error handling

- `rekey_to` returns `Not_encrypted` if the source store is not encrypted, and
  the `Block_error` bad-length message if `new_key ≠ 32` bytes. RNG-unseeded is
  surfaced the same way the open path does — the source store could only have
  been opened with a key if the RNG was seeded, so by the time `rekey_to` runs
  the generator is available; nonce generation will not raise.
- `copy_to` adds no new error cases.
- `rotate_key_file` maps source-open errors straight through (so a wrong
  `old_key` surfaces `Encryption_key_mismatch`, a plaintext source surfaces
  `Not_encrypted`) and wraps I/O failures as `Block_error`, like `copy_to_file`.

## Testing (strict TDD)

### #214 — `test/test_online_backup.ml` (extends the existing file-backed suite)

- **encrypted copy round-trip**: create encrypted source (key K), put values,
  `copy_to_file`, reopen destination with K, assert values round-trip.
- **destination needs the key**: after copying an encrypted source, opening the
  destination *without* a key ⇒ `Encryption_key_required`; with a *wrong* key ⇒
  `Encryption_key_mismatch`.
- **plaintext-leak guard**: write a known marker value, copy, then scan the raw
  destination file bytes and assert the marker does **not** appear in cleartext.
- **encrypted WAL source**: same round-trip with a WAL-mode source that has
  uncheckpointed frames, proving the merge re-encrypts WAL-resident pages
  correctly.
- **plaintext source unchanged**: a non-encrypted source still copies to a
  readable plaintext destination (regression guard for the `cipher = None` path).

### #215 — new `test/test_store_rekey.ml` (+ a unix file-backed case)

- **rotation round-trip**: encrypted DB under K1 with known values →
  `rotate_key_file` to K2 → destination opens with K2 and round-trips the values.
- **old key fails after rotation**: the rotated destination opened with K1 ⇒
  `Encryption_key_mismatch` (canary was rewritten under K2).
- **wrong new key fails**: opening the destination with K3 ≠ K2 ⇒
  `Encryption_key_mismatch`.
- **reject plaintext source**: `rekey_to` on a non-encrypted store ⇒
  `Not_encrypted`.
- **reject bad new key length**: `new_key` ≠ 32 bytes ⇒ `Block_error`.
- **plaintext-leak guard**: rotated file's raw bytes contain neither the known
  values in cleartext nor any ciphertext block decryptable under K1.

All suites seed the RNG with `Mirage_crypto_rng_unix.use_default ()` at startup,
matching `test_store_crypto.ml`.

## Scope notes / non-goals

- No plaintext↔encrypted conversion (encrypting an existing plaintext DB, or
  decrypting to plaintext) — out of scope per #84 non-goals; `rekey_to` requires
  an encrypted source and an encrypted destination.
- No in-place rotation; rotation produces a new file (crash-safe), and the
  caller swaps it in. Documented.
- No re-keying of a live/online database; rotation is offline (the source is
  opened, fully read, and closed by the tool).
- Header metadata (geometry, txn, root, schema) remains plaintext, as in #84.
