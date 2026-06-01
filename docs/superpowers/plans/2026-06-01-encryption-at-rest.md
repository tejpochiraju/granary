# Encryption at Rest (AES-256-GCM) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add opt-in, per-page AES-256-GCM encryption at rest to the sqlocaml storage layer (main DB + WAL), off by default and fixed at database creation.

**Architecture:** A pure `Crypto` module wraps the library-provided AES-GCM core. The store wraps its `read_page`/`write_page` callbacks (pages ≥ 2) and passes a cipher into the WAL frame codec; the pager and everything above it only ever see plaintext. Header pages stay plaintext and carry an encryption marker plus a key-check canary. See `docs/specs/2026-06-01-encryption-at-rest-design.md`.

**Tech Stack:** OCaml 5.4, `mirage-crypto` (AES.GCM), `mirage-crypto-rng` (nonces; seeded only in `bin/`/tests), Cstruct, Lwt, alcotest + qcheck-alcotest.

---

## Conventions for every task

**All dune/merlint commands run inside the `sqlocaml-dev` Podman image** (never on the host). From the worktree root `/home/tej/projects/sqlite_ocaml_port-84-encryption-at-rest`, define once per shell:

```sh
WT=/home/tej/projects/sqlite_ocaml_port-84-encryption-at-rest
chmod 777 "$WT"        # container's opam user needs to write _build (worktree perms)
POD="podman run --rm -v $WT:/workspace:Z -w /workspace sqlocaml-dev opam exec --"
```

- Build: `$POD dune build`
- Test (one file): `$POD dune exec test/test_crypto.exe` — or run the whole suite: `$POD dune runtest`
- Format: `$POD dune fmt` (apply) / `$POD dune build @fmt` (check)
- Lint: `podman run --rm -v $WT:/workspace:Z -w /workspace sqlocaml-dev merlint --color=never` (must be zero findings)

**TDD is non-negotiable** (red → green → refactor). Never weaken a test to pass. `dune runtest` must be green before every commit. End every commit message with:
```
Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
```

---

## File structure

- **Create** `lib/storage/crypto.ml` + `crypto.mli` — pure GCM page/frame/canary codec.
- **Create** `test/test_crypto.ml` — unit + property + NIST KAT tests for `Crypto`.
- **Create** `test/test_encryption_e2e.ml` — file-backed integration (round-trip, reopen, WAL, recovery, error matrix, plaintext-leak guard).
- **Modify** `Containerfile` — install `mirage-crypto`, `mirage-crypto-rng`, `mirage-crypto-rng-lwt`.
- **Modify** `dune-project` — add the three deps to the `depends` stanza.
- **Modify** `lib/storage/dune` — add `mirage-crypto`, `mirage-crypto-rng` to `libraries`.
- **Modify** `lib/storage/page.ml` + `page.mli` — header_fields gains `enc_magic`/`canary_nonce`/`canary_tag` (bytes 68/72/88).
- **Modify** `lib/storage/header.ml` + `header.mli` — `Header.t` gains an `enc` field; `build_page`/`decode_page`/`init`/`peek_geometry` handle it.
- **Modify** `lib/storage/wal.ml` + `wal.mli` — `open_` gains `?cipher`; frame size/codec become cipher-aware.
- **Modify** `lib/store/store.ml` + `store.mli` — new error constructors; `?key` on `open_block`/`open_block_wal`; callback wrapping; canary verify; preserve `enc` on commit.
- **Modify** `lib/unix/store.ml` + `store.mli` — `?key` on `open_file`/`open_file_wal`, threaded to core.
- **Modify** `test/dune` — register `test_crypto`, `test_encryption_e2e`; add crypto libs.

---

## Task 0: Dependencies and build image

**Files:**
- Modify: `Containerfile`
- Modify: `dune-project`
- Modify: `lib/storage/dune`

- [ ] **Step 1: Add crypto packages to the Containerfile**

In `Containerfile`, extend the first `opam install` line to include the crypto packages:

```dockerfile
RUN opam install -y lwt cstruct menhir alcotest qcheck-alcotest lwt_ppx mirage-block mirage-block-unix uutf uucp mirage-crypto mirage-crypto-rng mirage-crypto-rng-lwt
```

- [ ] **Step 2: Rebuild the image**

Run: `podman build -t sqlocaml-dev /home/tej/projects/sqlite_ocaml_port-84-encryption-at-rest`
Expected: build succeeds; `mirage-crypto.1.2.0`, `mirage-crypto-rng.1.2.0`, `mirage-crypto-rng-lwt.1.2.0` installed. (The rng cascade pins `mirage-crypto` to 1.2.0 — that is expected and fine.)

- [ ] **Step 3: Add deps to dune-project**

In `dune-project`, inside the `(depends ...)` of the `sqlocaml` package, add:

```
  (mirage-crypto (>= "1.2.0"))
  (mirage-crypto-rng (>= "1.2.0"))
  (mirage-crypto-rng-lwt :with-test)
```

- [ ] **Step 4: Add libraries to lib/storage/dune**

In `lib/storage/dune`, change the `(libraries ...)` line to:

```
 (libraries cstruct lwt sqlocaml.block mirage-crypto mirage-crypto-rng)
```

- [ ] **Step 5: Verify the build still works**

Run: `$POD dune build`
Expected: success (no code changes yet; just confirms the new libs resolve).

- [ ] **Step 6: Commit**

```bash
git add Containerfile dune-project lib/storage/dune
git commit -m "build(#84): add mirage-crypto + mirage-crypto-rng deps"
```

---

## Task 1: Crypto module (pure GCM codec)

**Files:**
- Create: `lib/storage/crypto.mli`
- Create: `lib/storage/crypto.ml`
- Create: `test/test_crypto.ml`
- Modify: `test/dune`

- [ ] **Step 1: Write the interface**

Create `lib/storage/crypto.mli`:

```ocaml
(** Per-page AES-256-GCM codec for at-rest encryption (#84).

    Pure: no I/O.  The application supplies a 32-byte raw key; nonces are
    freshly generated per call via {!Mirage_crypto_rng} (the application must
    seed the RNG at boot — [lib/] never seeds, to stay Mirage-clean).

    On-disk encrypted page layout (length [n] = page_size):
    {[ [ ciphertext : n - overhead ] [ nonce : nonce_len ] [ tag : tag_len ] ]}
    The encrypted region is the logical page minus the reserved tail; the
    B+-tree already keeps its data within that region. *)

type t

val nonce_len : int (** 16 *)
val tag_len : int (** 16 *)
val overhead : int (** 32 — reserved bytes an encrypted page must carve off its tail *)

(** Build a cipher from raw key material.  [Error `Bad_key_length] unless the
    key is exactly 32 bytes (AES-256). *)
val create : key:string -> (t, [ `Bad_key_length ]) result

(** Encrypt the page in place: encrypts bytes [0 .. len-overhead), then writes
    the freshly-generated nonce and the auth tag into the reserved tail.  AAD is
    the [page_id], binding the ciphertext to its slot.  [buf] length must be
    > [overhead]. *)
val encrypt_page : t -> page_id:int64 -> Cstruct.t -> unit

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
val decrypt_frame : t -> page_id:int64 -> Cstruct.t -> (Cstruct.t, [ `Tag_mismatch ]) result

(** Fixed associated data for the key-check canary. *)
val canary_adata : string

(** [make_canary t ~nonce] returns the [tag_len]-byte GCM tag over an empty
    message with [canary_adata], under [t]'s key and [nonce]. *)
val make_canary : t -> nonce:string -> string

(** [check_canary t ~nonce ~tag] recomputes the canary and reports whether it
    matches [tag] (i.e. the key is correct). *)
val check_canary : t -> nonce:string -> tag:string -> bool
```

- [ ] **Step 2: Write the failing test**

Create `test/test_crypto.ml`:

```ocaml
module C = Sqlocaml_storage.Crypto

let key32 = String.make 32 'k'
let cipher () = match C.create ~key:key32 with Ok t -> t | Error _ -> assert false

let seed_rng () =
  Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna)

let mk_page () =
  let p = Cstruct.create 4096 in
  for i = 0 to 4096 - C.overhead - 1 do
    Cstruct.set_uint8 p i (i land 0xff)
  done;
  p

let test_create_rejects_short_key () =
  (match C.create ~key:"short" with
   | Error `Bad_key_length -> ()
   | Ok _ -> Alcotest.fail "expected Bad_key_length")

let test_roundtrip () =
  let t = cipher () in
  let p = mk_page () in
  let orig = Cstruct.to_string p ~off:0 ~len:(4096 - C.overhead) in
  C.encrypt_page t ~page_id:7L p;
  Alcotest.(check bool)
    "ciphertext differs from plaintext"
    false
    (String.equal orig (Cstruct.to_string p ~off:0 ~len:(4096 - C.overhead)));
  (match C.decrypt_page t ~page_id:7L p with
   | Ok () -> ()
   | Error `Tag_mismatch -> Alcotest.fail "roundtrip decrypt failed");
  Alcotest.(check string)
    "plaintext restored"
    orig
    (Cstruct.to_string p ~off:0 ~len:(4096 - C.overhead))

let test_tail_zeroed_after_decrypt () =
  let t = cipher () in
  let p = mk_page () in
  C.encrypt_page t ~page_id:1L p;
  (match C.decrypt_page t ~page_id:1L p with Ok () -> () | Error _ -> Alcotest.fail "decrypt");
  for i = 4096 - C.overhead to 4095 do
    Alcotest.(check int) "tail zeroed" 0 (Cstruct.get_uint8 p i)
  done

let test_wrong_key_fails () =
  let t = cipher () in
  let p = mk_page () in
  C.encrypt_page t ~page_id:3L p;
  let t2 = match C.create ~key:(String.make 32 'x') with Ok t -> t | Error _ -> assert false in
  (match C.decrypt_page t2 ~page_id:3L p with
   | Error `Tag_mismatch -> ()
   | Ok () -> Alcotest.fail "wrong key must fail")

let test_wrong_page_id_fails () =
  let t = cipher () in
  let p = mk_page () in
  C.encrypt_page t ~page_id:3L p;
  (match C.decrypt_page t ~page_id:4L p with
   | Error `Tag_mismatch -> ()
   | Ok () -> Alcotest.fail "AAD mismatch must fail")

let test_tamper_fails () =
  let t = cipher () in
  let p = mk_page () in
  C.encrypt_page t ~page_id:3L p;
  Cstruct.set_uint8 p 0 (Cstruct.get_uint8 p 0 lxor 0xff);
  (match C.decrypt_page t ~page_id:3L p with
   | Error `Tag_mismatch -> ()
   | Ok () -> Alcotest.fail "tampered ciphertext must fail")

let test_nonce_freshness () =
  let t = cipher () in
  let p1 = mk_page () and p2 = mk_page () in
  C.encrypt_page t ~page_id:5L p1;
  C.encrypt_page t ~page_id:5L p2;
  Alcotest.(check bool)
    "same plaintext + page_id → different ciphertext (fresh nonce)"
    false
    (Cstruct.equal p1 p2)

let test_frame_roundtrip () =
  let t = cipher () in
  let pt = Cstruct.create 4096 in
  for i = 0 to 4095 do Cstruct.set_uint8 pt i ((i * 7) land 0xff) done;
  let enc = C.encrypt_frame t ~page_id:9L ~plaintext:pt in
  Alcotest.(check int) "frame payload grows by overhead" (4096 + C.overhead) (Cstruct.length enc);
  (match C.decrypt_frame t ~page_id:9L enc with
   | Ok dec -> Alcotest.(check bool) "frame roundtrip" true (Cstruct.equal pt dec)
   | Error `Tag_mismatch -> Alcotest.fail "frame decrypt failed")

let test_canary () =
  let t = cipher () in
  let nonce = Mirage_crypto_rng.generate C.nonce_len in
  let tag = C.make_canary t ~nonce in
  Alcotest.(check bool) "right key passes canary" true (C.check_canary t ~nonce ~tag);
  let t2 = match C.create ~key:(String.make 32 'z') with Ok t -> t | Error _ -> assert false in
  Alcotest.(check bool) "wrong key fails canary" false (C.check_canary t2 ~nonce ~tag)

(* NIST SP 800-38D AES-256-GCM known-answer vector (test case 14):
   key = 32 zero bytes, IV = 12 zero bytes, P = 16 zero bytes, A = empty.
   Expected C = cea7403d4d606b6e074ec5d3baf39d18,
            T = d0d1c8a799996bf0265b98b5d48ab919. *)
let test_nist_kat () =
  let h s = (* hex string -> raw *)
    String.init (String.length s / 2) (fun i ->
      Char.chr (int_of_string ("0x" ^ String.sub s (i * 2) 2)))
  in
  let key = h (String.make 64 '0') in
  let k = Mirage_crypto.AES.GCM.of_secret key in
  let nonce = h (String.make 24 '0') in
  let msg = h (String.make 32 '0') in
  let c, tag = Mirage_crypto.AES.GCM.authenticate_encrypt_tag ~key:k ~nonce msg in
  let to_hex s = String.concat "" (List.init (String.length s) (fun i ->
    Printf.sprintf "%02x" (Char.code s.[i]))) in
  Alcotest.(check string) "NIST ciphertext" "cea7403d4d606b6e074ec5d3baf39d18" (to_hex c);
  Alcotest.(check string) "NIST tag" "d0d1c8a799996bf0265b98b5d48ab919" (to_hex tag)

(* Property: arbitrary page contents round-trip. *)
let prop_roundtrip =
  QCheck.Test.make ~count:200 ~name:"encrypt/decrypt roundtrip"
    QCheck.(string_of_size (Gen.return (4096 - C.overhead)))
    (fun s ->
       let t = cipher () in
       let p = Cstruct.create 4096 in
       Cstruct.blit_from_string s 0 p 0 (String.length s);
       C.encrypt_page t ~page_id:42L p;
       (match C.decrypt_page t ~page_id:42L p with
        | Error _ -> false
        | Ok () -> String.equal s (Cstruct.to_string p ~off:0 ~len:(4096 - C.overhead))))

let () =
  seed_rng ();
  Alcotest.run "crypto"
    [ ( "unit"
      , [ Alcotest.test_case "create rejects short key" `Quick test_create_rejects_short_key
        ; Alcotest.test_case "page roundtrip" `Quick test_roundtrip
        ; Alcotest.test_case "tail zeroed" `Quick test_tail_zeroed_after_decrypt
        ; Alcotest.test_case "wrong key" `Quick test_wrong_key_fails
        ; Alcotest.test_case "wrong page_id" `Quick test_wrong_page_id_fails
        ; Alcotest.test_case "tamper" `Quick test_tamper_fails
        ; Alcotest.test_case "nonce freshness" `Quick test_nonce_freshness
        ; Alcotest.test_case "frame roundtrip" `Quick test_frame_roundtrip
        ; Alcotest.test_case "canary" `Quick test_canary
        ; Alcotest.test_case "NIST KAT" `Quick test_nist_kat
        ] )
    ; "property", List.map QCheck_alcotest.to_alcotest [ prop_roundtrip ]
    ]
```

- [ ] **Step 3: Register the test in test/dune**

In `test/dune`, add `test_crypto` to the `(names ...)` list, and add `mirage-crypto` and `mirage-crypto-rng-lwt` to the test `(libraries ...)` list (alongside the existing `alcotest`, `qcheck-alcotest`, `sqlocaml`, …).

- [ ] **Step 4: Run the test to verify it fails**

Run: `$POD dune build`
Expected: FAIL — `Unbound module Sqlocaml_storage.Crypto`.

- [ ] **Step 5: Implement the module**

Create `lib/storage/crypto.ml`:

```ocaml
module GCM = Mirage_crypto.AES.GCM

let nonce_len = 16
let tag_len = 16
let overhead = nonce_len + tag_len

type t = { key : GCM.key }

let create ~key =
  if String.length key <> 32 then Error `Bad_key_length else Ok { key = GCM.of_secret key }

let adata_of_page_id page_id =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 page_id;
  Bytes.unsafe_to_string b

let encrypt_page t ~page_id buf =
  let n = Cstruct.length buf in
  let region = n - overhead in
  let pt = Cstruct.to_string buf ~off:0 ~len:region in
  let nonce = Mirage_crypto_rng.generate nonce_len in
  let adata = adata_of_page_id page_id in
  let ct, tag = GCM.authenticate_encrypt_tag ~key:t.key ~nonce ~adata pt in
  Cstruct.blit_from_string ct 0 buf 0 region;
  Cstruct.blit_from_string nonce 0 buf region nonce_len;
  Cstruct.blit_from_string tag 0 buf (region + nonce_len) tag_len

let decrypt_page t ~page_id buf =
  let n = Cstruct.length buf in
  let region = n - overhead in
  let ct = Cstruct.to_string buf ~off:0 ~len:region in
  let nonce = Cstruct.to_string buf ~off:region ~len:nonce_len in
  let tag = Cstruct.to_string buf ~off:(region + nonce_len) ~len:tag_len in
  let adata = adata_of_page_id page_id in
  match GCM.authenticate_decrypt_tag ~key:t.key ~nonce ~adata ~tag ct with
  | None -> Error `Tag_mismatch
  | Some pt ->
    Cstruct.blit_from_string pt 0 buf 0 region;
    for i = region to n - 1 do
      Cstruct.set_uint8 buf i 0
    done;
    Ok ()

let encrypt_frame t ~page_id ~plaintext =
  let len = Cstruct.length plaintext in
  let pt = Cstruct.to_string plaintext in
  let nonce = Mirage_crypto_rng.generate nonce_len in
  let adata = adata_of_page_id page_id in
  let ct, tag = GCM.authenticate_encrypt_tag ~key:t.key ~nonce ~adata pt in
  let out = Cstruct.create (len + overhead) in
  Cstruct.blit_from_string ct 0 out 0 len;
  Cstruct.blit_from_string nonce 0 out len nonce_len;
  Cstruct.blit_from_string tag 0 out (len + nonce_len) tag_len;
  out

let decrypt_frame t ~page_id payload =
  let total = Cstruct.length payload in
  let len = total - overhead in
  let ct = Cstruct.to_string payload ~off:0 ~len in
  let nonce = Cstruct.to_string payload ~off:len ~len:nonce_len in
  let tag = Cstruct.to_string payload ~off:(len + nonce_len) ~len:tag_len in
  let adata = adata_of_page_id page_id in
  match GCM.authenticate_decrypt_tag ~key:t.key ~nonce ~adata ~tag ct with
  | None -> Error `Tag_mismatch
  | Some pt -> Ok (Cstruct.of_string pt)

let canary_adata = "sqlocaml-enc-v1"

let make_canary t ~nonce =
  let _, tag = GCM.authenticate_encrypt_tag ~key:t.key ~nonce ~adata:canary_adata "" in
  tag

let check_canary t ~nonce ~tag = String.equal tag (make_canary t ~nonce)

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-8"]
[@@@ai_provider "Anthropic"]
```

- [ ] **Step 6: Run the tests to verify they pass**

Run: `$POD dune exec test/test_crypto.exe`
Expected: PASS (all unit + property + NIST KAT cases green). If the NIST tag differs, stop — the AES-GCM usage is wrong; do not adjust the vector.

- [ ] **Step 7: Format, lint, commit**

```bash
$POD dune fmt
podman run --rm -v $WT:/workspace:Z -w /workspace sqlocaml-dev merlint --color=never
git add lib/storage/crypto.ml lib/storage/crypto.mli test/test_crypto.ml test/dune
git commit -m "feat(#84): pure AES-256-GCM page/frame/canary codec"
```

---

## Task 2: Header encryption marker and canary fields

**Files:**
- Modify: `lib/storage/page.ml`, `lib/storage/page.mli`
- Modify: `lib/storage/header.ml`, `lib/storage/header.mli`
- Modify: `test/test_header.ml`

The header page field area uses bytes 16–67; bytes 68+ are zeroed reserved space covered by the page CRC. We add `enc_magic` (byte 68), `canary_nonce` (bytes 72–87), `canary_tag` (bytes 88–103).

- [ ] **Step 1: Write the failing test**

Add to `test/test_header.ml` (and to its test list at the bottom):

```ocaml
let test_encryption_roundtrip_in_header () =
  let nonce = String.init 16 (fun i -> Char.chr (i + 1)) in
  let tag = String.init 16 (fun i -> Char.chr (i + 100)) in
  let geom = Sqlocaml_storage.Geometry.default in
  let h =
    { Sqlocaml_storage.Header.txn_id = 5L
    ; root_page = 3L
    ; freelist_page = 0L
    ; n_pages_total = 9L
    ; schema_version = 1L
    ; format_version = Sqlocaml_storage.Header.current_format_version
    ; geom
    ; enc = Some { Sqlocaml_storage.Header.canary_nonce = nonce; canary_tag = tag }
    }
  in
  let buf = Sqlocaml_storage.Header.build_page_for_test h in
  match Sqlocaml_storage.Header.decode_page_for_test buf with
  | None -> Alcotest.fail "decode failed"
  | Some h' ->
    (match h'.enc with
     | None -> Alcotest.fail "enc field lost"
     | Some e ->
       Alcotest.(check string) "nonce" nonce e.canary_nonce;
       Alcotest.(check string) "tag" tag e.canary_tag)

let test_plaintext_header_has_no_enc () =
  let h =
    { Sqlocaml_storage.Header.txn_id = 1L; root_page = 0L; freelist_page = 0L
    ; n_pages_total = 2L; schema_version = 0L
    ; format_version = Sqlocaml_storage.Header.current_format_version
    ; geom = Sqlocaml_storage.Geometry.default; enc = None }
  in
  let buf = Sqlocaml_storage.Header.build_page_for_test h in
  match Sqlocaml_storage.Header.decode_page_for_test buf with
  | Some { enc = None; _ } -> ()
  | _ -> Alcotest.fail "expected enc = None"
```

> NOTE: `build_page` and `decode_page` are currently private to `header.ml`. Expose thin test shims `build_page_for_test`/`decode_page_for_test` in `header.mli` (Step 3) — or, if the repo prefers not to widen the interface, gate them behind the existing test pattern used by other modules. Check `header.mli` for an existing test-only export convention first; follow it.

- [ ] **Step 2: Run to verify it fails**

Run: `$POD dune build`
Expected: FAIL — `enc` field / `build_page_for_test` unbound.

- [ ] **Step 3: Extend page.ml header_fields**

In `lib/storage/page.ml`, extend the `header_fields` record and its codec. After the existing `reserved_bytes_per_page` field add:

```ocaml
  ; enc_magic : int32 (** 0x53454E43 "SENC" when encrypted, else 0 (#84) *)
  ; canary_nonce : string (** 16 bytes; meaningful only when enc_magic set *)
  ; canary_tag : string (** 16 bytes; meaningful only when enc_magic set *)
```

In `read_header_fields`, after reading `reserved_bytes_per_page` at byte 64:

```ocaml
  let enc_magic = Cstruct.BE.get_uint32 buf 68 in
  let canary_nonce = Cstruct.to_string buf ~off:72 ~len:16 in
  let canary_tag = Cstruct.to_string buf ~off:88 ~len:16 in
```
and add them to the returned record.

In `write_header_fields`, after `Cstruct.BE.set_uint32 buf 64 hf.reserved_bytes_per_page;`:

```ocaml
  Cstruct.BE.set_uint32 buf 68 hf.enc_magic;
  if Int32.equal hf.enc_magic 0l
  then ()
  else (
    Cstruct.blit_from_string hf.canary_nonce 0 buf 72 16;
    Cstruct.blit_from_string hf.canary_tag 0 buf 88 16)
```

Update `page.mli`'s `header_fields` doc/type to match (add the three fields, document bytes 68/72/88).

- [ ] **Step 4: Add `enc` to Header.t and wire build/decode/init**

In `lib/storage/header.ml` and `header.mli`:

Add the encryption-info type and field to `Header.t`:

```ocaml
type encryption = { canary_nonce : string; canary_tag : string }
(* ... in type t, add: *)
  ; enc : encryption option
```

`enc_magic_value = 0x53454E43l` (a module-level constant).

In `build_page`, pass the three new fields when calling `Page.write_header_fields`:

```ocaml
    ; reserved_bytes_per_page = Int32.of_int h.geom.reserved_bytes_per_page
    ; enc_magic = (match h.enc with Some _ -> enc_magic_value | None -> 0l)
    ; canary_nonce = (match h.enc with Some e -> e.canary_nonce | None -> String.make 16 '\000')
    ; canary_tag = (match h.enc with Some e -> e.canary_tag | None -> String.make 16 '\000')
```

In `decode_page`, after building `geom`, reconstruct `enc`:

```ocaml
          let enc =
            if Int32.equal f.Page.enc_magic enc_magic_value
            then Some { canary_nonce = f.Page.canary_nonce; canary_tag = f.Page.canary_tag }
            else None
          in
```
and add `; enc` to the returned record.

In `init`, add an optional parameter and thread it into the `zero` header:

```ocaml
let init ?(enc = None) pager =
  let zero = { ...; geom = Pager.geom pager; enc } in
  ...
```
Update `header.mli`'s `init` signature to `?enc:encryption option -> Pager.t -> ...`, export the `encryption` type and the `enc` field on `t`, and (per Step 1 NOTE) the test shims if you chose to add them.

- [ ] **Step 5: Run the tests to verify they pass**

Run: `$POD dune exec test/test_header.exe`
Expected: PASS, including the existing header tests (the new fields default to plaintext/zeros for existing callers).

- [ ] **Step 6: Fix all other call sites of Header.t and Header.init**

Run: `$POD dune build` and fix every `{ Header.txn_id = …; … }` record literal and `Header.init` call the compiler flags (notably `lib/store/store.ml` `new_state` near line 1179 and the `init` calls in `open_block`/`open_block_wal`). For now, set `enc = None` / omit `?enc` everywhere except where Task 4 will change them. Expected after fixes: build clean.

- [ ] **Step 7: Format, lint, commit**

```bash
$POD dune fmt
podman run --rm -v $WT:/workspace:Z -w /workspace sqlocaml-dev merlint --color=never
git add lib/storage/page.ml lib/storage/page.mli lib/storage/header.ml lib/storage/header.mli lib/store/store.ml test/test_header.ml
git commit -m "feat(#84): encryption marker + key-check canary in the header"
```

---

## Task 3: WAL frame encryption

**Files:**
- Modify: `lib/storage/wal.ml`, `lib/storage/wal.mli`
- Create/extend: `test/test_wal_crypto.ml` (+ register in `test/dune`)

Frame layout when a cipher is present: `[ meta 24 ][ ciphertext page_size ][ nonce 16 ][ tag 16 ]`, so `frame_size = 24 + page_size + Crypto.overhead`. The checksum is computed over the whole payload region `[24 .. frame_size)` (ciphertext + nonce + tag), matching the spec's "checksum over ciphertext". The 24-byte meta stays plaintext.

- [ ] **Step 1: Write the failing test**

Create `test/test_wal_crypto.ml`:

```ocaml
module Wal = Sqlocaml_storage.Wal
module C = Sqlocaml_storage.Crypto

let seed () = Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna)

(* in-memory byte device *)
let make_dev () =
  let store = ref (Bytes.create 0) in
  let ensure n =
    if Bytes.length !store < n then (
      let b = Bytes.make n '\000' in
      Bytes.blit !store 0 b 0 (Bytes.length !store);
      store := b)
  in
  let read_at ~offset buf =
    let o = Int64.to_int offset in
    ensure (o + Cstruct.length buf);
    Cstruct.blit_from_bytes !store o buf 0 (Cstruct.length buf);
    Lwt.return_ok ()
  in
  let write_at ~offset buf =
    let o = Int64.to_int offset in
    ensure (o + Cstruct.length buf);
    Cstruct.blit_to_bytes buf 0 !store o (Cstruct.length buf);
    Lwt.return_ok ()
  in
  let sync () = Lwt.return_ok () in
  (read_at, write_at, sync, fun () -> Int64.of_int (Bytes.length !store))

let cipher () = match C.create ~key:(String.make 32 'k') with Ok t -> t | _ -> assert false

let mk_page tag =
  let p = Cstruct.create 4096 in
  for i = 0 to 4095 do Cstruct.set_uint8 p i ((i + tag) land 0xff) done;
  p

let test_encrypted_wal_roundtrip _ () =
  let read_at, write_at, sync, size = make_dev () in
  let%lwt w =
    match%lwt
      Wal.open_ ~cipher:(Some (cipher ())) ~read_at ~write_at ~sync ~size_bytes:(size ()) ()
    with
    | Ok w -> Lwt.return w
    | Error _ -> Alcotest.fail "open"
  in
  let p = mk_page 1 in
  let%lwt () =
    match%lwt Wal.append_commit w [ 5L, p ] with Ok () -> Lwt.return_unit | Error _ -> Alcotest.fail "append"
  in
  let%lwt got =
    match%lwt Wal.read_frame w 0 with Ok pg -> Lwt.return pg | Error _ -> Alcotest.fail "read"
  in
  Alcotest.(check bool) "decrypted page matches" true (Cstruct.equal p got);
  Lwt.return_unit

let test_ciphertext_on_disk _ () =
  (* the page bytes must NOT appear verbatim on the device *)
  let read_at, write_at, sync, size = make_dev () in
  let%lwt w =
    match%lwt Wal.open_ ~cipher:(Some (cipher ())) ~read_at ~write_at ~sync ~size_bytes:(size ()) () with
    | Ok w -> Lwt.return w | Error _ -> Alcotest.fail "open"
  in
  let p = mk_page 7 in
  let%lwt () = match%lwt Wal.append_commit w [ 2L, p ] with Ok () -> Lwt.return_unit | Error _ -> Alcotest.fail "" in
  (* read the raw frame payload region back off the device and ensure it != plaintext *)
  let raw = Cstruct.create 4096 in
  let%lwt () = match%lwt read_at ~offset:(Int64.of_int (24)) raw with Ok () -> Lwt.return_unit | Error _ -> Alcotest.fail "" in
  Alcotest.(check bool) "payload encrypted on disk" false (Cstruct.equal p raw);
  Lwt.return_unit

let () =
  seed ();
  Lwt_main.run
    (Alcotest_lwt.run "wal_crypto"
       [ ( "encrypted"
         , [ Alcotest_lwt.test_case "roundtrip" `Quick test_encrypted_wal_roundtrip
           ; Alcotest_lwt.test_case "ciphertext on disk" `Quick test_ciphertext_on_disk
           ] ) ])
```

> Check `test/dune` for whether `alcotest-lwt` is already a dependency (other async tests use it). If not, add `alcotest-lwt` to the test libraries and to the Containerfile `opam install` line (Task 0), then rebuild the image. Register `test_wal_crypto` in `(names ...)`.

- [ ] **Step 2: Run to verify it fails**

Run: `$POD dune build`
Expected: FAIL — `Wal.open_` has no `~cipher` label.

- [ ] **Step 3: Make the WAL cipher-aware**

In `lib/storage/wal.ml`:

Add `cipher : Crypto.t option` and a derived `cipher_overhead : int` to the `t` record. Compute `frame_size = frame_meta_bytes + page_size + cipher_overhead` where `cipher_overhead = match cipher with Some _ -> Crypto.overhead | None -> 0`.

Add `?(cipher = None)` as the first labelled arg of `open_`, set the field in all three construction sites, and compute `frame_size`/`cipher_overhead` once at the top of `open_`.

Replace `write_frame` body to branch on cipher:

```ocaml
let write_frame t ~idx ~page_id ~is_commit ~page =
  let buf = Cstruct.create t.frame_size in
  Cstruct.BE.set_uint64 buf 0 page_id;
  let flags = if is_commit then 1L else 0L in
  Cstruct.BE.set_uint64 buf 8 flags;
  let payload_len = t.page_size + t.cipher_overhead in
  (match t.cipher with
   | None -> Cstruct.blit page 0 buf frame_meta_bytes t.page_size
   | Some c ->
     let enc = Crypto.encrypt_frame c ~page_id ~plaintext:page in
     Cstruct.blit enc 0 buf frame_meta_bytes payload_len);
  let ck =
    frame_checksum ~salt:t.salt ~seed:t.seed ~page_id ~flags
      ~page:(Cstruct.sub buf frame_meta_bytes payload_len)
  in
  Cstruct.BE.set_uint64 buf 16 ck;
  let off = frame_offset t idx in
  t.write_at ~offset:off buf
```

In `read_frame_raw`, change the payload extraction + verify + decrypt:

```ocaml
      let payload_len = t.page_size + t.cipher_overhead in
      let payload = Cstruct.sub buf frame_meta_bytes payload_len in
      let ok =
        if verify
        then (
          let ck_have = Cstruct.BE.get_uint64 buf 16 in
          let ck_want = frame_checksum ~salt:t.salt ~seed:t.seed ~page_id ~flags ~page:payload in
          Int64.equal ck_have ck_want)
        else true
      in
      if ok
      then (
        match t.cipher with
        | None ->
          let page_copy = Cstruct.create t.page_size in
          Cstruct.blit payload 0 page_copy 0 t.page_size;
          let is_commit = Int64.logand flags 1L <> 0L in
          Lwt.return_ok (Some { frame_idx = idx; page_id; is_commit; page = page_copy })
        | Some c ->
          (match Crypto.decrypt_frame c ~page_id payload with
           | Error `Tag_mismatch -> Lwt.return_ok None
           | Ok page_copy ->
             let is_commit = Int64.logand flags 1L <> 0L in
             Lwt.return_ok (Some { frame_idx = idx; page_id; is_commit; page = page_copy })))
      else Lwt.return_ok None
```

(Keep the existing `frame_size_bytes`/`header_size_bytes` module constants for plaintext default callers.)

Add `?cipher:Crypto.t option` to `open_` in `wal.mli` (document: encrypts the page payload only; meta header stays plaintext; checksum over ciphertext).

- [ ] **Step 4: Run the tests to verify they pass**

Run: `$POD dune exec test/test_wal_crypto.exe`
Expected: PASS. Then `$POD dune exec test/test_wal.exe` (if present) and any WAL/group-commit tests — plaintext path must still pass unchanged.

- [ ] **Step 5: Format, lint, commit**

```bash
$POD dune fmt
podman run --rm -v $WT:/workspace:Z -w /workspace sqlocaml-dev merlint --color=never
git add lib/storage/wal.ml lib/storage/wal.mli test/test_wal_crypto.ml test/dune
git commit -m "feat(#84): encrypt WAL frame payloads inside the frame codec"
```

---

## Task 4: Store integration (key, wrapping, canary, errors)

**Files:**
- Modify: `lib/store/store.ml`, `lib/store/store.mli`

- [ ] **Step 1: Add error constructors (mli + ml)**

In `store.mli` and `store.ml`, extend `type error` with:

```ocaml
  | Encryption_key_required (** DB is encrypted but no key was supplied *)
  | Encryption_key_mismatch (** supplied key fails the header canary *)
  | Not_encrypted (** a key was supplied for a plaintext DB *)
```
and add `pp_error` cases.

- [ ] **Step 2: Write a failing store-level test**

Add to an existing store test or create `test/test_store_crypto.ml` (register it) the error-matrix expectations using an in-memory byte device + `Store.open_block`/`open_block_wal` with/without `~key`. Minimum cases:
- create fresh with `~key` (32 bytes) + `~geom` reserved=32 → ok; put/commit/reopen-with-key → value round-trips.
- reopen that encrypted DB with no key → `Error Encryption_key_required`.
- reopen with a wrong 32-byte key → `Error Encryption_key_mismatch`.
- open a plaintext DB with `~key` → `Error Not_encrypted`.

(Use the same in-memory device pattern as `test_wal_crypto.ml`, but with page-indexed `read_page ~page_id` / `write_page ~page_id` over a growable `bytes`. A 4096-byte page at `page_id * 4096`.)

- [ ] **Step 3: Run to verify it fails**

Run: `$POD dune build`
Expected: FAIL — `open_block` has no `~key` label / new error constructors unused.

- [ ] **Step 4: Implement `?key` in `open_block`**

Add `?(key : string option)` to `open_block`. Helper near the top of `store.ml`:

```ocaml
let build_cipher = function
  | None -> Ok None
  | Some k ->
    (match Sqlocaml_storage.Crypto.create ~key:k with
     | Ok c -> Ok (Some c)
     | Error `Bad_key_length -> Error (Block_error "encryption key must be 32 bytes"))

(* Wrap raw page callbacks so pages >= 2 are encrypted; headers (0,1) pass through. *)
let wrap_callbacks cipher ~read_page ~write_page =
  match cipher with
  | None -> read_page, write_page
  | Some c ->
    let rd ~page_id buf =
      let%lwt r = read_page ~page_id buf in
      match r with
      | Error _ as e -> Lwt.return e
      | Ok () ->
        if Int64.compare page_id 2L < 0
        then Lwt.return_ok ()
        else (
          match Sqlocaml_storage.Crypto.decrypt_page c ~page_id buf with
          | Ok () -> Lwt.return_ok ()
          | Error `Tag_mismatch -> Lwt.return_error "decrypt: tag mismatch")
    in
    let wr ~page_id buf =
      if Int64.compare page_id 2L < 0
      then write_page ~page_id buf
      else (
        let tmp = Cstruct.create (Cstruct.length buf) in
        Cstruct.blit buf 0 tmp 0 (Cstruct.length buf);
        Sqlocaml_storage.Crypto.encrypt_page c ~page_id tmp;
        write_page ~page_id tmp)
    in
    rd, wr
```

> The `write_page` wrapper copies into `tmp` before encrypting so it never mutates the pager's cached plaintext buffer.

Open flow changes in `open_block`:
1. `let cipher = build_cipher key` (propagate `Error`).
2. For a FRESH encrypted DB, force the creation geometry to carry the crypto overhead: `let geom = match cipher with Some _ -> geometry_with_min_reserved geom Sqlocaml_storage.Crypto.overhead | None -> geom` where `geometry_with_min_reserved g n` rebuilds via `Geometry.create ~page_size:g.page_size ~reserved_bytes_per_page:(max g.reserved_bytes_per_page n)` (this `geom` is only used as the fresh-creation fallback; an existing file's peeked geometry wins).
3. Build wrapped callbacks: `let read_page, write_page = wrap_callbacks cipher ~read_page ~write_page in` and create the pager with the wrapped ones. (peek_geometry uses the wrapped `read_page` on page 0, which passes through plaintext — correct.)
4. After `Header.read_live` returns `Ok h`, run the canary/open-time check (Step 6 helper) before proceeding; on the fresh-init branch, pass `~enc` to `Header.init` (Step 5).

- [ ] **Step 5: Compute + persist the canary on fresh encrypted init**

Add a helper:

```ocaml
let make_enc_info = function
  | None -> None
  | Some c ->
    let nonce = Mirage_crypto_rng.generate Sqlocaml_storage.Crypto.nonce_len in
    let tag = Sqlocaml_storage.Crypto.make_canary c ~nonce in
    Some { Header.canary_nonce = nonce; canary_tag = tag }
```

In the fresh-device branch of both `open_block` and `open_block_wal`, call `Header.init ~enc:(make_enc_info cipher) pager`.

- [ ] **Step 6: Open-time canary / matrix check**

Add a helper applied right after a successful `Header.read_live` (`Ok h`) on an EXISTING device, before building the store:

```ocaml
let check_key h cipher =
  match h.Header.enc, cipher with
  | None, None -> Ok ()
  | Some _, None -> Error Encryption_key_required
  | None, Some _ -> Error Not_encrypted
  | Some e, Some c ->
    if Sqlocaml_storage.Crypto.check_canary c ~nonce:e.Header.canary_nonce ~tag:e.Header.canary_tag
    then Ok ()
    else Error Encryption_key_mismatch
```

Apply it in `open_block`'s `Ok h` branch and in `finish_wal_open`'s `Ok h` branch (thread `cipher` into `finish_wal_open`). On the fresh-init path the canary was just written by us, so it passes; you may still run `check_key` after the post-init `read_live` for uniformity.

- [ ] **Step 7: Preserve `enc` across commits**

In `store.ml` near line 1179, the `new_state : Header.t` record copies `format_version` and `geom` from `st.current_header`. Add:

```ocaml
      ; enc = st.current_header.enc
```
so commits never drop the marker/canary.

- [ ] **Step 8: Implement `?key` in `open_block_wal`**

Mirror Steps 4–6 in `open_block_wal`:
- `build_cipher`, geometry overhead bump, `wrap_callbacks` for the main-DB pager callbacks.
- Pass `~cipher` to `Wal.open_` (Step from Task 3): `Wal.open_ ~cipher ~page_size:(Pager.page_size pager) ...`.
- Pass `~enc:(make_enc_info cipher)` to `Header.init` on fresh.
- Thread `cipher` into `finish_wal_open` and run `check_key` there.

- [ ] **Step 9: Update the mli signatures**

Add `?key:string` to `open_block` and `open_block_wal` in `store.mli` with a doc comment: "When supplied (32-byte AES-256 key), a fresh database is created encrypted and an existing one is opened encrypted; absent ⇒ plaintext (the default). Errors: `Encryption_key_required`, `Encryption_key_mismatch`, `Not_encrypted`."

- [ ] **Step 10: Run tests, format, lint, commit**

```bash
$POD dune build
$POD dune exec test/test_store_crypto.exe   # or: $POD dune runtest
$POD dune fmt
podman run --rm -v $WT:/workspace:Z -w /workspace sqlocaml-dev merlint --color=never
git add lib/store/store.ml lib/store/store.mli test/test_store_crypto.ml test/dune
git commit -m "feat(#84): opt-in page encryption in the store open paths"
```

---

## Task 5: Unix driver + full integration tests

**Files:**
- Modify: `lib/unix/store.ml`, `lib/unix/store.mli`
- Create: `test/test_encryption_e2e.ml` (+ register in `test/dune`)

- [ ] **Step 1: Thread `?key` through the unix driver**

In `lib/unix/store.mli`, add `?key:string` to `open_file` and `open_file_wal` (document opt-in, 32 bytes). In `lib/unix/store.ml`, pass `?key` through to `Core.open_block` / `Core.open_block_wal`. When `key` is supplied and creating a fresh file, ensure the pre-sized file uses a geometry whose `reserved_bytes_per_page >= Sqlocaml_storage.Crypto.overhead` — set the default `reserved_bytes_per_page` to `max requested Crypto.overhead` when `key <> None` so `resolve_geometry`/`set_page_size` size correctly. (Existing files keep their stored geometry.)

- [ ] **Step 2: Write the failing integration test**

Create `test/test_encryption_e2e.ml` covering, over a real temp file via the unix driver:

```ocaml
(* pseudocode outline — fill in with the same Lwt/alcotest-lwt style as
   test/test_online_backup.ml or test/test_crash_recovery.ml *)
let key = String.make 32 'K'

(* 1. create encrypted, write rows, commit, close *)
(* 2. reopen with key → rows present (round-trip across reopen) *)
(* 3. reopen without key → Error Encryption_key_required *)
(* 4. reopen with wrong key → Error Encryption_key_mismatch *)
(* 5. WAL mode: open_file_wal with key, commit, checkpoint, reopen → rows present *)
(* 6. WAL mode crash recovery: commit (no checkpoint), reopen with key → rows present *)
(* 7. plaintext-leak guard: write a distinctive value (e.g. "TOPSECRET-CANARY-VALUE"),
      then read the raw main-DB file bytes AND the -wal file bytes and assert the
      literal value string does NOT occur *)
```

Use `Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna)` once at the start of `main`. Read raw bytes with standard `In_channel` for the leak guard.

- [ ] **Step 3: Run to verify it fails**

Run: `$POD dune build`
Expected: FAIL — `open_file` has no `~key` label.

- [ ] **Step 4: Implement, then run to verify pass**

Run: `$POD dune exec test/test_encryption_e2e.exe`
Expected: PASS — all 7 scenarios, crucially the plaintext-leak guard (no plaintext value on disk in either file).

- [ ] **Step 5: Run the FULL suite (regression guard)**

Run: `$POD dune runtest`
Expected: the entire existing suite stays green (plaintext paths unchanged) plus the new crypto tests.

- [ ] **Step 6: Format, lint, commit**

```bash
$POD dune fmt
podman run --rm -v $WT:/workspace:Z -w /workspace sqlocaml-dev merlint --color=never
git add lib/unix/store.ml lib/unix/store.mli test/test_encryption_e2e.ml test/dune
git commit -m "feat(#84): unix-driver ?key + end-to-end encryption tests"
```

---

## Task 6: Docs, scope follow-ups, and PR

**Files:**
- Modify: `README.md` and/or `ROADMAP.md` (note the feature, opt-in, AES-256-GCM, fixed-at-creation)

- [ ] **Step 1: Document the feature**

Add a short "Encryption at rest" subsection: opt-in via a 32-byte key at `open_file`/`open_block`; AES-256-GCM page + WAL payload encryption; fixed at creation; metadata-leak caveats (header fields, WAL page-ids/commit pattern); RNG must be seeded by the app at boot.

- [ ] **Step 2: File the deferred-scope issues on Forgejo**

Per repo discipline (file deferred work as issues, not TODOs), create:
1. **copy_to / hot-copy is encryption-unaware** — `copy_to` sinks plaintext pages; a destination is encrypted only if reopened with a key, and the bytes transit the sink in cleartext. Make hot-copy encryption-aware (or document the requirement that the destination be opened with a key).
2. **Key rotation** — offline bulk re-encrypt tooling; out of scope for v1.

```bash
echo '{"title":"copy_to/hot-copy is encryption-unaware (#84 follow-up)","body":"`copy_to` resolves pages through the pager (plaintext), so a hot copy of an encrypted DB emits plaintext pages; the destination becomes encrypted only if itself opened with a key, and pages cross the sink in cleartext. Make hot-copy encryption-aware, or document/validate that the destination must be opened with the same key.","labels":[]}' \
  | ~/.local/bin/forgejo api POST /repos/tej/sqlite_ocaml_port/issues --input -
echo '{"title":"Key rotation for encrypted databases (#84 follow-up)","body":"Re-encrypting every page under a new key is an offline bulk operation. Out of scope for the v1 encryption-at-rest feature (#84). Track tooling/design here.","labels":[]}' \
  | ~/.local/bin/forgejo api POST /repos/tej/sqlite_ocaml_port/issues --input -
```

- [ ] **Step 3: Commit docs**

```bash
git add README.md ROADMAP.md docs/specs/2026-06-01-encryption-at-rest-design.md docs/superpowers/plans/2026-06-01-encryption-at-rest.md
git commit -m "docs(#84): document encryption at rest; spec + plan"
```

- [ ] **Step 4: Push and open the PR**

```bash
git push -u origin feat/84-encryption-at-rest
echo '{"title":"feat(#84): encryption at rest (AES-256-GCM, opt-in)","body":"Implements #84. Opt-in, off by default. Per-page AES-256-GCM for the main DB (callback wrap, pages >=2) and WAL (frame codec); header stays plaintext with an encryption marker + key-check canary. Fixed at creation. mirage-crypto + mirage-crypto-rng (seeded only in bin/tests). See docs/specs/2026-06-01-encryption-at-rest-design.md.\n\nFollow-ups filed: hot-copy encryption-awareness; key rotation.\n\nCloses #84\n\n🤖 Generated with [Claude Code](https://claude.com/claude-code)","head":"feat/84-encryption-at-rest","base":"main"}' \
  | ~/.local/bin/forgejo api POST /repos/tej/sqlite_ocaml_port/pulls --input -
```

---

## Self-review notes (author checklist, completed)

- **Spec coverage:** primitive/overhead → Task 1; layering (main DB wrap, WAL codec) → Tasks 3–4; header marker+canary+matrix → Tasks 2,4; `crypto.ml` module → Task 1; testing (unit/KAT/property/integration/leak-guard) → Tasks 1,3,5; opt-in default → Tasks 4,5; scope follow-ups → Task 6.
- **Type consistency:** `Crypto.{encrypt,decrypt}_page`, `{encrypt,decrypt}_frame`, `make_canary`/`check_canary`, `overhead`/`nonce_len`/`tag_len`; `Header.encryption = { canary_nonce; canary_tag }` and `Header.t.enc`; `page.ml header_fields.{enc_magic,canary_nonce,canary_tag}`; store errors `Encryption_key_required|Encryption_key_mismatch|Not_encrypted` — all used consistently across tasks.
- **No placeholders:** every code step shows real code; the one outlined file (`test_encryption_e2e.ml`, Step 5.2) lists the 7 concrete scenarios to implement against the existing test style.
- **Open verification items for the implementer:** (a) whether `header.mli` already has a test-export convention vs. adding `*_for_test` shims (Task 2 Step 1 NOTE); (b) whether `alcotest-lwt` is already a test dep (Task 3 Step 1 NOTE).
