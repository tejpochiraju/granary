# Encryption-aware hot-copy (#214) & offline key rotation (#215) — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `Store.copy_to` produce an encrypted destination for an encrypted source (#214), and add offline key rotation (`Store.rekey_to` + `Sqlocaml_unix.Store.rotate_key_file`) that re-encrypts every page under a new key (#215).

**Architecture:** Store the per-DB cipher in `bt_state`. Extract the existing `copy_to` page-iteration into a shared helper that yields each plaintext page under an RO snapshot (WAL overlay + main DB). `copy_to` re-encrypts pages ≥2 under the source's own cipher; `rekey_to` re-encrypts under a new cipher and rewrites the header canary. The Unix layer wraps `rekey_to` in a crash-safe, WAL-aware file rotation mirroring `copy_to_file`.

**Tech Stack:** OCaml, Lwt, `mirage-crypto` (AES-256-GCM), Alcotest. Build/test run inside the `sqlocaml-dev` Podman image (never call `dune` on the host).

**Build/test invocation (every "Run" step uses this):**
```bash
WT="$(pwd)"   # the worktree root, already chmod 777
PODMAN() { podman run --rm -v "${WT}:/workspace:Z" -w /workspace sqlocaml-dev bash -c "$1"; }
```
Run a single suite: `PODMAN "dune exec test/test_online_backup.exe 2>&1 | tail -40"`
Build only: `PODMAN "dune build 2>&1 | tail -30"`

---

## File Structure

- `lib/store/store.ml` — add `cipher` to `bt_state`, thread it through both open paths, extract `iter_snapshot_pages`, make `copy_to` encryption-aware, add `rekey_to`.
- `lib/store/store.mli` — document `copy_to`'s new behaviour; declare `rekey_to`.
- `lib/unix/store.ml` — add `rotate_key_file`.
- `lib/unix/store.mli` — declare `rotate_key_file`.
- `test/test_online_backup.ml` — add encrypted-copy cases (#214).
- `test/test_store_rekey.ml` — new suite for rotation (#215).
- `test/dune` — add `mirage-crypto-rng*` to `test_online_backup`; register `test_store_rekey`.

---

## Task 1: Store the cipher in `bt_state` and thread it through the open paths

This is a prerequisite for both #214 and #215. No behaviour change yet, so it is verified by a clean build + green existing crypto suite.

**Files:**
- Modify: `lib/store/store.ml` (bt_state record ~line 95; `make_btree_store` ~line 521; the two open sites ~line 748/758 and ~line 822)

- [ ] **Step 1: Add the `cipher` field to `bt_state`**

In `lib/store/store.ml`, the `bt_state` record begins at line 95 with `{ close_fn : ...`. Add the field right after `pager`:

```ocaml
type bt_state =
  { close_fn : unit -> unit Lwt.t
  ; pager : Pager.t
  ; cipher : Crypto.t option
    (** #84: the page cipher when the DB is encrypted, else [None].  Mirrors
        the cipher captured by the read/write callback closures; retained here
        so [copy_to]/[rekey_to] can re-encrypt the snapshot page image. *)
  ; mutable meta : Btree.t
  ; ...
```

(Leave every other field unchanged.)

- [ ] **Step 2: Add a `?cipher` parameter to `make_btree_store`**

`make_btree_store` is at line 521. Add the optional param and set the record field:

```ocaml
let make_btree_store
      ?(wal = None)
      ?(wal_close = None)
      ?(cipher = None)
      ~close_fn
      ~pager
      ~meta
      ~(h : Header.t)
      ()
  =
  let st =
    { close_fn
    ; pager
    ; cipher
    ; meta
    ; trees = Hashtbl.create 16
    ; ...
```

(Insert `; cipher` immediately after `; pager` in the record literal; keep the rest.)

- [ ] **Step 3: Pass `~cipher` at all three `make_btree_store` call sites**

In `open_block` there are two calls (the fresh-init branch ~line 748 and the existing-header branch ~line 758). In each, the local variable `cipher` is in scope (bound at line 723 `Ok (cipher, geom)`). Add `~cipher` to both:

```ocaml
Lwt.return_ok (make_btree_store ~cipher ~close_fn:close ~pager ~meta ~h ())
```

In `finish_wal_open` (~line 803), `cipher` is a labelled argument already in scope. Add `~cipher` to its `make_btree_store` call (~line 824):

```ocaml
       Lwt.return_ok
         (make_btree_store
            ~cipher
            ~wal:(Some wal)
            ~wal_close:(Some wal_close)
            ~close_fn:close
            ~pager
            ~meta
            ~h
            ())
```

- [ ] **Step 4: Build to verify it compiles**

Run: `PODMAN "dune build 2>&1 | tail -30"`
Expected: builds clean (only the pre-existing menhir parser warnings from `lib/sql/parser.mly`).

- [ ] **Step 5: Run the existing crypto suites to confirm no regression**

Run: `PODMAN "dune exec test/test_store_crypto.exe 2>&1 | tail -15 && dune exec test/test_wal_crypto.exe 2>&1 | tail -15"`
Expected: both end with `Test Successful`.

- [ ] **Step 6: Commit**

```bash
git add lib/store/store.ml
git commit -m "refactor(#84): retain page cipher in bt_state for copy/rekey

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: Make `copy_to` encryption-aware (#214)

**Files:**
- Modify: `lib/store/store.ml` (`copy_to`, lines 1989–2060)
- Modify: `lib/store/store.mli` (`copy_to` doc, ~line 299)
- Test: `test/test_online_backup.ml`; `test/dune`

- [ ] **Step 1: Add `mirage-crypto-rng` to the `test_online_backup` stanza**

In `test/dune`, the `test_online_backup` stanza (line 222) lists libraries. Add the two RNG libs so the test can seed the generator:

```
(test
 (name test_online_backup)
 (libraries
  sqlocaml
  sqlocaml.store
  sqlocaml.storage
  sqlocaml.unix
  alcotest
  cstruct
  lwt.unix
  unix
  mirage-crypto-rng
  mirage-crypto-rng.unix))
```

- [ ] **Step 2: Write the failing tests in `test/test_online_backup.ml`**

At the top of `test/test_online_backup.ml`, the file already `open`s Lwt.Syntax and defines `bs`, `run`, `fresh_path`, `cleanup`, `bytes_opt_eq`, `ok_store`, and `module UnixStore`. Add a 32-byte key helper and these test functions just before the final `let () = ... Alcotest.run` block:

```ocaml
let k32 c = String.make 32 c

(* #214: copy of an encrypted source must yield an encrypted destination
   that round-trips under the same key, leaks no plaintext, and refuses a
   missing/wrong key. *)
let test_copy_encrypted_round_trip () =
  let src_path = fresh_path "enc_src" in
  let dst_path = fresh_path "enc_dst" in
  cleanup src_path;
  cleanup dst_path;
  run
  @@ Lwt.finalize
       (fun () ->
          let key = k32 'a' in
          let* src_r = UnixStore.open_file ~key ~path:src_path () in
          let src = ok_store src_r in
          let* () =
            let* tx = S.rw_begin src in
            let* () = S.put tx 16 (bs "secretkey") (bs "PLAINTEXT_MARKER_42") in
            S.commit tx
          in
          let* cr = UnixStore.copy_to_file src ~dest:dst_path in
          (match cr with
           | Error e -> Alcotest.failf "copy_to_file error: %a" S.pp_error e
           | Ok () -> ());
          (* destination opens with the same key and round-trips *)
          let* dst_r = UnixStore.open_file ~key ~path:dst_path () in
          let dst = ok_store dst_r in
          let* got = S.with_ro dst (fun tx -> S.get tx 16 (bs "secretkey")) in
          Alcotest.check bytes_opt_eq "round-trip" (Some (bs "PLAINTEXT_MARKER_42")) got;
          let* () = S.close dst in
          let* () = S.close src in
          (* raw destination bytes must NOT contain the plaintext marker *)
          let ic = open_in_bin dst_path in
          let len = in_channel_length ic in
          let raw = really_input_string ic len in
          close_in ic;
          let contains hay needle =
            let nl = String.length needle and hl = String.length hay in
            let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
            nl > 0 && go 0
          in
          Alcotest.(check bool)
            "no plaintext marker on disk" false (contains raw "PLAINTEXT_MARKER_42");
          Lwt.return_unit)
       (fun () ->
          cleanup src_path;
          cleanup dst_path;
          Lwt.return_unit)
;;

let test_copy_encrypted_needs_key () =
  let src_path = fresh_path "enc_src2" in
  let dst_path = fresh_path "enc_dst2" in
  cleanup src_path;
  cleanup dst_path;
  run
  @@ Lwt.finalize
       (fun () ->
          let key = k32 'a' in
          let* src = UnixStore.open_file ~key ~path:src_path () in
          let src = ok_store src in
          let* () =
            let* tx = S.rw_begin src in
            let* () = S.put tx 16 (bs "k") (bs "v") in
            S.commit tx
          in
          let* _ = UnixStore.copy_to_file src ~dest:dst_path in
          let* () = S.close src in
          (* no key -> Encryption_key_required *)
          let* nr = UnixStore.open_file ~path:dst_path () in
          (match nr with
           | Error S.Encryption_key_required -> ()
           | Error e -> Alcotest.failf "expected key_required, got %a" S.pp_error e
           | Ok _ -> Alcotest.fail "expected key_required, got Ok");
          (* wrong key -> Encryption_key_mismatch *)
          let* wr = UnixStore.open_file ~key:(k32 'z') ~path:dst_path () in
          (match wr with
           | Error S.Encryption_key_mismatch -> ()
           | Error e -> Alcotest.failf "expected key_mismatch, got %a" S.pp_error e
           | Ok _ -> Alcotest.fail "expected key_mismatch, got Ok");
          Lwt.return_unit)
       (fun () ->
          cleanup src_path;
          cleanup dst_path;
          Lwt.return_unit)
;;
```

Register them in the `Alcotest.run` list (add to an existing or new group), e.g.:

```ocaml
; ( "encrypted_copy"
  , [ Alcotest.test_case "round_trip" `Quick test_copy_encrypted_round_trip
    ; Alcotest.test_case "needs_key" `Quick test_copy_encrypted_needs_key
    ] )
```

And ensure the RNG is seeded — add `Mirage_crypto_rng_unix.use_default ();` as the first line of the `let () =` block (immediately before `Alcotest.run`).

- [ ] **Step 3: Run the tests to verify they fail**

Run: `PODMAN "dune exec test/test_online_backup.exe 2>&1 | tail -40"`
Expected: FAIL — `round_trip` fails because the destination currently stores plaintext, so either the marker is found on disk (`no plaintext marker on disk` assertion) or the destination opened without re-encryption mismatches; `needs_key` fails because the plaintext destination opens fine without a key (gets `Ok` instead of `Encryption_key_required`).

- [ ] **Step 4: Extract the shared snapshot iterator and make `copy_to` encryption-aware**

Replace the body of `copy_to` (lines 1989–2060) so the page-iteration is a reusable helper. Insert `iter_snapshot_pages` just above `copy_to`, then rewrite `copy_to`:

```ocaml
(* Shared page-image iteration for copy_to/rekey_to: under an RO snapshot,
   yields each PLAINTEXT page (page_id, buf) for page_id in [0, n), resolving
   the WAL overlay (bounded to the snapshot horizon) before the main DB.  The
   buffer handed to [f] may be owned by the pager cache — callers that mutate
   it MUST copy first. *)
let iter_snapshot_pages st (Ro snap) ~(f : page_id:int64 -> page:Cstruct.t -> unit Lwt.t)
  : unit Lwt.t
  =
  let n = Pager.n_pages st.pager in
  let horizon = snap.rs_snap_frames in
  let rec loop (page_id : int64) =
    if Int64.compare page_id n >= 0
    then Lwt.return_unit
    else
      let* page_buf =
        match st.wal with
        | None ->
          let* r =
            Pager.read ~snapshot_frames:0 ~pin_set:snap.rs_pinned st.pager page_id
          in
          (match r with
           | Ok buf -> Lwt.return buf
           | Error e ->
             Lwt.fail_with
               (Format.asprintf "Store.iter_snapshot_pages(pg=%Ld): %a" page_id Pager.pp_error e))
        | Some wal ->
          (match Wal.find_page_at wal page_id ~max_frame:horizon with
           | Some idx ->
             let* r = Wal.read_frame wal idx in
             (match r with
              | Ok buf -> Lwt.return buf
              | Error e ->
                Lwt.fail_with
                  (Format.asprintf
                     "Store.iter_snapshot_pages(pg=%Ld,frame=%d): %a"
                     page_id idx Wal.pp_error e))
           | None ->
             let* r =
               Pager.read ~snapshot_frames:horizon ~pin_set:snap.rs_pinned st.pager page_id
             in
             (match r with
              | Ok buf -> Lwt.return buf
              | Error e ->
                Lwt.fail_with
                  (Format.asprintf "Store.iter_snapshot_pages(pg=%Ld): %a" page_id Pager.pp_error e)))
      in
      let* () = f ~page_id ~page:page_buf in
      loop (Int64.add page_id 1L)
  in
  loop 0L
;;

let copy_to (t : t) (sink : page_sink) : unit Lwt.t =
  match t.backend with
  | Mem _ -> Lwt.return_unit
  | Btree st ->
    with_ro t (fun ro ->
      iter_snapshot_pages st ro ~f:(fun ~page_id ~page ->
        match st.cipher with
        | Some c when Int64.compare page_id 2L >= 0 ->
          (* #214: re-encrypt data pages under the source's own key so the
             destination is a faithful encrypted DB and no user-data plaintext
             transits the sink.  Pages 0/1 are plaintext headers (canary), copied
             verbatim.  Copy first — [page] may be the pager's cached buffer. *)
          let tmp = Cstruct.create (Cstruct.length page) in
          Cstruct.blit page 0 tmp 0 (Cstruct.length page);
          Crypto.encrypt_page c ~page_id tmp;
          sink ~page_id ~page:tmp
        | _ -> sink ~page_id ~page))
;;
```

(Keep the existing doc comment block above `copy_to`. Delete the now-replaced old loop body.)

- [ ] **Step 5: Run the tests to verify they pass**

Run: `PODMAN "dune exec test/test_online_backup.exe 2>&1 | tail -40"`
Expected: PASS — all `test_online_backup` cases (the new `encrypted_copy` group plus the pre-existing plaintext copy cases) end with `Test Successful`.

- [ ] **Step 6: Update the `copy_to` doc-comment in `store.mli`**

In `lib/store/store.mli` at the `copy_to` declaration (~line 299), append a sentence:

```ocaml
(** One-shot consistent full copy via an RO snapshot + page sink (#93).
    When the source is encrypted (#84), data pages are re-encrypted under the
    source's key before reaching the sink, so the destination is a faithful,
    self-contained encrypted DB (open it with the same key); no user-data
    plaintext transits the sink. *)
val copy_to : t -> page_sink -> unit Lwt.t
```

(Preserve any existing wording; add the encryption sentence.)

- [ ] **Step 7: Build and commit**

Run: `PODMAN "dune build 2>&1 | tail -10"` (expect clean)

```bash
git add lib/store/store.ml lib/store/store.mli test/test_online_backup.ml test/dune
git commit -m "feat(#214): encryption-aware hot-copy — encrypted source yields encrypted dest

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: Add `Store.rekey_to` (#215 core)

**Files:**
- Modify: `lib/store/store.ml` (add `rekey_to` after `copy_to`)
- Modify: `lib/store/store.mli` (declare `rekey_to`)

- [ ] **Step 1: Implement `rekey_to` in `store.ml`**

Immediately after `copy_to`, add:

```ocaml
(* #215: offline key rotation.  [t] must have been opened WITH THE OLD KEY so
   reads decrypt to plaintext; every data page (>=2) is re-encrypted under a
   fresh cipher built from [new_key], and each header page (0,1) has its canary
   rewritten under the new key (txn parity and all other fields preserved) and
   its CRC resealed.  The sunk page image is a self-contained encrypted DB under
   [new_key] with no WAL.  Rejects a plaintext source ([Not_encrypted]) and a
   wrong-length key. *)
let rekey_to (t : t) ~(new_key : string) (sink : page_sink)
  : (unit, error) result Lwt.t
  =
  match t.backend with
  | Mem _ -> Lwt.return_ok ()
  | Btree st ->
    (match st.cipher with
     | None -> Lwt.return_error Not_encrypted
     | Some _old ->
       (match Crypto.create ~key:new_key with
        | Error `Bad_key_length ->
          Lwt.return_error (Block_error "encryption key must be 32 bytes")
        | Ok c' ->
          let nonce = Mirage_crypto_rng.generate Crypto.nonce_len in
          let canary_tag = Crypto.make_canary c' ~nonce in
          let* () =
            with_ro t (fun ro ->
              iter_snapshot_pages st ro ~f:(fun ~page_id ~page ->
                let len = Cstruct.length page in
                let tmp = Cstruct.create len in
                Cstruct.blit page 0 tmp 0 len;
                if Int64.compare page_id 2L < 0
                then (
                  (* header page: rewrite canary under the new key, reseal CRC *)
                  let f = Page.read_header_fields tmp in
                  Page.write_header_fields
                    tmp
                    { f with Page.canary_nonce = nonce; canary_tag };
                  Page.seal tmp;
                  sink ~page_id ~page:tmp)
                else (
                  Crypto.encrypt_page c' ~page_id tmp;
                  sink ~page_id ~page:tmp)))
          in
          Lwt.return_ok ()))
;;
```

Confirm `module Page = Sqlocaml_storage.Page` (or equivalent) is in scope at the top of `store.ml`; if not, add `module Page = Sqlocaml_storage.Page` near the other module aliases (line ~22, beside `module Crypto = Sqlocaml_storage.Crypto`).

- [ ] **Step 2: Declare `rekey_to` in `store.mli`**

Right after the `copy_to` declaration, add:

```ocaml
(** #215: offline key rotation.  [t] must have been opened with the OLD key.
    Reads every page as plaintext, re-encrypts data pages (>=2) under a fresh
    cipher built from [new_key], and rewrites the header canary under the new
    key, sinking a self-contained encrypted page image (no WAL).  Returns
    [Not_encrypted] if [t] is not an encrypted store, or a [Block_error] if
    [new_key] is not 32 bytes. *)
val rekey_to : t -> new_key:string -> page_sink -> (unit, error) result Lwt.t
```

- [ ] **Step 3: Build to verify it compiles**

Run: `PODMAN "dune build 2>&1 | tail -30"`
Expected: clean build (menhir warnings only). If `Page` is unbound, add the module alias from Step 1 and rebuild.

- [ ] **Step 4: Commit**

```bash
git add lib/store/store.ml lib/store/store.mli
git commit -m "feat(#215): Store.rekey_to — re-encrypt page image under a new key

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 4: Add `Sqlocaml_unix.Store.rotate_key_file` (#215 unix) + tests

**Files:**
- Modify: `lib/unix/store.ml` (add `rotate_key_file` after `copy_to_file`)
- Modify: `lib/unix/store.mli` (declare it)
- Create: `test/test_store_rekey.ml`
- Modify: `test/dune` (register the suite)

- [ ] **Step 1: Write the failing test suite `test/test_store_rekey.ml`**

```ocaml
(** #215: offline key rotation tests. *)

open Lwt.Syntax
module S = Sqlocaml_store.Store
module UnixStore = Sqlocaml_unix.Store

let bs s = Bytes.of_string s
let run = Lwt_main.run
let counter = ref 0

let fresh_path tag =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_rekey_%04d_%s.db" n tag
;;

let cleanup path =
  (try Unix.unlink path with _ -> ());
  (try Unix.unlink (path ^ "-wal") with _ -> ())
;;

let bytes_opt_eq =
  Alcotest.(option (testable (fun ppf b -> Format.fprintf ppf "%S" (Bytes.to_string b)) Bytes.equal))
;;

let ok_store : (S.t, S.error) result -> S.t = function
  | Ok t -> t
  | Error e -> Alcotest.failf "open error: %a" S.pp_error e
;;

let k32 c = String.make 32 c

(* rotate K1 -> K2: new key reads, old key fails, third key fails, no plaintext *)
let test_rotation_round_trip () =
  let src = fresh_path "src" in
  let dst = fresh_path "dst" in
  cleanup src;
  cleanup dst;
  run
  @@ Lwt.finalize
       (fun () ->
          let k1 = k32 '1' and k2 = k32 '2' and k3 = k32 '3' in
          let* s = UnixStore.open_file ~key:k1 ~path:src () in
          let s = ok_store s in
          let* () =
            let* tx = S.rw_begin s in
            let* () = S.put tx 16 (bs "kk") (bs "ROTATE_MARKER_99") in
            S.commit tx
          in
          let* () = S.close s in
          let* rr = UnixStore.rotate_key_file ~src_path:src ~old_key:k1 ~new_key:k2 ~dest:dst in
          (match rr with
           | Error e -> Alcotest.failf "rotate error: %a" S.pp_error e
           | Ok () -> ());
          (* new key round-trips *)
          let* d = UnixStore.open_file ~key:k2 ~path:dst () in
          let d = ok_store d in
          let* got = S.with_ro d (fun tx -> S.get tx 16 (bs "kk")) in
          Alcotest.check bytes_opt_eq "value under new key" (Some (bs "ROTATE_MARKER_99")) got;
          let* () = S.close d in
          (* old key now fails *)
          let* o = UnixStore.open_file ~key:k1 ~path:dst () in
          (match o with
           | Error S.Encryption_key_mismatch -> ()
           | Error e -> Alcotest.failf "expected mismatch under old key, got %a" S.pp_error e
           | Ok _ -> Alcotest.fail "old key should fail after rotation");
          (* unrelated third key fails *)
          let* o3 = UnixStore.open_file ~key:k3 ~path:dst () in
          (match o3 with
           | Error S.Encryption_key_mismatch -> ()
           | Error e -> Alcotest.failf "expected mismatch under k3, got %a" S.pp_error e
           | Ok _ -> Alcotest.fail "k3 should fail");
          (* no plaintext on disk *)
          let ic = open_in_bin dst in
          let raw = really_input_string ic (in_channel_length ic) in
          close_in ic;
          let contains hay needle =
            let nl = String.length needle and hl = String.length hay in
            let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
            nl > 0 && go 0
          in
          Alcotest.(check bool) "no plaintext on disk" false (contains raw "ROTATE_MARKER_99");
          Lwt.return_unit)
       (fun () -> cleanup src; cleanup dst; Lwt.return_unit)
;;

(* rekey_to rejects a plaintext source *)
let test_reject_plaintext () =
  let src = fresh_path "plain" in
  cleanup src;
  run
  @@ Lwt.finalize
       (fun () ->
          let* s = UnixStore.open_file ~path:src () in
          let s = ok_store s in
          let* () =
            let* tx = S.rw_begin s in
            let* () = S.put tx 16 (bs "k") (bs "v") in
            S.commit tx
          in
          let sink : S.page_sink = fun ~page_id:_ ~page:_ -> Lwt.return_unit in
          let* r = S.rekey_to s ~new_key:(k32 '2') sink in
          let* () = S.close s in
          (match r with
           | Error S.Not_encrypted -> ()
           | Error e -> Alcotest.failf "expected Not_encrypted, got %a" S.pp_error e
           | Ok () -> Alcotest.fail "expected Not_encrypted on plaintext source");
          Lwt.return_unit)
       (fun () -> cleanup src; Lwt.return_unit)
;;

(* rekey_to rejects a wrong-length new key *)
let test_reject_bad_key_len () =
  let src = fresh_path "enc" in
  cleanup src;
  run
  @@ Lwt.finalize
       (fun () ->
          let* s = UnixStore.open_file ~key:(k32 '1') ~path:src () in
          let s = ok_store s in
          let* () =
            let* tx = S.rw_begin s in
            let* () = S.put tx 16 (bs "k") (bs "v") in
            S.commit tx
          in
          let sink : S.page_sink = fun ~page_id:_ ~page:_ -> Lwt.return_unit in
          let* r = S.rekey_to s ~new_key:"too-short" sink in
          let* () = S.close s in
          (match r with
           | Error (S.Block_error _) -> ()
           | Error e -> Alcotest.failf "expected Block_error, got %a" S.pp_error e
           | Ok () -> Alcotest.fail "expected Block_error on bad key length");
          Lwt.return_unit)
       (fun () -> cleanup src; Lwt.return_unit)
;;

let () =
  Mirage_crypto_rng_unix.use_default ();
  Alcotest.run
    "store_rekey"
    [ ( "rotation"
      , [ Alcotest.test_case "round_trip" `Quick test_rotation_round_trip
        ; Alcotest.test_case "reject_plaintext" `Quick test_reject_plaintext
        ; Alcotest.test_case "reject_bad_key_len" `Quick test_reject_bad_key_len
        ] )
    ]
;;
```

- [ ] **Step 2: Register the suite in `test/dune`**

Add a new stanza (mirroring `test_online_backup`'s libs plus the RNG libs):

```
(test
 (name test_store_rekey)
 (libraries
  sqlocaml.store
  sqlocaml.storage
  sqlocaml.unix
  alcotest
  cstruct
  lwt.unix
  unix
  mirage-crypto-rng
  mirage-crypto-rng.unix))
```

- [ ] **Step 3: Run the suite to verify it fails to build/compile**

Run: `PODMAN "dune exec test/test_store_rekey.exe 2>&1 | tail -30"`
Expected: FAIL — `Unbound value UnixStore.rotate_key_file` (the function does not exist yet).

- [ ] **Step 4: Implement `rotate_key_file` in `lib/unix/store.ml`**

Add after `copy_to_file` (after line 321, before the `[@@@ai_disclosure ...]` attributes):

```ocaml
(** #215: offline key rotation.  Opens [src_path] with [old_key] (WAL-aware: if
    a [src_path ^ "-wal"] sidecar exists it opens the WAL form so any
    uncheckpointed committed frames are folded in), re-encrypts every page under
    [new_key] via {!Core.rekey_to}, and writes a self-contained encrypted file at
    [dest] (no WAL sidecar).  Crash-safe: writes [dest ^ ".tmp"], fsyncs, renames
    atomically, then fsyncs the directory.  A wrong [old_key] surfaces
    [Encryption_key_mismatch]; a plaintext source surfaces [Not_encrypted]. *)
let rotate_key_file ~src_path ~old_key ~new_key ~dest
  : (unit, Core.error) result Lwt.t
  =
  let open Lwt.Syntax in
  let has_wal = Sys.file_exists (src_path ^ "-wal") in
  let* src_r =
    if has_wal
    then open_file_wal ~key:old_key ~path:src_path ()
    else open_file ~key:old_key ~path:src_path ()
  in
  match src_r with
  | Error e -> Lwt.return_error e
  | Ok src ->
    let close_src () = Core.close src in
    let tmp = dest ^ ".tmp" in
    let n = Core.n_pages src in
    let page_size = (Core.geometry src).Geometry.page_size in
    let* fr = Unix_file.open_ ~path:tmp () in
    (match fr with
     | Error e ->
       let* () = close_src () in
       Lwt.return_error (Core.Block_error (Format.asprintf "%a" Unix_file.pp_error e))
     | Ok file ->
       Unix_file.set_page_size file page_size;
       let* rr =
         if Int64.compare n 0L > 0
         then Unix_file.resize file ~n_pages:n
         else Lwt.return (Ok ())
       in
       (match rr with
        | Error e ->
          let* _ = Unix_file.close file in
          let (_ : unit Lwt.t) = Lwt_unix.unlink tmp in
          let* () = close_src () in
          Lwt.return_error (Core.Block_error (Format.asprintf "%a" Unix_file.pp_error e))
        | Ok () ->
          let sink : Core.page_sink =
            fun ~page_id ~page ->
            let* r = Unix_file.write_page file ~page_id page in
            match r with
            | Ok () -> Lwt.return_unit
            | Error e ->
              Lwt.fail_with
                (Format.asprintf "rotate_key_file write pg=%Ld: %a" page_id Unix_file.pp_error e)
          in
          Lwt.catch
            (fun () ->
               let* rk = Core.rekey_to src ~new_key sink in
               match rk with
               | Error e ->
                 let* _ = Unix_file.close file in
                 let (_ : unit Lwt.t) = Lwt_unix.unlink tmp in
                 let* () = close_src () in
                 Lwt.return_error e
               | Ok () ->
                 let* sr = Unix_file.sync file in
                 (match sr with
                  | Error e ->
                    let* _ = Unix_file.close file in
                    let (_ : unit Lwt.t) = Lwt_unix.unlink tmp in
                    let* () = close_src () in
                    Lwt.return_error
                      (Core.Block_error (Format.asprintf "%a" Unix_file.pp_error e))
                  | Ok () ->
                    let* _ = Unix_file.close file in
                    let* () = Lwt_unix.rename tmp dest in
                    let dir_path = Filename.dirname dest in
                    let* dir_res =
                      Lwt.catch
                        (fun () ->
                           let* dir_fd = Lwt_unix.openfile dir_path [ Unix.O_RDONLY ] 0 in
                           let* () = Lwt_unix.fsync dir_fd in
                           let* () = Lwt_unix.close dir_fd in
                           Lwt.return_ok ())
                        (fun exn ->
                           Lwt.return_error
                             (Core.Block_error
                                (Printf.sprintf "dir fsync after rename: %s" (Printexc.to_string exn))))
                    in
                    let* () = close_src () in
                    Lwt.return dir_res))
            (fun exn ->
               let* _ = Unix_file.close file in
               let (_ : unit Lwt.t) = Lwt_unix.unlink tmp in
               let* () = close_src () in
               Lwt.return_error (Core.Block_error (Printexc.to_string exn)))))
;;
```

- [ ] **Step 5: Declare `rotate_key_file` in `lib/unix/store.mli`**

After the `copy_to_file` declaration (~line 52), add:

```ocaml
(** [rotate_key_file ~src_path ~old_key ~new_key ~dest] offline-rotates the
    encryption key of the database at [src_path] (opened with [old_key]),
    writing a new self-contained file at [dest] encrypted under [new_key] (#215).
    Crash-safe via a temp file + atomic rename + directory fsync.  WAL-aware: an
    existing [src_path ^ "-wal"] sidecar is folded in.  Returns
    [Encryption_key_mismatch] for a wrong [old_key], [Not_encrypted] if the
    source is not encrypted, or a [Block_error] for I/O / bad key length. *)
val rotate_key_file
  :  src_path:string
  -> old_key:string
  -> new_key:string
  -> dest:string
  -> (unit, Core.error) result Lwt.t
```

- [ ] **Step 6: Run the suite to verify it passes**

Run: `PODMAN "dune exec test/test_store_rekey.exe 2>&1 | tail -40"`
Expected: PASS — `Test Successful. 3 tests run.`

- [ ] **Step 7: Build everything and commit**

Run: `PODMAN "dune build 2>&1 | tail -10"` (expect clean)

```bash
git add lib/unix/store.ml lib/unix/store.mli test/test_store_rekey.ml test/dune
git commit -m "feat(#215): rotate_key_file — crash-safe offline key rotation

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 5: Full suite, docs, and finish

**Files:**
- Modify: `docs/specs/2026-06-01-encryption-at-rest-design.md` (mark #214/#215 resolved in scope notes)

- [ ] **Step 1: Run the full test suite**

Run: `PODMAN "dune runtest 2>&1 | tail -40"`
Expected: exit 0, every suite `Test Successful`, no `[FAIL]`.

- [ ] **Step 2: Check formatting (host-side, per repo convention)**

The repo formats via ocamlformat. Verify changed files are clean:
Run: `PODMAN "dune build @fmt 2>&1 | tail -30"`
If it reports diffs, apply them: `PODMAN "dune build @fmt --auto-promote 2>&1 | tail -10"` then rebuild. Commit any formatting changes.

- [ ] **Step 3: Update the parent #84 spec scope notes**

In `docs/specs/2026-06-01-encryption-at-rest-design.md`, the "Scope notes" section lists `copy_to` and Key rotation as deferred. Append resolution pointers:

- `copy_to` bullet: add `Resolved in #214 (approach a): copy_to re-encrypts under the source key; see docs/specs/2026-06-01-encryption-copy-rekey-design.md.`
- Key rotation bullet: add `Resolved in #215: Store.rekey_to + rotate_key_file; see the same design doc.`

- [ ] **Step 4: Commit docs**

```bash
git add docs/specs/2026-06-01-encryption-at-rest-design.md
git commit -m "docs(#214,#215): mark hot-copy + rotation follow-ups resolved

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

- [ ] **Step 5: Push and open the PR for review**

```bash
git push -u origin HEAD
```
Then open a PR via the Forgejo CLI (`~/.local/bin/forgejo`) targeting `main`, titled `feat(#214,#215): encryption-aware hot-copy & offline key rotation`, body summarizing both fixes, the design-doc link, the test matrix, and `Closes #214`, `Closes #215`.

---

## Self-Review notes (addressed)

- **Spec coverage:** #214 (Task 1–2), #215 core `rekey_to` (Task 3), `rotate_key_file` (Task 4), WAL-aware source open (Task 4 Step 4), all test cases from the spec testing section (Task 2 Step 2, Task 4 Step 1), docs (Task 5).
- **Type consistency:** `cipher : Crypto.t option` used identically in bt_state, `make_btree_store`, `copy_to`, `rekey_to`. `page_sink = page_id:int64 -> page:Cstruct.t -> unit Lwt.t` matches all sinks. `Page.header_fields` field names (`canary_nonce`, `canary_tag`, `enc_magic`) match `lib/storage/page.mli`.
- **Leak guards:** both #214 and #215 assert the known marker is absent from raw destination bytes.
- **Buffer ownership:** every mutation (`encrypt_page`, header reseal) is on a fresh `Cstruct.create` + `blit` copy, never the pager's cached buffer.
