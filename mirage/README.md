# Sample granary MirageOS unikernel (#403)

A minimal, in-tree unikernel that runs the granary engine over a
`Mirage_block` device. It is the **amd64 baseline** the aarch64 audit (#402)
cross-builds. The engine is 100 % OCaml with an explicitly byte-ordered on-disk
format and no C stubs, so the only architecture-relevant code is the WAL
fsync / commit path — which this unikernel exercises end-to-end.

## What it does

[`unikernel.ml`](unikernel.ml) wraps the supplied `Mirage_block.S` device with
[`Granary_mirage_block.Mirage_backend`](../lib/block/mirage_backend.ml), opens a
`Store` in **WAL mode** via `Store.open_block_wal`, wraps it as a `Db.t`, and
runs the shared workload [`Granary_sample.Sample.run_demo`](../lib/sample/sample.ml):

```
CREATE TABLE kv (id INTEGER PRIMARY KEY, name TEXT NOT NULL);
BEGIN; INSERT × 3; COMMIT;        -- drives the WAL fsync / commit path
SELECT id, name FROM kv ORDER BY id;
```

It logs e.g. `granary demo OK: read back 3 rows; WAL fsyncs=2`.

The exact same `Mirage_backend ↔ Store.open_block_wal` wiring and workload run as
a host unit test — [`test/test_mirage_unikernel_smoke.ml`](../test/test_mirage_unikernel_smoke.ml)
— in the normal `dune test` gate, so the wiring is guarded against bit-rot
without needing the mirage toolchain.

> **WAL backing.** The main DB lives on the real block device. The WAL needs
> positioned *byte* I/O at non-sector-aligned offsets, so it cannot ride a
> page-addressed `Mirage_block` device directly; this sample keeps it in an
> in-memory, lazily-grown buffer. The commit path (frame serialization +
> `wal_sync`) is still fully exercised — only WAL durability across reboots is
> out of scope for the sample. Putting the WAL on a second block device via a
> byte-over-sector shim is a possible follow-up.

## Build toolchain

The `mirage` CLI (`mirage.4.11.0`) and the Solo5/`ocaml-solo5` toolchain are
**not** in the everyday `granary-dev` image (they pull the mirage runtime +
opam-monorepo). Two options:

### Option A — dedicated image (recommended)

[`../Containerfile.mirage`](../Containerfile.mirage) extends `granary-dev` with
the mirage CLI and the Solo5 build tooling:

```sh
podman build -t granary-mirage -f Containerfile.mirage .
```

### Option B — on-demand opam install

In any OCaml 5.4 switch that already has the engine's deps:

```sh
opam install mirage.4.11.0
sudo apt-get install -y gcc make m4 libseccomp-dev   # Solo5 build deps (for -t hvt)
```

## Build & run

`granary` is an unpublished local package, so opam-monorepo vendors it from the
local pin — which it can only do from a **real git checkout** (a `git worktree`
will not work, because opam-monorepo runs `git ls-remote` on the pin and a
worktree's `.git` is a file pointing outside the build sandbox). From a normal
clone with your changes committed:

```sh
# from the repo root, inside the granary-mirage image:
opam pin add -yn granary .          # pin the local engine (vendored, not installed)
cd mirage

# unix target — genuinely runnable on amd64:
mirage configure -t unix
make depends                          # opam-monorepo lock + pull (vendors granary)
make build                            # -> dist/granary-demo
rm -f disk && truncate -s 8M disk     # the `block_of_file "disk"` backend; the
                                      # in-memory WAL is not durable across runs,
                                      # so start each run from a fresh disk
./dist/granary-demo --logs='*:info'  # prints the demo result, exits 0

# hvt (Solo5) target — configures + builds; running needs a hypervisor:
mirage clean
mirage configure -t hvt
make depends
make build                            # -> dist/granary-demo.hvt
# run (optional, needs the solo5-hvt tender + KVM):
#   solo5-hvt --block:disk=disk dist/granary-demo.hvt
```

`mirage clean` removes the generated build files. They are all gitignored — only
`config.ml`, `unikernel.ml`, and this README are tracked.

## aarch64 (#402)

The unikernel was audited on **aarch64** — the engine is 100 % OCaml with a
byte-ordered on-disk format and no C stubs, so this is toolchain/packaging
verification, not engine work. There is no native arm64 host here, so the audit
ran under qemu-user emulation (the binfmt handlers from #157).

Build the mirage image from the arm64 dev base, then build as above inside it:

```sh
podman build --platform=linux/arm64 \
  --build-arg BASE=localhost/granary-dev:arm64 \
  -t granary-mirage:arm64 -f Containerfile.mirage .
podman run --rm --platform=linux/arm64 -v "$PWD:/workspace:z" -w /workspace \
  granary-mirage:arm64 sh -c '...'   # same opam pin + mirage configure/build steps
```

**Result (both targets pass, no quirks):**

- `mirage v4.11.0` installs and runs on the aarch64 switch.
- `-t unix`: builds (native aarch64 ELF) and **runs** —
  `read back 3 rows; WAL fsyncs=2`. This clears the #157 risk note: the
  `mirage-block-unix` WAL fsync syscalls are arch-neutral on arm64.
- `-t hvt`: the `ocaml-solo5` aarch64 cross toolchain installs and cross-compiles
  cleanly; `dist/granary-demo.hvt` is a valid Solo5 image
  (ELF `e_machine = 0xB7` = `EM_AARCH64`). Running it needs an arm64
  hypervisor (`solo5-hvt` + arm64 KVM), out of scope for the x86 host here.

> Emulated arm64 mirage-CLI builds are slow (~45–60 min for the full
> duniverse/Solo5 compile), so there is no scheduled arm64 *unikernel* CI leg.
> The arm64 WAL-fsync wiring is still guarded weekly: `cross-arch.yml` runs the
> full `dune runtest` (including `test_mirage_unikernel_smoke`) on arm64.

## CI

- The host smoke test runs in the normal `dune test` gate (`ci.yml`).
- [`.forgejo/workflows/mirage.yml`](../.forgejo/workflows/mirage.yml) runs
  `mirage configure -t unix && make depends && make build` (and runs the unix
  unikernel) on a schedule / on demand, so the unikernel target does not
  silently bit-rot.
