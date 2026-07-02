# sqlocaml dev image. The ocaml/opam base tag is published multi-arch, so this
# Containerfile builds unchanged on both linux/amd64 and linux/arm64 (#157):
#
#   podman build --platform=linux/amd64 -t sqlocaml-dev:amd64 -f Containerfile .
#   podman build --platform=linux/arm64 -t sqlocaml-dev:arm64 -f Containerfile .
#
# Building/running a foreign arch on an x86 host needs the qemu binfmt handlers
# (Debian/Ubuntu: `sudo apt-get install -y qemu-user-static binfmt-support`).
# Note: `_build/` artefacts are arch-specific — `rm -rf _build` when switching
# the build host between architectures on the same checkout. See README.
#
# The `--platform` pin below is load-bearing (#426): the runner host shares a
# single rootless podman store, and the weekly cross-arch.yml job pulls this same
# base tag as linux/arm64. An *unpinned* `FROM` would then resolve to that cached
# arm64 image on an amd64 host and die at the first `RUN` with "Exec format
# error". `${TARGETPLATFORM:-linux/amd64}` defaults to amd64 when no `--platform`
# flag is passed, and buildah/podman override it when you *do* pass one — so the
# documented arm64 build above still works, while an unpinned build is safe.
# Repo-wide convention: every multi-arch base `FROM` pins its platform this way.
# The `ARG TARGETPLATFORM` line is required — without it buildah/podman does not
# expand TARGETPLATFORM at `FROM` time, so an explicit `--platform` flag would be
# silently ignored and the arm64 build above would produce an amd64 image.
ARG TARGETPLATFORM
FROM --platform=${TARGETPLATFORM:-linux/amd64} docker.io/ocaml/opam:ubuntu-24.04-ocaml-5.4
USER root
RUN apt-get update && apt-get install -y pkg-config libgmp-dev
USER opam
RUN opam install -y lwt cstruct menhir alcotest qcheck-alcotest lwt_ppx mirage-block mirage-block-unix uutf uucp mirage-crypto mirage-crypto-rng alcotest-lwt nottui nottui-lwt lwd
# merlint linter (#153): git-only, no opam release; pin the known-good commit.
RUN opam pin add -y -k git merlint "https://github.com/samoht/merlint.git#d5548822dea1ad3b845eabd4b569946a05e42423" \
 && opam install -y merlint
# ocamlformat (#171): exact pin — the .ocamlformat `version` field must equal this.
RUN opam install -y ocamlformat.0.29.0
WORKDIR /workspace
ENTRYPOINT ["opam", "exec", "--"]
