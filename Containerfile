FROM docker.io/ocaml/opam:ubuntu-24.04-ocaml-5.4
USER root
RUN apt-get update && apt-get install -y pkg-config libgmp-dev
USER opam
RUN opam install -y lwt cstruct menhir alcotest qcheck-alcotest lwt_ppx mirage-block mirage-block-unix uutf uucp
# merlint linter (#153): git-only, no opam release; pin the known-good commit.
RUN opam pin add -y -k git merlint "https://github.com/samoht/merlint.git#d5548822dea1ad3b845eabd4b569946a05e42423" \
 && opam install -y merlint
# ocamlformat (#171): exact pin — the .ocamlformat `version` field must equal this.
RUN opam install -y ocamlformat.0.29.0
WORKDIR /workspace
ENTRYPOINT ["opam", "exec", "--"]
