FROM docker.io/ocaml/opam:ubuntu-24.04-ocaml-5.4
USER root
RUN apt-get update && apt-get install -y pkg-config libgmp-dev
USER opam
RUN opam install -y lwt cstruct menhir alcotest qcheck-alcotest lwt_ppx mirage-block mirage-block-unix uutf uucp
WORKDIR /workspace
ENTRYPOINT ["opam", "exec", "--"]
