# Bench image for #222: adds in-process SQLite bindings to the dev toolchain.
# Layering on granary-dev pins ONE libsqlite3 version across both hosts (fair).
FROM localhost/granary-dev:latest

# apt needs root; the base image's default user is `opam`.
# libsqlite3-dev: headers/lib the in-process bindings link against (the reference).
# sqlite3: the CLI, only so the linked library version is trivially inspectable;
# both come from the same apt repo so they share ONE pinned version.
USER root
RUN apt-get update \
 && apt-get install -y --no-install-recommends libsqlite3-dev sqlite3 \
 && rm -rf /var/lib/apt/lists/*

# The opam switch is owned by the `opam` user, so install the bindings as that
# user. OPAMYES makes this non-interactive; pin nothing — take what the base
# repo offers.
USER opam
RUN opam install -y sqlite3 \
 && opam clean -y || true
