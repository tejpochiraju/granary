(lang dune 3.16)
(name granary)
(using menhir 2.1)
(generate_opam_files true)
(version 0.0.1)
(source
 (uri "git+https://github.com/tejpochiraju/granary.git"))

(package
 (name granary)
 (synopsis "Pure-OCaml SQL engine")
 (description "Pure-OCaml SQL engine targeting MirageOS unikernels.")
 (depends
  (ocaml (>= 5.4))
  (lwt (>= 5.7))
  (cstruct (>= 6.2))
  (menhir (>= 20240715))
  (alcotest :with-test)
  (qcheck-alcotest :with-test)
  (mirage-block (>= "3.0.0"))
  (mirage-block-unix (>= "2.14.0"))
  (mirage-crypto (>= "1.2.0"))
  (mirage-crypto-rng (>= "1.2.0"))
  (alcotest-lwt :with-test)))

(package
 (name granary_repl)
 (synopsis "TUI REPL for granary")
 (depends
  (ocaml (>= 5.4))
  granary
  (lwt (>= 5.7))
  (menhir (>= "20240715"))
  (lwd (>= 0.5))
  (nottui (>= 0.5))
  (nottui-lwt (>= 0.5))
  (lwt_ppx :build)))
