(lang dune 2.7)

(name api-watch)

(cram enable)
(generate_opam_files true)

(license ISC)
(authors "Nathan Rebours <nathan.p.rebours@gmail.com>")
(maintainers "Nathan Rebours <nathan.p.rebours@gmail.com>")
(source
 (github NathanReb/ocaml-api-watch))

(package
 (name api-watch)
 (synopsis "Libraries and tools to keep watch on your OCaml lib's API changes")
 (depends
  (ocaml (and (>= 4.14.0) (< 5.0.0)))
  (ppx_expect (and (>= v0.14.2) :with-test))
  logs
  (cmdliner (>= 1.1.0))))
