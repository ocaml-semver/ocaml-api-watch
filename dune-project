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
  (ocaml (and (>= 4.14.0) (< 5.2.0)))
  logs
  (cmdliner (>= 1.1.0))))
