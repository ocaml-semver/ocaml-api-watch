(lang dune 2.0)

(name api-watch)

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
  (ocaml (= 4.12.0))
  logs
  (cmdliner (< 1.1.0))))
