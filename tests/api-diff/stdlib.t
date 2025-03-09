Here we generate a `.mli` file with a type alias referencing a standard library type

  $ cat > ref.mli << EOF
  > type t = String.t
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

# A type that references the same type in the standard library

  $ cat > cur.mli << EOF
  > type t = string
  > EOF

We generate the .cmi file

  $ ocamlc cur.mli

Run the api-watcher on the two cmi files, there should be no diff

  $ api-diff ref.cmi cur.cmi
