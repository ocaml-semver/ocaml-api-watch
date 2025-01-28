Here we generate a `.mli` file with a private type abbreviation

  $ cat > ref.mli << EOF
  > type t = private int
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

# Removing a private type abbreviation from a type declaration

  $ cat > remove_private.mli << EOF
  > type t = int
  > EOF

We generate the .cmi file

  $ ocamlc remove_private.mli

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi remove_private.cmi
  diff module Remove_private:
  type t = -private int