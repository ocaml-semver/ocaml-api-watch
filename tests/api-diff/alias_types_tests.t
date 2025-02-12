Here we generate a basic `.mli` file with an alias type, and a value referencing it:

  $ cat > ref.mli << EOF
  > type t = int
  > val x : t
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

# Modifying the alias type

  $ cat > cur.mli << EOF
  > type t = float
  > val x : t
  > EOF

We generate the .cmi file

  $ ocamlc cur.mli

Run api-watcher on the two cmi files, there should be two diffs:

  $ api-diff ref.cmi cur.cmi
  diff module Cur:
  -type t = int
  +type t = float
  -val x : int
  +val x : float
  
  [1]
