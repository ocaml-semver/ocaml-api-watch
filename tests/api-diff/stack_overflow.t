Here we generate a basic `.mli` file with two types and a function:
Referencing a paramterized type should cause a stackoverflow

  $ cat > ref.mli << EOF
  > type 'a t
  > val f : unit -> 'a t
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

and the current, unmodified version:

  $ cat > curr.mli << EOF
  > type 'a t
  > val f : unit -> 'a t
  > EOF

We generate the .cmi file

  $ ocamlc curr.mli
Run api-watcher on the two cmi files

  $ api-diff ref.cmi curr.cmi
