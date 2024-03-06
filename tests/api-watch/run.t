This file makes the basic test suite for api-diff.

Each test starts from two `.mli` files that are generated on the fly and then
compiled into `.cmi` files using ocamlc.
api-diff is then run on the two files so we can actually ensure the output
is the expected one.

It is also possible to reuse the ref.cmi file from the first test as a baseline
and simply generate a modified version of it.


## Identical .cmi files:

Here we generate a basic `.mli` file with a type and a function:

  $ cat > ref.mli << EOF
  > type t = int
  > 
  > val f : t -> string
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

And now we run api-watcher on that same cmi file as both arguments,
there should be no diff:

  $ api-diff ref.cmi ref.cmi
  API unchanged!

Adding a value:

Generate a new .mli file with an additional value
  $ cat > add_value.mli << EOF
  > type t = int
  > 
  > val f : t -> string
  > val g : t -> t
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc add_value.mli

Run api-diff and check the output
  $ api-diff ref.cmi add_value.cmi

Removing a value:

Generate a new .mli file with the value removed
  $ cat > remove_value.mli << EOF
  > type t = int
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc remove_value.mli

Run api-diff and check the output
  $ api-diff ref.cmi remove_value.cmi

Modifying a value:

Generate a new .mli file with the value modified
  $ cat > modify_value.mli << EOF
  > type t = int
  > 
  > val f : t -> t
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc modify_value.mli

Run api-diff and check the output
  $ api-diff ref.cmi modify_value.cmi