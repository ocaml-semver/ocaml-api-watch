This file makes the basic test suite for api-diff.

Each test starts from two `.mli` files that are generated on the fly and then
compiled into `.cmi` files using ocamlc.
api-diff is then run on the two files so we can actually ensure the output
is the expected one.

It is also possible to reuse the ref.cmi file from the first test as a baseline
and simply generate a modified version of it.


## Identical .cmi files:

  $ cat > ref.mli << EOF
  > type t = int
  > type unused_type = string
  > val f : t -> string
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

And now we run api-watcher on that same cmi file as both arguments,
there should be no diff:

  $ api-diff ref.cmi ref.cmi
  API unchanged!

## Different .cmi files for type tests:

Generate a basic `.mli` files with two types and a function

### A file with an additional type:

  $ cat > add_type.mli <<EOF
  > type t = int
  > type unused_type = string
  > type added_t = float
  > val f : t -> string
  > EOF

We generate the .cmi file

  $ ocamlc add_type.mli

Run api-watcher on the two cmi files, there should be a difference

  $ api-diff ref.cmi add_type.cmi
  API changed!

### A file with a removed type:

  $ cat > remove_type.mli <<EOF
  > type t = int
  > val f : t -> string
  > EOF

We generate the .cmi file

  $ ocamlc remove_type.mli

Run api-watcher on the two cmi files, there should be a difference

  $ api-diff ref.cmi remove_type.cmi
  API changed!

### A file with a modified type:

  $ cat > modify_type.mli <<EOF
  > type t = float
  > type unused_type = string
  > val f : t -> string
  > EOF

We generate a .cmi file

  $ ocamlc modify_type.mli

Run api-watcher on the two cmi files, there should be a difference

  $ api-diff ref.cmi modify_type.cmi
  API changed!

## Different .cmi files for value tests:

### Adding a value:

Generate a new .mli file with an additional value
  $ cat > add_value.mli << EOF
  > type t = int
  > type unused_type = string
  > val f : t -> string
  > val g : t -> t
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc add_value.mli

Run api-diff and check the output
  $ api-diff ref.cmi add_value.cmi
  API changed!

### Removing a value:

Generate a new .mli file with the value removed
  $ cat > remove_value.mli << EOF
  > type t = int
  > type unused_type = string
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc remove_value.mli

Run api-diff and check the output
  $ api-diff ref.cmi remove_value.cmi
  API changed!

### Modifying a value:

Generate a new .mli file with the value modified
  $ cat > modify_value.mli << EOF
  > type t = int
  > type unused_type = string
  > val f : t -> t
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc modify_value.mli

Run api-diff and check the output
  $ api-diff ref.cmi modify_value.cmi
  API changed!
