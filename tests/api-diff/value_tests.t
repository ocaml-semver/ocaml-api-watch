Here we generate a basic `.mli` file with two types and a function:

  $ cat > ref.mli << EOF
  > type t = int
  > type unused_type = string
  > val f : t -> string
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

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
  diff module Add_value:
  +val g : t -> t
  
  [1]

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
  diff module Remove_value:
  -val f : t -> string
  
  [1]

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
  diff module Modify_value:
  -val f : t -> string
  +val f : t -> int
  
  [1]

### Value referencing an abstract type

api-diff should be able to tell two, non alias types from both versions of the
API should be considered equal when referenced by a value.

Generate a reference .mli file:
  $ cat > value_with_abstract_type_ref.mli << EOF
  > type t = { a : int }
  > val x : t
  > EOF

and the current .mli file, (identical):
  $ cat > value_with_abstract_type_cur.mli << EOF
  > type t = { a : float }
  > val x : t
  > EOF

Let's compile both interfaces:
  $ ocamlc value_with_abstract_type_ref.mli
  $ ocamlc value_with_abstract_type_cur.mli

and run the tool, it should report no diff:
  $ api-diff value_with_abstract_type_ref.cmi value_with_abstract_type_cur.cmi
  diff module Value_with_abstract_type_cur:
   type t =
  -  { a : int; }
  +  { a : float; }
  
  [1]
