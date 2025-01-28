Here we generate `.mli` files for testing the changes in the type kinds

# A .mli file with a record type

  $ cat > ref_record_kind.mli << EOF
  > type t = { a: int; b: float }
  > EOF

We generate the .cmi file

  $ ocamlc ref_record_kind.mli

### A file with a variant type:

  $ cat > cur_variant_kind.mli << EOF
  > type t = A of int | B of string
  > EOF

We generate the .cmi file

  $ ocamlc cur_variant_kind.mli

Run the api-watcher on the two cmi files

  $ api-diff ref_record_kind.cmi cur_variant_kind.cmi
  diff module Cur_variant_kind:
  -type t = { a : int; ; b : float; }
  +type t = A of int | B of string

  [1]

### A file with an abstract type:

  $ cat > cur_abstract_kind.mli << EOF
  > type t
  > EOF

We generate the .cmi file

  $ ocamlc cur_abstract_kind.mli

Run the api-watcher on the two cmi files

  $ api-diff ref_record_kind.cmi cur_abstract_kind.cmi
  diff module Cur_abstract_kind:
  -type t = { a : int; b : float }
  +type t

  [1]

### A file with an open type:

  $ cat > cur_open_kind.mli << EOF
  > type t = ..
  > EOF

We generate the .cmi file

  $ ocamlc cur_open_kind.mli

Run the api-watcher on the two cmi files

  $ api-diff ref_record_kind.cmi cur_open_kind.cmi
  diff module Cur_open_kind:
  -type t = { a : int; b : float }
  +type t = ..

  [1]