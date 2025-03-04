Here we generate files for testing the changes in the type kinds

# A `.mli` file with a record type

  $ cat > ref_record_kind.mli << EOF
  > type t = { a: int; b: float }
  > EOF

We generate the .cmi file

  $ ocamlc ref_record_kind.mli

### A `.mli` file with a variant type:

  $ cat > ref_variant_kind.mli << EOF
  > type t = A of int | B of string
  > EOF

We generate the .cmi file

  $ ocamlc ref_variant_kind.mli

### A `.mli` file with an abstract type:

  $ cat > ref_abstract_kind.mli << EOF
  > type t
  > EOF

We generate the .cmi file

  $ ocamlc ref_abstract_kind.mli

### A `.mli` file with an open type:

  $ cat > ref_open_kind.mli << EOF
  > type t = ..
  > EOF

We generate the .cmi file

  $ ocamlc ref_open_kind.mli

Run the api-watcher on record and varient type kinds cmi files

  $ api-diff ref_record_kind.cmi ref_variant_kind.cmi
  diff module Ref_variant_kind:
   type t =
  -  { a : int; b : float; }
  +  | A of int
  +  | B of string
  
  [1]

Run the api-watcher on record and abstract type kinds cmi files

  $ api-diff ref_record_kind.cmi ref_abstract_kind.cmi
  diff module Ref_abstract_kind:
  -type t =
  +type t
  -  { a : int; b : float; }
  
  [1]

Run the api-watcher on record and open type kinds cmi files

  $ api-diff ref_record_kind.cmi ref_open_kind.cmi
  diff module Ref_open_kind:
   type t =
  -  { a : int; b : float; }
  +  ..
  
  [1]

Run the api-watcher on two abstract type kinds cmi files

  $ api-diff ref_abstract_kind.cmi ref_abstract_kind.cmi

Run the api-watcher on two open type kinds cmi files

  $ api-diff ref_open_kind.cmi ref_open_kind.cmi

Here we generate a `.mli` file with a private type abbreviation

  $ cat > recursive.mli << EOF
  > type 'a lst = Nil | Cons of 'a * 'a lst
  > EOF

We generate the .cmi file

  $ ocamlc recursive.mli

# Removing a private type abbreviation from a type declaration

  $ cat > add_param.mli << EOF
  > type 'a lst = Nil | Cons of 'a * 'a lst
  > EOF

We generate the .cmi file

  $ ocamlc add_param.mli

Run the api-watcher on the two cmi files

  $ api-diff recursive.cmi add_param.cmi
