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
  -type t =
  - {
  -   a : int;
  -   b : float;
  - }
  +type t =
  + | A of int
  + | B of string
  
  [1]

Run the api-watcher on record and abstract type kinds cmi files

  $ api-diff ref_record_kind.cmi ref_abstract_kind.cmi
  diff module Ref_abstract_kind:
  -type t =
  - {
  -   a : int;
  -   b : float;
  - }
  +type t
  
  [1]

Run the api-watcher on record and open type kinds cmi files

  $ api-diff ref_record_kind.cmi ref_open_kind.cmi
  diff module Ref_open_kind:
  -type t =
  - {
  -   a : int;
  -   b : float;
  - }
  +type t =
  + ..
  
  [1]

Run the api-watcher on two abstract type kinds cmi files

  $ api-diff ref_abstract_kind.cmi ref_abstract_kind.cmi

Run the api-watcher on two open type kinds cmi files

  $ api-diff ref_open_kind.cmi ref_open_kind.cmi
