Here we generate a file for testing the expansion of alias types

# A `.mli` file with an alias type

  $ cat > ref_alias.mli << EOF
  > type t = int
  > type u = t
  > EOF

We generate the .cmi file

  $ ocamlc ref_alias.mli

### Changing the alias type

  $ cat > cur_alias.mli << EOF
  > type t = float
  > type u = t
  > EOF

We generate the .cmi file

  $ ocamlc cur_alias.mli

Run the api-watcher on the two cmi files, type u should expand

  $ api-diff ref_alias.cmi cur_alias.cmi
  diff module Cur_alias:
  -type t = int
  +type t = float
  -type u = int
  +type u = float
  
  [1]

Here we generate a file for testing the unexpanding of concrete types

# A `.mli` file with a record type

  $ cat > ref_record.mli << EOF
  > type t = { a : int }
  > type u = t
  > EOF

We generate the .cmi file

  $ ocamlc ref_record.mli

### Changing the record type

  $ cat > cur_record.mli << EOF
  > type t = { a : float }
  > type u = t
  > EOF

We generate the .cmi file

  $ ocamlc cur_record.mli

Run the api-watcher on the two cmi files, type u should not expand

  $ api-diff ref_record.cmi cur_record.cmi
  diff module Cur_record:
   type t =
  -  { a : int; }
  +  { a : float; }
  
  [1]
