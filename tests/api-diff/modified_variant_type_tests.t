Here we generate a `.mli` file with a variant type:

  $ cat > ref.mli << EOF
  > type rank = Ace | King | Queen | Number of int
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

# Tests for different kind of modifications to a variant type:

### Adding a constructor to a variant type:

  $ cat > add_constructor.mli << EOF
  > type rank = Ace | King | Queen | Jack | Number of int
  > EOF

We generate the .cmi file

  $ ocamlc add_constructor.mli 

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi add_constructor.cmi
  diff module Add_constructor:
  type rank = 
  ...
  + | Jack

  [1]

### Removing a constructor from a variant type:

  $ cat > remove_constructor.cmi << EOF
  > type rank = Ace | King | Queen
  > EOF

We generate the .cmi file

  $ ocamlc remove_constructor.mli

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi remove_constructor.cmi
  diff module Remove_constructor:
  type rank =
  ...
  - | Number of int

  [1]

### Modifying a constructor's type in a variant type: 

  $ cat > modify_constructor_type.mli << EOF
  > type rank = Ace | King | Queen | Number of float
  > EOF

We generate the .cmi file

  $ ocamlc modify_constructor_type.mli

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi modify_constructor_type.cmi
  diff module Modify_constructor_type:
  type rank = 
  ...
  - | Number of int
  + | Number of float

  [1]
