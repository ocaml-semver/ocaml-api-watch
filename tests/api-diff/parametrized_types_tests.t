Here we generate a `.mli` file with a single type variable:

  $ cat > ref.mli << EOF
  > type ('a, 'b) t = { a: 'a; b: 'b }
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

# Renaming a type variable consistently across the type declaration:

  $ cat > rename_type_vars.mli << EOF
  > type ('c, 'd) t = { a : 'c; b: 'd }
  > EOF

We generate the .cmi file

  $ ocamlc rename_type_vars.mli

Run the api-watcher on the two cmi files, there should be no diff

  $ api-diff ref.cmi rename_type_vars.cmi

# Adding a type variable to a type declaration:

  $ cat > add_type_var.mli << EOF
  > type ('a, 'b, 'c) t = { a : 'a; b: 'b * 'c }
  > EOF

We generate the .cmi file

  $ ocamlc add_type_var.mli

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi add_type_var.cmi
  diff module Add_type_var:
  -type ('a, 'b) t =
  +type ('a, 'b, 'c) t =
  -  { a : 'a; b : 'b; }
  +  { a : 'a; b : 'b * 'c; }
  
  [1]

# Removing a type variable from a type declaration:

  $ cat > remove_type_var.mli << EOF
  > type 'a t = { a : 'a; b : int }
  > EOF

We generate the .cmi file

  $ ocamlc remove_type_var.mli

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi remove_type_var.cmi
  diff module Remove_type_var:
  -type ('a, 'b) t =
  +type 'a t =
  -  { a : 'a; b : 'b; }
  +  { a : 'a; b : int; }
  
  [1]

# Changing the use of type variables in a type declaration:

  $ cat > change_type_var_use.mli << EOF
  > type ('a, 'b) t = { a : 'b; b : 'a }
  > EOF

We generate the .cmi file

  $ ocamlc change_type_var_use.mli

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi change_type_var_use.cmi
  diff module Change_type_var_use:
   type ('a, 'b) t =
  -  { a : 'a; b : 'b; }
  +  { a : 'b; b : 'a; }
  
  [1]

# Swapping the order of type variables in a type declaration:

  $ cat > swap_type_vars.mli << EOF
  > type ('b, 'a) t = { a : 'a; b : 'b }
  > EOF

We generate the .cmi file

  $ ocamlc swap_type_vars.mli

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi swap_type_vars.cmi
  diff module Swap_type_vars:
   type ('t1, 't2) t =
  -  { a : 't1; b : 't2; }
  +  { a : 't2; b : 't1; }
  
  [1]

Here we generate `.mli` files with two paramterized types:

  $ cat > ref2.mli << EOF
  > type 'a t = A of 'a
  > type ('a, 'b) s = { a : 'a; b : 'b }
  > EOF

We generate the .cmi file

  $ ocamlc ref2.mli

# Adding a type variable to the first type and removing a one from the second:

  $ cat > add_remove.mli << EOF
  > type ('x, 'y) t = A of 'x * 'y
  > type 'm s = { a : 'm; b : int }
  > EOF

We generate the .cmi file

  $ ocamlc add_remove.mli

Run the api-watcher on the two cmi files

  $ api-diff ref2.cmi add_remove.cmi
  diff module Add_remove:
  -type ('t1, 't2) s =
  +type 't1 s =
  -  { a : 't1; b : 't2; }
  +  { a : 't1; b : int; }
  -type 't1 t =
  +type ('t1, 't2) t =
  -  | A of 't1
  +  | A of 't1 * 't2
  
  [1]

Here we generate a `.mli` file with an arrow paramterized type:

  $ cat > ref_arrow.mli << EOF
  > type 'a t = 'a -> 'a
  > EOF

We generate the .cmi file

  $ ocamlc ref_arrow.mli

# Renaming a type variable consistently across the type declaration:

  $ cat > rename_arrow.mli << EOF
  > type 'b t = 'b -> 'b
  > EOF

We generate the .cmi file

  $ ocamlc rename_arrow.mli

Run the api-watcher on the two cmi files, there should be no diff

  $ api-diff ref_arrow.cmi rename_arrow.cmi

