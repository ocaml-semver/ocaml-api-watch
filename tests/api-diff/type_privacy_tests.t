Here we generate a `.mli` file with a private type abbreviation

  $ cat > ref.mli << EOF
  > type t = private { a : int; b : float }
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

# Removing a private type abbreviation from a type declaration

  $ cat > remove_private.mli << EOF
  > type t = { a : int; b : float }
  > EOF

We generate the .cmi file

  $ ocamlc remove_private.mli

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi remove_private.cmi
  diff module Remove_private:
  -type t = private
  +type t =
     { a : int; b : float; }
  
  [1]

# Removing a private type abbreviation from a type declaration and modifying record fields

  $ cat > remove_private_modify_record.mli << EOF
  > type t = { a : float }
  > EOF

We generate the .cmi file

  $ ocamlc remove_private_modify_record.mli

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi remove_private_modify_record.cmi
  diff module Remove_private_modify_record:
  -type t = private
  +type t =
  -  { a : int; b : float; }
  +  { a : float; }
  
  [1]
