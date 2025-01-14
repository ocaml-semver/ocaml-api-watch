Here we generate a `.mli` file with a record type:

  $ cat > ref.mli << EOF
  > type student = {first_name: string; last_name: string; id: int option}
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

# Tests for different kind of modifications to a record type:

### Adding a field to a record type:

  $ cat > add_field.mli << EOF
  > type student = {first_name: string; last_name: string; id: int option; level: int}
  > EOF

We generate the .cmi file

  $ ocamlc add_field.mli 

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi add_field.cmi
  diff module Add_field:
   type student = {
     first_name : string;
     id : int option;
     last_name : string;
  +  level : int;
   }
  
  [1]

### Removing a field from a record type:

  $ cat > remove_field.mli << EOF 
  > type student = {first_name: string; last_name: string}
  > EOF

We generate the .cmi file

  $ ocamlc remove_field.mli 

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi remove_field.cmi
  diff module Remove_field:
   type student = {
     first_name : string;
     last_name : string;
  -  id : int option;
   }
  
  [1]

### Modifying a field's type in a record type:

  $ cat > modify_field_type.mli << EOF
  > type student = {first_name: string; last_name: string; id: int}
  > EOF

We generate the .cmi file

  $ ocamlc modify_field_type.mli << EOF

Run api-watcher on the two cmi files

  $ api-diff ref.cmi modify_field_type.cmi
  diff module Modify_field_type:
   type student = {
     first_name : string;
     last_name : string;
  -  id : int option;
  +  id : int;
   }
  
  [1]

### Modifying a field's type in a record type to a same alias type:

  $ cat > alias_field_type.mli << EOF
  > type y = int
  > type student = {first_name: string; last_name: string; id: y}
  > EOF

We generate the .cmi file

  $ ocamlc alias_field_type.mli << EOF

Run api-watcher on the two cmi files

  $ api-diff ref.cmi alias_field_type.cmi
  diff module Alias_field_type:
  +type y = int
  
  [1]
