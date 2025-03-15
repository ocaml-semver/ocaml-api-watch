Here we generate a `.mli` file with an arrow type:

  $ cat > ref.mli << EOF
  > type t = int -> int -> int
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

### Changing a argument type in the arrow type

  $ cat > change_arg_type.mli << EOF
  > type t = float -> int -> int
  > EOF

We generate the .cmi file

  $ ocamlc change_arg_type.mli

Run the api-watcher on the two cmi files, a diff should be reported between the argument type of the two arrow types
 
  $ api-diff --plain ref.cmi change_arg_type.cmi
  diff module Change_arg_type:
  -type t = ([-int-] -> int -> int)
  +type t = ({+float+} -> int -> int)
  
  [1]

### Making an argument optional in the arrow type

  $ cat > opt_arg_type.mli << EOF
  > type t = ?opt:int -> int -> int
  > EOF

We generate the .cmi file

  $ ocamlc opt_arg_type.mli << EOF

Run the api-watcher on the two cmi files, the optional argument name should be highlighted

  $ api-diff --plain ref.cmi opt_arg_type.cmi
  diff module Opt_arg_type:
  -type t = (int -> int -> int)
  +type t = ({+?opt:+}int -> int -> int)
  
  [1]
