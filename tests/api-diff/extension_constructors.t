Here we generate a `.mli` file to test changes in extension constructors 

  $ cat > ref_extcstr.mli << EOF
  > type o = ..
  > type o += A of int
  > EOF

We generate the .cmi file

  $ ocamlc ref_extcstr.mli

### Marking an extension constructor private

  $ cat > cur_private_extcstr.mli << EOF
  > type o = ..
  > type o += private A of int
  > EOF

We generate the .cmi file

  $ ocamlc cur_private_extcstr.mli

Run the api-watcher on the two cmi files

  $ api-diff --plain ref_extcstr.cmi cur_private_extcstr.cmi
  diff module Cur_private_extcstr:
  -type o += A of int
  +type o +={+ private+} A of int
  
  [1]

### Changing a constructor paramter

  $ cat > cur_change_param_extcstr.mli << EOF
  > type o = ..
  > type o += A of float
  > EOF

We generate the .cmi file

  $ ocamlc cur_change_param_extcstr.mli

Run the api-watcher on the two cmi files

  $ api-diff --plain ref_extcstr.cmi cur_change_param_extcstr.cmi
  diff module Cur_change_param_extcstr:
  -type o += A of [-int-]
  +type o += A of {+float+}
  
  [1]
