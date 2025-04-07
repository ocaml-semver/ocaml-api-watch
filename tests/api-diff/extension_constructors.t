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

### Changing a constructor argument

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

Here we generate a `.mli` file to test changes in exceptions 

  $ cat > ref_exception.mli << EOF
  > exception BadExp of int
  > EOF

We generate the .cmi file

  $ ocamlc ref_exception.mli

### Changing a constructor argument 

  $ cat > cur_change_param_exn.mli << EOF
  > exception BadExp of float
  > EOF

We generate the .cmi file

  $ ocamlc cur_change_param_exn.mli

Run the api-watcher on the two cmi files, both should be displayed in the exception syntax

  $ api-diff --plain ref_exception.cmi cur_change_param_exn.cmi
  diff module Cur_change_param_exn:
  -exception BadExp of [-int-]
  +exception BadExp of {+float+}
  
  [1]

### Marking an exception private

  $ cat > cur_private_exn.mli << EOF
  > type exn += private BadExp of int
  > EOF

We generate the .cmi file

  $ ocamlc cur_private_exn.mli

Run the api-watcher on the two cmi files, both should be displayed in the extension constructor syntax

  $ api-diff --plain ref_exception.cmi cur_private_exn.cmi
  diff module Cur_private_exn:
  -type exn += BadExp of int
  +type exn +={+ private+} BadExp of int
  
  [1]

Here we generate a `.mli` with a parameterized extensible variant type:

  $ cat > ref_param_ext_var.mli << EOF
  > type ('a, 'b) po = ..
  > type ('a, 'b) po += A of 'a
  > EOF

We generate the .cmi file

  $ ocamlc ref_param_ext_var.mli

### Removing a type paramter from a parameterized extensible variant type

  $ cat > cur_param_ext_var.mli << EOF
  > type 'a po = ..
  > type 'a po += A of 'a
  > EOF

We generate the .cmi file

  $ ocamlc cur_param_ext_var.mli

Run the api-watcher, there should be no diff on the constructors

  $ api-diff --plain ref_param_ext_var.cmi cur_param_ext_var.cmi
  diff module Cur_param_ext_var:
  -type ('a[-, 'b-]) po =
  +type ('a) po =
     ..
  
  [1]
