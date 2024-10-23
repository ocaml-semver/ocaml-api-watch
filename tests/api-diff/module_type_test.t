# Tests for module type type modifications

Here we generate a `.mli` file with a module type:

  $ cat > modtype_ref.mli << EOF
  > module type M = sig val x : int end
  > 
  > EOF

We generate the .cmi file

  $ ocamlc modtype_ref.mli

And now we run api-watcher on that same cmi file as both arguments,
there should be no diff:

  $ api-diff modtype_ref.cmi modtype_ref.cmi

### Adding a module type:

Generate a new .mli file with an additional module type
  $ cat > add_modtype.mli << EOF
  > module type M = sig val x : int end
  > module type P = sig val y : float end
  > 
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc add_modtype.mli

Run api-diff and check the output
  $ api-diff modtype_ref.cmi add_modtype.cmi
  diff module Add_modtype:
  +module type P = sig val y : float end
  
  [1]

### Removing a module type:

Generate a new .mli file with the module type removed
  $ cat > remove_modtype.mli << EOF
  > 
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc remove_modtype.mli

Run api-diff and check the output
  $ api-diff modtype_ref.cmi remove_modtype.cmi
  diff module Remove_modtype:
  -module type M = sig val x : int end
  
  [1]

### Modifying a module type:

Generate a new .mli file with the module type modified
  $ cat > modify_modtype.mli << EOF
  > module type M = sig val x : float end
  > 
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc modify_modtype.mli

Run api-diff and check the output
  $ api-diff modtype_ref.cmi modify_modtype.cmi
  diff module Modify_modtype.M:
  -val x : int
  +val x : float
  
  [1]


# Switching a module type from concrete to abstract

Generate a new .mli file with a concrete submodule type
  $ cat > conc_modtype.mli << EOF
  > module type M = sig  val x : float end
  > module type P
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc conc_modtype.mli

Generate a new .mli file with an abstract submodule type
  $ cat > abs_modtype.mli << EOF
  > module type M
  > module type P = sig  val y : string end
  > module type N = sig  val d : int end
  > EOF

Compile the modified .mli file to a .cmi file
  $ ocamlc abs_modtype.mli

Run api-diff and check the output
  $ api-diff conc_modtype.cmi abs_modtype.cmi
  diff module Abs_modtype:
  +module type N = sig val d : int end
  
  diff module Abs_modtype.M:
  +<unsupported change>
  
  diff module Abs_modtype.P:
  +<unsupported change>
  
  [1]
