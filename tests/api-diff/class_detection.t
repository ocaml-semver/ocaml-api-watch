Here we generate a `.mli` file with the class declaration:

  $ cat > ref_class.mli << EOF
  > class ref_class : object
  >   method get : int
  >   method set : int -> unit
  > end 
  > EOF

We generate .cmi file
  $ ocamlc ref_class.mli

Now, we run api-diff on the same cmi file as both arguments, there should be no difference
  $ api-diff ref_class.cmi ref_class.cmi

### Adding a new class:

Generate a new .mli file with an additional class
  $ cat > add_class.mli << EOF
  > class ref_class : object
  >    method get : int 
  >    method set : int -> unit
  > end
  > class add_class : object
  >    method calculate : float -> float
  > end
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc add_class.mli

Run api-diff and check the output
  $ api-diff ref_class.cmi add_class.cmi
  diff module Add_class:
  +class add_class : object method calculate : float -> float end
  
  [1]

### Removing a class:

Generate a new .mli file with the class now removed
  $ cat > remove_class.mli << EOF
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc remove_class.mli

Run api-diff and check the output
  $ api-diff ref_class.cmi remove_class.cmi
  diff module Remove_class:
  -class ref_class : object method get : int method set : int -> unit end
  
  [1]

### Modifing a class:

  $ cat > modify_class.mli <<EOF
  > class ref_class : object
  >   method set : int -> int
  >   method size : int 
  > end 
  > EOF

We generate a .cmi file

  $ ocamlc modify_class.mli

Run api-watcher on the two cmi files, there should be a difference

  $ api-diff ref_class.cmi modify_class.cmi
  diff module Modify_class:
  -class ref_class : object method get : int method set : int -> unit end
  +class ref_class : object method set : int -> int method size : int end
  
  [1]
