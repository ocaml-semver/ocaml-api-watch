Here we generate a `.mli` file with the class type:

  $ cat > ref_class.mli << EOF
  > class type cls = object
  >   method get : int
  >   method set : int -> unit
  > end 
  > EOF

We generate .cmi file
  $ ocamlc ref_class.mli

Now, we run api-watcher on the same cmi file as both arguments, there should be no difference
  $ api-diff ref_class.cmi ref_class.cmi

### Adding a new class type:

Generate a new .mli file with an additional class type
  $ cat > add_class.mli << EOF
  > class type cls = object
  >    method get : int 
  >    method set : int -> unit
  > end
  > class type cls1 = object
  >    method calculate : float -> float
  > end
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc add_class.mli

Run api-diff and check the output
  $ api-diff ref_class.cmi add_class.cmi
  diff module Add_class:
  +type cls1 = < calculate : float -> float >
  
  [1]

### Removing a class type:

Generate a new .mli file with the class type now removed
  $ cat > remove_class.mli << EOF
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc remove_class.mli

Run api-diff and check the output
  $ api-diff ref_class.cmi remove_class.cmi
  diff module Remove_class:
  -type cls = < get : int; set : int -> unit >
  
  [1]
