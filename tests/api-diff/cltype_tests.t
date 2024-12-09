Here we generate a basic `.mli` file with a class type declaration (class interface):

  $ cat > ref_cltype.mli << EOF
  > class type ref_cltype = object
  >   method m1 : string
  >   method m2 : string -> unit
  > end 
  > EOF

We generate .cmi file
  $ ocamlc ref_cltype.mli

Now, we run api-diff on the same cmi file as both arguments, there should be no difference
  $ api-diff ref_cltype.cmi ref_cltype.cmi

### Adding a new class type:

Generate a new .mli file with an additional class type
  $ cat > add_cltype.mli << EOF
  > class type ref_cltype = object
  >    method m1 : string 
  >    method m2 : string -> unit
  > end
  > class type new_cltype = object
  >    method mk : int -> unit
  >    method mn : int -> int
  > end
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc add_cltype.mli

Run api-diff and check the output
  $ api-diff ref_cltype.cmi add_cltype.cmi
  diff module Add_cltype:
  +class type new_cltype =
  +  object method mk : int -> unit method mn : int -> int end
  
  [1]

### Removing a class type:

Generate a new .mli file with the class type now removed
  $ cat > remove_cltype.mli << EOF
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc remove_cltype.mli

Run api-diff and check the output
  $ api-diff ref_cltype.cmi remove_cltype.cmi
  diff module Remove_cltype:
  -class type ref_cltype =
  -  object method m1 : string method m2 : string -> unit end
  
  [1]
