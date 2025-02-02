Here we generate a `.mli` file to test the changes in the type manifest

  $ cat > ref.mli << EOF
  > type t
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

### Adding a type manifest

  $ cat > add_manifest.mli << EOF
  > type t = int list
  > EOF

We generate the .cmi file

  $ ocamlc add_manifest.mli

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi add_manifest.cmi
  diff module Add_manifest:
   -type t
   +type t = int list
   
  [1]
