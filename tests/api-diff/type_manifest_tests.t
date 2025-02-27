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

### Adding a type manifest and private type abberivation

  $ cat > add_manifest_private.mli << EOF
  > type t = private int list
  > EOF

We generate the .cmi file

  $ ocamlc add_manifest_private.mli

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi add_manifest_private.cmi
  diff module Add_manifest_private:
  -type t
  +type t = private int list
  
  [1]

### Adding a type manifest, private and a record type

  $ cat > add_manifest_private_record.mli << EOF
  > type u = { a : int }
  > type t = u = private { a : int }
  > EOF

We generate the .cmi file

  $ ocamlc add_manifest_private_record.mli

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi add_manifest_private_record.cmi
  diff module Add_manifest_private_record:
  -type t
  +type t = u = private
  +  { a : int; }
  +type u = { a : int; }
  
  [1]
