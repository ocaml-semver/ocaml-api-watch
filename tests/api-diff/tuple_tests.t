Here we generate a `.mli` file with a tuple:

  $ cat > ref.mli << EOF
  > type t = int * int * int
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

### Changing a component in a tuple

  $ cat > change.mli << EOF
  > type t = float * int * int
  > EOF

We generate the .cmi file

  $ ocamlc change.mli

Run the api-watcher on the two cmi files, a diff should be reported between the first
components of the two tuples

  $ api-diff --plain ref.cmi change.cmi
  diff module Change:
  -type t = ([-int-] * int * int)
  +type t = ({+float+} * int * int)
  
  [1]

### Adding a component to a tuple

  $ cat > add.mli << EOF
  > type t = int * int * int * string
  > EOF

We generate the .cmi file

  $ ocamlc add.mli

Run the api-watcher on the two cmi files, the added component should be marked

  $ api-diff --plain ref.cmi add.cmi
  diff module Add:
  -type t = (int * int * int)
  +type t = (int * int * int{+ * string+})
  
  [1]
