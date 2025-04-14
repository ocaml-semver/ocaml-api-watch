Here we generate a `.mli` file with two modules, one type in the second module references atype in the first module

  $ cat > ref.mli << EOF
  > module M : sig
  >  type t = int
  > end
  > module N : sig
  >  val x : M.t
  > end
  > EOF

We generate the `.cmi` file

  $ ocamlc ref.mli

  $ cat > cur.mli << EOF
  > module M : sig
  >  type t = float
  > end
  > module N : sig
  > val x : M.t
  > end
  > EOF

We generate the `.cmi` file

  $ ocamlc cur.mli

Run the api-watcher on the two files, there should be a diff on val x

Here we create two versions of a library, where one type in the second module references a type in the first module

  $ api-diff --plain ref.cmi cur.cmi
  diff module Cur.M:
  -type t = [-int-]
  +type t = {+float+}

  diff module Cur.N:
  -val x : [-int-]
  +val x : {+float+}
  
  [1]

  $ mkdir ref_lib && cd ref_lib
  $ cat > dune-project << EOF
  > (lang dune 2.9)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name library))
  > EOF

  $ cat > m.ml << EOF
  > type t = int
  > EOF

  $ cat > n.ml << EOF
  > type u = M.t
  > EOF

  $ dune build

  $ cd .. && mkdir cur_lib && cd cur_lib
  $ cat > dune-project << EOF
  > (lang dune 2.9)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name library))
  > EOF

  $ cat > m.ml << EOF
  > type t = float
  > EOF

  $ cat > n.ml << EOF
  > type u = M.t
  > EOF

  $ dune build

Run the api-watcher on the two libraries, there should be a diff reported on type u

  $ dune exec -- api-diff --main-module=library ../ref_lib/_build/default/.library.objs/byte ../cur_lib/_build/default/.library.objs/byte
  diff module Library.M:
  -type t = int
  +type t = float

  diff module Library.N:
  -type u = int
  +type u = float
  
  [1]
