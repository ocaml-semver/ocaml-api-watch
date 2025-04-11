When diffing two signatures, we heavily rely on the typing environment.

It should be set in a way that let's us lookup values outside the current module.

Consider the following two versions:

  $ cat > test.mli <<EOF
  > module M : sig
  >   type t = int
  > end
  > module N : sig
  >   val x : M.t
  > end
  > val x : M.t
  > EOF
  $ ocamlc test.mli

  $ cat > test2.mli <<EOF
  > module M : sig
  >   type t = float
  > end
  > module N : sig
  >   val x : M.t
  > end
  > val x : M.t
  > EOF
  $ ocamlc test2.mli

Here, a diff should be reported on both N.x and x:

  $ api-diff --plain test.cmi test2.cmi
  diff module Test2:
  -val x : [-int-]
  +val x : {+float+}
  
  diff module Test2.M:
  -type t = [-int-]
  +type t = {+float+}
  
  diff module Test2.N:
  +<unsupported change>
  
  [1]

Though at the moment, we can see it's not the case. M.t is treated as an unknown,
outside type and therefore, no diff is reported on neither x values.

Similarly, this should work across compilation units:

  $ mkdir v1
  $ cat > v1/m.mli << EOF
  > type t = int
  > EOF
  $ cat > v1/n.mli << EOF
  > val x : M.t
  > EOF
  $ cat > v1/main.mli << EOF
  > module M = M
  > module N = N
  > val x : M.t
  > EOF
  $ ocamlc v1/m.mli
  $ ocamlc -I v1 v1/n.mli
  $ ocamlc -I v1 v1/main.mli

  $ mkdir v2
  $ cat > v2/m.mli << EOF
  > type t = float
  > EOF
  $ cat > v2/n.mli << EOF
  > val x : M.t
  > EOF
  $ cat > v2/main.mli << EOF
  > module M = M
  > module N = N
  > val x : M.t
  > EOF
  $ ocamlc v2/m.mli
  $ ocamlc -I v2 v2/n.mli
  $ ocamlc -I v2 v2/main.mli

  $ api-diff --plain --main-module main v1 v2
  diff module Main:
  -val x : [-int-]
  +val x : {+float+}
  
  diff module Main.M:
  -type t = [-int-]
  +type t = {+float+}
  
  [1]

