This tests issue #121 (https://github.com/ocaml-semver/ocaml-api-watch/issues/121)

Let's setup a test case:

  $ cat > deps.mli << EOF
  > module X : sig end
  > module type Y = sig end
  > EOF

  $ cat > file.mli << EOF
  > module A = Deps.X
  > module B : Deps.Y
  > EOF

  $ ocamlc -c deps.mli
  $ ocamlc -c -I . file.mli

  $ api-diff --main-module file . .
