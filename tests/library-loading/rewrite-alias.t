When loading a library, module aliases should be properly expanded.
Consider the following library with the `Main` entry module:

  $ mkdir lib
  $ cat > lib/main.mli << EOF
  > module X = Sub.Y
  > EOF
  $ cat > lib/sub.mli << EOF
  > module Y = Ext.Z
  > EOF

And it's dependency:

  $ mkdir ext
  $ cat > ext/ext.mli << EOF
  > module type T
  > module Z : T
  > EOF

We generate the .cmi file:

  $ ocamlc -c ext/ext.mli
  $ ocamlc -I ext -c lib/sub.mli 
  $ ocamlc -I ext -I lib -c lib/main.mli

Now we load the library and print it:

  $ ../../dev-tools/print_api.exe --main-module Main lib
  module X = Sub.Y

As we can see, there is a bug in the loading code as the path is not expanded.
We dould expect the output to be: module X = Ext.Z
