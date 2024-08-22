This file makes the basic test suite for api-diff.

Each test starts from two `.mli` files that are generated on the fly and then
compiled into `.cmi` files using ocamlc.
api-diff is then run on the two files so we can actually ensure the output
is the expected one.

It is also possible to reuse the ref.cmi file from the first test as a baseline
and simply generate a modified version of it.


## Identical .cmi files:

Here we generate a basic `.mli` file with two types and a function:

  $ cat > ref.mli << EOF
  > type t = int
  > type unused_type = string
  > val f : t -> string
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

And now we run api-watcher on that same cmi file as both arguments,
there should be no diff:

  $ api-diff ref.cmi ref.cmi

## Different .cmi files for type tests:

### A file with an additional type:

  $ cat > add_type.mli <<EOF
  > type t = int
  > type unused_type = string
  > type added_t = float
  > val f : t -> string
  > EOF

We generate the .cmi file

  $ ocamlc add_type.mli

Run api-watcher on the two cmi files, there should be a difference

  $ api-diff ref.cmi add_type.cmi
  diff module Add_type:
  
  +<unsupported change>

### A file with a removed type:

  $ cat > remove_type.mli <<EOF
  > type t = int
  > val f : t -> string
  > EOF

We generate the .cmi file

  $ ocamlc remove_type.mli

Run api-watcher on the two cmi files, there should be a difference

  $ api-diff ref.cmi remove_type.cmi
  diff module Remove_type:
  
  +<unsupported change>

### A file with a modified type:

  $ cat > modify_type.mli <<EOF
  > type t = float
  > type unused_type = string
  > val f : t -> string
  > EOF

We generate a .cmi file

  $ ocamlc modify_type.mli

Run api-watcher on the two cmi files, there should be a difference

  $ api-diff ref.cmi modify_type.cmi
  diff module Modify_type:
  
  +<unsupported change>

## Different .cmi files for value tests:

### Adding a value:

Generate a new .mli file with an additional value
  $ cat > add_value.mli << EOF
  > type t = int
  > type unused_type = string
  > val f : t -> string
  > val g : t -> t
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc add_value.mli

Run api-diff and check the output
  $ api-diff ref.cmi add_value.cmi
  diff module Add_value:
  
  +val g : t -> t

### Removing a value:

Generate a new .mli file with the value removed
  $ cat > remove_value.mli << EOF
  > type t = int
  > type unused_type = string
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc remove_value.mli

Run api-diff and check the output
  $ api-diff ref.cmi remove_value.cmi
  diff module Remove_value:
  
  -val f : t -> string

### Modifying a value:

Generate a new .mli file with the value modified
  $ cat > modify_value.mli << EOF
  > type t = int
  > type unused_type = string
  > val f : t -> t
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc modify_value.mli

Run api-diff and check the output
  $ api-diff ref.cmi modify_value.cmi
  diff module Modify_value:
  
  -val f : t -> string
  +val f : t -> t

Here we generate a `.mli` file with a module:

  $ cat > mod_ref.mli << EOF
  > module M : sig val x : int end
  > 
  > EOF

We generate the .cmi file

  $ ocamlc mod_ref.mli

And now we run api-watcher on that same cmi file as both arguments,
there should be no diff:

  $ api-diff mod_ref.cmi mod_ref.cmi

### Adding a module:

Generate a new .mli file with an additional module
  $ cat > add_module.mli << EOF
  > module M : sig val x : int end
  > module N : sig val y : float end
  > 
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc add_module.mli

Run api-diff and check the output
  $ api-diff mod_ref.cmi add_module.cmi
  diff module Add_module:
  
  +module N: sig val y : float end

### Removing a module:

Generate a new .mli file with the module removed
  $ cat > remove_module.mli << EOF
  > 
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc remove_module.mli

Run api-diff and check the output
  $ api-diff mod_ref.cmi remove_module.cmi
  diff module Remove_module:
  
  -module M: sig val x : int end

### Modifying a module:

Generate a new .mli file with the module modified
  $ cat > modify_module.mli << EOF
  > module M : sig val x : float end
  > 
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc modify_module.mli

Run api-diff and check the output
  $ api-diff mod_ref.cmi modify_module.cmi
  diff module Modify_module.M:
  
  -val x : int
  +val x : float

Generate a new .mli file with values and submodules
  $ cat > orig_module.mli << EOF
  > module M : sig val x : float end
  > type ('a, 'b) result = Ok of 'a | Error of 'b
  >  val a : string -> int
  > val f : int -> string
  > module D : sig
  >   val b : int list -> int
  >   val g : int -> string
  > end
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc orig_module.mli

Generate a new .mli file with the values and submodules modified
  $ cat > modified_module.mli << EOF
  > module M : sig val x : float end
  > type ('a, 'b) result = Ok of 'a | Error of 'b
  > val a : string -> float
  > val f : int -> (string, string) result
  > module D : sig
  >   val b : float list -> float
  >   val g : int -> (string, string) result
  > end
  > module E : sig val x: int end
  > EOF

Compile the modified .mli file to a .cmi file
  $ ocamlc modified_module.mli

Run api-diff and check the output
  $ api-diff orig_module.cmi modified_module.cmi
  diff module Modified_module:
  -val a : string -> int
  +val a : string -> float
  -val f : int -> string
  +val f : int -> (string, string) result
  +module E: sig val x : int end
  
  diff module Modified_module.D:
  -val b : int list -> int
  +val b : float list -> float
  -val g : int -> string
  +val g : int -> (string, string) result
  

Create the first version of a simple project
  $ mkdir -p project_v1/lib

  $ cat > project_v1/dune-project <<EOF
  > (lang dune 2.9)
  > (name myproject)
  > EOF

  $ cat > project_v1/lib/dune <<EOF
  > (library
  >  (name mylib))
  > EOF

Create math.ml file with basic math functions and an Advanced module
  $ cat > project_v1/lib/math.ml <<EOF
  > let add x y = x + y
  > let subtract x y = x - y
  > module Advanced = struct
  >   let square x = x * x
  >   type shape = Square | Circle
  > end
  > EOF

Create math.mli interface file
  $ cat > project_v1/lib/math.mli <<EOF
  > val add : int -> int -> int
  > val subtract : int -> int -> int
  > module Advanced : sig
  >   val square : int -> int
  >   type shape = Square | Circle
  > end
  > EOF

Create utils.ml file with a double function
  $ cat > project_v1/lib/utils.ml <<EOF
  > let double x = x * 2
  > EOF

Create utils.mli interface file
  $ cat > project_v1/lib/utils.mli <<EOF
  > val double : int -> int
  > EOF

Build the first version
  $ cd project_v1 && dune build && cd ..

Create the second version of the same project with some changes
  $ cp -r project_v1 project_v2

Update math.ml in project_v2 with new functions and modifications
  $ cat > project_v2/lib/math.ml <<EOF
  > let add x y z = x + y + z
  > let subtract x y = x - y
  > let multiply x y = x * y
  > module Advanced = struct
  >   let square x = x * x
  >   let cube x = x * x * x
  >   type shape = Square | Circle | Triangle
  > end
  > module New_module = struct
  >   let hello () = "Hello, World!"
  > end
  > EOF

Update math.mli in project_v2 to reflect changes
  $ cat > project_v2/lib/math.mli <<EOF
  > val add : int -> int -> int -> int
  > val subtract : int -> int -> int
  > val multiply : int -> int -> int
  > module Advanced : sig
  >   val square : int -> int
  >   val cube : int -> int
  >   type shape = Square | Circle | Triangle
  > end
  > module New_module : sig
  >   val hello : unit -> string
  > end
  > EOF

Update utils.ml in project_v2 with a new triple function
  $ cat > project_v2/lib/utils.ml <<EOF
  > let double x = x * 2
  > let triple x = x * 3
  > EOF

Update utils.mli in project_v2 to include the new triple function
  $ cat > project_v2/lib/utils.mli <<EOF
  > val double : int -> int
  > val triple : int -> int
  > EOF

Build the second version
  $ cd project_v2 && dune build && cd ..

Run the api-diff tool on the two project versions
  $ api-diff project_v1 project_v2
  diff module project_v2.Mylib__Math:
  -val add : int -> int -> int
  +val add : int -> int -> int -> int
  +val multiply : int -> int -> int
  +module New_module: sig val hello : unit -> string end
  
  diff module project_v2.Mylib__Math.Advanced:
  +val cube : int -> int
  
  diff module project_v2.Mylib__Utils:
  +val triple : int -> int
  
