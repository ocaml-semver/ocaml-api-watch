Here we generate a `.mli` file with a tuple:

  $ cat > ref_tuple.mli << EOF
  > type t = int * int * int
  > EOF

We generate the .cmi file

  $ ocamlc ref_tuple.mli

### Changing a component in a tuple

  $ cat > change_tuple.mli << EOF
  > type t = float * int * int
  > EOF

We generate the .cmi file

  $ ocamlc change_tuple.mli

Run the api-watcher on the two cmi files, a diff should be reported between the first
components of the two tuples

  $ api-diff --plain ref_tuple.cmi change_tuple.cmi
  diff module Change_tuple:
  -type t = [-int-] * int * int
  +type t = {+float+} * int * int
  
  [1]

### Adding a component to a tuple

  $ cat > add_tuple.mli << EOF
  > type t = int * int * int * string
  > EOF

We generate the .cmi file

  $ ocamlc add_tuple.mli

Run the api-watcher on the two cmi files, the added component should be marked

  $ api-diff --plain ref_tuple.cmi add_tuple.cmi
  diff module Add_tuple:
  -type t = int * int * int
  +type t = int * int * int{+ * string+}
  
  [1]

Here we generate a `.mli` file a nested tuple:

  $ cat > ref_nested_tuple.mli << EOF
  > type t = int * (int * int)
  > EOF

We generate the .cmi file

  $ ocamlc ref_nested_tuple.mli

### Changing a component in the nested tuple

  $ cat > change_nested_tuple.mli << EOF
  > type t = int * (float * int)
  > EOF

We generate the .cmi file

  $ ocamlc change_nested_tuple.mli

Run the api-watcher on the two cmi files, a diff should be reported in the first component of the nested tuple

  $ api-diff --plain ref_nested_tuple.cmi change_nested_tuple.cmi
  diff module Change_nested_tuple:
  -type t = int * ([-int-] * int)
  +type t = int * ({+float+} * int)
  
  [1]

Here we generate a `.mli` file with an arrow type:

  $ cat > ref_arrow.mli << EOF
  > type t = int -> int -> int
  > EOF

We generate the .cmi file

  $ ocamlc ref_arrow.mli

### Changing a argument type in the arrow type

  $ cat > change_arg_type_in_arrow.mli << EOF
  > type t = float -> int -> int
  > EOF

We generate the .cmi file

  $ ocamlc change_arg_type_in_arrow.mli

Run the api-watcher on the two cmi files, a diff should be reported between the argument type of the two arrow types
 
  $ api-diff --plain ref_arrow.cmi change_arg_type_in_arrow.cmi
  diff module Change_arg_type_in_arrow:
  -type t = [-int-] -> int -> int
  +type t = {+float+} -> int -> int
  
  [1]

### Making an argument optional in the arrow type

  $ cat > opt_arg_type.mli << EOF
  > type t = ?opt:int -> int -> int
  > EOF

We generate the .cmi file

  $ ocamlc opt_arg_type.mli << EOF

Run the api-watcher on the two cmi files, the optional argument name should be highlighted

  $ api-diff --plain ref_arrow.cmi opt_arg_type.cmi
  diff module Opt_arg_type:
  -type t = int -> int -> int
  +type t = {+?opt:+}int -> int -> int
  
  [1]

### Changing the arg type in a arrow type with an arrow type as its argument

  $ cat > ref_arrow_arg.mli << EOF
  > type t = (int -> int) -> int
  > EOF

We generate the .cmi file

  $ ocamlc ref_arrow_arg.mli << EOF

  $ cat > cur_arrow_arg.mli << EOF
  > type t = (float -> string) -> int
  > EOF

We generate the .cmi file

  $ ocamlc cur_arrow_arg.mli << EOF

Run the api-watcher on the two cmi files, the argument arrow type should parenthesized

  $ api-diff --plain ref_arrow_arg.cmi cur_arrow_arg.cmi
  diff module Cur_arrow_arg:
  -type t = ([-int-] -> [-int-]) -> int
  +type t = ({+float+} -> {+string+}) -> int
  
  [1]

Here we generate a file with a tuple type nested inside an arrow type:

  $ cat > ref_tuple_in_arrow.mli << EOF
  > type t = int * float * string -> string -> char
  > EOF

We generate the .cmi file

  $ ocamlc ref_tuple_in_arrow.mli

  $ cat > cur_tuple_in_arrow.mli << EOF
  > type t = int * char * string -> string -> string
  > EOF

  $ ocamlc cur_tuple_in_arrow.mli

Run the api-watcher on the two cmi files, the tuple should not be parenthesized

  $ api-diff --plain ref_tuple_in_arrow.cmi cur_tuple_in_arrow.cmi
  diff module Cur_tuple_in_arrow:
  -type t = int * [-float-] * string -> string -> [-char-]
  +type t = int * {+char+} * string -> string -> {+string+}
  
  [1]

Here we generate a file with arrow type nested inside a tuple type:

  $ cat > ref_arrow_in_tuple.mli << EOF
  > type s = (int -> int) * float * ((int -> int) -> int)
  > EOF

We generate the .cmi file

  $ ocamlc ref_arrow_in_tuple.mli

  $ cat > cur_arrow_in_tuple.mli << EOF
  > type s = (float -> float) * string * ((string -> string) -> float)
  > EOF

  $ ocamlc cur_arrow_in_tuple.mli

Run the api-watcher on the two cmi files, the arrows should not be parenthesized

  $ api-diff --plain ref_arrow_in_tuple.cmi cur_arrow_in_tuple.cmi
  diff module Cur_arrow_in_tuple:
  -type s = ([-int-] -> [-int-]) * [-float-] * (([-int-] -> [-int-]) -> [-int-])
  +type s = ({+float+} -> {+float+}) * {+string+} * (({+string+} -> {+string+}) -> {+float+})
  
  [1]


Here we generate a `.mli` file with some type constructors:

  $ cat > ref_type_constrs.mli << EOF
  > type 'a t = 'a list
  > type 'a u = 'a t
  > type s = int u
  > type p = int -> int
  > type q = p
  > type r = q * int
  > type ('a, 'b) record = { a : 'a; b : 'b }
  > type ('a, 'b) record_alias = ('a, 'b) record
  > type c = (int, float) record_alias
  > EOF

We generate the .cmi file

  $ ocamlc ref_type_constrs.mli

Changing the the argument type in a type constructor

  $ cat > change_arg_type.mli << EOF
  > type 'a t = 'a list
  > type 'a u = 'a t
  > type s = float u
  > type p = int -> int
  > type q = p
  > type r = q * int
  > type ('a, 'b) record = { a : 'a; b : 'b }
  > type ('a, 'b) record_alias = ('a, 'b) record
  > type c = (int, float) record_alias
  > EOF

We generate the .cmi file

  $ ocamlc change_arg_type.mli

Run the api-watcher on the two cmi files, unchanged aliases should not be expanded

  $ api-diff --plain ref_type_constrs.cmi change_arg_type.cmi
  diff module Change_arg_type:
  -type s = [-int-] list
  +type s = {+float+} list
  
  [1]

Changing a component in a tuple type

  $ cat > change_tuple_comp.mli << EOF
  > type 'a t = 'a list
  > type 'a u = 'a t
  > type s = int u
  > type p = int -> int
  > type q = p
  > type r = q * float
  > type ('a, 'b) record = { a : 'a; b : 'b }
  > type ('a, 'b) record_alias = ('a, 'b) record
  > type c = (int, float) record_alias
  > EOF

We generate the .cmi file

  $ ocamlc change_tuple_comp.mli

Run the api-watcher on the two cmi files, aliases to unchanged arrow types should  not be expanded

  $ api-diff --plain ref_type_constrs.cmi change_tuple_comp.cmi
  diff module Change_tuple_comp:
  -type r = int -> int * [-int-]
  +type r = int -> int * {+float+}
  
  [1]

Changing arguments in an alias to a nominal type

  $ cat > change_record_arg_type.mli << EOF
  > type 'a t = 'a list
  > type 'a u = 'a t
  > type s = int u
  > type p = int -> int
  > type q = p
  > type r = q * int
  > type ('a, 'b) record = { a : 'a; b : 'b }
  > type ('a, 'b) record_alias = ('a, 'b) record
  > type c = (float, int) record_alias
  > EOF

We generate the .cmi file

  $ ocamlc change_record_arg_type.mli

Run the api-watcher on the two cmi files, the alias to the nominal type should not expand

  $ api-diff --plain ref_type_constrs.cmi change_record_arg_type.cmi
  diff module Change_record_arg_type:
  -type c = ([-int-], [-float-]) record
  +type c = ({+float+}, {+int+}) record
  
  [1]
