Here we generate a `.mli` file with a variant type:

  $ cat > ref.mli << EOF
  > type rank = Ace | King | Queen | Number of int
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

# Tests for different kind of modifications to a variant type:

### Adding a constructor to a variant type:

  $ cat > add_constructor.mli << EOF
  > type rank = Ace | King | Queen | Jack | Number of int
  > EOF

We generate the .cmi file

  $ ocamlc add_constructor.mli

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi add_constructor.cmi
  diff module Add_constructor:
   type rank =
     ...
  +  | Jack

  [1]

### Removing a constructor from a variant type:

  $ cat > remove_constructor.mli << EOF
  > type rank = Ace | King | Queen
  > EOF

We generate the .cmi file

  $ ocamlc remove_constructor.mli

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi remove_constructor.cmi
  diff module Remove_constructor:
   type rank =
     ...
  -  | Number of int

  [1]

### Modifying a constructor's arguments in a variant type:

  $ cat > modify_constructor_type.mli << EOF
  > type rank = Ace | King | Queen | Number of float
  > EOF

We generate the .cmi file

  $ ocamlc modify_constructor_type.mli

Run the api-watcher on the two cmi files

  $ api-diff ref.cmi modify_constructor_type.cmi
  diff module Modify_constructor_type:
   type rank =
     ...
  -  | Number of int
  +  | Number of float

  [1]

Tests for modifying constructor's record argument

  $ cat > shapes.mli << EOF
  > type point = float * float
  > type shape =
  >  | Circle of {center : point; raduis: int}
  >  | Rectangle of {lower_left: point; upper_right: point}
  > EOF

We generate the .cmi file

  $ ocamlc shapes.mli

### Adding a field to a record type in a constructor argument:

  $ cat > add_field.mli << EOF
  > type point = float * float
  > type shape =
  >  | Circle of {center : point; raduis: int; color:int}
  >  | Rectangle of {lower_left: point; upper_right: point; color:int}
  > EOF

We generate the .cmi file

  $ ocamlc add_field.mli

Run the api-watcher on the two cmi files

  $ api-diff shapes.cmi add_field.cmi
  diff module Add_field:
   type shape =
     ...
     | Circle of
       {
          ...
  +       color : int;
        }
     | Rectangle of
        {
        ...
  +     color : int;
        }
  
  [1]

### Removing a field from a record type in a constructor argument:

  $ cat > remove_field.mli << EOF
  > type point = float * float
  > type shape =
  >  | Circle of {center : point;}
  >  | Rectangle of {lower_left: point; upper_right: point;}
  > EOF

We generate the .cmi file

  $ ocamlc remove_field.mli

Run the api-watcher on the two cmi files

  $ api-diff shapes.cmi remove_field.cmi
  diff module Remove_field:
   type shape =
    ...
    | Circle of
      {
        ...
  -     raduis : int;
      }
  
  [1]

### Modifying a field in a record type in a constructor argument:

  $ cat > modify_field.mli << EOF
  > type point = float * float
  > type shape =
  >  | Circle of {center : point; raduis: float;}
  >  | Rectangle of {lower_left: point; upper_right: point;}
  > EOF

We generate the .cmi file

  $ ocamlc modify_field.mli

Run the api-watcher on the two cmi files

  $ api-diff shapes.cmi modify_field.cmi
  diff module Modify_field:
   type shape =
    ...
    | Circle of
      {
        ...
  -     raduis : int;
  +     raduis : float;
      }
  
  [1]

Tests for modifying constructor's tuple argument

  $ cat > shapes2.mli << EOF
  > type point = float * float
  > type shape =
  >  | Circle of point * int
  >  | Rectangle of point * point
  > EOF

We generate the .cmi file

  $ ocamlc shapes2.mli

### Adding an component to a tuple type in a constructor argument:

  $ cat > add_component.mli << EOF
  > type point = float * float
  > type shape =
  >  | Circle of point * int * int
  >  | Rectangle of point * point * int
  > EOF

We generate the .cmi file

  $ ocamlc add_component.mli

Run the api-watcher on the two cmi files

  $ api-diff shapes2.cmi add_component.cmi
  diff module Add_component:
   type shape =
       ...
  -    | Circle of point * int
  +    | Circle of point * int * int
  -    | Rectangle of point * point
  +    | Rectangle of point * point * int
  
  [1]

### Removing a component from a tuple type in a constructor argument:

  $ cat > remove_component.mli << EOF
  > type point = float * float
  > type shape =
  >  | Circle of point
  >  | Rectangle of point * point
  > EOF

We generate the .cmi file

  $ ocamlc remove_component.mli

Run the api-watcher on the two cmi files

  $ api-diff shapes2.cmi remove_component.cmi
  diff module Remove_component:
   type shape =
       ...
  -    | Circle of point * int
  +    | Circle of point
  
  [1]

### Modifying a component in a tuple type in a constructor argument:

  $ cat > modify_component.mli << EOF
  > type point = float * float
  > type shape =
  >  | Circle of point * float
  >  | Rectangle of point * point
  > EOF

We generate the .cmi file

  $ ocamlc modify_component.mli

Run the api-watcher on the two cmi files

  $ api-diff shapes2.cmi modify_component.cmi
  diff module Modify_component:
   type shape =
       ...
  -    | Circle of point * int
  +    | Circle of point * float
  
  [1]
