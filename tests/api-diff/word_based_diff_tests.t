Here we generate a `.mli` file with some types:

  $ cat > ref.mli << EOF
  > type t = int
  > type ('a, 'b, 'c) u = { mutable a : 'a; b : 'b; c : 'c }
  > type v = A of int * int | B of { a : int; b : float }
  > type 'a p = 'a * 'a
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

Changing the types in the ref.mli

  $ cat > cur.mli << EOF
  > type t
  > type ('a) u = { a : 'a; b : int; }
  > type v = A of { a : int; b : int } | B of { a : int; b : string }
  > type p = int * int
  > EOF

We generate the .cmi file

  $ ocamlc cur.mli

Run api-watcher on the two cmi file with word-level diffing flag enabled

  $ api-diff --word-diff ref.cmi cur.cmi
  diff module Cur:
   type 'a p = 'aint * 'aint
   type t = int
   type ('a, 'b, 'c) u =
     { mutable a : 'a; b : 'bint; c : 'c; }
   type v =
     | A of int * int{ a : int; b : int; }
     | B of { a : int; b : floatstring; }
  
  [1]

Run api-watcher on the two cmi file with plain and word-level diffing flags enabled

  $ api-diff --word-diff --plain ref.cmi cur.cmi
  diff module Cur:
   type [-'a-] p = [-'a-]{+int+} * [-'a-]{+int+}
   type t[- =-][- int-]
   type ('a, [-'b-], [-'c-]) u =
     {[- mutable-] a : 'a; b : [-'b-]{+int+};[- c : 'c;-] }
   type v =
     | A of [-int * int-]{+{ a : int; b : int; }+}
     | B of { a : int; b : [-float-]{+string+}; }
  
  [1]
