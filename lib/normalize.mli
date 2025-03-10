val type_decls :
  reference:Intermed.TypeDecl.t -> current:Intermed.TypeDecl.t -> unit
(** Rename the occurances of each corresponding pair of type parameters in
    [reference] and [current] to a unique ti,
    where 1 <= i <= Int.max(List.length [current], List.length [reference]) *)

val is_params :
  reference:Intermed.TypeDecl.param list ->
  current:Intermed.TypeDecl.param list ->
  bool
(** Return true if each pair of corresponding type paramters
    in [reference] and [current]
    have the same name, false otherwise *)

val params_arity :
  reference:Intermed.TypeDecl.param list ->
  current:Intermed.TypeDecl.param list ->
  Intermed.TypeDecl.param list * Intermed.TypeDecl.param list
(** Appends dummy type_expr values to the shorter list, until its length is the
    same as the other list *)
