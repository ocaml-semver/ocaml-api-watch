val type_decls :
  reference:Intermed.Type_decl.t -> current:Intermed.Type_decl.t -> unit
(** Rename the occurances of each corresponding pair of type parameters in
    [reference] and [current] to a unique ti,
    where 1 <= i <= Int.max(List.length [current], List.length [reference]) *)

val is_params :
  reference:Intermed.Type_decl.param list ->
  current:Intermed.Type_decl.param list ->
  bool
(** Return true if each pair of corresponding type paramters
    in [reference] and [current]
    have the same name, false otherwise *)

val params_arity :
  reference:Intermed.Type_decl.param list ->
  current:Intermed.Type_decl.param list ->
  Intermed.Type_decl.param list * Intermed.Type_decl.param list
(** Appends dummy type_expr values to the shorter list, until its length is the
    same as the other list *)
