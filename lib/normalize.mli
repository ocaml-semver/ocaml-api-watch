val type_declarations :
  reference:Types.type_declaration -> current:Types.type_declaration -> unit
(** Rename the occurances of each corresponding pair of type parameters in
    [reference] and [current] to a unique ti,
    where 1 <= i <= Int.max(List.length [current], List.length [reference]) *)

val is_type_params :
  reference:Types.type_expr list -> current:Types.type_expr list -> bool
(** Return true if each pair of corresponding type paramters
    in [reference] and [current]
    have the same name, false otherwise *)

val type_params_arity :
  reference:Types.type_expr list ->
  current:Types.type_expr list ->
  Types.type_expr list * Types.type_expr list
(** Appends dummy type_expr values to the shorter list, until its length is the
    same as the other list *)
