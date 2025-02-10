val normalize_type_decls :
  reference:Types.type_declaration ->
  current:Types.type_declaration ->
  Types.type_declaration * Types.type_declaration
(** Rename the occurances of each corresponding pair of type parameters in
    [reference] and [current] to a unique ti,
    where 1 <= i <= Int.max(List.length [current], List.length [reference]) *)

val normalized_type_params :
  reference:Types.type_expr list -> current:Types.type_expr list -> bool
(** Return true if each pair of corresponding type paramters
    in [reference] and [current]
    have the same name, false otherwise *)
