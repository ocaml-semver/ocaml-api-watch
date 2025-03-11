(** Converts from compiler representation of different items (type_declarations,
    value_description, etc.) to an internal intermediate representation used when diffing.
*)

val type_declaration : Types.type_declaration -> Intermed.TypeDecl.t
