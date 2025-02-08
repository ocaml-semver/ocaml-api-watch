val normalize_type_decls :
  Types.type_declaration ->
  Types.type_declaration ->
  Types.type_declaration * Types.type_declaration

val normalized_type_params :
  Types.type_expr list -> Types.type_expr list -> bool
