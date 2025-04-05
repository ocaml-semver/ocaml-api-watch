val pp_diff_option : Format.formatter -> Api_watch.Diff.module_ option -> unit
val compile_interface : string -> Types.signature

val first_type_declaration :
  Types.signature -> (Ident.t * Types.type_declaration) option
