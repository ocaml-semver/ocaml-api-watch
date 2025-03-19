val pp_diff_option : Format.formatter -> Api_watch.Diff.module_ option -> unit
val compile_interface : string -> Types.signature

val first_type_decl :
  Types.signature -> (string * Api_watch__.Intermed.Type_decl.t) option
