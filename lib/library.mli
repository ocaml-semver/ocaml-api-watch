val load_cmi : string -> (Types.signature * string, string) result
val load_unwrapped : string -> (Types.signature String_map.t, string) result

val load :
  main_module:string -> string -> (Types.signature String_map.t, string) result
