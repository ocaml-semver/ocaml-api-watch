val pp_module_diff: Format.formatter->Api_watch_diff.module_diff  -> unit
val pp_diff_list: Format.formatter->Api_watch_diff.module_diff option -> unit
val compile_interface : string -> Types.signature
