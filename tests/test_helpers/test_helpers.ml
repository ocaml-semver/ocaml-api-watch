open Api_watch_diff

let rec pp_module_diff fmt module_diff =
  let pp_change fmt = function
    | Added _ -> Format.fprintf fmt "Added"
    | Removed _ -> Format.fprintf fmt "Removed"
    | Modified _ -> Format.fprintf fmt "Modified"
  in
  let pp_item fmt = function
    | Value { name; change } ->
        Format.fprintf fmt " Value (%s, %a);" name pp_change change
    | Module sub_module_diff -> pp_module_diff fmt sub_module_diff
  in
  match module_diff.changes with
  | Unsupported ->
      Format.fprintf fmt "Module %s: Unsupported changes"
        module_diff.module_name
  | Supported changes ->
      Format.fprintf fmt "Module %s: [%a]" module_diff.module_name
        (Format.pp_print_list pp_item)
        changes

let pp_diff_list fmt diff_opt =
  match diff_opt with
  | None -> Format.fprintf fmt "None"
  | Some module_diff -> Format.fprintf fmt "Some %a" pp_module_diff module_diff

let parse_interface content =
  let lexbuf = Lexing.from_string content in
  Parse.interface lexbuf

let generate_signature intf =
  let typing_env =
    Typemod.initial_env ~loc:Location.none ~initially_opened_module:None
      ~open_implicit_modules:[]
  in
  let typed_tree = Typemod.type_interface typing_env intf in
  typed_tree.sig_type

let compile_interface (content : string) : Types.signature =
  let intf = parse_interface content in
  let signature = generate_signature intf in
  signature
