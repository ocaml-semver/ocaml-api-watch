open Api_watch.Diff

let rec pp_module_modification fmt = function
  | Unsupported -> Format.fprintf fmt "Unsupported"
  | Supported changes ->
      Format.fprintf fmt "Supported [ %a]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";\n")
           pp_item_diff)
        changes

and pp_item_diff fmt = function
  | Value value_diff -> pp_value_diff fmt value_diff
  | Module module_diff -> pp_module_diff fmt module_diff
  | Type type_diff -> pp_type_diff fmt type_diff
  | Modtype module_type_diff -> pp_module_type_diff fmt module_type_diff
  | Class class_diff -> pp_class_diff fmt class_diff

and pp_value_diff fmt { vname; vdiff } =
  match vdiff with
  | Added _ -> Format.fprintf fmt "Value (%s, Added)" vname
  | Removed _ -> Format.fprintf fmt "Value (%s, Removed)" vname
  | Modified _ -> Format.fprintf fmt "Value (%s, Modified)" vname

and pp_type_diff fmt { tname; tdiff } =
  match tdiff with
  | Added _ -> Format.fprintf fmt "Type (%s, Added)" tname
  | Removed _ -> Format.fprintf fmt "Type (%s, Removed)" tname
  | Modified _ -> Format.fprintf fmt "Type (%s, Modified)" tname

and pp_module_diff fmt { mname; mdiff } =
  match mdiff with
  | Added _ -> Format.fprintf fmt "Module %s: Added" mname
  | Removed _ -> Format.fprintf fmt "Module %s: Removed" mname
  | Modified mdiff ->
      Format.fprintf fmt "Module %s: {Modified (%a)}" mname
        pp_module_modification mdiff

and pp_module_type_diff fmt { mtname; mtdiff } =
  match mtdiff with
  | Added _ -> Format.fprintf fmt "Module_type %s: Added" mtname
  | Removed _ -> Format.fprintf fmt "Module_type %s: Removed" mtname
  | Modified mtdiff ->
      Format.fprintf fmt "Module_type %s: {Modified (%a)}" mtname
        pp_module_modification mtdiff

and pp_class_diff fmt { cname; cdiff } =
  match cdiff with
  | Added _ -> Format.fprintf fmt "Class (%s, Added)" cname
  | Removed _ -> Format.fprintf fmt "Class (%s, Removed)" cname
  | Modified _ -> Format.fprintf fmt "Class (%s, Modified)" cname

let pp_diff_option fmt = function
  | None -> Format.fprintf fmt "None"
  | Some module_diff ->
      Format.fprintf fmt "Some (%a)" pp_module_diff module_diff

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
