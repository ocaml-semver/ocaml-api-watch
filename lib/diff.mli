type type_expr =
  | Tuple of tuple
  | Arrow of arrow
  | Constr of constr
  | Atomic of Types.type_expr Stddiff.atomic_modification

and tuple = (Types.type_expr, type_expr) Stddiff.List.t

and arrow = {
  arg_label :
    ( arg_label option,
      (arg_label, arg_label_diff) Stddiff.Option.t )
    Stddiff.maybe_changed;
  arg_type : (Types.type_expr, type_expr) Stddiff.maybe_changed;
  return_type : (Types.type_expr, type_expr) Stddiff.maybe_changed;
}

and arg_label = Labelled_arg of string | Optional_arg of string

and arg_label_diff = {
  name : (string, string Stddiff.atomic_modification) Stddiff.maybe_changed;
  arg_optional : (bool, arg_optional) Stddiff.maybe_changed;
}

and arg_optional = Added_opt_arg | Removed_opt_arg

and constr = {
  path : (Path.t, Path.t Stddiff.atomic_modification) Stddiff.maybe_changed;
  args :
    ( Types.type_expr list,
      ( Types.type_expr list,
        (Types.type_expr, type_expr) Stddiff.List.t )
      Stddiff.entry )
    Stddiff.maybe_changed;
}

type type_modification = {
  type_kind : (Types.type_decl_kind, type_kind) Stddiff.maybe_changed;
  type_privacy : (Asttypes.private_flag, type_privacy) Stddiff.maybe_changed;
  type_manifest :
    ( Types.type_expr option,
      (Types.type_expr, type_expr) Stddiff.Option.t )
    Stddiff.maybe_changed;
  type_params :
    ( Types.type_expr list,
      ( Types.type_expr list,
        (Types.type_expr, type_expr) Stddiff.List.t )
      Stddiff.entry )
    Stddiff.maybe_changed;
}

and type_kind =
  | Record_tk of (Types.label_declaration, label) Stddiff.Map.t
  | Variant_tk of (Types.constructor_declaration, cstr_args) Stddiff.Map.t
  | Atomic_tk of Types.type_decl_kind Stddiff.atomic_modification

and label = {
  label_type : (Types.type_expr, type_expr) Stddiff.maybe_changed;
  label_mutable :
    (Asttypes.mutable_flag, field_mutability) Stddiff.maybe_changed;
}

and field_mutability = Added_m | Removed_m

and cstr_args =
  | Record_cstr of (Types.label_declaration, label) Stddiff.Map.t
  | Tuple_cstr of tuple
  | Atomic_cstr of Types.constructor_arguments Stddiff.atomic_modification

and type_privacy = Added_p | Removed_p

type type_ = {
  tname : string;
  tdiff : (Types.type_declaration, type_modification) Stddiff.entry;
}

type value = {
  vname : string;
  vdiff : (Types.value_description, type_expr) Stddiff.entry;
}

type class_ = {
  cname : string;
  cdiff : Types.class_declaration Stddiff.atomic_entry;
}

type cltype = {
  ctname : string;
  ctdiff : Types.class_type_declaration Stddiff.atomic_entry;
}

type extcstr = {
  ecname : string;
  ectname : string;
  ecexn : bool;
  ecdiff : (Types.extension_constructor, extcstr_modification) Stddiff.entry;
}

and extcstr_modification = {
  extcstr_params :
    ( Types.type_expr list,
      ( Types.type_expr list,
        (Types.type_expr, type_expr) Stddiff.List.t )
      Stddiff.entry )
    Stddiff.maybe_changed;
  extcstr_private : (Asttypes.private_flag, type_privacy) Stddiff.maybe_changed;
  extcstr_args : (Types.constructor_arguments, cstr_args) Stddiff.maybe_changed;
}

type module_ = {
  mname : string;
  mdiff : (Types.module_declaration, signature_modification) Stddiff.entry;
}

and modtype = {
  mtname : string;
  mtdiff : (Types.modtype_declaration, signature_modification) Stddiff.entry;
}

and signature_modification = Unsupported | Supported of sig_item list

and sig_item =
  | Value of value
  | Module of module_
  | Type of type_
  | Modtype of modtype
  | Class of class_
  | Classtype of cltype
  | Extcstr of extcstr

val interface :
  module_name:string ->
  reference:Types.signature ->
  current:Types.signature ->
  module_ option

val library :
  reference:Types.signature String_map.t ->
  current:Types.signature String_map.t ->
  module_ option String_map.t
