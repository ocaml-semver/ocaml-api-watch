type type_modification = {
  type_kind : (Types.type_decl_kind, type_kind) Stddiff.maybe_changed;
  type_privacy : (Asttypes.private_flag, type_privacy) Stddiff.maybe_changed;
  type_manifest : Types.type_expr Stddiff.atomic_option;
  type_params : (Types.type_expr, type_param) Stddiff.list_;
}

and type_kind =
  | Record_tk of (Types.label_declaration, label) Stddiff.map
  | Variant_tk of (Types.constructor_declaration, cstr_args) Stddiff.map
  | Atomic_tk of Types.type_decl_kind Stddiff.atomic_modification

and label = {
  label_type : Types.type_expr Stddiff.maybe_changed_atomic;
  label_mutable :
    (Asttypes.mutable_flag, field_mutability) Stddiff.maybe_changed;
}

and field_mutability = Added_m | Removed_m

and cstr_args =
  | Record_cstr of (Types.label_declaration, label) Stddiff.map
  | Tuple_cstr of Types.type_expr Stddiff.maybe_changed_atomic_entry list
  | Atomic_cstr of Types.constructor_arguments Stddiff.atomic_modification

and type_privacy = Added_p | Removed_p
and type_param = (Types.type_expr, type_param_diff) Stddiff.maybe_changed

and type_param_diff =
  | Added_tp of Types.type_expr
  | Removed_tp of Types.type_expr

type type_ = {
  tname : string;
  tdiff : (Types.type_declaration, type_modification) Stddiff.entry;
}

type value = {
  vname : string;
  vdiff : Types.value_description Stddiff.atomic_entry;
}

type class_ = {
  cname : string;
  cdiff : Types.class_declaration Stddiff.atomic_entry;
}

type cltype = {
  ctname : string;
  ctdiff : Types.class_type_declaration Stddiff.atomic_entry;
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

val interface :
  module_name:string ->
  reference:Types.signature ->
  current:Types.signature ->
  module_ option

val library :
  reference:Types.signature String_map.t ->
  current:Types.signature String_map.t ->
  module_ option String_map.t
