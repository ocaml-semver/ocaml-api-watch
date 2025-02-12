type ('item, 'diff) t = Added of 'item | Removed of 'item | Modified of 'diff
type 'a atomic_modification = { reference : 'a; current : 'a }

type 'item atomic_t = ('item, 'item atomic_modification) t

type ('same, 'different) maybe_changed =
  | Same of 'same
  | Different of 'different

type 'same atomic_maybe_changed =
  ('same, 'same atomic_modification) maybe_changed

type ('same, 'diff) option_ =
  ('same option, ('same, 'diff) t) maybe_changed

type 'same atomic_option =
  ('same, 'same atomic_modification) option_

type ('same, 'diff) atomic_variant =
  ('same, ('same atomic_maybe_changed, 'diff) maybe_changed) maybe_changed

type ('same, 'diff) list_ = ('same list, 'diff list) maybe_changed

type value = {
  vname : string;
  vdiff : Types.value_description atomic_t;
}

type type_ = {
  tname : string;
  tdiff : (Types.type_declaration, type_modification) t;
}

and type_modification = {
  type_kind : (Types.type_decl_kind, type_kind) atomic_variant;
  type_privacy : (Asttypes.private_flag, type_privacy) maybe_changed;
  type_manifest : Types.type_expr atomic_option;
  type_params: (Types.type_expr, type_param) list_
}

and type_param = (Types.type_expr, type_param_diff) maybe_changed

and type_param_diff =
  | Added_tp of Types.type_expr
  | Removed_tp of Types.type_expr

and type_privacy = Added_p | Removed_p

and type_kind =
  | Record_tk of record_field list
  | Variant_tk of constructor_ list

and record_field = {
  rname : string;
  rdiff : Types.label_declaration atomic_t;
}

and constructor_ = {
  csname : string;
  csdiff : (Types.constructor_declaration, constructor_modification) atomic_variant;
}

and constructor_modification =
  | Record_c of record_field list
  | Tuple_c of tuple_component list

and tuple_component = Types.type_expr atomic_maybe_changed

type class_ = {
  cname : string;
  cdiff : Types.class_declaration atomic_t;
}

and cltype = {
  ctname : string;
  ctdiff : Types.class_type_declaration atomic_t;
}

type module_ = {
  mname : string;
  mdiff : (Types.module_declaration, signature_modification) t;
}

and modtype = {
  mtname : string;
  mtdiff : (Types.modtype_declaration, signature_modification) t;
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
  reference:Library.t -> current:Library.t -> module_ option String_map.t
