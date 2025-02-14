type ('item, 'diff) entry =
  | Added of 'item
  | Removed of 'item
  | Modified of 'diff

type 'a atomic_modification = { reference : 'a; current : 'a }
type 'item atomic_entry = ('item, 'item atomic_modification) entry

type ('item, 'diff) map = {
  same : 'item String_map.t;
  different : ('item, 'diff) entry String_map.t;
}

type ('same, 'change) maybe_changed = Same of 'same | Changed of 'change

type 'same atomic_maybe_changed =
  ('same, 'same atomic_modification) maybe_changed

type 'same atomic_maybe_changed_entry =
  ('same, 'same atomic_entry) maybe_changed

type ('same, 'diff) option_ = ('same option, ('same, 'diff) entry) maybe_changed
type 'same atomic_option = ('same, 'same atomic_modification) option_

type ('same, 'diff) atomic_variant =
  ('same, ('same atomic_maybe_changed, 'diff) maybe_changed) maybe_changed

type ('same, 'diff) list_ = ('same list, 'diff list) maybe_changed

and type_modification = {
  type_kind : (Types.type_decl_kind, type_kind) atomic_variant;
  type_privacy : (Asttypes.private_flag, type_privacy) maybe_changed;
  type_manifest : Types.type_expr atomic_option;
  type_params : (Types.type_expr, type_param) list_;
}

and type_kind =
  | Record_tk of (Types.label_declaration, record_field) map
  | Variant_tk of
      ( Types.constructor_declaration,
        (Types.constructor_declaration, cstr_args) atomic_variant )
      map

and record_field = {
  type_expr : Types.type_expr atomic_maybe_changed;
  mutable_flag : (Asttypes.mutable_flag, field_mutability) maybe_changed;
}

and field_mutability = Added_m | Removed_m

and cstr_args =
  | Record_c of (Types.label_declaration, record_field) map
  | Tuple_c of
      (Types.type_expr, Types.type_expr atomic_maybe_changed_entry) list_

and type_privacy = Added_p | Removed_p

and type_param = (Types.type_expr, type_param_diff) maybe_changed

and type_param_diff =
  | Added_tp of Types.type_expr
  | Removed_tp of Types.type_expr

type module_ = {
  mname : string;
  mdiff : (Types.module_declaration, signature_modification) entry;
}

and signature_modification =
  | Unsupported
  | Supported of (Types.signature_item, sig_item) map

and sig_item =
  | Value of Types.value_description atomic_entry
  | Class of Types.class_declaration atomic_entry
  | Classtype of Types.class_type_declaration atomic_entry
  | Type of (Types.type_declaration, type_modification) entry
  | Module of (Types.module_declaration, signature_modification) entry
  | Modtype of (Types.modtype_declaration, signature_modification) entry

val interface :
  module_name:string ->
  reference:Types.signature ->
  current:Types.signature ->
  module_ option

val library :
  reference:Library.t -> current:Library.t -> module_ option String_map.t
