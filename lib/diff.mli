(** Represent a change of an entry in a collection *)
type ('item, 'diff) entry =
  | Added of 'item
  | Removed of 'item
  | Modified of 'diff

type 'a atomic_modification = { reference : 'a; current : 'a }
(** The simplest diff representation for the modification of a value of type 'a.
    [reference] is the value before and [current] is the value after the change
    occured. Use this type when there is no better representation available. *)

type ('item, 'diff) map = {
  same_map : 'item String_map.t;
  changed_map : ('item, 'diff) entry String_map.t;
}

type ('same, 'change) maybe_changed = Same of 'same | Changed of 'change

(** Type aliases for representing common OCaml types diffs *)
module Types_ : sig
  type 'item atomic_entry = ('item, 'item atomic_modification) entry

  type 'same maybe_changed_atomic =
    ('same, 'same atomic_modification) maybe_changed

  type 'same maybe_changed_atomic_entry =
    ('same, 'same atomic_entry) maybe_changed

  type ('same, 'diff) option_ =
    ('same option, ('same, 'diff) entry) maybe_changed

  type 'same atomic_option = ('same, 'same atomic_modification) option_

  type ('same, 'diff) variant =
    | Same_variant of 'same
    | Different_variant of 'diff

  type ('same, 'diff) atomic_variant =
    ('same, ('diff, 'same atomic_modification) variant) maybe_changed

  type ('same, 'diff) list_ = ('same list, 'diff list) maybe_changed
end

type type_modification = {
  type_kind : (Types.type_decl_kind, type_kind) Types_.atomic_variant;
  type_privacy : (Asttypes.private_flag, type_privacy) maybe_changed;
  type_manifest : Types.type_expr Types_.atomic_option;
  type_params : (Types.type_expr, type_param) Types_.list_;
}

and type_kind =
  | Record_tk of (Types.label_declaration, label) map
  | Variant_tk of (Types.constructor_declaration, cstr_args) map

and label = {
  label_type : Types.type_expr Types_.maybe_changed_atomic;
  label_mutable : (Asttypes.mutable_flag, field_mutability) maybe_changed;
}

and field_mutability = Added_m | Removed_m

and cstr_args =
  | Record_cstr of (Types.label_declaration, label) map
  | Tuple_cstr of Types.type_expr Types_.maybe_changed_atomic_entry list
  | Atomic_cstr of Types.constructor_declaration atomic_modification

and type_privacy = Added_p | Removed_p
and type_param = (Types.type_expr, type_param_diff) maybe_changed

and type_param_diff =
  | Added_tp of Types.type_expr
  | Removed_tp of Types.type_expr

type type_ = {
  tname : string;
  tdiff : (Types.type_declaration, type_modification) entry;
}

type value = {
  vname : string;
  vdiff : Types.value_description Types_.atomic_entry;
}

type class_ = {
  cname : string;
  cdiff : Types.class_declaration Types_.atomic_entry;
}

type cltype = {
  ctname : string;
  ctdiff : Types.class_type_declaration Types_.atomic_entry;
}

type module_ = {
  mname : string;
  mdiff : (Types.module_declaration, signature_modification) entry;
}

and modtype = {
  mtname : string;
  mtdiff : (Types.modtype_declaration, signature_modification) entry;
}

and signature_modification = Unsupported | Supported of sig_item list

and sig_item =
  | Value of value
  | Module of module_
  | Type of type_
  | Modtype of modtype
  | Class of class_
  | Classtype of cltype
