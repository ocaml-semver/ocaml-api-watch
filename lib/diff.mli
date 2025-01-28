type ('item, 'diff) t = Added of 'item | Removed of 'item | Modified of 'diff
type 'a atomic_modification = { reference : 'a; current : 'a }

type value = {
  vname : string;
  vdiff :
    (Types.value_description, Types.value_description atomic_modification) t;
}

type type_ = {
  tname : string;
  tdiff : (Types.type_declaration, type_modification) t;
}

and type_modification = { type_kind_mismatch : type_kind_mismatch option }

and type_kind_mismatch =
  | Record_mismatch of record_field list
  | Variant_mismatch of constructor_ list
  | Atomic_mismatch of Types.type_decl_kind atomic_modification

and record_field = {
  rname : string;
  rdiff :
    (Types.label_declaration, Types.label_declaration atomic_modification) t;
}

and constructor_ = {
  csname : string;
  csdiff : (Types.constructor_declaration, constructor_modification) t;
}

and constructor_modification =
  | Record_c of record_field list
  | Tuple_c of tuple_component list
  | Atomic_c of Types.constructor_declaration atomic_modification

and tuple_component =
  ( Types.type_expr,
    (Types.type_expr, Types.type_expr atomic_modification) t )
  Either.t

and type_param = (Types.type_expr, type_param_diff) Either.t

and type_param_diff =
  | Added_tp of Types.type_expr
  | Removed_tp of Types.type_expr

type class_ = {
  cname : string;
  cdiff :
    (Types.class_declaration, Types.class_declaration atomic_modification) t;
}

and cltype = {
  ctname : string;
  ctdiff :
    ( Types.class_type_declaration,
      Types.class_type_declaration atomic_modification )
    t;
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
