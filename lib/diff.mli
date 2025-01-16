type ('item, 'diff) t = Added of 'item | Removed of 'item | Modified of 'diff
type 'a atomic_modification = { reference : 'a; current : 'a }

type value = {
  vname : string;
  vdiff :
    (Types.value_description, Types.value_description atomic_modification) t;
}

type class_modification = Unsupported

and class_ = {
  cname : string;
  cdiff : (Types.class_declaration, class_modification) t;
}

type class_type_modification = Unsupported

and cltype = {
  ctname : string;
  ctdiff : (Types.class_type_declaration, class_type_modification) t;
}

type type_modification =
  | Compound of record_field list
  | Atomic of Types.type_declaration atomic_modification

and record_field = {
  lname : string;
  ldiff :
    (Types.label_declaration, Types.label_declaration atomic_modification) t;
}

type type_ = {
  tname : string;
  tdiff : (Types.type_declaration, type_modification) t;
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
