type ('item, 'diff) t = Added of 'item | Removed of 'item | Modified of 'diff
type 'a atomic_modification = { reference : 'a; current : 'a }

type value = {
  vname : string;
  vdiff :
    (Types.value_description, Types.value_description atomic_modification) t;
}

type class_ = {
  cname : string;
  cdiff : (Types.class_declaration, class_modification) t;
}

and class_modification = Unsupported

type type_ = {
  tname : string;
  tdiff : (Types.type_declaration, Types.type_declaration atomic_modification) t;
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

val interface :
  module_name:string ->
  reference:Types.signature ->
  current:Types.signature ->
  module_ option
