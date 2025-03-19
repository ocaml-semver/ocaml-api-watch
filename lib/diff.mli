module Type_decl : sig
  module Field : sig
    type mutable_change = Added | Removed

    type t = {
      mutable_ : (bool, mutable_change) Stddiff.maybe_changed;
      type_ : Types.type_expr Stddiff.maybe_changed_atomic;
    }
  end

  module Constructor : sig
    type args =
      | Record of (Intermed.Type_decl.Field.t, Field.t) Stddiff.map
      | Tuple of Types.type_expr Stddiff.maybe_changed_atomic_entry list
      | Atomic of
          Intermed.Type_decl.Constructor.args Stddiff.atomic_modification

    type t = { args : args }
  end

  module Kind : sig
    type private_change = Added | Removed

    type definition =
      | Record of (Intermed.Type_decl.Field.t, Field.t) Stddiff.map
      | Variant of (Intermed.Type_decl.Constructor.t, Constructor.t) Stddiff.map
      | Atomic_definition of
          Intermed.Type_decl.Kind.definition Stddiff.atomic_modification

    type t =
      | Alias of {
          type_expr : Types.type_expr Stddiff.maybe_changed_atomic;
          private_ : (bool, private_change) Stddiff.maybe_changed;
        }
      | Concrete of {
          manifest : Types.type_expr Stddiff.atomic_option;
          private_ : (bool, private_change) Stddiff.maybe_changed;
          definition :
            ( Intermed.Type_decl.Kind.definition,
              definition )
            Stddiff.maybe_changed;
        }
      | Atomic of Intermed.Type_decl.Kind.t Stddiff.atomic_modification
  end

  module Param : sig
    type param_change =
      | Added of Intermed.Type_decl.param
      | Removed of Intermed.Type_decl.param

    type t = (Intermed.Type_decl.param, param_change) Stddiff.maybe_changed
  end

  type t = {
    params : (Intermed.Type_decl.param, Param.t) Stddiff.list_;
    kind : (Intermed.Type_decl.Kind.t, Kind.t) Stddiff.maybe_changed;
  }
end

type type_ = {
  tname : string;
  tdiff : (Intermed.Type_decl.t, Type_decl.t) Stddiff.entry;
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
