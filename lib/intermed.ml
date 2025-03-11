module rec TypeDecl : sig
  module Field : sig
    type t = { name : string; mutable_ : bool; type_ : Types.type_expr }
  end

  module Constructor : sig
    type args = Tuple of Types.type_expr list | Record of Field.t list
    type t = { name : string; args : args }
  end

  type param = { type_expr : Types.type_expr }

  module Kind : sig
    type definition =
      | Open
      | Record of Field.t list
      | Variant of Constructor.t list

    type alias = { type_expr : Types.type_expr; private_ : bool }

    type concrete = {
      manifest : Types.type_expr option;
      private_ : bool;
      definition : definition;
    }

    type t = Abstract | Alias of alias | Concrete of concrete
  end

  type t = { params : param list; kind : Kind.t }
end =
  TypeDecl
