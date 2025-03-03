module rec TypeDecl : sig
  module Field : sig
    type t = { id : Ident.t; mutable_ : bool; type_ : Types.type_expr }
  end

  module Constructor : sig
    type args = Tuple of Types.type_expr list | Record of Field.t list
    type t = { id : Ident.t; args : args }
  end

  type param = { type_expr : Types.type_expr }

  module Kind : sig
    type definition =
      | Open
      | Record of Field.t list
      | Variant of Constructor.t list

    type t =
      | Abstract
      | Alias of { type_expr : Types.type_expr; private_ : bool }
      | Concrete of {
          manifest : Types.type_expr option;
          private_ : bool;
          definition : definition;
        }
  end

  type t = { params : param list; kind : Kind.t }
end =
  TypeDecl
