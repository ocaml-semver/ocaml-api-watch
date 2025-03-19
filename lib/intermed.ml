module Type_decl = struct
  module Field = struct
    type t = { name : string; mutable_ : bool; type_ : Types.type_expr }
  end

  module Constructor = struct
    type args = Tuple of Types.type_expr list | Record of Field.t list
    type t = { name : string; args : args }
  end

  type param = Types.type_expr

  module Kind = struct
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

  let bool_of_private = function
    | Asttypes.Private -> true
    | Asttypes.Public -> false

  let bool_of_mutable = function
    | Asttypes.Mutable -> true
    | Asttypes.Immutable -> false

  let field lbl =
    {
      Field.name = Ident.name lbl.Types.ld_id;
      mutable_ = bool_of_mutable lbl.ld_mutable;
      type_ = lbl.ld_type;
    }

  let cstr_args cd_args =
    match cd_args with
    | Types.Cstr_tuple type_exprs -> Constructor.Tuple type_exprs
    | Types.Cstr_record lbls -> Constructor.Record (List.map field lbls)

  let cstr cd =
    {
      Constructor.name = Ident.name cd.Types.cd_id;
      args = cstr_args cd.cd_args;
    }

  let from_type_declaration td =
    let params = td.Types.type_params in
    let kind =
      match (td.type_manifest, td.type_kind) with
      | None, Types.Type_abstract _ -> Kind.Abstract
      | Some type_expr, Type_abstract _ ->
          Alias { type_expr; private_ = bool_of_private td.type_private }
      | manifest, Type_open ->
          Concrete
            {
              manifest;
              private_ = bool_of_private td.type_private;
              definition = Open;
            }
      | manifest, Type_record (lbls, _) ->
          Concrete
            {
              manifest;
              private_ = bool_of_private td.type_private;
              definition = Record (List.map field lbls);
            }
      | manifest, Type_variant (cstrs, _) ->
          Concrete
            {
              manifest;
              private_ = bool_of_private td.type_private;
              definition = Variant (List.map cstr cstrs);
            }
    in
    { params; kind }
end
