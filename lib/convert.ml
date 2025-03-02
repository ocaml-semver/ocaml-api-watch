module T = Types
module TD = Intermed.TypeDecl

let bool_of_private = function
  | Asttypes.Private -> true
  | Asttypes.Public -> false

let bool_of_mutable = function
  | Asttypes.Mutable -> true
  | Asttypes.Immutable -> false

let field lbl =
  {
    TD.Field.id = lbl.T.ld_id;
    mutable_ = bool_of_mutable lbl.ld_mutable;
    type_ = lbl.ld_type;
  }

let cstr_args cd_args =
  match cd_args with
  | T.Cstr_tuple type_exprs -> TD.Constructor.Tuple type_exprs
  | T.Cstr_record lbls -> Record (List.map field lbls)

let cstr cd = { TD.Constructor.id = cd.T.cd_id; args = cstr_args cd.cd_args }
and param p = { TD.type_expr = p }

let type_declaration ~src =
  let params = src.T.type_params in
  let kind =
    match (src.type_manifest, src.type_kind) with
    | None, T.Type_abstract _ -> TD.Kind.Abstract
    | Some manifest, Type_abstract _ ->
        Alias { manifest; private_ = bool_of_private src.type_private }
    | manifest, Type_open ->
        Concrete
          {
            manifest;
            private_ = bool_of_private src.type_private;
            definition = Open;
          }
    | manifest, Type_record (lbls, _) ->
        Concrete
          {
            manifest;
            private_ = bool_of_private src.type_private;
            definition = Record (List.map field lbls);
          }
    | manifest, Type_variant (cstrs, _) ->
        Concrete
          {
            manifest;
            private_ = bool_of_private src.type_private;
            definition = Variant (List.map cstr cstrs);
          }
  in
  { TD.params = List.map param params; kind }
