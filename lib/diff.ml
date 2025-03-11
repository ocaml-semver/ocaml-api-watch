open Types
open Stddiff
open Intermed

let type_expr ~typing_env ?(ref_params = []) ?(cur_params = []) ~reference
    ~current () =
  let normed_ref, normed_cur =
    Normalize.params_arity ~reference:ref_params ~current:cur_params
  in
  if
    Ctype.is_equal typing_env true
      (List.map (fun param -> param.TypeDecl.type_expr) normed_ref
      @ [ reference ])
      (List.map (fun param -> param.TypeDecl.type_expr) normed_cur @ [ current ])
  then None
  else Some { reference; current }

module TypeDecl = struct
  module TD = TypeDecl
  module K = TD.Kind
  module F = TD.Field
  module C = TD.Constructor

  module Field = struct
    type mutable_change = Added | Removed

    type t = {
      mutable_ : (bool, mutable_change) maybe_changed;
      type_ : type_expr maybe_changed_atomic;
    }

    let mutable_ ~reference ~current =
      match (reference, current) with
      | true, true | false, false -> Same reference
      | true, false -> Changed Removed
      | false, true -> Changed Added

    let field ~typing_env ~ref_params ~cur_params reference current =
      let mutable_ =
        mutable_ ~reference:reference.F.mutable_ ~current:current.F.mutable_
      in
      let type_ =
        type_expr ~typing_env ~ref_params ~cur_params
          ~reference:reference.F.type_ ~current:current.F.type_ ()
      in
      match (mutable_, type_) with
      | Same _, None -> None
      | _, None -> Some { mutable_; type_ = Same reference.type_ }
      | _, Some type_change -> Some { mutable_; type_ = Changed type_change }
  end

  module Constructor = struct
    type args =
      | Record of (F.t, Field.t) map
      | Tuple of type_expr maybe_changed_atomic_entry list
      | Unshared of C.args atomic_modification

    type t = { args : args }

    let build_field_map fields =
      List.fold_left
        (fun map field -> String_map.add field.F.name field map)
        String_map.empty fields

    let fields ~typing_env ~ref_params ~cur_params ~reference ~current =
      let ref_fields_map = build_field_map reference in
      let cur_fields_map = build_field_map current in
      let fields_map =
        diff_map
          ~diff_one:(Field.field ~typing_env ~ref_params ~cur_params)
          ~ref_map:ref_fields_map ~cur_map:cur_fields_map
      in
      if String_map.is_empty fields_map.changed_map then None
      else Some (Record fields_map)

    let tuple ~typing_env ~ref_params ~cur_params ~reference ~current =
      let tuple =
        diff_list
          ~diff_one:(fun t1 t2 ->
            match (t1, t2) with
            | None, None -> assert false
            | Some t1, None -> Changed (Removed t1)
            | None, Some t2 -> Changed (Added t2)
            | Some t1, Some t2 -> (
                match
                  type_expr ~typing_env ~ref_params ~cur_params ~reference:t1
                    ~current:t2 ()
                with
                | None -> Same t1
                | Some diff -> Changed (Modified diff)))
          ~ref_list:reference ~cur_list:current
      in
      match tuple with
      | Same _ -> None
      | Changed tuple_change -> Some (Tuple tuple_change)

    let args ~typing_env ~ref_params ~cur_params ~reference ~current =
      match (reference, current) with
      | C.Record ref_fields, C.Record cur_fields ->
          fields ~typing_env ~ref_params ~cur_params ~reference:ref_fields
            ~current:cur_fields
      | C.Tuple ref_tuple, C.Tuple cur_tuple ->
          tuple ~typing_env ~ref_params ~cur_params ~reference:ref_tuple
            ~current:cur_tuple
      | _ -> Some (Unshared { reference; current })

    let cstr ~typing_env ~ref_params ~cur_params reference current =
      let args =
        args ~typing_env ~ref_params ~cur_params ~reference:reference.C.args
          ~current:current.C.args
      in
      Option.map (fun args -> { args }) args
  end

  module Kind = struct
    type private_change = Added | Removed

    type definition =
      | Record of (F.t, Field.t) map
      | Variant of (C.t, Constructor.t) map
      | Unshared_definition of K.definition atomic_modification

    type t =
      | Alias of {
          type_expr : type_expr maybe_changed_atomic;
          private_ : (bool, private_change) maybe_changed;
        }
      | Concrete of {
          manifest : type_expr atomic_option;
          private_ : (bool, private_change) maybe_changed;
          definition : (K.definition, definition) maybe_changed;
        }
      | Unshared of K.t atomic_modification

    let private_ ~reference ~current =
      match (reference, current) with
      | true, true | false, false -> Same reference
      | true, false -> Changed Removed
      | false, true -> Changed Added

    let alias ~typing_env ~ref_params ~cur_params ~reference ~current =
      let type_expr =
        type_expr ~typing_env ~ref_params ~cur_params
          ~reference:reference.K.type_expr ~current:current.K.type_expr ()
      in
      let private_ =
        private_ ~reference:reference.private_ ~current:current.private_
      in
      match (type_expr, private_) with
      | None, Same _ -> Same (K.Alias reference)
      | Some type_expr_change, private_ ->
          Changed (Alias { type_expr = Changed type_expr_change; private_ })
      | None, private_ ->
          Changed (Alias { type_expr = Same reference.type_expr; private_ })

    let manifest ~typing_env ~ref_params ~cur_params ~reference ~current =
      match (reference, current) with
      | None, None -> Same None
      | Some ref_type_expr, None -> Changed (Stddiff.Removed ref_type_expr)
      | None, Some cur_type_expr -> Changed (Stddiff.Added cur_type_expr)
      | Some ref_type_expr, Some cur_type_expr -> (
          match
            type_expr ~typing_env ~ref_params ~cur_params
              ~reference:ref_type_expr ~current:cur_type_expr ()
          with
          | None -> Same (Some ref_type_expr)
          | Some type_expr_change -> Changed (Modified type_expr_change))

    let build_cstr_map cstrs =
      List.fold_left
        (fun map cstr -> String_map.add cstr.C.name cstr map)
        String_map.empty cstrs

    let build_field_map fields =
      List.fold_left
        (fun map field -> String_map.add field.F.name field map)
        String_map.empty fields

    let fields ~typing_env ~ref_params ~cur_params ~reference ~current =
      let ref_fields_map = build_field_map reference in
      let cur_fields_map = build_field_map current in
      let fields_map =
        diff_map
          ~diff_one:(Field.field ~typing_env ~ref_params ~cur_params)
          ~ref_map:ref_fields_map ~cur_map:cur_fields_map
      in
      if String_map.is_empty fields_map.changed_map then
        Same (K.Record reference)
      else Changed (Record fields_map)

    let cstrs ~typing_env ~ref_params ~cur_params ~reference ~current =
      let ref_cstrs_map = build_cstr_map reference in
      let cur_cstrs_map = build_cstr_map current in
      let cstrs_map =
        diff_map
          ~diff_one:(Constructor.cstr ~typing_env ~ref_params ~cur_params)
          ~ref_map:ref_cstrs_map ~cur_map:cur_cstrs_map
      in
      if String_map.is_empty cstrs_map.changed_map then
        Same (K.Variant reference)
      else Changed (Variant cstrs_map)

    let definition ~typing_env ~ref_params ~cur_params ~reference ~current =
      match (reference, current) with
      | K.Open, K.Open -> Same K.Open
      | Record ref_fields, Record cur_fields ->
          fields ~typing_env ~ref_params ~cur_params ~reference:ref_fields
            ~current:cur_fields
      | Variant ref_cstrs, Variant cur_cstrs ->
          cstrs ~typing_env ~ref_params ~cur_params ~reference:ref_cstrs
            ~current:cur_cstrs
      | ref_definition, cur_definition ->
          Changed
            (Unshared_definition
               { reference = ref_definition; current = cur_definition })

    let concrete ~typing_env ~ref_params ~cur_params ~reference ~current =
      let manifest =
        manifest ~typing_env ~ref_params ~cur_params
          ~reference:reference.K.manifest ~current:current.K.manifest
      in
      let private_ =
        private_ ~reference:reference.private_ ~current:current.private_
      in
      let definition =
        definition ~typing_env ~ref_params ~cur_params
          ~reference:reference.definition ~current:current.definition
      in
      match (manifest, private_, definition) with
      | Same manifest, Same private_, Same definition ->
          Same (K.Concrete { manifest; private_; definition })
      | _ -> Changed (Concrete { manifest; private_; definition })

    let kind ~typing_env ~ref_params ~cur_params ~reference ~current =
      match (reference, current) with
      | K.Abstract, K.Abstract -> Same reference
      | K.Alias ref_alias, K.Alias cur_alias ->
          alias ~typing_env ~ref_params ~cur_params ~reference:ref_alias
            ~current:cur_alias
      | K.Concrete ref_concrete, K.Concrete cur_concrete ->
          concrete ~typing_env ~ref_params ~cur_params ~reference:ref_concrete
            ~current:cur_concrete
      | ref_kind, cur_kind ->
          Changed (Unshared { reference = ref_kind; current = cur_kind })
  end

  module Param = struct
    type param_change = Added of TD.param | Removed of TD.param
    type t = (TD.param, param_change) maybe_changed

    let params ~reference ~current =
      diff_list
        ~diff_one:(fun ref_param cur_param ->
          match (ref_param, cur_param) with
          | None, None -> assert false
          | Some param, None -> Changed (Removed param)
          | None, Some param -> Changed (Added param)
          | Some param, Some _ -> Same param)
        ~ref_list:reference ~cur_list:current
  end

  type t = {
    params : (TD.param, Param.t) list_;
    kind : (K.t, Kind.t) maybe_changed;
  }
end

type type_ = { tname : string; tdiff : (Intermed.TypeDecl.t, TypeDecl.t) entry }
type value = { vname : string; vdiff : value_description atomic_entry }
type class_ = { cname : string; cdiff : class_declaration atomic_entry }
type cltype = { ctname : string; ctdiff : class_type_declaration atomic_entry }

type module_ = {
  mname : string;
  mdiff : (module_declaration, signature_modification) entry;
}

and modtype = {
  mtname : string;
  mtdiff : (modtype_declaration, signature_modification) entry;
}

and signature_modification = Unsupported | Supported of sig_item list

and sig_item =
  | Value of value
  | Module of module_
  | Type of type_
  | Modtype of modtype
  | Class of class_
  | Classtype of cltype

let extract_items items =
  List.fold_left
    (fun tbl item ->
      match item with
      | Sig_module (id, _, mod_decl, _, Exported) ->
          Sig_item_map.add ~name:(Ident.name id) Sig_item_map.Module mod_decl
            tbl
      | Sig_modtype (id, mtd_decl, Exported) ->
          Sig_item_map.add ~name:(Ident.name id) Sig_item_map.Modtype mtd_decl
            tbl
      | Sig_value (id, val_des, Exported) ->
          Sig_item_map.add ~name:(Ident.name id) Sig_item_map.Value val_des tbl
      | Sig_type (id, type_decl, _, Exported) ->
          if
            Sig_item_map.has ~name:(Ident.name id) Sig_item_map.Class tbl
            || Sig_item_map.has ~name:(Ident.name id) Sig_item_map.Classtype tbl
          then tbl
          else
            Sig_item_map.add ~name:(Ident.name id) Sig_item_map.Type
              (Convert.type_declaration type_decl)
              tbl
      | Sig_class (id, cls_decl, _, Exported) ->
          Sig_item_map.add ~name:(Ident.name id) Sig_item_map.Class cls_decl tbl
      | Sig_class_type (id, class_type_decl, _, Exported) ->
          if Sig_item_map.has ~name:(Ident.name id) Sig_item_map.Class tbl then
            tbl
          else
            Sig_item_map.add ~name:(Ident.name id) Sig_item_map.Classtype
              class_type_decl tbl
      | _ -> tbl)
    Sig_item_map.empty items

let module_type_fallback ~loc ~typing_env ~name ~reference ~current =
  let modtype_coercion1 () =
    Includemod.modtypes ~loc typing_env ~mark:Mark_both reference current
  in
  let modtype_coercion2 () =
    Includemod.modtypes ~loc typing_env ~mark:Mark_both current reference
  in
  match (modtype_coercion1 (), modtype_coercion2 ()) with
  | Tcoerce_none, Tcoerce_none -> None
  | _, _ -> Some (Module { mname = name; mdiff = Modified Unsupported })
  | exception Includemod.Error _ ->
      Some (Module { mname = name; mdiff = Modified Unsupported })

let type_decls ~typing_env ~name ~reference ~current =
  let module TD = Intermed.TypeDecl in
  let open TypeDecl in
  if
    Normalize.is_params ~reference:reference.TD.params
      ~current:current.TD.params
  then ()
  else Normalize.type_decls ~reference ~current;
  let ref_params = reference.TD.params in
  let cur_params = current.TD.params in
  let params =
    TypeDecl.Param.params ~reference:reference.params ~current:current.params
  in
  let kind =
    Kind.kind ~typing_env ~ref_params ~cur_params ~reference:reference.kind
      ~current:current.kind
  in
  match { params; kind } with
  | { params = Same _; kind = Same _ } -> None
  | diff -> Some (Type { tname = name; tdiff = Modified diff })

let type_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | Some reference, None ->
      Some (Type { tname = name; tdiff = Removed reference })
  | None, Some current -> Some (Type { tname = name; tdiff = Added current })
  | Some reference, Some current ->
      type_decls ~typing_env ~name ~reference ~current

let value_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | Some reference, None ->
      Some (Value { vname = name; vdiff = Removed reference })
  | None, Some current -> Some (Value { vname = name; vdiff = Added current })
  | Some reference, Some current -> (
      let val_coercion1 () =
        Includecore.value_descriptions ~loc:current.val_loc typing_env name
          current reference
      in
      let val_coercion2 () =
        Includecore.value_descriptions ~loc:reference.val_loc typing_env name
          reference current
      in
      match (val_coercion1 (), val_coercion2 ()) with
      | Tcoerce_none, Tcoerce_none -> None
      | _, _ ->
          Some (Value { vname = name; vdiff = Modified { reference; current } })
      | exception Includecore.Dont_match _ ->
          Some (Value { vname = name; vdiff = Modified { reference; current } })
      )

let class_item ~typing_env ~name ~(reference : class_declaration option)
    ~(current : class_declaration option) =
  match (reference, current) with
  | None, None -> None
  | None, Some curr_cls -> Some (Class { cname = name; cdiff = Added curr_cls })
  | Some ref_cls, None -> Some (Class { cname = name; cdiff = Removed ref_cls })
  | Some ref_cls, Some curr_cls -> (
      let cls_mismatch_lst =
        Includeclass.class_declarations typing_env ref_cls curr_cls
      in
      match cls_mismatch_lst with
      | [] -> None
      | _ ->
          Some
            (Class
               {
                 cname = name;
                 cdiff = Modified { reference = ref_cls; current = curr_cls };
               }))

let class_type_item ~typing_env ~name
    ~(reference : class_type_declaration option)
    ~(current : class_type_declaration option) =
  match (reference, current) with
  | None, None -> None
  | None, Some curr_class_type ->
      Some (Classtype { ctname = name; ctdiff = Added curr_class_type })
  | Some ref_class_type, None ->
      Some (Classtype { ctname = name; ctdiff = Removed ref_class_type })
  | Some ref_class_type, Some curr_class_type -> (
      let cls_type_mismatch_lst =
        Includeclass.class_type_declarations ~loc:ref_class_type.clty_loc
          typing_env ref_class_type curr_class_type
      in
      match cls_type_mismatch_lst with
      | [] -> None
      | _ ->
          Some
            (Classtype
               {
                 ctname = name;
                 ctdiff =
                   Modified
                     { reference = ref_class_type; current = curr_class_type };
               }))

let rec items ~reference ~current =
  let env = Typing_env.for_diff ~reference ~current in
  let ref_items = extract_items reference in
  let curr_items = extract_items current in
  let diff_item : type a. (a, 'diff) Sig_item_map.diff_item =
   fun item_type name reference current ->
    match item_type with
    | Value -> value_item ~typing_env:env ~name ~reference ~current
    | Module -> module_item ~typing_env:env ~name ~reference ~current
    | Modtype -> module_type_item ~typing_env:env ~name ~reference ~current
    | Type -> type_item ~typing_env:env ~name ~reference ~current
    | Class -> class_item ~typing_env:env ~name ~reference ~current
    | Classtype -> class_type_item ~typing_env:env ~name ~reference ~current
  in
  Sig_item_map.diff ~diff_item:{ diff_item } ref_items curr_items

and module_item ~typing_env ~name ~(reference : module_declaration option)
    ~(current : module_declaration option) =
  match (reference, current) with
  | None, None -> None
  | None, Some curr_md -> Some (Module { mname = name; mdiff = Added curr_md })
  | Some ref_md, None -> Some (Module { mname = name; mdiff = Removed ref_md })
  | Some reference, Some current ->
      module_declaration ~typing_env ~name ~reference ~current

and module_type_item ~typing_env ~name ~(reference : modtype_declaration option)
    ~(current : modtype_declaration option) =
  match (reference, current) with
  | None, None -> None
  | None, Some curr_mtd ->
      Some (Modtype { mtname = name; mtdiff = Added curr_mtd })
  | Some ref_mtd, None ->
      Some (Modtype { mtname = name; mtdiff = Removed ref_mtd })
  | Some ref_mtd, Some curr_mtd ->
      modtype_declaration ~typing_env ~name ~reference:ref_mtd ~current:curr_mtd

and module_declaration ~typing_env ~name ~reference ~current =
  module_type ~typing_env ~name ~ref_module_type:reference.md_type
    ~current_module_type:current.md_type ~reference_location:reference.md_loc

and modtype_declaration ~typing_env ~name ~reference ~current =
  match (reference.mtd_type, current.mtd_type) with
  | Some ref_sub, Some curr_sub ->
      module_type ~typing_env ~name ~ref_module_type:ref_sub
        ~current_module_type:curr_sub ~reference_location:reference.mtd_loc
  | Some _, None | None, Some _ ->
      Some (Modtype { mtname = name; mtdiff = Modified Unsupported })
  | None, None -> None

and module_type ~typing_env ~name ~ref_module_type ~current_module_type
    ~reference_location =
  match (ref_module_type, current_module_type) with
  | Mty_signature ref_submod, Mty_signature curr_submod ->
      signatures ~typing_env ~reference:ref_submod ~current:curr_submod
      |> Option.map (fun mdiff -> Module { mname = name; mdiff })
  | ref_modtype, curr_modtype ->
      module_type_fallback ~loc:reference_location ~typing_env ~name
        ~reference:ref_modtype ~current:curr_modtype

and signatures ~typing_env ~reference ~current =
  match items ~reference ~current with
  | [] -> (
      let coercion1 () =
        Includemod.signatures typing_env ~mark:Mark_both reference current
      in
      let coercion2 () =
        Includemod.signatures typing_env ~mark:Mark_both current reference
      in
      match (coercion1 (), coercion2 ()) with
      | Tcoerce_none, Tcoerce_none -> None
      | _, _ -> Some (Modified Unsupported)
      | exception Includemod.Error _ -> Some (Modified Unsupported))
  | item_changes -> Some (Modified (Supported item_changes))

let interface ~module_name ~reference ~current =
  let typing_env = Env.empty in
  let sig_out = signatures ~typing_env ~reference ~current in
  Option.map (fun mdiff -> { mname = module_name; mdiff }) sig_out

let library ~reference ~current =
  let mod_dec_of_sig sign =
    {
      md_type = Mty_signature sign;
      md_attributes = [];
      md_loc = Location.none;
      md_uid = Uid.internal_not_actually_unique;
    }
  in
  String_map.merge
    (fun module_name ref_sig_opt cur_sig_opt ->
      match (ref_sig_opt, cur_sig_opt) with
      | None, None -> None
      | Some ref_sig, None ->
          Some
            (Some
               {
                 mname = module_name;
                 mdiff =
                   Modified
                     (Supported
                        [
                          Module
                            {
                              mname = module_name;
                              mdiff = Removed (mod_dec_of_sig ref_sig);
                            };
                        ]);
               })
      | None, Some cur_sig ->
          Some
            (Some
               {
                 mname = module_name;
                 mdiff =
                   Modified
                     (Supported
                        [
                          Module
                            {
                              mname = module_name;
                              mdiff = Added (mod_dec_of_sig cur_sig);
                            };
                        ]);
               })
      | Some ref_sig, Some cur_sig -> (
          let module_diff =
            interface ~module_name ~reference:ref_sig ~current:cur_sig
          in
          match module_diff with None -> None | Some _ -> Some module_diff))
    reference current
