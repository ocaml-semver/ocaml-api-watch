type type_expr =
  | Tuple of tuple
  | Arrow of arrow
  | Constr of constr
  | Atomic of Types.type_expr Stddiff.atomic_modification

and tuple = (Types.type_expr, type_expr) Stddiff.List.t

and arrow = {
  arg_label :
    ( arg_label option,
      (arg_label, arg_label_diff) Stddiff.Option.t )
    Stddiff.maybe_changed;
  arg_type : (Types.type_expr, type_expr) Stddiff.maybe_changed;
  return_type : (Types.type_expr, type_expr) Stddiff.maybe_changed;
}

and arg_label = Labelled_arg of string | Optional_arg of string

and arg_label_diff = {
  name : (string, string Stddiff.atomic_modification) Stddiff.maybe_changed;
  arg_optional : (bool, arg_optional) Stddiff.maybe_changed;
}

and arg_optional = Added_opt_arg | Removed_opt_arg

and constr = {
  path : (Path.t, Path.t Stddiff.atomic_modification) Stddiff.maybe_changed;
  args :
    ( Types.type_expr list,
      ( Types.type_expr list,
        (Types.type_expr, type_expr) Stddiff.List.t )
      Stddiff.entry )
    Stddiff.maybe_changed;
}

type type_modification = {
  type_kind : (Types.type_decl_kind, type_kind) Stddiff.maybe_changed;
  type_privacy : (Asttypes.private_flag, type_privacy) Stddiff.maybe_changed;
  type_manifest :
    ( Types.type_expr option,
      (Types.type_expr, type_expr) Stddiff.Option.t )
    Stddiff.maybe_changed;
  type_params :
    ( Types.type_expr list,
      ( Types.type_expr list,
        (Types.type_expr, type_expr) Stddiff.List.t )
      Stddiff.entry )
    Stddiff.maybe_changed;
}

and type_kind =
  | Record_tk of (Types.label_declaration, label) Stddiff.Map.t
  | Variant_tk of (Types.constructor_declaration, cstr_args) Stddiff.Map.t
  | Atomic_tk of Types.type_decl_kind Stddiff.atomic_modification

and label = {
  label_type : (Types.type_expr, type_expr) Stddiff.maybe_changed;
  label_mutable :
    (Asttypes.mutable_flag, field_mutability) Stddiff.maybe_changed;
}

and field_mutability = Added_m | Removed_m

and cstr_args =
  | Record_cstr of (Types.label_declaration, label) Stddiff.Map.t
  | Tuple_cstr of tuple
  | Atomic_cstr of Types.constructor_arguments Stddiff.atomic_modification

and type_privacy = Added_p | Removed_p

type type_ = {
  tname : string;
  tdiff : (Types.type_declaration, type_modification) Stddiff.entry;
}

type value = {
  vname : string;
  vdiff : (Types.value_description, type_expr) Stddiff.entry;
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

let extract_items items =
  List.fold_left
    (fun tbl item ->
      match (item : Types.signature_item) with
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
              (type_decl, id) tbl
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

let extract_lbls lbls =
  List.fold_left
    (fun map lbl -> String_map.add (Ident.name lbl.Types.ld_id) lbl map)
    String_map.empty lbls

let extract_cstrs cstrs =
  List.fold_left
    (fun map cstr -> String_map.add (Ident.name cstr.Types.cd_id) cstr map)
    String_map.empty cstrs

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

let expand_alias_types ~typing_env ~type_expr =
  Ctype.full_expand ~may_forget_scope:false typing_env type_expr

let rec type_expr ~typing_env ?(ref_params = []) ?(cur_params = []) reference
    current =
  let expanded_ref =
    Typing_env.fully_expand_type_expr ~typing_env ~type_expr:reference
  in
  let expanded_cur =
    Typing_env.fully_expand_type_expr ~typing_env ~type_expr:current
  in
  match (Types.get_desc expanded_ref, Types.get_desc expanded_cur) with
  | Ttuple ref_exps, Ttuple cur_exps -> (
      let type_exprs =
        type_exprs ~typing_env ~ref_params ~cur_params ~reference:ref_exps
          ~current:cur_exps
      in
      match type_exprs with
      | Stddiff.Same _ -> Stddiff.Same reference
      | Changed change -> Changed (Tuple change))
  | ( Tarrow (ref_arg_label, ref_arg_type, ref_return_type, _),
      Tarrow (cur_arg_label, cur_arg_type, cur_return_type, _) ) -> (
      let arrow =
        arrow ~typing_env ~ref_params ~cur_params
          ~reference:(ref_arg_label, ref_arg_type, ref_return_type)
          ~current:(cur_arg_label, cur_arg_type, cur_return_type)
      in
      match arrow with
      | Stddiff.Same _ -> Stddiff.Same reference
      | Changed change -> Changed (Arrow change))
  | Tconstr (ref_path, ref_args, _), Tconstr (cur_path, cur_args, _) -> (
      let constr =
        constr ~typing_env ~ref_params ~cur_params
          ~reference:(ref_path, ref_args) ~current:(cur_path, cur_args)
      in
      match constr with
      | Stddiff.Same _ -> Stddiff.Same current
      | Changed change -> Changed (Constr change))
  | _ ->
      let normed_ref, normed_cur =
        Normalize.type_params_arity ~reference:ref_params ~current:cur_params
      in
      if
        Ctype.is_equal typing_env true
          (normed_ref @ [ reference ])
          (normed_cur @ [ current ])
      then Same reference
      else
        Changed
          (Atomic
             {
               reference = expand_alias_types ~typing_env ~type_expr:reference;
               current = expand_alias_types ~typing_env ~type_expr:current;
             })

and constr ~typing_env ~ref_params ~cur_params ~reference ~current =
  let open Stddiff in
  let ref_path, ref_args = reference in
  let cur_path, cur_args = current in
  let path =
    if String.equal (Path.name ref_path) (Path.name cur_path) then Same ref_path
    else Changed { reference = ref_path; current = cur_path }
  in
  let args =
    match (ref_args, cur_args) with
    | [], _ :: _ -> Changed (Added cur_args)
    | _ :: _, [] -> Changed (Removed ref_args)
    | _ -> (
        let type_exprs =
          type_exprs ~typing_env ~ref_params ~cur_params ~reference:ref_args
            ~current:cur_args
        in
        match type_exprs with
        | Same same_params -> Same same_params
        | Changed change -> Changed (Modified change))
  in
  match (path, args) with
  | Same _, Same _ -> Same reference
  | _ -> Changed { path; args }

and type_exprs ~typing_env ~ref_params ~cur_params ~reference ~current =
  Stddiff.List.diff
    ~diff_one:(fun ref cur ->
      type_expr ~typing_env ~ref_params ~cur_params ref cur)
    ~reference ~current

and arrow ~typing_env ~ref_params ~cur_params ~reference ~current =
  let unwrap_optional_arg lbl typ =
    match lbl with
    | Asttypes.Nolabel | Labelled _ -> typ
    | Optional _ -> (
        match Types.get_desc typ with
        | Tconstr (_, [ te ], _) -> te
        | _ -> assert false)
  in
  let ref_arg_label, ref_arg_type, ref_return_type = reference in
  let cur_arg_label, cur_arg_type, cur_return_type = current in
  let arg_label = arg_label ~reference:ref_arg_label ~current:cur_arg_label in
  let arg_type =
    type_expr ~typing_env ~ref_params ~cur_params
      (unwrap_optional_arg ref_arg_label ref_arg_type)
      (unwrap_optional_arg cur_arg_label cur_arg_type)
  in
  let return_type =
    type_expr ~typing_env ~ref_params ~cur_params ref_return_type
      cur_return_type
  in
  match (arg_label, arg_type, return_type) with
  | Stddiff.Same _, Same _, Same _ -> Same reference
  | _ -> Changed { arg_label; arg_type; return_type }

and arg_label ~reference ~current =
  let open Stddiff in
  let convert = function
    | Asttypes.Nolabel -> None
    | Labelled name -> Some (Labelled_arg name)
    | Optional name -> Some (Optional_arg name)
  in
  Option.diff
    ~diff_one:(fun ref cur ->
      match (ref, cur) with
      | Labelled_arg ref_name, Labelled_arg cur_name ->
          if String.equal ref_name cur_name then Same (Labelled_arg ref_name)
          else
            Changed
              {
                name = Changed { reference = ref_name; current = cur_name };
                arg_optional = Same false;
              }
      | Labelled_arg ref_name, Optional_arg cur_name ->
          let name =
            if String.equal ref_name cur_name then Same ref_name
            else Changed { reference = ref_name; current = cur_name }
          in
          let arg_optional = Changed Added_opt_arg in
          Changed { name; arg_optional }
      | Optional_arg ref_name, Labelled_arg cur_name ->
          let name =
            if String.equal ref_name cur_name then Same ref_name
            else Changed { reference = ref_name; current = cur_name }
          in
          let arg_optional = Changed Removed_opt_arg in
          Changed { name; arg_optional }
      | Optional_arg ref_name, Optional_arg cur_name ->
          if String.equal ref_name cur_name then Same (Optional_arg ref_name)
          else
            Changed
              {
                name = Changed { reference = ref_name; current = cur_name };
                arg_optional = Same true;
              })
    ~reference:(convert reference) ~current:(convert current)

let rec type_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | Some (reference, _), None ->
      Some (Type { tname = name; tdiff = Removed reference })
  | None, Some (current, _) ->
      Some (Type { tname = name; tdiff = Added current })
  | Some (reference, _), Some (current, _) ->
      type_declarations ~typing_env ~name ~reference ~current

and type_declarations ~typing_env ~name ~reference ~current =
  if
    Normalize.is_type_params ~reference:reference.Types.type_params
      ~current:current.Types.type_params
  then ()
  else Normalize.type_declarations ~reference ~current;
  let ref_params = reference.type_params in
  let cur_params = current.type_params in
  let type_kind =
    type_kind ~typing_env ~ref_params ~cur_params ~reference:reference.type_kind
      ~current:current.type_kind
  in
  let type_privacy =
    type_privacy ~reference:reference.type_private ~current:current.type_private
  in
  let type_manifest =
    type_manifest ~typing_env ~ref_params ~cur_params
      ~reference:reference.type_manifest ~current:current.type_manifest
  in
  let type_params = type_params ~reference:ref_params ~current:cur_params in
  match { type_kind; type_privacy; type_manifest; type_params } with
  | {
   type_kind = Same _;
   type_privacy = Same _;
   type_manifest = Same _;
   type_params = Same _;
  } ->
      None
  | diff -> Some (Type { tname = name; tdiff = Modified diff })

and type_kind ~typing_env ~ref_params ~cur_params ~reference ~current =
  let open Stddiff.Map in
  match (reference, current) with
  | Type_record (ref_label_lst, _), Type_record (cur_label_lst, _) ->
      let label_map =
        record_type ~typing_env ~ref_params ~cur_params ~ref_label_lst
          ~cur_label_lst
      in
      if String_map.is_empty label_map.changed_map then Same reference
      else Changed (Record_tk label_map)
  | Type_variant (ref_constructor_lst, _), Type_variant (cur_constructor_lst, _)
    ->
      let cstr_map =
        variant_type ~typing_env ~ref_params ~cur_params ~ref_constructor_lst
          ~cur_constructor_lst
      in
      if String_map.is_empty cstr_map.changed_map then Same reference
      else Changed (Variant_tk cstr_map)
  | Type_abstract _, Type_abstract _ -> Same reference
  | Type_open, Type_open -> Same reference
  | ref_type_kind, cur_type_kind ->
      Changed (Atomic_tk { reference = ref_type_kind; current = cur_type_kind })

and record_type ~typing_env ~ref_params ~cur_params ~ref_label_lst
    ~cur_label_lst =
  let open Stddiff in
  let ref_lbls = extract_lbls ref_label_lst in
  let cur_lbls = extract_lbls cur_label_lst in
  Map.diff
    ~diff_one:(label ~typing_env ~ref_params ~cur_params)
    ~reference:ref_lbls ~current:cur_lbls

and label ~typing_env ~ref_params ~cur_params reference current =
  let open Stddiff in
  let label_type =
    type_expr ~typing_env ~ref_params ~cur_params reference.ld_type
      current.ld_type
  in
  let label_mutable =
    label_mutable ~reference:reference.ld_mutable ~current:current.ld_mutable
  in
  match (label_type, label_mutable) with
  | Same _, Same _ -> Same reference
  | Same _, label_mutable ->
      Changed { label_type = Same reference.ld_type; label_mutable }
  | Changed type_diff, label_mutable ->
      Changed { label_type = Changed type_diff; label_mutable }

and label_mutable ~reference ~current =
  match (reference, current) with
  | Asttypes.Mutable, Asttypes.Mutable | Asttypes.Immutable, Asttypes.Immutable
    ->
      Same reference
  | Asttypes.Mutable, Asttypes.Immutable -> Changed Removed_m
  | Asttypes.Immutable, Asttypes.Mutable -> Changed Added_m

and variant_type ~typing_env ~ref_params ~cur_params ~ref_constructor_lst
    ~cur_constructor_lst =
  let open Stddiff in
  let ref_cstrs = extract_cstrs ref_constructor_lst in
  let cur_cstrs = extract_cstrs cur_constructor_lst in
  Map.diff
    ~diff_one:(cstr ~typing_env ~ref_params ~cur_params)
    ~reference:ref_cstrs ~current:cur_cstrs

and cstr ~typing_env ~ref_params ~cur_params reference current =
  match (reference.cd_args, current.cd_args) with
  | Cstr_tuple ref_type_exprs, Cstr_tuple cur_type_exprs -> (
      let type_exprs =
        type_exprs ~typing_env ~ref_params ~cur_params ~reference:ref_type_exprs
          ~current:cur_type_exprs
      in
      match type_exprs with
      | Same _ -> Same reference
      | Changed change -> Changed (Tuple_cstr change))
  | Cstr_record ref_record, Cstr_record cur_record ->
      let label_map =
        record_type ~typing_env ~ref_params ~cur_params
          ~ref_label_lst:ref_record ~cur_label_lst:cur_record
      in
      if String_map.is_empty label_map.changed_map then Same reference
      else Changed (Record_cstr label_map)
  | _ ->
      Changed
        (Atomic_cstr
           { reference = reference.cd_args; current = current.cd_args })

and type_params ~reference ~current =
  let open Stddiff in
  match (reference, current) with
  | [], _ :: _ -> Changed (Added current)
  | _ :: _, [] -> Changed (Removed reference)
  | _ -> (
      let params_diff =
        List.diff ~diff_one:(fun t1 _ -> Same t1) ~reference ~current
      in
      match params_diff with
      | Same same_params -> Same same_params
      | Changed change -> Changed (Modified change))

and type_privacy ~reference ~current =
  match (reference, current) with
  | Asttypes.Public, Asttypes.Public -> Same Asttypes.Public
  | Asttypes.Public, Asttypes.Private -> Changed Added_p
  | Asttypes.Private, Asttypes.Public -> Changed Removed_p
  | Asttypes.Private, Asttypes.Private -> Same Asttypes.Private

and type_manifest ~typing_env ~ref_params ~cur_params ~reference ~current =
  let open Stddiff in
  Option.diff
    ~diff_one:(type_expr ~typing_env ~ref_params ~cur_params)
    ~reference ~current

let value_descripiton ~typing_env reference current =
  let open Types in
  type_expr ~typing_env reference.val_type current.val_type

let value_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | Some reference, None ->
      Some (Value { vname = name; vdiff = Removed reference })
  | None, Some current -> Some (Value { vname = name; vdiff = Added current })
  | Some reference, Some current -> (
      let val_type_diff = value_descripiton ~typing_env reference current in
      match val_type_diff with
      | Same _ -> None
      | Changed type_expr_diff ->
          Some (Value { vname = name; vdiff = Modified type_expr_diff }))

let class_item ~typing_env ~name ~(reference : Types.class_declaration option)
    ~(current : Types.class_declaration option) =
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
    ~(reference : Types.class_type_declaration option)
    ~(current : Types.class_type_declaration option) =
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

let rec items ~reference ~current ~typing_env =
  let ref_items = extract_items reference in
  let curr_items = extract_items current in
  let diff_item : type a. (a, 'diff) Sig_item_map.diff_item =
   fun item_type name reference current ->
    match item_type with
    | Value -> value_item ~typing_env ~name ~reference ~current
    | Module -> module_item ~typing_env ~name ~reference ~current
    | Modtype -> module_type_item ~typing_env ~name ~reference ~current
    | Type -> type_item ~typing_env ~name ~reference ~current
    | Class -> class_item ~typing_env ~name ~reference ~current
    | Classtype -> class_type_item ~typing_env ~name ~reference ~current
  in
  Sig_item_map.diff ~diff_item:{ diff_item } ref_items curr_items

and module_item ~typing_env ~name ~(reference : Types.module_declaration option)
    ~(current : Types.module_declaration option) =
  match (reference, current) with
  | None, None -> None
  | None, Some curr_md -> Some (Module { mname = name; mdiff = Added curr_md })
  | Some ref_md, None -> Some (Module { mname = name; mdiff = Removed ref_md })
  | Some reference, Some current ->
      module_declaration ~typing_env ~name ~reference ~current

and module_type_item ~typing_env ~name
    ~(reference : Types.modtype_declaration option)
    ~(current : Types.modtype_declaration option) =
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
      signatures ~reference:ref_submod ~current:curr_submod
      |> Option.map (fun mdiff -> Module { mname = name; mdiff })
  | ref_modtype, curr_modtype ->
      module_type_fallback ~loc:reference_location ~typing_env ~name
        ~reference:ref_modtype ~current:curr_modtype

and signatures ~reference ~current =
  let initialized_env = Typing_env.initialized_env () in
  let modified_reference, modified_current, typing_env =
    Typing_env.for_diff ~reference ~current
  in
  match
    items ~reference:modified_reference ~current:modified_current ~typing_env
  with
  | [] -> (
      let coercion1 () =
        Includemod.signatures initialized_env ~mark:Mark_both reference current
      in
      let coercion2 () =
        Includemod.signatures initialized_env ~mark:Mark_both current reference
      in
      match (coercion1 (), coercion2 ()) with
      | Tcoerce_none, Tcoerce_none -> None
      | _, _ -> Some (Modified Unsupported)
      | exception Includemod.Error _ -> Some (Modified Unsupported))
  | item_changes -> Some (Modified (Supported item_changes))

let interface ~module_name ~reference ~current =
  let sig_out = signatures ~reference ~current in
  Option.map (fun mdiff -> { mname = module_name; mdiff }) sig_out

let library ~reference ~current =
  let open Types in
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
