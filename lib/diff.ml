open Types
open Stddiff

type type_modification = {
  type_kind : (type_decl_kind, type_kind) maybe_changed;
  type_privacy : (Asttypes.private_flag, type_privacy) maybe_changed;
  type_manifest : type_expr Stddiff.atomic_option;
  type_params : (type_expr, type_param) Stddiff.list_;
}

and type_kind =
  | Record_tk of (label_declaration, label) map
  | Variant_tk of (Types.constructor_declaration, cstr_args) map
  | Atomic_tk of type_decl_kind atomic_modification

and label = {
  label_type : type_expr Stddiff.maybe_changed_atomic;
  label_mutable : (Asttypes.mutable_flag, field_mutability) maybe_changed;
}

and field_mutability = Added_m | Removed_m

and cstr_args =
  | Record_cstr of (label_declaration, label) map
  | Tuple_cstr of type_expr maybe_changed_atomic_entry list
  | Atomic_cstr of Types.constructor_arguments Stddiff.atomic_modification

and type_privacy = Added_p | Removed_p
and type_param = (type_expr, type_param_diff) maybe_changed
and type_param_diff = Added_tp of type_expr | Removed_tp of type_expr

type type_ = {
  tname : string;
  tdiff : (type_declaration, type_modification) entry;
}

type value = {
  vname : string;
  vdiff : (value_description, type_expr atomic_modification) entry;
}

type class_ = { cname : string; cdiff : class_declaration Stddiff.atomic_entry }

type cltype = {
  ctname : string;
  ctdiff : class_type_declaration Stddiff.atomic_entry;
}

type module_ = {
  mname : string;
  mdiff : (module_declaration, signature_modification) entry;
}

and modtype = {
  mtname : string;
  mtdiff : (modtype_declaration, signature_modification) entry;
}

and signature_modification = Unsupported | Supported of indexed_sig_item list

and sig_item =
  | Value of value
  | Module of module_
  | Type of type_
  | Modtype of modtype
  | Class of class_
  | Classtype of cltype

and indexed_sig_item = {
  ref_index : int option;
  cur_index : int option;
  sig_item : sig_item;
}

let extract_items items =
  let index = ref 0 in
  List.fold_left
    (fun tbl item ->
      index := !index + 1;
      match item with
      | Sig_module (id, _, mod_decl, _, Exported) ->
          Sig_item_map.add ~name:(Ident.name id) Sig_item_map.Module
            (!index, mod_decl) tbl
      | Sig_modtype (id, mtd_decl, Exported) ->
          Sig_item_map.add ~name:(Ident.name id) Sig_item_map.Modtype
            (!index, mtd_decl) tbl
      | Sig_value (id, val_des, Exported) ->
          Sig_item_map.add ~name:(Ident.name id) Sig_item_map.Value
            (!index, val_des) tbl
      | Sig_type (id, type_decl, _, Exported) ->
          if
            Sig_item_map.has ~name:(Ident.name id) Sig_item_map.Class tbl
            || Sig_item_map.has ~name:(Ident.name id) Sig_item_map.Classtype tbl
          then tbl
          else
            Sig_item_map.add ~name:(Ident.name id) Sig_item_map.Type
              (!index, type_decl, id) tbl
      | Sig_class (id, cls_decl, _, Exported) ->
          Sig_item_map.add ~name:(Ident.name id) Sig_item_map.Class
            (!index, cls_decl) tbl
      | Sig_class_type (id, class_type_decl, _, Exported) ->
          if Sig_item_map.has ~name:(Ident.name id) Sig_item_map.Class tbl then
            tbl
          else
            Sig_item_map.add ~name:(Ident.name id) Sig_item_map.Classtype
              (!index, class_type_decl) tbl
      | _ -> tbl)
    Sig_item_map.empty items

let extract_lbls lbls =
  List.fold_left
    (fun map lbl -> String_map.add (Ident.name lbl.ld_id) lbl map)
    String_map.empty lbls

let extract_cstrs cstrs =
  List.fold_left
    (fun map cstr -> String_map.add (Ident.name cstr.cd_id) cstr map)
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
  | _, _ -> Some ({ ref_index = None; cur_index = None; sig_item = Module { mname = name; mdiff = Modified Unsupported }})
  | exception Includemod.Error _ ->
    Some ({ ref_index = None; cur_index = None; sig_item = Module { mname = name; mdiff = Modified Unsupported }})

let expand_alias_types ~typing_env ~type_expr =
  Ctype.full_expand ~may_forget_scope:false typing_env type_expr

let type_expr ~typing_env ?(ref_params = []) ?(cur_params = []) reference
    current =
  let normed_ref, normed_cur =
    Normalize.type_params_arity ~reference:ref_params ~current:cur_params
  in
  if
    Ctype.is_equal typing_env true
      (normed_ref @ [ reference ])
      (normed_cur @ [ current ])
  then None
  else
    Some
      {
        reference = expand_alias_types ~typing_env ~type_expr:reference;
        current = expand_alias_types ~typing_env ~type_expr:current;
      }

let rec type_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | Some (ref_index, reference, _), None ->
      Some
        {
          ref_index = Some ref_index;
          cur_index = None;
          sig_item = Type { tname = name; tdiff = Removed reference };
        }
  | None, Some (cur_index, current, _) ->
      Some
        {
          ref_index = None;
          cur_index = Some cur_index;
          sig_item = Type { tname = name; tdiff = Added current };
        }
  | Some (ref_index, reference, _), Some (cur_index, current, _) ->
      let sig_item = type_declarations ~typing_env ~name ~reference ~current in
      Option.map
        (fun sig_item ->
          { ref_index = Some ref_index; cur_index = Some cur_index; sig_item })
        sig_item

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
  let ref_lbls = extract_lbls ref_label_lst in
  let cur_lbls = extract_lbls cur_label_lst in
  diff_map
    ~diff_one:(label ~typing_env ~ref_params ~cur_params)
    ~ref_map:ref_lbls ~cur_map:cur_lbls

and label ~typing_env ~ref_params ~cur_params reference current =
  let label_type =
    type_expr ~typing_env ~ref_params ~cur_params reference.ld_type
      current.ld_type
  in
  let label_mutable =
    label_mutable ~reference:reference.ld_mutable ~current:current.ld_mutable
  in
  match (label_type, label_mutable) with
  | None, Same _ -> None
  | None, label_mutable ->
      Some { label_type = Same reference.ld_type; label_mutable }
  | Some type_diff, label_mutable ->
      Some { label_type = Changed type_diff; label_mutable }

and label_mutable ~reference ~current =
  match (reference, current) with
  | Asttypes.Mutable, Asttypes.Mutable | Asttypes.Immutable, Asttypes.Immutable
    ->
      Same reference
  | Asttypes.Mutable, Asttypes.Immutable -> Changed Removed_m
  | Asttypes.Immutable, Asttypes.Mutable -> Changed Added_m

and variant_type ~typing_env ~ref_params ~cur_params ~ref_constructor_lst
    ~cur_constructor_lst =
  let ref_cstrs = extract_cstrs ref_constructor_lst in
  let cur_cstrs = extract_cstrs cur_constructor_lst in
  diff_map
    ~diff_one:(cstr ~typing_env ~ref_params ~cur_params)
    ~ref_map:ref_cstrs ~cur_map:cur_cstrs

and cstr ~typing_env ~ref_params ~cur_params reference current =
  match (reference.cd_args, current.cd_args) with
  | Cstr_tuple ref_tuple, Cstr_tuple cur_tuple -> (
      let tuple =
        tuple_type ~typing_env ~ref_params ~cur_params ~reference:ref_tuple
          ~current:cur_tuple
      in
      match tuple with Same _ -> None | Changed diff -> Some (Tuple_cstr diff))
  | Cstr_record ref_record, Cstr_record cur_record ->
      let label_map =
        record_type ~typing_env ~ref_params ~cur_params
          ~ref_label_lst:ref_record ~cur_label_lst:cur_record
      in
      if String_map.is_empty label_map.changed_map then None
      else Some (Record_cstr label_map)
  | _ ->
      Some
        (Atomic_cstr
           { reference = reference.cd_args; current = current.cd_args })

and tuple_type ~typing_env ~ref_params ~cur_params ~reference ~current =
  diff_list
    ~diff_one:(fun t1 t2 ->
      match (t1, t2) with
      | None, None -> assert false
      | Some t1, None -> Changed (Removed t1)
      | None, Some t2 -> Changed (Added t2)
      | Some t1, Some t2 -> (
          match type_expr ~typing_env ~ref_params ~cur_params t1 t2 with
          | None -> Same t1
          | Some diff -> Changed (Modified diff)))
    ~ref_list:reference ~cur_list:current

and type_params ~reference ~current =
  diff_list
    ~diff_one:(fun t1 t2 ->
      match (t1, t2) with
      | None, None -> assert false
      | Some t1, None -> Changed (Removed_tp t1)
      | None, Some t2 -> Changed (Added_tp t2)
      | Some t1, Some _ -> Same t1)
    ~ref_list:reference ~cur_list:current

and type_privacy ~reference ~current =
  match (reference, current) with
  | Asttypes.Public, Asttypes.Public -> Same Asttypes.Public
  | Asttypes.Public, Asttypes.Private -> Changed Added_p
  | Asttypes.Private, Asttypes.Public -> Changed Removed_p
  | Asttypes.Private, Asttypes.Private -> Same Asttypes.Private

and type_manifest ~typing_env ~ref_params ~cur_params ~reference ~current =
  match (reference, current) with
  | None, None -> Same None
  | Some t1, None -> Changed (Removed t1)
  | None, Some t2 -> Changed (Added t2)
  | Some t1, Some t2 -> (
      match type_expr ~typing_env ~ref_params ~cur_params t1 t2 with
      | None -> Same (Some t1)
      | Some diff -> Changed (Modified diff))

let value_descripiton ~typing_env reference current =
  type_expr ~typing_env reference.val_type current.val_type

let value_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | Some (ref_index, reference), None ->
      Some
        {
          ref_index = Some ref_index;
          cur_index = None;
          sig_item = Value { vname = name; vdiff = Removed reference };
        }
  | None, Some (cur_index, current) ->
      Some
        {
          ref_index = None;
          cur_index = Some cur_index;
          sig_item = Value { vname = name; vdiff = Added current };
        }
  | Some (ref_index, reference), Some (cur_index, current) -> (
      let val_type_diff = value_descripiton ~typing_env reference current in
      match val_type_diff with
      | None -> None
      | Some type_expr_diff ->
          Some
            {
              ref_index = Some ref_index;
              cur_index = Some cur_index;
              sig_item = Value { vname = name; vdiff = Modified type_expr_diff };
            })

let class_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | None, Some (cur_index, curr_cls) ->
      Some
        {
          ref_index = None;
          cur_index = Some cur_index;
          sig_item = Class { cname = name; cdiff = Added curr_cls };
        }
  | Some (ref_index, ref_cls), None ->
      Some
        {
          ref_index = Some ref_index;
          cur_index = None;
          sig_item = Class { cname = name; cdiff = Removed ref_cls };
        }
  | Some (ref_index, ref_cls), Some (cur_index, curr_cls) -> (
      let cls_mismatch_lst =
        Includeclass.class_declarations typing_env ref_cls curr_cls
      in
      match cls_mismatch_lst with
      | [] -> None
      | _ ->
          Some
            {
              ref_index = Some ref_index;
              cur_index = Some cur_index;
              sig_item =
                Class
                  {
                    cname = name;
                    cdiff = Modified { reference = ref_cls; current = curr_cls };
                  };
            })

let class_type_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | None, Some (cur_index, curr_class_type) ->
    Some
      {
        ref_index = None;
        cur_index = Some cur_index;
        sig_item = Classtype { ctname = name; ctdiff = Added curr_class_type }
      }
  | Some (ref_index, ref_class_type), None ->
    Some
      {
        ref_index = Some ref_index;
        cur_index = None;
        sig_item = Classtype { ctname = name; ctdiff = Removed ref_class_type }
      }
  | Some (ref_index, ref_class_type), Some (cur_index, curr_class_type) -> (
      let cls_type_mismatch_lst =
        Includeclass.class_type_declarations ~loc:ref_class_type.clty_loc
          typing_env ref_class_type curr_class_type
      in
      match cls_type_mismatch_lst with
      | [] -> None
      | _ ->
          Some
            {
              ref_index = Some ref_index;
              cur_index = Some cur_index;
              sig_item = Classtype
               {
                 ctname = name;
                 ctdiff =
                   Modified
                     { reference = ref_class_type; current = curr_class_type };
               }
            })

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

and module_item ~typing_env ~name ~(reference : (int * module_declaration) option)
    ~(current : (int * module_declaration) option) =
  match (reference, current) with
  | None, None -> None
  | None, Some (cur_index, curr_md) ->
    Some
      {
        ref_index = None;
        cur_index = Some cur_index;
        sig_item = Module { mname = name; mdiff = Added curr_md }
      }
  | Some (ref_index, ref_md), None ->
    Some
      {
        ref_index = Some ref_index;
        cur_index = None;
        sig_item = Module { mname = name; mdiff = Removed ref_md }
      }
  | Some reference, Some current ->
      module_declaration ~typing_env ~name ~reference ~current

and module_type_item ~typing_env ~name ~(reference : (int * modtype_declaration) option)
    ~(current : (int * modtype_declaration) option) =
  match (reference, current) with
  | None, None -> None
  | None, Some (cur_index, curr_mtd) ->
    Some
      {
        ref_index = None;
        cur_index = Some cur_index;
        sig_item = Modtype { mtname = name; mtdiff = Added curr_mtd }
      }
  | Some (ref_index, ref_mtd), None ->
    Some
      {
        ref_index = Some ref_index;
        cur_index = None;
        sig_item = Modtype { mtname = name; mtdiff = Removed ref_mtd }
      }
  | Some ref_mtd, Some curr_mtd ->
      modtype_declaration ~typing_env ~name ~reference:ref_mtd ~current:curr_mtd

and module_declaration ~typing_env ~name ~reference ~current =
  let _ref_index, reference = reference in
  let _cur_index, current = current in
  module_type ~typing_env ~name ~ref_module_type:reference.md_type
    ~current_module_type:current.md_type ~reference_location:reference.md_loc

and modtype_declaration ~typing_env ~name ~reference ~current =
  let ref_index, refer = reference in
  let cur_index, cur = current in
  match (refer.mtd_type, cur.mtd_type) with
  | Some ref_sub, Some curr_sub ->
      module_type ~typing_env ~name ~ref_module_type:ref_sub
        ~current_module_type:curr_sub ~reference_location:refer.mtd_loc
  | Some _, None | None, Some _ ->
    Some 
      { 
        ref_index = Some ref_index;
        cur_index = Some cur_index;
        sig_item = Modtype { mtname = name; mtdiff = Modified Unsupported }
      }
  | None, None -> None

and module_type ~typing_env ~name ~ref_module_type ~current_module_type
    ~reference_location =
  match (ref_module_type, current_module_type) with
  | Mty_signature ref_submod, Mty_signature curr_submod ->
      signatures ~reference:ref_submod ~current:curr_submod
      |> Option.map (fun mdiff -> { ref_index = None; cur_index = None;
                                    sig_item = Module { mname = name; mdiff }})
  | ref_modtype, curr_modtype ->
      module_type_fallback ~loc:reference_location ~typing_env ~name
        ~reference:ref_modtype ~current:curr_modtype

and signatures ~reference ~current =
  let modified_reference, modified_current, typing_env =
    Typing_env.for_diff ~reference ~current
  in
  match
    items ~reference:modified_reference ~current:modified_current ~typing_env
  with
  | [] -> (
      let coercion1 () =
        Includemod.signatures Env.empty ~mark:Mark_both reference current
      in
      let coercion2 () =
        Includemod.signatures Env.empty ~mark:Mark_both current reference
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
      | Some (ref_index, ref_sig), None ->
          Some
            (Some
               {
                 mname = module_name;
                 mdiff =
                   Modified
                     (Supported
                        [
                          { ref_index = Some ref_index;
                            cur_index = None;
                            sig_item = Module
                            {
                              mname = module_name;
                              mdiff = Removed (mod_dec_of_sig ref_sig);
                            };}
                        ]);
               })
      | None, Some (cur_index, cur_sig) ->
          Some
            (Some
               {
                 mname = module_name;
                 mdiff =
                   Modified
                     (Supported
                        [
                          { ref_index = None;
                            cur_index = Some cur_index;
                            sig_item = Module
                            {
                              mname = module_name;
                              mdiff = Added (mod_dec_of_sig cur_sig);
                            };}
                        ]);
               })
      | Some (_ref_index, ref_sig), Some (_cur_index, cur_sig) -> (
          let module_diff =
            interface ~module_name ~reference:ref_sig ~current:cur_sig
          in
          match module_diff with None -> None | Some _ -> Some module_diff))
    reference current
