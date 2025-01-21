open Types

type ('item, 'diff) t = Added of 'item | Removed of 'item | Modified of 'diff

type 'a atomic_modification = { reference : 'a; current : 'a }
(** The simplest diff representation for the modification of a value of type 'a.
    [reference] is the value before and [current] is the value after the change
    occured. Use this type when there is no better representation available. *)

type value = {
  vname : string;
  vdiff : (value_description, value_description atomic_modification) t;
}

type type_modification =
  | Compound of record_field list
  | Atomic of type_declaration atomic_modification

and record_field = {
  lname : string;
  ldiff : (label_declaration, label_declaration atomic_modification) t;
}

type type_ = { tname : string; tdiff : (type_declaration, type_modification) t }

type class_ = {
  cname : string;
  cdiff : (class_declaration, class_declaration atomic_modification) t;
}

type cltype = {
  ctname : string;
  ctdiff :
    (class_type_declaration, class_type_declaration atomic_modification) t;
}

type module_ = {
  mname : string;
  mdiff : (module_declaration, signature_modification) t;
}

and modtype = {
  mtname : string;
  mtdiff : (modtype_declaration, signature_modification) t;
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

let extract_lbls lbls =
  List.fold_left
    (fun map lbl -> String_map.add (Ident.name lbl.ld_id) lbl map)
    String_map.empty lbls

let rec type_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | Some (reference, _), None ->
      Some (Type { tname = name; tdiff = Removed reference })
  | None, Some (current, _) ->
      Some (Type { tname = name; tdiff = Added current })
  | Some (reference, refId), Some (current, curId) -> (
      let type_coercion1 () =
        Includecore.type_declarations ~loc:current.type_loc typing_env
          ~mark:false name current (Pident curId) reference
      in
      let type_coercion2 () =
        Includecore.type_declarations ~loc:reference.type_loc typing_env
          ~mark:false name reference (Pident refId) current
      in
      match (type_coercion1 (), type_coercion2 ()) with
      | None, None -> None
      | _, _ -> (
          match (reference.type_kind, current.type_kind) with
          | Type_record (ref_label_lst, _), Type_record (cur_label_lst, _) ->
              let changed_lbls =
                modified_record_type ~typing_env ~ref_label_lst ~cur_label_lst
              in
              Some
                (Type { tname = name; tdiff = Modified (Compound changed_lbls) })
          | _, _ ->
              Some
                (Type
                   {
                     tname = name;
                     tdiff = Modified (Atomic { reference; current });
                   })))

and modified_record_type ~typing_env ~ref_label_lst ~cur_label_lst =
  let ref_lbls = extract_lbls ref_label_lst in
  let curr_lbls = extract_lbls cur_label_lst in
  let changed_lbls =
    String_map.merge
      (fun name ref cur ->
        match (ref, cur) with
        | None, None -> None
        | Some ref, None -> Some { lname = name; ldiff = Removed ref }
        | None, Some cur -> Some { lname = name; ldiff = Added cur }
        | Some ref, Some cur ->
            if Ctype.does_match typing_env ref.ld_type cur.ld_type then None
            else
              Some
                {
                  lname = name;
                  ldiff = Modified { reference = ref; current = cur };
                })
      ref_lbls curr_lbls
    |> String_map.bindings |> List.map snd
  in
  changed_lbls

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
