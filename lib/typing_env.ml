open Types

type t = Env.t
type subst_kind = Type | Module | Modtype [@@deriving ord]

module Subst_item_map = Map.Make (struct
  type t = subst_kind * string [@@deriving ord]
end)

let apply_subst subst signature =
  List.map
    (function
      | Sig_type (id, td, r, v) ->
          Sig_type (id, Subst.type_declaration subst td, r, v)
      | Sig_value (id, vd, v) ->
          Sig_value (id, Subst.value_description subst vd, v)
      | Sig_class (id, cd, rc, v) ->
          Sig_class (id, Subst.class_declaration subst cd, rc, v)
      | Sig_class_type (id, ct, rc, v) ->
          Sig_class_type (id, Subst.cltype_declaration subst ct, rc, v)
      | Sig_modtype (id, m, v) ->
          Sig_modtype (id, Subst.modtype_declaration Subst.Keep subst m, v)
      | Sig_module (id, mp, md, rc, v) ->
          Sig_module
            (id, mp, Subst.module_declaration Subst.Keep subst md, rc, v)
      | Sig_typext (id, ec, es, v) ->
          Sig_typext (id, Subst.extension_constructor subst ec, es, v))
    signature

(* Traverses the current signature and generates unique IDs for items
   that have conflicting IDs with items in the reference signature. It then
   replaces the old IDs with the new generated ones using substitutions.
*)
let _replace_matching_ids ~reference ~current =
  let ref_env = Env.add_signature reference Env.empty in
  let subst, modified_current =
    List.fold_right
      (fun item (subst, lst) ->
        match item with
        | Sig_type (id, td, r, v) as sig_typ_decl -> (
            match Env.find_type_index id ref_env with
            | Some _ ->
                let new_id = Ident.rename id in
                ( Subst.add_type id (Path.Pident new_id) subst,
                  Sig_type (new_id, td, r, v) :: lst )
            | None -> (subst, sig_typ_decl :: lst))
        | Sig_module (id, mp, md, r, v) as sig_mod_decl -> (
            match Env.find_module_index id ref_env with
            | Some _ ->
                let new_id = Ident.rename id in
                ( Subst.add_module id (Path.Pident new_id) subst,
                  Sig_module (new_id, mp, md, r, v) :: lst )
            | None -> (subst, sig_mod_decl :: lst))
        | Sig_modtype (id, mtd, v) -> (
            match Env.find_modtype_index id ref_env with
            | Some _ ->
                let new_id = Ident.rename id in
                ( Subst.add_modtype id (Mty_ident (Pident new_id)) subst,
                  Sig_modtype (new_id, mtd, v) :: lst )
            | None ->
                (* This is a special case for functor paramters.
                   When two functors have different parameters,
                   they might treated equally by Includemod.modtypes, thus
                   one of parameters' id has to be rewritten. For example:
                   module F (M : X) : A and module F (M : Y) : A
                   X and Y could have the same stamp, thus they would be
                   treated equally, so Y stamp has to be rewritten.
                   Note: This should be removed once we have fine-grained
                   diffing of functors *)
                let new_id = ref (Ident.rename id) in
                while Option.is_some (Env.find_modtype_index !new_id ref_env) do
                  new_id := Ident.rename id
                done;
                ( Subst.add_modtype id (Mty_ident (Pident !new_id)) subst,
                  Sig_modtype (!new_id, mtd, v) :: lst ))
        | Sig_value (id, vd, v) as sig_val -> (
            match Env.find_value_index id ref_env with
            | Some _ ->
                let new_id = Ident.rename id in
                (subst, Sig_value (new_id, vd, v) :: lst)
            | None -> (subst, sig_val :: lst))
        | Sig_class (id, cd, r, v) as sig_cls_decl -> (
            match Env.find_class_index id ref_env with
            | Some _ ->
                let new_id = Ident.rename id in
                (subst, Sig_class (new_id, cd, r, v) :: lst)
            | None -> (subst, sig_cls_decl :: lst))
        | Sig_class_type (id, ct, r, v) as sig_cltype -> (
            match Env.find_cltype_index id ref_env with
            | Some _ ->
                let new_id = Ident.rename id in
                (subst, Sig_class_type (new_id, ct, r, v) :: lst)
            | None -> (subst, sig_cltype :: lst))
        | _ -> (subst, item :: lst))
      current (Subst.identity, [])
  in
  apply_subst subst modified_current

let full_path id path =
  (*let rec aux path =
    match path with
    | Path.Pdot (p, s) -> (Path.Pdot (aux p, s))
    | Path.Pident i -> Path.Pdot (Path.Pident id, Ident.name i)
    | x -> x
  in
  let res = aux path in
  Path.print Format.std_formatter res;
  Format.force_newline ();
  res*)
  Path.Pdot (path, Ident.name id)

let rec _fully_qualifiy_names ~module_path ~subst signature =
  List.fold_right
    (fun item (subst, lst) ->
       match item with
       | Sig_type (id, td, r, v) ->
         (Subst.add_type id (Path.Pdot (module_path, (Ident.name id))) subst,
          (*let x = Subst.type_declaration subst td in
          Printtyp.type_declaration id Format.std_formatter x;*)
           (Sig_type (id, Subst.type_declaration subst td, r, v)) :: lst )
      | Sig_value (id, vd, v) ->
          (subst, Sig_value (id, Subst.value_description subst vd, v) :: lst)
       | Sig_module (id, mp, md, rc, v) -> (
           let md_type =
             match md.md_type with
             | Mty_signature sign ->
               (*let sign_ = (apply_subst subst sign) in*)
               let _, sig_ =
                 _fully_qualifiy_names
                   ~module_path:(full_path id module_path)
                     ~subst sign
               in
               Mty_signature sig_
             | x -> x
           in
           (subst,
            Sig_module (id, mp, { md with md_type = md_type }, rc, v) :: lst)
       )
       | _ -> (subst, lst))
    (List.rev signature) (subst, [])

let extract_subst_items signature =
  List.fold_left
    (fun acc item ->
      match item with
      | Sig_type (id, { type_manifest = None; _ }, _, _) ->
          Subst_item_map.add (Type, Ident.name id) id acc
      | Sig_module (id, _, _, _, _) ->
          Subst_item_map.add (Module, Ident.name id) id acc
      | Sig_modtype (id, _, _) ->
          Subst_item_map.add (Modtype, Ident.name id) id acc
      | _ -> acc)
    Subst_item_map.empty signature

let _pair_items ~reference ~current =
  let subst_items = extract_subst_items reference in
  List.fold_left
    (fun subst item ->
      match item with
      | Sig_type (id, { type_manifest = None; _ }, _, _) -> (
          match Subst_item_map.find_opt (Type, Ident.name id) subst_items with
          | None -> subst
          | Some ref_id -> Subst.add_type id (Path.Pident ref_id) subst)
      (*| Sig_module (id, _, _, _, _) -> (
          match Subst_item_map.find_opt (Module, Ident.name id) subst_items with
          | None -> subst
          | Some ref_id -> Subst.add_module id (Path.Pident ref_id) subst)*)
      | Sig_modtype (id, _, _) -> (
          match
            Subst_item_map.find_opt (Modtype, Ident.name id) subst_items
          with
          | None -> subst
          | Some ref_id ->
              Subst.add_modtype id (Mty_ident (Path.Pident ref_id)) subst)
      | _ -> subst)
    Subst.identity current

let initialized_env =
  Compmisc.init_path ();
  let env = Compmisc.initial_env () in
  fun () -> env

let id1 =
  let new_id = Ident.create_local "Library" in
  fun () -> new_id

let id2 =
  let new_id = Ident.create_local "Library" in
  fun () -> new_id

let _mod_dec_of_sig _name sign =
  let md =
    {
      md_type = Mty_signature sign;
      md_attributes = [];
      md_loc = Location.none;
      md_uid = Uid.internal_not_actually_unique;
    }
  in
  [ Types.Sig_module ((id2) (), Types.Mp_present, md, Types.Trec_not,
                      Types.Exported) ]

let _mod_dec_of_sig2 _name sign =
  let md =
    {
      md_type = Mty_signature sign;
      md_attributes = [];
      md_loc = Location.none;
      md_uid = Uid.internal_not_actually_unique;
    }
  in
  [ Types.Sig_module ((id1) (), Types.Mp_present, md, Types.Trec_not,
                      Types.Exported) ]

let _traverse_sig sig_ =
  List.fold_right
    (fun item lst ->
       match item with
       | Sig_module (id, mp, md, rc, v) -> (
           let md_type =
             match md.md_type with
             | Mty_signature sign ->
               (*let sign_ = (apply_subst subst sign) in*)
               let _, sig_ =
                 _fully_qualifiy_names
                   ~module_path:(Pident id)
                     ~subst:Subst.identity sign
               in
               Mty_signature sig_
             | x -> x
           in
           (
            Sig_module (id, mp, { md with md_type = md_type }, rc, v) :: lst)
       )
       | Sig_modtype _ as modtype -> modtype :: lst
       | x -> x :: lst) sig_ []

let for_diff ~module_name:_ ~reference ~current =
  let reference = _traverse_sig reference in
  (*let current = _replace_matching_ids ~reference ~current in*)
  let current = _traverse_sig current in
  let env =
    Env.add_signature ((*mod_dec_of_sig module_name*) reference)
      (Env.in_signature true (initialized_env ()))
  in
  let env = Env.add_signature ((*(mod_dec_of_sig2 module_name*) current) env in
  (*let env =
    match Env.open_signature Asttypes.Fresh
            (Pident(Ident.create_persistent "Library")) env
    with
  | Ok e -> e
  | Error `Not_found -> assert false
  | Error `Functor -> assert false*)
        (* a compilation unit cannot refer to a functor *)
  (*let subst = _pair_items ~reference ~current in
    let modified_current = apply_subst subst current in*)
  (reference, current, env)

let set_type_equalities ~reference ~current =
  let subst = _pair_items ~reference ~current in
  let _modified_current = apply_subst subst current in
  (reference, _modified_current)

let expand_tconstr ~typing_env ~path ~args =
  let type_decl =
    try Some (Env.find_type path typing_env) with Not_found -> None
  in
  match type_decl with
  | None -> None
  | Some td -> (
      match td.Types.type_manifest with
      | None -> None
      | Some type_expr ->
          Some (Ctype.apply typing_env td.Types.type_params type_expr args))

let fully_expand_tconstr =
  let cnt = ref 1 in
  fun ~typing_env ~path ~args ->
    let rec aux last path args =
    match expand_tconstr ~typing_env ~path ~args with
    | None -> last
    | Some expr -> (
        match Types.get_desc expr with
        | Tconstr (path, args, _) -> aux (Some expr) path args
        | _ -> Some expr)
  in
  let path =
    match path with
    | Path.Pdot (Pdot (Pident _, j), k) ->
      Path.Pdot (Pdot ((Pident ((
          if !cnt = 2 then id1 else
            (cnt := !cnt + 1; id2)) ())), j), k)
    | x -> x
  in
  (*Path.print Format.std_formatter path;*)
  let res = aux None path args in
  (*(match res with
  | None -> Format.print_string "None"
  | Some e -> Printtyp.type_expr Format.std_formatter e);*)
  res

let pp fmt t =
  let summary = Env.summary t in
  Format.fprintf fmt "@[<hv 2>[@;";
  let pp_in_box kind id f =
    Format.fprintf fmt "%s %s:@[<hov 2>@;" kind (Ident.unique_toplevel_name id);
    f ();
    Format.fprintf fmt "@]@;"
  in
  let rec pp_rec s =
    match (s : Env.summary) with
    | Env_empty -> ()
    | Env_value (s, id, vd) ->
        pp_rec s;
        pp_in_box "value" id (fun () ->
            Format.fprintf fmt "%a" Printtyp.(value_description id) vd)
    | Env_type (s, id, td) ->
        pp_rec s;
        pp_in_box "type" id (fun () ->
            Format.fprintf fmt "%a" Printtyp.(type_declaration id) td)
    | Env_extension (s, id, ec) ->
        pp_rec s;
        pp_in_box "extension" id (fun () ->
            Format.fprintf fmt "%a" Printtyp.(extension_constructor id) ec)
    | Env_module (s, id, mp, { md_type; _ }) ->
        pp_rec s;
        pp_in_box "module" id (fun () ->
            Format.fprintf fmt "%s@;"
              (match mp with
              | Mp_present -> "Mp_present"
              | Mp_absent -> "Mp_absent");
            (match md_type with
            | Mty_functor (Named (Some pid, _pmt), _fmt) ->
                Ident.print Format.std_formatter pid
            | _ -> ());
            Format.fprintf fmt "%a" Printtyp.modtype md_type)
    | Env_modtype (s, id, mtyp) ->
        pp_rec s;
        pp_in_box "module type" id (fun () ->
            Format.fprintf fmt "%a" (Printtyp.modtype_declaration id) mtyp)
    | Env_class (s, id, cd) ->
        pp_rec s;
        pp_in_box "class" id (fun () ->
            Format.fprintf fmt "%a" (Printtyp.class_declaration id) cd)
    | Env_cltype (s, id, ctd) ->
        pp_rec s;
        pp_in_box "class type" id (fun () ->
            Format.fprintf fmt "%a" (Printtyp.cltype_declaration id) ctd)
    | Env_open (s, path) ->
        pp_rec s;
        Format.fprintf fmt "open %a@;" Printtyp.path path
    | Env_functor_arg (s, id) ->
        pp_rec s;
        Format.fprintf fmt "functor arg %s@;" (Ident.unique_toplevel_name id)
    | Env_constraints (s, td_map) ->
        pp_rec s;
        Format.fprintf fmt "constraints@[<hv 2>@;";
        Path.Map.iter
          (fun path td ->
            Format.fprintf fmt "%a@[<hov 2>@;" Printtyp.path path;
            Printtyp.type_declaration (Path.head path) fmt td;
            Format.fprintf fmt "@]@;")
          td_map;
        Format.fprintf fmt "@]@;"
    | Env_copy_types s ->
        pp_rec s;
        Format.fprintf fmt "copy_types@;"
    | Env_persistent (s, id) ->
        pp_rec s;
        Format.fprintf fmt "persistent %s@;" (Ident.unique_toplevel_name id)
    | Env_value_unbound (s, name, vu_reason) ->
        pp_rec s;
        Format.fprintf fmt "value unbound %s: %s@;" name
          (match vu_reason with
          | Val_unbound_instance_variable -> "instance variable"
          | Val_unbound_self -> "self"
          | Val_unbound_ancestor -> "ancestor"
          | Val_unbound_ghost_recursive _ -> "ghost recursive")
    | Env_module_unbound (s, name, Mod_unbound_illegal_recursion) ->
        pp_rec s;
        Format.fprintf fmt "module unbound %s: illegal recursion@;" name
  in
  pp_rec summary;
  Format.fprintf fmt "@]@;]@;"
