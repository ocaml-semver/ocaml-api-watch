open Types

let extract_non_alias_types signature =
  List.fold_left
    (fun acc item ->
      match item with
      | Sig_type (id, ({ type_manifest = None; _ } as type_decl), _, _) ->
          String_map.add (Ident.name id) (id, type_decl) acc
      | _ -> acc)
    String_map.empty signature

let set_type_equality ref_id ~ref_decl ~curr_decl =
  if List.length ref_decl.type_params = List.length curr_decl.type_params then
    let ref_path = Path.Pident ref_id in
    {
      curr_decl with
      type_manifest =
        Some
          (Btype.newgenty (Tconstr (ref_path, curr_decl.type_params, ref Mnil)));
    }
  else curr_decl

let set_type_equalities ~reference ~current =
  let ref_non_alias_types = extract_non_alias_types reference in
  List.map
    (fun item ->
      match item with
      | Sig_type
          ( curr_id,
            ({ type_manifest = None; _ } as curr_decl),
            rec_st,
            visibility ) -> (
          let name = Ident.name curr_id in
          match String_map.find_opt name ref_non_alias_types with
          | Some (ref_id, ref_decl) ->
              let new_decl = set_type_equality ref_id ~ref_decl ~curr_decl in
              Sig_type (curr_id, new_decl, rec_st, visibility)
          | None -> item)
      | _ -> item)
    current

let for_diff ~reference ~current =
  let modified_current = set_type_equalities ~reference ~current in
  let env = Env.empty in
  let env = Env.in_signature true env in
  let env = Env.add_signature reference env in
  Env.add_signature modified_current env

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
        pp_in_box "value" id (fun () ->
            Format.fprintf fmt "%a" Printtyp.(value_description id) vd);
        pp_rec s
    | Env_type (s, id, td) ->
        pp_in_box "type" id (fun () ->
            Format.fprintf fmt "%a" Printtyp.(type_declaration id) td);
        pp_rec s
    | Env_extension (s, id, ec) ->
        pp_in_box "extension" id (fun () ->
            Format.fprintf fmt "%a" Printtyp.(extension_constructor id) ec);
        pp_rec s
    | Env_module (s, id, mp, { md_type; _ }) ->
        pp_in_box "module" id (fun () ->
            Format.fprintf fmt "%s@;"
              (match mp with
              | Mp_present -> "Mp_present"
              | Mp_absent -> "Mp_absent");
            Format.fprintf fmt "%a" Printtyp.modtype md_type);
        pp_rec s
    | Env_modtype (s, id, mtyp) ->
        pp_in_box "module type" id (fun () ->
            Format.fprintf fmt "%a" (Printtyp.modtype_declaration id) mtyp);
        pp_rec s
    | Env_class (s, id, cd) ->
        pp_in_box "class" id (fun () ->
            Format.fprintf fmt "%a" (Printtyp.class_declaration id) cd);
        pp_rec s
    | Env_cltype (s, id, ctd) ->
        pp_in_box "class type" id (fun () ->
            Format.fprintf fmt "%a" (Printtyp.cltype_declaration id) ctd);
        pp_rec s
    | Env_open (s, path) ->
        Format.fprintf fmt "open %a@;" Printtyp.path path;
        pp_rec s
    | Env_functor_arg (s, id) ->
        Format.fprintf fmt "functor arg %s@;" (Ident.unique_toplevel_name id);
        pp_rec s
    | Env_constraints (s, td_map) ->
        Format.fprintf fmt "constraints@[<hv 2>@;";
        Path.Map.iter
          (fun path td ->
            Format.fprintf fmt "%a@[<hov 2>@;" Printtyp.path path;
            Printtyp.type_declaration (Path.head path) fmt td;
            Format.fprintf fmt "@]@;")
          td_map;
        Format.fprintf fmt "@]@;";
        pp_rec s
    | Env_copy_types s ->
        Format.fprintf fmt "copy_types@;";
        pp_rec s
    | Env_persistent (s, id) ->
        Format.fprintf fmt "persistent %s@;" (Ident.unique_toplevel_name id);
        pp_rec s
    | Env_value_unbound (s, name, vu_reason) ->
        Format.fprintf fmt "value unbound %s: %s@;" name
          (match vu_reason with
          | Val_unbound_instance_variable -> "instance variable"
          | Val_unbound_self -> "self"
          | Val_unbound_ancestor -> "ancestor"
          | Val_unbound_ghost_recursive _ -> "ghost recursive");
        pp_rec s
    | Env_module_unbound (s, name, Mod_unbound_illegal_recursion) ->
        Format.fprintf fmt "module unbound %s: illegal recursion@;" name;
        pp_rec s
  in
  pp_rec summary;
  Format.fprintf fmt "@]@;]@;"
