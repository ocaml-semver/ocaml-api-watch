open Types

let qualified_name ~module_path id =
  let name = Ident.name id in
  match module_path with
  | [] -> name
  | l -> String.concat "." (List.rev (name :: l))

let enter_module ~module_path id =
  let name = Ident.name id in
  name :: module_path

let qualified_path ~module_path id =
  List.fold_left
    (fun acc mod_name -> Path.Pdot (acc, mod_name))
    (Path.Pident id) module_path

let rec rec_extract_non_alias_types acc ~module_path signature =
  List.fold_left
    (fun acc item ->
      match item with
      | Sig_type (id, ({ type_manifest = None; _ } as type_decl), _, _) ->
          let name = qualified_name ~module_path id in
          String_map.add name (id, type_decl) acc
      | Sig_module (id, _, { md_type = Mty_signature s; _ }, _, _) ->
          let module_path = enter_module ~module_path id in
          rec_extract_non_alias_types acc ~module_path s
      | _ -> acc)
    acc signature

let extract_non_alias_types signature =
  rec_extract_non_alias_types String_map.empty ~module_path:[] signature

let set_type_equality ~module_path ~ref_decl ~curr_decl ref_id =
  if List.length ref_decl.type_params = List.length curr_decl.type_params then
    let ref_path = qualified_path ~module_path ref_id in
    {
      curr_decl with
      type_manifest =
        Some
          (Btype.newgenty (Tconstr (ref_path, curr_decl.type_params, ref Mnil)));
    }
  else curr_decl

let rec rec_set_type_equalities ~ref_non_alias_types ~module_path sig_ =
  List.map
    (fun item ->
      match item with
      | Sig_type
          ( curr_id,
            ({ type_manifest = None; _ } as curr_decl),
            rec_st,
            visibility ) -> (
          let name = qualified_name ~module_path curr_id in
          match String_map.find_opt name ref_non_alias_types with
          | Some (ref_id, ref_decl) ->
              let new_decl =
                set_type_equality ~module_path ~ref_decl ~curr_decl ref_id
              in
              Sig_type (curr_id, new_decl, rec_st, visibility)
          | None -> item)
      | Sig_module
          (id, presence, ({ md_type = Mty_signature s; _ } as decl), rs, vis) ->
          let module_path = enter_module ~module_path id in
          let s' =
            rec_set_type_equalities ~ref_non_alias_types ~module_path s
          in
          Sig_module
            (id, presence, { decl with md_type = Mty_signature s' }, rs, vis)
      | _ -> item)
    sig_

let set_type_equalities ~reference ~current =
  let ref_non_alias_types = extract_non_alias_types reference in
  rec_set_type_equalities ~ref_non_alias_types ~module_path:[] current

let for_diff ~reference ~current =
  let modified_current = set_type_equalities ~reference ~current in
  let env = Env.empty in
  let env = Env.in_signature true env in
  let env = Env.add_signature reference env in
  Env.add_signature modified_current env
