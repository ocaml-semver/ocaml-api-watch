open Types

type ('item, 'diff) change =
  | Added of 'item
  | Removed of 'item
  | Modified of 'diff

type diff =
  | Value of
      string * (value_description, value_description * value_description) change
  | Any

module FieldMap = Map.Make (struct
  type t = string

  let compare = String.compare
end)

let rec expand (env : Env.t) (typ : Types.type_expr) : Types.type_expr =
  match get_desc typ with
  | Tconstr _ -> Ctype.expand_head env typ
  | Tarrow (label, param_type, result_type, commutable) ->
      let expanded_param = expand env param_type in
      let expanded_result = expand env result_type in
      if expanded_param == param_type && expanded_result == result_type then typ
      else
        Ctype.newty
          (Tarrow (label, expanded_param, expanded_result, commutable))
  | _ -> typ

let extract_non_alias_types ref_sig =
  List.fold_left
    (fun acc item ->
      match item with
      | Sig_type (id, ({ type_manifest = None; _ } as decl), _, _) ->
          FieldMap.add (Ident.name id) (id, decl) acc
      | _ -> acc)
    FieldMap.empty ref_sig

let set_type_equality ref_id curr_decl =
  let ref_path = Path.Pident ref_id in
  {
    curr_decl with
    type_manifest = Some (Btype.newgenty (Tconstr (ref_path, [], ref Mnil)));
  }

let set_type_equalities ~ref_sig ~curr_sig =
  let ref_non_alias_types = extract_non_alias_types ref_sig in
  List.map
    (fun item ->
      match item with
      | Sig_type
          ( curr_id,
            ({ type_manifest = None; _ } as curr_decl),
            rec_st,
            visibility ) -> (
          let name = Ident.name curr_id in
          match FieldMap.find_opt name ref_non_alias_types with
          | Some (ref_id, ref_decl) -> (
              match (ref_decl.type_kind, curr_decl.type_kind) with
              | Type_abstract, Type_abstract
              | Type_record _, Type_record _
              | Type_variant _, Type_variant _
              | Type_open, Type_open ->
                  let new_decl = set_type_equality ref_id curr_decl in
                  Sig_type (curr_id, new_decl, rec_st, visibility)
              | _ -> item)
          | None -> item)
      | _ -> item)
    curr_sig

let env_setup ~ref_sig ~curr_sig =
  let modified_curr_sig = set_type_equalities ~ref_sig ~curr_sig in
  let env = Env.empty in
  let env = Env.in_signature true env in
  let env = Env.add_signature ref_sig env in
  Env.add_signature modified_curr_sig env

let extract_values items =
  List.fold_left
    (fun tbl item ->
      match item with
      | Sig_value (id, val_des, _) -> FieldMap.add (Ident.name id) val_des tbl
      | _ -> tbl)
    FieldMap.empty items

let diff_value ~typing_env ~val_name ~reference ~current =
  let val_coercion1 () =
    Includecore.value_descriptions ~loc:current.val_loc typing_env val_name
      current reference
  in
  let val_coercion2 () =
    Includecore.value_descriptions ~loc:reference.val_loc typing_env val_name
      reference current
  in
  match (val_coercion1 (), val_coercion2 ()) with
  | Tcoerce_none, Tcoerce_none -> None
  | _, _ -> Some ()
  | exception Includecore.Dont_match _ -> Some ()

let resolve env vd =
  let res_val_type = expand env vd.val_type in
  { vd with val_type = res_val_type }

let compare_values ~reference ~current =
  let env = env_setup ~ref_sig:reference ~curr_sig:current in
  let ref_values = extract_values reference in
  let curr_values = extract_values current in
  let diffs =
    FieldMap.fold
      (fun val_name curr_vd acc ->
        match FieldMap.find_opt val_name ref_values with
        | None -> Value (val_name, Added curr_vd) :: acc
        | Some ref_vd -> (
            let value_differs =
              diff_value ~typing_env:env ~val_name ~reference:ref_vd
                ~current:curr_vd
            in
            match value_differs with
            | None -> acc
            | Some _ ->
                let resolved_ref_vd = resolve env ref_vd in
                let resolved_curr_vd = resolve env curr_vd in
                Value (val_name, Modified (resolved_ref_vd, resolved_curr_vd))
                :: acc))
      curr_values []
  in
  FieldMap.fold
    (fun val_name ref_vd acc ->
      if not (FieldMap.mem val_name curr_values) then
        Value (val_name, Removed ref_vd) :: acc
      else acc)
    ref_values diffs

let diff_interface ~reference ~current =
  let value_diffs = compare_values ~reference ~current in
  if value_diffs = [] then
    let typing_env = Env.empty in
    let coercion1 () =
      Includemod.signatures typing_env ~mark:Mark_both reference current
    in
    let coercion2 () =
      Includemod.signatures typing_env ~mark:Mark_both current reference
    in
    match (coercion1 (), coercion2 ()) with
    | Tcoerce_none, Tcoerce_none -> []
    | _, _ -> [ Any ]
    | exception Includemod.Error _ -> [ Any ]
  else value_diffs
