open Types

type 'item change =
  | Added of 'item
  | Removed of 'item
  | Modified of { ref_ : 'item; current : 'item }

type module_diff = {
  module_name : string;
  changes : module_change;
}

and item_change =
  | Value of { name : string; change : value_description change }
  | Module of module_diff

and module_change =
  | Unsupported
  | Supported of item_change list

type diff = module_diff option 

module FieldMap = Map.Make (struct
  type t = string

  let compare = String.compare
end)

let extract_non_alias_types ref_sig =
  List.fold_left
    (fun acc item ->
      match item with
      | Sig_type (id, ({ type_manifest = None; _ } as type_decl), _, _) ->
          FieldMap.add (Ident.name id) (id, type_decl) acc
      | _ -> acc)
    FieldMap.empty ref_sig

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
          | Some (ref_id, ref_decl) ->
              let new_decl = set_type_equality ref_id ~ref_decl ~curr_decl in
              Sig_type (curr_id, new_decl, rec_st, visibility)
          | None -> item)
      | _ -> item)
    curr_sig

let env_setup ~ref_sig ~curr_sig =
  let modified_curr_sig = set_type_equalities ~ref_sig ~curr_sig in
  let env = Env.empty in
  let env = Env.in_signature true env in
  let env = Env.add_signature ref_sig env in
  Env.add_signature modified_curr_sig env

let extract_modules items =
  List.fold_left
    (fun tbl item ->
      match item with
      | Sig_module (id, _, mod_decl, _, _) -> FieldMap.add (Ident.name id) mod_decl tbl
      | _ -> tbl)
    FieldMap.empty items

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

let compare_values ~reference ~current =
  let env = env_setup ~ref_sig:reference ~curr_sig:current in
  let ref_values = extract_values reference in
  let curr_values = extract_values current in
  FieldMap.merge
    (fun val_name ref_opt curr_opt ->
      match (ref_opt, curr_opt) with
      | None, None -> None
      | None, Some curr_vd -> Some (Value {name=val_name; change= Added curr_vd})
      | Some ref_vd, None -> Some (Value {name=val_name; change= Removed ref_vd})
      | Some ref_vd, Some curr_vd -> (
          let value_differs =
            diff_value ~typing_env:env ~val_name ~reference:ref_vd
              ~current:curr_vd
          in
          match value_differs with
          | None -> None
          | Some _ ->
              Some (Value {name=val_name; change=Modified {ref_=ref_vd; current=curr_vd}})
          ))
    ref_values curr_values
  |> FieldMap.bindings |> List.map snd

let rec compare_modules curr_mod_name ~reference ~current =
  let value_changes = compare_values ~reference ~current in
  let ref_modules = extract_modules reference in
  let curr_modules = extract_modules current in
  let module_changes =
    FieldMap.merge
      (fun mod_name ref_opt curr_opt ->
        match (ref_opt, curr_opt) with
        | None, None -> None
        | None, Some _curr_md -> Some (Module {module_name= mod_name; changes=Unsupported}) ;
        | Some _ref_md, None ->  Some(Module {module_name= mod_name; changes=Unsupported}) ;
        | Some ref_md, Some curr_md -> match (ref_md.md_type, curr_md.md_type) with 
            |Mty_signature ref_submod, Mty_signature curr_submod->  compare_modules mod_name ~reference:ref_submod ~current:curr_submod
            |_, _-> None)
      ref_modules curr_modules
    |> FieldMap.bindings |> List.map snd
  in
  let item_changes= value_changes @ module_changes in
  if item_changes=[] then None else
  Some(Module{module_name= curr_mod_name; changes= Supported item_changes})

let diff_interface ~reference ~current =
  let module_diffs = compare_modules "main" ~reference ~current in
  match module_diffs with 
  | Some Module mod_diff->Some mod_diff
  | None ->  (let typing_env = Env.empty in
    let coercion1 () =
      Includemod.signatures typing_env ~mark:Mark_both reference current
    in
    let coercion2 () =
      Includemod.signatures typing_env ~mark:Mark_both current reference
    in
    match (coercion1 (), coercion2 ()) with
    | Tcoerce_none, Tcoerce_none -> None
    | _, _ -> Some{module_name= "main"; changes= Unsupported}
    | exception Includemod.Error _ -> Some{module_name= "main"; changes= Unsupported})
  |_-> None

let to_text_diff (diff_result : diff) : Diffutils.Diff.t FieldMap.t =
  let vd_to_string name vd =
    let buf = Buffer.create 256 in
    let formatter = Format.formatter_of_buffer buf in
    Printtyp.value_description (Ident.create_local name) formatter vd;
    Format.pp_print_flush formatter ();
    Buffer.contents buf
  in
  let open Diffutils.Diff in
  let rec process_module_diff module_path (module_diff : module_diff) acc =
    match module_diff.changes with
    | Unsupported -> 
        FieldMap.add module_path 
          [Diff { orig = []; new_ = [ "<unsupported change>" ] }] 
          acc
    | Supported changes ->
        List.fold_left
          (fun acc' change ->
            match change with
            | Value { name; change = value_change } ->
                let diff = match value_change with
                  | Added vd -> [Diff { orig = []; new_ = [ vd_to_string name vd ] }]
                  | Removed vd -> [Diff { orig = [ vd_to_string name vd ]; new_ = [] }]
                  | Modified { ref_; current } ->
                      [Diff {
                        orig = [ vd_to_string name ref_ ];
                        new_ = [ vd_to_string name current ];
                      }]
                in
                FieldMap.add module_path diff acc'
            | Module sub_module_diff ->
                let sub_module_path = 
                  if module_path = "" then sub_module_diff.module_name
                  else module_path ^ "." ^ sub_module_diff.module_name
                in
                process_module_diff sub_module_path sub_module_diff acc')
          acc
          changes
  in
  match diff_result with
  | None -> FieldMap.empty
  | Some module_diff -> process_module_diff module_diff.module_name module_diff FieldMap.empty