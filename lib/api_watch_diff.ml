open Types

type 'item change =
  | Added of 'item
  | Removed of 'item
  | Modified of { ref_ : 'item; current : 'item }

type module_diff = { module_name : string; changes : module_change }

and item_change =
  | Value of { name : string; change : value_description change }
  | Module of module_diff

and module_change = Unsupported | Supported of item_change list

type item_type = Value_item | Module_item [@@deriving ord]
type sig_items = Val of value_description | Mod of module_declaration

module Sig_item_map = Map.Make (struct
  type t = item_type * string [@@deriving ord]
end)

module String_map = Map.Make (String)

let extract_non_alias_types ref_sig =
  List.fold_left
    (fun acc item ->
      match item with
      | Sig_type (id, ({ type_manifest = None; _ } as type_decl), _, _) ->
          String_map.add (Ident.name id) (id, type_decl) acc
      | _ -> acc)
    String_map.empty ref_sig

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
          match String_map.find_opt name ref_non_alias_types with
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

let extract_items items =
  List.fold_left
    (fun tbl item ->
      match item with
      | Sig_module (id, _, mod_decl, _, _) ->
          Sig_item_map.add (Module_item, Ident.name id) (Mod mod_decl) tbl
      | Sig_value (id, val_des, _) ->
          Sig_item_map.add (Value_item, Ident.name id) (Val val_des) tbl
      | _ -> tbl)
    Sig_item_map.empty items

let diff_modtype_item ~loc ~typing_env ~name ~reference ~current =
  let modtype_coercion1 () =
    Includemod.modtypes ~loc typing_env ~mark:Mark_both reference current
  in
  let modtype_coercion2 () =
    Includemod.modtypes ~loc typing_env ~mark:Mark_both current reference
  in
  match (modtype_coercion1 (), modtype_coercion2 ()) with
  | Tcoerce_none, Tcoerce_none -> None
  | _, _ -> Some (Module { module_name = name; changes = Unsupported })
  | exception Includemod.Error _ ->
      Some (Module { module_name = name; changes = Unsupported })

let diff_value_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | Some (Val ref_), None -> Some (Value { name; change = Removed ref_ })
  | None, Some (Val current) -> Some (Value { name; change = Added current })
  | Some (Val ref_), Some (Val current) -> (
      let val_coercion1 () =
        Includecore.value_descriptions ~loc:current.val_loc typing_env name
          current ref_
      in
      let val_coercion2 () =
        Includecore.value_descriptions ~loc:ref_.val_loc typing_env name ref_
          current
      in
      match (val_coercion1 (), val_coercion2 ()) with
      | Tcoerce_none, Tcoerce_none -> None
      | _, _ -> Some (Value { name; change = Modified { ref_; current } })
      | exception Includecore.Dont_match _ ->
          Some (Value { name; change = Modified { ref_; current } }))
  | _ -> None

let rec diff_items ~reference ~current =
  let env = env_setup ~ref_sig:reference ~curr_sig:current in
  let ref_items = extract_items reference in
  let curr_items = extract_items current in
  Sig_item_map.merge
    (fun (item_type, name) ref_opt curr_opt ->
      match (item_type, ref_opt, curr_opt) with
      | Value_item, ref_opt, curr_opt ->
          diff_value_item ~typing_env:env ~name ~reference:ref_opt
            ~current:curr_opt
      | Module_item, reference, current ->
          diff_module_item ~typing_env:env ~name ~reference ~current)
    ref_items curr_items
  |> Sig_item_map.bindings |> List.map snd

and diff_module_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | None, Some (Mod _curr_md) ->
      Some (Module { module_name = name; changes = Unsupported })
  | Some (Mod _ref_md), None ->
      Some (Module { module_name = name; changes = Unsupported })
  | Some (Mod reference), Some (Mod current) ->
      diff_module_declaration ~typing_env ~name ~reference ~current
  | _ -> assert false

and diff_module_declaration ~typing_env ~name ~reference ~current =
  match (reference.md_type, current.md_type) with
  | Mty_signature ref_submod, Mty_signature curr_submod ->
      diff_signatures ~typing_env ~reference:ref_submod ~current:curr_submod
      |> Option.map (fun changes -> Module { module_name = name; changes })
  | ref_modtype, curr_modtype ->
      diff_modtype_item ~loc:reference.md_loc ~typing_env ~name
        ~reference:ref_modtype ~current:curr_modtype

and diff_signatures ~typing_env ~reference ~current =
  match diff_items ~reference ~current with
  | [] -> (
      let coercion1 () =
        Includemod.signatures typing_env ~mark:Mark_both reference current
      in
      let coercion2 () =
        Includemod.signatures typing_env ~mark:Mark_both current reference
      in
      match (coercion1 (), coercion2 ()) with
      | Tcoerce_none, Tcoerce_none -> None
      | _, _ -> Some Unsupported
      | exception Includemod.Error _ -> Some Unsupported)
  | item_changes -> Some (Supported item_changes)

let diff_interface ~module_name ~reference ~current =
  let typing_env = Env.empty in
  diff_signatures ~typing_env ~reference ~current
  |> Option.map (fun changes -> { module_name; changes })

let vd_to_string name vd =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.value_description (Ident.create_local name) formatter vd;
  Format.pp_print_flush formatter ();
  Buffer.contents buf

let process_value_diff val_name (val_change : value_description change) =
  match val_change with
  | Added vd ->
      [ Diffutils.Diff.Diff { orig = []; new_ = [ vd_to_string val_name vd ] } ]
  | Removed vd ->
      [ Diffutils.Diff.Diff { orig = [ vd_to_string val_name vd ]; new_ = [] } ]
  | Modified { ref_; current } ->
      [
        Diffutils.Diff.Diff
          {
            orig = [ vd_to_string val_name ref_ ];
            new_ = [ vd_to_string val_name current ];
          };
      ]

let to_text_diff (diff_result : module_diff) : Diffutils.Diff.t String_map.t =
  let rec process_module_diff module_path (module_diff : module_diff) acc =
    match module_diff.changes with
    | Unsupported ->
        String_map.add module_path
          [
            Diffutils.Diff.Diff { orig = []; new_ = [ "<unsupported change>" ] };
          ]
          acc
    | Supported changes ->
        List.fold_left
          (fun acc' change ->
            match change with
            | Value { name; change = val_change } ->
                let diff = process_value_diff name val_change in
                String_map.update module_path
                  (function
                    | None -> Some diff | Some existing -> Some (existing @ diff))
                  acc'
            | Module sub_module_diff ->
                let sub_module_path =
                  module_path ^ "." ^ sub_module_diff.module_name
                in
                process_module_diff sub_module_path sub_module_diff acc')
          acc changes
  in
  process_module_diff diff_result.module_name diff_result String_map.empty
