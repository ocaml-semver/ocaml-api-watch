let mod_name file =
  String.capitalize_ascii Filename.(remove_extension (basename file))

let lazy_sig path =
  Lazy.from_fun (fun () ->
      let cmi_infos = Cmi_format.read_cmi path in
      cmi_infos.cmi_sign)

let collect_modules dir =
  try
    let files = Sys.readdir dir in
    let map =
      Array.fold_left
        (fun acc file ->
          let path = Filename.concat dir file in
          if (not (Sys.is_directory path)) && Filename.check_suffix file ".cmi"
          then String_map.add (mod_name file) (lazy_sig path) acc
          else acc)
        String_map.empty files
    in
    Ok map
  with Sys_error e ->
    Error (Printf.sprintf "Error reading directory %s: %s" dir e)

let get_sig modname map =
  Option.map Lazy.force (String_map.find_opt modname map)

let load_cmi file_path =
  try
    let cmi_infos = Cmi_format.read_cmi file_path in
    Ok (cmi_infos.cmi_sign, cmi_infos.cmi_name)
  with e -> Error (Printexc.to_string e)

(* Attach a module name to its various representations, e.g. a [signature] or a
   [module_type].
   Mostly used to report lookup failures. *)
type 'a named = { name : string; value : 'a }

module Flat_path = struct
  type component = Id of Ident.t | Comp of string
  type t = component list

  let from_path path =
    match Path.flatten path with
    | `Contains_apply -> None
    | `Ok (id, comps) -> Some (Id id :: List.map (fun s -> Comp s) comps)

  let modname_from_component = function Id id -> Ident.name id | Comp s -> s
  let to_string t = String.concat "." (List.map modname_from_component t)
end

let rec path_in_module ~module_path flat_path =
  match flat_path with
  | [] -> module_path
  | hd :: tl ->
      let module_path =
        Path.Pdot (module_path, Flat_path.modname_from_component hd)
      in
      path_in_module ~module_path tl

let rewrite_mty_path mty path =
  let open Types in
  match mty with
  | Mty_ident _ -> Mty_ident path
  | Mty_alias _ -> Mty_alias path
  | _ -> assert false

let lookup_error ~path ~module_name =
  Error (Printf.sprintf "Could not find module %s in %s" path module_name)

let find_module modname sig_ =
  let open Types in
  let mty_opt =
    List.find_map
      (function
        | Sig_module (id, _, { md_type; _ }, _, _)
          when String.equal (Ident.name id) modname ->
            Some md_type
        | _ -> None)
      sig_.value
  in
  match mty_opt with
  | Some mty -> Ok mty
  | None -> lookup_error ~path:modname ~module_name:sig_.name

let rec find_module_in_sig ~library_modules path sig_ =
  let open CCResult.Infix in
  match (path : Flat_path.t) with
  | [ last ] ->
      let modname = Flat_path.modname_from_component last in
      find_module modname sig_
  | hd :: tl ->
      let modname = Flat_path.modname_from_component hd in
      let* mty = find_module modname sig_ in
      find_module_in_md_type ~library_modules tl { name = modname; value = mty }
  | [] -> assert false

and find_module_in_md_type ~library_modules path mty =
  let open CCResult.Infix in
  match mty.value with
  | Mty_signature s ->
      find_module_in_sig ~library_modules path { name = mty.name; value = s }
  | Mty_ident mty_path | Mty_alias mty_path -> (
      let* expanded =
        match Flat_path.from_path mty_path with
        | None -> Ok None
        | Some flat_mty_path ->
            find_module_in_lib ~library_modules flat_mty_path
      in
      match expanded with
      | Some expanded_mty ->
          find_module_in_md_type ~library_modules path
            { name = Path.name mty_path; value = expanded_mty }
      | None ->
          let expanded_path = path_in_module ~module_path:mty_path path in
          Ok (rewrite_mty_path mty.value expanded_path))
  | _ -> lookup_error ~path:(Flat_path.to_string path) ~module_name:mty.name

and find_module_in_lib ~library_modules path :
    (Types.module_type option, string) result =
  let open Types in
  let open CCResult.Infix in
  match path with
  | [ comp ] ->
      let modname = Flat_path.modname_from_component comp in
      let sig_opt = get_sig modname library_modules in
      Ok (Option.map (fun s -> Mty_signature s) sig_opt)
  | comp :: inner_path -> (
      let modname = Flat_path.modname_from_component comp in
      match get_sig modname library_modules with
      | None -> Ok None
      | Some parent_sig -> (
          let* mty =
            find_module_in_sig ~library_modules inner_path
              { name = modname; value = parent_sig }
          in
          match mty with
          | Mty_signature _ | Mty_functor _ -> Ok (Some mty)
          | Mty_ident path' | Mty_alias path' -> (
              match Flat_path.from_path path' with
              | None -> Ok (Some mty)
              | Some fpath -> find_module_in_lib ~library_modules fpath)))
  | _ -> Ok None

let rec expand_sig ~library_modules sig_ =
  let open Types in
  let open CCResult.Infix in
  CCResult.map_l
    (fun item ->
      match item with
      | Sig_module
          ( id,
            presence,
            ({ md_type = Mty_ident path | Mty_alias path; _ } as mod_decl),
            rs,
            vis ) -> (
          match Flat_path.from_path path with
          | None -> Ok item
          | Some fpath -> (
              let* mty_opt = find_module_in_lib ~library_modules fpath in
              match mty_opt with
              | None -> Ok item
              | Some mty ->
                  let* expanded =
                    match mty with
                    | Mty_signature s ->
                        let* expanded = expand_sig ~library_modules s in
                        Ok (Mty_signature expanded)
                    | _ -> Ok mty
                  in
                  let presence =
                    match expanded with
                    | Mty_alias _ -> presence
                    | _ -> Mp_present
                  in
                  let mod_decl' = { mod_decl with md_type = expanded } in
                  Ok (Sig_module (id, presence, mod_decl', rs, vis))))
      | _ -> Ok item)
    sig_

let load_unwrapped project_path =
  let open CCResult.Infix in
  let* library_modules = collect_modules project_path in
  let module_map =
    String_map.map
      (fun sig_ ->
        match expand_sig ~library_modules (Lazy.force sig_) with
        | Ok expanded_sig -> expanded_sig
        | Error e -> failwith e)
      library_modules
  in
  Ok module_map

let load ~main_module project_path =
  let open CCResult.Infix in
  let* library_modules = collect_modules project_path in
  let* main_sig =
    match get_sig main_module library_modules with
    | Some s -> Ok s
    | None ->
        Error
          (Printf.sprintf "Could not find main module %s in %s" main_module
             project_path)
  in
  let expanded_main_sig =
    match expand_sig ~library_modules main_sig with
    | Ok expanded_sig -> expanded_sig
    | Error e -> failwith e
  in
  Ok (String_map.singleton main_module expanded_main_sig)
