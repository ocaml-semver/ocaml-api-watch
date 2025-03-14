let ( let> ) x f =
  match x with Ok None -> Ok None | Ok (Some x) -> f x | Error x -> Error x

let mod_name file =
  String.capitalize_ascii Filename.(remove_extension (basename file))

let load_cmi file_path =
  try
    let cmi_infos = Cmi_format.read_cmi file_path in
    Ok (cmi_infos.cmi_sign, cmi_infos.cmi_name)
  with e -> Error (Printf.sprintf "Error parsing %s: %s" file_path (Printexc.to_string e))

let lazy_sig path =
  let open CCResult.Infix in
  Lazy.from_fun (fun () ->
      let+ cmi_sign, _ = load_cmi path in
      cmi_sign)

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
  let open CCResult.Infix in
  match String_map.find_opt modname map with
  | None -> Ok None
  | Some lazy_sig ->
    let* sig_ = Lazy.force lazy_sig in
    Ok (Some sig_)

type 'a named = { name : string; value : 'a }
(** Attach a module name to its various representations, e.g. a [signature] or a
   [module_type].
   Mostly used to report lookup failures. *)

(** A module for "flat path", paths without a functor application in them. *)
module Flat_path = struct
  type component = Id of Ident.t | Comp of string
  type t = component list

  let from_path path =
    match Path.flatten path with
    | `Contains_apply -> None
    | `Ok (id, comps) -> Some (Id id :: List.map (fun s -> Comp s) comps)

  let modname_from_component = function Id id -> Ident.name id | Comp s -> s
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

let find_module_item modname sig_ =
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

let find_module_type_in_sig modname sig_ =
  let open Types in
  let mty_opt =
    List.find_map
      (function
        | Sig_modtype (id, { mtd_type; _ }, _)
          when String.equal (Ident.name id) modname ->
            Some mtd_type
        | _ -> None)
      sig_.value
  in
  match mty_opt with
  | Some mty -> Ok mty
  | None -> lookup_error ~path:modname ~module_name:sig_.name

let populate_env typing_env sig_ =
  List.fold_left (fun env sigi ->
      match sigi with
      | Types.Sig_modtype (id, modtype, Exported) ->
        Env.add_modtype id modtype env
      | _ -> env)
    typing_env sig_

let rec find_module_in_sig ~library_modules path sig_ =
  let open CCResult.Infix in
  let typing_env = populate_env Env.empty sig_.value in
  match (path : Flat_path.t) with
  | [ last ] ->
      let modname = Flat_path.modname_from_component last in
      find_module_item modname sig_
  | hd :: tl ->
      let modname = Flat_path.modname_from_component hd in
      let* mty = find_module_item modname sig_ in
      find_module_in_md_type ~typing_env ~library_modules tl { name = modname; value = mty }
  | [] -> assert false

and find_module_in_md_type ~typing_env ~library_modules path mty =
  let open CCResult.Infix in
  let* sig_ = sig_of_module_type ~typing_env ~library_modules mty.value in
  match sig_ with
  | None ->
      let res =
        match mty.value with
        | Mty_alias mty_path | Mty_ident mty_path ->
            let expanded_path = path_in_module ~module_path:mty_path path in
            rewrite_mty_path mty.value expanded_path
        | _ -> mty.value
      in
      Ok res
  | Some s ->
      find_module_in_sig ~library_modules path { name = mty.name; value = s }

and find_module_in_lib ~library_modules path :
    (Types.module_type option, string) result =
  let open Types in
  let open CCResult.Infix in
  let> path = Ok (Flat_path.from_path path) in
  match path with
  | [ comp ] ->
      let modname = Flat_path.modname_from_component comp in
      let> sig_ = (get_sig modname library_modules) in
      Ok (Some (Mty_signature sig_))
  | comp :: inner_path -> (
      let modname = Flat_path.modname_from_component comp in
      let> parent_sig = get_sig modname library_modules in
          let+ mty =
            find_module_in_sig ~library_modules inner_path
              { name = modname; value = parent_sig }
          in
          Some mty)
  | _ -> Ok None

and find_local_module_type ~typing_env path =
  try Some (`Local, Env.find_modtype_expansion path typing_env)
  with Not_found -> None

and find_global_module_type ~library_modules path =
  let typing_env = Env.empty in
  match path with
  | Path.Pdot (parent_mod_path, mty_name) ->
      let> parent_mod = find_module_in_lib ~library_modules parent_mod_path in
      let> sig_ = sig_of_module_type ~typing_env ~library_modules parent_mod in
      let> mty = find_module_type_in_sig mty_name { name = mty_name; value = sig_ } in
      Ok (Some (`Global, mty))
  | _ -> assert false (* Path to module type cannot be root modules/functors *)

and find_module_type ~typing_env ~library_modules path =
    match find_local_module_type ~typing_env path with
    | Some modtype -> Ok (Some modtype)
    | None -> find_global_module_type ~library_modules path

(*and find_module_type_in_lib ~library_modules ~typing_env path :
    (Types.module_type option, string) result =
  match (find_local_module_type ~typing_env path) with
  | Some x ->
      Ok (Some x)
  | None -> 
      find_global_module_type ~library_modules 
*)
and typing_env_for ~typing_env = function
    | `Local -> typing_env
    | `Global -> Env.empty

and sig_of_module_type ~library_modules ~typing_env module_type =
  match module_type with
  | Types.Mty_alias path ->
    (* find_module_in_lib only looks up globally, we shoud proabably fix it. *)
      let> mty = find_module_in_lib ~library_modules path in
      sig_of_module_type ~typing_env:Env.empty ~library_modules mty
  | Mty_ident path ->
      let> where, mty = find_module_type ~typing_env ~library_modules path in
      let typing_env = typing_env_for ~typing_env where in
      sig_of_module_type ~typing_env ~library_modules mty
  | Mty_signature sig_ -> Ok (Some sig_)
  | Mty_functor _ -> Ok None

let rec expand_sig ~typing_env ~library_modules sig_ =
  let open Types in
  let open CCResult.Infix in
  let typing_env = populate_env typing_env sig_ in
  CCResult.map_l
    (fun item ->
      match item with
      | Sig_module (id, presence, ({ md_type; _ } as mod_decl), rs, vis) ->
        let* md_type =
            expand_module_type ~typing_env ~library_modules md_type
        in
          let presence =
            match md_type with
            | Mty_alias _ -> presence
            | _ -> Mp_present (* What is this fixing? *)
          in
          let mod_decl' = { mod_decl with md_type } in
          Ok (Sig_module (id, presence, mod_decl', rs, vis))
      | _ -> Ok item)
    sig_

and expand_module_type ~typing_env ~library_modules module_type =
  let open CCResult.Infix in
  let+ res =
    let> sig_ = sig_of_module_type ~typing_env ~library_modules module_type in
    let+ expanded = expand_sig ~typing_env ~library_modules sig_ in
    Some (Types.Mty_signature expanded)
  in
  Option.value ~default:module_type res

type t = Types.signature String_map.t

let load_unwrapped project_path : (t, string) result =
  let open CCResult.Infix in
  let* library_modules = collect_modules project_path in
  let module_res_map =
    String_map.map (fun sig_ -> Lazy.force sig_) library_modules
  in
  String_map.fold
    (fun key value acc ->
       let* acc = acc in
       let* value = value in
       Ok (String_map.add key value acc))
    module_res_map
    (Ok String_map.empty)

let load ~main_module project_path : (t, string) result =
  let open CCResult.Infix in
  let* library_modules = collect_modules project_path in
  let* main_sig =
    let* x = get_sig main_module library_modules in
    match x with
    | Some s -> Ok s
    | None ->
        Error
          (Printf.sprintf "Could not find main module %s in %s" main_module
             project_path)
  in
  let expanded_main_sig =
    match expand_sig ~typing_env:Env.empty ~library_modules main_sig with
    | Ok expanded_sig -> expanded_sig
    | Error e -> failwith e
  in
  Ok (String_map.singleton main_module expanded_main_sig)
