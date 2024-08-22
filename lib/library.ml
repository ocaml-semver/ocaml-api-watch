let collect_cmi_files dir =
  let open CCResult.Infix in
  let* files =
    try Ok (Sys.readdir dir)
    with Sys_error e ->
      Error (Printf.sprintf "Error reading directory %s: %s" dir e)
  in
  Array.fold_left
    (fun acc_res file ->
      let* acc = acc_res in
      let path = Filename.concat dir file in
      if
        Sys.file_exists path
        && (not (Sys.is_directory path))
        && Filename.check_suffix file ".cmi"
      then Ok (path :: acc)
      else Ok acc)
    (Ok []) files

let load_cmi file_path =
  try
    let cmi_infos = Cmi_format.read_cmi file_path in
    Ok (cmi_infos.cmi_sign, cmi_infos.cmi_name)
  with e -> Error (Printexc.to_string e)

let find_build_artifacts_dir project_path =
  let build_dir = Filename.concat project_path "_build/default/lib" in
  try
    let dirs = Sys.readdir build_dir |> Array.to_list in
    match List.find_opt (fun dir -> Filename.check_suffix dir ".objs") dirs with
    | Some objs_dir ->
        let full_objs_path = Filename.concat build_dir objs_dir in
        Some (Filename.concat full_objs_path "byte")
    | None -> None
  with Sys_error _ -> None

let load project_path =
  let open CCResult.Infix in
  match find_build_artifacts_dir project_path with
  | Some artifacts_dir ->
      let* cmi_files = collect_cmi_files artifacts_dir in
      let* signatures =
        CCResult.map_l
          (fun cmi_file ->
            let+ signature, module_name = load_cmi cmi_file in
            (module_name, signature))
          cmi_files
      in
      let merged_signature =
        List.fold_left
          (fun acc (module_name, signature) ->
            String_map.add module_name signature acc)
          String_map.empty signatures
      in
      Ok
        (String_map.fold
           (fun module_name module_sig acc ->
             Types.Sig_module
               ( Ident.create_local module_name,
                 Mp_present,
                 {
                   md_type = Mty_signature module_sig;
                   md_attributes = [];
                   md_loc = Location.none;
                   md_uid = Types.Uid.internal_not_actually_unique;
                 },
                 Trec_not,
                 Exported )
             :: acc)
           merged_signature [])
  | None -> Error "Could not find build artifacts directory"
