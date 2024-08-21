let rec collect_cmi_files dir =
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
      try
        if Sys.is_directory path then
          let+ subdir_files = collect_cmi_files path in
          acc @ subdir_files
        else if Filename.check_suffix file ".cmi" then Ok (path :: acc)
        else Ok acc
      with Sys_error e ->
        Error (Printf.sprintf "Error processing %s: %s" path e))
    (Ok []) files

let load_cmi file_path =
  try
    let cmi_infos = Cmi_format.read_cmi file_path in
    Ok cmi_infos.cmi_sign
  with e -> Error (Printexc.to_string e)

let module_name_of_file file_path =
  file_path |> Filename.basename |> Filename.remove_extension
  |> String.capitalize_ascii

let load project_path =
  let open CCResult.Infix in
  let* cmi_files = collect_cmi_files project_path in
  let* signatures =
    CCResult.map_l
      (fun cmi_file ->
        let module_name = module_name_of_file cmi_file in
        let+ signature = load_cmi cmi_file in
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
